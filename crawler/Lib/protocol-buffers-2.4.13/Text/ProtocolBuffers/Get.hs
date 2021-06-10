{-# LANGUAGE CPP,MagicHash,ScopedTypeVariables,FlexibleInstances,RankNTypes,TypeSynonymInstances,MultiParamTypeClasses,BangPatterns,CPP #-}
{-# OPTIONS_GHC -fno-warn-warnings-deprecations #-}
-- | By Chris Kuklewicz, drawing heavily from binary and binary-strict,
-- but all the bugs are my own.
--
-- This file is under the usual BSD3 licence, copyright 2008.
--
-- Modified the monad to be strict for version 2.0.0
--
-- This started out as an improvement to
-- "Data.Binary.Strict.IncrementalGet" with slightly better internals.
-- The simplified 'Get', 'runGet', 'Result' trio with the
-- "Data.Binary.Strict.Class.BinaryParser" instance are an _untested_
-- upgrade from IncrementalGet.  Especially untested are the
-- strictness properties.
--
-- 'Get' usefully implements Applicative and Monad, MonadError,
-- Alternative and MonadPlus.  Unhandled errors are reported along
-- with the number of bytes successfully consumed.  Effects of
-- 'suspend' and 'putAvailable' are visible after
-- fail/throwError/mzero.
--
-- Each time the parser reaches the end of the input it will return a
-- Partial wrapped continuation which requests a (Maybe
-- Lazy.ByteString).  Passing (Just bs) will append bs to the input so
-- far and continue processing.  If you pass Nothing to the
-- continuation then you are declaring that there will never be more
-- input and that the parser should never again return a partial
-- contination; it should return failure or finished.
--
-- 'suspendUntilComplete' repeatedly uses a partial continuation to
-- ask for more input until 'Nothing' is passed and then it proceeds
-- with parsing.
--
-- The 'getAvailable' command returns the lazy byte string the parser
-- has remaining before calling 'suspend'.  The 'putAvailable'
-- replaces this input and is a bit fancy: it also replaces the input
-- at the current offset for all the potential catchError/mplus
-- handlers.  This change is _not_ reverted by fail/throwError/mzero.
--
-- The three 'lookAhead' and 'lookAheadM' and 'lookAheadE' functions are
-- very similar to the ones in binary's Data.Binary.Get.
--
--
-- Add specialized high-bit-run
module Text.ProtocolBuffers.Get
    (Get,runGet,runGetAll,Result(..)
     -- main primitives
    ,ensureBytes,getStorable,getLazyByteString,suspendUntilComplete
     -- parser state manipulation
    ,getAvailable,putAvailable
     -- lookAhead capabilities
    ,lookAhead,lookAheadM,lookAheadE
     -- below is for implementation of BinaryParser (for Int64 and Lazy bytestrings)
    ,skip,bytesRead,isEmpty,isReallyEmpty,remaining,spanOf,highBitRun
    ,getWord8,getByteString
    ,getWord16be,getWord32be,getWord64be
    ,getWord16le,getWord32le,getWord64le
    ,getWordhost,getWord16host,getWord32host,getWord64host
    --
    -- ,scan
    ,decode7,decode7size,decode7unrolled
    ) where

-- The Get monad is an instance of binary-strict's BinaryParser:
-- import qualified Data.Binary.Strict.Class as P(BinaryParser(..))
-- The Get monad is an instance of all of these library classes:
import Control.Applicative(Alternative(empty,(<|>)))
import Control.Monad(MonadPlus(mzero,mplus),when)
import Control.Monad.Error.Class(MonadError(throwError,catchError),Error(strMsg))
-- It can be a MonadCont, but the semantics are too broken without a ton of work.

-- implementation imports
--import Control.Monad(replicateM,(>=>))           -- XXX testing
--import qualified Data.ByteString as S(unpack)    -- XXX testing
--import qualified Data.ByteString.Lazy as L(pack) -- XXX testing
import Control.Monad(ap)                             -- instead of Functor.fmap; ap for Applicative
import qualified Control.Monad.Fail as Fail
import Data.Bits(Bits((.|.),(.&.)),shiftL)
import qualified Data.ByteString as S(concat,length,null,splitAt,findIndex)
import qualified Data.ByteString.Internal as S(ByteString(..),toForeignPtr,inlinePerformIO)
import qualified Data.ByteString.Unsafe as S(unsafeIndex,unsafeDrop {-,unsafeTake-})
import qualified Data.ByteString.Lazy as L(take,drop,length,span,toChunks,fromChunks,null,findIndex)
import qualified Data.ByteString.Lazy.Internal as L(ByteString(..),chunk)
import qualified Data.Foldable as F(foldr,foldr1)    -- used with Seq
import Data.Int(Int32,Int64)                         -- index type for L.ByteString
import Data.Word(Word8,Word16,Word32,Word64)
import Data.Sequence(Seq,null,(|>))                  -- used for future queue in handler state
import Foreign.ForeignPtr(withForeignPtr)
import Foreign.Ptr(Ptr,castPtr,plusPtr,minusPtr,nullPtr)
import Foreign.Storable(Storable(peek,sizeOf))
import System.IO.Unsafe(unsafePerformIO)
#if defined(__GLASGOW_HASKELL__) && !defined(__HADDOCK__)
import GHC.Base(Int(..),uncheckedShiftL#)
import GHC.Word(Word16(..),Word32(..),Word64(..),uncheckedShiftL64#)
#endif
--import Debug.Trace(trace)

trace :: a -> b -> b
trace _ = id

-- Simple external return type
data Result a = Failed {-# UNPACK #-} !Int64 String
              | Finished !L.ByteString {-# UNPACK #-} !Int64 a
              | Partial (Maybe L.ByteString -> Result a)

-- Internal state type, not exposed to the user.
-- Invariant: (S.null _top) implies (L.null _current)
data S = S { _top :: {-# UNPACK #-} !S.ByteString
           , _current :: !L.ByteString
           , consumed :: {-# UNPACK #-} !Int64
           } deriving Show

data T3 s = T3 !Int !s !Int

--data TU s = TU'OK !s !Int | TU'DO (Get s)
data TU s = TU'OK !s !Int

{-# SPECIALIZE decode7unrolled :: Get Int64 #-}
{-# SPECIALIZE decode7unrolled :: Get Int32 #-}
{-# SPECIALIZE decode7unrolled :: Get Word64 #-}
{-# SPECIALIZE decode7unrolled :: Get Word32 #-}
{-# SPECIALIZE decode7unrolled :: Get Int #-}
{-# SPECIALIZE decode7unrolled :: Get Integer #-}
decode7unrolled :: forall s. (Num s,Integral s, Bits s) => Get s
-- NOINLINE decode7unrolled removed to allow SPECIALIZE to work
decode7unrolled = Get $ \ sc sIn@(S ss@(S.PS fp off len) bs n) pc -> trace ("decode7unrolled: "++show (len,n)) $
  if S.null ss
    then trace ("decode7unrolled: S.null ss") $ unGet decode7 sc sIn pc -- decode7 will try suspend then will fail if still bad
    else
      let (TU'OK x i) =
            unsafePerformIO $ withForeignPtr fp $ \ptr0 -> do
                if ptr0 == nullPtr || len < 1 then error "Get.decode7unrolled: ByteString invariant failed" else do
                let ok :: s -> Int -> IO (TU s)
                    ok x0 i0 = return (TU'OK x0 i0)
                    more,err :: IO (TU s)
                    more = return (TU'OK 0 0)  -- decode7
                    err = return (TU'OK 0 (-1))  -- throwError
                    {-# INLINE ok #-}
                    {-# INLINE more #-}
                    {-# INLINE err #-}

--                -- Next line is segfault fix for null bytestrings from Nathan Howell <nhowell@alphaheavy.com>
--                if ptr0 == nullPtr then more else do

                let start = ptr0 `plusPtr` off :: Ptr Word8
                b'1 <- peek start
                if b'1 < 128 then ok (fromIntegral b'1) 1 else do
                let !val'1 = fromIntegral (b'1 .&. 0x7F)
                    !end = start `plusPtr` len
                    !ptr2 = start `plusPtr` 1 :: Ptr Word8
                if ptr2 >= end then more else do

                b'2 <- peek ptr2
                if b'2 < 128 then ok (val'1 .|. (fromIntegral b'2 `shiftL` 7)) 2 else do
                let !val'2 = (val'1 .|. (fromIntegral (b'2 .&. 0x7F) `shiftL` 7))
                    !ptr3 = ptr2 `plusPtr` 1
                if ptr3 >= end then more else do

                b'3::Word8 <- peek ptr3
                if b'3 < 128 then ok (val'2 .|. (fromIntegral b'3 `shiftL` 14)) 3 else do
                let !val'3 = (val'2 .|. (fromIntegral (b'3 .&. 0x7F) `shiftL` 14))
                    !ptr4 = ptr3 `plusPtr` 1
                if ptr4 >= end then more else do

                b'4::Word8 <- peek ptr4
                if b'4 < 128 then ok (val'3 .|. (fromIntegral b'4 `shiftL` 21)) 4 else do
                let !val'4 = (val'3 .|. (fromIntegral (b'4 .&. 0x7F) `shiftL` 21))
                    !ptr5 = ptr4 `plusPtr` 1
                if ptr5 >= end then more else do

                b'5::Word8 <- peek ptr5
                if b'5 < 128 then ok (val'4 .|. (fromIntegral b'5 `shiftL` 28)) 5 else do
                let !val'5 = (val'4 .|. (fromIntegral (b'5 .&. 0x7F) `shiftL` 28))
                    !ptr6 = ptr5 `plusPtr` 1
                if ptr6 >= end then more else do

                b'6::Word8 <- peek ptr6
                if b'6 < 128 then ok (val'5 .|. (fromIntegral b'6 `shiftL` 35)) 6 else do
                let !val'6 = (val'5 .|. (fromIntegral (b'6 .&. 0x7F) `shiftL` 35))
                    !ptr7 = ptr6 `plusPtr` 1
                if ptr7 >= end then more else do

                b'7::Word8 <- peek ptr7
                if b'7 < 128 then ok (val'6 .|. (fromIntegral b'7 `shiftL` 42)) 7 else do
                let !val'7 = (val'6 .|. (fromIntegral (b'7 .&. 0x7F) `shiftL` 42))
                    !ptr8 = ptr7 `plusPtr` 1
                if ptr8 >= end then more else do

                b'8::Word8 <- peek ptr8
                if b'8 < 128 then ok (val'7 .|. (fromIntegral b'8 `shiftL` 49)) 8 else do
                let !val'8 = (val'7 .|. (fromIntegral (b'8 .&. 0x7F) `shiftL` 49))
                    !ptr9 = ptr8 `plusPtr` 1
                if ptr9 >= end then more else do

                b'9::Word8 <- peek ptr9
                if b'9 < 128 then ok (val'8 .|. (fromIntegral b'9 `shiftL` 56)) 9 else do
                let !val'9 = (val'8 .|. (fromIntegral (b'9 .&. 0x7F) `shiftL` 56))
                    !ptrA = ptr9 `plusPtr` 1
                if ptrA >= end then more else do

                b'A::Word8 <- peek ptrA
                if b'A < 128 then ok (val'9 .|. (fromIntegral b'A `shiftL` 63)) 10 else do
                err

      in if i > 0
           then let ss' = (S.unsafeDrop i ss)
                    n' = n+fromIntegral i
                    s'safe = make_safe (S ss' bs n')
                in sc x s'safe pc
           else if i==0 then unGet decode7 sc sIn pc
                        else unGet (throwError $ "Text.ProtocolBuffers.Get.decode7unrolled: more than 10 bytes needed at bytes read of "++show n) sc sIn pc

{-# SPECIALIZE decode7 :: Get Int64 #-}
{-# SPECIALIZE decode7 :: Get Int32 #-}
{-# SPECIALIZE decode7 :: Get Word64 #-}
{-# SPECIALIZE decode7 :: Get Word32 #-}
{-# SPECIALIZE decode7 :: Get Int #-}
{-# SPECIALIZE decode7 :: Get Integer #-}
decode7 :: forall s. (Integral s, Bits s) => Get s
-- NOINLINE decode7 removed to allow SPECIALIZE to work
decode7 = go 0 0
 where
  go !s1 !shift1 = trace ("decode7.go: "++show (toInteger s1, shift1)) $ do
    let -- scanner's inner loop decodes only in current top strict bytestring, does not advance input state
        scanner (S.PS fp off len) =
          withForeignPtr fp $ \ptr0 -> do
           if ptr0 == nullPtr || len < 1 then error "Get.decode7: ByteString invariant failed" else do
            let start = ptr0 `plusPtr` off   -- start is a pointer to the next valid byte
                end   = start `plusPtr` len  -- end is a pointer one byte past the last valid byte
                inner :: (Ptr Word8) -> s -> Int -> IO (T3 s)
                inner !ptr !s !shift
                  | ptr < end = do
                      w <- peek ptr
                      trace ("w: " ++ show w) $ do
                      if (128>) w
                        then return $ T3 (succ (ptr `minusPtr` start) )            -- length of capture
                                         (s .|. ((fromIntegral w) `shiftL` shift)) -- put the last bits into high position
                                         (-1)                                      -- negative shift indicates satisfied
                        else inner (ptr `plusPtr` 1)  -- loop on next byte
                                   (s .|. ((fromIntegral (w .&. 0x7F)) `shiftL` shift)) -- put the new bits into high position
                                   (shift+7)          -- increase high position for next loop
                  | otherwise = return $ T3 (ptr `minusPtr` start)  -- length so far (ptr past end-of-string so no succ)
                                            s                       -- value so far
                                            shift                   -- next shift to use
            inner start s1 shift1
    (S ss bs n) <- getFull
    trace ("getFull says: "++ show ((S.length ss,ss),(L.length bs),n)) $ do
    if S.null ss
      then do
        continue <- suspend
        if continue
          then go s1 shift1
          else fail "Get.decode7: Zero length input" -- XXX can be triggered!
      else do
        let (T3 i sOut shiftOut) = unsafePerformIO $ scanner ss
            t = S.unsafeDrop i ss -- Warning: 't' may be mempty
            n' = n + fromIntegral i
        trace ("scanner says "++show ((i,toInteger sOut,shiftOut),(S.length t,n'))) $ do
        if 0 <= shiftOut
          then do
            putFull_unsafe (make_state bs n')
            if L.null bs
              then do
                continue <- suspend
                if continue
                  then go sOut shiftOut
                  else return sOut
              else do
                go sOut shiftOut
          else do
            putFull_safe (S t bs n') -- bs from getFull is still valid
            return sOut

data T2 = T2 !Int64 !Bool

decode7size :: Get Int64
decode7size = go 0
 where
  go !len1 = do
    let scanner (S.PS fp off len) =
          withForeignPtr fp $ \ptr0 -> do
           if ptr0 == nullPtr || len < 1 then error "Get.decode7size: ByteString invariant failed" else do
            let start = ptr0 `plusPtr` off
                end   = start `plusPtr` len
                inner :: (Ptr Word8) -> IO T2
                inner !ptr
                  | ptr < end = do
                      w <- peek ptr
                      if (128>) w
                        then return $ T2 (fromIntegral (ptr `minusPtr` start)) True
                        else inner (ptr `plusPtr` 1)
                  | otherwise = return $ T2 (fromIntegral (ptr `minusPtr` start)) False
            inner start
    (S ss bs n) <- getFull
    if S.null ss
      then do
        continue <- suspend
        if continue
          then go len1
          else fail "Get.decode7size: zero length input"
      else do
        let (T2 i ok) = unsafePerformIO $ scanner ss
            t = S.unsafeDrop (fromIntegral i) ss
            n' = n + i
            len2 = len1 + i
        if ok
          then do
            putFull_unsafe (S t bs n')
            return len2
          else do
            putFull_unsafe (make_state bs n')
            if L.null bs
              then do
                continue <- suspend
                if continue
                  then go len2
                  else return len2
              else
                go len2

-- Private Internal error handling stack type
-- This must NOT be exposed by this module
--
-- The ErrorFrame is the top-level error handler setup when execution begins.
-- It starts with the Bool set to True: meaning suspend can ask for more input.
-- Once suspend get 'Nothing' in reply the Bool is set to False, which means
-- that 'suspend' should no longer ask for input -- the input is finished.
-- Why store the Bool there?  It was handy when I needed to add it.
data FrameStack b = ErrorFrame (String -> S -> Result b) -- top level handler
                               Bool -- True at start, False if Nothing passed to suspend continuation
                  | HandlerFrame (Maybe ( S -> FrameStack b -> String -> Result b ))  -- encapsulated handler
                                 S  -- stored state to pass to handler
                                 (Seq L.ByteString)  -- additional input to hass to handler
                                 (FrameStack b)  -- earlier/shallower/outer handlers

type Success b a = (a -> S -> FrameStack b -> Result b)

-- Internal monad type
newtype Get a = Get {
  unGet :: forall b.    -- the forall hides the CPS style (and prevents use of MonadCont)
           Success b a  -- main continuation
        -> S            -- parser state
        -> FrameStack b -- error handler stack
        -> Result b     -- operation
    }

-- These implement the checkponting needed to store and revive the
-- state for lookAhead.  They are fragile because the setCheckpoint
-- must preceed either useCheckpoint or clearCheckpoint but not both.
-- The FutureFrame must be the most recent handler, so the commands
-- must be in the same scope depth.  Because of these constraints, the reader
-- value 'r' does not need to be stored and can be taken from the Get
-- parameter.
--
-- IMPORTANT: Any FutureFrame at the top level(s) is discarded by throwError.
setCheckpoint,useCheckpoint,clearCheckpoint :: Get ()
setCheckpoint = Get $ \ sc s pc -> sc () s (HandlerFrame Nothing s mempty pc)

useCheckpoint = Get $ \ sc (S _ _ _) frame ->
  case frame of
    (HandlerFrame Nothing s future pc) -> sc () (collect s future) pc
    _ -> error "Text.ProtocolBuffers.Get: Impossible useCheckpoint frame!"

clearCheckpoint = Get $ \ sc s frame ->
   case frame of
     (HandlerFrame Nothing _s _future pc) -> sc () s pc
     _ -> error "Text.ProtocolBuffers.Get: Impossible clearCheckpoint frame!"

-- | 'lookAhead' runs the @todo@ action and then rewinds only the
-- BinaryParser state.  Any new input from 'suspend' or changes from
-- 'putAvailable' are kept.  Changes to the user state (MonadState)
-- are kept.  The MonadWriter output is retained.
--
-- If an error is thrown then the entire monad state is reset to last
-- catchError as usual.
lookAhead :: Get a -> Get a
lookAhead todo = do
  setCheckpoint
  a <- todo
  useCheckpoint
  return a

-- | 'lookAheadM' runs the @todo@ action. If the action returns 'Nothing' then the
-- BinaryParser state is rewound (as in 'lookAhead').  If the action return 'Just' then
-- the BinaryParser is not rewound, and lookAheadM acts as an identity.
--
-- If an error is thrown then the entire monad state is reset to last
-- catchError as usual.
lookAheadM :: Get (Maybe a) -> Get (Maybe a)
lookAheadM todo = do
  setCheckpoint
  a <- todo
  maybe useCheckpoint (const clearCheckpoint) a
  return a

-- | 'lookAheadE' runs the @todo@ action. If the action returns 'Left' then the
-- BinaryParser state is rewound (as in 'lookAhead').  If the action return 'Right' then
-- the BinaryParser is not rewound, and lookAheadE acts as an identity.
--
-- If an error is thrown then the entire monad state is reset to last
-- catchError as usual.
lookAheadE :: Get (Either a b) -> Get (Either a b)
lookAheadE todo = do
  setCheckpoint
  a <- todo
  either (const useCheckpoint) (const clearCheckpoint) a
  return a

-- 'collect' is used by 'putCheckpoint' and 'throwError'
collect :: S -> Seq L.ByteString -> S
collect s@(S ss bs n) future | Data.Sequence.null future = make_safe $ s
                             | otherwise = make_safe $ S ss (mappend bs (F.foldr1 mappend future)) n

-- Put the Show instances here

instance (Show a) => Show (Result a) where
  showsPrec _ (Failed n msg) = ("(Failed "++) . shows n . (' ':) . shows msg . (")"++)
  showsPrec _ (Finished bs n a) =
    ("(CFinished ("++)
    . shows bs . (") ("++)
    . shows n . (") ("++)
    . shows a . ("))"++)
  showsPrec _ (Partial {}) = ("(Partial <Maybe Data.ByteString.Lazy.ByteString-> Result a)"++)

instance Show (FrameStack b) where
  showsPrec _ (ErrorFrame _ p) =(++) "(ErrorFrame <e->s->m b> " . shows p . (")"++)
  showsPrec _ (HandlerFrame _ s future pc) = ("(HandlerFrame <> ("++)
                                     . shows s . (") ("++) . shows future . (") ("++)
                                     . shows pc . (")"++)

-- | 'runGet' is the simple executor
runGet :: Get a -> L.ByteString -> Result a
runGet (Get f) bsIn = f scIn sIn (ErrorFrame ec True)
  where scIn a (S ss bs n) _pc = Finished (L.chunk ss bs) n a
        sIn = make_state bsIn 0
        ec msg sOut = Failed (consumed sOut) msg

-- | 'runGetAll' is the simple executor, and will not ask for any continuation because this lazy bytestring is all the input
runGetAll :: Get a -> L.ByteString -> Result a
runGetAll (Get f) bsIn = f scIn sIn (ErrorFrame ec False)
  where scIn a (S ss bs n) _pc = Finished (L.chunk ss bs) n a
        sIn = make_state bsIn 0
        ec msg sOut = Failed (consumed sOut) msg

-- | Get the input currently available to the parser.
getAvailable :: Get L.ByteString
getAvailable = Get $ \ sc s@(S ss bs _) pc -> sc (L.chunk ss bs) s pc

-- | 'putAvailable' replaces the bytestream past the current # of read
-- bytes.  This will also affect pending MonadError handler and
-- MonadPlus branches.  I think all pending branches have to have
-- fewer bytesRead than the current one.  If this is wrong then an
-- error will be thrown.
--
-- WARNING : 'putAvailable' is still untested.
putAvailable :: L.ByteString -> Get ()
putAvailable !bsNew = Get $ \ sc (S _ss _bs n) pc ->
  let !s' = make_state bsNew n
      rebuild (HandlerFrame catcher (S ss1 bs1 n1) future pc') =
               HandlerFrame catcher sNew mempty (rebuild pc')
        where balance = n - n1
              whole | balance < 0 = error "Impossible? Cannot rebuild HandlerFrame in MyGet.putAvailable: balance is negative!"
                    | otherwise = L.take balance $ L.chunk ss1 bs1 `mappend` F.foldr mappend mempty future
              sNew | balance /= L.length whole = error "Impossible? MyGet.putAvailable.rebuild.sNew HandlerFrame assertion failed."
                   | otherwise = make_state (mappend whole bsNew) n1
      rebuild x@(ErrorFrame {}) = x
  in sc () s' (rebuild pc)

-- Internal access to full internal state, as helper functions
getFull :: Get S
getFull = Get $ \ sc s pc -> sc s s pc

{-# INLINE putFull_unsafe #-}
putFull_unsafe :: S -> Get ()
putFull_unsafe !s = Get $ \ sc _s pc -> sc () s pc

{-# INLINE make_safe #-}
make_safe :: S -> S
make_safe s@(S ss bs n) =
  if S.null ss
    then make_state bs n
    else s

{-# INLINE make_state #-}
make_state :: L.ByteString -> Int64 -> S
make_state L.Empty n = S mempty mempty n
make_state (L.Chunk ss bs) n = S ss bs n

putFull_safe :: S -> Get ()
putFull_safe= putFull_unsafe . make_safe

-- | Keep calling 'suspend' until Nothing is passed to the 'Partial'
-- continuation.  This ensures all the data has been loaded into the
-- state of the parser.
suspendUntilComplete :: Get ()
suspendUntilComplete = do
  continue <- suspend
  when continue suspendUntilComplete

-- | Call suspend and throw and error with the provided @msg@ if
-- Nothing has been passed to the 'Partial' continuation.  Otherwise
-- return ().
suspendMsg :: String -> Get ()
suspendMsg msg = do continue <- suspend
                    if continue then return ()
                      else throwError msg

-- | check that there are at least @n@ bytes available in the input.
-- This will suspend if there is to little data.
ensureBytes :: Int64 -> Get ()
ensureBytes n = do
  (S ss bs _read) <- getFull
  if S.null ss
    then suspendMsg "ensureBytes failed" >> ensureBytes n
    else do
      if n < fromIntegral (S.length ss)
        then return ()
        else do if n == L.length (L.take n (L.chunk ss bs))
                  then return ()
                  else suspendMsg "ensureBytes failed" >> ensureBytes n
{-# INLINE ensureBytes #-}

-- | Pull @n@ bytes from the input, as a lazy ByteString.  This will
-- suspend if there is too little data.
getLazyByteString :: Int64 -> Get L.ByteString
getLazyByteString n | n<=0 = return mempty
                    | otherwise = do
  (S ss bs offset) <- getFull
  if S.null ss
    then do
      suspendMsg ("getLazyByteString S.null ss failed with "++show (n,(S.length ss,L.length bs,offset)))
      getLazyByteString n
    else do
      case splitAtOrDie n (L.chunk ss bs) of  -- safe use of L.chunk because of S.null ss check above
        Just (consume,rest) -> do
           putFull_unsafe (make_state rest (offset+n))
           return $! consume
        Nothing -> do
           suspendMsg ("getLazyByteString (Nothing from splitAtOrDie) failed with "++show (n,(S.length ss,L.length bs,offset)))
           getLazyByteString n
{-# INLINE getLazyByteString #-} -- important

-- | 'suspend' is supposed to allow the execution of the monad to be
-- halted, awaiting more input.  The computation is supposed to
-- continue normally if this returns True, and is supposed to halt
-- without calling suspend again if this returns False.  All future
-- calls to suspend will return False automatically and no nothing
-- else.
--
-- These semantics are too specialized to let this escape this module.
class MonadSuspend m where
  suspend :: m Bool

-- The instance here is fairly specific to the stack manipluation done
-- by 'addFuture' to ('S' user) and to the packaging of the resumption
-- function in 'IResult'('IPartial').
instance MonadSuspend Get where
    suspend = Get (
      let checkBool (ErrorFrame _ b) = b
          checkBool (HandlerFrame _ _ _ pc) = checkBool pc
          -- addFuture puts the new data in 'future' where throwError's collect can find and use it
          addFuture bs (HandlerFrame catcher s future pc) =
                        HandlerFrame catcher s (future |> bs) (addFuture bs pc)
          addFuture _bs x@(ErrorFrame {}) = x
          -- Once suspend is given Nothing, it remembers this and always returns False
          rememberFalse (ErrorFrame ec _) = ErrorFrame ec False
          rememberFalse (HandlerFrame catcher s future pc) =
                         HandlerFrame catcher s future (rememberFalse pc)
      in \ sc sIn pcIn ->
      if checkBool pcIn -- Has Nothing ever been given to a partial continuation?
        then let f Nothing = let pcOut = rememberFalse pcIn
                             in sc False sIn pcOut
                 f (Just bs') = let sOut = appendBS sIn bs'
                                    pcOut = addFuture bs' pcIn
                                in sc True sOut pcOut
             in Partial f
        else sc False sIn pcIn  -- once Nothing has been given suspend is a no-op
                  )
     where appendBS (S ss bs n) bs' = make_safe (S ss (mappend bs bs') n)

-- A unique sort of command...

-- | 'discardInnerHandler' causes the most recent catchError to be
-- discarded, i.e. this reduces the stack of error handlers by removing
-- the top one.  These are the same handlers which Alternative((<|>)) and
-- MonadPlus(mplus) use.  This is useful to commit to the current branch and let
-- the garbage collector release the suspended handler and its hold on
-- the earlier input.
discardInnerHandler :: Get ()
discardInnerHandler = Get $ \ sc s pcIn ->
  let pcOut = case pcIn of ErrorFrame {} -> pcIn
                           HandlerFrame _ _ _ pc' -> pc'
  in sc () s pcOut
{-# INLINE discardInnerHandler #-}

{- Currently unused, commented out to satisfy -Wall

-- | 'discardAllHandlers' causes all catchError handler to be
-- discarded, i.e. this reduces the stack of error handlers to the top
-- level handler.  These are the same handlers which Alternative((<|>))
-- and MonadPlus(mplus) use.  This is useful to commit to the current
-- branch and let the garbage collector release the suspended handlers
-- and their hold on the earlier input.
discardAllHandlers :: Get ()
discardAllHandlers = Get $ \ sc s pcIn ->
  let base pc@(ErrorFrame {}) = pc
      base (HandlerFrame _ _ _ pc) = base pc
  in sc () s (base pcIn)
{-# INLINE discardAllHandlers #-}
-}
-- The BinaryParser instance:

-- | Discard the next @m@ bytes
skip :: Int64 -> Get ()
skip m | m <=0 = return ()
       | otherwise = do
  ensureBytes m
  (S ss bs n) <- getFull
  -- Could ignore impossible S.null ss due to (ensureBytes m) and (0 < m) but be paranoid
  let lbs = L.chunk ss bs -- L.chunk is safe
  putFull_unsafe (make_state (L.drop m lbs) (n+m))  -- drop will not perform less than 'm' bytes due to ensureBytes above

-- | Return the number of 'bytesRead' so far.  Initially 0, never negative.
bytesRead :: Get Int64
bytesRead = fmap consumed getFull

-- | Return the number of bytes 'remaining' before the current input
-- runs out and 'suspend' might be called.
remaining :: Get Int64
remaining = do (S ss bs _) <- getFull
               return $ fromIntegral (S.length ss) + (L.length bs)

-- | Return True if the number of bytes 'remaining' is 0.  Any futher
-- attempts to read an empty parser will call 'suspend' which might
-- result in more input to consume.
--
-- Compare with 'isReallyEmpty'
isEmpty :: Get Bool
isEmpty = do (S ss _bs _n) <- getFull
             return (S.null ss) --  && (L.null bs)

-- | Return True if the input is exhausted and will never be added to.
-- Returns False if there is input left to consume.
--
-- Compare with 'isEmpty'
isReallyEmpty :: Get Bool
isReallyEmpty = isEmpty >>= loop
 where loop False = return False
       loop True = do
         continue <- suspend
         if continue
           then isReallyEmpty
           else return True

-- | get the longest prefix of the input where the high bit is set as well as following byte.
-- This made getVarInt slower.
highBitRun :: Get Int64
{-# INLINE highBitRun #-}
highBitRun = loop where
  loop :: Get Int64
  {-# INLINE loop #-}
  loop = do
    (S ss bs _n) <- getFull
    -- S.null ss is okay, will lead to Nothing, Nothing, suspend below
    let mi = S.findIndex (128>) ss
    case mi of
      Just i -> return (succ $ fromIntegral i)
      Nothing -> do
        let mj = L.findIndex (128>) bs
        case mj of
          Just j -> return (fromIntegral (S.length ss) + succ j)
          Nothing -> do
            continue <- suspend
            if continue then loop
              else fail "highBitRun has failed"

-- | get the longest prefix of the input where all the bytes satisfy the predicate.
spanOf :: (Word8 -> Bool) ->  Get (L.ByteString)
spanOf f = do let loop = do (S ss bs n) <- getFull
                            let (pre,post) = L.span f (L.chunk ss bs) -- L.chunk is safe
                            putFull_unsafe (make_state post (n + L.length pre))
                            if L.null post
                              then do continue <- suspend
                                      if continue then fmap ((L.toChunks pre)++) loop
                                        else return (L.toChunks pre)
                              else return (L.toChunks pre)
              fmap L.fromChunks loop
{-# INLINE spanOf #-}

-- | Pull @n@ bytes from the input, as a strict ByteString.  This will
-- suspend if there is too little data.  If the result spans multiple
-- lazy chunks then the result occupies a freshly allocated strict
-- bytestring, otherwise it fits in a single chunk and refers to the
-- same immutable memory block as the whole chunk.
getByteString :: Int -> Get S.ByteString
getByteString nIn | nIn <= 0 = return mempty
                  | otherwise = do
  (S ss bs n) <- getFull
  if nIn < S.length ss -- Leave at least one character of 'ss' in 'post' allowing putFull_unsafe below
    then do let (pre,post) = S.splitAt nIn ss
            putFull_unsafe (S post bs (n+fromIntegral nIn))
            return $! pre
    -- Expect nIn to be less than S.length ss the vast majority of times
    -- so do not worry about doing anything fancy here.
    else do now <- fmap (S.concat . L.toChunks) (getLazyByteString (fromIntegral nIn))
            return $! now
{-# INLINE getByteString #-} -- important

getWordhost :: Get Word
getWordhost = getStorable
{-# INLINE getWordhost #-}

getWord8 :: Get Word8
getWord8 = getPtr 1
{-# INLINE getWord8 #-}

getWord16be,getWord16le,getWord16host :: Get Word16
getWord16be = do
    s <- getByteString 2
    return $! (fromIntegral (s `S.unsafeIndex` 0) `shiftl_w16` 8) .|.
              (fromIntegral (s `S.unsafeIndex` 1))
{-# INLINE getWord16be #-}
getWord16le = do
    s <- getByteString 2
    return $! (fromIntegral (s `S.unsafeIndex` 1) `shiftl_w16` 8) .|.
              (fromIntegral (s `S.unsafeIndex` 0) )
{-# INLINE getWord16le #-}
getWord16host = getStorable
{-# INLINE getWord16host #-}

getWord32be,getWord32le,getWord32host :: Get Word32
getWord32be = do
    s <- getByteString 4
    return $! (fromIntegral (s `S.unsafeIndex` 0) `shiftl_w32` 24) .|.
              (fromIntegral (s `S.unsafeIndex` 1) `shiftl_w32` 16) .|.
              (fromIntegral (s `S.unsafeIndex` 2) `shiftl_w32`  8) .|.
              (fromIntegral (s `S.unsafeIndex` 3) )
{-# INLINE getWord32be #-}
getWord32le = do
    s <- getByteString 4
    return $! (fromIntegral (s `S.unsafeIndex` 3) `shiftl_w32` 24) .|.
              (fromIntegral (s `S.unsafeIndex` 2) `shiftl_w32` 16) .|.
              (fromIntegral (s `S.unsafeIndex` 1) `shiftl_w32`  8) .|.
              (fromIntegral (s `S.unsafeIndex` 0) )
{-# INLINE getWord32le #-}
getWord32host = getStorable
{-# INLINE getWord32host #-}


getWord64be,getWord64le,getWord64host :: Get Word64
getWord64be = do
    s <- getByteString 8
    return $! (fromIntegral (s `S.unsafeIndex` 0) `shiftl_w64` 56) .|.
              (fromIntegral (s `S.unsafeIndex` 1) `shiftl_w64` 48) .|.
              (fromIntegral (s `S.unsafeIndex` 2) `shiftl_w64` 40) .|.
              (fromIntegral (s `S.unsafeIndex` 3) `shiftl_w64` 32) .|.
              (fromIntegral (s `S.unsafeIndex` 4) `shiftl_w64` 24) .|.
              (fromIntegral (s `S.unsafeIndex` 5) `shiftl_w64` 16) .|.
              (fromIntegral (s `S.unsafeIndex` 6) `shiftl_w64`  8) .|.
              (fromIntegral (s `S.unsafeIndex` 7) )
{-# INLINE getWord64be #-}
getWord64le = do
    s <- getByteString 8
    return $! (fromIntegral (s `S.unsafeIndex` 7) `shiftl_w64` 56) .|.
              (fromIntegral (s `S.unsafeIndex` 6) `shiftl_w64` 48) .|.
              (fromIntegral (s `S.unsafeIndex` 5) `shiftl_w64` 40) .|.
              (fromIntegral (s `S.unsafeIndex` 4) `shiftl_w64` 32) .|.
              (fromIntegral (s `S.unsafeIndex` 3) `shiftl_w64` 24) .|.
              (fromIntegral (s `S.unsafeIndex` 2) `shiftl_w64` 16) .|.
              (fromIntegral (s `S.unsafeIndex` 1) `shiftl_w64`  8) .|.
              (fromIntegral (s `S.unsafeIndex` 0) )
{-# INLINE getWord64le #-}
getWord64host = getStorable
{-# INLINE getWord64host #-}

-- Below here are the class instances

instance Functor Get where
  fmap f m = Get (\sc -> unGet m (sc . f))
  {-# INLINE fmap #-}

instance Monad Get where
  return a = seq a $ Get (\sc -> sc a)
  {-# INLINE return #-}
  m >>= k  = Get (\sc -> unGet m (\ a -> seq a $ unGet (k a) sc))
  {-# INLINE (>>=) #-}
#if !MIN_VERSION_base(4,11,0)
  fail = Fail.fail
#endif

instance Fail.MonadFail Get where
  fail = throwError . strMsg

instance MonadError String Get where
  throwError msg = Get $ \_sc  s pcIn ->
    let go (ErrorFrame ec _) = ec msg s
        go (HandlerFrame (Just catcher) s1 future pc1) = catcher (collect s1 future) pc1 msg
        go (HandlerFrame Nothing _s1 _future pc1) = go pc1
    in go pcIn

  catchError mayFail handler = Get $ \sc s pc ->
    let pcWithHandler = let catcher s1 pc1 e1 = unGet (handler e1) sc s1 pc1
                        in HandlerFrame (Just catcher) s mempty pc
        actionWithCleanup = mayFail >>= \a -> discardInnerHandler >> return a
    in unGet actionWithCleanup sc s pcWithHandler

instance MonadPlus Get where
  mzero = throwError (strMsg "[mzero:no message]")
  mplus m1 m2 = catchError m1 (const m2)

instance Applicative Get where
  pure = return
  (<*>) = ap

instance Alternative Get where
  empty = mzero
  (<|>) = mplus

-- | I use "splitAt" without tolerating too few bytes, so write a Maybe version.
-- This is the only place I invoke L.Chunk as constructor instead of pattern matching.
-- I claim that the first argument cannot be empty.
splitAtOrDie :: Int64 -> L.ByteString -> Maybe (L.ByteString, L.ByteString)
splitAtOrDie i ps | i <= 0 = Just (mempty, ps)
splitAtOrDie _i L.Empty = Nothing
splitAtOrDie i (L.Chunk x xs) | i < len = let (pre,post) = S.splitAt (fromIntegral i) x
                                          in Just (L.chunk pre mempty, L.chunk post xs)
                              | otherwise = case splitAtOrDie (i-len) xs of
                                              Nothing -> Nothing
                                              Just (y1,y2) -> Just (L.chunk x y1,y2)
  where len = fromIntegral (S.length x)
{-# INLINE splitAtOrDie #-}

------------------------------------------------------------------------
-- getPtr copied from binary's Get.hs

-- helper, get a raw Ptr onto a strict ByteString copied out of the
-- underlying lazy byteString. So many indirections from the raw parser
-- state that my head hurts...

-- Assume n>0
getPtr :: (Storable a) => Int -> Get a
getPtr n = do
    (fp,o,_) <- fmap S.toForeignPtr (getByteString n)
    return . S.inlinePerformIO $ withForeignPtr fp $ \p -> peek (castPtr $ p `plusPtr` o)
{-# INLINE getPtr #-}

-- I pushed the sizeOf into here (uses ScopedTypeVariables)
-- Assume sizeOf (undefined :: a)) > 0
getStorable :: forall a. (Storable a) => Get a
getStorable = do
    (fp,o,_) <- fmap S.toForeignPtr (getByteString (sizeOf (undefined :: a)))
    return . S.inlinePerformIO $ withForeignPtr fp $ \p -> peek (castPtr $ p `plusPtr` o)
{-# INLINE getStorable #-}

------------------------------------------------------------------------
------------------------------------------------------------------------
-- Unchecked shifts copied from binary's Get.hs

shiftl_w16 :: Word16 -> Int -> Word16
shiftl_w32 :: Word32 -> Int -> Word32
shiftl_w64 :: Word64 -> Int -> Word64

#if defined(__GLASGOW_HASKELL__) && !defined(__HADDOCK__)
shiftl_w16 (W16# w) (I# i) = W16# (w `uncheckedShiftL#`   i)
shiftl_w32 (W32# w) (I# i) = W32# (w `uncheckedShiftL#`   i)

#if WORD_SIZE_IN_BITS < 64
shiftl_w64 (W64# w) (I# i) = W64# (w `uncheckedShiftL64#` i)

#else
shiftl_w64 (W64# w) (I# i) = W64# (w `uncheckedShiftL#` i)
#endif

#else
shiftl_w16 = shiftL
shiftl_w32 = shiftL
shiftl_w64 = shiftL
#endif
