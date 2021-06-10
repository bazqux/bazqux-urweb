{-# LANGUAGE DeriveDataTypeable,GeneralizedNewtypeDeriving,CPP #-}
-- | "Text.ProtocolBuffers.Basic" defines or re-exports most of the
-- basic field types; 'Maybe','Bool', 'Double', and 'Float' come from
-- the Prelude instead. This module also defines the 'Mergeable' and
-- 'Default' classes. The 'Wire' class is not defined here to avoid orphans.
module Text.ProtocolBuffers.Basic
  ( -- * Basic types for protocol buffer fields in Haskell
    Double,Float,Bool,Maybe,Seq,Utf8(Utf8),ByteString,Int32,Int64,Word32,Word64
    -- * Haskell types that act in the place of DescritorProto values
  , WireTag(..),FieldId(..),WireType(..),FieldType(..),EnumCode(..),WireSize
    -- * Some of the type classes implemented messages and fields
  , Mergeable(..),Default(..) -- ,Wire(..)
  , isValidUTF8, toUtf8, utf8, uToString, uFromString
  ) where

import Data.Aeson
import Data.Bits(Bits)
import Data.ByteString.Lazy(ByteString)
import Data.Foldable as F(Foldable(foldl))
import Data.Generics(Data(..))
import Data.Int(Int32,Int64)
import Data.Ix(Ix)
import Data.Semigroup (Semigroup(..))
import Data.Sequence(Seq,(><))
import Data.String (IsString(..))
import Data.Typeable(Typeable)
import Data.Word(Word8,Word32,Word64)

import qualified Data.ByteString.Lazy as L(unpack)
import Data.ByteString.Lazy.UTF8 as U (toString,fromString)
import qualified Data.Text.Lazy as TL
import qualified Data.Text.Lazy.Encoding as TL

-- Num instances are derived below for the purpose of getting fromInteger for case matching

-- | 'Utf8' is used to mark 'ByteString' values that (should) contain
-- valid utf8 encoded strings.  This type is used to represent
-- 'TYPE_STRING' values.
newtype Utf8 = Utf8 ByteString deriving (Data,Typeable,Eq,Ord)

utf8 :: Utf8 -> ByteString
utf8 (Utf8 bs) = bs

instance Read Utf8 where
  readsPrec d xs = let r :: Int -> ReadS String
                       r = readsPrec
                       f :: (String,String) -> (Utf8,String)
                       f (a,b) = (Utf8 (U.fromString a),b)
                   in map f . r d $ xs

instance Show Utf8 where
  showsPrec d (Utf8 bs) = let s :: Int -> String -> ShowS
                              s = showsPrec
                          in s d (U.toString bs)

instance IsString Utf8 where
  fromString = uFromString

instance Semigroup Utf8 where
  (<>) (Utf8 x) (Utf8 y) = Utf8 (x <> y)

instance Monoid Utf8 where
  mempty = Utf8 mempty
  mappend = (<>)

instance ToJSON Utf8 where
    toJSON (Utf8 t) = toJSON (TL.decodeUtf8 t)

instance FromJSON Utf8 where
    parseJSON value =
        case value of
          String t -> return . Utf8 . TL.encodeUtf8 . TL.fromStrict $ t
          _ -> fail ("Value " ++ show value ++ " is not a UTF-8 string")

-- | 'WireTag' is the 32 bit value with the upper 29 bits being the
-- 'FieldId' and the lower 3 bits being the 'WireType'
newtype WireTag = WireTag { getWireTag :: Word32 } -- bit concatenation of FieldId and WireType
  deriving (Eq,Ord,Enum,Read,Show,Num,Bits,Bounded,Data,Typeable)

-- | 'FieldId' is the field number which can be in the range 1 to
-- 2^29-1 but the value from 19000 to 19999 are forbidden (so sayeth
-- Google).
newtype FieldId = FieldId { getFieldId :: Int32 } -- really 29 bits
  deriving (Eq,Ord,Enum,Read,Show,Num,Data,Typeable,Ix)

-- Note that values 19000-19999 are forbidden for FieldId
instance Bounded FieldId where
  minBound = 1
  maxBound = 536870911 -- 2^29-1

-- | 'WireType' is the 3 bit wire encoding value, and is currently in
-- the range 0 to 5, leaving 6 and 7 currently invalid.
--
-- * 0 /Varint/ : int32, int64, uint32, uint64, sint32, sint64, bool, enum
--
-- * 1 /64-bit/ : fixed64, sfixed64, double
--
-- * 2 /Length-delimited/ : string, bytes, embedded messages
--
-- * 3 /Start group/ : groups (deprecated)
--
-- * 4 /End group/ : groups (deprecated)
--
-- * 5 /32-bit/ : fixed32, sfixed32, float
--
newtype WireType = WireType { getWireType :: Word32 }    -- really 3 bits
  deriving (Eq,Ord,Enum,Read,Show,Num,Data,Typeable)

instance Bounded WireType where
  minBound = 0
  maxBound = 5

{- | 'FieldType' is the integer associated with the
  FieldDescriptorProto's Type.  The allowed range is currently 1 to
  18, as shown below (excerpt from descritor.proto)

>    // 0 is reserved for errors.
>    // Order is weird for historical reasons.
>    TYPE_DOUBLE         = 1;
>    TYPE_FLOAT          = 2;
>    TYPE_INT64          = 3;   // Not ZigZag encoded.  Negative numbers
>                               // take 10 bytes.  Use TYPE_SINT64 if negative
>                               // values are likely.
>    TYPE_UINT64         = 4;
>    TYPE_INT32          = 5;   // Not ZigZag encoded.  Negative numbers
>                               // take 10 bytes.  Use TYPE_SINT32 if negative
>                               // values are likely.
>    TYPE_FIXED64        = 6;
>    TYPE_FIXED32        = 7;
>    TYPE_BOOL           = 8;
>    TYPE_STRING         = 9;
>    TYPE_GROUP          = 10;  // Tag-delimited aggregate.
>    TYPE_MESSAGE        = 11;  // Length-delimited aggregate.
>
>    // New in version 2.
>    TYPE_BYTES          = 12;
>    TYPE_UINT32         = 13;
>    TYPE_ENUM           = 14;
>    TYPE_SFIXED32       = 15;
>    TYPE_SFIXED64       = 16;
>    TYPE_SINT32         = 17;  // Uses ZigZag encoding.
>    TYPE_SINT64         = 18;  // Uses ZigZag encoding.

-}

newtype FieldType = FieldType { getFieldType :: Int } -- really [1..18] as fromEnum of Type from Type.hs
  deriving (Eq,Ord,Enum,Read,Show,Num,Data,Typeable)

instance Bounded FieldType where
  minBound = 1
  maxBound = 18

-- | 'EnumCode' is the Int32 assoicated with a
-- EnumValueDescriptorProto and is in the range 0 to 2^31-1.
newtype EnumCode = EnumCode { getEnumCode :: Int32 }  -- really [0..maxBound::Int32] of some .proto defined enumeration
  deriving (Eq,Ord,Read,Show,Num,Data,Typeable)

instance Bounded EnumCode where
  minBound = 0
  maxBound = 2147483647 -- 2^-31 -1

-- | 'WireSize' is the Int64 size type associated with the lazy
-- bytestrings used in the 'Put' and 'Get' monads.
type WireSize = Int64

-- | The 'Mergeable' class is not a 'Monoid', 'mergeEmpty' is not a
-- left or right unit like 'mempty'.  The default 'mergeAppend' is to
-- take the second parameter and discard the first one.  The
-- 'mergeConcat' defaults to @foldl@ associativity.
--
-- NOTE: 'mergeEmpty' has been removed in protocol buffers version 2.
-- Use 'defaultValue' instead.  New strict fields would mean that required
-- fields in messages will be automatic errors with 'mergeEmpty'.
class Default a => Mergeable a where
{-
  -- | The 'mergeEmpty' value of a basic type or a message with
  -- required fields will be undefined and a runtime error to
  -- evaluate.  These are only handy for reading the wire encoding and
  -- users should employ 'defaultValue' instead.
  mergeEmpty :: a
  mergeEmpty = error "You did not define Mergeable.mergeEmpty!"
-}
  -- | 'mergeAppend' is the right-biased merge of two values.  A
  -- message (or group) is merged recursively.  Required field are
  -- always taken from the second message. Optional field values are
  -- taken from the most defined message or the second message if
  -- both are set.  Repeated fields have the sequences concatenated.
  -- Note that strings and bytes are NOT concatenated.
  mergeAppend :: a -> a -> a
  mergeAppend _a b = b

  -- | 'mergeConcat' is @ F.foldl mergeAppend defaultValue @ and this
  -- default definition is not overridden in any of the code except
  -- for the (Seq a) instance.
  mergeConcat :: F.Foldable t => t a -> a
  mergeConcat = F.foldl mergeAppend defaultValue

-- | The Default class has the default-default values of types.  See
-- <http://code.google.com/apis/protocolbuffers/docs/proto.html#optional>
-- and also note that 'Enum' types have a 'defaultValue' that is the
-- first one in the @.proto@ file (there is always at least one
-- value).  Instances of this for messages hold any default value
-- defined in the @.proto@ file.  'defaultValue' is where the
-- 'MessageAPI' function 'getVal' looks when an optional field is not
-- set.
class Default a where
  -- | The 'defaultValue' is never undefined or an error to evalute.
  -- This makes it much more useful compared to 'mergeEmpty'. In a
  -- default message all Optional field values are set to 'Nothing'
  -- and Repeated field values are empty.
  defaultValue :: a

-- Returns Nothing if valid, and the position of the error if invalid
isValidUTF8 :: ByteString -> Maybe Int
isValidUTF8 bs = go 0 (L.unpack bs) 0 where
  go :: Int -> [Word8] -> Int -> Maybe Int
  go 0 [] _ = Nothing
  go 0 (x:xs) n | x <= 127 = go 0 xs $! succ n -- binary 01111111
                | x <= 193 = Just n            -- binary 11000001, decodes to <=127, should not be here
                | x <= 223 = go 1 xs $! succ n -- binary 11011111
                | x <= 239 = go 2 xs $! succ n -- binary 11101111
                | x <= 243 = go 3 xs $! succ n -- binary 11110011
                | x == 244 = high xs $! succ n -- binary 11110100
                | otherwise = Just n
  go i (x:xs) n | 128 <= x && x <= 191 = go (pred i) xs $! succ n
  go _ _ n = Just n
  -- leading 3 bits are 100, so next 6 are at most 001111, i.e. 10001111
  high (x:xs) n | 128 <= x && x <= 143 = go 2 xs $! succ n
                | otherwise = Just n
  high [] n = Just n

toUtf8 :: ByteString -> Either Int Utf8
toUtf8 bs = maybe (Right (Utf8 bs)) Left (isValidUTF8 bs)

uToString :: Utf8 -> String
uToString (Utf8 bs) = U.toString bs

uFromString :: String -> Utf8
uFromString s = Utf8 (U.fromString s)


-- Base types are not very mergeable, but their Maybe and Seq versions are:
instance Mergeable a => Mergeable (Maybe a) where
--    mergeEmpty = Nothing
    mergeAppend = mayMerge

{-# INLINE mayMerge #-}
mayMerge :: (Mergeable b) => Maybe b -> Maybe b -> Maybe b
mayMerge Nothing  y        = y
mayMerge x        Nothing  = x
mayMerge (Just x) (Just y) = Just (mergeAppend x y)

instance Mergeable (Seq a) where
--    mergeEmpty = empty
    mergeAppend = (><)

-- These all have errors as mergeEmpty and use the second paramater for mergeAppend
instance Mergeable Bool
instance Mergeable Utf8
instance Mergeable ByteString
instance Mergeable Double
instance Mergeable Float
instance Mergeable Int32
instance Mergeable Int64
instance Mergeable Word32
instance Mergeable Word64

instance Default Word64 where defaultValue = 0
instance Default Word32 where defaultValue = 0
instance Default Int64 where defaultValue = 0
instance Default Int32 where defaultValue = 0
instance Default Float where defaultValue = 0
instance Default Double where defaultValue = 0
instance Default Bool where defaultValue = False
instance Default (Maybe a) where defaultValue = Nothing
instance Default (Seq a) where defaultValue = mempty
instance Default ByteString where defaultValue = mempty
instance Default Utf8 where defaultValue = mempty
