{-# LANGUAGE GeneralizedNewtypeDeriving, RecordWildCards, BangPatterns #-}
-- Генерим:
--   * Структуры данных для urweb/haskell
--   * Сериализацию между urweb/haskell (instance UrData)
--   * инстансы Binary с сериализацией и версионностью

import Control.Monad
import Control.Monad.Trans
import Control.Monad.Reader
import System.IO
import System.Directory
import Data.IORef
import Data.List
import Data.Char
import System.Cmd
import System.Exit
data GenState
    = GenState
      { gsHs :: Handle
      , gsUr :: Handle
      , gsHsFFI :: Handle
      , gsCFFI :: Handle
      , gsUrFFI :: Handle
      , gsUrFFIur :: Handle
      , gsUrCalls :: Handle
      , gsUrp :: Handle
      , gsRiakIO :: Handle
--       , gsH  :: Handle
      , gsHsTypes :: IORef [String]
      , gsUrTypes :: IORef [Type]
      }
newtype Gen a = Gen { unGen :: ReaderT GenState IO a }
    deriving (Monad, MonadIO, Functor, MonadReader GenState)

genFile fn act = do
    r <- withFile tfn WriteMode act
    e <- doesFileExist gfn
    if e then do
        tf <- readFile tfn
        gf <- readFile gfn
        when (tf /= gf) mv
    else
        mv
    return r
    where tfn = "/tmp/" ++ fn
          gfn = "Generated/" ++ fn
          mv = void $ system $ "mv " ++ tfn ++ " " ++ gfn

runGen :: Gen a -> IO a
runGen g = do
    r <- genFile "DataTypes.hs" $ \ gsHs ->
         genFile "datatypes.ur" $ \ gsUr -> do
         genFile "HsFFI.hs" $ \ gsHsFFI -> do
         genFile "RiakIO.hs" $ \ gsRiakIO -> do
         genFile "CFFI.h" $ \ gsCFFI -> do
         genFile "ur_ffi.urs" $ \ gsUrFFI -> do
         genFile "ur_ffi.ur" $ \ gsUrFFIur -> do
         genFile "urCalls.ur" $ \ gsUrCalls -> do
         withFile "Generated/hsffi.urp" WriteMode $ \ gsUrp -> do
--          withFile "Generated/datatypes.h" WriteMode $ \ gsH -> do
            gsHsTypes <- newIORef []
            gsUrTypes <- newIORef []
            runReaderT (unGen g') (GenState {..})
    ExitSuccess <- system "cd Generated && derive DataTypes.hs && cd ../ && make -s strict_derive"
    return r
    where g' = do
              putHs "-- ФАЙЛ СГЕНЕРИРОВАН АВТОМАТИЧЕСКИ (см. Gen.hs) !!!\n\n\
                    \{-# LANGUAGE CPP, BangPatterns #-}\n\
                    \{-# OPTIONS_DERIVE --output=BinaryInstances_nonstrict.h #-}\n\n\
                    \-- | Описание структур данных, сохраняемых в Riak \n\
                    \-- и передаваемых между Ur/Web и Haskell\n\
                    \module Generated.DataTypes where\n\n\
                    \import qualified Data.ByteString.Char8 as B\n\
                    \import qualified Data.ByteString.Internal as B\n\
                    \import qualified Data.Text as T\n\
                    \import qualified Data.Text.Encoding as T\n\
                    \import qualified Data.HashSet as HS\n\
                    \import qualified Data.HashMap.Strict as HM\n\
                    \import Lib.UrTime\n\
                    \import Lib.UnsafeRef\n\
                    \import Lib.ReadSet (ReadSet)\n\
                    \import URL\n\
                    \import Data.Set (Set)\n\
                    \import qualified Data.Set as Set\n\
                    \import Data.Map (Map)\n\
                    \import qualified Data.Map as Map\n\
                    \import Data.IntMap (IntMap)\n\
                    \import Data.IntSet (IntSet)\n\
                    \import Data.Array\n\
                    \import Data.Binary\n\
                    \import Data.Binary.Get (getByteString)\n\
                    \import Data.List\n\
                    \import Data.Ord\n\
                    \import Data.Hashable\n\
                    \import Lib.BinaryInstances\n\
                    \\n\
                    \instance Hashable ItemTag where\n\
                    \    hashWithSalt s ITStarred = s `hashWithSalt` (0 :: Int)\n\
                    \    hashWithSalt s (ITTag t) = s `hashWithSalt` t\n\
                    \\n\
                    \ "
              putUr "open Binary\n"
              putUr "open Ur_ffi\n"
              -- почему-то не обрабатывало GHCRTS с более чем двумя параметрами
              -- поэтому захардкодил их тут
              putCFFI "#include <urweb.h>\n\
                      \#include \"HsFFI.h\"\n\n\
                      \#include \"Rts.h\"\n\n\
                      \uw_unit uw_Ur_ffi_init(uw_context ctx) { \n\
                      \static int argc = 6;\n\
                      \static char* arg0 = \"/tmp/coreader.exe\";\n\
                      \static char* argv[20]; argv[0]=arg0; int i; for(i=1; i<20;i++)argv[i]=NULL; \n\
                      \argv[1] = \"+RTS\";\n\
                      \argv[2] = \"-N6\";\n\
                      \argv[3] = \"-T\";\n\
                      \argv[4] = \"-A10m\";\n\
                      \argv[5] = \"-I0\";\n\
                      \argv[6] = \"-M15G\";\n\
                      \static char** argv_ = argv;\n\
                      \RtsConfig conf = defaultRtsConfig;\n\
                      \/*conf.rts_opts = \"-N4 -I0 -T -A2M\";*/\n\
                      \conf.rts_opts_enabled = RtsOptsAll;\n\
                      \setvbuf(stdout,NULL,_IOLBF,0);\n\
                      \/*puts(getenv(\"GHCRTS\"));*/\n\
                      \hs_init_ghc(&argc, &argv_, conf);\n\
                      \return 0; }\n\n\
                      \uw_Basis_bool uw_Js_alwaysFalse(uw_context ctx, uw_Basis_unit x) { \n\
                      \return 0; }\n\n"
              putRiakIO
                  "{-# LANGUAGE TypeFamilies, OverloadedStrings #-}\n\
                  \module Generated.RiakIO where\n\
                  \import Generated.DataTypes\n\
                  \import Data.Maybe\n\
                  \import Control.Monad\n\
                  \import qualified Data.Text as T\n\
                  \import Resolvables\n\
                  \import Riak\n\
                  \import Lib.UrTime\n\
                  \import URL\n\
                  \import System.IO.Unsafe\n\
                  \ "
              putHsFFI "{-# LANGUAGE ForeignFunctionInterface, OverloadedStrings #-}\n\
                     \module Generated.HsFFI where\n\
                     \import Generated.DataTypes\n\
                     \import Generated.RiakIO\n\
                     \import URL (TURL)\n\
                     \import Lib.UrTime\n\
                     \import UrCalls\n\
                     \import Discovery (searchSubscriptions)\n\
                     \import API\n\
                     \import Auth\n\
                     \import Data.Binary\n\
                     \import qualified Data.Text as T\n\
                     \import qualified Data.ByteString.Char8 as B\n\
                     \import qualified Data.ByteString.Lazy as BL\n\
                     \import qualified Data.ByteString.Internal as B\n\
                     \import qualified Data.ByteString.Unsafe as B\n\
                     \import qualified Data.Map as Map\n\
                     \import qualified Control.Exception as E\n\
                     \import Control.Concurrent\n\
                     \import System.IO.Unsafe\n\
                     \import System.IO\n\
                     \import Foreign hiding (unsafePerformIO)\n\
                     \import Foreign.C.Types\n\n\
                     \type Ctx = Ptr ()\n\n\
                     \retMap :: MVar (Map.Map Ctx B.ByteString)\n\
                     \retMap = unsafePerformIO $ newMVar Map.empty\n\
                     \{-# NOINLINE retMap #-}\n\
                     \saveBSretPtr ctx bs = do\n\
                     \    modifyMVar_ retMap $ \\ m -> do\n\
                     \        let r = Map.insert ctx bs m\n\
                     \            s = Map.size r\n\
                     \        --print s\n\
                     \        --hFlush stdout\n\
                     \        s `seq` return r\n\
                     \    -- сохраняем строку, дабы сборщик ее не убил \n\
                     \    withForeignPtr fp $ \\ p -> return (p `plusPtr` s)\n\
                     \    where (fp, s, l) = B.toForeignPtr bs\n\
                     \peekArg :: Binary a => Ptr () -> IO a\n\
                     \peekArg ptr = do\n\
                     \    len <- peek (ptr `plusPtr` (-8)) :: IO Int\n\
                     \    s <- B.create len (\\ dst -> B.memcpy dst (castPtr ptr) (toEnum len))\n\
                     \    --print (\"peekArg\", s)\n\
                     \    return $ decode $ BL.fromChunks [s]\n\
                     \ret :: Ctx -> Either E.SomeException B.ByteString \n\
                     \    -> Ptr CLong -> IO (Ptr ())\n\
                     \ret ctx (Left ex) pLen = do\n\
                     \    poke pLen (toEnum $ 0 - B.length bs)\n\
                     \    saveBSretPtr ctx bs\n\
                     \    where bs' = B.pack (show ex)\n\
                     \          bs = if bs' /= \"\" then bs' else \"emtpy error?\"\n\
                     \ret ctx (Right bs) pLen = do\n\
                     \    poke pLen (toEnum $ B.length bs)\n\
                     \    saveBSretPtr ctx bs\n\
                     \enc a = B.concat $ BL.toChunks $ encode a \n\n\
                     \ "
              putUrCalls "open Binary"
              putUrCalls "open Ur_ffi"
              putUrCalls "open Datatypes\n"
              putUrp "ffi ur_ffi\n\
                     \library ../../lib/lib\n\
                     \include CFFI.h\n\
                     \link HsFFI\n\
                     \effectful Ur_ffi.init"
              putUrFFI "val init : transaction {}"
              r <- g
              putUrp "\ndatatypes\nurCalls"

              putHs "\n{-!"
              types <- getList gsHsTypes
              forM_ types $ \ t ->
                  putHs $ "deriving instance Binary " ++ t
              putHs "!-}\n#include \"BinaryInstances.h\""

              urTypes <- getList gsUrTypes
              putUr $ formatUrSerialization urTypes

              return r

getList listRef = fmap reverse . liftIO . readIORef =<< asks listRef
pushList listRef elt = do
    ref <- asks listRef
    liftIO $ modifyIORef ref (elt :)

putF f s = asks f >>= liftIO . flip hPutStrLn s

putRiakIO = putF gsRiakIO
putHsFFI = putF gsHsFFI
putUrFFI = putF gsUrFFI
-- putUrFFIur = putF gsUrFFIur
putUrCalls = putF gsUrCalls
putCFFI = putF gsCFFI
putHs = putF gsHs
putUr = putF gsUr
putUrp = putF gsUrp
-- putH  = putF gsH

data Field = String :. Type -- name type
    deriving Show
data Ctor = String :/ [Field]
    deriving Show
data UrWeb = NoUw | Uw | UwD
    deriving (Show, Eq)
data Type
    = Type
      { tUrWeb :: UrWeb
      , tName :: String
      , tPrefix :: String
      , tCtors :: [Ctor]
      }
    | Builtin String String -- haskell/urweb type
    | List Type
    | Tuple [Type]
    | PType String String [Type]
    deriving Show

formatHsType = formatHsType' True
formatHsType' top = go top False False
    where go top field br t = case t of
              Type {..}
                  | top -> "data " ++ tName ++ "\n"
                           ++ unlines (align "::" $ ident "    " $
                                       ctors tPrefix True tCtors)
                  | otherwise -> tName
              Builtin hs _
                  | field && hs `elem` ["Int", "UrTime"] ->
                      -- а вот Bool нельзя UNPACK, т.к. два конструктора
                      "{-# UNPACK #-} !" ++ hs
              Builtin hs _ | field -> "!" ++ hs
              Builtin hs ur -> hs
              List t -> "[" ++ go False False False t ++ "]"
              Tuple t ->
                  "(" ++ intercalate ", " (map (go False False False) t) ++ ")"
              PType hs ur t -> braces br $ hs ++ " "
                               ++ intercalate " " (map (go False False True) t)
          ctors _ _ [] =
              ["deriving (Show, Eq, Ord)"]
          ctors p first ((ctor :/ flds) : cs) =
              [(if first then "= " else "| ") ++ ctor]
              ++ ident "  " (fields p True flds)
              ++ ctors p False cs
          fields _ first []
              | first = []
              | otherwise = ["}"]
          fields p first ((name :. typ) : fs) =
              [(if first then "{ " else ", ") ++ p ++ name ++ " :: "
               ++ go False True False typ]
              ++ fields p False fs

lf (x:xs) = toLower x : xs

formatUrType = formatUrType' True
formatUrType' top = go top False
    where go top br t = case t of
              Type {..}
                  | top ->
                      if tUrWeb == UwD then
                           "datatype " ++ lf tName ++ "\n"
                           ++ unlines (align ":" $ ident "  " $
                                       ctors True tCtors)
                      else
                           "con " ++ lf tName ++ "\n"
                           ++ unlines (align ":" $ ident "  " $
                                       ctor tCtors)
                  | otherwise -> lf tName
              Builtin hs ur -> ur
              List t -> braces br $ "list " ++ go False True t
              Tuple t -> "(" ++ intercalate " * " (map (go False False) t) ++ ")"
              PType hs ur t -> braces br $ ur ++ " "
                               ++ intercalate " " (map (go False True) t)
          ctors _ [] =
              []
          ctors first ((ctor :/ flds) : cs) =
              [(if first then "= " else "| ") ++ ctor ++ ifFields " of" ""]
              ++ ifFields (ident "  " (fields True flds)) []
              ++ ctors False cs
              where ifFields t e | null flds = e
                                 | otherwise = t
          ctor [ctor :/ flds] =
              zipWith (++) ("= " : repeat "  ") (fields True flds)
          fields _ [] = ["}"]
          fields first ((name :. typ) : fs) =
              [(if first then "{ " else ", ") ++ name ++ " : "
               ++ go False False typ]
              ++ fields False fs

formatUrSerialization types =
    "fun recurseGets () = ()\n" ++ concatMap genGet types ++ "\n\n" ++
    "fun recursePuts () = ()\n" ++ concatMap genPut types ++ "\n\n" ++
    concatMap genBinary types
    where genBinary (Type {..}) =
              "val binary_" ++ lf tName ++ " : binary " ++ lf tName
                        ++ " = mkBinary put_"
                        ++ lf tName ++ " get_" ++ lf tName ++ "\n"
          genGet (Type {..}) =
              "and get_" ++ lf tName ++
              " b : (getBuf * " ++ lf tName ++ ") = \n" ++
              (if length tCtors /= 1 then
                   "  let val (b, c) = get_char b in case ord c of\n"
               else "  case 0 of\n" -- один конструктор
              ) ++
              unlines (ident "  " $ getCtors (tUrWeb == UwD)
                                             True (zip [0..] tCtors)) ++
              (if length tCtors /= 1 then "  end\n" else "")
          genPut (Type {..}) =
              "and put_" ++ lf tName ++ " b (x : " ++ lf tName ++ ") = \n" ++
              "  case x of\n" ++ unlines (ident "  " $
                                          putCtors (tUrWeb == UwD)
                                                   (zip [0..] tCtors)) ++
              "  \n"
          putNames [] = "  b\n"
          putNames ((n, t) : ns) = put t n (putNames ns)
          put t x next =
              "let val b = " ++ put_func t ++ " b " ++ x ++ " in\n"
              ++ next ++ "end\n"
          put_func t = case t of
              Type {..} -> "put_" ++ lf tName
              Builtin hs ur -> "put_" ++ ur
              List t -> "put_list (" ++ put_func t ++ ")"
              Tuple ts -> "(fn b t =>"
                          ++ putNames [ ("t." ++ show i, t)
                                      | (i,t) <- zip [1..] ts]
                          ++ ")"
              PType hs "assoc_list" [a,b] -> put_func $ List (Tuple [a,b])
              PType hs ur [t] -> "put_" ++ ur ++ " (" ++ put_func t ++ ")"
              PType hs ur [a,b] -> "put_" ++ ur ++ " (" ++ put_func a ++ ") (" ++ put_func b ++ ")"

          getNames [] r = "  " ++ r ++ "\n"
          getNames ((n, t) : ns) r = get t n (getNames ns r)
          get t x next =
              "let val (b, _" ++ x ++ ") = " ++ get_func t ++ " b  in\n"
              ++ next ++ "end\n"
          get_func t = case t of
              Type {..} -> "get_" ++ lf tName
              Builtin hs ur -> "get_" ++ ur
              List t -> "get_list (" ++ get_func t ++ ")"
              Tuple ts -> "(fn b =>"
                          ++ getNames [ (show i, t)
                                      | (i,t) <- zip [1..] ts]
                             ("(b,(" ++ intercalate ", "
                                      (map (("_" ++) . show) [1..length ts]) ++ "))")
                          ++ ")"
              PType hs "assoc_list" [a,b] -> get_func $ List (Tuple [a,b])
              PType hs ur ts -> "get_" ++ ur
                                ++ concat [" (" ++ get_func t ++ ")" | t <- ts]
          lf (x:xs) = toLower x : xs

          putCtors d [(_, (ctor :/ fields))] = -- один конструктор
              ["  | " ++ (if d then ctor else "")
                      ++ (if null fields then "" else " x") ++ " => \n"]
              ++ put1Ctor fields
          putCtors d cs = putCtors' d True cs
          putCtors' _ _ [] =
              []
          putCtors' d first ((n, (ctor :/ fields)) : cs) =
              [(if first then "    " else "  | ") ++ (if d then ctor else "")
              ++ (if null fields then "" else " x") ++ " => \n"
              ++ "        let val b = put_char b (chr " ++ show n ++ ") in"]
              ++ put1Ctor fields
              ++ ["      end"]
              ++ putCtors' d False cs
          put1Ctor fields =
              ident "      " (lines $ putNames [("x." ++ name, typ)
                                               | (name :. typ) <- fields])

          getCtors _ _ [] =
              ["  | n => error <xml>Oh, shi -- can't deserialize \
               \({[n]} is out of range)</xml>"]
          getCtors d first ((n, (ctor :/ fields)) : cs) =
              [(if first then "    " else "  | ") ++ show n ++ " => "]
              ++ ident "      "
                 (lines $ getNames [(name, typ) | (name :. typ) <- fields] $
                  if null fields then "(b, " ++ ctor ++ ")" else
                  "(b," ++ (if d then "\n   " ++ ctor else "") ++ "\n   {"
                       ++ unlines
                              [ p ++ name ++ " = _" ++ name
                              | (p,(name:._)) <- zip (" " : repeat "   , ") fields]
                       ++ "   })"
                 )
              ++ getCtors d False cs

braces br inner
    | br        = "(" ++ inner ++ ")"
    | otherwise = inner
ident prefix = map (prefix ++)
align prefix xs = map (aln 0) xs
    where maxPos = maximum $ map (pos 0) xs
          pos _ [] = -1
          pos n s@(_:xs)
              | prefix `isPrefixOf` s = n
              | otherwise = pos (n+1) xs
          aln _ [] = []
          aln n s@(x:xs)
              | prefix `isPrefixOf` s = replicate (maxPos - n) ' ' ++ s
              | otherwise = x : aln (n+1) xs

io = regFunc (Just "effectful")
benign = regFunc (Just "benignEffectful")
pure = regFunc Nothing
regFunc eff name args result = do
    putUrFFI $ "val " ++ name ++ "_ : " ++
             intercalate " -> " (map (const "string") args) ++
             " -> transaction string"
    putUrCalls $ "fun " ++ name ++ " " ++
             concat ["(x" ++ show i ++ " : " ++ urT arg ++ ")"
                     | (i, arg) <- iargs ]
             ++ " : transaction (" ++ urT result ++ ") = \n"
             ++ "  r <- " ++ name ++ "_ " ++
             concat ["(toHaskell x" ++ show i ++ ") " | i <- is ]
             ++ "; return (fromHaskell r)\n"
    putCFFI $ "extern HsPtr uw_HsFFI_" ++ name ++ "(HsPtr ctx, HsPtr pLen"
                ++ concat [", HsPtr a" ++ show i | i <- is] ++ ");"
    putCFFI $ uwStr ++ " uw_Ur_ffi_" ++ name ++ "_(uw_context ctx, " ++
            intercalate ", " [uwStr ++ " x" ++ show i | i <- is]
            ++ ")\n{\n    long size;\n"
            ++ "    char* cr = uw_HsFFI_" ++ name ++ "(ctx, &size, "
            ++ intercalate ", " ["x" ++ show i | i <- is] ++ ");\n"
            ++ "    long sz = size >= 0 ? size : -size;\n"
            ++ "    " ++ uwStr ++ " r = uw_malloc(ctx, sz + 1);\n"
            ++ "    memcpy(r, cr, sz);\n"
            ++ "    r[sz] = '\\0';\n"
            ++ "    if (size >= 0) return r; else uw_error(ctx, FATAL, r);\n"
            ++ "\n}\n"
    putHsFFI $ "foreign export ccall uw_HsFFI_" ++ name ++
             " :: Ctx -> Ptr CLong -> " ++
             intercalate " -> " (map (const "Ptr ()") args) ++
             " -> IO (Ptr ())\nuw_HsFFI_" ++ name ++ " ctx pLen " ++
             concat [" x" ++ show i | i <- is] ++ " = do\n" ++
             concat ["    h" ++ show i ++ " <- peekArg x" ++ show i
                     ++ " :: IO (" ++ hsT arg ++ ")\n"
                     | (i, arg) <- iargs] ++
             "    r <- E.try $ do\n" ++
             "        r <- " ++ name ++ concat [" h" ++ show i | i <- is]
                     ++ " :: IO (" ++ hsT result ++ ")\n" ++
             "        let bs = enc r\n" ++
             "        B.length bs `seq` return bs\n" ++
             "    ret ctx r pLen\n"
    case eff of
        Just e -> putUrp $ e ++ " Ur_ffi." ++ name ++ "_"
        Nothing -> return ()
    where uwStr = "uw_Basis_string"
          iargs = zip [1..] args
          is = map fst iargs

urT = formatUrType' False
hsT = formatHsType' False

regRiak = regRiakP "riakPool"
regRiakP pool t@(Type {..}) = regRiak' pool tName t
regRiak' pool bucket t@(Type {..}) key checkTime cacheTime cacheSizeInMB = do
    fields <- case tCtors of
        [ctor :/ fields] -> return fields
        _ -> err "Single ctor expected"
    keyType <- case find (\ (n :. _) -> n == key) fields of
        Just (_ :. keyType) -> return keyType
        _ -> err $ "Key " ++ show key ++ " not found"
    putRiakIO $
        "instance KV " ++ tName ++ " where\n" ++
        "    type Key " ++ tName ++ " = " ++ hsT keyType ++ "\n" ++
        "    kvBucket _ = " ++ show bucket ++ "\n" ++
        "    kvKey = " ++ tPrefix ++ key ++ "\n" ++
        "    kvCache _ = " ++ cacheVar ++ "\n" ++
        "    kvPool _ = " ++ pool ++ "\n" ++
        cacheVar ++ " = unsafePerformIO $ newCache "
                 ++ show checkTime ++ " " ++ show cacheTime
                 ++ " (" ++ show cacheSizeInMB ++ "*1024*1024)\n" ++
        "{-# NOINLINE " ++ cacheVar ++ " #-}"
    rfunc ("read" ++) [keyType] (maybe_ t)
    rfunc ("cachedRead" ++) [keyType] (maybe_ t)
    rfunc ("cachedNothingRead" ++) [keyType] (maybe_ t)
    func ("mergeWrite" ++) [t] unit
--    func ("write" ++) [t] t
    func ("delete" ++) [t] unit
    rfunc (\ x -> "readMany" ++ x ++ "s") [List keyType] (List (maybe_ t))
    rfunc (\ x -> "cachedReadMany" ++ x ++ "s") [List keyType] (List (maybe_ t))
    rfunc (\ x -> "cachedNothingReadMany" ++ x ++ "s") [List keyType] (List (maybe_ t))
    func (\ x -> "writeMany" ++ x ++ "s") [List t] unit
    putRiakIO $ "modify" ++ tName ++ " :: "  ++ hsT keyType
                  ++ " -> (Maybe " ++ tName ++ " -> IO (" ++ tName ++ ", b))"
                  ++ " -> IO b\n"
                  ++ "modify" ++ tName ++ " = modifyKV\n"
    putRiakIO $ "modify" ++ tName ++ "_ :: "  ++ hsT keyType
                  ++ " -> (Maybe " ++ tName ++ " -> IO " ++ tName ++ ")"
                  ++ " -> IO ()\n"
                  ++ "modify" ++ tName ++ "_ = modifyKV_\n"

    putRiakIO $ "modify" ++ tName ++ "' :: "  ++ hsT keyType
                  ++ " -> (" ++ tName ++ " -> IO (" ++ tName ++ ", b))"
                  ++ " -> IO b\n"
                  ++ "modify" ++ tName ++ "' key f "
                  ++ "= modifyKV key (f . fromMaybe (default" ++ tName
                  ++ " key))\n"
    putRiakIO $ "modify" ++ tName ++ "'_ :: "  ++ hsT keyType
                  ++ " -> (" ++ tName ++ " -> IO " ++ tName ++ ")"
                  ++ " -> IO ()\n"
                  ++ "modify" ++ tName ++ "'_ key f "
                  ++ "= modifyKV_ key (f . fromMaybe (default" ++ tName
                  ++ " key))\n"
    putRiakIO $ "read" ++ tName ++ "' :: Key "
                  ++ tName ++ " -> IO " ++ tName ++ "\n"
                  ++ "read" ++ tName ++ "' key "
                  ++ "= liftM (fromMaybe (default" ++ tName
                  ++ " key)) (readKV key)\n"
    putRiakIO $ "cachedRead" ++ tName ++ "' :: Key "
                  ++ tName ++ " -> IO " ++ tName ++ "\n"
                  ++ "cachedRead" ++ tName ++ "' key "
                  ++ "= liftM (fromMaybe (default" ++ tName
                  ++ " key)) (cachedReadKV key)\n"
    putRiakIO $ "cachedNothingRead" ++ tName ++ "' :: Key "
                  ++ tName ++ " -> IO " ++ tName ++ "\n"
                  ++ "cachedNothingRead" ++ tName ++ "' key "
                  ++ "= liftM (fromMaybe (default" ++ tName
                  ++ " key)) (cachedNothingReadKV key)\n"
    where err s = fail $ "regRiak " ++ tName ++ ": " ++ s
          cacheVar = "_" ++ tPrefix ++ "Cache"
          rfunc = func' io -- benign
          func = func' io
          func' f mkName args r = do
              putRiakIO $
                  name ++ " :: "
                  ++ intercalate " -> " (map hsT args)
                  ++ " -> IO (" ++ hsT r ++ ")\n"
                  ++ name ++ " = " ++ mkName "KV" ++ "\n"
              when (tUrWeb == Uw) $
                  f name args r
              where name = mkName tName



array_ t = PType "Array Int" "array???" [t]
maybe_ t = PType "Maybe" "option" [t]
intMap t = PType "IntMap" "intMap???" [t]
set_ t = PType "Set" "map???" [t]
map_ k v = PType "Map" "assoc_list" [k, v]
either_ l r = PType "Either" "either" [l, r]
unsafeRef t = PType "UnsafeRef" "option" [t]
hashSet t = PType "HS.HashSet" "option" [t]
hashMap k v = PType "HM.HashMap" "assoc_list" [k, v]


int = Builtin "Int" "int"
double = Builtin "Double" "float"
bool = Builtin "Bool" "bool"
bs = Builtin "T.Text" "string"
urId = Builtin "T.Text" "Basis.id"
blob = Builtin "B.ByteString" "blob"
xhead = Builtin "T.Text" "xhead"
xbody = Builtin "T.Text" "xbody"
page = Builtin "T.Text" "page"
unit = Builtin "()" "{}"
guid = bs
url = Builtin "TURL" "url"
time = Builtin "UrTime" "time"
intSet = Builtin "IntSet" "intSet???"

regType t = do
    case t of
        Type {..} -> do
            pushList gsHsTypes tName
            when (tUrWeb /= NoUw) $
                regUrType t
        _ ->
            return ()
    putHs $ formatHsType t
    return t
regUrType t = do
    putUr $ formatUrType t
--    putUrFFI $ formatUrType t
    --  ^ не может сделать urlify/unurlify
    --  а если сделать одновременно и .urs и .ur, то ошибка Impossible 9
    pushList gsUrTypes t

main = runGen $ do

    stats <- regType $ Type NoUw "Stats" "stats"
        ["Stats" :/
            [ "Key" :. bs
            , "Map" :. map_ bs int
            ]]
    regRiak stats "Key" 0 0 0

    --  при подписке мы добавляем задачу на подписку
    --  и ждем, пока в subscriptionInfo не появится свежая инфа об url.
    subscriptionState <- regType $ Type UwD "SubscriptionState" "ss"
        [ "SSAdded" :/ []
        , "SSScanning" :/ [ "StartTime" :. time ]
        , "SSError" :/ [ "Message" :. bs ]
        , "SSFeed" :/ [ "Url" :. bs ]
--        , "SSNewFeed" :/ [ "Url" :. bs ]
-- потом написать свое сравнение для resolve-а
        ]

    subscription <- regType $ Type Uw "Subscription" "s"
        ["Subscription" :/
            [ "Url" :. bs
            , "State" :. subscriptionState
            , "EditsCount" :. int
            , "Title" :. maybe_ bs -- имя (пользовательское)
            , "Folders" :. List bs -- пользовательские папки
            ]]

    postsViewMode <- regType $ Type UwD "PostsViewMode" "pvm"
        [ "PVMShort" :/ []
        , "PVMFull" :/ []
        , "PVMMagazine" :/ []
        , "PVMMosaic" :/ []
        ]

    msgTreeViewMode <- regType $ Type Uw "MsgTreeViewMode" "mtvm"
        [ "MsgTreeViewMode" :/
            [ "Ascending"     :. bool
            , "UnreadOnly"    :. bool
            , "ExpandedComments" :. bool
            , "Posts"     :. postsViewMode
            , "FolderExpanded" :. bool
              --  ^ не относится к msgTreeViewMode, но наиболее удобно
              --    засунуть режим сюда
            , "NoOverride" :. bool
              -- view mode тега должен заменять остальные
            ]]

    payment <- regType $ Type UwD "Payment" "p"
        [ "PReserved" :/ []
        , "PFastSpring" :/
            [ "OrderId" :. bs
            , "OrderType" :. bs
            , "OrderTime" :. time ]
        ]

    paidTill <- regType $ Type UwD "PaidTill" "pt"
        [ "PTUnknown" :/ [] -- для совместимости
        , "PTPaid" :/
            [ "Till" :. time ]
        , "PTFreeTrial" :/
            [ "Till" :. time ]
        , "PTFreeTrialFinished" :/
            [ "Till" :. time ]
        , "PTPaidFinished" :/
            [ "Till" :. time ]
        ]

    let v x = Tuple [int, x] -- versioned

    userViewMode <- regType $ Type NoUw "UserViewMode" "uvm"
        ["UserViewMode" :/
            [ "PaidTill" :. paidTill
            , "OnlyUpdatedSubscriptions" :. v bool
            , "SubViewModes" :. hashMap bs (v msgTreeViewMode)
            , "FolderViewModes" :. hashMap bs (v msgTreeViewMode)
            , "SubUrlRenames" :. List (Tuple [time, bs, bs])
              -- храним за последние сутки,
              -- удаляем/обновляем в checkSubscriptions,
              -- time = SSScanning StartTime
            ]]

    user <- regType $ Type NoUw "User" "u"
        ["User" :/
            [ "Id" :. bs  -- key
            , "Subscriptions" :. List subscription
            , "ViewMode" :. userViewMode
            , "Payments" :. List payment
            ]]
    regRiak user "Id" 240 240 200

    userFilters <- regType $ Type NoUw "UserFilters" "uf"
        ["UserFilters" :/
            [ "User" :. bs  -- key
            , "Filters" :. List (Tuple [time, bs])
            , "Reserved1" :. List bs
            , "Reserved2" :. List bs
            , "Reserved3" :. List bs
            ]]
    regRiak userFilters "User" 240 240 10

    scrollMode <- regType $ Type UwD "ScrollMode" "sm"
        [ "SMNormal" :/ []
        , "SMQuick" :/ []
        , "SMImmediate" :/ []
        ]
    listViewMode <- regType $ Type UwD "ListViewMode" "lvm"
        [ "LVMCompact" :/ []
        , "LVMTwoLines" :/ []
        ]
    markReadMode <- regType $ Type UwD "MarkReadMode" "mrm"
        [ "MRMOnScroll" :/ []
        , "MRMManual" :/ []
        , "MRMOnScrollEverywhere" :/ []
        ]
    publicFeedType <- regType $ Type UwD "PublicFeedType" "pft"
        [ "PFTAll" :/ []
        , "PFTFolder" :/
            [ "Folder" :. bs ]
        , "PFTTag" :/
            [ "TagName" :. bs ]
        , "PFTStarred" :/ []
        , "PFTAllTags" :/ []
        ]
    let publicFeedInfo = List $ Tuple [bs, bool, maybe_ bs]
    userSettings <- regType $ Type Uw "UserSettings" "ust"
        ["UserSettings" :/
            [ "User" :. bs  -- key
            , "EditsCount" :. int
            , "ScrollMode" :. scrollMode
            , "ListViewMode" :. listViewMode
            , "ShowFavicons" :. bool
            , "MarkReadMode" :. markReadMode
            , "UltraCompact" :. bool
            , "MobileLogin" :. maybe_ bs
            , "ExactUnreadCounts" :. bool
            , "PublicFeeds" :. maybe_ (map_ publicFeedType publicFeedInfo)
            , "Country" :. maybe_ bs
            , "Reserved6" :. maybe_ bs
            , "Reserved7" :. maybe_ bs
            , "Reserved8" :. maybe_ bs
            , "Reserved9" :. maybe_ bs
            ]]
    regRiak userSettings "User" 240 240 10

    publicFeed <- regType $ Type NoUw "PublicFeed" "pf"
        ["PublicFeed" :/
            [ "Id" :. bs  -- key
            , "User" :. maybe_ bs
            , "Reserved1" :. maybe_ bs
            , "Reserved2" :. maybe_ bs
            , "Reserved3" :. maybe_ bs
            , "Reserved4" :. maybe_ bs
            ]]
    regRiak publicFeed "Id" 240 240 10

    uid <- regType $ Type UwD "UID" "uid"
        [ "EMail" :/ [ "Id" :. bs ]
        , "Url" :/ [ "Id" :. bs ]
        ]

    mobileLogin <- regType $ Type NoUw "MobileLogin" "ml"
        ["MobileLogin" :/
            [ "Login" :. bs  -- key
            , "EditsCount" :. int
            , "UID" :. maybe_ uid
            , "PasswordHash" :. bs
            , "FeverApiKey" :. maybe_ bs
            , "Reserved2" :. maybe_ bs
            , "Reserved3" :. maybe_ bs
            , "Reserved4" :. maybe_ bs
            ]]
    regRiak mobileLogin "Login" 300 300 10

    feverApiKey <- regType $ Type NoUw "FeverApiKey" "fak"
        ["FeverApiKey" :/
            [ "Key" :. bs
            , "EditsCount" :. int
            , "UID" :. maybe_ uid
            , "Reserved1" :. maybe_ bs
            , "Reserved2" :. maybe_ bs
            , "Reserved3" :. maybe_ bs
            , "Reserved4" :. maybe_ bs
            ]]
    regRiak feverApiKey "Key" 300 300 10

    feverIds <- regType $ Type NoUw "FeverIds" "fi"
        ["FeverIds" :/
            [ "User" :. bs
            , "MaxId" :. int
            , "GRIds" :. intMap int -- fever id -> gr id
            , "FeverIds" :. intMap int -- gr id -> fever id
            , "LastRefresh" :. maybe_ time
            , "Reserved2" :. maybe_ bs
            , "Reserved3" :. maybe_ bs
            , "Reserved4" :. maybe_ bs
            ]]
    regRiak feverIds "User" 240 240 100

    userStats <- regType $ Type NoUw "UserStats" "us"
        ["UserStats" :/
            [ "Id" :. bs  -- key
            , "UID" :. uid
            , "FirstSignInTime" :. time
            , "FirstSignInDetails" :. List (Tuple [bs,bs])
            , "Counters" :. map_ bs int
            , "SubscriptionCounters" :. map_ bs (map_ bs int)
            , "Reserved1" :. map_ bs int
            , "Reserved2" :. map_ bs int
            , "Reserved3" :. map_ bs int
            , "Reserved4" :. map_ bs int
            ]]
    regRiak userStats "Id" 240 240 200

    mailQueue <- regType $ Type NoUw "MailQueue" "mq"
        ["MailQueue" :/
            [ "Id" :. bs  -- key
            , "Active" :. set_ bs
            , "Inactive" :. set_ bs
            ]]
    regRiak mailQueue "Id" 300 300 200

    session <- regType $ Type Uw "Session" "session"
        ["Session" :/
            [ "Key" :. bs  -- key
            , "Expire" :. time
            , "Cleared" :. bool
            , "User" :. bs ]]
    regRiak session "Key" 3600 3600 100

    --  в процессе сканирования мы вычитываем инфу об url-е,
    --  если time больше, чем N минут назад, добавляем в очередь на подписку
    --  и меняем состояние на SUScanning
    --  В противном случае используем инфу из SubscriptionUrlInfo
    --
    --     (вот стоит ли для фида проверять время???)
    --     Тут вопрос подписки на блог из нерабочего сайта
    --     По идее, проверка важна на случай redirect-а,
    --     а вот на случай ошибки можно и на posts подписаться
    --     (Можно даже говорить, что subscription url unavailable, but
    --     there was a feed with such address???)

    subscriptionUrlKind <- regType $ Type NoUw "SubscriptionUrlKind" "suk"
        [ "SUKError" :/ [ "Message" :. bs ]
        , "SUKFeed"  :/ [ "Url" :. url ]
        ]

    subscriptionUrlInfo <- regType $ Type NoUw "SubscriptionUrlInfo" "sui"
        [ "SubscriptionUrlInfo" :/
             [ "Url"  :. url
             , "Time" :. time
             , "Kind" :. subscriptionUrlKind
             ]]
    regRiak subscriptionUrlInfo "Url" 0 0 0

    attachment <- regType $ Type UwD "Attachment" "a"
        [ "AImage" :/
            [ "Url" :. url
            , "Width" :. maybe_ int
            , "Height" :. maybe_ int
            , "Title" :. maybe_ bs
            ]
        , "AAudio" :/
            [ "Url" :. url
            , "Mime" :. bs
            , "FileSize" :. maybe_ int
            , "Duration" :. maybe_ int
            , "Title" :. maybe_ bs
            ]
        , "AVideo" :/
            [ "Url" :. url
            , "Mime" :. bs
            , "FileSize" :. maybe_ int
            , "Duration" :. maybe_ int
            , "Title" :. maybe_ bs
            , "Width" :. maybe_ int
            , "Height" :. maybe_ int
            , "Poster" :. maybe_ url
            ]
        , "AIframe" :/
            [ "Url" :. url
            , "Xml" :. bs
            , "Duration" :. maybe_ int
            , "Title" :. maybe_ bs
            ]
        , "AOther" :/
            [ "Url" :. url
            , "Mime" :. bs
            , "FileSize" :. maybe_ int
            ]
        , "AGrOrigin" :/
            [ "Feed" :. url
            , "Guid" :. bs
            , "StreamTitle" :. bs
            , "HtmlUrl" :. bs
            ]
        ]

    msgKey <- regType $ Type Uw "MsgKey" "msgKey"
        ["MsgKey" :/
            [ "BlogFeedUrl"     :. bs
            , "PostGuid"        :. maybe_ guid
            , "CommentGuid"     :. maybe_ guid
            ]]
            -- так проще всего, но так будет дублирование для разных
            -- SubscriptionUrl.
            -- Хотя гуглоридер вообще умудряется кравлить несколько раз
            -- один фид с разным feed-id.
            -- Так получается, что никаких ppHashMsgId не нужно.
            -- Т.е. только link остается (и только для wfw:commentrss и ?thread)
        -- а может SubscriptionUrl : PostGuid [ : MessageGuid ]
        -- кстати, двоеточия не нужны, ключ сериализуем в Binary и усе
    msg <- regType $ Type Uw "Msg" "msg"
        ["Msg" :/
            [ "Key"     :. msgKey
            , "Attachments" :. List attachment
            , "Author"  :. bs
            , "AuthorUri"   :. maybe_ url
            , "AuthorEmail" :. bs
            , "AuthorPic"   :. maybe_ url
            , "Link"    :. maybe_ url
            , "Subject" :. bs
            , "Tags"    :. List bs
            , "Time"    :. maybe_ time
            , "DlTime"  :. time
            , "Text"    :. bs
            , "ShortText" :. bs
            , "Debug"   :. bs
      --       , "Feed"    :. maybe_ bs
      --       , "Next"    :. maybe_ bs
            ]]
    regRiak msg "Key" 60 60 10

    msgHeader <- regType $ Type Uw "MsgHeader" "mh"
        ["MsgHeader" :/
            [ "Guid" :. bs
            , "ContentHash" :. bs -- чтобы не дублировать по контенту
            , "Author"  :. bs
            , "AuthorPic"   :. maybe_ url
            , "Subject" :. bs
            , "Time"    :. maybe_ time
            , "DlTime"  :. time
            , "ShortText" :. bs -- для показывания в сокращенном виде
              -- без лишних залезаний в базу
            ]]

    msgTree <- regType $ Type NoUw "MsgTree" "mt"
         ["MsgTree" :/
            [ "Headers"  :. array_ msgHeader
              -- размер можно из массива узнать
              -- id (-1 root) -> [child]
            , "Children" :. intMap (set_ (Tuple [time, int]))
            ]]

    commentUrlState <- regType $ Type NoUw "CommentUrlState" "cus"
        [ "CUSNew" :/ []
        , "CUSError" :/ [ "Message" :. bs ]
        , "CUSRedirect" :/ [ "URL" :. bs ]
        , "CUSNoComments" :/ []
        , "CUSOK" :/ []
        ]

    blogPostsScanned <- regType $ Type NoUw "BlogPostsScanned" "bps"
        ["BlogPostsScanned" :/
            [ "BlogFeedUrl"   :. url -- ключ
            , "SubscribeTime" :. time
            , "Urls" :. map_ guid (map_ url (Tuple [time, maybe_ time, commentUrlState]))
                   --   [post -> [url -> startTime x scanTime x state]]
            ]]
    regRiak blogPostsScanned "BlogFeedUrl" 3 240 128

    posts <- regType $ Type NoUw "Posts" "p"
        ["Posts" :/
            [ "BlogFeedUrl"   :. url -- ключ
--            , "CurrentUrlKind" :. urlKind -- должен быть Feed, но вдруг что???
              --
            , "UpdatedComments" :. intSet
            , "RootMessage"   :. msg
            , "MsgTree"       :. msgTree
            , "TotalComments" :. int
              --  ^ заодно являются версией
            , "DeletedComments" :. int
              -- кол-во удаленных постов можно увидеть по bounds mtHeaders
            , "CommentCounts" :. intMap (intMap int)
                              -- postId -> totalComments(==version) -> count
              -- В большинстве случаев тут будет не дерево, а список,
              -- но, при подписке на страницу жж/disqus/ltu может быть
              -- и дерево.
              -- Если у отдельного элемента Children=[], а Size > 0,
              -- значит надо выгребать остатки из Comments.
              -- Число комментариев =
              --     mtSize pMsgTree - Map.size (mtChildren pMsgTree)

              -- кол-во комментов для заданного времени скачивания
              -- дабы можно было вырезать из CommentCounts старые
              -- версии.
            , "CCVersions" :. map_ time int
            ]]
    regRiak posts "BlogFeedUrl" 3 140 700
            -- проверяем каждые 3 сек,
            -- дабы на свежих подписках что-то обновлялось

    discoveryFeed <- regType $ Type NoUw "DiscoveryFeed" "df"
        ["DiscoveryFeed" :/
            [ "Url"   :. url -- ключ
            , "Title" :. bs
            , "Website" :. maybe_ bs
            , "Image" :. maybe_ bs
            , "Category" :. bs
            , "TranslatedCategory" :. hashMap bs bs
            , "Tags"  :. List bs
            , "TranslatedTags" :. hashMap bs (List bs)
            , "Subscribers" :. int
            , "NormalizedSubscribers" :. double
            , "PostsPerDay" :. double
            , "LastRefreshTime" :. time
            , "PaidCountries" :. hashMap bs int
              --  ^ только страны, где >10 человек, иначе нерепрезентативная
              --  выборка получается и сортируем уже по NormalizedSubscribers
            , "Countries" :. hashMap bs int
            , "Reserved1" :. int
            , "Reserved2" :. int
            , "Reserved3" :. int
            , "Reserved4" :. int
            ]]
    regRiak discoveryFeed "Url" 60 60 100

    postsClearTime <- regType $ Type NoUw "PostsClearTime" "pct"
        ["PostsClearTime" :/
            [ "BlogFeedUrl"   :. url -- ключ
            , "Time" :. time
            , "Reserved1" :. int
            , "Reserved2" :. int
            , "Reserved3" :. int
            , "Reserved4" :. int
            ]]
    regRiak postsClearTime "BlogFeedUrl" 300 300 100

    postsSubscribers <- regType $ Type NoUw "PostsSubscribers" "ps"
        ["PostsSubscribers" :/
            [ "BlogFeedUrl"   :. url -- ключ
            , "Actions" :. set_ (Tuple [time, bs, bool])
            , "Subscribers" :. hashSet bs
            , "Reserved1" :. int
            , "Reserved2" :. int
            , "Reserved3" :. int
            , "Reserved4" :. int
            ]]
    regRiak postsSubscribers "BlogFeedUrl" 300 300 100

    commentsKey <- regType $ Type Uw "CommentsKey" "ck"
        ["CommentsKey" :/
            [ "BlogFeedUrl" :. bs
            , "PostGuid"    :. guid
            ]]
    comments <- regType $ Type NoUw "Comments" "c"
        ["Comments" :/
            [ "Key"         :. commentsKey
            , "MsgTree"     :. msgTree
            ]]
    regRiak comments "Key" 3 60 20

    -- по-идее, тут должно быть дерево, чтобы при redirect-ах не добавлять
    -- одинаковые url-ы. Еще есть вариант -- вообще не копить parentPath,
    -- а только хранить parents -- но так будет много лишних случайных
    -- чтений.
    -- Лучше дерево.
    -- data Parent
    --      = List [Parent] | FirstScan URL Parent | Subscription
    -- Ни фига -- по одному пути можно подписаться, по другому нет.
    -- Оставляем как есть

    subscriptionParentUrl <- regType $ Type NoUw "SubscriptionParentUrl" "spu"
        [ "SpuRedirect"  :/ [ "Url" :. url ]
        , "SpuHtml"      :/ [ "Url" :. url , "Debug" :. bs ]
        ]
    parentUrl <- regType $ Type NoUw "ParentUrl" "pu"
        [ "PuRedirect"   :/ [ "Url" :. url ]
        , "PuHtml"       :/ [ "Url" :. url , "Debug" :. bs ]
        , "PuFeed"       :/ [ "Url" :. url , "Guid" :. maybe_ guid ]
          -- а нужен ли нам parsedHtml???
          -- по идее, нужен Feed (что-то с постами/комментами)
          -- и инфа о посте

          -- error-а нет, т.к. с него никуда не попадешь
          -- firstScan тоже нафиг не нужен, но они оба где-то в parent-ах
          -- должны быть
        , "PuCommentsFeed" :/ [ "Url" :. url ]
        ]
--     -- Не нужны такие списки
--     Redirect $ Post guid $ Feed "posts.rss"
--     ParsedHtml url -- может быть и в корне -- тоже RootFeed
--     -- по идее Html со всякой ботвой -- тоже Redirect??? -- нет!
--     ParsedHtml url $ Comment guid $ ParsedHtml url $ Post guid $ RootFeed "asdf"
--     Feed guid $ Comment guid $ Feed url $ Redirect url $ Html url $ Post guid $ RootFeed "asdf"
    subscriptionParentPath <- regType $ Type NoUw "SubscriptionParentPath" "spp"
        ["SubscriptionParentPath" :/
            [ "SubscriptionUrl" :. url
            , "Parents" :. List subscriptionParentUrl
            ]]
    parentPath <- regType $ Type NoUw "ParentPath" "pp"
        ["ParentPath" :/
            [ "BlogFeedUrl" :. url
            , "Parents" :. List parentUrl
            ]]
    urlToScan <- regType $ Type NoUw "UrlToScan" "uts"
        ["UrlToScan" :/
            [ "Url" :. url -- key
            , "RedownloadOptions" :. List bs
            , "DataHash" :. bs
            , "ModifyTime" :. time
            , "NextScanTime" :. time
            , "ErrorStartTime" :. maybe_ time
            , "ParentPaths" :. List parentPath
            , "SubscriptionParentPaths" :. List subscriptionParentPath
            ]]
    regRiakP "riakPool" urlToScan "Url" 0 0 0

    queueType <- regType $ Type NoUw "QueueType" "qt"
        [ "QTSubscription"  :/ []
        , "QTBlogFeed"      :/ []
        , "QTTemporary1"    :/ []
        , "QTNewComment1"   :/ []
        , "QTRescan1"       :/ []
        , "QTTemporary"     :/ []
        , "QTNewComment"    :/ []
        , "QTRescan"        :/ []
        ]
    scanList <- regType $ Type NoUw "ScanList" "sl"
        ["ScanList" :/
            [ "Time" :. time -- ключ, для подписки будет 0-9 секунд
            , "Urls" :. List (Tuple [url, queueType])
            ]]
    regRiakP "riakPool" scanList "Time" 0 0 0

    let readSet = Builtin "ReadSet" "ReadSet???"
--     commentsRead <- regType $ Type NoUw "CommentsRead" "cr"
--         ["CommentsRead" :/
--             [ "Key" :. Tuple [bs, commentsKey]
--             , "Set" :. readSet
--             ]]
--     regRiak commentsRead "Key" 300 100

    postsRead <- regType $ Type NoUw "PostsRead" "pr"
        ["PostsRead" :/
            [ "Key" :. Tuple [bs, url]
            , "Set" :. readSet
            , "TotalCommentsRead" :. int
            , "CommentsRead" :. intMap readSet
            , "IgnoredPosts" :. readSet
              -- инвариант: CommentsRead и IgnoredPosts не должны пересекаться
            , "IgnoredComments" :. intMap intSet
              --  ^ нигде не используется, можно keepunread сюда добавить
              --  по-идее должен быть readSet для постов и intMap readSet
              --  для комментариев, т.е. надо будет разбить на 8 maybe
              --  также mark all as read не должен его помечать прочитанным
            ]]
    regRiak postsRead "Key" 600 600 200

    postsTagged <- regType $ Type NoUw "PostsTagged" "pt"
        ["PostsTagged" :/
            [ "BlogFeedUrl" :. url
            , "Set" :. readSet
            , "Reserved1" :. int
            , "Reserved2" :. int
            , "Reserved3" :. int
            , "Reserved4" :. int
            ]]
    regRiak postsTagged "BlogFeedUrl" 120 120 200
    postsTaggedGuids <- regType $ Type NoUw "PostsTaggedGuids" "ptg"
        ["PostsTaggedGuids" :/
            [ "BlogFeedUrl" :. url
            , "Guids" :. intMap bs
            , "Reserved1" :. int
            , "Reserved2" :. int
            , "Reserved3" :. int
            , "Reserved4" :. int
            ]]
    regRiak postsTaggedGuids "BlogFeedUrl" 120 120 200

    itemTag <- regType $ Type UwD "ItemTag" "it"
        ["ITStarred" :/ []
        ,"ITTag" :/ [ "TagName" :. bs ]]

    removedFeedInfo <- regType $ Type NoUw "RemovedFeedInfo" "rfi"
        [ "RemovedFeedInfo" :/
            [ "StreamTitle" :. bs -- как называлась подписка у пользователя?
            , "HtmlUrl" :. bs
            , "Time" :. time
            , "Reserved" :. int
            ]
        ]
    grIds <- regType $ Type NoUw "GRIds" "gri"
        ["GRIds" :/
            [ "User"   :. bs
            , "LastId" :. int
            , "FeedIds" :. hashMap url int
            , "FolderIds" :. hashMap bs int -- для сортировки
            , "FeedUrls" :. intMap url
            , "FolderNames" :. intMap bs
            , "TaggedItems" :. map_ itemTag (List (Tuple [time, int, msgKey]))
              --  ^ Set??, только надо как то пересериализовать
              -- или добавить проверку на ordering
            , "ItemTags" :. intMap (List itemTag)
            , "Ordering" :. map_ bs (List int) -- folder -> [sortId]
            , "ActiveFeeds" :. maybe_ (hashSet bs)
            , "RemovedFeeds" :. maybe_ (hashMap bs removedFeedInfo)
              --  ^ необходимо для вывода
            , "GRTagsImported" :. bool
            , "Reserved4" :. bool
            , "Reserved5" :. bool
            , "Reserved6" :. bool
            , "Reserved7" :. bool
            , "Reserved8" :. bool
            ]]
    regRiak grIds "User" 200 200 200

    apiMode <- regType $ Type UwD "ApiMode" "am"
        ["AMNormal" :/ []
        ,"AMGRIdsOnly" :/
            [ "Fetch" :. bool
            , "Count" :. int
            , "Continuation" :. unsafeRef (maybe_ msgKey)
            , "MinDlTime" :. maybe_ time
            , "MaxDlTime" :. maybe_ time
            , "MaxTime" :. maybe_ time
            , "ExcludeTags" :. hashSet itemTag
            , "IncludeTags" :. hashSet itemTag
            , "ReadOnly" :. bool ]
        ,"AMDiscovery" :/
            [ "Url" :. bs ]
        ]

    -- запросы дерева сообщений
    msgTreePoint <- regType $ Type Uw "MsgTreePoint" "mtp"
        [ "MsgTreePoint" :/
            [ "ParentId"  :. int
              -- время и идентификатор первого _подходящего_ элемента
              -- (может и не быть)
            , "Time"      :. time
            , "Id"        :. int
            ]]
        -- точки хранятся на клиенте в подписках и меняются по мере пометки
        -- сообщений прочитанными

    postsReq <- regType $ Type Uw "PostsReq" "prq"
        [ "PostsReq" :/
            [ "BlogFeedUrl"   :. bs
            , "MsgTreePoint"  :. msgTreePoint
            , "TotalPosts"    :. int
            , "TotalComments" :. int
            ]]
    commentsReq <- regType $ Type Uw "CommentsReq" "crq"
        [ "CommentsReq" :/
            [ "Key" :. commentsKey
            , "PostId" :. int
            , "MsgTreePoint"  :. msgTreePoint
            , "TotalComments" :. int
            ]]

    treeReq <- regType $ Type UwD "TreeReq" "tr"
        [ "TRPosts" :/
            [ "Reqs" :. List postsReq
            ]
          -- nextReq для all items вернет новые MsgTreePoint
          -- при следующем запросе можно будет выбирать следующие
          -- 15 фидов (на стороне клиента)
          -- Для начала можно все передавать, фильтрация -- уже оптимизация
        , "TRComments" :/
            [ "OnExpand" :. bool
            , "Req" :. commentsReq
            ]
        , "TRSearch" :/
            [ "Query" :. bs
            , "Feeds" :. List (Tuple [bs,int,int])
              -- также, как и в MsgTreePoint положение _следующего_ сообщения
              -- если есть
            , "PostTime" :. time
            , "BlogFeedUrl" :. bs
            , "PostGuid" :. bs
              -- выдает все комменты поста целиком (хотя можно
              -- и TRSearchComments добавить)
            ]
        , "TRTags" :/
            [ "LastMsg" :. maybe_ msgKey
            , "Tags" :. maybe_ (List itemTag)
              -- если Nothing, то все теги
            ]
        ]

    -- результат запроса дерева сообщений
    msgView <- regType $ Type UwD "MsgView" "msgView"
        [ "MVFull" :/
            [ "Msg" :. msg ]
        , "MVShort" :/
            [ "Header" :. msgHeader
--             , "MsgKey" :. msgKey
            , "CachedMsg" :. maybe_ msg ]
        ]

    -- msgKey + id-шки. Можно было бы хранить guid->id в posts/comments,
    -- но это доп объем памяти (где-то 20% 60/300байт)
    msgId <- regType $ Type Uw "MsgId" "mid"
        ["MsgId" :/
            [ "MsgKey" :. msgKey
            , "FeedId" :. int
            , "PostId" :. int
            , "CommentId" :. maybe_ int
            ]]
    msgItem <- regType $ Type Uw "MsgItem" "mi"
        ["MsgItem" :/
            [ "MsgView" :. msgView
            , "MsgId" :. msgId
            -- номер в посте для запоминания прочитанности
--             , "PostId" :. int
--             , "CommentId" :. maybe_ int
            , "Read" :. bool
            , "Starred" :. bool
            , "Tags" :. List bs
            , "ReadLocked" :. bool
--             , "Ignored" :. bool
            -- feed не нужен, его можно по MsgKey из MsgView найти
            ]]
            -- Фактически collapsed/expanded+next+read будут отображаться на
            -- source-ы.

    let msgForest = Type UwD "MsgForest" "mf"
         ["MsgForest" :/
            [ "ResultsCount" :. int
            , "UnreadCount" :. int
              -- общий размер, а не подгруженный,
              -- для отображения числа комментариев
            , "List" :. List (Tuple [msgItem, msgForest])
            , "NextReq" :. maybe_ treeReq
            ]]
    regType msgForest

    -- идентификатор для пометки прочитанным. guid-а коммента/поста нет,
    -- т.к. работаем по числовому идентификатору.
    -- В принципе, возможна ситуация с кривым resolve-ом, когда id-шки
    -- сдвинутся и у пользователя пометится прочитанным не то сообщение,
    -- что ему хотелось, но чтобы этого избежать надо работать по guid-ам,
    -- а это сильно увеличивает объемы сохраняемой инфы (Set guid вместо
    -- сжатого ReadSet), пока избежать такой ситуации позволяет блокировка
    -- на обновление posts/comments, а при resolve после отвала ноды выбор
    -- posts/comments с большим maxId. Когда кравлер станет распределенным
    -- все равно проблем быть не должно, т.к. добавление сообщений с
    -- posts/comments возможно только для одного домена (агрегаторы с
    -- разными доменами могут только обновлять число комментариев), а
    -- кравлер будет параллелиться именно по доменам.

    loginType <- regType $ Type UwD "LoginType" "lt"
        ["Google" :/ []
        ,"Facebook" :/ []
        ,"Twitter" :/ []
        ,"OpenId" :/
            [ "URL" :. bs ]
        ]

    io "loginGetForwardUrl" [loginType, bs, bs, url] url
    io "loginCallback" [loginType, bs, url, bs] (Tuple [uid, maybe_ bs]) -- (either_ bs bs)
    io "fbTokenGetForwardUrl" [bs, url] url
    io "fbTokenCallback" [bs, url, bs] bs -- (either_ bs bs)

    io "importFromGoogleReaderGetForwardUrl" [bs, url] url
    io "importFromGoogleReaderCallback" [bs, bs, bs, bs] unit
    io "importStarredAndTaggedItemsFromGoogleReaderCallback" [bs, bs, bs, bs] unit

    io "userSubscribe" [bs, bs, maybe_ bs, List bs] bs
    io "userDiscoverySubscribe" [bs, bs, bs, bs, maybe_ bs, List bs] bs
    io "userRenameSubscription" [bs,bs,bs] unit
    io "userRenameFolder" [bs,bs,bs] bs
    io "userEditSubscriptionFolders" [bs,bs,bs,bool] unit
    io "userUnsubscribe" [bs, List bs] unit
    io "userRetrySubscription" [bs, bs] unit
    io "userOPML" [bs] bs
    io "opmlSubscriptions" [blob, bs] unit

    counters <- regType $ Type Uw "Counters" "c"
        ["Counters" :/
            [ "ReadPosts"       :. int
            , "ReadComments"    :. int
            , "TotalPosts"      :. int
            , "TotalComments"   :. int
            , "Scanning"        :. int
            , "ScanningComments" :. int
            , "Error"           :. int
            , "Feed"            :. int
            , "ScannedPercent"  :. int
            ]]

    sitFeedDetails <- regType $ Type NoUw "SITFeedDetails" "sit"
        [ "SITFeedDetails" :/
            [ "PointAllAsc"     :. maybe_ msgTreePoint
            , "PointUnreadAsc"  :. maybe_ msgTreePoint
            , "PointUnreadDesc" :. maybe_ msgTreePoint
            , "PointUnreadPostsOnlyAsc"  :. maybe_ msgTreePoint
            , "PointUnreadPostsOnlyDesc" :. maybe_ msgTreePoint ]
        ]

    subItemType <- regType $ Type UwD "SubItemType" "sit"
        [ "SITAll" :/ []
        , "SITSearch" :/
            [ "Query" :. bs ]
        , "SITFolder" :/
            [ "Folder" :. bs ]
        , "SITFeed" :/
            [ "Subscription"    :. subscription
            , "FeedLink"        :. maybe_ bs
            , "PointAllDesc"    :. maybe_ msgTreePoint
            ]
        , "SITTag" :/
            [ "TagName" :. bs ]
        , "SITStarred" :/ []
        , "SITAllTags" :/ []
        ]
    subItemRpc <- regType $ Type Uw "SubItemRpc" "sir"
        [ "SubItemRpc" :/
            [ "Hash" :. bs
            , "Index" :. int
            , "Title" :. bs
            , "SIType" :. subItemType
            , "Counters" :. counters
            , "ViewMode" :. msgTreeViewMode
            , "ParentFolders" :. List int
            , "DomIds" :. List int
            , "FaviconStyle" :. maybe_ bs
            , "GRId" :. int
            ]
        ]

    io "userSubscriptionsAndRenames" [bool, time, bs, bs, List bs, bs] (Tuple [maybe_ (Tuple [xbody, bs, List bs]), bs, List subItemRpc, bool, List (Tuple [time, bs, bs])])
    io "userSubscriptionsAndSettings" [bs, bs] (Tuple [maybe_ (Tuple [xbody, bs, List bs]), bs, List subItemRpc, Tuple [bool, bool, List (Tuple [time, bs, bs]), List bs, userSettings]])
--     io "testSubscriptions" [unit] (List subscription)
    io "orderNotification" [bs] payment
    io "checkOrder" [bs] payment
    io "getPaidTill" [bs] paidTill
    io "activeGRImportsCount" [unit] int
    io "activeGRImportNames" [unit] xbody

    io "getFeedDetails" [bs, bs] (Tuple [bs, maybe_ bs, maybe_ bs, msgTreeViewMode])

    io "tagsMsgForest" [apiMode, bs, maybe_ (List itemTag), msgTreeViewMode] msgForest
    let readCounters = List (Tuple [bs, int, int, int, int])
--    io "feedMsgForest" [apiMode, bs, bs, int, int, msgTreeViewMode] msgForest
    io "folderMsgForest" [apiMode, bs, readCounters, List postsReq, msgTreeViewMode] (Tuple [readCounters, msgForest])
    -- не subItemType, а конкретно SITFeed
    io "userGetTree" [apiMode, bs, msgTreeViewMode, List treeReq] (List (maybe_ msgForest))
    shareAction <- regType $ Type UwD "ShareAction" "sa" $
        map (\ a -> ("SA" ++ a) :/ [])
            ["EMail", "Twitter", "Facebook", "GooglePlus", "Tumblr"
            ,"Evernote", "Delicious", "Pinboard", "Pocket", "Readability"
            ,"Instapaper", "Translate"]

    bgAction <- regType $ Type UwD "BgAction" "ba"
        ["BGMarkMsgRead" :/
            [ "MsgId"      :. msgId
            , "Read"       :. bool
            , "TotalComments" :. int ]
        ,"BGAddTag" :/
            [ "MsgId"      :. msgId
            , "Tag"        :. itemTag ]
        ,"BGRemoveTag" :/
            [ "MsgId"      :. msgId
            , "Tag"        :. itemTag ]
        ,"BGSkipComments" :/
            [ "MsgId" :. msgId
            , "TotalComments" :. int ]
        ,"BGIgnorePost" :/
            [ "MsgId" :. msgId
            , "TotalComments" :. int ]
        ,"BGMarkBlogRead" :/
            [ "BlogFeedUrl" :. bs
            , "TotalPosts" :. int
            , "TotalComments" :. int ]
        ,"BGSetOnlyUpdatedSubscriptions" :/
            [ "Value" :. bool ]
        ,"BGSetFolderViewMode" :/
            [ "Folder" :. bs
            , "ViewMode" :. msgTreeViewMode ]
        ,"BGSetSubscriptionViewMode" :/
            [ "Url" :. bs
            , "ViewMode" :. msgTreeViewMode ]
--         ,"BGUnsubscribe" :/
--             [ "Url" :. bs ]
--         ,"BGRenameSubscription" :/
--             [ "Url" :. bs
--             , "NewTitle" :. bs ]
--         ,"BGRenameFolder" :/
--             [ "Folder" :. bs
--             , "NewTitle" :. bs ]
        ,"BGClearAllSubscriptions" :/ []
        ,"BGSaveFilterQuery" :/
            [ "Query" :. bs ]
        ,"BGSetScrollMode" :/
            [ "ScrollMode" :. scrollMode ]
        ,"BGSetListViewMode" :/
            [ "ListViewMode" :. listViewMode ]
        ,"BGSetMarkReadMode" :/
            [ "MarkReadMode" :. markReadMode ]
        ,"BGSetUltraCompact" :/
            [ "UltraCompact" :. bool ]
        ,"BGDragAndDrop" :/
            [ "What" :. subItemType
            , "InsertAfter" :. maybe_ subItemType
            , "SourceFolder" :. maybe_ bs
            , "TargetFolder" :. maybe_ bs
            ]
        ,"BGSetExactUnreadCounts" :/
            [ "Value" :. bool ]
        ,"BGSortAllFeedsAndFolders" :/ []
        ,"BGSortFolder" :/
            [ "Folder" :. bs ]
        ,"BGSortTags" :/ []
        ,"BGShareAction" :/
            [ "ShareAction" :. shareAction ]
        ,"BGSetCountry" :/
            [ "Country" :. bs ]
        ]

    io "performBgActions" [bs, List bgAction] bs
    --  ^ не benign, конечно, но попробуй докажи

--     io "markBlogRead" [bs, bs, int, int] (maybe_ subInfo)
--     io "markMsgsRead" [bs, List (Tuple [msgId, bool])] unit

    searchResults <- regType $ Type Uw "SearchResults" "sr"
        ["SearchResults" :/
            [ "Total"      :. int
            , "Took"       :. int
            , "TookReal"   :. int
            , "MsgForest"  :. msgForest
            ]]
    io "searchMsgForest" [bs, bs, readCounters, msgTreeViewMode] searchResults
    io "searchTagsMsgForest" [bs, bs, maybe_ (List itemTag), msgTreeViewMode] searchResults
    pure "htmlHead" [unit] xhead
    pure "htmlHeadMain" [unit] xhead
    pure "htmlHeadMainNoTranslate" [unit] xhead
    pure "htmlLikeButtons" [unit] xbody
    pure "htmlLandingScripts" [unit] page
    pure "htmlOpenIdSignInButton" [unit] xbody
    pure "htmlConversionLogin" [unit] xbody
    pure "version" [unit] bs
    pure "blessId" [bs] urId
    pure "parseQueryStringUtf8Only" [bs] (List (Tuple [bs,bs]))
    pure "buyLink" [bs, bs] bs
    pure "encodeURIComponent" [bs] bs
    pure "prettyUID" [bs] bs
    pure "textToXbody" [bs] xbody
    io "newSession" [uid, List (Tuple [bs,bs])] session
    io "clearSession" [bs] unit
    io "userEvent" [bs,bs,bs] unit
    io "initMailer" [unit] unit
    io "initApiServer" [unit] unit

    fullTextCache <- regType $ Type Uw "FullTextCache" "ftc"
        ["FullTextCache" :/
            [ "Url"        :. url
            , "Text"       :. either_ bs bs
            , "Time"       :. time
            , "Reserved1"  :. bool
            , "Reserved2"  :. bool
            ]]
    regRiak fullTextCache "Url" 60 60 200
    io "getFullText" [msgKey] (either_ bs bs)
    io "getUrTime_" [unit] time
--     io "isBeta" [unit] bool
    io "setMobileLogin" [bs,bs,bs,bs] bool
    io "tryGetFeverUser" [bs] (maybe_ bs)

    io "userEnablePublicFeed" [publicFeedType, bs] publicFeedInfo
    io "userDisablePublicFeed" [publicFeedType, bs] publicFeedInfo
    io "userGenerateNewPublicFeed" [publicFeedType, bs] publicFeedInfo

    io "searchSubscriptions" [bs,bs,bs] (maybe_ (Tuple [xbody, List (Tuple [bs, msgTreeViewMode])]))

    return ()
