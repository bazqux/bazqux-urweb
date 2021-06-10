{-# LANGUAGE GeneralizedNewtypeDeriving, RecordWildCards, BangPatterns #-}
-- Генерим:
--   * Структуры данных для urweb/haskell
--   * Сериализацию между urweb/haskell (instance UrData)
--   * инстансы Binary с сериализацией и версионностью

module Gen (main) where

import Control.Monad
import Control.Monad.Trans
import Control.Monad.Reader
import System.IO
import System.Directory
import System.Info
import Data.IORef
import Data.List
import Data.Char
import Data.Maybe
import System.Process
import System.Exit

data GenState
    = GenState
      { gsHs :: Handle
      , gsUr :: Handle
      , gsUrBinary :: Handle
      , gsHsFFI :: Handle
      , gsCFFI :: Handle
      , gsH_ffi_urs :: Handle
      , gsH_urs :: Handle
      , gsH_ur :: Handle
      , gsUrp :: Handle
      , gsAllowMult :: Handle
      , gsRiakIO :: Handle
      , gsHsTypes :: IORef [String]
      , gsUrTypes :: IORef [Type]
      }
newtype Gen a = Gen { unGen :: ReaderT GenState IO a }
    deriving (Monad, Applicative, MonadIO, Functor, MonadReader GenState)

genFile = genFile' Nothing
genFile' after fn act = do
    r <- withFile tfn WriteMode act
    e <- doesFileExist gfn
    if e then do
        mt <- getModificationTime gfn
        genMt <- getModificationTime "Gen.hs"
        tf <- readFile tfn
        gf <- readFile gfn
        when (tf /= gf -- || genMt >= mt
             ) mv
    else
        mv
    return r
    where tfn = "/tmp/" ++ fn
          gfn = "Generated/" ++ fn
          mv = do
              system $ "mv " ++ tfn ++ " " ++ gfn
              forM_ after $ \ a -> a gfn

runGen :: Gen a -> IO a
runGen g = do
    r <- genFile "DataTypes.hs" $ \ gsHs ->
         genFile "datatypes.ur" $ \ gsUr -> do
         withFile "Generated/datatypesBinary.ur" WriteMode $ \ gsUrBinary -> do
         genFile "HsFFI.hs" $ \ gsHsFFI -> do
         genFile "RiakIO.hs" $ \ gsRiakIO -> do
         genFile "CFFI.h" $ \ gsCFFI -> do
         genFile "h_ffi.urs" $ \ gsH_ffi_urs -> do
         genFile "h.urs" $ \ gsH_urs -> do
         genFile "h.ur" $ \ gsH_ur -> do
         genFile "h_ffi.urp" $ \ gsUrp -> do
         genFile' (Just runSH) "allow_mult.sh" $ \ gsAllowMult -> do
            gsHsTypes <- newIORef []
            gsUrTypes <- newIORef []
            runReaderT (unGen g') (GenState {..})
    ExitSuccess <-
        if os == "darwin" then
            runCmd derive
        else
            system derive
    return r
    where derive = "cabal exec derive -- Generated/DataTypes.hs && make -s strict_derive"
          runCmd x = system $ "bash -l -c \"" <> x <> "\""
          runSH x  = system $ "bash -l \"" <> x <> "\""
          g' = do
              putHs "-- ФАЙЛ СГЕНЕРИРОВАН АВТОМАТИЧЕСКИ (см. Gen.hs) !!!\n\n\
                    \{-# LANGUAGE CPP, BangPatterns, StandaloneDeriving, DeriveGeneric #-}\n\
                    \{-# OPTIONS_DERIVE --output=Generated/BinaryInstances_nonstrict.h #-}\n\n\
                    \-- | Описание структур данных, сохраняемых в Riak \n\
                    \-- и передаваемых между Ur/Web и Haskell\n\
                    \module Generated.DataTypes where\n\n\
                    \import qualified Data.ByteString.Short as SB\n\
                    \import qualified Data.Text as T\n\
                    \import qualified Data.HashSet as HS\n\
                    \import qualified Data.HashMap.Strict as HM\n\
                    \import Lib.UrTime\n\
                    \import qualified Lib.BArray as BA\n\
                    \import Lib.UnsafeRef\n\
                    \import Lib.ReadSet (ReadSet)\n\
                    \import URL\n\
                    \import Data.Scientific (Scientific)\n\
                    \import Data.Set (Set)\n\
                    \import Data.Map (Map)\n\
                    \import Data.IntMap (IntMap)\n\
                    \import Data.IntSet (IntSet)\n\
                    \import Data.Binary\n\
                    \import Data.Hashable\n\
                    \-- import Control.DeepSeq\n\
                    \import Lib.BinaryInstances()\n\
                    \-- import GHC.Generics\n\
                    \\n\
                    \instance Hashable ItemTag where\n\
                    \    hashWithSalt s ITStarred = s `hashWithSalt` (0 :: Int)\n\
                    \    hashWithSalt s (ITTag t) = s `hashWithSalt` t\n\
                    \\n\
                    \ "
              -- почему-то не обрабатывало GHCRTS с более чем двумя параметрами
              -- поэтому захардкодил их тут
              -- -c compaction дает зависания на несколько секунд, но меньший CPU
              -- -H без -c дает отжирание проца в начале работы
              putCFFI "#include <urweb.h>\n\
                      \#include \"HsFFI.h\"\n\n\
                      \#include \"Rts.h\"\n\n\
                      \uw_unit uw_H_ffi_init(uw_context ctx) { \n\
                      \static int argc = 7;\n\
                      \static char* arg0 = \"/tmp/coreader.exe\";\n\
                      \static char* argv[20]; argv[0]=arg0; int i; for(i=1; i<20;i++)argv[i]=NULL; \n\
                      \argv[1] = \"+RTS\";\n\
                      \argv[2] = \"-N4\";\n\
                      \argv[3] = \"-T\";\n\
                      \argv[4] = \"-A64m\";\n\
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
                     \import URL\n\
                     \import Lib.UrTime\n\
                     \import Lib.Log\n\
                     \import Lib.Stats\n\n\
                     \import UrCalls\n\
                     \import OPML\n\
                     \import Session\n\
                     \import Account\n\
                     \import Payments\n\
                     \import Preprocess\n\
                     \import Mailer\n\
                     \import Feedback\n\
                     \import Search\n\
                     \import UsageFlags\n\
                     \import Discovery (searchSubscriptions)\n\
                     \import APIServer\n\
                     \import Auth\n\
                     \import Data.Binary\n\
                     \import Data.List\n\
                     \import Text.HTML.TagSoup.Fast (escapeHtmlT)\n\
                     \import qualified Data.Text as T\n\
                     \import qualified Data.Text.Encoding as T\n\
                     \import qualified Data.ByteString.Char8 as B\n\
                     \import qualified Data.ByteString.Lazy as BL\n\
                     \import qualified Data.ByteString.Internal as B\n\
                     \import qualified Data.ByteString.Unsafe as B\n\
                     \import qualified Data.Map as Map\n\
                     \import qualified Control.Exception as E\n\
                     \import Control.Concurrent\n\
                     \import System.IO.Unsafe\n\
                     \import System.IO\n\
                     \import System.Timeout\n\
                     \import Foreign hiding (unsafePerformIO)\n\
                     \import Foreign.C.Types\n\n\
                     \import Lib.BinaryInstances\n\n\
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
                     \stripUserError :: String -> String\n\
                     \stripUserError s\n\
                     \    | isPrefixOf prefix s && isSuffixOf \")\" s =\n\
                     \        drop (length prefix) $ init s\n\
                     \    | otherwise = s\n\
                     \    where prefix = \"user error (\"\n\
                     \retErr :: String -> Ctx -> String\n\
                     \       -> Ptr CLong -> IO (Ptr ())\n\
                     \retErr func ctx err0 pLen = do\n\
                     \    logS $ \"Exception: \" ++ err\n\
                     \    incrStat \"exceptions\" 1\n\
                     \    poke pLen (toEnum $ 0 - B.length bs)\n\
                     \    saveBSretPtr ctx bs\n\
                     \    where err = func ++ \": \" ++ err0\n\
                     \          bs = T.encodeUtf8 $ T.pack err\n\
                     \ret :: String -> Ctx -> Either E.SomeException (Maybe B.ByteString) \n\
                     \    -> Ptr CLong -> IO (Ptr ())\n\
                     \ret func ctx (Left ex) pLen = do\n\
                     \    retErr func ctx (stripUserError $ show ex) pLen\n\
                     \ret func ctx (Right Nothing) pLen = do\n\
                     \    incrStat \"timeouts\" 1\n\
                     \    retErr func ctx \"Timeout\" pLen\n\
                     \ret _ ctx (Right (Just bs)) pLen = do\n\
                     \    poke pLen (toEnum $ B.length bs)\n\
                     \    saveBSretPtr ctx bs\n\
                     \enc a = B.concat $ BL.toChunks $ encode a \n\n\
                     \ "
              putH_ur
                  "open Binary\n\
                  \open H_ffi\n\
                  \open Datatypes\n\n\
                  \open DatatypesBinary\n\n\
                  \task initialize = fn () => H_ffi.init\n"
              putUrp "ffi h_ffi\n\
                     \library ../../lib/lib\n\
                     \include CFFI.h\n\
                     \link HsFFI\n\
                     \effectful h_ffi.init"
              putUrFFI "val init : transaction {}"
              putF gsAllowMult "buckets=("

              r <- g

              putF gsAllowMult
                   ")\n\
                   \for bucket in \"${buckets[@]}\"\n\
                   \do\n\
                   \   curl -v -XPUT -H \"Content-Type: application/json\" \\\n\
                   \        -d '{\"props\":{\"allow_mult\":true,\"last_write_wins\":false,\"dvv_enabled\":true}}' \\\n\
                   \        http://127.0.0.1:8098/riak/$bucket\n\
                   \done";

              putHs "\n{-!"
              types <- getList gsHsTypes
              forM_ types $ \ t -> do
                  putHs $ "deriving instance Binary " ++ t
              putHs "!-}\n#include \"BinaryInstances.h\""
--               types <- getList gsHsTypes
--               forM_ types $ \ t -> do
--                   putHs $ "instance Binary " ++ t
--                   putHs $ "instance NFData " ++ t

              urTypes <- getList gsUrTypes
              let (ur, urp) = formatUrSerialization urTypes
              putF gsUrBinary "open Datatypes\nopen Binary\n"
              putF gsUrBinary ur
              putUrp urp

              putUrp "\ndatatypes\ndatatypesBinary\nh"

              return r

getList listRef = fmap reverse . liftIO . readIORef =<< asks listRef
pushList listRef elt = do
    ref <- asks listRef
    liftIO $ modifyIORef ref (elt :)

putF f s = asks f >>= liftIO . flip hPutStrLn s

putRiakIO = putF gsRiakIO
putHsFFI = putF gsHsFFI
putUrFFI = putF gsH_ffi_urs
putH_ur = putF gsH_ur
putH_urs = putF gsH_urs
putCFFI = putF gsCFFI
putHs = putF gsHs
putUr = putF gsUr
putUrp = putF gsUrp
-- putH  = putF gsH

data Field = String :. Type -- name type
    deriving (Show, Eq)
data Ctor = String :/ [Field]
    deriving (Show, Eq)
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
    | PType Bool String String [Type]
    deriving (Show, Eq)

formatHsType = formatHsType' True
formatHsType' top typ = go top False False typ
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
              Builtin hs _ | field -> -- && hs `notElem` ["T.Text", "TURL"] ->
                               "!" ++ hs
              Builtin hs ur -> hs
              List t -> "[" ++ go False False False t ++ "]"
              Tuple t ->
                  "(" ++ intercalate ", " (map (go False False False) t) ++ ")"
              PType _ hs ur t -> braces br $ hs ++ " "
                               ++ intercalate " " (map (go False False True) t)
          ctors _ _ [] =
              ["deriving (Show, Eq" ++ (if hasOrd [] typ then ", Ord" else "")
               ++ "{-, Generic-})"]
              -- с Generic компилируется на 20 сек (~10%) дольше
              -- и кода на ~10% больше, разница небольшая,
              -- но пока работает derive, лучше уж пользоваться им
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


hasOrd p (Type {..})
    | tName `elem` p = True
    | otherwise =
        all (hasOrd (tName:p)) [t | (_ :/ flds) <- tCtors, (_ :. t) <- flds]
hasOrd _ (Builtin _ _) = True
hasOrd p (List t) = hasOrd p t
hasOrd p (Tuple t) = all (hasOrd p) t
hasOrd p (PType o _ _ t) = o && all (hasOrd p) t

lf (x:xs) = toLower x : xs

data FormatMode
    = Top
    | Unqualified
    | Qualified
    deriving (Show, Eq)

urT mode = go mode False
    where go mode br t = case t of
              Type {..} -> case mode of
                  Top ->
                      if tUrWeb == UwD then
                           "datatype " ++ lf tName ++ "\n"
                           ++ unlines (align ":" $ ident "  " $
                                       ctors True tCtors)
                      else
                           "con " ++ lf tName ++ " :: Type\n"
                           ++ unlines (align ":" $ ident "  " $
                                       ctor tCtors)
                  Unqualified -> lf tName
                  Qualified -> "Datatypes." ++ lf tName
              Builtin hs ur -> ur
              List t -> braces br $ "list " ++ go mode' True t
              Tuple t -> "(" ++ intercalate " * " (map (go mode' False) t) ++ ")"
              PType _ hs "assoc_list" [a,b] -> go mode br (List (Tuple [a,b]))
              PType _ hs "int_assoc_list" [a] -> go mode br (List (Tuple [int,a]))
              PType _ hs "int_list" [] -> go mode br (List int)
              PType _ hs ur t -> braces br $ ur ++ " "
                               ++ intercalate " " (map (go mode' True) t)
          mode'
              | mode == Top = Unqualified
              | otherwise   = mode
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
               ++ go mode' False typ]
              ++ fields False fs

formatUrSerialization types =
    (concatMap genGet types ++ "\n\n" ++
     concatMap genPut types ++ "\n\n" ++
     concatMap genBinary types
    ,concatMap genNeverInline types)
    where genBinary (Type {..}) =
              "val binary_" ++ lf tName ++ " : binary " ++ lf tName
                        ++ " = mkBinary put_"
                        ++ lf tName ++ " get_" ++ lf tName ++ "\n"
          genNeverInline (Type {..}) =
              "neverInline DatatypesBinary/get_" ++ lf tName ++ "\n" ++
              "neverInline DatatypesBinary/put_" ++ lf tName ++ "\n"
          genGet (Type {..}) =
              "fun get_" ++ lf tName ++
              " (b : getBuf) : (getBuf * " ++ lf tName ++ ") = \n" ++
              (if length tCtors /= 1 then
                   "  let val (b, c) = get_byte b in case c of\n"
               else "  case 0 of\n" -- один конструктор
              ) ++
              unlines (ident "  " $ getCtors tName (tUrWeb == UwD)
                                             True (zip [0..] tCtors)) ++
              (if length tCtors /= 1 then "  end\n" else "")
          genPut (Type {..}) =
              "fun put_" ++ lf tName ++ " (b : putBuf) (x : " ++ lf tName ++ ") : putBuf = \n" ++
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
              Builtin hs ur -> "put_" ++ dropModuleName ur
              List t -> "put_list (" ++ put_func t ++ ")"
              Tuple ts -> "(fn (b : putBuf) t => "
                          ++ putNames [ ("t." ++ show i, t)
                                      | (i,t) <- zip [1..] ts]
                          ++ ")"
              PType _ hs "assoc_list" [a,b] -> put_func $ List (Tuple [a,b])
              PType _ hs "int_assoc_list" [a] -> put_func $ List (Tuple [int,a])
              PType _ hs "int_list" [] -> put_func $ List int
              PType _ hs ur [t] ->
                  "put_" ++ dropModuleName ur ++ " (" ++ put_func t ++ ")"
              PType _ hs ur [a,b] ->
                  "put_" ++ dropModuleName ur ++ " (" ++ put_func a ++ ") (" ++ put_func b ++ ")"
          dropModuleName ur = case dropWhile (/= '.') ur of
              '.':xs -> xs
              _ -> ur
          getNames [] r = "  " ++ r ++ "\n"
          getNames ((n, t) : ns) r = get t n (getNames ns r)
          get t x next =
              "let val (b, _" ++ x ++ ") = " ++ get_func t ++ " b  in\n"
              ++ next ++ "end\n"
          get_func t = case t of
              Type {..} -> "get_" ++ lf tName
              Builtin hs ur -> "get_" ++ dropModuleName ur
              List t -> "get_list (" ++ get_func t ++ ")"
              Tuple ts -> "(fn (b : getBuf) =>"
                          ++ getNames [ (show i, t)
                                      | (i,t) <- zip [1..] ts]
                             ("(b,(" ++ intercalate ", "
                                      (map (("_" ++) . show) [1..length ts]) ++ "))")
                          ++ ")"
              PType _ hs "assoc_list" [a,b] -> get_func $ List (Tuple [a,b])
              PType _ hs "int_assoc_list" [a] -> get_func $ List (Tuple [int,a])
              PType _ hs "int_list" [] -> get_func $ List int
              PType _ hs ur ts -> "get_" ++ dropModuleName ur
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
              ++ "        let val b = put_byte b " ++ show n ++ " in"]
              ++ put1Ctor fields
              ++ ["      end"]
              ++ putCtors' d False cs
          put1Ctor fields =
              ident "      " (lines $ putNames [("x." ++ name, typ)
                                               | (name :. typ) <- fields])

          getCtors tName _ _ [] =
              ["  | n => error <xml>Oh, shi -- can’t deserialize "
               <> tName <> " ({[n]} is out of range)</xml>"]
          getCtors tName d first ((n, (ctor :/ fields)) : cs) =
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
              ++ getCtors tName d False cs

braces :: Bool -> String -> String
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

io = regFunc (Just "effectful") Nothing
benign = regFunc (Just "benignEffectful") Nothing
func = regFunc Nothing Nothing
func' n h = regFunc Nothing (Just h) n
regFunc eff hName name args result = do
    putUrFFI $ "val " ++ name ++ "_ : "
        ++ intercalate " -> "
            (map (const "string") args ++ [e "" "transaction " ++ "string"])
    let urCall = name ++ "_ " ++
            concat ["(toHaskell x" ++ show i ++ ") " | i <- is ]
    putH_ur $ (if null args then "val " else "fun ") ++ name ++ " " ++
        concat ["(x" ++ show i ++ " : " ++ urT Unqualified arg ++ ")"
                | (i, arg) <- iargs ]
        ++ " : " ++ e "" "transaction" ++ " (" ++ urT Unqualified result ++ ") = \n"
        ++ e ("  fromHaskell (" ++ urCall ++ ")")
             ("  r <- " ++ urCall ++ "; return (fromHaskell r)")
    putH_urs $ "val " ++ name ++ " : " ++
        concat [urT Qualified arg ++ " -> "
                | (i, arg) <- iargs ]
        ++ e "" "transaction" ++ " (" ++ urT Qualified result ++ ")\n"
    putCFFI $ "extern HsPtr uw_HsFFI_" ++ name ++ "(HsPtr ctx, HsPtr pLen"
        ++ concat [", HsPtr a" ++ show i | i <- is] ++ ");"
    putCFFI $ uwStr ++ " uw_H_ffi_" ++ name ++ "_(" ++
        intercalate ", " ("uw_context ctx" : [uwStr ++ " x" ++ show i | i <- is])
        ++ ")\n{\n    long size;\n"
        ++ "    char* cr = uw_HsFFI_" ++ name ++ "(ctx, "
        ++ intercalate ", " ("&size" : ["x" ++ show i | i <- is]) ++ ");\n"
        ++ "    long sz = size >= 0 ? size : -size;\n"
        ++ "    " ++ uwStr ++ " r = uw_malloc(ctx, sz + 1);\n"
        ++ "    memcpy(r, cr, sz);\n"
        ++ "    r[sz] = '\\0';\n"
        ++ "    if (size >= 0) return r; else uw_error(ctx, FATAL, r);\n"
        ++ "\n}\n"
    putHsFFI $ "foreign export ccall uw_HsFFI_" ++ name ++
        " :: Ctx -> " ++
        intercalate " -> " ("Ptr CLong" : map (const "Ptr ()") args) ++
        " -> IO (Ptr ())\nuw_HsFFI_" ++ name ++ " ctx pLen " ++
        concat [" x" ++ show i | i <- is] ++ " = do\n" ++
        concat ["    h" ++ show i ++ " <- peekArg x" ++ show i
                ++ " :: IO (" ++ hsT arg ++ ")\n"
                | (i, arg) <- iargs] ++
        "    r <- E.try $ timeout (120*1000*1000) $ do\n" ++
        "        " ++ e "let r = " "r <- "
                ++ fromMaybe name hName ++ concat [" h" ++ show i | i <- is]
                ++ " :: " ++ e "" "IO " ++ "(" ++ hsT result ++ ")\n" ++
        "        let bs = enc r\n" ++
        "        return $! bs\n" ++
        "    ret " ++ show name ++ " ctx r pLen\n"
    case eff of
        Just e -> putUrp $ e ++ " H_ffi." ++ name ++ "_"
        Nothing -> return ()
    putUrp $ "neverInline H." ++ name
    where uwStr = "uw_Basis_string"
          iargs = zip [1..] args
          is = map fst iargs
          e pure io
              | isNothing eff = pure
              | otherwise = io

hsT = formatHsType' False

regRiak t@(Type {..}) = regRiak' tName t
regRiak' bucket t@(Type {..}) key checkTime cacheTime cacheSizeInMB = do
    fields <- case tCtors of
        [ctor :/ fields] -> return fields
        _ -> err "Single ctor expected"
    keyType <- case find (\ (n :. _) -> n == key) fields of
        Just (_ :. keyType) -> return keyType
        _ -> err $ "Key " ++ show key ++ " not found"
    putF gsAllowMult $ "    " <> tName
    putRiakIO $
        "instance KV " ++ tName ++ " where\n" ++
        "    type Key " ++ tName ++ " = " ++ hsT keyType ++ "\n" ++
        "    kvBucket _ = " ++ show bucket ++ "\n" ++
        "    kvKey = " ++ tPrefix ++ key ++ "\n" ++
        "    kvCache _ = " ++ cacheVar ++ "\n" ++
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
    putRiakIO $ "alter" ++ tName ++ " :: "  ++ hsT keyType
                  ++ " -> (Maybe " ++ tName ++ " -> IO (Maybe " ++ tName ++ ", b))"
                  ++ " -> IO b\n"
                  ++ "alter" ++ tName ++ " = alterKV\n"
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

array_ t = PType False "BA.Array Int" "array???" [t]
maybe_ t = PType True "Maybe" "option" [t]
intMap t = PType False "IntMap" "int_assoc_list" [t]
set_ t
     | t == unit = error "Never use “set_ unit”"
       -- если обновить структуру, но забыть перекомпилировать зависимости,
       -- можно получить ситуацию с чтением новых "битых" данных старым кодом,
       -- когда размер set_ unit (использовался для некоторых Reserved полей)
       -- будет кривой (не равен нулю), а unit в binary-представлении не
       -- занимает места, в итоге decode может пытаться создавать
       -- гигантский список [(),(),…], съедая всю память, так и не
       -- остановишись из-за закончившихся входных данных
     | otherwise = PType False "Set" "set???" [t]
hashSet t
     | t == unit = error "Never use 'hashSet unit'"
     | otherwise = PType False "HS.HashSet" "list" [t]
map_ k v = PType False "Map" "assoc_list" [k, v]
either_ l r = PType True "Either" "Either.either" [l, r]
unsafeRef t = PType False "UnsafeRef" "option" [t]
hashMap k v = PType False "HM.HashMap" "assoc_list" [k, v]
-- не сравниваем HashSet -- у них кривой instance Ord, да и не нужно оно нам,
-- Set/Map тоже не сравниваем за ненадобностью
intSet = PType True "IntSet" "int_list" []


int = Builtin "Int" "int"
money = Builtin "Scientific" "???"
double = Builtin "Double" "float"
bool = Builtin "Bool" "bool"
bs = Builtin "T.Text" "string"
xbodyString = Builtin "T.Text" "Binary_ffi.xbodyString"
junkText = Builtin "JunkText" "string"
sbs = Builtin "SB.ShortByteString" "string"
urId = Builtin "T.Text" "Basis.id"
blob = Builtin "B.ByteString" "blob"
xhead = Builtin "T.Text" "xhead"
page = Builtin "T.Text" "page"
xbody = Builtin "T.Text" "xbody"
xbodyBS = Builtin "B.ByteString" "xbody"
unit = Builtin "()" "{}"
guid = sbs
url = Builtin "TURL" "url"
time = Builtin "UrTime" "time"

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
    putUr $ urT Top t
--    putUrFFI $ urT Top t
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

    subscriptionParentUrl <- regType $ Type UwD "SubscriptionParentUrl" "spu"
        [ "SpuRedirect"  :/ [ "Url" :. bs ]
          -- redirect на заданный url
        , "SpuHtml"      :/ [ "Url" :. bs , "Debug" :. bs ]
          -- в html (parent-е) была ссылка на заданный фид
        ]

    --  при подписке мы добавляем задачу на подписку
    --  и ждем, пока в subscriptionInfo не появится свежая инфа об url.
    subscriptionState <- regType $ Type UwD "SubscriptionState" "ss"
        [ "SSAdded" :/ []
        , "SSScanning" :/ [ "StartTime" :. time ]
        , "SSError" :/ [ "Message" :. bs ]
        , "SSFeed" :/ [ "Url" :. bs ]
        , "SSErrorPath" :/
            [ "Message" :. bs
            , "Path" :. List subscriptionParentUrl ]
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

    mtvmEx <- regType $ Type UwD "MTVMEx" "mtvmex"
        -- расширение MsgTreeViewMode (нельзя добавить в сам msgtreeviewmode
        -- из-за бинарной несовместимости)
        [ "MTVMFolderCollapsed" :/ []
        , "MTVMFolderExpanded" :/ []
        , "MTVMEx" :/
            [ "FolderExpanded" :. bool
              --  ^ не относится к msgTreeViewMode, но наиболее удобно
              --    засунуть режим сюда
              -- FolderCollapsed | FolderExpanded
              --    | More { FolderExpanded :: True, GroupByFeed }
            , "GroupByFeed" :. bool
            , "Reserved1" :. bool
            , "Reserved2" :. int
            ]]
    msgTreeViewMode <- regType $ Type Uw "MsgTreeViewMode" "mtvm"
        [ "MsgTreeViewMode" :/
            [ "Ascending"     :. bool
            , "UnreadOnly"    :. bool
            , "ExpandedComments" :. bool
            , "Posts"     :. postsViewMode
            , "Ex" :. mtvmEx
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
        , "PFTSmartStream" :/
            [ "StreamName" :. bs ]
        ]
    let publicFeedInfo = List $ Tuple [bs, bool, maybe_ bs]
    -- список, т.к. при слиянии папок (из-за переименования) также сливаются
    -- их публичные фиды.
    loginAccessToken <- regType $ Type UwD "LoginAccessToken" "lat"
        [ "LATNone" :/ []
        , "LATFacebook" :/ [ "AccessToken" :. bs]
        , "LATTwitter"  :/ [ "Credentials" :. List (Tuple [bs,bs])]
        ]
    apiKeys <- regType $ Type Uw "ApiKeys" "ak"
        ["ApiKeys" :/
            [ "Pocket" :. maybe_ (Tuple [bs,bs]) -- (access_token, username)
            , "PocketRequest" :. maybe_ (Tuple [bs,bs,bs])
              -- (requestToken, url, title)
            , "Reserved10" :. maybe_ int -- (Tuple [time,bs,bs,bs])
              -- (time, key, name, e-mail)
            , "FacebookAccessToken" :. maybe_ (Tuple [time, bs])
            , "TwitterAccessToken" :. maybe_ (Tuple [time, List $ Tuple [bs,bs]])
            , "Reserved13" :. bool
            , "Reserved14" :. bool
            , "Reserved15" :. bool
            , "Reserved16" :. bool
            , "Reserved17" :. bool
            , "Reserved2" :. int
            , "Reserved3" :. int
            , "Reserved4" :. int
            ]]
    userExperiment <- regType $ Type UwD "UserExperiment" "ue"
        [ "UENo9" :/ []
        ]
    customShareAction <- regType $ Type Uw "CustomShareAction" "csa" $
        ["CustomShareAction" :/
            [ "Id" :. int
            , "Title" :. bs
            , "UrlFormat" :. bs
            , "Shorten" :. bool
            ]]
    shareAction <- regType $ Type UwD "ShareAction" "sa" $
        map (\ a -> ("SA" ++ a) :/ [])
            ["EMail", "Twitter", "Facebook", "GooglePlus"
            ,"Tumblr" -- 44/22USA  -- убрать
            ,"Evernote" -- 390
            ,"Delicious_discontinued" -- 2.5k/10k USA пора вниз
            ,"Pinboard" -- 23k/12k -- как delicious
            ,"Pocket" -- 800!
            ,"Readability_discontinued" -- 13k
            ,"Instapaper" -- 8.4 вместе с readability можно убрать
            ,"Translate"
             --  ^ старое
            ,"Blogger"  -- 75, но blogspot 42
            ,"Wordpress" -- 41 -- как blogspot
            ,"LinkedIn" -- 17/12 USA
            ,"Pinterest" -- 30/13 usa
            ,"VK" -- 20/2 Russia
            ,"Skype" -- 235
            ,"Reddit" -- 34/9
            ,"StumbleUpon" -- 400
            ,"Digg" -- 1k
            ,"ScoopIt" -- 1k
            ,"Flipboard" -- 2k
            ,"Buffer" -- 2.5k
            ,"NewsVine" -- 7k -- больше sharing
            ,"Diigo" -- 2.7k
            ,"RememberTheMilk" -- 9k
            ,"GoogleBookmarks"
            ,"Wallabag"
            ,"Wakelet"
--             ,"Baidu"  -- China Google +1    4/1
--             ,"Weibo"  -- China Twitter     18/5
--             ,"Renren" -- China Facebook  1.4k/179china
            -- Raindrop.io
            ]
        ++
        ["SACustom" :/
            [ "CustomShareAction" :. customShareAction
            ]]
        ++
        map (\ a -> ("SA" ++ a) :/ [])
            ["System"]
        -- добавляем новые варианты в конец,
        -- и не забываем пересобрать MuninPlugin и все frontend-ы
    msgButton <- regType $ Type UwD "MsgButton" "mb"
        ["MBKeepUnread" :/ []
        ,"MBStar" :/ []
        ,"MBTag" :/ []
        ,"MBShare" :/ []
        ,"MBShareAction" :/
            [ "ShareAction" :. shareAction ]
        ]
    emailContact <- regType $ Type Uw "EmailContact" "ct"
        [ "EMailContact" :/
            [ "EMail" :. bs
            , "FullName" :. bs
            , "Groups" :. List bs
              -- для отправки группе контактов, не уверен,
              -- что это надо реализовывать
            , "Avatar" :. maybe_ bs
            , "Stats" :. maybe_ (Tuple [time, int])
              -- можно просто последнего отправленного ставить в начало,
              -- но не факт, что это правильно.
              -- по-идее, статистику нужно отдельно хранить,
              -- да желательно с блеклистом (если контакт запретил отправку
              -- писем себе)
            ]]
    sharingSettings <- regType $ Type Uw "SharingSettings" "shs"
        [ "SharingSettings" :/
            [ "ShareMenuButtons" :. maybe_ (List msgButton)
            , "MsgButtons"       :. maybe_ (List msgButton)
            , "ListViewButtons"  :. maybe_ (List msgButton)
            , "CustomShareActions" :. List customShareAction
            -- нужен список всех возможных share actions
            -- (с SAFacebook appId, чтобы appId был в одном месте)
            , "EMailUsingMailto" :. bool
            , "ReplyToEMail" :. maybe_ (Tuple [bs,bs]) -- Name, E-mail
            , "Contacts" :. List emailContact
            , "Reserved1" :. int
            ]]
        -- насчет shortener-ов -- пароль/ключ к bitly должен быть в api keys
        -- хотя это попозже

    loginType <- regType $ Type UwD "LoginType" "lt"
        ["LTGoogle" :/ [ "Email" :. bs ]
        ,"LTFacebook" :/ [ "Email" :. bs ]
        ,"LTTwitter" :/ [ "Id" :. bs ] -- без redirect_by
        ,"LTOpenId" :/ [ "URL" :. bs ]
        ,"LTEmail" :/ [ "Email" :. bs ]
        ,"LTUsername" :/ [ "Username" :. bs ]
        ,"LTFeverApiKey"  :/ [ "ApiKey" :. bs ]
        ]

    login <- regType $ Type NoUw "Login" "l"
        ["Login" :/
            [ "LoginType" :. loginType
            , "UserID" :. bs
            , "Reserved1" :. int
            , "Reserved2" :. int
            , "Reserved3" :. int
            , "Reserved4" :. int
            ]]
    regRiak login "LoginType" 300 300 10

    userSettingsEx <- regType $ Type Uw "UserSettingsEx" "uste"
        ["UserSettingsEx" :/
            [ "LastWhatsNewTime" :. time
            , "PasswordHash" :. maybe_ bs
            , "Reserved1_1" :. maybe_ int
            , "Reserved1_2" :. maybe_ int
            , "Reserved1_3" :. maybe_ int
            , "Reserved1_4" :. maybe_ int
            , "Reserved1_5" :. maybe_ int
            , "Reserved1_6" :. maybe_ int
            , "Reserved1_7" :. maybe_ int
            , "AssociatedAccounts" :. List loginType
            , "AssociatedAccountNames" :. map_ loginType bs
              -- twitter id -> twitter screen name
            , "Reserved4" :. int
            , "Reserved5" :. int
            , "Reserved6" :. int
            , "Reserved7" :. int
            , "Reserved8" :. int
            , "Reserved9" :. int
            , "Reserved10" :. int
            , "Reserved11" :. int
            , "Reserved12" :. int
            , "Reserved13" :. int
            , "Reserved14" :. int
            , "Reserved15" :. int
            ]]
    userSettings <- regType $ Type Uw "UserSettings" "ust"
        ["UserSettings" :/
            [ "User" :. bs  -- key
            , "EditsCount" :. int
            , "ScrollMode" :. scrollMode
            , "ListViewMode" :. listViewMode
            , "ShowFavicons" :. bool
            , "MarkReadMode" :. markReadMode
            , "UltraCompact" :. bool
            , "Reserved" :. maybe_ bs -- было username
            , "ExactUnreadCounts" :. bool
            , "PublicFeeds" :. maybe_ (map_ publicFeedType publicFeedInfo)
            , "Country" :. maybe_ bs
            , "ApiKeys" :. maybe_ apiKeys
            , "Experiments" :. maybe_ (List userExperiment)
            , "SharingSettings_" :. maybe_ sharingSettings
            , "Ex" :. maybe_ userSettingsEx
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

    emailVerificationType <- regType $ Type NoUw "EmailVerificationType" "evt"
        ["EVTSignUp" :/
            [ "PasswordHash" :. bs
            , "FeverApiKey" :. bs
            ]
        ,"EVTChangeEmail" :/
            [ "User" :. bs
            ]
        ,"EVTResetPassword" :/
            [ "User" :. bs
            ]
        ,"EVTRestoreAccess" :/
            [ "User" :. bs
            ]
        ]
    emailVerificationToken <- regType $ Type NoUw "EmailVerificationToken" "evtk"
        ["EmailVerificationToken" :/
            [ "Token" :. bs
            , "Expire" :. time
            , "Verified" :. bool
            , "Email" :. bs
            , "VerificationType" :. emailVerificationType
            , "Reserved1" :. int
            , "Reserved2" :. int
            , "Reserved3" :. int
            , "Reserved4" :. int
            ]]
    regRiak emailVerificationToken "Token" 60 60 10

    emailVerification <- regType $ Type NoUw "EmailVerification" "ev"
        ["EmailVerification" :/
            [ "Email" :. bs
            , "Verified" :. List bs -- для каких аккаунтов
              -- списки токенов для ограничения числа отправляемых писем
            , "SignUpTokens" :. List (Tuple [time, bs])
            , "ChangeEmailTokens" :. hashMap bs (List (Tuple [time, bs]))
            , "ResetTokens" :. List (Tuple [time, bs])
            , "Reserved1" :. int
            , "Reserved2" :. int
            , "Reserved3" :. int
            , "Reserved4" :. int
            ]]
    regRiak emailVerification "Email" 60 60 10

    -- списки токенов для очистки
    -- сброс пароля/смена email удаляет остальные токены сброса/смены
    -- restore удаляет более новые restore и все токены сброса/смены
    userEmailVerificationTokens <- regType $ Type NoUw "UserEmailVerificationTokens" "uevt"
        ["UserEmailVerificationTokens" :/
            [ "User" :. bs
              -- tokenId -> (time, email, type)
            , "Tokens" :. hashMap bs (Tuple [time, bs, emailVerificationType])
            , "Reserved1" :. int
            , "Reserved2" :. int
            , "Reserved3" :. int
            , "Reserved4" :. int
            ]]
    regRiak userEmailVerificationTokens "User" 60 60 10

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
        , "SUKErrorPath" :/
             [ "Message" :. bs
             , "Path" :. List subscriptionParentUrl ]
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
            , "Guid" :. guid
            , "StreamTitle" :. bs
            , "HtmlUrl" :. bs
            ]
        , "AVideo2" :/
            [ "Url" :. url
            , "Mime" :. bs
            , "FileSize" :. maybe_ int
            , "Duration" :. maybe_ int
            , "Title" :. maybe_ bs
            , "Width" :. maybe_ int
            , "Height" :. maybe_ int
            , "Poster" :. maybe_ url
            , "Loop" :. bool -- единственное отличие от AVideo
            ]
        , "AThumbnail" :/
            [ "Url" :. url
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
            , "Text"    :. xbodyString
            , "ShortText" :. bs
            , "ShorterText" :. bs -- более короткий текст для magazine
      --       , "Feed"    :. maybe_ bs
      --       , "Next"    :. maybe_ bs
            ]]
    regRiak msg "Key" 60 60 10

    msgHeader <- regType $ Type Uw "MsgHeader" "mh"
        ["MsgHeader" :/
            [ "Guid" :. guid
            , "ContentHash" :. sbs -- чтобы не дублировать по контенту
            , "Author"  :. bs
            , "AuthorPic"  :. maybe_ url
            , "Subject" :. bs
            , "Time"    :. maybe_ time
            , "DlTime"  :. time
            , "ShortText" :. bs -- для показывания в сокращенном виде
              -- без лишних залезаний в базу
            ]]

    timeId <- regType $ Type NoUw "TimeId" "ti"
         ["TimeId" :/
            [ "Time" :. time
            , "Id"   :. int
            ]]

    msgTree <- regType $ Type NoUw "MsgTree" "mt"
         ["MsgTree" :/
            [ "Headers"  :. array_ msgHeader
              -- размер можно из массива узнать
              -- id (-1 root) -> [child]
            , "Children" :. intMap (set_ timeId)
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
            , "UpdatedComments" :. intSet -- не используется
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
    regRiak posts "BlogFeedUrl" 3 30 1000
    -- проверяем каждые 3 сек,
    -- дабы на свежих подписках что-то обновлялось
    -- 30 сек -- начальное время, затем делаются recachePosts

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
            , "AveragePostLength" :. double
            , "PaidCountries" :. hashMap bs int
              --  ^ только страны, где >10 человек, иначе нерепрезентативная
              --  выборка получается и сортируем уже по NormalizedSubscribers
            , "Countries" :. hashMap bs int
            ]]

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

    activeCheckSubscriptions <- regType $ Type NoUw "ActiveCheckSubscriptions" "acs"
        ["ActiveCheckSubscriptions" :/
            [ "Key"   :. unit
            , "Users" :. hashMap bs time
            ]]
    regRiak activeCheckSubscriptions "Key" 0 0 0

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
    regRiak urlToScan "Url" 0 0 0

    queueType <- regType $ Type NoUw "QueueType" "qt"
        [ "QTSubscription"  :/ []
        , "QTBlogFeed"      :/ []
        , "QTTemporary1"    :/ []
        , "QTNewComment1"   :/ []
        , "QTRescan1"       :/ []
        , "QTTemporary"     :/ []
        , "QTNewComment"    :/ []
        , "QTRescan"        :/ []
        , "QTSubscriptionOPML" :/ []
        ]
    scanList <- regType $ Type NoUw "ScanList" "sl"
        ["ScanList" :/
            [ "Time" :. time -- ключ, для подписки будет 0-9 секунд
            , "Urls" :. List (Tuple [url, queueType])
            ]]
    regRiak scanList "Time" 0 0 0

    let readSet = Builtin "ReadSet" "ReadSet???"
--     commentsRead <- regType $ Type NoUw "CommentsRead" "cr"
--         ["CommentsRead" :/
--             [ "Key" :. Tuple [bs, commentsKey]
--             , "Set" :. readSet
--             ]]
--     regRiak commentsRead "Key" 300 100

    oldFeedMask <- regType $ Type NoUw "OldFeedMask" "ofm"
        ["OldFeedMask" :/
            [ "Posts" :. readSet
            , "Comments" :. maybe_ (intMap readSet)
            ] ]
    feedMask <- regType $ Type NoUw "FeedMask" "fm"
        ["FMFeedMask" :/
            [ "PostsMask" :. readSet
            , "CommentsMask" :. maybe_ (intMap readSet)
              -- если не было фильрации по комментариям вид со свернутыми
              -- комментариями) то fmComments = Nothing)
              -- Тогда у нас видны все комментарии у постов из маски,
              -- по сути:
              -- > fmComments = IntMap.fromList
              -- >     [(pid, ReadSet.fromRange 0 (commentsCount pid))
              -- >     | pid <- ReadSet.toList fmPosts]
            ]
        ,"FMError" :/ []
         --  ^ все сообщения фида спрятаны.
         -- Требуется обновление после overload seconds.
        ]
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
              --  user/-/state/com.google/kept-unread
            ]]
    regRiak postsRead "Key" 600 600 400

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
            , "Guids" :. intMap guid
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
    regRiak grIds "User" 200 200 400

    userBackup <- regType $ Type NoUw "UserBackup" "ub"
        ["UserBackup" :/
            [ "Key"          :. Tuple [bs, time]
            , "User"         :. user
            , "UserStats"    :. userStats
            , "UserFilters"  :. userFilters
            , "UserSettings" :. userSettings
            , "GRIds"        :. grIds
            , "FeverIds"     :. feverIds
            , "Reserved1"    :. bool
            , "Reserved2"    :. bool
            , "Reserved3"    :. bool
            , "Reserved4"    :. bool
            ]]
    regRiak userBackup "Key" 200 200 200

    deletedUser <- regType $ Type NoUw "DeletedUser" "du"
        ["DeletedUser" :/
            [ "User"         :. bs
            , "Backups"      :. List time
            , "MailsSent"    :. maybe_ (List time)
            , "Reserved2"    :. bool
            , "Reserved3"    :. bool
            , "Reserved4"    :. bool
            ]]
    regRiak deletedUser "User" 200 200 200

    mailType <- regType $ Type NoUw "MailType" "mt"
        ["MTRenewInTwoWeeksReminder" :/
            [ "PaidTill" :. time ]
        ,"MTInactivityReasonRequest" :/ []
        ]
    mailsSent <- regType $ Type NoUw "MailsSent" "ms"
        ["MailsSent" :/
            [ "User"         :. bs
            , "MailsSent"    :. set_ mailType
            , "Reserved2"    :. int
            , "Reserved3"    :. int
            , "Reserved4"    :. int
            ]]
    regRiak mailsSent "User" 200 200 200

    filterQuery <- regType $ Type NoUw "FilterQuery" "fq"
        ["FilterQuery" :/
            [ "Query" :. bs
            , "Negate" :. bool
              -- у smart stream negate == false
            , "Feeds" :. hashMap bs bool  -- [(feed,expanded)]
            , "Reserved1" :. int
            , "Reserved2" :. int
            ]]
    -- для веба, с более компактным представлением списка фидов,
    -- а то слишком много данных передаётся
    filterQueryRpc <- regType $ Type Uw "FilterQueryRpc" "fqr"
        ["FilterQueryRpc" :/
            [ "Query" :. bs
            , "Negate" :. bool
            , "FeedGRIds" :. List int
            ]]
    searchError <- regType $ Type UwD "SearchError" "se"
        ["SESyntaxError" :/
            [ "ErrorMessage" :. bs ]
        ,"SESystemError" :/
            [ "ErrorMessage" :. bs ]
        ]
    filterUpdateTime <- regType $ Type NoUw "FilterUpdateTime" "fut"
        ["FUTNever" :/ []
        ,"FUTUpdatedAt" :/
            [ "UpdateTime" :. time ]
        ,"FUTError" :/
            [ "ErrorTime" :. time
            , "SearchError" :. searchError
              -- источник не нужен, потом будут отдельные маски для
              -- каждого фильтра, там будет понятно, где ошибка.
            ]
        ,"FUTEdited" :/
            [ "UpdateTime" :. time
            , "ChangedFeeds" :. intSet
              -- Набор измененных фидов, для которых нужно делать
              -- неинкрементальный запрос (т.е., без времени).
              -- Включают в себя удаленные фиды, чтобы чистить старые маски
            ]
        ]
    filterFeedMasks <- regType $ Type NoUw "FilterFeedMasks" "ffm"
        ["FilterFeedMasks" :/
            [ "LastUpdated" :. filterUpdateTime
            , "FeedMasks" :. intMap feedMask
            , "OldFeedMasks" :. intMap oldFeedMask
            , "Reserved2" :. int
            ]]
    oldSmartStream <- regType $ Type NoUw "OldSmartStream" "oss"
        ["OldSmartStream" :/
            [ "Name" :. bs
            , "Queries" :. List filterQuery
            , "FeedMasks" :. filterFeedMasks
            , "Reserved1" :. int
            , "Reserved2" :. int
            ]]
    filter <- regType $ Type NoUw "Filter" "filter"
        ["Filter" :/
            [ "Id" :. int
            , "Query" :. filterQuery
            , "FeedMasks" :. filterFeedMasks
              -- Маски поиска по Query. Общие маски будут получаться
              -- (пока не сделано) через объединение масок отдельных фидов
              -- (а не через объединение запросов, как сделано сейчас)
            , "Reserved1" :. int
            , "Reserved2" :. int
            ]]
    smartStream <- regType $ Type NoUw "SmartStream" "ss"
        ["SmartStream" :/
            [ "Name" :. bs
            , "Query" :. filterQuery
            , "FeedMasks" :. filterFeedMasks
              -- Маски поиска по Query с примененными фильтрами
            , "UnfilteredFeedMasks" :. filterFeedMasks
              -- Маски поиска по Query без участия фильтров
            , "Reserved1" :. int
            , "Reserved2" :. int
            ]]
    filters <- regType $ Type NoUw "Filters" "f"
        ["Filters" :/
            [ "User" :. bs
            , "Version" :. int
            , "OldFilters" :. List filterQuery
            , "FeedMasks" :. filterFeedMasks
            , "OldSmartStreams" :. List oldSmartStream
            , "OverloadDelay" :. int
            , "NewFilters" :. List filter
            , "NewSmartStreams" :. List smartStream
            , "Reserved4" :. int
            ]]
    regRiak filters "User" 600 600 400

    apiMode <- regType $ Type UwD "ApiMode" "am"
        ["AMNormal" :/
            [ "HostName" :. bs
            , "AcceptLanguage" :. bs ]
        ,"AMGRIdsOnly" :/
            [ "Fetch" :. bool
            , "Count" :. int
            , "Continuation" :. unsafeRef (maybe_ msgKey)
            , "MinDlTime" :. maybe_ time
            , "MaxDlTime" :. maybe_ time
            , "MaxTime" :. maybe_ time
            , "ExcludeTags" :. hashSet itemTag
            , "IncludeTags" :. hashSet itemTag
            , "ReadOnly" :. bool
            , "MsgLinkParams" :. List (Tuple [bs, bs])
            , "FromUI" :. bool
            , "MaxMsgTextLength" :. maybe_ int
            ]
        ,"AMDiscovery" :/
            [ "HostName" :. bs
            , "AcceptLanguage" :. bs
            , "Url" :. bs ]
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
            [ "FeedId"        :. int
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

    -- msgKey + id-шки. Можно было бы хранить guid->id в posts/comments,
    -- но это доп объем памяти (где-то 20% 60/300байт)
    msgId <- regType $ Type Uw "MsgId" "mid"
        ["MsgId" :/
            [ "FeedId" :. int
            , "PostId" :. int
            , "CommentId" :. maybe_ int
            ]]
    longMsgId <- regType $ Type Uw "LongMsgId" "lmid"
        ["LongMsgId" :/
            [ "MsgKey" :. msgKey
            , "MsgId" :. msgId
            ]]

    treeReq <- regType $ Type UwD "TreeReq" "tr"
        [ "TRPosts" :/
            [ "Reqs" :. List postsReq
            ]
          -- при Sort By Feed получится то же, только Reqs будут по-очереди
          -- обрабатываться
          -- По сути можно в postsReq закодировать, что он полностью выдает
          -- до упора (ParentId сделать -2 например и отключать проверку
          -- на время), тогда любую папку,
          -- smart stream и поиск по ним можно не менять
          -- в MsgTreeViewMode пока вроде некуда добавлять группировку,
          -- но куда-то надо запихнуть

          -- у hot links в принципе для next только список checksum
          -- все равно их не более 100-150

          -- nextReq для all items вернет новые MsgTreePoint
          -- при следующем запросе можно будет выбирать следующие
          -- 15 фидов (на стороне клиента)
          -- Для начала можно все передавать, фильтрация -- уже оптимизация
        , "TRTags" :/
            [ "Tags" :. maybe_ (List itemTag)
              -- если Nothing, то все теги
            , "MaxTag" :. maybe_ (Tuple [time, msgId])
              -- версия тегов, новые теги имеют большее время
              -- (нигде не форсируется). Нужен ли msgId?
            , "LastMsg" :. maybe_ (Tuple [time, msgId])
            ]
        , "TRComments" :/
            [ "OnExpand" :. bool
            , "Req" :. commentsReq
            ]
        , "TRCommentsS" :/
            [ "OnExpand" :. bool
            , "StreamName" :. bs
            , "Req" :. commentsReq
            ]
        , "TRSmartStream" :/
            [ "StreamName" :. bs
            , "Reqs" :. List postsReq ]
        , "TRSearchPosts" :/
            [ "Query" :. bs
            , "FeedMasksKey" :. bs
            , "Reqs" :. List postsReq ]
        , "TRSearchSmartStream" :/
            [ "StreamName" :. bs
            , "Query" :. bs
            , "FeedMasksKey" :. bs
            , "Reqs" :. List postsReq ]
        , "TRSearchTags" :/
            [ "Query" :. bs
            , "IdsKey" :. bs
            , "Tags" :. maybe_ (List itemTag)
            , "MaxTag" :. maybe_ (Tuple [time, msgId])
            , "LastMsg" :. maybe_ (Tuple [time, msgId])
            ]
        ]

    -- результат запроса дерева сообщений
    msgView <- regType $ Type UwD "MsgView" "msgView"
        [ "MVFull" :/
            [ "Msg" :. msg ]
        , "MVShort" :/
            [ "Header" :. msgHeader
            , "CachedMsg" :. maybe_ msg ]
        ]

    msgItem <- regType $ Type Uw "MsgItem" "mi"
        ["MsgItem" :/
            [ "MsgView" :. msgView
            , "MsgKey" :. msgKey
            , "MsgId" :. msgId
            -- номер в посте для запоминания прочитанности
--             , "PostId" :. int
--             , "CommentId" :. maybe_ int
            , "Read" :. bool
            , "Tags" :. List itemTag
            , "SmartStreams" :. intSet
            , "ReadLocked" :. bool
            , "Full" :. bool
            , "SearchResult" :. bool
--             , "Ignored" :. bool
            -- feed не нужен, его можно по MsgKey из MsgView найти
            ]]
            -- Фактически collapsed/expanded+next+read будут отображаться на
            -- source-ы.

    let msgForest = Type UwD "MsgForest" "mf"
         ["MsgForest" :/
            -- все счетчики показывают общий размер, а не подгруженный
            -- commentsForest всегда пробегает по всем комментариям,
            -- считая сумму (но не добавляя сами сообщения, если их более 15),
            -- чтобы были корректные числа у поддеревьев.
            --
            -- Для случаев, когда комментарии не раскрыты, postsForest
            -- считает правильную сумму по маскам прочитанности/поиска.
            --
            -- Для root forest не имеет особого смысла, т.к. там смешивается
            -- число постов и комментариев и выдается только сумма для
            -- выдаваемого куска постов.
            -- Для общего числа используются subItems
            --
            [ "TotalCount" :. int
              -- сколько всего комментариев (включая прочитанные)
              -- используется для обновления счетчиков тегов
              -- (TotalComments) при отмечании тегом поста
              -- с развернутыми комментариями
            , "UnreadCount" :. int
              -- число непрочитанных комментариев, причем в любом режиме
              -- просмотра (в поиске и smart stream тоже),
              -- используется в skip/ignore для правильного обновления
              -- числа непрочитанных в исходном фиде
            , "TotalResultsCount" :. int
              -- всего комментариев-результатов поиска
              -- в обычном режиме (не поиск/smart stream) равен TotalCount
              -- используется для отображения числа комментариев
              -- в режиме ShowAll
            , "UnreadResultsCount" :. int
              -- число непрочитанных результатов поиска,
              -- отличается от UnreadCount только
              -- при поиске или в smart stream (или поиске в smart stream)
              -- используется для отображения числа комментариев
              -- в режиме Show Unread
            , "SmartStreamUnreadCounts" :. intMap int
            , "SmartStreamUnreadResultCounts" :. intMap int
              -- число непрочитанных результатов поиска, нужно, чтобы
              -- корректировать число SmartStreamUnreadCounts
              -- при mark above/below
            , "TagTotalCounts" :. map_ (maybe_ itemTag) int
            , "TagUnreadCounts" :. map_ (maybe_ itemTag) int
            , "TagUnreadResultCounts" :. map_ (maybe_ itemTag) int
              -- не GRIds, т.к. лень на клиенте возиться с перекодированием
              -- туда и обратно (к тому же, у новых тегов может еще не быть
              -- GRId, а делать отложенное перекодирование совсем лень).
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

    externalLoginType <- regType $ Type UwD "ExternalLoginType" "elt"
        ["Google" :/ []
        ,"Facebook" :/ []
        ,"Twitter" :/ []
        ,"OpenId" :/
            [ "URL" :. bs ]
        ]
    externalLoginAction <- regType $ Type UwD "ExternalLoginAction" "ela"
        ["ELALogin" :/ []
        ,"ELAAddUrl" :/ [ "URL" :. bs ]
        ,"ELAAddAssociatedAccount" :/ []
        ]

    io "loginGetForwardUrl" [externalLoginType, bs, externalLoginAction, url] url
    io "loginCallback" [externalLoginType, bs, url, bs] (Tuple [loginType, loginAccessToken, externalLoginAction, maybe_ bs])

--     io "importFromGoogleReaderGetForwardUrl" [bs, url] url
--     io "importFromGoogleReaderCallback" [bs, bs, bs, bs] unit
--     io "importStarredAndTaggedItemsFromGoogleReaderCallback" [bs, bs, bs, bs] unit

    io "userSubscribe" [bs, bs, maybe_ bs, List bs] bs
    io "userDiscoverySubscribe" [bs, bs, bs, bs, maybe_ bs, List bs] bs
    io "userRenameSubscription" [bs,bs,bs] unit
    io "userRenameFolder" [bs,bs,bs] bs
    io "userEditSubscriptionFolders" [bs,bs,bs,bool] unit
    io "userUnsubscribe" [bs, List bs] unit
    io "userRetrySubscription" [bs, bs] unit
    io "deleteFilter" [bs, int] unit
    io "deleteSmartStream" [bs, bs] unit
    io "checkQuerySyntax" [bs] (maybe_ bs)
    io "addFilter" [bs, bs, bool, List int] unit
    io "editFilter" [bs, int, bs, bool, List int] unit
    io "addSmartStream" [bs, bs, bs, List int] unit
    io "editSmartStream" [bs, bs, bs, List int] unit
    io "userOPML" [bool, bs] bs
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
        , "SITSmartStream" :/
            [ "StreamName" :. bs
            , "StreamFeedSirs" :. List int ]
        , "SITStarred" :/ []
        , "SITAllTags" :/ []
        ]
    subItemRpc <- regType $ Type Uw "SubItemRpc" "sir"
        [ "SubItemRpc" :/
            [ "Path" :. bs
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
    welcomeState <- regType $ Type Uw "WelcomeState" "ws"
        ["WelcomeState" :/
            [ "HasPrevAccount" :. bool
            , "HasPrevSubs" :. bool
            , "StarredRestored" :. bool
            , "TaggedRestored" :. bool
            ]]

    updateFilters <- regType $ Type UwD "UpdateFilters" "uf"
        ["UFNone" :/ [] -- не обновляем фильтры
        ,"UFChanged" :/ [] -- обновляем только отредактированные
        ,"UFAll" :/ [] -- обновляем все
        ]
    let subscriptionsAndRenames = Tuple
            [maybe_ (Tuple [xbodyString, bs, List bs])
            ,bs
            ,List subItemRpc
            ,maybe_
                (Tuple
                    [Tuple
                        [List (Tuple [int, filterQueryRpc])
                        ,List (Tuple [bs, filterQueryRpc])]
                    ,bs])
            ,List (Tuple [time, bs, bs])]

    io "subscriptionsAndRenames" [bs, bool, updateFilters, time, bs, bs, bs, bs]
        subscriptionsAndRenames
    io "subscriptionsAndSettings" [bs, bool, bool, bs]
        (Tuple
         [subscriptionsAndRenames
         ,Tuple [List bs, bool, userSettings]
         ,Tuple [welcomeState, List (Tuple [time, bs, bs]), int, int]])
    io "orderNotification" [bs] (Tuple [bs, payment])
    io "orderNotificationNew" [bs, bs] unit
    io "checkOrder" [bs] (Tuple [bs, payment])
    io "getPaidTill" [bs] paidTill
--     io "activeGRImportsCount" [] int
--     io "activeGRImportNames" [] xbody
    io "getUserAccountTypeAndRenewalUrl" [bs] (Tuple [bs,bs])

    io "getFeedDetails" [bs, bs, bs] (Tuple [bs, maybe_ bs, maybe_ bs, msgTreeViewMode])

    browserType <- regType $ Type UwD "BrowserType" "" $ map (:/ [])
        [ "BTUnknown", "BTAndroid", "BTIPhone", "BTIPad", "BTIPod"
        , "BTChrome", "BTIE", "BTIEMobile", "BTSafari", "BTOpera", "BTOperaMini"
        , "BTFirefox", "BTVivaldi", "BTEdge" ]
    appType <- regType $ Type UwD "AppType" "" $ map (:/ [])
        [ "ATUnknown", "ATFeeddler", "ATMrReader", "ATReeder"
        , "ATSlowFeeds", "ATJustReader", "ATNewsPlus", "ATPress"
        , "ATVienna", "ATReadKit", "ATNewsJet", "ATAmber", "ATgzip"
        , "ATUnread", "ATFeedMe", "ATFieryFeeds", "ATLire", "ATWebSubscriber"
        , "ATReadably", "ATokhttp", "ATFluentReader", "ATRavenReader"
        , "ATFocusReader", "ATNetNewsWire" ]
    operatingSystem <- regType $ Type UwD "OperatingSystem" "" $ map (:/ [])
        [ "OSUnknown", "OSWindows", "OSMac", "OSLinux", "OSAndroid", "OSIOS"
        , "OSChromeOS" ]

    usageFlag <- regType $ Type UwD "UsageFlag" "uf" $
        [ "UFWeb" :/
          [ "BrowserType" :. browserType
          , "OperatingSystem" :. operatingSystem ]
        , "UFApp" :/
          [ "AppType" :. appType
          , "OperatingSystem" :. operatingSystem ]
        , "UFShareAction" :/ [ "ShareAction" :. shareAction ]
        ]
        ++
        map (:/ [])
        [ "UFOPML"
        , "UFAddSubscription"
        , "UFSearchSubscriptions"
        , "UFDiscoverySubscription", "UFAddDiscoverySubscription"
        , "UFUnsubscribe", "UFRetrySubscription"
        , "UFRenameSubscription", "UFRenameFolder"
        , "UFEditSubscriptionFolders"
        , "UFDragAndDrop"
        , "UFSearch", "UFSearchTags"
        , "UFSkip", "UFIgnore", "UFKeepUnread", "UFMarkAllAsRead"
        , "UFStar", "UFTag"
        , "UFReadability", "UFSetUsername"
        , "UFEnablePublicFeed", "UFDisablePublicFeed", "UFGenerateNewPublicFeed"
        , "UFDeleteAccount", "UFExportOPML" ]
        ++
        [ "UFMarkAllAsReadD" :/ [ "OlderThan" :. int ]
        , "UFMarkSearchAsReadD" :/ [ "OlderThan" :. int ]
        ]
        ++
        map (:/ [])
        [ "UFFilterApply", "UFFilterHide", "UFNewSmartStream"
        , "UFEditFilter", "UFEditSmartStream"
        , "UFDeleteFilter", "UFDeleteSmartStream" ]
        ++
        [ "UFWhatsNewClick" :/ [ "Time" :. time ]
        , "UFWhatsNewClose" :/ [ "Time" :. time ]
        , "UFThemeChange" :/ [ "ThemeName" :. bs ]
        , "UFFontChange" :/ [ "FontName" :. bs ]
        , "UFFontSizeChange" :/ [ "Size" :. int ]
        , "UFLineHeightChange" :/ [ "Pixels" :. int, "FontSize" :. int ]
        , "UFSetPassword" :/ []
        , "UFSetEmail" :/ []
        , "UFMarkReadAbove" :/ []
        , "UFMarkReadBelow" :/ []
        , "UFUnstarAbove" :/ []
        , "UFUnstarBelow" :/ []
        , "UFUntagAbove" :/ []
        , "UFUntagBelow" :/ []
        ]
    userUsageFlags <- regType $ Type NoUw "UserUsageFlags" "uuf"
        ["UserUsageFlags" :/
            [ "PaidTill"     :. paidTill
            , "Country"      :. bs
            , "UsageFlags"   :. set_ usageFlag
            , "Reserved1"    :. int
            , "Reserved2"    :. int
            , "Reserved3"    :. int
            , "Reserved4"    :. int
            ]]
    usageFlags <- regType $ Type NoUw "UsageFlags" "ufl"
        ["UsageFlags" :/
            [ "Time"      :. time
            , "Flags"     :. hashMap bs userUsageFlags
            , "Reserved1" :. int
            , "Reserved2" :. int
            , "Reserved3" :. int
            , "Reserved4" :. int
            ]]
    regRiak usageFlags "Time" 3600 3600 200

--     userSession <- regType $ Type Uw "UserSession" "uSession"
--         ["UserSession" :/
--             [ "Key" :. bs  -- key
--             , "Expire" :. time
--             , "Cleared" :. bool
--             , "User" :. bs
--             , "LastAccessTime" :. time
--             , "OS" : operatingSystem
--             , "BrowserOrApp" :. either_ browserType appType
--             , "UserAgent" :. bs
--             , "IP" :. bs
--             , "Country" :. bs
--             , "Region" :. bs
--             , "City" :. bs
--             , "Reserved1" :. int
--             , "Reserved2" :. int
--             , "Reserved3" :. int
--             , "Reserved4" :. int
--             ]]
--     regRiak userSession "Key" 3600 3600 100

    userSessions <- regType $ Type NoUw "UserSessions" "uSessions"
        ["UserSessions" :/
            [ "User" :. bs
            , "Sessions" :. hashSet bs
            , "Reserved1" :. int
            , "Reserved2" :. int
            , "Reserved3" :. int
            , "Reserved4" :. int
            ]]
    regRiak userSessions "User" 3600 3600 100

    let readCounters = List (Tuple [int, int, int, int, int])
    -- [(feedId/smartstreamId/tagId, readPosts, readComments, totalPosts, totalComments)]
    let feedTcs = List (Tuple [int, Tuple [int, int]])
    -- [(feedId, totalPosts, totalComments)]

    markReq <- regType $ Type UwD "MarkReq" "mr"
        [ "MRPosts" :/
            [ "FeedTcs" :. feedTcs ]
        , "MRTags" :/
            [ "Tags" :. maybe_ (List itemTag)
              -- если Nothing, то все теги
            , "MaxTag" :. maybe_ (Tuple [time, msgId])
            ]
        , "MRSmartStream" :/
            [ "StreamName" :. bs
            , "FeedTcs" :. feedTcs ]
        , "MRSearchPosts" :/
            [ "Query" :. bs
            , "FeedMasksKey" :. bs
            , "FeedTcs" :. feedTcs ]
        , "MRSearchTags" :/
            [ "Query" :. bs
            , "IdsKey" :. bs
            , "Tags" :. maybe_ (List itemTag)
            , "MaxTag" :. maybe_ (Tuple [time, msgId])
            ]
        , "MRSearchSmartStream" :/
            [ "StreamName" :. bs
            , "Query" :. bs
            , "FeedMasksKey" :. bs
            , "FeedTcs" :. feedTcs ]
        ]
    let rootMsgId = Tuple [bool, msgId] -- (isRoot, msgId)
        -- является ли сообщение "постом" в UI
        -- Нужно для работы mark above/below в комментариях, чтобы отличить
        -- настоящий комментарий от помеченного звездочкой и отображаемого
        -- как пост.
    markReadDirection <- regType $ Type UwD "MarkReadDirection" "mrd"
        [ "MRDAll" :/ []
        , "MRDAbove" :/
            [ "Point" :. Tuple [time, rootMsgId, rootMsgId] ]
            -- (post time, click message, start message)
            -- start message используется при помечании комментариев
        , "MRDBelow" :/
            [ "Point" :. Tuple [time, rootMsgId, rootMsgId] ]
        ]

    bgAction <- regType $ Type UwD "BgAction" "ba"
        ["BGMarkMsgRead" :/
            [ "MsgId" :. msgId
            , "Read"  :. bool
            , "TotalComments" :. int ]
        ,"BGAddTag" :/
            [ "LongMsgId" :. longMsgId
            , "Tag"   :. itemTag ]
        ,"BGRemoveTag" :/
            [ "LongMsgId" :. longMsgId
            , "Tag"   :. itemTag ]
        ,"BGSkipComments" :/
            [ "MsgId" :. msgId
            , "TotalComments" :. int ]
        ,"BGIgnorePost" :/
            [ "MsgId" :. msgId
            , "TotalComments" :. int ]
        ,"BGMarkRead" :/
            [ "Direction" :. markReadDirection
            , "OlderThan" :. int -- days
            , "ViewMode" :. msgTreeViewMode
            , "Posts" :. List (Tuple [time, msgId])
              -- список постов, загруженных и отмеченных в UI.
              -- нужен для отмечания прочитанными сообщений, у которых
              -- убрали перед этим звездочку/тег
              -- для Mark all as read в тегах тоже передаём
              -- в тегах тут могут быть и комментарии (с depth=0 как у постов
              -- в обычных фидах). как-то надо по-другому назвать
            , "MarkReq" :. markReq
            ]
        ,"BGRemoveTagFromTree" :/
            [ "Above" :. bool
            , "Tags" :. maybe_ (List itemTag) -- Nothing для всех ITTag
            , "ViewMode" :. msgTreeViewMode
            , "TreeReqs" :. List treeReq
            ]
            -- более красиво было бы обобщить код markTags из BGMarkRead
            -- с другой стороны, подход с пробеганием по TreeReqs более
            -- общий и достаточно быстрый, т.к. не надо забегать в комментарии
            -- (включая комментарии прочитанных сообщений, которые не видно).
        ,"BGRemoveTagD" :/
            [ "Tags" :. maybe_ (List itemTag) -- Nothing для всех ITTag
            , "OlderThan" :. int -- days
            ]
        ,"BGSetOnlyUpdatedSubscriptions" :/
            [ "Value" :. bool ]
        ,"BGSetFolderViewMode" :/
            [ "Folder" :. bs
            , "ViewMode" :. msgTreeViewMode ]
        ,"BGSetSubscriptionViewMode" :/
            [ "Url" :. bs
            , "ViewMode" :. msgTreeViewMode ]
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
        ,"BGWhatsNewClick" :/
            [ "Time" :. time ]
        ,"BGWhatsNewClose" :/
            [ "Time" :. time ]
        ]

    io "performBgActions" [bs, List bgAction] bs
    --  ^ не benign, конечно, но попробуй докажи

--     io "markBlogRead" [bs, bs, int, int] (maybe_ subInfo)
--     io "markMsgsRead" [bs, List (Tuple [msgId, bool])] unit

    io "tagsForest" [apiMode, bs, maybe_ (List itemTag), msgTreeViewMode] (Tuple [markReq, readCounters, msgForest])

    feedsOrDiscovery <- regType $ Type UwD "FeedsOrDiscovery" "fod"
        [ "FODFeeds" :/
            [ "ReadCounters" :. readCounters ]
        , "FODFeedsApi" :/
            [ "APIMode" :. apiMode
            , "Feeds" :. List bs ]
        , "FODDiscovery" :/
            [ "Url" :. bs ]
        ]

    io "folderForest" [bs, maybe_ bs, feedsOrDiscovery, List postsReq, msgTreeViewMode, bs, bs] (Tuple [markReq, readCounters, msgForest])
    io "getTree" [apiMode, bs, msgTreeViewMode, List treeReq] (List (maybe_ msgForest))

    filterResults <- regType $ Type Uw "FilterResults" "fr"
        ["FilterResults" :/
            [ "TotalPosts" :. int
            , "TotalComments" :. int
            , "UnreadPosts" :. int
            , "UnreadComments" :. int
            , "Took"       :. int
            , "TookReal"   :. int
            , "MsgForest"  :. msgForest
            ]]
    io "filterForest" [bs, maybe_ bs, bs, maybe_ bs, feedsOrDiscovery, msgTreeViewMode, bs, bs] (either_ bs $ Tuple [markReq, readCounters, filterResults])
    io "filterTagsForest" [bs, bs, maybe_ (List itemTag), msgTreeViewMode, bs, bs] (either_ bs $ Tuple [markReq, readCounters, filterResults])
    io "smartStreamForest" [apiMode, bs, bs, readCounters, msgTreeViewMode] (Tuple [markReq, readCounters, msgForest])
    io "markReqReadCounters" [bs, msgTreeViewMode, markReq, List msgId] readCounters
    benign "pageFromFile" [bs] page
    benign "addWebpackScripts" [bs] bs
    benign "webpackStyles" [] xhead
    func' "blessId" "id" [bs] urId
    func "parseQueryStringUtf8Only" [bs] (List (Tuple [bs,bs]))

    emailAddress <- regType $ Type Uw "EmailAddress" "ea"
        ["EmailAddress" :/
            [ "Email"     :. bs
            , "FirstName" :. bs
            , "LastName"  :. bs
            ]
        ]

    io   "userEmail" [bs] (maybe_ emailAddress)
    io   "buyPage" [bs, bs, maybe_ emailAddress] page
    func "invoiceLink" [bs] bs
    io   "prettyUID" [bs] bs
    func' "xbodyStringToString" "id" [xbodyString] bs
    func' "xbodyStringToXbody" "id" [xbodyString] xbody
--     func' "textToXbody" "id" [bs] xbody
    func  "escapeXbody" [xbodyBS] xbody
    func' "hyphenatePage" "hyphenateHtml" [page] page
    func' "hyphenateXbody" "hyphenateHtml" [xbody] xbody
    func' "toLowerCase" "T.toLower" [bs] bs
    io "addTwitterScreenName" [bs, bs, bs] unit
    io "newSessionJunk" [loginType, loginAccessToken, List (Tuple [junkText,junkText])] session
    io "getUserByLogin" [loginType, maybe_ bs] (maybe_ bs)
    io "clearSession" [bs, bs] unit
    io "userEvent" [bs,bs,bs] unit
    io "runTasks" [] unit
    io "runApiServer" [] unit
    io "reloadBrowserPage" [] unit
    io "logOutAllSessions" [bs, List bs] unit

    fullText <- regType $ Type NoUw "FullText" "ft"
        ["FTError" :/
            [ "Message" :. bs ]
        ,"FTTextV0" :/
            [ "Text" :. bs ]
        ,"FTTitleAndText" :/
            [ "Title" :. bs
            , "Text" :. bs ]
        ]

    fullTextCache <- regType $ Type NoUw "FullTextCache" "ftc"
        ["FullTextCache" :/
            [ "Url"        :. url
            , "Text"       :. fullText
            , "Time"       :. time
            , "Reserved1"  :. bool
            , "Reserved2"  :. bool
            ]]
    regRiak fullTextCache "Url" 60 60 200

    io "getFullText" [bs, bool, bs, bs, msgKey] (either_ bs xbodyString)
    io "getUrTime" [] time
    io "setUsername" [maybe_ bs,bs,bs] bool
    io "setPassword" [maybe_ bs,bs,bs] unit
    io "tryRemoveAssociatedAccount" [maybe_ bs, bs, loginType] bool
    io "tryAddAssociatedAccount" [maybe_ bs, bs, loginType] bool
    io "tryGetFeverUser" [bs] (maybe_ bs)

    io "enablePublicFeed" [publicFeedType, bs] publicFeedInfo
    io "disablePublicFeed" [publicFeedType, bs] publicFeedInfo
    io "generateNewPublicFeed" [publicFeedType, bs] publicFeedInfo

    io "discover" [bs,bs,bs,bs] (maybe_ (Tuple [xbody, List (Tuple [bs, msgTreeViewMode])]))
    io "restoreSubscriptionsFromBackup" [bs] unit
    io "isUserExists" [bs] bool
    io "deleteAccount" [bool, bs] unit
    io "recordWebUsage" [bs, maybe_ junkText] unit
    io "readMsgAndApplyFixes" [bs, bs, msgKey] (maybe_ msg)
    io "parseRenewalUserId" [bs] bs
    io "passwordResetEmail" [bs] (either_ bs (Tuple [bs, bs]))
    io "sendSignUpEmail" [bs, bs, bs] bool
    io "sendPasswordResetEmail" [bs, bs, bs] bool
    io "sendChangeEmailEmail" [bs, bs, bs] bool
    io "verifySignUpToken" [bs] (maybe_ bs)
    io "verifyPasswordResetToken" [bs, bs, bs] (maybe_ bs)
    io "verifyChangeEmailToken" [bs, bs] (maybe_ bs)
    io "verifyRestoreAccessToken" [bs, bs, bs] (maybe_ bs)
    func "validateEmail" [bs] (maybe_ bs)
    func "maskEmail" [bs] bs

    okErrorRedirect <- regType $ Type UwD "OkErrorRedirect" "oer"
        [ "OEROK" :/ []
        , "OERError" :/
            [ "Error" :. bs ]
        , "OERRedirect" :/
            [ "Url" :. bs ]
        ]
    io "userAddToPocket" [bs, bs, url, bs, bs] okErrorRedirect
    io "userAuthorizeAndAddToPocket" [bs] unit
    io "logT" [bs] unit

    pageIconSize <- regType $ Type NoUw "PageIconSize" "pis"
        [ "PISAny" :/ []
        , "PIS" :/
            [ "Width"      :. int
            , "Height"     :. int ]
        ]
    pageInfoTitleSource <- regType $ Type NoUw "PageInfoTitleSource" "pits"
        [ "PITSTag" :/ []
        , "PITSItempropName" :/ []
        , "PITSTwitter" :/ []
        , "PITSOpenGraph" :/ []
        ]
    -- title: title tag, itemprop->name, twitter:title, og:title

    pageInfoDescriptionSource <- regType $ Type NoUw "PageInfoDescriptionSource" "pids"
        [ "PIDSItemprop" :/ []
        , "PIDSName" :/ []
        , "PIDSTwitter" :/ []
        , "PIDSOpenGraph" :/ []
        ]
    -- description: itemprop->description, name->description, og:, twitter:

    pageInfoImageSource <- regType $ Type NoUw "PageInfoImageSource" "piis"
        [ "PIISLinkRelImageSrc" :/ []
        , "PIISItemprop" :/ []
        , "PIISTwitter" :/ []
        , "PIISOpenGraph" :/ []
        , "PIISUserPic" :/ []
        ]
    -- image: link rel=image_src, itemprop->image, twitter:image:src, og:

    pageInfoIconSource <- regType $ Type NoUw "PageInfoIconSource" "piics"
        [ "PIICSIcon" :/ []
        , "PIICSShortcutIcon" :/ []
        , "PIICSShortcut" :/ []
        , "PIICSAppleTouchIcon" :/ []
        , "PIICSAppleTouchIconPrecomposed" :/ []
        , "PIICSIconMask" :/ []
        ]
    -- icon: ["icon", "shortcut icon", "shortcut", "apple-touch-icon", "apple-touch-icon-precomposed"]
    -- robots: robot, googlebot, bingbot, teoma -- nosnippet нафиг,
    -- мы не поисковый движок

    pageInfo <- regType $ Type NoUw "PageInfo" "pi"
        ["PageInfo" :/
            [ "Url"        :. url
            , "FetchTime"  :. time
            , "RedownloadOptions" :. List bs
            , "Error"      :. maybe_ (Tuple [time, bs])
            , "ContentType" :. maybe_ bs
            , "ContentLength" :. maybe_ int
            , "Title" :. List (Tuple [pageInfoTitleSource, bs])
            , "Description" :. List (Tuple [pageInfoDescriptionSource, bs])
            , "Image" :. List (Tuple [pageInfoImageSource, bs])
            , "Icon" :. List (Tuple [ pageInfoIconSource
                                    , Tuple [List pageIconSize, maybe_ bs, bs] ])
            , "RedirectUrl" :. maybe_ bs
            , "Reserved11"  :. bool
            , "Reserved12"  :. bool
            , "Reserved13"  :. bool
            , "Reserved14"  :. bool
            , "Reserved15"  :. bool
            , "Reserved16"  :. bool
            , "Reserved17"  :. bool
            , "ErrorsCount" :. int
            , "Reserved3"  :. int
            , "Reserved4"  :. int
            ]]
    regRiak pageInfo "Url" 600 600 200

    favicon <- regType $ Type NoUw "Favicon" "favicon"
        ["Favicon" :/
            [ "SourceUrl"   :. url
            , "FetchTime"   :. time
            , "RedownloadOptions" :. List bs
            , "RedirectUrl" :. maybe_ bs
            , "File" :. either_ (Tuple [time, bs]) (Tuple [sbs, sbs])
            , "ErrorsCount" :. int
            , "Reserved2"   :. int
            , "Reserved3"   :. int
            , "Reserved4"   :. int
            ]]
    regRiak favicon "SourceUrl" 600 600 200

    linkInfo <- regType $ Type Uw "LinkInfo" "li"
        ["LinkInfo" :/
            [ "Url"        :. url
--            , "FileSize" :. maybe_ int
            , "Title" :. bs
            , "Description" :. bs
            , "Image" :. maybe_ url
            , "Avatar" :. maybe_ url
            ]
        ]

    hotLink <- regType $ Type NoUw "HotLink" "hl"
        ["HotLink" :/
            [ "Checksum" :. int
            , "LinkInfo" :. linkInfo
            , "UniqMsgs" :. List longMsgId -- уникальные сообщения, по одному на фид
            , "MoreMsgs" :. List longMsgId -- дополнительные сообщения из тех же фидов
            , "DupMsgs" :. List longMsgId -- сообщения, дублирующие предыдущие
            , "Time" :. time
            ]
        ]
--     еще продумать вид дерева для hotlink
--     в принципе для next только список checksum
--     посты можно сразу все для линка, чтобы не париться
--     посещение линка делает его прочитанным,
--     а посты при прокрутке должны пропускаться, только если next нажали
--     enter на ссылке раскрывает текст
--     еще интересно про дубликаты - я их определяю, и хорошо бы их сохранять,
--     чтобы как-то показывать (они могут и не попасть в top 50 hotlink-ов)

    hotLinkState <- regType $ Type NoUw "HotLinkState" "hls"
        ["HotLinkState" :/
            [ "Read" :. bool -- прочитанный -- это посещенный или помеченный
              -- игнорирование -- это вообще прятать?
            , "FirstAppearedAt" :. time
              -- храним последний месяц-два, чтобы не вылезал?
            , "Reserved_1" :. int
            , "Reserved_2" :. int
            ]
        ]
    hotLinks <- regType $ Type NoUw "HotLinks" "hls"
        ["HotLinks" :/
            [ "User" :. bs
              -- ссылки
            , "HotLinks" :. List hotLink

              -- состояние
            , "Version" :. time
            , "LastVisit" :. time -- для пометки новых
            , "HiddenLinks" :. intSet -- то, что убрали навсегда
              -- уметь показывать новые ссылки с последнего раза
              -- нужны версии
              -- помнить, что прочитано
            , "HotLinksState" :. intMap hotLinkState

              -- настройки
            , "ExcludedFeeds" :. hashSet url
            , "Blacklist" :. List bs -- шаблоны url-ов
            , "TimeRange" :. int -- секунды
            , "TimeOffset" :. int -- секунды
            , "MaxHotLinks" :. int -- 50, 100
            , "MinLinks" :. int -- 2 ???
            , "UnreadOnly" :. bool
            , "SortByTime" :. bool -- или по числу ссылок
            , "NewLinksFirst" :. bool
            , "Reserved1" :. int
            , "Reserved2" :. int
            , "Reserved3" :. int
            , "Reserved4" :. int
            ]]
    regRiak hotLinks "User" 600 600 200

    feedbackEmail <- regType $ Type Uw "FeedbackEmail" "feedbackEmail"
        ["FeedbackEmail" :/
            [ "Address" :. emailAddress
            , "Time" :. time
            , "Subject" :. bs
            , "Text" :. bs
            , "Reserved1" :. int
            , "Reserved2" :. int
            ]
        ]
--         сгенерить такой список на сервере и Binary.encode в файл
--         интерфейс оттачивать уже на маке
    feedbackUserInfo <- regType $ Type Uw "FeedbackUserInfo" "fui"
        ["FeedbackUserInfo" :/
            [ "Id"          :. bs
            , "Who"         :. maybe_ bs -- twitter/fb link
            , "PaidTill"    :. paidTill
            , "Country"     :. bs
            , "UsageFlags"  :. List usageFlag
            , "LastUsedTime" :. time
            , "Deleted"     :. bool
            , "Payments"    :. List (Tuple [time, bs, bs, emailAddress])
              -- time, id, type, email
            , "FeedsCount"  :. int
            , "ErrorFeedsCount" :. int

            , "ProcessedAt" :. maybe_ time -- время skip/mail
            , "MailSent"    :. maybe_ feedbackEmail
            , "RepliedAt" :. maybe_ time -- а нужно ли?
            , "Tags"      :. List bs
            , "Notes"     :. bs
            , "Reserved1" :. int
            , "Reserved2" :. int
            , "Reserved3" :. int
            , "Reserved4" :. int
            ]]
    feedbackUserInfosList <- regType $ Type NoUw "FeedbackUserInfosList" "fuil"
        ["FeedbackUserInfoList" :/
            [ "Id"        :. bs
            , "Processed" :. List feedbackUserInfo
            , "OrderEmailsCache" :. hashMap bs emailAddress
            , "Reserved2" :. int
            , "Reserved3" :. int
            , "Reserved4" :. int
            ]]
    regRiak feedbackUserInfosList "Id" 60 60 10
    io "findUsersLeft" [int] (Tuple [List feedbackUserInfo, List feedbackUserInfo])
    io "updateFeedbackUserInfo" [feedbackUserInfo] unit

    ftsReceiptTaxSystem <- regType $ Type NoUw "FtsReceiptTaxSystem" ""
        ["FRTSObschaya" :/ []
        ,"FRTSUsnDohod" :/ []
        ,"FRTSUsnDohodMinusRashod" :/ []
        ,"FRTSEnvd" :/ []
        ,"FRTSEshn" :/ [] --
        ,"FRTSPatent" :/ []]

    ftsReceiptOperationType <- regType $ Type NoUw "FtsReceiptOperationType" ""
        ["FROTPrihod" :/ []
        ,"FROTVozvratPrihoda" :/ []
        ,"FROTRashod" :/ []
        ,"FROTVozvratRashoda" :/ []
        ]
    ftsReceiptVatType <- regType $ Type NoUw "FtsReceiptVatType" ""
        ["FRVT18" :/ []
        ,"FRVT10" :/ []
        ,"FRVT118" :/ []
        ,"FRVT110" :/ []
        ,"FRVT0" :/ []
        ,"FRVTNone" :/ []
        ]
    ftsReceiptItem <- regType $ Type NoUw "FtsReceiptItem" "fri"
        ["FtsReceiptItem" :/
            [ "Title" :. bs
            , "Count" :. int
            , "Price" :. money
            , "Total" :. money
            , "Vat"   :. ftsReceiptVatType
            , "Reserved1" :. int
            , "Reserved2" :. int
            , "Reserved3" :. int
            , "Reserved4" :. int
            ]]
            -- интересно, что суммы налога нет, только ставка
            -- наименование товаров, работ, услуг (если объем и список услуг возможно определить в момент оплаты), платежа, выплаты, их количество, цена (в валюте Российской Федерации) за единицу с учетом скидок и наценок, стоимость с учетом скидок и наценок, с указанием ставки налога на добавленную стоимость (за исключением случаев осуществления расчетов пользователями, не являющимися налогоплательщиками налога на добавленную стоимость или освобожденными от исполнения обязанностей налогоплательщика налога на добавленную стоимость, а также осуществления расчетов за товары, работы, услуги, не подлежащие налогообложению (освобождаемые от налогообложения) налогом на добавленную стоимость);

    -- обязательные поля чека для ФНС
    -- https://normativ.kontur.ru/document?moduleId=1&documentId=316169#h369
    ftsReceipt <- regType $ Type NoUw "FtsReceipt" "fr"
        ["FtsReceipt" :/
            [ "DocumentName" :. bs -- "Кассовый чек"
              -- наименование документа;
            , "NomerZaSmenu" :. int
              -- порядковый номер за смену;
            , "Time"         :. time
            , "Address"      :. bs
              -- дата, время и место (адрес) осуществления расчета (при расчете в зданиях и помещениях - адрес здания и помещения с почтовым индексом, при расчете в транспортных средствах - наименование и номер транспортного средства, адрес организации либо адрес регистрации индивидуального предпринимателя, при расчете в сети "Интернет" - адрес сайта пользователя);
            , "OrganizationName" :. bs
              -- наименование организации-пользователя или фамилия, имя, отчество (при наличии) индивидуального предпринимателя - пользователя;
            , "INN" :. bs
              -- идентификационный номер налогоплательщика пользователя;
            , "TaxSystem" :. ftsReceiptTaxSystem
              -- применяемая при расчете система налогообложения;
            , "OperationType" :. ftsReceiptOperationType
              -- признак расчета (получение средств от покупателя (клиента) - приход, возврат покупателю (клиенту) средств, полученных от него, - возврат прихода, выдача средств покупателю (клиенту) - расход, получение средств от покупателя (клиента), выданных ему, - возврат расхода);
            , "Items" :. List ftsReceiptItem
            , "Total" :. money
            , "TotalVats" :. List (Tuple [ftsReceiptVatType, money])
              -- сумма расчета с отдельным указанием ставок и сумм налога на добавленную стоимость по этим ставкам (за исключением случаев осуществления расчетов пользователями, не являющимися налогоплательщиками налога на добавленную стоимость или освобожденными от исполнения обязанностей налогоплательщика налога на добавленную стоимость, а также осуществления расчетов за товары, работы, услуги, не подлежащие налогообложению (освобождаемые от налогообложения) налогом на добавленную стоимость);
              -- у нас всегда Электронный платеж и безнал
              -- форма расчета (оплата наличными деньгами и (или) в безналичном порядке), а также сумма оплаты наличными деньгами и (или) в безналичном порядке; (в ред. Федерального закона от 03.07.2018 N 192-ФЗ)
              -- должность и фамилия лица, осуществившего расчет с покупателем (клиентом), оформившего кассовый чек или бланк строгой отчетности и выдавшего (передавшего) его покупателю (клиенту) (за исключением расчетов, осуществленных с использованием автоматических устройств для расчетов, применяемых в том числе при осуществлении расчетов в безналичном порядке в сети "Интернет")
              -- Т.е., нам это не нужно
            , "KKTNumber" :. bs
              -- заводской номер контрольно-кассовой техники
              -- этого поля нет в 54-ФЗ, но на всякий случай запомним
            , "KKTRegNumber" :. bs
              -- регистрационный номер контрольно-кассовой техники;
            , "FNNumber" :. bs
              -- заводской номер экземпляра модели фискального накопителя;
            , "FPD" :. bs
              -- фискальный признак документа;
            , "ReceiptSite" :. bs -- www.nalog.ru
              -- адрес сайта уполномоченного органа в сети "Интернет", на котором может быть осуществлена проверка факта записи этого расчета и подлинности фискального признака;
            , "BuyerEmail" :. bs
              -- абонентский номер либо адрес электронной почты покупателя (клиента) в случае передачи ему кассового чека или бланка строгой отчетности в электронной форме или идентифицирующих такие кассовый чек или бланк строгой отчетности признаков и информации об адресе информационного ресурса в сети "Интернет", на котором такой документ может быть получен;
            , "SenderEmail" :. bs
              -- адрес электронной почты отправителя кассового чека или бланка строгой отчетности в электронной форме в случае передачи покупателю (клиенту) кассового чека или бланка строгой отчетности в электронной форме;
            , "FDNumber" :. int
              -- порядковый номер фискального документа;
            , "ShiftNumber" :. int
              -- номер смены;
            -- , "" то же, что и ФПД?
              -- фискальный признак сообщения (для кассового чека или бланка строгой отчетности, хранимых в фискальном накопителе или передаваемых оператору фискальных данных).
              -- QR-код. (в ред. Федерального закона от 03.07.2018 N 192-ФЗ)
              -- мы генерируем его на лету
            , "ExchangeRate" :. Tuple [time, money]
              -- сохраняем дату и курс ЦБ, используемый при создании чека
              -- Возможно, что мы будем создавать чек в 23:59:59 по вчерашнему
              -- курсу, а создастся чек уже на следующий день,
              -- Для даты курса надо использовать время продажи на FastSpring
            , "RetailAddress" :. bs
              -- адрес физического магазине, не требуется,
              -- но все же присутствует в JSON от e-ofd
              -- (равно юр. адресу самого e-ofd)
            , "Reserved_2" :. int
            , "Reserved_3" :. int
            , "Reserved_4" :. int
            , "Reserved_5" :. int
            , "Reserved_6" :. int
            , "Reserved_7" :. int
            , "Reserved_8" :. int
            , "Reserved_9" :. int
            , "Reserved_10" :. int
            ]
        ]

    printableFtsReceipt <- regType $ Type NoUw "PrintableFtsReceipt" "pfr"
        ["PrintableFtsReceipt" :/
            [ "PdfRus"     :. sbs
            , "HtmlRus"    :. bs
            , "HtmlEng"    :. bs
            , "Reserved_1" :. int
            , "Reserved_2" :. int
            , "Reserved_3" :. int
            , "Reserved_4" :. int
            ]]

    ofdReceipt <- regType $ Type NoUw "OfdReceipt" "or"
        ["OfdReceipt" :/
            [ "OrderIdRefund" :. Tuple [bs, bool]
            , "TransactionID" :. bs
            , "FtsReceipt"    :. ftsReceipt
            , "PrintableFtsReceipt" :. maybe_ printableFtsReceipt
            , "Reserved_1" :. int
            , "Reserved_2" :. int
            , "Reserved_3" :. int
            , "Reserved_4" :. int
            ]]
    regRiak ofdReceipt "OrderIdRefund" 60 60 10

    parserEnvironment <- regType $ Type NoUw "ParserEnvironment" "pe"
        ["ParserEnvironment" :/
            [ "Key" :. bs
            , "Value" :. maybe_ bs ]]
    regRiak parserEnvironment "Key" 3600 3600 100

    return ()
