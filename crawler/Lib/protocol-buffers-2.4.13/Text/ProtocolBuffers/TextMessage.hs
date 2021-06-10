{-# LANGUAGE BangPatterns, FlexibleContexts, ScopedTypeVariables #-}
module Text.ProtocolBuffers.TextMessage (
-- * User API functions
-- ** Main encoding and decoding operations
        messagePutText,
        messageGetText,
-- * Internal API functions
        TextMsg(..),
        TextType(..),
        tellShow,
        tellSubMessage,
        getRead,
        getSubMessage,
    ) where

import Control.Monad.Identity (Identity)
import Control.Monad (void)
import Control.Monad.Writer (Writer, execWriter, tell, censor)
import Data.Char
import Data.Foldable (toList)
import Data.Int
import Data.List (intercalate)
import Data.Sequence (singleton)
import Data.Traversable
import Data.Word
import Text.Parsec
import Text.Printf
import Text.ProtocolBuffers.Basic
import Text.Read

import qualified Data.ByteString.Lazy.Char8 as C8
import qualified Data.ByteString.Lazy.UTF8 as U8
import qualified Text.Parsec.Token as T

type Log = Seq (Int, String)
type Output = Writer Log ()

-- | Printable and readable messages
class TextMsg a where
    textPut :: a -> Output
    textGet :: Stream s Identity Char => Parsec s () a

-- | Printable and readable field types
class TextType a where
    tellT :: String -> a -> Output
    getT :: Stream s Identity Char => String -> Parsec s () a

tells :: String -> Output
tells s = tell $ singleton (0, s)

tellShow :: Show a => String -> a -> Output
tellShow name v = tells $ name ++ ": " ++ show v

tellStr :: String -> ByteString -> Output
tellStr name s = tells $ name ++ ": \"" ++ dumpDecimal s ++ "\""

tellSubMessage :: TextMsg a => String -> a -> Output
tellSubMessage name m = do
    tells $ name ++ " {"
    indent $ textPut m
    tells "}"
    where
    indent = censor (fmap (\(!n, s) -> (n + 1, s)))

dumpDecimal :: ByteString -> String
dumpDecimal = C8.foldr escape []
    where
    escape '\n' str = "\\n" ++ str
    escape '\"' str = "\\\"" ++ str
    escape c str | isAscii c && isPrint c = c : str
    escape c str = printf "\\%03d" c ++ str

instance TextType Int32 where
    tellT = tellShow
    getT = getScalar integer

instance TextType Int64 where
    tellT = tellShow
    getT = getScalar integer

instance TextType Word32 where
    tellT = tellShow
    getT = getScalar natural

instance TextType Word64 where
    tellT = tellShow
    getT = getScalar natural

instance TextType Bool where
    tellT name True = tells $ name ++ ": true"
    tellT name False = tells $ name ++ ": false"
    getT name = do
        v <- getScalar (string "true" <|> string "false") name
        return (v == "true")

instance TextType Double where
    tellT = tellShow
    getT = getScalar float

instance TextType Float where
    tellT = tellShow
    getT name = realToFrac <$> getScalar float name

instance TextType Utf8 where
    tellT name (Utf8 s) = tellStr name s
    getT name = uFromString <$> getScalar stringLiteral name

instance TextType ByteString where
    tellT = tellStr
    getT name = U8.fromString <$> getScalar stringLiteral name

instance TextType a => TextType (Maybe a) where
    tellT _ Nothing = return ()
    tellT name (Just v) = tellT name v
    getT name = Just <$> getT name

instance TextType a => TextType (Seq a) where
    tellT name xs = void $ forM xs $ tellT name
    getT = error "should not take sequence directly"

-- | This writes message as text-format protobuf to 'String'
messagePutText :: TextMsg a => a -> String
messagePutText = intercalate "\n" . toList . fmap setIndent . execWriter . textPut
    where
    setIndent (n, s) = replicate (n * 2) ' ' ++ s

lexer :: Stream s Identity Char => T.GenTokenParser s () Identity
lexer = T.makeTokenParser T.LanguageDef {
            T.identStart     = letter <|> char '_'
          , T.identLetter    = alphaNum <|> char '_'
          , T.opStart        = oneOf ":!#$%&*+./<=>?@\\^|-~"
          , T.opLetter       = oneOf ":!#$%&*+./<=>?@\\^|-~"
          , T.caseSensitive  = True
          , T.commentStart   = "/*"
          , T.commentEnd     = "*/"
          , T.commentLine    = "//"
          , T.nestedComments = True
          , T.reservedNames  = []
          , T.reservedOpNames= []
    }

symbol :: Stream s Identity Char => String -> Parsec s () ()
symbol sym = void $ T.symbol lexer sym

colon :: Stream s Identity Char => Parsec s () ()
colon = void $ T.colon lexer

braces :: Stream s Identity Char => Parsec s () a -> Parsec s () a
braces = T.braces lexer

natural :: (Integral a, Stream s Identity Char) => Parsec s () a
natural = fromIntegral <$> T.natural lexer

integer :: (Integral a, Stream s Identity Char) => Parsec s () a
integer = fromIntegral <$> T.integer lexer

float :: Stream s Identity Char => Parsec s () Double
float = either realToFrac id <$> T.naturalOrFloat lexer

stringLiteral :: Stream s Identity Char => Parsec s () String
stringLiteral = T.stringLiteral lexer

getRead :: forall a s . (Read a, Stream s Identity Char) => String -> Parsec s () a
getRead name = try $ do
    v <- getScalar (T.identifier lexer) name
    case readMaybe v of
        Just r -> return r
        Nothing -> fail $ "can't parse " ++ v

getScalar :: Stream s Identity Char => Parsec s () a -> String -> Parsec s () a
getScalar parser name = symbol name *> colon *> parser

getSubMessage :: (Stream s Identity Char, TextMsg a) => String -> Parsec s () a
getSubMessage name = symbol name *> braces textGet

-- | This reads message as text-format protobuf from any Parsec-compatible source.
-- Input must be completely consumed.
messageGetText :: (TextMsg a, Stream s Identity Char) => s -> Either String a
messageGetText s = case parse (textGet <* eof) "<protobuf>" s of
    Left e -> Left (show e)
    Right m -> Right m
