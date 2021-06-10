{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -Werror #-}
module Text.ProtocolBuffers.ProtoJSON where

import Data.Aeson
import Data.Aeson.Types
import qualified Data.Vector as V
import Text.ProtocolBuffers.Basic
import Text.Read (readEither)

import qualified Data.ByteString.Lazy as BL
import qualified Data.ByteString.Base16 as B16
import qualified Data.Text.Encoding as T

objectNoEmpty :: [Pair] -> Value
objectNoEmpty = object . filter (hasContent . snd)
    where
      hasContent Null = False
      hasContent (Array xs) | V.null xs = False
      hasContent _ = True

toJSONShowWithPayload :: Show a => a -> Value
toJSONShowWithPayload x = object [("payload", toJSON . show $ x)]

parseJSONReadWithPayload :: Read a => String -> Value -> Parser a
parseJSONReadWithPayload tyName = withObject tyName $ \o -> do
    t <- o .: "payload"
    case readEither t of
      Left e -> fail e
      Right res -> return res

parseJSONBool :: Value -> Parser Bool
parseJSONBool (Bool b) = return b
parseJSONBool (Number sci) = return (sci >= 1)
parseJSONBool _ = fail "Expected Bool"

toJSONByteString :: ByteString -> Value
toJSONByteString bs = object [("payload", String . T.decodeUtf8 . B16.encode . BL.toStrict $ bs)]

parseJSONByteString :: Value -> Parser ByteString
parseJSONByteString = withObject "bytes" $ \o -> do
    t <- o .: "payload"
    case B16.decode (T.encodeUtf8 t) of
      Right bs -> return (BL.fromStrict bs)
      Left e -> fail $ "Failed to parse base16: " <> e
