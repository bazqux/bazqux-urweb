{-# LANGUAGE OverloadedStrings #-}

-- |
-- Module:      Network.Riak.Content
-- Copyright:   (c) 2011 MailRank, Inc.
-- License:     Apache
-- Maintainer:  Mark Hibberd <mark@hibberd.id.au>, Nathan Hunter <nhunter@janrain.com>
-- Stability:   experimental
-- Portability: portable
--
-- Low-level content and link types and functions.

module Network.Riak.Content
    (
    -- * Types
      Content(..)
    , Link.Link(..)
    -- * Functions
    , empty
    , binary
    , json
    , link
    ) where

import Data.Aeson (encode)
import Data.Aeson.Types (ToJSON)
import Network.Riak.Protocol.Content (Content(..))
import Network.Riak.Types.Internal (Bucket, Key, Tag)
import qualified Data.ByteString.Lazy.Char8 as L
import qualified Data.Sequence as Seq
import qualified Network.Riak.Protocol.Link as Link

-- | Create a link.
link :: Bucket -> Key -> Tag -> Link.Link
link bucket key tag = Link.Link (Just bucket) (Just key) (Just tag)
{-# INLINE link #-}

-- | An empty piece of content.
empty :: Content
empty = Content { value = L.empty
                , content_type = Nothing
                , charset = Nothing
                , content_encoding = Nothing
                , vtag = Nothing
                , links = Seq.empty
                , last_mod = Nothing
                , last_mod_usecs = Nothing
                , usermeta = Seq.empty
                , indexes  = Seq.empty
                , deleted = Nothing
                }

-- | Content encoded as @application/octet-stream@.
binary :: L.ByteString -> Content
binary bs = empty { value = bs
                  , content_type = Just "application/octet-stream"
                  }

-- | Content encoded as @application/json@.
json :: ToJSON a => a -> Content
json j = empty { value = encode j
               , content_type = Just "application/json"
               }
