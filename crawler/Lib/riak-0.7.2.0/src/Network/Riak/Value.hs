{-# LANGUAGE GeneralizedNewtypeDeriving, OverloadedStrings, RecordWildCards,
    StandaloneDeriving #-}

-- |
-- Module:      Network.Riak.Value
-- Copyright:   (c) 2011 MailRank, Inc.
-- License:     Apache
-- Maintainer:  Mark Hibberd <mark@hibberd.id.au>, Nathan Hunter <nhunter@janrain.com>
-- Stability:   experimental
-- Portability: portable
--
-- This module allows storage and retrieval of data using the
-- 'IsContent' typeclass.  This provides access to more of Riak's
-- storage features than JSON, e.g. links.
--
-- The functions in this module do not perform any conflict resolution.

module Network.Riak.Value
    (
      IsContent(..)
    , fromContent
    , get
    , getMany
    , put
    , put_
    , putMany
    , putMany_
    ) where

import Control.Applicative
import Data.Aeson.Types (Parser, Result(..), parse)
import Data.Foldable (toList)
import Network.Riak.Connection.Internal
import Network.Riak.Protocol.Content (Content(..))
import Network.Riak.Protocol.GetResponse (GetResponse(..))
import Network.Riak.Protocol.PutResponse (PutResponse(..))
import Network.Riak.Resolvable (ResolvableMonoid(..))
import Network.Riak.Response (unescapeLinks)
import Network.Riak.Types.Internal hiding (MessageTag(..))
import qualified Data.Aeson.Parser as Aeson
import qualified Data.Aeson.Types as Aeson
import qualified Data.Attoparsec.Lazy as A
import qualified Data.ByteString.Lazy as L
import qualified Data.Sequence as Seq
import qualified Network.Riak.Content as C
import qualified Network.Riak.Request as Req

fromContent :: IsContent c => Content -> Maybe c
fromContent c = case parse parseContent c of
                  Success a -> Just a
                  Error _   -> Nothing

class IsContent c where
    parseContent :: Content -> Parser c
    toContent :: c -> Content

instance IsContent Content where
    parseContent = return
    {-# INLINE parseContent #-}

    toContent v = v
    {-# INLINE toContent #-}

instance IsContent () where
    parseContent c | c == C.empty = pure ()
                   | otherwise    = empty
    {-# INLINE parseContent #-}

    toContent _ = C.empty
    {-# INLINE toContent #-}

instance IsContent Aeson.Value where
    parseContent c | content_type c == Just "application/json" =
                      case A.parse Aeson.json (value c) of
                        A.Done _ a     -> return a
                        A.Fail _ _ err -> fail err
                   | otherwise = fail "non-JSON document"
    toContent = C.json
    {-# INLINE toContent #-}

deriving instance (IsContent a) => IsContent (ResolvableMonoid a)

-- | Store a single value.  This may return multiple conflicting
-- siblings.  Choosing among them, and storing a new value, is your
-- responsibility.
--
-- You should /only/ supply 'Nothing' as a 'T.VClock' if you are sure
-- that the given bucket+key combination does not already exist.  If
-- you omit a 'T.VClock' but the bucket+key /does/ exist, your value
-- will not be stored.
put :: (IsContent c) => Connection -> Bucket -> Key -> Maybe VClock -> c
    -> W -> DW -> IO ([c], VClock)
put conn bucket key mvclock val w dw =
  putResp =<< exchange conn
              (Req.put bucket key mvclock (toContent val) w dw True)

-- | Store many values.  This may return multiple conflicting siblings
-- for each value stored.  Choosing among them, and storing a new
-- value in each case, is your responsibility.
--
-- You should /only/ supply 'Nothing' as a 'T.VClock' if you are sure
-- that the given bucket+key combination does not already exist.  If
-- you omit a 'T.VClock' but the bucket+key /does/ exist, your value
-- will not be stored.
putMany :: (IsContent c) => Connection -> Bucket -> [(Key, Maybe VClock, c)]
        -> W -> DW -> IO [([c], VClock)]
putMany conn b puts w dw =
  mapM putResp =<< pipeline conn (map (\(k,v,c) -> Req.put b k v (toContent c) w dw True) puts)

putResp :: (IsContent c) => PutResponse -> IO ([c], VClock)
putResp PutResponse{..} = do
  case vclock of
    Nothing -> return ([], VClock L.empty)
    Just s  -> do
      c <- convert content
      return (c, VClock s)

-- | Store a single value, without the possibility of conflict
-- resolution.
--
-- You should /only/ supply 'Nothing' as a 'T.VClock' if you are sure
-- that the given bucket+key combination does not already exist.  If
-- you omit a 'T.VClock' but the bucket+key /does/ exist, your value
-- will not be stored, and you will not be notified.
put_ :: (IsContent c) => Connection -> Bucket -> Key -> Maybe VClock -> c
    -> W -> DW -> IO ()
put_ conn bucket key mvclock val w dw =
  exchange_ conn (Req.put bucket key mvclock (toContent val) w dw False)

-- | Store many values, without the possibility of conflict
-- resolution.
--
-- You should /only/ supply 'Nothing' as a 'T.VClock' if you are sure
-- that the given bucket+key combination does not already exist.  If
-- you omit a 'T.VClock' but the bucket+key /does/ exist, your value
-- will not be stored, and you will not be notified.
putMany_ :: (IsContent c) => Connection -> Bucket -> [(Key, Maybe VClock, c)]
         -> W -> DW -> IO ()
putMany_ conn b puts w dw =
  pipeline_ conn . map (\(k,v,c) -> Req.put b k v (toContent c) w dw False) $ puts

-- | Retrieve a value.  This may return multiple conflicting siblings.
-- Choosing among them is your responsibility.
get :: (IsContent c) => Connection -> Bucket -> Key -> R
    -> IO (Maybe ([c], VClock))
get conn bucket key r = getResp =<< exchangeMaybe conn (Req.get bucket key r)

getMany :: (IsContent c) => Connection -> Bucket -> [Key] -> R
        -> IO [Maybe ([c], VClock)]
getMany conn b ks r =
    mapM getResp =<< pipelineMaybe conn (map (\k -> Req.get b k r) ks)

getResp :: (IsContent c) => Maybe GetResponse -> IO (Maybe ([c], VClock))
getResp resp =
  case resp of
    Just (GetResponse content (Just s) _) -> do
           c <- convert content
           return $ Just (c, VClock s)
    _   -> return Nothing

convert :: IsContent v => Seq.Seq Content -> IO [v]
convert = go [] [] . toList
    where go cs vs (x:xs) = case fromContent y of
                              Just v -> go cs (v:vs) xs
                              _      -> go (y:cs) vs xs
              where y = unescapeLinks x
          go [] vs _      = return (reverse vs)
          go cs _  _      = typeError "Network.Riak.Value" "convert" $
                            show (length cs) ++ " values failed conversion: " ++
                            show cs
