{-# LANGUAGE DeriveDataTypeable,RankNTypes #-}
-- | This module add unknown field support to the library.  There are no user API things here,
-- except for advanced spelunking into the data structures which can and have changed with no
-- notice.  Importer beware.
module Text.ProtocolBuffers.Unknown
  ( UnknownField(..),UnknownMessage(..),UnknownFieldValue(..)
  , wireSizeUnknownField,wirePutUnknownField, wirePutUnknownFieldWithSize
  , catch'Unknown, catch'Unknown', loadUnknown, discardUnknown
  ) where

import qualified Data.ByteString.Lazy as L
import qualified Data.Foldable as F
import Data.Generics
import Data.Sequence((|>))
import Data.Typeable()
import Control.Monad.Error.Class(catchError)

import Text.ProtocolBuffers.Basic
import Text.ProtocolBuffers.WireMessage

-- err :: String -> b
-- err msg = error $ "Text.ProtocolBuffers.Unknown error\n"++msg

-- | Messages that can store unknown fields implement this interface.
-- UnknownField is a supposedly opaque type.
class UnknownMessage msg where
  getUnknownField :: msg -> UnknownField
  putUnknownField :: UnknownField -> msg -> msg

-- | This is a suposedly opaque type
newtype UnknownField = UnknownField (Seq UnknownFieldValue)
  deriving (Eq,Ord,Show,Read,Data,Typeable)

data UnknownFieldValue = UFV {-# UNPACK #-} !WireTag !ByteString
  deriving (Eq,Ord,Show,Read,Data,Typeable)

instance Mergeable UnknownField where
--  mergeEmpty = UnknownField mempty
  mergeAppend (UnknownField m1) (UnknownField m2) = UnknownField (mappend m1 m2)

instance Default UnknownField where
  defaultValue = UnknownField mempty

-- | This is used by the generated code
wireSizeUnknownField :: UnknownField -> WireSize
wireSizeUnknownField (UnknownField m) = F.foldl' aSize 0 m  where
  aSize old (UFV tag bs) = old + size'WireTag tag + L.length bs

-- | This is used by the generated code
wirePutUnknownField :: UnknownField -> Put
wirePutUnknownField (UnknownField m) = F.mapM_ aPut m where
  aPut (UFV tag bs) = putVarUInt (getWireTag tag) >> putLazyByteString bs

-- | This is used by the generated code
wirePutUnknownFieldWithSize :: UnknownField -> PutM WireSize
wirePutUnknownFieldWithSize m =
  wirePutUnknownField m >> return (wireSizeUnknownField m)

{-# INLINE catch'Unknown #-}
-- | This is used by the generated code. Here for backwards compatibility.
catch'Unknown :: (UnknownMessage a) => (WireTag -> a -> Get a) -> WireTag -> a -> Get a
catch'Unknown = catch'Unknown' loadUnknown

{-# INLINE catch'Unknown' #-}
catch'Unknown' :: (WireTag -> a -> Get a) -> (WireTag -> a -> Get a) -> WireTag -> a -> Get a
catch'Unknown' handleUnknown update'Self wire'Tag old'Self =
    catchError (update'Self wire'Tag old'Self) (\_ -> handleUnknown wire'Tag old'Self)

{-# INLINE loadUnknown #-}
-- | This is used by the generated code
loadUnknown :: (UnknownMessage a) => WireTag -> a -> Get a
loadUnknown tag msg = do
  let (fieldId,wireType) = splitWireTag tag
      (UnknownField uf) = getUnknownField msg
  bs <- wireGetFromWire fieldId wireType
  let v' = seq bs $ UFV tag bs
      uf' = seq v' $ uf |> v'
  seq uf' $ return $ putUnknownField (UnknownField uf') msg

{-# INLINE discardUnknown #-}
-- | This is used by the generated code
discardUnknown :: WireTag -> a -> Get a
discardUnknown tag msg = do
  let (fieldId,wireType) = splitWireTag tag
  _bs <- wireGetFromWire fieldId wireType
  return msg
