{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# LANGUAGE BangPatterns, DeriveDataTypeable, FlexibleInstances, MultiParamTypeClasses #-}
module Network.Riak.Protocol.BucketProps.ReplMode (ReplMode(..)) where
import Prelude ((+), (/), (.))
import qualified Prelude as Prelude'
import qualified Data.Typeable as Prelude'
import qualified Data.Data as Prelude'
import qualified Text.ProtocolBuffers.Header as P'
 
data ReplMode = False
              | Realtime
              | Fullsync
              | True
              deriving (Prelude'.Read, Prelude'.Show, Prelude'.Eq, Prelude'.Ord, Prelude'.Typeable, Prelude'.Data)
 
instance P'.Mergeable ReplMode
 
instance Prelude'.Bounded ReplMode where
  minBound = False
  maxBound = True
 
instance P'.Default ReplMode where
  defaultValue = False
 
toMaybe'Enum :: Prelude'.Int -> P'.Maybe ReplMode
toMaybe'Enum 0 = Prelude'.Just False
toMaybe'Enum 1 = Prelude'.Just Realtime
toMaybe'Enum 2 = Prelude'.Just Fullsync
toMaybe'Enum 3 = Prelude'.Just True
toMaybe'Enum _ = Prelude'.Nothing
 
instance Prelude'.Enum ReplMode where
  fromEnum False = 0
  fromEnum Realtime = 1
  fromEnum Fullsync = 2
  fromEnum True = 3
  toEnum
   = P'.fromMaybe (Prelude'.error "hprotoc generated code: toEnum failure for type Network.Riak.Protocol.BucketProps.ReplMode") .
      toMaybe'Enum
  succ False = Realtime
  succ Realtime = Fullsync
  succ Fullsync = True
  succ _ = Prelude'.error "hprotoc generated code: succ failure for type Network.Riak.Protocol.BucketProps.ReplMode"
  pred Realtime = False
  pred Fullsync = Realtime
  pred True = Fullsync
  pred _ = Prelude'.error "hprotoc generated code: pred failure for type Network.Riak.Protocol.BucketProps.ReplMode"
 
instance P'.Wire ReplMode where
  wireSize ft' enum = P'.wireSize ft' (Prelude'.fromEnum enum)
  wirePut ft' enum = P'.wirePut ft' (Prelude'.fromEnum enum)
  wireGet 14 = P'.wireGetEnum toMaybe'Enum
  wireGet ft' = P'.wireGetErr ft'
  wireGetPacked 14 = P'.wireGetPackedEnum toMaybe'Enum
  wireGetPacked ft' = P'.wireGetErr ft'
 
instance P'.GPB ReplMode
 
instance P'.MessageAPI msg' (msg' -> ReplMode) ReplMode where
  getVal m' f' = f' m'
 
instance P'.ReflectEnum ReplMode where
  reflectEnum = [(0, "False", False), (1, "Realtime", Realtime), (2, "Fullsync", Fullsync), (3, "True", True)]
  reflectEnumInfo _
   = P'.EnumInfo (P'.makePNF (P'.pack ".Protocol.BucketProps.ReplMode") ["Network", "Riak"] ["Protocol", "BucketProps"] "ReplMode")
      ["Network", "Riak", "Protocol", "BucketProps", "ReplMode.hs"]
      [(0, "False"), (1, "Realtime"), (2, "Fullsync"), (3, "True")]