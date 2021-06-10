{-# LANGUAGE BangPatterns, DeriveDataTypeable, FlexibleInstances, MultiParamTypeClasses, TemplateHaskell #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
module Network.Riak.Protocol.BucketProps.ReplMode (ReplMode(..)) where
import Prelude ((+), (/), (.))
import qualified Prelude as Prelude'
import qualified Data.Typeable as Prelude'
import qualified Data.Data as Prelude'
import qualified Text.ProtocolBuffers.Header as P'

data ReplMode = FALSE
              | REALTIME
              | FULLSYNC
              | TRUE
              deriving (Prelude'.Read, Prelude'.Show, Prelude'.Eq, Prelude'.Ord, Prelude'.Typeable, Prelude'.Data)

instance P'.Mergeable ReplMode

instance Prelude'.Bounded ReplMode where
  minBound = FALSE
  maxBound = TRUE

instance P'.Default ReplMode where
  defaultValue = FALSE

toMaybe'Enum :: Prelude'.Int -> P'.Maybe ReplMode
toMaybe'Enum 0 = Prelude'.Just FALSE
toMaybe'Enum 1 = Prelude'.Just REALTIME
toMaybe'Enum 2 = Prelude'.Just FULLSYNC
toMaybe'Enum 3 = Prelude'.Just TRUE
toMaybe'Enum _ = Prelude'.Nothing

instance Prelude'.Enum ReplMode where
  fromEnum FALSE = 0
  fromEnum REALTIME = 1
  fromEnum FULLSYNC = 2
  fromEnum TRUE = 3
  toEnum
   = P'.fromMaybe (Prelude'.error "hprotoc generated code: toEnum failure for type Network.Riak.Protocol.BucketProps.ReplMode") .
      toMaybe'Enum
  succ FALSE = REALTIME
  succ REALTIME = FULLSYNC
  succ FULLSYNC = TRUE
  succ _ = Prelude'.error "hprotoc generated code: succ failure for type Network.Riak.Protocol.BucketProps.ReplMode"
  pred REALTIME = FALSE
  pred FULLSYNC = REALTIME
  pred TRUE = FULLSYNC
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
  reflectEnum = [(0, "FALSE", FALSE), (1, "REALTIME", REALTIME), (2, "FULLSYNC", FULLSYNC), (3, "TRUE", TRUE)]
  reflectEnumInfo _
   = P'.EnumInfo (P'.makePNF (P'.pack ".Protocol.BucketProps.ReplMode") ["Network", "Riak"] ["Protocol", "BucketProps"] "ReplMode")
      ["Network", "Riak", "Protocol", "BucketProps", "ReplMode.hs"]
      [(0, "FALSE"), (1, "REALTIME"), (2, "FULLSYNC"), (3, "TRUE")]
      Prelude'.False

instance P'.TextType ReplMode where
  tellT = P'.tellShow
  getT = P'.getRead
