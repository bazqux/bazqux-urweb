{-# LANGUAGE BangPatterns, DeriveDataTypeable, FlexibleInstances, MultiParamTypeClasses, TemplateHaskell #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
module Network.Riak.Protocol.DtFetchResponse.DataType (DataType(..)) where
import Prelude ((+), (/), (.))
import qualified Prelude as Prelude'
import qualified Data.Typeable as Prelude'
import qualified Data.Data as Prelude'
import qualified Text.ProtocolBuffers.Header as P'

data DataType = COUNTER
              | SET
              | MAP
              deriving (Prelude'.Read, Prelude'.Show, Prelude'.Eq, Prelude'.Ord, Prelude'.Typeable, Prelude'.Data)

instance P'.Mergeable DataType

instance Prelude'.Bounded DataType where
  minBound = COUNTER
  maxBound = MAP

instance P'.Default DataType where
  defaultValue = COUNTER

toMaybe'Enum :: Prelude'.Int -> P'.Maybe DataType
toMaybe'Enum 1 = Prelude'.Just COUNTER
toMaybe'Enum 2 = Prelude'.Just SET
toMaybe'Enum 3 = Prelude'.Just MAP
toMaybe'Enum _ = Prelude'.Nothing

instance Prelude'.Enum DataType where
  fromEnum COUNTER = 1
  fromEnum SET = 2
  fromEnum MAP = 3
  toEnum
   = P'.fromMaybe (Prelude'.error "hprotoc generated code: toEnum failure for type Network.Riak.Protocol.DtFetchResponse.DataType")
      . toMaybe'Enum
  succ COUNTER = SET
  succ SET = MAP
  succ _ = Prelude'.error "hprotoc generated code: succ failure for type Network.Riak.Protocol.DtFetchResponse.DataType"
  pred SET = COUNTER
  pred MAP = SET
  pred _ = Prelude'.error "hprotoc generated code: pred failure for type Network.Riak.Protocol.DtFetchResponse.DataType"

instance P'.Wire DataType where
  wireSize ft' enum = P'.wireSize ft' (Prelude'.fromEnum enum)
  wirePut ft' enum = P'.wirePut ft' (Prelude'.fromEnum enum)
  wireGet 14 = P'.wireGetEnum toMaybe'Enum
  wireGet ft' = P'.wireGetErr ft'
  wireGetPacked 14 = P'.wireGetPackedEnum toMaybe'Enum
  wireGetPacked ft' = P'.wireGetErr ft'

instance P'.GPB DataType

instance P'.MessageAPI msg' (msg' -> DataType) DataType where
  getVal m' f' = f' m'

instance P'.ReflectEnum DataType where
  reflectEnum = [(1, "COUNTER", COUNTER), (2, "SET", SET), (3, "MAP", MAP)]
  reflectEnumInfo _
   = P'.EnumInfo
      (P'.makePNF (P'.pack ".Protocol.DtFetchResponse.DataType") ["Network", "Riak"] ["Protocol", "DtFetchResponse"] "DataType")
      ["Network", "Riak", "Protocol", "DtFetchResponse", "DataType.hs"]
      [(1, "COUNTER"), (2, "SET"), (3, "MAP")]
      Prelude'.False

instance P'.TextType DataType where
  tellT = P'.tellShow
  getT = P'.getRead
