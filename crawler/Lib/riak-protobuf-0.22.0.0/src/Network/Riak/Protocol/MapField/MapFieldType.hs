{-# LANGUAGE BangPatterns, DeriveDataTypeable, FlexibleInstances, MultiParamTypeClasses, TemplateHaskell #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
module Network.Riak.Protocol.MapField.MapFieldType (MapFieldType(..)) where
import Prelude ((+), (/), (.))
import qualified Prelude as Prelude'
import qualified Data.Typeable as Prelude'
import qualified Data.Data as Prelude'
import qualified Text.ProtocolBuffers.Header as P'

data MapFieldType = COUNTER
                  | SET
                  | REGISTER
                  | FLAG
                  | MAP
                  deriving (Prelude'.Read, Prelude'.Show, Prelude'.Eq, Prelude'.Ord, Prelude'.Typeable, Prelude'.Data)

instance P'.Mergeable MapFieldType

instance Prelude'.Bounded MapFieldType where
  minBound = COUNTER
  maxBound = MAP

instance P'.Default MapFieldType where
  defaultValue = COUNTER

toMaybe'Enum :: Prelude'.Int -> P'.Maybe MapFieldType
toMaybe'Enum 1 = Prelude'.Just COUNTER
toMaybe'Enum 2 = Prelude'.Just SET
toMaybe'Enum 3 = Prelude'.Just REGISTER
toMaybe'Enum 4 = Prelude'.Just FLAG
toMaybe'Enum 5 = Prelude'.Just MAP
toMaybe'Enum _ = Prelude'.Nothing

instance Prelude'.Enum MapFieldType where
  fromEnum COUNTER = 1
  fromEnum SET = 2
  fromEnum REGISTER = 3
  fromEnum FLAG = 4
  fromEnum MAP = 5
  toEnum
   = P'.fromMaybe (Prelude'.error "hprotoc generated code: toEnum failure for type Network.Riak.Protocol.MapField.MapFieldType") .
      toMaybe'Enum
  succ COUNTER = SET
  succ SET = REGISTER
  succ REGISTER = FLAG
  succ FLAG = MAP
  succ _ = Prelude'.error "hprotoc generated code: succ failure for type Network.Riak.Protocol.MapField.MapFieldType"
  pred SET = COUNTER
  pred REGISTER = SET
  pred FLAG = REGISTER
  pred MAP = FLAG
  pred _ = Prelude'.error "hprotoc generated code: pred failure for type Network.Riak.Protocol.MapField.MapFieldType"

instance P'.Wire MapFieldType where
  wireSize ft' enum = P'.wireSize ft' (Prelude'.fromEnum enum)
  wirePut ft' enum = P'.wirePut ft' (Prelude'.fromEnum enum)
  wireGet 14 = P'.wireGetEnum toMaybe'Enum
  wireGet ft' = P'.wireGetErr ft'
  wireGetPacked 14 = P'.wireGetPackedEnum toMaybe'Enum
  wireGetPacked ft' = P'.wireGetErr ft'

instance P'.GPB MapFieldType

instance P'.MessageAPI msg' (msg' -> MapFieldType) MapFieldType where
  getVal m' f' = f' m'

instance P'.ReflectEnum MapFieldType where
  reflectEnum = [(1, "COUNTER", COUNTER), (2, "SET", SET), (3, "REGISTER", REGISTER), (4, "FLAG", FLAG), (5, "MAP", MAP)]
  reflectEnumInfo _
   = P'.EnumInfo
      (P'.makePNF (P'.pack ".Protocol.MapField.MapFieldType") ["Network", "Riak"] ["Protocol", "MapField"] "MapFieldType")
      ["Network", "Riak", "Protocol", "MapField", "MapFieldType.hs"]
      [(1, "COUNTER"), (2, "SET"), (3, "REGISTER"), (4, "FLAG"), (5, "MAP")]
      Prelude'.False

instance P'.TextType MapFieldType where
  tellT = P'.tellShow
  getT = P'.getRead
