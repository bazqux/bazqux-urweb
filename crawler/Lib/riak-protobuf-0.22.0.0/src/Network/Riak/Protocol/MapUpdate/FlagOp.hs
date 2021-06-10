{-# LANGUAGE BangPatterns, DeriveDataTypeable, FlexibleInstances, MultiParamTypeClasses, TemplateHaskell #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
module Network.Riak.Protocol.MapUpdate.FlagOp (FlagOp(..)) where
import Prelude ((+), (/), (.))
import qualified Prelude as Prelude'
import qualified Data.Typeable as Prelude'
import qualified Data.Data as Prelude'
import qualified Text.ProtocolBuffers.Header as P'

data FlagOp = ENABLE
            | DISABLE
            deriving (Prelude'.Read, Prelude'.Show, Prelude'.Eq, Prelude'.Ord, Prelude'.Typeable, Prelude'.Data)

instance P'.Mergeable FlagOp

instance Prelude'.Bounded FlagOp where
  minBound = ENABLE
  maxBound = DISABLE

instance P'.Default FlagOp where
  defaultValue = ENABLE

toMaybe'Enum :: Prelude'.Int -> P'.Maybe FlagOp
toMaybe'Enum 1 = Prelude'.Just ENABLE
toMaybe'Enum 2 = Prelude'.Just DISABLE
toMaybe'Enum _ = Prelude'.Nothing

instance Prelude'.Enum FlagOp where
  fromEnum ENABLE = 1
  fromEnum DISABLE = 2
  toEnum
   = P'.fromMaybe (Prelude'.error "hprotoc generated code: toEnum failure for type Network.Riak.Protocol.MapUpdate.FlagOp") .
      toMaybe'Enum
  succ ENABLE = DISABLE
  succ _ = Prelude'.error "hprotoc generated code: succ failure for type Network.Riak.Protocol.MapUpdate.FlagOp"
  pred DISABLE = ENABLE
  pred _ = Prelude'.error "hprotoc generated code: pred failure for type Network.Riak.Protocol.MapUpdate.FlagOp"

instance P'.Wire FlagOp where
  wireSize ft' enum = P'.wireSize ft' (Prelude'.fromEnum enum)
  wirePut ft' enum = P'.wirePut ft' (Prelude'.fromEnum enum)
  wireGet 14 = P'.wireGetEnum toMaybe'Enum
  wireGet ft' = P'.wireGetErr ft'
  wireGetPacked 14 = P'.wireGetPackedEnum toMaybe'Enum
  wireGetPacked ft' = P'.wireGetErr ft'

instance P'.GPB FlagOp

instance P'.MessageAPI msg' (msg' -> FlagOp) FlagOp where
  getVal m' f' = f' m'

instance P'.ReflectEnum FlagOp where
  reflectEnum = [(1, "ENABLE", ENABLE), (2, "DISABLE", DISABLE)]
  reflectEnumInfo _
   = P'.EnumInfo (P'.makePNF (P'.pack ".Protocol.MapUpdate.FlagOp") ["Network", "Riak"] ["Protocol", "MapUpdate"] "FlagOp")
      ["Network", "Riak", "Protocol", "MapUpdate", "FlagOp.hs"]
      [(1, "ENABLE"), (2, "DISABLE")]
      Prelude'.False

instance P'.TextType FlagOp where
  tellT = P'.tellShow
  getT = P'.getRead
