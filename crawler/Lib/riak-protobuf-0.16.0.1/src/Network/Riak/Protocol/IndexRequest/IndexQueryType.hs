{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# LANGUAGE BangPatterns, DeriveDataTypeable, FlexibleInstances, MultiParamTypeClasses #-}
module Network.Riak.Protocol.IndexRequest.IndexQueryType (IndexQueryType(..)) where
import Prelude ((+), (/), (.))
import qualified Prelude as Prelude'
import qualified Data.Typeable as Prelude'
import qualified Data.Data as Prelude'
import qualified Text.ProtocolBuffers.Header as P'
 
data IndexQueryType = Eq
                    | Range
                    deriving (Prelude'.Read, Prelude'.Show, Prelude'.Eq, Prelude'.Ord, Prelude'.Typeable, Prelude'.Data)
 
instance P'.Mergeable IndexQueryType
 
instance Prelude'.Bounded IndexQueryType where
  minBound = Eq
  maxBound = Range
 
instance P'.Default IndexQueryType where
  defaultValue = Eq
 
toMaybe'Enum :: Prelude'.Int -> P'.Maybe IndexQueryType
toMaybe'Enum 0 = Prelude'.Just Eq
toMaybe'Enum 1 = Prelude'.Just Range
toMaybe'Enum _ = Prelude'.Nothing
 
instance Prelude'.Enum IndexQueryType where
  fromEnum Eq = 0
  fromEnum Range = 1
  toEnum
   = P'.fromMaybe
      (Prelude'.error "hprotoc generated code: toEnum failure for type Network.Riak.Protocol.IndexRequest.IndexQueryType")
      . toMaybe'Enum
  succ Eq = Range
  succ _ = Prelude'.error "hprotoc generated code: succ failure for type Network.Riak.Protocol.IndexRequest.IndexQueryType"
  pred Range = Eq
  pred _ = Prelude'.error "hprotoc generated code: pred failure for type Network.Riak.Protocol.IndexRequest.IndexQueryType"
 
instance P'.Wire IndexQueryType where
  wireSize ft' enum = P'.wireSize ft' (Prelude'.fromEnum enum)
  wirePut ft' enum = P'.wirePut ft' (Prelude'.fromEnum enum)
  wireGet 14 = P'.wireGetEnum toMaybe'Enum
  wireGet ft' = P'.wireGetErr ft'
  wireGetPacked 14 = P'.wireGetPackedEnum toMaybe'Enum
  wireGetPacked ft' = P'.wireGetErr ft'
 
instance P'.GPB IndexQueryType
 
instance P'.MessageAPI msg' (msg' -> IndexQueryType) IndexQueryType where
  getVal m' f' = f' m'
 
instance P'.ReflectEnum IndexQueryType where
  reflectEnum = [(0, "Eq", Eq), (1, "Range", Range)]
  reflectEnumInfo _
   = P'.EnumInfo
      (P'.makePNF (P'.pack ".Protocol.IndexRequest.IndexQueryType") ["Network", "Riak"] ["Protocol", "IndexRequest"]
        "IndexQueryType")
      ["Network", "Riak", "Protocol", "IndexRequest", "IndexQueryType.hs"]
      [(0, "Eq"), (1, "Range")]