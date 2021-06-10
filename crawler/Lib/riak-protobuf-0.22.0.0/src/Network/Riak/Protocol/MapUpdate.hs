{-# LANGUAGE BangPatterns, DeriveDataTypeable, FlexibleInstances, MultiParamTypeClasses, TemplateHaskell #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
module Network.Riak.Protocol.MapUpdate (MapUpdate(..)) where
import Prelude ((+), (/))
import qualified Prelude as Prelude'
import qualified Data.Typeable as Prelude'
import qualified Data.Data as Prelude'
import qualified Text.ProtocolBuffers.Header as P'
import qualified Network.Riak.Protocol.CounterOp as Protocol (CounterOp)
import qualified Network.Riak.Protocol.MapField as Protocol (MapField)
import {-# SOURCE #-} qualified Network.Riak.Protocol.MapOp as Protocol (MapOp)
import qualified Network.Riak.Protocol.MapUpdate.FlagOp as Protocol.MapUpdate (FlagOp)
import qualified Network.Riak.Protocol.SetOp as Protocol (SetOp)
 
data MapUpdate = MapUpdate{field :: !(Protocol.MapField), counter_op :: !(P'.Maybe Protocol.CounterOp),
                           set_op :: !(P'.Maybe Protocol.SetOp), register_op :: !(P'.Maybe P'.ByteString),
                           flag_op :: !(P'.Maybe Protocol.MapUpdate.FlagOp), map_op :: !(P'.Maybe Protocol.MapOp)}
               deriving (Prelude'.Show, Prelude'.Eq, Prelude'.Ord, Prelude'.Typeable, Prelude'.Data)
 
instance P'.Mergeable MapUpdate where
  mergeAppend (MapUpdate x'1 x'2 x'3 x'4 x'5 x'6) (MapUpdate y'1 y'2 y'3 y'4 y'5 y'6)
   = MapUpdate (P'.mergeAppend x'1 y'1) (P'.mergeAppend x'2 y'2) (P'.mergeAppend x'3 y'3) (P'.mergeAppend x'4 y'4)
      (P'.mergeAppend x'5 y'5)
      (P'.mergeAppend x'6 y'6)
 
instance P'.Default MapUpdate where
  defaultValue = MapUpdate P'.defaultValue P'.defaultValue P'.defaultValue P'.defaultValue P'.defaultValue P'.defaultValue
 
instance P'.Wire MapUpdate where
  wireSize ft' self'@(MapUpdate x'1 x'2 x'3 x'4 x'5 x'6)
   = case ft' of
       10 -> calc'Size
       11 -> P'.prependMessageSize calc'Size
       _ -> P'.wireSizeErr ft' self'
    where
        calc'Size
         = (P'.wireSizeReq 1 11 x'1 + P'.wireSizeOpt 1 11 x'2 + P'.wireSizeOpt 1 11 x'3 + P'.wireSizeOpt 1 12 x'4 +
             P'.wireSizeOpt 1 14 x'5
             + P'.wireSizeOpt 1 11 x'6)
  wirePut ft' self'@(MapUpdate x'1 x'2 x'3 x'4 x'5 x'6)
   = case ft' of
       10 -> put'Fields
       11 -> do
               P'.putSize (P'.wireSize 10 self')
               put'Fields
       _ -> P'.wirePutErr ft' self'
    where
        put'Fields
         = do
             P'.wirePutReq 10 11 x'1
             P'.wirePutOpt 18 11 x'2
             P'.wirePutOpt 26 11 x'3
             P'.wirePutOpt 34 12 x'4
             P'.wirePutOpt 40 14 x'5
             P'.wirePutOpt 50 11 x'6
  wireGet ft'
   = case ft' of
       10 -> P'.getBareMessageWith update'Self
       11 -> P'.getMessageWith update'Self
       _ -> P'.wireGetErr ft'
    where
        update'Self wire'Tag old'Self
         = case wire'Tag of
             10 -> Prelude'.fmap (\ !new'Field -> old'Self{field = P'.mergeAppend (field old'Self) (new'Field)}) (P'.wireGet 11)
             18 -> Prelude'.fmap
                    (\ !new'Field -> old'Self{counter_op = P'.mergeAppend (counter_op old'Self) (Prelude'.Just new'Field)})
                    (P'.wireGet 11)
             26 -> Prelude'.fmap (\ !new'Field -> old'Self{set_op = P'.mergeAppend (set_op old'Self) (Prelude'.Just new'Field)})
                    (P'.wireGet 11)
             34 -> Prelude'.fmap (\ !new'Field -> old'Self{register_op = Prelude'.Just new'Field}) (P'.wireGet 12)
             40 -> Prelude'.fmap (\ !new'Field -> old'Self{flag_op = Prelude'.Just new'Field}) (P'.wireGet 14)
             50 -> Prelude'.fmap (\ !new'Field -> old'Self{map_op = P'.mergeAppend (map_op old'Self) (Prelude'.Just new'Field)})
                    (P'.wireGet 11)
             _ -> let (field'Number, wire'Type) = P'.splitWireTag wire'Tag in P'.unknown field'Number wire'Type old'Self
 
instance P'.MessageAPI msg' (msg' -> MapUpdate) MapUpdate where
  getVal m' f' = f' m'
 
instance P'.GPB MapUpdate
 
instance P'.ReflectDescriptor MapUpdate where
  getMessageInfo _ = P'.GetMessageInfo (P'.fromDistinctAscList [10]) (P'.fromDistinctAscList [10, 18, 26, 34, 40, 50])
  reflectDescriptorInfo _
   = Prelude'.read
      "DescriptorInfo {descName = ProtoName {protobufName = FIName \".Protocol.MapUpdate\", haskellPrefix = [MName \"Network\",MName \"Riak\"], parentModule = [MName \"Protocol\"], baseName = MName \"MapUpdate\"}, descFilePath = [\"Network\",\"Riak\",\"Protocol\",\"MapUpdate.hs\"], isGroup = False, fields = fromList [FieldInfo {fieldName = ProtoFName {protobufName' = FIName \".Protocol.MapUpdate.field\", haskellPrefix' = [MName \"Network\",MName \"Riak\"], parentModule' = [MName \"Protocol\",MName \"MapUpdate\"], baseName' = FName \"field\", baseNamePrefix' = \"\"}, fieldNumber = FieldId {getFieldId = 1}, wireTag = WireTag {getWireTag = 10}, packedTag = Nothing, wireTagLength = 1, isPacked = False, isRequired = True, canRepeat = False, mightPack = False, typeCode = FieldType {getFieldType = 11}, typeName = Just (ProtoName {protobufName = FIName \".Protocol.MapField\", haskellPrefix = [MName \"Network\",MName \"Riak\"], parentModule = [MName \"Protocol\"], baseName = MName \"MapField\"}), hsRawDefault = Nothing, hsDefault = Nothing},FieldInfo {fieldName = ProtoFName {protobufName' = FIName \".Protocol.MapUpdate.counter_op\", haskellPrefix' = [MName \"Network\",MName \"Riak\"], parentModule' = [MName \"Protocol\",MName \"MapUpdate\"], baseName' = FName \"counter_op\", baseNamePrefix' = \"\"}, fieldNumber = FieldId {getFieldId = 2}, wireTag = WireTag {getWireTag = 18}, packedTag = Nothing, wireTagLength = 1, isPacked = False, isRequired = False, canRepeat = False, mightPack = False, typeCode = FieldType {getFieldType = 11}, typeName = Just (ProtoName {protobufName = FIName \".Protocol.CounterOp\", haskellPrefix = [MName \"Network\",MName \"Riak\"], parentModule = [MName \"Protocol\"], baseName = MName \"CounterOp\"}), hsRawDefault = Nothing, hsDefault = Nothing},FieldInfo {fieldName = ProtoFName {protobufName' = FIName \".Protocol.MapUpdate.set_op\", haskellPrefix' = [MName \"Network\",MName \"Riak\"], parentModule' = [MName \"Protocol\",MName \"MapUpdate\"], baseName' = FName \"set_op\", baseNamePrefix' = \"\"}, fieldNumber = FieldId {getFieldId = 3}, wireTag = WireTag {getWireTag = 26}, packedTag = Nothing, wireTagLength = 1, isPacked = False, isRequired = False, canRepeat = False, mightPack = False, typeCode = FieldType {getFieldType = 11}, typeName = Just (ProtoName {protobufName = FIName \".Protocol.SetOp\", haskellPrefix = [MName \"Network\",MName \"Riak\"], parentModule = [MName \"Protocol\"], baseName = MName \"SetOp\"}), hsRawDefault = Nothing, hsDefault = Nothing},FieldInfo {fieldName = ProtoFName {protobufName' = FIName \".Protocol.MapUpdate.register_op\", haskellPrefix' = [MName \"Network\",MName \"Riak\"], parentModule' = [MName \"Protocol\",MName \"MapUpdate\"], baseName' = FName \"register_op\", baseNamePrefix' = \"\"}, fieldNumber = FieldId {getFieldId = 4}, wireTag = WireTag {getWireTag = 34}, packedTag = Nothing, wireTagLength = 1, isPacked = False, isRequired = False, canRepeat = False, mightPack = False, typeCode = FieldType {getFieldType = 12}, typeName = Nothing, hsRawDefault = Nothing, hsDefault = Nothing},FieldInfo {fieldName = ProtoFName {protobufName' = FIName \".Protocol.MapUpdate.flag_op\", haskellPrefix' = [MName \"Network\",MName \"Riak\"], parentModule' = [MName \"Protocol\",MName \"MapUpdate\"], baseName' = FName \"flag_op\", baseNamePrefix' = \"\"}, fieldNumber = FieldId {getFieldId = 5}, wireTag = WireTag {getWireTag = 40}, packedTag = Nothing, wireTagLength = 1, isPacked = False, isRequired = False, canRepeat = False, mightPack = False, typeCode = FieldType {getFieldType = 14}, typeName = Just (ProtoName {protobufName = FIName \".Protocol.MapUpdate.FlagOp\", haskellPrefix = [MName \"Network\",MName \"Riak\"], parentModule = [MName \"Protocol\",MName \"MapUpdate\"], baseName = MName \"FlagOp\"}), hsRawDefault = Nothing, hsDefault = Nothing},FieldInfo {fieldName = ProtoFName {protobufName' = FIName \".Protocol.MapUpdate.map_op\", haskellPrefix' = [MName \"Network\",MName \"Riak\"], parentModule' = [MName \"Protocol\",MName \"MapUpdate\"], baseName' = FName \"map_op\", baseNamePrefix' = \"\"}, fieldNumber = FieldId {getFieldId = 6}, wireTag = WireTag {getWireTag = 50}, packedTag = Nothing, wireTagLength = 1, isPacked = False, isRequired = False, canRepeat = False, mightPack = False, typeCode = FieldType {getFieldType = 11}, typeName = Just (ProtoName {protobufName = FIName \".Protocol.MapOp\", haskellPrefix = [MName \"Network\",MName \"Riak\"], parentModule = [MName \"Protocol\"], baseName = MName \"MapOp\"}), hsRawDefault = Nothing, hsDefault = Nothing}], keys = fromList [], extRanges = [], knownKeys = fromList [], storeUnknown = False, lazyFields = False, makeLenses = False}"
 
instance P'.TextType MapUpdate where
  tellT = P'.tellSubMessage
  getT = P'.getSubMessage
 
instance P'.TextMsg MapUpdate where
  textPut msg
   = do
       P'.tellT "field" (field msg)
       P'.tellT "counter_op" (counter_op msg)
       P'.tellT "set_op" (set_op msg)
       P'.tellT "register_op" (register_op msg)
       P'.tellT "flag_op" (flag_op msg)
       P'.tellT "map_op" (map_op msg)
  textGet
   = do
       mods <- P'.sepEndBy (P'.choice [parse'field, parse'counter_op, parse'set_op, parse'register_op, parse'flag_op, parse'map_op])
                P'.spaces
       Prelude'.return (Prelude'.foldl (\ v f -> f v) P'.defaultValue mods)
    where
        parse'field
         = P'.try
            (do
               v <- P'.getT "field"
               Prelude'.return (\ o -> o{field = v}))
        parse'counter_op
         = P'.try
            (do
               v <- P'.getT "counter_op"
               Prelude'.return (\ o -> o{counter_op = v}))
        parse'set_op
         = P'.try
            (do
               v <- P'.getT "set_op"
               Prelude'.return (\ o -> o{set_op = v}))
        parse'register_op
         = P'.try
            (do
               v <- P'.getT "register_op"
               Prelude'.return (\ o -> o{register_op = v}))
        parse'flag_op
         = P'.try
            (do
               v <- P'.getT "flag_op"
               Prelude'.return (\ o -> o{flag_op = v}))
        parse'map_op
         = P'.try
            (do
               v <- P'.getT "map_op"
               Prelude'.return (\ o -> o{map_op = v}))