{-# LANGUAGE BangPatterns, DeriveDataTypeable, FlexibleInstances, MultiParamTypeClasses, TemplateHaskell #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
module Network.Riak.Protocol.BucketKeyPreflistItem (BucketKeyPreflistItem(..)) where
import Prelude ((+), (/))
import qualified Prelude as Prelude'
import qualified Data.Typeable as Prelude'
import qualified Data.Data as Prelude'
import qualified Text.ProtocolBuffers.Header as P'
 
data BucketKeyPreflistItem = BucketKeyPreflistItem{partition :: !(P'.Int64), node :: !(P'.ByteString), primary :: !(P'.Bool)}
                           deriving (Prelude'.Show, Prelude'.Eq, Prelude'.Ord, Prelude'.Typeable, Prelude'.Data)
 
instance P'.Mergeable BucketKeyPreflistItem where
  mergeAppend (BucketKeyPreflistItem x'1 x'2 x'3) (BucketKeyPreflistItem y'1 y'2 y'3)
   = BucketKeyPreflistItem (P'.mergeAppend x'1 y'1) (P'.mergeAppend x'2 y'2) (P'.mergeAppend x'3 y'3)
 
instance P'.Default BucketKeyPreflistItem where
  defaultValue = BucketKeyPreflistItem P'.defaultValue P'.defaultValue P'.defaultValue
 
instance P'.Wire BucketKeyPreflistItem where
  wireSize ft' self'@(BucketKeyPreflistItem x'1 x'2 x'3)
   = case ft' of
       10 -> calc'Size
       11 -> P'.prependMessageSize calc'Size
       _ -> P'.wireSizeErr ft' self'
    where
        calc'Size = (P'.wireSizeReq 1 3 x'1 + P'.wireSizeReq 1 12 x'2 + P'.wireSizeReq 1 8 x'3)
  wirePut ft' self'@(BucketKeyPreflistItem x'1 x'2 x'3)
   = case ft' of
       10 -> put'Fields
       11 -> do
               P'.putSize (P'.wireSize 10 self')
               put'Fields
       _ -> P'.wirePutErr ft' self'
    where
        put'Fields
         = do
             P'.wirePutReq 8 3 x'1
             P'.wirePutReq 18 12 x'2
             P'.wirePutReq 24 8 x'3
  wireGet ft'
   = case ft' of
       10 -> P'.getBareMessageWith update'Self
       11 -> P'.getMessageWith update'Self
       _ -> P'.wireGetErr ft'
    where
        update'Self wire'Tag old'Self
         = case wire'Tag of
             8 -> Prelude'.fmap (\ !new'Field -> old'Self{partition = new'Field}) (P'.wireGet 3)
             18 -> Prelude'.fmap (\ !new'Field -> old'Self{node = new'Field}) (P'.wireGet 12)
             24 -> Prelude'.fmap (\ !new'Field -> old'Self{primary = new'Field}) (P'.wireGet 8)
             _ -> let (field'Number, wire'Type) = P'.splitWireTag wire'Tag in P'.unknown field'Number wire'Type old'Self
 
instance P'.MessageAPI msg' (msg' -> BucketKeyPreflistItem) BucketKeyPreflistItem where
  getVal m' f' = f' m'
 
instance P'.GPB BucketKeyPreflistItem
 
instance P'.ReflectDescriptor BucketKeyPreflistItem where
  getMessageInfo _ = P'.GetMessageInfo (P'.fromDistinctAscList [8, 18, 24]) (P'.fromDistinctAscList [8, 18, 24])
  reflectDescriptorInfo _
   = Prelude'.read
      "DescriptorInfo {descName = ProtoName {protobufName = FIName \".Protocol.BucketKeyPreflistItem\", haskellPrefix = [MName \"Network\",MName \"Riak\"], parentModule = [MName \"Protocol\"], baseName = MName \"BucketKeyPreflistItem\"}, descFilePath = [\"Network\",\"Riak\",\"Protocol\",\"BucketKeyPreflistItem.hs\"], isGroup = False, fields = fromList [FieldInfo {fieldName = ProtoFName {protobufName' = FIName \".Protocol.BucketKeyPreflistItem.partition\", haskellPrefix' = [MName \"Network\",MName \"Riak\"], parentModule' = [MName \"Protocol\",MName \"BucketKeyPreflistItem\"], baseName' = FName \"partition\", baseNamePrefix' = \"\"}, fieldNumber = FieldId {getFieldId = 1}, wireTag = WireTag {getWireTag = 8}, packedTag = Nothing, wireTagLength = 1, isPacked = False, isRequired = True, canRepeat = False, mightPack = False, typeCode = FieldType {getFieldType = 3}, typeName = Nothing, hsRawDefault = Nothing, hsDefault = Nothing},FieldInfo {fieldName = ProtoFName {protobufName' = FIName \".Protocol.BucketKeyPreflistItem.node\", haskellPrefix' = [MName \"Network\",MName \"Riak\"], parentModule' = [MName \"Protocol\",MName \"BucketKeyPreflistItem\"], baseName' = FName \"node\", baseNamePrefix' = \"\"}, fieldNumber = FieldId {getFieldId = 2}, wireTag = WireTag {getWireTag = 18}, packedTag = Nothing, wireTagLength = 1, isPacked = False, isRequired = True, canRepeat = False, mightPack = False, typeCode = FieldType {getFieldType = 12}, typeName = Nothing, hsRawDefault = Nothing, hsDefault = Nothing},FieldInfo {fieldName = ProtoFName {protobufName' = FIName \".Protocol.BucketKeyPreflistItem.primary\", haskellPrefix' = [MName \"Network\",MName \"Riak\"], parentModule' = [MName \"Protocol\",MName \"BucketKeyPreflistItem\"], baseName' = FName \"primary\", baseNamePrefix' = \"\"}, fieldNumber = FieldId {getFieldId = 3}, wireTag = WireTag {getWireTag = 24}, packedTag = Nothing, wireTagLength = 1, isPacked = False, isRequired = True, canRepeat = False, mightPack = False, typeCode = FieldType {getFieldType = 8}, typeName = Nothing, hsRawDefault = Nothing, hsDefault = Nothing}], keys = fromList [], extRanges = [], knownKeys = fromList [], storeUnknown = False, lazyFields = False, makeLenses = False}"
 
instance P'.TextType BucketKeyPreflistItem where
  tellT = P'.tellSubMessage
  getT = P'.getSubMessage
 
instance P'.TextMsg BucketKeyPreflistItem where
  textPut msg
   = do
       P'.tellT "partition" (partition msg)
       P'.tellT "node" (node msg)
       P'.tellT "primary" (primary msg)
  textGet
   = do
       mods <- P'.sepEndBy (P'.choice [parse'partition, parse'node, parse'primary]) P'.spaces
       Prelude'.return (Prelude'.foldl (\ v f -> f v) P'.defaultValue mods)
    where
        parse'partition
         = P'.try
            (do
               v <- P'.getT "partition"
               Prelude'.return (\ o -> o{partition = v}))
        parse'node
         = P'.try
            (do
               v <- P'.getT "node"
               Prelude'.return (\ o -> o{node = v}))
        parse'primary
         = P'.try
            (do
               v <- P'.getT "primary"
               Prelude'.return (\ o -> o{primary = v}))