{-# LANGUAGE BangPatterns, DeriveDataTypeable, FlexibleInstances, MultiParamTypeClasses, TemplateHaskell #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
module Network.Riak.Protocol.GetBucketKeyPreflistResponse (GetBucketKeyPreflistResponse(..)) where
import Prelude ((+), (/))
import qualified Prelude as Prelude'
import qualified Data.Typeable as Prelude'
import qualified Data.Data as Prelude'
import qualified Text.ProtocolBuffers.Header as P'
import qualified Network.Riak.Protocol.BucketKeyPreflistItem as Protocol (BucketKeyPreflistItem)
 
data GetBucketKeyPreflistResponse = GetBucketKeyPreflistResponse{preflist :: !(P'.Seq Protocol.BucketKeyPreflistItem)}
                                  deriving (Prelude'.Show, Prelude'.Eq, Prelude'.Ord, Prelude'.Typeable, Prelude'.Data)
 
instance P'.Mergeable GetBucketKeyPreflistResponse where
  mergeAppend (GetBucketKeyPreflistResponse x'1) (GetBucketKeyPreflistResponse y'1)
   = GetBucketKeyPreflistResponse (P'.mergeAppend x'1 y'1)
 
instance P'.Default GetBucketKeyPreflistResponse where
  defaultValue = GetBucketKeyPreflistResponse P'.defaultValue
 
instance P'.Wire GetBucketKeyPreflistResponse where
  wireSize ft' self'@(GetBucketKeyPreflistResponse x'1)
   = case ft' of
       10 -> calc'Size
       11 -> P'.prependMessageSize calc'Size
       _ -> P'.wireSizeErr ft' self'
    where
        calc'Size = (P'.wireSizeRep 1 11 x'1)
  wirePut ft' self'@(GetBucketKeyPreflistResponse x'1)
   = case ft' of
       10 -> put'Fields
       11 -> do
               P'.putSize (P'.wireSize 10 self')
               put'Fields
       _ -> P'.wirePutErr ft' self'
    where
        put'Fields
         = do
             P'.wirePutRep 10 11 x'1
  wireGet ft'
   = case ft' of
       10 -> P'.getBareMessageWith update'Self
       11 -> P'.getMessageWith update'Self
       _ -> P'.wireGetErr ft'
    where
        update'Self wire'Tag old'Self
         = case wire'Tag of
             10 -> Prelude'.fmap (\ !new'Field -> old'Self{preflist = P'.append (preflist old'Self) new'Field}) (P'.wireGet 11)
             _ -> let (field'Number, wire'Type) = P'.splitWireTag wire'Tag in P'.unknown field'Number wire'Type old'Self
 
instance P'.MessageAPI msg' (msg' -> GetBucketKeyPreflistResponse) GetBucketKeyPreflistResponse where
  getVal m' f' = f' m'
 
instance P'.GPB GetBucketKeyPreflistResponse
 
instance P'.ReflectDescriptor GetBucketKeyPreflistResponse where
  getMessageInfo _ = P'.GetMessageInfo (P'.fromDistinctAscList []) (P'.fromDistinctAscList [10])
  reflectDescriptorInfo _
   = Prelude'.read
      "DescriptorInfo {descName = ProtoName {protobufName = FIName \".Protocol.GetBucketKeyPreflistResponse\", haskellPrefix = [MName \"Network\",MName \"Riak\"], parentModule = [MName \"Protocol\"], baseName = MName \"GetBucketKeyPreflistResponse\"}, descFilePath = [\"Network\",\"Riak\",\"Protocol\",\"GetBucketKeyPreflistResponse.hs\"], isGroup = False, fields = fromList [FieldInfo {fieldName = ProtoFName {protobufName' = FIName \".Protocol.GetBucketKeyPreflistResponse.preflist\", haskellPrefix' = [MName \"Network\",MName \"Riak\"], parentModule' = [MName \"Protocol\",MName \"GetBucketKeyPreflistResponse\"], baseName' = FName \"preflist\", baseNamePrefix' = \"\"}, fieldNumber = FieldId {getFieldId = 1}, wireTag = WireTag {getWireTag = 10}, packedTag = Nothing, wireTagLength = 1, isPacked = False, isRequired = False, canRepeat = True, mightPack = False, typeCode = FieldType {getFieldType = 11}, typeName = Just (ProtoName {protobufName = FIName \".Protocol.BucketKeyPreflistItem\", haskellPrefix = [MName \"Network\",MName \"Riak\"], parentModule = [MName \"Protocol\"], baseName = MName \"BucketKeyPreflistItem\"}), hsRawDefault = Nothing, hsDefault = Nothing}], keys = fromList [], extRanges = [], knownKeys = fromList [], storeUnknown = False, lazyFields = False, makeLenses = False}"
 
instance P'.TextType GetBucketKeyPreflistResponse where
  tellT = P'.tellSubMessage
  getT = P'.getSubMessage
 
instance P'.TextMsg GetBucketKeyPreflistResponse where
  textPut msg
   = do
       P'.tellT "preflist" (preflist msg)
  textGet
   = do
       mods <- P'.sepEndBy (P'.choice [parse'preflist]) P'.spaces
       Prelude'.return (Prelude'.foldl (\ v f -> f v) P'.defaultValue mods)
    where
        parse'preflist
         = P'.try
            (do
               v <- P'.getT "preflist"
               Prelude'.return (\ o -> o{preflist = P'.append (preflist o) v}))