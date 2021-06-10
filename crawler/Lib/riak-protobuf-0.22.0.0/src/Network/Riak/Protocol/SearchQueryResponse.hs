{-# LANGUAGE BangPatterns, DeriveDataTypeable, FlexibleInstances, MultiParamTypeClasses, TemplateHaskell #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
module Network.Riak.Protocol.SearchQueryResponse (SearchQueryResponse(..)) where
import Prelude ((+), (/))
import qualified Prelude as Prelude'
import qualified Data.Typeable as Prelude'
import qualified Data.Data as Prelude'
import qualified Text.ProtocolBuffers.Header as P'
import qualified Network.Riak.Protocol.SearchDoc as Protocol (SearchDoc)
 
data SearchQueryResponse = SearchQueryResponse{docs :: !(P'.Seq Protocol.SearchDoc), max_score :: !(P'.Maybe P'.Float),
                                               num_found :: !(P'.Maybe P'.Word32)}
                         deriving (Prelude'.Show, Prelude'.Eq, Prelude'.Ord, Prelude'.Typeable, Prelude'.Data)
 
instance P'.Mergeable SearchQueryResponse where
  mergeAppend (SearchQueryResponse x'1 x'2 x'3) (SearchQueryResponse y'1 y'2 y'3)
   = SearchQueryResponse (P'.mergeAppend x'1 y'1) (P'.mergeAppend x'2 y'2) (P'.mergeAppend x'3 y'3)
 
instance P'.Default SearchQueryResponse where
  defaultValue = SearchQueryResponse P'.defaultValue P'.defaultValue P'.defaultValue
 
instance P'.Wire SearchQueryResponse where
  wireSize ft' self'@(SearchQueryResponse x'1 x'2 x'3)
   = case ft' of
       10 -> calc'Size
       11 -> P'.prependMessageSize calc'Size
       _ -> P'.wireSizeErr ft' self'
    where
        calc'Size = (P'.wireSizeRep 1 11 x'1 + P'.wireSizeOpt 1 2 x'2 + P'.wireSizeOpt 1 13 x'3)
  wirePut ft' self'@(SearchQueryResponse x'1 x'2 x'3)
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
             P'.wirePutOpt 21 2 x'2
             P'.wirePutOpt 24 13 x'3
  wireGet ft'
   = case ft' of
       10 -> P'.getBareMessageWith update'Self
       11 -> P'.getMessageWith update'Self
       _ -> P'.wireGetErr ft'
    where
        update'Self wire'Tag old'Self
         = case wire'Tag of
             10 -> Prelude'.fmap (\ !new'Field -> old'Self{docs = P'.append (docs old'Self) new'Field}) (P'.wireGet 11)
             21 -> Prelude'.fmap (\ !new'Field -> old'Self{max_score = Prelude'.Just new'Field}) (P'.wireGet 2)
             24 -> Prelude'.fmap (\ !new'Field -> old'Self{num_found = Prelude'.Just new'Field}) (P'.wireGet 13)
             _ -> let (field'Number, wire'Type) = P'.splitWireTag wire'Tag in P'.unknown field'Number wire'Type old'Self
 
instance P'.MessageAPI msg' (msg' -> SearchQueryResponse) SearchQueryResponse where
  getVal m' f' = f' m'
 
instance P'.GPB SearchQueryResponse
 
instance P'.ReflectDescriptor SearchQueryResponse where
  getMessageInfo _ = P'.GetMessageInfo (P'.fromDistinctAscList []) (P'.fromDistinctAscList [10, 21, 24])
  reflectDescriptorInfo _
   = Prelude'.read
      "DescriptorInfo {descName = ProtoName {protobufName = FIName \".Protocol.SearchQueryResponse\", haskellPrefix = [MName \"Network\",MName \"Riak\"], parentModule = [MName \"Protocol\"], baseName = MName \"SearchQueryResponse\"}, descFilePath = [\"Network\",\"Riak\",\"Protocol\",\"SearchQueryResponse.hs\"], isGroup = False, fields = fromList [FieldInfo {fieldName = ProtoFName {protobufName' = FIName \".Protocol.SearchQueryResponse.docs\", haskellPrefix' = [MName \"Network\",MName \"Riak\"], parentModule' = [MName \"Protocol\",MName \"SearchQueryResponse\"], baseName' = FName \"docs\", baseNamePrefix' = \"\"}, fieldNumber = FieldId {getFieldId = 1}, wireTag = WireTag {getWireTag = 10}, packedTag = Nothing, wireTagLength = 1, isPacked = False, isRequired = False, canRepeat = True, mightPack = False, typeCode = FieldType {getFieldType = 11}, typeName = Just (ProtoName {protobufName = FIName \".Protocol.SearchDoc\", haskellPrefix = [MName \"Network\",MName \"Riak\"], parentModule = [MName \"Protocol\"], baseName = MName \"SearchDoc\"}), hsRawDefault = Nothing, hsDefault = Nothing},FieldInfo {fieldName = ProtoFName {protobufName' = FIName \".Protocol.SearchQueryResponse.max_score\", haskellPrefix' = [MName \"Network\",MName \"Riak\"], parentModule' = [MName \"Protocol\",MName \"SearchQueryResponse\"], baseName' = FName \"max_score\", baseNamePrefix' = \"\"}, fieldNumber = FieldId {getFieldId = 2}, wireTag = WireTag {getWireTag = 21}, packedTag = Nothing, wireTagLength = 1, isPacked = False, isRequired = False, canRepeat = False, mightPack = False, typeCode = FieldType {getFieldType = 2}, typeName = Nothing, hsRawDefault = Nothing, hsDefault = Nothing},FieldInfo {fieldName = ProtoFName {protobufName' = FIName \".Protocol.SearchQueryResponse.num_found\", haskellPrefix' = [MName \"Network\",MName \"Riak\"], parentModule' = [MName \"Protocol\",MName \"SearchQueryResponse\"], baseName' = FName \"num_found\", baseNamePrefix' = \"\"}, fieldNumber = FieldId {getFieldId = 3}, wireTag = WireTag {getWireTag = 24}, packedTag = Nothing, wireTagLength = 1, isPacked = False, isRequired = False, canRepeat = False, mightPack = False, typeCode = FieldType {getFieldType = 13}, typeName = Nothing, hsRawDefault = Nothing, hsDefault = Nothing}], keys = fromList [], extRanges = [], knownKeys = fromList [], storeUnknown = False, lazyFields = False, makeLenses = False}"
 
instance P'.TextType SearchQueryResponse where
  tellT = P'.tellSubMessage
  getT = P'.getSubMessage
 
instance P'.TextMsg SearchQueryResponse where
  textPut msg
   = do
       P'.tellT "docs" (docs msg)
       P'.tellT "max_score" (max_score msg)
       P'.tellT "num_found" (num_found msg)
  textGet
   = do
       mods <- P'.sepEndBy (P'.choice [parse'docs, parse'max_score, parse'num_found]) P'.spaces
       Prelude'.return (Prelude'.foldl (\ v f -> f v) P'.defaultValue mods)
    where
        parse'docs
         = P'.try
            (do
               v <- P'.getT "docs"
               Prelude'.return (\ o -> o{docs = P'.append (docs o) v}))
        parse'max_score
         = P'.try
            (do
               v <- P'.getT "max_score"
               Prelude'.return (\ o -> o{max_score = v}))
        parse'num_found
         = P'.try
            (do
               v <- P'.getT "num_found"
               Prelude'.return (\ o -> o{num_found = v}))