{-# LANGUAGE BangPatterns, DeriveDataTypeable, FlexibleInstances, MultiParamTypeClasses, TemplateHaskell #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
module Network.Riak.Protocol.SearchQueryRequest (SearchQueryRequest(..)) where
import Prelude ((+), (/))
import qualified Prelude as Prelude'
import qualified Data.Typeable as Prelude'
import qualified Data.Data as Prelude'
import qualified Text.ProtocolBuffers.Header as P'
 
data SearchQueryRequest = SearchQueryRequest{q :: !(P'.ByteString), index :: !(P'.ByteString), rows :: !(P'.Maybe P'.Word32),
                                             start :: !(P'.Maybe P'.Word32), sort :: !(P'.Maybe P'.ByteString),
                                             filter :: !(P'.Maybe P'.ByteString), df :: !(P'.Maybe P'.ByteString),
                                             op :: !(P'.Maybe P'.ByteString), fl :: !(P'.Seq P'.ByteString),
                                             presort :: !(P'.Maybe P'.ByteString)}
                        deriving (Prelude'.Show, Prelude'.Eq, Prelude'.Ord, Prelude'.Typeable, Prelude'.Data)
 
instance P'.Mergeable SearchQueryRequest where
  mergeAppend (SearchQueryRequest x'1 x'2 x'3 x'4 x'5 x'6 x'7 x'8 x'9 x'10)
   (SearchQueryRequest y'1 y'2 y'3 y'4 y'5 y'6 y'7 y'8 y'9 y'10)
   = SearchQueryRequest (P'.mergeAppend x'1 y'1) (P'.mergeAppend x'2 y'2) (P'.mergeAppend x'3 y'3) (P'.mergeAppend x'4 y'4)
      (P'.mergeAppend x'5 y'5)
      (P'.mergeAppend x'6 y'6)
      (P'.mergeAppend x'7 y'7)
      (P'.mergeAppend x'8 y'8)
      (P'.mergeAppend x'9 y'9)
      (P'.mergeAppend x'10 y'10)
 
instance P'.Default SearchQueryRequest where
  defaultValue
   = SearchQueryRequest P'.defaultValue P'.defaultValue P'.defaultValue P'.defaultValue P'.defaultValue P'.defaultValue
      P'.defaultValue
      P'.defaultValue
      P'.defaultValue
      P'.defaultValue
 
instance P'.Wire SearchQueryRequest where
  wireSize ft' self'@(SearchQueryRequest x'1 x'2 x'3 x'4 x'5 x'6 x'7 x'8 x'9 x'10)
   = case ft' of
       10 -> calc'Size
       11 -> P'.prependMessageSize calc'Size
       _ -> P'.wireSizeErr ft' self'
    where
        calc'Size
         = (P'.wireSizeReq 1 12 x'1 + P'.wireSizeReq 1 12 x'2 + P'.wireSizeOpt 1 13 x'3 + P'.wireSizeOpt 1 13 x'4 +
             P'.wireSizeOpt 1 12 x'5
             + P'.wireSizeOpt 1 12 x'6
             + P'.wireSizeOpt 1 12 x'7
             + P'.wireSizeOpt 1 12 x'8
             + P'.wireSizeRep 1 12 x'9
             + P'.wireSizeOpt 1 12 x'10)
  wirePut ft' self'@(SearchQueryRequest x'1 x'2 x'3 x'4 x'5 x'6 x'7 x'8 x'9 x'10)
   = case ft' of
       10 -> put'Fields
       11 -> do
               P'.putSize (P'.wireSize 10 self')
               put'Fields
       _ -> P'.wirePutErr ft' self'
    where
        put'Fields
         = do
             P'.wirePutReq 10 12 x'1
             P'.wirePutReq 18 12 x'2
             P'.wirePutOpt 24 13 x'3
             P'.wirePutOpt 32 13 x'4
             P'.wirePutOpt 42 12 x'5
             P'.wirePutOpt 50 12 x'6
             P'.wirePutOpt 58 12 x'7
             P'.wirePutOpt 66 12 x'8
             P'.wirePutRep 74 12 x'9
             P'.wirePutOpt 82 12 x'10
  wireGet ft'
   = case ft' of
       10 -> P'.getBareMessageWith update'Self
       11 -> P'.getMessageWith update'Self
       _ -> P'.wireGetErr ft'
    where
        update'Self wire'Tag old'Self
         = case wire'Tag of
             10 -> Prelude'.fmap (\ !new'Field -> old'Self{q = new'Field}) (P'.wireGet 12)
             18 -> Prelude'.fmap (\ !new'Field -> old'Self{index = new'Field}) (P'.wireGet 12)
             24 -> Prelude'.fmap (\ !new'Field -> old'Self{rows = Prelude'.Just new'Field}) (P'.wireGet 13)
             32 -> Prelude'.fmap (\ !new'Field -> old'Self{start = Prelude'.Just new'Field}) (P'.wireGet 13)
             42 -> Prelude'.fmap (\ !new'Field -> old'Self{sort = Prelude'.Just new'Field}) (P'.wireGet 12)
             50 -> Prelude'.fmap (\ !new'Field -> old'Self{filter = Prelude'.Just new'Field}) (P'.wireGet 12)
             58 -> Prelude'.fmap (\ !new'Field -> old'Self{df = Prelude'.Just new'Field}) (P'.wireGet 12)
             66 -> Prelude'.fmap (\ !new'Field -> old'Self{op = Prelude'.Just new'Field}) (P'.wireGet 12)
             74 -> Prelude'.fmap (\ !new'Field -> old'Self{fl = P'.append (fl old'Self) new'Field}) (P'.wireGet 12)
             82 -> Prelude'.fmap (\ !new'Field -> old'Self{presort = Prelude'.Just new'Field}) (P'.wireGet 12)
             _ -> let (field'Number, wire'Type) = P'.splitWireTag wire'Tag in P'.unknown field'Number wire'Type old'Self
 
instance P'.MessageAPI msg' (msg' -> SearchQueryRequest) SearchQueryRequest where
  getVal m' f' = f' m'
 
instance P'.GPB SearchQueryRequest
 
instance P'.ReflectDescriptor SearchQueryRequest where
  getMessageInfo _
   = P'.GetMessageInfo (P'.fromDistinctAscList [10, 18]) (P'.fromDistinctAscList [10, 18, 24, 32, 42, 50, 58, 66, 74, 82])
  reflectDescriptorInfo _
   = Prelude'.read
      "DescriptorInfo {descName = ProtoName {protobufName = FIName \".Protocol.SearchQueryRequest\", haskellPrefix = [MName \"Network\",MName \"Riak\"], parentModule = [MName \"Protocol\"], baseName = MName \"SearchQueryRequest\"}, descFilePath = [\"Network\",\"Riak\",\"Protocol\",\"SearchQueryRequest.hs\"], isGroup = False, fields = fromList [FieldInfo {fieldName = ProtoFName {protobufName' = FIName \".Protocol.SearchQueryRequest.q\", haskellPrefix' = [MName \"Network\",MName \"Riak\"], parentModule' = [MName \"Protocol\",MName \"SearchQueryRequest\"], baseName' = FName \"q\", baseNamePrefix' = \"\"}, fieldNumber = FieldId {getFieldId = 1}, wireTag = WireTag {getWireTag = 10}, packedTag = Nothing, wireTagLength = 1, isPacked = False, isRequired = True, canRepeat = False, mightPack = False, typeCode = FieldType {getFieldType = 12}, typeName = Nothing, hsRawDefault = Nothing, hsDefault = Nothing},FieldInfo {fieldName = ProtoFName {protobufName' = FIName \".Protocol.SearchQueryRequest.index\", haskellPrefix' = [MName \"Network\",MName \"Riak\"], parentModule' = [MName \"Protocol\",MName \"SearchQueryRequest\"], baseName' = FName \"index\", baseNamePrefix' = \"\"}, fieldNumber = FieldId {getFieldId = 2}, wireTag = WireTag {getWireTag = 18}, packedTag = Nothing, wireTagLength = 1, isPacked = False, isRequired = True, canRepeat = False, mightPack = False, typeCode = FieldType {getFieldType = 12}, typeName = Nothing, hsRawDefault = Nothing, hsDefault = Nothing},FieldInfo {fieldName = ProtoFName {protobufName' = FIName \".Protocol.SearchQueryRequest.rows\", haskellPrefix' = [MName \"Network\",MName \"Riak\"], parentModule' = [MName \"Protocol\",MName \"SearchQueryRequest\"], baseName' = FName \"rows\", baseNamePrefix' = \"\"}, fieldNumber = FieldId {getFieldId = 3}, wireTag = WireTag {getWireTag = 24}, packedTag = Nothing, wireTagLength = 1, isPacked = False, isRequired = False, canRepeat = False, mightPack = False, typeCode = FieldType {getFieldType = 13}, typeName = Nothing, hsRawDefault = Nothing, hsDefault = Nothing},FieldInfo {fieldName = ProtoFName {protobufName' = FIName \".Protocol.SearchQueryRequest.start\", haskellPrefix' = [MName \"Network\",MName \"Riak\"], parentModule' = [MName \"Protocol\",MName \"SearchQueryRequest\"], baseName' = FName \"start\", baseNamePrefix' = \"\"}, fieldNumber = FieldId {getFieldId = 4}, wireTag = WireTag {getWireTag = 32}, packedTag = Nothing, wireTagLength = 1, isPacked = False, isRequired = False, canRepeat = False, mightPack = False, typeCode = FieldType {getFieldType = 13}, typeName = Nothing, hsRawDefault = Nothing, hsDefault = Nothing},FieldInfo {fieldName = ProtoFName {protobufName' = FIName \".Protocol.SearchQueryRequest.sort\", haskellPrefix' = [MName \"Network\",MName \"Riak\"], parentModule' = [MName \"Protocol\",MName \"SearchQueryRequest\"], baseName' = FName \"sort\", baseNamePrefix' = \"\"}, fieldNumber = FieldId {getFieldId = 5}, wireTag = WireTag {getWireTag = 42}, packedTag = Nothing, wireTagLength = 1, isPacked = False, isRequired = False, canRepeat = False, mightPack = False, typeCode = FieldType {getFieldType = 12}, typeName = Nothing, hsRawDefault = Nothing, hsDefault = Nothing},FieldInfo {fieldName = ProtoFName {protobufName' = FIName \".Protocol.SearchQueryRequest.filter\", haskellPrefix' = [MName \"Network\",MName \"Riak\"], parentModule' = [MName \"Protocol\",MName \"SearchQueryRequest\"], baseName' = FName \"filter\", baseNamePrefix' = \"\"}, fieldNumber = FieldId {getFieldId = 6}, wireTag = WireTag {getWireTag = 50}, packedTag = Nothing, wireTagLength = 1, isPacked = False, isRequired = False, canRepeat = False, mightPack = False, typeCode = FieldType {getFieldType = 12}, typeName = Nothing, hsRawDefault = Nothing, hsDefault = Nothing},FieldInfo {fieldName = ProtoFName {protobufName' = FIName \".Protocol.SearchQueryRequest.df\", haskellPrefix' = [MName \"Network\",MName \"Riak\"], parentModule' = [MName \"Protocol\",MName \"SearchQueryRequest\"], baseName' = FName \"df\", baseNamePrefix' = \"\"}, fieldNumber = FieldId {getFieldId = 7}, wireTag = WireTag {getWireTag = 58}, packedTag = Nothing, wireTagLength = 1, isPacked = False, isRequired = False, canRepeat = False, mightPack = False, typeCode = FieldType {getFieldType = 12}, typeName = Nothing, hsRawDefault = Nothing, hsDefault = Nothing},FieldInfo {fieldName = ProtoFName {protobufName' = FIName \".Protocol.SearchQueryRequest.op\", haskellPrefix' = [MName \"Network\",MName \"Riak\"], parentModule' = [MName \"Protocol\",MName \"SearchQueryRequest\"], baseName' = FName \"op\", baseNamePrefix' = \"\"}, fieldNumber = FieldId {getFieldId = 8}, wireTag = WireTag {getWireTag = 66}, packedTag = Nothing, wireTagLength = 1, isPacked = False, isRequired = False, canRepeat = False, mightPack = False, typeCode = FieldType {getFieldType = 12}, typeName = Nothing, hsRawDefault = Nothing, hsDefault = Nothing},FieldInfo {fieldName = ProtoFName {protobufName' = FIName \".Protocol.SearchQueryRequest.fl\", haskellPrefix' = [MName \"Network\",MName \"Riak\"], parentModule' = [MName \"Protocol\",MName \"SearchQueryRequest\"], baseName' = FName \"fl\", baseNamePrefix' = \"\"}, fieldNumber = FieldId {getFieldId = 9}, wireTag = WireTag {getWireTag = 74}, packedTag = Nothing, wireTagLength = 1, isPacked = False, isRequired = False, canRepeat = True, mightPack = False, typeCode = FieldType {getFieldType = 12}, typeName = Nothing, hsRawDefault = Nothing, hsDefault = Nothing},FieldInfo {fieldName = ProtoFName {protobufName' = FIName \".Protocol.SearchQueryRequest.presort\", haskellPrefix' = [MName \"Network\",MName \"Riak\"], parentModule' = [MName \"Protocol\",MName \"SearchQueryRequest\"], baseName' = FName \"presort\", baseNamePrefix' = \"\"}, fieldNumber = FieldId {getFieldId = 10}, wireTag = WireTag {getWireTag = 82}, packedTag = Nothing, wireTagLength = 1, isPacked = False, isRequired = False, canRepeat = False, mightPack = False, typeCode = FieldType {getFieldType = 12}, typeName = Nothing, hsRawDefault = Nothing, hsDefault = Nothing}], keys = fromList [], extRanges = [], knownKeys = fromList [], storeUnknown = False, lazyFields = False, makeLenses = False}"
 
instance P'.TextType SearchQueryRequest where
  tellT = P'.tellSubMessage
  getT = P'.getSubMessage
 
instance P'.TextMsg SearchQueryRequest where
  textPut msg
   = do
       P'.tellT "q" (q msg)
       P'.tellT "index" (index msg)
       P'.tellT "rows" (rows msg)
       P'.tellT "start" (start msg)
       P'.tellT "sort" (sort msg)
       P'.tellT "filter" (filter msg)
       P'.tellT "df" (df msg)
       P'.tellT "op" (op msg)
       P'.tellT "fl" (fl msg)
       P'.tellT "presort" (presort msg)
  textGet
   = do
       mods <- P'.sepEndBy
                (P'.choice
                  [parse'q, parse'index, parse'rows, parse'start, parse'sort, parse'filter, parse'df, parse'op, parse'fl,
                   parse'presort])
                P'.spaces
       Prelude'.return (Prelude'.foldl (\ v f -> f v) P'.defaultValue mods)
    where
        parse'q
         = P'.try
            (do
               v <- P'.getT "q"
               Prelude'.return (\ o -> o{q = v}))
        parse'index
         = P'.try
            (do
               v <- P'.getT "index"
               Prelude'.return (\ o -> o{index = v}))
        parse'rows
         = P'.try
            (do
               v <- P'.getT "rows"
               Prelude'.return (\ o -> o{rows = v}))
        parse'start
         = P'.try
            (do
               v <- P'.getT "start"
               Prelude'.return (\ o -> o{start = v}))
        parse'sort
         = P'.try
            (do
               v <- P'.getT "sort"
               Prelude'.return (\ o -> o{sort = v}))
        parse'filter
         = P'.try
            (do
               v <- P'.getT "filter"
               Prelude'.return (\ o -> o{filter = v}))
        parse'df
         = P'.try
            (do
               v <- P'.getT "df"
               Prelude'.return (\ o -> o{df = v}))
        parse'op
         = P'.try
            (do
               v <- P'.getT "op"
               Prelude'.return (\ o -> o{op = v}))
        parse'fl
         = P'.try
            (do
               v <- P'.getT "fl"
               Prelude'.return (\ o -> o{fl = P'.append (fl o) v}))
        parse'presort
         = P'.try
            (do
               v <- P'.getT "presort"
               Prelude'.return (\ o -> o{presort = v}))