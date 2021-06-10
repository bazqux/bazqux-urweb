{-# LANGUAGE BangPatterns, DeriveDataTypeable, FlexibleInstances, MultiParamTypeClasses, TemplateHaskell #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
module Network.Riak.Protocol.BucketProps (BucketProps(..)) where
import Prelude ((+), (/))
import qualified Prelude as Prelude'
import qualified Data.Typeable as Prelude'
import qualified Data.Data as Prelude'
import qualified Text.ProtocolBuffers.Header as P'
import qualified Network.Riak.Protocol.BucketProps.ReplMode as Protocol.BucketProps (ReplMode)
import qualified Network.Riak.Protocol.CommitHook as Protocol (CommitHook)
import qualified Network.Riak.Protocol.ModFun as Protocol (ModFun)
 
data BucketProps = BucketProps{n_val :: !(P'.Maybe P'.Word32), allow_mult :: !(P'.Maybe P'.Bool),
                               last_write_wins :: !(P'.Maybe P'.Bool), precommit :: !(P'.Seq Protocol.CommitHook),
                               has_precommit :: !(P'.Maybe P'.Bool), postcommit :: !(P'.Seq Protocol.CommitHook),
                               has_postcommit :: !(P'.Maybe P'.Bool), chash_keyfun :: !(P'.Maybe Protocol.ModFun),
                               linkfun :: !(P'.Maybe Protocol.ModFun), old_vclock :: !(P'.Maybe P'.Word32),
                               young_vclock :: !(P'.Maybe P'.Word32), big_vclock :: !(P'.Maybe P'.Word32),
                               small_vclock :: !(P'.Maybe P'.Word32), pr :: !(P'.Maybe P'.Word32), r :: !(P'.Maybe P'.Word32),
                               w :: !(P'.Maybe P'.Word32), pw :: !(P'.Maybe P'.Word32), dw :: !(P'.Maybe P'.Word32),
                               rw :: !(P'.Maybe P'.Word32), basic_quorum :: !(P'.Maybe P'.Bool), notfound_ok :: !(P'.Maybe P'.Bool),
                               backend :: !(P'.Maybe P'.ByteString), search :: !(P'.Maybe P'.Bool),
                               repl :: !(P'.Maybe Protocol.BucketProps.ReplMode), search_index :: !(P'.Maybe P'.ByteString),
                               datatype :: !(P'.Maybe P'.ByteString), consistent :: !(P'.Maybe P'.Bool),
                               write_once :: !(P'.Maybe P'.Bool)}
                 deriving (Prelude'.Show, Prelude'.Eq, Prelude'.Ord, Prelude'.Typeable, Prelude'.Data)
 
instance P'.Mergeable BucketProps where
  mergeAppend
   (BucketProps x'1 x'2 x'3 x'4 x'5 x'6 x'7 x'8 x'9 x'10 x'11 x'12 x'13 x'14 x'15 x'16 x'17 x'18 x'19 x'20 x'21 x'22 x'23 x'24 x'25
     x'26 x'27 x'28)
   (BucketProps y'1 y'2 y'3 y'4 y'5 y'6 y'7 y'8 y'9 y'10 y'11 y'12 y'13 y'14 y'15 y'16 y'17 y'18 y'19 y'20 y'21 y'22 y'23 y'24 y'25
     y'26 y'27 y'28)
   = BucketProps (P'.mergeAppend x'1 y'1) (P'.mergeAppend x'2 y'2) (P'.mergeAppend x'3 y'3) (P'.mergeAppend x'4 y'4)
      (P'.mergeAppend x'5 y'5)
      (P'.mergeAppend x'6 y'6)
      (P'.mergeAppend x'7 y'7)
      (P'.mergeAppend x'8 y'8)
      (P'.mergeAppend x'9 y'9)
      (P'.mergeAppend x'10 y'10)
      (P'.mergeAppend x'11 y'11)
      (P'.mergeAppend x'12 y'12)
      (P'.mergeAppend x'13 y'13)
      (P'.mergeAppend x'14 y'14)
      (P'.mergeAppend x'15 y'15)
      (P'.mergeAppend x'16 y'16)
      (P'.mergeAppend x'17 y'17)
      (P'.mergeAppend x'18 y'18)
      (P'.mergeAppend x'19 y'19)
      (P'.mergeAppend x'20 y'20)
      (P'.mergeAppend x'21 y'21)
      (P'.mergeAppend x'22 y'22)
      (P'.mergeAppend x'23 y'23)
      (P'.mergeAppend x'24 y'24)
      (P'.mergeAppend x'25 y'25)
      (P'.mergeAppend x'26 y'26)
      (P'.mergeAppend x'27 y'27)
      (P'.mergeAppend x'28 y'28)
 
instance P'.Default BucketProps where
  defaultValue
   = BucketProps P'.defaultValue P'.defaultValue P'.defaultValue P'.defaultValue (Prelude'.Just Prelude'.False) P'.defaultValue
      (Prelude'.Just Prelude'.False)
      P'.defaultValue
      P'.defaultValue
      P'.defaultValue
      P'.defaultValue
      P'.defaultValue
      P'.defaultValue
      P'.defaultValue
      P'.defaultValue
      P'.defaultValue
      P'.defaultValue
      P'.defaultValue
      P'.defaultValue
      P'.defaultValue
      P'.defaultValue
      P'.defaultValue
      P'.defaultValue
      P'.defaultValue
      P'.defaultValue
      P'.defaultValue
      P'.defaultValue
      P'.defaultValue
 
instance P'.Wire BucketProps where
  wireSize ft'
   self'@(BucketProps x'1 x'2 x'3 x'4 x'5 x'6 x'7 x'8 x'9 x'10 x'11 x'12 x'13 x'14 x'15 x'16 x'17 x'18 x'19 x'20 x'21 x'22 x'23 x'24
           x'25 x'26 x'27 x'28)
   = case ft' of
       10 -> calc'Size
       11 -> P'.prependMessageSize calc'Size
       _ -> P'.wireSizeErr ft' self'
    where
        calc'Size
         = (P'.wireSizeOpt 1 13 x'1 + P'.wireSizeOpt 1 8 x'2 + P'.wireSizeOpt 1 8 x'3 + P'.wireSizeRep 1 11 x'4 +
             P'.wireSizeOpt 1 8 x'5
             + P'.wireSizeRep 1 11 x'6
             + P'.wireSizeOpt 1 8 x'7
             + P'.wireSizeOpt 1 11 x'8
             + P'.wireSizeOpt 1 11 x'9
             + P'.wireSizeOpt 1 13 x'10
             + P'.wireSizeOpt 1 13 x'11
             + P'.wireSizeOpt 1 13 x'12
             + P'.wireSizeOpt 1 13 x'13
             + P'.wireSizeOpt 1 13 x'14
             + P'.wireSizeOpt 1 13 x'15
             + P'.wireSizeOpt 2 13 x'16
             + P'.wireSizeOpt 2 13 x'17
             + P'.wireSizeOpt 2 13 x'18
             + P'.wireSizeOpt 2 13 x'19
             + P'.wireSizeOpt 2 8 x'20
             + P'.wireSizeOpt 2 8 x'21
             + P'.wireSizeOpt 2 12 x'22
             + P'.wireSizeOpt 2 8 x'23
             + P'.wireSizeOpt 2 14 x'24
             + P'.wireSizeOpt 2 12 x'25
             + P'.wireSizeOpt 2 12 x'26
             + P'.wireSizeOpt 2 8 x'27
             + P'.wireSizeOpt 2 8 x'28)
  wirePut ft'
   self'@(BucketProps x'1 x'2 x'3 x'4 x'5 x'6 x'7 x'8 x'9 x'10 x'11 x'12 x'13 x'14 x'15 x'16 x'17 x'18 x'19 x'20 x'21 x'22 x'23 x'24
           x'25 x'26 x'27 x'28)
   = case ft' of
       10 -> put'Fields
       11 -> do
               P'.putSize (P'.wireSize 10 self')
               put'Fields
       _ -> P'.wirePutErr ft' self'
    where
        put'Fields
         = do
             P'.wirePutOpt 8 13 x'1
             P'.wirePutOpt 16 8 x'2
             P'.wirePutOpt 24 8 x'3
             P'.wirePutRep 34 11 x'4
             P'.wirePutOpt 40 8 x'5
             P'.wirePutRep 50 11 x'6
             P'.wirePutOpt 56 8 x'7
             P'.wirePutOpt 66 11 x'8
             P'.wirePutOpt 74 11 x'9
             P'.wirePutOpt 80 13 x'10
             P'.wirePutOpt 88 13 x'11
             P'.wirePutOpt 96 13 x'12
             P'.wirePutOpt 104 13 x'13
             P'.wirePutOpt 112 13 x'14
             P'.wirePutOpt 120 13 x'15
             P'.wirePutOpt 128 13 x'16
             P'.wirePutOpt 136 13 x'17
             P'.wirePutOpt 144 13 x'18
             P'.wirePutOpt 152 13 x'19
             P'.wirePutOpt 160 8 x'20
             P'.wirePutOpt 168 8 x'21
             P'.wirePutOpt 178 12 x'22
             P'.wirePutOpt 184 8 x'23
             P'.wirePutOpt 192 14 x'24
             P'.wirePutOpt 202 12 x'25
             P'.wirePutOpt 210 12 x'26
             P'.wirePutOpt 216 8 x'27
             P'.wirePutOpt 224 8 x'28
  wireGet ft'
   = case ft' of
       10 -> P'.getBareMessageWith update'Self
       11 -> P'.getMessageWith update'Self
       _ -> P'.wireGetErr ft'
    where
        update'Self wire'Tag old'Self
         = case wire'Tag of
             8 -> Prelude'.fmap (\ !new'Field -> old'Self{n_val = Prelude'.Just new'Field}) (P'.wireGet 13)
             16 -> Prelude'.fmap (\ !new'Field -> old'Self{allow_mult = Prelude'.Just new'Field}) (P'.wireGet 8)
             24 -> Prelude'.fmap (\ !new'Field -> old'Self{last_write_wins = Prelude'.Just new'Field}) (P'.wireGet 8)
             34 -> Prelude'.fmap (\ !new'Field -> old'Self{precommit = P'.append (precommit old'Self) new'Field}) (P'.wireGet 11)
             40 -> Prelude'.fmap (\ !new'Field -> old'Self{has_precommit = Prelude'.Just new'Field}) (P'.wireGet 8)
             50 -> Prelude'.fmap (\ !new'Field -> old'Self{postcommit = P'.append (postcommit old'Self) new'Field}) (P'.wireGet 11)
             56 -> Prelude'.fmap (\ !new'Field -> old'Self{has_postcommit = Prelude'.Just new'Field}) (P'.wireGet 8)
             66 -> Prelude'.fmap
                    (\ !new'Field -> old'Self{chash_keyfun = P'.mergeAppend (chash_keyfun old'Self) (Prelude'.Just new'Field)})
                    (P'.wireGet 11)
             74 -> Prelude'.fmap (\ !new'Field -> old'Self{linkfun = P'.mergeAppend (linkfun old'Self) (Prelude'.Just new'Field)})
                    (P'.wireGet 11)
             80 -> Prelude'.fmap (\ !new'Field -> old'Self{old_vclock = Prelude'.Just new'Field}) (P'.wireGet 13)
             88 -> Prelude'.fmap (\ !new'Field -> old'Self{young_vclock = Prelude'.Just new'Field}) (P'.wireGet 13)
             96 -> Prelude'.fmap (\ !new'Field -> old'Self{big_vclock = Prelude'.Just new'Field}) (P'.wireGet 13)
             104 -> Prelude'.fmap (\ !new'Field -> old'Self{small_vclock = Prelude'.Just new'Field}) (P'.wireGet 13)
             112 -> Prelude'.fmap (\ !new'Field -> old'Self{pr = Prelude'.Just new'Field}) (P'.wireGet 13)
             120 -> Prelude'.fmap (\ !new'Field -> old'Self{r = Prelude'.Just new'Field}) (P'.wireGet 13)
             128 -> Prelude'.fmap (\ !new'Field -> old'Self{w = Prelude'.Just new'Field}) (P'.wireGet 13)
             136 -> Prelude'.fmap (\ !new'Field -> old'Self{pw = Prelude'.Just new'Field}) (P'.wireGet 13)
             144 -> Prelude'.fmap (\ !new'Field -> old'Self{dw = Prelude'.Just new'Field}) (P'.wireGet 13)
             152 -> Prelude'.fmap (\ !new'Field -> old'Self{rw = Prelude'.Just new'Field}) (P'.wireGet 13)
             160 -> Prelude'.fmap (\ !new'Field -> old'Self{basic_quorum = Prelude'.Just new'Field}) (P'.wireGet 8)
             168 -> Prelude'.fmap (\ !new'Field -> old'Self{notfound_ok = Prelude'.Just new'Field}) (P'.wireGet 8)
             178 -> Prelude'.fmap (\ !new'Field -> old'Self{backend = Prelude'.Just new'Field}) (P'.wireGet 12)
             184 -> Prelude'.fmap (\ !new'Field -> old'Self{search = Prelude'.Just new'Field}) (P'.wireGet 8)
             192 -> Prelude'.fmap (\ !new'Field -> old'Self{repl = Prelude'.Just new'Field}) (P'.wireGet 14)
             202 -> Prelude'.fmap (\ !new'Field -> old'Self{search_index = Prelude'.Just new'Field}) (P'.wireGet 12)
             210 -> Prelude'.fmap (\ !new'Field -> old'Self{datatype = Prelude'.Just new'Field}) (P'.wireGet 12)
             216 -> Prelude'.fmap (\ !new'Field -> old'Self{consistent = Prelude'.Just new'Field}) (P'.wireGet 8)
             224 -> Prelude'.fmap (\ !new'Field -> old'Self{write_once = Prelude'.Just new'Field}) (P'.wireGet 8)
             _ -> let (field'Number, wire'Type) = P'.splitWireTag wire'Tag in P'.unknown field'Number wire'Type old'Self
 
instance P'.MessageAPI msg' (msg' -> BucketProps) BucketProps where
  getVal m' f' = f' m'
 
instance P'.GPB BucketProps
 
instance P'.ReflectDescriptor BucketProps where
  getMessageInfo _
   = P'.GetMessageInfo (P'.fromDistinctAscList [])
      (P'.fromDistinctAscList
        [8, 16, 24, 34, 40, 50, 56, 66, 74, 80, 88, 96, 104, 112, 120, 128, 136, 144, 152, 160, 168, 178, 184, 192, 202, 210, 216,
         224])
  reflectDescriptorInfo _
   = Prelude'.read
      "DescriptorInfo {descName = ProtoName {protobufName = FIName \".Protocol.BucketProps\", haskellPrefix = [MName \"Network\",MName \"Riak\"], parentModule = [MName \"Protocol\"], baseName = MName \"BucketProps\"}, descFilePath = [\"Network\",\"Riak\",\"Protocol\",\"BucketProps.hs\"], isGroup = False, fields = fromList [FieldInfo {fieldName = ProtoFName {protobufName' = FIName \".Protocol.BucketProps.n_val\", haskellPrefix' = [MName \"Network\",MName \"Riak\"], parentModule' = [MName \"Protocol\",MName \"BucketProps\"], baseName' = FName \"n_val\", baseNamePrefix' = \"\"}, fieldNumber = FieldId {getFieldId = 1}, wireTag = WireTag {getWireTag = 8}, packedTag = Nothing, wireTagLength = 1, isPacked = False, isRequired = False, canRepeat = False, mightPack = False, typeCode = FieldType {getFieldType = 13}, typeName = Nothing, hsRawDefault = Nothing, hsDefault = Nothing},FieldInfo {fieldName = ProtoFName {protobufName' = FIName \".Protocol.BucketProps.allow_mult\", haskellPrefix' = [MName \"Network\",MName \"Riak\"], parentModule' = [MName \"Protocol\",MName \"BucketProps\"], baseName' = FName \"allow_mult\", baseNamePrefix' = \"\"}, fieldNumber = FieldId {getFieldId = 2}, wireTag = WireTag {getWireTag = 16}, packedTag = Nothing, wireTagLength = 1, isPacked = False, isRequired = False, canRepeat = False, mightPack = False, typeCode = FieldType {getFieldType = 8}, typeName = Nothing, hsRawDefault = Nothing, hsDefault = Nothing},FieldInfo {fieldName = ProtoFName {protobufName' = FIName \".Protocol.BucketProps.last_write_wins\", haskellPrefix' = [MName \"Network\",MName \"Riak\"], parentModule' = [MName \"Protocol\",MName \"BucketProps\"], baseName' = FName \"last_write_wins\", baseNamePrefix' = \"\"}, fieldNumber = FieldId {getFieldId = 3}, wireTag = WireTag {getWireTag = 24}, packedTag = Nothing, wireTagLength = 1, isPacked = False, isRequired = False, canRepeat = False, mightPack = False, typeCode = FieldType {getFieldType = 8}, typeName = Nothing, hsRawDefault = Nothing, hsDefault = Nothing},FieldInfo {fieldName = ProtoFName {protobufName' = FIName \".Protocol.BucketProps.precommit\", haskellPrefix' = [MName \"Network\",MName \"Riak\"], parentModule' = [MName \"Protocol\",MName \"BucketProps\"], baseName' = FName \"precommit\", baseNamePrefix' = \"\"}, fieldNumber = FieldId {getFieldId = 4}, wireTag = WireTag {getWireTag = 34}, packedTag = Nothing, wireTagLength = 1, isPacked = False, isRequired = False, canRepeat = True, mightPack = False, typeCode = FieldType {getFieldType = 11}, typeName = Just (ProtoName {protobufName = FIName \".Protocol.CommitHook\", haskellPrefix = [MName \"Network\",MName \"Riak\"], parentModule = [MName \"Protocol\"], baseName = MName \"CommitHook\"}), hsRawDefault = Nothing, hsDefault = Nothing},FieldInfo {fieldName = ProtoFName {protobufName' = FIName \".Protocol.BucketProps.has_precommit\", haskellPrefix' = [MName \"Network\",MName \"Riak\"], parentModule' = [MName \"Protocol\",MName \"BucketProps\"], baseName' = FName \"has_precommit\", baseNamePrefix' = \"\"}, fieldNumber = FieldId {getFieldId = 5}, wireTag = WireTag {getWireTag = 40}, packedTag = Nothing, wireTagLength = 1, isPacked = False, isRequired = False, canRepeat = False, mightPack = False, typeCode = FieldType {getFieldType = 8}, typeName = Nothing, hsRawDefault = Just \"false\", hsDefault = Just (HsDef'Bool False)},FieldInfo {fieldName = ProtoFName {protobufName' = FIName \".Protocol.BucketProps.postcommit\", haskellPrefix' = [MName \"Network\",MName \"Riak\"], parentModule' = [MName \"Protocol\",MName \"BucketProps\"], baseName' = FName \"postcommit\", baseNamePrefix' = \"\"}, fieldNumber = FieldId {getFieldId = 6}, wireTag = WireTag {getWireTag = 50}, packedTag = Nothing, wireTagLength = 1, isPacked = False, isRequired = False, canRepeat = True, mightPack = False, typeCode = FieldType {getFieldType = 11}, typeName = Just (ProtoName {protobufName = FIName \".Protocol.CommitHook\", haskellPrefix = [MName \"Network\",MName \"Riak\"], parentModule = [MName \"Protocol\"], baseName = MName \"CommitHook\"}), hsRawDefault = Nothing, hsDefault = Nothing},FieldInfo {fieldName = ProtoFName {protobufName' = FIName \".Protocol.BucketProps.has_postcommit\", haskellPrefix' = [MName \"Network\",MName \"Riak\"], parentModule' = [MName \"Protocol\",MName \"BucketProps\"], baseName' = FName \"has_postcommit\", baseNamePrefix' = \"\"}, fieldNumber = FieldId {getFieldId = 7}, wireTag = WireTag {getWireTag = 56}, packedTag = Nothing, wireTagLength = 1, isPacked = False, isRequired = False, canRepeat = False, mightPack = False, typeCode = FieldType {getFieldType = 8}, typeName = Nothing, hsRawDefault = Just \"false\", hsDefault = Just (HsDef'Bool False)},FieldInfo {fieldName = ProtoFName {protobufName' = FIName \".Protocol.BucketProps.chash_keyfun\", haskellPrefix' = [MName \"Network\",MName \"Riak\"], parentModule' = [MName \"Protocol\",MName \"BucketProps\"], baseName' = FName \"chash_keyfun\", baseNamePrefix' = \"\"}, fieldNumber = FieldId {getFieldId = 8}, wireTag = WireTag {getWireTag = 66}, packedTag = Nothing, wireTagLength = 1, isPacked = False, isRequired = False, canRepeat = False, mightPack = False, typeCode = FieldType {getFieldType = 11}, typeName = Just (ProtoName {protobufName = FIName \".Protocol.ModFun\", haskellPrefix = [MName \"Network\",MName \"Riak\"], parentModule = [MName \"Protocol\"], baseName = MName \"ModFun\"}), hsRawDefault = Nothing, hsDefault = Nothing},FieldInfo {fieldName = ProtoFName {protobufName' = FIName \".Protocol.BucketProps.linkfun\", haskellPrefix' = [MName \"Network\",MName \"Riak\"], parentModule' = [MName \"Protocol\",MName \"BucketProps\"], baseName' = FName \"linkfun\", baseNamePrefix' = \"\"}, fieldNumber = FieldId {getFieldId = 9}, wireTag = WireTag {getWireTag = 74}, packedTag = Nothing, wireTagLength = 1, isPacked = False, isRequired = False, canRepeat = False, mightPack = False, typeCode = FieldType {getFieldType = 11}, typeName = Just (ProtoName {protobufName = FIName \".Protocol.ModFun\", haskellPrefix = [MName \"Network\",MName \"Riak\"], parentModule = [MName \"Protocol\"], baseName = MName \"ModFun\"}), hsRawDefault = Nothing, hsDefault = Nothing},FieldInfo {fieldName = ProtoFName {protobufName' = FIName \".Protocol.BucketProps.old_vclock\", haskellPrefix' = [MName \"Network\",MName \"Riak\"], parentModule' = [MName \"Protocol\",MName \"BucketProps\"], baseName' = FName \"old_vclock\", baseNamePrefix' = \"\"}, fieldNumber = FieldId {getFieldId = 10}, wireTag = WireTag {getWireTag = 80}, packedTag = Nothing, wireTagLength = 1, isPacked = False, isRequired = False, canRepeat = False, mightPack = False, typeCode = FieldType {getFieldType = 13}, typeName = Nothing, hsRawDefault = Nothing, hsDefault = Nothing},FieldInfo {fieldName = ProtoFName {protobufName' = FIName \".Protocol.BucketProps.young_vclock\", haskellPrefix' = [MName \"Network\",MName \"Riak\"], parentModule' = [MName \"Protocol\",MName \"BucketProps\"], baseName' = FName \"young_vclock\", baseNamePrefix' = \"\"}, fieldNumber = FieldId {getFieldId = 11}, wireTag = WireTag {getWireTag = 88}, packedTag = Nothing, wireTagLength = 1, isPacked = False, isRequired = False, canRepeat = False, mightPack = False, typeCode = FieldType {getFieldType = 13}, typeName = Nothing, hsRawDefault = Nothing, hsDefault = Nothing},FieldInfo {fieldName = ProtoFName {protobufName' = FIName \".Protocol.BucketProps.big_vclock\", haskellPrefix' = [MName \"Network\",MName \"Riak\"], parentModule' = [MName \"Protocol\",MName \"BucketProps\"], baseName' = FName \"big_vclock\", baseNamePrefix' = \"\"}, fieldNumber = FieldId {getFieldId = 12}, wireTag = WireTag {getWireTag = 96}, packedTag = Nothing, wireTagLength = 1, isPacked = False, isRequired = False, canRepeat = False, mightPack = False, typeCode = FieldType {getFieldType = 13}, typeName = Nothing, hsRawDefault = Nothing, hsDefault = Nothing},FieldInfo {fieldName = ProtoFName {protobufName' = FIName \".Protocol.BucketProps.small_vclock\", haskellPrefix' = [MName \"Network\",MName \"Riak\"], parentModule' = [MName \"Protocol\",MName \"BucketProps\"], baseName' = FName \"small_vclock\", baseNamePrefix' = \"\"}, fieldNumber = FieldId {getFieldId = 13}, wireTag = WireTag {getWireTag = 104}, packedTag = Nothing, wireTagLength = 1, isPacked = False, isRequired = False, canRepeat = False, mightPack = False, typeCode = FieldType {getFieldType = 13}, typeName = Nothing, hsRawDefault = Nothing, hsDefault = Nothing},FieldInfo {fieldName = ProtoFName {protobufName' = FIName \".Protocol.BucketProps.pr\", haskellPrefix' = [MName \"Network\",MName \"Riak\"], parentModule' = [MName \"Protocol\",MName \"BucketProps\"], baseName' = FName \"pr\", baseNamePrefix' = \"\"}, fieldNumber = FieldId {getFieldId = 14}, wireTag = WireTag {getWireTag = 112}, packedTag = Nothing, wireTagLength = 1, isPacked = False, isRequired = False, canRepeat = False, mightPack = False, typeCode = FieldType {getFieldType = 13}, typeName = Nothing, hsRawDefault = Nothing, hsDefault = Nothing},FieldInfo {fieldName = ProtoFName {protobufName' = FIName \".Protocol.BucketProps.r\", haskellPrefix' = [MName \"Network\",MName \"Riak\"], parentModule' = [MName \"Protocol\",MName \"BucketProps\"], baseName' = FName \"r\", baseNamePrefix' = \"\"}, fieldNumber = FieldId {getFieldId = 15}, wireTag = WireTag {getWireTag = 120}, packedTag = Nothing, wireTagLength = 1, isPacked = False, isRequired = False, canRepeat = False, mightPack = False, typeCode = FieldType {getFieldType = 13}, typeName = Nothing, hsRawDefault = Nothing, hsDefault = Nothing},FieldInfo {fieldName = ProtoFName {protobufName' = FIName \".Protocol.BucketProps.w\", haskellPrefix' = [MName \"Network\",MName \"Riak\"], parentModule' = [MName \"Protocol\",MName \"BucketProps\"], baseName' = FName \"w\", baseNamePrefix' = \"\"}, fieldNumber = FieldId {getFieldId = 16}, wireTag = WireTag {getWireTag = 128}, packedTag = Nothing, wireTagLength = 2, isPacked = False, isRequired = False, canRepeat = False, mightPack = False, typeCode = FieldType {getFieldType = 13}, typeName = Nothing, hsRawDefault = Nothing, hsDefault = Nothing},FieldInfo {fieldName = ProtoFName {protobufName' = FIName \".Protocol.BucketProps.pw\", haskellPrefix' = [MName \"Network\",MName \"Riak\"], parentModule' = [MName \"Protocol\",MName \"BucketProps\"], baseName' = FName \"pw\", baseNamePrefix' = \"\"}, fieldNumber = FieldId {getFieldId = 17}, wireTag = WireTag {getWireTag = 136}, packedTag = Nothing, wireTagLength = 2, isPacked = False, isRequired = False, canRepeat = False, mightPack = False, typeCode = FieldType {getFieldType = 13}, typeName = Nothing, hsRawDefault = Nothing, hsDefault = Nothing},FieldInfo {fieldName = ProtoFName {protobufName' = FIName \".Protocol.BucketProps.dw\", haskellPrefix' = [MName \"Network\",MName \"Riak\"], parentModule' = [MName \"Protocol\",MName \"BucketProps\"], baseName' = FName \"dw\", baseNamePrefix' = \"\"}, fieldNumber = FieldId {getFieldId = 18}, wireTag = WireTag {getWireTag = 144}, packedTag = Nothing, wireTagLength = 2, isPacked = False, isRequired = False, canRepeat = False, mightPack = False, typeCode = FieldType {getFieldType = 13}, typeName = Nothing, hsRawDefault = Nothing, hsDefault = Nothing},FieldInfo {fieldName = ProtoFName {protobufName' = FIName \".Protocol.BucketProps.rw\", haskellPrefix' = [MName \"Network\",MName \"Riak\"], parentModule' = [MName \"Protocol\",MName \"BucketProps\"], baseName' = FName \"rw\", baseNamePrefix' = \"\"}, fieldNumber = FieldId {getFieldId = 19}, wireTag = WireTag {getWireTag = 152}, packedTag = Nothing, wireTagLength = 2, isPacked = False, isRequired = False, canRepeat = False, mightPack = False, typeCode = FieldType {getFieldType = 13}, typeName = Nothing, hsRawDefault = Nothing, hsDefault = Nothing},FieldInfo {fieldName = ProtoFName {protobufName' = FIName \".Protocol.BucketProps.basic_quorum\", haskellPrefix' = [MName \"Network\",MName \"Riak\"], parentModule' = [MName \"Protocol\",MName \"BucketProps\"], baseName' = FName \"basic_quorum\", baseNamePrefix' = \"\"}, fieldNumber = FieldId {getFieldId = 20}, wireTag = WireTag {getWireTag = 160}, packedTag = Nothing, wireTagLength = 2, isPacked = False, isRequired = False, canRepeat = False, mightPack = False, typeCode = FieldType {getFieldType = 8}, typeName = Nothing, hsRawDefault = Nothing, hsDefault = Nothing},FieldInfo {fieldName = ProtoFName {protobufName' = FIName \".Protocol.BucketProps.notfound_ok\", haskellPrefix' = [MName \"Network\",MName \"Riak\"], parentModule' = [MName \"Protocol\",MName \"BucketProps\"], baseName' = FName \"notfound_ok\", baseNamePrefix' = \"\"}, fieldNumber = FieldId {getFieldId = 21}, wireTag = WireTag {getWireTag = 168}, packedTag = Nothing, wireTagLength = 2, isPacked = False, isRequired = False, canRepeat = False, mightPack = False, typeCode = FieldType {getFieldType = 8}, typeName = Nothing, hsRawDefault = Nothing, hsDefault = Nothing},FieldInfo {fieldName = ProtoFName {protobufName' = FIName \".Protocol.BucketProps.backend\", haskellPrefix' = [MName \"Network\",MName \"Riak\"], parentModule' = [MName \"Protocol\",MName \"BucketProps\"], baseName' = FName \"backend\", baseNamePrefix' = \"\"}, fieldNumber = FieldId {getFieldId = 22}, wireTag = WireTag {getWireTag = 178}, packedTag = Nothing, wireTagLength = 2, isPacked = False, isRequired = False, canRepeat = False, mightPack = False, typeCode = FieldType {getFieldType = 12}, typeName = Nothing, hsRawDefault = Nothing, hsDefault = Nothing},FieldInfo {fieldName = ProtoFName {protobufName' = FIName \".Protocol.BucketProps.search\", haskellPrefix' = [MName \"Network\",MName \"Riak\"], parentModule' = [MName \"Protocol\",MName \"BucketProps\"], baseName' = FName \"search\", baseNamePrefix' = \"\"}, fieldNumber = FieldId {getFieldId = 23}, wireTag = WireTag {getWireTag = 184}, packedTag = Nothing, wireTagLength = 2, isPacked = False, isRequired = False, canRepeat = False, mightPack = False, typeCode = FieldType {getFieldType = 8}, typeName = Nothing, hsRawDefault = Nothing, hsDefault = Nothing},FieldInfo {fieldName = ProtoFName {protobufName' = FIName \".Protocol.BucketProps.repl\", haskellPrefix' = [MName \"Network\",MName \"Riak\"], parentModule' = [MName \"Protocol\",MName \"BucketProps\"], baseName' = FName \"repl\", baseNamePrefix' = \"\"}, fieldNumber = FieldId {getFieldId = 24}, wireTag = WireTag {getWireTag = 192}, packedTag = Nothing, wireTagLength = 2, isPacked = False, isRequired = False, canRepeat = False, mightPack = False, typeCode = FieldType {getFieldType = 14}, typeName = Just (ProtoName {protobufName = FIName \".Protocol.BucketProps.ReplMode\", haskellPrefix = [MName \"Network\",MName \"Riak\"], parentModule = [MName \"Protocol\",MName \"BucketProps\"], baseName = MName \"ReplMode\"}), hsRawDefault = Nothing, hsDefault = Nothing},FieldInfo {fieldName = ProtoFName {protobufName' = FIName \".Protocol.BucketProps.search_index\", haskellPrefix' = [MName \"Network\",MName \"Riak\"], parentModule' = [MName \"Protocol\",MName \"BucketProps\"], baseName' = FName \"search_index\", baseNamePrefix' = \"\"}, fieldNumber = FieldId {getFieldId = 25}, wireTag = WireTag {getWireTag = 202}, packedTag = Nothing, wireTagLength = 2, isPacked = False, isRequired = False, canRepeat = False, mightPack = False, typeCode = FieldType {getFieldType = 12}, typeName = Nothing, hsRawDefault = Nothing, hsDefault = Nothing},FieldInfo {fieldName = ProtoFName {protobufName' = FIName \".Protocol.BucketProps.datatype\", haskellPrefix' = [MName \"Network\",MName \"Riak\"], parentModule' = [MName \"Protocol\",MName \"BucketProps\"], baseName' = FName \"datatype\", baseNamePrefix' = \"\"}, fieldNumber = FieldId {getFieldId = 26}, wireTag = WireTag {getWireTag = 210}, packedTag = Nothing, wireTagLength = 2, isPacked = False, isRequired = False, canRepeat = False, mightPack = False, typeCode = FieldType {getFieldType = 12}, typeName = Nothing, hsRawDefault = Nothing, hsDefault = Nothing},FieldInfo {fieldName = ProtoFName {protobufName' = FIName \".Protocol.BucketProps.consistent\", haskellPrefix' = [MName \"Network\",MName \"Riak\"], parentModule' = [MName \"Protocol\",MName \"BucketProps\"], baseName' = FName \"consistent\", baseNamePrefix' = \"\"}, fieldNumber = FieldId {getFieldId = 27}, wireTag = WireTag {getWireTag = 216}, packedTag = Nothing, wireTagLength = 2, isPacked = False, isRequired = False, canRepeat = False, mightPack = False, typeCode = FieldType {getFieldType = 8}, typeName = Nothing, hsRawDefault = Nothing, hsDefault = Nothing},FieldInfo {fieldName = ProtoFName {protobufName' = FIName \".Protocol.BucketProps.write_once\", haskellPrefix' = [MName \"Network\",MName \"Riak\"], parentModule' = [MName \"Protocol\",MName \"BucketProps\"], baseName' = FName \"write_once\", baseNamePrefix' = \"\"}, fieldNumber = FieldId {getFieldId = 28}, wireTag = WireTag {getWireTag = 224}, packedTag = Nothing, wireTagLength = 2, isPacked = False, isRequired = False, canRepeat = False, mightPack = False, typeCode = FieldType {getFieldType = 8}, typeName = Nothing, hsRawDefault = Nothing, hsDefault = Nothing}], keys = fromList [], extRanges = [], knownKeys = fromList [], storeUnknown = False, lazyFields = False, makeLenses = False}"
 
instance P'.TextType BucketProps where
  tellT = P'.tellSubMessage
  getT = P'.getSubMessage
 
instance P'.TextMsg BucketProps where
  textPut msg
   = do
       P'.tellT "n_val" (n_val msg)
       P'.tellT "allow_mult" (allow_mult msg)
       P'.tellT "last_write_wins" (last_write_wins msg)
       P'.tellT "precommit" (precommit msg)
       P'.tellT "has_precommit" (has_precommit msg)
       P'.tellT "postcommit" (postcommit msg)
       P'.tellT "has_postcommit" (has_postcommit msg)
       P'.tellT "chash_keyfun" (chash_keyfun msg)
       P'.tellT "linkfun" (linkfun msg)
       P'.tellT "old_vclock" (old_vclock msg)
       P'.tellT "young_vclock" (young_vclock msg)
       P'.tellT "big_vclock" (big_vclock msg)
       P'.tellT "small_vclock" (small_vclock msg)
       P'.tellT "pr" (pr msg)
       P'.tellT "r" (r msg)
       P'.tellT "w" (w msg)
       P'.tellT "pw" (pw msg)
       P'.tellT "dw" (dw msg)
       P'.tellT "rw" (rw msg)
       P'.tellT "basic_quorum" (basic_quorum msg)
       P'.tellT "notfound_ok" (notfound_ok msg)
       P'.tellT "backend" (backend msg)
       P'.tellT "search" (search msg)
       P'.tellT "repl" (repl msg)
       P'.tellT "search_index" (search_index msg)
       P'.tellT "datatype" (datatype msg)
       P'.tellT "consistent" (consistent msg)
       P'.tellT "write_once" (write_once msg)
  textGet
   = do
       mods <- P'.sepEndBy
                (P'.choice
                  [parse'n_val, parse'allow_mult, parse'last_write_wins, parse'precommit, parse'has_precommit, parse'postcommit,
                   parse'has_postcommit, parse'chash_keyfun, parse'linkfun, parse'old_vclock, parse'young_vclock, parse'big_vclock,
                   parse'small_vclock, parse'pr, parse'r, parse'w, parse'pw, parse'dw, parse'rw, parse'basic_quorum,
                   parse'notfound_ok, parse'backend, parse'search, parse'repl, parse'search_index, parse'datatype, parse'consistent,
                   parse'write_once])
                P'.spaces
       Prelude'.return (Prelude'.foldl (\ v f -> f v) P'.defaultValue mods)
    where
        parse'n_val
         = P'.try
            (do
               v <- P'.getT "n_val"
               Prelude'.return (\ o -> o{n_val = v}))
        parse'allow_mult
         = P'.try
            (do
               v <- P'.getT "allow_mult"
               Prelude'.return (\ o -> o{allow_mult = v}))
        parse'last_write_wins
         = P'.try
            (do
               v <- P'.getT "last_write_wins"
               Prelude'.return (\ o -> o{last_write_wins = v}))
        parse'precommit
         = P'.try
            (do
               v <- P'.getT "precommit"
               Prelude'.return (\ o -> o{precommit = P'.append (precommit o) v}))
        parse'has_precommit
         = P'.try
            (do
               v <- P'.getT "has_precommit"
               Prelude'.return (\ o -> o{has_precommit = v}))
        parse'postcommit
         = P'.try
            (do
               v <- P'.getT "postcommit"
               Prelude'.return (\ o -> o{postcommit = P'.append (postcommit o) v}))
        parse'has_postcommit
         = P'.try
            (do
               v <- P'.getT "has_postcommit"
               Prelude'.return (\ o -> o{has_postcommit = v}))
        parse'chash_keyfun
         = P'.try
            (do
               v <- P'.getT "chash_keyfun"
               Prelude'.return (\ o -> o{chash_keyfun = v}))
        parse'linkfun
         = P'.try
            (do
               v <- P'.getT "linkfun"
               Prelude'.return (\ o -> o{linkfun = v}))
        parse'old_vclock
         = P'.try
            (do
               v <- P'.getT "old_vclock"
               Prelude'.return (\ o -> o{old_vclock = v}))
        parse'young_vclock
         = P'.try
            (do
               v <- P'.getT "young_vclock"
               Prelude'.return (\ o -> o{young_vclock = v}))
        parse'big_vclock
         = P'.try
            (do
               v <- P'.getT "big_vclock"
               Prelude'.return (\ o -> o{big_vclock = v}))
        parse'small_vclock
         = P'.try
            (do
               v <- P'.getT "small_vclock"
               Prelude'.return (\ o -> o{small_vclock = v}))
        parse'pr
         = P'.try
            (do
               v <- P'.getT "pr"
               Prelude'.return (\ o -> o{pr = v}))
        parse'r
         = P'.try
            (do
               v <- P'.getT "r"
               Prelude'.return (\ o -> o{r = v}))
        parse'w
         = P'.try
            (do
               v <- P'.getT "w"
               Prelude'.return (\ o -> o{w = v}))
        parse'pw
         = P'.try
            (do
               v <- P'.getT "pw"
               Prelude'.return (\ o -> o{pw = v}))
        parse'dw
         = P'.try
            (do
               v <- P'.getT "dw"
               Prelude'.return (\ o -> o{dw = v}))
        parse'rw
         = P'.try
            (do
               v <- P'.getT "rw"
               Prelude'.return (\ o -> o{rw = v}))
        parse'basic_quorum
         = P'.try
            (do
               v <- P'.getT "basic_quorum"
               Prelude'.return (\ o -> o{basic_quorum = v}))
        parse'notfound_ok
         = P'.try
            (do
               v <- P'.getT "notfound_ok"
               Prelude'.return (\ o -> o{notfound_ok = v}))
        parse'backend
         = P'.try
            (do
               v <- P'.getT "backend"
               Prelude'.return (\ o -> o{backend = v}))
        parse'search
         = P'.try
            (do
               v <- P'.getT "search"
               Prelude'.return (\ o -> o{search = v}))
        parse'repl
         = P'.try
            (do
               v <- P'.getT "repl"
               Prelude'.return (\ o -> o{repl = v}))
        parse'search_index
         = P'.try
            (do
               v <- P'.getT "search_index"
               Prelude'.return (\ o -> o{search_index = v}))
        parse'datatype
         = P'.try
            (do
               v <- P'.getT "datatype"
               Prelude'.return (\ o -> o{datatype = v}))
        parse'consistent
         = P'.try
            (do
               v <- P'.getT "consistent"
               Prelude'.return (\ o -> o{consistent = v}))
        parse'write_once
         = P'.try
            (do
               v <- P'.getT "write_once"
               Prelude'.return (\ o -> o{write_once = v}))