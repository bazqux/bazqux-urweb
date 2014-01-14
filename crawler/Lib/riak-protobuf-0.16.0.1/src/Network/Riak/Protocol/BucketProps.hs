{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# LANGUAGE BangPatterns, DeriveDataTypeable, FlexibleInstances, MultiParamTypeClasses #-}
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
                               repl :: !(P'.Maybe Protocol.BucketProps.ReplMode)}
                 deriving (Prelude'.Show, Prelude'.Eq, Prelude'.Ord, Prelude'.Typeable, Prelude'.Data)
 
instance P'.Mergeable BucketProps where
  mergeAppend
   (BucketProps x'1 x'2 x'3 x'4 x'5 x'6 x'7 x'8 x'9 x'10 x'11 x'12 x'13 x'14 x'15 x'16 x'17 x'18 x'19 x'20 x'21 x'22 x'23 x'24)
   (BucketProps y'1 y'2 y'3 y'4 y'5 y'6 y'7 y'8 y'9 y'10 y'11 y'12 y'13 y'14 y'15 y'16 y'17 y'18 y'19 y'20 y'21 y'22 y'23 y'24)
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
 
instance P'.Wire BucketProps where
  wireSize ft'
   self'@(BucketProps x'1 x'2 x'3 x'4 x'5 x'6 x'7 x'8 x'9 x'10 x'11 x'12 x'13 x'14 x'15 x'16 x'17 x'18 x'19 x'20 x'21 x'22 x'23
           x'24)
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
             + P'.wireSizeOpt 2 14 x'24)
  wirePut ft'
   self'@(BucketProps x'1 x'2 x'3 x'4 x'5 x'6 x'7 x'8 x'9 x'10 x'11 x'12 x'13 x'14 x'15 x'16 x'17 x'18 x'19 x'20 x'21 x'22 x'23
           x'24)
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
             _ -> let (field'Number, wire'Type) = P'.splitWireTag wire'Tag in P'.unknown field'Number wire'Type old'Self
 
instance P'.MessageAPI msg' (msg' -> BucketProps) BucketProps where
  getVal m' f' = f' m'
 
instance P'.GPB BucketProps
 
instance P'.ReflectDescriptor BucketProps where
  getMessageInfo _
   = P'.GetMessageInfo (P'.fromDistinctAscList [])
      (P'.fromDistinctAscList
        [8, 16, 24, 34, 40, 50, 56, 66, 74, 80, 88, 96, 104, 112, 120, 128, 136, 144, 152, 160, 168, 178, 184, 192])
  reflectDescriptorInfo _
   = Prelude'.read
      "DescriptorInfo {descName = ProtoName {protobufName = FIName \".Protocol.BucketProps\", haskellPrefix = [MName \"Network\",MName \"Riak\"], parentModule = [MName \"Protocol\"], baseName = MName \"BucketProps\"}, descFilePath = [\"Network\",\"Riak\",\"Protocol\",\"BucketProps.hs\"], isGroup = False, fields = fromList [FieldInfo {fieldName = ProtoFName {protobufName' = FIName \".Protocol.BucketProps.n_val\", haskellPrefix' = [MName \"Network\",MName \"Riak\"], parentModule' = [MName \"Protocol\",MName \"BucketProps\"], baseName' = FName \"n_val\"}, fieldNumber = FieldId {getFieldId = 1}, wireTag = WireTag {getWireTag = 8}, packedTag = Nothing, wireTagLength = 1, isPacked = False, isRequired = False, canRepeat = False, mightPack = False, typeCode = FieldType {getFieldType = 13}, typeName = Nothing, hsRawDefault = Nothing, hsDefault = Nothing},FieldInfo {fieldName = ProtoFName {protobufName' = FIName \".Protocol.BucketProps.allow_mult\", haskellPrefix' = [MName \"Network\",MName \"Riak\"], parentModule' = [MName \"Protocol\",MName \"BucketProps\"], baseName' = FName \"allow_mult\"}, fieldNumber = FieldId {getFieldId = 2}, wireTag = WireTag {getWireTag = 16}, packedTag = Nothing, wireTagLength = 1, isPacked = False, isRequired = False, canRepeat = False, mightPack = False, typeCode = FieldType {getFieldType = 8}, typeName = Nothing, hsRawDefault = Nothing, hsDefault = Nothing},FieldInfo {fieldName = ProtoFName {protobufName' = FIName \".Protocol.BucketProps.last_write_wins\", haskellPrefix' = [MName \"Network\",MName \"Riak\"], parentModule' = [MName \"Protocol\",MName \"BucketProps\"], baseName' = FName \"last_write_wins\"}, fieldNumber = FieldId {getFieldId = 3}, wireTag = WireTag {getWireTag = 24}, packedTag = Nothing, wireTagLength = 1, isPacked = False, isRequired = False, canRepeat = False, mightPack = False, typeCode = FieldType {getFieldType = 8}, typeName = Nothing, hsRawDefault = Nothing, hsDefault = Nothing},FieldInfo {fieldName = ProtoFName {protobufName' = FIName \".Protocol.BucketProps.precommit\", haskellPrefix' = [MName \"Network\",MName \"Riak\"], parentModule' = [MName \"Protocol\",MName \"BucketProps\"], baseName' = FName \"precommit\"}, fieldNumber = FieldId {getFieldId = 4}, wireTag = WireTag {getWireTag = 34}, packedTag = Nothing, wireTagLength = 1, isPacked = False, isRequired = False, canRepeat = True, mightPack = False, typeCode = FieldType {getFieldType = 11}, typeName = Just (ProtoName {protobufName = FIName \".Protocol.CommitHook\", haskellPrefix = [MName \"Network\",MName \"Riak\"], parentModule = [MName \"Protocol\"], baseName = MName \"CommitHook\"}), hsRawDefault = Nothing, hsDefault = Nothing},FieldInfo {fieldName = ProtoFName {protobufName' = FIName \".Protocol.BucketProps.has_precommit\", haskellPrefix' = [MName \"Network\",MName \"Riak\"], parentModule' = [MName \"Protocol\",MName \"BucketProps\"], baseName' = FName \"has_precommit\"}, fieldNumber = FieldId {getFieldId = 5}, wireTag = WireTag {getWireTag = 40}, packedTag = Nothing, wireTagLength = 1, isPacked = False, isRequired = False, canRepeat = False, mightPack = False, typeCode = FieldType {getFieldType = 8}, typeName = Nothing, hsRawDefault = Just \"false\", hsDefault = Just (HsDef'Bool False)},FieldInfo {fieldName = ProtoFName {protobufName' = FIName \".Protocol.BucketProps.postcommit\", haskellPrefix' = [MName \"Network\",MName \"Riak\"], parentModule' = [MName \"Protocol\",MName \"BucketProps\"], baseName' = FName \"postcommit\"}, fieldNumber = FieldId {getFieldId = 6}, wireTag = WireTag {getWireTag = 50}, packedTag = Nothing, wireTagLength = 1, isPacked = False, isRequired = False, canRepeat = True, mightPack = False, typeCode = FieldType {getFieldType = 11}, typeName = Just (ProtoName {protobufName = FIName \".Protocol.CommitHook\", haskellPrefix = [MName \"Network\",MName \"Riak\"], parentModule = [MName \"Protocol\"], baseName = MName \"CommitHook\"}), hsRawDefault = Nothing, hsDefault = Nothing},FieldInfo {fieldName = ProtoFName {protobufName' = FIName \".Protocol.BucketProps.has_postcommit\", haskellPrefix' = [MName \"Network\",MName \"Riak\"], parentModule' = [MName \"Protocol\",MName \"BucketProps\"], baseName' = FName \"has_postcommit\"}, fieldNumber = FieldId {getFieldId = 7}, wireTag = WireTag {getWireTag = 56}, packedTag = Nothing, wireTagLength = 1, isPacked = False, isRequired = False, canRepeat = False, mightPack = False, typeCode = FieldType {getFieldType = 8}, typeName = Nothing, hsRawDefault = Just \"false\", hsDefault = Just (HsDef'Bool False)},FieldInfo {fieldName = ProtoFName {protobufName' = FIName \".Protocol.BucketProps.chash_keyfun\", haskellPrefix' = [MName \"Network\",MName \"Riak\"], parentModule' = [MName \"Protocol\",MName \"BucketProps\"], baseName' = FName \"chash_keyfun\"}, fieldNumber = FieldId {getFieldId = 8}, wireTag = WireTag {getWireTag = 66}, packedTag = Nothing, wireTagLength = 1, isPacked = False, isRequired = False, canRepeat = False, mightPack = False, typeCode = FieldType {getFieldType = 11}, typeName = Just (ProtoName {protobufName = FIName \".Protocol.ModFun\", haskellPrefix = [MName \"Network\",MName \"Riak\"], parentModule = [MName \"Protocol\"], baseName = MName \"ModFun\"}), hsRawDefault = Nothing, hsDefault = Nothing},FieldInfo {fieldName = ProtoFName {protobufName' = FIName \".Protocol.BucketProps.linkfun\", haskellPrefix' = [MName \"Network\",MName \"Riak\"], parentModule' = [MName \"Protocol\",MName \"BucketProps\"], baseName' = FName \"linkfun\"}, fieldNumber = FieldId {getFieldId = 9}, wireTag = WireTag {getWireTag = 74}, packedTag = Nothing, wireTagLength = 1, isPacked = False, isRequired = False, canRepeat = False, mightPack = False, typeCode = FieldType {getFieldType = 11}, typeName = Just (ProtoName {protobufName = FIName \".Protocol.ModFun\", haskellPrefix = [MName \"Network\",MName \"Riak\"], parentModule = [MName \"Protocol\"], baseName = MName \"ModFun\"}), hsRawDefault = Nothing, hsDefault = Nothing},FieldInfo {fieldName = ProtoFName {protobufName' = FIName \".Protocol.BucketProps.old_vclock\", haskellPrefix' = [MName \"Network\",MName \"Riak\"], parentModule' = [MName \"Protocol\",MName \"BucketProps\"], baseName' = FName \"old_vclock\"}, fieldNumber = FieldId {getFieldId = 10}, wireTag = WireTag {getWireTag = 80}, packedTag = Nothing, wireTagLength = 1, isPacked = False, isRequired = False, canRepeat = False, mightPack = False, typeCode = FieldType {getFieldType = 13}, typeName = Nothing, hsRawDefault = Nothing, hsDefault = Nothing},FieldInfo {fieldName = ProtoFName {protobufName' = FIName \".Protocol.BucketProps.young_vclock\", haskellPrefix' = [MName \"Network\",MName \"Riak\"], parentModule' = [MName \"Protocol\",MName \"BucketProps\"], baseName' = FName \"young_vclock\"}, fieldNumber = FieldId {getFieldId = 11}, wireTag = WireTag {getWireTag = 88}, packedTag = Nothing, wireTagLength = 1, isPacked = False, isRequired = False, canRepeat = False, mightPack = False, typeCode = FieldType {getFieldType = 13}, typeName = Nothing, hsRawDefault = Nothing, hsDefault = Nothing},FieldInfo {fieldName = ProtoFName {protobufName' = FIName \".Protocol.BucketProps.big_vclock\", haskellPrefix' = [MName \"Network\",MName \"Riak\"], parentModule' = [MName \"Protocol\",MName \"BucketProps\"], baseName' = FName \"big_vclock\"}, fieldNumber = FieldId {getFieldId = 12}, wireTag = WireTag {getWireTag = 96}, packedTag = Nothing, wireTagLength = 1, isPacked = False, isRequired = False, canRepeat = False, mightPack = False, typeCode = FieldType {getFieldType = 13}, typeName = Nothing, hsRawDefault = Nothing, hsDefault = Nothing},FieldInfo {fieldName = ProtoFName {protobufName' = FIName \".Protocol.BucketProps.small_vclock\", haskellPrefix' = [MName \"Network\",MName \"Riak\"], parentModule' = [MName \"Protocol\",MName \"BucketProps\"], baseName' = FName \"small_vclock\"}, fieldNumber = FieldId {getFieldId = 13}, wireTag = WireTag {getWireTag = 104}, packedTag = Nothing, wireTagLength = 1, isPacked = False, isRequired = False, canRepeat = False, mightPack = False, typeCode = FieldType {getFieldType = 13}, typeName = Nothing, hsRawDefault = Nothing, hsDefault = Nothing},FieldInfo {fieldName = ProtoFName {protobufName' = FIName \".Protocol.BucketProps.pr\", haskellPrefix' = [MName \"Network\",MName \"Riak\"], parentModule' = [MName \"Protocol\",MName \"BucketProps\"], baseName' = FName \"pr\"}, fieldNumber = FieldId {getFieldId = 14}, wireTag = WireTag {getWireTag = 112}, packedTag = Nothing, wireTagLength = 1, isPacked = False, isRequired = False, canRepeat = False, mightPack = False, typeCode = FieldType {getFieldType = 13}, typeName = Nothing, hsRawDefault = Nothing, hsDefault = Nothing},FieldInfo {fieldName = ProtoFName {protobufName' = FIName \".Protocol.BucketProps.r\", haskellPrefix' = [MName \"Network\",MName \"Riak\"], parentModule' = [MName \"Protocol\",MName \"BucketProps\"], baseName' = FName \"r\"}, fieldNumber = FieldId {getFieldId = 15}, wireTag = WireTag {getWireTag = 120}, packedTag = Nothing, wireTagLength = 1, isPacked = False, isRequired = False, canRepeat = False, mightPack = False, typeCode = FieldType {getFieldType = 13}, typeName = Nothing, hsRawDefault = Nothing, hsDefault = Nothing},FieldInfo {fieldName = ProtoFName {protobufName' = FIName \".Protocol.BucketProps.w\", haskellPrefix' = [MName \"Network\",MName \"Riak\"], parentModule' = [MName \"Protocol\",MName \"BucketProps\"], baseName' = FName \"w\"}, fieldNumber = FieldId {getFieldId = 16}, wireTag = WireTag {getWireTag = 128}, packedTag = Nothing, wireTagLength = 2, isPacked = False, isRequired = False, canRepeat = False, mightPack = False, typeCode = FieldType {getFieldType = 13}, typeName = Nothing, hsRawDefault = Nothing, hsDefault = Nothing},FieldInfo {fieldName = ProtoFName {protobufName' = FIName \".Protocol.BucketProps.pw\", haskellPrefix' = [MName \"Network\",MName \"Riak\"], parentModule' = [MName \"Protocol\",MName \"BucketProps\"], baseName' = FName \"pw\"}, fieldNumber = FieldId {getFieldId = 17}, wireTag = WireTag {getWireTag = 136}, packedTag = Nothing, wireTagLength = 2, isPacked = False, isRequired = False, canRepeat = False, mightPack = False, typeCode = FieldType {getFieldType = 13}, typeName = Nothing, hsRawDefault = Nothing, hsDefault = Nothing},FieldInfo {fieldName = ProtoFName {protobufName' = FIName \".Protocol.BucketProps.dw\", haskellPrefix' = [MName \"Network\",MName \"Riak\"], parentModule' = [MName \"Protocol\",MName \"BucketProps\"], baseName' = FName \"dw\"}, fieldNumber = FieldId {getFieldId = 18}, wireTag = WireTag {getWireTag = 144}, packedTag = Nothing, wireTagLength = 2, isPacked = False, isRequired = False, canRepeat = False, mightPack = False, typeCode = FieldType {getFieldType = 13}, typeName = Nothing, hsRawDefault = Nothing, hsDefault = Nothing},FieldInfo {fieldName = ProtoFName {protobufName' = FIName \".Protocol.BucketProps.rw\", haskellPrefix' = [MName \"Network\",MName \"Riak\"], parentModule' = [MName \"Protocol\",MName \"BucketProps\"], baseName' = FName \"rw\"}, fieldNumber = FieldId {getFieldId = 19}, wireTag = WireTag {getWireTag = 152}, packedTag = Nothing, wireTagLength = 2, isPacked = False, isRequired = False, canRepeat = False, mightPack = False, typeCode = FieldType {getFieldType = 13}, typeName = Nothing, hsRawDefault = Nothing, hsDefault = Nothing},FieldInfo {fieldName = ProtoFName {protobufName' = FIName \".Protocol.BucketProps.basic_quorum\", haskellPrefix' = [MName \"Network\",MName \"Riak\"], parentModule' = [MName \"Protocol\",MName \"BucketProps\"], baseName' = FName \"basic_quorum\"}, fieldNumber = FieldId {getFieldId = 20}, wireTag = WireTag {getWireTag = 160}, packedTag = Nothing, wireTagLength = 2, isPacked = False, isRequired = False, canRepeat = False, mightPack = False, typeCode = FieldType {getFieldType = 8}, typeName = Nothing, hsRawDefault = Nothing, hsDefault = Nothing},FieldInfo {fieldName = ProtoFName {protobufName' = FIName \".Protocol.BucketProps.notfound_ok\", haskellPrefix' = [MName \"Network\",MName \"Riak\"], parentModule' = [MName \"Protocol\",MName \"BucketProps\"], baseName' = FName \"notfound_ok\"}, fieldNumber = FieldId {getFieldId = 21}, wireTag = WireTag {getWireTag = 168}, packedTag = Nothing, wireTagLength = 2, isPacked = False, isRequired = False, canRepeat = False, mightPack = False, typeCode = FieldType {getFieldType = 8}, typeName = Nothing, hsRawDefault = Nothing, hsDefault = Nothing},FieldInfo {fieldName = ProtoFName {protobufName' = FIName \".Protocol.BucketProps.backend\", haskellPrefix' = [MName \"Network\",MName \"Riak\"], parentModule' = [MName \"Protocol\",MName \"BucketProps\"], baseName' = FName \"backend\"}, fieldNumber = FieldId {getFieldId = 22}, wireTag = WireTag {getWireTag = 178}, packedTag = Nothing, wireTagLength = 2, isPacked = False, isRequired = False, canRepeat = False, mightPack = False, typeCode = FieldType {getFieldType = 12}, typeName = Nothing, hsRawDefault = Nothing, hsDefault = Nothing},FieldInfo {fieldName = ProtoFName {protobufName' = FIName \".Protocol.BucketProps.search\", haskellPrefix' = [MName \"Network\",MName \"Riak\"], parentModule' = [MName \"Protocol\",MName \"BucketProps\"], baseName' = FName \"search\"}, fieldNumber = FieldId {getFieldId = 23}, wireTag = WireTag {getWireTag = 184}, packedTag = Nothing, wireTagLength = 2, isPacked = False, isRequired = False, canRepeat = False, mightPack = False, typeCode = FieldType {getFieldType = 8}, typeName = Nothing, hsRawDefault = Nothing, hsDefault = Nothing},FieldInfo {fieldName = ProtoFName {protobufName' = FIName \".Protocol.BucketProps.repl\", haskellPrefix' = [MName \"Network\",MName \"Riak\"], parentModule' = [MName \"Protocol\",MName \"BucketProps\"], baseName' = FName \"repl\"}, fieldNumber = FieldId {getFieldId = 24}, wireTag = WireTag {getWireTag = 192}, packedTag = Nothing, wireTagLength = 2, isPacked = False, isRequired = False, canRepeat = False, mightPack = False, typeCode = FieldType {getFieldType = 14}, typeName = Just (ProtoName {protobufName = FIName \".Protocol.BucketProps.ReplMode\", haskellPrefix = [MName \"Network\",MName \"Riak\"], parentModule = [MName \"Protocol\",MName \"BucketProps\"], baseName = MName \"ReplMode\"}), hsRawDefault = Nothing, hsDefault = Nothing}], keys = fromList [], extRanges = [], knownKeys = fromList [], storeUnknown = False, lazyFields = False}"