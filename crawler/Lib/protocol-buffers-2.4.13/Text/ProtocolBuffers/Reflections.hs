{-# LANGUAGE DeriveDataTypeable #-}
-- | A strong feature of the protocol-buffers package is that it does
-- not contain any structures defined by descriptor.proto!  This
-- prevents me hitting any annoying circular dependencies.  The
-- structures defined here are included in each module created by
-- 'hprotoc'.  They are optimized for use in code generation.
--
-- These values can be inspected at runtime by the user's code, but I
-- have yet to write much documentation.  Luckily the record field
-- names are somewhat descriptive.
--
-- The other reflection is using the 'fileDescriptorProto' which
-- is put into the top level module created by hprotoc.
module Text.ProtocolBuffers.Reflections
  ( ProtoName(..),ProtoFName(..),ProtoInfo(..),DescriptorInfo(..),FieldInfo(..),KeyInfo
  , HsDefault(..),SomeRealFloat(..),EnumInfo(..),EnumInfoApp
  , ReflectDescriptor(..),ReflectEnum(..),GetMessageInfo(..)
  , OneofInfo(..)
  , makePNF, toRF, fromRF
  ) where

import Text.ProtocolBuffers.Basic
import Text.ProtocolBuffers.Identifiers

import Data.List(sort)
import qualified Data.Foldable as F(toList)
import Data.Set(Set)
import qualified Data.Set as Set(fromDistinctAscList)
import Data.Generics(Data)
import Data.Typeable(Typeable)
import Data.Map(Map)

-- | 'makePNF' is used by the generated code to create a ProtoName with less newtype noise.
makePNF :: ByteString -> [String] -> [String] -> String -> ProtoName
makePNF a bs cs d =
  ProtoName (FIName (Utf8 a))
            (map MName bs)
            (map MName cs)
            (MName d)

-- | This is fully qualified name data type for code generation.  The
-- 'haskellPrefix' was possibly specified on the 'hprotoc' command
-- line.  The 'parentModule' is a combination of the module prefix
-- from the '.proto' file and any nested levels of definition.
--
-- The name components are likely to have been mangled to ensure the
-- 'baseName' started with an uppercase letter, in @ ['A'..'Z'] @.
data ProtoName = ProtoName { protobufName :: FIName Utf8     -- ^ fully qualified name using "package" prefix (no mangling)
                           , haskellPrefix :: [MName String] -- ^ Haskell specific prefix to module hierarchy (e.g. Text.Foo)
                           , parentModule :: [MName String]  -- ^ .proto specified namespace (like Com.Google.Bar)
                           , baseName :: MName String
                           }
  deriving (Show,Read,Eq,Ord,Data,Typeable)

data ProtoFName = ProtoFName { protobufName' :: FIName Utf8     -- ^ fully qualified name using "package" prefix (no mangling)
                             , haskellPrefix' :: [MName String] -- ^ Haskell specific prefix to module hierarchy (e.g. Text.Foo)
                             , parentModule' :: [MName String]  -- ^ .proto specified namespace (like Com.Google.Bar)
                             , baseName' :: FName String
                             , baseNamePrefix' :: String -- "_" if lenses are generated
                             }
  deriving (Show,Read,Eq,Ord,Data,Typeable)

data ProtoInfo = ProtoInfo { protoMod :: ProtoName        -- ^ blank protobufName, maybe blank haskellPrefix and/or parentModule
                           , protoFilePath :: [FilePath]  -- ^ path to haskell module
                           , protoSource :: FilePath      -- ^ filename without path of .proto file
                           , extensionKeys :: Seq KeyInfo -- ^ top level keys
                           , messages :: [DescriptorInfo] -- ^ all messages and groups
                           , enums :: [EnumInfo]          -- ^ all enums
                           , oneofs :: [OneofInfo]
                           , knownKeyMap :: Map ProtoName (Seq FieldInfo) -- all keys in namespace
                           }
  deriving (Show,Read,Eq,Ord,Data,Typeable)

data DescriptorInfo = DescriptorInfo { descName :: ProtoName
                                     , descFilePath :: [FilePath]
                                     , isGroup :: Bool
                                     , fields :: Seq FieldInfo
                                     , descOneofs :: Seq OneofInfo 
                                     , keys :: Seq KeyInfo
                                     , extRanges :: [(FieldId,FieldId)]
                                     , knownKeys :: Seq FieldInfo
                                     , storeUnknown :: Bool
                                     , lazyFields :: Bool
                                     , makeLenses :: Bool
                                     , jsonInstances :: Bool
                                     }
  deriving (Show,Read,Eq,Ord,Data,Typeable)


-- | 'GetMessageInfo' is used in getting messages from the wire.  It
-- supplies the 'Set' of precomposed wire tags that must be found in
-- the message as well as a 'Set' of all allowed tags (including known
-- extension fields and all required wire tags).
--
-- Extension fields not in the allowedTags set are still loaded, but
-- only as 'ByteString' blobs that will have to interpreted later.
data GetMessageInfo = GetMessageInfo { requiredTags :: Set WireTag
                                     , allowedTags :: Set WireTag
                                     }
  deriving (Show,Read,Eq,Ord,Data,Typeable)

type KeyInfo = (ProtoName,FieldInfo) -- Extendee and FieldInfo

data FieldInfo = FieldInfo { fieldName     :: ProtoFName
                           , fieldNumber   :: FieldId
                           , wireTag       :: WireTag          -- ^ Used for writing and reading if packedTag is Nothing
                           , packedTag     :: Maybe (WireTag,WireTag) -- ^ used for reading when Just {} instead of wireTag
                           , wireTagLength :: WireSize         -- ^ Bytes required in the Varint formatted wireTag
                           , isPacked      :: Bool
                           , isRequired    :: Bool
                           , canRepeat     :: Bool             -- ^ True if repeated is the field type
                           , mightPack     :: Bool             -- ^ True if packed would be valid for this field type
                           , typeCode      :: FieldType        -- ^ fromEnum of Text.DescriptorProtos.FieldDescriptorProto.Type
                           , typeName      :: Maybe ProtoName  -- ^ Set for Messages,Groups,and Enums
                           , hsRawDefault  :: Maybe ByteString -- ^ crappy, but not escaped, thing
                           , hsDefault     :: Maybe HsDefault  -- ^ nice parsed thing
                           }
  deriving (Show,Read,Eq,Ord,Data,Typeable)

-- | 'HsDefault' stores the parsed default from the proto file in a
-- form that will make a nice literal in the
-- "Language.Haskell.Exts.Syntax" code generation by 'hprotoc'.
--
-- Note that Utf8 labeled byte sequences have been stripped to just
-- 'ByteString' here as this is sufficient for code generation.
--
-- On 25 August 2010 20:12, George van den Driessche <georgevdd@google.com> sent Chris Kuklewicz a
-- patch to MakeReflections.parseDefEnum to ensure that HsDef'Enum holds the mangled form of the
-- name.
data HsDefault = HsDef'Bool Bool
               | HsDef'ByteString ByteString
               | HsDef'RealFloat SomeRealFloat
               | HsDef'Integer Integer
               | HsDef'Enum String
  deriving (Show,Read,Eq,Ord,Data,Typeable)

-- | 'SomeRealFloat' projects Double/Float to Rational or a special IEEE type.
-- This is needed to track protobuf-2.3.0 which allows nan and inf and -inf default values.
data SomeRealFloat = SRF'Rational Rational | SRF'nan | SRF'ninf | SRF'inf
  deriving (Show,Read,Eq,Ord,Data,Typeable)

toRF :: (RealFloat a, Fractional a) => SomeRealFloat -> a
toRF (SRF'Rational r) = fromRational r
toRF SRF'nan = (0/0)
toRF SRF'ninf = (-1/0)
toRF SRF'inf = (1/0)

fromRF :: (RealFloat a, Fractional a) => a -> SomeRealFloat
fromRF x | isNaN x = SRF'nan
         | isInfinite x = if 0 < x then SRF'inf else SRF'ninf
         | otherwise = SRF'Rational (toRational x)

data OneofInfo = OneofInfo { oneofName :: ProtoName
                           , oneofFName :: ProtoFName  
                           , oneofFilePath :: [FilePath]
                           , oneofFields :: Seq (ProtoName,FieldInfo)
                           , oneofMakeLenses :: Bool
                           }
  deriving (Show,Read,Eq,Ord,Data,Typeable)

data EnumInfo = EnumInfo { enumName :: ProtoName
                         , enumFilePath :: [FilePath]
                         , enumValues :: [(EnumCode,String)] -- ^ The String is the Haskell name to write into the generated source files
                         , enumJsonInstances :: Bool
                         }
  deriving (Show,Read,Eq,Ord,Data,Typeable)

type EnumInfoApp e = [(EnumCode,String,e)]


class ReflectEnum e where
  reflectEnum :: EnumInfoApp e
  reflectEnumInfo :: e -> EnumInfo            -- ^ Must not inspect argument
  parentOfEnum :: e -> Maybe DescriptorInfo   -- ^ Must not inspect argument
  parentOfEnum _ = Nothing

class ReflectDescriptor m where
  -- | This is obtained via 'read' on the stored 'show' output of the 'DescriptorInfo' in
  -- the module file. It is used in getting messages from the wire.
  -- 
  -- Must not inspect argument
  getMessageInfo :: m -> GetMessageInfo
  getMessageInfo x = cached
    where cached = makeMessageInfo (reflectDescriptorInfo (undefined `asTypeOf` x))
          makeMessageInfo :: DescriptorInfo -> GetMessageInfo
          makeMessageInfo di = GetMessageInfo { requiredTags = Set.fromDistinctAscList . sort $
                                                  [ wireTag f | f <- F.toList (fields di), isRequired f]
                                              , allowedTags = Set.fromDistinctAscList . sort $
                                                  [ wireTag f | f <- F.toList (fields di)] ++
                                                  [ wireTag f | f <- F.toList (knownKeys di)]
                                              }
  reflectDescriptorInfo :: m -> DescriptorInfo    -- ^ Must not inspect argument
