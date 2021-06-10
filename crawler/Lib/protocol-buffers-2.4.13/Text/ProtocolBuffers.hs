{- | 

"Text.ProtocolBuffers" exposes the client API.  This merely re-exports parts of the
other modules in protocol-buffers.  The exposed parts are:

@
import Text.ProtocolBuffers.Basic
  ( Seq,isValidUTF8,toUtf8,utf8,Utf8(Utf8),Int32,Int64,Word32,Word64
  , WireTag,FieldId,WireType,FieldType,EnumCode,WireSize
  , Mergeable(mergeAppend,mergeConcat),Default(defaultValue))
import Text.ProtocolBuffers.Extensions
  ( Key,ExtKey(getExt,putExt,clearExt),MessageAPI(getVal,isSet)
  , getKeyFieldId,getKeyFieldType,getKeyDefaultValue)
import Text.ProtocolBuffers.Identifiers
import Text.ProtocolBuffers.Reflections
  ( ReflectDescriptor(..),ReflectEnum(..),ProtoName(..),HsDefault(..),EnumInfoApp
  , KeyInfo,FieldInfo(..),DescriptorInfo(..),EnumInfo(..),ProtoInfo(..),makePNF )
import Text.ProtocolBuffers.TextMessage
  ( messagePutText, messageGetText )
import Text.ProtocolBuffers.WireMessage
  ( Wire,Put,Get,runPut,runGet,runGetOnLazy
  , messageSize,messagePut,messageGet,messagePutM,messageGetM
  , messageWithLengthSize,messageWithLengthPut,messageWithLengthGet,messageWithLengthPutM,messageWithLengthGetM
  , messageAsFieldSize,messageAsFieldPutM,messageAsFieldGetM)
@

The message serialization is taken care of by "WireMessage"
operations, especially 'messagePut' and 'messageGet'.  The
'MessageAPI' provides the useful polymorphic 'getVal' and 'isSet'
where 'getVal' looks up default values and also works with extension
keys.  The 'Utf8' newtype is used to indicate the format in the
underlying lazy 'ByteString'.  Messages and values can be combined
with the right-biased 'Mergeable' operations.  The 'mergeEmpty' should
not be used as required values are filled in with undefined errors,
please use 'defaultValue' instead.

The Utf8 type is a newtype of the Lazy ByteString.  It can be safely
constructed by checking for errors with 'toUtf8', which returns 'Left
Int' indicating the index where an error is detected.  It can be
deconstructed with 'utf8'.

-}
module Text.ProtocolBuffers(
    module Text.ProtocolBuffers.Basic
  , module Text.ProtocolBuffers.Extensions
  , module Text.ProtocolBuffers.Identifiers
  , module Text.ProtocolBuffers.Reflections
  , module Text.ProtocolBuffers.TextMessage
  , module Text.ProtocolBuffers.WireMessage
  , module Text.ProtocolBuffers.ProtoJSON
  ) where

import Text.ProtocolBuffers.Basic
  ( Seq,isValidUTF8,toUtf8,utf8,Utf8(Utf8),Int32,Int64,Word32,Word64
  , WireTag,FieldId,WireType,FieldType,EnumCode,WireSize
  , Mergeable(mergeAppend,mergeConcat),Default(defaultValue))
import Text.ProtocolBuffers.Extensions
  ( Key,ExtKey(getExt,putExt,clearExt),MessageAPI(getVal,isSet)
  , getKeyFieldId,getKeyFieldType,getKeyDefaultValue)
import Text.ProtocolBuffers.Identifiers
import Text.ProtocolBuffers.Reflections
  ( ReflectDescriptor(..),ReflectEnum(..),ProtoName(..),HsDefault(..),EnumInfoApp
  , KeyInfo,FieldInfo(..),DescriptorInfo(..),EnumInfo(..),ProtoInfo(..),makePNF )
import Text.ProtocolBuffers.TextMessage
  ( messagePutText, messageGetText )
import Text.ProtocolBuffers.ProtoJSON
import Text.ProtocolBuffers.WireMessage
  ( Wire,Put,Get,runPut,runGet,runGetOnLazy
  , messageSize,messagePut,messageGet,messagePutM,messageGetM
  , messageWithLengthSize,messageWithLengthPut,messageWithLengthGet,messageWithLengthPutM,messageWithLengthGetM
  , messageAsFieldSize,messageAsFieldPutM,messageAsFieldGetM)
