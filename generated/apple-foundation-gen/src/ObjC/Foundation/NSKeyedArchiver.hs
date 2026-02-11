{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Generated bindings for @NSKeyedArchiver@.
module ObjC.Foundation.NSKeyedArchiver
  ( NSKeyedArchiver
  , IsNSKeyedArchiver(..)
  , initRequiringSecureCoding
  , archivedDataWithRootObject_requiringSecureCoding_error
  , init_
  , initForWritingWithMutableData
  , archivedDataWithRootObject
  , archiveRootObject_toFile
  , finishEncoding
  , nsKeyedArchiverSetClassName_forClass
  , setClassName_forClass
  , nsKeyedArchiverClassNameForClass
  , classNameForClass
  , encodeObject_forKey
  , encodeConditionalObject_forKey
  , encodeBool_forKey
  , encodeInt_forKey
  , encodeInt32_forKey
  , encodeInt64_forKey
  , encodeFloat_forKey
  , encodeDouble_forKey
  , encodeBytes_length_forKey
  , delegate
  , setDelegate
  , outputFormat
  , setOutputFormat
  , encodedData
  , requiresSecureCoding
  , setRequiresSecureCoding
  , initRequiringSecureCodingSelector
  , archivedDataWithRootObject_requiringSecureCoding_errorSelector
  , initSelector
  , initForWritingWithMutableDataSelector
  , archivedDataWithRootObjectSelector
  , archiveRootObject_toFileSelector
  , finishEncodingSelector
  , setClassName_forClassSelector
  , classNameForClassSelector
  , encodeObject_forKeySelector
  , encodeConditionalObject_forKeySelector
  , encodeBool_forKeySelector
  , encodeInt_forKeySelector
  , encodeInt32_forKeySelector
  , encodeInt64_forKeySelector
  , encodeFloat_forKeySelector
  , encodeDouble_forKeySelector
  , encodeBytes_length_forKeySelector
  , delegateSelector
  , setDelegateSelector
  , outputFormatSelector
  , setOutputFormatSelector
  , encodedDataSelector
  , requiresSecureCodingSelector
  , setRequiresSecureCodingSelector

  -- * Enum types
  , NSPropertyListFormat(NSPropertyListFormat)
  , pattern NSPropertyListOpenStepFormat
  , pattern NSPropertyListXMLFormat_v1_0
  , pattern NSPropertyListBinaryFormat_v1_0

  ) where

import Foreign.Ptr (Ptr, nullPtr, castPtr)
import Foreign.LibFFI
import Foreign.C.Types
import Data.Int (Int8, Int16)
import Data.Word (Word16)
import Data.Coerce (coerce)

import ObjC.Runtime.Types
import ObjC.Runtime.MsgSend (sendMsg, sendClassMsg)
import ObjC.Runtime.Selector (mkSelector)
import ObjC.Runtime.Class (getRequiredClass)

import ObjC.Foundation.Internal.Classes
import ObjC.Foundation.Internal.Enums

-- | Initializes the receiver for encoding an archive, optionally disabling secure coding.
--
-- If @NSSecureCoding@ cannot be used, @requiresSecureCoding@ may be turned off here; for improved security, however, @requiresSecureCoding@ should be left enabled whenever possible. @requiresSecureCoding@ ensures that all encoded objects conform to @NSSecureCoding,@ preventing the possibility of encoding objects which cannot be decoded later.
--
-- To produce archives whose structure matches those previously encoded using @+archivedDataWithRootObject,@ encode the top-level object in your archive for the @NSKeyedArchiveRootObjectKey.@
--
-- ObjC selector: @- initRequiringSecureCoding:@
initRequiringSecureCoding :: IsNSKeyedArchiver nsKeyedArchiver => nsKeyedArchiver -> Bool -> IO (Id NSKeyedArchiver)
initRequiringSecureCoding nsKeyedArchiver  requiresSecureCoding =
    sendMsg nsKeyedArchiver (mkSelector "initRequiringSecureCoding:") (retPtr retVoid) [argCULong (if requiresSecureCoding then 1 else 0)] >>= ownedObject . castPtr

-- | Returns an @NSData@ object containing the encoded form of the object graph whose root object is given, optionally disabling secure coding.
--
-- If @NSSecureCoding@ cannot be used, @requiresSecureCoding@ may be turned off here; for improved security, however, @requiresSecureCoding@ should be left enabled whenever possible. @requiresSecureCoding@ ensures that all encoded objects conform to @NSSecureCoding,@ preventing the possibility of encoding objects which cannot be decoded later.
--
-- If the object graph cannot be encoded, returns @nil@ and sets the @error@ out parameter.
--
-- ObjC selector: @+ archivedDataWithRootObject:requiringSecureCoding:error:@
archivedDataWithRootObject_requiringSecureCoding_error :: IsNSError error_ => RawId -> Bool -> error_ -> IO (Id NSData)
archivedDataWithRootObject_requiringSecureCoding_error object requiresSecureCoding error_ =
  do
    cls' <- getRequiredClass "NSKeyedArchiver"
    withObjCPtr error_ $ \raw_error_ ->
      sendClassMsg cls' (mkSelector "archivedDataWithRootObject:requiringSecureCoding:error:") (retPtr retVoid) [argPtr (castPtr (unRawId object) :: Ptr ()), argCULong (if requiresSecureCoding then 1 else 0), argPtr (castPtr raw_error_ :: Ptr ())] >>= retainedObject . castPtr

-- | Initialize the archiver with empty data, ready for writing.
--
-- ObjC selector: @- init@
init_ :: IsNSKeyedArchiver nsKeyedArchiver => nsKeyedArchiver -> IO (Id NSKeyedArchiver)
init_ nsKeyedArchiver  =
    sendMsg nsKeyedArchiver (mkSelector "init") (retPtr retVoid) [] >>= ownedObject . castPtr

-- | @- initForWritingWithMutableData:@
initForWritingWithMutableData :: (IsNSKeyedArchiver nsKeyedArchiver, IsNSMutableData data_) => nsKeyedArchiver -> data_ -> IO (Id NSKeyedArchiver)
initForWritingWithMutableData nsKeyedArchiver  data_ =
  withObjCPtr data_ $ \raw_data_ ->
      sendMsg nsKeyedArchiver (mkSelector "initForWritingWithMutableData:") (retPtr retVoid) [argPtr (castPtr raw_data_ :: Ptr ())] >>= ownedObject . castPtr

-- | @+ archivedDataWithRootObject:@
archivedDataWithRootObject :: RawId -> IO (Id NSData)
archivedDataWithRootObject rootObject =
  do
    cls' <- getRequiredClass "NSKeyedArchiver"
    sendClassMsg cls' (mkSelector "archivedDataWithRootObject:") (retPtr retVoid) [argPtr (castPtr (unRawId rootObject) :: Ptr ())] >>= retainedObject . castPtr

-- | @+ archiveRootObject:toFile:@
archiveRootObject_toFile :: IsNSString path => RawId -> path -> IO Bool
archiveRootObject_toFile rootObject path =
  do
    cls' <- getRequiredClass "NSKeyedArchiver"
    withObjCPtr path $ \raw_path ->
      fmap ((/= 0) :: CULong -> Bool) $ sendClassMsg cls' (mkSelector "archiveRootObject:toFile:") retCULong [argPtr (castPtr (unRawId rootObject) :: Ptr ()), argPtr (castPtr raw_path :: Ptr ())]

-- | @- finishEncoding@
finishEncoding :: IsNSKeyedArchiver nsKeyedArchiver => nsKeyedArchiver -> IO ()
finishEncoding nsKeyedArchiver  =
    sendMsg nsKeyedArchiver (mkSelector "finishEncoding") retVoid []

-- | @+ setClassName:forClass:@
nsKeyedArchiverSetClassName_forClass :: IsNSString codedName => codedName -> Class -> IO ()
nsKeyedArchiverSetClassName_forClass codedName cls =
  do
    cls' <- getRequiredClass "NSKeyedArchiver"
    withObjCPtr codedName $ \raw_codedName ->
      sendClassMsg cls' (mkSelector "setClassName:forClass:") retVoid [argPtr (castPtr raw_codedName :: Ptr ()), argPtr (unClass cls)]

-- | @- setClassName:forClass:@
setClassName_forClass :: (IsNSKeyedArchiver nsKeyedArchiver, IsNSString codedName) => nsKeyedArchiver -> codedName -> Class -> IO ()
setClassName_forClass nsKeyedArchiver  codedName cls =
  withObjCPtr codedName $ \raw_codedName ->
      sendMsg nsKeyedArchiver (mkSelector "setClassName:forClass:") retVoid [argPtr (castPtr raw_codedName :: Ptr ()), argPtr (unClass cls)]

-- | @+ classNameForClass:@
nsKeyedArchiverClassNameForClass :: Class -> IO (Id NSString)
nsKeyedArchiverClassNameForClass cls =
  do
    cls' <- getRequiredClass "NSKeyedArchiver"
    sendClassMsg cls' (mkSelector "classNameForClass:") (retPtr retVoid) [argPtr (unClass cls)] >>= retainedObject . castPtr

-- | @- classNameForClass:@
classNameForClass :: IsNSKeyedArchiver nsKeyedArchiver => nsKeyedArchiver -> Class -> IO (Id NSString)
classNameForClass nsKeyedArchiver  cls =
    sendMsg nsKeyedArchiver (mkSelector "classNameForClass:") (retPtr retVoid) [argPtr (unClass cls)] >>= retainedObject . castPtr

-- | @- encodeObject:forKey:@
encodeObject_forKey :: (IsNSKeyedArchiver nsKeyedArchiver, IsNSString key) => nsKeyedArchiver -> RawId -> key -> IO ()
encodeObject_forKey nsKeyedArchiver  object key =
  withObjCPtr key $ \raw_key ->
      sendMsg nsKeyedArchiver (mkSelector "encodeObject:forKey:") retVoid [argPtr (castPtr (unRawId object) :: Ptr ()), argPtr (castPtr raw_key :: Ptr ())]

-- | @- encodeConditionalObject:forKey:@
encodeConditionalObject_forKey :: (IsNSKeyedArchiver nsKeyedArchiver, IsNSString key) => nsKeyedArchiver -> RawId -> key -> IO ()
encodeConditionalObject_forKey nsKeyedArchiver  object key =
  withObjCPtr key $ \raw_key ->
      sendMsg nsKeyedArchiver (mkSelector "encodeConditionalObject:forKey:") retVoid [argPtr (castPtr (unRawId object) :: Ptr ()), argPtr (castPtr raw_key :: Ptr ())]

-- | @- encodeBool:forKey:@
encodeBool_forKey :: (IsNSKeyedArchiver nsKeyedArchiver, IsNSString key) => nsKeyedArchiver -> Bool -> key -> IO ()
encodeBool_forKey nsKeyedArchiver  value key =
  withObjCPtr key $ \raw_key ->
      sendMsg nsKeyedArchiver (mkSelector "encodeBool:forKey:") retVoid [argCULong (if value then 1 else 0), argPtr (castPtr raw_key :: Ptr ())]

-- | @- encodeInt:forKey:@
encodeInt_forKey :: (IsNSKeyedArchiver nsKeyedArchiver, IsNSString key) => nsKeyedArchiver -> CInt -> key -> IO ()
encodeInt_forKey nsKeyedArchiver  value key =
  withObjCPtr key $ \raw_key ->
      sendMsg nsKeyedArchiver (mkSelector "encodeInt:forKey:") retVoid [argCInt value, argPtr (castPtr raw_key :: Ptr ())]

-- | @- encodeInt32:forKey:@
encodeInt32_forKey :: (IsNSKeyedArchiver nsKeyedArchiver, IsNSString key) => nsKeyedArchiver -> CInt -> key -> IO ()
encodeInt32_forKey nsKeyedArchiver  value key =
  withObjCPtr key $ \raw_key ->
      sendMsg nsKeyedArchiver (mkSelector "encodeInt32:forKey:") retVoid [argCInt value, argPtr (castPtr raw_key :: Ptr ())]

-- | @- encodeInt64:forKey:@
encodeInt64_forKey :: (IsNSKeyedArchiver nsKeyedArchiver, IsNSString key) => nsKeyedArchiver -> CLong -> key -> IO ()
encodeInt64_forKey nsKeyedArchiver  value key =
  withObjCPtr key $ \raw_key ->
      sendMsg nsKeyedArchiver (mkSelector "encodeInt64:forKey:") retVoid [argCLong value, argPtr (castPtr raw_key :: Ptr ())]

-- | @- encodeFloat:forKey:@
encodeFloat_forKey :: (IsNSKeyedArchiver nsKeyedArchiver, IsNSString key) => nsKeyedArchiver -> CFloat -> key -> IO ()
encodeFloat_forKey nsKeyedArchiver  value key =
  withObjCPtr key $ \raw_key ->
      sendMsg nsKeyedArchiver (mkSelector "encodeFloat:forKey:") retVoid [argCFloat value, argPtr (castPtr raw_key :: Ptr ())]

-- | @- encodeDouble:forKey:@
encodeDouble_forKey :: (IsNSKeyedArchiver nsKeyedArchiver, IsNSString key) => nsKeyedArchiver -> CDouble -> key -> IO ()
encodeDouble_forKey nsKeyedArchiver  value key =
  withObjCPtr key $ \raw_key ->
      sendMsg nsKeyedArchiver (mkSelector "encodeDouble:forKey:") retVoid [argCDouble value, argPtr (castPtr raw_key :: Ptr ())]

-- | @- encodeBytes:length:forKey:@
encodeBytes_length_forKey :: (IsNSKeyedArchiver nsKeyedArchiver, IsNSString key) => nsKeyedArchiver -> Const (Ptr CUChar) -> CULong -> key -> IO ()
encodeBytes_length_forKey nsKeyedArchiver  bytes length_ key =
  withObjCPtr key $ \raw_key ->
      sendMsg nsKeyedArchiver (mkSelector "encodeBytes:length:forKey:") retVoid [argPtr (unConst bytes), argCULong length_, argPtr (castPtr raw_key :: Ptr ())]

-- | @- delegate@
delegate :: IsNSKeyedArchiver nsKeyedArchiver => nsKeyedArchiver -> IO RawId
delegate nsKeyedArchiver  =
    fmap (RawId . castPtr) $ sendMsg nsKeyedArchiver (mkSelector "delegate") (retPtr retVoid) []

-- | @- setDelegate:@
setDelegate :: IsNSKeyedArchiver nsKeyedArchiver => nsKeyedArchiver -> RawId -> IO ()
setDelegate nsKeyedArchiver  value =
    sendMsg nsKeyedArchiver (mkSelector "setDelegate:") retVoid [argPtr (castPtr (unRawId value) :: Ptr ())]

-- | @- outputFormat@
outputFormat :: IsNSKeyedArchiver nsKeyedArchiver => nsKeyedArchiver -> IO NSPropertyListFormat
outputFormat nsKeyedArchiver  =
    fmap (coerce :: CULong -> NSPropertyListFormat) $ sendMsg nsKeyedArchiver (mkSelector "outputFormat") retCULong []

-- | @- setOutputFormat:@
setOutputFormat :: IsNSKeyedArchiver nsKeyedArchiver => nsKeyedArchiver -> NSPropertyListFormat -> IO ()
setOutputFormat nsKeyedArchiver  value =
    sendMsg nsKeyedArchiver (mkSelector "setOutputFormat:") retVoid [argCULong (coerce value)]

-- | If encoding has not yet finished, then invoking this property will call finishEncoding and return the data. If you initialized the keyed archiver with a specific mutable data instance, then it will be returned from this property after finishEncoding is called.
--
-- ObjC selector: @- encodedData@
encodedData :: IsNSKeyedArchiver nsKeyedArchiver => nsKeyedArchiver -> IO (Id NSData)
encodedData nsKeyedArchiver  =
    sendMsg nsKeyedArchiver (mkSelector "encodedData") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- requiresSecureCoding@
requiresSecureCoding :: IsNSKeyedArchiver nsKeyedArchiver => nsKeyedArchiver -> IO Bool
requiresSecureCoding nsKeyedArchiver  =
    fmap ((/= 0) :: CULong -> Bool) $ sendMsg nsKeyedArchiver (mkSelector "requiresSecureCoding") retCULong []

-- | @- setRequiresSecureCoding:@
setRequiresSecureCoding :: IsNSKeyedArchiver nsKeyedArchiver => nsKeyedArchiver -> Bool -> IO ()
setRequiresSecureCoding nsKeyedArchiver  value =
    sendMsg nsKeyedArchiver (mkSelector "setRequiresSecureCoding:") retVoid [argCULong (if value then 1 else 0)]

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @initRequiringSecureCoding:@
initRequiringSecureCodingSelector :: Selector
initRequiringSecureCodingSelector = mkSelector "initRequiringSecureCoding:"

-- | @Selector@ for @archivedDataWithRootObject:requiringSecureCoding:error:@
archivedDataWithRootObject_requiringSecureCoding_errorSelector :: Selector
archivedDataWithRootObject_requiringSecureCoding_errorSelector = mkSelector "archivedDataWithRootObject:requiringSecureCoding:error:"

-- | @Selector@ for @init@
initSelector :: Selector
initSelector = mkSelector "init"

-- | @Selector@ for @initForWritingWithMutableData:@
initForWritingWithMutableDataSelector :: Selector
initForWritingWithMutableDataSelector = mkSelector "initForWritingWithMutableData:"

-- | @Selector@ for @archivedDataWithRootObject:@
archivedDataWithRootObjectSelector :: Selector
archivedDataWithRootObjectSelector = mkSelector "archivedDataWithRootObject:"

-- | @Selector@ for @archiveRootObject:toFile:@
archiveRootObject_toFileSelector :: Selector
archiveRootObject_toFileSelector = mkSelector "archiveRootObject:toFile:"

-- | @Selector@ for @finishEncoding@
finishEncodingSelector :: Selector
finishEncodingSelector = mkSelector "finishEncoding"

-- | @Selector@ for @setClassName:forClass:@
setClassName_forClassSelector :: Selector
setClassName_forClassSelector = mkSelector "setClassName:forClass:"

-- | @Selector@ for @classNameForClass:@
classNameForClassSelector :: Selector
classNameForClassSelector = mkSelector "classNameForClass:"

-- | @Selector@ for @encodeObject:forKey:@
encodeObject_forKeySelector :: Selector
encodeObject_forKeySelector = mkSelector "encodeObject:forKey:"

-- | @Selector@ for @encodeConditionalObject:forKey:@
encodeConditionalObject_forKeySelector :: Selector
encodeConditionalObject_forKeySelector = mkSelector "encodeConditionalObject:forKey:"

-- | @Selector@ for @encodeBool:forKey:@
encodeBool_forKeySelector :: Selector
encodeBool_forKeySelector = mkSelector "encodeBool:forKey:"

-- | @Selector@ for @encodeInt:forKey:@
encodeInt_forKeySelector :: Selector
encodeInt_forKeySelector = mkSelector "encodeInt:forKey:"

-- | @Selector@ for @encodeInt32:forKey:@
encodeInt32_forKeySelector :: Selector
encodeInt32_forKeySelector = mkSelector "encodeInt32:forKey:"

-- | @Selector@ for @encodeInt64:forKey:@
encodeInt64_forKeySelector :: Selector
encodeInt64_forKeySelector = mkSelector "encodeInt64:forKey:"

-- | @Selector@ for @encodeFloat:forKey:@
encodeFloat_forKeySelector :: Selector
encodeFloat_forKeySelector = mkSelector "encodeFloat:forKey:"

-- | @Selector@ for @encodeDouble:forKey:@
encodeDouble_forKeySelector :: Selector
encodeDouble_forKeySelector = mkSelector "encodeDouble:forKey:"

-- | @Selector@ for @encodeBytes:length:forKey:@
encodeBytes_length_forKeySelector :: Selector
encodeBytes_length_forKeySelector = mkSelector "encodeBytes:length:forKey:"

-- | @Selector@ for @delegate@
delegateSelector :: Selector
delegateSelector = mkSelector "delegate"

-- | @Selector@ for @setDelegate:@
setDelegateSelector :: Selector
setDelegateSelector = mkSelector "setDelegate:"

-- | @Selector@ for @outputFormat@
outputFormatSelector :: Selector
outputFormatSelector = mkSelector "outputFormat"

-- | @Selector@ for @setOutputFormat:@
setOutputFormatSelector :: Selector
setOutputFormatSelector = mkSelector "setOutputFormat:"

-- | @Selector@ for @encodedData@
encodedDataSelector :: Selector
encodedDataSelector = mkSelector "encodedData"

-- | @Selector@ for @requiresSecureCoding@
requiresSecureCodingSelector :: Selector
requiresSecureCodingSelector = mkSelector "requiresSecureCoding"

-- | @Selector@ for @setRequiresSecureCoding:@
setRequiresSecureCodingSelector :: Selector
setRequiresSecureCodingSelector = mkSelector "setRequiresSecureCoding:"

