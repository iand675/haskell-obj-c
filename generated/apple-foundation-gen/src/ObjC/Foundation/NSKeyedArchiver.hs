{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE DataKinds #-}
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
  , archiveRootObject_toFileSelector
  , archivedDataWithRootObjectSelector
  , archivedDataWithRootObject_requiringSecureCoding_errorSelector
  , classNameForClassSelector
  , delegateSelector
  , encodeBool_forKeySelector
  , encodeBytes_length_forKeySelector
  , encodeConditionalObject_forKeySelector
  , encodeDouble_forKeySelector
  , encodeFloat_forKeySelector
  , encodeInt32_forKeySelector
  , encodeInt64_forKeySelector
  , encodeInt_forKeySelector
  , encodeObject_forKeySelector
  , encodedDataSelector
  , finishEncodingSelector
  , initForWritingWithMutableDataSelector
  , initRequiringSecureCodingSelector
  , initSelector
  , nsKeyedArchiverClassNameForClassSelector
  , nsKeyedArchiverSetClassName_forClassSelector
  , outputFormatSelector
  , requiresSecureCodingSelector
  , setClassName_forClassSelector
  , setDelegateSelector
  , setOutputFormatSelector
  , setRequiresSecureCodingSelector

  -- * Enum types
  , NSPropertyListFormat(NSPropertyListFormat)
  , pattern NSPropertyListOpenStepFormat
  , pattern NSPropertyListXMLFormat_v1_0
  , pattern NSPropertyListBinaryFormat_v1_0

  ) where

import Foreign.Ptr (Ptr, FunPtr)
import Foreign.C.Types

import ObjC.Runtime.Types
import ObjC.Runtime.Message (sendMessage, sendOwnedMessage, sendClassMessage, sendOwnedClassMessage)
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
initRequiringSecureCoding nsKeyedArchiver requiresSecureCoding =
  sendOwnedMessage nsKeyedArchiver initRequiringSecureCodingSelector requiresSecureCoding

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
    sendClassMessage cls' archivedDataWithRootObject_requiringSecureCoding_errorSelector object requiresSecureCoding (toNSError error_)

-- | Initialize the archiver with empty data, ready for writing.
--
-- ObjC selector: @- init@
init_ :: IsNSKeyedArchiver nsKeyedArchiver => nsKeyedArchiver -> IO (Id NSKeyedArchiver)
init_ nsKeyedArchiver =
  sendOwnedMessage nsKeyedArchiver initSelector

-- | @- initForWritingWithMutableData:@
initForWritingWithMutableData :: (IsNSKeyedArchiver nsKeyedArchiver, IsNSMutableData data_) => nsKeyedArchiver -> data_ -> IO (Id NSKeyedArchiver)
initForWritingWithMutableData nsKeyedArchiver data_ =
  sendOwnedMessage nsKeyedArchiver initForWritingWithMutableDataSelector (toNSMutableData data_)

-- | @+ archivedDataWithRootObject:@
archivedDataWithRootObject :: RawId -> IO (Id NSData)
archivedDataWithRootObject rootObject =
  do
    cls' <- getRequiredClass "NSKeyedArchiver"
    sendClassMessage cls' archivedDataWithRootObjectSelector rootObject

-- | @+ archiveRootObject:toFile:@
archiveRootObject_toFile :: IsNSString path => RawId -> path -> IO Bool
archiveRootObject_toFile rootObject path =
  do
    cls' <- getRequiredClass "NSKeyedArchiver"
    sendClassMessage cls' archiveRootObject_toFileSelector rootObject (toNSString path)

-- | @- finishEncoding@
finishEncoding :: IsNSKeyedArchiver nsKeyedArchiver => nsKeyedArchiver -> IO ()
finishEncoding nsKeyedArchiver =
  sendMessage nsKeyedArchiver finishEncodingSelector

-- | @+ setClassName:forClass:@
nsKeyedArchiverSetClassName_forClass :: IsNSString codedName => codedName -> Class -> IO ()
nsKeyedArchiverSetClassName_forClass codedName cls =
  do
    cls' <- getRequiredClass "NSKeyedArchiver"
    sendClassMessage cls' nsKeyedArchiverSetClassName_forClassSelector (toNSString codedName) cls

-- | @- setClassName:forClass:@
setClassName_forClass :: (IsNSKeyedArchiver nsKeyedArchiver, IsNSString codedName) => nsKeyedArchiver -> codedName -> Class -> IO ()
setClassName_forClass nsKeyedArchiver codedName cls =
  sendMessage nsKeyedArchiver setClassName_forClassSelector (toNSString codedName) cls

-- | @+ classNameForClass:@
nsKeyedArchiverClassNameForClass :: Class -> IO (Id NSString)
nsKeyedArchiverClassNameForClass cls =
  do
    cls' <- getRequiredClass "NSKeyedArchiver"
    sendClassMessage cls' nsKeyedArchiverClassNameForClassSelector cls

-- | @- classNameForClass:@
classNameForClass :: IsNSKeyedArchiver nsKeyedArchiver => nsKeyedArchiver -> Class -> IO (Id NSString)
classNameForClass nsKeyedArchiver cls =
  sendMessage nsKeyedArchiver classNameForClassSelector cls

-- | @- encodeObject:forKey:@
encodeObject_forKey :: (IsNSKeyedArchiver nsKeyedArchiver, IsNSString key) => nsKeyedArchiver -> RawId -> key -> IO ()
encodeObject_forKey nsKeyedArchiver object key =
  sendMessage nsKeyedArchiver encodeObject_forKeySelector object (toNSString key)

-- | @- encodeConditionalObject:forKey:@
encodeConditionalObject_forKey :: (IsNSKeyedArchiver nsKeyedArchiver, IsNSString key) => nsKeyedArchiver -> RawId -> key -> IO ()
encodeConditionalObject_forKey nsKeyedArchiver object key =
  sendMessage nsKeyedArchiver encodeConditionalObject_forKeySelector object (toNSString key)

-- | @- encodeBool:forKey:@
encodeBool_forKey :: (IsNSKeyedArchiver nsKeyedArchiver, IsNSString key) => nsKeyedArchiver -> Bool -> key -> IO ()
encodeBool_forKey nsKeyedArchiver value key =
  sendMessage nsKeyedArchiver encodeBool_forKeySelector value (toNSString key)

-- | @- encodeInt:forKey:@
encodeInt_forKey :: (IsNSKeyedArchiver nsKeyedArchiver, IsNSString key) => nsKeyedArchiver -> CInt -> key -> IO ()
encodeInt_forKey nsKeyedArchiver value key =
  sendMessage nsKeyedArchiver encodeInt_forKeySelector value (toNSString key)

-- | @- encodeInt32:forKey:@
encodeInt32_forKey :: (IsNSKeyedArchiver nsKeyedArchiver, IsNSString key) => nsKeyedArchiver -> CInt -> key -> IO ()
encodeInt32_forKey nsKeyedArchiver value key =
  sendMessage nsKeyedArchiver encodeInt32_forKeySelector value (toNSString key)

-- | @- encodeInt64:forKey:@
encodeInt64_forKey :: (IsNSKeyedArchiver nsKeyedArchiver, IsNSString key) => nsKeyedArchiver -> CLong -> key -> IO ()
encodeInt64_forKey nsKeyedArchiver value key =
  sendMessage nsKeyedArchiver encodeInt64_forKeySelector value (toNSString key)

-- | @- encodeFloat:forKey:@
encodeFloat_forKey :: (IsNSKeyedArchiver nsKeyedArchiver, IsNSString key) => nsKeyedArchiver -> CFloat -> key -> IO ()
encodeFloat_forKey nsKeyedArchiver value key =
  sendMessage nsKeyedArchiver encodeFloat_forKeySelector value (toNSString key)

-- | @- encodeDouble:forKey:@
encodeDouble_forKey :: (IsNSKeyedArchiver nsKeyedArchiver, IsNSString key) => nsKeyedArchiver -> CDouble -> key -> IO ()
encodeDouble_forKey nsKeyedArchiver value key =
  sendMessage nsKeyedArchiver encodeDouble_forKeySelector value (toNSString key)

-- | @- encodeBytes:length:forKey:@
encodeBytes_length_forKey :: (IsNSKeyedArchiver nsKeyedArchiver, IsNSString key) => nsKeyedArchiver -> Const (Ptr CUChar) -> CULong -> key -> IO ()
encodeBytes_length_forKey nsKeyedArchiver bytes length_ key =
  sendMessage nsKeyedArchiver encodeBytes_length_forKeySelector bytes length_ (toNSString key)

-- | @- delegate@
delegate :: IsNSKeyedArchiver nsKeyedArchiver => nsKeyedArchiver -> IO RawId
delegate nsKeyedArchiver =
  sendMessage nsKeyedArchiver delegateSelector

-- | @- setDelegate:@
setDelegate :: IsNSKeyedArchiver nsKeyedArchiver => nsKeyedArchiver -> RawId -> IO ()
setDelegate nsKeyedArchiver value =
  sendMessage nsKeyedArchiver setDelegateSelector value

-- | @- outputFormat@
outputFormat :: IsNSKeyedArchiver nsKeyedArchiver => nsKeyedArchiver -> IO NSPropertyListFormat
outputFormat nsKeyedArchiver =
  sendMessage nsKeyedArchiver outputFormatSelector

-- | @- setOutputFormat:@
setOutputFormat :: IsNSKeyedArchiver nsKeyedArchiver => nsKeyedArchiver -> NSPropertyListFormat -> IO ()
setOutputFormat nsKeyedArchiver value =
  sendMessage nsKeyedArchiver setOutputFormatSelector value

-- | If encoding has not yet finished, then invoking this property will call finishEncoding and return the data. If you initialized the keyed archiver with a specific mutable data instance, then it will be returned from this property after finishEncoding is called.
--
-- ObjC selector: @- encodedData@
encodedData :: IsNSKeyedArchiver nsKeyedArchiver => nsKeyedArchiver -> IO (Id NSData)
encodedData nsKeyedArchiver =
  sendMessage nsKeyedArchiver encodedDataSelector

-- | @- requiresSecureCoding@
requiresSecureCoding :: IsNSKeyedArchiver nsKeyedArchiver => nsKeyedArchiver -> IO Bool
requiresSecureCoding nsKeyedArchiver =
  sendMessage nsKeyedArchiver requiresSecureCodingSelector

-- | @- setRequiresSecureCoding:@
setRequiresSecureCoding :: IsNSKeyedArchiver nsKeyedArchiver => nsKeyedArchiver -> Bool -> IO ()
setRequiresSecureCoding nsKeyedArchiver value =
  sendMessage nsKeyedArchiver setRequiresSecureCodingSelector value

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @initRequiringSecureCoding:@
initRequiringSecureCodingSelector :: Selector '[Bool] (Id NSKeyedArchiver)
initRequiringSecureCodingSelector = mkSelector "initRequiringSecureCoding:"

-- | @Selector@ for @archivedDataWithRootObject:requiringSecureCoding:error:@
archivedDataWithRootObject_requiringSecureCoding_errorSelector :: Selector '[RawId, Bool, Id NSError] (Id NSData)
archivedDataWithRootObject_requiringSecureCoding_errorSelector = mkSelector "archivedDataWithRootObject:requiringSecureCoding:error:"

-- | @Selector@ for @init@
initSelector :: Selector '[] (Id NSKeyedArchiver)
initSelector = mkSelector "init"

-- | @Selector@ for @initForWritingWithMutableData:@
initForWritingWithMutableDataSelector :: Selector '[Id NSMutableData] (Id NSKeyedArchiver)
initForWritingWithMutableDataSelector = mkSelector "initForWritingWithMutableData:"

-- | @Selector@ for @archivedDataWithRootObject:@
archivedDataWithRootObjectSelector :: Selector '[RawId] (Id NSData)
archivedDataWithRootObjectSelector = mkSelector "archivedDataWithRootObject:"

-- | @Selector@ for @archiveRootObject:toFile:@
archiveRootObject_toFileSelector :: Selector '[RawId, Id NSString] Bool
archiveRootObject_toFileSelector = mkSelector "archiveRootObject:toFile:"

-- | @Selector@ for @finishEncoding@
finishEncodingSelector :: Selector '[] ()
finishEncodingSelector = mkSelector "finishEncoding"

-- | @Selector@ for @setClassName:forClass:@
nsKeyedArchiverSetClassName_forClassSelector :: Selector '[Id NSString, Class] ()
nsKeyedArchiverSetClassName_forClassSelector = mkSelector "setClassName:forClass:"

-- | @Selector@ for @setClassName:forClass:@
setClassName_forClassSelector :: Selector '[Id NSString, Class] ()
setClassName_forClassSelector = mkSelector "setClassName:forClass:"

-- | @Selector@ for @classNameForClass:@
nsKeyedArchiverClassNameForClassSelector :: Selector '[Class] (Id NSString)
nsKeyedArchiverClassNameForClassSelector = mkSelector "classNameForClass:"

-- | @Selector@ for @classNameForClass:@
classNameForClassSelector :: Selector '[Class] (Id NSString)
classNameForClassSelector = mkSelector "classNameForClass:"

-- | @Selector@ for @encodeObject:forKey:@
encodeObject_forKeySelector :: Selector '[RawId, Id NSString] ()
encodeObject_forKeySelector = mkSelector "encodeObject:forKey:"

-- | @Selector@ for @encodeConditionalObject:forKey:@
encodeConditionalObject_forKeySelector :: Selector '[RawId, Id NSString] ()
encodeConditionalObject_forKeySelector = mkSelector "encodeConditionalObject:forKey:"

-- | @Selector@ for @encodeBool:forKey:@
encodeBool_forKeySelector :: Selector '[Bool, Id NSString] ()
encodeBool_forKeySelector = mkSelector "encodeBool:forKey:"

-- | @Selector@ for @encodeInt:forKey:@
encodeInt_forKeySelector :: Selector '[CInt, Id NSString] ()
encodeInt_forKeySelector = mkSelector "encodeInt:forKey:"

-- | @Selector@ for @encodeInt32:forKey:@
encodeInt32_forKeySelector :: Selector '[CInt, Id NSString] ()
encodeInt32_forKeySelector = mkSelector "encodeInt32:forKey:"

-- | @Selector@ for @encodeInt64:forKey:@
encodeInt64_forKeySelector :: Selector '[CLong, Id NSString] ()
encodeInt64_forKeySelector = mkSelector "encodeInt64:forKey:"

-- | @Selector@ for @encodeFloat:forKey:@
encodeFloat_forKeySelector :: Selector '[CFloat, Id NSString] ()
encodeFloat_forKeySelector = mkSelector "encodeFloat:forKey:"

-- | @Selector@ for @encodeDouble:forKey:@
encodeDouble_forKeySelector :: Selector '[CDouble, Id NSString] ()
encodeDouble_forKeySelector = mkSelector "encodeDouble:forKey:"

-- | @Selector@ for @encodeBytes:length:forKey:@
encodeBytes_length_forKeySelector :: Selector '[Const (Ptr CUChar), CULong, Id NSString] ()
encodeBytes_length_forKeySelector = mkSelector "encodeBytes:length:forKey:"

-- | @Selector@ for @delegate@
delegateSelector :: Selector '[] RawId
delegateSelector = mkSelector "delegate"

-- | @Selector@ for @setDelegate:@
setDelegateSelector :: Selector '[RawId] ()
setDelegateSelector = mkSelector "setDelegate:"

-- | @Selector@ for @outputFormat@
outputFormatSelector :: Selector '[] NSPropertyListFormat
outputFormatSelector = mkSelector "outputFormat"

-- | @Selector@ for @setOutputFormat:@
setOutputFormatSelector :: Selector '[NSPropertyListFormat] ()
setOutputFormatSelector = mkSelector "setOutputFormat:"

-- | @Selector@ for @encodedData@
encodedDataSelector :: Selector '[] (Id NSData)
encodedDataSelector = mkSelector "encodedData"

-- | @Selector@ for @requiresSecureCoding@
requiresSecureCodingSelector :: Selector '[] Bool
requiresSecureCodingSelector = mkSelector "requiresSecureCoding"

-- | @Selector@ for @setRequiresSecureCoding:@
setRequiresSecureCodingSelector :: Selector '[Bool] ()
setRequiresSecureCodingSelector = mkSelector "setRequiresSecureCoding:"

