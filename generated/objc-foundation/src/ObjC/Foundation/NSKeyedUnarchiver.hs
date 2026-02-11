{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Generated bindings for @NSKeyedUnarchiver@.
module ObjC.Foundation.NSKeyedUnarchiver
  ( NSKeyedUnarchiver
  , IsNSKeyedUnarchiver(..)
  , initForReadingFromData_error
  , unarchivedObjectOfClass_fromData_error
  , unarchivedArrayOfObjectsOfClass_fromData_error
  , unarchivedDictionaryWithKeysOfClass_objectsOfClass_fromData_error
  , unarchivedObjectOfClasses_fromData_error
  , unarchivedArrayOfObjectsOfClasses_fromData_error
  , unarchivedDictionaryWithKeysOfClasses_objectsOfClasses_fromData_error
  , init_
  , initForReadingWithData
  , unarchiveObjectWithData
  , unarchiveTopLevelObjectWithData_error
  , unarchiveObjectWithFile
  , finishDecoding
  , nsKeyedUnarchiverSetClass_forClassName
  , setClass_forClassName
  , nsKeyedUnarchiverClassForClassName
  , classForClassName
  , containsValueForKey
  , decodeObjectForKey
  , decodeBoolForKey
  , decodeIntForKey
  , decodeInt32ForKey
  , decodeInt64ForKey
  , decodeFloatForKey
  , decodeDoubleForKey
  , decodeBytesForKey_returnedLength
  , requiresSecureCoding
  , setRequiresSecureCoding
  , decodingFailurePolicy
  , setDecodingFailurePolicy
  , initForReadingFromData_errorSelector
  , unarchivedObjectOfClass_fromData_errorSelector
  , unarchivedArrayOfObjectsOfClass_fromData_errorSelector
  , unarchivedDictionaryWithKeysOfClass_objectsOfClass_fromData_errorSelector
  , unarchivedObjectOfClasses_fromData_errorSelector
  , unarchivedArrayOfObjectsOfClasses_fromData_errorSelector
  , unarchivedDictionaryWithKeysOfClasses_objectsOfClasses_fromData_errorSelector
  , initSelector
  , initForReadingWithDataSelector
  , unarchiveObjectWithDataSelector
  , unarchiveTopLevelObjectWithData_errorSelector
  , unarchiveObjectWithFileSelector
  , finishDecodingSelector
  , setClass_forClassNameSelector
  , classForClassNameSelector
  , containsValueForKeySelector
  , decodeObjectForKeySelector
  , decodeBoolForKeySelector
  , decodeIntForKeySelector
  , decodeInt32ForKeySelector
  , decodeInt64ForKeySelector
  , decodeFloatForKeySelector
  , decodeDoubleForKeySelector
  , decodeBytesForKey_returnedLengthSelector
  , requiresSecureCodingSelector
  , setRequiresSecureCodingSelector
  , decodingFailurePolicySelector
  , setDecodingFailurePolicySelector

  -- * Enum types
  , NSDecodingFailurePolicy(NSDecodingFailurePolicy)
  , pattern NSDecodingFailurePolicyRaiseException
  , pattern NSDecodingFailurePolicySetErrorAndReturn

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

-- | Initializes the receiver for decoding an archive previously encoded by @NSKeyedUnarchiver.@
--
-- Enables @requiresSecureCoding@ by default. If @NSSecureCoding@ cannot be used, @requiresSecureCoding@ may be turned off manually; for improved security, @requiresSecureCoding@ should be left enabled whenever possible.
--
-- Sets the unarchiver's @decodingFailurePolicy@ to @NSDecodingFailurePolicySetErrorAndReturn.@
--
-- Returns @nil@ if the given data is not valid, and sets the @error@ out parameter.
--
-- ObjC selector: @- initForReadingFromData:error:@
initForReadingFromData_error :: (IsNSKeyedUnarchiver nsKeyedUnarchiver, IsNSData data_, IsNSError error_) => nsKeyedUnarchiver -> data_ -> error_ -> IO (Id NSKeyedUnarchiver)
initForReadingFromData_error nsKeyedUnarchiver  data_ error_ =
withObjCPtr data_ $ \raw_data_ ->
  withObjCPtr error_ $ \raw_error_ ->
      sendMsg nsKeyedUnarchiver (mkSelector "initForReadingFromData:error:") (retPtr retVoid) [argPtr (castPtr raw_data_ :: Ptr ()), argPtr (castPtr raw_error_ :: Ptr ())] >>= ownedObject . castPtr

-- | Decodes the root object of the given class from the given archive, previously encoded by @NSKeyedArchiver.@
--
-- Enables @requiresSecureCoding@ and sets the @decodingFailurePolicy@ to @NSDecodingFailurePolicySetErrorAndReturn.@
--
-- Returns @nil@ if the given data is not valid or cannot be decoded, and sets the @error@ out parameter.
--
-- ObjC selector: @+ unarchivedObjectOfClass:fromData:error:@
unarchivedObjectOfClass_fromData_error :: (IsNSData data_, IsNSError error_) => Class -> data_ -> error_ -> IO RawId
unarchivedObjectOfClass_fromData_error cls data_ error_ =
  do
    cls' <- getRequiredClass "NSKeyedUnarchiver"
    withObjCPtr data_ $ \raw_data_ ->
      withObjCPtr error_ $ \raw_error_ ->
        fmap (RawId . castPtr) $ sendClassMsg cls' (mkSelector "unarchivedObjectOfClass:fromData:error:") (retPtr retVoid) [argPtr (unClass cls), argPtr (castPtr raw_data_ :: Ptr ()), argPtr (castPtr raw_error_ :: Ptr ())]

-- | Decodes the @NSArray@ root object from @data@ which should be an @NSArray<cls>@ containing the given non-collection class (no nested arrays or arrays of dictionaries, etc) from the given archive, previously encoded by @NSKeyedArchiver.@  Enables @requiresSecureCoding@ and sets the @decodingFailurePolicy@ to @NSDecodingFailurePolicySetErrorAndReturn.@
--
-- Returns @nil@ if the given data is not valid or cannot be decoded, and sets the @error@ out parameter.
--
-- ObjC selector: @+ unarchivedArrayOfObjectsOfClass:fromData:error:@
unarchivedArrayOfObjectsOfClass_fromData_error :: (IsNSData data_, IsNSError error_) => Class -> data_ -> error_ -> IO (Id NSArray)
unarchivedArrayOfObjectsOfClass_fromData_error cls data_ error_ =
  do
    cls' <- getRequiredClass "NSKeyedUnarchiver"
    withObjCPtr data_ $ \raw_data_ ->
      withObjCPtr error_ $ \raw_error_ ->
        sendClassMsg cls' (mkSelector "unarchivedArrayOfObjectsOfClass:fromData:error:") (retPtr retVoid) [argPtr (unClass cls), argPtr (castPtr raw_data_ :: Ptr ()), argPtr (castPtr raw_error_ :: Ptr ())] >>= retainedObject . castPtr

-- | Decodes the @NSDictionary@ root object from @data@ which should be an @NSDictionary<keyCls,objectCls>@  with keys of type given in @keyCls@ and objects of the given non-collection class @objectCls@ (no nested dictionaries or other dictionaries contained in the dictionary, etc) from the given archive, previously encoded by @NSKeyedArchiver.@
--
-- Enables @requiresSecureCoding@ and sets the @decodingFailurePolicy@ to @NSDecodingFailurePolicySetErrorAndReturn.@
--
-- Returns @nil@ if the given data is not valid or cannot be decoded, and sets the @error@ out parameter.
--
-- ObjC selector: @+ unarchivedDictionaryWithKeysOfClass:objectsOfClass:fromData:error:@
unarchivedDictionaryWithKeysOfClass_objectsOfClass_fromData_error :: (IsNSData data_, IsNSError error_) => Class -> Class -> data_ -> error_ -> IO (Id NSDictionary)
unarchivedDictionaryWithKeysOfClass_objectsOfClass_fromData_error keyCls valueCls data_ error_ =
  do
    cls' <- getRequiredClass "NSKeyedUnarchiver"
    withObjCPtr data_ $ \raw_data_ ->
      withObjCPtr error_ $ \raw_error_ ->
        sendClassMsg cls' (mkSelector "unarchivedDictionaryWithKeysOfClass:objectsOfClass:fromData:error:") (retPtr retVoid) [argPtr (unClass keyCls), argPtr (unClass valueCls), argPtr (castPtr raw_data_ :: Ptr ()), argPtr (castPtr raw_error_ :: Ptr ())] >>= retainedObject . castPtr

-- | Decodes the root object of one of the given classes from the given archive, previously encoded by @NSKeyedArchiver.@
--
-- Enables @requiresSecureCoding@ and sets the @decodingFailurePolicy@ to @NSDecodingFailurePolicySetErrorAndReturn.@
--
-- Returns @nil@ if the given data is not valid or cannot be decoded, and sets the @error@ out parameter.
--
-- ObjC selector: @+ unarchivedObjectOfClasses:fromData:error:@
unarchivedObjectOfClasses_fromData_error :: (IsNSSet classes, IsNSData data_, IsNSError error_) => classes -> data_ -> error_ -> IO RawId
unarchivedObjectOfClasses_fromData_error classes data_ error_ =
  do
    cls' <- getRequiredClass "NSKeyedUnarchiver"
    withObjCPtr classes $ \raw_classes ->
      withObjCPtr data_ $ \raw_data_ ->
        withObjCPtr error_ $ \raw_error_ ->
          fmap (RawId . castPtr) $ sendClassMsg cls' (mkSelector "unarchivedObjectOfClasses:fromData:error:") (retPtr retVoid) [argPtr (castPtr raw_classes :: Ptr ()), argPtr (castPtr raw_data_ :: Ptr ()), argPtr (castPtr raw_error_ :: Ptr ())]

-- | Decodes the @NSArray@ root object from @data@ which should be an @NSArray,@ containing the given non-collection classes in @classes@  (no nested arrays or arrays of dictionaries, etc) from the given archive, previously encoded by @NSKeyedArchiver.@
--
-- Enables @requiresSecureCoding@ and sets the @decodingFailurePolicy@ to @NSDecodingFailurePolicySetErrorAndReturn.@
--
-- Returns @nil@ if the given data is not valid or cannot be decoded, and sets the @error@ out parameter.
--
-- ObjC selector: @+ unarchivedArrayOfObjectsOfClasses:fromData:error:@
unarchivedArrayOfObjectsOfClasses_fromData_error :: (IsNSSet classes, IsNSData data_, IsNSError error_) => classes -> data_ -> error_ -> IO (Id NSArray)
unarchivedArrayOfObjectsOfClasses_fromData_error classes data_ error_ =
  do
    cls' <- getRequiredClass "NSKeyedUnarchiver"
    withObjCPtr classes $ \raw_classes ->
      withObjCPtr data_ $ \raw_data_ ->
        withObjCPtr error_ $ \raw_error_ ->
          sendClassMsg cls' (mkSelector "unarchivedArrayOfObjectsOfClasses:fromData:error:") (retPtr retVoid) [argPtr (castPtr raw_classes :: Ptr ()), argPtr (castPtr raw_data_ :: Ptr ()), argPtr (castPtr raw_error_ :: Ptr ())] >>= retainedObject . castPtr

-- | Decodes the @NSDictionary@ root object from @data@ which should be an @NSDictionary,@ with keys of the types given in @keyClasses@ and objects of the given non-collection classes in @objectClasses@ (no nested dictionaries or other dictionaries contained in the dictionary, etc) from the given archive, previously encoded by @NSKeyedArchiver.@
--
-- Enables @requiresSecureCoding@ and sets the @decodingFailurePolicy@ to @NSDecodingFailurePolicySetErrorAndReturn.@
--
-- Returns @nil@ if the given data is not valid or cannot be decoded, and sets the @error@ out parameter.
--
-- ObjC selector: @+ unarchivedDictionaryWithKeysOfClasses:objectsOfClasses:fromData:error:@
unarchivedDictionaryWithKeysOfClasses_objectsOfClasses_fromData_error :: (IsNSSet keyClasses, IsNSSet valueClasses, IsNSData data_, IsNSError error_) => keyClasses -> valueClasses -> data_ -> error_ -> IO (Id NSDictionary)
unarchivedDictionaryWithKeysOfClasses_objectsOfClasses_fromData_error keyClasses valueClasses data_ error_ =
  do
    cls' <- getRequiredClass "NSKeyedUnarchiver"
    withObjCPtr keyClasses $ \raw_keyClasses ->
      withObjCPtr valueClasses $ \raw_valueClasses ->
        withObjCPtr data_ $ \raw_data_ ->
          withObjCPtr error_ $ \raw_error_ ->
            sendClassMsg cls' (mkSelector "unarchivedDictionaryWithKeysOfClasses:objectsOfClasses:fromData:error:") (retPtr retVoid) [argPtr (castPtr raw_keyClasses :: Ptr ()), argPtr (castPtr raw_valueClasses :: Ptr ()), argPtr (castPtr raw_data_ :: Ptr ()), argPtr (castPtr raw_error_ :: Ptr ())] >>= retainedObject . castPtr

-- | @- init@
init_ :: IsNSKeyedUnarchiver nsKeyedUnarchiver => nsKeyedUnarchiver -> IO (Id NSKeyedUnarchiver)
init_ nsKeyedUnarchiver  =
  sendMsg nsKeyedUnarchiver (mkSelector "init") (retPtr retVoid) [] >>= ownedObject . castPtr

-- | @- initForReadingWithData:@
initForReadingWithData :: (IsNSKeyedUnarchiver nsKeyedUnarchiver, IsNSData data_) => nsKeyedUnarchiver -> data_ -> IO (Id NSKeyedUnarchiver)
initForReadingWithData nsKeyedUnarchiver  data_ =
withObjCPtr data_ $ \raw_data_ ->
    sendMsg nsKeyedUnarchiver (mkSelector "initForReadingWithData:") (retPtr retVoid) [argPtr (castPtr raw_data_ :: Ptr ())] >>= ownedObject . castPtr

-- | @+ unarchiveObjectWithData:@
unarchiveObjectWithData :: IsNSData data_ => data_ -> IO RawId
unarchiveObjectWithData data_ =
  do
    cls' <- getRequiredClass "NSKeyedUnarchiver"
    withObjCPtr data_ $ \raw_data_ ->
      fmap (RawId . castPtr) $ sendClassMsg cls' (mkSelector "unarchiveObjectWithData:") (retPtr retVoid) [argPtr (castPtr raw_data_ :: Ptr ())]

-- | @+ unarchiveTopLevelObjectWithData:error:@
unarchiveTopLevelObjectWithData_error :: (IsNSData data_, IsNSError error_) => data_ -> error_ -> IO RawId
unarchiveTopLevelObjectWithData_error data_ error_ =
  do
    cls' <- getRequiredClass "NSKeyedUnarchiver"
    withObjCPtr data_ $ \raw_data_ ->
      withObjCPtr error_ $ \raw_error_ ->
        fmap (RawId . castPtr) $ sendClassMsg cls' (mkSelector "unarchiveTopLevelObjectWithData:error:") (retPtr retVoid) [argPtr (castPtr raw_data_ :: Ptr ()), argPtr (castPtr raw_error_ :: Ptr ())]

-- | @+ unarchiveObjectWithFile:@
unarchiveObjectWithFile :: IsNSString path => path -> IO RawId
unarchiveObjectWithFile path =
  do
    cls' <- getRequiredClass "NSKeyedUnarchiver"
    withObjCPtr path $ \raw_path ->
      fmap (RawId . castPtr) $ sendClassMsg cls' (mkSelector "unarchiveObjectWithFile:") (retPtr retVoid) [argPtr (castPtr raw_path :: Ptr ())]

-- | @- finishDecoding@
finishDecoding :: IsNSKeyedUnarchiver nsKeyedUnarchiver => nsKeyedUnarchiver -> IO ()
finishDecoding nsKeyedUnarchiver  =
  sendMsg nsKeyedUnarchiver (mkSelector "finishDecoding") retVoid []

-- | @+ setClass:forClassName:@
nsKeyedUnarchiverSetClass_forClassName :: IsNSString codedName => Class -> codedName -> IO ()
nsKeyedUnarchiverSetClass_forClassName cls codedName =
  do
    cls' <- getRequiredClass "NSKeyedUnarchiver"
    withObjCPtr codedName $ \raw_codedName ->
      sendClassMsg cls' (mkSelector "setClass:forClassName:") retVoid [argPtr (unClass cls), argPtr (castPtr raw_codedName :: Ptr ())]

-- | @- setClass:forClassName:@
setClass_forClassName :: (IsNSKeyedUnarchiver nsKeyedUnarchiver, IsNSString codedName) => nsKeyedUnarchiver -> Class -> codedName -> IO ()
setClass_forClassName nsKeyedUnarchiver  cls codedName =
withObjCPtr codedName $ \raw_codedName ->
    sendMsg nsKeyedUnarchiver (mkSelector "setClass:forClassName:") retVoid [argPtr (unClass cls), argPtr (castPtr raw_codedName :: Ptr ())]

-- | @+ classForClassName:@
nsKeyedUnarchiverClassForClassName :: IsNSString codedName => codedName -> IO Class
nsKeyedUnarchiverClassForClassName codedName =
  do
    cls' <- getRequiredClass "NSKeyedUnarchiver"
    withObjCPtr codedName $ \raw_codedName ->
      fmap (Class . castPtr) $ sendClassMsg cls' (mkSelector "classForClassName:") (retPtr retVoid) [argPtr (castPtr raw_codedName :: Ptr ())]

-- | @- classForClassName:@
classForClassName :: (IsNSKeyedUnarchiver nsKeyedUnarchiver, IsNSString codedName) => nsKeyedUnarchiver -> codedName -> IO Class
classForClassName nsKeyedUnarchiver  codedName =
withObjCPtr codedName $ \raw_codedName ->
    fmap (Class . castPtr) $ sendMsg nsKeyedUnarchiver (mkSelector "classForClassName:") (retPtr retVoid) [argPtr (castPtr raw_codedName :: Ptr ())]

-- | @- containsValueForKey:@
containsValueForKey :: (IsNSKeyedUnarchiver nsKeyedUnarchiver, IsNSString key) => nsKeyedUnarchiver -> key -> IO Bool
containsValueForKey nsKeyedUnarchiver  key =
withObjCPtr key $ \raw_key ->
    fmap ((/= 0) :: CULong -> Bool) $ sendMsg nsKeyedUnarchiver (mkSelector "containsValueForKey:") retCULong [argPtr (castPtr raw_key :: Ptr ())]

-- | @- decodeObjectForKey:@
decodeObjectForKey :: (IsNSKeyedUnarchiver nsKeyedUnarchiver, IsNSString key) => nsKeyedUnarchiver -> key -> IO RawId
decodeObjectForKey nsKeyedUnarchiver  key =
withObjCPtr key $ \raw_key ->
    fmap (RawId . castPtr) $ sendMsg nsKeyedUnarchiver (mkSelector "decodeObjectForKey:") (retPtr retVoid) [argPtr (castPtr raw_key :: Ptr ())]

-- | @- decodeBoolForKey:@
decodeBoolForKey :: (IsNSKeyedUnarchiver nsKeyedUnarchiver, IsNSString key) => nsKeyedUnarchiver -> key -> IO Bool
decodeBoolForKey nsKeyedUnarchiver  key =
withObjCPtr key $ \raw_key ->
    fmap ((/= 0) :: CULong -> Bool) $ sendMsg nsKeyedUnarchiver (mkSelector "decodeBoolForKey:") retCULong [argPtr (castPtr raw_key :: Ptr ())]

-- | @- decodeIntForKey:@
decodeIntForKey :: (IsNSKeyedUnarchiver nsKeyedUnarchiver, IsNSString key) => nsKeyedUnarchiver -> key -> IO CInt
decodeIntForKey nsKeyedUnarchiver  key =
withObjCPtr key $ \raw_key ->
    sendMsg nsKeyedUnarchiver (mkSelector "decodeIntForKey:") retCInt [argPtr (castPtr raw_key :: Ptr ())]

-- | @- decodeInt32ForKey:@
decodeInt32ForKey :: (IsNSKeyedUnarchiver nsKeyedUnarchiver, IsNSString key) => nsKeyedUnarchiver -> key -> IO CInt
decodeInt32ForKey nsKeyedUnarchiver  key =
withObjCPtr key $ \raw_key ->
    sendMsg nsKeyedUnarchiver (mkSelector "decodeInt32ForKey:") retCInt [argPtr (castPtr raw_key :: Ptr ())]

-- | @- decodeInt64ForKey:@
decodeInt64ForKey :: (IsNSKeyedUnarchiver nsKeyedUnarchiver, IsNSString key) => nsKeyedUnarchiver -> key -> IO CLong
decodeInt64ForKey nsKeyedUnarchiver  key =
withObjCPtr key $ \raw_key ->
    sendMsg nsKeyedUnarchiver (mkSelector "decodeInt64ForKey:") retCLong [argPtr (castPtr raw_key :: Ptr ())]

-- | @- decodeFloatForKey:@
decodeFloatForKey :: (IsNSKeyedUnarchiver nsKeyedUnarchiver, IsNSString key) => nsKeyedUnarchiver -> key -> IO CFloat
decodeFloatForKey nsKeyedUnarchiver  key =
withObjCPtr key $ \raw_key ->
    sendMsg nsKeyedUnarchiver (mkSelector "decodeFloatForKey:") retCFloat [argPtr (castPtr raw_key :: Ptr ())]

-- | @- decodeDoubleForKey:@
decodeDoubleForKey :: (IsNSKeyedUnarchiver nsKeyedUnarchiver, IsNSString key) => nsKeyedUnarchiver -> key -> IO CDouble
decodeDoubleForKey nsKeyedUnarchiver  key =
withObjCPtr key $ \raw_key ->
    sendMsg nsKeyedUnarchiver (mkSelector "decodeDoubleForKey:") retCDouble [argPtr (castPtr raw_key :: Ptr ())]

-- | @- decodeBytesForKey:returnedLength:@
decodeBytesForKey_returnedLength :: (IsNSKeyedUnarchiver nsKeyedUnarchiver, IsNSString key) => nsKeyedUnarchiver -> key -> Ptr CULong -> IO (Const (Ptr CUChar))
decodeBytesForKey_returnedLength nsKeyedUnarchiver  key lengthp =
withObjCPtr key $ \raw_key ->
    fmap Const $ fmap castPtr $ sendMsg nsKeyedUnarchiver (mkSelector "decodeBytesForKey:returnedLength:") (retPtr retVoid) [argPtr (castPtr raw_key :: Ptr ()), argPtr lengthp]

-- | @- requiresSecureCoding@
requiresSecureCoding :: IsNSKeyedUnarchiver nsKeyedUnarchiver => nsKeyedUnarchiver -> IO Bool
requiresSecureCoding nsKeyedUnarchiver  =
  fmap ((/= 0) :: CULong -> Bool) $ sendMsg nsKeyedUnarchiver (mkSelector "requiresSecureCoding") retCULong []

-- | @- setRequiresSecureCoding:@
setRequiresSecureCoding :: IsNSKeyedUnarchiver nsKeyedUnarchiver => nsKeyedUnarchiver -> Bool -> IO ()
setRequiresSecureCoding nsKeyedUnarchiver  value =
  sendMsg nsKeyedUnarchiver (mkSelector "setRequiresSecureCoding:") retVoid [argCULong (if value then 1 else 0)]

-- | @- decodingFailurePolicy@
decodingFailurePolicy :: IsNSKeyedUnarchiver nsKeyedUnarchiver => nsKeyedUnarchiver -> IO NSDecodingFailurePolicy
decodingFailurePolicy nsKeyedUnarchiver  =
  fmap (coerce :: CLong -> NSDecodingFailurePolicy) $ sendMsg nsKeyedUnarchiver (mkSelector "decodingFailurePolicy") retCLong []

-- | @- setDecodingFailurePolicy:@
setDecodingFailurePolicy :: IsNSKeyedUnarchiver nsKeyedUnarchiver => nsKeyedUnarchiver -> NSDecodingFailurePolicy -> IO ()
setDecodingFailurePolicy nsKeyedUnarchiver  value =
  sendMsg nsKeyedUnarchiver (mkSelector "setDecodingFailurePolicy:") retVoid [argCLong (coerce value)]

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @initForReadingFromData:error:@
initForReadingFromData_errorSelector :: Selector
initForReadingFromData_errorSelector = mkSelector "initForReadingFromData:error:"

-- | @Selector@ for @unarchivedObjectOfClass:fromData:error:@
unarchivedObjectOfClass_fromData_errorSelector :: Selector
unarchivedObjectOfClass_fromData_errorSelector = mkSelector "unarchivedObjectOfClass:fromData:error:"

-- | @Selector@ for @unarchivedArrayOfObjectsOfClass:fromData:error:@
unarchivedArrayOfObjectsOfClass_fromData_errorSelector :: Selector
unarchivedArrayOfObjectsOfClass_fromData_errorSelector = mkSelector "unarchivedArrayOfObjectsOfClass:fromData:error:"

-- | @Selector@ for @unarchivedDictionaryWithKeysOfClass:objectsOfClass:fromData:error:@
unarchivedDictionaryWithKeysOfClass_objectsOfClass_fromData_errorSelector :: Selector
unarchivedDictionaryWithKeysOfClass_objectsOfClass_fromData_errorSelector = mkSelector "unarchivedDictionaryWithKeysOfClass:objectsOfClass:fromData:error:"

-- | @Selector@ for @unarchivedObjectOfClasses:fromData:error:@
unarchivedObjectOfClasses_fromData_errorSelector :: Selector
unarchivedObjectOfClasses_fromData_errorSelector = mkSelector "unarchivedObjectOfClasses:fromData:error:"

-- | @Selector@ for @unarchivedArrayOfObjectsOfClasses:fromData:error:@
unarchivedArrayOfObjectsOfClasses_fromData_errorSelector :: Selector
unarchivedArrayOfObjectsOfClasses_fromData_errorSelector = mkSelector "unarchivedArrayOfObjectsOfClasses:fromData:error:"

-- | @Selector@ for @unarchivedDictionaryWithKeysOfClasses:objectsOfClasses:fromData:error:@
unarchivedDictionaryWithKeysOfClasses_objectsOfClasses_fromData_errorSelector :: Selector
unarchivedDictionaryWithKeysOfClasses_objectsOfClasses_fromData_errorSelector = mkSelector "unarchivedDictionaryWithKeysOfClasses:objectsOfClasses:fromData:error:"

-- | @Selector@ for @init@
initSelector :: Selector
initSelector = mkSelector "init"

-- | @Selector@ for @initForReadingWithData:@
initForReadingWithDataSelector :: Selector
initForReadingWithDataSelector = mkSelector "initForReadingWithData:"

-- | @Selector@ for @unarchiveObjectWithData:@
unarchiveObjectWithDataSelector :: Selector
unarchiveObjectWithDataSelector = mkSelector "unarchiveObjectWithData:"

-- | @Selector@ for @unarchiveTopLevelObjectWithData:error:@
unarchiveTopLevelObjectWithData_errorSelector :: Selector
unarchiveTopLevelObjectWithData_errorSelector = mkSelector "unarchiveTopLevelObjectWithData:error:"

-- | @Selector@ for @unarchiveObjectWithFile:@
unarchiveObjectWithFileSelector :: Selector
unarchiveObjectWithFileSelector = mkSelector "unarchiveObjectWithFile:"

-- | @Selector@ for @finishDecoding@
finishDecodingSelector :: Selector
finishDecodingSelector = mkSelector "finishDecoding"

-- | @Selector@ for @setClass:forClassName:@
setClass_forClassNameSelector :: Selector
setClass_forClassNameSelector = mkSelector "setClass:forClassName:"

-- | @Selector@ for @classForClassName:@
classForClassNameSelector :: Selector
classForClassNameSelector = mkSelector "classForClassName:"

-- | @Selector@ for @containsValueForKey:@
containsValueForKeySelector :: Selector
containsValueForKeySelector = mkSelector "containsValueForKey:"

-- | @Selector@ for @decodeObjectForKey:@
decodeObjectForKeySelector :: Selector
decodeObjectForKeySelector = mkSelector "decodeObjectForKey:"

-- | @Selector@ for @decodeBoolForKey:@
decodeBoolForKeySelector :: Selector
decodeBoolForKeySelector = mkSelector "decodeBoolForKey:"

-- | @Selector@ for @decodeIntForKey:@
decodeIntForKeySelector :: Selector
decodeIntForKeySelector = mkSelector "decodeIntForKey:"

-- | @Selector@ for @decodeInt32ForKey:@
decodeInt32ForKeySelector :: Selector
decodeInt32ForKeySelector = mkSelector "decodeInt32ForKey:"

-- | @Selector@ for @decodeInt64ForKey:@
decodeInt64ForKeySelector :: Selector
decodeInt64ForKeySelector = mkSelector "decodeInt64ForKey:"

-- | @Selector@ for @decodeFloatForKey:@
decodeFloatForKeySelector :: Selector
decodeFloatForKeySelector = mkSelector "decodeFloatForKey:"

-- | @Selector@ for @decodeDoubleForKey:@
decodeDoubleForKeySelector :: Selector
decodeDoubleForKeySelector = mkSelector "decodeDoubleForKey:"

-- | @Selector@ for @decodeBytesForKey:returnedLength:@
decodeBytesForKey_returnedLengthSelector :: Selector
decodeBytesForKey_returnedLengthSelector = mkSelector "decodeBytesForKey:returnedLength:"

-- | @Selector@ for @requiresSecureCoding@
requiresSecureCodingSelector :: Selector
requiresSecureCodingSelector = mkSelector "requiresSecureCoding"

-- | @Selector@ for @setRequiresSecureCoding:@
setRequiresSecureCodingSelector :: Selector
setRequiresSecureCodingSelector = mkSelector "setRequiresSecureCoding:"

-- | @Selector@ for @decodingFailurePolicy@
decodingFailurePolicySelector :: Selector
decodingFailurePolicySelector = mkSelector "decodingFailurePolicy"

-- | @Selector@ for @setDecodingFailurePolicy:@
setDecodingFailurePolicySelector :: Selector
setDecodingFailurePolicySelector = mkSelector "setDecodingFailurePolicy:"

