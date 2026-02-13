{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE DataKinds #-}
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
  , delegate
  , setDelegate
  , requiresSecureCoding
  , setRequiresSecureCoding
  , decodingFailurePolicy
  , setDecodingFailurePolicy
  , classForClassNameSelector
  , containsValueForKeySelector
  , decodeBoolForKeySelector
  , decodeBytesForKey_returnedLengthSelector
  , decodeDoubleForKeySelector
  , decodeFloatForKeySelector
  , decodeInt32ForKeySelector
  , decodeInt64ForKeySelector
  , decodeIntForKeySelector
  , decodeObjectForKeySelector
  , decodingFailurePolicySelector
  , delegateSelector
  , finishDecodingSelector
  , initForReadingFromData_errorSelector
  , initForReadingWithDataSelector
  , initSelector
  , nsKeyedUnarchiverClassForClassNameSelector
  , nsKeyedUnarchiverSetClass_forClassNameSelector
  , requiresSecureCodingSelector
  , setClass_forClassNameSelector
  , setDecodingFailurePolicySelector
  , setDelegateSelector
  , setRequiresSecureCodingSelector
  , unarchiveObjectWithDataSelector
  , unarchiveObjectWithFileSelector
  , unarchiveTopLevelObjectWithData_errorSelector
  , unarchivedArrayOfObjectsOfClass_fromData_errorSelector
  , unarchivedArrayOfObjectsOfClasses_fromData_errorSelector
  , unarchivedDictionaryWithKeysOfClass_objectsOfClass_fromData_errorSelector
  , unarchivedDictionaryWithKeysOfClasses_objectsOfClasses_fromData_errorSelector
  , unarchivedObjectOfClass_fromData_errorSelector
  , unarchivedObjectOfClasses_fromData_errorSelector

  -- * Enum types
  , NSDecodingFailurePolicy(NSDecodingFailurePolicy)
  , pattern NSDecodingFailurePolicyRaiseException
  , pattern NSDecodingFailurePolicySetErrorAndReturn

  ) where

import Foreign.Ptr (Ptr, FunPtr)
import Foreign.C.Types

import ObjC.Runtime.Types
import ObjC.Runtime.Message (sendMessage, sendOwnedMessage, sendClassMessage, sendOwnedClassMessage)
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
initForReadingFromData_error nsKeyedUnarchiver data_ error_ =
  sendOwnedMessage nsKeyedUnarchiver initForReadingFromData_errorSelector (toNSData data_) (toNSError error_)

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
    sendClassMessage cls' unarchivedObjectOfClass_fromData_errorSelector cls (toNSData data_) (toNSError error_)

-- | Decodes the @NSArray@ root object from @data@ which should be an @NSArray<cls>@ containing the given non-collection class (no nested arrays or arrays of dictionaries, etc) from the given archive, previously encoded by @NSKeyedArchiver.@  Enables @requiresSecureCoding@ and sets the @decodingFailurePolicy@ to @NSDecodingFailurePolicySetErrorAndReturn.@
--
-- Returns @nil@ if the given data is not valid or cannot be decoded, and sets the @error@ out parameter.
--
-- ObjC selector: @+ unarchivedArrayOfObjectsOfClass:fromData:error:@
unarchivedArrayOfObjectsOfClass_fromData_error :: (IsNSData data_, IsNSError error_) => Class -> data_ -> error_ -> IO (Id NSArray)
unarchivedArrayOfObjectsOfClass_fromData_error cls data_ error_ =
  do
    cls' <- getRequiredClass "NSKeyedUnarchiver"
    sendClassMessage cls' unarchivedArrayOfObjectsOfClass_fromData_errorSelector cls (toNSData data_) (toNSError error_)

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
    sendClassMessage cls' unarchivedDictionaryWithKeysOfClass_objectsOfClass_fromData_errorSelector keyCls valueCls (toNSData data_) (toNSError error_)

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
    sendClassMessage cls' unarchivedObjectOfClasses_fromData_errorSelector (toNSSet classes) (toNSData data_) (toNSError error_)

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
    sendClassMessage cls' unarchivedArrayOfObjectsOfClasses_fromData_errorSelector (toNSSet classes) (toNSData data_) (toNSError error_)

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
    sendClassMessage cls' unarchivedDictionaryWithKeysOfClasses_objectsOfClasses_fromData_errorSelector (toNSSet keyClasses) (toNSSet valueClasses) (toNSData data_) (toNSError error_)

-- | @- init@
init_ :: IsNSKeyedUnarchiver nsKeyedUnarchiver => nsKeyedUnarchiver -> IO (Id NSKeyedUnarchiver)
init_ nsKeyedUnarchiver =
  sendOwnedMessage nsKeyedUnarchiver initSelector

-- | @- initForReadingWithData:@
initForReadingWithData :: (IsNSKeyedUnarchiver nsKeyedUnarchiver, IsNSData data_) => nsKeyedUnarchiver -> data_ -> IO (Id NSKeyedUnarchiver)
initForReadingWithData nsKeyedUnarchiver data_ =
  sendOwnedMessage nsKeyedUnarchiver initForReadingWithDataSelector (toNSData data_)

-- | @+ unarchiveObjectWithData:@
unarchiveObjectWithData :: IsNSData data_ => data_ -> IO RawId
unarchiveObjectWithData data_ =
  do
    cls' <- getRequiredClass "NSKeyedUnarchiver"
    sendClassMessage cls' unarchiveObjectWithDataSelector (toNSData data_)

-- | @+ unarchiveTopLevelObjectWithData:error:@
unarchiveTopLevelObjectWithData_error :: (IsNSData data_, IsNSError error_) => data_ -> error_ -> IO RawId
unarchiveTopLevelObjectWithData_error data_ error_ =
  do
    cls' <- getRequiredClass "NSKeyedUnarchiver"
    sendClassMessage cls' unarchiveTopLevelObjectWithData_errorSelector (toNSData data_) (toNSError error_)

-- | @+ unarchiveObjectWithFile:@
unarchiveObjectWithFile :: IsNSString path => path -> IO RawId
unarchiveObjectWithFile path =
  do
    cls' <- getRequiredClass "NSKeyedUnarchiver"
    sendClassMessage cls' unarchiveObjectWithFileSelector (toNSString path)

-- | @- finishDecoding@
finishDecoding :: IsNSKeyedUnarchiver nsKeyedUnarchiver => nsKeyedUnarchiver -> IO ()
finishDecoding nsKeyedUnarchiver =
  sendMessage nsKeyedUnarchiver finishDecodingSelector

-- | @+ setClass:forClassName:@
nsKeyedUnarchiverSetClass_forClassName :: IsNSString codedName => Class -> codedName -> IO ()
nsKeyedUnarchiverSetClass_forClassName cls codedName =
  do
    cls' <- getRequiredClass "NSKeyedUnarchiver"
    sendClassMessage cls' nsKeyedUnarchiverSetClass_forClassNameSelector cls (toNSString codedName)

-- | @- setClass:forClassName:@
setClass_forClassName :: (IsNSKeyedUnarchiver nsKeyedUnarchiver, IsNSString codedName) => nsKeyedUnarchiver -> Class -> codedName -> IO ()
setClass_forClassName nsKeyedUnarchiver cls codedName =
  sendMessage nsKeyedUnarchiver setClass_forClassNameSelector cls (toNSString codedName)

-- | @+ classForClassName:@
nsKeyedUnarchiverClassForClassName :: IsNSString codedName => codedName -> IO Class
nsKeyedUnarchiverClassForClassName codedName =
  do
    cls' <- getRequiredClass "NSKeyedUnarchiver"
    sendClassMessage cls' nsKeyedUnarchiverClassForClassNameSelector (toNSString codedName)

-- | @- classForClassName:@
classForClassName :: (IsNSKeyedUnarchiver nsKeyedUnarchiver, IsNSString codedName) => nsKeyedUnarchiver -> codedName -> IO Class
classForClassName nsKeyedUnarchiver codedName =
  sendMessage nsKeyedUnarchiver classForClassNameSelector (toNSString codedName)

-- | @- containsValueForKey:@
containsValueForKey :: (IsNSKeyedUnarchiver nsKeyedUnarchiver, IsNSString key) => nsKeyedUnarchiver -> key -> IO Bool
containsValueForKey nsKeyedUnarchiver key =
  sendMessage nsKeyedUnarchiver containsValueForKeySelector (toNSString key)

-- | @- decodeObjectForKey:@
decodeObjectForKey :: (IsNSKeyedUnarchiver nsKeyedUnarchiver, IsNSString key) => nsKeyedUnarchiver -> key -> IO RawId
decodeObjectForKey nsKeyedUnarchiver key =
  sendMessage nsKeyedUnarchiver decodeObjectForKeySelector (toNSString key)

-- | @- decodeBoolForKey:@
decodeBoolForKey :: (IsNSKeyedUnarchiver nsKeyedUnarchiver, IsNSString key) => nsKeyedUnarchiver -> key -> IO Bool
decodeBoolForKey nsKeyedUnarchiver key =
  sendMessage nsKeyedUnarchiver decodeBoolForKeySelector (toNSString key)

-- | @- decodeIntForKey:@
decodeIntForKey :: (IsNSKeyedUnarchiver nsKeyedUnarchiver, IsNSString key) => nsKeyedUnarchiver -> key -> IO CInt
decodeIntForKey nsKeyedUnarchiver key =
  sendMessage nsKeyedUnarchiver decodeIntForKeySelector (toNSString key)

-- | @- decodeInt32ForKey:@
decodeInt32ForKey :: (IsNSKeyedUnarchiver nsKeyedUnarchiver, IsNSString key) => nsKeyedUnarchiver -> key -> IO CInt
decodeInt32ForKey nsKeyedUnarchiver key =
  sendMessage nsKeyedUnarchiver decodeInt32ForKeySelector (toNSString key)

-- | @- decodeInt64ForKey:@
decodeInt64ForKey :: (IsNSKeyedUnarchiver nsKeyedUnarchiver, IsNSString key) => nsKeyedUnarchiver -> key -> IO CLong
decodeInt64ForKey nsKeyedUnarchiver key =
  sendMessage nsKeyedUnarchiver decodeInt64ForKeySelector (toNSString key)

-- | @- decodeFloatForKey:@
decodeFloatForKey :: (IsNSKeyedUnarchiver nsKeyedUnarchiver, IsNSString key) => nsKeyedUnarchiver -> key -> IO CFloat
decodeFloatForKey nsKeyedUnarchiver key =
  sendMessage nsKeyedUnarchiver decodeFloatForKeySelector (toNSString key)

-- | @- decodeDoubleForKey:@
decodeDoubleForKey :: (IsNSKeyedUnarchiver nsKeyedUnarchiver, IsNSString key) => nsKeyedUnarchiver -> key -> IO CDouble
decodeDoubleForKey nsKeyedUnarchiver key =
  sendMessage nsKeyedUnarchiver decodeDoubleForKeySelector (toNSString key)

-- | @- decodeBytesForKey:returnedLength:@
decodeBytesForKey_returnedLength :: (IsNSKeyedUnarchiver nsKeyedUnarchiver, IsNSString key) => nsKeyedUnarchiver -> key -> Ptr CULong -> IO (Const (Ptr CUChar))
decodeBytesForKey_returnedLength nsKeyedUnarchiver key lengthp =
  sendMessage nsKeyedUnarchiver decodeBytesForKey_returnedLengthSelector (toNSString key) lengthp

-- | @- delegate@
delegate :: IsNSKeyedUnarchiver nsKeyedUnarchiver => nsKeyedUnarchiver -> IO RawId
delegate nsKeyedUnarchiver =
  sendMessage nsKeyedUnarchiver delegateSelector

-- | @- setDelegate:@
setDelegate :: IsNSKeyedUnarchiver nsKeyedUnarchiver => nsKeyedUnarchiver -> RawId -> IO ()
setDelegate nsKeyedUnarchiver value =
  sendMessage nsKeyedUnarchiver setDelegateSelector value

-- | @- requiresSecureCoding@
requiresSecureCoding :: IsNSKeyedUnarchiver nsKeyedUnarchiver => nsKeyedUnarchiver -> IO Bool
requiresSecureCoding nsKeyedUnarchiver =
  sendMessage nsKeyedUnarchiver requiresSecureCodingSelector

-- | @- setRequiresSecureCoding:@
setRequiresSecureCoding :: IsNSKeyedUnarchiver nsKeyedUnarchiver => nsKeyedUnarchiver -> Bool -> IO ()
setRequiresSecureCoding nsKeyedUnarchiver value =
  sendMessage nsKeyedUnarchiver setRequiresSecureCodingSelector value

-- | @- decodingFailurePolicy@
decodingFailurePolicy :: IsNSKeyedUnarchiver nsKeyedUnarchiver => nsKeyedUnarchiver -> IO NSDecodingFailurePolicy
decodingFailurePolicy nsKeyedUnarchiver =
  sendMessage nsKeyedUnarchiver decodingFailurePolicySelector

-- | @- setDecodingFailurePolicy:@
setDecodingFailurePolicy :: IsNSKeyedUnarchiver nsKeyedUnarchiver => nsKeyedUnarchiver -> NSDecodingFailurePolicy -> IO ()
setDecodingFailurePolicy nsKeyedUnarchiver value =
  sendMessage nsKeyedUnarchiver setDecodingFailurePolicySelector value

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @initForReadingFromData:error:@
initForReadingFromData_errorSelector :: Selector '[Id NSData, Id NSError] (Id NSKeyedUnarchiver)
initForReadingFromData_errorSelector = mkSelector "initForReadingFromData:error:"

-- | @Selector@ for @unarchivedObjectOfClass:fromData:error:@
unarchivedObjectOfClass_fromData_errorSelector :: Selector '[Class, Id NSData, Id NSError] RawId
unarchivedObjectOfClass_fromData_errorSelector = mkSelector "unarchivedObjectOfClass:fromData:error:"

-- | @Selector@ for @unarchivedArrayOfObjectsOfClass:fromData:error:@
unarchivedArrayOfObjectsOfClass_fromData_errorSelector :: Selector '[Class, Id NSData, Id NSError] (Id NSArray)
unarchivedArrayOfObjectsOfClass_fromData_errorSelector = mkSelector "unarchivedArrayOfObjectsOfClass:fromData:error:"

-- | @Selector@ for @unarchivedDictionaryWithKeysOfClass:objectsOfClass:fromData:error:@
unarchivedDictionaryWithKeysOfClass_objectsOfClass_fromData_errorSelector :: Selector '[Class, Class, Id NSData, Id NSError] (Id NSDictionary)
unarchivedDictionaryWithKeysOfClass_objectsOfClass_fromData_errorSelector = mkSelector "unarchivedDictionaryWithKeysOfClass:objectsOfClass:fromData:error:"

-- | @Selector@ for @unarchivedObjectOfClasses:fromData:error:@
unarchivedObjectOfClasses_fromData_errorSelector :: Selector '[Id NSSet, Id NSData, Id NSError] RawId
unarchivedObjectOfClasses_fromData_errorSelector = mkSelector "unarchivedObjectOfClasses:fromData:error:"

-- | @Selector@ for @unarchivedArrayOfObjectsOfClasses:fromData:error:@
unarchivedArrayOfObjectsOfClasses_fromData_errorSelector :: Selector '[Id NSSet, Id NSData, Id NSError] (Id NSArray)
unarchivedArrayOfObjectsOfClasses_fromData_errorSelector = mkSelector "unarchivedArrayOfObjectsOfClasses:fromData:error:"

-- | @Selector@ for @unarchivedDictionaryWithKeysOfClasses:objectsOfClasses:fromData:error:@
unarchivedDictionaryWithKeysOfClasses_objectsOfClasses_fromData_errorSelector :: Selector '[Id NSSet, Id NSSet, Id NSData, Id NSError] (Id NSDictionary)
unarchivedDictionaryWithKeysOfClasses_objectsOfClasses_fromData_errorSelector = mkSelector "unarchivedDictionaryWithKeysOfClasses:objectsOfClasses:fromData:error:"

-- | @Selector@ for @init@
initSelector :: Selector '[] (Id NSKeyedUnarchiver)
initSelector = mkSelector "init"

-- | @Selector@ for @initForReadingWithData:@
initForReadingWithDataSelector :: Selector '[Id NSData] (Id NSKeyedUnarchiver)
initForReadingWithDataSelector = mkSelector "initForReadingWithData:"

-- | @Selector@ for @unarchiveObjectWithData:@
unarchiveObjectWithDataSelector :: Selector '[Id NSData] RawId
unarchiveObjectWithDataSelector = mkSelector "unarchiveObjectWithData:"

-- | @Selector@ for @unarchiveTopLevelObjectWithData:error:@
unarchiveTopLevelObjectWithData_errorSelector :: Selector '[Id NSData, Id NSError] RawId
unarchiveTopLevelObjectWithData_errorSelector = mkSelector "unarchiveTopLevelObjectWithData:error:"

-- | @Selector@ for @unarchiveObjectWithFile:@
unarchiveObjectWithFileSelector :: Selector '[Id NSString] RawId
unarchiveObjectWithFileSelector = mkSelector "unarchiveObjectWithFile:"

-- | @Selector@ for @finishDecoding@
finishDecodingSelector :: Selector '[] ()
finishDecodingSelector = mkSelector "finishDecoding"

-- | @Selector@ for @setClass:forClassName:@
nsKeyedUnarchiverSetClass_forClassNameSelector :: Selector '[Class, Id NSString] ()
nsKeyedUnarchiverSetClass_forClassNameSelector = mkSelector "setClass:forClassName:"

-- | @Selector@ for @setClass:forClassName:@
setClass_forClassNameSelector :: Selector '[Class, Id NSString] ()
setClass_forClassNameSelector = mkSelector "setClass:forClassName:"

-- | @Selector@ for @classForClassName:@
nsKeyedUnarchiverClassForClassNameSelector :: Selector '[Id NSString] Class
nsKeyedUnarchiverClassForClassNameSelector = mkSelector "classForClassName:"

-- | @Selector@ for @classForClassName:@
classForClassNameSelector :: Selector '[Id NSString] Class
classForClassNameSelector = mkSelector "classForClassName:"

-- | @Selector@ for @containsValueForKey:@
containsValueForKeySelector :: Selector '[Id NSString] Bool
containsValueForKeySelector = mkSelector "containsValueForKey:"

-- | @Selector@ for @decodeObjectForKey:@
decodeObjectForKeySelector :: Selector '[Id NSString] RawId
decodeObjectForKeySelector = mkSelector "decodeObjectForKey:"

-- | @Selector@ for @decodeBoolForKey:@
decodeBoolForKeySelector :: Selector '[Id NSString] Bool
decodeBoolForKeySelector = mkSelector "decodeBoolForKey:"

-- | @Selector@ for @decodeIntForKey:@
decodeIntForKeySelector :: Selector '[Id NSString] CInt
decodeIntForKeySelector = mkSelector "decodeIntForKey:"

-- | @Selector@ for @decodeInt32ForKey:@
decodeInt32ForKeySelector :: Selector '[Id NSString] CInt
decodeInt32ForKeySelector = mkSelector "decodeInt32ForKey:"

-- | @Selector@ for @decodeInt64ForKey:@
decodeInt64ForKeySelector :: Selector '[Id NSString] CLong
decodeInt64ForKeySelector = mkSelector "decodeInt64ForKey:"

-- | @Selector@ for @decodeFloatForKey:@
decodeFloatForKeySelector :: Selector '[Id NSString] CFloat
decodeFloatForKeySelector = mkSelector "decodeFloatForKey:"

-- | @Selector@ for @decodeDoubleForKey:@
decodeDoubleForKeySelector :: Selector '[Id NSString] CDouble
decodeDoubleForKeySelector = mkSelector "decodeDoubleForKey:"

-- | @Selector@ for @decodeBytesForKey:returnedLength:@
decodeBytesForKey_returnedLengthSelector :: Selector '[Id NSString, Ptr CULong] (Const (Ptr CUChar))
decodeBytesForKey_returnedLengthSelector = mkSelector "decodeBytesForKey:returnedLength:"

-- | @Selector@ for @delegate@
delegateSelector :: Selector '[] RawId
delegateSelector = mkSelector "delegate"

-- | @Selector@ for @setDelegate:@
setDelegateSelector :: Selector '[RawId] ()
setDelegateSelector = mkSelector "setDelegate:"

-- | @Selector@ for @requiresSecureCoding@
requiresSecureCodingSelector :: Selector '[] Bool
requiresSecureCodingSelector = mkSelector "requiresSecureCoding"

-- | @Selector@ for @setRequiresSecureCoding:@
setRequiresSecureCodingSelector :: Selector '[Bool] ()
setRequiresSecureCodingSelector = mkSelector "setRequiresSecureCoding:"

-- | @Selector@ for @decodingFailurePolicy@
decodingFailurePolicySelector :: Selector '[] NSDecodingFailurePolicy
decodingFailurePolicySelector = mkSelector "decodingFailurePolicy"

-- | @Selector@ for @setDecodingFailurePolicy:@
setDecodingFailurePolicySelector :: Selector '[NSDecodingFailurePolicy] ()
setDecodingFailurePolicySelector = mkSelector "setDecodingFailurePolicy:"

