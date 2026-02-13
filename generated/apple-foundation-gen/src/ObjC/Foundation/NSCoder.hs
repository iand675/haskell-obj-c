{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Generated bindings for @NSCoder@.
module ObjC.Foundation.NSCoder
  ( NSCoder
  , IsNSCoder(..)
  , encodeValueOfObjCType_at
  , encodeDataObject
  , decodeDataObject
  , decodeValueOfObjCType_at_size
  , versionForClassName
  , encodePoint_forKey
  , encodeSize_forKey
  , encodeRect_forKey
  , decodePointForKey
  , decodeSizeForKey
  , decodeRectForKey
  , encodePoint
  , decodePoint
  , encodeSize
  , decodeSize
  , encodeRect
  , decodeRect
  , decodeValueOfObjCType_at
  , encodeNXObject
  , decodeNXObject
  , encodeObject
  , encodeRootObject
  , encodeBycopyObject
  , encodeByrefObject
  , encodeConditionalObject
  , encodeValuesOfObjCTypes
  , encodeArrayOfObjCType_count_at
  , encodeBytes_length
  , decodeObject
  , decodeTopLevelObjectAndReturnError
  , decodeValuesOfObjCTypes
  , decodeArrayOfObjCType_count_at
  , decodeBytesWithReturnedLength
  , encodePropertyList
  , decodePropertyList
  , setObjectZone
  , objectZone
  , encodeObject_forKey
  , encodeConditionalObject_forKey
  , encodeBool_forKey
  , encodeInt_forKey
  , encodeInt32_forKey
  , encodeInt64_forKey
  , encodeFloat_forKey
  , encodeDouble_forKey
  , encodeBytes_length_forKey
  , containsValueForKey
  , decodeObjectForKey
  , decodeTopLevelObjectForKey_error
  , decodeBoolForKey
  , decodeIntForKey
  , decodeInt32ForKey
  , decodeInt64ForKey
  , decodeFloatForKey
  , decodeDoubleForKey
  , decodeBytesForKey_returnedLength
  , decodeBytesWithMinimumLength
  , decodeBytesForKey_minimumLength
  , encodeInteger_forKey
  , decodeIntegerForKey
  , decodeObjectOfClass_forKey
  , decodeTopLevelObjectOfClass_forKey_error
  , decodeArrayOfObjectsOfClass_forKey
  , decodeDictionaryWithKeysOfClass_objectsOfClass_forKey
  , decodeObjectOfClasses_forKey
  , decodeTopLevelObjectOfClasses_forKey_error
  , decodeArrayOfObjectsOfClasses_forKey
  , decodeDictionaryWithKeysOfClasses_objectsOfClasses_forKey
  , decodePropertyListForKey
  , failWithError
  , systemVersion
  , allowsKeyedCoding
  , requiresSecureCoding
  , allowedClasses
  , decodingFailurePolicy
  , error_
  , allowedClassesSelector
  , allowsKeyedCodingSelector
  , containsValueForKeySelector
  , decodeArrayOfObjCType_count_atSelector
  , decodeArrayOfObjectsOfClass_forKeySelector
  , decodeArrayOfObjectsOfClasses_forKeySelector
  , decodeBoolForKeySelector
  , decodeBytesForKey_minimumLengthSelector
  , decodeBytesForKey_returnedLengthSelector
  , decodeBytesWithMinimumLengthSelector
  , decodeBytesWithReturnedLengthSelector
  , decodeDataObjectSelector
  , decodeDictionaryWithKeysOfClass_objectsOfClass_forKeySelector
  , decodeDictionaryWithKeysOfClasses_objectsOfClasses_forKeySelector
  , decodeDoubleForKeySelector
  , decodeFloatForKeySelector
  , decodeInt32ForKeySelector
  , decodeInt64ForKeySelector
  , decodeIntForKeySelector
  , decodeIntegerForKeySelector
  , decodeNXObjectSelector
  , decodeObjectForKeySelector
  , decodeObjectOfClass_forKeySelector
  , decodeObjectOfClasses_forKeySelector
  , decodeObjectSelector
  , decodePointForKeySelector
  , decodePointSelector
  , decodePropertyListForKeySelector
  , decodePropertyListSelector
  , decodeRectForKeySelector
  , decodeRectSelector
  , decodeSizeForKeySelector
  , decodeSizeSelector
  , decodeTopLevelObjectAndReturnErrorSelector
  , decodeTopLevelObjectForKey_errorSelector
  , decodeTopLevelObjectOfClass_forKey_errorSelector
  , decodeTopLevelObjectOfClasses_forKey_errorSelector
  , decodeValueOfObjCType_atSelector
  , decodeValueOfObjCType_at_sizeSelector
  , decodeValuesOfObjCTypesSelector
  , decodingFailurePolicySelector
  , encodeArrayOfObjCType_count_atSelector
  , encodeBool_forKeySelector
  , encodeBycopyObjectSelector
  , encodeByrefObjectSelector
  , encodeBytes_lengthSelector
  , encodeBytes_length_forKeySelector
  , encodeConditionalObjectSelector
  , encodeConditionalObject_forKeySelector
  , encodeDataObjectSelector
  , encodeDouble_forKeySelector
  , encodeFloat_forKeySelector
  , encodeInt32_forKeySelector
  , encodeInt64_forKeySelector
  , encodeInt_forKeySelector
  , encodeInteger_forKeySelector
  , encodeNXObjectSelector
  , encodeObjectSelector
  , encodeObject_forKeySelector
  , encodePointSelector
  , encodePoint_forKeySelector
  , encodePropertyListSelector
  , encodeRectSelector
  , encodeRect_forKeySelector
  , encodeRootObjectSelector
  , encodeSizeSelector
  , encodeSize_forKeySelector
  , encodeValueOfObjCType_atSelector
  , encodeValuesOfObjCTypesSelector
  , errorSelector
  , failWithErrorSelector
  , objectZoneSelector
  , requiresSecureCodingSelector
  , setObjectZoneSelector
  , systemVersionSelector
  , versionForClassNameSelector

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
import ObjC.Foundation.Internal.Structs
import ObjC.Foundation.Internal.Enums

-- | @- encodeValueOfObjCType:at:@
encodeValueOfObjCType_at :: IsNSCoder nsCoder => nsCoder -> Const (Ptr CChar) -> Const (Ptr ()) -> IO ()
encodeValueOfObjCType_at nsCoder type_ addr =
  sendMessage nsCoder encodeValueOfObjCType_atSelector type_ addr

-- | @- encodeDataObject:@
encodeDataObject :: (IsNSCoder nsCoder, IsNSData data_) => nsCoder -> data_ -> IO ()
encodeDataObject nsCoder data_ =
  sendMessage nsCoder encodeDataObjectSelector (toNSData data_)

-- | @- decodeDataObject@
decodeDataObject :: IsNSCoder nsCoder => nsCoder -> IO (Id NSData)
decodeDataObject nsCoder =
  sendMessage nsCoder decodeDataObjectSelector

-- | @- decodeValueOfObjCType:at:size:@
decodeValueOfObjCType_at_size :: IsNSCoder nsCoder => nsCoder -> Const (Ptr CChar) -> Ptr () -> CULong -> IO ()
decodeValueOfObjCType_at_size nsCoder type_ data_ size =
  sendMessage nsCoder decodeValueOfObjCType_at_sizeSelector type_ data_ size

-- | @- versionForClassName:@
versionForClassName :: (IsNSCoder nsCoder, IsNSString className) => nsCoder -> className -> IO CLong
versionForClassName nsCoder className =
  sendMessage nsCoder versionForClassNameSelector (toNSString className)

-- | @- encodePoint:forKey:@
encodePoint_forKey :: (IsNSCoder nsCoder, IsNSString key) => nsCoder -> NSPoint -> key -> IO ()
encodePoint_forKey nsCoder point key =
  sendMessage nsCoder encodePoint_forKeySelector point (toNSString key)

-- | @- encodeSize:forKey:@
encodeSize_forKey :: (IsNSCoder nsCoder, IsNSString key) => nsCoder -> NSSize -> key -> IO ()
encodeSize_forKey nsCoder size key =
  sendMessage nsCoder encodeSize_forKeySelector size (toNSString key)

-- | @- encodeRect:forKey:@
encodeRect_forKey :: (IsNSCoder nsCoder, IsNSString key) => nsCoder -> NSRect -> key -> IO ()
encodeRect_forKey nsCoder rect key =
  sendMessage nsCoder encodeRect_forKeySelector rect (toNSString key)

-- | @- decodePointForKey:@
decodePointForKey :: (IsNSCoder nsCoder, IsNSString key) => nsCoder -> key -> IO NSPoint
decodePointForKey nsCoder key =
  sendMessage nsCoder decodePointForKeySelector (toNSString key)

-- | @- decodeSizeForKey:@
decodeSizeForKey :: (IsNSCoder nsCoder, IsNSString key) => nsCoder -> key -> IO NSSize
decodeSizeForKey nsCoder key =
  sendMessage nsCoder decodeSizeForKeySelector (toNSString key)

-- | @- decodeRectForKey:@
decodeRectForKey :: (IsNSCoder nsCoder, IsNSString key) => nsCoder -> key -> IO NSRect
decodeRectForKey nsCoder key =
  sendMessage nsCoder decodeRectForKeySelector (toNSString key)

-- | @- encodePoint:@
encodePoint :: IsNSCoder nsCoder => nsCoder -> NSPoint -> IO ()
encodePoint nsCoder point =
  sendMessage nsCoder encodePointSelector point

-- | @- decodePoint@
decodePoint :: IsNSCoder nsCoder => nsCoder -> IO NSPoint
decodePoint nsCoder =
  sendMessage nsCoder decodePointSelector

-- | @- encodeSize:@
encodeSize :: IsNSCoder nsCoder => nsCoder -> NSSize -> IO ()
encodeSize nsCoder size =
  sendMessage nsCoder encodeSizeSelector size

-- | @- decodeSize@
decodeSize :: IsNSCoder nsCoder => nsCoder -> IO NSSize
decodeSize nsCoder =
  sendMessage nsCoder decodeSizeSelector

-- | @- encodeRect:@
encodeRect :: IsNSCoder nsCoder => nsCoder -> NSRect -> IO ()
encodeRect nsCoder rect =
  sendMessage nsCoder encodeRectSelector rect

-- | @- decodeRect@
decodeRect :: IsNSCoder nsCoder => nsCoder -> IO NSRect
decodeRect nsCoder =
  sendMessage nsCoder decodeRectSelector

-- | @- decodeValueOfObjCType:at:@
decodeValueOfObjCType_at :: IsNSCoder nsCoder => nsCoder -> Const (Ptr CChar) -> Ptr () -> IO ()
decodeValueOfObjCType_at nsCoder type_ data_ =
  sendMessage nsCoder decodeValueOfObjCType_atSelector type_ data_

-- | @- encodeNXObject:@
encodeNXObject :: IsNSCoder nsCoder => nsCoder -> RawId -> IO ()
encodeNXObject nsCoder object =
  sendMessage nsCoder encodeNXObjectSelector object

-- | @- decodeNXObject@
decodeNXObject :: IsNSCoder nsCoder => nsCoder -> IO RawId
decodeNXObject nsCoder =
  sendMessage nsCoder decodeNXObjectSelector

-- | @- encodeObject:@
encodeObject :: IsNSCoder nsCoder => nsCoder -> RawId -> IO ()
encodeObject nsCoder object =
  sendMessage nsCoder encodeObjectSelector object

-- | @- encodeRootObject:@
encodeRootObject :: IsNSCoder nsCoder => nsCoder -> RawId -> IO ()
encodeRootObject nsCoder rootObject =
  sendMessage nsCoder encodeRootObjectSelector rootObject

-- | @- encodeBycopyObject:@
encodeBycopyObject :: IsNSCoder nsCoder => nsCoder -> RawId -> IO ()
encodeBycopyObject nsCoder anObject =
  sendMessage nsCoder encodeBycopyObjectSelector anObject

-- | @- encodeByrefObject:@
encodeByrefObject :: IsNSCoder nsCoder => nsCoder -> RawId -> IO ()
encodeByrefObject nsCoder anObject =
  sendMessage nsCoder encodeByrefObjectSelector anObject

-- | @- encodeConditionalObject:@
encodeConditionalObject :: IsNSCoder nsCoder => nsCoder -> RawId -> IO ()
encodeConditionalObject nsCoder object =
  sendMessage nsCoder encodeConditionalObjectSelector object

-- | @- encodeValuesOfObjCTypes:@
encodeValuesOfObjCTypes :: IsNSCoder nsCoder => nsCoder -> Const (Ptr CChar) -> IO ()
encodeValuesOfObjCTypes nsCoder types =
  sendMessage nsCoder encodeValuesOfObjCTypesSelector types

-- | @- encodeArrayOfObjCType:count:at:@
encodeArrayOfObjCType_count_at :: IsNSCoder nsCoder => nsCoder -> Const (Ptr CChar) -> CULong -> Const (Ptr ()) -> IO ()
encodeArrayOfObjCType_count_at nsCoder type_ count array =
  sendMessage nsCoder encodeArrayOfObjCType_count_atSelector type_ count array

-- | @- encodeBytes:length:@
encodeBytes_length :: IsNSCoder nsCoder => nsCoder -> Const (Ptr ()) -> CULong -> IO ()
encodeBytes_length nsCoder byteaddr length_ =
  sendMessage nsCoder encodeBytes_lengthSelector byteaddr length_

-- | @- decodeObject@
decodeObject :: IsNSCoder nsCoder => nsCoder -> IO RawId
decodeObject nsCoder =
  sendMessage nsCoder decodeObjectSelector

-- | @- decodeTopLevelObjectAndReturnError:@
decodeTopLevelObjectAndReturnError :: (IsNSCoder nsCoder, IsNSError error_) => nsCoder -> error_ -> IO RawId
decodeTopLevelObjectAndReturnError nsCoder error_ =
  sendMessage nsCoder decodeTopLevelObjectAndReturnErrorSelector (toNSError error_)

-- | @- decodeValuesOfObjCTypes:@
decodeValuesOfObjCTypes :: IsNSCoder nsCoder => nsCoder -> Const (Ptr CChar) -> IO ()
decodeValuesOfObjCTypes nsCoder types =
  sendMessage nsCoder decodeValuesOfObjCTypesSelector types

-- | @- decodeArrayOfObjCType:count:at:@
decodeArrayOfObjCType_count_at :: IsNSCoder nsCoder => nsCoder -> Const (Ptr CChar) -> CULong -> Ptr () -> IO ()
decodeArrayOfObjCType_count_at nsCoder itemType count array =
  sendMessage nsCoder decodeArrayOfObjCType_count_atSelector itemType count array

-- | @- decodeBytesWithReturnedLength:@
decodeBytesWithReturnedLength :: IsNSCoder nsCoder => nsCoder -> Ptr CULong -> IO (Ptr ())
decodeBytesWithReturnedLength nsCoder lengthp =
  sendMessage nsCoder decodeBytesWithReturnedLengthSelector lengthp

-- | @- encodePropertyList:@
encodePropertyList :: IsNSCoder nsCoder => nsCoder -> RawId -> IO ()
encodePropertyList nsCoder aPropertyList =
  sendMessage nsCoder encodePropertyListSelector aPropertyList

-- | @- decodePropertyList@
decodePropertyList :: IsNSCoder nsCoder => nsCoder -> IO RawId
decodePropertyList nsCoder =
  sendMessage nsCoder decodePropertyListSelector

-- | @- setObjectZone:@
setObjectZone :: IsNSCoder nsCoder => nsCoder -> Ptr () -> IO ()
setObjectZone nsCoder zone =
  sendMessage nsCoder setObjectZoneSelector zone

-- | @- objectZone@
objectZone :: IsNSCoder nsCoder => nsCoder -> IO (Ptr ())
objectZone nsCoder =
  sendMessage nsCoder objectZoneSelector

-- | @- encodeObject:forKey:@
encodeObject_forKey :: (IsNSCoder nsCoder, IsNSString key) => nsCoder -> RawId -> key -> IO ()
encodeObject_forKey nsCoder object key =
  sendMessage nsCoder encodeObject_forKeySelector object (toNSString key)

-- | @- encodeConditionalObject:forKey:@
encodeConditionalObject_forKey :: (IsNSCoder nsCoder, IsNSString key) => nsCoder -> RawId -> key -> IO ()
encodeConditionalObject_forKey nsCoder object key =
  sendMessage nsCoder encodeConditionalObject_forKeySelector object (toNSString key)

-- | @- encodeBool:forKey:@
encodeBool_forKey :: (IsNSCoder nsCoder, IsNSString key) => nsCoder -> Bool -> key -> IO ()
encodeBool_forKey nsCoder value key =
  sendMessage nsCoder encodeBool_forKeySelector value (toNSString key)

-- | @- encodeInt:forKey:@
encodeInt_forKey :: (IsNSCoder nsCoder, IsNSString key) => nsCoder -> CInt -> key -> IO ()
encodeInt_forKey nsCoder value key =
  sendMessage nsCoder encodeInt_forKeySelector value (toNSString key)

-- | @- encodeInt32:forKey:@
encodeInt32_forKey :: (IsNSCoder nsCoder, IsNSString key) => nsCoder -> CInt -> key -> IO ()
encodeInt32_forKey nsCoder value key =
  sendMessage nsCoder encodeInt32_forKeySelector value (toNSString key)

-- | @- encodeInt64:forKey:@
encodeInt64_forKey :: (IsNSCoder nsCoder, IsNSString key) => nsCoder -> CLong -> key -> IO ()
encodeInt64_forKey nsCoder value key =
  sendMessage nsCoder encodeInt64_forKeySelector value (toNSString key)

-- | @- encodeFloat:forKey:@
encodeFloat_forKey :: (IsNSCoder nsCoder, IsNSString key) => nsCoder -> CFloat -> key -> IO ()
encodeFloat_forKey nsCoder value key =
  sendMessage nsCoder encodeFloat_forKeySelector value (toNSString key)

-- | @- encodeDouble:forKey:@
encodeDouble_forKey :: (IsNSCoder nsCoder, IsNSString key) => nsCoder -> CDouble -> key -> IO ()
encodeDouble_forKey nsCoder value key =
  sendMessage nsCoder encodeDouble_forKeySelector value (toNSString key)

-- | @- encodeBytes:length:forKey:@
encodeBytes_length_forKey :: (IsNSCoder nsCoder, IsNSString key) => nsCoder -> Const (Ptr CUChar) -> CULong -> key -> IO ()
encodeBytes_length_forKey nsCoder bytes length_ key =
  sendMessage nsCoder encodeBytes_length_forKeySelector bytes length_ (toNSString key)

-- | @- containsValueForKey:@
containsValueForKey :: (IsNSCoder nsCoder, IsNSString key) => nsCoder -> key -> IO Bool
containsValueForKey nsCoder key =
  sendMessage nsCoder containsValueForKeySelector (toNSString key)

-- | @- decodeObjectForKey:@
decodeObjectForKey :: (IsNSCoder nsCoder, IsNSString key) => nsCoder -> key -> IO RawId
decodeObjectForKey nsCoder key =
  sendMessage nsCoder decodeObjectForKeySelector (toNSString key)

-- | @- decodeTopLevelObjectForKey:error:@
decodeTopLevelObjectForKey_error :: (IsNSCoder nsCoder, IsNSString key, IsNSError error_) => nsCoder -> key -> error_ -> IO RawId
decodeTopLevelObjectForKey_error nsCoder key error_ =
  sendMessage nsCoder decodeTopLevelObjectForKey_errorSelector (toNSString key) (toNSError error_)

-- | @- decodeBoolForKey:@
decodeBoolForKey :: (IsNSCoder nsCoder, IsNSString key) => nsCoder -> key -> IO Bool
decodeBoolForKey nsCoder key =
  sendMessage nsCoder decodeBoolForKeySelector (toNSString key)

-- | @- decodeIntForKey:@
decodeIntForKey :: (IsNSCoder nsCoder, IsNSString key) => nsCoder -> key -> IO CInt
decodeIntForKey nsCoder key =
  sendMessage nsCoder decodeIntForKeySelector (toNSString key)

-- | @- decodeInt32ForKey:@
decodeInt32ForKey :: (IsNSCoder nsCoder, IsNSString key) => nsCoder -> key -> IO CInt
decodeInt32ForKey nsCoder key =
  sendMessage nsCoder decodeInt32ForKeySelector (toNSString key)

-- | @- decodeInt64ForKey:@
decodeInt64ForKey :: (IsNSCoder nsCoder, IsNSString key) => nsCoder -> key -> IO CLong
decodeInt64ForKey nsCoder key =
  sendMessage nsCoder decodeInt64ForKeySelector (toNSString key)

-- | @- decodeFloatForKey:@
decodeFloatForKey :: (IsNSCoder nsCoder, IsNSString key) => nsCoder -> key -> IO CFloat
decodeFloatForKey nsCoder key =
  sendMessage nsCoder decodeFloatForKeySelector (toNSString key)

-- | @- decodeDoubleForKey:@
decodeDoubleForKey :: (IsNSCoder nsCoder, IsNSString key) => nsCoder -> key -> IO CDouble
decodeDoubleForKey nsCoder key =
  sendMessage nsCoder decodeDoubleForKeySelector (toNSString key)

-- | @- decodeBytesForKey:returnedLength:@
decodeBytesForKey_returnedLength :: (IsNSCoder nsCoder, IsNSString key) => nsCoder -> key -> Ptr CULong -> IO (Const (Ptr CUChar))
decodeBytesForKey_returnedLength nsCoder key lengthp =
  sendMessage nsCoder decodeBytesForKey_returnedLengthSelector (toNSString key) lengthp

-- | Decode bytes from the decoder. The length of the bytes must be greater than or equal to the @length@ parameter. If the result exists, but is of insufficient length, then the decoder uses @failWithError@ to fail the entire decode operation. The result of that is configurable on a per-NSCoder basis using @NSDecodingFailurePolicy@.
--
-- ObjC selector: @- decodeBytesWithMinimumLength:@
decodeBytesWithMinimumLength :: IsNSCoder nsCoder => nsCoder -> CULong -> IO (Ptr ())
decodeBytesWithMinimumLength nsCoder length_ =
  sendMessage nsCoder decodeBytesWithMinimumLengthSelector length_

-- | Decode bytes from the decoder for a given key. The length of the bytes must be greater than or equal to the @length@ parameter. If the result exists, but is of insufficient length, then the decoder uses @failWithError@ to fail the entire decode operation. The result of that is configurable on a per-NSCoder basis using @NSDecodingFailurePolicy@.
--
-- ObjC selector: @- decodeBytesForKey:minimumLength:@
decodeBytesForKey_minimumLength :: (IsNSCoder nsCoder, IsNSString key) => nsCoder -> key -> CULong -> IO (Const (Ptr CUChar))
decodeBytesForKey_minimumLength nsCoder key length_ =
  sendMessage nsCoder decodeBytesForKey_minimumLengthSelector (toNSString key) length_

-- | @- encodeInteger:forKey:@
encodeInteger_forKey :: (IsNSCoder nsCoder, IsNSString key) => nsCoder -> CLong -> key -> IO ()
encodeInteger_forKey nsCoder value key =
  sendMessage nsCoder encodeInteger_forKeySelector value (toNSString key)

-- | @- decodeIntegerForKey:@
decodeIntegerForKey :: (IsNSCoder nsCoder, IsNSString key) => nsCoder -> key -> IO CLong
decodeIntegerForKey nsCoder key =
  sendMessage nsCoder decodeIntegerForKeySelector (toNSString key)

-- | @- decodeObjectOfClass:forKey:@
decodeObjectOfClass_forKey :: (IsNSCoder nsCoder, IsNSString key) => nsCoder -> Class -> key -> IO RawId
decodeObjectOfClass_forKey nsCoder aClass key =
  sendMessage nsCoder decodeObjectOfClass_forKeySelector aClass (toNSString key)

-- | @- decodeTopLevelObjectOfClass:forKey:error:@
decodeTopLevelObjectOfClass_forKey_error :: (IsNSCoder nsCoder, IsNSString key, IsNSError error_) => nsCoder -> Class -> key -> error_ -> IO RawId
decodeTopLevelObjectOfClass_forKey_error nsCoder aClass key error_ =
  sendMessage nsCoder decodeTopLevelObjectOfClass_forKey_errorSelector aClass (toNSString key) (toNSError error_)

-- | Decodes the @NSArray@ object for the given  @key,@ which should be an @NSArray<cls>,@ containing the given non-collection class (no nested arrays or arrays of dictionaries, etc) from the coder.
--
-- Requires @NSSecureCoding@ otherwise an exception is thrown and sets the @decodingFailurePolicy@ to @NSDecodingFailurePolicySetErrorAndReturn.@
--
-- Returns @nil@ if the object for @key@ is not of the expected types, or cannot be decoded, and sets the @error@ on the decoder.
--
-- ObjC selector: @- decodeArrayOfObjectsOfClass:forKey:@
decodeArrayOfObjectsOfClass_forKey :: (IsNSCoder nsCoder, IsNSString key) => nsCoder -> Class -> key -> IO (Id NSArray)
decodeArrayOfObjectsOfClass_forKey nsCoder cls key =
  sendMessage nsCoder decodeArrayOfObjectsOfClass_forKeySelector cls (toNSString key)

-- | Decodes the @NSDictionary@ object for the given @key,@ which should be an @NSDictionary<keyCls,objectCls>@ , with keys of type given in @keyCls@ and objects of the given non-collection class @objectCls@ (no nested dictionaries or other dictionaries contained in the dictionary, etc) from the coder.
--
-- Requires @NSSecureCoding@ otherwise an exception is thrown and sets the @decodingFailurePolicy@ to @NSDecodingFailurePolicySetErrorAndReturn.@
--
-- Returns @nil@ if the object for @key@ is not of the expected types, or cannot be decoded, and sets the @error@ on the decoder.
--
-- ObjC selector: @- decodeDictionaryWithKeysOfClass:objectsOfClass:forKey:@
decodeDictionaryWithKeysOfClass_objectsOfClass_forKey :: (IsNSCoder nsCoder, IsNSString key) => nsCoder -> Class -> Class -> key -> IO (Id NSDictionary)
decodeDictionaryWithKeysOfClass_objectsOfClass_forKey nsCoder keyCls objectCls key =
  sendMessage nsCoder decodeDictionaryWithKeysOfClass_objectsOfClass_forKeySelector keyCls objectCls (toNSString key)

-- | @- decodeObjectOfClasses:forKey:@
decodeObjectOfClasses_forKey :: (IsNSCoder nsCoder, IsNSSet classes, IsNSString key) => nsCoder -> classes -> key -> IO RawId
decodeObjectOfClasses_forKey nsCoder classes key =
  sendMessage nsCoder decodeObjectOfClasses_forKeySelector (toNSSet classes) (toNSString key)

-- | @- decodeTopLevelObjectOfClasses:forKey:error:@
decodeTopLevelObjectOfClasses_forKey_error :: (IsNSCoder nsCoder, IsNSSet classes, IsNSString key, IsNSError error_) => nsCoder -> classes -> key -> error_ -> IO RawId
decodeTopLevelObjectOfClasses_forKey_error nsCoder classes key error_ =
  sendMessage nsCoder decodeTopLevelObjectOfClasses_forKey_errorSelector (toNSSet classes) (toNSString key) (toNSError error_)

-- | Decodes the @NSArray@ object for the given @key,@ which should be an @NSArray,@ containing the given non-collection classes (no nested arrays or arrays of dictionaries, etc) from the coder.
--
-- Requires @NSSecureCoding@ otherwise an exception is thrown and sets the @decodingFailurePolicy@ to @NSDecodingFailurePolicySetErrorAndReturn.@
--
-- Returns @nil@ if the object for @key@ is not of the expected types, or cannot be decoded, and sets the @error@ on the decoder.
--
-- ObjC selector: @- decodeArrayOfObjectsOfClasses:forKey:@
decodeArrayOfObjectsOfClasses_forKey :: (IsNSCoder nsCoder, IsNSSet classes, IsNSString key) => nsCoder -> classes -> key -> IO (Id NSArray)
decodeArrayOfObjectsOfClasses_forKey nsCoder classes key =
  sendMessage nsCoder decodeArrayOfObjectsOfClasses_forKeySelector (toNSSet classes) (toNSString key)

-- | Decodes the @NSDictionary@ object for the given @key,@ which should be an @NSDictionary,@ with keys of the types given in @keyClasses@ and objects of the given non-collection classes in @objectClasses@ (no nested dictionaries or other dictionaries contained in the dictionary, etc) from the given coder.
--
-- Requires @NSSecureCoding@ otherwise an exception is thrown and sets the @decodingFailurePolicy@ to @NSDecodingFailurePolicySetErrorAndReturn.@
--
-- Returns @nil@ if the object for @key@ is not of the expected types, or cannot be decoded, and sets the @error@ on the decoder.
--
-- ObjC selector: @- decodeDictionaryWithKeysOfClasses:objectsOfClasses:forKey:@
decodeDictionaryWithKeysOfClasses_objectsOfClasses_forKey :: (IsNSCoder nsCoder, IsNSSet keyClasses, IsNSSet objectClasses, IsNSString key) => nsCoder -> keyClasses -> objectClasses -> key -> IO (Id NSDictionary)
decodeDictionaryWithKeysOfClasses_objectsOfClasses_forKey nsCoder keyClasses objectClasses key =
  sendMessage nsCoder decodeDictionaryWithKeysOfClasses_objectsOfClasses_forKeySelector (toNSSet keyClasses) (toNSSet objectClasses) (toNSString key)

-- | @- decodePropertyListForKey:@
decodePropertyListForKey :: (IsNSCoder nsCoder, IsNSString key) => nsCoder -> key -> IO RawId
decodePropertyListForKey nsCoder key =
  sendMessage nsCoder decodePropertyListForKeySelector (toNSString key)

-- | Signals to this coder that the decode has failed.  non-nil error that describes the reason why the decode failed
--
-- Sets an error on this NSCoder once per TopLevel decode; calling it repeatedly will have no effect until the call stack unwinds to one of the TopLevel decode entry-points.
--
-- This method is only meaningful to call for decodes.
--
-- Typically, you would want to call this method in your -initWithCoder: implementation when you detect situations like: - lack of secure coding - corruption of your data - domain validation failures
--
-- After calling -failWithError: within your -initWithCoder: implementation, you should clean up and return nil as early as possible.
--
-- Once an error has been signaled to a decoder, it remains set until it has handed off to the first TopLevel decode invocation above it.  For example, consider the following call graph: A    -decodeTopLevelObjectForKey:error: B        -initWithCoder: C            -decodeObjectForKey: D                -initWithCoder: E                    -decodeObjectForKey: F                        -failWithError:
--
-- In this case the error provided in stack-frame F will be returned via the outError in stack-frame A. Furthermore the result object from decodeTopLevelObjectForKey:error: will be nil, regardless of the result of stack-frame B.
--
-- NSCoder implementations support two mechanisms for the stack-unwinding from F to A: - forced (NSException based) - particpatory (error based)
--
-- The kind of unwinding you get is determined by the decodingFailurePolicy property of this NSCoder (which defaults to NSDecodingFailurePolicyRaiseException to match historical behavior).
--
-- ObjC selector: @- failWithError:@
failWithError :: (IsNSCoder nsCoder, IsNSError error_) => nsCoder -> error_ -> IO ()
failWithError nsCoder error_ =
  sendMessage nsCoder failWithErrorSelector (toNSError error_)

-- | @- systemVersion@
systemVersion :: IsNSCoder nsCoder => nsCoder -> IO CUInt
systemVersion nsCoder =
  sendMessage nsCoder systemVersionSelector

-- | @- allowsKeyedCoding@
allowsKeyedCoding :: IsNSCoder nsCoder => nsCoder -> IO Bool
allowsKeyedCoding nsCoder =
  sendMessage nsCoder allowsKeyedCodingSelector

-- | @- requiresSecureCoding@
requiresSecureCoding :: IsNSCoder nsCoder => nsCoder -> IO Bool
requiresSecureCoding nsCoder =
  sendMessage nsCoder requiresSecureCodingSelector

-- | @- allowedClasses@
allowedClasses :: IsNSCoder nsCoder => nsCoder -> IO (Id NSSet)
allowedClasses nsCoder =
  sendMessage nsCoder allowedClassesSelector

-- | Defines the behavior this NSCoder should take on decode failure (i.e. corrupt archive, invalid data, etc.).
--
-- The default result of this property is NSDecodingFailurePolicyRaiseException, subclasses can change this to an alternative policy.
--
-- ObjC selector: @- decodingFailurePolicy@
decodingFailurePolicy :: IsNSCoder nsCoder => nsCoder -> IO NSDecodingFailurePolicy
decodingFailurePolicy nsCoder =
  sendMessage nsCoder decodingFailurePolicySelector

-- | The current error (if there is one) for the current TopLevel decode.
--
-- The meaning of this property changes based on the result of the decodingFailurePolicy property: For NSDecodingFailurePolicyRaiseException, this property will always be nil. For NSDecodingFailurePolicySetErrorAndReturn, this property can be non-nil, and if so, indicates that there was a failure while decoding the archive (specifically its the very first error encountered).
--
-- While .error is non-nil, all attempts to decode data from this coder will return a nil/zero-equivalent value.
--
-- This error is consumed by a TopLevel decode API (which resets this coder back to a being able to potentially decode data).
--
-- ObjC selector: @- error@
error_ :: IsNSCoder nsCoder => nsCoder -> IO (Id NSError)
error_ nsCoder =
  sendMessage nsCoder errorSelector

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @encodeValueOfObjCType:at:@
encodeValueOfObjCType_atSelector :: Selector '[Const (Ptr CChar), Const (Ptr ())] ()
encodeValueOfObjCType_atSelector = mkSelector "encodeValueOfObjCType:at:"

-- | @Selector@ for @encodeDataObject:@
encodeDataObjectSelector :: Selector '[Id NSData] ()
encodeDataObjectSelector = mkSelector "encodeDataObject:"

-- | @Selector@ for @decodeDataObject@
decodeDataObjectSelector :: Selector '[] (Id NSData)
decodeDataObjectSelector = mkSelector "decodeDataObject"

-- | @Selector@ for @decodeValueOfObjCType:at:size:@
decodeValueOfObjCType_at_sizeSelector :: Selector '[Const (Ptr CChar), Ptr (), CULong] ()
decodeValueOfObjCType_at_sizeSelector = mkSelector "decodeValueOfObjCType:at:size:"

-- | @Selector@ for @versionForClassName:@
versionForClassNameSelector :: Selector '[Id NSString] CLong
versionForClassNameSelector = mkSelector "versionForClassName:"

-- | @Selector@ for @encodePoint:forKey:@
encodePoint_forKeySelector :: Selector '[NSPoint, Id NSString] ()
encodePoint_forKeySelector = mkSelector "encodePoint:forKey:"

-- | @Selector@ for @encodeSize:forKey:@
encodeSize_forKeySelector :: Selector '[NSSize, Id NSString] ()
encodeSize_forKeySelector = mkSelector "encodeSize:forKey:"

-- | @Selector@ for @encodeRect:forKey:@
encodeRect_forKeySelector :: Selector '[NSRect, Id NSString] ()
encodeRect_forKeySelector = mkSelector "encodeRect:forKey:"

-- | @Selector@ for @decodePointForKey:@
decodePointForKeySelector :: Selector '[Id NSString] NSPoint
decodePointForKeySelector = mkSelector "decodePointForKey:"

-- | @Selector@ for @decodeSizeForKey:@
decodeSizeForKeySelector :: Selector '[Id NSString] NSSize
decodeSizeForKeySelector = mkSelector "decodeSizeForKey:"

-- | @Selector@ for @decodeRectForKey:@
decodeRectForKeySelector :: Selector '[Id NSString] NSRect
decodeRectForKeySelector = mkSelector "decodeRectForKey:"

-- | @Selector@ for @encodePoint:@
encodePointSelector :: Selector '[NSPoint] ()
encodePointSelector = mkSelector "encodePoint:"

-- | @Selector@ for @decodePoint@
decodePointSelector :: Selector '[] NSPoint
decodePointSelector = mkSelector "decodePoint"

-- | @Selector@ for @encodeSize:@
encodeSizeSelector :: Selector '[NSSize] ()
encodeSizeSelector = mkSelector "encodeSize:"

-- | @Selector@ for @decodeSize@
decodeSizeSelector :: Selector '[] NSSize
decodeSizeSelector = mkSelector "decodeSize"

-- | @Selector@ for @encodeRect:@
encodeRectSelector :: Selector '[NSRect] ()
encodeRectSelector = mkSelector "encodeRect:"

-- | @Selector@ for @decodeRect@
decodeRectSelector :: Selector '[] NSRect
decodeRectSelector = mkSelector "decodeRect"

-- | @Selector@ for @decodeValueOfObjCType:at:@
decodeValueOfObjCType_atSelector :: Selector '[Const (Ptr CChar), Ptr ()] ()
decodeValueOfObjCType_atSelector = mkSelector "decodeValueOfObjCType:at:"

-- | @Selector@ for @encodeNXObject:@
encodeNXObjectSelector :: Selector '[RawId] ()
encodeNXObjectSelector = mkSelector "encodeNXObject:"

-- | @Selector@ for @decodeNXObject@
decodeNXObjectSelector :: Selector '[] RawId
decodeNXObjectSelector = mkSelector "decodeNXObject"

-- | @Selector@ for @encodeObject:@
encodeObjectSelector :: Selector '[RawId] ()
encodeObjectSelector = mkSelector "encodeObject:"

-- | @Selector@ for @encodeRootObject:@
encodeRootObjectSelector :: Selector '[RawId] ()
encodeRootObjectSelector = mkSelector "encodeRootObject:"

-- | @Selector@ for @encodeBycopyObject:@
encodeBycopyObjectSelector :: Selector '[RawId] ()
encodeBycopyObjectSelector = mkSelector "encodeBycopyObject:"

-- | @Selector@ for @encodeByrefObject:@
encodeByrefObjectSelector :: Selector '[RawId] ()
encodeByrefObjectSelector = mkSelector "encodeByrefObject:"

-- | @Selector@ for @encodeConditionalObject:@
encodeConditionalObjectSelector :: Selector '[RawId] ()
encodeConditionalObjectSelector = mkSelector "encodeConditionalObject:"

-- | @Selector@ for @encodeValuesOfObjCTypes:@
encodeValuesOfObjCTypesSelector :: Selector '[Const (Ptr CChar)] ()
encodeValuesOfObjCTypesSelector = mkSelector "encodeValuesOfObjCTypes:"

-- | @Selector@ for @encodeArrayOfObjCType:count:at:@
encodeArrayOfObjCType_count_atSelector :: Selector '[Const (Ptr CChar), CULong, Const (Ptr ())] ()
encodeArrayOfObjCType_count_atSelector = mkSelector "encodeArrayOfObjCType:count:at:"

-- | @Selector@ for @encodeBytes:length:@
encodeBytes_lengthSelector :: Selector '[Const (Ptr ()), CULong] ()
encodeBytes_lengthSelector = mkSelector "encodeBytes:length:"

-- | @Selector@ for @decodeObject@
decodeObjectSelector :: Selector '[] RawId
decodeObjectSelector = mkSelector "decodeObject"

-- | @Selector@ for @decodeTopLevelObjectAndReturnError:@
decodeTopLevelObjectAndReturnErrorSelector :: Selector '[Id NSError] RawId
decodeTopLevelObjectAndReturnErrorSelector = mkSelector "decodeTopLevelObjectAndReturnError:"

-- | @Selector@ for @decodeValuesOfObjCTypes:@
decodeValuesOfObjCTypesSelector :: Selector '[Const (Ptr CChar)] ()
decodeValuesOfObjCTypesSelector = mkSelector "decodeValuesOfObjCTypes:"

-- | @Selector@ for @decodeArrayOfObjCType:count:at:@
decodeArrayOfObjCType_count_atSelector :: Selector '[Const (Ptr CChar), CULong, Ptr ()] ()
decodeArrayOfObjCType_count_atSelector = mkSelector "decodeArrayOfObjCType:count:at:"

-- | @Selector@ for @decodeBytesWithReturnedLength:@
decodeBytesWithReturnedLengthSelector :: Selector '[Ptr CULong] (Ptr ())
decodeBytesWithReturnedLengthSelector = mkSelector "decodeBytesWithReturnedLength:"

-- | @Selector@ for @encodePropertyList:@
encodePropertyListSelector :: Selector '[RawId] ()
encodePropertyListSelector = mkSelector "encodePropertyList:"

-- | @Selector@ for @decodePropertyList@
decodePropertyListSelector :: Selector '[] RawId
decodePropertyListSelector = mkSelector "decodePropertyList"

-- | @Selector@ for @setObjectZone:@
setObjectZoneSelector :: Selector '[Ptr ()] ()
setObjectZoneSelector = mkSelector "setObjectZone:"

-- | @Selector@ for @objectZone@
objectZoneSelector :: Selector '[] (Ptr ())
objectZoneSelector = mkSelector "objectZone"

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

-- | @Selector@ for @containsValueForKey:@
containsValueForKeySelector :: Selector '[Id NSString] Bool
containsValueForKeySelector = mkSelector "containsValueForKey:"

-- | @Selector@ for @decodeObjectForKey:@
decodeObjectForKeySelector :: Selector '[Id NSString] RawId
decodeObjectForKeySelector = mkSelector "decodeObjectForKey:"

-- | @Selector@ for @decodeTopLevelObjectForKey:error:@
decodeTopLevelObjectForKey_errorSelector :: Selector '[Id NSString, Id NSError] RawId
decodeTopLevelObjectForKey_errorSelector = mkSelector "decodeTopLevelObjectForKey:error:"

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

-- | @Selector@ for @decodeBytesWithMinimumLength:@
decodeBytesWithMinimumLengthSelector :: Selector '[CULong] (Ptr ())
decodeBytesWithMinimumLengthSelector = mkSelector "decodeBytesWithMinimumLength:"

-- | @Selector@ for @decodeBytesForKey:minimumLength:@
decodeBytesForKey_minimumLengthSelector :: Selector '[Id NSString, CULong] (Const (Ptr CUChar))
decodeBytesForKey_minimumLengthSelector = mkSelector "decodeBytesForKey:minimumLength:"

-- | @Selector@ for @encodeInteger:forKey:@
encodeInteger_forKeySelector :: Selector '[CLong, Id NSString] ()
encodeInteger_forKeySelector = mkSelector "encodeInteger:forKey:"

-- | @Selector@ for @decodeIntegerForKey:@
decodeIntegerForKeySelector :: Selector '[Id NSString] CLong
decodeIntegerForKeySelector = mkSelector "decodeIntegerForKey:"

-- | @Selector@ for @decodeObjectOfClass:forKey:@
decodeObjectOfClass_forKeySelector :: Selector '[Class, Id NSString] RawId
decodeObjectOfClass_forKeySelector = mkSelector "decodeObjectOfClass:forKey:"

-- | @Selector@ for @decodeTopLevelObjectOfClass:forKey:error:@
decodeTopLevelObjectOfClass_forKey_errorSelector :: Selector '[Class, Id NSString, Id NSError] RawId
decodeTopLevelObjectOfClass_forKey_errorSelector = mkSelector "decodeTopLevelObjectOfClass:forKey:error:"

-- | @Selector@ for @decodeArrayOfObjectsOfClass:forKey:@
decodeArrayOfObjectsOfClass_forKeySelector :: Selector '[Class, Id NSString] (Id NSArray)
decodeArrayOfObjectsOfClass_forKeySelector = mkSelector "decodeArrayOfObjectsOfClass:forKey:"

-- | @Selector@ for @decodeDictionaryWithKeysOfClass:objectsOfClass:forKey:@
decodeDictionaryWithKeysOfClass_objectsOfClass_forKeySelector :: Selector '[Class, Class, Id NSString] (Id NSDictionary)
decodeDictionaryWithKeysOfClass_objectsOfClass_forKeySelector = mkSelector "decodeDictionaryWithKeysOfClass:objectsOfClass:forKey:"

-- | @Selector@ for @decodeObjectOfClasses:forKey:@
decodeObjectOfClasses_forKeySelector :: Selector '[Id NSSet, Id NSString] RawId
decodeObjectOfClasses_forKeySelector = mkSelector "decodeObjectOfClasses:forKey:"

-- | @Selector@ for @decodeTopLevelObjectOfClasses:forKey:error:@
decodeTopLevelObjectOfClasses_forKey_errorSelector :: Selector '[Id NSSet, Id NSString, Id NSError] RawId
decodeTopLevelObjectOfClasses_forKey_errorSelector = mkSelector "decodeTopLevelObjectOfClasses:forKey:error:"

-- | @Selector@ for @decodeArrayOfObjectsOfClasses:forKey:@
decodeArrayOfObjectsOfClasses_forKeySelector :: Selector '[Id NSSet, Id NSString] (Id NSArray)
decodeArrayOfObjectsOfClasses_forKeySelector = mkSelector "decodeArrayOfObjectsOfClasses:forKey:"

-- | @Selector@ for @decodeDictionaryWithKeysOfClasses:objectsOfClasses:forKey:@
decodeDictionaryWithKeysOfClasses_objectsOfClasses_forKeySelector :: Selector '[Id NSSet, Id NSSet, Id NSString] (Id NSDictionary)
decodeDictionaryWithKeysOfClasses_objectsOfClasses_forKeySelector = mkSelector "decodeDictionaryWithKeysOfClasses:objectsOfClasses:forKey:"

-- | @Selector@ for @decodePropertyListForKey:@
decodePropertyListForKeySelector :: Selector '[Id NSString] RawId
decodePropertyListForKeySelector = mkSelector "decodePropertyListForKey:"

-- | @Selector@ for @failWithError:@
failWithErrorSelector :: Selector '[Id NSError] ()
failWithErrorSelector = mkSelector "failWithError:"

-- | @Selector@ for @systemVersion@
systemVersionSelector :: Selector '[] CUInt
systemVersionSelector = mkSelector "systemVersion"

-- | @Selector@ for @allowsKeyedCoding@
allowsKeyedCodingSelector :: Selector '[] Bool
allowsKeyedCodingSelector = mkSelector "allowsKeyedCoding"

-- | @Selector@ for @requiresSecureCoding@
requiresSecureCodingSelector :: Selector '[] Bool
requiresSecureCodingSelector = mkSelector "requiresSecureCoding"

-- | @Selector@ for @allowedClasses@
allowedClassesSelector :: Selector '[] (Id NSSet)
allowedClassesSelector = mkSelector "allowedClasses"

-- | @Selector@ for @decodingFailurePolicy@
decodingFailurePolicySelector :: Selector '[] NSDecodingFailurePolicy
decodingFailurePolicySelector = mkSelector "decodingFailurePolicy"

-- | @Selector@ for @error@
errorSelector :: Selector '[] (Id NSError)
errorSelector = mkSelector "error"

