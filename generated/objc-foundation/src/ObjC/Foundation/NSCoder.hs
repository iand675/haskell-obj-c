{-# LANGUAGE PatternSynonyms #-}
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
  , encodeValueOfObjCType_atSelector
  , encodeDataObjectSelector
  , decodeDataObjectSelector
  , decodeValueOfObjCType_at_sizeSelector
  , versionForClassNameSelector
  , encodePoint_forKeySelector
  , encodeSize_forKeySelector
  , encodeRect_forKeySelector
  , decodePointForKeySelector
  , decodeSizeForKeySelector
  , decodeRectForKeySelector
  , encodePointSelector
  , decodePointSelector
  , encodeSizeSelector
  , decodeSizeSelector
  , encodeRectSelector
  , decodeRectSelector
  , decodeValueOfObjCType_atSelector
  , encodeNXObjectSelector
  , decodeNXObjectSelector
  , encodeObjectSelector
  , encodeRootObjectSelector
  , encodeBycopyObjectSelector
  , encodeByrefObjectSelector
  , encodeConditionalObjectSelector
  , encodeValuesOfObjCTypesSelector
  , encodeArrayOfObjCType_count_atSelector
  , encodeBytes_lengthSelector
  , decodeObjectSelector
  , decodeTopLevelObjectAndReturnErrorSelector
  , decodeValuesOfObjCTypesSelector
  , decodeArrayOfObjCType_count_atSelector
  , decodeBytesWithReturnedLengthSelector
  , encodePropertyListSelector
  , decodePropertyListSelector
  , setObjectZoneSelector
  , objectZoneSelector
  , encodeObject_forKeySelector
  , encodeConditionalObject_forKeySelector
  , encodeBool_forKeySelector
  , encodeInt_forKeySelector
  , encodeInt32_forKeySelector
  , encodeInt64_forKeySelector
  , encodeFloat_forKeySelector
  , encodeDouble_forKeySelector
  , encodeBytes_length_forKeySelector
  , containsValueForKeySelector
  , decodeObjectForKeySelector
  , decodeTopLevelObjectForKey_errorSelector
  , decodeBoolForKeySelector
  , decodeIntForKeySelector
  , decodeInt32ForKeySelector
  , decodeInt64ForKeySelector
  , decodeFloatForKeySelector
  , decodeDoubleForKeySelector
  , decodeBytesForKey_returnedLengthSelector
  , decodeBytesWithMinimumLengthSelector
  , decodeBytesForKey_minimumLengthSelector
  , encodeInteger_forKeySelector
  , decodeIntegerForKeySelector
  , decodeObjectOfClass_forKeySelector
  , decodeTopLevelObjectOfClass_forKey_errorSelector
  , decodeArrayOfObjectsOfClass_forKeySelector
  , decodeDictionaryWithKeysOfClass_objectsOfClass_forKeySelector
  , decodeObjectOfClasses_forKeySelector
  , decodeTopLevelObjectOfClasses_forKey_errorSelector
  , decodeArrayOfObjectsOfClasses_forKeySelector
  , decodeDictionaryWithKeysOfClasses_objectsOfClasses_forKeySelector
  , decodePropertyListForKeySelector
  , failWithErrorSelector
  , systemVersionSelector
  , allowsKeyedCodingSelector
  , requiresSecureCodingSelector
  , allowedClassesSelector
  , decodingFailurePolicySelector
  , errorSelector

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
import ObjC.Runtime.MsgSend (sendMsg, sendClassMsg, sendMsgStret, sendClassMsgStret)
import ObjC.Runtime.Selector (mkSelector)
import ObjC.Runtime.Class (getRequiredClass)

import ObjC.Foundation.Internal.Classes
import ObjC.Foundation.Internal.Structs
import ObjC.Foundation.Internal.Enums

-- | @- encodeValueOfObjCType:at:@
encodeValueOfObjCType_at :: IsNSCoder nsCoder => nsCoder -> Const (Ptr CChar) -> Const (Ptr ()) -> IO ()
encodeValueOfObjCType_at nsCoder  type_ addr =
  sendMsg nsCoder (mkSelector "encodeValueOfObjCType:at:") retVoid [argPtr (unConst type_), argPtr (unConst addr)]

-- | @- encodeDataObject:@
encodeDataObject :: (IsNSCoder nsCoder, IsNSData data_) => nsCoder -> data_ -> IO ()
encodeDataObject nsCoder  data_ =
withObjCPtr data_ $ \raw_data_ ->
    sendMsg nsCoder (mkSelector "encodeDataObject:") retVoid [argPtr (castPtr raw_data_ :: Ptr ())]

-- | @- decodeDataObject@
decodeDataObject :: IsNSCoder nsCoder => nsCoder -> IO (Id NSData)
decodeDataObject nsCoder  =
  sendMsg nsCoder (mkSelector "decodeDataObject") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- decodeValueOfObjCType:at:size:@
decodeValueOfObjCType_at_size :: IsNSCoder nsCoder => nsCoder -> Const (Ptr CChar) -> Ptr () -> CULong -> IO ()
decodeValueOfObjCType_at_size nsCoder  type_ data_ size =
  sendMsg nsCoder (mkSelector "decodeValueOfObjCType:at:size:") retVoid [argPtr (unConst type_), argPtr data_, argCULong (fromIntegral size)]

-- | @- versionForClassName:@
versionForClassName :: (IsNSCoder nsCoder, IsNSString className) => nsCoder -> className -> IO CLong
versionForClassName nsCoder  className =
withObjCPtr className $ \raw_className ->
    sendMsg nsCoder (mkSelector "versionForClassName:") retCLong [argPtr (castPtr raw_className :: Ptr ())]

-- | @- encodePoint:forKey:@
encodePoint_forKey :: (IsNSCoder nsCoder, IsNSString key) => nsCoder -> NSPoint -> key -> IO ()
encodePoint_forKey nsCoder  point key =
withObjCPtr key $ \raw_key ->
    sendMsg nsCoder (mkSelector "encodePoint:forKey:") retVoid [argNSPoint point, argPtr (castPtr raw_key :: Ptr ())]

-- | @- encodeSize:forKey:@
encodeSize_forKey :: (IsNSCoder nsCoder, IsNSString key) => nsCoder -> NSSize -> key -> IO ()
encodeSize_forKey nsCoder  size key =
withObjCPtr key $ \raw_key ->
    sendMsg nsCoder (mkSelector "encodeSize:forKey:") retVoid [argNSSize size, argPtr (castPtr raw_key :: Ptr ())]

-- | @- encodeRect:forKey:@
encodeRect_forKey :: (IsNSCoder nsCoder, IsNSString key) => nsCoder -> NSRect -> key -> IO ()
encodeRect_forKey nsCoder  rect key =
withObjCPtr key $ \raw_key ->
    sendMsg nsCoder (mkSelector "encodeRect:forKey:") retVoid [argNSRect rect, argPtr (castPtr raw_key :: Ptr ())]

-- | @- decodePointForKey:@
decodePointForKey :: (IsNSCoder nsCoder, IsNSString key) => nsCoder -> key -> IO NSPoint
decodePointForKey nsCoder  key =
withObjCPtr key $ \raw_key ->
    sendMsgStret nsCoder (mkSelector "decodePointForKey:") retNSPoint [argPtr (castPtr raw_key :: Ptr ())]

-- | @- decodeSizeForKey:@
decodeSizeForKey :: (IsNSCoder nsCoder, IsNSString key) => nsCoder -> key -> IO NSSize
decodeSizeForKey nsCoder  key =
withObjCPtr key $ \raw_key ->
    sendMsgStret nsCoder (mkSelector "decodeSizeForKey:") retNSSize [argPtr (castPtr raw_key :: Ptr ())]

-- | @- decodeRectForKey:@
decodeRectForKey :: (IsNSCoder nsCoder, IsNSString key) => nsCoder -> key -> IO NSRect
decodeRectForKey nsCoder  key =
withObjCPtr key $ \raw_key ->
    sendMsgStret nsCoder (mkSelector "decodeRectForKey:") retNSRect [argPtr (castPtr raw_key :: Ptr ())]

-- | @- encodePoint:@
encodePoint :: IsNSCoder nsCoder => nsCoder -> NSPoint -> IO ()
encodePoint nsCoder  point =
  sendMsg nsCoder (mkSelector "encodePoint:") retVoid [argNSPoint point]

-- | @- decodePoint@
decodePoint :: IsNSCoder nsCoder => nsCoder -> IO NSPoint
decodePoint nsCoder  =
  sendMsgStret nsCoder (mkSelector "decodePoint") retNSPoint []

-- | @- encodeSize:@
encodeSize :: IsNSCoder nsCoder => nsCoder -> NSSize -> IO ()
encodeSize nsCoder  size =
  sendMsg nsCoder (mkSelector "encodeSize:") retVoid [argNSSize size]

-- | @- decodeSize@
decodeSize :: IsNSCoder nsCoder => nsCoder -> IO NSSize
decodeSize nsCoder  =
  sendMsgStret nsCoder (mkSelector "decodeSize") retNSSize []

-- | @- encodeRect:@
encodeRect :: IsNSCoder nsCoder => nsCoder -> NSRect -> IO ()
encodeRect nsCoder  rect =
  sendMsg nsCoder (mkSelector "encodeRect:") retVoid [argNSRect rect]

-- | @- decodeRect@
decodeRect :: IsNSCoder nsCoder => nsCoder -> IO NSRect
decodeRect nsCoder  =
  sendMsgStret nsCoder (mkSelector "decodeRect") retNSRect []

-- | @- decodeValueOfObjCType:at:@
decodeValueOfObjCType_at :: IsNSCoder nsCoder => nsCoder -> Const (Ptr CChar) -> Ptr () -> IO ()
decodeValueOfObjCType_at nsCoder  type_ data_ =
  sendMsg nsCoder (mkSelector "decodeValueOfObjCType:at:") retVoid [argPtr (unConst type_), argPtr data_]

-- | @- encodeNXObject:@
encodeNXObject :: IsNSCoder nsCoder => nsCoder -> RawId -> IO ()
encodeNXObject nsCoder  object =
  sendMsg nsCoder (mkSelector "encodeNXObject:") retVoid [argPtr (castPtr (unRawId object) :: Ptr ())]

-- | @- decodeNXObject@
decodeNXObject :: IsNSCoder nsCoder => nsCoder -> IO RawId
decodeNXObject nsCoder  =
  fmap (RawId . castPtr) $ sendMsg nsCoder (mkSelector "decodeNXObject") (retPtr retVoid) []

-- | @- encodeObject:@
encodeObject :: IsNSCoder nsCoder => nsCoder -> RawId -> IO ()
encodeObject nsCoder  object =
  sendMsg nsCoder (mkSelector "encodeObject:") retVoid [argPtr (castPtr (unRawId object) :: Ptr ())]

-- | @- encodeRootObject:@
encodeRootObject :: IsNSCoder nsCoder => nsCoder -> RawId -> IO ()
encodeRootObject nsCoder  rootObject =
  sendMsg nsCoder (mkSelector "encodeRootObject:") retVoid [argPtr (castPtr (unRawId rootObject) :: Ptr ())]

-- | @- encodeBycopyObject:@
encodeBycopyObject :: IsNSCoder nsCoder => nsCoder -> RawId -> IO ()
encodeBycopyObject nsCoder  anObject =
  sendMsg nsCoder (mkSelector "encodeBycopyObject:") retVoid [argPtr (castPtr (unRawId anObject) :: Ptr ())]

-- | @- encodeByrefObject:@
encodeByrefObject :: IsNSCoder nsCoder => nsCoder -> RawId -> IO ()
encodeByrefObject nsCoder  anObject =
  sendMsg nsCoder (mkSelector "encodeByrefObject:") retVoid [argPtr (castPtr (unRawId anObject) :: Ptr ())]

-- | @- encodeConditionalObject:@
encodeConditionalObject :: IsNSCoder nsCoder => nsCoder -> RawId -> IO ()
encodeConditionalObject nsCoder  object =
  sendMsg nsCoder (mkSelector "encodeConditionalObject:") retVoid [argPtr (castPtr (unRawId object) :: Ptr ())]

-- | @- encodeValuesOfObjCTypes:@
encodeValuesOfObjCTypes :: IsNSCoder nsCoder => nsCoder -> Const (Ptr CChar) -> IO ()
encodeValuesOfObjCTypes nsCoder  types =
  sendMsg nsCoder (mkSelector "encodeValuesOfObjCTypes:") retVoid [argPtr (unConst types)]

-- | @- encodeArrayOfObjCType:count:at:@
encodeArrayOfObjCType_count_at :: IsNSCoder nsCoder => nsCoder -> Const (Ptr CChar) -> CULong -> Const (Ptr ()) -> IO ()
encodeArrayOfObjCType_count_at nsCoder  type_ count array =
  sendMsg nsCoder (mkSelector "encodeArrayOfObjCType:count:at:") retVoid [argPtr (unConst type_), argCULong (fromIntegral count), argPtr (unConst array)]

-- | @- encodeBytes:length:@
encodeBytes_length :: IsNSCoder nsCoder => nsCoder -> Const (Ptr ()) -> CULong -> IO ()
encodeBytes_length nsCoder  byteaddr length_ =
  sendMsg nsCoder (mkSelector "encodeBytes:length:") retVoid [argPtr (unConst byteaddr), argCULong (fromIntegral length_)]

-- | @- decodeObject@
decodeObject :: IsNSCoder nsCoder => nsCoder -> IO RawId
decodeObject nsCoder  =
  fmap (RawId . castPtr) $ sendMsg nsCoder (mkSelector "decodeObject") (retPtr retVoid) []

-- | @- decodeTopLevelObjectAndReturnError:@
decodeTopLevelObjectAndReturnError :: (IsNSCoder nsCoder, IsNSError error_) => nsCoder -> error_ -> IO RawId
decodeTopLevelObjectAndReturnError nsCoder  error_ =
withObjCPtr error_ $ \raw_error_ ->
    fmap (RawId . castPtr) $ sendMsg nsCoder (mkSelector "decodeTopLevelObjectAndReturnError:") (retPtr retVoid) [argPtr (castPtr raw_error_ :: Ptr ())]

-- | @- decodeValuesOfObjCTypes:@
decodeValuesOfObjCTypes :: IsNSCoder nsCoder => nsCoder -> Const (Ptr CChar) -> IO ()
decodeValuesOfObjCTypes nsCoder  types =
  sendMsg nsCoder (mkSelector "decodeValuesOfObjCTypes:") retVoid [argPtr (unConst types)]

-- | @- decodeArrayOfObjCType:count:at:@
decodeArrayOfObjCType_count_at :: IsNSCoder nsCoder => nsCoder -> Const (Ptr CChar) -> CULong -> Ptr () -> IO ()
decodeArrayOfObjCType_count_at nsCoder  itemType count array =
  sendMsg nsCoder (mkSelector "decodeArrayOfObjCType:count:at:") retVoid [argPtr (unConst itemType), argCULong (fromIntegral count), argPtr array]

-- | @- decodeBytesWithReturnedLength:@
decodeBytesWithReturnedLength :: IsNSCoder nsCoder => nsCoder -> Ptr CULong -> IO (Ptr ())
decodeBytesWithReturnedLength nsCoder  lengthp =
  fmap castPtr $ sendMsg nsCoder (mkSelector "decodeBytesWithReturnedLength:") (retPtr retVoid) [argPtr lengthp]

-- | @- encodePropertyList:@
encodePropertyList :: IsNSCoder nsCoder => nsCoder -> RawId -> IO ()
encodePropertyList nsCoder  aPropertyList =
  sendMsg nsCoder (mkSelector "encodePropertyList:") retVoid [argPtr (castPtr (unRawId aPropertyList) :: Ptr ())]

-- | @- decodePropertyList@
decodePropertyList :: IsNSCoder nsCoder => nsCoder -> IO RawId
decodePropertyList nsCoder  =
  fmap (RawId . castPtr) $ sendMsg nsCoder (mkSelector "decodePropertyList") (retPtr retVoid) []

-- | @- setObjectZone:@
setObjectZone :: IsNSCoder nsCoder => nsCoder -> Ptr () -> IO ()
setObjectZone nsCoder  zone =
  sendMsg nsCoder (mkSelector "setObjectZone:") retVoid [argPtr zone]

-- | @- objectZone@
objectZone :: IsNSCoder nsCoder => nsCoder -> IO (Ptr ())
objectZone nsCoder  =
  fmap castPtr $ sendMsg nsCoder (mkSelector "objectZone") (retPtr retVoid) []

-- | @- encodeObject:forKey:@
encodeObject_forKey :: (IsNSCoder nsCoder, IsNSString key) => nsCoder -> RawId -> key -> IO ()
encodeObject_forKey nsCoder  object key =
withObjCPtr key $ \raw_key ->
    sendMsg nsCoder (mkSelector "encodeObject:forKey:") retVoid [argPtr (castPtr (unRawId object) :: Ptr ()), argPtr (castPtr raw_key :: Ptr ())]

-- | @- encodeConditionalObject:forKey:@
encodeConditionalObject_forKey :: (IsNSCoder nsCoder, IsNSString key) => nsCoder -> RawId -> key -> IO ()
encodeConditionalObject_forKey nsCoder  object key =
withObjCPtr key $ \raw_key ->
    sendMsg nsCoder (mkSelector "encodeConditionalObject:forKey:") retVoid [argPtr (castPtr (unRawId object) :: Ptr ()), argPtr (castPtr raw_key :: Ptr ())]

-- | @- encodeBool:forKey:@
encodeBool_forKey :: (IsNSCoder nsCoder, IsNSString key) => nsCoder -> Bool -> key -> IO ()
encodeBool_forKey nsCoder  value key =
withObjCPtr key $ \raw_key ->
    sendMsg nsCoder (mkSelector "encodeBool:forKey:") retVoid [argCULong (if value then 1 else 0), argPtr (castPtr raw_key :: Ptr ())]

-- | @- encodeInt:forKey:@
encodeInt_forKey :: (IsNSCoder nsCoder, IsNSString key) => nsCoder -> CInt -> key -> IO ()
encodeInt_forKey nsCoder  value key =
withObjCPtr key $ \raw_key ->
    sendMsg nsCoder (mkSelector "encodeInt:forKey:") retVoid [argCInt (fromIntegral value), argPtr (castPtr raw_key :: Ptr ())]

-- | @- encodeInt32:forKey:@
encodeInt32_forKey :: (IsNSCoder nsCoder, IsNSString key) => nsCoder -> CInt -> key -> IO ()
encodeInt32_forKey nsCoder  value key =
withObjCPtr key $ \raw_key ->
    sendMsg nsCoder (mkSelector "encodeInt32:forKey:") retVoid [argCInt (fromIntegral value), argPtr (castPtr raw_key :: Ptr ())]

-- | @- encodeInt64:forKey:@
encodeInt64_forKey :: (IsNSCoder nsCoder, IsNSString key) => nsCoder -> CLong -> key -> IO ()
encodeInt64_forKey nsCoder  value key =
withObjCPtr key $ \raw_key ->
    sendMsg nsCoder (mkSelector "encodeInt64:forKey:") retVoid [argCLong (fromIntegral value), argPtr (castPtr raw_key :: Ptr ())]

-- | @- encodeFloat:forKey:@
encodeFloat_forKey :: (IsNSCoder nsCoder, IsNSString key) => nsCoder -> CFloat -> key -> IO ()
encodeFloat_forKey nsCoder  value key =
withObjCPtr key $ \raw_key ->
    sendMsg nsCoder (mkSelector "encodeFloat:forKey:") retVoid [argCFloat (fromIntegral value), argPtr (castPtr raw_key :: Ptr ())]

-- | @- encodeDouble:forKey:@
encodeDouble_forKey :: (IsNSCoder nsCoder, IsNSString key) => nsCoder -> CDouble -> key -> IO ()
encodeDouble_forKey nsCoder  value key =
withObjCPtr key $ \raw_key ->
    sendMsg nsCoder (mkSelector "encodeDouble:forKey:") retVoid [argCDouble (fromIntegral value), argPtr (castPtr raw_key :: Ptr ())]

-- | @- encodeBytes:length:forKey:@
encodeBytes_length_forKey :: (IsNSCoder nsCoder, IsNSString key) => nsCoder -> Const (Ptr CUChar) -> CULong -> key -> IO ()
encodeBytes_length_forKey nsCoder  bytes length_ key =
withObjCPtr key $ \raw_key ->
    sendMsg nsCoder (mkSelector "encodeBytes:length:forKey:") retVoid [argPtr (unConst bytes), argCULong (fromIntegral length_), argPtr (castPtr raw_key :: Ptr ())]

-- | @- containsValueForKey:@
containsValueForKey :: (IsNSCoder nsCoder, IsNSString key) => nsCoder -> key -> IO Bool
containsValueForKey nsCoder  key =
withObjCPtr key $ \raw_key ->
    fmap ((/= 0) :: CULong -> Bool) $ sendMsg nsCoder (mkSelector "containsValueForKey:") retCULong [argPtr (castPtr raw_key :: Ptr ())]

-- | @- decodeObjectForKey:@
decodeObjectForKey :: (IsNSCoder nsCoder, IsNSString key) => nsCoder -> key -> IO RawId
decodeObjectForKey nsCoder  key =
withObjCPtr key $ \raw_key ->
    fmap (RawId . castPtr) $ sendMsg nsCoder (mkSelector "decodeObjectForKey:") (retPtr retVoid) [argPtr (castPtr raw_key :: Ptr ())]

-- | @- decodeTopLevelObjectForKey:error:@
decodeTopLevelObjectForKey_error :: (IsNSCoder nsCoder, IsNSString key, IsNSError error_) => nsCoder -> key -> error_ -> IO RawId
decodeTopLevelObjectForKey_error nsCoder  key error_ =
withObjCPtr key $ \raw_key ->
  withObjCPtr error_ $ \raw_error_ ->
      fmap (RawId . castPtr) $ sendMsg nsCoder (mkSelector "decodeTopLevelObjectForKey:error:") (retPtr retVoid) [argPtr (castPtr raw_key :: Ptr ()), argPtr (castPtr raw_error_ :: Ptr ())]

-- | @- decodeBoolForKey:@
decodeBoolForKey :: (IsNSCoder nsCoder, IsNSString key) => nsCoder -> key -> IO Bool
decodeBoolForKey nsCoder  key =
withObjCPtr key $ \raw_key ->
    fmap ((/= 0) :: CULong -> Bool) $ sendMsg nsCoder (mkSelector "decodeBoolForKey:") retCULong [argPtr (castPtr raw_key :: Ptr ())]

-- | @- decodeIntForKey:@
decodeIntForKey :: (IsNSCoder nsCoder, IsNSString key) => nsCoder -> key -> IO CInt
decodeIntForKey nsCoder  key =
withObjCPtr key $ \raw_key ->
    sendMsg nsCoder (mkSelector "decodeIntForKey:") retCInt [argPtr (castPtr raw_key :: Ptr ())]

-- | @- decodeInt32ForKey:@
decodeInt32ForKey :: (IsNSCoder nsCoder, IsNSString key) => nsCoder -> key -> IO CInt
decodeInt32ForKey nsCoder  key =
withObjCPtr key $ \raw_key ->
    sendMsg nsCoder (mkSelector "decodeInt32ForKey:") retCInt [argPtr (castPtr raw_key :: Ptr ())]

-- | @- decodeInt64ForKey:@
decodeInt64ForKey :: (IsNSCoder nsCoder, IsNSString key) => nsCoder -> key -> IO CLong
decodeInt64ForKey nsCoder  key =
withObjCPtr key $ \raw_key ->
    sendMsg nsCoder (mkSelector "decodeInt64ForKey:") retCLong [argPtr (castPtr raw_key :: Ptr ())]

-- | @- decodeFloatForKey:@
decodeFloatForKey :: (IsNSCoder nsCoder, IsNSString key) => nsCoder -> key -> IO CFloat
decodeFloatForKey nsCoder  key =
withObjCPtr key $ \raw_key ->
    sendMsg nsCoder (mkSelector "decodeFloatForKey:") retCFloat [argPtr (castPtr raw_key :: Ptr ())]

-- | @- decodeDoubleForKey:@
decodeDoubleForKey :: (IsNSCoder nsCoder, IsNSString key) => nsCoder -> key -> IO CDouble
decodeDoubleForKey nsCoder  key =
withObjCPtr key $ \raw_key ->
    sendMsg nsCoder (mkSelector "decodeDoubleForKey:") retCDouble [argPtr (castPtr raw_key :: Ptr ())]

-- | @- decodeBytesForKey:returnedLength:@
decodeBytesForKey_returnedLength :: (IsNSCoder nsCoder, IsNSString key) => nsCoder -> key -> Ptr CULong -> IO (Const (Ptr CUChar))
decodeBytesForKey_returnedLength nsCoder  key lengthp =
withObjCPtr key $ \raw_key ->
    fmap Const $ fmap castPtr $ sendMsg nsCoder (mkSelector "decodeBytesForKey:returnedLength:") (retPtr retVoid) [argPtr (castPtr raw_key :: Ptr ()), argPtr lengthp]

-- | Decode bytes from the decoder. The length of the bytes must be greater than or equal to the @length@ parameter. If the result exists, but is of insufficient length, then the decoder uses @failWithError@ to fail the entire decode operation. The result of that is configurable on a per-NSCoder basis using @NSDecodingFailurePolicy@.
--
-- ObjC selector: @- decodeBytesWithMinimumLength:@
decodeBytesWithMinimumLength :: IsNSCoder nsCoder => nsCoder -> CULong -> IO (Ptr ())
decodeBytesWithMinimumLength nsCoder  length_ =
  fmap castPtr $ sendMsg nsCoder (mkSelector "decodeBytesWithMinimumLength:") (retPtr retVoid) [argCULong (fromIntegral length_)]

-- | Decode bytes from the decoder for a given key. The length of the bytes must be greater than or equal to the @length@ parameter. If the result exists, but is of insufficient length, then the decoder uses @failWithError@ to fail the entire decode operation. The result of that is configurable on a per-NSCoder basis using @NSDecodingFailurePolicy@.
--
-- ObjC selector: @- decodeBytesForKey:minimumLength:@
decodeBytesForKey_minimumLength :: (IsNSCoder nsCoder, IsNSString key) => nsCoder -> key -> CULong -> IO (Const (Ptr CUChar))
decodeBytesForKey_minimumLength nsCoder  key length_ =
withObjCPtr key $ \raw_key ->
    fmap Const $ fmap castPtr $ sendMsg nsCoder (mkSelector "decodeBytesForKey:minimumLength:") (retPtr retVoid) [argPtr (castPtr raw_key :: Ptr ()), argCULong (fromIntegral length_)]

-- | @- encodeInteger:forKey:@
encodeInteger_forKey :: (IsNSCoder nsCoder, IsNSString key) => nsCoder -> CLong -> key -> IO ()
encodeInteger_forKey nsCoder  value key =
withObjCPtr key $ \raw_key ->
    sendMsg nsCoder (mkSelector "encodeInteger:forKey:") retVoid [argCLong (fromIntegral value), argPtr (castPtr raw_key :: Ptr ())]

-- | @- decodeIntegerForKey:@
decodeIntegerForKey :: (IsNSCoder nsCoder, IsNSString key) => nsCoder -> key -> IO CLong
decodeIntegerForKey nsCoder  key =
withObjCPtr key $ \raw_key ->
    sendMsg nsCoder (mkSelector "decodeIntegerForKey:") retCLong [argPtr (castPtr raw_key :: Ptr ())]

-- | @- decodeObjectOfClass:forKey:@
decodeObjectOfClass_forKey :: (IsNSCoder nsCoder, IsNSString key) => nsCoder -> Class -> key -> IO RawId
decodeObjectOfClass_forKey nsCoder  aClass key =
withObjCPtr key $ \raw_key ->
    fmap (RawId . castPtr) $ sendMsg nsCoder (mkSelector "decodeObjectOfClass:forKey:") (retPtr retVoid) [argPtr (unClass aClass), argPtr (castPtr raw_key :: Ptr ())]

-- | @- decodeTopLevelObjectOfClass:forKey:error:@
decodeTopLevelObjectOfClass_forKey_error :: (IsNSCoder nsCoder, IsNSString key, IsNSError error_) => nsCoder -> Class -> key -> error_ -> IO RawId
decodeTopLevelObjectOfClass_forKey_error nsCoder  aClass key error_ =
withObjCPtr key $ \raw_key ->
  withObjCPtr error_ $ \raw_error_ ->
      fmap (RawId . castPtr) $ sendMsg nsCoder (mkSelector "decodeTopLevelObjectOfClass:forKey:error:") (retPtr retVoid) [argPtr (unClass aClass), argPtr (castPtr raw_key :: Ptr ()), argPtr (castPtr raw_error_ :: Ptr ())]

-- | Decodes the @NSArray@ object for the given  @key,@ which should be an @NSArray<cls>,@ containing the given non-collection class (no nested arrays or arrays of dictionaries, etc) from the coder.
--
-- Requires @NSSecureCoding@ otherwise an exception is thrown and sets the @decodingFailurePolicy@ to @NSDecodingFailurePolicySetErrorAndReturn.@
--
-- Returns @nil@ if the object for @key@ is not of the expected types, or cannot be decoded, and sets the @error@ on the decoder.
--
-- ObjC selector: @- decodeArrayOfObjectsOfClass:forKey:@
decodeArrayOfObjectsOfClass_forKey :: (IsNSCoder nsCoder, IsNSString key) => nsCoder -> Class -> key -> IO (Id NSArray)
decodeArrayOfObjectsOfClass_forKey nsCoder  cls key =
withObjCPtr key $ \raw_key ->
    sendMsg nsCoder (mkSelector "decodeArrayOfObjectsOfClass:forKey:") (retPtr retVoid) [argPtr (unClass cls), argPtr (castPtr raw_key :: Ptr ())] >>= retainedObject . castPtr

-- | Decodes the @NSDictionary@ object for the given @key,@ which should be an @NSDictionary<keyCls,objectCls>@ , with keys of type given in @keyCls@ and objects of the given non-collection class @objectCls@ (no nested dictionaries or other dictionaries contained in the dictionary, etc) from the coder.
--
-- Requires @NSSecureCoding@ otherwise an exception is thrown and sets the @decodingFailurePolicy@ to @NSDecodingFailurePolicySetErrorAndReturn.@
--
-- Returns @nil@ if the object for @key@ is not of the expected types, or cannot be decoded, and sets the @error@ on the decoder.
--
-- ObjC selector: @- decodeDictionaryWithKeysOfClass:objectsOfClass:forKey:@
decodeDictionaryWithKeysOfClass_objectsOfClass_forKey :: (IsNSCoder nsCoder, IsNSString key) => nsCoder -> Class -> Class -> key -> IO (Id NSDictionary)
decodeDictionaryWithKeysOfClass_objectsOfClass_forKey nsCoder  keyCls objectCls key =
withObjCPtr key $ \raw_key ->
    sendMsg nsCoder (mkSelector "decodeDictionaryWithKeysOfClass:objectsOfClass:forKey:") (retPtr retVoid) [argPtr (unClass keyCls), argPtr (unClass objectCls), argPtr (castPtr raw_key :: Ptr ())] >>= retainedObject . castPtr

-- | @- decodeObjectOfClasses:forKey:@
decodeObjectOfClasses_forKey :: (IsNSCoder nsCoder, IsNSSet classes, IsNSString key) => nsCoder -> classes -> key -> IO RawId
decodeObjectOfClasses_forKey nsCoder  classes key =
withObjCPtr classes $ \raw_classes ->
  withObjCPtr key $ \raw_key ->
      fmap (RawId . castPtr) $ sendMsg nsCoder (mkSelector "decodeObjectOfClasses:forKey:") (retPtr retVoid) [argPtr (castPtr raw_classes :: Ptr ()), argPtr (castPtr raw_key :: Ptr ())]

-- | @- decodeTopLevelObjectOfClasses:forKey:error:@
decodeTopLevelObjectOfClasses_forKey_error :: (IsNSCoder nsCoder, IsNSSet classes, IsNSString key, IsNSError error_) => nsCoder -> classes -> key -> error_ -> IO RawId
decodeTopLevelObjectOfClasses_forKey_error nsCoder  classes key error_ =
withObjCPtr classes $ \raw_classes ->
  withObjCPtr key $ \raw_key ->
    withObjCPtr error_ $ \raw_error_ ->
        fmap (RawId . castPtr) $ sendMsg nsCoder (mkSelector "decodeTopLevelObjectOfClasses:forKey:error:") (retPtr retVoid) [argPtr (castPtr raw_classes :: Ptr ()), argPtr (castPtr raw_key :: Ptr ()), argPtr (castPtr raw_error_ :: Ptr ())]

-- | Decodes the @NSArray@ object for the given @key,@ which should be an @NSArray,@ containing the given non-collection classes (no nested arrays or arrays of dictionaries, etc) from the coder.
--
-- Requires @NSSecureCoding@ otherwise an exception is thrown and sets the @decodingFailurePolicy@ to @NSDecodingFailurePolicySetErrorAndReturn.@
--
-- Returns @nil@ if the object for @key@ is not of the expected types, or cannot be decoded, and sets the @error@ on the decoder.
--
-- ObjC selector: @- decodeArrayOfObjectsOfClasses:forKey:@
decodeArrayOfObjectsOfClasses_forKey :: (IsNSCoder nsCoder, IsNSSet classes, IsNSString key) => nsCoder -> classes -> key -> IO (Id NSArray)
decodeArrayOfObjectsOfClasses_forKey nsCoder  classes key =
withObjCPtr classes $ \raw_classes ->
  withObjCPtr key $ \raw_key ->
      sendMsg nsCoder (mkSelector "decodeArrayOfObjectsOfClasses:forKey:") (retPtr retVoid) [argPtr (castPtr raw_classes :: Ptr ()), argPtr (castPtr raw_key :: Ptr ())] >>= retainedObject . castPtr

-- | Decodes the @NSDictionary@ object for the given @key,@ which should be an @NSDictionary,@ with keys of the types given in @keyClasses@ and objects of the given non-collection classes in @objectClasses@ (no nested dictionaries or other dictionaries contained in the dictionary, etc) from the given coder.
--
-- Requires @NSSecureCoding@ otherwise an exception is thrown and sets the @decodingFailurePolicy@ to @NSDecodingFailurePolicySetErrorAndReturn.@
--
-- Returns @nil@ if the object for @key@ is not of the expected types, or cannot be decoded, and sets the @error@ on the decoder.
--
-- ObjC selector: @- decodeDictionaryWithKeysOfClasses:objectsOfClasses:forKey:@
decodeDictionaryWithKeysOfClasses_objectsOfClasses_forKey :: (IsNSCoder nsCoder, IsNSSet keyClasses, IsNSSet objectClasses, IsNSString key) => nsCoder -> keyClasses -> objectClasses -> key -> IO (Id NSDictionary)
decodeDictionaryWithKeysOfClasses_objectsOfClasses_forKey nsCoder  keyClasses objectClasses key =
withObjCPtr keyClasses $ \raw_keyClasses ->
  withObjCPtr objectClasses $ \raw_objectClasses ->
    withObjCPtr key $ \raw_key ->
        sendMsg nsCoder (mkSelector "decodeDictionaryWithKeysOfClasses:objectsOfClasses:forKey:") (retPtr retVoid) [argPtr (castPtr raw_keyClasses :: Ptr ()), argPtr (castPtr raw_objectClasses :: Ptr ()), argPtr (castPtr raw_key :: Ptr ())] >>= retainedObject . castPtr

-- | @- decodePropertyListForKey:@
decodePropertyListForKey :: (IsNSCoder nsCoder, IsNSString key) => nsCoder -> key -> IO RawId
decodePropertyListForKey nsCoder  key =
withObjCPtr key $ \raw_key ->
    fmap (RawId . castPtr) $ sendMsg nsCoder (mkSelector "decodePropertyListForKey:") (retPtr retVoid) [argPtr (castPtr raw_key :: Ptr ())]

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
failWithError nsCoder  error_ =
withObjCPtr error_ $ \raw_error_ ->
    sendMsg nsCoder (mkSelector "failWithError:") retVoid [argPtr (castPtr raw_error_ :: Ptr ())]

-- | @- systemVersion@
systemVersion :: IsNSCoder nsCoder => nsCoder -> IO CUInt
systemVersion nsCoder  =
  sendMsg nsCoder (mkSelector "systemVersion") retCUInt []

-- | @- allowsKeyedCoding@
allowsKeyedCoding :: IsNSCoder nsCoder => nsCoder -> IO Bool
allowsKeyedCoding nsCoder  =
  fmap ((/= 0) :: CULong -> Bool) $ sendMsg nsCoder (mkSelector "allowsKeyedCoding") retCULong []

-- | @- requiresSecureCoding@
requiresSecureCoding :: IsNSCoder nsCoder => nsCoder -> IO Bool
requiresSecureCoding nsCoder  =
  fmap ((/= 0) :: CULong -> Bool) $ sendMsg nsCoder (mkSelector "requiresSecureCoding") retCULong []

-- | @- allowedClasses@
allowedClasses :: IsNSCoder nsCoder => nsCoder -> IO (Id NSSet)
allowedClasses nsCoder  =
  sendMsg nsCoder (mkSelector "allowedClasses") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | Defines the behavior this NSCoder should take on decode failure (i.e. corrupt archive, invalid data, etc.).
--
-- The default result of this property is NSDecodingFailurePolicyRaiseException, subclasses can change this to an alternative policy.
--
-- ObjC selector: @- decodingFailurePolicy@
decodingFailurePolicy :: IsNSCoder nsCoder => nsCoder -> IO NSDecodingFailurePolicy
decodingFailurePolicy nsCoder  =
  fmap (coerce :: CLong -> NSDecodingFailurePolicy) $ sendMsg nsCoder (mkSelector "decodingFailurePolicy") retCLong []

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
error_ nsCoder  =
  sendMsg nsCoder (mkSelector "error") (retPtr retVoid) [] >>= retainedObject . castPtr

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @encodeValueOfObjCType:at:@
encodeValueOfObjCType_atSelector :: Selector
encodeValueOfObjCType_atSelector = mkSelector "encodeValueOfObjCType:at:"

-- | @Selector@ for @encodeDataObject:@
encodeDataObjectSelector :: Selector
encodeDataObjectSelector = mkSelector "encodeDataObject:"

-- | @Selector@ for @decodeDataObject@
decodeDataObjectSelector :: Selector
decodeDataObjectSelector = mkSelector "decodeDataObject"

-- | @Selector@ for @decodeValueOfObjCType:at:size:@
decodeValueOfObjCType_at_sizeSelector :: Selector
decodeValueOfObjCType_at_sizeSelector = mkSelector "decodeValueOfObjCType:at:size:"

-- | @Selector@ for @versionForClassName:@
versionForClassNameSelector :: Selector
versionForClassNameSelector = mkSelector "versionForClassName:"

-- | @Selector@ for @encodePoint:forKey:@
encodePoint_forKeySelector :: Selector
encodePoint_forKeySelector = mkSelector "encodePoint:forKey:"

-- | @Selector@ for @encodeSize:forKey:@
encodeSize_forKeySelector :: Selector
encodeSize_forKeySelector = mkSelector "encodeSize:forKey:"

-- | @Selector@ for @encodeRect:forKey:@
encodeRect_forKeySelector :: Selector
encodeRect_forKeySelector = mkSelector "encodeRect:forKey:"

-- | @Selector@ for @decodePointForKey:@
decodePointForKeySelector :: Selector
decodePointForKeySelector = mkSelector "decodePointForKey:"

-- | @Selector@ for @decodeSizeForKey:@
decodeSizeForKeySelector :: Selector
decodeSizeForKeySelector = mkSelector "decodeSizeForKey:"

-- | @Selector@ for @decodeRectForKey:@
decodeRectForKeySelector :: Selector
decodeRectForKeySelector = mkSelector "decodeRectForKey:"

-- | @Selector@ for @encodePoint:@
encodePointSelector :: Selector
encodePointSelector = mkSelector "encodePoint:"

-- | @Selector@ for @decodePoint@
decodePointSelector :: Selector
decodePointSelector = mkSelector "decodePoint"

-- | @Selector@ for @encodeSize:@
encodeSizeSelector :: Selector
encodeSizeSelector = mkSelector "encodeSize:"

-- | @Selector@ for @decodeSize@
decodeSizeSelector :: Selector
decodeSizeSelector = mkSelector "decodeSize"

-- | @Selector@ for @encodeRect:@
encodeRectSelector :: Selector
encodeRectSelector = mkSelector "encodeRect:"

-- | @Selector@ for @decodeRect@
decodeRectSelector :: Selector
decodeRectSelector = mkSelector "decodeRect"

-- | @Selector@ for @decodeValueOfObjCType:at:@
decodeValueOfObjCType_atSelector :: Selector
decodeValueOfObjCType_atSelector = mkSelector "decodeValueOfObjCType:at:"

-- | @Selector@ for @encodeNXObject:@
encodeNXObjectSelector :: Selector
encodeNXObjectSelector = mkSelector "encodeNXObject:"

-- | @Selector@ for @decodeNXObject@
decodeNXObjectSelector :: Selector
decodeNXObjectSelector = mkSelector "decodeNXObject"

-- | @Selector@ for @encodeObject:@
encodeObjectSelector :: Selector
encodeObjectSelector = mkSelector "encodeObject:"

-- | @Selector@ for @encodeRootObject:@
encodeRootObjectSelector :: Selector
encodeRootObjectSelector = mkSelector "encodeRootObject:"

-- | @Selector@ for @encodeBycopyObject:@
encodeBycopyObjectSelector :: Selector
encodeBycopyObjectSelector = mkSelector "encodeBycopyObject:"

-- | @Selector@ for @encodeByrefObject:@
encodeByrefObjectSelector :: Selector
encodeByrefObjectSelector = mkSelector "encodeByrefObject:"

-- | @Selector@ for @encodeConditionalObject:@
encodeConditionalObjectSelector :: Selector
encodeConditionalObjectSelector = mkSelector "encodeConditionalObject:"

-- | @Selector@ for @encodeValuesOfObjCTypes:@
encodeValuesOfObjCTypesSelector :: Selector
encodeValuesOfObjCTypesSelector = mkSelector "encodeValuesOfObjCTypes:"

-- | @Selector@ for @encodeArrayOfObjCType:count:at:@
encodeArrayOfObjCType_count_atSelector :: Selector
encodeArrayOfObjCType_count_atSelector = mkSelector "encodeArrayOfObjCType:count:at:"

-- | @Selector@ for @encodeBytes:length:@
encodeBytes_lengthSelector :: Selector
encodeBytes_lengthSelector = mkSelector "encodeBytes:length:"

-- | @Selector@ for @decodeObject@
decodeObjectSelector :: Selector
decodeObjectSelector = mkSelector "decodeObject"

-- | @Selector@ for @decodeTopLevelObjectAndReturnError:@
decodeTopLevelObjectAndReturnErrorSelector :: Selector
decodeTopLevelObjectAndReturnErrorSelector = mkSelector "decodeTopLevelObjectAndReturnError:"

-- | @Selector@ for @decodeValuesOfObjCTypes:@
decodeValuesOfObjCTypesSelector :: Selector
decodeValuesOfObjCTypesSelector = mkSelector "decodeValuesOfObjCTypes:"

-- | @Selector@ for @decodeArrayOfObjCType:count:at:@
decodeArrayOfObjCType_count_atSelector :: Selector
decodeArrayOfObjCType_count_atSelector = mkSelector "decodeArrayOfObjCType:count:at:"

-- | @Selector@ for @decodeBytesWithReturnedLength:@
decodeBytesWithReturnedLengthSelector :: Selector
decodeBytesWithReturnedLengthSelector = mkSelector "decodeBytesWithReturnedLength:"

-- | @Selector@ for @encodePropertyList:@
encodePropertyListSelector :: Selector
encodePropertyListSelector = mkSelector "encodePropertyList:"

-- | @Selector@ for @decodePropertyList@
decodePropertyListSelector :: Selector
decodePropertyListSelector = mkSelector "decodePropertyList"

-- | @Selector@ for @setObjectZone:@
setObjectZoneSelector :: Selector
setObjectZoneSelector = mkSelector "setObjectZone:"

-- | @Selector@ for @objectZone@
objectZoneSelector :: Selector
objectZoneSelector = mkSelector "objectZone"

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

-- | @Selector@ for @containsValueForKey:@
containsValueForKeySelector :: Selector
containsValueForKeySelector = mkSelector "containsValueForKey:"

-- | @Selector@ for @decodeObjectForKey:@
decodeObjectForKeySelector :: Selector
decodeObjectForKeySelector = mkSelector "decodeObjectForKey:"

-- | @Selector@ for @decodeTopLevelObjectForKey:error:@
decodeTopLevelObjectForKey_errorSelector :: Selector
decodeTopLevelObjectForKey_errorSelector = mkSelector "decodeTopLevelObjectForKey:error:"

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

-- | @Selector@ for @decodeBytesWithMinimumLength:@
decodeBytesWithMinimumLengthSelector :: Selector
decodeBytesWithMinimumLengthSelector = mkSelector "decodeBytesWithMinimumLength:"

-- | @Selector@ for @decodeBytesForKey:minimumLength:@
decodeBytesForKey_minimumLengthSelector :: Selector
decodeBytesForKey_minimumLengthSelector = mkSelector "decodeBytesForKey:minimumLength:"

-- | @Selector@ for @encodeInteger:forKey:@
encodeInteger_forKeySelector :: Selector
encodeInteger_forKeySelector = mkSelector "encodeInteger:forKey:"

-- | @Selector@ for @decodeIntegerForKey:@
decodeIntegerForKeySelector :: Selector
decodeIntegerForKeySelector = mkSelector "decodeIntegerForKey:"

-- | @Selector@ for @decodeObjectOfClass:forKey:@
decodeObjectOfClass_forKeySelector :: Selector
decodeObjectOfClass_forKeySelector = mkSelector "decodeObjectOfClass:forKey:"

-- | @Selector@ for @decodeTopLevelObjectOfClass:forKey:error:@
decodeTopLevelObjectOfClass_forKey_errorSelector :: Selector
decodeTopLevelObjectOfClass_forKey_errorSelector = mkSelector "decodeTopLevelObjectOfClass:forKey:error:"

-- | @Selector@ for @decodeArrayOfObjectsOfClass:forKey:@
decodeArrayOfObjectsOfClass_forKeySelector :: Selector
decodeArrayOfObjectsOfClass_forKeySelector = mkSelector "decodeArrayOfObjectsOfClass:forKey:"

-- | @Selector@ for @decodeDictionaryWithKeysOfClass:objectsOfClass:forKey:@
decodeDictionaryWithKeysOfClass_objectsOfClass_forKeySelector :: Selector
decodeDictionaryWithKeysOfClass_objectsOfClass_forKeySelector = mkSelector "decodeDictionaryWithKeysOfClass:objectsOfClass:forKey:"

-- | @Selector@ for @decodeObjectOfClasses:forKey:@
decodeObjectOfClasses_forKeySelector :: Selector
decodeObjectOfClasses_forKeySelector = mkSelector "decodeObjectOfClasses:forKey:"

-- | @Selector@ for @decodeTopLevelObjectOfClasses:forKey:error:@
decodeTopLevelObjectOfClasses_forKey_errorSelector :: Selector
decodeTopLevelObjectOfClasses_forKey_errorSelector = mkSelector "decodeTopLevelObjectOfClasses:forKey:error:"

-- | @Selector@ for @decodeArrayOfObjectsOfClasses:forKey:@
decodeArrayOfObjectsOfClasses_forKeySelector :: Selector
decodeArrayOfObjectsOfClasses_forKeySelector = mkSelector "decodeArrayOfObjectsOfClasses:forKey:"

-- | @Selector@ for @decodeDictionaryWithKeysOfClasses:objectsOfClasses:forKey:@
decodeDictionaryWithKeysOfClasses_objectsOfClasses_forKeySelector :: Selector
decodeDictionaryWithKeysOfClasses_objectsOfClasses_forKeySelector = mkSelector "decodeDictionaryWithKeysOfClasses:objectsOfClasses:forKey:"

-- | @Selector@ for @decodePropertyListForKey:@
decodePropertyListForKeySelector :: Selector
decodePropertyListForKeySelector = mkSelector "decodePropertyListForKey:"

-- | @Selector@ for @failWithError:@
failWithErrorSelector :: Selector
failWithErrorSelector = mkSelector "failWithError:"

-- | @Selector@ for @systemVersion@
systemVersionSelector :: Selector
systemVersionSelector = mkSelector "systemVersion"

-- | @Selector@ for @allowsKeyedCoding@
allowsKeyedCodingSelector :: Selector
allowsKeyedCodingSelector = mkSelector "allowsKeyedCoding"

-- | @Selector@ for @requiresSecureCoding@
requiresSecureCodingSelector :: Selector
requiresSecureCodingSelector = mkSelector "requiresSecureCoding"

-- | @Selector@ for @allowedClasses@
allowedClassesSelector :: Selector
allowedClassesSelector = mkSelector "allowedClasses"

-- | @Selector@ for @decodingFailurePolicy@
decodingFailurePolicySelector :: Selector
decodingFailurePolicySelector = mkSelector "decodingFailurePolicy"

-- | @Selector@ for @error@
errorSelector :: Selector
errorSelector = mkSelector "error"

