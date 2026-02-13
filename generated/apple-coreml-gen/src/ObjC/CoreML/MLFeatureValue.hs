{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | An immutable variant holding a data value of a supported MLFeatureType
--
-- MLFeatureValue does not support type conversion in its accessor properties. It can also have a missing or undefined value of a well defined type.
--
-- Generated bindings for @MLFeatureValue@.
module ObjC.CoreML.MLFeatureValue
  ( MLFeatureValue
  , IsMLFeatureValue(..)
  , featureValueWithInt64
  , featureValueWithDouble
  , featureValueWithString
  , featureValueWithMultiArray
  , featureValueWithPixelBuffer
  , featureValueWithSequence
  , undefinedFeatureValueWithType
  , featureValueWithDictionary_error
  , isEqualToFeatureValue
  , featureValueWithImageAtURL_pixelsWide_pixelsHigh_pixelFormatType_options_error
  , featureValueWithImageAtURL_constraint_options_error
  , featureValueWithCGImage_pixelsWide_pixelsHigh_pixelFormatType_options_error
  , featureValueWithCGImage_constraint_options_error
  , featureValueWithImageAtURL_orientation_pixelsWide_pixelsHigh_pixelFormatType_options_error
  , featureValueWithImageAtURL_orientation_constraint_options_error
  , featureValueWithCGImage_orientation_pixelsWide_pixelsHigh_pixelFormatType_options_error
  , featureValueWithCGImage_orientation_constraint_options_error
  , type_
  , undefined_
  , int64Value
  , doubleValue
  , stringValue
  , multiArrayValue
  , dictionaryValue
  , imageBufferValue
  , sequenceValue
  , dictionaryValueSelector
  , doubleValueSelector
  , featureValueWithCGImage_constraint_options_errorSelector
  , featureValueWithCGImage_orientation_constraint_options_errorSelector
  , featureValueWithCGImage_orientation_pixelsWide_pixelsHigh_pixelFormatType_options_errorSelector
  , featureValueWithCGImage_pixelsWide_pixelsHigh_pixelFormatType_options_errorSelector
  , featureValueWithDictionary_errorSelector
  , featureValueWithDoubleSelector
  , featureValueWithImageAtURL_constraint_options_errorSelector
  , featureValueWithImageAtURL_orientation_constraint_options_errorSelector
  , featureValueWithImageAtURL_orientation_pixelsWide_pixelsHigh_pixelFormatType_options_errorSelector
  , featureValueWithImageAtURL_pixelsWide_pixelsHigh_pixelFormatType_options_errorSelector
  , featureValueWithInt64Selector
  , featureValueWithMultiArraySelector
  , featureValueWithPixelBufferSelector
  , featureValueWithSequenceSelector
  , featureValueWithStringSelector
  , imageBufferValueSelector
  , int64ValueSelector
  , isEqualToFeatureValueSelector
  , multiArrayValueSelector
  , sequenceValueSelector
  , stringValueSelector
  , typeSelector
  , undefinedFeatureValueWithTypeSelector
  , undefinedSelector

  -- * Enum types
  , MLFeatureType(MLFeatureType)
  , pattern MLFeatureTypeInvalid
  , pattern MLFeatureTypeInt64
  , pattern MLFeatureTypeDouble
  , pattern MLFeatureTypeString
  , pattern MLFeatureTypeImage
  , pattern MLFeatureTypeMultiArray
  , pattern MLFeatureTypeDictionary
  , pattern MLFeatureTypeSequence
  , pattern MLFeatureTypeState

  ) where

import Foreign.Ptr (Ptr, FunPtr)
import Foreign.C.Types

import ObjC.Runtime.Types
import ObjC.Runtime.Message (sendMessage, sendOwnedMessage, sendClassMessage, sendOwnedClassMessage)
import ObjC.Runtime.Selector (mkSelector)
import ObjC.Runtime.Class (getRequiredClass)

import ObjC.CoreML.Internal.Classes
import ObjC.CoreML.Internal.Enums
import ObjC.Foundation.Internal.Classes

-- | Hold an object with the specified value
--
-- ObjC selector: @+ featureValueWithInt64:@
featureValueWithInt64 :: CLong -> IO (Id MLFeatureValue)
featureValueWithInt64 value =
  do
    cls' <- getRequiredClass "MLFeatureValue"
    sendClassMessage cls' featureValueWithInt64Selector value

-- | @+ featureValueWithDouble:@
featureValueWithDouble :: CDouble -> IO (Id MLFeatureValue)
featureValueWithDouble value =
  do
    cls' <- getRequiredClass "MLFeatureValue"
    sendClassMessage cls' featureValueWithDoubleSelector value

-- | @+ featureValueWithString:@
featureValueWithString :: IsNSString value => value -> IO (Id MLFeatureValue)
featureValueWithString value =
  do
    cls' <- getRequiredClass "MLFeatureValue"
    sendClassMessage cls' featureValueWithStringSelector (toNSString value)

-- | @+ featureValueWithMultiArray:@
featureValueWithMultiArray :: IsMLMultiArray value => value -> IO (Id MLFeatureValue)
featureValueWithMultiArray value =
  do
    cls' <- getRequiredClass "MLFeatureValue"
    sendClassMessage cls' featureValueWithMultiArraySelector (toMLMultiArray value)

-- | @+ featureValueWithPixelBuffer:@
featureValueWithPixelBuffer :: Ptr () -> IO (Id MLFeatureValue)
featureValueWithPixelBuffer value =
  do
    cls' <- getRequiredClass "MLFeatureValue"
    sendClassMessage cls' featureValueWithPixelBufferSelector value

-- | @+ featureValueWithSequence:@
featureValueWithSequence :: IsMLSequence sequence_ => sequence_ -> IO (Id MLFeatureValue)
featureValueWithSequence sequence_ =
  do
    cls' <- getRequiredClass "MLFeatureValue"
    sendClassMessage cls' featureValueWithSequenceSelector (toMLSequence sequence_)

-- | Represent an undefined value of a specified type
--
-- ObjC selector: @+ undefinedFeatureValueWithType:@
undefinedFeatureValueWithType :: MLFeatureType -> IO (Id MLFeatureValue)
undefinedFeatureValueWithType type_ =
  do
    cls' <- getRequiredClass "MLFeatureValue"
    sendClassMessage cls' undefinedFeatureValueWithTypeSelector type_

-- | For encoding a sparse feature set or for encoding probabilities. Input keys that are not NSNumber * or NSString * are rejected on construction and return a MLModelErrorFeatureTypeMismatch error. Further validation for consistency occurs on evaluation
--
-- ObjC selector: @+ featureValueWithDictionary:error:@
featureValueWithDictionary_error :: (IsNSDictionary value, IsNSError error_) => value -> error_ -> IO (Id MLFeatureValue)
featureValueWithDictionary_error value error_ =
  do
    cls' <- getRequiredClass "MLFeatureValue"
    sendClassMessage cls' featureValueWithDictionary_errorSelector (toNSDictionary value) (toNSError error_)

-- | Returns a Boolean value that indicates whether a feature value is equal to another.
--
-- If the types of the MLFeatureValue objects "self" and "value"  are integer in one case and double in the other (in either order) then those mixed mode numeric values are compared as NSNumbers. Otherwise if the types of the MLFeatureValue objects are different NO is returned. When "self" and "value" are both PixelBuffer MLFeatureValue types, only their CVPixelBufferRef values are compared for equality, the underlying arrays of pixelValues are not examined. [So, distinct PixelBuffer MLFeatureValue objects with distinct CVPixelBufferRef values which encapsulate the same array of pixels will compare *not* equal.] For all other (matching) MLFeatureValue types, the BOOL value returned is the result of comparing "self" with "value" via isEqualToNumber:, isEqualToString:, isEqualtoDictionary:, isEqualToMultiArray:, isEqualToArray: as chosen by the MLFeatureValue types.
--
-- ObjC selector: @- isEqualToFeatureValue:@
isEqualToFeatureValue :: (IsMLFeatureValue mlFeatureValue, IsMLFeatureValue value) => mlFeatureValue -> value -> IO Bool
isEqualToFeatureValue mlFeatureValue value =
  sendMessage mlFeatureValue isEqualToFeatureValueSelector (toMLFeatureValue value)

-- | Construct image feature value from an image on disk. Orientation is read from Exif if avaiable
--
-- ObjC selector: @+ featureValueWithImageAtURL:pixelsWide:pixelsHigh:pixelFormatType:options:error:@
featureValueWithImageAtURL_pixelsWide_pixelsHigh_pixelFormatType_options_error :: (IsNSURL url, IsNSDictionary options, IsNSError error_) => url -> CLong -> CLong -> CUInt -> options -> error_ -> IO (Id MLFeatureValue)
featureValueWithImageAtURL_pixelsWide_pixelsHigh_pixelFormatType_options_error url pixelsWide pixelsHigh pixelFormatType options error_ =
  do
    cls' <- getRequiredClass "MLFeatureValue"
    sendClassMessage cls' featureValueWithImageAtURL_pixelsWide_pixelsHigh_pixelFormatType_options_errorSelector (toNSURL url) pixelsWide pixelsHigh pixelFormatType (toNSDictionary options) (toNSError error_)

-- | Construct image feature value from an image on disk, using a model specified image constraint. Orientation is read from Exif if avaiable
--
-- ObjC selector: @+ featureValueWithImageAtURL:constraint:options:error:@
featureValueWithImageAtURL_constraint_options_error :: (IsNSURL url, IsMLImageConstraint constraint, IsNSDictionary options, IsNSError error_) => url -> constraint -> options -> error_ -> IO (Id MLFeatureValue)
featureValueWithImageAtURL_constraint_options_error url constraint options error_ =
  do
    cls' <- getRequiredClass "MLFeatureValue"
    sendClassMessage cls' featureValueWithImageAtURL_constraint_options_errorSelector (toNSURL url) (toMLImageConstraint constraint) (toNSDictionary options) (toNSError error_)

-- | Construct image feature value from CGImage (orientation is assumed to be kCGImagePropertyOrientationUp)
--
-- ObjC selector: @+ featureValueWithCGImage:pixelsWide:pixelsHigh:pixelFormatType:options:error:@
featureValueWithCGImage_pixelsWide_pixelsHigh_pixelFormatType_options_error :: (IsNSDictionary options, IsNSError error_) => Ptr () -> CLong -> CLong -> CUInt -> options -> error_ -> IO (Id MLFeatureValue)
featureValueWithCGImage_pixelsWide_pixelsHigh_pixelFormatType_options_error cgImage pixelsWide pixelsHigh pixelFormatType options error_ =
  do
    cls' <- getRequiredClass "MLFeatureValue"
    sendClassMessage cls' featureValueWithCGImage_pixelsWide_pixelsHigh_pixelFormatType_options_errorSelector cgImage pixelsWide pixelsHigh pixelFormatType (toNSDictionary options) (toNSError error_)

-- | Construct image feature value from CGImage, using the size and type information required by feature description (orientation is assumed to be kCGImagePropertyOrientationUp)
--
-- ObjC selector: @+ featureValueWithCGImage:constraint:options:error:@
featureValueWithCGImage_constraint_options_error :: (IsMLImageConstraint constraint, IsNSDictionary options, IsNSError error_) => Ptr () -> constraint -> options -> error_ -> IO (Id MLFeatureValue)
featureValueWithCGImage_constraint_options_error cgImage constraint options error_ =
  do
    cls' <- getRequiredClass "MLFeatureValue"
    sendClassMessage cls' featureValueWithCGImage_constraint_options_errorSelector cgImage (toMLImageConstraint constraint) (toNSDictionary options) (toNSError error_)

-- | Construct image feature value from an image on disk. The passed in orientation supersedes any in the file
--
-- ObjC selector: @+ featureValueWithImageAtURL:orientation:pixelsWide:pixelsHigh:pixelFormatType:options:error:@
featureValueWithImageAtURL_orientation_pixelsWide_pixelsHigh_pixelFormatType_options_error :: (IsNSURL url, IsNSDictionary options, IsNSError error_) => url -> CInt -> CLong -> CLong -> CUInt -> options -> error_ -> IO (Id MLFeatureValue)
featureValueWithImageAtURL_orientation_pixelsWide_pixelsHigh_pixelFormatType_options_error url orientation pixelsWide pixelsHigh pixelFormatType options error_ =
  do
    cls' <- getRequiredClass "MLFeatureValue"
    sendClassMessage cls' featureValueWithImageAtURL_orientation_pixelsWide_pixelsHigh_pixelFormatType_options_errorSelector (toNSURL url) orientation pixelsWide pixelsHigh pixelFormatType (toNSDictionary options) (toNSError error_)

-- | Construct image feature value from an image on disk using a model specified image constraint. The passed in orientation supersedes any in the file
--
-- ObjC selector: @+ featureValueWithImageAtURL:orientation:constraint:options:error:@
featureValueWithImageAtURL_orientation_constraint_options_error :: (IsNSURL url, IsMLImageConstraint constraint, IsNSDictionary options, IsNSError error_) => url -> CInt -> constraint -> options -> error_ -> IO (Id MLFeatureValue)
featureValueWithImageAtURL_orientation_constraint_options_error url orientation constraint options error_ =
  do
    cls' <- getRequiredClass "MLFeatureValue"
    sendClassMessage cls' featureValueWithImageAtURL_orientation_constraint_options_errorSelector (toNSURL url) orientation (toMLImageConstraint constraint) (toNSDictionary options) (toNSError error_)

-- | Construct image feature value from CGImage w/ specified orientation
--
-- ObjC selector: @+ featureValueWithCGImage:orientation:pixelsWide:pixelsHigh:pixelFormatType:options:error:@
featureValueWithCGImage_orientation_pixelsWide_pixelsHigh_pixelFormatType_options_error :: (IsNSDictionary options, IsNSError error_) => Ptr () -> CInt -> CLong -> CLong -> CUInt -> options -> error_ -> IO (Id MLFeatureValue)
featureValueWithCGImage_orientation_pixelsWide_pixelsHigh_pixelFormatType_options_error cgImage orientation pixelsWide pixelsHigh pixelFormatType options error_ =
  do
    cls' <- getRequiredClass "MLFeatureValue"
    sendClassMessage cls' featureValueWithCGImage_orientation_pixelsWide_pixelsHigh_pixelFormatType_options_errorSelector cgImage orientation pixelsWide pixelsHigh pixelFormatType (toNSDictionary options) (toNSError error_)

-- | Construct image feature value from CGImage w/ specified orientation, using the size and type information required by feature description
--
-- ObjC selector: @+ featureValueWithCGImage:orientation:constraint:options:error:@
featureValueWithCGImage_orientation_constraint_options_error :: (IsMLImageConstraint constraint, IsNSDictionary options, IsNSError error_) => Ptr () -> CInt -> constraint -> options -> error_ -> IO (Id MLFeatureValue)
featureValueWithCGImage_orientation_constraint_options_error cgImage orientation constraint options error_ =
  do
    cls' <- getRequiredClass "MLFeatureValue"
    sendClassMessage cls' featureValueWithCGImage_orientation_constraint_options_errorSelector cgImage orientation (toMLImageConstraint constraint) (toNSDictionary options) (toNSError error_)

-- | Type of the value for which the corresponding property below is held
--
-- ObjC selector: @- type@
type_ :: IsMLFeatureValue mlFeatureValue => mlFeatureValue -> IO MLFeatureType
type_ mlFeatureValue =
  sendMessage mlFeatureValue typeSelector

-- | True if the value represents a missing or undefined value
--
-- ObjC selector: @- undefined@
undefined_ :: IsMLFeatureValue mlFeatureValue => mlFeatureValue -> IO Bool
undefined_ mlFeatureValue =
  sendMessage mlFeatureValue undefinedSelector

-- | Populated value if the type is MLFeatureTypeInt64
--
-- ObjC selector: @- int64Value@
int64Value :: IsMLFeatureValue mlFeatureValue => mlFeatureValue -> IO CLong
int64Value mlFeatureValue =
  sendMessage mlFeatureValue int64ValueSelector

-- | Populated value if the type is MLFeatureTypeDouble
--
-- ObjC selector: @- doubleValue@
doubleValue :: IsMLFeatureValue mlFeatureValue => mlFeatureValue -> IO CDouble
doubleValue mlFeatureValue =
  sendMessage mlFeatureValue doubleValueSelector

-- | Populated value if the type is MLFeatureTypeString
--
-- ObjC selector: @- stringValue@
stringValue :: IsMLFeatureValue mlFeatureValue => mlFeatureValue -> IO (Id NSString)
stringValue mlFeatureValue =
  sendMessage mlFeatureValue stringValueSelector

-- | Populated value if the type is MLFeatureTypeMultiArray
--
-- ObjC selector: @- multiArrayValue@
multiArrayValue :: IsMLFeatureValue mlFeatureValue => mlFeatureValue -> IO (Id MLMultiArray)
multiArrayValue mlFeatureValue =
  sendMessage mlFeatureValue multiArrayValueSelector

-- | Populated value if the type is MLFeatureTypeDictionary
--
-- ObjC selector: @- dictionaryValue@
dictionaryValue :: IsMLFeatureValue mlFeatureValue => mlFeatureValue -> IO (Id NSDictionary)
dictionaryValue mlFeatureValue =
  sendMessage mlFeatureValue dictionaryValueSelector

-- | Populated value if the type is MLFeatureTypeImage
--
-- ObjC selector: @- imageBufferValue@
imageBufferValue :: IsMLFeatureValue mlFeatureValue => mlFeatureValue -> IO (Ptr ())
imageBufferValue mlFeatureValue =
  sendMessage mlFeatureValue imageBufferValueSelector

-- | Populated value if the type is MLFeatureTypeSequence
--
-- ObjC selector: @- sequenceValue@
sequenceValue :: IsMLFeatureValue mlFeatureValue => mlFeatureValue -> IO (Id MLSequence)
sequenceValue mlFeatureValue =
  sendMessage mlFeatureValue sequenceValueSelector

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @featureValueWithInt64:@
featureValueWithInt64Selector :: Selector '[CLong] (Id MLFeatureValue)
featureValueWithInt64Selector = mkSelector "featureValueWithInt64:"

-- | @Selector@ for @featureValueWithDouble:@
featureValueWithDoubleSelector :: Selector '[CDouble] (Id MLFeatureValue)
featureValueWithDoubleSelector = mkSelector "featureValueWithDouble:"

-- | @Selector@ for @featureValueWithString:@
featureValueWithStringSelector :: Selector '[Id NSString] (Id MLFeatureValue)
featureValueWithStringSelector = mkSelector "featureValueWithString:"

-- | @Selector@ for @featureValueWithMultiArray:@
featureValueWithMultiArraySelector :: Selector '[Id MLMultiArray] (Id MLFeatureValue)
featureValueWithMultiArraySelector = mkSelector "featureValueWithMultiArray:"

-- | @Selector@ for @featureValueWithPixelBuffer:@
featureValueWithPixelBufferSelector :: Selector '[Ptr ()] (Id MLFeatureValue)
featureValueWithPixelBufferSelector = mkSelector "featureValueWithPixelBuffer:"

-- | @Selector@ for @featureValueWithSequence:@
featureValueWithSequenceSelector :: Selector '[Id MLSequence] (Id MLFeatureValue)
featureValueWithSequenceSelector = mkSelector "featureValueWithSequence:"

-- | @Selector@ for @undefinedFeatureValueWithType:@
undefinedFeatureValueWithTypeSelector :: Selector '[MLFeatureType] (Id MLFeatureValue)
undefinedFeatureValueWithTypeSelector = mkSelector "undefinedFeatureValueWithType:"

-- | @Selector@ for @featureValueWithDictionary:error:@
featureValueWithDictionary_errorSelector :: Selector '[Id NSDictionary, Id NSError] (Id MLFeatureValue)
featureValueWithDictionary_errorSelector = mkSelector "featureValueWithDictionary:error:"

-- | @Selector@ for @isEqualToFeatureValue:@
isEqualToFeatureValueSelector :: Selector '[Id MLFeatureValue] Bool
isEqualToFeatureValueSelector = mkSelector "isEqualToFeatureValue:"

-- | @Selector@ for @featureValueWithImageAtURL:pixelsWide:pixelsHigh:pixelFormatType:options:error:@
featureValueWithImageAtURL_pixelsWide_pixelsHigh_pixelFormatType_options_errorSelector :: Selector '[Id NSURL, CLong, CLong, CUInt, Id NSDictionary, Id NSError] (Id MLFeatureValue)
featureValueWithImageAtURL_pixelsWide_pixelsHigh_pixelFormatType_options_errorSelector = mkSelector "featureValueWithImageAtURL:pixelsWide:pixelsHigh:pixelFormatType:options:error:"

-- | @Selector@ for @featureValueWithImageAtURL:constraint:options:error:@
featureValueWithImageAtURL_constraint_options_errorSelector :: Selector '[Id NSURL, Id MLImageConstraint, Id NSDictionary, Id NSError] (Id MLFeatureValue)
featureValueWithImageAtURL_constraint_options_errorSelector = mkSelector "featureValueWithImageAtURL:constraint:options:error:"

-- | @Selector@ for @featureValueWithCGImage:pixelsWide:pixelsHigh:pixelFormatType:options:error:@
featureValueWithCGImage_pixelsWide_pixelsHigh_pixelFormatType_options_errorSelector :: Selector '[Ptr (), CLong, CLong, CUInt, Id NSDictionary, Id NSError] (Id MLFeatureValue)
featureValueWithCGImage_pixelsWide_pixelsHigh_pixelFormatType_options_errorSelector = mkSelector "featureValueWithCGImage:pixelsWide:pixelsHigh:pixelFormatType:options:error:"

-- | @Selector@ for @featureValueWithCGImage:constraint:options:error:@
featureValueWithCGImage_constraint_options_errorSelector :: Selector '[Ptr (), Id MLImageConstraint, Id NSDictionary, Id NSError] (Id MLFeatureValue)
featureValueWithCGImage_constraint_options_errorSelector = mkSelector "featureValueWithCGImage:constraint:options:error:"

-- | @Selector@ for @featureValueWithImageAtURL:orientation:pixelsWide:pixelsHigh:pixelFormatType:options:error:@
featureValueWithImageAtURL_orientation_pixelsWide_pixelsHigh_pixelFormatType_options_errorSelector :: Selector '[Id NSURL, CInt, CLong, CLong, CUInt, Id NSDictionary, Id NSError] (Id MLFeatureValue)
featureValueWithImageAtURL_orientation_pixelsWide_pixelsHigh_pixelFormatType_options_errorSelector = mkSelector "featureValueWithImageAtURL:orientation:pixelsWide:pixelsHigh:pixelFormatType:options:error:"

-- | @Selector@ for @featureValueWithImageAtURL:orientation:constraint:options:error:@
featureValueWithImageAtURL_orientation_constraint_options_errorSelector :: Selector '[Id NSURL, CInt, Id MLImageConstraint, Id NSDictionary, Id NSError] (Id MLFeatureValue)
featureValueWithImageAtURL_orientation_constraint_options_errorSelector = mkSelector "featureValueWithImageAtURL:orientation:constraint:options:error:"

-- | @Selector@ for @featureValueWithCGImage:orientation:pixelsWide:pixelsHigh:pixelFormatType:options:error:@
featureValueWithCGImage_orientation_pixelsWide_pixelsHigh_pixelFormatType_options_errorSelector :: Selector '[Ptr (), CInt, CLong, CLong, CUInt, Id NSDictionary, Id NSError] (Id MLFeatureValue)
featureValueWithCGImage_orientation_pixelsWide_pixelsHigh_pixelFormatType_options_errorSelector = mkSelector "featureValueWithCGImage:orientation:pixelsWide:pixelsHigh:pixelFormatType:options:error:"

-- | @Selector@ for @featureValueWithCGImage:orientation:constraint:options:error:@
featureValueWithCGImage_orientation_constraint_options_errorSelector :: Selector '[Ptr (), CInt, Id MLImageConstraint, Id NSDictionary, Id NSError] (Id MLFeatureValue)
featureValueWithCGImage_orientation_constraint_options_errorSelector = mkSelector "featureValueWithCGImage:orientation:constraint:options:error:"

-- | @Selector@ for @type@
typeSelector :: Selector '[] MLFeatureType
typeSelector = mkSelector "type"

-- | @Selector@ for @undefined@
undefinedSelector :: Selector '[] Bool
undefinedSelector = mkSelector "undefined"

-- | @Selector@ for @int64Value@
int64ValueSelector :: Selector '[] CLong
int64ValueSelector = mkSelector "int64Value"

-- | @Selector@ for @doubleValue@
doubleValueSelector :: Selector '[] CDouble
doubleValueSelector = mkSelector "doubleValue"

-- | @Selector@ for @stringValue@
stringValueSelector :: Selector '[] (Id NSString)
stringValueSelector = mkSelector "stringValue"

-- | @Selector@ for @multiArrayValue@
multiArrayValueSelector :: Selector '[] (Id MLMultiArray)
multiArrayValueSelector = mkSelector "multiArrayValue"

-- | @Selector@ for @dictionaryValue@
dictionaryValueSelector :: Selector '[] (Id NSDictionary)
dictionaryValueSelector = mkSelector "dictionaryValue"

-- | @Selector@ for @imageBufferValue@
imageBufferValueSelector :: Selector '[] (Ptr ())
imageBufferValueSelector = mkSelector "imageBufferValue"

-- | @Selector@ for @sequenceValue@
sequenceValueSelector :: Selector '[] (Id MLSequence)
sequenceValueSelector = mkSelector "sequenceValue"

