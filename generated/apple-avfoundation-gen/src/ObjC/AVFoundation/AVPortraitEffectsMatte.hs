{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | AVPortraitEffectsMatte
--
-- An object wrapping a matting image used for high quality rendering of portrait style effects onto an image (i.e. shallow depth of field, stage lighting, etc).
--
-- The pixel data in the matting image is represented in CVPixelBuffers as kCVPixelFormatType_OneComponent8 ('L008'). It's stored in image files as an auxiliary image, accessible using CGImageSourceCopyAuxiliaryDataInfoAtIndex with the data type kCGImageAuxiliaryDataTypePortraitEffectsMatte (see <ImageIO/CGImageProperties.h>).
--
-- Generated bindings for @AVPortraitEffectsMatte@.
module ObjC.AVFoundation.AVPortraitEffectsMatte
  ( AVPortraitEffectsMatte
  , IsAVPortraitEffectsMatte(..)
  , init_
  , new
  , portraitEffectsMatteFromDictionaryRepresentation_error
  , portraitEffectsMatteByApplyingExifOrientation
  , portraitEffectsMatteByReplacingPortraitEffectsMatteWithPixelBuffer_error
  , dictionaryRepresentationForAuxiliaryDataType
  , pixelFormatType
  , mattingImage
  , dictionaryRepresentationForAuxiliaryDataTypeSelector
  , initSelector
  , mattingImageSelector
  , newSelector
  , pixelFormatTypeSelector
  , portraitEffectsMatteByApplyingExifOrientationSelector
  , portraitEffectsMatteByReplacingPortraitEffectsMatteWithPixelBuffer_errorSelector
  , portraitEffectsMatteFromDictionaryRepresentation_errorSelector


  ) where

import Foreign.Ptr (Ptr, FunPtr)
import Foreign.C.Types

import ObjC.Runtime.Types
import ObjC.Runtime.Message (sendMessage, sendOwnedMessage, sendClassMessage, sendOwnedClassMessage)
import ObjC.Runtime.Selector (mkSelector)
import ObjC.Runtime.Class (getRequiredClass)

import ObjC.AVFoundation.Internal.Classes
import ObjC.Foundation.Internal.Classes

-- | @- init@
init_ :: IsAVPortraitEffectsMatte avPortraitEffectsMatte => avPortraitEffectsMatte -> IO (Id AVPortraitEffectsMatte)
init_ avPortraitEffectsMatte =
  sendOwnedMessage avPortraitEffectsMatte initSelector

-- | @+ new@
new :: IO (Id AVPortraitEffectsMatte)
new  =
  do
    cls' <- getRequiredClass "AVPortraitEffectsMatte"
    sendOwnedClassMessage cls' newSelector

-- | portraitEffectsMatteFromDictionaryRepresentation:error:
--
-- Returns an AVPortraitEffectsMatte instance from auxiliary image information in an image file.
--
-- @imageSourceAuxDataInfoDictionary@ — A dictionary of primitive portrait effects matte related information obtained from CGImageSourceCopyAuxiliaryDataInfoAtIndex.
--
-- @outError@ — On return, if the portrait effects matte cannot be created, points to an NSError describing the problem.
--
-- Returns: An AVPortraitEffectsMatte instance, or nil if the auxiliary data info dictionary was malformed.
--
-- When using ImageIO framework's CGImageSource API to read from a HEIF or JPEG file containing a portrait effects matte, AVPortraitEffectsMatte can be instantiated using the result of CGImageSourceCopyAuxiliaryDataInfoAtIndex, which returns a CFDictionary of primitive map information.
--
-- ObjC selector: @+ portraitEffectsMatteFromDictionaryRepresentation:error:@
portraitEffectsMatteFromDictionaryRepresentation_error :: (IsNSDictionary imageSourceAuxDataInfoDictionary, IsNSError outError) => imageSourceAuxDataInfoDictionary -> outError -> IO (Id AVPortraitEffectsMatte)
portraitEffectsMatteFromDictionaryRepresentation_error imageSourceAuxDataInfoDictionary outError =
  do
    cls' <- getRequiredClass "AVPortraitEffectsMatte"
    sendClassMessage cls' portraitEffectsMatteFromDictionaryRepresentation_errorSelector (toNSDictionary imageSourceAuxDataInfoDictionary) (toNSError outError)

-- | portraitEffectsMatteByApplyingExifOrientation:
--
-- Returns a derivative AVPortraitEffectsMatte instance in which the specified Exif orientation has been applied.
--
-- @exifOrientation@ — One of the 8 standard Exif orientation tags expressing how the portrait effects matte should be rotated / mirrored.
--
-- Returns: An AVPortraitEffectsMatte instance.
--
-- When applying simple 90 degree rotation or mirroring edits to media containing a portrait effects matte, you may use this initializer to create a derivative copy of the portrait effects matte in which the specified orientation is applied. This method throws an NSInvalidArgumentException if you pass an unrecognized exifOrientation.
--
-- ObjC selector: @- portraitEffectsMatteByApplyingExifOrientation:@
portraitEffectsMatteByApplyingExifOrientation :: IsAVPortraitEffectsMatte avPortraitEffectsMatte => avPortraitEffectsMatte -> CInt -> IO (Id AVPortraitEffectsMatte)
portraitEffectsMatteByApplyingExifOrientation avPortraitEffectsMatte exifOrientation =
  sendMessage avPortraitEffectsMatte portraitEffectsMatteByApplyingExifOrientationSelector exifOrientation

-- | portraitEffectsMatteByReplacingPortraitEffectsMatteWithPixelBuffer:error:
--
-- Returns an AVPortraitEffectsMatte instance wrapping the replacement pixel buffer.
--
-- @pixelBuffer@ — A pixel buffer containing a portrait effects matting image, represented as kCVPixelFormatType_OneComponent8 with a kCVImageBufferTransferFunction_Linear transfer function.
--
-- @outError@ — On return, if the AVPortraitEffectsMatte cannot be created, points to an NSError describing the problem.
--
-- Returns: An AVPortraitEffectsMatte instance, or nil if the pixel buffer is malformed.
--
-- When applying complex edits to media containing a portrait effects matte, you may create a derivative matte with arbitrary transforms applied to it, then use this initializer to create a new AVPortraitEffectsMatte.
--
-- ObjC selector: @- portraitEffectsMatteByReplacingPortraitEffectsMatteWithPixelBuffer:error:@
portraitEffectsMatteByReplacingPortraitEffectsMatteWithPixelBuffer_error :: (IsAVPortraitEffectsMatte avPortraitEffectsMatte, IsNSError outError) => avPortraitEffectsMatte -> Ptr () -> outError -> IO (Id AVPortraitEffectsMatte)
portraitEffectsMatteByReplacingPortraitEffectsMatteWithPixelBuffer_error avPortraitEffectsMatte pixelBuffer outError =
  sendMessage avPortraitEffectsMatte portraitEffectsMatteByReplacingPortraitEffectsMatteWithPixelBuffer_errorSelector pixelBuffer (toNSError outError)

-- | dictionaryRepresentationForAuxiliaryDataType:
--
-- Returns a dictionary of primitive map information to be used when writing an image file with a portrait effects matte.
--
-- @outAuxDataType@ — On output, the auxiliary data type to be used when calling CGImageDestinationAddAuxiliaryDataInfo. Currently the only supported auxiliary data type is kCGImageAuxiliaryDataTypePortraitEffectsMatte.
--
-- Returns: A dictionary of CGImageDestination compatible portrait effects matte information, or nil if the auxDataType is unsupported.
--
-- When using ImageIO framework's CGImageDestination API to write portrait effects matte information to a HEIF or JPEG file, you may use this method to generate a dictionary of primitive map information consumed by CGImageDestinationAddAuxiliaryDataInfo.
--
-- ObjC selector: @- dictionaryRepresentationForAuxiliaryDataType:@
dictionaryRepresentationForAuxiliaryDataType :: (IsAVPortraitEffectsMatte avPortraitEffectsMatte, IsNSString outAuxDataType) => avPortraitEffectsMatte -> outAuxDataType -> IO (Id NSDictionary)
dictionaryRepresentationForAuxiliaryDataType avPortraitEffectsMatte outAuxDataType =
  sendMessage avPortraitEffectsMatte dictionaryRepresentationForAuxiliaryDataTypeSelector (toNSString outAuxDataType)

-- | pixelFormatType
--
-- Specifies the pixel format type of this object's internal matting image.
--
-- Currently the only supported CV pixel format type for the matting image is kCVPixelFormatType_OneComponent8.
--
-- ObjC selector: @- pixelFormatType@
pixelFormatType :: IsAVPortraitEffectsMatte avPortraitEffectsMatte => avPortraitEffectsMatte -> IO CUInt
pixelFormatType avPortraitEffectsMatte =
  sendMessage avPortraitEffectsMatte pixelFormatTypeSelector

-- | mattingImage
--
-- Provides access to the portrait effects matte's internal image.
--
-- The pixel format can be queried using the pixelFormatType property.
--
-- ObjC selector: @- mattingImage@
mattingImage :: IsAVPortraitEffectsMatte avPortraitEffectsMatte => avPortraitEffectsMatte -> IO (Ptr ())
mattingImage avPortraitEffectsMatte =
  sendMessage avPortraitEffectsMatte mattingImageSelector

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @init@
initSelector :: Selector '[] (Id AVPortraitEffectsMatte)
initSelector = mkSelector "init"

-- | @Selector@ for @new@
newSelector :: Selector '[] (Id AVPortraitEffectsMatte)
newSelector = mkSelector "new"

-- | @Selector@ for @portraitEffectsMatteFromDictionaryRepresentation:error:@
portraitEffectsMatteFromDictionaryRepresentation_errorSelector :: Selector '[Id NSDictionary, Id NSError] (Id AVPortraitEffectsMatte)
portraitEffectsMatteFromDictionaryRepresentation_errorSelector = mkSelector "portraitEffectsMatteFromDictionaryRepresentation:error:"

-- | @Selector@ for @portraitEffectsMatteByApplyingExifOrientation:@
portraitEffectsMatteByApplyingExifOrientationSelector :: Selector '[CInt] (Id AVPortraitEffectsMatte)
portraitEffectsMatteByApplyingExifOrientationSelector = mkSelector "portraitEffectsMatteByApplyingExifOrientation:"

-- | @Selector@ for @portraitEffectsMatteByReplacingPortraitEffectsMatteWithPixelBuffer:error:@
portraitEffectsMatteByReplacingPortraitEffectsMatteWithPixelBuffer_errorSelector :: Selector '[Ptr (), Id NSError] (Id AVPortraitEffectsMatte)
portraitEffectsMatteByReplacingPortraitEffectsMatteWithPixelBuffer_errorSelector = mkSelector "portraitEffectsMatteByReplacingPortraitEffectsMatteWithPixelBuffer:error:"

-- | @Selector@ for @dictionaryRepresentationForAuxiliaryDataType:@
dictionaryRepresentationForAuxiliaryDataTypeSelector :: Selector '[Id NSString] (Id NSDictionary)
dictionaryRepresentationForAuxiliaryDataTypeSelector = mkSelector "dictionaryRepresentationForAuxiliaryDataType:"

-- | @Selector@ for @pixelFormatType@
pixelFormatTypeSelector :: Selector '[] CUInt
pixelFormatTypeSelector = mkSelector "pixelFormatType"

-- | @Selector@ for @mattingImage@
mattingImageSelector :: Selector '[] (Ptr ())
mattingImageSelector = mkSelector "mattingImage"

