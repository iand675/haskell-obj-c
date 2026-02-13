{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | AVSemanticSegmentationMatte
--
-- An object wrapping a matting image for a particular semantic segmentation.
--
-- The pixel data in the matting image is represented in CVPixelBuffers as kCVPixelFormatType_OneComponent8 ('L008'). It is stored in image files as an auxiliary image, accessible using CGImageSourceCopyAuxiliaryDataInfoAtIndex using data types defined in <ImageIO/CGImageProperties.h>.
--
-- Generated bindings for @AVSemanticSegmentationMatte@.
module ObjC.AVFoundation.AVSemanticSegmentationMatte
  ( AVSemanticSegmentationMatte
  , IsAVSemanticSegmentationMatte(..)
  , init_
  , new
  , semanticSegmentationMatteFromImageSourceAuxiliaryDataType_dictionaryRepresentation_error
  , semanticSegmentationMatteByApplyingExifOrientation
  , semanticSegmentationMatteByReplacingSemanticSegmentationMatteWithPixelBuffer_error
  , dictionaryRepresentationForAuxiliaryDataType
  , matteType
  , pixelFormatType
  , mattingImage
  , dictionaryRepresentationForAuxiliaryDataTypeSelector
  , initSelector
  , matteTypeSelector
  , mattingImageSelector
  , newSelector
  , pixelFormatTypeSelector
  , semanticSegmentationMatteByApplyingExifOrientationSelector
  , semanticSegmentationMatteByReplacingSemanticSegmentationMatteWithPixelBuffer_errorSelector
  , semanticSegmentationMatteFromImageSourceAuxiliaryDataType_dictionaryRepresentation_errorSelector


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
init_ :: IsAVSemanticSegmentationMatte avSemanticSegmentationMatte => avSemanticSegmentationMatte -> IO (Id AVSemanticSegmentationMatte)
init_ avSemanticSegmentationMatte =
  sendOwnedMessage avSemanticSegmentationMatte initSelector

-- | @+ new@
new :: IO (Id AVSemanticSegmentationMatte)
new  =
  do
    cls' <- getRequiredClass "AVSemanticSegmentationMatte"
    sendOwnedClassMessage cls' newSelector

-- | semanticSegmentationMatteFromDictionaryRepresentation:error:
--
-- Returns an AVSemanticSegmentationMatte instance from auxiliary image information in an image file.
--
-- @imageSourceAuxiliaryDataType@ — The kCGImageAuxiliaryDataType constant corresponding to the semantic segmentation matte being created (see <ImageIO/CGImageProperties.h>.
--
-- @imageSourceAuxiliaryDataInfoDictionary@ — A dictionary of primitive semantic segmentation matte related information obtained from CGImageSourceCopyAuxiliaryDataInfoAtIndex.
--
-- @outError@ — On return, if the semantic segmentation matte cannot be created, points to an NSError describing the problem.
--
-- Returns: An AVSemanticSegmentationMatte instance, or nil if the auxiliary data info dictionary was malformed.
--
-- When using ImageIO framework's CGImageSource API to read from a HEIF or JPEG file containing a semantic segmentation matte, AVSemanticSegmentationMatte can be instantiated using the result of CGImageSourceCopyAuxiliaryDataInfoAtIndex, which returns a CFDictionary of primitive segmentation matte information.
--
-- ObjC selector: @+ semanticSegmentationMatteFromImageSourceAuxiliaryDataType:dictionaryRepresentation:error:@
semanticSegmentationMatteFromImageSourceAuxiliaryDataType_dictionaryRepresentation_error :: (IsNSDictionary imageSourceAuxiliaryDataInfoDictionary, IsNSError outError) => RawId -> imageSourceAuxiliaryDataInfoDictionary -> outError -> IO (Id AVSemanticSegmentationMatte)
semanticSegmentationMatteFromImageSourceAuxiliaryDataType_dictionaryRepresentation_error imageSourceAuxiliaryDataType imageSourceAuxiliaryDataInfoDictionary outError =
  do
    cls' <- getRequiredClass "AVSemanticSegmentationMatte"
    sendClassMessage cls' semanticSegmentationMatteFromImageSourceAuxiliaryDataType_dictionaryRepresentation_errorSelector imageSourceAuxiliaryDataType (toNSDictionary imageSourceAuxiliaryDataInfoDictionary) (toNSError outError)

-- | semanticSegmentationMatteByApplyingExifOrientation:
--
-- Returns a derivative AVSemanticSegmentationMatte instance in which the specified Exif orientation has been applied.
--
-- @exifOrientation@ — One of the 8 standard Exif orientation tags expressing how the matte should be rotated / mirrored.
--
-- Returns: An AVSemanticSegmentationMatte's instance.
--
-- When applying simple 90 degree rotation or mirroring edits to media containing a semantic segmentation matte, you may use this initializer to create a derivative copy of the matte in which the specified orientation is applied. This method throws an NSInvalidArgumentException if you pass an unrecognized exifOrientation.
--
-- ObjC selector: @- semanticSegmentationMatteByApplyingExifOrientation:@
semanticSegmentationMatteByApplyingExifOrientation :: IsAVSemanticSegmentationMatte avSemanticSegmentationMatte => avSemanticSegmentationMatte -> CInt -> IO (Id AVSemanticSegmentationMatte)
semanticSegmentationMatteByApplyingExifOrientation avSemanticSegmentationMatte exifOrientation =
  sendMessage avSemanticSegmentationMatte semanticSegmentationMatteByApplyingExifOrientationSelector exifOrientation

-- | semanticSegmentationMatteByReplacingSemanticSegmentationMatteWithPixelBuffer:error:
--
-- Returns an AVSemanticSegmentationMatte instance wrapping the replacement pixel buffer.
--
-- @pixelBuffer@ — A pixel buffer containing a semantic segmentation matting image, represented as kCVPixelFormatType_OneComponent8 with a kCVImageBufferTransferFunction_Linear transfer function.
--
-- @outError@ — On return, if the AVSemanticSegmentationMatte cannot be created, points to an NSError describing the problem.
--
-- Returns: An AVSemanticSegmentationMatte instance, or nil if the pixel buffer is malformed.
--
-- When applying complex edits to media containing a semantic segmentation matte, you may create a derivative matte with arbitrary transforms applied to it, then use this initializer to create a new AVSemanticSegmentationMatte.
--
-- ObjC selector: @- semanticSegmentationMatteByReplacingSemanticSegmentationMatteWithPixelBuffer:error:@
semanticSegmentationMatteByReplacingSemanticSegmentationMatteWithPixelBuffer_error :: (IsAVSemanticSegmentationMatte avSemanticSegmentationMatte, IsNSError outError) => avSemanticSegmentationMatte -> Ptr () -> outError -> IO (Id AVSemanticSegmentationMatte)
semanticSegmentationMatteByReplacingSemanticSegmentationMatteWithPixelBuffer_error avSemanticSegmentationMatte pixelBuffer outError =
  sendMessage avSemanticSegmentationMatte semanticSegmentationMatteByReplacingSemanticSegmentationMatteWithPixelBuffer_errorSelector pixelBuffer (toNSError outError)

-- | dictionaryRepresentationForAuxiliaryDataType:
--
-- Returns a dictionary of primitive map information to be used when writing an image file with a semantic segmentation matte.
--
-- @outAuxDataType@ — On output, the auxiliary data type to be used when calling CGImageDestinationAddAuxiliaryDataInfo. Currently supported auxiliary data types are enumerated in <ImageIO/CGImageProperties.h>
--
-- Returns: A dictionary of CGImageDestination compatible semantic segmentation matte information, or nil if the auxDataType is unsupported.
--
-- When using ImageIO framework's CGImageDestination API to write semantic segmentation matte information to a HEIF or JPEG file, you may use this method to generate a dictionary of primitive map information consumed by CGImageDestinationAddAuxiliaryDataInfo.
--
-- ObjC selector: @- dictionaryRepresentationForAuxiliaryDataType:@
dictionaryRepresentationForAuxiliaryDataType :: (IsAVSemanticSegmentationMatte avSemanticSegmentationMatte, IsNSString outAuxDataType) => avSemanticSegmentationMatte -> outAuxDataType -> IO (Id NSDictionary)
dictionaryRepresentationForAuxiliaryDataType avSemanticSegmentationMatte outAuxDataType =
  sendMessage avSemanticSegmentationMatte dictionaryRepresentationForAuxiliaryDataTypeSelector (toNSString outAuxDataType)

-- | matteType
--
-- Specifies the receiver's semantic segmentation matting image type.
--
-- An AVSemanticSegmentationMatte's matteType is immutable for the life of the object.
--
-- ObjC selector: @- matteType@
matteType :: IsAVSemanticSegmentationMatte avSemanticSegmentationMatte => avSemanticSegmentationMatte -> IO (Id NSString)
matteType avSemanticSegmentationMatte =
  sendMessage avSemanticSegmentationMatte matteTypeSelector

-- | pixelFormatType
--
-- Specifies the pixel format type of this object's internal matting image.
--
-- Currently the only supported CV pixel format type for the matting image is kCVPixelFormatType_OneComponent8.
--
-- ObjC selector: @- pixelFormatType@
pixelFormatType :: IsAVSemanticSegmentationMatte avSemanticSegmentationMatte => avSemanticSegmentationMatte -> IO CUInt
pixelFormatType avSemanticSegmentationMatte =
  sendMessage avSemanticSegmentationMatte pixelFormatTypeSelector

-- | mattingImage
--
-- Provides access to the semantic segmentation matte's internal image.
--
-- The pixel format can be queried using the pixelFormatType property.
--
-- ObjC selector: @- mattingImage@
mattingImage :: IsAVSemanticSegmentationMatte avSemanticSegmentationMatte => avSemanticSegmentationMatte -> IO (Ptr ())
mattingImage avSemanticSegmentationMatte =
  sendMessage avSemanticSegmentationMatte mattingImageSelector

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @init@
initSelector :: Selector '[] (Id AVSemanticSegmentationMatte)
initSelector = mkSelector "init"

-- | @Selector@ for @new@
newSelector :: Selector '[] (Id AVSemanticSegmentationMatte)
newSelector = mkSelector "new"

-- | @Selector@ for @semanticSegmentationMatteFromImageSourceAuxiliaryDataType:dictionaryRepresentation:error:@
semanticSegmentationMatteFromImageSourceAuxiliaryDataType_dictionaryRepresentation_errorSelector :: Selector '[RawId, Id NSDictionary, Id NSError] (Id AVSemanticSegmentationMatte)
semanticSegmentationMatteFromImageSourceAuxiliaryDataType_dictionaryRepresentation_errorSelector = mkSelector "semanticSegmentationMatteFromImageSourceAuxiliaryDataType:dictionaryRepresentation:error:"

-- | @Selector@ for @semanticSegmentationMatteByApplyingExifOrientation:@
semanticSegmentationMatteByApplyingExifOrientationSelector :: Selector '[CInt] (Id AVSemanticSegmentationMatte)
semanticSegmentationMatteByApplyingExifOrientationSelector = mkSelector "semanticSegmentationMatteByApplyingExifOrientation:"

-- | @Selector@ for @semanticSegmentationMatteByReplacingSemanticSegmentationMatteWithPixelBuffer:error:@
semanticSegmentationMatteByReplacingSemanticSegmentationMatteWithPixelBuffer_errorSelector :: Selector '[Ptr (), Id NSError] (Id AVSemanticSegmentationMatte)
semanticSegmentationMatteByReplacingSemanticSegmentationMatteWithPixelBuffer_errorSelector = mkSelector "semanticSegmentationMatteByReplacingSemanticSegmentationMatteWithPixelBuffer:error:"

-- | @Selector@ for @dictionaryRepresentationForAuxiliaryDataType:@
dictionaryRepresentationForAuxiliaryDataTypeSelector :: Selector '[Id NSString] (Id NSDictionary)
dictionaryRepresentationForAuxiliaryDataTypeSelector = mkSelector "dictionaryRepresentationForAuxiliaryDataType:"

-- | @Selector@ for @matteType@
matteTypeSelector :: Selector '[] (Id NSString)
matteTypeSelector = mkSelector "matteType"

-- | @Selector@ for @pixelFormatType@
pixelFormatTypeSelector :: Selector '[] CUInt
pixelFormatTypeSelector = mkSelector "pixelFormatType"

-- | @Selector@ for @mattingImage@
mattingImageSelector :: Selector '[] (Ptr ())
mattingImageSelector = mkSelector "mattingImage"

