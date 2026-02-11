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
  , initSelector
  , newSelector
  , semanticSegmentationMatteFromImageSourceAuxiliaryDataType_dictionaryRepresentation_errorSelector
  , semanticSegmentationMatteByApplyingExifOrientationSelector
  , semanticSegmentationMatteByReplacingSemanticSegmentationMatteWithPixelBuffer_errorSelector
  , dictionaryRepresentationForAuxiliaryDataTypeSelector
  , matteTypeSelector
  , pixelFormatTypeSelector
  , mattingImageSelector


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

import ObjC.AVFoundation.Internal.Classes
import ObjC.Foundation.Internal.Classes

-- | @- init@
init_ :: IsAVSemanticSegmentationMatte avSemanticSegmentationMatte => avSemanticSegmentationMatte -> IO (Id AVSemanticSegmentationMatte)
init_ avSemanticSegmentationMatte  =
  sendMsg avSemanticSegmentationMatte (mkSelector "init") (retPtr retVoid) [] >>= ownedObject . castPtr

-- | @+ new@
new :: IO (Id AVSemanticSegmentationMatte)
new  =
  do
    cls' <- getRequiredClass "AVSemanticSegmentationMatte"
    sendClassMsg cls' (mkSelector "new") (retPtr retVoid) [] >>= ownedObject . castPtr

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
    withObjCPtr imageSourceAuxiliaryDataInfoDictionary $ \raw_imageSourceAuxiliaryDataInfoDictionary ->
      withObjCPtr outError $ \raw_outError ->
        sendClassMsg cls' (mkSelector "semanticSegmentationMatteFromImageSourceAuxiliaryDataType:dictionaryRepresentation:error:") (retPtr retVoid) [argPtr (castPtr (unRawId imageSourceAuxiliaryDataType) :: Ptr ()), argPtr (castPtr raw_imageSourceAuxiliaryDataInfoDictionary :: Ptr ()), argPtr (castPtr raw_outError :: Ptr ())] >>= retainedObject . castPtr

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
semanticSegmentationMatteByApplyingExifOrientation avSemanticSegmentationMatte  exifOrientation =
  sendMsg avSemanticSegmentationMatte (mkSelector "semanticSegmentationMatteByApplyingExifOrientation:") (retPtr retVoid) [argCInt (fromIntegral exifOrientation)] >>= retainedObject . castPtr

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
semanticSegmentationMatteByReplacingSemanticSegmentationMatteWithPixelBuffer_error avSemanticSegmentationMatte  pixelBuffer outError =
withObjCPtr outError $ \raw_outError ->
    sendMsg avSemanticSegmentationMatte (mkSelector "semanticSegmentationMatteByReplacingSemanticSegmentationMatteWithPixelBuffer:error:") (retPtr retVoid) [argPtr pixelBuffer, argPtr (castPtr raw_outError :: Ptr ())] >>= retainedObject . castPtr

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
dictionaryRepresentationForAuxiliaryDataType avSemanticSegmentationMatte  outAuxDataType =
withObjCPtr outAuxDataType $ \raw_outAuxDataType ->
    sendMsg avSemanticSegmentationMatte (mkSelector "dictionaryRepresentationForAuxiliaryDataType:") (retPtr retVoid) [argPtr (castPtr raw_outAuxDataType :: Ptr ())] >>= retainedObject . castPtr

-- | matteType
--
-- Specifies the receiver's semantic segmentation matting image type.
--
-- An AVSemanticSegmentationMatte's matteType is immutable for the life of the object.
--
-- ObjC selector: @- matteType@
matteType :: IsAVSemanticSegmentationMatte avSemanticSegmentationMatte => avSemanticSegmentationMatte -> IO (Id NSString)
matteType avSemanticSegmentationMatte  =
  sendMsg avSemanticSegmentationMatte (mkSelector "matteType") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | pixelFormatType
--
-- Specifies the pixel format type of this object's internal matting image.
--
-- Currently the only supported CV pixel format type for the matting image is kCVPixelFormatType_OneComponent8.
--
-- ObjC selector: @- pixelFormatType@
pixelFormatType :: IsAVSemanticSegmentationMatte avSemanticSegmentationMatte => avSemanticSegmentationMatte -> IO CUInt
pixelFormatType avSemanticSegmentationMatte  =
  sendMsg avSemanticSegmentationMatte (mkSelector "pixelFormatType") retCUInt []

-- | mattingImage
--
-- Provides access to the semantic segmentation matte's internal image.
--
-- The pixel format can be queried using the pixelFormatType property.
--
-- ObjC selector: @- mattingImage@
mattingImage :: IsAVSemanticSegmentationMatte avSemanticSegmentationMatte => avSemanticSegmentationMatte -> IO (Ptr ())
mattingImage avSemanticSegmentationMatte  =
  fmap castPtr $ sendMsg avSemanticSegmentationMatte (mkSelector "mattingImage") (retPtr retVoid) []

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @init@
initSelector :: Selector
initSelector = mkSelector "init"

-- | @Selector@ for @new@
newSelector :: Selector
newSelector = mkSelector "new"

-- | @Selector@ for @semanticSegmentationMatteFromImageSourceAuxiliaryDataType:dictionaryRepresentation:error:@
semanticSegmentationMatteFromImageSourceAuxiliaryDataType_dictionaryRepresentation_errorSelector :: Selector
semanticSegmentationMatteFromImageSourceAuxiliaryDataType_dictionaryRepresentation_errorSelector = mkSelector "semanticSegmentationMatteFromImageSourceAuxiliaryDataType:dictionaryRepresentation:error:"

-- | @Selector@ for @semanticSegmentationMatteByApplyingExifOrientation:@
semanticSegmentationMatteByApplyingExifOrientationSelector :: Selector
semanticSegmentationMatteByApplyingExifOrientationSelector = mkSelector "semanticSegmentationMatteByApplyingExifOrientation:"

-- | @Selector@ for @semanticSegmentationMatteByReplacingSemanticSegmentationMatteWithPixelBuffer:error:@
semanticSegmentationMatteByReplacingSemanticSegmentationMatteWithPixelBuffer_errorSelector :: Selector
semanticSegmentationMatteByReplacingSemanticSegmentationMatteWithPixelBuffer_errorSelector = mkSelector "semanticSegmentationMatteByReplacingSemanticSegmentationMatteWithPixelBuffer:error:"

-- | @Selector@ for @dictionaryRepresentationForAuxiliaryDataType:@
dictionaryRepresentationForAuxiliaryDataTypeSelector :: Selector
dictionaryRepresentationForAuxiliaryDataTypeSelector = mkSelector "dictionaryRepresentationForAuxiliaryDataType:"

-- | @Selector@ for @matteType@
matteTypeSelector :: Selector
matteTypeSelector = mkSelector "matteType"

-- | @Selector@ for @pixelFormatType@
pixelFormatTypeSelector :: Selector
pixelFormatTypeSelector = mkSelector "pixelFormatType"

-- | @Selector@ for @mattingImage@
mattingImageSelector :: Selector
mattingImageSelector = mkSelector "mattingImage"

