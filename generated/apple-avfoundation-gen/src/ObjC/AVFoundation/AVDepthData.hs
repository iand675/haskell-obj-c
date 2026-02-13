{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | AVDepthData
--
-- An object wrapping a map of disparity or depth pixel data, plus metadata.
--
-- "Depth Data" is a generic term for a map of pixel data containing depth-related information. AVDepthData wraps a disparity or depth map and provides conversion methods, focus information, and camera calibration data to aid in using the map for rendering or computer vision tasks. CoreVideo supports the following four depth data pixel formats:          kCVPixelFormatType_DisparityFloat16    = 'hdis'          kCVPixelFormatType_DisparityFloat32    = 'fdis'          kCVPixelFormatType_DepthFloat16        = 'hdep'          kCVPixelFormatType_DepthFloat32        = 'fdep'
--
-- The disparity formats describe normalized shift values when comparing two images. Units are 1/meters: ( pixelShift / (pixelFocalLength * baselineInMeters) ).     The depth formats describe the distance to an object in meters.
--
-- Disparity / depth maps are generated from camera images containing non-rectilinear data. Camera lenses have small imperfections that cause small distortions in their resultant images compared to a pinhole camera. AVDepthData maps contain non-rectilinear (non-distortion-corrected) data as well. Their values are warped to match the lens distortion characteristics present in their accompanying YUV image. Therefore an AVDepthData map can be used as a proxy for depth when rendering effects to its accompanying image, but not to correlate points in 3D space. In order to use AVDepthData for computer vision tasks, you should use its accompanying camera calibration data to rectify the depth data (see AVCameraCalibrationData).
--
-- When capturing depth data from a camera using AVCaptureDepthDataOutput, AVDepthData objects are delivered to your AVCaptureDepthDataOutputDelegate in a streaming fashion. When capturing depth data along with photos using AVCapturePhotoOutput, depth data is delivered to your AVCapturePhotoCaptureDelegate as a property of an AVCapturePhoto (see -[AVCapturePhotoCaptureDelegate captureOutput:didFinishProcessingPhoto:error:]). When working with image files containing depth information, AVDepthData may be instantiated using information obtained from ImageIO. When editing images containing depth information, derivative AVDepthData objects may be instantiated reflecting the edits that have been performed.
--
-- Generated bindings for @AVDepthData@.
module ObjC.AVFoundation.AVDepthData
  ( AVDepthData
  , IsAVDepthData(..)
  , init_
  , new
  , depthDataFromDictionaryRepresentation_error
  , depthDataByConvertingToDepthDataType
  , depthDataByApplyingExifOrientation
  , depthDataByReplacingDepthDataMapWithPixelBuffer_error
  , dictionaryRepresentationForAuxiliaryDataType
  , availableDepthDataTypes
  , depthDataType
  , depthDataMap
  , depthDataQuality
  , depthDataFiltered
  , depthDataAccuracy
  , cameraCalibrationData
  , availableDepthDataTypesSelector
  , cameraCalibrationDataSelector
  , depthDataAccuracySelector
  , depthDataByApplyingExifOrientationSelector
  , depthDataByConvertingToDepthDataTypeSelector
  , depthDataByReplacingDepthDataMapWithPixelBuffer_errorSelector
  , depthDataFilteredSelector
  , depthDataFromDictionaryRepresentation_errorSelector
  , depthDataMapSelector
  , depthDataQualitySelector
  , depthDataTypeSelector
  , dictionaryRepresentationForAuxiliaryDataTypeSelector
  , initSelector
  , newSelector

  -- * Enum types
  , AVDepthDataAccuracy(AVDepthDataAccuracy)
  , pattern AVDepthDataAccuracyRelative
  , pattern AVDepthDataAccuracyAbsolute
  , AVDepthDataQuality(AVDepthDataQuality)
  , pattern AVDepthDataQualityLow
  , pattern AVDepthDataQualityHigh

  ) where

import Foreign.Ptr (Ptr, FunPtr)
import Foreign.C.Types

import ObjC.Runtime.Types
import ObjC.Runtime.Message (sendMessage, sendOwnedMessage, sendClassMessage, sendOwnedClassMessage)
import ObjC.Runtime.Selector (mkSelector)
import ObjC.Runtime.Class (getRequiredClass)

import ObjC.AVFoundation.Internal.Classes
import ObjC.AVFoundation.Internal.Enums
import ObjC.Foundation.Internal.Classes

-- | @- init@
init_ :: IsAVDepthData avDepthData => avDepthData -> IO (Id AVDepthData)
init_ avDepthData =
  sendOwnedMessage avDepthData initSelector

-- | @+ new@
new :: IO (Id AVDepthData)
new  =
  do
    cls' <- getRequiredClass "AVDepthData"
    sendOwnedClassMessage cls' newSelector

-- | depthDataFromDictionaryRepresentation:error:
--
-- Returns an AVDepthData instance from depth information in an image file.
--
-- @imageSourceAuxDataInfoDictionary@ — A dictionary of primitive depth-related information obtained from CGImageSourceCopyAuxiliaryDataInfoAtIndex.
--
-- @outError@ — On return, if the depth data cannot be created, points to an NSError describing the problem.
--
-- Returns: An AVDepthData instance, or nil if the auxiliary data info dictionary was malformed.
--
-- When using ImageIO framework's CGImageSource API to read from a HEIF or JPEG file containing depth data, AVDepthData can be instantiated using the result of CGImageSourceCopyAuxiliaryDataInfoAtIndex, which returns a CFDictionary of primitive map information.
--
-- ObjC selector: @+ depthDataFromDictionaryRepresentation:error:@
depthDataFromDictionaryRepresentation_error :: (IsNSDictionary imageSourceAuxDataInfoDictionary, IsNSError outError) => imageSourceAuxDataInfoDictionary -> outError -> IO (Id AVDepthData)
depthDataFromDictionaryRepresentation_error imageSourceAuxDataInfoDictionary outError =
  do
    cls' <- getRequiredClass "AVDepthData"
    sendClassMessage cls' depthDataFromDictionaryRepresentation_errorSelector (toNSDictionary imageSourceAuxDataInfoDictionary) (toNSError outError)

-- | depthDataByConvertingToDepthDataType:
--
-- Returns a converted, derivative AVDepthData instance in the specified depthDataType.
--
-- @depthDataType@ — The OSType of depthData object to which you'd like to convert. Must be present in availableDepthDataTypes.
--
-- Returns: An AVDepthData instance.
--
-- This method throws an NSInvalidArgumentException if you pass an unrecognized depthDataType. See
--
-- ObjC selector: @- depthDataByConvertingToDepthDataType:@
depthDataByConvertingToDepthDataType :: IsAVDepthData avDepthData => avDepthData -> CUInt -> IO (Id AVDepthData)
depthDataByConvertingToDepthDataType avDepthData depthDataType =
  sendMessage avDepthData depthDataByConvertingToDepthDataTypeSelector depthDataType

-- | depthDataByApplyingExifOrientation:
--
-- Returns a derivative AVDepthData instance in which the specified Exif orientation has been applied.
--
-- @exifOrientation@ — One of the 8 standard Exif orientation tags expressing how the depth data should be rotated / mirrored.
--
-- Returns: An AVDepthData instance.
--
-- When applying simple 90 degree rotation or mirroring edits to media containing depth data, you may use this initializer to create a derivative copy of the depth in which the specified orientation is applied to both the underlying pixel map data and the camera calibration data. This method throws an NSInvalidArgumentException if you pass an unrecognized exifOrientation.
--
-- ObjC selector: @- depthDataByApplyingExifOrientation:@
depthDataByApplyingExifOrientation :: IsAVDepthData avDepthData => avDepthData -> CInt -> IO (Id AVDepthData)
depthDataByApplyingExifOrientation avDepthData exifOrientation =
  sendMessage avDepthData depthDataByApplyingExifOrientationSelector exifOrientation

-- | depthDataByReplacingDepthDataMapWithPixelBuffer:error:
--
-- Returns an AVDepthData instance wrapping the replacement depth data map pixel buffer.
--
-- @pixelBuffer@ — A pixel buffer containing depth data information in one of the 4 supported disparity / depth pixel formats.
--
-- @outError@ — On return, if the depth data cannot be created, points to an NSError describing the problem.
--
-- Returns: An AVDepthData instance, or nil if the pixel buffer is malformed.
--
-- When applying complex edits to media containing depth data, you may create a derivative map with arbitrary transforms applied to it, then use this initializer to create a new AVDepthData. Note that this new depth data object has no camera calibration data, so its cameraCalibrationData property always returns nil.
--
-- ObjC selector: @- depthDataByReplacingDepthDataMapWithPixelBuffer:error:@
depthDataByReplacingDepthDataMapWithPixelBuffer_error :: (IsAVDepthData avDepthData, IsNSError outError) => avDepthData -> Ptr () -> outError -> IO (Id AVDepthData)
depthDataByReplacingDepthDataMapWithPixelBuffer_error avDepthData pixelBuffer outError =
  sendMessage avDepthData depthDataByReplacingDepthDataMapWithPixelBuffer_errorSelector pixelBuffer (toNSError outError)

-- | dictionaryRepresentationForAuxiliaryDataType:
--
-- Returns a dictionary of primitive map information to be used when writing an image file with depth data.
--
-- @outAuxDataType@ — On output, either kCGImageAuxiliaryDataTypeDisparity or kCGImageAuxiliaryDataTypeDepth, depending on the depth data's file.
--
-- Returns: A dictionary of CGImageDestination compatible depth information, or nil if the auxDataType is unsupported.
--
-- When using ImageIO framework's CGImageDestination API to write depth data to a HEIF or JPEG file, you may use this method to generate a dictionary of primitive map information consumed by CGImageDestinationAddAuxiliaryDataInfo.
--
-- ObjC selector: @- dictionaryRepresentationForAuxiliaryDataType:@
dictionaryRepresentationForAuxiliaryDataType :: (IsAVDepthData avDepthData, IsNSString outAuxDataType) => avDepthData -> outAuxDataType -> IO (Id NSDictionary)
dictionaryRepresentationForAuxiliaryDataType avDepthData outAuxDataType =
  sendMessage avDepthData dictionaryRepresentationForAuxiliaryDataTypeSelector (toNSString outAuxDataType)

-- | availableDepthDataTypes
--
-- Specifies which depth data pixel formats may be used with depthDataByConvertingToDepthDataType:.
--
-- This property presents the available pixel format types as an array of NSNumbers, each wrapping an OSType (CV pixel format type).
--
-- ObjC selector: @- availableDepthDataTypes@
availableDepthDataTypes :: IsAVDepthData avDepthData => avDepthData -> IO (Id NSArray)
availableDepthDataTypes avDepthData =
  sendMessage avDepthData availableDepthDataTypesSelector

-- | depthDataType
--
-- Specifies the pixel format type of this depth data object's internal map.
--
-- One of kCVPixelFormatType_DisparityFloat16, kCVPixelFormatType_DisparityFloat32, kCVPixelFormatType_DepthFloat16, or kCVPixelFormatType_DepthFloat32.
--
-- ObjC selector: @- depthDataType@
depthDataType :: IsAVDepthData avDepthData => avDepthData -> IO CUInt
depthDataType avDepthData =
  sendMessage avDepthData depthDataTypeSelector

-- | depthDataMap
--
-- Provides access to the depth data object's internal map.
--
-- The depth data map's pixel format can be queried using the depthDataType property.
--
-- ObjC selector: @- depthDataMap@
depthDataMap :: IsAVDepthData avDepthData => avDepthData -> IO (Ptr ())
depthDataMap avDepthData =
  sendMessage avDepthData depthDataMapSelector

-- | depthDataQuality
--
-- Specifies the overall quality of the depth data map's values.
--
-- See AVDepthDataQuality documentation for more information.
--
-- ObjC selector: @- depthDataQuality@
depthDataQuality :: IsAVDepthData avDepthData => avDepthData -> IO AVDepthDataQuality
depthDataQuality avDepthData =
  sendMessage avDepthData depthDataQualitySelector

-- | depthDataFiltered
--
-- Specifies whether the depth data pixel buffer map contains filtered (hole-filled) data.
--
-- By setting either AVCaptureDepthDataOutput's filteringEnabled property or AVCapturePhotoSettings' depthDataFiltered property to YES, the resulting depth data are filtered to remove invalid pixel values that may be present due to a variety of factors including low light and lens occlusion. If you've requested depth data filtering, all depth data holes are filled. Note that filtering the depth data makes it more usable for applying effects, but alters the data such that it may no longer be suitable for computer vision tasks. Unfiltered depth maps present missing data as NaN.
--
-- ObjC selector: @- depthDataFiltered@
depthDataFiltered :: IsAVDepthData avDepthData => avDepthData -> IO Bool
depthDataFiltered avDepthData =
  sendMessage avDepthData depthDataFilteredSelector

-- | depthDataAccuracy
--
-- Specifies the accuracy of the units in the depth data map's values.
--
-- See AVDepthDataAccuracy documentation for more information.
--
-- ObjC selector: @- depthDataAccuracy@
depthDataAccuracy :: IsAVDepthData avDepthData => avDepthData -> IO AVDepthDataAccuracy
depthDataAccuracy avDepthData =
  sendMessage avDepthData depthDataAccuracySelector

-- | cameraCalibrationData
--
-- The calibration data of the camera with which AVDepthData map's values are aligned.
--
-- See AVCameraCalibrationData for more information. This property may return nil if no camera calibration data is available for the depth data.
--
-- ObjC selector: @- cameraCalibrationData@
cameraCalibrationData :: IsAVDepthData avDepthData => avDepthData -> IO (Id AVCameraCalibrationData)
cameraCalibrationData avDepthData =
  sendMessage avDepthData cameraCalibrationDataSelector

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @init@
initSelector :: Selector '[] (Id AVDepthData)
initSelector = mkSelector "init"

-- | @Selector@ for @new@
newSelector :: Selector '[] (Id AVDepthData)
newSelector = mkSelector "new"

-- | @Selector@ for @depthDataFromDictionaryRepresentation:error:@
depthDataFromDictionaryRepresentation_errorSelector :: Selector '[Id NSDictionary, Id NSError] (Id AVDepthData)
depthDataFromDictionaryRepresentation_errorSelector = mkSelector "depthDataFromDictionaryRepresentation:error:"

-- | @Selector@ for @depthDataByConvertingToDepthDataType:@
depthDataByConvertingToDepthDataTypeSelector :: Selector '[CUInt] (Id AVDepthData)
depthDataByConvertingToDepthDataTypeSelector = mkSelector "depthDataByConvertingToDepthDataType:"

-- | @Selector@ for @depthDataByApplyingExifOrientation:@
depthDataByApplyingExifOrientationSelector :: Selector '[CInt] (Id AVDepthData)
depthDataByApplyingExifOrientationSelector = mkSelector "depthDataByApplyingExifOrientation:"

-- | @Selector@ for @depthDataByReplacingDepthDataMapWithPixelBuffer:error:@
depthDataByReplacingDepthDataMapWithPixelBuffer_errorSelector :: Selector '[Ptr (), Id NSError] (Id AVDepthData)
depthDataByReplacingDepthDataMapWithPixelBuffer_errorSelector = mkSelector "depthDataByReplacingDepthDataMapWithPixelBuffer:error:"

-- | @Selector@ for @dictionaryRepresentationForAuxiliaryDataType:@
dictionaryRepresentationForAuxiliaryDataTypeSelector :: Selector '[Id NSString] (Id NSDictionary)
dictionaryRepresentationForAuxiliaryDataTypeSelector = mkSelector "dictionaryRepresentationForAuxiliaryDataType:"

-- | @Selector@ for @availableDepthDataTypes@
availableDepthDataTypesSelector :: Selector '[] (Id NSArray)
availableDepthDataTypesSelector = mkSelector "availableDepthDataTypes"

-- | @Selector@ for @depthDataType@
depthDataTypeSelector :: Selector '[] CUInt
depthDataTypeSelector = mkSelector "depthDataType"

-- | @Selector@ for @depthDataMap@
depthDataMapSelector :: Selector '[] (Ptr ())
depthDataMapSelector = mkSelector "depthDataMap"

-- | @Selector@ for @depthDataQuality@
depthDataQualitySelector :: Selector '[] AVDepthDataQuality
depthDataQualitySelector = mkSelector "depthDataQuality"

-- | @Selector@ for @depthDataFiltered@
depthDataFilteredSelector :: Selector '[] Bool
depthDataFilteredSelector = mkSelector "depthDataFiltered"

-- | @Selector@ for @depthDataAccuracy@
depthDataAccuracySelector :: Selector '[] AVDepthDataAccuracy
depthDataAccuracySelector = mkSelector "depthDataAccuracy"

-- | @Selector@ for @cameraCalibrationData@
cameraCalibrationDataSelector :: Selector '[] (Id AVCameraCalibrationData)
cameraCalibrationDataSelector = mkSelector "cameraCalibrationData"

