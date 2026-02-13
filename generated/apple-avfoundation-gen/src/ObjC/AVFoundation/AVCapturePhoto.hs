{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | AVCapturePhoto
--
-- An object representing a photo in memory, produced by the -captureOutput:didFinishingProcessingPhoto:error: in the AVCapturePhotoCaptureDelegate protocol method.
--
-- Beginning in iOS 11, AVCapturePhotoOutput's AVCapturePhotoCaptureDelegate supports a simplified callback for delivering image data, namely -captureOutput:didFinishingProcessingPhoto:error:. This callback presents each image result for your capture request as an AVCapturePhoto object, an immutable wrapper from which various properties of the photo capture may be queried, such as the photo's preview pixel buffer, metadata, depth data, camera calibration data, and image bracket specific properties. AVCapturePhoto can wrap file-containerized photo results, such as HEVC encoded image data, containerized in the HEIC file format. CMSampleBufferRef, on the other hand, may only be used to express non file format containerized photo data. For this reason, the AVCapturePhotoCaptureDelegate protocol methods that return CMSampleBuffers have been deprecated in favor of -captureOutput:didFinishingProcessingPhoto:error:. A AVCapturePhoto wraps a single image result. For instance, if you've requested a bracketed capture of 3 images, your callback is called 3 times, each time delivering an AVCapturePhoto.
--
-- Generated bindings for @AVCapturePhoto@.
module ObjC.AVFoundation.AVCapturePhoto
  ( AVCapturePhoto
  , IsAVCapturePhoto(..)
  , init_
  , new
  , semanticSegmentationMatteForType
  , fileDataRepresentation
  , fileDataRepresentationWithCustomizer
  , fileDataRepresentationWithReplacementMetadata_replacementEmbeddedThumbnailPhotoFormat_replacementEmbeddedThumbnailPixelBuffer_replacementDepthData
  , cgImageRepresentation
  , previewCGImageRepresentation
  , rawPhoto
  , pixelBuffer
  , previewPixelBuffer
  , embeddedThumbnailPhotoFormat
  , depthData
  , portraitEffectsMatte
  , metadata
  , cameraCalibrationData
  , resolvedSettings
  , photoCount
  , sourceDeviceType
  , constantColorConfidenceMap
  , constantColorCenterWeightedMeanConfidenceLevel
  , constantColorFallbackPhoto
  , bracketSettings
  , sequenceCount
  , lensStabilizationStatus
  , bracketSettingsSelector
  , cameraCalibrationDataSelector
  , cgImageRepresentationSelector
  , constantColorCenterWeightedMeanConfidenceLevelSelector
  , constantColorConfidenceMapSelector
  , constantColorFallbackPhotoSelector
  , depthDataSelector
  , embeddedThumbnailPhotoFormatSelector
  , fileDataRepresentationSelector
  , fileDataRepresentationWithCustomizerSelector
  , fileDataRepresentationWithReplacementMetadata_replacementEmbeddedThumbnailPhotoFormat_replacementEmbeddedThumbnailPixelBuffer_replacementDepthDataSelector
  , initSelector
  , lensStabilizationStatusSelector
  , metadataSelector
  , newSelector
  , photoCountSelector
  , pixelBufferSelector
  , portraitEffectsMatteSelector
  , previewCGImageRepresentationSelector
  , previewPixelBufferSelector
  , rawPhotoSelector
  , resolvedSettingsSelector
  , semanticSegmentationMatteForTypeSelector
  , sequenceCountSelector
  , sourceDeviceTypeSelector

  -- * Enum types
  , AVCaptureLensStabilizationStatus(AVCaptureLensStabilizationStatus)
  , pattern AVCaptureLensStabilizationStatusUnsupported
  , pattern AVCaptureLensStabilizationStatusOff
  , pattern AVCaptureLensStabilizationStatusActive
  , pattern AVCaptureLensStabilizationStatusOutOfRange
  , pattern AVCaptureLensStabilizationStatusUnavailable

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
init_ :: IsAVCapturePhoto avCapturePhoto => avCapturePhoto -> IO (Id AVCapturePhoto)
init_ avCapturePhoto =
  sendOwnedMessage avCapturePhoto initSelector

-- | @+ new@
new :: IO (Id AVCapturePhoto)
new  =
  do
    cls' <- getRequiredClass "AVCapturePhoto"
    sendOwnedClassMessage cls' newSelector

-- | semanticSegmentationMatteForType:
--
-- An accessor for semantic segmentation mattes associated with this photo.
--
-- @semanticSegmentationMatteType@ — The matte type of interest (hair, skin, etc).
--
-- Returns: An instance of AVSemanticSegmentationMatte, or nil if none could be found for the specified type.
--
-- If you requested one or more semantic segmentation mattes by calling -[AVCapturePhotoSettings setEnabledSemanticSegmentationMatteTypes:] with a non-empty array of types, this property offers access to the resulting AVSemanticSegmentationMatte objects. Nil is returned if you did not request semantic segmentation matte delivery, or if no mattes of the specified type are available. Note that semantic segmentation mattes are only embedded in the photo's internal file format container if you call -[AVCapturePhotoSettings setEmbedsSemanticSegmentationMattesInPhoto:YES].
--
-- ObjC selector: @- semanticSegmentationMatteForType:@
semanticSegmentationMatteForType :: (IsAVCapturePhoto avCapturePhoto, IsNSString semanticSegmentationMatteType) => avCapturePhoto -> semanticSegmentationMatteType -> IO (Id AVSemanticSegmentationMatte)
semanticSegmentationMatteForType avCapturePhoto semanticSegmentationMatteType =
  sendMessage avCapturePhoto semanticSegmentationMatteForTypeSelector (toNSString semanticSegmentationMatteType)

-- | fileDataRepresentation
--
-- Flattens the AVCapturePhoto to an NSData using the file container format (processedFileType or rawFileType) specified in the AVCapturePhotoSettings (e.g. JFIF, HEIF, DNG, DICOM).
--
-- Returns: An NSData containing bits in the file container's format, or nil if the flattening process fails.
--
-- ObjC selector: @- fileDataRepresentation@
fileDataRepresentation :: IsAVCapturePhoto avCapturePhoto => avCapturePhoto -> IO (Id NSData)
fileDataRepresentation avCapturePhoto =
  sendMessage avCapturePhoto fileDataRepresentationSelector

-- | fileDataRepresentationWithCustomizer:
--
-- Flattens the AVCapturePhoto to an NSData using the file container format (processedFileType or rawFileType) specified in the AVCapturePhotoSettings (e.g. JFIF, HEIF, DNG, DICOM), and allows you to strip or replace various pieces of metadata in the process.
--
-- @customizer@ — An object conforming to the AVCapturePhotoFileDataRepresentationCustomizer protocol that will be called synchronously to provide customization of metadata written to the container format. An NSInvalidArgumentException is thrown if you pass nil.
--
-- Returns: An NSData containing bits in the file container's format, or nil if the flattening process fails.
--
-- ObjC selector: @- fileDataRepresentationWithCustomizer:@
fileDataRepresentationWithCustomizer :: IsAVCapturePhoto avCapturePhoto => avCapturePhoto -> RawId -> IO (Id NSData)
fileDataRepresentationWithCustomizer avCapturePhoto customizer =
  sendMessage avCapturePhoto fileDataRepresentationWithCustomizerSelector customizer

-- | fileDataRepresentationWithReplacementMetadata:replacementEmbeddedThumbnailPhotoFormat:replacementEmbeddedThumbnailPixelBuffer:replacementDepthData:
--
-- Flattens the AVCapturePhoto to an NSData using the file container format (processedFileType or rawFileType) specified in the AVCapturePhotoSettings (e.g. JFIF, HEIF, DNG, DICOM), and allows you to replace metadata, thumbnail, and depth data in the process.
--
-- @replacementMetadata@ — A dictionary of keys and values from <ImageIO/CGImageProperties.h>. To preserve existing metadata to the file, pass self.metadata. To strip existing metadata, pass nil. To replace metadata, pass a replacement dictionary.
--
-- @replacementEmbeddedThumbnailPhotoFormat@ — A dictionary of keys and values from <AVFoundation/AVVideoSettings.h>. If you pass a non-nil dictionary, AVVideoCodecKey is required, with AVVideoWidthKey and AVVideoHeightKey being optional. To preserve the existing embedded thumbnail photo to the file, pass self.embeddedThumbnailPhotoFormat and pass nil as your replacementEmbeddedThumbnailPixelBuffer parameter. To strip the existing embedded thumbnail, pass nil for both replacementEmbeddedThumbnailPhotoFormat and replacementEmbeddedThumbnailPixelBuffer. To replace the existing embedded thumbnail photo, pass both a non-nil replacementThumbnailPixelBuffer and replacementEmbeddedThumbnailPhotoFormat dictionary.
--
-- @replacementEmbeddedThumbnailPixelBuffer@ — A pixel buffer containing a source image to be encoded to the file as the replacement thumbnail image. To preserve the existing embedded thumbnail photo to the file, pass self.embeddedThumbnailPhotoFormat as your replacementEmbeddedThumbnailPhotoFormat parameter and nil as your replacementEmbeddedThumbnailPixelBuffer parameter. To strip the existing embedded thumbnail, pass nil for both replacementEmbeddedThumbnailPhotoFormat and replacementEmbeddedThumbnailPixelBuffer. To replace the existing embedded thumbnail photo, pass both a non-nil replacementThumbnailPixelBuffer and replacementEmbeddedThumbnailPhotoFormat dictionary.
--
-- @replacementDepthData@ — Replacement depth data to be written to the flattened file container. To preserve existing depth data to the file, pass self.depthData. To strip it, pass nil. To replace it, pass a new AVDepthData instance.
--
-- Returns: An NSData containing bits in the file container's format, or nil if the flattening process fails.
--
-- ObjC selector: @- fileDataRepresentationWithReplacementMetadata:replacementEmbeddedThumbnailPhotoFormat:replacementEmbeddedThumbnailPixelBuffer:replacementDepthData:@
fileDataRepresentationWithReplacementMetadata_replacementEmbeddedThumbnailPhotoFormat_replacementEmbeddedThumbnailPixelBuffer_replacementDepthData :: (IsAVCapturePhoto avCapturePhoto, IsNSDictionary replacementMetadata, IsNSDictionary replacementEmbeddedThumbnailPhotoFormat, IsAVDepthData replacementDepthData) => avCapturePhoto -> replacementMetadata -> replacementEmbeddedThumbnailPhotoFormat -> Ptr () -> replacementDepthData -> IO (Id NSData)
fileDataRepresentationWithReplacementMetadata_replacementEmbeddedThumbnailPhotoFormat_replacementEmbeddedThumbnailPixelBuffer_replacementDepthData avCapturePhoto replacementMetadata replacementEmbeddedThumbnailPhotoFormat replacementEmbeddedThumbnailPixelBuffer replacementDepthData =
  sendMessage avCapturePhoto fileDataRepresentationWithReplacementMetadata_replacementEmbeddedThumbnailPhotoFormat_replacementEmbeddedThumbnailPixelBuffer_replacementDepthDataSelector (toNSDictionary replacementMetadata) (toNSDictionary replacementEmbeddedThumbnailPhotoFormat) replacementEmbeddedThumbnailPixelBuffer (toAVDepthData replacementDepthData)

-- | CGImageRepresentation
--
-- Utility method that converts the AVCapturePhoto's primary photo to a CGImage.
--
-- Returns: A CGImageRef, or nil if the conversion process fails.
--
-- Each time you access this method, AVCapturePhoto generates a new CGImageRef. When backed by a compressed container (such as HEIC), the CGImageRepresentation is decoded lazily as needed. When backed by an uncompressed format such as BGRA, it is copied into a separate backing buffer whose lifetime is not tied to that of the AVCapturePhoto. For a 12 megapixel image, a BGRA CGImage represents ~48 megabytes per call. If you only intend to use the CGImage for on-screen rendering, use the previewCGImageRepresentation instead. Note that the physical rotation of the CGImageRef matches that of the main image. Exif orientation has not been applied. If you wish to apply rotation when working with UIImage, you can do so by querying the photo's metadata[kCGImagePropertyOrientation] value, and passing it as the orientation parameter to +[UIImage imageWithCGImage:scale:orientation:]. RAW images always return a CGImageRepresentation of nil. If you wish to make a CGImageRef from a RAW image, use CIRAWFilter in the CoreImage framework.
--
-- ObjC selector: @- CGImageRepresentation@
cgImageRepresentation :: IsAVCapturePhoto avCapturePhoto => avCapturePhoto -> IO (Ptr ())
cgImageRepresentation avCapturePhoto =
  sendMessage avCapturePhoto cgImageRepresentationSelector

-- | CGImageRepresentation
--
-- Utility method that converts the AVCapturePhoto's preview photo to a CGImage.
--
-- Returns: A CGImageRef, or nil if the conversion process fails, or if you did not request a preview photo.
--
-- Each time you access this method, AVCapturePhoto generates a new CGImageRef. This CGImageRepresentation is a RGB rendering of the previewPixelBuffer property. If you did not request a preview photo by setting the -[AVCapturePhotoSettings previewPhotoFormat] property, this method returns nil. Note that the physical rotation of the CGImageRef matches that of the main image. Exif orientation has not been applied. If you wish to apply rotation when working with UIImage, you can do so by querying the photo's metadata[kCGImagePropertyOrientation] value, and passing it as the orientation parameter to +[UIImage imageWithCGImage:scale:orientation:].
--
-- ObjC selector: @- previewCGImageRepresentation@
previewCGImageRepresentation :: IsAVCapturePhoto avCapturePhoto => avCapturePhoto -> IO (Ptr ())
previewCGImageRepresentation avCapturePhoto =
  sendMessage avCapturePhoto previewCGImageRepresentationSelector

-- | rawPhoto
--
-- This property returns YES if this photo is a RAW image.
--
-- Your AVCapturePhotoCaptureDelegate's -captureOutput:didFinishingProcessingPhoto:error: method may be called one or more times with image results, including RAW or non-RAW images. This property distinguishes RAW from non-RAW image results, for instance, if you've requested a RAW + JPEG capture.
--
-- ObjC selector: @- rawPhoto@
rawPhoto :: IsAVCapturePhoto avCapturePhoto => avCapturePhoto -> IO Bool
rawPhoto avCapturePhoto =
  sendMessage avCapturePhoto rawPhotoSelector

-- | pixelBuffer
--
-- For uncompressed or RAW captures, this property offers access to the pixel data.
--
-- Uncompressed captures, such as '420f' or 'BGRA', Bayer RAW captures, such as 'bgg4', or Apple ProRAW captures, such as 'l64r', present pixel data as a CVPixelBuffer. See AVCapturePhotoOutput's -appleProRAWEnabled for a discussion on the differences between Bayer RAW and Apple ProRAW. This property is analogous to CMSampleBufferGetImageBuffer(). The pixel buffer contains only the minimal attachments required for correct display. Compressed captures, such as 'jpeg', return nil.
--
-- ObjC selector: @- pixelBuffer@
pixelBuffer :: IsAVCapturePhoto avCapturePhoto => avCapturePhoto -> IO (Ptr ())
pixelBuffer avCapturePhoto =
  sendMessage avCapturePhoto pixelBufferSelector

-- | previewPixelBuffer
--
-- This property offers access to the preview image pixel data if you've requested it.
--
-- If you requested a preview image by calling -[AVCapturePhotoSettings setPreviewPhotoFormat:] with a non-nil value, this property offers access to the resulting preview image pixel data, and is analogous to CMSampleBufferGetImageBuffer(). The pixel buffer contains only the minimal attachments required for correct display. Nil is returned if you did not request a preview image.
--
-- ObjC selector: @- previewPixelBuffer@
previewPixelBuffer :: IsAVCapturePhoto avCapturePhoto => avCapturePhoto -> IO (Ptr ())
previewPixelBuffer avCapturePhoto =
  sendMessage avCapturePhoto previewPixelBufferSelector

-- | embeddedThumbnailPhotoFormat
--
-- The format of the embedded thumbnail contained in this AVCapturePhoto.
--
-- If you requested an embedded thumbnail image by calling -[AVCapturePhotoSettings setEmbeddedThumbnailPhotoFormat:] with a non-nil value, this property offers access to the resolved embedded thumbnail AVVideoSettings dictionary. Nil is returned if you did not request an embedded thumbnail image.
--
-- ObjC selector: @- embeddedThumbnailPhotoFormat@
embeddedThumbnailPhotoFormat :: IsAVCapturePhoto avCapturePhoto => avCapturePhoto -> IO (Id NSDictionary)
embeddedThumbnailPhotoFormat avCapturePhoto =
  sendMessage avCapturePhoto embeddedThumbnailPhotoFormatSelector

-- | depthData
--
-- An AVDepthData object wrapping a disparity/depth map associated with this photo.
--
-- If you requested depth data delivery by calling -[AVCapturePhotoSettings setDepthDataDeliveryEnabled:YES], this property offers access to the resulting AVDepthData object. Nil is returned if you did not request depth data delivery. Note that the depth data is only embedded in the photo's internal file format container if you set -[AVCapturePhotoSettings setEmbedsDepthDataInPhoto:YES].
--
-- ObjC selector: @- depthData@
depthData :: IsAVCapturePhoto avCapturePhoto => avCapturePhoto -> IO (Id AVDepthData)
depthData avCapturePhoto =
  sendMessage avCapturePhoto depthDataSelector

-- | portraitEffectsMatte
--
-- An AVPortraitEffectsMatte object wrapping a matte associated with this photo.
--
-- If you requested portrait effects matte delivery by calling -[AVCapturePhotoSettings setPortraitEffectsMatteDeliveryEnabled:YES], this property offers access to the resulting AVPortraitEffectsMatte object. Nil is returned if you did not request portrait effects matte delivery. Note that the portrait effects matte is only embedded in the photo's internal file format container if you set -[AVCapturePhotoSettings setEmbedsPortraitEffectsMatteInPhoto:YES].
--
-- ObjC selector: @- portraitEffectsMatte@
portraitEffectsMatte :: IsAVCapturePhoto avCapturePhoto => avCapturePhoto -> IO (Id AVPortraitEffectsMatte)
portraitEffectsMatte avCapturePhoto =
  sendMessage avCapturePhoto portraitEffectsMatteSelector

-- | metadata
--
-- An ImageIO property style dictionary of metadata associated with this photo.
--
-- Valid metadata keys are found in <ImageIO/CGImageProperties.h>, such as kCGImagePropertyOrientation, kCGImagePropertyExifDictionary, kCGImagePropertyMakerAppleDictionary, etc.
--
-- ObjC selector: @- metadata@
metadata :: IsAVCapturePhoto avCapturePhoto => avCapturePhoto -> IO (Id NSDictionary)
metadata avCapturePhoto =
  sendMessage avCapturePhoto metadataSelector

-- | cameraCalibrationData
--
-- An AVCameraCalibrationData object representing the calibration information for the camera providing the photo.
--
-- Camera calibration data is only present if you set AVCapturePhotoSettings.setCameraCalibrationDataDeliveryEnabled to YES. When requesting virtual device constituent photo delivery plus cameraCalibrationDataDeliveryEnabled, camera calibration information is delivered with all resultant photos and is specific to the constituent device producing that photo.
--
-- ObjC selector: @- cameraCalibrationData@
cameraCalibrationData :: IsAVCapturePhoto avCapturePhoto => avCapturePhoto -> IO (Id AVCameraCalibrationData)
cameraCalibrationData avCapturePhoto =
  sendMessage avCapturePhoto cameraCalibrationDataSelector

-- | resolvedSettings
--
-- The AVCaptureResolvedPhotoSettings associated with all photo results for a given -[AVCapturePhotoOutput capturePhotoWithSettings:delegate:] request.
--
-- Even in the event of an error, the resolved settings are always non nil.
--
-- ObjC selector: @- resolvedSettings@
resolvedSettings :: IsAVCapturePhoto avCapturePhoto => avCapturePhoto -> IO (Id AVCaptureResolvedPhotoSettings)
resolvedSettings avCapturePhoto =
  sendMessage avCapturePhoto resolvedSettingsSelector

-- | photoCount
--
-- This photo's index (1-based) in the total expected photo count.
--
-- The resolvedSettings.expectedPhotoCount property indicates the total number of images that will be returned for a given capture request. This property indicates this photo's index (1-based). When you receive a -captureOutput:didFinishProcessingPhoto:error: callback with a photo whose photoCount matches resolvedSettings.expectedPhotoCount, you know you've received the last one for the given capture request.
--
-- ObjC selector: @- photoCount@
photoCount :: IsAVCapturePhoto avCapturePhoto => avCapturePhoto -> IO CLong
photoCount avCapturePhoto =
  sendMessage avCapturePhoto photoCountSelector

-- | sourceDeviceType
--
-- The device type of the source camera providing the photo.
--
-- When taking a virtual device constituent photo capture, you may query this property to find out the source type of the photo. For instance, on a DualCamera, resulting photos will be of sourceDeviceType AVCaptureDeviceTypeBuiltInWideCamera, or AVCaptureDeviceTypeBuiltInTelephotoCamera. For all other types of capture, the source device type is equal to the -[AVCaptureDevice deviceType] of the AVCaptureDevice to which the AVCapturePhotoOutput is connected. Returns nil if the source of the photo is not an AVCaptureDevice.
--
-- ObjC selector: @- sourceDeviceType@
sourceDeviceType :: IsAVCapturePhoto avCapturePhoto => avCapturePhoto -> IO (Id NSString)
sourceDeviceType avCapturePhoto =
  sendMessage avCapturePhoto sourceDeviceTypeSelector

-- | constantColorConfidenceMap
--
-- Returns a pixel buffer with the same aspect ratio as the constant color photo, where each pixel value (unsigned 8-bit integer) indicates how fully the constant color effect has been achieved in the corresponding region of the constant color photo -- 255 means full confidence, 0 means zero confidence.
--
-- NULL is returned for any non constant color photos.
--
-- ObjC selector: @- constantColorConfidenceMap@
constantColorConfidenceMap :: IsAVCapturePhoto avCapturePhoto => avCapturePhoto -> IO (Ptr ())
constantColorConfidenceMap avCapturePhoto =
  sendMessage avCapturePhoto constantColorConfidenceMapSelector

-- | constantColorCenterWeightedMeanConfidenceLevel
--
-- Returns a score summarizing the overall confidence level of a constant color photo -- 1.0 means full confidence, 0.0 means zero confidence.
--
-- Default is 0.0.
--
-- In most use cases (document scanning for example), the central region of the photo is considered more important than the peripherals, therefore the confidence level of the central pixels are weighted more heavily than pixels on the edges of the photo.
--
-- Use constantColorConfidenceMap for more use case specific analyses of the confidence level.
--
-- ObjC selector: @- constantColorCenterWeightedMeanConfidenceLevel@
constantColorCenterWeightedMeanConfidenceLevel :: IsAVCapturePhoto avCapturePhoto => avCapturePhoto -> IO CFloat
constantColorCenterWeightedMeanConfidenceLevel avCapturePhoto =
  sendMessage avCapturePhoto constantColorCenterWeightedMeanConfidenceLevelSelector

-- | constantColorFallbackPhoto
--
-- Indicates whether this photo is a fallback photo for a constant color capture.
--
-- ObjC selector: @- constantColorFallbackPhoto@
constantColorFallbackPhoto :: IsAVCapturePhoto avCapturePhoto => avCapturePhoto -> IO Bool
constantColorFallbackPhoto avCapturePhoto =
  sendMessage avCapturePhoto constantColorFallbackPhotoSelector

-- | bracketSettings
--
-- The AVCaptureBracketedStillImageSettings associated with this photo.
--
-- When specifying a bracketed capture using AVCapturePhotoBracketSettings, you specify an array of AVCaptureBracketedStillImageSettings -- one per image in the bracket. This property indicates the AVCaptureBracketedStillImageSettings associated with this particular photo, or nil if this photo is not part of a bracketed capture.
--
-- ObjC selector: @- bracketSettings@
bracketSettings :: IsAVCapturePhoto avCapturePhoto => avCapturePhoto -> IO (Id AVCaptureBracketedStillImageSettings)
bracketSettings avCapturePhoto =
  sendMessage avCapturePhoto bracketSettingsSelector

-- | sequenceCount
--
-- 1-based sequence count of the photo.
--
-- If this photo is part of a bracketed capture (invoked using AVCapturePhotoBracketSettings), this property indicates the current result's count in the sequence, starting with 1 for the first result, or 0 if this photo is not part of a bracketed capture.
--
-- ObjC selector: @- sequenceCount@
sequenceCount :: IsAVCapturePhoto avCapturePhoto => avCapturePhoto -> IO CLong
sequenceCount avCapturePhoto =
  sendMessage avCapturePhoto sequenceCountSelector

-- | lensStabilizationStatus
--
-- The status of the lens stabilization module during capture of this photo.
--
-- In configurations where lens stabilization (OIS) is unsupported, AVCaptureLensStabilizationStatusUnsupported is returned. If lens stabilization is supported, but this photo is not part of a bracketed capture in which -[AVCapturePhotoBracketSettings setLensStabilizationEnabled:YES] was called, AVCaptureLensStabilizationStatusOff is returned. Otherwise a lens stabilization status is returned indicating how lens stabilization was applied during the capture.
--
-- ObjC selector: @- lensStabilizationStatus@
lensStabilizationStatus :: IsAVCapturePhoto avCapturePhoto => avCapturePhoto -> IO AVCaptureLensStabilizationStatus
lensStabilizationStatus avCapturePhoto =
  sendMessage avCapturePhoto lensStabilizationStatusSelector

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @init@
initSelector :: Selector '[] (Id AVCapturePhoto)
initSelector = mkSelector "init"

-- | @Selector@ for @new@
newSelector :: Selector '[] (Id AVCapturePhoto)
newSelector = mkSelector "new"

-- | @Selector@ for @semanticSegmentationMatteForType:@
semanticSegmentationMatteForTypeSelector :: Selector '[Id NSString] (Id AVSemanticSegmentationMatte)
semanticSegmentationMatteForTypeSelector = mkSelector "semanticSegmentationMatteForType:"

-- | @Selector@ for @fileDataRepresentation@
fileDataRepresentationSelector :: Selector '[] (Id NSData)
fileDataRepresentationSelector = mkSelector "fileDataRepresentation"

-- | @Selector@ for @fileDataRepresentationWithCustomizer:@
fileDataRepresentationWithCustomizerSelector :: Selector '[RawId] (Id NSData)
fileDataRepresentationWithCustomizerSelector = mkSelector "fileDataRepresentationWithCustomizer:"

-- | @Selector@ for @fileDataRepresentationWithReplacementMetadata:replacementEmbeddedThumbnailPhotoFormat:replacementEmbeddedThumbnailPixelBuffer:replacementDepthData:@
fileDataRepresentationWithReplacementMetadata_replacementEmbeddedThumbnailPhotoFormat_replacementEmbeddedThumbnailPixelBuffer_replacementDepthDataSelector :: Selector '[Id NSDictionary, Id NSDictionary, Ptr (), Id AVDepthData] (Id NSData)
fileDataRepresentationWithReplacementMetadata_replacementEmbeddedThumbnailPhotoFormat_replacementEmbeddedThumbnailPixelBuffer_replacementDepthDataSelector = mkSelector "fileDataRepresentationWithReplacementMetadata:replacementEmbeddedThumbnailPhotoFormat:replacementEmbeddedThumbnailPixelBuffer:replacementDepthData:"

-- | @Selector@ for @CGImageRepresentation@
cgImageRepresentationSelector :: Selector '[] (Ptr ())
cgImageRepresentationSelector = mkSelector "CGImageRepresentation"

-- | @Selector@ for @previewCGImageRepresentation@
previewCGImageRepresentationSelector :: Selector '[] (Ptr ())
previewCGImageRepresentationSelector = mkSelector "previewCGImageRepresentation"

-- | @Selector@ for @rawPhoto@
rawPhotoSelector :: Selector '[] Bool
rawPhotoSelector = mkSelector "rawPhoto"

-- | @Selector@ for @pixelBuffer@
pixelBufferSelector :: Selector '[] (Ptr ())
pixelBufferSelector = mkSelector "pixelBuffer"

-- | @Selector@ for @previewPixelBuffer@
previewPixelBufferSelector :: Selector '[] (Ptr ())
previewPixelBufferSelector = mkSelector "previewPixelBuffer"

-- | @Selector@ for @embeddedThumbnailPhotoFormat@
embeddedThumbnailPhotoFormatSelector :: Selector '[] (Id NSDictionary)
embeddedThumbnailPhotoFormatSelector = mkSelector "embeddedThumbnailPhotoFormat"

-- | @Selector@ for @depthData@
depthDataSelector :: Selector '[] (Id AVDepthData)
depthDataSelector = mkSelector "depthData"

-- | @Selector@ for @portraitEffectsMatte@
portraitEffectsMatteSelector :: Selector '[] (Id AVPortraitEffectsMatte)
portraitEffectsMatteSelector = mkSelector "portraitEffectsMatte"

-- | @Selector@ for @metadata@
metadataSelector :: Selector '[] (Id NSDictionary)
metadataSelector = mkSelector "metadata"

-- | @Selector@ for @cameraCalibrationData@
cameraCalibrationDataSelector :: Selector '[] (Id AVCameraCalibrationData)
cameraCalibrationDataSelector = mkSelector "cameraCalibrationData"

-- | @Selector@ for @resolvedSettings@
resolvedSettingsSelector :: Selector '[] (Id AVCaptureResolvedPhotoSettings)
resolvedSettingsSelector = mkSelector "resolvedSettings"

-- | @Selector@ for @photoCount@
photoCountSelector :: Selector '[] CLong
photoCountSelector = mkSelector "photoCount"

-- | @Selector@ for @sourceDeviceType@
sourceDeviceTypeSelector :: Selector '[] (Id NSString)
sourceDeviceTypeSelector = mkSelector "sourceDeviceType"

-- | @Selector@ for @constantColorConfidenceMap@
constantColorConfidenceMapSelector :: Selector '[] (Ptr ())
constantColorConfidenceMapSelector = mkSelector "constantColorConfidenceMap"

-- | @Selector@ for @constantColorCenterWeightedMeanConfidenceLevel@
constantColorCenterWeightedMeanConfidenceLevelSelector :: Selector '[] CFloat
constantColorCenterWeightedMeanConfidenceLevelSelector = mkSelector "constantColorCenterWeightedMeanConfidenceLevel"

-- | @Selector@ for @constantColorFallbackPhoto@
constantColorFallbackPhotoSelector :: Selector '[] Bool
constantColorFallbackPhotoSelector = mkSelector "constantColorFallbackPhoto"

-- | @Selector@ for @bracketSettings@
bracketSettingsSelector :: Selector '[] (Id AVCaptureBracketedStillImageSettings)
bracketSettingsSelector = mkSelector "bracketSettings"

-- | @Selector@ for @sequenceCount@
sequenceCountSelector :: Selector '[] CLong
sequenceCountSelector = mkSelector "sequenceCount"

-- | @Selector@ for @lensStabilizationStatus@
lensStabilizationStatusSelector :: Selector '[] AVCaptureLensStabilizationStatus
lensStabilizationStatusSelector = mkSelector "lensStabilizationStatus"

