{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | AVCapturePhotoSettings
--
-- A mutable settings object encapsulating all the desired properties of a photo capture.
--
-- To take a picture, a client instantiates and configures an AVCapturePhotoSettings object, then calls AVCapturePhotoOutput's -capturePhotoWithSettings:delegate:, passing the settings and a delegate to be informed when events relating to the photo capture occur. Since AVCapturePhotoSettings has no reference to the AVCapturePhotoOutput instance with which it will be used, minimal validation occurs while you configure an AVCapturePhotoSettings instance. The bulk of the validation is executed when you call AVCapturePhotoOutput's -capturePhotoWithSettings:delegate:.
--
-- Generated bindings for @AVCapturePhotoSettings@.
module ObjC.AVFoundation.AVCapturePhotoSettings
  ( AVCapturePhotoSettings
  , IsAVCapturePhotoSettings(..)
  , photoSettings
  , photoSettingsWithFormat
  , photoSettingsWithRawPixelFormatType
  , photoSettingsWithRawPixelFormatType_processedFormat
  , photoSettingsWithRawPixelFormatType_rawFileType_processedFormat_processedFileType
  , photoSettingsFromPhotoSettings
  , uniqueID
  , format
  , rawFileFormat
  , setRawFileFormat
  , processedFileType
  , rawPhotoPixelFormatType
  , rawFileType
  , flashMode
  , setFlashMode
  , autoRedEyeReductionEnabled
  , setAutoRedEyeReductionEnabled
  , photoQualityPrioritization
  , setPhotoQualityPrioritization
  , autoStillImageStabilizationEnabled
  , setAutoStillImageStabilizationEnabled
  , autoVirtualDeviceFusionEnabled
  , setAutoVirtualDeviceFusionEnabled
  , autoDualCameraFusionEnabled
  , setAutoDualCameraFusionEnabled
  , virtualDeviceConstituentPhotoDeliveryEnabledDevices
  , setVirtualDeviceConstituentPhotoDeliveryEnabledDevices
  , dualCameraDualPhotoDeliveryEnabled
  , setDualCameraDualPhotoDeliveryEnabled
  , highResolutionPhotoEnabled
  , setHighResolutionPhotoEnabled
  , depthDataDeliveryEnabled
  , setDepthDataDeliveryEnabled
  , embedsDepthDataInPhoto
  , setEmbedsDepthDataInPhoto
  , depthDataFiltered
  , setDepthDataFiltered
  , cameraCalibrationDataDeliveryEnabled
  , setCameraCalibrationDataDeliveryEnabled
  , portraitEffectsMatteDeliveryEnabled
  , setPortraitEffectsMatteDeliveryEnabled
  , embedsPortraitEffectsMatteInPhoto
  , setEmbedsPortraitEffectsMatteInPhoto
  , enabledSemanticSegmentationMatteTypes
  , setEnabledSemanticSegmentationMatteTypes
  , embedsSemanticSegmentationMattesInPhoto
  , setEmbedsSemanticSegmentationMattesInPhoto
  , metadata
  , setMetadata
  , livePhotoMovieFileURL
  , setLivePhotoMovieFileURL
  , livePhotoVideoCodecType
  , setLivePhotoVideoCodecType
  , livePhotoMovieMetadata
  , setLivePhotoMovieMetadata
  , availablePreviewPhotoPixelFormatTypes
  , previewPhotoFormat
  , setPreviewPhotoFormat
  , availableEmbeddedThumbnailPhotoCodecTypes
  , embeddedThumbnailPhotoFormat
  , setEmbeddedThumbnailPhotoFormat
  , availableRawEmbeddedThumbnailPhotoCodecTypes
  , rawEmbeddedThumbnailPhotoFormat
  , setRawEmbeddedThumbnailPhotoFormat
  , autoContentAwareDistortionCorrectionEnabled
  , setAutoContentAwareDistortionCorrectionEnabled
  , constantColorEnabled
  , setConstantColorEnabled
  , constantColorFallbackPhotoDeliveryEnabled
  , setConstantColorFallbackPhotoDeliveryEnabled
  , shutterSoundSuppressionEnabled
  , setShutterSoundSuppressionEnabled
  , autoContentAwareDistortionCorrectionEnabledSelector
  , autoDualCameraFusionEnabledSelector
  , autoRedEyeReductionEnabledSelector
  , autoStillImageStabilizationEnabledSelector
  , autoVirtualDeviceFusionEnabledSelector
  , availableEmbeddedThumbnailPhotoCodecTypesSelector
  , availablePreviewPhotoPixelFormatTypesSelector
  , availableRawEmbeddedThumbnailPhotoCodecTypesSelector
  , cameraCalibrationDataDeliveryEnabledSelector
  , constantColorEnabledSelector
  , constantColorFallbackPhotoDeliveryEnabledSelector
  , depthDataDeliveryEnabledSelector
  , depthDataFilteredSelector
  , dualCameraDualPhotoDeliveryEnabledSelector
  , embeddedThumbnailPhotoFormatSelector
  , embedsDepthDataInPhotoSelector
  , embedsPortraitEffectsMatteInPhotoSelector
  , embedsSemanticSegmentationMattesInPhotoSelector
  , enabledSemanticSegmentationMatteTypesSelector
  , flashModeSelector
  , formatSelector
  , highResolutionPhotoEnabledSelector
  , livePhotoMovieFileURLSelector
  , livePhotoMovieMetadataSelector
  , livePhotoVideoCodecTypeSelector
  , metadataSelector
  , photoQualityPrioritizationSelector
  , photoSettingsFromPhotoSettingsSelector
  , photoSettingsSelector
  , photoSettingsWithFormatSelector
  , photoSettingsWithRawPixelFormatTypeSelector
  , photoSettingsWithRawPixelFormatType_processedFormatSelector
  , photoSettingsWithRawPixelFormatType_rawFileType_processedFormat_processedFileTypeSelector
  , portraitEffectsMatteDeliveryEnabledSelector
  , previewPhotoFormatSelector
  , processedFileTypeSelector
  , rawEmbeddedThumbnailPhotoFormatSelector
  , rawFileFormatSelector
  , rawFileTypeSelector
  , rawPhotoPixelFormatTypeSelector
  , setAutoContentAwareDistortionCorrectionEnabledSelector
  , setAutoDualCameraFusionEnabledSelector
  , setAutoRedEyeReductionEnabledSelector
  , setAutoStillImageStabilizationEnabledSelector
  , setAutoVirtualDeviceFusionEnabledSelector
  , setCameraCalibrationDataDeliveryEnabledSelector
  , setConstantColorEnabledSelector
  , setConstantColorFallbackPhotoDeliveryEnabledSelector
  , setDepthDataDeliveryEnabledSelector
  , setDepthDataFilteredSelector
  , setDualCameraDualPhotoDeliveryEnabledSelector
  , setEmbeddedThumbnailPhotoFormatSelector
  , setEmbedsDepthDataInPhotoSelector
  , setEmbedsPortraitEffectsMatteInPhotoSelector
  , setEmbedsSemanticSegmentationMattesInPhotoSelector
  , setEnabledSemanticSegmentationMatteTypesSelector
  , setFlashModeSelector
  , setHighResolutionPhotoEnabledSelector
  , setLivePhotoMovieFileURLSelector
  , setLivePhotoMovieMetadataSelector
  , setLivePhotoVideoCodecTypeSelector
  , setMetadataSelector
  , setPhotoQualityPrioritizationSelector
  , setPortraitEffectsMatteDeliveryEnabledSelector
  , setPreviewPhotoFormatSelector
  , setRawEmbeddedThumbnailPhotoFormatSelector
  , setRawFileFormatSelector
  , setShutterSoundSuppressionEnabledSelector
  , setVirtualDeviceConstituentPhotoDeliveryEnabledDevicesSelector
  , shutterSoundSuppressionEnabledSelector
  , uniqueIDSelector
  , virtualDeviceConstituentPhotoDeliveryEnabledDevicesSelector

  -- * Enum types
  , AVCaptureFlashMode(AVCaptureFlashMode)
  , pattern AVCaptureFlashModeOff
  , pattern AVCaptureFlashModeOn
  , pattern AVCaptureFlashModeAuto
  , AVCapturePhotoQualityPrioritization(AVCapturePhotoQualityPrioritization)
  , pattern AVCapturePhotoQualityPrioritizationSpeed
  , pattern AVCapturePhotoQualityPrioritizationBalanced
  , pattern AVCapturePhotoQualityPrioritizationQuality

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

-- | photoSettings
--
-- Creates a default instance of AVCapturePhotoSettings.
--
-- Returns: An instance of AVCapturePhotoSettings.
--
-- A default AVCapturePhotoSettings object has a format of AVVideoCodecTypeJPEG, a fileType of AVFileTypeJPEG, and photoQualityPrioritization set to AVCapturePhotoQualityPrioritizationBalanced.
--
-- ObjC selector: @+ photoSettings@
photoSettings :: IO (Id AVCapturePhotoSettings)
photoSettings  =
  do
    cls' <- getRequiredClass "AVCapturePhotoSettings"
    sendClassMessage cls' photoSettingsSelector

-- | photoSettingsWithFormat:
--
-- Creates an instance of AVCapturePhotoSettings with a user-specified output format.
--
-- @format@ — A dictionary of Core Video pixel buffer attributes or AVVideoSettings, analogous to AVCaptureStillImageOutput's outputSettings property.
--
-- Returns: An instance of AVCapturePhotoSettings.
--
-- If you wish an uncompressed format, your dictionary must contain kCVPixelBufferPixelFormatTypeKey, and the format specified must be present in AVCapturePhotoOutput's -availablePhotoPixelFormatTypes array. kCVPixelBufferPixelFormatTypeKey is the only supported key when expressing uncompressed output. If you wish a compressed format, your dictionary must contain AVVideoCodecKey and the codec specified must be present in AVCapturePhotoOutput's -availablePhotoCodecTypes array. If you are specifying a compressed format, the AVVideoCompressionPropertiesKey is also supported, with a payload dictionary containing a single AVVideoQualityKey. Passing a nil format dictionary is analogous to calling +photoSettings.
--
-- ObjC selector: @+ photoSettingsWithFormat:@
photoSettingsWithFormat :: IsNSDictionary format => format -> IO (Id AVCapturePhotoSettings)
photoSettingsWithFormat format =
  do
    cls' <- getRequiredClass "AVCapturePhotoSettings"
    sendClassMessage cls' photoSettingsWithFormatSelector (toNSDictionary format)

-- | photoSettingsWithRawPixelFormatType:
--
-- Creates an instance of AVCapturePhotoSettings specifying RAW only output.
--
-- @rawPixelFormatType@ — A Bayer RAW or Apple ProRAW pixel format OSType (defined in CVPixelBuffer.h).
--
-- Returns: An instance of AVCapturePhotoSettings.
--
-- rawPixelFormatType must be one of the OSTypes contained in AVCapturePhotoOutput's -availableRawPhotoPixelFormatTypes array. See AVCapturePhotoOutput's -capturePhotoWithSettings:delegate: inline documentation for a discussion of restrictions on AVCapturePhotoSettings when requesting RAW capture.
--
-- ObjC selector: @+ photoSettingsWithRawPixelFormatType:@
photoSettingsWithRawPixelFormatType :: CUInt -> IO (Id AVCapturePhotoSettings)
photoSettingsWithRawPixelFormatType rawPixelFormatType =
  do
    cls' <- getRequiredClass "AVCapturePhotoSettings"
    sendClassMessage cls' photoSettingsWithRawPixelFormatTypeSelector rawPixelFormatType

-- | photoSettingsWithRawPixelFormatType:processedFormat:
--
-- Creates an instance of AVCapturePhotoSettings specifying RAW + a processed format (such as JPEG).
--
-- @rawPixelFormatType@ — A Bayer RAW or Apple ProRAW pixel format OSType (defined in CVPixelBuffer.h).
--
-- @processedFormat@ — A dictionary of Core Video pixel buffer attributes or AVVideoSettings, analogous to AVCaptureStillImageOutput's outputSettings property.
--
-- Returns: An instance of AVCapturePhotoSettings.
--
-- rawPixelFormatType must be one of the OSTypes contained in AVCapturePhotoOutput's -availableRawPhotoPixelFormatTypes array. If you wish an uncompressed processedFormat, your dictionary must contain kCVPixelBufferPixelFormatTypeKey, and the processedFormat specified must be present in AVCapturePhotoOutput's -availablePhotoPixelFormatTypes array. kCVPixelBufferPixelFormatTypeKey is the only supported key when expressing uncompressed processedFormat. If you wish a compressed format, your dictionary must contain AVVideoCodecKey and the codec specified must be present in AVCapturePhotoOutput's -availablePhotoCodecTypes array. If you are specifying a compressed format, the AVVideoCompressionPropertiesKey is also supported, with a payload dictionary containing a single AVVideoQualityKey. Passing a nil processedFormat dictionary is analogous to calling +photoSettingsWithRawPixelFormatType:. See AVCapturePhotoOutput's -capturePhotoWithSettings:delegate: inline documentation for a discussion of restrictions on AVCapturePhotoSettings when requesting RAW capture.
--
-- ObjC selector: @+ photoSettingsWithRawPixelFormatType:processedFormat:@
photoSettingsWithRawPixelFormatType_processedFormat :: IsNSDictionary processedFormat => CUInt -> processedFormat -> IO (Id AVCapturePhotoSettings)
photoSettingsWithRawPixelFormatType_processedFormat rawPixelFormatType processedFormat =
  do
    cls' <- getRequiredClass "AVCapturePhotoSettings"
    sendClassMessage cls' photoSettingsWithRawPixelFormatType_processedFormatSelector rawPixelFormatType (toNSDictionary processedFormat)

-- | photoSettingsWithRawPixelFormatType:rawFileType:processedFormat:processedFileType:
--
-- Creates an instance of AVCapturePhotoSettings specifying RAW + a processed format (such as JPEG) and a file container to which it will be written.
--
-- @rawPixelFormatType@ — A Bayer RAW or Apple ProRAW pixel format OSType (defined in CVPixelBuffer.h). Pass 0 if you do not desire a RAW photo callback.
--
-- @rawFileType@ — The file container for which the RAW image should be formatted to be written. Pass nil if you have no preferred file container. A default container will be chosen for you.
--
-- @processedFormat@ — A dictionary of Core Video pixel buffer attributes or AVVideoSettings, analogous to AVCaptureStillImageOutput's outputSettings property. Pass nil if you do not desire a processed photo callback.
--
-- @processedFileType@ — The file container for which the processed image should be formatted to be written. Pass nil if you have no preferred file container. A default container will be chosen for you.
--
-- Returns: An instance of AVCapturePhotoSettings.
--
-- rawPixelFormatType must be one of the OSTypes contained in AVCapturePhotoOutput's -availableRawPhotoPixelFormatTypes array. Set rawPixelFormatType to 0 if you do not desire a RAW photo callback. If you are specifying a rawFileType, it must be present in AVCapturePhotoOutput's -availableRawPhotoFileTypes array. If you wish an uncompressed processedFormat, your dictionary must contain kCVPixelBufferPixelFormatTypeKey, and the processedFormat specified must be present in AVCapturePhotoOutput's -availablePhotoPixelFormatTypes array. kCVPixelBufferPixelFormatTypeKey is the only supported key when expressing uncompressed processedFormat. If you wish a compressed format, your dictionary must contain AVVideoCodecKey and the codec specified must be present in AVCapturePhotoOutput's -availablePhotoCodecTypes array. If you are specifying a compressed format, the AVVideoCompressionPropertiesKey is also supported, with a payload dictionary containing a single AVVideoQualityKey. If you are specifying a processedFileType (such as AVFileTypeJPEG, AVFileTypeHEIC or AVFileTypeDICOM), it must be present in AVCapturePhotoOutput's -availablePhotoFileTypes array. Pass a nil processedFormat dictionary if you only desire a RAW photo capture. See AVCapturePhotoOutput's -capturePhotoWithSettings:delegate: inline documentation for a discussion of restrictions on AVCapturePhotoSettings when requesting RAW capture.
--
-- ObjC selector: @+ photoSettingsWithRawPixelFormatType:rawFileType:processedFormat:processedFileType:@
photoSettingsWithRawPixelFormatType_rawFileType_processedFormat_processedFileType :: (IsNSString rawFileType, IsNSDictionary processedFormat, IsNSString processedFileType) => CUInt -> rawFileType -> processedFormat -> processedFileType -> IO (Id AVCapturePhotoSettings)
photoSettingsWithRawPixelFormatType_rawFileType_processedFormat_processedFileType rawPixelFormatType rawFileType processedFormat processedFileType =
  do
    cls' <- getRequiredClass "AVCapturePhotoSettings"
    sendClassMessage cls' photoSettingsWithRawPixelFormatType_rawFileType_processedFormat_processedFileTypeSelector rawPixelFormatType (toNSString rawFileType) (toNSDictionary processedFormat) (toNSString processedFileType)

-- | photoSettingsFromPhotoSettings:
--
-- Creates an instance of AVCapturePhotoSettings with a new uniqueID from an existing instance of AVCapturePhotoSettings.
--
-- @photoSettings@ — An existing AVCapturePhotoSettings instance.
--
-- Returns: An new instance of AVCapturePhotoSettings with new uniqueID.
--
-- Use this factory method to create a clone of an existing photo settings instance, but with a new uniqueID that can safely be passed to AVCapturePhotoOutput -capturePhotoWithSettings:delegate:.
--
-- ObjC selector: @+ photoSettingsFromPhotoSettings:@
photoSettingsFromPhotoSettings :: IsAVCapturePhotoSettings photoSettings => photoSettings -> IO (Id AVCapturePhotoSettings)
photoSettingsFromPhotoSettings photoSettings =
  do
    cls' <- getRequiredClass "AVCapturePhotoSettings"
    sendClassMessage cls' photoSettingsFromPhotoSettingsSelector (toAVCapturePhotoSettings photoSettings)

-- | uniqueID
--
-- A 64-bit number that uniquely identifies this instance.
--
-- When you create an instance of AVCapturePhotoSettings, a uniqueID is generated automatically. This uniqueID is guaranteed to be unique for the life time of your process.
--
-- ObjC selector: @- uniqueID@
uniqueID :: IsAVCapturePhotoSettings avCapturePhotoSettings => avCapturePhotoSettings -> IO CLong
uniqueID avCapturePhotoSettings =
  sendMessage avCapturePhotoSettings uniqueIDSelector

-- | format
--
-- A dictionary of Core Video pixel buffer attributes or AVVideoSettings, analogous to AVCaptureStillImageOutput's outputSettings property.
--
-- The format dictionary you passed to one of the creation methods. May be nil if you've specified RAW-only capture.
--
-- ObjC selector: @- format@
format :: IsAVCapturePhotoSettings avCapturePhotoSettings => avCapturePhotoSettings -> IO (Id NSDictionary)
format avCapturePhotoSettings =
  sendMessage avCapturePhotoSettings formatSelector

-- | rawFileFormat
--
-- A dictionary of AVVideoSettings keys specifying the RAW file format to be used for the RAW photo.
--
-- One can specify desired format properties of the RAW file that will be created. Currently only the key AVVideoAppleProRAWBitDepthKey is allowed and the value to which it can be set should be from 8-16.  The AVVideoCodecKey must be present in the receiver's -availableRawPhotoCodecTypes array as well as in -supportedRawPhotoCodecTypesForRawPhotoPixelFormatType:fileType:. AVVideoQualityKey (NSNumber in range [0.0,1.0]) can be optionally set and a value between [0.0,1.0] will use lossy compression with lower values being more lossy resulting in smaller file sizes but lower image quality, while a value of 1.0 will use lossless compression resulting in the largest file size but also the best quality.
--
-- ObjC selector: @- rawFileFormat@
rawFileFormat :: IsAVCapturePhotoSettings avCapturePhotoSettings => avCapturePhotoSettings -> IO (Id NSDictionary)
rawFileFormat avCapturePhotoSettings =
  sendMessage avCapturePhotoSettings rawFileFormatSelector

-- | rawFileFormat
--
-- A dictionary of AVVideoSettings keys specifying the RAW file format to be used for the RAW photo.
--
-- One can specify desired format properties of the RAW file that will be created. Currently only the key AVVideoAppleProRAWBitDepthKey is allowed and the value to which it can be set should be from 8-16.  The AVVideoCodecKey must be present in the receiver's -availableRawPhotoCodecTypes array as well as in -supportedRawPhotoCodecTypesForRawPhotoPixelFormatType:fileType:. AVVideoQualityKey (NSNumber in range [0.0,1.0]) can be optionally set and a value between [0.0,1.0] will use lossy compression with lower values being more lossy resulting in smaller file sizes but lower image quality, while a value of 1.0 will use lossless compression resulting in the largest file size but also the best quality.
--
-- ObjC selector: @- setRawFileFormat:@
setRawFileFormat :: (IsAVCapturePhotoSettings avCapturePhotoSettings, IsNSDictionary value) => avCapturePhotoSettings -> value -> IO ()
setRawFileFormat avCapturePhotoSettings value =
  sendMessage avCapturePhotoSettings setRawFileFormatSelector (toNSDictionary value)

-- | processedFileType
--
-- The file container for which the processed photo is formatted to be stored.
--
-- The formatting of data within a photo buffer is often dependent on the file format intended for storage. For instance, a JPEG encoded photo buffer intended for storage in a JPEG (JPEG File Interchange Format) file differs from JPEG to be stored in HEIF. The HEIF-containerized JPEG buffer is tiled for readback efficiency and partitioned into the box structure dictated by the HEIF file format. Some codecs are only supported by AVCapturePhotoOutput if containerized. For instance, the AVVideoCodecTypeHEVC is only supported with AVFileTypeHEIF and AVFileTypeHEIC formatting. To discover which photo pixel format types and video codecs are supported for a given file type, you may query AVCapturePhotoOutput's -supportedPhotoPixelFormatTypesForFileType:, or -supportedPhotoCodecTypesForFileType: respectively.
--
-- ObjC selector: @- processedFileType@
processedFileType :: IsAVCapturePhotoSettings avCapturePhotoSettings => avCapturePhotoSettings -> IO (Id NSString)
processedFileType avCapturePhotoSettings =
  sendMessage avCapturePhotoSettings processedFileTypeSelector

-- | rawPhotoPixelFormatType
--
-- A Bayer RAW or Apple ProRAW pixel format OSType (defined in CVPixelBuffer.h).
--
-- The rawPixelFormatType you specified in one of the creation methods. Returns 0 if you did not specify RAW capture. See AVCapturePhotoOutput's -capturePhotoWithSettings:delegate: inline documentation for a discussion of restrictions on AVCapturePhotoSettings when requesting RAW capture.
--
-- ObjC selector: @- rawPhotoPixelFormatType@
rawPhotoPixelFormatType :: IsAVCapturePhotoSettings avCapturePhotoSettings => avCapturePhotoSettings -> IO CUInt
rawPhotoPixelFormatType avCapturePhotoSettings =
  sendMessage avCapturePhotoSettings rawPhotoPixelFormatTypeSelector

-- | rawFileType
--
-- The file container for which the RAW photo is formatted to be stored.
--
-- The formatting of data within a RAW photo buffer may be dependent on the file format intended for storage. To discover which RAW photo pixel format types are supported for a given file type, you may query AVCapturePhotoOutput's -supportedRawPhotoPixelFormatTypesForFileType:.
--
-- ObjC selector: @- rawFileType@
rawFileType :: IsAVCapturePhotoSettings avCapturePhotoSettings => avCapturePhotoSettings -> IO (Id NSString)
rawFileType avCapturePhotoSettings =
  sendMessage avCapturePhotoSettings rawFileTypeSelector

-- | flashMode
--
-- Specifies whether the flash should be on, off, or chosen automatically by AVCapturePhotoOutput.
--
-- flashMode takes the place of the deprecated AVCaptureDevice -flashMode API. Setting AVCaptureDevice.flashMode has no effect on AVCapturePhotoOutput, which only pays attention to the flashMode specified in your AVCapturePhotoSettings. The default value is AVCaptureFlashModeOff. Flash modes are defined in AVCaptureDevice.h. If you specify a flashMode of AVCaptureFlashModeOn, it wins over autoStillImageStabilizationEnabled=YES. When the device becomes very hot, the flash becomes temporarily unavailable until the device cools down (see AVCaptureDevice's -flashAvailable). While the flash is unavailable, AVCapturePhotoOutput's -supportedFlashModes property still reports AVCaptureFlashModeOn and AVCaptureFlashModeAuto as being available, thus allowing you to specify a flashMode of AVCaptureModeOn. You should always check the AVCaptureResolvedPhotoSettings provided to you in the AVCapturePhotoCaptureDelegate callbacks, as the resolved flashEnabled property will tell you definitively if the flash is being used.
--
-- ObjC selector: @- flashMode@
flashMode :: IsAVCapturePhotoSettings avCapturePhotoSettings => avCapturePhotoSettings -> IO AVCaptureFlashMode
flashMode avCapturePhotoSettings =
  sendMessage avCapturePhotoSettings flashModeSelector

-- | flashMode
--
-- Specifies whether the flash should be on, off, or chosen automatically by AVCapturePhotoOutput.
--
-- flashMode takes the place of the deprecated AVCaptureDevice -flashMode API. Setting AVCaptureDevice.flashMode has no effect on AVCapturePhotoOutput, which only pays attention to the flashMode specified in your AVCapturePhotoSettings. The default value is AVCaptureFlashModeOff. Flash modes are defined in AVCaptureDevice.h. If you specify a flashMode of AVCaptureFlashModeOn, it wins over autoStillImageStabilizationEnabled=YES. When the device becomes very hot, the flash becomes temporarily unavailable until the device cools down (see AVCaptureDevice's -flashAvailable). While the flash is unavailable, AVCapturePhotoOutput's -supportedFlashModes property still reports AVCaptureFlashModeOn and AVCaptureFlashModeAuto as being available, thus allowing you to specify a flashMode of AVCaptureModeOn. You should always check the AVCaptureResolvedPhotoSettings provided to you in the AVCapturePhotoCaptureDelegate callbacks, as the resolved flashEnabled property will tell you definitively if the flash is being used.
--
-- ObjC selector: @- setFlashMode:@
setFlashMode :: IsAVCapturePhotoSettings avCapturePhotoSettings => avCapturePhotoSettings -> AVCaptureFlashMode -> IO ()
setFlashMode avCapturePhotoSettings value =
  sendMessage avCapturePhotoSettings setFlashModeSelector value

-- | autoRedEyeReductionEnabled
--
-- Specifies whether red-eye reduction should be applied automatically on flash captures.
--
-- Default is YES on platforms that support automatic red-eye reduction unless you are capturing a bracket using AVCapturePhotoBracketSettings or a RAW photo without a processed photo.  For RAW photos with a processed photo the red-eye reduction will be applied to the processed photo only (RAW photos by definition are not processed). When set to YES, red-eye reduction is applied as needed for flash captures if the photo output's autoRedEyeReductionSupported property returns YES.
--
-- ObjC selector: @- autoRedEyeReductionEnabled@
autoRedEyeReductionEnabled :: IsAVCapturePhotoSettings avCapturePhotoSettings => avCapturePhotoSettings -> IO Bool
autoRedEyeReductionEnabled avCapturePhotoSettings =
  sendMessage avCapturePhotoSettings autoRedEyeReductionEnabledSelector

-- | autoRedEyeReductionEnabled
--
-- Specifies whether red-eye reduction should be applied automatically on flash captures.
--
-- Default is YES on platforms that support automatic red-eye reduction unless you are capturing a bracket using AVCapturePhotoBracketSettings or a RAW photo without a processed photo.  For RAW photos with a processed photo the red-eye reduction will be applied to the processed photo only (RAW photos by definition are not processed). When set to YES, red-eye reduction is applied as needed for flash captures if the photo output's autoRedEyeReductionSupported property returns YES.
--
-- ObjC selector: @- setAutoRedEyeReductionEnabled:@
setAutoRedEyeReductionEnabled :: IsAVCapturePhotoSettings avCapturePhotoSettings => avCapturePhotoSettings -> Bool -> IO ()
setAutoRedEyeReductionEnabled avCapturePhotoSettings value =
  sendMessage avCapturePhotoSettings setAutoRedEyeReductionEnabledSelector value

-- | photoQualityPrioritization
--
-- Indicates how photo quality should be prioritized against speed of photo delivery.
--
-- Default value is AVCapturePhotoQualityPrioritizationBalanced. The AVCapturePhotoOutput is capable of applying a variety of techniques to improve photo quality (reduce noise, preserve detail in low light, freeze motion, etc), depending on the source device's activeFormat. Some of these techniques can take significant processing time before the photo is returned to your delegate callback. The photoQualityPrioritization property allows you to specify your preferred quality vs speed of delivery. By default, speed and quality are considered to be of equal importance. When you specify AVCapturePhotoQualityPrioritizationSpeed, you indicate that speed should be prioritized at the expense of quality. Likewise, when you choose AVCapturePhotoQualityPrioritizationQuality, you signal your willingness to prioritize the very best quality at the expense of speed, and your readiness to wait (perhaps significantly) longer for the photo to be returned to your delegate.
--
-- ObjC selector: @- photoQualityPrioritization@
photoQualityPrioritization :: IsAVCapturePhotoSettings avCapturePhotoSettings => avCapturePhotoSettings -> IO AVCapturePhotoQualityPrioritization
photoQualityPrioritization avCapturePhotoSettings =
  sendMessage avCapturePhotoSettings photoQualityPrioritizationSelector

-- | photoQualityPrioritization
--
-- Indicates how photo quality should be prioritized against speed of photo delivery.
--
-- Default value is AVCapturePhotoQualityPrioritizationBalanced. The AVCapturePhotoOutput is capable of applying a variety of techniques to improve photo quality (reduce noise, preserve detail in low light, freeze motion, etc), depending on the source device's activeFormat. Some of these techniques can take significant processing time before the photo is returned to your delegate callback. The photoQualityPrioritization property allows you to specify your preferred quality vs speed of delivery. By default, speed and quality are considered to be of equal importance. When you specify AVCapturePhotoQualityPrioritizationSpeed, you indicate that speed should be prioritized at the expense of quality. Likewise, when you choose AVCapturePhotoQualityPrioritizationQuality, you signal your willingness to prioritize the very best quality at the expense of speed, and your readiness to wait (perhaps significantly) longer for the photo to be returned to your delegate.
--
-- ObjC selector: @- setPhotoQualityPrioritization:@
setPhotoQualityPrioritization :: IsAVCapturePhotoSettings avCapturePhotoSettings => avCapturePhotoSettings -> AVCapturePhotoQualityPrioritization -> IO ()
setPhotoQualityPrioritization avCapturePhotoSettings value =
  sendMessage avCapturePhotoSettings setPhotoQualityPrioritizationSelector value

-- | autoStillImageStabilizationEnabled
--
-- Specifies whether still image stabilization should be used automatically.
--
-- Default is YES unless you are capturing a Bayer RAW photo (Bayer RAW photos may not be processed by definition) or a bracket using AVCapturePhotoBracketSettings. When set to YES, still image stabilization is applied automatically in low light to counteract hand shake. If the device has optical image stabilization, autoStillImageStabilizationEnabled makes use of lens stabilization as well.
--
-- As of iOS 13 hardware, the AVCapturePhotoOutput is capable of applying a variety of multi-image fusion techniques to improve photo quality (reduce noise, preserve detail in low light, freeze motion, etc), all of which have been previously lumped under the stillImageStabilization moniker. This property should no longer be used as it no longer provides meaningful information about the techniques used to improve quality in a photo capture. Instead, you should use -photoQualityPrioritization to indicate your preferred quality vs speed.
--
-- ObjC selector: @- autoStillImageStabilizationEnabled@
autoStillImageStabilizationEnabled :: IsAVCapturePhotoSettings avCapturePhotoSettings => avCapturePhotoSettings -> IO Bool
autoStillImageStabilizationEnabled avCapturePhotoSettings =
  sendMessage avCapturePhotoSettings autoStillImageStabilizationEnabledSelector

-- | autoStillImageStabilizationEnabled
--
-- Specifies whether still image stabilization should be used automatically.
--
-- Default is YES unless you are capturing a Bayer RAW photo (Bayer RAW photos may not be processed by definition) or a bracket using AVCapturePhotoBracketSettings. When set to YES, still image stabilization is applied automatically in low light to counteract hand shake. If the device has optical image stabilization, autoStillImageStabilizationEnabled makes use of lens stabilization as well.
--
-- As of iOS 13 hardware, the AVCapturePhotoOutput is capable of applying a variety of multi-image fusion techniques to improve photo quality (reduce noise, preserve detail in low light, freeze motion, etc), all of which have been previously lumped under the stillImageStabilization moniker. This property should no longer be used as it no longer provides meaningful information about the techniques used to improve quality in a photo capture. Instead, you should use -photoQualityPrioritization to indicate your preferred quality vs speed.
--
-- ObjC selector: @- setAutoStillImageStabilizationEnabled:@
setAutoStillImageStabilizationEnabled :: IsAVCapturePhotoSettings avCapturePhotoSettings => avCapturePhotoSettings -> Bool -> IO ()
setAutoStillImageStabilizationEnabled avCapturePhotoSettings value =
  sendMessage avCapturePhotoSettings setAutoStillImageStabilizationEnabledSelector value

-- | autoVirtualDeviceFusionEnabled
--
-- Specifies whether virtual device image fusion should be used automatically.
--
-- Default is YES unless you are capturing a RAW photo (RAW photos may not be processed by definition) or a bracket using AVCapturePhotoBracketSettings. When set to YES, and -[AVCapturePhotoOutput isVirtualDeviceFusionSupported] is also YES, constituent camera images of a virtual device may be fused to improve still image quality, depending on the current zoom factor, light levels, and focus position. You may determine whether virtual device fusion is enabled for a particular capture request by inspecting the virtualDeviceFusionEnabled property of the AVCaptureResolvedPhotoSettings. Note that when using the deprecated AVCaptureStillImageOutput interface with a virtual device, autoVirtualDeviceFusionEnabled fusion is always enabled if supported, and may not be turned off.
--
-- ObjC selector: @- autoVirtualDeviceFusionEnabled@
autoVirtualDeviceFusionEnabled :: IsAVCapturePhotoSettings avCapturePhotoSettings => avCapturePhotoSettings -> IO Bool
autoVirtualDeviceFusionEnabled avCapturePhotoSettings =
  sendMessage avCapturePhotoSettings autoVirtualDeviceFusionEnabledSelector

-- | autoVirtualDeviceFusionEnabled
--
-- Specifies whether virtual device image fusion should be used automatically.
--
-- Default is YES unless you are capturing a RAW photo (RAW photos may not be processed by definition) or a bracket using AVCapturePhotoBracketSettings. When set to YES, and -[AVCapturePhotoOutput isVirtualDeviceFusionSupported] is also YES, constituent camera images of a virtual device may be fused to improve still image quality, depending on the current zoom factor, light levels, and focus position. You may determine whether virtual device fusion is enabled for a particular capture request by inspecting the virtualDeviceFusionEnabled property of the AVCaptureResolvedPhotoSettings. Note that when using the deprecated AVCaptureStillImageOutput interface with a virtual device, autoVirtualDeviceFusionEnabled fusion is always enabled if supported, and may not be turned off.
--
-- ObjC selector: @- setAutoVirtualDeviceFusionEnabled:@
setAutoVirtualDeviceFusionEnabled :: IsAVCapturePhotoSettings avCapturePhotoSettings => avCapturePhotoSettings -> Bool -> IO ()
setAutoVirtualDeviceFusionEnabled avCapturePhotoSettings value =
  sendMessage avCapturePhotoSettings setAutoVirtualDeviceFusionEnabledSelector value

-- | autoDualCameraFusionEnabled
--
-- Specifies whether DualCamera image fusion should be used automatically.
--
-- Default is YES unless you are capturing a RAW photo (RAW photos may not be processed by definition) or a bracket using AVCapturePhotoBracketSettings. When set to YES, and -[AVCapturePhotoOutput isDualCameraFusionSupported] is also YES, wide-angle and telephoto images may be fused to improve still image quality, depending on the current zoom factor, light levels, and focus position. You may determine whether DualCamera fusion is enabled for a particular capture request by inspecting the dualCameraFusionEnabled property of the AVCaptureResolvedPhotoSettings. Note that when using the deprecated AVCaptureStillImageOutput interface with the DualCamera, auto DualCamera fusion is always enabled and may not be turned off. As of iOS 13, this property is deprecated in favor of autoVirtualDeviceFusionEnabled.
--
-- ObjC selector: @- autoDualCameraFusionEnabled@
autoDualCameraFusionEnabled :: IsAVCapturePhotoSettings avCapturePhotoSettings => avCapturePhotoSettings -> IO Bool
autoDualCameraFusionEnabled avCapturePhotoSettings =
  sendMessage avCapturePhotoSettings autoDualCameraFusionEnabledSelector

-- | autoDualCameraFusionEnabled
--
-- Specifies whether DualCamera image fusion should be used automatically.
--
-- Default is YES unless you are capturing a RAW photo (RAW photos may not be processed by definition) or a bracket using AVCapturePhotoBracketSettings. When set to YES, and -[AVCapturePhotoOutput isDualCameraFusionSupported] is also YES, wide-angle and telephoto images may be fused to improve still image quality, depending on the current zoom factor, light levels, and focus position. You may determine whether DualCamera fusion is enabled for a particular capture request by inspecting the dualCameraFusionEnabled property of the AVCaptureResolvedPhotoSettings. Note that when using the deprecated AVCaptureStillImageOutput interface with the DualCamera, auto DualCamera fusion is always enabled and may not be turned off. As of iOS 13, this property is deprecated in favor of autoVirtualDeviceFusionEnabled.
--
-- ObjC selector: @- setAutoDualCameraFusionEnabled:@
setAutoDualCameraFusionEnabled :: IsAVCapturePhotoSettings avCapturePhotoSettings => avCapturePhotoSettings -> Bool -> IO ()
setAutoDualCameraFusionEnabled avCapturePhotoSettings value =
  sendMessage avCapturePhotoSettings setAutoDualCameraFusionEnabledSelector value

-- | virtualDeviceConstituentPhotoDeliveryEnabledDevices
--
-- Specifies the constituent devices for which the virtual device should deliver photos.
--
-- Default is empty array. To opt in for constituent device photo delivery, you may set this property to any subset of 2 or more of the devices in virtualDevice.constituentDevices. Your captureOutput:didFinishProcessingPhoto:error: callback will be called n times -- one for each of the devices you include in the array. You may only set this property to a non-nil array if you've set your AVCapturePhotoOutput's virtualDeviceConstituentPhotoDeliveryEnabled property to YES, and your delegate responds to the captureOutput:didFinishProcessingPhoto:error: selector.
--
-- ObjC selector: @- virtualDeviceConstituentPhotoDeliveryEnabledDevices@
virtualDeviceConstituentPhotoDeliveryEnabledDevices :: IsAVCapturePhotoSettings avCapturePhotoSettings => avCapturePhotoSettings -> IO (Id NSArray)
virtualDeviceConstituentPhotoDeliveryEnabledDevices avCapturePhotoSettings =
  sendMessage avCapturePhotoSettings virtualDeviceConstituentPhotoDeliveryEnabledDevicesSelector

-- | virtualDeviceConstituentPhotoDeliveryEnabledDevices
--
-- Specifies the constituent devices for which the virtual device should deliver photos.
--
-- Default is empty array. To opt in for constituent device photo delivery, you may set this property to any subset of 2 or more of the devices in virtualDevice.constituentDevices. Your captureOutput:didFinishProcessingPhoto:error: callback will be called n times -- one for each of the devices you include in the array. You may only set this property to a non-nil array if you've set your AVCapturePhotoOutput's virtualDeviceConstituentPhotoDeliveryEnabled property to YES, and your delegate responds to the captureOutput:didFinishProcessingPhoto:error: selector.
--
-- ObjC selector: @- setVirtualDeviceConstituentPhotoDeliveryEnabledDevices:@
setVirtualDeviceConstituentPhotoDeliveryEnabledDevices :: (IsAVCapturePhotoSettings avCapturePhotoSettings, IsNSArray value) => avCapturePhotoSettings -> value -> IO ()
setVirtualDeviceConstituentPhotoDeliveryEnabledDevices avCapturePhotoSettings value =
  sendMessage avCapturePhotoSettings setVirtualDeviceConstituentPhotoDeliveryEnabledDevicesSelector (toNSArray value)

-- | dualCameraDualPhotoDeliveryEnabled
--
-- Specifies whether the DualCamera should return both the telephoto and wide image.
--
-- Default is NO. When set to YES, your captureOutput:didFinishProcessingPhoto:error: callback will receive twice the number of callbacks, as both the telephoto image(s) and wide-angle image(s) are delivered. You may only set this property to YES if you've set your AVCapturePhotoOutput's dualCameraDualPhotoDeliveryEnabled property to YES, and your delegate responds to the captureOutput:didFinishProcessingPhoto:error: selector. As of iOS 13, this property is deprecated in favor of virtualDeviceConstituentPhotoDeliveryEnabledDevices.
--
-- ObjC selector: @- dualCameraDualPhotoDeliveryEnabled@
dualCameraDualPhotoDeliveryEnabled :: IsAVCapturePhotoSettings avCapturePhotoSettings => avCapturePhotoSettings -> IO Bool
dualCameraDualPhotoDeliveryEnabled avCapturePhotoSettings =
  sendMessage avCapturePhotoSettings dualCameraDualPhotoDeliveryEnabledSelector

-- | dualCameraDualPhotoDeliveryEnabled
--
-- Specifies whether the DualCamera should return both the telephoto and wide image.
--
-- Default is NO. When set to YES, your captureOutput:didFinishProcessingPhoto:error: callback will receive twice the number of callbacks, as both the telephoto image(s) and wide-angle image(s) are delivered. You may only set this property to YES if you've set your AVCapturePhotoOutput's dualCameraDualPhotoDeliveryEnabled property to YES, and your delegate responds to the captureOutput:didFinishProcessingPhoto:error: selector. As of iOS 13, this property is deprecated in favor of virtualDeviceConstituentPhotoDeliveryEnabledDevices.
--
-- ObjC selector: @- setDualCameraDualPhotoDeliveryEnabled:@
setDualCameraDualPhotoDeliveryEnabled :: IsAVCapturePhotoSettings avCapturePhotoSettings => avCapturePhotoSettings -> Bool -> IO ()
setDualCameraDualPhotoDeliveryEnabled avCapturePhotoSettings value =
  sendMessage avCapturePhotoSettings setDualCameraDualPhotoDeliveryEnabledSelector value

-- | highResolutionPhotoEnabled
--
-- Specifies whether photos should be captured at the highest resolution supported by the source AVCaptureDevice's activeFormat.
--
-- Default is NO. By default, AVCapturePhotoOutput emits images with the same dimensions as its source AVCaptureDevice's activeFormat.formatDescription. However, if you set this property to YES, the AVCapturePhotoOutput emits images at its source AVCaptureDevice's activeFormat.highResolutionStillImageDimensions. Note that if you enable video stabilization (see AVCaptureConnection's preferredVideoStabilizationMode) for any output, the high resolution photos emitted by AVCapturePhotoOutput may be smaller by 10 or more percent. You may inspect your AVCaptureResolvedPhotoSettings in the delegate callbacks to discover the exact dimensions of the capture photo(s).
--
-- Starting in iOS 14.5 if you disable geometric distortion correction, the high resolution photo emitted by AVCapturePhotoOutput may be is smaller depending on the format.
--
-- ObjC selector: @- highResolutionPhotoEnabled@
highResolutionPhotoEnabled :: IsAVCapturePhotoSettings avCapturePhotoSettings => avCapturePhotoSettings -> IO Bool
highResolutionPhotoEnabled avCapturePhotoSettings =
  sendMessage avCapturePhotoSettings highResolutionPhotoEnabledSelector

-- | highResolutionPhotoEnabled
--
-- Specifies whether photos should be captured at the highest resolution supported by the source AVCaptureDevice's activeFormat.
--
-- Default is NO. By default, AVCapturePhotoOutput emits images with the same dimensions as its source AVCaptureDevice's activeFormat.formatDescription. However, if you set this property to YES, the AVCapturePhotoOutput emits images at its source AVCaptureDevice's activeFormat.highResolutionStillImageDimensions. Note that if you enable video stabilization (see AVCaptureConnection's preferredVideoStabilizationMode) for any output, the high resolution photos emitted by AVCapturePhotoOutput may be smaller by 10 or more percent. You may inspect your AVCaptureResolvedPhotoSettings in the delegate callbacks to discover the exact dimensions of the capture photo(s).
--
-- Starting in iOS 14.5 if you disable geometric distortion correction, the high resolution photo emitted by AVCapturePhotoOutput may be is smaller depending on the format.
--
-- ObjC selector: @- setHighResolutionPhotoEnabled:@
setHighResolutionPhotoEnabled :: IsAVCapturePhotoSettings avCapturePhotoSettings => avCapturePhotoSettings -> Bool -> IO ()
setHighResolutionPhotoEnabled avCapturePhotoSettings value =
  sendMessage avCapturePhotoSettings setHighResolutionPhotoEnabledSelector value

-- | depthDataDeliveryEnabled
--
-- Specifies whether AVDepthData should be captured along with the photo.
--
-- Default is NO. Set to YES if you wish to receive depth data with your photo. Throws an exception if -[AVCapturePhotoOutput depthDataDeliveryEnabled] is not set to YES or your delegate does not respond to the captureOutput:didFinishProcessingPhoto:error: selector. Note that setting this property to YES may add significant processing time to the delivery of your didFinishProcessingPhoto: callback.
--
-- For best rendering results in Apple's Photos.app, portrait photos should be captured with both embedded depth data and a portrait effects matte (see portraitEffectsMatteDeliveryEnabled). When supported, it is recommended to opt in for both of these auxiliary images in your photo captures involving depth.
--
-- ObjC selector: @- depthDataDeliveryEnabled@
depthDataDeliveryEnabled :: IsAVCapturePhotoSettings avCapturePhotoSettings => avCapturePhotoSettings -> IO Bool
depthDataDeliveryEnabled avCapturePhotoSettings =
  sendMessage avCapturePhotoSettings depthDataDeliveryEnabledSelector

-- | depthDataDeliveryEnabled
--
-- Specifies whether AVDepthData should be captured along with the photo.
--
-- Default is NO. Set to YES if you wish to receive depth data with your photo. Throws an exception if -[AVCapturePhotoOutput depthDataDeliveryEnabled] is not set to YES or your delegate does not respond to the captureOutput:didFinishProcessingPhoto:error: selector. Note that setting this property to YES may add significant processing time to the delivery of your didFinishProcessingPhoto: callback.
--
-- For best rendering results in Apple's Photos.app, portrait photos should be captured with both embedded depth data and a portrait effects matte (see portraitEffectsMatteDeliveryEnabled). When supported, it is recommended to opt in for both of these auxiliary images in your photo captures involving depth.
--
-- ObjC selector: @- setDepthDataDeliveryEnabled:@
setDepthDataDeliveryEnabled :: IsAVCapturePhotoSettings avCapturePhotoSettings => avCapturePhotoSettings -> Bool -> IO ()
setDepthDataDeliveryEnabled avCapturePhotoSettings value =
  sendMessage avCapturePhotoSettings setDepthDataDeliveryEnabledSelector value

-- | embedsDepthDataInPhoto
--
-- Specifies whether depth data included with this photo should be written to the photo's file structure.
--
-- Default is YES. When depthDataDeliveryEnabled is set to YES, this property specifies whether the included depth data should be written to the resulting photo's internal file structure. Depth data is currently only supported in HEIF and JPEG. This property is ignored if depthDataDeliveryEnabled is set to NO.
--
-- ObjC selector: @- embedsDepthDataInPhoto@
embedsDepthDataInPhoto :: IsAVCapturePhotoSettings avCapturePhotoSettings => avCapturePhotoSettings -> IO Bool
embedsDepthDataInPhoto avCapturePhotoSettings =
  sendMessage avCapturePhotoSettings embedsDepthDataInPhotoSelector

-- | embedsDepthDataInPhoto
--
-- Specifies whether depth data included with this photo should be written to the photo's file structure.
--
-- Default is YES. When depthDataDeliveryEnabled is set to YES, this property specifies whether the included depth data should be written to the resulting photo's internal file structure. Depth data is currently only supported in HEIF and JPEG. This property is ignored if depthDataDeliveryEnabled is set to NO.
--
-- ObjC selector: @- setEmbedsDepthDataInPhoto:@
setEmbedsDepthDataInPhoto :: IsAVCapturePhotoSettings avCapturePhotoSettings => avCapturePhotoSettings -> Bool -> IO ()
setEmbedsDepthDataInPhoto avCapturePhotoSettings value =
  sendMessage avCapturePhotoSettings setEmbedsDepthDataInPhotoSelector value

-- | depthDataFiltered
--
-- Specifies whether the depth data delivered with the photo should be filtered to fill invalid values.
--
-- Default is YES. This property is ignored unless depthDataDeliveryEnabled is set to YES. Depth data maps may contain invalid pixel values due to a variety of factors including occlusions and low light. When depthDataFiltered is set to YES, the photo output interpolates missing data, filling in all holes.
--
-- ObjC selector: @- depthDataFiltered@
depthDataFiltered :: IsAVCapturePhotoSettings avCapturePhotoSettings => avCapturePhotoSettings -> IO Bool
depthDataFiltered avCapturePhotoSettings =
  sendMessage avCapturePhotoSettings depthDataFilteredSelector

-- | depthDataFiltered
--
-- Specifies whether the depth data delivered with the photo should be filtered to fill invalid values.
--
-- Default is YES. This property is ignored unless depthDataDeliveryEnabled is set to YES. Depth data maps may contain invalid pixel values due to a variety of factors including occlusions and low light. When depthDataFiltered is set to YES, the photo output interpolates missing data, filling in all holes.
--
-- ObjC selector: @- setDepthDataFiltered:@
setDepthDataFiltered :: IsAVCapturePhotoSettings avCapturePhotoSettings => avCapturePhotoSettings -> Bool -> IO ()
setDepthDataFiltered avCapturePhotoSettings value =
  sendMessage avCapturePhotoSettings setDepthDataFilteredSelector value

-- | cameraCalibrationDataDeliveryEnabled
--
-- Specifies whether AVCameraCalibrationData should be captured and delivered along with this photo.
--
-- Default is NO. Set to YES if you wish to receive camera calibration data with your photo. Camera calibration data is delivered as a property of an AVCapturePhoto, so if you are using the CMSampleBuffer delegate callbacks rather than -captureOutput:didFinishProcessingPhoto:error:, an exception is thrown. Also, you may only set this property to YES if your AVCapturePhotoOutput's cameraCalibrationDataDeliverySupported property is YES and 2 or more devices are selected for virtual device constituent photo delivery. When requesting virtual device constituent photo delivery plus camera calibration data, the photos for each constituent device each contain camera calibration data. Note that AVCameraCalibrationData can be delivered as a property of an AVCapturePhoto or an AVDepthData, thus your delegate must respond to the captureOutput:didFinishProcessingPhoto:error: selector.
--
-- ObjC selector: @- cameraCalibrationDataDeliveryEnabled@
cameraCalibrationDataDeliveryEnabled :: IsAVCapturePhotoSettings avCapturePhotoSettings => avCapturePhotoSettings -> IO Bool
cameraCalibrationDataDeliveryEnabled avCapturePhotoSettings =
  sendMessage avCapturePhotoSettings cameraCalibrationDataDeliveryEnabledSelector

-- | cameraCalibrationDataDeliveryEnabled
--
-- Specifies whether AVCameraCalibrationData should be captured and delivered along with this photo.
--
-- Default is NO. Set to YES if you wish to receive camera calibration data with your photo. Camera calibration data is delivered as a property of an AVCapturePhoto, so if you are using the CMSampleBuffer delegate callbacks rather than -captureOutput:didFinishProcessingPhoto:error:, an exception is thrown. Also, you may only set this property to YES if your AVCapturePhotoOutput's cameraCalibrationDataDeliverySupported property is YES and 2 or more devices are selected for virtual device constituent photo delivery. When requesting virtual device constituent photo delivery plus camera calibration data, the photos for each constituent device each contain camera calibration data. Note that AVCameraCalibrationData can be delivered as a property of an AVCapturePhoto or an AVDepthData, thus your delegate must respond to the captureOutput:didFinishProcessingPhoto:error: selector.
--
-- ObjC selector: @- setCameraCalibrationDataDeliveryEnabled:@
setCameraCalibrationDataDeliveryEnabled :: IsAVCapturePhotoSettings avCapturePhotoSettings => avCapturePhotoSettings -> Bool -> IO ()
setCameraCalibrationDataDeliveryEnabled avCapturePhotoSettings value =
  sendMessage avCapturePhotoSettings setCameraCalibrationDataDeliveryEnabledSelector value

-- | portraitEffectsMatteDeliveryEnabled
--
-- Specifies whether an AVPortraitEffectsMatte should be captured along with the photo.
--
-- Default is NO. Set to YES if you wish to receive a portrait effects matte with your photo. Throws an exception if -[AVCapturePhotoOutput portraitEffectsMatteDeliveryEnabled] is not set to YES or your delegate does not respond to the captureOutput:didFinishProcessingPhoto:error: selector. Portrait effects matte generation requires depth to be present, so if you wish to enable portrait effects matte delivery, you must set depthDataDeliveryEnabled to YES. Setting this property to YES does not guarantee that a portrait effects matte will be present in the resulting AVCapturePhoto. As the property name implies, the matte is primarily used to improve the rendering quality of portrait effects on the image. If the photo's content lacks a clear foreground subject, no portrait effects matte is generated, and the -[AVCapturePhoto portraitEffectsMatte] property returns nil. Note that setting this property to YES may add significant processing time to the delivery of your didFinishProcessingPhoto: callback.
--
-- For best rendering results in Apple's Photos.app, portrait photos should be captured with both embedded depth data (see depthDataDeliveryEnabled) and a portrait effects matte. When supported, it is recommended to opt in for both of these auxiliary images in your photo captures involving depth.
--
-- ObjC selector: @- portraitEffectsMatteDeliveryEnabled@
portraitEffectsMatteDeliveryEnabled :: IsAVCapturePhotoSettings avCapturePhotoSettings => avCapturePhotoSettings -> IO Bool
portraitEffectsMatteDeliveryEnabled avCapturePhotoSettings =
  sendMessage avCapturePhotoSettings portraitEffectsMatteDeliveryEnabledSelector

-- | portraitEffectsMatteDeliveryEnabled
--
-- Specifies whether an AVPortraitEffectsMatte should be captured along with the photo.
--
-- Default is NO. Set to YES if you wish to receive a portrait effects matte with your photo. Throws an exception if -[AVCapturePhotoOutput portraitEffectsMatteDeliveryEnabled] is not set to YES or your delegate does not respond to the captureOutput:didFinishProcessingPhoto:error: selector. Portrait effects matte generation requires depth to be present, so if you wish to enable portrait effects matte delivery, you must set depthDataDeliveryEnabled to YES. Setting this property to YES does not guarantee that a portrait effects matte will be present in the resulting AVCapturePhoto. As the property name implies, the matte is primarily used to improve the rendering quality of portrait effects on the image. If the photo's content lacks a clear foreground subject, no portrait effects matte is generated, and the -[AVCapturePhoto portraitEffectsMatte] property returns nil. Note that setting this property to YES may add significant processing time to the delivery of your didFinishProcessingPhoto: callback.
--
-- For best rendering results in Apple's Photos.app, portrait photos should be captured with both embedded depth data (see depthDataDeliveryEnabled) and a portrait effects matte. When supported, it is recommended to opt in for both of these auxiliary images in your photo captures involving depth.
--
-- ObjC selector: @- setPortraitEffectsMatteDeliveryEnabled:@
setPortraitEffectsMatteDeliveryEnabled :: IsAVCapturePhotoSettings avCapturePhotoSettings => avCapturePhotoSettings -> Bool -> IO ()
setPortraitEffectsMatteDeliveryEnabled avCapturePhotoSettings value =
  sendMessage avCapturePhotoSettings setPortraitEffectsMatteDeliveryEnabledSelector value

-- | embedsPortraitEffectsMatteInPhoto
--
-- Specifies whether the portrait effects matte captured with this photo should be written to the photo's file structure.
--
-- Default is YES. When portraitEffectsMatteDeliveryEnabled is set to YES, this property specifies whether the included portrait effects matte should be written to the resulting photo's internal file structure. Portrait effects mattes are currently only supported in HEIF and JPEG. This property is ignored if portraitEffectsMatteDeliveryEnabled is set to NO.
--
-- ObjC selector: @- embedsPortraitEffectsMatteInPhoto@
embedsPortraitEffectsMatteInPhoto :: IsAVCapturePhotoSettings avCapturePhotoSettings => avCapturePhotoSettings -> IO Bool
embedsPortraitEffectsMatteInPhoto avCapturePhotoSettings =
  sendMessage avCapturePhotoSettings embedsPortraitEffectsMatteInPhotoSelector

-- | embedsPortraitEffectsMatteInPhoto
--
-- Specifies whether the portrait effects matte captured with this photo should be written to the photo's file structure.
--
-- Default is YES. When portraitEffectsMatteDeliveryEnabled is set to YES, this property specifies whether the included portrait effects matte should be written to the resulting photo's internal file structure. Portrait effects mattes are currently only supported in HEIF and JPEG. This property is ignored if portraitEffectsMatteDeliveryEnabled is set to NO.
--
-- ObjC selector: @- setEmbedsPortraitEffectsMatteInPhoto:@
setEmbedsPortraitEffectsMatteInPhoto :: IsAVCapturePhotoSettings avCapturePhotoSettings => avCapturePhotoSettings -> Bool -> IO ()
setEmbedsPortraitEffectsMatteInPhoto avCapturePhotoSettings value =
  sendMessage avCapturePhotoSettings setEmbedsPortraitEffectsMatteInPhotoSelector value

-- | enabledSemanticSegmentationMatteTypes
--
-- Specifies which types of AVSemanticSegmentationMatte should be captured along with the photo.
--
-- Default is empty array. You may set this property to an array of AVSemanticSegmentationMatteTypes you'd like to capture. Throws an exception if -[AVCapturePhotoOutput enabledSemanticSegmentationMatteTypes] does not contain any of the AVSemanticSegmentationMatteTypes specified. In other words, when setting up a capture session, you opt in for the superset of segmentation matte types you might like to receive, and then on a shot-by-shot basis, you may opt in to all or a subset of the previously specified types by setting this property. An exception is also thrown during -[AVCapturePhotoOutput capturePhotoWithSettings:delegate:] if your delegate does not respond to the captureOutput:didFinishProcessingPhoto:error: selector. Setting this property to YES does not guarantee that the specified mattes will be present in the resulting AVCapturePhoto. If the photo's content lacks any persons, for instance, no hair, skin, or teeth mattes are generated, and the -[AVCapturePhoto semanticSegmentationMatteForType:] property returns nil. Note that setting this property to YES may add significant processing time to the delivery of your didFinishProcessingPhoto: callback.
--
-- ObjC selector: @- enabledSemanticSegmentationMatteTypes@
enabledSemanticSegmentationMatteTypes :: IsAVCapturePhotoSettings avCapturePhotoSettings => avCapturePhotoSettings -> IO (Id NSArray)
enabledSemanticSegmentationMatteTypes avCapturePhotoSettings =
  sendMessage avCapturePhotoSettings enabledSemanticSegmentationMatteTypesSelector

-- | enabledSemanticSegmentationMatteTypes
--
-- Specifies which types of AVSemanticSegmentationMatte should be captured along with the photo.
--
-- Default is empty array. You may set this property to an array of AVSemanticSegmentationMatteTypes you'd like to capture. Throws an exception if -[AVCapturePhotoOutput enabledSemanticSegmentationMatteTypes] does not contain any of the AVSemanticSegmentationMatteTypes specified. In other words, when setting up a capture session, you opt in for the superset of segmentation matte types you might like to receive, and then on a shot-by-shot basis, you may opt in to all or a subset of the previously specified types by setting this property. An exception is also thrown during -[AVCapturePhotoOutput capturePhotoWithSettings:delegate:] if your delegate does not respond to the captureOutput:didFinishProcessingPhoto:error: selector. Setting this property to YES does not guarantee that the specified mattes will be present in the resulting AVCapturePhoto. If the photo's content lacks any persons, for instance, no hair, skin, or teeth mattes are generated, and the -[AVCapturePhoto semanticSegmentationMatteForType:] property returns nil. Note that setting this property to YES may add significant processing time to the delivery of your didFinishProcessingPhoto: callback.
--
-- ObjC selector: @- setEnabledSemanticSegmentationMatteTypes:@
setEnabledSemanticSegmentationMatteTypes :: (IsAVCapturePhotoSettings avCapturePhotoSettings, IsNSArray value) => avCapturePhotoSettings -> value -> IO ()
setEnabledSemanticSegmentationMatteTypes avCapturePhotoSettings value =
  sendMessage avCapturePhotoSettings setEnabledSemanticSegmentationMatteTypesSelector (toNSArray value)

-- | embedsSemanticSegmentationMattesInPhoto
--
-- Specifies whether enabledSemanticSegmentationMatteTypes captured with this photo should be written to the photo's file structure.
--
-- Default is YES. This property specifies whether the captured semantic segmentation mattes should be written to the resulting photo's internal file structure. Semantic segmentation mattes are currently only supported in HEIF and JPEG. This property is ignored if enabledSemanticSegmentationMatteTypes is set to an empty array.
--
-- ObjC selector: @- embedsSemanticSegmentationMattesInPhoto@
embedsSemanticSegmentationMattesInPhoto :: IsAVCapturePhotoSettings avCapturePhotoSettings => avCapturePhotoSettings -> IO Bool
embedsSemanticSegmentationMattesInPhoto avCapturePhotoSettings =
  sendMessage avCapturePhotoSettings embedsSemanticSegmentationMattesInPhotoSelector

-- | embedsSemanticSegmentationMattesInPhoto
--
-- Specifies whether enabledSemanticSegmentationMatteTypes captured with this photo should be written to the photo's file structure.
--
-- Default is YES. This property specifies whether the captured semantic segmentation mattes should be written to the resulting photo's internal file structure. Semantic segmentation mattes are currently only supported in HEIF and JPEG. This property is ignored if enabledSemanticSegmentationMatteTypes is set to an empty array.
--
-- ObjC selector: @- setEmbedsSemanticSegmentationMattesInPhoto:@
setEmbedsSemanticSegmentationMattesInPhoto :: IsAVCapturePhotoSettings avCapturePhotoSettings => avCapturePhotoSettings -> Bool -> IO ()
setEmbedsSemanticSegmentationMattesInPhoto avCapturePhotoSettings value =
  sendMessage avCapturePhotoSettings setEmbedsSemanticSegmentationMattesInPhotoSelector value

-- | metadata
--
-- A dictionary of metadata key/value pairs you'd like to have written to each photo in the capture request.
--
-- Valid metadata keys are found in <ImageIO/CGImageProperties.h>. AVCapturePhotoOutput inserts a base set of metadata into each photo it captures, such as kCGImagePropertyOrientation, kCGImagePropertyExifDictionary, and kCGImagePropertyMakerAppleDictionary. You may specify metadata keys and values that should be written to each photo in the capture request. If you've specified metadata that also appears in AVCapturePhotoOutput's base set, your value replaces the base value. An NSInvalidArgumentException is thrown if you specify keys other than those found in <ImageIO/CGImageProperties.h>.
--
-- ObjC selector: @- metadata@
metadata :: IsAVCapturePhotoSettings avCapturePhotoSettings => avCapturePhotoSettings -> IO (Id NSDictionary)
metadata avCapturePhotoSettings =
  sendMessage avCapturePhotoSettings metadataSelector

-- | metadata
--
-- A dictionary of metadata key/value pairs you'd like to have written to each photo in the capture request.
--
-- Valid metadata keys are found in <ImageIO/CGImageProperties.h>. AVCapturePhotoOutput inserts a base set of metadata into each photo it captures, such as kCGImagePropertyOrientation, kCGImagePropertyExifDictionary, and kCGImagePropertyMakerAppleDictionary. You may specify metadata keys and values that should be written to each photo in the capture request. If you've specified metadata that also appears in AVCapturePhotoOutput's base set, your value replaces the base value. An NSInvalidArgumentException is thrown if you specify keys other than those found in <ImageIO/CGImageProperties.h>.
--
-- ObjC selector: @- setMetadata:@
setMetadata :: (IsAVCapturePhotoSettings avCapturePhotoSettings, IsNSDictionary value) => avCapturePhotoSettings -> value -> IO ()
setMetadata avCapturePhotoSettings value =
  sendMessage avCapturePhotoSettings setMetadataSelector (toNSDictionary value)

-- | livePhotoMovieFileURL
--
-- Specifies that a Live Photo movie be captured to complement the still photo.
--
-- A Live Photo movie is a short movie (with audio, if you've added an audio input to your session) containing the moments right before and after the still photo. A QuickTime movie file will be written to disk at the URL specified if it is a valid file URL accessible to your app's sandbox. You may only set this property if AVCapturePhotoOutput's livePhotoCaptureSupported property is YES. When you specify a Live Photo, your AVCapturePhotoCaptureDelegate object must implement -captureOutput:didFinishProcessingLivePhotoToMovieFileAtURL:duration:photoDisplayTime:resolvedSettings:error:.
--
-- ObjC selector: @- livePhotoMovieFileURL@
livePhotoMovieFileURL :: IsAVCapturePhotoSettings avCapturePhotoSettings => avCapturePhotoSettings -> IO (Id NSURL)
livePhotoMovieFileURL avCapturePhotoSettings =
  sendMessage avCapturePhotoSettings livePhotoMovieFileURLSelector

-- | livePhotoMovieFileURL
--
-- Specifies that a Live Photo movie be captured to complement the still photo.
--
-- A Live Photo movie is a short movie (with audio, if you've added an audio input to your session) containing the moments right before and after the still photo. A QuickTime movie file will be written to disk at the URL specified if it is a valid file URL accessible to your app's sandbox. You may only set this property if AVCapturePhotoOutput's livePhotoCaptureSupported property is YES. When you specify a Live Photo, your AVCapturePhotoCaptureDelegate object must implement -captureOutput:didFinishProcessingLivePhotoToMovieFileAtURL:duration:photoDisplayTime:resolvedSettings:error:.
--
-- ObjC selector: @- setLivePhotoMovieFileURL:@
setLivePhotoMovieFileURL :: (IsAVCapturePhotoSettings avCapturePhotoSettings, IsNSURL value) => avCapturePhotoSettings -> value -> IO ()
setLivePhotoMovieFileURL avCapturePhotoSettings value =
  sendMessage avCapturePhotoSettings setLivePhotoMovieFileURLSelector (toNSURL value)

-- | livePhotoVideoCodecType
--
-- Specifies the video codec type to use when compressing video for the Live Photo movie complement.
--
-- Prior to iOS 11, all Live Photo movie video tracks are compressed using H.264. Beginning in iOS 11, you can select the Live Photo movie video compression format by specifying one of the strings present in AVCapturePhotoOutput's availableLivePhotoVideoCodecTypes array.
--
-- ObjC selector: @- livePhotoVideoCodecType@
livePhotoVideoCodecType :: IsAVCapturePhotoSettings avCapturePhotoSettings => avCapturePhotoSettings -> IO (Id NSString)
livePhotoVideoCodecType avCapturePhotoSettings =
  sendMessage avCapturePhotoSettings livePhotoVideoCodecTypeSelector

-- | livePhotoVideoCodecType
--
-- Specifies the video codec type to use when compressing video for the Live Photo movie complement.
--
-- Prior to iOS 11, all Live Photo movie video tracks are compressed using H.264. Beginning in iOS 11, you can select the Live Photo movie video compression format by specifying one of the strings present in AVCapturePhotoOutput's availableLivePhotoVideoCodecTypes array.
--
-- ObjC selector: @- setLivePhotoVideoCodecType:@
setLivePhotoVideoCodecType :: (IsAVCapturePhotoSettings avCapturePhotoSettings, IsNSString value) => avCapturePhotoSettings -> value -> IO ()
setLivePhotoVideoCodecType avCapturePhotoSettings value =
  sendMessage avCapturePhotoSettings setLivePhotoVideoCodecTypeSelector (toNSString value)

-- | livePhotoMovieMetadata
--
-- Movie-level metadata to be written to the Live Photo movie.
--
-- An array of AVMetadataItems to be inserted into the top level of the Live Photo movie. The receiver makes immutable copies of the AVMetadataItems in the array. Live Photo movies always contain a AVMetadataQuickTimeMetadataKeyContentIdentifier which allow them to be paired with a similar identifier in the MakerNote of the photo complement. AVCapturePhotoSettings generates a unique content identifier for you. If you provide a metadata array containing an AVMetadataItem with keyspace = AVMetadataKeySpaceQuickTimeMetadata and key = AVMetadataQuickTimeMetadataKeyContentIdentifier, an NSInvalidArgumentException is thrown.
--
-- ObjC selector: @- livePhotoMovieMetadata@
livePhotoMovieMetadata :: IsAVCapturePhotoSettings avCapturePhotoSettings => avCapturePhotoSettings -> IO (Id NSArray)
livePhotoMovieMetadata avCapturePhotoSettings =
  sendMessage avCapturePhotoSettings livePhotoMovieMetadataSelector

-- | livePhotoMovieMetadata
--
-- Movie-level metadata to be written to the Live Photo movie.
--
-- An array of AVMetadataItems to be inserted into the top level of the Live Photo movie. The receiver makes immutable copies of the AVMetadataItems in the array. Live Photo movies always contain a AVMetadataQuickTimeMetadataKeyContentIdentifier which allow them to be paired with a similar identifier in the MakerNote of the photo complement. AVCapturePhotoSettings generates a unique content identifier for you. If you provide a metadata array containing an AVMetadataItem with keyspace = AVMetadataKeySpaceQuickTimeMetadata and key = AVMetadataQuickTimeMetadataKeyContentIdentifier, an NSInvalidArgumentException is thrown.
--
-- ObjC selector: @- setLivePhotoMovieMetadata:@
setLivePhotoMovieMetadata :: (IsAVCapturePhotoSettings avCapturePhotoSettings, IsNSArray value) => avCapturePhotoSettings -> value -> IO ()
setLivePhotoMovieMetadata avCapturePhotoSettings value =
  sendMessage avCapturePhotoSettings setLivePhotoMovieMetadataSelector (toNSArray value)

-- | availablePreviewPhotoPixelFormatTypes
--
-- An array of available kCVPixelBufferPixelFormatTypeKeys that may be used when specifying a previewPhotoFormat.
--
-- The array is sorted such that the preview format requiring the fewest conversions is presented first.
--
-- ObjC selector: @- availablePreviewPhotoPixelFormatTypes@
availablePreviewPhotoPixelFormatTypes :: IsAVCapturePhotoSettings avCapturePhotoSettings => avCapturePhotoSettings -> IO (Id NSArray)
availablePreviewPhotoPixelFormatTypes avCapturePhotoSettings =
  sendMessage avCapturePhotoSettings availablePreviewPhotoPixelFormatTypesSelector

-- | previewPhotoFormat
--
-- A dictionary of Core Video pixel buffer attributes specifying the preview photo format to be delivered along with the RAW or processed photo.
--
-- A dictionary of pixel buffer attributes specifying a smaller version of the RAW or processed photo for preview purposes. The kCVPixelBufferPixelFormatTypeKey is required and must be present in the receiver's -availablePreviewPhotoPixelFormatTypes array. Optional keys are { kCVPixelBufferWidthKey | kCVPixelBufferHeightKey }. If you wish to specify dimensions, you must add both width and height. Width and height are only honored up to the display dimensions. If you specify a width and height whose aspect ratio differs from the RAW or processed photo, the larger of the two dimensions is honored and aspect ratio of the RAW or processed photo is always preserved.
--
-- ObjC selector: @- previewPhotoFormat@
previewPhotoFormat :: IsAVCapturePhotoSettings avCapturePhotoSettings => avCapturePhotoSettings -> IO (Id NSDictionary)
previewPhotoFormat avCapturePhotoSettings =
  sendMessage avCapturePhotoSettings previewPhotoFormatSelector

-- | previewPhotoFormat
--
-- A dictionary of Core Video pixel buffer attributes specifying the preview photo format to be delivered along with the RAW or processed photo.
--
-- A dictionary of pixel buffer attributes specifying a smaller version of the RAW or processed photo for preview purposes. The kCVPixelBufferPixelFormatTypeKey is required and must be present in the receiver's -availablePreviewPhotoPixelFormatTypes array. Optional keys are { kCVPixelBufferWidthKey | kCVPixelBufferHeightKey }. If you wish to specify dimensions, you must add both width and height. Width and height are only honored up to the display dimensions. If you specify a width and height whose aspect ratio differs from the RAW or processed photo, the larger of the two dimensions is honored and aspect ratio of the RAW or processed photo is always preserved.
--
-- ObjC selector: @- setPreviewPhotoFormat:@
setPreviewPhotoFormat :: (IsAVCapturePhotoSettings avCapturePhotoSettings, IsNSDictionary value) => avCapturePhotoSettings -> value -> IO ()
setPreviewPhotoFormat avCapturePhotoSettings value =
  sendMessage avCapturePhotoSettings setPreviewPhotoFormatSelector (toNSDictionary value)

-- | availableEmbeddedThumbnailPhotoCodecTypes
--
-- An array of available AVVideoCodecKeys that may be used when specifying an embeddedThumbnailPhotoFormat.
--
-- The array is sorted such that the thumbnail codec type that is most backward compatible is listed first.
--
-- ObjC selector: @- availableEmbeddedThumbnailPhotoCodecTypes@
availableEmbeddedThumbnailPhotoCodecTypes :: IsAVCapturePhotoSettings avCapturePhotoSettings => avCapturePhotoSettings -> IO (Id NSArray)
availableEmbeddedThumbnailPhotoCodecTypes avCapturePhotoSettings =
  sendMessage avCapturePhotoSettings availableEmbeddedThumbnailPhotoCodecTypesSelector

-- | embeddedThumbnailPhotoFormat
--
-- A dictionary of AVVideoSettings keys specifying the thumbnail format to be written to the processed or RAW photo.
--
-- A dictionary of AVVideoSettings keys specifying a thumbnail (usually smaller) version of the processed photo to be embedded in that image before calling the AVCapturePhotoCaptureDelegate. This image is sometimes referred to as a "thumbnail image". The AVVideoCodecKey is required and must be present in the receiver's -availableEmbeddedThumbnailPhotoCodecTypes array. Optional keys are { AVVideoWidthKey | AVVideoHeightKey }. If you wish to specify dimensions, you must specify both width and height. If you specify a width and height whose aspect ratio differs from the processed photo, the larger of the two dimensions is honored and aspect ratio of the RAW or processed photo is always preserved. For RAW captures, use -rawEmbeddedThumbnailPhotoFormat to specify the thumbnail format you'd like to capture in the RAW image. For apps linked on or after iOS 12, the raw thumbnail format must be specified using the -rawEmbeddedThumbnailPhotoFormat API rather than -embeddedThumbnailPhotoFormat. Beginning in iOS 12, HEIC files may contain thumbnails up to the full resolution of the main image.
--
-- ObjC selector: @- embeddedThumbnailPhotoFormat@
embeddedThumbnailPhotoFormat :: IsAVCapturePhotoSettings avCapturePhotoSettings => avCapturePhotoSettings -> IO (Id NSDictionary)
embeddedThumbnailPhotoFormat avCapturePhotoSettings =
  sendMessage avCapturePhotoSettings embeddedThumbnailPhotoFormatSelector

-- | embeddedThumbnailPhotoFormat
--
-- A dictionary of AVVideoSettings keys specifying the thumbnail format to be written to the processed or RAW photo.
--
-- A dictionary of AVVideoSettings keys specifying a thumbnail (usually smaller) version of the processed photo to be embedded in that image before calling the AVCapturePhotoCaptureDelegate. This image is sometimes referred to as a "thumbnail image". The AVVideoCodecKey is required and must be present in the receiver's -availableEmbeddedThumbnailPhotoCodecTypes array. Optional keys are { AVVideoWidthKey | AVVideoHeightKey }. If you wish to specify dimensions, you must specify both width and height. If you specify a width and height whose aspect ratio differs from the processed photo, the larger of the two dimensions is honored and aspect ratio of the RAW or processed photo is always preserved. For RAW captures, use -rawEmbeddedThumbnailPhotoFormat to specify the thumbnail format you'd like to capture in the RAW image. For apps linked on or after iOS 12, the raw thumbnail format must be specified using the -rawEmbeddedThumbnailPhotoFormat API rather than -embeddedThumbnailPhotoFormat. Beginning in iOS 12, HEIC files may contain thumbnails up to the full resolution of the main image.
--
-- ObjC selector: @- setEmbeddedThumbnailPhotoFormat:@
setEmbeddedThumbnailPhotoFormat :: (IsAVCapturePhotoSettings avCapturePhotoSettings, IsNSDictionary value) => avCapturePhotoSettings -> value -> IO ()
setEmbeddedThumbnailPhotoFormat avCapturePhotoSettings value =
  sendMessage avCapturePhotoSettings setEmbeddedThumbnailPhotoFormatSelector (toNSDictionary value)

-- | availableRawEmbeddedThumbnailPhotoCodecTypes
--
-- An array of available AVVideoCodecKeys that may be used when specifying a rawEmbeddedThumbnailPhotoFormat.
--
-- The array is sorted such that the thumbnail codec type that is most backward compatible is listed first.
--
-- ObjC selector: @- availableRawEmbeddedThumbnailPhotoCodecTypes@
availableRawEmbeddedThumbnailPhotoCodecTypes :: IsAVCapturePhotoSettings avCapturePhotoSettings => avCapturePhotoSettings -> IO (Id NSArray)
availableRawEmbeddedThumbnailPhotoCodecTypes avCapturePhotoSettings =
  sendMessage avCapturePhotoSettings availableRawEmbeddedThumbnailPhotoCodecTypesSelector

-- | rawEmbeddedThumbnailPhotoFormat
--
-- A dictionary of AVVideoSettings keys specifying the thumbnail format to be written to the RAW photo in a RAW photo request.
--
-- A dictionary of AVVideoSettings keys specifying a thumbnail (usually smaller) version of the RAW photo to be embedded in that image's DNG before calling back the AVCapturePhotoCaptureDelegate. The AVVideoCodecKey is required and must be present in the receiver's -availableRawEmbeddedThumbnailPhotoCodecTypes array. Optional keys are { AVVideoWidthKey | AVVideoHeightKey }. If you wish to specify dimensions, you must specify both width and height. If you specify a width and height whose aspect ratio differs from the RAW or processed photo, the larger of the two dimensions is honored and aspect ratio of the RAW or processed photo is always preserved. For apps linked on or after iOS 12, the raw thumbnail format must be specified using the -rawEmbeddedThumbnailPhotoFormat API rather than -embeddedThumbnailPhotoFormat. Beginning in iOS 12, DNG files may contain thumbnails up to the full resolution of the RAW image.
--
-- ObjC selector: @- rawEmbeddedThumbnailPhotoFormat@
rawEmbeddedThumbnailPhotoFormat :: IsAVCapturePhotoSettings avCapturePhotoSettings => avCapturePhotoSettings -> IO (Id NSDictionary)
rawEmbeddedThumbnailPhotoFormat avCapturePhotoSettings =
  sendMessage avCapturePhotoSettings rawEmbeddedThumbnailPhotoFormatSelector

-- | rawEmbeddedThumbnailPhotoFormat
--
-- A dictionary of AVVideoSettings keys specifying the thumbnail format to be written to the RAW photo in a RAW photo request.
--
-- A dictionary of AVVideoSettings keys specifying a thumbnail (usually smaller) version of the RAW photo to be embedded in that image's DNG before calling back the AVCapturePhotoCaptureDelegate. The AVVideoCodecKey is required and must be present in the receiver's -availableRawEmbeddedThumbnailPhotoCodecTypes array. Optional keys are { AVVideoWidthKey | AVVideoHeightKey }. If you wish to specify dimensions, you must specify both width and height. If you specify a width and height whose aspect ratio differs from the RAW or processed photo, the larger of the two dimensions is honored and aspect ratio of the RAW or processed photo is always preserved. For apps linked on or after iOS 12, the raw thumbnail format must be specified using the -rawEmbeddedThumbnailPhotoFormat API rather than -embeddedThumbnailPhotoFormat. Beginning in iOS 12, DNG files may contain thumbnails up to the full resolution of the RAW image.
--
-- ObjC selector: @- setRawEmbeddedThumbnailPhotoFormat:@
setRawEmbeddedThumbnailPhotoFormat :: (IsAVCapturePhotoSettings avCapturePhotoSettings, IsNSDictionary value) => avCapturePhotoSettings -> value -> IO ()
setRawEmbeddedThumbnailPhotoFormat avCapturePhotoSettings value =
  sendMessage avCapturePhotoSettings setRawEmbeddedThumbnailPhotoFormatSelector (toNSDictionary value)

-- | autoContentAwareDistortionCorrectionEnabled
--
-- Specifies whether the photo output should use content aware distortion correction on this photo request (at its discretion).
--
-- Default is NO. Set to YES if you wish content aware distortion correction to be performed on your AVCapturePhotos, when the photo output deems it necessary. Photos may or may not benefit from distortion correction. For instance, photos lacking faces may be left as is. Setting this property to YES does introduce a small additional amount of latency to the photo processing. You may check your AVCaptureResolvedPhotoSettings to see whether content aware distortion correction will be enabled for a given photo request. Throws an exception if -[AVCapturePhotoOutput contentAwareDistortionCorrectionEnabled] is not set to YES.
--
-- ObjC selector: @- autoContentAwareDistortionCorrectionEnabled@
autoContentAwareDistortionCorrectionEnabled :: IsAVCapturePhotoSettings avCapturePhotoSettings => avCapturePhotoSettings -> IO Bool
autoContentAwareDistortionCorrectionEnabled avCapturePhotoSettings =
  sendMessage avCapturePhotoSettings autoContentAwareDistortionCorrectionEnabledSelector

-- | autoContentAwareDistortionCorrectionEnabled
--
-- Specifies whether the photo output should use content aware distortion correction on this photo request (at its discretion).
--
-- Default is NO. Set to YES if you wish content aware distortion correction to be performed on your AVCapturePhotos, when the photo output deems it necessary. Photos may or may not benefit from distortion correction. For instance, photos lacking faces may be left as is. Setting this property to YES does introduce a small additional amount of latency to the photo processing. You may check your AVCaptureResolvedPhotoSettings to see whether content aware distortion correction will be enabled for a given photo request. Throws an exception if -[AVCapturePhotoOutput contentAwareDistortionCorrectionEnabled] is not set to YES.
--
-- ObjC selector: @- setAutoContentAwareDistortionCorrectionEnabled:@
setAutoContentAwareDistortionCorrectionEnabled :: IsAVCapturePhotoSettings avCapturePhotoSettings => avCapturePhotoSettings -> Bool -> IO ()
setAutoContentAwareDistortionCorrectionEnabled avCapturePhotoSettings value =
  sendMessage avCapturePhotoSettings setAutoContentAwareDistortionCorrectionEnabledSelector value

-- | constantColorEnabled
--
-- Specifies whether the photo will be captured with constant color.
--
-- Default is NO. Set to YES if you wish to capture a constant color photo. Throws an exception if -[AVCapturePhotoOutput constantColorEnabled] is not set to YES.
--
-- ObjC selector: @- constantColorEnabled@
constantColorEnabled :: IsAVCapturePhotoSettings avCapturePhotoSettings => avCapturePhotoSettings -> IO Bool
constantColorEnabled avCapturePhotoSettings =
  sendMessage avCapturePhotoSettings constantColorEnabledSelector

-- | constantColorEnabled
--
-- Specifies whether the photo will be captured with constant color.
--
-- Default is NO. Set to YES if you wish to capture a constant color photo. Throws an exception if -[AVCapturePhotoOutput constantColorEnabled] is not set to YES.
--
-- ObjC selector: @- setConstantColorEnabled:@
setConstantColorEnabled :: IsAVCapturePhotoSettings avCapturePhotoSettings => avCapturePhotoSettings -> Bool -> IO ()
setConstantColorEnabled avCapturePhotoSettings value =
  sendMessage avCapturePhotoSettings setConstantColorEnabledSelector value

-- | constantColorFallbackPhotoDeliveryEnabled
--
-- Specifies whether a fallback photo is delivered when taking a constant color capture.
--
-- Default is NO. Set to YES if you wish to receive a fallback photo that can be used in case the main constant color photo's confidence level is too low for your use case.
--
-- ObjC selector: @- constantColorFallbackPhotoDeliveryEnabled@
constantColorFallbackPhotoDeliveryEnabled :: IsAVCapturePhotoSettings avCapturePhotoSettings => avCapturePhotoSettings -> IO Bool
constantColorFallbackPhotoDeliveryEnabled avCapturePhotoSettings =
  sendMessage avCapturePhotoSettings constantColorFallbackPhotoDeliveryEnabledSelector

-- | constantColorFallbackPhotoDeliveryEnabled
--
-- Specifies whether a fallback photo is delivered when taking a constant color capture.
--
-- Default is NO. Set to YES if you wish to receive a fallback photo that can be used in case the main constant color photo's confidence level is too low for your use case.
--
-- ObjC selector: @- setConstantColorFallbackPhotoDeliveryEnabled:@
setConstantColorFallbackPhotoDeliveryEnabled :: IsAVCapturePhotoSettings avCapturePhotoSettings => avCapturePhotoSettings -> Bool -> IO ()
setConstantColorFallbackPhotoDeliveryEnabled avCapturePhotoSettings value =
  sendMessage avCapturePhotoSettings setConstantColorFallbackPhotoDeliveryEnabledSelector value

-- | shutterSoundSuppressionEnabled
--
-- Specifies whether the built-in shutter sound should be suppressed when capturing a photo with these settings.
--
-- Default is NO. Set to YES if you wish to suppress AVCapturePhotoOutput's built-in shutter sound for this request. AVCapturePhotoOutput throws an NSInvalidArgumentException in @-capturePhotoWithSettings:@ if its @shutterSoundSuppressionSupported@ property returns NO.
--
-- ObjC selector: @- shutterSoundSuppressionEnabled@
shutterSoundSuppressionEnabled :: IsAVCapturePhotoSettings avCapturePhotoSettings => avCapturePhotoSettings -> IO Bool
shutterSoundSuppressionEnabled avCapturePhotoSettings =
  sendMessage avCapturePhotoSettings shutterSoundSuppressionEnabledSelector

-- | shutterSoundSuppressionEnabled
--
-- Specifies whether the built-in shutter sound should be suppressed when capturing a photo with these settings.
--
-- Default is NO. Set to YES if you wish to suppress AVCapturePhotoOutput's built-in shutter sound for this request. AVCapturePhotoOutput throws an NSInvalidArgumentException in @-capturePhotoWithSettings:@ if its @shutterSoundSuppressionSupported@ property returns NO.
--
-- ObjC selector: @- setShutterSoundSuppressionEnabled:@
setShutterSoundSuppressionEnabled :: IsAVCapturePhotoSettings avCapturePhotoSettings => avCapturePhotoSettings -> Bool -> IO ()
setShutterSoundSuppressionEnabled avCapturePhotoSettings value =
  sendMessage avCapturePhotoSettings setShutterSoundSuppressionEnabledSelector value

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @photoSettings@
photoSettingsSelector :: Selector '[] (Id AVCapturePhotoSettings)
photoSettingsSelector = mkSelector "photoSettings"

-- | @Selector@ for @photoSettingsWithFormat:@
photoSettingsWithFormatSelector :: Selector '[Id NSDictionary] (Id AVCapturePhotoSettings)
photoSettingsWithFormatSelector = mkSelector "photoSettingsWithFormat:"

-- | @Selector@ for @photoSettingsWithRawPixelFormatType:@
photoSettingsWithRawPixelFormatTypeSelector :: Selector '[CUInt] (Id AVCapturePhotoSettings)
photoSettingsWithRawPixelFormatTypeSelector = mkSelector "photoSettingsWithRawPixelFormatType:"

-- | @Selector@ for @photoSettingsWithRawPixelFormatType:processedFormat:@
photoSettingsWithRawPixelFormatType_processedFormatSelector :: Selector '[CUInt, Id NSDictionary] (Id AVCapturePhotoSettings)
photoSettingsWithRawPixelFormatType_processedFormatSelector = mkSelector "photoSettingsWithRawPixelFormatType:processedFormat:"

-- | @Selector@ for @photoSettingsWithRawPixelFormatType:rawFileType:processedFormat:processedFileType:@
photoSettingsWithRawPixelFormatType_rawFileType_processedFormat_processedFileTypeSelector :: Selector '[CUInt, Id NSString, Id NSDictionary, Id NSString] (Id AVCapturePhotoSettings)
photoSettingsWithRawPixelFormatType_rawFileType_processedFormat_processedFileTypeSelector = mkSelector "photoSettingsWithRawPixelFormatType:rawFileType:processedFormat:processedFileType:"

-- | @Selector@ for @photoSettingsFromPhotoSettings:@
photoSettingsFromPhotoSettingsSelector :: Selector '[Id AVCapturePhotoSettings] (Id AVCapturePhotoSettings)
photoSettingsFromPhotoSettingsSelector = mkSelector "photoSettingsFromPhotoSettings:"

-- | @Selector@ for @uniqueID@
uniqueIDSelector :: Selector '[] CLong
uniqueIDSelector = mkSelector "uniqueID"

-- | @Selector@ for @format@
formatSelector :: Selector '[] (Id NSDictionary)
formatSelector = mkSelector "format"

-- | @Selector@ for @rawFileFormat@
rawFileFormatSelector :: Selector '[] (Id NSDictionary)
rawFileFormatSelector = mkSelector "rawFileFormat"

-- | @Selector@ for @setRawFileFormat:@
setRawFileFormatSelector :: Selector '[Id NSDictionary] ()
setRawFileFormatSelector = mkSelector "setRawFileFormat:"

-- | @Selector@ for @processedFileType@
processedFileTypeSelector :: Selector '[] (Id NSString)
processedFileTypeSelector = mkSelector "processedFileType"

-- | @Selector@ for @rawPhotoPixelFormatType@
rawPhotoPixelFormatTypeSelector :: Selector '[] CUInt
rawPhotoPixelFormatTypeSelector = mkSelector "rawPhotoPixelFormatType"

-- | @Selector@ for @rawFileType@
rawFileTypeSelector :: Selector '[] (Id NSString)
rawFileTypeSelector = mkSelector "rawFileType"

-- | @Selector@ for @flashMode@
flashModeSelector :: Selector '[] AVCaptureFlashMode
flashModeSelector = mkSelector "flashMode"

-- | @Selector@ for @setFlashMode:@
setFlashModeSelector :: Selector '[AVCaptureFlashMode] ()
setFlashModeSelector = mkSelector "setFlashMode:"

-- | @Selector@ for @autoRedEyeReductionEnabled@
autoRedEyeReductionEnabledSelector :: Selector '[] Bool
autoRedEyeReductionEnabledSelector = mkSelector "autoRedEyeReductionEnabled"

-- | @Selector@ for @setAutoRedEyeReductionEnabled:@
setAutoRedEyeReductionEnabledSelector :: Selector '[Bool] ()
setAutoRedEyeReductionEnabledSelector = mkSelector "setAutoRedEyeReductionEnabled:"

-- | @Selector@ for @photoQualityPrioritization@
photoQualityPrioritizationSelector :: Selector '[] AVCapturePhotoQualityPrioritization
photoQualityPrioritizationSelector = mkSelector "photoQualityPrioritization"

-- | @Selector@ for @setPhotoQualityPrioritization:@
setPhotoQualityPrioritizationSelector :: Selector '[AVCapturePhotoQualityPrioritization] ()
setPhotoQualityPrioritizationSelector = mkSelector "setPhotoQualityPrioritization:"

-- | @Selector@ for @autoStillImageStabilizationEnabled@
autoStillImageStabilizationEnabledSelector :: Selector '[] Bool
autoStillImageStabilizationEnabledSelector = mkSelector "autoStillImageStabilizationEnabled"

-- | @Selector@ for @setAutoStillImageStabilizationEnabled:@
setAutoStillImageStabilizationEnabledSelector :: Selector '[Bool] ()
setAutoStillImageStabilizationEnabledSelector = mkSelector "setAutoStillImageStabilizationEnabled:"

-- | @Selector@ for @autoVirtualDeviceFusionEnabled@
autoVirtualDeviceFusionEnabledSelector :: Selector '[] Bool
autoVirtualDeviceFusionEnabledSelector = mkSelector "autoVirtualDeviceFusionEnabled"

-- | @Selector@ for @setAutoVirtualDeviceFusionEnabled:@
setAutoVirtualDeviceFusionEnabledSelector :: Selector '[Bool] ()
setAutoVirtualDeviceFusionEnabledSelector = mkSelector "setAutoVirtualDeviceFusionEnabled:"

-- | @Selector@ for @autoDualCameraFusionEnabled@
autoDualCameraFusionEnabledSelector :: Selector '[] Bool
autoDualCameraFusionEnabledSelector = mkSelector "autoDualCameraFusionEnabled"

-- | @Selector@ for @setAutoDualCameraFusionEnabled:@
setAutoDualCameraFusionEnabledSelector :: Selector '[Bool] ()
setAutoDualCameraFusionEnabledSelector = mkSelector "setAutoDualCameraFusionEnabled:"

-- | @Selector@ for @virtualDeviceConstituentPhotoDeliveryEnabledDevices@
virtualDeviceConstituentPhotoDeliveryEnabledDevicesSelector :: Selector '[] (Id NSArray)
virtualDeviceConstituentPhotoDeliveryEnabledDevicesSelector = mkSelector "virtualDeviceConstituentPhotoDeliveryEnabledDevices"

-- | @Selector@ for @setVirtualDeviceConstituentPhotoDeliveryEnabledDevices:@
setVirtualDeviceConstituentPhotoDeliveryEnabledDevicesSelector :: Selector '[Id NSArray] ()
setVirtualDeviceConstituentPhotoDeliveryEnabledDevicesSelector = mkSelector "setVirtualDeviceConstituentPhotoDeliveryEnabledDevices:"

-- | @Selector@ for @dualCameraDualPhotoDeliveryEnabled@
dualCameraDualPhotoDeliveryEnabledSelector :: Selector '[] Bool
dualCameraDualPhotoDeliveryEnabledSelector = mkSelector "dualCameraDualPhotoDeliveryEnabled"

-- | @Selector@ for @setDualCameraDualPhotoDeliveryEnabled:@
setDualCameraDualPhotoDeliveryEnabledSelector :: Selector '[Bool] ()
setDualCameraDualPhotoDeliveryEnabledSelector = mkSelector "setDualCameraDualPhotoDeliveryEnabled:"

-- | @Selector@ for @highResolutionPhotoEnabled@
highResolutionPhotoEnabledSelector :: Selector '[] Bool
highResolutionPhotoEnabledSelector = mkSelector "highResolutionPhotoEnabled"

-- | @Selector@ for @setHighResolutionPhotoEnabled:@
setHighResolutionPhotoEnabledSelector :: Selector '[Bool] ()
setHighResolutionPhotoEnabledSelector = mkSelector "setHighResolutionPhotoEnabled:"

-- | @Selector@ for @depthDataDeliveryEnabled@
depthDataDeliveryEnabledSelector :: Selector '[] Bool
depthDataDeliveryEnabledSelector = mkSelector "depthDataDeliveryEnabled"

-- | @Selector@ for @setDepthDataDeliveryEnabled:@
setDepthDataDeliveryEnabledSelector :: Selector '[Bool] ()
setDepthDataDeliveryEnabledSelector = mkSelector "setDepthDataDeliveryEnabled:"

-- | @Selector@ for @embedsDepthDataInPhoto@
embedsDepthDataInPhotoSelector :: Selector '[] Bool
embedsDepthDataInPhotoSelector = mkSelector "embedsDepthDataInPhoto"

-- | @Selector@ for @setEmbedsDepthDataInPhoto:@
setEmbedsDepthDataInPhotoSelector :: Selector '[Bool] ()
setEmbedsDepthDataInPhotoSelector = mkSelector "setEmbedsDepthDataInPhoto:"

-- | @Selector@ for @depthDataFiltered@
depthDataFilteredSelector :: Selector '[] Bool
depthDataFilteredSelector = mkSelector "depthDataFiltered"

-- | @Selector@ for @setDepthDataFiltered:@
setDepthDataFilteredSelector :: Selector '[Bool] ()
setDepthDataFilteredSelector = mkSelector "setDepthDataFiltered:"

-- | @Selector@ for @cameraCalibrationDataDeliveryEnabled@
cameraCalibrationDataDeliveryEnabledSelector :: Selector '[] Bool
cameraCalibrationDataDeliveryEnabledSelector = mkSelector "cameraCalibrationDataDeliveryEnabled"

-- | @Selector@ for @setCameraCalibrationDataDeliveryEnabled:@
setCameraCalibrationDataDeliveryEnabledSelector :: Selector '[Bool] ()
setCameraCalibrationDataDeliveryEnabledSelector = mkSelector "setCameraCalibrationDataDeliveryEnabled:"

-- | @Selector@ for @portraitEffectsMatteDeliveryEnabled@
portraitEffectsMatteDeliveryEnabledSelector :: Selector '[] Bool
portraitEffectsMatteDeliveryEnabledSelector = mkSelector "portraitEffectsMatteDeliveryEnabled"

-- | @Selector@ for @setPortraitEffectsMatteDeliveryEnabled:@
setPortraitEffectsMatteDeliveryEnabledSelector :: Selector '[Bool] ()
setPortraitEffectsMatteDeliveryEnabledSelector = mkSelector "setPortraitEffectsMatteDeliveryEnabled:"

-- | @Selector@ for @embedsPortraitEffectsMatteInPhoto@
embedsPortraitEffectsMatteInPhotoSelector :: Selector '[] Bool
embedsPortraitEffectsMatteInPhotoSelector = mkSelector "embedsPortraitEffectsMatteInPhoto"

-- | @Selector@ for @setEmbedsPortraitEffectsMatteInPhoto:@
setEmbedsPortraitEffectsMatteInPhotoSelector :: Selector '[Bool] ()
setEmbedsPortraitEffectsMatteInPhotoSelector = mkSelector "setEmbedsPortraitEffectsMatteInPhoto:"

-- | @Selector@ for @enabledSemanticSegmentationMatteTypes@
enabledSemanticSegmentationMatteTypesSelector :: Selector '[] (Id NSArray)
enabledSemanticSegmentationMatteTypesSelector = mkSelector "enabledSemanticSegmentationMatteTypes"

-- | @Selector@ for @setEnabledSemanticSegmentationMatteTypes:@
setEnabledSemanticSegmentationMatteTypesSelector :: Selector '[Id NSArray] ()
setEnabledSemanticSegmentationMatteTypesSelector = mkSelector "setEnabledSemanticSegmentationMatteTypes:"

-- | @Selector@ for @embedsSemanticSegmentationMattesInPhoto@
embedsSemanticSegmentationMattesInPhotoSelector :: Selector '[] Bool
embedsSemanticSegmentationMattesInPhotoSelector = mkSelector "embedsSemanticSegmentationMattesInPhoto"

-- | @Selector@ for @setEmbedsSemanticSegmentationMattesInPhoto:@
setEmbedsSemanticSegmentationMattesInPhotoSelector :: Selector '[Bool] ()
setEmbedsSemanticSegmentationMattesInPhotoSelector = mkSelector "setEmbedsSemanticSegmentationMattesInPhoto:"

-- | @Selector@ for @metadata@
metadataSelector :: Selector '[] (Id NSDictionary)
metadataSelector = mkSelector "metadata"

-- | @Selector@ for @setMetadata:@
setMetadataSelector :: Selector '[Id NSDictionary] ()
setMetadataSelector = mkSelector "setMetadata:"

-- | @Selector@ for @livePhotoMovieFileURL@
livePhotoMovieFileURLSelector :: Selector '[] (Id NSURL)
livePhotoMovieFileURLSelector = mkSelector "livePhotoMovieFileURL"

-- | @Selector@ for @setLivePhotoMovieFileURL:@
setLivePhotoMovieFileURLSelector :: Selector '[Id NSURL] ()
setLivePhotoMovieFileURLSelector = mkSelector "setLivePhotoMovieFileURL:"

-- | @Selector@ for @livePhotoVideoCodecType@
livePhotoVideoCodecTypeSelector :: Selector '[] (Id NSString)
livePhotoVideoCodecTypeSelector = mkSelector "livePhotoVideoCodecType"

-- | @Selector@ for @setLivePhotoVideoCodecType:@
setLivePhotoVideoCodecTypeSelector :: Selector '[Id NSString] ()
setLivePhotoVideoCodecTypeSelector = mkSelector "setLivePhotoVideoCodecType:"

-- | @Selector@ for @livePhotoMovieMetadata@
livePhotoMovieMetadataSelector :: Selector '[] (Id NSArray)
livePhotoMovieMetadataSelector = mkSelector "livePhotoMovieMetadata"

-- | @Selector@ for @setLivePhotoMovieMetadata:@
setLivePhotoMovieMetadataSelector :: Selector '[Id NSArray] ()
setLivePhotoMovieMetadataSelector = mkSelector "setLivePhotoMovieMetadata:"

-- | @Selector@ for @availablePreviewPhotoPixelFormatTypes@
availablePreviewPhotoPixelFormatTypesSelector :: Selector '[] (Id NSArray)
availablePreviewPhotoPixelFormatTypesSelector = mkSelector "availablePreviewPhotoPixelFormatTypes"

-- | @Selector@ for @previewPhotoFormat@
previewPhotoFormatSelector :: Selector '[] (Id NSDictionary)
previewPhotoFormatSelector = mkSelector "previewPhotoFormat"

-- | @Selector@ for @setPreviewPhotoFormat:@
setPreviewPhotoFormatSelector :: Selector '[Id NSDictionary] ()
setPreviewPhotoFormatSelector = mkSelector "setPreviewPhotoFormat:"

-- | @Selector@ for @availableEmbeddedThumbnailPhotoCodecTypes@
availableEmbeddedThumbnailPhotoCodecTypesSelector :: Selector '[] (Id NSArray)
availableEmbeddedThumbnailPhotoCodecTypesSelector = mkSelector "availableEmbeddedThumbnailPhotoCodecTypes"

-- | @Selector@ for @embeddedThumbnailPhotoFormat@
embeddedThumbnailPhotoFormatSelector :: Selector '[] (Id NSDictionary)
embeddedThumbnailPhotoFormatSelector = mkSelector "embeddedThumbnailPhotoFormat"

-- | @Selector@ for @setEmbeddedThumbnailPhotoFormat:@
setEmbeddedThumbnailPhotoFormatSelector :: Selector '[Id NSDictionary] ()
setEmbeddedThumbnailPhotoFormatSelector = mkSelector "setEmbeddedThumbnailPhotoFormat:"

-- | @Selector@ for @availableRawEmbeddedThumbnailPhotoCodecTypes@
availableRawEmbeddedThumbnailPhotoCodecTypesSelector :: Selector '[] (Id NSArray)
availableRawEmbeddedThumbnailPhotoCodecTypesSelector = mkSelector "availableRawEmbeddedThumbnailPhotoCodecTypes"

-- | @Selector@ for @rawEmbeddedThumbnailPhotoFormat@
rawEmbeddedThumbnailPhotoFormatSelector :: Selector '[] (Id NSDictionary)
rawEmbeddedThumbnailPhotoFormatSelector = mkSelector "rawEmbeddedThumbnailPhotoFormat"

-- | @Selector@ for @setRawEmbeddedThumbnailPhotoFormat:@
setRawEmbeddedThumbnailPhotoFormatSelector :: Selector '[Id NSDictionary] ()
setRawEmbeddedThumbnailPhotoFormatSelector = mkSelector "setRawEmbeddedThumbnailPhotoFormat:"

-- | @Selector@ for @autoContentAwareDistortionCorrectionEnabled@
autoContentAwareDistortionCorrectionEnabledSelector :: Selector '[] Bool
autoContentAwareDistortionCorrectionEnabledSelector = mkSelector "autoContentAwareDistortionCorrectionEnabled"

-- | @Selector@ for @setAutoContentAwareDistortionCorrectionEnabled:@
setAutoContentAwareDistortionCorrectionEnabledSelector :: Selector '[Bool] ()
setAutoContentAwareDistortionCorrectionEnabledSelector = mkSelector "setAutoContentAwareDistortionCorrectionEnabled:"

-- | @Selector@ for @constantColorEnabled@
constantColorEnabledSelector :: Selector '[] Bool
constantColorEnabledSelector = mkSelector "constantColorEnabled"

-- | @Selector@ for @setConstantColorEnabled:@
setConstantColorEnabledSelector :: Selector '[Bool] ()
setConstantColorEnabledSelector = mkSelector "setConstantColorEnabled:"

-- | @Selector@ for @constantColorFallbackPhotoDeliveryEnabled@
constantColorFallbackPhotoDeliveryEnabledSelector :: Selector '[] Bool
constantColorFallbackPhotoDeliveryEnabledSelector = mkSelector "constantColorFallbackPhotoDeliveryEnabled"

-- | @Selector@ for @setConstantColorFallbackPhotoDeliveryEnabled:@
setConstantColorFallbackPhotoDeliveryEnabledSelector :: Selector '[Bool] ()
setConstantColorFallbackPhotoDeliveryEnabledSelector = mkSelector "setConstantColorFallbackPhotoDeliveryEnabled:"

-- | @Selector@ for @shutterSoundSuppressionEnabled@
shutterSoundSuppressionEnabledSelector :: Selector '[] Bool
shutterSoundSuppressionEnabledSelector = mkSelector "shutterSoundSuppressionEnabled"

-- | @Selector@ for @setShutterSoundSuppressionEnabled:@
setShutterSoundSuppressionEnabledSelector :: Selector '[Bool] ()
setShutterSoundSuppressionEnabledSelector = mkSelector "setShutterSoundSuppressionEnabled:"

