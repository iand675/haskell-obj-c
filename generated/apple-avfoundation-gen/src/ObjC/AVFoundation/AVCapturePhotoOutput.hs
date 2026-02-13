{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | AVCapturePhotoOutput
--
-- AVCapturePhotoOutput is a concrete subclass of AVCaptureOutput that supersedes AVCaptureStillImageOutput as the preferred interface for capturing photos. In addition to capturing all flavors of still image supported by AVCaptureStillImageOutput, it supports Live Photo capture, preview-sized image delivery, wide color, RAW, RAW+JPG and RAW+DNG formats.
--
-- Taking a photo is multi-step process. Clients wishing to build a responsive UI need to know about the progress of a photo capture request as it advances from capture to processing to finished delivery. AVCapturePhotoOutput informs clients of photo capture progress through a delegate protocol. To take a picture, a client instantiates and configures an AVCapturePhotoSettings object, then calls AVCapturePhotoOutput's -capturePhotoWithSettings:delegate:, passing a delegate to be informed when events relating to the photo capture occur (e.g., the photo is about to be captured, the photo has been captured but not processed yet, the Live Photo movie is ready, etc.).
--
-- Some AVCapturePhotoSettings properties can be set to "Auto", such as flashMode. When set to AVCaptureFlashModeAuto, the photo output decides at capture time whether the current scene and lighting conditions require use of the flash. Thus the client doesn't know with certainty which features will be enabled when making the capture request. With the first and each subsequent delegate callback, the client is provided an AVCaptureResolvedPhotoSettings instance that indicates the settings that were applied to the capture. All "Auto" features have now been resolved to on or off. The AVCaptureResolvedPhotoSettings object passed in the client's delegate callbacks has a uniqueID identical to the AVCapturePhotoSettings request. This uniqueID allows clients to pair unresolved and resolved settings objects. See AVCapturePhotoCaptureDelegate below for a detailed discussion of the delegate callbacks.
--
-- Enabling certain photo features (Live Photo capture and high resolution capture) requires a reconfiguration of the capture render pipeline. Clients wishing to opt in for these features should call -setLivePhotoCaptureEnabled: and/or -setHighResolutionCaptureEnabled: before calling -startRunning on the AVCaptureSession. Changing any of these properties while the session is running requires a disruptive reconfiguration of the capture render pipeline. Live Photo captures in progress will be ended immediately; unfulfilled photo requests will be aborted; video preview will temporarily freeze. If you wish to capture Live Photos containing sound, you must add an audio AVCaptureDeviceInput to your AVCaptureSession.
--
-- Simultaneous Live Photo capture and MovieFileOutput capture is not supported. If an AVCaptureMovieFileOutput is added to your session, AVCapturePhotoOutput's livePhotoCaptureSupported property returns NO. Note that simultaneous Live Photo capture and AVCaptureVideoDataOutput is supported.
--
-- AVCaptureStillImageOutput and AVCapturePhotoOutput may not both be added to a capture session. You must use one or the other. If you add both to a session, a NSInvalidArgumentException is thrown.
--
-- AVCapturePhotoOutput implicitly supports wide color photo capture, following the activeColorSpace of the source AVCaptureDevice. If the source device's activeColorSpace is AVCaptureColorSpace_P3_D65, photos are encoded with wide color information, unless you've specified an output format of '420v', which does not support wide color.
--
-- Generated bindings for @AVCapturePhotoOutput@.
module ObjC.AVFoundation.AVCapturePhotoOutput
  ( AVCapturePhotoOutput
  , IsAVCapturePhotoOutput(..)
  , init_
  , new
  , capturePhotoWithSettings_delegate
  , setPreparedPhotoSettingsArray_completionHandler
  , isBayerRAWPixelFormat
  , isAppleProRAWPixelFormat
  , supportedPhotoPixelFormatTypesForFileType
  , supportedPhotoCodecTypesForFileType
  , supportedRawPhotoCodecTypesForRawPhotoPixelFormatType_fileType
  , supportedRawPhotoPixelFormatTypesForFileType
  , jpegPhotoDataRepresentationForJPEGSampleBuffer_previewPhotoSampleBuffer
  , dngPhotoDataRepresentationForRawSampleBuffer_previewPhotoSampleBuffer
  , preparedPhotoSettingsArray
  , availablePhotoPixelFormatTypes
  , availablePhotoCodecTypes
  , availableRawPhotoCodecTypes
  , appleProRAWSupported
  , appleProRAWEnabled
  , setAppleProRAWEnabled
  , availableRawPhotoPixelFormatTypes
  , availablePhotoFileTypes
  , availableRawPhotoFileTypes
  , maxPhotoQualityPrioritization
  , setMaxPhotoQualityPrioritization
  , fastCapturePrioritizationSupported
  , setFastCapturePrioritizationSupported
  , fastCapturePrioritizationEnabled
  , setFastCapturePrioritizationEnabled
  , autoDeferredPhotoDeliverySupported
  , autoDeferredPhotoDeliveryEnabled
  , setAutoDeferredPhotoDeliveryEnabled
  , stillImageStabilizationSupported
  , isStillImageStabilizationScene
  , virtualDeviceFusionSupported
  , dualCameraFusionSupported
  , virtualDeviceConstituentPhotoDeliverySupported
  , dualCameraDualPhotoDeliverySupported
  , virtualDeviceConstituentPhotoDeliveryEnabled
  , setVirtualDeviceConstituentPhotoDeliveryEnabled
  , dualCameraDualPhotoDeliveryEnabled
  , setDualCameraDualPhotoDeliveryEnabled
  , cameraCalibrationDataDeliverySupported
  , supportedFlashModes
  , autoRedEyeReductionSupported
  , isFlashScene
  , photoSettingsForSceneMonitoring
  , setPhotoSettingsForSceneMonitoring
  , highResolutionCaptureEnabled
  , setHighResolutionCaptureEnabled
  , maxBracketedCapturePhotoCount
  , lensStabilizationDuringBracketedCaptureSupported
  , livePhotoCaptureSupported
  , livePhotoCaptureEnabled
  , setLivePhotoCaptureEnabled
  , livePhotoCaptureSuspended
  , setLivePhotoCaptureSuspended
  , preservesLivePhotoCaptureSuspendedOnSessionStop
  , setPreservesLivePhotoCaptureSuspendedOnSessionStop
  , livePhotoAutoTrimmingEnabled
  , setLivePhotoAutoTrimmingEnabled
  , availableLivePhotoVideoCodecTypes
  , contentAwareDistortionCorrectionSupported
  , contentAwareDistortionCorrectionEnabled
  , setContentAwareDistortionCorrectionEnabled
  , zeroShutterLagSupported
  , zeroShutterLagEnabled
  , setZeroShutterLagEnabled
  , responsiveCaptureSupported
  , responsiveCaptureEnabled
  , setResponsiveCaptureEnabled
  , captureReadiness
  , constantColorSupported
  , constantColorEnabled
  , setConstantColorEnabled
  , shutterSoundSuppressionSupported
  , cameraSensorOrientationCompensationSupported
  , cameraSensorOrientationCompensationEnabled
  , setCameraSensorOrientationCompensationEnabled
  , depthDataDeliverySupported
  , depthDataDeliveryEnabled
  , setDepthDataDeliveryEnabled
  , portraitEffectsMatteDeliverySupported
  , portraitEffectsMatteDeliveryEnabled
  , setPortraitEffectsMatteDeliveryEnabled
  , availableSemanticSegmentationMatteTypes
  , enabledSemanticSegmentationMatteTypes
  , setEnabledSemanticSegmentationMatteTypes
  , appleProRAWEnabledSelector
  , appleProRAWSupportedSelector
  , autoDeferredPhotoDeliveryEnabledSelector
  , autoDeferredPhotoDeliverySupportedSelector
  , autoRedEyeReductionSupportedSelector
  , availableLivePhotoVideoCodecTypesSelector
  , availablePhotoCodecTypesSelector
  , availablePhotoFileTypesSelector
  , availablePhotoPixelFormatTypesSelector
  , availableRawPhotoCodecTypesSelector
  , availableRawPhotoFileTypesSelector
  , availableRawPhotoPixelFormatTypesSelector
  , availableSemanticSegmentationMatteTypesSelector
  , cameraCalibrationDataDeliverySupportedSelector
  , cameraSensorOrientationCompensationEnabledSelector
  , cameraSensorOrientationCompensationSupportedSelector
  , capturePhotoWithSettings_delegateSelector
  , captureReadinessSelector
  , constantColorEnabledSelector
  , constantColorSupportedSelector
  , contentAwareDistortionCorrectionEnabledSelector
  , contentAwareDistortionCorrectionSupportedSelector
  , depthDataDeliveryEnabledSelector
  , depthDataDeliverySupportedSelector
  , dngPhotoDataRepresentationForRawSampleBuffer_previewPhotoSampleBufferSelector
  , dualCameraDualPhotoDeliveryEnabledSelector
  , dualCameraDualPhotoDeliverySupportedSelector
  , dualCameraFusionSupportedSelector
  , enabledSemanticSegmentationMatteTypesSelector
  , fastCapturePrioritizationEnabledSelector
  , fastCapturePrioritizationSupportedSelector
  , highResolutionCaptureEnabledSelector
  , initSelector
  , isAppleProRAWPixelFormatSelector
  , isBayerRAWPixelFormatSelector
  , isFlashSceneSelector
  , isStillImageStabilizationSceneSelector
  , jpegPhotoDataRepresentationForJPEGSampleBuffer_previewPhotoSampleBufferSelector
  , lensStabilizationDuringBracketedCaptureSupportedSelector
  , livePhotoAutoTrimmingEnabledSelector
  , livePhotoCaptureEnabledSelector
  , livePhotoCaptureSupportedSelector
  , livePhotoCaptureSuspendedSelector
  , maxBracketedCapturePhotoCountSelector
  , maxPhotoQualityPrioritizationSelector
  , newSelector
  , photoSettingsForSceneMonitoringSelector
  , portraitEffectsMatteDeliveryEnabledSelector
  , portraitEffectsMatteDeliverySupportedSelector
  , preparedPhotoSettingsArraySelector
  , preservesLivePhotoCaptureSuspendedOnSessionStopSelector
  , responsiveCaptureEnabledSelector
  , responsiveCaptureSupportedSelector
  , setAppleProRAWEnabledSelector
  , setAutoDeferredPhotoDeliveryEnabledSelector
  , setCameraSensorOrientationCompensationEnabledSelector
  , setConstantColorEnabledSelector
  , setContentAwareDistortionCorrectionEnabledSelector
  , setDepthDataDeliveryEnabledSelector
  , setDualCameraDualPhotoDeliveryEnabledSelector
  , setEnabledSemanticSegmentationMatteTypesSelector
  , setFastCapturePrioritizationEnabledSelector
  , setFastCapturePrioritizationSupportedSelector
  , setHighResolutionCaptureEnabledSelector
  , setLivePhotoAutoTrimmingEnabledSelector
  , setLivePhotoCaptureEnabledSelector
  , setLivePhotoCaptureSuspendedSelector
  , setMaxPhotoQualityPrioritizationSelector
  , setPhotoSettingsForSceneMonitoringSelector
  , setPortraitEffectsMatteDeliveryEnabledSelector
  , setPreparedPhotoSettingsArray_completionHandlerSelector
  , setPreservesLivePhotoCaptureSuspendedOnSessionStopSelector
  , setResponsiveCaptureEnabledSelector
  , setVirtualDeviceConstituentPhotoDeliveryEnabledSelector
  , setZeroShutterLagEnabledSelector
  , shutterSoundSuppressionSupportedSelector
  , stillImageStabilizationSupportedSelector
  , supportedFlashModesSelector
  , supportedPhotoCodecTypesForFileTypeSelector
  , supportedPhotoPixelFormatTypesForFileTypeSelector
  , supportedRawPhotoCodecTypesForRawPhotoPixelFormatType_fileTypeSelector
  , supportedRawPhotoPixelFormatTypesForFileTypeSelector
  , virtualDeviceConstituentPhotoDeliveryEnabledSelector
  , virtualDeviceConstituentPhotoDeliverySupportedSelector
  , virtualDeviceFusionSupportedSelector
  , zeroShutterLagEnabledSelector
  , zeroShutterLagSupportedSelector

  -- * Enum types
  , AVCapturePhotoOutputCaptureReadiness(AVCapturePhotoOutputCaptureReadiness)
  , pattern AVCapturePhotoOutputCaptureReadinessSessionNotRunning
  , pattern AVCapturePhotoOutputCaptureReadinessReady
  , pattern AVCapturePhotoOutputCaptureReadinessNotReadyMomentarily
  , pattern AVCapturePhotoOutputCaptureReadinessNotReadyWaitingForCapture
  , pattern AVCapturePhotoOutputCaptureReadinessNotReadyWaitingForProcessing
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

-- | @- init@
init_ :: IsAVCapturePhotoOutput avCapturePhotoOutput => avCapturePhotoOutput -> IO (Id AVCapturePhotoOutput)
init_ avCapturePhotoOutput =
  sendOwnedMessage avCapturePhotoOutput initSelector

-- | @+ new@
new :: IO (Id AVCapturePhotoOutput)
new  =
  do
    cls' <- getRequiredClass "AVCapturePhotoOutput"
    sendOwnedClassMessage cls' newSelector

-- | capturePhotoWithSettings:delegate:
--
-- Method for initiating a photo capture request with progress monitoring through the supplied delegate.
--
-- @settings@ — An AVCapturePhotoSettings object you have configured. May not be nil.
--
-- @delegate@ — An object conforming to the AVCapturePhotoCaptureDelegate protocol. This object's delegate methods are called back as the photo advances from capture to processing to finished delivery. May not be nil.
--
-- This method initiates a photo capture. The receiver copies your provided settings to prevent unintentional mutation. It is illegal to re-use settings. The receiver throws an NSInvalidArgumentException if your settings.uniqueID matches that of any previously used settings. This method is used to initiate all flavors of photo capture: single photo, RAW capture with or without a processed image (such as a JPEG), bracketed capture, and Live Photo.
--
-- Clients need not wait for a capture photo request to complete before issuing another request. This is true for single photo captures as well as Live Photos, where movie complements of adjacent photo captures are allowed to overlap.
--
-- This method validates your settings and enforces the following rules in order to ensure deterministic behavior. If any of these rules are violated, a NSInvalidArgumentException is thrown.    RAW rules:        See +isBayerRAWPixelFormat: and +isAppleProRAWPixelFormat: on the difference between Bayer RAW and Apple ProRAW pixel formats.        Common RAW rules:        - If rawPhotoPixelFormatType is non-zero, it must be present in the receiver's -availableRawPhotoPixelFormatTypes array.        - If rawPhotoPixelFormatType is non-zero, your delegate must respond to -captureOutput:didFinishProcessingRawPhotoSampleBuffer:previewPhotoSampleBuffer:resolvedSettings:bracketSettings:error:.        - If rawPhotoPixelFormatType is non-zero, highResolutionPhotoEnabled may be YES or NO, but the setting only applies to the processed image, if you've specified one.        - If rawPhotoPixelFormatType is non-zero, constantColorEnabled must be set to NO.        - If rawFileType is specified, it must be present in -availableRawPhotoFileTypes and must support the rawPhotoPixelFormatType specified using -supportedRawPhotoPixelFormatTypesForFileType:.        Bayer RAW rules (isBayerRAWPixelFormat: returns yes for rawPhotoPixelFormatType):        - photoQualityPrioritization must be set to AVCapturePhotoQualityPrioritizationSpeed (deprecated autoStillImageStabilizationEnabled must be set to NO).        - the videoZoomFactor of the source device and the videoScaleAndCropFactor of the photo output's video connection must both be 1.0. Ensure no zoom is applied before requesting a RAW capture, and don't change the zoom during RAW capture.        Apple ProRAW rules (isAppleProRAWPixelFormat: returns yes for rawPhotoPixelFormatType):        - livePhotoMovieFileURL must be nil in AVCapturePhotoSettings settings        - autoContentAwareDistortionCorrectionEnabled will automatically be disabled in AVCapturePhotoSettings        - autoRedEyeReductionEnabled will automatically be disabled in AVCapturePhotoSettings        - portraitEffectsMatteDeliveryEnabled will automatically be disabled in AVCapturePhotoSettings        - enabledSemanticSegmentationMatteTypes will automatically be cleared in AVCapturePhotoSettings    Processed Format rules:        - If format is non-nil, a kCVPixelBufferPixelFormatTypeKey or AVVideoCodecKey must be present. You cannot specify both.        - If format has a kCVPixelBufferPixelFormatTypeKey, its value must be present in the receiver's -availablePhotoPixelFormatTypes array.        - If format has an AVVideoCodecKey, its value must be present in the receiver's -availablePhotoCodecTypes array.        - If format is non-nil, your delegate must respond to -captureOutput:didFinishProcessingPhotoSampleBuffer:previewPhotoSampleBuffer:resolvedSettings:bracketSettings:error:.        - If processedFileType is specified, it must be present in -availablePhotoFileTypes and must support the format's specified kCVPixelBufferPixelFormatTypeKey (using -supportedPhotoPixelFormatTypesForFileType:) or AVVideoCodecKey (using -supportedPhotoCodecTypesForFileType:).        - The photoQualityPrioritization you specify may not be a greater number than the photo output's maxPhotoQualityPrioritization. You must set your AVCapturePhotoOutput maxPhotoQualityPrioritization up front.    Flash rules:        - The specified flashMode must be present in the receiver's -supportedFlashModes array.    Live Photo rules:        - The receiver's livePhotoCaptureEnabled must be YES if settings.livePhotoMovieURL is non-nil.        - If settings.livePhotoMovieURL is non-nil, the receiver's livePhotoCaptureSuspended property must be set to NO.        - If settings.livePhotoMovieURL is non-nil, it must be a file URL that's accessible to your app's sandbox.        - If settings.livePhotoMovieURL is non-nil, your delegate must respond to -captureOutput:didFinishProcessingLivePhotoToMovieFileAtURL:duration:photoDisplayTime:resolvedSettings:error:.    Bracketed capture rules:        - bracketedSettings.count must be <= the receiver's maxBracketedCapturePhotoCount property.        - For manual exposure brackets, ISO value must be within the source device activeFormat's minISO and maxISO values.        - For manual exposure brackets, exposureDuration value must be within the source device activeFormat's minExposureDuration and maxExposureDuration values.        - For auto exposure brackets, exposureTargetBias value must be within the source device's minExposureTargetBias and maxExposureTargetBias values.    Deferred Photo Delivery rules:     - If the receiver's autoDeferredPhotoDeliveryEnabled is YES, your delegate must respond to -captureOutput:didFinishCapturingDeferredPhotoProxy:error:.     - The maxPhotoDimensions setting for 24MP (5712, 4284), when supported, is only serviced as 24MP via deferred photo delivery.    Color space rules:        - Photo capture is not supported when AVCaptureDevice has selected AVCaptureColorSpace_AppleLog or AVCaptureColorSpace_AppleLog2 as color space.
--
-- ObjC selector: @- capturePhotoWithSettings:delegate:@
capturePhotoWithSettings_delegate :: (IsAVCapturePhotoOutput avCapturePhotoOutput, IsAVCapturePhotoSettings settings) => avCapturePhotoOutput -> settings -> RawId -> IO ()
capturePhotoWithSettings_delegate avCapturePhotoOutput settings delegate =
  sendMessage avCapturePhotoOutput capturePhotoWithSettings_delegateSelector (toAVCapturePhotoSettings settings) delegate

-- | setPreparedPhotoSettingsArray:completionHandler:
--
-- Method allowing the receiver to prepare resources in advance for future -capturePhotoWithSettings:delegate: requests.
--
-- @preparedPhotoSettingsArray@ — An array of AVCapturePhotoSettings instances indicating the types of capture for which the receiver should prepare resources.
--
-- @completionHandler@ — A completion block to be fired on a serial dispatch queue once the receiver has finished preparing. You may pass nil to indicate you do not wish to be called back when preparation is complete.
--
-- Some types of photo capture, such as bracketed captures and RAW captures, require the receiver to allocate additional buffers or prepare other resources. To prevent photo capture requests from executing slowly due to lazy resource allocation, you may call this method with an array of settings objects representative of the types of capture you will be performing (e.g., settings for a bracketed capture, RAW capture, and/or still image stabilization capture). You may call this method even before calling -[AVCaptureSession startRunning] in order to hint the receiver up front which features you'll be utilizing. Each time you call this method with an array of settings, the receiver evaluates what additional resources it needs to allocate, as well as existing resources that can be reclaimed, and calls back your completionHandler when it has finished preparing (and possibly reclaiming) needed resources. By default, the receiver prepares sufficient resources to capture photos with default settings, +[AVCapturePhotoSettings photoSettings]. If you wish to reclaim all possible resources, you may call this method with an empty array.
--
-- Preparation for photo capture is always optional. You may call -capturePhotoWithSettings:delegate: without first calling -setPreparedPhotoSettingsArray:completionHandler:, but be advised that some of your photo captures may execute slowly as additional resources are allocated just-in-time.
--
-- If you call this method while your AVCaptureSession is not running, your completionHandler does not fire immediately. It only fires once you've called -[AVCaptureSession startRunning], and the needed resources have actually been prepared. If you call -setPreparedPhotoSettingsArray:completionHandler: with an array of settings, and then call it a second time, your first prepare call's completionHandler fires immediately with prepared == NO.
--
-- Prepared settings persist across session starts/stops and committed configuration changes. This property participates in -[AVCaptureSession beginConfiguration] / -[AVCaptureSession commitConfiguration] deferred work behavior. That is, if you call -[AVCaptureSession beginConfiguration], change your session's input/output topology, and call this method, preparation is deferred until you call -[AVCaptureSession commitConfiguration], enabling you to atomically commit a new configuration as well as prepare to take photos in that new configuration.
--
-- ObjC selector: @- setPreparedPhotoSettingsArray:completionHandler:@
setPreparedPhotoSettingsArray_completionHandler :: (IsAVCapturePhotoOutput avCapturePhotoOutput, IsNSArray preparedPhotoSettingsArray) => avCapturePhotoOutput -> preparedPhotoSettingsArray -> Ptr () -> IO ()
setPreparedPhotoSettingsArray_completionHandler avCapturePhotoOutput preparedPhotoSettingsArray completionHandler =
  sendMessage avCapturePhotoOutput setPreparedPhotoSettingsArray_completionHandlerSelector (toNSArray preparedPhotoSettingsArray) completionHandler

-- | isBayerRAWPixelFormat:
--
-- Returns YES if the given pixel format is a Bayer RAW format.
--
-- May be used to distinguish Bayer RAW from Apple ProRAW pixel formats in -availableRawPhotoPixelFormatTypes once appleProRAWEnabled has been set to YES.
--
-- ObjC selector: @+ isBayerRAWPixelFormat:@
isBayerRAWPixelFormat :: CUInt -> IO Bool
isBayerRAWPixelFormat pixelFormat =
  do
    cls' <- getRequiredClass "AVCapturePhotoOutput"
    sendClassMessage cls' isBayerRAWPixelFormatSelector pixelFormat

-- | isAppleProRAWPixelFormat:
--
-- Returns YES if the given pixel format is an Apple ProRAW format.
--
-- May be used to distinguish Bayer RAW from Apple ProRAW pixel formats in -availableRawPhotoPixelFormatTypes once appleProRAWEnabled has been set to YES.
--
-- See appleProRAWEnabled for more information on Apple ProRAW.
--
-- ObjC selector: @+ isAppleProRAWPixelFormat:@
isAppleProRAWPixelFormat :: CUInt -> IO Bool
isAppleProRAWPixelFormat pixelFormat =
  do
    cls' <- getRequiredClass "AVCapturePhotoOutput"
    sendClassMessage cls' isAppleProRAWPixelFormatSelector pixelFormat

-- | supportedPhotoPixelFormatTypesForFileType:
--
-- An array of pixel format type values that are currently supported by the receiver for a particular file container.
--
-- @fileType@ — The AVFileType container type intended for storage of a photo.
--
-- Returns: An array of CVPixelBufferPixelFormatTypeKey values supported by the receiver for the file type in question.
--
-- If you wish to capture a photo for storage in a particular file container, such as TIFF, you must ensure that the photo pixel format type you request is valid for that file type. If no pixel format types are supported for a given fileType, an empty array is returned. If you've not yet added your receiver to an AVCaptureSession with a video source, no pixel format types are supported.
--
-- ObjC selector: @- supportedPhotoPixelFormatTypesForFileType:@
supportedPhotoPixelFormatTypesForFileType :: (IsAVCapturePhotoOutput avCapturePhotoOutput, IsNSString fileType) => avCapturePhotoOutput -> fileType -> IO (Id NSArray)
supportedPhotoPixelFormatTypesForFileType avCapturePhotoOutput fileType =
  sendMessage avCapturePhotoOutput supportedPhotoPixelFormatTypesForFileTypeSelector (toNSString fileType)

-- | supportedPhotoCodecTypesForFileType:
--
-- An array of AVVideoCodecKey values that are currently supported by the receiver for a particular file container.
--
-- @fileType@ — The AVFileType container type intended for storage of a photo.
--
-- Returns: An array of AVVideoCodecKey values supported by the receiver for the file type in question.
--
-- If you wish to capture a photo for storage in a particular file container, such as HEIF, you must ensure that the photo codec type you request is valid for that file type. If no codec types are supported for a given fileType, an empty array is returned. If you've not yet added your receiver to an AVCaptureSession with a video source, no codec types are supported.
--
-- ObjC selector: @- supportedPhotoCodecTypesForFileType:@
supportedPhotoCodecTypesForFileType :: (IsAVCapturePhotoOutput avCapturePhotoOutput, IsNSString fileType) => avCapturePhotoOutput -> fileType -> IO (Id NSArray)
supportedPhotoCodecTypesForFileType avCapturePhotoOutput fileType =
  sendMessage avCapturePhotoOutput supportedPhotoCodecTypesForFileTypeSelector (toNSString fileType)

-- | supportedRawPhotoCodecTypesForRawPhotoPixelFormatType:fileType:
--
-- An array of AVVideoCodecType values that are currently supported by the receiver for a particular file container and raw pixel format.
--
-- @pixelFormatType@ — A Bayer RAW or Apple ProRAW pixel format OSType (defined in CVPixelBuffer.h).
--
-- @fileType@ — The AVFileType container type intended for storage of a photo which can be retrieved from -availableRawPhotoFileTypes.
--
-- Returns: An array of AVVideoCodecType values supported by the receiver for the file type and and raw pixel format in question.
--
-- If you wish to capture a raw photo for storage using a Bayer RAW or Apple ProRAW pixel format and to be stored in a file container, such as DNG, you must ensure that the codec type you request is valid for that file and pixel format type. If no RAW codec types are supported for a given file type and/or pixel format type, an empty array is returned. If you have not yet added your receiver to an AVCaptureSession with a video source, an empty array is returned.
--
-- ObjC selector: @- supportedRawPhotoCodecTypesForRawPhotoPixelFormatType:fileType:@
supportedRawPhotoCodecTypesForRawPhotoPixelFormatType_fileType :: (IsAVCapturePhotoOutput avCapturePhotoOutput, IsNSString fileType) => avCapturePhotoOutput -> CUInt -> fileType -> IO (Id NSArray)
supportedRawPhotoCodecTypesForRawPhotoPixelFormatType_fileType avCapturePhotoOutput pixelFormatType fileType =
  sendMessage avCapturePhotoOutput supportedRawPhotoCodecTypesForRawPhotoPixelFormatType_fileTypeSelector pixelFormatType (toNSString fileType)

-- | supportedRawPhotoPixelFormatTypesForFileType:
--
-- An array of CVPixelBufferPixelFormatType values that are currently supported by the receiver for a particular file container.
--
-- @fileType@ — The AVFileType container type intended for storage of a photo.
--
-- Returns: An array of CVPixelBufferPixelFormatType values supported by the receiver for the file type in question.
--
-- If you wish to capture a photo for storage in a particular file container, such as DNG, you must ensure that the RAW pixel format type you request is valid for that file type. If no RAW pixel format types are supported for a given fileType, an empty array is returned. If you've not yet added your receiver to an AVCaptureSession with a video source, no pixel format types are supported.
--
-- ObjC selector: @- supportedRawPhotoPixelFormatTypesForFileType:@
supportedRawPhotoPixelFormatTypesForFileType :: (IsAVCapturePhotoOutput avCapturePhotoOutput, IsNSString fileType) => avCapturePhotoOutput -> fileType -> IO (Id NSArray)
supportedRawPhotoPixelFormatTypesForFileType avCapturePhotoOutput fileType =
  sendMessage avCapturePhotoOutput supportedRawPhotoPixelFormatTypesForFileTypeSelector (toNSString fileType)

-- | JPEGPhotoDataRepresentationForJPEGSampleBuffer:previewPhotoSampleBuffer:
--
-- A class method that writes a JPEG sample buffer to an NSData in the JPEG file format.
--
-- @JPEGSampleBuffer@ — A CMSampleBuffer containing JPEG compressed data.
--
-- @previewPhotoSampleBuffer@ — An optional CMSampleBuffer containing pixel buffer image data to be written as a thumbnail image.
--
-- Returns: An NSData containing bits in the JPEG file format. May return nil if the re-packaging process fails.
--
-- AVCapturePhotoOutput's depecrated -captureOutput:didFinishProcessingPhotoSampleBuffer:previewPhotoSampleBuffer:resolvedSettings:bracketSettings:error: callback delivers JPEG photos to clients as CMSampleBuffers. To re-package these buffers in a data format suitable for writing to a JPEG file, you may call this class method, optionally inserting your own metadata into the JPEG CMSampleBuffer first, and optionally passing a preview image to be written to the JPEG file format as a thumbnail image.
--
-- ObjC selector: @+ JPEGPhotoDataRepresentationForJPEGSampleBuffer:previewPhotoSampleBuffer:@
jpegPhotoDataRepresentationForJPEGSampleBuffer_previewPhotoSampleBuffer :: Ptr () -> Ptr () -> IO (Id NSData)
jpegPhotoDataRepresentationForJPEGSampleBuffer_previewPhotoSampleBuffer jpegSampleBuffer previewPhotoSampleBuffer =
  do
    cls' <- getRequiredClass "AVCapturePhotoOutput"
    sendClassMessage cls' jpegPhotoDataRepresentationForJPEGSampleBuffer_previewPhotoSampleBufferSelector jpegSampleBuffer previewPhotoSampleBuffer

-- | DNGPhotoDataRepresentationForRawSampleBuffer:previewPhotoSampleBuffer:
--
-- A class method that writes a RAW sample buffer to an NSData containing bits in the DNG file format.
--
-- @rawSampleBuffer@ — A CMSampleBuffer containing Bayer RAW data.
--
-- @previewPhotoSampleBuffer@ — An optional CMSampleBuffer containing pixel buffer image data to be written as a thumbnail image.
--
-- Returns: An NSData containing bits in the DNG file format. May return nil if the re-packaging process fails.
--
-- AVCapturePhotoOutput's deprecated -captureOutput:didFinishProcessingRawPhotoSampleBuffer:previewPhotoSampleBuffer:resolvedSettings:bracketSettings:error: callback delivers RAW photos to clients as CMSampleBuffers. To re-package these buffers in a data format suitable for writing to a DNG file, you may call this class method, optionally inserting your own metadata into the RAW CMSampleBuffer first, and optionally passing a preview image to be written to the DNG file format as a thumbnail image. Only RAW images from Apple built-in cameras are supported.
--
-- ObjC selector: @+ DNGPhotoDataRepresentationForRawSampleBuffer:previewPhotoSampleBuffer:@
dngPhotoDataRepresentationForRawSampleBuffer_previewPhotoSampleBuffer :: Ptr () -> Ptr () -> IO (Id NSData)
dngPhotoDataRepresentationForRawSampleBuffer_previewPhotoSampleBuffer rawSampleBuffer previewPhotoSampleBuffer =
  do
    cls' <- getRequiredClass "AVCapturePhotoOutput"
    sendClassMessage cls' dngPhotoDataRepresentationForRawSampleBuffer_previewPhotoSampleBufferSelector rawSampleBuffer previewPhotoSampleBuffer

-- | preparedPhotoSettingsArray
--
-- An array of AVCapturePhotoSettings instances for which the receiver is prepared to capture.
--
-- See also setPreparedPhotoSettingsArray:completionHandler:    Some types of photo capture, such as bracketed captures and RAW captures, require the receiver to allocate additional buffers or prepare other resources. To prevent photo capture requests from executing slowly due to lazy resource allocation, you may call -setPreparedPhotoSettingsArray:completionHandler: with an array of settings objects representative of the types of capture you will be performing (e.g., settings for a bracketed capture, RAW capture, and/or still image stabilization capture). By default, the receiver prepares sufficient resources to capture photos with default settings, +[AVCapturePhotoSettings photoSettings].
--
-- ObjC selector: @- preparedPhotoSettingsArray@
preparedPhotoSettingsArray :: IsAVCapturePhotoOutput avCapturePhotoOutput => avCapturePhotoOutput -> IO (Id NSArray)
preparedPhotoSettingsArray avCapturePhotoOutput =
  sendMessage avCapturePhotoOutput preparedPhotoSettingsArraySelector

-- | availablePhotoPixelFormatTypes
--
-- An array of kCVPixelBufferPixelFormatTypeKey values that are currently supported by the receiver.
--
-- If you wish to capture a photo in an uncompressed format, such as 420f, 420v, or BGRA, you must ensure that the format you want is present in the receiver's availablePhotoPixelFormatTypes array. If you've not yet added your receiver to an AVCaptureSession with a video source, no pixel format types are available. This property is key-value observable.
--
-- ObjC selector: @- availablePhotoPixelFormatTypes@
availablePhotoPixelFormatTypes :: IsAVCapturePhotoOutput avCapturePhotoOutput => avCapturePhotoOutput -> IO (Id NSArray)
availablePhotoPixelFormatTypes avCapturePhotoOutput =
  sendMessage avCapturePhotoOutput availablePhotoPixelFormatTypesSelector

-- | availablePhotoCodecTypes
--
-- An array of AVVideoCodecKey values that are currently supported by the receiver.
--
-- If you wish to capture a photo in a compressed format, such as JPEG, you must ensure that the format you want is present in the receiver's availablePhotoCodecTypes array. If you've not yet added your receiver to an AVCaptureSession with a video source, no codec types are available. This property is key-value observable.
--
-- ObjC selector: @- availablePhotoCodecTypes@
availablePhotoCodecTypes :: IsAVCapturePhotoOutput avCapturePhotoOutput => avCapturePhotoOutput -> IO (Id NSArray)
availablePhotoCodecTypes avCapturePhotoOutput =
  sendMessage avCapturePhotoOutput availablePhotoCodecTypesSelector

-- | availableRawPhotoCodecTypes
--
-- An array of available AVVideoCodecType values that may be used for the raw photo.
--
-- Not all codecs can be used for all rawPixelFormatType values and this call will show all of the possible codecs available. To check if a codec is available for a specific rawPixelFormatType and rawFileType, one should use supportedRawPhotoCodecTypesForRawPhotoPixelFormatType:fileType:.
--
-- ObjC selector: @- availableRawPhotoCodecTypes@
availableRawPhotoCodecTypes :: IsAVCapturePhotoOutput avCapturePhotoOutput => avCapturePhotoOutput -> IO (Id NSArray)
availableRawPhotoCodecTypes avCapturePhotoOutput =
  sendMessage avCapturePhotoOutput availableRawPhotoCodecTypesSelector

-- | appleProRAWSupported
--
-- Indicates whether the current configuration supports Apple ProRAW pixel formats.
--
-- The AVCapturePhotoSettings appleProRAWEnabled property may only be set to YES if this property returns YES. This property is key-value observable.
--
-- ObjC selector: @- appleProRAWSupported@
appleProRAWSupported :: IsAVCapturePhotoOutput avCapturePhotoOutput => avCapturePhotoOutput -> IO Bool
appleProRAWSupported avCapturePhotoOutput =
  sendMessage avCapturePhotoOutput appleProRAWSupportedSelector

-- | appleProRAWEnabled
--
-- Indicates whether the photo output is configured for delivery of Apple ProRAW pixel formats as well as Bayer RAW formats.
--
-- Setting this property to YES will enable support for taking photos in Apple ProRAW pixel formats. These formats will be added to -availableRawPhotoPixelFormatTypes after any existing Bayer RAW formats. Compared to photos taken with a Bayer RAW format, these photos will be demosaiced and partially processed. They are still scene-referred, and allow capturing RAW photos in modes where there is no traditional sensor/Bayer RAW available. Examples are any modes that rely on fusion of multiple captures. Use +isBayerRAWPixelFormat: to determine if a pixel format in -availableRawPhotoPixelFormatTypes is a Bayer RAW format, and +isAppleProRAWPixelFormat: to determine if it is an Apple ProRAW format. When writing an Apple ProRAW buffer to a DNG file, the resulting file is known as "Linear DNG". Apple ProRAW formats are not supported on all platforms and devices. This property may only be set to YES if appleProRAWSupported returns YES. This property is key-value observable.
--
-- Enabling this property requires a lengthy reconfiguration of the capture render pipeline, so you should set this property to YES before calling -[AVCaptureSession startRunning].
--
-- ObjC selector: @- appleProRAWEnabled@
appleProRAWEnabled :: IsAVCapturePhotoOutput avCapturePhotoOutput => avCapturePhotoOutput -> IO Bool
appleProRAWEnabled avCapturePhotoOutput =
  sendMessage avCapturePhotoOutput appleProRAWEnabledSelector

-- | appleProRAWEnabled
--
-- Indicates whether the photo output is configured for delivery of Apple ProRAW pixel formats as well as Bayer RAW formats.
--
-- Setting this property to YES will enable support for taking photos in Apple ProRAW pixel formats. These formats will be added to -availableRawPhotoPixelFormatTypes after any existing Bayer RAW formats. Compared to photos taken with a Bayer RAW format, these photos will be demosaiced and partially processed. They are still scene-referred, and allow capturing RAW photos in modes where there is no traditional sensor/Bayer RAW available. Examples are any modes that rely on fusion of multiple captures. Use +isBayerRAWPixelFormat: to determine if a pixel format in -availableRawPhotoPixelFormatTypes is a Bayer RAW format, and +isAppleProRAWPixelFormat: to determine if it is an Apple ProRAW format. When writing an Apple ProRAW buffer to a DNG file, the resulting file is known as "Linear DNG". Apple ProRAW formats are not supported on all platforms and devices. This property may only be set to YES if appleProRAWSupported returns YES. This property is key-value observable.
--
-- Enabling this property requires a lengthy reconfiguration of the capture render pipeline, so you should set this property to YES before calling -[AVCaptureSession startRunning].
--
-- ObjC selector: @- setAppleProRAWEnabled:@
setAppleProRAWEnabled :: IsAVCapturePhotoOutput avCapturePhotoOutput => avCapturePhotoOutput -> Bool -> IO ()
setAppleProRAWEnabled avCapturePhotoOutput value =
  sendMessage avCapturePhotoOutput setAppleProRAWEnabledSelector value

-- | availableRawPhotoPixelFormatTypes
--
-- An array of RAW CVPixelBufferPixelFormatTypeKey values that are currently supported by the receiver.
--
-- If you wish to capture a RAW photo, you must ensure that the RAW format you want is present in the receiver's availableRawPhotoPixelFormatTypes array. If you've not yet added your receiver to an AVCaptureSession with a video source, no RAW formats are available. See AVCapturePhotoOutput.appleProRAWEnabled on how to enable support for partially processed RAW formats. This property is key-value observable. RAW capture is not supported on all platforms.
--
-- ObjC selector: @- availableRawPhotoPixelFormatTypes@
availableRawPhotoPixelFormatTypes :: IsAVCapturePhotoOutput avCapturePhotoOutput => avCapturePhotoOutput -> IO (Id NSArray)
availableRawPhotoPixelFormatTypes avCapturePhotoOutput =
  sendMessage avCapturePhotoOutput availableRawPhotoPixelFormatTypesSelector

-- | availablePhotoFileTypes
--
-- An array of AVFileType values that are currently supported by the receiver.
--
-- If you wish to capture a photo that is formatted for a particular file container, such as HEIF or DICOM, you must ensure that the fileType you desire is present in the receiver's availablePhotoFileTypes array. If you've not yet added your receiver to an AVCaptureSession with a video source, no file types are available. This property is key-value observable.
--
-- ObjC selector: @- availablePhotoFileTypes@
availablePhotoFileTypes :: IsAVCapturePhotoOutput avCapturePhotoOutput => avCapturePhotoOutput -> IO (Id NSArray)
availablePhotoFileTypes avCapturePhotoOutput =
  sendMessage avCapturePhotoOutput availablePhotoFileTypesSelector

-- | availableRawPhotoFileTypes
--
-- An array of AVFileType values that are currently supported by the receiver for RAW capture.
--
-- If you wish to capture a RAW photo that is formatted for a particular file container, such as DNG, you must ensure that the fileType you desire is present in the receiver's availableRawPhotoFileTypes array. If you've not yet added your receiver to an AVCaptureSession with a video source, no file types are available. This property is key-value observable.
--
-- ObjC selector: @- availableRawPhotoFileTypes@
availableRawPhotoFileTypes :: IsAVCapturePhotoOutput avCapturePhotoOutput => avCapturePhotoOutput -> IO (Id NSArray)
availableRawPhotoFileTypes avCapturePhotoOutput =
  sendMessage avCapturePhotoOutput availableRawPhotoFileTypesSelector

-- | maxPhotoQualityPrioritization
--
-- Indicates the highest quality the receiver should be prepared to output on a capture-by-capture basis.
--
-- Default value is AVCapturePhotoQualityPrioritizationBalanced when attached to an AVCaptureSession, and AVCapturePhotoQualityPrioritizationSpeed when attached to an AVCaptureMultiCamSession. The AVCapturePhotoOutput is capable of applying a variety of techniques to improve photo quality (reduce noise, preserve detail in low light, freeze motion, etc). Some techniques improve image quality at the expense of speed (shot-to-shot time). Before starting your session, you may set this property to indicate the highest quality prioritization you intend to request when calling -capturePhotoWithSettings:delegate:. When configuring an AVCapturePhotoSettings object, you may not exceed this quality prioritization level, but you may select a lower (speedier) prioritization level.
--
-- Changing the maxPhotoQualityPrioritization while the session is running can result in a lengthy rebuild of the session in which video preview is disrupted.
--
-- Setting the maxPhotoQualityPrioritization to .quality will turn on optical image stabilization if the -isHighPhotoQualitySupported of the source device's -activeFormat is true.
--
-- ObjC selector: @- maxPhotoQualityPrioritization@
maxPhotoQualityPrioritization :: IsAVCapturePhotoOutput avCapturePhotoOutput => avCapturePhotoOutput -> IO AVCapturePhotoQualityPrioritization
maxPhotoQualityPrioritization avCapturePhotoOutput =
  sendMessage avCapturePhotoOutput maxPhotoQualityPrioritizationSelector

-- | maxPhotoQualityPrioritization
--
-- Indicates the highest quality the receiver should be prepared to output on a capture-by-capture basis.
--
-- Default value is AVCapturePhotoQualityPrioritizationBalanced when attached to an AVCaptureSession, and AVCapturePhotoQualityPrioritizationSpeed when attached to an AVCaptureMultiCamSession. The AVCapturePhotoOutput is capable of applying a variety of techniques to improve photo quality (reduce noise, preserve detail in low light, freeze motion, etc). Some techniques improve image quality at the expense of speed (shot-to-shot time). Before starting your session, you may set this property to indicate the highest quality prioritization you intend to request when calling -capturePhotoWithSettings:delegate:. When configuring an AVCapturePhotoSettings object, you may not exceed this quality prioritization level, but you may select a lower (speedier) prioritization level.
--
-- Changing the maxPhotoQualityPrioritization while the session is running can result in a lengthy rebuild of the session in which video preview is disrupted.
--
-- Setting the maxPhotoQualityPrioritization to .quality will turn on optical image stabilization if the -isHighPhotoQualitySupported of the source device's -activeFormat is true.
--
-- ObjC selector: @- setMaxPhotoQualityPrioritization:@
setMaxPhotoQualityPrioritization :: IsAVCapturePhotoOutput avCapturePhotoOutput => avCapturePhotoOutput -> AVCapturePhotoQualityPrioritization -> IO ()
setMaxPhotoQualityPrioritization avCapturePhotoOutput value =
  sendMessage avCapturePhotoOutput setMaxPhotoQualityPrioritizationSelector value

-- | fastCapturePrioritizationSupported
--
-- Specifies whether fast capture prioritization is supported.
--
-- Fast capture prioritization allows capture quality to be automatically reduced from the selected AVCapturePhotoQualityPrioritization to ensure the photo output can keep up when captures are requested in rapid succession. Fast capture prioritization is only supported for certain AVCaptureSession sessionPresets and AVCaptureDevice activeFormats and only when responsiveCaptureEnabled is YES. When switching cameras or formats this property may change. When this property changes from YES to NO, fastCapturePrioritizationEnabled also reverts to NO. If you've previously opted in for fast capture prioritization and then change configurations, you may need to set fastCapturePrioritizationEnabled = YES again.
--
-- ObjC selector: @- fastCapturePrioritizationSupported@
fastCapturePrioritizationSupported :: IsAVCapturePhotoOutput avCapturePhotoOutput => avCapturePhotoOutput -> IO Bool
fastCapturePrioritizationSupported avCapturePhotoOutput =
  sendMessage avCapturePhotoOutput fastCapturePrioritizationSupportedSelector

-- | fastCapturePrioritizationSupported
--
-- Specifies whether fast capture prioritization is supported.
--
-- Fast capture prioritization allows capture quality to be automatically reduced from the selected AVCapturePhotoQualityPrioritization to ensure the photo output can keep up when captures are requested in rapid succession. Fast capture prioritization is only supported for certain AVCaptureSession sessionPresets and AVCaptureDevice activeFormats and only when responsiveCaptureEnabled is YES. When switching cameras or formats this property may change. When this property changes from YES to NO, fastCapturePrioritizationEnabled also reverts to NO. If you've previously opted in for fast capture prioritization and then change configurations, you may need to set fastCapturePrioritizationEnabled = YES again.
--
-- ObjC selector: @- setFastCapturePrioritizationSupported:@
setFastCapturePrioritizationSupported :: IsAVCapturePhotoOutput avCapturePhotoOutput => avCapturePhotoOutput -> Bool -> IO ()
setFastCapturePrioritizationSupported avCapturePhotoOutput value =
  sendMessage avCapturePhotoOutput setFastCapturePrioritizationSupportedSelector value

-- | fastCapturePrioritizationEnabled
--
-- Specifies whether fast capture prioritization is enabled.
--
-- This property defaults to NO. This property may only be set to YES if fastCapturePrioritizationSupported is YES, otherwise an NSInvalidArgumentException is thrown. By setting this property to YES, the photo output prepares itself to automatically reduce capture quality from the selected AVCapturePhotoQualityPrioritization when needed to keep up with rapid capture requests. In many cases the slightly reduced quality is preferable to missing the moment entirely. If you intend to use fast capture prioritization, you should set this property to YES before calling -[AVCaptureSession startRunning] or within -[AVCaptureSession beginConfiguration] and -[AVCaptureSession commitConfiguration] while running.
--
-- ObjC selector: @- fastCapturePrioritizationEnabled@
fastCapturePrioritizationEnabled :: IsAVCapturePhotoOutput avCapturePhotoOutput => avCapturePhotoOutput -> IO Bool
fastCapturePrioritizationEnabled avCapturePhotoOutput =
  sendMessage avCapturePhotoOutput fastCapturePrioritizationEnabledSelector

-- | fastCapturePrioritizationEnabled
--
-- Specifies whether fast capture prioritization is enabled.
--
-- This property defaults to NO. This property may only be set to YES if fastCapturePrioritizationSupported is YES, otherwise an NSInvalidArgumentException is thrown. By setting this property to YES, the photo output prepares itself to automatically reduce capture quality from the selected AVCapturePhotoQualityPrioritization when needed to keep up with rapid capture requests. In many cases the slightly reduced quality is preferable to missing the moment entirely. If you intend to use fast capture prioritization, you should set this property to YES before calling -[AVCaptureSession startRunning] or within -[AVCaptureSession beginConfiguration] and -[AVCaptureSession commitConfiguration] while running.
--
-- ObjC selector: @- setFastCapturePrioritizationEnabled:@
setFastCapturePrioritizationEnabled :: IsAVCapturePhotoOutput avCapturePhotoOutput => avCapturePhotoOutput -> Bool -> IO ()
setFastCapturePrioritizationEnabled avCapturePhotoOutput value =
  sendMessage avCapturePhotoOutput setFastCapturePrioritizationEnabledSelector value

-- | autoDeferredPhotoDeliverySupported
--
-- Indicates whether the deferred photo delivery feature is supported by the receiver.
--
-- This property may change as the session's -sessionPreset or source device's -activeFormat change. When deferred photo delivery is not supported, your capture requests always resolve their AVCaptureResolvedPhotoSettings.deferredPhotoProxyDimensions to { 0, 0 }. This property is key-value observable.
--
-- Automatic deferred photo delivery can produce a lightweight photo representation, called a "proxy", at the time of capture that can later be processed to completion while improving camera responsiveness.  When it's appropriate for the receiver to deliver a photo proxy for deferred processing, the delegate callback -captureOutput:didFinishCapturingDeferredPhotoProxy:error: will be invoked instead of -captureOutput:didFinishProcessingPhoto:error:.  See the documentation for AVCaptureDeferredPhotoProxy for more details.
--
-- ObjC selector: @- autoDeferredPhotoDeliverySupported@
autoDeferredPhotoDeliverySupported :: IsAVCapturePhotoOutput avCapturePhotoOutput => avCapturePhotoOutput -> IO Bool
autoDeferredPhotoDeliverySupported avCapturePhotoOutput =
  sendMessage avCapturePhotoOutput autoDeferredPhotoDeliverySupportedSelector

-- | autoDeferredPhotoDeliveryEnabled
--
-- Specifies whether automatic deferred photo delivery is enabled.
--
-- Setting this value to either YES or NO requires a lengthy reconfiguration of the capture pipeline, so you should set this property before calling -[AVCaptureSession startRunning].  Setting this property to YES throws an NSInvalidArgumentException if autoDeferredPhotoDeliverySupported is NO.
--
-- ObjC selector: @- autoDeferredPhotoDeliveryEnabled@
autoDeferredPhotoDeliveryEnabled :: IsAVCapturePhotoOutput avCapturePhotoOutput => avCapturePhotoOutput -> IO Bool
autoDeferredPhotoDeliveryEnabled avCapturePhotoOutput =
  sendMessage avCapturePhotoOutput autoDeferredPhotoDeliveryEnabledSelector

-- | autoDeferredPhotoDeliveryEnabled
--
-- Specifies whether automatic deferred photo delivery is enabled.
--
-- Setting this value to either YES or NO requires a lengthy reconfiguration of the capture pipeline, so you should set this property before calling -[AVCaptureSession startRunning].  Setting this property to YES throws an NSInvalidArgumentException if autoDeferredPhotoDeliverySupported is NO.
--
-- ObjC selector: @- setAutoDeferredPhotoDeliveryEnabled:@
setAutoDeferredPhotoDeliveryEnabled :: IsAVCapturePhotoOutput avCapturePhotoOutput => avCapturePhotoOutput -> Bool -> IO ()
setAutoDeferredPhotoDeliveryEnabled avCapturePhotoOutput value =
  sendMessage avCapturePhotoOutput setAutoDeferredPhotoDeliveryEnabledSelector value

-- | stillImageStabilizationSupported
--
-- Indicates whether the still image stabilization feature is supported by the receiver.
--
-- This property may change as the session's -sessionPreset or source device's -activeFormat change. When still image stabilization is not supported, your capture requests always resolve stillImageStabilizationEnabled to NO. This property is key-value observable.
--
-- As of iOS 13 hardware, the AVCapturePhotoOutput is capable of applying a variety of multi-image fusion techniques to improve photo quality (reduce noise, preserve detail in low light, freeze motion, etc), all of which have been previously lumped under the stillImageStabilization moniker. This property should no longer be used as it no longer provides meaningful information about the techniques used to improve quality in a photo capture. Instead, you should use -maxPhotoQualityPrioritization to indicate the highest quality prioritization level you might request in a photo capture, understanding that the higher the quality, the longer the potential wait. You may also use AVCapturePhotoSettings' photoQualityPrioritization property to specify a prioritization level for a particular photo capture, and then query the AVCaptureResolvedPhotoSettings photoProcessingTimeRange property to find out how long it might take to receive the resulting photo in your delegate callback.
--
-- ObjC selector: @- stillImageStabilizationSupported@
stillImageStabilizationSupported :: IsAVCapturePhotoOutput avCapturePhotoOutput => avCapturePhotoOutput -> IO Bool
stillImageStabilizationSupported avCapturePhotoOutput =
  sendMessage avCapturePhotoOutput stillImageStabilizationSupportedSelector

-- | isStillImageStabilizationScene
--
-- Indicates whether the current scene is dark enough to warrant use of still image stabilization.
--
-- This property reports whether the current scene being previewed by the camera is dark enough to benefit from still image stabilization. You can influence this property's answers by setting the photoSettingsForSceneMonitoring property, indicating whether autoStillImageStabilization monitoring should be on or off. If you set autoStillImageStabilization to NO, isStillImageStabilizationScene always reports NO. If you set it to YES, this property returns YES or NO depending on the current scene's lighting conditions. Note that some very dark scenes do not benefit from still image stabilization, but do benefit from flash. By default, this property always returns NO unless you set photoSettingsForSceneMonitoring to a non-nil value. This property may be key-value observed.
--
-- As of iOS 13 hardware, the AVCapturePhotoOutput is capable of applying a variety of multi-image fusion techniques to improve photo quality (reduce noise, preserve detail in low light, freeze motion, etc), all of which have been previously lumped under the stillImageStabilization moniker. This property should no longer be used as it no longer provides meaningful information about the techniques used to improve quality in a photo capture. Instead, you should use -maxPhotoQualityPrioritization to indicate the highest quality prioritization level you might request in a photo capture, understanding that the higher the quality, the longer the potential wait. You may also use AVCapturePhotoSettings' photoQualityPrioritization property to specify a prioritization level for a particular photo capture, and then query the AVCaptureResolvedPhotoSettings photoProcessingTimeRange property to find out how long it might take to receive the resulting photo in your delegate callback.
--
-- ObjC selector: @- isStillImageStabilizationScene@
isStillImageStabilizationScene :: IsAVCapturePhotoOutput avCapturePhotoOutput => avCapturePhotoOutput -> IO Bool
isStillImageStabilizationScene avCapturePhotoOutput =
  sendMessage avCapturePhotoOutput isStillImageStabilizationSceneSelector

-- | virtualDeviceFusionSupported
--
-- Indicates whether the virtual device image fusion feature is supported by the receiver.
--
-- This property may change as the session's -sessionPreset or source device's -activeFormat change. When using a virtual AVCaptureDevice, its constituent camera images can be fused together to improve image quality when this property answers YES. When virtual device fusion is not supported by the current configuration, your capture requests always resolve virtualDeviceFusionEnabled to NO. This property is key-value observable.
--
-- ObjC selector: @- virtualDeviceFusionSupported@
virtualDeviceFusionSupported :: IsAVCapturePhotoOutput avCapturePhotoOutput => avCapturePhotoOutput -> IO Bool
virtualDeviceFusionSupported avCapturePhotoOutput =
  sendMessage avCapturePhotoOutput virtualDeviceFusionSupportedSelector

-- | dualCameraFusionSupported
--
-- Indicates whether the DualCamera image fusion feature is supported by the receiver.
--
-- This property may change as the session's -sessionPreset or source device's -activeFormat change. When using the AVCaptureDevice with deviceType AVCaptureDeviceTypeBuiltInDualCamera, the wide-angle and telephoto camera images can be fused together to improve image quality in some configurations. When DualCamera image fusion is not supported by the current configuration, your capture requests always resolve dualCameraFusionEnabled to NO. This property is key-value observable. As of iOS 13, this property is deprecated in favor of virtualDeviceFusionSupported.
--
-- ObjC selector: @- dualCameraFusionSupported@
dualCameraFusionSupported :: IsAVCapturePhotoOutput avCapturePhotoOutput => avCapturePhotoOutput -> IO Bool
dualCameraFusionSupported avCapturePhotoOutput =
  sendMessage avCapturePhotoOutput dualCameraFusionSupportedSelector

-- | virtualDeviceConstituentPhotoDeliverySupported
--
-- Specifies whether the photo output's current configuration supports delivery of photos from constituent cameras of a virtual device.
--
-- Virtual device constituent photo delivery is only supported for certain AVCaptureSession sessionPresets and AVCaptureDevice activeFormats. When switching cameras or formats this property may change. When this property changes from YES to NO, virtualDeviceConstituentPhotoDeliveryEnabled also reverts to NO. If you've previously opted in for virtual device constituent photo delivery and then change configurations, you may need to set virtualDeviceConstituentPhotoDeliveryEnabled = YES again. This property is key-value observable.
--
-- ObjC selector: @- virtualDeviceConstituentPhotoDeliverySupported@
virtualDeviceConstituentPhotoDeliverySupported :: IsAVCapturePhotoOutput avCapturePhotoOutput => avCapturePhotoOutput -> IO Bool
virtualDeviceConstituentPhotoDeliverySupported avCapturePhotoOutput =
  sendMessage avCapturePhotoOutput virtualDeviceConstituentPhotoDeliverySupportedSelector

-- | dualCameraDualPhotoDeliverySupported
--
-- Specifies whether the photo output's current configuration supports delivery of both telephoto and wide images from the DualCamera.
--
-- DualCamera dual photo delivery is only supported for certain AVCaptureSession sessionPresets and AVCaptureDevice activeFormats. When switching cameras or formats this property may change. When this property changes from YES to NO, dualCameraDualPhotoDeliveryEnabled also reverts to NO. If you've previously opted in for DualCamera dual photo delivery and then change configurations, you may need to set dualCameraDualPhotoDeliveryEnabled = YES again. This property is key-value observable. As of iOS 13, this property is deprecated in favor of virtualDeviceConstituentPhotoDeliverySupported.
--
-- ObjC selector: @- dualCameraDualPhotoDeliverySupported@
dualCameraDualPhotoDeliverySupported :: IsAVCapturePhotoOutput avCapturePhotoOutput => avCapturePhotoOutput -> IO Bool
dualCameraDualPhotoDeliverySupported avCapturePhotoOutput =
  sendMessage avCapturePhotoOutput dualCameraDualPhotoDeliverySupportedSelector

-- | virtualDeviceConstituentPhotoDeliveryEnabled
--
-- Indicates whether the photo output is configured for delivery of photos from constituent cameras of a virtual device.
--
-- Default value is NO. This property may only be set to YES if virtualDeviceConstituentPhotoDeliverySupported is YES. Virtual device constituent photo delivery requires a lengthy reconfiguration of the capture render pipeline, so if you intend to do any constituent photo delivery captures, you should set this property to YES before calling -[AVCaptureSession startRunning]. See also -[AVCapturePhotoSettings virtualDeviceConstituentPhotoDeliveryEnabledDevices].
--
-- ObjC selector: @- virtualDeviceConstituentPhotoDeliveryEnabled@
virtualDeviceConstituentPhotoDeliveryEnabled :: IsAVCapturePhotoOutput avCapturePhotoOutput => avCapturePhotoOutput -> IO Bool
virtualDeviceConstituentPhotoDeliveryEnabled avCapturePhotoOutput =
  sendMessage avCapturePhotoOutput virtualDeviceConstituentPhotoDeliveryEnabledSelector

-- | virtualDeviceConstituentPhotoDeliveryEnabled
--
-- Indicates whether the photo output is configured for delivery of photos from constituent cameras of a virtual device.
--
-- Default value is NO. This property may only be set to YES if virtualDeviceConstituentPhotoDeliverySupported is YES. Virtual device constituent photo delivery requires a lengthy reconfiguration of the capture render pipeline, so if you intend to do any constituent photo delivery captures, you should set this property to YES before calling -[AVCaptureSession startRunning]. See also -[AVCapturePhotoSettings virtualDeviceConstituentPhotoDeliveryEnabledDevices].
--
-- ObjC selector: @- setVirtualDeviceConstituentPhotoDeliveryEnabled:@
setVirtualDeviceConstituentPhotoDeliveryEnabled :: IsAVCapturePhotoOutput avCapturePhotoOutput => avCapturePhotoOutput -> Bool -> IO ()
setVirtualDeviceConstituentPhotoDeliveryEnabled avCapturePhotoOutput value =
  sendMessage avCapturePhotoOutput setVirtualDeviceConstituentPhotoDeliveryEnabledSelector value

-- | dualCameraDualPhotoDeliveryEnabled
--
-- Indicates whether the photo output is configured for delivery of both the telephoto and wide images from the DualCamera.
--
-- Default value is NO. This property may only be set to YES if dualCameraDualPhotoDeliverySupported is YES. DualCamera dual photo delivery requires a lengthy reconfiguration of the capture render pipeline, so if you intend to do any dual photo delivery captures, you should set this property to YES before calling -[AVCaptureSession startRunning]. See also -[AVCapturePhotoSettings dualCameraDualPhotoDeliveryEnabled]. As of iOS 13, this property is deprecated in favor of virtualDeviceConstituentPhotoDeliveryEnabled.
--
-- ObjC selector: @- dualCameraDualPhotoDeliveryEnabled@
dualCameraDualPhotoDeliveryEnabled :: IsAVCapturePhotoOutput avCapturePhotoOutput => avCapturePhotoOutput -> IO Bool
dualCameraDualPhotoDeliveryEnabled avCapturePhotoOutput =
  sendMessage avCapturePhotoOutput dualCameraDualPhotoDeliveryEnabledSelector

-- | dualCameraDualPhotoDeliveryEnabled
--
-- Indicates whether the photo output is configured for delivery of both the telephoto and wide images from the DualCamera.
--
-- Default value is NO. This property may only be set to YES if dualCameraDualPhotoDeliverySupported is YES. DualCamera dual photo delivery requires a lengthy reconfiguration of the capture render pipeline, so if you intend to do any dual photo delivery captures, you should set this property to YES before calling -[AVCaptureSession startRunning]. See also -[AVCapturePhotoSettings dualCameraDualPhotoDeliveryEnabled]. As of iOS 13, this property is deprecated in favor of virtualDeviceConstituentPhotoDeliveryEnabled.
--
-- ObjC selector: @- setDualCameraDualPhotoDeliveryEnabled:@
setDualCameraDualPhotoDeliveryEnabled :: IsAVCapturePhotoOutput avCapturePhotoOutput => avCapturePhotoOutput -> Bool -> IO ()
setDualCameraDualPhotoDeliveryEnabled avCapturePhotoOutput value =
  sendMessage avCapturePhotoOutput setDualCameraDualPhotoDeliveryEnabledSelector value

-- | cameraCalibrationDataDeliverySupported
--
-- Specifies whether the photo output's current configuration supports delivery of AVCameraCalibrationData in the resultant AVCapturePhoto.
--
-- Camera calibration data delivery (intrinsics, extrinsics, lens distortion characteristics, etc.) is only supported if virtualDeviceConstituentPhotoDeliveryEnabled is YES and contentAwareDistortionCorrectionEnabled is NO and the source device's geometricDistortionCorrectionEnabled property is set to NO. This property is key-value observable.
--
-- ObjC selector: @- cameraCalibrationDataDeliverySupported@
cameraCalibrationDataDeliverySupported :: IsAVCapturePhotoOutput avCapturePhotoOutput => avCapturePhotoOutput -> IO Bool
cameraCalibrationDataDeliverySupported avCapturePhotoOutput =
  sendMessage avCapturePhotoOutput cameraCalibrationDataDeliverySupportedSelector

-- | supportedFlashModes
--
-- An array of AVCaptureFlashMode constants for the current capture session configuration.
--
-- This property supersedes AVCaptureDevice's isFlashModeSupported: It returns an array of AVCaptureFlashMode constants. To test whether a particular flash mode is supported, use NSArray's containsObject API: [photoOutput.supportedFlashModes containsObject:\@(AVCaptureFlashModeAuto)]. This property is key-value observable.
--
-- ObjC selector: @- supportedFlashModes@
supportedFlashModes :: IsAVCapturePhotoOutput avCapturePhotoOutput => avCapturePhotoOutput -> IO (Id NSArray)
supportedFlashModes avCapturePhotoOutput =
  sendMessage avCapturePhotoOutput supportedFlashModesSelector

-- | autoRedEyeReductionSupported
--
-- Indicates whether the receiver supports automatic red-eye reduction for flash captures.
--
-- Flash images may cause subjects' eyes to appear red, golden, or white. Automatic red-eye reduction detects and corrects for reflected light in eyes, at the cost of additional processing time per image. This property may change as the session's -sessionPreset or source device's -activeFormat change. When red-eye reduction is not supported, your capture requests always resolve redEyeReductionEnabled to NO. This property is key-value observable.
--
-- ObjC selector: @- autoRedEyeReductionSupported@
autoRedEyeReductionSupported :: IsAVCapturePhotoOutput avCapturePhotoOutput => avCapturePhotoOutput -> IO Bool
autoRedEyeReductionSupported avCapturePhotoOutput =
  sendMessage avCapturePhotoOutput autoRedEyeReductionSupportedSelector

-- | isFlashScene
--
-- Indicates whether the current scene is dark enough to warrant use of the flash.
--
-- This property reports whether the current scene being previewed by the camera is dark enough to need the flash. If -supportedFlashModes only contains AVCaptureFlashModeOff, isFlashScene always reports NO. You can influence this property's answers by setting the photoSettingsForSceneMonitoring property, indicating the flashMode you wish to monitor. If you set flashMode to AVCaptureFlashModeOff, isFlashScene always reports NO. If you set it to AVCaptureFlashModeAuto or AVCaptureFlashModeOn, isFlashScene answers YES or NO based on the current scene's lighting conditions. By default, this property always returns NO unless you set photoSettingsForSceneMonitoring to a non-nil value. Note that there is some overlap in the light level ranges that benefit from still image stabilization and flash. If your photoSettingsForSceneMonitoring indicate that both still image stabilization and flash scenes should be monitored, still image stabilization takes precedence, and isFlashScene becomes YES at lower overall light levels. This property may be key-value observed.
--
-- ObjC selector: @- isFlashScene@
isFlashScene :: IsAVCapturePhotoOutput avCapturePhotoOutput => avCapturePhotoOutput -> IO Bool
isFlashScene avCapturePhotoOutput =
  sendMessage avCapturePhotoOutput isFlashSceneSelector

-- | photoSettingsForSceneMonitoring
--
-- Settings that govern the behavior of isFlashScene and isStillImageStabilizationScene.
--
-- You can influence the return values of isFlashScene and isStillImageStabilizationScene by setting this property, indicating the flashMode and photoQualityPrioritization values that should be considered for scene monitoring. For instance, if you set flashMode to AVCaptureFlashModeOff, isFlashScene always reports NO. If you set it to AVCaptureFlashModeAuto or AVCaptureFlashModeOn, isFlashScene answers YES or NO based on the current scene's lighting conditions. Note that there is some overlap in the light level ranges that benefit from still image stabilization and flash. If your photoSettingsForSceneMonitoring indicate that both still image stabilization and flash scenes should be monitored, still image stabilization takes precedence, and isFlashScene becomes YES at lower overall light levels. The default value for this property is nil. See isStillImageStabilizationScene and isFlashScene for further discussion.
--
-- ObjC selector: @- photoSettingsForSceneMonitoring@
photoSettingsForSceneMonitoring :: IsAVCapturePhotoOutput avCapturePhotoOutput => avCapturePhotoOutput -> IO (Id AVCapturePhotoSettings)
photoSettingsForSceneMonitoring avCapturePhotoOutput =
  sendMessage avCapturePhotoOutput photoSettingsForSceneMonitoringSelector

-- | photoSettingsForSceneMonitoring
--
-- Settings that govern the behavior of isFlashScene and isStillImageStabilizationScene.
--
-- You can influence the return values of isFlashScene and isStillImageStabilizationScene by setting this property, indicating the flashMode and photoQualityPrioritization values that should be considered for scene monitoring. For instance, if you set flashMode to AVCaptureFlashModeOff, isFlashScene always reports NO. If you set it to AVCaptureFlashModeAuto or AVCaptureFlashModeOn, isFlashScene answers YES or NO based on the current scene's lighting conditions. Note that there is some overlap in the light level ranges that benefit from still image stabilization and flash. If your photoSettingsForSceneMonitoring indicate that both still image stabilization and flash scenes should be monitored, still image stabilization takes precedence, and isFlashScene becomes YES at lower overall light levels. The default value for this property is nil. See isStillImageStabilizationScene and isFlashScene for further discussion.
--
-- ObjC selector: @- setPhotoSettingsForSceneMonitoring:@
setPhotoSettingsForSceneMonitoring :: (IsAVCapturePhotoOutput avCapturePhotoOutput, IsAVCapturePhotoSettings value) => avCapturePhotoOutput -> value -> IO ()
setPhotoSettingsForSceneMonitoring avCapturePhotoOutput value =
  sendMessage avCapturePhotoOutput setPhotoSettingsForSceneMonitoringSelector (toAVCapturePhotoSettings value)

-- | highResolutionCaptureEnabled
--
-- Indicates whether the photo render pipeline should be configured to deliver high resolution still images.
--
-- Some AVCaptureDeviceFormats support outputting higher resolution stills than their streaming resolution (See AVCaptureDeviceFormat.highResolutionStillImageDimensions). Under some conditions, AVCaptureSession needs to set up the photo render pipeline differently to support high resolution still image capture. If you intend to take high resolution still images at all, you should set this property to YES before calling -[AVCaptureSession startRunning]. Once you've opted in for high resolution capture, you are free to issue photo capture requests with or without highResolutionCaptureEnabled in the AVCapturePhotoSettings. If you have not set this property to YES and call capturePhotoWithSettings:delegate: with settings.highResolutionCaptureEnabled set to YES, an NSInvalidArgumentException will be thrown.
--
-- ObjC selector: @- highResolutionCaptureEnabled@
highResolutionCaptureEnabled :: IsAVCapturePhotoOutput avCapturePhotoOutput => avCapturePhotoOutput -> IO Bool
highResolutionCaptureEnabled avCapturePhotoOutput =
  sendMessage avCapturePhotoOutput highResolutionCaptureEnabledSelector

-- | highResolutionCaptureEnabled
--
-- Indicates whether the photo render pipeline should be configured to deliver high resolution still images.
--
-- Some AVCaptureDeviceFormats support outputting higher resolution stills than their streaming resolution (See AVCaptureDeviceFormat.highResolutionStillImageDimensions). Under some conditions, AVCaptureSession needs to set up the photo render pipeline differently to support high resolution still image capture. If you intend to take high resolution still images at all, you should set this property to YES before calling -[AVCaptureSession startRunning]. Once you've opted in for high resolution capture, you are free to issue photo capture requests with or without highResolutionCaptureEnabled in the AVCapturePhotoSettings. If you have not set this property to YES and call capturePhotoWithSettings:delegate: with settings.highResolutionCaptureEnabled set to YES, an NSInvalidArgumentException will be thrown.
--
-- ObjC selector: @- setHighResolutionCaptureEnabled:@
setHighResolutionCaptureEnabled :: IsAVCapturePhotoOutput avCapturePhotoOutput => avCapturePhotoOutput -> Bool -> IO ()
setHighResolutionCaptureEnabled avCapturePhotoOutput value =
  sendMessage avCapturePhotoOutput setHighResolutionCaptureEnabledSelector value

-- | maxBracketedCapturePhotoCount
--
-- Specifies the maximum number of photos that may be taken in a single bracket.
--
-- AVCapturePhotoOutput can only satisfy a limited number of image requests in a single bracket without exhausting system resources. The maximum number of photos that may be taken in a single bracket depends on the size and format of the images being captured, and consequently may vary with AVCaptureSession -sessionPreset and AVCaptureDevice -activeFormat. Some formats do not support bracketed capture at all, and thus this property may return a value of 0. This read-only property is key-value observable. If you call -capturePhotoWithSettings:delegate: with a bracketedSettings whose count exceeds -maxBracketedCapturePhotoCount, an NSInvalidArgumentException is thrown.
--
-- ObjC selector: @- maxBracketedCapturePhotoCount@
maxBracketedCapturePhotoCount :: IsAVCapturePhotoOutput avCapturePhotoOutput => avCapturePhotoOutput -> IO CULong
maxBracketedCapturePhotoCount avCapturePhotoOutput =
  sendMessage avCapturePhotoOutput maxBracketedCapturePhotoCountSelector

-- | lensStabilizationDuringBracketedCaptureSupported
--
-- Indicates whether the receiver supports lens stabilization during bracketed captures.
--
-- The AVCapturePhotoBracketSettings lensStabilizationEnabled property may only be set if this property returns YES. Its value may change as the session's -sessionPreset or input device's -activeFormat changes. This read-only property is key-value observable.
--
-- ObjC selector: @- lensStabilizationDuringBracketedCaptureSupported@
lensStabilizationDuringBracketedCaptureSupported :: IsAVCapturePhotoOutput avCapturePhotoOutput => avCapturePhotoOutput -> IO Bool
lensStabilizationDuringBracketedCaptureSupported avCapturePhotoOutput =
  sendMessage avCapturePhotoOutput lensStabilizationDuringBracketedCaptureSupportedSelector

-- | livePhotoCaptureSupported
--
-- Indicates whether the receiver supports Live Photo capture.
--
-- Live Photo capture is only supported for certain AVCaptureSession sessionPresets and AVCaptureDevice activeFormats. When switching cameras or formats this property may change. When this property changes from YES to NO, livePhotoCaptureEnabled also reverts to NO. If you've previously opted in for Live Photo capture and then change configurations, you may need to set livePhotoCaptureEnabled = YES again.
--
-- ObjC selector: @- livePhotoCaptureSupported@
livePhotoCaptureSupported :: IsAVCapturePhotoOutput avCapturePhotoOutput => avCapturePhotoOutput -> IO Bool
livePhotoCaptureSupported avCapturePhotoOutput =
  sendMessage avCapturePhotoOutput livePhotoCaptureSupportedSelector

-- | livePhotoCaptureEnabled
--
-- Indicates whether the receiver is configured for Live Photo capture.
--
-- Default value is NO. This property may only be set to YES if livePhotoCaptureSupported is YES. Live Photo capture requires a lengthy reconfiguration of the capture render pipeline, so if you intend to do any Live Photo captures at all, you should set livePhotoCaptureEnabled to YES before calling -[AVCaptureSession startRunning].
--
-- ObjC selector: @- livePhotoCaptureEnabled@
livePhotoCaptureEnabled :: IsAVCapturePhotoOutput avCapturePhotoOutput => avCapturePhotoOutput -> IO Bool
livePhotoCaptureEnabled avCapturePhotoOutput =
  sendMessage avCapturePhotoOutput livePhotoCaptureEnabledSelector

-- | livePhotoCaptureEnabled
--
-- Indicates whether the receiver is configured for Live Photo capture.
--
-- Default value is NO. This property may only be set to YES if livePhotoCaptureSupported is YES. Live Photo capture requires a lengthy reconfiguration of the capture render pipeline, so if you intend to do any Live Photo captures at all, you should set livePhotoCaptureEnabled to YES before calling -[AVCaptureSession startRunning].
--
-- ObjC selector: @- setLivePhotoCaptureEnabled:@
setLivePhotoCaptureEnabled :: IsAVCapturePhotoOutput avCapturePhotoOutput => avCapturePhotoOutput -> Bool -> IO ()
setLivePhotoCaptureEnabled avCapturePhotoOutput value =
  sendMessage avCapturePhotoOutput setLivePhotoCaptureEnabledSelector value

-- | livePhotoCaptureSuspended
--
-- Indicates whether Live Photo capture is enabled, but currently suspended.
--
-- This property allows you to cut current Live Photo movie captures short (for instance, if you suddenly need to do something that you don't want to show up in the Live Photo movie, such as take a non Live Photo capture that makes a shutter sound). By default, livePhotoCaptureSuspended is NO. When you set livePhotoCaptureSuspended = YES, any Live Photo movie captures in progress are trimmed to the current time. Likewise, when you toggle livePhotoCaptureSuspended from YES to NO, subsequent Live Photo movie captures will not contain any samples earlier than the time you un-suspended Live Photo capture. Setting this property to YES throws an NSInvalidArgumentException if livePhotoCaptureEnabled is NO. By default, this property resets to NO when the AVCaptureSession stops. This behavior can be prevented by setting preservesLivePhotoCaptureSuspendedOnSessionStop to YES before stopping the session.
--
-- ObjC selector: @- livePhotoCaptureSuspended@
livePhotoCaptureSuspended :: IsAVCapturePhotoOutput avCapturePhotoOutput => avCapturePhotoOutput -> IO Bool
livePhotoCaptureSuspended avCapturePhotoOutput =
  sendMessage avCapturePhotoOutput livePhotoCaptureSuspendedSelector

-- | livePhotoCaptureSuspended
--
-- Indicates whether Live Photo capture is enabled, but currently suspended.
--
-- This property allows you to cut current Live Photo movie captures short (for instance, if you suddenly need to do something that you don't want to show up in the Live Photo movie, such as take a non Live Photo capture that makes a shutter sound). By default, livePhotoCaptureSuspended is NO. When you set livePhotoCaptureSuspended = YES, any Live Photo movie captures in progress are trimmed to the current time. Likewise, when you toggle livePhotoCaptureSuspended from YES to NO, subsequent Live Photo movie captures will not contain any samples earlier than the time you un-suspended Live Photo capture. Setting this property to YES throws an NSInvalidArgumentException if livePhotoCaptureEnabled is NO. By default, this property resets to NO when the AVCaptureSession stops. This behavior can be prevented by setting preservesLivePhotoCaptureSuspendedOnSessionStop to YES before stopping the session.
--
-- ObjC selector: @- setLivePhotoCaptureSuspended:@
setLivePhotoCaptureSuspended :: IsAVCapturePhotoOutput avCapturePhotoOutput => avCapturePhotoOutput -> Bool -> IO ()
setLivePhotoCaptureSuspended avCapturePhotoOutput value =
  sendMessage avCapturePhotoOutput setLivePhotoCaptureSuspendedSelector value

-- | preservesLivePhotoCaptureSuspendedOnSessionStop
--
-- By default, Live Photo capture is resumed when the session stops. This property allows clients to opt out of this and preserve the value of livePhotoCaptureSuspended.
--
-- Defaults to NO.
--
-- ObjC selector: @- preservesLivePhotoCaptureSuspendedOnSessionStop@
preservesLivePhotoCaptureSuspendedOnSessionStop :: IsAVCapturePhotoOutput avCapturePhotoOutput => avCapturePhotoOutput -> IO Bool
preservesLivePhotoCaptureSuspendedOnSessionStop avCapturePhotoOutput =
  sendMessage avCapturePhotoOutput preservesLivePhotoCaptureSuspendedOnSessionStopSelector

-- | preservesLivePhotoCaptureSuspendedOnSessionStop
--
-- By default, Live Photo capture is resumed when the session stops. This property allows clients to opt out of this and preserve the value of livePhotoCaptureSuspended.
--
-- Defaults to NO.
--
-- ObjC selector: @- setPreservesLivePhotoCaptureSuspendedOnSessionStop:@
setPreservesLivePhotoCaptureSuspendedOnSessionStop :: IsAVCapturePhotoOutput avCapturePhotoOutput => avCapturePhotoOutput -> Bool -> IO ()
setPreservesLivePhotoCaptureSuspendedOnSessionStop avCapturePhotoOutput value =
  sendMessage avCapturePhotoOutput setPreservesLivePhotoCaptureSuspendedOnSessionStopSelector value

-- | livePhotoAutoTrimmingEnabled
--
-- Indicates whether Live Photo movies are trimmed in real time to avoid excessive movement.
--
-- This property defaults to YES when livePhotoCaptureSupported is YES. Changing this property's value while your session is running will cause a lengthy reconfiguration of the session. You should set livePhotoAutoTrimmingEnabled to YES or NO before calling -[AVCaptureSession startRunning]. When set to YES, Live Photo movies are analyzed in real time and trimmed if there's excessive movement before or after the photo is taken. Nominally, Live Photos are approximately 3 seconds long. With trimming enabled, they may be shorter, depending on movement. This feature prevents common problems such as Live Photo movies containing shoe or pocket shots.
--
-- ObjC selector: @- livePhotoAutoTrimmingEnabled@
livePhotoAutoTrimmingEnabled :: IsAVCapturePhotoOutput avCapturePhotoOutput => avCapturePhotoOutput -> IO Bool
livePhotoAutoTrimmingEnabled avCapturePhotoOutput =
  sendMessage avCapturePhotoOutput livePhotoAutoTrimmingEnabledSelector

-- | livePhotoAutoTrimmingEnabled
--
-- Indicates whether Live Photo movies are trimmed in real time to avoid excessive movement.
--
-- This property defaults to YES when livePhotoCaptureSupported is YES. Changing this property's value while your session is running will cause a lengthy reconfiguration of the session. You should set livePhotoAutoTrimmingEnabled to YES or NO before calling -[AVCaptureSession startRunning]. When set to YES, Live Photo movies are analyzed in real time and trimmed if there's excessive movement before or after the photo is taken. Nominally, Live Photos are approximately 3 seconds long. With trimming enabled, they may be shorter, depending on movement. This feature prevents common problems such as Live Photo movies containing shoe or pocket shots.
--
-- ObjC selector: @- setLivePhotoAutoTrimmingEnabled:@
setLivePhotoAutoTrimmingEnabled :: IsAVCapturePhotoOutput avCapturePhotoOutput => avCapturePhotoOutput -> Bool -> IO ()
setLivePhotoAutoTrimmingEnabled avCapturePhotoOutput value =
  sendMessage avCapturePhotoOutput setLivePhotoAutoTrimmingEnabledSelector value

-- | availableLivePhotoVideoCodecTypes
--
-- An array of AVVideoCodecKey values that are currently supported by the receiver for use in the movie complement of a Live Photo.
--
-- Prior to iOS 11, all Live Photo movie video tracks are compressed using H.264. Beginning in iOS 11, you can select the Live Photo movie video compression format using one of the AVVideoCodecKey strings presented in this property. The system's default (preferred) video codec is always presented first in the list. If you've not yet added your receiver to an AVCaptureSession with a video source, no codecs are available. This property is key-value observable.
--
-- ObjC selector: @- availableLivePhotoVideoCodecTypes@
availableLivePhotoVideoCodecTypes :: IsAVCapturePhotoOutput avCapturePhotoOutput => avCapturePhotoOutput -> IO (Id NSArray)
availableLivePhotoVideoCodecTypes avCapturePhotoOutput =
  sendMessage avCapturePhotoOutput availableLivePhotoVideoCodecTypesSelector

-- | contentAwareDistortionCorrectionSupported
--
-- A BOOL value specifying whether content aware distortion correction is supported.
--
-- The rectilinear model used in optical design and by geometric distortion correction only preserves lines but not area, angles, or distance. Thus the wider the field of view of a lens, the greater the areal distortion at the edges of images. Content aware distortion correction, when enabled, intelligently corrects distortions by taking content into consideration, such as faces near the edges of the image. This property returns YES if the session's current configuration allows photos to be captured with content aware distortion correction. When switching cameras or formats or enabling depth data delivery this property may change. When this property changes from YES to NO, contentAwareDistortionCorrectionEnabled also reverts to NO. This property is key-value observable.
--
-- ObjC selector: @- contentAwareDistortionCorrectionSupported@
contentAwareDistortionCorrectionSupported :: IsAVCapturePhotoOutput avCapturePhotoOutput => avCapturePhotoOutput -> IO Bool
contentAwareDistortionCorrectionSupported avCapturePhotoOutput =
  sendMessage avCapturePhotoOutput contentAwareDistortionCorrectionSupportedSelector

-- | contentAwareDistortionCorrectionEnabled
--
-- A BOOL value specifying whether the photo render pipeline is set up to perform content aware distortion correction.
--
-- Default is NO. Set to YES if you wish content aware distortion correction to be performed on your AVCapturePhotos. This property may only be set to YES if contentAwareDistortionCorrectionSupported is YES. Note that warping the photos to preserve more natural looking content may result in a small change in field of view compared to what you see in the AVCaptureVideoPreviewLayer. The amount of field of view lost or gained is content specific and may vary from photo to photo. Enabling this property requires a lengthy reconfiguration of the capture render pipeline, so you should set this property to YES before calling -[AVCaptureSession startRunning].
--
-- ObjC selector: @- contentAwareDistortionCorrectionEnabled@
contentAwareDistortionCorrectionEnabled :: IsAVCapturePhotoOutput avCapturePhotoOutput => avCapturePhotoOutput -> IO Bool
contentAwareDistortionCorrectionEnabled avCapturePhotoOutput =
  sendMessage avCapturePhotoOutput contentAwareDistortionCorrectionEnabledSelector

-- | contentAwareDistortionCorrectionEnabled
--
-- A BOOL value specifying whether the photo render pipeline is set up to perform content aware distortion correction.
--
-- Default is NO. Set to YES if you wish content aware distortion correction to be performed on your AVCapturePhotos. This property may only be set to YES if contentAwareDistortionCorrectionSupported is YES. Note that warping the photos to preserve more natural looking content may result in a small change in field of view compared to what you see in the AVCaptureVideoPreviewLayer. The amount of field of view lost or gained is content specific and may vary from photo to photo. Enabling this property requires a lengthy reconfiguration of the capture render pipeline, so you should set this property to YES before calling -[AVCaptureSession startRunning].
--
-- ObjC selector: @- setContentAwareDistortionCorrectionEnabled:@
setContentAwareDistortionCorrectionEnabled :: IsAVCapturePhotoOutput avCapturePhotoOutput => avCapturePhotoOutput -> Bool -> IO ()
setContentAwareDistortionCorrectionEnabled avCapturePhotoOutput value =
  sendMessage avCapturePhotoOutput setContentAwareDistortionCorrectionEnabledSelector value

-- | zeroShutterLagSupported
--
-- A BOOL value specifying whether zero shutter lag is supported.
--
-- This property returns YES if the session's current configuration allows zero shutter lag. When switching cameras or formats, setting depthDataDeliveryEnabled, or setting virtualDeviceConstituentPhotoDeliveryEnabled this property may change. When this property changes from YES to NO, zeroShutterLagEnabled also reverts to NO. This property is key-value observable.
--
-- ObjC selector: @- zeroShutterLagSupported@
zeroShutterLagSupported :: IsAVCapturePhotoOutput avCapturePhotoOutput => avCapturePhotoOutput -> IO Bool
zeroShutterLagSupported avCapturePhotoOutput =
  sendMessage avCapturePhotoOutput zeroShutterLagSupportedSelector

-- | zeroShutterLagEnabled
--
-- A BOOL value specifying whether the output is set up to support zero shutter lag.
--
-- This property may only be set to YES if zeroShutterLagSupported is YES, otherwise an NSInvalidArgumentException is thrown. For apps linked on or after iOS 17 zero shutter lag is automatically enabled when supported. Enabling zero shutter lag reduces or eliminates shutter lag when using AVCapturePhotoQualityPrioritizationBalanced or Quality at the cost of additional memory usage by the photo output. The timestamp of the AVCapturePhoto may be slightly earlier than when -capturePhotoWithSettings:delegate: was called. To minimize camera shake from the user's tapping gesture it is recommended that -capturePhotoWithSettings:delegate: be called as early as possible when handling the touch down event. Zero shutter lag isn't available when using manual exposure or bracketed capture. Changing this property requires a lengthy reconfiguration of the capture render pipeline, so you should set this property to YES before calling -[AVCaptureSession startRunning] or within -[AVCaptureSession beginConfiguration] and -[AVCaptureSession commitConfiguration] while running.
--
-- ObjC selector: @- zeroShutterLagEnabled@
zeroShutterLagEnabled :: IsAVCapturePhotoOutput avCapturePhotoOutput => avCapturePhotoOutput -> IO Bool
zeroShutterLagEnabled avCapturePhotoOutput =
  sendMessage avCapturePhotoOutput zeroShutterLagEnabledSelector

-- | zeroShutterLagEnabled
--
-- A BOOL value specifying whether the output is set up to support zero shutter lag.
--
-- This property may only be set to YES if zeroShutterLagSupported is YES, otherwise an NSInvalidArgumentException is thrown. For apps linked on or after iOS 17 zero shutter lag is automatically enabled when supported. Enabling zero shutter lag reduces or eliminates shutter lag when using AVCapturePhotoQualityPrioritizationBalanced or Quality at the cost of additional memory usage by the photo output. The timestamp of the AVCapturePhoto may be slightly earlier than when -capturePhotoWithSettings:delegate: was called. To minimize camera shake from the user's tapping gesture it is recommended that -capturePhotoWithSettings:delegate: be called as early as possible when handling the touch down event. Zero shutter lag isn't available when using manual exposure or bracketed capture. Changing this property requires a lengthy reconfiguration of the capture render pipeline, so you should set this property to YES before calling -[AVCaptureSession startRunning] or within -[AVCaptureSession beginConfiguration] and -[AVCaptureSession commitConfiguration] while running.
--
-- ObjC selector: @- setZeroShutterLagEnabled:@
setZeroShutterLagEnabled :: IsAVCapturePhotoOutput avCapturePhotoOutput => avCapturePhotoOutput -> Bool -> IO ()
setZeroShutterLagEnabled avCapturePhotoOutput value =
  sendMessage avCapturePhotoOutput setZeroShutterLagEnabledSelector value

-- | responsiveCaptureSupported
--
-- A BOOL value specifying whether responsive capture is supported.
--
-- Enabling responsive capture increases peak and sustained capture rates, and reduces shutter lag at the cost of additional memory usage by the photo output. This property returns YES if the session's current configuration allows responsive capture. When switching cameras or formats, enabling depth data delivery, or enabling zero shutter lag this property may change. Responsive capture is only supported when zero shutter lag is enabled. When this property changes from YES to NO, responsiveCaptureEnabled also reverts to NO. This property is key-value observable.
--
-- ObjC selector: @- responsiveCaptureSupported@
responsiveCaptureSupported :: IsAVCapturePhotoOutput avCapturePhotoOutput => avCapturePhotoOutput -> IO Bool
responsiveCaptureSupported avCapturePhotoOutput =
  sendMessage avCapturePhotoOutput responsiveCaptureSupportedSelector

-- | responsiveCaptureEnabled
--
-- A BOOL value specifying whether the photo output is set up to support responsive capture.
--
-- This property may only be set to YES if responsiveCaptureSupported is YES, otherwise an NSInvalidArgumentException is thrown. When responsiveCaptureEnabled is YES the captureReadiness property should be used to determine whether new capture requests can be serviced in a reasonable time and whether the shutter control should be available to the user. Responsive capture adds buffering between the capture and photo processing stages which allows a new capture to start before processing has completed for the previous capture, so be prepared to handle -captureOutput:willBeginCaptureForResolvedSettings: being called before the -captureOutput:didFinishProcessingPhoto: for the prior requests. Processed photos continue to be delivered in the order they were captured. To minimize camera shake from the user's tapping gesture it is recommended that -capturePhotoWithSettings:delegate: be called as early as possible when handling the touch down event. Enabling responsive capture allows the fast capture prioritization feature to be used, which further increases capture rates and reduces preview and recording disruptions. See the fastCapturePrioritizationEnabled property. When requesting uncompressed output using kCVPixelBufferPixelFormatTypeKey in AVCapturePhotoSetting.format the AVCapturePhoto's pixelBuffer is allocated from a pool with enough capacity for that request only, and overlap between capture and processing is disabled. The client must release the AVCapturePhoto and references to the pixelBuffer before capturing again and the pixelBuffer's IOSurface must also no longer be in use. Changing this property requires a lengthy reconfiguration of the capture render pipeline, so you should set this property to YES before calling -[AVCaptureSession startRunning] or within -[AVCaptureSession beginConfiguration] and -[AVCaptureSession commitConfiguration] while running.
--
-- ObjC selector: @- responsiveCaptureEnabled@
responsiveCaptureEnabled :: IsAVCapturePhotoOutput avCapturePhotoOutput => avCapturePhotoOutput -> IO Bool
responsiveCaptureEnabled avCapturePhotoOutput =
  sendMessage avCapturePhotoOutput responsiveCaptureEnabledSelector

-- | responsiveCaptureEnabled
--
-- A BOOL value specifying whether the photo output is set up to support responsive capture.
--
-- This property may only be set to YES if responsiveCaptureSupported is YES, otherwise an NSInvalidArgumentException is thrown. When responsiveCaptureEnabled is YES the captureReadiness property should be used to determine whether new capture requests can be serviced in a reasonable time and whether the shutter control should be available to the user. Responsive capture adds buffering between the capture and photo processing stages which allows a new capture to start before processing has completed for the previous capture, so be prepared to handle -captureOutput:willBeginCaptureForResolvedSettings: being called before the -captureOutput:didFinishProcessingPhoto: for the prior requests. Processed photos continue to be delivered in the order they were captured. To minimize camera shake from the user's tapping gesture it is recommended that -capturePhotoWithSettings:delegate: be called as early as possible when handling the touch down event. Enabling responsive capture allows the fast capture prioritization feature to be used, which further increases capture rates and reduces preview and recording disruptions. See the fastCapturePrioritizationEnabled property. When requesting uncompressed output using kCVPixelBufferPixelFormatTypeKey in AVCapturePhotoSetting.format the AVCapturePhoto's pixelBuffer is allocated from a pool with enough capacity for that request only, and overlap between capture and processing is disabled. The client must release the AVCapturePhoto and references to the pixelBuffer before capturing again and the pixelBuffer's IOSurface must also no longer be in use. Changing this property requires a lengthy reconfiguration of the capture render pipeline, so you should set this property to YES before calling -[AVCaptureSession startRunning] or within -[AVCaptureSession beginConfiguration] and -[AVCaptureSession commitConfiguration] while running.
--
-- ObjC selector: @- setResponsiveCaptureEnabled:@
setResponsiveCaptureEnabled :: IsAVCapturePhotoOutput avCapturePhotoOutput => avCapturePhotoOutput -> Bool -> IO ()
setResponsiveCaptureEnabled avCapturePhotoOutput value =
  sendMessage avCapturePhotoOutput setResponsiveCaptureEnabledSelector value

-- | captureReadiness
--
-- A value specifying whether the photo output is ready to respond to new capture requests in a timely manner.
--
-- This property can be key-value observed to enable and disable shutter button UI depending on whether the output is ready to capture, which is especially important when the responsiveCaptureEnabled property is YES. When interacting with AVCapturePhotoOutput on a background queue AVCapturePhotoOutputReadinessCoordinator should instead be used to observe readiness changes and perform UI updates. Capturing only when the output is ready limits the number of requests inflight to minimize shutter lag while maintaining the fastest shot to shot time. When the property returns a value other than Ready the output is not ready to capture and the shutter button should be disabled to prevent the user from initiating new requests. The output continues to accept requests when the captureReadiness property returns a value other than Ready, but the request may not be serviced for a longer period. The visual presentation of the shutter button can be customized based on the readiness value. When the user rapidly taps the shutter button the property may transition to NotReadyMomentarily for a brief period. Although the shutter button should be disabled during this period it is short lived enough that dimming or changing the appearance of the shutter is not recommended as it would be visually distracting to the user. Longer running capture types like flash or captures with AVCapturePhotoQualityPrioritizationQuality may prevent the output from capturing for an extended period, indicated by NotReadyWaitingForCapture or NotReadyWaitingForProcessing, which is appropriate to show by dimming or disabling the shutter button. For NotReadyWaitingForProcessing it is also appropriate to show a spinner or other indication that the shutter is busy.
--
-- ObjC selector: @- captureReadiness@
captureReadiness :: IsAVCapturePhotoOutput avCapturePhotoOutput => avCapturePhotoOutput -> IO AVCapturePhotoOutputCaptureReadiness
captureReadiness avCapturePhotoOutput =
  sendMessage avCapturePhotoOutput captureReadinessSelector

-- | constantColorSupported
--
-- A BOOL value specifying whether constant color capture is supported.
--
-- An object's color in a photograph is affected by the light sources illuminating the scene, so the color of the same object photographed in warm light might look markedly different than in colder light. In some use cases, such ambient light induced color variation is undesirable, and the user may prefer an estimate of what these materials would look like under a standard light such as daylight (D65), regardless of the lighting conditions at the time the photograph was taken. Some devices are capable of producing such constant color photos.
--
-- Constant color captures require the flash to be fired and may require pre-flash sequence to determine the correct focus and exposure, therefore it might take several seconds to acquire a constant color photo. Due to this flash requirement, a constant color capture can only be taken with AVCaptureFlashModeAuto or AVCaptureFlashModeOn as the flash mode, otherwise an exception is thrown.
--
-- Constant color can only be achieved when the flash has a discernible effect on the scene so it may not perform well in bright conditions such as direct sunlight. Use the constantColorConfidenceMap property to examine the confidence level, and therefore the usefulness, of each region of a constant color photo.
--
-- Constant color should not be used in conjunction with locked or manual white balance.
--
-- This property returns YES if the session's current configuration allows photos to be captured with constant color. When switching cameras or formats this property may change. When this property changes from YES to NO, constantColorEnabled also reverts to NO. If you've previously opted in for constant color and then change configurations, you may need to set constantColorEnabled = YES again. This property is key-value observable.
--
-- ObjC selector: @- constantColorSupported@
constantColorSupported :: IsAVCapturePhotoOutput avCapturePhotoOutput => avCapturePhotoOutput -> IO Bool
constantColorSupported avCapturePhotoOutput =
  sendMessage avCapturePhotoOutput constantColorSupportedSelector

-- | constantColorEnabled
--
-- A BOOL value specifying whether the photo render pipeline is set up to perform constant color captures.
--
-- Default is NO. Set to YES to enable support for taking constant color photos. This property may only be set to YES if constantColorSupported is YES. Enabling constant color requires a lengthy reconfiguration of the capture render pipeline, so if you intend to capture constant color photos, you should set this property to YES before calling -[AVCaptureSession startRunning] or within -[AVCaptureSession beginConfiguration] and -[AVCaptureSession commitConfiguration] while running.
--
-- ObjC selector: @- constantColorEnabled@
constantColorEnabled :: IsAVCapturePhotoOutput avCapturePhotoOutput => avCapturePhotoOutput -> IO Bool
constantColorEnabled avCapturePhotoOutput =
  sendMessage avCapturePhotoOutput constantColorEnabledSelector

-- | constantColorEnabled
--
-- A BOOL value specifying whether the photo render pipeline is set up to perform constant color captures.
--
-- Default is NO. Set to YES to enable support for taking constant color photos. This property may only be set to YES if constantColorSupported is YES. Enabling constant color requires a lengthy reconfiguration of the capture render pipeline, so if you intend to capture constant color photos, you should set this property to YES before calling -[AVCaptureSession startRunning] or within -[AVCaptureSession beginConfiguration] and -[AVCaptureSession commitConfiguration] while running.
--
-- ObjC selector: @- setConstantColorEnabled:@
setConstantColorEnabled :: IsAVCapturePhotoOutput avCapturePhotoOutput => avCapturePhotoOutput -> Bool -> IO ()
setConstantColorEnabled avCapturePhotoOutput value =
  sendMessage avCapturePhotoOutput setConstantColorEnabledSelector value

-- | shutterSoundSuppressionSupported
--
-- Specifies whether suppressing the shutter sound is supported.
--
-- On iOS, this property returns NO in jurisdictions where shutter sound production cannot be disabled. On all other platforms, it always returns NO.
--
-- ObjC selector: @- shutterSoundSuppressionSupported@
shutterSoundSuppressionSupported :: IsAVCapturePhotoOutput avCapturePhotoOutput => avCapturePhotoOutput -> IO Bool
shutterSoundSuppressionSupported avCapturePhotoOutput =
  sendMessage avCapturePhotoOutput shutterSoundSuppressionSupportedSelector

-- | cameraSensorOrientationCompensationSupported
--
-- A read-only BOOL value indicating whether still image buffers may be rotated to match the sensor orientation of earlier generation hardware.
--
-- Value is YES for camera configurations which support compensation for the sensor orientation, which is applied to HEIC, JPEG, and uncompressed processed photos only; compensation is never applied to Bayer RAW or Apple ProRaw captures.
--
-- ObjC selector: @- cameraSensorOrientationCompensationSupported@
cameraSensorOrientationCompensationSupported :: IsAVCapturePhotoOutput avCapturePhotoOutput => avCapturePhotoOutput -> IO Bool
cameraSensorOrientationCompensationSupported avCapturePhotoOutput =
  sendMessage avCapturePhotoOutput cameraSensorOrientationCompensationSupportedSelector

-- | cameraSensorOrientationCompensationEnabled
--
-- A BOOL value indicating that still image buffers will be rotated to match the sensor orientation of earlier generation hardware.
--
-- Default is YES when cameraSensorOrientationCompensationSupported is YES. Set to NO if your app does not require sensor orientation compensation.
--
-- ObjC selector: @- cameraSensorOrientationCompensationEnabled@
cameraSensorOrientationCompensationEnabled :: IsAVCapturePhotoOutput avCapturePhotoOutput => avCapturePhotoOutput -> IO Bool
cameraSensorOrientationCompensationEnabled avCapturePhotoOutput =
  sendMessage avCapturePhotoOutput cameraSensorOrientationCompensationEnabledSelector

-- | cameraSensorOrientationCompensationEnabled
--
-- A BOOL value indicating that still image buffers will be rotated to match the sensor orientation of earlier generation hardware.
--
-- Default is YES when cameraSensorOrientationCompensationSupported is YES. Set to NO if your app does not require sensor orientation compensation.
--
-- ObjC selector: @- setCameraSensorOrientationCompensationEnabled:@
setCameraSensorOrientationCompensationEnabled :: IsAVCapturePhotoOutput avCapturePhotoOutput => avCapturePhotoOutput -> Bool -> IO ()
setCameraSensorOrientationCompensationEnabled avCapturePhotoOutput value =
  sendMessage avCapturePhotoOutput setCameraSensorOrientationCompensationEnabledSelector value

-- | depthDataDeliverySupported
--
-- A BOOL value specifying whether depth data delivery is supported.
--
-- Some cameras and configurations support the delivery of depth data (e.g. disparity maps) along with the photo. This property returns YES if the session's current configuration allows photos to be captured with depth data, from which depth-related filters may be applied. When switching cameras or formats this property may change. When this property changes from YES to NO, depthDataDeliveryEnabled also reverts to NO. If you've previously opted in for depth data delivery and then change configurations, you may need to set depthDataDeliveryEnabled = YES again. This property is key-value observable.
--
-- ObjC selector: @- depthDataDeliverySupported@
depthDataDeliverySupported :: IsAVCapturePhotoOutput avCapturePhotoOutput => avCapturePhotoOutput -> IO Bool
depthDataDeliverySupported avCapturePhotoOutput =
  sendMessage avCapturePhotoOutput depthDataDeliverySupportedSelector

-- | depthDataDeliveryEnabled
--
-- A BOOL specifying whether the photo render pipeline is prepared for depth data delivery.
--
-- Default is NO. Set to YES if you wish depth data to be delivered with your AVCapturePhotos. This property may only be set to YES if depthDataDeliverySupported is YES. Enabling depth data delivery requires a lengthy reconfiguration of the capture render pipeline, so if you intend to capture depth data, you should set this property to YES before calling -[AVCaptureSession startRunning].
--
-- ObjC selector: @- depthDataDeliveryEnabled@
depthDataDeliveryEnabled :: IsAVCapturePhotoOutput avCapturePhotoOutput => avCapturePhotoOutput -> IO Bool
depthDataDeliveryEnabled avCapturePhotoOutput =
  sendMessage avCapturePhotoOutput depthDataDeliveryEnabledSelector

-- | depthDataDeliveryEnabled
--
-- A BOOL specifying whether the photo render pipeline is prepared for depth data delivery.
--
-- Default is NO. Set to YES if you wish depth data to be delivered with your AVCapturePhotos. This property may only be set to YES if depthDataDeliverySupported is YES. Enabling depth data delivery requires a lengthy reconfiguration of the capture render pipeline, so if you intend to capture depth data, you should set this property to YES before calling -[AVCaptureSession startRunning].
--
-- ObjC selector: @- setDepthDataDeliveryEnabled:@
setDepthDataDeliveryEnabled :: IsAVCapturePhotoOutput avCapturePhotoOutput => avCapturePhotoOutput -> Bool -> IO ()
setDepthDataDeliveryEnabled avCapturePhotoOutput value =
  sendMessage avCapturePhotoOutput setDepthDataDeliveryEnabledSelector value

-- | portraitEffectsMatteDeliverySupported
--
-- A BOOL value specifying whether portrait effects matte delivery is supported.
--
-- Some cameras and configurations support the delivery of a matting image to augment depth data and aid in high quality portrait effect rendering (see AVPortraitEffectsMatte.h). This property returns YES if the session's current configuration allows photos to be captured with a portrait effects matte. When switching cameras or formats this property may change. When this property changes from YES to NO, portraitEffectsMatteDeliveryEnabled also reverts to NO. If you've previously opted in for portrait effects matte delivery and then change configurations, you may need to set portraitEffectsMatteDeliveryEnabled = YES again. This property is key-value observable.
--
-- ObjC selector: @- portraitEffectsMatteDeliverySupported@
portraitEffectsMatteDeliverySupported :: IsAVCapturePhotoOutput avCapturePhotoOutput => avCapturePhotoOutput -> IO Bool
portraitEffectsMatteDeliverySupported avCapturePhotoOutput =
  sendMessage avCapturePhotoOutput portraitEffectsMatteDeliverySupportedSelector

-- | portraitEffectsMatteDeliveryEnabled
--
-- A BOOL specifying whether the photo render pipeline is prepared for portrait effects matte delivery.
--
-- Default is NO. Set to YES if you wish portrait effects mattes to be delivered with your AVCapturePhotos. This property may only be set to YES if portraitEffectsMatteDeliverySupported is YES. Portrait effects matte generation requires depth to be present, so when enabling portrait effects matte delivery, you must also set depthDataDeliveryEnabled to YES. Enabling portrait effects matte delivery requires a lengthy reconfiguration of the capture render pipeline, so if you intend to capture portrait effects mattes, you should set this property to YES before calling -[AVCaptureSession startRunning].
--
-- ObjC selector: @- portraitEffectsMatteDeliveryEnabled@
portraitEffectsMatteDeliveryEnabled :: IsAVCapturePhotoOutput avCapturePhotoOutput => avCapturePhotoOutput -> IO Bool
portraitEffectsMatteDeliveryEnabled avCapturePhotoOutput =
  sendMessage avCapturePhotoOutput portraitEffectsMatteDeliveryEnabledSelector

-- | portraitEffectsMatteDeliveryEnabled
--
-- A BOOL specifying whether the photo render pipeline is prepared for portrait effects matte delivery.
--
-- Default is NO. Set to YES if you wish portrait effects mattes to be delivered with your AVCapturePhotos. This property may only be set to YES if portraitEffectsMatteDeliverySupported is YES. Portrait effects matte generation requires depth to be present, so when enabling portrait effects matte delivery, you must also set depthDataDeliveryEnabled to YES. Enabling portrait effects matte delivery requires a lengthy reconfiguration of the capture render pipeline, so if you intend to capture portrait effects mattes, you should set this property to YES before calling -[AVCaptureSession startRunning].
--
-- ObjC selector: @- setPortraitEffectsMatteDeliveryEnabled:@
setPortraitEffectsMatteDeliveryEnabled :: IsAVCapturePhotoOutput avCapturePhotoOutput => avCapturePhotoOutput -> Bool -> IO ()
setPortraitEffectsMatteDeliveryEnabled avCapturePhotoOutput value =
  sendMessage avCapturePhotoOutput setPortraitEffectsMatteDeliveryEnabledSelector value

-- | availableSemanticSegmentationMatteTypes
--
-- An array of supported semantic segmentation matte types that may be captured and delivered along with your AVCapturePhotos.
--
-- Some cameras and configurations support the delivery of semantic segmentation matting images (e.g. segmentations of the hair, skin, or teeth in the photo). This property returns an array of AVSemanticSegmentationMatteTypes available given the session's current configuration. When switching cameras or formats this property may change. When this property changes, enabledSemanticSegmentationMatteTypes reverts to an empty array. If you've previously opted in for delivery of one or more semantic segmentation mattes and then change configurations, you need to set up your enabledSemanticSegmentationMatteTypes again. This property is key-value observable.
--
-- ObjC selector: @- availableSemanticSegmentationMatteTypes@
availableSemanticSegmentationMatteTypes :: IsAVCapturePhotoOutput avCapturePhotoOutput => avCapturePhotoOutput -> IO (Id NSArray)
availableSemanticSegmentationMatteTypes avCapturePhotoOutput =
  sendMessage avCapturePhotoOutput availableSemanticSegmentationMatteTypesSelector

-- | enabledSemanticSegmentationMatteTypes
--
-- An array of semantic segmentation matte types which the photo render pipeline is prepared to deliver.
--
-- Default is empty array. You may set this to the array of matte types you'd like to be delivered with your AVCapturePhotos. The array may only contain values present in availableSemanticSegmentationMatteTypes. Enabling semantic segmentation matte delivery requires a lengthy reconfiguration of the capture render pipeline, so if you intend to capture semantic segmentation mattes, you should set this property to YES before calling -[AVCaptureSession startRunning].
--
-- ObjC selector: @- enabledSemanticSegmentationMatteTypes@
enabledSemanticSegmentationMatteTypes :: IsAVCapturePhotoOutput avCapturePhotoOutput => avCapturePhotoOutput -> IO (Id NSArray)
enabledSemanticSegmentationMatteTypes avCapturePhotoOutput =
  sendMessage avCapturePhotoOutput enabledSemanticSegmentationMatteTypesSelector

-- | enabledSemanticSegmentationMatteTypes
--
-- An array of semantic segmentation matte types which the photo render pipeline is prepared to deliver.
--
-- Default is empty array. You may set this to the array of matte types you'd like to be delivered with your AVCapturePhotos. The array may only contain values present in availableSemanticSegmentationMatteTypes. Enabling semantic segmentation matte delivery requires a lengthy reconfiguration of the capture render pipeline, so if you intend to capture semantic segmentation mattes, you should set this property to YES before calling -[AVCaptureSession startRunning].
--
-- ObjC selector: @- setEnabledSemanticSegmentationMatteTypes:@
setEnabledSemanticSegmentationMatteTypes :: (IsAVCapturePhotoOutput avCapturePhotoOutput, IsNSArray value) => avCapturePhotoOutput -> value -> IO ()
setEnabledSemanticSegmentationMatteTypes avCapturePhotoOutput value =
  sendMessage avCapturePhotoOutput setEnabledSemanticSegmentationMatteTypesSelector (toNSArray value)

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @init@
initSelector :: Selector '[] (Id AVCapturePhotoOutput)
initSelector = mkSelector "init"

-- | @Selector@ for @new@
newSelector :: Selector '[] (Id AVCapturePhotoOutput)
newSelector = mkSelector "new"

-- | @Selector@ for @capturePhotoWithSettings:delegate:@
capturePhotoWithSettings_delegateSelector :: Selector '[Id AVCapturePhotoSettings, RawId] ()
capturePhotoWithSettings_delegateSelector = mkSelector "capturePhotoWithSettings:delegate:"

-- | @Selector@ for @setPreparedPhotoSettingsArray:completionHandler:@
setPreparedPhotoSettingsArray_completionHandlerSelector :: Selector '[Id NSArray, Ptr ()] ()
setPreparedPhotoSettingsArray_completionHandlerSelector = mkSelector "setPreparedPhotoSettingsArray:completionHandler:"

-- | @Selector@ for @isBayerRAWPixelFormat:@
isBayerRAWPixelFormatSelector :: Selector '[CUInt] Bool
isBayerRAWPixelFormatSelector = mkSelector "isBayerRAWPixelFormat:"

-- | @Selector@ for @isAppleProRAWPixelFormat:@
isAppleProRAWPixelFormatSelector :: Selector '[CUInt] Bool
isAppleProRAWPixelFormatSelector = mkSelector "isAppleProRAWPixelFormat:"

-- | @Selector@ for @supportedPhotoPixelFormatTypesForFileType:@
supportedPhotoPixelFormatTypesForFileTypeSelector :: Selector '[Id NSString] (Id NSArray)
supportedPhotoPixelFormatTypesForFileTypeSelector = mkSelector "supportedPhotoPixelFormatTypesForFileType:"

-- | @Selector@ for @supportedPhotoCodecTypesForFileType:@
supportedPhotoCodecTypesForFileTypeSelector :: Selector '[Id NSString] (Id NSArray)
supportedPhotoCodecTypesForFileTypeSelector = mkSelector "supportedPhotoCodecTypesForFileType:"

-- | @Selector@ for @supportedRawPhotoCodecTypesForRawPhotoPixelFormatType:fileType:@
supportedRawPhotoCodecTypesForRawPhotoPixelFormatType_fileTypeSelector :: Selector '[CUInt, Id NSString] (Id NSArray)
supportedRawPhotoCodecTypesForRawPhotoPixelFormatType_fileTypeSelector = mkSelector "supportedRawPhotoCodecTypesForRawPhotoPixelFormatType:fileType:"

-- | @Selector@ for @supportedRawPhotoPixelFormatTypesForFileType:@
supportedRawPhotoPixelFormatTypesForFileTypeSelector :: Selector '[Id NSString] (Id NSArray)
supportedRawPhotoPixelFormatTypesForFileTypeSelector = mkSelector "supportedRawPhotoPixelFormatTypesForFileType:"

-- | @Selector@ for @JPEGPhotoDataRepresentationForJPEGSampleBuffer:previewPhotoSampleBuffer:@
jpegPhotoDataRepresentationForJPEGSampleBuffer_previewPhotoSampleBufferSelector :: Selector '[Ptr (), Ptr ()] (Id NSData)
jpegPhotoDataRepresentationForJPEGSampleBuffer_previewPhotoSampleBufferSelector = mkSelector "JPEGPhotoDataRepresentationForJPEGSampleBuffer:previewPhotoSampleBuffer:"

-- | @Selector@ for @DNGPhotoDataRepresentationForRawSampleBuffer:previewPhotoSampleBuffer:@
dngPhotoDataRepresentationForRawSampleBuffer_previewPhotoSampleBufferSelector :: Selector '[Ptr (), Ptr ()] (Id NSData)
dngPhotoDataRepresentationForRawSampleBuffer_previewPhotoSampleBufferSelector = mkSelector "DNGPhotoDataRepresentationForRawSampleBuffer:previewPhotoSampleBuffer:"

-- | @Selector@ for @preparedPhotoSettingsArray@
preparedPhotoSettingsArraySelector :: Selector '[] (Id NSArray)
preparedPhotoSettingsArraySelector = mkSelector "preparedPhotoSettingsArray"

-- | @Selector@ for @availablePhotoPixelFormatTypes@
availablePhotoPixelFormatTypesSelector :: Selector '[] (Id NSArray)
availablePhotoPixelFormatTypesSelector = mkSelector "availablePhotoPixelFormatTypes"

-- | @Selector@ for @availablePhotoCodecTypes@
availablePhotoCodecTypesSelector :: Selector '[] (Id NSArray)
availablePhotoCodecTypesSelector = mkSelector "availablePhotoCodecTypes"

-- | @Selector@ for @availableRawPhotoCodecTypes@
availableRawPhotoCodecTypesSelector :: Selector '[] (Id NSArray)
availableRawPhotoCodecTypesSelector = mkSelector "availableRawPhotoCodecTypes"

-- | @Selector@ for @appleProRAWSupported@
appleProRAWSupportedSelector :: Selector '[] Bool
appleProRAWSupportedSelector = mkSelector "appleProRAWSupported"

-- | @Selector@ for @appleProRAWEnabled@
appleProRAWEnabledSelector :: Selector '[] Bool
appleProRAWEnabledSelector = mkSelector "appleProRAWEnabled"

-- | @Selector@ for @setAppleProRAWEnabled:@
setAppleProRAWEnabledSelector :: Selector '[Bool] ()
setAppleProRAWEnabledSelector = mkSelector "setAppleProRAWEnabled:"

-- | @Selector@ for @availableRawPhotoPixelFormatTypes@
availableRawPhotoPixelFormatTypesSelector :: Selector '[] (Id NSArray)
availableRawPhotoPixelFormatTypesSelector = mkSelector "availableRawPhotoPixelFormatTypes"

-- | @Selector@ for @availablePhotoFileTypes@
availablePhotoFileTypesSelector :: Selector '[] (Id NSArray)
availablePhotoFileTypesSelector = mkSelector "availablePhotoFileTypes"

-- | @Selector@ for @availableRawPhotoFileTypes@
availableRawPhotoFileTypesSelector :: Selector '[] (Id NSArray)
availableRawPhotoFileTypesSelector = mkSelector "availableRawPhotoFileTypes"

-- | @Selector@ for @maxPhotoQualityPrioritization@
maxPhotoQualityPrioritizationSelector :: Selector '[] AVCapturePhotoQualityPrioritization
maxPhotoQualityPrioritizationSelector = mkSelector "maxPhotoQualityPrioritization"

-- | @Selector@ for @setMaxPhotoQualityPrioritization:@
setMaxPhotoQualityPrioritizationSelector :: Selector '[AVCapturePhotoQualityPrioritization] ()
setMaxPhotoQualityPrioritizationSelector = mkSelector "setMaxPhotoQualityPrioritization:"

-- | @Selector@ for @fastCapturePrioritizationSupported@
fastCapturePrioritizationSupportedSelector :: Selector '[] Bool
fastCapturePrioritizationSupportedSelector = mkSelector "fastCapturePrioritizationSupported"

-- | @Selector@ for @setFastCapturePrioritizationSupported:@
setFastCapturePrioritizationSupportedSelector :: Selector '[Bool] ()
setFastCapturePrioritizationSupportedSelector = mkSelector "setFastCapturePrioritizationSupported:"

-- | @Selector@ for @fastCapturePrioritizationEnabled@
fastCapturePrioritizationEnabledSelector :: Selector '[] Bool
fastCapturePrioritizationEnabledSelector = mkSelector "fastCapturePrioritizationEnabled"

-- | @Selector@ for @setFastCapturePrioritizationEnabled:@
setFastCapturePrioritizationEnabledSelector :: Selector '[Bool] ()
setFastCapturePrioritizationEnabledSelector = mkSelector "setFastCapturePrioritizationEnabled:"

-- | @Selector@ for @autoDeferredPhotoDeliverySupported@
autoDeferredPhotoDeliverySupportedSelector :: Selector '[] Bool
autoDeferredPhotoDeliverySupportedSelector = mkSelector "autoDeferredPhotoDeliverySupported"

-- | @Selector@ for @autoDeferredPhotoDeliveryEnabled@
autoDeferredPhotoDeliveryEnabledSelector :: Selector '[] Bool
autoDeferredPhotoDeliveryEnabledSelector = mkSelector "autoDeferredPhotoDeliveryEnabled"

-- | @Selector@ for @setAutoDeferredPhotoDeliveryEnabled:@
setAutoDeferredPhotoDeliveryEnabledSelector :: Selector '[Bool] ()
setAutoDeferredPhotoDeliveryEnabledSelector = mkSelector "setAutoDeferredPhotoDeliveryEnabled:"

-- | @Selector@ for @stillImageStabilizationSupported@
stillImageStabilizationSupportedSelector :: Selector '[] Bool
stillImageStabilizationSupportedSelector = mkSelector "stillImageStabilizationSupported"

-- | @Selector@ for @isStillImageStabilizationScene@
isStillImageStabilizationSceneSelector :: Selector '[] Bool
isStillImageStabilizationSceneSelector = mkSelector "isStillImageStabilizationScene"

-- | @Selector@ for @virtualDeviceFusionSupported@
virtualDeviceFusionSupportedSelector :: Selector '[] Bool
virtualDeviceFusionSupportedSelector = mkSelector "virtualDeviceFusionSupported"

-- | @Selector@ for @dualCameraFusionSupported@
dualCameraFusionSupportedSelector :: Selector '[] Bool
dualCameraFusionSupportedSelector = mkSelector "dualCameraFusionSupported"

-- | @Selector@ for @virtualDeviceConstituentPhotoDeliverySupported@
virtualDeviceConstituentPhotoDeliverySupportedSelector :: Selector '[] Bool
virtualDeviceConstituentPhotoDeliverySupportedSelector = mkSelector "virtualDeviceConstituentPhotoDeliverySupported"

-- | @Selector@ for @dualCameraDualPhotoDeliverySupported@
dualCameraDualPhotoDeliverySupportedSelector :: Selector '[] Bool
dualCameraDualPhotoDeliverySupportedSelector = mkSelector "dualCameraDualPhotoDeliverySupported"

-- | @Selector@ for @virtualDeviceConstituentPhotoDeliveryEnabled@
virtualDeviceConstituentPhotoDeliveryEnabledSelector :: Selector '[] Bool
virtualDeviceConstituentPhotoDeliveryEnabledSelector = mkSelector "virtualDeviceConstituentPhotoDeliveryEnabled"

-- | @Selector@ for @setVirtualDeviceConstituentPhotoDeliveryEnabled:@
setVirtualDeviceConstituentPhotoDeliveryEnabledSelector :: Selector '[Bool] ()
setVirtualDeviceConstituentPhotoDeliveryEnabledSelector = mkSelector "setVirtualDeviceConstituentPhotoDeliveryEnabled:"

-- | @Selector@ for @dualCameraDualPhotoDeliveryEnabled@
dualCameraDualPhotoDeliveryEnabledSelector :: Selector '[] Bool
dualCameraDualPhotoDeliveryEnabledSelector = mkSelector "dualCameraDualPhotoDeliveryEnabled"

-- | @Selector@ for @setDualCameraDualPhotoDeliveryEnabled:@
setDualCameraDualPhotoDeliveryEnabledSelector :: Selector '[Bool] ()
setDualCameraDualPhotoDeliveryEnabledSelector = mkSelector "setDualCameraDualPhotoDeliveryEnabled:"

-- | @Selector@ for @cameraCalibrationDataDeliverySupported@
cameraCalibrationDataDeliverySupportedSelector :: Selector '[] Bool
cameraCalibrationDataDeliverySupportedSelector = mkSelector "cameraCalibrationDataDeliverySupported"

-- | @Selector@ for @supportedFlashModes@
supportedFlashModesSelector :: Selector '[] (Id NSArray)
supportedFlashModesSelector = mkSelector "supportedFlashModes"

-- | @Selector@ for @autoRedEyeReductionSupported@
autoRedEyeReductionSupportedSelector :: Selector '[] Bool
autoRedEyeReductionSupportedSelector = mkSelector "autoRedEyeReductionSupported"

-- | @Selector@ for @isFlashScene@
isFlashSceneSelector :: Selector '[] Bool
isFlashSceneSelector = mkSelector "isFlashScene"

-- | @Selector@ for @photoSettingsForSceneMonitoring@
photoSettingsForSceneMonitoringSelector :: Selector '[] (Id AVCapturePhotoSettings)
photoSettingsForSceneMonitoringSelector = mkSelector "photoSettingsForSceneMonitoring"

-- | @Selector@ for @setPhotoSettingsForSceneMonitoring:@
setPhotoSettingsForSceneMonitoringSelector :: Selector '[Id AVCapturePhotoSettings] ()
setPhotoSettingsForSceneMonitoringSelector = mkSelector "setPhotoSettingsForSceneMonitoring:"

-- | @Selector@ for @highResolutionCaptureEnabled@
highResolutionCaptureEnabledSelector :: Selector '[] Bool
highResolutionCaptureEnabledSelector = mkSelector "highResolutionCaptureEnabled"

-- | @Selector@ for @setHighResolutionCaptureEnabled:@
setHighResolutionCaptureEnabledSelector :: Selector '[Bool] ()
setHighResolutionCaptureEnabledSelector = mkSelector "setHighResolutionCaptureEnabled:"

-- | @Selector@ for @maxBracketedCapturePhotoCount@
maxBracketedCapturePhotoCountSelector :: Selector '[] CULong
maxBracketedCapturePhotoCountSelector = mkSelector "maxBracketedCapturePhotoCount"

-- | @Selector@ for @lensStabilizationDuringBracketedCaptureSupported@
lensStabilizationDuringBracketedCaptureSupportedSelector :: Selector '[] Bool
lensStabilizationDuringBracketedCaptureSupportedSelector = mkSelector "lensStabilizationDuringBracketedCaptureSupported"

-- | @Selector@ for @livePhotoCaptureSupported@
livePhotoCaptureSupportedSelector :: Selector '[] Bool
livePhotoCaptureSupportedSelector = mkSelector "livePhotoCaptureSupported"

-- | @Selector@ for @livePhotoCaptureEnabled@
livePhotoCaptureEnabledSelector :: Selector '[] Bool
livePhotoCaptureEnabledSelector = mkSelector "livePhotoCaptureEnabled"

-- | @Selector@ for @setLivePhotoCaptureEnabled:@
setLivePhotoCaptureEnabledSelector :: Selector '[Bool] ()
setLivePhotoCaptureEnabledSelector = mkSelector "setLivePhotoCaptureEnabled:"

-- | @Selector@ for @livePhotoCaptureSuspended@
livePhotoCaptureSuspendedSelector :: Selector '[] Bool
livePhotoCaptureSuspendedSelector = mkSelector "livePhotoCaptureSuspended"

-- | @Selector@ for @setLivePhotoCaptureSuspended:@
setLivePhotoCaptureSuspendedSelector :: Selector '[Bool] ()
setLivePhotoCaptureSuspendedSelector = mkSelector "setLivePhotoCaptureSuspended:"

-- | @Selector@ for @preservesLivePhotoCaptureSuspendedOnSessionStop@
preservesLivePhotoCaptureSuspendedOnSessionStopSelector :: Selector '[] Bool
preservesLivePhotoCaptureSuspendedOnSessionStopSelector = mkSelector "preservesLivePhotoCaptureSuspendedOnSessionStop"

-- | @Selector@ for @setPreservesLivePhotoCaptureSuspendedOnSessionStop:@
setPreservesLivePhotoCaptureSuspendedOnSessionStopSelector :: Selector '[Bool] ()
setPreservesLivePhotoCaptureSuspendedOnSessionStopSelector = mkSelector "setPreservesLivePhotoCaptureSuspendedOnSessionStop:"

-- | @Selector@ for @livePhotoAutoTrimmingEnabled@
livePhotoAutoTrimmingEnabledSelector :: Selector '[] Bool
livePhotoAutoTrimmingEnabledSelector = mkSelector "livePhotoAutoTrimmingEnabled"

-- | @Selector@ for @setLivePhotoAutoTrimmingEnabled:@
setLivePhotoAutoTrimmingEnabledSelector :: Selector '[Bool] ()
setLivePhotoAutoTrimmingEnabledSelector = mkSelector "setLivePhotoAutoTrimmingEnabled:"

-- | @Selector@ for @availableLivePhotoVideoCodecTypes@
availableLivePhotoVideoCodecTypesSelector :: Selector '[] (Id NSArray)
availableLivePhotoVideoCodecTypesSelector = mkSelector "availableLivePhotoVideoCodecTypes"

-- | @Selector@ for @contentAwareDistortionCorrectionSupported@
contentAwareDistortionCorrectionSupportedSelector :: Selector '[] Bool
contentAwareDistortionCorrectionSupportedSelector = mkSelector "contentAwareDistortionCorrectionSupported"

-- | @Selector@ for @contentAwareDistortionCorrectionEnabled@
contentAwareDistortionCorrectionEnabledSelector :: Selector '[] Bool
contentAwareDistortionCorrectionEnabledSelector = mkSelector "contentAwareDistortionCorrectionEnabled"

-- | @Selector@ for @setContentAwareDistortionCorrectionEnabled:@
setContentAwareDistortionCorrectionEnabledSelector :: Selector '[Bool] ()
setContentAwareDistortionCorrectionEnabledSelector = mkSelector "setContentAwareDistortionCorrectionEnabled:"

-- | @Selector@ for @zeroShutterLagSupported@
zeroShutterLagSupportedSelector :: Selector '[] Bool
zeroShutterLagSupportedSelector = mkSelector "zeroShutterLagSupported"

-- | @Selector@ for @zeroShutterLagEnabled@
zeroShutterLagEnabledSelector :: Selector '[] Bool
zeroShutterLagEnabledSelector = mkSelector "zeroShutterLagEnabled"

-- | @Selector@ for @setZeroShutterLagEnabled:@
setZeroShutterLagEnabledSelector :: Selector '[Bool] ()
setZeroShutterLagEnabledSelector = mkSelector "setZeroShutterLagEnabled:"

-- | @Selector@ for @responsiveCaptureSupported@
responsiveCaptureSupportedSelector :: Selector '[] Bool
responsiveCaptureSupportedSelector = mkSelector "responsiveCaptureSupported"

-- | @Selector@ for @responsiveCaptureEnabled@
responsiveCaptureEnabledSelector :: Selector '[] Bool
responsiveCaptureEnabledSelector = mkSelector "responsiveCaptureEnabled"

-- | @Selector@ for @setResponsiveCaptureEnabled:@
setResponsiveCaptureEnabledSelector :: Selector '[Bool] ()
setResponsiveCaptureEnabledSelector = mkSelector "setResponsiveCaptureEnabled:"

-- | @Selector@ for @captureReadiness@
captureReadinessSelector :: Selector '[] AVCapturePhotoOutputCaptureReadiness
captureReadinessSelector = mkSelector "captureReadiness"

-- | @Selector@ for @constantColorSupported@
constantColorSupportedSelector :: Selector '[] Bool
constantColorSupportedSelector = mkSelector "constantColorSupported"

-- | @Selector@ for @constantColorEnabled@
constantColorEnabledSelector :: Selector '[] Bool
constantColorEnabledSelector = mkSelector "constantColorEnabled"

-- | @Selector@ for @setConstantColorEnabled:@
setConstantColorEnabledSelector :: Selector '[Bool] ()
setConstantColorEnabledSelector = mkSelector "setConstantColorEnabled:"

-- | @Selector@ for @shutterSoundSuppressionSupported@
shutterSoundSuppressionSupportedSelector :: Selector '[] Bool
shutterSoundSuppressionSupportedSelector = mkSelector "shutterSoundSuppressionSupported"

-- | @Selector@ for @cameraSensorOrientationCompensationSupported@
cameraSensorOrientationCompensationSupportedSelector :: Selector '[] Bool
cameraSensorOrientationCompensationSupportedSelector = mkSelector "cameraSensorOrientationCompensationSupported"

-- | @Selector@ for @cameraSensorOrientationCompensationEnabled@
cameraSensorOrientationCompensationEnabledSelector :: Selector '[] Bool
cameraSensorOrientationCompensationEnabledSelector = mkSelector "cameraSensorOrientationCompensationEnabled"

-- | @Selector@ for @setCameraSensorOrientationCompensationEnabled:@
setCameraSensorOrientationCompensationEnabledSelector :: Selector '[Bool] ()
setCameraSensorOrientationCompensationEnabledSelector = mkSelector "setCameraSensorOrientationCompensationEnabled:"

-- | @Selector@ for @depthDataDeliverySupported@
depthDataDeliverySupportedSelector :: Selector '[] Bool
depthDataDeliverySupportedSelector = mkSelector "depthDataDeliverySupported"

-- | @Selector@ for @depthDataDeliveryEnabled@
depthDataDeliveryEnabledSelector :: Selector '[] Bool
depthDataDeliveryEnabledSelector = mkSelector "depthDataDeliveryEnabled"

-- | @Selector@ for @setDepthDataDeliveryEnabled:@
setDepthDataDeliveryEnabledSelector :: Selector '[Bool] ()
setDepthDataDeliveryEnabledSelector = mkSelector "setDepthDataDeliveryEnabled:"

-- | @Selector@ for @portraitEffectsMatteDeliverySupported@
portraitEffectsMatteDeliverySupportedSelector :: Selector '[] Bool
portraitEffectsMatteDeliverySupportedSelector = mkSelector "portraitEffectsMatteDeliverySupported"

-- | @Selector@ for @portraitEffectsMatteDeliveryEnabled@
portraitEffectsMatteDeliveryEnabledSelector :: Selector '[] Bool
portraitEffectsMatteDeliveryEnabledSelector = mkSelector "portraitEffectsMatteDeliveryEnabled"

-- | @Selector@ for @setPortraitEffectsMatteDeliveryEnabled:@
setPortraitEffectsMatteDeliveryEnabledSelector :: Selector '[Bool] ()
setPortraitEffectsMatteDeliveryEnabledSelector = mkSelector "setPortraitEffectsMatteDeliveryEnabled:"

-- | @Selector@ for @availableSemanticSegmentationMatteTypes@
availableSemanticSegmentationMatteTypesSelector :: Selector '[] (Id NSArray)
availableSemanticSegmentationMatteTypesSelector = mkSelector "availableSemanticSegmentationMatteTypes"

-- | @Selector@ for @enabledSemanticSegmentationMatteTypes@
enabledSemanticSegmentationMatteTypesSelector :: Selector '[] (Id NSArray)
enabledSemanticSegmentationMatteTypesSelector = mkSelector "enabledSemanticSegmentationMatteTypes"

-- | @Selector@ for @setEnabledSemanticSegmentationMatteTypes:@
setEnabledSemanticSegmentationMatteTypesSelector :: Selector '[Id NSArray] ()
setEnabledSemanticSegmentationMatteTypesSelector = mkSelector "setEnabledSemanticSegmentationMatteTypes:"

