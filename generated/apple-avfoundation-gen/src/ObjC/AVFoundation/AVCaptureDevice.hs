{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | AVCaptureDevice
--
-- An AVCaptureDevice represents a physical device that provides realtime input media data, such as video and audio.
--
-- Each instance of AVCaptureDevice corresponds to a device, such as a camera or microphone. Instances of AVCaptureDevice cannot be created directly. An array of all currently available devices can also be obtained using the AVCaptureDeviceDiscoverySession. Devices can provide one or more streams of a given media type. Applications can search for devices matching desired criteria by using AVCaptureDeviceDiscoverySession, or may obtain a reference to the default device matching desired criteria by using +[AVCaptureDevice defaultDeviceWithDeviceType:mediaType:position:].
--
-- Instances of AVCaptureDevice can be used to provide media data to an AVCaptureSession by creating an AVCaptureDeviceInput with the device and adding that to the capture session.
--
-- Generated bindings for @AVCaptureDevice@.
module ObjC.AVFoundation.AVCaptureDevice
  ( AVCaptureDevice
  , IsAVCaptureDevice(..)
  , init_
  , new
  , devices
  , devicesWithMediaType
  , defaultDeviceWithMediaType
  , deviceWithUniqueID
  , hasMediaType
  , lockForConfiguration
  , unlockForConfiguration
  , supportsAVCaptureSessionPreset
  , setDynamicAspectRatio_completionHandler
  , showSystemUserInterface
  , performEffectForReaction
  , extrinsicMatrixFromDevice_toDevice
  , setTransportControlsPlaybackMode_speed
  , authorizationStatusForMediaType
  , requestAccessForMediaType_completionHandler
  , rampToVideoZoomFactor_withRate
  , cancelVideoZoomRamp
  , isWhiteBalanceModeSupported
  , isExposureModeSupported
  , setExposureTargetBias_completionHandler
  , isFocusModeSupported
  , setFocusModeLockedWithLensPosition_completionHandler
  , setCinematicVideoTrackingFocusWithDetectedObjectID_focusMode
  , isTorchModeSupported
  , setTorchModeOnWithLevel_error
  , isFlashModeSupported
  , setPrimaryConstituentDeviceSwitchingBehavior_restrictedSwitchingBehaviorConditions
  , defaultDeviceWithDeviceType_mediaType_position
  , uniqueID
  , modelID
  , localizedName
  , manufacturer
  , transportType
  , connected
  , inUseByAnotherApplication
  , suspended
  , linkedDevices
  , formats
  , activeFormat
  , setActiveFormat
  , videoFrameDurationLocked
  , followingExternalSyncDevice
  , autoVideoFrameRateEnabled
  , setAutoVideoFrameRateEnabled
  , inputSources
  , activeInputSource
  , setActiveInputSource
  , cameraLensSmudgeDetectionEnabled
  , cameraLensSmudgeDetectionStatus
  , edgeLightEnabled
  , edgeLightActive
  , studioLightEnabled
  , studioLightActive
  , nominalFocalLengthIn35mmFilm
  , smartFramingMonitor
  , dynamicAspectRatio
  , cinematicVideoCaptureSceneMonitoringStatuses
  , spatialCaptureDiscomfortReasons
  , preferredMicrophoneMode
  , activeMicrophoneMode
  , companionDeskViewCamera
  , continuityCamera
  , backgroundReplacementEnabled
  , backgroundReplacementActive
  , reactionEffectsEnabled
  , reactionEffectGesturesEnabled
  , canPerformReactionEffects
  , availableReactionTypes
  , reactionEffectsInProgress
  , portraitEffectEnabled
  , portraitEffectActive
  , centerStageControlMode
  , setCenterStageControlMode
  , centerStageEnabled
  , setCenterStageEnabled
  , centerStageActive
  , centerStageRectOfInterestSupported
  , geometricDistortionCorrectionSupported
  , geometricDistortionCorrectionEnabled
  , setGeometricDistortionCorrectionEnabled
  , activeDepthDataFormat
  , setActiveDepthDataFormat
  , minAvailableVideoZoomFactor
  , maxAvailableVideoZoomFactor
  , activeColorSpace
  , setActiveColorSpace
  , automaticallyAdjustsVideoHDREnabled
  , setAutomaticallyAdjustsVideoHDREnabled
  , videoHDREnabled
  , setVideoHDREnabled
  , transportControlsSupported
  , transportControlsPlaybackMode
  , transportControlsSpeed
  , videoZoomFactor
  , setVideoZoomFactor
  , rampingVideoZoom
  , dualCameraSwitchOverVideoZoomFactor
  , displayVideoZoomFactorMultiplier
  , lowLightBoostSupported
  , lowLightBoostEnabled
  , automaticallyEnablesLowLightBoostWhenAvailable
  , setAutomaticallyEnablesLowLightBoostWhenAvailable
  , subjectAreaChangeMonitoringEnabled
  , setSubjectAreaChangeMonitoringEnabled
  , lockingWhiteBalanceWithCustomDeviceGainsSupported
  , whiteBalanceMode
  , setWhiteBalanceMode
  , adjustingWhiteBalance
  , maxWhiteBalanceGain
  , globalToneMappingEnabled
  , setGlobalToneMappingEnabled
  , exposureMode
  , setExposureMode
  , exposurePointOfInterestSupported
  , exposureRectOfInterestSupported
  , automaticallyAdjustsFaceDrivenAutoExposureEnabled
  , setAutomaticallyAdjustsFaceDrivenAutoExposureEnabled
  , faceDrivenAutoExposureEnabled
  , setFaceDrivenAutoExposureEnabled
  , adjustingExposure
  , lensAperture
  , iso
  , exposureTargetOffset
  , exposureTargetBias
  , minExposureTargetBias
  , maxExposureTargetBias
  , lockingFocusWithCustomLensPositionSupported
  , focusMode
  , setFocusMode
  , focusPointOfInterestSupported
  , focusRectOfInterestSupported
  , adjustingFocus
  , autoFocusRangeRestrictionSupported
  , autoFocusRangeRestriction
  , setAutoFocusRangeRestriction
  , smoothAutoFocusSupported
  , smoothAutoFocusEnabled
  , setSmoothAutoFocusEnabled
  , automaticallyAdjustsFaceDrivenAutoFocusEnabled
  , setAutomaticallyAdjustsFaceDrivenAutoFocusEnabled
  , faceDrivenAutoFocusEnabled
  , setFaceDrivenAutoFocusEnabled
  , lensPosition
  , minimumFocusDistance
  , hasTorch
  , torchAvailable
  , torchActive
  , torchLevel
  , torchMode
  , setTorchMode
  , hasFlash
  , flashAvailable
  , flashActive
  , flashMode
  , setFlashMode
  , virtualDevice
  , constituentDevices
  , virtualDeviceSwitchOverVideoZoomFactors
  , primaryConstituentDeviceSwitchingBehavior
  , primaryConstituentDeviceRestrictedSwitchingBehaviorConditions
  , activePrimaryConstituentDeviceSwitchingBehavior
  , activePrimaryConstituentDeviceRestrictedSwitchingBehaviorConditions
  , activePrimaryConstituentDevice
  , supportedFallbackPrimaryConstituentDevices
  , fallbackPrimaryConstituentDevices
  , setFallbackPrimaryConstituentDevices
  , systemPressureState
  , userPreferredCamera
  , setUserPreferredCamera
  , systemPreferredCamera
  , deviceType
  , position
  , activeColorSpaceSelector
  , activeDepthDataFormatSelector
  , activeFormatSelector
  , activeInputSourceSelector
  , activeMicrophoneModeSelector
  , activePrimaryConstituentDeviceRestrictedSwitchingBehaviorConditionsSelector
  , activePrimaryConstituentDeviceSelector
  , activePrimaryConstituentDeviceSwitchingBehaviorSelector
  , adjustingExposureSelector
  , adjustingFocusSelector
  , adjustingWhiteBalanceSelector
  , authorizationStatusForMediaTypeSelector
  , autoFocusRangeRestrictionSelector
  , autoFocusRangeRestrictionSupportedSelector
  , autoVideoFrameRateEnabledSelector
  , automaticallyAdjustsFaceDrivenAutoExposureEnabledSelector
  , automaticallyAdjustsFaceDrivenAutoFocusEnabledSelector
  , automaticallyAdjustsVideoHDREnabledSelector
  , automaticallyEnablesLowLightBoostWhenAvailableSelector
  , availableReactionTypesSelector
  , backgroundReplacementActiveSelector
  , backgroundReplacementEnabledSelector
  , cameraLensSmudgeDetectionEnabledSelector
  , cameraLensSmudgeDetectionStatusSelector
  , canPerformReactionEffectsSelector
  , cancelVideoZoomRampSelector
  , centerStageActiveSelector
  , centerStageControlModeSelector
  , centerStageEnabledSelector
  , centerStageRectOfInterestSupportedSelector
  , cinematicVideoCaptureSceneMonitoringStatusesSelector
  , companionDeskViewCameraSelector
  , connectedSelector
  , constituentDevicesSelector
  , continuityCameraSelector
  , defaultDeviceWithDeviceType_mediaType_positionSelector
  , defaultDeviceWithMediaTypeSelector
  , deviceTypeSelector
  , deviceWithUniqueIDSelector
  , devicesSelector
  , devicesWithMediaTypeSelector
  , displayVideoZoomFactorMultiplierSelector
  , dualCameraSwitchOverVideoZoomFactorSelector
  , dynamicAspectRatioSelector
  , edgeLightActiveSelector
  , edgeLightEnabledSelector
  , exposureModeSelector
  , exposurePointOfInterestSupportedSelector
  , exposureRectOfInterestSupportedSelector
  , exposureTargetBiasSelector
  , exposureTargetOffsetSelector
  , extrinsicMatrixFromDevice_toDeviceSelector
  , faceDrivenAutoExposureEnabledSelector
  , faceDrivenAutoFocusEnabledSelector
  , fallbackPrimaryConstituentDevicesSelector
  , flashActiveSelector
  , flashAvailableSelector
  , flashModeSelector
  , focusModeSelector
  , focusPointOfInterestSupportedSelector
  , focusRectOfInterestSupportedSelector
  , followingExternalSyncDeviceSelector
  , formatsSelector
  , geometricDistortionCorrectionEnabledSelector
  , geometricDistortionCorrectionSupportedSelector
  , globalToneMappingEnabledSelector
  , hasFlashSelector
  , hasMediaTypeSelector
  , hasTorchSelector
  , inUseByAnotherApplicationSelector
  , initSelector
  , inputSourcesSelector
  , isExposureModeSupportedSelector
  , isFlashModeSupportedSelector
  , isFocusModeSupportedSelector
  , isTorchModeSupportedSelector
  , isWhiteBalanceModeSupportedSelector
  , isoSelector
  , lensApertureSelector
  , lensPositionSelector
  , linkedDevicesSelector
  , localizedNameSelector
  , lockForConfigurationSelector
  , lockingFocusWithCustomLensPositionSupportedSelector
  , lockingWhiteBalanceWithCustomDeviceGainsSupportedSelector
  , lowLightBoostEnabledSelector
  , lowLightBoostSupportedSelector
  , manufacturerSelector
  , maxAvailableVideoZoomFactorSelector
  , maxExposureTargetBiasSelector
  , maxWhiteBalanceGainSelector
  , minAvailableVideoZoomFactorSelector
  , minExposureTargetBiasSelector
  , minimumFocusDistanceSelector
  , modelIDSelector
  , newSelector
  , nominalFocalLengthIn35mmFilmSelector
  , performEffectForReactionSelector
  , portraitEffectActiveSelector
  , portraitEffectEnabledSelector
  , positionSelector
  , preferredMicrophoneModeSelector
  , primaryConstituentDeviceRestrictedSwitchingBehaviorConditionsSelector
  , primaryConstituentDeviceSwitchingBehaviorSelector
  , rampToVideoZoomFactor_withRateSelector
  , rampingVideoZoomSelector
  , reactionEffectGesturesEnabledSelector
  , reactionEffectsEnabledSelector
  , reactionEffectsInProgressSelector
  , requestAccessForMediaType_completionHandlerSelector
  , setActiveColorSpaceSelector
  , setActiveDepthDataFormatSelector
  , setActiveFormatSelector
  , setActiveInputSourceSelector
  , setAutoFocusRangeRestrictionSelector
  , setAutoVideoFrameRateEnabledSelector
  , setAutomaticallyAdjustsFaceDrivenAutoExposureEnabledSelector
  , setAutomaticallyAdjustsFaceDrivenAutoFocusEnabledSelector
  , setAutomaticallyAdjustsVideoHDREnabledSelector
  , setAutomaticallyEnablesLowLightBoostWhenAvailableSelector
  , setCenterStageControlModeSelector
  , setCenterStageEnabledSelector
  , setCinematicVideoTrackingFocusWithDetectedObjectID_focusModeSelector
  , setDynamicAspectRatio_completionHandlerSelector
  , setExposureModeSelector
  , setExposureTargetBias_completionHandlerSelector
  , setFaceDrivenAutoExposureEnabledSelector
  , setFaceDrivenAutoFocusEnabledSelector
  , setFallbackPrimaryConstituentDevicesSelector
  , setFlashModeSelector
  , setFocusModeLockedWithLensPosition_completionHandlerSelector
  , setFocusModeSelector
  , setGeometricDistortionCorrectionEnabledSelector
  , setGlobalToneMappingEnabledSelector
  , setPrimaryConstituentDeviceSwitchingBehavior_restrictedSwitchingBehaviorConditionsSelector
  , setSmoothAutoFocusEnabledSelector
  , setSubjectAreaChangeMonitoringEnabledSelector
  , setTorchModeOnWithLevel_errorSelector
  , setTorchModeSelector
  , setTransportControlsPlaybackMode_speedSelector
  , setUserPreferredCameraSelector
  , setVideoHDREnabledSelector
  , setVideoZoomFactorSelector
  , setWhiteBalanceModeSelector
  , showSystemUserInterfaceSelector
  , smartFramingMonitorSelector
  , smoothAutoFocusEnabledSelector
  , smoothAutoFocusSupportedSelector
  , spatialCaptureDiscomfortReasonsSelector
  , studioLightActiveSelector
  , studioLightEnabledSelector
  , subjectAreaChangeMonitoringEnabledSelector
  , supportedFallbackPrimaryConstituentDevicesSelector
  , supportsAVCaptureSessionPresetSelector
  , suspendedSelector
  , systemPreferredCameraSelector
  , systemPressureStateSelector
  , torchActiveSelector
  , torchAvailableSelector
  , torchLevelSelector
  , torchModeSelector
  , transportControlsPlaybackModeSelector
  , transportControlsSpeedSelector
  , transportControlsSupportedSelector
  , transportTypeSelector
  , uniqueIDSelector
  , unlockForConfigurationSelector
  , userPreferredCameraSelector
  , videoFrameDurationLockedSelector
  , videoHDREnabledSelector
  , videoZoomFactorSelector
  , virtualDeviceSelector
  , virtualDeviceSwitchOverVideoZoomFactorsSelector
  , whiteBalanceModeSelector

  -- * Enum types
  , AVAuthorizationStatus(AVAuthorizationStatus)
  , pattern AVAuthorizationStatusNotDetermined
  , pattern AVAuthorizationStatusRestricted
  , pattern AVAuthorizationStatusDenied
  , pattern AVAuthorizationStatusAuthorized
  , AVCaptureAutoFocusRangeRestriction(AVCaptureAutoFocusRangeRestriction)
  , pattern AVCaptureAutoFocusRangeRestrictionNone
  , pattern AVCaptureAutoFocusRangeRestrictionNear
  , pattern AVCaptureAutoFocusRangeRestrictionFar
  , AVCaptureCameraLensSmudgeDetectionStatus(AVCaptureCameraLensSmudgeDetectionStatus)
  , pattern AVCaptureCameraLensSmudgeDetectionStatusDisabled
  , pattern AVCaptureCameraLensSmudgeDetectionStatusSmudgeNotDetected
  , pattern AVCaptureCameraLensSmudgeDetectionStatusSmudged
  , pattern AVCaptureCameraLensSmudgeDetectionStatusUnknown
  , AVCaptureCenterStageControlMode(AVCaptureCenterStageControlMode)
  , pattern AVCaptureCenterStageControlModeUser
  , pattern AVCaptureCenterStageControlModeApp
  , pattern AVCaptureCenterStageControlModeCooperative
  , AVCaptureCinematicVideoFocusMode(AVCaptureCinematicVideoFocusMode)
  , pattern AVCaptureCinematicVideoFocusModeNone
  , pattern AVCaptureCinematicVideoFocusModeStrong
  , pattern AVCaptureCinematicVideoFocusModeWeak
  , AVCaptureColorSpace(AVCaptureColorSpace)
  , pattern AVCaptureColorSpace_sRGB
  , pattern AVCaptureColorSpace_P3_D65
  , pattern AVCaptureColorSpace_HLG_BT2020
  , pattern AVCaptureColorSpace_AppleLog
  , pattern AVCaptureColorSpace_AppleLog2
  , AVCaptureDevicePosition(AVCaptureDevicePosition)
  , pattern AVCaptureDevicePositionUnspecified
  , pattern AVCaptureDevicePositionBack
  , pattern AVCaptureDevicePositionFront
  , AVCaptureDeviceTransportControlsPlaybackMode(AVCaptureDeviceTransportControlsPlaybackMode)
  , pattern AVCaptureDeviceTransportControlsNotPlayingMode
  , pattern AVCaptureDeviceTransportControlsPlayingMode
  , AVCaptureExposureMode(AVCaptureExposureMode)
  , pattern AVCaptureExposureModeLocked
  , pattern AVCaptureExposureModeAutoExpose
  , pattern AVCaptureExposureModeContinuousAutoExposure
  , pattern AVCaptureExposureModeCustom
  , AVCaptureFlashMode(AVCaptureFlashMode)
  , pattern AVCaptureFlashModeOff
  , pattern AVCaptureFlashModeOn
  , pattern AVCaptureFlashModeAuto
  , AVCaptureFocusMode(AVCaptureFocusMode)
  , pattern AVCaptureFocusModeLocked
  , pattern AVCaptureFocusModeAutoFocus
  , pattern AVCaptureFocusModeContinuousAutoFocus
  , AVCaptureMicrophoneMode(AVCaptureMicrophoneMode)
  , pattern AVCaptureMicrophoneModeStandard
  , pattern AVCaptureMicrophoneModeWideSpectrum
  , pattern AVCaptureMicrophoneModeVoiceIsolation
  , AVCapturePrimaryConstituentDeviceRestrictedSwitchingBehaviorConditions(AVCapturePrimaryConstituentDeviceRestrictedSwitchingBehaviorConditions)
  , pattern AVCapturePrimaryConstituentDeviceRestrictedSwitchingBehaviorConditionNone
  , pattern AVCapturePrimaryConstituentDeviceRestrictedSwitchingBehaviorConditionVideoZoomChanged
  , pattern AVCapturePrimaryConstituentDeviceRestrictedSwitchingBehaviorConditionFocusModeChanged
  , pattern AVCapturePrimaryConstituentDeviceRestrictedSwitchingBehaviorConditionExposureModeChanged
  , AVCapturePrimaryConstituentDeviceSwitchingBehavior(AVCapturePrimaryConstituentDeviceSwitchingBehavior)
  , pattern AVCapturePrimaryConstituentDeviceSwitchingBehaviorUnsupported
  , pattern AVCapturePrimaryConstituentDeviceSwitchingBehaviorAuto
  , pattern AVCapturePrimaryConstituentDeviceSwitchingBehaviorRestricted
  , pattern AVCapturePrimaryConstituentDeviceSwitchingBehaviorLocked
  , AVCaptureSystemUserInterface(AVCaptureSystemUserInterface)
  , pattern AVCaptureSystemUserInterfaceVideoEffects
  , pattern AVCaptureSystemUserInterfaceMicrophoneModes
  , AVCaptureTorchMode(AVCaptureTorchMode)
  , pattern AVCaptureTorchModeOff
  , pattern AVCaptureTorchModeOn
  , pattern AVCaptureTorchModeAuto
  , AVCaptureWhiteBalanceMode(AVCaptureWhiteBalanceMode)
  , pattern AVCaptureWhiteBalanceModeLocked
  , pattern AVCaptureWhiteBalanceModeAutoWhiteBalance
  , pattern AVCaptureWhiteBalanceModeContinuousAutoWhiteBalance

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
init_ :: IsAVCaptureDevice avCaptureDevice => avCaptureDevice -> IO (Id AVCaptureDevice)
init_ avCaptureDevice =
  sendOwnedMessage avCaptureDevice initSelector

-- | @+ new@
new :: IO (Id AVCaptureDevice)
new  =
  do
    cls' <- getRequiredClass "AVCaptureDevice"
    sendOwnedClassMessage cls' newSelector

-- | devices
--
-- Returns an array of devices currently available for use as media input sources.
--
-- Returns: An NSArray of AVCaptureDevice instances for each available device.
--
-- This method returns an array of AVCaptureDevice instances for input devices currently connected and available for capture. The returned array contains all devices that are available at the time the method is called. Applications should observe AVCaptureDeviceWasConnectedNotification and AVCaptureDeviceWasDisconnectedNotification to be notified when the list of available devices has changed.
--
-- ObjC selector: @+ devices@
devices :: IO (Id NSArray)
devices  =
  do
    cls' <- getRequiredClass "AVCaptureDevice"
    sendClassMessage cls' devicesSelector

-- | devicesWithMediaType:
--
-- Returns an array of devices currently available for use as sources of media with the given media type.
--
-- @mediaType@ — The media type, such as AVMediaTypeVideo, AVMediaTypeAudio, or AVMediaTypeMuxed, supported by each returned device.
--
-- Returns: An NSArray of AVCaptureDevice instances for each available device.
--
-- This method returns an array of AVCaptureDevice instances for input devices currently connected and available for capture that provide media of the given type. Media type constants are defined in AVMediaFormat.h. The returned array contains all devices that are available at the time the method is called. Applications should observe AVCaptureDeviceWasConnectedNotification and AVCaptureDeviceWasDisconnectedNotification to be notified when the list of available devices has changed.
--
-- ObjC selector: @+ devicesWithMediaType:@
devicesWithMediaType :: IsNSString mediaType => mediaType -> IO (Id NSArray)
devicesWithMediaType mediaType =
  do
    cls' <- getRequiredClass "AVCaptureDevice"
    sendClassMessage cls' devicesWithMediaTypeSelector (toNSString mediaType)

-- | defaultDeviceWithMediaType:
--
-- Returns an AVCaptureDevice instance for the default device of the given media type.
--
-- @mediaType@ — The media type, such as AVMediaTypeVideo, AVMediaTypeAudio, or AVMediaTypeMuxed, supported by the returned device.
--
-- Returns: The default device with the given media type, or nil if no device with that media type exists.
--
-- This method returns the default device of the given media type currently available on the system. For example, for AVMediaTypeVideo, this method will return the built in camera that is primarily used for capture and recording. Media type constants are defined in AVMediaFormat.h.
--
-- ObjC selector: @+ defaultDeviceWithMediaType:@
defaultDeviceWithMediaType :: IsNSString mediaType => mediaType -> IO (Id AVCaptureDevice)
defaultDeviceWithMediaType mediaType =
  do
    cls' <- getRequiredClass "AVCaptureDevice"
    sendClassMessage cls' defaultDeviceWithMediaTypeSelector (toNSString mediaType)

-- | deviceWithUniqueID:
--
-- Returns an AVCaptureDevice instance with the given unique ID.
--
-- @deviceUniqueID@ — The unique ID of the device instance to be returned.
--
-- Returns: An AVCaptureDevice instance with the given unique ID, or nil if no device with that unique ID is available.
--
-- Every available capture device has a unique ID that persists on one system across device connections and disconnections, application restarts, and reboots of the system itself. This method can be used to recall or track the status of a specific device whose unique ID has previously been saved.
--
-- ObjC selector: @+ deviceWithUniqueID:@
deviceWithUniqueID :: IsNSString deviceUniqueID => deviceUniqueID -> IO (Id AVCaptureDevice)
deviceWithUniqueID deviceUniqueID =
  do
    cls' <- getRequiredClass "AVCaptureDevice"
    sendClassMessage cls' deviceWithUniqueIDSelector (toNSString deviceUniqueID)

-- | hasMediaType:
--
-- Returns whether the receiver provides media with the given media type.
--
-- @mediaType@ — A media type, such as AVMediaTypeVideo, AVMediaTypeAudio, or AVMediaTypeMuxed.
--
-- Returns: YES if the device outputs the given media type, NO otherwise.
--
-- Media type constants are defined in AVMediaFormat.h.
--
-- ObjC selector: @- hasMediaType:@
hasMediaType :: (IsAVCaptureDevice avCaptureDevice, IsNSString mediaType) => avCaptureDevice -> mediaType -> IO Bool
hasMediaType avCaptureDevice mediaType =
  sendMessage avCaptureDevice hasMediaTypeSelector (toNSString mediaType)

-- | lockForConfiguration:
--
-- Requests exclusive access to configure device hardware properties.
--
-- @outError@ — On return, if the device could not be locked, points to an NSError describing why the failure occurred.
--
-- Returns: A BOOL indicating whether the device was successfully locked for configuration.
--
-- In order to set hardware properties on an AVCaptureDevice, such as focusMode and exposureMode, clients must first acquire a lock on the device. Clients should only hold the device lock if they require settable device properties to remain unchanged. Holding the device lock unnecessarily may degrade capture quality in other applications sharing the device.
--
-- ObjC selector: @- lockForConfiguration:@
lockForConfiguration :: (IsAVCaptureDevice avCaptureDevice, IsNSError outError) => avCaptureDevice -> outError -> IO Bool
lockForConfiguration avCaptureDevice outError =
  sendMessage avCaptureDevice lockForConfigurationSelector (toNSError outError)

-- | unlockForConfiguration
--
-- Release exclusive control over device hardware properties.
--
-- This method should be called to match an invocation of lockForConfiguration: when an application no longer needs to keep device hardware properties from changing automatically.
--
-- ObjC selector: @- unlockForConfiguration@
unlockForConfiguration :: IsAVCaptureDevice avCaptureDevice => avCaptureDevice -> IO ()
unlockForConfiguration avCaptureDevice =
  sendMessage avCaptureDevice unlockForConfigurationSelector

-- | supportsAVCaptureSessionPreset:
--
-- Returns whether the receiver can be used in an AVCaptureSession configured with the given preset.
--
-- @preset@ — An AVCaptureSession preset.
--
-- Returns: YES if the receiver can be used with the given preset, NO otherwise.
--
-- An AVCaptureSession instance can be associated with a preset that configures its inputs and outputs to fulfill common use cases. This method can be used to determine if the receiver can be used in a capture session with the given preset. Presets are defined in AVCaptureSession.h.
--
-- ObjC selector: @- supportsAVCaptureSessionPreset:@
supportsAVCaptureSessionPreset :: (IsAVCaptureDevice avCaptureDevice, IsNSString preset) => avCaptureDevice -> preset -> IO Bool
supportsAVCaptureSessionPreset avCaptureDevice preset =
  sendMessage avCaptureDevice supportsAVCaptureSessionPresetSelector (toNSString preset)

-- | Updates the dynamic aspect ratio of the device.
--
-- - Parameter dynamicAspectRatio: The new ``AVCaptureAspectRatio`` the device should output. - Parameter handler: A block called by the device when @dynamicAspectRatio@ is set to the value specified. If you call ``setDynamicAspectRatio:completionHandler:`` multiple times, the completion handlers are called in FIFO order. The block receives a timestamp which matches that of the first buffer to which all settings have been applied. Note that the timestamp is synchronized to the device clock, and thus must be converted to the ``AVCaptureSession/synchronizationClock`` prior to comparison with the timestamps of buffers delivered via an ``AVCaptureVideoDataOutput``. You may pass @nil@ for the @handler@ parameter if you do not need to know when the operation completes.
--
-- This is the only way of setting ``dynamicAspectRatio``. This method throws an @NSInvalidArgumentException@ if @dynamicAspectRatio@ is not a supported aspect ratio found in the device's activeFormat's ``AVCaptureDeviceFormat/supportedDynamicAspectRatios``. This method throws an @NSGenericException@ if you call it without first obtaining exclusive access to the device using ``AVCaptureDevice/lockForConfiguration:``.
--
-- ObjC selector: @- setDynamicAspectRatio:completionHandler:@
setDynamicAspectRatio_completionHandler :: (IsAVCaptureDevice avCaptureDevice, IsNSString dynamicAspectRatio) => avCaptureDevice -> dynamicAspectRatio -> Ptr () -> IO ()
setDynamicAspectRatio_completionHandler avCaptureDevice dynamicAspectRatio handler =
  sendMessage avCaptureDevice setDynamicAspectRatio_completionHandlerSelector (toNSString dynamicAspectRatio) handler

-- | showSystemUserInterface:
--
-- Displays the system's user interface for video effects or microphone modes.
--
-- @systemUserInterface@ — The system UI to show.
--
-- This method allows the calling application to prompt the user to make changes to Video Effects (such as Center Stage or the Portrait Effect) or Microphone Modes. It brings up the system user interface and deep links to the appropriate module. This method is non-blocking. After presenting the desired system user interface, control returns immediately to the application.
--
-- ObjC selector: @+ showSystemUserInterface:@
showSystemUserInterface :: AVCaptureSystemUserInterface -> IO ()
showSystemUserInterface systemUserInterface =
  do
    cls' <- getRequiredClass "AVCaptureDevice"
    sendClassMessage cls' showSystemUserInterfaceSelector systemUserInterface

-- | performEffectForReaction:
--
-- Triggers a specified reaction on the video stream.
--
-- @reactionType@ — Indicates which reaction to perform.
--
-- The entries in reactionEffectsInProgress may not reflect one-to-one against calls to this method. Depending on reaction style or resource limits, triggering multiple overlapping reactions of the same type may be coalesced into extending an existing reaction rather than overlaying a new one.
--
-- The reactionType requested must be one of those listed in availableReactionTypes or an exception will be thrown. Performing a reaction when canPerformReactionEffects is NO is ignored, and VoIP applications are encouraged to transmit and display such reactions outside of the video feed.
--
-- ObjC selector: @- performEffectForReaction:@
performEffectForReaction :: (IsAVCaptureDevice avCaptureDevice, IsNSString reactionType) => avCaptureDevice -> reactionType -> IO ()
performEffectForReaction avCaptureDevice reactionType =
  sendMessage avCaptureDevice performEffectForReactionSelector (toNSString reactionType)

-- | extrinsicMatrixFromDevice:toDevice:
--
-- An NSData containing the relative extrinsic matrix from one AVCaptureDevice to another.
--
-- @fromDevice@ — The AVCaptureDevice to use as the source. Must be non nil or an NSInvalidArgumentException is thrown.
--
-- @toDevice@ — The AVCaptureDevice to use as the destination. Must be non nil or an NSInvalidArgumentException is thrown.
--
-- The extrinsic matrix consists of a unitless 3x3 rotation matrix (R) on the left and a translation (t) 3x1 column vector on the right. The translation vector's units are millimeters. The extrinsics of the "toDevice" camera are expressed with respect to a reference camera "fromDevice". If X_from is a 3D point in "fromCamera"'s coordinate system, then it can be projected into "toCamera"'s coordinate system with X_to = [R | t] * X_from. Note that a matrix_float4x3 matrix is column major with 3 rows and 4 columns. The extrinsicMatrix is only provided for physical cameras for which factory calibrations exist. Virtual device cameras return nil.               /                       \\       /   \\   | r1,1  r1,2  r1,3 | t1 |       |R|t| = | r2,1  r2,2  r2,3 | t2 |       \\   /   | r3,1  r3,2  r3,3 | t3 |               \\                       /
--
-- Note that if you enable video stabilization (see AVCaptureConnection.preferredVideoStabilizationMode), the pixels in stabilized video frames no longer match the relative extrinsicMatrix from one device to another due to warping. The extrinsicMatrix and camera intrinsics should only be used when video stabilization is disabled.
--
-- ObjC selector: @+ extrinsicMatrixFromDevice:toDevice:@
extrinsicMatrixFromDevice_toDevice :: (IsAVCaptureDevice fromDevice, IsAVCaptureDevice toDevice) => fromDevice -> toDevice -> IO (Id NSData)
extrinsicMatrixFromDevice_toDevice fromDevice toDevice =
  do
    cls' <- getRequiredClass "AVCaptureDevice"
    sendClassMessage cls' extrinsicMatrixFromDevice_toDeviceSelector (toAVCaptureDevice fromDevice) (toAVCaptureDevice toDevice)

-- | setTransportControlsPlaybackMode:speed:
--
-- Sets both the transport controls playback mode and speed in a single method.
--
-- @mode@ — A AVCaptureDeviceTransportControlsPlaybackMode indicating whether the deck should be put into play mode.
--
-- @speed@ — A AVCaptureDeviceTransportControlsSpeed indicating the speed at which to wind or play the tape.
--
-- A method for setting the receiver's transport controls playback mode and speed. The receiver must be locked for configuration using lockForConfiguration: before clients can set this method, otherwise an NSGenericException is thrown.
--
-- ObjC selector: @- setTransportControlsPlaybackMode:speed:@
setTransportControlsPlaybackMode_speed :: IsAVCaptureDevice avCaptureDevice => avCaptureDevice -> AVCaptureDeviceTransportControlsPlaybackMode -> CFloat -> IO ()
setTransportControlsPlaybackMode_speed avCaptureDevice mode speed =
  sendMessage avCaptureDevice setTransportControlsPlaybackMode_speedSelector mode speed

-- | authorizationStatusForMediaType:
--
-- Returns the client's authorization status for accessing the underlying hardware that supports a given media type.
--
-- @mediaType@ — The media type, either AVMediaTypeVideo or AVMediaTypeAudio
--
-- Returns: The authorization status of the client
--
-- This method returns the AVAuthorizationStatus of the client for accessing the underlying hardware supporting the media type. Media type constants are defined in AVMediaFormat.h. If any media type other than AVMediaTypeVideo or AVMediaTypeAudio is supplied, an NSInvalidArgumentException will be thrown. If the status is AVAuthorizationStatusNotDetermined, you may use the +requestAccessForMediaType:completionHandler: method to request access by prompting the user.
--
-- ObjC selector: @+ authorizationStatusForMediaType:@
authorizationStatusForMediaType :: IsNSString mediaType => mediaType -> IO AVAuthorizationStatus
authorizationStatusForMediaType mediaType =
  do
    cls' <- getRequiredClass "AVCaptureDevice"
    sendClassMessage cls' authorizationStatusForMediaTypeSelector (toNSString mediaType)

-- | requestAccessForMediaType:completionHandler:
--
-- Requests access to the underlying hardware for the media type, showing a dialog to the user if necessary.
--
-- @mediaType@ — The media type, either AVMediaTypeVideo or AVMediaTypeAudio
--
-- @handler@ — A block called with the result of requesting access
--
-- Use this function to request access to the hardware for a given media type. Media type constants are defined in AVMediaFormat.h. If any media type other than AVMediaTypeVideo or AVMediaTypeAudio is supplied, an NSInvalidArgumentException will be thrown.
--
-- This call will not block while the user is being asked for access, allowing the client to continue running. Until access has been granted, any AVCaptureDevices for the media type will vend silent audio samples or black video frames. The user is only asked for permission the first time the client requests access. Later calls use the permission granted by the user.
--
-- Note that the authorization dialog will automatically be shown if the status is AVAuthorizationStatusNotDetermined when creating an AVCaptureDeviceInput.
--
-- Invoking this method with AVMediaTypeAudio is equivalent to calling -[AVAudioSession requestRecordPermission:].
--
-- The completion handler is called on an arbitrary dispatch queue. It is the client's responsibility to ensure that any UIKit-related updates are called on the main queue or main thread as a result.
--
-- ObjC selector: @+ requestAccessForMediaType:completionHandler:@
requestAccessForMediaType_completionHandler :: IsNSString mediaType => mediaType -> Ptr () -> IO ()
requestAccessForMediaType_completionHandler mediaType handler =
  do
    cls' <- getRequiredClass "AVCaptureDevice"
    sendClassMessage cls' requestAccessForMediaType_completionHandlerSelector (toNSString mediaType) handler

-- | rampToVideoZoomFactor:withRate:
--
-- Provides smooth changes in zoom factor.
--
-- This method provides a change in zoom by compounding magnification at the specified rate over time. Although the zoom factor will grow exponentially, this yields a visually linear zoom in the image over time.
--
-- The zoom transition will stop at the specified factor, which must be in the valid range for videoZoomFactor. Assignments to videoZoomFactor while a ramp is in progress will cancel the ramp and snap to the assigned value.
--
-- The zoom factor is continuously scaled by pow(2,rate * time). A rate of 0 causes no change in zoom factor, equivalent to calling cancelVideoZoomRamp. A rate of 1 will cause the magnification to double every second (or halve every second if zooming out), and similarly larger or smaller values will zoom faster or slower respectively. Only the absolute value of the rate is significant--sign is corrected for the direction of the target. Changes in rate will be smoothed by an internal acceleration limit.
--
-- When depth data delivery is enabled, -rampToVideoZoomFactor:withRate: sets the videoZoomFactor to the nearest supportedVideoZoomFactor from -[AVCaptureDeviceFormat supportedVideoZoomFactorsForDepthDataDelivery] with a disruptive reconfiguration of the capture render pipeline.
--
-- -rampToVideoZoomFactor:withRate: throws an NSGenericException if called without first obtaining exclusive access to the receiver using lockForConfiguration:.
--
-- ObjC selector: @- rampToVideoZoomFactor:withRate:@
rampToVideoZoomFactor_withRate :: IsAVCaptureDevice avCaptureDevice => avCaptureDevice -> CDouble -> CFloat -> IO ()
rampToVideoZoomFactor_withRate avCaptureDevice factor rate =
  sendMessage avCaptureDevice rampToVideoZoomFactor_withRateSelector factor rate

-- | cancelVideoZoomRamp
--
-- Eases out of any video zoom transitions initiated by rampToVideoZoomFactor:withRate:
--
-- This method is equivalent to calling rampToVideoZoomFactor:withRate: using the current zoom factor target and a rate of 0. This allows a smooth stop to any changes in zoom which were in progress.
--
-- -cancelVideoZoomRamp: throws an NSGenericException if called without first obtaining exclusive access to the receiver using lockForConfiguration:.
--
-- ObjC selector: @- cancelVideoZoomRamp@
cancelVideoZoomRamp :: IsAVCaptureDevice avCaptureDevice => avCaptureDevice -> IO ()
cancelVideoZoomRamp avCaptureDevice =
  sendMessage avCaptureDevice cancelVideoZoomRampSelector

-- | isWhiteBalanceModeSupported:
--
-- Returns whether the receiver supports the given white balance mode.
--
-- @whiteBalanceMode@ — An AVCaptureWhiteBalanceMode to be checked.
--
-- Returns: YES if the receiver supports the given white balance mode, NO otherwise.
--
-- The receiver's whiteBalanceMode property can only be set to a certain mode if this method returns YES for that mode.
--
-- ObjC selector: @- isWhiteBalanceModeSupported:@
isWhiteBalanceModeSupported :: IsAVCaptureDevice avCaptureDevice => avCaptureDevice -> AVCaptureWhiteBalanceMode -> IO Bool
isWhiteBalanceModeSupported avCaptureDevice whiteBalanceMode =
  sendMessage avCaptureDevice isWhiteBalanceModeSupportedSelector whiteBalanceMode

-- | isExposureModeSupported:
--
-- Returns whether the receiver supports the given exposure mode.
--
-- @exposureMode@ — An AVCaptureExposureMode to be checked.
--
-- Returns: YES if the receiver supports the given exposure mode, NO otherwise.
--
-- The receiver's exposureMode property can only be set to a certain mode if this method returns YES for that mode.
--
-- ObjC selector: @- isExposureModeSupported:@
isExposureModeSupported :: IsAVCaptureDevice avCaptureDevice => avCaptureDevice -> AVCaptureExposureMode -> IO Bool
isExposureModeSupported avCaptureDevice exposureMode =
  sendMessage avCaptureDevice isExposureModeSupportedSelector exposureMode

-- | setExposureTargetBias:completionHandler:
--
-- Sets the bias to be applied to the target exposure value.
--
-- @bias@ — The bias to be applied to the exposure target value, as described in the documentation for the exposureTargetBias property.
--
-- @handler@ — A block to be called when exposureTargetBias has been set to the value specified. If setExposureTargetBias:completionHandler: is called multiple times, the completion handlers will be called in FIFO order. The block receives a timestamp which matches that of the first buffer to which the setting has been applied. Note that the timestamp is synchronized to the device clock, and thus must be converted to the @AVCaptureSession/synchronizationClock@ prior to comparison with the timestamps of buffers delivered via an AVCaptureVideoDataOutput. The client may pass nil for the handler parameter if knowledge of the operation's completion is not required.
--
-- This is the only way of setting exposureTargetBias. This method throws an NSRangeException if exposureTargetBias is set to an unsupported level. This method throws an NSGenericException if called without first obtaining exclusive access to the receiver using lockForConfiguration:.
--
-- ObjC selector: @- setExposureTargetBias:completionHandler:@
setExposureTargetBias_completionHandler :: IsAVCaptureDevice avCaptureDevice => avCaptureDevice -> CFloat -> Ptr () -> IO ()
setExposureTargetBias_completionHandler avCaptureDevice bias handler =
  sendMessage avCaptureDevice setExposureTargetBias_completionHandlerSelector bias handler

-- | isFocusModeSupported:
--
-- Returns whether the receiver supports the given focus mode.
--
-- @focusMode@ — An AVCaptureFocusMode to be checked.
--
-- Returns: YES if the receiver supports the given focus mode, NO otherwise.
--
-- The receiver's focusMode property can only be set to a certain mode if this method returns YES for that mode.
--
-- ObjC selector: @- isFocusModeSupported:@
isFocusModeSupported :: IsAVCaptureDevice avCaptureDevice => avCaptureDevice -> AVCaptureFocusMode -> IO Bool
isFocusModeSupported avCaptureDevice focusMode =
  sendMessage avCaptureDevice isFocusModeSupportedSelector focusMode

-- | setFocusModeLockedWithLensPosition:completionHandler:
--
-- Sets focusMode to AVCaptureFocusModeLocked and locks lensPosition at an explicit value.
--
-- @lensPosition@ — The lens position, as described in the documentation for the lensPosition property. A value of AVCaptureLensPositionCurrent can be used to indicate that the caller does not wish to specify a value for lensPosition.
--
-- @handler@ — A block to be called when lensPosition has been set to the value specified and focusMode is set to AVCaptureFocusModeLocked. If setFocusModeLockedWithLensPosition:completionHandler: is called multiple times, the completion handlers will be called in FIFO order. The block receives a timestamp which matches that of the first buffer to which all settings have been applied. Note that the timestamp is synchronized to the device clock, and thus must be converted to the @AVCaptureSession/synchronizationClock@ prior to comparison with the timestamps of buffers delivered via an AVCaptureVideoDataOutput. The client may pass nil for the handler parameter if knowledge of the operation's completion is not required.
--
-- This is the only way of setting lensPosition. This method throws an NSRangeException if lensPosition is set to an unsupported level. This method throws an NSGenericException if called without first obtaining exclusive access to the receiver using lockForConfiguration:.
--
-- ObjC selector: @- setFocusModeLockedWithLensPosition:completionHandler:@
setFocusModeLockedWithLensPosition_completionHandler :: IsAVCaptureDevice avCaptureDevice => avCaptureDevice -> CFloat -> Ptr () -> IO ()
setFocusModeLockedWithLensPosition_completionHandler avCaptureDevice lensPosition handler =
  sendMessage avCaptureDevice setFocusModeLockedWithLensPosition_completionHandlerSelector lensPosition handler

-- | Focus on and start tracking a detected object.
--
-- - Parameter detectedObjectID: The ID of the detected object. - Parameter focusMode: Specify whether to focus strongly or weakly.
--
-- ObjC selector: @- setCinematicVideoTrackingFocusWithDetectedObjectID:focusMode:@
setCinematicVideoTrackingFocusWithDetectedObjectID_focusMode :: IsAVCaptureDevice avCaptureDevice => avCaptureDevice -> CLong -> AVCaptureCinematicVideoFocusMode -> IO ()
setCinematicVideoTrackingFocusWithDetectedObjectID_focusMode avCaptureDevice detectedObjectID focusMode =
  sendMessage avCaptureDevice setCinematicVideoTrackingFocusWithDetectedObjectID_focusModeSelector detectedObjectID focusMode

-- | isTorchModeSupported:
--
-- Returns whether the receiver supports the given torch mode.
--
-- @torchMode@ — An AVCaptureTorchMode to be checked.
--
-- Returns: YES if the receiver supports the given torch mode, NO otherwise.
--
-- The receiver's torchMode property can only be set to a certain mode if this method returns YES for that mode.
--
-- ObjC selector: @- isTorchModeSupported:@
isTorchModeSupported :: IsAVCaptureDevice avCaptureDevice => avCaptureDevice -> AVCaptureTorchMode -> IO Bool
isTorchModeSupported avCaptureDevice torchMode =
  sendMessage avCaptureDevice isTorchModeSupportedSelector torchMode

-- | setTorchModeOnWithLevel:error:
--
-- Sets the current mode of the receiver's torch to AVCaptureTorchModeOn at the specified level.
--
-- This method sets the torch mode to AVCaptureTorchModeOn at a specified level. torchLevel must be a value between 0 and 1, or the special value AVCaptureMaxAvailableTorchLevel. The specified value may not be available if the iOS device is too hot. This method throws an NSInvalidArgumentException if set to an unsupported level. If the specified level is valid, but unavailable, the method returns NO with AVErrorTorchLevelUnavailable. -setTorchModeOnWithLevel:error: throws an NSGenericException if called without first obtaining exclusive access to the receiver using lockForConfiguration:. Clients can observe automatic changes to the receiver's torchMode by key value observing the torchMode property.
--
-- ObjC selector: @- setTorchModeOnWithLevel:error:@
setTorchModeOnWithLevel_error :: (IsAVCaptureDevice avCaptureDevice, IsNSError outError) => avCaptureDevice -> CFloat -> outError -> IO Bool
setTorchModeOnWithLevel_error avCaptureDevice torchLevel outError =
  sendMessage avCaptureDevice setTorchModeOnWithLevel_errorSelector torchLevel (toNSError outError)

-- | isFlashModeSupported:
--
-- Returns whether the receiver supports the given flash mode.
--
-- @flashMode@ — An AVCaptureFlashMode to be checked.
--
-- Returns: YES if the receiver supports the given flash mode, NO otherwise.
--
-- The receiver's flashMode property can only be set to a certain mode if this method returns YES for that mode.
--
-- ObjC selector: @- isFlashModeSupported:@
isFlashModeSupported :: IsAVCaptureDevice avCaptureDevice => avCaptureDevice -> AVCaptureFlashMode -> IO Bool
isFlashModeSupported avCaptureDevice flashMode =
  sendMessage avCaptureDevice isFlashModeSupportedSelector flashMode

-- | setPrimaryConstituentDeviceSwitchingBehavior:restrictedSwitchingBehaviorConditions:
--
-- The switching behavior and conditions, unless overwritten via -[AVCaptureMovieFileOutput setPrimaryConstituentDeviceSwitchingBehavior:restrictedSwitchingBehaviorConditions].
--
-- @switchingBehavior@ — The desired switching behavior.
--
-- @restrictedSwitchingBehaviorConditions@ — The desired conditions for restricting camera switching. This must be set to AVCapturePrimaryConstituentDeviceRestrictedSwitchingBehaviorConditionNone whenever switchingBehavior is not equal to AVCapturePrimaryConstituentDeviceSwitchingBehaviorRestricted.
--
-- The switching behavior may be overridden on the AVCaptureMovieFileOutput while recording (see -[AVCaptureMovieFileOutput setPrimaryConstituentDeviceSwitchingBehavior:restrictedSwitchingBehaviorConditions]). This method throws an NSInvalidArgumentException if constituent device switching is not supported by the receiver or if restrictedSwitchingBehaviorConditions is not equal to AVCapturePrimaryConstituentDeviceRestrictedSwitchingBehaviorConditionNone and switchingBehavior is not equal to AVCapturePrimaryConstituentDeviceSwitchingBehaviorRestricted.
--
-- ObjC selector: @- setPrimaryConstituentDeviceSwitchingBehavior:restrictedSwitchingBehaviorConditions:@
setPrimaryConstituentDeviceSwitchingBehavior_restrictedSwitchingBehaviorConditions :: IsAVCaptureDevice avCaptureDevice => avCaptureDevice -> AVCapturePrimaryConstituentDeviceSwitchingBehavior -> AVCapturePrimaryConstituentDeviceRestrictedSwitchingBehaviorConditions -> IO ()
setPrimaryConstituentDeviceSwitchingBehavior_restrictedSwitchingBehaviorConditions avCaptureDevice switchingBehavior restrictedSwitchingBehaviorConditions =
  sendMessage avCaptureDevice setPrimaryConstituentDeviceSwitchingBehavior_restrictedSwitchingBehaviorConditionsSelector switchingBehavior restrictedSwitchingBehaviorConditions

-- | defaultDeviceWithDeviceType:mediaType:position:
--
-- Returns an AVCaptureDevice instance for the default device of the given device type, media type, and position.
--
-- @deviceType@ — The device type supported by the returned device. It must be a valid AVCaptureDeviceType.
--
-- @mediaType@ — The media type, such as AVMediaTypeVideo, AVMediaTypeAudio, or AVMediaTypeMuxed, supported by the returned device. Pass nil to consider devices with any media type.
--
-- @position@ — The position supported by the returned device. Pass AVCaptureDevicePositionUnspecified to consider devices with any position.
--
-- Returns: The default device with the given device type, media type and position or nil if no device with that media type exists and nil otherwise.
--
-- This method returns the default device of the given combination of device type, media type, and position currently available on the system.
--
-- ObjC selector: @+ defaultDeviceWithDeviceType:mediaType:position:@
defaultDeviceWithDeviceType_mediaType_position :: (IsNSString deviceType, IsNSString mediaType) => deviceType -> mediaType -> AVCaptureDevicePosition -> IO (Id AVCaptureDevice)
defaultDeviceWithDeviceType_mediaType_position deviceType mediaType position =
  do
    cls' <- getRequiredClass "AVCaptureDevice"
    sendClassMessage cls' defaultDeviceWithDeviceType_mediaType_positionSelector (toNSString deviceType) (toNSString mediaType) position

-- | uniqueID
--
-- An ID unique to the model of device corresponding to the receiver.
--
-- Every available capture device has a unique ID that persists on one system across device connections and disconnections, application restarts, and reboots of the system itself. Applications can store the value returned by this property to recall or track the status of a specific device in the future.
--
-- ObjC selector: @- uniqueID@
uniqueID :: IsAVCaptureDevice avCaptureDevice => avCaptureDevice -> IO (Id NSString)
uniqueID avCaptureDevice =
  sendMessage avCaptureDevice uniqueIDSelector

-- | modelID
--
-- The model ID of the receiver.
--
-- The value of this property is an identifier unique to all devices of the same model. The value is persistent across device connections and disconnections, and across different systems. For example, the model ID of the camera built in to two identical iPhone models will be the same even though they are different physical devices.
--
-- ObjC selector: @- modelID@
modelID :: IsAVCaptureDevice avCaptureDevice => avCaptureDevice -> IO (Id NSString)
modelID avCaptureDevice =
  sendMessage avCaptureDevice modelIDSelector

-- | localizedName
--
-- A localized human-readable name for the receiver.
--
-- This property can be used for displaying the name of a capture device in a user interface.
--
-- ObjC selector: @- localizedName@
localizedName :: IsAVCaptureDevice avCaptureDevice => avCaptureDevice -> IO (Id NSString)
localizedName avCaptureDevice =
  sendMessage avCaptureDevice localizedNameSelector

-- | manufacturer
--
-- The human-readable manufacturer name for the receiver.
--
-- This property can be used to identify capture devices from a particular manufacturer. All Apple devices return "Apple Inc.". Devices from third party manufacturers may return an empty string.
--
-- ObjC selector: @- manufacturer@
manufacturer :: IsAVCaptureDevice avCaptureDevice => avCaptureDevice -> IO (Id NSString)
manufacturer avCaptureDevice =
  sendMessage avCaptureDevice manufacturerSelector

-- | transportType
--
-- The transport type of the receiver (e.g. USB, PCI, etc).
--
-- This property can be used to discover the transport type of a capture device. Transport types are defined in <IOKit/audio/IOAudioTypes.h> as kIOAudioDeviceTransportType*.
--
-- ObjC selector: @- transportType@
transportType :: IsAVCaptureDevice avCaptureDevice => avCaptureDevice -> IO CInt
transportType avCaptureDevice =
  sendMessage avCaptureDevice transportTypeSelector

-- | connected
--
-- Indicates whether the device is connected and available to the system.
--
-- The value of this property is a BOOL indicating whether the device represented by the receiver is connected and available for use as a capture device. Clients can key value observe the value of this property to be notified when a device is no longer available. When the value of this property becomes NO for a given instance, it will not become YES again. If the same physical device again becomes available to the system, it will be represented using a new instance of AVCaptureDevice.
--
-- ObjC selector: @- connected@
connected :: IsAVCaptureDevice avCaptureDevice => avCaptureDevice -> IO Bool
connected avCaptureDevice =
  sendMessage avCaptureDevice connectedSelector

-- | inUseByAnotherApplication
--
-- Indicates whether the device is in use by another application.
--
-- The value of this property is a BOOL indicating whether the device represented by the receiver is in use by another application. Clients can key value observe the value of this property to be notified when another app starts or stops using this device.
--
-- ObjC selector: @- inUseByAnotherApplication@
inUseByAnotherApplication :: IsAVCaptureDevice avCaptureDevice => avCaptureDevice -> IO Bool
inUseByAnotherApplication avCaptureDevice =
  sendMessage avCaptureDevice inUseByAnotherApplicationSelector

-- | suspended
--
-- Indicates whether the device is suspended.
--
-- The value of this property is a BOOL indicating whether the device represented by the receiver is currently suspended. Some devices disallow data capture due to a feature on the device. For example, isSuspended returns YES for the external iSight when its privacy iris is closed, or for the internal iSight on a notebook when the notebook's display is closed. Clients can key value observe the value of this property to be notified when the device becomes suspended or unsuspended.
--
-- ObjC selector: @- suspended@
suspended :: IsAVCaptureDevice avCaptureDevice => avCaptureDevice -> IO Bool
suspended avCaptureDevice =
  sendMessage avCaptureDevice suspendedSelector

-- | linkedDevices
--
-- An array of AVCaptureDevice objects physically linked to the receiver.
--
-- The value of this property is an array of AVCaptureDevice objects that are a part of the same physical device as the receiver. For example, for the external iSight camera, linkedDevices returns an array containing an AVCaptureDevice for the external iSight microphone.
--
-- ObjC selector: @- linkedDevices@
linkedDevices :: IsAVCaptureDevice avCaptureDevice => avCaptureDevice -> IO (Id NSArray)
linkedDevices avCaptureDevice =
  sendMessage avCaptureDevice linkedDevicesSelector

-- | formats
--
-- An array of AVCaptureDeviceFormat objects supported by the receiver.
--
-- This property can be used to enumerate the formats natively supported by the receiver. The capture device's activeFormat property may be set to one of the formats in this array. Clients can observe automatic changes to the receiver's formats by key value observing this property.
--
-- ObjC selector: @- formats@
formats :: IsAVCaptureDevice avCaptureDevice => avCaptureDevice -> IO (Id NSArray)
formats avCaptureDevice =
  sendMessage avCaptureDevice formatsSelector

-- | activeFormat
--
-- The currently active format of the receiver.
--
-- This property can be used to get or set the currently active device format.
--
-- -setActiveFormat: throws an NSInvalidArgumentException if set to a format not present in the formats array.
--
-- -setActiveFormat: throws an NSGenericException if called without first obtaining exclusive access to the receiver using lockForConfiguration:.
--
-- Clients can observe automatic changes to the receiver's activeFormat by key value observing this property.
--
-- On iOS, use of AVCaptureDevice's setActiveFormat: and AVCaptureSession's setSessionPreset: are mutually exclusive. If you set a capture device's active format, the session to which it is attached changes its preset to AVCaptureSessionPresetInputPriority. Likewise if you set the AVCaptureSession's sessionPreset property, the session assumes control of its input devices, and configures their activeFormat appropriately. Note that audio devices do not expose any user-configurable formats on iOS. To configure audio input on iOS, you should use the AVAudioSession APIs instead (see AVAudioSession.h).
--
-- The activeFormat, activeVideoMinFrameDuration, and activeVideoMaxFrameDuration properties may be set simultaneously by using AVCaptureSession's begin/commitConfiguration methods:
--
-- [session beginConfiguration]; // the session to which the receiver's AVCaptureDeviceInput is added.    if ( [device lockForConfiguration:&error] ) {        [device setActiveFormat:newFormat];        [device setActiveVideoMinFrameDuration:newMinDuration];        [device setActiveVideoMaxFrameDuration:newMaxDuration];        [device unlockForConfiguration];    }    [session commitConfiguration]; // The new format and frame rates are applied together in commitConfiguration
--
-- Note that when configuring a session to use an active format intended for high resolution still photography and applying one or more of the following operations to an AVCaptureVideoDataOutput, the system may not meet the target framerate: zoom, orientation changes, format conversion.
--
-- ObjC selector: @- activeFormat@
activeFormat :: IsAVCaptureDevice avCaptureDevice => avCaptureDevice -> IO (Id AVCaptureDeviceFormat)
activeFormat avCaptureDevice =
  sendMessage avCaptureDevice activeFormatSelector

-- | activeFormat
--
-- The currently active format of the receiver.
--
-- This property can be used to get or set the currently active device format.
--
-- -setActiveFormat: throws an NSInvalidArgumentException if set to a format not present in the formats array.
--
-- -setActiveFormat: throws an NSGenericException if called without first obtaining exclusive access to the receiver using lockForConfiguration:.
--
-- Clients can observe automatic changes to the receiver's activeFormat by key value observing this property.
--
-- On iOS, use of AVCaptureDevice's setActiveFormat: and AVCaptureSession's setSessionPreset: are mutually exclusive. If you set a capture device's active format, the session to which it is attached changes its preset to AVCaptureSessionPresetInputPriority. Likewise if you set the AVCaptureSession's sessionPreset property, the session assumes control of its input devices, and configures their activeFormat appropriately. Note that audio devices do not expose any user-configurable formats on iOS. To configure audio input on iOS, you should use the AVAudioSession APIs instead (see AVAudioSession.h).
--
-- The activeFormat, activeVideoMinFrameDuration, and activeVideoMaxFrameDuration properties may be set simultaneously by using AVCaptureSession's begin/commitConfiguration methods:
--
-- [session beginConfiguration]; // the session to which the receiver's AVCaptureDeviceInput is added.    if ( [device lockForConfiguration:&error] ) {        [device setActiveFormat:newFormat];        [device setActiveVideoMinFrameDuration:newMinDuration];        [device setActiveVideoMaxFrameDuration:newMaxDuration];        [device unlockForConfiguration];    }    [session commitConfiguration]; // The new format and frame rates are applied together in commitConfiguration
--
-- Note that when configuring a session to use an active format intended for high resolution still photography and applying one or more of the following operations to an AVCaptureVideoDataOutput, the system may not meet the target framerate: zoom, orientation changes, format conversion.
--
-- ObjC selector: @- setActiveFormat:@
setActiveFormat :: (IsAVCaptureDevice avCaptureDevice, IsAVCaptureDeviceFormat value) => avCaptureDevice -> value -> IO ()
setActiveFormat avCaptureDevice value =
  sendMessage avCaptureDevice setActiveFormatSelector (toAVCaptureDeviceFormat value)

-- | Whether the device's video frame rate (expressed as a duration) is currently locked.
--
-- Returns @true@ when an ``AVCaptureDeviceInput`` associated with the device has its ``AVCaptureDeviceInput/activeLockedVideoFrameDuration`` property set to something other than @kCMTimeInvalid@. See ``AVCaptureDeviceInput/activeLockedVideoFrameDuration`` for more information on video frame duration locking.
--
-- ObjC selector: @- videoFrameDurationLocked@
videoFrameDurationLocked :: IsAVCaptureDevice avCaptureDevice => avCaptureDevice -> IO Bool
videoFrameDurationLocked avCaptureDevice =
  sendMessage avCaptureDevice videoFrameDurationLockedSelector

-- | Whether the device is following an external sync device.
--
-- See ``AVCaptureDeviceInput/followExternalSyncDevice:videoFrameDuration:delegate:`` for more information on external sync.
--
-- ObjC selector: @- followingExternalSyncDevice@
followingExternalSyncDevice :: IsAVCaptureDevice avCaptureDevice => avCaptureDevice -> IO Bool
followingExternalSyncDevice avCaptureDevice =
  sendMessage avCaptureDevice followingExternalSyncDeviceSelector

-- | Indicates whether the receiver should enable auto video frame rate.
--
-- When you enable this property, the device automatically adjusts the active frame rate, depending on light level. Under low light conditions, it decreases the frame rate to properly expose the scene. For formats with a maximum frame rate of 30 fps, the device switches the frame rate between 30 - 24. For formats with a maximum frame rate of 60 fps, the device switches the frame rate between 60 - 30 - 24.
--
-- Setting this property throws an @NSInvalidArgumentException@ if the active format's ``AVCaptureDeviceFormat/autoVideoFrameRateSupported`` returns @false@. When you change the device's active format, this property resets to its default value of @false@.
--
-- If you set this property to @true@, frame rate is under device control, and you may not set ``activeVideoMinFrameDuration`` or ``activeVideoMaxFrameDuration``. Doing so throws an @NSInvalidArgumentException@.
--
-- - Note: Setting this property to @true@ throws an @NSInvalidArgumentException@ if ``videoFrameDurationLocked`` or ``followingExternalSyncDevice`` are @true@.
--
-- ObjC selector: @- autoVideoFrameRateEnabled@
autoVideoFrameRateEnabled :: IsAVCaptureDevice avCaptureDevice => avCaptureDevice -> IO Bool
autoVideoFrameRateEnabled avCaptureDevice =
  sendMessage avCaptureDevice autoVideoFrameRateEnabledSelector

-- | Indicates whether the receiver should enable auto video frame rate.
--
-- When you enable this property, the device automatically adjusts the active frame rate, depending on light level. Under low light conditions, it decreases the frame rate to properly expose the scene. For formats with a maximum frame rate of 30 fps, the device switches the frame rate between 30 - 24. For formats with a maximum frame rate of 60 fps, the device switches the frame rate between 60 - 30 - 24.
--
-- Setting this property throws an @NSInvalidArgumentException@ if the active format's ``AVCaptureDeviceFormat/autoVideoFrameRateSupported`` returns @false@. When you change the device's active format, this property resets to its default value of @false@.
--
-- If you set this property to @true@, frame rate is under device control, and you may not set ``activeVideoMinFrameDuration`` or ``activeVideoMaxFrameDuration``. Doing so throws an @NSInvalidArgumentException@.
--
-- - Note: Setting this property to @true@ throws an @NSInvalidArgumentException@ if ``videoFrameDurationLocked`` or ``followingExternalSyncDevice`` are @true@.
--
-- ObjC selector: @- setAutoVideoFrameRateEnabled:@
setAutoVideoFrameRateEnabled :: IsAVCaptureDevice avCaptureDevice => avCaptureDevice -> Bool -> IO ()
setAutoVideoFrameRateEnabled avCaptureDevice value =
  sendMessage avCaptureDevice setAutoVideoFrameRateEnabledSelector value

-- | inputSources
--
-- An array of AVCaptureDeviceInputSource objects supported by the receiver.
--
-- Some devices can capture data from one of multiple data sources (different input jacks on the same audio device, for example). For devices with multiple possible data sources, inputSources can be used to enumerate the possible choices. Clients can observe automatic changes to the receiver's inputSources by key value observing this property.
--
-- ObjC selector: @- inputSources@
inputSources :: IsAVCaptureDevice avCaptureDevice => avCaptureDevice -> IO (Id NSArray)
inputSources avCaptureDevice =
  sendMessage avCaptureDevice inputSourcesSelector

-- | activeInputSource
--
-- The currently active input source of the receiver.
--
-- This property can be used to get or set the currently active device input source. -setActiveInputSource: throws an NSInvalidArgumentException if set to a value not present in the inputSources array. -setActiveInputSource: throws an NSGenericException if called without first obtaining exclusive access to the receiver using lockForConfiguration:. Clients can observe automatic changes to the receiver's activeInputSource by key value observing this property.
--
-- ObjC selector: @- activeInputSource@
activeInputSource :: IsAVCaptureDevice avCaptureDevice => avCaptureDevice -> IO (Id AVCaptureDeviceInputSource)
activeInputSource avCaptureDevice =
  sendMessage avCaptureDevice activeInputSourceSelector

-- | activeInputSource
--
-- The currently active input source of the receiver.
--
-- This property can be used to get or set the currently active device input source. -setActiveInputSource: throws an NSInvalidArgumentException if set to a value not present in the inputSources array. -setActiveInputSource: throws an NSGenericException if called without first obtaining exclusive access to the receiver using lockForConfiguration:. Clients can observe automatic changes to the receiver's activeInputSource by key value observing this property.
--
-- ObjC selector: @- setActiveInputSource:@
setActiveInputSource :: (IsAVCaptureDevice avCaptureDevice, IsAVCaptureDeviceInputSource value) => avCaptureDevice -> value -> IO ()
setActiveInputSource avCaptureDevice value =
  sendMessage avCaptureDevice setActiveInputSourceSelector (toAVCaptureDeviceInputSource value)

-- | Whether camera lens smudge detection is enabled.
--
-- You enable lens smudge detection by calling ``setCameraLensSmudgeDetectionEnabled:detectionInterval:``. By default, this property is returns @false@.
--
-- ObjC selector: @- cameraLensSmudgeDetectionEnabled@
cameraLensSmudgeDetectionEnabled :: IsAVCaptureDevice avCaptureDevice => avCaptureDevice -> IO Bool
cameraLensSmudgeDetectionEnabled avCaptureDevice =
  sendMessage avCaptureDevice cameraLensSmudgeDetectionEnabledSelector

-- | A value specifying the status of camera lens smudge detection.
--
-- During initial detection execution, ``cameraLensSmudgeDetectionStatus`` returns ``AVCaptureCameraLensSmudgeDetectionStatusUnknown`` until the detection result settles. Once a detection result is produced, ``cameraLensSmudgeDetectionStatus`` returns the most recent detection result. This property can be key-value observed.
--
-- ObjC selector: @- cameraLensSmudgeDetectionStatus@
cameraLensSmudgeDetectionStatus :: IsAVCaptureDevice avCaptureDevice => avCaptureDevice -> IO AVCaptureCameraLensSmudgeDetectionStatus
cameraLensSmudgeDetectionStatus avCaptureDevice =
  sendMessage avCaptureDevice cameraLensSmudgeDetectionStatusSelector

-- | A class property indicating whether the Edge Light feature is currently enabled in Control Center.
--
-- This readonly property changes to reflect the Edge Light state in Control Center. It is key-value observable.
--
-- ObjC selector: @+ edgeLightEnabled@
edgeLightEnabled :: IO Bool
edgeLightEnabled  =
  do
    cls' <- getRequiredClass "AVCaptureDevice"
    sendClassMessage cls' edgeLightEnabledSelector

-- | A class property indicating whether the edge light UI is actively being shown on a screen.
--
-- This readonly property reflects whether the edge light UI is actively being shown on a screen. It is key-value observable.
--
-- ObjC selector: @+ edgeLightActive@
edgeLightActive :: IO Bool
edgeLightActive  =
  do
    cls' <- getRequiredClass "AVCaptureDevice"
    sendClassMessage cls' edgeLightActiveSelector

-- | studioLightEnabled
--
-- A class property indicating whether the Studio Light feature is currently enabled in Control Center.
--
-- This property changes to reflect the Studio Light state in Control Center. It is key-value observable.  On iOS, Studio Light only applies to video conferencing apps by default (apps that use "voip" as one of their UIBackgroundModes). Non video conferencing apps may opt in for Studio Light by adding the following key to their Info.plist:        <key>NSCameraStudioLightEnabled</key>        <true/>
--
-- ObjC selector: @+ studioLightEnabled@
studioLightEnabled :: IO Bool
studioLightEnabled  =
  do
    cls' <- getRequiredClass "AVCaptureDevice"
    sendClassMessage cls' studioLightEnabledSelector

-- | studioLightActive
--
-- Indicates whether Studio Light is currently active on a particular AVCaptureDevice.
--
-- This readonly property returns YES when Studio Light is currently active on the receiver. When active, the subject's face is artificially lit to simulate the presence of a studio light near the camera.
--
-- ObjC selector: @- studioLightActive@
studioLightActive :: IsAVCaptureDevice avCaptureDevice => avCaptureDevice -> IO Bool
studioLightActive avCaptureDevice =
  sendMessage avCaptureDevice studioLightActiveSelector

-- | The nominal 35mm equivalent focal length of the capture device's lens.
--
-- This value represents a nominal measurement of the device's field of view, expressed as a 35mm equivalent focal length, measured diagonally. The value is similar to the @FocalLengthIn35mmFormat@ EXIF entry (see <doc://com.apple.documentation/documentation/imageio/kcgimagepropertyexiffocallenin35mmfilm>) for a photo captured using the device's format where ``AVCaptureDeviceFormat/highestPhotoQualitySupported`` is @true@ or when you've configured the session with the ``AVCaptureSessionPresetPhoto`` preset.
--
-- This property value is @0@ for virtual devices and external cameras.
--
-- ObjC selector: @- nominalFocalLengthIn35mmFilm@
nominalFocalLengthIn35mmFilm :: IsAVCaptureDevice avCaptureDevice => avCaptureDevice -> IO CFloat
nominalFocalLengthIn35mmFilm avCaptureDevice =
  sendMessage avCaptureDevice nominalFocalLengthIn35mmFilmSelector

-- | A monitor owned by the device that recommends an optimal framing based on the content in the scene.
--
-- An ultra wide camera device that supports dynamic aspect ratio configuration may also support "smart framing monitoring". If this property returns non @nil@, you may use it to listen for framing recommendations by configuring its ``AVCaptureSmartFramingMonitor/enabledFramings`` and calling ``AVCaptureSmartFramingMonitor/startMonitoringWithError:``. The smart framing monitor only makes recommendations when the current ``AVCaptureDevice/activeFormat`` supports smart framing (see ``AVCaptureDeviceFormat/smartFramingSupported``).
--
-- ObjC selector: @- smartFramingMonitor@
smartFramingMonitor :: IsAVCaptureDevice avCaptureDevice => avCaptureDevice -> IO (Id AVCaptureSmartFramingMonitor)
smartFramingMonitor avCaptureDevice =
  sendMessage avCaptureDevice smartFramingMonitorSelector

-- | A key-value observable property indicating the current aspect ratio for a device.
--
-- This property is initialized to the first ``AVCaptureAspectRatio`` listed in the device's activeFormat's ``AVCaptureDeviceFormat/supportedDynamicAspectRatios`` property. If the activeFormat's ``AVCaptureDeviceFormat/supportedDynamicAspectRatios`` is an empty array, this property returns nil.
--
-- ObjC selector: @- dynamicAspectRatio@
dynamicAspectRatio :: IsAVCaptureDevice avCaptureDevice => avCaptureDevice -> IO (Id NSString)
dynamicAspectRatio avCaptureDevice =
  sendMessage avCaptureDevice dynamicAspectRatioSelector

-- | The current scene monitoring statuses related to Cinematic Video capture.
--
-- Monitor this property via key-value observation to present a UI informing the user that they should reframe their scene for a better Cinematic Video experience ("scene is too dark").
--
-- ObjC selector: @- cinematicVideoCaptureSceneMonitoringStatuses@
cinematicVideoCaptureSceneMonitoringStatuses :: IsAVCaptureDevice avCaptureDevice => avCaptureDevice -> IO (Id NSSet)
cinematicVideoCaptureSceneMonitoringStatuses avCaptureDevice =
  sendMessage avCaptureDevice cinematicVideoCaptureSceneMonitoringStatusesSelector

-- | spatialCaptureDiscomfortReasons
--
-- Indicates whether or not the current environmental conditions are amenable to a spatial capture that is comfortable to view.
--
-- This property can be monitored in order to determine the presentation of UI elements to inform the user that they should reframe their scene for a more pleasing spatial capture ("subject is too close", "scene is too dark").
--
-- ObjC selector: @- spatialCaptureDiscomfortReasons@
spatialCaptureDiscomfortReasons :: IsAVCaptureDevice avCaptureDevice => avCaptureDevice -> IO (Id NSSet)
spatialCaptureDiscomfortReasons avCaptureDevice =
  sendMessage avCaptureDevice spatialCaptureDiscomfortReasonsSelector

-- | preferredMicrophoneMode
--
-- Indicates the microphone mode that has been selected by the user in Control Center.
--
-- This readonly property returns the microphone mode selected by the user in Control Center. It is key-value observable.
--
-- ObjC selector: @+ preferredMicrophoneMode@
preferredMicrophoneMode :: IO AVCaptureMicrophoneMode
preferredMicrophoneMode  =
  do
    cls' <- getRequiredClass "AVCaptureDevice"
    sendClassMessage cls' preferredMicrophoneModeSelector

-- | activeMicrophoneMode
--
-- Indicates the currently active microphone mode.
--
-- This readonly property returns the currently active microphone mode, which may differ from the preferredMicrophoneMode if the application's active audio route does not support the preferred microphone mode. This property is key-value observable.
--
-- ObjC selector: @+ activeMicrophoneMode@
activeMicrophoneMode :: IO AVCaptureMicrophoneMode
activeMicrophoneMode  =
  do
    cls' <- getRequiredClass "AVCaptureDevice"
    sendClassMessage cls' activeMicrophoneModeSelector

-- | companionDeskViewCamera
--
-- A reference to the Desk View Camera that is associated with and derived from this camera.
--
-- The companionDeskViewCamera property allows you to discover if the receiver has a paired Desk View Camera which derives its desk framing from the receiver's ultra wide frame. In the presence of multiple Continuity Cameras, this property allows you to pair a particular Continuity Camera with its associated Desk View Camera.
--
-- ObjC selector: @- companionDeskViewCamera@
companionDeskViewCamera :: IsAVCaptureDevice avCaptureDevice => avCaptureDevice -> IO (Id AVCaptureDevice)
companionDeskViewCamera avCaptureDevice =
  sendMessage avCaptureDevice companionDeskViewCameraSelector

-- | continuityCamera
--
-- A property that reports YES if the receiver is a Continuity Camera.
--
-- Access this property to discover if the receiver is a Continuity Camera (external iPhone webcam).
--
-- ObjC selector: @- continuityCamera@
continuityCamera :: IsAVCaptureDevice avCaptureDevice => avCaptureDevice -> IO Bool
continuityCamera avCaptureDevice =
  sendMessage avCaptureDevice continuityCameraSelector

-- | backgroundReplacementEnabled
--
-- A class property indicating whether the user has enabled the Background Replacement feature for this application.
--
-- ObjC selector: @+ backgroundReplacementEnabled@
backgroundReplacementEnabled :: IO Bool
backgroundReplacementEnabled  =
  do
    cls' <- getRequiredClass "AVCaptureDevice"
    sendClassMessage cls' backgroundReplacementEnabledSelector

-- | backgroundReplacementActive
--
-- Indicates whether Background Replacement is currently active on a particular AVCaptureDevice.
--
-- This property is key-value observable.
--
-- ObjC selector: @- backgroundReplacementActive@
backgroundReplacementActive :: IsAVCaptureDevice avCaptureDevice => avCaptureDevice -> IO Bool
backgroundReplacementActive avCaptureDevice =
  sendMessage avCaptureDevice backgroundReplacementActiveSelector

-- | reactionEffectsEnabled
--
-- A class property indicating whether the application is suitable for reaction effects, either by automatic gesture detection, or by calls to -[AVCaptureDevice performEffectForReaction:]. Reactions are only rendered when the device's activeFormat.reactionEffectsSupported is also YES, which will be reflected by canPerformReactionEffects when the feature is both enabled and supported.
--
-- On macOS, Reaction Effects are enabled by default for all applications. On iOS, Reaction Effects are enabled by default for video conferencing applications (apps that use "voip" as one of their UIBackgroundModes). Non video conferencing applications may opt in for Reaction Effects by adding the following key to their Info.plist:        <key>NSCameraReactionEffectsEnabled</key>        <true/>
--
-- ObjC selector: @+ reactionEffectsEnabled@
reactionEffectsEnabled :: IO Bool
reactionEffectsEnabled  =
  do
    cls' <- getRequiredClass "AVCaptureDevice"
    sendClassMessage cls' reactionEffectsEnabledSelector

-- | reactionEffectGesturesEnabled
--
-- A class property indicating whether gesture detection will trigger reaction effects on the video stream. Gesture detection will only run when the device's activeFormat.reactionEffectsSupported is also YES, which will be reflected by canPerformReactionEffects.
--
-- This property changes to reflect the Gestures state in Control Center. It is key-value observable. Clients can call performEffectForReaction: independently of whether gesture detection is enabled, reaction effects from either source will be intermixed.    By default, gesture detection is enabled.  As of iOS 17.4 and macOS 14.4, applications can control the default value of this property by adding the following key to their Info.plist:        <key>NSCameraReactionEffectGesturesEnabledDefault</key>    A value of true enables gesture detection and a value of false disables it, until such time that the user makes their own selection in Control Center.
--
-- ObjC selector: @+ reactionEffectGesturesEnabled@
reactionEffectGesturesEnabled :: IO Bool
reactionEffectGesturesEnabled  =
  do
    cls' <- getRequiredClass "AVCaptureDevice"
    sendClassMessage cls' reactionEffectGesturesEnabledSelector

-- | canPerformReactionEffects
--
-- Indicates whether reactions can be performed on a particular AVCaptureDevice. This requires reactionEffectsEnabled to be YES, as well as using a AVCaptureDeviceFormat with reactionEffectsSupported.
--
-- This readonly property returns YES when resources for reactions are available on the device instance. When YES, calls to performEffectForReaction: will render on the video feed, otherwise those calls are ignored. It is key-value observable.
--
-- ObjC selector: @- canPerformReactionEffects@
canPerformReactionEffects :: IsAVCaptureDevice avCaptureDevice => avCaptureDevice -> IO Bool
canPerformReactionEffects avCaptureDevice =
  sendMessage avCaptureDevice canPerformReactionEffectsSelector

-- | availableReactionTypes
--
-- Returns a list of reaction types which can be passed to performEffectForReaction.
--
-- The list may differ between devices, or be affected by changes to active format, and can be key-value observed.
--
-- ObjC selector: @- availableReactionTypes@
availableReactionTypes :: IsAVCaptureDevice avCaptureDevice => avCaptureDevice -> IO (Id NSSet)
availableReactionTypes avCaptureDevice =
  sendMessage avCaptureDevice availableReactionTypesSelector

-- | reactionEffectsInProgress
--
-- Contains an array of reaction effects that are currently being performed by the device, sorted by timestamp. If observing old and new values in the KVO callback, the reaction effects which are still running in the new array will have kCMTimeInvalid as their endTime property. Reaction effects which have ended will only be in the old array, and will have their endTime property set to the presentation time of the first frame where the reaction effect was no longer present.
--
-- Reaction effects which are triggered by either a call to performEffectForReaction: or by the automatic gesture detection will be reflected in this array. It is key-value observable to be notified when reaction effects begin or end.
--
-- ObjC selector: @- reactionEffectsInProgress@
reactionEffectsInProgress :: IsAVCaptureDevice avCaptureDevice => avCaptureDevice -> IO (Id NSArray)
reactionEffectsInProgress avCaptureDevice =
  sendMessage avCaptureDevice reactionEffectsInProgressSelector

-- | portraitEffectEnabled
--
-- A class property indicating whether the Portrait Effect feature is currently enabled in Control Center.
--
-- This property changes to reflect the Portrait Effect state in Control Center. It is key-value observable. On iOS, Portrait Effect only applies to video conferencing apps by default (apps that use "voip" as one of their UIBackgroundModes). Non video conferencing apps may opt in for the Portrait Effect by adding the following key to their Info.plist:        <key>NSCameraPortraitEffectEnabled</key>        <true/>
--
-- ObjC selector: @+ portraitEffectEnabled@
portraitEffectEnabled :: IO Bool
portraitEffectEnabled  =
  do
    cls' <- getRequiredClass "AVCaptureDevice"
    sendClassMessage cls' portraitEffectEnabledSelector

-- | portraitEffectActive
--
-- Indicates whether Portrait Effect is currently active for a particular AVCaptureDevice.
--
-- This readonly property returns YES when Portrait Effect is currently active on the receiver. When active, the device blurs the background, simulating a shallow depth of field effect. Certain restrictions come into play when Portrait Effect is active:        - The device's activeVideoMinFrameDuration and activeVideoMaxFrameDuration are limited (see AVCaptureDeviceFormat's videoFrameRateRangeForPortraitEffect).    Note that when +AVCaptureDevice.portraitEffectEnabled is YES, a particular AVCaptureDevice instance may return YES for this property, depending whether it supports the feature in its current configuration.    This property is key-value observable.
--
-- ObjC selector: @- portraitEffectActive@
portraitEffectActive :: IsAVCaptureDevice avCaptureDevice => avCaptureDevice -> IO Bool
portraitEffectActive avCaptureDevice =
  sendMessage avCaptureDevice portraitEffectActiveSelector

-- | centerStageControlMode
--
-- A class property indicating the current mode of Center Stage control (user, app, or cooperative).
--
-- This class property determines how the Center Stage feature is controlled. When set to the default value of AVCaptureCenterStageControlModeUser, centerStageEnabled may not be set programmatically and throws an NSInvalidArgumentException. In User mode, the feature may only be set by the user in Control Center. If you wish to take Center Stage control away from the user and exclusively enable / disable it programmatically, set this property to AVCaptureCenterStageControlModeApp. When under exclusive app control, Center Stage user control is disallowed (for instance, the toggle is grayed out in Control Center). If you wish to take control of Center Stage, but also cooperate with the user by listening for and appropriately reacting to their changes to the centerStageEnabled property, set this property to AVCaptureCenterStageControlModeCooperative. Note that in this mode, the onus is on you, the app developer, to honor user intent and conform your AVCaptureSession configuration to make Center Stage active (see the AVCaptureDevice instance property centerStageActive). In cooperative mode, the centerStageEnabled property may change at any time (such as when the user enables / disables the feature in Control Center).
--
-- ObjC selector: @+ centerStageControlMode@
centerStageControlMode :: IO AVCaptureCenterStageControlMode
centerStageControlMode  =
  do
    cls' <- getRequiredClass "AVCaptureDevice"
    sendClassMessage cls' centerStageControlModeSelector

-- | centerStageControlMode
--
-- A class property indicating the current mode of Center Stage control (user, app, or cooperative).
--
-- This class property determines how the Center Stage feature is controlled. When set to the default value of AVCaptureCenterStageControlModeUser, centerStageEnabled may not be set programmatically and throws an NSInvalidArgumentException. In User mode, the feature may only be set by the user in Control Center. If you wish to take Center Stage control away from the user and exclusively enable / disable it programmatically, set this property to AVCaptureCenterStageControlModeApp. When under exclusive app control, Center Stage user control is disallowed (for instance, the toggle is grayed out in Control Center). If you wish to take control of Center Stage, but also cooperate with the user by listening for and appropriately reacting to their changes to the centerStageEnabled property, set this property to AVCaptureCenterStageControlModeCooperative. Note that in this mode, the onus is on you, the app developer, to honor user intent and conform your AVCaptureSession configuration to make Center Stage active (see the AVCaptureDevice instance property centerStageActive). In cooperative mode, the centerStageEnabled property may change at any time (such as when the user enables / disables the feature in Control Center).
--
-- ObjC selector: @+ setCenterStageControlMode:@
setCenterStageControlMode :: AVCaptureCenterStageControlMode -> IO ()
setCenterStageControlMode value =
  do
    cls' <- getRequiredClass "AVCaptureDevice"
    sendClassMessage cls' setCenterStageControlModeSelector value

-- | centerStageEnabled
--
-- A class property indicating whether the Center Stage feature is currently enabled or disabled (such as in Control Center or programmatically via your app).
--
-- This property may only be set if centerStageControlMode is AVCaptureCenterStageControlModeApp or AVCaptureCenterStageControlModeCooperative, and otherwise throws an NSInvalidArgumentException. When centerStageControlMode is AVCaptureCenterStageControlModeUser or AVCaptureCenterStageControlModeCooperative, this property may change according to user desire (such as enabling / disabling the feature in Control Center), so you should key-value observe it.
--
-- ObjC selector: @+ centerStageEnabled@
centerStageEnabled :: IO Bool
centerStageEnabled  =
  do
    cls' <- getRequiredClass "AVCaptureDevice"
    sendClassMessage cls' centerStageEnabledSelector

-- | centerStageEnabled
--
-- A class property indicating whether the Center Stage feature is currently enabled or disabled (such as in Control Center or programmatically via your app).
--
-- This property may only be set if centerStageControlMode is AVCaptureCenterStageControlModeApp or AVCaptureCenterStageControlModeCooperative, and otherwise throws an NSInvalidArgumentException. When centerStageControlMode is AVCaptureCenterStageControlModeUser or AVCaptureCenterStageControlModeCooperative, this property may change according to user desire (such as enabling / disabling the feature in Control Center), so you should key-value observe it.
--
-- ObjC selector: @+ setCenterStageEnabled:@
setCenterStageEnabled :: Bool -> IO ()
setCenterStageEnabled value =
  do
    cls' <- getRequiredClass "AVCaptureDevice"
    sendClassMessage cls' setCenterStageEnabledSelector value

-- | centerStageActive
--
-- Indicates whether Center Stage is currently active on a particular AVCaptureDevice.
--
-- This readonly property returns YES when Center Stage is currently active on the receiver. When active, the camera automatically adjusts to keep people optimally framed within the field of view. The field of view may pan, tighten or widen as needed. Certain restrictions come into play when Center Stage is active:        - The device's minAvailableVideoZoomFactor and maxAvailableVideoZoomFactor become restricted (see AVCaptureDeviceFormat's videoMinZoomFactorForCenterStage and videoMaxZoomFactorForCenterStage).        - The device's activeVideoMinFrameDuration and activeVideoMaxFrameDuration are limited (see AVCaptureDeviceFormat's videoFrameRateRangeForCenterStage).    Center Stage may be enabled via user control or application control, depending on the current +AVCaptureDevice.centerStageControlMode. When +AVCaptureDevice.centerStageEnabled is YES, a particular AVCaptureDevice instance may return YES for this property, depending whether it supports the feature in its current configuration. Some device features are mutually exclusive to Center Stage:        - If depth data delivery is enabled on any output, such as AVCaptureDepthDataOutput, or -AVCapturePhotoOutput.depthDataDeliveryEnabled, Center Stage is deactivated.        - If geometricDistortionCorrectionSupported is YES, geometricDistortionCorrectionEnabled must also be YES, or Center Stage is deactivated.    This property is key-value observable.
--
-- ObjC selector: @- centerStageActive@
centerStageActive :: IsAVCaptureDevice avCaptureDevice => avCaptureDevice -> IO Bool
centerStageActive avCaptureDevice =
  sendMessage avCaptureDevice centerStageActiveSelector

-- | centerStageRectOfInterestSupported
--
-- Indicates whether the device supports the Center Stage Rect of Interest feature.
--
-- This property returns YES if the device supports Center Stage Rect of Interest.
--
-- ObjC selector: @- centerStageRectOfInterestSupported@
centerStageRectOfInterestSupported :: IsAVCaptureDevice avCaptureDevice => avCaptureDevice -> IO Bool
centerStageRectOfInterestSupported avCaptureDevice =
  sendMessage avCaptureDevice centerStageRectOfInterestSupportedSelector

-- | geometricDistortionCorrectionSupported
--
-- Indicates that geometric distortion correction is supported by the receiver.
--
-- Some AVCaptureDevices benefit from geometric distortion correction (GDC), such as devices with a very wide field of view. GDC lessens the fisheye effect at the outer edge of the frame at the cost of losing a small amount of vertical and horizontal field of view. When GDC is enabled on the AVCaptureDevice (see geometricDistortionCorrectionEnabled), the corrected image is upscaled to the original image size when needed.  With respect to the AVCaptureDevice.videoZoomFactor API, the full viewable field of view is always represented with a videoZoomFactor of 1.0. Thus, when GDC is enabled, the AVCaptureDevice.activeFormat's field of view at videoZoomFactor = 1.0 will be different than when GDC is disabled. The smaller field of view is reported through the activeFormat's geometricDistortionCorrectedVideoFieldOfView property. Beware though that RAW photo captures never have GDC applied, regardless of the value of AVCaptureDevice.geometricDistortionCorrectionEnabled.
--
-- ObjC selector: @- geometricDistortionCorrectionSupported@
geometricDistortionCorrectionSupported :: IsAVCaptureDevice avCaptureDevice => avCaptureDevice -> IO Bool
geometricDistortionCorrectionSupported avCaptureDevice =
  sendMessage avCaptureDevice geometricDistortionCorrectionSupportedSelector

-- | geometricDistortionCorrectionEnabled
--
-- Indicates whether geometric distortion correction is enabled by the receiver.
--
-- Where supported, the default value is YES. The receiver must be locked for configuration using lockForConfiguration: before clients can set this method, otherwise an NSGenericException is thrown.
--
-- In the case of ProRes RAW, when geometricDistortionCorrectionEnabled is YES, GDC is applied to your outputs in different ways:    - It is always applied to AVCaptureVideoPreviewLayer.    - It is applied to AVCaptureVideoDataOutput only if deliversPreviewSizedOutputBuffers is set to YES.    - It is never applied to AVCaptureMovieFileOutput.
--
-- When GDC is enabled, AVCaptureVideoDataOutput buffers contain GDC metadata attachments, and AVCaptureMovieFileOutput movies contain GDC metadata which an application supporting ProRes RAW can optionally apply at playback time using the ProRes RAW SDK. To learn more about the ProRes RAW SDK, refer to the Apple ProRes and ProRes RAW Authorized Products article at https://support.apple.com/en-us/118584.
--
-- ObjC selector: @- geometricDistortionCorrectionEnabled@
geometricDistortionCorrectionEnabled :: IsAVCaptureDevice avCaptureDevice => avCaptureDevice -> IO Bool
geometricDistortionCorrectionEnabled avCaptureDevice =
  sendMessage avCaptureDevice geometricDistortionCorrectionEnabledSelector

-- | geometricDistortionCorrectionEnabled
--
-- Indicates whether geometric distortion correction is enabled by the receiver.
--
-- Where supported, the default value is YES. The receiver must be locked for configuration using lockForConfiguration: before clients can set this method, otherwise an NSGenericException is thrown.
--
-- In the case of ProRes RAW, when geometricDistortionCorrectionEnabled is YES, GDC is applied to your outputs in different ways:    - It is always applied to AVCaptureVideoPreviewLayer.    - It is applied to AVCaptureVideoDataOutput only if deliversPreviewSizedOutputBuffers is set to YES.    - It is never applied to AVCaptureMovieFileOutput.
--
-- When GDC is enabled, AVCaptureVideoDataOutput buffers contain GDC metadata attachments, and AVCaptureMovieFileOutput movies contain GDC metadata which an application supporting ProRes RAW can optionally apply at playback time using the ProRes RAW SDK. To learn more about the ProRes RAW SDK, refer to the Apple ProRes and ProRes RAW Authorized Products article at https://support.apple.com/en-us/118584.
--
-- ObjC selector: @- setGeometricDistortionCorrectionEnabled:@
setGeometricDistortionCorrectionEnabled :: IsAVCaptureDevice avCaptureDevice => avCaptureDevice -> Bool -> IO ()
setGeometricDistortionCorrectionEnabled avCaptureDevice value =
  sendMessage avCaptureDevice setGeometricDistortionCorrectionEnabledSelector value

-- | activeDepthDataFormat
--
-- The currently active depth data format of the receiver.
--
-- This property can be used to get or set the device's currently active depth data format. -setActiveDepthDataFormat: throws an NSInvalidArgumentException if set to a format not present in the activeFormat's -supportedDepthDataFormats array. -setActiveDepthDataFormat: throws an NSGenericException if called without first obtaining exclusive access to the receiver using lockForConfiguration:. Clients can observe automatic changes to the receiver's activeDepthDataFormat by key value observing this property. On devices where depth data is not supported, this property returns nil.
--
-- The frame rate of depth data may not be set directly. Depth data frame rate is synchronized to the device's activeMin/MaxFrameDurations. It may match the device's current frame rate, or lower, if depth data cannot be produced fast enough for the active video frame rate.
--
-- Delivery of depth data to a AVCaptureDepthDataOutput may increase the system load, resulting in a reduced video frame rate for thermal sustainability.
--
-- ObjC selector: @- activeDepthDataFormat@
activeDepthDataFormat :: IsAVCaptureDevice avCaptureDevice => avCaptureDevice -> IO (Id AVCaptureDeviceFormat)
activeDepthDataFormat avCaptureDevice =
  sendMessage avCaptureDevice activeDepthDataFormatSelector

-- | activeDepthDataFormat
--
-- The currently active depth data format of the receiver.
--
-- This property can be used to get or set the device's currently active depth data format. -setActiveDepthDataFormat: throws an NSInvalidArgumentException if set to a format not present in the activeFormat's -supportedDepthDataFormats array. -setActiveDepthDataFormat: throws an NSGenericException if called without first obtaining exclusive access to the receiver using lockForConfiguration:. Clients can observe automatic changes to the receiver's activeDepthDataFormat by key value observing this property. On devices where depth data is not supported, this property returns nil.
--
-- The frame rate of depth data may not be set directly. Depth data frame rate is synchronized to the device's activeMin/MaxFrameDurations. It may match the device's current frame rate, or lower, if depth data cannot be produced fast enough for the active video frame rate.
--
-- Delivery of depth data to a AVCaptureDepthDataOutput may increase the system load, resulting in a reduced video frame rate for thermal sustainability.
--
-- ObjC selector: @- setActiveDepthDataFormat:@
setActiveDepthDataFormat :: (IsAVCaptureDevice avCaptureDevice, IsAVCaptureDeviceFormat value) => avCaptureDevice -> value -> IO ()
setActiveDepthDataFormat avCaptureDevice value =
  sendMessage avCaptureDevice setActiveDepthDataFormatSelector (toAVCaptureDeviceFormat value)

-- | minAvailableVideoZoomFactor
--
-- Indicates the minimum zoom factor available for the AVCaptureDevice's videoZoomFactor property.
--
-- On non-virtual devices the minAvailableVideoZoomFactor is always 1.0. If the device's videoZoomFactor property is assigned a value smaller than 1.0, an NSRangeException is thrown.    On a virtual device the minAvailableVideoZoomFactor can change when the device is delivering depth data to one or more outputs (see -[AVCaptureDeviceFormat supportedVideoZoomFactorsForDepthDataDelivery]). When depth data delivery is enabled, allowed zoom factor values are governed by -[AVCaptureDeviceFormat supportedVideoZoomFactorsForDepthDataDelivery] and this contains the absolute minimum zoom of all allowed zoom factors.    Setting the videoZoomFactor to a value greater than or equal to 1.0, but lower than minAvailableVideoZoomFactor results in the value being clamped to the minAvailableVideoZoomFactor. Clients can key value observe the value of this property.
--
-- ObjC selector: @- minAvailableVideoZoomFactor@
minAvailableVideoZoomFactor :: IsAVCaptureDevice avCaptureDevice => avCaptureDevice -> IO CDouble
minAvailableVideoZoomFactor avCaptureDevice =
  sendMessage avCaptureDevice minAvailableVideoZoomFactorSelector

-- | maxAvailableVideoZoomFactor
--
-- Indicates the maximum zoom factor available for the AVCaptureDevice's videoZoomFactor property.
--
-- On non-virtual devices the maxAvailableVideoZoomFactor is always equal to the activeFormat.videoMaxZoomFactor. If the device's videoZoomFactor property is assigned a value greater than activeFormat.videoMaxZoomFactor, an NSRangeException is thrown.    On a virtual device the maxAvailableVideoZoomFactor can change when the device is delivering depth data to one or more outputs (see -[AVCaptureDeviceFormat supportedVideoZoomFactorsForDepthDataDelivery]). When depth data delivery is enabled, allowed zoom factor values are governed by -[AVCaptureDeviceFormat supportedVideoZoomFactorsForDepthDataDelivery] and this contains the absolute maximum zoom of all allowed zoom factors.    Setting the videoZoomFactor to a value less than or equal to activeFormat.videoMaxZoomFactor, but greater than maxAvailableVideoZoomFactor results in the value being clamped to the maxAvailableVideoZoomFactor. Clients can key value observe the value of this property.
--
-- ObjC selector: @- maxAvailableVideoZoomFactor@
maxAvailableVideoZoomFactor :: IsAVCaptureDevice avCaptureDevice => avCaptureDevice -> IO CDouble
maxAvailableVideoZoomFactor avCaptureDevice =
  sendMessage avCaptureDevice maxAvailableVideoZoomFactorSelector

-- | activeColorSpace
--
-- Indicates the receiver's current active color space.
--
-- By default, an AVCaptureDevice attached to an AVCaptureSession is automatically configured for wide color by the AVCaptureSession (see AVCaptureSession automaticallyConfiguresCaptureDeviceForWideColor). You may also set the activeColorSpace manually. To prevent the AVCaptureSession from undoing your work, remember to set AVCaptureSession's automaticallyConfiguresCaptureDeviceForWideColor property to NO. Changing the receiver's activeColorSpace while the session is running requires a disruptive reconfiguration of the capture render pipeline. Movie captures in progress will be ended immediately; unfulfilled photo requests will be aborted; video preview will temporarily freeze. -setActiveColorSpace: throws an NSGenericException if called without first obtaining exclusive access to the receiver using -lockForConfiguration:.
--
-- ObjC selector: @- activeColorSpace@
activeColorSpace :: IsAVCaptureDevice avCaptureDevice => avCaptureDevice -> IO AVCaptureColorSpace
activeColorSpace avCaptureDevice =
  sendMessage avCaptureDevice activeColorSpaceSelector

-- | activeColorSpace
--
-- Indicates the receiver's current active color space.
--
-- By default, an AVCaptureDevice attached to an AVCaptureSession is automatically configured for wide color by the AVCaptureSession (see AVCaptureSession automaticallyConfiguresCaptureDeviceForWideColor). You may also set the activeColorSpace manually. To prevent the AVCaptureSession from undoing your work, remember to set AVCaptureSession's automaticallyConfiguresCaptureDeviceForWideColor property to NO. Changing the receiver's activeColorSpace while the session is running requires a disruptive reconfiguration of the capture render pipeline. Movie captures in progress will be ended immediately; unfulfilled photo requests will be aborted; video preview will temporarily freeze. -setActiveColorSpace: throws an NSGenericException if called without first obtaining exclusive access to the receiver using -lockForConfiguration:.
--
-- ObjC selector: @- setActiveColorSpace:@
setActiveColorSpace :: IsAVCaptureDevice avCaptureDevice => avCaptureDevice -> AVCaptureColorSpace -> IO ()
setActiveColorSpace avCaptureDevice value =
  sendMessage avCaptureDevice setActiveColorSpaceSelector value

-- | automaticallyAdjustsVideoHDREnabled
--
-- Indicates whether the receiver is allowed to turn high dynamic range streaming on or off.
--
-- The value of this property is a BOOL indicating whether the receiver is free to turn high dynamic range streaming on or off. This property defaults to YES. When automaticallyAdjustsVideoHDREnabled, the AVCaptureDevice turns videoHDR on automatically if it's a good fit for the activeFormat. -setAutomaticallyAdjustsVideoHDREnabled: throws an NSGenericException if called without first obtaining exclusive access to the receiver using -lockForConfiguration:. Clients can key-value observe videoHDREnabled to know when the receiver has automatically changed the value.
--
-- ObjC selector: @- automaticallyAdjustsVideoHDREnabled@
automaticallyAdjustsVideoHDREnabled :: IsAVCaptureDevice avCaptureDevice => avCaptureDevice -> IO Bool
automaticallyAdjustsVideoHDREnabled avCaptureDevice =
  sendMessage avCaptureDevice automaticallyAdjustsVideoHDREnabledSelector

-- | automaticallyAdjustsVideoHDREnabled
--
-- Indicates whether the receiver is allowed to turn high dynamic range streaming on or off.
--
-- The value of this property is a BOOL indicating whether the receiver is free to turn high dynamic range streaming on or off. This property defaults to YES. When automaticallyAdjustsVideoHDREnabled, the AVCaptureDevice turns videoHDR on automatically if it's a good fit for the activeFormat. -setAutomaticallyAdjustsVideoHDREnabled: throws an NSGenericException if called without first obtaining exclusive access to the receiver using -lockForConfiguration:. Clients can key-value observe videoHDREnabled to know when the receiver has automatically changed the value.
--
-- ObjC selector: @- setAutomaticallyAdjustsVideoHDREnabled:@
setAutomaticallyAdjustsVideoHDREnabled :: IsAVCaptureDevice avCaptureDevice => avCaptureDevice -> Bool -> IO ()
setAutomaticallyAdjustsVideoHDREnabled avCaptureDevice value =
  sendMessage avCaptureDevice setAutomaticallyAdjustsVideoHDREnabledSelector value

-- | videoHDREnabled
--
-- Indicates whether the receiver's streaming high dynamic range feature is enabled. See AVCaptureDeviceFormat.isVideoHDRSupported.
--
-- The value of this property is a BOOL indicating whether the receiver is currently streaming high dynamic range video buffers, also known as Extended Dynamic Range (EDR). The value of this property is ignored when device.activeColorSpace is HLG BT2020 color space since HDR is effectively always on and can't be disabled. The property may only be set if you first set automaticallyAdjustsVideoHDREnabled to NO, otherwise an NSGenericException is thrown. videoHDREnabled may only be set to YES if the receiver's activeFormat.isVideoHDRSupported property returns YES, otherwise an NSGenericException is thrown. This property may be key-value observed.
--
-- Note that setting this property may cause a lengthy reconfiguration of the receiver, similar to setting a new active format or AVCaptureSession sessionPreset. If you are setting either the active format or the AVCaptureSession's sessionPreset AND this property, you should bracket these operations with [session beginConfiguration] and [session commitConfiguration] to minimize reconfiguration time.
--
-- ObjC selector: @- videoHDREnabled@
videoHDREnabled :: IsAVCaptureDevice avCaptureDevice => avCaptureDevice -> IO Bool
videoHDREnabled avCaptureDevice =
  sendMessage avCaptureDevice videoHDREnabledSelector

-- | videoHDREnabled
--
-- Indicates whether the receiver's streaming high dynamic range feature is enabled. See AVCaptureDeviceFormat.isVideoHDRSupported.
--
-- The value of this property is a BOOL indicating whether the receiver is currently streaming high dynamic range video buffers, also known as Extended Dynamic Range (EDR). The value of this property is ignored when device.activeColorSpace is HLG BT2020 color space since HDR is effectively always on and can't be disabled. The property may only be set if you first set automaticallyAdjustsVideoHDREnabled to NO, otherwise an NSGenericException is thrown. videoHDREnabled may only be set to YES if the receiver's activeFormat.isVideoHDRSupported property returns YES, otherwise an NSGenericException is thrown. This property may be key-value observed.
--
-- Note that setting this property may cause a lengthy reconfiguration of the receiver, similar to setting a new active format or AVCaptureSession sessionPreset. If you are setting either the active format or the AVCaptureSession's sessionPreset AND this property, you should bracket these operations with [session beginConfiguration] and [session commitConfiguration] to minimize reconfiguration time.
--
-- ObjC selector: @- setVideoHDREnabled:@
setVideoHDREnabled :: IsAVCaptureDevice avCaptureDevice => avCaptureDevice -> Bool -> IO ()
setVideoHDREnabled avCaptureDevice value =
  sendMessage avCaptureDevice setVideoHDREnabledSelector value

-- | transportControlsSupported
--
-- Returns whether the receiver supports transport control commands.
--
-- For devices with transport controls, such as AVC tape-based camcorders or pro capture devices with RS422 deck control, the value of this property is YES. If transport controls are not supported, none of the associated transport control methods and properties are available on the receiver.
--
-- ObjC selector: @- transportControlsSupported@
transportControlsSupported :: IsAVCaptureDevice avCaptureDevice => avCaptureDevice -> IO Bool
transportControlsSupported avCaptureDevice =
  sendMessage avCaptureDevice transportControlsSupportedSelector

-- | transportControlsPlaybackMode
--
-- Returns the receiver's current playback mode.
--
-- For devices that support transport control, this property may be queried to discover the current playback mode.
--
-- ObjC selector: @- transportControlsPlaybackMode@
transportControlsPlaybackMode :: IsAVCaptureDevice avCaptureDevice => avCaptureDevice -> IO AVCaptureDeviceTransportControlsPlaybackMode
transportControlsPlaybackMode avCaptureDevice =
  sendMessage avCaptureDevice transportControlsPlaybackModeSelector

-- | transportControlsSpeed
--
-- Returns the receiver's current playback speed as a floating point value.
--
-- For devices that support transport control, this property may be queried to discover the current playback speed of the deck.    0.0 -> stopped.    1.0 -> forward at normal speed.    -1.0-> reverse at normal speed.    2.0 -> forward at 2x normal speed.    etc.
--
-- ObjC selector: @- transportControlsSpeed@
transportControlsSpeed :: IsAVCaptureDevice avCaptureDevice => avCaptureDevice -> IO CFloat
transportControlsSpeed avCaptureDevice =
  sendMessage avCaptureDevice transportControlsSpeedSelector

-- | videoZoomFactor
--
-- Controls zoom level of image outputs
--
-- Applies a centered crop for all image outputs, scaling as necessary to maintain output dimensions. Minimum value of 1.0 yields full field of view, increasing values will increase magnification, up to a maximum value specified in the activeFormat's videoMaxZoomFactor property. Modifying the zoom factor will cancel any active rampToVideoZoomFactor:withRate:, and snap directly to the assigned value. Assigning values outside the acceptable range will generate an NSRangeException. Clients can key value observe the value of this property. When depth data delivery is enabled, changing the zoom factor sets the videoZoomFactor to the nearest supportedVideoZoomFactor from -[AVCaptureDeviceFormat supportedVideoZoomFactorsForDepthDataDelivery] with a disruptive reconfiguration of the capture render pipeline.
--
-- -setVideoZoomFactor: throws an NSGenericException if called without first obtaining exclusive access to the receiver using lockForConfiguration:.
--
-- -[AVCaptureDeviceFormat videoMaxZoomFactor], -[AVCaptureDeviceFormat videoZoomFactorUpscaleThreshold], -[AVCaptureDevice minAvailableVideoZoomFactor], -[AVCaptureDevice maxAvailableVideoZoomFactor],  -[AVCaptureDeviceFormat supportedVideoZoomFactorsForDepthDataDelivery], -[AVCaptureDeviceFormat videoMinZoomFactorForCenterStage] and -[AVCaptureDeviceFormat videoMaxZoomFactorForCenterStage]
--
-- ObjC selector: @- videoZoomFactor@
videoZoomFactor :: IsAVCaptureDevice avCaptureDevice => avCaptureDevice -> IO CDouble
videoZoomFactor avCaptureDevice =
  sendMessage avCaptureDevice videoZoomFactorSelector

-- | videoZoomFactor
--
-- Controls zoom level of image outputs
--
-- Applies a centered crop for all image outputs, scaling as necessary to maintain output dimensions. Minimum value of 1.0 yields full field of view, increasing values will increase magnification, up to a maximum value specified in the activeFormat's videoMaxZoomFactor property. Modifying the zoom factor will cancel any active rampToVideoZoomFactor:withRate:, and snap directly to the assigned value. Assigning values outside the acceptable range will generate an NSRangeException. Clients can key value observe the value of this property. When depth data delivery is enabled, changing the zoom factor sets the videoZoomFactor to the nearest supportedVideoZoomFactor from -[AVCaptureDeviceFormat supportedVideoZoomFactorsForDepthDataDelivery] with a disruptive reconfiguration of the capture render pipeline.
--
-- -setVideoZoomFactor: throws an NSGenericException if called without first obtaining exclusive access to the receiver using lockForConfiguration:.
--
-- -[AVCaptureDeviceFormat videoMaxZoomFactor], -[AVCaptureDeviceFormat videoZoomFactorUpscaleThreshold], -[AVCaptureDevice minAvailableVideoZoomFactor], -[AVCaptureDevice maxAvailableVideoZoomFactor],  -[AVCaptureDeviceFormat supportedVideoZoomFactorsForDepthDataDelivery], -[AVCaptureDeviceFormat videoMinZoomFactorForCenterStage] and -[AVCaptureDeviceFormat videoMaxZoomFactorForCenterStage]
--
-- ObjC selector: @- setVideoZoomFactor:@
setVideoZoomFactor :: IsAVCaptureDevice avCaptureDevice => avCaptureDevice -> CDouble -> IO ()
setVideoZoomFactor avCaptureDevice value =
  sendMessage avCaptureDevice setVideoZoomFactorSelector value

-- | rampingVideoZoom
--
-- Indicates if the zoom factor is transitioning to a value set by rampToVideoZoomFactor:withRate:
--
-- Clients can observe this value to determine when a ramp begins or completes.
--
-- ObjC selector: @- rampingVideoZoom@
rampingVideoZoom :: IsAVCaptureDevice avCaptureDevice => avCaptureDevice -> IO Bool
rampingVideoZoom avCaptureDevice =
  sendMessage avCaptureDevice rampingVideoZoomSelector

-- | dualCameraSwitchOverVideoZoomFactor
--
-- The video zoom factor at or above which a DualCamera can select between its wide angle camera and its telephoto camera.
--
-- This is the zoom factor at which the wide angle camera's field of view matches telephoto camera's full field of view. On non-DualCamera devices this will return 1.0. As of iOS 13.0, this API has been deprecated in favor of virtualDeviceSwitchOverVideoZoomFactors.
--
-- ObjC selector: @- dualCameraSwitchOverVideoZoomFactor@
dualCameraSwitchOverVideoZoomFactor :: IsAVCaptureDevice avCaptureDevice => avCaptureDevice -> IO CDouble
dualCameraSwitchOverVideoZoomFactor avCaptureDevice =
  sendMessage avCaptureDevice dualCameraSwitchOverVideoZoomFactorSelector

-- | displayVideoZoomFactorMultiplier
--
-- A multiplier that can be used with the receiver's videoZoomFactor property for displaying a video zoom factor in a user interface.
--
-- In some system user interfaces, like the macOS Video Effects Menu, the video zoom factor value is displayed in a way most appropriate for visual representation and might differ from the videoZoomFactor property value on the receiver by a fixed ratio. For example, if the videoZoomFactor property value is 1.0 and the displayVideoZoomFactorMultiplier property value is 0.5, then multiplying 1.0 and 0.5 produces 0.5 which can be displayed in the UI. Client applications can key value observe this property to update the display video zoom factor values in their UI to stay consistent with Apple's system UIs.
--
-- ObjC selector: @- displayVideoZoomFactorMultiplier@
displayVideoZoomFactorMultiplier :: IsAVCaptureDevice avCaptureDevice => avCaptureDevice -> IO CDouble
displayVideoZoomFactorMultiplier avCaptureDevice =
  sendMessage avCaptureDevice displayVideoZoomFactorMultiplierSelector

-- | lowLightBoostSupported
--
-- Indicates whether the receiver supports boosting images in low light conditions.
--
-- The receiver's automaticallyEnablesLowLightBoostWhenAvailable property can only be set if this property returns YES.
--
-- ObjC selector: @- lowLightBoostSupported@
lowLightBoostSupported :: IsAVCaptureDevice avCaptureDevice => avCaptureDevice -> IO Bool
lowLightBoostSupported avCaptureDevice =
  sendMessage avCaptureDevice lowLightBoostSupportedSelector

-- | lowLightBoostEnabled
--
-- Indicates whether the receiver's low light boost feature is enabled.
--
-- The value of this property is a BOOL indicating whether the receiver is currently enhancing images to improve quality due to low light conditions. When -isLowLightBoostEnabled returns YES, the receiver has switched into a special mode in which more light can be perceived in images. This property is key-value observable.
--
-- ObjC selector: @- lowLightBoostEnabled@
lowLightBoostEnabled :: IsAVCaptureDevice avCaptureDevice => avCaptureDevice -> IO Bool
lowLightBoostEnabled avCaptureDevice =
  sendMessage avCaptureDevice lowLightBoostEnabledSelector

-- | automaticallyEnablesLowLightBoostWhenAvailable
--
-- Indicates whether the receiver should automatically switch to low light boost mode when necessary.
--
-- On a receiver where -isLowLightBoostSupported returns YES, a special low light boost mode may be engaged to improve image quality. When the automaticallyEnablesLowLightBoostWhenAvailable property is set to YES, the receiver switches at its discretion to a special boost mode under low light, and back to normal operation when the scene becomes sufficiently lit. An AVCaptureDevice that supports this feature may only engage boost mode for certain source formats or resolutions. Clients may observe changes to the lowLightBoostEnabled property to know when the mode has engaged. The switch between normal operation and low light boost mode may drop one or more video frames. The default value is NO. Setting this property throws an NSInvalidArgumentException if -isLowLightBoostSupported returns NO. The receiver must be locked for configuration using lockForConfiguration: before clients can set this method, otherwise an NSGenericException is thrown.
--
-- ObjC selector: @- automaticallyEnablesLowLightBoostWhenAvailable@
automaticallyEnablesLowLightBoostWhenAvailable :: IsAVCaptureDevice avCaptureDevice => avCaptureDevice -> IO Bool
automaticallyEnablesLowLightBoostWhenAvailable avCaptureDevice =
  sendMessage avCaptureDevice automaticallyEnablesLowLightBoostWhenAvailableSelector

-- | automaticallyEnablesLowLightBoostWhenAvailable
--
-- Indicates whether the receiver should automatically switch to low light boost mode when necessary.
--
-- On a receiver where -isLowLightBoostSupported returns YES, a special low light boost mode may be engaged to improve image quality. When the automaticallyEnablesLowLightBoostWhenAvailable property is set to YES, the receiver switches at its discretion to a special boost mode under low light, and back to normal operation when the scene becomes sufficiently lit. An AVCaptureDevice that supports this feature may only engage boost mode for certain source formats or resolutions. Clients may observe changes to the lowLightBoostEnabled property to know when the mode has engaged. The switch between normal operation and low light boost mode may drop one or more video frames. The default value is NO. Setting this property throws an NSInvalidArgumentException if -isLowLightBoostSupported returns NO. The receiver must be locked for configuration using lockForConfiguration: before clients can set this method, otherwise an NSGenericException is thrown.
--
-- ObjC selector: @- setAutomaticallyEnablesLowLightBoostWhenAvailable:@
setAutomaticallyEnablesLowLightBoostWhenAvailable :: IsAVCaptureDevice avCaptureDevice => avCaptureDevice -> Bool -> IO ()
setAutomaticallyEnablesLowLightBoostWhenAvailable avCaptureDevice value =
  sendMessage avCaptureDevice setAutomaticallyEnablesLowLightBoostWhenAvailableSelector value

-- | subjectAreaChangeMonitoringEnabled
--
-- Indicates whether the receiver should monitor the subject area for changes.
--
-- The value of this property is a BOOL indicating whether the receiver should monitor the video subject area for changes, such as lighting changes, substantial movement, etc. If subject area change monitoring is enabled, the receiver sends an AVCaptureDeviceSubjectAreaDidChangeNotification whenever it detects a change to the subject area, at which time an interested client may wish to re-focus, adjust exposure, white balance, etc. The receiver must be locked for configuration using lockForConfiguration: before clients can set the value of this property.
--
-- ObjC selector: @- subjectAreaChangeMonitoringEnabled@
subjectAreaChangeMonitoringEnabled :: IsAVCaptureDevice avCaptureDevice => avCaptureDevice -> IO Bool
subjectAreaChangeMonitoringEnabled avCaptureDevice =
  sendMessage avCaptureDevice subjectAreaChangeMonitoringEnabledSelector

-- | subjectAreaChangeMonitoringEnabled
--
-- Indicates whether the receiver should monitor the subject area for changes.
--
-- The value of this property is a BOOL indicating whether the receiver should monitor the video subject area for changes, such as lighting changes, substantial movement, etc. If subject area change monitoring is enabled, the receiver sends an AVCaptureDeviceSubjectAreaDidChangeNotification whenever it detects a change to the subject area, at which time an interested client may wish to re-focus, adjust exposure, white balance, etc. The receiver must be locked for configuration using lockForConfiguration: before clients can set the value of this property.
--
-- ObjC selector: @- setSubjectAreaChangeMonitoringEnabled:@
setSubjectAreaChangeMonitoringEnabled :: IsAVCaptureDevice avCaptureDevice => avCaptureDevice -> Bool -> IO ()
setSubjectAreaChangeMonitoringEnabled avCaptureDevice value =
  sendMessage avCaptureDevice setSubjectAreaChangeMonitoringEnabledSelector value

-- | lockingWhiteBalanceWithCustomDeviceGainsSupported
--
-- Indicates whether the receiver supports white balance gains other than AVCaptureWhiteBalanceGainsCurrent.
--
-- If lockingWhiteBalanceWithCustomDeviceGainsSupported returns NO, setWhiteBalanceModeLockedWithDeviceWhiteBalanceGains: may only be called with AVCaptureWhiteBalanceGainsCurrent. Passing any other white balance gains will result in an exception.
--
-- ObjC selector: @- lockingWhiteBalanceWithCustomDeviceGainsSupported@
lockingWhiteBalanceWithCustomDeviceGainsSupported :: IsAVCaptureDevice avCaptureDevice => avCaptureDevice -> IO Bool
lockingWhiteBalanceWithCustomDeviceGainsSupported avCaptureDevice =
  sendMessage avCaptureDevice lockingWhiteBalanceWithCustomDeviceGainsSupportedSelector

-- | whiteBalanceMode
--
-- Indicates current white balance mode of the receiver, if it has adjustable white balance.
--
-- The value of this property is an AVCaptureWhiteBalanceMode that determines the receiver's white balance mode, if it has adjustable white balance. -setWhiteBalanceMode: throws an NSInvalidArgumentException if set to an unsupported value (see -isWhiteBalanceModeSupported:). -setWhiteBalanceMode: throws an NSGenericException if called without first obtaining exclusive access to the receiver using lockForConfiguration:. Clients can observe automatic changes to the receiver's whiteBalanceMode by key value observing this property.
--
-- ObjC selector: @- whiteBalanceMode@
whiteBalanceMode :: IsAVCaptureDevice avCaptureDevice => avCaptureDevice -> IO AVCaptureWhiteBalanceMode
whiteBalanceMode avCaptureDevice =
  sendMessage avCaptureDevice whiteBalanceModeSelector

-- | whiteBalanceMode
--
-- Indicates current white balance mode of the receiver, if it has adjustable white balance.
--
-- The value of this property is an AVCaptureWhiteBalanceMode that determines the receiver's white balance mode, if it has adjustable white balance. -setWhiteBalanceMode: throws an NSInvalidArgumentException if set to an unsupported value (see -isWhiteBalanceModeSupported:). -setWhiteBalanceMode: throws an NSGenericException if called without first obtaining exclusive access to the receiver using lockForConfiguration:. Clients can observe automatic changes to the receiver's whiteBalanceMode by key value observing this property.
--
-- ObjC selector: @- setWhiteBalanceMode:@
setWhiteBalanceMode :: IsAVCaptureDevice avCaptureDevice => avCaptureDevice -> AVCaptureWhiteBalanceMode -> IO ()
setWhiteBalanceMode avCaptureDevice value =
  sendMessage avCaptureDevice setWhiteBalanceModeSelector value

-- | adjustingWhiteBalance
--
-- Indicates whether the receiver is currently adjusting camera white balance.
--
-- The value of this property is a BOOL indicating whether the receiver's camera white balance is being automatically adjusted because its white balance mode is AVCaptureWhiteBalanceModeAutoWhiteBalance or AVCaptureWhiteBalanceModeContinuousAutoWhiteBalance. Clients can observe the value of this property to determine whether the camera white balance is stable or is being automatically adjusted.
--
-- ObjC selector: @- adjustingWhiteBalance@
adjustingWhiteBalance :: IsAVCaptureDevice avCaptureDevice => avCaptureDevice -> IO Bool
adjustingWhiteBalance avCaptureDevice =
  sendMessage avCaptureDevice adjustingWhiteBalanceSelector

-- | maxWhiteBalanceGain
--
-- Indicates the maximum supported value to which a channel in the AVCaptureWhiteBalanceGains may be set.
--
-- This property does not change for the life of the receiver.
--
-- ObjC selector: @- maxWhiteBalanceGain@
maxWhiteBalanceGain :: IsAVCaptureDevice avCaptureDevice => avCaptureDevice -> IO CFloat
maxWhiteBalanceGain avCaptureDevice =
  sendMessage avCaptureDevice maxWhiteBalanceGainSelector

-- | globalToneMappingEnabled
--
-- Indicates whether the receiver should use global tone mapping.
--
-- Tone mapping is a technique used by the device to map the pixel levels in high dynamic range images to a more limited dynamic range (such as 16 bit to 8 bit), while still retaining as close an appearance as possible. Normally the device employs adaptive, local tone curves to preserve highest image quality and adapt quickly to changing lighting conditions.
--
-- This property indicates to the receiver to use a global tone map. If set to YES, the tone map is adjusted dynamically depending on the current scene and the same tone map is applied to all pixels in an image. If set to its default value of NO, different tone maps may be applied to different pixels in an image.
--
-- globalToneMappingEnabled may only be set to YES if the receiver's activeFormat.isGlobalToneMappingSupported property returns YES, otherwise an NSGenericException is thrown. Setting globalToneMappingEnabled throws an NSGenericException if called without first obtaining exclusive access to the receiver using lockForConfiguration:.
--
-- When global tone mapping is enabled, an AVCapturePhotoOutput connected to the AVCaptureDeviceInput’s session disables all forms of still image fusion, resulting in still images with no automatic stabilization applied.
--
-- The receiver’s globalToneMappingEnabled resets to its default value of NO under the following conditions:     - The receiver’s activeFormat changes     - The receiver’s AVCaptureDeviceInput’s session’s sessionPreset changes     - The receiver’s AVCaptureDeviceInput is added to a session
--
-- Clients can observe automatic changes to the receiver's globalToneMappingEnabled by key value observing this property.
--
-- ObjC selector: @- globalToneMappingEnabled@
globalToneMappingEnabled :: IsAVCaptureDevice avCaptureDevice => avCaptureDevice -> IO Bool
globalToneMappingEnabled avCaptureDevice =
  sendMessage avCaptureDevice globalToneMappingEnabledSelector

-- | globalToneMappingEnabled
--
-- Indicates whether the receiver should use global tone mapping.
--
-- Tone mapping is a technique used by the device to map the pixel levels in high dynamic range images to a more limited dynamic range (such as 16 bit to 8 bit), while still retaining as close an appearance as possible. Normally the device employs adaptive, local tone curves to preserve highest image quality and adapt quickly to changing lighting conditions.
--
-- This property indicates to the receiver to use a global tone map. If set to YES, the tone map is adjusted dynamically depending on the current scene and the same tone map is applied to all pixels in an image. If set to its default value of NO, different tone maps may be applied to different pixels in an image.
--
-- globalToneMappingEnabled may only be set to YES if the receiver's activeFormat.isGlobalToneMappingSupported property returns YES, otherwise an NSGenericException is thrown. Setting globalToneMappingEnabled throws an NSGenericException if called without first obtaining exclusive access to the receiver using lockForConfiguration:.
--
-- When global tone mapping is enabled, an AVCapturePhotoOutput connected to the AVCaptureDeviceInput’s session disables all forms of still image fusion, resulting in still images with no automatic stabilization applied.
--
-- The receiver’s globalToneMappingEnabled resets to its default value of NO under the following conditions:     - The receiver’s activeFormat changes     - The receiver’s AVCaptureDeviceInput’s session’s sessionPreset changes     - The receiver’s AVCaptureDeviceInput is added to a session
--
-- Clients can observe automatic changes to the receiver's globalToneMappingEnabled by key value observing this property.
--
-- ObjC selector: @- setGlobalToneMappingEnabled:@
setGlobalToneMappingEnabled :: IsAVCaptureDevice avCaptureDevice => avCaptureDevice -> Bool -> IO ()
setGlobalToneMappingEnabled avCaptureDevice value =
  sendMessage avCaptureDevice setGlobalToneMappingEnabledSelector value

-- | exposureMode
--
-- Indicates current exposure mode of the receiver, if it has adjustable exposure.
--
-- The value of this property is an AVCaptureExposureMode that determines the receiver's exposure mode, if it has adjustable exposure. -setExposureMode: throws an NSInvalidArgumentException if set to an unsupported value (see -isExposureModeSupported:). -setExposureMode: throws an NSGenericException if called without first obtaining exclusive access to the receiver using lockForConfiguration:. When using AVCapturePhotoOutput and capturing photos with AVCapturePhotoSettings' photoQualityPrioritization property set to AVCapturePhotoQualityPrioritizationBalanced or higher, the receiver's ISO and exposureDuration values may be overridden when exposing the photo if the scene is dark enough to warrant some form of multi-image fusion to improve quality. To ensure that the receiver's ISO and exposureDuration values are honored while in AVCaptureExposureModeCustom or AVCaptureExposureModeLocked, you must set your AVCapturePhotoSettings.photoQualityPrioritization property to AVCapturePhotoQualityPrioritizationSpeed. The same rule applies if you are using the deprecated AVCapturePhotoSettings.autoStillImageStabilizationEnabled property; you must set it to NO to preserve your custom exposure values in the photo capture. Likewise if you're using AVCaptureStillImageOutput, automaticallyEnablesStillImageStabilizationWhenAvailable must be set to NO to preserve your custom exposure values in a still image capture. Clients can observe automatic changes to the receiver's exposureMode by key value observing this property.
--
-- ObjC selector: @- exposureMode@
exposureMode :: IsAVCaptureDevice avCaptureDevice => avCaptureDevice -> IO AVCaptureExposureMode
exposureMode avCaptureDevice =
  sendMessage avCaptureDevice exposureModeSelector

-- | exposureMode
--
-- Indicates current exposure mode of the receiver, if it has adjustable exposure.
--
-- The value of this property is an AVCaptureExposureMode that determines the receiver's exposure mode, if it has adjustable exposure. -setExposureMode: throws an NSInvalidArgumentException if set to an unsupported value (see -isExposureModeSupported:). -setExposureMode: throws an NSGenericException if called without first obtaining exclusive access to the receiver using lockForConfiguration:. When using AVCapturePhotoOutput and capturing photos with AVCapturePhotoSettings' photoQualityPrioritization property set to AVCapturePhotoQualityPrioritizationBalanced or higher, the receiver's ISO and exposureDuration values may be overridden when exposing the photo if the scene is dark enough to warrant some form of multi-image fusion to improve quality. To ensure that the receiver's ISO and exposureDuration values are honored while in AVCaptureExposureModeCustom or AVCaptureExposureModeLocked, you must set your AVCapturePhotoSettings.photoQualityPrioritization property to AVCapturePhotoQualityPrioritizationSpeed. The same rule applies if you are using the deprecated AVCapturePhotoSettings.autoStillImageStabilizationEnabled property; you must set it to NO to preserve your custom exposure values in the photo capture. Likewise if you're using AVCaptureStillImageOutput, automaticallyEnablesStillImageStabilizationWhenAvailable must be set to NO to preserve your custom exposure values in a still image capture. Clients can observe automatic changes to the receiver's exposureMode by key value observing this property.
--
-- ObjC selector: @- setExposureMode:@
setExposureMode :: IsAVCaptureDevice avCaptureDevice => avCaptureDevice -> AVCaptureExposureMode -> IO ()
setExposureMode avCaptureDevice value =
  sendMessage avCaptureDevice setExposureModeSelector value

-- | exposurePointOfInterestSupported:
--
-- Indicates whether the receiver supports exposure points of interest.
--
-- The receiver's exposurePointOfInterest property can only be set if this property returns YES.
--
-- ObjC selector: @- exposurePointOfInterestSupported@
exposurePointOfInterestSupported :: IsAVCaptureDevice avCaptureDevice => avCaptureDevice -> IO Bool
exposurePointOfInterestSupported avCaptureDevice =
  sendMessage avCaptureDevice exposurePointOfInterestSupportedSelector

-- | Whether the device supports exposure rectangles of interest.
--
-- You may only set the device's ``exposureRectOfInterest`` property if this property returns @true@.
--
-- ObjC selector: @- exposureRectOfInterestSupported@
exposureRectOfInterestSupported :: IsAVCaptureDevice avCaptureDevice => avCaptureDevice -> IO Bool
exposureRectOfInterestSupported avCaptureDevice =
  sendMessage avCaptureDevice exposureRectOfInterestSupportedSelector

-- | automaticallyAdjustsFaceDrivenAutoExposureEnabled
--
-- Indicates whether the receiver should automatically adjust face-driven auto exposure.
--
-- The value of this property is a BOOL that determines the receiver's automatic adjustment of face-driven auto exposure. Default is YES on all platforms, if the receiver supports auto exposure. This property must be set to NO before manually setting faceDrivenAutoExposureEnabled to YES/NO. -setAutomaticallyAdjustsFaceDrivenAutoExposureEnabled: throws an NSInvalidArgumentException if the receiver doesn't support auto exposure. -setAutomaticallyAdjustsFaceDrivenAutoExposureEnabled: throws an NSGenericException if called without first obtaining exclusive access to the receiver using -lockForConfiguration:. After setting automaticallyAdjustsFaceDrivenAutoExposureEnabled, call -setExposureMode: to apply the change.
--
-- ObjC selector: @- automaticallyAdjustsFaceDrivenAutoExposureEnabled@
automaticallyAdjustsFaceDrivenAutoExposureEnabled :: IsAVCaptureDevice avCaptureDevice => avCaptureDevice -> IO Bool
automaticallyAdjustsFaceDrivenAutoExposureEnabled avCaptureDevice =
  sendMessage avCaptureDevice automaticallyAdjustsFaceDrivenAutoExposureEnabledSelector

-- | automaticallyAdjustsFaceDrivenAutoExposureEnabled
--
-- Indicates whether the receiver should automatically adjust face-driven auto exposure.
--
-- The value of this property is a BOOL that determines the receiver's automatic adjustment of face-driven auto exposure. Default is YES on all platforms, if the receiver supports auto exposure. This property must be set to NO before manually setting faceDrivenAutoExposureEnabled to YES/NO. -setAutomaticallyAdjustsFaceDrivenAutoExposureEnabled: throws an NSInvalidArgumentException if the receiver doesn't support auto exposure. -setAutomaticallyAdjustsFaceDrivenAutoExposureEnabled: throws an NSGenericException if called without first obtaining exclusive access to the receiver using -lockForConfiguration:. After setting automaticallyAdjustsFaceDrivenAutoExposureEnabled, call -setExposureMode: to apply the change.
--
-- ObjC selector: @- setAutomaticallyAdjustsFaceDrivenAutoExposureEnabled:@
setAutomaticallyAdjustsFaceDrivenAutoExposureEnabled :: IsAVCaptureDevice avCaptureDevice => avCaptureDevice -> Bool -> IO ()
setAutomaticallyAdjustsFaceDrivenAutoExposureEnabled avCaptureDevice value =
  sendMessage avCaptureDevice setAutomaticallyAdjustsFaceDrivenAutoExposureEnabledSelector value

-- | faceDrivenAutoExposureEnabled
--
-- Indicates whether face-driven auto exposure is enabled on the receiver.
--
-- Default is YES for all apps linked on or after iOS 15.4 when the receiver supports auto exposure. -setFaceDrivenAutoExposureEnabled: throws an NSInvalidArgumentException if automaticallyAdjustsFaceDrivenAutoExposureEnabled returns YES. -setFaceDrivenAutoExposureEnabled: throws an NSInvalidArgumentException if the receiver doesn't support auto exposure. -setFaceDrivenAutoExposureEnabled: throws an NSGenericException if called without first obtaining exclusive access to the receiver using -lockForConfiguration:. Note that setting faceDrivenAutoExposureEnabled alone does not initiate this exposure change operation. After setting faceDrivenAutoExposureEnabled, call -setExposureMode: to apply the change.
--
-- ObjC selector: @- faceDrivenAutoExposureEnabled@
faceDrivenAutoExposureEnabled :: IsAVCaptureDevice avCaptureDevice => avCaptureDevice -> IO Bool
faceDrivenAutoExposureEnabled avCaptureDevice =
  sendMessage avCaptureDevice faceDrivenAutoExposureEnabledSelector

-- | faceDrivenAutoExposureEnabled
--
-- Indicates whether face-driven auto exposure is enabled on the receiver.
--
-- Default is YES for all apps linked on or after iOS 15.4 when the receiver supports auto exposure. -setFaceDrivenAutoExposureEnabled: throws an NSInvalidArgumentException if automaticallyAdjustsFaceDrivenAutoExposureEnabled returns YES. -setFaceDrivenAutoExposureEnabled: throws an NSInvalidArgumentException if the receiver doesn't support auto exposure. -setFaceDrivenAutoExposureEnabled: throws an NSGenericException if called without first obtaining exclusive access to the receiver using -lockForConfiguration:. Note that setting faceDrivenAutoExposureEnabled alone does not initiate this exposure change operation. After setting faceDrivenAutoExposureEnabled, call -setExposureMode: to apply the change.
--
-- ObjC selector: @- setFaceDrivenAutoExposureEnabled:@
setFaceDrivenAutoExposureEnabled :: IsAVCaptureDevice avCaptureDevice => avCaptureDevice -> Bool -> IO ()
setFaceDrivenAutoExposureEnabled avCaptureDevice value =
  sendMessage avCaptureDevice setFaceDrivenAutoExposureEnabledSelector value

-- | adjustingExposure
--
-- Indicates whether the receiver is currently adjusting camera exposure.
--
-- The value of this property is a BOOL indicating whether the receiver's camera exposure is being automatically adjusted because its exposure mode is AVCaptureExposureModeAutoExpose or AVCaptureExposureModeContinuousAutoExposure. Clients can observe the value of this property to determine whether the camera exposure is stable or is being automatically adjusted.
--
-- ObjC selector: @- adjustingExposure@
adjustingExposure :: IsAVCaptureDevice avCaptureDevice => avCaptureDevice -> IO Bool
adjustingExposure avCaptureDevice =
  sendMessage avCaptureDevice adjustingExposureSelector

-- | lensAperture
--
-- The size of the lens diaphragm.
--
-- The value of this property is a float indicating the size (f number) of the lens diaphragm. This property does not change.
--
-- ObjC selector: @- lensAperture@
lensAperture :: IsAVCaptureDevice avCaptureDevice => avCaptureDevice -> IO CFloat
lensAperture avCaptureDevice =
  sendMessage avCaptureDevice lensApertureSelector

-- | ISO
--
-- The current exposure ISO value.
--
-- This property controls the sensor's sensitivity to light by means of a gain value applied to the signal. Only ISO values between activeFormat.minISO and activeFormat.maxISO are supported. Higher values will result in noisier images. This property is key-value observable. It can be read at any time, regardless of exposure mode, but can only be set via setExposureModeCustomWithDuration:ISO:completionHandler:.
--
-- ObjC selector: @- ISO@
iso :: IsAVCaptureDevice avCaptureDevice => avCaptureDevice -> IO CFloat
iso avCaptureDevice =
  sendMessage avCaptureDevice isoSelector

-- | exposureTargetOffset
--
-- Indicates the metered exposure level's offset from the target exposure value, in EV units.
--
-- The value of this read-only property indicates the difference between the metered exposure level of the current scene and the target exposure value. This property is key-value observable.
--
-- ObjC selector: @- exposureTargetOffset@
exposureTargetOffset :: IsAVCaptureDevice avCaptureDevice => avCaptureDevice -> IO CFloat
exposureTargetOffset avCaptureDevice =
  sendMessage avCaptureDevice exposureTargetOffsetSelector

-- | exposureTargetBias
--
-- Bias applied to the target exposure value, in EV units.
--
-- When exposureMode is AVCaptureExposureModeContinuousAutoExposure or AVCaptureExposureModeLocked, the bias will affect both metering (exposureTargetOffset), and the actual exposure level (exposureDuration and ISO). When the exposure mode is AVCaptureExposureModeCustom, it will only affect metering. This property is key-value observable. It can be read at any time, but can only be set via setExposureTargetBias:completionHandler:.
--
-- ObjC selector: @- exposureTargetBias@
exposureTargetBias :: IsAVCaptureDevice avCaptureDevice => avCaptureDevice -> IO CFloat
exposureTargetBias avCaptureDevice =
  sendMessage avCaptureDevice exposureTargetBiasSelector

-- | minExposureTargetBias
--
-- A float indicating the minimum supported exposure bias, in EV units.
--
-- This read-only property indicates the minimum supported exposure bias.
--
-- ObjC selector: @- minExposureTargetBias@
minExposureTargetBias :: IsAVCaptureDevice avCaptureDevice => avCaptureDevice -> IO CFloat
minExposureTargetBias avCaptureDevice =
  sendMessage avCaptureDevice minExposureTargetBiasSelector

-- | maxExposureTargetBias
--
-- A float indicating the maximum supported exposure bias, in EV units.
--
-- This read-only property indicates the maximum supported exposure bias.
--
-- ObjC selector: @- maxExposureTargetBias@
maxExposureTargetBias :: IsAVCaptureDevice avCaptureDevice => avCaptureDevice -> IO CFloat
maxExposureTargetBias avCaptureDevice =
  sendMessage avCaptureDevice maxExposureTargetBiasSelector

-- | lockingFocusWithCustomLensPositionSupported
--
-- Indicates whether the receiver supports a lens position other than AVCaptureLensPositionCurrent.
--
-- If lockingFocusWithCustomLensPositionSupported returns NO, setFocusModeLockedWithLensPosition: may only be called with AVCaptureLensPositionCurrent. Passing any other lens position will result in an exception.
--
-- ObjC selector: @- lockingFocusWithCustomLensPositionSupported@
lockingFocusWithCustomLensPositionSupported :: IsAVCaptureDevice avCaptureDevice => avCaptureDevice -> IO Bool
lockingFocusWithCustomLensPositionSupported avCaptureDevice =
  sendMessage avCaptureDevice lockingFocusWithCustomLensPositionSupportedSelector

-- | focusMode
--
-- Indicates current focus mode of the receiver, if it has one.
--
-- The value of this property is an AVCaptureFocusMode that determines the receiver's focus mode, if it has one. -setFocusMode: throws an NSInvalidArgumentException if set to an unsupported value (see -isFocusModeSupported:). -setFocusMode: throws an NSGenericException if called without first obtaining exclusive access to the receiver using lockForConfiguration:. Clients can observe automatic changes to the receiver's focusMode by key value observing this property.
--
-- ObjC selector: @- focusMode@
focusMode :: IsAVCaptureDevice avCaptureDevice => avCaptureDevice -> IO AVCaptureFocusMode
focusMode avCaptureDevice =
  sendMessage avCaptureDevice focusModeSelector

-- | focusMode
--
-- Indicates current focus mode of the receiver, if it has one.
--
-- The value of this property is an AVCaptureFocusMode that determines the receiver's focus mode, if it has one. -setFocusMode: throws an NSInvalidArgumentException if set to an unsupported value (see -isFocusModeSupported:). -setFocusMode: throws an NSGenericException if called without first obtaining exclusive access to the receiver using lockForConfiguration:. Clients can observe automatic changes to the receiver's focusMode by key value observing this property.
--
-- ObjC selector: @- setFocusMode:@
setFocusMode :: IsAVCaptureDevice avCaptureDevice => avCaptureDevice -> AVCaptureFocusMode -> IO ()
setFocusMode avCaptureDevice value =
  sendMessage avCaptureDevice setFocusModeSelector value

-- | focusPointOfInterestSupported
--
-- Indicates whether the receiver supports focus points of interest.
--
-- The receiver's focusPointOfInterest property can only be set if this property returns YES.
--
-- ObjC selector: @- focusPointOfInterestSupported@
focusPointOfInterestSupported :: IsAVCaptureDevice avCaptureDevice => avCaptureDevice -> IO Bool
focusPointOfInterestSupported avCaptureDevice =
  sendMessage avCaptureDevice focusPointOfInterestSupportedSelector

-- | Whether the receiver supports focus rectangles of interest.
--
-- You may only set the device's ``focusRectOfInterest`` property if this property returns @true@.
--
-- ObjC selector: @- focusRectOfInterestSupported@
focusRectOfInterestSupported :: IsAVCaptureDevice avCaptureDevice => avCaptureDevice -> IO Bool
focusRectOfInterestSupported avCaptureDevice =
  sendMessage avCaptureDevice focusRectOfInterestSupportedSelector

-- | adjustingFocus
--
-- Indicates whether the receiver is currently performing a focus scan to adjust focus.
--
-- The value of this property is a BOOL indicating whether the receiver's camera focus is being automatically adjusted by means of a focus scan, because its focus mode is AVCaptureFocusModeAutoFocus or AVCaptureFocusModeContinuousAutoFocus. Clients can observe the value of this property to determine whether the camera's focus is stable.
--
-- lensPosition
--
-- AVCaptureAutoFocusSystem
--
-- ObjC selector: @- adjustingFocus@
adjustingFocus :: IsAVCaptureDevice avCaptureDevice => avCaptureDevice -> IO Bool
adjustingFocus avCaptureDevice =
  sendMessage avCaptureDevice adjustingFocusSelector

-- | autoFocusRangeRestrictionSupported
--
-- Indicates whether the receiver supports autofocus range restrictions.
--
-- The receiver's autoFocusRangeRestriction property can only be set if this property returns YES.
--
-- ObjC selector: @- autoFocusRangeRestrictionSupported@
autoFocusRangeRestrictionSupported :: IsAVCaptureDevice avCaptureDevice => avCaptureDevice -> IO Bool
autoFocusRangeRestrictionSupported avCaptureDevice =
  sendMessage avCaptureDevice autoFocusRangeRestrictionSupportedSelector

-- | autoFocusRangeRestriction
--
-- Indicates current restriction of the receiver's autofocus system to a particular range of focus scan, if it supports range restrictions.
--
-- The value of this property is an AVCaptureAutoFocusRangeRestriction indicating how the autofocus system should limit its focus scan. The default value is AVCaptureAutoFocusRangeRestrictionNone. -setAutoFocusRangeRestriction: throws an NSInvalidArgumentException if isAutoFocusRangeRestrictionSupported returns NO. -setAutoFocusRangeRestriction: throws an NSGenericException if called without first obtaining exclusive access to the receiver using lockForConfiguration:. This property only has an effect when the focusMode property is set to AVCaptureFocusModeAutoFocus or AVCaptureFocusModeContinuousAutoFocus. Note that setting autoFocusRangeRestriction alone does not initiate a focus operation. After setting autoFocusRangeRestriction, call -setFocusMode: to apply the new restriction.
--
-- ObjC selector: @- autoFocusRangeRestriction@
autoFocusRangeRestriction :: IsAVCaptureDevice avCaptureDevice => avCaptureDevice -> IO AVCaptureAutoFocusRangeRestriction
autoFocusRangeRestriction avCaptureDevice =
  sendMessage avCaptureDevice autoFocusRangeRestrictionSelector

-- | autoFocusRangeRestriction
--
-- Indicates current restriction of the receiver's autofocus system to a particular range of focus scan, if it supports range restrictions.
--
-- The value of this property is an AVCaptureAutoFocusRangeRestriction indicating how the autofocus system should limit its focus scan. The default value is AVCaptureAutoFocusRangeRestrictionNone. -setAutoFocusRangeRestriction: throws an NSInvalidArgumentException if isAutoFocusRangeRestrictionSupported returns NO. -setAutoFocusRangeRestriction: throws an NSGenericException if called without first obtaining exclusive access to the receiver using lockForConfiguration:. This property only has an effect when the focusMode property is set to AVCaptureFocusModeAutoFocus or AVCaptureFocusModeContinuousAutoFocus. Note that setting autoFocusRangeRestriction alone does not initiate a focus operation. After setting autoFocusRangeRestriction, call -setFocusMode: to apply the new restriction.
--
-- ObjC selector: @- setAutoFocusRangeRestriction:@
setAutoFocusRangeRestriction :: IsAVCaptureDevice avCaptureDevice => avCaptureDevice -> AVCaptureAutoFocusRangeRestriction -> IO ()
setAutoFocusRangeRestriction avCaptureDevice value =
  sendMessage avCaptureDevice setAutoFocusRangeRestrictionSelector value

-- | smoothAutoFocusSupported
--
-- Indicates whether the receiver supports smooth autofocus.
--
-- The receiver's smoothAutoFocusEnabled property can only be set if this property returns YES.
--
-- ObjC selector: @- smoothAutoFocusSupported@
smoothAutoFocusSupported :: IsAVCaptureDevice avCaptureDevice => avCaptureDevice -> IO Bool
smoothAutoFocusSupported avCaptureDevice =
  sendMessage avCaptureDevice smoothAutoFocusSupportedSelector

-- | smoothAutoFocusEnabled
--
-- Indicates whether the receiver should use smooth autofocus.
--
-- On a receiver where -isSmoothAutoFocusSupported returns YES and smoothAutoFocusEnabled is set to YES, a smooth autofocus will be engaged when the focus mode is set to AVCaptureFocusModeAutoFocus or AVCaptureFocusModeContinuousAutoFocus. Enabling smooth autofocus is appropriate for movie recording. Smooth autofocus is slower and less visually invasive. Disabling smooth autofocus is more appropriate for video processing where a fast autofocus is necessary. The default value is NO. Setting this property throws an NSInvalidArgumentException if -isSmoothAutoFocusSupported returns NO. The receiver must be locked for configuration using lockForConfiguration: before clients can set this method, otherwise an NSGenericException is thrown. Note that setting smoothAutoFocusEnabled alone does not initiate a focus operation. After setting smoothAutoFocusEnabled, call -setFocusMode: to apply the new smooth autofocus mode.
--
-- ObjC selector: @- smoothAutoFocusEnabled@
smoothAutoFocusEnabled :: IsAVCaptureDevice avCaptureDevice => avCaptureDevice -> IO Bool
smoothAutoFocusEnabled avCaptureDevice =
  sendMessage avCaptureDevice smoothAutoFocusEnabledSelector

-- | smoothAutoFocusEnabled
--
-- Indicates whether the receiver should use smooth autofocus.
--
-- On a receiver where -isSmoothAutoFocusSupported returns YES and smoothAutoFocusEnabled is set to YES, a smooth autofocus will be engaged when the focus mode is set to AVCaptureFocusModeAutoFocus or AVCaptureFocusModeContinuousAutoFocus. Enabling smooth autofocus is appropriate for movie recording. Smooth autofocus is slower and less visually invasive. Disabling smooth autofocus is more appropriate for video processing where a fast autofocus is necessary. The default value is NO. Setting this property throws an NSInvalidArgumentException if -isSmoothAutoFocusSupported returns NO. The receiver must be locked for configuration using lockForConfiguration: before clients can set this method, otherwise an NSGenericException is thrown. Note that setting smoothAutoFocusEnabled alone does not initiate a focus operation. After setting smoothAutoFocusEnabled, call -setFocusMode: to apply the new smooth autofocus mode.
--
-- ObjC selector: @- setSmoothAutoFocusEnabled:@
setSmoothAutoFocusEnabled :: IsAVCaptureDevice avCaptureDevice => avCaptureDevice -> Bool -> IO ()
setSmoothAutoFocusEnabled avCaptureDevice value =
  sendMessage avCaptureDevice setSmoothAutoFocusEnabledSelector value

-- | automaticallyAdjustsFaceDrivenAutoFocusEnabled
--
-- Indicates whether the receiver should automatically adjust face-driven autofocus.
--
-- The value of this property is a BOOL that determines the receiver's automatic adjustment of face-driven autofocus. Default is YES on all platforms, if the receiver supports autofocus. This property must be set to NO before manually setting faceDrivenAutoFocusEnabled to YES/NO. -setAutomaticallyAdjustsFaceDrivenAutoFocusEnabled: throws an NSInvalidArgumentException if the receiver doesn't support autofocus. -setAutomaticallyAdjustsFaceDrivenAutoFocusEnabled: throws an NSGenericException if called without first obtaining exclusive access to the receiver using -lockForConfiguration:. After setting automaticallyAdjustsFaceDrivenAutoFocusEnabled, call -setFocusMode: to apply the change.
--
-- ObjC selector: @- automaticallyAdjustsFaceDrivenAutoFocusEnabled@
automaticallyAdjustsFaceDrivenAutoFocusEnabled :: IsAVCaptureDevice avCaptureDevice => avCaptureDevice -> IO Bool
automaticallyAdjustsFaceDrivenAutoFocusEnabled avCaptureDevice =
  sendMessage avCaptureDevice automaticallyAdjustsFaceDrivenAutoFocusEnabledSelector

-- | automaticallyAdjustsFaceDrivenAutoFocusEnabled
--
-- Indicates whether the receiver should automatically adjust face-driven autofocus.
--
-- The value of this property is a BOOL that determines the receiver's automatic adjustment of face-driven autofocus. Default is YES on all platforms, if the receiver supports autofocus. This property must be set to NO before manually setting faceDrivenAutoFocusEnabled to YES/NO. -setAutomaticallyAdjustsFaceDrivenAutoFocusEnabled: throws an NSInvalidArgumentException if the receiver doesn't support autofocus. -setAutomaticallyAdjustsFaceDrivenAutoFocusEnabled: throws an NSGenericException if called without first obtaining exclusive access to the receiver using -lockForConfiguration:. After setting automaticallyAdjustsFaceDrivenAutoFocusEnabled, call -setFocusMode: to apply the change.
--
-- ObjC selector: @- setAutomaticallyAdjustsFaceDrivenAutoFocusEnabled:@
setAutomaticallyAdjustsFaceDrivenAutoFocusEnabled :: IsAVCaptureDevice avCaptureDevice => avCaptureDevice -> Bool -> IO ()
setAutomaticallyAdjustsFaceDrivenAutoFocusEnabled avCaptureDevice value =
  sendMessage avCaptureDevice setAutomaticallyAdjustsFaceDrivenAutoFocusEnabledSelector value

-- | faceDrivenAutoFocusEnabled
--
-- Indicates whether face-driven autofocus is enabled on the receiver.
--
-- Default is YES for all apps linked on or after iOS 15.4 when the receiver supports autofocus. -setFaceDrivenAutoFocusEnabled: throws an NSInvalidArgumentException if automaticallyAdjustsFaceDrivenAutoFocusEnabled returns YES.  -setFaceDrivenAutoFocusEnabled: throws an NSInvalidArgumentException if the receiver doesn't support autofocus. -setFaceDrivenAutoFocusEnabled: throws an NSGenericException if called without first obtaining exclusive access to the receiver using -lockForConfiguration:. Note that setting faceDrivenAutoFocusEnabled alone does not initiate this focus change operation. After setting faceDrivenAutoFocusEnabled, call -setFocusMode: to apply the change.
--
-- ObjC selector: @- faceDrivenAutoFocusEnabled@
faceDrivenAutoFocusEnabled :: IsAVCaptureDevice avCaptureDevice => avCaptureDevice -> IO Bool
faceDrivenAutoFocusEnabled avCaptureDevice =
  sendMessage avCaptureDevice faceDrivenAutoFocusEnabledSelector

-- | faceDrivenAutoFocusEnabled
--
-- Indicates whether face-driven autofocus is enabled on the receiver.
--
-- Default is YES for all apps linked on or after iOS 15.4 when the receiver supports autofocus. -setFaceDrivenAutoFocusEnabled: throws an NSInvalidArgumentException if automaticallyAdjustsFaceDrivenAutoFocusEnabled returns YES.  -setFaceDrivenAutoFocusEnabled: throws an NSInvalidArgumentException if the receiver doesn't support autofocus. -setFaceDrivenAutoFocusEnabled: throws an NSGenericException if called without first obtaining exclusive access to the receiver using -lockForConfiguration:. Note that setting faceDrivenAutoFocusEnabled alone does not initiate this focus change operation. After setting faceDrivenAutoFocusEnabled, call -setFocusMode: to apply the change.
--
-- ObjC selector: @- setFaceDrivenAutoFocusEnabled:@
setFaceDrivenAutoFocusEnabled :: IsAVCaptureDevice avCaptureDevice => avCaptureDevice -> Bool -> IO ()
setFaceDrivenAutoFocusEnabled avCaptureDevice value =
  sendMessage avCaptureDevice setFaceDrivenAutoFocusEnabledSelector value

-- | lensPosition
--
-- Indicates the focus position of the lens.
--
-- The range of possible positions is 0.0 to 1.0, with 0.0 being the shortest distance at which the lens can focus and 1.0 the furthest. Note that 1.0 does not represent focus at infinity. The default value is 1.0. Note that a given lens position value does not correspond to an exact physical distance, nor does it represent a consistent focus distance from device to device. This property is key-value observable. It can be read at any time, regardless of focus mode, but can only be set via setFocusModeLockedWithLensPosition:completionHandler:.
--
-- ObjC selector: @- lensPosition@
lensPosition :: IsAVCaptureDevice avCaptureDevice => avCaptureDevice -> IO CFloat
lensPosition avCaptureDevice =
  sendMessage avCaptureDevice lensPositionSelector

-- | minimumFocusDistance
--
-- A property indicating the minimum focus distance.
--
-- The minimum focus distance is given in millimeters, -1 if unknown. For virtual cameras (AVCaptureDeviceTypeBuiltInDualCamera, AVCaptureDeviceTypeBuiltInTripleCamera, etc.), the value reported is the smallest minimum focus distance of the auto-focus-capable cameras that it sources.
--
-- ObjC selector: @- minimumFocusDistance@
minimumFocusDistance :: IsAVCaptureDevice avCaptureDevice => avCaptureDevice -> IO CLong
minimumFocusDistance avCaptureDevice =
  sendMessage avCaptureDevice minimumFocusDistanceSelector

-- | hasTorch
--
-- Indicates whether the receiver has a torch.
--
-- The value of this property is a BOOL indicating whether the receiver has a torch. The receiver's torchMode property can only be set when this property returns YES.
--
-- ObjC selector: @- hasTorch@
hasTorch :: IsAVCaptureDevice avCaptureDevice => avCaptureDevice -> IO Bool
hasTorch avCaptureDevice =
  sendMessage avCaptureDevice hasTorchSelector

-- | torchAvailable
--
-- Indicates whether the receiver's torch is currently available for use.
--
-- The value of this property is a BOOL indicating whether the receiver's torch is currently available. The torch may become unavailable if, for example, the device overheats and needs to cool off. This property is key-value observable.
--
-- ObjC selector: @- torchAvailable@
torchAvailable :: IsAVCaptureDevice avCaptureDevice => avCaptureDevice -> IO Bool
torchAvailable avCaptureDevice =
  sendMessage avCaptureDevice torchAvailableSelector

-- | torchActive
--
-- Indicates whether the receiver's torch is currently active.
--
-- The value of this property is a BOOL indicating whether the receiver's torch is currently active. If the current torchMode is AVCaptureTorchModeAuto and isTorchActive is YES, the torch will illuminate once a recording starts (see AVCaptureOutput.h -startRecordingToOutputFileURL:recordingDelegate:). This property is key-value observable.
--
-- ObjC selector: @- torchActive@
torchActive :: IsAVCaptureDevice avCaptureDevice => avCaptureDevice -> IO Bool
torchActive avCaptureDevice =
  sendMessage avCaptureDevice torchActiveSelector

-- | torchLevel
--
-- Indicates the receiver's current torch brightness level as a floating point value.
--
-- The value of this property is a float indicating the receiver's torch level from 0.0 (off) -> 1.0 (full). This property is key-value observable.
--
-- ObjC selector: @- torchLevel@
torchLevel :: IsAVCaptureDevice avCaptureDevice => avCaptureDevice -> IO CFloat
torchLevel avCaptureDevice =
  sendMessage avCaptureDevice torchLevelSelector

-- | torchMode
--
-- Indicates current mode of the receiver's torch, if it has one.
--
-- The value of this property is an AVCaptureTorchMode that determines the mode of the receiver's torch, if it has one. -setTorchMode: throws an NSInvalidArgumentException if set to an unsupported value (see -isTorchModeSupported:). -setTorchMode: throws an NSGenericException if called without first obtaining exclusive access to the receiver using lockForConfiguration:. Clients can observe automatic changes to the receiver's torchMode by key value observing this property.
--
-- ObjC selector: @- torchMode@
torchMode :: IsAVCaptureDevice avCaptureDevice => avCaptureDevice -> IO AVCaptureTorchMode
torchMode avCaptureDevice =
  sendMessage avCaptureDevice torchModeSelector

-- | torchMode
--
-- Indicates current mode of the receiver's torch, if it has one.
--
-- The value of this property is an AVCaptureTorchMode that determines the mode of the receiver's torch, if it has one. -setTorchMode: throws an NSInvalidArgumentException if set to an unsupported value (see -isTorchModeSupported:). -setTorchMode: throws an NSGenericException if called without first obtaining exclusive access to the receiver using lockForConfiguration:. Clients can observe automatic changes to the receiver's torchMode by key value observing this property.
--
-- ObjC selector: @- setTorchMode:@
setTorchMode :: IsAVCaptureDevice avCaptureDevice => avCaptureDevice -> AVCaptureTorchMode -> IO ()
setTorchMode avCaptureDevice value =
  sendMessage avCaptureDevice setTorchModeSelector value

-- | hasFlash
--
-- Indicates whether the receiver has a flash.
--
-- The value of this property is a BOOL indicating whether the receiver has a flash. The receiver's flashMode property can only be set when this property returns YES.
--
-- ObjC selector: @- hasFlash@
hasFlash :: IsAVCaptureDevice avCaptureDevice => avCaptureDevice -> IO Bool
hasFlash avCaptureDevice =
  sendMessage avCaptureDevice hasFlashSelector

-- | flashAvailable
--
-- Indicates whether the receiver's flash is currently available for use.
--
-- The value of this property is a BOOL indicating whether the receiver's flash is currently available. The flash may become unavailable if, for example, the device overheats and needs to cool off. This property is key-value observable.
--
-- ObjC selector: @- flashAvailable@
flashAvailable :: IsAVCaptureDevice avCaptureDevice => avCaptureDevice -> IO Bool
flashAvailable avCaptureDevice =
  sendMessage avCaptureDevice flashAvailableSelector

-- | flashActive
--
-- Indicates whether the receiver's flash is currently active.
--
-- The value of this property is a BOOL indicating whether the receiver's flash is currently active. When the flash is active, it will flash if a still image is captured. When a still image is captured with the flash active, exposure and white balance settings are overridden for the still. This is true even when using AVCaptureExposureModeCustom and/or AVCaptureWhiteBalanceModeLocked. This property is key-value observable.
--
-- ObjC selector: @- flashActive@
flashActive :: IsAVCaptureDevice avCaptureDevice => avCaptureDevice -> IO Bool
flashActive avCaptureDevice =
  sendMessage avCaptureDevice flashActiveSelector

-- | flashMode
--
-- Indicates current mode of the receiver's flash, if it has one.
--
-- The value of this property is an AVCaptureFlashMode that determines the mode of the receiver's flash, if it has one. -setFlashMode: throws an NSInvalidArgumentException if set to an unsupported value (see -isFlashModeSupported:). -setFlashMode: throws an NSGenericException if called without first obtaining exclusive access to the receiver using lockForConfiguration:. Clients can observe automatic changes to the receiver's flashMode by key value observing this property.
--
-- When using AVCapturePhotoOutput, AVCaptureDevice's flashMode property is ignored. You specify flashMode on a per photo basis by setting the AVCapturePhotoSettings.flashMode property.
--
-- ObjC selector: @- flashMode@
flashMode :: IsAVCaptureDevice avCaptureDevice => avCaptureDevice -> IO AVCaptureFlashMode
flashMode avCaptureDevice =
  sendMessage avCaptureDevice flashModeSelector

-- | flashMode
--
-- Indicates current mode of the receiver's flash, if it has one.
--
-- The value of this property is an AVCaptureFlashMode that determines the mode of the receiver's flash, if it has one. -setFlashMode: throws an NSInvalidArgumentException if set to an unsupported value (see -isFlashModeSupported:). -setFlashMode: throws an NSGenericException if called without first obtaining exclusive access to the receiver using lockForConfiguration:. Clients can observe automatic changes to the receiver's flashMode by key value observing this property.
--
-- When using AVCapturePhotoOutput, AVCaptureDevice's flashMode property is ignored. You specify flashMode on a per photo basis by setting the AVCapturePhotoSettings.flashMode property.
--
-- ObjC selector: @- setFlashMode:@
setFlashMode :: IsAVCaptureDevice avCaptureDevice => avCaptureDevice -> AVCaptureFlashMode -> IO ()
setFlashMode avCaptureDevice value =
  sendMessage avCaptureDevice setFlashModeSelector value

-- | virtualDevice
--
-- A property indicating whether the receiver is a virtual device consisting of constituent physical devices.
--
-- Two examples of virtual devices are:        The Dual Camera, which supports seamlessly switching between a wide and telephoto camera while zooming and generating depth data from the disparities between the different points of view of the physical cameras.        The TrueDepth Camera, which generates depth data from disparities between a YUV camera and an Infrared camera pointed in the same direction.
--
-- ObjC selector: @- virtualDevice@
virtualDevice :: IsAVCaptureDevice avCaptureDevice => avCaptureDevice -> IO Bool
virtualDevice avCaptureDevice =
  sendMessage avCaptureDevice virtualDeviceSelector

-- | constituentDevices
--
-- An array of constituent physical devices comprising a virtual device.
--
-- When called on a device for which virtualDevice == NO, an empty array is returned.
--
-- ObjC selector: @- constituentDevices@
constituentDevices :: IsAVCaptureDevice avCaptureDevice => avCaptureDevice -> IO (Id NSArray)
constituentDevices avCaptureDevice =
  sendMessage avCaptureDevice constituentDevicesSelector

-- | virtualDeviceSwitchOverVideoZoomFactors
--
-- An array of video zoom factors at or above which a virtual device (such as the Dual Camera) may switch to its next constituent device.
--
-- This array contains zoom factors at which one of the constituent device's field of view matches the next constituent device's full field of view. The number of switch over video zoom factors is always one less than the count of the constituentDevices property, and the factors progress in the same order as the devices listed in that property. On non-virtual devices this property returns an empty array.
--
-- ObjC selector: @- virtualDeviceSwitchOverVideoZoomFactors@
virtualDeviceSwitchOverVideoZoomFactors :: IsAVCaptureDevice avCaptureDevice => avCaptureDevice -> IO (Id NSArray)
virtualDeviceSwitchOverVideoZoomFactors avCaptureDevice =
  sendMessage avCaptureDevice virtualDeviceSwitchOverVideoZoomFactorsSelector

-- | primaryConstituentDeviceSwitchingBehavior
--
-- The primaryConstituentDeviceSwitchingBehavior as set by -[AVCaptureDevice setPrimaryConstituentDeviceSwitchingBehavior:restrictedSwitchingBehaviorConditions:].
--
-- By default, this property is set to AVCapturePrimaryConstituentDeviceSwitchingBehaviorAuto for AVCaptureDevices that support it.  This property is key-value observable.
--
-- ObjC selector: @- primaryConstituentDeviceSwitchingBehavior@
primaryConstituentDeviceSwitchingBehavior :: IsAVCaptureDevice avCaptureDevice => avCaptureDevice -> IO AVCapturePrimaryConstituentDeviceSwitchingBehavior
primaryConstituentDeviceSwitchingBehavior avCaptureDevice =
  sendMessage avCaptureDevice primaryConstituentDeviceSwitchingBehaviorSelector

-- | primaryConstituentDeviceRestrictedSwitchingBehaviorConditions
--
-- The primaryConstituentDeviceRestrictedSwitchingBehaviorConditions as set by -[AVCaptureDevice setPrimaryConstituentDeviceSwitchingBehavior:restrictedSwitchingBehaviorConditions:].
--
-- By default, this propety is set to AVCapturePrimaryConstituentDeviceRestrictedSwitchingBehaviorConditionNone. This property is key-value observable.
--
-- ObjC selector: @- primaryConstituentDeviceRestrictedSwitchingBehaviorConditions@
primaryConstituentDeviceRestrictedSwitchingBehaviorConditions :: IsAVCaptureDevice avCaptureDevice => avCaptureDevice -> IO AVCapturePrimaryConstituentDeviceRestrictedSwitchingBehaviorConditions
primaryConstituentDeviceRestrictedSwitchingBehaviorConditions avCaptureDevice =
  sendMessage avCaptureDevice primaryConstituentDeviceRestrictedSwitchingBehaviorConditionsSelector

-- | activePrimaryConstituentDeviceSwitchingBehavior
--
-- The active constituent device switching behavior.
--
-- For virtual devices with multiple constituent devices, this property returns the active switching behavior. This is equal to primaryConstituentDeviceSwitchingBehavior except while recording using an AVCaptureMovieFileOutput configured with a different switching behavior (see -[AVCaptureMovieFileOutput setPrimaryConstituentDeviceSwitchingBehavior:restrictedSwitchingBehaviorConditions]). Devices that do not support constituent device switching return AVCapturePrimaryConstituentDeviceSwitchingBehaviorUnsupported. This property is key-value observable.
--
-- ObjC selector: @- activePrimaryConstituentDeviceSwitchingBehavior@
activePrimaryConstituentDeviceSwitchingBehavior :: IsAVCaptureDevice avCaptureDevice => avCaptureDevice -> IO AVCapturePrimaryConstituentDeviceSwitchingBehavior
activePrimaryConstituentDeviceSwitchingBehavior avCaptureDevice =
  sendMessage avCaptureDevice activePrimaryConstituentDeviceSwitchingBehaviorSelector

-- | activePrimaryConstituentDeviceRestrictedSwitchingBehaviorConditions
--
-- The active constituent device restricted  switching behavior.
--
-- For virtual devices with multiple constituent devices, this property returns the active restricted switching behavior conditions. This is equal to primaryConstituentDeviceRestrictedSwitchingBehaviorConditions except while recording using an AVCaptureMovieFileOutput configured with different restricted switching behavior conditions (see -[AVCaptureMovieFileOutput setPrimaryConstituentDeviceSwitchingBehaviorForRecording:restrictedSwitchingBehaviorConditions]). Devices that do not support constituent device switching return AVCapturePrimaryConstituentDeviceRestrictedSwitchingBehaviorConditionNone. This property is key-value observable.
--
-- ObjC selector: @- activePrimaryConstituentDeviceRestrictedSwitchingBehaviorConditions@
activePrimaryConstituentDeviceRestrictedSwitchingBehaviorConditions :: IsAVCaptureDevice avCaptureDevice => avCaptureDevice -> IO AVCapturePrimaryConstituentDeviceRestrictedSwitchingBehaviorConditions
activePrimaryConstituentDeviceRestrictedSwitchingBehaviorConditions avCaptureDevice =
  sendMessage avCaptureDevice activePrimaryConstituentDeviceRestrictedSwitchingBehaviorConditionsSelector

-- | activePrimaryConstituentDevice
--
-- For virtual devices, this property indicates which constituent device is currently the primary constituent device. The primary constituent device may change when zoom, exposure, or focus changes.
--
-- This property returns nil for non-virtual devices. On virtual devices this property returns nil until the device is used in a running AVCaptureSession. This property is key-value observable.
--
-- ObjC selector: @- activePrimaryConstituentDevice@
activePrimaryConstituentDevice :: IsAVCaptureDevice avCaptureDevice => avCaptureDevice -> IO (Id AVCaptureDevice)
activePrimaryConstituentDevice avCaptureDevice =
  sendMessage avCaptureDevice activePrimaryConstituentDeviceSelector

-- | supportedFallbackPrimaryConstituentDevices
--
-- The constituent devices that may be selected as a fallback for a longer focal length primary constituent device.
--
-- This property returns an empty array for non-virtual devices. This property never changes for a given virtual device.
--
-- ObjC selector: @- supportedFallbackPrimaryConstituentDevices@
supportedFallbackPrimaryConstituentDevices :: IsAVCaptureDevice avCaptureDevice => avCaptureDevice -> IO (Id NSArray)
supportedFallbackPrimaryConstituentDevices avCaptureDevice =
  sendMessage avCaptureDevice supportedFallbackPrimaryConstituentDevicesSelector

-- | fallbackPrimaryConstituentDevices
--
-- The constituent devices that may be used as a fallback device when a constituent device with a longer focal length becomes limited by its light sensitivity or minimum focus distance.
--
-- This may only be set to the supportedFallbackPrimaryConstituentDevices or a subset thereof. By default this is set to all supportedFallbackPrimaryConstituentDevices. This property will throw an NSInvalidArgumentException if the array includes any device not reported in supportedFallbackPrimaryConstituentDevices. This property is key-value observable.
--
-- ObjC selector: @- fallbackPrimaryConstituentDevices@
fallbackPrimaryConstituentDevices :: IsAVCaptureDevice avCaptureDevice => avCaptureDevice -> IO (Id NSArray)
fallbackPrimaryConstituentDevices avCaptureDevice =
  sendMessage avCaptureDevice fallbackPrimaryConstituentDevicesSelector

-- | fallbackPrimaryConstituentDevices
--
-- The constituent devices that may be used as a fallback device when a constituent device with a longer focal length becomes limited by its light sensitivity or minimum focus distance.
--
-- This may only be set to the supportedFallbackPrimaryConstituentDevices or a subset thereof. By default this is set to all supportedFallbackPrimaryConstituentDevices. This property will throw an NSInvalidArgumentException if the array includes any device not reported in supportedFallbackPrimaryConstituentDevices. This property is key-value observable.
--
-- ObjC selector: @- setFallbackPrimaryConstituentDevices:@
setFallbackPrimaryConstituentDevices :: (IsAVCaptureDevice avCaptureDevice, IsNSArray value) => avCaptureDevice -> value -> IO ()
setFallbackPrimaryConstituentDevices avCaptureDevice value =
  sendMessage avCaptureDevice setFallbackPrimaryConstituentDevicesSelector (toNSArray value)

-- | systemPressureState
--
-- A key-value observable property indicating the capture device's current system pressure state.
--
-- This property indicates whether the capture device is currently subject to an elevated system pressure condition. When system pressure reaches AVCaptureSystemPressureLevelShutdown, the capture device cannot continue to provide input, so the AVCaptureSession becomes interrupted until the pressured state abates. System pressure can be effectively mitigated by lowering the device's activeVideoMinFrameDuration in response to changes in the systemPressureState. Clients are encouraged to implement frame rate throttling to bring system pressure down if their capture use case can tolerate a reduced frame rate.
--
-- ObjC selector: @- systemPressureState@
systemPressureState :: IsAVCaptureDevice avCaptureDevice => avCaptureDevice -> IO (Id AVCaptureSystemPressureState)
systemPressureState avCaptureDevice =
  sendMessage avCaptureDevice systemPressureStateSelector

-- | userPreferredCamera
--
-- Settable property that specifies a user preferred camera.
--
-- Setting this property allows an application to persist its user’s preferred camera across app launches and reboots. The property internally maintains a short history, so if your user’s most recent preferred camera is not currently connected, it still reports the next best choice. This property always returns a device that is present. If no camera is available nil is returned. Setting the property to nil has no effect.
--
-- ObjC selector: @+ userPreferredCamera@
userPreferredCamera :: IO (Id AVCaptureDevice)
userPreferredCamera  =
  do
    cls' <- getRequiredClass "AVCaptureDevice"
    sendClassMessage cls' userPreferredCameraSelector

-- | userPreferredCamera
--
-- Settable property that specifies a user preferred camera.
--
-- Setting this property allows an application to persist its user’s preferred camera across app launches and reboots. The property internally maintains a short history, so if your user’s most recent preferred camera is not currently connected, it still reports the next best choice. This property always returns a device that is present. If no camera is available nil is returned. Setting the property to nil has no effect.
--
-- ObjC selector: @+ setUserPreferredCamera:@
setUserPreferredCamera :: IsAVCaptureDevice value => value -> IO ()
setUserPreferredCamera value =
  do
    cls' <- getRequiredClass "AVCaptureDevice"
    sendClassMessage cls' setUserPreferredCameraSelector (toAVCaptureDevice value)

-- | systemPreferredCamera
--
-- Specifies the best camera to use as determined by the system.
--
-- Apple chooses the default value. This property incorporates userPreferredCamera as well as other factors, such as camera suspension and Apple cameras appearing that should be automatically chosen. The property may change spontaneously, such as when the preferred camera goes away. This property always returns a device that is present. If no camera is available nil is returned.
--
-- Applications that adopt this API should always key-value observe this property and update their AVCaptureSession’s input device to reflect changes to the systemPreferredCamera. The application can still offer users the ability to pick a camera by setting userPreferredCamera, which will cause the systemPreferredCamera API to put the user’s choice first until either another Apple-preferred device becomes available or the machine is rebooted (after which it reverts to its original behavior of returning the internally determined best camera to use).
--
-- If the application wishes to offer users a fully manual camera selection mode in addition to automatic camera selection, it is recommended to call setUserPreferredCamera: each time the user makes a camera selection, but ignore key-value observer updates to systemPreferredCamera while in manual selection mode.
--
-- ObjC selector: @+ systemPreferredCamera@
systemPreferredCamera :: IO (Id AVCaptureDevice)
systemPreferredCamera  =
  do
    cls' <- getRequiredClass "AVCaptureDevice"
    sendClassMessage cls' systemPreferredCameraSelector

-- | deviceType
--
-- The type of the capture device.
--
-- A capture device's type never changes.
--
-- ObjC selector: @- deviceType@
deviceType :: IsAVCaptureDevice avCaptureDevice => avCaptureDevice -> IO (Id NSString)
deviceType avCaptureDevice =
  sendMessage avCaptureDevice deviceTypeSelector

-- | position
--
-- Indicates the physical position of an AVCaptureDevice's hardware on the system.
--
-- The value of this property is an AVCaptureDevicePosition indicating where the receiver's device is physically located on the system hardware.
--
-- ObjC selector: @- position@
position :: IsAVCaptureDevice avCaptureDevice => avCaptureDevice -> IO AVCaptureDevicePosition
position avCaptureDevice =
  sendMessage avCaptureDevice positionSelector

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @init@
initSelector :: Selector '[] (Id AVCaptureDevice)
initSelector = mkSelector "init"

-- | @Selector@ for @new@
newSelector :: Selector '[] (Id AVCaptureDevice)
newSelector = mkSelector "new"

-- | @Selector@ for @devices@
devicesSelector :: Selector '[] (Id NSArray)
devicesSelector = mkSelector "devices"

-- | @Selector@ for @devicesWithMediaType:@
devicesWithMediaTypeSelector :: Selector '[Id NSString] (Id NSArray)
devicesWithMediaTypeSelector = mkSelector "devicesWithMediaType:"

-- | @Selector@ for @defaultDeviceWithMediaType:@
defaultDeviceWithMediaTypeSelector :: Selector '[Id NSString] (Id AVCaptureDevice)
defaultDeviceWithMediaTypeSelector = mkSelector "defaultDeviceWithMediaType:"

-- | @Selector@ for @deviceWithUniqueID:@
deviceWithUniqueIDSelector :: Selector '[Id NSString] (Id AVCaptureDevice)
deviceWithUniqueIDSelector = mkSelector "deviceWithUniqueID:"

-- | @Selector@ for @hasMediaType:@
hasMediaTypeSelector :: Selector '[Id NSString] Bool
hasMediaTypeSelector = mkSelector "hasMediaType:"

-- | @Selector@ for @lockForConfiguration:@
lockForConfigurationSelector :: Selector '[Id NSError] Bool
lockForConfigurationSelector = mkSelector "lockForConfiguration:"

-- | @Selector@ for @unlockForConfiguration@
unlockForConfigurationSelector :: Selector '[] ()
unlockForConfigurationSelector = mkSelector "unlockForConfiguration"

-- | @Selector@ for @supportsAVCaptureSessionPreset:@
supportsAVCaptureSessionPresetSelector :: Selector '[Id NSString] Bool
supportsAVCaptureSessionPresetSelector = mkSelector "supportsAVCaptureSessionPreset:"

-- | @Selector@ for @setDynamicAspectRatio:completionHandler:@
setDynamicAspectRatio_completionHandlerSelector :: Selector '[Id NSString, Ptr ()] ()
setDynamicAspectRatio_completionHandlerSelector = mkSelector "setDynamicAspectRatio:completionHandler:"

-- | @Selector@ for @showSystemUserInterface:@
showSystemUserInterfaceSelector :: Selector '[AVCaptureSystemUserInterface] ()
showSystemUserInterfaceSelector = mkSelector "showSystemUserInterface:"

-- | @Selector@ for @performEffectForReaction:@
performEffectForReactionSelector :: Selector '[Id NSString] ()
performEffectForReactionSelector = mkSelector "performEffectForReaction:"

-- | @Selector@ for @extrinsicMatrixFromDevice:toDevice:@
extrinsicMatrixFromDevice_toDeviceSelector :: Selector '[Id AVCaptureDevice, Id AVCaptureDevice] (Id NSData)
extrinsicMatrixFromDevice_toDeviceSelector = mkSelector "extrinsicMatrixFromDevice:toDevice:"

-- | @Selector@ for @setTransportControlsPlaybackMode:speed:@
setTransportControlsPlaybackMode_speedSelector :: Selector '[AVCaptureDeviceTransportControlsPlaybackMode, CFloat] ()
setTransportControlsPlaybackMode_speedSelector = mkSelector "setTransportControlsPlaybackMode:speed:"

-- | @Selector@ for @authorizationStatusForMediaType:@
authorizationStatusForMediaTypeSelector :: Selector '[Id NSString] AVAuthorizationStatus
authorizationStatusForMediaTypeSelector = mkSelector "authorizationStatusForMediaType:"

-- | @Selector@ for @requestAccessForMediaType:completionHandler:@
requestAccessForMediaType_completionHandlerSelector :: Selector '[Id NSString, Ptr ()] ()
requestAccessForMediaType_completionHandlerSelector = mkSelector "requestAccessForMediaType:completionHandler:"

-- | @Selector@ for @rampToVideoZoomFactor:withRate:@
rampToVideoZoomFactor_withRateSelector :: Selector '[CDouble, CFloat] ()
rampToVideoZoomFactor_withRateSelector = mkSelector "rampToVideoZoomFactor:withRate:"

-- | @Selector@ for @cancelVideoZoomRamp@
cancelVideoZoomRampSelector :: Selector '[] ()
cancelVideoZoomRampSelector = mkSelector "cancelVideoZoomRamp"

-- | @Selector@ for @isWhiteBalanceModeSupported:@
isWhiteBalanceModeSupportedSelector :: Selector '[AVCaptureWhiteBalanceMode] Bool
isWhiteBalanceModeSupportedSelector = mkSelector "isWhiteBalanceModeSupported:"

-- | @Selector@ for @isExposureModeSupported:@
isExposureModeSupportedSelector :: Selector '[AVCaptureExposureMode] Bool
isExposureModeSupportedSelector = mkSelector "isExposureModeSupported:"

-- | @Selector@ for @setExposureTargetBias:completionHandler:@
setExposureTargetBias_completionHandlerSelector :: Selector '[CFloat, Ptr ()] ()
setExposureTargetBias_completionHandlerSelector = mkSelector "setExposureTargetBias:completionHandler:"

-- | @Selector@ for @isFocusModeSupported:@
isFocusModeSupportedSelector :: Selector '[AVCaptureFocusMode] Bool
isFocusModeSupportedSelector = mkSelector "isFocusModeSupported:"

-- | @Selector@ for @setFocusModeLockedWithLensPosition:completionHandler:@
setFocusModeLockedWithLensPosition_completionHandlerSelector :: Selector '[CFloat, Ptr ()] ()
setFocusModeLockedWithLensPosition_completionHandlerSelector = mkSelector "setFocusModeLockedWithLensPosition:completionHandler:"

-- | @Selector@ for @setCinematicVideoTrackingFocusWithDetectedObjectID:focusMode:@
setCinematicVideoTrackingFocusWithDetectedObjectID_focusModeSelector :: Selector '[CLong, AVCaptureCinematicVideoFocusMode] ()
setCinematicVideoTrackingFocusWithDetectedObjectID_focusModeSelector = mkSelector "setCinematicVideoTrackingFocusWithDetectedObjectID:focusMode:"

-- | @Selector@ for @isTorchModeSupported:@
isTorchModeSupportedSelector :: Selector '[AVCaptureTorchMode] Bool
isTorchModeSupportedSelector = mkSelector "isTorchModeSupported:"

-- | @Selector@ for @setTorchModeOnWithLevel:error:@
setTorchModeOnWithLevel_errorSelector :: Selector '[CFloat, Id NSError] Bool
setTorchModeOnWithLevel_errorSelector = mkSelector "setTorchModeOnWithLevel:error:"

-- | @Selector@ for @isFlashModeSupported:@
isFlashModeSupportedSelector :: Selector '[AVCaptureFlashMode] Bool
isFlashModeSupportedSelector = mkSelector "isFlashModeSupported:"

-- | @Selector@ for @setPrimaryConstituentDeviceSwitchingBehavior:restrictedSwitchingBehaviorConditions:@
setPrimaryConstituentDeviceSwitchingBehavior_restrictedSwitchingBehaviorConditionsSelector :: Selector '[AVCapturePrimaryConstituentDeviceSwitchingBehavior, AVCapturePrimaryConstituentDeviceRestrictedSwitchingBehaviorConditions] ()
setPrimaryConstituentDeviceSwitchingBehavior_restrictedSwitchingBehaviorConditionsSelector = mkSelector "setPrimaryConstituentDeviceSwitchingBehavior:restrictedSwitchingBehaviorConditions:"

-- | @Selector@ for @defaultDeviceWithDeviceType:mediaType:position:@
defaultDeviceWithDeviceType_mediaType_positionSelector :: Selector '[Id NSString, Id NSString, AVCaptureDevicePosition] (Id AVCaptureDevice)
defaultDeviceWithDeviceType_mediaType_positionSelector = mkSelector "defaultDeviceWithDeviceType:mediaType:position:"

-- | @Selector@ for @uniqueID@
uniqueIDSelector :: Selector '[] (Id NSString)
uniqueIDSelector = mkSelector "uniqueID"

-- | @Selector@ for @modelID@
modelIDSelector :: Selector '[] (Id NSString)
modelIDSelector = mkSelector "modelID"

-- | @Selector@ for @localizedName@
localizedNameSelector :: Selector '[] (Id NSString)
localizedNameSelector = mkSelector "localizedName"

-- | @Selector@ for @manufacturer@
manufacturerSelector :: Selector '[] (Id NSString)
manufacturerSelector = mkSelector "manufacturer"

-- | @Selector@ for @transportType@
transportTypeSelector :: Selector '[] CInt
transportTypeSelector = mkSelector "transportType"

-- | @Selector@ for @connected@
connectedSelector :: Selector '[] Bool
connectedSelector = mkSelector "connected"

-- | @Selector@ for @inUseByAnotherApplication@
inUseByAnotherApplicationSelector :: Selector '[] Bool
inUseByAnotherApplicationSelector = mkSelector "inUseByAnotherApplication"

-- | @Selector@ for @suspended@
suspendedSelector :: Selector '[] Bool
suspendedSelector = mkSelector "suspended"

-- | @Selector@ for @linkedDevices@
linkedDevicesSelector :: Selector '[] (Id NSArray)
linkedDevicesSelector = mkSelector "linkedDevices"

-- | @Selector@ for @formats@
formatsSelector :: Selector '[] (Id NSArray)
formatsSelector = mkSelector "formats"

-- | @Selector@ for @activeFormat@
activeFormatSelector :: Selector '[] (Id AVCaptureDeviceFormat)
activeFormatSelector = mkSelector "activeFormat"

-- | @Selector@ for @setActiveFormat:@
setActiveFormatSelector :: Selector '[Id AVCaptureDeviceFormat] ()
setActiveFormatSelector = mkSelector "setActiveFormat:"

-- | @Selector@ for @videoFrameDurationLocked@
videoFrameDurationLockedSelector :: Selector '[] Bool
videoFrameDurationLockedSelector = mkSelector "videoFrameDurationLocked"

-- | @Selector@ for @followingExternalSyncDevice@
followingExternalSyncDeviceSelector :: Selector '[] Bool
followingExternalSyncDeviceSelector = mkSelector "followingExternalSyncDevice"

-- | @Selector@ for @autoVideoFrameRateEnabled@
autoVideoFrameRateEnabledSelector :: Selector '[] Bool
autoVideoFrameRateEnabledSelector = mkSelector "autoVideoFrameRateEnabled"

-- | @Selector@ for @setAutoVideoFrameRateEnabled:@
setAutoVideoFrameRateEnabledSelector :: Selector '[Bool] ()
setAutoVideoFrameRateEnabledSelector = mkSelector "setAutoVideoFrameRateEnabled:"

-- | @Selector@ for @inputSources@
inputSourcesSelector :: Selector '[] (Id NSArray)
inputSourcesSelector = mkSelector "inputSources"

-- | @Selector@ for @activeInputSource@
activeInputSourceSelector :: Selector '[] (Id AVCaptureDeviceInputSource)
activeInputSourceSelector = mkSelector "activeInputSource"

-- | @Selector@ for @setActiveInputSource:@
setActiveInputSourceSelector :: Selector '[Id AVCaptureDeviceInputSource] ()
setActiveInputSourceSelector = mkSelector "setActiveInputSource:"

-- | @Selector@ for @cameraLensSmudgeDetectionEnabled@
cameraLensSmudgeDetectionEnabledSelector :: Selector '[] Bool
cameraLensSmudgeDetectionEnabledSelector = mkSelector "cameraLensSmudgeDetectionEnabled"

-- | @Selector@ for @cameraLensSmudgeDetectionStatus@
cameraLensSmudgeDetectionStatusSelector :: Selector '[] AVCaptureCameraLensSmudgeDetectionStatus
cameraLensSmudgeDetectionStatusSelector = mkSelector "cameraLensSmudgeDetectionStatus"

-- | @Selector@ for @edgeLightEnabled@
edgeLightEnabledSelector :: Selector '[] Bool
edgeLightEnabledSelector = mkSelector "edgeLightEnabled"

-- | @Selector@ for @edgeLightActive@
edgeLightActiveSelector :: Selector '[] Bool
edgeLightActiveSelector = mkSelector "edgeLightActive"

-- | @Selector@ for @studioLightEnabled@
studioLightEnabledSelector :: Selector '[] Bool
studioLightEnabledSelector = mkSelector "studioLightEnabled"

-- | @Selector@ for @studioLightActive@
studioLightActiveSelector :: Selector '[] Bool
studioLightActiveSelector = mkSelector "studioLightActive"

-- | @Selector@ for @nominalFocalLengthIn35mmFilm@
nominalFocalLengthIn35mmFilmSelector :: Selector '[] CFloat
nominalFocalLengthIn35mmFilmSelector = mkSelector "nominalFocalLengthIn35mmFilm"

-- | @Selector@ for @smartFramingMonitor@
smartFramingMonitorSelector :: Selector '[] (Id AVCaptureSmartFramingMonitor)
smartFramingMonitorSelector = mkSelector "smartFramingMonitor"

-- | @Selector@ for @dynamicAspectRatio@
dynamicAspectRatioSelector :: Selector '[] (Id NSString)
dynamicAspectRatioSelector = mkSelector "dynamicAspectRatio"

-- | @Selector@ for @cinematicVideoCaptureSceneMonitoringStatuses@
cinematicVideoCaptureSceneMonitoringStatusesSelector :: Selector '[] (Id NSSet)
cinematicVideoCaptureSceneMonitoringStatusesSelector = mkSelector "cinematicVideoCaptureSceneMonitoringStatuses"

-- | @Selector@ for @spatialCaptureDiscomfortReasons@
spatialCaptureDiscomfortReasonsSelector :: Selector '[] (Id NSSet)
spatialCaptureDiscomfortReasonsSelector = mkSelector "spatialCaptureDiscomfortReasons"

-- | @Selector@ for @preferredMicrophoneMode@
preferredMicrophoneModeSelector :: Selector '[] AVCaptureMicrophoneMode
preferredMicrophoneModeSelector = mkSelector "preferredMicrophoneMode"

-- | @Selector@ for @activeMicrophoneMode@
activeMicrophoneModeSelector :: Selector '[] AVCaptureMicrophoneMode
activeMicrophoneModeSelector = mkSelector "activeMicrophoneMode"

-- | @Selector@ for @companionDeskViewCamera@
companionDeskViewCameraSelector :: Selector '[] (Id AVCaptureDevice)
companionDeskViewCameraSelector = mkSelector "companionDeskViewCamera"

-- | @Selector@ for @continuityCamera@
continuityCameraSelector :: Selector '[] Bool
continuityCameraSelector = mkSelector "continuityCamera"

-- | @Selector@ for @backgroundReplacementEnabled@
backgroundReplacementEnabledSelector :: Selector '[] Bool
backgroundReplacementEnabledSelector = mkSelector "backgroundReplacementEnabled"

-- | @Selector@ for @backgroundReplacementActive@
backgroundReplacementActiveSelector :: Selector '[] Bool
backgroundReplacementActiveSelector = mkSelector "backgroundReplacementActive"

-- | @Selector@ for @reactionEffectsEnabled@
reactionEffectsEnabledSelector :: Selector '[] Bool
reactionEffectsEnabledSelector = mkSelector "reactionEffectsEnabled"

-- | @Selector@ for @reactionEffectGesturesEnabled@
reactionEffectGesturesEnabledSelector :: Selector '[] Bool
reactionEffectGesturesEnabledSelector = mkSelector "reactionEffectGesturesEnabled"

-- | @Selector@ for @canPerformReactionEffects@
canPerformReactionEffectsSelector :: Selector '[] Bool
canPerformReactionEffectsSelector = mkSelector "canPerformReactionEffects"

-- | @Selector@ for @availableReactionTypes@
availableReactionTypesSelector :: Selector '[] (Id NSSet)
availableReactionTypesSelector = mkSelector "availableReactionTypes"

-- | @Selector@ for @reactionEffectsInProgress@
reactionEffectsInProgressSelector :: Selector '[] (Id NSArray)
reactionEffectsInProgressSelector = mkSelector "reactionEffectsInProgress"

-- | @Selector@ for @portraitEffectEnabled@
portraitEffectEnabledSelector :: Selector '[] Bool
portraitEffectEnabledSelector = mkSelector "portraitEffectEnabled"

-- | @Selector@ for @portraitEffectActive@
portraitEffectActiveSelector :: Selector '[] Bool
portraitEffectActiveSelector = mkSelector "portraitEffectActive"

-- | @Selector@ for @centerStageControlMode@
centerStageControlModeSelector :: Selector '[] AVCaptureCenterStageControlMode
centerStageControlModeSelector = mkSelector "centerStageControlMode"

-- | @Selector@ for @setCenterStageControlMode:@
setCenterStageControlModeSelector :: Selector '[AVCaptureCenterStageControlMode] ()
setCenterStageControlModeSelector = mkSelector "setCenterStageControlMode:"

-- | @Selector@ for @centerStageEnabled@
centerStageEnabledSelector :: Selector '[] Bool
centerStageEnabledSelector = mkSelector "centerStageEnabled"

-- | @Selector@ for @setCenterStageEnabled:@
setCenterStageEnabledSelector :: Selector '[Bool] ()
setCenterStageEnabledSelector = mkSelector "setCenterStageEnabled:"

-- | @Selector@ for @centerStageActive@
centerStageActiveSelector :: Selector '[] Bool
centerStageActiveSelector = mkSelector "centerStageActive"

-- | @Selector@ for @centerStageRectOfInterestSupported@
centerStageRectOfInterestSupportedSelector :: Selector '[] Bool
centerStageRectOfInterestSupportedSelector = mkSelector "centerStageRectOfInterestSupported"

-- | @Selector@ for @geometricDistortionCorrectionSupported@
geometricDistortionCorrectionSupportedSelector :: Selector '[] Bool
geometricDistortionCorrectionSupportedSelector = mkSelector "geometricDistortionCorrectionSupported"

-- | @Selector@ for @geometricDistortionCorrectionEnabled@
geometricDistortionCorrectionEnabledSelector :: Selector '[] Bool
geometricDistortionCorrectionEnabledSelector = mkSelector "geometricDistortionCorrectionEnabled"

-- | @Selector@ for @setGeometricDistortionCorrectionEnabled:@
setGeometricDistortionCorrectionEnabledSelector :: Selector '[Bool] ()
setGeometricDistortionCorrectionEnabledSelector = mkSelector "setGeometricDistortionCorrectionEnabled:"

-- | @Selector@ for @activeDepthDataFormat@
activeDepthDataFormatSelector :: Selector '[] (Id AVCaptureDeviceFormat)
activeDepthDataFormatSelector = mkSelector "activeDepthDataFormat"

-- | @Selector@ for @setActiveDepthDataFormat:@
setActiveDepthDataFormatSelector :: Selector '[Id AVCaptureDeviceFormat] ()
setActiveDepthDataFormatSelector = mkSelector "setActiveDepthDataFormat:"

-- | @Selector@ for @minAvailableVideoZoomFactor@
minAvailableVideoZoomFactorSelector :: Selector '[] CDouble
minAvailableVideoZoomFactorSelector = mkSelector "minAvailableVideoZoomFactor"

-- | @Selector@ for @maxAvailableVideoZoomFactor@
maxAvailableVideoZoomFactorSelector :: Selector '[] CDouble
maxAvailableVideoZoomFactorSelector = mkSelector "maxAvailableVideoZoomFactor"

-- | @Selector@ for @activeColorSpace@
activeColorSpaceSelector :: Selector '[] AVCaptureColorSpace
activeColorSpaceSelector = mkSelector "activeColorSpace"

-- | @Selector@ for @setActiveColorSpace:@
setActiveColorSpaceSelector :: Selector '[AVCaptureColorSpace] ()
setActiveColorSpaceSelector = mkSelector "setActiveColorSpace:"

-- | @Selector@ for @automaticallyAdjustsVideoHDREnabled@
automaticallyAdjustsVideoHDREnabledSelector :: Selector '[] Bool
automaticallyAdjustsVideoHDREnabledSelector = mkSelector "automaticallyAdjustsVideoHDREnabled"

-- | @Selector@ for @setAutomaticallyAdjustsVideoHDREnabled:@
setAutomaticallyAdjustsVideoHDREnabledSelector :: Selector '[Bool] ()
setAutomaticallyAdjustsVideoHDREnabledSelector = mkSelector "setAutomaticallyAdjustsVideoHDREnabled:"

-- | @Selector@ for @videoHDREnabled@
videoHDREnabledSelector :: Selector '[] Bool
videoHDREnabledSelector = mkSelector "videoHDREnabled"

-- | @Selector@ for @setVideoHDREnabled:@
setVideoHDREnabledSelector :: Selector '[Bool] ()
setVideoHDREnabledSelector = mkSelector "setVideoHDREnabled:"

-- | @Selector@ for @transportControlsSupported@
transportControlsSupportedSelector :: Selector '[] Bool
transportControlsSupportedSelector = mkSelector "transportControlsSupported"

-- | @Selector@ for @transportControlsPlaybackMode@
transportControlsPlaybackModeSelector :: Selector '[] AVCaptureDeviceTransportControlsPlaybackMode
transportControlsPlaybackModeSelector = mkSelector "transportControlsPlaybackMode"

-- | @Selector@ for @transportControlsSpeed@
transportControlsSpeedSelector :: Selector '[] CFloat
transportControlsSpeedSelector = mkSelector "transportControlsSpeed"

-- | @Selector@ for @videoZoomFactor@
videoZoomFactorSelector :: Selector '[] CDouble
videoZoomFactorSelector = mkSelector "videoZoomFactor"

-- | @Selector@ for @setVideoZoomFactor:@
setVideoZoomFactorSelector :: Selector '[CDouble] ()
setVideoZoomFactorSelector = mkSelector "setVideoZoomFactor:"

-- | @Selector@ for @rampingVideoZoom@
rampingVideoZoomSelector :: Selector '[] Bool
rampingVideoZoomSelector = mkSelector "rampingVideoZoom"

-- | @Selector@ for @dualCameraSwitchOverVideoZoomFactor@
dualCameraSwitchOverVideoZoomFactorSelector :: Selector '[] CDouble
dualCameraSwitchOverVideoZoomFactorSelector = mkSelector "dualCameraSwitchOverVideoZoomFactor"

-- | @Selector@ for @displayVideoZoomFactorMultiplier@
displayVideoZoomFactorMultiplierSelector :: Selector '[] CDouble
displayVideoZoomFactorMultiplierSelector = mkSelector "displayVideoZoomFactorMultiplier"

-- | @Selector@ for @lowLightBoostSupported@
lowLightBoostSupportedSelector :: Selector '[] Bool
lowLightBoostSupportedSelector = mkSelector "lowLightBoostSupported"

-- | @Selector@ for @lowLightBoostEnabled@
lowLightBoostEnabledSelector :: Selector '[] Bool
lowLightBoostEnabledSelector = mkSelector "lowLightBoostEnabled"

-- | @Selector@ for @automaticallyEnablesLowLightBoostWhenAvailable@
automaticallyEnablesLowLightBoostWhenAvailableSelector :: Selector '[] Bool
automaticallyEnablesLowLightBoostWhenAvailableSelector = mkSelector "automaticallyEnablesLowLightBoostWhenAvailable"

-- | @Selector@ for @setAutomaticallyEnablesLowLightBoostWhenAvailable:@
setAutomaticallyEnablesLowLightBoostWhenAvailableSelector :: Selector '[Bool] ()
setAutomaticallyEnablesLowLightBoostWhenAvailableSelector = mkSelector "setAutomaticallyEnablesLowLightBoostWhenAvailable:"

-- | @Selector@ for @subjectAreaChangeMonitoringEnabled@
subjectAreaChangeMonitoringEnabledSelector :: Selector '[] Bool
subjectAreaChangeMonitoringEnabledSelector = mkSelector "subjectAreaChangeMonitoringEnabled"

-- | @Selector@ for @setSubjectAreaChangeMonitoringEnabled:@
setSubjectAreaChangeMonitoringEnabledSelector :: Selector '[Bool] ()
setSubjectAreaChangeMonitoringEnabledSelector = mkSelector "setSubjectAreaChangeMonitoringEnabled:"

-- | @Selector@ for @lockingWhiteBalanceWithCustomDeviceGainsSupported@
lockingWhiteBalanceWithCustomDeviceGainsSupportedSelector :: Selector '[] Bool
lockingWhiteBalanceWithCustomDeviceGainsSupportedSelector = mkSelector "lockingWhiteBalanceWithCustomDeviceGainsSupported"

-- | @Selector@ for @whiteBalanceMode@
whiteBalanceModeSelector :: Selector '[] AVCaptureWhiteBalanceMode
whiteBalanceModeSelector = mkSelector "whiteBalanceMode"

-- | @Selector@ for @setWhiteBalanceMode:@
setWhiteBalanceModeSelector :: Selector '[AVCaptureWhiteBalanceMode] ()
setWhiteBalanceModeSelector = mkSelector "setWhiteBalanceMode:"

-- | @Selector@ for @adjustingWhiteBalance@
adjustingWhiteBalanceSelector :: Selector '[] Bool
adjustingWhiteBalanceSelector = mkSelector "adjustingWhiteBalance"

-- | @Selector@ for @maxWhiteBalanceGain@
maxWhiteBalanceGainSelector :: Selector '[] CFloat
maxWhiteBalanceGainSelector = mkSelector "maxWhiteBalanceGain"

-- | @Selector@ for @globalToneMappingEnabled@
globalToneMappingEnabledSelector :: Selector '[] Bool
globalToneMappingEnabledSelector = mkSelector "globalToneMappingEnabled"

-- | @Selector@ for @setGlobalToneMappingEnabled:@
setGlobalToneMappingEnabledSelector :: Selector '[Bool] ()
setGlobalToneMappingEnabledSelector = mkSelector "setGlobalToneMappingEnabled:"

-- | @Selector@ for @exposureMode@
exposureModeSelector :: Selector '[] AVCaptureExposureMode
exposureModeSelector = mkSelector "exposureMode"

-- | @Selector@ for @setExposureMode:@
setExposureModeSelector :: Selector '[AVCaptureExposureMode] ()
setExposureModeSelector = mkSelector "setExposureMode:"

-- | @Selector@ for @exposurePointOfInterestSupported@
exposurePointOfInterestSupportedSelector :: Selector '[] Bool
exposurePointOfInterestSupportedSelector = mkSelector "exposurePointOfInterestSupported"

-- | @Selector@ for @exposureRectOfInterestSupported@
exposureRectOfInterestSupportedSelector :: Selector '[] Bool
exposureRectOfInterestSupportedSelector = mkSelector "exposureRectOfInterestSupported"

-- | @Selector@ for @automaticallyAdjustsFaceDrivenAutoExposureEnabled@
automaticallyAdjustsFaceDrivenAutoExposureEnabledSelector :: Selector '[] Bool
automaticallyAdjustsFaceDrivenAutoExposureEnabledSelector = mkSelector "automaticallyAdjustsFaceDrivenAutoExposureEnabled"

-- | @Selector@ for @setAutomaticallyAdjustsFaceDrivenAutoExposureEnabled:@
setAutomaticallyAdjustsFaceDrivenAutoExposureEnabledSelector :: Selector '[Bool] ()
setAutomaticallyAdjustsFaceDrivenAutoExposureEnabledSelector = mkSelector "setAutomaticallyAdjustsFaceDrivenAutoExposureEnabled:"

-- | @Selector@ for @faceDrivenAutoExposureEnabled@
faceDrivenAutoExposureEnabledSelector :: Selector '[] Bool
faceDrivenAutoExposureEnabledSelector = mkSelector "faceDrivenAutoExposureEnabled"

-- | @Selector@ for @setFaceDrivenAutoExposureEnabled:@
setFaceDrivenAutoExposureEnabledSelector :: Selector '[Bool] ()
setFaceDrivenAutoExposureEnabledSelector = mkSelector "setFaceDrivenAutoExposureEnabled:"

-- | @Selector@ for @adjustingExposure@
adjustingExposureSelector :: Selector '[] Bool
adjustingExposureSelector = mkSelector "adjustingExposure"

-- | @Selector@ for @lensAperture@
lensApertureSelector :: Selector '[] CFloat
lensApertureSelector = mkSelector "lensAperture"

-- | @Selector@ for @ISO@
isoSelector :: Selector '[] CFloat
isoSelector = mkSelector "ISO"

-- | @Selector@ for @exposureTargetOffset@
exposureTargetOffsetSelector :: Selector '[] CFloat
exposureTargetOffsetSelector = mkSelector "exposureTargetOffset"

-- | @Selector@ for @exposureTargetBias@
exposureTargetBiasSelector :: Selector '[] CFloat
exposureTargetBiasSelector = mkSelector "exposureTargetBias"

-- | @Selector@ for @minExposureTargetBias@
minExposureTargetBiasSelector :: Selector '[] CFloat
minExposureTargetBiasSelector = mkSelector "minExposureTargetBias"

-- | @Selector@ for @maxExposureTargetBias@
maxExposureTargetBiasSelector :: Selector '[] CFloat
maxExposureTargetBiasSelector = mkSelector "maxExposureTargetBias"

-- | @Selector@ for @lockingFocusWithCustomLensPositionSupported@
lockingFocusWithCustomLensPositionSupportedSelector :: Selector '[] Bool
lockingFocusWithCustomLensPositionSupportedSelector = mkSelector "lockingFocusWithCustomLensPositionSupported"

-- | @Selector@ for @focusMode@
focusModeSelector :: Selector '[] AVCaptureFocusMode
focusModeSelector = mkSelector "focusMode"

-- | @Selector@ for @setFocusMode:@
setFocusModeSelector :: Selector '[AVCaptureFocusMode] ()
setFocusModeSelector = mkSelector "setFocusMode:"

-- | @Selector@ for @focusPointOfInterestSupported@
focusPointOfInterestSupportedSelector :: Selector '[] Bool
focusPointOfInterestSupportedSelector = mkSelector "focusPointOfInterestSupported"

-- | @Selector@ for @focusRectOfInterestSupported@
focusRectOfInterestSupportedSelector :: Selector '[] Bool
focusRectOfInterestSupportedSelector = mkSelector "focusRectOfInterestSupported"

-- | @Selector@ for @adjustingFocus@
adjustingFocusSelector :: Selector '[] Bool
adjustingFocusSelector = mkSelector "adjustingFocus"

-- | @Selector@ for @autoFocusRangeRestrictionSupported@
autoFocusRangeRestrictionSupportedSelector :: Selector '[] Bool
autoFocusRangeRestrictionSupportedSelector = mkSelector "autoFocusRangeRestrictionSupported"

-- | @Selector@ for @autoFocusRangeRestriction@
autoFocusRangeRestrictionSelector :: Selector '[] AVCaptureAutoFocusRangeRestriction
autoFocusRangeRestrictionSelector = mkSelector "autoFocusRangeRestriction"

-- | @Selector@ for @setAutoFocusRangeRestriction:@
setAutoFocusRangeRestrictionSelector :: Selector '[AVCaptureAutoFocusRangeRestriction] ()
setAutoFocusRangeRestrictionSelector = mkSelector "setAutoFocusRangeRestriction:"

-- | @Selector@ for @smoothAutoFocusSupported@
smoothAutoFocusSupportedSelector :: Selector '[] Bool
smoothAutoFocusSupportedSelector = mkSelector "smoothAutoFocusSupported"

-- | @Selector@ for @smoothAutoFocusEnabled@
smoothAutoFocusEnabledSelector :: Selector '[] Bool
smoothAutoFocusEnabledSelector = mkSelector "smoothAutoFocusEnabled"

-- | @Selector@ for @setSmoothAutoFocusEnabled:@
setSmoothAutoFocusEnabledSelector :: Selector '[Bool] ()
setSmoothAutoFocusEnabledSelector = mkSelector "setSmoothAutoFocusEnabled:"

-- | @Selector@ for @automaticallyAdjustsFaceDrivenAutoFocusEnabled@
automaticallyAdjustsFaceDrivenAutoFocusEnabledSelector :: Selector '[] Bool
automaticallyAdjustsFaceDrivenAutoFocusEnabledSelector = mkSelector "automaticallyAdjustsFaceDrivenAutoFocusEnabled"

-- | @Selector@ for @setAutomaticallyAdjustsFaceDrivenAutoFocusEnabled:@
setAutomaticallyAdjustsFaceDrivenAutoFocusEnabledSelector :: Selector '[Bool] ()
setAutomaticallyAdjustsFaceDrivenAutoFocusEnabledSelector = mkSelector "setAutomaticallyAdjustsFaceDrivenAutoFocusEnabled:"

-- | @Selector@ for @faceDrivenAutoFocusEnabled@
faceDrivenAutoFocusEnabledSelector :: Selector '[] Bool
faceDrivenAutoFocusEnabledSelector = mkSelector "faceDrivenAutoFocusEnabled"

-- | @Selector@ for @setFaceDrivenAutoFocusEnabled:@
setFaceDrivenAutoFocusEnabledSelector :: Selector '[Bool] ()
setFaceDrivenAutoFocusEnabledSelector = mkSelector "setFaceDrivenAutoFocusEnabled:"

-- | @Selector@ for @lensPosition@
lensPositionSelector :: Selector '[] CFloat
lensPositionSelector = mkSelector "lensPosition"

-- | @Selector@ for @minimumFocusDistance@
minimumFocusDistanceSelector :: Selector '[] CLong
minimumFocusDistanceSelector = mkSelector "minimumFocusDistance"

-- | @Selector@ for @hasTorch@
hasTorchSelector :: Selector '[] Bool
hasTorchSelector = mkSelector "hasTorch"

-- | @Selector@ for @torchAvailable@
torchAvailableSelector :: Selector '[] Bool
torchAvailableSelector = mkSelector "torchAvailable"

-- | @Selector@ for @torchActive@
torchActiveSelector :: Selector '[] Bool
torchActiveSelector = mkSelector "torchActive"

-- | @Selector@ for @torchLevel@
torchLevelSelector :: Selector '[] CFloat
torchLevelSelector = mkSelector "torchLevel"

-- | @Selector@ for @torchMode@
torchModeSelector :: Selector '[] AVCaptureTorchMode
torchModeSelector = mkSelector "torchMode"

-- | @Selector@ for @setTorchMode:@
setTorchModeSelector :: Selector '[AVCaptureTorchMode] ()
setTorchModeSelector = mkSelector "setTorchMode:"

-- | @Selector@ for @hasFlash@
hasFlashSelector :: Selector '[] Bool
hasFlashSelector = mkSelector "hasFlash"

-- | @Selector@ for @flashAvailable@
flashAvailableSelector :: Selector '[] Bool
flashAvailableSelector = mkSelector "flashAvailable"

-- | @Selector@ for @flashActive@
flashActiveSelector :: Selector '[] Bool
flashActiveSelector = mkSelector "flashActive"

-- | @Selector@ for @flashMode@
flashModeSelector :: Selector '[] AVCaptureFlashMode
flashModeSelector = mkSelector "flashMode"

-- | @Selector@ for @setFlashMode:@
setFlashModeSelector :: Selector '[AVCaptureFlashMode] ()
setFlashModeSelector = mkSelector "setFlashMode:"

-- | @Selector@ for @virtualDevice@
virtualDeviceSelector :: Selector '[] Bool
virtualDeviceSelector = mkSelector "virtualDevice"

-- | @Selector@ for @constituentDevices@
constituentDevicesSelector :: Selector '[] (Id NSArray)
constituentDevicesSelector = mkSelector "constituentDevices"

-- | @Selector@ for @virtualDeviceSwitchOverVideoZoomFactors@
virtualDeviceSwitchOverVideoZoomFactorsSelector :: Selector '[] (Id NSArray)
virtualDeviceSwitchOverVideoZoomFactorsSelector = mkSelector "virtualDeviceSwitchOverVideoZoomFactors"

-- | @Selector@ for @primaryConstituentDeviceSwitchingBehavior@
primaryConstituentDeviceSwitchingBehaviorSelector :: Selector '[] AVCapturePrimaryConstituentDeviceSwitchingBehavior
primaryConstituentDeviceSwitchingBehaviorSelector = mkSelector "primaryConstituentDeviceSwitchingBehavior"

-- | @Selector@ for @primaryConstituentDeviceRestrictedSwitchingBehaviorConditions@
primaryConstituentDeviceRestrictedSwitchingBehaviorConditionsSelector :: Selector '[] AVCapturePrimaryConstituentDeviceRestrictedSwitchingBehaviorConditions
primaryConstituentDeviceRestrictedSwitchingBehaviorConditionsSelector = mkSelector "primaryConstituentDeviceRestrictedSwitchingBehaviorConditions"

-- | @Selector@ for @activePrimaryConstituentDeviceSwitchingBehavior@
activePrimaryConstituentDeviceSwitchingBehaviorSelector :: Selector '[] AVCapturePrimaryConstituentDeviceSwitchingBehavior
activePrimaryConstituentDeviceSwitchingBehaviorSelector = mkSelector "activePrimaryConstituentDeviceSwitchingBehavior"

-- | @Selector@ for @activePrimaryConstituentDeviceRestrictedSwitchingBehaviorConditions@
activePrimaryConstituentDeviceRestrictedSwitchingBehaviorConditionsSelector :: Selector '[] AVCapturePrimaryConstituentDeviceRestrictedSwitchingBehaviorConditions
activePrimaryConstituentDeviceRestrictedSwitchingBehaviorConditionsSelector = mkSelector "activePrimaryConstituentDeviceRestrictedSwitchingBehaviorConditions"

-- | @Selector@ for @activePrimaryConstituentDevice@
activePrimaryConstituentDeviceSelector :: Selector '[] (Id AVCaptureDevice)
activePrimaryConstituentDeviceSelector = mkSelector "activePrimaryConstituentDevice"

-- | @Selector@ for @supportedFallbackPrimaryConstituentDevices@
supportedFallbackPrimaryConstituentDevicesSelector :: Selector '[] (Id NSArray)
supportedFallbackPrimaryConstituentDevicesSelector = mkSelector "supportedFallbackPrimaryConstituentDevices"

-- | @Selector@ for @fallbackPrimaryConstituentDevices@
fallbackPrimaryConstituentDevicesSelector :: Selector '[] (Id NSArray)
fallbackPrimaryConstituentDevicesSelector = mkSelector "fallbackPrimaryConstituentDevices"

-- | @Selector@ for @setFallbackPrimaryConstituentDevices:@
setFallbackPrimaryConstituentDevicesSelector :: Selector '[Id NSArray] ()
setFallbackPrimaryConstituentDevicesSelector = mkSelector "setFallbackPrimaryConstituentDevices:"

-- | @Selector@ for @systemPressureState@
systemPressureStateSelector :: Selector '[] (Id AVCaptureSystemPressureState)
systemPressureStateSelector = mkSelector "systemPressureState"

-- | @Selector@ for @userPreferredCamera@
userPreferredCameraSelector :: Selector '[] (Id AVCaptureDevice)
userPreferredCameraSelector = mkSelector "userPreferredCamera"

-- | @Selector@ for @setUserPreferredCamera:@
setUserPreferredCameraSelector :: Selector '[Id AVCaptureDevice] ()
setUserPreferredCameraSelector = mkSelector "setUserPreferredCamera:"

-- | @Selector@ for @systemPreferredCamera@
systemPreferredCameraSelector :: Selector '[] (Id AVCaptureDevice)
systemPreferredCameraSelector = mkSelector "systemPreferredCamera"

-- | @Selector@ for @deviceType@
deviceTypeSelector :: Selector '[] (Id NSString)
deviceTypeSelector = mkSelector "deviceType"

-- | @Selector@ for @position@
positionSelector :: Selector '[] AVCaptureDevicePosition
positionSelector = mkSelector "position"

