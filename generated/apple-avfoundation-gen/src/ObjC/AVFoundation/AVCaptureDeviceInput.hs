{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | AVCaptureDeviceInput
--
-- AVCaptureDeviceInput is a concrete subclass of AVCaptureInput that provides an interface for capturing media from an AVCaptureDevice.
--
-- Instances of AVCaptureDeviceInput are input sources for AVCaptureSession that provide media data from devices connected to the system, represented by instances of AVCaptureDevice.
--
-- Generated bindings for @AVCaptureDeviceInput@.
module ObjC.AVFoundation.AVCaptureDeviceInput
  ( AVCaptureDeviceInput
  , IsAVCaptureDeviceInput(..)
  , deviceInputWithDevice_error
  , initWithDevice_error
  , portsWithMediaType_sourceDeviceType_sourceDevicePosition
  , unfollowExternalSyncDevice
  , isMultichannelAudioModeSupported
  , device
  , unifiedAutoExposureDefaultsEnabled
  , setUnifiedAutoExposureDefaultsEnabled
  , lockedVideoFrameDurationSupported
  , externalSyncSupported
  , externalSyncDevice
  , multichannelAudioMode
  , setMultichannelAudioMode
  , windNoiseRemovalSupported
  , windNoiseRemovalEnabled
  , setWindNoiseRemovalEnabled
  , cinematicVideoCaptureSupported
  , cinematicVideoCaptureEnabled
  , setCinematicVideoCaptureEnabled
  , simulatedAperture
  , setSimulatedAperture
  , deviceInputWithDevice_errorSelector
  , initWithDevice_errorSelector
  , portsWithMediaType_sourceDeviceType_sourceDevicePositionSelector
  , unfollowExternalSyncDeviceSelector
  , isMultichannelAudioModeSupportedSelector
  , deviceSelector
  , unifiedAutoExposureDefaultsEnabledSelector
  , setUnifiedAutoExposureDefaultsEnabledSelector
  , lockedVideoFrameDurationSupportedSelector
  , externalSyncSupportedSelector
  , externalSyncDeviceSelector
  , multichannelAudioModeSelector
  , setMultichannelAudioModeSelector
  , windNoiseRemovalSupportedSelector
  , windNoiseRemovalEnabledSelector
  , setWindNoiseRemovalEnabledSelector
  , cinematicVideoCaptureSupportedSelector
  , cinematicVideoCaptureEnabledSelector
  , setCinematicVideoCaptureEnabledSelector
  , simulatedApertureSelector
  , setSimulatedApertureSelector

  -- * Enum types
  , AVCaptureDevicePosition(AVCaptureDevicePosition)
  , pattern AVCaptureDevicePositionUnspecified
  , pattern AVCaptureDevicePositionBack
  , pattern AVCaptureDevicePositionFront
  , AVCaptureMultichannelAudioMode(AVCaptureMultichannelAudioMode)
  , pattern AVCaptureMultichannelAudioModeNone
  , pattern AVCaptureMultichannelAudioModeStereo
  , pattern AVCaptureMultichannelAudioModeFirstOrderAmbisonics

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
import ObjC.AVFoundation.Internal.Enums
import ObjC.Foundation.Internal.Classes

-- | deviceInputWithDevice:error:
--
-- Returns an AVCaptureDeviceInput instance that provides media data from the given device.
--
-- @device@ — An AVCaptureDevice instance to be used for capture.
--
-- @outError@ — On return, if the given device cannot be used for capture, points to an NSError describing the problem.
--
-- Returns: An AVCaptureDeviceInput instance that provides data from the given device, or nil, if the device could not be used for capture.
--
-- This method returns an instance of AVCaptureDeviceInput that can be used to capture data from an AVCaptureDevice in an AVCaptureSession. This method attempts to open the device for capture, taking exclusive control of it if necessary. If the device cannot be opened because it is no longer available or because it is in use, for example, this method returns nil, and the optional outError parameter points to an NSError describing the problem.
--
-- ObjC selector: @+ deviceInputWithDevice:error:@
deviceInputWithDevice_error :: (IsAVCaptureDevice device, IsNSError outError) => device -> outError -> IO (Id AVCaptureDeviceInput)
deviceInputWithDevice_error device outError =
  do
    cls' <- getRequiredClass "AVCaptureDeviceInput"
    withObjCPtr device $ \raw_device ->
      withObjCPtr outError $ \raw_outError ->
        sendClassMsg cls' (mkSelector "deviceInputWithDevice:error:") (retPtr retVoid) [argPtr (castPtr raw_device :: Ptr ()), argPtr (castPtr raw_outError :: Ptr ())] >>= retainedObject . castPtr

-- | initWithDevice:error:
--
-- Creates an AVCaptureDeviceInput instance that provides media data from the given device.
--
-- @device@ — An AVCaptureDevice instance to be used for capture.
--
-- @outError@ — On return, if the given device cannot be used for capture, points to an NSError describing the problem.
--
-- Returns: An AVCaptureDeviceInput instance that provides data from the given device, or nil, if the device could not be used for capture.
--
-- This method creates an instance of AVCaptureDeviceInput that can be used to capture data from an AVCaptureDevice in an AVCaptureSession. This method attempts to open the device for capture, taking exclusive control of it if necessary. If the device cannot be opened because it is no longer available or because it is in use, for example, this method returns nil, and the optional outError parameter points to an NSError describing the problem.
--
-- ObjC selector: @- initWithDevice:error:@
initWithDevice_error :: (IsAVCaptureDeviceInput avCaptureDeviceInput, IsAVCaptureDevice device, IsNSError outError) => avCaptureDeviceInput -> device -> outError -> IO (Id AVCaptureDeviceInput)
initWithDevice_error avCaptureDeviceInput  device outError =
  withObjCPtr device $ \raw_device ->
    withObjCPtr outError $ \raw_outError ->
        sendMsg avCaptureDeviceInput (mkSelector "initWithDevice:error:") (retPtr retVoid) [argPtr (castPtr raw_device :: Ptr ()), argPtr (castPtr raw_outError :: Ptr ())] >>= ownedObject . castPtr

-- | portsWithMediaType:sourceDeviceType:sourceDevicePosition:
--
-- An accessor method used to retrieve a virtual device's constituent device ports for use in an AVCaptureMultiCamSession.
--
-- @mediaType@ — The AVMediaType of the port for which you're searching, or nil if all media types should be considered.
--
-- @sourceDeviceType@ — The AVCaptureDeviceType of the port for which you're searching, or nil if source device type is irrelevant.
--
-- @sourceDevicePosition@ — The AVCaptureDevicePosition of the port for which you're searching. AVCaptureDevicePositionUnspecified is germane to audio devices, indicating omnidirectional audio. For other types of capture devices (e.g. cameras), AVCaptureDevicePositionUnspecified means all positions should be considered in the search.
--
-- Returns: An array of AVCaptureInputPorts satisfying the search criteria, or an empty array could be found.
--
-- When using AVCaptureMultiCamSession, multiple devices may be run simultaneously. You may also run simultaneous streams from a virtual device such as the Dual Camera. By inspecting a virtual device's constituentDevices property, you can find its underlying physical devices and, using this method, search for ports originating from one of those constituent devices. Note that the AVCaptureInput.ports array does not include constituent device ports for virtual devices. You must use this accessor method to discover the ports for which you're specifically looking. These constituent device ports may be used to make connections to outputs for use with an AVCaptureMultiCamSession. Using the Dual Camera as an example, the AVCaptureInput.ports property exposes only those ports supported by the virtual device (it switches automatically between wide and telephoto cameras according to the zoom factor). You may use this method to find the video ports for the constituentDevices.
--
-- AVCaptureInputPort *wideVideoPort = [dualCameraInput portsWithMediaType:AVMediaTypeVideo sourceDeviceType:AVCaptureDeviceTypeBuiltInWideAngleCamera sourceDevicePosition:AVCaptureDevicePositionBack].firstObject;         AVCaptureInputPort *teleVideoPort = [dualCameraInput portsWithMediaType:AVMediaTypeVideo sourceDeviceType:AVCaptureDeviceTypeBuiltInTelephotoCamera sourceDevicePosition:AVCaptureDevicePositionBack].firstObject;
--
-- These ports may be used to create connections, say, to two AVCaptureVideoDataOutput instances, allowing for synchronized full frame rate delivery of both wide and telephoto streams.
--
-- As of iOS 13, constituent device ports may not be connected to AVCapturePhotoOutput instances. Clients who wish to capture multiple photos from a virtual device should use AVCapturePhotoOutput's virtualDeviceConstituentPhotoDeliveryEnabled feature.
--
-- When used in conjunction with an audio device, this method allows you to discover microphones in different AVCaptureDevicePositions. When you intend to work with an AVCaptureMultiCamSession, you may use these ports to make connections and simultaneously capture both front facing and back facing audio simultaneously to two different outputs. When used with an AVCaptureMultiCamSession, the audio device port whose sourceDevicePosition is AVCaptureDevicePositionUnspecified produces omnidirectional sound.
--
-- ObjC selector: @- portsWithMediaType:sourceDeviceType:sourceDevicePosition:@
portsWithMediaType_sourceDeviceType_sourceDevicePosition :: (IsAVCaptureDeviceInput avCaptureDeviceInput, IsNSString mediaType, IsNSString sourceDeviceType) => avCaptureDeviceInput -> mediaType -> sourceDeviceType -> AVCaptureDevicePosition -> IO (Id NSArray)
portsWithMediaType_sourceDeviceType_sourceDevicePosition avCaptureDeviceInput  mediaType sourceDeviceType sourceDevicePosition =
  withObjCPtr mediaType $ \raw_mediaType ->
    withObjCPtr sourceDeviceType $ \raw_sourceDeviceType ->
        sendMsg avCaptureDeviceInput (mkSelector "portsWithMediaType:sourceDeviceType:sourceDevicePosition:") (retPtr retVoid) [argPtr (castPtr raw_mediaType :: Ptr ()), argPtr (castPtr raw_sourceDeviceType :: Ptr ()), argCLong (coerce sourceDevicePosition)] >>= retainedObject . castPtr

-- | Discontinues external sync.
--
-- This method stops your input from syncing to the external sync device you specified in ``followExternalSyncDevice:videoFrameDuration:delegate:``.
--
-- ObjC selector: @- unfollowExternalSyncDevice@
unfollowExternalSyncDevice :: IsAVCaptureDeviceInput avCaptureDeviceInput => avCaptureDeviceInput -> IO ()
unfollowExternalSyncDevice avCaptureDeviceInput  =
    sendMsg avCaptureDeviceInput (mkSelector "unfollowExternalSyncDevice") retVoid []

-- | isMultichannelAudioModeSupported:
--
-- Returns whether the receiver supports the given multichannel audio mode.
--
-- @multichannelAudioMode@ — An AVCaptureMultichannelAudioMode to be checked.
--
-- Returns: YES if the receiver supports the given multichannel audio mode, NO otherwise.
--
-- The receiver's multichannelAudioMode property can only be set to a certain mode if this method returns YES for that mode.
--
-- Multichannel audio modes are not supported when used in conjunction with AVCaptureMultiCamSession.
--
-- ObjC selector: @- isMultichannelAudioModeSupported:@
isMultichannelAudioModeSupported :: IsAVCaptureDeviceInput avCaptureDeviceInput => avCaptureDeviceInput -> AVCaptureMultichannelAudioMode -> IO Bool
isMultichannelAudioModeSupported avCaptureDeviceInput  multichannelAudioMode =
    fmap ((/= 0) :: CULong -> Bool) $ sendMsg avCaptureDeviceInput (mkSelector "isMultichannelAudioModeSupported:") retCULong [argCLong (coerce multichannelAudioMode)]

-- | device
--
-- The device from which the receiver provides data.
--
-- The value of this property is the AVCaptureDevice instance that was used to create the receiver.
--
-- ObjC selector: @- device@
device :: IsAVCaptureDeviceInput avCaptureDeviceInput => avCaptureDeviceInput -> IO (Id AVCaptureDevice)
device avCaptureDeviceInput  =
    sendMsg avCaptureDeviceInput (mkSelector "device") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | unifiedAutoExposureDefaultsEnabled
--
-- Specifies whether the source device should use the same default auto exposure behaviors for -[AVCaptureSession setSessionPreset:] and -[AVCaptureDevice setActiveFormat:].
--
-- AVCaptureDevice's activeFormat property may be set two different ways. 1) You set it directly using one of the formats in the device's -formats array, or 2) the AVCaptureSession sets it on your behalf when you set the AVCaptureSession's sessionPreset property. Depending on the device and format, the default auto exposure behavior may be configured differently when you use one method or the other, resulting in non-uniform auto exposure behavior. Auto exposure defaults include min frame rate, max frame rate, and max exposure duration. If you wish to ensure that consistent default behaviors are applied to the device regardless of the API you use to configure the activeFormat, you may set the device input's unifiedAutoExposureDefaultsEnabled property to YES. Default value for this property is NO.
--
-- Note that if you manually set the device's min frame rate, max frame rate, or max exposure duration, your custom values will override the device defaults regardless of whether you've set this property to YES.
--
-- ObjC selector: @- unifiedAutoExposureDefaultsEnabled@
unifiedAutoExposureDefaultsEnabled :: IsAVCaptureDeviceInput avCaptureDeviceInput => avCaptureDeviceInput -> IO Bool
unifiedAutoExposureDefaultsEnabled avCaptureDeviceInput  =
    fmap ((/= 0) :: CULong -> Bool) $ sendMsg avCaptureDeviceInput (mkSelector "unifiedAutoExposureDefaultsEnabled") retCULong []

-- | unifiedAutoExposureDefaultsEnabled
--
-- Specifies whether the source device should use the same default auto exposure behaviors for -[AVCaptureSession setSessionPreset:] and -[AVCaptureDevice setActiveFormat:].
--
-- AVCaptureDevice's activeFormat property may be set two different ways. 1) You set it directly using one of the formats in the device's -formats array, or 2) the AVCaptureSession sets it on your behalf when you set the AVCaptureSession's sessionPreset property. Depending on the device and format, the default auto exposure behavior may be configured differently when you use one method or the other, resulting in non-uniform auto exposure behavior. Auto exposure defaults include min frame rate, max frame rate, and max exposure duration. If you wish to ensure that consistent default behaviors are applied to the device regardless of the API you use to configure the activeFormat, you may set the device input's unifiedAutoExposureDefaultsEnabled property to YES. Default value for this property is NO.
--
-- Note that if you manually set the device's min frame rate, max frame rate, or max exposure duration, your custom values will override the device defaults regardless of whether you've set this property to YES.
--
-- ObjC selector: @- setUnifiedAutoExposureDefaultsEnabled:@
setUnifiedAutoExposureDefaultsEnabled :: IsAVCaptureDeviceInput avCaptureDeviceInput => avCaptureDeviceInput -> Bool -> IO ()
setUnifiedAutoExposureDefaultsEnabled avCaptureDeviceInput  value =
    sendMsg avCaptureDeviceInput (mkSelector "setUnifiedAutoExposureDefaultsEnabled:") retVoid [argCULong (if value then 1 else 0)]

-- | Indicates whether the device input supports locked frame durations.
--
-- See ``AVCaptureDeviceInput/activeLockedVideoFrameDuration`` for more information on video frame duration locking.
--
-- ObjC selector: @- lockedVideoFrameDurationSupported@
lockedVideoFrameDurationSupported :: IsAVCaptureDeviceInput avCaptureDeviceInput => avCaptureDeviceInput -> IO Bool
lockedVideoFrameDurationSupported avCaptureDeviceInput  =
    fmap ((/= 0) :: CULong -> Bool) $ sendMsg avCaptureDeviceInput (mkSelector "lockedVideoFrameDurationSupported") retCULong []

-- | Indicates whether the device input supports being configured to follow an external sync device.
--
-- See ``AVCaptureDeviceInput/followExternalSyncDevice:videoFrameDuration:delegate:`` for more information on external sync.
--
-- ObjC selector: @- externalSyncSupported@
externalSyncSupported :: IsAVCaptureDeviceInput avCaptureDeviceInput => avCaptureDeviceInput -> IO Bool
externalSyncSupported avCaptureDeviceInput  =
    fmap ((/= 0) :: CULong -> Bool) $ sendMsg avCaptureDeviceInput (mkSelector "externalSyncSupported") retCULong []

-- | The external sync device currently being followed by this input.
--
-- This readonly property returns the ``AVExternalSyncDevice`` instance you provided in ``followExternalSyncDevice:videoFrameDuration:delegate:``. This property returns @nil@ when an external sync device is disconnected or fails to calibrate.
--
-- ObjC selector: @- externalSyncDevice@
externalSyncDevice :: IsAVCaptureDeviceInput avCaptureDeviceInput => avCaptureDeviceInput -> IO (Id AVExternalSyncDevice)
externalSyncDevice avCaptureDeviceInput  =
    sendMsg avCaptureDeviceInput (mkSelector "externalSyncDevice") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | multichannelAudioMode
--
-- Indicates the multichannel audio mode to apply when recording audio.
--
-- This property only takes effect when audio is being routed through the built-in microphone, and is ignored if an external microphone is in use.
--
-- The default value is AVCaptureMultichannelAudioModeNone, in which case the default single channel audio recording is used.
--
-- ObjC selector: @- multichannelAudioMode@
multichannelAudioMode :: IsAVCaptureDeviceInput avCaptureDeviceInput => avCaptureDeviceInput -> IO AVCaptureMultichannelAudioMode
multichannelAudioMode avCaptureDeviceInput  =
    fmap (coerce :: CLong -> AVCaptureMultichannelAudioMode) $ sendMsg avCaptureDeviceInput (mkSelector "multichannelAudioMode") retCLong []

-- | multichannelAudioMode
--
-- Indicates the multichannel audio mode to apply when recording audio.
--
-- This property only takes effect when audio is being routed through the built-in microphone, and is ignored if an external microphone is in use.
--
-- The default value is AVCaptureMultichannelAudioModeNone, in which case the default single channel audio recording is used.
--
-- ObjC selector: @- setMultichannelAudioMode:@
setMultichannelAudioMode :: IsAVCaptureDeviceInput avCaptureDeviceInput => avCaptureDeviceInput -> AVCaptureMultichannelAudioMode -> IO ()
setMultichannelAudioMode avCaptureDeviceInput  value =
    sendMsg avCaptureDeviceInput (mkSelector "setMultichannelAudioMode:") retVoid [argCLong (coerce value)]

-- | windNoiseRemovalSupported
--
-- Returns whether or not the device supports wind noise removal during audio capture.
--
-- YES if the device supports wind noise removal, NO otherwise.
--
-- ObjC selector: @- windNoiseRemovalSupported@
windNoiseRemovalSupported :: IsAVCaptureDeviceInput avCaptureDeviceInput => avCaptureDeviceInput -> IO Bool
windNoiseRemovalSupported avCaptureDeviceInput  =
    fmap ((/= 0) :: CULong -> Bool) $ sendMsg avCaptureDeviceInput (mkSelector "windNoiseRemovalSupported") retCULong []

-- | windNoiseRemovalEnabled
--
-- Specifies whether or not wind noise is removed during audio capture.
--
-- Wind noise removal is available when the AVCaptureDeviceInput multichannelAudioMode property is set to any value other than AVCaptureMultichannelAudioModeNone.
--
-- ObjC selector: @- windNoiseRemovalEnabled@
windNoiseRemovalEnabled :: IsAVCaptureDeviceInput avCaptureDeviceInput => avCaptureDeviceInput -> IO Bool
windNoiseRemovalEnabled avCaptureDeviceInput  =
    fmap ((/= 0) :: CULong -> Bool) $ sendMsg avCaptureDeviceInput (mkSelector "windNoiseRemovalEnabled") retCULong []

-- | windNoiseRemovalEnabled
--
-- Specifies whether or not wind noise is removed during audio capture.
--
-- Wind noise removal is available when the AVCaptureDeviceInput multichannelAudioMode property is set to any value other than AVCaptureMultichannelAudioModeNone.
--
-- ObjC selector: @- setWindNoiseRemovalEnabled:@
setWindNoiseRemovalEnabled :: IsAVCaptureDeviceInput avCaptureDeviceInput => avCaptureDeviceInput -> Bool -> IO ()
setWindNoiseRemovalEnabled avCaptureDeviceInput  value =
    sendMsg avCaptureDeviceInput (mkSelector "setWindNoiseRemovalEnabled:") retVoid [argCULong (if value then 1 else 0)]

-- | A BOOL value specifying whether Cinematic Video capture is supported.
--
-- With Cinematic Video capture, you get a simulated depth-of-field effect that keeps your subjects (people, pets, and more) in sharp focus while applying a pleasing blur to the background (or foreground). Depending on the focus mode (see ``AVCaptureCinematicVideoFocusMode`` for detail), the camera either uses machine learning to automatically detect and focus on subjects in the scene, or it fixes focus on a subject until it exits the scene. Cinematic Videos can be played back and edited using the Cinematic framework.
--
-- You can adjust the video's simulated aperture before starting a recording using the ``simulatedAperture`` property. With Cinematic Video specific focus methods on ``AVCaptureDevice``, you can dynamically control focus transitions.
--
-- Movie files captured with Cinematic Video enabled can be played back and edited with the [Cinematic framework] (https://developer.apple.com/documentation/cinematic/playing-and-editing-cinematic-mode-video?language=objc).
--
-- This property returns @true@ if the session's current configuration allows Cinematic Video capture. When switching cameras or formats, this property may change. When this property changes from @true@ to @false@, ``cinematicVideoCaptureEnabled`` also reverts to @false@. If you've previously opted in for Cinematic Video capture and then change configuration, you may need to set ``cinematicVideoCaptureEnabled`` to @true@ again. This property is key-value observable.
--
-- - Note: ``AVCaptureDepthDataOutput`` is not supported when ``cinematicVideoCaptureEnabled`` is set to @true@. Running an ``AVCaptureSession`` with both of these features throws an @NSInvalidArgumentException@.
--
-- ObjC selector: @- cinematicVideoCaptureSupported@
cinematicVideoCaptureSupported :: IsAVCaptureDeviceInput avCaptureDeviceInput => avCaptureDeviceInput -> IO Bool
cinematicVideoCaptureSupported avCaptureDeviceInput  =
    fmap ((/= 0) :: CULong -> Bool) $ sendMsg avCaptureDeviceInput (mkSelector "cinematicVideoCaptureSupported") retCULong []

-- | A BOOL value specifying whether the Cinematic Video effect is being applied to any movie file output, video data output, metadata output, or video preview layer added to the capture session.
--
-- Default is @false@. Set to @true@ to enable support for Cinematic Video capture.
--
-- When you set this property to @true@, your input's associated ``AVCaptureDevice/focusMode`` changes to ``AVCaptureFocusModeContinuousAutoFocus``. While Cinematic Video capture is enabled, you are not permitted to change your device's focus mode, and any attempt to do so results in an @NSInvalidArgumentException@. You may only set this property to @true@ if ``cinematicVideoCaptureSupported`` is @true@.
--
-- - Note: Enabling Cinematic Video capture requires a lengthy reconfiguration of the capture render pipeline, so if you intend to capture Cinematic Video, you should set this property to @true@ before calling ``AVCaptureSession/startRunning`` or within ``AVCaptureSession/beginConfiguration`` and ``AVCaptureSession/commitConfiguration`` while running.
--
-- ObjC selector: @- cinematicVideoCaptureEnabled@
cinematicVideoCaptureEnabled :: IsAVCaptureDeviceInput avCaptureDeviceInput => avCaptureDeviceInput -> IO Bool
cinematicVideoCaptureEnabled avCaptureDeviceInput  =
    fmap ((/= 0) :: CULong -> Bool) $ sendMsg avCaptureDeviceInput (mkSelector "cinematicVideoCaptureEnabled") retCULong []

-- | A BOOL value specifying whether the Cinematic Video effect is being applied to any movie file output, video data output, metadata output, or video preview layer added to the capture session.
--
-- Default is @false@. Set to @true@ to enable support for Cinematic Video capture.
--
-- When you set this property to @true@, your input's associated ``AVCaptureDevice/focusMode`` changes to ``AVCaptureFocusModeContinuousAutoFocus``. While Cinematic Video capture is enabled, you are not permitted to change your device's focus mode, and any attempt to do so results in an @NSInvalidArgumentException@. You may only set this property to @true@ if ``cinematicVideoCaptureSupported`` is @true@.
--
-- - Note: Enabling Cinematic Video capture requires a lengthy reconfiguration of the capture render pipeline, so if you intend to capture Cinematic Video, you should set this property to @true@ before calling ``AVCaptureSession/startRunning`` or within ``AVCaptureSession/beginConfiguration`` and ``AVCaptureSession/commitConfiguration`` while running.
--
-- ObjC selector: @- setCinematicVideoCaptureEnabled:@
setCinematicVideoCaptureEnabled :: IsAVCaptureDeviceInput avCaptureDeviceInput => avCaptureDeviceInput -> Bool -> IO ()
setCinematicVideoCaptureEnabled avCaptureDeviceInput  value =
    sendMsg avCaptureDeviceInput (mkSelector "setCinematicVideoCaptureEnabled:") retVoid [argCULong (if value then 1 else 0)]

-- | Shallow depth of field simulated aperture.
--
-- When capturing a Cinematic Video, use this property to control the amount of blur in the simulated depth of field effect.
--
-- This property only takes effect when ``cinematicVideoCaptureEnabled`` is set to @true@.
--
-- - Important: Setting this property to a value less than the ``AVCaptureDevice/activeFormat/minSimulatedAperture`` or greater than the ``AVCaptureDevice/activeFormat/maxSimulatedAperture`` throws an @NSRangeException@. you may only set this property if ``AVCaptureDevice/activeFormat/minSimulatedAperture`` returns a non-zero value, otherwise an @NSInvalidArgumentException@ is thrown. You must set this property before starting a Cinematic Video capture. If you attempt to set it while a recording is in progress, an @NSInvalidArgumentException@ is thrown.
--
-- This property is initialized to the associated ``AVCaptureDevice/activeFormat/defaultSimulatedAperture``.
--
-- This property is key-value observable.
--
-- ObjC selector: @- simulatedAperture@
simulatedAperture :: IsAVCaptureDeviceInput avCaptureDeviceInput => avCaptureDeviceInput -> IO CFloat
simulatedAperture avCaptureDeviceInput  =
    sendMsg avCaptureDeviceInput (mkSelector "simulatedAperture") retCFloat []

-- | Shallow depth of field simulated aperture.
--
-- When capturing a Cinematic Video, use this property to control the amount of blur in the simulated depth of field effect.
--
-- This property only takes effect when ``cinematicVideoCaptureEnabled`` is set to @true@.
--
-- - Important: Setting this property to a value less than the ``AVCaptureDevice/activeFormat/minSimulatedAperture`` or greater than the ``AVCaptureDevice/activeFormat/maxSimulatedAperture`` throws an @NSRangeException@. you may only set this property if ``AVCaptureDevice/activeFormat/minSimulatedAperture`` returns a non-zero value, otherwise an @NSInvalidArgumentException@ is thrown. You must set this property before starting a Cinematic Video capture. If you attempt to set it while a recording is in progress, an @NSInvalidArgumentException@ is thrown.
--
-- This property is initialized to the associated ``AVCaptureDevice/activeFormat/defaultSimulatedAperture``.
--
-- This property is key-value observable.
--
-- ObjC selector: @- setSimulatedAperture:@
setSimulatedAperture :: IsAVCaptureDeviceInput avCaptureDeviceInput => avCaptureDeviceInput -> CFloat -> IO ()
setSimulatedAperture avCaptureDeviceInput  value =
    sendMsg avCaptureDeviceInput (mkSelector "setSimulatedAperture:") retVoid [argCFloat value]

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @deviceInputWithDevice:error:@
deviceInputWithDevice_errorSelector :: Selector
deviceInputWithDevice_errorSelector = mkSelector "deviceInputWithDevice:error:"

-- | @Selector@ for @initWithDevice:error:@
initWithDevice_errorSelector :: Selector
initWithDevice_errorSelector = mkSelector "initWithDevice:error:"

-- | @Selector@ for @portsWithMediaType:sourceDeviceType:sourceDevicePosition:@
portsWithMediaType_sourceDeviceType_sourceDevicePositionSelector :: Selector
portsWithMediaType_sourceDeviceType_sourceDevicePositionSelector = mkSelector "portsWithMediaType:sourceDeviceType:sourceDevicePosition:"

-- | @Selector@ for @unfollowExternalSyncDevice@
unfollowExternalSyncDeviceSelector :: Selector
unfollowExternalSyncDeviceSelector = mkSelector "unfollowExternalSyncDevice"

-- | @Selector@ for @isMultichannelAudioModeSupported:@
isMultichannelAudioModeSupportedSelector :: Selector
isMultichannelAudioModeSupportedSelector = mkSelector "isMultichannelAudioModeSupported:"

-- | @Selector@ for @device@
deviceSelector :: Selector
deviceSelector = mkSelector "device"

-- | @Selector@ for @unifiedAutoExposureDefaultsEnabled@
unifiedAutoExposureDefaultsEnabledSelector :: Selector
unifiedAutoExposureDefaultsEnabledSelector = mkSelector "unifiedAutoExposureDefaultsEnabled"

-- | @Selector@ for @setUnifiedAutoExposureDefaultsEnabled:@
setUnifiedAutoExposureDefaultsEnabledSelector :: Selector
setUnifiedAutoExposureDefaultsEnabledSelector = mkSelector "setUnifiedAutoExposureDefaultsEnabled:"

-- | @Selector@ for @lockedVideoFrameDurationSupported@
lockedVideoFrameDurationSupportedSelector :: Selector
lockedVideoFrameDurationSupportedSelector = mkSelector "lockedVideoFrameDurationSupported"

-- | @Selector@ for @externalSyncSupported@
externalSyncSupportedSelector :: Selector
externalSyncSupportedSelector = mkSelector "externalSyncSupported"

-- | @Selector@ for @externalSyncDevice@
externalSyncDeviceSelector :: Selector
externalSyncDeviceSelector = mkSelector "externalSyncDevice"

-- | @Selector@ for @multichannelAudioMode@
multichannelAudioModeSelector :: Selector
multichannelAudioModeSelector = mkSelector "multichannelAudioMode"

-- | @Selector@ for @setMultichannelAudioMode:@
setMultichannelAudioModeSelector :: Selector
setMultichannelAudioModeSelector = mkSelector "setMultichannelAudioMode:"

-- | @Selector@ for @windNoiseRemovalSupported@
windNoiseRemovalSupportedSelector :: Selector
windNoiseRemovalSupportedSelector = mkSelector "windNoiseRemovalSupported"

-- | @Selector@ for @windNoiseRemovalEnabled@
windNoiseRemovalEnabledSelector :: Selector
windNoiseRemovalEnabledSelector = mkSelector "windNoiseRemovalEnabled"

-- | @Selector@ for @setWindNoiseRemovalEnabled:@
setWindNoiseRemovalEnabledSelector :: Selector
setWindNoiseRemovalEnabledSelector = mkSelector "setWindNoiseRemovalEnabled:"

-- | @Selector@ for @cinematicVideoCaptureSupported@
cinematicVideoCaptureSupportedSelector :: Selector
cinematicVideoCaptureSupportedSelector = mkSelector "cinematicVideoCaptureSupported"

-- | @Selector@ for @cinematicVideoCaptureEnabled@
cinematicVideoCaptureEnabledSelector :: Selector
cinematicVideoCaptureEnabledSelector = mkSelector "cinematicVideoCaptureEnabled"

-- | @Selector@ for @setCinematicVideoCaptureEnabled:@
setCinematicVideoCaptureEnabledSelector :: Selector
setCinematicVideoCaptureEnabledSelector = mkSelector "setCinematicVideoCaptureEnabled:"

-- | @Selector@ for @simulatedAperture@
simulatedApertureSelector :: Selector
simulatedApertureSelector = mkSelector "simulatedAperture"

-- | @Selector@ for @setSimulatedAperture:@
setSimulatedApertureSelector :: Selector
setSimulatedApertureSelector = mkSelector "setSimulatedAperture:"

