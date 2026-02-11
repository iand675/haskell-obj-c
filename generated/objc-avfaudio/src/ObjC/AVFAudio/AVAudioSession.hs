{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Generated bindings for @AVAudioSession@.
module ObjC.AVFAudio.AVAudioSession
  ( AVAudioSession
  , IsAVAudioSession(..)
  , sharedInstance
  , setCategory_error
  , setCategory_withOptions_error
  , setCategory_mode_options_error
  , setCategory_mode_routeSharingPolicy_options_error
  , setMode_error
  , setAllowHapticsAndSystemSoundsDuringRecording_error
  , requestRecordPermission
  , overrideOutputAudioPort_error
  , setPreferredInput_error
  , setPrefersNoInterruptionsFromSystemAlerts_error
  , setPrefersEchoCancelledInput_error
  , setOutputMuted_error
  , init_
  , setActive_withFlags_error
  , setPreferredHardwareSampleRate_error
  , setPreferredMicrophoneInjectionMode_error
  , setAggregatedIOPreference_error
  , setSupportsMultichannelContent_error
  , setPrefersInterruptionOnRouteDisconnect_error
  , setPreferredSampleRate_error
  , setPreferredIOBufferDuration_error
  , setPreferredInputNumberOfChannels_error
  , setPreferredOutputNumberOfChannels_error
  , setPreferredInputOrientation_error
  , setInputGain_error
  , setInputDataSource_error
  , setOutputDataSource_error
  , setActive_error
  , setActive_withOptions_error
  , activateWithOptions_completionHandler
  , category
  , categoryOptions
  , routeSharingPolicy
  , mode
  , allowHapticsAndSystemSoundsDuringRecording
  , recordPermission
  , preferredInput
  , prefersNoInterruptionsFromSystemAlerts
  , renderingMode
  , prefersEchoCancelledInput
  , isEchoCancelledInputEnabled
  , isEchoCancelledInputAvailable
  , outputMuted
  , inputIsAvailable
  , currentHardwareSampleRate
  , currentHardwareInputNumberOfChannels
  , currentHardwareOutputNumberOfChannels
  , preferredHardwareSampleRate
  , preferredMicrophoneInjectionMode
  , isMicrophoneInjectionAvailable
  , supportsMultichannelContent
  , prefersInterruptionOnRouteDisconnect
  , otherAudioPlaying
  , secondaryAudioShouldBeSilencedHint
  , outputVolume
  , promptStyle
  , preferredSampleRate
  , preferredIOBufferDuration
  , preferredInputNumberOfChannels
  , preferredOutputNumberOfChannels
  , preferredInputOrientation
  , inputOrientation
  , maximumInputNumberOfChannels
  , maximumOutputNumberOfChannels
  , inputGain
  , inputGainSettable
  , inputAvailable
  , sampleRate
  , inputNumberOfChannels
  , outputNumberOfChannels
  , inputLatency
  , outputLatency
  , ioBufferDuration
  , supportedOutputChannelLayouts
  , sharedInstanceSelector
  , setCategory_errorSelector
  , setCategory_withOptions_errorSelector
  , setCategory_mode_options_errorSelector
  , setCategory_mode_routeSharingPolicy_options_errorSelector
  , setMode_errorSelector
  , setAllowHapticsAndSystemSoundsDuringRecording_errorSelector
  , requestRecordPermissionSelector
  , overrideOutputAudioPort_errorSelector
  , setPreferredInput_errorSelector
  , setPrefersNoInterruptionsFromSystemAlerts_errorSelector
  , setPrefersEchoCancelledInput_errorSelector
  , setOutputMuted_errorSelector
  , initSelector
  , setActive_withFlags_errorSelector
  , setPreferredHardwareSampleRate_errorSelector
  , setPreferredMicrophoneInjectionMode_errorSelector
  , setAggregatedIOPreference_errorSelector
  , setSupportsMultichannelContent_errorSelector
  , setPrefersInterruptionOnRouteDisconnect_errorSelector
  , setPreferredSampleRate_errorSelector
  , setPreferredIOBufferDuration_errorSelector
  , setPreferredInputNumberOfChannels_errorSelector
  , setPreferredOutputNumberOfChannels_errorSelector
  , setPreferredInputOrientation_errorSelector
  , setInputGain_errorSelector
  , setInputDataSource_errorSelector
  , setOutputDataSource_errorSelector
  , setActive_errorSelector
  , setActive_withOptions_errorSelector
  , activateWithOptions_completionHandlerSelector
  , categorySelector
  , categoryOptionsSelector
  , routeSharingPolicySelector
  , modeSelector
  , allowHapticsAndSystemSoundsDuringRecordingSelector
  , recordPermissionSelector
  , preferredInputSelector
  , prefersNoInterruptionsFromSystemAlertsSelector
  , renderingModeSelector
  , prefersEchoCancelledInputSelector
  , isEchoCancelledInputEnabledSelector
  , isEchoCancelledInputAvailableSelector
  , outputMutedSelector
  , inputIsAvailableSelector
  , currentHardwareSampleRateSelector
  , currentHardwareInputNumberOfChannelsSelector
  , currentHardwareOutputNumberOfChannelsSelector
  , preferredHardwareSampleRateSelector
  , preferredMicrophoneInjectionModeSelector
  , isMicrophoneInjectionAvailableSelector
  , supportsMultichannelContentSelector
  , prefersInterruptionOnRouteDisconnectSelector
  , otherAudioPlayingSelector
  , secondaryAudioShouldBeSilencedHintSelector
  , outputVolumeSelector
  , promptStyleSelector
  , preferredSampleRateSelector
  , preferredIOBufferDurationSelector
  , preferredInputNumberOfChannelsSelector
  , preferredOutputNumberOfChannelsSelector
  , preferredInputOrientationSelector
  , inputOrientationSelector
  , maximumInputNumberOfChannelsSelector
  , maximumOutputNumberOfChannelsSelector
  , inputGainSelector
  , inputGainSettableSelector
  , inputAvailableSelector
  , sampleRateSelector
  , inputNumberOfChannelsSelector
  , outputNumberOfChannelsSelector
  , inputLatencySelector
  , outputLatencySelector
  , ioBufferDurationSelector
  , supportedOutputChannelLayoutsSelector

  -- * Enum types
  , AVAudioSessionActivationOptions(AVAudioSessionActivationOptions)
  , pattern AVAudioSessionActivationOptionNone
  , AVAudioSessionCategoryOptions(AVAudioSessionCategoryOptions)
  , pattern AVAudioSessionCategoryOptionMixWithOthers
  , pattern AVAudioSessionCategoryOptionDuckOthers
  , pattern AVAudioSessionCategoryOptionAllowBluetooth
  , pattern AVAudioSessionCategoryOptionAllowBluetoothHFP
  , pattern AVAudioSessionCategoryOptionDefaultToSpeaker
  , pattern AVAudioSessionCategoryOptionInterruptSpokenAudioAndMixWithOthers
  , pattern AVAudioSessionCategoryOptionAllowBluetoothA2DP
  , pattern AVAudioSessionCategoryOptionAllowAirPlay
  , pattern AVAudioSessionCategoryOptionOverrideMutedMicrophoneInterruption
  , pattern AVAudioSessionCategoryOptionFarFieldInput
  , pattern AVAudioSessionCategoryOptionBluetoothHighQualityRecording
  , AVAudioSessionIOType(AVAudioSessionIOType)
  , pattern AVAudioSessionIOTypeNotSpecified
  , pattern AVAudioSessionIOTypeAggregated
  , AVAudioSessionMicrophoneInjectionMode(AVAudioSessionMicrophoneInjectionMode)
  , pattern AVAudioSessionMicrophoneInjectionModeNone
  , pattern AVAudioSessionMicrophoneInjectionModeSpokenAudio
  , AVAudioSessionPortOverride(AVAudioSessionPortOverride)
  , pattern AVAudioSessionPortOverrideNone
  , pattern AVAudioSessionPortOverrideSpeaker
  , AVAudioSessionPromptStyle(AVAudioSessionPromptStyle)
  , pattern AVAudioSessionPromptStyleNone
  , pattern AVAudioSessionPromptStyleShort
  , pattern AVAudioSessionPromptStyleNormal
  , AVAudioSessionRecordPermission(AVAudioSessionRecordPermission)
  , pattern AVAudioSessionRecordPermissionUndetermined
  , pattern AVAudioSessionRecordPermissionDenied
  , pattern AVAudioSessionRecordPermissionGranted
  , AVAudioSessionRenderingMode(AVAudioSessionRenderingMode)
  , pattern AVAudioSessionRenderingModeNotApplicable
  , pattern AVAudioSessionRenderingModeMonoStereo
  , pattern AVAudioSessionRenderingModeSurround
  , pattern AVAudioSessionRenderingModeSpatialAudio
  , pattern AVAudioSessionRenderingModeDolbyAudio
  , pattern AVAudioSessionRenderingModeDolbyAtmos
  , AVAudioSessionRouteSharingPolicy(AVAudioSessionRouteSharingPolicy)
  , pattern AVAudioSessionRouteSharingPolicyDefault
  , pattern AVAudioSessionRouteSharingPolicyLongFormAudio
  , pattern AVAudioSessionRouteSharingPolicyLongForm
  , pattern AVAudioSessionRouteSharingPolicyIndependent
  , pattern AVAudioSessionRouteSharingPolicyLongFormVideo
  , AVAudioSessionSetActiveOptions(AVAudioSessionSetActiveOptions)
  , pattern AVAudioSessionSetActiveOptionNotifyOthersOnDeactivation
  , AVAudioStereoOrientation(AVAudioStereoOrientation)
  , pattern AVAudioStereoOrientationNone
  , pattern AVAudioStereoOrientationPortrait
  , pattern AVAudioStereoOrientationPortraitUpsideDown
  , pattern AVAudioStereoOrientationLandscapeRight
  , pattern AVAudioStereoOrientationLandscapeLeft

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

import ObjC.AVFAudio.Internal.Classes
import ObjC.AVFAudio.Internal.Enums
import ObjC.Foundation.Internal.Classes

-- | Return singleton instance.
--
-- ObjC selector: @+ sharedInstance@
sharedInstance :: IO (Id AVAudioSession)
sharedInstance  =
  do
    cls' <- getRequiredClass "AVAudioSession"
    sendClassMsg cls' (mkSelector "sharedInstance") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | Set session category.
--
-- ObjC selector: @- setCategory:error:@
setCategory_error :: (IsAVAudioSession avAudioSession, IsNSString category, IsNSError outError) => avAudioSession -> category -> outError -> IO Bool
setCategory_error avAudioSession  category outError =
withObjCPtr category $ \raw_category ->
  withObjCPtr outError $ \raw_outError ->
      fmap ((/= 0) :: CULong -> Bool) $ sendMsg avAudioSession (mkSelector "setCategory:error:") retCULong [argPtr (castPtr raw_category :: Ptr ()), argPtr (castPtr raw_outError :: Ptr ())]

-- | Set session category with options.
--
-- ObjC selector: @- setCategory:withOptions:error:@
setCategory_withOptions_error :: (IsAVAudioSession avAudioSession, IsNSString category, IsNSError outError) => avAudioSession -> category -> AVAudioSessionCategoryOptions -> outError -> IO Bool
setCategory_withOptions_error avAudioSession  category options outError =
withObjCPtr category $ \raw_category ->
  withObjCPtr outError $ \raw_outError ->
      fmap ((/= 0) :: CULong -> Bool) $ sendMsg avAudioSession (mkSelector "setCategory:withOptions:error:") retCULong [argPtr (castPtr raw_category :: Ptr ()), argCULong (coerce options), argPtr (castPtr raw_outError :: Ptr ())]

-- | Set session category and mode with options.
--
-- ObjC selector: @- setCategory:mode:options:error:@
setCategory_mode_options_error :: (IsAVAudioSession avAudioSession, IsNSString category, IsNSString mode, IsNSError outError) => avAudioSession -> category -> mode -> AVAudioSessionCategoryOptions -> outError -> IO Bool
setCategory_mode_options_error avAudioSession  category mode options outError =
withObjCPtr category $ \raw_category ->
  withObjCPtr mode $ \raw_mode ->
    withObjCPtr outError $ \raw_outError ->
        fmap ((/= 0) :: CULong -> Bool) $ sendMsg avAudioSession (mkSelector "setCategory:mode:options:error:") retCULong [argPtr (castPtr raw_category :: Ptr ()), argPtr (castPtr raw_mode :: Ptr ()), argCULong (coerce options), argPtr (castPtr raw_outError :: Ptr ())]

-- | Set session category, mode, routing sharing policy, and options.
--
-- Use of the long-form route sharing policy is only valid in conjunction with a limited set of	category, mode, and option values.
--
-- Allowed categories: AVAudioSessionCategoryPlayback.
--
-- Allowed modes: AVAudioSessionModeDefault, AVAudioSessionModeMoviePlayback,	AVAudioSessionModeSpokenAudio.
--
-- Allowed options: None. Options are allowed when changing the routing policy back to Default,	however.
--
-- ObjC selector: @- setCategory:mode:routeSharingPolicy:options:error:@
setCategory_mode_routeSharingPolicy_options_error :: (IsAVAudioSession avAudioSession, IsNSString category, IsNSString mode, IsNSError outError) => avAudioSession -> category -> mode -> AVAudioSessionRouteSharingPolicy -> AVAudioSessionCategoryOptions -> outError -> IO Bool
setCategory_mode_routeSharingPolicy_options_error avAudioSession  category mode policy options outError =
withObjCPtr category $ \raw_category ->
  withObjCPtr mode $ \raw_mode ->
    withObjCPtr outError $ \raw_outError ->
        fmap ((/= 0) :: CULong -> Bool) $ sendMsg avAudioSession (mkSelector "setCategory:mode:routeSharingPolicy:options:error:") retCULong [argPtr (castPtr raw_category :: Ptr ()), argPtr (castPtr raw_mode :: Ptr ()), argCULong (coerce policy), argCULong (coerce options), argPtr (castPtr raw_outError :: Ptr ())]

-- | Set the session's mode.
--
-- Modes modify the audio category in order to introduce behavior that is tailored to the specific	use of audio within an application. Examples:  AVAudioSessionModeVideoRecording,	AVAudioSessionModeVoiceChat, AVAudioSessionModeMeasurement, etc.
--
-- ObjC selector: @- setMode:error:@
setMode_error :: (IsAVAudioSession avAudioSession, IsNSString mode, IsNSError outError) => avAudioSession -> mode -> outError -> IO Bool
setMode_error avAudioSession  mode outError =
withObjCPtr mode $ \raw_mode ->
  withObjCPtr outError $ \raw_outError ->
      fmap ((/= 0) :: CULong -> Bool) $ sendMsg avAudioSession (mkSelector "setMode:error:") retCULong [argPtr (castPtr raw_mode :: Ptr ()), argPtr (castPtr raw_outError :: Ptr ())]

-- | Set allowHapticsAndSystemSoundsDuringRecording to YES in order to allow system sounds and haptics to play while the session is actively using audio input. Default value is NO.
--
-- ObjC selector: @- setAllowHapticsAndSystemSoundsDuringRecording:error:@
setAllowHapticsAndSystemSoundsDuringRecording_error :: (IsAVAudioSession avAudioSession, IsNSError outError) => avAudioSession -> Bool -> outError -> IO Bool
setAllowHapticsAndSystemSoundsDuringRecording_error avAudioSession  inValue outError =
withObjCPtr outError $ \raw_outError ->
    fmap ((/= 0) :: CULong -> Bool) $ sendMsg avAudioSession (mkSelector "setAllowHapticsAndSystemSoundsDuringRecording:error:") retCULong [argCULong (if inValue then 1 else 0), argPtr (castPtr raw_outError :: Ptr ())]

-- | Checks to see if calling process has permission to record audio.
--
-- The 'response' block will be called immediately if permission has already been granted or	denied.  Otherwise, it presents a dialog to notify the user and allow them to choose, and calls	the block once the UI has been dismissed.  'granted' indicates whether permission has been	granted. Note that the block may be called in a different thread context.
--
-- ObjC selector: @- requestRecordPermission:@
requestRecordPermission :: IsAVAudioSession avAudioSession => avAudioSession -> Ptr () -> IO ()
requestRecordPermission avAudioSession  response =
  sendMsg avAudioSession (mkSelector "requestRecordPermission:") retVoid [argPtr (castPtr response :: Ptr ())]

-- | Use this method to temporarily override the output to built-in speaker.
--
-- This method is only valid for a session using PlayAndRecord category. This change remains in    effect only until the current route changes or you call this method again with the    AVAudioSessionPortOverrideNone option. Sessions using PlayAndRecord category that always want to    prefer the built-in speaker output over the receiver, should use    AVAudioSessionCategoryOptionDefaultToSpeaker instead.
--
-- ObjC selector: @- overrideOutputAudioPort:error:@
overrideOutputAudioPort_error :: (IsAVAudioSession avAudioSession, IsNSError outError) => avAudioSession -> AVAudioSessionPortOverride -> outError -> IO Bool
overrideOutputAudioPort_error avAudioSession  portOverride outError =
withObjCPtr outError $ \raw_outError ->
    fmap ((/= 0) :: CULong -> Bool) $ sendMsg avAudioSession (mkSelector "overrideOutputAudioPort:error:") retCULong [argCULong (coerce portOverride), argPtr (castPtr raw_outError :: Ptr ())]

-- | Select a preferred input port for audio routing.
--
-- If the input port is already part of the current audio route, this will have no effect.    Otherwise, selecting an input port for routing will initiate a route change to use the preferred    input port. Setting a nil value will clear the preference.
--
-- ObjC selector: @- setPreferredInput:error:@
setPreferredInput_error :: (IsAVAudioSession avAudioSession, IsAVAudioSessionPortDescription inPort, IsNSError outError) => avAudioSession -> inPort -> outError -> IO Bool
setPreferredInput_error avAudioSession  inPort outError =
withObjCPtr inPort $ \raw_inPort ->
  withObjCPtr outError $ \raw_outError ->
      fmap ((/= 0) :: CULong -> Bool) $ sendMsg avAudioSession (mkSelector "setPreferredInput:error:") retCULong [argPtr (castPtr raw_inPort :: Ptr ()), argPtr (castPtr raw_outError :: Ptr ())]

-- | Set ringtone and alert interruption preference.
--
-- Inform the system when the session prefers to not be interrupted by    ringtones and alerts. By setting this property to YES, clients will not be interrupted    by incoming call notifications and other alerts. Starting in iOS 14.0, users can set a global    preference for incoming call display style to "Banner" or "Full Screen". With "Banner" display style,    if below property is set to YES then system audio will be silenced. Thus, clients will not be interrupted    on incoming call notification and user will have opportunity to accept or decline the call. If call is declined,    the session will not be interrupted, but if user accepts the incoming call, the session will be interrupted.    With  display style set as "Full Screen", below property will have no effect and clients will be    interrupted by incoming calls. Apps that record audio and/or video and apps that are used for    music performance are candidates for using this feature.
--
-- ObjC selector: @- setPrefersNoInterruptionsFromSystemAlerts:error:@
setPrefersNoInterruptionsFromSystemAlerts_error :: (IsAVAudioSession avAudioSession, IsNSError outError) => avAudioSession -> Bool -> outError -> IO Bool
setPrefersNoInterruptionsFromSystemAlerts_error avAudioSession  inValue outError =
withObjCPtr outError $ \raw_outError ->
    fmap ((/= 0) :: CULong -> Bool) $ sendMsg avAudioSession (mkSelector "setPrefersNoInterruptionsFromSystemAlerts:error:") retCULong [argCULong (if inValue then 1 else 0), argPtr (castPtr raw_outError :: Ptr ())]

-- | Set a preference to enable echo cancelled input on supported hardware
--
-- Applications might want to record the built-in microphone's input while also playing audio out via the built-in speaker.Enabling echo cancelled input is useful when the application needs the input signal to be clear of any echoesfrom the audio playing out of the built-in speaker.
--
-- Audio sessions using Voice Processor don't need this option as echo cancellation is implicitly applied for those routes.The Voice Processor solution is tuned for voice signals, unlike this option, which is tuned for better captureof wider range of audio signals in the presence of built-in speaker echo.
--
-- This option is only available on certain 2024 or later iPhone models and is valid only when used with the following configurations: - AVAudioSessionCategoryPlayAndRecord and AVAudioSessionModeDefault - AVAudioSessionCategoryMultiRoute and AVAudioSessionModeDualRouteSupport can be queried using property @isEchoCancelledInputAvailable@.Other recording sessions might be interrupted if this option is not compatible with sessions that are already recording.
--
-- After an audio session goes active, @isEchoCancelledInputEnabled@ property can be queried to check if the option was honored.Note that the enabled state may change after route changes, e.g. if user plugs in a headset, that route might not support echo cancellation.
--
-- ObjC selector: @- setPrefersEchoCancelledInput:error:@
setPrefersEchoCancelledInput_error :: (IsAVAudioSession avAudioSession, IsNSError error_) => avAudioSession -> Bool -> error_ -> IO Bool
setPrefersEchoCancelledInput_error avAudioSession  value error_ =
withObjCPtr error_ $ \raw_error_ ->
    fmap ((/= 0) :: CULong -> Bool) $ sendMsg avAudioSession (mkSelector "setPrefersEchoCancelledInput:error:") retCULong [argCULong (if value then 1 else 0), argPtr (castPtr raw_error_ :: Ptr ())]

-- | Sets a Boolean value to inform the system to mute the session's output audio. The default value is false (unmuted).
--
-- This property is supported with all categories and modes, except for ``AVAudioSessionCategoryPlayAndRecord`` where it is only supported with ``AVAudioSessionModeDefault``. Changing the mode to non-default mode with ``AVAudioSessionCategoryPlayAndRecord`` category will cause the session to unmute.
--
-- Changes in output mute state can be observed via ``AVAudioSessionOutputMuteStateChangeNotification``. If this value is set to true, ``AVAudioSessionUserIntentToUnmuteOutputNotification`` may be sent when a user hints to unmute by changing the volume.
--
-- - Note: This will not mute system sounds and haptics.
--
-- - Parameters: - @muted@: A Boolean value to set the audio output to the desired muted state. - @error@: A pointer to an error object. If an error occurs, the framework sets the pointer to an error object that describes the failure.
--
-- ObjC selector: @- setOutputMuted:error:@
setOutputMuted_error :: (IsAVAudioSession avAudioSession, IsNSError outError) => avAudioSession -> Bool -> outError -> IO Bool
setOutputMuted_error avAudioSession  muted outError =
withObjCPtr outError $ \raw_outError ->
    fmap ((/= 0) :: CULong -> Bool) $ sendMsg avAudioSession (mkSelector "setOutputMuted:error:") retCULong [argCULong (if muted then 1 else 0), argPtr (castPtr raw_outError :: Ptr ())]

-- | @- init@
init_ :: IsAVAudioSession avAudioSession => avAudioSession -> IO (Id AVAudioSession)
init_ avAudioSession  =
  sendMsg avAudioSession (mkSelector "init") (retPtr retVoid) [] >>= ownedObject . castPtr

-- | @- setActive:withFlags:error:@
setActive_withFlags_error :: (IsAVAudioSession avAudioSession, IsNSError outError) => avAudioSession -> Bool -> CLong -> outError -> IO Bool
setActive_withFlags_error avAudioSession  active flags outError =
withObjCPtr outError $ \raw_outError ->
    fmap ((/= 0) :: CULong -> Bool) $ sendMsg avAudioSession (mkSelector "setActive:withFlags:error:") retCULong [argCULong (if active then 1 else 0), argCLong (fromIntegral flags), argPtr (castPtr raw_outError :: Ptr ())]

-- | @- setPreferredHardwareSampleRate:error:@
setPreferredHardwareSampleRate_error :: (IsAVAudioSession avAudioSession, IsNSError outError) => avAudioSession -> CDouble -> outError -> IO Bool
setPreferredHardwareSampleRate_error avAudioSession  sampleRate outError =
withObjCPtr outError $ \raw_outError ->
    fmap ((/= 0) :: CULong -> Bool) $ sendMsg avAudioSession (mkSelector "setPreferredHardwareSampleRate:error:") retCULong [argCDouble (fromIntegral sampleRate), argPtr (castPtr raw_outError :: Ptr ())]

-- | Set the preferred form of audio injection into another app's input stream See AVAudioSessionMicrophoneInjectionMode for available modes
--
-- ObjC selector: @- setPreferredMicrophoneInjectionMode:error:@
setPreferredMicrophoneInjectionMode_error :: (IsAVAudioSession avAudioSession, IsNSError outError) => avAudioSession -> AVAudioSessionMicrophoneInjectionMode -> outError -> IO Bool
setPreferredMicrophoneInjectionMode_error avAudioSession  inValue outError =
withObjCPtr outError $ \raw_outError ->
    fmap ((/= 0) :: CULong -> Bool) $ sendMsg avAudioSession (mkSelector "setPreferredMicrophoneInjectionMode:error:") retCULong [argCLong (coerce inValue), argPtr (castPtr raw_outError :: Ptr ())]

-- | Controls whether audio input and output are aggregated. Only valid in combination with    AVAudioSessionCategoryPlayAndRecord or AVAudioSessionCategoryMultiRoute.
--
-- See the AVAudioSessionIOType documentation for a more detailed explanation of why a client may    want to change the IO type.
--
-- ObjC selector: @- setAggregatedIOPreference:error:@
setAggregatedIOPreference_error :: (IsAVAudioSession avAudioSession, IsNSError outError) => avAudioSession -> AVAudioSessionIOType -> outError -> IO Bool
setAggregatedIOPreference_error avAudioSession  inIOType outError =
withObjCPtr outError $ \raw_outError ->
    fmap ((/= 0) :: CULong -> Bool) $ sendMsg avAudioSession (mkSelector "setAggregatedIOPreference:error:") retCULong [argCULong (coerce inIOType), argPtr (castPtr raw_outError :: Ptr ())]

-- | Set YES to inform the system if the app can supply multichannel audio content. Default value is NO. This property is intended to be used by 'Now Playing' applications. See https://developer.apple.com/documentation/mediaplayer/becoming_a_now_playable_app for more information about what it means to be a 'Now Playing' application. Typically 'Now Playing' applications will also use AVAudioSessionRouteSharingPolicyLongFormAudio or AVAudioSessionRouteSharingPolicyLongFormVideo.
--
-- ObjC selector: @- setSupportsMultichannelContent:error:@
setSupportsMultichannelContent_error :: (IsAVAudioSession avAudioSession, IsNSError outError) => avAudioSession -> Bool -> outError -> IO Bool
setSupportsMultichannelContent_error avAudioSession  inValue outError =
withObjCPtr outError $ \raw_outError ->
    fmap ((/= 0) :: CULong -> Bool) $ sendMsg avAudioSession (mkSelector "setSupportsMultichannelContent:error:") retCULong [argCULong (if inValue then 1 else 0), argPtr (castPtr raw_outError :: Ptr ())]

-- | Use this method to opt in or opt out of interruption on route disconnect policy.
--
-- As described in the Audio Session Programming Guide, most media playback apps are expected    to pause playback if the route change reason is AVAudioSessionRouteChangeReasonOldDeviceUnavailable.
--
-- Starting in iOS 17, by default Now Playing sessions will be interrupted if they are active    when a route change occurs because of a disconnect event. All other sessions will not be    interrupted due to a disconnect event.
--
-- ObjC selector: @- setPrefersInterruptionOnRouteDisconnect:error:@
setPrefersInterruptionOnRouteDisconnect_error :: (IsAVAudioSession avAudioSession, IsNSError outError) => avAudioSession -> Bool -> outError -> IO Bool
setPrefersInterruptionOnRouteDisconnect_error avAudioSession  inValue outError =
withObjCPtr outError $ \raw_outError ->
    fmap ((/= 0) :: CULong -> Bool) $ sendMsg avAudioSession (mkSelector "setPrefersInterruptionOnRouteDisconnect:error:") retCULong [argCULong (if inValue then 1 else 0), argPtr (castPtr raw_outError :: Ptr ())]

-- | The preferred hardware sample rate for the session. The actual sample rate may be different.
--
-- ObjC selector: @- setPreferredSampleRate:error:@
setPreferredSampleRate_error :: (IsAVAudioSession avAudioSession, IsNSError outError) => avAudioSession -> CDouble -> outError -> IO Bool
setPreferredSampleRate_error avAudioSession  sampleRate outError =
withObjCPtr outError $ \raw_outError ->
    fmap ((/= 0) :: CULong -> Bool) $ sendMsg avAudioSession (mkSelector "setPreferredSampleRate:error:") retCULong [argCDouble (fromIntegral sampleRate), argPtr (castPtr raw_outError :: Ptr ())]

-- | The preferred hardware IO buffer duration in seconds. The actual IO buffer duration may be different.
--
-- ObjC selector: @- setPreferredIOBufferDuration:error:@
setPreferredIOBufferDuration_error :: (IsAVAudioSession avAudioSession, IsNSError outError) => avAudioSession -> CDouble -> outError -> IO Bool
setPreferredIOBufferDuration_error avAudioSession  duration outError =
withObjCPtr outError $ \raw_outError ->
    fmap ((/= 0) :: CULong -> Bool) $ sendMsg avAudioSession (mkSelector "setPreferredIOBufferDuration:error:") retCULong [argCDouble (fromIntegral duration), argPtr (castPtr raw_outError :: Ptr ())]

-- | Sets the number of input channels that the app would prefer for the current route
--
-- ObjC selector: @- setPreferredInputNumberOfChannels:error:@
setPreferredInputNumberOfChannels_error :: (IsAVAudioSession avAudioSession, IsNSError outError) => avAudioSession -> CLong -> outError -> IO Bool
setPreferredInputNumberOfChannels_error avAudioSession  count outError =
withObjCPtr outError $ \raw_outError ->
    fmap ((/= 0) :: CULong -> Bool) $ sendMsg avAudioSession (mkSelector "setPreferredInputNumberOfChannels:error:") retCULong [argCLong (fromIntegral count), argPtr (castPtr raw_outError :: Ptr ())]

-- | Sets the number of output channels that the app would prefer for the current route
--
-- ObjC selector: @- setPreferredOutputNumberOfChannels:error:@
setPreferredOutputNumberOfChannels_error :: (IsAVAudioSession avAudioSession, IsNSError outError) => avAudioSession -> CLong -> outError -> IO Bool
setPreferredOutputNumberOfChannels_error avAudioSession  count outError =
withObjCPtr outError $ \raw_outError ->
    fmap ((/= 0) :: CULong -> Bool) $ sendMsg avAudioSession (mkSelector "setPreferredOutputNumberOfChannels:error:") retCULong [argCLong (fromIntegral count), argPtr (castPtr raw_outError :: Ptr ())]

-- | Sets the preferred input orientation. The input orientation determines which directions will be left and right when a built-in mic data source with the AVAudioSessionPolarPatternStereo polar pattern is selected. Typically, this orientation should match how the user is holding the device while recording, which will match the application's interface orientation when a single app is on the screen. The actual input orientation may be different, for example, if another app's session is in control of routing. The input orientation is independent of the orientation property of an AVAudioSessionDataSourceDescription.
--
-- ObjC selector: @- setPreferredInputOrientation:error:@
setPreferredInputOrientation_error :: (IsAVAudioSession avAudioSession, IsNSError outError) => avAudioSession -> AVAudioStereoOrientation -> outError -> IO Bool
setPreferredInputOrientation_error avAudioSession  orientation outError =
withObjCPtr outError $ \raw_outError ->
    fmap ((/= 0) :: CULong -> Bool) $ sendMsg avAudioSession (mkSelector "setPreferredInputOrientation:error:") retCULong [argCLong (coerce orientation), argPtr (castPtr raw_outError :: Ptr ())]

-- | A value defined over the range [0.0, 1.0], with 0.0 corresponding to the lowest analog	gain setting and 1.0 corresponding to the highest analog gain setting.
--
-- Attempting to set values outside of the defined range will result in the value being "clamped"	to a valid input.  This is a global input gain setting that applies to the current input source	for the entire system. When no applications are using the input gain control, the system will	restore the default input gain setting for the input source.  Note that some audio accessories,	such as USB devices, may not have a default value.  This property is only valid if	inputGainSettable is true.  Note: inputGain is key-value observable.
--
-- ObjC selector: @- setInputGain:error:@
setInputGain_error :: (IsAVAudioSession avAudioSession, IsNSError outError) => avAudioSession -> CFloat -> outError -> IO Bool
setInputGain_error avAudioSession  gain outError =
withObjCPtr outError $ \raw_outError ->
    fmap ((/= 0) :: CULong -> Bool) $ sendMsg avAudioSession (mkSelector "setInputGain:error:") retCULong [argCFloat (fromIntegral gain), argPtr (castPtr raw_outError :: Ptr ())]

-- | Select a new input data source. Setting a nil value will clear the data source preference.
--
-- ObjC selector: @- setInputDataSource:error:@
setInputDataSource_error :: (IsAVAudioSession avAudioSession, IsAVAudioSessionDataSourceDescription dataSource, IsNSError outError) => avAudioSession -> dataSource -> outError -> IO Bool
setInputDataSource_error avAudioSession  dataSource outError =
withObjCPtr dataSource $ \raw_dataSource ->
  withObjCPtr outError $ \raw_outError ->
      fmap ((/= 0) :: CULong -> Bool) $ sendMsg avAudioSession (mkSelector "setInputDataSource:error:") retCULong [argPtr (castPtr raw_dataSource :: Ptr ()), argPtr (castPtr raw_outError :: Ptr ())]

-- | Select a new output data source. Setting a nil value will clear the data source preference.
--
-- ObjC selector: @- setOutputDataSource:error:@
setOutputDataSource_error :: (IsAVAudioSession avAudioSession, IsAVAudioSessionDataSourceDescription dataSource, IsNSError outError) => avAudioSession -> dataSource -> outError -> IO Bool
setOutputDataSource_error avAudioSession  dataSource outError =
withObjCPtr dataSource $ \raw_dataSource ->
  withObjCPtr outError $ \raw_outError ->
      fmap ((/= 0) :: CULong -> Bool) $ sendMsg avAudioSession (mkSelector "setOutputDataSource:error:") retCULong [argPtr (castPtr raw_dataSource :: Ptr ()), argPtr (castPtr raw_outError :: Ptr ())]

-- | Set the session active or inactive.
--
-- Note that activating an audio session is a synchronous (blocking) operation.	Therefore, we recommend that applications not activate their session from a thread where a long	blocking operation will be problematic.	Apps may activate a AVAudioSessionCategoryPlayback session when another app is hosting a	call (to start a SharePlay activity for example). However, they are not permitted to capture the    microphone of the active call, so attempts to activate a session with category	AVAudioSessionCategoryRecord or AVAudioSessionCategoryPlayAndRecord will fail with error	AVAudioSessionErrorCodeInsufficientPriority.	When deactivating a session, the caller is required to	first stop or pause all running I/Os (e.g. audio queues, players, recorders, converters,	remote I/Os, etc.). Starting in iOS 8, if the session has running I/Os at the time that	deactivation is requested, the session will be deactivated, but the method will return NO and	populate the NSError with the code property set to AVAudioSessionErrorCodeIsBusy to indicate the	misuse of the API. Prior to iOS 8, the session would have remained active if it had running I/Os	at the time of the deactivation request. Starting in iOS 26.0, deactivating while IO is running will	no longer return AVAudioSessionErrorCodeIsBusy.
--
-- ObjC selector: @- setActive:error:@
setActive_error :: (IsAVAudioSession avAudioSession, IsNSError outError) => avAudioSession -> Bool -> outError -> IO Bool
setActive_error avAudioSession  active outError =
withObjCPtr outError $ \raw_outError ->
    fmap ((/= 0) :: CULong -> Bool) $ sendMsg avAudioSession (mkSelector "setActive:error:") retCULong [argCULong (if active then 1 else 0), argPtr (castPtr raw_outError :: Ptr ())]

-- | @- setActive:withOptions:error:@
setActive_withOptions_error :: (IsAVAudioSession avAudioSession, IsNSError outError) => avAudioSession -> Bool -> AVAudioSessionSetActiveOptions -> outError -> IO Bool
setActive_withOptions_error avAudioSession  active options outError =
withObjCPtr outError $ \raw_outError ->
    fmap ((/= 0) :: CULong -> Bool) $ sendMsg avAudioSession (mkSelector "setActive:withOptions:error:") retCULong [argCULong (if active then 1 else 0), argCULong (coerce options), argPtr (castPtr raw_outError :: Ptr ())]

-- | Asynchronously activate the session.
--
-- This is a relatively time consuming operation. The completion handler will be called when the	activation completes or if an error occurs while attempting to activate the session. If the	session is configured to use AVAudioSessionRouteSharingPolicyLongFormAudio on watchOS, this	method will also cause a route picker to be presented to the user in cases where an appropriate	output route has not already been selected automatically. watchOS apps using	AVAudioSessionRouteSharingPolicyLongFormAudio should be prepared for this method to fail if no	eligible audio route can be activated or if the user cancels the route picker view.
--
-- ObjC selector: @- activateWithOptions:completionHandler:@
activateWithOptions_completionHandler :: IsAVAudioSession avAudioSession => avAudioSession -> AVAudioSessionActivationOptions -> Ptr () -> IO ()
activateWithOptions_completionHandler avAudioSession  options handler =
  sendMsg avAudioSession (mkSelector "activateWithOptions:completionHandler:") retVoid [argCULong (coerce options), argPtr (castPtr handler :: Ptr ())]

-- | Get session category. Examples: AVAudioSessionCategoryRecord, AVAudioSessionCategoryPlayAndRecord, etc.
--
-- ObjC selector: @- category@
category :: IsAVAudioSession avAudioSession => avAudioSession -> IO (Id NSString)
category avAudioSession  =
  sendMsg avAudioSession (mkSelector "category") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | Get the current set of AVAudioSessionCategoryOptions.
--
-- ObjC selector: @- categoryOptions@
categoryOptions :: IsAVAudioSession avAudioSession => avAudioSession -> IO AVAudioSessionCategoryOptions
categoryOptions avAudioSession  =
  fmap (coerce :: CULong -> AVAudioSessionCategoryOptions) $ sendMsg avAudioSession (mkSelector "categoryOptions") retCULong []

-- | Get the route sharing policy.
--
-- See AVAudioSessionRouteSharingPolicy for a description of the available policies.	See setCategory:mode:routeSharingPolicy:options:error: for additional discussion.
--
-- ObjC selector: @- routeSharingPolicy@
routeSharingPolicy :: IsAVAudioSession avAudioSession => avAudioSession -> IO AVAudioSessionRouteSharingPolicy
routeSharingPolicy avAudioSession  =
  fmap (coerce :: CULong -> AVAudioSessionRouteSharingPolicy) $ sendMsg avAudioSession (mkSelector "routeSharingPolicy") retCULong []

-- | Get the session's mode.
--
-- ObjC selector: @- mode@
mode :: IsAVAudioSession avAudioSession => avAudioSession -> IO (Id NSString)
mode avAudioSession  =
  sendMsg avAudioSession (mkSelector "mode") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | Whether system sounds and haptics can play while the session is actively using audio input.
--
-- ObjC selector: @- allowHapticsAndSystemSoundsDuringRecording@
allowHapticsAndSystemSoundsDuringRecording :: IsAVAudioSession avAudioSession => avAudioSession -> IO Bool
allowHapticsAndSystemSoundsDuringRecording avAudioSession  =
  fmap ((/= 0) :: CULong -> Bool) $ sendMsg avAudioSession (mkSelector "allowHapticsAndSystemSoundsDuringRecording") retCULong []

-- | Returns an enum indicating whether the user has granted or denied permission to record, or has not been asked
--
-- ObjC selector: @- recordPermission@
recordPermission :: IsAVAudioSession avAudioSession => avAudioSession -> IO AVAudioSessionRecordPermission
recordPermission avAudioSession  =
  fmap (coerce :: CULong -> AVAudioSessionRecordPermission) $ sendMsg avAudioSession (mkSelector "recordPermission") retCULong []

-- | Get the preferred input port.  Will be nil if no preference has been set.
--
-- ObjC selector: @- preferredInput@
preferredInput :: IsAVAudioSession avAudioSession => avAudioSession -> IO (Id AVAudioSessionPortDescription)
preferredInput avAudioSession  =
  sendMsg avAudioSession (mkSelector "preferredInput") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- prefersNoInterruptionsFromSystemAlerts@
prefersNoInterruptionsFromSystemAlerts :: IsAVAudioSession avAudioSession => avAudioSession -> IO Bool
prefersNoInterruptionsFromSystemAlerts avAudioSession  =
  fmap ((/= 0) :: CULong -> Bool) $ sendMsg avAudioSession (mkSelector "prefersNoInterruptionsFromSystemAlerts") retCULong []

-- | Get the currently resolved rendering mode to badge content appropriately. Clients should use this property to determine what to badge content as.
--
-- ObjC selector: @- renderingMode@
renderingMode :: IsAVAudioSession avAudioSession => avAudioSession -> IO AVAudioSessionRenderingMode
renderingMode avAudioSession  =
  fmap (coerce :: CLong -> AVAudioSessionRenderingMode) $ sendMsg avAudioSession (mkSelector "renderingMode") retCLong []

-- | @- prefersEchoCancelledInput@
prefersEchoCancelledInput :: IsAVAudioSession avAudioSession => avAudioSession -> IO Bool
prefersEchoCancelledInput avAudioSession  =
  fmap ((/= 0) :: CULong -> Bool) $ sendMsg avAudioSession (mkSelector "prefersEchoCancelledInput") retCULong []

-- | Returns YES if echo cancelled input is successfully enabled on an active session. Please see @prefersEchoCancelledInput@ above for more details.
--
-- ObjC selector: @- isEchoCancelledInputEnabled@
isEchoCancelledInputEnabled :: IsAVAudioSession avAudioSession => avAudioSession -> IO Bool
isEchoCancelledInputEnabled avAudioSession  =
  fmap ((/= 0) :: CULong -> Bool) $ sendMsg avAudioSession (mkSelector "isEchoCancelledInputEnabled") retCULong []

-- | This property will return YES if the device supports echo cancellation with the following category and mode combinations:	- ``AVAudioSessionCategoryPlayAndRecord`` with ``AVAudioSessionModeDefault``	- ``AVAudioSessionCategoryMultiRoute`` with ``AVAudioSessionModeDualRoute``
--
-- Query whether built-in mic / built-in speaker route supports echo cancellation for the session's given category and mode.
--
-- ObjC selector: @- isEchoCancelledInputAvailable@
isEchoCancelledInputAvailable :: IsAVAudioSession avAudioSession => avAudioSession -> IO Bool
isEchoCancelledInputAvailable avAudioSession  =
  fmap ((/= 0) :: CULong -> Bool) $ sendMsg avAudioSession (mkSelector "isEchoCancelledInputAvailable") retCULong []

-- | A Boolean value that indicates whether audio output is in a muted state.
--
-- ObjC selector: @- outputMuted@
outputMuted :: IsAVAudioSession avAudioSession => avAudioSession -> IO Bool
outputMuted avAudioSession  =
  fmap ((/= 0) :: CULong -> Bool) $ sendMsg avAudioSession (mkSelector "outputMuted") retCULong []

-- | @- inputIsAvailable@
inputIsAvailable :: IsAVAudioSession avAudioSession => avAudioSession -> IO Bool
inputIsAvailable avAudioSession  =
  fmap ((/= 0) :: CULong -> Bool) $ sendMsg avAudioSession (mkSelector "inputIsAvailable") retCULong []

-- | @- currentHardwareSampleRate@
currentHardwareSampleRate :: IsAVAudioSession avAudioSession => avAudioSession -> IO CDouble
currentHardwareSampleRate avAudioSession  =
  sendMsg avAudioSession (mkSelector "currentHardwareSampleRate") retCDouble []

-- | @- currentHardwareInputNumberOfChannels@
currentHardwareInputNumberOfChannels :: IsAVAudioSession avAudioSession => avAudioSession -> IO CLong
currentHardwareInputNumberOfChannels avAudioSession  =
  sendMsg avAudioSession (mkSelector "currentHardwareInputNumberOfChannels") retCLong []

-- | @- currentHardwareOutputNumberOfChannels@
currentHardwareOutputNumberOfChannels :: IsAVAudioSession avAudioSession => avAudioSession -> IO CLong
currentHardwareOutputNumberOfChannels avAudioSession  =
  sendMsg avAudioSession (mkSelector "currentHardwareOutputNumberOfChannels") retCLong []

-- | @- preferredHardwareSampleRate@
preferredHardwareSampleRate :: IsAVAudioSession avAudioSession => avAudioSession -> IO CDouble
preferredHardwareSampleRate avAudioSession  =
  sendMsg avAudioSession (mkSelector "preferredHardwareSampleRate") retCDouble []

-- | @- preferredMicrophoneInjectionMode@
preferredMicrophoneInjectionMode :: IsAVAudioSession avAudioSession => avAudioSession -> IO AVAudioSessionMicrophoneInjectionMode
preferredMicrophoneInjectionMode avAudioSession  =
  fmap (coerce :: CLong -> AVAudioSessionMicrophoneInjectionMode) $ sendMsg avAudioSession (mkSelector "preferredMicrophoneInjectionMode") retCLong []

-- | Indicates if microphone injection is available. Observe AVAudioSessionMicrophoneInjectionCapabilitiesChangeNotification for changes to this property
--
-- ObjC selector: @- isMicrophoneInjectionAvailable@
isMicrophoneInjectionAvailable :: IsAVAudioSession avAudioSession => avAudioSession -> IO Bool
isMicrophoneInjectionAvailable avAudioSession  =
  fmap ((/= 0) :: CULong -> Bool) $ sendMsg avAudioSession (mkSelector "isMicrophoneInjectionAvailable") retCULong []

-- | @- supportsMultichannelContent@
supportsMultichannelContent :: IsAVAudioSession avAudioSession => avAudioSession -> IO Bool
supportsMultichannelContent avAudioSession  =
  fmap ((/= 0) :: CULong -> Bool) $ sendMsg avAudioSession (mkSelector "supportsMultichannelContent") retCULong []

-- | Indicates if session will be interrupted on route disconnect.
--
-- ObjC selector: @- prefersInterruptionOnRouteDisconnect@
prefersInterruptionOnRouteDisconnect :: IsAVAudioSession avAudioSession => avAudioSession -> IO Bool
prefersInterruptionOnRouteDisconnect avAudioSession  =
  fmap ((/= 0) :: CULong -> Bool) $ sendMsg avAudioSession (mkSelector "prefersInterruptionOnRouteDisconnect") retCULong []

-- | True when another application is playing audio.
--
-- Note: As of iOS 8.0, Apple recommends that most applications use	secondaryAudioShouldBeSilencedHint instead of this property. The otherAudioPlaying property    will be true if any other audio (including audio from an app using    AVAudioSessionCategoryAmbient) is playing, whereas the secondaryAudioShouldBeSilencedHint    property is more restrictive in its consideration of whether primary audio from another    application is playing.
--
-- ObjC selector: @- otherAudioPlaying@
otherAudioPlaying :: IsAVAudioSession avAudioSession => avAudioSession -> IO Bool
otherAudioPlaying avAudioSession  =
  fmap ((/= 0) :: CULong -> Bool) $ sendMsg avAudioSession (mkSelector "otherAudioPlaying") retCULong []

-- | True when another application with a non-mixable audio session is playing audio.
--
-- Applications may use this property as a hint to silence audio that is secondary to the	functionality of the application. For example, a game app using AVAudioSessionCategoryAmbient	may use this property to decide to mute its soundtrack while leaving its sound effects unmuted.	Note: This property is closely related to AVAudioSessionSilenceSecondaryAudioHintNotification.
--
-- ObjC selector: @- secondaryAudioShouldBeSilencedHint@
secondaryAudioShouldBeSilencedHint :: IsAVAudioSession avAudioSession => avAudioSession -> IO Bool
secondaryAudioShouldBeSilencedHint avAudioSession  =
  fmap ((/= 0) :: CULong -> Bool) $ sendMsg avAudioSession (mkSelector "secondaryAudioShouldBeSilencedHint") retCULong []

-- | The current output volume. Value in range [0.0, 1.0]. Is key-value observable.
--
-- ObjC selector: @- outputVolume@
outputVolume :: IsAVAudioSession avAudioSession => avAudioSession -> IO CFloat
outputVolume avAudioSession  =
  sendMsg avAudioSession (mkSelector "outputVolume") retCFloat []

-- | The prompt style is a hint to sessions using AVAudioSessionModeVoicePrompt to alter the type of prompts they issue in response to other audio activity on the system, such as Siri and phone calls. This property is key-value observable.
--
-- ObjC selector: @- promptStyle@
promptStyle :: IsAVAudioSession avAudioSession => avAudioSession -> IO AVAudioSessionPromptStyle
promptStyle avAudioSession  =
  fmap (coerce :: CULong -> AVAudioSessionPromptStyle) $ sendMsg avAudioSession (mkSelector "promptStyle") retCULong []

-- | @- preferredSampleRate@
preferredSampleRate :: IsAVAudioSession avAudioSession => avAudioSession -> IO CDouble
preferredSampleRate avAudioSession  =
  sendMsg avAudioSession (mkSelector "preferredSampleRate") retCDouble []

-- | @- preferredIOBufferDuration@
preferredIOBufferDuration :: IsAVAudioSession avAudioSession => avAudioSession -> IO CDouble
preferredIOBufferDuration avAudioSession  =
  sendMsg avAudioSession (mkSelector "preferredIOBufferDuration") retCDouble []

-- | @- preferredInputNumberOfChannels@
preferredInputNumberOfChannels :: IsAVAudioSession avAudioSession => avAudioSession -> IO CLong
preferredInputNumberOfChannels avAudioSession  =
  sendMsg avAudioSession (mkSelector "preferredInputNumberOfChannels") retCLong []

-- | @- preferredOutputNumberOfChannels@
preferredOutputNumberOfChannels :: IsAVAudioSession avAudioSession => avAudioSession -> IO CLong
preferredOutputNumberOfChannels avAudioSession  =
  sendMsg avAudioSession (mkSelector "preferredOutputNumberOfChannels") retCLong []

-- | @- preferredInputOrientation@
preferredInputOrientation :: IsAVAudioSession avAudioSession => avAudioSession -> IO AVAudioStereoOrientation
preferredInputOrientation avAudioSession  =
  fmap (coerce :: CLong -> AVAudioStereoOrientation) $ sendMsg avAudioSession (mkSelector "preferredInputOrientation") retCLong []

-- | Describes the orientation of the input data source (valid for the built-in mic input data source when a stereo polar pattern is selected).
--
-- ObjC selector: @- inputOrientation@
inputOrientation :: IsAVAudioSession avAudioSession => avAudioSession -> IO AVAudioStereoOrientation
inputOrientation avAudioSession  =
  fmap (coerce :: CLong -> AVAudioStereoOrientation) $ sendMsg avAudioSession (mkSelector "inputOrientation") retCLong []

-- | Returns the largest number of audio input channels available for the current route
--
-- ObjC selector: @- maximumInputNumberOfChannels@
maximumInputNumberOfChannels :: IsAVAudioSession avAudioSession => avAudioSession -> IO CLong
maximumInputNumberOfChannels avAudioSession  =
  sendMsg avAudioSession (mkSelector "maximumInputNumberOfChannels") retCLong []

-- | Returns the largest number of audio output channels available for the current route
--
-- ObjC selector: @- maximumOutputNumberOfChannels@
maximumOutputNumberOfChannels :: IsAVAudioSession avAudioSession => avAudioSession -> IO CLong
maximumOutputNumberOfChannels avAudioSession  =
  sendMsg avAudioSession (mkSelector "maximumOutputNumberOfChannels") retCLong []

-- | value in range [0.0, 1.0]
--
-- ObjC selector: @- inputGain@
inputGain :: IsAVAudioSession avAudioSession => avAudioSession -> IO CFloat
inputGain avAudioSession  =
  sendMsg avAudioSession (mkSelector "inputGain") retCFloat []

-- | True when audio input gain is available.  Some input ports may not provide the ability to set the input gain, so check this value before attempting to set input gain.
--
-- ObjC selector: @- inputGainSettable@
inputGainSettable :: IsAVAudioSession avAudioSession => avAudioSession -> IO Bool
inputGainSettable avAudioSession  =
  fmap ((/= 0) :: CULong -> Bool) $ sendMsg avAudioSession (mkSelector "inputGainSettable") retCULong []

-- | True if input hardware is available. Key-value observable.
--
-- ObjC selector: @- inputAvailable@
inputAvailable :: IsAVAudioSession avAudioSession => avAudioSession -> IO Bool
inputAvailable avAudioSession  =
  fmap ((/= 0) :: CULong -> Bool) $ sendMsg avAudioSession (mkSelector "inputAvailable") retCULong []

-- | The current hardware sample rate. Is key-value observable (starting iOS 18.0).
--
-- ObjC selector: @- sampleRate@
sampleRate :: IsAVAudioSession avAudioSession => avAudioSession -> IO CDouble
sampleRate avAudioSession  =
  sendMsg avAudioSession (mkSelector "sampleRate") retCDouble []

-- | The current number of hardware input channels. Is key-value observable.
--
-- ObjC selector: @- inputNumberOfChannels@
inputNumberOfChannels :: IsAVAudioSession avAudioSession => avAudioSession -> IO CLong
inputNumberOfChannels avAudioSession  =
  sendMsg avAudioSession (mkSelector "inputNumberOfChannels") retCLong []

-- | The current number of hardware output channels. Is key-value observable.
--
-- ObjC selector: @- outputNumberOfChannels@
outputNumberOfChannels :: IsAVAudioSession avAudioSession => avAudioSession -> IO CLong
outputNumberOfChannels avAudioSession  =
  sendMsg avAudioSession (mkSelector "outputNumberOfChannels") retCLong []

-- | The current hardware input latency in seconds.
--
-- ObjC selector: @- inputLatency@
inputLatency :: IsAVAudioSession avAudioSession => avAudioSession -> IO CDouble
inputLatency avAudioSession  =
  sendMsg avAudioSession (mkSelector "inputLatency") retCDouble []

-- | The current hardware output latency in seconds.
--
-- ObjC selector: @- outputLatency@
outputLatency :: IsAVAudioSession avAudioSession => avAudioSession -> IO CDouble
outputLatency avAudioSession  =
  sendMsg avAudioSession (mkSelector "outputLatency") retCDouble []

-- | The current hardware IO buffer duration in seconds. Is key-value observable.
--
-- ObjC selector: @- IOBufferDuration@
ioBufferDuration :: IsAVAudioSession avAudioSession => avAudioSession -> IO CDouble
ioBufferDuration avAudioSession  =
  sendMsg avAudioSession (mkSelector "IOBufferDuration") retCDouble []

-- | Get an array of channel layouts that the current route supports. This property is only supported when the output is routed to ports of type AVAudioSessionPortCarAudio or AVAudioSessionPortAirPlay Otherwise, an empty array will be returned. Note that this will return an empty array if session is inactive. Clients should listen to AVAudioSessionRenderingCapabilitiesChangeNotification to be notified when this changes.
--
-- ObjC selector: @- supportedOutputChannelLayouts@
supportedOutputChannelLayouts :: IsAVAudioSession avAudioSession => avAudioSession -> IO (Id NSArray)
supportedOutputChannelLayouts avAudioSession  =
  sendMsg avAudioSession (mkSelector "supportedOutputChannelLayouts") (retPtr retVoid) [] >>= retainedObject . castPtr

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @sharedInstance@
sharedInstanceSelector :: Selector
sharedInstanceSelector = mkSelector "sharedInstance"

-- | @Selector@ for @setCategory:error:@
setCategory_errorSelector :: Selector
setCategory_errorSelector = mkSelector "setCategory:error:"

-- | @Selector@ for @setCategory:withOptions:error:@
setCategory_withOptions_errorSelector :: Selector
setCategory_withOptions_errorSelector = mkSelector "setCategory:withOptions:error:"

-- | @Selector@ for @setCategory:mode:options:error:@
setCategory_mode_options_errorSelector :: Selector
setCategory_mode_options_errorSelector = mkSelector "setCategory:mode:options:error:"

-- | @Selector@ for @setCategory:mode:routeSharingPolicy:options:error:@
setCategory_mode_routeSharingPolicy_options_errorSelector :: Selector
setCategory_mode_routeSharingPolicy_options_errorSelector = mkSelector "setCategory:mode:routeSharingPolicy:options:error:"

-- | @Selector@ for @setMode:error:@
setMode_errorSelector :: Selector
setMode_errorSelector = mkSelector "setMode:error:"

-- | @Selector@ for @setAllowHapticsAndSystemSoundsDuringRecording:error:@
setAllowHapticsAndSystemSoundsDuringRecording_errorSelector :: Selector
setAllowHapticsAndSystemSoundsDuringRecording_errorSelector = mkSelector "setAllowHapticsAndSystemSoundsDuringRecording:error:"

-- | @Selector@ for @requestRecordPermission:@
requestRecordPermissionSelector :: Selector
requestRecordPermissionSelector = mkSelector "requestRecordPermission:"

-- | @Selector@ for @overrideOutputAudioPort:error:@
overrideOutputAudioPort_errorSelector :: Selector
overrideOutputAudioPort_errorSelector = mkSelector "overrideOutputAudioPort:error:"

-- | @Selector@ for @setPreferredInput:error:@
setPreferredInput_errorSelector :: Selector
setPreferredInput_errorSelector = mkSelector "setPreferredInput:error:"

-- | @Selector@ for @setPrefersNoInterruptionsFromSystemAlerts:error:@
setPrefersNoInterruptionsFromSystemAlerts_errorSelector :: Selector
setPrefersNoInterruptionsFromSystemAlerts_errorSelector = mkSelector "setPrefersNoInterruptionsFromSystemAlerts:error:"

-- | @Selector@ for @setPrefersEchoCancelledInput:error:@
setPrefersEchoCancelledInput_errorSelector :: Selector
setPrefersEchoCancelledInput_errorSelector = mkSelector "setPrefersEchoCancelledInput:error:"

-- | @Selector@ for @setOutputMuted:error:@
setOutputMuted_errorSelector :: Selector
setOutputMuted_errorSelector = mkSelector "setOutputMuted:error:"

-- | @Selector@ for @init@
initSelector :: Selector
initSelector = mkSelector "init"

-- | @Selector@ for @setActive:withFlags:error:@
setActive_withFlags_errorSelector :: Selector
setActive_withFlags_errorSelector = mkSelector "setActive:withFlags:error:"

-- | @Selector@ for @setPreferredHardwareSampleRate:error:@
setPreferredHardwareSampleRate_errorSelector :: Selector
setPreferredHardwareSampleRate_errorSelector = mkSelector "setPreferredHardwareSampleRate:error:"

-- | @Selector@ for @setPreferredMicrophoneInjectionMode:error:@
setPreferredMicrophoneInjectionMode_errorSelector :: Selector
setPreferredMicrophoneInjectionMode_errorSelector = mkSelector "setPreferredMicrophoneInjectionMode:error:"

-- | @Selector@ for @setAggregatedIOPreference:error:@
setAggregatedIOPreference_errorSelector :: Selector
setAggregatedIOPreference_errorSelector = mkSelector "setAggregatedIOPreference:error:"

-- | @Selector@ for @setSupportsMultichannelContent:error:@
setSupportsMultichannelContent_errorSelector :: Selector
setSupportsMultichannelContent_errorSelector = mkSelector "setSupportsMultichannelContent:error:"

-- | @Selector@ for @setPrefersInterruptionOnRouteDisconnect:error:@
setPrefersInterruptionOnRouteDisconnect_errorSelector :: Selector
setPrefersInterruptionOnRouteDisconnect_errorSelector = mkSelector "setPrefersInterruptionOnRouteDisconnect:error:"

-- | @Selector@ for @setPreferredSampleRate:error:@
setPreferredSampleRate_errorSelector :: Selector
setPreferredSampleRate_errorSelector = mkSelector "setPreferredSampleRate:error:"

-- | @Selector@ for @setPreferredIOBufferDuration:error:@
setPreferredIOBufferDuration_errorSelector :: Selector
setPreferredIOBufferDuration_errorSelector = mkSelector "setPreferredIOBufferDuration:error:"

-- | @Selector@ for @setPreferredInputNumberOfChannels:error:@
setPreferredInputNumberOfChannels_errorSelector :: Selector
setPreferredInputNumberOfChannels_errorSelector = mkSelector "setPreferredInputNumberOfChannels:error:"

-- | @Selector@ for @setPreferredOutputNumberOfChannels:error:@
setPreferredOutputNumberOfChannels_errorSelector :: Selector
setPreferredOutputNumberOfChannels_errorSelector = mkSelector "setPreferredOutputNumberOfChannels:error:"

-- | @Selector@ for @setPreferredInputOrientation:error:@
setPreferredInputOrientation_errorSelector :: Selector
setPreferredInputOrientation_errorSelector = mkSelector "setPreferredInputOrientation:error:"

-- | @Selector@ for @setInputGain:error:@
setInputGain_errorSelector :: Selector
setInputGain_errorSelector = mkSelector "setInputGain:error:"

-- | @Selector@ for @setInputDataSource:error:@
setInputDataSource_errorSelector :: Selector
setInputDataSource_errorSelector = mkSelector "setInputDataSource:error:"

-- | @Selector@ for @setOutputDataSource:error:@
setOutputDataSource_errorSelector :: Selector
setOutputDataSource_errorSelector = mkSelector "setOutputDataSource:error:"

-- | @Selector@ for @setActive:error:@
setActive_errorSelector :: Selector
setActive_errorSelector = mkSelector "setActive:error:"

-- | @Selector@ for @setActive:withOptions:error:@
setActive_withOptions_errorSelector :: Selector
setActive_withOptions_errorSelector = mkSelector "setActive:withOptions:error:"

-- | @Selector@ for @activateWithOptions:completionHandler:@
activateWithOptions_completionHandlerSelector :: Selector
activateWithOptions_completionHandlerSelector = mkSelector "activateWithOptions:completionHandler:"

-- | @Selector@ for @category@
categorySelector :: Selector
categorySelector = mkSelector "category"

-- | @Selector@ for @categoryOptions@
categoryOptionsSelector :: Selector
categoryOptionsSelector = mkSelector "categoryOptions"

-- | @Selector@ for @routeSharingPolicy@
routeSharingPolicySelector :: Selector
routeSharingPolicySelector = mkSelector "routeSharingPolicy"

-- | @Selector@ for @mode@
modeSelector :: Selector
modeSelector = mkSelector "mode"

-- | @Selector@ for @allowHapticsAndSystemSoundsDuringRecording@
allowHapticsAndSystemSoundsDuringRecordingSelector :: Selector
allowHapticsAndSystemSoundsDuringRecordingSelector = mkSelector "allowHapticsAndSystemSoundsDuringRecording"

-- | @Selector@ for @recordPermission@
recordPermissionSelector :: Selector
recordPermissionSelector = mkSelector "recordPermission"

-- | @Selector@ for @preferredInput@
preferredInputSelector :: Selector
preferredInputSelector = mkSelector "preferredInput"

-- | @Selector@ for @prefersNoInterruptionsFromSystemAlerts@
prefersNoInterruptionsFromSystemAlertsSelector :: Selector
prefersNoInterruptionsFromSystemAlertsSelector = mkSelector "prefersNoInterruptionsFromSystemAlerts"

-- | @Selector@ for @renderingMode@
renderingModeSelector :: Selector
renderingModeSelector = mkSelector "renderingMode"

-- | @Selector@ for @prefersEchoCancelledInput@
prefersEchoCancelledInputSelector :: Selector
prefersEchoCancelledInputSelector = mkSelector "prefersEchoCancelledInput"

-- | @Selector@ for @isEchoCancelledInputEnabled@
isEchoCancelledInputEnabledSelector :: Selector
isEchoCancelledInputEnabledSelector = mkSelector "isEchoCancelledInputEnabled"

-- | @Selector@ for @isEchoCancelledInputAvailable@
isEchoCancelledInputAvailableSelector :: Selector
isEchoCancelledInputAvailableSelector = mkSelector "isEchoCancelledInputAvailable"

-- | @Selector@ for @outputMuted@
outputMutedSelector :: Selector
outputMutedSelector = mkSelector "outputMuted"

-- | @Selector@ for @inputIsAvailable@
inputIsAvailableSelector :: Selector
inputIsAvailableSelector = mkSelector "inputIsAvailable"

-- | @Selector@ for @currentHardwareSampleRate@
currentHardwareSampleRateSelector :: Selector
currentHardwareSampleRateSelector = mkSelector "currentHardwareSampleRate"

-- | @Selector@ for @currentHardwareInputNumberOfChannels@
currentHardwareInputNumberOfChannelsSelector :: Selector
currentHardwareInputNumberOfChannelsSelector = mkSelector "currentHardwareInputNumberOfChannels"

-- | @Selector@ for @currentHardwareOutputNumberOfChannels@
currentHardwareOutputNumberOfChannelsSelector :: Selector
currentHardwareOutputNumberOfChannelsSelector = mkSelector "currentHardwareOutputNumberOfChannels"

-- | @Selector@ for @preferredHardwareSampleRate@
preferredHardwareSampleRateSelector :: Selector
preferredHardwareSampleRateSelector = mkSelector "preferredHardwareSampleRate"

-- | @Selector@ for @preferredMicrophoneInjectionMode@
preferredMicrophoneInjectionModeSelector :: Selector
preferredMicrophoneInjectionModeSelector = mkSelector "preferredMicrophoneInjectionMode"

-- | @Selector@ for @isMicrophoneInjectionAvailable@
isMicrophoneInjectionAvailableSelector :: Selector
isMicrophoneInjectionAvailableSelector = mkSelector "isMicrophoneInjectionAvailable"

-- | @Selector@ for @supportsMultichannelContent@
supportsMultichannelContentSelector :: Selector
supportsMultichannelContentSelector = mkSelector "supportsMultichannelContent"

-- | @Selector@ for @prefersInterruptionOnRouteDisconnect@
prefersInterruptionOnRouteDisconnectSelector :: Selector
prefersInterruptionOnRouteDisconnectSelector = mkSelector "prefersInterruptionOnRouteDisconnect"

-- | @Selector@ for @otherAudioPlaying@
otherAudioPlayingSelector :: Selector
otherAudioPlayingSelector = mkSelector "otherAudioPlaying"

-- | @Selector@ for @secondaryAudioShouldBeSilencedHint@
secondaryAudioShouldBeSilencedHintSelector :: Selector
secondaryAudioShouldBeSilencedHintSelector = mkSelector "secondaryAudioShouldBeSilencedHint"

-- | @Selector@ for @outputVolume@
outputVolumeSelector :: Selector
outputVolumeSelector = mkSelector "outputVolume"

-- | @Selector@ for @promptStyle@
promptStyleSelector :: Selector
promptStyleSelector = mkSelector "promptStyle"

-- | @Selector@ for @preferredSampleRate@
preferredSampleRateSelector :: Selector
preferredSampleRateSelector = mkSelector "preferredSampleRate"

-- | @Selector@ for @preferredIOBufferDuration@
preferredIOBufferDurationSelector :: Selector
preferredIOBufferDurationSelector = mkSelector "preferredIOBufferDuration"

-- | @Selector@ for @preferredInputNumberOfChannels@
preferredInputNumberOfChannelsSelector :: Selector
preferredInputNumberOfChannelsSelector = mkSelector "preferredInputNumberOfChannels"

-- | @Selector@ for @preferredOutputNumberOfChannels@
preferredOutputNumberOfChannelsSelector :: Selector
preferredOutputNumberOfChannelsSelector = mkSelector "preferredOutputNumberOfChannels"

-- | @Selector@ for @preferredInputOrientation@
preferredInputOrientationSelector :: Selector
preferredInputOrientationSelector = mkSelector "preferredInputOrientation"

-- | @Selector@ for @inputOrientation@
inputOrientationSelector :: Selector
inputOrientationSelector = mkSelector "inputOrientation"

-- | @Selector@ for @maximumInputNumberOfChannels@
maximumInputNumberOfChannelsSelector :: Selector
maximumInputNumberOfChannelsSelector = mkSelector "maximumInputNumberOfChannels"

-- | @Selector@ for @maximumOutputNumberOfChannels@
maximumOutputNumberOfChannelsSelector :: Selector
maximumOutputNumberOfChannelsSelector = mkSelector "maximumOutputNumberOfChannels"

-- | @Selector@ for @inputGain@
inputGainSelector :: Selector
inputGainSelector = mkSelector "inputGain"

-- | @Selector@ for @inputGainSettable@
inputGainSettableSelector :: Selector
inputGainSettableSelector = mkSelector "inputGainSettable"

-- | @Selector@ for @inputAvailable@
inputAvailableSelector :: Selector
inputAvailableSelector = mkSelector "inputAvailable"

-- | @Selector@ for @sampleRate@
sampleRateSelector :: Selector
sampleRateSelector = mkSelector "sampleRate"

-- | @Selector@ for @inputNumberOfChannels@
inputNumberOfChannelsSelector :: Selector
inputNumberOfChannelsSelector = mkSelector "inputNumberOfChannels"

-- | @Selector@ for @outputNumberOfChannels@
outputNumberOfChannelsSelector :: Selector
outputNumberOfChannelsSelector = mkSelector "outputNumberOfChannels"

-- | @Selector@ for @inputLatency@
inputLatencySelector :: Selector
inputLatencySelector = mkSelector "inputLatency"

-- | @Selector@ for @outputLatency@
outputLatencySelector :: Selector
outputLatencySelector = mkSelector "outputLatency"

-- | @Selector@ for @IOBufferDuration@
ioBufferDurationSelector :: Selector
ioBufferDurationSelector = mkSelector "IOBufferDuration"

-- | @Selector@ for @supportedOutputChannelLayouts@
supportedOutputChannelLayoutsSelector :: Selector
supportedOutputChannelLayoutsSelector = mkSelector "supportedOutputChannelLayouts"

