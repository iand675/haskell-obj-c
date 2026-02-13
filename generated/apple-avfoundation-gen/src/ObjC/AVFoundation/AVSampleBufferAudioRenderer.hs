{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Generated bindings for @AVSampleBufferAudioRenderer@.
module ObjC.AVFoundation.AVSampleBufferAudioRenderer
  ( AVSampleBufferAudioRenderer
  , IsAVSampleBufferAudioRenderer(..)
  , status
  , error_
  , audioOutputDeviceUniqueID
  , setAudioOutputDeviceUniqueID
  , audioTimePitchAlgorithm
  , setAudioTimePitchAlgorithm
  , allowedAudioSpatializationFormats
  , setAllowedAudioSpatializationFormats
  , volume
  , setVolume
  , muted
  , setMuted
  , allowedAudioSpatializationFormatsSelector
  , audioOutputDeviceUniqueIDSelector
  , audioTimePitchAlgorithmSelector
  , errorSelector
  , mutedSelector
  , setAllowedAudioSpatializationFormatsSelector
  , setAudioOutputDeviceUniqueIDSelector
  , setAudioTimePitchAlgorithmSelector
  , setMutedSelector
  , setVolumeSelector
  , statusSelector
  , volumeSelector

  -- * Enum types
  , AVAudioSpatializationFormats(AVAudioSpatializationFormats)
  , pattern AVAudioSpatializationFormatNone
  , pattern AVAudioSpatializationFormatMonoAndStereo
  , pattern AVAudioSpatializationFormatMultichannel
  , pattern AVAudioSpatializationFormatMonoStereoAndMultichannel
  , AVQueuedSampleBufferRenderingStatus(AVQueuedSampleBufferRenderingStatus)
  , pattern AVQueuedSampleBufferRenderingStatusUnknown
  , pattern AVQueuedSampleBufferRenderingStatusRendering
  , pattern AVQueuedSampleBufferRenderingStatusFailed

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

-- | @- status@
status :: IsAVSampleBufferAudioRenderer avSampleBufferAudioRenderer => avSampleBufferAudioRenderer -> IO AVQueuedSampleBufferRenderingStatus
status avSampleBufferAudioRenderer =
  sendMessage avSampleBufferAudioRenderer statusSelector

-- | @- error@
error_ :: IsAVSampleBufferAudioRenderer avSampleBufferAudioRenderer => avSampleBufferAudioRenderer -> IO (Id NSError)
error_ avSampleBufferAudioRenderer =
  sendMessage avSampleBufferAudioRenderer errorSelector

-- | audioOutputDeviceUniqueID
--
-- Specifies the unique ID of the Core Audio output device used to play audio.
--
-- By default, the value of this property is nil, indicating that the default audio output device is used. Otherwise the value of this property is an NSString containing the unique ID of the Core Audio output device to be used for audio output.
--
-- Core Audio's kAudioDevicePropertyDeviceUID is a suitable source of audio output device unique IDs.
--
-- Modifying this property while the timebase's rate is not 0.0 may cause the rate to briefly change to 0.0.
--
-- On macOS, the audio device clock may be used as the AVSampleBufferRenderSynchronizer's and all attached AVQueuedSampleBufferRendering's timebase's clocks.  If the audioOutputDeviceUniqueID is modified, the clocks of all these timebases may also change.
--
-- If multiple AVSampleBufferAudioRenderers with different values for audioOutputDeviceUniqueID are attached to the same AVSampleBufferRenderSynchronizer, audio may not stay in sync during playback.  To avoid this, ensure that all synchronized AVSampleBufferAudioRenderers are using the same audio output device.
--
-- ObjC selector: @- audioOutputDeviceUniqueID@
audioOutputDeviceUniqueID :: IsAVSampleBufferAudioRenderer avSampleBufferAudioRenderer => avSampleBufferAudioRenderer -> IO (Id NSString)
audioOutputDeviceUniqueID avSampleBufferAudioRenderer =
  sendMessage avSampleBufferAudioRenderer audioOutputDeviceUniqueIDSelector

-- | audioOutputDeviceUniqueID
--
-- Specifies the unique ID of the Core Audio output device used to play audio.
--
-- By default, the value of this property is nil, indicating that the default audio output device is used. Otherwise the value of this property is an NSString containing the unique ID of the Core Audio output device to be used for audio output.
--
-- Core Audio's kAudioDevicePropertyDeviceUID is a suitable source of audio output device unique IDs.
--
-- Modifying this property while the timebase's rate is not 0.0 may cause the rate to briefly change to 0.0.
--
-- On macOS, the audio device clock may be used as the AVSampleBufferRenderSynchronizer's and all attached AVQueuedSampleBufferRendering's timebase's clocks.  If the audioOutputDeviceUniqueID is modified, the clocks of all these timebases may also change.
--
-- If multiple AVSampleBufferAudioRenderers with different values for audioOutputDeviceUniqueID are attached to the same AVSampleBufferRenderSynchronizer, audio may not stay in sync during playback.  To avoid this, ensure that all synchronized AVSampleBufferAudioRenderers are using the same audio output device.
--
-- ObjC selector: @- setAudioOutputDeviceUniqueID:@
setAudioOutputDeviceUniqueID :: (IsAVSampleBufferAudioRenderer avSampleBufferAudioRenderer, IsNSString value) => avSampleBufferAudioRenderer -> value -> IO ()
setAudioOutputDeviceUniqueID avSampleBufferAudioRenderer value =
  sendMessage avSampleBufferAudioRenderer setAudioOutputDeviceUniqueIDSelector (toNSString value)

-- | audioTimePitchAlgorithm
--
-- Indicates the processing algorithm used to manage audio pitch at varying rates.
--
-- Constants for various time pitch algorithms, e.g. AVAudioTimePitchSpectral, are defined in AVAudioProcessingSettings.h.
--
-- The default value for applications linked on or after iOS 15.0 or macOS 12.0 is AVAudioTimePitchAlgorithmTimeDomain. For iOS versions prior to 15.0 the default value is AVAudioTimePitchAlgorithmLowQualityZeroLatency.		For macOS versions prior to 12.0 the default value is AVAudioTimePitchAlgorithmSpectral.
--
-- If the timebase's rate is not supported by the audioTimePitchAlgorithm, audio will be muted.
--
-- Modifying this property while the timebase's rate is not 0.0 may cause the rate to briefly change to 0.0.
--
-- ObjC selector: @- audioTimePitchAlgorithm@
audioTimePitchAlgorithm :: IsAVSampleBufferAudioRenderer avSampleBufferAudioRenderer => avSampleBufferAudioRenderer -> IO (Id NSString)
audioTimePitchAlgorithm avSampleBufferAudioRenderer =
  sendMessage avSampleBufferAudioRenderer audioTimePitchAlgorithmSelector

-- | audioTimePitchAlgorithm
--
-- Indicates the processing algorithm used to manage audio pitch at varying rates.
--
-- Constants for various time pitch algorithms, e.g. AVAudioTimePitchSpectral, are defined in AVAudioProcessingSettings.h.
--
-- The default value for applications linked on or after iOS 15.0 or macOS 12.0 is AVAudioTimePitchAlgorithmTimeDomain. For iOS versions prior to 15.0 the default value is AVAudioTimePitchAlgorithmLowQualityZeroLatency.		For macOS versions prior to 12.0 the default value is AVAudioTimePitchAlgorithmSpectral.
--
-- If the timebase's rate is not supported by the audioTimePitchAlgorithm, audio will be muted.
--
-- Modifying this property while the timebase's rate is not 0.0 may cause the rate to briefly change to 0.0.
--
-- ObjC selector: @- setAudioTimePitchAlgorithm:@
setAudioTimePitchAlgorithm :: (IsAVSampleBufferAudioRenderer avSampleBufferAudioRenderer, IsNSString value) => avSampleBufferAudioRenderer -> value -> IO ()
setAudioTimePitchAlgorithm avSampleBufferAudioRenderer value =
  sendMessage avSampleBufferAudioRenderer setAudioTimePitchAlgorithmSelector (toNSString value)

-- | allowedAudioSpatializationFormats
--
-- Indicates the source audio channel layouts allowed by the receiver for spatialization.
--
-- Spatialization uses psychoacoustic methods to create a more immersive audio rendering when the content is played on specialized headphones and speaker arrangements. When an  AVSampleBufferAudioRenderer's allowedAudioSpatializationFormats property is set to AVAudioSpatializationFormatMonoAndStereo the  AVSampleBufferAudioRenderer will attempt to spatialize content tagged with a stereo channel layout, two-channel content with no layout specified as well as mono. It is considered incorrect to render a binaural recording with spatialization. A binaural recording is captured using two carefully placed microphones at each ear where the intent, when played on headphones, is to reproduce a naturally occurring spatial effect. Content tagged with a binaural channel layout will ignore this property value. When an  AVSampleBufferAudioRenderer's allowedAudioSpatializationFormats property is set to AVAudioSpatializationFormatMultichannel the  AVSampleBufferAudioRenderer will attempt to spatialize any decodable multichannel layout. Setting this property to AVAudioSpatializationFormatMonoStereoAndMultichannel indicates that the sender allows the  AVSampleBufferAudioRenderer to spatialize any decodable mono, stereo or multichannel layout. This property is not observable. The default value for this property is AVAudioSpatializationFormatMultichannel.
--
-- ObjC selector: @- allowedAudioSpatializationFormats@
allowedAudioSpatializationFormats :: IsAVSampleBufferAudioRenderer avSampleBufferAudioRenderer => avSampleBufferAudioRenderer -> IO AVAudioSpatializationFormats
allowedAudioSpatializationFormats avSampleBufferAudioRenderer =
  sendMessage avSampleBufferAudioRenderer allowedAudioSpatializationFormatsSelector

-- | allowedAudioSpatializationFormats
--
-- Indicates the source audio channel layouts allowed by the receiver for spatialization.
--
-- Spatialization uses psychoacoustic methods to create a more immersive audio rendering when the content is played on specialized headphones and speaker arrangements. When an  AVSampleBufferAudioRenderer's allowedAudioSpatializationFormats property is set to AVAudioSpatializationFormatMonoAndStereo the  AVSampleBufferAudioRenderer will attempt to spatialize content tagged with a stereo channel layout, two-channel content with no layout specified as well as mono. It is considered incorrect to render a binaural recording with spatialization. A binaural recording is captured using two carefully placed microphones at each ear where the intent, when played on headphones, is to reproduce a naturally occurring spatial effect. Content tagged with a binaural channel layout will ignore this property value. When an  AVSampleBufferAudioRenderer's allowedAudioSpatializationFormats property is set to AVAudioSpatializationFormatMultichannel the  AVSampleBufferAudioRenderer will attempt to spatialize any decodable multichannel layout. Setting this property to AVAudioSpatializationFormatMonoStereoAndMultichannel indicates that the sender allows the  AVSampleBufferAudioRenderer to spatialize any decodable mono, stereo or multichannel layout. This property is not observable. The default value for this property is AVAudioSpatializationFormatMultichannel.
--
-- ObjC selector: @- setAllowedAudioSpatializationFormats:@
setAllowedAudioSpatializationFormats :: IsAVSampleBufferAudioRenderer avSampleBufferAudioRenderer => avSampleBufferAudioRenderer -> AVAudioSpatializationFormats -> IO ()
setAllowedAudioSpatializationFormats avSampleBufferAudioRenderer value =
  sendMessage avSampleBufferAudioRenderer setAllowedAudioSpatializationFormatsSelector value

-- | @- volume@
volume :: IsAVSampleBufferAudioRenderer avSampleBufferAudioRenderer => avSampleBufferAudioRenderer -> IO CFloat
volume avSampleBufferAudioRenderer =
  sendMessage avSampleBufferAudioRenderer volumeSelector

-- | @- setVolume:@
setVolume :: IsAVSampleBufferAudioRenderer avSampleBufferAudioRenderer => avSampleBufferAudioRenderer -> CFloat -> IO ()
setVolume avSampleBufferAudioRenderer value =
  sendMessage avSampleBufferAudioRenderer setVolumeSelector value

-- | @- muted@
muted :: IsAVSampleBufferAudioRenderer avSampleBufferAudioRenderer => avSampleBufferAudioRenderer -> IO Bool
muted avSampleBufferAudioRenderer =
  sendMessage avSampleBufferAudioRenderer mutedSelector

-- | @- setMuted:@
setMuted :: IsAVSampleBufferAudioRenderer avSampleBufferAudioRenderer => avSampleBufferAudioRenderer -> Bool -> IO ()
setMuted avSampleBufferAudioRenderer value =
  sendMessage avSampleBufferAudioRenderer setMutedSelector value

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @status@
statusSelector :: Selector '[] AVQueuedSampleBufferRenderingStatus
statusSelector = mkSelector "status"

-- | @Selector@ for @error@
errorSelector :: Selector '[] (Id NSError)
errorSelector = mkSelector "error"

-- | @Selector@ for @audioOutputDeviceUniqueID@
audioOutputDeviceUniqueIDSelector :: Selector '[] (Id NSString)
audioOutputDeviceUniqueIDSelector = mkSelector "audioOutputDeviceUniqueID"

-- | @Selector@ for @setAudioOutputDeviceUniqueID:@
setAudioOutputDeviceUniqueIDSelector :: Selector '[Id NSString] ()
setAudioOutputDeviceUniqueIDSelector = mkSelector "setAudioOutputDeviceUniqueID:"

-- | @Selector@ for @audioTimePitchAlgorithm@
audioTimePitchAlgorithmSelector :: Selector '[] (Id NSString)
audioTimePitchAlgorithmSelector = mkSelector "audioTimePitchAlgorithm"

-- | @Selector@ for @setAudioTimePitchAlgorithm:@
setAudioTimePitchAlgorithmSelector :: Selector '[Id NSString] ()
setAudioTimePitchAlgorithmSelector = mkSelector "setAudioTimePitchAlgorithm:"

-- | @Selector@ for @allowedAudioSpatializationFormats@
allowedAudioSpatializationFormatsSelector :: Selector '[] AVAudioSpatializationFormats
allowedAudioSpatializationFormatsSelector = mkSelector "allowedAudioSpatializationFormats"

-- | @Selector@ for @setAllowedAudioSpatializationFormats:@
setAllowedAudioSpatializationFormatsSelector :: Selector '[AVAudioSpatializationFormats] ()
setAllowedAudioSpatializationFormatsSelector = mkSelector "setAllowedAudioSpatializationFormats:"

-- | @Selector@ for @volume@
volumeSelector :: Selector '[] CFloat
volumeSelector = mkSelector "volume"

-- | @Selector@ for @setVolume:@
setVolumeSelector :: Selector '[CFloat] ()
setVolumeSelector = mkSelector "setVolume:"

-- | @Selector@ for @muted@
mutedSelector :: Selector '[] Bool
mutedSelector = mkSelector "muted"

-- | @Selector@ for @setMuted:@
setMutedSelector :: Selector '[Bool] ()
setMutedSelector = mkSelector "setMuted:"

