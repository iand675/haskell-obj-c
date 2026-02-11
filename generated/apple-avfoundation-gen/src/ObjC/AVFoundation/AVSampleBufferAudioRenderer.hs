{-# LANGUAGE PatternSynonyms #-}
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
  , statusSelector
  , errorSelector
  , audioOutputDeviceUniqueIDSelector
  , setAudioOutputDeviceUniqueIDSelector
  , audioTimePitchAlgorithmSelector
  , setAudioTimePitchAlgorithmSelector
  , allowedAudioSpatializationFormatsSelector
  , setAllowedAudioSpatializationFormatsSelector
  , volumeSelector
  , setVolumeSelector
  , mutedSelector
  , setMutedSelector

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

-- | @- status@
status :: IsAVSampleBufferAudioRenderer avSampleBufferAudioRenderer => avSampleBufferAudioRenderer -> IO AVQueuedSampleBufferRenderingStatus
status avSampleBufferAudioRenderer  =
    fmap (coerce :: CLong -> AVQueuedSampleBufferRenderingStatus) $ sendMsg avSampleBufferAudioRenderer (mkSelector "status") retCLong []

-- | @- error@
error_ :: IsAVSampleBufferAudioRenderer avSampleBufferAudioRenderer => avSampleBufferAudioRenderer -> IO (Id NSError)
error_ avSampleBufferAudioRenderer  =
    sendMsg avSampleBufferAudioRenderer (mkSelector "error") (retPtr retVoid) [] >>= retainedObject . castPtr

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
audioOutputDeviceUniqueID avSampleBufferAudioRenderer  =
    sendMsg avSampleBufferAudioRenderer (mkSelector "audioOutputDeviceUniqueID") (retPtr retVoid) [] >>= retainedObject . castPtr

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
setAudioOutputDeviceUniqueID avSampleBufferAudioRenderer  value =
  withObjCPtr value $ \raw_value ->
      sendMsg avSampleBufferAudioRenderer (mkSelector "setAudioOutputDeviceUniqueID:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

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
audioTimePitchAlgorithm avSampleBufferAudioRenderer  =
    sendMsg avSampleBufferAudioRenderer (mkSelector "audioTimePitchAlgorithm") (retPtr retVoid) [] >>= retainedObject . castPtr

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
setAudioTimePitchAlgorithm avSampleBufferAudioRenderer  value =
  withObjCPtr value $ \raw_value ->
      sendMsg avSampleBufferAudioRenderer (mkSelector "setAudioTimePitchAlgorithm:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- | allowedAudioSpatializationFormats
--
-- Indicates the source audio channel layouts allowed by the receiver for spatialization.
--
-- Spatialization uses psychoacoustic methods to create a more immersive audio rendering when the content is played on specialized headphones and speaker arrangements. When an  AVSampleBufferAudioRenderer's allowedAudioSpatializationFormats property is set to AVAudioSpatializationFormatMonoAndStereo the  AVSampleBufferAudioRenderer will attempt to spatialize content tagged with a stereo channel layout, two-channel content with no layout specified as well as mono. It is considered incorrect to render a binaural recording with spatialization. A binaural recording is captured using two carefully placed microphones at each ear where the intent, when played on headphones, is to reproduce a naturally occurring spatial effect. Content tagged with a binaural channel layout will ignore this property value. When an  AVSampleBufferAudioRenderer's allowedAudioSpatializationFormats property is set to AVAudioSpatializationFormatMultichannel the  AVSampleBufferAudioRenderer will attempt to spatialize any decodable multichannel layout. Setting this property to AVAudioSpatializationFormatMonoStereoAndMultichannel indicates that the sender allows the  AVSampleBufferAudioRenderer to spatialize any decodable mono, stereo or multichannel layout. This property is not observable. The default value for this property is AVAudioSpatializationFormatMultichannel.
--
-- ObjC selector: @- allowedAudioSpatializationFormats@
allowedAudioSpatializationFormats :: IsAVSampleBufferAudioRenderer avSampleBufferAudioRenderer => avSampleBufferAudioRenderer -> IO AVAudioSpatializationFormats
allowedAudioSpatializationFormats avSampleBufferAudioRenderer  =
    fmap (coerce :: CULong -> AVAudioSpatializationFormats) $ sendMsg avSampleBufferAudioRenderer (mkSelector "allowedAudioSpatializationFormats") retCULong []

-- | allowedAudioSpatializationFormats
--
-- Indicates the source audio channel layouts allowed by the receiver for spatialization.
--
-- Spatialization uses psychoacoustic methods to create a more immersive audio rendering when the content is played on specialized headphones and speaker arrangements. When an  AVSampleBufferAudioRenderer's allowedAudioSpatializationFormats property is set to AVAudioSpatializationFormatMonoAndStereo the  AVSampleBufferAudioRenderer will attempt to spatialize content tagged with a stereo channel layout, two-channel content with no layout specified as well as mono. It is considered incorrect to render a binaural recording with spatialization. A binaural recording is captured using two carefully placed microphones at each ear where the intent, when played on headphones, is to reproduce a naturally occurring spatial effect. Content tagged with a binaural channel layout will ignore this property value. When an  AVSampleBufferAudioRenderer's allowedAudioSpatializationFormats property is set to AVAudioSpatializationFormatMultichannel the  AVSampleBufferAudioRenderer will attempt to spatialize any decodable multichannel layout. Setting this property to AVAudioSpatializationFormatMonoStereoAndMultichannel indicates that the sender allows the  AVSampleBufferAudioRenderer to spatialize any decodable mono, stereo or multichannel layout. This property is not observable. The default value for this property is AVAudioSpatializationFormatMultichannel.
--
-- ObjC selector: @- setAllowedAudioSpatializationFormats:@
setAllowedAudioSpatializationFormats :: IsAVSampleBufferAudioRenderer avSampleBufferAudioRenderer => avSampleBufferAudioRenderer -> AVAudioSpatializationFormats -> IO ()
setAllowedAudioSpatializationFormats avSampleBufferAudioRenderer  value =
    sendMsg avSampleBufferAudioRenderer (mkSelector "setAllowedAudioSpatializationFormats:") retVoid [argCULong (coerce value)]

-- | @- volume@
volume :: IsAVSampleBufferAudioRenderer avSampleBufferAudioRenderer => avSampleBufferAudioRenderer -> IO CFloat
volume avSampleBufferAudioRenderer  =
    sendMsg avSampleBufferAudioRenderer (mkSelector "volume") retCFloat []

-- | @- setVolume:@
setVolume :: IsAVSampleBufferAudioRenderer avSampleBufferAudioRenderer => avSampleBufferAudioRenderer -> CFloat -> IO ()
setVolume avSampleBufferAudioRenderer  value =
    sendMsg avSampleBufferAudioRenderer (mkSelector "setVolume:") retVoid [argCFloat value]

-- | @- muted@
muted :: IsAVSampleBufferAudioRenderer avSampleBufferAudioRenderer => avSampleBufferAudioRenderer -> IO Bool
muted avSampleBufferAudioRenderer  =
    fmap ((/= 0) :: CULong -> Bool) $ sendMsg avSampleBufferAudioRenderer (mkSelector "muted") retCULong []

-- | @- setMuted:@
setMuted :: IsAVSampleBufferAudioRenderer avSampleBufferAudioRenderer => avSampleBufferAudioRenderer -> Bool -> IO ()
setMuted avSampleBufferAudioRenderer  value =
    sendMsg avSampleBufferAudioRenderer (mkSelector "setMuted:") retVoid [argCULong (if value then 1 else 0)]

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @status@
statusSelector :: Selector
statusSelector = mkSelector "status"

-- | @Selector@ for @error@
errorSelector :: Selector
errorSelector = mkSelector "error"

-- | @Selector@ for @audioOutputDeviceUniqueID@
audioOutputDeviceUniqueIDSelector :: Selector
audioOutputDeviceUniqueIDSelector = mkSelector "audioOutputDeviceUniqueID"

-- | @Selector@ for @setAudioOutputDeviceUniqueID:@
setAudioOutputDeviceUniqueIDSelector :: Selector
setAudioOutputDeviceUniqueIDSelector = mkSelector "setAudioOutputDeviceUniqueID:"

-- | @Selector@ for @audioTimePitchAlgorithm@
audioTimePitchAlgorithmSelector :: Selector
audioTimePitchAlgorithmSelector = mkSelector "audioTimePitchAlgorithm"

-- | @Selector@ for @setAudioTimePitchAlgorithm:@
setAudioTimePitchAlgorithmSelector :: Selector
setAudioTimePitchAlgorithmSelector = mkSelector "setAudioTimePitchAlgorithm:"

-- | @Selector@ for @allowedAudioSpatializationFormats@
allowedAudioSpatializationFormatsSelector :: Selector
allowedAudioSpatializationFormatsSelector = mkSelector "allowedAudioSpatializationFormats"

-- | @Selector@ for @setAllowedAudioSpatializationFormats:@
setAllowedAudioSpatializationFormatsSelector :: Selector
setAllowedAudioSpatializationFormatsSelector = mkSelector "setAllowedAudioSpatializationFormats:"

-- | @Selector@ for @volume@
volumeSelector :: Selector
volumeSelector = mkSelector "volume"

-- | @Selector@ for @setVolume:@
setVolumeSelector :: Selector
setVolumeSelector = mkSelector "setVolume:"

-- | @Selector@ for @muted@
mutedSelector :: Selector
mutedSelector = mkSelector "muted"

-- | @Selector@ for @setMuted:@
setMutedSelector :: Selector
setMutedSelector = mkSelector "setMuted:"

