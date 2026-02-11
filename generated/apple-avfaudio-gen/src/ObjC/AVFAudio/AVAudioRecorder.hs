{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | AVAudioRecorder
--
-- An object that records audio data to a file.
--
-- Generated bindings for @AVAudioRecorder@.
module ObjC.AVFAudio.AVAudioRecorder
  ( AVAudioRecorder
  , IsAVAudioRecorder(..)
  , initWithURL_settings_error
  , initWithURL_format_error
  , prepareToRecord
  , record
  , recordAtTime
  , recordForDuration
  , recordAtTime_forDuration
  , pause
  , stop
  , deleteRecording
  , updateMeters
  , peakPowerForChannel
  , averagePowerForChannel
  , recording
  , url
  , settings
  , format
  , delegate
  , setDelegate
  , currentTime
  , deviceCurrentTime
  , meteringEnabled
  , setMeteringEnabled
  , channelAssignments
  , setChannelAssignments
  , initWithURL_settings_errorSelector
  , initWithURL_format_errorSelector
  , prepareToRecordSelector
  , recordSelector
  , recordAtTimeSelector
  , recordForDurationSelector
  , recordAtTime_forDurationSelector
  , pauseSelector
  , stopSelector
  , deleteRecordingSelector
  , updateMetersSelector
  , peakPowerForChannelSelector
  , averagePowerForChannelSelector
  , recordingSelector
  , urlSelector
  , settingsSelector
  , formatSelector
  , delegateSelector
  , setDelegateSelector
  , currentTimeSelector
  , deviceCurrentTimeSelector
  , meteringEnabledSelector
  , setMeteringEnabledSelector
  , channelAssignmentsSelector
  , setChannelAssignmentsSelector


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
import ObjC.Foundation.Internal.Classes

-- | initWithURL:settings:error:
--
-- Init the AudioRecorder with a specified url and settings.
--
-- The file type to create can be set through the corresponding settings key. If not set, it will be inferred from the file extension. Will overwrite a file at the specified url if a file exists.
--
-- ObjC selector: @- initWithURL:settings:error:@
initWithURL_settings_error :: (IsAVAudioRecorder avAudioRecorder, IsNSURL url, IsNSDictionary settings, IsNSError outError) => avAudioRecorder -> url -> settings -> outError -> IO (Id AVAudioRecorder)
initWithURL_settings_error avAudioRecorder  url settings outError =
  withObjCPtr url $ \raw_url ->
    withObjCPtr settings $ \raw_settings ->
      withObjCPtr outError $ \raw_outError ->
          sendMsg avAudioRecorder (mkSelector "initWithURL:settings:error:") (retPtr retVoid) [argPtr (castPtr raw_url :: Ptr ()), argPtr (castPtr raw_settings :: Ptr ()), argPtr (castPtr raw_outError :: Ptr ())] >>= ownedObject . castPtr

-- | initWithURL:format:error:
--
-- Init the AudioRecorder with a specified url and format.
--
-- The file type to create can be set through the corresponding settings key. If not set, it will be inferred from the file extension. Will overwrite a file at the specified url if a file exists.
--
-- ObjC selector: @- initWithURL:format:error:@
initWithURL_format_error :: (IsAVAudioRecorder avAudioRecorder, IsNSURL url, IsAVAudioFormat format, IsNSError outError) => avAudioRecorder -> url -> format -> outError -> IO (Id AVAudioRecorder)
initWithURL_format_error avAudioRecorder  url format outError =
  withObjCPtr url $ \raw_url ->
    withObjCPtr format $ \raw_format ->
      withObjCPtr outError $ \raw_outError ->
          sendMsg avAudioRecorder (mkSelector "initWithURL:format:error:") (retPtr retVoid) [argPtr (castPtr raw_url :: Ptr ()), argPtr (castPtr raw_format :: Ptr ()), argPtr (castPtr raw_outError :: Ptr ())] >>= ownedObject . castPtr

-- | prepareToRecord
--
-- Creates the output file and gets ready to record.
--
-- This method is called automatically on record. Returns YES on success and NO on failure.
--
-- ObjC selector: @- prepareToRecord@
prepareToRecord :: IsAVAudioRecorder avAudioRecorder => avAudioRecorder -> IO Bool
prepareToRecord avAudioRecorder  =
    fmap ((/= 0) :: CULong -> Bool) $ sendMsg avAudioRecorder (mkSelector "prepareToRecord") retCULong []

-- | record
--
-- Start or resume recording to file.
--
-- Returns YES on success and NO on failure.
--
-- ObjC selector: @- record@
record :: IsAVAudioRecorder avAudioRecorder => avAudioRecorder -> IO Bool
record avAudioRecorder  =
    fmap ((/= 0) :: CULong -> Bool) $ sendMsg avAudioRecorder (mkSelector "record") retCULong []

-- | recordAtTime:
--
-- Start recording at specified time in the future.
--
-- Time is an absolute time based on and greater than deviceCurrentTime. Returns YES on success and NO on failure.
--
-- ObjC selector: @- recordAtTime:@
recordAtTime :: IsAVAudioRecorder avAudioRecorder => avAudioRecorder -> CDouble -> IO Bool
recordAtTime avAudioRecorder  time =
    fmap ((/= 0) :: CULong -> Bool) $ sendMsg avAudioRecorder (mkSelector "recordAtTime:") retCULong [argCDouble time]

-- | recordForDuration:
--
-- Record for a specified duration.
--
-- The recorder will stop when it has recorded this length of audio. Returns YES on success and NO on failure.
--
-- ObjC selector: @- recordForDuration:@
recordForDuration :: IsAVAudioRecorder avAudioRecorder => avAudioRecorder -> CDouble -> IO Bool
recordForDuration avAudioRecorder  duration =
    fmap ((/= 0) :: CULong -> Bool) $ sendMsg avAudioRecorder (mkSelector "recordForDuration:") retCULong [argCDouble duration]

-- | recordAtTime:forDuration:
--
-- Record for a specified duration at a specified time in the future.
--
-- Time is an absolute time based on and greater than deviceCurrentTime. Returns YES on success and NO on failure.
--
-- ObjC selector: @- recordAtTime:forDuration:@
recordAtTime_forDuration :: IsAVAudioRecorder avAudioRecorder => avAudioRecorder -> CDouble -> CDouble -> IO Bool
recordAtTime_forDuration avAudioRecorder  time duration =
    fmap ((/= 0) :: CULong -> Bool) $ sendMsg avAudioRecorder (mkSelector "recordAtTime:forDuration:") retCULong [argCDouble time, argCDouble duration]

-- | pause
--
-- Pause recording.
--
-- ObjC selector: @- pause@
pause :: IsAVAudioRecorder avAudioRecorder => avAudioRecorder -> IO ()
pause avAudioRecorder  =
    sendMsg avAudioRecorder (mkSelector "pause") retVoid []

-- | stop
--
-- Stop recording.
--
-- This method also closes the output file.
--
-- ObjC selector: @- stop@
stop :: IsAVAudioRecorder avAudioRecorder => avAudioRecorder -> IO ()
stop avAudioRecorder  =
    sendMsg avAudioRecorder (mkSelector "stop") retVoid []

-- | deleteRecording
--
-- Delete the recorded file.
--
-- AudioRecorder must be stopped. Returns YES on success and NO on failure.
--
-- ObjC selector: @- deleteRecording@
deleteRecording :: IsAVAudioRecorder avAudioRecorder => avAudioRecorder -> IO Bool
deleteRecording avAudioRecorder  =
    fmap ((/= 0) :: CULong -> Bool) $ sendMsg avAudioRecorder (mkSelector "deleteRecording") retCULong []

-- | updateMeters
--
-- Call this method to refresh meter values.
--
-- ObjC selector: @- updateMeters@
updateMeters :: IsAVAudioRecorder avAudioRecorder => avAudioRecorder -> IO ()
updateMeters avAudioRecorder  =
    sendMsg avAudioRecorder (mkSelector "updateMeters") retVoid []

-- | peakPowerForChannel:
--
-- Returns peak power in decibels for a given channel.
--
-- ObjC selector: @- peakPowerForChannel:@
peakPowerForChannel :: IsAVAudioRecorder avAudioRecorder => avAudioRecorder -> CULong -> IO CFloat
peakPowerForChannel avAudioRecorder  channelNumber =
    sendMsg avAudioRecorder (mkSelector "peakPowerForChannel:") retCFloat [argCULong channelNumber]

-- | averagePowerForChannel:
--
-- Returns average power in decibels for a given channel.
--
-- ObjC selector: @- averagePowerForChannel:@
averagePowerForChannel :: IsAVAudioRecorder avAudioRecorder => avAudioRecorder -> CULong -> IO CFloat
averagePowerForChannel avAudioRecorder  channelNumber =
    sendMsg avAudioRecorder (mkSelector "averagePowerForChannel:") retCFloat [argCULong channelNumber]

-- | recording
--
-- Returns YES if the AudioRecorder is currently recording.
--
-- ObjC selector: @- recording@
recording :: IsAVAudioRecorder avAudioRecorder => avAudioRecorder -> IO Bool
recording avAudioRecorder  =
    fmap ((/= 0) :: CULong -> Bool) $ sendMsg avAudioRecorder (mkSelector "recording") retCULong []

-- | url
--
-- URL of the recorded file.
--
-- ObjC selector: @- url@
url :: IsAVAudioRecorder avAudioRecorder => avAudioRecorder -> IO (Id NSURL)
url avAudioRecorder  =
    sendMsg avAudioRecorder (mkSelector "url") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | settings
--
-- A dictionary of settings for the AudioRecorder.
--
-- These settings are fully valid only when prepareToRecord has been called. For supported key-value pairs, see https://developer.apple.com/documentation/avfaudio/avaudiorecorder/1388386-initwithurl?language=objc
--
-- ObjC selector: @- settings@
settings :: IsAVAudioRecorder avAudioRecorder => avAudioRecorder -> IO (Id NSDictionary)
settings avAudioRecorder  =
    sendMsg avAudioRecorder (mkSelector "settings") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | format
--
-- The audio format of the AudioRecorder.
--
-- This property is fully valid only when prepareToRecord has been called.
--
-- ObjC selector: @- format@
format :: IsAVAudioRecorder avAudioRecorder => avAudioRecorder -> IO (Id AVAudioFormat)
format avAudioRecorder  =
    sendMsg avAudioRecorder (mkSelector "format") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | delegate
--
-- A delegate object to the AudioRecorder that conforms to the AVAudioRecorderDelegate protocol.
--
-- ObjC selector: @- delegate@
delegate :: IsAVAudioRecorder avAudioRecorder => avAudioRecorder -> IO RawId
delegate avAudioRecorder  =
    fmap (RawId . castPtr) $ sendMsg avAudioRecorder (mkSelector "delegate") (retPtr retVoid) []

-- | delegate
--
-- A delegate object to the AudioRecorder that conforms to the AVAudioRecorderDelegate protocol.
--
-- ObjC selector: @- setDelegate:@
setDelegate :: IsAVAudioRecorder avAudioRecorder => avAudioRecorder -> RawId -> IO ()
setDelegate avAudioRecorder  value =
    sendMsg avAudioRecorder (mkSelector "setDelegate:") retVoid [argPtr (castPtr (unRawId value) :: Ptr ())]

-- | currentTime
--
-- Get the current time of the recording.
--
-- This method is only vaild while recording.
--
-- ObjC selector: @- currentTime@
currentTime :: IsAVAudioRecorder avAudioRecorder => avAudioRecorder -> IO CDouble
currentTime avAudioRecorder  =
    sendMsg avAudioRecorder (mkSelector "currentTime") retCDouble []

-- | deviceCurrentTime
--
-- Get the device current time.
--
-- This method is always valid.
--
-- ObjC selector: @- deviceCurrentTime@
deviceCurrentTime :: IsAVAudioRecorder avAudioRecorder => avAudioRecorder -> IO CDouble
deviceCurrentTime avAudioRecorder  =
    sendMsg avAudioRecorder (mkSelector "deviceCurrentTime") retCDouble []

-- | meteringEnabled
--
-- Turns level metering on or off.
--
-- Default is off.
--
-- ObjC selector: @- meteringEnabled@
meteringEnabled :: IsAVAudioRecorder avAudioRecorder => avAudioRecorder -> IO Bool
meteringEnabled avAudioRecorder  =
    fmap ((/= 0) :: CULong -> Bool) $ sendMsg avAudioRecorder (mkSelector "meteringEnabled") retCULong []

-- | meteringEnabled
--
-- Turns level metering on or off.
--
-- Default is off.
--
-- ObjC selector: @- setMeteringEnabled:@
setMeteringEnabled :: IsAVAudioRecorder avAudioRecorder => avAudioRecorder -> Bool -> IO ()
setMeteringEnabled avAudioRecorder  value =
    sendMsg avAudioRecorder (mkSelector "setMeteringEnabled:") retVoid [argCULong (if value then 1 else 0)]

-- | channelAssignments
--
-- Array of AVAudioSessionChannelDescription objects
--
-- The channels property lets you assign the output to record specific channels as described by AVAudioSessionPortDescription's channels property. This property is nil valued until set. The array must have the same number of channels as returned by the numberOfChannels property.
--
-- ObjC selector: @- channelAssignments@
channelAssignments :: IsAVAudioRecorder avAudioRecorder => avAudioRecorder -> IO (Id NSArray)
channelAssignments avAudioRecorder  =
    sendMsg avAudioRecorder (mkSelector "channelAssignments") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | channelAssignments
--
-- Array of AVAudioSessionChannelDescription objects
--
-- The channels property lets you assign the output to record specific channels as described by AVAudioSessionPortDescription's channels property. This property is nil valued until set. The array must have the same number of channels as returned by the numberOfChannels property.
--
-- ObjC selector: @- setChannelAssignments:@
setChannelAssignments :: (IsAVAudioRecorder avAudioRecorder, IsNSArray value) => avAudioRecorder -> value -> IO ()
setChannelAssignments avAudioRecorder  value =
  withObjCPtr value $ \raw_value ->
      sendMsg avAudioRecorder (mkSelector "setChannelAssignments:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @initWithURL:settings:error:@
initWithURL_settings_errorSelector :: Selector
initWithURL_settings_errorSelector = mkSelector "initWithURL:settings:error:"

-- | @Selector@ for @initWithURL:format:error:@
initWithURL_format_errorSelector :: Selector
initWithURL_format_errorSelector = mkSelector "initWithURL:format:error:"

-- | @Selector@ for @prepareToRecord@
prepareToRecordSelector :: Selector
prepareToRecordSelector = mkSelector "prepareToRecord"

-- | @Selector@ for @record@
recordSelector :: Selector
recordSelector = mkSelector "record"

-- | @Selector@ for @recordAtTime:@
recordAtTimeSelector :: Selector
recordAtTimeSelector = mkSelector "recordAtTime:"

-- | @Selector@ for @recordForDuration:@
recordForDurationSelector :: Selector
recordForDurationSelector = mkSelector "recordForDuration:"

-- | @Selector@ for @recordAtTime:forDuration:@
recordAtTime_forDurationSelector :: Selector
recordAtTime_forDurationSelector = mkSelector "recordAtTime:forDuration:"

-- | @Selector@ for @pause@
pauseSelector :: Selector
pauseSelector = mkSelector "pause"

-- | @Selector@ for @stop@
stopSelector :: Selector
stopSelector = mkSelector "stop"

-- | @Selector@ for @deleteRecording@
deleteRecordingSelector :: Selector
deleteRecordingSelector = mkSelector "deleteRecording"

-- | @Selector@ for @updateMeters@
updateMetersSelector :: Selector
updateMetersSelector = mkSelector "updateMeters"

-- | @Selector@ for @peakPowerForChannel:@
peakPowerForChannelSelector :: Selector
peakPowerForChannelSelector = mkSelector "peakPowerForChannel:"

-- | @Selector@ for @averagePowerForChannel:@
averagePowerForChannelSelector :: Selector
averagePowerForChannelSelector = mkSelector "averagePowerForChannel:"

-- | @Selector@ for @recording@
recordingSelector :: Selector
recordingSelector = mkSelector "recording"

-- | @Selector@ for @url@
urlSelector :: Selector
urlSelector = mkSelector "url"

-- | @Selector@ for @settings@
settingsSelector :: Selector
settingsSelector = mkSelector "settings"

-- | @Selector@ for @format@
formatSelector :: Selector
formatSelector = mkSelector "format"

-- | @Selector@ for @delegate@
delegateSelector :: Selector
delegateSelector = mkSelector "delegate"

-- | @Selector@ for @setDelegate:@
setDelegateSelector :: Selector
setDelegateSelector = mkSelector "setDelegate:"

-- | @Selector@ for @currentTime@
currentTimeSelector :: Selector
currentTimeSelector = mkSelector "currentTime"

-- | @Selector@ for @deviceCurrentTime@
deviceCurrentTimeSelector :: Selector
deviceCurrentTimeSelector = mkSelector "deviceCurrentTime"

-- | @Selector@ for @meteringEnabled@
meteringEnabledSelector :: Selector
meteringEnabledSelector = mkSelector "meteringEnabled"

-- | @Selector@ for @setMeteringEnabled:@
setMeteringEnabledSelector :: Selector
setMeteringEnabledSelector = mkSelector "setMeteringEnabled:"

-- | @Selector@ for @channelAssignments@
channelAssignmentsSelector :: Selector
channelAssignmentsSelector = mkSelector "channelAssignments"

-- | @Selector@ for @setChannelAssignments:@
setChannelAssignmentsSelector :: Selector
setChannelAssignmentsSelector = mkSelector "setChannelAssignments:"

