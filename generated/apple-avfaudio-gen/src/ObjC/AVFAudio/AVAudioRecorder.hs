{-# LANGUAGE DataKinds #-}
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
  , averagePowerForChannelSelector
  , channelAssignmentsSelector
  , currentTimeSelector
  , delegateSelector
  , deleteRecordingSelector
  , deviceCurrentTimeSelector
  , formatSelector
  , initWithURL_format_errorSelector
  , initWithURL_settings_errorSelector
  , meteringEnabledSelector
  , pauseSelector
  , peakPowerForChannelSelector
  , prepareToRecordSelector
  , recordAtTimeSelector
  , recordAtTime_forDurationSelector
  , recordForDurationSelector
  , recordSelector
  , recordingSelector
  , setChannelAssignmentsSelector
  , setDelegateSelector
  , setMeteringEnabledSelector
  , settingsSelector
  , stopSelector
  , updateMetersSelector
  , urlSelector


  ) where

import Foreign.Ptr (Ptr, FunPtr)
import Foreign.C.Types

import ObjC.Runtime.Types
import ObjC.Runtime.Message (sendMessage, sendOwnedMessage, sendClassMessage, sendOwnedClassMessage)
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
initWithURL_settings_error avAudioRecorder url settings outError =
  sendOwnedMessage avAudioRecorder initWithURL_settings_errorSelector (toNSURL url) (toNSDictionary settings) (toNSError outError)

-- | initWithURL:format:error:
--
-- Init the AudioRecorder with a specified url and format.
--
-- The file type to create can be set through the corresponding settings key. If not set, it will be inferred from the file extension. Will overwrite a file at the specified url if a file exists.
--
-- ObjC selector: @- initWithURL:format:error:@
initWithURL_format_error :: (IsAVAudioRecorder avAudioRecorder, IsNSURL url, IsAVAudioFormat format, IsNSError outError) => avAudioRecorder -> url -> format -> outError -> IO (Id AVAudioRecorder)
initWithURL_format_error avAudioRecorder url format outError =
  sendOwnedMessage avAudioRecorder initWithURL_format_errorSelector (toNSURL url) (toAVAudioFormat format) (toNSError outError)

-- | prepareToRecord
--
-- Creates the output file and gets ready to record.
--
-- This method is called automatically on record. Returns YES on success and NO on failure.
--
-- ObjC selector: @- prepareToRecord@
prepareToRecord :: IsAVAudioRecorder avAudioRecorder => avAudioRecorder -> IO Bool
prepareToRecord avAudioRecorder =
  sendMessage avAudioRecorder prepareToRecordSelector

-- | record
--
-- Start or resume recording to file.
--
-- Returns YES on success and NO on failure.
--
-- ObjC selector: @- record@
record :: IsAVAudioRecorder avAudioRecorder => avAudioRecorder -> IO Bool
record avAudioRecorder =
  sendMessage avAudioRecorder recordSelector

-- | recordAtTime:
--
-- Start recording at specified time in the future.
--
-- Time is an absolute time based on and greater than deviceCurrentTime. Returns YES on success and NO on failure.
--
-- ObjC selector: @- recordAtTime:@
recordAtTime :: IsAVAudioRecorder avAudioRecorder => avAudioRecorder -> CDouble -> IO Bool
recordAtTime avAudioRecorder time =
  sendMessage avAudioRecorder recordAtTimeSelector time

-- | recordForDuration:
--
-- Record for a specified duration.
--
-- The recorder will stop when it has recorded this length of audio. Returns YES on success and NO on failure.
--
-- ObjC selector: @- recordForDuration:@
recordForDuration :: IsAVAudioRecorder avAudioRecorder => avAudioRecorder -> CDouble -> IO Bool
recordForDuration avAudioRecorder duration =
  sendMessage avAudioRecorder recordForDurationSelector duration

-- | recordAtTime:forDuration:
--
-- Record for a specified duration at a specified time in the future.
--
-- Time is an absolute time based on and greater than deviceCurrentTime. Returns YES on success and NO on failure.
--
-- ObjC selector: @- recordAtTime:forDuration:@
recordAtTime_forDuration :: IsAVAudioRecorder avAudioRecorder => avAudioRecorder -> CDouble -> CDouble -> IO Bool
recordAtTime_forDuration avAudioRecorder time duration =
  sendMessage avAudioRecorder recordAtTime_forDurationSelector time duration

-- | pause
--
-- Pause recording.
--
-- ObjC selector: @- pause@
pause :: IsAVAudioRecorder avAudioRecorder => avAudioRecorder -> IO ()
pause avAudioRecorder =
  sendMessage avAudioRecorder pauseSelector

-- | stop
--
-- Stop recording.
--
-- This method also closes the output file.
--
-- ObjC selector: @- stop@
stop :: IsAVAudioRecorder avAudioRecorder => avAudioRecorder -> IO ()
stop avAudioRecorder =
  sendMessage avAudioRecorder stopSelector

-- | deleteRecording
--
-- Delete the recorded file.
--
-- AudioRecorder must be stopped. Returns YES on success and NO on failure.
--
-- ObjC selector: @- deleteRecording@
deleteRecording :: IsAVAudioRecorder avAudioRecorder => avAudioRecorder -> IO Bool
deleteRecording avAudioRecorder =
  sendMessage avAudioRecorder deleteRecordingSelector

-- | updateMeters
--
-- Call this method to refresh meter values.
--
-- ObjC selector: @- updateMeters@
updateMeters :: IsAVAudioRecorder avAudioRecorder => avAudioRecorder -> IO ()
updateMeters avAudioRecorder =
  sendMessage avAudioRecorder updateMetersSelector

-- | peakPowerForChannel:
--
-- Returns peak power in decibels for a given channel.
--
-- ObjC selector: @- peakPowerForChannel:@
peakPowerForChannel :: IsAVAudioRecorder avAudioRecorder => avAudioRecorder -> CULong -> IO CFloat
peakPowerForChannel avAudioRecorder channelNumber =
  sendMessage avAudioRecorder peakPowerForChannelSelector channelNumber

-- | averagePowerForChannel:
--
-- Returns average power in decibels for a given channel.
--
-- ObjC selector: @- averagePowerForChannel:@
averagePowerForChannel :: IsAVAudioRecorder avAudioRecorder => avAudioRecorder -> CULong -> IO CFloat
averagePowerForChannel avAudioRecorder channelNumber =
  sendMessage avAudioRecorder averagePowerForChannelSelector channelNumber

-- | recording
--
-- Returns YES if the AudioRecorder is currently recording.
--
-- ObjC selector: @- recording@
recording :: IsAVAudioRecorder avAudioRecorder => avAudioRecorder -> IO Bool
recording avAudioRecorder =
  sendMessage avAudioRecorder recordingSelector

-- | url
--
-- URL of the recorded file.
--
-- ObjC selector: @- url@
url :: IsAVAudioRecorder avAudioRecorder => avAudioRecorder -> IO (Id NSURL)
url avAudioRecorder =
  sendMessage avAudioRecorder urlSelector

-- | settings
--
-- A dictionary of settings for the AudioRecorder.
--
-- These settings are fully valid only when prepareToRecord has been called. For supported key-value pairs, see https://developer.apple.com/documentation/avfaudio/avaudiorecorder/1388386-initwithurl?language=objc
--
-- ObjC selector: @- settings@
settings :: IsAVAudioRecorder avAudioRecorder => avAudioRecorder -> IO (Id NSDictionary)
settings avAudioRecorder =
  sendMessage avAudioRecorder settingsSelector

-- | format
--
-- The audio format of the AudioRecorder.
--
-- This property is fully valid only when prepareToRecord has been called.
--
-- ObjC selector: @- format@
format :: IsAVAudioRecorder avAudioRecorder => avAudioRecorder -> IO (Id AVAudioFormat)
format avAudioRecorder =
  sendMessage avAudioRecorder formatSelector

-- | delegate
--
-- A delegate object to the AudioRecorder that conforms to the AVAudioRecorderDelegate protocol.
--
-- ObjC selector: @- delegate@
delegate :: IsAVAudioRecorder avAudioRecorder => avAudioRecorder -> IO RawId
delegate avAudioRecorder =
  sendMessage avAudioRecorder delegateSelector

-- | delegate
--
-- A delegate object to the AudioRecorder that conforms to the AVAudioRecorderDelegate protocol.
--
-- ObjC selector: @- setDelegate:@
setDelegate :: IsAVAudioRecorder avAudioRecorder => avAudioRecorder -> RawId -> IO ()
setDelegate avAudioRecorder value =
  sendMessage avAudioRecorder setDelegateSelector value

-- | currentTime
--
-- Get the current time of the recording.
--
-- This method is only vaild while recording.
--
-- ObjC selector: @- currentTime@
currentTime :: IsAVAudioRecorder avAudioRecorder => avAudioRecorder -> IO CDouble
currentTime avAudioRecorder =
  sendMessage avAudioRecorder currentTimeSelector

-- | deviceCurrentTime
--
-- Get the device current time.
--
-- This method is always valid.
--
-- ObjC selector: @- deviceCurrentTime@
deviceCurrentTime :: IsAVAudioRecorder avAudioRecorder => avAudioRecorder -> IO CDouble
deviceCurrentTime avAudioRecorder =
  sendMessage avAudioRecorder deviceCurrentTimeSelector

-- | meteringEnabled
--
-- Turns level metering on or off.
--
-- Default is off.
--
-- ObjC selector: @- meteringEnabled@
meteringEnabled :: IsAVAudioRecorder avAudioRecorder => avAudioRecorder -> IO Bool
meteringEnabled avAudioRecorder =
  sendMessage avAudioRecorder meteringEnabledSelector

-- | meteringEnabled
--
-- Turns level metering on or off.
--
-- Default is off.
--
-- ObjC selector: @- setMeteringEnabled:@
setMeteringEnabled :: IsAVAudioRecorder avAudioRecorder => avAudioRecorder -> Bool -> IO ()
setMeteringEnabled avAudioRecorder value =
  sendMessage avAudioRecorder setMeteringEnabledSelector value

-- | channelAssignments
--
-- Array of AVAudioSessionChannelDescription objects
--
-- The channels property lets you assign the output to record specific channels as described by AVAudioSessionPortDescription's channels property. This property is nil valued until set. The array must have the same number of channels as returned by the numberOfChannels property.
--
-- ObjC selector: @- channelAssignments@
channelAssignments :: IsAVAudioRecorder avAudioRecorder => avAudioRecorder -> IO (Id NSArray)
channelAssignments avAudioRecorder =
  sendMessage avAudioRecorder channelAssignmentsSelector

-- | channelAssignments
--
-- Array of AVAudioSessionChannelDescription objects
--
-- The channels property lets you assign the output to record specific channels as described by AVAudioSessionPortDescription's channels property. This property is nil valued until set. The array must have the same number of channels as returned by the numberOfChannels property.
--
-- ObjC selector: @- setChannelAssignments:@
setChannelAssignments :: (IsAVAudioRecorder avAudioRecorder, IsNSArray value) => avAudioRecorder -> value -> IO ()
setChannelAssignments avAudioRecorder value =
  sendMessage avAudioRecorder setChannelAssignmentsSelector (toNSArray value)

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @initWithURL:settings:error:@
initWithURL_settings_errorSelector :: Selector '[Id NSURL, Id NSDictionary, Id NSError] (Id AVAudioRecorder)
initWithURL_settings_errorSelector = mkSelector "initWithURL:settings:error:"

-- | @Selector@ for @initWithURL:format:error:@
initWithURL_format_errorSelector :: Selector '[Id NSURL, Id AVAudioFormat, Id NSError] (Id AVAudioRecorder)
initWithURL_format_errorSelector = mkSelector "initWithURL:format:error:"

-- | @Selector@ for @prepareToRecord@
prepareToRecordSelector :: Selector '[] Bool
prepareToRecordSelector = mkSelector "prepareToRecord"

-- | @Selector@ for @record@
recordSelector :: Selector '[] Bool
recordSelector = mkSelector "record"

-- | @Selector@ for @recordAtTime:@
recordAtTimeSelector :: Selector '[CDouble] Bool
recordAtTimeSelector = mkSelector "recordAtTime:"

-- | @Selector@ for @recordForDuration:@
recordForDurationSelector :: Selector '[CDouble] Bool
recordForDurationSelector = mkSelector "recordForDuration:"

-- | @Selector@ for @recordAtTime:forDuration:@
recordAtTime_forDurationSelector :: Selector '[CDouble, CDouble] Bool
recordAtTime_forDurationSelector = mkSelector "recordAtTime:forDuration:"

-- | @Selector@ for @pause@
pauseSelector :: Selector '[] ()
pauseSelector = mkSelector "pause"

-- | @Selector@ for @stop@
stopSelector :: Selector '[] ()
stopSelector = mkSelector "stop"

-- | @Selector@ for @deleteRecording@
deleteRecordingSelector :: Selector '[] Bool
deleteRecordingSelector = mkSelector "deleteRecording"

-- | @Selector@ for @updateMeters@
updateMetersSelector :: Selector '[] ()
updateMetersSelector = mkSelector "updateMeters"

-- | @Selector@ for @peakPowerForChannel:@
peakPowerForChannelSelector :: Selector '[CULong] CFloat
peakPowerForChannelSelector = mkSelector "peakPowerForChannel:"

-- | @Selector@ for @averagePowerForChannel:@
averagePowerForChannelSelector :: Selector '[CULong] CFloat
averagePowerForChannelSelector = mkSelector "averagePowerForChannel:"

-- | @Selector@ for @recording@
recordingSelector :: Selector '[] Bool
recordingSelector = mkSelector "recording"

-- | @Selector@ for @url@
urlSelector :: Selector '[] (Id NSURL)
urlSelector = mkSelector "url"

-- | @Selector@ for @settings@
settingsSelector :: Selector '[] (Id NSDictionary)
settingsSelector = mkSelector "settings"

-- | @Selector@ for @format@
formatSelector :: Selector '[] (Id AVAudioFormat)
formatSelector = mkSelector "format"

-- | @Selector@ for @delegate@
delegateSelector :: Selector '[] RawId
delegateSelector = mkSelector "delegate"

-- | @Selector@ for @setDelegate:@
setDelegateSelector :: Selector '[RawId] ()
setDelegateSelector = mkSelector "setDelegate:"

-- | @Selector@ for @currentTime@
currentTimeSelector :: Selector '[] CDouble
currentTimeSelector = mkSelector "currentTime"

-- | @Selector@ for @deviceCurrentTime@
deviceCurrentTimeSelector :: Selector '[] CDouble
deviceCurrentTimeSelector = mkSelector "deviceCurrentTime"

-- | @Selector@ for @meteringEnabled@
meteringEnabledSelector :: Selector '[] Bool
meteringEnabledSelector = mkSelector "meteringEnabled"

-- | @Selector@ for @setMeteringEnabled:@
setMeteringEnabledSelector :: Selector '[Bool] ()
setMeteringEnabledSelector = mkSelector "setMeteringEnabled:"

-- | @Selector@ for @channelAssignments@
channelAssignmentsSelector :: Selector '[] (Id NSArray)
channelAssignmentsSelector = mkSelector "channelAssignments"

-- | @Selector@ for @setChannelAssignments:@
setChannelAssignmentsSelector :: Selector '[Id NSArray] ()
setChannelAssignmentsSelector = mkSelector "setChannelAssignments:"

