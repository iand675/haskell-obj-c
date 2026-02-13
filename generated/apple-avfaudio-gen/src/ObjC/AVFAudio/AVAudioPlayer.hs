{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Generated bindings for @AVAudioPlayer@.
module ObjC.AVFAudio.AVAudioPlayer
  ( AVAudioPlayer
  , IsAVAudioPlayer(..)
  , initWithContentsOfURL_error
  , initWithData_error
  , initWithContentsOfURL_fileTypeHint_error
  , initWithData_fileTypeHint_error
  , prepareToPlay
  , play
  , playAtTime
  , pause
  , stop
  , setVolume_fadeDuration
  , updateMeters
  , peakPowerForChannel
  , averagePowerForChannel
  , playing
  , numberOfChannels
  , duration
  , currentDevice
  , setCurrentDevice
  , delegate
  , setDelegate
  , url
  , data_
  , pan
  , setPan
  , volume
  , setVolume
  , enableRate
  , setEnableRate
  , rate
  , setRate
  , currentTime
  , setCurrentTime
  , deviceCurrentTime
  , numberOfLoops
  , setNumberOfLoops
  , settings
  , format
  , meteringEnabled
  , setMeteringEnabled
  , channelAssignments
  , setChannelAssignments
  , intendedSpatialExperience
  , setIntendedSpatialExperience
  , averagePowerForChannelSelector
  , channelAssignmentsSelector
  , currentDeviceSelector
  , currentTimeSelector
  , dataSelector
  , delegateSelector
  , deviceCurrentTimeSelector
  , durationSelector
  , enableRateSelector
  , formatSelector
  , initWithContentsOfURL_errorSelector
  , initWithContentsOfURL_fileTypeHint_errorSelector
  , initWithData_errorSelector
  , initWithData_fileTypeHint_errorSelector
  , intendedSpatialExperienceSelector
  , meteringEnabledSelector
  , numberOfChannelsSelector
  , numberOfLoopsSelector
  , panSelector
  , pauseSelector
  , peakPowerForChannelSelector
  , playAtTimeSelector
  , playSelector
  , playingSelector
  , prepareToPlaySelector
  , rateSelector
  , setChannelAssignmentsSelector
  , setCurrentDeviceSelector
  , setCurrentTimeSelector
  , setDelegateSelector
  , setEnableRateSelector
  , setIntendedSpatialExperienceSelector
  , setMeteringEnabledSelector
  , setNumberOfLoopsSelector
  , setPanSelector
  , setRateSelector
  , setVolumeSelector
  , setVolume_fadeDurationSelector
  , settingsSelector
  , stopSelector
  , updateMetersSelector
  , urlSelector
  , volumeSelector


  ) where

import Foreign.Ptr (Ptr, FunPtr)
import Foreign.C.Types

import ObjC.Runtime.Types
import ObjC.Runtime.Message (sendMessage, sendOwnedMessage, sendClassMessage, sendOwnedClassMessage)
import ObjC.Runtime.Selector (mkSelector)
import ObjC.Runtime.Class (getRequiredClass)

import ObjC.AVFAudio.Internal.Classes
import ObjC.Foundation.Internal.Classes

-- | @- initWithContentsOfURL:error:@
initWithContentsOfURL_error :: (IsAVAudioPlayer avAudioPlayer, IsNSURL url, IsNSError outError) => avAudioPlayer -> url -> outError -> IO (Id AVAudioPlayer)
initWithContentsOfURL_error avAudioPlayer url outError =
  sendOwnedMessage avAudioPlayer initWithContentsOfURL_errorSelector (toNSURL url) (toNSError outError)

-- | @- initWithData:error:@
initWithData_error :: (IsAVAudioPlayer avAudioPlayer, IsNSData data_, IsNSError outError) => avAudioPlayer -> data_ -> outError -> IO (Id AVAudioPlayer)
initWithData_error avAudioPlayer data_ outError =
  sendOwnedMessage avAudioPlayer initWithData_errorSelector (toNSData data_) (toNSError outError)

-- | @- initWithContentsOfURL:fileTypeHint:error:@
initWithContentsOfURL_fileTypeHint_error :: (IsAVAudioPlayer avAudioPlayer, IsNSURL url, IsNSString utiString, IsNSError outError) => avAudioPlayer -> url -> utiString -> outError -> IO (Id AVAudioPlayer)
initWithContentsOfURL_fileTypeHint_error avAudioPlayer url utiString outError =
  sendOwnedMessage avAudioPlayer initWithContentsOfURL_fileTypeHint_errorSelector (toNSURL url) (toNSString utiString) (toNSError outError)

-- | @- initWithData:fileTypeHint:error:@
initWithData_fileTypeHint_error :: (IsAVAudioPlayer avAudioPlayer, IsNSData data_, IsNSString utiString, IsNSError outError) => avAudioPlayer -> data_ -> utiString -> outError -> IO (Id AVAudioPlayer)
initWithData_fileTypeHint_error avAudioPlayer data_ utiString outError =
  sendOwnedMessage avAudioPlayer initWithData_fileTypeHint_errorSelector (toNSData data_) (toNSString utiString) (toNSError outError)

-- | @- prepareToPlay@
prepareToPlay :: IsAVAudioPlayer avAudioPlayer => avAudioPlayer -> IO Bool
prepareToPlay avAudioPlayer =
  sendMessage avAudioPlayer prepareToPlaySelector

-- | @- play@
play :: IsAVAudioPlayer avAudioPlayer => avAudioPlayer -> IO Bool
play avAudioPlayer =
  sendMessage avAudioPlayer playSelector

-- | @- playAtTime:@
playAtTime :: IsAVAudioPlayer avAudioPlayer => avAudioPlayer -> CDouble -> IO Bool
playAtTime avAudioPlayer time =
  sendMessage avAudioPlayer playAtTimeSelector time

-- | @- pause@
pause :: IsAVAudioPlayer avAudioPlayer => avAudioPlayer -> IO ()
pause avAudioPlayer =
  sendMessage avAudioPlayer pauseSelector

-- | @- stop@
stop :: IsAVAudioPlayer avAudioPlayer => avAudioPlayer -> IO ()
stop avAudioPlayer =
  sendMessage avAudioPlayer stopSelector

-- | @- setVolume:fadeDuration:@
setVolume_fadeDuration :: IsAVAudioPlayer avAudioPlayer => avAudioPlayer -> CFloat -> CDouble -> IO ()
setVolume_fadeDuration avAudioPlayer volume duration =
  sendMessage avAudioPlayer setVolume_fadeDurationSelector volume duration

-- | @- updateMeters@
updateMeters :: IsAVAudioPlayer avAudioPlayer => avAudioPlayer -> IO ()
updateMeters avAudioPlayer =
  sendMessage avAudioPlayer updateMetersSelector

-- | @- peakPowerForChannel:@
peakPowerForChannel :: IsAVAudioPlayer avAudioPlayer => avAudioPlayer -> CULong -> IO CFloat
peakPowerForChannel avAudioPlayer channelNumber =
  sendMessage avAudioPlayer peakPowerForChannelSelector channelNumber

-- | @- averagePowerForChannel:@
averagePowerForChannel :: IsAVAudioPlayer avAudioPlayer => avAudioPlayer -> CULong -> IO CFloat
averagePowerForChannel avAudioPlayer channelNumber =
  sendMessage avAudioPlayer averagePowerForChannelSelector channelNumber

-- | @- playing@
playing :: IsAVAudioPlayer avAudioPlayer => avAudioPlayer -> IO Bool
playing avAudioPlayer =
  sendMessage avAudioPlayer playingSelector

-- | @- numberOfChannels@
numberOfChannels :: IsAVAudioPlayer avAudioPlayer => avAudioPlayer -> IO CULong
numberOfChannels avAudioPlayer =
  sendMessage avAudioPlayer numberOfChannelsSelector

-- | @- duration@
duration :: IsAVAudioPlayer avAudioPlayer => avAudioPlayer -> IO CDouble
duration avAudioPlayer =
  sendMessage avAudioPlayer durationSelector

-- | @- currentDevice@
currentDevice :: IsAVAudioPlayer avAudioPlayer => avAudioPlayer -> IO (Id NSString)
currentDevice avAudioPlayer =
  sendMessage avAudioPlayer currentDeviceSelector

-- | @- setCurrentDevice:@
setCurrentDevice :: (IsAVAudioPlayer avAudioPlayer, IsNSString value) => avAudioPlayer -> value -> IO ()
setCurrentDevice avAudioPlayer value =
  sendMessage avAudioPlayer setCurrentDeviceSelector (toNSString value)

-- | @- delegate@
delegate :: IsAVAudioPlayer avAudioPlayer => avAudioPlayer -> IO RawId
delegate avAudioPlayer =
  sendMessage avAudioPlayer delegateSelector

-- | @- setDelegate:@
setDelegate :: IsAVAudioPlayer avAudioPlayer => avAudioPlayer -> RawId -> IO ()
setDelegate avAudioPlayer value =
  sendMessage avAudioPlayer setDelegateSelector value

-- | @- url@
url :: IsAVAudioPlayer avAudioPlayer => avAudioPlayer -> IO (Id NSURL)
url avAudioPlayer =
  sendMessage avAudioPlayer urlSelector

-- | @- data@
data_ :: IsAVAudioPlayer avAudioPlayer => avAudioPlayer -> IO (Id NSData)
data_ avAudioPlayer =
  sendMessage avAudioPlayer dataSelector

-- | @- pan@
pan :: IsAVAudioPlayer avAudioPlayer => avAudioPlayer -> IO CFloat
pan avAudioPlayer =
  sendMessage avAudioPlayer panSelector

-- | @- setPan:@
setPan :: IsAVAudioPlayer avAudioPlayer => avAudioPlayer -> CFloat -> IO ()
setPan avAudioPlayer value =
  sendMessage avAudioPlayer setPanSelector value

-- | @- volume@
volume :: IsAVAudioPlayer avAudioPlayer => avAudioPlayer -> IO CFloat
volume avAudioPlayer =
  sendMessage avAudioPlayer volumeSelector

-- | @- setVolume:@
setVolume :: IsAVAudioPlayer avAudioPlayer => avAudioPlayer -> CFloat -> IO ()
setVolume avAudioPlayer value =
  sendMessage avAudioPlayer setVolumeSelector value

-- | @- enableRate@
enableRate :: IsAVAudioPlayer avAudioPlayer => avAudioPlayer -> IO Bool
enableRate avAudioPlayer =
  sendMessage avAudioPlayer enableRateSelector

-- | @- setEnableRate:@
setEnableRate :: IsAVAudioPlayer avAudioPlayer => avAudioPlayer -> Bool -> IO ()
setEnableRate avAudioPlayer value =
  sendMessage avAudioPlayer setEnableRateSelector value

-- | @- rate@
rate :: IsAVAudioPlayer avAudioPlayer => avAudioPlayer -> IO CFloat
rate avAudioPlayer =
  sendMessage avAudioPlayer rateSelector

-- | @- setRate:@
setRate :: IsAVAudioPlayer avAudioPlayer => avAudioPlayer -> CFloat -> IO ()
setRate avAudioPlayer value =
  sendMessage avAudioPlayer setRateSelector value

-- | @- currentTime@
currentTime :: IsAVAudioPlayer avAudioPlayer => avAudioPlayer -> IO CDouble
currentTime avAudioPlayer =
  sendMessage avAudioPlayer currentTimeSelector

-- | @- setCurrentTime:@
setCurrentTime :: IsAVAudioPlayer avAudioPlayer => avAudioPlayer -> CDouble -> IO ()
setCurrentTime avAudioPlayer value =
  sendMessage avAudioPlayer setCurrentTimeSelector value

-- | @- deviceCurrentTime@
deviceCurrentTime :: IsAVAudioPlayer avAudioPlayer => avAudioPlayer -> IO CDouble
deviceCurrentTime avAudioPlayer =
  sendMessage avAudioPlayer deviceCurrentTimeSelector

-- | @- numberOfLoops@
numberOfLoops :: IsAVAudioPlayer avAudioPlayer => avAudioPlayer -> IO CLong
numberOfLoops avAudioPlayer =
  sendMessage avAudioPlayer numberOfLoopsSelector

-- | @- setNumberOfLoops:@
setNumberOfLoops :: IsAVAudioPlayer avAudioPlayer => avAudioPlayer -> CLong -> IO ()
setNumberOfLoops avAudioPlayer value =
  sendMessage avAudioPlayer setNumberOfLoopsSelector value

-- | @- settings@
settings :: IsAVAudioPlayer avAudioPlayer => avAudioPlayer -> IO (Id NSDictionary)
settings avAudioPlayer =
  sendMessage avAudioPlayer settingsSelector

-- | @- format@
format :: IsAVAudioPlayer avAudioPlayer => avAudioPlayer -> IO (Id AVAudioFormat)
format avAudioPlayer =
  sendMessage avAudioPlayer formatSelector

-- | @- meteringEnabled@
meteringEnabled :: IsAVAudioPlayer avAudioPlayer => avAudioPlayer -> IO Bool
meteringEnabled avAudioPlayer =
  sendMessage avAudioPlayer meteringEnabledSelector

-- | @- setMeteringEnabled:@
setMeteringEnabled :: IsAVAudioPlayer avAudioPlayer => avAudioPlayer -> Bool -> IO ()
setMeteringEnabled avAudioPlayer value =
  sendMessage avAudioPlayer setMeteringEnabledSelector value

-- | @- channelAssignments@
channelAssignments :: IsAVAudioPlayer avAudioPlayer => avAudioPlayer -> IO (Id NSArray)
channelAssignments avAudioPlayer =
  sendMessage avAudioPlayer channelAssignmentsSelector

-- | @- setChannelAssignments:@
setChannelAssignments :: (IsAVAudioPlayer avAudioPlayer, IsNSArray value) => avAudioPlayer -> value -> IO ()
setChannelAssignments avAudioPlayer value =
  sendMessage avAudioPlayer setChannelAssignmentsSelector (toNSArray value)

-- | @- intendedSpatialExperience@
intendedSpatialExperience :: IsAVAudioPlayer avAudioPlayer => avAudioPlayer -> IO RawId
intendedSpatialExperience avAudioPlayer =
  sendMessage avAudioPlayer intendedSpatialExperienceSelector

-- | @- setIntendedSpatialExperience:@
setIntendedSpatialExperience :: IsAVAudioPlayer avAudioPlayer => avAudioPlayer -> RawId -> IO ()
setIntendedSpatialExperience avAudioPlayer value =
  sendMessage avAudioPlayer setIntendedSpatialExperienceSelector value

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @initWithContentsOfURL:error:@
initWithContentsOfURL_errorSelector :: Selector '[Id NSURL, Id NSError] (Id AVAudioPlayer)
initWithContentsOfURL_errorSelector = mkSelector "initWithContentsOfURL:error:"

-- | @Selector@ for @initWithData:error:@
initWithData_errorSelector :: Selector '[Id NSData, Id NSError] (Id AVAudioPlayer)
initWithData_errorSelector = mkSelector "initWithData:error:"

-- | @Selector@ for @initWithContentsOfURL:fileTypeHint:error:@
initWithContentsOfURL_fileTypeHint_errorSelector :: Selector '[Id NSURL, Id NSString, Id NSError] (Id AVAudioPlayer)
initWithContentsOfURL_fileTypeHint_errorSelector = mkSelector "initWithContentsOfURL:fileTypeHint:error:"

-- | @Selector@ for @initWithData:fileTypeHint:error:@
initWithData_fileTypeHint_errorSelector :: Selector '[Id NSData, Id NSString, Id NSError] (Id AVAudioPlayer)
initWithData_fileTypeHint_errorSelector = mkSelector "initWithData:fileTypeHint:error:"

-- | @Selector@ for @prepareToPlay@
prepareToPlaySelector :: Selector '[] Bool
prepareToPlaySelector = mkSelector "prepareToPlay"

-- | @Selector@ for @play@
playSelector :: Selector '[] Bool
playSelector = mkSelector "play"

-- | @Selector@ for @playAtTime:@
playAtTimeSelector :: Selector '[CDouble] Bool
playAtTimeSelector = mkSelector "playAtTime:"

-- | @Selector@ for @pause@
pauseSelector :: Selector '[] ()
pauseSelector = mkSelector "pause"

-- | @Selector@ for @stop@
stopSelector :: Selector '[] ()
stopSelector = mkSelector "stop"

-- | @Selector@ for @setVolume:fadeDuration:@
setVolume_fadeDurationSelector :: Selector '[CFloat, CDouble] ()
setVolume_fadeDurationSelector = mkSelector "setVolume:fadeDuration:"

-- | @Selector@ for @updateMeters@
updateMetersSelector :: Selector '[] ()
updateMetersSelector = mkSelector "updateMeters"

-- | @Selector@ for @peakPowerForChannel:@
peakPowerForChannelSelector :: Selector '[CULong] CFloat
peakPowerForChannelSelector = mkSelector "peakPowerForChannel:"

-- | @Selector@ for @averagePowerForChannel:@
averagePowerForChannelSelector :: Selector '[CULong] CFloat
averagePowerForChannelSelector = mkSelector "averagePowerForChannel:"

-- | @Selector@ for @playing@
playingSelector :: Selector '[] Bool
playingSelector = mkSelector "playing"

-- | @Selector@ for @numberOfChannels@
numberOfChannelsSelector :: Selector '[] CULong
numberOfChannelsSelector = mkSelector "numberOfChannels"

-- | @Selector@ for @duration@
durationSelector :: Selector '[] CDouble
durationSelector = mkSelector "duration"

-- | @Selector@ for @currentDevice@
currentDeviceSelector :: Selector '[] (Id NSString)
currentDeviceSelector = mkSelector "currentDevice"

-- | @Selector@ for @setCurrentDevice:@
setCurrentDeviceSelector :: Selector '[Id NSString] ()
setCurrentDeviceSelector = mkSelector "setCurrentDevice:"

-- | @Selector@ for @delegate@
delegateSelector :: Selector '[] RawId
delegateSelector = mkSelector "delegate"

-- | @Selector@ for @setDelegate:@
setDelegateSelector :: Selector '[RawId] ()
setDelegateSelector = mkSelector "setDelegate:"

-- | @Selector@ for @url@
urlSelector :: Selector '[] (Id NSURL)
urlSelector = mkSelector "url"

-- | @Selector@ for @data@
dataSelector :: Selector '[] (Id NSData)
dataSelector = mkSelector "data"

-- | @Selector@ for @pan@
panSelector :: Selector '[] CFloat
panSelector = mkSelector "pan"

-- | @Selector@ for @setPan:@
setPanSelector :: Selector '[CFloat] ()
setPanSelector = mkSelector "setPan:"

-- | @Selector@ for @volume@
volumeSelector :: Selector '[] CFloat
volumeSelector = mkSelector "volume"

-- | @Selector@ for @setVolume:@
setVolumeSelector :: Selector '[CFloat] ()
setVolumeSelector = mkSelector "setVolume:"

-- | @Selector@ for @enableRate@
enableRateSelector :: Selector '[] Bool
enableRateSelector = mkSelector "enableRate"

-- | @Selector@ for @setEnableRate:@
setEnableRateSelector :: Selector '[Bool] ()
setEnableRateSelector = mkSelector "setEnableRate:"

-- | @Selector@ for @rate@
rateSelector :: Selector '[] CFloat
rateSelector = mkSelector "rate"

-- | @Selector@ for @setRate:@
setRateSelector :: Selector '[CFloat] ()
setRateSelector = mkSelector "setRate:"

-- | @Selector@ for @currentTime@
currentTimeSelector :: Selector '[] CDouble
currentTimeSelector = mkSelector "currentTime"

-- | @Selector@ for @setCurrentTime:@
setCurrentTimeSelector :: Selector '[CDouble] ()
setCurrentTimeSelector = mkSelector "setCurrentTime:"

-- | @Selector@ for @deviceCurrentTime@
deviceCurrentTimeSelector :: Selector '[] CDouble
deviceCurrentTimeSelector = mkSelector "deviceCurrentTime"

-- | @Selector@ for @numberOfLoops@
numberOfLoopsSelector :: Selector '[] CLong
numberOfLoopsSelector = mkSelector "numberOfLoops"

-- | @Selector@ for @setNumberOfLoops:@
setNumberOfLoopsSelector :: Selector '[CLong] ()
setNumberOfLoopsSelector = mkSelector "setNumberOfLoops:"

-- | @Selector@ for @settings@
settingsSelector :: Selector '[] (Id NSDictionary)
settingsSelector = mkSelector "settings"

-- | @Selector@ for @format@
formatSelector :: Selector '[] (Id AVAudioFormat)
formatSelector = mkSelector "format"

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

-- | @Selector@ for @intendedSpatialExperience@
intendedSpatialExperienceSelector :: Selector '[] RawId
intendedSpatialExperienceSelector = mkSelector "intendedSpatialExperience"

-- | @Selector@ for @setIntendedSpatialExperience:@
setIntendedSpatialExperienceSelector :: Selector '[RawId] ()
setIntendedSpatialExperienceSelector = mkSelector "setIntendedSpatialExperience:"

