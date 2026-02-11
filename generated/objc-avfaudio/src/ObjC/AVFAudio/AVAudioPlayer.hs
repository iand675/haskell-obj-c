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
  , initWithContentsOfURL_errorSelector
  , initWithData_errorSelector
  , initWithContentsOfURL_fileTypeHint_errorSelector
  , initWithData_fileTypeHint_errorSelector
  , prepareToPlaySelector
  , playSelector
  , playAtTimeSelector
  , pauseSelector
  , stopSelector
  , setVolume_fadeDurationSelector
  , updateMetersSelector
  , peakPowerForChannelSelector
  , averagePowerForChannelSelector
  , playingSelector
  , numberOfChannelsSelector
  , durationSelector
  , urlSelector
  , dataSelector
  , panSelector
  , setPanSelector
  , volumeSelector
  , setVolumeSelector
  , enableRateSelector
  , setEnableRateSelector
  , rateSelector
  , setRateSelector
  , currentTimeSelector
  , setCurrentTimeSelector
  , deviceCurrentTimeSelector
  , numberOfLoopsSelector
  , setNumberOfLoopsSelector
  , settingsSelector
  , formatSelector
  , meteringEnabledSelector
  , setMeteringEnabledSelector


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

-- | @- initWithContentsOfURL:error:@
initWithContentsOfURL_error :: (IsAVAudioPlayer avAudioPlayer, IsNSURL url, IsNSError outError) => avAudioPlayer -> url -> outError -> IO (Id AVAudioPlayer)
initWithContentsOfURL_error avAudioPlayer  url outError =
withObjCPtr url $ \raw_url ->
  withObjCPtr outError $ \raw_outError ->
      sendMsg avAudioPlayer (mkSelector "initWithContentsOfURL:error:") (retPtr retVoid) [argPtr (castPtr raw_url :: Ptr ()), argPtr (castPtr raw_outError :: Ptr ())] >>= ownedObject . castPtr

-- | @- initWithData:error:@
initWithData_error :: (IsAVAudioPlayer avAudioPlayer, IsNSData data_, IsNSError outError) => avAudioPlayer -> data_ -> outError -> IO (Id AVAudioPlayer)
initWithData_error avAudioPlayer  data_ outError =
withObjCPtr data_ $ \raw_data_ ->
  withObjCPtr outError $ \raw_outError ->
      sendMsg avAudioPlayer (mkSelector "initWithData:error:") (retPtr retVoid) [argPtr (castPtr raw_data_ :: Ptr ()), argPtr (castPtr raw_outError :: Ptr ())] >>= ownedObject . castPtr

-- | @- initWithContentsOfURL:fileTypeHint:error:@
initWithContentsOfURL_fileTypeHint_error :: (IsAVAudioPlayer avAudioPlayer, IsNSURL url, IsNSString utiString, IsNSError outError) => avAudioPlayer -> url -> utiString -> outError -> IO (Id AVAudioPlayer)
initWithContentsOfURL_fileTypeHint_error avAudioPlayer  url utiString outError =
withObjCPtr url $ \raw_url ->
  withObjCPtr utiString $ \raw_utiString ->
    withObjCPtr outError $ \raw_outError ->
        sendMsg avAudioPlayer (mkSelector "initWithContentsOfURL:fileTypeHint:error:") (retPtr retVoid) [argPtr (castPtr raw_url :: Ptr ()), argPtr (castPtr raw_utiString :: Ptr ()), argPtr (castPtr raw_outError :: Ptr ())] >>= ownedObject . castPtr

-- | @- initWithData:fileTypeHint:error:@
initWithData_fileTypeHint_error :: (IsAVAudioPlayer avAudioPlayer, IsNSData data_, IsNSString utiString, IsNSError outError) => avAudioPlayer -> data_ -> utiString -> outError -> IO (Id AVAudioPlayer)
initWithData_fileTypeHint_error avAudioPlayer  data_ utiString outError =
withObjCPtr data_ $ \raw_data_ ->
  withObjCPtr utiString $ \raw_utiString ->
    withObjCPtr outError $ \raw_outError ->
        sendMsg avAudioPlayer (mkSelector "initWithData:fileTypeHint:error:") (retPtr retVoid) [argPtr (castPtr raw_data_ :: Ptr ()), argPtr (castPtr raw_utiString :: Ptr ()), argPtr (castPtr raw_outError :: Ptr ())] >>= ownedObject . castPtr

-- | @- prepareToPlay@
prepareToPlay :: IsAVAudioPlayer avAudioPlayer => avAudioPlayer -> IO Bool
prepareToPlay avAudioPlayer  =
  fmap ((/= 0) :: CULong -> Bool) $ sendMsg avAudioPlayer (mkSelector "prepareToPlay") retCULong []

-- | @- play@
play :: IsAVAudioPlayer avAudioPlayer => avAudioPlayer -> IO Bool
play avAudioPlayer  =
  fmap ((/= 0) :: CULong -> Bool) $ sendMsg avAudioPlayer (mkSelector "play") retCULong []

-- | @- playAtTime:@
playAtTime :: IsAVAudioPlayer avAudioPlayer => avAudioPlayer -> CDouble -> IO Bool
playAtTime avAudioPlayer  time =
  fmap ((/= 0) :: CULong -> Bool) $ sendMsg avAudioPlayer (mkSelector "playAtTime:") retCULong [argCDouble (fromIntegral time)]

-- | @- pause@
pause :: IsAVAudioPlayer avAudioPlayer => avAudioPlayer -> IO ()
pause avAudioPlayer  =
  sendMsg avAudioPlayer (mkSelector "pause") retVoid []

-- | @- stop@
stop :: IsAVAudioPlayer avAudioPlayer => avAudioPlayer -> IO ()
stop avAudioPlayer  =
  sendMsg avAudioPlayer (mkSelector "stop") retVoid []

-- | @- setVolume:fadeDuration:@
setVolume_fadeDuration :: IsAVAudioPlayer avAudioPlayer => avAudioPlayer -> CFloat -> CDouble -> IO ()
setVolume_fadeDuration avAudioPlayer  volume duration =
  sendMsg avAudioPlayer (mkSelector "setVolume:fadeDuration:") retVoid [argCFloat (fromIntegral volume), argCDouble (fromIntegral duration)]

-- | @- updateMeters@
updateMeters :: IsAVAudioPlayer avAudioPlayer => avAudioPlayer -> IO ()
updateMeters avAudioPlayer  =
  sendMsg avAudioPlayer (mkSelector "updateMeters") retVoid []

-- | @- peakPowerForChannel:@
peakPowerForChannel :: IsAVAudioPlayer avAudioPlayer => avAudioPlayer -> CULong -> IO CFloat
peakPowerForChannel avAudioPlayer  channelNumber =
  sendMsg avAudioPlayer (mkSelector "peakPowerForChannel:") retCFloat [argCULong (fromIntegral channelNumber)]

-- | @- averagePowerForChannel:@
averagePowerForChannel :: IsAVAudioPlayer avAudioPlayer => avAudioPlayer -> CULong -> IO CFloat
averagePowerForChannel avAudioPlayer  channelNumber =
  sendMsg avAudioPlayer (mkSelector "averagePowerForChannel:") retCFloat [argCULong (fromIntegral channelNumber)]

-- | @- playing@
playing :: IsAVAudioPlayer avAudioPlayer => avAudioPlayer -> IO Bool
playing avAudioPlayer  =
  fmap ((/= 0) :: CULong -> Bool) $ sendMsg avAudioPlayer (mkSelector "playing") retCULong []

-- | @- numberOfChannels@
numberOfChannels :: IsAVAudioPlayer avAudioPlayer => avAudioPlayer -> IO CULong
numberOfChannels avAudioPlayer  =
  sendMsg avAudioPlayer (mkSelector "numberOfChannels") retCULong []

-- | @- duration@
duration :: IsAVAudioPlayer avAudioPlayer => avAudioPlayer -> IO CDouble
duration avAudioPlayer  =
  sendMsg avAudioPlayer (mkSelector "duration") retCDouble []

-- | @- url@
url :: IsAVAudioPlayer avAudioPlayer => avAudioPlayer -> IO (Id NSURL)
url avAudioPlayer  =
  sendMsg avAudioPlayer (mkSelector "url") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- data@
data_ :: IsAVAudioPlayer avAudioPlayer => avAudioPlayer -> IO (Id NSData)
data_ avAudioPlayer  =
  sendMsg avAudioPlayer (mkSelector "data") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- pan@
pan :: IsAVAudioPlayer avAudioPlayer => avAudioPlayer -> IO CFloat
pan avAudioPlayer  =
  sendMsg avAudioPlayer (mkSelector "pan") retCFloat []

-- | @- setPan:@
setPan :: IsAVAudioPlayer avAudioPlayer => avAudioPlayer -> CFloat -> IO ()
setPan avAudioPlayer  value =
  sendMsg avAudioPlayer (mkSelector "setPan:") retVoid [argCFloat (fromIntegral value)]

-- | @- volume@
volume :: IsAVAudioPlayer avAudioPlayer => avAudioPlayer -> IO CFloat
volume avAudioPlayer  =
  sendMsg avAudioPlayer (mkSelector "volume") retCFloat []

-- | @- setVolume:@
setVolume :: IsAVAudioPlayer avAudioPlayer => avAudioPlayer -> CFloat -> IO ()
setVolume avAudioPlayer  value =
  sendMsg avAudioPlayer (mkSelector "setVolume:") retVoid [argCFloat (fromIntegral value)]

-- | @- enableRate@
enableRate :: IsAVAudioPlayer avAudioPlayer => avAudioPlayer -> IO Bool
enableRate avAudioPlayer  =
  fmap ((/= 0) :: CULong -> Bool) $ sendMsg avAudioPlayer (mkSelector "enableRate") retCULong []

-- | @- setEnableRate:@
setEnableRate :: IsAVAudioPlayer avAudioPlayer => avAudioPlayer -> Bool -> IO ()
setEnableRate avAudioPlayer  value =
  sendMsg avAudioPlayer (mkSelector "setEnableRate:") retVoid [argCULong (if value then 1 else 0)]

-- | @- rate@
rate :: IsAVAudioPlayer avAudioPlayer => avAudioPlayer -> IO CFloat
rate avAudioPlayer  =
  sendMsg avAudioPlayer (mkSelector "rate") retCFloat []

-- | @- setRate:@
setRate :: IsAVAudioPlayer avAudioPlayer => avAudioPlayer -> CFloat -> IO ()
setRate avAudioPlayer  value =
  sendMsg avAudioPlayer (mkSelector "setRate:") retVoid [argCFloat (fromIntegral value)]

-- | @- currentTime@
currentTime :: IsAVAudioPlayer avAudioPlayer => avAudioPlayer -> IO CDouble
currentTime avAudioPlayer  =
  sendMsg avAudioPlayer (mkSelector "currentTime") retCDouble []

-- | @- setCurrentTime:@
setCurrentTime :: IsAVAudioPlayer avAudioPlayer => avAudioPlayer -> CDouble -> IO ()
setCurrentTime avAudioPlayer  value =
  sendMsg avAudioPlayer (mkSelector "setCurrentTime:") retVoid [argCDouble (fromIntegral value)]

-- | @- deviceCurrentTime@
deviceCurrentTime :: IsAVAudioPlayer avAudioPlayer => avAudioPlayer -> IO CDouble
deviceCurrentTime avAudioPlayer  =
  sendMsg avAudioPlayer (mkSelector "deviceCurrentTime") retCDouble []

-- | @- numberOfLoops@
numberOfLoops :: IsAVAudioPlayer avAudioPlayer => avAudioPlayer -> IO CLong
numberOfLoops avAudioPlayer  =
  sendMsg avAudioPlayer (mkSelector "numberOfLoops") retCLong []

-- | @- setNumberOfLoops:@
setNumberOfLoops :: IsAVAudioPlayer avAudioPlayer => avAudioPlayer -> CLong -> IO ()
setNumberOfLoops avAudioPlayer  value =
  sendMsg avAudioPlayer (mkSelector "setNumberOfLoops:") retVoid [argCLong (fromIntegral value)]

-- | @- settings@
settings :: IsAVAudioPlayer avAudioPlayer => avAudioPlayer -> IO (Id NSDictionary)
settings avAudioPlayer  =
  sendMsg avAudioPlayer (mkSelector "settings") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- format@
format :: IsAVAudioPlayer avAudioPlayer => avAudioPlayer -> IO (Id AVAudioFormat)
format avAudioPlayer  =
  sendMsg avAudioPlayer (mkSelector "format") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- meteringEnabled@
meteringEnabled :: IsAVAudioPlayer avAudioPlayer => avAudioPlayer -> IO Bool
meteringEnabled avAudioPlayer  =
  fmap ((/= 0) :: CULong -> Bool) $ sendMsg avAudioPlayer (mkSelector "meteringEnabled") retCULong []

-- | @- setMeteringEnabled:@
setMeteringEnabled :: IsAVAudioPlayer avAudioPlayer => avAudioPlayer -> Bool -> IO ()
setMeteringEnabled avAudioPlayer  value =
  sendMsg avAudioPlayer (mkSelector "setMeteringEnabled:") retVoid [argCULong (if value then 1 else 0)]

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @initWithContentsOfURL:error:@
initWithContentsOfURL_errorSelector :: Selector
initWithContentsOfURL_errorSelector = mkSelector "initWithContentsOfURL:error:"

-- | @Selector@ for @initWithData:error:@
initWithData_errorSelector :: Selector
initWithData_errorSelector = mkSelector "initWithData:error:"

-- | @Selector@ for @initWithContentsOfURL:fileTypeHint:error:@
initWithContentsOfURL_fileTypeHint_errorSelector :: Selector
initWithContentsOfURL_fileTypeHint_errorSelector = mkSelector "initWithContentsOfURL:fileTypeHint:error:"

-- | @Selector@ for @initWithData:fileTypeHint:error:@
initWithData_fileTypeHint_errorSelector :: Selector
initWithData_fileTypeHint_errorSelector = mkSelector "initWithData:fileTypeHint:error:"

-- | @Selector@ for @prepareToPlay@
prepareToPlaySelector :: Selector
prepareToPlaySelector = mkSelector "prepareToPlay"

-- | @Selector@ for @play@
playSelector :: Selector
playSelector = mkSelector "play"

-- | @Selector@ for @playAtTime:@
playAtTimeSelector :: Selector
playAtTimeSelector = mkSelector "playAtTime:"

-- | @Selector@ for @pause@
pauseSelector :: Selector
pauseSelector = mkSelector "pause"

-- | @Selector@ for @stop@
stopSelector :: Selector
stopSelector = mkSelector "stop"

-- | @Selector@ for @setVolume:fadeDuration:@
setVolume_fadeDurationSelector :: Selector
setVolume_fadeDurationSelector = mkSelector "setVolume:fadeDuration:"

-- | @Selector@ for @updateMeters@
updateMetersSelector :: Selector
updateMetersSelector = mkSelector "updateMeters"

-- | @Selector@ for @peakPowerForChannel:@
peakPowerForChannelSelector :: Selector
peakPowerForChannelSelector = mkSelector "peakPowerForChannel:"

-- | @Selector@ for @averagePowerForChannel:@
averagePowerForChannelSelector :: Selector
averagePowerForChannelSelector = mkSelector "averagePowerForChannel:"

-- | @Selector@ for @playing@
playingSelector :: Selector
playingSelector = mkSelector "playing"

-- | @Selector@ for @numberOfChannels@
numberOfChannelsSelector :: Selector
numberOfChannelsSelector = mkSelector "numberOfChannels"

-- | @Selector@ for @duration@
durationSelector :: Selector
durationSelector = mkSelector "duration"

-- | @Selector@ for @url@
urlSelector :: Selector
urlSelector = mkSelector "url"

-- | @Selector@ for @data@
dataSelector :: Selector
dataSelector = mkSelector "data"

-- | @Selector@ for @pan@
panSelector :: Selector
panSelector = mkSelector "pan"

-- | @Selector@ for @setPan:@
setPanSelector :: Selector
setPanSelector = mkSelector "setPan:"

-- | @Selector@ for @volume@
volumeSelector :: Selector
volumeSelector = mkSelector "volume"

-- | @Selector@ for @setVolume:@
setVolumeSelector :: Selector
setVolumeSelector = mkSelector "setVolume:"

-- | @Selector@ for @enableRate@
enableRateSelector :: Selector
enableRateSelector = mkSelector "enableRate"

-- | @Selector@ for @setEnableRate:@
setEnableRateSelector :: Selector
setEnableRateSelector = mkSelector "setEnableRate:"

-- | @Selector@ for @rate@
rateSelector :: Selector
rateSelector = mkSelector "rate"

-- | @Selector@ for @setRate:@
setRateSelector :: Selector
setRateSelector = mkSelector "setRate:"

-- | @Selector@ for @currentTime@
currentTimeSelector :: Selector
currentTimeSelector = mkSelector "currentTime"

-- | @Selector@ for @setCurrentTime:@
setCurrentTimeSelector :: Selector
setCurrentTimeSelector = mkSelector "setCurrentTime:"

-- | @Selector@ for @deviceCurrentTime@
deviceCurrentTimeSelector :: Selector
deviceCurrentTimeSelector = mkSelector "deviceCurrentTime"

-- | @Selector@ for @numberOfLoops@
numberOfLoopsSelector :: Selector
numberOfLoopsSelector = mkSelector "numberOfLoops"

-- | @Selector@ for @setNumberOfLoops:@
setNumberOfLoopsSelector :: Selector
setNumberOfLoopsSelector = mkSelector "setNumberOfLoops:"

-- | @Selector@ for @settings@
settingsSelector :: Selector
settingsSelector = mkSelector "settings"

-- | @Selector@ for @format@
formatSelector :: Selector
formatSelector = mkSelector "format"

-- | @Selector@ for @meteringEnabled@
meteringEnabledSelector :: Selector
meteringEnabledSelector = mkSelector "meteringEnabled"

-- | @Selector@ for @setMeteringEnabled:@
setMeteringEnabledSelector :: Selector
setMeteringEnabledSelector = mkSelector "setMeteringEnabled:"

