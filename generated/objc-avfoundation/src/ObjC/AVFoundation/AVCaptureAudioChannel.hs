{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | AVCaptureAudioChannel
--
-- AVCaptureAudioChannel represents a single channel of audio flowing through an AVCaptureSession.
--
-- An AVCaptureConnection from an input producing audio to an output receiving audio exposes an array of AVCaptureAudioChannel objects, one for each channel of audio available. Iterating through these audio channel objects, a client may poll for audio levels. Instances of AVCaptureAudioChannel cannot be created directly.
--
-- Generated bindings for @AVCaptureAudioChannel@.
module ObjC.AVFoundation.AVCaptureAudioChannel
  ( AVCaptureAudioChannel
  , IsAVCaptureAudioChannel(..)
  , init_
  , new
  , averagePowerLevel
  , peakHoldLevel
  , volume
  , setVolume
  , enabled
  , setEnabled
  , initSelector
  , newSelector
  , averagePowerLevelSelector
  , peakHoldLevelSelector
  , volumeSelector
  , setVolumeSelector
  , enabledSelector
  , setEnabledSelector


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
import ObjC.Foundation.Internal.Classes

-- | @- init@
init_ :: IsAVCaptureAudioChannel avCaptureAudioChannel => avCaptureAudioChannel -> IO (Id AVCaptureAudioChannel)
init_ avCaptureAudioChannel  =
  sendMsg avCaptureAudioChannel (mkSelector "init") (retPtr retVoid) [] >>= ownedObject . castPtr

-- | @+ new@
new :: IO (Id AVCaptureAudioChannel)
new  =
  do
    cls' <- getRequiredClass "AVCaptureAudioChannel"
    sendClassMsg cls' (mkSelector "new") (retPtr retVoid) [] >>= ownedObject . castPtr

-- | averagePowerLevel
--
-- A measurement of the instantaneous average power level of the audio flowing through the receiver.
--
-- A client may poll an AVCaptureAudioChannel object for its current averagePowerLevel to get its instantaneous average power level in decibels. This property is not key-value observable.
--
-- ObjC selector: @- averagePowerLevel@
averagePowerLevel :: IsAVCaptureAudioChannel avCaptureAudioChannel => avCaptureAudioChannel -> IO CFloat
averagePowerLevel avCaptureAudioChannel  =
  sendMsg avCaptureAudioChannel (mkSelector "averagePowerLevel") retCFloat []

-- | peakHoldLevel
--
-- A measurement of the peak/hold level of the audio flowing through the receiver.
--
-- A client may poll an AVCaptureAudioChannel object for its current peakHoldLevel to get its most recent peak hold level in decibels. This property is not key-value observable.
--
-- ObjC selector: @- peakHoldLevel@
peakHoldLevel :: IsAVCaptureAudioChannel avCaptureAudioChannel => avCaptureAudioChannel -> IO CFloat
peakHoldLevel avCaptureAudioChannel  =
  sendMsg avCaptureAudioChannel (mkSelector "peakHoldLevel") retCFloat []

-- | volume
--
-- A property indicating the current volume (gain) of the receiver.
--
-- The volume property indicates the current volume or gain of the receiver as a floating point value between 0.0 -> 1.0. If you desire to boost the gain in software, you may specify a a value greater than 1.0.
--
-- ObjC selector: @- volume@
volume :: IsAVCaptureAudioChannel avCaptureAudioChannel => avCaptureAudioChannel -> IO CFloat
volume avCaptureAudioChannel  =
  sendMsg avCaptureAudioChannel (mkSelector "volume") retCFloat []

-- | volume
--
-- A property indicating the current volume (gain) of the receiver.
--
-- The volume property indicates the current volume or gain of the receiver as a floating point value between 0.0 -> 1.0. If you desire to boost the gain in software, you may specify a a value greater than 1.0.
--
-- ObjC selector: @- setVolume:@
setVolume :: IsAVCaptureAudioChannel avCaptureAudioChannel => avCaptureAudioChannel -> CFloat -> IO ()
setVolume avCaptureAudioChannel  value =
  sendMsg avCaptureAudioChannel (mkSelector "setVolume:") retVoid [argCFloat (fromIntegral value)]

-- | enabled
--
-- A property indicating whether the receiver is currently enabled for data capture.
--
-- By default, all AVCaptureAudioChannel objects exposed by a connection are enabled. You may set enabled to NO to stop the flow of data for a particular AVCaptureAudioChannel.
--
-- ObjC selector: @- enabled@
enabled :: IsAVCaptureAudioChannel avCaptureAudioChannel => avCaptureAudioChannel -> IO Bool
enabled avCaptureAudioChannel  =
  fmap ((/= 0) :: CULong -> Bool) $ sendMsg avCaptureAudioChannel (mkSelector "enabled") retCULong []

-- | enabled
--
-- A property indicating whether the receiver is currently enabled for data capture.
--
-- By default, all AVCaptureAudioChannel objects exposed by a connection are enabled. You may set enabled to NO to stop the flow of data for a particular AVCaptureAudioChannel.
--
-- ObjC selector: @- setEnabled:@
setEnabled :: IsAVCaptureAudioChannel avCaptureAudioChannel => avCaptureAudioChannel -> Bool -> IO ()
setEnabled avCaptureAudioChannel  value =
  sendMsg avCaptureAudioChannel (mkSelector "setEnabled:") retVoid [argCULong (if value then 1 else 0)]

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @init@
initSelector :: Selector
initSelector = mkSelector "init"

-- | @Selector@ for @new@
newSelector :: Selector
newSelector = mkSelector "new"

-- | @Selector@ for @averagePowerLevel@
averagePowerLevelSelector :: Selector
averagePowerLevelSelector = mkSelector "averagePowerLevel"

-- | @Selector@ for @peakHoldLevel@
peakHoldLevelSelector :: Selector
peakHoldLevelSelector = mkSelector "peakHoldLevel"

-- | @Selector@ for @volume@
volumeSelector :: Selector
volumeSelector = mkSelector "volume"

-- | @Selector@ for @setVolume:@
setVolumeSelector :: Selector
setVolumeSelector = mkSelector "setVolume:"

-- | @Selector@ for @enabled@
enabledSelector :: Selector
enabledSelector = mkSelector "enabled"

-- | @Selector@ for @setEnabled:@
setEnabledSelector :: Selector
setEnabledSelector = mkSelector "setEnabled:"

