{-# LANGUAGE DataKinds #-}
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
  , averagePowerLevelSelector
  , enabledSelector
  , initSelector
  , newSelector
  , peakHoldLevelSelector
  , setEnabledSelector
  , setVolumeSelector
  , volumeSelector


  ) where

import Foreign.Ptr (Ptr, FunPtr)
import Foreign.C.Types

import ObjC.Runtime.Types
import ObjC.Runtime.Message (sendMessage, sendOwnedMessage, sendClassMessage, sendOwnedClassMessage)
import ObjC.Runtime.Selector (mkSelector)
import ObjC.Runtime.Class (getRequiredClass)

import ObjC.AVFoundation.Internal.Classes
import ObjC.Foundation.Internal.Classes

-- | @- init@
init_ :: IsAVCaptureAudioChannel avCaptureAudioChannel => avCaptureAudioChannel -> IO (Id AVCaptureAudioChannel)
init_ avCaptureAudioChannel =
  sendOwnedMessage avCaptureAudioChannel initSelector

-- | @+ new@
new :: IO (Id AVCaptureAudioChannel)
new  =
  do
    cls' <- getRequiredClass "AVCaptureAudioChannel"
    sendOwnedClassMessage cls' newSelector

-- | averagePowerLevel
--
-- A measurement of the instantaneous average power level of the audio flowing through the receiver.
--
-- A client may poll an AVCaptureAudioChannel object for its current averagePowerLevel to get its instantaneous average power level in decibels. This property is not key-value observable.
--
-- ObjC selector: @- averagePowerLevel@
averagePowerLevel :: IsAVCaptureAudioChannel avCaptureAudioChannel => avCaptureAudioChannel -> IO CFloat
averagePowerLevel avCaptureAudioChannel =
  sendMessage avCaptureAudioChannel averagePowerLevelSelector

-- | peakHoldLevel
--
-- A measurement of the peak/hold level of the audio flowing through the receiver.
--
-- A client may poll an AVCaptureAudioChannel object for its current peakHoldLevel to get its most recent peak hold level in decibels. This property is not key-value observable.
--
-- ObjC selector: @- peakHoldLevel@
peakHoldLevel :: IsAVCaptureAudioChannel avCaptureAudioChannel => avCaptureAudioChannel -> IO CFloat
peakHoldLevel avCaptureAudioChannel =
  sendMessage avCaptureAudioChannel peakHoldLevelSelector

-- | volume
--
-- A property indicating the current volume (gain) of the receiver.
--
-- The volume property indicates the current volume or gain of the receiver as a floating point value between 0.0 -> 1.0. If you desire to boost the gain in software, you may specify a a value greater than 1.0.
--
-- ObjC selector: @- volume@
volume :: IsAVCaptureAudioChannel avCaptureAudioChannel => avCaptureAudioChannel -> IO CFloat
volume avCaptureAudioChannel =
  sendMessage avCaptureAudioChannel volumeSelector

-- | volume
--
-- A property indicating the current volume (gain) of the receiver.
--
-- The volume property indicates the current volume or gain of the receiver as a floating point value between 0.0 -> 1.0. If you desire to boost the gain in software, you may specify a a value greater than 1.0.
--
-- ObjC selector: @- setVolume:@
setVolume :: IsAVCaptureAudioChannel avCaptureAudioChannel => avCaptureAudioChannel -> CFloat -> IO ()
setVolume avCaptureAudioChannel value =
  sendMessage avCaptureAudioChannel setVolumeSelector value

-- | enabled
--
-- A property indicating whether the receiver is currently enabled for data capture.
--
-- By default, all AVCaptureAudioChannel objects exposed by a connection are enabled. You may set enabled to NO to stop the flow of data for a particular AVCaptureAudioChannel.
--
-- ObjC selector: @- enabled@
enabled :: IsAVCaptureAudioChannel avCaptureAudioChannel => avCaptureAudioChannel -> IO Bool
enabled avCaptureAudioChannel =
  sendMessage avCaptureAudioChannel enabledSelector

-- | enabled
--
-- A property indicating whether the receiver is currently enabled for data capture.
--
-- By default, all AVCaptureAudioChannel objects exposed by a connection are enabled. You may set enabled to NO to stop the flow of data for a particular AVCaptureAudioChannel.
--
-- ObjC selector: @- setEnabled:@
setEnabled :: IsAVCaptureAudioChannel avCaptureAudioChannel => avCaptureAudioChannel -> Bool -> IO ()
setEnabled avCaptureAudioChannel value =
  sendMessage avCaptureAudioChannel setEnabledSelector value

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @init@
initSelector :: Selector '[] (Id AVCaptureAudioChannel)
initSelector = mkSelector "init"

-- | @Selector@ for @new@
newSelector :: Selector '[] (Id AVCaptureAudioChannel)
newSelector = mkSelector "new"

-- | @Selector@ for @averagePowerLevel@
averagePowerLevelSelector :: Selector '[] CFloat
averagePowerLevelSelector = mkSelector "averagePowerLevel"

-- | @Selector@ for @peakHoldLevel@
peakHoldLevelSelector :: Selector '[] CFloat
peakHoldLevelSelector = mkSelector "peakHoldLevel"

-- | @Selector@ for @volume@
volumeSelector :: Selector '[] CFloat
volumeSelector = mkSelector "volume"

-- | @Selector@ for @setVolume:@
setVolumeSelector :: Selector '[CFloat] ()
setVolumeSelector = mkSelector "setVolume:"

-- | @Selector@ for @enabled@
enabledSelector :: Selector '[] Bool
enabledSelector = mkSelector "enabled"

-- | @Selector@ for @setEnabled:@
setEnabledSelector :: Selector '[Bool] ()
setEnabledSelector = mkSelector "setEnabled:"

