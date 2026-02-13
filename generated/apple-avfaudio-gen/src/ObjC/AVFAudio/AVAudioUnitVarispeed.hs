{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | AVAudioUnitVarispeed
--
-- an AVAudioUnitTimeEffect that can be used to control the playback rate
--
-- Generated bindings for @AVAudioUnitVarispeed@.
module ObjC.AVFAudio.AVAudioUnitVarispeed
  ( AVAudioUnitVarispeed
  , IsAVAudioUnitVarispeed(..)
  , rate
  , setRate
  , rateSelector
  , setRateSelector


  ) where

import Foreign.Ptr (Ptr, FunPtr)
import Foreign.C.Types

import ObjC.Runtime.Types
import ObjC.Runtime.Message (sendMessage, sendOwnedMessage, sendClassMessage, sendOwnedClassMessage)
import ObjC.Runtime.Selector (mkSelector)
import ObjC.Runtime.Class (getRequiredClass)

import ObjC.AVFAudio.Internal.Classes
import ObjC.Foundation.Internal.Classes

-- | rate
--
-- controls the playback rate of the audio signal
--
-- Since this unit resamples the input signal, changing the playback rate also changes the pitch.
--
-- i.e. changing the rate to 2.0 results in the output audio playing one octave higher.    Similarly changing the rate to 0.5, results in the output audio playing one octave lower.
--
-- The playback rate and pitch can be calculated as                  rate  = pow(2, cents/1200.0)        pitch in cents  = 1200.0 * log2(rate)
--
-- Where,    1 octave  = 1200 cents    1 musical semitone  = 100 cents
--
-- Range:      0.25 -> 4.0    Default:    1.0    Unit:       Generic
--
-- ObjC selector: @- rate@
rate :: IsAVAudioUnitVarispeed avAudioUnitVarispeed => avAudioUnitVarispeed -> IO CFloat
rate avAudioUnitVarispeed =
  sendMessage avAudioUnitVarispeed rateSelector

-- | rate
--
-- controls the playback rate of the audio signal
--
-- Since this unit resamples the input signal, changing the playback rate also changes the pitch.
--
-- i.e. changing the rate to 2.0 results in the output audio playing one octave higher.    Similarly changing the rate to 0.5, results in the output audio playing one octave lower.
--
-- The playback rate and pitch can be calculated as                  rate  = pow(2, cents/1200.0)        pitch in cents  = 1200.0 * log2(rate)
--
-- Where,    1 octave  = 1200 cents    1 musical semitone  = 100 cents
--
-- Range:      0.25 -> 4.0    Default:    1.0    Unit:       Generic
--
-- ObjC selector: @- setRate:@
setRate :: IsAVAudioUnitVarispeed avAudioUnitVarispeed => avAudioUnitVarispeed -> CFloat -> IO ()
setRate avAudioUnitVarispeed value =
  sendMessage avAudioUnitVarispeed setRateSelector value

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @rate@
rateSelector :: Selector '[] CFloat
rateSelector = mkSelector "rate"

-- | @Selector@ for @setRate:@
setRateSelector :: Selector '[CFloat] ()
setRateSelector = mkSelector "setRate:"

