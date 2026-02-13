{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | AVAudioUnitTimePitch
--
-- an AVAudioUnitTimeEffect that provides good quality time stretching and pitch shifting
--
-- In this time effect, the playback rate and pitch parameters function independently of each other
--
-- Generated bindings for @AVAudioUnitTimePitch@.
module ObjC.AVFAudio.AVAudioUnitTimePitch
  ( AVAudioUnitTimePitch
  , IsAVAudioUnitTimePitch(..)
  , rate
  , setRate
  , pitch
  , setPitch
  , overlap
  , setOverlap
  , overlapSelector
  , pitchSelector
  , rateSelector
  , setOverlapSelector
  , setPitchSelector
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
-- playback rate of the input signal
--
-- Range:      1/32 -> 32.0    Default:    1.0    Unit:       Generic
--
-- ObjC selector: @- rate@
rate :: IsAVAudioUnitTimePitch avAudioUnitTimePitch => avAudioUnitTimePitch -> IO CFloat
rate avAudioUnitTimePitch =
  sendMessage avAudioUnitTimePitch rateSelector

-- | rate
--
-- playback rate of the input signal
--
-- Range:      1/32 -> 32.0    Default:    1.0    Unit:       Generic
--
-- ObjC selector: @- setRate:@
setRate :: IsAVAudioUnitTimePitch avAudioUnitTimePitch => avAudioUnitTimePitch -> CFloat -> IO ()
setRate avAudioUnitTimePitch value =
  sendMessage avAudioUnitTimePitch setRateSelector value

-- | pitch
--
-- amount by which the input signal is pitch shifted
--
-- 1 octave  = 1200 cents    1 musical semitone  = 100 cents
--
-- Range:      -2400 -> 2400    Default:    0.0    Unit:       Cents
--
-- ObjC selector: @- pitch@
pitch :: IsAVAudioUnitTimePitch avAudioUnitTimePitch => avAudioUnitTimePitch -> IO CFloat
pitch avAudioUnitTimePitch =
  sendMessage avAudioUnitTimePitch pitchSelector

-- | pitch
--
-- amount by which the input signal is pitch shifted
--
-- 1 octave  = 1200 cents    1 musical semitone  = 100 cents
--
-- Range:      -2400 -> 2400    Default:    0.0    Unit:       Cents
--
-- ObjC selector: @- setPitch:@
setPitch :: IsAVAudioUnitTimePitch avAudioUnitTimePitch => avAudioUnitTimePitch -> CFloat -> IO ()
setPitch avAudioUnitTimePitch value =
  sendMessage avAudioUnitTimePitch setPitchSelector value

-- | overlap
--
-- amount of overlap between segments of the input audio signal
--
-- A higher value results in fewer artifacts in the output signal.    This parameter also impacts the amount of CPU used.
--
-- Range:      3.0 -> 32.0    Default:    8.0    Unit:       Generic
--
-- ObjC selector: @- overlap@
overlap :: IsAVAudioUnitTimePitch avAudioUnitTimePitch => avAudioUnitTimePitch -> IO CFloat
overlap avAudioUnitTimePitch =
  sendMessage avAudioUnitTimePitch overlapSelector

-- | overlap
--
-- amount of overlap between segments of the input audio signal
--
-- A higher value results in fewer artifacts in the output signal.    This parameter also impacts the amount of CPU used.
--
-- Range:      3.0 -> 32.0    Default:    8.0    Unit:       Generic
--
-- ObjC selector: @- setOverlap:@
setOverlap :: IsAVAudioUnitTimePitch avAudioUnitTimePitch => avAudioUnitTimePitch -> CFloat -> IO ()
setOverlap avAudioUnitTimePitch value =
  sendMessage avAudioUnitTimePitch setOverlapSelector value

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @rate@
rateSelector :: Selector '[] CFloat
rateSelector = mkSelector "rate"

-- | @Selector@ for @setRate:@
setRateSelector :: Selector '[CFloat] ()
setRateSelector = mkSelector "setRate:"

-- | @Selector@ for @pitch@
pitchSelector :: Selector '[] CFloat
pitchSelector = mkSelector "pitch"

-- | @Selector@ for @setPitch:@
setPitchSelector :: Selector '[CFloat] ()
setPitchSelector = mkSelector "setPitch:"

-- | @Selector@ for @overlap@
overlapSelector :: Selector '[] CFloat
overlapSelector = mkSelector "overlap"

-- | @Selector@ for @setOverlap:@
setOverlapSelector :: Selector '[CFloat] ()
setOverlapSelector = mkSelector "setOverlap:"

