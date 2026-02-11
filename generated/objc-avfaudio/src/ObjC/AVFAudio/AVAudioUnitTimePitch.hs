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
  , rateSelector
  , setRateSelector
  , pitchSelector
  , setPitchSelector
  , overlapSelector
  , setOverlapSelector


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

-- | rate
--
-- playback rate of the input signal
--
-- Range:      1/32 -> 32.0    Default:    1.0    Unit:       Generic
--
-- ObjC selector: @- rate@
rate :: IsAVAudioUnitTimePitch avAudioUnitTimePitch => avAudioUnitTimePitch -> IO CFloat
rate avAudioUnitTimePitch  =
  sendMsg avAudioUnitTimePitch (mkSelector "rate") retCFloat []

-- | rate
--
-- playback rate of the input signal
--
-- Range:      1/32 -> 32.0    Default:    1.0    Unit:       Generic
--
-- ObjC selector: @- setRate:@
setRate :: IsAVAudioUnitTimePitch avAudioUnitTimePitch => avAudioUnitTimePitch -> CFloat -> IO ()
setRate avAudioUnitTimePitch  value =
  sendMsg avAudioUnitTimePitch (mkSelector "setRate:") retVoid [argCFloat (fromIntegral value)]

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
pitch avAudioUnitTimePitch  =
  sendMsg avAudioUnitTimePitch (mkSelector "pitch") retCFloat []

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
setPitch avAudioUnitTimePitch  value =
  sendMsg avAudioUnitTimePitch (mkSelector "setPitch:") retVoid [argCFloat (fromIntegral value)]

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
overlap avAudioUnitTimePitch  =
  sendMsg avAudioUnitTimePitch (mkSelector "overlap") retCFloat []

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
setOverlap avAudioUnitTimePitch  value =
  sendMsg avAudioUnitTimePitch (mkSelector "setOverlap:") retVoid [argCFloat (fromIntegral value)]

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @rate@
rateSelector :: Selector
rateSelector = mkSelector "rate"

-- | @Selector@ for @setRate:@
setRateSelector :: Selector
setRateSelector = mkSelector "setRate:"

-- | @Selector@ for @pitch@
pitchSelector :: Selector
pitchSelector = mkSelector "pitch"

-- | @Selector@ for @setPitch:@
setPitchSelector :: Selector
setPitchSelector = mkSelector "setPitch:"

-- | @Selector@ for @overlap@
overlapSelector :: Selector
overlapSelector = mkSelector "overlap"

-- | @Selector@ for @setOverlap:@
setOverlapSelector :: Selector
setOverlapSelector = mkSelector "setOverlap:"

