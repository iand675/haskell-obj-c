{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | AVAudioUnitDelay
--
-- an AVAudioUnitEffect that implements a delay effect
--
-- A delay unit delays the input signal by the specified time interval        and then blends it with the input signal. The amount of high frequency        roll-off can also be controlled in order to simulate the effect of        a tape delay.
--
-- Generated bindings for @AVAudioUnitDelay@.
module ObjC.AVFAudio.AVAudioUnitDelay
  ( AVAudioUnitDelay
  , IsAVAudioUnitDelay(..)
  , delayTime
  , setDelayTime
  , feedback
  , setFeedback
  , lowPassCutoff
  , setLowPassCutoff
  , wetDryMix
  , setWetDryMix
  , delayTimeSelector
  , feedbackSelector
  , lowPassCutoffSelector
  , setDelayTimeSelector
  , setFeedbackSelector
  , setLowPassCutoffSelector
  , setWetDryMixSelector
  , wetDryMixSelector


  ) where

import Foreign.Ptr (Ptr, FunPtr)
import Foreign.C.Types

import ObjC.Runtime.Types
import ObjC.Runtime.Message (sendMessage, sendOwnedMessage, sendClassMessage, sendOwnedClassMessage)
import ObjC.Runtime.Selector (mkSelector)
import ObjC.Runtime.Class (getRequiredClass)

import ObjC.AVFAudio.Internal.Classes
import ObjC.Foundation.Internal.Classes

-- | delayTime
--
-- Time taken by the delayed input signal to reach the output
--
-- Range:      0 -> 2    Default:    1    Unit:       Seconds
--
-- ObjC selector: @- delayTime@
delayTime :: IsAVAudioUnitDelay avAudioUnitDelay => avAudioUnitDelay -> IO CDouble
delayTime avAudioUnitDelay =
  sendMessage avAudioUnitDelay delayTimeSelector

-- | delayTime
--
-- Time taken by the delayed input signal to reach the output
--
-- Range:      0 -> 2    Default:    1    Unit:       Seconds
--
-- ObjC selector: @- setDelayTime:@
setDelayTime :: IsAVAudioUnitDelay avAudioUnitDelay => avAudioUnitDelay -> CDouble -> IO ()
setDelayTime avAudioUnitDelay value =
  sendMessage avAudioUnitDelay setDelayTimeSelector value

-- | feedback
--
-- Amount of the output signal fed back into the delay line    Range:      -100 -> 100    Default:    50    Unit:       Percent
--
-- ObjC selector: @- feedback@
feedback :: IsAVAudioUnitDelay avAudioUnitDelay => avAudioUnitDelay -> IO CFloat
feedback avAudioUnitDelay =
  sendMessage avAudioUnitDelay feedbackSelector

-- | feedback
--
-- Amount of the output signal fed back into the delay line    Range:      -100 -> 100    Default:    50    Unit:       Percent
--
-- ObjC selector: @- setFeedback:@
setFeedback :: IsAVAudioUnitDelay avAudioUnitDelay => avAudioUnitDelay -> CFloat -> IO ()
setFeedback avAudioUnitDelay value =
  sendMessage avAudioUnitDelay setFeedbackSelector value

-- | lowPassCutoff
--
-- Cutoff frequency above which high frequency content is rolled off    Range:      10 -> (samplerate/2)    Default:    15000    Unit:       Hertz
--
-- ObjC selector: @- lowPassCutoff@
lowPassCutoff :: IsAVAudioUnitDelay avAudioUnitDelay => avAudioUnitDelay -> IO CFloat
lowPassCutoff avAudioUnitDelay =
  sendMessage avAudioUnitDelay lowPassCutoffSelector

-- | lowPassCutoff
--
-- Cutoff frequency above which high frequency content is rolled off    Range:      10 -> (samplerate/2)    Default:    15000    Unit:       Hertz
--
-- ObjC selector: @- setLowPassCutoff:@
setLowPassCutoff :: IsAVAudioUnitDelay avAudioUnitDelay => avAudioUnitDelay -> CFloat -> IO ()
setLowPassCutoff avAudioUnitDelay value =
  sendMessage avAudioUnitDelay setLowPassCutoffSelector value

-- | wetDryMix
--
-- Blend of the wet and dry signals    Range:      0 (all dry) -> 100 (all wet)    Default:    100    Unit:       Percent
--
-- ObjC selector: @- wetDryMix@
wetDryMix :: IsAVAudioUnitDelay avAudioUnitDelay => avAudioUnitDelay -> IO CFloat
wetDryMix avAudioUnitDelay =
  sendMessage avAudioUnitDelay wetDryMixSelector

-- | wetDryMix
--
-- Blend of the wet and dry signals    Range:      0 (all dry) -> 100 (all wet)    Default:    100    Unit:       Percent
--
-- ObjC selector: @- setWetDryMix:@
setWetDryMix :: IsAVAudioUnitDelay avAudioUnitDelay => avAudioUnitDelay -> CFloat -> IO ()
setWetDryMix avAudioUnitDelay value =
  sendMessage avAudioUnitDelay setWetDryMixSelector value

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @delayTime@
delayTimeSelector :: Selector '[] CDouble
delayTimeSelector = mkSelector "delayTime"

-- | @Selector@ for @setDelayTime:@
setDelayTimeSelector :: Selector '[CDouble] ()
setDelayTimeSelector = mkSelector "setDelayTime:"

-- | @Selector@ for @feedback@
feedbackSelector :: Selector '[] CFloat
feedbackSelector = mkSelector "feedback"

-- | @Selector@ for @setFeedback:@
setFeedbackSelector :: Selector '[CFloat] ()
setFeedbackSelector = mkSelector "setFeedback:"

-- | @Selector@ for @lowPassCutoff@
lowPassCutoffSelector :: Selector '[] CFloat
lowPassCutoffSelector = mkSelector "lowPassCutoff"

-- | @Selector@ for @setLowPassCutoff:@
setLowPassCutoffSelector :: Selector '[CFloat] ()
setLowPassCutoffSelector = mkSelector "setLowPassCutoff:"

-- | @Selector@ for @wetDryMix@
wetDryMixSelector :: Selector '[] CFloat
wetDryMixSelector = mkSelector "wetDryMix"

-- | @Selector@ for @setWetDryMix:@
setWetDryMixSelector :: Selector '[CFloat] ()
setWetDryMixSelector = mkSelector "setWetDryMix:"

