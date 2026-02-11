{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | AVAudioUnitReverb
--
-- an AVAudioUnitEffect that implements a reverb
--
-- A reverb simulates the acoustic characteristics of a particular environment.        Use the different presets to simulate a particular space and blend it in with        the original signal using the wetDryMix parameter.
--
-- Generated bindings for @AVAudioUnitReverb@.
module ObjC.AVFAudio.AVAudioUnitReverb
  ( AVAudioUnitReverb
  , IsAVAudioUnitReverb(..)
  , loadFactoryPreset
  , wetDryMix
  , setWetDryMix
  , loadFactoryPresetSelector
  , wetDryMixSelector
  , setWetDryMixSelector

  -- * Enum types
  , AVAudioUnitReverbPreset(AVAudioUnitReverbPreset)
  , pattern AVAudioUnitReverbPresetSmallRoom
  , pattern AVAudioUnitReverbPresetMediumRoom
  , pattern AVAudioUnitReverbPresetLargeRoom
  , pattern AVAudioUnitReverbPresetMediumHall
  , pattern AVAudioUnitReverbPresetLargeHall
  , pattern AVAudioUnitReverbPresetPlate
  , pattern AVAudioUnitReverbPresetMediumChamber
  , pattern AVAudioUnitReverbPresetLargeChamber
  , pattern AVAudioUnitReverbPresetCathedral
  , pattern AVAudioUnitReverbPresetLargeRoom2
  , pattern AVAudioUnitReverbPresetMediumHall2
  , pattern AVAudioUnitReverbPresetMediumHall3
  , pattern AVAudioUnitReverbPresetLargeHall2

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
import ObjC.AVFAudio.Internal.Enums
import ObjC.Foundation.Internal.Classes

-- | loadFactoryPreset:
--
-- load a reverb preset    Default:    AVAudioUnitReverbPresetMediumHall
--
-- ObjC selector: @- loadFactoryPreset:@
loadFactoryPreset :: IsAVAudioUnitReverb avAudioUnitReverb => avAudioUnitReverb -> AVAudioUnitReverbPreset -> IO ()
loadFactoryPreset avAudioUnitReverb  preset =
  sendMsg avAudioUnitReverb (mkSelector "loadFactoryPreset:") retVoid [argCLong (coerce preset)]

-- | wetDryMix
--
-- Blend of the wet and dry signals    Range:      0 (all dry) -> 100 (all wet)    Unit:       Percent
--
-- ObjC selector: @- wetDryMix@
wetDryMix :: IsAVAudioUnitReverb avAudioUnitReverb => avAudioUnitReverb -> IO CFloat
wetDryMix avAudioUnitReverb  =
  sendMsg avAudioUnitReverb (mkSelector "wetDryMix") retCFloat []

-- | wetDryMix
--
-- Blend of the wet and dry signals    Range:      0 (all dry) -> 100 (all wet)    Unit:       Percent
--
-- ObjC selector: @- setWetDryMix:@
setWetDryMix :: IsAVAudioUnitReverb avAudioUnitReverb => avAudioUnitReverb -> CFloat -> IO ()
setWetDryMix avAudioUnitReverb  value =
  sendMsg avAudioUnitReverb (mkSelector "setWetDryMix:") retVoid [argCFloat (fromIntegral value)]

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @loadFactoryPreset:@
loadFactoryPresetSelector :: Selector
loadFactoryPresetSelector = mkSelector "loadFactoryPreset:"

-- | @Selector@ for @wetDryMix@
wetDryMixSelector :: Selector
wetDryMixSelector = mkSelector "wetDryMix"

-- | @Selector@ for @setWetDryMix:@
setWetDryMixSelector :: Selector
setWetDryMixSelector = mkSelector "setWetDryMix:"

