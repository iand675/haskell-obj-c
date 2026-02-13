{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | AVAudioUnitDistortion
--
-- An AVAudioUnitEffect that implements a multi-stage distortion effect.
--
-- Generated bindings for @AVAudioUnitDistortion@.
module ObjC.AVFAudio.AVAudioUnitDistortion
  ( AVAudioUnitDistortion
  , IsAVAudioUnitDistortion(..)
  , loadFactoryPreset
  , preGain
  , setPreGain
  , wetDryMix
  , setWetDryMix
  , loadFactoryPresetSelector
  , preGainSelector
  , setPreGainSelector
  , setWetDryMixSelector
  , wetDryMixSelector

  -- * Enum types
  , AVAudioUnitDistortionPreset(AVAudioUnitDistortionPreset)
  , pattern AVAudioUnitDistortionPresetDrumsBitBrush
  , pattern AVAudioUnitDistortionPresetDrumsBufferBeats
  , pattern AVAudioUnitDistortionPresetDrumsLoFi
  , pattern AVAudioUnitDistortionPresetMultiBrokenSpeaker
  , pattern AVAudioUnitDistortionPresetMultiCellphoneConcert
  , pattern AVAudioUnitDistortionPresetMultiDecimated1
  , pattern AVAudioUnitDistortionPresetMultiDecimated2
  , pattern AVAudioUnitDistortionPresetMultiDecimated3
  , pattern AVAudioUnitDistortionPresetMultiDecimated4
  , pattern AVAudioUnitDistortionPresetMultiDistortedFunk
  , pattern AVAudioUnitDistortionPresetMultiDistortedCubed
  , pattern AVAudioUnitDistortionPresetMultiDistortedSquared
  , pattern AVAudioUnitDistortionPresetMultiEcho1
  , pattern AVAudioUnitDistortionPresetMultiEcho2
  , pattern AVAudioUnitDistortionPresetMultiEchoTight1
  , pattern AVAudioUnitDistortionPresetMultiEchoTight2
  , pattern AVAudioUnitDistortionPresetMultiEverythingIsBroken
  , pattern AVAudioUnitDistortionPresetSpeechAlienChatter
  , pattern AVAudioUnitDistortionPresetSpeechCosmicInterference
  , pattern AVAudioUnitDistortionPresetSpeechGoldenPi
  , pattern AVAudioUnitDistortionPresetSpeechRadioTower
  , pattern AVAudioUnitDistortionPresetSpeechWaves

  ) where

import Foreign.Ptr (Ptr, FunPtr)
import Foreign.C.Types

import ObjC.Runtime.Types
import ObjC.Runtime.Message (sendMessage, sendOwnedMessage, sendClassMessage, sendOwnedClassMessage)
import ObjC.Runtime.Selector (mkSelector)
import ObjC.Runtime.Class (getRequiredClass)

import ObjC.AVFAudio.Internal.Classes
import ObjC.AVFAudio.Internal.Enums
import ObjC.Foundation.Internal.Classes

-- | loadFactoryPreset:
--
-- Load a distortion preset.    Default:    AVAudioUnitDistortionPresetDrumsBitBrush
--
-- ObjC selector: @- loadFactoryPreset:@
loadFactoryPreset :: IsAVAudioUnitDistortion avAudioUnitDistortion => avAudioUnitDistortion -> AVAudioUnitDistortionPreset -> IO ()
loadFactoryPreset avAudioUnitDistortion preset =
  sendMessage avAudioUnitDistortion loadFactoryPresetSelector preset

-- | preGain
--
-- Gain applied to the signal before being distorted    Range:      -80 -> 20    Default:    -6    Unit:       dB
--
-- ObjC selector: @- preGain@
preGain :: IsAVAudioUnitDistortion avAudioUnitDistortion => avAudioUnitDistortion -> IO CFloat
preGain avAudioUnitDistortion =
  sendMessage avAudioUnitDistortion preGainSelector

-- | preGain
--
-- Gain applied to the signal before being distorted    Range:      -80 -> 20    Default:    -6    Unit:       dB
--
-- ObjC selector: @- setPreGain:@
setPreGain :: IsAVAudioUnitDistortion avAudioUnitDistortion => avAudioUnitDistortion -> CFloat -> IO ()
setPreGain avAudioUnitDistortion value =
  sendMessage avAudioUnitDistortion setPreGainSelector value

-- | wetDryMix
--
-- Blend of the distorted and dry signals    Range:      0 (all dry) -> 100 (all distorted)    Default:    50    Unit:       Percent
--
-- ObjC selector: @- wetDryMix@
wetDryMix :: IsAVAudioUnitDistortion avAudioUnitDistortion => avAudioUnitDistortion -> IO CFloat
wetDryMix avAudioUnitDistortion =
  sendMessage avAudioUnitDistortion wetDryMixSelector

-- | wetDryMix
--
-- Blend of the distorted and dry signals    Range:      0 (all dry) -> 100 (all distorted)    Default:    50    Unit:       Percent
--
-- ObjC selector: @- setWetDryMix:@
setWetDryMix :: IsAVAudioUnitDistortion avAudioUnitDistortion => avAudioUnitDistortion -> CFloat -> IO ()
setWetDryMix avAudioUnitDistortion value =
  sendMessage avAudioUnitDistortion setWetDryMixSelector value

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @loadFactoryPreset:@
loadFactoryPresetSelector :: Selector '[AVAudioUnitDistortionPreset] ()
loadFactoryPresetSelector = mkSelector "loadFactoryPreset:"

-- | @Selector@ for @preGain@
preGainSelector :: Selector '[] CFloat
preGainSelector = mkSelector "preGain"

-- | @Selector@ for @setPreGain:@
setPreGainSelector :: Selector '[CFloat] ()
setPreGainSelector = mkSelector "setPreGain:"

-- | @Selector@ for @wetDryMix@
wetDryMixSelector :: Selector '[] CFloat
wetDryMixSelector = mkSelector "wetDryMix"

-- | @Selector@ for @setWetDryMix:@
setWetDryMixSelector :: Selector '[CFloat] ()
setWetDryMixSelector = mkSelector "setWetDryMix:"

