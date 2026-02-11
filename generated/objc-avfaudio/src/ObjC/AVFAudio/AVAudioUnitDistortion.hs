{-# LANGUAGE PatternSynonyms #-}
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
  , wetDryMixSelector
  , setWetDryMixSelector

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
-- Load a distortion preset.    Default:    AVAudioUnitDistortionPresetDrumsBitBrush
--
-- ObjC selector: @- loadFactoryPreset:@
loadFactoryPreset :: IsAVAudioUnitDistortion avAudioUnitDistortion => avAudioUnitDistortion -> AVAudioUnitDistortionPreset -> IO ()
loadFactoryPreset avAudioUnitDistortion  preset =
  sendMsg avAudioUnitDistortion (mkSelector "loadFactoryPreset:") retVoid [argCLong (coerce preset)]

-- | preGain
--
-- Gain applied to the signal before being distorted    Range:      -80 -> 20    Default:    -6    Unit:       dB
--
-- ObjC selector: @- preGain@
preGain :: IsAVAudioUnitDistortion avAudioUnitDistortion => avAudioUnitDistortion -> IO CFloat
preGain avAudioUnitDistortion  =
  sendMsg avAudioUnitDistortion (mkSelector "preGain") retCFloat []

-- | preGain
--
-- Gain applied to the signal before being distorted    Range:      -80 -> 20    Default:    -6    Unit:       dB
--
-- ObjC selector: @- setPreGain:@
setPreGain :: IsAVAudioUnitDistortion avAudioUnitDistortion => avAudioUnitDistortion -> CFloat -> IO ()
setPreGain avAudioUnitDistortion  value =
  sendMsg avAudioUnitDistortion (mkSelector "setPreGain:") retVoid [argCFloat (fromIntegral value)]

-- | wetDryMix
--
-- Blend of the distorted and dry signals    Range:      0 (all dry) -> 100 (all distorted)    Default:    50    Unit:       Percent
--
-- ObjC selector: @- wetDryMix@
wetDryMix :: IsAVAudioUnitDistortion avAudioUnitDistortion => avAudioUnitDistortion -> IO CFloat
wetDryMix avAudioUnitDistortion  =
  sendMsg avAudioUnitDistortion (mkSelector "wetDryMix") retCFloat []

-- | wetDryMix
--
-- Blend of the distorted and dry signals    Range:      0 (all dry) -> 100 (all distorted)    Default:    50    Unit:       Percent
--
-- ObjC selector: @- setWetDryMix:@
setWetDryMix :: IsAVAudioUnitDistortion avAudioUnitDistortion => avAudioUnitDistortion -> CFloat -> IO ()
setWetDryMix avAudioUnitDistortion  value =
  sendMsg avAudioUnitDistortion (mkSelector "setWetDryMix:") retVoid [argCFloat (fromIntegral value)]

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @loadFactoryPreset:@
loadFactoryPresetSelector :: Selector
loadFactoryPresetSelector = mkSelector "loadFactoryPreset:"

-- | @Selector@ for @preGain@
preGainSelector :: Selector
preGainSelector = mkSelector "preGain"

-- | @Selector@ for @setPreGain:@
setPreGainSelector :: Selector
setPreGainSelector = mkSelector "setPreGain:"

-- | @Selector@ for @wetDryMix@
wetDryMixSelector :: Selector
wetDryMixSelector = mkSelector "wetDryMix"

-- | @Selector@ for @setWetDryMix:@
setWetDryMixSelector :: Selector
setWetDryMixSelector = mkSelector "setWetDryMix:"

