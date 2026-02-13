{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | AVAudioEnvironmentReverbParameters
--
-- Parameters used to control the reverb in AVAudioEnvironmentNode
--
-- Reverberation can be used to simulate the acoustic characteristics of an environment.        AVAudioEnvironmentNode has a built in reverb that describes the space that the listener         is in.
--
-- The reverb also has a single filter that sits at the end of the chain. This filter is useful         to shape the overall sound of the reverb. For instance, one of the reverb presets can be         selected to simulate the general space and then the filter can be used to brighten or darken         the overall sound.
--
-- A standalone instance of AVAudioEnvironmentReverbParameters cannot be created.        Only an instance vended out by a source object (e.g. AVAudioEnvironmentNode) can be used.
--
-- Generated bindings for @AVAudioEnvironmentReverbParameters@.
module ObjC.AVFAudio.AVAudioEnvironmentReverbParameters
  ( AVAudioEnvironmentReverbParameters
  , IsAVAudioEnvironmentReverbParameters(..)
  , init_
  , loadFactoryReverbPreset
  , enable
  , setEnable
  , level
  , setLevel
  , filterParameters
  , enableSelector
  , filterParametersSelector
  , initSelector
  , levelSelector
  , loadFactoryReverbPresetSelector
  , setEnableSelector
  , setLevelSelector

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

import Foreign.Ptr (Ptr, FunPtr)
import Foreign.C.Types

import ObjC.Runtime.Types
import ObjC.Runtime.Message (sendMessage, sendOwnedMessage, sendClassMessage, sendOwnedClassMessage)
import ObjC.Runtime.Selector (mkSelector)
import ObjC.Runtime.Class (getRequiredClass)

import ObjC.AVFAudio.Internal.Classes
import ObjC.AVFAudio.Internal.Enums
import ObjC.Foundation.Internal.Classes

-- | @- init@
init_ :: IsAVAudioEnvironmentReverbParameters avAudioEnvironmentReverbParameters => avAudioEnvironmentReverbParameters -> IO (Id AVAudioEnvironmentReverbParameters)
init_ avAudioEnvironmentReverbParameters =
  sendOwnedMessage avAudioEnvironmentReverbParameters initSelector

-- | loadFactoryReverbPreset:
--
-- Load one of the reverb's factory presets
--
-- @preset@ — Reverb preset to be set.
--
-- Loading a factory reverb preset changes the sound of the reverb. This works independently        of the filter which follows the reverb in the signal chain.
--
-- ObjC selector: @- loadFactoryReverbPreset:@
loadFactoryReverbPreset :: IsAVAudioEnvironmentReverbParameters avAudioEnvironmentReverbParameters => avAudioEnvironmentReverbParameters -> AVAudioUnitReverbPreset -> IO ()
loadFactoryReverbPreset avAudioEnvironmentReverbParameters preset =
  sendMessage avAudioEnvironmentReverbParameters loadFactoryReverbPresetSelector preset

-- | enable
--
-- Turns on/off the reverb
--
-- Default:    NO
--
-- ObjC selector: @- enable@
enable :: IsAVAudioEnvironmentReverbParameters avAudioEnvironmentReverbParameters => avAudioEnvironmentReverbParameters -> IO Bool
enable avAudioEnvironmentReverbParameters =
  sendMessage avAudioEnvironmentReverbParameters enableSelector

-- | enable
--
-- Turns on/off the reverb
--
-- Default:    NO
--
-- ObjC selector: @- setEnable:@
setEnable :: IsAVAudioEnvironmentReverbParameters avAudioEnvironmentReverbParameters => avAudioEnvironmentReverbParameters -> Bool -> IO ()
setEnable avAudioEnvironmentReverbParameters value =
  sendMessage avAudioEnvironmentReverbParameters setEnableSelector value

-- | level
--
-- Controls the master level of the reverb
--
-- Range:      -40 to 40 dB        Default:    0.0
--
-- ObjC selector: @- level@
level :: IsAVAudioEnvironmentReverbParameters avAudioEnvironmentReverbParameters => avAudioEnvironmentReverbParameters -> IO CFloat
level avAudioEnvironmentReverbParameters =
  sendMessage avAudioEnvironmentReverbParameters levelSelector

-- | level
--
-- Controls the master level of the reverb
--
-- Range:      -40 to 40 dB        Default:    0.0
--
-- ObjC selector: @- setLevel:@
setLevel :: IsAVAudioEnvironmentReverbParameters avAudioEnvironmentReverbParameters => avAudioEnvironmentReverbParameters -> CFloat -> IO ()
setLevel avAudioEnvironmentReverbParameters value =
  sendMessage avAudioEnvironmentReverbParameters setLevelSelector value

-- | filterParameters
--
-- filter that applies to the output of the reverb
--
-- ObjC selector: @- filterParameters@
filterParameters :: IsAVAudioEnvironmentReverbParameters avAudioEnvironmentReverbParameters => avAudioEnvironmentReverbParameters -> IO (Id AVAudioUnitEQFilterParameters)
filterParameters avAudioEnvironmentReverbParameters =
  sendMessage avAudioEnvironmentReverbParameters filterParametersSelector

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @init@
initSelector :: Selector '[] (Id AVAudioEnvironmentReverbParameters)
initSelector = mkSelector "init"

-- | @Selector@ for @loadFactoryReverbPreset:@
loadFactoryReverbPresetSelector :: Selector '[AVAudioUnitReverbPreset] ()
loadFactoryReverbPresetSelector = mkSelector "loadFactoryReverbPreset:"

-- | @Selector@ for @enable@
enableSelector :: Selector '[] Bool
enableSelector = mkSelector "enable"

-- | @Selector@ for @setEnable:@
setEnableSelector :: Selector '[Bool] ()
setEnableSelector = mkSelector "setEnable:"

-- | @Selector@ for @level@
levelSelector :: Selector '[] CFloat
levelSelector = mkSelector "level"

-- | @Selector@ for @setLevel:@
setLevelSelector :: Selector '[CFloat] ()
setLevelSelector = mkSelector "setLevel:"

-- | @Selector@ for @filterParameters@
filterParametersSelector :: Selector '[] (Id AVAudioUnitEQFilterParameters)
filterParametersSelector = mkSelector "filterParameters"

