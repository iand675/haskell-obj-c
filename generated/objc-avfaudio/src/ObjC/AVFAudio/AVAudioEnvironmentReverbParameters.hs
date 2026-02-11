{-# LANGUAGE PatternSynonyms #-}
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
  , initSelector
  , loadFactoryReverbPresetSelector
  , enableSelector
  , setEnableSelector
  , levelSelector
  , setLevelSelector
  , filterParametersSelector

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

-- | @- init@
init_ :: IsAVAudioEnvironmentReverbParameters avAudioEnvironmentReverbParameters => avAudioEnvironmentReverbParameters -> IO (Id AVAudioEnvironmentReverbParameters)
init_ avAudioEnvironmentReverbParameters  =
  sendMsg avAudioEnvironmentReverbParameters (mkSelector "init") (retPtr retVoid) [] >>= ownedObject . castPtr

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
loadFactoryReverbPreset avAudioEnvironmentReverbParameters  preset =
  sendMsg avAudioEnvironmentReverbParameters (mkSelector "loadFactoryReverbPreset:") retVoid [argCLong (coerce preset)]

-- | enable
--
-- Turns on/off the reverb
--
-- Default:    NO
--
-- ObjC selector: @- enable@
enable :: IsAVAudioEnvironmentReverbParameters avAudioEnvironmentReverbParameters => avAudioEnvironmentReverbParameters -> IO Bool
enable avAudioEnvironmentReverbParameters  =
  fmap ((/= 0) :: CULong -> Bool) $ sendMsg avAudioEnvironmentReverbParameters (mkSelector "enable") retCULong []

-- | enable
--
-- Turns on/off the reverb
--
-- Default:    NO
--
-- ObjC selector: @- setEnable:@
setEnable :: IsAVAudioEnvironmentReverbParameters avAudioEnvironmentReverbParameters => avAudioEnvironmentReverbParameters -> Bool -> IO ()
setEnable avAudioEnvironmentReverbParameters  value =
  sendMsg avAudioEnvironmentReverbParameters (mkSelector "setEnable:") retVoid [argCULong (if value then 1 else 0)]

-- | level
--
-- Controls the master level of the reverb
--
-- Range:      -40 to 40 dB        Default:    0.0
--
-- ObjC selector: @- level@
level :: IsAVAudioEnvironmentReverbParameters avAudioEnvironmentReverbParameters => avAudioEnvironmentReverbParameters -> IO CFloat
level avAudioEnvironmentReverbParameters  =
  sendMsg avAudioEnvironmentReverbParameters (mkSelector "level") retCFloat []

-- | level
--
-- Controls the master level of the reverb
--
-- Range:      -40 to 40 dB        Default:    0.0
--
-- ObjC selector: @- setLevel:@
setLevel :: IsAVAudioEnvironmentReverbParameters avAudioEnvironmentReverbParameters => avAudioEnvironmentReverbParameters -> CFloat -> IO ()
setLevel avAudioEnvironmentReverbParameters  value =
  sendMsg avAudioEnvironmentReverbParameters (mkSelector "setLevel:") retVoid [argCFloat (fromIntegral value)]

-- | filterParameters
--
-- filter that applies to the output of the reverb
--
-- ObjC selector: @- filterParameters@
filterParameters :: IsAVAudioEnvironmentReverbParameters avAudioEnvironmentReverbParameters => avAudioEnvironmentReverbParameters -> IO (Id AVAudioUnitEQFilterParameters)
filterParameters avAudioEnvironmentReverbParameters  =
  sendMsg avAudioEnvironmentReverbParameters (mkSelector "filterParameters") (retPtr retVoid) [] >>= retainedObject . castPtr

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @init@
initSelector :: Selector
initSelector = mkSelector "init"

-- | @Selector@ for @loadFactoryReverbPreset:@
loadFactoryReverbPresetSelector :: Selector
loadFactoryReverbPresetSelector = mkSelector "loadFactoryReverbPreset:"

-- | @Selector@ for @enable@
enableSelector :: Selector
enableSelector = mkSelector "enable"

-- | @Selector@ for @setEnable:@
setEnableSelector :: Selector
setEnableSelector = mkSelector "setEnable:"

-- | @Selector@ for @level@
levelSelector :: Selector
levelSelector = mkSelector "level"

-- | @Selector@ for @setLevel:@
setLevelSelector :: Selector
setLevelSelector = mkSelector "setLevel:"

-- | @Selector@ for @filterParameters@
filterParametersSelector :: Selector
filterParametersSelector = mkSelector "filterParameters"

