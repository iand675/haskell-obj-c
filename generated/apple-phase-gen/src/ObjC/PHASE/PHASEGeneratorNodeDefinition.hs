{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | *************************************************************************************************
--
-- PHASEGeneratorNodeDefinition
--
-- An object for defining a generator node when building a sound event.
--
-- Generated bindings for @PHASEGeneratorNodeDefinition@.
module ObjC.PHASE.PHASEGeneratorNodeDefinition
  ( PHASEGeneratorNodeDefinition
  , IsPHASEGeneratorNodeDefinition(..)
  , init_
  , new
  , setCalibrationMode_level
  , calibrationMode
  , level
  , rate
  , setRate
  , group
  , setGroup
  , gainMetaParameterDefinition
  , setGainMetaParameterDefinition
  , rateMetaParameterDefinition
  , setRateMetaParameterDefinition
  , mixerDefinition
  , calibrationModeSelector
  , gainMetaParameterDefinitionSelector
  , groupSelector
  , initSelector
  , levelSelector
  , mixerDefinitionSelector
  , newSelector
  , rateMetaParameterDefinitionSelector
  , rateSelector
  , setCalibrationMode_levelSelector
  , setGainMetaParameterDefinitionSelector
  , setGroupSelector
  , setRateMetaParameterDefinitionSelector
  , setRateSelector

  -- * Enum types
  , PHASECalibrationMode(PHASECalibrationMode)
  , pattern PHASECalibrationModeNone
  , pattern PHASECalibrationModeRelativeSpl
  , pattern PHASECalibrationModeAbsoluteSpl

  ) where

import Foreign.Ptr (Ptr, FunPtr)
import Foreign.C.Types

import ObjC.Runtime.Types
import ObjC.Runtime.Message (sendMessage, sendOwnedMessage, sendClassMessage, sendOwnedClassMessage)
import ObjC.Runtime.Selector (mkSelector)
import ObjC.Runtime.Class (getRequiredClass)

import ObjC.PHASE.Internal.Classes
import ObjC.PHASE.Internal.Enums
import ObjC.Foundation.Internal.Classes

-- | @- init@
init_ :: IsPHASEGeneratorNodeDefinition phaseGeneratorNodeDefinition => phaseGeneratorNodeDefinition -> IO (Id PHASEGeneratorNodeDefinition)
init_ phaseGeneratorNodeDefinition =
  sendOwnedMessage phaseGeneratorNodeDefinition initSelector

-- | @+ new@
new :: IO (Id PHASEGeneratorNodeDefinition)
new  =
  do
    cls' <- getRequiredClass "PHASEGeneratorNodeDefinition"
    sendOwnedClassMessage cls' newSelector

-- | setCalibrationMode:level
--
-- Set the generator's calibration mode and level.
--
-- @calibrationMode@ — The calibration mode.
--
-- @level@ — The level.
--
-- Note: The level, including its underlying unit and range, are dependent on the calibration mode.
--
-- ObjC selector: @- setCalibrationMode:level:@
setCalibrationMode_level :: IsPHASEGeneratorNodeDefinition phaseGeneratorNodeDefinition => phaseGeneratorNodeDefinition -> PHASECalibrationMode -> CDouble -> IO ()
setCalibrationMode_level phaseGeneratorNodeDefinition calibrationMode level =
  sendMessage phaseGeneratorNodeDefinition setCalibrationMode_levelSelector calibrationMode level

-- | calibrationMode
--
-- The generator's calibration mode. The default value is PHASECalibrationModeNone.
--
-- ObjC selector: @- calibrationMode@
calibrationMode :: IsPHASEGeneratorNodeDefinition phaseGeneratorNodeDefinition => phaseGeneratorNodeDefinition -> IO PHASECalibrationMode
calibrationMode phaseGeneratorNodeDefinition =
  sendMessage phaseGeneratorNodeDefinition calibrationModeSelector

-- | level
--
-- The generator's level. The default value is 1.
--
-- Note: The level's underlying unit and range are dependent on the calibrationMode.
--
-- ObjC selector: @- level@
level :: IsPHASEGeneratorNodeDefinition phaseGeneratorNodeDefinition => phaseGeneratorNodeDefinition -> IO CDouble
level phaseGeneratorNodeDefinition =
  sendMessage phaseGeneratorNodeDefinition levelSelector

-- | rate
--
-- Linear rate scalar.
--
-- Note: Values are clamped to the range [0.25, 4]. Default value is 1.
--
-- ObjC selector: @- rate@
rate :: IsPHASEGeneratorNodeDefinition phaseGeneratorNodeDefinition => phaseGeneratorNodeDefinition -> IO CDouble
rate phaseGeneratorNodeDefinition =
  sendMessage phaseGeneratorNodeDefinition rateSelector

-- | rate
--
-- Linear rate scalar.
--
-- Note: Values are clamped to the range [0.25, 4]. Default value is 1.
--
-- ObjC selector: @- setRate:@
setRate :: IsPHASEGeneratorNodeDefinition phaseGeneratorNodeDefinition => phaseGeneratorNodeDefinition -> CDouble -> IO ()
setRate phaseGeneratorNodeDefinition value =
  sendMessage phaseGeneratorNodeDefinition setRateSelector value

-- | group
--
-- The PHASEGroup object this generator should be associated with for gain and rate control.
--
-- ObjC selector: @- group@
group :: IsPHASEGeneratorNodeDefinition phaseGeneratorNodeDefinition => phaseGeneratorNodeDefinition -> IO (Id PHASEGroup)
group phaseGeneratorNodeDefinition =
  sendMessage phaseGeneratorNodeDefinition groupSelector

-- | group
--
-- The PHASEGroup object this generator should be associated with for gain and rate control.
--
-- ObjC selector: @- setGroup:@
setGroup :: (IsPHASEGeneratorNodeDefinition phaseGeneratorNodeDefinition, IsPHASEGroup value) => phaseGeneratorNodeDefinition -> value -> IO ()
setGroup phaseGeneratorNodeDefinition value =
  sendMessage phaseGeneratorNodeDefinition setGroupSelector (toPHASEGroup value)

-- | gainMetaParameterDefinition
--
-- Optionally attach a metaparameter definition here to enable dynamic control of the gain during playback.
--
-- ObjC selector: @- gainMetaParameterDefinition@
gainMetaParameterDefinition :: IsPHASEGeneratorNodeDefinition phaseGeneratorNodeDefinition => phaseGeneratorNodeDefinition -> IO (Id PHASENumberMetaParameterDefinition)
gainMetaParameterDefinition phaseGeneratorNodeDefinition =
  sendMessage phaseGeneratorNodeDefinition gainMetaParameterDefinitionSelector

-- | gainMetaParameterDefinition
--
-- Optionally attach a metaparameter definition here to enable dynamic control of the gain during playback.
--
-- ObjC selector: @- setGainMetaParameterDefinition:@
setGainMetaParameterDefinition :: (IsPHASEGeneratorNodeDefinition phaseGeneratorNodeDefinition, IsPHASENumberMetaParameterDefinition value) => phaseGeneratorNodeDefinition -> value -> IO ()
setGainMetaParameterDefinition phaseGeneratorNodeDefinition value =
  sendMessage phaseGeneratorNodeDefinition setGainMetaParameterDefinitionSelector (toPHASENumberMetaParameterDefinition value)

-- | rateMetaParameterDefinition
--
-- Optionally attach a metaparameter definition here to enable dynamic control of the rate during playback.
--
-- ObjC selector: @- rateMetaParameterDefinition@
rateMetaParameterDefinition :: IsPHASEGeneratorNodeDefinition phaseGeneratorNodeDefinition => phaseGeneratorNodeDefinition -> IO (Id PHASENumberMetaParameterDefinition)
rateMetaParameterDefinition phaseGeneratorNodeDefinition =
  sendMessage phaseGeneratorNodeDefinition rateMetaParameterDefinitionSelector

-- | rateMetaParameterDefinition
--
-- Optionally attach a metaparameter definition here to enable dynamic control of the rate during playback.
--
-- ObjC selector: @- setRateMetaParameterDefinition:@
setRateMetaParameterDefinition :: (IsPHASEGeneratorNodeDefinition phaseGeneratorNodeDefinition, IsPHASENumberMetaParameterDefinition value) => phaseGeneratorNodeDefinition -> value -> IO ()
setRateMetaParameterDefinition phaseGeneratorNodeDefinition value =
  sendMessage phaseGeneratorNodeDefinition setRateMetaParameterDefinitionSelector (toPHASENumberMetaParameterDefinition value)

-- | mixerDefinition
--
-- The readonly property that returns the PHASEMixerDefinition this generator was created with and assigned to.
--
-- ObjC selector: @- mixerDefinition@
mixerDefinition :: IsPHASEGeneratorNodeDefinition phaseGeneratorNodeDefinition => phaseGeneratorNodeDefinition -> IO (Id PHASEMixerDefinition)
mixerDefinition phaseGeneratorNodeDefinition =
  sendMessage phaseGeneratorNodeDefinition mixerDefinitionSelector

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @init@
initSelector :: Selector '[] (Id PHASEGeneratorNodeDefinition)
initSelector = mkSelector "init"

-- | @Selector@ for @new@
newSelector :: Selector '[] (Id PHASEGeneratorNodeDefinition)
newSelector = mkSelector "new"

-- | @Selector@ for @setCalibrationMode:level:@
setCalibrationMode_levelSelector :: Selector '[PHASECalibrationMode, CDouble] ()
setCalibrationMode_levelSelector = mkSelector "setCalibrationMode:level:"

-- | @Selector@ for @calibrationMode@
calibrationModeSelector :: Selector '[] PHASECalibrationMode
calibrationModeSelector = mkSelector "calibrationMode"

-- | @Selector@ for @level@
levelSelector :: Selector '[] CDouble
levelSelector = mkSelector "level"

-- | @Selector@ for @rate@
rateSelector :: Selector '[] CDouble
rateSelector = mkSelector "rate"

-- | @Selector@ for @setRate:@
setRateSelector :: Selector '[CDouble] ()
setRateSelector = mkSelector "setRate:"

-- | @Selector@ for @group@
groupSelector :: Selector '[] (Id PHASEGroup)
groupSelector = mkSelector "group"

-- | @Selector@ for @setGroup:@
setGroupSelector :: Selector '[Id PHASEGroup] ()
setGroupSelector = mkSelector "setGroup:"

-- | @Selector@ for @gainMetaParameterDefinition@
gainMetaParameterDefinitionSelector :: Selector '[] (Id PHASENumberMetaParameterDefinition)
gainMetaParameterDefinitionSelector = mkSelector "gainMetaParameterDefinition"

-- | @Selector@ for @setGainMetaParameterDefinition:@
setGainMetaParameterDefinitionSelector :: Selector '[Id PHASENumberMetaParameterDefinition] ()
setGainMetaParameterDefinitionSelector = mkSelector "setGainMetaParameterDefinition:"

-- | @Selector@ for @rateMetaParameterDefinition@
rateMetaParameterDefinitionSelector :: Selector '[] (Id PHASENumberMetaParameterDefinition)
rateMetaParameterDefinitionSelector = mkSelector "rateMetaParameterDefinition"

-- | @Selector@ for @setRateMetaParameterDefinition:@
setRateMetaParameterDefinitionSelector :: Selector '[Id PHASENumberMetaParameterDefinition] ()
setRateMetaParameterDefinitionSelector = mkSelector "setRateMetaParameterDefinition:"

-- | @Selector@ for @mixerDefinition@
mixerDefinitionSelector :: Selector '[] (Id PHASEMixerDefinition)
mixerDefinitionSelector = mkSelector "mixerDefinition"

