{-# LANGUAGE PatternSynonyms #-}
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
  , initSelector
  , newSelector
  , setCalibrationMode_levelSelector
  , calibrationModeSelector
  , levelSelector
  , rateSelector
  , setRateSelector
  , groupSelector
  , setGroupSelector
  , gainMetaParameterDefinitionSelector
  , setGainMetaParameterDefinitionSelector
  , rateMetaParameterDefinitionSelector
  , setRateMetaParameterDefinitionSelector
  , mixerDefinitionSelector

  -- * Enum types
  , PHASECalibrationMode(PHASECalibrationMode)
  , pattern PHASECalibrationModeNone
  , pattern PHASECalibrationModeRelativeSpl
  , pattern PHASECalibrationModeAbsoluteSpl

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

import ObjC.PHASE.Internal.Classes
import ObjC.PHASE.Internal.Enums
import ObjC.Foundation.Internal.Classes

-- | @- init@
init_ :: IsPHASEGeneratorNodeDefinition phaseGeneratorNodeDefinition => phaseGeneratorNodeDefinition -> IO (Id PHASEGeneratorNodeDefinition)
init_ phaseGeneratorNodeDefinition  =
  sendMsg phaseGeneratorNodeDefinition (mkSelector "init") (retPtr retVoid) [] >>= ownedObject . castPtr

-- | @+ new@
new :: IO (Id PHASEGeneratorNodeDefinition)
new  =
  do
    cls' <- getRequiredClass "PHASEGeneratorNodeDefinition"
    sendClassMsg cls' (mkSelector "new") (retPtr retVoid) [] >>= ownedObject . castPtr

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
setCalibrationMode_level phaseGeneratorNodeDefinition  calibrationMode level =
  sendMsg phaseGeneratorNodeDefinition (mkSelector "setCalibrationMode:level:") retVoid [argCLong (coerce calibrationMode), argCDouble (fromIntegral level)]

-- | calibrationMode
--
-- The generator's calibration mode. The default value is PHASECalibrationModeNone.
--
-- ObjC selector: @- calibrationMode@
calibrationMode :: IsPHASEGeneratorNodeDefinition phaseGeneratorNodeDefinition => phaseGeneratorNodeDefinition -> IO PHASECalibrationMode
calibrationMode phaseGeneratorNodeDefinition  =
  fmap (coerce :: CLong -> PHASECalibrationMode) $ sendMsg phaseGeneratorNodeDefinition (mkSelector "calibrationMode") retCLong []

-- | level
--
-- The generator's level. The default value is 1.
--
-- Note: The level's underlying unit and range are dependent on the calibrationMode.
--
-- ObjC selector: @- level@
level :: IsPHASEGeneratorNodeDefinition phaseGeneratorNodeDefinition => phaseGeneratorNodeDefinition -> IO CDouble
level phaseGeneratorNodeDefinition  =
  sendMsg phaseGeneratorNodeDefinition (mkSelector "level") retCDouble []

-- | rate
--
-- Linear rate scalar.
--
-- Note: Values are clamped to the range [0.25, 4]. Default value is 1.
--
-- ObjC selector: @- rate@
rate :: IsPHASEGeneratorNodeDefinition phaseGeneratorNodeDefinition => phaseGeneratorNodeDefinition -> IO CDouble
rate phaseGeneratorNodeDefinition  =
  sendMsg phaseGeneratorNodeDefinition (mkSelector "rate") retCDouble []

-- | rate
--
-- Linear rate scalar.
--
-- Note: Values are clamped to the range [0.25, 4]. Default value is 1.
--
-- ObjC selector: @- setRate:@
setRate :: IsPHASEGeneratorNodeDefinition phaseGeneratorNodeDefinition => phaseGeneratorNodeDefinition -> CDouble -> IO ()
setRate phaseGeneratorNodeDefinition  value =
  sendMsg phaseGeneratorNodeDefinition (mkSelector "setRate:") retVoid [argCDouble (fromIntegral value)]

-- | group
--
-- The PHASEGroup object this generator should be associated with for gain and rate control.
--
-- ObjC selector: @- group@
group :: IsPHASEGeneratorNodeDefinition phaseGeneratorNodeDefinition => phaseGeneratorNodeDefinition -> IO (Id PHASEGroup)
group phaseGeneratorNodeDefinition  =
  sendMsg phaseGeneratorNodeDefinition (mkSelector "group") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | group
--
-- The PHASEGroup object this generator should be associated with for gain and rate control.
--
-- ObjC selector: @- setGroup:@
setGroup :: (IsPHASEGeneratorNodeDefinition phaseGeneratorNodeDefinition, IsPHASEGroup value) => phaseGeneratorNodeDefinition -> value -> IO ()
setGroup phaseGeneratorNodeDefinition  value =
withObjCPtr value $ \raw_value ->
    sendMsg phaseGeneratorNodeDefinition (mkSelector "setGroup:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- | gainMetaParameterDefinition
--
-- Optionally attach a metaparameter definition here to enable dynamic control of the gain during playback.
--
-- ObjC selector: @- gainMetaParameterDefinition@
gainMetaParameterDefinition :: IsPHASEGeneratorNodeDefinition phaseGeneratorNodeDefinition => phaseGeneratorNodeDefinition -> IO (Id PHASENumberMetaParameterDefinition)
gainMetaParameterDefinition phaseGeneratorNodeDefinition  =
  sendMsg phaseGeneratorNodeDefinition (mkSelector "gainMetaParameterDefinition") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | gainMetaParameterDefinition
--
-- Optionally attach a metaparameter definition here to enable dynamic control of the gain during playback.
--
-- ObjC selector: @- setGainMetaParameterDefinition:@
setGainMetaParameterDefinition :: (IsPHASEGeneratorNodeDefinition phaseGeneratorNodeDefinition, IsPHASENumberMetaParameterDefinition value) => phaseGeneratorNodeDefinition -> value -> IO ()
setGainMetaParameterDefinition phaseGeneratorNodeDefinition  value =
withObjCPtr value $ \raw_value ->
    sendMsg phaseGeneratorNodeDefinition (mkSelector "setGainMetaParameterDefinition:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- | rateMetaParameterDefinition
--
-- Optionally attach a metaparameter definition here to enable dynamic control of the rate during playback.
--
-- ObjC selector: @- rateMetaParameterDefinition@
rateMetaParameterDefinition :: IsPHASEGeneratorNodeDefinition phaseGeneratorNodeDefinition => phaseGeneratorNodeDefinition -> IO (Id PHASENumberMetaParameterDefinition)
rateMetaParameterDefinition phaseGeneratorNodeDefinition  =
  sendMsg phaseGeneratorNodeDefinition (mkSelector "rateMetaParameterDefinition") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | rateMetaParameterDefinition
--
-- Optionally attach a metaparameter definition here to enable dynamic control of the rate during playback.
--
-- ObjC selector: @- setRateMetaParameterDefinition:@
setRateMetaParameterDefinition :: (IsPHASEGeneratorNodeDefinition phaseGeneratorNodeDefinition, IsPHASENumberMetaParameterDefinition value) => phaseGeneratorNodeDefinition -> value -> IO ()
setRateMetaParameterDefinition phaseGeneratorNodeDefinition  value =
withObjCPtr value $ \raw_value ->
    sendMsg phaseGeneratorNodeDefinition (mkSelector "setRateMetaParameterDefinition:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- | mixerDefinition
--
-- The readonly property that returns the PHASEMixerDefinition this generator was created with and assigned to.
--
-- ObjC selector: @- mixerDefinition@
mixerDefinition :: IsPHASEGeneratorNodeDefinition phaseGeneratorNodeDefinition => phaseGeneratorNodeDefinition -> IO (Id PHASEMixerDefinition)
mixerDefinition phaseGeneratorNodeDefinition  =
  sendMsg phaseGeneratorNodeDefinition (mkSelector "mixerDefinition") (retPtr retVoid) [] >>= retainedObject . castPtr

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @init@
initSelector :: Selector
initSelector = mkSelector "init"

-- | @Selector@ for @new@
newSelector :: Selector
newSelector = mkSelector "new"

-- | @Selector@ for @setCalibrationMode:level:@
setCalibrationMode_levelSelector :: Selector
setCalibrationMode_levelSelector = mkSelector "setCalibrationMode:level:"

-- | @Selector@ for @calibrationMode@
calibrationModeSelector :: Selector
calibrationModeSelector = mkSelector "calibrationMode"

-- | @Selector@ for @level@
levelSelector :: Selector
levelSelector = mkSelector "level"

-- | @Selector@ for @rate@
rateSelector :: Selector
rateSelector = mkSelector "rate"

-- | @Selector@ for @setRate:@
setRateSelector :: Selector
setRateSelector = mkSelector "setRate:"

-- | @Selector@ for @group@
groupSelector :: Selector
groupSelector = mkSelector "group"

-- | @Selector@ for @setGroup:@
setGroupSelector :: Selector
setGroupSelector = mkSelector "setGroup:"

-- | @Selector@ for @gainMetaParameterDefinition@
gainMetaParameterDefinitionSelector :: Selector
gainMetaParameterDefinitionSelector = mkSelector "gainMetaParameterDefinition"

-- | @Selector@ for @setGainMetaParameterDefinition:@
setGainMetaParameterDefinitionSelector :: Selector
setGainMetaParameterDefinitionSelector = mkSelector "setGainMetaParameterDefinition:"

-- | @Selector@ for @rateMetaParameterDefinition@
rateMetaParameterDefinitionSelector :: Selector
rateMetaParameterDefinitionSelector = mkSelector "rateMetaParameterDefinition"

-- | @Selector@ for @setRateMetaParameterDefinition:@
setRateMetaParameterDefinitionSelector :: Selector
setRateMetaParameterDefinitionSelector = mkSelector "setRateMetaParameterDefinition:"

-- | @Selector@ for @mixerDefinition@
mixerDefinitionSelector :: Selector
mixerDefinitionSelector = mkSelector "mixerDefinition"

