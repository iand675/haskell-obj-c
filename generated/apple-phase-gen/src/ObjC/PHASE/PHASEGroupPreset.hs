{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | *************************************************************************************************
--
-- PHASEGroupPreset
--
-- A PHASEGroupPreset holds a collection of PHASEGroupPresetSetting objects and other parameters to be applied all at once during playback.
--
-- Initialize beforehand, and use activate or deactivate to switch to the new preset during playback.        Activating a preset will automatically deactivate the current one.
--
-- Generated bindings for @PHASEGroupPreset@.
module ObjC.PHASE.PHASEGroupPreset
  ( PHASEGroupPreset
  , IsPHASEGroupPreset(..)
  , init_
  , new
  , initWithEngine_settings_timeToTarget_timeToReset
  , activate
  , activateWithTimeToTargetOverride
  , deactivate
  , deactivateWithTimeToResetOverride
  , settings
  , timeToTarget
  , timeToReset
  , activateSelector
  , activateWithTimeToTargetOverrideSelector
  , deactivateSelector
  , deactivateWithTimeToResetOverrideSelector
  , initSelector
  , initWithEngine_settings_timeToTarget_timeToResetSelector
  , newSelector
  , settingsSelector
  , timeToResetSelector
  , timeToTargetSelector


  ) where

import Foreign.Ptr (Ptr, FunPtr)
import Foreign.C.Types

import ObjC.Runtime.Types
import ObjC.Runtime.Message (sendMessage, sendOwnedMessage, sendClassMessage, sendOwnedClassMessage)
import ObjC.Runtime.Selector (mkSelector)
import ObjC.Runtime.Class (getRequiredClass)

import ObjC.PHASE.Internal.Classes
import ObjC.Foundation.Internal.Classes

-- | @- init@
init_ :: IsPHASEGroupPreset phaseGroupPreset => phaseGroupPreset -> IO (Id PHASEGroupPreset)
init_ phaseGroupPreset =
  sendOwnedMessage phaseGroupPreset initSelector

-- | @+ new@
new :: IO (Id PHASEGroupPreset)
new  =
  do
    cls' <- getRequiredClass "PHASEGroupPreset"
    sendOwnedClassMessage cls' newSelector

-- | initWithEngine
--
-- Create a new PHASEGroupPreset object with a given PHASEEngine object.
--
-- @engine@ — The PHASEEngine object to register this preset with.
--
-- @settings@ — A dictionary containing PHASEGroupPresetSetting objects paired with PHASEGroup objects as keys.
--
-- @timeToTarget@ — The time interval that all group settings in this preset will take to gradually fade to the new value
--
-- @timeToReset@ — The time interval that all group settings in this preset will take to gradually fade to the unity value
--
-- Note: The timeToTarget and timeToReset are scaled by unitsPerSecond internally, so can be provided at the client's native time scale.
--
-- ObjC selector: @- initWithEngine:settings:timeToTarget:timeToReset:@
initWithEngine_settings_timeToTarget_timeToReset :: (IsPHASEGroupPreset phaseGroupPreset, IsPHASEEngine engine, IsNSDictionary settings) => phaseGroupPreset -> engine -> settings -> CDouble -> CDouble -> IO (Id PHASEGroupPreset)
initWithEngine_settings_timeToTarget_timeToReset phaseGroupPreset engine settings timeToTarget timeToReset =
  sendOwnedMessage phaseGroupPreset initWithEngine_settings_timeToTarget_timeToResetSelector (toPHASEEngine engine) (toNSDictionary settings) timeToTarget timeToReset

-- | activate
--
-- Activate this preset in the PHASEEngine object it was initialized with. The internal timeToTarget value is used.        The current preset will be deactivated automatically.
--
-- ObjC selector: @- activate@
activate :: IsPHASEGroupPreset phaseGroupPreset => phaseGroupPreset -> IO ()
activate phaseGroupPreset =
  sendMessage phaseGroupPreset activateSelector

-- | activateWithTimeToTargetOverride
--
-- Activate this preset in the PHASEEngine object it was initialized with.        The current preset will be deactivated automatically.
--
-- @timeToTargetOverride@ — Override the timeToTarget value in the preset with this value.
--
-- Note: The timeToTargetOverride is scaled by unitsPerSecond internally, so can be provided at the client's native time scale.
--
-- ObjC selector: @- activateWithTimeToTargetOverride:@
activateWithTimeToTargetOverride :: IsPHASEGroupPreset phaseGroupPreset => phaseGroupPreset -> CDouble -> IO ()
activateWithTimeToTargetOverride phaseGroupPreset timeToTargetOverride =
  sendMessage phaseGroupPreset activateWithTimeToTargetOverrideSelector timeToTargetOverride

-- | deactivate
--
-- Deactivate this preset and return the system to default unity values. The internal timeToReset value is used.
--
-- ObjC selector: @- deactivate@
deactivate :: IsPHASEGroupPreset phaseGroupPreset => phaseGroupPreset -> IO ()
deactivate phaseGroupPreset =
  sendMessage phaseGroupPreset deactivateSelector

-- | deactivateWithTimeToResetOverride
--
-- Deactivate this preset and return the system to default unity values.
--
-- @timeToResetOverride@ — Override the timeToReset value in the preset with this value.
--
-- Note: The timeToResetOverride is scaled by unitsPerSecond internally, so can be provided at the client's native time scale.
--
-- ObjC selector: @- deactivateWithTimeToResetOverride:@
deactivateWithTimeToResetOverride :: IsPHASEGroupPreset phaseGroupPreset => phaseGroupPreset -> CDouble -> IO ()
deactivateWithTimeToResetOverride phaseGroupPreset timeToResetOverride =
  sendMessage phaseGroupPreset deactivateWithTimeToResetOverrideSelector timeToResetOverride

-- | settings
--
-- The collection of PHASEGroupPresetSetting objects to apply when this preset is activated.
--
-- ObjC selector: @- settings@
settings :: IsPHASEGroupPreset phaseGroupPreset => phaseGroupPreset -> IO (Id NSDictionary)
settings phaseGroupPreset =
  sendMessage phaseGroupPreset settingsSelector

-- | timeToTarget
--
-- The time interval that all group settings in this preset will take to gradually fade to the new value
--
-- Note: The timeToTarget is scaled by unitsPerSecond internally, so can be provided at the client's native time scale.
--
-- ObjC selector: @- timeToTarget@
timeToTarget :: IsPHASEGroupPreset phaseGroupPreset => phaseGroupPreset -> IO CDouble
timeToTarget phaseGroupPreset =
  sendMessage phaseGroupPreset timeToTargetSelector

-- | timeToReset
--
-- The time interval that all group settings in this preset will take to gradually fade to the unity value
--
-- Note: The timeToReset is scaled by unitsPerSecond internally, so can be provided at the client's native time scale.
--
-- ObjC selector: @- timeToReset@
timeToReset :: IsPHASEGroupPreset phaseGroupPreset => phaseGroupPreset -> IO CDouble
timeToReset phaseGroupPreset =
  sendMessage phaseGroupPreset timeToResetSelector

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @init@
initSelector :: Selector '[] (Id PHASEGroupPreset)
initSelector = mkSelector "init"

-- | @Selector@ for @new@
newSelector :: Selector '[] (Id PHASEGroupPreset)
newSelector = mkSelector "new"

-- | @Selector@ for @initWithEngine:settings:timeToTarget:timeToReset:@
initWithEngine_settings_timeToTarget_timeToResetSelector :: Selector '[Id PHASEEngine, Id NSDictionary, CDouble, CDouble] (Id PHASEGroupPreset)
initWithEngine_settings_timeToTarget_timeToResetSelector = mkSelector "initWithEngine:settings:timeToTarget:timeToReset:"

-- | @Selector@ for @activate@
activateSelector :: Selector '[] ()
activateSelector = mkSelector "activate"

-- | @Selector@ for @activateWithTimeToTargetOverride:@
activateWithTimeToTargetOverrideSelector :: Selector '[CDouble] ()
activateWithTimeToTargetOverrideSelector = mkSelector "activateWithTimeToTargetOverride:"

-- | @Selector@ for @deactivate@
deactivateSelector :: Selector '[] ()
deactivateSelector = mkSelector "deactivate"

-- | @Selector@ for @deactivateWithTimeToResetOverride:@
deactivateWithTimeToResetOverrideSelector :: Selector '[CDouble] ()
deactivateWithTimeToResetOverrideSelector = mkSelector "deactivateWithTimeToResetOverride:"

-- | @Selector@ for @settings@
settingsSelector :: Selector '[] (Id NSDictionary)
settingsSelector = mkSelector "settings"

-- | @Selector@ for @timeToTarget@
timeToTargetSelector :: Selector '[] CDouble
timeToTargetSelector = mkSelector "timeToTarget"

-- | @Selector@ for @timeToReset@
timeToResetSelector :: Selector '[] CDouble
timeToResetSelector = mkSelector "timeToReset"

