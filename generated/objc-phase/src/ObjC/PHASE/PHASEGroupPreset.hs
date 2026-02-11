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
  , initSelector
  , newSelector
  , initWithEngine_settings_timeToTarget_timeToResetSelector
  , activateSelector
  , activateWithTimeToTargetOverrideSelector
  , deactivateSelector
  , deactivateWithTimeToResetOverrideSelector
  , settingsSelector
  , timeToTargetSelector
  , timeToResetSelector


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
import ObjC.Foundation.Internal.Classes

-- | @- init@
init_ :: IsPHASEGroupPreset phaseGroupPreset => phaseGroupPreset -> IO (Id PHASEGroupPreset)
init_ phaseGroupPreset  =
  sendMsg phaseGroupPreset (mkSelector "init") (retPtr retVoid) [] >>= ownedObject . castPtr

-- | @+ new@
new :: IO (Id PHASEGroupPreset)
new  =
  do
    cls' <- getRequiredClass "PHASEGroupPreset"
    sendClassMsg cls' (mkSelector "new") (retPtr retVoid) [] >>= ownedObject . castPtr

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
initWithEngine_settings_timeToTarget_timeToReset phaseGroupPreset  engine settings timeToTarget timeToReset =
withObjCPtr engine $ \raw_engine ->
  withObjCPtr settings $ \raw_settings ->
      sendMsg phaseGroupPreset (mkSelector "initWithEngine:settings:timeToTarget:timeToReset:") (retPtr retVoid) [argPtr (castPtr raw_engine :: Ptr ()), argPtr (castPtr raw_settings :: Ptr ()), argCDouble (fromIntegral timeToTarget), argCDouble (fromIntegral timeToReset)] >>= ownedObject . castPtr

-- | activate
--
-- Activate this preset in the PHASEEngine object it was initialized with. The internal timeToTarget value is used.        The current preset will be deactivated automatically.
--
-- ObjC selector: @- activate@
activate :: IsPHASEGroupPreset phaseGroupPreset => phaseGroupPreset -> IO ()
activate phaseGroupPreset  =
  sendMsg phaseGroupPreset (mkSelector "activate") retVoid []

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
activateWithTimeToTargetOverride phaseGroupPreset  timeToTargetOverride =
  sendMsg phaseGroupPreset (mkSelector "activateWithTimeToTargetOverride:") retVoid [argCDouble (fromIntegral timeToTargetOverride)]

-- | deactivate
--
-- Deactivate this preset and return the system to default unity values. The internal timeToReset value is used.
--
-- ObjC selector: @- deactivate@
deactivate :: IsPHASEGroupPreset phaseGroupPreset => phaseGroupPreset -> IO ()
deactivate phaseGroupPreset  =
  sendMsg phaseGroupPreset (mkSelector "deactivate") retVoid []

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
deactivateWithTimeToResetOverride phaseGroupPreset  timeToResetOverride =
  sendMsg phaseGroupPreset (mkSelector "deactivateWithTimeToResetOverride:") retVoid [argCDouble (fromIntegral timeToResetOverride)]

-- | settings
--
-- The collection of PHASEGroupPresetSetting objects to apply when this preset is activated.
--
-- ObjC selector: @- settings@
settings :: IsPHASEGroupPreset phaseGroupPreset => phaseGroupPreset -> IO (Id NSDictionary)
settings phaseGroupPreset  =
  sendMsg phaseGroupPreset (mkSelector "settings") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | timeToTarget
--
-- The time interval that all group settings in this preset will take to gradually fade to the new value
--
-- Note: The timeToTarget is scaled by unitsPerSecond internally, so can be provided at the client's native time scale.
--
-- ObjC selector: @- timeToTarget@
timeToTarget :: IsPHASEGroupPreset phaseGroupPreset => phaseGroupPreset -> IO CDouble
timeToTarget phaseGroupPreset  =
  sendMsg phaseGroupPreset (mkSelector "timeToTarget") retCDouble []

-- | timeToReset
--
-- The time interval that all group settings in this preset will take to gradually fade to the unity value
--
-- Note: The timeToReset is scaled by unitsPerSecond internally, so can be provided at the client's native time scale.
--
-- ObjC selector: @- timeToReset@
timeToReset :: IsPHASEGroupPreset phaseGroupPreset => phaseGroupPreset -> IO CDouble
timeToReset phaseGroupPreset  =
  sendMsg phaseGroupPreset (mkSelector "timeToReset") retCDouble []

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @init@
initSelector :: Selector
initSelector = mkSelector "init"

-- | @Selector@ for @new@
newSelector :: Selector
newSelector = mkSelector "new"

-- | @Selector@ for @initWithEngine:settings:timeToTarget:timeToReset:@
initWithEngine_settings_timeToTarget_timeToResetSelector :: Selector
initWithEngine_settings_timeToTarget_timeToResetSelector = mkSelector "initWithEngine:settings:timeToTarget:timeToReset:"

-- | @Selector@ for @activate@
activateSelector :: Selector
activateSelector = mkSelector "activate"

-- | @Selector@ for @activateWithTimeToTargetOverride:@
activateWithTimeToTargetOverrideSelector :: Selector
activateWithTimeToTargetOverrideSelector = mkSelector "activateWithTimeToTargetOverride:"

-- | @Selector@ for @deactivate@
deactivateSelector :: Selector
deactivateSelector = mkSelector "deactivate"

-- | @Selector@ for @deactivateWithTimeToResetOverride:@
deactivateWithTimeToResetOverrideSelector :: Selector
deactivateWithTimeToResetOverrideSelector = mkSelector "deactivateWithTimeToResetOverride:"

-- | @Selector@ for @settings@
settingsSelector :: Selector
settingsSelector = mkSelector "settings"

-- | @Selector@ for @timeToTarget@
timeToTargetSelector :: Selector
timeToTargetSelector = mkSelector "timeToTarget"

-- | @Selector@ for @timeToReset@
timeToResetSelector :: Selector
timeToResetSelector = mkSelector "timeToReset"

