{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | *************************************************************************************************
--
-- PHASEEngine
--
-- PHASE engine instance.
--
-- Generated bindings for @PHASEEngine@.
module ObjC.PHASE.PHASEEngine
  ( PHASEEngine
  , IsPHASEEngine(..)
  , init_
  , new
  , initWithUpdateMode
  , initWithUpdateMode_renderingMode
  , startAndReturnError
  , pause
  , stop
  , update
  , outputSpatializationMode
  , setOutputSpatializationMode
  , renderingState
  , rootObject
  , defaultMedium
  , setDefaultMedium
  , defaultReverbPreset
  , setDefaultReverbPreset
  , unitsPerSecond
  , setUnitsPerSecond
  , unitsPerMeter
  , setUnitsPerMeter
  , assetRegistry
  , soundEvents
  , groups
  , duckers
  , activeGroupPreset
  , initSelector
  , newSelector
  , initWithUpdateModeSelector
  , initWithUpdateMode_renderingModeSelector
  , startAndReturnErrorSelector
  , pauseSelector
  , stopSelector
  , updateSelector
  , outputSpatializationModeSelector
  , setOutputSpatializationModeSelector
  , renderingStateSelector
  , rootObjectSelector
  , defaultMediumSelector
  , setDefaultMediumSelector
  , defaultReverbPresetSelector
  , setDefaultReverbPresetSelector
  , unitsPerSecondSelector
  , setUnitsPerSecondSelector
  , unitsPerMeterSelector
  , setUnitsPerMeterSelector
  , assetRegistrySelector
  , soundEventsSelector
  , groupsSelector
  , duckersSelector
  , activeGroupPresetSelector

  -- * Enum types
  , PHASERenderingMode(PHASERenderingMode)
  , pattern PHASERenderingModeLocal
  , pattern PHASERenderingModeClient
  , PHASERenderingState(PHASERenderingState)
  , pattern PHASERenderingStateStopped
  , pattern PHASERenderingStateStarted
  , pattern PHASERenderingStatePaused
  , PHASEReverbPreset(PHASEReverbPreset)
  , pattern PHASEReverbPresetNone
  , pattern PHASEReverbPresetSmallRoom
  , pattern PHASEReverbPresetMediumRoom
  , pattern PHASEReverbPresetLargeRoom
  , pattern PHASEReverbPresetLargeRoom2
  , pattern PHASEReverbPresetMediumChamber
  , pattern PHASEReverbPresetLargeChamber
  , pattern PHASEReverbPresetMediumHall
  , pattern PHASEReverbPresetMediumHall2
  , pattern PHASEReverbPresetMediumHall3
  , pattern PHASEReverbPresetLargeHall
  , pattern PHASEReverbPresetLargeHall2
  , pattern PHASEReverbPresetCathedral
  , PHASESpatializationMode(PHASESpatializationMode)
  , pattern PHASESpatializationModeAutomatic
  , pattern PHASESpatializationModeAlwaysUseBinaural
  , pattern PHASESpatializationModeAlwaysUseChannelBased
  , PHASEUpdateMode(PHASEUpdateMode)
  , pattern PHASEUpdateModeAutomatic
  , pattern PHASEUpdateModeManual

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
init_ :: IsPHASEEngine phaseEngine => phaseEngine -> IO (Id PHASEEngine)
init_ phaseEngine  =
  sendMsg phaseEngine (mkSelector "init") (retPtr retVoid) [] >>= ownedObject . castPtr

-- | @+ new@
new :: IO (Id PHASEEngine)
new  =
  do
    cls' <- getRequiredClass "PHASEEngine"
    sendClassMsg cls' (mkSelector "new") (retPtr retVoid) [] >>= ownedObject . castPtr

-- | initWithUpdateMode:
--
-- Initialize a new engine with an update mode.
--
-- @updateMode@ — Defines how the engine will be updated.
--
-- ObjC selector: @- initWithUpdateMode:@
initWithUpdateMode :: IsPHASEEngine phaseEngine => phaseEngine -> PHASEUpdateMode -> IO (Id PHASEEngine)
initWithUpdateMode phaseEngine  updateMode =
  sendMsg phaseEngine (mkSelector "initWithUpdateMode:") (retPtr retVoid) [argCLong (coerce updateMode)] >>= ownedObject . castPtr

-- | initWithUpdateMode:
--
-- Initialize a new engine with an update mode.
--
-- @updateMode@ — Defines how the engine will be updated.
--
-- @renderingMode@ — Defines where the engine applies rendering. See PHASERenderingMode for more info.
--
-- ObjC selector: @- initWithUpdateMode:renderingMode:@
initWithUpdateMode_renderingMode :: IsPHASEEngine phaseEngine => phaseEngine -> PHASEUpdateMode -> PHASERenderingMode -> IO (Id PHASEEngine)
initWithUpdateMode_renderingMode phaseEngine  updateMode renderingMode =
  sendMsg phaseEngine (mkSelector "initWithUpdateMode:renderingMode:") (retPtr retVoid) [argCLong (coerce updateMode), argCLong (coerce renderingMode)] >>= ownedObject . castPtr

-- | startAndReturnError:
--
-- Start or resume the engine.
--
-- Returns: YES for success.
--
-- ObjC selector: @- startAndReturnError:@
startAndReturnError :: (IsPHASEEngine phaseEngine, IsNSError error_) => phaseEngine -> error_ -> IO Bool
startAndReturnError phaseEngine  error_ =
withObjCPtr error_ $ \raw_error_ ->
    fmap ((/= 0) :: CULong -> Bool) $ sendMsg phaseEngine (mkSelector "startAndReturnError:") retCULong [argPtr (castPtr raw_error_ :: Ptr ())]

-- | pause
--
-- Pause the engine.
--
-- ObjC selector: @- pause@
pause :: IsPHASEEngine phaseEngine => phaseEngine -> IO ()
pause phaseEngine  =
  sendMsg phaseEngine (mkSelector "pause") retVoid []

-- | stop
--
-- Stop the engine.
--
-- ObjC selector: @- stop@
stop :: IsPHASEEngine phaseEngine => phaseEngine -> IO ()
stop phaseEngine  =
  sendMsg phaseEngine (mkSelector "stop") retVoid []

-- | update:
--
-- Manually update the engine instance on the calling thread.
--
-- This will kick off all of the API commands called since the last call to update,        update any systems and objects that need to be kept current, and call any registered handlers.
--
-- Note: This function has no effect if the engine's update mode is PHASEUpdateModeAutomatic.
--
-- ObjC selector: @- update@
update :: IsPHASEEngine phaseEngine => phaseEngine -> IO ()
update phaseEngine  =
  sendMsg phaseEngine (mkSelector "update") retVoid []

-- | outputSpatializationMode
--
-- When set to a value other than PHASESpatializationModeAutomatic,        overrides the default output spatializer and uses the specified one instead.
--
-- ObjC selector: @- outputSpatializationMode@
outputSpatializationMode :: IsPHASEEngine phaseEngine => phaseEngine -> IO PHASESpatializationMode
outputSpatializationMode phaseEngine  =
  fmap (coerce :: CLong -> PHASESpatializationMode) $ sendMsg phaseEngine (mkSelector "outputSpatializationMode") retCLong []

-- | outputSpatializationMode
--
-- When set to a value other than PHASESpatializationModeAutomatic,        overrides the default output spatializer and uses the specified one instead.
--
-- ObjC selector: @- setOutputSpatializationMode:@
setOutputSpatializationMode :: IsPHASEEngine phaseEngine => phaseEngine -> PHASESpatializationMode -> IO ()
setOutputSpatializationMode phaseEngine  value =
  sendMsg phaseEngine (mkSelector "setOutputSpatializationMode:") retVoid [argCLong (coerce value)]

-- | renderingState
--
-- The engine's current rendering state.
--
-- ObjC selector: @- renderingState@
renderingState :: IsPHASEEngine phaseEngine => phaseEngine -> IO PHASERenderingState
renderingState phaseEngine  =
  fmap (coerce :: CLong -> PHASERenderingState) $ sendMsg phaseEngine (mkSelector "renderingState") retCLong []

-- | rootObject
--
-- The root object of the engine's scene graph.
--
-- Attach objects to the engine's rootObject or one of its children to make them active within the engine's scene graph.        This will ensure they take part in the simulation.
--
-- Note: The rootObject is created and owned by the engine.        The rootObject may not be set as the child of another object. This will cause an error to be thrown.        The rootObject's transform may not be changed. This will cause an error to be thrown.        The rootObject may not be copied. This will cause an error to be thrown.
--
-- ObjC selector: @- rootObject@
rootObject :: IsPHASEEngine phaseEngine => phaseEngine -> IO (Id PHASEObject)
rootObject phaseEngine  =
  sendMsg phaseEngine (mkSelector "rootObject") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | defaultMedium
--
-- The default medium in the engine.
--
-- The default value is PHASEMediumPresetAir.
--
-- ObjC selector: @- defaultMedium@
defaultMedium :: IsPHASEEngine phaseEngine => phaseEngine -> IO (Id PHASEMedium)
defaultMedium phaseEngine  =
  sendMsg phaseEngine (mkSelector "defaultMedium") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | defaultMedium
--
-- The default medium in the engine.
--
-- The default value is PHASEMediumPresetAir.
--
-- ObjC selector: @- setDefaultMedium:@
setDefaultMedium :: (IsPHASEEngine phaseEngine, IsPHASEMedium value) => phaseEngine -> value -> IO ()
setDefaultMedium phaseEngine  value =
withObjCPtr value $ \raw_value ->
    sendMsg phaseEngine (mkSelector "setDefaultMedium:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- | defaultReverbPreset
--
-- The default reverb preset in the engine.
--
-- The default value is PHASEReverbPresetNone.
--
-- ObjC selector: @- defaultReverbPreset@
defaultReverbPreset :: IsPHASEEngine phaseEngine => phaseEngine -> IO PHASEReverbPreset
defaultReverbPreset phaseEngine  =
  fmap (coerce :: CLong -> PHASEReverbPreset) $ sendMsg phaseEngine (mkSelector "defaultReverbPreset") retCLong []

-- | defaultReverbPreset
--
-- The default reverb preset in the engine.
--
-- The default value is PHASEReverbPresetNone.
--
-- ObjC selector: @- setDefaultReverbPreset:@
setDefaultReverbPreset :: IsPHASEEngine phaseEngine => phaseEngine -> PHASEReverbPreset -> IO ()
setDefaultReverbPreset phaseEngine  value =
  sendMsg phaseEngine (mkSelector "setDefaultReverbPreset:") retVoid [argCLong (coerce value)]

-- | unitsPerSecond
--
-- The number of units in a second.
--
-- The unitsPerSecond is used internally to scale time/duration values passed to the API.        This allows clients to pass time/duration values in their own native time scale.
--
-- Note: Values are clamped to the range (0, inf]. Default value is 1.
--
-- ObjC selector: @- unitsPerSecond@
unitsPerSecond :: IsPHASEEngine phaseEngine => phaseEngine -> IO CDouble
unitsPerSecond phaseEngine  =
  sendMsg phaseEngine (mkSelector "unitsPerSecond") retCDouble []

-- | unitsPerSecond
--
-- The number of units in a second.
--
-- The unitsPerSecond is used internally to scale time/duration values passed to the API.        This allows clients to pass time/duration values in their own native time scale.
--
-- Note: Values are clamped to the range (0, inf]. Default value is 1.
--
-- ObjC selector: @- setUnitsPerSecond:@
setUnitsPerSecond :: IsPHASEEngine phaseEngine => phaseEngine -> CDouble -> IO ()
setUnitsPerSecond phaseEngine  value =
  sendMsg phaseEngine (mkSelector "setUnitsPerSecond:") retVoid [argCDouble (fromIntegral value)]

-- | unitsPerMeter
--
-- The number of units in a meter.
--
-- The unitsPerMeter is used internally to scale metric values passed to the API.        This allows clients to pass metric values in their own native spatial scale.
--
-- Note: Values are clamped to the range (0, inf]. Default value is 1.
--
-- ObjC selector: @- unitsPerMeter@
unitsPerMeter :: IsPHASEEngine phaseEngine => phaseEngine -> IO CDouble
unitsPerMeter phaseEngine  =
  sendMsg phaseEngine (mkSelector "unitsPerMeter") retCDouble []

-- | unitsPerMeter
--
-- The number of units in a meter.
--
-- The unitsPerMeter is used internally to scale metric values passed to the API.        This allows clients to pass metric values in their own native spatial scale.
--
-- Note: Values are clamped to the range (0, inf]. Default value is 1.
--
-- ObjC selector: @- setUnitsPerMeter:@
setUnitsPerMeter :: IsPHASEEngine phaseEngine => phaseEngine -> CDouble -> IO ()
setUnitsPerMeter phaseEngine  value =
  sendMsg phaseEngine (mkSelector "setUnitsPerMeter:") retVoid [argCDouble (fromIntegral value)]

-- | assetRegistry
--
-- A registry for assets available to the engine
--
-- ObjC selector: @- assetRegistry@
assetRegistry :: IsPHASEEngine phaseEngine => phaseEngine -> IO (Id PHASEAssetRegistry)
assetRegistry phaseEngine  =
  sendMsg phaseEngine (mkSelector "assetRegistry") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | soundEvents
--
-- An array of the active sound event objects in the system
--
-- Returns a dictionary of the sound events at the time it is retrieved. This includes all sound events that are registered with the engine, including those that are preparing, playing, paused or stopping.
--
-- ObjC selector: @- soundEvents@
soundEvents :: IsPHASEEngine phaseEngine => phaseEngine -> IO (Id NSArray)
soundEvents phaseEngine  =
  sendMsg phaseEngine (mkSelector "soundEvents") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | groups
--
-- A dictionary of the groups in the system
--
-- Returns a dictionary of the groups at the time it is retrieved.
--
-- ObjC selector: @- groups@
groups :: IsPHASEEngine phaseEngine => phaseEngine -> IO (Id NSDictionary)
groups phaseEngine  =
  sendMsg phaseEngine (mkSelector "groups") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | duckers
--
-- An array of the ducker objects in the system
--
-- Returns a dictionary of the ducker objects at the time it is retrieved.
--
-- ObjC selector: @- duckers@
duckers :: IsPHASEEngine phaseEngine => phaseEngine -> IO (Id NSArray)
duckers phaseEngine  =
  sendMsg phaseEngine (mkSelector "duckers") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | activeGroupPreset
--
-- The active group mixer preset in the system
--
-- Returns nil if there are no active group presets in the engine. Activate or Deactivate the preset via [PHASEGroupPreset activate] and [PHASEGroupPreset deactivate]
--
-- ObjC selector: @- activeGroupPreset@
activeGroupPreset :: IsPHASEEngine phaseEngine => phaseEngine -> IO (Id PHASEGroupPreset)
activeGroupPreset phaseEngine  =
  sendMsg phaseEngine (mkSelector "activeGroupPreset") (retPtr retVoid) [] >>= retainedObject . castPtr

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @init@
initSelector :: Selector
initSelector = mkSelector "init"

-- | @Selector@ for @new@
newSelector :: Selector
newSelector = mkSelector "new"

-- | @Selector@ for @initWithUpdateMode:@
initWithUpdateModeSelector :: Selector
initWithUpdateModeSelector = mkSelector "initWithUpdateMode:"

-- | @Selector@ for @initWithUpdateMode:renderingMode:@
initWithUpdateMode_renderingModeSelector :: Selector
initWithUpdateMode_renderingModeSelector = mkSelector "initWithUpdateMode:renderingMode:"

-- | @Selector@ for @startAndReturnError:@
startAndReturnErrorSelector :: Selector
startAndReturnErrorSelector = mkSelector "startAndReturnError:"

-- | @Selector@ for @pause@
pauseSelector :: Selector
pauseSelector = mkSelector "pause"

-- | @Selector@ for @stop@
stopSelector :: Selector
stopSelector = mkSelector "stop"

-- | @Selector@ for @update@
updateSelector :: Selector
updateSelector = mkSelector "update"

-- | @Selector@ for @outputSpatializationMode@
outputSpatializationModeSelector :: Selector
outputSpatializationModeSelector = mkSelector "outputSpatializationMode"

-- | @Selector@ for @setOutputSpatializationMode:@
setOutputSpatializationModeSelector :: Selector
setOutputSpatializationModeSelector = mkSelector "setOutputSpatializationMode:"

-- | @Selector@ for @renderingState@
renderingStateSelector :: Selector
renderingStateSelector = mkSelector "renderingState"

-- | @Selector@ for @rootObject@
rootObjectSelector :: Selector
rootObjectSelector = mkSelector "rootObject"

-- | @Selector@ for @defaultMedium@
defaultMediumSelector :: Selector
defaultMediumSelector = mkSelector "defaultMedium"

-- | @Selector@ for @setDefaultMedium:@
setDefaultMediumSelector :: Selector
setDefaultMediumSelector = mkSelector "setDefaultMedium:"

-- | @Selector@ for @defaultReverbPreset@
defaultReverbPresetSelector :: Selector
defaultReverbPresetSelector = mkSelector "defaultReverbPreset"

-- | @Selector@ for @setDefaultReverbPreset:@
setDefaultReverbPresetSelector :: Selector
setDefaultReverbPresetSelector = mkSelector "setDefaultReverbPreset:"

-- | @Selector@ for @unitsPerSecond@
unitsPerSecondSelector :: Selector
unitsPerSecondSelector = mkSelector "unitsPerSecond"

-- | @Selector@ for @setUnitsPerSecond:@
setUnitsPerSecondSelector :: Selector
setUnitsPerSecondSelector = mkSelector "setUnitsPerSecond:"

-- | @Selector@ for @unitsPerMeter@
unitsPerMeterSelector :: Selector
unitsPerMeterSelector = mkSelector "unitsPerMeter"

-- | @Selector@ for @setUnitsPerMeter:@
setUnitsPerMeterSelector :: Selector
setUnitsPerMeterSelector = mkSelector "setUnitsPerMeter:"

-- | @Selector@ for @assetRegistry@
assetRegistrySelector :: Selector
assetRegistrySelector = mkSelector "assetRegistry"

-- | @Selector@ for @soundEvents@
soundEventsSelector :: Selector
soundEventsSelector = mkSelector "soundEvents"

-- | @Selector@ for @groups@
groupsSelector :: Selector
groupsSelector = mkSelector "groups"

-- | @Selector@ for @duckers@
duckersSelector :: Selector
duckersSelector = mkSelector "duckers"

-- | @Selector@ for @activeGroupPreset@
activeGroupPresetSelector :: Selector
activeGroupPresetSelector = mkSelector "activeGroupPreset"

