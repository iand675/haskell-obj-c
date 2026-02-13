{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Generated bindings for @MTRThermostatClusterScheduleTransitionStruct@.
module ObjC.Matter.MTRThermostatClusterScheduleTransitionStruct
  ( MTRThermostatClusterScheduleTransitionStruct
  , IsMTRThermostatClusterScheduleTransitionStruct(..)
  , dayOfWeek
  , setDayOfWeek
  , transitionTime
  , setTransitionTime
  , presetHandle
  , setPresetHandle
  , systemMode
  , setSystemMode
  , coolingSetpoint
  , setCoolingSetpoint
  , heatingSetpoint
  , setHeatingSetpoint
  , coolingSetpointSelector
  , dayOfWeekSelector
  , heatingSetpointSelector
  , presetHandleSelector
  , setCoolingSetpointSelector
  , setDayOfWeekSelector
  , setHeatingSetpointSelector
  , setPresetHandleSelector
  , setSystemModeSelector
  , setTransitionTimeSelector
  , systemModeSelector
  , transitionTimeSelector


  ) where

import Foreign.Ptr (Ptr, FunPtr)
import Foreign.C.Types

import ObjC.Runtime.Types
import ObjC.Runtime.Message (sendMessage, sendOwnedMessage, sendClassMessage, sendOwnedClassMessage)
import ObjC.Runtime.Selector (mkSelector)
import ObjC.Runtime.Class (getRequiredClass)

import ObjC.Matter.Internal.Classes
import ObjC.Foundation.Internal.Classes

-- | @- dayOfWeek@
dayOfWeek :: IsMTRThermostatClusterScheduleTransitionStruct mtrThermostatClusterScheduleTransitionStruct => mtrThermostatClusterScheduleTransitionStruct -> IO (Id NSNumber)
dayOfWeek mtrThermostatClusterScheduleTransitionStruct =
  sendMessage mtrThermostatClusterScheduleTransitionStruct dayOfWeekSelector

-- | @- setDayOfWeek:@
setDayOfWeek :: (IsMTRThermostatClusterScheduleTransitionStruct mtrThermostatClusterScheduleTransitionStruct, IsNSNumber value) => mtrThermostatClusterScheduleTransitionStruct -> value -> IO ()
setDayOfWeek mtrThermostatClusterScheduleTransitionStruct value =
  sendMessage mtrThermostatClusterScheduleTransitionStruct setDayOfWeekSelector (toNSNumber value)

-- | @- transitionTime@
transitionTime :: IsMTRThermostatClusterScheduleTransitionStruct mtrThermostatClusterScheduleTransitionStruct => mtrThermostatClusterScheduleTransitionStruct -> IO (Id NSNumber)
transitionTime mtrThermostatClusterScheduleTransitionStruct =
  sendMessage mtrThermostatClusterScheduleTransitionStruct transitionTimeSelector

-- | @- setTransitionTime:@
setTransitionTime :: (IsMTRThermostatClusterScheduleTransitionStruct mtrThermostatClusterScheduleTransitionStruct, IsNSNumber value) => mtrThermostatClusterScheduleTransitionStruct -> value -> IO ()
setTransitionTime mtrThermostatClusterScheduleTransitionStruct value =
  sendMessage mtrThermostatClusterScheduleTransitionStruct setTransitionTimeSelector (toNSNumber value)

-- | @- presetHandle@
presetHandle :: IsMTRThermostatClusterScheduleTransitionStruct mtrThermostatClusterScheduleTransitionStruct => mtrThermostatClusterScheduleTransitionStruct -> IO (Id NSData)
presetHandle mtrThermostatClusterScheduleTransitionStruct =
  sendMessage mtrThermostatClusterScheduleTransitionStruct presetHandleSelector

-- | @- setPresetHandle:@
setPresetHandle :: (IsMTRThermostatClusterScheduleTransitionStruct mtrThermostatClusterScheduleTransitionStruct, IsNSData value) => mtrThermostatClusterScheduleTransitionStruct -> value -> IO ()
setPresetHandle mtrThermostatClusterScheduleTransitionStruct value =
  sendMessage mtrThermostatClusterScheduleTransitionStruct setPresetHandleSelector (toNSData value)

-- | @- systemMode@
systemMode :: IsMTRThermostatClusterScheduleTransitionStruct mtrThermostatClusterScheduleTransitionStruct => mtrThermostatClusterScheduleTransitionStruct -> IO (Id NSNumber)
systemMode mtrThermostatClusterScheduleTransitionStruct =
  sendMessage mtrThermostatClusterScheduleTransitionStruct systemModeSelector

-- | @- setSystemMode:@
setSystemMode :: (IsMTRThermostatClusterScheduleTransitionStruct mtrThermostatClusterScheduleTransitionStruct, IsNSNumber value) => mtrThermostatClusterScheduleTransitionStruct -> value -> IO ()
setSystemMode mtrThermostatClusterScheduleTransitionStruct value =
  sendMessage mtrThermostatClusterScheduleTransitionStruct setSystemModeSelector (toNSNumber value)

-- | @- coolingSetpoint@
coolingSetpoint :: IsMTRThermostatClusterScheduleTransitionStruct mtrThermostatClusterScheduleTransitionStruct => mtrThermostatClusterScheduleTransitionStruct -> IO (Id NSNumber)
coolingSetpoint mtrThermostatClusterScheduleTransitionStruct =
  sendMessage mtrThermostatClusterScheduleTransitionStruct coolingSetpointSelector

-- | @- setCoolingSetpoint:@
setCoolingSetpoint :: (IsMTRThermostatClusterScheduleTransitionStruct mtrThermostatClusterScheduleTransitionStruct, IsNSNumber value) => mtrThermostatClusterScheduleTransitionStruct -> value -> IO ()
setCoolingSetpoint mtrThermostatClusterScheduleTransitionStruct value =
  sendMessage mtrThermostatClusterScheduleTransitionStruct setCoolingSetpointSelector (toNSNumber value)

-- | @- heatingSetpoint@
heatingSetpoint :: IsMTRThermostatClusterScheduleTransitionStruct mtrThermostatClusterScheduleTransitionStruct => mtrThermostatClusterScheduleTransitionStruct -> IO (Id NSNumber)
heatingSetpoint mtrThermostatClusterScheduleTransitionStruct =
  sendMessage mtrThermostatClusterScheduleTransitionStruct heatingSetpointSelector

-- | @- setHeatingSetpoint:@
setHeatingSetpoint :: (IsMTRThermostatClusterScheduleTransitionStruct mtrThermostatClusterScheduleTransitionStruct, IsNSNumber value) => mtrThermostatClusterScheduleTransitionStruct -> value -> IO ()
setHeatingSetpoint mtrThermostatClusterScheduleTransitionStruct value =
  sendMessage mtrThermostatClusterScheduleTransitionStruct setHeatingSetpointSelector (toNSNumber value)

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @dayOfWeek@
dayOfWeekSelector :: Selector '[] (Id NSNumber)
dayOfWeekSelector = mkSelector "dayOfWeek"

-- | @Selector@ for @setDayOfWeek:@
setDayOfWeekSelector :: Selector '[Id NSNumber] ()
setDayOfWeekSelector = mkSelector "setDayOfWeek:"

-- | @Selector@ for @transitionTime@
transitionTimeSelector :: Selector '[] (Id NSNumber)
transitionTimeSelector = mkSelector "transitionTime"

-- | @Selector@ for @setTransitionTime:@
setTransitionTimeSelector :: Selector '[Id NSNumber] ()
setTransitionTimeSelector = mkSelector "setTransitionTime:"

-- | @Selector@ for @presetHandle@
presetHandleSelector :: Selector '[] (Id NSData)
presetHandleSelector = mkSelector "presetHandle"

-- | @Selector@ for @setPresetHandle:@
setPresetHandleSelector :: Selector '[Id NSData] ()
setPresetHandleSelector = mkSelector "setPresetHandle:"

-- | @Selector@ for @systemMode@
systemModeSelector :: Selector '[] (Id NSNumber)
systemModeSelector = mkSelector "systemMode"

-- | @Selector@ for @setSystemMode:@
setSystemModeSelector :: Selector '[Id NSNumber] ()
setSystemModeSelector = mkSelector "setSystemMode:"

-- | @Selector@ for @coolingSetpoint@
coolingSetpointSelector :: Selector '[] (Id NSNumber)
coolingSetpointSelector = mkSelector "coolingSetpoint"

-- | @Selector@ for @setCoolingSetpoint:@
setCoolingSetpointSelector :: Selector '[Id NSNumber] ()
setCoolingSetpointSelector = mkSelector "setCoolingSetpoint:"

-- | @Selector@ for @heatingSetpoint@
heatingSetpointSelector :: Selector '[] (Id NSNumber)
heatingSetpointSelector = mkSelector "heatingSetpoint"

-- | @Selector@ for @setHeatingSetpoint:@
setHeatingSetpointSelector :: Selector '[Id NSNumber] ()
setHeatingSetpointSelector = mkSelector "setHeatingSetpoint:"

