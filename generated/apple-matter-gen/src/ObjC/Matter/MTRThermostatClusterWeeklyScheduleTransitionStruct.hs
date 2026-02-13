{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Generated bindings for @MTRThermostatClusterWeeklyScheduleTransitionStruct@.
module ObjC.Matter.MTRThermostatClusterWeeklyScheduleTransitionStruct
  ( MTRThermostatClusterWeeklyScheduleTransitionStruct
  , IsMTRThermostatClusterWeeklyScheduleTransitionStruct(..)
  , transitionTime
  , setTransitionTime
  , heatSetpoint
  , setHeatSetpoint
  , coolSetpoint
  , setCoolSetpoint
  , coolSetpointSelector
  , heatSetpointSelector
  , setCoolSetpointSelector
  , setHeatSetpointSelector
  , setTransitionTimeSelector
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

-- | @- transitionTime@
transitionTime :: IsMTRThermostatClusterWeeklyScheduleTransitionStruct mtrThermostatClusterWeeklyScheduleTransitionStruct => mtrThermostatClusterWeeklyScheduleTransitionStruct -> IO (Id NSNumber)
transitionTime mtrThermostatClusterWeeklyScheduleTransitionStruct =
  sendMessage mtrThermostatClusterWeeklyScheduleTransitionStruct transitionTimeSelector

-- | @- setTransitionTime:@
setTransitionTime :: (IsMTRThermostatClusterWeeklyScheduleTransitionStruct mtrThermostatClusterWeeklyScheduleTransitionStruct, IsNSNumber value) => mtrThermostatClusterWeeklyScheduleTransitionStruct -> value -> IO ()
setTransitionTime mtrThermostatClusterWeeklyScheduleTransitionStruct value =
  sendMessage mtrThermostatClusterWeeklyScheduleTransitionStruct setTransitionTimeSelector (toNSNumber value)

-- | @- heatSetpoint@
heatSetpoint :: IsMTRThermostatClusterWeeklyScheduleTransitionStruct mtrThermostatClusterWeeklyScheduleTransitionStruct => mtrThermostatClusterWeeklyScheduleTransitionStruct -> IO (Id NSNumber)
heatSetpoint mtrThermostatClusterWeeklyScheduleTransitionStruct =
  sendMessage mtrThermostatClusterWeeklyScheduleTransitionStruct heatSetpointSelector

-- | @- setHeatSetpoint:@
setHeatSetpoint :: (IsMTRThermostatClusterWeeklyScheduleTransitionStruct mtrThermostatClusterWeeklyScheduleTransitionStruct, IsNSNumber value) => mtrThermostatClusterWeeklyScheduleTransitionStruct -> value -> IO ()
setHeatSetpoint mtrThermostatClusterWeeklyScheduleTransitionStruct value =
  sendMessage mtrThermostatClusterWeeklyScheduleTransitionStruct setHeatSetpointSelector (toNSNumber value)

-- | @- coolSetpoint@
coolSetpoint :: IsMTRThermostatClusterWeeklyScheduleTransitionStruct mtrThermostatClusterWeeklyScheduleTransitionStruct => mtrThermostatClusterWeeklyScheduleTransitionStruct -> IO (Id NSNumber)
coolSetpoint mtrThermostatClusterWeeklyScheduleTransitionStruct =
  sendMessage mtrThermostatClusterWeeklyScheduleTransitionStruct coolSetpointSelector

-- | @- setCoolSetpoint:@
setCoolSetpoint :: (IsMTRThermostatClusterWeeklyScheduleTransitionStruct mtrThermostatClusterWeeklyScheduleTransitionStruct, IsNSNumber value) => mtrThermostatClusterWeeklyScheduleTransitionStruct -> value -> IO ()
setCoolSetpoint mtrThermostatClusterWeeklyScheduleTransitionStruct value =
  sendMessage mtrThermostatClusterWeeklyScheduleTransitionStruct setCoolSetpointSelector (toNSNumber value)

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @transitionTime@
transitionTimeSelector :: Selector '[] (Id NSNumber)
transitionTimeSelector = mkSelector "transitionTime"

-- | @Selector@ for @setTransitionTime:@
setTransitionTimeSelector :: Selector '[Id NSNumber] ()
setTransitionTimeSelector = mkSelector "setTransitionTime:"

-- | @Selector@ for @heatSetpoint@
heatSetpointSelector :: Selector '[] (Id NSNumber)
heatSetpointSelector = mkSelector "heatSetpoint"

-- | @Selector@ for @setHeatSetpoint:@
setHeatSetpointSelector :: Selector '[Id NSNumber] ()
setHeatSetpointSelector = mkSelector "setHeatSetpoint:"

-- | @Selector@ for @coolSetpoint@
coolSetpointSelector :: Selector '[] (Id NSNumber)
coolSetpointSelector = mkSelector "coolSetpoint"

-- | @Selector@ for @setCoolSetpoint:@
setCoolSetpointSelector :: Selector '[Id NSNumber] ()
setCoolSetpointSelector = mkSelector "setCoolSetpoint:"

