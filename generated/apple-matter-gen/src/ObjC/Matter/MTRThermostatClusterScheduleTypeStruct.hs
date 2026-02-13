{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Generated bindings for @MTRThermostatClusterScheduleTypeStruct@.
module ObjC.Matter.MTRThermostatClusterScheduleTypeStruct
  ( MTRThermostatClusterScheduleTypeStruct
  , IsMTRThermostatClusterScheduleTypeStruct(..)
  , systemMode
  , setSystemMode
  , numberOfSchedules
  , setNumberOfSchedules
  , scheduleTypeFeatures
  , setScheduleTypeFeatures
  , numberOfSchedulesSelector
  , scheduleTypeFeaturesSelector
  , setNumberOfSchedulesSelector
  , setScheduleTypeFeaturesSelector
  , setSystemModeSelector
  , systemModeSelector


  ) where

import Foreign.Ptr (Ptr, FunPtr)
import Foreign.C.Types

import ObjC.Runtime.Types
import ObjC.Runtime.Message (sendMessage, sendOwnedMessage, sendClassMessage, sendOwnedClassMessage)
import ObjC.Runtime.Selector (mkSelector)
import ObjC.Runtime.Class (getRequiredClass)

import ObjC.Matter.Internal.Classes
import ObjC.Foundation.Internal.Classes

-- | @- systemMode@
systemMode :: IsMTRThermostatClusterScheduleTypeStruct mtrThermostatClusterScheduleTypeStruct => mtrThermostatClusterScheduleTypeStruct -> IO (Id NSNumber)
systemMode mtrThermostatClusterScheduleTypeStruct =
  sendMessage mtrThermostatClusterScheduleTypeStruct systemModeSelector

-- | @- setSystemMode:@
setSystemMode :: (IsMTRThermostatClusterScheduleTypeStruct mtrThermostatClusterScheduleTypeStruct, IsNSNumber value) => mtrThermostatClusterScheduleTypeStruct -> value -> IO ()
setSystemMode mtrThermostatClusterScheduleTypeStruct value =
  sendMessage mtrThermostatClusterScheduleTypeStruct setSystemModeSelector (toNSNumber value)

-- | @- numberOfSchedules@
numberOfSchedules :: IsMTRThermostatClusterScheduleTypeStruct mtrThermostatClusterScheduleTypeStruct => mtrThermostatClusterScheduleTypeStruct -> IO (Id NSNumber)
numberOfSchedules mtrThermostatClusterScheduleTypeStruct =
  sendMessage mtrThermostatClusterScheduleTypeStruct numberOfSchedulesSelector

-- | @- setNumberOfSchedules:@
setNumberOfSchedules :: (IsMTRThermostatClusterScheduleTypeStruct mtrThermostatClusterScheduleTypeStruct, IsNSNumber value) => mtrThermostatClusterScheduleTypeStruct -> value -> IO ()
setNumberOfSchedules mtrThermostatClusterScheduleTypeStruct value =
  sendMessage mtrThermostatClusterScheduleTypeStruct setNumberOfSchedulesSelector (toNSNumber value)

-- | @- scheduleTypeFeatures@
scheduleTypeFeatures :: IsMTRThermostatClusterScheduleTypeStruct mtrThermostatClusterScheduleTypeStruct => mtrThermostatClusterScheduleTypeStruct -> IO (Id NSNumber)
scheduleTypeFeatures mtrThermostatClusterScheduleTypeStruct =
  sendMessage mtrThermostatClusterScheduleTypeStruct scheduleTypeFeaturesSelector

-- | @- setScheduleTypeFeatures:@
setScheduleTypeFeatures :: (IsMTRThermostatClusterScheduleTypeStruct mtrThermostatClusterScheduleTypeStruct, IsNSNumber value) => mtrThermostatClusterScheduleTypeStruct -> value -> IO ()
setScheduleTypeFeatures mtrThermostatClusterScheduleTypeStruct value =
  sendMessage mtrThermostatClusterScheduleTypeStruct setScheduleTypeFeaturesSelector (toNSNumber value)

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @systemMode@
systemModeSelector :: Selector '[] (Id NSNumber)
systemModeSelector = mkSelector "systemMode"

-- | @Selector@ for @setSystemMode:@
setSystemModeSelector :: Selector '[Id NSNumber] ()
setSystemModeSelector = mkSelector "setSystemMode:"

-- | @Selector@ for @numberOfSchedules@
numberOfSchedulesSelector :: Selector '[] (Id NSNumber)
numberOfSchedulesSelector = mkSelector "numberOfSchedules"

-- | @Selector@ for @setNumberOfSchedules:@
setNumberOfSchedulesSelector :: Selector '[Id NSNumber] ()
setNumberOfSchedulesSelector = mkSelector "setNumberOfSchedules:"

-- | @Selector@ for @scheduleTypeFeatures@
scheduleTypeFeaturesSelector :: Selector '[] (Id NSNumber)
scheduleTypeFeaturesSelector = mkSelector "scheduleTypeFeatures"

-- | @Selector@ for @setScheduleTypeFeatures:@
setScheduleTypeFeaturesSelector :: Selector '[Id NSNumber] ()
setScheduleTypeFeaturesSelector = mkSelector "setScheduleTypeFeatures:"

