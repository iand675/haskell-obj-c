{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Generated bindings for @MTRThermostatClusterSetpointChangeEvent@.
module ObjC.Matter.MTRThermostatClusterSetpointChangeEvent
  ( MTRThermostatClusterSetpointChangeEvent
  , IsMTRThermostatClusterSetpointChangeEvent(..)
  , systemMode
  , setSystemMode
  , occupancy
  , setOccupancy
  , previousSetpoint
  , setPreviousSetpoint
  , currentSetpoint
  , setCurrentSetpoint
  , currentSetpointSelector
  , occupancySelector
  , previousSetpointSelector
  , setCurrentSetpointSelector
  , setOccupancySelector
  , setPreviousSetpointSelector
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
systemMode :: IsMTRThermostatClusterSetpointChangeEvent mtrThermostatClusterSetpointChangeEvent => mtrThermostatClusterSetpointChangeEvent -> IO (Id NSNumber)
systemMode mtrThermostatClusterSetpointChangeEvent =
  sendMessage mtrThermostatClusterSetpointChangeEvent systemModeSelector

-- | @- setSystemMode:@
setSystemMode :: (IsMTRThermostatClusterSetpointChangeEvent mtrThermostatClusterSetpointChangeEvent, IsNSNumber value) => mtrThermostatClusterSetpointChangeEvent -> value -> IO ()
setSystemMode mtrThermostatClusterSetpointChangeEvent value =
  sendMessage mtrThermostatClusterSetpointChangeEvent setSystemModeSelector (toNSNumber value)

-- | @- occupancy@
occupancy :: IsMTRThermostatClusterSetpointChangeEvent mtrThermostatClusterSetpointChangeEvent => mtrThermostatClusterSetpointChangeEvent -> IO (Id NSNumber)
occupancy mtrThermostatClusterSetpointChangeEvent =
  sendMessage mtrThermostatClusterSetpointChangeEvent occupancySelector

-- | @- setOccupancy:@
setOccupancy :: (IsMTRThermostatClusterSetpointChangeEvent mtrThermostatClusterSetpointChangeEvent, IsNSNumber value) => mtrThermostatClusterSetpointChangeEvent -> value -> IO ()
setOccupancy mtrThermostatClusterSetpointChangeEvent value =
  sendMessage mtrThermostatClusterSetpointChangeEvent setOccupancySelector (toNSNumber value)

-- | @- previousSetpoint@
previousSetpoint :: IsMTRThermostatClusterSetpointChangeEvent mtrThermostatClusterSetpointChangeEvent => mtrThermostatClusterSetpointChangeEvent -> IO (Id NSNumber)
previousSetpoint mtrThermostatClusterSetpointChangeEvent =
  sendMessage mtrThermostatClusterSetpointChangeEvent previousSetpointSelector

-- | @- setPreviousSetpoint:@
setPreviousSetpoint :: (IsMTRThermostatClusterSetpointChangeEvent mtrThermostatClusterSetpointChangeEvent, IsNSNumber value) => mtrThermostatClusterSetpointChangeEvent -> value -> IO ()
setPreviousSetpoint mtrThermostatClusterSetpointChangeEvent value =
  sendMessage mtrThermostatClusterSetpointChangeEvent setPreviousSetpointSelector (toNSNumber value)

-- | @- currentSetpoint@
currentSetpoint :: IsMTRThermostatClusterSetpointChangeEvent mtrThermostatClusterSetpointChangeEvent => mtrThermostatClusterSetpointChangeEvent -> IO (Id NSNumber)
currentSetpoint mtrThermostatClusterSetpointChangeEvent =
  sendMessage mtrThermostatClusterSetpointChangeEvent currentSetpointSelector

-- | @- setCurrentSetpoint:@
setCurrentSetpoint :: (IsMTRThermostatClusterSetpointChangeEvent mtrThermostatClusterSetpointChangeEvent, IsNSNumber value) => mtrThermostatClusterSetpointChangeEvent -> value -> IO ()
setCurrentSetpoint mtrThermostatClusterSetpointChangeEvent value =
  sendMessage mtrThermostatClusterSetpointChangeEvent setCurrentSetpointSelector (toNSNumber value)

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @systemMode@
systemModeSelector :: Selector '[] (Id NSNumber)
systemModeSelector = mkSelector "systemMode"

-- | @Selector@ for @setSystemMode:@
setSystemModeSelector :: Selector '[Id NSNumber] ()
setSystemModeSelector = mkSelector "setSystemMode:"

-- | @Selector@ for @occupancy@
occupancySelector :: Selector '[] (Id NSNumber)
occupancySelector = mkSelector "occupancy"

-- | @Selector@ for @setOccupancy:@
setOccupancySelector :: Selector '[Id NSNumber] ()
setOccupancySelector = mkSelector "setOccupancy:"

-- | @Selector@ for @previousSetpoint@
previousSetpointSelector :: Selector '[] (Id NSNumber)
previousSetpointSelector = mkSelector "previousSetpoint"

-- | @Selector@ for @setPreviousSetpoint:@
setPreviousSetpointSelector :: Selector '[Id NSNumber] ()
setPreviousSetpointSelector = mkSelector "setPreviousSetpoint:"

-- | @Selector@ for @currentSetpoint@
currentSetpointSelector :: Selector '[] (Id NSNumber)
currentSetpointSelector = mkSelector "currentSetpoint"

-- | @Selector@ for @setCurrentSetpoint:@
setCurrentSetpointSelector :: Selector '[Id NSNumber] ()
setCurrentSetpointSelector = mkSelector "setCurrentSetpoint:"

