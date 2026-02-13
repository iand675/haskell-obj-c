{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Generated bindings for @MTRThermostatClusterOccupancyChangeEvent@.
module ObjC.Matter.MTRThermostatClusterOccupancyChangeEvent
  ( MTRThermostatClusterOccupancyChangeEvent
  , IsMTRThermostatClusterOccupancyChangeEvent(..)
  , previousOccupancy
  , setPreviousOccupancy
  , currentOccupancy
  , setCurrentOccupancy
  , currentOccupancySelector
  , previousOccupancySelector
  , setCurrentOccupancySelector
  , setPreviousOccupancySelector


  ) where

import Foreign.Ptr (Ptr, FunPtr)
import Foreign.C.Types

import ObjC.Runtime.Types
import ObjC.Runtime.Message (sendMessage, sendOwnedMessage, sendClassMessage, sendOwnedClassMessage)
import ObjC.Runtime.Selector (mkSelector)
import ObjC.Runtime.Class (getRequiredClass)

import ObjC.Matter.Internal.Classes
import ObjC.Foundation.Internal.Classes

-- | @- previousOccupancy@
previousOccupancy :: IsMTRThermostatClusterOccupancyChangeEvent mtrThermostatClusterOccupancyChangeEvent => mtrThermostatClusterOccupancyChangeEvent -> IO (Id NSNumber)
previousOccupancy mtrThermostatClusterOccupancyChangeEvent =
  sendMessage mtrThermostatClusterOccupancyChangeEvent previousOccupancySelector

-- | @- setPreviousOccupancy:@
setPreviousOccupancy :: (IsMTRThermostatClusterOccupancyChangeEvent mtrThermostatClusterOccupancyChangeEvent, IsNSNumber value) => mtrThermostatClusterOccupancyChangeEvent -> value -> IO ()
setPreviousOccupancy mtrThermostatClusterOccupancyChangeEvent value =
  sendMessage mtrThermostatClusterOccupancyChangeEvent setPreviousOccupancySelector (toNSNumber value)

-- | @- currentOccupancy@
currentOccupancy :: IsMTRThermostatClusterOccupancyChangeEvent mtrThermostatClusterOccupancyChangeEvent => mtrThermostatClusterOccupancyChangeEvent -> IO (Id NSNumber)
currentOccupancy mtrThermostatClusterOccupancyChangeEvent =
  sendMessage mtrThermostatClusterOccupancyChangeEvent currentOccupancySelector

-- | @- setCurrentOccupancy:@
setCurrentOccupancy :: (IsMTRThermostatClusterOccupancyChangeEvent mtrThermostatClusterOccupancyChangeEvent, IsNSNumber value) => mtrThermostatClusterOccupancyChangeEvent -> value -> IO ()
setCurrentOccupancy mtrThermostatClusterOccupancyChangeEvent value =
  sendMessage mtrThermostatClusterOccupancyChangeEvent setCurrentOccupancySelector (toNSNumber value)

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @previousOccupancy@
previousOccupancySelector :: Selector '[] (Id NSNumber)
previousOccupancySelector = mkSelector "previousOccupancy"

-- | @Selector@ for @setPreviousOccupancy:@
setPreviousOccupancySelector :: Selector '[Id NSNumber] ()
setPreviousOccupancySelector = mkSelector "setPreviousOccupancy:"

-- | @Selector@ for @currentOccupancy@
currentOccupancySelector :: Selector '[] (Id NSNumber)
currentOccupancySelector = mkSelector "currentOccupancy"

-- | @Selector@ for @setCurrentOccupancy:@
setCurrentOccupancySelector :: Selector '[Id NSNumber] ()
setCurrentOccupancySelector = mkSelector "setCurrentOccupancy:"

