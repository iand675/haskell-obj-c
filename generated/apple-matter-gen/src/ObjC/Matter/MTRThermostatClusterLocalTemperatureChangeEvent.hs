{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Generated bindings for @MTRThermostatClusterLocalTemperatureChangeEvent@.
module ObjC.Matter.MTRThermostatClusterLocalTemperatureChangeEvent
  ( MTRThermostatClusterLocalTemperatureChangeEvent
  , IsMTRThermostatClusterLocalTemperatureChangeEvent(..)
  , currentLocalTemperature
  , setCurrentLocalTemperature
  , currentLocalTemperatureSelector
  , setCurrentLocalTemperatureSelector


  ) where

import Foreign.Ptr (Ptr, FunPtr)
import Foreign.C.Types

import ObjC.Runtime.Types
import ObjC.Runtime.Message (sendMessage, sendOwnedMessage, sendClassMessage, sendOwnedClassMessage)
import ObjC.Runtime.Selector (mkSelector)
import ObjC.Runtime.Class (getRequiredClass)

import ObjC.Matter.Internal.Classes
import ObjC.Foundation.Internal.Classes

-- | @- currentLocalTemperature@
currentLocalTemperature :: IsMTRThermostatClusterLocalTemperatureChangeEvent mtrThermostatClusterLocalTemperatureChangeEvent => mtrThermostatClusterLocalTemperatureChangeEvent -> IO (Id NSNumber)
currentLocalTemperature mtrThermostatClusterLocalTemperatureChangeEvent =
  sendMessage mtrThermostatClusterLocalTemperatureChangeEvent currentLocalTemperatureSelector

-- | @- setCurrentLocalTemperature:@
setCurrentLocalTemperature :: (IsMTRThermostatClusterLocalTemperatureChangeEvent mtrThermostatClusterLocalTemperatureChangeEvent, IsNSNumber value) => mtrThermostatClusterLocalTemperatureChangeEvent -> value -> IO ()
setCurrentLocalTemperature mtrThermostatClusterLocalTemperatureChangeEvent value =
  sendMessage mtrThermostatClusterLocalTemperatureChangeEvent setCurrentLocalTemperatureSelector (toNSNumber value)

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @currentLocalTemperature@
currentLocalTemperatureSelector :: Selector '[] (Id NSNumber)
currentLocalTemperatureSelector = mkSelector "currentLocalTemperature"

-- | @Selector@ for @setCurrentLocalTemperature:@
setCurrentLocalTemperatureSelector :: Selector '[Id NSNumber] ()
setCurrentLocalTemperatureSelector = mkSelector "setCurrentLocalTemperature:"

