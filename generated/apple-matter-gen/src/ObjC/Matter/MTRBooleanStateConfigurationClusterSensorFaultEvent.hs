{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Generated bindings for @MTRBooleanStateConfigurationClusterSensorFaultEvent@.
module ObjC.Matter.MTRBooleanStateConfigurationClusterSensorFaultEvent
  ( MTRBooleanStateConfigurationClusterSensorFaultEvent
  , IsMTRBooleanStateConfigurationClusterSensorFaultEvent(..)
  , sensorFault
  , setSensorFault
  , sensorFaultSelector
  , setSensorFaultSelector


  ) where

import Foreign.Ptr (Ptr, FunPtr)
import Foreign.C.Types

import ObjC.Runtime.Types
import ObjC.Runtime.Message (sendMessage, sendOwnedMessage, sendClassMessage, sendOwnedClassMessage)
import ObjC.Runtime.Selector (mkSelector)
import ObjC.Runtime.Class (getRequiredClass)

import ObjC.Matter.Internal.Classes
import ObjC.Foundation.Internal.Classes

-- | @- sensorFault@
sensorFault :: IsMTRBooleanStateConfigurationClusterSensorFaultEvent mtrBooleanStateConfigurationClusterSensorFaultEvent => mtrBooleanStateConfigurationClusterSensorFaultEvent -> IO (Id NSNumber)
sensorFault mtrBooleanStateConfigurationClusterSensorFaultEvent =
  sendMessage mtrBooleanStateConfigurationClusterSensorFaultEvent sensorFaultSelector

-- | @- setSensorFault:@
setSensorFault :: (IsMTRBooleanStateConfigurationClusterSensorFaultEvent mtrBooleanStateConfigurationClusterSensorFaultEvent, IsNSNumber value) => mtrBooleanStateConfigurationClusterSensorFaultEvent -> value -> IO ()
setSensorFault mtrBooleanStateConfigurationClusterSensorFaultEvent value =
  sendMessage mtrBooleanStateConfigurationClusterSensorFaultEvent setSensorFaultSelector (toNSNumber value)

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @sensorFault@
sensorFaultSelector :: Selector '[] (Id NSNumber)
sensorFaultSelector = mkSelector "sensorFault"

-- | @Selector@ for @setSensorFault:@
setSensorFaultSelector :: Selector '[Id NSNumber] ()
setSensorFaultSelector = mkSelector "setSensorFault:"

