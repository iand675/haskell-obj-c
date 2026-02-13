{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Generated bindings for @MTRDeviceEnergyManagementClusterConstraintsStruct@.
module ObjC.Matter.MTRDeviceEnergyManagementClusterConstraintsStruct
  ( MTRDeviceEnergyManagementClusterConstraintsStruct
  , IsMTRDeviceEnergyManagementClusterConstraintsStruct(..)
  , startTime
  , setStartTime
  , duration
  , setDuration
  , nominalPower
  , setNominalPower
  , maximumEnergy
  , setMaximumEnergy
  , loadControl
  , setLoadControl
  , durationSelector
  , loadControlSelector
  , maximumEnergySelector
  , nominalPowerSelector
  , setDurationSelector
  , setLoadControlSelector
  , setMaximumEnergySelector
  , setNominalPowerSelector
  , setStartTimeSelector
  , startTimeSelector


  ) where

import Foreign.Ptr (Ptr, FunPtr)
import Foreign.C.Types

import ObjC.Runtime.Types
import ObjC.Runtime.Message (sendMessage, sendOwnedMessage, sendClassMessage, sendOwnedClassMessage)
import ObjC.Runtime.Selector (mkSelector)
import ObjC.Runtime.Class (getRequiredClass)

import ObjC.Matter.Internal.Classes
import ObjC.Foundation.Internal.Classes

-- | @- startTime@
startTime :: IsMTRDeviceEnergyManagementClusterConstraintsStruct mtrDeviceEnergyManagementClusterConstraintsStruct => mtrDeviceEnergyManagementClusterConstraintsStruct -> IO (Id NSNumber)
startTime mtrDeviceEnergyManagementClusterConstraintsStruct =
  sendMessage mtrDeviceEnergyManagementClusterConstraintsStruct startTimeSelector

-- | @- setStartTime:@
setStartTime :: (IsMTRDeviceEnergyManagementClusterConstraintsStruct mtrDeviceEnergyManagementClusterConstraintsStruct, IsNSNumber value) => mtrDeviceEnergyManagementClusterConstraintsStruct -> value -> IO ()
setStartTime mtrDeviceEnergyManagementClusterConstraintsStruct value =
  sendMessage mtrDeviceEnergyManagementClusterConstraintsStruct setStartTimeSelector (toNSNumber value)

-- | @- duration@
duration :: IsMTRDeviceEnergyManagementClusterConstraintsStruct mtrDeviceEnergyManagementClusterConstraintsStruct => mtrDeviceEnergyManagementClusterConstraintsStruct -> IO (Id NSNumber)
duration mtrDeviceEnergyManagementClusterConstraintsStruct =
  sendMessage mtrDeviceEnergyManagementClusterConstraintsStruct durationSelector

-- | @- setDuration:@
setDuration :: (IsMTRDeviceEnergyManagementClusterConstraintsStruct mtrDeviceEnergyManagementClusterConstraintsStruct, IsNSNumber value) => mtrDeviceEnergyManagementClusterConstraintsStruct -> value -> IO ()
setDuration mtrDeviceEnergyManagementClusterConstraintsStruct value =
  sendMessage mtrDeviceEnergyManagementClusterConstraintsStruct setDurationSelector (toNSNumber value)

-- | @- nominalPower@
nominalPower :: IsMTRDeviceEnergyManagementClusterConstraintsStruct mtrDeviceEnergyManagementClusterConstraintsStruct => mtrDeviceEnergyManagementClusterConstraintsStruct -> IO (Id NSNumber)
nominalPower mtrDeviceEnergyManagementClusterConstraintsStruct =
  sendMessage mtrDeviceEnergyManagementClusterConstraintsStruct nominalPowerSelector

-- | @- setNominalPower:@
setNominalPower :: (IsMTRDeviceEnergyManagementClusterConstraintsStruct mtrDeviceEnergyManagementClusterConstraintsStruct, IsNSNumber value) => mtrDeviceEnergyManagementClusterConstraintsStruct -> value -> IO ()
setNominalPower mtrDeviceEnergyManagementClusterConstraintsStruct value =
  sendMessage mtrDeviceEnergyManagementClusterConstraintsStruct setNominalPowerSelector (toNSNumber value)

-- | @- maximumEnergy@
maximumEnergy :: IsMTRDeviceEnergyManagementClusterConstraintsStruct mtrDeviceEnergyManagementClusterConstraintsStruct => mtrDeviceEnergyManagementClusterConstraintsStruct -> IO (Id NSNumber)
maximumEnergy mtrDeviceEnergyManagementClusterConstraintsStruct =
  sendMessage mtrDeviceEnergyManagementClusterConstraintsStruct maximumEnergySelector

-- | @- setMaximumEnergy:@
setMaximumEnergy :: (IsMTRDeviceEnergyManagementClusterConstraintsStruct mtrDeviceEnergyManagementClusterConstraintsStruct, IsNSNumber value) => mtrDeviceEnergyManagementClusterConstraintsStruct -> value -> IO ()
setMaximumEnergy mtrDeviceEnergyManagementClusterConstraintsStruct value =
  sendMessage mtrDeviceEnergyManagementClusterConstraintsStruct setMaximumEnergySelector (toNSNumber value)

-- | @- loadControl@
loadControl :: IsMTRDeviceEnergyManagementClusterConstraintsStruct mtrDeviceEnergyManagementClusterConstraintsStruct => mtrDeviceEnergyManagementClusterConstraintsStruct -> IO (Id NSNumber)
loadControl mtrDeviceEnergyManagementClusterConstraintsStruct =
  sendMessage mtrDeviceEnergyManagementClusterConstraintsStruct loadControlSelector

-- | @- setLoadControl:@
setLoadControl :: (IsMTRDeviceEnergyManagementClusterConstraintsStruct mtrDeviceEnergyManagementClusterConstraintsStruct, IsNSNumber value) => mtrDeviceEnergyManagementClusterConstraintsStruct -> value -> IO ()
setLoadControl mtrDeviceEnergyManagementClusterConstraintsStruct value =
  sendMessage mtrDeviceEnergyManagementClusterConstraintsStruct setLoadControlSelector (toNSNumber value)

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @startTime@
startTimeSelector :: Selector '[] (Id NSNumber)
startTimeSelector = mkSelector "startTime"

-- | @Selector@ for @setStartTime:@
setStartTimeSelector :: Selector '[Id NSNumber] ()
setStartTimeSelector = mkSelector "setStartTime:"

-- | @Selector@ for @duration@
durationSelector :: Selector '[] (Id NSNumber)
durationSelector = mkSelector "duration"

-- | @Selector@ for @setDuration:@
setDurationSelector :: Selector '[Id NSNumber] ()
setDurationSelector = mkSelector "setDuration:"

-- | @Selector@ for @nominalPower@
nominalPowerSelector :: Selector '[] (Id NSNumber)
nominalPowerSelector = mkSelector "nominalPower"

-- | @Selector@ for @setNominalPower:@
setNominalPowerSelector :: Selector '[Id NSNumber] ()
setNominalPowerSelector = mkSelector "setNominalPower:"

-- | @Selector@ for @maximumEnergy@
maximumEnergySelector :: Selector '[] (Id NSNumber)
maximumEnergySelector = mkSelector "maximumEnergy"

-- | @Selector@ for @setMaximumEnergy:@
setMaximumEnergySelector :: Selector '[Id NSNumber] ()
setMaximumEnergySelector = mkSelector "setMaximumEnergy:"

-- | @Selector@ for @loadControl@
loadControlSelector :: Selector '[] (Id NSNumber)
loadControlSelector = mkSelector "loadControl"

-- | @Selector@ for @setLoadControl:@
setLoadControlSelector :: Selector '[Id NSNumber] ()
setLoadControlSelector = mkSelector "setLoadControl:"

