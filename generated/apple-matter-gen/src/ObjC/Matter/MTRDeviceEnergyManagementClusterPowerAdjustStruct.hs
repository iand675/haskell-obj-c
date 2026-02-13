{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Generated bindings for @MTRDeviceEnergyManagementClusterPowerAdjustStruct@.
module ObjC.Matter.MTRDeviceEnergyManagementClusterPowerAdjustStruct
  ( MTRDeviceEnergyManagementClusterPowerAdjustStruct
  , IsMTRDeviceEnergyManagementClusterPowerAdjustStruct(..)
  , minPower
  , setMinPower
  , maxPower
  , setMaxPower
  , minDuration
  , setMinDuration
  , maxDuration
  , setMaxDuration
  , maxDurationSelector
  , maxPowerSelector
  , minDurationSelector
  , minPowerSelector
  , setMaxDurationSelector
  , setMaxPowerSelector
  , setMinDurationSelector
  , setMinPowerSelector


  ) where

import Foreign.Ptr (Ptr, FunPtr)
import Foreign.C.Types

import ObjC.Runtime.Types
import ObjC.Runtime.Message (sendMessage, sendOwnedMessage, sendClassMessage, sendOwnedClassMessage)
import ObjC.Runtime.Selector (mkSelector)
import ObjC.Runtime.Class (getRequiredClass)

import ObjC.Matter.Internal.Classes
import ObjC.Foundation.Internal.Classes

-- | @- minPower@
minPower :: IsMTRDeviceEnergyManagementClusterPowerAdjustStruct mtrDeviceEnergyManagementClusterPowerAdjustStruct => mtrDeviceEnergyManagementClusterPowerAdjustStruct -> IO (Id NSNumber)
minPower mtrDeviceEnergyManagementClusterPowerAdjustStruct =
  sendMessage mtrDeviceEnergyManagementClusterPowerAdjustStruct minPowerSelector

-- | @- setMinPower:@
setMinPower :: (IsMTRDeviceEnergyManagementClusterPowerAdjustStruct mtrDeviceEnergyManagementClusterPowerAdjustStruct, IsNSNumber value) => mtrDeviceEnergyManagementClusterPowerAdjustStruct -> value -> IO ()
setMinPower mtrDeviceEnergyManagementClusterPowerAdjustStruct value =
  sendMessage mtrDeviceEnergyManagementClusterPowerAdjustStruct setMinPowerSelector (toNSNumber value)

-- | @- maxPower@
maxPower :: IsMTRDeviceEnergyManagementClusterPowerAdjustStruct mtrDeviceEnergyManagementClusterPowerAdjustStruct => mtrDeviceEnergyManagementClusterPowerAdjustStruct -> IO (Id NSNumber)
maxPower mtrDeviceEnergyManagementClusterPowerAdjustStruct =
  sendMessage mtrDeviceEnergyManagementClusterPowerAdjustStruct maxPowerSelector

-- | @- setMaxPower:@
setMaxPower :: (IsMTRDeviceEnergyManagementClusterPowerAdjustStruct mtrDeviceEnergyManagementClusterPowerAdjustStruct, IsNSNumber value) => mtrDeviceEnergyManagementClusterPowerAdjustStruct -> value -> IO ()
setMaxPower mtrDeviceEnergyManagementClusterPowerAdjustStruct value =
  sendMessage mtrDeviceEnergyManagementClusterPowerAdjustStruct setMaxPowerSelector (toNSNumber value)

-- | @- minDuration@
minDuration :: IsMTRDeviceEnergyManagementClusterPowerAdjustStruct mtrDeviceEnergyManagementClusterPowerAdjustStruct => mtrDeviceEnergyManagementClusterPowerAdjustStruct -> IO (Id NSNumber)
minDuration mtrDeviceEnergyManagementClusterPowerAdjustStruct =
  sendMessage mtrDeviceEnergyManagementClusterPowerAdjustStruct minDurationSelector

-- | @- setMinDuration:@
setMinDuration :: (IsMTRDeviceEnergyManagementClusterPowerAdjustStruct mtrDeviceEnergyManagementClusterPowerAdjustStruct, IsNSNumber value) => mtrDeviceEnergyManagementClusterPowerAdjustStruct -> value -> IO ()
setMinDuration mtrDeviceEnergyManagementClusterPowerAdjustStruct value =
  sendMessage mtrDeviceEnergyManagementClusterPowerAdjustStruct setMinDurationSelector (toNSNumber value)

-- | @- maxDuration@
maxDuration :: IsMTRDeviceEnergyManagementClusterPowerAdjustStruct mtrDeviceEnergyManagementClusterPowerAdjustStruct => mtrDeviceEnergyManagementClusterPowerAdjustStruct -> IO (Id NSNumber)
maxDuration mtrDeviceEnergyManagementClusterPowerAdjustStruct =
  sendMessage mtrDeviceEnergyManagementClusterPowerAdjustStruct maxDurationSelector

-- | @- setMaxDuration:@
setMaxDuration :: (IsMTRDeviceEnergyManagementClusterPowerAdjustStruct mtrDeviceEnergyManagementClusterPowerAdjustStruct, IsNSNumber value) => mtrDeviceEnergyManagementClusterPowerAdjustStruct -> value -> IO ()
setMaxDuration mtrDeviceEnergyManagementClusterPowerAdjustStruct value =
  sendMessage mtrDeviceEnergyManagementClusterPowerAdjustStruct setMaxDurationSelector (toNSNumber value)

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @minPower@
minPowerSelector :: Selector '[] (Id NSNumber)
minPowerSelector = mkSelector "minPower"

-- | @Selector@ for @setMinPower:@
setMinPowerSelector :: Selector '[Id NSNumber] ()
setMinPowerSelector = mkSelector "setMinPower:"

-- | @Selector@ for @maxPower@
maxPowerSelector :: Selector '[] (Id NSNumber)
maxPowerSelector = mkSelector "maxPower"

-- | @Selector@ for @setMaxPower:@
setMaxPowerSelector :: Selector '[Id NSNumber] ()
setMaxPowerSelector = mkSelector "setMaxPower:"

-- | @Selector@ for @minDuration@
minDurationSelector :: Selector '[] (Id NSNumber)
minDurationSelector = mkSelector "minDuration"

-- | @Selector@ for @setMinDuration:@
setMinDurationSelector :: Selector '[Id NSNumber] ()
setMinDurationSelector = mkSelector "setMinDuration:"

-- | @Selector@ for @maxDuration@
maxDurationSelector :: Selector '[] (Id NSNumber)
maxDurationSelector = mkSelector "maxDuration"

-- | @Selector@ for @setMaxDuration:@
setMaxDurationSelector :: Selector '[Id NSNumber] ()
setMaxDurationSelector = mkSelector "setMaxDuration:"

