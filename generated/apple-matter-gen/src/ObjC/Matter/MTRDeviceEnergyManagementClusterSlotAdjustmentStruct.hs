{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Generated bindings for @MTRDeviceEnergyManagementClusterSlotAdjustmentStruct@.
module ObjC.Matter.MTRDeviceEnergyManagementClusterSlotAdjustmentStruct
  ( MTRDeviceEnergyManagementClusterSlotAdjustmentStruct
  , IsMTRDeviceEnergyManagementClusterSlotAdjustmentStruct(..)
  , slotIndex
  , setSlotIndex
  , nominalPower
  , setNominalPower
  , duration
  , setDuration
  , durationSelector
  , nominalPowerSelector
  , setDurationSelector
  , setNominalPowerSelector
  , setSlotIndexSelector
  , slotIndexSelector


  ) where

import Foreign.Ptr (Ptr, FunPtr)
import Foreign.C.Types

import ObjC.Runtime.Types
import ObjC.Runtime.Message (sendMessage, sendOwnedMessage, sendClassMessage, sendOwnedClassMessage)
import ObjC.Runtime.Selector (mkSelector)
import ObjC.Runtime.Class (getRequiredClass)

import ObjC.Matter.Internal.Classes
import ObjC.Foundation.Internal.Classes

-- | @- slotIndex@
slotIndex :: IsMTRDeviceEnergyManagementClusterSlotAdjustmentStruct mtrDeviceEnergyManagementClusterSlotAdjustmentStruct => mtrDeviceEnergyManagementClusterSlotAdjustmentStruct -> IO (Id NSNumber)
slotIndex mtrDeviceEnergyManagementClusterSlotAdjustmentStruct =
  sendMessage mtrDeviceEnergyManagementClusterSlotAdjustmentStruct slotIndexSelector

-- | @- setSlotIndex:@
setSlotIndex :: (IsMTRDeviceEnergyManagementClusterSlotAdjustmentStruct mtrDeviceEnergyManagementClusterSlotAdjustmentStruct, IsNSNumber value) => mtrDeviceEnergyManagementClusterSlotAdjustmentStruct -> value -> IO ()
setSlotIndex mtrDeviceEnergyManagementClusterSlotAdjustmentStruct value =
  sendMessage mtrDeviceEnergyManagementClusterSlotAdjustmentStruct setSlotIndexSelector (toNSNumber value)

-- | @- nominalPower@
nominalPower :: IsMTRDeviceEnergyManagementClusterSlotAdjustmentStruct mtrDeviceEnergyManagementClusterSlotAdjustmentStruct => mtrDeviceEnergyManagementClusterSlotAdjustmentStruct -> IO (Id NSNumber)
nominalPower mtrDeviceEnergyManagementClusterSlotAdjustmentStruct =
  sendMessage mtrDeviceEnergyManagementClusterSlotAdjustmentStruct nominalPowerSelector

-- | @- setNominalPower:@
setNominalPower :: (IsMTRDeviceEnergyManagementClusterSlotAdjustmentStruct mtrDeviceEnergyManagementClusterSlotAdjustmentStruct, IsNSNumber value) => mtrDeviceEnergyManagementClusterSlotAdjustmentStruct -> value -> IO ()
setNominalPower mtrDeviceEnergyManagementClusterSlotAdjustmentStruct value =
  sendMessage mtrDeviceEnergyManagementClusterSlotAdjustmentStruct setNominalPowerSelector (toNSNumber value)

-- | @- duration@
duration :: IsMTRDeviceEnergyManagementClusterSlotAdjustmentStruct mtrDeviceEnergyManagementClusterSlotAdjustmentStruct => mtrDeviceEnergyManagementClusterSlotAdjustmentStruct -> IO (Id NSNumber)
duration mtrDeviceEnergyManagementClusterSlotAdjustmentStruct =
  sendMessage mtrDeviceEnergyManagementClusterSlotAdjustmentStruct durationSelector

-- | @- setDuration:@
setDuration :: (IsMTRDeviceEnergyManagementClusterSlotAdjustmentStruct mtrDeviceEnergyManagementClusterSlotAdjustmentStruct, IsNSNumber value) => mtrDeviceEnergyManagementClusterSlotAdjustmentStruct -> value -> IO ()
setDuration mtrDeviceEnergyManagementClusterSlotAdjustmentStruct value =
  sendMessage mtrDeviceEnergyManagementClusterSlotAdjustmentStruct setDurationSelector (toNSNumber value)

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @slotIndex@
slotIndexSelector :: Selector '[] (Id NSNumber)
slotIndexSelector = mkSelector "slotIndex"

-- | @Selector@ for @setSlotIndex:@
setSlotIndexSelector :: Selector '[Id NSNumber] ()
setSlotIndexSelector = mkSelector "setSlotIndex:"

-- | @Selector@ for @nominalPower@
nominalPowerSelector :: Selector '[] (Id NSNumber)
nominalPowerSelector = mkSelector "nominalPower"

-- | @Selector@ for @setNominalPower:@
setNominalPowerSelector :: Selector '[Id NSNumber] ()
setNominalPowerSelector = mkSelector "setNominalPower:"

-- | @Selector@ for @duration@
durationSelector :: Selector '[] (Id NSNumber)
durationSelector = mkSelector "duration"

-- | @Selector@ for @setDuration:@
setDurationSelector :: Selector '[Id NSNumber] ()
setDurationSelector = mkSelector "setDuration:"

