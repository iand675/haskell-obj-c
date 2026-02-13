{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Generated bindings for @MTRDeviceEnergyManagementClusterPowerAdjustCapabilityStruct@.
module ObjC.Matter.MTRDeviceEnergyManagementClusterPowerAdjustCapabilityStruct
  ( MTRDeviceEnergyManagementClusterPowerAdjustCapabilityStruct
  , IsMTRDeviceEnergyManagementClusterPowerAdjustCapabilityStruct(..)
  , powerAdjustCapability
  , setPowerAdjustCapability
  , cause
  , setCause
  , causeSelector
  , powerAdjustCapabilitySelector
  , setCauseSelector
  , setPowerAdjustCapabilitySelector


  ) where

import Foreign.Ptr (Ptr, FunPtr)
import Foreign.C.Types

import ObjC.Runtime.Types
import ObjC.Runtime.Message (sendMessage, sendOwnedMessage, sendClassMessage, sendOwnedClassMessage)
import ObjC.Runtime.Selector (mkSelector)
import ObjC.Runtime.Class (getRequiredClass)

import ObjC.Matter.Internal.Classes
import ObjC.Foundation.Internal.Classes

-- | @- powerAdjustCapability@
powerAdjustCapability :: IsMTRDeviceEnergyManagementClusterPowerAdjustCapabilityStruct mtrDeviceEnergyManagementClusterPowerAdjustCapabilityStruct => mtrDeviceEnergyManagementClusterPowerAdjustCapabilityStruct -> IO (Id NSArray)
powerAdjustCapability mtrDeviceEnergyManagementClusterPowerAdjustCapabilityStruct =
  sendMessage mtrDeviceEnergyManagementClusterPowerAdjustCapabilityStruct powerAdjustCapabilitySelector

-- | @- setPowerAdjustCapability:@
setPowerAdjustCapability :: (IsMTRDeviceEnergyManagementClusterPowerAdjustCapabilityStruct mtrDeviceEnergyManagementClusterPowerAdjustCapabilityStruct, IsNSArray value) => mtrDeviceEnergyManagementClusterPowerAdjustCapabilityStruct -> value -> IO ()
setPowerAdjustCapability mtrDeviceEnergyManagementClusterPowerAdjustCapabilityStruct value =
  sendMessage mtrDeviceEnergyManagementClusterPowerAdjustCapabilityStruct setPowerAdjustCapabilitySelector (toNSArray value)

-- | @- cause@
cause :: IsMTRDeviceEnergyManagementClusterPowerAdjustCapabilityStruct mtrDeviceEnergyManagementClusterPowerAdjustCapabilityStruct => mtrDeviceEnergyManagementClusterPowerAdjustCapabilityStruct -> IO (Id NSNumber)
cause mtrDeviceEnergyManagementClusterPowerAdjustCapabilityStruct =
  sendMessage mtrDeviceEnergyManagementClusterPowerAdjustCapabilityStruct causeSelector

-- | @- setCause:@
setCause :: (IsMTRDeviceEnergyManagementClusterPowerAdjustCapabilityStruct mtrDeviceEnergyManagementClusterPowerAdjustCapabilityStruct, IsNSNumber value) => mtrDeviceEnergyManagementClusterPowerAdjustCapabilityStruct -> value -> IO ()
setCause mtrDeviceEnergyManagementClusterPowerAdjustCapabilityStruct value =
  sendMessage mtrDeviceEnergyManagementClusterPowerAdjustCapabilityStruct setCauseSelector (toNSNumber value)

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @powerAdjustCapability@
powerAdjustCapabilitySelector :: Selector '[] (Id NSArray)
powerAdjustCapabilitySelector = mkSelector "powerAdjustCapability"

-- | @Selector@ for @setPowerAdjustCapability:@
setPowerAdjustCapabilitySelector :: Selector '[Id NSArray] ()
setPowerAdjustCapabilitySelector = mkSelector "setPowerAdjustCapability:"

-- | @Selector@ for @cause@
causeSelector :: Selector '[] (Id NSNumber)
causeSelector = mkSelector "cause"

-- | @Selector@ for @setCause:@
setCauseSelector :: Selector '[Id NSNumber] ()
setCauseSelector = mkSelector "setCause:"

