{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Generated bindings for @MTRDeviceEnergyManagementClusterPowerAdjustEndEvent@.
module ObjC.Matter.MTRDeviceEnergyManagementClusterPowerAdjustEndEvent
  ( MTRDeviceEnergyManagementClusterPowerAdjustEndEvent
  , IsMTRDeviceEnergyManagementClusterPowerAdjustEndEvent(..)
  , cause
  , setCause
  , duration
  , setDuration
  , energyUse
  , setEnergyUse
  , causeSelector
  , durationSelector
  , energyUseSelector
  , setCauseSelector
  , setDurationSelector
  , setEnergyUseSelector


  ) where

import Foreign.Ptr (Ptr, FunPtr)
import Foreign.C.Types

import ObjC.Runtime.Types
import ObjC.Runtime.Message (sendMessage, sendOwnedMessage, sendClassMessage, sendOwnedClassMessage)
import ObjC.Runtime.Selector (mkSelector)
import ObjC.Runtime.Class (getRequiredClass)

import ObjC.Matter.Internal.Classes
import ObjC.Foundation.Internal.Classes

-- | @- cause@
cause :: IsMTRDeviceEnergyManagementClusterPowerAdjustEndEvent mtrDeviceEnergyManagementClusterPowerAdjustEndEvent => mtrDeviceEnergyManagementClusterPowerAdjustEndEvent -> IO (Id NSNumber)
cause mtrDeviceEnergyManagementClusterPowerAdjustEndEvent =
  sendMessage mtrDeviceEnergyManagementClusterPowerAdjustEndEvent causeSelector

-- | @- setCause:@
setCause :: (IsMTRDeviceEnergyManagementClusterPowerAdjustEndEvent mtrDeviceEnergyManagementClusterPowerAdjustEndEvent, IsNSNumber value) => mtrDeviceEnergyManagementClusterPowerAdjustEndEvent -> value -> IO ()
setCause mtrDeviceEnergyManagementClusterPowerAdjustEndEvent value =
  sendMessage mtrDeviceEnergyManagementClusterPowerAdjustEndEvent setCauseSelector (toNSNumber value)

-- | @- duration@
duration :: IsMTRDeviceEnergyManagementClusterPowerAdjustEndEvent mtrDeviceEnergyManagementClusterPowerAdjustEndEvent => mtrDeviceEnergyManagementClusterPowerAdjustEndEvent -> IO (Id NSNumber)
duration mtrDeviceEnergyManagementClusterPowerAdjustEndEvent =
  sendMessage mtrDeviceEnergyManagementClusterPowerAdjustEndEvent durationSelector

-- | @- setDuration:@
setDuration :: (IsMTRDeviceEnergyManagementClusterPowerAdjustEndEvent mtrDeviceEnergyManagementClusterPowerAdjustEndEvent, IsNSNumber value) => mtrDeviceEnergyManagementClusterPowerAdjustEndEvent -> value -> IO ()
setDuration mtrDeviceEnergyManagementClusterPowerAdjustEndEvent value =
  sendMessage mtrDeviceEnergyManagementClusterPowerAdjustEndEvent setDurationSelector (toNSNumber value)

-- | @- energyUse@
energyUse :: IsMTRDeviceEnergyManagementClusterPowerAdjustEndEvent mtrDeviceEnergyManagementClusterPowerAdjustEndEvent => mtrDeviceEnergyManagementClusterPowerAdjustEndEvent -> IO (Id NSNumber)
energyUse mtrDeviceEnergyManagementClusterPowerAdjustEndEvent =
  sendMessage mtrDeviceEnergyManagementClusterPowerAdjustEndEvent energyUseSelector

-- | @- setEnergyUse:@
setEnergyUse :: (IsMTRDeviceEnergyManagementClusterPowerAdjustEndEvent mtrDeviceEnergyManagementClusterPowerAdjustEndEvent, IsNSNumber value) => mtrDeviceEnergyManagementClusterPowerAdjustEndEvent -> value -> IO ()
setEnergyUse mtrDeviceEnergyManagementClusterPowerAdjustEndEvent value =
  sendMessage mtrDeviceEnergyManagementClusterPowerAdjustEndEvent setEnergyUseSelector (toNSNumber value)

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @cause@
causeSelector :: Selector '[] (Id NSNumber)
causeSelector = mkSelector "cause"

-- | @Selector@ for @setCause:@
setCauseSelector :: Selector '[Id NSNumber] ()
setCauseSelector = mkSelector "setCause:"

-- | @Selector@ for @duration@
durationSelector :: Selector '[] (Id NSNumber)
durationSelector = mkSelector "duration"

-- | @Selector@ for @setDuration:@
setDurationSelector :: Selector '[Id NSNumber] ()
setDurationSelector = mkSelector "setDuration:"

-- | @Selector@ for @energyUse@
energyUseSelector :: Selector '[] (Id NSNumber)
energyUseSelector = mkSelector "energyUse"

-- | @Selector@ for @setEnergyUse:@
setEnergyUseSelector :: Selector '[Id NSNumber] ()
setEnergyUseSelector = mkSelector "setEnergyUse:"

