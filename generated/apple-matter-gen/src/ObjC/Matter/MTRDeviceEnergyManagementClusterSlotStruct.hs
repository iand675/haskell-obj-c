{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Generated bindings for @MTRDeviceEnergyManagementClusterSlotStruct@.
module ObjC.Matter.MTRDeviceEnergyManagementClusterSlotStruct
  ( MTRDeviceEnergyManagementClusterSlotStruct
  , IsMTRDeviceEnergyManagementClusterSlotStruct(..)
  , minDuration
  , setMinDuration
  , maxDuration
  , setMaxDuration
  , defaultDuration
  , setDefaultDuration
  , elapsedSlotTime
  , setElapsedSlotTime
  , remainingSlotTime
  , setRemainingSlotTime
  , slotIsPausable
  , setSlotIsPausable
  , minPauseDuration
  , setMinPauseDuration
  , maxPauseDuration
  , setMaxPauseDuration
  , manufacturerESAState
  , setManufacturerESAState
  , nominalPower
  , setNominalPower
  , minPower
  , setMinPower
  , maxPower
  , setMaxPower
  , nominalEnergy
  , setNominalEnergy
  , costs
  , setCosts
  , minPowerAdjustment
  , setMinPowerAdjustment
  , maxPowerAdjustment
  , setMaxPowerAdjustment
  , minDurationAdjustment
  , setMinDurationAdjustment
  , maxDurationAdjustment
  , setMaxDurationAdjustment
  , costsSelector
  , defaultDurationSelector
  , elapsedSlotTimeSelector
  , manufacturerESAStateSelector
  , maxDurationAdjustmentSelector
  , maxDurationSelector
  , maxPauseDurationSelector
  , maxPowerAdjustmentSelector
  , maxPowerSelector
  , minDurationAdjustmentSelector
  , minDurationSelector
  , minPauseDurationSelector
  , minPowerAdjustmentSelector
  , minPowerSelector
  , nominalEnergySelector
  , nominalPowerSelector
  , remainingSlotTimeSelector
  , setCostsSelector
  , setDefaultDurationSelector
  , setElapsedSlotTimeSelector
  , setManufacturerESAStateSelector
  , setMaxDurationAdjustmentSelector
  , setMaxDurationSelector
  , setMaxPauseDurationSelector
  , setMaxPowerAdjustmentSelector
  , setMaxPowerSelector
  , setMinDurationAdjustmentSelector
  , setMinDurationSelector
  , setMinPauseDurationSelector
  , setMinPowerAdjustmentSelector
  , setMinPowerSelector
  , setNominalEnergySelector
  , setNominalPowerSelector
  , setRemainingSlotTimeSelector
  , setSlotIsPausableSelector
  , slotIsPausableSelector


  ) where

import Foreign.Ptr (Ptr, FunPtr)
import Foreign.C.Types

import ObjC.Runtime.Types
import ObjC.Runtime.Message (sendMessage, sendOwnedMessage, sendClassMessage, sendOwnedClassMessage)
import ObjC.Runtime.Selector (mkSelector)
import ObjC.Runtime.Class (getRequiredClass)

import ObjC.Matter.Internal.Classes
import ObjC.Foundation.Internal.Classes

-- | @- minDuration@
minDuration :: IsMTRDeviceEnergyManagementClusterSlotStruct mtrDeviceEnergyManagementClusterSlotStruct => mtrDeviceEnergyManagementClusterSlotStruct -> IO (Id NSNumber)
minDuration mtrDeviceEnergyManagementClusterSlotStruct =
  sendMessage mtrDeviceEnergyManagementClusterSlotStruct minDurationSelector

-- | @- setMinDuration:@
setMinDuration :: (IsMTRDeviceEnergyManagementClusterSlotStruct mtrDeviceEnergyManagementClusterSlotStruct, IsNSNumber value) => mtrDeviceEnergyManagementClusterSlotStruct -> value -> IO ()
setMinDuration mtrDeviceEnergyManagementClusterSlotStruct value =
  sendMessage mtrDeviceEnergyManagementClusterSlotStruct setMinDurationSelector (toNSNumber value)

-- | @- maxDuration@
maxDuration :: IsMTRDeviceEnergyManagementClusterSlotStruct mtrDeviceEnergyManagementClusterSlotStruct => mtrDeviceEnergyManagementClusterSlotStruct -> IO (Id NSNumber)
maxDuration mtrDeviceEnergyManagementClusterSlotStruct =
  sendMessage mtrDeviceEnergyManagementClusterSlotStruct maxDurationSelector

-- | @- setMaxDuration:@
setMaxDuration :: (IsMTRDeviceEnergyManagementClusterSlotStruct mtrDeviceEnergyManagementClusterSlotStruct, IsNSNumber value) => mtrDeviceEnergyManagementClusterSlotStruct -> value -> IO ()
setMaxDuration mtrDeviceEnergyManagementClusterSlotStruct value =
  sendMessage mtrDeviceEnergyManagementClusterSlotStruct setMaxDurationSelector (toNSNumber value)

-- | @- defaultDuration@
defaultDuration :: IsMTRDeviceEnergyManagementClusterSlotStruct mtrDeviceEnergyManagementClusterSlotStruct => mtrDeviceEnergyManagementClusterSlotStruct -> IO (Id NSNumber)
defaultDuration mtrDeviceEnergyManagementClusterSlotStruct =
  sendMessage mtrDeviceEnergyManagementClusterSlotStruct defaultDurationSelector

-- | @- setDefaultDuration:@
setDefaultDuration :: (IsMTRDeviceEnergyManagementClusterSlotStruct mtrDeviceEnergyManagementClusterSlotStruct, IsNSNumber value) => mtrDeviceEnergyManagementClusterSlotStruct -> value -> IO ()
setDefaultDuration mtrDeviceEnergyManagementClusterSlotStruct value =
  sendMessage mtrDeviceEnergyManagementClusterSlotStruct setDefaultDurationSelector (toNSNumber value)

-- | @- elapsedSlotTime@
elapsedSlotTime :: IsMTRDeviceEnergyManagementClusterSlotStruct mtrDeviceEnergyManagementClusterSlotStruct => mtrDeviceEnergyManagementClusterSlotStruct -> IO (Id NSNumber)
elapsedSlotTime mtrDeviceEnergyManagementClusterSlotStruct =
  sendMessage mtrDeviceEnergyManagementClusterSlotStruct elapsedSlotTimeSelector

-- | @- setElapsedSlotTime:@
setElapsedSlotTime :: (IsMTRDeviceEnergyManagementClusterSlotStruct mtrDeviceEnergyManagementClusterSlotStruct, IsNSNumber value) => mtrDeviceEnergyManagementClusterSlotStruct -> value -> IO ()
setElapsedSlotTime mtrDeviceEnergyManagementClusterSlotStruct value =
  sendMessage mtrDeviceEnergyManagementClusterSlotStruct setElapsedSlotTimeSelector (toNSNumber value)

-- | @- remainingSlotTime@
remainingSlotTime :: IsMTRDeviceEnergyManagementClusterSlotStruct mtrDeviceEnergyManagementClusterSlotStruct => mtrDeviceEnergyManagementClusterSlotStruct -> IO (Id NSNumber)
remainingSlotTime mtrDeviceEnergyManagementClusterSlotStruct =
  sendMessage mtrDeviceEnergyManagementClusterSlotStruct remainingSlotTimeSelector

-- | @- setRemainingSlotTime:@
setRemainingSlotTime :: (IsMTRDeviceEnergyManagementClusterSlotStruct mtrDeviceEnergyManagementClusterSlotStruct, IsNSNumber value) => mtrDeviceEnergyManagementClusterSlotStruct -> value -> IO ()
setRemainingSlotTime mtrDeviceEnergyManagementClusterSlotStruct value =
  sendMessage mtrDeviceEnergyManagementClusterSlotStruct setRemainingSlotTimeSelector (toNSNumber value)

-- | @- slotIsPausable@
slotIsPausable :: IsMTRDeviceEnergyManagementClusterSlotStruct mtrDeviceEnergyManagementClusterSlotStruct => mtrDeviceEnergyManagementClusterSlotStruct -> IO (Id NSNumber)
slotIsPausable mtrDeviceEnergyManagementClusterSlotStruct =
  sendMessage mtrDeviceEnergyManagementClusterSlotStruct slotIsPausableSelector

-- | @- setSlotIsPausable:@
setSlotIsPausable :: (IsMTRDeviceEnergyManagementClusterSlotStruct mtrDeviceEnergyManagementClusterSlotStruct, IsNSNumber value) => mtrDeviceEnergyManagementClusterSlotStruct -> value -> IO ()
setSlotIsPausable mtrDeviceEnergyManagementClusterSlotStruct value =
  sendMessage mtrDeviceEnergyManagementClusterSlotStruct setSlotIsPausableSelector (toNSNumber value)

-- | @- minPauseDuration@
minPauseDuration :: IsMTRDeviceEnergyManagementClusterSlotStruct mtrDeviceEnergyManagementClusterSlotStruct => mtrDeviceEnergyManagementClusterSlotStruct -> IO (Id NSNumber)
minPauseDuration mtrDeviceEnergyManagementClusterSlotStruct =
  sendMessage mtrDeviceEnergyManagementClusterSlotStruct minPauseDurationSelector

-- | @- setMinPauseDuration:@
setMinPauseDuration :: (IsMTRDeviceEnergyManagementClusterSlotStruct mtrDeviceEnergyManagementClusterSlotStruct, IsNSNumber value) => mtrDeviceEnergyManagementClusterSlotStruct -> value -> IO ()
setMinPauseDuration mtrDeviceEnergyManagementClusterSlotStruct value =
  sendMessage mtrDeviceEnergyManagementClusterSlotStruct setMinPauseDurationSelector (toNSNumber value)

-- | @- maxPauseDuration@
maxPauseDuration :: IsMTRDeviceEnergyManagementClusterSlotStruct mtrDeviceEnergyManagementClusterSlotStruct => mtrDeviceEnergyManagementClusterSlotStruct -> IO (Id NSNumber)
maxPauseDuration mtrDeviceEnergyManagementClusterSlotStruct =
  sendMessage mtrDeviceEnergyManagementClusterSlotStruct maxPauseDurationSelector

-- | @- setMaxPauseDuration:@
setMaxPauseDuration :: (IsMTRDeviceEnergyManagementClusterSlotStruct mtrDeviceEnergyManagementClusterSlotStruct, IsNSNumber value) => mtrDeviceEnergyManagementClusterSlotStruct -> value -> IO ()
setMaxPauseDuration mtrDeviceEnergyManagementClusterSlotStruct value =
  sendMessage mtrDeviceEnergyManagementClusterSlotStruct setMaxPauseDurationSelector (toNSNumber value)

-- | @- manufacturerESAState@
manufacturerESAState :: IsMTRDeviceEnergyManagementClusterSlotStruct mtrDeviceEnergyManagementClusterSlotStruct => mtrDeviceEnergyManagementClusterSlotStruct -> IO (Id NSNumber)
manufacturerESAState mtrDeviceEnergyManagementClusterSlotStruct =
  sendMessage mtrDeviceEnergyManagementClusterSlotStruct manufacturerESAStateSelector

-- | @- setManufacturerESAState:@
setManufacturerESAState :: (IsMTRDeviceEnergyManagementClusterSlotStruct mtrDeviceEnergyManagementClusterSlotStruct, IsNSNumber value) => mtrDeviceEnergyManagementClusterSlotStruct -> value -> IO ()
setManufacturerESAState mtrDeviceEnergyManagementClusterSlotStruct value =
  sendMessage mtrDeviceEnergyManagementClusterSlotStruct setManufacturerESAStateSelector (toNSNumber value)

-- | @- nominalPower@
nominalPower :: IsMTRDeviceEnergyManagementClusterSlotStruct mtrDeviceEnergyManagementClusterSlotStruct => mtrDeviceEnergyManagementClusterSlotStruct -> IO (Id NSNumber)
nominalPower mtrDeviceEnergyManagementClusterSlotStruct =
  sendMessage mtrDeviceEnergyManagementClusterSlotStruct nominalPowerSelector

-- | @- setNominalPower:@
setNominalPower :: (IsMTRDeviceEnergyManagementClusterSlotStruct mtrDeviceEnergyManagementClusterSlotStruct, IsNSNumber value) => mtrDeviceEnergyManagementClusterSlotStruct -> value -> IO ()
setNominalPower mtrDeviceEnergyManagementClusterSlotStruct value =
  sendMessage mtrDeviceEnergyManagementClusterSlotStruct setNominalPowerSelector (toNSNumber value)

-- | @- minPower@
minPower :: IsMTRDeviceEnergyManagementClusterSlotStruct mtrDeviceEnergyManagementClusterSlotStruct => mtrDeviceEnergyManagementClusterSlotStruct -> IO (Id NSNumber)
minPower mtrDeviceEnergyManagementClusterSlotStruct =
  sendMessage mtrDeviceEnergyManagementClusterSlotStruct minPowerSelector

-- | @- setMinPower:@
setMinPower :: (IsMTRDeviceEnergyManagementClusterSlotStruct mtrDeviceEnergyManagementClusterSlotStruct, IsNSNumber value) => mtrDeviceEnergyManagementClusterSlotStruct -> value -> IO ()
setMinPower mtrDeviceEnergyManagementClusterSlotStruct value =
  sendMessage mtrDeviceEnergyManagementClusterSlotStruct setMinPowerSelector (toNSNumber value)

-- | @- maxPower@
maxPower :: IsMTRDeviceEnergyManagementClusterSlotStruct mtrDeviceEnergyManagementClusterSlotStruct => mtrDeviceEnergyManagementClusterSlotStruct -> IO (Id NSNumber)
maxPower mtrDeviceEnergyManagementClusterSlotStruct =
  sendMessage mtrDeviceEnergyManagementClusterSlotStruct maxPowerSelector

-- | @- setMaxPower:@
setMaxPower :: (IsMTRDeviceEnergyManagementClusterSlotStruct mtrDeviceEnergyManagementClusterSlotStruct, IsNSNumber value) => mtrDeviceEnergyManagementClusterSlotStruct -> value -> IO ()
setMaxPower mtrDeviceEnergyManagementClusterSlotStruct value =
  sendMessage mtrDeviceEnergyManagementClusterSlotStruct setMaxPowerSelector (toNSNumber value)

-- | @- nominalEnergy@
nominalEnergy :: IsMTRDeviceEnergyManagementClusterSlotStruct mtrDeviceEnergyManagementClusterSlotStruct => mtrDeviceEnergyManagementClusterSlotStruct -> IO (Id NSNumber)
nominalEnergy mtrDeviceEnergyManagementClusterSlotStruct =
  sendMessage mtrDeviceEnergyManagementClusterSlotStruct nominalEnergySelector

-- | @- setNominalEnergy:@
setNominalEnergy :: (IsMTRDeviceEnergyManagementClusterSlotStruct mtrDeviceEnergyManagementClusterSlotStruct, IsNSNumber value) => mtrDeviceEnergyManagementClusterSlotStruct -> value -> IO ()
setNominalEnergy mtrDeviceEnergyManagementClusterSlotStruct value =
  sendMessage mtrDeviceEnergyManagementClusterSlotStruct setNominalEnergySelector (toNSNumber value)

-- | @- costs@
costs :: IsMTRDeviceEnergyManagementClusterSlotStruct mtrDeviceEnergyManagementClusterSlotStruct => mtrDeviceEnergyManagementClusterSlotStruct -> IO (Id NSArray)
costs mtrDeviceEnergyManagementClusterSlotStruct =
  sendMessage mtrDeviceEnergyManagementClusterSlotStruct costsSelector

-- | @- setCosts:@
setCosts :: (IsMTRDeviceEnergyManagementClusterSlotStruct mtrDeviceEnergyManagementClusterSlotStruct, IsNSArray value) => mtrDeviceEnergyManagementClusterSlotStruct -> value -> IO ()
setCosts mtrDeviceEnergyManagementClusterSlotStruct value =
  sendMessage mtrDeviceEnergyManagementClusterSlotStruct setCostsSelector (toNSArray value)

-- | @- minPowerAdjustment@
minPowerAdjustment :: IsMTRDeviceEnergyManagementClusterSlotStruct mtrDeviceEnergyManagementClusterSlotStruct => mtrDeviceEnergyManagementClusterSlotStruct -> IO (Id NSNumber)
minPowerAdjustment mtrDeviceEnergyManagementClusterSlotStruct =
  sendMessage mtrDeviceEnergyManagementClusterSlotStruct minPowerAdjustmentSelector

-- | @- setMinPowerAdjustment:@
setMinPowerAdjustment :: (IsMTRDeviceEnergyManagementClusterSlotStruct mtrDeviceEnergyManagementClusterSlotStruct, IsNSNumber value) => mtrDeviceEnergyManagementClusterSlotStruct -> value -> IO ()
setMinPowerAdjustment mtrDeviceEnergyManagementClusterSlotStruct value =
  sendMessage mtrDeviceEnergyManagementClusterSlotStruct setMinPowerAdjustmentSelector (toNSNumber value)

-- | @- maxPowerAdjustment@
maxPowerAdjustment :: IsMTRDeviceEnergyManagementClusterSlotStruct mtrDeviceEnergyManagementClusterSlotStruct => mtrDeviceEnergyManagementClusterSlotStruct -> IO (Id NSNumber)
maxPowerAdjustment mtrDeviceEnergyManagementClusterSlotStruct =
  sendMessage mtrDeviceEnergyManagementClusterSlotStruct maxPowerAdjustmentSelector

-- | @- setMaxPowerAdjustment:@
setMaxPowerAdjustment :: (IsMTRDeviceEnergyManagementClusterSlotStruct mtrDeviceEnergyManagementClusterSlotStruct, IsNSNumber value) => mtrDeviceEnergyManagementClusterSlotStruct -> value -> IO ()
setMaxPowerAdjustment mtrDeviceEnergyManagementClusterSlotStruct value =
  sendMessage mtrDeviceEnergyManagementClusterSlotStruct setMaxPowerAdjustmentSelector (toNSNumber value)

-- | @- minDurationAdjustment@
minDurationAdjustment :: IsMTRDeviceEnergyManagementClusterSlotStruct mtrDeviceEnergyManagementClusterSlotStruct => mtrDeviceEnergyManagementClusterSlotStruct -> IO (Id NSNumber)
minDurationAdjustment mtrDeviceEnergyManagementClusterSlotStruct =
  sendMessage mtrDeviceEnergyManagementClusterSlotStruct minDurationAdjustmentSelector

-- | @- setMinDurationAdjustment:@
setMinDurationAdjustment :: (IsMTRDeviceEnergyManagementClusterSlotStruct mtrDeviceEnergyManagementClusterSlotStruct, IsNSNumber value) => mtrDeviceEnergyManagementClusterSlotStruct -> value -> IO ()
setMinDurationAdjustment mtrDeviceEnergyManagementClusterSlotStruct value =
  sendMessage mtrDeviceEnergyManagementClusterSlotStruct setMinDurationAdjustmentSelector (toNSNumber value)

-- | @- maxDurationAdjustment@
maxDurationAdjustment :: IsMTRDeviceEnergyManagementClusterSlotStruct mtrDeviceEnergyManagementClusterSlotStruct => mtrDeviceEnergyManagementClusterSlotStruct -> IO (Id NSNumber)
maxDurationAdjustment mtrDeviceEnergyManagementClusterSlotStruct =
  sendMessage mtrDeviceEnergyManagementClusterSlotStruct maxDurationAdjustmentSelector

-- | @- setMaxDurationAdjustment:@
setMaxDurationAdjustment :: (IsMTRDeviceEnergyManagementClusterSlotStruct mtrDeviceEnergyManagementClusterSlotStruct, IsNSNumber value) => mtrDeviceEnergyManagementClusterSlotStruct -> value -> IO ()
setMaxDurationAdjustment mtrDeviceEnergyManagementClusterSlotStruct value =
  sendMessage mtrDeviceEnergyManagementClusterSlotStruct setMaxDurationAdjustmentSelector (toNSNumber value)

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

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

-- | @Selector@ for @defaultDuration@
defaultDurationSelector :: Selector '[] (Id NSNumber)
defaultDurationSelector = mkSelector "defaultDuration"

-- | @Selector@ for @setDefaultDuration:@
setDefaultDurationSelector :: Selector '[Id NSNumber] ()
setDefaultDurationSelector = mkSelector "setDefaultDuration:"

-- | @Selector@ for @elapsedSlotTime@
elapsedSlotTimeSelector :: Selector '[] (Id NSNumber)
elapsedSlotTimeSelector = mkSelector "elapsedSlotTime"

-- | @Selector@ for @setElapsedSlotTime:@
setElapsedSlotTimeSelector :: Selector '[Id NSNumber] ()
setElapsedSlotTimeSelector = mkSelector "setElapsedSlotTime:"

-- | @Selector@ for @remainingSlotTime@
remainingSlotTimeSelector :: Selector '[] (Id NSNumber)
remainingSlotTimeSelector = mkSelector "remainingSlotTime"

-- | @Selector@ for @setRemainingSlotTime:@
setRemainingSlotTimeSelector :: Selector '[Id NSNumber] ()
setRemainingSlotTimeSelector = mkSelector "setRemainingSlotTime:"

-- | @Selector@ for @slotIsPausable@
slotIsPausableSelector :: Selector '[] (Id NSNumber)
slotIsPausableSelector = mkSelector "slotIsPausable"

-- | @Selector@ for @setSlotIsPausable:@
setSlotIsPausableSelector :: Selector '[Id NSNumber] ()
setSlotIsPausableSelector = mkSelector "setSlotIsPausable:"

-- | @Selector@ for @minPauseDuration@
minPauseDurationSelector :: Selector '[] (Id NSNumber)
minPauseDurationSelector = mkSelector "minPauseDuration"

-- | @Selector@ for @setMinPauseDuration:@
setMinPauseDurationSelector :: Selector '[Id NSNumber] ()
setMinPauseDurationSelector = mkSelector "setMinPauseDuration:"

-- | @Selector@ for @maxPauseDuration@
maxPauseDurationSelector :: Selector '[] (Id NSNumber)
maxPauseDurationSelector = mkSelector "maxPauseDuration"

-- | @Selector@ for @setMaxPauseDuration:@
setMaxPauseDurationSelector :: Selector '[Id NSNumber] ()
setMaxPauseDurationSelector = mkSelector "setMaxPauseDuration:"

-- | @Selector@ for @manufacturerESAState@
manufacturerESAStateSelector :: Selector '[] (Id NSNumber)
manufacturerESAStateSelector = mkSelector "manufacturerESAState"

-- | @Selector@ for @setManufacturerESAState:@
setManufacturerESAStateSelector :: Selector '[Id NSNumber] ()
setManufacturerESAStateSelector = mkSelector "setManufacturerESAState:"

-- | @Selector@ for @nominalPower@
nominalPowerSelector :: Selector '[] (Id NSNumber)
nominalPowerSelector = mkSelector "nominalPower"

-- | @Selector@ for @setNominalPower:@
setNominalPowerSelector :: Selector '[Id NSNumber] ()
setNominalPowerSelector = mkSelector "setNominalPower:"

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

-- | @Selector@ for @nominalEnergy@
nominalEnergySelector :: Selector '[] (Id NSNumber)
nominalEnergySelector = mkSelector "nominalEnergy"

-- | @Selector@ for @setNominalEnergy:@
setNominalEnergySelector :: Selector '[Id NSNumber] ()
setNominalEnergySelector = mkSelector "setNominalEnergy:"

-- | @Selector@ for @costs@
costsSelector :: Selector '[] (Id NSArray)
costsSelector = mkSelector "costs"

-- | @Selector@ for @setCosts:@
setCostsSelector :: Selector '[Id NSArray] ()
setCostsSelector = mkSelector "setCosts:"

-- | @Selector@ for @minPowerAdjustment@
minPowerAdjustmentSelector :: Selector '[] (Id NSNumber)
minPowerAdjustmentSelector = mkSelector "minPowerAdjustment"

-- | @Selector@ for @setMinPowerAdjustment:@
setMinPowerAdjustmentSelector :: Selector '[Id NSNumber] ()
setMinPowerAdjustmentSelector = mkSelector "setMinPowerAdjustment:"

-- | @Selector@ for @maxPowerAdjustment@
maxPowerAdjustmentSelector :: Selector '[] (Id NSNumber)
maxPowerAdjustmentSelector = mkSelector "maxPowerAdjustment"

-- | @Selector@ for @setMaxPowerAdjustment:@
setMaxPowerAdjustmentSelector :: Selector '[Id NSNumber] ()
setMaxPowerAdjustmentSelector = mkSelector "setMaxPowerAdjustment:"

-- | @Selector@ for @minDurationAdjustment@
minDurationAdjustmentSelector :: Selector '[] (Id NSNumber)
minDurationAdjustmentSelector = mkSelector "minDurationAdjustment"

-- | @Selector@ for @setMinDurationAdjustment:@
setMinDurationAdjustmentSelector :: Selector '[Id NSNumber] ()
setMinDurationAdjustmentSelector = mkSelector "setMinDurationAdjustment:"

-- | @Selector@ for @maxDurationAdjustment@
maxDurationAdjustmentSelector :: Selector '[] (Id NSNumber)
maxDurationAdjustmentSelector = mkSelector "maxDurationAdjustment"

-- | @Selector@ for @setMaxDurationAdjustment:@
setMaxDurationAdjustmentSelector :: Selector '[Id NSNumber] ()
setMaxDurationAdjustmentSelector = mkSelector "setMaxDurationAdjustment:"

