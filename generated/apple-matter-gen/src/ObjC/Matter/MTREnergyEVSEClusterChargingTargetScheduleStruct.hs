{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Generated bindings for @MTREnergyEVSEClusterChargingTargetScheduleStruct@.
module ObjC.Matter.MTREnergyEVSEClusterChargingTargetScheduleStruct
  ( MTREnergyEVSEClusterChargingTargetScheduleStruct
  , IsMTREnergyEVSEClusterChargingTargetScheduleStruct(..)
  , dayOfWeekForSequence
  , setDayOfWeekForSequence
  , chargingTargets
  , setChargingTargets
  , chargingTargetsSelector
  , dayOfWeekForSequenceSelector
  , setChargingTargetsSelector
  , setDayOfWeekForSequenceSelector


  ) where

import Foreign.Ptr (Ptr, FunPtr)
import Foreign.C.Types

import ObjC.Runtime.Types
import ObjC.Runtime.Message (sendMessage, sendOwnedMessage, sendClassMessage, sendOwnedClassMessage)
import ObjC.Runtime.Selector (mkSelector)
import ObjC.Runtime.Class (getRequiredClass)

import ObjC.Matter.Internal.Classes
import ObjC.Foundation.Internal.Classes

-- | @- dayOfWeekForSequence@
dayOfWeekForSequence :: IsMTREnergyEVSEClusterChargingTargetScheduleStruct mtrEnergyEVSEClusterChargingTargetScheduleStruct => mtrEnergyEVSEClusterChargingTargetScheduleStruct -> IO (Id NSNumber)
dayOfWeekForSequence mtrEnergyEVSEClusterChargingTargetScheduleStruct =
  sendMessage mtrEnergyEVSEClusterChargingTargetScheduleStruct dayOfWeekForSequenceSelector

-- | @- setDayOfWeekForSequence:@
setDayOfWeekForSequence :: (IsMTREnergyEVSEClusterChargingTargetScheduleStruct mtrEnergyEVSEClusterChargingTargetScheduleStruct, IsNSNumber value) => mtrEnergyEVSEClusterChargingTargetScheduleStruct -> value -> IO ()
setDayOfWeekForSequence mtrEnergyEVSEClusterChargingTargetScheduleStruct value =
  sendMessage mtrEnergyEVSEClusterChargingTargetScheduleStruct setDayOfWeekForSequenceSelector (toNSNumber value)

-- | @- chargingTargets@
chargingTargets :: IsMTREnergyEVSEClusterChargingTargetScheduleStruct mtrEnergyEVSEClusterChargingTargetScheduleStruct => mtrEnergyEVSEClusterChargingTargetScheduleStruct -> IO (Id NSArray)
chargingTargets mtrEnergyEVSEClusterChargingTargetScheduleStruct =
  sendMessage mtrEnergyEVSEClusterChargingTargetScheduleStruct chargingTargetsSelector

-- | @- setChargingTargets:@
setChargingTargets :: (IsMTREnergyEVSEClusterChargingTargetScheduleStruct mtrEnergyEVSEClusterChargingTargetScheduleStruct, IsNSArray value) => mtrEnergyEVSEClusterChargingTargetScheduleStruct -> value -> IO ()
setChargingTargets mtrEnergyEVSEClusterChargingTargetScheduleStruct value =
  sendMessage mtrEnergyEVSEClusterChargingTargetScheduleStruct setChargingTargetsSelector (toNSArray value)

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @dayOfWeekForSequence@
dayOfWeekForSequenceSelector :: Selector '[] (Id NSNumber)
dayOfWeekForSequenceSelector = mkSelector "dayOfWeekForSequence"

-- | @Selector@ for @setDayOfWeekForSequence:@
setDayOfWeekForSequenceSelector :: Selector '[Id NSNumber] ()
setDayOfWeekForSequenceSelector = mkSelector "setDayOfWeekForSequence:"

-- | @Selector@ for @chargingTargets@
chargingTargetsSelector :: Selector '[] (Id NSArray)
chargingTargetsSelector = mkSelector "chargingTargets"

-- | @Selector@ for @setChargingTargets:@
setChargingTargetsSelector :: Selector '[Id NSArray] ()
setChargingTargetsSelector = mkSelector "setChargingTargets:"

