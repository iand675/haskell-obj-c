{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Generated bindings for @MTREnergyEVSEClusterChargingTargetStruct@.
module ObjC.Matter.MTREnergyEVSEClusterChargingTargetStruct
  ( MTREnergyEVSEClusterChargingTargetStruct
  , IsMTREnergyEVSEClusterChargingTargetStruct(..)
  , targetTimeMinutesPastMidnight
  , setTargetTimeMinutesPastMidnight
  , targetSoC
  , setTargetSoC
  , addedEnergy
  , setAddedEnergy
  , addedEnergySelector
  , setAddedEnergySelector
  , setTargetSoCSelector
  , setTargetTimeMinutesPastMidnightSelector
  , targetSoCSelector
  , targetTimeMinutesPastMidnightSelector


  ) where

import Foreign.Ptr (Ptr, FunPtr)
import Foreign.C.Types

import ObjC.Runtime.Types
import ObjC.Runtime.Message (sendMessage, sendOwnedMessage, sendClassMessage, sendOwnedClassMessage)
import ObjC.Runtime.Selector (mkSelector)
import ObjC.Runtime.Class (getRequiredClass)

import ObjC.Matter.Internal.Classes
import ObjC.Foundation.Internal.Classes

-- | @- targetTimeMinutesPastMidnight@
targetTimeMinutesPastMidnight :: IsMTREnergyEVSEClusterChargingTargetStruct mtrEnergyEVSEClusterChargingTargetStruct => mtrEnergyEVSEClusterChargingTargetStruct -> IO (Id NSNumber)
targetTimeMinutesPastMidnight mtrEnergyEVSEClusterChargingTargetStruct =
  sendMessage mtrEnergyEVSEClusterChargingTargetStruct targetTimeMinutesPastMidnightSelector

-- | @- setTargetTimeMinutesPastMidnight:@
setTargetTimeMinutesPastMidnight :: (IsMTREnergyEVSEClusterChargingTargetStruct mtrEnergyEVSEClusterChargingTargetStruct, IsNSNumber value) => mtrEnergyEVSEClusterChargingTargetStruct -> value -> IO ()
setTargetTimeMinutesPastMidnight mtrEnergyEVSEClusterChargingTargetStruct value =
  sendMessage mtrEnergyEVSEClusterChargingTargetStruct setTargetTimeMinutesPastMidnightSelector (toNSNumber value)

-- | @- targetSoC@
targetSoC :: IsMTREnergyEVSEClusterChargingTargetStruct mtrEnergyEVSEClusterChargingTargetStruct => mtrEnergyEVSEClusterChargingTargetStruct -> IO (Id NSNumber)
targetSoC mtrEnergyEVSEClusterChargingTargetStruct =
  sendMessage mtrEnergyEVSEClusterChargingTargetStruct targetSoCSelector

-- | @- setTargetSoC:@
setTargetSoC :: (IsMTREnergyEVSEClusterChargingTargetStruct mtrEnergyEVSEClusterChargingTargetStruct, IsNSNumber value) => mtrEnergyEVSEClusterChargingTargetStruct -> value -> IO ()
setTargetSoC mtrEnergyEVSEClusterChargingTargetStruct value =
  sendMessage mtrEnergyEVSEClusterChargingTargetStruct setTargetSoCSelector (toNSNumber value)

-- | @- addedEnergy@
addedEnergy :: IsMTREnergyEVSEClusterChargingTargetStruct mtrEnergyEVSEClusterChargingTargetStruct => mtrEnergyEVSEClusterChargingTargetStruct -> IO (Id NSNumber)
addedEnergy mtrEnergyEVSEClusterChargingTargetStruct =
  sendMessage mtrEnergyEVSEClusterChargingTargetStruct addedEnergySelector

-- | @- setAddedEnergy:@
setAddedEnergy :: (IsMTREnergyEVSEClusterChargingTargetStruct mtrEnergyEVSEClusterChargingTargetStruct, IsNSNumber value) => mtrEnergyEVSEClusterChargingTargetStruct -> value -> IO ()
setAddedEnergy mtrEnergyEVSEClusterChargingTargetStruct value =
  sendMessage mtrEnergyEVSEClusterChargingTargetStruct setAddedEnergySelector (toNSNumber value)

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @targetTimeMinutesPastMidnight@
targetTimeMinutesPastMidnightSelector :: Selector '[] (Id NSNumber)
targetTimeMinutesPastMidnightSelector = mkSelector "targetTimeMinutesPastMidnight"

-- | @Selector@ for @setTargetTimeMinutesPastMidnight:@
setTargetTimeMinutesPastMidnightSelector :: Selector '[Id NSNumber] ()
setTargetTimeMinutesPastMidnightSelector = mkSelector "setTargetTimeMinutesPastMidnight:"

-- | @Selector@ for @targetSoC@
targetSoCSelector :: Selector '[] (Id NSNumber)
targetSoCSelector = mkSelector "targetSoC"

-- | @Selector@ for @setTargetSoC:@
setTargetSoCSelector :: Selector '[Id NSNumber] ()
setTargetSoCSelector = mkSelector "setTargetSoC:"

-- | @Selector@ for @addedEnergy@
addedEnergySelector :: Selector '[] (Id NSNumber)
addedEnergySelector = mkSelector "addedEnergy"

-- | @Selector@ for @setAddedEnergy:@
setAddedEnergySelector :: Selector '[Id NSNumber] ()
setAddedEnergySelector = mkSelector "setAddedEnergy:"

