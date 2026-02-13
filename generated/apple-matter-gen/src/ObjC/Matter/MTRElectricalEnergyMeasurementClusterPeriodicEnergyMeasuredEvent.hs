{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Generated bindings for @MTRElectricalEnergyMeasurementClusterPeriodicEnergyMeasuredEvent@.
module ObjC.Matter.MTRElectricalEnergyMeasurementClusterPeriodicEnergyMeasuredEvent
  ( MTRElectricalEnergyMeasurementClusterPeriodicEnergyMeasuredEvent
  , IsMTRElectricalEnergyMeasurementClusterPeriodicEnergyMeasuredEvent(..)
  , energyImported
  , setEnergyImported
  , energyExported
  , setEnergyExported
  , energyExportedSelector
  , energyImportedSelector
  , setEnergyExportedSelector
  , setEnergyImportedSelector


  ) where

import Foreign.Ptr (Ptr, FunPtr)
import Foreign.C.Types

import ObjC.Runtime.Types
import ObjC.Runtime.Message (sendMessage, sendOwnedMessage, sendClassMessage, sendOwnedClassMessage)
import ObjC.Runtime.Selector (mkSelector)
import ObjC.Runtime.Class (getRequiredClass)

import ObjC.Matter.Internal.Classes
import ObjC.Foundation.Internal.Classes

-- | @- energyImported@
energyImported :: IsMTRElectricalEnergyMeasurementClusterPeriodicEnergyMeasuredEvent mtrElectricalEnergyMeasurementClusterPeriodicEnergyMeasuredEvent => mtrElectricalEnergyMeasurementClusterPeriodicEnergyMeasuredEvent -> IO (Id MTRElectricalEnergyMeasurementClusterEnergyMeasurementStruct)
energyImported mtrElectricalEnergyMeasurementClusterPeriodicEnergyMeasuredEvent =
  sendMessage mtrElectricalEnergyMeasurementClusterPeriodicEnergyMeasuredEvent energyImportedSelector

-- | @- setEnergyImported:@
setEnergyImported :: (IsMTRElectricalEnergyMeasurementClusterPeriodicEnergyMeasuredEvent mtrElectricalEnergyMeasurementClusterPeriodicEnergyMeasuredEvent, IsMTRElectricalEnergyMeasurementClusterEnergyMeasurementStruct value) => mtrElectricalEnergyMeasurementClusterPeriodicEnergyMeasuredEvent -> value -> IO ()
setEnergyImported mtrElectricalEnergyMeasurementClusterPeriodicEnergyMeasuredEvent value =
  sendMessage mtrElectricalEnergyMeasurementClusterPeriodicEnergyMeasuredEvent setEnergyImportedSelector (toMTRElectricalEnergyMeasurementClusterEnergyMeasurementStruct value)

-- | @- energyExported@
energyExported :: IsMTRElectricalEnergyMeasurementClusterPeriodicEnergyMeasuredEvent mtrElectricalEnergyMeasurementClusterPeriodicEnergyMeasuredEvent => mtrElectricalEnergyMeasurementClusterPeriodicEnergyMeasuredEvent -> IO (Id MTRElectricalEnergyMeasurementClusterEnergyMeasurementStruct)
energyExported mtrElectricalEnergyMeasurementClusterPeriodicEnergyMeasuredEvent =
  sendMessage mtrElectricalEnergyMeasurementClusterPeriodicEnergyMeasuredEvent energyExportedSelector

-- | @- setEnergyExported:@
setEnergyExported :: (IsMTRElectricalEnergyMeasurementClusterPeriodicEnergyMeasuredEvent mtrElectricalEnergyMeasurementClusterPeriodicEnergyMeasuredEvent, IsMTRElectricalEnergyMeasurementClusterEnergyMeasurementStruct value) => mtrElectricalEnergyMeasurementClusterPeriodicEnergyMeasuredEvent -> value -> IO ()
setEnergyExported mtrElectricalEnergyMeasurementClusterPeriodicEnergyMeasuredEvent value =
  sendMessage mtrElectricalEnergyMeasurementClusterPeriodicEnergyMeasuredEvent setEnergyExportedSelector (toMTRElectricalEnergyMeasurementClusterEnergyMeasurementStruct value)

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @energyImported@
energyImportedSelector :: Selector '[] (Id MTRElectricalEnergyMeasurementClusterEnergyMeasurementStruct)
energyImportedSelector = mkSelector "energyImported"

-- | @Selector@ for @setEnergyImported:@
setEnergyImportedSelector :: Selector '[Id MTRElectricalEnergyMeasurementClusterEnergyMeasurementStruct] ()
setEnergyImportedSelector = mkSelector "setEnergyImported:"

-- | @Selector@ for @energyExported@
energyExportedSelector :: Selector '[] (Id MTRElectricalEnergyMeasurementClusterEnergyMeasurementStruct)
energyExportedSelector = mkSelector "energyExported"

-- | @Selector@ for @setEnergyExported:@
setEnergyExportedSelector :: Selector '[Id MTRElectricalEnergyMeasurementClusterEnergyMeasurementStruct] ()
setEnergyExportedSelector = mkSelector "setEnergyExported:"

