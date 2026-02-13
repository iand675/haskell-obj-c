{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Generated bindings for @MTRElectricalEnergyMeasurementClusterCumulativeEnergyMeasuredEvent@.
module ObjC.Matter.MTRElectricalEnergyMeasurementClusterCumulativeEnergyMeasuredEvent
  ( MTRElectricalEnergyMeasurementClusterCumulativeEnergyMeasuredEvent
  , IsMTRElectricalEnergyMeasurementClusterCumulativeEnergyMeasuredEvent(..)
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
energyImported :: IsMTRElectricalEnergyMeasurementClusterCumulativeEnergyMeasuredEvent mtrElectricalEnergyMeasurementClusterCumulativeEnergyMeasuredEvent => mtrElectricalEnergyMeasurementClusterCumulativeEnergyMeasuredEvent -> IO (Id MTRElectricalEnergyMeasurementClusterEnergyMeasurementStruct)
energyImported mtrElectricalEnergyMeasurementClusterCumulativeEnergyMeasuredEvent =
  sendMessage mtrElectricalEnergyMeasurementClusterCumulativeEnergyMeasuredEvent energyImportedSelector

-- | @- setEnergyImported:@
setEnergyImported :: (IsMTRElectricalEnergyMeasurementClusterCumulativeEnergyMeasuredEvent mtrElectricalEnergyMeasurementClusterCumulativeEnergyMeasuredEvent, IsMTRElectricalEnergyMeasurementClusterEnergyMeasurementStruct value) => mtrElectricalEnergyMeasurementClusterCumulativeEnergyMeasuredEvent -> value -> IO ()
setEnergyImported mtrElectricalEnergyMeasurementClusterCumulativeEnergyMeasuredEvent value =
  sendMessage mtrElectricalEnergyMeasurementClusterCumulativeEnergyMeasuredEvent setEnergyImportedSelector (toMTRElectricalEnergyMeasurementClusterEnergyMeasurementStruct value)

-- | @- energyExported@
energyExported :: IsMTRElectricalEnergyMeasurementClusterCumulativeEnergyMeasuredEvent mtrElectricalEnergyMeasurementClusterCumulativeEnergyMeasuredEvent => mtrElectricalEnergyMeasurementClusterCumulativeEnergyMeasuredEvent -> IO (Id MTRElectricalEnergyMeasurementClusterEnergyMeasurementStruct)
energyExported mtrElectricalEnergyMeasurementClusterCumulativeEnergyMeasuredEvent =
  sendMessage mtrElectricalEnergyMeasurementClusterCumulativeEnergyMeasuredEvent energyExportedSelector

-- | @- setEnergyExported:@
setEnergyExported :: (IsMTRElectricalEnergyMeasurementClusterCumulativeEnergyMeasuredEvent mtrElectricalEnergyMeasurementClusterCumulativeEnergyMeasuredEvent, IsMTRElectricalEnergyMeasurementClusterEnergyMeasurementStruct value) => mtrElectricalEnergyMeasurementClusterCumulativeEnergyMeasuredEvent -> value -> IO ()
setEnergyExported mtrElectricalEnergyMeasurementClusterCumulativeEnergyMeasuredEvent value =
  sendMessage mtrElectricalEnergyMeasurementClusterCumulativeEnergyMeasuredEvent setEnergyExportedSelector (toMTRElectricalEnergyMeasurementClusterEnergyMeasurementStruct value)

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

