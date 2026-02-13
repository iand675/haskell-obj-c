{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Generated bindings for @MTRElectricalEnergyMeasurementClusterCumulativeEnergyResetStruct@.
module ObjC.Matter.MTRElectricalEnergyMeasurementClusterCumulativeEnergyResetStruct
  ( MTRElectricalEnergyMeasurementClusterCumulativeEnergyResetStruct
  , IsMTRElectricalEnergyMeasurementClusterCumulativeEnergyResetStruct(..)
  , importedResetTimestamp
  , setImportedResetTimestamp
  , exportedResetTimestamp
  , setExportedResetTimestamp
  , importedResetSystime
  , setImportedResetSystime
  , exportedResetSystime
  , setExportedResetSystime
  , exportedResetSystimeSelector
  , exportedResetTimestampSelector
  , importedResetSystimeSelector
  , importedResetTimestampSelector
  , setExportedResetSystimeSelector
  , setExportedResetTimestampSelector
  , setImportedResetSystimeSelector
  , setImportedResetTimestampSelector


  ) where

import Foreign.Ptr (Ptr, FunPtr)
import Foreign.C.Types

import ObjC.Runtime.Types
import ObjC.Runtime.Message (sendMessage, sendOwnedMessage, sendClassMessage, sendOwnedClassMessage)
import ObjC.Runtime.Selector (mkSelector)
import ObjC.Runtime.Class (getRequiredClass)

import ObjC.Matter.Internal.Classes
import ObjC.Foundation.Internal.Classes

-- | @- importedResetTimestamp@
importedResetTimestamp :: IsMTRElectricalEnergyMeasurementClusterCumulativeEnergyResetStruct mtrElectricalEnergyMeasurementClusterCumulativeEnergyResetStruct => mtrElectricalEnergyMeasurementClusterCumulativeEnergyResetStruct -> IO (Id NSNumber)
importedResetTimestamp mtrElectricalEnergyMeasurementClusterCumulativeEnergyResetStruct =
  sendMessage mtrElectricalEnergyMeasurementClusterCumulativeEnergyResetStruct importedResetTimestampSelector

-- | @- setImportedResetTimestamp:@
setImportedResetTimestamp :: (IsMTRElectricalEnergyMeasurementClusterCumulativeEnergyResetStruct mtrElectricalEnergyMeasurementClusterCumulativeEnergyResetStruct, IsNSNumber value) => mtrElectricalEnergyMeasurementClusterCumulativeEnergyResetStruct -> value -> IO ()
setImportedResetTimestamp mtrElectricalEnergyMeasurementClusterCumulativeEnergyResetStruct value =
  sendMessage mtrElectricalEnergyMeasurementClusterCumulativeEnergyResetStruct setImportedResetTimestampSelector (toNSNumber value)

-- | @- exportedResetTimestamp@
exportedResetTimestamp :: IsMTRElectricalEnergyMeasurementClusterCumulativeEnergyResetStruct mtrElectricalEnergyMeasurementClusterCumulativeEnergyResetStruct => mtrElectricalEnergyMeasurementClusterCumulativeEnergyResetStruct -> IO (Id NSNumber)
exportedResetTimestamp mtrElectricalEnergyMeasurementClusterCumulativeEnergyResetStruct =
  sendMessage mtrElectricalEnergyMeasurementClusterCumulativeEnergyResetStruct exportedResetTimestampSelector

-- | @- setExportedResetTimestamp:@
setExportedResetTimestamp :: (IsMTRElectricalEnergyMeasurementClusterCumulativeEnergyResetStruct mtrElectricalEnergyMeasurementClusterCumulativeEnergyResetStruct, IsNSNumber value) => mtrElectricalEnergyMeasurementClusterCumulativeEnergyResetStruct -> value -> IO ()
setExportedResetTimestamp mtrElectricalEnergyMeasurementClusterCumulativeEnergyResetStruct value =
  sendMessage mtrElectricalEnergyMeasurementClusterCumulativeEnergyResetStruct setExportedResetTimestampSelector (toNSNumber value)

-- | @- importedResetSystime@
importedResetSystime :: IsMTRElectricalEnergyMeasurementClusterCumulativeEnergyResetStruct mtrElectricalEnergyMeasurementClusterCumulativeEnergyResetStruct => mtrElectricalEnergyMeasurementClusterCumulativeEnergyResetStruct -> IO (Id NSNumber)
importedResetSystime mtrElectricalEnergyMeasurementClusterCumulativeEnergyResetStruct =
  sendMessage mtrElectricalEnergyMeasurementClusterCumulativeEnergyResetStruct importedResetSystimeSelector

-- | @- setImportedResetSystime:@
setImportedResetSystime :: (IsMTRElectricalEnergyMeasurementClusterCumulativeEnergyResetStruct mtrElectricalEnergyMeasurementClusterCumulativeEnergyResetStruct, IsNSNumber value) => mtrElectricalEnergyMeasurementClusterCumulativeEnergyResetStruct -> value -> IO ()
setImportedResetSystime mtrElectricalEnergyMeasurementClusterCumulativeEnergyResetStruct value =
  sendMessage mtrElectricalEnergyMeasurementClusterCumulativeEnergyResetStruct setImportedResetSystimeSelector (toNSNumber value)

-- | @- exportedResetSystime@
exportedResetSystime :: IsMTRElectricalEnergyMeasurementClusterCumulativeEnergyResetStruct mtrElectricalEnergyMeasurementClusterCumulativeEnergyResetStruct => mtrElectricalEnergyMeasurementClusterCumulativeEnergyResetStruct -> IO (Id NSNumber)
exportedResetSystime mtrElectricalEnergyMeasurementClusterCumulativeEnergyResetStruct =
  sendMessage mtrElectricalEnergyMeasurementClusterCumulativeEnergyResetStruct exportedResetSystimeSelector

-- | @- setExportedResetSystime:@
setExportedResetSystime :: (IsMTRElectricalEnergyMeasurementClusterCumulativeEnergyResetStruct mtrElectricalEnergyMeasurementClusterCumulativeEnergyResetStruct, IsNSNumber value) => mtrElectricalEnergyMeasurementClusterCumulativeEnergyResetStruct -> value -> IO ()
setExportedResetSystime mtrElectricalEnergyMeasurementClusterCumulativeEnergyResetStruct value =
  sendMessage mtrElectricalEnergyMeasurementClusterCumulativeEnergyResetStruct setExportedResetSystimeSelector (toNSNumber value)

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @importedResetTimestamp@
importedResetTimestampSelector :: Selector '[] (Id NSNumber)
importedResetTimestampSelector = mkSelector "importedResetTimestamp"

-- | @Selector@ for @setImportedResetTimestamp:@
setImportedResetTimestampSelector :: Selector '[Id NSNumber] ()
setImportedResetTimestampSelector = mkSelector "setImportedResetTimestamp:"

-- | @Selector@ for @exportedResetTimestamp@
exportedResetTimestampSelector :: Selector '[] (Id NSNumber)
exportedResetTimestampSelector = mkSelector "exportedResetTimestamp"

-- | @Selector@ for @setExportedResetTimestamp:@
setExportedResetTimestampSelector :: Selector '[Id NSNumber] ()
setExportedResetTimestampSelector = mkSelector "setExportedResetTimestamp:"

-- | @Selector@ for @importedResetSystime@
importedResetSystimeSelector :: Selector '[] (Id NSNumber)
importedResetSystimeSelector = mkSelector "importedResetSystime"

-- | @Selector@ for @setImportedResetSystime:@
setImportedResetSystimeSelector :: Selector '[Id NSNumber] ()
setImportedResetSystimeSelector = mkSelector "setImportedResetSystime:"

-- | @Selector@ for @exportedResetSystime@
exportedResetSystimeSelector :: Selector '[] (Id NSNumber)
exportedResetSystimeSelector = mkSelector "exportedResetSystime"

-- | @Selector@ for @setExportedResetSystime:@
setExportedResetSystimeSelector :: Selector '[Id NSNumber] ()
setExportedResetSystimeSelector = mkSelector "setExportedResetSystime:"

