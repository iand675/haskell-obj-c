{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Cluster Electrical Energy Measurement    This cluster provides a mechanism for querying data about the electrical energy imported or provided by the server.
--
-- Generated bindings for @MTRClusterElectricalEnergyMeasurement@.
module ObjC.Matter.MTRClusterElectricalEnergyMeasurement
  ( MTRClusterElectricalEnergyMeasurement
  , IsMTRClusterElectricalEnergyMeasurement(..)
  , readAttributeAccuracyWithParams
  , readAttributeCumulativeEnergyImportedWithParams
  , readAttributeCumulativeEnergyExportedWithParams
  , readAttributePeriodicEnergyImportedWithParams
  , readAttributePeriodicEnergyExportedWithParams
  , readAttributeCumulativeEnergyResetWithParams
  , readAttributeGeneratedCommandListWithParams
  , readAttributeAcceptedCommandListWithParams
  , readAttributeAttributeListWithParams
  , readAttributeFeatureMapWithParams
  , readAttributeClusterRevisionWithParams
  , init_
  , new
  , initWithDevice_endpointID_queue
  , initSelector
  , initWithDevice_endpointID_queueSelector
  , newSelector
  , readAttributeAcceptedCommandListWithParamsSelector
  , readAttributeAccuracyWithParamsSelector
  , readAttributeAttributeListWithParamsSelector
  , readAttributeClusterRevisionWithParamsSelector
  , readAttributeCumulativeEnergyExportedWithParamsSelector
  , readAttributeCumulativeEnergyImportedWithParamsSelector
  , readAttributeCumulativeEnergyResetWithParamsSelector
  , readAttributeFeatureMapWithParamsSelector
  , readAttributeGeneratedCommandListWithParamsSelector
  , readAttributePeriodicEnergyExportedWithParamsSelector
  , readAttributePeriodicEnergyImportedWithParamsSelector


  ) where

import Foreign.Ptr (Ptr, FunPtr)
import Foreign.C.Types

import ObjC.Runtime.Types
import ObjC.Runtime.Message (sendMessage, sendOwnedMessage, sendClassMessage, sendOwnedClassMessage)
import ObjC.Runtime.Selector (mkSelector)
import ObjC.Runtime.Class (getRequiredClass)

import ObjC.Matter.Internal.Classes
import ObjC.Foundation.Internal.Classes

-- | @- readAttributeAccuracyWithParams:@
readAttributeAccuracyWithParams :: (IsMTRClusterElectricalEnergyMeasurement mtrClusterElectricalEnergyMeasurement, IsMTRReadParams params) => mtrClusterElectricalEnergyMeasurement -> params -> IO (Id NSDictionary)
readAttributeAccuracyWithParams mtrClusterElectricalEnergyMeasurement params =
  sendMessage mtrClusterElectricalEnergyMeasurement readAttributeAccuracyWithParamsSelector (toMTRReadParams params)

-- | @- readAttributeCumulativeEnergyImportedWithParams:@
readAttributeCumulativeEnergyImportedWithParams :: (IsMTRClusterElectricalEnergyMeasurement mtrClusterElectricalEnergyMeasurement, IsMTRReadParams params) => mtrClusterElectricalEnergyMeasurement -> params -> IO (Id NSDictionary)
readAttributeCumulativeEnergyImportedWithParams mtrClusterElectricalEnergyMeasurement params =
  sendMessage mtrClusterElectricalEnergyMeasurement readAttributeCumulativeEnergyImportedWithParamsSelector (toMTRReadParams params)

-- | @- readAttributeCumulativeEnergyExportedWithParams:@
readAttributeCumulativeEnergyExportedWithParams :: (IsMTRClusterElectricalEnergyMeasurement mtrClusterElectricalEnergyMeasurement, IsMTRReadParams params) => mtrClusterElectricalEnergyMeasurement -> params -> IO (Id NSDictionary)
readAttributeCumulativeEnergyExportedWithParams mtrClusterElectricalEnergyMeasurement params =
  sendMessage mtrClusterElectricalEnergyMeasurement readAttributeCumulativeEnergyExportedWithParamsSelector (toMTRReadParams params)

-- | @- readAttributePeriodicEnergyImportedWithParams:@
readAttributePeriodicEnergyImportedWithParams :: (IsMTRClusterElectricalEnergyMeasurement mtrClusterElectricalEnergyMeasurement, IsMTRReadParams params) => mtrClusterElectricalEnergyMeasurement -> params -> IO (Id NSDictionary)
readAttributePeriodicEnergyImportedWithParams mtrClusterElectricalEnergyMeasurement params =
  sendMessage mtrClusterElectricalEnergyMeasurement readAttributePeriodicEnergyImportedWithParamsSelector (toMTRReadParams params)

-- | @- readAttributePeriodicEnergyExportedWithParams:@
readAttributePeriodicEnergyExportedWithParams :: (IsMTRClusterElectricalEnergyMeasurement mtrClusterElectricalEnergyMeasurement, IsMTRReadParams params) => mtrClusterElectricalEnergyMeasurement -> params -> IO (Id NSDictionary)
readAttributePeriodicEnergyExportedWithParams mtrClusterElectricalEnergyMeasurement params =
  sendMessage mtrClusterElectricalEnergyMeasurement readAttributePeriodicEnergyExportedWithParamsSelector (toMTRReadParams params)

-- | @- readAttributeCumulativeEnergyResetWithParams:@
readAttributeCumulativeEnergyResetWithParams :: (IsMTRClusterElectricalEnergyMeasurement mtrClusterElectricalEnergyMeasurement, IsMTRReadParams params) => mtrClusterElectricalEnergyMeasurement -> params -> IO (Id NSDictionary)
readAttributeCumulativeEnergyResetWithParams mtrClusterElectricalEnergyMeasurement params =
  sendMessage mtrClusterElectricalEnergyMeasurement readAttributeCumulativeEnergyResetWithParamsSelector (toMTRReadParams params)

-- | @- readAttributeGeneratedCommandListWithParams:@
readAttributeGeneratedCommandListWithParams :: (IsMTRClusterElectricalEnergyMeasurement mtrClusterElectricalEnergyMeasurement, IsMTRReadParams params) => mtrClusterElectricalEnergyMeasurement -> params -> IO (Id NSDictionary)
readAttributeGeneratedCommandListWithParams mtrClusterElectricalEnergyMeasurement params =
  sendMessage mtrClusterElectricalEnergyMeasurement readAttributeGeneratedCommandListWithParamsSelector (toMTRReadParams params)

-- | @- readAttributeAcceptedCommandListWithParams:@
readAttributeAcceptedCommandListWithParams :: (IsMTRClusterElectricalEnergyMeasurement mtrClusterElectricalEnergyMeasurement, IsMTRReadParams params) => mtrClusterElectricalEnergyMeasurement -> params -> IO (Id NSDictionary)
readAttributeAcceptedCommandListWithParams mtrClusterElectricalEnergyMeasurement params =
  sendMessage mtrClusterElectricalEnergyMeasurement readAttributeAcceptedCommandListWithParamsSelector (toMTRReadParams params)

-- | @- readAttributeAttributeListWithParams:@
readAttributeAttributeListWithParams :: (IsMTRClusterElectricalEnergyMeasurement mtrClusterElectricalEnergyMeasurement, IsMTRReadParams params) => mtrClusterElectricalEnergyMeasurement -> params -> IO (Id NSDictionary)
readAttributeAttributeListWithParams mtrClusterElectricalEnergyMeasurement params =
  sendMessage mtrClusterElectricalEnergyMeasurement readAttributeAttributeListWithParamsSelector (toMTRReadParams params)

-- | @- readAttributeFeatureMapWithParams:@
readAttributeFeatureMapWithParams :: (IsMTRClusterElectricalEnergyMeasurement mtrClusterElectricalEnergyMeasurement, IsMTRReadParams params) => mtrClusterElectricalEnergyMeasurement -> params -> IO (Id NSDictionary)
readAttributeFeatureMapWithParams mtrClusterElectricalEnergyMeasurement params =
  sendMessage mtrClusterElectricalEnergyMeasurement readAttributeFeatureMapWithParamsSelector (toMTRReadParams params)

-- | @- readAttributeClusterRevisionWithParams:@
readAttributeClusterRevisionWithParams :: (IsMTRClusterElectricalEnergyMeasurement mtrClusterElectricalEnergyMeasurement, IsMTRReadParams params) => mtrClusterElectricalEnergyMeasurement -> params -> IO (Id NSDictionary)
readAttributeClusterRevisionWithParams mtrClusterElectricalEnergyMeasurement params =
  sendMessage mtrClusterElectricalEnergyMeasurement readAttributeClusterRevisionWithParamsSelector (toMTRReadParams params)

-- | @- init@
init_ :: IsMTRClusterElectricalEnergyMeasurement mtrClusterElectricalEnergyMeasurement => mtrClusterElectricalEnergyMeasurement -> IO (Id MTRClusterElectricalEnergyMeasurement)
init_ mtrClusterElectricalEnergyMeasurement =
  sendOwnedMessage mtrClusterElectricalEnergyMeasurement initSelector

-- | @+ new@
new :: IO (Id MTRClusterElectricalEnergyMeasurement)
new  =
  do
    cls' <- getRequiredClass "MTRClusterElectricalEnergyMeasurement"
    sendOwnedClassMessage cls' newSelector

-- | The queue is currently unused, but may be used in the future for calling completions for command invocations if commands are added to this cluster.
--
-- ObjC selector: @- initWithDevice:endpointID:queue:@
initWithDevice_endpointID_queue :: (IsMTRClusterElectricalEnergyMeasurement mtrClusterElectricalEnergyMeasurement, IsMTRDevice device, IsNSNumber endpointID, IsNSObject queue) => mtrClusterElectricalEnergyMeasurement -> device -> endpointID -> queue -> IO (Id MTRClusterElectricalEnergyMeasurement)
initWithDevice_endpointID_queue mtrClusterElectricalEnergyMeasurement device endpointID queue =
  sendOwnedMessage mtrClusterElectricalEnergyMeasurement initWithDevice_endpointID_queueSelector (toMTRDevice device) (toNSNumber endpointID) (toNSObject queue)

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @readAttributeAccuracyWithParams:@
readAttributeAccuracyWithParamsSelector :: Selector '[Id MTRReadParams] (Id NSDictionary)
readAttributeAccuracyWithParamsSelector = mkSelector "readAttributeAccuracyWithParams:"

-- | @Selector@ for @readAttributeCumulativeEnergyImportedWithParams:@
readAttributeCumulativeEnergyImportedWithParamsSelector :: Selector '[Id MTRReadParams] (Id NSDictionary)
readAttributeCumulativeEnergyImportedWithParamsSelector = mkSelector "readAttributeCumulativeEnergyImportedWithParams:"

-- | @Selector@ for @readAttributeCumulativeEnergyExportedWithParams:@
readAttributeCumulativeEnergyExportedWithParamsSelector :: Selector '[Id MTRReadParams] (Id NSDictionary)
readAttributeCumulativeEnergyExportedWithParamsSelector = mkSelector "readAttributeCumulativeEnergyExportedWithParams:"

-- | @Selector@ for @readAttributePeriodicEnergyImportedWithParams:@
readAttributePeriodicEnergyImportedWithParamsSelector :: Selector '[Id MTRReadParams] (Id NSDictionary)
readAttributePeriodicEnergyImportedWithParamsSelector = mkSelector "readAttributePeriodicEnergyImportedWithParams:"

-- | @Selector@ for @readAttributePeriodicEnergyExportedWithParams:@
readAttributePeriodicEnergyExportedWithParamsSelector :: Selector '[Id MTRReadParams] (Id NSDictionary)
readAttributePeriodicEnergyExportedWithParamsSelector = mkSelector "readAttributePeriodicEnergyExportedWithParams:"

-- | @Selector@ for @readAttributeCumulativeEnergyResetWithParams:@
readAttributeCumulativeEnergyResetWithParamsSelector :: Selector '[Id MTRReadParams] (Id NSDictionary)
readAttributeCumulativeEnergyResetWithParamsSelector = mkSelector "readAttributeCumulativeEnergyResetWithParams:"

-- | @Selector@ for @readAttributeGeneratedCommandListWithParams:@
readAttributeGeneratedCommandListWithParamsSelector :: Selector '[Id MTRReadParams] (Id NSDictionary)
readAttributeGeneratedCommandListWithParamsSelector = mkSelector "readAttributeGeneratedCommandListWithParams:"

-- | @Selector@ for @readAttributeAcceptedCommandListWithParams:@
readAttributeAcceptedCommandListWithParamsSelector :: Selector '[Id MTRReadParams] (Id NSDictionary)
readAttributeAcceptedCommandListWithParamsSelector = mkSelector "readAttributeAcceptedCommandListWithParams:"

-- | @Selector@ for @readAttributeAttributeListWithParams:@
readAttributeAttributeListWithParamsSelector :: Selector '[Id MTRReadParams] (Id NSDictionary)
readAttributeAttributeListWithParamsSelector = mkSelector "readAttributeAttributeListWithParams:"

-- | @Selector@ for @readAttributeFeatureMapWithParams:@
readAttributeFeatureMapWithParamsSelector :: Selector '[Id MTRReadParams] (Id NSDictionary)
readAttributeFeatureMapWithParamsSelector = mkSelector "readAttributeFeatureMapWithParams:"

-- | @Selector@ for @readAttributeClusterRevisionWithParams:@
readAttributeClusterRevisionWithParamsSelector :: Selector '[Id MTRReadParams] (Id NSDictionary)
readAttributeClusterRevisionWithParamsSelector = mkSelector "readAttributeClusterRevisionWithParams:"

-- | @Selector@ for @init@
initSelector :: Selector '[] (Id MTRClusterElectricalEnergyMeasurement)
initSelector = mkSelector "init"

-- | @Selector@ for @new@
newSelector :: Selector '[] (Id MTRClusterElectricalEnergyMeasurement)
newSelector = mkSelector "new"

-- | @Selector@ for @initWithDevice:endpointID:queue:@
initWithDevice_endpointID_queueSelector :: Selector '[Id MTRDevice, Id NSNumber, Id NSObject] (Id MTRClusterElectricalEnergyMeasurement)
initWithDevice_endpointID_queueSelector = mkSelector "initWithDevice:endpointID:queue:"

