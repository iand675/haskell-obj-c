{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Cluster Soil Measurement    This cluster provides an interface to soil measurement functionality, including configuration and provision of notifications of soil measurements.
--
-- Generated bindings for @MTRClusterSoilMeasurement@.
module ObjC.Matter.MTRClusterSoilMeasurement
  ( MTRClusterSoilMeasurement
  , IsMTRClusterSoilMeasurement(..)
  , readAttributeSoilMoistureMeasurementLimitsWithParams
  , readAttributeSoilMoistureMeasuredValueWithParams
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
  , readAttributeAttributeListWithParamsSelector
  , readAttributeClusterRevisionWithParamsSelector
  , readAttributeFeatureMapWithParamsSelector
  , readAttributeGeneratedCommandListWithParamsSelector
  , readAttributeSoilMoistureMeasuredValueWithParamsSelector
  , readAttributeSoilMoistureMeasurementLimitsWithParamsSelector


  ) where

import Foreign.Ptr (Ptr, FunPtr)
import Foreign.C.Types

import ObjC.Runtime.Types
import ObjC.Runtime.Message (sendMessage, sendOwnedMessage, sendClassMessage, sendOwnedClassMessage)
import ObjC.Runtime.Selector (mkSelector)
import ObjC.Runtime.Class (getRequiredClass)

import ObjC.Matter.Internal.Classes
import ObjC.Foundation.Internal.Classes

-- | @- readAttributeSoilMoistureMeasurementLimitsWithParams:@
readAttributeSoilMoistureMeasurementLimitsWithParams :: (IsMTRClusterSoilMeasurement mtrClusterSoilMeasurement, IsMTRReadParams params) => mtrClusterSoilMeasurement -> params -> IO (Id NSDictionary)
readAttributeSoilMoistureMeasurementLimitsWithParams mtrClusterSoilMeasurement params =
  sendMessage mtrClusterSoilMeasurement readAttributeSoilMoistureMeasurementLimitsWithParamsSelector (toMTRReadParams params)

-- | @- readAttributeSoilMoistureMeasuredValueWithParams:@
readAttributeSoilMoistureMeasuredValueWithParams :: (IsMTRClusterSoilMeasurement mtrClusterSoilMeasurement, IsMTRReadParams params) => mtrClusterSoilMeasurement -> params -> IO (Id NSDictionary)
readAttributeSoilMoistureMeasuredValueWithParams mtrClusterSoilMeasurement params =
  sendMessage mtrClusterSoilMeasurement readAttributeSoilMoistureMeasuredValueWithParamsSelector (toMTRReadParams params)

-- | @- readAttributeGeneratedCommandListWithParams:@
readAttributeGeneratedCommandListWithParams :: (IsMTRClusterSoilMeasurement mtrClusterSoilMeasurement, IsMTRReadParams params) => mtrClusterSoilMeasurement -> params -> IO (Id NSDictionary)
readAttributeGeneratedCommandListWithParams mtrClusterSoilMeasurement params =
  sendMessage mtrClusterSoilMeasurement readAttributeGeneratedCommandListWithParamsSelector (toMTRReadParams params)

-- | @- readAttributeAcceptedCommandListWithParams:@
readAttributeAcceptedCommandListWithParams :: (IsMTRClusterSoilMeasurement mtrClusterSoilMeasurement, IsMTRReadParams params) => mtrClusterSoilMeasurement -> params -> IO (Id NSDictionary)
readAttributeAcceptedCommandListWithParams mtrClusterSoilMeasurement params =
  sendMessage mtrClusterSoilMeasurement readAttributeAcceptedCommandListWithParamsSelector (toMTRReadParams params)

-- | @- readAttributeAttributeListWithParams:@
readAttributeAttributeListWithParams :: (IsMTRClusterSoilMeasurement mtrClusterSoilMeasurement, IsMTRReadParams params) => mtrClusterSoilMeasurement -> params -> IO (Id NSDictionary)
readAttributeAttributeListWithParams mtrClusterSoilMeasurement params =
  sendMessage mtrClusterSoilMeasurement readAttributeAttributeListWithParamsSelector (toMTRReadParams params)

-- | @- readAttributeFeatureMapWithParams:@
readAttributeFeatureMapWithParams :: (IsMTRClusterSoilMeasurement mtrClusterSoilMeasurement, IsMTRReadParams params) => mtrClusterSoilMeasurement -> params -> IO (Id NSDictionary)
readAttributeFeatureMapWithParams mtrClusterSoilMeasurement params =
  sendMessage mtrClusterSoilMeasurement readAttributeFeatureMapWithParamsSelector (toMTRReadParams params)

-- | @- readAttributeClusterRevisionWithParams:@
readAttributeClusterRevisionWithParams :: (IsMTRClusterSoilMeasurement mtrClusterSoilMeasurement, IsMTRReadParams params) => mtrClusterSoilMeasurement -> params -> IO (Id NSDictionary)
readAttributeClusterRevisionWithParams mtrClusterSoilMeasurement params =
  sendMessage mtrClusterSoilMeasurement readAttributeClusterRevisionWithParamsSelector (toMTRReadParams params)

-- | @- init@
init_ :: IsMTRClusterSoilMeasurement mtrClusterSoilMeasurement => mtrClusterSoilMeasurement -> IO (Id MTRClusterSoilMeasurement)
init_ mtrClusterSoilMeasurement =
  sendOwnedMessage mtrClusterSoilMeasurement initSelector

-- | @+ new@
new :: IO (Id MTRClusterSoilMeasurement)
new  =
  do
    cls' <- getRequiredClass "MTRClusterSoilMeasurement"
    sendOwnedClassMessage cls' newSelector

-- | The queue is currently unused, but may be used in the future for calling completions for command invocations if commands are added to this cluster.
--
-- ObjC selector: @- initWithDevice:endpointID:queue:@
initWithDevice_endpointID_queue :: (IsMTRClusterSoilMeasurement mtrClusterSoilMeasurement, IsMTRDevice device, IsNSNumber endpointID, IsNSObject queue) => mtrClusterSoilMeasurement -> device -> endpointID -> queue -> IO (Id MTRClusterSoilMeasurement)
initWithDevice_endpointID_queue mtrClusterSoilMeasurement device endpointID queue =
  sendOwnedMessage mtrClusterSoilMeasurement initWithDevice_endpointID_queueSelector (toMTRDevice device) (toNSNumber endpointID) (toNSObject queue)

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @readAttributeSoilMoistureMeasurementLimitsWithParams:@
readAttributeSoilMoistureMeasurementLimitsWithParamsSelector :: Selector '[Id MTRReadParams] (Id NSDictionary)
readAttributeSoilMoistureMeasurementLimitsWithParamsSelector = mkSelector "readAttributeSoilMoistureMeasurementLimitsWithParams:"

-- | @Selector@ for @readAttributeSoilMoistureMeasuredValueWithParams:@
readAttributeSoilMoistureMeasuredValueWithParamsSelector :: Selector '[Id MTRReadParams] (Id NSDictionary)
readAttributeSoilMoistureMeasuredValueWithParamsSelector = mkSelector "readAttributeSoilMoistureMeasuredValueWithParams:"

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
initSelector :: Selector '[] (Id MTRClusterSoilMeasurement)
initSelector = mkSelector "init"

-- | @Selector@ for @new@
newSelector :: Selector '[] (Id MTRClusterSoilMeasurement)
newSelector = mkSelector "new"

-- | @Selector@ for @initWithDevice:endpointID:queue:@
initWithDevice_endpointID_queueSelector :: Selector '[Id MTRDevice, Id NSNumber, Id NSObject] (Id MTRClusterSoilMeasurement)
initWithDevice_endpointID_queueSelector = mkSelector "initWithDevice:endpointID:queue:"

