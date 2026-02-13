{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Cluster Illuminance Measurement    Attributes and commands for configuring the measurement of illuminance, and reporting illuminance measurements.
--
-- Generated bindings for @MTRClusterIlluminanceMeasurement@.
module ObjC.Matter.MTRClusterIlluminanceMeasurement
  ( MTRClusterIlluminanceMeasurement
  , IsMTRClusterIlluminanceMeasurement(..)
  , readAttributeMeasuredValueWithParams
  , readAttributeMinMeasuredValueWithParams
  , readAttributeMaxMeasuredValueWithParams
  , readAttributeToleranceWithParams
  , readAttributeLightSensorTypeWithParams
  , readAttributeGeneratedCommandListWithParams
  , readAttributeAcceptedCommandListWithParams
  , readAttributeAttributeListWithParams
  , readAttributeFeatureMapWithParams
  , readAttributeClusterRevisionWithParams
  , init_
  , new
  , initWithDevice_endpoint_queue
  , initWithDevice_endpointID_queue
  , initSelector
  , initWithDevice_endpointID_queueSelector
  , initWithDevice_endpoint_queueSelector
  , newSelector
  , readAttributeAcceptedCommandListWithParamsSelector
  , readAttributeAttributeListWithParamsSelector
  , readAttributeClusterRevisionWithParamsSelector
  , readAttributeFeatureMapWithParamsSelector
  , readAttributeGeneratedCommandListWithParamsSelector
  , readAttributeLightSensorTypeWithParamsSelector
  , readAttributeMaxMeasuredValueWithParamsSelector
  , readAttributeMeasuredValueWithParamsSelector
  , readAttributeMinMeasuredValueWithParamsSelector
  , readAttributeToleranceWithParamsSelector


  ) where

import Foreign.Ptr (Ptr, FunPtr)
import Foreign.C.Types

import ObjC.Runtime.Types
import ObjC.Runtime.Message (sendMessage, sendOwnedMessage, sendClassMessage, sendOwnedClassMessage)
import ObjC.Runtime.Selector (mkSelector)
import ObjC.Runtime.Class (getRequiredClass)

import ObjC.Matter.Internal.Classes
import ObjC.Foundation.Internal.Classes

-- | @- readAttributeMeasuredValueWithParams:@
readAttributeMeasuredValueWithParams :: (IsMTRClusterIlluminanceMeasurement mtrClusterIlluminanceMeasurement, IsMTRReadParams params) => mtrClusterIlluminanceMeasurement -> params -> IO (Id NSDictionary)
readAttributeMeasuredValueWithParams mtrClusterIlluminanceMeasurement params =
  sendMessage mtrClusterIlluminanceMeasurement readAttributeMeasuredValueWithParamsSelector (toMTRReadParams params)

-- | @- readAttributeMinMeasuredValueWithParams:@
readAttributeMinMeasuredValueWithParams :: (IsMTRClusterIlluminanceMeasurement mtrClusterIlluminanceMeasurement, IsMTRReadParams params) => mtrClusterIlluminanceMeasurement -> params -> IO (Id NSDictionary)
readAttributeMinMeasuredValueWithParams mtrClusterIlluminanceMeasurement params =
  sendMessage mtrClusterIlluminanceMeasurement readAttributeMinMeasuredValueWithParamsSelector (toMTRReadParams params)

-- | @- readAttributeMaxMeasuredValueWithParams:@
readAttributeMaxMeasuredValueWithParams :: (IsMTRClusterIlluminanceMeasurement mtrClusterIlluminanceMeasurement, IsMTRReadParams params) => mtrClusterIlluminanceMeasurement -> params -> IO (Id NSDictionary)
readAttributeMaxMeasuredValueWithParams mtrClusterIlluminanceMeasurement params =
  sendMessage mtrClusterIlluminanceMeasurement readAttributeMaxMeasuredValueWithParamsSelector (toMTRReadParams params)

-- | @- readAttributeToleranceWithParams:@
readAttributeToleranceWithParams :: (IsMTRClusterIlluminanceMeasurement mtrClusterIlluminanceMeasurement, IsMTRReadParams params) => mtrClusterIlluminanceMeasurement -> params -> IO (Id NSDictionary)
readAttributeToleranceWithParams mtrClusterIlluminanceMeasurement params =
  sendMessage mtrClusterIlluminanceMeasurement readAttributeToleranceWithParamsSelector (toMTRReadParams params)

-- | @- readAttributeLightSensorTypeWithParams:@
readAttributeLightSensorTypeWithParams :: (IsMTRClusterIlluminanceMeasurement mtrClusterIlluminanceMeasurement, IsMTRReadParams params) => mtrClusterIlluminanceMeasurement -> params -> IO (Id NSDictionary)
readAttributeLightSensorTypeWithParams mtrClusterIlluminanceMeasurement params =
  sendMessage mtrClusterIlluminanceMeasurement readAttributeLightSensorTypeWithParamsSelector (toMTRReadParams params)

-- | @- readAttributeGeneratedCommandListWithParams:@
readAttributeGeneratedCommandListWithParams :: (IsMTRClusterIlluminanceMeasurement mtrClusterIlluminanceMeasurement, IsMTRReadParams params) => mtrClusterIlluminanceMeasurement -> params -> IO (Id NSDictionary)
readAttributeGeneratedCommandListWithParams mtrClusterIlluminanceMeasurement params =
  sendMessage mtrClusterIlluminanceMeasurement readAttributeGeneratedCommandListWithParamsSelector (toMTRReadParams params)

-- | @- readAttributeAcceptedCommandListWithParams:@
readAttributeAcceptedCommandListWithParams :: (IsMTRClusterIlluminanceMeasurement mtrClusterIlluminanceMeasurement, IsMTRReadParams params) => mtrClusterIlluminanceMeasurement -> params -> IO (Id NSDictionary)
readAttributeAcceptedCommandListWithParams mtrClusterIlluminanceMeasurement params =
  sendMessage mtrClusterIlluminanceMeasurement readAttributeAcceptedCommandListWithParamsSelector (toMTRReadParams params)

-- | @- readAttributeAttributeListWithParams:@
readAttributeAttributeListWithParams :: (IsMTRClusterIlluminanceMeasurement mtrClusterIlluminanceMeasurement, IsMTRReadParams params) => mtrClusterIlluminanceMeasurement -> params -> IO (Id NSDictionary)
readAttributeAttributeListWithParams mtrClusterIlluminanceMeasurement params =
  sendMessage mtrClusterIlluminanceMeasurement readAttributeAttributeListWithParamsSelector (toMTRReadParams params)

-- | @- readAttributeFeatureMapWithParams:@
readAttributeFeatureMapWithParams :: (IsMTRClusterIlluminanceMeasurement mtrClusterIlluminanceMeasurement, IsMTRReadParams params) => mtrClusterIlluminanceMeasurement -> params -> IO (Id NSDictionary)
readAttributeFeatureMapWithParams mtrClusterIlluminanceMeasurement params =
  sendMessage mtrClusterIlluminanceMeasurement readAttributeFeatureMapWithParamsSelector (toMTRReadParams params)

-- | @- readAttributeClusterRevisionWithParams:@
readAttributeClusterRevisionWithParams :: (IsMTRClusterIlluminanceMeasurement mtrClusterIlluminanceMeasurement, IsMTRReadParams params) => mtrClusterIlluminanceMeasurement -> params -> IO (Id NSDictionary)
readAttributeClusterRevisionWithParams mtrClusterIlluminanceMeasurement params =
  sendMessage mtrClusterIlluminanceMeasurement readAttributeClusterRevisionWithParamsSelector (toMTRReadParams params)

-- | @- init@
init_ :: IsMTRClusterIlluminanceMeasurement mtrClusterIlluminanceMeasurement => mtrClusterIlluminanceMeasurement -> IO (Id MTRClusterIlluminanceMeasurement)
init_ mtrClusterIlluminanceMeasurement =
  sendOwnedMessage mtrClusterIlluminanceMeasurement initSelector

-- | @+ new@
new :: IO (Id MTRClusterIlluminanceMeasurement)
new  =
  do
    cls' <- getRequiredClass "MTRClusterIlluminanceMeasurement"
    sendOwnedClassMessage cls' newSelector

-- | @- initWithDevice:endpoint:queue:@
initWithDevice_endpoint_queue :: (IsMTRClusterIlluminanceMeasurement mtrClusterIlluminanceMeasurement, IsMTRDevice device, IsNSObject queue) => mtrClusterIlluminanceMeasurement -> device -> CUShort -> queue -> IO (Id MTRClusterIlluminanceMeasurement)
initWithDevice_endpoint_queue mtrClusterIlluminanceMeasurement device endpoint queue =
  sendOwnedMessage mtrClusterIlluminanceMeasurement initWithDevice_endpoint_queueSelector (toMTRDevice device) endpoint (toNSObject queue)

-- | The queue is currently unused, but may be used in the future for calling completions for command invocations if commands are added to this cluster.
--
-- ObjC selector: @- initWithDevice:endpointID:queue:@
initWithDevice_endpointID_queue :: (IsMTRClusterIlluminanceMeasurement mtrClusterIlluminanceMeasurement, IsMTRDevice device, IsNSNumber endpointID, IsNSObject queue) => mtrClusterIlluminanceMeasurement -> device -> endpointID -> queue -> IO (Id MTRClusterIlluminanceMeasurement)
initWithDevice_endpointID_queue mtrClusterIlluminanceMeasurement device endpointID queue =
  sendOwnedMessage mtrClusterIlluminanceMeasurement initWithDevice_endpointID_queueSelector (toMTRDevice device) (toNSNumber endpointID) (toNSObject queue)

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @readAttributeMeasuredValueWithParams:@
readAttributeMeasuredValueWithParamsSelector :: Selector '[Id MTRReadParams] (Id NSDictionary)
readAttributeMeasuredValueWithParamsSelector = mkSelector "readAttributeMeasuredValueWithParams:"

-- | @Selector@ for @readAttributeMinMeasuredValueWithParams:@
readAttributeMinMeasuredValueWithParamsSelector :: Selector '[Id MTRReadParams] (Id NSDictionary)
readAttributeMinMeasuredValueWithParamsSelector = mkSelector "readAttributeMinMeasuredValueWithParams:"

-- | @Selector@ for @readAttributeMaxMeasuredValueWithParams:@
readAttributeMaxMeasuredValueWithParamsSelector :: Selector '[Id MTRReadParams] (Id NSDictionary)
readAttributeMaxMeasuredValueWithParamsSelector = mkSelector "readAttributeMaxMeasuredValueWithParams:"

-- | @Selector@ for @readAttributeToleranceWithParams:@
readAttributeToleranceWithParamsSelector :: Selector '[Id MTRReadParams] (Id NSDictionary)
readAttributeToleranceWithParamsSelector = mkSelector "readAttributeToleranceWithParams:"

-- | @Selector@ for @readAttributeLightSensorTypeWithParams:@
readAttributeLightSensorTypeWithParamsSelector :: Selector '[Id MTRReadParams] (Id NSDictionary)
readAttributeLightSensorTypeWithParamsSelector = mkSelector "readAttributeLightSensorTypeWithParams:"

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
initSelector :: Selector '[] (Id MTRClusterIlluminanceMeasurement)
initSelector = mkSelector "init"

-- | @Selector@ for @new@
newSelector :: Selector '[] (Id MTRClusterIlluminanceMeasurement)
newSelector = mkSelector "new"

-- | @Selector@ for @initWithDevice:endpoint:queue:@
initWithDevice_endpoint_queueSelector :: Selector '[Id MTRDevice, CUShort, Id NSObject] (Id MTRClusterIlluminanceMeasurement)
initWithDevice_endpoint_queueSelector = mkSelector "initWithDevice:endpoint:queue:"

-- | @Selector@ for @initWithDevice:endpointID:queue:@
initWithDevice_endpointID_queueSelector :: Selector '[Id MTRDevice, Id NSNumber, Id NSObject] (Id MTRClusterIlluminanceMeasurement)
initWithDevice_endpointID_queueSelector = mkSelector "initWithDevice:endpointID:queue:"

