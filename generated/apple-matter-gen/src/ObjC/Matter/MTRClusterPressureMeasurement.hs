{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Cluster Pressure Measurement    Attributes and commands for configuring the measurement of pressure, and reporting pressure measurements.
--
-- Generated bindings for @MTRClusterPressureMeasurement@.
module ObjC.Matter.MTRClusterPressureMeasurement
  ( MTRClusterPressureMeasurement
  , IsMTRClusterPressureMeasurement(..)
  , readAttributeMeasuredValueWithParams
  , readAttributeMinMeasuredValueWithParams
  , readAttributeMaxMeasuredValueWithParams
  , readAttributeToleranceWithParams
  , readAttributeScaledValueWithParams
  , readAttributeMinScaledValueWithParams
  , readAttributeMaxScaledValueWithParams
  , readAttributeScaledToleranceWithParams
  , readAttributeScaleWithParams
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
  , readAttributeMaxMeasuredValueWithParamsSelector
  , readAttributeMaxScaledValueWithParamsSelector
  , readAttributeMeasuredValueWithParamsSelector
  , readAttributeMinMeasuredValueWithParamsSelector
  , readAttributeMinScaledValueWithParamsSelector
  , readAttributeScaleWithParamsSelector
  , readAttributeScaledToleranceWithParamsSelector
  , readAttributeScaledValueWithParamsSelector
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
readAttributeMeasuredValueWithParams :: (IsMTRClusterPressureMeasurement mtrClusterPressureMeasurement, IsMTRReadParams params) => mtrClusterPressureMeasurement -> params -> IO (Id NSDictionary)
readAttributeMeasuredValueWithParams mtrClusterPressureMeasurement params =
  sendMessage mtrClusterPressureMeasurement readAttributeMeasuredValueWithParamsSelector (toMTRReadParams params)

-- | @- readAttributeMinMeasuredValueWithParams:@
readAttributeMinMeasuredValueWithParams :: (IsMTRClusterPressureMeasurement mtrClusterPressureMeasurement, IsMTRReadParams params) => mtrClusterPressureMeasurement -> params -> IO (Id NSDictionary)
readAttributeMinMeasuredValueWithParams mtrClusterPressureMeasurement params =
  sendMessage mtrClusterPressureMeasurement readAttributeMinMeasuredValueWithParamsSelector (toMTRReadParams params)

-- | @- readAttributeMaxMeasuredValueWithParams:@
readAttributeMaxMeasuredValueWithParams :: (IsMTRClusterPressureMeasurement mtrClusterPressureMeasurement, IsMTRReadParams params) => mtrClusterPressureMeasurement -> params -> IO (Id NSDictionary)
readAttributeMaxMeasuredValueWithParams mtrClusterPressureMeasurement params =
  sendMessage mtrClusterPressureMeasurement readAttributeMaxMeasuredValueWithParamsSelector (toMTRReadParams params)

-- | @- readAttributeToleranceWithParams:@
readAttributeToleranceWithParams :: (IsMTRClusterPressureMeasurement mtrClusterPressureMeasurement, IsMTRReadParams params) => mtrClusterPressureMeasurement -> params -> IO (Id NSDictionary)
readAttributeToleranceWithParams mtrClusterPressureMeasurement params =
  sendMessage mtrClusterPressureMeasurement readAttributeToleranceWithParamsSelector (toMTRReadParams params)

-- | @- readAttributeScaledValueWithParams:@
readAttributeScaledValueWithParams :: (IsMTRClusterPressureMeasurement mtrClusterPressureMeasurement, IsMTRReadParams params) => mtrClusterPressureMeasurement -> params -> IO (Id NSDictionary)
readAttributeScaledValueWithParams mtrClusterPressureMeasurement params =
  sendMessage mtrClusterPressureMeasurement readAttributeScaledValueWithParamsSelector (toMTRReadParams params)

-- | @- readAttributeMinScaledValueWithParams:@
readAttributeMinScaledValueWithParams :: (IsMTRClusterPressureMeasurement mtrClusterPressureMeasurement, IsMTRReadParams params) => mtrClusterPressureMeasurement -> params -> IO (Id NSDictionary)
readAttributeMinScaledValueWithParams mtrClusterPressureMeasurement params =
  sendMessage mtrClusterPressureMeasurement readAttributeMinScaledValueWithParamsSelector (toMTRReadParams params)

-- | @- readAttributeMaxScaledValueWithParams:@
readAttributeMaxScaledValueWithParams :: (IsMTRClusterPressureMeasurement mtrClusterPressureMeasurement, IsMTRReadParams params) => mtrClusterPressureMeasurement -> params -> IO (Id NSDictionary)
readAttributeMaxScaledValueWithParams mtrClusterPressureMeasurement params =
  sendMessage mtrClusterPressureMeasurement readAttributeMaxScaledValueWithParamsSelector (toMTRReadParams params)

-- | @- readAttributeScaledToleranceWithParams:@
readAttributeScaledToleranceWithParams :: (IsMTRClusterPressureMeasurement mtrClusterPressureMeasurement, IsMTRReadParams params) => mtrClusterPressureMeasurement -> params -> IO (Id NSDictionary)
readAttributeScaledToleranceWithParams mtrClusterPressureMeasurement params =
  sendMessage mtrClusterPressureMeasurement readAttributeScaledToleranceWithParamsSelector (toMTRReadParams params)

-- | @- readAttributeScaleWithParams:@
readAttributeScaleWithParams :: (IsMTRClusterPressureMeasurement mtrClusterPressureMeasurement, IsMTRReadParams params) => mtrClusterPressureMeasurement -> params -> IO (Id NSDictionary)
readAttributeScaleWithParams mtrClusterPressureMeasurement params =
  sendMessage mtrClusterPressureMeasurement readAttributeScaleWithParamsSelector (toMTRReadParams params)

-- | @- readAttributeGeneratedCommandListWithParams:@
readAttributeGeneratedCommandListWithParams :: (IsMTRClusterPressureMeasurement mtrClusterPressureMeasurement, IsMTRReadParams params) => mtrClusterPressureMeasurement -> params -> IO (Id NSDictionary)
readAttributeGeneratedCommandListWithParams mtrClusterPressureMeasurement params =
  sendMessage mtrClusterPressureMeasurement readAttributeGeneratedCommandListWithParamsSelector (toMTRReadParams params)

-- | @- readAttributeAcceptedCommandListWithParams:@
readAttributeAcceptedCommandListWithParams :: (IsMTRClusterPressureMeasurement mtrClusterPressureMeasurement, IsMTRReadParams params) => mtrClusterPressureMeasurement -> params -> IO (Id NSDictionary)
readAttributeAcceptedCommandListWithParams mtrClusterPressureMeasurement params =
  sendMessage mtrClusterPressureMeasurement readAttributeAcceptedCommandListWithParamsSelector (toMTRReadParams params)

-- | @- readAttributeAttributeListWithParams:@
readAttributeAttributeListWithParams :: (IsMTRClusterPressureMeasurement mtrClusterPressureMeasurement, IsMTRReadParams params) => mtrClusterPressureMeasurement -> params -> IO (Id NSDictionary)
readAttributeAttributeListWithParams mtrClusterPressureMeasurement params =
  sendMessage mtrClusterPressureMeasurement readAttributeAttributeListWithParamsSelector (toMTRReadParams params)

-- | @- readAttributeFeatureMapWithParams:@
readAttributeFeatureMapWithParams :: (IsMTRClusterPressureMeasurement mtrClusterPressureMeasurement, IsMTRReadParams params) => mtrClusterPressureMeasurement -> params -> IO (Id NSDictionary)
readAttributeFeatureMapWithParams mtrClusterPressureMeasurement params =
  sendMessage mtrClusterPressureMeasurement readAttributeFeatureMapWithParamsSelector (toMTRReadParams params)

-- | @- readAttributeClusterRevisionWithParams:@
readAttributeClusterRevisionWithParams :: (IsMTRClusterPressureMeasurement mtrClusterPressureMeasurement, IsMTRReadParams params) => mtrClusterPressureMeasurement -> params -> IO (Id NSDictionary)
readAttributeClusterRevisionWithParams mtrClusterPressureMeasurement params =
  sendMessage mtrClusterPressureMeasurement readAttributeClusterRevisionWithParamsSelector (toMTRReadParams params)

-- | @- init@
init_ :: IsMTRClusterPressureMeasurement mtrClusterPressureMeasurement => mtrClusterPressureMeasurement -> IO (Id MTRClusterPressureMeasurement)
init_ mtrClusterPressureMeasurement =
  sendOwnedMessage mtrClusterPressureMeasurement initSelector

-- | @+ new@
new :: IO (Id MTRClusterPressureMeasurement)
new  =
  do
    cls' <- getRequiredClass "MTRClusterPressureMeasurement"
    sendOwnedClassMessage cls' newSelector

-- | @- initWithDevice:endpoint:queue:@
initWithDevice_endpoint_queue :: (IsMTRClusterPressureMeasurement mtrClusterPressureMeasurement, IsMTRDevice device, IsNSObject queue) => mtrClusterPressureMeasurement -> device -> CUShort -> queue -> IO (Id MTRClusterPressureMeasurement)
initWithDevice_endpoint_queue mtrClusterPressureMeasurement device endpoint queue =
  sendOwnedMessage mtrClusterPressureMeasurement initWithDevice_endpoint_queueSelector (toMTRDevice device) endpoint (toNSObject queue)

-- | The queue is currently unused, but may be used in the future for calling completions for command invocations if commands are added to this cluster.
--
-- ObjC selector: @- initWithDevice:endpointID:queue:@
initWithDevice_endpointID_queue :: (IsMTRClusterPressureMeasurement mtrClusterPressureMeasurement, IsMTRDevice device, IsNSNumber endpointID, IsNSObject queue) => mtrClusterPressureMeasurement -> device -> endpointID -> queue -> IO (Id MTRClusterPressureMeasurement)
initWithDevice_endpointID_queue mtrClusterPressureMeasurement device endpointID queue =
  sendOwnedMessage mtrClusterPressureMeasurement initWithDevice_endpointID_queueSelector (toMTRDevice device) (toNSNumber endpointID) (toNSObject queue)

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

-- | @Selector@ for @readAttributeScaledValueWithParams:@
readAttributeScaledValueWithParamsSelector :: Selector '[Id MTRReadParams] (Id NSDictionary)
readAttributeScaledValueWithParamsSelector = mkSelector "readAttributeScaledValueWithParams:"

-- | @Selector@ for @readAttributeMinScaledValueWithParams:@
readAttributeMinScaledValueWithParamsSelector :: Selector '[Id MTRReadParams] (Id NSDictionary)
readAttributeMinScaledValueWithParamsSelector = mkSelector "readAttributeMinScaledValueWithParams:"

-- | @Selector@ for @readAttributeMaxScaledValueWithParams:@
readAttributeMaxScaledValueWithParamsSelector :: Selector '[Id MTRReadParams] (Id NSDictionary)
readAttributeMaxScaledValueWithParamsSelector = mkSelector "readAttributeMaxScaledValueWithParams:"

-- | @Selector@ for @readAttributeScaledToleranceWithParams:@
readAttributeScaledToleranceWithParamsSelector :: Selector '[Id MTRReadParams] (Id NSDictionary)
readAttributeScaledToleranceWithParamsSelector = mkSelector "readAttributeScaledToleranceWithParams:"

-- | @Selector@ for @readAttributeScaleWithParams:@
readAttributeScaleWithParamsSelector :: Selector '[Id MTRReadParams] (Id NSDictionary)
readAttributeScaleWithParamsSelector = mkSelector "readAttributeScaleWithParams:"

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
initSelector :: Selector '[] (Id MTRClusterPressureMeasurement)
initSelector = mkSelector "init"

-- | @Selector@ for @new@
newSelector :: Selector '[] (Id MTRClusterPressureMeasurement)
newSelector = mkSelector "new"

-- | @Selector@ for @initWithDevice:endpoint:queue:@
initWithDevice_endpoint_queueSelector :: Selector '[Id MTRDevice, CUShort, Id NSObject] (Id MTRClusterPressureMeasurement)
initWithDevice_endpoint_queueSelector = mkSelector "initWithDevice:endpoint:queue:"

-- | @Selector@ for @initWithDevice:endpointID:queue:@
initWithDevice_endpointID_queueSelector :: Selector '[Id MTRDevice, Id NSNumber, Id NSObject] (Id MTRClusterPressureMeasurement)
initWithDevice_endpointID_queueSelector = mkSelector "initWithDevice:endpointID:queue:"

