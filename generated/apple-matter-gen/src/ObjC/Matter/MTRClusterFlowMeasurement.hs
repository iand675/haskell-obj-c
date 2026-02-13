{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Cluster Flow Measurement    Attributes and commands for configuring the measurement of flow, and reporting flow measurements.
--
-- Generated bindings for @MTRClusterFlowMeasurement@.
module ObjC.Matter.MTRClusterFlowMeasurement
  ( MTRClusterFlowMeasurement
  , IsMTRClusterFlowMeasurement(..)
  , readAttributeMeasuredValueWithParams
  , readAttributeMinMeasuredValueWithParams
  , readAttributeMaxMeasuredValueWithParams
  , readAttributeToleranceWithParams
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
readAttributeMeasuredValueWithParams :: (IsMTRClusterFlowMeasurement mtrClusterFlowMeasurement, IsMTRReadParams params) => mtrClusterFlowMeasurement -> params -> IO (Id NSDictionary)
readAttributeMeasuredValueWithParams mtrClusterFlowMeasurement params =
  sendMessage mtrClusterFlowMeasurement readAttributeMeasuredValueWithParamsSelector (toMTRReadParams params)

-- | @- readAttributeMinMeasuredValueWithParams:@
readAttributeMinMeasuredValueWithParams :: (IsMTRClusterFlowMeasurement mtrClusterFlowMeasurement, IsMTRReadParams params) => mtrClusterFlowMeasurement -> params -> IO (Id NSDictionary)
readAttributeMinMeasuredValueWithParams mtrClusterFlowMeasurement params =
  sendMessage mtrClusterFlowMeasurement readAttributeMinMeasuredValueWithParamsSelector (toMTRReadParams params)

-- | @- readAttributeMaxMeasuredValueWithParams:@
readAttributeMaxMeasuredValueWithParams :: (IsMTRClusterFlowMeasurement mtrClusterFlowMeasurement, IsMTRReadParams params) => mtrClusterFlowMeasurement -> params -> IO (Id NSDictionary)
readAttributeMaxMeasuredValueWithParams mtrClusterFlowMeasurement params =
  sendMessage mtrClusterFlowMeasurement readAttributeMaxMeasuredValueWithParamsSelector (toMTRReadParams params)

-- | @- readAttributeToleranceWithParams:@
readAttributeToleranceWithParams :: (IsMTRClusterFlowMeasurement mtrClusterFlowMeasurement, IsMTRReadParams params) => mtrClusterFlowMeasurement -> params -> IO (Id NSDictionary)
readAttributeToleranceWithParams mtrClusterFlowMeasurement params =
  sendMessage mtrClusterFlowMeasurement readAttributeToleranceWithParamsSelector (toMTRReadParams params)

-- | @- readAttributeGeneratedCommandListWithParams:@
readAttributeGeneratedCommandListWithParams :: (IsMTRClusterFlowMeasurement mtrClusterFlowMeasurement, IsMTRReadParams params) => mtrClusterFlowMeasurement -> params -> IO (Id NSDictionary)
readAttributeGeneratedCommandListWithParams mtrClusterFlowMeasurement params =
  sendMessage mtrClusterFlowMeasurement readAttributeGeneratedCommandListWithParamsSelector (toMTRReadParams params)

-- | @- readAttributeAcceptedCommandListWithParams:@
readAttributeAcceptedCommandListWithParams :: (IsMTRClusterFlowMeasurement mtrClusterFlowMeasurement, IsMTRReadParams params) => mtrClusterFlowMeasurement -> params -> IO (Id NSDictionary)
readAttributeAcceptedCommandListWithParams mtrClusterFlowMeasurement params =
  sendMessage mtrClusterFlowMeasurement readAttributeAcceptedCommandListWithParamsSelector (toMTRReadParams params)

-- | @- readAttributeAttributeListWithParams:@
readAttributeAttributeListWithParams :: (IsMTRClusterFlowMeasurement mtrClusterFlowMeasurement, IsMTRReadParams params) => mtrClusterFlowMeasurement -> params -> IO (Id NSDictionary)
readAttributeAttributeListWithParams mtrClusterFlowMeasurement params =
  sendMessage mtrClusterFlowMeasurement readAttributeAttributeListWithParamsSelector (toMTRReadParams params)

-- | @- readAttributeFeatureMapWithParams:@
readAttributeFeatureMapWithParams :: (IsMTRClusterFlowMeasurement mtrClusterFlowMeasurement, IsMTRReadParams params) => mtrClusterFlowMeasurement -> params -> IO (Id NSDictionary)
readAttributeFeatureMapWithParams mtrClusterFlowMeasurement params =
  sendMessage mtrClusterFlowMeasurement readAttributeFeatureMapWithParamsSelector (toMTRReadParams params)

-- | @- readAttributeClusterRevisionWithParams:@
readAttributeClusterRevisionWithParams :: (IsMTRClusterFlowMeasurement mtrClusterFlowMeasurement, IsMTRReadParams params) => mtrClusterFlowMeasurement -> params -> IO (Id NSDictionary)
readAttributeClusterRevisionWithParams mtrClusterFlowMeasurement params =
  sendMessage mtrClusterFlowMeasurement readAttributeClusterRevisionWithParamsSelector (toMTRReadParams params)

-- | @- init@
init_ :: IsMTRClusterFlowMeasurement mtrClusterFlowMeasurement => mtrClusterFlowMeasurement -> IO (Id MTRClusterFlowMeasurement)
init_ mtrClusterFlowMeasurement =
  sendOwnedMessage mtrClusterFlowMeasurement initSelector

-- | @+ new@
new :: IO (Id MTRClusterFlowMeasurement)
new  =
  do
    cls' <- getRequiredClass "MTRClusterFlowMeasurement"
    sendOwnedClassMessage cls' newSelector

-- | @- initWithDevice:endpoint:queue:@
initWithDevice_endpoint_queue :: (IsMTRClusterFlowMeasurement mtrClusterFlowMeasurement, IsMTRDevice device, IsNSObject queue) => mtrClusterFlowMeasurement -> device -> CUShort -> queue -> IO (Id MTRClusterFlowMeasurement)
initWithDevice_endpoint_queue mtrClusterFlowMeasurement device endpoint queue =
  sendOwnedMessage mtrClusterFlowMeasurement initWithDevice_endpoint_queueSelector (toMTRDevice device) endpoint (toNSObject queue)

-- | The queue is currently unused, but may be used in the future for calling completions for command invocations if commands are added to this cluster.
--
-- ObjC selector: @- initWithDevice:endpointID:queue:@
initWithDevice_endpointID_queue :: (IsMTRClusterFlowMeasurement mtrClusterFlowMeasurement, IsMTRDevice device, IsNSNumber endpointID, IsNSObject queue) => mtrClusterFlowMeasurement -> device -> endpointID -> queue -> IO (Id MTRClusterFlowMeasurement)
initWithDevice_endpointID_queue mtrClusterFlowMeasurement device endpointID queue =
  sendOwnedMessage mtrClusterFlowMeasurement initWithDevice_endpointID_queueSelector (toMTRDevice device) (toNSNumber endpointID) (toNSObject queue)

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
initSelector :: Selector '[] (Id MTRClusterFlowMeasurement)
initSelector = mkSelector "init"

-- | @Selector@ for @new@
newSelector :: Selector '[] (Id MTRClusterFlowMeasurement)
newSelector = mkSelector "new"

-- | @Selector@ for @initWithDevice:endpoint:queue:@
initWithDevice_endpoint_queueSelector :: Selector '[Id MTRDevice, CUShort, Id NSObject] (Id MTRClusterFlowMeasurement)
initWithDevice_endpoint_queueSelector = mkSelector "initWithDevice:endpoint:queue:"

-- | @Selector@ for @initWithDevice:endpointID:queue:@
initWithDevice_endpointID_queueSelector :: Selector '[Id MTRDevice, Id NSNumber, Id NSObject] (Id MTRClusterFlowMeasurement)
initWithDevice_endpointID_queueSelector = mkSelector "initWithDevice:endpointID:queue:"

