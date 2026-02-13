{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Cluster Temperature Measurement    Attributes and commands for configuring the measurement of temperature, and reporting temperature measurements.
--
-- Generated bindings for @MTRClusterTemperatureMeasurement@.
module ObjC.Matter.MTRClusterTemperatureMeasurement
  ( MTRClusterTemperatureMeasurement
  , IsMTRClusterTemperatureMeasurement(..)
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
readAttributeMeasuredValueWithParams :: (IsMTRClusterTemperatureMeasurement mtrClusterTemperatureMeasurement, IsMTRReadParams params) => mtrClusterTemperatureMeasurement -> params -> IO (Id NSDictionary)
readAttributeMeasuredValueWithParams mtrClusterTemperatureMeasurement params =
  sendMessage mtrClusterTemperatureMeasurement readAttributeMeasuredValueWithParamsSelector (toMTRReadParams params)

-- | @- readAttributeMinMeasuredValueWithParams:@
readAttributeMinMeasuredValueWithParams :: (IsMTRClusterTemperatureMeasurement mtrClusterTemperatureMeasurement, IsMTRReadParams params) => mtrClusterTemperatureMeasurement -> params -> IO (Id NSDictionary)
readAttributeMinMeasuredValueWithParams mtrClusterTemperatureMeasurement params =
  sendMessage mtrClusterTemperatureMeasurement readAttributeMinMeasuredValueWithParamsSelector (toMTRReadParams params)

-- | @- readAttributeMaxMeasuredValueWithParams:@
readAttributeMaxMeasuredValueWithParams :: (IsMTRClusterTemperatureMeasurement mtrClusterTemperatureMeasurement, IsMTRReadParams params) => mtrClusterTemperatureMeasurement -> params -> IO (Id NSDictionary)
readAttributeMaxMeasuredValueWithParams mtrClusterTemperatureMeasurement params =
  sendMessage mtrClusterTemperatureMeasurement readAttributeMaxMeasuredValueWithParamsSelector (toMTRReadParams params)

-- | @- readAttributeToleranceWithParams:@
readAttributeToleranceWithParams :: (IsMTRClusterTemperatureMeasurement mtrClusterTemperatureMeasurement, IsMTRReadParams params) => mtrClusterTemperatureMeasurement -> params -> IO (Id NSDictionary)
readAttributeToleranceWithParams mtrClusterTemperatureMeasurement params =
  sendMessage mtrClusterTemperatureMeasurement readAttributeToleranceWithParamsSelector (toMTRReadParams params)

-- | @- readAttributeGeneratedCommandListWithParams:@
readAttributeGeneratedCommandListWithParams :: (IsMTRClusterTemperatureMeasurement mtrClusterTemperatureMeasurement, IsMTRReadParams params) => mtrClusterTemperatureMeasurement -> params -> IO (Id NSDictionary)
readAttributeGeneratedCommandListWithParams mtrClusterTemperatureMeasurement params =
  sendMessage mtrClusterTemperatureMeasurement readAttributeGeneratedCommandListWithParamsSelector (toMTRReadParams params)

-- | @- readAttributeAcceptedCommandListWithParams:@
readAttributeAcceptedCommandListWithParams :: (IsMTRClusterTemperatureMeasurement mtrClusterTemperatureMeasurement, IsMTRReadParams params) => mtrClusterTemperatureMeasurement -> params -> IO (Id NSDictionary)
readAttributeAcceptedCommandListWithParams mtrClusterTemperatureMeasurement params =
  sendMessage mtrClusterTemperatureMeasurement readAttributeAcceptedCommandListWithParamsSelector (toMTRReadParams params)

-- | @- readAttributeAttributeListWithParams:@
readAttributeAttributeListWithParams :: (IsMTRClusterTemperatureMeasurement mtrClusterTemperatureMeasurement, IsMTRReadParams params) => mtrClusterTemperatureMeasurement -> params -> IO (Id NSDictionary)
readAttributeAttributeListWithParams mtrClusterTemperatureMeasurement params =
  sendMessage mtrClusterTemperatureMeasurement readAttributeAttributeListWithParamsSelector (toMTRReadParams params)

-- | @- readAttributeFeatureMapWithParams:@
readAttributeFeatureMapWithParams :: (IsMTRClusterTemperatureMeasurement mtrClusterTemperatureMeasurement, IsMTRReadParams params) => mtrClusterTemperatureMeasurement -> params -> IO (Id NSDictionary)
readAttributeFeatureMapWithParams mtrClusterTemperatureMeasurement params =
  sendMessage mtrClusterTemperatureMeasurement readAttributeFeatureMapWithParamsSelector (toMTRReadParams params)

-- | @- readAttributeClusterRevisionWithParams:@
readAttributeClusterRevisionWithParams :: (IsMTRClusterTemperatureMeasurement mtrClusterTemperatureMeasurement, IsMTRReadParams params) => mtrClusterTemperatureMeasurement -> params -> IO (Id NSDictionary)
readAttributeClusterRevisionWithParams mtrClusterTemperatureMeasurement params =
  sendMessage mtrClusterTemperatureMeasurement readAttributeClusterRevisionWithParamsSelector (toMTRReadParams params)

-- | @- init@
init_ :: IsMTRClusterTemperatureMeasurement mtrClusterTemperatureMeasurement => mtrClusterTemperatureMeasurement -> IO (Id MTRClusterTemperatureMeasurement)
init_ mtrClusterTemperatureMeasurement =
  sendOwnedMessage mtrClusterTemperatureMeasurement initSelector

-- | @+ new@
new :: IO (Id MTRClusterTemperatureMeasurement)
new  =
  do
    cls' <- getRequiredClass "MTRClusterTemperatureMeasurement"
    sendOwnedClassMessage cls' newSelector

-- | @- initWithDevice:endpoint:queue:@
initWithDevice_endpoint_queue :: (IsMTRClusterTemperatureMeasurement mtrClusterTemperatureMeasurement, IsMTRDevice device, IsNSObject queue) => mtrClusterTemperatureMeasurement -> device -> CUShort -> queue -> IO (Id MTRClusterTemperatureMeasurement)
initWithDevice_endpoint_queue mtrClusterTemperatureMeasurement device endpoint queue =
  sendOwnedMessage mtrClusterTemperatureMeasurement initWithDevice_endpoint_queueSelector (toMTRDevice device) endpoint (toNSObject queue)

-- | The queue is currently unused, but may be used in the future for calling completions for command invocations if commands are added to this cluster.
--
-- ObjC selector: @- initWithDevice:endpointID:queue:@
initWithDevice_endpointID_queue :: (IsMTRClusterTemperatureMeasurement mtrClusterTemperatureMeasurement, IsMTRDevice device, IsNSNumber endpointID, IsNSObject queue) => mtrClusterTemperatureMeasurement -> device -> endpointID -> queue -> IO (Id MTRClusterTemperatureMeasurement)
initWithDevice_endpointID_queue mtrClusterTemperatureMeasurement device endpointID queue =
  sendOwnedMessage mtrClusterTemperatureMeasurement initWithDevice_endpointID_queueSelector (toMTRDevice device) (toNSNumber endpointID) (toNSObject queue)

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
initSelector :: Selector '[] (Id MTRClusterTemperatureMeasurement)
initSelector = mkSelector "init"

-- | @Selector@ for @new@
newSelector :: Selector '[] (Id MTRClusterTemperatureMeasurement)
newSelector = mkSelector "new"

-- | @Selector@ for @initWithDevice:endpoint:queue:@
initWithDevice_endpoint_queueSelector :: Selector '[Id MTRDevice, CUShort, Id NSObject] (Id MTRClusterTemperatureMeasurement)
initWithDevice_endpoint_queueSelector = mkSelector "initWithDevice:endpoint:queue:"

-- | @Selector@ for @initWithDevice:endpointID:queue:@
initWithDevice_endpointID_queueSelector :: Selector '[Id MTRDevice, Id NSNumber, Id NSObject] (Id MTRClusterTemperatureMeasurement)
initWithDevice_endpointID_queueSelector = mkSelector "initWithDevice:endpointID:queue:"

