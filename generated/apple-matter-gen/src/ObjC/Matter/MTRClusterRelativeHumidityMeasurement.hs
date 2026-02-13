{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Cluster Relative Humidity Measurement    Attributes and commands for configuring the measurement of relative humidity, and reporting relative humidity measurements.
--
-- Generated bindings for @MTRClusterRelativeHumidityMeasurement@.
module ObjC.Matter.MTRClusterRelativeHumidityMeasurement
  ( MTRClusterRelativeHumidityMeasurement
  , IsMTRClusterRelativeHumidityMeasurement(..)
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
readAttributeMeasuredValueWithParams :: (IsMTRClusterRelativeHumidityMeasurement mtrClusterRelativeHumidityMeasurement, IsMTRReadParams params) => mtrClusterRelativeHumidityMeasurement -> params -> IO (Id NSDictionary)
readAttributeMeasuredValueWithParams mtrClusterRelativeHumidityMeasurement params =
  sendMessage mtrClusterRelativeHumidityMeasurement readAttributeMeasuredValueWithParamsSelector (toMTRReadParams params)

-- | @- readAttributeMinMeasuredValueWithParams:@
readAttributeMinMeasuredValueWithParams :: (IsMTRClusterRelativeHumidityMeasurement mtrClusterRelativeHumidityMeasurement, IsMTRReadParams params) => mtrClusterRelativeHumidityMeasurement -> params -> IO (Id NSDictionary)
readAttributeMinMeasuredValueWithParams mtrClusterRelativeHumidityMeasurement params =
  sendMessage mtrClusterRelativeHumidityMeasurement readAttributeMinMeasuredValueWithParamsSelector (toMTRReadParams params)

-- | @- readAttributeMaxMeasuredValueWithParams:@
readAttributeMaxMeasuredValueWithParams :: (IsMTRClusterRelativeHumidityMeasurement mtrClusterRelativeHumidityMeasurement, IsMTRReadParams params) => mtrClusterRelativeHumidityMeasurement -> params -> IO (Id NSDictionary)
readAttributeMaxMeasuredValueWithParams mtrClusterRelativeHumidityMeasurement params =
  sendMessage mtrClusterRelativeHumidityMeasurement readAttributeMaxMeasuredValueWithParamsSelector (toMTRReadParams params)

-- | @- readAttributeToleranceWithParams:@
readAttributeToleranceWithParams :: (IsMTRClusterRelativeHumidityMeasurement mtrClusterRelativeHumidityMeasurement, IsMTRReadParams params) => mtrClusterRelativeHumidityMeasurement -> params -> IO (Id NSDictionary)
readAttributeToleranceWithParams mtrClusterRelativeHumidityMeasurement params =
  sendMessage mtrClusterRelativeHumidityMeasurement readAttributeToleranceWithParamsSelector (toMTRReadParams params)

-- | @- readAttributeGeneratedCommandListWithParams:@
readAttributeGeneratedCommandListWithParams :: (IsMTRClusterRelativeHumidityMeasurement mtrClusterRelativeHumidityMeasurement, IsMTRReadParams params) => mtrClusterRelativeHumidityMeasurement -> params -> IO (Id NSDictionary)
readAttributeGeneratedCommandListWithParams mtrClusterRelativeHumidityMeasurement params =
  sendMessage mtrClusterRelativeHumidityMeasurement readAttributeGeneratedCommandListWithParamsSelector (toMTRReadParams params)

-- | @- readAttributeAcceptedCommandListWithParams:@
readAttributeAcceptedCommandListWithParams :: (IsMTRClusterRelativeHumidityMeasurement mtrClusterRelativeHumidityMeasurement, IsMTRReadParams params) => mtrClusterRelativeHumidityMeasurement -> params -> IO (Id NSDictionary)
readAttributeAcceptedCommandListWithParams mtrClusterRelativeHumidityMeasurement params =
  sendMessage mtrClusterRelativeHumidityMeasurement readAttributeAcceptedCommandListWithParamsSelector (toMTRReadParams params)

-- | @- readAttributeAttributeListWithParams:@
readAttributeAttributeListWithParams :: (IsMTRClusterRelativeHumidityMeasurement mtrClusterRelativeHumidityMeasurement, IsMTRReadParams params) => mtrClusterRelativeHumidityMeasurement -> params -> IO (Id NSDictionary)
readAttributeAttributeListWithParams mtrClusterRelativeHumidityMeasurement params =
  sendMessage mtrClusterRelativeHumidityMeasurement readAttributeAttributeListWithParamsSelector (toMTRReadParams params)

-- | @- readAttributeFeatureMapWithParams:@
readAttributeFeatureMapWithParams :: (IsMTRClusterRelativeHumidityMeasurement mtrClusterRelativeHumidityMeasurement, IsMTRReadParams params) => mtrClusterRelativeHumidityMeasurement -> params -> IO (Id NSDictionary)
readAttributeFeatureMapWithParams mtrClusterRelativeHumidityMeasurement params =
  sendMessage mtrClusterRelativeHumidityMeasurement readAttributeFeatureMapWithParamsSelector (toMTRReadParams params)

-- | @- readAttributeClusterRevisionWithParams:@
readAttributeClusterRevisionWithParams :: (IsMTRClusterRelativeHumidityMeasurement mtrClusterRelativeHumidityMeasurement, IsMTRReadParams params) => mtrClusterRelativeHumidityMeasurement -> params -> IO (Id NSDictionary)
readAttributeClusterRevisionWithParams mtrClusterRelativeHumidityMeasurement params =
  sendMessage mtrClusterRelativeHumidityMeasurement readAttributeClusterRevisionWithParamsSelector (toMTRReadParams params)

-- | @- init@
init_ :: IsMTRClusterRelativeHumidityMeasurement mtrClusterRelativeHumidityMeasurement => mtrClusterRelativeHumidityMeasurement -> IO (Id MTRClusterRelativeHumidityMeasurement)
init_ mtrClusterRelativeHumidityMeasurement =
  sendOwnedMessage mtrClusterRelativeHumidityMeasurement initSelector

-- | @+ new@
new :: IO (Id MTRClusterRelativeHumidityMeasurement)
new  =
  do
    cls' <- getRequiredClass "MTRClusterRelativeHumidityMeasurement"
    sendOwnedClassMessage cls' newSelector

-- | @- initWithDevice:endpoint:queue:@
initWithDevice_endpoint_queue :: (IsMTRClusterRelativeHumidityMeasurement mtrClusterRelativeHumidityMeasurement, IsMTRDevice device, IsNSObject queue) => mtrClusterRelativeHumidityMeasurement -> device -> CUShort -> queue -> IO (Id MTRClusterRelativeHumidityMeasurement)
initWithDevice_endpoint_queue mtrClusterRelativeHumidityMeasurement device endpoint queue =
  sendOwnedMessage mtrClusterRelativeHumidityMeasurement initWithDevice_endpoint_queueSelector (toMTRDevice device) endpoint (toNSObject queue)

-- | The queue is currently unused, but may be used in the future for calling completions for command invocations if commands are added to this cluster.
--
-- ObjC selector: @- initWithDevice:endpointID:queue:@
initWithDevice_endpointID_queue :: (IsMTRClusterRelativeHumidityMeasurement mtrClusterRelativeHumidityMeasurement, IsMTRDevice device, IsNSNumber endpointID, IsNSObject queue) => mtrClusterRelativeHumidityMeasurement -> device -> endpointID -> queue -> IO (Id MTRClusterRelativeHumidityMeasurement)
initWithDevice_endpointID_queue mtrClusterRelativeHumidityMeasurement device endpointID queue =
  sendOwnedMessage mtrClusterRelativeHumidityMeasurement initWithDevice_endpointID_queueSelector (toMTRDevice device) (toNSNumber endpointID) (toNSObject queue)

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
initSelector :: Selector '[] (Id MTRClusterRelativeHumidityMeasurement)
initSelector = mkSelector "init"

-- | @Selector@ for @new@
newSelector :: Selector '[] (Id MTRClusterRelativeHumidityMeasurement)
newSelector = mkSelector "new"

-- | @Selector@ for @initWithDevice:endpoint:queue:@
initWithDevice_endpoint_queueSelector :: Selector '[Id MTRDevice, CUShort, Id NSObject] (Id MTRClusterRelativeHumidityMeasurement)
initWithDevice_endpoint_queueSelector = mkSelector "initWithDevice:endpoint:queue:"

-- | @Selector@ for @initWithDevice:endpointID:queue:@
initWithDevice_endpointID_queueSelector :: Selector '[Id MTRDevice, Id NSNumber, Id NSObject] (Id MTRClusterRelativeHumidityMeasurement)
initWithDevice_endpointID_queueSelector = mkSelector "initWithDevice:endpointID:queue:"

