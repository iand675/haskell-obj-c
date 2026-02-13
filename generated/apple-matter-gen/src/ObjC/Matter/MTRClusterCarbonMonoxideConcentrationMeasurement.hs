{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Cluster Carbon Monoxide Concentration Measurement    Attributes for reporting carbon monoxide concentration measurements
--
-- Generated bindings for @MTRClusterCarbonMonoxideConcentrationMeasurement@.
module ObjC.Matter.MTRClusterCarbonMonoxideConcentrationMeasurement
  ( MTRClusterCarbonMonoxideConcentrationMeasurement
  , IsMTRClusterCarbonMonoxideConcentrationMeasurement(..)
  , readAttributeMeasuredValueWithParams
  , readAttributeMinMeasuredValueWithParams
  , readAttributeMaxMeasuredValueWithParams
  , readAttributePeakMeasuredValueWithParams
  , readAttributePeakMeasuredValueWindowWithParams
  , readAttributeAverageMeasuredValueWithParams
  , readAttributeAverageMeasuredValueWindowWithParams
  , readAttributeUncertaintyWithParams
  , readAttributeMeasurementUnitWithParams
  , readAttributeMeasurementMediumWithParams
  , readAttributeLevelValueWithParams
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
  , readAttributeAverageMeasuredValueWindowWithParamsSelector
  , readAttributeAverageMeasuredValueWithParamsSelector
  , readAttributeClusterRevisionWithParamsSelector
  , readAttributeFeatureMapWithParamsSelector
  , readAttributeGeneratedCommandListWithParamsSelector
  , readAttributeLevelValueWithParamsSelector
  , readAttributeMaxMeasuredValueWithParamsSelector
  , readAttributeMeasuredValueWithParamsSelector
  , readAttributeMeasurementMediumWithParamsSelector
  , readAttributeMeasurementUnitWithParamsSelector
  , readAttributeMinMeasuredValueWithParamsSelector
  , readAttributePeakMeasuredValueWindowWithParamsSelector
  , readAttributePeakMeasuredValueWithParamsSelector
  , readAttributeUncertaintyWithParamsSelector


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
readAttributeMeasuredValueWithParams :: (IsMTRClusterCarbonMonoxideConcentrationMeasurement mtrClusterCarbonMonoxideConcentrationMeasurement, IsMTRReadParams params) => mtrClusterCarbonMonoxideConcentrationMeasurement -> params -> IO (Id NSDictionary)
readAttributeMeasuredValueWithParams mtrClusterCarbonMonoxideConcentrationMeasurement params =
  sendMessage mtrClusterCarbonMonoxideConcentrationMeasurement readAttributeMeasuredValueWithParamsSelector (toMTRReadParams params)

-- | @- readAttributeMinMeasuredValueWithParams:@
readAttributeMinMeasuredValueWithParams :: (IsMTRClusterCarbonMonoxideConcentrationMeasurement mtrClusterCarbonMonoxideConcentrationMeasurement, IsMTRReadParams params) => mtrClusterCarbonMonoxideConcentrationMeasurement -> params -> IO (Id NSDictionary)
readAttributeMinMeasuredValueWithParams mtrClusterCarbonMonoxideConcentrationMeasurement params =
  sendMessage mtrClusterCarbonMonoxideConcentrationMeasurement readAttributeMinMeasuredValueWithParamsSelector (toMTRReadParams params)

-- | @- readAttributeMaxMeasuredValueWithParams:@
readAttributeMaxMeasuredValueWithParams :: (IsMTRClusterCarbonMonoxideConcentrationMeasurement mtrClusterCarbonMonoxideConcentrationMeasurement, IsMTRReadParams params) => mtrClusterCarbonMonoxideConcentrationMeasurement -> params -> IO (Id NSDictionary)
readAttributeMaxMeasuredValueWithParams mtrClusterCarbonMonoxideConcentrationMeasurement params =
  sendMessage mtrClusterCarbonMonoxideConcentrationMeasurement readAttributeMaxMeasuredValueWithParamsSelector (toMTRReadParams params)

-- | @- readAttributePeakMeasuredValueWithParams:@
readAttributePeakMeasuredValueWithParams :: (IsMTRClusterCarbonMonoxideConcentrationMeasurement mtrClusterCarbonMonoxideConcentrationMeasurement, IsMTRReadParams params) => mtrClusterCarbonMonoxideConcentrationMeasurement -> params -> IO (Id NSDictionary)
readAttributePeakMeasuredValueWithParams mtrClusterCarbonMonoxideConcentrationMeasurement params =
  sendMessage mtrClusterCarbonMonoxideConcentrationMeasurement readAttributePeakMeasuredValueWithParamsSelector (toMTRReadParams params)

-- | @- readAttributePeakMeasuredValueWindowWithParams:@
readAttributePeakMeasuredValueWindowWithParams :: (IsMTRClusterCarbonMonoxideConcentrationMeasurement mtrClusterCarbonMonoxideConcentrationMeasurement, IsMTRReadParams params) => mtrClusterCarbonMonoxideConcentrationMeasurement -> params -> IO (Id NSDictionary)
readAttributePeakMeasuredValueWindowWithParams mtrClusterCarbonMonoxideConcentrationMeasurement params =
  sendMessage mtrClusterCarbonMonoxideConcentrationMeasurement readAttributePeakMeasuredValueWindowWithParamsSelector (toMTRReadParams params)

-- | @- readAttributeAverageMeasuredValueWithParams:@
readAttributeAverageMeasuredValueWithParams :: (IsMTRClusterCarbonMonoxideConcentrationMeasurement mtrClusterCarbonMonoxideConcentrationMeasurement, IsMTRReadParams params) => mtrClusterCarbonMonoxideConcentrationMeasurement -> params -> IO (Id NSDictionary)
readAttributeAverageMeasuredValueWithParams mtrClusterCarbonMonoxideConcentrationMeasurement params =
  sendMessage mtrClusterCarbonMonoxideConcentrationMeasurement readAttributeAverageMeasuredValueWithParamsSelector (toMTRReadParams params)

-- | @- readAttributeAverageMeasuredValueWindowWithParams:@
readAttributeAverageMeasuredValueWindowWithParams :: (IsMTRClusterCarbonMonoxideConcentrationMeasurement mtrClusterCarbonMonoxideConcentrationMeasurement, IsMTRReadParams params) => mtrClusterCarbonMonoxideConcentrationMeasurement -> params -> IO (Id NSDictionary)
readAttributeAverageMeasuredValueWindowWithParams mtrClusterCarbonMonoxideConcentrationMeasurement params =
  sendMessage mtrClusterCarbonMonoxideConcentrationMeasurement readAttributeAverageMeasuredValueWindowWithParamsSelector (toMTRReadParams params)

-- | @- readAttributeUncertaintyWithParams:@
readAttributeUncertaintyWithParams :: (IsMTRClusterCarbonMonoxideConcentrationMeasurement mtrClusterCarbonMonoxideConcentrationMeasurement, IsMTRReadParams params) => mtrClusterCarbonMonoxideConcentrationMeasurement -> params -> IO (Id NSDictionary)
readAttributeUncertaintyWithParams mtrClusterCarbonMonoxideConcentrationMeasurement params =
  sendMessage mtrClusterCarbonMonoxideConcentrationMeasurement readAttributeUncertaintyWithParamsSelector (toMTRReadParams params)

-- | @- readAttributeMeasurementUnitWithParams:@
readAttributeMeasurementUnitWithParams :: (IsMTRClusterCarbonMonoxideConcentrationMeasurement mtrClusterCarbonMonoxideConcentrationMeasurement, IsMTRReadParams params) => mtrClusterCarbonMonoxideConcentrationMeasurement -> params -> IO (Id NSDictionary)
readAttributeMeasurementUnitWithParams mtrClusterCarbonMonoxideConcentrationMeasurement params =
  sendMessage mtrClusterCarbonMonoxideConcentrationMeasurement readAttributeMeasurementUnitWithParamsSelector (toMTRReadParams params)

-- | @- readAttributeMeasurementMediumWithParams:@
readAttributeMeasurementMediumWithParams :: (IsMTRClusterCarbonMonoxideConcentrationMeasurement mtrClusterCarbonMonoxideConcentrationMeasurement, IsMTRReadParams params) => mtrClusterCarbonMonoxideConcentrationMeasurement -> params -> IO (Id NSDictionary)
readAttributeMeasurementMediumWithParams mtrClusterCarbonMonoxideConcentrationMeasurement params =
  sendMessage mtrClusterCarbonMonoxideConcentrationMeasurement readAttributeMeasurementMediumWithParamsSelector (toMTRReadParams params)

-- | @- readAttributeLevelValueWithParams:@
readAttributeLevelValueWithParams :: (IsMTRClusterCarbonMonoxideConcentrationMeasurement mtrClusterCarbonMonoxideConcentrationMeasurement, IsMTRReadParams params) => mtrClusterCarbonMonoxideConcentrationMeasurement -> params -> IO (Id NSDictionary)
readAttributeLevelValueWithParams mtrClusterCarbonMonoxideConcentrationMeasurement params =
  sendMessage mtrClusterCarbonMonoxideConcentrationMeasurement readAttributeLevelValueWithParamsSelector (toMTRReadParams params)

-- | @- readAttributeGeneratedCommandListWithParams:@
readAttributeGeneratedCommandListWithParams :: (IsMTRClusterCarbonMonoxideConcentrationMeasurement mtrClusterCarbonMonoxideConcentrationMeasurement, IsMTRReadParams params) => mtrClusterCarbonMonoxideConcentrationMeasurement -> params -> IO (Id NSDictionary)
readAttributeGeneratedCommandListWithParams mtrClusterCarbonMonoxideConcentrationMeasurement params =
  sendMessage mtrClusterCarbonMonoxideConcentrationMeasurement readAttributeGeneratedCommandListWithParamsSelector (toMTRReadParams params)

-- | @- readAttributeAcceptedCommandListWithParams:@
readAttributeAcceptedCommandListWithParams :: (IsMTRClusterCarbonMonoxideConcentrationMeasurement mtrClusterCarbonMonoxideConcentrationMeasurement, IsMTRReadParams params) => mtrClusterCarbonMonoxideConcentrationMeasurement -> params -> IO (Id NSDictionary)
readAttributeAcceptedCommandListWithParams mtrClusterCarbonMonoxideConcentrationMeasurement params =
  sendMessage mtrClusterCarbonMonoxideConcentrationMeasurement readAttributeAcceptedCommandListWithParamsSelector (toMTRReadParams params)

-- | @- readAttributeAttributeListWithParams:@
readAttributeAttributeListWithParams :: (IsMTRClusterCarbonMonoxideConcentrationMeasurement mtrClusterCarbonMonoxideConcentrationMeasurement, IsMTRReadParams params) => mtrClusterCarbonMonoxideConcentrationMeasurement -> params -> IO (Id NSDictionary)
readAttributeAttributeListWithParams mtrClusterCarbonMonoxideConcentrationMeasurement params =
  sendMessage mtrClusterCarbonMonoxideConcentrationMeasurement readAttributeAttributeListWithParamsSelector (toMTRReadParams params)

-- | @- readAttributeFeatureMapWithParams:@
readAttributeFeatureMapWithParams :: (IsMTRClusterCarbonMonoxideConcentrationMeasurement mtrClusterCarbonMonoxideConcentrationMeasurement, IsMTRReadParams params) => mtrClusterCarbonMonoxideConcentrationMeasurement -> params -> IO (Id NSDictionary)
readAttributeFeatureMapWithParams mtrClusterCarbonMonoxideConcentrationMeasurement params =
  sendMessage mtrClusterCarbonMonoxideConcentrationMeasurement readAttributeFeatureMapWithParamsSelector (toMTRReadParams params)

-- | @- readAttributeClusterRevisionWithParams:@
readAttributeClusterRevisionWithParams :: (IsMTRClusterCarbonMonoxideConcentrationMeasurement mtrClusterCarbonMonoxideConcentrationMeasurement, IsMTRReadParams params) => mtrClusterCarbonMonoxideConcentrationMeasurement -> params -> IO (Id NSDictionary)
readAttributeClusterRevisionWithParams mtrClusterCarbonMonoxideConcentrationMeasurement params =
  sendMessage mtrClusterCarbonMonoxideConcentrationMeasurement readAttributeClusterRevisionWithParamsSelector (toMTRReadParams params)

-- | @- init@
init_ :: IsMTRClusterCarbonMonoxideConcentrationMeasurement mtrClusterCarbonMonoxideConcentrationMeasurement => mtrClusterCarbonMonoxideConcentrationMeasurement -> IO (Id MTRClusterCarbonMonoxideConcentrationMeasurement)
init_ mtrClusterCarbonMonoxideConcentrationMeasurement =
  sendOwnedMessage mtrClusterCarbonMonoxideConcentrationMeasurement initSelector

-- | @+ new@
new :: IO (Id MTRClusterCarbonMonoxideConcentrationMeasurement)
new  =
  do
    cls' <- getRequiredClass "MTRClusterCarbonMonoxideConcentrationMeasurement"
    sendOwnedClassMessage cls' newSelector

-- | The queue is currently unused, but may be used in the future for calling completions for command invocations if commands are added to this cluster.
--
-- ObjC selector: @- initWithDevice:endpointID:queue:@
initWithDevice_endpointID_queue :: (IsMTRClusterCarbonMonoxideConcentrationMeasurement mtrClusterCarbonMonoxideConcentrationMeasurement, IsMTRDevice device, IsNSNumber endpointID, IsNSObject queue) => mtrClusterCarbonMonoxideConcentrationMeasurement -> device -> endpointID -> queue -> IO (Id MTRClusterCarbonMonoxideConcentrationMeasurement)
initWithDevice_endpointID_queue mtrClusterCarbonMonoxideConcentrationMeasurement device endpointID queue =
  sendOwnedMessage mtrClusterCarbonMonoxideConcentrationMeasurement initWithDevice_endpointID_queueSelector (toMTRDevice device) (toNSNumber endpointID) (toNSObject queue)

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

-- | @Selector@ for @readAttributePeakMeasuredValueWithParams:@
readAttributePeakMeasuredValueWithParamsSelector :: Selector '[Id MTRReadParams] (Id NSDictionary)
readAttributePeakMeasuredValueWithParamsSelector = mkSelector "readAttributePeakMeasuredValueWithParams:"

-- | @Selector@ for @readAttributePeakMeasuredValueWindowWithParams:@
readAttributePeakMeasuredValueWindowWithParamsSelector :: Selector '[Id MTRReadParams] (Id NSDictionary)
readAttributePeakMeasuredValueWindowWithParamsSelector = mkSelector "readAttributePeakMeasuredValueWindowWithParams:"

-- | @Selector@ for @readAttributeAverageMeasuredValueWithParams:@
readAttributeAverageMeasuredValueWithParamsSelector :: Selector '[Id MTRReadParams] (Id NSDictionary)
readAttributeAverageMeasuredValueWithParamsSelector = mkSelector "readAttributeAverageMeasuredValueWithParams:"

-- | @Selector@ for @readAttributeAverageMeasuredValueWindowWithParams:@
readAttributeAverageMeasuredValueWindowWithParamsSelector :: Selector '[Id MTRReadParams] (Id NSDictionary)
readAttributeAverageMeasuredValueWindowWithParamsSelector = mkSelector "readAttributeAverageMeasuredValueWindowWithParams:"

-- | @Selector@ for @readAttributeUncertaintyWithParams:@
readAttributeUncertaintyWithParamsSelector :: Selector '[Id MTRReadParams] (Id NSDictionary)
readAttributeUncertaintyWithParamsSelector = mkSelector "readAttributeUncertaintyWithParams:"

-- | @Selector@ for @readAttributeMeasurementUnitWithParams:@
readAttributeMeasurementUnitWithParamsSelector :: Selector '[Id MTRReadParams] (Id NSDictionary)
readAttributeMeasurementUnitWithParamsSelector = mkSelector "readAttributeMeasurementUnitWithParams:"

-- | @Selector@ for @readAttributeMeasurementMediumWithParams:@
readAttributeMeasurementMediumWithParamsSelector :: Selector '[Id MTRReadParams] (Id NSDictionary)
readAttributeMeasurementMediumWithParamsSelector = mkSelector "readAttributeMeasurementMediumWithParams:"

-- | @Selector@ for @readAttributeLevelValueWithParams:@
readAttributeLevelValueWithParamsSelector :: Selector '[Id MTRReadParams] (Id NSDictionary)
readAttributeLevelValueWithParamsSelector = mkSelector "readAttributeLevelValueWithParams:"

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
initSelector :: Selector '[] (Id MTRClusterCarbonMonoxideConcentrationMeasurement)
initSelector = mkSelector "init"

-- | @Selector@ for @new@
newSelector :: Selector '[] (Id MTRClusterCarbonMonoxideConcentrationMeasurement)
newSelector = mkSelector "new"

-- | @Selector@ for @initWithDevice:endpointID:queue:@
initWithDevice_endpointID_queueSelector :: Selector '[Id MTRDevice, Id NSNumber, Id NSObject] (Id MTRClusterCarbonMonoxideConcentrationMeasurement)
initWithDevice_endpointID_queueSelector = mkSelector "initWithDevice:endpointID:queue:"

