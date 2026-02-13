{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Cluster Carbon Dioxide Concentration Measurement    Attributes for reporting carbon dioxide concentration measurements
--
-- Generated bindings for @MTRClusterCarbonDioxideConcentrationMeasurement@.
module ObjC.Matter.MTRClusterCarbonDioxideConcentrationMeasurement
  ( MTRClusterCarbonDioxideConcentrationMeasurement
  , IsMTRClusterCarbonDioxideConcentrationMeasurement(..)
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
readAttributeMeasuredValueWithParams :: (IsMTRClusterCarbonDioxideConcentrationMeasurement mtrClusterCarbonDioxideConcentrationMeasurement, IsMTRReadParams params) => mtrClusterCarbonDioxideConcentrationMeasurement -> params -> IO (Id NSDictionary)
readAttributeMeasuredValueWithParams mtrClusterCarbonDioxideConcentrationMeasurement params =
  sendMessage mtrClusterCarbonDioxideConcentrationMeasurement readAttributeMeasuredValueWithParamsSelector (toMTRReadParams params)

-- | @- readAttributeMinMeasuredValueWithParams:@
readAttributeMinMeasuredValueWithParams :: (IsMTRClusterCarbonDioxideConcentrationMeasurement mtrClusterCarbonDioxideConcentrationMeasurement, IsMTRReadParams params) => mtrClusterCarbonDioxideConcentrationMeasurement -> params -> IO (Id NSDictionary)
readAttributeMinMeasuredValueWithParams mtrClusterCarbonDioxideConcentrationMeasurement params =
  sendMessage mtrClusterCarbonDioxideConcentrationMeasurement readAttributeMinMeasuredValueWithParamsSelector (toMTRReadParams params)

-- | @- readAttributeMaxMeasuredValueWithParams:@
readAttributeMaxMeasuredValueWithParams :: (IsMTRClusterCarbonDioxideConcentrationMeasurement mtrClusterCarbonDioxideConcentrationMeasurement, IsMTRReadParams params) => mtrClusterCarbonDioxideConcentrationMeasurement -> params -> IO (Id NSDictionary)
readAttributeMaxMeasuredValueWithParams mtrClusterCarbonDioxideConcentrationMeasurement params =
  sendMessage mtrClusterCarbonDioxideConcentrationMeasurement readAttributeMaxMeasuredValueWithParamsSelector (toMTRReadParams params)

-- | @- readAttributePeakMeasuredValueWithParams:@
readAttributePeakMeasuredValueWithParams :: (IsMTRClusterCarbonDioxideConcentrationMeasurement mtrClusterCarbonDioxideConcentrationMeasurement, IsMTRReadParams params) => mtrClusterCarbonDioxideConcentrationMeasurement -> params -> IO (Id NSDictionary)
readAttributePeakMeasuredValueWithParams mtrClusterCarbonDioxideConcentrationMeasurement params =
  sendMessage mtrClusterCarbonDioxideConcentrationMeasurement readAttributePeakMeasuredValueWithParamsSelector (toMTRReadParams params)

-- | @- readAttributePeakMeasuredValueWindowWithParams:@
readAttributePeakMeasuredValueWindowWithParams :: (IsMTRClusterCarbonDioxideConcentrationMeasurement mtrClusterCarbonDioxideConcentrationMeasurement, IsMTRReadParams params) => mtrClusterCarbonDioxideConcentrationMeasurement -> params -> IO (Id NSDictionary)
readAttributePeakMeasuredValueWindowWithParams mtrClusterCarbonDioxideConcentrationMeasurement params =
  sendMessage mtrClusterCarbonDioxideConcentrationMeasurement readAttributePeakMeasuredValueWindowWithParamsSelector (toMTRReadParams params)

-- | @- readAttributeAverageMeasuredValueWithParams:@
readAttributeAverageMeasuredValueWithParams :: (IsMTRClusterCarbonDioxideConcentrationMeasurement mtrClusterCarbonDioxideConcentrationMeasurement, IsMTRReadParams params) => mtrClusterCarbonDioxideConcentrationMeasurement -> params -> IO (Id NSDictionary)
readAttributeAverageMeasuredValueWithParams mtrClusterCarbonDioxideConcentrationMeasurement params =
  sendMessage mtrClusterCarbonDioxideConcentrationMeasurement readAttributeAverageMeasuredValueWithParamsSelector (toMTRReadParams params)

-- | @- readAttributeAverageMeasuredValueWindowWithParams:@
readAttributeAverageMeasuredValueWindowWithParams :: (IsMTRClusterCarbonDioxideConcentrationMeasurement mtrClusterCarbonDioxideConcentrationMeasurement, IsMTRReadParams params) => mtrClusterCarbonDioxideConcentrationMeasurement -> params -> IO (Id NSDictionary)
readAttributeAverageMeasuredValueWindowWithParams mtrClusterCarbonDioxideConcentrationMeasurement params =
  sendMessage mtrClusterCarbonDioxideConcentrationMeasurement readAttributeAverageMeasuredValueWindowWithParamsSelector (toMTRReadParams params)

-- | @- readAttributeUncertaintyWithParams:@
readAttributeUncertaintyWithParams :: (IsMTRClusterCarbonDioxideConcentrationMeasurement mtrClusterCarbonDioxideConcentrationMeasurement, IsMTRReadParams params) => mtrClusterCarbonDioxideConcentrationMeasurement -> params -> IO (Id NSDictionary)
readAttributeUncertaintyWithParams mtrClusterCarbonDioxideConcentrationMeasurement params =
  sendMessage mtrClusterCarbonDioxideConcentrationMeasurement readAttributeUncertaintyWithParamsSelector (toMTRReadParams params)

-- | @- readAttributeMeasurementUnitWithParams:@
readAttributeMeasurementUnitWithParams :: (IsMTRClusterCarbonDioxideConcentrationMeasurement mtrClusterCarbonDioxideConcentrationMeasurement, IsMTRReadParams params) => mtrClusterCarbonDioxideConcentrationMeasurement -> params -> IO (Id NSDictionary)
readAttributeMeasurementUnitWithParams mtrClusterCarbonDioxideConcentrationMeasurement params =
  sendMessage mtrClusterCarbonDioxideConcentrationMeasurement readAttributeMeasurementUnitWithParamsSelector (toMTRReadParams params)

-- | @- readAttributeMeasurementMediumWithParams:@
readAttributeMeasurementMediumWithParams :: (IsMTRClusterCarbonDioxideConcentrationMeasurement mtrClusterCarbonDioxideConcentrationMeasurement, IsMTRReadParams params) => mtrClusterCarbonDioxideConcentrationMeasurement -> params -> IO (Id NSDictionary)
readAttributeMeasurementMediumWithParams mtrClusterCarbonDioxideConcentrationMeasurement params =
  sendMessage mtrClusterCarbonDioxideConcentrationMeasurement readAttributeMeasurementMediumWithParamsSelector (toMTRReadParams params)

-- | @- readAttributeLevelValueWithParams:@
readAttributeLevelValueWithParams :: (IsMTRClusterCarbonDioxideConcentrationMeasurement mtrClusterCarbonDioxideConcentrationMeasurement, IsMTRReadParams params) => mtrClusterCarbonDioxideConcentrationMeasurement -> params -> IO (Id NSDictionary)
readAttributeLevelValueWithParams mtrClusterCarbonDioxideConcentrationMeasurement params =
  sendMessage mtrClusterCarbonDioxideConcentrationMeasurement readAttributeLevelValueWithParamsSelector (toMTRReadParams params)

-- | @- readAttributeGeneratedCommandListWithParams:@
readAttributeGeneratedCommandListWithParams :: (IsMTRClusterCarbonDioxideConcentrationMeasurement mtrClusterCarbonDioxideConcentrationMeasurement, IsMTRReadParams params) => mtrClusterCarbonDioxideConcentrationMeasurement -> params -> IO (Id NSDictionary)
readAttributeGeneratedCommandListWithParams mtrClusterCarbonDioxideConcentrationMeasurement params =
  sendMessage mtrClusterCarbonDioxideConcentrationMeasurement readAttributeGeneratedCommandListWithParamsSelector (toMTRReadParams params)

-- | @- readAttributeAcceptedCommandListWithParams:@
readAttributeAcceptedCommandListWithParams :: (IsMTRClusterCarbonDioxideConcentrationMeasurement mtrClusterCarbonDioxideConcentrationMeasurement, IsMTRReadParams params) => mtrClusterCarbonDioxideConcentrationMeasurement -> params -> IO (Id NSDictionary)
readAttributeAcceptedCommandListWithParams mtrClusterCarbonDioxideConcentrationMeasurement params =
  sendMessage mtrClusterCarbonDioxideConcentrationMeasurement readAttributeAcceptedCommandListWithParamsSelector (toMTRReadParams params)

-- | @- readAttributeAttributeListWithParams:@
readAttributeAttributeListWithParams :: (IsMTRClusterCarbonDioxideConcentrationMeasurement mtrClusterCarbonDioxideConcentrationMeasurement, IsMTRReadParams params) => mtrClusterCarbonDioxideConcentrationMeasurement -> params -> IO (Id NSDictionary)
readAttributeAttributeListWithParams mtrClusterCarbonDioxideConcentrationMeasurement params =
  sendMessage mtrClusterCarbonDioxideConcentrationMeasurement readAttributeAttributeListWithParamsSelector (toMTRReadParams params)

-- | @- readAttributeFeatureMapWithParams:@
readAttributeFeatureMapWithParams :: (IsMTRClusterCarbonDioxideConcentrationMeasurement mtrClusterCarbonDioxideConcentrationMeasurement, IsMTRReadParams params) => mtrClusterCarbonDioxideConcentrationMeasurement -> params -> IO (Id NSDictionary)
readAttributeFeatureMapWithParams mtrClusterCarbonDioxideConcentrationMeasurement params =
  sendMessage mtrClusterCarbonDioxideConcentrationMeasurement readAttributeFeatureMapWithParamsSelector (toMTRReadParams params)

-- | @- readAttributeClusterRevisionWithParams:@
readAttributeClusterRevisionWithParams :: (IsMTRClusterCarbonDioxideConcentrationMeasurement mtrClusterCarbonDioxideConcentrationMeasurement, IsMTRReadParams params) => mtrClusterCarbonDioxideConcentrationMeasurement -> params -> IO (Id NSDictionary)
readAttributeClusterRevisionWithParams mtrClusterCarbonDioxideConcentrationMeasurement params =
  sendMessage mtrClusterCarbonDioxideConcentrationMeasurement readAttributeClusterRevisionWithParamsSelector (toMTRReadParams params)

-- | @- init@
init_ :: IsMTRClusterCarbonDioxideConcentrationMeasurement mtrClusterCarbonDioxideConcentrationMeasurement => mtrClusterCarbonDioxideConcentrationMeasurement -> IO (Id MTRClusterCarbonDioxideConcentrationMeasurement)
init_ mtrClusterCarbonDioxideConcentrationMeasurement =
  sendOwnedMessage mtrClusterCarbonDioxideConcentrationMeasurement initSelector

-- | @+ new@
new :: IO (Id MTRClusterCarbonDioxideConcentrationMeasurement)
new  =
  do
    cls' <- getRequiredClass "MTRClusterCarbonDioxideConcentrationMeasurement"
    sendOwnedClassMessage cls' newSelector

-- | The queue is currently unused, but may be used in the future for calling completions for command invocations if commands are added to this cluster.
--
-- ObjC selector: @- initWithDevice:endpointID:queue:@
initWithDevice_endpointID_queue :: (IsMTRClusterCarbonDioxideConcentrationMeasurement mtrClusterCarbonDioxideConcentrationMeasurement, IsMTRDevice device, IsNSNumber endpointID, IsNSObject queue) => mtrClusterCarbonDioxideConcentrationMeasurement -> device -> endpointID -> queue -> IO (Id MTRClusterCarbonDioxideConcentrationMeasurement)
initWithDevice_endpointID_queue mtrClusterCarbonDioxideConcentrationMeasurement device endpointID queue =
  sendOwnedMessage mtrClusterCarbonDioxideConcentrationMeasurement initWithDevice_endpointID_queueSelector (toMTRDevice device) (toNSNumber endpointID) (toNSObject queue)

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
initSelector :: Selector '[] (Id MTRClusterCarbonDioxideConcentrationMeasurement)
initSelector = mkSelector "init"

-- | @Selector@ for @new@
newSelector :: Selector '[] (Id MTRClusterCarbonDioxideConcentrationMeasurement)
newSelector = mkSelector "new"

-- | @Selector@ for @initWithDevice:endpointID:queue:@
initWithDevice_endpointID_queueSelector :: Selector '[Id MTRDevice, Id NSNumber, Id NSObject] (Id MTRClusterCarbonDioxideConcentrationMeasurement)
initWithDevice_endpointID_queueSelector = mkSelector "initWithDevice:endpointID:queue:"

