{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Cluster PM10 Concentration Measurement    Attributes for reporting PM10 concentration measurements
--
-- Generated bindings for @MTRClusterPM10ConcentrationMeasurement@.
module ObjC.Matter.MTRClusterPM10ConcentrationMeasurement
  ( MTRClusterPM10ConcentrationMeasurement
  , IsMTRClusterPM10ConcentrationMeasurement(..)
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
readAttributeMeasuredValueWithParams :: (IsMTRClusterPM10ConcentrationMeasurement mtrClusterPM10ConcentrationMeasurement, IsMTRReadParams params) => mtrClusterPM10ConcentrationMeasurement -> params -> IO (Id NSDictionary)
readAttributeMeasuredValueWithParams mtrClusterPM10ConcentrationMeasurement params =
  sendMessage mtrClusterPM10ConcentrationMeasurement readAttributeMeasuredValueWithParamsSelector (toMTRReadParams params)

-- | @- readAttributeMinMeasuredValueWithParams:@
readAttributeMinMeasuredValueWithParams :: (IsMTRClusterPM10ConcentrationMeasurement mtrClusterPM10ConcentrationMeasurement, IsMTRReadParams params) => mtrClusterPM10ConcentrationMeasurement -> params -> IO (Id NSDictionary)
readAttributeMinMeasuredValueWithParams mtrClusterPM10ConcentrationMeasurement params =
  sendMessage mtrClusterPM10ConcentrationMeasurement readAttributeMinMeasuredValueWithParamsSelector (toMTRReadParams params)

-- | @- readAttributeMaxMeasuredValueWithParams:@
readAttributeMaxMeasuredValueWithParams :: (IsMTRClusterPM10ConcentrationMeasurement mtrClusterPM10ConcentrationMeasurement, IsMTRReadParams params) => mtrClusterPM10ConcentrationMeasurement -> params -> IO (Id NSDictionary)
readAttributeMaxMeasuredValueWithParams mtrClusterPM10ConcentrationMeasurement params =
  sendMessage mtrClusterPM10ConcentrationMeasurement readAttributeMaxMeasuredValueWithParamsSelector (toMTRReadParams params)

-- | @- readAttributePeakMeasuredValueWithParams:@
readAttributePeakMeasuredValueWithParams :: (IsMTRClusterPM10ConcentrationMeasurement mtrClusterPM10ConcentrationMeasurement, IsMTRReadParams params) => mtrClusterPM10ConcentrationMeasurement -> params -> IO (Id NSDictionary)
readAttributePeakMeasuredValueWithParams mtrClusterPM10ConcentrationMeasurement params =
  sendMessage mtrClusterPM10ConcentrationMeasurement readAttributePeakMeasuredValueWithParamsSelector (toMTRReadParams params)

-- | @- readAttributePeakMeasuredValueWindowWithParams:@
readAttributePeakMeasuredValueWindowWithParams :: (IsMTRClusterPM10ConcentrationMeasurement mtrClusterPM10ConcentrationMeasurement, IsMTRReadParams params) => mtrClusterPM10ConcentrationMeasurement -> params -> IO (Id NSDictionary)
readAttributePeakMeasuredValueWindowWithParams mtrClusterPM10ConcentrationMeasurement params =
  sendMessage mtrClusterPM10ConcentrationMeasurement readAttributePeakMeasuredValueWindowWithParamsSelector (toMTRReadParams params)

-- | @- readAttributeAverageMeasuredValueWithParams:@
readAttributeAverageMeasuredValueWithParams :: (IsMTRClusterPM10ConcentrationMeasurement mtrClusterPM10ConcentrationMeasurement, IsMTRReadParams params) => mtrClusterPM10ConcentrationMeasurement -> params -> IO (Id NSDictionary)
readAttributeAverageMeasuredValueWithParams mtrClusterPM10ConcentrationMeasurement params =
  sendMessage mtrClusterPM10ConcentrationMeasurement readAttributeAverageMeasuredValueWithParamsSelector (toMTRReadParams params)

-- | @- readAttributeAverageMeasuredValueWindowWithParams:@
readAttributeAverageMeasuredValueWindowWithParams :: (IsMTRClusterPM10ConcentrationMeasurement mtrClusterPM10ConcentrationMeasurement, IsMTRReadParams params) => mtrClusterPM10ConcentrationMeasurement -> params -> IO (Id NSDictionary)
readAttributeAverageMeasuredValueWindowWithParams mtrClusterPM10ConcentrationMeasurement params =
  sendMessage mtrClusterPM10ConcentrationMeasurement readAttributeAverageMeasuredValueWindowWithParamsSelector (toMTRReadParams params)

-- | @- readAttributeUncertaintyWithParams:@
readAttributeUncertaintyWithParams :: (IsMTRClusterPM10ConcentrationMeasurement mtrClusterPM10ConcentrationMeasurement, IsMTRReadParams params) => mtrClusterPM10ConcentrationMeasurement -> params -> IO (Id NSDictionary)
readAttributeUncertaintyWithParams mtrClusterPM10ConcentrationMeasurement params =
  sendMessage mtrClusterPM10ConcentrationMeasurement readAttributeUncertaintyWithParamsSelector (toMTRReadParams params)

-- | @- readAttributeMeasurementUnitWithParams:@
readAttributeMeasurementUnitWithParams :: (IsMTRClusterPM10ConcentrationMeasurement mtrClusterPM10ConcentrationMeasurement, IsMTRReadParams params) => mtrClusterPM10ConcentrationMeasurement -> params -> IO (Id NSDictionary)
readAttributeMeasurementUnitWithParams mtrClusterPM10ConcentrationMeasurement params =
  sendMessage mtrClusterPM10ConcentrationMeasurement readAttributeMeasurementUnitWithParamsSelector (toMTRReadParams params)

-- | @- readAttributeMeasurementMediumWithParams:@
readAttributeMeasurementMediumWithParams :: (IsMTRClusterPM10ConcentrationMeasurement mtrClusterPM10ConcentrationMeasurement, IsMTRReadParams params) => mtrClusterPM10ConcentrationMeasurement -> params -> IO (Id NSDictionary)
readAttributeMeasurementMediumWithParams mtrClusterPM10ConcentrationMeasurement params =
  sendMessage mtrClusterPM10ConcentrationMeasurement readAttributeMeasurementMediumWithParamsSelector (toMTRReadParams params)

-- | @- readAttributeLevelValueWithParams:@
readAttributeLevelValueWithParams :: (IsMTRClusterPM10ConcentrationMeasurement mtrClusterPM10ConcentrationMeasurement, IsMTRReadParams params) => mtrClusterPM10ConcentrationMeasurement -> params -> IO (Id NSDictionary)
readAttributeLevelValueWithParams mtrClusterPM10ConcentrationMeasurement params =
  sendMessage mtrClusterPM10ConcentrationMeasurement readAttributeLevelValueWithParamsSelector (toMTRReadParams params)

-- | @- readAttributeGeneratedCommandListWithParams:@
readAttributeGeneratedCommandListWithParams :: (IsMTRClusterPM10ConcentrationMeasurement mtrClusterPM10ConcentrationMeasurement, IsMTRReadParams params) => mtrClusterPM10ConcentrationMeasurement -> params -> IO (Id NSDictionary)
readAttributeGeneratedCommandListWithParams mtrClusterPM10ConcentrationMeasurement params =
  sendMessage mtrClusterPM10ConcentrationMeasurement readAttributeGeneratedCommandListWithParamsSelector (toMTRReadParams params)

-- | @- readAttributeAcceptedCommandListWithParams:@
readAttributeAcceptedCommandListWithParams :: (IsMTRClusterPM10ConcentrationMeasurement mtrClusterPM10ConcentrationMeasurement, IsMTRReadParams params) => mtrClusterPM10ConcentrationMeasurement -> params -> IO (Id NSDictionary)
readAttributeAcceptedCommandListWithParams mtrClusterPM10ConcentrationMeasurement params =
  sendMessage mtrClusterPM10ConcentrationMeasurement readAttributeAcceptedCommandListWithParamsSelector (toMTRReadParams params)

-- | @- readAttributeAttributeListWithParams:@
readAttributeAttributeListWithParams :: (IsMTRClusterPM10ConcentrationMeasurement mtrClusterPM10ConcentrationMeasurement, IsMTRReadParams params) => mtrClusterPM10ConcentrationMeasurement -> params -> IO (Id NSDictionary)
readAttributeAttributeListWithParams mtrClusterPM10ConcentrationMeasurement params =
  sendMessage mtrClusterPM10ConcentrationMeasurement readAttributeAttributeListWithParamsSelector (toMTRReadParams params)

-- | @- readAttributeFeatureMapWithParams:@
readAttributeFeatureMapWithParams :: (IsMTRClusterPM10ConcentrationMeasurement mtrClusterPM10ConcentrationMeasurement, IsMTRReadParams params) => mtrClusterPM10ConcentrationMeasurement -> params -> IO (Id NSDictionary)
readAttributeFeatureMapWithParams mtrClusterPM10ConcentrationMeasurement params =
  sendMessage mtrClusterPM10ConcentrationMeasurement readAttributeFeatureMapWithParamsSelector (toMTRReadParams params)

-- | @- readAttributeClusterRevisionWithParams:@
readAttributeClusterRevisionWithParams :: (IsMTRClusterPM10ConcentrationMeasurement mtrClusterPM10ConcentrationMeasurement, IsMTRReadParams params) => mtrClusterPM10ConcentrationMeasurement -> params -> IO (Id NSDictionary)
readAttributeClusterRevisionWithParams mtrClusterPM10ConcentrationMeasurement params =
  sendMessage mtrClusterPM10ConcentrationMeasurement readAttributeClusterRevisionWithParamsSelector (toMTRReadParams params)

-- | @- init@
init_ :: IsMTRClusterPM10ConcentrationMeasurement mtrClusterPM10ConcentrationMeasurement => mtrClusterPM10ConcentrationMeasurement -> IO (Id MTRClusterPM10ConcentrationMeasurement)
init_ mtrClusterPM10ConcentrationMeasurement =
  sendOwnedMessage mtrClusterPM10ConcentrationMeasurement initSelector

-- | @+ new@
new :: IO (Id MTRClusterPM10ConcentrationMeasurement)
new  =
  do
    cls' <- getRequiredClass "MTRClusterPM10ConcentrationMeasurement"
    sendOwnedClassMessage cls' newSelector

-- | The queue is currently unused, but may be used in the future for calling completions for command invocations if commands are added to this cluster.
--
-- ObjC selector: @- initWithDevice:endpointID:queue:@
initWithDevice_endpointID_queue :: (IsMTRClusterPM10ConcentrationMeasurement mtrClusterPM10ConcentrationMeasurement, IsMTRDevice device, IsNSNumber endpointID, IsNSObject queue) => mtrClusterPM10ConcentrationMeasurement -> device -> endpointID -> queue -> IO (Id MTRClusterPM10ConcentrationMeasurement)
initWithDevice_endpointID_queue mtrClusterPM10ConcentrationMeasurement device endpointID queue =
  sendOwnedMessage mtrClusterPM10ConcentrationMeasurement initWithDevice_endpointID_queueSelector (toMTRDevice device) (toNSNumber endpointID) (toNSObject queue)

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
initSelector :: Selector '[] (Id MTRClusterPM10ConcentrationMeasurement)
initSelector = mkSelector "init"

-- | @Selector@ for @new@
newSelector :: Selector '[] (Id MTRClusterPM10ConcentrationMeasurement)
newSelector = mkSelector "new"

-- | @Selector@ for @initWithDevice:endpointID:queue:@
initWithDevice_endpointID_queueSelector :: Selector '[Id MTRDevice, Id NSNumber, Id NSObject] (Id MTRClusterPM10ConcentrationMeasurement)
initWithDevice_endpointID_queueSelector = mkSelector "initWithDevice:endpointID:queue:"

