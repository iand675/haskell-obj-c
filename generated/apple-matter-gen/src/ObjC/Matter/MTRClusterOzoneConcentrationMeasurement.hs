{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Cluster Ozone Concentration Measurement    Attributes for reporting ozone concentration measurements
--
-- Generated bindings for @MTRClusterOzoneConcentrationMeasurement@.
module ObjC.Matter.MTRClusterOzoneConcentrationMeasurement
  ( MTRClusterOzoneConcentrationMeasurement
  , IsMTRClusterOzoneConcentrationMeasurement(..)
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
readAttributeMeasuredValueWithParams :: (IsMTRClusterOzoneConcentrationMeasurement mtrClusterOzoneConcentrationMeasurement, IsMTRReadParams params) => mtrClusterOzoneConcentrationMeasurement -> params -> IO (Id NSDictionary)
readAttributeMeasuredValueWithParams mtrClusterOzoneConcentrationMeasurement params =
  sendMessage mtrClusterOzoneConcentrationMeasurement readAttributeMeasuredValueWithParamsSelector (toMTRReadParams params)

-- | @- readAttributeMinMeasuredValueWithParams:@
readAttributeMinMeasuredValueWithParams :: (IsMTRClusterOzoneConcentrationMeasurement mtrClusterOzoneConcentrationMeasurement, IsMTRReadParams params) => mtrClusterOzoneConcentrationMeasurement -> params -> IO (Id NSDictionary)
readAttributeMinMeasuredValueWithParams mtrClusterOzoneConcentrationMeasurement params =
  sendMessage mtrClusterOzoneConcentrationMeasurement readAttributeMinMeasuredValueWithParamsSelector (toMTRReadParams params)

-- | @- readAttributeMaxMeasuredValueWithParams:@
readAttributeMaxMeasuredValueWithParams :: (IsMTRClusterOzoneConcentrationMeasurement mtrClusterOzoneConcentrationMeasurement, IsMTRReadParams params) => mtrClusterOzoneConcentrationMeasurement -> params -> IO (Id NSDictionary)
readAttributeMaxMeasuredValueWithParams mtrClusterOzoneConcentrationMeasurement params =
  sendMessage mtrClusterOzoneConcentrationMeasurement readAttributeMaxMeasuredValueWithParamsSelector (toMTRReadParams params)

-- | @- readAttributePeakMeasuredValueWithParams:@
readAttributePeakMeasuredValueWithParams :: (IsMTRClusterOzoneConcentrationMeasurement mtrClusterOzoneConcentrationMeasurement, IsMTRReadParams params) => mtrClusterOzoneConcentrationMeasurement -> params -> IO (Id NSDictionary)
readAttributePeakMeasuredValueWithParams mtrClusterOzoneConcentrationMeasurement params =
  sendMessage mtrClusterOzoneConcentrationMeasurement readAttributePeakMeasuredValueWithParamsSelector (toMTRReadParams params)

-- | @- readAttributePeakMeasuredValueWindowWithParams:@
readAttributePeakMeasuredValueWindowWithParams :: (IsMTRClusterOzoneConcentrationMeasurement mtrClusterOzoneConcentrationMeasurement, IsMTRReadParams params) => mtrClusterOzoneConcentrationMeasurement -> params -> IO (Id NSDictionary)
readAttributePeakMeasuredValueWindowWithParams mtrClusterOzoneConcentrationMeasurement params =
  sendMessage mtrClusterOzoneConcentrationMeasurement readAttributePeakMeasuredValueWindowWithParamsSelector (toMTRReadParams params)

-- | @- readAttributeAverageMeasuredValueWithParams:@
readAttributeAverageMeasuredValueWithParams :: (IsMTRClusterOzoneConcentrationMeasurement mtrClusterOzoneConcentrationMeasurement, IsMTRReadParams params) => mtrClusterOzoneConcentrationMeasurement -> params -> IO (Id NSDictionary)
readAttributeAverageMeasuredValueWithParams mtrClusterOzoneConcentrationMeasurement params =
  sendMessage mtrClusterOzoneConcentrationMeasurement readAttributeAverageMeasuredValueWithParamsSelector (toMTRReadParams params)

-- | @- readAttributeAverageMeasuredValueWindowWithParams:@
readAttributeAverageMeasuredValueWindowWithParams :: (IsMTRClusterOzoneConcentrationMeasurement mtrClusterOzoneConcentrationMeasurement, IsMTRReadParams params) => mtrClusterOzoneConcentrationMeasurement -> params -> IO (Id NSDictionary)
readAttributeAverageMeasuredValueWindowWithParams mtrClusterOzoneConcentrationMeasurement params =
  sendMessage mtrClusterOzoneConcentrationMeasurement readAttributeAverageMeasuredValueWindowWithParamsSelector (toMTRReadParams params)

-- | @- readAttributeUncertaintyWithParams:@
readAttributeUncertaintyWithParams :: (IsMTRClusterOzoneConcentrationMeasurement mtrClusterOzoneConcentrationMeasurement, IsMTRReadParams params) => mtrClusterOzoneConcentrationMeasurement -> params -> IO (Id NSDictionary)
readAttributeUncertaintyWithParams mtrClusterOzoneConcentrationMeasurement params =
  sendMessage mtrClusterOzoneConcentrationMeasurement readAttributeUncertaintyWithParamsSelector (toMTRReadParams params)

-- | @- readAttributeMeasurementUnitWithParams:@
readAttributeMeasurementUnitWithParams :: (IsMTRClusterOzoneConcentrationMeasurement mtrClusterOzoneConcentrationMeasurement, IsMTRReadParams params) => mtrClusterOzoneConcentrationMeasurement -> params -> IO (Id NSDictionary)
readAttributeMeasurementUnitWithParams mtrClusterOzoneConcentrationMeasurement params =
  sendMessage mtrClusterOzoneConcentrationMeasurement readAttributeMeasurementUnitWithParamsSelector (toMTRReadParams params)

-- | @- readAttributeMeasurementMediumWithParams:@
readAttributeMeasurementMediumWithParams :: (IsMTRClusterOzoneConcentrationMeasurement mtrClusterOzoneConcentrationMeasurement, IsMTRReadParams params) => mtrClusterOzoneConcentrationMeasurement -> params -> IO (Id NSDictionary)
readAttributeMeasurementMediumWithParams mtrClusterOzoneConcentrationMeasurement params =
  sendMessage mtrClusterOzoneConcentrationMeasurement readAttributeMeasurementMediumWithParamsSelector (toMTRReadParams params)

-- | @- readAttributeLevelValueWithParams:@
readAttributeLevelValueWithParams :: (IsMTRClusterOzoneConcentrationMeasurement mtrClusterOzoneConcentrationMeasurement, IsMTRReadParams params) => mtrClusterOzoneConcentrationMeasurement -> params -> IO (Id NSDictionary)
readAttributeLevelValueWithParams mtrClusterOzoneConcentrationMeasurement params =
  sendMessage mtrClusterOzoneConcentrationMeasurement readAttributeLevelValueWithParamsSelector (toMTRReadParams params)

-- | @- readAttributeGeneratedCommandListWithParams:@
readAttributeGeneratedCommandListWithParams :: (IsMTRClusterOzoneConcentrationMeasurement mtrClusterOzoneConcentrationMeasurement, IsMTRReadParams params) => mtrClusterOzoneConcentrationMeasurement -> params -> IO (Id NSDictionary)
readAttributeGeneratedCommandListWithParams mtrClusterOzoneConcentrationMeasurement params =
  sendMessage mtrClusterOzoneConcentrationMeasurement readAttributeGeneratedCommandListWithParamsSelector (toMTRReadParams params)

-- | @- readAttributeAcceptedCommandListWithParams:@
readAttributeAcceptedCommandListWithParams :: (IsMTRClusterOzoneConcentrationMeasurement mtrClusterOzoneConcentrationMeasurement, IsMTRReadParams params) => mtrClusterOzoneConcentrationMeasurement -> params -> IO (Id NSDictionary)
readAttributeAcceptedCommandListWithParams mtrClusterOzoneConcentrationMeasurement params =
  sendMessage mtrClusterOzoneConcentrationMeasurement readAttributeAcceptedCommandListWithParamsSelector (toMTRReadParams params)

-- | @- readAttributeAttributeListWithParams:@
readAttributeAttributeListWithParams :: (IsMTRClusterOzoneConcentrationMeasurement mtrClusterOzoneConcentrationMeasurement, IsMTRReadParams params) => mtrClusterOzoneConcentrationMeasurement -> params -> IO (Id NSDictionary)
readAttributeAttributeListWithParams mtrClusterOzoneConcentrationMeasurement params =
  sendMessage mtrClusterOzoneConcentrationMeasurement readAttributeAttributeListWithParamsSelector (toMTRReadParams params)

-- | @- readAttributeFeatureMapWithParams:@
readAttributeFeatureMapWithParams :: (IsMTRClusterOzoneConcentrationMeasurement mtrClusterOzoneConcentrationMeasurement, IsMTRReadParams params) => mtrClusterOzoneConcentrationMeasurement -> params -> IO (Id NSDictionary)
readAttributeFeatureMapWithParams mtrClusterOzoneConcentrationMeasurement params =
  sendMessage mtrClusterOzoneConcentrationMeasurement readAttributeFeatureMapWithParamsSelector (toMTRReadParams params)

-- | @- readAttributeClusterRevisionWithParams:@
readAttributeClusterRevisionWithParams :: (IsMTRClusterOzoneConcentrationMeasurement mtrClusterOzoneConcentrationMeasurement, IsMTRReadParams params) => mtrClusterOzoneConcentrationMeasurement -> params -> IO (Id NSDictionary)
readAttributeClusterRevisionWithParams mtrClusterOzoneConcentrationMeasurement params =
  sendMessage mtrClusterOzoneConcentrationMeasurement readAttributeClusterRevisionWithParamsSelector (toMTRReadParams params)

-- | @- init@
init_ :: IsMTRClusterOzoneConcentrationMeasurement mtrClusterOzoneConcentrationMeasurement => mtrClusterOzoneConcentrationMeasurement -> IO (Id MTRClusterOzoneConcentrationMeasurement)
init_ mtrClusterOzoneConcentrationMeasurement =
  sendOwnedMessage mtrClusterOzoneConcentrationMeasurement initSelector

-- | @+ new@
new :: IO (Id MTRClusterOzoneConcentrationMeasurement)
new  =
  do
    cls' <- getRequiredClass "MTRClusterOzoneConcentrationMeasurement"
    sendOwnedClassMessage cls' newSelector

-- | The queue is currently unused, but may be used in the future for calling completions for command invocations if commands are added to this cluster.
--
-- ObjC selector: @- initWithDevice:endpointID:queue:@
initWithDevice_endpointID_queue :: (IsMTRClusterOzoneConcentrationMeasurement mtrClusterOzoneConcentrationMeasurement, IsMTRDevice device, IsNSNumber endpointID, IsNSObject queue) => mtrClusterOzoneConcentrationMeasurement -> device -> endpointID -> queue -> IO (Id MTRClusterOzoneConcentrationMeasurement)
initWithDevice_endpointID_queue mtrClusterOzoneConcentrationMeasurement device endpointID queue =
  sendOwnedMessage mtrClusterOzoneConcentrationMeasurement initWithDevice_endpointID_queueSelector (toMTRDevice device) (toNSNumber endpointID) (toNSObject queue)

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
initSelector :: Selector '[] (Id MTRClusterOzoneConcentrationMeasurement)
initSelector = mkSelector "init"

-- | @Selector@ for @new@
newSelector :: Selector '[] (Id MTRClusterOzoneConcentrationMeasurement)
newSelector = mkSelector "new"

-- | @Selector@ for @initWithDevice:endpointID:queue:@
initWithDevice_endpointID_queueSelector :: Selector '[Id MTRDevice, Id NSNumber, Id NSObject] (Id MTRClusterOzoneConcentrationMeasurement)
initWithDevice_endpointID_queueSelector = mkSelector "initWithDevice:endpointID:queue:"

