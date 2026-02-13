{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Cluster Formaldehyde Concentration Measurement    Attributes for reporting formaldehyde concentration measurements
--
-- Generated bindings for @MTRClusterFormaldehydeConcentrationMeasurement@.
module ObjC.Matter.MTRClusterFormaldehydeConcentrationMeasurement
  ( MTRClusterFormaldehydeConcentrationMeasurement
  , IsMTRClusterFormaldehydeConcentrationMeasurement(..)
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
readAttributeMeasuredValueWithParams :: (IsMTRClusterFormaldehydeConcentrationMeasurement mtrClusterFormaldehydeConcentrationMeasurement, IsMTRReadParams params) => mtrClusterFormaldehydeConcentrationMeasurement -> params -> IO (Id NSDictionary)
readAttributeMeasuredValueWithParams mtrClusterFormaldehydeConcentrationMeasurement params =
  sendMessage mtrClusterFormaldehydeConcentrationMeasurement readAttributeMeasuredValueWithParamsSelector (toMTRReadParams params)

-- | @- readAttributeMinMeasuredValueWithParams:@
readAttributeMinMeasuredValueWithParams :: (IsMTRClusterFormaldehydeConcentrationMeasurement mtrClusterFormaldehydeConcentrationMeasurement, IsMTRReadParams params) => mtrClusterFormaldehydeConcentrationMeasurement -> params -> IO (Id NSDictionary)
readAttributeMinMeasuredValueWithParams mtrClusterFormaldehydeConcentrationMeasurement params =
  sendMessage mtrClusterFormaldehydeConcentrationMeasurement readAttributeMinMeasuredValueWithParamsSelector (toMTRReadParams params)

-- | @- readAttributeMaxMeasuredValueWithParams:@
readAttributeMaxMeasuredValueWithParams :: (IsMTRClusterFormaldehydeConcentrationMeasurement mtrClusterFormaldehydeConcentrationMeasurement, IsMTRReadParams params) => mtrClusterFormaldehydeConcentrationMeasurement -> params -> IO (Id NSDictionary)
readAttributeMaxMeasuredValueWithParams mtrClusterFormaldehydeConcentrationMeasurement params =
  sendMessage mtrClusterFormaldehydeConcentrationMeasurement readAttributeMaxMeasuredValueWithParamsSelector (toMTRReadParams params)

-- | @- readAttributePeakMeasuredValueWithParams:@
readAttributePeakMeasuredValueWithParams :: (IsMTRClusterFormaldehydeConcentrationMeasurement mtrClusterFormaldehydeConcentrationMeasurement, IsMTRReadParams params) => mtrClusterFormaldehydeConcentrationMeasurement -> params -> IO (Id NSDictionary)
readAttributePeakMeasuredValueWithParams mtrClusterFormaldehydeConcentrationMeasurement params =
  sendMessage mtrClusterFormaldehydeConcentrationMeasurement readAttributePeakMeasuredValueWithParamsSelector (toMTRReadParams params)

-- | @- readAttributePeakMeasuredValueWindowWithParams:@
readAttributePeakMeasuredValueWindowWithParams :: (IsMTRClusterFormaldehydeConcentrationMeasurement mtrClusterFormaldehydeConcentrationMeasurement, IsMTRReadParams params) => mtrClusterFormaldehydeConcentrationMeasurement -> params -> IO (Id NSDictionary)
readAttributePeakMeasuredValueWindowWithParams mtrClusterFormaldehydeConcentrationMeasurement params =
  sendMessage mtrClusterFormaldehydeConcentrationMeasurement readAttributePeakMeasuredValueWindowWithParamsSelector (toMTRReadParams params)

-- | @- readAttributeAverageMeasuredValueWithParams:@
readAttributeAverageMeasuredValueWithParams :: (IsMTRClusterFormaldehydeConcentrationMeasurement mtrClusterFormaldehydeConcentrationMeasurement, IsMTRReadParams params) => mtrClusterFormaldehydeConcentrationMeasurement -> params -> IO (Id NSDictionary)
readAttributeAverageMeasuredValueWithParams mtrClusterFormaldehydeConcentrationMeasurement params =
  sendMessage mtrClusterFormaldehydeConcentrationMeasurement readAttributeAverageMeasuredValueWithParamsSelector (toMTRReadParams params)

-- | @- readAttributeAverageMeasuredValueWindowWithParams:@
readAttributeAverageMeasuredValueWindowWithParams :: (IsMTRClusterFormaldehydeConcentrationMeasurement mtrClusterFormaldehydeConcentrationMeasurement, IsMTRReadParams params) => mtrClusterFormaldehydeConcentrationMeasurement -> params -> IO (Id NSDictionary)
readAttributeAverageMeasuredValueWindowWithParams mtrClusterFormaldehydeConcentrationMeasurement params =
  sendMessage mtrClusterFormaldehydeConcentrationMeasurement readAttributeAverageMeasuredValueWindowWithParamsSelector (toMTRReadParams params)

-- | @- readAttributeUncertaintyWithParams:@
readAttributeUncertaintyWithParams :: (IsMTRClusterFormaldehydeConcentrationMeasurement mtrClusterFormaldehydeConcentrationMeasurement, IsMTRReadParams params) => mtrClusterFormaldehydeConcentrationMeasurement -> params -> IO (Id NSDictionary)
readAttributeUncertaintyWithParams mtrClusterFormaldehydeConcentrationMeasurement params =
  sendMessage mtrClusterFormaldehydeConcentrationMeasurement readAttributeUncertaintyWithParamsSelector (toMTRReadParams params)

-- | @- readAttributeMeasurementUnitWithParams:@
readAttributeMeasurementUnitWithParams :: (IsMTRClusterFormaldehydeConcentrationMeasurement mtrClusterFormaldehydeConcentrationMeasurement, IsMTRReadParams params) => mtrClusterFormaldehydeConcentrationMeasurement -> params -> IO (Id NSDictionary)
readAttributeMeasurementUnitWithParams mtrClusterFormaldehydeConcentrationMeasurement params =
  sendMessage mtrClusterFormaldehydeConcentrationMeasurement readAttributeMeasurementUnitWithParamsSelector (toMTRReadParams params)

-- | @- readAttributeMeasurementMediumWithParams:@
readAttributeMeasurementMediumWithParams :: (IsMTRClusterFormaldehydeConcentrationMeasurement mtrClusterFormaldehydeConcentrationMeasurement, IsMTRReadParams params) => mtrClusterFormaldehydeConcentrationMeasurement -> params -> IO (Id NSDictionary)
readAttributeMeasurementMediumWithParams mtrClusterFormaldehydeConcentrationMeasurement params =
  sendMessage mtrClusterFormaldehydeConcentrationMeasurement readAttributeMeasurementMediumWithParamsSelector (toMTRReadParams params)

-- | @- readAttributeLevelValueWithParams:@
readAttributeLevelValueWithParams :: (IsMTRClusterFormaldehydeConcentrationMeasurement mtrClusterFormaldehydeConcentrationMeasurement, IsMTRReadParams params) => mtrClusterFormaldehydeConcentrationMeasurement -> params -> IO (Id NSDictionary)
readAttributeLevelValueWithParams mtrClusterFormaldehydeConcentrationMeasurement params =
  sendMessage mtrClusterFormaldehydeConcentrationMeasurement readAttributeLevelValueWithParamsSelector (toMTRReadParams params)

-- | @- readAttributeGeneratedCommandListWithParams:@
readAttributeGeneratedCommandListWithParams :: (IsMTRClusterFormaldehydeConcentrationMeasurement mtrClusterFormaldehydeConcentrationMeasurement, IsMTRReadParams params) => mtrClusterFormaldehydeConcentrationMeasurement -> params -> IO (Id NSDictionary)
readAttributeGeneratedCommandListWithParams mtrClusterFormaldehydeConcentrationMeasurement params =
  sendMessage mtrClusterFormaldehydeConcentrationMeasurement readAttributeGeneratedCommandListWithParamsSelector (toMTRReadParams params)

-- | @- readAttributeAcceptedCommandListWithParams:@
readAttributeAcceptedCommandListWithParams :: (IsMTRClusterFormaldehydeConcentrationMeasurement mtrClusterFormaldehydeConcentrationMeasurement, IsMTRReadParams params) => mtrClusterFormaldehydeConcentrationMeasurement -> params -> IO (Id NSDictionary)
readAttributeAcceptedCommandListWithParams mtrClusterFormaldehydeConcentrationMeasurement params =
  sendMessage mtrClusterFormaldehydeConcentrationMeasurement readAttributeAcceptedCommandListWithParamsSelector (toMTRReadParams params)

-- | @- readAttributeAttributeListWithParams:@
readAttributeAttributeListWithParams :: (IsMTRClusterFormaldehydeConcentrationMeasurement mtrClusterFormaldehydeConcentrationMeasurement, IsMTRReadParams params) => mtrClusterFormaldehydeConcentrationMeasurement -> params -> IO (Id NSDictionary)
readAttributeAttributeListWithParams mtrClusterFormaldehydeConcentrationMeasurement params =
  sendMessage mtrClusterFormaldehydeConcentrationMeasurement readAttributeAttributeListWithParamsSelector (toMTRReadParams params)

-- | @- readAttributeFeatureMapWithParams:@
readAttributeFeatureMapWithParams :: (IsMTRClusterFormaldehydeConcentrationMeasurement mtrClusterFormaldehydeConcentrationMeasurement, IsMTRReadParams params) => mtrClusterFormaldehydeConcentrationMeasurement -> params -> IO (Id NSDictionary)
readAttributeFeatureMapWithParams mtrClusterFormaldehydeConcentrationMeasurement params =
  sendMessage mtrClusterFormaldehydeConcentrationMeasurement readAttributeFeatureMapWithParamsSelector (toMTRReadParams params)

-- | @- readAttributeClusterRevisionWithParams:@
readAttributeClusterRevisionWithParams :: (IsMTRClusterFormaldehydeConcentrationMeasurement mtrClusterFormaldehydeConcentrationMeasurement, IsMTRReadParams params) => mtrClusterFormaldehydeConcentrationMeasurement -> params -> IO (Id NSDictionary)
readAttributeClusterRevisionWithParams mtrClusterFormaldehydeConcentrationMeasurement params =
  sendMessage mtrClusterFormaldehydeConcentrationMeasurement readAttributeClusterRevisionWithParamsSelector (toMTRReadParams params)

-- | @- init@
init_ :: IsMTRClusterFormaldehydeConcentrationMeasurement mtrClusterFormaldehydeConcentrationMeasurement => mtrClusterFormaldehydeConcentrationMeasurement -> IO (Id MTRClusterFormaldehydeConcentrationMeasurement)
init_ mtrClusterFormaldehydeConcentrationMeasurement =
  sendOwnedMessage mtrClusterFormaldehydeConcentrationMeasurement initSelector

-- | @+ new@
new :: IO (Id MTRClusterFormaldehydeConcentrationMeasurement)
new  =
  do
    cls' <- getRequiredClass "MTRClusterFormaldehydeConcentrationMeasurement"
    sendOwnedClassMessage cls' newSelector

-- | The queue is currently unused, but may be used in the future for calling completions for command invocations if commands are added to this cluster.
--
-- ObjC selector: @- initWithDevice:endpointID:queue:@
initWithDevice_endpointID_queue :: (IsMTRClusterFormaldehydeConcentrationMeasurement mtrClusterFormaldehydeConcentrationMeasurement, IsMTRDevice device, IsNSNumber endpointID, IsNSObject queue) => mtrClusterFormaldehydeConcentrationMeasurement -> device -> endpointID -> queue -> IO (Id MTRClusterFormaldehydeConcentrationMeasurement)
initWithDevice_endpointID_queue mtrClusterFormaldehydeConcentrationMeasurement device endpointID queue =
  sendOwnedMessage mtrClusterFormaldehydeConcentrationMeasurement initWithDevice_endpointID_queueSelector (toMTRDevice device) (toNSNumber endpointID) (toNSObject queue)

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
initSelector :: Selector '[] (Id MTRClusterFormaldehydeConcentrationMeasurement)
initSelector = mkSelector "init"

-- | @Selector@ for @new@
newSelector :: Selector '[] (Id MTRClusterFormaldehydeConcentrationMeasurement)
newSelector = mkSelector "new"

-- | @Selector@ for @initWithDevice:endpointID:queue:@
initWithDevice_endpointID_queueSelector :: Selector '[Id MTRDevice, Id NSNumber, Id NSObject] (Id MTRClusterFormaldehydeConcentrationMeasurement)
initWithDevice_endpointID_queueSelector = mkSelector "initWithDevice:endpointID:queue:"

