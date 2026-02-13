{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Cluster Total Volatile Organic Compounds Concentration Measurement    Attributes for reporting total volatile organic compounds concentration measurements
--
-- Generated bindings for @MTRClusterTotalVolatileOrganicCompoundsConcentrationMeasurement@.
module ObjC.Matter.MTRClusterTotalVolatileOrganicCompoundsConcentrationMeasurement
  ( MTRClusterTotalVolatileOrganicCompoundsConcentrationMeasurement
  , IsMTRClusterTotalVolatileOrganicCompoundsConcentrationMeasurement(..)
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
readAttributeMeasuredValueWithParams :: (IsMTRClusterTotalVolatileOrganicCompoundsConcentrationMeasurement mtrClusterTotalVolatileOrganicCompoundsConcentrationMeasurement, IsMTRReadParams params) => mtrClusterTotalVolatileOrganicCompoundsConcentrationMeasurement -> params -> IO (Id NSDictionary)
readAttributeMeasuredValueWithParams mtrClusterTotalVolatileOrganicCompoundsConcentrationMeasurement params =
  sendMessage mtrClusterTotalVolatileOrganicCompoundsConcentrationMeasurement readAttributeMeasuredValueWithParamsSelector (toMTRReadParams params)

-- | @- readAttributeMinMeasuredValueWithParams:@
readAttributeMinMeasuredValueWithParams :: (IsMTRClusterTotalVolatileOrganicCompoundsConcentrationMeasurement mtrClusterTotalVolatileOrganicCompoundsConcentrationMeasurement, IsMTRReadParams params) => mtrClusterTotalVolatileOrganicCompoundsConcentrationMeasurement -> params -> IO (Id NSDictionary)
readAttributeMinMeasuredValueWithParams mtrClusterTotalVolatileOrganicCompoundsConcentrationMeasurement params =
  sendMessage mtrClusterTotalVolatileOrganicCompoundsConcentrationMeasurement readAttributeMinMeasuredValueWithParamsSelector (toMTRReadParams params)

-- | @- readAttributeMaxMeasuredValueWithParams:@
readAttributeMaxMeasuredValueWithParams :: (IsMTRClusterTotalVolatileOrganicCompoundsConcentrationMeasurement mtrClusterTotalVolatileOrganicCompoundsConcentrationMeasurement, IsMTRReadParams params) => mtrClusterTotalVolatileOrganicCompoundsConcentrationMeasurement -> params -> IO (Id NSDictionary)
readAttributeMaxMeasuredValueWithParams mtrClusterTotalVolatileOrganicCompoundsConcentrationMeasurement params =
  sendMessage mtrClusterTotalVolatileOrganicCompoundsConcentrationMeasurement readAttributeMaxMeasuredValueWithParamsSelector (toMTRReadParams params)

-- | @- readAttributePeakMeasuredValueWithParams:@
readAttributePeakMeasuredValueWithParams :: (IsMTRClusterTotalVolatileOrganicCompoundsConcentrationMeasurement mtrClusterTotalVolatileOrganicCompoundsConcentrationMeasurement, IsMTRReadParams params) => mtrClusterTotalVolatileOrganicCompoundsConcentrationMeasurement -> params -> IO (Id NSDictionary)
readAttributePeakMeasuredValueWithParams mtrClusterTotalVolatileOrganicCompoundsConcentrationMeasurement params =
  sendMessage mtrClusterTotalVolatileOrganicCompoundsConcentrationMeasurement readAttributePeakMeasuredValueWithParamsSelector (toMTRReadParams params)

-- | @- readAttributePeakMeasuredValueWindowWithParams:@
readAttributePeakMeasuredValueWindowWithParams :: (IsMTRClusterTotalVolatileOrganicCompoundsConcentrationMeasurement mtrClusterTotalVolatileOrganicCompoundsConcentrationMeasurement, IsMTRReadParams params) => mtrClusterTotalVolatileOrganicCompoundsConcentrationMeasurement -> params -> IO (Id NSDictionary)
readAttributePeakMeasuredValueWindowWithParams mtrClusterTotalVolatileOrganicCompoundsConcentrationMeasurement params =
  sendMessage mtrClusterTotalVolatileOrganicCompoundsConcentrationMeasurement readAttributePeakMeasuredValueWindowWithParamsSelector (toMTRReadParams params)

-- | @- readAttributeAverageMeasuredValueWithParams:@
readAttributeAverageMeasuredValueWithParams :: (IsMTRClusterTotalVolatileOrganicCompoundsConcentrationMeasurement mtrClusterTotalVolatileOrganicCompoundsConcentrationMeasurement, IsMTRReadParams params) => mtrClusterTotalVolatileOrganicCompoundsConcentrationMeasurement -> params -> IO (Id NSDictionary)
readAttributeAverageMeasuredValueWithParams mtrClusterTotalVolatileOrganicCompoundsConcentrationMeasurement params =
  sendMessage mtrClusterTotalVolatileOrganicCompoundsConcentrationMeasurement readAttributeAverageMeasuredValueWithParamsSelector (toMTRReadParams params)

-- | @- readAttributeAverageMeasuredValueWindowWithParams:@
readAttributeAverageMeasuredValueWindowWithParams :: (IsMTRClusterTotalVolatileOrganicCompoundsConcentrationMeasurement mtrClusterTotalVolatileOrganicCompoundsConcentrationMeasurement, IsMTRReadParams params) => mtrClusterTotalVolatileOrganicCompoundsConcentrationMeasurement -> params -> IO (Id NSDictionary)
readAttributeAverageMeasuredValueWindowWithParams mtrClusterTotalVolatileOrganicCompoundsConcentrationMeasurement params =
  sendMessage mtrClusterTotalVolatileOrganicCompoundsConcentrationMeasurement readAttributeAverageMeasuredValueWindowWithParamsSelector (toMTRReadParams params)

-- | @- readAttributeUncertaintyWithParams:@
readAttributeUncertaintyWithParams :: (IsMTRClusterTotalVolatileOrganicCompoundsConcentrationMeasurement mtrClusterTotalVolatileOrganicCompoundsConcentrationMeasurement, IsMTRReadParams params) => mtrClusterTotalVolatileOrganicCompoundsConcentrationMeasurement -> params -> IO (Id NSDictionary)
readAttributeUncertaintyWithParams mtrClusterTotalVolatileOrganicCompoundsConcentrationMeasurement params =
  sendMessage mtrClusterTotalVolatileOrganicCompoundsConcentrationMeasurement readAttributeUncertaintyWithParamsSelector (toMTRReadParams params)

-- | @- readAttributeMeasurementUnitWithParams:@
readAttributeMeasurementUnitWithParams :: (IsMTRClusterTotalVolatileOrganicCompoundsConcentrationMeasurement mtrClusterTotalVolatileOrganicCompoundsConcentrationMeasurement, IsMTRReadParams params) => mtrClusterTotalVolatileOrganicCompoundsConcentrationMeasurement -> params -> IO (Id NSDictionary)
readAttributeMeasurementUnitWithParams mtrClusterTotalVolatileOrganicCompoundsConcentrationMeasurement params =
  sendMessage mtrClusterTotalVolatileOrganicCompoundsConcentrationMeasurement readAttributeMeasurementUnitWithParamsSelector (toMTRReadParams params)

-- | @- readAttributeMeasurementMediumWithParams:@
readAttributeMeasurementMediumWithParams :: (IsMTRClusterTotalVolatileOrganicCompoundsConcentrationMeasurement mtrClusterTotalVolatileOrganicCompoundsConcentrationMeasurement, IsMTRReadParams params) => mtrClusterTotalVolatileOrganicCompoundsConcentrationMeasurement -> params -> IO (Id NSDictionary)
readAttributeMeasurementMediumWithParams mtrClusterTotalVolatileOrganicCompoundsConcentrationMeasurement params =
  sendMessage mtrClusterTotalVolatileOrganicCompoundsConcentrationMeasurement readAttributeMeasurementMediumWithParamsSelector (toMTRReadParams params)

-- | @- readAttributeLevelValueWithParams:@
readAttributeLevelValueWithParams :: (IsMTRClusterTotalVolatileOrganicCompoundsConcentrationMeasurement mtrClusterTotalVolatileOrganicCompoundsConcentrationMeasurement, IsMTRReadParams params) => mtrClusterTotalVolatileOrganicCompoundsConcentrationMeasurement -> params -> IO (Id NSDictionary)
readAttributeLevelValueWithParams mtrClusterTotalVolatileOrganicCompoundsConcentrationMeasurement params =
  sendMessage mtrClusterTotalVolatileOrganicCompoundsConcentrationMeasurement readAttributeLevelValueWithParamsSelector (toMTRReadParams params)

-- | @- readAttributeGeneratedCommandListWithParams:@
readAttributeGeneratedCommandListWithParams :: (IsMTRClusterTotalVolatileOrganicCompoundsConcentrationMeasurement mtrClusterTotalVolatileOrganicCompoundsConcentrationMeasurement, IsMTRReadParams params) => mtrClusterTotalVolatileOrganicCompoundsConcentrationMeasurement -> params -> IO (Id NSDictionary)
readAttributeGeneratedCommandListWithParams mtrClusterTotalVolatileOrganicCompoundsConcentrationMeasurement params =
  sendMessage mtrClusterTotalVolatileOrganicCompoundsConcentrationMeasurement readAttributeGeneratedCommandListWithParamsSelector (toMTRReadParams params)

-- | @- readAttributeAcceptedCommandListWithParams:@
readAttributeAcceptedCommandListWithParams :: (IsMTRClusterTotalVolatileOrganicCompoundsConcentrationMeasurement mtrClusterTotalVolatileOrganicCompoundsConcentrationMeasurement, IsMTRReadParams params) => mtrClusterTotalVolatileOrganicCompoundsConcentrationMeasurement -> params -> IO (Id NSDictionary)
readAttributeAcceptedCommandListWithParams mtrClusterTotalVolatileOrganicCompoundsConcentrationMeasurement params =
  sendMessage mtrClusterTotalVolatileOrganicCompoundsConcentrationMeasurement readAttributeAcceptedCommandListWithParamsSelector (toMTRReadParams params)

-- | @- readAttributeAttributeListWithParams:@
readAttributeAttributeListWithParams :: (IsMTRClusterTotalVolatileOrganicCompoundsConcentrationMeasurement mtrClusterTotalVolatileOrganicCompoundsConcentrationMeasurement, IsMTRReadParams params) => mtrClusterTotalVolatileOrganicCompoundsConcentrationMeasurement -> params -> IO (Id NSDictionary)
readAttributeAttributeListWithParams mtrClusterTotalVolatileOrganicCompoundsConcentrationMeasurement params =
  sendMessage mtrClusterTotalVolatileOrganicCompoundsConcentrationMeasurement readAttributeAttributeListWithParamsSelector (toMTRReadParams params)

-- | @- readAttributeFeatureMapWithParams:@
readAttributeFeatureMapWithParams :: (IsMTRClusterTotalVolatileOrganicCompoundsConcentrationMeasurement mtrClusterTotalVolatileOrganicCompoundsConcentrationMeasurement, IsMTRReadParams params) => mtrClusterTotalVolatileOrganicCompoundsConcentrationMeasurement -> params -> IO (Id NSDictionary)
readAttributeFeatureMapWithParams mtrClusterTotalVolatileOrganicCompoundsConcentrationMeasurement params =
  sendMessage mtrClusterTotalVolatileOrganicCompoundsConcentrationMeasurement readAttributeFeatureMapWithParamsSelector (toMTRReadParams params)

-- | @- readAttributeClusterRevisionWithParams:@
readAttributeClusterRevisionWithParams :: (IsMTRClusterTotalVolatileOrganicCompoundsConcentrationMeasurement mtrClusterTotalVolatileOrganicCompoundsConcentrationMeasurement, IsMTRReadParams params) => mtrClusterTotalVolatileOrganicCompoundsConcentrationMeasurement -> params -> IO (Id NSDictionary)
readAttributeClusterRevisionWithParams mtrClusterTotalVolatileOrganicCompoundsConcentrationMeasurement params =
  sendMessage mtrClusterTotalVolatileOrganicCompoundsConcentrationMeasurement readAttributeClusterRevisionWithParamsSelector (toMTRReadParams params)

-- | @- init@
init_ :: IsMTRClusterTotalVolatileOrganicCompoundsConcentrationMeasurement mtrClusterTotalVolatileOrganicCompoundsConcentrationMeasurement => mtrClusterTotalVolatileOrganicCompoundsConcentrationMeasurement -> IO (Id MTRClusterTotalVolatileOrganicCompoundsConcentrationMeasurement)
init_ mtrClusterTotalVolatileOrganicCompoundsConcentrationMeasurement =
  sendOwnedMessage mtrClusterTotalVolatileOrganicCompoundsConcentrationMeasurement initSelector

-- | @+ new@
new :: IO (Id MTRClusterTotalVolatileOrganicCompoundsConcentrationMeasurement)
new  =
  do
    cls' <- getRequiredClass "MTRClusterTotalVolatileOrganicCompoundsConcentrationMeasurement"
    sendOwnedClassMessage cls' newSelector

-- | The queue is currently unused, but may be used in the future for calling completions for command invocations if commands are added to this cluster.
--
-- ObjC selector: @- initWithDevice:endpointID:queue:@
initWithDevice_endpointID_queue :: (IsMTRClusterTotalVolatileOrganicCompoundsConcentrationMeasurement mtrClusterTotalVolatileOrganicCompoundsConcentrationMeasurement, IsMTRDevice device, IsNSNumber endpointID, IsNSObject queue) => mtrClusterTotalVolatileOrganicCompoundsConcentrationMeasurement -> device -> endpointID -> queue -> IO (Id MTRClusterTotalVolatileOrganicCompoundsConcentrationMeasurement)
initWithDevice_endpointID_queue mtrClusterTotalVolatileOrganicCompoundsConcentrationMeasurement device endpointID queue =
  sendOwnedMessage mtrClusterTotalVolatileOrganicCompoundsConcentrationMeasurement initWithDevice_endpointID_queueSelector (toMTRDevice device) (toNSNumber endpointID) (toNSObject queue)

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
initSelector :: Selector '[] (Id MTRClusterTotalVolatileOrganicCompoundsConcentrationMeasurement)
initSelector = mkSelector "init"

-- | @Selector@ for @new@
newSelector :: Selector '[] (Id MTRClusterTotalVolatileOrganicCompoundsConcentrationMeasurement)
newSelector = mkSelector "new"

-- | @Selector@ for @initWithDevice:endpointID:queue:@
initWithDevice_endpointID_queueSelector :: Selector '[Id MTRDevice, Id NSNumber, Id NSObject] (Id MTRClusterTotalVolatileOrganicCompoundsConcentrationMeasurement)
initWithDevice_endpointID_queueSelector = mkSelector "initWithDevice:endpointID:queue:"

