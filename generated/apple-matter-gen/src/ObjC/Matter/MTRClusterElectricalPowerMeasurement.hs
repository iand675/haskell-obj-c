{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Cluster Electrical Power Measurement    This cluster provides a mechanism for querying data about electrical power as measured by the server.
--
-- Generated bindings for @MTRClusterElectricalPowerMeasurement@.
module ObjC.Matter.MTRClusterElectricalPowerMeasurement
  ( MTRClusterElectricalPowerMeasurement
  , IsMTRClusterElectricalPowerMeasurement(..)
  , readAttributePowerModeWithParams
  , readAttributeNumberOfMeasurementTypesWithParams
  , readAttributeAccuracyWithParams
  , readAttributeRangesWithParams
  , readAttributeVoltageWithParams
  , readAttributeActiveCurrentWithParams
  , readAttributeReactiveCurrentWithParams
  , readAttributeApparentCurrentWithParams
  , readAttributeActivePowerWithParams
  , readAttributeReactivePowerWithParams
  , readAttributeApparentPowerWithParams
  , readAttributeRMSVoltageWithParams
  , readAttributeRMSCurrentWithParams
  , readAttributeRMSPowerWithParams
  , readAttributeFrequencyWithParams
  , readAttributeHarmonicCurrentsWithParams
  , readAttributeHarmonicPhasesWithParams
  , readAttributePowerFactorWithParams
  , readAttributeNeutralCurrentWithParams
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
  , readAttributeAccuracyWithParamsSelector
  , readAttributeActiveCurrentWithParamsSelector
  , readAttributeActivePowerWithParamsSelector
  , readAttributeApparentCurrentWithParamsSelector
  , readAttributeApparentPowerWithParamsSelector
  , readAttributeAttributeListWithParamsSelector
  , readAttributeClusterRevisionWithParamsSelector
  , readAttributeFeatureMapWithParamsSelector
  , readAttributeFrequencyWithParamsSelector
  , readAttributeGeneratedCommandListWithParamsSelector
  , readAttributeHarmonicCurrentsWithParamsSelector
  , readAttributeHarmonicPhasesWithParamsSelector
  , readAttributeNeutralCurrentWithParamsSelector
  , readAttributeNumberOfMeasurementTypesWithParamsSelector
  , readAttributePowerFactorWithParamsSelector
  , readAttributePowerModeWithParamsSelector
  , readAttributeRMSCurrentWithParamsSelector
  , readAttributeRMSPowerWithParamsSelector
  , readAttributeRMSVoltageWithParamsSelector
  , readAttributeRangesWithParamsSelector
  , readAttributeReactiveCurrentWithParamsSelector
  , readAttributeReactivePowerWithParamsSelector
  , readAttributeVoltageWithParamsSelector


  ) where

import Foreign.Ptr (Ptr, FunPtr)
import Foreign.C.Types

import ObjC.Runtime.Types
import ObjC.Runtime.Message (sendMessage, sendOwnedMessage, sendClassMessage, sendOwnedClassMessage)
import ObjC.Runtime.Selector (mkSelector)
import ObjC.Runtime.Class (getRequiredClass)

import ObjC.Matter.Internal.Classes
import ObjC.Foundation.Internal.Classes

-- | @- readAttributePowerModeWithParams:@
readAttributePowerModeWithParams :: (IsMTRClusterElectricalPowerMeasurement mtrClusterElectricalPowerMeasurement, IsMTRReadParams params) => mtrClusterElectricalPowerMeasurement -> params -> IO (Id NSDictionary)
readAttributePowerModeWithParams mtrClusterElectricalPowerMeasurement params =
  sendMessage mtrClusterElectricalPowerMeasurement readAttributePowerModeWithParamsSelector (toMTRReadParams params)

-- | @- readAttributeNumberOfMeasurementTypesWithParams:@
readAttributeNumberOfMeasurementTypesWithParams :: (IsMTRClusterElectricalPowerMeasurement mtrClusterElectricalPowerMeasurement, IsMTRReadParams params) => mtrClusterElectricalPowerMeasurement -> params -> IO (Id NSDictionary)
readAttributeNumberOfMeasurementTypesWithParams mtrClusterElectricalPowerMeasurement params =
  sendMessage mtrClusterElectricalPowerMeasurement readAttributeNumberOfMeasurementTypesWithParamsSelector (toMTRReadParams params)

-- | @- readAttributeAccuracyWithParams:@
readAttributeAccuracyWithParams :: (IsMTRClusterElectricalPowerMeasurement mtrClusterElectricalPowerMeasurement, IsMTRReadParams params) => mtrClusterElectricalPowerMeasurement -> params -> IO (Id NSDictionary)
readAttributeAccuracyWithParams mtrClusterElectricalPowerMeasurement params =
  sendMessage mtrClusterElectricalPowerMeasurement readAttributeAccuracyWithParamsSelector (toMTRReadParams params)

-- | @- readAttributeRangesWithParams:@
readAttributeRangesWithParams :: (IsMTRClusterElectricalPowerMeasurement mtrClusterElectricalPowerMeasurement, IsMTRReadParams params) => mtrClusterElectricalPowerMeasurement -> params -> IO (Id NSDictionary)
readAttributeRangesWithParams mtrClusterElectricalPowerMeasurement params =
  sendMessage mtrClusterElectricalPowerMeasurement readAttributeRangesWithParamsSelector (toMTRReadParams params)

-- | @- readAttributeVoltageWithParams:@
readAttributeVoltageWithParams :: (IsMTRClusterElectricalPowerMeasurement mtrClusterElectricalPowerMeasurement, IsMTRReadParams params) => mtrClusterElectricalPowerMeasurement -> params -> IO (Id NSDictionary)
readAttributeVoltageWithParams mtrClusterElectricalPowerMeasurement params =
  sendMessage mtrClusterElectricalPowerMeasurement readAttributeVoltageWithParamsSelector (toMTRReadParams params)

-- | @- readAttributeActiveCurrentWithParams:@
readAttributeActiveCurrentWithParams :: (IsMTRClusterElectricalPowerMeasurement mtrClusterElectricalPowerMeasurement, IsMTRReadParams params) => mtrClusterElectricalPowerMeasurement -> params -> IO (Id NSDictionary)
readAttributeActiveCurrentWithParams mtrClusterElectricalPowerMeasurement params =
  sendMessage mtrClusterElectricalPowerMeasurement readAttributeActiveCurrentWithParamsSelector (toMTRReadParams params)

-- | @- readAttributeReactiveCurrentWithParams:@
readAttributeReactiveCurrentWithParams :: (IsMTRClusterElectricalPowerMeasurement mtrClusterElectricalPowerMeasurement, IsMTRReadParams params) => mtrClusterElectricalPowerMeasurement -> params -> IO (Id NSDictionary)
readAttributeReactiveCurrentWithParams mtrClusterElectricalPowerMeasurement params =
  sendMessage mtrClusterElectricalPowerMeasurement readAttributeReactiveCurrentWithParamsSelector (toMTRReadParams params)

-- | @- readAttributeApparentCurrentWithParams:@
readAttributeApparentCurrentWithParams :: (IsMTRClusterElectricalPowerMeasurement mtrClusterElectricalPowerMeasurement, IsMTRReadParams params) => mtrClusterElectricalPowerMeasurement -> params -> IO (Id NSDictionary)
readAttributeApparentCurrentWithParams mtrClusterElectricalPowerMeasurement params =
  sendMessage mtrClusterElectricalPowerMeasurement readAttributeApparentCurrentWithParamsSelector (toMTRReadParams params)

-- | @- readAttributeActivePowerWithParams:@
readAttributeActivePowerWithParams :: (IsMTRClusterElectricalPowerMeasurement mtrClusterElectricalPowerMeasurement, IsMTRReadParams params) => mtrClusterElectricalPowerMeasurement -> params -> IO (Id NSDictionary)
readAttributeActivePowerWithParams mtrClusterElectricalPowerMeasurement params =
  sendMessage mtrClusterElectricalPowerMeasurement readAttributeActivePowerWithParamsSelector (toMTRReadParams params)

-- | @- readAttributeReactivePowerWithParams:@
readAttributeReactivePowerWithParams :: (IsMTRClusterElectricalPowerMeasurement mtrClusterElectricalPowerMeasurement, IsMTRReadParams params) => mtrClusterElectricalPowerMeasurement -> params -> IO (Id NSDictionary)
readAttributeReactivePowerWithParams mtrClusterElectricalPowerMeasurement params =
  sendMessage mtrClusterElectricalPowerMeasurement readAttributeReactivePowerWithParamsSelector (toMTRReadParams params)

-- | @- readAttributeApparentPowerWithParams:@
readAttributeApparentPowerWithParams :: (IsMTRClusterElectricalPowerMeasurement mtrClusterElectricalPowerMeasurement, IsMTRReadParams params) => mtrClusterElectricalPowerMeasurement -> params -> IO (Id NSDictionary)
readAttributeApparentPowerWithParams mtrClusterElectricalPowerMeasurement params =
  sendMessage mtrClusterElectricalPowerMeasurement readAttributeApparentPowerWithParamsSelector (toMTRReadParams params)

-- | @- readAttributeRMSVoltageWithParams:@
readAttributeRMSVoltageWithParams :: (IsMTRClusterElectricalPowerMeasurement mtrClusterElectricalPowerMeasurement, IsMTRReadParams params) => mtrClusterElectricalPowerMeasurement -> params -> IO (Id NSDictionary)
readAttributeRMSVoltageWithParams mtrClusterElectricalPowerMeasurement params =
  sendMessage mtrClusterElectricalPowerMeasurement readAttributeRMSVoltageWithParamsSelector (toMTRReadParams params)

-- | @- readAttributeRMSCurrentWithParams:@
readAttributeRMSCurrentWithParams :: (IsMTRClusterElectricalPowerMeasurement mtrClusterElectricalPowerMeasurement, IsMTRReadParams params) => mtrClusterElectricalPowerMeasurement -> params -> IO (Id NSDictionary)
readAttributeRMSCurrentWithParams mtrClusterElectricalPowerMeasurement params =
  sendMessage mtrClusterElectricalPowerMeasurement readAttributeRMSCurrentWithParamsSelector (toMTRReadParams params)

-- | @- readAttributeRMSPowerWithParams:@
readAttributeRMSPowerWithParams :: (IsMTRClusterElectricalPowerMeasurement mtrClusterElectricalPowerMeasurement, IsMTRReadParams params) => mtrClusterElectricalPowerMeasurement -> params -> IO (Id NSDictionary)
readAttributeRMSPowerWithParams mtrClusterElectricalPowerMeasurement params =
  sendMessage mtrClusterElectricalPowerMeasurement readAttributeRMSPowerWithParamsSelector (toMTRReadParams params)

-- | @- readAttributeFrequencyWithParams:@
readAttributeFrequencyWithParams :: (IsMTRClusterElectricalPowerMeasurement mtrClusterElectricalPowerMeasurement, IsMTRReadParams params) => mtrClusterElectricalPowerMeasurement -> params -> IO (Id NSDictionary)
readAttributeFrequencyWithParams mtrClusterElectricalPowerMeasurement params =
  sendMessage mtrClusterElectricalPowerMeasurement readAttributeFrequencyWithParamsSelector (toMTRReadParams params)

-- | @- readAttributeHarmonicCurrentsWithParams:@
readAttributeHarmonicCurrentsWithParams :: (IsMTRClusterElectricalPowerMeasurement mtrClusterElectricalPowerMeasurement, IsMTRReadParams params) => mtrClusterElectricalPowerMeasurement -> params -> IO (Id NSDictionary)
readAttributeHarmonicCurrentsWithParams mtrClusterElectricalPowerMeasurement params =
  sendMessage mtrClusterElectricalPowerMeasurement readAttributeHarmonicCurrentsWithParamsSelector (toMTRReadParams params)

-- | @- readAttributeHarmonicPhasesWithParams:@
readAttributeHarmonicPhasesWithParams :: (IsMTRClusterElectricalPowerMeasurement mtrClusterElectricalPowerMeasurement, IsMTRReadParams params) => mtrClusterElectricalPowerMeasurement -> params -> IO (Id NSDictionary)
readAttributeHarmonicPhasesWithParams mtrClusterElectricalPowerMeasurement params =
  sendMessage mtrClusterElectricalPowerMeasurement readAttributeHarmonicPhasesWithParamsSelector (toMTRReadParams params)

-- | @- readAttributePowerFactorWithParams:@
readAttributePowerFactorWithParams :: (IsMTRClusterElectricalPowerMeasurement mtrClusterElectricalPowerMeasurement, IsMTRReadParams params) => mtrClusterElectricalPowerMeasurement -> params -> IO (Id NSDictionary)
readAttributePowerFactorWithParams mtrClusterElectricalPowerMeasurement params =
  sendMessage mtrClusterElectricalPowerMeasurement readAttributePowerFactorWithParamsSelector (toMTRReadParams params)

-- | @- readAttributeNeutralCurrentWithParams:@
readAttributeNeutralCurrentWithParams :: (IsMTRClusterElectricalPowerMeasurement mtrClusterElectricalPowerMeasurement, IsMTRReadParams params) => mtrClusterElectricalPowerMeasurement -> params -> IO (Id NSDictionary)
readAttributeNeutralCurrentWithParams mtrClusterElectricalPowerMeasurement params =
  sendMessage mtrClusterElectricalPowerMeasurement readAttributeNeutralCurrentWithParamsSelector (toMTRReadParams params)

-- | @- readAttributeGeneratedCommandListWithParams:@
readAttributeGeneratedCommandListWithParams :: (IsMTRClusterElectricalPowerMeasurement mtrClusterElectricalPowerMeasurement, IsMTRReadParams params) => mtrClusterElectricalPowerMeasurement -> params -> IO (Id NSDictionary)
readAttributeGeneratedCommandListWithParams mtrClusterElectricalPowerMeasurement params =
  sendMessage mtrClusterElectricalPowerMeasurement readAttributeGeneratedCommandListWithParamsSelector (toMTRReadParams params)

-- | @- readAttributeAcceptedCommandListWithParams:@
readAttributeAcceptedCommandListWithParams :: (IsMTRClusterElectricalPowerMeasurement mtrClusterElectricalPowerMeasurement, IsMTRReadParams params) => mtrClusterElectricalPowerMeasurement -> params -> IO (Id NSDictionary)
readAttributeAcceptedCommandListWithParams mtrClusterElectricalPowerMeasurement params =
  sendMessage mtrClusterElectricalPowerMeasurement readAttributeAcceptedCommandListWithParamsSelector (toMTRReadParams params)

-- | @- readAttributeAttributeListWithParams:@
readAttributeAttributeListWithParams :: (IsMTRClusterElectricalPowerMeasurement mtrClusterElectricalPowerMeasurement, IsMTRReadParams params) => mtrClusterElectricalPowerMeasurement -> params -> IO (Id NSDictionary)
readAttributeAttributeListWithParams mtrClusterElectricalPowerMeasurement params =
  sendMessage mtrClusterElectricalPowerMeasurement readAttributeAttributeListWithParamsSelector (toMTRReadParams params)

-- | @- readAttributeFeatureMapWithParams:@
readAttributeFeatureMapWithParams :: (IsMTRClusterElectricalPowerMeasurement mtrClusterElectricalPowerMeasurement, IsMTRReadParams params) => mtrClusterElectricalPowerMeasurement -> params -> IO (Id NSDictionary)
readAttributeFeatureMapWithParams mtrClusterElectricalPowerMeasurement params =
  sendMessage mtrClusterElectricalPowerMeasurement readAttributeFeatureMapWithParamsSelector (toMTRReadParams params)

-- | @- readAttributeClusterRevisionWithParams:@
readAttributeClusterRevisionWithParams :: (IsMTRClusterElectricalPowerMeasurement mtrClusterElectricalPowerMeasurement, IsMTRReadParams params) => mtrClusterElectricalPowerMeasurement -> params -> IO (Id NSDictionary)
readAttributeClusterRevisionWithParams mtrClusterElectricalPowerMeasurement params =
  sendMessage mtrClusterElectricalPowerMeasurement readAttributeClusterRevisionWithParamsSelector (toMTRReadParams params)

-- | @- init@
init_ :: IsMTRClusterElectricalPowerMeasurement mtrClusterElectricalPowerMeasurement => mtrClusterElectricalPowerMeasurement -> IO (Id MTRClusterElectricalPowerMeasurement)
init_ mtrClusterElectricalPowerMeasurement =
  sendOwnedMessage mtrClusterElectricalPowerMeasurement initSelector

-- | @+ new@
new :: IO (Id MTRClusterElectricalPowerMeasurement)
new  =
  do
    cls' <- getRequiredClass "MTRClusterElectricalPowerMeasurement"
    sendOwnedClassMessage cls' newSelector

-- | The queue is currently unused, but may be used in the future for calling completions for command invocations if commands are added to this cluster.
--
-- ObjC selector: @- initWithDevice:endpointID:queue:@
initWithDevice_endpointID_queue :: (IsMTRClusterElectricalPowerMeasurement mtrClusterElectricalPowerMeasurement, IsMTRDevice device, IsNSNumber endpointID, IsNSObject queue) => mtrClusterElectricalPowerMeasurement -> device -> endpointID -> queue -> IO (Id MTRClusterElectricalPowerMeasurement)
initWithDevice_endpointID_queue mtrClusterElectricalPowerMeasurement device endpointID queue =
  sendOwnedMessage mtrClusterElectricalPowerMeasurement initWithDevice_endpointID_queueSelector (toMTRDevice device) (toNSNumber endpointID) (toNSObject queue)

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @readAttributePowerModeWithParams:@
readAttributePowerModeWithParamsSelector :: Selector '[Id MTRReadParams] (Id NSDictionary)
readAttributePowerModeWithParamsSelector = mkSelector "readAttributePowerModeWithParams:"

-- | @Selector@ for @readAttributeNumberOfMeasurementTypesWithParams:@
readAttributeNumberOfMeasurementTypesWithParamsSelector :: Selector '[Id MTRReadParams] (Id NSDictionary)
readAttributeNumberOfMeasurementTypesWithParamsSelector = mkSelector "readAttributeNumberOfMeasurementTypesWithParams:"

-- | @Selector@ for @readAttributeAccuracyWithParams:@
readAttributeAccuracyWithParamsSelector :: Selector '[Id MTRReadParams] (Id NSDictionary)
readAttributeAccuracyWithParamsSelector = mkSelector "readAttributeAccuracyWithParams:"

-- | @Selector@ for @readAttributeRangesWithParams:@
readAttributeRangesWithParamsSelector :: Selector '[Id MTRReadParams] (Id NSDictionary)
readAttributeRangesWithParamsSelector = mkSelector "readAttributeRangesWithParams:"

-- | @Selector@ for @readAttributeVoltageWithParams:@
readAttributeVoltageWithParamsSelector :: Selector '[Id MTRReadParams] (Id NSDictionary)
readAttributeVoltageWithParamsSelector = mkSelector "readAttributeVoltageWithParams:"

-- | @Selector@ for @readAttributeActiveCurrentWithParams:@
readAttributeActiveCurrentWithParamsSelector :: Selector '[Id MTRReadParams] (Id NSDictionary)
readAttributeActiveCurrentWithParamsSelector = mkSelector "readAttributeActiveCurrentWithParams:"

-- | @Selector@ for @readAttributeReactiveCurrentWithParams:@
readAttributeReactiveCurrentWithParamsSelector :: Selector '[Id MTRReadParams] (Id NSDictionary)
readAttributeReactiveCurrentWithParamsSelector = mkSelector "readAttributeReactiveCurrentWithParams:"

-- | @Selector@ for @readAttributeApparentCurrentWithParams:@
readAttributeApparentCurrentWithParamsSelector :: Selector '[Id MTRReadParams] (Id NSDictionary)
readAttributeApparentCurrentWithParamsSelector = mkSelector "readAttributeApparentCurrentWithParams:"

-- | @Selector@ for @readAttributeActivePowerWithParams:@
readAttributeActivePowerWithParamsSelector :: Selector '[Id MTRReadParams] (Id NSDictionary)
readAttributeActivePowerWithParamsSelector = mkSelector "readAttributeActivePowerWithParams:"

-- | @Selector@ for @readAttributeReactivePowerWithParams:@
readAttributeReactivePowerWithParamsSelector :: Selector '[Id MTRReadParams] (Id NSDictionary)
readAttributeReactivePowerWithParamsSelector = mkSelector "readAttributeReactivePowerWithParams:"

-- | @Selector@ for @readAttributeApparentPowerWithParams:@
readAttributeApparentPowerWithParamsSelector :: Selector '[Id MTRReadParams] (Id NSDictionary)
readAttributeApparentPowerWithParamsSelector = mkSelector "readAttributeApparentPowerWithParams:"

-- | @Selector@ for @readAttributeRMSVoltageWithParams:@
readAttributeRMSVoltageWithParamsSelector :: Selector '[Id MTRReadParams] (Id NSDictionary)
readAttributeRMSVoltageWithParamsSelector = mkSelector "readAttributeRMSVoltageWithParams:"

-- | @Selector@ for @readAttributeRMSCurrentWithParams:@
readAttributeRMSCurrentWithParamsSelector :: Selector '[Id MTRReadParams] (Id NSDictionary)
readAttributeRMSCurrentWithParamsSelector = mkSelector "readAttributeRMSCurrentWithParams:"

-- | @Selector@ for @readAttributeRMSPowerWithParams:@
readAttributeRMSPowerWithParamsSelector :: Selector '[Id MTRReadParams] (Id NSDictionary)
readAttributeRMSPowerWithParamsSelector = mkSelector "readAttributeRMSPowerWithParams:"

-- | @Selector@ for @readAttributeFrequencyWithParams:@
readAttributeFrequencyWithParamsSelector :: Selector '[Id MTRReadParams] (Id NSDictionary)
readAttributeFrequencyWithParamsSelector = mkSelector "readAttributeFrequencyWithParams:"

-- | @Selector@ for @readAttributeHarmonicCurrentsWithParams:@
readAttributeHarmonicCurrentsWithParamsSelector :: Selector '[Id MTRReadParams] (Id NSDictionary)
readAttributeHarmonicCurrentsWithParamsSelector = mkSelector "readAttributeHarmonicCurrentsWithParams:"

-- | @Selector@ for @readAttributeHarmonicPhasesWithParams:@
readAttributeHarmonicPhasesWithParamsSelector :: Selector '[Id MTRReadParams] (Id NSDictionary)
readAttributeHarmonicPhasesWithParamsSelector = mkSelector "readAttributeHarmonicPhasesWithParams:"

-- | @Selector@ for @readAttributePowerFactorWithParams:@
readAttributePowerFactorWithParamsSelector :: Selector '[Id MTRReadParams] (Id NSDictionary)
readAttributePowerFactorWithParamsSelector = mkSelector "readAttributePowerFactorWithParams:"

-- | @Selector@ for @readAttributeNeutralCurrentWithParams:@
readAttributeNeutralCurrentWithParamsSelector :: Selector '[Id MTRReadParams] (Id NSDictionary)
readAttributeNeutralCurrentWithParamsSelector = mkSelector "readAttributeNeutralCurrentWithParams:"

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
initSelector :: Selector '[] (Id MTRClusterElectricalPowerMeasurement)
initSelector = mkSelector "init"

-- | @Selector@ for @new@
newSelector :: Selector '[] (Id MTRClusterElectricalPowerMeasurement)
newSelector = mkSelector "new"

-- | @Selector@ for @initWithDevice:endpointID:queue:@
initWithDevice_endpointID_queueSelector :: Selector '[Id MTRDevice, Id NSNumber, Id NSObject] (Id MTRClusterElectricalPowerMeasurement)
initWithDevice_endpointID_queueSelector = mkSelector "initWithDevice:endpointID:queue:"

