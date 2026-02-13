{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Cluster Pump Configuration and Control    An interface for configuring and controlling pumps.
--
-- Generated bindings for @MTRClusterPumpConfigurationAndControl@.
module ObjC.Matter.MTRClusterPumpConfigurationAndControl
  ( MTRClusterPumpConfigurationAndControl
  , IsMTRClusterPumpConfigurationAndControl(..)
  , readAttributeMaxPressureWithParams
  , readAttributeMaxSpeedWithParams
  , readAttributeMaxFlowWithParams
  , readAttributeMinConstPressureWithParams
  , readAttributeMaxConstPressureWithParams
  , readAttributeMinCompPressureWithParams
  , readAttributeMaxCompPressureWithParams
  , readAttributeMinConstSpeedWithParams
  , readAttributeMaxConstSpeedWithParams
  , readAttributeMinConstFlowWithParams
  , readAttributeMaxConstFlowWithParams
  , readAttributeMinConstTempWithParams
  , readAttributeMaxConstTempWithParams
  , readAttributePumpStatusWithParams
  , readAttributeEffectiveOperationModeWithParams
  , readAttributeEffectiveControlModeWithParams
  , readAttributeCapacityWithParams
  , readAttributeSpeedWithParams
  , readAttributeLifetimeRunningHoursWithParams
  , writeAttributeLifetimeRunningHoursWithValue_expectedValueInterval
  , writeAttributeLifetimeRunningHoursWithValue_expectedValueInterval_params
  , readAttributePowerWithParams
  , readAttributeLifetimeEnergyConsumedWithParams
  , writeAttributeLifetimeEnergyConsumedWithValue_expectedValueInterval
  , writeAttributeLifetimeEnergyConsumedWithValue_expectedValueInterval_params
  , readAttributeOperationModeWithParams
  , writeAttributeOperationModeWithValue_expectedValueInterval
  , writeAttributeOperationModeWithValue_expectedValueInterval_params
  , readAttributeControlModeWithParams
  , writeAttributeControlModeWithValue_expectedValueInterval
  , writeAttributeControlModeWithValue_expectedValueInterval_params
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
  , readAttributeCapacityWithParamsSelector
  , readAttributeClusterRevisionWithParamsSelector
  , readAttributeControlModeWithParamsSelector
  , readAttributeEffectiveControlModeWithParamsSelector
  , readAttributeEffectiveOperationModeWithParamsSelector
  , readAttributeFeatureMapWithParamsSelector
  , readAttributeGeneratedCommandListWithParamsSelector
  , readAttributeLifetimeEnergyConsumedWithParamsSelector
  , readAttributeLifetimeRunningHoursWithParamsSelector
  , readAttributeMaxCompPressureWithParamsSelector
  , readAttributeMaxConstFlowWithParamsSelector
  , readAttributeMaxConstPressureWithParamsSelector
  , readAttributeMaxConstSpeedWithParamsSelector
  , readAttributeMaxConstTempWithParamsSelector
  , readAttributeMaxFlowWithParamsSelector
  , readAttributeMaxPressureWithParamsSelector
  , readAttributeMaxSpeedWithParamsSelector
  , readAttributeMinCompPressureWithParamsSelector
  , readAttributeMinConstFlowWithParamsSelector
  , readAttributeMinConstPressureWithParamsSelector
  , readAttributeMinConstSpeedWithParamsSelector
  , readAttributeMinConstTempWithParamsSelector
  , readAttributeOperationModeWithParamsSelector
  , readAttributePowerWithParamsSelector
  , readAttributePumpStatusWithParamsSelector
  , readAttributeSpeedWithParamsSelector
  , writeAttributeControlModeWithValue_expectedValueIntervalSelector
  , writeAttributeControlModeWithValue_expectedValueInterval_paramsSelector
  , writeAttributeLifetimeEnergyConsumedWithValue_expectedValueIntervalSelector
  , writeAttributeLifetimeEnergyConsumedWithValue_expectedValueInterval_paramsSelector
  , writeAttributeLifetimeRunningHoursWithValue_expectedValueIntervalSelector
  , writeAttributeLifetimeRunningHoursWithValue_expectedValueInterval_paramsSelector
  , writeAttributeOperationModeWithValue_expectedValueIntervalSelector
  , writeAttributeOperationModeWithValue_expectedValueInterval_paramsSelector


  ) where

import Foreign.Ptr (Ptr, FunPtr)
import Foreign.C.Types

import ObjC.Runtime.Types
import ObjC.Runtime.Message (sendMessage, sendOwnedMessage, sendClassMessage, sendOwnedClassMessage)
import ObjC.Runtime.Selector (mkSelector)
import ObjC.Runtime.Class (getRequiredClass)

import ObjC.Matter.Internal.Classes
import ObjC.Foundation.Internal.Classes

-- | @- readAttributeMaxPressureWithParams:@
readAttributeMaxPressureWithParams :: (IsMTRClusterPumpConfigurationAndControl mtrClusterPumpConfigurationAndControl, IsMTRReadParams params) => mtrClusterPumpConfigurationAndControl -> params -> IO (Id NSDictionary)
readAttributeMaxPressureWithParams mtrClusterPumpConfigurationAndControl params =
  sendMessage mtrClusterPumpConfigurationAndControl readAttributeMaxPressureWithParamsSelector (toMTRReadParams params)

-- | @- readAttributeMaxSpeedWithParams:@
readAttributeMaxSpeedWithParams :: (IsMTRClusterPumpConfigurationAndControl mtrClusterPumpConfigurationAndControl, IsMTRReadParams params) => mtrClusterPumpConfigurationAndControl -> params -> IO (Id NSDictionary)
readAttributeMaxSpeedWithParams mtrClusterPumpConfigurationAndControl params =
  sendMessage mtrClusterPumpConfigurationAndControl readAttributeMaxSpeedWithParamsSelector (toMTRReadParams params)

-- | @- readAttributeMaxFlowWithParams:@
readAttributeMaxFlowWithParams :: (IsMTRClusterPumpConfigurationAndControl mtrClusterPumpConfigurationAndControl, IsMTRReadParams params) => mtrClusterPumpConfigurationAndControl -> params -> IO (Id NSDictionary)
readAttributeMaxFlowWithParams mtrClusterPumpConfigurationAndControl params =
  sendMessage mtrClusterPumpConfigurationAndControl readAttributeMaxFlowWithParamsSelector (toMTRReadParams params)

-- | @- readAttributeMinConstPressureWithParams:@
readAttributeMinConstPressureWithParams :: (IsMTRClusterPumpConfigurationAndControl mtrClusterPumpConfigurationAndControl, IsMTRReadParams params) => mtrClusterPumpConfigurationAndControl -> params -> IO (Id NSDictionary)
readAttributeMinConstPressureWithParams mtrClusterPumpConfigurationAndControl params =
  sendMessage mtrClusterPumpConfigurationAndControl readAttributeMinConstPressureWithParamsSelector (toMTRReadParams params)

-- | @- readAttributeMaxConstPressureWithParams:@
readAttributeMaxConstPressureWithParams :: (IsMTRClusterPumpConfigurationAndControl mtrClusterPumpConfigurationAndControl, IsMTRReadParams params) => mtrClusterPumpConfigurationAndControl -> params -> IO (Id NSDictionary)
readAttributeMaxConstPressureWithParams mtrClusterPumpConfigurationAndControl params =
  sendMessage mtrClusterPumpConfigurationAndControl readAttributeMaxConstPressureWithParamsSelector (toMTRReadParams params)

-- | @- readAttributeMinCompPressureWithParams:@
readAttributeMinCompPressureWithParams :: (IsMTRClusterPumpConfigurationAndControl mtrClusterPumpConfigurationAndControl, IsMTRReadParams params) => mtrClusterPumpConfigurationAndControl -> params -> IO (Id NSDictionary)
readAttributeMinCompPressureWithParams mtrClusterPumpConfigurationAndControl params =
  sendMessage mtrClusterPumpConfigurationAndControl readAttributeMinCompPressureWithParamsSelector (toMTRReadParams params)

-- | @- readAttributeMaxCompPressureWithParams:@
readAttributeMaxCompPressureWithParams :: (IsMTRClusterPumpConfigurationAndControl mtrClusterPumpConfigurationAndControl, IsMTRReadParams params) => mtrClusterPumpConfigurationAndControl -> params -> IO (Id NSDictionary)
readAttributeMaxCompPressureWithParams mtrClusterPumpConfigurationAndControl params =
  sendMessage mtrClusterPumpConfigurationAndControl readAttributeMaxCompPressureWithParamsSelector (toMTRReadParams params)

-- | @- readAttributeMinConstSpeedWithParams:@
readAttributeMinConstSpeedWithParams :: (IsMTRClusterPumpConfigurationAndControl mtrClusterPumpConfigurationAndControl, IsMTRReadParams params) => mtrClusterPumpConfigurationAndControl -> params -> IO (Id NSDictionary)
readAttributeMinConstSpeedWithParams mtrClusterPumpConfigurationAndControl params =
  sendMessage mtrClusterPumpConfigurationAndControl readAttributeMinConstSpeedWithParamsSelector (toMTRReadParams params)

-- | @- readAttributeMaxConstSpeedWithParams:@
readAttributeMaxConstSpeedWithParams :: (IsMTRClusterPumpConfigurationAndControl mtrClusterPumpConfigurationAndControl, IsMTRReadParams params) => mtrClusterPumpConfigurationAndControl -> params -> IO (Id NSDictionary)
readAttributeMaxConstSpeedWithParams mtrClusterPumpConfigurationAndControl params =
  sendMessage mtrClusterPumpConfigurationAndControl readAttributeMaxConstSpeedWithParamsSelector (toMTRReadParams params)

-- | @- readAttributeMinConstFlowWithParams:@
readAttributeMinConstFlowWithParams :: (IsMTRClusterPumpConfigurationAndControl mtrClusterPumpConfigurationAndControl, IsMTRReadParams params) => mtrClusterPumpConfigurationAndControl -> params -> IO (Id NSDictionary)
readAttributeMinConstFlowWithParams mtrClusterPumpConfigurationAndControl params =
  sendMessage mtrClusterPumpConfigurationAndControl readAttributeMinConstFlowWithParamsSelector (toMTRReadParams params)

-- | @- readAttributeMaxConstFlowWithParams:@
readAttributeMaxConstFlowWithParams :: (IsMTRClusterPumpConfigurationAndControl mtrClusterPumpConfigurationAndControl, IsMTRReadParams params) => mtrClusterPumpConfigurationAndControl -> params -> IO (Id NSDictionary)
readAttributeMaxConstFlowWithParams mtrClusterPumpConfigurationAndControl params =
  sendMessage mtrClusterPumpConfigurationAndControl readAttributeMaxConstFlowWithParamsSelector (toMTRReadParams params)

-- | @- readAttributeMinConstTempWithParams:@
readAttributeMinConstTempWithParams :: (IsMTRClusterPumpConfigurationAndControl mtrClusterPumpConfigurationAndControl, IsMTRReadParams params) => mtrClusterPumpConfigurationAndControl -> params -> IO (Id NSDictionary)
readAttributeMinConstTempWithParams mtrClusterPumpConfigurationAndControl params =
  sendMessage mtrClusterPumpConfigurationAndControl readAttributeMinConstTempWithParamsSelector (toMTRReadParams params)

-- | @- readAttributeMaxConstTempWithParams:@
readAttributeMaxConstTempWithParams :: (IsMTRClusterPumpConfigurationAndControl mtrClusterPumpConfigurationAndControl, IsMTRReadParams params) => mtrClusterPumpConfigurationAndControl -> params -> IO (Id NSDictionary)
readAttributeMaxConstTempWithParams mtrClusterPumpConfigurationAndControl params =
  sendMessage mtrClusterPumpConfigurationAndControl readAttributeMaxConstTempWithParamsSelector (toMTRReadParams params)

-- | @- readAttributePumpStatusWithParams:@
readAttributePumpStatusWithParams :: (IsMTRClusterPumpConfigurationAndControl mtrClusterPumpConfigurationAndControl, IsMTRReadParams params) => mtrClusterPumpConfigurationAndControl -> params -> IO (Id NSDictionary)
readAttributePumpStatusWithParams mtrClusterPumpConfigurationAndControl params =
  sendMessage mtrClusterPumpConfigurationAndControl readAttributePumpStatusWithParamsSelector (toMTRReadParams params)

-- | @- readAttributeEffectiveOperationModeWithParams:@
readAttributeEffectiveOperationModeWithParams :: (IsMTRClusterPumpConfigurationAndControl mtrClusterPumpConfigurationAndControl, IsMTRReadParams params) => mtrClusterPumpConfigurationAndControl -> params -> IO (Id NSDictionary)
readAttributeEffectiveOperationModeWithParams mtrClusterPumpConfigurationAndControl params =
  sendMessage mtrClusterPumpConfigurationAndControl readAttributeEffectiveOperationModeWithParamsSelector (toMTRReadParams params)

-- | @- readAttributeEffectiveControlModeWithParams:@
readAttributeEffectiveControlModeWithParams :: (IsMTRClusterPumpConfigurationAndControl mtrClusterPumpConfigurationAndControl, IsMTRReadParams params) => mtrClusterPumpConfigurationAndControl -> params -> IO (Id NSDictionary)
readAttributeEffectiveControlModeWithParams mtrClusterPumpConfigurationAndControl params =
  sendMessage mtrClusterPumpConfigurationAndControl readAttributeEffectiveControlModeWithParamsSelector (toMTRReadParams params)

-- | @- readAttributeCapacityWithParams:@
readAttributeCapacityWithParams :: (IsMTRClusterPumpConfigurationAndControl mtrClusterPumpConfigurationAndControl, IsMTRReadParams params) => mtrClusterPumpConfigurationAndControl -> params -> IO (Id NSDictionary)
readAttributeCapacityWithParams mtrClusterPumpConfigurationAndControl params =
  sendMessage mtrClusterPumpConfigurationAndControl readAttributeCapacityWithParamsSelector (toMTRReadParams params)

-- | @- readAttributeSpeedWithParams:@
readAttributeSpeedWithParams :: (IsMTRClusterPumpConfigurationAndControl mtrClusterPumpConfigurationAndControl, IsMTRReadParams params) => mtrClusterPumpConfigurationAndControl -> params -> IO (Id NSDictionary)
readAttributeSpeedWithParams mtrClusterPumpConfigurationAndControl params =
  sendMessage mtrClusterPumpConfigurationAndControl readAttributeSpeedWithParamsSelector (toMTRReadParams params)

-- | @- readAttributeLifetimeRunningHoursWithParams:@
readAttributeLifetimeRunningHoursWithParams :: (IsMTRClusterPumpConfigurationAndControl mtrClusterPumpConfigurationAndControl, IsMTRReadParams params) => mtrClusterPumpConfigurationAndControl -> params -> IO (Id NSDictionary)
readAttributeLifetimeRunningHoursWithParams mtrClusterPumpConfigurationAndControl params =
  sendMessage mtrClusterPumpConfigurationAndControl readAttributeLifetimeRunningHoursWithParamsSelector (toMTRReadParams params)

-- | @- writeAttributeLifetimeRunningHoursWithValue:expectedValueInterval:@
writeAttributeLifetimeRunningHoursWithValue_expectedValueInterval :: (IsMTRClusterPumpConfigurationAndControl mtrClusterPumpConfigurationAndControl, IsNSDictionary dataValueDictionary, IsNSNumber expectedValueIntervalMs) => mtrClusterPumpConfigurationAndControl -> dataValueDictionary -> expectedValueIntervalMs -> IO ()
writeAttributeLifetimeRunningHoursWithValue_expectedValueInterval mtrClusterPumpConfigurationAndControl dataValueDictionary expectedValueIntervalMs =
  sendMessage mtrClusterPumpConfigurationAndControl writeAttributeLifetimeRunningHoursWithValue_expectedValueIntervalSelector (toNSDictionary dataValueDictionary) (toNSNumber expectedValueIntervalMs)

-- | @- writeAttributeLifetimeRunningHoursWithValue:expectedValueInterval:params:@
writeAttributeLifetimeRunningHoursWithValue_expectedValueInterval_params :: (IsMTRClusterPumpConfigurationAndControl mtrClusterPumpConfigurationAndControl, IsNSDictionary dataValueDictionary, IsNSNumber expectedValueIntervalMs, IsMTRWriteParams params) => mtrClusterPumpConfigurationAndControl -> dataValueDictionary -> expectedValueIntervalMs -> params -> IO ()
writeAttributeLifetimeRunningHoursWithValue_expectedValueInterval_params mtrClusterPumpConfigurationAndControl dataValueDictionary expectedValueIntervalMs params =
  sendMessage mtrClusterPumpConfigurationAndControl writeAttributeLifetimeRunningHoursWithValue_expectedValueInterval_paramsSelector (toNSDictionary dataValueDictionary) (toNSNumber expectedValueIntervalMs) (toMTRWriteParams params)

-- | @- readAttributePowerWithParams:@
readAttributePowerWithParams :: (IsMTRClusterPumpConfigurationAndControl mtrClusterPumpConfigurationAndControl, IsMTRReadParams params) => mtrClusterPumpConfigurationAndControl -> params -> IO (Id NSDictionary)
readAttributePowerWithParams mtrClusterPumpConfigurationAndControl params =
  sendMessage mtrClusterPumpConfigurationAndControl readAttributePowerWithParamsSelector (toMTRReadParams params)

-- | @- readAttributeLifetimeEnergyConsumedWithParams:@
readAttributeLifetimeEnergyConsumedWithParams :: (IsMTRClusterPumpConfigurationAndControl mtrClusterPumpConfigurationAndControl, IsMTRReadParams params) => mtrClusterPumpConfigurationAndControl -> params -> IO (Id NSDictionary)
readAttributeLifetimeEnergyConsumedWithParams mtrClusterPumpConfigurationAndControl params =
  sendMessage mtrClusterPumpConfigurationAndControl readAttributeLifetimeEnergyConsumedWithParamsSelector (toMTRReadParams params)

-- | @- writeAttributeLifetimeEnergyConsumedWithValue:expectedValueInterval:@
writeAttributeLifetimeEnergyConsumedWithValue_expectedValueInterval :: (IsMTRClusterPumpConfigurationAndControl mtrClusterPumpConfigurationAndControl, IsNSDictionary dataValueDictionary, IsNSNumber expectedValueIntervalMs) => mtrClusterPumpConfigurationAndControl -> dataValueDictionary -> expectedValueIntervalMs -> IO ()
writeAttributeLifetimeEnergyConsumedWithValue_expectedValueInterval mtrClusterPumpConfigurationAndControl dataValueDictionary expectedValueIntervalMs =
  sendMessage mtrClusterPumpConfigurationAndControl writeAttributeLifetimeEnergyConsumedWithValue_expectedValueIntervalSelector (toNSDictionary dataValueDictionary) (toNSNumber expectedValueIntervalMs)

-- | @- writeAttributeLifetimeEnergyConsumedWithValue:expectedValueInterval:params:@
writeAttributeLifetimeEnergyConsumedWithValue_expectedValueInterval_params :: (IsMTRClusterPumpConfigurationAndControl mtrClusterPumpConfigurationAndControl, IsNSDictionary dataValueDictionary, IsNSNumber expectedValueIntervalMs, IsMTRWriteParams params) => mtrClusterPumpConfigurationAndControl -> dataValueDictionary -> expectedValueIntervalMs -> params -> IO ()
writeAttributeLifetimeEnergyConsumedWithValue_expectedValueInterval_params mtrClusterPumpConfigurationAndControl dataValueDictionary expectedValueIntervalMs params =
  sendMessage mtrClusterPumpConfigurationAndControl writeAttributeLifetimeEnergyConsumedWithValue_expectedValueInterval_paramsSelector (toNSDictionary dataValueDictionary) (toNSNumber expectedValueIntervalMs) (toMTRWriteParams params)

-- | @- readAttributeOperationModeWithParams:@
readAttributeOperationModeWithParams :: (IsMTRClusterPumpConfigurationAndControl mtrClusterPumpConfigurationAndControl, IsMTRReadParams params) => mtrClusterPumpConfigurationAndControl -> params -> IO (Id NSDictionary)
readAttributeOperationModeWithParams mtrClusterPumpConfigurationAndControl params =
  sendMessage mtrClusterPumpConfigurationAndControl readAttributeOperationModeWithParamsSelector (toMTRReadParams params)

-- | @- writeAttributeOperationModeWithValue:expectedValueInterval:@
writeAttributeOperationModeWithValue_expectedValueInterval :: (IsMTRClusterPumpConfigurationAndControl mtrClusterPumpConfigurationAndControl, IsNSDictionary dataValueDictionary, IsNSNumber expectedValueIntervalMs) => mtrClusterPumpConfigurationAndControl -> dataValueDictionary -> expectedValueIntervalMs -> IO ()
writeAttributeOperationModeWithValue_expectedValueInterval mtrClusterPumpConfigurationAndControl dataValueDictionary expectedValueIntervalMs =
  sendMessage mtrClusterPumpConfigurationAndControl writeAttributeOperationModeWithValue_expectedValueIntervalSelector (toNSDictionary dataValueDictionary) (toNSNumber expectedValueIntervalMs)

-- | @- writeAttributeOperationModeWithValue:expectedValueInterval:params:@
writeAttributeOperationModeWithValue_expectedValueInterval_params :: (IsMTRClusterPumpConfigurationAndControl mtrClusterPumpConfigurationAndControl, IsNSDictionary dataValueDictionary, IsNSNumber expectedValueIntervalMs, IsMTRWriteParams params) => mtrClusterPumpConfigurationAndControl -> dataValueDictionary -> expectedValueIntervalMs -> params -> IO ()
writeAttributeOperationModeWithValue_expectedValueInterval_params mtrClusterPumpConfigurationAndControl dataValueDictionary expectedValueIntervalMs params =
  sendMessage mtrClusterPumpConfigurationAndControl writeAttributeOperationModeWithValue_expectedValueInterval_paramsSelector (toNSDictionary dataValueDictionary) (toNSNumber expectedValueIntervalMs) (toMTRWriteParams params)

-- | @- readAttributeControlModeWithParams:@
readAttributeControlModeWithParams :: (IsMTRClusterPumpConfigurationAndControl mtrClusterPumpConfigurationAndControl, IsMTRReadParams params) => mtrClusterPumpConfigurationAndControl -> params -> IO (Id NSDictionary)
readAttributeControlModeWithParams mtrClusterPumpConfigurationAndControl params =
  sendMessage mtrClusterPumpConfigurationAndControl readAttributeControlModeWithParamsSelector (toMTRReadParams params)

-- | @- writeAttributeControlModeWithValue:expectedValueInterval:@
writeAttributeControlModeWithValue_expectedValueInterval :: (IsMTRClusterPumpConfigurationAndControl mtrClusterPumpConfigurationAndControl, IsNSDictionary dataValueDictionary, IsNSNumber expectedValueIntervalMs) => mtrClusterPumpConfigurationAndControl -> dataValueDictionary -> expectedValueIntervalMs -> IO ()
writeAttributeControlModeWithValue_expectedValueInterval mtrClusterPumpConfigurationAndControl dataValueDictionary expectedValueIntervalMs =
  sendMessage mtrClusterPumpConfigurationAndControl writeAttributeControlModeWithValue_expectedValueIntervalSelector (toNSDictionary dataValueDictionary) (toNSNumber expectedValueIntervalMs)

-- | @- writeAttributeControlModeWithValue:expectedValueInterval:params:@
writeAttributeControlModeWithValue_expectedValueInterval_params :: (IsMTRClusterPumpConfigurationAndControl mtrClusterPumpConfigurationAndControl, IsNSDictionary dataValueDictionary, IsNSNumber expectedValueIntervalMs, IsMTRWriteParams params) => mtrClusterPumpConfigurationAndControl -> dataValueDictionary -> expectedValueIntervalMs -> params -> IO ()
writeAttributeControlModeWithValue_expectedValueInterval_params mtrClusterPumpConfigurationAndControl dataValueDictionary expectedValueIntervalMs params =
  sendMessage mtrClusterPumpConfigurationAndControl writeAttributeControlModeWithValue_expectedValueInterval_paramsSelector (toNSDictionary dataValueDictionary) (toNSNumber expectedValueIntervalMs) (toMTRWriteParams params)

-- | @- readAttributeGeneratedCommandListWithParams:@
readAttributeGeneratedCommandListWithParams :: (IsMTRClusterPumpConfigurationAndControl mtrClusterPumpConfigurationAndControl, IsMTRReadParams params) => mtrClusterPumpConfigurationAndControl -> params -> IO (Id NSDictionary)
readAttributeGeneratedCommandListWithParams mtrClusterPumpConfigurationAndControl params =
  sendMessage mtrClusterPumpConfigurationAndControl readAttributeGeneratedCommandListWithParamsSelector (toMTRReadParams params)

-- | @- readAttributeAcceptedCommandListWithParams:@
readAttributeAcceptedCommandListWithParams :: (IsMTRClusterPumpConfigurationAndControl mtrClusterPumpConfigurationAndControl, IsMTRReadParams params) => mtrClusterPumpConfigurationAndControl -> params -> IO (Id NSDictionary)
readAttributeAcceptedCommandListWithParams mtrClusterPumpConfigurationAndControl params =
  sendMessage mtrClusterPumpConfigurationAndControl readAttributeAcceptedCommandListWithParamsSelector (toMTRReadParams params)

-- | @- readAttributeAttributeListWithParams:@
readAttributeAttributeListWithParams :: (IsMTRClusterPumpConfigurationAndControl mtrClusterPumpConfigurationAndControl, IsMTRReadParams params) => mtrClusterPumpConfigurationAndControl -> params -> IO (Id NSDictionary)
readAttributeAttributeListWithParams mtrClusterPumpConfigurationAndControl params =
  sendMessage mtrClusterPumpConfigurationAndControl readAttributeAttributeListWithParamsSelector (toMTRReadParams params)

-- | @- readAttributeFeatureMapWithParams:@
readAttributeFeatureMapWithParams :: (IsMTRClusterPumpConfigurationAndControl mtrClusterPumpConfigurationAndControl, IsMTRReadParams params) => mtrClusterPumpConfigurationAndControl -> params -> IO (Id NSDictionary)
readAttributeFeatureMapWithParams mtrClusterPumpConfigurationAndControl params =
  sendMessage mtrClusterPumpConfigurationAndControl readAttributeFeatureMapWithParamsSelector (toMTRReadParams params)

-- | @- readAttributeClusterRevisionWithParams:@
readAttributeClusterRevisionWithParams :: (IsMTRClusterPumpConfigurationAndControl mtrClusterPumpConfigurationAndControl, IsMTRReadParams params) => mtrClusterPumpConfigurationAndControl -> params -> IO (Id NSDictionary)
readAttributeClusterRevisionWithParams mtrClusterPumpConfigurationAndControl params =
  sendMessage mtrClusterPumpConfigurationAndControl readAttributeClusterRevisionWithParamsSelector (toMTRReadParams params)

-- | @- init@
init_ :: IsMTRClusterPumpConfigurationAndControl mtrClusterPumpConfigurationAndControl => mtrClusterPumpConfigurationAndControl -> IO (Id MTRClusterPumpConfigurationAndControl)
init_ mtrClusterPumpConfigurationAndControl =
  sendOwnedMessage mtrClusterPumpConfigurationAndControl initSelector

-- | @+ new@
new :: IO (Id MTRClusterPumpConfigurationAndControl)
new  =
  do
    cls' <- getRequiredClass "MTRClusterPumpConfigurationAndControl"
    sendOwnedClassMessage cls' newSelector

-- | @- initWithDevice:endpoint:queue:@
initWithDevice_endpoint_queue :: (IsMTRClusterPumpConfigurationAndControl mtrClusterPumpConfigurationAndControl, IsMTRDevice device, IsNSObject queue) => mtrClusterPumpConfigurationAndControl -> device -> CUShort -> queue -> IO (Id MTRClusterPumpConfigurationAndControl)
initWithDevice_endpoint_queue mtrClusterPumpConfigurationAndControl device endpoint queue =
  sendOwnedMessage mtrClusterPumpConfigurationAndControl initWithDevice_endpoint_queueSelector (toMTRDevice device) endpoint (toNSObject queue)

-- | The queue is currently unused, but may be used in the future for calling completions for command invocations if commands are added to this cluster.
--
-- ObjC selector: @- initWithDevice:endpointID:queue:@
initWithDevice_endpointID_queue :: (IsMTRClusterPumpConfigurationAndControl mtrClusterPumpConfigurationAndControl, IsMTRDevice device, IsNSNumber endpointID, IsNSObject queue) => mtrClusterPumpConfigurationAndControl -> device -> endpointID -> queue -> IO (Id MTRClusterPumpConfigurationAndControl)
initWithDevice_endpointID_queue mtrClusterPumpConfigurationAndControl device endpointID queue =
  sendOwnedMessage mtrClusterPumpConfigurationAndControl initWithDevice_endpointID_queueSelector (toMTRDevice device) (toNSNumber endpointID) (toNSObject queue)

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @readAttributeMaxPressureWithParams:@
readAttributeMaxPressureWithParamsSelector :: Selector '[Id MTRReadParams] (Id NSDictionary)
readAttributeMaxPressureWithParamsSelector = mkSelector "readAttributeMaxPressureWithParams:"

-- | @Selector@ for @readAttributeMaxSpeedWithParams:@
readAttributeMaxSpeedWithParamsSelector :: Selector '[Id MTRReadParams] (Id NSDictionary)
readAttributeMaxSpeedWithParamsSelector = mkSelector "readAttributeMaxSpeedWithParams:"

-- | @Selector@ for @readAttributeMaxFlowWithParams:@
readAttributeMaxFlowWithParamsSelector :: Selector '[Id MTRReadParams] (Id NSDictionary)
readAttributeMaxFlowWithParamsSelector = mkSelector "readAttributeMaxFlowWithParams:"

-- | @Selector@ for @readAttributeMinConstPressureWithParams:@
readAttributeMinConstPressureWithParamsSelector :: Selector '[Id MTRReadParams] (Id NSDictionary)
readAttributeMinConstPressureWithParamsSelector = mkSelector "readAttributeMinConstPressureWithParams:"

-- | @Selector@ for @readAttributeMaxConstPressureWithParams:@
readAttributeMaxConstPressureWithParamsSelector :: Selector '[Id MTRReadParams] (Id NSDictionary)
readAttributeMaxConstPressureWithParamsSelector = mkSelector "readAttributeMaxConstPressureWithParams:"

-- | @Selector@ for @readAttributeMinCompPressureWithParams:@
readAttributeMinCompPressureWithParamsSelector :: Selector '[Id MTRReadParams] (Id NSDictionary)
readAttributeMinCompPressureWithParamsSelector = mkSelector "readAttributeMinCompPressureWithParams:"

-- | @Selector@ for @readAttributeMaxCompPressureWithParams:@
readAttributeMaxCompPressureWithParamsSelector :: Selector '[Id MTRReadParams] (Id NSDictionary)
readAttributeMaxCompPressureWithParamsSelector = mkSelector "readAttributeMaxCompPressureWithParams:"

-- | @Selector@ for @readAttributeMinConstSpeedWithParams:@
readAttributeMinConstSpeedWithParamsSelector :: Selector '[Id MTRReadParams] (Id NSDictionary)
readAttributeMinConstSpeedWithParamsSelector = mkSelector "readAttributeMinConstSpeedWithParams:"

-- | @Selector@ for @readAttributeMaxConstSpeedWithParams:@
readAttributeMaxConstSpeedWithParamsSelector :: Selector '[Id MTRReadParams] (Id NSDictionary)
readAttributeMaxConstSpeedWithParamsSelector = mkSelector "readAttributeMaxConstSpeedWithParams:"

-- | @Selector@ for @readAttributeMinConstFlowWithParams:@
readAttributeMinConstFlowWithParamsSelector :: Selector '[Id MTRReadParams] (Id NSDictionary)
readAttributeMinConstFlowWithParamsSelector = mkSelector "readAttributeMinConstFlowWithParams:"

-- | @Selector@ for @readAttributeMaxConstFlowWithParams:@
readAttributeMaxConstFlowWithParamsSelector :: Selector '[Id MTRReadParams] (Id NSDictionary)
readAttributeMaxConstFlowWithParamsSelector = mkSelector "readAttributeMaxConstFlowWithParams:"

-- | @Selector@ for @readAttributeMinConstTempWithParams:@
readAttributeMinConstTempWithParamsSelector :: Selector '[Id MTRReadParams] (Id NSDictionary)
readAttributeMinConstTempWithParamsSelector = mkSelector "readAttributeMinConstTempWithParams:"

-- | @Selector@ for @readAttributeMaxConstTempWithParams:@
readAttributeMaxConstTempWithParamsSelector :: Selector '[Id MTRReadParams] (Id NSDictionary)
readAttributeMaxConstTempWithParamsSelector = mkSelector "readAttributeMaxConstTempWithParams:"

-- | @Selector@ for @readAttributePumpStatusWithParams:@
readAttributePumpStatusWithParamsSelector :: Selector '[Id MTRReadParams] (Id NSDictionary)
readAttributePumpStatusWithParamsSelector = mkSelector "readAttributePumpStatusWithParams:"

-- | @Selector@ for @readAttributeEffectiveOperationModeWithParams:@
readAttributeEffectiveOperationModeWithParamsSelector :: Selector '[Id MTRReadParams] (Id NSDictionary)
readAttributeEffectiveOperationModeWithParamsSelector = mkSelector "readAttributeEffectiveOperationModeWithParams:"

-- | @Selector@ for @readAttributeEffectiveControlModeWithParams:@
readAttributeEffectiveControlModeWithParamsSelector :: Selector '[Id MTRReadParams] (Id NSDictionary)
readAttributeEffectiveControlModeWithParamsSelector = mkSelector "readAttributeEffectiveControlModeWithParams:"

-- | @Selector@ for @readAttributeCapacityWithParams:@
readAttributeCapacityWithParamsSelector :: Selector '[Id MTRReadParams] (Id NSDictionary)
readAttributeCapacityWithParamsSelector = mkSelector "readAttributeCapacityWithParams:"

-- | @Selector@ for @readAttributeSpeedWithParams:@
readAttributeSpeedWithParamsSelector :: Selector '[Id MTRReadParams] (Id NSDictionary)
readAttributeSpeedWithParamsSelector = mkSelector "readAttributeSpeedWithParams:"

-- | @Selector@ for @readAttributeLifetimeRunningHoursWithParams:@
readAttributeLifetimeRunningHoursWithParamsSelector :: Selector '[Id MTRReadParams] (Id NSDictionary)
readAttributeLifetimeRunningHoursWithParamsSelector = mkSelector "readAttributeLifetimeRunningHoursWithParams:"

-- | @Selector@ for @writeAttributeLifetimeRunningHoursWithValue:expectedValueInterval:@
writeAttributeLifetimeRunningHoursWithValue_expectedValueIntervalSelector :: Selector '[Id NSDictionary, Id NSNumber] ()
writeAttributeLifetimeRunningHoursWithValue_expectedValueIntervalSelector = mkSelector "writeAttributeLifetimeRunningHoursWithValue:expectedValueInterval:"

-- | @Selector@ for @writeAttributeLifetimeRunningHoursWithValue:expectedValueInterval:params:@
writeAttributeLifetimeRunningHoursWithValue_expectedValueInterval_paramsSelector :: Selector '[Id NSDictionary, Id NSNumber, Id MTRWriteParams] ()
writeAttributeLifetimeRunningHoursWithValue_expectedValueInterval_paramsSelector = mkSelector "writeAttributeLifetimeRunningHoursWithValue:expectedValueInterval:params:"

-- | @Selector@ for @readAttributePowerWithParams:@
readAttributePowerWithParamsSelector :: Selector '[Id MTRReadParams] (Id NSDictionary)
readAttributePowerWithParamsSelector = mkSelector "readAttributePowerWithParams:"

-- | @Selector@ for @readAttributeLifetimeEnergyConsumedWithParams:@
readAttributeLifetimeEnergyConsumedWithParamsSelector :: Selector '[Id MTRReadParams] (Id NSDictionary)
readAttributeLifetimeEnergyConsumedWithParamsSelector = mkSelector "readAttributeLifetimeEnergyConsumedWithParams:"

-- | @Selector@ for @writeAttributeLifetimeEnergyConsumedWithValue:expectedValueInterval:@
writeAttributeLifetimeEnergyConsumedWithValue_expectedValueIntervalSelector :: Selector '[Id NSDictionary, Id NSNumber] ()
writeAttributeLifetimeEnergyConsumedWithValue_expectedValueIntervalSelector = mkSelector "writeAttributeLifetimeEnergyConsumedWithValue:expectedValueInterval:"

-- | @Selector@ for @writeAttributeLifetimeEnergyConsumedWithValue:expectedValueInterval:params:@
writeAttributeLifetimeEnergyConsumedWithValue_expectedValueInterval_paramsSelector :: Selector '[Id NSDictionary, Id NSNumber, Id MTRWriteParams] ()
writeAttributeLifetimeEnergyConsumedWithValue_expectedValueInterval_paramsSelector = mkSelector "writeAttributeLifetimeEnergyConsumedWithValue:expectedValueInterval:params:"

-- | @Selector@ for @readAttributeOperationModeWithParams:@
readAttributeOperationModeWithParamsSelector :: Selector '[Id MTRReadParams] (Id NSDictionary)
readAttributeOperationModeWithParamsSelector = mkSelector "readAttributeOperationModeWithParams:"

-- | @Selector@ for @writeAttributeOperationModeWithValue:expectedValueInterval:@
writeAttributeOperationModeWithValue_expectedValueIntervalSelector :: Selector '[Id NSDictionary, Id NSNumber] ()
writeAttributeOperationModeWithValue_expectedValueIntervalSelector = mkSelector "writeAttributeOperationModeWithValue:expectedValueInterval:"

-- | @Selector@ for @writeAttributeOperationModeWithValue:expectedValueInterval:params:@
writeAttributeOperationModeWithValue_expectedValueInterval_paramsSelector :: Selector '[Id NSDictionary, Id NSNumber, Id MTRWriteParams] ()
writeAttributeOperationModeWithValue_expectedValueInterval_paramsSelector = mkSelector "writeAttributeOperationModeWithValue:expectedValueInterval:params:"

-- | @Selector@ for @readAttributeControlModeWithParams:@
readAttributeControlModeWithParamsSelector :: Selector '[Id MTRReadParams] (Id NSDictionary)
readAttributeControlModeWithParamsSelector = mkSelector "readAttributeControlModeWithParams:"

-- | @Selector@ for @writeAttributeControlModeWithValue:expectedValueInterval:@
writeAttributeControlModeWithValue_expectedValueIntervalSelector :: Selector '[Id NSDictionary, Id NSNumber] ()
writeAttributeControlModeWithValue_expectedValueIntervalSelector = mkSelector "writeAttributeControlModeWithValue:expectedValueInterval:"

-- | @Selector@ for @writeAttributeControlModeWithValue:expectedValueInterval:params:@
writeAttributeControlModeWithValue_expectedValueInterval_paramsSelector :: Selector '[Id NSDictionary, Id NSNumber, Id MTRWriteParams] ()
writeAttributeControlModeWithValue_expectedValueInterval_paramsSelector = mkSelector "writeAttributeControlModeWithValue:expectedValueInterval:params:"

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
initSelector :: Selector '[] (Id MTRClusterPumpConfigurationAndControl)
initSelector = mkSelector "init"

-- | @Selector@ for @new@
newSelector :: Selector '[] (Id MTRClusterPumpConfigurationAndControl)
newSelector = mkSelector "new"

-- | @Selector@ for @initWithDevice:endpoint:queue:@
initWithDevice_endpoint_queueSelector :: Selector '[Id MTRDevice, CUShort, Id NSObject] (Id MTRClusterPumpConfigurationAndControl)
initWithDevice_endpoint_queueSelector = mkSelector "initWithDevice:endpoint:queue:"

-- | @Selector@ for @initWithDevice:endpointID:queue:@
initWithDevice_endpointID_queueSelector :: Selector '[Id MTRDevice, Id NSNumber, Id NSObject] (Id MTRClusterPumpConfigurationAndControl)
initWithDevice_endpointID_queueSelector = mkSelector "initWithDevice:endpointID:queue:"

