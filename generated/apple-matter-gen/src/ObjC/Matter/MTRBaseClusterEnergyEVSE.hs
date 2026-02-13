{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Cluster Energy EVSE
--
-- Electric Vehicle Supply Equipment (EVSE) is equipment used to charge an Electric Vehicle (EV) or Plug-In Hybrid Electric Vehicle. This cluster provides an interface to the functionality of Electric Vehicle Supply Equipment (EVSE) management.
--
-- Generated bindings for @MTRBaseClusterEnergyEVSE@.
module ObjC.Matter.MTRBaseClusterEnergyEVSE
  ( MTRBaseClusterEnergyEVSE
  , IsMTRBaseClusterEnergyEVSE(..)
  , disableWithParams_completion
  , disableWithCompletion
  , enableChargingWithParams_completion
  , enableDischargingWithParams_completion
  , startDiagnosticsWithParams_completion
  , startDiagnosticsWithCompletion
  , setTargetsWithParams_completion
  , getTargetsWithParams_completion
  , getTargetsWithCompletion
  , clearTargetsWithParams_completion
  , clearTargetsWithCompletion
  , readAttributeStateWithCompletion
  , subscribeAttributeStateWithParams_subscriptionEstablished_reportHandler
  , readAttributeStateWithClusterStateCache_endpoint_queue_completion
  , readAttributeSupplyStateWithCompletion
  , subscribeAttributeSupplyStateWithParams_subscriptionEstablished_reportHandler
  , readAttributeSupplyStateWithClusterStateCache_endpoint_queue_completion
  , readAttributeFaultStateWithCompletion
  , subscribeAttributeFaultStateWithParams_subscriptionEstablished_reportHandler
  , readAttributeFaultStateWithClusterStateCache_endpoint_queue_completion
  , readAttributeChargingEnabledUntilWithCompletion
  , subscribeAttributeChargingEnabledUntilWithParams_subscriptionEstablished_reportHandler
  , readAttributeChargingEnabledUntilWithClusterStateCache_endpoint_queue_completion
  , readAttributeDischargingEnabledUntilWithCompletion
  , subscribeAttributeDischargingEnabledUntilWithParams_subscriptionEstablished_reportHandler
  , readAttributeDischargingEnabledUntilWithClusterStateCache_endpoint_queue_completion
  , readAttributeCircuitCapacityWithCompletion
  , subscribeAttributeCircuitCapacityWithParams_subscriptionEstablished_reportHandler
  , readAttributeCircuitCapacityWithClusterStateCache_endpoint_queue_completion
  , readAttributeMinimumChargeCurrentWithCompletion
  , subscribeAttributeMinimumChargeCurrentWithParams_subscriptionEstablished_reportHandler
  , readAttributeMinimumChargeCurrentWithClusterStateCache_endpoint_queue_completion
  , readAttributeMaximumChargeCurrentWithCompletion
  , subscribeAttributeMaximumChargeCurrentWithParams_subscriptionEstablished_reportHandler
  , readAttributeMaximumChargeCurrentWithClusterStateCache_endpoint_queue_completion
  , readAttributeMaximumDischargeCurrentWithCompletion
  , subscribeAttributeMaximumDischargeCurrentWithParams_subscriptionEstablished_reportHandler
  , readAttributeMaximumDischargeCurrentWithClusterStateCache_endpoint_queue_completion
  , readAttributeUserMaximumChargeCurrentWithCompletion
  , writeAttributeUserMaximumChargeCurrentWithValue_completion
  , writeAttributeUserMaximumChargeCurrentWithValue_params_completion
  , subscribeAttributeUserMaximumChargeCurrentWithParams_subscriptionEstablished_reportHandler
  , readAttributeUserMaximumChargeCurrentWithClusterStateCache_endpoint_queue_completion
  , readAttributeRandomizationDelayWindowWithCompletion
  , writeAttributeRandomizationDelayWindowWithValue_completion
  , writeAttributeRandomizationDelayWindowWithValue_params_completion
  , subscribeAttributeRandomizationDelayWindowWithParams_subscriptionEstablished_reportHandler
  , readAttributeRandomizationDelayWindowWithClusterStateCache_endpoint_queue_completion
  , readAttributeNextChargeStartTimeWithCompletion
  , subscribeAttributeNextChargeStartTimeWithParams_subscriptionEstablished_reportHandler
  , readAttributeNextChargeStartTimeWithClusterStateCache_endpoint_queue_completion
  , readAttributeNextChargeTargetTimeWithCompletion
  , subscribeAttributeNextChargeTargetTimeWithParams_subscriptionEstablished_reportHandler
  , readAttributeNextChargeTargetTimeWithClusterStateCache_endpoint_queue_completion
  , readAttributeNextChargeRequiredEnergyWithCompletion
  , subscribeAttributeNextChargeRequiredEnergyWithParams_subscriptionEstablished_reportHandler
  , readAttributeNextChargeRequiredEnergyWithClusterStateCache_endpoint_queue_completion
  , readAttributeNextChargeTargetSoCWithCompletion
  , subscribeAttributeNextChargeTargetSoCWithParams_subscriptionEstablished_reportHandler
  , readAttributeNextChargeTargetSoCWithClusterStateCache_endpoint_queue_completion
  , readAttributeApproximateEVEfficiencyWithCompletion
  , writeAttributeApproximateEVEfficiencyWithValue_completion
  , writeAttributeApproximateEVEfficiencyWithValue_params_completion
  , subscribeAttributeApproximateEVEfficiencyWithParams_subscriptionEstablished_reportHandler
  , readAttributeApproximateEVEfficiencyWithClusterStateCache_endpoint_queue_completion
  , readAttributeStateOfChargeWithCompletion
  , subscribeAttributeStateOfChargeWithParams_subscriptionEstablished_reportHandler
  , readAttributeStateOfChargeWithClusterStateCache_endpoint_queue_completion
  , readAttributeBatteryCapacityWithCompletion
  , subscribeAttributeBatteryCapacityWithParams_subscriptionEstablished_reportHandler
  , readAttributeBatteryCapacityWithClusterStateCache_endpoint_queue_completion
  , readAttributeVehicleIDWithCompletion
  , subscribeAttributeVehicleIDWithParams_subscriptionEstablished_reportHandler
  , readAttributeVehicleIDWithClusterStateCache_endpoint_queue_completion
  , readAttributeSessionIDWithCompletion
  , subscribeAttributeSessionIDWithParams_subscriptionEstablished_reportHandler
  , readAttributeSessionIDWithClusterStateCache_endpoint_queue_completion
  , readAttributeSessionDurationWithCompletion
  , subscribeAttributeSessionDurationWithParams_subscriptionEstablished_reportHandler
  , readAttributeSessionDurationWithClusterStateCache_endpoint_queue_completion
  , readAttributeSessionEnergyChargedWithCompletion
  , subscribeAttributeSessionEnergyChargedWithParams_subscriptionEstablished_reportHandler
  , readAttributeSessionEnergyChargedWithClusterStateCache_endpoint_queue_completion
  , readAttributeSessionEnergyDischargedWithCompletion
  , subscribeAttributeSessionEnergyDischargedWithParams_subscriptionEstablished_reportHandler
  , readAttributeSessionEnergyDischargedWithClusterStateCache_endpoint_queue_completion
  , readAttributeGeneratedCommandListWithCompletion
  , subscribeAttributeGeneratedCommandListWithParams_subscriptionEstablished_reportHandler
  , readAttributeGeneratedCommandListWithClusterStateCache_endpoint_queue_completion
  , readAttributeAcceptedCommandListWithCompletion
  , subscribeAttributeAcceptedCommandListWithParams_subscriptionEstablished_reportHandler
  , readAttributeAcceptedCommandListWithClusterStateCache_endpoint_queue_completion
  , readAttributeAttributeListWithCompletion
  , subscribeAttributeAttributeListWithParams_subscriptionEstablished_reportHandler
  , readAttributeAttributeListWithClusterStateCache_endpoint_queue_completion
  , readAttributeFeatureMapWithCompletion
  , subscribeAttributeFeatureMapWithParams_subscriptionEstablished_reportHandler
  , readAttributeFeatureMapWithClusterStateCache_endpoint_queue_completion
  , readAttributeClusterRevisionWithCompletion
  , subscribeAttributeClusterRevisionWithParams_subscriptionEstablished_reportHandler
  , readAttributeClusterRevisionWithClusterStateCache_endpoint_queue_completion
  , init_
  , new
  , initWithDevice_endpointID_queue
  , clearTargetsWithCompletionSelector
  , clearTargetsWithParams_completionSelector
  , disableWithCompletionSelector
  , disableWithParams_completionSelector
  , enableChargingWithParams_completionSelector
  , enableDischargingWithParams_completionSelector
  , getTargetsWithCompletionSelector
  , getTargetsWithParams_completionSelector
  , initSelector
  , initWithDevice_endpointID_queueSelector
  , newSelector
  , readAttributeAcceptedCommandListWithClusterStateCache_endpoint_queue_completionSelector
  , readAttributeAcceptedCommandListWithCompletionSelector
  , readAttributeApproximateEVEfficiencyWithClusterStateCache_endpoint_queue_completionSelector
  , readAttributeApproximateEVEfficiencyWithCompletionSelector
  , readAttributeAttributeListWithClusterStateCache_endpoint_queue_completionSelector
  , readAttributeAttributeListWithCompletionSelector
  , readAttributeBatteryCapacityWithClusterStateCache_endpoint_queue_completionSelector
  , readAttributeBatteryCapacityWithCompletionSelector
  , readAttributeChargingEnabledUntilWithClusterStateCache_endpoint_queue_completionSelector
  , readAttributeChargingEnabledUntilWithCompletionSelector
  , readAttributeCircuitCapacityWithClusterStateCache_endpoint_queue_completionSelector
  , readAttributeCircuitCapacityWithCompletionSelector
  , readAttributeClusterRevisionWithClusterStateCache_endpoint_queue_completionSelector
  , readAttributeClusterRevisionWithCompletionSelector
  , readAttributeDischargingEnabledUntilWithClusterStateCache_endpoint_queue_completionSelector
  , readAttributeDischargingEnabledUntilWithCompletionSelector
  , readAttributeFaultStateWithClusterStateCache_endpoint_queue_completionSelector
  , readAttributeFaultStateWithCompletionSelector
  , readAttributeFeatureMapWithClusterStateCache_endpoint_queue_completionSelector
  , readAttributeFeatureMapWithCompletionSelector
  , readAttributeGeneratedCommandListWithClusterStateCache_endpoint_queue_completionSelector
  , readAttributeGeneratedCommandListWithCompletionSelector
  , readAttributeMaximumChargeCurrentWithClusterStateCache_endpoint_queue_completionSelector
  , readAttributeMaximumChargeCurrentWithCompletionSelector
  , readAttributeMaximumDischargeCurrentWithClusterStateCache_endpoint_queue_completionSelector
  , readAttributeMaximumDischargeCurrentWithCompletionSelector
  , readAttributeMinimumChargeCurrentWithClusterStateCache_endpoint_queue_completionSelector
  , readAttributeMinimumChargeCurrentWithCompletionSelector
  , readAttributeNextChargeRequiredEnergyWithClusterStateCache_endpoint_queue_completionSelector
  , readAttributeNextChargeRequiredEnergyWithCompletionSelector
  , readAttributeNextChargeStartTimeWithClusterStateCache_endpoint_queue_completionSelector
  , readAttributeNextChargeStartTimeWithCompletionSelector
  , readAttributeNextChargeTargetSoCWithClusterStateCache_endpoint_queue_completionSelector
  , readAttributeNextChargeTargetSoCWithCompletionSelector
  , readAttributeNextChargeTargetTimeWithClusterStateCache_endpoint_queue_completionSelector
  , readAttributeNextChargeTargetTimeWithCompletionSelector
  , readAttributeRandomizationDelayWindowWithClusterStateCache_endpoint_queue_completionSelector
  , readAttributeRandomizationDelayWindowWithCompletionSelector
  , readAttributeSessionDurationWithClusterStateCache_endpoint_queue_completionSelector
  , readAttributeSessionDurationWithCompletionSelector
  , readAttributeSessionEnergyChargedWithClusterStateCache_endpoint_queue_completionSelector
  , readAttributeSessionEnergyChargedWithCompletionSelector
  , readAttributeSessionEnergyDischargedWithClusterStateCache_endpoint_queue_completionSelector
  , readAttributeSessionEnergyDischargedWithCompletionSelector
  , readAttributeSessionIDWithClusterStateCache_endpoint_queue_completionSelector
  , readAttributeSessionIDWithCompletionSelector
  , readAttributeStateOfChargeWithClusterStateCache_endpoint_queue_completionSelector
  , readAttributeStateOfChargeWithCompletionSelector
  , readAttributeStateWithClusterStateCache_endpoint_queue_completionSelector
  , readAttributeStateWithCompletionSelector
  , readAttributeSupplyStateWithClusterStateCache_endpoint_queue_completionSelector
  , readAttributeSupplyStateWithCompletionSelector
  , readAttributeUserMaximumChargeCurrentWithClusterStateCache_endpoint_queue_completionSelector
  , readAttributeUserMaximumChargeCurrentWithCompletionSelector
  , readAttributeVehicleIDWithClusterStateCache_endpoint_queue_completionSelector
  , readAttributeVehicleIDWithCompletionSelector
  , setTargetsWithParams_completionSelector
  , startDiagnosticsWithCompletionSelector
  , startDiagnosticsWithParams_completionSelector
  , subscribeAttributeAcceptedCommandListWithParams_subscriptionEstablished_reportHandlerSelector
  , subscribeAttributeApproximateEVEfficiencyWithParams_subscriptionEstablished_reportHandlerSelector
  , subscribeAttributeAttributeListWithParams_subscriptionEstablished_reportHandlerSelector
  , subscribeAttributeBatteryCapacityWithParams_subscriptionEstablished_reportHandlerSelector
  , subscribeAttributeChargingEnabledUntilWithParams_subscriptionEstablished_reportHandlerSelector
  , subscribeAttributeCircuitCapacityWithParams_subscriptionEstablished_reportHandlerSelector
  , subscribeAttributeClusterRevisionWithParams_subscriptionEstablished_reportHandlerSelector
  , subscribeAttributeDischargingEnabledUntilWithParams_subscriptionEstablished_reportHandlerSelector
  , subscribeAttributeFaultStateWithParams_subscriptionEstablished_reportHandlerSelector
  , subscribeAttributeFeatureMapWithParams_subscriptionEstablished_reportHandlerSelector
  , subscribeAttributeGeneratedCommandListWithParams_subscriptionEstablished_reportHandlerSelector
  , subscribeAttributeMaximumChargeCurrentWithParams_subscriptionEstablished_reportHandlerSelector
  , subscribeAttributeMaximumDischargeCurrentWithParams_subscriptionEstablished_reportHandlerSelector
  , subscribeAttributeMinimumChargeCurrentWithParams_subscriptionEstablished_reportHandlerSelector
  , subscribeAttributeNextChargeRequiredEnergyWithParams_subscriptionEstablished_reportHandlerSelector
  , subscribeAttributeNextChargeStartTimeWithParams_subscriptionEstablished_reportHandlerSelector
  , subscribeAttributeNextChargeTargetSoCWithParams_subscriptionEstablished_reportHandlerSelector
  , subscribeAttributeNextChargeTargetTimeWithParams_subscriptionEstablished_reportHandlerSelector
  , subscribeAttributeRandomizationDelayWindowWithParams_subscriptionEstablished_reportHandlerSelector
  , subscribeAttributeSessionDurationWithParams_subscriptionEstablished_reportHandlerSelector
  , subscribeAttributeSessionEnergyChargedWithParams_subscriptionEstablished_reportHandlerSelector
  , subscribeAttributeSessionEnergyDischargedWithParams_subscriptionEstablished_reportHandlerSelector
  , subscribeAttributeSessionIDWithParams_subscriptionEstablished_reportHandlerSelector
  , subscribeAttributeStateOfChargeWithParams_subscriptionEstablished_reportHandlerSelector
  , subscribeAttributeStateWithParams_subscriptionEstablished_reportHandlerSelector
  , subscribeAttributeSupplyStateWithParams_subscriptionEstablished_reportHandlerSelector
  , subscribeAttributeUserMaximumChargeCurrentWithParams_subscriptionEstablished_reportHandlerSelector
  , subscribeAttributeVehicleIDWithParams_subscriptionEstablished_reportHandlerSelector
  , writeAttributeApproximateEVEfficiencyWithValue_completionSelector
  , writeAttributeApproximateEVEfficiencyWithValue_params_completionSelector
  , writeAttributeRandomizationDelayWindowWithValue_completionSelector
  , writeAttributeRandomizationDelayWindowWithValue_params_completionSelector
  , writeAttributeUserMaximumChargeCurrentWithValue_completionSelector
  , writeAttributeUserMaximumChargeCurrentWithValue_params_completionSelector


  ) where

import Foreign.Ptr (Ptr, FunPtr)
import Foreign.C.Types

import ObjC.Runtime.Types
import ObjC.Runtime.Message (sendMessage, sendOwnedMessage, sendClassMessage, sendOwnedClassMessage)
import ObjC.Runtime.Selector (mkSelector)
import ObjC.Runtime.Class (getRequiredClass)

import ObjC.Matter.Internal.Classes
import ObjC.Foundation.Internal.Classes

-- | Command Disable
--
-- Allows a client to disable the EVSE from charging and discharging.
--
-- ObjC selector: @- disableWithParams:completion:@
disableWithParams_completion :: (IsMTRBaseClusterEnergyEVSE mtrBaseClusterEnergyEVSE, IsMTREnergyEVSEClusterDisableParams params) => mtrBaseClusterEnergyEVSE -> params -> Ptr () -> IO ()
disableWithParams_completion mtrBaseClusterEnergyEVSE params completion =
  sendMessage mtrBaseClusterEnergyEVSE disableWithParams_completionSelector (toMTREnergyEVSEClusterDisableParams params) completion

-- | @- disableWithCompletion:@
disableWithCompletion :: IsMTRBaseClusterEnergyEVSE mtrBaseClusterEnergyEVSE => mtrBaseClusterEnergyEVSE -> Ptr () -> IO ()
disableWithCompletion mtrBaseClusterEnergyEVSE completion =
  sendMessage mtrBaseClusterEnergyEVSE disableWithCompletionSelector completion

-- | Command EnableCharging
--
-- This command allows a client to enable the EVSE to charge an EV, and to provide or update the maximum and minimum charge current.
--
-- ObjC selector: @- enableChargingWithParams:completion:@
enableChargingWithParams_completion :: (IsMTRBaseClusterEnergyEVSE mtrBaseClusterEnergyEVSE, IsMTREnergyEVSEClusterEnableChargingParams params) => mtrBaseClusterEnergyEVSE -> params -> Ptr () -> IO ()
enableChargingWithParams_completion mtrBaseClusterEnergyEVSE params completion =
  sendMessage mtrBaseClusterEnergyEVSE enableChargingWithParams_completionSelector (toMTREnergyEVSEClusterEnableChargingParams params) completion

-- | Command EnableDischarging
--
-- Upon receipt, this SHALL allow a client to enable the discharge of an EV, and to provide or update the maximum discharge current.
--
-- ObjC selector: @- enableDischargingWithParams:completion:@
enableDischargingWithParams_completion :: (IsMTRBaseClusterEnergyEVSE mtrBaseClusterEnergyEVSE, IsMTREnergyEVSEClusterEnableDischargingParams params) => mtrBaseClusterEnergyEVSE -> params -> Ptr () -> IO ()
enableDischargingWithParams_completion mtrBaseClusterEnergyEVSE params completion =
  sendMessage mtrBaseClusterEnergyEVSE enableDischargingWithParams_completionSelector (toMTREnergyEVSEClusterEnableDischargingParams params) completion

-- | Command StartDiagnostics
--
-- Allows a client to put the EVSE into a self-diagnostics mode.
--
-- ObjC selector: @- startDiagnosticsWithParams:completion:@
startDiagnosticsWithParams_completion :: (IsMTRBaseClusterEnergyEVSE mtrBaseClusterEnergyEVSE, IsMTREnergyEVSEClusterStartDiagnosticsParams params) => mtrBaseClusterEnergyEVSE -> params -> Ptr () -> IO ()
startDiagnosticsWithParams_completion mtrBaseClusterEnergyEVSE params completion =
  sendMessage mtrBaseClusterEnergyEVSE startDiagnosticsWithParams_completionSelector (toMTREnergyEVSEClusterStartDiagnosticsParams params) completion

-- | @- startDiagnosticsWithCompletion:@
startDiagnosticsWithCompletion :: IsMTRBaseClusterEnergyEVSE mtrBaseClusterEnergyEVSE => mtrBaseClusterEnergyEVSE -> Ptr () -> IO ()
startDiagnosticsWithCompletion mtrBaseClusterEnergyEVSE completion =
  sendMessage mtrBaseClusterEnergyEVSE startDiagnosticsWithCompletionSelector completion

-- | Command SetTargets
--
-- Allows a client to set the user specified charging targets.
--
-- ObjC selector: @- setTargetsWithParams:completion:@
setTargetsWithParams_completion :: (IsMTRBaseClusterEnergyEVSE mtrBaseClusterEnergyEVSE, IsMTREnergyEVSEClusterSetTargetsParams params) => mtrBaseClusterEnergyEVSE -> params -> Ptr () -> IO ()
setTargetsWithParams_completion mtrBaseClusterEnergyEVSE params completion =
  sendMessage mtrBaseClusterEnergyEVSE setTargetsWithParams_completionSelector (toMTREnergyEVSEClusterSetTargetsParams params) completion

-- | Command GetTargets
--
-- Allows a client to retrieve the current set of charging targets.
--
-- ObjC selector: @- getTargetsWithParams:completion:@
getTargetsWithParams_completion :: (IsMTRBaseClusterEnergyEVSE mtrBaseClusterEnergyEVSE, IsMTREnergyEVSEClusterGetTargetsParams params) => mtrBaseClusterEnergyEVSE -> params -> Ptr () -> IO ()
getTargetsWithParams_completion mtrBaseClusterEnergyEVSE params completion =
  sendMessage mtrBaseClusterEnergyEVSE getTargetsWithParams_completionSelector (toMTREnergyEVSEClusterGetTargetsParams params) completion

-- | @- getTargetsWithCompletion:@
getTargetsWithCompletion :: IsMTRBaseClusterEnergyEVSE mtrBaseClusterEnergyEVSE => mtrBaseClusterEnergyEVSE -> Ptr () -> IO ()
getTargetsWithCompletion mtrBaseClusterEnergyEVSE completion =
  sendMessage mtrBaseClusterEnergyEVSE getTargetsWithCompletionSelector completion

-- | Command ClearTargets
--
-- Allows a client to clear all stored charging targets.
--
-- ObjC selector: @- clearTargetsWithParams:completion:@
clearTargetsWithParams_completion :: (IsMTRBaseClusterEnergyEVSE mtrBaseClusterEnergyEVSE, IsMTREnergyEVSEClusterClearTargetsParams params) => mtrBaseClusterEnergyEVSE -> params -> Ptr () -> IO ()
clearTargetsWithParams_completion mtrBaseClusterEnergyEVSE params completion =
  sendMessage mtrBaseClusterEnergyEVSE clearTargetsWithParams_completionSelector (toMTREnergyEVSEClusterClearTargetsParams params) completion

-- | @- clearTargetsWithCompletion:@
clearTargetsWithCompletion :: IsMTRBaseClusterEnergyEVSE mtrBaseClusterEnergyEVSE => mtrBaseClusterEnergyEVSE -> Ptr () -> IO ()
clearTargetsWithCompletion mtrBaseClusterEnergyEVSE completion =
  sendMessage mtrBaseClusterEnergyEVSE clearTargetsWithCompletionSelector completion

-- | @- readAttributeStateWithCompletion:@
readAttributeStateWithCompletion :: IsMTRBaseClusterEnergyEVSE mtrBaseClusterEnergyEVSE => mtrBaseClusterEnergyEVSE -> Ptr () -> IO ()
readAttributeStateWithCompletion mtrBaseClusterEnergyEVSE completion =
  sendMessage mtrBaseClusterEnergyEVSE readAttributeStateWithCompletionSelector completion

-- | @- subscribeAttributeStateWithParams:subscriptionEstablished:reportHandler:@
subscribeAttributeStateWithParams_subscriptionEstablished_reportHandler :: (IsMTRBaseClusterEnergyEVSE mtrBaseClusterEnergyEVSE, IsMTRSubscribeParams params) => mtrBaseClusterEnergyEVSE -> params -> Ptr () -> Ptr () -> IO ()
subscribeAttributeStateWithParams_subscriptionEstablished_reportHandler mtrBaseClusterEnergyEVSE params subscriptionEstablished reportHandler =
  sendMessage mtrBaseClusterEnergyEVSE subscribeAttributeStateWithParams_subscriptionEstablished_reportHandlerSelector (toMTRSubscribeParams params) subscriptionEstablished reportHandler

-- | @+ readAttributeStateWithClusterStateCache:endpoint:queue:completion:@
readAttributeStateWithClusterStateCache_endpoint_queue_completion :: (IsMTRClusterStateCacheContainer clusterStateCacheContainer, IsNSNumber endpoint, IsNSObject queue) => clusterStateCacheContainer -> endpoint -> queue -> Ptr () -> IO ()
readAttributeStateWithClusterStateCache_endpoint_queue_completion clusterStateCacheContainer endpoint queue completion =
  do
    cls' <- getRequiredClass "MTRBaseClusterEnergyEVSE"
    sendClassMessage cls' readAttributeStateWithClusterStateCache_endpoint_queue_completionSelector (toMTRClusterStateCacheContainer clusterStateCacheContainer) (toNSNumber endpoint) (toNSObject queue) completion

-- | @- readAttributeSupplyStateWithCompletion:@
readAttributeSupplyStateWithCompletion :: IsMTRBaseClusterEnergyEVSE mtrBaseClusterEnergyEVSE => mtrBaseClusterEnergyEVSE -> Ptr () -> IO ()
readAttributeSupplyStateWithCompletion mtrBaseClusterEnergyEVSE completion =
  sendMessage mtrBaseClusterEnergyEVSE readAttributeSupplyStateWithCompletionSelector completion

-- | @- subscribeAttributeSupplyStateWithParams:subscriptionEstablished:reportHandler:@
subscribeAttributeSupplyStateWithParams_subscriptionEstablished_reportHandler :: (IsMTRBaseClusterEnergyEVSE mtrBaseClusterEnergyEVSE, IsMTRSubscribeParams params) => mtrBaseClusterEnergyEVSE -> params -> Ptr () -> Ptr () -> IO ()
subscribeAttributeSupplyStateWithParams_subscriptionEstablished_reportHandler mtrBaseClusterEnergyEVSE params subscriptionEstablished reportHandler =
  sendMessage mtrBaseClusterEnergyEVSE subscribeAttributeSupplyStateWithParams_subscriptionEstablished_reportHandlerSelector (toMTRSubscribeParams params) subscriptionEstablished reportHandler

-- | @+ readAttributeSupplyStateWithClusterStateCache:endpoint:queue:completion:@
readAttributeSupplyStateWithClusterStateCache_endpoint_queue_completion :: (IsMTRClusterStateCacheContainer clusterStateCacheContainer, IsNSNumber endpoint, IsNSObject queue) => clusterStateCacheContainer -> endpoint -> queue -> Ptr () -> IO ()
readAttributeSupplyStateWithClusterStateCache_endpoint_queue_completion clusterStateCacheContainer endpoint queue completion =
  do
    cls' <- getRequiredClass "MTRBaseClusterEnergyEVSE"
    sendClassMessage cls' readAttributeSupplyStateWithClusterStateCache_endpoint_queue_completionSelector (toMTRClusterStateCacheContainer clusterStateCacheContainer) (toNSNumber endpoint) (toNSObject queue) completion

-- | @- readAttributeFaultStateWithCompletion:@
readAttributeFaultStateWithCompletion :: IsMTRBaseClusterEnergyEVSE mtrBaseClusterEnergyEVSE => mtrBaseClusterEnergyEVSE -> Ptr () -> IO ()
readAttributeFaultStateWithCompletion mtrBaseClusterEnergyEVSE completion =
  sendMessage mtrBaseClusterEnergyEVSE readAttributeFaultStateWithCompletionSelector completion

-- | @- subscribeAttributeFaultStateWithParams:subscriptionEstablished:reportHandler:@
subscribeAttributeFaultStateWithParams_subscriptionEstablished_reportHandler :: (IsMTRBaseClusterEnergyEVSE mtrBaseClusterEnergyEVSE, IsMTRSubscribeParams params) => mtrBaseClusterEnergyEVSE -> params -> Ptr () -> Ptr () -> IO ()
subscribeAttributeFaultStateWithParams_subscriptionEstablished_reportHandler mtrBaseClusterEnergyEVSE params subscriptionEstablished reportHandler =
  sendMessage mtrBaseClusterEnergyEVSE subscribeAttributeFaultStateWithParams_subscriptionEstablished_reportHandlerSelector (toMTRSubscribeParams params) subscriptionEstablished reportHandler

-- | @+ readAttributeFaultStateWithClusterStateCache:endpoint:queue:completion:@
readAttributeFaultStateWithClusterStateCache_endpoint_queue_completion :: (IsMTRClusterStateCacheContainer clusterStateCacheContainer, IsNSNumber endpoint, IsNSObject queue) => clusterStateCacheContainer -> endpoint -> queue -> Ptr () -> IO ()
readAttributeFaultStateWithClusterStateCache_endpoint_queue_completion clusterStateCacheContainer endpoint queue completion =
  do
    cls' <- getRequiredClass "MTRBaseClusterEnergyEVSE"
    sendClassMessage cls' readAttributeFaultStateWithClusterStateCache_endpoint_queue_completionSelector (toMTRClusterStateCacheContainer clusterStateCacheContainer) (toNSNumber endpoint) (toNSObject queue) completion

-- | @- readAttributeChargingEnabledUntilWithCompletion:@
readAttributeChargingEnabledUntilWithCompletion :: IsMTRBaseClusterEnergyEVSE mtrBaseClusterEnergyEVSE => mtrBaseClusterEnergyEVSE -> Ptr () -> IO ()
readAttributeChargingEnabledUntilWithCompletion mtrBaseClusterEnergyEVSE completion =
  sendMessage mtrBaseClusterEnergyEVSE readAttributeChargingEnabledUntilWithCompletionSelector completion

-- | @- subscribeAttributeChargingEnabledUntilWithParams:subscriptionEstablished:reportHandler:@
subscribeAttributeChargingEnabledUntilWithParams_subscriptionEstablished_reportHandler :: (IsMTRBaseClusterEnergyEVSE mtrBaseClusterEnergyEVSE, IsMTRSubscribeParams params) => mtrBaseClusterEnergyEVSE -> params -> Ptr () -> Ptr () -> IO ()
subscribeAttributeChargingEnabledUntilWithParams_subscriptionEstablished_reportHandler mtrBaseClusterEnergyEVSE params subscriptionEstablished reportHandler =
  sendMessage mtrBaseClusterEnergyEVSE subscribeAttributeChargingEnabledUntilWithParams_subscriptionEstablished_reportHandlerSelector (toMTRSubscribeParams params) subscriptionEstablished reportHandler

-- | @+ readAttributeChargingEnabledUntilWithClusterStateCache:endpoint:queue:completion:@
readAttributeChargingEnabledUntilWithClusterStateCache_endpoint_queue_completion :: (IsMTRClusterStateCacheContainer clusterStateCacheContainer, IsNSNumber endpoint, IsNSObject queue) => clusterStateCacheContainer -> endpoint -> queue -> Ptr () -> IO ()
readAttributeChargingEnabledUntilWithClusterStateCache_endpoint_queue_completion clusterStateCacheContainer endpoint queue completion =
  do
    cls' <- getRequiredClass "MTRBaseClusterEnergyEVSE"
    sendClassMessage cls' readAttributeChargingEnabledUntilWithClusterStateCache_endpoint_queue_completionSelector (toMTRClusterStateCacheContainer clusterStateCacheContainer) (toNSNumber endpoint) (toNSObject queue) completion

-- | @- readAttributeDischargingEnabledUntilWithCompletion:@
readAttributeDischargingEnabledUntilWithCompletion :: IsMTRBaseClusterEnergyEVSE mtrBaseClusterEnergyEVSE => mtrBaseClusterEnergyEVSE -> Ptr () -> IO ()
readAttributeDischargingEnabledUntilWithCompletion mtrBaseClusterEnergyEVSE completion =
  sendMessage mtrBaseClusterEnergyEVSE readAttributeDischargingEnabledUntilWithCompletionSelector completion

-- | @- subscribeAttributeDischargingEnabledUntilWithParams:subscriptionEstablished:reportHandler:@
subscribeAttributeDischargingEnabledUntilWithParams_subscriptionEstablished_reportHandler :: (IsMTRBaseClusterEnergyEVSE mtrBaseClusterEnergyEVSE, IsMTRSubscribeParams params) => mtrBaseClusterEnergyEVSE -> params -> Ptr () -> Ptr () -> IO ()
subscribeAttributeDischargingEnabledUntilWithParams_subscriptionEstablished_reportHandler mtrBaseClusterEnergyEVSE params subscriptionEstablished reportHandler =
  sendMessage mtrBaseClusterEnergyEVSE subscribeAttributeDischargingEnabledUntilWithParams_subscriptionEstablished_reportHandlerSelector (toMTRSubscribeParams params) subscriptionEstablished reportHandler

-- | @+ readAttributeDischargingEnabledUntilWithClusterStateCache:endpoint:queue:completion:@
readAttributeDischargingEnabledUntilWithClusterStateCache_endpoint_queue_completion :: (IsMTRClusterStateCacheContainer clusterStateCacheContainer, IsNSNumber endpoint, IsNSObject queue) => clusterStateCacheContainer -> endpoint -> queue -> Ptr () -> IO ()
readAttributeDischargingEnabledUntilWithClusterStateCache_endpoint_queue_completion clusterStateCacheContainer endpoint queue completion =
  do
    cls' <- getRequiredClass "MTRBaseClusterEnergyEVSE"
    sendClassMessage cls' readAttributeDischargingEnabledUntilWithClusterStateCache_endpoint_queue_completionSelector (toMTRClusterStateCacheContainer clusterStateCacheContainer) (toNSNumber endpoint) (toNSObject queue) completion

-- | @- readAttributeCircuitCapacityWithCompletion:@
readAttributeCircuitCapacityWithCompletion :: IsMTRBaseClusterEnergyEVSE mtrBaseClusterEnergyEVSE => mtrBaseClusterEnergyEVSE -> Ptr () -> IO ()
readAttributeCircuitCapacityWithCompletion mtrBaseClusterEnergyEVSE completion =
  sendMessage mtrBaseClusterEnergyEVSE readAttributeCircuitCapacityWithCompletionSelector completion

-- | @- subscribeAttributeCircuitCapacityWithParams:subscriptionEstablished:reportHandler:@
subscribeAttributeCircuitCapacityWithParams_subscriptionEstablished_reportHandler :: (IsMTRBaseClusterEnergyEVSE mtrBaseClusterEnergyEVSE, IsMTRSubscribeParams params) => mtrBaseClusterEnergyEVSE -> params -> Ptr () -> Ptr () -> IO ()
subscribeAttributeCircuitCapacityWithParams_subscriptionEstablished_reportHandler mtrBaseClusterEnergyEVSE params subscriptionEstablished reportHandler =
  sendMessage mtrBaseClusterEnergyEVSE subscribeAttributeCircuitCapacityWithParams_subscriptionEstablished_reportHandlerSelector (toMTRSubscribeParams params) subscriptionEstablished reportHandler

-- | @+ readAttributeCircuitCapacityWithClusterStateCache:endpoint:queue:completion:@
readAttributeCircuitCapacityWithClusterStateCache_endpoint_queue_completion :: (IsMTRClusterStateCacheContainer clusterStateCacheContainer, IsNSNumber endpoint, IsNSObject queue) => clusterStateCacheContainer -> endpoint -> queue -> Ptr () -> IO ()
readAttributeCircuitCapacityWithClusterStateCache_endpoint_queue_completion clusterStateCacheContainer endpoint queue completion =
  do
    cls' <- getRequiredClass "MTRBaseClusterEnergyEVSE"
    sendClassMessage cls' readAttributeCircuitCapacityWithClusterStateCache_endpoint_queue_completionSelector (toMTRClusterStateCacheContainer clusterStateCacheContainer) (toNSNumber endpoint) (toNSObject queue) completion

-- | @- readAttributeMinimumChargeCurrentWithCompletion:@
readAttributeMinimumChargeCurrentWithCompletion :: IsMTRBaseClusterEnergyEVSE mtrBaseClusterEnergyEVSE => mtrBaseClusterEnergyEVSE -> Ptr () -> IO ()
readAttributeMinimumChargeCurrentWithCompletion mtrBaseClusterEnergyEVSE completion =
  sendMessage mtrBaseClusterEnergyEVSE readAttributeMinimumChargeCurrentWithCompletionSelector completion

-- | @- subscribeAttributeMinimumChargeCurrentWithParams:subscriptionEstablished:reportHandler:@
subscribeAttributeMinimumChargeCurrentWithParams_subscriptionEstablished_reportHandler :: (IsMTRBaseClusterEnergyEVSE mtrBaseClusterEnergyEVSE, IsMTRSubscribeParams params) => mtrBaseClusterEnergyEVSE -> params -> Ptr () -> Ptr () -> IO ()
subscribeAttributeMinimumChargeCurrentWithParams_subscriptionEstablished_reportHandler mtrBaseClusterEnergyEVSE params subscriptionEstablished reportHandler =
  sendMessage mtrBaseClusterEnergyEVSE subscribeAttributeMinimumChargeCurrentWithParams_subscriptionEstablished_reportHandlerSelector (toMTRSubscribeParams params) subscriptionEstablished reportHandler

-- | @+ readAttributeMinimumChargeCurrentWithClusterStateCache:endpoint:queue:completion:@
readAttributeMinimumChargeCurrentWithClusterStateCache_endpoint_queue_completion :: (IsMTRClusterStateCacheContainer clusterStateCacheContainer, IsNSNumber endpoint, IsNSObject queue) => clusterStateCacheContainer -> endpoint -> queue -> Ptr () -> IO ()
readAttributeMinimumChargeCurrentWithClusterStateCache_endpoint_queue_completion clusterStateCacheContainer endpoint queue completion =
  do
    cls' <- getRequiredClass "MTRBaseClusterEnergyEVSE"
    sendClassMessage cls' readAttributeMinimumChargeCurrentWithClusterStateCache_endpoint_queue_completionSelector (toMTRClusterStateCacheContainer clusterStateCacheContainer) (toNSNumber endpoint) (toNSObject queue) completion

-- | @- readAttributeMaximumChargeCurrentWithCompletion:@
readAttributeMaximumChargeCurrentWithCompletion :: IsMTRBaseClusterEnergyEVSE mtrBaseClusterEnergyEVSE => mtrBaseClusterEnergyEVSE -> Ptr () -> IO ()
readAttributeMaximumChargeCurrentWithCompletion mtrBaseClusterEnergyEVSE completion =
  sendMessage mtrBaseClusterEnergyEVSE readAttributeMaximumChargeCurrentWithCompletionSelector completion

-- | @- subscribeAttributeMaximumChargeCurrentWithParams:subscriptionEstablished:reportHandler:@
subscribeAttributeMaximumChargeCurrentWithParams_subscriptionEstablished_reportHandler :: (IsMTRBaseClusterEnergyEVSE mtrBaseClusterEnergyEVSE, IsMTRSubscribeParams params) => mtrBaseClusterEnergyEVSE -> params -> Ptr () -> Ptr () -> IO ()
subscribeAttributeMaximumChargeCurrentWithParams_subscriptionEstablished_reportHandler mtrBaseClusterEnergyEVSE params subscriptionEstablished reportHandler =
  sendMessage mtrBaseClusterEnergyEVSE subscribeAttributeMaximumChargeCurrentWithParams_subscriptionEstablished_reportHandlerSelector (toMTRSubscribeParams params) subscriptionEstablished reportHandler

-- | @+ readAttributeMaximumChargeCurrentWithClusterStateCache:endpoint:queue:completion:@
readAttributeMaximumChargeCurrentWithClusterStateCache_endpoint_queue_completion :: (IsMTRClusterStateCacheContainer clusterStateCacheContainer, IsNSNumber endpoint, IsNSObject queue) => clusterStateCacheContainer -> endpoint -> queue -> Ptr () -> IO ()
readAttributeMaximumChargeCurrentWithClusterStateCache_endpoint_queue_completion clusterStateCacheContainer endpoint queue completion =
  do
    cls' <- getRequiredClass "MTRBaseClusterEnergyEVSE"
    sendClassMessage cls' readAttributeMaximumChargeCurrentWithClusterStateCache_endpoint_queue_completionSelector (toMTRClusterStateCacheContainer clusterStateCacheContainer) (toNSNumber endpoint) (toNSObject queue) completion

-- | @- readAttributeMaximumDischargeCurrentWithCompletion:@
readAttributeMaximumDischargeCurrentWithCompletion :: IsMTRBaseClusterEnergyEVSE mtrBaseClusterEnergyEVSE => mtrBaseClusterEnergyEVSE -> Ptr () -> IO ()
readAttributeMaximumDischargeCurrentWithCompletion mtrBaseClusterEnergyEVSE completion =
  sendMessage mtrBaseClusterEnergyEVSE readAttributeMaximumDischargeCurrentWithCompletionSelector completion

-- | @- subscribeAttributeMaximumDischargeCurrentWithParams:subscriptionEstablished:reportHandler:@
subscribeAttributeMaximumDischargeCurrentWithParams_subscriptionEstablished_reportHandler :: (IsMTRBaseClusterEnergyEVSE mtrBaseClusterEnergyEVSE, IsMTRSubscribeParams params) => mtrBaseClusterEnergyEVSE -> params -> Ptr () -> Ptr () -> IO ()
subscribeAttributeMaximumDischargeCurrentWithParams_subscriptionEstablished_reportHandler mtrBaseClusterEnergyEVSE params subscriptionEstablished reportHandler =
  sendMessage mtrBaseClusterEnergyEVSE subscribeAttributeMaximumDischargeCurrentWithParams_subscriptionEstablished_reportHandlerSelector (toMTRSubscribeParams params) subscriptionEstablished reportHandler

-- | @+ readAttributeMaximumDischargeCurrentWithClusterStateCache:endpoint:queue:completion:@
readAttributeMaximumDischargeCurrentWithClusterStateCache_endpoint_queue_completion :: (IsMTRClusterStateCacheContainer clusterStateCacheContainer, IsNSNumber endpoint, IsNSObject queue) => clusterStateCacheContainer -> endpoint -> queue -> Ptr () -> IO ()
readAttributeMaximumDischargeCurrentWithClusterStateCache_endpoint_queue_completion clusterStateCacheContainer endpoint queue completion =
  do
    cls' <- getRequiredClass "MTRBaseClusterEnergyEVSE"
    sendClassMessage cls' readAttributeMaximumDischargeCurrentWithClusterStateCache_endpoint_queue_completionSelector (toMTRClusterStateCacheContainer clusterStateCacheContainer) (toNSNumber endpoint) (toNSObject queue) completion

-- | @- readAttributeUserMaximumChargeCurrentWithCompletion:@
readAttributeUserMaximumChargeCurrentWithCompletion :: IsMTRBaseClusterEnergyEVSE mtrBaseClusterEnergyEVSE => mtrBaseClusterEnergyEVSE -> Ptr () -> IO ()
readAttributeUserMaximumChargeCurrentWithCompletion mtrBaseClusterEnergyEVSE completion =
  sendMessage mtrBaseClusterEnergyEVSE readAttributeUserMaximumChargeCurrentWithCompletionSelector completion

-- | @- writeAttributeUserMaximumChargeCurrentWithValue:completion:@
writeAttributeUserMaximumChargeCurrentWithValue_completion :: (IsMTRBaseClusterEnergyEVSE mtrBaseClusterEnergyEVSE, IsNSNumber value) => mtrBaseClusterEnergyEVSE -> value -> Ptr () -> IO ()
writeAttributeUserMaximumChargeCurrentWithValue_completion mtrBaseClusterEnergyEVSE value completion =
  sendMessage mtrBaseClusterEnergyEVSE writeAttributeUserMaximumChargeCurrentWithValue_completionSelector (toNSNumber value) completion

-- | @- writeAttributeUserMaximumChargeCurrentWithValue:params:completion:@
writeAttributeUserMaximumChargeCurrentWithValue_params_completion :: (IsMTRBaseClusterEnergyEVSE mtrBaseClusterEnergyEVSE, IsNSNumber value, IsMTRWriteParams params) => mtrBaseClusterEnergyEVSE -> value -> params -> Ptr () -> IO ()
writeAttributeUserMaximumChargeCurrentWithValue_params_completion mtrBaseClusterEnergyEVSE value params completion =
  sendMessage mtrBaseClusterEnergyEVSE writeAttributeUserMaximumChargeCurrentWithValue_params_completionSelector (toNSNumber value) (toMTRWriteParams params) completion

-- | @- subscribeAttributeUserMaximumChargeCurrentWithParams:subscriptionEstablished:reportHandler:@
subscribeAttributeUserMaximumChargeCurrentWithParams_subscriptionEstablished_reportHandler :: (IsMTRBaseClusterEnergyEVSE mtrBaseClusterEnergyEVSE, IsMTRSubscribeParams params) => mtrBaseClusterEnergyEVSE -> params -> Ptr () -> Ptr () -> IO ()
subscribeAttributeUserMaximumChargeCurrentWithParams_subscriptionEstablished_reportHandler mtrBaseClusterEnergyEVSE params subscriptionEstablished reportHandler =
  sendMessage mtrBaseClusterEnergyEVSE subscribeAttributeUserMaximumChargeCurrentWithParams_subscriptionEstablished_reportHandlerSelector (toMTRSubscribeParams params) subscriptionEstablished reportHandler

-- | @+ readAttributeUserMaximumChargeCurrentWithClusterStateCache:endpoint:queue:completion:@
readAttributeUserMaximumChargeCurrentWithClusterStateCache_endpoint_queue_completion :: (IsMTRClusterStateCacheContainer clusterStateCacheContainer, IsNSNumber endpoint, IsNSObject queue) => clusterStateCacheContainer -> endpoint -> queue -> Ptr () -> IO ()
readAttributeUserMaximumChargeCurrentWithClusterStateCache_endpoint_queue_completion clusterStateCacheContainer endpoint queue completion =
  do
    cls' <- getRequiredClass "MTRBaseClusterEnergyEVSE"
    sendClassMessage cls' readAttributeUserMaximumChargeCurrentWithClusterStateCache_endpoint_queue_completionSelector (toMTRClusterStateCacheContainer clusterStateCacheContainer) (toNSNumber endpoint) (toNSObject queue) completion

-- | @- readAttributeRandomizationDelayWindowWithCompletion:@
readAttributeRandomizationDelayWindowWithCompletion :: IsMTRBaseClusterEnergyEVSE mtrBaseClusterEnergyEVSE => mtrBaseClusterEnergyEVSE -> Ptr () -> IO ()
readAttributeRandomizationDelayWindowWithCompletion mtrBaseClusterEnergyEVSE completion =
  sendMessage mtrBaseClusterEnergyEVSE readAttributeRandomizationDelayWindowWithCompletionSelector completion

-- | @- writeAttributeRandomizationDelayWindowWithValue:completion:@
writeAttributeRandomizationDelayWindowWithValue_completion :: (IsMTRBaseClusterEnergyEVSE mtrBaseClusterEnergyEVSE, IsNSNumber value) => mtrBaseClusterEnergyEVSE -> value -> Ptr () -> IO ()
writeAttributeRandomizationDelayWindowWithValue_completion mtrBaseClusterEnergyEVSE value completion =
  sendMessage mtrBaseClusterEnergyEVSE writeAttributeRandomizationDelayWindowWithValue_completionSelector (toNSNumber value) completion

-- | @- writeAttributeRandomizationDelayWindowWithValue:params:completion:@
writeAttributeRandomizationDelayWindowWithValue_params_completion :: (IsMTRBaseClusterEnergyEVSE mtrBaseClusterEnergyEVSE, IsNSNumber value, IsMTRWriteParams params) => mtrBaseClusterEnergyEVSE -> value -> params -> Ptr () -> IO ()
writeAttributeRandomizationDelayWindowWithValue_params_completion mtrBaseClusterEnergyEVSE value params completion =
  sendMessage mtrBaseClusterEnergyEVSE writeAttributeRandomizationDelayWindowWithValue_params_completionSelector (toNSNumber value) (toMTRWriteParams params) completion

-- | @- subscribeAttributeRandomizationDelayWindowWithParams:subscriptionEstablished:reportHandler:@
subscribeAttributeRandomizationDelayWindowWithParams_subscriptionEstablished_reportHandler :: (IsMTRBaseClusterEnergyEVSE mtrBaseClusterEnergyEVSE, IsMTRSubscribeParams params) => mtrBaseClusterEnergyEVSE -> params -> Ptr () -> Ptr () -> IO ()
subscribeAttributeRandomizationDelayWindowWithParams_subscriptionEstablished_reportHandler mtrBaseClusterEnergyEVSE params subscriptionEstablished reportHandler =
  sendMessage mtrBaseClusterEnergyEVSE subscribeAttributeRandomizationDelayWindowWithParams_subscriptionEstablished_reportHandlerSelector (toMTRSubscribeParams params) subscriptionEstablished reportHandler

-- | @+ readAttributeRandomizationDelayWindowWithClusterStateCache:endpoint:queue:completion:@
readAttributeRandomizationDelayWindowWithClusterStateCache_endpoint_queue_completion :: (IsMTRClusterStateCacheContainer clusterStateCacheContainer, IsNSNumber endpoint, IsNSObject queue) => clusterStateCacheContainer -> endpoint -> queue -> Ptr () -> IO ()
readAttributeRandomizationDelayWindowWithClusterStateCache_endpoint_queue_completion clusterStateCacheContainer endpoint queue completion =
  do
    cls' <- getRequiredClass "MTRBaseClusterEnergyEVSE"
    sendClassMessage cls' readAttributeRandomizationDelayWindowWithClusterStateCache_endpoint_queue_completionSelector (toMTRClusterStateCacheContainer clusterStateCacheContainer) (toNSNumber endpoint) (toNSObject queue) completion

-- | @- readAttributeNextChargeStartTimeWithCompletion:@
readAttributeNextChargeStartTimeWithCompletion :: IsMTRBaseClusterEnergyEVSE mtrBaseClusterEnergyEVSE => mtrBaseClusterEnergyEVSE -> Ptr () -> IO ()
readAttributeNextChargeStartTimeWithCompletion mtrBaseClusterEnergyEVSE completion =
  sendMessage mtrBaseClusterEnergyEVSE readAttributeNextChargeStartTimeWithCompletionSelector completion

-- | @- subscribeAttributeNextChargeStartTimeWithParams:subscriptionEstablished:reportHandler:@
subscribeAttributeNextChargeStartTimeWithParams_subscriptionEstablished_reportHandler :: (IsMTRBaseClusterEnergyEVSE mtrBaseClusterEnergyEVSE, IsMTRSubscribeParams params) => mtrBaseClusterEnergyEVSE -> params -> Ptr () -> Ptr () -> IO ()
subscribeAttributeNextChargeStartTimeWithParams_subscriptionEstablished_reportHandler mtrBaseClusterEnergyEVSE params subscriptionEstablished reportHandler =
  sendMessage mtrBaseClusterEnergyEVSE subscribeAttributeNextChargeStartTimeWithParams_subscriptionEstablished_reportHandlerSelector (toMTRSubscribeParams params) subscriptionEstablished reportHandler

-- | @+ readAttributeNextChargeStartTimeWithClusterStateCache:endpoint:queue:completion:@
readAttributeNextChargeStartTimeWithClusterStateCache_endpoint_queue_completion :: (IsMTRClusterStateCacheContainer clusterStateCacheContainer, IsNSNumber endpoint, IsNSObject queue) => clusterStateCacheContainer -> endpoint -> queue -> Ptr () -> IO ()
readAttributeNextChargeStartTimeWithClusterStateCache_endpoint_queue_completion clusterStateCacheContainer endpoint queue completion =
  do
    cls' <- getRequiredClass "MTRBaseClusterEnergyEVSE"
    sendClassMessage cls' readAttributeNextChargeStartTimeWithClusterStateCache_endpoint_queue_completionSelector (toMTRClusterStateCacheContainer clusterStateCacheContainer) (toNSNumber endpoint) (toNSObject queue) completion

-- | @- readAttributeNextChargeTargetTimeWithCompletion:@
readAttributeNextChargeTargetTimeWithCompletion :: IsMTRBaseClusterEnergyEVSE mtrBaseClusterEnergyEVSE => mtrBaseClusterEnergyEVSE -> Ptr () -> IO ()
readAttributeNextChargeTargetTimeWithCompletion mtrBaseClusterEnergyEVSE completion =
  sendMessage mtrBaseClusterEnergyEVSE readAttributeNextChargeTargetTimeWithCompletionSelector completion

-- | @- subscribeAttributeNextChargeTargetTimeWithParams:subscriptionEstablished:reportHandler:@
subscribeAttributeNextChargeTargetTimeWithParams_subscriptionEstablished_reportHandler :: (IsMTRBaseClusterEnergyEVSE mtrBaseClusterEnergyEVSE, IsMTRSubscribeParams params) => mtrBaseClusterEnergyEVSE -> params -> Ptr () -> Ptr () -> IO ()
subscribeAttributeNextChargeTargetTimeWithParams_subscriptionEstablished_reportHandler mtrBaseClusterEnergyEVSE params subscriptionEstablished reportHandler =
  sendMessage mtrBaseClusterEnergyEVSE subscribeAttributeNextChargeTargetTimeWithParams_subscriptionEstablished_reportHandlerSelector (toMTRSubscribeParams params) subscriptionEstablished reportHandler

-- | @+ readAttributeNextChargeTargetTimeWithClusterStateCache:endpoint:queue:completion:@
readAttributeNextChargeTargetTimeWithClusterStateCache_endpoint_queue_completion :: (IsMTRClusterStateCacheContainer clusterStateCacheContainer, IsNSNumber endpoint, IsNSObject queue) => clusterStateCacheContainer -> endpoint -> queue -> Ptr () -> IO ()
readAttributeNextChargeTargetTimeWithClusterStateCache_endpoint_queue_completion clusterStateCacheContainer endpoint queue completion =
  do
    cls' <- getRequiredClass "MTRBaseClusterEnergyEVSE"
    sendClassMessage cls' readAttributeNextChargeTargetTimeWithClusterStateCache_endpoint_queue_completionSelector (toMTRClusterStateCacheContainer clusterStateCacheContainer) (toNSNumber endpoint) (toNSObject queue) completion

-- | @- readAttributeNextChargeRequiredEnergyWithCompletion:@
readAttributeNextChargeRequiredEnergyWithCompletion :: IsMTRBaseClusterEnergyEVSE mtrBaseClusterEnergyEVSE => mtrBaseClusterEnergyEVSE -> Ptr () -> IO ()
readAttributeNextChargeRequiredEnergyWithCompletion mtrBaseClusterEnergyEVSE completion =
  sendMessage mtrBaseClusterEnergyEVSE readAttributeNextChargeRequiredEnergyWithCompletionSelector completion

-- | @- subscribeAttributeNextChargeRequiredEnergyWithParams:subscriptionEstablished:reportHandler:@
subscribeAttributeNextChargeRequiredEnergyWithParams_subscriptionEstablished_reportHandler :: (IsMTRBaseClusterEnergyEVSE mtrBaseClusterEnergyEVSE, IsMTRSubscribeParams params) => mtrBaseClusterEnergyEVSE -> params -> Ptr () -> Ptr () -> IO ()
subscribeAttributeNextChargeRequiredEnergyWithParams_subscriptionEstablished_reportHandler mtrBaseClusterEnergyEVSE params subscriptionEstablished reportHandler =
  sendMessage mtrBaseClusterEnergyEVSE subscribeAttributeNextChargeRequiredEnergyWithParams_subscriptionEstablished_reportHandlerSelector (toMTRSubscribeParams params) subscriptionEstablished reportHandler

-- | @+ readAttributeNextChargeRequiredEnergyWithClusterStateCache:endpoint:queue:completion:@
readAttributeNextChargeRequiredEnergyWithClusterStateCache_endpoint_queue_completion :: (IsMTRClusterStateCacheContainer clusterStateCacheContainer, IsNSNumber endpoint, IsNSObject queue) => clusterStateCacheContainer -> endpoint -> queue -> Ptr () -> IO ()
readAttributeNextChargeRequiredEnergyWithClusterStateCache_endpoint_queue_completion clusterStateCacheContainer endpoint queue completion =
  do
    cls' <- getRequiredClass "MTRBaseClusterEnergyEVSE"
    sendClassMessage cls' readAttributeNextChargeRequiredEnergyWithClusterStateCache_endpoint_queue_completionSelector (toMTRClusterStateCacheContainer clusterStateCacheContainer) (toNSNumber endpoint) (toNSObject queue) completion

-- | @- readAttributeNextChargeTargetSoCWithCompletion:@
readAttributeNextChargeTargetSoCWithCompletion :: IsMTRBaseClusterEnergyEVSE mtrBaseClusterEnergyEVSE => mtrBaseClusterEnergyEVSE -> Ptr () -> IO ()
readAttributeNextChargeTargetSoCWithCompletion mtrBaseClusterEnergyEVSE completion =
  sendMessage mtrBaseClusterEnergyEVSE readAttributeNextChargeTargetSoCWithCompletionSelector completion

-- | @- subscribeAttributeNextChargeTargetSoCWithParams:subscriptionEstablished:reportHandler:@
subscribeAttributeNextChargeTargetSoCWithParams_subscriptionEstablished_reportHandler :: (IsMTRBaseClusterEnergyEVSE mtrBaseClusterEnergyEVSE, IsMTRSubscribeParams params) => mtrBaseClusterEnergyEVSE -> params -> Ptr () -> Ptr () -> IO ()
subscribeAttributeNextChargeTargetSoCWithParams_subscriptionEstablished_reportHandler mtrBaseClusterEnergyEVSE params subscriptionEstablished reportHandler =
  sendMessage mtrBaseClusterEnergyEVSE subscribeAttributeNextChargeTargetSoCWithParams_subscriptionEstablished_reportHandlerSelector (toMTRSubscribeParams params) subscriptionEstablished reportHandler

-- | @+ readAttributeNextChargeTargetSoCWithClusterStateCache:endpoint:queue:completion:@
readAttributeNextChargeTargetSoCWithClusterStateCache_endpoint_queue_completion :: (IsMTRClusterStateCacheContainer clusterStateCacheContainer, IsNSNumber endpoint, IsNSObject queue) => clusterStateCacheContainer -> endpoint -> queue -> Ptr () -> IO ()
readAttributeNextChargeTargetSoCWithClusterStateCache_endpoint_queue_completion clusterStateCacheContainer endpoint queue completion =
  do
    cls' <- getRequiredClass "MTRBaseClusterEnergyEVSE"
    sendClassMessage cls' readAttributeNextChargeTargetSoCWithClusterStateCache_endpoint_queue_completionSelector (toMTRClusterStateCacheContainer clusterStateCacheContainer) (toNSNumber endpoint) (toNSObject queue) completion

-- | @- readAttributeApproximateEVEfficiencyWithCompletion:@
readAttributeApproximateEVEfficiencyWithCompletion :: IsMTRBaseClusterEnergyEVSE mtrBaseClusterEnergyEVSE => mtrBaseClusterEnergyEVSE -> Ptr () -> IO ()
readAttributeApproximateEVEfficiencyWithCompletion mtrBaseClusterEnergyEVSE completion =
  sendMessage mtrBaseClusterEnergyEVSE readAttributeApproximateEVEfficiencyWithCompletionSelector completion

-- | @- writeAttributeApproximateEVEfficiencyWithValue:completion:@
writeAttributeApproximateEVEfficiencyWithValue_completion :: (IsMTRBaseClusterEnergyEVSE mtrBaseClusterEnergyEVSE, IsNSNumber value) => mtrBaseClusterEnergyEVSE -> value -> Ptr () -> IO ()
writeAttributeApproximateEVEfficiencyWithValue_completion mtrBaseClusterEnergyEVSE value completion =
  sendMessage mtrBaseClusterEnergyEVSE writeAttributeApproximateEVEfficiencyWithValue_completionSelector (toNSNumber value) completion

-- | @- writeAttributeApproximateEVEfficiencyWithValue:params:completion:@
writeAttributeApproximateEVEfficiencyWithValue_params_completion :: (IsMTRBaseClusterEnergyEVSE mtrBaseClusterEnergyEVSE, IsNSNumber value, IsMTRWriteParams params) => mtrBaseClusterEnergyEVSE -> value -> params -> Ptr () -> IO ()
writeAttributeApproximateEVEfficiencyWithValue_params_completion mtrBaseClusterEnergyEVSE value params completion =
  sendMessage mtrBaseClusterEnergyEVSE writeAttributeApproximateEVEfficiencyWithValue_params_completionSelector (toNSNumber value) (toMTRWriteParams params) completion

-- | @- subscribeAttributeApproximateEVEfficiencyWithParams:subscriptionEstablished:reportHandler:@
subscribeAttributeApproximateEVEfficiencyWithParams_subscriptionEstablished_reportHandler :: (IsMTRBaseClusterEnergyEVSE mtrBaseClusterEnergyEVSE, IsMTRSubscribeParams params) => mtrBaseClusterEnergyEVSE -> params -> Ptr () -> Ptr () -> IO ()
subscribeAttributeApproximateEVEfficiencyWithParams_subscriptionEstablished_reportHandler mtrBaseClusterEnergyEVSE params subscriptionEstablished reportHandler =
  sendMessage mtrBaseClusterEnergyEVSE subscribeAttributeApproximateEVEfficiencyWithParams_subscriptionEstablished_reportHandlerSelector (toMTRSubscribeParams params) subscriptionEstablished reportHandler

-- | @+ readAttributeApproximateEVEfficiencyWithClusterStateCache:endpoint:queue:completion:@
readAttributeApproximateEVEfficiencyWithClusterStateCache_endpoint_queue_completion :: (IsMTRClusterStateCacheContainer clusterStateCacheContainer, IsNSNumber endpoint, IsNSObject queue) => clusterStateCacheContainer -> endpoint -> queue -> Ptr () -> IO ()
readAttributeApproximateEVEfficiencyWithClusterStateCache_endpoint_queue_completion clusterStateCacheContainer endpoint queue completion =
  do
    cls' <- getRequiredClass "MTRBaseClusterEnergyEVSE"
    sendClassMessage cls' readAttributeApproximateEVEfficiencyWithClusterStateCache_endpoint_queue_completionSelector (toMTRClusterStateCacheContainer clusterStateCacheContainer) (toNSNumber endpoint) (toNSObject queue) completion

-- | @- readAttributeStateOfChargeWithCompletion:@
readAttributeStateOfChargeWithCompletion :: IsMTRBaseClusterEnergyEVSE mtrBaseClusterEnergyEVSE => mtrBaseClusterEnergyEVSE -> Ptr () -> IO ()
readAttributeStateOfChargeWithCompletion mtrBaseClusterEnergyEVSE completion =
  sendMessage mtrBaseClusterEnergyEVSE readAttributeStateOfChargeWithCompletionSelector completion

-- | @- subscribeAttributeStateOfChargeWithParams:subscriptionEstablished:reportHandler:@
subscribeAttributeStateOfChargeWithParams_subscriptionEstablished_reportHandler :: (IsMTRBaseClusterEnergyEVSE mtrBaseClusterEnergyEVSE, IsMTRSubscribeParams params) => mtrBaseClusterEnergyEVSE -> params -> Ptr () -> Ptr () -> IO ()
subscribeAttributeStateOfChargeWithParams_subscriptionEstablished_reportHandler mtrBaseClusterEnergyEVSE params subscriptionEstablished reportHandler =
  sendMessage mtrBaseClusterEnergyEVSE subscribeAttributeStateOfChargeWithParams_subscriptionEstablished_reportHandlerSelector (toMTRSubscribeParams params) subscriptionEstablished reportHandler

-- | @+ readAttributeStateOfChargeWithClusterStateCache:endpoint:queue:completion:@
readAttributeStateOfChargeWithClusterStateCache_endpoint_queue_completion :: (IsMTRClusterStateCacheContainer clusterStateCacheContainer, IsNSNumber endpoint, IsNSObject queue) => clusterStateCacheContainer -> endpoint -> queue -> Ptr () -> IO ()
readAttributeStateOfChargeWithClusterStateCache_endpoint_queue_completion clusterStateCacheContainer endpoint queue completion =
  do
    cls' <- getRequiredClass "MTRBaseClusterEnergyEVSE"
    sendClassMessage cls' readAttributeStateOfChargeWithClusterStateCache_endpoint_queue_completionSelector (toMTRClusterStateCacheContainer clusterStateCacheContainer) (toNSNumber endpoint) (toNSObject queue) completion

-- | @- readAttributeBatteryCapacityWithCompletion:@
readAttributeBatteryCapacityWithCompletion :: IsMTRBaseClusterEnergyEVSE mtrBaseClusterEnergyEVSE => mtrBaseClusterEnergyEVSE -> Ptr () -> IO ()
readAttributeBatteryCapacityWithCompletion mtrBaseClusterEnergyEVSE completion =
  sendMessage mtrBaseClusterEnergyEVSE readAttributeBatteryCapacityWithCompletionSelector completion

-- | @- subscribeAttributeBatteryCapacityWithParams:subscriptionEstablished:reportHandler:@
subscribeAttributeBatteryCapacityWithParams_subscriptionEstablished_reportHandler :: (IsMTRBaseClusterEnergyEVSE mtrBaseClusterEnergyEVSE, IsMTRSubscribeParams params) => mtrBaseClusterEnergyEVSE -> params -> Ptr () -> Ptr () -> IO ()
subscribeAttributeBatteryCapacityWithParams_subscriptionEstablished_reportHandler mtrBaseClusterEnergyEVSE params subscriptionEstablished reportHandler =
  sendMessage mtrBaseClusterEnergyEVSE subscribeAttributeBatteryCapacityWithParams_subscriptionEstablished_reportHandlerSelector (toMTRSubscribeParams params) subscriptionEstablished reportHandler

-- | @+ readAttributeBatteryCapacityWithClusterStateCache:endpoint:queue:completion:@
readAttributeBatteryCapacityWithClusterStateCache_endpoint_queue_completion :: (IsMTRClusterStateCacheContainer clusterStateCacheContainer, IsNSNumber endpoint, IsNSObject queue) => clusterStateCacheContainer -> endpoint -> queue -> Ptr () -> IO ()
readAttributeBatteryCapacityWithClusterStateCache_endpoint_queue_completion clusterStateCacheContainer endpoint queue completion =
  do
    cls' <- getRequiredClass "MTRBaseClusterEnergyEVSE"
    sendClassMessage cls' readAttributeBatteryCapacityWithClusterStateCache_endpoint_queue_completionSelector (toMTRClusterStateCacheContainer clusterStateCacheContainer) (toNSNumber endpoint) (toNSObject queue) completion

-- | @- readAttributeVehicleIDWithCompletion:@
readAttributeVehicleIDWithCompletion :: IsMTRBaseClusterEnergyEVSE mtrBaseClusterEnergyEVSE => mtrBaseClusterEnergyEVSE -> Ptr () -> IO ()
readAttributeVehicleIDWithCompletion mtrBaseClusterEnergyEVSE completion =
  sendMessage mtrBaseClusterEnergyEVSE readAttributeVehicleIDWithCompletionSelector completion

-- | @- subscribeAttributeVehicleIDWithParams:subscriptionEstablished:reportHandler:@
subscribeAttributeVehicleIDWithParams_subscriptionEstablished_reportHandler :: (IsMTRBaseClusterEnergyEVSE mtrBaseClusterEnergyEVSE, IsMTRSubscribeParams params) => mtrBaseClusterEnergyEVSE -> params -> Ptr () -> Ptr () -> IO ()
subscribeAttributeVehicleIDWithParams_subscriptionEstablished_reportHandler mtrBaseClusterEnergyEVSE params subscriptionEstablished reportHandler =
  sendMessage mtrBaseClusterEnergyEVSE subscribeAttributeVehicleIDWithParams_subscriptionEstablished_reportHandlerSelector (toMTRSubscribeParams params) subscriptionEstablished reportHandler

-- | @+ readAttributeVehicleIDWithClusterStateCache:endpoint:queue:completion:@
readAttributeVehicleIDWithClusterStateCache_endpoint_queue_completion :: (IsMTRClusterStateCacheContainer clusterStateCacheContainer, IsNSNumber endpoint, IsNSObject queue) => clusterStateCacheContainer -> endpoint -> queue -> Ptr () -> IO ()
readAttributeVehicleIDWithClusterStateCache_endpoint_queue_completion clusterStateCacheContainer endpoint queue completion =
  do
    cls' <- getRequiredClass "MTRBaseClusterEnergyEVSE"
    sendClassMessage cls' readAttributeVehicleIDWithClusterStateCache_endpoint_queue_completionSelector (toMTRClusterStateCacheContainer clusterStateCacheContainer) (toNSNumber endpoint) (toNSObject queue) completion

-- | @- readAttributeSessionIDWithCompletion:@
readAttributeSessionIDWithCompletion :: IsMTRBaseClusterEnergyEVSE mtrBaseClusterEnergyEVSE => mtrBaseClusterEnergyEVSE -> Ptr () -> IO ()
readAttributeSessionIDWithCompletion mtrBaseClusterEnergyEVSE completion =
  sendMessage mtrBaseClusterEnergyEVSE readAttributeSessionIDWithCompletionSelector completion

-- | @- subscribeAttributeSessionIDWithParams:subscriptionEstablished:reportHandler:@
subscribeAttributeSessionIDWithParams_subscriptionEstablished_reportHandler :: (IsMTRBaseClusterEnergyEVSE mtrBaseClusterEnergyEVSE, IsMTRSubscribeParams params) => mtrBaseClusterEnergyEVSE -> params -> Ptr () -> Ptr () -> IO ()
subscribeAttributeSessionIDWithParams_subscriptionEstablished_reportHandler mtrBaseClusterEnergyEVSE params subscriptionEstablished reportHandler =
  sendMessage mtrBaseClusterEnergyEVSE subscribeAttributeSessionIDWithParams_subscriptionEstablished_reportHandlerSelector (toMTRSubscribeParams params) subscriptionEstablished reportHandler

-- | @+ readAttributeSessionIDWithClusterStateCache:endpoint:queue:completion:@
readAttributeSessionIDWithClusterStateCache_endpoint_queue_completion :: (IsMTRClusterStateCacheContainer clusterStateCacheContainer, IsNSNumber endpoint, IsNSObject queue) => clusterStateCacheContainer -> endpoint -> queue -> Ptr () -> IO ()
readAttributeSessionIDWithClusterStateCache_endpoint_queue_completion clusterStateCacheContainer endpoint queue completion =
  do
    cls' <- getRequiredClass "MTRBaseClusterEnergyEVSE"
    sendClassMessage cls' readAttributeSessionIDWithClusterStateCache_endpoint_queue_completionSelector (toMTRClusterStateCacheContainer clusterStateCacheContainer) (toNSNumber endpoint) (toNSObject queue) completion

-- | @- readAttributeSessionDurationWithCompletion:@
readAttributeSessionDurationWithCompletion :: IsMTRBaseClusterEnergyEVSE mtrBaseClusterEnergyEVSE => mtrBaseClusterEnergyEVSE -> Ptr () -> IO ()
readAttributeSessionDurationWithCompletion mtrBaseClusterEnergyEVSE completion =
  sendMessage mtrBaseClusterEnergyEVSE readAttributeSessionDurationWithCompletionSelector completion

-- | @- subscribeAttributeSessionDurationWithParams:subscriptionEstablished:reportHandler:@
subscribeAttributeSessionDurationWithParams_subscriptionEstablished_reportHandler :: (IsMTRBaseClusterEnergyEVSE mtrBaseClusterEnergyEVSE, IsMTRSubscribeParams params) => mtrBaseClusterEnergyEVSE -> params -> Ptr () -> Ptr () -> IO ()
subscribeAttributeSessionDurationWithParams_subscriptionEstablished_reportHandler mtrBaseClusterEnergyEVSE params subscriptionEstablished reportHandler =
  sendMessage mtrBaseClusterEnergyEVSE subscribeAttributeSessionDurationWithParams_subscriptionEstablished_reportHandlerSelector (toMTRSubscribeParams params) subscriptionEstablished reportHandler

-- | @+ readAttributeSessionDurationWithClusterStateCache:endpoint:queue:completion:@
readAttributeSessionDurationWithClusterStateCache_endpoint_queue_completion :: (IsMTRClusterStateCacheContainer clusterStateCacheContainer, IsNSNumber endpoint, IsNSObject queue) => clusterStateCacheContainer -> endpoint -> queue -> Ptr () -> IO ()
readAttributeSessionDurationWithClusterStateCache_endpoint_queue_completion clusterStateCacheContainer endpoint queue completion =
  do
    cls' <- getRequiredClass "MTRBaseClusterEnergyEVSE"
    sendClassMessage cls' readAttributeSessionDurationWithClusterStateCache_endpoint_queue_completionSelector (toMTRClusterStateCacheContainer clusterStateCacheContainer) (toNSNumber endpoint) (toNSObject queue) completion

-- | @- readAttributeSessionEnergyChargedWithCompletion:@
readAttributeSessionEnergyChargedWithCompletion :: IsMTRBaseClusterEnergyEVSE mtrBaseClusterEnergyEVSE => mtrBaseClusterEnergyEVSE -> Ptr () -> IO ()
readAttributeSessionEnergyChargedWithCompletion mtrBaseClusterEnergyEVSE completion =
  sendMessage mtrBaseClusterEnergyEVSE readAttributeSessionEnergyChargedWithCompletionSelector completion

-- | @- subscribeAttributeSessionEnergyChargedWithParams:subscriptionEstablished:reportHandler:@
subscribeAttributeSessionEnergyChargedWithParams_subscriptionEstablished_reportHandler :: (IsMTRBaseClusterEnergyEVSE mtrBaseClusterEnergyEVSE, IsMTRSubscribeParams params) => mtrBaseClusterEnergyEVSE -> params -> Ptr () -> Ptr () -> IO ()
subscribeAttributeSessionEnergyChargedWithParams_subscriptionEstablished_reportHandler mtrBaseClusterEnergyEVSE params subscriptionEstablished reportHandler =
  sendMessage mtrBaseClusterEnergyEVSE subscribeAttributeSessionEnergyChargedWithParams_subscriptionEstablished_reportHandlerSelector (toMTRSubscribeParams params) subscriptionEstablished reportHandler

-- | @+ readAttributeSessionEnergyChargedWithClusterStateCache:endpoint:queue:completion:@
readAttributeSessionEnergyChargedWithClusterStateCache_endpoint_queue_completion :: (IsMTRClusterStateCacheContainer clusterStateCacheContainer, IsNSNumber endpoint, IsNSObject queue) => clusterStateCacheContainer -> endpoint -> queue -> Ptr () -> IO ()
readAttributeSessionEnergyChargedWithClusterStateCache_endpoint_queue_completion clusterStateCacheContainer endpoint queue completion =
  do
    cls' <- getRequiredClass "MTRBaseClusterEnergyEVSE"
    sendClassMessage cls' readAttributeSessionEnergyChargedWithClusterStateCache_endpoint_queue_completionSelector (toMTRClusterStateCacheContainer clusterStateCacheContainer) (toNSNumber endpoint) (toNSObject queue) completion

-- | @- readAttributeSessionEnergyDischargedWithCompletion:@
readAttributeSessionEnergyDischargedWithCompletion :: IsMTRBaseClusterEnergyEVSE mtrBaseClusterEnergyEVSE => mtrBaseClusterEnergyEVSE -> Ptr () -> IO ()
readAttributeSessionEnergyDischargedWithCompletion mtrBaseClusterEnergyEVSE completion =
  sendMessage mtrBaseClusterEnergyEVSE readAttributeSessionEnergyDischargedWithCompletionSelector completion

-- | @- subscribeAttributeSessionEnergyDischargedWithParams:subscriptionEstablished:reportHandler:@
subscribeAttributeSessionEnergyDischargedWithParams_subscriptionEstablished_reportHandler :: (IsMTRBaseClusterEnergyEVSE mtrBaseClusterEnergyEVSE, IsMTRSubscribeParams params) => mtrBaseClusterEnergyEVSE -> params -> Ptr () -> Ptr () -> IO ()
subscribeAttributeSessionEnergyDischargedWithParams_subscriptionEstablished_reportHandler mtrBaseClusterEnergyEVSE params subscriptionEstablished reportHandler =
  sendMessage mtrBaseClusterEnergyEVSE subscribeAttributeSessionEnergyDischargedWithParams_subscriptionEstablished_reportHandlerSelector (toMTRSubscribeParams params) subscriptionEstablished reportHandler

-- | @+ readAttributeSessionEnergyDischargedWithClusterStateCache:endpoint:queue:completion:@
readAttributeSessionEnergyDischargedWithClusterStateCache_endpoint_queue_completion :: (IsMTRClusterStateCacheContainer clusterStateCacheContainer, IsNSNumber endpoint, IsNSObject queue) => clusterStateCacheContainer -> endpoint -> queue -> Ptr () -> IO ()
readAttributeSessionEnergyDischargedWithClusterStateCache_endpoint_queue_completion clusterStateCacheContainer endpoint queue completion =
  do
    cls' <- getRequiredClass "MTRBaseClusterEnergyEVSE"
    sendClassMessage cls' readAttributeSessionEnergyDischargedWithClusterStateCache_endpoint_queue_completionSelector (toMTRClusterStateCacheContainer clusterStateCacheContainer) (toNSNumber endpoint) (toNSObject queue) completion

-- | @- readAttributeGeneratedCommandListWithCompletion:@
readAttributeGeneratedCommandListWithCompletion :: IsMTRBaseClusterEnergyEVSE mtrBaseClusterEnergyEVSE => mtrBaseClusterEnergyEVSE -> Ptr () -> IO ()
readAttributeGeneratedCommandListWithCompletion mtrBaseClusterEnergyEVSE completion =
  sendMessage mtrBaseClusterEnergyEVSE readAttributeGeneratedCommandListWithCompletionSelector completion

-- | @- subscribeAttributeGeneratedCommandListWithParams:subscriptionEstablished:reportHandler:@
subscribeAttributeGeneratedCommandListWithParams_subscriptionEstablished_reportHandler :: (IsMTRBaseClusterEnergyEVSE mtrBaseClusterEnergyEVSE, IsMTRSubscribeParams params) => mtrBaseClusterEnergyEVSE -> params -> Ptr () -> Ptr () -> IO ()
subscribeAttributeGeneratedCommandListWithParams_subscriptionEstablished_reportHandler mtrBaseClusterEnergyEVSE params subscriptionEstablished reportHandler =
  sendMessage mtrBaseClusterEnergyEVSE subscribeAttributeGeneratedCommandListWithParams_subscriptionEstablished_reportHandlerSelector (toMTRSubscribeParams params) subscriptionEstablished reportHandler

-- | @+ readAttributeGeneratedCommandListWithClusterStateCache:endpoint:queue:completion:@
readAttributeGeneratedCommandListWithClusterStateCache_endpoint_queue_completion :: (IsMTRClusterStateCacheContainer clusterStateCacheContainer, IsNSNumber endpoint, IsNSObject queue) => clusterStateCacheContainer -> endpoint -> queue -> Ptr () -> IO ()
readAttributeGeneratedCommandListWithClusterStateCache_endpoint_queue_completion clusterStateCacheContainer endpoint queue completion =
  do
    cls' <- getRequiredClass "MTRBaseClusterEnergyEVSE"
    sendClassMessage cls' readAttributeGeneratedCommandListWithClusterStateCache_endpoint_queue_completionSelector (toMTRClusterStateCacheContainer clusterStateCacheContainer) (toNSNumber endpoint) (toNSObject queue) completion

-- | @- readAttributeAcceptedCommandListWithCompletion:@
readAttributeAcceptedCommandListWithCompletion :: IsMTRBaseClusterEnergyEVSE mtrBaseClusterEnergyEVSE => mtrBaseClusterEnergyEVSE -> Ptr () -> IO ()
readAttributeAcceptedCommandListWithCompletion mtrBaseClusterEnergyEVSE completion =
  sendMessage mtrBaseClusterEnergyEVSE readAttributeAcceptedCommandListWithCompletionSelector completion

-- | @- subscribeAttributeAcceptedCommandListWithParams:subscriptionEstablished:reportHandler:@
subscribeAttributeAcceptedCommandListWithParams_subscriptionEstablished_reportHandler :: (IsMTRBaseClusterEnergyEVSE mtrBaseClusterEnergyEVSE, IsMTRSubscribeParams params) => mtrBaseClusterEnergyEVSE -> params -> Ptr () -> Ptr () -> IO ()
subscribeAttributeAcceptedCommandListWithParams_subscriptionEstablished_reportHandler mtrBaseClusterEnergyEVSE params subscriptionEstablished reportHandler =
  sendMessage mtrBaseClusterEnergyEVSE subscribeAttributeAcceptedCommandListWithParams_subscriptionEstablished_reportHandlerSelector (toMTRSubscribeParams params) subscriptionEstablished reportHandler

-- | @+ readAttributeAcceptedCommandListWithClusterStateCache:endpoint:queue:completion:@
readAttributeAcceptedCommandListWithClusterStateCache_endpoint_queue_completion :: (IsMTRClusterStateCacheContainer clusterStateCacheContainer, IsNSNumber endpoint, IsNSObject queue) => clusterStateCacheContainer -> endpoint -> queue -> Ptr () -> IO ()
readAttributeAcceptedCommandListWithClusterStateCache_endpoint_queue_completion clusterStateCacheContainer endpoint queue completion =
  do
    cls' <- getRequiredClass "MTRBaseClusterEnergyEVSE"
    sendClassMessage cls' readAttributeAcceptedCommandListWithClusterStateCache_endpoint_queue_completionSelector (toMTRClusterStateCacheContainer clusterStateCacheContainer) (toNSNumber endpoint) (toNSObject queue) completion

-- | @- readAttributeAttributeListWithCompletion:@
readAttributeAttributeListWithCompletion :: IsMTRBaseClusterEnergyEVSE mtrBaseClusterEnergyEVSE => mtrBaseClusterEnergyEVSE -> Ptr () -> IO ()
readAttributeAttributeListWithCompletion mtrBaseClusterEnergyEVSE completion =
  sendMessage mtrBaseClusterEnergyEVSE readAttributeAttributeListWithCompletionSelector completion

-- | @- subscribeAttributeAttributeListWithParams:subscriptionEstablished:reportHandler:@
subscribeAttributeAttributeListWithParams_subscriptionEstablished_reportHandler :: (IsMTRBaseClusterEnergyEVSE mtrBaseClusterEnergyEVSE, IsMTRSubscribeParams params) => mtrBaseClusterEnergyEVSE -> params -> Ptr () -> Ptr () -> IO ()
subscribeAttributeAttributeListWithParams_subscriptionEstablished_reportHandler mtrBaseClusterEnergyEVSE params subscriptionEstablished reportHandler =
  sendMessage mtrBaseClusterEnergyEVSE subscribeAttributeAttributeListWithParams_subscriptionEstablished_reportHandlerSelector (toMTRSubscribeParams params) subscriptionEstablished reportHandler

-- | @+ readAttributeAttributeListWithClusterStateCache:endpoint:queue:completion:@
readAttributeAttributeListWithClusterStateCache_endpoint_queue_completion :: (IsMTRClusterStateCacheContainer clusterStateCacheContainer, IsNSNumber endpoint, IsNSObject queue) => clusterStateCacheContainer -> endpoint -> queue -> Ptr () -> IO ()
readAttributeAttributeListWithClusterStateCache_endpoint_queue_completion clusterStateCacheContainer endpoint queue completion =
  do
    cls' <- getRequiredClass "MTRBaseClusterEnergyEVSE"
    sendClassMessage cls' readAttributeAttributeListWithClusterStateCache_endpoint_queue_completionSelector (toMTRClusterStateCacheContainer clusterStateCacheContainer) (toNSNumber endpoint) (toNSObject queue) completion

-- | @- readAttributeFeatureMapWithCompletion:@
readAttributeFeatureMapWithCompletion :: IsMTRBaseClusterEnergyEVSE mtrBaseClusterEnergyEVSE => mtrBaseClusterEnergyEVSE -> Ptr () -> IO ()
readAttributeFeatureMapWithCompletion mtrBaseClusterEnergyEVSE completion =
  sendMessage mtrBaseClusterEnergyEVSE readAttributeFeatureMapWithCompletionSelector completion

-- | @- subscribeAttributeFeatureMapWithParams:subscriptionEstablished:reportHandler:@
subscribeAttributeFeatureMapWithParams_subscriptionEstablished_reportHandler :: (IsMTRBaseClusterEnergyEVSE mtrBaseClusterEnergyEVSE, IsMTRSubscribeParams params) => mtrBaseClusterEnergyEVSE -> params -> Ptr () -> Ptr () -> IO ()
subscribeAttributeFeatureMapWithParams_subscriptionEstablished_reportHandler mtrBaseClusterEnergyEVSE params subscriptionEstablished reportHandler =
  sendMessage mtrBaseClusterEnergyEVSE subscribeAttributeFeatureMapWithParams_subscriptionEstablished_reportHandlerSelector (toMTRSubscribeParams params) subscriptionEstablished reportHandler

-- | @+ readAttributeFeatureMapWithClusterStateCache:endpoint:queue:completion:@
readAttributeFeatureMapWithClusterStateCache_endpoint_queue_completion :: (IsMTRClusterStateCacheContainer clusterStateCacheContainer, IsNSNumber endpoint, IsNSObject queue) => clusterStateCacheContainer -> endpoint -> queue -> Ptr () -> IO ()
readAttributeFeatureMapWithClusterStateCache_endpoint_queue_completion clusterStateCacheContainer endpoint queue completion =
  do
    cls' <- getRequiredClass "MTRBaseClusterEnergyEVSE"
    sendClassMessage cls' readAttributeFeatureMapWithClusterStateCache_endpoint_queue_completionSelector (toMTRClusterStateCacheContainer clusterStateCacheContainer) (toNSNumber endpoint) (toNSObject queue) completion

-- | @- readAttributeClusterRevisionWithCompletion:@
readAttributeClusterRevisionWithCompletion :: IsMTRBaseClusterEnergyEVSE mtrBaseClusterEnergyEVSE => mtrBaseClusterEnergyEVSE -> Ptr () -> IO ()
readAttributeClusterRevisionWithCompletion mtrBaseClusterEnergyEVSE completion =
  sendMessage mtrBaseClusterEnergyEVSE readAttributeClusterRevisionWithCompletionSelector completion

-- | @- subscribeAttributeClusterRevisionWithParams:subscriptionEstablished:reportHandler:@
subscribeAttributeClusterRevisionWithParams_subscriptionEstablished_reportHandler :: (IsMTRBaseClusterEnergyEVSE mtrBaseClusterEnergyEVSE, IsMTRSubscribeParams params) => mtrBaseClusterEnergyEVSE -> params -> Ptr () -> Ptr () -> IO ()
subscribeAttributeClusterRevisionWithParams_subscriptionEstablished_reportHandler mtrBaseClusterEnergyEVSE params subscriptionEstablished reportHandler =
  sendMessage mtrBaseClusterEnergyEVSE subscribeAttributeClusterRevisionWithParams_subscriptionEstablished_reportHandlerSelector (toMTRSubscribeParams params) subscriptionEstablished reportHandler

-- | @+ readAttributeClusterRevisionWithClusterStateCache:endpoint:queue:completion:@
readAttributeClusterRevisionWithClusterStateCache_endpoint_queue_completion :: (IsMTRClusterStateCacheContainer clusterStateCacheContainer, IsNSNumber endpoint, IsNSObject queue) => clusterStateCacheContainer -> endpoint -> queue -> Ptr () -> IO ()
readAttributeClusterRevisionWithClusterStateCache_endpoint_queue_completion clusterStateCacheContainer endpoint queue completion =
  do
    cls' <- getRequiredClass "MTRBaseClusterEnergyEVSE"
    sendClassMessage cls' readAttributeClusterRevisionWithClusterStateCache_endpoint_queue_completionSelector (toMTRClusterStateCacheContainer clusterStateCacheContainer) (toNSNumber endpoint) (toNSObject queue) completion

-- | @- init@
init_ :: IsMTRBaseClusterEnergyEVSE mtrBaseClusterEnergyEVSE => mtrBaseClusterEnergyEVSE -> IO (Id MTRBaseClusterEnergyEVSE)
init_ mtrBaseClusterEnergyEVSE =
  sendOwnedMessage mtrBaseClusterEnergyEVSE initSelector

-- | @+ new@
new :: IO (Id MTRBaseClusterEnergyEVSE)
new  =
  do
    cls' <- getRequiredClass "MTRBaseClusterEnergyEVSE"
    sendOwnedClassMessage cls' newSelector

-- | For all instance methods (reads, writes, commands) that take a completion, the completion will be called on the provided queue.
--
-- ObjC selector: @- initWithDevice:endpointID:queue:@
initWithDevice_endpointID_queue :: (IsMTRBaseClusterEnergyEVSE mtrBaseClusterEnergyEVSE, IsMTRBaseDevice device, IsNSNumber endpointID, IsNSObject queue) => mtrBaseClusterEnergyEVSE -> device -> endpointID -> queue -> IO (Id MTRBaseClusterEnergyEVSE)
initWithDevice_endpointID_queue mtrBaseClusterEnergyEVSE device endpointID queue =
  sendOwnedMessage mtrBaseClusterEnergyEVSE initWithDevice_endpointID_queueSelector (toMTRBaseDevice device) (toNSNumber endpointID) (toNSObject queue)

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @disableWithParams:completion:@
disableWithParams_completionSelector :: Selector '[Id MTREnergyEVSEClusterDisableParams, Ptr ()] ()
disableWithParams_completionSelector = mkSelector "disableWithParams:completion:"

-- | @Selector@ for @disableWithCompletion:@
disableWithCompletionSelector :: Selector '[Ptr ()] ()
disableWithCompletionSelector = mkSelector "disableWithCompletion:"

-- | @Selector@ for @enableChargingWithParams:completion:@
enableChargingWithParams_completionSelector :: Selector '[Id MTREnergyEVSEClusterEnableChargingParams, Ptr ()] ()
enableChargingWithParams_completionSelector = mkSelector "enableChargingWithParams:completion:"

-- | @Selector@ for @enableDischargingWithParams:completion:@
enableDischargingWithParams_completionSelector :: Selector '[Id MTREnergyEVSEClusterEnableDischargingParams, Ptr ()] ()
enableDischargingWithParams_completionSelector = mkSelector "enableDischargingWithParams:completion:"

-- | @Selector@ for @startDiagnosticsWithParams:completion:@
startDiagnosticsWithParams_completionSelector :: Selector '[Id MTREnergyEVSEClusterStartDiagnosticsParams, Ptr ()] ()
startDiagnosticsWithParams_completionSelector = mkSelector "startDiagnosticsWithParams:completion:"

-- | @Selector@ for @startDiagnosticsWithCompletion:@
startDiagnosticsWithCompletionSelector :: Selector '[Ptr ()] ()
startDiagnosticsWithCompletionSelector = mkSelector "startDiagnosticsWithCompletion:"

-- | @Selector@ for @setTargetsWithParams:completion:@
setTargetsWithParams_completionSelector :: Selector '[Id MTREnergyEVSEClusterSetTargetsParams, Ptr ()] ()
setTargetsWithParams_completionSelector = mkSelector "setTargetsWithParams:completion:"

-- | @Selector@ for @getTargetsWithParams:completion:@
getTargetsWithParams_completionSelector :: Selector '[Id MTREnergyEVSEClusterGetTargetsParams, Ptr ()] ()
getTargetsWithParams_completionSelector = mkSelector "getTargetsWithParams:completion:"

-- | @Selector@ for @getTargetsWithCompletion:@
getTargetsWithCompletionSelector :: Selector '[Ptr ()] ()
getTargetsWithCompletionSelector = mkSelector "getTargetsWithCompletion:"

-- | @Selector@ for @clearTargetsWithParams:completion:@
clearTargetsWithParams_completionSelector :: Selector '[Id MTREnergyEVSEClusterClearTargetsParams, Ptr ()] ()
clearTargetsWithParams_completionSelector = mkSelector "clearTargetsWithParams:completion:"

-- | @Selector@ for @clearTargetsWithCompletion:@
clearTargetsWithCompletionSelector :: Selector '[Ptr ()] ()
clearTargetsWithCompletionSelector = mkSelector "clearTargetsWithCompletion:"

-- | @Selector@ for @readAttributeStateWithCompletion:@
readAttributeStateWithCompletionSelector :: Selector '[Ptr ()] ()
readAttributeStateWithCompletionSelector = mkSelector "readAttributeStateWithCompletion:"

-- | @Selector@ for @subscribeAttributeStateWithParams:subscriptionEstablished:reportHandler:@
subscribeAttributeStateWithParams_subscriptionEstablished_reportHandlerSelector :: Selector '[Id MTRSubscribeParams, Ptr (), Ptr ()] ()
subscribeAttributeStateWithParams_subscriptionEstablished_reportHandlerSelector = mkSelector "subscribeAttributeStateWithParams:subscriptionEstablished:reportHandler:"

-- | @Selector@ for @readAttributeStateWithClusterStateCache:endpoint:queue:completion:@
readAttributeStateWithClusterStateCache_endpoint_queue_completionSelector :: Selector '[Id MTRClusterStateCacheContainer, Id NSNumber, Id NSObject, Ptr ()] ()
readAttributeStateWithClusterStateCache_endpoint_queue_completionSelector = mkSelector "readAttributeStateWithClusterStateCache:endpoint:queue:completion:"

-- | @Selector@ for @readAttributeSupplyStateWithCompletion:@
readAttributeSupplyStateWithCompletionSelector :: Selector '[Ptr ()] ()
readAttributeSupplyStateWithCompletionSelector = mkSelector "readAttributeSupplyStateWithCompletion:"

-- | @Selector@ for @subscribeAttributeSupplyStateWithParams:subscriptionEstablished:reportHandler:@
subscribeAttributeSupplyStateWithParams_subscriptionEstablished_reportHandlerSelector :: Selector '[Id MTRSubscribeParams, Ptr (), Ptr ()] ()
subscribeAttributeSupplyStateWithParams_subscriptionEstablished_reportHandlerSelector = mkSelector "subscribeAttributeSupplyStateWithParams:subscriptionEstablished:reportHandler:"

-- | @Selector@ for @readAttributeSupplyStateWithClusterStateCache:endpoint:queue:completion:@
readAttributeSupplyStateWithClusterStateCache_endpoint_queue_completionSelector :: Selector '[Id MTRClusterStateCacheContainer, Id NSNumber, Id NSObject, Ptr ()] ()
readAttributeSupplyStateWithClusterStateCache_endpoint_queue_completionSelector = mkSelector "readAttributeSupplyStateWithClusterStateCache:endpoint:queue:completion:"

-- | @Selector@ for @readAttributeFaultStateWithCompletion:@
readAttributeFaultStateWithCompletionSelector :: Selector '[Ptr ()] ()
readAttributeFaultStateWithCompletionSelector = mkSelector "readAttributeFaultStateWithCompletion:"

-- | @Selector@ for @subscribeAttributeFaultStateWithParams:subscriptionEstablished:reportHandler:@
subscribeAttributeFaultStateWithParams_subscriptionEstablished_reportHandlerSelector :: Selector '[Id MTRSubscribeParams, Ptr (), Ptr ()] ()
subscribeAttributeFaultStateWithParams_subscriptionEstablished_reportHandlerSelector = mkSelector "subscribeAttributeFaultStateWithParams:subscriptionEstablished:reportHandler:"

-- | @Selector@ for @readAttributeFaultStateWithClusterStateCache:endpoint:queue:completion:@
readAttributeFaultStateWithClusterStateCache_endpoint_queue_completionSelector :: Selector '[Id MTRClusterStateCacheContainer, Id NSNumber, Id NSObject, Ptr ()] ()
readAttributeFaultStateWithClusterStateCache_endpoint_queue_completionSelector = mkSelector "readAttributeFaultStateWithClusterStateCache:endpoint:queue:completion:"

-- | @Selector@ for @readAttributeChargingEnabledUntilWithCompletion:@
readAttributeChargingEnabledUntilWithCompletionSelector :: Selector '[Ptr ()] ()
readAttributeChargingEnabledUntilWithCompletionSelector = mkSelector "readAttributeChargingEnabledUntilWithCompletion:"

-- | @Selector@ for @subscribeAttributeChargingEnabledUntilWithParams:subscriptionEstablished:reportHandler:@
subscribeAttributeChargingEnabledUntilWithParams_subscriptionEstablished_reportHandlerSelector :: Selector '[Id MTRSubscribeParams, Ptr (), Ptr ()] ()
subscribeAttributeChargingEnabledUntilWithParams_subscriptionEstablished_reportHandlerSelector = mkSelector "subscribeAttributeChargingEnabledUntilWithParams:subscriptionEstablished:reportHandler:"

-- | @Selector@ for @readAttributeChargingEnabledUntilWithClusterStateCache:endpoint:queue:completion:@
readAttributeChargingEnabledUntilWithClusterStateCache_endpoint_queue_completionSelector :: Selector '[Id MTRClusterStateCacheContainer, Id NSNumber, Id NSObject, Ptr ()] ()
readAttributeChargingEnabledUntilWithClusterStateCache_endpoint_queue_completionSelector = mkSelector "readAttributeChargingEnabledUntilWithClusterStateCache:endpoint:queue:completion:"

-- | @Selector@ for @readAttributeDischargingEnabledUntilWithCompletion:@
readAttributeDischargingEnabledUntilWithCompletionSelector :: Selector '[Ptr ()] ()
readAttributeDischargingEnabledUntilWithCompletionSelector = mkSelector "readAttributeDischargingEnabledUntilWithCompletion:"

-- | @Selector@ for @subscribeAttributeDischargingEnabledUntilWithParams:subscriptionEstablished:reportHandler:@
subscribeAttributeDischargingEnabledUntilWithParams_subscriptionEstablished_reportHandlerSelector :: Selector '[Id MTRSubscribeParams, Ptr (), Ptr ()] ()
subscribeAttributeDischargingEnabledUntilWithParams_subscriptionEstablished_reportHandlerSelector = mkSelector "subscribeAttributeDischargingEnabledUntilWithParams:subscriptionEstablished:reportHandler:"

-- | @Selector@ for @readAttributeDischargingEnabledUntilWithClusterStateCache:endpoint:queue:completion:@
readAttributeDischargingEnabledUntilWithClusterStateCache_endpoint_queue_completionSelector :: Selector '[Id MTRClusterStateCacheContainer, Id NSNumber, Id NSObject, Ptr ()] ()
readAttributeDischargingEnabledUntilWithClusterStateCache_endpoint_queue_completionSelector = mkSelector "readAttributeDischargingEnabledUntilWithClusterStateCache:endpoint:queue:completion:"

-- | @Selector@ for @readAttributeCircuitCapacityWithCompletion:@
readAttributeCircuitCapacityWithCompletionSelector :: Selector '[Ptr ()] ()
readAttributeCircuitCapacityWithCompletionSelector = mkSelector "readAttributeCircuitCapacityWithCompletion:"

-- | @Selector@ for @subscribeAttributeCircuitCapacityWithParams:subscriptionEstablished:reportHandler:@
subscribeAttributeCircuitCapacityWithParams_subscriptionEstablished_reportHandlerSelector :: Selector '[Id MTRSubscribeParams, Ptr (), Ptr ()] ()
subscribeAttributeCircuitCapacityWithParams_subscriptionEstablished_reportHandlerSelector = mkSelector "subscribeAttributeCircuitCapacityWithParams:subscriptionEstablished:reportHandler:"

-- | @Selector@ for @readAttributeCircuitCapacityWithClusterStateCache:endpoint:queue:completion:@
readAttributeCircuitCapacityWithClusterStateCache_endpoint_queue_completionSelector :: Selector '[Id MTRClusterStateCacheContainer, Id NSNumber, Id NSObject, Ptr ()] ()
readAttributeCircuitCapacityWithClusterStateCache_endpoint_queue_completionSelector = mkSelector "readAttributeCircuitCapacityWithClusterStateCache:endpoint:queue:completion:"

-- | @Selector@ for @readAttributeMinimumChargeCurrentWithCompletion:@
readAttributeMinimumChargeCurrentWithCompletionSelector :: Selector '[Ptr ()] ()
readAttributeMinimumChargeCurrentWithCompletionSelector = mkSelector "readAttributeMinimumChargeCurrentWithCompletion:"

-- | @Selector@ for @subscribeAttributeMinimumChargeCurrentWithParams:subscriptionEstablished:reportHandler:@
subscribeAttributeMinimumChargeCurrentWithParams_subscriptionEstablished_reportHandlerSelector :: Selector '[Id MTRSubscribeParams, Ptr (), Ptr ()] ()
subscribeAttributeMinimumChargeCurrentWithParams_subscriptionEstablished_reportHandlerSelector = mkSelector "subscribeAttributeMinimumChargeCurrentWithParams:subscriptionEstablished:reportHandler:"

-- | @Selector@ for @readAttributeMinimumChargeCurrentWithClusterStateCache:endpoint:queue:completion:@
readAttributeMinimumChargeCurrentWithClusterStateCache_endpoint_queue_completionSelector :: Selector '[Id MTRClusterStateCacheContainer, Id NSNumber, Id NSObject, Ptr ()] ()
readAttributeMinimumChargeCurrentWithClusterStateCache_endpoint_queue_completionSelector = mkSelector "readAttributeMinimumChargeCurrentWithClusterStateCache:endpoint:queue:completion:"

-- | @Selector@ for @readAttributeMaximumChargeCurrentWithCompletion:@
readAttributeMaximumChargeCurrentWithCompletionSelector :: Selector '[Ptr ()] ()
readAttributeMaximumChargeCurrentWithCompletionSelector = mkSelector "readAttributeMaximumChargeCurrentWithCompletion:"

-- | @Selector@ for @subscribeAttributeMaximumChargeCurrentWithParams:subscriptionEstablished:reportHandler:@
subscribeAttributeMaximumChargeCurrentWithParams_subscriptionEstablished_reportHandlerSelector :: Selector '[Id MTRSubscribeParams, Ptr (), Ptr ()] ()
subscribeAttributeMaximumChargeCurrentWithParams_subscriptionEstablished_reportHandlerSelector = mkSelector "subscribeAttributeMaximumChargeCurrentWithParams:subscriptionEstablished:reportHandler:"

-- | @Selector@ for @readAttributeMaximumChargeCurrentWithClusterStateCache:endpoint:queue:completion:@
readAttributeMaximumChargeCurrentWithClusterStateCache_endpoint_queue_completionSelector :: Selector '[Id MTRClusterStateCacheContainer, Id NSNumber, Id NSObject, Ptr ()] ()
readAttributeMaximumChargeCurrentWithClusterStateCache_endpoint_queue_completionSelector = mkSelector "readAttributeMaximumChargeCurrentWithClusterStateCache:endpoint:queue:completion:"

-- | @Selector@ for @readAttributeMaximumDischargeCurrentWithCompletion:@
readAttributeMaximumDischargeCurrentWithCompletionSelector :: Selector '[Ptr ()] ()
readAttributeMaximumDischargeCurrentWithCompletionSelector = mkSelector "readAttributeMaximumDischargeCurrentWithCompletion:"

-- | @Selector@ for @subscribeAttributeMaximumDischargeCurrentWithParams:subscriptionEstablished:reportHandler:@
subscribeAttributeMaximumDischargeCurrentWithParams_subscriptionEstablished_reportHandlerSelector :: Selector '[Id MTRSubscribeParams, Ptr (), Ptr ()] ()
subscribeAttributeMaximumDischargeCurrentWithParams_subscriptionEstablished_reportHandlerSelector = mkSelector "subscribeAttributeMaximumDischargeCurrentWithParams:subscriptionEstablished:reportHandler:"

-- | @Selector@ for @readAttributeMaximumDischargeCurrentWithClusterStateCache:endpoint:queue:completion:@
readAttributeMaximumDischargeCurrentWithClusterStateCache_endpoint_queue_completionSelector :: Selector '[Id MTRClusterStateCacheContainer, Id NSNumber, Id NSObject, Ptr ()] ()
readAttributeMaximumDischargeCurrentWithClusterStateCache_endpoint_queue_completionSelector = mkSelector "readAttributeMaximumDischargeCurrentWithClusterStateCache:endpoint:queue:completion:"

-- | @Selector@ for @readAttributeUserMaximumChargeCurrentWithCompletion:@
readAttributeUserMaximumChargeCurrentWithCompletionSelector :: Selector '[Ptr ()] ()
readAttributeUserMaximumChargeCurrentWithCompletionSelector = mkSelector "readAttributeUserMaximumChargeCurrentWithCompletion:"

-- | @Selector@ for @writeAttributeUserMaximumChargeCurrentWithValue:completion:@
writeAttributeUserMaximumChargeCurrentWithValue_completionSelector :: Selector '[Id NSNumber, Ptr ()] ()
writeAttributeUserMaximumChargeCurrentWithValue_completionSelector = mkSelector "writeAttributeUserMaximumChargeCurrentWithValue:completion:"

-- | @Selector@ for @writeAttributeUserMaximumChargeCurrentWithValue:params:completion:@
writeAttributeUserMaximumChargeCurrentWithValue_params_completionSelector :: Selector '[Id NSNumber, Id MTRWriteParams, Ptr ()] ()
writeAttributeUserMaximumChargeCurrentWithValue_params_completionSelector = mkSelector "writeAttributeUserMaximumChargeCurrentWithValue:params:completion:"

-- | @Selector@ for @subscribeAttributeUserMaximumChargeCurrentWithParams:subscriptionEstablished:reportHandler:@
subscribeAttributeUserMaximumChargeCurrentWithParams_subscriptionEstablished_reportHandlerSelector :: Selector '[Id MTRSubscribeParams, Ptr (), Ptr ()] ()
subscribeAttributeUserMaximumChargeCurrentWithParams_subscriptionEstablished_reportHandlerSelector = mkSelector "subscribeAttributeUserMaximumChargeCurrentWithParams:subscriptionEstablished:reportHandler:"

-- | @Selector@ for @readAttributeUserMaximumChargeCurrentWithClusterStateCache:endpoint:queue:completion:@
readAttributeUserMaximumChargeCurrentWithClusterStateCache_endpoint_queue_completionSelector :: Selector '[Id MTRClusterStateCacheContainer, Id NSNumber, Id NSObject, Ptr ()] ()
readAttributeUserMaximumChargeCurrentWithClusterStateCache_endpoint_queue_completionSelector = mkSelector "readAttributeUserMaximumChargeCurrentWithClusterStateCache:endpoint:queue:completion:"

-- | @Selector@ for @readAttributeRandomizationDelayWindowWithCompletion:@
readAttributeRandomizationDelayWindowWithCompletionSelector :: Selector '[Ptr ()] ()
readAttributeRandomizationDelayWindowWithCompletionSelector = mkSelector "readAttributeRandomizationDelayWindowWithCompletion:"

-- | @Selector@ for @writeAttributeRandomizationDelayWindowWithValue:completion:@
writeAttributeRandomizationDelayWindowWithValue_completionSelector :: Selector '[Id NSNumber, Ptr ()] ()
writeAttributeRandomizationDelayWindowWithValue_completionSelector = mkSelector "writeAttributeRandomizationDelayWindowWithValue:completion:"

-- | @Selector@ for @writeAttributeRandomizationDelayWindowWithValue:params:completion:@
writeAttributeRandomizationDelayWindowWithValue_params_completionSelector :: Selector '[Id NSNumber, Id MTRWriteParams, Ptr ()] ()
writeAttributeRandomizationDelayWindowWithValue_params_completionSelector = mkSelector "writeAttributeRandomizationDelayWindowWithValue:params:completion:"

-- | @Selector@ for @subscribeAttributeRandomizationDelayWindowWithParams:subscriptionEstablished:reportHandler:@
subscribeAttributeRandomizationDelayWindowWithParams_subscriptionEstablished_reportHandlerSelector :: Selector '[Id MTRSubscribeParams, Ptr (), Ptr ()] ()
subscribeAttributeRandomizationDelayWindowWithParams_subscriptionEstablished_reportHandlerSelector = mkSelector "subscribeAttributeRandomizationDelayWindowWithParams:subscriptionEstablished:reportHandler:"

-- | @Selector@ for @readAttributeRandomizationDelayWindowWithClusterStateCache:endpoint:queue:completion:@
readAttributeRandomizationDelayWindowWithClusterStateCache_endpoint_queue_completionSelector :: Selector '[Id MTRClusterStateCacheContainer, Id NSNumber, Id NSObject, Ptr ()] ()
readAttributeRandomizationDelayWindowWithClusterStateCache_endpoint_queue_completionSelector = mkSelector "readAttributeRandomizationDelayWindowWithClusterStateCache:endpoint:queue:completion:"

-- | @Selector@ for @readAttributeNextChargeStartTimeWithCompletion:@
readAttributeNextChargeStartTimeWithCompletionSelector :: Selector '[Ptr ()] ()
readAttributeNextChargeStartTimeWithCompletionSelector = mkSelector "readAttributeNextChargeStartTimeWithCompletion:"

-- | @Selector@ for @subscribeAttributeNextChargeStartTimeWithParams:subscriptionEstablished:reportHandler:@
subscribeAttributeNextChargeStartTimeWithParams_subscriptionEstablished_reportHandlerSelector :: Selector '[Id MTRSubscribeParams, Ptr (), Ptr ()] ()
subscribeAttributeNextChargeStartTimeWithParams_subscriptionEstablished_reportHandlerSelector = mkSelector "subscribeAttributeNextChargeStartTimeWithParams:subscriptionEstablished:reportHandler:"

-- | @Selector@ for @readAttributeNextChargeStartTimeWithClusterStateCache:endpoint:queue:completion:@
readAttributeNextChargeStartTimeWithClusterStateCache_endpoint_queue_completionSelector :: Selector '[Id MTRClusterStateCacheContainer, Id NSNumber, Id NSObject, Ptr ()] ()
readAttributeNextChargeStartTimeWithClusterStateCache_endpoint_queue_completionSelector = mkSelector "readAttributeNextChargeStartTimeWithClusterStateCache:endpoint:queue:completion:"

-- | @Selector@ for @readAttributeNextChargeTargetTimeWithCompletion:@
readAttributeNextChargeTargetTimeWithCompletionSelector :: Selector '[Ptr ()] ()
readAttributeNextChargeTargetTimeWithCompletionSelector = mkSelector "readAttributeNextChargeTargetTimeWithCompletion:"

-- | @Selector@ for @subscribeAttributeNextChargeTargetTimeWithParams:subscriptionEstablished:reportHandler:@
subscribeAttributeNextChargeTargetTimeWithParams_subscriptionEstablished_reportHandlerSelector :: Selector '[Id MTRSubscribeParams, Ptr (), Ptr ()] ()
subscribeAttributeNextChargeTargetTimeWithParams_subscriptionEstablished_reportHandlerSelector = mkSelector "subscribeAttributeNextChargeTargetTimeWithParams:subscriptionEstablished:reportHandler:"

-- | @Selector@ for @readAttributeNextChargeTargetTimeWithClusterStateCache:endpoint:queue:completion:@
readAttributeNextChargeTargetTimeWithClusterStateCache_endpoint_queue_completionSelector :: Selector '[Id MTRClusterStateCacheContainer, Id NSNumber, Id NSObject, Ptr ()] ()
readAttributeNextChargeTargetTimeWithClusterStateCache_endpoint_queue_completionSelector = mkSelector "readAttributeNextChargeTargetTimeWithClusterStateCache:endpoint:queue:completion:"

-- | @Selector@ for @readAttributeNextChargeRequiredEnergyWithCompletion:@
readAttributeNextChargeRequiredEnergyWithCompletionSelector :: Selector '[Ptr ()] ()
readAttributeNextChargeRequiredEnergyWithCompletionSelector = mkSelector "readAttributeNextChargeRequiredEnergyWithCompletion:"

-- | @Selector@ for @subscribeAttributeNextChargeRequiredEnergyWithParams:subscriptionEstablished:reportHandler:@
subscribeAttributeNextChargeRequiredEnergyWithParams_subscriptionEstablished_reportHandlerSelector :: Selector '[Id MTRSubscribeParams, Ptr (), Ptr ()] ()
subscribeAttributeNextChargeRequiredEnergyWithParams_subscriptionEstablished_reportHandlerSelector = mkSelector "subscribeAttributeNextChargeRequiredEnergyWithParams:subscriptionEstablished:reportHandler:"

-- | @Selector@ for @readAttributeNextChargeRequiredEnergyWithClusterStateCache:endpoint:queue:completion:@
readAttributeNextChargeRequiredEnergyWithClusterStateCache_endpoint_queue_completionSelector :: Selector '[Id MTRClusterStateCacheContainer, Id NSNumber, Id NSObject, Ptr ()] ()
readAttributeNextChargeRequiredEnergyWithClusterStateCache_endpoint_queue_completionSelector = mkSelector "readAttributeNextChargeRequiredEnergyWithClusterStateCache:endpoint:queue:completion:"

-- | @Selector@ for @readAttributeNextChargeTargetSoCWithCompletion:@
readAttributeNextChargeTargetSoCWithCompletionSelector :: Selector '[Ptr ()] ()
readAttributeNextChargeTargetSoCWithCompletionSelector = mkSelector "readAttributeNextChargeTargetSoCWithCompletion:"

-- | @Selector@ for @subscribeAttributeNextChargeTargetSoCWithParams:subscriptionEstablished:reportHandler:@
subscribeAttributeNextChargeTargetSoCWithParams_subscriptionEstablished_reportHandlerSelector :: Selector '[Id MTRSubscribeParams, Ptr (), Ptr ()] ()
subscribeAttributeNextChargeTargetSoCWithParams_subscriptionEstablished_reportHandlerSelector = mkSelector "subscribeAttributeNextChargeTargetSoCWithParams:subscriptionEstablished:reportHandler:"

-- | @Selector@ for @readAttributeNextChargeTargetSoCWithClusterStateCache:endpoint:queue:completion:@
readAttributeNextChargeTargetSoCWithClusterStateCache_endpoint_queue_completionSelector :: Selector '[Id MTRClusterStateCacheContainer, Id NSNumber, Id NSObject, Ptr ()] ()
readAttributeNextChargeTargetSoCWithClusterStateCache_endpoint_queue_completionSelector = mkSelector "readAttributeNextChargeTargetSoCWithClusterStateCache:endpoint:queue:completion:"

-- | @Selector@ for @readAttributeApproximateEVEfficiencyWithCompletion:@
readAttributeApproximateEVEfficiencyWithCompletionSelector :: Selector '[Ptr ()] ()
readAttributeApproximateEVEfficiencyWithCompletionSelector = mkSelector "readAttributeApproximateEVEfficiencyWithCompletion:"

-- | @Selector@ for @writeAttributeApproximateEVEfficiencyWithValue:completion:@
writeAttributeApproximateEVEfficiencyWithValue_completionSelector :: Selector '[Id NSNumber, Ptr ()] ()
writeAttributeApproximateEVEfficiencyWithValue_completionSelector = mkSelector "writeAttributeApproximateEVEfficiencyWithValue:completion:"

-- | @Selector@ for @writeAttributeApproximateEVEfficiencyWithValue:params:completion:@
writeAttributeApproximateEVEfficiencyWithValue_params_completionSelector :: Selector '[Id NSNumber, Id MTRWriteParams, Ptr ()] ()
writeAttributeApproximateEVEfficiencyWithValue_params_completionSelector = mkSelector "writeAttributeApproximateEVEfficiencyWithValue:params:completion:"

-- | @Selector@ for @subscribeAttributeApproximateEVEfficiencyWithParams:subscriptionEstablished:reportHandler:@
subscribeAttributeApproximateEVEfficiencyWithParams_subscriptionEstablished_reportHandlerSelector :: Selector '[Id MTRSubscribeParams, Ptr (), Ptr ()] ()
subscribeAttributeApproximateEVEfficiencyWithParams_subscriptionEstablished_reportHandlerSelector = mkSelector "subscribeAttributeApproximateEVEfficiencyWithParams:subscriptionEstablished:reportHandler:"

-- | @Selector@ for @readAttributeApproximateEVEfficiencyWithClusterStateCache:endpoint:queue:completion:@
readAttributeApproximateEVEfficiencyWithClusterStateCache_endpoint_queue_completionSelector :: Selector '[Id MTRClusterStateCacheContainer, Id NSNumber, Id NSObject, Ptr ()] ()
readAttributeApproximateEVEfficiencyWithClusterStateCache_endpoint_queue_completionSelector = mkSelector "readAttributeApproximateEVEfficiencyWithClusterStateCache:endpoint:queue:completion:"

-- | @Selector@ for @readAttributeStateOfChargeWithCompletion:@
readAttributeStateOfChargeWithCompletionSelector :: Selector '[Ptr ()] ()
readAttributeStateOfChargeWithCompletionSelector = mkSelector "readAttributeStateOfChargeWithCompletion:"

-- | @Selector@ for @subscribeAttributeStateOfChargeWithParams:subscriptionEstablished:reportHandler:@
subscribeAttributeStateOfChargeWithParams_subscriptionEstablished_reportHandlerSelector :: Selector '[Id MTRSubscribeParams, Ptr (), Ptr ()] ()
subscribeAttributeStateOfChargeWithParams_subscriptionEstablished_reportHandlerSelector = mkSelector "subscribeAttributeStateOfChargeWithParams:subscriptionEstablished:reportHandler:"

-- | @Selector@ for @readAttributeStateOfChargeWithClusterStateCache:endpoint:queue:completion:@
readAttributeStateOfChargeWithClusterStateCache_endpoint_queue_completionSelector :: Selector '[Id MTRClusterStateCacheContainer, Id NSNumber, Id NSObject, Ptr ()] ()
readAttributeStateOfChargeWithClusterStateCache_endpoint_queue_completionSelector = mkSelector "readAttributeStateOfChargeWithClusterStateCache:endpoint:queue:completion:"

-- | @Selector@ for @readAttributeBatteryCapacityWithCompletion:@
readAttributeBatteryCapacityWithCompletionSelector :: Selector '[Ptr ()] ()
readAttributeBatteryCapacityWithCompletionSelector = mkSelector "readAttributeBatteryCapacityWithCompletion:"

-- | @Selector@ for @subscribeAttributeBatteryCapacityWithParams:subscriptionEstablished:reportHandler:@
subscribeAttributeBatteryCapacityWithParams_subscriptionEstablished_reportHandlerSelector :: Selector '[Id MTRSubscribeParams, Ptr (), Ptr ()] ()
subscribeAttributeBatteryCapacityWithParams_subscriptionEstablished_reportHandlerSelector = mkSelector "subscribeAttributeBatteryCapacityWithParams:subscriptionEstablished:reportHandler:"

-- | @Selector@ for @readAttributeBatteryCapacityWithClusterStateCache:endpoint:queue:completion:@
readAttributeBatteryCapacityWithClusterStateCache_endpoint_queue_completionSelector :: Selector '[Id MTRClusterStateCacheContainer, Id NSNumber, Id NSObject, Ptr ()] ()
readAttributeBatteryCapacityWithClusterStateCache_endpoint_queue_completionSelector = mkSelector "readAttributeBatteryCapacityWithClusterStateCache:endpoint:queue:completion:"

-- | @Selector@ for @readAttributeVehicleIDWithCompletion:@
readAttributeVehicleIDWithCompletionSelector :: Selector '[Ptr ()] ()
readAttributeVehicleIDWithCompletionSelector = mkSelector "readAttributeVehicleIDWithCompletion:"

-- | @Selector@ for @subscribeAttributeVehicleIDWithParams:subscriptionEstablished:reportHandler:@
subscribeAttributeVehicleIDWithParams_subscriptionEstablished_reportHandlerSelector :: Selector '[Id MTRSubscribeParams, Ptr (), Ptr ()] ()
subscribeAttributeVehicleIDWithParams_subscriptionEstablished_reportHandlerSelector = mkSelector "subscribeAttributeVehicleIDWithParams:subscriptionEstablished:reportHandler:"

-- | @Selector@ for @readAttributeVehicleIDWithClusterStateCache:endpoint:queue:completion:@
readAttributeVehicleIDWithClusterStateCache_endpoint_queue_completionSelector :: Selector '[Id MTRClusterStateCacheContainer, Id NSNumber, Id NSObject, Ptr ()] ()
readAttributeVehicleIDWithClusterStateCache_endpoint_queue_completionSelector = mkSelector "readAttributeVehicleIDWithClusterStateCache:endpoint:queue:completion:"

-- | @Selector@ for @readAttributeSessionIDWithCompletion:@
readAttributeSessionIDWithCompletionSelector :: Selector '[Ptr ()] ()
readAttributeSessionIDWithCompletionSelector = mkSelector "readAttributeSessionIDWithCompletion:"

-- | @Selector@ for @subscribeAttributeSessionIDWithParams:subscriptionEstablished:reportHandler:@
subscribeAttributeSessionIDWithParams_subscriptionEstablished_reportHandlerSelector :: Selector '[Id MTRSubscribeParams, Ptr (), Ptr ()] ()
subscribeAttributeSessionIDWithParams_subscriptionEstablished_reportHandlerSelector = mkSelector "subscribeAttributeSessionIDWithParams:subscriptionEstablished:reportHandler:"

-- | @Selector@ for @readAttributeSessionIDWithClusterStateCache:endpoint:queue:completion:@
readAttributeSessionIDWithClusterStateCache_endpoint_queue_completionSelector :: Selector '[Id MTRClusterStateCacheContainer, Id NSNumber, Id NSObject, Ptr ()] ()
readAttributeSessionIDWithClusterStateCache_endpoint_queue_completionSelector = mkSelector "readAttributeSessionIDWithClusterStateCache:endpoint:queue:completion:"

-- | @Selector@ for @readAttributeSessionDurationWithCompletion:@
readAttributeSessionDurationWithCompletionSelector :: Selector '[Ptr ()] ()
readAttributeSessionDurationWithCompletionSelector = mkSelector "readAttributeSessionDurationWithCompletion:"

-- | @Selector@ for @subscribeAttributeSessionDurationWithParams:subscriptionEstablished:reportHandler:@
subscribeAttributeSessionDurationWithParams_subscriptionEstablished_reportHandlerSelector :: Selector '[Id MTRSubscribeParams, Ptr (), Ptr ()] ()
subscribeAttributeSessionDurationWithParams_subscriptionEstablished_reportHandlerSelector = mkSelector "subscribeAttributeSessionDurationWithParams:subscriptionEstablished:reportHandler:"

-- | @Selector@ for @readAttributeSessionDurationWithClusterStateCache:endpoint:queue:completion:@
readAttributeSessionDurationWithClusterStateCache_endpoint_queue_completionSelector :: Selector '[Id MTRClusterStateCacheContainer, Id NSNumber, Id NSObject, Ptr ()] ()
readAttributeSessionDurationWithClusterStateCache_endpoint_queue_completionSelector = mkSelector "readAttributeSessionDurationWithClusterStateCache:endpoint:queue:completion:"

-- | @Selector@ for @readAttributeSessionEnergyChargedWithCompletion:@
readAttributeSessionEnergyChargedWithCompletionSelector :: Selector '[Ptr ()] ()
readAttributeSessionEnergyChargedWithCompletionSelector = mkSelector "readAttributeSessionEnergyChargedWithCompletion:"

-- | @Selector@ for @subscribeAttributeSessionEnergyChargedWithParams:subscriptionEstablished:reportHandler:@
subscribeAttributeSessionEnergyChargedWithParams_subscriptionEstablished_reportHandlerSelector :: Selector '[Id MTRSubscribeParams, Ptr (), Ptr ()] ()
subscribeAttributeSessionEnergyChargedWithParams_subscriptionEstablished_reportHandlerSelector = mkSelector "subscribeAttributeSessionEnergyChargedWithParams:subscriptionEstablished:reportHandler:"

-- | @Selector@ for @readAttributeSessionEnergyChargedWithClusterStateCache:endpoint:queue:completion:@
readAttributeSessionEnergyChargedWithClusterStateCache_endpoint_queue_completionSelector :: Selector '[Id MTRClusterStateCacheContainer, Id NSNumber, Id NSObject, Ptr ()] ()
readAttributeSessionEnergyChargedWithClusterStateCache_endpoint_queue_completionSelector = mkSelector "readAttributeSessionEnergyChargedWithClusterStateCache:endpoint:queue:completion:"

-- | @Selector@ for @readAttributeSessionEnergyDischargedWithCompletion:@
readAttributeSessionEnergyDischargedWithCompletionSelector :: Selector '[Ptr ()] ()
readAttributeSessionEnergyDischargedWithCompletionSelector = mkSelector "readAttributeSessionEnergyDischargedWithCompletion:"

-- | @Selector@ for @subscribeAttributeSessionEnergyDischargedWithParams:subscriptionEstablished:reportHandler:@
subscribeAttributeSessionEnergyDischargedWithParams_subscriptionEstablished_reportHandlerSelector :: Selector '[Id MTRSubscribeParams, Ptr (), Ptr ()] ()
subscribeAttributeSessionEnergyDischargedWithParams_subscriptionEstablished_reportHandlerSelector = mkSelector "subscribeAttributeSessionEnergyDischargedWithParams:subscriptionEstablished:reportHandler:"

-- | @Selector@ for @readAttributeSessionEnergyDischargedWithClusterStateCache:endpoint:queue:completion:@
readAttributeSessionEnergyDischargedWithClusterStateCache_endpoint_queue_completionSelector :: Selector '[Id MTRClusterStateCacheContainer, Id NSNumber, Id NSObject, Ptr ()] ()
readAttributeSessionEnergyDischargedWithClusterStateCache_endpoint_queue_completionSelector = mkSelector "readAttributeSessionEnergyDischargedWithClusterStateCache:endpoint:queue:completion:"

-- | @Selector@ for @readAttributeGeneratedCommandListWithCompletion:@
readAttributeGeneratedCommandListWithCompletionSelector :: Selector '[Ptr ()] ()
readAttributeGeneratedCommandListWithCompletionSelector = mkSelector "readAttributeGeneratedCommandListWithCompletion:"

-- | @Selector@ for @subscribeAttributeGeneratedCommandListWithParams:subscriptionEstablished:reportHandler:@
subscribeAttributeGeneratedCommandListWithParams_subscriptionEstablished_reportHandlerSelector :: Selector '[Id MTRSubscribeParams, Ptr (), Ptr ()] ()
subscribeAttributeGeneratedCommandListWithParams_subscriptionEstablished_reportHandlerSelector = mkSelector "subscribeAttributeGeneratedCommandListWithParams:subscriptionEstablished:reportHandler:"

-- | @Selector@ for @readAttributeGeneratedCommandListWithClusterStateCache:endpoint:queue:completion:@
readAttributeGeneratedCommandListWithClusterStateCache_endpoint_queue_completionSelector :: Selector '[Id MTRClusterStateCacheContainer, Id NSNumber, Id NSObject, Ptr ()] ()
readAttributeGeneratedCommandListWithClusterStateCache_endpoint_queue_completionSelector = mkSelector "readAttributeGeneratedCommandListWithClusterStateCache:endpoint:queue:completion:"

-- | @Selector@ for @readAttributeAcceptedCommandListWithCompletion:@
readAttributeAcceptedCommandListWithCompletionSelector :: Selector '[Ptr ()] ()
readAttributeAcceptedCommandListWithCompletionSelector = mkSelector "readAttributeAcceptedCommandListWithCompletion:"

-- | @Selector@ for @subscribeAttributeAcceptedCommandListWithParams:subscriptionEstablished:reportHandler:@
subscribeAttributeAcceptedCommandListWithParams_subscriptionEstablished_reportHandlerSelector :: Selector '[Id MTRSubscribeParams, Ptr (), Ptr ()] ()
subscribeAttributeAcceptedCommandListWithParams_subscriptionEstablished_reportHandlerSelector = mkSelector "subscribeAttributeAcceptedCommandListWithParams:subscriptionEstablished:reportHandler:"

-- | @Selector@ for @readAttributeAcceptedCommandListWithClusterStateCache:endpoint:queue:completion:@
readAttributeAcceptedCommandListWithClusterStateCache_endpoint_queue_completionSelector :: Selector '[Id MTRClusterStateCacheContainer, Id NSNumber, Id NSObject, Ptr ()] ()
readAttributeAcceptedCommandListWithClusterStateCache_endpoint_queue_completionSelector = mkSelector "readAttributeAcceptedCommandListWithClusterStateCache:endpoint:queue:completion:"

-- | @Selector@ for @readAttributeAttributeListWithCompletion:@
readAttributeAttributeListWithCompletionSelector :: Selector '[Ptr ()] ()
readAttributeAttributeListWithCompletionSelector = mkSelector "readAttributeAttributeListWithCompletion:"

-- | @Selector@ for @subscribeAttributeAttributeListWithParams:subscriptionEstablished:reportHandler:@
subscribeAttributeAttributeListWithParams_subscriptionEstablished_reportHandlerSelector :: Selector '[Id MTRSubscribeParams, Ptr (), Ptr ()] ()
subscribeAttributeAttributeListWithParams_subscriptionEstablished_reportHandlerSelector = mkSelector "subscribeAttributeAttributeListWithParams:subscriptionEstablished:reportHandler:"

-- | @Selector@ for @readAttributeAttributeListWithClusterStateCache:endpoint:queue:completion:@
readAttributeAttributeListWithClusterStateCache_endpoint_queue_completionSelector :: Selector '[Id MTRClusterStateCacheContainer, Id NSNumber, Id NSObject, Ptr ()] ()
readAttributeAttributeListWithClusterStateCache_endpoint_queue_completionSelector = mkSelector "readAttributeAttributeListWithClusterStateCache:endpoint:queue:completion:"

-- | @Selector@ for @readAttributeFeatureMapWithCompletion:@
readAttributeFeatureMapWithCompletionSelector :: Selector '[Ptr ()] ()
readAttributeFeatureMapWithCompletionSelector = mkSelector "readAttributeFeatureMapWithCompletion:"

-- | @Selector@ for @subscribeAttributeFeatureMapWithParams:subscriptionEstablished:reportHandler:@
subscribeAttributeFeatureMapWithParams_subscriptionEstablished_reportHandlerSelector :: Selector '[Id MTRSubscribeParams, Ptr (), Ptr ()] ()
subscribeAttributeFeatureMapWithParams_subscriptionEstablished_reportHandlerSelector = mkSelector "subscribeAttributeFeatureMapWithParams:subscriptionEstablished:reportHandler:"

-- | @Selector@ for @readAttributeFeatureMapWithClusterStateCache:endpoint:queue:completion:@
readAttributeFeatureMapWithClusterStateCache_endpoint_queue_completionSelector :: Selector '[Id MTRClusterStateCacheContainer, Id NSNumber, Id NSObject, Ptr ()] ()
readAttributeFeatureMapWithClusterStateCache_endpoint_queue_completionSelector = mkSelector "readAttributeFeatureMapWithClusterStateCache:endpoint:queue:completion:"

-- | @Selector@ for @readAttributeClusterRevisionWithCompletion:@
readAttributeClusterRevisionWithCompletionSelector :: Selector '[Ptr ()] ()
readAttributeClusterRevisionWithCompletionSelector = mkSelector "readAttributeClusterRevisionWithCompletion:"

-- | @Selector@ for @subscribeAttributeClusterRevisionWithParams:subscriptionEstablished:reportHandler:@
subscribeAttributeClusterRevisionWithParams_subscriptionEstablished_reportHandlerSelector :: Selector '[Id MTRSubscribeParams, Ptr (), Ptr ()] ()
subscribeAttributeClusterRevisionWithParams_subscriptionEstablished_reportHandlerSelector = mkSelector "subscribeAttributeClusterRevisionWithParams:subscriptionEstablished:reportHandler:"

-- | @Selector@ for @readAttributeClusterRevisionWithClusterStateCache:endpoint:queue:completion:@
readAttributeClusterRevisionWithClusterStateCache_endpoint_queue_completionSelector :: Selector '[Id MTRClusterStateCacheContainer, Id NSNumber, Id NSObject, Ptr ()] ()
readAttributeClusterRevisionWithClusterStateCache_endpoint_queue_completionSelector = mkSelector "readAttributeClusterRevisionWithClusterStateCache:endpoint:queue:completion:"

-- | @Selector@ for @init@
initSelector :: Selector '[] (Id MTRBaseClusterEnergyEVSE)
initSelector = mkSelector "init"

-- | @Selector@ for @new@
newSelector :: Selector '[] (Id MTRBaseClusterEnergyEVSE)
newSelector = mkSelector "new"

-- | @Selector@ for @initWithDevice:endpointID:queue:@
initWithDevice_endpointID_queueSelector :: Selector '[Id MTRBaseDevice, Id NSNumber, Id NSObject] (Id MTRBaseClusterEnergyEVSE)
initWithDevice_endpointID_queueSelector = mkSelector "initWithDevice:endpointID:queue:"

