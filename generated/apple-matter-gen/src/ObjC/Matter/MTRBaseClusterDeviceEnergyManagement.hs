{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Cluster Device Energy Management
--
-- This cluster allows a client to manage the power draw of a device. An example of such a client could be an Energy Management System (EMS) which controls an Energy Smart Appliance (ESA).
--
-- Generated bindings for @MTRBaseClusterDeviceEnergyManagement@.
module ObjC.Matter.MTRBaseClusterDeviceEnergyManagement
  ( MTRBaseClusterDeviceEnergyManagement
  , IsMTRBaseClusterDeviceEnergyManagement(..)
  , powerAdjustRequestWithParams_completion
  , cancelPowerAdjustRequestWithParams_completion
  , cancelPowerAdjustRequestWithCompletion
  , startTimeAdjustRequestWithParams_completion
  , pauseRequestWithParams_completion
  , resumeRequestWithParams_completion
  , resumeRequestWithCompletion
  , modifyForecastRequestWithParams_completion
  , requestConstraintBasedForecastWithParams_completion
  , cancelRequestWithParams_completion
  , cancelRequestWithCompletion
  , readAttributeESATypeWithCompletion
  , subscribeAttributeESATypeWithParams_subscriptionEstablished_reportHandler
  , readAttributeESATypeWithClusterStateCache_endpoint_queue_completion
  , readAttributeESACanGenerateWithCompletion
  , subscribeAttributeESACanGenerateWithParams_subscriptionEstablished_reportHandler
  , readAttributeESACanGenerateWithClusterStateCache_endpoint_queue_completion
  , readAttributeESAStateWithCompletion
  , subscribeAttributeESAStateWithParams_subscriptionEstablished_reportHandler
  , readAttributeESAStateWithClusterStateCache_endpoint_queue_completion
  , readAttributeAbsMinPowerWithCompletion
  , subscribeAttributeAbsMinPowerWithParams_subscriptionEstablished_reportHandler
  , readAttributeAbsMinPowerWithClusterStateCache_endpoint_queue_completion
  , readAttributeAbsMaxPowerWithCompletion
  , subscribeAttributeAbsMaxPowerWithParams_subscriptionEstablished_reportHandler
  , readAttributeAbsMaxPowerWithClusterStateCache_endpoint_queue_completion
  , readAttributePowerAdjustmentCapabilityWithCompletion
  , subscribeAttributePowerAdjustmentCapabilityWithParams_subscriptionEstablished_reportHandler
  , readAttributePowerAdjustmentCapabilityWithClusterStateCache_endpoint_queue_completion
  , readAttributeForecastWithCompletion
  , subscribeAttributeForecastWithParams_subscriptionEstablished_reportHandler
  , readAttributeForecastWithClusterStateCache_endpoint_queue_completion
  , readAttributeOptOutStateWithCompletion
  , subscribeAttributeOptOutStateWithParams_subscriptionEstablished_reportHandler
  , readAttributeOptOutStateWithClusterStateCache_endpoint_queue_completion
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
  , cancelPowerAdjustRequestWithCompletionSelector
  , cancelPowerAdjustRequestWithParams_completionSelector
  , cancelRequestWithCompletionSelector
  , cancelRequestWithParams_completionSelector
  , initSelector
  , initWithDevice_endpointID_queueSelector
  , modifyForecastRequestWithParams_completionSelector
  , newSelector
  , pauseRequestWithParams_completionSelector
  , powerAdjustRequestWithParams_completionSelector
  , readAttributeAbsMaxPowerWithClusterStateCache_endpoint_queue_completionSelector
  , readAttributeAbsMaxPowerWithCompletionSelector
  , readAttributeAbsMinPowerWithClusterStateCache_endpoint_queue_completionSelector
  , readAttributeAbsMinPowerWithCompletionSelector
  , readAttributeAcceptedCommandListWithClusterStateCache_endpoint_queue_completionSelector
  , readAttributeAcceptedCommandListWithCompletionSelector
  , readAttributeAttributeListWithClusterStateCache_endpoint_queue_completionSelector
  , readAttributeAttributeListWithCompletionSelector
  , readAttributeClusterRevisionWithClusterStateCache_endpoint_queue_completionSelector
  , readAttributeClusterRevisionWithCompletionSelector
  , readAttributeESACanGenerateWithClusterStateCache_endpoint_queue_completionSelector
  , readAttributeESACanGenerateWithCompletionSelector
  , readAttributeESAStateWithClusterStateCache_endpoint_queue_completionSelector
  , readAttributeESAStateWithCompletionSelector
  , readAttributeESATypeWithClusterStateCache_endpoint_queue_completionSelector
  , readAttributeESATypeWithCompletionSelector
  , readAttributeFeatureMapWithClusterStateCache_endpoint_queue_completionSelector
  , readAttributeFeatureMapWithCompletionSelector
  , readAttributeForecastWithClusterStateCache_endpoint_queue_completionSelector
  , readAttributeForecastWithCompletionSelector
  , readAttributeGeneratedCommandListWithClusterStateCache_endpoint_queue_completionSelector
  , readAttributeGeneratedCommandListWithCompletionSelector
  , readAttributeOptOutStateWithClusterStateCache_endpoint_queue_completionSelector
  , readAttributeOptOutStateWithCompletionSelector
  , readAttributePowerAdjustmentCapabilityWithClusterStateCache_endpoint_queue_completionSelector
  , readAttributePowerAdjustmentCapabilityWithCompletionSelector
  , requestConstraintBasedForecastWithParams_completionSelector
  , resumeRequestWithCompletionSelector
  , resumeRequestWithParams_completionSelector
  , startTimeAdjustRequestWithParams_completionSelector
  , subscribeAttributeAbsMaxPowerWithParams_subscriptionEstablished_reportHandlerSelector
  , subscribeAttributeAbsMinPowerWithParams_subscriptionEstablished_reportHandlerSelector
  , subscribeAttributeAcceptedCommandListWithParams_subscriptionEstablished_reportHandlerSelector
  , subscribeAttributeAttributeListWithParams_subscriptionEstablished_reportHandlerSelector
  , subscribeAttributeClusterRevisionWithParams_subscriptionEstablished_reportHandlerSelector
  , subscribeAttributeESACanGenerateWithParams_subscriptionEstablished_reportHandlerSelector
  , subscribeAttributeESAStateWithParams_subscriptionEstablished_reportHandlerSelector
  , subscribeAttributeESATypeWithParams_subscriptionEstablished_reportHandlerSelector
  , subscribeAttributeFeatureMapWithParams_subscriptionEstablished_reportHandlerSelector
  , subscribeAttributeForecastWithParams_subscriptionEstablished_reportHandlerSelector
  , subscribeAttributeGeneratedCommandListWithParams_subscriptionEstablished_reportHandlerSelector
  , subscribeAttributeOptOutStateWithParams_subscriptionEstablished_reportHandlerSelector
  , subscribeAttributePowerAdjustmentCapabilityWithParams_subscriptionEstablished_reportHandlerSelector


  ) where

import Foreign.Ptr (Ptr, FunPtr)
import Foreign.C.Types

import ObjC.Runtime.Types
import ObjC.Runtime.Message (sendMessage, sendOwnedMessage, sendClassMessage, sendOwnedClassMessage)
import ObjC.Runtime.Selector (mkSelector)
import ObjC.Runtime.Class (getRequiredClass)

import ObjC.Matter.Internal.Classes
import ObjC.Foundation.Internal.Classes

-- | Command PowerAdjustRequest
--
-- Allows a client to request an adjustment in the power consumption of an ESA for a specified duration.
--
-- ObjC selector: @- powerAdjustRequestWithParams:completion:@
powerAdjustRequestWithParams_completion :: (IsMTRBaseClusterDeviceEnergyManagement mtrBaseClusterDeviceEnergyManagement, IsMTRDeviceEnergyManagementClusterPowerAdjustRequestParams params) => mtrBaseClusterDeviceEnergyManagement -> params -> Ptr () -> IO ()
powerAdjustRequestWithParams_completion mtrBaseClusterDeviceEnergyManagement params completion =
  sendMessage mtrBaseClusterDeviceEnergyManagement powerAdjustRequestWithParams_completionSelector (toMTRDeviceEnergyManagementClusterPowerAdjustRequestParams params) completion

-- | Command CancelPowerAdjustRequest
--
-- Allows a client to cancel an ongoing PowerAdjustmentRequest operation.
--
-- ObjC selector: @- cancelPowerAdjustRequestWithParams:completion:@
cancelPowerAdjustRequestWithParams_completion :: (IsMTRBaseClusterDeviceEnergyManagement mtrBaseClusterDeviceEnergyManagement, IsMTRDeviceEnergyManagementClusterCancelPowerAdjustRequestParams params) => mtrBaseClusterDeviceEnergyManagement -> params -> Ptr () -> IO ()
cancelPowerAdjustRequestWithParams_completion mtrBaseClusterDeviceEnergyManagement params completion =
  sendMessage mtrBaseClusterDeviceEnergyManagement cancelPowerAdjustRequestWithParams_completionSelector (toMTRDeviceEnergyManagementClusterCancelPowerAdjustRequestParams params) completion

-- | @- cancelPowerAdjustRequestWithCompletion:@
cancelPowerAdjustRequestWithCompletion :: IsMTRBaseClusterDeviceEnergyManagement mtrBaseClusterDeviceEnergyManagement => mtrBaseClusterDeviceEnergyManagement -> Ptr () -> IO ()
cancelPowerAdjustRequestWithCompletion mtrBaseClusterDeviceEnergyManagement completion =
  sendMessage mtrBaseClusterDeviceEnergyManagement cancelPowerAdjustRequestWithCompletionSelector completion

-- | Command StartTimeAdjustRequest
--
-- Allows a client to adjust the start time of a Forecast sequence that has not yet started operation (i.e. where the current Forecast StartTime is in the future).
--
-- ObjC selector: @- startTimeAdjustRequestWithParams:completion:@
startTimeAdjustRequestWithParams_completion :: (IsMTRBaseClusterDeviceEnergyManagement mtrBaseClusterDeviceEnergyManagement, IsMTRDeviceEnergyManagementClusterStartTimeAdjustRequestParams params) => mtrBaseClusterDeviceEnergyManagement -> params -> Ptr () -> IO ()
startTimeAdjustRequestWithParams_completion mtrBaseClusterDeviceEnergyManagement params completion =
  sendMessage mtrBaseClusterDeviceEnergyManagement startTimeAdjustRequestWithParams_completionSelector (toMTRDeviceEnergyManagementClusterStartTimeAdjustRequestParams params) completion

-- | Command PauseRequest
--
-- Allows a client to temporarily pause an operation and reduce the ESAs energy demand.
--
-- ObjC selector: @- pauseRequestWithParams:completion:@
pauseRequestWithParams_completion :: (IsMTRBaseClusterDeviceEnergyManagement mtrBaseClusterDeviceEnergyManagement, IsMTRDeviceEnergyManagementClusterPauseRequestParams params) => mtrBaseClusterDeviceEnergyManagement -> params -> Ptr () -> IO ()
pauseRequestWithParams_completion mtrBaseClusterDeviceEnergyManagement params completion =
  sendMessage mtrBaseClusterDeviceEnergyManagement pauseRequestWithParams_completionSelector (toMTRDeviceEnergyManagementClusterPauseRequestParams params) completion

-- | Command ResumeRequest
--
-- Allows a client to cancel the PauseRequest command and enable earlier resumption of operation.
--
-- ObjC selector: @- resumeRequestWithParams:completion:@
resumeRequestWithParams_completion :: (IsMTRBaseClusterDeviceEnergyManagement mtrBaseClusterDeviceEnergyManagement, IsMTRDeviceEnergyManagementClusterResumeRequestParams params) => mtrBaseClusterDeviceEnergyManagement -> params -> Ptr () -> IO ()
resumeRequestWithParams_completion mtrBaseClusterDeviceEnergyManagement params completion =
  sendMessage mtrBaseClusterDeviceEnergyManagement resumeRequestWithParams_completionSelector (toMTRDeviceEnergyManagementClusterResumeRequestParams params) completion

-- | @- resumeRequestWithCompletion:@
resumeRequestWithCompletion :: IsMTRBaseClusterDeviceEnergyManagement mtrBaseClusterDeviceEnergyManagement => mtrBaseClusterDeviceEnergyManagement -> Ptr () -> IO ()
resumeRequestWithCompletion mtrBaseClusterDeviceEnergyManagement completion =
  sendMessage mtrBaseClusterDeviceEnergyManagement resumeRequestWithCompletionSelector completion

-- | Command ModifyForecastRequest
--
-- Allows a client to modify a Forecast within the limits allowed by the ESA.
--
-- ObjC selector: @- modifyForecastRequestWithParams:completion:@
modifyForecastRequestWithParams_completion :: (IsMTRBaseClusterDeviceEnergyManagement mtrBaseClusterDeviceEnergyManagement, IsMTRDeviceEnergyManagementClusterModifyForecastRequestParams params) => mtrBaseClusterDeviceEnergyManagement -> params -> Ptr () -> IO ()
modifyForecastRequestWithParams_completion mtrBaseClusterDeviceEnergyManagement params completion =
  sendMessage mtrBaseClusterDeviceEnergyManagement modifyForecastRequestWithParams_completionSelector (toMTRDeviceEnergyManagementClusterModifyForecastRequestParams params) completion

-- | Command RequestConstraintBasedForecast
--
-- Allows a client to ask the ESA to recompute its Forecast based on power and time constraints.
--
-- ObjC selector: @- requestConstraintBasedForecastWithParams:completion:@
requestConstraintBasedForecastWithParams_completion :: (IsMTRBaseClusterDeviceEnergyManagement mtrBaseClusterDeviceEnergyManagement, IsMTRDeviceEnergyManagementClusterRequestConstraintBasedForecastParams params) => mtrBaseClusterDeviceEnergyManagement -> params -> Ptr () -> IO ()
requestConstraintBasedForecastWithParams_completion mtrBaseClusterDeviceEnergyManagement params completion =
  sendMessage mtrBaseClusterDeviceEnergyManagement requestConstraintBasedForecastWithParams_completionSelector (toMTRDeviceEnergyManagementClusterRequestConstraintBasedForecastParams params) completion

-- | Command CancelRequest
--
-- Allows a client to request cancellation of a previous adjustment request in a StartTimeAdjustRequest, ModifyForecastRequest or RequestConstraintBasedForecast command.
--
-- ObjC selector: @- cancelRequestWithParams:completion:@
cancelRequestWithParams_completion :: (IsMTRBaseClusterDeviceEnergyManagement mtrBaseClusterDeviceEnergyManagement, IsMTRDeviceEnergyManagementClusterCancelRequestParams params) => mtrBaseClusterDeviceEnergyManagement -> params -> Ptr () -> IO ()
cancelRequestWithParams_completion mtrBaseClusterDeviceEnergyManagement params completion =
  sendMessage mtrBaseClusterDeviceEnergyManagement cancelRequestWithParams_completionSelector (toMTRDeviceEnergyManagementClusterCancelRequestParams params) completion

-- | @- cancelRequestWithCompletion:@
cancelRequestWithCompletion :: IsMTRBaseClusterDeviceEnergyManagement mtrBaseClusterDeviceEnergyManagement => mtrBaseClusterDeviceEnergyManagement -> Ptr () -> IO ()
cancelRequestWithCompletion mtrBaseClusterDeviceEnergyManagement completion =
  sendMessage mtrBaseClusterDeviceEnergyManagement cancelRequestWithCompletionSelector completion

-- | @- readAttributeESATypeWithCompletion:@
readAttributeESATypeWithCompletion :: IsMTRBaseClusterDeviceEnergyManagement mtrBaseClusterDeviceEnergyManagement => mtrBaseClusterDeviceEnergyManagement -> Ptr () -> IO ()
readAttributeESATypeWithCompletion mtrBaseClusterDeviceEnergyManagement completion =
  sendMessage mtrBaseClusterDeviceEnergyManagement readAttributeESATypeWithCompletionSelector completion

-- | @- subscribeAttributeESATypeWithParams:subscriptionEstablished:reportHandler:@
subscribeAttributeESATypeWithParams_subscriptionEstablished_reportHandler :: (IsMTRBaseClusterDeviceEnergyManagement mtrBaseClusterDeviceEnergyManagement, IsMTRSubscribeParams params) => mtrBaseClusterDeviceEnergyManagement -> params -> Ptr () -> Ptr () -> IO ()
subscribeAttributeESATypeWithParams_subscriptionEstablished_reportHandler mtrBaseClusterDeviceEnergyManagement params subscriptionEstablished reportHandler =
  sendMessage mtrBaseClusterDeviceEnergyManagement subscribeAttributeESATypeWithParams_subscriptionEstablished_reportHandlerSelector (toMTRSubscribeParams params) subscriptionEstablished reportHandler

-- | @+ readAttributeESATypeWithClusterStateCache:endpoint:queue:completion:@
readAttributeESATypeWithClusterStateCache_endpoint_queue_completion :: (IsMTRClusterStateCacheContainer clusterStateCacheContainer, IsNSNumber endpoint, IsNSObject queue) => clusterStateCacheContainer -> endpoint -> queue -> Ptr () -> IO ()
readAttributeESATypeWithClusterStateCache_endpoint_queue_completion clusterStateCacheContainer endpoint queue completion =
  do
    cls' <- getRequiredClass "MTRBaseClusterDeviceEnergyManagement"
    sendClassMessage cls' readAttributeESATypeWithClusterStateCache_endpoint_queue_completionSelector (toMTRClusterStateCacheContainer clusterStateCacheContainer) (toNSNumber endpoint) (toNSObject queue) completion

-- | @- readAttributeESACanGenerateWithCompletion:@
readAttributeESACanGenerateWithCompletion :: IsMTRBaseClusterDeviceEnergyManagement mtrBaseClusterDeviceEnergyManagement => mtrBaseClusterDeviceEnergyManagement -> Ptr () -> IO ()
readAttributeESACanGenerateWithCompletion mtrBaseClusterDeviceEnergyManagement completion =
  sendMessage mtrBaseClusterDeviceEnergyManagement readAttributeESACanGenerateWithCompletionSelector completion

-- | @- subscribeAttributeESACanGenerateWithParams:subscriptionEstablished:reportHandler:@
subscribeAttributeESACanGenerateWithParams_subscriptionEstablished_reportHandler :: (IsMTRBaseClusterDeviceEnergyManagement mtrBaseClusterDeviceEnergyManagement, IsMTRSubscribeParams params) => mtrBaseClusterDeviceEnergyManagement -> params -> Ptr () -> Ptr () -> IO ()
subscribeAttributeESACanGenerateWithParams_subscriptionEstablished_reportHandler mtrBaseClusterDeviceEnergyManagement params subscriptionEstablished reportHandler =
  sendMessage mtrBaseClusterDeviceEnergyManagement subscribeAttributeESACanGenerateWithParams_subscriptionEstablished_reportHandlerSelector (toMTRSubscribeParams params) subscriptionEstablished reportHandler

-- | @+ readAttributeESACanGenerateWithClusterStateCache:endpoint:queue:completion:@
readAttributeESACanGenerateWithClusterStateCache_endpoint_queue_completion :: (IsMTRClusterStateCacheContainer clusterStateCacheContainer, IsNSNumber endpoint, IsNSObject queue) => clusterStateCacheContainer -> endpoint -> queue -> Ptr () -> IO ()
readAttributeESACanGenerateWithClusterStateCache_endpoint_queue_completion clusterStateCacheContainer endpoint queue completion =
  do
    cls' <- getRequiredClass "MTRBaseClusterDeviceEnergyManagement"
    sendClassMessage cls' readAttributeESACanGenerateWithClusterStateCache_endpoint_queue_completionSelector (toMTRClusterStateCacheContainer clusterStateCacheContainer) (toNSNumber endpoint) (toNSObject queue) completion

-- | @- readAttributeESAStateWithCompletion:@
readAttributeESAStateWithCompletion :: IsMTRBaseClusterDeviceEnergyManagement mtrBaseClusterDeviceEnergyManagement => mtrBaseClusterDeviceEnergyManagement -> Ptr () -> IO ()
readAttributeESAStateWithCompletion mtrBaseClusterDeviceEnergyManagement completion =
  sendMessage mtrBaseClusterDeviceEnergyManagement readAttributeESAStateWithCompletionSelector completion

-- | @- subscribeAttributeESAStateWithParams:subscriptionEstablished:reportHandler:@
subscribeAttributeESAStateWithParams_subscriptionEstablished_reportHandler :: (IsMTRBaseClusterDeviceEnergyManagement mtrBaseClusterDeviceEnergyManagement, IsMTRSubscribeParams params) => mtrBaseClusterDeviceEnergyManagement -> params -> Ptr () -> Ptr () -> IO ()
subscribeAttributeESAStateWithParams_subscriptionEstablished_reportHandler mtrBaseClusterDeviceEnergyManagement params subscriptionEstablished reportHandler =
  sendMessage mtrBaseClusterDeviceEnergyManagement subscribeAttributeESAStateWithParams_subscriptionEstablished_reportHandlerSelector (toMTRSubscribeParams params) subscriptionEstablished reportHandler

-- | @+ readAttributeESAStateWithClusterStateCache:endpoint:queue:completion:@
readAttributeESAStateWithClusterStateCache_endpoint_queue_completion :: (IsMTRClusterStateCacheContainer clusterStateCacheContainer, IsNSNumber endpoint, IsNSObject queue) => clusterStateCacheContainer -> endpoint -> queue -> Ptr () -> IO ()
readAttributeESAStateWithClusterStateCache_endpoint_queue_completion clusterStateCacheContainer endpoint queue completion =
  do
    cls' <- getRequiredClass "MTRBaseClusterDeviceEnergyManagement"
    sendClassMessage cls' readAttributeESAStateWithClusterStateCache_endpoint_queue_completionSelector (toMTRClusterStateCacheContainer clusterStateCacheContainer) (toNSNumber endpoint) (toNSObject queue) completion

-- | @- readAttributeAbsMinPowerWithCompletion:@
readAttributeAbsMinPowerWithCompletion :: IsMTRBaseClusterDeviceEnergyManagement mtrBaseClusterDeviceEnergyManagement => mtrBaseClusterDeviceEnergyManagement -> Ptr () -> IO ()
readAttributeAbsMinPowerWithCompletion mtrBaseClusterDeviceEnergyManagement completion =
  sendMessage mtrBaseClusterDeviceEnergyManagement readAttributeAbsMinPowerWithCompletionSelector completion

-- | @- subscribeAttributeAbsMinPowerWithParams:subscriptionEstablished:reportHandler:@
subscribeAttributeAbsMinPowerWithParams_subscriptionEstablished_reportHandler :: (IsMTRBaseClusterDeviceEnergyManagement mtrBaseClusterDeviceEnergyManagement, IsMTRSubscribeParams params) => mtrBaseClusterDeviceEnergyManagement -> params -> Ptr () -> Ptr () -> IO ()
subscribeAttributeAbsMinPowerWithParams_subscriptionEstablished_reportHandler mtrBaseClusterDeviceEnergyManagement params subscriptionEstablished reportHandler =
  sendMessage mtrBaseClusterDeviceEnergyManagement subscribeAttributeAbsMinPowerWithParams_subscriptionEstablished_reportHandlerSelector (toMTRSubscribeParams params) subscriptionEstablished reportHandler

-- | @+ readAttributeAbsMinPowerWithClusterStateCache:endpoint:queue:completion:@
readAttributeAbsMinPowerWithClusterStateCache_endpoint_queue_completion :: (IsMTRClusterStateCacheContainer clusterStateCacheContainer, IsNSNumber endpoint, IsNSObject queue) => clusterStateCacheContainer -> endpoint -> queue -> Ptr () -> IO ()
readAttributeAbsMinPowerWithClusterStateCache_endpoint_queue_completion clusterStateCacheContainer endpoint queue completion =
  do
    cls' <- getRequiredClass "MTRBaseClusterDeviceEnergyManagement"
    sendClassMessage cls' readAttributeAbsMinPowerWithClusterStateCache_endpoint_queue_completionSelector (toMTRClusterStateCacheContainer clusterStateCacheContainer) (toNSNumber endpoint) (toNSObject queue) completion

-- | @- readAttributeAbsMaxPowerWithCompletion:@
readAttributeAbsMaxPowerWithCompletion :: IsMTRBaseClusterDeviceEnergyManagement mtrBaseClusterDeviceEnergyManagement => mtrBaseClusterDeviceEnergyManagement -> Ptr () -> IO ()
readAttributeAbsMaxPowerWithCompletion mtrBaseClusterDeviceEnergyManagement completion =
  sendMessage mtrBaseClusterDeviceEnergyManagement readAttributeAbsMaxPowerWithCompletionSelector completion

-- | @- subscribeAttributeAbsMaxPowerWithParams:subscriptionEstablished:reportHandler:@
subscribeAttributeAbsMaxPowerWithParams_subscriptionEstablished_reportHandler :: (IsMTRBaseClusterDeviceEnergyManagement mtrBaseClusterDeviceEnergyManagement, IsMTRSubscribeParams params) => mtrBaseClusterDeviceEnergyManagement -> params -> Ptr () -> Ptr () -> IO ()
subscribeAttributeAbsMaxPowerWithParams_subscriptionEstablished_reportHandler mtrBaseClusterDeviceEnergyManagement params subscriptionEstablished reportHandler =
  sendMessage mtrBaseClusterDeviceEnergyManagement subscribeAttributeAbsMaxPowerWithParams_subscriptionEstablished_reportHandlerSelector (toMTRSubscribeParams params) subscriptionEstablished reportHandler

-- | @+ readAttributeAbsMaxPowerWithClusterStateCache:endpoint:queue:completion:@
readAttributeAbsMaxPowerWithClusterStateCache_endpoint_queue_completion :: (IsMTRClusterStateCacheContainer clusterStateCacheContainer, IsNSNumber endpoint, IsNSObject queue) => clusterStateCacheContainer -> endpoint -> queue -> Ptr () -> IO ()
readAttributeAbsMaxPowerWithClusterStateCache_endpoint_queue_completion clusterStateCacheContainer endpoint queue completion =
  do
    cls' <- getRequiredClass "MTRBaseClusterDeviceEnergyManagement"
    sendClassMessage cls' readAttributeAbsMaxPowerWithClusterStateCache_endpoint_queue_completionSelector (toMTRClusterStateCacheContainer clusterStateCacheContainer) (toNSNumber endpoint) (toNSObject queue) completion

-- | @- readAttributePowerAdjustmentCapabilityWithCompletion:@
readAttributePowerAdjustmentCapabilityWithCompletion :: IsMTRBaseClusterDeviceEnergyManagement mtrBaseClusterDeviceEnergyManagement => mtrBaseClusterDeviceEnergyManagement -> Ptr () -> IO ()
readAttributePowerAdjustmentCapabilityWithCompletion mtrBaseClusterDeviceEnergyManagement completion =
  sendMessage mtrBaseClusterDeviceEnergyManagement readAttributePowerAdjustmentCapabilityWithCompletionSelector completion

-- | @- subscribeAttributePowerAdjustmentCapabilityWithParams:subscriptionEstablished:reportHandler:@
subscribeAttributePowerAdjustmentCapabilityWithParams_subscriptionEstablished_reportHandler :: (IsMTRBaseClusterDeviceEnergyManagement mtrBaseClusterDeviceEnergyManagement, IsMTRSubscribeParams params) => mtrBaseClusterDeviceEnergyManagement -> params -> Ptr () -> Ptr () -> IO ()
subscribeAttributePowerAdjustmentCapabilityWithParams_subscriptionEstablished_reportHandler mtrBaseClusterDeviceEnergyManagement params subscriptionEstablished reportHandler =
  sendMessage mtrBaseClusterDeviceEnergyManagement subscribeAttributePowerAdjustmentCapabilityWithParams_subscriptionEstablished_reportHandlerSelector (toMTRSubscribeParams params) subscriptionEstablished reportHandler

-- | @+ readAttributePowerAdjustmentCapabilityWithClusterStateCache:endpoint:queue:completion:@
readAttributePowerAdjustmentCapabilityWithClusterStateCache_endpoint_queue_completion :: (IsMTRClusterStateCacheContainer clusterStateCacheContainer, IsNSNumber endpoint, IsNSObject queue) => clusterStateCacheContainer -> endpoint -> queue -> Ptr () -> IO ()
readAttributePowerAdjustmentCapabilityWithClusterStateCache_endpoint_queue_completion clusterStateCacheContainer endpoint queue completion =
  do
    cls' <- getRequiredClass "MTRBaseClusterDeviceEnergyManagement"
    sendClassMessage cls' readAttributePowerAdjustmentCapabilityWithClusterStateCache_endpoint_queue_completionSelector (toMTRClusterStateCacheContainer clusterStateCacheContainer) (toNSNumber endpoint) (toNSObject queue) completion

-- | @- readAttributeForecastWithCompletion:@
readAttributeForecastWithCompletion :: IsMTRBaseClusterDeviceEnergyManagement mtrBaseClusterDeviceEnergyManagement => mtrBaseClusterDeviceEnergyManagement -> Ptr () -> IO ()
readAttributeForecastWithCompletion mtrBaseClusterDeviceEnergyManagement completion =
  sendMessage mtrBaseClusterDeviceEnergyManagement readAttributeForecastWithCompletionSelector completion

-- | @- subscribeAttributeForecastWithParams:subscriptionEstablished:reportHandler:@
subscribeAttributeForecastWithParams_subscriptionEstablished_reportHandler :: (IsMTRBaseClusterDeviceEnergyManagement mtrBaseClusterDeviceEnergyManagement, IsMTRSubscribeParams params) => mtrBaseClusterDeviceEnergyManagement -> params -> Ptr () -> Ptr () -> IO ()
subscribeAttributeForecastWithParams_subscriptionEstablished_reportHandler mtrBaseClusterDeviceEnergyManagement params subscriptionEstablished reportHandler =
  sendMessage mtrBaseClusterDeviceEnergyManagement subscribeAttributeForecastWithParams_subscriptionEstablished_reportHandlerSelector (toMTRSubscribeParams params) subscriptionEstablished reportHandler

-- | @+ readAttributeForecastWithClusterStateCache:endpoint:queue:completion:@
readAttributeForecastWithClusterStateCache_endpoint_queue_completion :: (IsMTRClusterStateCacheContainer clusterStateCacheContainer, IsNSNumber endpoint, IsNSObject queue) => clusterStateCacheContainer -> endpoint -> queue -> Ptr () -> IO ()
readAttributeForecastWithClusterStateCache_endpoint_queue_completion clusterStateCacheContainer endpoint queue completion =
  do
    cls' <- getRequiredClass "MTRBaseClusterDeviceEnergyManagement"
    sendClassMessage cls' readAttributeForecastWithClusterStateCache_endpoint_queue_completionSelector (toMTRClusterStateCacheContainer clusterStateCacheContainer) (toNSNumber endpoint) (toNSObject queue) completion

-- | @- readAttributeOptOutStateWithCompletion:@
readAttributeOptOutStateWithCompletion :: IsMTRBaseClusterDeviceEnergyManagement mtrBaseClusterDeviceEnergyManagement => mtrBaseClusterDeviceEnergyManagement -> Ptr () -> IO ()
readAttributeOptOutStateWithCompletion mtrBaseClusterDeviceEnergyManagement completion =
  sendMessage mtrBaseClusterDeviceEnergyManagement readAttributeOptOutStateWithCompletionSelector completion

-- | @- subscribeAttributeOptOutStateWithParams:subscriptionEstablished:reportHandler:@
subscribeAttributeOptOutStateWithParams_subscriptionEstablished_reportHandler :: (IsMTRBaseClusterDeviceEnergyManagement mtrBaseClusterDeviceEnergyManagement, IsMTRSubscribeParams params) => mtrBaseClusterDeviceEnergyManagement -> params -> Ptr () -> Ptr () -> IO ()
subscribeAttributeOptOutStateWithParams_subscriptionEstablished_reportHandler mtrBaseClusterDeviceEnergyManagement params subscriptionEstablished reportHandler =
  sendMessage mtrBaseClusterDeviceEnergyManagement subscribeAttributeOptOutStateWithParams_subscriptionEstablished_reportHandlerSelector (toMTRSubscribeParams params) subscriptionEstablished reportHandler

-- | @+ readAttributeOptOutStateWithClusterStateCache:endpoint:queue:completion:@
readAttributeOptOutStateWithClusterStateCache_endpoint_queue_completion :: (IsMTRClusterStateCacheContainer clusterStateCacheContainer, IsNSNumber endpoint, IsNSObject queue) => clusterStateCacheContainer -> endpoint -> queue -> Ptr () -> IO ()
readAttributeOptOutStateWithClusterStateCache_endpoint_queue_completion clusterStateCacheContainer endpoint queue completion =
  do
    cls' <- getRequiredClass "MTRBaseClusterDeviceEnergyManagement"
    sendClassMessage cls' readAttributeOptOutStateWithClusterStateCache_endpoint_queue_completionSelector (toMTRClusterStateCacheContainer clusterStateCacheContainer) (toNSNumber endpoint) (toNSObject queue) completion

-- | @- readAttributeGeneratedCommandListWithCompletion:@
readAttributeGeneratedCommandListWithCompletion :: IsMTRBaseClusterDeviceEnergyManagement mtrBaseClusterDeviceEnergyManagement => mtrBaseClusterDeviceEnergyManagement -> Ptr () -> IO ()
readAttributeGeneratedCommandListWithCompletion mtrBaseClusterDeviceEnergyManagement completion =
  sendMessage mtrBaseClusterDeviceEnergyManagement readAttributeGeneratedCommandListWithCompletionSelector completion

-- | @- subscribeAttributeGeneratedCommandListWithParams:subscriptionEstablished:reportHandler:@
subscribeAttributeGeneratedCommandListWithParams_subscriptionEstablished_reportHandler :: (IsMTRBaseClusterDeviceEnergyManagement mtrBaseClusterDeviceEnergyManagement, IsMTRSubscribeParams params) => mtrBaseClusterDeviceEnergyManagement -> params -> Ptr () -> Ptr () -> IO ()
subscribeAttributeGeneratedCommandListWithParams_subscriptionEstablished_reportHandler mtrBaseClusterDeviceEnergyManagement params subscriptionEstablished reportHandler =
  sendMessage mtrBaseClusterDeviceEnergyManagement subscribeAttributeGeneratedCommandListWithParams_subscriptionEstablished_reportHandlerSelector (toMTRSubscribeParams params) subscriptionEstablished reportHandler

-- | @+ readAttributeGeneratedCommandListWithClusterStateCache:endpoint:queue:completion:@
readAttributeGeneratedCommandListWithClusterStateCache_endpoint_queue_completion :: (IsMTRClusterStateCacheContainer clusterStateCacheContainer, IsNSNumber endpoint, IsNSObject queue) => clusterStateCacheContainer -> endpoint -> queue -> Ptr () -> IO ()
readAttributeGeneratedCommandListWithClusterStateCache_endpoint_queue_completion clusterStateCacheContainer endpoint queue completion =
  do
    cls' <- getRequiredClass "MTRBaseClusterDeviceEnergyManagement"
    sendClassMessage cls' readAttributeGeneratedCommandListWithClusterStateCache_endpoint_queue_completionSelector (toMTRClusterStateCacheContainer clusterStateCacheContainer) (toNSNumber endpoint) (toNSObject queue) completion

-- | @- readAttributeAcceptedCommandListWithCompletion:@
readAttributeAcceptedCommandListWithCompletion :: IsMTRBaseClusterDeviceEnergyManagement mtrBaseClusterDeviceEnergyManagement => mtrBaseClusterDeviceEnergyManagement -> Ptr () -> IO ()
readAttributeAcceptedCommandListWithCompletion mtrBaseClusterDeviceEnergyManagement completion =
  sendMessage mtrBaseClusterDeviceEnergyManagement readAttributeAcceptedCommandListWithCompletionSelector completion

-- | @- subscribeAttributeAcceptedCommandListWithParams:subscriptionEstablished:reportHandler:@
subscribeAttributeAcceptedCommandListWithParams_subscriptionEstablished_reportHandler :: (IsMTRBaseClusterDeviceEnergyManagement mtrBaseClusterDeviceEnergyManagement, IsMTRSubscribeParams params) => mtrBaseClusterDeviceEnergyManagement -> params -> Ptr () -> Ptr () -> IO ()
subscribeAttributeAcceptedCommandListWithParams_subscriptionEstablished_reportHandler mtrBaseClusterDeviceEnergyManagement params subscriptionEstablished reportHandler =
  sendMessage mtrBaseClusterDeviceEnergyManagement subscribeAttributeAcceptedCommandListWithParams_subscriptionEstablished_reportHandlerSelector (toMTRSubscribeParams params) subscriptionEstablished reportHandler

-- | @+ readAttributeAcceptedCommandListWithClusterStateCache:endpoint:queue:completion:@
readAttributeAcceptedCommandListWithClusterStateCache_endpoint_queue_completion :: (IsMTRClusterStateCacheContainer clusterStateCacheContainer, IsNSNumber endpoint, IsNSObject queue) => clusterStateCacheContainer -> endpoint -> queue -> Ptr () -> IO ()
readAttributeAcceptedCommandListWithClusterStateCache_endpoint_queue_completion clusterStateCacheContainer endpoint queue completion =
  do
    cls' <- getRequiredClass "MTRBaseClusterDeviceEnergyManagement"
    sendClassMessage cls' readAttributeAcceptedCommandListWithClusterStateCache_endpoint_queue_completionSelector (toMTRClusterStateCacheContainer clusterStateCacheContainer) (toNSNumber endpoint) (toNSObject queue) completion

-- | @- readAttributeAttributeListWithCompletion:@
readAttributeAttributeListWithCompletion :: IsMTRBaseClusterDeviceEnergyManagement mtrBaseClusterDeviceEnergyManagement => mtrBaseClusterDeviceEnergyManagement -> Ptr () -> IO ()
readAttributeAttributeListWithCompletion mtrBaseClusterDeviceEnergyManagement completion =
  sendMessage mtrBaseClusterDeviceEnergyManagement readAttributeAttributeListWithCompletionSelector completion

-- | @- subscribeAttributeAttributeListWithParams:subscriptionEstablished:reportHandler:@
subscribeAttributeAttributeListWithParams_subscriptionEstablished_reportHandler :: (IsMTRBaseClusterDeviceEnergyManagement mtrBaseClusterDeviceEnergyManagement, IsMTRSubscribeParams params) => mtrBaseClusterDeviceEnergyManagement -> params -> Ptr () -> Ptr () -> IO ()
subscribeAttributeAttributeListWithParams_subscriptionEstablished_reportHandler mtrBaseClusterDeviceEnergyManagement params subscriptionEstablished reportHandler =
  sendMessage mtrBaseClusterDeviceEnergyManagement subscribeAttributeAttributeListWithParams_subscriptionEstablished_reportHandlerSelector (toMTRSubscribeParams params) subscriptionEstablished reportHandler

-- | @+ readAttributeAttributeListWithClusterStateCache:endpoint:queue:completion:@
readAttributeAttributeListWithClusterStateCache_endpoint_queue_completion :: (IsMTRClusterStateCacheContainer clusterStateCacheContainer, IsNSNumber endpoint, IsNSObject queue) => clusterStateCacheContainer -> endpoint -> queue -> Ptr () -> IO ()
readAttributeAttributeListWithClusterStateCache_endpoint_queue_completion clusterStateCacheContainer endpoint queue completion =
  do
    cls' <- getRequiredClass "MTRBaseClusterDeviceEnergyManagement"
    sendClassMessage cls' readAttributeAttributeListWithClusterStateCache_endpoint_queue_completionSelector (toMTRClusterStateCacheContainer clusterStateCacheContainer) (toNSNumber endpoint) (toNSObject queue) completion

-- | @- readAttributeFeatureMapWithCompletion:@
readAttributeFeatureMapWithCompletion :: IsMTRBaseClusterDeviceEnergyManagement mtrBaseClusterDeviceEnergyManagement => mtrBaseClusterDeviceEnergyManagement -> Ptr () -> IO ()
readAttributeFeatureMapWithCompletion mtrBaseClusterDeviceEnergyManagement completion =
  sendMessage mtrBaseClusterDeviceEnergyManagement readAttributeFeatureMapWithCompletionSelector completion

-- | @- subscribeAttributeFeatureMapWithParams:subscriptionEstablished:reportHandler:@
subscribeAttributeFeatureMapWithParams_subscriptionEstablished_reportHandler :: (IsMTRBaseClusterDeviceEnergyManagement mtrBaseClusterDeviceEnergyManagement, IsMTRSubscribeParams params) => mtrBaseClusterDeviceEnergyManagement -> params -> Ptr () -> Ptr () -> IO ()
subscribeAttributeFeatureMapWithParams_subscriptionEstablished_reportHandler mtrBaseClusterDeviceEnergyManagement params subscriptionEstablished reportHandler =
  sendMessage mtrBaseClusterDeviceEnergyManagement subscribeAttributeFeatureMapWithParams_subscriptionEstablished_reportHandlerSelector (toMTRSubscribeParams params) subscriptionEstablished reportHandler

-- | @+ readAttributeFeatureMapWithClusterStateCache:endpoint:queue:completion:@
readAttributeFeatureMapWithClusterStateCache_endpoint_queue_completion :: (IsMTRClusterStateCacheContainer clusterStateCacheContainer, IsNSNumber endpoint, IsNSObject queue) => clusterStateCacheContainer -> endpoint -> queue -> Ptr () -> IO ()
readAttributeFeatureMapWithClusterStateCache_endpoint_queue_completion clusterStateCacheContainer endpoint queue completion =
  do
    cls' <- getRequiredClass "MTRBaseClusterDeviceEnergyManagement"
    sendClassMessage cls' readAttributeFeatureMapWithClusterStateCache_endpoint_queue_completionSelector (toMTRClusterStateCacheContainer clusterStateCacheContainer) (toNSNumber endpoint) (toNSObject queue) completion

-- | @- readAttributeClusterRevisionWithCompletion:@
readAttributeClusterRevisionWithCompletion :: IsMTRBaseClusterDeviceEnergyManagement mtrBaseClusterDeviceEnergyManagement => mtrBaseClusterDeviceEnergyManagement -> Ptr () -> IO ()
readAttributeClusterRevisionWithCompletion mtrBaseClusterDeviceEnergyManagement completion =
  sendMessage mtrBaseClusterDeviceEnergyManagement readAttributeClusterRevisionWithCompletionSelector completion

-- | @- subscribeAttributeClusterRevisionWithParams:subscriptionEstablished:reportHandler:@
subscribeAttributeClusterRevisionWithParams_subscriptionEstablished_reportHandler :: (IsMTRBaseClusterDeviceEnergyManagement mtrBaseClusterDeviceEnergyManagement, IsMTRSubscribeParams params) => mtrBaseClusterDeviceEnergyManagement -> params -> Ptr () -> Ptr () -> IO ()
subscribeAttributeClusterRevisionWithParams_subscriptionEstablished_reportHandler mtrBaseClusterDeviceEnergyManagement params subscriptionEstablished reportHandler =
  sendMessage mtrBaseClusterDeviceEnergyManagement subscribeAttributeClusterRevisionWithParams_subscriptionEstablished_reportHandlerSelector (toMTRSubscribeParams params) subscriptionEstablished reportHandler

-- | @+ readAttributeClusterRevisionWithClusterStateCache:endpoint:queue:completion:@
readAttributeClusterRevisionWithClusterStateCache_endpoint_queue_completion :: (IsMTRClusterStateCacheContainer clusterStateCacheContainer, IsNSNumber endpoint, IsNSObject queue) => clusterStateCacheContainer -> endpoint -> queue -> Ptr () -> IO ()
readAttributeClusterRevisionWithClusterStateCache_endpoint_queue_completion clusterStateCacheContainer endpoint queue completion =
  do
    cls' <- getRequiredClass "MTRBaseClusterDeviceEnergyManagement"
    sendClassMessage cls' readAttributeClusterRevisionWithClusterStateCache_endpoint_queue_completionSelector (toMTRClusterStateCacheContainer clusterStateCacheContainer) (toNSNumber endpoint) (toNSObject queue) completion

-- | @- init@
init_ :: IsMTRBaseClusterDeviceEnergyManagement mtrBaseClusterDeviceEnergyManagement => mtrBaseClusterDeviceEnergyManagement -> IO (Id MTRBaseClusterDeviceEnergyManagement)
init_ mtrBaseClusterDeviceEnergyManagement =
  sendOwnedMessage mtrBaseClusterDeviceEnergyManagement initSelector

-- | @+ new@
new :: IO (Id MTRBaseClusterDeviceEnergyManagement)
new  =
  do
    cls' <- getRequiredClass "MTRBaseClusterDeviceEnergyManagement"
    sendOwnedClassMessage cls' newSelector

-- | For all instance methods (reads, writes, commands) that take a completion, the completion will be called on the provided queue.
--
-- ObjC selector: @- initWithDevice:endpointID:queue:@
initWithDevice_endpointID_queue :: (IsMTRBaseClusterDeviceEnergyManagement mtrBaseClusterDeviceEnergyManagement, IsMTRBaseDevice device, IsNSNumber endpointID, IsNSObject queue) => mtrBaseClusterDeviceEnergyManagement -> device -> endpointID -> queue -> IO (Id MTRBaseClusterDeviceEnergyManagement)
initWithDevice_endpointID_queue mtrBaseClusterDeviceEnergyManagement device endpointID queue =
  sendOwnedMessage mtrBaseClusterDeviceEnergyManagement initWithDevice_endpointID_queueSelector (toMTRBaseDevice device) (toNSNumber endpointID) (toNSObject queue)

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @powerAdjustRequestWithParams:completion:@
powerAdjustRequestWithParams_completionSelector :: Selector '[Id MTRDeviceEnergyManagementClusterPowerAdjustRequestParams, Ptr ()] ()
powerAdjustRequestWithParams_completionSelector = mkSelector "powerAdjustRequestWithParams:completion:"

-- | @Selector@ for @cancelPowerAdjustRequestWithParams:completion:@
cancelPowerAdjustRequestWithParams_completionSelector :: Selector '[Id MTRDeviceEnergyManagementClusterCancelPowerAdjustRequestParams, Ptr ()] ()
cancelPowerAdjustRequestWithParams_completionSelector = mkSelector "cancelPowerAdjustRequestWithParams:completion:"

-- | @Selector@ for @cancelPowerAdjustRequestWithCompletion:@
cancelPowerAdjustRequestWithCompletionSelector :: Selector '[Ptr ()] ()
cancelPowerAdjustRequestWithCompletionSelector = mkSelector "cancelPowerAdjustRequestWithCompletion:"

-- | @Selector@ for @startTimeAdjustRequestWithParams:completion:@
startTimeAdjustRequestWithParams_completionSelector :: Selector '[Id MTRDeviceEnergyManagementClusterStartTimeAdjustRequestParams, Ptr ()] ()
startTimeAdjustRequestWithParams_completionSelector = mkSelector "startTimeAdjustRequestWithParams:completion:"

-- | @Selector@ for @pauseRequestWithParams:completion:@
pauseRequestWithParams_completionSelector :: Selector '[Id MTRDeviceEnergyManagementClusterPauseRequestParams, Ptr ()] ()
pauseRequestWithParams_completionSelector = mkSelector "pauseRequestWithParams:completion:"

-- | @Selector@ for @resumeRequestWithParams:completion:@
resumeRequestWithParams_completionSelector :: Selector '[Id MTRDeviceEnergyManagementClusterResumeRequestParams, Ptr ()] ()
resumeRequestWithParams_completionSelector = mkSelector "resumeRequestWithParams:completion:"

-- | @Selector@ for @resumeRequestWithCompletion:@
resumeRequestWithCompletionSelector :: Selector '[Ptr ()] ()
resumeRequestWithCompletionSelector = mkSelector "resumeRequestWithCompletion:"

-- | @Selector@ for @modifyForecastRequestWithParams:completion:@
modifyForecastRequestWithParams_completionSelector :: Selector '[Id MTRDeviceEnergyManagementClusterModifyForecastRequestParams, Ptr ()] ()
modifyForecastRequestWithParams_completionSelector = mkSelector "modifyForecastRequestWithParams:completion:"

-- | @Selector@ for @requestConstraintBasedForecastWithParams:completion:@
requestConstraintBasedForecastWithParams_completionSelector :: Selector '[Id MTRDeviceEnergyManagementClusterRequestConstraintBasedForecastParams, Ptr ()] ()
requestConstraintBasedForecastWithParams_completionSelector = mkSelector "requestConstraintBasedForecastWithParams:completion:"

-- | @Selector@ for @cancelRequestWithParams:completion:@
cancelRequestWithParams_completionSelector :: Selector '[Id MTRDeviceEnergyManagementClusterCancelRequestParams, Ptr ()] ()
cancelRequestWithParams_completionSelector = mkSelector "cancelRequestWithParams:completion:"

-- | @Selector@ for @cancelRequestWithCompletion:@
cancelRequestWithCompletionSelector :: Selector '[Ptr ()] ()
cancelRequestWithCompletionSelector = mkSelector "cancelRequestWithCompletion:"

-- | @Selector@ for @readAttributeESATypeWithCompletion:@
readAttributeESATypeWithCompletionSelector :: Selector '[Ptr ()] ()
readAttributeESATypeWithCompletionSelector = mkSelector "readAttributeESATypeWithCompletion:"

-- | @Selector@ for @subscribeAttributeESATypeWithParams:subscriptionEstablished:reportHandler:@
subscribeAttributeESATypeWithParams_subscriptionEstablished_reportHandlerSelector :: Selector '[Id MTRSubscribeParams, Ptr (), Ptr ()] ()
subscribeAttributeESATypeWithParams_subscriptionEstablished_reportHandlerSelector = mkSelector "subscribeAttributeESATypeWithParams:subscriptionEstablished:reportHandler:"

-- | @Selector@ for @readAttributeESATypeWithClusterStateCache:endpoint:queue:completion:@
readAttributeESATypeWithClusterStateCache_endpoint_queue_completionSelector :: Selector '[Id MTRClusterStateCacheContainer, Id NSNumber, Id NSObject, Ptr ()] ()
readAttributeESATypeWithClusterStateCache_endpoint_queue_completionSelector = mkSelector "readAttributeESATypeWithClusterStateCache:endpoint:queue:completion:"

-- | @Selector@ for @readAttributeESACanGenerateWithCompletion:@
readAttributeESACanGenerateWithCompletionSelector :: Selector '[Ptr ()] ()
readAttributeESACanGenerateWithCompletionSelector = mkSelector "readAttributeESACanGenerateWithCompletion:"

-- | @Selector@ for @subscribeAttributeESACanGenerateWithParams:subscriptionEstablished:reportHandler:@
subscribeAttributeESACanGenerateWithParams_subscriptionEstablished_reportHandlerSelector :: Selector '[Id MTRSubscribeParams, Ptr (), Ptr ()] ()
subscribeAttributeESACanGenerateWithParams_subscriptionEstablished_reportHandlerSelector = mkSelector "subscribeAttributeESACanGenerateWithParams:subscriptionEstablished:reportHandler:"

-- | @Selector@ for @readAttributeESACanGenerateWithClusterStateCache:endpoint:queue:completion:@
readAttributeESACanGenerateWithClusterStateCache_endpoint_queue_completionSelector :: Selector '[Id MTRClusterStateCacheContainer, Id NSNumber, Id NSObject, Ptr ()] ()
readAttributeESACanGenerateWithClusterStateCache_endpoint_queue_completionSelector = mkSelector "readAttributeESACanGenerateWithClusterStateCache:endpoint:queue:completion:"

-- | @Selector@ for @readAttributeESAStateWithCompletion:@
readAttributeESAStateWithCompletionSelector :: Selector '[Ptr ()] ()
readAttributeESAStateWithCompletionSelector = mkSelector "readAttributeESAStateWithCompletion:"

-- | @Selector@ for @subscribeAttributeESAStateWithParams:subscriptionEstablished:reportHandler:@
subscribeAttributeESAStateWithParams_subscriptionEstablished_reportHandlerSelector :: Selector '[Id MTRSubscribeParams, Ptr (), Ptr ()] ()
subscribeAttributeESAStateWithParams_subscriptionEstablished_reportHandlerSelector = mkSelector "subscribeAttributeESAStateWithParams:subscriptionEstablished:reportHandler:"

-- | @Selector@ for @readAttributeESAStateWithClusterStateCache:endpoint:queue:completion:@
readAttributeESAStateWithClusterStateCache_endpoint_queue_completionSelector :: Selector '[Id MTRClusterStateCacheContainer, Id NSNumber, Id NSObject, Ptr ()] ()
readAttributeESAStateWithClusterStateCache_endpoint_queue_completionSelector = mkSelector "readAttributeESAStateWithClusterStateCache:endpoint:queue:completion:"

-- | @Selector@ for @readAttributeAbsMinPowerWithCompletion:@
readAttributeAbsMinPowerWithCompletionSelector :: Selector '[Ptr ()] ()
readAttributeAbsMinPowerWithCompletionSelector = mkSelector "readAttributeAbsMinPowerWithCompletion:"

-- | @Selector@ for @subscribeAttributeAbsMinPowerWithParams:subscriptionEstablished:reportHandler:@
subscribeAttributeAbsMinPowerWithParams_subscriptionEstablished_reportHandlerSelector :: Selector '[Id MTRSubscribeParams, Ptr (), Ptr ()] ()
subscribeAttributeAbsMinPowerWithParams_subscriptionEstablished_reportHandlerSelector = mkSelector "subscribeAttributeAbsMinPowerWithParams:subscriptionEstablished:reportHandler:"

-- | @Selector@ for @readAttributeAbsMinPowerWithClusterStateCache:endpoint:queue:completion:@
readAttributeAbsMinPowerWithClusterStateCache_endpoint_queue_completionSelector :: Selector '[Id MTRClusterStateCacheContainer, Id NSNumber, Id NSObject, Ptr ()] ()
readAttributeAbsMinPowerWithClusterStateCache_endpoint_queue_completionSelector = mkSelector "readAttributeAbsMinPowerWithClusterStateCache:endpoint:queue:completion:"

-- | @Selector@ for @readAttributeAbsMaxPowerWithCompletion:@
readAttributeAbsMaxPowerWithCompletionSelector :: Selector '[Ptr ()] ()
readAttributeAbsMaxPowerWithCompletionSelector = mkSelector "readAttributeAbsMaxPowerWithCompletion:"

-- | @Selector@ for @subscribeAttributeAbsMaxPowerWithParams:subscriptionEstablished:reportHandler:@
subscribeAttributeAbsMaxPowerWithParams_subscriptionEstablished_reportHandlerSelector :: Selector '[Id MTRSubscribeParams, Ptr (), Ptr ()] ()
subscribeAttributeAbsMaxPowerWithParams_subscriptionEstablished_reportHandlerSelector = mkSelector "subscribeAttributeAbsMaxPowerWithParams:subscriptionEstablished:reportHandler:"

-- | @Selector@ for @readAttributeAbsMaxPowerWithClusterStateCache:endpoint:queue:completion:@
readAttributeAbsMaxPowerWithClusterStateCache_endpoint_queue_completionSelector :: Selector '[Id MTRClusterStateCacheContainer, Id NSNumber, Id NSObject, Ptr ()] ()
readAttributeAbsMaxPowerWithClusterStateCache_endpoint_queue_completionSelector = mkSelector "readAttributeAbsMaxPowerWithClusterStateCache:endpoint:queue:completion:"

-- | @Selector@ for @readAttributePowerAdjustmentCapabilityWithCompletion:@
readAttributePowerAdjustmentCapabilityWithCompletionSelector :: Selector '[Ptr ()] ()
readAttributePowerAdjustmentCapabilityWithCompletionSelector = mkSelector "readAttributePowerAdjustmentCapabilityWithCompletion:"

-- | @Selector@ for @subscribeAttributePowerAdjustmentCapabilityWithParams:subscriptionEstablished:reportHandler:@
subscribeAttributePowerAdjustmentCapabilityWithParams_subscriptionEstablished_reportHandlerSelector :: Selector '[Id MTRSubscribeParams, Ptr (), Ptr ()] ()
subscribeAttributePowerAdjustmentCapabilityWithParams_subscriptionEstablished_reportHandlerSelector = mkSelector "subscribeAttributePowerAdjustmentCapabilityWithParams:subscriptionEstablished:reportHandler:"

-- | @Selector@ for @readAttributePowerAdjustmentCapabilityWithClusterStateCache:endpoint:queue:completion:@
readAttributePowerAdjustmentCapabilityWithClusterStateCache_endpoint_queue_completionSelector :: Selector '[Id MTRClusterStateCacheContainer, Id NSNumber, Id NSObject, Ptr ()] ()
readAttributePowerAdjustmentCapabilityWithClusterStateCache_endpoint_queue_completionSelector = mkSelector "readAttributePowerAdjustmentCapabilityWithClusterStateCache:endpoint:queue:completion:"

-- | @Selector@ for @readAttributeForecastWithCompletion:@
readAttributeForecastWithCompletionSelector :: Selector '[Ptr ()] ()
readAttributeForecastWithCompletionSelector = mkSelector "readAttributeForecastWithCompletion:"

-- | @Selector@ for @subscribeAttributeForecastWithParams:subscriptionEstablished:reportHandler:@
subscribeAttributeForecastWithParams_subscriptionEstablished_reportHandlerSelector :: Selector '[Id MTRSubscribeParams, Ptr (), Ptr ()] ()
subscribeAttributeForecastWithParams_subscriptionEstablished_reportHandlerSelector = mkSelector "subscribeAttributeForecastWithParams:subscriptionEstablished:reportHandler:"

-- | @Selector@ for @readAttributeForecastWithClusterStateCache:endpoint:queue:completion:@
readAttributeForecastWithClusterStateCache_endpoint_queue_completionSelector :: Selector '[Id MTRClusterStateCacheContainer, Id NSNumber, Id NSObject, Ptr ()] ()
readAttributeForecastWithClusterStateCache_endpoint_queue_completionSelector = mkSelector "readAttributeForecastWithClusterStateCache:endpoint:queue:completion:"

-- | @Selector@ for @readAttributeOptOutStateWithCompletion:@
readAttributeOptOutStateWithCompletionSelector :: Selector '[Ptr ()] ()
readAttributeOptOutStateWithCompletionSelector = mkSelector "readAttributeOptOutStateWithCompletion:"

-- | @Selector@ for @subscribeAttributeOptOutStateWithParams:subscriptionEstablished:reportHandler:@
subscribeAttributeOptOutStateWithParams_subscriptionEstablished_reportHandlerSelector :: Selector '[Id MTRSubscribeParams, Ptr (), Ptr ()] ()
subscribeAttributeOptOutStateWithParams_subscriptionEstablished_reportHandlerSelector = mkSelector "subscribeAttributeOptOutStateWithParams:subscriptionEstablished:reportHandler:"

-- | @Selector@ for @readAttributeOptOutStateWithClusterStateCache:endpoint:queue:completion:@
readAttributeOptOutStateWithClusterStateCache_endpoint_queue_completionSelector :: Selector '[Id MTRClusterStateCacheContainer, Id NSNumber, Id NSObject, Ptr ()] ()
readAttributeOptOutStateWithClusterStateCache_endpoint_queue_completionSelector = mkSelector "readAttributeOptOutStateWithClusterStateCache:endpoint:queue:completion:"

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
initSelector :: Selector '[] (Id MTRBaseClusterDeviceEnergyManagement)
initSelector = mkSelector "init"

-- | @Selector@ for @new@
newSelector :: Selector '[] (Id MTRBaseClusterDeviceEnergyManagement)
newSelector = mkSelector "new"

-- | @Selector@ for @initWithDevice:endpointID:queue:@
initWithDevice_endpointID_queueSelector :: Selector '[Id MTRBaseDevice, Id NSNumber, Id NSObject] (Id MTRBaseClusterDeviceEnergyManagement)
initWithDevice_endpointID_queueSelector = mkSelector "initWithDevice:endpointID:queue:"

