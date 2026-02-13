{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Cluster ICD Management
--
-- Allows servers to ensure that listed clients are notified when a server is available for communication.
--
-- Generated bindings for @MTRBaseClusterICDManagement@.
module ObjC.Matter.MTRBaseClusterICDManagement
  ( MTRBaseClusterICDManagement
  , IsMTRBaseClusterICDManagement(..)
  , registerClientWithParams_completion
  , unregisterClientWithParams_completion
  , stayActiveRequestWithParams_completion
  , readAttributeIdleModeDurationWithCompletion
  , subscribeAttributeIdleModeDurationWithParams_subscriptionEstablished_reportHandler
  , readAttributeIdleModeDurationWithClusterStateCache_endpoint_queue_completion
  , readAttributeActiveModeDurationWithCompletion
  , subscribeAttributeActiveModeDurationWithParams_subscriptionEstablished_reportHandler
  , readAttributeActiveModeDurationWithClusterStateCache_endpoint_queue_completion
  , readAttributeActiveModeThresholdWithCompletion
  , subscribeAttributeActiveModeThresholdWithParams_subscriptionEstablished_reportHandler
  , readAttributeActiveModeThresholdWithClusterStateCache_endpoint_queue_completion
  , readAttributeRegisteredClientsWithParams_completion
  , subscribeAttributeRegisteredClientsWithParams_subscriptionEstablished_reportHandler
  , readAttributeRegisteredClientsWithClusterStateCache_endpoint_queue_completion
  , readAttributeICDCounterWithCompletion
  , subscribeAttributeICDCounterWithParams_subscriptionEstablished_reportHandler
  , readAttributeICDCounterWithClusterStateCache_endpoint_queue_completion
  , readAttributeClientsSupportedPerFabricWithCompletion
  , subscribeAttributeClientsSupportedPerFabricWithParams_subscriptionEstablished_reportHandler
  , readAttributeClientsSupportedPerFabricWithClusterStateCache_endpoint_queue_completion
  , readAttributeUserActiveModeTriggerHintWithCompletion
  , subscribeAttributeUserActiveModeTriggerHintWithParams_subscriptionEstablished_reportHandler
  , readAttributeUserActiveModeTriggerHintWithClusterStateCache_endpoint_queue_completion
  , readAttributeUserActiveModeTriggerInstructionWithCompletion
  , subscribeAttributeUserActiveModeTriggerInstructionWithParams_subscriptionEstablished_reportHandler
  , readAttributeUserActiveModeTriggerInstructionWithClusterStateCache_endpoint_queue_completion
  , readAttributeOperatingModeWithCompletion
  , subscribeAttributeOperatingModeWithParams_subscriptionEstablished_reportHandler
  , readAttributeOperatingModeWithClusterStateCache_endpoint_queue_completion
  , readAttributeMaximumCheckInBackOffWithCompletion
  , subscribeAttributeMaximumCheckInBackOffWithParams_subscriptionEstablished_reportHandler
  , readAttributeMaximumCheckInBackOffWithClusterStateCache_endpoint_queue_completion
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
  , initSelector
  , initWithDevice_endpointID_queueSelector
  , newSelector
  , readAttributeAcceptedCommandListWithClusterStateCache_endpoint_queue_completionSelector
  , readAttributeAcceptedCommandListWithCompletionSelector
  , readAttributeActiveModeDurationWithClusterStateCache_endpoint_queue_completionSelector
  , readAttributeActiveModeDurationWithCompletionSelector
  , readAttributeActiveModeThresholdWithClusterStateCache_endpoint_queue_completionSelector
  , readAttributeActiveModeThresholdWithCompletionSelector
  , readAttributeAttributeListWithClusterStateCache_endpoint_queue_completionSelector
  , readAttributeAttributeListWithCompletionSelector
  , readAttributeClientsSupportedPerFabricWithClusterStateCache_endpoint_queue_completionSelector
  , readAttributeClientsSupportedPerFabricWithCompletionSelector
  , readAttributeClusterRevisionWithClusterStateCache_endpoint_queue_completionSelector
  , readAttributeClusterRevisionWithCompletionSelector
  , readAttributeFeatureMapWithClusterStateCache_endpoint_queue_completionSelector
  , readAttributeFeatureMapWithCompletionSelector
  , readAttributeGeneratedCommandListWithClusterStateCache_endpoint_queue_completionSelector
  , readAttributeGeneratedCommandListWithCompletionSelector
  , readAttributeICDCounterWithClusterStateCache_endpoint_queue_completionSelector
  , readAttributeICDCounterWithCompletionSelector
  , readAttributeIdleModeDurationWithClusterStateCache_endpoint_queue_completionSelector
  , readAttributeIdleModeDurationWithCompletionSelector
  , readAttributeMaximumCheckInBackOffWithClusterStateCache_endpoint_queue_completionSelector
  , readAttributeMaximumCheckInBackOffWithCompletionSelector
  , readAttributeOperatingModeWithClusterStateCache_endpoint_queue_completionSelector
  , readAttributeOperatingModeWithCompletionSelector
  , readAttributeRegisteredClientsWithClusterStateCache_endpoint_queue_completionSelector
  , readAttributeRegisteredClientsWithParams_completionSelector
  , readAttributeUserActiveModeTriggerHintWithClusterStateCache_endpoint_queue_completionSelector
  , readAttributeUserActiveModeTriggerHintWithCompletionSelector
  , readAttributeUserActiveModeTriggerInstructionWithClusterStateCache_endpoint_queue_completionSelector
  , readAttributeUserActiveModeTriggerInstructionWithCompletionSelector
  , registerClientWithParams_completionSelector
  , stayActiveRequestWithParams_completionSelector
  , subscribeAttributeAcceptedCommandListWithParams_subscriptionEstablished_reportHandlerSelector
  , subscribeAttributeActiveModeDurationWithParams_subscriptionEstablished_reportHandlerSelector
  , subscribeAttributeActiveModeThresholdWithParams_subscriptionEstablished_reportHandlerSelector
  , subscribeAttributeAttributeListWithParams_subscriptionEstablished_reportHandlerSelector
  , subscribeAttributeClientsSupportedPerFabricWithParams_subscriptionEstablished_reportHandlerSelector
  , subscribeAttributeClusterRevisionWithParams_subscriptionEstablished_reportHandlerSelector
  , subscribeAttributeFeatureMapWithParams_subscriptionEstablished_reportHandlerSelector
  , subscribeAttributeGeneratedCommandListWithParams_subscriptionEstablished_reportHandlerSelector
  , subscribeAttributeICDCounterWithParams_subscriptionEstablished_reportHandlerSelector
  , subscribeAttributeIdleModeDurationWithParams_subscriptionEstablished_reportHandlerSelector
  , subscribeAttributeMaximumCheckInBackOffWithParams_subscriptionEstablished_reportHandlerSelector
  , subscribeAttributeOperatingModeWithParams_subscriptionEstablished_reportHandlerSelector
  , subscribeAttributeRegisteredClientsWithParams_subscriptionEstablished_reportHandlerSelector
  , subscribeAttributeUserActiveModeTriggerHintWithParams_subscriptionEstablished_reportHandlerSelector
  , subscribeAttributeUserActiveModeTriggerInstructionWithParams_subscriptionEstablished_reportHandlerSelector
  , unregisterClientWithParams_completionSelector


  ) where

import Foreign.Ptr (Ptr, FunPtr)
import Foreign.C.Types

import ObjC.Runtime.Types
import ObjC.Runtime.Message (sendMessage, sendOwnedMessage, sendClassMessage, sendOwnedClassMessage)
import ObjC.Runtime.Selector (mkSelector)
import ObjC.Runtime.Class (getRequiredClass)

import ObjC.Matter.Internal.Classes
import ObjC.Foundation.Internal.Classes

-- | Command RegisterClient
--
-- This command allows a client to register itself with the ICD to be notified when the device is available for communication.
--
-- ObjC selector: @- registerClientWithParams:completion:@
registerClientWithParams_completion :: (IsMTRBaseClusterICDManagement mtrBaseClusterICDManagement, IsMTRICDManagementClusterRegisterClientParams params) => mtrBaseClusterICDManagement -> params -> Ptr () -> IO ()
registerClientWithParams_completion mtrBaseClusterICDManagement params completion =
  sendMessage mtrBaseClusterICDManagement registerClientWithParams_completionSelector (toMTRICDManagementClusterRegisterClientParams params) completion

-- | Command UnregisterClient
--
-- This command allows a client to unregister itself with the ICD.
--
-- ObjC selector: @- unregisterClientWithParams:completion:@
unregisterClientWithParams_completion :: (IsMTRBaseClusterICDManagement mtrBaseClusterICDManagement, IsMTRICDManagementClusterUnregisterClientParams params) => mtrBaseClusterICDManagement -> params -> Ptr () -> IO ()
unregisterClientWithParams_completion mtrBaseClusterICDManagement params completion =
  sendMessage mtrBaseClusterICDManagement unregisterClientWithParams_completionSelector (toMTRICDManagementClusterUnregisterClientParams params) completion

-- | Command StayActiveRequest
--
-- This command allows a client to request that the server stays in active mode for at least a given time duration (in milliseconds) from when this command is received.
--
-- ObjC selector: @- stayActiveRequestWithParams:completion:@
stayActiveRequestWithParams_completion :: (IsMTRBaseClusterICDManagement mtrBaseClusterICDManagement, IsMTRICDManagementClusterStayActiveRequestParams params) => mtrBaseClusterICDManagement -> params -> Ptr () -> IO ()
stayActiveRequestWithParams_completion mtrBaseClusterICDManagement params completion =
  sendMessage mtrBaseClusterICDManagement stayActiveRequestWithParams_completionSelector (toMTRICDManagementClusterStayActiveRequestParams params) completion

-- | @- readAttributeIdleModeDurationWithCompletion:@
readAttributeIdleModeDurationWithCompletion :: IsMTRBaseClusterICDManagement mtrBaseClusterICDManagement => mtrBaseClusterICDManagement -> Ptr () -> IO ()
readAttributeIdleModeDurationWithCompletion mtrBaseClusterICDManagement completion =
  sendMessage mtrBaseClusterICDManagement readAttributeIdleModeDurationWithCompletionSelector completion

-- | @- subscribeAttributeIdleModeDurationWithParams:subscriptionEstablished:reportHandler:@
subscribeAttributeIdleModeDurationWithParams_subscriptionEstablished_reportHandler :: (IsMTRBaseClusterICDManagement mtrBaseClusterICDManagement, IsMTRSubscribeParams params) => mtrBaseClusterICDManagement -> params -> Ptr () -> Ptr () -> IO ()
subscribeAttributeIdleModeDurationWithParams_subscriptionEstablished_reportHandler mtrBaseClusterICDManagement params subscriptionEstablished reportHandler =
  sendMessage mtrBaseClusterICDManagement subscribeAttributeIdleModeDurationWithParams_subscriptionEstablished_reportHandlerSelector (toMTRSubscribeParams params) subscriptionEstablished reportHandler

-- | @+ readAttributeIdleModeDurationWithClusterStateCache:endpoint:queue:completion:@
readAttributeIdleModeDurationWithClusterStateCache_endpoint_queue_completion :: (IsMTRClusterStateCacheContainer clusterStateCacheContainer, IsNSNumber endpoint, IsNSObject queue) => clusterStateCacheContainer -> endpoint -> queue -> Ptr () -> IO ()
readAttributeIdleModeDurationWithClusterStateCache_endpoint_queue_completion clusterStateCacheContainer endpoint queue completion =
  do
    cls' <- getRequiredClass "MTRBaseClusterICDManagement"
    sendClassMessage cls' readAttributeIdleModeDurationWithClusterStateCache_endpoint_queue_completionSelector (toMTRClusterStateCacheContainer clusterStateCacheContainer) (toNSNumber endpoint) (toNSObject queue) completion

-- | @- readAttributeActiveModeDurationWithCompletion:@
readAttributeActiveModeDurationWithCompletion :: IsMTRBaseClusterICDManagement mtrBaseClusterICDManagement => mtrBaseClusterICDManagement -> Ptr () -> IO ()
readAttributeActiveModeDurationWithCompletion mtrBaseClusterICDManagement completion =
  sendMessage mtrBaseClusterICDManagement readAttributeActiveModeDurationWithCompletionSelector completion

-- | @- subscribeAttributeActiveModeDurationWithParams:subscriptionEstablished:reportHandler:@
subscribeAttributeActiveModeDurationWithParams_subscriptionEstablished_reportHandler :: (IsMTRBaseClusterICDManagement mtrBaseClusterICDManagement, IsMTRSubscribeParams params) => mtrBaseClusterICDManagement -> params -> Ptr () -> Ptr () -> IO ()
subscribeAttributeActiveModeDurationWithParams_subscriptionEstablished_reportHandler mtrBaseClusterICDManagement params subscriptionEstablished reportHandler =
  sendMessage mtrBaseClusterICDManagement subscribeAttributeActiveModeDurationWithParams_subscriptionEstablished_reportHandlerSelector (toMTRSubscribeParams params) subscriptionEstablished reportHandler

-- | @+ readAttributeActiveModeDurationWithClusterStateCache:endpoint:queue:completion:@
readAttributeActiveModeDurationWithClusterStateCache_endpoint_queue_completion :: (IsMTRClusterStateCacheContainer clusterStateCacheContainer, IsNSNumber endpoint, IsNSObject queue) => clusterStateCacheContainer -> endpoint -> queue -> Ptr () -> IO ()
readAttributeActiveModeDurationWithClusterStateCache_endpoint_queue_completion clusterStateCacheContainer endpoint queue completion =
  do
    cls' <- getRequiredClass "MTRBaseClusterICDManagement"
    sendClassMessage cls' readAttributeActiveModeDurationWithClusterStateCache_endpoint_queue_completionSelector (toMTRClusterStateCacheContainer clusterStateCacheContainer) (toNSNumber endpoint) (toNSObject queue) completion

-- | @- readAttributeActiveModeThresholdWithCompletion:@
readAttributeActiveModeThresholdWithCompletion :: IsMTRBaseClusterICDManagement mtrBaseClusterICDManagement => mtrBaseClusterICDManagement -> Ptr () -> IO ()
readAttributeActiveModeThresholdWithCompletion mtrBaseClusterICDManagement completion =
  sendMessage mtrBaseClusterICDManagement readAttributeActiveModeThresholdWithCompletionSelector completion

-- | @- subscribeAttributeActiveModeThresholdWithParams:subscriptionEstablished:reportHandler:@
subscribeAttributeActiveModeThresholdWithParams_subscriptionEstablished_reportHandler :: (IsMTRBaseClusterICDManagement mtrBaseClusterICDManagement, IsMTRSubscribeParams params) => mtrBaseClusterICDManagement -> params -> Ptr () -> Ptr () -> IO ()
subscribeAttributeActiveModeThresholdWithParams_subscriptionEstablished_reportHandler mtrBaseClusterICDManagement params subscriptionEstablished reportHandler =
  sendMessage mtrBaseClusterICDManagement subscribeAttributeActiveModeThresholdWithParams_subscriptionEstablished_reportHandlerSelector (toMTRSubscribeParams params) subscriptionEstablished reportHandler

-- | @+ readAttributeActiveModeThresholdWithClusterStateCache:endpoint:queue:completion:@
readAttributeActiveModeThresholdWithClusterStateCache_endpoint_queue_completion :: (IsMTRClusterStateCacheContainer clusterStateCacheContainer, IsNSNumber endpoint, IsNSObject queue) => clusterStateCacheContainer -> endpoint -> queue -> Ptr () -> IO ()
readAttributeActiveModeThresholdWithClusterStateCache_endpoint_queue_completion clusterStateCacheContainer endpoint queue completion =
  do
    cls' <- getRequiredClass "MTRBaseClusterICDManagement"
    sendClassMessage cls' readAttributeActiveModeThresholdWithClusterStateCache_endpoint_queue_completionSelector (toMTRClusterStateCacheContainer clusterStateCacheContainer) (toNSNumber endpoint) (toNSObject queue) completion

-- | @- readAttributeRegisteredClientsWithParams:completion:@
readAttributeRegisteredClientsWithParams_completion :: (IsMTRBaseClusterICDManagement mtrBaseClusterICDManagement, IsMTRReadParams params) => mtrBaseClusterICDManagement -> params -> Ptr () -> IO ()
readAttributeRegisteredClientsWithParams_completion mtrBaseClusterICDManagement params completion =
  sendMessage mtrBaseClusterICDManagement readAttributeRegisteredClientsWithParams_completionSelector (toMTRReadParams params) completion

-- | @- subscribeAttributeRegisteredClientsWithParams:subscriptionEstablished:reportHandler:@
subscribeAttributeRegisteredClientsWithParams_subscriptionEstablished_reportHandler :: (IsMTRBaseClusterICDManagement mtrBaseClusterICDManagement, IsMTRSubscribeParams params) => mtrBaseClusterICDManagement -> params -> Ptr () -> Ptr () -> IO ()
subscribeAttributeRegisteredClientsWithParams_subscriptionEstablished_reportHandler mtrBaseClusterICDManagement params subscriptionEstablished reportHandler =
  sendMessage mtrBaseClusterICDManagement subscribeAttributeRegisteredClientsWithParams_subscriptionEstablished_reportHandlerSelector (toMTRSubscribeParams params) subscriptionEstablished reportHandler

-- | @+ readAttributeRegisteredClientsWithClusterStateCache:endpoint:queue:completion:@
readAttributeRegisteredClientsWithClusterStateCache_endpoint_queue_completion :: (IsMTRClusterStateCacheContainer clusterStateCacheContainer, IsNSNumber endpoint, IsNSObject queue) => clusterStateCacheContainer -> endpoint -> queue -> Ptr () -> IO ()
readAttributeRegisteredClientsWithClusterStateCache_endpoint_queue_completion clusterStateCacheContainer endpoint queue completion =
  do
    cls' <- getRequiredClass "MTRBaseClusterICDManagement"
    sendClassMessage cls' readAttributeRegisteredClientsWithClusterStateCache_endpoint_queue_completionSelector (toMTRClusterStateCacheContainer clusterStateCacheContainer) (toNSNumber endpoint) (toNSObject queue) completion

-- | @- readAttributeICDCounterWithCompletion:@
readAttributeICDCounterWithCompletion :: IsMTRBaseClusterICDManagement mtrBaseClusterICDManagement => mtrBaseClusterICDManagement -> Ptr () -> IO ()
readAttributeICDCounterWithCompletion mtrBaseClusterICDManagement completion =
  sendMessage mtrBaseClusterICDManagement readAttributeICDCounterWithCompletionSelector completion

-- | @- subscribeAttributeICDCounterWithParams:subscriptionEstablished:reportHandler:@
subscribeAttributeICDCounterWithParams_subscriptionEstablished_reportHandler :: (IsMTRBaseClusterICDManagement mtrBaseClusterICDManagement, IsMTRSubscribeParams params) => mtrBaseClusterICDManagement -> params -> Ptr () -> Ptr () -> IO ()
subscribeAttributeICDCounterWithParams_subscriptionEstablished_reportHandler mtrBaseClusterICDManagement params subscriptionEstablished reportHandler =
  sendMessage mtrBaseClusterICDManagement subscribeAttributeICDCounterWithParams_subscriptionEstablished_reportHandlerSelector (toMTRSubscribeParams params) subscriptionEstablished reportHandler

-- | @+ readAttributeICDCounterWithClusterStateCache:endpoint:queue:completion:@
readAttributeICDCounterWithClusterStateCache_endpoint_queue_completion :: (IsMTRClusterStateCacheContainer clusterStateCacheContainer, IsNSNumber endpoint, IsNSObject queue) => clusterStateCacheContainer -> endpoint -> queue -> Ptr () -> IO ()
readAttributeICDCounterWithClusterStateCache_endpoint_queue_completion clusterStateCacheContainer endpoint queue completion =
  do
    cls' <- getRequiredClass "MTRBaseClusterICDManagement"
    sendClassMessage cls' readAttributeICDCounterWithClusterStateCache_endpoint_queue_completionSelector (toMTRClusterStateCacheContainer clusterStateCacheContainer) (toNSNumber endpoint) (toNSObject queue) completion

-- | @- readAttributeClientsSupportedPerFabricWithCompletion:@
readAttributeClientsSupportedPerFabricWithCompletion :: IsMTRBaseClusterICDManagement mtrBaseClusterICDManagement => mtrBaseClusterICDManagement -> Ptr () -> IO ()
readAttributeClientsSupportedPerFabricWithCompletion mtrBaseClusterICDManagement completion =
  sendMessage mtrBaseClusterICDManagement readAttributeClientsSupportedPerFabricWithCompletionSelector completion

-- | @- subscribeAttributeClientsSupportedPerFabricWithParams:subscriptionEstablished:reportHandler:@
subscribeAttributeClientsSupportedPerFabricWithParams_subscriptionEstablished_reportHandler :: (IsMTRBaseClusterICDManagement mtrBaseClusterICDManagement, IsMTRSubscribeParams params) => mtrBaseClusterICDManagement -> params -> Ptr () -> Ptr () -> IO ()
subscribeAttributeClientsSupportedPerFabricWithParams_subscriptionEstablished_reportHandler mtrBaseClusterICDManagement params subscriptionEstablished reportHandler =
  sendMessage mtrBaseClusterICDManagement subscribeAttributeClientsSupportedPerFabricWithParams_subscriptionEstablished_reportHandlerSelector (toMTRSubscribeParams params) subscriptionEstablished reportHandler

-- | @+ readAttributeClientsSupportedPerFabricWithClusterStateCache:endpoint:queue:completion:@
readAttributeClientsSupportedPerFabricWithClusterStateCache_endpoint_queue_completion :: (IsMTRClusterStateCacheContainer clusterStateCacheContainer, IsNSNumber endpoint, IsNSObject queue) => clusterStateCacheContainer -> endpoint -> queue -> Ptr () -> IO ()
readAttributeClientsSupportedPerFabricWithClusterStateCache_endpoint_queue_completion clusterStateCacheContainer endpoint queue completion =
  do
    cls' <- getRequiredClass "MTRBaseClusterICDManagement"
    sendClassMessage cls' readAttributeClientsSupportedPerFabricWithClusterStateCache_endpoint_queue_completionSelector (toMTRClusterStateCacheContainer clusterStateCacheContainer) (toNSNumber endpoint) (toNSObject queue) completion

-- | @- readAttributeUserActiveModeTriggerHintWithCompletion:@
readAttributeUserActiveModeTriggerHintWithCompletion :: IsMTRBaseClusterICDManagement mtrBaseClusterICDManagement => mtrBaseClusterICDManagement -> Ptr () -> IO ()
readAttributeUserActiveModeTriggerHintWithCompletion mtrBaseClusterICDManagement completion =
  sendMessage mtrBaseClusterICDManagement readAttributeUserActiveModeTriggerHintWithCompletionSelector completion

-- | @- subscribeAttributeUserActiveModeTriggerHintWithParams:subscriptionEstablished:reportHandler:@
subscribeAttributeUserActiveModeTriggerHintWithParams_subscriptionEstablished_reportHandler :: (IsMTRBaseClusterICDManagement mtrBaseClusterICDManagement, IsMTRSubscribeParams params) => mtrBaseClusterICDManagement -> params -> Ptr () -> Ptr () -> IO ()
subscribeAttributeUserActiveModeTriggerHintWithParams_subscriptionEstablished_reportHandler mtrBaseClusterICDManagement params subscriptionEstablished reportHandler =
  sendMessage mtrBaseClusterICDManagement subscribeAttributeUserActiveModeTriggerHintWithParams_subscriptionEstablished_reportHandlerSelector (toMTRSubscribeParams params) subscriptionEstablished reportHandler

-- | @+ readAttributeUserActiveModeTriggerHintWithClusterStateCache:endpoint:queue:completion:@
readAttributeUserActiveModeTriggerHintWithClusterStateCache_endpoint_queue_completion :: (IsMTRClusterStateCacheContainer clusterStateCacheContainer, IsNSNumber endpoint, IsNSObject queue) => clusterStateCacheContainer -> endpoint -> queue -> Ptr () -> IO ()
readAttributeUserActiveModeTriggerHintWithClusterStateCache_endpoint_queue_completion clusterStateCacheContainer endpoint queue completion =
  do
    cls' <- getRequiredClass "MTRBaseClusterICDManagement"
    sendClassMessage cls' readAttributeUserActiveModeTriggerHintWithClusterStateCache_endpoint_queue_completionSelector (toMTRClusterStateCacheContainer clusterStateCacheContainer) (toNSNumber endpoint) (toNSObject queue) completion

-- | @- readAttributeUserActiveModeTriggerInstructionWithCompletion:@
readAttributeUserActiveModeTriggerInstructionWithCompletion :: IsMTRBaseClusterICDManagement mtrBaseClusterICDManagement => mtrBaseClusterICDManagement -> Ptr () -> IO ()
readAttributeUserActiveModeTriggerInstructionWithCompletion mtrBaseClusterICDManagement completion =
  sendMessage mtrBaseClusterICDManagement readAttributeUserActiveModeTriggerInstructionWithCompletionSelector completion

-- | @- subscribeAttributeUserActiveModeTriggerInstructionWithParams:subscriptionEstablished:reportHandler:@
subscribeAttributeUserActiveModeTriggerInstructionWithParams_subscriptionEstablished_reportHandler :: (IsMTRBaseClusterICDManagement mtrBaseClusterICDManagement, IsMTRSubscribeParams params) => mtrBaseClusterICDManagement -> params -> Ptr () -> Ptr () -> IO ()
subscribeAttributeUserActiveModeTriggerInstructionWithParams_subscriptionEstablished_reportHandler mtrBaseClusterICDManagement params subscriptionEstablished reportHandler =
  sendMessage mtrBaseClusterICDManagement subscribeAttributeUserActiveModeTriggerInstructionWithParams_subscriptionEstablished_reportHandlerSelector (toMTRSubscribeParams params) subscriptionEstablished reportHandler

-- | @+ readAttributeUserActiveModeTriggerInstructionWithClusterStateCache:endpoint:queue:completion:@
readAttributeUserActiveModeTriggerInstructionWithClusterStateCache_endpoint_queue_completion :: (IsMTRClusterStateCacheContainer clusterStateCacheContainer, IsNSNumber endpoint, IsNSObject queue) => clusterStateCacheContainer -> endpoint -> queue -> Ptr () -> IO ()
readAttributeUserActiveModeTriggerInstructionWithClusterStateCache_endpoint_queue_completion clusterStateCacheContainer endpoint queue completion =
  do
    cls' <- getRequiredClass "MTRBaseClusterICDManagement"
    sendClassMessage cls' readAttributeUserActiveModeTriggerInstructionWithClusterStateCache_endpoint_queue_completionSelector (toMTRClusterStateCacheContainer clusterStateCacheContainer) (toNSNumber endpoint) (toNSObject queue) completion

-- | @- readAttributeOperatingModeWithCompletion:@
readAttributeOperatingModeWithCompletion :: IsMTRBaseClusterICDManagement mtrBaseClusterICDManagement => mtrBaseClusterICDManagement -> Ptr () -> IO ()
readAttributeOperatingModeWithCompletion mtrBaseClusterICDManagement completion =
  sendMessage mtrBaseClusterICDManagement readAttributeOperatingModeWithCompletionSelector completion

-- | @- subscribeAttributeOperatingModeWithParams:subscriptionEstablished:reportHandler:@
subscribeAttributeOperatingModeWithParams_subscriptionEstablished_reportHandler :: (IsMTRBaseClusterICDManagement mtrBaseClusterICDManagement, IsMTRSubscribeParams params) => mtrBaseClusterICDManagement -> params -> Ptr () -> Ptr () -> IO ()
subscribeAttributeOperatingModeWithParams_subscriptionEstablished_reportHandler mtrBaseClusterICDManagement params subscriptionEstablished reportHandler =
  sendMessage mtrBaseClusterICDManagement subscribeAttributeOperatingModeWithParams_subscriptionEstablished_reportHandlerSelector (toMTRSubscribeParams params) subscriptionEstablished reportHandler

-- | @+ readAttributeOperatingModeWithClusterStateCache:endpoint:queue:completion:@
readAttributeOperatingModeWithClusterStateCache_endpoint_queue_completion :: (IsMTRClusterStateCacheContainer clusterStateCacheContainer, IsNSNumber endpoint, IsNSObject queue) => clusterStateCacheContainer -> endpoint -> queue -> Ptr () -> IO ()
readAttributeOperatingModeWithClusterStateCache_endpoint_queue_completion clusterStateCacheContainer endpoint queue completion =
  do
    cls' <- getRequiredClass "MTRBaseClusterICDManagement"
    sendClassMessage cls' readAttributeOperatingModeWithClusterStateCache_endpoint_queue_completionSelector (toMTRClusterStateCacheContainer clusterStateCacheContainer) (toNSNumber endpoint) (toNSObject queue) completion

-- | @- readAttributeMaximumCheckInBackOffWithCompletion:@
readAttributeMaximumCheckInBackOffWithCompletion :: IsMTRBaseClusterICDManagement mtrBaseClusterICDManagement => mtrBaseClusterICDManagement -> Ptr () -> IO ()
readAttributeMaximumCheckInBackOffWithCompletion mtrBaseClusterICDManagement completion =
  sendMessage mtrBaseClusterICDManagement readAttributeMaximumCheckInBackOffWithCompletionSelector completion

-- | @- subscribeAttributeMaximumCheckInBackOffWithParams:subscriptionEstablished:reportHandler:@
subscribeAttributeMaximumCheckInBackOffWithParams_subscriptionEstablished_reportHandler :: (IsMTRBaseClusterICDManagement mtrBaseClusterICDManagement, IsMTRSubscribeParams params) => mtrBaseClusterICDManagement -> params -> Ptr () -> Ptr () -> IO ()
subscribeAttributeMaximumCheckInBackOffWithParams_subscriptionEstablished_reportHandler mtrBaseClusterICDManagement params subscriptionEstablished reportHandler =
  sendMessage mtrBaseClusterICDManagement subscribeAttributeMaximumCheckInBackOffWithParams_subscriptionEstablished_reportHandlerSelector (toMTRSubscribeParams params) subscriptionEstablished reportHandler

-- | @+ readAttributeMaximumCheckInBackOffWithClusterStateCache:endpoint:queue:completion:@
readAttributeMaximumCheckInBackOffWithClusterStateCache_endpoint_queue_completion :: (IsMTRClusterStateCacheContainer clusterStateCacheContainer, IsNSNumber endpoint, IsNSObject queue) => clusterStateCacheContainer -> endpoint -> queue -> Ptr () -> IO ()
readAttributeMaximumCheckInBackOffWithClusterStateCache_endpoint_queue_completion clusterStateCacheContainer endpoint queue completion =
  do
    cls' <- getRequiredClass "MTRBaseClusterICDManagement"
    sendClassMessage cls' readAttributeMaximumCheckInBackOffWithClusterStateCache_endpoint_queue_completionSelector (toMTRClusterStateCacheContainer clusterStateCacheContainer) (toNSNumber endpoint) (toNSObject queue) completion

-- | @- readAttributeGeneratedCommandListWithCompletion:@
readAttributeGeneratedCommandListWithCompletion :: IsMTRBaseClusterICDManagement mtrBaseClusterICDManagement => mtrBaseClusterICDManagement -> Ptr () -> IO ()
readAttributeGeneratedCommandListWithCompletion mtrBaseClusterICDManagement completion =
  sendMessage mtrBaseClusterICDManagement readAttributeGeneratedCommandListWithCompletionSelector completion

-- | @- subscribeAttributeGeneratedCommandListWithParams:subscriptionEstablished:reportHandler:@
subscribeAttributeGeneratedCommandListWithParams_subscriptionEstablished_reportHandler :: (IsMTRBaseClusterICDManagement mtrBaseClusterICDManagement, IsMTRSubscribeParams params) => mtrBaseClusterICDManagement -> params -> Ptr () -> Ptr () -> IO ()
subscribeAttributeGeneratedCommandListWithParams_subscriptionEstablished_reportHandler mtrBaseClusterICDManagement params subscriptionEstablished reportHandler =
  sendMessage mtrBaseClusterICDManagement subscribeAttributeGeneratedCommandListWithParams_subscriptionEstablished_reportHandlerSelector (toMTRSubscribeParams params) subscriptionEstablished reportHandler

-- | @+ readAttributeGeneratedCommandListWithClusterStateCache:endpoint:queue:completion:@
readAttributeGeneratedCommandListWithClusterStateCache_endpoint_queue_completion :: (IsMTRClusterStateCacheContainer clusterStateCacheContainer, IsNSNumber endpoint, IsNSObject queue) => clusterStateCacheContainer -> endpoint -> queue -> Ptr () -> IO ()
readAttributeGeneratedCommandListWithClusterStateCache_endpoint_queue_completion clusterStateCacheContainer endpoint queue completion =
  do
    cls' <- getRequiredClass "MTRBaseClusterICDManagement"
    sendClassMessage cls' readAttributeGeneratedCommandListWithClusterStateCache_endpoint_queue_completionSelector (toMTRClusterStateCacheContainer clusterStateCacheContainer) (toNSNumber endpoint) (toNSObject queue) completion

-- | @- readAttributeAcceptedCommandListWithCompletion:@
readAttributeAcceptedCommandListWithCompletion :: IsMTRBaseClusterICDManagement mtrBaseClusterICDManagement => mtrBaseClusterICDManagement -> Ptr () -> IO ()
readAttributeAcceptedCommandListWithCompletion mtrBaseClusterICDManagement completion =
  sendMessage mtrBaseClusterICDManagement readAttributeAcceptedCommandListWithCompletionSelector completion

-- | @- subscribeAttributeAcceptedCommandListWithParams:subscriptionEstablished:reportHandler:@
subscribeAttributeAcceptedCommandListWithParams_subscriptionEstablished_reportHandler :: (IsMTRBaseClusterICDManagement mtrBaseClusterICDManagement, IsMTRSubscribeParams params) => mtrBaseClusterICDManagement -> params -> Ptr () -> Ptr () -> IO ()
subscribeAttributeAcceptedCommandListWithParams_subscriptionEstablished_reportHandler mtrBaseClusterICDManagement params subscriptionEstablished reportHandler =
  sendMessage mtrBaseClusterICDManagement subscribeAttributeAcceptedCommandListWithParams_subscriptionEstablished_reportHandlerSelector (toMTRSubscribeParams params) subscriptionEstablished reportHandler

-- | @+ readAttributeAcceptedCommandListWithClusterStateCache:endpoint:queue:completion:@
readAttributeAcceptedCommandListWithClusterStateCache_endpoint_queue_completion :: (IsMTRClusterStateCacheContainer clusterStateCacheContainer, IsNSNumber endpoint, IsNSObject queue) => clusterStateCacheContainer -> endpoint -> queue -> Ptr () -> IO ()
readAttributeAcceptedCommandListWithClusterStateCache_endpoint_queue_completion clusterStateCacheContainer endpoint queue completion =
  do
    cls' <- getRequiredClass "MTRBaseClusterICDManagement"
    sendClassMessage cls' readAttributeAcceptedCommandListWithClusterStateCache_endpoint_queue_completionSelector (toMTRClusterStateCacheContainer clusterStateCacheContainer) (toNSNumber endpoint) (toNSObject queue) completion

-- | @- readAttributeAttributeListWithCompletion:@
readAttributeAttributeListWithCompletion :: IsMTRBaseClusterICDManagement mtrBaseClusterICDManagement => mtrBaseClusterICDManagement -> Ptr () -> IO ()
readAttributeAttributeListWithCompletion mtrBaseClusterICDManagement completion =
  sendMessage mtrBaseClusterICDManagement readAttributeAttributeListWithCompletionSelector completion

-- | @- subscribeAttributeAttributeListWithParams:subscriptionEstablished:reportHandler:@
subscribeAttributeAttributeListWithParams_subscriptionEstablished_reportHandler :: (IsMTRBaseClusterICDManagement mtrBaseClusterICDManagement, IsMTRSubscribeParams params) => mtrBaseClusterICDManagement -> params -> Ptr () -> Ptr () -> IO ()
subscribeAttributeAttributeListWithParams_subscriptionEstablished_reportHandler mtrBaseClusterICDManagement params subscriptionEstablished reportHandler =
  sendMessage mtrBaseClusterICDManagement subscribeAttributeAttributeListWithParams_subscriptionEstablished_reportHandlerSelector (toMTRSubscribeParams params) subscriptionEstablished reportHandler

-- | @+ readAttributeAttributeListWithClusterStateCache:endpoint:queue:completion:@
readAttributeAttributeListWithClusterStateCache_endpoint_queue_completion :: (IsMTRClusterStateCacheContainer clusterStateCacheContainer, IsNSNumber endpoint, IsNSObject queue) => clusterStateCacheContainer -> endpoint -> queue -> Ptr () -> IO ()
readAttributeAttributeListWithClusterStateCache_endpoint_queue_completion clusterStateCacheContainer endpoint queue completion =
  do
    cls' <- getRequiredClass "MTRBaseClusterICDManagement"
    sendClassMessage cls' readAttributeAttributeListWithClusterStateCache_endpoint_queue_completionSelector (toMTRClusterStateCacheContainer clusterStateCacheContainer) (toNSNumber endpoint) (toNSObject queue) completion

-- | @- readAttributeFeatureMapWithCompletion:@
readAttributeFeatureMapWithCompletion :: IsMTRBaseClusterICDManagement mtrBaseClusterICDManagement => mtrBaseClusterICDManagement -> Ptr () -> IO ()
readAttributeFeatureMapWithCompletion mtrBaseClusterICDManagement completion =
  sendMessage mtrBaseClusterICDManagement readAttributeFeatureMapWithCompletionSelector completion

-- | @- subscribeAttributeFeatureMapWithParams:subscriptionEstablished:reportHandler:@
subscribeAttributeFeatureMapWithParams_subscriptionEstablished_reportHandler :: (IsMTRBaseClusterICDManagement mtrBaseClusterICDManagement, IsMTRSubscribeParams params) => mtrBaseClusterICDManagement -> params -> Ptr () -> Ptr () -> IO ()
subscribeAttributeFeatureMapWithParams_subscriptionEstablished_reportHandler mtrBaseClusterICDManagement params subscriptionEstablished reportHandler =
  sendMessage mtrBaseClusterICDManagement subscribeAttributeFeatureMapWithParams_subscriptionEstablished_reportHandlerSelector (toMTRSubscribeParams params) subscriptionEstablished reportHandler

-- | @+ readAttributeFeatureMapWithClusterStateCache:endpoint:queue:completion:@
readAttributeFeatureMapWithClusterStateCache_endpoint_queue_completion :: (IsMTRClusterStateCacheContainer clusterStateCacheContainer, IsNSNumber endpoint, IsNSObject queue) => clusterStateCacheContainer -> endpoint -> queue -> Ptr () -> IO ()
readAttributeFeatureMapWithClusterStateCache_endpoint_queue_completion clusterStateCacheContainer endpoint queue completion =
  do
    cls' <- getRequiredClass "MTRBaseClusterICDManagement"
    sendClassMessage cls' readAttributeFeatureMapWithClusterStateCache_endpoint_queue_completionSelector (toMTRClusterStateCacheContainer clusterStateCacheContainer) (toNSNumber endpoint) (toNSObject queue) completion

-- | @- readAttributeClusterRevisionWithCompletion:@
readAttributeClusterRevisionWithCompletion :: IsMTRBaseClusterICDManagement mtrBaseClusterICDManagement => mtrBaseClusterICDManagement -> Ptr () -> IO ()
readAttributeClusterRevisionWithCompletion mtrBaseClusterICDManagement completion =
  sendMessage mtrBaseClusterICDManagement readAttributeClusterRevisionWithCompletionSelector completion

-- | @- subscribeAttributeClusterRevisionWithParams:subscriptionEstablished:reportHandler:@
subscribeAttributeClusterRevisionWithParams_subscriptionEstablished_reportHandler :: (IsMTRBaseClusterICDManagement mtrBaseClusterICDManagement, IsMTRSubscribeParams params) => mtrBaseClusterICDManagement -> params -> Ptr () -> Ptr () -> IO ()
subscribeAttributeClusterRevisionWithParams_subscriptionEstablished_reportHandler mtrBaseClusterICDManagement params subscriptionEstablished reportHandler =
  sendMessage mtrBaseClusterICDManagement subscribeAttributeClusterRevisionWithParams_subscriptionEstablished_reportHandlerSelector (toMTRSubscribeParams params) subscriptionEstablished reportHandler

-- | @+ readAttributeClusterRevisionWithClusterStateCache:endpoint:queue:completion:@
readAttributeClusterRevisionWithClusterStateCache_endpoint_queue_completion :: (IsMTRClusterStateCacheContainer clusterStateCacheContainer, IsNSNumber endpoint, IsNSObject queue) => clusterStateCacheContainer -> endpoint -> queue -> Ptr () -> IO ()
readAttributeClusterRevisionWithClusterStateCache_endpoint_queue_completion clusterStateCacheContainer endpoint queue completion =
  do
    cls' <- getRequiredClass "MTRBaseClusterICDManagement"
    sendClassMessage cls' readAttributeClusterRevisionWithClusterStateCache_endpoint_queue_completionSelector (toMTRClusterStateCacheContainer clusterStateCacheContainer) (toNSNumber endpoint) (toNSObject queue) completion

-- | @- init@
init_ :: IsMTRBaseClusterICDManagement mtrBaseClusterICDManagement => mtrBaseClusterICDManagement -> IO (Id MTRBaseClusterICDManagement)
init_ mtrBaseClusterICDManagement =
  sendOwnedMessage mtrBaseClusterICDManagement initSelector

-- | @+ new@
new :: IO (Id MTRBaseClusterICDManagement)
new  =
  do
    cls' <- getRequiredClass "MTRBaseClusterICDManagement"
    sendOwnedClassMessage cls' newSelector

-- | For all instance methods (reads, writes, commands) that take a completion, the completion will be called on the provided queue.
--
-- ObjC selector: @- initWithDevice:endpointID:queue:@
initWithDevice_endpointID_queue :: (IsMTRBaseClusterICDManagement mtrBaseClusterICDManagement, IsMTRBaseDevice device, IsNSNumber endpointID, IsNSObject queue) => mtrBaseClusterICDManagement -> device -> endpointID -> queue -> IO (Id MTRBaseClusterICDManagement)
initWithDevice_endpointID_queue mtrBaseClusterICDManagement device endpointID queue =
  sendOwnedMessage mtrBaseClusterICDManagement initWithDevice_endpointID_queueSelector (toMTRBaseDevice device) (toNSNumber endpointID) (toNSObject queue)

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @registerClientWithParams:completion:@
registerClientWithParams_completionSelector :: Selector '[Id MTRICDManagementClusterRegisterClientParams, Ptr ()] ()
registerClientWithParams_completionSelector = mkSelector "registerClientWithParams:completion:"

-- | @Selector@ for @unregisterClientWithParams:completion:@
unregisterClientWithParams_completionSelector :: Selector '[Id MTRICDManagementClusterUnregisterClientParams, Ptr ()] ()
unregisterClientWithParams_completionSelector = mkSelector "unregisterClientWithParams:completion:"

-- | @Selector@ for @stayActiveRequestWithParams:completion:@
stayActiveRequestWithParams_completionSelector :: Selector '[Id MTRICDManagementClusterStayActiveRequestParams, Ptr ()] ()
stayActiveRequestWithParams_completionSelector = mkSelector "stayActiveRequestWithParams:completion:"

-- | @Selector@ for @readAttributeIdleModeDurationWithCompletion:@
readAttributeIdleModeDurationWithCompletionSelector :: Selector '[Ptr ()] ()
readAttributeIdleModeDurationWithCompletionSelector = mkSelector "readAttributeIdleModeDurationWithCompletion:"

-- | @Selector@ for @subscribeAttributeIdleModeDurationWithParams:subscriptionEstablished:reportHandler:@
subscribeAttributeIdleModeDurationWithParams_subscriptionEstablished_reportHandlerSelector :: Selector '[Id MTRSubscribeParams, Ptr (), Ptr ()] ()
subscribeAttributeIdleModeDurationWithParams_subscriptionEstablished_reportHandlerSelector = mkSelector "subscribeAttributeIdleModeDurationWithParams:subscriptionEstablished:reportHandler:"

-- | @Selector@ for @readAttributeIdleModeDurationWithClusterStateCache:endpoint:queue:completion:@
readAttributeIdleModeDurationWithClusterStateCache_endpoint_queue_completionSelector :: Selector '[Id MTRClusterStateCacheContainer, Id NSNumber, Id NSObject, Ptr ()] ()
readAttributeIdleModeDurationWithClusterStateCache_endpoint_queue_completionSelector = mkSelector "readAttributeIdleModeDurationWithClusterStateCache:endpoint:queue:completion:"

-- | @Selector@ for @readAttributeActiveModeDurationWithCompletion:@
readAttributeActiveModeDurationWithCompletionSelector :: Selector '[Ptr ()] ()
readAttributeActiveModeDurationWithCompletionSelector = mkSelector "readAttributeActiveModeDurationWithCompletion:"

-- | @Selector@ for @subscribeAttributeActiveModeDurationWithParams:subscriptionEstablished:reportHandler:@
subscribeAttributeActiveModeDurationWithParams_subscriptionEstablished_reportHandlerSelector :: Selector '[Id MTRSubscribeParams, Ptr (), Ptr ()] ()
subscribeAttributeActiveModeDurationWithParams_subscriptionEstablished_reportHandlerSelector = mkSelector "subscribeAttributeActiveModeDurationWithParams:subscriptionEstablished:reportHandler:"

-- | @Selector@ for @readAttributeActiveModeDurationWithClusterStateCache:endpoint:queue:completion:@
readAttributeActiveModeDurationWithClusterStateCache_endpoint_queue_completionSelector :: Selector '[Id MTRClusterStateCacheContainer, Id NSNumber, Id NSObject, Ptr ()] ()
readAttributeActiveModeDurationWithClusterStateCache_endpoint_queue_completionSelector = mkSelector "readAttributeActiveModeDurationWithClusterStateCache:endpoint:queue:completion:"

-- | @Selector@ for @readAttributeActiveModeThresholdWithCompletion:@
readAttributeActiveModeThresholdWithCompletionSelector :: Selector '[Ptr ()] ()
readAttributeActiveModeThresholdWithCompletionSelector = mkSelector "readAttributeActiveModeThresholdWithCompletion:"

-- | @Selector@ for @subscribeAttributeActiveModeThresholdWithParams:subscriptionEstablished:reportHandler:@
subscribeAttributeActiveModeThresholdWithParams_subscriptionEstablished_reportHandlerSelector :: Selector '[Id MTRSubscribeParams, Ptr (), Ptr ()] ()
subscribeAttributeActiveModeThresholdWithParams_subscriptionEstablished_reportHandlerSelector = mkSelector "subscribeAttributeActiveModeThresholdWithParams:subscriptionEstablished:reportHandler:"

-- | @Selector@ for @readAttributeActiveModeThresholdWithClusterStateCache:endpoint:queue:completion:@
readAttributeActiveModeThresholdWithClusterStateCache_endpoint_queue_completionSelector :: Selector '[Id MTRClusterStateCacheContainer, Id NSNumber, Id NSObject, Ptr ()] ()
readAttributeActiveModeThresholdWithClusterStateCache_endpoint_queue_completionSelector = mkSelector "readAttributeActiveModeThresholdWithClusterStateCache:endpoint:queue:completion:"

-- | @Selector@ for @readAttributeRegisteredClientsWithParams:completion:@
readAttributeRegisteredClientsWithParams_completionSelector :: Selector '[Id MTRReadParams, Ptr ()] ()
readAttributeRegisteredClientsWithParams_completionSelector = mkSelector "readAttributeRegisteredClientsWithParams:completion:"

-- | @Selector@ for @subscribeAttributeRegisteredClientsWithParams:subscriptionEstablished:reportHandler:@
subscribeAttributeRegisteredClientsWithParams_subscriptionEstablished_reportHandlerSelector :: Selector '[Id MTRSubscribeParams, Ptr (), Ptr ()] ()
subscribeAttributeRegisteredClientsWithParams_subscriptionEstablished_reportHandlerSelector = mkSelector "subscribeAttributeRegisteredClientsWithParams:subscriptionEstablished:reportHandler:"

-- | @Selector@ for @readAttributeRegisteredClientsWithClusterStateCache:endpoint:queue:completion:@
readAttributeRegisteredClientsWithClusterStateCache_endpoint_queue_completionSelector :: Selector '[Id MTRClusterStateCacheContainer, Id NSNumber, Id NSObject, Ptr ()] ()
readAttributeRegisteredClientsWithClusterStateCache_endpoint_queue_completionSelector = mkSelector "readAttributeRegisteredClientsWithClusterStateCache:endpoint:queue:completion:"

-- | @Selector@ for @readAttributeICDCounterWithCompletion:@
readAttributeICDCounterWithCompletionSelector :: Selector '[Ptr ()] ()
readAttributeICDCounterWithCompletionSelector = mkSelector "readAttributeICDCounterWithCompletion:"

-- | @Selector@ for @subscribeAttributeICDCounterWithParams:subscriptionEstablished:reportHandler:@
subscribeAttributeICDCounterWithParams_subscriptionEstablished_reportHandlerSelector :: Selector '[Id MTRSubscribeParams, Ptr (), Ptr ()] ()
subscribeAttributeICDCounterWithParams_subscriptionEstablished_reportHandlerSelector = mkSelector "subscribeAttributeICDCounterWithParams:subscriptionEstablished:reportHandler:"

-- | @Selector@ for @readAttributeICDCounterWithClusterStateCache:endpoint:queue:completion:@
readAttributeICDCounterWithClusterStateCache_endpoint_queue_completionSelector :: Selector '[Id MTRClusterStateCacheContainer, Id NSNumber, Id NSObject, Ptr ()] ()
readAttributeICDCounterWithClusterStateCache_endpoint_queue_completionSelector = mkSelector "readAttributeICDCounterWithClusterStateCache:endpoint:queue:completion:"

-- | @Selector@ for @readAttributeClientsSupportedPerFabricWithCompletion:@
readAttributeClientsSupportedPerFabricWithCompletionSelector :: Selector '[Ptr ()] ()
readAttributeClientsSupportedPerFabricWithCompletionSelector = mkSelector "readAttributeClientsSupportedPerFabricWithCompletion:"

-- | @Selector@ for @subscribeAttributeClientsSupportedPerFabricWithParams:subscriptionEstablished:reportHandler:@
subscribeAttributeClientsSupportedPerFabricWithParams_subscriptionEstablished_reportHandlerSelector :: Selector '[Id MTRSubscribeParams, Ptr (), Ptr ()] ()
subscribeAttributeClientsSupportedPerFabricWithParams_subscriptionEstablished_reportHandlerSelector = mkSelector "subscribeAttributeClientsSupportedPerFabricWithParams:subscriptionEstablished:reportHandler:"

-- | @Selector@ for @readAttributeClientsSupportedPerFabricWithClusterStateCache:endpoint:queue:completion:@
readAttributeClientsSupportedPerFabricWithClusterStateCache_endpoint_queue_completionSelector :: Selector '[Id MTRClusterStateCacheContainer, Id NSNumber, Id NSObject, Ptr ()] ()
readAttributeClientsSupportedPerFabricWithClusterStateCache_endpoint_queue_completionSelector = mkSelector "readAttributeClientsSupportedPerFabricWithClusterStateCache:endpoint:queue:completion:"

-- | @Selector@ for @readAttributeUserActiveModeTriggerHintWithCompletion:@
readAttributeUserActiveModeTriggerHintWithCompletionSelector :: Selector '[Ptr ()] ()
readAttributeUserActiveModeTriggerHintWithCompletionSelector = mkSelector "readAttributeUserActiveModeTriggerHintWithCompletion:"

-- | @Selector@ for @subscribeAttributeUserActiveModeTriggerHintWithParams:subscriptionEstablished:reportHandler:@
subscribeAttributeUserActiveModeTriggerHintWithParams_subscriptionEstablished_reportHandlerSelector :: Selector '[Id MTRSubscribeParams, Ptr (), Ptr ()] ()
subscribeAttributeUserActiveModeTriggerHintWithParams_subscriptionEstablished_reportHandlerSelector = mkSelector "subscribeAttributeUserActiveModeTriggerHintWithParams:subscriptionEstablished:reportHandler:"

-- | @Selector@ for @readAttributeUserActiveModeTriggerHintWithClusterStateCache:endpoint:queue:completion:@
readAttributeUserActiveModeTriggerHintWithClusterStateCache_endpoint_queue_completionSelector :: Selector '[Id MTRClusterStateCacheContainer, Id NSNumber, Id NSObject, Ptr ()] ()
readAttributeUserActiveModeTriggerHintWithClusterStateCache_endpoint_queue_completionSelector = mkSelector "readAttributeUserActiveModeTriggerHintWithClusterStateCache:endpoint:queue:completion:"

-- | @Selector@ for @readAttributeUserActiveModeTriggerInstructionWithCompletion:@
readAttributeUserActiveModeTriggerInstructionWithCompletionSelector :: Selector '[Ptr ()] ()
readAttributeUserActiveModeTriggerInstructionWithCompletionSelector = mkSelector "readAttributeUserActiveModeTriggerInstructionWithCompletion:"

-- | @Selector@ for @subscribeAttributeUserActiveModeTriggerInstructionWithParams:subscriptionEstablished:reportHandler:@
subscribeAttributeUserActiveModeTriggerInstructionWithParams_subscriptionEstablished_reportHandlerSelector :: Selector '[Id MTRSubscribeParams, Ptr (), Ptr ()] ()
subscribeAttributeUserActiveModeTriggerInstructionWithParams_subscriptionEstablished_reportHandlerSelector = mkSelector "subscribeAttributeUserActiveModeTriggerInstructionWithParams:subscriptionEstablished:reportHandler:"

-- | @Selector@ for @readAttributeUserActiveModeTriggerInstructionWithClusterStateCache:endpoint:queue:completion:@
readAttributeUserActiveModeTriggerInstructionWithClusterStateCache_endpoint_queue_completionSelector :: Selector '[Id MTRClusterStateCacheContainer, Id NSNumber, Id NSObject, Ptr ()] ()
readAttributeUserActiveModeTriggerInstructionWithClusterStateCache_endpoint_queue_completionSelector = mkSelector "readAttributeUserActiveModeTriggerInstructionWithClusterStateCache:endpoint:queue:completion:"

-- | @Selector@ for @readAttributeOperatingModeWithCompletion:@
readAttributeOperatingModeWithCompletionSelector :: Selector '[Ptr ()] ()
readAttributeOperatingModeWithCompletionSelector = mkSelector "readAttributeOperatingModeWithCompletion:"

-- | @Selector@ for @subscribeAttributeOperatingModeWithParams:subscriptionEstablished:reportHandler:@
subscribeAttributeOperatingModeWithParams_subscriptionEstablished_reportHandlerSelector :: Selector '[Id MTRSubscribeParams, Ptr (), Ptr ()] ()
subscribeAttributeOperatingModeWithParams_subscriptionEstablished_reportHandlerSelector = mkSelector "subscribeAttributeOperatingModeWithParams:subscriptionEstablished:reportHandler:"

-- | @Selector@ for @readAttributeOperatingModeWithClusterStateCache:endpoint:queue:completion:@
readAttributeOperatingModeWithClusterStateCache_endpoint_queue_completionSelector :: Selector '[Id MTRClusterStateCacheContainer, Id NSNumber, Id NSObject, Ptr ()] ()
readAttributeOperatingModeWithClusterStateCache_endpoint_queue_completionSelector = mkSelector "readAttributeOperatingModeWithClusterStateCache:endpoint:queue:completion:"

-- | @Selector@ for @readAttributeMaximumCheckInBackOffWithCompletion:@
readAttributeMaximumCheckInBackOffWithCompletionSelector :: Selector '[Ptr ()] ()
readAttributeMaximumCheckInBackOffWithCompletionSelector = mkSelector "readAttributeMaximumCheckInBackOffWithCompletion:"

-- | @Selector@ for @subscribeAttributeMaximumCheckInBackOffWithParams:subscriptionEstablished:reportHandler:@
subscribeAttributeMaximumCheckInBackOffWithParams_subscriptionEstablished_reportHandlerSelector :: Selector '[Id MTRSubscribeParams, Ptr (), Ptr ()] ()
subscribeAttributeMaximumCheckInBackOffWithParams_subscriptionEstablished_reportHandlerSelector = mkSelector "subscribeAttributeMaximumCheckInBackOffWithParams:subscriptionEstablished:reportHandler:"

-- | @Selector@ for @readAttributeMaximumCheckInBackOffWithClusterStateCache:endpoint:queue:completion:@
readAttributeMaximumCheckInBackOffWithClusterStateCache_endpoint_queue_completionSelector :: Selector '[Id MTRClusterStateCacheContainer, Id NSNumber, Id NSObject, Ptr ()] ()
readAttributeMaximumCheckInBackOffWithClusterStateCache_endpoint_queue_completionSelector = mkSelector "readAttributeMaximumCheckInBackOffWithClusterStateCache:endpoint:queue:completion:"

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
initSelector :: Selector '[] (Id MTRBaseClusterICDManagement)
initSelector = mkSelector "init"

-- | @Selector@ for @new@
newSelector :: Selector '[] (Id MTRBaseClusterICDManagement)
newSelector = mkSelector "new"

-- | @Selector@ for @initWithDevice:endpointID:queue:@
initWithDevice_endpointID_queueSelector :: Selector '[Id MTRBaseDevice, Id NSNumber, Id NSObject] (Id MTRBaseClusterICDManagement)
initWithDevice_endpointID_queueSelector = mkSelector "initWithDevice:endpointID:queue:"

