{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Cluster Thread Border Router Management
--
-- Manage the Thread network of Thread Border Router
--
-- Generated bindings for @MTRBaseClusterThreadBorderRouterManagement@.
module ObjC.Matter.MTRBaseClusterThreadBorderRouterManagement
  ( MTRBaseClusterThreadBorderRouterManagement
  , IsMTRBaseClusterThreadBorderRouterManagement(..)
  , getActiveDatasetRequestWithParams_completion
  , getActiveDatasetRequestWithCompletion
  , getPendingDatasetRequestWithParams_completion
  , getPendingDatasetRequestWithCompletion
  , setActiveDatasetRequestWithParams_completion
  , setPendingDatasetRequestWithParams_completion
  , readAttributeBorderRouterNameWithCompletion
  , subscribeAttributeBorderRouterNameWithParams_subscriptionEstablished_reportHandler
  , readAttributeBorderRouterNameWithClusterStateCache_endpoint_queue_completion
  , readAttributeBorderAgentIDWithCompletion
  , subscribeAttributeBorderAgentIDWithParams_subscriptionEstablished_reportHandler
  , readAttributeBorderAgentIDWithClusterStateCache_endpoint_queue_completion
  , readAttributeThreadVersionWithCompletion
  , subscribeAttributeThreadVersionWithParams_subscriptionEstablished_reportHandler
  , readAttributeThreadVersionWithClusterStateCache_endpoint_queue_completion
  , readAttributeInterfaceEnabledWithCompletion
  , subscribeAttributeInterfaceEnabledWithParams_subscriptionEstablished_reportHandler
  , readAttributeInterfaceEnabledWithClusterStateCache_endpoint_queue_completion
  , readAttributeActiveDatasetTimestampWithCompletion
  , subscribeAttributeActiveDatasetTimestampWithParams_subscriptionEstablished_reportHandler
  , readAttributeActiveDatasetTimestampWithClusterStateCache_endpoint_queue_completion
  , readAttributePendingDatasetTimestampWithCompletion
  , subscribeAttributePendingDatasetTimestampWithParams_subscriptionEstablished_reportHandler
  , readAttributePendingDatasetTimestampWithClusterStateCache_endpoint_queue_completion
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
  , getActiveDatasetRequestWithCompletionSelector
  , getActiveDatasetRequestWithParams_completionSelector
  , getPendingDatasetRequestWithCompletionSelector
  , getPendingDatasetRequestWithParams_completionSelector
  , initSelector
  , initWithDevice_endpointID_queueSelector
  , newSelector
  , readAttributeAcceptedCommandListWithClusterStateCache_endpoint_queue_completionSelector
  , readAttributeAcceptedCommandListWithCompletionSelector
  , readAttributeActiveDatasetTimestampWithClusterStateCache_endpoint_queue_completionSelector
  , readAttributeActiveDatasetTimestampWithCompletionSelector
  , readAttributeAttributeListWithClusterStateCache_endpoint_queue_completionSelector
  , readAttributeAttributeListWithCompletionSelector
  , readAttributeBorderAgentIDWithClusterStateCache_endpoint_queue_completionSelector
  , readAttributeBorderAgentIDWithCompletionSelector
  , readAttributeBorderRouterNameWithClusterStateCache_endpoint_queue_completionSelector
  , readAttributeBorderRouterNameWithCompletionSelector
  , readAttributeClusterRevisionWithClusterStateCache_endpoint_queue_completionSelector
  , readAttributeClusterRevisionWithCompletionSelector
  , readAttributeFeatureMapWithClusterStateCache_endpoint_queue_completionSelector
  , readAttributeFeatureMapWithCompletionSelector
  , readAttributeGeneratedCommandListWithClusterStateCache_endpoint_queue_completionSelector
  , readAttributeGeneratedCommandListWithCompletionSelector
  , readAttributeInterfaceEnabledWithClusterStateCache_endpoint_queue_completionSelector
  , readAttributeInterfaceEnabledWithCompletionSelector
  , readAttributePendingDatasetTimestampWithClusterStateCache_endpoint_queue_completionSelector
  , readAttributePendingDatasetTimestampWithCompletionSelector
  , readAttributeThreadVersionWithClusterStateCache_endpoint_queue_completionSelector
  , readAttributeThreadVersionWithCompletionSelector
  , setActiveDatasetRequestWithParams_completionSelector
  , setPendingDatasetRequestWithParams_completionSelector
  , subscribeAttributeAcceptedCommandListWithParams_subscriptionEstablished_reportHandlerSelector
  , subscribeAttributeActiveDatasetTimestampWithParams_subscriptionEstablished_reportHandlerSelector
  , subscribeAttributeAttributeListWithParams_subscriptionEstablished_reportHandlerSelector
  , subscribeAttributeBorderAgentIDWithParams_subscriptionEstablished_reportHandlerSelector
  , subscribeAttributeBorderRouterNameWithParams_subscriptionEstablished_reportHandlerSelector
  , subscribeAttributeClusterRevisionWithParams_subscriptionEstablished_reportHandlerSelector
  , subscribeAttributeFeatureMapWithParams_subscriptionEstablished_reportHandlerSelector
  , subscribeAttributeGeneratedCommandListWithParams_subscriptionEstablished_reportHandlerSelector
  , subscribeAttributeInterfaceEnabledWithParams_subscriptionEstablished_reportHandlerSelector
  , subscribeAttributePendingDatasetTimestampWithParams_subscriptionEstablished_reportHandlerSelector
  , subscribeAttributeThreadVersionWithParams_subscriptionEstablished_reportHandlerSelector


  ) where

import Foreign.Ptr (Ptr, FunPtr)
import Foreign.C.Types

import ObjC.Runtime.Types
import ObjC.Runtime.Message (sendMessage, sendOwnedMessage, sendClassMessage, sendOwnedClassMessage)
import ObjC.Runtime.Selector (mkSelector)
import ObjC.Runtime.Class (getRequiredClass)

import ObjC.Matter.Internal.Classes
import ObjC.Foundation.Internal.Classes

-- | Command GetActiveDatasetRequest
--
-- This command SHALL be used to request the active operational dataset of the Thread network to which the border router is connected.
--
-- ObjC selector: @- getActiveDatasetRequestWithParams:completion:@
getActiveDatasetRequestWithParams_completion :: (IsMTRBaseClusterThreadBorderRouterManagement mtrBaseClusterThreadBorderRouterManagement, IsMTRThreadBorderRouterManagementClusterGetActiveDatasetRequestParams params) => mtrBaseClusterThreadBorderRouterManagement -> params -> Ptr () -> IO ()
getActiveDatasetRequestWithParams_completion mtrBaseClusterThreadBorderRouterManagement params completion =
  sendMessage mtrBaseClusterThreadBorderRouterManagement getActiveDatasetRequestWithParams_completionSelector (toMTRThreadBorderRouterManagementClusterGetActiveDatasetRequestParams params) completion

-- | @- getActiveDatasetRequestWithCompletion:@
getActiveDatasetRequestWithCompletion :: IsMTRBaseClusterThreadBorderRouterManagement mtrBaseClusterThreadBorderRouterManagement => mtrBaseClusterThreadBorderRouterManagement -> Ptr () -> IO ()
getActiveDatasetRequestWithCompletion mtrBaseClusterThreadBorderRouterManagement completion =
  sendMessage mtrBaseClusterThreadBorderRouterManagement getActiveDatasetRequestWithCompletionSelector completion

-- | Command GetPendingDatasetRequest
--
-- This command SHALL be used to request the pending dataset of the Thread network to which the border router is connected.
--
-- ObjC selector: @- getPendingDatasetRequestWithParams:completion:@
getPendingDatasetRequestWithParams_completion :: (IsMTRBaseClusterThreadBorderRouterManagement mtrBaseClusterThreadBorderRouterManagement, IsMTRThreadBorderRouterManagementClusterGetPendingDatasetRequestParams params) => mtrBaseClusterThreadBorderRouterManagement -> params -> Ptr () -> IO ()
getPendingDatasetRequestWithParams_completion mtrBaseClusterThreadBorderRouterManagement params completion =
  sendMessage mtrBaseClusterThreadBorderRouterManagement getPendingDatasetRequestWithParams_completionSelector (toMTRThreadBorderRouterManagementClusterGetPendingDatasetRequestParams params) completion

-- | @- getPendingDatasetRequestWithCompletion:@
getPendingDatasetRequestWithCompletion :: IsMTRBaseClusterThreadBorderRouterManagement mtrBaseClusterThreadBorderRouterManagement => mtrBaseClusterThreadBorderRouterManagement -> Ptr () -> IO ()
getPendingDatasetRequestWithCompletion mtrBaseClusterThreadBorderRouterManagement completion =
  sendMessage mtrBaseClusterThreadBorderRouterManagement getPendingDatasetRequestWithCompletionSelector completion

-- | Command SetActiveDatasetRequest
--
-- This command SHALL be used to set the active Dataset of the Thread network to which the Border Router is connected, when there is no active dataset already.
--
-- ObjC selector: @- setActiveDatasetRequestWithParams:completion:@
setActiveDatasetRequestWithParams_completion :: (IsMTRBaseClusterThreadBorderRouterManagement mtrBaseClusterThreadBorderRouterManagement, IsMTRThreadBorderRouterManagementClusterSetActiveDatasetRequestParams params) => mtrBaseClusterThreadBorderRouterManagement -> params -> Ptr () -> IO ()
setActiveDatasetRequestWithParams_completion mtrBaseClusterThreadBorderRouterManagement params completion =
  sendMessage mtrBaseClusterThreadBorderRouterManagement setActiveDatasetRequestWithParams_completionSelector (toMTRThreadBorderRouterManagementClusterSetActiveDatasetRequestParams params) completion

-- | Command SetPendingDatasetRequest
--
-- This command SHALL be used to set or update the pending Dataset of the Thread network to which the Border Router is connected, if the Border Router supports PANChange Feature.
--
-- ObjC selector: @- setPendingDatasetRequestWithParams:completion:@
setPendingDatasetRequestWithParams_completion :: (IsMTRBaseClusterThreadBorderRouterManagement mtrBaseClusterThreadBorderRouterManagement, IsMTRThreadBorderRouterManagementClusterSetPendingDatasetRequestParams params) => mtrBaseClusterThreadBorderRouterManagement -> params -> Ptr () -> IO ()
setPendingDatasetRequestWithParams_completion mtrBaseClusterThreadBorderRouterManagement params completion =
  sendMessage mtrBaseClusterThreadBorderRouterManagement setPendingDatasetRequestWithParams_completionSelector (toMTRThreadBorderRouterManagementClusterSetPendingDatasetRequestParams params) completion

-- | @- readAttributeBorderRouterNameWithCompletion:@
readAttributeBorderRouterNameWithCompletion :: IsMTRBaseClusterThreadBorderRouterManagement mtrBaseClusterThreadBorderRouterManagement => mtrBaseClusterThreadBorderRouterManagement -> Ptr () -> IO ()
readAttributeBorderRouterNameWithCompletion mtrBaseClusterThreadBorderRouterManagement completion =
  sendMessage mtrBaseClusterThreadBorderRouterManagement readAttributeBorderRouterNameWithCompletionSelector completion

-- | @- subscribeAttributeBorderRouterNameWithParams:subscriptionEstablished:reportHandler:@
subscribeAttributeBorderRouterNameWithParams_subscriptionEstablished_reportHandler :: (IsMTRBaseClusterThreadBorderRouterManagement mtrBaseClusterThreadBorderRouterManagement, IsMTRSubscribeParams params) => mtrBaseClusterThreadBorderRouterManagement -> params -> Ptr () -> Ptr () -> IO ()
subscribeAttributeBorderRouterNameWithParams_subscriptionEstablished_reportHandler mtrBaseClusterThreadBorderRouterManagement params subscriptionEstablished reportHandler =
  sendMessage mtrBaseClusterThreadBorderRouterManagement subscribeAttributeBorderRouterNameWithParams_subscriptionEstablished_reportHandlerSelector (toMTRSubscribeParams params) subscriptionEstablished reportHandler

-- | @+ readAttributeBorderRouterNameWithClusterStateCache:endpoint:queue:completion:@
readAttributeBorderRouterNameWithClusterStateCache_endpoint_queue_completion :: (IsMTRClusterStateCacheContainer clusterStateCacheContainer, IsNSNumber endpoint, IsNSObject queue) => clusterStateCacheContainer -> endpoint -> queue -> Ptr () -> IO ()
readAttributeBorderRouterNameWithClusterStateCache_endpoint_queue_completion clusterStateCacheContainer endpoint queue completion =
  do
    cls' <- getRequiredClass "MTRBaseClusterThreadBorderRouterManagement"
    sendClassMessage cls' readAttributeBorderRouterNameWithClusterStateCache_endpoint_queue_completionSelector (toMTRClusterStateCacheContainer clusterStateCacheContainer) (toNSNumber endpoint) (toNSObject queue) completion

-- | @- readAttributeBorderAgentIDWithCompletion:@
readAttributeBorderAgentIDWithCompletion :: IsMTRBaseClusterThreadBorderRouterManagement mtrBaseClusterThreadBorderRouterManagement => mtrBaseClusterThreadBorderRouterManagement -> Ptr () -> IO ()
readAttributeBorderAgentIDWithCompletion mtrBaseClusterThreadBorderRouterManagement completion =
  sendMessage mtrBaseClusterThreadBorderRouterManagement readAttributeBorderAgentIDWithCompletionSelector completion

-- | @- subscribeAttributeBorderAgentIDWithParams:subscriptionEstablished:reportHandler:@
subscribeAttributeBorderAgentIDWithParams_subscriptionEstablished_reportHandler :: (IsMTRBaseClusterThreadBorderRouterManagement mtrBaseClusterThreadBorderRouterManagement, IsMTRSubscribeParams params) => mtrBaseClusterThreadBorderRouterManagement -> params -> Ptr () -> Ptr () -> IO ()
subscribeAttributeBorderAgentIDWithParams_subscriptionEstablished_reportHandler mtrBaseClusterThreadBorderRouterManagement params subscriptionEstablished reportHandler =
  sendMessage mtrBaseClusterThreadBorderRouterManagement subscribeAttributeBorderAgentIDWithParams_subscriptionEstablished_reportHandlerSelector (toMTRSubscribeParams params) subscriptionEstablished reportHandler

-- | @+ readAttributeBorderAgentIDWithClusterStateCache:endpoint:queue:completion:@
readAttributeBorderAgentIDWithClusterStateCache_endpoint_queue_completion :: (IsMTRClusterStateCacheContainer clusterStateCacheContainer, IsNSNumber endpoint, IsNSObject queue) => clusterStateCacheContainer -> endpoint -> queue -> Ptr () -> IO ()
readAttributeBorderAgentIDWithClusterStateCache_endpoint_queue_completion clusterStateCacheContainer endpoint queue completion =
  do
    cls' <- getRequiredClass "MTRBaseClusterThreadBorderRouterManagement"
    sendClassMessage cls' readAttributeBorderAgentIDWithClusterStateCache_endpoint_queue_completionSelector (toMTRClusterStateCacheContainer clusterStateCacheContainer) (toNSNumber endpoint) (toNSObject queue) completion

-- | @- readAttributeThreadVersionWithCompletion:@
readAttributeThreadVersionWithCompletion :: IsMTRBaseClusterThreadBorderRouterManagement mtrBaseClusterThreadBorderRouterManagement => mtrBaseClusterThreadBorderRouterManagement -> Ptr () -> IO ()
readAttributeThreadVersionWithCompletion mtrBaseClusterThreadBorderRouterManagement completion =
  sendMessage mtrBaseClusterThreadBorderRouterManagement readAttributeThreadVersionWithCompletionSelector completion

-- | @- subscribeAttributeThreadVersionWithParams:subscriptionEstablished:reportHandler:@
subscribeAttributeThreadVersionWithParams_subscriptionEstablished_reportHandler :: (IsMTRBaseClusterThreadBorderRouterManagement mtrBaseClusterThreadBorderRouterManagement, IsMTRSubscribeParams params) => mtrBaseClusterThreadBorderRouterManagement -> params -> Ptr () -> Ptr () -> IO ()
subscribeAttributeThreadVersionWithParams_subscriptionEstablished_reportHandler mtrBaseClusterThreadBorderRouterManagement params subscriptionEstablished reportHandler =
  sendMessage mtrBaseClusterThreadBorderRouterManagement subscribeAttributeThreadVersionWithParams_subscriptionEstablished_reportHandlerSelector (toMTRSubscribeParams params) subscriptionEstablished reportHandler

-- | @+ readAttributeThreadVersionWithClusterStateCache:endpoint:queue:completion:@
readAttributeThreadVersionWithClusterStateCache_endpoint_queue_completion :: (IsMTRClusterStateCacheContainer clusterStateCacheContainer, IsNSNumber endpoint, IsNSObject queue) => clusterStateCacheContainer -> endpoint -> queue -> Ptr () -> IO ()
readAttributeThreadVersionWithClusterStateCache_endpoint_queue_completion clusterStateCacheContainer endpoint queue completion =
  do
    cls' <- getRequiredClass "MTRBaseClusterThreadBorderRouterManagement"
    sendClassMessage cls' readAttributeThreadVersionWithClusterStateCache_endpoint_queue_completionSelector (toMTRClusterStateCacheContainer clusterStateCacheContainer) (toNSNumber endpoint) (toNSObject queue) completion

-- | @- readAttributeInterfaceEnabledWithCompletion:@
readAttributeInterfaceEnabledWithCompletion :: IsMTRBaseClusterThreadBorderRouterManagement mtrBaseClusterThreadBorderRouterManagement => mtrBaseClusterThreadBorderRouterManagement -> Ptr () -> IO ()
readAttributeInterfaceEnabledWithCompletion mtrBaseClusterThreadBorderRouterManagement completion =
  sendMessage mtrBaseClusterThreadBorderRouterManagement readAttributeInterfaceEnabledWithCompletionSelector completion

-- | @- subscribeAttributeInterfaceEnabledWithParams:subscriptionEstablished:reportHandler:@
subscribeAttributeInterfaceEnabledWithParams_subscriptionEstablished_reportHandler :: (IsMTRBaseClusterThreadBorderRouterManagement mtrBaseClusterThreadBorderRouterManagement, IsMTRSubscribeParams params) => mtrBaseClusterThreadBorderRouterManagement -> params -> Ptr () -> Ptr () -> IO ()
subscribeAttributeInterfaceEnabledWithParams_subscriptionEstablished_reportHandler mtrBaseClusterThreadBorderRouterManagement params subscriptionEstablished reportHandler =
  sendMessage mtrBaseClusterThreadBorderRouterManagement subscribeAttributeInterfaceEnabledWithParams_subscriptionEstablished_reportHandlerSelector (toMTRSubscribeParams params) subscriptionEstablished reportHandler

-- | @+ readAttributeInterfaceEnabledWithClusterStateCache:endpoint:queue:completion:@
readAttributeInterfaceEnabledWithClusterStateCache_endpoint_queue_completion :: (IsMTRClusterStateCacheContainer clusterStateCacheContainer, IsNSNumber endpoint, IsNSObject queue) => clusterStateCacheContainer -> endpoint -> queue -> Ptr () -> IO ()
readAttributeInterfaceEnabledWithClusterStateCache_endpoint_queue_completion clusterStateCacheContainer endpoint queue completion =
  do
    cls' <- getRequiredClass "MTRBaseClusterThreadBorderRouterManagement"
    sendClassMessage cls' readAttributeInterfaceEnabledWithClusterStateCache_endpoint_queue_completionSelector (toMTRClusterStateCacheContainer clusterStateCacheContainer) (toNSNumber endpoint) (toNSObject queue) completion

-- | @- readAttributeActiveDatasetTimestampWithCompletion:@
readAttributeActiveDatasetTimestampWithCompletion :: IsMTRBaseClusterThreadBorderRouterManagement mtrBaseClusterThreadBorderRouterManagement => mtrBaseClusterThreadBorderRouterManagement -> Ptr () -> IO ()
readAttributeActiveDatasetTimestampWithCompletion mtrBaseClusterThreadBorderRouterManagement completion =
  sendMessage mtrBaseClusterThreadBorderRouterManagement readAttributeActiveDatasetTimestampWithCompletionSelector completion

-- | @- subscribeAttributeActiveDatasetTimestampWithParams:subscriptionEstablished:reportHandler:@
subscribeAttributeActiveDatasetTimestampWithParams_subscriptionEstablished_reportHandler :: (IsMTRBaseClusterThreadBorderRouterManagement mtrBaseClusterThreadBorderRouterManagement, IsMTRSubscribeParams params) => mtrBaseClusterThreadBorderRouterManagement -> params -> Ptr () -> Ptr () -> IO ()
subscribeAttributeActiveDatasetTimestampWithParams_subscriptionEstablished_reportHandler mtrBaseClusterThreadBorderRouterManagement params subscriptionEstablished reportHandler =
  sendMessage mtrBaseClusterThreadBorderRouterManagement subscribeAttributeActiveDatasetTimestampWithParams_subscriptionEstablished_reportHandlerSelector (toMTRSubscribeParams params) subscriptionEstablished reportHandler

-- | @+ readAttributeActiveDatasetTimestampWithClusterStateCache:endpoint:queue:completion:@
readAttributeActiveDatasetTimestampWithClusterStateCache_endpoint_queue_completion :: (IsMTRClusterStateCacheContainer clusterStateCacheContainer, IsNSNumber endpoint, IsNSObject queue) => clusterStateCacheContainer -> endpoint -> queue -> Ptr () -> IO ()
readAttributeActiveDatasetTimestampWithClusterStateCache_endpoint_queue_completion clusterStateCacheContainer endpoint queue completion =
  do
    cls' <- getRequiredClass "MTRBaseClusterThreadBorderRouterManagement"
    sendClassMessage cls' readAttributeActiveDatasetTimestampWithClusterStateCache_endpoint_queue_completionSelector (toMTRClusterStateCacheContainer clusterStateCacheContainer) (toNSNumber endpoint) (toNSObject queue) completion

-- | @- readAttributePendingDatasetTimestampWithCompletion:@
readAttributePendingDatasetTimestampWithCompletion :: IsMTRBaseClusterThreadBorderRouterManagement mtrBaseClusterThreadBorderRouterManagement => mtrBaseClusterThreadBorderRouterManagement -> Ptr () -> IO ()
readAttributePendingDatasetTimestampWithCompletion mtrBaseClusterThreadBorderRouterManagement completion =
  sendMessage mtrBaseClusterThreadBorderRouterManagement readAttributePendingDatasetTimestampWithCompletionSelector completion

-- | @- subscribeAttributePendingDatasetTimestampWithParams:subscriptionEstablished:reportHandler:@
subscribeAttributePendingDatasetTimestampWithParams_subscriptionEstablished_reportHandler :: (IsMTRBaseClusterThreadBorderRouterManagement mtrBaseClusterThreadBorderRouterManagement, IsMTRSubscribeParams params) => mtrBaseClusterThreadBorderRouterManagement -> params -> Ptr () -> Ptr () -> IO ()
subscribeAttributePendingDatasetTimestampWithParams_subscriptionEstablished_reportHandler mtrBaseClusterThreadBorderRouterManagement params subscriptionEstablished reportHandler =
  sendMessage mtrBaseClusterThreadBorderRouterManagement subscribeAttributePendingDatasetTimestampWithParams_subscriptionEstablished_reportHandlerSelector (toMTRSubscribeParams params) subscriptionEstablished reportHandler

-- | @+ readAttributePendingDatasetTimestampWithClusterStateCache:endpoint:queue:completion:@
readAttributePendingDatasetTimestampWithClusterStateCache_endpoint_queue_completion :: (IsMTRClusterStateCacheContainer clusterStateCacheContainer, IsNSNumber endpoint, IsNSObject queue) => clusterStateCacheContainer -> endpoint -> queue -> Ptr () -> IO ()
readAttributePendingDatasetTimestampWithClusterStateCache_endpoint_queue_completion clusterStateCacheContainer endpoint queue completion =
  do
    cls' <- getRequiredClass "MTRBaseClusterThreadBorderRouterManagement"
    sendClassMessage cls' readAttributePendingDatasetTimestampWithClusterStateCache_endpoint_queue_completionSelector (toMTRClusterStateCacheContainer clusterStateCacheContainer) (toNSNumber endpoint) (toNSObject queue) completion

-- | @- readAttributeGeneratedCommandListWithCompletion:@
readAttributeGeneratedCommandListWithCompletion :: IsMTRBaseClusterThreadBorderRouterManagement mtrBaseClusterThreadBorderRouterManagement => mtrBaseClusterThreadBorderRouterManagement -> Ptr () -> IO ()
readAttributeGeneratedCommandListWithCompletion mtrBaseClusterThreadBorderRouterManagement completion =
  sendMessage mtrBaseClusterThreadBorderRouterManagement readAttributeGeneratedCommandListWithCompletionSelector completion

-- | @- subscribeAttributeGeneratedCommandListWithParams:subscriptionEstablished:reportHandler:@
subscribeAttributeGeneratedCommandListWithParams_subscriptionEstablished_reportHandler :: (IsMTRBaseClusterThreadBorderRouterManagement mtrBaseClusterThreadBorderRouterManagement, IsMTRSubscribeParams params) => mtrBaseClusterThreadBorderRouterManagement -> params -> Ptr () -> Ptr () -> IO ()
subscribeAttributeGeneratedCommandListWithParams_subscriptionEstablished_reportHandler mtrBaseClusterThreadBorderRouterManagement params subscriptionEstablished reportHandler =
  sendMessage mtrBaseClusterThreadBorderRouterManagement subscribeAttributeGeneratedCommandListWithParams_subscriptionEstablished_reportHandlerSelector (toMTRSubscribeParams params) subscriptionEstablished reportHandler

-- | @+ readAttributeGeneratedCommandListWithClusterStateCache:endpoint:queue:completion:@
readAttributeGeneratedCommandListWithClusterStateCache_endpoint_queue_completion :: (IsMTRClusterStateCacheContainer clusterStateCacheContainer, IsNSNumber endpoint, IsNSObject queue) => clusterStateCacheContainer -> endpoint -> queue -> Ptr () -> IO ()
readAttributeGeneratedCommandListWithClusterStateCache_endpoint_queue_completion clusterStateCacheContainer endpoint queue completion =
  do
    cls' <- getRequiredClass "MTRBaseClusterThreadBorderRouterManagement"
    sendClassMessage cls' readAttributeGeneratedCommandListWithClusterStateCache_endpoint_queue_completionSelector (toMTRClusterStateCacheContainer clusterStateCacheContainer) (toNSNumber endpoint) (toNSObject queue) completion

-- | @- readAttributeAcceptedCommandListWithCompletion:@
readAttributeAcceptedCommandListWithCompletion :: IsMTRBaseClusterThreadBorderRouterManagement mtrBaseClusterThreadBorderRouterManagement => mtrBaseClusterThreadBorderRouterManagement -> Ptr () -> IO ()
readAttributeAcceptedCommandListWithCompletion mtrBaseClusterThreadBorderRouterManagement completion =
  sendMessage mtrBaseClusterThreadBorderRouterManagement readAttributeAcceptedCommandListWithCompletionSelector completion

-- | @- subscribeAttributeAcceptedCommandListWithParams:subscriptionEstablished:reportHandler:@
subscribeAttributeAcceptedCommandListWithParams_subscriptionEstablished_reportHandler :: (IsMTRBaseClusterThreadBorderRouterManagement mtrBaseClusterThreadBorderRouterManagement, IsMTRSubscribeParams params) => mtrBaseClusterThreadBorderRouterManagement -> params -> Ptr () -> Ptr () -> IO ()
subscribeAttributeAcceptedCommandListWithParams_subscriptionEstablished_reportHandler mtrBaseClusterThreadBorderRouterManagement params subscriptionEstablished reportHandler =
  sendMessage mtrBaseClusterThreadBorderRouterManagement subscribeAttributeAcceptedCommandListWithParams_subscriptionEstablished_reportHandlerSelector (toMTRSubscribeParams params) subscriptionEstablished reportHandler

-- | @+ readAttributeAcceptedCommandListWithClusterStateCache:endpoint:queue:completion:@
readAttributeAcceptedCommandListWithClusterStateCache_endpoint_queue_completion :: (IsMTRClusterStateCacheContainer clusterStateCacheContainer, IsNSNumber endpoint, IsNSObject queue) => clusterStateCacheContainer -> endpoint -> queue -> Ptr () -> IO ()
readAttributeAcceptedCommandListWithClusterStateCache_endpoint_queue_completion clusterStateCacheContainer endpoint queue completion =
  do
    cls' <- getRequiredClass "MTRBaseClusterThreadBorderRouterManagement"
    sendClassMessage cls' readAttributeAcceptedCommandListWithClusterStateCache_endpoint_queue_completionSelector (toMTRClusterStateCacheContainer clusterStateCacheContainer) (toNSNumber endpoint) (toNSObject queue) completion

-- | @- readAttributeAttributeListWithCompletion:@
readAttributeAttributeListWithCompletion :: IsMTRBaseClusterThreadBorderRouterManagement mtrBaseClusterThreadBorderRouterManagement => mtrBaseClusterThreadBorderRouterManagement -> Ptr () -> IO ()
readAttributeAttributeListWithCompletion mtrBaseClusterThreadBorderRouterManagement completion =
  sendMessage mtrBaseClusterThreadBorderRouterManagement readAttributeAttributeListWithCompletionSelector completion

-- | @- subscribeAttributeAttributeListWithParams:subscriptionEstablished:reportHandler:@
subscribeAttributeAttributeListWithParams_subscriptionEstablished_reportHandler :: (IsMTRBaseClusterThreadBorderRouterManagement mtrBaseClusterThreadBorderRouterManagement, IsMTRSubscribeParams params) => mtrBaseClusterThreadBorderRouterManagement -> params -> Ptr () -> Ptr () -> IO ()
subscribeAttributeAttributeListWithParams_subscriptionEstablished_reportHandler mtrBaseClusterThreadBorderRouterManagement params subscriptionEstablished reportHandler =
  sendMessage mtrBaseClusterThreadBorderRouterManagement subscribeAttributeAttributeListWithParams_subscriptionEstablished_reportHandlerSelector (toMTRSubscribeParams params) subscriptionEstablished reportHandler

-- | @+ readAttributeAttributeListWithClusterStateCache:endpoint:queue:completion:@
readAttributeAttributeListWithClusterStateCache_endpoint_queue_completion :: (IsMTRClusterStateCacheContainer clusterStateCacheContainer, IsNSNumber endpoint, IsNSObject queue) => clusterStateCacheContainer -> endpoint -> queue -> Ptr () -> IO ()
readAttributeAttributeListWithClusterStateCache_endpoint_queue_completion clusterStateCacheContainer endpoint queue completion =
  do
    cls' <- getRequiredClass "MTRBaseClusterThreadBorderRouterManagement"
    sendClassMessage cls' readAttributeAttributeListWithClusterStateCache_endpoint_queue_completionSelector (toMTRClusterStateCacheContainer clusterStateCacheContainer) (toNSNumber endpoint) (toNSObject queue) completion

-- | @- readAttributeFeatureMapWithCompletion:@
readAttributeFeatureMapWithCompletion :: IsMTRBaseClusterThreadBorderRouterManagement mtrBaseClusterThreadBorderRouterManagement => mtrBaseClusterThreadBorderRouterManagement -> Ptr () -> IO ()
readAttributeFeatureMapWithCompletion mtrBaseClusterThreadBorderRouterManagement completion =
  sendMessage mtrBaseClusterThreadBorderRouterManagement readAttributeFeatureMapWithCompletionSelector completion

-- | @- subscribeAttributeFeatureMapWithParams:subscriptionEstablished:reportHandler:@
subscribeAttributeFeatureMapWithParams_subscriptionEstablished_reportHandler :: (IsMTRBaseClusterThreadBorderRouterManagement mtrBaseClusterThreadBorderRouterManagement, IsMTRSubscribeParams params) => mtrBaseClusterThreadBorderRouterManagement -> params -> Ptr () -> Ptr () -> IO ()
subscribeAttributeFeatureMapWithParams_subscriptionEstablished_reportHandler mtrBaseClusterThreadBorderRouterManagement params subscriptionEstablished reportHandler =
  sendMessage mtrBaseClusterThreadBorderRouterManagement subscribeAttributeFeatureMapWithParams_subscriptionEstablished_reportHandlerSelector (toMTRSubscribeParams params) subscriptionEstablished reportHandler

-- | @+ readAttributeFeatureMapWithClusterStateCache:endpoint:queue:completion:@
readAttributeFeatureMapWithClusterStateCache_endpoint_queue_completion :: (IsMTRClusterStateCacheContainer clusterStateCacheContainer, IsNSNumber endpoint, IsNSObject queue) => clusterStateCacheContainer -> endpoint -> queue -> Ptr () -> IO ()
readAttributeFeatureMapWithClusterStateCache_endpoint_queue_completion clusterStateCacheContainer endpoint queue completion =
  do
    cls' <- getRequiredClass "MTRBaseClusterThreadBorderRouterManagement"
    sendClassMessage cls' readAttributeFeatureMapWithClusterStateCache_endpoint_queue_completionSelector (toMTRClusterStateCacheContainer clusterStateCacheContainer) (toNSNumber endpoint) (toNSObject queue) completion

-- | @- readAttributeClusterRevisionWithCompletion:@
readAttributeClusterRevisionWithCompletion :: IsMTRBaseClusterThreadBorderRouterManagement mtrBaseClusterThreadBorderRouterManagement => mtrBaseClusterThreadBorderRouterManagement -> Ptr () -> IO ()
readAttributeClusterRevisionWithCompletion mtrBaseClusterThreadBorderRouterManagement completion =
  sendMessage mtrBaseClusterThreadBorderRouterManagement readAttributeClusterRevisionWithCompletionSelector completion

-- | @- subscribeAttributeClusterRevisionWithParams:subscriptionEstablished:reportHandler:@
subscribeAttributeClusterRevisionWithParams_subscriptionEstablished_reportHandler :: (IsMTRBaseClusterThreadBorderRouterManagement mtrBaseClusterThreadBorderRouterManagement, IsMTRSubscribeParams params) => mtrBaseClusterThreadBorderRouterManagement -> params -> Ptr () -> Ptr () -> IO ()
subscribeAttributeClusterRevisionWithParams_subscriptionEstablished_reportHandler mtrBaseClusterThreadBorderRouterManagement params subscriptionEstablished reportHandler =
  sendMessage mtrBaseClusterThreadBorderRouterManagement subscribeAttributeClusterRevisionWithParams_subscriptionEstablished_reportHandlerSelector (toMTRSubscribeParams params) subscriptionEstablished reportHandler

-- | @+ readAttributeClusterRevisionWithClusterStateCache:endpoint:queue:completion:@
readAttributeClusterRevisionWithClusterStateCache_endpoint_queue_completion :: (IsMTRClusterStateCacheContainer clusterStateCacheContainer, IsNSNumber endpoint, IsNSObject queue) => clusterStateCacheContainer -> endpoint -> queue -> Ptr () -> IO ()
readAttributeClusterRevisionWithClusterStateCache_endpoint_queue_completion clusterStateCacheContainer endpoint queue completion =
  do
    cls' <- getRequiredClass "MTRBaseClusterThreadBorderRouterManagement"
    sendClassMessage cls' readAttributeClusterRevisionWithClusterStateCache_endpoint_queue_completionSelector (toMTRClusterStateCacheContainer clusterStateCacheContainer) (toNSNumber endpoint) (toNSObject queue) completion

-- | @- init@
init_ :: IsMTRBaseClusterThreadBorderRouterManagement mtrBaseClusterThreadBorderRouterManagement => mtrBaseClusterThreadBorderRouterManagement -> IO (Id MTRBaseClusterThreadBorderRouterManagement)
init_ mtrBaseClusterThreadBorderRouterManagement =
  sendOwnedMessage mtrBaseClusterThreadBorderRouterManagement initSelector

-- | @+ new@
new :: IO (Id MTRBaseClusterThreadBorderRouterManagement)
new  =
  do
    cls' <- getRequiredClass "MTRBaseClusterThreadBorderRouterManagement"
    sendOwnedClassMessage cls' newSelector

-- | For all instance methods (reads, writes, commands) that take a completion, the completion will be called on the provided queue.
--
-- ObjC selector: @- initWithDevice:endpointID:queue:@
initWithDevice_endpointID_queue :: (IsMTRBaseClusterThreadBorderRouterManagement mtrBaseClusterThreadBorderRouterManagement, IsMTRBaseDevice device, IsNSNumber endpointID, IsNSObject queue) => mtrBaseClusterThreadBorderRouterManagement -> device -> endpointID -> queue -> IO (Id MTRBaseClusterThreadBorderRouterManagement)
initWithDevice_endpointID_queue mtrBaseClusterThreadBorderRouterManagement device endpointID queue =
  sendOwnedMessage mtrBaseClusterThreadBorderRouterManagement initWithDevice_endpointID_queueSelector (toMTRBaseDevice device) (toNSNumber endpointID) (toNSObject queue)

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @getActiveDatasetRequestWithParams:completion:@
getActiveDatasetRequestWithParams_completionSelector :: Selector '[Id MTRThreadBorderRouterManagementClusterGetActiveDatasetRequestParams, Ptr ()] ()
getActiveDatasetRequestWithParams_completionSelector = mkSelector "getActiveDatasetRequestWithParams:completion:"

-- | @Selector@ for @getActiveDatasetRequestWithCompletion:@
getActiveDatasetRequestWithCompletionSelector :: Selector '[Ptr ()] ()
getActiveDatasetRequestWithCompletionSelector = mkSelector "getActiveDatasetRequestWithCompletion:"

-- | @Selector@ for @getPendingDatasetRequestWithParams:completion:@
getPendingDatasetRequestWithParams_completionSelector :: Selector '[Id MTRThreadBorderRouterManagementClusterGetPendingDatasetRequestParams, Ptr ()] ()
getPendingDatasetRequestWithParams_completionSelector = mkSelector "getPendingDatasetRequestWithParams:completion:"

-- | @Selector@ for @getPendingDatasetRequestWithCompletion:@
getPendingDatasetRequestWithCompletionSelector :: Selector '[Ptr ()] ()
getPendingDatasetRequestWithCompletionSelector = mkSelector "getPendingDatasetRequestWithCompletion:"

-- | @Selector@ for @setActiveDatasetRequestWithParams:completion:@
setActiveDatasetRequestWithParams_completionSelector :: Selector '[Id MTRThreadBorderRouterManagementClusterSetActiveDatasetRequestParams, Ptr ()] ()
setActiveDatasetRequestWithParams_completionSelector = mkSelector "setActiveDatasetRequestWithParams:completion:"

-- | @Selector@ for @setPendingDatasetRequestWithParams:completion:@
setPendingDatasetRequestWithParams_completionSelector :: Selector '[Id MTRThreadBorderRouterManagementClusterSetPendingDatasetRequestParams, Ptr ()] ()
setPendingDatasetRequestWithParams_completionSelector = mkSelector "setPendingDatasetRequestWithParams:completion:"

-- | @Selector@ for @readAttributeBorderRouterNameWithCompletion:@
readAttributeBorderRouterNameWithCompletionSelector :: Selector '[Ptr ()] ()
readAttributeBorderRouterNameWithCompletionSelector = mkSelector "readAttributeBorderRouterNameWithCompletion:"

-- | @Selector@ for @subscribeAttributeBorderRouterNameWithParams:subscriptionEstablished:reportHandler:@
subscribeAttributeBorderRouterNameWithParams_subscriptionEstablished_reportHandlerSelector :: Selector '[Id MTRSubscribeParams, Ptr (), Ptr ()] ()
subscribeAttributeBorderRouterNameWithParams_subscriptionEstablished_reportHandlerSelector = mkSelector "subscribeAttributeBorderRouterNameWithParams:subscriptionEstablished:reportHandler:"

-- | @Selector@ for @readAttributeBorderRouterNameWithClusterStateCache:endpoint:queue:completion:@
readAttributeBorderRouterNameWithClusterStateCache_endpoint_queue_completionSelector :: Selector '[Id MTRClusterStateCacheContainer, Id NSNumber, Id NSObject, Ptr ()] ()
readAttributeBorderRouterNameWithClusterStateCache_endpoint_queue_completionSelector = mkSelector "readAttributeBorderRouterNameWithClusterStateCache:endpoint:queue:completion:"

-- | @Selector@ for @readAttributeBorderAgentIDWithCompletion:@
readAttributeBorderAgentIDWithCompletionSelector :: Selector '[Ptr ()] ()
readAttributeBorderAgentIDWithCompletionSelector = mkSelector "readAttributeBorderAgentIDWithCompletion:"

-- | @Selector@ for @subscribeAttributeBorderAgentIDWithParams:subscriptionEstablished:reportHandler:@
subscribeAttributeBorderAgentIDWithParams_subscriptionEstablished_reportHandlerSelector :: Selector '[Id MTRSubscribeParams, Ptr (), Ptr ()] ()
subscribeAttributeBorderAgentIDWithParams_subscriptionEstablished_reportHandlerSelector = mkSelector "subscribeAttributeBorderAgentIDWithParams:subscriptionEstablished:reportHandler:"

-- | @Selector@ for @readAttributeBorderAgentIDWithClusterStateCache:endpoint:queue:completion:@
readAttributeBorderAgentIDWithClusterStateCache_endpoint_queue_completionSelector :: Selector '[Id MTRClusterStateCacheContainer, Id NSNumber, Id NSObject, Ptr ()] ()
readAttributeBorderAgentIDWithClusterStateCache_endpoint_queue_completionSelector = mkSelector "readAttributeBorderAgentIDWithClusterStateCache:endpoint:queue:completion:"

-- | @Selector@ for @readAttributeThreadVersionWithCompletion:@
readAttributeThreadVersionWithCompletionSelector :: Selector '[Ptr ()] ()
readAttributeThreadVersionWithCompletionSelector = mkSelector "readAttributeThreadVersionWithCompletion:"

-- | @Selector@ for @subscribeAttributeThreadVersionWithParams:subscriptionEstablished:reportHandler:@
subscribeAttributeThreadVersionWithParams_subscriptionEstablished_reportHandlerSelector :: Selector '[Id MTRSubscribeParams, Ptr (), Ptr ()] ()
subscribeAttributeThreadVersionWithParams_subscriptionEstablished_reportHandlerSelector = mkSelector "subscribeAttributeThreadVersionWithParams:subscriptionEstablished:reportHandler:"

-- | @Selector@ for @readAttributeThreadVersionWithClusterStateCache:endpoint:queue:completion:@
readAttributeThreadVersionWithClusterStateCache_endpoint_queue_completionSelector :: Selector '[Id MTRClusterStateCacheContainer, Id NSNumber, Id NSObject, Ptr ()] ()
readAttributeThreadVersionWithClusterStateCache_endpoint_queue_completionSelector = mkSelector "readAttributeThreadVersionWithClusterStateCache:endpoint:queue:completion:"

-- | @Selector@ for @readAttributeInterfaceEnabledWithCompletion:@
readAttributeInterfaceEnabledWithCompletionSelector :: Selector '[Ptr ()] ()
readAttributeInterfaceEnabledWithCompletionSelector = mkSelector "readAttributeInterfaceEnabledWithCompletion:"

-- | @Selector@ for @subscribeAttributeInterfaceEnabledWithParams:subscriptionEstablished:reportHandler:@
subscribeAttributeInterfaceEnabledWithParams_subscriptionEstablished_reportHandlerSelector :: Selector '[Id MTRSubscribeParams, Ptr (), Ptr ()] ()
subscribeAttributeInterfaceEnabledWithParams_subscriptionEstablished_reportHandlerSelector = mkSelector "subscribeAttributeInterfaceEnabledWithParams:subscriptionEstablished:reportHandler:"

-- | @Selector@ for @readAttributeInterfaceEnabledWithClusterStateCache:endpoint:queue:completion:@
readAttributeInterfaceEnabledWithClusterStateCache_endpoint_queue_completionSelector :: Selector '[Id MTRClusterStateCacheContainer, Id NSNumber, Id NSObject, Ptr ()] ()
readAttributeInterfaceEnabledWithClusterStateCache_endpoint_queue_completionSelector = mkSelector "readAttributeInterfaceEnabledWithClusterStateCache:endpoint:queue:completion:"

-- | @Selector@ for @readAttributeActiveDatasetTimestampWithCompletion:@
readAttributeActiveDatasetTimestampWithCompletionSelector :: Selector '[Ptr ()] ()
readAttributeActiveDatasetTimestampWithCompletionSelector = mkSelector "readAttributeActiveDatasetTimestampWithCompletion:"

-- | @Selector@ for @subscribeAttributeActiveDatasetTimestampWithParams:subscriptionEstablished:reportHandler:@
subscribeAttributeActiveDatasetTimestampWithParams_subscriptionEstablished_reportHandlerSelector :: Selector '[Id MTRSubscribeParams, Ptr (), Ptr ()] ()
subscribeAttributeActiveDatasetTimestampWithParams_subscriptionEstablished_reportHandlerSelector = mkSelector "subscribeAttributeActiveDatasetTimestampWithParams:subscriptionEstablished:reportHandler:"

-- | @Selector@ for @readAttributeActiveDatasetTimestampWithClusterStateCache:endpoint:queue:completion:@
readAttributeActiveDatasetTimestampWithClusterStateCache_endpoint_queue_completionSelector :: Selector '[Id MTRClusterStateCacheContainer, Id NSNumber, Id NSObject, Ptr ()] ()
readAttributeActiveDatasetTimestampWithClusterStateCache_endpoint_queue_completionSelector = mkSelector "readAttributeActiveDatasetTimestampWithClusterStateCache:endpoint:queue:completion:"

-- | @Selector@ for @readAttributePendingDatasetTimestampWithCompletion:@
readAttributePendingDatasetTimestampWithCompletionSelector :: Selector '[Ptr ()] ()
readAttributePendingDatasetTimestampWithCompletionSelector = mkSelector "readAttributePendingDatasetTimestampWithCompletion:"

-- | @Selector@ for @subscribeAttributePendingDatasetTimestampWithParams:subscriptionEstablished:reportHandler:@
subscribeAttributePendingDatasetTimestampWithParams_subscriptionEstablished_reportHandlerSelector :: Selector '[Id MTRSubscribeParams, Ptr (), Ptr ()] ()
subscribeAttributePendingDatasetTimestampWithParams_subscriptionEstablished_reportHandlerSelector = mkSelector "subscribeAttributePendingDatasetTimestampWithParams:subscriptionEstablished:reportHandler:"

-- | @Selector@ for @readAttributePendingDatasetTimestampWithClusterStateCache:endpoint:queue:completion:@
readAttributePendingDatasetTimestampWithClusterStateCache_endpoint_queue_completionSelector :: Selector '[Id MTRClusterStateCacheContainer, Id NSNumber, Id NSObject, Ptr ()] ()
readAttributePendingDatasetTimestampWithClusterStateCache_endpoint_queue_completionSelector = mkSelector "readAttributePendingDatasetTimestampWithClusterStateCache:endpoint:queue:completion:"

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
initSelector :: Selector '[] (Id MTRBaseClusterThreadBorderRouterManagement)
initSelector = mkSelector "init"

-- | @Selector@ for @new@
newSelector :: Selector '[] (Id MTRBaseClusterThreadBorderRouterManagement)
newSelector = mkSelector "new"

-- | @Selector@ for @initWithDevice:endpointID:queue:@
initWithDevice_endpointID_queueSelector :: Selector '[Id MTRBaseDevice, Id NSNumber, Id NSObject] (Id MTRBaseClusterThreadBorderRouterManagement)
initWithDevice_endpointID_queueSelector = mkSelector "initWithDevice:endpointID:queue:"

