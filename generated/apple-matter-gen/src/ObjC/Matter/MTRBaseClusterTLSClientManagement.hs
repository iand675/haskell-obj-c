{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Cluster TLS Client Management
--
-- This Cluster is used to provision TLS Endpoints with enough information to facilitate subsequent connection.
--
-- Generated bindings for @MTRBaseClusterTLSClientManagement@.
module ObjC.Matter.MTRBaseClusterTLSClientManagement
  ( MTRBaseClusterTLSClientManagement
  , IsMTRBaseClusterTLSClientManagement(..)
  , provisionEndpointWithParams_completion
  , findEndpointWithParams_completion
  , removeEndpointWithParams_completion
  , readAttributeMaxProvisionedWithCompletion
  , subscribeAttributeMaxProvisionedWithParams_subscriptionEstablished_reportHandler
  , readAttributeMaxProvisionedWithClusterStateCache_endpoint_queue_completion
  , readAttributeProvisionedEndpointsWithParams_completion
  , subscribeAttributeProvisionedEndpointsWithParams_subscriptionEstablished_reportHandler
  , readAttributeProvisionedEndpointsWithClusterStateCache_endpoint_queue_completion
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
  , findEndpointWithParams_completionSelector
  , initSelector
  , initWithDevice_endpointID_queueSelector
  , newSelector
  , provisionEndpointWithParams_completionSelector
  , readAttributeAcceptedCommandListWithClusterStateCache_endpoint_queue_completionSelector
  , readAttributeAcceptedCommandListWithCompletionSelector
  , readAttributeAttributeListWithClusterStateCache_endpoint_queue_completionSelector
  , readAttributeAttributeListWithCompletionSelector
  , readAttributeClusterRevisionWithClusterStateCache_endpoint_queue_completionSelector
  , readAttributeClusterRevisionWithCompletionSelector
  , readAttributeFeatureMapWithClusterStateCache_endpoint_queue_completionSelector
  , readAttributeFeatureMapWithCompletionSelector
  , readAttributeGeneratedCommandListWithClusterStateCache_endpoint_queue_completionSelector
  , readAttributeGeneratedCommandListWithCompletionSelector
  , readAttributeMaxProvisionedWithClusterStateCache_endpoint_queue_completionSelector
  , readAttributeMaxProvisionedWithCompletionSelector
  , readAttributeProvisionedEndpointsWithClusterStateCache_endpoint_queue_completionSelector
  , readAttributeProvisionedEndpointsWithParams_completionSelector
  , removeEndpointWithParams_completionSelector
  , subscribeAttributeAcceptedCommandListWithParams_subscriptionEstablished_reportHandlerSelector
  , subscribeAttributeAttributeListWithParams_subscriptionEstablished_reportHandlerSelector
  , subscribeAttributeClusterRevisionWithParams_subscriptionEstablished_reportHandlerSelector
  , subscribeAttributeFeatureMapWithParams_subscriptionEstablished_reportHandlerSelector
  , subscribeAttributeGeneratedCommandListWithParams_subscriptionEstablished_reportHandlerSelector
  , subscribeAttributeMaxProvisionedWithParams_subscriptionEstablished_reportHandlerSelector
  , subscribeAttributeProvisionedEndpointsWithParams_subscriptionEstablished_reportHandlerSelector


  ) where

import Foreign.Ptr (Ptr, FunPtr)
import Foreign.C.Types

import ObjC.Runtime.Types
import ObjC.Runtime.Message (sendMessage, sendOwnedMessage, sendClassMessage, sendOwnedClassMessage)
import ObjC.Runtime.Selector (mkSelector)
import ObjC.Runtime.Class (getRequiredClass)

import ObjC.Matter.Internal.Classes
import ObjC.Foundation.Internal.Classes

-- | Command ProvisionEndpoint
--
-- This command is used to provision a TLS Endpoint for the provided Hostname / Port combination.
--
-- ObjC selector: @- provisionEndpointWithParams:completion:@
provisionEndpointWithParams_completion :: (IsMTRBaseClusterTLSClientManagement mtrBaseClusterTLSClientManagement, IsMTRTLSClientManagementClusterProvisionEndpointParams params) => mtrBaseClusterTLSClientManagement -> params -> Ptr () -> IO ()
provisionEndpointWithParams_completion mtrBaseClusterTLSClientManagement params completion =
  sendMessage mtrBaseClusterTLSClientManagement provisionEndpointWithParams_completionSelector (toMTRTLSClientManagementClusterProvisionEndpointParams params) completion

-- | Command FindEndpoint
--
-- This command is used to find a TLS Endpoint by its ID.
--
-- ObjC selector: @- findEndpointWithParams:completion:@
findEndpointWithParams_completion :: (IsMTRBaseClusterTLSClientManagement mtrBaseClusterTLSClientManagement, IsMTRTLSClientManagementClusterFindEndpointParams params) => mtrBaseClusterTLSClientManagement -> params -> Ptr () -> IO ()
findEndpointWithParams_completion mtrBaseClusterTLSClientManagement params completion =
  sendMessage mtrBaseClusterTLSClientManagement findEndpointWithParams_completionSelector (toMTRTLSClientManagementClusterFindEndpointParams params) completion

-- | Command RemoveEndpoint
--
-- This command is used to remove a TLS Endpoint by its ID.
--
-- ObjC selector: @- removeEndpointWithParams:completion:@
removeEndpointWithParams_completion :: (IsMTRBaseClusterTLSClientManagement mtrBaseClusterTLSClientManagement, IsMTRTLSClientManagementClusterRemoveEndpointParams params) => mtrBaseClusterTLSClientManagement -> params -> Ptr () -> IO ()
removeEndpointWithParams_completion mtrBaseClusterTLSClientManagement params completion =
  sendMessage mtrBaseClusterTLSClientManagement removeEndpointWithParams_completionSelector (toMTRTLSClientManagementClusterRemoveEndpointParams params) completion

-- | @- readAttributeMaxProvisionedWithCompletion:@
readAttributeMaxProvisionedWithCompletion :: IsMTRBaseClusterTLSClientManagement mtrBaseClusterTLSClientManagement => mtrBaseClusterTLSClientManagement -> Ptr () -> IO ()
readAttributeMaxProvisionedWithCompletion mtrBaseClusterTLSClientManagement completion =
  sendMessage mtrBaseClusterTLSClientManagement readAttributeMaxProvisionedWithCompletionSelector completion

-- | @- subscribeAttributeMaxProvisionedWithParams:subscriptionEstablished:reportHandler:@
subscribeAttributeMaxProvisionedWithParams_subscriptionEstablished_reportHandler :: (IsMTRBaseClusterTLSClientManagement mtrBaseClusterTLSClientManagement, IsMTRSubscribeParams params) => mtrBaseClusterTLSClientManagement -> params -> Ptr () -> Ptr () -> IO ()
subscribeAttributeMaxProvisionedWithParams_subscriptionEstablished_reportHandler mtrBaseClusterTLSClientManagement params subscriptionEstablished reportHandler =
  sendMessage mtrBaseClusterTLSClientManagement subscribeAttributeMaxProvisionedWithParams_subscriptionEstablished_reportHandlerSelector (toMTRSubscribeParams params) subscriptionEstablished reportHandler

-- | @+ readAttributeMaxProvisionedWithClusterStateCache:endpoint:queue:completion:@
readAttributeMaxProvisionedWithClusterStateCache_endpoint_queue_completion :: (IsMTRClusterStateCacheContainer clusterStateCacheContainer, IsNSNumber endpoint, IsNSObject queue) => clusterStateCacheContainer -> endpoint -> queue -> Ptr () -> IO ()
readAttributeMaxProvisionedWithClusterStateCache_endpoint_queue_completion clusterStateCacheContainer endpoint queue completion =
  do
    cls' <- getRequiredClass "MTRBaseClusterTLSClientManagement"
    sendClassMessage cls' readAttributeMaxProvisionedWithClusterStateCache_endpoint_queue_completionSelector (toMTRClusterStateCacheContainer clusterStateCacheContainer) (toNSNumber endpoint) (toNSObject queue) completion

-- | @- readAttributeProvisionedEndpointsWithParams:completion:@
readAttributeProvisionedEndpointsWithParams_completion :: (IsMTRBaseClusterTLSClientManagement mtrBaseClusterTLSClientManagement, IsMTRReadParams params) => mtrBaseClusterTLSClientManagement -> params -> Ptr () -> IO ()
readAttributeProvisionedEndpointsWithParams_completion mtrBaseClusterTLSClientManagement params completion =
  sendMessage mtrBaseClusterTLSClientManagement readAttributeProvisionedEndpointsWithParams_completionSelector (toMTRReadParams params) completion

-- | @- subscribeAttributeProvisionedEndpointsWithParams:subscriptionEstablished:reportHandler:@
subscribeAttributeProvisionedEndpointsWithParams_subscriptionEstablished_reportHandler :: (IsMTRBaseClusterTLSClientManagement mtrBaseClusterTLSClientManagement, IsMTRSubscribeParams params) => mtrBaseClusterTLSClientManagement -> params -> Ptr () -> Ptr () -> IO ()
subscribeAttributeProvisionedEndpointsWithParams_subscriptionEstablished_reportHandler mtrBaseClusterTLSClientManagement params subscriptionEstablished reportHandler =
  sendMessage mtrBaseClusterTLSClientManagement subscribeAttributeProvisionedEndpointsWithParams_subscriptionEstablished_reportHandlerSelector (toMTRSubscribeParams params) subscriptionEstablished reportHandler

-- | @+ readAttributeProvisionedEndpointsWithClusterStateCache:endpoint:queue:completion:@
readAttributeProvisionedEndpointsWithClusterStateCache_endpoint_queue_completion :: (IsMTRClusterStateCacheContainer clusterStateCacheContainer, IsNSNumber endpoint, IsNSObject queue) => clusterStateCacheContainer -> endpoint -> queue -> Ptr () -> IO ()
readAttributeProvisionedEndpointsWithClusterStateCache_endpoint_queue_completion clusterStateCacheContainer endpoint queue completion =
  do
    cls' <- getRequiredClass "MTRBaseClusterTLSClientManagement"
    sendClassMessage cls' readAttributeProvisionedEndpointsWithClusterStateCache_endpoint_queue_completionSelector (toMTRClusterStateCacheContainer clusterStateCacheContainer) (toNSNumber endpoint) (toNSObject queue) completion

-- | @- readAttributeGeneratedCommandListWithCompletion:@
readAttributeGeneratedCommandListWithCompletion :: IsMTRBaseClusterTLSClientManagement mtrBaseClusterTLSClientManagement => mtrBaseClusterTLSClientManagement -> Ptr () -> IO ()
readAttributeGeneratedCommandListWithCompletion mtrBaseClusterTLSClientManagement completion =
  sendMessage mtrBaseClusterTLSClientManagement readAttributeGeneratedCommandListWithCompletionSelector completion

-- | @- subscribeAttributeGeneratedCommandListWithParams:subscriptionEstablished:reportHandler:@
subscribeAttributeGeneratedCommandListWithParams_subscriptionEstablished_reportHandler :: (IsMTRBaseClusterTLSClientManagement mtrBaseClusterTLSClientManagement, IsMTRSubscribeParams params) => mtrBaseClusterTLSClientManagement -> params -> Ptr () -> Ptr () -> IO ()
subscribeAttributeGeneratedCommandListWithParams_subscriptionEstablished_reportHandler mtrBaseClusterTLSClientManagement params subscriptionEstablished reportHandler =
  sendMessage mtrBaseClusterTLSClientManagement subscribeAttributeGeneratedCommandListWithParams_subscriptionEstablished_reportHandlerSelector (toMTRSubscribeParams params) subscriptionEstablished reportHandler

-- | @+ readAttributeGeneratedCommandListWithClusterStateCache:endpoint:queue:completion:@
readAttributeGeneratedCommandListWithClusterStateCache_endpoint_queue_completion :: (IsMTRClusterStateCacheContainer clusterStateCacheContainer, IsNSNumber endpoint, IsNSObject queue) => clusterStateCacheContainer -> endpoint -> queue -> Ptr () -> IO ()
readAttributeGeneratedCommandListWithClusterStateCache_endpoint_queue_completion clusterStateCacheContainer endpoint queue completion =
  do
    cls' <- getRequiredClass "MTRBaseClusterTLSClientManagement"
    sendClassMessage cls' readAttributeGeneratedCommandListWithClusterStateCache_endpoint_queue_completionSelector (toMTRClusterStateCacheContainer clusterStateCacheContainer) (toNSNumber endpoint) (toNSObject queue) completion

-- | @- readAttributeAcceptedCommandListWithCompletion:@
readAttributeAcceptedCommandListWithCompletion :: IsMTRBaseClusterTLSClientManagement mtrBaseClusterTLSClientManagement => mtrBaseClusterTLSClientManagement -> Ptr () -> IO ()
readAttributeAcceptedCommandListWithCompletion mtrBaseClusterTLSClientManagement completion =
  sendMessage mtrBaseClusterTLSClientManagement readAttributeAcceptedCommandListWithCompletionSelector completion

-- | @- subscribeAttributeAcceptedCommandListWithParams:subscriptionEstablished:reportHandler:@
subscribeAttributeAcceptedCommandListWithParams_subscriptionEstablished_reportHandler :: (IsMTRBaseClusterTLSClientManagement mtrBaseClusterTLSClientManagement, IsMTRSubscribeParams params) => mtrBaseClusterTLSClientManagement -> params -> Ptr () -> Ptr () -> IO ()
subscribeAttributeAcceptedCommandListWithParams_subscriptionEstablished_reportHandler mtrBaseClusterTLSClientManagement params subscriptionEstablished reportHandler =
  sendMessage mtrBaseClusterTLSClientManagement subscribeAttributeAcceptedCommandListWithParams_subscriptionEstablished_reportHandlerSelector (toMTRSubscribeParams params) subscriptionEstablished reportHandler

-- | @+ readAttributeAcceptedCommandListWithClusterStateCache:endpoint:queue:completion:@
readAttributeAcceptedCommandListWithClusterStateCache_endpoint_queue_completion :: (IsMTRClusterStateCacheContainer clusterStateCacheContainer, IsNSNumber endpoint, IsNSObject queue) => clusterStateCacheContainer -> endpoint -> queue -> Ptr () -> IO ()
readAttributeAcceptedCommandListWithClusterStateCache_endpoint_queue_completion clusterStateCacheContainer endpoint queue completion =
  do
    cls' <- getRequiredClass "MTRBaseClusterTLSClientManagement"
    sendClassMessage cls' readAttributeAcceptedCommandListWithClusterStateCache_endpoint_queue_completionSelector (toMTRClusterStateCacheContainer clusterStateCacheContainer) (toNSNumber endpoint) (toNSObject queue) completion

-- | @- readAttributeAttributeListWithCompletion:@
readAttributeAttributeListWithCompletion :: IsMTRBaseClusterTLSClientManagement mtrBaseClusterTLSClientManagement => mtrBaseClusterTLSClientManagement -> Ptr () -> IO ()
readAttributeAttributeListWithCompletion mtrBaseClusterTLSClientManagement completion =
  sendMessage mtrBaseClusterTLSClientManagement readAttributeAttributeListWithCompletionSelector completion

-- | @- subscribeAttributeAttributeListWithParams:subscriptionEstablished:reportHandler:@
subscribeAttributeAttributeListWithParams_subscriptionEstablished_reportHandler :: (IsMTRBaseClusterTLSClientManagement mtrBaseClusterTLSClientManagement, IsMTRSubscribeParams params) => mtrBaseClusterTLSClientManagement -> params -> Ptr () -> Ptr () -> IO ()
subscribeAttributeAttributeListWithParams_subscriptionEstablished_reportHandler mtrBaseClusterTLSClientManagement params subscriptionEstablished reportHandler =
  sendMessage mtrBaseClusterTLSClientManagement subscribeAttributeAttributeListWithParams_subscriptionEstablished_reportHandlerSelector (toMTRSubscribeParams params) subscriptionEstablished reportHandler

-- | @+ readAttributeAttributeListWithClusterStateCache:endpoint:queue:completion:@
readAttributeAttributeListWithClusterStateCache_endpoint_queue_completion :: (IsMTRClusterStateCacheContainer clusterStateCacheContainer, IsNSNumber endpoint, IsNSObject queue) => clusterStateCacheContainer -> endpoint -> queue -> Ptr () -> IO ()
readAttributeAttributeListWithClusterStateCache_endpoint_queue_completion clusterStateCacheContainer endpoint queue completion =
  do
    cls' <- getRequiredClass "MTRBaseClusterTLSClientManagement"
    sendClassMessage cls' readAttributeAttributeListWithClusterStateCache_endpoint_queue_completionSelector (toMTRClusterStateCacheContainer clusterStateCacheContainer) (toNSNumber endpoint) (toNSObject queue) completion

-- | @- readAttributeFeatureMapWithCompletion:@
readAttributeFeatureMapWithCompletion :: IsMTRBaseClusterTLSClientManagement mtrBaseClusterTLSClientManagement => mtrBaseClusterTLSClientManagement -> Ptr () -> IO ()
readAttributeFeatureMapWithCompletion mtrBaseClusterTLSClientManagement completion =
  sendMessage mtrBaseClusterTLSClientManagement readAttributeFeatureMapWithCompletionSelector completion

-- | @- subscribeAttributeFeatureMapWithParams:subscriptionEstablished:reportHandler:@
subscribeAttributeFeatureMapWithParams_subscriptionEstablished_reportHandler :: (IsMTRBaseClusterTLSClientManagement mtrBaseClusterTLSClientManagement, IsMTRSubscribeParams params) => mtrBaseClusterTLSClientManagement -> params -> Ptr () -> Ptr () -> IO ()
subscribeAttributeFeatureMapWithParams_subscriptionEstablished_reportHandler mtrBaseClusterTLSClientManagement params subscriptionEstablished reportHandler =
  sendMessage mtrBaseClusterTLSClientManagement subscribeAttributeFeatureMapWithParams_subscriptionEstablished_reportHandlerSelector (toMTRSubscribeParams params) subscriptionEstablished reportHandler

-- | @+ readAttributeFeatureMapWithClusterStateCache:endpoint:queue:completion:@
readAttributeFeatureMapWithClusterStateCache_endpoint_queue_completion :: (IsMTRClusterStateCacheContainer clusterStateCacheContainer, IsNSNumber endpoint, IsNSObject queue) => clusterStateCacheContainer -> endpoint -> queue -> Ptr () -> IO ()
readAttributeFeatureMapWithClusterStateCache_endpoint_queue_completion clusterStateCacheContainer endpoint queue completion =
  do
    cls' <- getRequiredClass "MTRBaseClusterTLSClientManagement"
    sendClassMessage cls' readAttributeFeatureMapWithClusterStateCache_endpoint_queue_completionSelector (toMTRClusterStateCacheContainer clusterStateCacheContainer) (toNSNumber endpoint) (toNSObject queue) completion

-- | @- readAttributeClusterRevisionWithCompletion:@
readAttributeClusterRevisionWithCompletion :: IsMTRBaseClusterTLSClientManagement mtrBaseClusterTLSClientManagement => mtrBaseClusterTLSClientManagement -> Ptr () -> IO ()
readAttributeClusterRevisionWithCompletion mtrBaseClusterTLSClientManagement completion =
  sendMessage mtrBaseClusterTLSClientManagement readAttributeClusterRevisionWithCompletionSelector completion

-- | @- subscribeAttributeClusterRevisionWithParams:subscriptionEstablished:reportHandler:@
subscribeAttributeClusterRevisionWithParams_subscriptionEstablished_reportHandler :: (IsMTRBaseClusterTLSClientManagement mtrBaseClusterTLSClientManagement, IsMTRSubscribeParams params) => mtrBaseClusterTLSClientManagement -> params -> Ptr () -> Ptr () -> IO ()
subscribeAttributeClusterRevisionWithParams_subscriptionEstablished_reportHandler mtrBaseClusterTLSClientManagement params subscriptionEstablished reportHandler =
  sendMessage mtrBaseClusterTLSClientManagement subscribeAttributeClusterRevisionWithParams_subscriptionEstablished_reportHandlerSelector (toMTRSubscribeParams params) subscriptionEstablished reportHandler

-- | @+ readAttributeClusterRevisionWithClusterStateCache:endpoint:queue:completion:@
readAttributeClusterRevisionWithClusterStateCache_endpoint_queue_completion :: (IsMTRClusterStateCacheContainer clusterStateCacheContainer, IsNSNumber endpoint, IsNSObject queue) => clusterStateCacheContainer -> endpoint -> queue -> Ptr () -> IO ()
readAttributeClusterRevisionWithClusterStateCache_endpoint_queue_completion clusterStateCacheContainer endpoint queue completion =
  do
    cls' <- getRequiredClass "MTRBaseClusterTLSClientManagement"
    sendClassMessage cls' readAttributeClusterRevisionWithClusterStateCache_endpoint_queue_completionSelector (toMTRClusterStateCacheContainer clusterStateCacheContainer) (toNSNumber endpoint) (toNSObject queue) completion

-- | @- init@
init_ :: IsMTRBaseClusterTLSClientManagement mtrBaseClusterTLSClientManagement => mtrBaseClusterTLSClientManagement -> IO (Id MTRBaseClusterTLSClientManagement)
init_ mtrBaseClusterTLSClientManagement =
  sendOwnedMessage mtrBaseClusterTLSClientManagement initSelector

-- | @+ new@
new :: IO (Id MTRBaseClusterTLSClientManagement)
new  =
  do
    cls' <- getRequiredClass "MTRBaseClusterTLSClientManagement"
    sendOwnedClassMessage cls' newSelector

-- | For all instance methods (reads, writes, commands) that take a completion, the completion will be called on the provided queue.
--
-- ObjC selector: @- initWithDevice:endpointID:queue:@
initWithDevice_endpointID_queue :: (IsMTRBaseClusterTLSClientManagement mtrBaseClusterTLSClientManagement, IsMTRBaseDevice device, IsNSNumber endpointID, IsNSObject queue) => mtrBaseClusterTLSClientManagement -> device -> endpointID -> queue -> IO (Id MTRBaseClusterTLSClientManagement)
initWithDevice_endpointID_queue mtrBaseClusterTLSClientManagement device endpointID queue =
  sendOwnedMessage mtrBaseClusterTLSClientManagement initWithDevice_endpointID_queueSelector (toMTRBaseDevice device) (toNSNumber endpointID) (toNSObject queue)

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @provisionEndpointWithParams:completion:@
provisionEndpointWithParams_completionSelector :: Selector '[Id MTRTLSClientManagementClusterProvisionEndpointParams, Ptr ()] ()
provisionEndpointWithParams_completionSelector = mkSelector "provisionEndpointWithParams:completion:"

-- | @Selector@ for @findEndpointWithParams:completion:@
findEndpointWithParams_completionSelector :: Selector '[Id MTRTLSClientManagementClusterFindEndpointParams, Ptr ()] ()
findEndpointWithParams_completionSelector = mkSelector "findEndpointWithParams:completion:"

-- | @Selector@ for @removeEndpointWithParams:completion:@
removeEndpointWithParams_completionSelector :: Selector '[Id MTRTLSClientManagementClusterRemoveEndpointParams, Ptr ()] ()
removeEndpointWithParams_completionSelector = mkSelector "removeEndpointWithParams:completion:"

-- | @Selector@ for @readAttributeMaxProvisionedWithCompletion:@
readAttributeMaxProvisionedWithCompletionSelector :: Selector '[Ptr ()] ()
readAttributeMaxProvisionedWithCompletionSelector = mkSelector "readAttributeMaxProvisionedWithCompletion:"

-- | @Selector@ for @subscribeAttributeMaxProvisionedWithParams:subscriptionEstablished:reportHandler:@
subscribeAttributeMaxProvisionedWithParams_subscriptionEstablished_reportHandlerSelector :: Selector '[Id MTRSubscribeParams, Ptr (), Ptr ()] ()
subscribeAttributeMaxProvisionedWithParams_subscriptionEstablished_reportHandlerSelector = mkSelector "subscribeAttributeMaxProvisionedWithParams:subscriptionEstablished:reportHandler:"

-- | @Selector@ for @readAttributeMaxProvisionedWithClusterStateCache:endpoint:queue:completion:@
readAttributeMaxProvisionedWithClusterStateCache_endpoint_queue_completionSelector :: Selector '[Id MTRClusterStateCacheContainer, Id NSNumber, Id NSObject, Ptr ()] ()
readAttributeMaxProvisionedWithClusterStateCache_endpoint_queue_completionSelector = mkSelector "readAttributeMaxProvisionedWithClusterStateCache:endpoint:queue:completion:"

-- | @Selector@ for @readAttributeProvisionedEndpointsWithParams:completion:@
readAttributeProvisionedEndpointsWithParams_completionSelector :: Selector '[Id MTRReadParams, Ptr ()] ()
readAttributeProvisionedEndpointsWithParams_completionSelector = mkSelector "readAttributeProvisionedEndpointsWithParams:completion:"

-- | @Selector@ for @subscribeAttributeProvisionedEndpointsWithParams:subscriptionEstablished:reportHandler:@
subscribeAttributeProvisionedEndpointsWithParams_subscriptionEstablished_reportHandlerSelector :: Selector '[Id MTRSubscribeParams, Ptr (), Ptr ()] ()
subscribeAttributeProvisionedEndpointsWithParams_subscriptionEstablished_reportHandlerSelector = mkSelector "subscribeAttributeProvisionedEndpointsWithParams:subscriptionEstablished:reportHandler:"

-- | @Selector@ for @readAttributeProvisionedEndpointsWithClusterStateCache:endpoint:queue:completion:@
readAttributeProvisionedEndpointsWithClusterStateCache_endpoint_queue_completionSelector :: Selector '[Id MTRClusterStateCacheContainer, Id NSNumber, Id NSObject, Ptr ()] ()
readAttributeProvisionedEndpointsWithClusterStateCache_endpoint_queue_completionSelector = mkSelector "readAttributeProvisionedEndpointsWithClusterStateCache:endpoint:queue:completion:"

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
initSelector :: Selector '[] (Id MTRBaseClusterTLSClientManagement)
initSelector = mkSelector "init"

-- | @Selector@ for @new@
newSelector :: Selector '[] (Id MTRBaseClusterTLSClientManagement)
newSelector = mkSelector "new"

-- | @Selector@ for @initWithDevice:endpointID:queue:@
initWithDevice_endpointID_queueSelector :: Selector '[Id MTRBaseDevice, Id NSNumber, Id NSObject] (Id MTRBaseClusterTLSClientManagement)
initWithDevice_endpointID_queueSelector = mkSelector "initWithDevice:endpointID:queue:"

