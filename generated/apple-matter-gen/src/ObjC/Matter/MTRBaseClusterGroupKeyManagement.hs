{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Cluster Group Key Management
--
-- The Group Key Management Cluster is the mechanism by which group keys are managed.
--
-- Generated bindings for @MTRBaseClusterGroupKeyManagement@.
module ObjC.Matter.MTRBaseClusterGroupKeyManagement
  ( MTRBaseClusterGroupKeyManagement
  , IsMTRBaseClusterGroupKeyManagement(..)
  , keySetWriteWithParams_completion
  , keySetReadWithParams_completion
  , keySetRemoveWithParams_completion
  , keySetReadAllIndicesWithParams_completion
  , keySetReadAllIndicesWithCompletion
  , readAttributeGroupKeyMapWithParams_completion
  , writeAttributeGroupKeyMapWithValue_completion
  , writeAttributeGroupKeyMapWithValue_params_completion
  , subscribeAttributeGroupKeyMapWithParams_subscriptionEstablished_reportHandler
  , readAttributeGroupKeyMapWithClusterStateCache_endpoint_queue_completion
  , readAttributeGroupTableWithParams_completion
  , subscribeAttributeGroupTableWithParams_subscriptionEstablished_reportHandler
  , readAttributeGroupTableWithClusterStateCache_endpoint_queue_completion
  , readAttributeMaxGroupsPerFabricWithCompletion
  , subscribeAttributeMaxGroupsPerFabricWithParams_subscriptionEstablished_reportHandler
  , readAttributeMaxGroupsPerFabricWithClusterStateCache_endpoint_queue_completion
  , readAttributeMaxGroupKeysPerFabricWithCompletion
  , subscribeAttributeMaxGroupKeysPerFabricWithParams_subscriptionEstablished_reportHandler
  , readAttributeMaxGroupKeysPerFabricWithClusterStateCache_endpoint_queue_completion
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
  , initWithDevice_endpoint_queue
  , keySetWriteWithParams_completionHandler
  , keySetReadWithParams_completionHandler
  , keySetRemoveWithParams_completionHandler
  , keySetReadAllIndicesWithParams_completionHandler
  , readAttributeGroupKeyMapWithParams_completionHandler
  , writeAttributeGroupKeyMapWithValue_completionHandler
  , writeAttributeGroupKeyMapWithValue_params_completionHandler
  , subscribeAttributeGroupKeyMapWithMinInterval_maxInterval_params_subscriptionEstablished_reportHandler
  , readAttributeGroupKeyMapWithAttributeCache_endpoint_queue_completionHandler
  , readAttributeGroupTableWithParams_completionHandler
  , subscribeAttributeGroupTableWithMinInterval_maxInterval_params_subscriptionEstablished_reportHandler
  , readAttributeGroupTableWithAttributeCache_endpoint_queue_completionHandler
  , readAttributeMaxGroupsPerFabricWithCompletionHandler
  , subscribeAttributeMaxGroupsPerFabricWithMinInterval_maxInterval_params_subscriptionEstablished_reportHandler
  , readAttributeMaxGroupsPerFabricWithAttributeCache_endpoint_queue_completionHandler
  , readAttributeMaxGroupKeysPerFabricWithCompletionHandler
  , subscribeAttributeMaxGroupKeysPerFabricWithMinInterval_maxInterval_params_subscriptionEstablished_reportHandler
  , readAttributeMaxGroupKeysPerFabricWithAttributeCache_endpoint_queue_completionHandler
  , readAttributeGeneratedCommandListWithCompletionHandler
  , subscribeAttributeGeneratedCommandListWithMinInterval_maxInterval_params_subscriptionEstablished_reportHandler
  , readAttributeGeneratedCommandListWithAttributeCache_endpoint_queue_completionHandler
  , readAttributeAcceptedCommandListWithCompletionHandler
  , subscribeAttributeAcceptedCommandListWithMinInterval_maxInterval_params_subscriptionEstablished_reportHandler
  , readAttributeAcceptedCommandListWithAttributeCache_endpoint_queue_completionHandler
  , readAttributeAttributeListWithCompletionHandler
  , subscribeAttributeAttributeListWithMinInterval_maxInterval_params_subscriptionEstablished_reportHandler
  , readAttributeAttributeListWithAttributeCache_endpoint_queue_completionHandler
  , readAttributeFeatureMapWithCompletionHandler
  , subscribeAttributeFeatureMapWithMinInterval_maxInterval_params_subscriptionEstablished_reportHandler
  , readAttributeFeatureMapWithAttributeCache_endpoint_queue_completionHandler
  , readAttributeClusterRevisionWithCompletionHandler
  , subscribeAttributeClusterRevisionWithMinInterval_maxInterval_params_subscriptionEstablished_reportHandler
  , readAttributeClusterRevisionWithAttributeCache_endpoint_queue_completionHandler
  , initWithDevice_endpointID_queue
  , initSelector
  , initWithDevice_endpointID_queueSelector
  , initWithDevice_endpoint_queueSelector
  , keySetReadAllIndicesWithCompletionSelector
  , keySetReadAllIndicesWithParams_completionHandlerSelector
  , keySetReadAllIndicesWithParams_completionSelector
  , keySetReadWithParams_completionHandlerSelector
  , keySetReadWithParams_completionSelector
  , keySetRemoveWithParams_completionHandlerSelector
  , keySetRemoveWithParams_completionSelector
  , keySetWriteWithParams_completionHandlerSelector
  , keySetWriteWithParams_completionSelector
  , newSelector
  , readAttributeAcceptedCommandListWithAttributeCache_endpoint_queue_completionHandlerSelector
  , readAttributeAcceptedCommandListWithClusterStateCache_endpoint_queue_completionSelector
  , readAttributeAcceptedCommandListWithCompletionHandlerSelector
  , readAttributeAcceptedCommandListWithCompletionSelector
  , readAttributeAttributeListWithAttributeCache_endpoint_queue_completionHandlerSelector
  , readAttributeAttributeListWithClusterStateCache_endpoint_queue_completionSelector
  , readAttributeAttributeListWithCompletionHandlerSelector
  , readAttributeAttributeListWithCompletionSelector
  , readAttributeClusterRevisionWithAttributeCache_endpoint_queue_completionHandlerSelector
  , readAttributeClusterRevisionWithClusterStateCache_endpoint_queue_completionSelector
  , readAttributeClusterRevisionWithCompletionHandlerSelector
  , readAttributeClusterRevisionWithCompletionSelector
  , readAttributeFeatureMapWithAttributeCache_endpoint_queue_completionHandlerSelector
  , readAttributeFeatureMapWithClusterStateCache_endpoint_queue_completionSelector
  , readAttributeFeatureMapWithCompletionHandlerSelector
  , readAttributeFeatureMapWithCompletionSelector
  , readAttributeGeneratedCommandListWithAttributeCache_endpoint_queue_completionHandlerSelector
  , readAttributeGeneratedCommandListWithClusterStateCache_endpoint_queue_completionSelector
  , readAttributeGeneratedCommandListWithCompletionHandlerSelector
  , readAttributeGeneratedCommandListWithCompletionSelector
  , readAttributeGroupKeyMapWithAttributeCache_endpoint_queue_completionHandlerSelector
  , readAttributeGroupKeyMapWithClusterStateCache_endpoint_queue_completionSelector
  , readAttributeGroupKeyMapWithParams_completionHandlerSelector
  , readAttributeGroupKeyMapWithParams_completionSelector
  , readAttributeGroupTableWithAttributeCache_endpoint_queue_completionHandlerSelector
  , readAttributeGroupTableWithClusterStateCache_endpoint_queue_completionSelector
  , readAttributeGroupTableWithParams_completionHandlerSelector
  , readAttributeGroupTableWithParams_completionSelector
  , readAttributeMaxGroupKeysPerFabricWithAttributeCache_endpoint_queue_completionHandlerSelector
  , readAttributeMaxGroupKeysPerFabricWithClusterStateCache_endpoint_queue_completionSelector
  , readAttributeMaxGroupKeysPerFabricWithCompletionHandlerSelector
  , readAttributeMaxGroupKeysPerFabricWithCompletionSelector
  , readAttributeMaxGroupsPerFabricWithAttributeCache_endpoint_queue_completionHandlerSelector
  , readAttributeMaxGroupsPerFabricWithClusterStateCache_endpoint_queue_completionSelector
  , readAttributeMaxGroupsPerFabricWithCompletionHandlerSelector
  , readAttributeMaxGroupsPerFabricWithCompletionSelector
  , subscribeAttributeAcceptedCommandListWithMinInterval_maxInterval_params_subscriptionEstablished_reportHandlerSelector
  , subscribeAttributeAcceptedCommandListWithParams_subscriptionEstablished_reportHandlerSelector
  , subscribeAttributeAttributeListWithMinInterval_maxInterval_params_subscriptionEstablished_reportHandlerSelector
  , subscribeAttributeAttributeListWithParams_subscriptionEstablished_reportHandlerSelector
  , subscribeAttributeClusterRevisionWithMinInterval_maxInterval_params_subscriptionEstablished_reportHandlerSelector
  , subscribeAttributeClusterRevisionWithParams_subscriptionEstablished_reportHandlerSelector
  , subscribeAttributeFeatureMapWithMinInterval_maxInterval_params_subscriptionEstablished_reportHandlerSelector
  , subscribeAttributeFeatureMapWithParams_subscriptionEstablished_reportHandlerSelector
  , subscribeAttributeGeneratedCommandListWithMinInterval_maxInterval_params_subscriptionEstablished_reportHandlerSelector
  , subscribeAttributeGeneratedCommandListWithParams_subscriptionEstablished_reportHandlerSelector
  , subscribeAttributeGroupKeyMapWithMinInterval_maxInterval_params_subscriptionEstablished_reportHandlerSelector
  , subscribeAttributeGroupKeyMapWithParams_subscriptionEstablished_reportHandlerSelector
  , subscribeAttributeGroupTableWithMinInterval_maxInterval_params_subscriptionEstablished_reportHandlerSelector
  , subscribeAttributeGroupTableWithParams_subscriptionEstablished_reportHandlerSelector
  , subscribeAttributeMaxGroupKeysPerFabricWithMinInterval_maxInterval_params_subscriptionEstablished_reportHandlerSelector
  , subscribeAttributeMaxGroupKeysPerFabricWithParams_subscriptionEstablished_reportHandlerSelector
  , subscribeAttributeMaxGroupsPerFabricWithMinInterval_maxInterval_params_subscriptionEstablished_reportHandlerSelector
  , subscribeAttributeMaxGroupsPerFabricWithParams_subscriptionEstablished_reportHandlerSelector
  , writeAttributeGroupKeyMapWithValue_completionHandlerSelector
  , writeAttributeGroupKeyMapWithValue_completionSelector
  , writeAttributeGroupKeyMapWithValue_params_completionHandlerSelector
  , writeAttributeGroupKeyMapWithValue_params_completionSelector


  ) where

import Foreign.Ptr (Ptr, FunPtr)
import Foreign.C.Types

import ObjC.Runtime.Types
import ObjC.Runtime.Message (sendMessage, sendOwnedMessage, sendClassMessage, sendOwnedClassMessage)
import ObjC.Runtime.Selector (mkSelector)
import ObjC.Runtime.Class (getRequiredClass)

import ObjC.Matter.Internal.Classes
import ObjC.Foundation.Internal.Classes

-- | Command KeySetWrite
--
-- Write a new set of keys for the given key set id.
--
-- ObjC selector: @- keySetWriteWithParams:completion:@
keySetWriteWithParams_completion :: (IsMTRBaseClusterGroupKeyManagement mtrBaseClusterGroupKeyManagement, IsMTRGroupKeyManagementClusterKeySetWriteParams params) => mtrBaseClusterGroupKeyManagement -> params -> Ptr () -> IO ()
keySetWriteWithParams_completion mtrBaseClusterGroupKeyManagement params completion =
  sendMessage mtrBaseClusterGroupKeyManagement keySetWriteWithParams_completionSelector (toMTRGroupKeyManagementClusterKeySetWriteParams params) completion

-- | Command KeySetRead
--
-- Read the keys for a given key set id.
--
-- ObjC selector: @- keySetReadWithParams:completion:@
keySetReadWithParams_completion :: (IsMTRBaseClusterGroupKeyManagement mtrBaseClusterGroupKeyManagement, IsMTRGroupKeyManagementClusterKeySetReadParams params) => mtrBaseClusterGroupKeyManagement -> params -> Ptr () -> IO ()
keySetReadWithParams_completion mtrBaseClusterGroupKeyManagement params completion =
  sendMessage mtrBaseClusterGroupKeyManagement keySetReadWithParams_completionSelector (toMTRGroupKeyManagementClusterKeySetReadParams params) completion

-- | Command KeySetRemove
--
-- Revoke a Root Key from a Group
--
-- ObjC selector: @- keySetRemoveWithParams:completion:@
keySetRemoveWithParams_completion :: (IsMTRBaseClusterGroupKeyManagement mtrBaseClusterGroupKeyManagement, IsMTRGroupKeyManagementClusterKeySetRemoveParams params) => mtrBaseClusterGroupKeyManagement -> params -> Ptr () -> IO ()
keySetRemoveWithParams_completion mtrBaseClusterGroupKeyManagement params completion =
  sendMessage mtrBaseClusterGroupKeyManagement keySetRemoveWithParams_completionSelector (toMTRGroupKeyManagementClusterKeySetRemoveParams params) completion

-- | Command KeySetReadAllIndices
--
-- Return the list of Group Key Sets associated with the accessing fabric
--
-- ObjC selector: @- keySetReadAllIndicesWithParams:completion:@
keySetReadAllIndicesWithParams_completion :: (IsMTRBaseClusterGroupKeyManagement mtrBaseClusterGroupKeyManagement, IsMTRGroupKeyManagementClusterKeySetReadAllIndicesParams params) => mtrBaseClusterGroupKeyManagement -> params -> Ptr () -> IO ()
keySetReadAllIndicesWithParams_completion mtrBaseClusterGroupKeyManagement params completion =
  sendMessage mtrBaseClusterGroupKeyManagement keySetReadAllIndicesWithParams_completionSelector (toMTRGroupKeyManagementClusterKeySetReadAllIndicesParams params) completion

-- | @- keySetReadAllIndicesWithCompletion:@
keySetReadAllIndicesWithCompletion :: IsMTRBaseClusterGroupKeyManagement mtrBaseClusterGroupKeyManagement => mtrBaseClusterGroupKeyManagement -> Ptr () -> IO ()
keySetReadAllIndicesWithCompletion mtrBaseClusterGroupKeyManagement completion =
  sendMessage mtrBaseClusterGroupKeyManagement keySetReadAllIndicesWithCompletionSelector completion

-- | @- readAttributeGroupKeyMapWithParams:completion:@
readAttributeGroupKeyMapWithParams_completion :: (IsMTRBaseClusterGroupKeyManagement mtrBaseClusterGroupKeyManagement, IsMTRReadParams params) => mtrBaseClusterGroupKeyManagement -> params -> Ptr () -> IO ()
readAttributeGroupKeyMapWithParams_completion mtrBaseClusterGroupKeyManagement params completion =
  sendMessage mtrBaseClusterGroupKeyManagement readAttributeGroupKeyMapWithParams_completionSelector (toMTRReadParams params) completion

-- | @- writeAttributeGroupKeyMapWithValue:completion:@
writeAttributeGroupKeyMapWithValue_completion :: (IsMTRBaseClusterGroupKeyManagement mtrBaseClusterGroupKeyManagement, IsNSArray value) => mtrBaseClusterGroupKeyManagement -> value -> Ptr () -> IO ()
writeAttributeGroupKeyMapWithValue_completion mtrBaseClusterGroupKeyManagement value completion =
  sendMessage mtrBaseClusterGroupKeyManagement writeAttributeGroupKeyMapWithValue_completionSelector (toNSArray value) completion

-- | @- writeAttributeGroupKeyMapWithValue:params:completion:@
writeAttributeGroupKeyMapWithValue_params_completion :: (IsMTRBaseClusterGroupKeyManagement mtrBaseClusterGroupKeyManagement, IsNSArray value, IsMTRWriteParams params) => mtrBaseClusterGroupKeyManagement -> value -> params -> Ptr () -> IO ()
writeAttributeGroupKeyMapWithValue_params_completion mtrBaseClusterGroupKeyManagement value params completion =
  sendMessage mtrBaseClusterGroupKeyManagement writeAttributeGroupKeyMapWithValue_params_completionSelector (toNSArray value) (toMTRWriteParams params) completion

-- | @- subscribeAttributeGroupKeyMapWithParams:subscriptionEstablished:reportHandler:@
subscribeAttributeGroupKeyMapWithParams_subscriptionEstablished_reportHandler :: (IsMTRBaseClusterGroupKeyManagement mtrBaseClusterGroupKeyManagement, IsMTRSubscribeParams params) => mtrBaseClusterGroupKeyManagement -> params -> Ptr () -> Ptr () -> IO ()
subscribeAttributeGroupKeyMapWithParams_subscriptionEstablished_reportHandler mtrBaseClusterGroupKeyManagement params subscriptionEstablished reportHandler =
  sendMessage mtrBaseClusterGroupKeyManagement subscribeAttributeGroupKeyMapWithParams_subscriptionEstablished_reportHandlerSelector (toMTRSubscribeParams params) subscriptionEstablished reportHandler

-- | @+ readAttributeGroupKeyMapWithClusterStateCache:endpoint:queue:completion:@
readAttributeGroupKeyMapWithClusterStateCache_endpoint_queue_completion :: (IsMTRClusterStateCacheContainer clusterStateCacheContainer, IsNSNumber endpoint, IsNSObject queue) => clusterStateCacheContainer -> endpoint -> queue -> Ptr () -> IO ()
readAttributeGroupKeyMapWithClusterStateCache_endpoint_queue_completion clusterStateCacheContainer endpoint queue completion =
  do
    cls' <- getRequiredClass "MTRBaseClusterGroupKeyManagement"
    sendClassMessage cls' readAttributeGroupKeyMapWithClusterStateCache_endpoint_queue_completionSelector (toMTRClusterStateCacheContainer clusterStateCacheContainer) (toNSNumber endpoint) (toNSObject queue) completion

-- | @- readAttributeGroupTableWithParams:completion:@
readAttributeGroupTableWithParams_completion :: (IsMTRBaseClusterGroupKeyManagement mtrBaseClusterGroupKeyManagement, IsMTRReadParams params) => mtrBaseClusterGroupKeyManagement -> params -> Ptr () -> IO ()
readAttributeGroupTableWithParams_completion mtrBaseClusterGroupKeyManagement params completion =
  sendMessage mtrBaseClusterGroupKeyManagement readAttributeGroupTableWithParams_completionSelector (toMTRReadParams params) completion

-- | @- subscribeAttributeGroupTableWithParams:subscriptionEstablished:reportHandler:@
subscribeAttributeGroupTableWithParams_subscriptionEstablished_reportHandler :: (IsMTRBaseClusterGroupKeyManagement mtrBaseClusterGroupKeyManagement, IsMTRSubscribeParams params) => mtrBaseClusterGroupKeyManagement -> params -> Ptr () -> Ptr () -> IO ()
subscribeAttributeGroupTableWithParams_subscriptionEstablished_reportHandler mtrBaseClusterGroupKeyManagement params subscriptionEstablished reportHandler =
  sendMessage mtrBaseClusterGroupKeyManagement subscribeAttributeGroupTableWithParams_subscriptionEstablished_reportHandlerSelector (toMTRSubscribeParams params) subscriptionEstablished reportHandler

-- | @+ readAttributeGroupTableWithClusterStateCache:endpoint:queue:completion:@
readAttributeGroupTableWithClusterStateCache_endpoint_queue_completion :: (IsMTRClusterStateCacheContainer clusterStateCacheContainer, IsNSNumber endpoint, IsNSObject queue) => clusterStateCacheContainer -> endpoint -> queue -> Ptr () -> IO ()
readAttributeGroupTableWithClusterStateCache_endpoint_queue_completion clusterStateCacheContainer endpoint queue completion =
  do
    cls' <- getRequiredClass "MTRBaseClusterGroupKeyManagement"
    sendClassMessage cls' readAttributeGroupTableWithClusterStateCache_endpoint_queue_completionSelector (toMTRClusterStateCacheContainer clusterStateCacheContainer) (toNSNumber endpoint) (toNSObject queue) completion

-- | @- readAttributeMaxGroupsPerFabricWithCompletion:@
readAttributeMaxGroupsPerFabricWithCompletion :: IsMTRBaseClusterGroupKeyManagement mtrBaseClusterGroupKeyManagement => mtrBaseClusterGroupKeyManagement -> Ptr () -> IO ()
readAttributeMaxGroupsPerFabricWithCompletion mtrBaseClusterGroupKeyManagement completion =
  sendMessage mtrBaseClusterGroupKeyManagement readAttributeMaxGroupsPerFabricWithCompletionSelector completion

-- | @- subscribeAttributeMaxGroupsPerFabricWithParams:subscriptionEstablished:reportHandler:@
subscribeAttributeMaxGroupsPerFabricWithParams_subscriptionEstablished_reportHandler :: (IsMTRBaseClusterGroupKeyManagement mtrBaseClusterGroupKeyManagement, IsMTRSubscribeParams params) => mtrBaseClusterGroupKeyManagement -> params -> Ptr () -> Ptr () -> IO ()
subscribeAttributeMaxGroupsPerFabricWithParams_subscriptionEstablished_reportHandler mtrBaseClusterGroupKeyManagement params subscriptionEstablished reportHandler =
  sendMessage mtrBaseClusterGroupKeyManagement subscribeAttributeMaxGroupsPerFabricWithParams_subscriptionEstablished_reportHandlerSelector (toMTRSubscribeParams params) subscriptionEstablished reportHandler

-- | @+ readAttributeMaxGroupsPerFabricWithClusterStateCache:endpoint:queue:completion:@
readAttributeMaxGroupsPerFabricWithClusterStateCache_endpoint_queue_completion :: (IsMTRClusterStateCacheContainer clusterStateCacheContainer, IsNSNumber endpoint, IsNSObject queue) => clusterStateCacheContainer -> endpoint -> queue -> Ptr () -> IO ()
readAttributeMaxGroupsPerFabricWithClusterStateCache_endpoint_queue_completion clusterStateCacheContainer endpoint queue completion =
  do
    cls' <- getRequiredClass "MTRBaseClusterGroupKeyManagement"
    sendClassMessage cls' readAttributeMaxGroupsPerFabricWithClusterStateCache_endpoint_queue_completionSelector (toMTRClusterStateCacheContainer clusterStateCacheContainer) (toNSNumber endpoint) (toNSObject queue) completion

-- | @- readAttributeMaxGroupKeysPerFabricWithCompletion:@
readAttributeMaxGroupKeysPerFabricWithCompletion :: IsMTRBaseClusterGroupKeyManagement mtrBaseClusterGroupKeyManagement => mtrBaseClusterGroupKeyManagement -> Ptr () -> IO ()
readAttributeMaxGroupKeysPerFabricWithCompletion mtrBaseClusterGroupKeyManagement completion =
  sendMessage mtrBaseClusterGroupKeyManagement readAttributeMaxGroupKeysPerFabricWithCompletionSelector completion

-- | @- subscribeAttributeMaxGroupKeysPerFabricWithParams:subscriptionEstablished:reportHandler:@
subscribeAttributeMaxGroupKeysPerFabricWithParams_subscriptionEstablished_reportHandler :: (IsMTRBaseClusterGroupKeyManagement mtrBaseClusterGroupKeyManagement, IsMTRSubscribeParams params) => mtrBaseClusterGroupKeyManagement -> params -> Ptr () -> Ptr () -> IO ()
subscribeAttributeMaxGroupKeysPerFabricWithParams_subscriptionEstablished_reportHandler mtrBaseClusterGroupKeyManagement params subscriptionEstablished reportHandler =
  sendMessage mtrBaseClusterGroupKeyManagement subscribeAttributeMaxGroupKeysPerFabricWithParams_subscriptionEstablished_reportHandlerSelector (toMTRSubscribeParams params) subscriptionEstablished reportHandler

-- | @+ readAttributeMaxGroupKeysPerFabricWithClusterStateCache:endpoint:queue:completion:@
readAttributeMaxGroupKeysPerFabricWithClusterStateCache_endpoint_queue_completion :: (IsMTRClusterStateCacheContainer clusterStateCacheContainer, IsNSNumber endpoint, IsNSObject queue) => clusterStateCacheContainer -> endpoint -> queue -> Ptr () -> IO ()
readAttributeMaxGroupKeysPerFabricWithClusterStateCache_endpoint_queue_completion clusterStateCacheContainer endpoint queue completion =
  do
    cls' <- getRequiredClass "MTRBaseClusterGroupKeyManagement"
    sendClassMessage cls' readAttributeMaxGroupKeysPerFabricWithClusterStateCache_endpoint_queue_completionSelector (toMTRClusterStateCacheContainer clusterStateCacheContainer) (toNSNumber endpoint) (toNSObject queue) completion

-- | @- readAttributeGeneratedCommandListWithCompletion:@
readAttributeGeneratedCommandListWithCompletion :: IsMTRBaseClusterGroupKeyManagement mtrBaseClusterGroupKeyManagement => mtrBaseClusterGroupKeyManagement -> Ptr () -> IO ()
readAttributeGeneratedCommandListWithCompletion mtrBaseClusterGroupKeyManagement completion =
  sendMessage mtrBaseClusterGroupKeyManagement readAttributeGeneratedCommandListWithCompletionSelector completion

-- | @- subscribeAttributeGeneratedCommandListWithParams:subscriptionEstablished:reportHandler:@
subscribeAttributeGeneratedCommandListWithParams_subscriptionEstablished_reportHandler :: (IsMTRBaseClusterGroupKeyManagement mtrBaseClusterGroupKeyManagement, IsMTRSubscribeParams params) => mtrBaseClusterGroupKeyManagement -> params -> Ptr () -> Ptr () -> IO ()
subscribeAttributeGeneratedCommandListWithParams_subscriptionEstablished_reportHandler mtrBaseClusterGroupKeyManagement params subscriptionEstablished reportHandler =
  sendMessage mtrBaseClusterGroupKeyManagement subscribeAttributeGeneratedCommandListWithParams_subscriptionEstablished_reportHandlerSelector (toMTRSubscribeParams params) subscriptionEstablished reportHandler

-- | @+ readAttributeGeneratedCommandListWithClusterStateCache:endpoint:queue:completion:@
readAttributeGeneratedCommandListWithClusterStateCache_endpoint_queue_completion :: (IsMTRClusterStateCacheContainer clusterStateCacheContainer, IsNSNumber endpoint, IsNSObject queue) => clusterStateCacheContainer -> endpoint -> queue -> Ptr () -> IO ()
readAttributeGeneratedCommandListWithClusterStateCache_endpoint_queue_completion clusterStateCacheContainer endpoint queue completion =
  do
    cls' <- getRequiredClass "MTRBaseClusterGroupKeyManagement"
    sendClassMessage cls' readAttributeGeneratedCommandListWithClusterStateCache_endpoint_queue_completionSelector (toMTRClusterStateCacheContainer clusterStateCacheContainer) (toNSNumber endpoint) (toNSObject queue) completion

-- | @- readAttributeAcceptedCommandListWithCompletion:@
readAttributeAcceptedCommandListWithCompletion :: IsMTRBaseClusterGroupKeyManagement mtrBaseClusterGroupKeyManagement => mtrBaseClusterGroupKeyManagement -> Ptr () -> IO ()
readAttributeAcceptedCommandListWithCompletion mtrBaseClusterGroupKeyManagement completion =
  sendMessage mtrBaseClusterGroupKeyManagement readAttributeAcceptedCommandListWithCompletionSelector completion

-- | @- subscribeAttributeAcceptedCommandListWithParams:subscriptionEstablished:reportHandler:@
subscribeAttributeAcceptedCommandListWithParams_subscriptionEstablished_reportHandler :: (IsMTRBaseClusterGroupKeyManagement mtrBaseClusterGroupKeyManagement, IsMTRSubscribeParams params) => mtrBaseClusterGroupKeyManagement -> params -> Ptr () -> Ptr () -> IO ()
subscribeAttributeAcceptedCommandListWithParams_subscriptionEstablished_reportHandler mtrBaseClusterGroupKeyManagement params subscriptionEstablished reportHandler =
  sendMessage mtrBaseClusterGroupKeyManagement subscribeAttributeAcceptedCommandListWithParams_subscriptionEstablished_reportHandlerSelector (toMTRSubscribeParams params) subscriptionEstablished reportHandler

-- | @+ readAttributeAcceptedCommandListWithClusterStateCache:endpoint:queue:completion:@
readAttributeAcceptedCommandListWithClusterStateCache_endpoint_queue_completion :: (IsMTRClusterStateCacheContainer clusterStateCacheContainer, IsNSNumber endpoint, IsNSObject queue) => clusterStateCacheContainer -> endpoint -> queue -> Ptr () -> IO ()
readAttributeAcceptedCommandListWithClusterStateCache_endpoint_queue_completion clusterStateCacheContainer endpoint queue completion =
  do
    cls' <- getRequiredClass "MTRBaseClusterGroupKeyManagement"
    sendClassMessage cls' readAttributeAcceptedCommandListWithClusterStateCache_endpoint_queue_completionSelector (toMTRClusterStateCacheContainer clusterStateCacheContainer) (toNSNumber endpoint) (toNSObject queue) completion

-- | @- readAttributeAttributeListWithCompletion:@
readAttributeAttributeListWithCompletion :: IsMTRBaseClusterGroupKeyManagement mtrBaseClusterGroupKeyManagement => mtrBaseClusterGroupKeyManagement -> Ptr () -> IO ()
readAttributeAttributeListWithCompletion mtrBaseClusterGroupKeyManagement completion =
  sendMessage mtrBaseClusterGroupKeyManagement readAttributeAttributeListWithCompletionSelector completion

-- | @- subscribeAttributeAttributeListWithParams:subscriptionEstablished:reportHandler:@
subscribeAttributeAttributeListWithParams_subscriptionEstablished_reportHandler :: (IsMTRBaseClusterGroupKeyManagement mtrBaseClusterGroupKeyManagement, IsMTRSubscribeParams params) => mtrBaseClusterGroupKeyManagement -> params -> Ptr () -> Ptr () -> IO ()
subscribeAttributeAttributeListWithParams_subscriptionEstablished_reportHandler mtrBaseClusterGroupKeyManagement params subscriptionEstablished reportHandler =
  sendMessage mtrBaseClusterGroupKeyManagement subscribeAttributeAttributeListWithParams_subscriptionEstablished_reportHandlerSelector (toMTRSubscribeParams params) subscriptionEstablished reportHandler

-- | @+ readAttributeAttributeListWithClusterStateCache:endpoint:queue:completion:@
readAttributeAttributeListWithClusterStateCache_endpoint_queue_completion :: (IsMTRClusterStateCacheContainer clusterStateCacheContainer, IsNSNumber endpoint, IsNSObject queue) => clusterStateCacheContainer -> endpoint -> queue -> Ptr () -> IO ()
readAttributeAttributeListWithClusterStateCache_endpoint_queue_completion clusterStateCacheContainer endpoint queue completion =
  do
    cls' <- getRequiredClass "MTRBaseClusterGroupKeyManagement"
    sendClassMessage cls' readAttributeAttributeListWithClusterStateCache_endpoint_queue_completionSelector (toMTRClusterStateCacheContainer clusterStateCacheContainer) (toNSNumber endpoint) (toNSObject queue) completion

-- | @- readAttributeFeatureMapWithCompletion:@
readAttributeFeatureMapWithCompletion :: IsMTRBaseClusterGroupKeyManagement mtrBaseClusterGroupKeyManagement => mtrBaseClusterGroupKeyManagement -> Ptr () -> IO ()
readAttributeFeatureMapWithCompletion mtrBaseClusterGroupKeyManagement completion =
  sendMessage mtrBaseClusterGroupKeyManagement readAttributeFeatureMapWithCompletionSelector completion

-- | @- subscribeAttributeFeatureMapWithParams:subscriptionEstablished:reportHandler:@
subscribeAttributeFeatureMapWithParams_subscriptionEstablished_reportHandler :: (IsMTRBaseClusterGroupKeyManagement mtrBaseClusterGroupKeyManagement, IsMTRSubscribeParams params) => mtrBaseClusterGroupKeyManagement -> params -> Ptr () -> Ptr () -> IO ()
subscribeAttributeFeatureMapWithParams_subscriptionEstablished_reportHandler mtrBaseClusterGroupKeyManagement params subscriptionEstablished reportHandler =
  sendMessage mtrBaseClusterGroupKeyManagement subscribeAttributeFeatureMapWithParams_subscriptionEstablished_reportHandlerSelector (toMTRSubscribeParams params) subscriptionEstablished reportHandler

-- | @+ readAttributeFeatureMapWithClusterStateCache:endpoint:queue:completion:@
readAttributeFeatureMapWithClusterStateCache_endpoint_queue_completion :: (IsMTRClusterStateCacheContainer clusterStateCacheContainer, IsNSNumber endpoint, IsNSObject queue) => clusterStateCacheContainer -> endpoint -> queue -> Ptr () -> IO ()
readAttributeFeatureMapWithClusterStateCache_endpoint_queue_completion clusterStateCacheContainer endpoint queue completion =
  do
    cls' <- getRequiredClass "MTRBaseClusterGroupKeyManagement"
    sendClassMessage cls' readAttributeFeatureMapWithClusterStateCache_endpoint_queue_completionSelector (toMTRClusterStateCacheContainer clusterStateCacheContainer) (toNSNumber endpoint) (toNSObject queue) completion

-- | @- readAttributeClusterRevisionWithCompletion:@
readAttributeClusterRevisionWithCompletion :: IsMTRBaseClusterGroupKeyManagement mtrBaseClusterGroupKeyManagement => mtrBaseClusterGroupKeyManagement -> Ptr () -> IO ()
readAttributeClusterRevisionWithCompletion mtrBaseClusterGroupKeyManagement completion =
  sendMessage mtrBaseClusterGroupKeyManagement readAttributeClusterRevisionWithCompletionSelector completion

-- | @- subscribeAttributeClusterRevisionWithParams:subscriptionEstablished:reportHandler:@
subscribeAttributeClusterRevisionWithParams_subscriptionEstablished_reportHandler :: (IsMTRBaseClusterGroupKeyManagement mtrBaseClusterGroupKeyManagement, IsMTRSubscribeParams params) => mtrBaseClusterGroupKeyManagement -> params -> Ptr () -> Ptr () -> IO ()
subscribeAttributeClusterRevisionWithParams_subscriptionEstablished_reportHandler mtrBaseClusterGroupKeyManagement params subscriptionEstablished reportHandler =
  sendMessage mtrBaseClusterGroupKeyManagement subscribeAttributeClusterRevisionWithParams_subscriptionEstablished_reportHandlerSelector (toMTRSubscribeParams params) subscriptionEstablished reportHandler

-- | @+ readAttributeClusterRevisionWithClusterStateCache:endpoint:queue:completion:@
readAttributeClusterRevisionWithClusterStateCache_endpoint_queue_completion :: (IsMTRClusterStateCacheContainer clusterStateCacheContainer, IsNSNumber endpoint, IsNSObject queue) => clusterStateCacheContainer -> endpoint -> queue -> Ptr () -> IO ()
readAttributeClusterRevisionWithClusterStateCache_endpoint_queue_completion clusterStateCacheContainer endpoint queue completion =
  do
    cls' <- getRequiredClass "MTRBaseClusterGroupKeyManagement"
    sendClassMessage cls' readAttributeClusterRevisionWithClusterStateCache_endpoint_queue_completionSelector (toMTRClusterStateCacheContainer clusterStateCacheContainer) (toNSNumber endpoint) (toNSObject queue) completion

-- | @- init@
init_ :: IsMTRBaseClusterGroupKeyManagement mtrBaseClusterGroupKeyManagement => mtrBaseClusterGroupKeyManagement -> IO (Id MTRBaseClusterGroupKeyManagement)
init_ mtrBaseClusterGroupKeyManagement =
  sendOwnedMessage mtrBaseClusterGroupKeyManagement initSelector

-- | @+ new@
new :: IO (Id MTRBaseClusterGroupKeyManagement)
new  =
  do
    cls' <- getRequiredClass "MTRBaseClusterGroupKeyManagement"
    sendOwnedClassMessage cls' newSelector

-- | @- initWithDevice:endpoint:queue:@
initWithDevice_endpoint_queue :: (IsMTRBaseClusterGroupKeyManagement mtrBaseClusterGroupKeyManagement, IsMTRBaseDevice device, IsNSObject queue) => mtrBaseClusterGroupKeyManagement -> device -> CUShort -> queue -> IO (Id MTRBaseClusterGroupKeyManagement)
initWithDevice_endpoint_queue mtrBaseClusterGroupKeyManagement device endpoint queue =
  sendOwnedMessage mtrBaseClusterGroupKeyManagement initWithDevice_endpoint_queueSelector (toMTRBaseDevice device) endpoint (toNSObject queue)

-- | @- keySetWriteWithParams:completionHandler:@
keySetWriteWithParams_completionHandler :: (IsMTRBaseClusterGroupKeyManagement mtrBaseClusterGroupKeyManagement, IsMTRGroupKeyManagementClusterKeySetWriteParams params) => mtrBaseClusterGroupKeyManagement -> params -> Ptr () -> IO ()
keySetWriteWithParams_completionHandler mtrBaseClusterGroupKeyManagement params completionHandler =
  sendMessage mtrBaseClusterGroupKeyManagement keySetWriteWithParams_completionHandlerSelector (toMTRGroupKeyManagementClusterKeySetWriteParams params) completionHandler

-- | @- keySetReadWithParams:completionHandler:@
keySetReadWithParams_completionHandler :: (IsMTRBaseClusterGroupKeyManagement mtrBaseClusterGroupKeyManagement, IsMTRGroupKeyManagementClusterKeySetReadParams params) => mtrBaseClusterGroupKeyManagement -> params -> Ptr () -> IO ()
keySetReadWithParams_completionHandler mtrBaseClusterGroupKeyManagement params completionHandler =
  sendMessage mtrBaseClusterGroupKeyManagement keySetReadWithParams_completionHandlerSelector (toMTRGroupKeyManagementClusterKeySetReadParams params) completionHandler

-- | @- keySetRemoveWithParams:completionHandler:@
keySetRemoveWithParams_completionHandler :: (IsMTRBaseClusterGroupKeyManagement mtrBaseClusterGroupKeyManagement, IsMTRGroupKeyManagementClusterKeySetRemoveParams params) => mtrBaseClusterGroupKeyManagement -> params -> Ptr () -> IO ()
keySetRemoveWithParams_completionHandler mtrBaseClusterGroupKeyManagement params completionHandler =
  sendMessage mtrBaseClusterGroupKeyManagement keySetRemoveWithParams_completionHandlerSelector (toMTRGroupKeyManagementClusterKeySetRemoveParams params) completionHandler

-- | @- keySetReadAllIndicesWithParams:completionHandler:@
keySetReadAllIndicesWithParams_completionHandler :: (IsMTRBaseClusterGroupKeyManagement mtrBaseClusterGroupKeyManagement, IsMTRGroupKeyManagementClusterKeySetReadAllIndicesParams params) => mtrBaseClusterGroupKeyManagement -> params -> Ptr () -> IO ()
keySetReadAllIndicesWithParams_completionHandler mtrBaseClusterGroupKeyManagement params completionHandler =
  sendMessage mtrBaseClusterGroupKeyManagement keySetReadAllIndicesWithParams_completionHandlerSelector (toMTRGroupKeyManagementClusterKeySetReadAllIndicesParams params) completionHandler

-- | @- readAttributeGroupKeyMapWithParams:completionHandler:@
readAttributeGroupKeyMapWithParams_completionHandler :: (IsMTRBaseClusterGroupKeyManagement mtrBaseClusterGroupKeyManagement, IsMTRReadParams params) => mtrBaseClusterGroupKeyManagement -> params -> Ptr () -> IO ()
readAttributeGroupKeyMapWithParams_completionHandler mtrBaseClusterGroupKeyManagement params completionHandler =
  sendMessage mtrBaseClusterGroupKeyManagement readAttributeGroupKeyMapWithParams_completionHandlerSelector (toMTRReadParams params) completionHandler

-- | @- writeAttributeGroupKeyMapWithValue:completionHandler:@
writeAttributeGroupKeyMapWithValue_completionHandler :: (IsMTRBaseClusterGroupKeyManagement mtrBaseClusterGroupKeyManagement, IsNSArray value) => mtrBaseClusterGroupKeyManagement -> value -> Ptr () -> IO ()
writeAttributeGroupKeyMapWithValue_completionHandler mtrBaseClusterGroupKeyManagement value completionHandler =
  sendMessage mtrBaseClusterGroupKeyManagement writeAttributeGroupKeyMapWithValue_completionHandlerSelector (toNSArray value) completionHandler

-- | @- writeAttributeGroupKeyMapWithValue:params:completionHandler:@
writeAttributeGroupKeyMapWithValue_params_completionHandler :: (IsMTRBaseClusterGroupKeyManagement mtrBaseClusterGroupKeyManagement, IsNSArray value, IsMTRWriteParams params) => mtrBaseClusterGroupKeyManagement -> value -> params -> Ptr () -> IO ()
writeAttributeGroupKeyMapWithValue_params_completionHandler mtrBaseClusterGroupKeyManagement value params completionHandler =
  sendMessage mtrBaseClusterGroupKeyManagement writeAttributeGroupKeyMapWithValue_params_completionHandlerSelector (toNSArray value) (toMTRWriteParams params) completionHandler

-- | @- subscribeAttributeGroupKeyMapWithMinInterval:maxInterval:params:subscriptionEstablished:reportHandler:@
subscribeAttributeGroupKeyMapWithMinInterval_maxInterval_params_subscriptionEstablished_reportHandler :: (IsMTRBaseClusterGroupKeyManagement mtrBaseClusterGroupKeyManagement, IsNSNumber minInterval, IsNSNumber maxInterval, IsMTRSubscribeParams params) => mtrBaseClusterGroupKeyManagement -> minInterval -> maxInterval -> params -> Ptr () -> Ptr () -> IO ()
subscribeAttributeGroupKeyMapWithMinInterval_maxInterval_params_subscriptionEstablished_reportHandler mtrBaseClusterGroupKeyManagement minInterval maxInterval params subscriptionEstablishedHandler reportHandler =
  sendMessage mtrBaseClusterGroupKeyManagement subscribeAttributeGroupKeyMapWithMinInterval_maxInterval_params_subscriptionEstablished_reportHandlerSelector (toNSNumber minInterval) (toNSNumber maxInterval) (toMTRSubscribeParams params) subscriptionEstablishedHandler reportHandler

-- | @+ readAttributeGroupKeyMapWithAttributeCache:endpoint:queue:completionHandler:@
readAttributeGroupKeyMapWithAttributeCache_endpoint_queue_completionHandler :: (IsMTRAttributeCacheContainer attributeCacheContainer, IsNSNumber endpoint, IsNSObject queue) => attributeCacheContainer -> endpoint -> queue -> Ptr () -> IO ()
readAttributeGroupKeyMapWithAttributeCache_endpoint_queue_completionHandler attributeCacheContainer endpoint queue completionHandler =
  do
    cls' <- getRequiredClass "MTRBaseClusterGroupKeyManagement"
    sendClassMessage cls' readAttributeGroupKeyMapWithAttributeCache_endpoint_queue_completionHandlerSelector (toMTRAttributeCacheContainer attributeCacheContainer) (toNSNumber endpoint) (toNSObject queue) completionHandler

-- | @- readAttributeGroupTableWithParams:completionHandler:@
readAttributeGroupTableWithParams_completionHandler :: (IsMTRBaseClusterGroupKeyManagement mtrBaseClusterGroupKeyManagement, IsMTRReadParams params) => mtrBaseClusterGroupKeyManagement -> params -> Ptr () -> IO ()
readAttributeGroupTableWithParams_completionHandler mtrBaseClusterGroupKeyManagement params completionHandler =
  sendMessage mtrBaseClusterGroupKeyManagement readAttributeGroupTableWithParams_completionHandlerSelector (toMTRReadParams params) completionHandler

-- | @- subscribeAttributeGroupTableWithMinInterval:maxInterval:params:subscriptionEstablished:reportHandler:@
subscribeAttributeGroupTableWithMinInterval_maxInterval_params_subscriptionEstablished_reportHandler :: (IsMTRBaseClusterGroupKeyManagement mtrBaseClusterGroupKeyManagement, IsNSNumber minInterval, IsNSNumber maxInterval, IsMTRSubscribeParams params) => mtrBaseClusterGroupKeyManagement -> minInterval -> maxInterval -> params -> Ptr () -> Ptr () -> IO ()
subscribeAttributeGroupTableWithMinInterval_maxInterval_params_subscriptionEstablished_reportHandler mtrBaseClusterGroupKeyManagement minInterval maxInterval params subscriptionEstablishedHandler reportHandler =
  sendMessage mtrBaseClusterGroupKeyManagement subscribeAttributeGroupTableWithMinInterval_maxInterval_params_subscriptionEstablished_reportHandlerSelector (toNSNumber minInterval) (toNSNumber maxInterval) (toMTRSubscribeParams params) subscriptionEstablishedHandler reportHandler

-- | @+ readAttributeGroupTableWithAttributeCache:endpoint:queue:completionHandler:@
readAttributeGroupTableWithAttributeCache_endpoint_queue_completionHandler :: (IsMTRAttributeCacheContainer attributeCacheContainer, IsNSNumber endpoint, IsNSObject queue) => attributeCacheContainer -> endpoint -> queue -> Ptr () -> IO ()
readAttributeGroupTableWithAttributeCache_endpoint_queue_completionHandler attributeCacheContainer endpoint queue completionHandler =
  do
    cls' <- getRequiredClass "MTRBaseClusterGroupKeyManagement"
    sendClassMessage cls' readAttributeGroupTableWithAttributeCache_endpoint_queue_completionHandlerSelector (toMTRAttributeCacheContainer attributeCacheContainer) (toNSNumber endpoint) (toNSObject queue) completionHandler

-- | @- readAttributeMaxGroupsPerFabricWithCompletionHandler:@
readAttributeMaxGroupsPerFabricWithCompletionHandler :: IsMTRBaseClusterGroupKeyManagement mtrBaseClusterGroupKeyManagement => mtrBaseClusterGroupKeyManagement -> Ptr () -> IO ()
readAttributeMaxGroupsPerFabricWithCompletionHandler mtrBaseClusterGroupKeyManagement completionHandler =
  sendMessage mtrBaseClusterGroupKeyManagement readAttributeMaxGroupsPerFabricWithCompletionHandlerSelector completionHandler

-- | @- subscribeAttributeMaxGroupsPerFabricWithMinInterval:maxInterval:params:subscriptionEstablished:reportHandler:@
subscribeAttributeMaxGroupsPerFabricWithMinInterval_maxInterval_params_subscriptionEstablished_reportHandler :: (IsMTRBaseClusterGroupKeyManagement mtrBaseClusterGroupKeyManagement, IsNSNumber minInterval, IsNSNumber maxInterval, IsMTRSubscribeParams params) => mtrBaseClusterGroupKeyManagement -> minInterval -> maxInterval -> params -> Ptr () -> Ptr () -> IO ()
subscribeAttributeMaxGroupsPerFabricWithMinInterval_maxInterval_params_subscriptionEstablished_reportHandler mtrBaseClusterGroupKeyManagement minInterval maxInterval params subscriptionEstablishedHandler reportHandler =
  sendMessage mtrBaseClusterGroupKeyManagement subscribeAttributeMaxGroupsPerFabricWithMinInterval_maxInterval_params_subscriptionEstablished_reportHandlerSelector (toNSNumber minInterval) (toNSNumber maxInterval) (toMTRSubscribeParams params) subscriptionEstablishedHandler reportHandler

-- | @+ readAttributeMaxGroupsPerFabricWithAttributeCache:endpoint:queue:completionHandler:@
readAttributeMaxGroupsPerFabricWithAttributeCache_endpoint_queue_completionHandler :: (IsMTRAttributeCacheContainer attributeCacheContainer, IsNSNumber endpoint, IsNSObject queue) => attributeCacheContainer -> endpoint -> queue -> Ptr () -> IO ()
readAttributeMaxGroupsPerFabricWithAttributeCache_endpoint_queue_completionHandler attributeCacheContainer endpoint queue completionHandler =
  do
    cls' <- getRequiredClass "MTRBaseClusterGroupKeyManagement"
    sendClassMessage cls' readAttributeMaxGroupsPerFabricWithAttributeCache_endpoint_queue_completionHandlerSelector (toMTRAttributeCacheContainer attributeCacheContainer) (toNSNumber endpoint) (toNSObject queue) completionHandler

-- | @- readAttributeMaxGroupKeysPerFabricWithCompletionHandler:@
readAttributeMaxGroupKeysPerFabricWithCompletionHandler :: IsMTRBaseClusterGroupKeyManagement mtrBaseClusterGroupKeyManagement => mtrBaseClusterGroupKeyManagement -> Ptr () -> IO ()
readAttributeMaxGroupKeysPerFabricWithCompletionHandler mtrBaseClusterGroupKeyManagement completionHandler =
  sendMessage mtrBaseClusterGroupKeyManagement readAttributeMaxGroupKeysPerFabricWithCompletionHandlerSelector completionHandler

-- | @- subscribeAttributeMaxGroupKeysPerFabricWithMinInterval:maxInterval:params:subscriptionEstablished:reportHandler:@
subscribeAttributeMaxGroupKeysPerFabricWithMinInterval_maxInterval_params_subscriptionEstablished_reportHandler :: (IsMTRBaseClusterGroupKeyManagement mtrBaseClusterGroupKeyManagement, IsNSNumber minInterval, IsNSNumber maxInterval, IsMTRSubscribeParams params) => mtrBaseClusterGroupKeyManagement -> minInterval -> maxInterval -> params -> Ptr () -> Ptr () -> IO ()
subscribeAttributeMaxGroupKeysPerFabricWithMinInterval_maxInterval_params_subscriptionEstablished_reportHandler mtrBaseClusterGroupKeyManagement minInterval maxInterval params subscriptionEstablishedHandler reportHandler =
  sendMessage mtrBaseClusterGroupKeyManagement subscribeAttributeMaxGroupKeysPerFabricWithMinInterval_maxInterval_params_subscriptionEstablished_reportHandlerSelector (toNSNumber minInterval) (toNSNumber maxInterval) (toMTRSubscribeParams params) subscriptionEstablishedHandler reportHandler

-- | @+ readAttributeMaxGroupKeysPerFabricWithAttributeCache:endpoint:queue:completionHandler:@
readAttributeMaxGroupKeysPerFabricWithAttributeCache_endpoint_queue_completionHandler :: (IsMTRAttributeCacheContainer attributeCacheContainer, IsNSNumber endpoint, IsNSObject queue) => attributeCacheContainer -> endpoint -> queue -> Ptr () -> IO ()
readAttributeMaxGroupKeysPerFabricWithAttributeCache_endpoint_queue_completionHandler attributeCacheContainer endpoint queue completionHandler =
  do
    cls' <- getRequiredClass "MTRBaseClusterGroupKeyManagement"
    sendClassMessage cls' readAttributeMaxGroupKeysPerFabricWithAttributeCache_endpoint_queue_completionHandlerSelector (toMTRAttributeCacheContainer attributeCacheContainer) (toNSNumber endpoint) (toNSObject queue) completionHandler

-- | @- readAttributeGeneratedCommandListWithCompletionHandler:@
readAttributeGeneratedCommandListWithCompletionHandler :: IsMTRBaseClusterGroupKeyManagement mtrBaseClusterGroupKeyManagement => mtrBaseClusterGroupKeyManagement -> Ptr () -> IO ()
readAttributeGeneratedCommandListWithCompletionHandler mtrBaseClusterGroupKeyManagement completionHandler =
  sendMessage mtrBaseClusterGroupKeyManagement readAttributeGeneratedCommandListWithCompletionHandlerSelector completionHandler

-- | @- subscribeAttributeGeneratedCommandListWithMinInterval:maxInterval:params:subscriptionEstablished:reportHandler:@
subscribeAttributeGeneratedCommandListWithMinInterval_maxInterval_params_subscriptionEstablished_reportHandler :: (IsMTRBaseClusterGroupKeyManagement mtrBaseClusterGroupKeyManagement, IsNSNumber minInterval, IsNSNumber maxInterval, IsMTRSubscribeParams params) => mtrBaseClusterGroupKeyManagement -> minInterval -> maxInterval -> params -> Ptr () -> Ptr () -> IO ()
subscribeAttributeGeneratedCommandListWithMinInterval_maxInterval_params_subscriptionEstablished_reportHandler mtrBaseClusterGroupKeyManagement minInterval maxInterval params subscriptionEstablishedHandler reportHandler =
  sendMessage mtrBaseClusterGroupKeyManagement subscribeAttributeGeneratedCommandListWithMinInterval_maxInterval_params_subscriptionEstablished_reportHandlerSelector (toNSNumber minInterval) (toNSNumber maxInterval) (toMTRSubscribeParams params) subscriptionEstablishedHandler reportHandler

-- | @+ readAttributeGeneratedCommandListWithAttributeCache:endpoint:queue:completionHandler:@
readAttributeGeneratedCommandListWithAttributeCache_endpoint_queue_completionHandler :: (IsMTRAttributeCacheContainer attributeCacheContainer, IsNSNumber endpoint, IsNSObject queue) => attributeCacheContainer -> endpoint -> queue -> Ptr () -> IO ()
readAttributeGeneratedCommandListWithAttributeCache_endpoint_queue_completionHandler attributeCacheContainer endpoint queue completionHandler =
  do
    cls' <- getRequiredClass "MTRBaseClusterGroupKeyManagement"
    sendClassMessage cls' readAttributeGeneratedCommandListWithAttributeCache_endpoint_queue_completionHandlerSelector (toMTRAttributeCacheContainer attributeCacheContainer) (toNSNumber endpoint) (toNSObject queue) completionHandler

-- | @- readAttributeAcceptedCommandListWithCompletionHandler:@
readAttributeAcceptedCommandListWithCompletionHandler :: IsMTRBaseClusterGroupKeyManagement mtrBaseClusterGroupKeyManagement => mtrBaseClusterGroupKeyManagement -> Ptr () -> IO ()
readAttributeAcceptedCommandListWithCompletionHandler mtrBaseClusterGroupKeyManagement completionHandler =
  sendMessage mtrBaseClusterGroupKeyManagement readAttributeAcceptedCommandListWithCompletionHandlerSelector completionHandler

-- | @- subscribeAttributeAcceptedCommandListWithMinInterval:maxInterval:params:subscriptionEstablished:reportHandler:@
subscribeAttributeAcceptedCommandListWithMinInterval_maxInterval_params_subscriptionEstablished_reportHandler :: (IsMTRBaseClusterGroupKeyManagement mtrBaseClusterGroupKeyManagement, IsNSNumber minInterval, IsNSNumber maxInterval, IsMTRSubscribeParams params) => mtrBaseClusterGroupKeyManagement -> minInterval -> maxInterval -> params -> Ptr () -> Ptr () -> IO ()
subscribeAttributeAcceptedCommandListWithMinInterval_maxInterval_params_subscriptionEstablished_reportHandler mtrBaseClusterGroupKeyManagement minInterval maxInterval params subscriptionEstablishedHandler reportHandler =
  sendMessage mtrBaseClusterGroupKeyManagement subscribeAttributeAcceptedCommandListWithMinInterval_maxInterval_params_subscriptionEstablished_reportHandlerSelector (toNSNumber minInterval) (toNSNumber maxInterval) (toMTRSubscribeParams params) subscriptionEstablishedHandler reportHandler

-- | @+ readAttributeAcceptedCommandListWithAttributeCache:endpoint:queue:completionHandler:@
readAttributeAcceptedCommandListWithAttributeCache_endpoint_queue_completionHandler :: (IsMTRAttributeCacheContainer attributeCacheContainer, IsNSNumber endpoint, IsNSObject queue) => attributeCacheContainer -> endpoint -> queue -> Ptr () -> IO ()
readAttributeAcceptedCommandListWithAttributeCache_endpoint_queue_completionHandler attributeCacheContainer endpoint queue completionHandler =
  do
    cls' <- getRequiredClass "MTRBaseClusterGroupKeyManagement"
    sendClassMessage cls' readAttributeAcceptedCommandListWithAttributeCache_endpoint_queue_completionHandlerSelector (toMTRAttributeCacheContainer attributeCacheContainer) (toNSNumber endpoint) (toNSObject queue) completionHandler

-- | @- readAttributeAttributeListWithCompletionHandler:@
readAttributeAttributeListWithCompletionHandler :: IsMTRBaseClusterGroupKeyManagement mtrBaseClusterGroupKeyManagement => mtrBaseClusterGroupKeyManagement -> Ptr () -> IO ()
readAttributeAttributeListWithCompletionHandler mtrBaseClusterGroupKeyManagement completionHandler =
  sendMessage mtrBaseClusterGroupKeyManagement readAttributeAttributeListWithCompletionHandlerSelector completionHandler

-- | @- subscribeAttributeAttributeListWithMinInterval:maxInterval:params:subscriptionEstablished:reportHandler:@
subscribeAttributeAttributeListWithMinInterval_maxInterval_params_subscriptionEstablished_reportHandler :: (IsMTRBaseClusterGroupKeyManagement mtrBaseClusterGroupKeyManagement, IsNSNumber minInterval, IsNSNumber maxInterval, IsMTRSubscribeParams params) => mtrBaseClusterGroupKeyManagement -> minInterval -> maxInterval -> params -> Ptr () -> Ptr () -> IO ()
subscribeAttributeAttributeListWithMinInterval_maxInterval_params_subscriptionEstablished_reportHandler mtrBaseClusterGroupKeyManagement minInterval maxInterval params subscriptionEstablishedHandler reportHandler =
  sendMessage mtrBaseClusterGroupKeyManagement subscribeAttributeAttributeListWithMinInterval_maxInterval_params_subscriptionEstablished_reportHandlerSelector (toNSNumber minInterval) (toNSNumber maxInterval) (toMTRSubscribeParams params) subscriptionEstablishedHandler reportHandler

-- | @+ readAttributeAttributeListWithAttributeCache:endpoint:queue:completionHandler:@
readAttributeAttributeListWithAttributeCache_endpoint_queue_completionHandler :: (IsMTRAttributeCacheContainer attributeCacheContainer, IsNSNumber endpoint, IsNSObject queue) => attributeCacheContainer -> endpoint -> queue -> Ptr () -> IO ()
readAttributeAttributeListWithAttributeCache_endpoint_queue_completionHandler attributeCacheContainer endpoint queue completionHandler =
  do
    cls' <- getRequiredClass "MTRBaseClusterGroupKeyManagement"
    sendClassMessage cls' readAttributeAttributeListWithAttributeCache_endpoint_queue_completionHandlerSelector (toMTRAttributeCacheContainer attributeCacheContainer) (toNSNumber endpoint) (toNSObject queue) completionHandler

-- | @- readAttributeFeatureMapWithCompletionHandler:@
readAttributeFeatureMapWithCompletionHandler :: IsMTRBaseClusterGroupKeyManagement mtrBaseClusterGroupKeyManagement => mtrBaseClusterGroupKeyManagement -> Ptr () -> IO ()
readAttributeFeatureMapWithCompletionHandler mtrBaseClusterGroupKeyManagement completionHandler =
  sendMessage mtrBaseClusterGroupKeyManagement readAttributeFeatureMapWithCompletionHandlerSelector completionHandler

-- | @- subscribeAttributeFeatureMapWithMinInterval:maxInterval:params:subscriptionEstablished:reportHandler:@
subscribeAttributeFeatureMapWithMinInterval_maxInterval_params_subscriptionEstablished_reportHandler :: (IsMTRBaseClusterGroupKeyManagement mtrBaseClusterGroupKeyManagement, IsNSNumber minInterval, IsNSNumber maxInterval, IsMTRSubscribeParams params) => mtrBaseClusterGroupKeyManagement -> minInterval -> maxInterval -> params -> Ptr () -> Ptr () -> IO ()
subscribeAttributeFeatureMapWithMinInterval_maxInterval_params_subscriptionEstablished_reportHandler mtrBaseClusterGroupKeyManagement minInterval maxInterval params subscriptionEstablishedHandler reportHandler =
  sendMessage mtrBaseClusterGroupKeyManagement subscribeAttributeFeatureMapWithMinInterval_maxInterval_params_subscriptionEstablished_reportHandlerSelector (toNSNumber minInterval) (toNSNumber maxInterval) (toMTRSubscribeParams params) subscriptionEstablishedHandler reportHandler

-- | @+ readAttributeFeatureMapWithAttributeCache:endpoint:queue:completionHandler:@
readAttributeFeatureMapWithAttributeCache_endpoint_queue_completionHandler :: (IsMTRAttributeCacheContainer attributeCacheContainer, IsNSNumber endpoint, IsNSObject queue) => attributeCacheContainer -> endpoint -> queue -> Ptr () -> IO ()
readAttributeFeatureMapWithAttributeCache_endpoint_queue_completionHandler attributeCacheContainer endpoint queue completionHandler =
  do
    cls' <- getRequiredClass "MTRBaseClusterGroupKeyManagement"
    sendClassMessage cls' readAttributeFeatureMapWithAttributeCache_endpoint_queue_completionHandlerSelector (toMTRAttributeCacheContainer attributeCacheContainer) (toNSNumber endpoint) (toNSObject queue) completionHandler

-- | @- readAttributeClusterRevisionWithCompletionHandler:@
readAttributeClusterRevisionWithCompletionHandler :: IsMTRBaseClusterGroupKeyManagement mtrBaseClusterGroupKeyManagement => mtrBaseClusterGroupKeyManagement -> Ptr () -> IO ()
readAttributeClusterRevisionWithCompletionHandler mtrBaseClusterGroupKeyManagement completionHandler =
  sendMessage mtrBaseClusterGroupKeyManagement readAttributeClusterRevisionWithCompletionHandlerSelector completionHandler

-- | @- subscribeAttributeClusterRevisionWithMinInterval:maxInterval:params:subscriptionEstablished:reportHandler:@
subscribeAttributeClusterRevisionWithMinInterval_maxInterval_params_subscriptionEstablished_reportHandler :: (IsMTRBaseClusterGroupKeyManagement mtrBaseClusterGroupKeyManagement, IsNSNumber minInterval, IsNSNumber maxInterval, IsMTRSubscribeParams params) => mtrBaseClusterGroupKeyManagement -> minInterval -> maxInterval -> params -> Ptr () -> Ptr () -> IO ()
subscribeAttributeClusterRevisionWithMinInterval_maxInterval_params_subscriptionEstablished_reportHandler mtrBaseClusterGroupKeyManagement minInterval maxInterval params subscriptionEstablishedHandler reportHandler =
  sendMessage mtrBaseClusterGroupKeyManagement subscribeAttributeClusterRevisionWithMinInterval_maxInterval_params_subscriptionEstablished_reportHandlerSelector (toNSNumber minInterval) (toNSNumber maxInterval) (toMTRSubscribeParams params) subscriptionEstablishedHandler reportHandler

-- | @+ readAttributeClusterRevisionWithAttributeCache:endpoint:queue:completionHandler:@
readAttributeClusterRevisionWithAttributeCache_endpoint_queue_completionHandler :: (IsMTRAttributeCacheContainer attributeCacheContainer, IsNSNumber endpoint, IsNSObject queue) => attributeCacheContainer -> endpoint -> queue -> Ptr () -> IO ()
readAttributeClusterRevisionWithAttributeCache_endpoint_queue_completionHandler attributeCacheContainer endpoint queue completionHandler =
  do
    cls' <- getRequiredClass "MTRBaseClusterGroupKeyManagement"
    sendClassMessage cls' readAttributeClusterRevisionWithAttributeCache_endpoint_queue_completionHandlerSelector (toMTRAttributeCacheContainer attributeCacheContainer) (toNSNumber endpoint) (toNSObject queue) completionHandler

-- | For all instance methods (reads, writes, commands) that take a completion, the completion will be called on the provided queue.
--
-- ObjC selector: @- initWithDevice:endpointID:queue:@
initWithDevice_endpointID_queue :: (IsMTRBaseClusterGroupKeyManagement mtrBaseClusterGroupKeyManagement, IsMTRBaseDevice device, IsNSNumber endpointID, IsNSObject queue) => mtrBaseClusterGroupKeyManagement -> device -> endpointID -> queue -> IO (Id MTRBaseClusterGroupKeyManagement)
initWithDevice_endpointID_queue mtrBaseClusterGroupKeyManagement device endpointID queue =
  sendOwnedMessage mtrBaseClusterGroupKeyManagement initWithDevice_endpointID_queueSelector (toMTRBaseDevice device) (toNSNumber endpointID) (toNSObject queue)

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @keySetWriteWithParams:completion:@
keySetWriteWithParams_completionSelector :: Selector '[Id MTRGroupKeyManagementClusterKeySetWriteParams, Ptr ()] ()
keySetWriteWithParams_completionSelector = mkSelector "keySetWriteWithParams:completion:"

-- | @Selector@ for @keySetReadWithParams:completion:@
keySetReadWithParams_completionSelector :: Selector '[Id MTRGroupKeyManagementClusterKeySetReadParams, Ptr ()] ()
keySetReadWithParams_completionSelector = mkSelector "keySetReadWithParams:completion:"

-- | @Selector@ for @keySetRemoveWithParams:completion:@
keySetRemoveWithParams_completionSelector :: Selector '[Id MTRGroupKeyManagementClusterKeySetRemoveParams, Ptr ()] ()
keySetRemoveWithParams_completionSelector = mkSelector "keySetRemoveWithParams:completion:"

-- | @Selector@ for @keySetReadAllIndicesWithParams:completion:@
keySetReadAllIndicesWithParams_completionSelector :: Selector '[Id MTRGroupKeyManagementClusterKeySetReadAllIndicesParams, Ptr ()] ()
keySetReadAllIndicesWithParams_completionSelector = mkSelector "keySetReadAllIndicesWithParams:completion:"

-- | @Selector@ for @keySetReadAllIndicesWithCompletion:@
keySetReadAllIndicesWithCompletionSelector :: Selector '[Ptr ()] ()
keySetReadAllIndicesWithCompletionSelector = mkSelector "keySetReadAllIndicesWithCompletion:"

-- | @Selector@ for @readAttributeGroupKeyMapWithParams:completion:@
readAttributeGroupKeyMapWithParams_completionSelector :: Selector '[Id MTRReadParams, Ptr ()] ()
readAttributeGroupKeyMapWithParams_completionSelector = mkSelector "readAttributeGroupKeyMapWithParams:completion:"

-- | @Selector@ for @writeAttributeGroupKeyMapWithValue:completion:@
writeAttributeGroupKeyMapWithValue_completionSelector :: Selector '[Id NSArray, Ptr ()] ()
writeAttributeGroupKeyMapWithValue_completionSelector = mkSelector "writeAttributeGroupKeyMapWithValue:completion:"

-- | @Selector@ for @writeAttributeGroupKeyMapWithValue:params:completion:@
writeAttributeGroupKeyMapWithValue_params_completionSelector :: Selector '[Id NSArray, Id MTRWriteParams, Ptr ()] ()
writeAttributeGroupKeyMapWithValue_params_completionSelector = mkSelector "writeAttributeGroupKeyMapWithValue:params:completion:"

-- | @Selector@ for @subscribeAttributeGroupKeyMapWithParams:subscriptionEstablished:reportHandler:@
subscribeAttributeGroupKeyMapWithParams_subscriptionEstablished_reportHandlerSelector :: Selector '[Id MTRSubscribeParams, Ptr (), Ptr ()] ()
subscribeAttributeGroupKeyMapWithParams_subscriptionEstablished_reportHandlerSelector = mkSelector "subscribeAttributeGroupKeyMapWithParams:subscriptionEstablished:reportHandler:"

-- | @Selector@ for @readAttributeGroupKeyMapWithClusterStateCache:endpoint:queue:completion:@
readAttributeGroupKeyMapWithClusterStateCache_endpoint_queue_completionSelector :: Selector '[Id MTRClusterStateCacheContainer, Id NSNumber, Id NSObject, Ptr ()] ()
readAttributeGroupKeyMapWithClusterStateCache_endpoint_queue_completionSelector = mkSelector "readAttributeGroupKeyMapWithClusterStateCache:endpoint:queue:completion:"

-- | @Selector@ for @readAttributeGroupTableWithParams:completion:@
readAttributeGroupTableWithParams_completionSelector :: Selector '[Id MTRReadParams, Ptr ()] ()
readAttributeGroupTableWithParams_completionSelector = mkSelector "readAttributeGroupTableWithParams:completion:"

-- | @Selector@ for @subscribeAttributeGroupTableWithParams:subscriptionEstablished:reportHandler:@
subscribeAttributeGroupTableWithParams_subscriptionEstablished_reportHandlerSelector :: Selector '[Id MTRSubscribeParams, Ptr (), Ptr ()] ()
subscribeAttributeGroupTableWithParams_subscriptionEstablished_reportHandlerSelector = mkSelector "subscribeAttributeGroupTableWithParams:subscriptionEstablished:reportHandler:"

-- | @Selector@ for @readAttributeGroupTableWithClusterStateCache:endpoint:queue:completion:@
readAttributeGroupTableWithClusterStateCache_endpoint_queue_completionSelector :: Selector '[Id MTRClusterStateCacheContainer, Id NSNumber, Id NSObject, Ptr ()] ()
readAttributeGroupTableWithClusterStateCache_endpoint_queue_completionSelector = mkSelector "readAttributeGroupTableWithClusterStateCache:endpoint:queue:completion:"

-- | @Selector@ for @readAttributeMaxGroupsPerFabricWithCompletion:@
readAttributeMaxGroupsPerFabricWithCompletionSelector :: Selector '[Ptr ()] ()
readAttributeMaxGroupsPerFabricWithCompletionSelector = mkSelector "readAttributeMaxGroupsPerFabricWithCompletion:"

-- | @Selector@ for @subscribeAttributeMaxGroupsPerFabricWithParams:subscriptionEstablished:reportHandler:@
subscribeAttributeMaxGroupsPerFabricWithParams_subscriptionEstablished_reportHandlerSelector :: Selector '[Id MTRSubscribeParams, Ptr (), Ptr ()] ()
subscribeAttributeMaxGroupsPerFabricWithParams_subscriptionEstablished_reportHandlerSelector = mkSelector "subscribeAttributeMaxGroupsPerFabricWithParams:subscriptionEstablished:reportHandler:"

-- | @Selector@ for @readAttributeMaxGroupsPerFabricWithClusterStateCache:endpoint:queue:completion:@
readAttributeMaxGroupsPerFabricWithClusterStateCache_endpoint_queue_completionSelector :: Selector '[Id MTRClusterStateCacheContainer, Id NSNumber, Id NSObject, Ptr ()] ()
readAttributeMaxGroupsPerFabricWithClusterStateCache_endpoint_queue_completionSelector = mkSelector "readAttributeMaxGroupsPerFabricWithClusterStateCache:endpoint:queue:completion:"

-- | @Selector@ for @readAttributeMaxGroupKeysPerFabricWithCompletion:@
readAttributeMaxGroupKeysPerFabricWithCompletionSelector :: Selector '[Ptr ()] ()
readAttributeMaxGroupKeysPerFabricWithCompletionSelector = mkSelector "readAttributeMaxGroupKeysPerFabricWithCompletion:"

-- | @Selector@ for @subscribeAttributeMaxGroupKeysPerFabricWithParams:subscriptionEstablished:reportHandler:@
subscribeAttributeMaxGroupKeysPerFabricWithParams_subscriptionEstablished_reportHandlerSelector :: Selector '[Id MTRSubscribeParams, Ptr (), Ptr ()] ()
subscribeAttributeMaxGroupKeysPerFabricWithParams_subscriptionEstablished_reportHandlerSelector = mkSelector "subscribeAttributeMaxGroupKeysPerFabricWithParams:subscriptionEstablished:reportHandler:"

-- | @Selector@ for @readAttributeMaxGroupKeysPerFabricWithClusterStateCache:endpoint:queue:completion:@
readAttributeMaxGroupKeysPerFabricWithClusterStateCache_endpoint_queue_completionSelector :: Selector '[Id MTRClusterStateCacheContainer, Id NSNumber, Id NSObject, Ptr ()] ()
readAttributeMaxGroupKeysPerFabricWithClusterStateCache_endpoint_queue_completionSelector = mkSelector "readAttributeMaxGroupKeysPerFabricWithClusterStateCache:endpoint:queue:completion:"

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
initSelector :: Selector '[] (Id MTRBaseClusterGroupKeyManagement)
initSelector = mkSelector "init"

-- | @Selector@ for @new@
newSelector :: Selector '[] (Id MTRBaseClusterGroupKeyManagement)
newSelector = mkSelector "new"

-- | @Selector@ for @initWithDevice:endpoint:queue:@
initWithDevice_endpoint_queueSelector :: Selector '[Id MTRBaseDevice, CUShort, Id NSObject] (Id MTRBaseClusterGroupKeyManagement)
initWithDevice_endpoint_queueSelector = mkSelector "initWithDevice:endpoint:queue:"

-- | @Selector@ for @keySetWriteWithParams:completionHandler:@
keySetWriteWithParams_completionHandlerSelector :: Selector '[Id MTRGroupKeyManagementClusterKeySetWriteParams, Ptr ()] ()
keySetWriteWithParams_completionHandlerSelector = mkSelector "keySetWriteWithParams:completionHandler:"

-- | @Selector@ for @keySetReadWithParams:completionHandler:@
keySetReadWithParams_completionHandlerSelector :: Selector '[Id MTRGroupKeyManagementClusterKeySetReadParams, Ptr ()] ()
keySetReadWithParams_completionHandlerSelector = mkSelector "keySetReadWithParams:completionHandler:"

-- | @Selector@ for @keySetRemoveWithParams:completionHandler:@
keySetRemoveWithParams_completionHandlerSelector :: Selector '[Id MTRGroupKeyManagementClusterKeySetRemoveParams, Ptr ()] ()
keySetRemoveWithParams_completionHandlerSelector = mkSelector "keySetRemoveWithParams:completionHandler:"

-- | @Selector@ for @keySetReadAllIndicesWithParams:completionHandler:@
keySetReadAllIndicesWithParams_completionHandlerSelector :: Selector '[Id MTRGroupKeyManagementClusterKeySetReadAllIndicesParams, Ptr ()] ()
keySetReadAllIndicesWithParams_completionHandlerSelector = mkSelector "keySetReadAllIndicesWithParams:completionHandler:"

-- | @Selector@ for @readAttributeGroupKeyMapWithParams:completionHandler:@
readAttributeGroupKeyMapWithParams_completionHandlerSelector :: Selector '[Id MTRReadParams, Ptr ()] ()
readAttributeGroupKeyMapWithParams_completionHandlerSelector = mkSelector "readAttributeGroupKeyMapWithParams:completionHandler:"

-- | @Selector@ for @writeAttributeGroupKeyMapWithValue:completionHandler:@
writeAttributeGroupKeyMapWithValue_completionHandlerSelector :: Selector '[Id NSArray, Ptr ()] ()
writeAttributeGroupKeyMapWithValue_completionHandlerSelector = mkSelector "writeAttributeGroupKeyMapWithValue:completionHandler:"

-- | @Selector@ for @writeAttributeGroupKeyMapWithValue:params:completionHandler:@
writeAttributeGroupKeyMapWithValue_params_completionHandlerSelector :: Selector '[Id NSArray, Id MTRWriteParams, Ptr ()] ()
writeAttributeGroupKeyMapWithValue_params_completionHandlerSelector = mkSelector "writeAttributeGroupKeyMapWithValue:params:completionHandler:"

-- | @Selector@ for @subscribeAttributeGroupKeyMapWithMinInterval:maxInterval:params:subscriptionEstablished:reportHandler:@
subscribeAttributeGroupKeyMapWithMinInterval_maxInterval_params_subscriptionEstablished_reportHandlerSelector :: Selector '[Id NSNumber, Id NSNumber, Id MTRSubscribeParams, Ptr (), Ptr ()] ()
subscribeAttributeGroupKeyMapWithMinInterval_maxInterval_params_subscriptionEstablished_reportHandlerSelector = mkSelector "subscribeAttributeGroupKeyMapWithMinInterval:maxInterval:params:subscriptionEstablished:reportHandler:"

-- | @Selector@ for @readAttributeGroupKeyMapWithAttributeCache:endpoint:queue:completionHandler:@
readAttributeGroupKeyMapWithAttributeCache_endpoint_queue_completionHandlerSelector :: Selector '[Id MTRAttributeCacheContainer, Id NSNumber, Id NSObject, Ptr ()] ()
readAttributeGroupKeyMapWithAttributeCache_endpoint_queue_completionHandlerSelector = mkSelector "readAttributeGroupKeyMapWithAttributeCache:endpoint:queue:completionHandler:"

-- | @Selector@ for @readAttributeGroupTableWithParams:completionHandler:@
readAttributeGroupTableWithParams_completionHandlerSelector :: Selector '[Id MTRReadParams, Ptr ()] ()
readAttributeGroupTableWithParams_completionHandlerSelector = mkSelector "readAttributeGroupTableWithParams:completionHandler:"

-- | @Selector@ for @subscribeAttributeGroupTableWithMinInterval:maxInterval:params:subscriptionEstablished:reportHandler:@
subscribeAttributeGroupTableWithMinInterval_maxInterval_params_subscriptionEstablished_reportHandlerSelector :: Selector '[Id NSNumber, Id NSNumber, Id MTRSubscribeParams, Ptr (), Ptr ()] ()
subscribeAttributeGroupTableWithMinInterval_maxInterval_params_subscriptionEstablished_reportHandlerSelector = mkSelector "subscribeAttributeGroupTableWithMinInterval:maxInterval:params:subscriptionEstablished:reportHandler:"

-- | @Selector@ for @readAttributeGroupTableWithAttributeCache:endpoint:queue:completionHandler:@
readAttributeGroupTableWithAttributeCache_endpoint_queue_completionHandlerSelector :: Selector '[Id MTRAttributeCacheContainer, Id NSNumber, Id NSObject, Ptr ()] ()
readAttributeGroupTableWithAttributeCache_endpoint_queue_completionHandlerSelector = mkSelector "readAttributeGroupTableWithAttributeCache:endpoint:queue:completionHandler:"

-- | @Selector@ for @readAttributeMaxGroupsPerFabricWithCompletionHandler:@
readAttributeMaxGroupsPerFabricWithCompletionHandlerSelector :: Selector '[Ptr ()] ()
readAttributeMaxGroupsPerFabricWithCompletionHandlerSelector = mkSelector "readAttributeMaxGroupsPerFabricWithCompletionHandler:"

-- | @Selector@ for @subscribeAttributeMaxGroupsPerFabricWithMinInterval:maxInterval:params:subscriptionEstablished:reportHandler:@
subscribeAttributeMaxGroupsPerFabricWithMinInterval_maxInterval_params_subscriptionEstablished_reportHandlerSelector :: Selector '[Id NSNumber, Id NSNumber, Id MTRSubscribeParams, Ptr (), Ptr ()] ()
subscribeAttributeMaxGroupsPerFabricWithMinInterval_maxInterval_params_subscriptionEstablished_reportHandlerSelector = mkSelector "subscribeAttributeMaxGroupsPerFabricWithMinInterval:maxInterval:params:subscriptionEstablished:reportHandler:"

-- | @Selector@ for @readAttributeMaxGroupsPerFabricWithAttributeCache:endpoint:queue:completionHandler:@
readAttributeMaxGroupsPerFabricWithAttributeCache_endpoint_queue_completionHandlerSelector :: Selector '[Id MTRAttributeCacheContainer, Id NSNumber, Id NSObject, Ptr ()] ()
readAttributeMaxGroupsPerFabricWithAttributeCache_endpoint_queue_completionHandlerSelector = mkSelector "readAttributeMaxGroupsPerFabricWithAttributeCache:endpoint:queue:completionHandler:"

-- | @Selector@ for @readAttributeMaxGroupKeysPerFabricWithCompletionHandler:@
readAttributeMaxGroupKeysPerFabricWithCompletionHandlerSelector :: Selector '[Ptr ()] ()
readAttributeMaxGroupKeysPerFabricWithCompletionHandlerSelector = mkSelector "readAttributeMaxGroupKeysPerFabricWithCompletionHandler:"

-- | @Selector@ for @subscribeAttributeMaxGroupKeysPerFabricWithMinInterval:maxInterval:params:subscriptionEstablished:reportHandler:@
subscribeAttributeMaxGroupKeysPerFabricWithMinInterval_maxInterval_params_subscriptionEstablished_reportHandlerSelector :: Selector '[Id NSNumber, Id NSNumber, Id MTRSubscribeParams, Ptr (), Ptr ()] ()
subscribeAttributeMaxGroupKeysPerFabricWithMinInterval_maxInterval_params_subscriptionEstablished_reportHandlerSelector = mkSelector "subscribeAttributeMaxGroupKeysPerFabricWithMinInterval:maxInterval:params:subscriptionEstablished:reportHandler:"

-- | @Selector@ for @readAttributeMaxGroupKeysPerFabricWithAttributeCache:endpoint:queue:completionHandler:@
readAttributeMaxGroupKeysPerFabricWithAttributeCache_endpoint_queue_completionHandlerSelector :: Selector '[Id MTRAttributeCacheContainer, Id NSNumber, Id NSObject, Ptr ()] ()
readAttributeMaxGroupKeysPerFabricWithAttributeCache_endpoint_queue_completionHandlerSelector = mkSelector "readAttributeMaxGroupKeysPerFabricWithAttributeCache:endpoint:queue:completionHandler:"

-- | @Selector@ for @readAttributeGeneratedCommandListWithCompletionHandler:@
readAttributeGeneratedCommandListWithCompletionHandlerSelector :: Selector '[Ptr ()] ()
readAttributeGeneratedCommandListWithCompletionHandlerSelector = mkSelector "readAttributeGeneratedCommandListWithCompletionHandler:"

-- | @Selector@ for @subscribeAttributeGeneratedCommandListWithMinInterval:maxInterval:params:subscriptionEstablished:reportHandler:@
subscribeAttributeGeneratedCommandListWithMinInterval_maxInterval_params_subscriptionEstablished_reportHandlerSelector :: Selector '[Id NSNumber, Id NSNumber, Id MTRSubscribeParams, Ptr (), Ptr ()] ()
subscribeAttributeGeneratedCommandListWithMinInterval_maxInterval_params_subscriptionEstablished_reportHandlerSelector = mkSelector "subscribeAttributeGeneratedCommandListWithMinInterval:maxInterval:params:subscriptionEstablished:reportHandler:"

-- | @Selector@ for @readAttributeGeneratedCommandListWithAttributeCache:endpoint:queue:completionHandler:@
readAttributeGeneratedCommandListWithAttributeCache_endpoint_queue_completionHandlerSelector :: Selector '[Id MTRAttributeCacheContainer, Id NSNumber, Id NSObject, Ptr ()] ()
readAttributeGeneratedCommandListWithAttributeCache_endpoint_queue_completionHandlerSelector = mkSelector "readAttributeGeneratedCommandListWithAttributeCache:endpoint:queue:completionHandler:"

-- | @Selector@ for @readAttributeAcceptedCommandListWithCompletionHandler:@
readAttributeAcceptedCommandListWithCompletionHandlerSelector :: Selector '[Ptr ()] ()
readAttributeAcceptedCommandListWithCompletionHandlerSelector = mkSelector "readAttributeAcceptedCommandListWithCompletionHandler:"

-- | @Selector@ for @subscribeAttributeAcceptedCommandListWithMinInterval:maxInterval:params:subscriptionEstablished:reportHandler:@
subscribeAttributeAcceptedCommandListWithMinInterval_maxInterval_params_subscriptionEstablished_reportHandlerSelector :: Selector '[Id NSNumber, Id NSNumber, Id MTRSubscribeParams, Ptr (), Ptr ()] ()
subscribeAttributeAcceptedCommandListWithMinInterval_maxInterval_params_subscriptionEstablished_reportHandlerSelector = mkSelector "subscribeAttributeAcceptedCommandListWithMinInterval:maxInterval:params:subscriptionEstablished:reportHandler:"

-- | @Selector@ for @readAttributeAcceptedCommandListWithAttributeCache:endpoint:queue:completionHandler:@
readAttributeAcceptedCommandListWithAttributeCache_endpoint_queue_completionHandlerSelector :: Selector '[Id MTRAttributeCacheContainer, Id NSNumber, Id NSObject, Ptr ()] ()
readAttributeAcceptedCommandListWithAttributeCache_endpoint_queue_completionHandlerSelector = mkSelector "readAttributeAcceptedCommandListWithAttributeCache:endpoint:queue:completionHandler:"

-- | @Selector@ for @readAttributeAttributeListWithCompletionHandler:@
readAttributeAttributeListWithCompletionHandlerSelector :: Selector '[Ptr ()] ()
readAttributeAttributeListWithCompletionHandlerSelector = mkSelector "readAttributeAttributeListWithCompletionHandler:"

-- | @Selector@ for @subscribeAttributeAttributeListWithMinInterval:maxInterval:params:subscriptionEstablished:reportHandler:@
subscribeAttributeAttributeListWithMinInterval_maxInterval_params_subscriptionEstablished_reportHandlerSelector :: Selector '[Id NSNumber, Id NSNumber, Id MTRSubscribeParams, Ptr (), Ptr ()] ()
subscribeAttributeAttributeListWithMinInterval_maxInterval_params_subscriptionEstablished_reportHandlerSelector = mkSelector "subscribeAttributeAttributeListWithMinInterval:maxInterval:params:subscriptionEstablished:reportHandler:"

-- | @Selector@ for @readAttributeAttributeListWithAttributeCache:endpoint:queue:completionHandler:@
readAttributeAttributeListWithAttributeCache_endpoint_queue_completionHandlerSelector :: Selector '[Id MTRAttributeCacheContainer, Id NSNumber, Id NSObject, Ptr ()] ()
readAttributeAttributeListWithAttributeCache_endpoint_queue_completionHandlerSelector = mkSelector "readAttributeAttributeListWithAttributeCache:endpoint:queue:completionHandler:"

-- | @Selector@ for @readAttributeFeatureMapWithCompletionHandler:@
readAttributeFeatureMapWithCompletionHandlerSelector :: Selector '[Ptr ()] ()
readAttributeFeatureMapWithCompletionHandlerSelector = mkSelector "readAttributeFeatureMapWithCompletionHandler:"

-- | @Selector@ for @subscribeAttributeFeatureMapWithMinInterval:maxInterval:params:subscriptionEstablished:reportHandler:@
subscribeAttributeFeatureMapWithMinInterval_maxInterval_params_subscriptionEstablished_reportHandlerSelector :: Selector '[Id NSNumber, Id NSNumber, Id MTRSubscribeParams, Ptr (), Ptr ()] ()
subscribeAttributeFeatureMapWithMinInterval_maxInterval_params_subscriptionEstablished_reportHandlerSelector = mkSelector "subscribeAttributeFeatureMapWithMinInterval:maxInterval:params:subscriptionEstablished:reportHandler:"

-- | @Selector@ for @readAttributeFeatureMapWithAttributeCache:endpoint:queue:completionHandler:@
readAttributeFeatureMapWithAttributeCache_endpoint_queue_completionHandlerSelector :: Selector '[Id MTRAttributeCacheContainer, Id NSNumber, Id NSObject, Ptr ()] ()
readAttributeFeatureMapWithAttributeCache_endpoint_queue_completionHandlerSelector = mkSelector "readAttributeFeatureMapWithAttributeCache:endpoint:queue:completionHandler:"

-- | @Selector@ for @readAttributeClusterRevisionWithCompletionHandler:@
readAttributeClusterRevisionWithCompletionHandlerSelector :: Selector '[Ptr ()] ()
readAttributeClusterRevisionWithCompletionHandlerSelector = mkSelector "readAttributeClusterRevisionWithCompletionHandler:"

-- | @Selector@ for @subscribeAttributeClusterRevisionWithMinInterval:maxInterval:params:subscriptionEstablished:reportHandler:@
subscribeAttributeClusterRevisionWithMinInterval_maxInterval_params_subscriptionEstablished_reportHandlerSelector :: Selector '[Id NSNumber, Id NSNumber, Id MTRSubscribeParams, Ptr (), Ptr ()] ()
subscribeAttributeClusterRevisionWithMinInterval_maxInterval_params_subscriptionEstablished_reportHandlerSelector = mkSelector "subscribeAttributeClusterRevisionWithMinInterval:maxInterval:params:subscriptionEstablished:reportHandler:"

-- | @Selector@ for @readAttributeClusterRevisionWithAttributeCache:endpoint:queue:completionHandler:@
readAttributeClusterRevisionWithAttributeCache_endpoint_queue_completionHandlerSelector :: Selector '[Id MTRAttributeCacheContainer, Id NSNumber, Id NSObject, Ptr ()] ()
readAttributeClusterRevisionWithAttributeCache_endpoint_queue_completionHandlerSelector = mkSelector "readAttributeClusterRevisionWithAttributeCache:endpoint:queue:completionHandler:"

-- | @Selector@ for @initWithDevice:endpointID:queue:@
initWithDevice_endpointID_queueSelector :: Selector '[Id MTRBaseDevice, Id NSNumber, Id NSObject] (Id MTRBaseClusterGroupKeyManagement)
initWithDevice_endpointID_queueSelector = mkSelector "initWithDevice:endpointID:queue:"

