{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Cluster Scenes Management
--
-- Attributes and commands for scene configuration and manipulation.
--
-- Generated bindings for @MTRBaseClusterScenesManagement@.
module ObjC.Matter.MTRBaseClusterScenesManagement
  ( MTRBaseClusterScenesManagement
  , IsMTRBaseClusterScenesManagement(..)
  , addSceneWithParams_completion
  , viewSceneWithParams_completion
  , removeSceneWithParams_completion
  , removeAllScenesWithParams_completion
  , storeSceneWithParams_completion
  , recallSceneWithParams_completion
  , getSceneMembershipWithParams_completion
  , copySceneWithParams_completion
  , readAttributeSceneTableSizeWithCompletion
  , subscribeAttributeSceneTableSizeWithParams_subscriptionEstablished_reportHandler
  , readAttributeSceneTableSizeWithClusterStateCache_endpoint_queue_completion
  , readAttributeFabricSceneInfoWithParams_completion
  , subscribeAttributeFabricSceneInfoWithParams_subscriptionEstablished_reportHandler
  , readAttributeFabricSceneInfoWithClusterStateCache_endpoint_queue_completion
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
  , addSceneWithParams_completionSelector
  , copySceneWithParams_completionSelector
  , getSceneMembershipWithParams_completionSelector
  , initSelector
  , initWithDevice_endpointID_queueSelector
  , newSelector
  , readAttributeAcceptedCommandListWithClusterStateCache_endpoint_queue_completionSelector
  , readAttributeAcceptedCommandListWithCompletionSelector
  , readAttributeAttributeListWithClusterStateCache_endpoint_queue_completionSelector
  , readAttributeAttributeListWithCompletionSelector
  , readAttributeClusterRevisionWithClusterStateCache_endpoint_queue_completionSelector
  , readAttributeClusterRevisionWithCompletionSelector
  , readAttributeFabricSceneInfoWithClusterStateCache_endpoint_queue_completionSelector
  , readAttributeFabricSceneInfoWithParams_completionSelector
  , readAttributeFeatureMapWithClusterStateCache_endpoint_queue_completionSelector
  , readAttributeFeatureMapWithCompletionSelector
  , readAttributeGeneratedCommandListWithClusterStateCache_endpoint_queue_completionSelector
  , readAttributeGeneratedCommandListWithCompletionSelector
  , readAttributeSceneTableSizeWithClusterStateCache_endpoint_queue_completionSelector
  , readAttributeSceneTableSizeWithCompletionSelector
  , recallSceneWithParams_completionSelector
  , removeAllScenesWithParams_completionSelector
  , removeSceneWithParams_completionSelector
  , storeSceneWithParams_completionSelector
  , subscribeAttributeAcceptedCommandListWithParams_subscriptionEstablished_reportHandlerSelector
  , subscribeAttributeAttributeListWithParams_subscriptionEstablished_reportHandlerSelector
  , subscribeAttributeClusterRevisionWithParams_subscriptionEstablished_reportHandlerSelector
  , subscribeAttributeFabricSceneInfoWithParams_subscriptionEstablished_reportHandlerSelector
  , subscribeAttributeFeatureMapWithParams_subscriptionEstablished_reportHandlerSelector
  , subscribeAttributeGeneratedCommandListWithParams_subscriptionEstablished_reportHandlerSelector
  , subscribeAttributeSceneTableSizeWithParams_subscriptionEstablished_reportHandlerSelector
  , viewSceneWithParams_completionSelector


  ) where

import Foreign.Ptr (Ptr, FunPtr)
import Foreign.C.Types

import ObjC.Runtime.Types
import ObjC.Runtime.Message (sendMessage, sendOwnedMessage, sendClassMessage, sendOwnedClassMessage)
import ObjC.Runtime.Selector (mkSelector)
import ObjC.Runtime.Class (getRequiredClass)

import ObjC.Matter.Internal.Classes
import ObjC.Foundation.Internal.Classes

-- | Command AddScene
--
-- Add a scene to the scene table. Extension field sets are input as '{"ClusterID": VALUE, "AttributeValueList":[{"AttributeID": VALUE, "Value*": VALUE}]}'.
--
-- ObjC selector: @- addSceneWithParams:completion:@
addSceneWithParams_completion :: (IsMTRBaseClusterScenesManagement mtrBaseClusterScenesManagement, IsMTRScenesManagementClusterAddSceneParams params) => mtrBaseClusterScenesManagement -> params -> Ptr () -> IO ()
addSceneWithParams_completion mtrBaseClusterScenesManagement params completion =
  sendMessage mtrBaseClusterScenesManagement addSceneWithParams_completionSelector (toMTRScenesManagementClusterAddSceneParams params) completion

-- | Command ViewScene
--
-- Retrieves the requested scene entry from its Scene table.
--
-- ObjC selector: @- viewSceneWithParams:completion:@
viewSceneWithParams_completion :: (IsMTRBaseClusterScenesManagement mtrBaseClusterScenesManagement, IsMTRScenesManagementClusterViewSceneParams params) => mtrBaseClusterScenesManagement -> params -> Ptr () -> IO ()
viewSceneWithParams_completion mtrBaseClusterScenesManagement params completion =
  sendMessage mtrBaseClusterScenesManagement viewSceneWithParams_completionSelector (toMTRScenesManagementClusterViewSceneParams params) completion

-- | Command RemoveScene
--
-- Removes the requested scene entry, corresponding to the value of the GroupID field, from its Scene Table
--
-- ObjC selector: @- removeSceneWithParams:completion:@
removeSceneWithParams_completion :: (IsMTRBaseClusterScenesManagement mtrBaseClusterScenesManagement, IsMTRScenesManagementClusterRemoveSceneParams params) => mtrBaseClusterScenesManagement -> params -> Ptr () -> IO ()
removeSceneWithParams_completion mtrBaseClusterScenesManagement params completion =
  sendMessage mtrBaseClusterScenesManagement removeSceneWithParams_completionSelector (toMTRScenesManagementClusterRemoveSceneParams params) completion

-- | Command RemoveAllScenes
--
-- Remove all scenes, corresponding to the value of the GroupID field, from its Scene Table
--
-- ObjC selector: @- removeAllScenesWithParams:completion:@
removeAllScenesWithParams_completion :: (IsMTRBaseClusterScenesManagement mtrBaseClusterScenesManagement, IsMTRScenesManagementClusterRemoveAllScenesParams params) => mtrBaseClusterScenesManagement -> params -> Ptr () -> IO ()
removeAllScenesWithParams_completion mtrBaseClusterScenesManagement params completion =
  sendMessage mtrBaseClusterScenesManagement removeAllScenesWithParams_completionSelector (toMTRScenesManagementClusterRemoveAllScenesParams params) completion

-- | Command StoreScene
--
-- Adds the scene entry into its Scene Table along with all extension field sets corresponding to the current state of other clusters on the same endpoint
--
-- ObjC selector: @- storeSceneWithParams:completion:@
storeSceneWithParams_completion :: (IsMTRBaseClusterScenesManagement mtrBaseClusterScenesManagement, IsMTRScenesManagementClusterStoreSceneParams params) => mtrBaseClusterScenesManagement -> params -> Ptr () -> IO ()
storeSceneWithParams_completion mtrBaseClusterScenesManagement params completion =
  sendMessage mtrBaseClusterScenesManagement storeSceneWithParams_completionSelector (toMTRScenesManagementClusterStoreSceneParams params) completion

-- | Command RecallScene
--
-- Set the attributes and corresponding state for each other cluster implemented on the endpoint accordingly to the resquested scene entry in the Scene Table
--
-- ObjC selector: @- recallSceneWithParams:completion:@
recallSceneWithParams_completion :: (IsMTRBaseClusterScenesManagement mtrBaseClusterScenesManagement, IsMTRScenesManagementClusterRecallSceneParams params) => mtrBaseClusterScenesManagement -> params -> Ptr () -> IO ()
recallSceneWithParams_completion mtrBaseClusterScenesManagement params completion =
  sendMessage mtrBaseClusterScenesManagement recallSceneWithParams_completionSelector (toMTRScenesManagementClusterRecallSceneParams params) completion

-- | Command GetSceneMembership
--
-- This command can be used to get the used scene identifiers within a certain group, for the endpoint that implements this cluster.
--
-- ObjC selector: @- getSceneMembershipWithParams:completion:@
getSceneMembershipWithParams_completion :: (IsMTRBaseClusterScenesManagement mtrBaseClusterScenesManagement, IsMTRScenesManagementClusterGetSceneMembershipParams params) => mtrBaseClusterScenesManagement -> params -> Ptr () -> IO ()
getSceneMembershipWithParams_completion mtrBaseClusterScenesManagement params completion =
  sendMessage mtrBaseClusterScenesManagement getSceneMembershipWithParams_completionSelector (toMTRScenesManagementClusterGetSceneMembershipParams params) completion

-- | Command CopyScene
--
-- This command allows a client to efficiently copy scenes from one group/scene identifier pair to another group/scene identifier pair.
--
-- ObjC selector: @- copySceneWithParams:completion:@
copySceneWithParams_completion :: (IsMTRBaseClusterScenesManagement mtrBaseClusterScenesManagement, IsMTRScenesManagementClusterCopySceneParams params) => mtrBaseClusterScenesManagement -> params -> Ptr () -> IO ()
copySceneWithParams_completion mtrBaseClusterScenesManagement params completion =
  sendOwnedMessage mtrBaseClusterScenesManagement copySceneWithParams_completionSelector (toMTRScenesManagementClusterCopySceneParams params) completion

-- | @- readAttributeSceneTableSizeWithCompletion:@
readAttributeSceneTableSizeWithCompletion :: IsMTRBaseClusterScenesManagement mtrBaseClusterScenesManagement => mtrBaseClusterScenesManagement -> Ptr () -> IO ()
readAttributeSceneTableSizeWithCompletion mtrBaseClusterScenesManagement completion =
  sendMessage mtrBaseClusterScenesManagement readAttributeSceneTableSizeWithCompletionSelector completion

-- | @- subscribeAttributeSceneTableSizeWithParams:subscriptionEstablished:reportHandler:@
subscribeAttributeSceneTableSizeWithParams_subscriptionEstablished_reportHandler :: (IsMTRBaseClusterScenesManagement mtrBaseClusterScenesManagement, IsMTRSubscribeParams params) => mtrBaseClusterScenesManagement -> params -> Ptr () -> Ptr () -> IO ()
subscribeAttributeSceneTableSizeWithParams_subscriptionEstablished_reportHandler mtrBaseClusterScenesManagement params subscriptionEstablished reportHandler =
  sendMessage mtrBaseClusterScenesManagement subscribeAttributeSceneTableSizeWithParams_subscriptionEstablished_reportHandlerSelector (toMTRSubscribeParams params) subscriptionEstablished reportHandler

-- | @+ readAttributeSceneTableSizeWithClusterStateCache:endpoint:queue:completion:@
readAttributeSceneTableSizeWithClusterStateCache_endpoint_queue_completion :: (IsMTRClusterStateCacheContainer clusterStateCacheContainer, IsNSNumber endpoint, IsNSObject queue) => clusterStateCacheContainer -> endpoint -> queue -> Ptr () -> IO ()
readAttributeSceneTableSizeWithClusterStateCache_endpoint_queue_completion clusterStateCacheContainer endpoint queue completion =
  do
    cls' <- getRequiredClass "MTRBaseClusterScenesManagement"
    sendClassMessage cls' readAttributeSceneTableSizeWithClusterStateCache_endpoint_queue_completionSelector (toMTRClusterStateCacheContainer clusterStateCacheContainer) (toNSNumber endpoint) (toNSObject queue) completion

-- | @- readAttributeFabricSceneInfoWithParams:completion:@
readAttributeFabricSceneInfoWithParams_completion :: (IsMTRBaseClusterScenesManagement mtrBaseClusterScenesManagement, IsMTRReadParams params) => mtrBaseClusterScenesManagement -> params -> Ptr () -> IO ()
readAttributeFabricSceneInfoWithParams_completion mtrBaseClusterScenesManagement params completion =
  sendMessage mtrBaseClusterScenesManagement readAttributeFabricSceneInfoWithParams_completionSelector (toMTRReadParams params) completion

-- | @- subscribeAttributeFabricSceneInfoWithParams:subscriptionEstablished:reportHandler:@
subscribeAttributeFabricSceneInfoWithParams_subscriptionEstablished_reportHandler :: (IsMTRBaseClusterScenesManagement mtrBaseClusterScenesManagement, IsMTRSubscribeParams params) => mtrBaseClusterScenesManagement -> params -> Ptr () -> Ptr () -> IO ()
subscribeAttributeFabricSceneInfoWithParams_subscriptionEstablished_reportHandler mtrBaseClusterScenesManagement params subscriptionEstablished reportHandler =
  sendMessage mtrBaseClusterScenesManagement subscribeAttributeFabricSceneInfoWithParams_subscriptionEstablished_reportHandlerSelector (toMTRSubscribeParams params) subscriptionEstablished reportHandler

-- | @+ readAttributeFabricSceneInfoWithClusterStateCache:endpoint:queue:completion:@
readAttributeFabricSceneInfoWithClusterStateCache_endpoint_queue_completion :: (IsMTRClusterStateCacheContainer clusterStateCacheContainer, IsNSNumber endpoint, IsNSObject queue) => clusterStateCacheContainer -> endpoint -> queue -> Ptr () -> IO ()
readAttributeFabricSceneInfoWithClusterStateCache_endpoint_queue_completion clusterStateCacheContainer endpoint queue completion =
  do
    cls' <- getRequiredClass "MTRBaseClusterScenesManagement"
    sendClassMessage cls' readAttributeFabricSceneInfoWithClusterStateCache_endpoint_queue_completionSelector (toMTRClusterStateCacheContainer clusterStateCacheContainer) (toNSNumber endpoint) (toNSObject queue) completion

-- | @- readAttributeGeneratedCommandListWithCompletion:@
readAttributeGeneratedCommandListWithCompletion :: IsMTRBaseClusterScenesManagement mtrBaseClusterScenesManagement => mtrBaseClusterScenesManagement -> Ptr () -> IO ()
readAttributeGeneratedCommandListWithCompletion mtrBaseClusterScenesManagement completion =
  sendMessage mtrBaseClusterScenesManagement readAttributeGeneratedCommandListWithCompletionSelector completion

-- | @- subscribeAttributeGeneratedCommandListWithParams:subscriptionEstablished:reportHandler:@
subscribeAttributeGeneratedCommandListWithParams_subscriptionEstablished_reportHandler :: (IsMTRBaseClusterScenesManagement mtrBaseClusterScenesManagement, IsMTRSubscribeParams params) => mtrBaseClusterScenesManagement -> params -> Ptr () -> Ptr () -> IO ()
subscribeAttributeGeneratedCommandListWithParams_subscriptionEstablished_reportHandler mtrBaseClusterScenesManagement params subscriptionEstablished reportHandler =
  sendMessage mtrBaseClusterScenesManagement subscribeAttributeGeneratedCommandListWithParams_subscriptionEstablished_reportHandlerSelector (toMTRSubscribeParams params) subscriptionEstablished reportHandler

-- | @+ readAttributeGeneratedCommandListWithClusterStateCache:endpoint:queue:completion:@
readAttributeGeneratedCommandListWithClusterStateCache_endpoint_queue_completion :: (IsMTRClusterStateCacheContainer clusterStateCacheContainer, IsNSNumber endpoint, IsNSObject queue) => clusterStateCacheContainer -> endpoint -> queue -> Ptr () -> IO ()
readAttributeGeneratedCommandListWithClusterStateCache_endpoint_queue_completion clusterStateCacheContainer endpoint queue completion =
  do
    cls' <- getRequiredClass "MTRBaseClusterScenesManagement"
    sendClassMessage cls' readAttributeGeneratedCommandListWithClusterStateCache_endpoint_queue_completionSelector (toMTRClusterStateCacheContainer clusterStateCacheContainer) (toNSNumber endpoint) (toNSObject queue) completion

-- | @- readAttributeAcceptedCommandListWithCompletion:@
readAttributeAcceptedCommandListWithCompletion :: IsMTRBaseClusterScenesManagement mtrBaseClusterScenesManagement => mtrBaseClusterScenesManagement -> Ptr () -> IO ()
readAttributeAcceptedCommandListWithCompletion mtrBaseClusterScenesManagement completion =
  sendMessage mtrBaseClusterScenesManagement readAttributeAcceptedCommandListWithCompletionSelector completion

-- | @- subscribeAttributeAcceptedCommandListWithParams:subscriptionEstablished:reportHandler:@
subscribeAttributeAcceptedCommandListWithParams_subscriptionEstablished_reportHandler :: (IsMTRBaseClusterScenesManagement mtrBaseClusterScenesManagement, IsMTRSubscribeParams params) => mtrBaseClusterScenesManagement -> params -> Ptr () -> Ptr () -> IO ()
subscribeAttributeAcceptedCommandListWithParams_subscriptionEstablished_reportHandler mtrBaseClusterScenesManagement params subscriptionEstablished reportHandler =
  sendMessage mtrBaseClusterScenesManagement subscribeAttributeAcceptedCommandListWithParams_subscriptionEstablished_reportHandlerSelector (toMTRSubscribeParams params) subscriptionEstablished reportHandler

-- | @+ readAttributeAcceptedCommandListWithClusterStateCache:endpoint:queue:completion:@
readAttributeAcceptedCommandListWithClusterStateCache_endpoint_queue_completion :: (IsMTRClusterStateCacheContainer clusterStateCacheContainer, IsNSNumber endpoint, IsNSObject queue) => clusterStateCacheContainer -> endpoint -> queue -> Ptr () -> IO ()
readAttributeAcceptedCommandListWithClusterStateCache_endpoint_queue_completion clusterStateCacheContainer endpoint queue completion =
  do
    cls' <- getRequiredClass "MTRBaseClusterScenesManagement"
    sendClassMessage cls' readAttributeAcceptedCommandListWithClusterStateCache_endpoint_queue_completionSelector (toMTRClusterStateCacheContainer clusterStateCacheContainer) (toNSNumber endpoint) (toNSObject queue) completion

-- | @- readAttributeAttributeListWithCompletion:@
readAttributeAttributeListWithCompletion :: IsMTRBaseClusterScenesManagement mtrBaseClusterScenesManagement => mtrBaseClusterScenesManagement -> Ptr () -> IO ()
readAttributeAttributeListWithCompletion mtrBaseClusterScenesManagement completion =
  sendMessage mtrBaseClusterScenesManagement readAttributeAttributeListWithCompletionSelector completion

-- | @- subscribeAttributeAttributeListWithParams:subscriptionEstablished:reportHandler:@
subscribeAttributeAttributeListWithParams_subscriptionEstablished_reportHandler :: (IsMTRBaseClusterScenesManagement mtrBaseClusterScenesManagement, IsMTRSubscribeParams params) => mtrBaseClusterScenesManagement -> params -> Ptr () -> Ptr () -> IO ()
subscribeAttributeAttributeListWithParams_subscriptionEstablished_reportHandler mtrBaseClusterScenesManagement params subscriptionEstablished reportHandler =
  sendMessage mtrBaseClusterScenesManagement subscribeAttributeAttributeListWithParams_subscriptionEstablished_reportHandlerSelector (toMTRSubscribeParams params) subscriptionEstablished reportHandler

-- | @+ readAttributeAttributeListWithClusterStateCache:endpoint:queue:completion:@
readAttributeAttributeListWithClusterStateCache_endpoint_queue_completion :: (IsMTRClusterStateCacheContainer clusterStateCacheContainer, IsNSNumber endpoint, IsNSObject queue) => clusterStateCacheContainer -> endpoint -> queue -> Ptr () -> IO ()
readAttributeAttributeListWithClusterStateCache_endpoint_queue_completion clusterStateCacheContainer endpoint queue completion =
  do
    cls' <- getRequiredClass "MTRBaseClusterScenesManagement"
    sendClassMessage cls' readAttributeAttributeListWithClusterStateCache_endpoint_queue_completionSelector (toMTRClusterStateCacheContainer clusterStateCacheContainer) (toNSNumber endpoint) (toNSObject queue) completion

-- | @- readAttributeFeatureMapWithCompletion:@
readAttributeFeatureMapWithCompletion :: IsMTRBaseClusterScenesManagement mtrBaseClusterScenesManagement => mtrBaseClusterScenesManagement -> Ptr () -> IO ()
readAttributeFeatureMapWithCompletion mtrBaseClusterScenesManagement completion =
  sendMessage mtrBaseClusterScenesManagement readAttributeFeatureMapWithCompletionSelector completion

-- | @- subscribeAttributeFeatureMapWithParams:subscriptionEstablished:reportHandler:@
subscribeAttributeFeatureMapWithParams_subscriptionEstablished_reportHandler :: (IsMTRBaseClusterScenesManagement mtrBaseClusterScenesManagement, IsMTRSubscribeParams params) => mtrBaseClusterScenesManagement -> params -> Ptr () -> Ptr () -> IO ()
subscribeAttributeFeatureMapWithParams_subscriptionEstablished_reportHandler mtrBaseClusterScenesManagement params subscriptionEstablished reportHandler =
  sendMessage mtrBaseClusterScenesManagement subscribeAttributeFeatureMapWithParams_subscriptionEstablished_reportHandlerSelector (toMTRSubscribeParams params) subscriptionEstablished reportHandler

-- | @+ readAttributeFeatureMapWithClusterStateCache:endpoint:queue:completion:@
readAttributeFeatureMapWithClusterStateCache_endpoint_queue_completion :: (IsMTRClusterStateCacheContainer clusterStateCacheContainer, IsNSNumber endpoint, IsNSObject queue) => clusterStateCacheContainer -> endpoint -> queue -> Ptr () -> IO ()
readAttributeFeatureMapWithClusterStateCache_endpoint_queue_completion clusterStateCacheContainer endpoint queue completion =
  do
    cls' <- getRequiredClass "MTRBaseClusterScenesManagement"
    sendClassMessage cls' readAttributeFeatureMapWithClusterStateCache_endpoint_queue_completionSelector (toMTRClusterStateCacheContainer clusterStateCacheContainer) (toNSNumber endpoint) (toNSObject queue) completion

-- | @- readAttributeClusterRevisionWithCompletion:@
readAttributeClusterRevisionWithCompletion :: IsMTRBaseClusterScenesManagement mtrBaseClusterScenesManagement => mtrBaseClusterScenesManagement -> Ptr () -> IO ()
readAttributeClusterRevisionWithCompletion mtrBaseClusterScenesManagement completion =
  sendMessage mtrBaseClusterScenesManagement readAttributeClusterRevisionWithCompletionSelector completion

-- | @- subscribeAttributeClusterRevisionWithParams:subscriptionEstablished:reportHandler:@
subscribeAttributeClusterRevisionWithParams_subscriptionEstablished_reportHandler :: (IsMTRBaseClusterScenesManagement mtrBaseClusterScenesManagement, IsMTRSubscribeParams params) => mtrBaseClusterScenesManagement -> params -> Ptr () -> Ptr () -> IO ()
subscribeAttributeClusterRevisionWithParams_subscriptionEstablished_reportHandler mtrBaseClusterScenesManagement params subscriptionEstablished reportHandler =
  sendMessage mtrBaseClusterScenesManagement subscribeAttributeClusterRevisionWithParams_subscriptionEstablished_reportHandlerSelector (toMTRSubscribeParams params) subscriptionEstablished reportHandler

-- | @+ readAttributeClusterRevisionWithClusterStateCache:endpoint:queue:completion:@
readAttributeClusterRevisionWithClusterStateCache_endpoint_queue_completion :: (IsMTRClusterStateCacheContainer clusterStateCacheContainer, IsNSNumber endpoint, IsNSObject queue) => clusterStateCacheContainer -> endpoint -> queue -> Ptr () -> IO ()
readAttributeClusterRevisionWithClusterStateCache_endpoint_queue_completion clusterStateCacheContainer endpoint queue completion =
  do
    cls' <- getRequiredClass "MTRBaseClusterScenesManagement"
    sendClassMessage cls' readAttributeClusterRevisionWithClusterStateCache_endpoint_queue_completionSelector (toMTRClusterStateCacheContainer clusterStateCacheContainer) (toNSNumber endpoint) (toNSObject queue) completion

-- | @- init@
init_ :: IsMTRBaseClusterScenesManagement mtrBaseClusterScenesManagement => mtrBaseClusterScenesManagement -> IO (Id MTRBaseClusterScenesManagement)
init_ mtrBaseClusterScenesManagement =
  sendOwnedMessage mtrBaseClusterScenesManagement initSelector

-- | @+ new@
new :: IO (Id MTRBaseClusterScenesManagement)
new  =
  do
    cls' <- getRequiredClass "MTRBaseClusterScenesManagement"
    sendOwnedClassMessage cls' newSelector

-- | For all instance methods (reads, writes, commands) that take a completion, the completion will be called on the provided queue.
--
-- ObjC selector: @- initWithDevice:endpointID:queue:@
initWithDevice_endpointID_queue :: (IsMTRBaseClusterScenesManagement mtrBaseClusterScenesManagement, IsMTRBaseDevice device, IsNSNumber endpointID, IsNSObject queue) => mtrBaseClusterScenesManagement -> device -> endpointID -> queue -> IO (Id MTRBaseClusterScenesManagement)
initWithDevice_endpointID_queue mtrBaseClusterScenesManagement device endpointID queue =
  sendOwnedMessage mtrBaseClusterScenesManagement initWithDevice_endpointID_queueSelector (toMTRBaseDevice device) (toNSNumber endpointID) (toNSObject queue)

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @addSceneWithParams:completion:@
addSceneWithParams_completionSelector :: Selector '[Id MTRScenesManagementClusterAddSceneParams, Ptr ()] ()
addSceneWithParams_completionSelector = mkSelector "addSceneWithParams:completion:"

-- | @Selector@ for @viewSceneWithParams:completion:@
viewSceneWithParams_completionSelector :: Selector '[Id MTRScenesManagementClusterViewSceneParams, Ptr ()] ()
viewSceneWithParams_completionSelector = mkSelector "viewSceneWithParams:completion:"

-- | @Selector@ for @removeSceneWithParams:completion:@
removeSceneWithParams_completionSelector :: Selector '[Id MTRScenesManagementClusterRemoveSceneParams, Ptr ()] ()
removeSceneWithParams_completionSelector = mkSelector "removeSceneWithParams:completion:"

-- | @Selector@ for @removeAllScenesWithParams:completion:@
removeAllScenesWithParams_completionSelector :: Selector '[Id MTRScenesManagementClusterRemoveAllScenesParams, Ptr ()] ()
removeAllScenesWithParams_completionSelector = mkSelector "removeAllScenesWithParams:completion:"

-- | @Selector@ for @storeSceneWithParams:completion:@
storeSceneWithParams_completionSelector :: Selector '[Id MTRScenesManagementClusterStoreSceneParams, Ptr ()] ()
storeSceneWithParams_completionSelector = mkSelector "storeSceneWithParams:completion:"

-- | @Selector@ for @recallSceneWithParams:completion:@
recallSceneWithParams_completionSelector :: Selector '[Id MTRScenesManagementClusterRecallSceneParams, Ptr ()] ()
recallSceneWithParams_completionSelector = mkSelector "recallSceneWithParams:completion:"

-- | @Selector@ for @getSceneMembershipWithParams:completion:@
getSceneMembershipWithParams_completionSelector :: Selector '[Id MTRScenesManagementClusterGetSceneMembershipParams, Ptr ()] ()
getSceneMembershipWithParams_completionSelector = mkSelector "getSceneMembershipWithParams:completion:"

-- | @Selector@ for @copySceneWithParams:completion:@
copySceneWithParams_completionSelector :: Selector '[Id MTRScenesManagementClusterCopySceneParams, Ptr ()] ()
copySceneWithParams_completionSelector = mkSelector "copySceneWithParams:completion:"

-- | @Selector@ for @readAttributeSceneTableSizeWithCompletion:@
readAttributeSceneTableSizeWithCompletionSelector :: Selector '[Ptr ()] ()
readAttributeSceneTableSizeWithCompletionSelector = mkSelector "readAttributeSceneTableSizeWithCompletion:"

-- | @Selector@ for @subscribeAttributeSceneTableSizeWithParams:subscriptionEstablished:reportHandler:@
subscribeAttributeSceneTableSizeWithParams_subscriptionEstablished_reportHandlerSelector :: Selector '[Id MTRSubscribeParams, Ptr (), Ptr ()] ()
subscribeAttributeSceneTableSizeWithParams_subscriptionEstablished_reportHandlerSelector = mkSelector "subscribeAttributeSceneTableSizeWithParams:subscriptionEstablished:reportHandler:"

-- | @Selector@ for @readAttributeSceneTableSizeWithClusterStateCache:endpoint:queue:completion:@
readAttributeSceneTableSizeWithClusterStateCache_endpoint_queue_completionSelector :: Selector '[Id MTRClusterStateCacheContainer, Id NSNumber, Id NSObject, Ptr ()] ()
readAttributeSceneTableSizeWithClusterStateCache_endpoint_queue_completionSelector = mkSelector "readAttributeSceneTableSizeWithClusterStateCache:endpoint:queue:completion:"

-- | @Selector@ for @readAttributeFabricSceneInfoWithParams:completion:@
readAttributeFabricSceneInfoWithParams_completionSelector :: Selector '[Id MTRReadParams, Ptr ()] ()
readAttributeFabricSceneInfoWithParams_completionSelector = mkSelector "readAttributeFabricSceneInfoWithParams:completion:"

-- | @Selector@ for @subscribeAttributeFabricSceneInfoWithParams:subscriptionEstablished:reportHandler:@
subscribeAttributeFabricSceneInfoWithParams_subscriptionEstablished_reportHandlerSelector :: Selector '[Id MTRSubscribeParams, Ptr (), Ptr ()] ()
subscribeAttributeFabricSceneInfoWithParams_subscriptionEstablished_reportHandlerSelector = mkSelector "subscribeAttributeFabricSceneInfoWithParams:subscriptionEstablished:reportHandler:"

-- | @Selector@ for @readAttributeFabricSceneInfoWithClusterStateCache:endpoint:queue:completion:@
readAttributeFabricSceneInfoWithClusterStateCache_endpoint_queue_completionSelector :: Selector '[Id MTRClusterStateCacheContainer, Id NSNumber, Id NSObject, Ptr ()] ()
readAttributeFabricSceneInfoWithClusterStateCache_endpoint_queue_completionSelector = mkSelector "readAttributeFabricSceneInfoWithClusterStateCache:endpoint:queue:completion:"

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
initSelector :: Selector '[] (Id MTRBaseClusterScenesManagement)
initSelector = mkSelector "init"

-- | @Selector@ for @new@
newSelector :: Selector '[] (Id MTRBaseClusterScenesManagement)
newSelector = mkSelector "new"

-- | @Selector@ for @initWithDevice:endpointID:queue:@
initWithDevice_endpointID_queueSelector :: Selector '[Id MTRBaseDevice, Id NSNumber, Id NSObject] (Id MTRBaseClusterScenesManagement)
initWithDevice_endpointID_queueSelector = mkSelector "initWithDevice:endpointID:queue:"

