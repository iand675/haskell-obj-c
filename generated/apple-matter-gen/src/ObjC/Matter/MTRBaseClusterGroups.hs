{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Cluster Groups
--
-- Attributes and commands for group configuration and manipulation.
--
-- Generated bindings for @MTRBaseClusterGroups@.
module ObjC.Matter.MTRBaseClusterGroups
  ( MTRBaseClusterGroups
  , IsMTRBaseClusterGroups(..)
  , addGroupWithParams_completion
  , viewGroupWithParams_completion
  , getGroupMembershipWithParams_completion
  , removeGroupWithParams_completion
  , removeAllGroupsWithParams_completion
  , removeAllGroupsWithCompletion
  , addGroupIfIdentifyingWithParams_completion
  , readAttributeNameSupportWithCompletion
  , subscribeAttributeNameSupportWithParams_subscriptionEstablished_reportHandler
  , readAttributeNameSupportWithClusterStateCache_endpoint_queue_completion
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
  , addGroupWithParams_completionHandler
  , viewGroupWithParams_completionHandler
  , getGroupMembershipWithParams_completionHandler
  , removeGroupWithParams_completionHandler
  , removeAllGroupsWithParams_completionHandler
  , removeAllGroupsWithCompletionHandler
  , addGroupIfIdentifyingWithParams_completionHandler
  , readAttributeNameSupportWithCompletionHandler
  , subscribeAttributeNameSupportWithMinInterval_maxInterval_params_subscriptionEstablished_reportHandler
  , readAttributeNameSupportWithAttributeCache_endpoint_queue_completionHandler
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
  , addGroupIfIdentifyingWithParams_completionHandlerSelector
  , addGroupIfIdentifyingWithParams_completionSelector
  , addGroupWithParams_completionHandlerSelector
  , addGroupWithParams_completionSelector
  , getGroupMembershipWithParams_completionHandlerSelector
  , getGroupMembershipWithParams_completionSelector
  , initSelector
  , initWithDevice_endpointID_queueSelector
  , initWithDevice_endpoint_queueSelector
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
  , readAttributeNameSupportWithAttributeCache_endpoint_queue_completionHandlerSelector
  , readAttributeNameSupportWithClusterStateCache_endpoint_queue_completionSelector
  , readAttributeNameSupportWithCompletionHandlerSelector
  , readAttributeNameSupportWithCompletionSelector
  , removeAllGroupsWithCompletionHandlerSelector
  , removeAllGroupsWithCompletionSelector
  , removeAllGroupsWithParams_completionHandlerSelector
  , removeAllGroupsWithParams_completionSelector
  , removeGroupWithParams_completionHandlerSelector
  , removeGroupWithParams_completionSelector
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
  , subscribeAttributeNameSupportWithMinInterval_maxInterval_params_subscriptionEstablished_reportHandlerSelector
  , subscribeAttributeNameSupportWithParams_subscriptionEstablished_reportHandlerSelector
  , viewGroupWithParams_completionHandlerSelector
  , viewGroupWithParams_completionSelector


  ) where

import Foreign.Ptr (Ptr, FunPtr)
import Foreign.C.Types

import ObjC.Runtime.Types
import ObjC.Runtime.Message (sendMessage, sendOwnedMessage, sendClassMessage, sendOwnedClassMessage)
import ObjC.Runtime.Selector (mkSelector)
import ObjC.Runtime.Class (getRequiredClass)

import ObjC.Matter.Internal.Classes
import ObjC.Foundation.Internal.Classes

-- | Command AddGroup
--
-- The AddGroup command allows a client to add group membership in a particular group for the server endpoint.
--
-- ObjC selector: @- addGroupWithParams:completion:@
addGroupWithParams_completion :: (IsMTRBaseClusterGroups mtrBaseClusterGroups, IsMTRGroupsClusterAddGroupParams params) => mtrBaseClusterGroups -> params -> Ptr () -> IO ()
addGroupWithParams_completion mtrBaseClusterGroups params completion =
  sendMessage mtrBaseClusterGroups addGroupWithParams_completionSelector (toMTRGroupsClusterAddGroupParams params) completion

-- | Command ViewGroup
--
-- The ViewGroup command allows a client to request that the server responds with a ViewGroupResponse command containing the name string for a particular group.
--
-- ObjC selector: @- viewGroupWithParams:completion:@
viewGroupWithParams_completion :: (IsMTRBaseClusterGroups mtrBaseClusterGroups, IsMTRGroupsClusterViewGroupParams params) => mtrBaseClusterGroups -> params -> Ptr () -> IO ()
viewGroupWithParams_completion mtrBaseClusterGroups params completion =
  sendMessage mtrBaseClusterGroups viewGroupWithParams_completionSelector (toMTRGroupsClusterViewGroupParams params) completion

-- | Command GetGroupMembership
--
-- The GetGroupMembership command allows a client to inquire about the group membership of the server endpoint, in a number of ways.
--
-- ObjC selector: @- getGroupMembershipWithParams:completion:@
getGroupMembershipWithParams_completion :: (IsMTRBaseClusterGroups mtrBaseClusterGroups, IsMTRGroupsClusterGetGroupMembershipParams params) => mtrBaseClusterGroups -> params -> Ptr () -> IO ()
getGroupMembershipWithParams_completion mtrBaseClusterGroups params completion =
  sendMessage mtrBaseClusterGroups getGroupMembershipWithParams_completionSelector (toMTRGroupsClusterGetGroupMembershipParams params) completion

-- | Command RemoveGroup
--
-- The RemoveGroup command allows a client to request that the server removes the membership for the server endpoint, if any, in a particular group.
--
-- ObjC selector: @- removeGroupWithParams:completion:@
removeGroupWithParams_completion :: (IsMTRBaseClusterGroups mtrBaseClusterGroups, IsMTRGroupsClusterRemoveGroupParams params) => mtrBaseClusterGroups -> params -> Ptr () -> IO ()
removeGroupWithParams_completion mtrBaseClusterGroups params completion =
  sendMessage mtrBaseClusterGroups removeGroupWithParams_completionSelector (toMTRGroupsClusterRemoveGroupParams params) completion

-- | Command RemoveAllGroups
--
-- The RemoveAllGroups command allows a client to direct the server to remove all group associations for the server endpoint.
--
-- ObjC selector: @- removeAllGroupsWithParams:completion:@
removeAllGroupsWithParams_completion :: (IsMTRBaseClusterGroups mtrBaseClusterGroups, IsMTRGroupsClusterRemoveAllGroupsParams params) => mtrBaseClusterGroups -> params -> Ptr () -> IO ()
removeAllGroupsWithParams_completion mtrBaseClusterGroups params completion =
  sendMessage mtrBaseClusterGroups removeAllGroupsWithParams_completionSelector (toMTRGroupsClusterRemoveAllGroupsParams params) completion

-- | @- removeAllGroupsWithCompletion:@
removeAllGroupsWithCompletion :: IsMTRBaseClusterGroups mtrBaseClusterGroups => mtrBaseClusterGroups -> Ptr () -> IO ()
removeAllGroupsWithCompletion mtrBaseClusterGroups completion =
  sendMessage mtrBaseClusterGroups removeAllGroupsWithCompletionSelector completion

-- | Command AddGroupIfIdentifying
--
-- The AddGroupIfIdentifying command allows a client to add group membership in a particular group for the server endpoint, on condition that the endpoint is identifying itself.
--
-- ObjC selector: @- addGroupIfIdentifyingWithParams:completion:@
addGroupIfIdentifyingWithParams_completion :: (IsMTRBaseClusterGroups mtrBaseClusterGroups, IsMTRGroupsClusterAddGroupIfIdentifyingParams params) => mtrBaseClusterGroups -> params -> Ptr () -> IO ()
addGroupIfIdentifyingWithParams_completion mtrBaseClusterGroups params completion =
  sendMessage mtrBaseClusterGroups addGroupIfIdentifyingWithParams_completionSelector (toMTRGroupsClusterAddGroupIfIdentifyingParams params) completion

-- | @- readAttributeNameSupportWithCompletion:@
readAttributeNameSupportWithCompletion :: IsMTRBaseClusterGroups mtrBaseClusterGroups => mtrBaseClusterGroups -> Ptr () -> IO ()
readAttributeNameSupportWithCompletion mtrBaseClusterGroups completion =
  sendMessage mtrBaseClusterGroups readAttributeNameSupportWithCompletionSelector completion

-- | @- subscribeAttributeNameSupportWithParams:subscriptionEstablished:reportHandler:@
subscribeAttributeNameSupportWithParams_subscriptionEstablished_reportHandler :: (IsMTRBaseClusterGroups mtrBaseClusterGroups, IsMTRSubscribeParams params) => mtrBaseClusterGroups -> params -> Ptr () -> Ptr () -> IO ()
subscribeAttributeNameSupportWithParams_subscriptionEstablished_reportHandler mtrBaseClusterGroups params subscriptionEstablished reportHandler =
  sendMessage mtrBaseClusterGroups subscribeAttributeNameSupportWithParams_subscriptionEstablished_reportHandlerSelector (toMTRSubscribeParams params) subscriptionEstablished reportHandler

-- | @+ readAttributeNameSupportWithClusterStateCache:endpoint:queue:completion:@
readAttributeNameSupportWithClusterStateCache_endpoint_queue_completion :: (IsMTRClusterStateCacheContainer clusterStateCacheContainer, IsNSNumber endpoint, IsNSObject queue) => clusterStateCacheContainer -> endpoint -> queue -> Ptr () -> IO ()
readAttributeNameSupportWithClusterStateCache_endpoint_queue_completion clusterStateCacheContainer endpoint queue completion =
  do
    cls' <- getRequiredClass "MTRBaseClusterGroups"
    sendClassMessage cls' readAttributeNameSupportWithClusterStateCache_endpoint_queue_completionSelector (toMTRClusterStateCacheContainer clusterStateCacheContainer) (toNSNumber endpoint) (toNSObject queue) completion

-- | @- readAttributeGeneratedCommandListWithCompletion:@
readAttributeGeneratedCommandListWithCompletion :: IsMTRBaseClusterGroups mtrBaseClusterGroups => mtrBaseClusterGroups -> Ptr () -> IO ()
readAttributeGeneratedCommandListWithCompletion mtrBaseClusterGroups completion =
  sendMessage mtrBaseClusterGroups readAttributeGeneratedCommandListWithCompletionSelector completion

-- | @- subscribeAttributeGeneratedCommandListWithParams:subscriptionEstablished:reportHandler:@
subscribeAttributeGeneratedCommandListWithParams_subscriptionEstablished_reportHandler :: (IsMTRBaseClusterGroups mtrBaseClusterGroups, IsMTRSubscribeParams params) => mtrBaseClusterGroups -> params -> Ptr () -> Ptr () -> IO ()
subscribeAttributeGeneratedCommandListWithParams_subscriptionEstablished_reportHandler mtrBaseClusterGroups params subscriptionEstablished reportHandler =
  sendMessage mtrBaseClusterGroups subscribeAttributeGeneratedCommandListWithParams_subscriptionEstablished_reportHandlerSelector (toMTRSubscribeParams params) subscriptionEstablished reportHandler

-- | @+ readAttributeGeneratedCommandListWithClusterStateCache:endpoint:queue:completion:@
readAttributeGeneratedCommandListWithClusterStateCache_endpoint_queue_completion :: (IsMTRClusterStateCacheContainer clusterStateCacheContainer, IsNSNumber endpoint, IsNSObject queue) => clusterStateCacheContainer -> endpoint -> queue -> Ptr () -> IO ()
readAttributeGeneratedCommandListWithClusterStateCache_endpoint_queue_completion clusterStateCacheContainer endpoint queue completion =
  do
    cls' <- getRequiredClass "MTRBaseClusterGroups"
    sendClassMessage cls' readAttributeGeneratedCommandListWithClusterStateCache_endpoint_queue_completionSelector (toMTRClusterStateCacheContainer clusterStateCacheContainer) (toNSNumber endpoint) (toNSObject queue) completion

-- | @- readAttributeAcceptedCommandListWithCompletion:@
readAttributeAcceptedCommandListWithCompletion :: IsMTRBaseClusterGroups mtrBaseClusterGroups => mtrBaseClusterGroups -> Ptr () -> IO ()
readAttributeAcceptedCommandListWithCompletion mtrBaseClusterGroups completion =
  sendMessage mtrBaseClusterGroups readAttributeAcceptedCommandListWithCompletionSelector completion

-- | @- subscribeAttributeAcceptedCommandListWithParams:subscriptionEstablished:reportHandler:@
subscribeAttributeAcceptedCommandListWithParams_subscriptionEstablished_reportHandler :: (IsMTRBaseClusterGroups mtrBaseClusterGroups, IsMTRSubscribeParams params) => mtrBaseClusterGroups -> params -> Ptr () -> Ptr () -> IO ()
subscribeAttributeAcceptedCommandListWithParams_subscriptionEstablished_reportHandler mtrBaseClusterGroups params subscriptionEstablished reportHandler =
  sendMessage mtrBaseClusterGroups subscribeAttributeAcceptedCommandListWithParams_subscriptionEstablished_reportHandlerSelector (toMTRSubscribeParams params) subscriptionEstablished reportHandler

-- | @+ readAttributeAcceptedCommandListWithClusterStateCache:endpoint:queue:completion:@
readAttributeAcceptedCommandListWithClusterStateCache_endpoint_queue_completion :: (IsMTRClusterStateCacheContainer clusterStateCacheContainer, IsNSNumber endpoint, IsNSObject queue) => clusterStateCacheContainer -> endpoint -> queue -> Ptr () -> IO ()
readAttributeAcceptedCommandListWithClusterStateCache_endpoint_queue_completion clusterStateCacheContainer endpoint queue completion =
  do
    cls' <- getRequiredClass "MTRBaseClusterGroups"
    sendClassMessage cls' readAttributeAcceptedCommandListWithClusterStateCache_endpoint_queue_completionSelector (toMTRClusterStateCacheContainer clusterStateCacheContainer) (toNSNumber endpoint) (toNSObject queue) completion

-- | @- readAttributeAttributeListWithCompletion:@
readAttributeAttributeListWithCompletion :: IsMTRBaseClusterGroups mtrBaseClusterGroups => mtrBaseClusterGroups -> Ptr () -> IO ()
readAttributeAttributeListWithCompletion mtrBaseClusterGroups completion =
  sendMessage mtrBaseClusterGroups readAttributeAttributeListWithCompletionSelector completion

-- | @- subscribeAttributeAttributeListWithParams:subscriptionEstablished:reportHandler:@
subscribeAttributeAttributeListWithParams_subscriptionEstablished_reportHandler :: (IsMTRBaseClusterGroups mtrBaseClusterGroups, IsMTRSubscribeParams params) => mtrBaseClusterGroups -> params -> Ptr () -> Ptr () -> IO ()
subscribeAttributeAttributeListWithParams_subscriptionEstablished_reportHandler mtrBaseClusterGroups params subscriptionEstablished reportHandler =
  sendMessage mtrBaseClusterGroups subscribeAttributeAttributeListWithParams_subscriptionEstablished_reportHandlerSelector (toMTRSubscribeParams params) subscriptionEstablished reportHandler

-- | @+ readAttributeAttributeListWithClusterStateCache:endpoint:queue:completion:@
readAttributeAttributeListWithClusterStateCache_endpoint_queue_completion :: (IsMTRClusterStateCacheContainer clusterStateCacheContainer, IsNSNumber endpoint, IsNSObject queue) => clusterStateCacheContainer -> endpoint -> queue -> Ptr () -> IO ()
readAttributeAttributeListWithClusterStateCache_endpoint_queue_completion clusterStateCacheContainer endpoint queue completion =
  do
    cls' <- getRequiredClass "MTRBaseClusterGroups"
    sendClassMessage cls' readAttributeAttributeListWithClusterStateCache_endpoint_queue_completionSelector (toMTRClusterStateCacheContainer clusterStateCacheContainer) (toNSNumber endpoint) (toNSObject queue) completion

-- | @- readAttributeFeatureMapWithCompletion:@
readAttributeFeatureMapWithCompletion :: IsMTRBaseClusterGroups mtrBaseClusterGroups => mtrBaseClusterGroups -> Ptr () -> IO ()
readAttributeFeatureMapWithCompletion mtrBaseClusterGroups completion =
  sendMessage mtrBaseClusterGroups readAttributeFeatureMapWithCompletionSelector completion

-- | @- subscribeAttributeFeatureMapWithParams:subscriptionEstablished:reportHandler:@
subscribeAttributeFeatureMapWithParams_subscriptionEstablished_reportHandler :: (IsMTRBaseClusterGroups mtrBaseClusterGroups, IsMTRSubscribeParams params) => mtrBaseClusterGroups -> params -> Ptr () -> Ptr () -> IO ()
subscribeAttributeFeatureMapWithParams_subscriptionEstablished_reportHandler mtrBaseClusterGroups params subscriptionEstablished reportHandler =
  sendMessage mtrBaseClusterGroups subscribeAttributeFeatureMapWithParams_subscriptionEstablished_reportHandlerSelector (toMTRSubscribeParams params) subscriptionEstablished reportHandler

-- | @+ readAttributeFeatureMapWithClusterStateCache:endpoint:queue:completion:@
readAttributeFeatureMapWithClusterStateCache_endpoint_queue_completion :: (IsMTRClusterStateCacheContainer clusterStateCacheContainer, IsNSNumber endpoint, IsNSObject queue) => clusterStateCacheContainer -> endpoint -> queue -> Ptr () -> IO ()
readAttributeFeatureMapWithClusterStateCache_endpoint_queue_completion clusterStateCacheContainer endpoint queue completion =
  do
    cls' <- getRequiredClass "MTRBaseClusterGroups"
    sendClassMessage cls' readAttributeFeatureMapWithClusterStateCache_endpoint_queue_completionSelector (toMTRClusterStateCacheContainer clusterStateCacheContainer) (toNSNumber endpoint) (toNSObject queue) completion

-- | @- readAttributeClusterRevisionWithCompletion:@
readAttributeClusterRevisionWithCompletion :: IsMTRBaseClusterGroups mtrBaseClusterGroups => mtrBaseClusterGroups -> Ptr () -> IO ()
readAttributeClusterRevisionWithCompletion mtrBaseClusterGroups completion =
  sendMessage mtrBaseClusterGroups readAttributeClusterRevisionWithCompletionSelector completion

-- | @- subscribeAttributeClusterRevisionWithParams:subscriptionEstablished:reportHandler:@
subscribeAttributeClusterRevisionWithParams_subscriptionEstablished_reportHandler :: (IsMTRBaseClusterGroups mtrBaseClusterGroups, IsMTRSubscribeParams params) => mtrBaseClusterGroups -> params -> Ptr () -> Ptr () -> IO ()
subscribeAttributeClusterRevisionWithParams_subscriptionEstablished_reportHandler mtrBaseClusterGroups params subscriptionEstablished reportHandler =
  sendMessage mtrBaseClusterGroups subscribeAttributeClusterRevisionWithParams_subscriptionEstablished_reportHandlerSelector (toMTRSubscribeParams params) subscriptionEstablished reportHandler

-- | @+ readAttributeClusterRevisionWithClusterStateCache:endpoint:queue:completion:@
readAttributeClusterRevisionWithClusterStateCache_endpoint_queue_completion :: (IsMTRClusterStateCacheContainer clusterStateCacheContainer, IsNSNumber endpoint, IsNSObject queue) => clusterStateCacheContainer -> endpoint -> queue -> Ptr () -> IO ()
readAttributeClusterRevisionWithClusterStateCache_endpoint_queue_completion clusterStateCacheContainer endpoint queue completion =
  do
    cls' <- getRequiredClass "MTRBaseClusterGroups"
    sendClassMessage cls' readAttributeClusterRevisionWithClusterStateCache_endpoint_queue_completionSelector (toMTRClusterStateCacheContainer clusterStateCacheContainer) (toNSNumber endpoint) (toNSObject queue) completion

-- | @- init@
init_ :: IsMTRBaseClusterGroups mtrBaseClusterGroups => mtrBaseClusterGroups -> IO (Id MTRBaseClusterGroups)
init_ mtrBaseClusterGroups =
  sendOwnedMessage mtrBaseClusterGroups initSelector

-- | @+ new@
new :: IO (Id MTRBaseClusterGroups)
new  =
  do
    cls' <- getRequiredClass "MTRBaseClusterGroups"
    sendOwnedClassMessage cls' newSelector

-- | @- initWithDevice:endpoint:queue:@
initWithDevice_endpoint_queue :: (IsMTRBaseClusterGroups mtrBaseClusterGroups, IsMTRBaseDevice device, IsNSObject queue) => mtrBaseClusterGroups -> device -> CUShort -> queue -> IO (Id MTRBaseClusterGroups)
initWithDevice_endpoint_queue mtrBaseClusterGroups device endpoint queue =
  sendOwnedMessage mtrBaseClusterGroups initWithDevice_endpoint_queueSelector (toMTRBaseDevice device) endpoint (toNSObject queue)

-- | @- addGroupWithParams:completionHandler:@
addGroupWithParams_completionHandler :: (IsMTRBaseClusterGroups mtrBaseClusterGroups, IsMTRGroupsClusterAddGroupParams params) => mtrBaseClusterGroups -> params -> Ptr () -> IO ()
addGroupWithParams_completionHandler mtrBaseClusterGroups params completionHandler =
  sendMessage mtrBaseClusterGroups addGroupWithParams_completionHandlerSelector (toMTRGroupsClusterAddGroupParams params) completionHandler

-- | @- viewGroupWithParams:completionHandler:@
viewGroupWithParams_completionHandler :: (IsMTRBaseClusterGroups mtrBaseClusterGroups, IsMTRGroupsClusterViewGroupParams params) => mtrBaseClusterGroups -> params -> Ptr () -> IO ()
viewGroupWithParams_completionHandler mtrBaseClusterGroups params completionHandler =
  sendMessage mtrBaseClusterGroups viewGroupWithParams_completionHandlerSelector (toMTRGroupsClusterViewGroupParams params) completionHandler

-- | @- getGroupMembershipWithParams:completionHandler:@
getGroupMembershipWithParams_completionHandler :: (IsMTRBaseClusterGroups mtrBaseClusterGroups, IsMTRGroupsClusterGetGroupMembershipParams params) => mtrBaseClusterGroups -> params -> Ptr () -> IO ()
getGroupMembershipWithParams_completionHandler mtrBaseClusterGroups params completionHandler =
  sendMessage mtrBaseClusterGroups getGroupMembershipWithParams_completionHandlerSelector (toMTRGroupsClusterGetGroupMembershipParams params) completionHandler

-- | @- removeGroupWithParams:completionHandler:@
removeGroupWithParams_completionHandler :: (IsMTRBaseClusterGroups mtrBaseClusterGroups, IsMTRGroupsClusterRemoveGroupParams params) => mtrBaseClusterGroups -> params -> Ptr () -> IO ()
removeGroupWithParams_completionHandler mtrBaseClusterGroups params completionHandler =
  sendMessage mtrBaseClusterGroups removeGroupWithParams_completionHandlerSelector (toMTRGroupsClusterRemoveGroupParams params) completionHandler

-- | @- removeAllGroupsWithParams:completionHandler:@
removeAllGroupsWithParams_completionHandler :: (IsMTRBaseClusterGroups mtrBaseClusterGroups, IsMTRGroupsClusterRemoveAllGroupsParams params) => mtrBaseClusterGroups -> params -> Ptr () -> IO ()
removeAllGroupsWithParams_completionHandler mtrBaseClusterGroups params completionHandler =
  sendMessage mtrBaseClusterGroups removeAllGroupsWithParams_completionHandlerSelector (toMTRGroupsClusterRemoveAllGroupsParams params) completionHandler

-- | @- removeAllGroupsWithCompletionHandler:@
removeAllGroupsWithCompletionHandler :: IsMTRBaseClusterGroups mtrBaseClusterGroups => mtrBaseClusterGroups -> Ptr () -> IO ()
removeAllGroupsWithCompletionHandler mtrBaseClusterGroups completionHandler =
  sendMessage mtrBaseClusterGroups removeAllGroupsWithCompletionHandlerSelector completionHandler

-- | @- addGroupIfIdentifyingWithParams:completionHandler:@
addGroupIfIdentifyingWithParams_completionHandler :: (IsMTRBaseClusterGroups mtrBaseClusterGroups, IsMTRGroupsClusterAddGroupIfIdentifyingParams params) => mtrBaseClusterGroups -> params -> Ptr () -> IO ()
addGroupIfIdentifyingWithParams_completionHandler mtrBaseClusterGroups params completionHandler =
  sendMessage mtrBaseClusterGroups addGroupIfIdentifyingWithParams_completionHandlerSelector (toMTRGroupsClusterAddGroupIfIdentifyingParams params) completionHandler

-- | @- readAttributeNameSupportWithCompletionHandler:@
readAttributeNameSupportWithCompletionHandler :: IsMTRBaseClusterGroups mtrBaseClusterGroups => mtrBaseClusterGroups -> Ptr () -> IO ()
readAttributeNameSupportWithCompletionHandler mtrBaseClusterGroups completionHandler =
  sendMessage mtrBaseClusterGroups readAttributeNameSupportWithCompletionHandlerSelector completionHandler

-- | @- subscribeAttributeNameSupportWithMinInterval:maxInterval:params:subscriptionEstablished:reportHandler:@
subscribeAttributeNameSupportWithMinInterval_maxInterval_params_subscriptionEstablished_reportHandler :: (IsMTRBaseClusterGroups mtrBaseClusterGroups, IsNSNumber minInterval, IsNSNumber maxInterval, IsMTRSubscribeParams params) => mtrBaseClusterGroups -> minInterval -> maxInterval -> params -> Ptr () -> Ptr () -> IO ()
subscribeAttributeNameSupportWithMinInterval_maxInterval_params_subscriptionEstablished_reportHandler mtrBaseClusterGroups minInterval maxInterval params subscriptionEstablishedHandler reportHandler =
  sendMessage mtrBaseClusterGroups subscribeAttributeNameSupportWithMinInterval_maxInterval_params_subscriptionEstablished_reportHandlerSelector (toNSNumber minInterval) (toNSNumber maxInterval) (toMTRSubscribeParams params) subscriptionEstablishedHandler reportHandler

-- | @+ readAttributeNameSupportWithAttributeCache:endpoint:queue:completionHandler:@
readAttributeNameSupportWithAttributeCache_endpoint_queue_completionHandler :: (IsMTRAttributeCacheContainer attributeCacheContainer, IsNSNumber endpoint, IsNSObject queue) => attributeCacheContainer -> endpoint -> queue -> Ptr () -> IO ()
readAttributeNameSupportWithAttributeCache_endpoint_queue_completionHandler attributeCacheContainer endpoint queue completionHandler =
  do
    cls' <- getRequiredClass "MTRBaseClusterGroups"
    sendClassMessage cls' readAttributeNameSupportWithAttributeCache_endpoint_queue_completionHandlerSelector (toMTRAttributeCacheContainer attributeCacheContainer) (toNSNumber endpoint) (toNSObject queue) completionHandler

-- | @- readAttributeGeneratedCommandListWithCompletionHandler:@
readAttributeGeneratedCommandListWithCompletionHandler :: IsMTRBaseClusterGroups mtrBaseClusterGroups => mtrBaseClusterGroups -> Ptr () -> IO ()
readAttributeGeneratedCommandListWithCompletionHandler mtrBaseClusterGroups completionHandler =
  sendMessage mtrBaseClusterGroups readAttributeGeneratedCommandListWithCompletionHandlerSelector completionHandler

-- | @- subscribeAttributeGeneratedCommandListWithMinInterval:maxInterval:params:subscriptionEstablished:reportHandler:@
subscribeAttributeGeneratedCommandListWithMinInterval_maxInterval_params_subscriptionEstablished_reportHandler :: (IsMTRBaseClusterGroups mtrBaseClusterGroups, IsNSNumber minInterval, IsNSNumber maxInterval, IsMTRSubscribeParams params) => mtrBaseClusterGroups -> minInterval -> maxInterval -> params -> Ptr () -> Ptr () -> IO ()
subscribeAttributeGeneratedCommandListWithMinInterval_maxInterval_params_subscriptionEstablished_reportHandler mtrBaseClusterGroups minInterval maxInterval params subscriptionEstablishedHandler reportHandler =
  sendMessage mtrBaseClusterGroups subscribeAttributeGeneratedCommandListWithMinInterval_maxInterval_params_subscriptionEstablished_reportHandlerSelector (toNSNumber minInterval) (toNSNumber maxInterval) (toMTRSubscribeParams params) subscriptionEstablishedHandler reportHandler

-- | @+ readAttributeGeneratedCommandListWithAttributeCache:endpoint:queue:completionHandler:@
readAttributeGeneratedCommandListWithAttributeCache_endpoint_queue_completionHandler :: (IsMTRAttributeCacheContainer attributeCacheContainer, IsNSNumber endpoint, IsNSObject queue) => attributeCacheContainer -> endpoint -> queue -> Ptr () -> IO ()
readAttributeGeneratedCommandListWithAttributeCache_endpoint_queue_completionHandler attributeCacheContainer endpoint queue completionHandler =
  do
    cls' <- getRequiredClass "MTRBaseClusterGroups"
    sendClassMessage cls' readAttributeGeneratedCommandListWithAttributeCache_endpoint_queue_completionHandlerSelector (toMTRAttributeCacheContainer attributeCacheContainer) (toNSNumber endpoint) (toNSObject queue) completionHandler

-- | @- readAttributeAcceptedCommandListWithCompletionHandler:@
readAttributeAcceptedCommandListWithCompletionHandler :: IsMTRBaseClusterGroups mtrBaseClusterGroups => mtrBaseClusterGroups -> Ptr () -> IO ()
readAttributeAcceptedCommandListWithCompletionHandler mtrBaseClusterGroups completionHandler =
  sendMessage mtrBaseClusterGroups readAttributeAcceptedCommandListWithCompletionHandlerSelector completionHandler

-- | @- subscribeAttributeAcceptedCommandListWithMinInterval:maxInterval:params:subscriptionEstablished:reportHandler:@
subscribeAttributeAcceptedCommandListWithMinInterval_maxInterval_params_subscriptionEstablished_reportHandler :: (IsMTRBaseClusterGroups mtrBaseClusterGroups, IsNSNumber minInterval, IsNSNumber maxInterval, IsMTRSubscribeParams params) => mtrBaseClusterGroups -> minInterval -> maxInterval -> params -> Ptr () -> Ptr () -> IO ()
subscribeAttributeAcceptedCommandListWithMinInterval_maxInterval_params_subscriptionEstablished_reportHandler mtrBaseClusterGroups minInterval maxInterval params subscriptionEstablishedHandler reportHandler =
  sendMessage mtrBaseClusterGroups subscribeAttributeAcceptedCommandListWithMinInterval_maxInterval_params_subscriptionEstablished_reportHandlerSelector (toNSNumber minInterval) (toNSNumber maxInterval) (toMTRSubscribeParams params) subscriptionEstablishedHandler reportHandler

-- | @+ readAttributeAcceptedCommandListWithAttributeCache:endpoint:queue:completionHandler:@
readAttributeAcceptedCommandListWithAttributeCache_endpoint_queue_completionHandler :: (IsMTRAttributeCacheContainer attributeCacheContainer, IsNSNumber endpoint, IsNSObject queue) => attributeCacheContainer -> endpoint -> queue -> Ptr () -> IO ()
readAttributeAcceptedCommandListWithAttributeCache_endpoint_queue_completionHandler attributeCacheContainer endpoint queue completionHandler =
  do
    cls' <- getRequiredClass "MTRBaseClusterGroups"
    sendClassMessage cls' readAttributeAcceptedCommandListWithAttributeCache_endpoint_queue_completionHandlerSelector (toMTRAttributeCacheContainer attributeCacheContainer) (toNSNumber endpoint) (toNSObject queue) completionHandler

-- | @- readAttributeAttributeListWithCompletionHandler:@
readAttributeAttributeListWithCompletionHandler :: IsMTRBaseClusterGroups mtrBaseClusterGroups => mtrBaseClusterGroups -> Ptr () -> IO ()
readAttributeAttributeListWithCompletionHandler mtrBaseClusterGroups completionHandler =
  sendMessage mtrBaseClusterGroups readAttributeAttributeListWithCompletionHandlerSelector completionHandler

-- | @- subscribeAttributeAttributeListWithMinInterval:maxInterval:params:subscriptionEstablished:reportHandler:@
subscribeAttributeAttributeListWithMinInterval_maxInterval_params_subscriptionEstablished_reportHandler :: (IsMTRBaseClusterGroups mtrBaseClusterGroups, IsNSNumber minInterval, IsNSNumber maxInterval, IsMTRSubscribeParams params) => mtrBaseClusterGroups -> minInterval -> maxInterval -> params -> Ptr () -> Ptr () -> IO ()
subscribeAttributeAttributeListWithMinInterval_maxInterval_params_subscriptionEstablished_reportHandler mtrBaseClusterGroups minInterval maxInterval params subscriptionEstablishedHandler reportHandler =
  sendMessage mtrBaseClusterGroups subscribeAttributeAttributeListWithMinInterval_maxInterval_params_subscriptionEstablished_reportHandlerSelector (toNSNumber minInterval) (toNSNumber maxInterval) (toMTRSubscribeParams params) subscriptionEstablishedHandler reportHandler

-- | @+ readAttributeAttributeListWithAttributeCache:endpoint:queue:completionHandler:@
readAttributeAttributeListWithAttributeCache_endpoint_queue_completionHandler :: (IsMTRAttributeCacheContainer attributeCacheContainer, IsNSNumber endpoint, IsNSObject queue) => attributeCacheContainer -> endpoint -> queue -> Ptr () -> IO ()
readAttributeAttributeListWithAttributeCache_endpoint_queue_completionHandler attributeCacheContainer endpoint queue completionHandler =
  do
    cls' <- getRequiredClass "MTRBaseClusterGroups"
    sendClassMessage cls' readAttributeAttributeListWithAttributeCache_endpoint_queue_completionHandlerSelector (toMTRAttributeCacheContainer attributeCacheContainer) (toNSNumber endpoint) (toNSObject queue) completionHandler

-- | @- readAttributeFeatureMapWithCompletionHandler:@
readAttributeFeatureMapWithCompletionHandler :: IsMTRBaseClusterGroups mtrBaseClusterGroups => mtrBaseClusterGroups -> Ptr () -> IO ()
readAttributeFeatureMapWithCompletionHandler mtrBaseClusterGroups completionHandler =
  sendMessage mtrBaseClusterGroups readAttributeFeatureMapWithCompletionHandlerSelector completionHandler

-- | @- subscribeAttributeFeatureMapWithMinInterval:maxInterval:params:subscriptionEstablished:reportHandler:@
subscribeAttributeFeatureMapWithMinInterval_maxInterval_params_subscriptionEstablished_reportHandler :: (IsMTRBaseClusterGroups mtrBaseClusterGroups, IsNSNumber minInterval, IsNSNumber maxInterval, IsMTRSubscribeParams params) => mtrBaseClusterGroups -> minInterval -> maxInterval -> params -> Ptr () -> Ptr () -> IO ()
subscribeAttributeFeatureMapWithMinInterval_maxInterval_params_subscriptionEstablished_reportHandler mtrBaseClusterGroups minInterval maxInterval params subscriptionEstablishedHandler reportHandler =
  sendMessage mtrBaseClusterGroups subscribeAttributeFeatureMapWithMinInterval_maxInterval_params_subscriptionEstablished_reportHandlerSelector (toNSNumber minInterval) (toNSNumber maxInterval) (toMTRSubscribeParams params) subscriptionEstablishedHandler reportHandler

-- | @+ readAttributeFeatureMapWithAttributeCache:endpoint:queue:completionHandler:@
readAttributeFeatureMapWithAttributeCache_endpoint_queue_completionHandler :: (IsMTRAttributeCacheContainer attributeCacheContainer, IsNSNumber endpoint, IsNSObject queue) => attributeCacheContainer -> endpoint -> queue -> Ptr () -> IO ()
readAttributeFeatureMapWithAttributeCache_endpoint_queue_completionHandler attributeCacheContainer endpoint queue completionHandler =
  do
    cls' <- getRequiredClass "MTRBaseClusterGroups"
    sendClassMessage cls' readAttributeFeatureMapWithAttributeCache_endpoint_queue_completionHandlerSelector (toMTRAttributeCacheContainer attributeCacheContainer) (toNSNumber endpoint) (toNSObject queue) completionHandler

-- | @- readAttributeClusterRevisionWithCompletionHandler:@
readAttributeClusterRevisionWithCompletionHandler :: IsMTRBaseClusterGroups mtrBaseClusterGroups => mtrBaseClusterGroups -> Ptr () -> IO ()
readAttributeClusterRevisionWithCompletionHandler mtrBaseClusterGroups completionHandler =
  sendMessage mtrBaseClusterGroups readAttributeClusterRevisionWithCompletionHandlerSelector completionHandler

-- | @- subscribeAttributeClusterRevisionWithMinInterval:maxInterval:params:subscriptionEstablished:reportHandler:@
subscribeAttributeClusterRevisionWithMinInterval_maxInterval_params_subscriptionEstablished_reportHandler :: (IsMTRBaseClusterGroups mtrBaseClusterGroups, IsNSNumber minInterval, IsNSNumber maxInterval, IsMTRSubscribeParams params) => mtrBaseClusterGroups -> minInterval -> maxInterval -> params -> Ptr () -> Ptr () -> IO ()
subscribeAttributeClusterRevisionWithMinInterval_maxInterval_params_subscriptionEstablished_reportHandler mtrBaseClusterGroups minInterval maxInterval params subscriptionEstablishedHandler reportHandler =
  sendMessage mtrBaseClusterGroups subscribeAttributeClusterRevisionWithMinInterval_maxInterval_params_subscriptionEstablished_reportHandlerSelector (toNSNumber minInterval) (toNSNumber maxInterval) (toMTRSubscribeParams params) subscriptionEstablishedHandler reportHandler

-- | @+ readAttributeClusterRevisionWithAttributeCache:endpoint:queue:completionHandler:@
readAttributeClusterRevisionWithAttributeCache_endpoint_queue_completionHandler :: (IsMTRAttributeCacheContainer attributeCacheContainer, IsNSNumber endpoint, IsNSObject queue) => attributeCacheContainer -> endpoint -> queue -> Ptr () -> IO ()
readAttributeClusterRevisionWithAttributeCache_endpoint_queue_completionHandler attributeCacheContainer endpoint queue completionHandler =
  do
    cls' <- getRequiredClass "MTRBaseClusterGroups"
    sendClassMessage cls' readAttributeClusterRevisionWithAttributeCache_endpoint_queue_completionHandlerSelector (toMTRAttributeCacheContainer attributeCacheContainer) (toNSNumber endpoint) (toNSObject queue) completionHandler

-- | For all instance methods (reads, writes, commands) that take a completion, the completion will be called on the provided queue.
--
-- ObjC selector: @- initWithDevice:endpointID:queue:@
initWithDevice_endpointID_queue :: (IsMTRBaseClusterGroups mtrBaseClusterGroups, IsMTRBaseDevice device, IsNSNumber endpointID, IsNSObject queue) => mtrBaseClusterGroups -> device -> endpointID -> queue -> IO (Id MTRBaseClusterGroups)
initWithDevice_endpointID_queue mtrBaseClusterGroups device endpointID queue =
  sendOwnedMessage mtrBaseClusterGroups initWithDevice_endpointID_queueSelector (toMTRBaseDevice device) (toNSNumber endpointID) (toNSObject queue)

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @addGroupWithParams:completion:@
addGroupWithParams_completionSelector :: Selector '[Id MTRGroupsClusterAddGroupParams, Ptr ()] ()
addGroupWithParams_completionSelector = mkSelector "addGroupWithParams:completion:"

-- | @Selector@ for @viewGroupWithParams:completion:@
viewGroupWithParams_completionSelector :: Selector '[Id MTRGroupsClusterViewGroupParams, Ptr ()] ()
viewGroupWithParams_completionSelector = mkSelector "viewGroupWithParams:completion:"

-- | @Selector@ for @getGroupMembershipWithParams:completion:@
getGroupMembershipWithParams_completionSelector :: Selector '[Id MTRGroupsClusterGetGroupMembershipParams, Ptr ()] ()
getGroupMembershipWithParams_completionSelector = mkSelector "getGroupMembershipWithParams:completion:"

-- | @Selector@ for @removeGroupWithParams:completion:@
removeGroupWithParams_completionSelector :: Selector '[Id MTRGroupsClusterRemoveGroupParams, Ptr ()] ()
removeGroupWithParams_completionSelector = mkSelector "removeGroupWithParams:completion:"

-- | @Selector@ for @removeAllGroupsWithParams:completion:@
removeAllGroupsWithParams_completionSelector :: Selector '[Id MTRGroupsClusterRemoveAllGroupsParams, Ptr ()] ()
removeAllGroupsWithParams_completionSelector = mkSelector "removeAllGroupsWithParams:completion:"

-- | @Selector@ for @removeAllGroupsWithCompletion:@
removeAllGroupsWithCompletionSelector :: Selector '[Ptr ()] ()
removeAllGroupsWithCompletionSelector = mkSelector "removeAllGroupsWithCompletion:"

-- | @Selector@ for @addGroupIfIdentifyingWithParams:completion:@
addGroupIfIdentifyingWithParams_completionSelector :: Selector '[Id MTRGroupsClusterAddGroupIfIdentifyingParams, Ptr ()] ()
addGroupIfIdentifyingWithParams_completionSelector = mkSelector "addGroupIfIdentifyingWithParams:completion:"

-- | @Selector@ for @readAttributeNameSupportWithCompletion:@
readAttributeNameSupportWithCompletionSelector :: Selector '[Ptr ()] ()
readAttributeNameSupportWithCompletionSelector = mkSelector "readAttributeNameSupportWithCompletion:"

-- | @Selector@ for @subscribeAttributeNameSupportWithParams:subscriptionEstablished:reportHandler:@
subscribeAttributeNameSupportWithParams_subscriptionEstablished_reportHandlerSelector :: Selector '[Id MTRSubscribeParams, Ptr (), Ptr ()] ()
subscribeAttributeNameSupportWithParams_subscriptionEstablished_reportHandlerSelector = mkSelector "subscribeAttributeNameSupportWithParams:subscriptionEstablished:reportHandler:"

-- | @Selector@ for @readAttributeNameSupportWithClusterStateCache:endpoint:queue:completion:@
readAttributeNameSupportWithClusterStateCache_endpoint_queue_completionSelector :: Selector '[Id MTRClusterStateCacheContainer, Id NSNumber, Id NSObject, Ptr ()] ()
readAttributeNameSupportWithClusterStateCache_endpoint_queue_completionSelector = mkSelector "readAttributeNameSupportWithClusterStateCache:endpoint:queue:completion:"

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
initSelector :: Selector '[] (Id MTRBaseClusterGroups)
initSelector = mkSelector "init"

-- | @Selector@ for @new@
newSelector :: Selector '[] (Id MTRBaseClusterGroups)
newSelector = mkSelector "new"

-- | @Selector@ for @initWithDevice:endpoint:queue:@
initWithDevice_endpoint_queueSelector :: Selector '[Id MTRBaseDevice, CUShort, Id NSObject] (Id MTRBaseClusterGroups)
initWithDevice_endpoint_queueSelector = mkSelector "initWithDevice:endpoint:queue:"

-- | @Selector@ for @addGroupWithParams:completionHandler:@
addGroupWithParams_completionHandlerSelector :: Selector '[Id MTRGroupsClusterAddGroupParams, Ptr ()] ()
addGroupWithParams_completionHandlerSelector = mkSelector "addGroupWithParams:completionHandler:"

-- | @Selector@ for @viewGroupWithParams:completionHandler:@
viewGroupWithParams_completionHandlerSelector :: Selector '[Id MTRGroupsClusterViewGroupParams, Ptr ()] ()
viewGroupWithParams_completionHandlerSelector = mkSelector "viewGroupWithParams:completionHandler:"

-- | @Selector@ for @getGroupMembershipWithParams:completionHandler:@
getGroupMembershipWithParams_completionHandlerSelector :: Selector '[Id MTRGroupsClusterGetGroupMembershipParams, Ptr ()] ()
getGroupMembershipWithParams_completionHandlerSelector = mkSelector "getGroupMembershipWithParams:completionHandler:"

-- | @Selector@ for @removeGroupWithParams:completionHandler:@
removeGroupWithParams_completionHandlerSelector :: Selector '[Id MTRGroupsClusterRemoveGroupParams, Ptr ()] ()
removeGroupWithParams_completionHandlerSelector = mkSelector "removeGroupWithParams:completionHandler:"

-- | @Selector@ for @removeAllGroupsWithParams:completionHandler:@
removeAllGroupsWithParams_completionHandlerSelector :: Selector '[Id MTRGroupsClusterRemoveAllGroupsParams, Ptr ()] ()
removeAllGroupsWithParams_completionHandlerSelector = mkSelector "removeAllGroupsWithParams:completionHandler:"

-- | @Selector@ for @removeAllGroupsWithCompletionHandler:@
removeAllGroupsWithCompletionHandlerSelector :: Selector '[Ptr ()] ()
removeAllGroupsWithCompletionHandlerSelector = mkSelector "removeAllGroupsWithCompletionHandler:"

-- | @Selector@ for @addGroupIfIdentifyingWithParams:completionHandler:@
addGroupIfIdentifyingWithParams_completionHandlerSelector :: Selector '[Id MTRGroupsClusterAddGroupIfIdentifyingParams, Ptr ()] ()
addGroupIfIdentifyingWithParams_completionHandlerSelector = mkSelector "addGroupIfIdentifyingWithParams:completionHandler:"

-- | @Selector@ for @readAttributeNameSupportWithCompletionHandler:@
readAttributeNameSupportWithCompletionHandlerSelector :: Selector '[Ptr ()] ()
readAttributeNameSupportWithCompletionHandlerSelector = mkSelector "readAttributeNameSupportWithCompletionHandler:"

-- | @Selector@ for @subscribeAttributeNameSupportWithMinInterval:maxInterval:params:subscriptionEstablished:reportHandler:@
subscribeAttributeNameSupportWithMinInterval_maxInterval_params_subscriptionEstablished_reportHandlerSelector :: Selector '[Id NSNumber, Id NSNumber, Id MTRSubscribeParams, Ptr (), Ptr ()] ()
subscribeAttributeNameSupportWithMinInterval_maxInterval_params_subscriptionEstablished_reportHandlerSelector = mkSelector "subscribeAttributeNameSupportWithMinInterval:maxInterval:params:subscriptionEstablished:reportHandler:"

-- | @Selector@ for @readAttributeNameSupportWithAttributeCache:endpoint:queue:completionHandler:@
readAttributeNameSupportWithAttributeCache_endpoint_queue_completionHandlerSelector :: Selector '[Id MTRAttributeCacheContainer, Id NSNumber, Id NSObject, Ptr ()] ()
readAttributeNameSupportWithAttributeCache_endpoint_queue_completionHandlerSelector = mkSelector "readAttributeNameSupportWithAttributeCache:endpoint:queue:completionHandler:"

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
initWithDevice_endpointID_queueSelector :: Selector '[Id MTRBaseDevice, Id NSNumber, Id NSObject] (Id MTRBaseClusterGroups)
initWithDevice_endpointID_queueSelector = mkSelector "initWithDevice:endpointID:queue:"

