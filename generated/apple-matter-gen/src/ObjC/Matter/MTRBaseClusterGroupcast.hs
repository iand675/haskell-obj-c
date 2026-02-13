{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Cluster Groupcast
--
-- The Groupcast cluster manages the content of the node-wide multicast Group membership that is part of the underlying interaction layer.
--
-- Generated bindings for @MTRBaseClusterGroupcast@.
module ObjC.Matter.MTRBaseClusterGroupcast
  ( MTRBaseClusterGroupcast
  , IsMTRBaseClusterGroupcast(..)
  , joinGroupWithParams_completion
  , leaveGroupWithParams_completion
  , updateGroupKeyWithParams_completion
  , expireGracePeriodWithParams_completion
  , configureAuxiliaryACLWithParams_completion
  , readAttributeMembershipWithParams_completion
  , subscribeAttributeMembershipWithParams_subscriptionEstablished_reportHandler
  , readAttributeMembershipWithClusterStateCache_endpoint_queue_completion
  , readAttributeMaxMembershipCountWithCompletion
  , subscribeAttributeMaxMembershipCountWithParams_subscriptionEstablished_reportHandler
  , readAttributeMaxMembershipCountWithClusterStateCache_endpoint_queue_completion
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
  , configureAuxiliaryACLWithParams_completionSelector
  , expireGracePeriodWithParams_completionSelector
  , initSelector
  , initWithDevice_endpointID_queueSelector
  , joinGroupWithParams_completionSelector
  , leaveGroupWithParams_completionSelector
  , newSelector
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
  , readAttributeMaxMembershipCountWithClusterStateCache_endpoint_queue_completionSelector
  , readAttributeMaxMembershipCountWithCompletionSelector
  , readAttributeMembershipWithClusterStateCache_endpoint_queue_completionSelector
  , readAttributeMembershipWithParams_completionSelector
  , subscribeAttributeAcceptedCommandListWithParams_subscriptionEstablished_reportHandlerSelector
  , subscribeAttributeAttributeListWithParams_subscriptionEstablished_reportHandlerSelector
  , subscribeAttributeClusterRevisionWithParams_subscriptionEstablished_reportHandlerSelector
  , subscribeAttributeFeatureMapWithParams_subscriptionEstablished_reportHandlerSelector
  , subscribeAttributeGeneratedCommandListWithParams_subscriptionEstablished_reportHandlerSelector
  , subscribeAttributeMaxMembershipCountWithParams_subscriptionEstablished_reportHandlerSelector
  , subscribeAttributeMembershipWithParams_subscriptionEstablished_reportHandlerSelector
  , updateGroupKeyWithParams_completionSelector


  ) where

import Foreign.Ptr (Ptr, FunPtr)
import Foreign.C.Types

import ObjC.Runtime.Types
import ObjC.Runtime.Message (sendMessage, sendOwnedMessage, sendClassMessage, sendOwnedClassMessage)
import ObjC.Runtime.Selector (mkSelector)
import ObjC.Runtime.Class (getRequiredClass)

import ObjC.Matter.Internal.Classes
import ObjC.Foundation.Internal.Classes

-- | Command JoinGroup
--
-- This command SHALL be used to instruct the server to join a multicast group.
--
-- ObjC selector: @- joinGroupWithParams:completion:@
joinGroupWithParams_completion :: (IsMTRBaseClusterGroupcast mtrBaseClusterGroupcast, IsMTRGroupcastClusterJoinGroupParams params) => mtrBaseClusterGroupcast -> params -> Ptr () -> IO ()
joinGroupWithParams_completion mtrBaseClusterGroupcast params completion =
  sendMessage mtrBaseClusterGroupcast joinGroupWithParams_completionSelector (toMTRGroupcastClusterJoinGroupParams params) completion

-- | Command LeaveGroup
--
-- This command SHALL allow a maintainer to request that the server withdraws itself or specific endpoints from a specific group or from all groups of this client's fabric.
--
-- ObjC selector: @- leaveGroupWithParams:completion:@
leaveGroupWithParams_completion :: (IsMTRBaseClusterGroupcast mtrBaseClusterGroupcast, IsMTRGroupcastClusterLeaveGroupParams params) => mtrBaseClusterGroupcast -> params -> Ptr () -> IO ()
leaveGroupWithParams_completion mtrBaseClusterGroupcast params completion =
  sendMessage mtrBaseClusterGroupcast leaveGroupWithParams_completionSelector (toMTRGroupcastClusterLeaveGroupParams params) completion

-- | Command UpdateGroupKey
--
-- This command SHALL allow a fabric maintainer to update the OperationalGroupKey for an existing group ID that the server is a member of.
--
-- ObjC selector: @- updateGroupKeyWithParams:completion:@
updateGroupKeyWithParams_completion :: (IsMTRBaseClusterGroupcast mtrBaseClusterGroupcast, IsMTRGroupcastClusterUpdateGroupKeyParams params) => mtrBaseClusterGroupcast -> params -> Ptr () -> IO ()
updateGroupKeyWithParams_completion mtrBaseClusterGroupcast params completion =
  sendMessage mtrBaseClusterGroupcast updateGroupKeyWithParams_completionSelector (toMTRGroupcastClusterUpdateGroupKeyParams params) completion

-- | Command ExpireGracePeriod
--
-- This command SHALL allow a fabric maintainer to expire the grace period of the previous key for an existing group ID that the server is a member of.
--
-- ObjC selector: @- expireGracePeriodWithParams:completion:@
expireGracePeriodWithParams_completion :: (IsMTRBaseClusterGroupcast mtrBaseClusterGroupcast, IsMTRGroupcastClusterExpireGracePeriodParams params) => mtrBaseClusterGroupcast -> params -> Ptr () -> IO ()
expireGracePeriodWithParams_completion mtrBaseClusterGroupcast params completion =
  sendMessage mtrBaseClusterGroupcast expireGracePeriodWithParams_completionSelector (toMTRGroupcastClusterExpireGracePeriodParams params) completion

-- | Command ConfigureAuxiliaryACL
--
-- This command SHALL allow an Administrator to enable or disable the generation of AuxiliaryACL entries in the Access Control Cluster based on the groups joined (see Groupcast Auxiliary ACL Handling).
--
-- ObjC selector: @- configureAuxiliaryACLWithParams:completion:@
configureAuxiliaryACLWithParams_completion :: (IsMTRBaseClusterGroupcast mtrBaseClusterGroupcast, IsMTRGroupcastClusterConfigureAuxiliaryACLParams params) => mtrBaseClusterGroupcast -> params -> Ptr () -> IO ()
configureAuxiliaryACLWithParams_completion mtrBaseClusterGroupcast params completion =
  sendMessage mtrBaseClusterGroupcast configureAuxiliaryACLWithParams_completionSelector (toMTRGroupcastClusterConfigureAuxiliaryACLParams params) completion

-- | @- readAttributeMembershipWithParams:completion:@
readAttributeMembershipWithParams_completion :: (IsMTRBaseClusterGroupcast mtrBaseClusterGroupcast, IsMTRReadParams params) => mtrBaseClusterGroupcast -> params -> Ptr () -> IO ()
readAttributeMembershipWithParams_completion mtrBaseClusterGroupcast params completion =
  sendMessage mtrBaseClusterGroupcast readAttributeMembershipWithParams_completionSelector (toMTRReadParams params) completion

-- | @- subscribeAttributeMembershipWithParams:subscriptionEstablished:reportHandler:@
subscribeAttributeMembershipWithParams_subscriptionEstablished_reportHandler :: (IsMTRBaseClusterGroupcast mtrBaseClusterGroupcast, IsMTRSubscribeParams params) => mtrBaseClusterGroupcast -> params -> Ptr () -> Ptr () -> IO ()
subscribeAttributeMembershipWithParams_subscriptionEstablished_reportHandler mtrBaseClusterGroupcast params subscriptionEstablished reportHandler =
  sendMessage mtrBaseClusterGroupcast subscribeAttributeMembershipWithParams_subscriptionEstablished_reportHandlerSelector (toMTRSubscribeParams params) subscriptionEstablished reportHandler

-- | @+ readAttributeMembershipWithClusterStateCache:endpoint:queue:completion:@
readAttributeMembershipWithClusterStateCache_endpoint_queue_completion :: (IsMTRClusterStateCacheContainer clusterStateCacheContainer, IsNSNumber endpoint, IsNSObject queue) => clusterStateCacheContainer -> endpoint -> queue -> Ptr () -> IO ()
readAttributeMembershipWithClusterStateCache_endpoint_queue_completion clusterStateCacheContainer endpoint queue completion =
  do
    cls' <- getRequiredClass "MTRBaseClusterGroupcast"
    sendClassMessage cls' readAttributeMembershipWithClusterStateCache_endpoint_queue_completionSelector (toMTRClusterStateCacheContainer clusterStateCacheContainer) (toNSNumber endpoint) (toNSObject queue) completion

-- | @- readAttributeMaxMembershipCountWithCompletion:@
readAttributeMaxMembershipCountWithCompletion :: IsMTRBaseClusterGroupcast mtrBaseClusterGroupcast => mtrBaseClusterGroupcast -> Ptr () -> IO ()
readAttributeMaxMembershipCountWithCompletion mtrBaseClusterGroupcast completion =
  sendMessage mtrBaseClusterGroupcast readAttributeMaxMembershipCountWithCompletionSelector completion

-- | @- subscribeAttributeMaxMembershipCountWithParams:subscriptionEstablished:reportHandler:@
subscribeAttributeMaxMembershipCountWithParams_subscriptionEstablished_reportHandler :: (IsMTRBaseClusterGroupcast mtrBaseClusterGroupcast, IsMTRSubscribeParams params) => mtrBaseClusterGroupcast -> params -> Ptr () -> Ptr () -> IO ()
subscribeAttributeMaxMembershipCountWithParams_subscriptionEstablished_reportHandler mtrBaseClusterGroupcast params subscriptionEstablished reportHandler =
  sendMessage mtrBaseClusterGroupcast subscribeAttributeMaxMembershipCountWithParams_subscriptionEstablished_reportHandlerSelector (toMTRSubscribeParams params) subscriptionEstablished reportHandler

-- | @+ readAttributeMaxMembershipCountWithClusterStateCache:endpoint:queue:completion:@
readAttributeMaxMembershipCountWithClusterStateCache_endpoint_queue_completion :: (IsMTRClusterStateCacheContainer clusterStateCacheContainer, IsNSNumber endpoint, IsNSObject queue) => clusterStateCacheContainer -> endpoint -> queue -> Ptr () -> IO ()
readAttributeMaxMembershipCountWithClusterStateCache_endpoint_queue_completion clusterStateCacheContainer endpoint queue completion =
  do
    cls' <- getRequiredClass "MTRBaseClusterGroupcast"
    sendClassMessage cls' readAttributeMaxMembershipCountWithClusterStateCache_endpoint_queue_completionSelector (toMTRClusterStateCacheContainer clusterStateCacheContainer) (toNSNumber endpoint) (toNSObject queue) completion

-- | @- readAttributeGeneratedCommandListWithCompletion:@
readAttributeGeneratedCommandListWithCompletion :: IsMTRBaseClusterGroupcast mtrBaseClusterGroupcast => mtrBaseClusterGroupcast -> Ptr () -> IO ()
readAttributeGeneratedCommandListWithCompletion mtrBaseClusterGroupcast completion =
  sendMessage mtrBaseClusterGroupcast readAttributeGeneratedCommandListWithCompletionSelector completion

-- | @- subscribeAttributeGeneratedCommandListWithParams:subscriptionEstablished:reportHandler:@
subscribeAttributeGeneratedCommandListWithParams_subscriptionEstablished_reportHandler :: (IsMTRBaseClusterGroupcast mtrBaseClusterGroupcast, IsMTRSubscribeParams params) => mtrBaseClusterGroupcast -> params -> Ptr () -> Ptr () -> IO ()
subscribeAttributeGeneratedCommandListWithParams_subscriptionEstablished_reportHandler mtrBaseClusterGroupcast params subscriptionEstablished reportHandler =
  sendMessage mtrBaseClusterGroupcast subscribeAttributeGeneratedCommandListWithParams_subscriptionEstablished_reportHandlerSelector (toMTRSubscribeParams params) subscriptionEstablished reportHandler

-- | @+ readAttributeGeneratedCommandListWithClusterStateCache:endpoint:queue:completion:@
readAttributeGeneratedCommandListWithClusterStateCache_endpoint_queue_completion :: (IsMTRClusterStateCacheContainer clusterStateCacheContainer, IsNSNumber endpoint, IsNSObject queue) => clusterStateCacheContainer -> endpoint -> queue -> Ptr () -> IO ()
readAttributeGeneratedCommandListWithClusterStateCache_endpoint_queue_completion clusterStateCacheContainer endpoint queue completion =
  do
    cls' <- getRequiredClass "MTRBaseClusterGroupcast"
    sendClassMessage cls' readAttributeGeneratedCommandListWithClusterStateCache_endpoint_queue_completionSelector (toMTRClusterStateCacheContainer clusterStateCacheContainer) (toNSNumber endpoint) (toNSObject queue) completion

-- | @- readAttributeAcceptedCommandListWithCompletion:@
readAttributeAcceptedCommandListWithCompletion :: IsMTRBaseClusterGroupcast mtrBaseClusterGroupcast => mtrBaseClusterGroupcast -> Ptr () -> IO ()
readAttributeAcceptedCommandListWithCompletion mtrBaseClusterGroupcast completion =
  sendMessage mtrBaseClusterGroupcast readAttributeAcceptedCommandListWithCompletionSelector completion

-- | @- subscribeAttributeAcceptedCommandListWithParams:subscriptionEstablished:reportHandler:@
subscribeAttributeAcceptedCommandListWithParams_subscriptionEstablished_reportHandler :: (IsMTRBaseClusterGroupcast mtrBaseClusterGroupcast, IsMTRSubscribeParams params) => mtrBaseClusterGroupcast -> params -> Ptr () -> Ptr () -> IO ()
subscribeAttributeAcceptedCommandListWithParams_subscriptionEstablished_reportHandler mtrBaseClusterGroupcast params subscriptionEstablished reportHandler =
  sendMessage mtrBaseClusterGroupcast subscribeAttributeAcceptedCommandListWithParams_subscriptionEstablished_reportHandlerSelector (toMTRSubscribeParams params) subscriptionEstablished reportHandler

-- | @+ readAttributeAcceptedCommandListWithClusterStateCache:endpoint:queue:completion:@
readAttributeAcceptedCommandListWithClusterStateCache_endpoint_queue_completion :: (IsMTRClusterStateCacheContainer clusterStateCacheContainer, IsNSNumber endpoint, IsNSObject queue) => clusterStateCacheContainer -> endpoint -> queue -> Ptr () -> IO ()
readAttributeAcceptedCommandListWithClusterStateCache_endpoint_queue_completion clusterStateCacheContainer endpoint queue completion =
  do
    cls' <- getRequiredClass "MTRBaseClusterGroupcast"
    sendClassMessage cls' readAttributeAcceptedCommandListWithClusterStateCache_endpoint_queue_completionSelector (toMTRClusterStateCacheContainer clusterStateCacheContainer) (toNSNumber endpoint) (toNSObject queue) completion

-- | @- readAttributeAttributeListWithCompletion:@
readAttributeAttributeListWithCompletion :: IsMTRBaseClusterGroupcast mtrBaseClusterGroupcast => mtrBaseClusterGroupcast -> Ptr () -> IO ()
readAttributeAttributeListWithCompletion mtrBaseClusterGroupcast completion =
  sendMessage mtrBaseClusterGroupcast readAttributeAttributeListWithCompletionSelector completion

-- | @- subscribeAttributeAttributeListWithParams:subscriptionEstablished:reportHandler:@
subscribeAttributeAttributeListWithParams_subscriptionEstablished_reportHandler :: (IsMTRBaseClusterGroupcast mtrBaseClusterGroupcast, IsMTRSubscribeParams params) => mtrBaseClusterGroupcast -> params -> Ptr () -> Ptr () -> IO ()
subscribeAttributeAttributeListWithParams_subscriptionEstablished_reportHandler mtrBaseClusterGroupcast params subscriptionEstablished reportHandler =
  sendMessage mtrBaseClusterGroupcast subscribeAttributeAttributeListWithParams_subscriptionEstablished_reportHandlerSelector (toMTRSubscribeParams params) subscriptionEstablished reportHandler

-- | @+ readAttributeAttributeListWithClusterStateCache:endpoint:queue:completion:@
readAttributeAttributeListWithClusterStateCache_endpoint_queue_completion :: (IsMTRClusterStateCacheContainer clusterStateCacheContainer, IsNSNumber endpoint, IsNSObject queue) => clusterStateCacheContainer -> endpoint -> queue -> Ptr () -> IO ()
readAttributeAttributeListWithClusterStateCache_endpoint_queue_completion clusterStateCacheContainer endpoint queue completion =
  do
    cls' <- getRequiredClass "MTRBaseClusterGroupcast"
    sendClassMessage cls' readAttributeAttributeListWithClusterStateCache_endpoint_queue_completionSelector (toMTRClusterStateCacheContainer clusterStateCacheContainer) (toNSNumber endpoint) (toNSObject queue) completion

-- | @- readAttributeFeatureMapWithCompletion:@
readAttributeFeatureMapWithCompletion :: IsMTRBaseClusterGroupcast mtrBaseClusterGroupcast => mtrBaseClusterGroupcast -> Ptr () -> IO ()
readAttributeFeatureMapWithCompletion mtrBaseClusterGroupcast completion =
  sendMessage mtrBaseClusterGroupcast readAttributeFeatureMapWithCompletionSelector completion

-- | @- subscribeAttributeFeatureMapWithParams:subscriptionEstablished:reportHandler:@
subscribeAttributeFeatureMapWithParams_subscriptionEstablished_reportHandler :: (IsMTRBaseClusterGroupcast mtrBaseClusterGroupcast, IsMTRSubscribeParams params) => mtrBaseClusterGroupcast -> params -> Ptr () -> Ptr () -> IO ()
subscribeAttributeFeatureMapWithParams_subscriptionEstablished_reportHandler mtrBaseClusterGroupcast params subscriptionEstablished reportHandler =
  sendMessage mtrBaseClusterGroupcast subscribeAttributeFeatureMapWithParams_subscriptionEstablished_reportHandlerSelector (toMTRSubscribeParams params) subscriptionEstablished reportHandler

-- | @+ readAttributeFeatureMapWithClusterStateCache:endpoint:queue:completion:@
readAttributeFeatureMapWithClusterStateCache_endpoint_queue_completion :: (IsMTRClusterStateCacheContainer clusterStateCacheContainer, IsNSNumber endpoint, IsNSObject queue) => clusterStateCacheContainer -> endpoint -> queue -> Ptr () -> IO ()
readAttributeFeatureMapWithClusterStateCache_endpoint_queue_completion clusterStateCacheContainer endpoint queue completion =
  do
    cls' <- getRequiredClass "MTRBaseClusterGroupcast"
    sendClassMessage cls' readAttributeFeatureMapWithClusterStateCache_endpoint_queue_completionSelector (toMTRClusterStateCacheContainer clusterStateCacheContainer) (toNSNumber endpoint) (toNSObject queue) completion

-- | @- readAttributeClusterRevisionWithCompletion:@
readAttributeClusterRevisionWithCompletion :: IsMTRBaseClusterGroupcast mtrBaseClusterGroupcast => mtrBaseClusterGroupcast -> Ptr () -> IO ()
readAttributeClusterRevisionWithCompletion mtrBaseClusterGroupcast completion =
  sendMessage mtrBaseClusterGroupcast readAttributeClusterRevisionWithCompletionSelector completion

-- | @- subscribeAttributeClusterRevisionWithParams:subscriptionEstablished:reportHandler:@
subscribeAttributeClusterRevisionWithParams_subscriptionEstablished_reportHandler :: (IsMTRBaseClusterGroupcast mtrBaseClusterGroupcast, IsMTRSubscribeParams params) => mtrBaseClusterGroupcast -> params -> Ptr () -> Ptr () -> IO ()
subscribeAttributeClusterRevisionWithParams_subscriptionEstablished_reportHandler mtrBaseClusterGroupcast params subscriptionEstablished reportHandler =
  sendMessage mtrBaseClusterGroupcast subscribeAttributeClusterRevisionWithParams_subscriptionEstablished_reportHandlerSelector (toMTRSubscribeParams params) subscriptionEstablished reportHandler

-- | @+ readAttributeClusterRevisionWithClusterStateCache:endpoint:queue:completion:@
readAttributeClusterRevisionWithClusterStateCache_endpoint_queue_completion :: (IsMTRClusterStateCacheContainer clusterStateCacheContainer, IsNSNumber endpoint, IsNSObject queue) => clusterStateCacheContainer -> endpoint -> queue -> Ptr () -> IO ()
readAttributeClusterRevisionWithClusterStateCache_endpoint_queue_completion clusterStateCacheContainer endpoint queue completion =
  do
    cls' <- getRequiredClass "MTRBaseClusterGroupcast"
    sendClassMessage cls' readAttributeClusterRevisionWithClusterStateCache_endpoint_queue_completionSelector (toMTRClusterStateCacheContainer clusterStateCacheContainer) (toNSNumber endpoint) (toNSObject queue) completion

-- | @- init@
init_ :: IsMTRBaseClusterGroupcast mtrBaseClusterGroupcast => mtrBaseClusterGroupcast -> IO (Id MTRBaseClusterGroupcast)
init_ mtrBaseClusterGroupcast =
  sendOwnedMessage mtrBaseClusterGroupcast initSelector

-- | @+ new@
new :: IO (Id MTRBaseClusterGroupcast)
new  =
  do
    cls' <- getRequiredClass "MTRBaseClusterGroupcast"
    sendOwnedClassMessage cls' newSelector

-- | For all instance methods (reads, writes, commands) that take a completion, the completion will be called on the provided queue.
--
-- ObjC selector: @- initWithDevice:endpointID:queue:@
initWithDevice_endpointID_queue :: (IsMTRBaseClusterGroupcast mtrBaseClusterGroupcast, IsMTRBaseDevice device, IsNSNumber endpointID, IsNSObject queue) => mtrBaseClusterGroupcast -> device -> endpointID -> queue -> IO (Id MTRBaseClusterGroupcast)
initWithDevice_endpointID_queue mtrBaseClusterGroupcast device endpointID queue =
  sendOwnedMessage mtrBaseClusterGroupcast initWithDevice_endpointID_queueSelector (toMTRBaseDevice device) (toNSNumber endpointID) (toNSObject queue)

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @joinGroupWithParams:completion:@
joinGroupWithParams_completionSelector :: Selector '[Id MTRGroupcastClusterJoinGroupParams, Ptr ()] ()
joinGroupWithParams_completionSelector = mkSelector "joinGroupWithParams:completion:"

-- | @Selector@ for @leaveGroupWithParams:completion:@
leaveGroupWithParams_completionSelector :: Selector '[Id MTRGroupcastClusterLeaveGroupParams, Ptr ()] ()
leaveGroupWithParams_completionSelector = mkSelector "leaveGroupWithParams:completion:"

-- | @Selector@ for @updateGroupKeyWithParams:completion:@
updateGroupKeyWithParams_completionSelector :: Selector '[Id MTRGroupcastClusterUpdateGroupKeyParams, Ptr ()] ()
updateGroupKeyWithParams_completionSelector = mkSelector "updateGroupKeyWithParams:completion:"

-- | @Selector@ for @expireGracePeriodWithParams:completion:@
expireGracePeriodWithParams_completionSelector :: Selector '[Id MTRGroupcastClusterExpireGracePeriodParams, Ptr ()] ()
expireGracePeriodWithParams_completionSelector = mkSelector "expireGracePeriodWithParams:completion:"

-- | @Selector@ for @configureAuxiliaryACLWithParams:completion:@
configureAuxiliaryACLWithParams_completionSelector :: Selector '[Id MTRGroupcastClusterConfigureAuxiliaryACLParams, Ptr ()] ()
configureAuxiliaryACLWithParams_completionSelector = mkSelector "configureAuxiliaryACLWithParams:completion:"

-- | @Selector@ for @readAttributeMembershipWithParams:completion:@
readAttributeMembershipWithParams_completionSelector :: Selector '[Id MTRReadParams, Ptr ()] ()
readAttributeMembershipWithParams_completionSelector = mkSelector "readAttributeMembershipWithParams:completion:"

-- | @Selector@ for @subscribeAttributeMembershipWithParams:subscriptionEstablished:reportHandler:@
subscribeAttributeMembershipWithParams_subscriptionEstablished_reportHandlerSelector :: Selector '[Id MTRSubscribeParams, Ptr (), Ptr ()] ()
subscribeAttributeMembershipWithParams_subscriptionEstablished_reportHandlerSelector = mkSelector "subscribeAttributeMembershipWithParams:subscriptionEstablished:reportHandler:"

-- | @Selector@ for @readAttributeMembershipWithClusterStateCache:endpoint:queue:completion:@
readAttributeMembershipWithClusterStateCache_endpoint_queue_completionSelector :: Selector '[Id MTRClusterStateCacheContainer, Id NSNumber, Id NSObject, Ptr ()] ()
readAttributeMembershipWithClusterStateCache_endpoint_queue_completionSelector = mkSelector "readAttributeMembershipWithClusterStateCache:endpoint:queue:completion:"

-- | @Selector@ for @readAttributeMaxMembershipCountWithCompletion:@
readAttributeMaxMembershipCountWithCompletionSelector :: Selector '[Ptr ()] ()
readAttributeMaxMembershipCountWithCompletionSelector = mkSelector "readAttributeMaxMembershipCountWithCompletion:"

-- | @Selector@ for @subscribeAttributeMaxMembershipCountWithParams:subscriptionEstablished:reportHandler:@
subscribeAttributeMaxMembershipCountWithParams_subscriptionEstablished_reportHandlerSelector :: Selector '[Id MTRSubscribeParams, Ptr (), Ptr ()] ()
subscribeAttributeMaxMembershipCountWithParams_subscriptionEstablished_reportHandlerSelector = mkSelector "subscribeAttributeMaxMembershipCountWithParams:subscriptionEstablished:reportHandler:"

-- | @Selector@ for @readAttributeMaxMembershipCountWithClusterStateCache:endpoint:queue:completion:@
readAttributeMaxMembershipCountWithClusterStateCache_endpoint_queue_completionSelector :: Selector '[Id MTRClusterStateCacheContainer, Id NSNumber, Id NSObject, Ptr ()] ()
readAttributeMaxMembershipCountWithClusterStateCache_endpoint_queue_completionSelector = mkSelector "readAttributeMaxMembershipCountWithClusterStateCache:endpoint:queue:completion:"

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
initSelector :: Selector '[] (Id MTRBaseClusterGroupcast)
initSelector = mkSelector "init"

-- | @Selector@ for @new@
newSelector :: Selector '[] (Id MTRBaseClusterGroupcast)
newSelector = mkSelector "new"

-- | @Selector@ for @initWithDevice:endpointID:queue:@
initWithDevice_endpointID_queueSelector :: Selector '[Id MTRBaseDevice, Id NSNumber, Id NSObject] (Id MTRBaseClusterGroupcast)
initWithDevice_endpointID_queueSelector = mkSelector "initWithDevice:endpointID:queue:"

