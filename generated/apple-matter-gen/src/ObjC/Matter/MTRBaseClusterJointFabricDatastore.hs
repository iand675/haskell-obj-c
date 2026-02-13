{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Cluster Joint Fabric Datastore
--
-- The Joint Fabric Datastore Cluster is a cluster that provides a mechanism for the Joint Fabric Administrators to manage the set of Nodes, Groups, and Group membership among Nodes in the Joint Fabric.
--
-- Generated bindings for @MTRBaseClusterJointFabricDatastore@.
module ObjC.Matter.MTRBaseClusterJointFabricDatastore
  ( MTRBaseClusterJointFabricDatastore
  , IsMTRBaseClusterJointFabricDatastore(..)
  , addKeySetWithParams_completion
  , updateKeySetWithParams_completion
  , removeKeySetWithParams_completion
  , addGroupWithParams_completion
  , updateGroupWithParams_completion
  , removeGroupWithParams_completion
  , addAdminWithParams_completion
  , updateAdminWithParams_completion
  , removeAdminWithParams_completion
  , addPendingNodeWithParams_completion
  , refreshNodeWithParams_completion
  , updateNodeWithParams_completion
  , removeNodeWithParams_completion
  , updateEndpointForNodeWithParams_completion
  , addGroupIDToEndpointForNodeWithParams_completion
  , removeGroupIDFromEndpointForNodeWithParams_completion
  , addBindingToEndpointForNodeWithParams_completion
  , removeBindingFromEndpointForNodeWithParams_completion
  , addACLToNodeWithParams_completion
  , removeACLFromNodeWithParams_completion
  , readAttributeAnchorRootCAWithCompletion
  , subscribeAttributeAnchorRootCAWithParams_subscriptionEstablished_reportHandler
  , readAttributeAnchorRootCAWithClusterStateCache_endpoint_queue_completion
  , readAttributeAnchorNodeIDWithCompletion
  , subscribeAttributeAnchorNodeIDWithParams_subscriptionEstablished_reportHandler
  , readAttributeAnchorNodeIDWithClusterStateCache_endpoint_queue_completion
  , readAttributeAnchorVendorIDWithCompletion
  , subscribeAttributeAnchorVendorIDWithParams_subscriptionEstablished_reportHandler
  , readAttributeAnchorVendorIDWithClusterStateCache_endpoint_queue_completion
  , readAttributeFriendlyNameWithCompletion
  , subscribeAttributeFriendlyNameWithParams_subscriptionEstablished_reportHandler
  , readAttributeFriendlyNameWithClusterStateCache_endpoint_queue_completion
  , readAttributeGroupKeySetListWithCompletion
  , subscribeAttributeGroupKeySetListWithParams_subscriptionEstablished_reportHandler
  , readAttributeGroupKeySetListWithClusterStateCache_endpoint_queue_completion
  , readAttributeGroupListWithCompletion
  , subscribeAttributeGroupListWithParams_subscriptionEstablished_reportHandler
  , readAttributeGroupListWithClusterStateCache_endpoint_queue_completion
  , readAttributeNodeListWithCompletion
  , subscribeAttributeNodeListWithParams_subscriptionEstablished_reportHandler
  , readAttributeNodeListWithClusterStateCache_endpoint_queue_completion
  , readAttributeAdminListWithCompletion
  , subscribeAttributeAdminListWithParams_subscriptionEstablished_reportHandler
  , readAttributeAdminListWithClusterStateCache_endpoint_queue_completion
  , readAttributeStatusWithCompletion
  , subscribeAttributeStatusWithParams_subscriptionEstablished_reportHandler
  , readAttributeStatusWithClusterStateCache_endpoint_queue_completion
  , readAttributeEndpointGroupIDListWithCompletion
  , subscribeAttributeEndpointGroupIDListWithParams_subscriptionEstablished_reportHandler
  , readAttributeEndpointGroupIDListWithClusterStateCache_endpoint_queue_completion
  , readAttributeEndpointBindingListWithCompletion
  , subscribeAttributeEndpointBindingListWithParams_subscriptionEstablished_reportHandler
  , readAttributeEndpointBindingListWithClusterStateCache_endpoint_queue_completion
  , readAttributeNodeKeySetListWithCompletion
  , subscribeAttributeNodeKeySetListWithParams_subscriptionEstablished_reportHandler
  , readAttributeNodeKeySetListWithClusterStateCache_endpoint_queue_completion
  , readAttributeNodeACLListWithCompletion
  , subscribeAttributeNodeACLListWithParams_subscriptionEstablished_reportHandler
  , readAttributeNodeACLListWithClusterStateCache_endpoint_queue_completion
  , readAttributeNodeEndpointListWithCompletion
  , subscribeAttributeNodeEndpointListWithParams_subscriptionEstablished_reportHandler
  , readAttributeNodeEndpointListWithClusterStateCache_endpoint_queue_completion
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
  , addACLToNodeWithParams_completionSelector
  , addAdminWithParams_completionSelector
  , addBindingToEndpointForNodeWithParams_completionSelector
  , addGroupIDToEndpointForNodeWithParams_completionSelector
  , addGroupWithParams_completionSelector
  , addKeySetWithParams_completionSelector
  , addPendingNodeWithParams_completionSelector
  , initSelector
  , initWithDevice_endpointID_queueSelector
  , newSelector
  , readAttributeAcceptedCommandListWithClusterStateCache_endpoint_queue_completionSelector
  , readAttributeAcceptedCommandListWithCompletionSelector
  , readAttributeAdminListWithClusterStateCache_endpoint_queue_completionSelector
  , readAttributeAdminListWithCompletionSelector
  , readAttributeAnchorNodeIDWithClusterStateCache_endpoint_queue_completionSelector
  , readAttributeAnchorNodeIDWithCompletionSelector
  , readAttributeAnchorRootCAWithClusterStateCache_endpoint_queue_completionSelector
  , readAttributeAnchorRootCAWithCompletionSelector
  , readAttributeAnchorVendorIDWithClusterStateCache_endpoint_queue_completionSelector
  , readAttributeAnchorVendorIDWithCompletionSelector
  , readAttributeAttributeListWithClusterStateCache_endpoint_queue_completionSelector
  , readAttributeAttributeListWithCompletionSelector
  , readAttributeClusterRevisionWithClusterStateCache_endpoint_queue_completionSelector
  , readAttributeClusterRevisionWithCompletionSelector
  , readAttributeEndpointBindingListWithClusterStateCache_endpoint_queue_completionSelector
  , readAttributeEndpointBindingListWithCompletionSelector
  , readAttributeEndpointGroupIDListWithClusterStateCache_endpoint_queue_completionSelector
  , readAttributeEndpointGroupIDListWithCompletionSelector
  , readAttributeFeatureMapWithClusterStateCache_endpoint_queue_completionSelector
  , readAttributeFeatureMapWithCompletionSelector
  , readAttributeFriendlyNameWithClusterStateCache_endpoint_queue_completionSelector
  , readAttributeFriendlyNameWithCompletionSelector
  , readAttributeGeneratedCommandListWithClusterStateCache_endpoint_queue_completionSelector
  , readAttributeGeneratedCommandListWithCompletionSelector
  , readAttributeGroupKeySetListWithClusterStateCache_endpoint_queue_completionSelector
  , readAttributeGroupKeySetListWithCompletionSelector
  , readAttributeGroupListWithClusterStateCache_endpoint_queue_completionSelector
  , readAttributeGroupListWithCompletionSelector
  , readAttributeNodeACLListWithClusterStateCache_endpoint_queue_completionSelector
  , readAttributeNodeACLListWithCompletionSelector
  , readAttributeNodeEndpointListWithClusterStateCache_endpoint_queue_completionSelector
  , readAttributeNodeEndpointListWithCompletionSelector
  , readAttributeNodeKeySetListWithClusterStateCache_endpoint_queue_completionSelector
  , readAttributeNodeKeySetListWithCompletionSelector
  , readAttributeNodeListWithClusterStateCache_endpoint_queue_completionSelector
  , readAttributeNodeListWithCompletionSelector
  , readAttributeStatusWithClusterStateCache_endpoint_queue_completionSelector
  , readAttributeStatusWithCompletionSelector
  , refreshNodeWithParams_completionSelector
  , removeACLFromNodeWithParams_completionSelector
  , removeAdminWithParams_completionSelector
  , removeBindingFromEndpointForNodeWithParams_completionSelector
  , removeGroupIDFromEndpointForNodeWithParams_completionSelector
  , removeGroupWithParams_completionSelector
  , removeKeySetWithParams_completionSelector
  , removeNodeWithParams_completionSelector
  , subscribeAttributeAcceptedCommandListWithParams_subscriptionEstablished_reportHandlerSelector
  , subscribeAttributeAdminListWithParams_subscriptionEstablished_reportHandlerSelector
  , subscribeAttributeAnchorNodeIDWithParams_subscriptionEstablished_reportHandlerSelector
  , subscribeAttributeAnchorRootCAWithParams_subscriptionEstablished_reportHandlerSelector
  , subscribeAttributeAnchorVendorIDWithParams_subscriptionEstablished_reportHandlerSelector
  , subscribeAttributeAttributeListWithParams_subscriptionEstablished_reportHandlerSelector
  , subscribeAttributeClusterRevisionWithParams_subscriptionEstablished_reportHandlerSelector
  , subscribeAttributeEndpointBindingListWithParams_subscriptionEstablished_reportHandlerSelector
  , subscribeAttributeEndpointGroupIDListWithParams_subscriptionEstablished_reportHandlerSelector
  , subscribeAttributeFeatureMapWithParams_subscriptionEstablished_reportHandlerSelector
  , subscribeAttributeFriendlyNameWithParams_subscriptionEstablished_reportHandlerSelector
  , subscribeAttributeGeneratedCommandListWithParams_subscriptionEstablished_reportHandlerSelector
  , subscribeAttributeGroupKeySetListWithParams_subscriptionEstablished_reportHandlerSelector
  , subscribeAttributeGroupListWithParams_subscriptionEstablished_reportHandlerSelector
  , subscribeAttributeNodeACLListWithParams_subscriptionEstablished_reportHandlerSelector
  , subscribeAttributeNodeEndpointListWithParams_subscriptionEstablished_reportHandlerSelector
  , subscribeAttributeNodeKeySetListWithParams_subscriptionEstablished_reportHandlerSelector
  , subscribeAttributeNodeListWithParams_subscriptionEstablished_reportHandlerSelector
  , subscribeAttributeStatusWithParams_subscriptionEstablished_reportHandlerSelector
  , updateAdminWithParams_completionSelector
  , updateEndpointForNodeWithParams_completionSelector
  , updateGroupWithParams_completionSelector
  , updateKeySetWithParams_completionSelector
  , updateNodeWithParams_completionSelector


  ) where

import Foreign.Ptr (Ptr, FunPtr)
import Foreign.C.Types

import ObjC.Runtime.Types
import ObjC.Runtime.Message (sendMessage, sendOwnedMessage, sendClassMessage, sendOwnedClassMessage)
import ObjC.Runtime.Selector (mkSelector)
import ObjC.Runtime.Class (getRequiredClass)

import ObjC.Matter.Internal.Classes
import ObjC.Foundation.Internal.Classes

-- | Command AddKeySet
--
-- This command SHALL be used to add a KeySet to the Joint Fabric Datastore Cluster of the accessing fabric.
--
-- ObjC selector: @- addKeySetWithParams:completion:@
addKeySetWithParams_completion :: (IsMTRBaseClusterJointFabricDatastore mtrBaseClusterJointFabricDatastore, IsMTRJointFabricDatastoreClusterAddKeySetParams params) => mtrBaseClusterJointFabricDatastore -> params -> Ptr () -> IO ()
addKeySetWithParams_completion mtrBaseClusterJointFabricDatastore params completion =
  sendMessage mtrBaseClusterJointFabricDatastore addKeySetWithParams_completionSelector (toMTRJointFabricDatastoreClusterAddKeySetParams params) completion

-- | Command UpdateKeySet
--
-- This command SHALL be used to update a KeySet in the Joint Fabric Datastore Cluster of the accessing fabric.
--
-- ObjC selector: @- updateKeySetWithParams:completion:@
updateKeySetWithParams_completion :: (IsMTRBaseClusterJointFabricDatastore mtrBaseClusterJointFabricDatastore, IsMTRJointFabricDatastoreClusterUpdateKeySetParams params) => mtrBaseClusterJointFabricDatastore -> params -> Ptr () -> IO ()
updateKeySetWithParams_completion mtrBaseClusterJointFabricDatastore params completion =
  sendMessage mtrBaseClusterJointFabricDatastore updateKeySetWithParams_completionSelector (toMTRJointFabricDatastoreClusterUpdateKeySetParams params) completion

-- | Command RemoveKeySet
--
-- This command SHALL be used to remove a KeySet from the Joint Fabric Datastore Cluster of the accessing fabric.
--
-- ObjC selector: @- removeKeySetWithParams:completion:@
removeKeySetWithParams_completion :: (IsMTRBaseClusterJointFabricDatastore mtrBaseClusterJointFabricDatastore, IsMTRJointFabricDatastoreClusterRemoveKeySetParams params) => mtrBaseClusterJointFabricDatastore -> params -> Ptr () -> IO ()
removeKeySetWithParams_completion mtrBaseClusterJointFabricDatastore params completion =
  sendMessage mtrBaseClusterJointFabricDatastore removeKeySetWithParams_completionSelector (toMTRJointFabricDatastoreClusterRemoveKeySetParams params) completion

-- | Command AddGroup
--
-- This command SHALL be used to add a group to the Joint Fabric Datastore Cluster of the accessing fabric.
--
-- ObjC selector: @- addGroupWithParams:completion:@
addGroupWithParams_completion :: (IsMTRBaseClusterJointFabricDatastore mtrBaseClusterJointFabricDatastore, IsMTRJointFabricDatastoreClusterAddGroupParams params) => mtrBaseClusterJointFabricDatastore -> params -> Ptr () -> IO ()
addGroupWithParams_completion mtrBaseClusterJointFabricDatastore params completion =
  sendMessage mtrBaseClusterJointFabricDatastore addGroupWithParams_completionSelector (toMTRJointFabricDatastoreClusterAddGroupParams params) completion

-- | Command UpdateGroup
--
-- This command SHALL be used to update a group in the Joint Fabric Datastore Cluster of the accessing fabric.
--
-- ObjC selector: @- updateGroupWithParams:completion:@
updateGroupWithParams_completion :: (IsMTRBaseClusterJointFabricDatastore mtrBaseClusterJointFabricDatastore, IsMTRJointFabricDatastoreClusterUpdateGroupParams params) => mtrBaseClusterJointFabricDatastore -> params -> Ptr () -> IO ()
updateGroupWithParams_completion mtrBaseClusterJointFabricDatastore params completion =
  sendMessage mtrBaseClusterJointFabricDatastore updateGroupWithParams_completionSelector (toMTRJointFabricDatastoreClusterUpdateGroupParams params) completion

-- | Command RemoveGroup
--
-- This command SHALL be used to remove a group from the Joint Fabric Datastore Cluster of the accessing fabric.
--
-- ObjC selector: @- removeGroupWithParams:completion:@
removeGroupWithParams_completion :: (IsMTRBaseClusterJointFabricDatastore mtrBaseClusterJointFabricDatastore, IsMTRJointFabricDatastoreClusterRemoveGroupParams params) => mtrBaseClusterJointFabricDatastore -> params -> Ptr () -> IO ()
removeGroupWithParams_completion mtrBaseClusterJointFabricDatastore params completion =
  sendMessage mtrBaseClusterJointFabricDatastore removeGroupWithParams_completionSelector (toMTRJointFabricDatastoreClusterRemoveGroupParams params) completion

-- | Command AddAdmin
--
-- This command SHALL be used to add an admin to the Joint Fabric Datastore Cluster of the accessing fabric.
--
-- ObjC selector: @- addAdminWithParams:completion:@
addAdminWithParams_completion :: (IsMTRBaseClusterJointFabricDatastore mtrBaseClusterJointFabricDatastore, IsMTRJointFabricDatastoreClusterAddAdminParams params) => mtrBaseClusterJointFabricDatastore -> params -> Ptr () -> IO ()
addAdminWithParams_completion mtrBaseClusterJointFabricDatastore params completion =
  sendMessage mtrBaseClusterJointFabricDatastore addAdminWithParams_completionSelector (toMTRJointFabricDatastoreClusterAddAdminParams params) completion

-- | Command UpdateAdmin
--
-- This command SHALL be used to update an admin in the Joint Fabric Datastore Cluster of the accessing fabric.
--
-- ObjC selector: @- updateAdminWithParams:completion:@
updateAdminWithParams_completion :: (IsMTRBaseClusterJointFabricDatastore mtrBaseClusterJointFabricDatastore, IsMTRJointFabricDatastoreClusterUpdateAdminParams params) => mtrBaseClusterJointFabricDatastore -> params -> Ptr () -> IO ()
updateAdminWithParams_completion mtrBaseClusterJointFabricDatastore params completion =
  sendMessage mtrBaseClusterJointFabricDatastore updateAdminWithParams_completionSelector (toMTRJointFabricDatastoreClusterUpdateAdminParams params) completion

-- | Command RemoveAdmin
--
-- This command SHALL be used to remove an admin from the Joint Fabric Datastore Cluster of the accessing fabric.
--
-- ObjC selector: @- removeAdminWithParams:completion:@
removeAdminWithParams_completion :: (IsMTRBaseClusterJointFabricDatastore mtrBaseClusterJointFabricDatastore, IsMTRJointFabricDatastoreClusterRemoveAdminParams params) => mtrBaseClusterJointFabricDatastore -> params -> Ptr () -> IO ()
removeAdminWithParams_completion mtrBaseClusterJointFabricDatastore params completion =
  sendMessage mtrBaseClusterJointFabricDatastore removeAdminWithParams_completionSelector (toMTRJointFabricDatastoreClusterRemoveAdminParams params) completion

-- | Command AddPendingNode
--
-- The command SHALL be used to add a node to the Joint Fabric Datastore Cluster of the accessing fabric.
--
-- ObjC selector: @- addPendingNodeWithParams:completion:@
addPendingNodeWithParams_completion :: (IsMTRBaseClusterJointFabricDatastore mtrBaseClusterJointFabricDatastore, IsMTRJointFabricDatastoreClusterAddPendingNodeParams params) => mtrBaseClusterJointFabricDatastore -> params -> Ptr () -> IO ()
addPendingNodeWithParams_completion mtrBaseClusterJointFabricDatastore params completion =
  sendMessage mtrBaseClusterJointFabricDatastore addPendingNodeWithParams_completionSelector (toMTRJointFabricDatastoreClusterAddPendingNodeParams params) completion

-- | Command RefreshNode
--
-- The command SHALL be used to request that Datastore information relating to a Node of the accessing fabric is refreshed.
--
-- ObjC selector: @- refreshNodeWithParams:completion:@
refreshNodeWithParams_completion :: (IsMTRBaseClusterJointFabricDatastore mtrBaseClusterJointFabricDatastore, IsMTRJointFabricDatastoreClusterRefreshNodeParams params) => mtrBaseClusterJointFabricDatastore -> params -> Ptr () -> IO ()
refreshNodeWithParams_completion mtrBaseClusterJointFabricDatastore params completion =
  sendMessage mtrBaseClusterJointFabricDatastore refreshNodeWithParams_completionSelector (toMTRJointFabricDatastoreClusterRefreshNodeParams params) completion

-- | Command UpdateNode
--
-- The command SHALL be used to update the friendly name for a node in the Joint Fabric Datastore Cluster of the accessing fabric.
--
-- ObjC selector: @- updateNodeWithParams:completion:@
updateNodeWithParams_completion :: (IsMTRBaseClusterJointFabricDatastore mtrBaseClusterJointFabricDatastore, IsMTRJointFabricDatastoreClusterUpdateNodeParams params) => mtrBaseClusterJointFabricDatastore -> params -> Ptr () -> IO ()
updateNodeWithParams_completion mtrBaseClusterJointFabricDatastore params completion =
  sendMessage mtrBaseClusterJointFabricDatastore updateNodeWithParams_completionSelector (toMTRJointFabricDatastoreClusterUpdateNodeParams params) completion

-- | Command RemoveNode
--
-- This command SHALL be used to remove a node from the Joint Fabric Datastore Cluster of the accessing fabric.
--
-- ObjC selector: @- removeNodeWithParams:completion:@
removeNodeWithParams_completion :: (IsMTRBaseClusterJointFabricDatastore mtrBaseClusterJointFabricDatastore, IsMTRJointFabricDatastoreClusterRemoveNodeParams params) => mtrBaseClusterJointFabricDatastore -> params -> Ptr () -> IO ()
removeNodeWithParams_completion mtrBaseClusterJointFabricDatastore params completion =
  sendMessage mtrBaseClusterJointFabricDatastore removeNodeWithParams_completionSelector (toMTRJointFabricDatastoreClusterRemoveNodeParams params) completion

-- | Command UpdateEndpointForNode
--
-- This command SHALL be used to update the state of an endpoint for a node in the Joint Fabric Datastore Cluster of the accessing fabric.
--
-- ObjC selector: @- updateEndpointForNodeWithParams:completion:@
updateEndpointForNodeWithParams_completion :: (IsMTRBaseClusterJointFabricDatastore mtrBaseClusterJointFabricDatastore, IsMTRJointFabricDatastoreClusterUpdateEndpointForNodeParams params) => mtrBaseClusterJointFabricDatastore -> params -> Ptr () -> IO ()
updateEndpointForNodeWithParams_completion mtrBaseClusterJointFabricDatastore params completion =
  sendMessage mtrBaseClusterJointFabricDatastore updateEndpointForNodeWithParams_completionSelector (toMTRJointFabricDatastoreClusterUpdateEndpointForNodeParams params) completion

-- | Command AddGroupIDToEndpointForNode
--
-- This command SHALL be used to add a Group ID to an endpoint for a node in the Joint Fabric Datastore Cluster of the accessing fabric.
--
-- ObjC selector: @- addGroupIDToEndpointForNodeWithParams:completion:@
addGroupIDToEndpointForNodeWithParams_completion :: (IsMTRBaseClusterJointFabricDatastore mtrBaseClusterJointFabricDatastore, IsMTRJointFabricDatastoreClusterAddGroupIDToEndpointForNodeParams params) => mtrBaseClusterJointFabricDatastore -> params -> Ptr () -> IO ()
addGroupIDToEndpointForNodeWithParams_completion mtrBaseClusterJointFabricDatastore params completion =
  sendMessage mtrBaseClusterJointFabricDatastore addGroupIDToEndpointForNodeWithParams_completionSelector (toMTRJointFabricDatastoreClusterAddGroupIDToEndpointForNodeParams params) completion

-- | Command RemoveGroupIDFromEndpointForNode
--
-- This command SHALL be used to remove a Group ID from an endpoint for a node in the Joint Fabric Datastore Cluster of the accessing fabric.
--
-- ObjC selector: @- removeGroupIDFromEndpointForNodeWithParams:completion:@
removeGroupIDFromEndpointForNodeWithParams_completion :: (IsMTRBaseClusterJointFabricDatastore mtrBaseClusterJointFabricDatastore, IsMTRJointFabricDatastoreClusterRemoveGroupIDFromEndpointForNodeParams params) => mtrBaseClusterJointFabricDatastore -> params -> Ptr () -> IO ()
removeGroupIDFromEndpointForNodeWithParams_completion mtrBaseClusterJointFabricDatastore params completion =
  sendMessage mtrBaseClusterJointFabricDatastore removeGroupIDFromEndpointForNodeWithParams_completionSelector (toMTRJointFabricDatastoreClusterRemoveGroupIDFromEndpointForNodeParams params) completion

-- | Command AddBindingToEndpointForNode
--
-- This command SHALL be used to add a binding to an endpoint for a node in the Joint Fabric Datastore Cluster of the accessing fabric.
--
-- ObjC selector: @- addBindingToEndpointForNodeWithParams:completion:@
addBindingToEndpointForNodeWithParams_completion :: (IsMTRBaseClusterJointFabricDatastore mtrBaseClusterJointFabricDatastore, IsMTRJointFabricDatastoreClusterAddBindingToEndpointForNodeParams params) => mtrBaseClusterJointFabricDatastore -> params -> Ptr () -> IO ()
addBindingToEndpointForNodeWithParams_completion mtrBaseClusterJointFabricDatastore params completion =
  sendMessage mtrBaseClusterJointFabricDatastore addBindingToEndpointForNodeWithParams_completionSelector (toMTRJointFabricDatastoreClusterAddBindingToEndpointForNodeParams params) completion

-- | Command RemoveBindingFromEndpointForNode
--
-- This command SHALL be used to remove a binding from an endpoint for a node in the Joint Fabric Datastore Cluster of the accessing fabric.
--
-- ObjC selector: @- removeBindingFromEndpointForNodeWithParams:completion:@
removeBindingFromEndpointForNodeWithParams_completion :: (IsMTRBaseClusterJointFabricDatastore mtrBaseClusterJointFabricDatastore, IsMTRJointFabricDatastoreClusterRemoveBindingFromEndpointForNodeParams params) => mtrBaseClusterJointFabricDatastore -> params -> Ptr () -> IO ()
removeBindingFromEndpointForNodeWithParams_completion mtrBaseClusterJointFabricDatastore params completion =
  sendMessage mtrBaseClusterJointFabricDatastore removeBindingFromEndpointForNodeWithParams_completionSelector (toMTRJointFabricDatastoreClusterRemoveBindingFromEndpointForNodeParams params) completion

-- | Command AddACLToNode
--
-- This command SHALL be used to add an ACL to a node in the Joint Fabric Datastore Cluster of the accessing fabric.
--
-- ObjC selector: @- addACLToNodeWithParams:completion:@
addACLToNodeWithParams_completion :: (IsMTRBaseClusterJointFabricDatastore mtrBaseClusterJointFabricDatastore, IsMTRJointFabricDatastoreClusterAddACLToNodeParams params) => mtrBaseClusterJointFabricDatastore -> params -> Ptr () -> IO ()
addACLToNodeWithParams_completion mtrBaseClusterJointFabricDatastore params completion =
  sendMessage mtrBaseClusterJointFabricDatastore addACLToNodeWithParams_completionSelector (toMTRJointFabricDatastoreClusterAddACLToNodeParams params) completion

-- | Command RemoveACLFromNode
--
-- This command SHALL be used to remove an ACL from a node in the Joint Fabric Datastore Cluster of the accessing fabric.
--
-- ObjC selector: @- removeACLFromNodeWithParams:completion:@
removeACLFromNodeWithParams_completion :: (IsMTRBaseClusterJointFabricDatastore mtrBaseClusterJointFabricDatastore, IsMTRJointFabricDatastoreClusterRemoveACLFromNodeParams params) => mtrBaseClusterJointFabricDatastore -> params -> Ptr () -> IO ()
removeACLFromNodeWithParams_completion mtrBaseClusterJointFabricDatastore params completion =
  sendMessage mtrBaseClusterJointFabricDatastore removeACLFromNodeWithParams_completionSelector (toMTRJointFabricDatastoreClusterRemoveACLFromNodeParams params) completion

-- | @- readAttributeAnchorRootCAWithCompletion:@
readAttributeAnchorRootCAWithCompletion :: IsMTRBaseClusterJointFabricDatastore mtrBaseClusterJointFabricDatastore => mtrBaseClusterJointFabricDatastore -> Ptr () -> IO ()
readAttributeAnchorRootCAWithCompletion mtrBaseClusterJointFabricDatastore completion =
  sendMessage mtrBaseClusterJointFabricDatastore readAttributeAnchorRootCAWithCompletionSelector completion

-- | @- subscribeAttributeAnchorRootCAWithParams:subscriptionEstablished:reportHandler:@
subscribeAttributeAnchorRootCAWithParams_subscriptionEstablished_reportHandler :: (IsMTRBaseClusterJointFabricDatastore mtrBaseClusterJointFabricDatastore, IsMTRSubscribeParams params) => mtrBaseClusterJointFabricDatastore -> params -> Ptr () -> Ptr () -> IO ()
subscribeAttributeAnchorRootCAWithParams_subscriptionEstablished_reportHandler mtrBaseClusterJointFabricDatastore params subscriptionEstablished reportHandler =
  sendMessage mtrBaseClusterJointFabricDatastore subscribeAttributeAnchorRootCAWithParams_subscriptionEstablished_reportHandlerSelector (toMTRSubscribeParams params) subscriptionEstablished reportHandler

-- | @+ readAttributeAnchorRootCAWithClusterStateCache:endpoint:queue:completion:@
readAttributeAnchorRootCAWithClusterStateCache_endpoint_queue_completion :: (IsMTRClusterStateCacheContainer clusterStateCacheContainer, IsNSNumber endpoint, IsNSObject queue) => clusterStateCacheContainer -> endpoint -> queue -> Ptr () -> IO ()
readAttributeAnchorRootCAWithClusterStateCache_endpoint_queue_completion clusterStateCacheContainer endpoint queue completion =
  do
    cls' <- getRequiredClass "MTRBaseClusterJointFabricDatastore"
    sendClassMessage cls' readAttributeAnchorRootCAWithClusterStateCache_endpoint_queue_completionSelector (toMTRClusterStateCacheContainer clusterStateCacheContainer) (toNSNumber endpoint) (toNSObject queue) completion

-- | @- readAttributeAnchorNodeIDWithCompletion:@
readAttributeAnchorNodeIDWithCompletion :: IsMTRBaseClusterJointFabricDatastore mtrBaseClusterJointFabricDatastore => mtrBaseClusterJointFabricDatastore -> Ptr () -> IO ()
readAttributeAnchorNodeIDWithCompletion mtrBaseClusterJointFabricDatastore completion =
  sendMessage mtrBaseClusterJointFabricDatastore readAttributeAnchorNodeIDWithCompletionSelector completion

-- | @- subscribeAttributeAnchorNodeIDWithParams:subscriptionEstablished:reportHandler:@
subscribeAttributeAnchorNodeIDWithParams_subscriptionEstablished_reportHandler :: (IsMTRBaseClusterJointFabricDatastore mtrBaseClusterJointFabricDatastore, IsMTRSubscribeParams params) => mtrBaseClusterJointFabricDatastore -> params -> Ptr () -> Ptr () -> IO ()
subscribeAttributeAnchorNodeIDWithParams_subscriptionEstablished_reportHandler mtrBaseClusterJointFabricDatastore params subscriptionEstablished reportHandler =
  sendMessage mtrBaseClusterJointFabricDatastore subscribeAttributeAnchorNodeIDWithParams_subscriptionEstablished_reportHandlerSelector (toMTRSubscribeParams params) subscriptionEstablished reportHandler

-- | @+ readAttributeAnchorNodeIDWithClusterStateCache:endpoint:queue:completion:@
readAttributeAnchorNodeIDWithClusterStateCache_endpoint_queue_completion :: (IsMTRClusterStateCacheContainer clusterStateCacheContainer, IsNSNumber endpoint, IsNSObject queue) => clusterStateCacheContainer -> endpoint -> queue -> Ptr () -> IO ()
readAttributeAnchorNodeIDWithClusterStateCache_endpoint_queue_completion clusterStateCacheContainer endpoint queue completion =
  do
    cls' <- getRequiredClass "MTRBaseClusterJointFabricDatastore"
    sendClassMessage cls' readAttributeAnchorNodeIDWithClusterStateCache_endpoint_queue_completionSelector (toMTRClusterStateCacheContainer clusterStateCacheContainer) (toNSNumber endpoint) (toNSObject queue) completion

-- | @- readAttributeAnchorVendorIDWithCompletion:@
readAttributeAnchorVendorIDWithCompletion :: IsMTRBaseClusterJointFabricDatastore mtrBaseClusterJointFabricDatastore => mtrBaseClusterJointFabricDatastore -> Ptr () -> IO ()
readAttributeAnchorVendorIDWithCompletion mtrBaseClusterJointFabricDatastore completion =
  sendMessage mtrBaseClusterJointFabricDatastore readAttributeAnchorVendorIDWithCompletionSelector completion

-- | @- subscribeAttributeAnchorVendorIDWithParams:subscriptionEstablished:reportHandler:@
subscribeAttributeAnchorVendorIDWithParams_subscriptionEstablished_reportHandler :: (IsMTRBaseClusterJointFabricDatastore mtrBaseClusterJointFabricDatastore, IsMTRSubscribeParams params) => mtrBaseClusterJointFabricDatastore -> params -> Ptr () -> Ptr () -> IO ()
subscribeAttributeAnchorVendorIDWithParams_subscriptionEstablished_reportHandler mtrBaseClusterJointFabricDatastore params subscriptionEstablished reportHandler =
  sendMessage mtrBaseClusterJointFabricDatastore subscribeAttributeAnchorVendorIDWithParams_subscriptionEstablished_reportHandlerSelector (toMTRSubscribeParams params) subscriptionEstablished reportHandler

-- | @+ readAttributeAnchorVendorIDWithClusterStateCache:endpoint:queue:completion:@
readAttributeAnchorVendorIDWithClusterStateCache_endpoint_queue_completion :: (IsMTRClusterStateCacheContainer clusterStateCacheContainer, IsNSNumber endpoint, IsNSObject queue) => clusterStateCacheContainer -> endpoint -> queue -> Ptr () -> IO ()
readAttributeAnchorVendorIDWithClusterStateCache_endpoint_queue_completion clusterStateCacheContainer endpoint queue completion =
  do
    cls' <- getRequiredClass "MTRBaseClusterJointFabricDatastore"
    sendClassMessage cls' readAttributeAnchorVendorIDWithClusterStateCache_endpoint_queue_completionSelector (toMTRClusterStateCacheContainer clusterStateCacheContainer) (toNSNumber endpoint) (toNSObject queue) completion

-- | @- readAttributeFriendlyNameWithCompletion:@
readAttributeFriendlyNameWithCompletion :: IsMTRBaseClusterJointFabricDatastore mtrBaseClusterJointFabricDatastore => mtrBaseClusterJointFabricDatastore -> Ptr () -> IO ()
readAttributeFriendlyNameWithCompletion mtrBaseClusterJointFabricDatastore completion =
  sendMessage mtrBaseClusterJointFabricDatastore readAttributeFriendlyNameWithCompletionSelector completion

-- | @- subscribeAttributeFriendlyNameWithParams:subscriptionEstablished:reportHandler:@
subscribeAttributeFriendlyNameWithParams_subscriptionEstablished_reportHandler :: (IsMTRBaseClusterJointFabricDatastore mtrBaseClusterJointFabricDatastore, IsMTRSubscribeParams params) => mtrBaseClusterJointFabricDatastore -> params -> Ptr () -> Ptr () -> IO ()
subscribeAttributeFriendlyNameWithParams_subscriptionEstablished_reportHandler mtrBaseClusterJointFabricDatastore params subscriptionEstablished reportHandler =
  sendMessage mtrBaseClusterJointFabricDatastore subscribeAttributeFriendlyNameWithParams_subscriptionEstablished_reportHandlerSelector (toMTRSubscribeParams params) subscriptionEstablished reportHandler

-- | @+ readAttributeFriendlyNameWithClusterStateCache:endpoint:queue:completion:@
readAttributeFriendlyNameWithClusterStateCache_endpoint_queue_completion :: (IsMTRClusterStateCacheContainer clusterStateCacheContainer, IsNSNumber endpoint, IsNSObject queue) => clusterStateCacheContainer -> endpoint -> queue -> Ptr () -> IO ()
readAttributeFriendlyNameWithClusterStateCache_endpoint_queue_completion clusterStateCacheContainer endpoint queue completion =
  do
    cls' <- getRequiredClass "MTRBaseClusterJointFabricDatastore"
    sendClassMessage cls' readAttributeFriendlyNameWithClusterStateCache_endpoint_queue_completionSelector (toMTRClusterStateCacheContainer clusterStateCacheContainer) (toNSNumber endpoint) (toNSObject queue) completion

-- | @- readAttributeGroupKeySetListWithCompletion:@
readAttributeGroupKeySetListWithCompletion :: IsMTRBaseClusterJointFabricDatastore mtrBaseClusterJointFabricDatastore => mtrBaseClusterJointFabricDatastore -> Ptr () -> IO ()
readAttributeGroupKeySetListWithCompletion mtrBaseClusterJointFabricDatastore completion =
  sendMessage mtrBaseClusterJointFabricDatastore readAttributeGroupKeySetListWithCompletionSelector completion

-- | @- subscribeAttributeGroupKeySetListWithParams:subscriptionEstablished:reportHandler:@
subscribeAttributeGroupKeySetListWithParams_subscriptionEstablished_reportHandler :: (IsMTRBaseClusterJointFabricDatastore mtrBaseClusterJointFabricDatastore, IsMTRSubscribeParams params) => mtrBaseClusterJointFabricDatastore -> params -> Ptr () -> Ptr () -> IO ()
subscribeAttributeGroupKeySetListWithParams_subscriptionEstablished_reportHandler mtrBaseClusterJointFabricDatastore params subscriptionEstablished reportHandler =
  sendMessage mtrBaseClusterJointFabricDatastore subscribeAttributeGroupKeySetListWithParams_subscriptionEstablished_reportHandlerSelector (toMTRSubscribeParams params) subscriptionEstablished reportHandler

-- | @+ readAttributeGroupKeySetListWithClusterStateCache:endpoint:queue:completion:@
readAttributeGroupKeySetListWithClusterStateCache_endpoint_queue_completion :: (IsMTRClusterStateCacheContainer clusterStateCacheContainer, IsNSNumber endpoint, IsNSObject queue) => clusterStateCacheContainer -> endpoint -> queue -> Ptr () -> IO ()
readAttributeGroupKeySetListWithClusterStateCache_endpoint_queue_completion clusterStateCacheContainer endpoint queue completion =
  do
    cls' <- getRequiredClass "MTRBaseClusterJointFabricDatastore"
    sendClassMessage cls' readAttributeGroupKeySetListWithClusterStateCache_endpoint_queue_completionSelector (toMTRClusterStateCacheContainer clusterStateCacheContainer) (toNSNumber endpoint) (toNSObject queue) completion

-- | @- readAttributeGroupListWithCompletion:@
readAttributeGroupListWithCompletion :: IsMTRBaseClusterJointFabricDatastore mtrBaseClusterJointFabricDatastore => mtrBaseClusterJointFabricDatastore -> Ptr () -> IO ()
readAttributeGroupListWithCompletion mtrBaseClusterJointFabricDatastore completion =
  sendMessage mtrBaseClusterJointFabricDatastore readAttributeGroupListWithCompletionSelector completion

-- | @- subscribeAttributeGroupListWithParams:subscriptionEstablished:reportHandler:@
subscribeAttributeGroupListWithParams_subscriptionEstablished_reportHandler :: (IsMTRBaseClusterJointFabricDatastore mtrBaseClusterJointFabricDatastore, IsMTRSubscribeParams params) => mtrBaseClusterJointFabricDatastore -> params -> Ptr () -> Ptr () -> IO ()
subscribeAttributeGroupListWithParams_subscriptionEstablished_reportHandler mtrBaseClusterJointFabricDatastore params subscriptionEstablished reportHandler =
  sendMessage mtrBaseClusterJointFabricDatastore subscribeAttributeGroupListWithParams_subscriptionEstablished_reportHandlerSelector (toMTRSubscribeParams params) subscriptionEstablished reportHandler

-- | @+ readAttributeGroupListWithClusterStateCache:endpoint:queue:completion:@
readAttributeGroupListWithClusterStateCache_endpoint_queue_completion :: (IsMTRClusterStateCacheContainer clusterStateCacheContainer, IsNSNumber endpoint, IsNSObject queue) => clusterStateCacheContainer -> endpoint -> queue -> Ptr () -> IO ()
readAttributeGroupListWithClusterStateCache_endpoint_queue_completion clusterStateCacheContainer endpoint queue completion =
  do
    cls' <- getRequiredClass "MTRBaseClusterJointFabricDatastore"
    sendClassMessage cls' readAttributeGroupListWithClusterStateCache_endpoint_queue_completionSelector (toMTRClusterStateCacheContainer clusterStateCacheContainer) (toNSNumber endpoint) (toNSObject queue) completion

-- | @- readAttributeNodeListWithCompletion:@
readAttributeNodeListWithCompletion :: IsMTRBaseClusterJointFabricDatastore mtrBaseClusterJointFabricDatastore => mtrBaseClusterJointFabricDatastore -> Ptr () -> IO ()
readAttributeNodeListWithCompletion mtrBaseClusterJointFabricDatastore completion =
  sendMessage mtrBaseClusterJointFabricDatastore readAttributeNodeListWithCompletionSelector completion

-- | @- subscribeAttributeNodeListWithParams:subscriptionEstablished:reportHandler:@
subscribeAttributeNodeListWithParams_subscriptionEstablished_reportHandler :: (IsMTRBaseClusterJointFabricDatastore mtrBaseClusterJointFabricDatastore, IsMTRSubscribeParams params) => mtrBaseClusterJointFabricDatastore -> params -> Ptr () -> Ptr () -> IO ()
subscribeAttributeNodeListWithParams_subscriptionEstablished_reportHandler mtrBaseClusterJointFabricDatastore params subscriptionEstablished reportHandler =
  sendMessage mtrBaseClusterJointFabricDatastore subscribeAttributeNodeListWithParams_subscriptionEstablished_reportHandlerSelector (toMTRSubscribeParams params) subscriptionEstablished reportHandler

-- | @+ readAttributeNodeListWithClusterStateCache:endpoint:queue:completion:@
readAttributeNodeListWithClusterStateCache_endpoint_queue_completion :: (IsMTRClusterStateCacheContainer clusterStateCacheContainer, IsNSNumber endpoint, IsNSObject queue) => clusterStateCacheContainer -> endpoint -> queue -> Ptr () -> IO ()
readAttributeNodeListWithClusterStateCache_endpoint_queue_completion clusterStateCacheContainer endpoint queue completion =
  do
    cls' <- getRequiredClass "MTRBaseClusterJointFabricDatastore"
    sendClassMessage cls' readAttributeNodeListWithClusterStateCache_endpoint_queue_completionSelector (toMTRClusterStateCacheContainer clusterStateCacheContainer) (toNSNumber endpoint) (toNSObject queue) completion

-- | @- readAttributeAdminListWithCompletion:@
readAttributeAdminListWithCompletion :: IsMTRBaseClusterJointFabricDatastore mtrBaseClusterJointFabricDatastore => mtrBaseClusterJointFabricDatastore -> Ptr () -> IO ()
readAttributeAdminListWithCompletion mtrBaseClusterJointFabricDatastore completion =
  sendMessage mtrBaseClusterJointFabricDatastore readAttributeAdminListWithCompletionSelector completion

-- | @- subscribeAttributeAdminListWithParams:subscriptionEstablished:reportHandler:@
subscribeAttributeAdminListWithParams_subscriptionEstablished_reportHandler :: (IsMTRBaseClusterJointFabricDatastore mtrBaseClusterJointFabricDatastore, IsMTRSubscribeParams params) => mtrBaseClusterJointFabricDatastore -> params -> Ptr () -> Ptr () -> IO ()
subscribeAttributeAdminListWithParams_subscriptionEstablished_reportHandler mtrBaseClusterJointFabricDatastore params subscriptionEstablished reportHandler =
  sendMessage mtrBaseClusterJointFabricDatastore subscribeAttributeAdminListWithParams_subscriptionEstablished_reportHandlerSelector (toMTRSubscribeParams params) subscriptionEstablished reportHandler

-- | @+ readAttributeAdminListWithClusterStateCache:endpoint:queue:completion:@
readAttributeAdminListWithClusterStateCache_endpoint_queue_completion :: (IsMTRClusterStateCacheContainer clusterStateCacheContainer, IsNSNumber endpoint, IsNSObject queue) => clusterStateCacheContainer -> endpoint -> queue -> Ptr () -> IO ()
readAttributeAdminListWithClusterStateCache_endpoint_queue_completion clusterStateCacheContainer endpoint queue completion =
  do
    cls' <- getRequiredClass "MTRBaseClusterJointFabricDatastore"
    sendClassMessage cls' readAttributeAdminListWithClusterStateCache_endpoint_queue_completionSelector (toMTRClusterStateCacheContainer clusterStateCacheContainer) (toNSNumber endpoint) (toNSObject queue) completion

-- | @- readAttributeStatusWithCompletion:@
readAttributeStatusWithCompletion :: IsMTRBaseClusterJointFabricDatastore mtrBaseClusterJointFabricDatastore => mtrBaseClusterJointFabricDatastore -> Ptr () -> IO ()
readAttributeStatusWithCompletion mtrBaseClusterJointFabricDatastore completion =
  sendMessage mtrBaseClusterJointFabricDatastore readAttributeStatusWithCompletionSelector completion

-- | @- subscribeAttributeStatusWithParams:subscriptionEstablished:reportHandler:@
subscribeAttributeStatusWithParams_subscriptionEstablished_reportHandler :: (IsMTRBaseClusterJointFabricDatastore mtrBaseClusterJointFabricDatastore, IsMTRSubscribeParams params) => mtrBaseClusterJointFabricDatastore -> params -> Ptr () -> Ptr () -> IO ()
subscribeAttributeStatusWithParams_subscriptionEstablished_reportHandler mtrBaseClusterJointFabricDatastore params subscriptionEstablished reportHandler =
  sendMessage mtrBaseClusterJointFabricDatastore subscribeAttributeStatusWithParams_subscriptionEstablished_reportHandlerSelector (toMTRSubscribeParams params) subscriptionEstablished reportHandler

-- | @+ readAttributeStatusWithClusterStateCache:endpoint:queue:completion:@
readAttributeStatusWithClusterStateCache_endpoint_queue_completion :: (IsMTRClusterStateCacheContainer clusterStateCacheContainer, IsNSNumber endpoint, IsNSObject queue) => clusterStateCacheContainer -> endpoint -> queue -> Ptr () -> IO ()
readAttributeStatusWithClusterStateCache_endpoint_queue_completion clusterStateCacheContainer endpoint queue completion =
  do
    cls' <- getRequiredClass "MTRBaseClusterJointFabricDatastore"
    sendClassMessage cls' readAttributeStatusWithClusterStateCache_endpoint_queue_completionSelector (toMTRClusterStateCacheContainer clusterStateCacheContainer) (toNSNumber endpoint) (toNSObject queue) completion

-- | @- readAttributeEndpointGroupIDListWithCompletion:@
readAttributeEndpointGroupIDListWithCompletion :: IsMTRBaseClusterJointFabricDatastore mtrBaseClusterJointFabricDatastore => mtrBaseClusterJointFabricDatastore -> Ptr () -> IO ()
readAttributeEndpointGroupIDListWithCompletion mtrBaseClusterJointFabricDatastore completion =
  sendMessage mtrBaseClusterJointFabricDatastore readAttributeEndpointGroupIDListWithCompletionSelector completion

-- | @- subscribeAttributeEndpointGroupIDListWithParams:subscriptionEstablished:reportHandler:@
subscribeAttributeEndpointGroupIDListWithParams_subscriptionEstablished_reportHandler :: (IsMTRBaseClusterJointFabricDatastore mtrBaseClusterJointFabricDatastore, IsMTRSubscribeParams params) => mtrBaseClusterJointFabricDatastore -> params -> Ptr () -> Ptr () -> IO ()
subscribeAttributeEndpointGroupIDListWithParams_subscriptionEstablished_reportHandler mtrBaseClusterJointFabricDatastore params subscriptionEstablished reportHandler =
  sendMessage mtrBaseClusterJointFabricDatastore subscribeAttributeEndpointGroupIDListWithParams_subscriptionEstablished_reportHandlerSelector (toMTRSubscribeParams params) subscriptionEstablished reportHandler

-- | @+ readAttributeEndpointGroupIDListWithClusterStateCache:endpoint:queue:completion:@
readAttributeEndpointGroupIDListWithClusterStateCache_endpoint_queue_completion :: (IsMTRClusterStateCacheContainer clusterStateCacheContainer, IsNSNumber endpoint, IsNSObject queue) => clusterStateCacheContainer -> endpoint -> queue -> Ptr () -> IO ()
readAttributeEndpointGroupIDListWithClusterStateCache_endpoint_queue_completion clusterStateCacheContainer endpoint queue completion =
  do
    cls' <- getRequiredClass "MTRBaseClusterJointFabricDatastore"
    sendClassMessage cls' readAttributeEndpointGroupIDListWithClusterStateCache_endpoint_queue_completionSelector (toMTRClusterStateCacheContainer clusterStateCacheContainer) (toNSNumber endpoint) (toNSObject queue) completion

-- | @- readAttributeEndpointBindingListWithCompletion:@
readAttributeEndpointBindingListWithCompletion :: IsMTRBaseClusterJointFabricDatastore mtrBaseClusterJointFabricDatastore => mtrBaseClusterJointFabricDatastore -> Ptr () -> IO ()
readAttributeEndpointBindingListWithCompletion mtrBaseClusterJointFabricDatastore completion =
  sendMessage mtrBaseClusterJointFabricDatastore readAttributeEndpointBindingListWithCompletionSelector completion

-- | @- subscribeAttributeEndpointBindingListWithParams:subscriptionEstablished:reportHandler:@
subscribeAttributeEndpointBindingListWithParams_subscriptionEstablished_reportHandler :: (IsMTRBaseClusterJointFabricDatastore mtrBaseClusterJointFabricDatastore, IsMTRSubscribeParams params) => mtrBaseClusterJointFabricDatastore -> params -> Ptr () -> Ptr () -> IO ()
subscribeAttributeEndpointBindingListWithParams_subscriptionEstablished_reportHandler mtrBaseClusterJointFabricDatastore params subscriptionEstablished reportHandler =
  sendMessage mtrBaseClusterJointFabricDatastore subscribeAttributeEndpointBindingListWithParams_subscriptionEstablished_reportHandlerSelector (toMTRSubscribeParams params) subscriptionEstablished reportHandler

-- | @+ readAttributeEndpointBindingListWithClusterStateCache:endpoint:queue:completion:@
readAttributeEndpointBindingListWithClusterStateCache_endpoint_queue_completion :: (IsMTRClusterStateCacheContainer clusterStateCacheContainer, IsNSNumber endpoint, IsNSObject queue) => clusterStateCacheContainer -> endpoint -> queue -> Ptr () -> IO ()
readAttributeEndpointBindingListWithClusterStateCache_endpoint_queue_completion clusterStateCacheContainer endpoint queue completion =
  do
    cls' <- getRequiredClass "MTRBaseClusterJointFabricDatastore"
    sendClassMessage cls' readAttributeEndpointBindingListWithClusterStateCache_endpoint_queue_completionSelector (toMTRClusterStateCacheContainer clusterStateCacheContainer) (toNSNumber endpoint) (toNSObject queue) completion

-- | @- readAttributeNodeKeySetListWithCompletion:@
readAttributeNodeKeySetListWithCompletion :: IsMTRBaseClusterJointFabricDatastore mtrBaseClusterJointFabricDatastore => mtrBaseClusterJointFabricDatastore -> Ptr () -> IO ()
readAttributeNodeKeySetListWithCompletion mtrBaseClusterJointFabricDatastore completion =
  sendMessage mtrBaseClusterJointFabricDatastore readAttributeNodeKeySetListWithCompletionSelector completion

-- | @- subscribeAttributeNodeKeySetListWithParams:subscriptionEstablished:reportHandler:@
subscribeAttributeNodeKeySetListWithParams_subscriptionEstablished_reportHandler :: (IsMTRBaseClusterJointFabricDatastore mtrBaseClusterJointFabricDatastore, IsMTRSubscribeParams params) => mtrBaseClusterJointFabricDatastore -> params -> Ptr () -> Ptr () -> IO ()
subscribeAttributeNodeKeySetListWithParams_subscriptionEstablished_reportHandler mtrBaseClusterJointFabricDatastore params subscriptionEstablished reportHandler =
  sendMessage mtrBaseClusterJointFabricDatastore subscribeAttributeNodeKeySetListWithParams_subscriptionEstablished_reportHandlerSelector (toMTRSubscribeParams params) subscriptionEstablished reportHandler

-- | @+ readAttributeNodeKeySetListWithClusterStateCache:endpoint:queue:completion:@
readAttributeNodeKeySetListWithClusterStateCache_endpoint_queue_completion :: (IsMTRClusterStateCacheContainer clusterStateCacheContainer, IsNSNumber endpoint, IsNSObject queue) => clusterStateCacheContainer -> endpoint -> queue -> Ptr () -> IO ()
readAttributeNodeKeySetListWithClusterStateCache_endpoint_queue_completion clusterStateCacheContainer endpoint queue completion =
  do
    cls' <- getRequiredClass "MTRBaseClusterJointFabricDatastore"
    sendClassMessage cls' readAttributeNodeKeySetListWithClusterStateCache_endpoint_queue_completionSelector (toMTRClusterStateCacheContainer clusterStateCacheContainer) (toNSNumber endpoint) (toNSObject queue) completion

-- | @- readAttributeNodeACLListWithCompletion:@
readAttributeNodeACLListWithCompletion :: IsMTRBaseClusterJointFabricDatastore mtrBaseClusterJointFabricDatastore => mtrBaseClusterJointFabricDatastore -> Ptr () -> IO ()
readAttributeNodeACLListWithCompletion mtrBaseClusterJointFabricDatastore completion =
  sendMessage mtrBaseClusterJointFabricDatastore readAttributeNodeACLListWithCompletionSelector completion

-- | @- subscribeAttributeNodeACLListWithParams:subscriptionEstablished:reportHandler:@
subscribeAttributeNodeACLListWithParams_subscriptionEstablished_reportHandler :: (IsMTRBaseClusterJointFabricDatastore mtrBaseClusterJointFabricDatastore, IsMTRSubscribeParams params) => mtrBaseClusterJointFabricDatastore -> params -> Ptr () -> Ptr () -> IO ()
subscribeAttributeNodeACLListWithParams_subscriptionEstablished_reportHandler mtrBaseClusterJointFabricDatastore params subscriptionEstablished reportHandler =
  sendMessage mtrBaseClusterJointFabricDatastore subscribeAttributeNodeACLListWithParams_subscriptionEstablished_reportHandlerSelector (toMTRSubscribeParams params) subscriptionEstablished reportHandler

-- | @+ readAttributeNodeACLListWithClusterStateCache:endpoint:queue:completion:@
readAttributeNodeACLListWithClusterStateCache_endpoint_queue_completion :: (IsMTRClusterStateCacheContainer clusterStateCacheContainer, IsNSNumber endpoint, IsNSObject queue) => clusterStateCacheContainer -> endpoint -> queue -> Ptr () -> IO ()
readAttributeNodeACLListWithClusterStateCache_endpoint_queue_completion clusterStateCacheContainer endpoint queue completion =
  do
    cls' <- getRequiredClass "MTRBaseClusterJointFabricDatastore"
    sendClassMessage cls' readAttributeNodeACLListWithClusterStateCache_endpoint_queue_completionSelector (toMTRClusterStateCacheContainer clusterStateCacheContainer) (toNSNumber endpoint) (toNSObject queue) completion

-- | @- readAttributeNodeEndpointListWithCompletion:@
readAttributeNodeEndpointListWithCompletion :: IsMTRBaseClusterJointFabricDatastore mtrBaseClusterJointFabricDatastore => mtrBaseClusterJointFabricDatastore -> Ptr () -> IO ()
readAttributeNodeEndpointListWithCompletion mtrBaseClusterJointFabricDatastore completion =
  sendMessage mtrBaseClusterJointFabricDatastore readAttributeNodeEndpointListWithCompletionSelector completion

-- | @- subscribeAttributeNodeEndpointListWithParams:subscriptionEstablished:reportHandler:@
subscribeAttributeNodeEndpointListWithParams_subscriptionEstablished_reportHandler :: (IsMTRBaseClusterJointFabricDatastore mtrBaseClusterJointFabricDatastore, IsMTRSubscribeParams params) => mtrBaseClusterJointFabricDatastore -> params -> Ptr () -> Ptr () -> IO ()
subscribeAttributeNodeEndpointListWithParams_subscriptionEstablished_reportHandler mtrBaseClusterJointFabricDatastore params subscriptionEstablished reportHandler =
  sendMessage mtrBaseClusterJointFabricDatastore subscribeAttributeNodeEndpointListWithParams_subscriptionEstablished_reportHandlerSelector (toMTRSubscribeParams params) subscriptionEstablished reportHandler

-- | @+ readAttributeNodeEndpointListWithClusterStateCache:endpoint:queue:completion:@
readAttributeNodeEndpointListWithClusterStateCache_endpoint_queue_completion :: (IsMTRClusterStateCacheContainer clusterStateCacheContainer, IsNSNumber endpoint, IsNSObject queue) => clusterStateCacheContainer -> endpoint -> queue -> Ptr () -> IO ()
readAttributeNodeEndpointListWithClusterStateCache_endpoint_queue_completion clusterStateCacheContainer endpoint queue completion =
  do
    cls' <- getRequiredClass "MTRBaseClusterJointFabricDatastore"
    sendClassMessage cls' readAttributeNodeEndpointListWithClusterStateCache_endpoint_queue_completionSelector (toMTRClusterStateCacheContainer clusterStateCacheContainer) (toNSNumber endpoint) (toNSObject queue) completion

-- | @- readAttributeGeneratedCommandListWithCompletion:@
readAttributeGeneratedCommandListWithCompletion :: IsMTRBaseClusterJointFabricDatastore mtrBaseClusterJointFabricDatastore => mtrBaseClusterJointFabricDatastore -> Ptr () -> IO ()
readAttributeGeneratedCommandListWithCompletion mtrBaseClusterJointFabricDatastore completion =
  sendMessage mtrBaseClusterJointFabricDatastore readAttributeGeneratedCommandListWithCompletionSelector completion

-- | @- subscribeAttributeGeneratedCommandListWithParams:subscriptionEstablished:reportHandler:@
subscribeAttributeGeneratedCommandListWithParams_subscriptionEstablished_reportHandler :: (IsMTRBaseClusterJointFabricDatastore mtrBaseClusterJointFabricDatastore, IsMTRSubscribeParams params) => mtrBaseClusterJointFabricDatastore -> params -> Ptr () -> Ptr () -> IO ()
subscribeAttributeGeneratedCommandListWithParams_subscriptionEstablished_reportHandler mtrBaseClusterJointFabricDatastore params subscriptionEstablished reportHandler =
  sendMessage mtrBaseClusterJointFabricDatastore subscribeAttributeGeneratedCommandListWithParams_subscriptionEstablished_reportHandlerSelector (toMTRSubscribeParams params) subscriptionEstablished reportHandler

-- | @+ readAttributeGeneratedCommandListWithClusterStateCache:endpoint:queue:completion:@
readAttributeGeneratedCommandListWithClusterStateCache_endpoint_queue_completion :: (IsMTRClusterStateCacheContainer clusterStateCacheContainer, IsNSNumber endpoint, IsNSObject queue) => clusterStateCacheContainer -> endpoint -> queue -> Ptr () -> IO ()
readAttributeGeneratedCommandListWithClusterStateCache_endpoint_queue_completion clusterStateCacheContainer endpoint queue completion =
  do
    cls' <- getRequiredClass "MTRBaseClusterJointFabricDatastore"
    sendClassMessage cls' readAttributeGeneratedCommandListWithClusterStateCache_endpoint_queue_completionSelector (toMTRClusterStateCacheContainer clusterStateCacheContainer) (toNSNumber endpoint) (toNSObject queue) completion

-- | @- readAttributeAcceptedCommandListWithCompletion:@
readAttributeAcceptedCommandListWithCompletion :: IsMTRBaseClusterJointFabricDatastore mtrBaseClusterJointFabricDatastore => mtrBaseClusterJointFabricDatastore -> Ptr () -> IO ()
readAttributeAcceptedCommandListWithCompletion mtrBaseClusterJointFabricDatastore completion =
  sendMessage mtrBaseClusterJointFabricDatastore readAttributeAcceptedCommandListWithCompletionSelector completion

-- | @- subscribeAttributeAcceptedCommandListWithParams:subscriptionEstablished:reportHandler:@
subscribeAttributeAcceptedCommandListWithParams_subscriptionEstablished_reportHandler :: (IsMTRBaseClusterJointFabricDatastore mtrBaseClusterJointFabricDatastore, IsMTRSubscribeParams params) => mtrBaseClusterJointFabricDatastore -> params -> Ptr () -> Ptr () -> IO ()
subscribeAttributeAcceptedCommandListWithParams_subscriptionEstablished_reportHandler mtrBaseClusterJointFabricDatastore params subscriptionEstablished reportHandler =
  sendMessage mtrBaseClusterJointFabricDatastore subscribeAttributeAcceptedCommandListWithParams_subscriptionEstablished_reportHandlerSelector (toMTRSubscribeParams params) subscriptionEstablished reportHandler

-- | @+ readAttributeAcceptedCommandListWithClusterStateCache:endpoint:queue:completion:@
readAttributeAcceptedCommandListWithClusterStateCache_endpoint_queue_completion :: (IsMTRClusterStateCacheContainer clusterStateCacheContainer, IsNSNumber endpoint, IsNSObject queue) => clusterStateCacheContainer -> endpoint -> queue -> Ptr () -> IO ()
readAttributeAcceptedCommandListWithClusterStateCache_endpoint_queue_completion clusterStateCacheContainer endpoint queue completion =
  do
    cls' <- getRequiredClass "MTRBaseClusterJointFabricDatastore"
    sendClassMessage cls' readAttributeAcceptedCommandListWithClusterStateCache_endpoint_queue_completionSelector (toMTRClusterStateCacheContainer clusterStateCacheContainer) (toNSNumber endpoint) (toNSObject queue) completion

-- | @- readAttributeAttributeListWithCompletion:@
readAttributeAttributeListWithCompletion :: IsMTRBaseClusterJointFabricDatastore mtrBaseClusterJointFabricDatastore => mtrBaseClusterJointFabricDatastore -> Ptr () -> IO ()
readAttributeAttributeListWithCompletion mtrBaseClusterJointFabricDatastore completion =
  sendMessage mtrBaseClusterJointFabricDatastore readAttributeAttributeListWithCompletionSelector completion

-- | @- subscribeAttributeAttributeListWithParams:subscriptionEstablished:reportHandler:@
subscribeAttributeAttributeListWithParams_subscriptionEstablished_reportHandler :: (IsMTRBaseClusterJointFabricDatastore mtrBaseClusterJointFabricDatastore, IsMTRSubscribeParams params) => mtrBaseClusterJointFabricDatastore -> params -> Ptr () -> Ptr () -> IO ()
subscribeAttributeAttributeListWithParams_subscriptionEstablished_reportHandler mtrBaseClusterJointFabricDatastore params subscriptionEstablished reportHandler =
  sendMessage mtrBaseClusterJointFabricDatastore subscribeAttributeAttributeListWithParams_subscriptionEstablished_reportHandlerSelector (toMTRSubscribeParams params) subscriptionEstablished reportHandler

-- | @+ readAttributeAttributeListWithClusterStateCache:endpoint:queue:completion:@
readAttributeAttributeListWithClusterStateCache_endpoint_queue_completion :: (IsMTRClusterStateCacheContainer clusterStateCacheContainer, IsNSNumber endpoint, IsNSObject queue) => clusterStateCacheContainer -> endpoint -> queue -> Ptr () -> IO ()
readAttributeAttributeListWithClusterStateCache_endpoint_queue_completion clusterStateCacheContainer endpoint queue completion =
  do
    cls' <- getRequiredClass "MTRBaseClusterJointFabricDatastore"
    sendClassMessage cls' readAttributeAttributeListWithClusterStateCache_endpoint_queue_completionSelector (toMTRClusterStateCacheContainer clusterStateCacheContainer) (toNSNumber endpoint) (toNSObject queue) completion

-- | @- readAttributeFeatureMapWithCompletion:@
readAttributeFeatureMapWithCompletion :: IsMTRBaseClusterJointFabricDatastore mtrBaseClusterJointFabricDatastore => mtrBaseClusterJointFabricDatastore -> Ptr () -> IO ()
readAttributeFeatureMapWithCompletion mtrBaseClusterJointFabricDatastore completion =
  sendMessage mtrBaseClusterJointFabricDatastore readAttributeFeatureMapWithCompletionSelector completion

-- | @- subscribeAttributeFeatureMapWithParams:subscriptionEstablished:reportHandler:@
subscribeAttributeFeatureMapWithParams_subscriptionEstablished_reportHandler :: (IsMTRBaseClusterJointFabricDatastore mtrBaseClusterJointFabricDatastore, IsMTRSubscribeParams params) => mtrBaseClusterJointFabricDatastore -> params -> Ptr () -> Ptr () -> IO ()
subscribeAttributeFeatureMapWithParams_subscriptionEstablished_reportHandler mtrBaseClusterJointFabricDatastore params subscriptionEstablished reportHandler =
  sendMessage mtrBaseClusterJointFabricDatastore subscribeAttributeFeatureMapWithParams_subscriptionEstablished_reportHandlerSelector (toMTRSubscribeParams params) subscriptionEstablished reportHandler

-- | @+ readAttributeFeatureMapWithClusterStateCache:endpoint:queue:completion:@
readAttributeFeatureMapWithClusterStateCache_endpoint_queue_completion :: (IsMTRClusterStateCacheContainer clusterStateCacheContainer, IsNSNumber endpoint, IsNSObject queue) => clusterStateCacheContainer -> endpoint -> queue -> Ptr () -> IO ()
readAttributeFeatureMapWithClusterStateCache_endpoint_queue_completion clusterStateCacheContainer endpoint queue completion =
  do
    cls' <- getRequiredClass "MTRBaseClusterJointFabricDatastore"
    sendClassMessage cls' readAttributeFeatureMapWithClusterStateCache_endpoint_queue_completionSelector (toMTRClusterStateCacheContainer clusterStateCacheContainer) (toNSNumber endpoint) (toNSObject queue) completion

-- | @- readAttributeClusterRevisionWithCompletion:@
readAttributeClusterRevisionWithCompletion :: IsMTRBaseClusterJointFabricDatastore mtrBaseClusterJointFabricDatastore => mtrBaseClusterJointFabricDatastore -> Ptr () -> IO ()
readAttributeClusterRevisionWithCompletion mtrBaseClusterJointFabricDatastore completion =
  sendMessage mtrBaseClusterJointFabricDatastore readAttributeClusterRevisionWithCompletionSelector completion

-- | @- subscribeAttributeClusterRevisionWithParams:subscriptionEstablished:reportHandler:@
subscribeAttributeClusterRevisionWithParams_subscriptionEstablished_reportHandler :: (IsMTRBaseClusterJointFabricDatastore mtrBaseClusterJointFabricDatastore, IsMTRSubscribeParams params) => mtrBaseClusterJointFabricDatastore -> params -> Ptr () -> Ptr () -> IO ()
subscribeAttributeClusterRevisionWithParams_subscriptionEstablished_reportHandler mtrBaseClusterJointFabricDatastore params subscriptionEstablished reportHandler =
  sendMessage mtrBaseClusterJointFabricDatastore subscribeAttributeClusterRevisionWithParams_subscriptionEstablished_reportHandlerSelector (toMTRSubscribeParams params) subscriptionEstablished reportHandler

-- | @+ readAttributeClusterRevisionWithClusterStateCache:endpoint:queue:completion:@
readAttributeClusterRevisionWithClusterStateCache_endpoint_queue_completion :: (IsMTRClusterStateCacheContainer clusterStateCacheContainer, IsNSNumber endpoint, IsNSObject queue) => clusterStateCacheContainer -> endpoint -> queue -> Ptr () -> IO ()
readAttributeClusterRevisionWithClusterStateCache_endpoint_queue_completion clusterStateCacheContainer endpoint queue completion =
  do
    cls' <- getRequiredClass "MTRBaseClusterJointFabricDatastore"
    sendClassMessage cls' readAttributeClusterRevisionWithClusterStateCache_endpoint_queue_completionSelector (toMTRClusterStateCacheContainer clusterStateCacheContainer) (toNSNumber endpoint) (toNSObject queue) completion

-- | @- init@
init_ :: IsMTRBaseClusterJointFabricDatastore mtrBaseClusterJointFabricDatastore => mtrBaseClusterJointFabricDatastore -> IO (Id MTRBaseClusterJointFabricDatastore)
init_ mtrBaseClusterJointFabricDatastore =
  sendOwnedMessage mtrBaseClusterJointFabricDatastore initSelector

-- | @+ new@
new :: IO (Id MTRBaseClusterJointFabricDatastore)
new  =
  do
    cls' <- getRequiredClass "MTRBaseClusterJointFabricDatastore"
    sendOwnedClassMessage cls' newSelector

-- | For all instance methods (reads, writes, commands) that take a completion, the completion will be called on the provided queue.
--
-- ObjC selector: @- initWithDevice:endpointID:queue:@
initWithDevice_endpointID_queue :: (IsMTRBaseClusterJointFabricDatastore mtrBaseClusterJointFabricDatastore, IsMTRBaseDevice device, IsNSNumber endpointID, IsNSObject queue) => mtrBaseClusterJointFabricDatastore -> device -> endpointID -> queue -> IO (Id MTRBaseClusterJointFabricDatastore)
initWithDevice_endpointID_queue mtrBaseClusterJointFabricDatastore device endpointID queue =
  sendOwnedMessage mtrBaseClusterJointFabricDatastore initWithDevice_endpointID_queueSelector (toMTRBaseDevice device) (toNSNumber endpointID) (toNSObject queue)

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @addKeySetWithParams:completion:@
addKeySetWithParams_completionSelector :: Selector '[Id MTRJointFabricDatastoreClusterAddKeySetParams, Ptr ()] ()
addKeySetWithParams_completionSelector = mkSelector "addKeySetWithParams:completion:"

-- | @Selector@ for @updateKeySetWithParams:completion:@
updateKeySetWithParams_completionSelector :: Selector '[Id MTRJointFabricDatastoreClusterUpdateKeySetParams, Ptr ()] ()
updateKeySetWithParams_completionSelector = mkSelector "updateKeySetWithParams:completion:"

-- | @Selector@ for @removeKeySetWithParams:completion:@
removeKeySetWithParams_completionSelector :: Selector '[Id MTRJointFabricDatastoreClusterRemoveKeySetParams, Ptr ()] ()
removeKeySetWithParams_completionSelector = mkSelector "removeKeySetWithParams:completion:"

-- | @Selector@ for @addGroupWithParams:completion:@
addGroupWithParams_completionSelector :: Selector '[Id MTRJointFabricDatastoreClusterAddGroupParams, Ptr ()] ()
addGroupWithParams_completionSelector = mkSelector "addGroupWithParams:completion:"

-- | @Selector@ for @updateGroupWithParams:completion:@
updateGroupWithParams_completionSelector :: Selector '[Id MTRJointFabricDatastoreClusterUpdateGroupParams, Ptr ()] ()
updateGroupWithParams_completionSelector = mkSelector "updateGroupWithParams:completion:"

-- | @Selector@ for @removeGroupWithParams:completion:@
removeGroupWithParams_completionSelector :: Selector '[Id MTRJointFabricDatastoreClusterRemoveGroupParams, Ptr ()] ()
removeGroupWithParams_completionSelector = mkSelector "removeGroupWithParams:completion:"

-- | @Selector@ for @addAdminWithParams:completion:@
addAdminWithParams_completionSelector :: Selector '[Id MTRJointFabricDatastoreClusterAddAdminParams, Ptr ()] ()
addAdminWithParams_completionSelector = mkSelector "addAdminWithParams:completion:"

-- | @Selector@ for @updateAdminWithParams:completion:@
updateAdminWithParams_completionSelector :: Selector '[Id MTRJointFabricDatastoreClusterUpdateAdminParams, Ptr ()] ()
updateAdminWithParams_completionSelector = mkSelector "updateAdminWithParams:completion:"

-- | @Selector@ for @removeAdminWithParams:completion:@
removeAdminWithParams_completionSelector :: Selector '[Id MTRJointFabricDatastoreClusterRemoveAdminParams, Ptr ()] ()
removeAdminWithParams_completionSelector = mkSelector "removeAdminWithParams:completion:"

-- | @Selector@ for @addPendingNodeWithParams:completion:@
addPendingNodeWithParams_completionSelector :: Selector '[Id MTRJointFabricDatastoreClusterAddPendingNodeParams, Ptr ()] ()
addPendingNodeWithParams_completionSelector = mkSelector "addPendingNodeWithParams:completion:"

-- | @Selector@ for @refreshNodeWithParams:completion:@
refreshNodeWithParams_completionSelector :: Selector '[Id MTRJointFabricDatastoreClusterRefreshNodeParams, Ptr ()] ()
refreshNodeWithParams_completionSelector = mkSelector "refreshNodeWithParams:completion:"

-- | @Selector@ for @updateNodeWithParams:completion:@
updateNodeWithParams_completionSelector :: Selector '[Id MTRJointFabricDatastoreClusterUpdateNodeParams, Ptr ()] ()
updateNodeWithParams_completionSelector = mkSelector "updateNodeWithParams:completion:"

-- | @Selector@ for @removeNodeWithParams:completion:@
removeNodeWithParams_completionSelector :: Selector '[Id MTRJointFabricDatastoreClusterRemoveNodeParams, Ptr ()] ()
removeNodeWithParams_completionSelector = mkSelector "removeNodeWithParams:completion:"

-- | @Selector@ for @updateEndpointForNodeWithParams:completion:@
updateEndpointForNodeWithParams_completionSelector :: Selector '[Id MTRJointFabricDatastoreClusterUpdateEndpointForNodeParams, Ptr ()] ()
updateEndpointForNodeWithParams_completionSelector = mkSelector "updateEndpointForNodeWithParams:completion:"

-- | @Selector@ for @addGroupIDToEndpointForNodeWithParams:completion:@
addGroupIDToEndpointForNodeWithParams_completionSelector :: Selector '[Id MTRJointFabricDatastoreClusterAddGroupIDToEndpointForNodeParams, Ptr ()] ()
addGroupIDToEndpointForNodeWithParams_completionSelector = mkSelector "addGroupIDToEndpointForNodeWithParams:completion:"

-- | @Selector@ for @removeGroupIDFromEndpointForNodeWithParams:completion:@
removeGroupIDFromEndpointForNodeWithParams_completionSelector :: Selector '[Id MTRJointFabricDatastoreClusterRemoveGroupIDFromEndpointForNodeParams, Ptr ()] ()
removeGroupIDFromEndpointForNodeWithParams_completionSelector = mkSelector "removeGroupIDFromEndpointForNodeWithParams:completion:"

-- | @Selector@ for @addBindingToEndpointForNodeWithParams:completion:@
addBindingToEndpointForNodeWithParams_completionSelector :: Selector '[Id MTRJointFabricDatastoreClusterAddBindingToEndpointForNodeParams, Ptr ()] ()
addBindingToEndpointForNodeWithParams_completionSelector = mkSelector "addBindingToEndpointForNodeWithParams:completion:"

-- | @Selector@ for @removeBindingFromEndpointForNodeWithParams:completion:@
removeBindingFromEndpointForNodeWithParams_completionSelector :: Selector '[Id MTRJointFabricDatastoreClusterRemoveBindingFromEndpointForNodeParams, Ptr ()] ()
removeBindingFromEndpointForNodeWithParams_completionSelector = mkSelector "removeBindingFromEndpointForNodeWithParams:completion:"

-- | @Selector@ for @addACLToNodeWithParams:completion:@
addACLToNodeWithParams_completionSelector :: Selector '[Id MTRJointFabricDatastoreClusterAddACLToNodeParams, Ptr ()] ()
addACLToNodeWithParams_completionSelector = mkSelector "addACLToNodeWithParams:completion:"

-- | @Selector@ for @removeACLFromNodeWithParams:completion:@
removeACLFromNodeWithParams_completionSelector :: Selector '[Id MTRJointFabricDatastoreClusterRemoveACLFromNodeParams, Ptr ()] ()
removeACLFromNodeWithParams_completionSelector = mkSelector "removeACLFromNodeWithParams:completion:"

-- | @Selector@ for @readAttributeAnchorRootCAWithCompletion:@
readAttributeAnchorRootCAWithCompletionSelector :: Selector '[Ptr ()] ()
readAttributeAnchorRootCAWithCompletionSelector = mkSelector "readAttributeAnchorRootCAWithCompletion:"

-- | @Selector@ for @subscribeAttributeAnchorRootCAWithParams:subscriptionEstablished:reportHandler:@
subscribeAttributeAnchorRootCAWithParams_subscriptionEstablished_reportHandlerSelector :: Selector '[Id MTRSubscribeParams, Ptr (), Ptr ()] ()
subscribeAttributeAnchorRootCAWithParams_subscriptionEstablished_reportHandlerSelector = mkSelector "subscribeAttributeAnchorRootCAWithParams:subscriptionEstablished:reportHandler:"

-- | @Selector@ for @readAttributeAnchorRootCAWithClusterStateCache:endpoint:queue:completion:@
readAttributeAnchorRootCAWithClusterStateCache_endpoint_queue_completionSelector :: Selector '[Id MTRClusterStateCacheContainer, Id NSNumber, Id NSObject, Ptr ()] ()
readAttributeAnchorRootCAWithClusterStateCache_endpoint_queue_completionSelector = mkSelector "readAttributeAnchorRootCAWithClusterStateCache:endpoint:queue:completion:"

-- | @Selector@ for @readAttributeAnchorNodeIDWithCompletion:@
readAttributeAnchorNodeIDWithCompletionSelector :: Selector '[Ptr ()] ()
readAttributeAnchorNodeIDWithCompletionSelector = mkSelector "readAttributeAnchorNodeIDWithCompletion:"

-- | @Selector@ for @subscribeAttributeAnchorNodeIDWithParams:subscriptionEstablished:reportHandler:@
subscribeAttributeAnchorNodeIDWithParams_subscriptionEstablished_reportHandlerSelector :: Selector '[Id MTRSubscribeParams, Ptr (), Ptr ()] ()
subscribeAttributeAnchorNodeIDWithParams_subscriptionEstablished_reportHandlerSelector = mkSelector "subscribeAttributeAnchorNodeIDWithParams:subscriptionEstablished:reportHandler:"

-- | @Selector@ for @readAttributeAnchorNodeIDWithClusterStateCache:endpoint:queue:completion:@
readAttributeAnchorNodeIDWithClusterStateCache_endpoint_queue_completionSelector :: Selector '[Id MTRClusterStateCacheContainer, Id NSNumber, Id NSObject, Ptr ()] ()
readAttributeAnchorNodeIDWithClusterStateCache_endpoint_queue_completionSelector = mkSelector "readAttributeAnchorNodeIDWithClusterStateCache:endpoint:queue:completion:"

-- | @Selector@ for @readAttributeAnchorVendorIDWithCompletion:@
readAttributeAnchorVendorIDWithCompletionSelector :: Selector '[Ptr ()] ()
readAttributeAnchorVendorIDWithCompletionSelector = mkSelector "readAttributeAnchorVendorIDWithCompletion:"

-- | @Selector@ for @subscribeAttributeAnchorVendorIDWithParams:subscriptionEstablished:reportHandler:@
subscribeAttributeAnchorVendorIDWithParams_subscriptionEstablished_reportHandlerSelector :: Selector '[Id MTRSubscribeParams, Ptr (), Ptr ()] ()
subscribeAttributeAnchorVendorIDWithParams_subscriptionEstablished_reportHandlerSelector = mkSelector "subscribeAttributeAnchorVendorIDWithParams:subscriptionEstablished:reportHandler:"

-- | @Selector@ for @readAttributeAnchorVendorIDWithClusterStateCache:endpoint:queue:completion:@
readAttributeAnchorVendorIDWithClusterStateCache_endpoint_queue_completionSelector :: Selector '[Id MTRClusterStateCacheContainer, Id NSNumber, Id NSObject, Ptr ()] ()
readAttributeAnchorVendorIDWithClusterStateCache_endpoint_queue_completionSelector = mkSelector "readAttributeAnchorVendorIDWithClusterStateCache:endpoint:queue:completion:"

-- | @Selector@ for @readAttributeFriendlyNameWithCompletion:@
readAttributeFriendlyNameWithCompletionSelector :: Selector '[Ptr ()] ()
readAttributeFriendlyNameWithCompletionSelector = mkSelector "readAttributeFriendlyNameWithCompletion:"

-- | @Selector@ for @subscribeAttributeFriendlyNameWithParams:subscriptionEstablished:reportHandler:@
subscribeAttributeFriendlyNameWithParams_subscriptionEstablished_reportHandlerSelector :: Selector '[Id MTRSubscribeParams, Ptr (), Ptr ()] ()
subscribeAttributeFriendlyNameWithParams_subscriptionEstablished_reportHandlerSelector = mkSelector "subscribeAttributeFriendlyNameWithParams:subscriptionEstablished:reportHandler:"

-- | @Selector@ for @readAttributeFriendlyNameWithClusterStateCache:endpoint:queue:completion:@
readAttributeFriendlyNameWithClusterStateCache_endpoint_queue_completionSelector :: Selector '[Id MTRClusterStateCacheContainer, Id NSNumber, Id NSObject, Ptr ()] ()
readAttributeFriendlyNameWithClusterStateCache_endpoint_queue_completionSelector = mkSelector "readAttributeFriendlyNameWithClusterStateCache:endpoint:queue:completion:"

-- | @Selector@ for @readAttributeGroupKeySetListWithCompletion:@
readAttributeGroupKeySetListWithCompletionSelector :: Selector '[Ptr ()] ()
readAttributeGroupKeySetListWithCompletionSelector = mkSelector "readAttributeGroupKeySetListWithCompletion:"

-- | @Selector@ for @subscribeAttributeGroupKeySetListWithParams:subscriptionEstablished:reportHandler:@
subscribeAttributeGroupKeySetListWithParams_subscriptionEstablished_reportHandlerSelector :: Selector '[Id MTRSubscribeParams, Ptr (), Ptr ()] ()
subscribeAttributeGroupKeySetListWithParams_subscriptionEstablished_reportHandlerSelector = mkSelector "subscribeAttributeGroupKeySetListWithParams:subscriptionEstablished:reportHandler:"

-- | @Selector@ for @readAttributeGroupKeySetListWithClusterStateCache:endpoint:queue:completion:@
readAttributeGroupKeySetListWithClusterStateCache_endpoint_queue_completionSelector :: Selector '[Id MTRClusterStateCacheContainer, Id NSNumber, Id NSObject, Ptr ()] ()
readAttributeGroupKeySetListWithClusterStateCache_endpoint_queue_completionSelector = mkSelector "readAttributeGroupKeySetListWithClusterStateCache:endpoint:queue:completion:"

-- | @Selector@ for @readAttributeGroupListWithCompletion:@
readAttributeGroupListWithCompletionSelector :: Selector '[Ptr ()] ()
readAttributeGroupListWithCompletionSelector = mkSelector "readAttributeGroupListWithCompletion:"

-- | @Selector@ for @subscribeAttributeGroupListWithParams:subscriptionEstablished:reportHandler:@
subscribeAttributeGroupListWithParams_subscriptionEstablished_reportHandlerSelector :: Selector '[Id MTRSubscribeParams, Ptr (), Ptr ()] ()
subscribeAttributeGroupListWithParams_subscriptionEstablished_reportHandlerSelector = mkSelector "subscribeAttributeGroupListWithParams:subscriptionEstablished:reportHandler:"

-- | @Selector@ for @readAttributeGroupListWithClusterStateCache:endpoint:queue:completion:@
readAttributeGroupListWithClusterStateCache_endpoint_queue_completionSelector :: Selector '[Id MTRClusterStateCacheContainer, Id NSNumber, Id NSObject, Ptr ()] ()
readAttributeGroupListWithClusterStateCache_endpoint_queue_completionSelector = mkSelector "readAttributeGroupListWithClusterStateCache:endpoint:queue:completion:"

-- | @Selector@ for @readAttributeNodeListWithCompletion:@
readAttributeNodeListWithCompletionSelector :: Selector '[Ptr ()] ()
readAttributeNodeListWithCompletionSelector = mkSelector "readAttributeNodeListWithCompletion:"

-- | @Selector@ for @subscribeAttributeNodeListWithParams:subscriptionEstablished:reportHandler:@
subscribeAttributeNodeListWithParams_subscriptionEstablished_reportHandlerSelector :: Selector '[Id MTRSubscribeParams, Ptr (), Ptr ()] ()
subscribeAttributeNodeListWithParams_subscriptionEstablished_reportHandlerSelector = mkSelector "subscribeAttributeNodeListWithParams:subscriptionEstablished:reportHandler:"

-- | @Selector@ for @readAttributeNodeListWithClusterStateCache:endpoint:queue:completion:@
readAttributeNodeListWithClusterStateCache_endpoint_queue_completionSelector :: Selector '[Id MTRClusterStateCacheContainer, Id NSNumber, Id NSObject, Ptr ()] ()
readAttributeNodeListWithClusterStateCache_endpoint_queue_completionSelector = mkSelector "readAttributeNodeListWithClusterStateCache:endpoint:queue:completion:"

-- | @Selector@ for @readAttributeAdminListWithCompletion:@
readAttributeAdminListWithCompletionSelector :: Selector '[Ptr ()] ()
readAttributeAdminListWithCompletionSelector = mkSelector "readAttributeAdminListWithCompletion:"

-- | @Selector@ for @subscribeAttributeAdminListWithParams:subscriptionEstablished:reportHandler:@
subscribeAttributeAdminListWithParams_subscriptionEstablished_reportHandlerSelector :: Selector '[Id MTRSubscribeParams, Ptr (), Ptr ()] ()
subscribeAttributeAdminListWithParams_subscriptionEstablished_reportHandlerSelector = mkSelector "subscribeAttributeAdminListWithParams:subscriptionEstablished:reportHandler:"

-- | @Selector@ for @readAttributeAdminListWithClusterStateCache:endpoint:queue:completion:@
readAttributeAdminListWithClusterStateCache_endpoint_queue_completionSelector :: Selector '[Id MTRClusterStateCacheContainer, Id NSNumber, Id NSObject, Ptr ()] ()
readAttributeAdminListWithClusterStateCache_endpoint_queue_completionSelector = mkSelector "readAttributeAdminListWithClusterStateCache:endpoint:queue:completion:"

-- | @Selector@ for @readAttributeStatusWithCompletion:@
readAttributeStatusWithCompletionSelector :: Selector '[Ptr ()] ()
readAttributeStatusWithCompletionSelector = mkSelector "readAttributeStatusWithCompletion:"

-- | @Selector@ for @subscribeAttributeStatusWithParams:subscriptionEstablished:reportHandler:@
subscribeAttributeStatusWithParams_subscriptionEstablished_reportHandlerSelector :: Selector '[Id MTRSubscribeParams, Ptr (), Ptr ()] ()
subscribeAttributeStatusWithParams_subscriptionEstablished_reportHandlerSelector = mkSelector "subscribeAttributeStatusWithParams:subscriptionEstablished:reportHandler:"

-- | @Selector@ for @readAttributeStatusWithClusterStateCache:endpoint:queue:completion:@
readAttributeStatusWithClusterStateCache_endpoint_queue_completionSelector :: Selector '[Id MTRClusterStateCacheContainer, Id NSNumber, Id NSObject, Ptr ()] ()
readAttributeStatusWithClusterStateCache_endpoint_queue_completionSelector = mkSelector "readAttributeStatusWithClusterStateCache:endpoint:queue:completion:"

-- | @Selector@ for @readAttributeEndpointGroupIDListWithCompletion:@
readAttributeEndpointGroupIDListWithCompletionSelector :: Selector '[Ptr ()] ()
readAttributeEndpointGroupIDListWithCompletionSelector = mkSelector "readAttributeEndpointGroupIDListWithCompletion:"

-- | @Selector@ for @subscribeAttributeEndpointGroupIDListWithParams:subscriptionEstablished:reportHandler:@
subscribeAttributeEndpointGroupIDListWithParams_subscriptionEstablished_reportHandlerSelector :: Selector '[Id MTRSubscribeParams, Ptr (), Ptr ()] ()
subscribeAttributeEndpointGroupIDListWithParams_subscriptionEstablished_reportHandlerSelector = mkSelector "subscribeAttributeEndpointGroupIDListWithParams:subscriptionEstablished:reportHandler:"

-- | @Selector@ for @readAttributeEndpointGroupIDListWithClusterStateCache:endpoint:queue:completion:@
readAttributeEndpointGroupIDListWithClusterStateCache_endpoint_queue_completionSelector :: Selector '[Id MTRClusterStateCacheContainer, Id NSNumber, Id NSObject, Ptr ()] ()
readAttributeEndpointGroupIDListWithClusterStateCache_endpoint_queue_completionSelector = mkSelector "readAttributeEndpointGroupIDListWithClusterStateCache:endpoint:queue:completion:"

-- | @Selector@ for @readAttributeEndpointBindingListWithCompletion:@
readAttributeEndpointBindingListWithCompletionSelector :: Selector '[Ptr ()] ()
readAttributeEndpointBindingListWithCompletionSelector = mkSelector "readAttributeEndpointBindingListWithCompletion:"

-- | @Selector@ for @subscribeAttributeEndpointBindingListWithParams:subscriptionEstablished:reportHandler:@
subscribeAttributeEndpointBindingListWithParams_subscriptionEstablished_reportHandlerSelector :: Selector '[Id MTRSubscribeParams, Ptr (), Ptr ()] ()
subscribeAttributeEndpointBindingListWithParams_subscriptionEstablished_reportHandlerSelector = mkSelector "subscribeAttributeEndpointBindingListWithParams:subscriptionEstablished:reportHandler:"

-- | @Selector@ for @readAttributeEndpointBindingListWithClusterStateCache:endpoint:queue:completion:@
readAttributeEndpointBindingListWithClusterStateCache_endpoint_queue_completionSelector :: Selector '[Id MTRClusterStateCacheContainer, Id NSNumber, Id NSObject, Ptr ()] ()
readAttributeEndpointBindingListWithClusterStateCache_endpoint_queue_completionSelector = mkSelector "readAttributeEndpointBindingListWithClusterStateCache:endpoint:queue:completion:"

-- | @Selector@ for @readAttributeNodeKeySetListWithCompletion:@
readAttributeNodeKeySetListWithCompletionSelector :: Selector '[Ptr ()] ()
readAttributeNodeKeySetListWithCompletionSelector = mkSelector "readAttributeNodeKeySetListWithCompletion:"

-- | @Selector@ for @subscribeAttributeNodeKeySetListWithParams:subscriptionEstablished:reportHandler:@
subscribeAttributeNodeKeySetListWithParams_subscriptionEstablished_reportHandlerSelector :: Selector '[Id MTRSubscribeParams, Ptr (), Ptr ()] ()
subscribeAttributeNodeKeySetListWithParams_subscriptionEstablished_reportHandlerSelector = mkSelector "subscribeAttributeNodeKeySetListWithParams:subscriptionEstablished:reportHandler:"

-- | @Selector@ for @readAttributeNodeKeySetListWithClusterStateCache:endpoint:queue:completion:@
readAttributeNodeKeySetListWithClusterStateCache_endpoint_queue_completionSelector :: Selector '[Id MTRClusterStateCacheContainer, Id NSNumber, Id NSObject, Ptr ()] ()
readAttributeNodeKeySetListWithClusterStateCache_endpoint_queue_completionSelector = mkSelector "readAttributeNodeKeySetListWithClusterStateCache:endpoint:queue:completion:"

-- | @Selector@ for @readAttributeNodeACLListWithCompletion:@
readAttributeNodeACLListWithCompletionSelector :: Selector '[Ptr ()] ()
readAttributeNodeACLListWithCompletionSelector = mkSelector "readAttributeNodeACLListWithCompletion:"

-- | @Selector@ for @subscribeAttributeNodeACLListWithParams:subscriptionEstablished:reportHandler:@
subscribeAttributeNodeACLListWithParams_subscriptionEstablished_reportHandlerSelector :: Selector '[Id MTRSubscribeParams, Ptr (), Ptr ()] ()
subscribeAttributeNodeACLListWithParams_subscriptionEstablished_reportHandlerSelector = mkSelector "subscribeAttributeNodeACLListWithParams:subscriptionEstablished:reportHandler:"

-- | @Selector@ for @readAttributeNodeACLListWithClusterStateCache:endpoint:queue:completion:@
readAttributeNodeACLListWithClusterStateCache_endpoint_queue_completionSelector :: Selector '[Id MTRClusterStateCacheContainer, Id NSNumber, Id NSObject, Ptr ()] ()
readAttributeNodeACLListWithClusterStateCache_endpoint_queue_completionSelector = mkSelector "readAttributeNodeACLListWithClusterStateCache:endpoint:queue:completion:"

-- | @Selector@ for @readAttributeNodeEndpointListWithCompletion:@
readAttributeNodeEndpointListWithCompletionSelector :: Selector '[Ptr ()] ()
readAttributeNodeEndpointListWithCompletionSelector = mkSelector "readAttributeNodeEndpointListWithCompletion:"

-- | @Selector@ for @subscribeAttributeNodeEndpointListWithParams:subscriptionEstablished:reportHandler:@
subscribeAttributeNodeEndpointListWithParams_subscriptionEstablished_reportHandlerSelector :: Selector '[Id MTRSubscribeParams, Ptr (), Ptr ()] ()
subscribeAttributeNodeEndpointListWithParams_subscriptionEstablished_reportHandlerSelector = mkSelector "subscribeAttributeNodeEndpointListWithParams:subscriptionEstablished:reportHandler:"

-- | @Selector@ for @readAttributeNodeEndpointListWithClusterStateCache:endpoint:queue:completion:@
readAttributeNodeEndpointListWithClusterStateCache_endpoint_queue_completionSelector :: Selector '[Id MTRClusterStateCacheContainer, Id NSNumber, Id NSObject, Ptr ()] ()
readAttributeNodeEndpointListWithClusterStateCache_endpoint_queue_completionSelector = mkSelector "readAttributeNodeEndpointListWithClusterStateCache:endpoint:queue:completion:"

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
initSelector :: Selector '[] (Id MTRBaseClusterJointFabricDatastore)
initSelector = mkSelector "init"

-- | @Selector@ for @new@
newSelector :: Selector '[] (Id MTRBaseClusterJointFabricDatastore)
newSelector = mkSelector "new"

-- | @Selector@ for @initWithDevice:endpointID:queue:@
initWithDevice_endpointID_queueSelector :: Selector '[Id MTRBaseDevice, Id NSNumber, Id NSObject] (Id MTRBaseClusterJointFabricDatastore)
initWithDevice_endpointID_queueSelector = mkSelector "initWithDevice:endpointID:queue:"

