{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Cluster Joint Fabric Datastore    The Joint Fabric Datastore Cluster is a cluster that provides a mechanism for the Joint Fabric Administrators to manage the set of Nodes, Groups, and Group membership among Nodes in the Joint Fabric.
--
-- Generated bindings for @MTRClusterJointFabricDatastore@.
module ObjC.Matter.MTRClusterJointFabricDatastore
  ( MTRClusterJointFabricDatastore
  , IsMTRClusterJointFabricDatastore(..)
  , addKeySetWithParams_expectedValues_expectedValueInterval_completion
  , updateKeySetWithParams_expectedValues_expectedValueInterval_completion
  , removeKeySetWithParams_expectedValues_expectedValueInterval_completion
  , addGroupWithParams_expectedValues_expectedValueInterval_completion
  , updateGroupWithParams_expectedValues_expectedValueInterval_completion
  , removeGroupWithParams_expectedValues_expectedValueInterval_completion
  , addAdminWithParams_expectedValues_expectedValueInterval_completion
  , updateAdminWithParams_expectedValues_expectedValueInterval_completion
  , removeAdminWithParams_expectedValues_expectedValueInterval_completion
  , addPendingNodeWithParams_expectedValues_expectedValueInterval_completion
  , refreshNodeWithParams_expectedValues_expectedValueInterval_completion
  , updateNodeWithParams_expectedValues_expectedValueInterval_completion
  , removeNodeWithParams_expectedValues_expectedValueInterval_completion
  , updateEndpointForNodeWithParams_expectedValues_expectedValueInterval_completion
  , addGroupIDToEndpointForNodeWithParams_expectedValues_expectedValueInterval_completion
  , removeGroupIDFromEndpointForNodeWithParams_expectedValues_expectedValueInterval_completion
  , addBindingToEndpointForNodeWithParams_expectedValues_expectedValueInterval_completion
  , removeBindingFromEndpointForNodeWithParams_expectedValues_expectedValueInterval_completion
  , addACLToNodeWithParams_expectedValues_expectedValueInterval_completion
  , removeACLFromNodeWithParams_expectedValues_expectedValueInterval_completion
  , readAttributeAnchorRootCAWithParams
  , readAttributeAnchorNodeIDWithParams
  , readAttributeAnchorVendorIDWithParams
  , readAttributeFriendlyNameWithParams
  , readAttributeGroupKeySetListWithParams
  , readAttributeGroupListWithParams
  , readAttributeNodeListWithParams
  , readAttributeAdminListWithParams
  , readAttributeStatusWithParams
  , readAttributeEndpointGroupIDListWithParams
  , readAttributeEndpointBindingListWithParams
  , readAttributeNodeKeySetListWithParams
  , readAttributeNodeACLListWithParams
  , readAttributeNodeEndpointListWithParams
  , readAttributeGeneratedCommandListWithParams
  , readAttributeAcceptedCommandListWithParams
  , readAttributeAttributeListWithParams
  , readAttributeFeatureMapWithParams
  , readAttributeClusterRevisionWithParams
  , init_
  , new
  , initWithDevice_endpointID_queue
  , addACLToNodeWithParams_expectedValues_expectedValueInterval_completionSelector
  , addAdminWithParams_expectedValues_expectedValueInterval_completionSelector
  , addBindingToEndpointForNodeWithParams_expectedValues_expectedValueInterval_completionSelector
  , addGroupIDToEndpointForNodeWithParams_expectedValues_expectedValueInterval_completionSelector
  , addGroupWithParams_expectedValues_expectedValueInterval_completionSelector
  , addKeySetWithParams_expectedValues_expectedValueInterval_completionSelector
  , addPendingNodeWithParams_expectedValues_expectedValueInterval_completionSelector
  , initSelector
  , initWithDevice_endpointID_queueSelector
  , newSelector
  , readAttributeAcceptedCommandListWithParamsSelector
  , readAttributeAdminListWithParamsSelector
  , readAttributeAnchorNodeIDWithParamsSelector
  , readAttributeAnchorRootCAWithParamsSelector
  , readAttributeAnchorVendorIDWithParamsSelector
  , readAttributeAttributeListWithParamsSelector
  , readAttributeClusterRevisionWithParamsSelector
  , readAttributeEndpointBindingListWithParamsSelector
  , readAttributeEndpointGroupIDListWithParamsSelector
  , readAttributeFeatureMapWithParamsSelector
  , readAttributeFriendlyNameWithParamsSelector
  , readAttributeGeneratedCommandListWithParamsSelector
  , readAttributeGroupKeySetListWithParamsSelector
  , readAttributeGroupListWithParamsSelector
  , readAttributeNodeACLListWithParamsSelector
  , readAttributeNodeEndpointListWithParamsSelector
  , readAttributeNodeKeySetListWithParamsSelector
  , readAttributeNodeListWithParamsSelector
  , readAttributeStatusWithParamsSelector
  , refreshNodeWithParams_expectedValues_expectedValueInterval_completionSelector
  , removeACLFromNodeWithParams_expectedValues_expectedValueInterval_completionSelector
  , removeAdminWithParams_expectedValues_expectedValueInterval_completionSelector
  , removeBindingFromEndpointForNodeWithParams_expectedValues_expectedValueInterval_completionSelector
  , removeGroupIDFromEndpointForNodeWithParams_expectedValues_expectedValueInterval_completionSelector
  , removeGroupWithParams_expectedValues_expectedValueInterval_completionSelector
  , removeKeySetWithParams_expectedValues_expectedValueInterval_completionSelector
  , removeNodeWithParams_expectedValues_expectedValueInterval_completionSelector
  , updateAdminWithParams_expectedValues_expectedValueInterval_completionSelector
  , updateEndpointForNodeWithParams_expectedValues_expectedValueInterval_completionSelector
  , updateGroupWithParams_expectedValues_expectedValueInterval_completionSelector
  , updateKeySetWithParams_expectedValues_expectedValueInterval_completionSelector
  , updateNodeWithParams_expectedValues_expectedValueInterval_completionSelector


  ) where

import Foreign.Ptr (Ptr, FunPtr)
import Foreign.C.Types

import ObjC.Runtime.Types
import ObjC.Runtime.Message (sendMessage, sendOwnedMessage, sendClassMessage, sendOwnedClassMessage)
import ObjC.Runtime.Selector (mkSelector)
import ObjC.Runtime.Class (getRequiredClass)

import ObjC.Matter.Internal.Classes
import ObjC.Foundation.Internal.Classes

-- | @- addKeySetWithParams:expectedValues:expectedValueInterval:completion:@
addKeySetWithParams_expectedValues_expectedValueInterval_completion :: (IsMTRClusterJointFabricDatastore mtrClusterJointFabricDatastore, IsMTRJointFabricDatastoreClusterAddKeySetParams params, IsNSArray expectedDataValueDictionaries, IsNSNumber expectedValueIntervalMs) => mtrClusterJointFabricDatastore -> params -> expectedDataValueDictionaries -> expectedValueIntervalMs -> Ptr () -> IO ()
addKeySetWithParams_expectedValues_expectedValueInterval_completion mtrClusterJointFabricDatastore params expectedDataValueDictionaries expectedValueIntervalMs completion =
  sendMessage mtrClusterJointFabricDatastore addKeySetWithParams_expectedValues_expectedValueInterval_completionSelector (toMTRJointFabricDatastoreClusterAddKeySetParams params) (toNSArray expectedDataValueDictionaries) (toNSNumber expectedValueIntervalMs) completion

-- | @- updateKeySetWithParams:expectedValues:expectedValueInterval:completion:@
updateKeySetWithParams_expectedValues_expectedValueInterval_completion :: (IsMTRClusterJointFabricDatastore mtrClusterJointFabricDatastore, IsMTRJointFabricDatastoreClusterUpdateKeySetParams params, IsNSArray expectedDataValueDictionaries, IsNSNumber expectedValueIntervalMs) => mtrClusterJointFabricDatastore -> params -> expectedDataValueDictionaries -> expectedValueIntervalMs -> Ptr () -> IO ()
updateKeySetWithParams_expectedValues_expectedValueInterval_completion mtrClusterJointFabricDatastore params expectedDataValueDictionaries expectedValueIntervalMs completion =
  sendMessage mtrClusterJointFabricDatastore updateKeySetWithParams_expectedValues_expectedValueInterval_completionSelector (toMTRJointFabricDatastoreClusterUpdateKeySetParams params) (toNSArray expectedDataValueDictionaries) (toNSNumber expectedValueIntervalMs) completion

-- | @- removeKeySetWithParams:expectedValues:expectedValueInterval:completion:@
removeKeySetWithParams_expectedValues_expectedValueInterval_completion :: (IsMTRClusterJointFabricDatastore mtrClusterJointFabricDatastore, IsMTRJointFabricDatastoreClusterRemoveKeySetParams params, IsNSArray expectedDataValueDictionaries, IsNSNumber expectedValueIntervalMs) => mtrClusterJointFabricDatastore -> params -> expectedDataValueDictionaries -> expectedValueIntervalMs -> Ptr () -> IO ()
removeKeySetWithParams_expectedValues_expectedValueInterval_completion mtrClusterJointFabricDatastore params expectedDataValueDictionaries expectedValueIntervalMs completion =
  sendMessage mtrClusterJointFabricDatastore removeKeySetWithParams_expectedValues_expectedValueInterval_completionSelector (toMTRJointFabricDatastoreClusterRemoveKeySetParams params) (toNSArray expectedDataValueDictionaries) (toNSNumber expectedValueIntervalMs) completion

-- | @- addGroupWithParams:expectedValues:expectedValueInterval:completion:@
addGroupWithParams_expectedValues_expectedValueInterval_completion :: (IsMTRClusterJointFabricDatastore mtrClusterJointFabricDatastore, IsMTRJointFabricDatastoreClusterAddGroupParams params, IsNSArray expectedDataValueDictionaries, IsNSNumber expectedValueIntervalMs) => mtrClusterJointFabricDatastore -> params -> expectedDataValueDictionaries -> expectedValueIntervalMs -> Ptr () -> IO ()
addGroupWithParams_expectedValues_expectedValueInterval_completion mtrClusterJointFabricDatastore params expectedDataValueDictionaries expectedValueIntervalMs completion =
  sendMessage mtrClusterJointFabricDatastore addGroupWithParams_expectedValues_expectedValueInterval_completionSelector (toMTRJointFabricDatastoreClusterAddGroupParams params) (toNSArray expectedDataValueDictionaries) (toNSNumber expectedValueIntervalMs) completion

-- | @- updateGroupWithParams:expectedValues:expectedValueInterval:completion:@
updateGroupWithParams_expectedValues_expectedValueInterval_completion :: (IsMTRClusterJointFabricDatastore mtrClusterJointFabricDatastore, IsMTRJointFabricDatastoreClusterUpdateGroupParams params, IsNSArray expectedDataValueDictionaries, IsNSNumber expectedValueIntervalMs) => mtrClusterJointFabricDatastore -> params -> expectedDataValueDictionaries -> expectedValueIntervalMs -> Ptr () -> IO ()
updateGroupWithParams_expectedValues_expectedValueInterval_completion mtrClusterJointFabricDatastore params expectedDataValueDictionaries expectedValueIntervalMs completion =
  sendMessage mtrClusterJointFabricDatastore updateGroupWithParams_expectedValues_expectedValueInterval_completionSelector (toMTRJointFabricDatastoreClusterUpdateGroupParams params) (toNSArray expectedDataValueDictionaries) (toNSNumber expectedValueIntervalMs) completion

-- | @- removeGroupWithParams:expectedValues:expectedValueInterval:completion:@
removeGroupWithParams_expectedValues_expectedValueInterval_completion :: (IsMTRClusterJointFabricDatastore mtrClusterJointFabricDatastore, IsMTRJointFabricDatastoreClusterRemoveGroupParams params, IsNSArray expectedDataValueDictionaries, IsNSNumber expectedValueIntervalMs) => mtrClusterJointFabricDatastore -> params -> expectedDataValueDictionaries -> expectedValueIntervalMs -> Ptr () -> IO ()
removeGroupWithParams_expectedValues_expectedValueInterval_completion mtrClusterJointFabricDatastore params expectedDataValueDictionaries expectedValueIntervalMs completion =
  sendMessage mtrClusterJointFabricDatastore removeGroupWithParams_expectedValues_expectedValueInterval_completionSelector (toMTRJointFabricDatastoreClusterRemoveGroupParams params) (toNSArray expectedDataValueDictionaries) (toNSNumber expectedValueIntervalMs) completion

-- | @- addAdminWithParams:expectedValues:expectedValueInterval:completion:@
addAdminWithParams_expectedValues_expectedValueInterval_completion :: (IsMTRClusterJointFabricDatastore mtrClusterJointFabricDatastore, IsMTRJointFabricDatastoreClusterAddAdminParams params, IsNSArray expectedDataValueDictionaries, IsNSNumber expectedValueIntervalMs) => mtrClusterJointFabricDatastore -> params -> expectedDataValueDictionaries -> expectedValueIntervalMs -> Ptr () -> IO ()
addAdminWithParams_expectedValues_expectedValueInterval_completion mtrClusterJointFabricDatastore params expectedDataValueDictionaries expectedValueIntervalMs completion =
  sendMessage mtrClusterJointFabricDatastore addAdminWithParams_expectedValues_expectedValueInterval_completionSelector (toMTRJointFabricDatastoreClusterAddAdminParams params) (toNSArray expectedDataValueDictionaries) (toNSNumber expectedValueIntervalMs) completion

-- | @- updateAdminWithParams:expectedValues:expectedValueInterval:completion:@
updateAdminWithParams_expectedValues_expectedValueInterval_completion :: (IsMTRClusterJointFabricDatastore mtrClusterJointFabricDatastore, IsMTRJointFabricDatastoreClusterUpdateAdminParams params, IsNSArray expectedDataValueDictionaries, IsNSNumber expectedValueIntervalMs) => mtrClusterJointFabricDatastore -> params -> expectedDataValueDictionaries -> expectedValueIntervalMs -> Ptr () -> IO ()
updateAdminWithParams_expectedValues_expectedValueInterval_completion mtrClusterJointFabricDatastore params expectedDataValueDictionaries expectedValueIntervalMs completion =
  sendMessage mtrClusterJointFabricDatastore updateAdminWithParams_expectedValues_expectedValueInterval_completionSelector (toMTRJointFabricDatastoreClusterUpdateAdminParams params) (toNSArray expectedDataValueDictionaries) (toNSNumber expectedValueIntervalMs) completion

-- | @- removeAdminWithParams:expectedValues:expectedValueInterval:completion:@
removeAdminWithParams_expectedValues_expectedValueInterval_completion :: (IsMTRClusterJointFabricDatastore mtrClusterJointFabricDatastore, IsMTRJointFabricDatastoreClusterRemoveAdminParams params, IsNSArray expectedDataValueDictionaries, IsNSNumber expectedValueIntervalMs) => mtrClusterJointFabricDatastore -> params -> expectedDataValueDictionaries -> expectedValueIntervalMs -> Ptr () -> IO ()
removeAdminWithParams_expectedValues_expectedValueInterval_completion mtrClusterJointFabricDatastore params expectedDataValueDictionaries expectedValueIntervalMs completion =
  sendMessage mtrClusterJointFabricDatastore removeAdminWithParams_expectedValues_expectedValueInterval_completionSelector (toMTRJointFabricDatastoreClusterRemoveAdminParams params) (toNSArray expectedDataValueDictionaries) (toNSNumber expectedValueIntervalMs) completion

-- | @- addPendingNodeWithParams:expectedValues:expectedValueInterval:completion:@
addPendingNodeWithParams_expectedValues_expectedValueInterval_completion :: (IsMTRClusterJointFabricDatastore mtrClusterJointFabricDatastore, IsMTRJointFabricDatastoreClusterAddPendingNodeParams params, IsNSArray expectedDataValueDictionaries, IsNSNumber expectedValueIntervalMs) => mtrClusterJointFabricDatastore -> params -> expectedDataValueDictionaries -> expectedValueIntervalMs -> Ptr () -> IO ()
addPendingNodeWithParams_expectedValues_expectedValueInterval_completion mtrClusterJointFabricDatastore params expectedDataValueDictionaries expectedValueIntervalMs completion =
  sendMessage mtrClusterJointFabricDatastore addPendingNodeWithParams_expectedValues_expectedValueInterval_completionSelector (toMTRJointFabricDatastoreClusterAddPendingNodeParams params) (toNSArray expectedDataValueDictionaries) (toNSNumber expectedValueIntervalMs) completion

-- | @- refreshNodeWithParams:expectedValues:expectedValueInterval:completion:@
refreshNodeWithParams_expectedValues_expectedValueInterval_completion :: (IsMTRClusterJointFabricDatastore mtrClusterJointFabricDatastore, IsMTRJointFabricDatastoreClusterRefreshNodeParams params, IsNSArray expectedDataValueDictionaries, IsNSNumber expectedValueIntervalMs) => mtrClusterJointFabricDatastore -> params -> expectedDataValueDictionaries -> expectedValueIntervalMs -> Ptr () -> IO ()
refreshNodeWithParams_expectedValues_expectedValueInterval_completion mtrClusterJointFabricDatastore params expectedDataValueDictionaries expectedValueIntervalMs completion =
  sendMessage mtrClusterJointFabricDatastore refreshNodeWithParams_expectedValues_expectedValueInterval_completionSelector (toMTRJointFabricDatastoreClusterRefreshNodeParams params) (toNSArray expectedDataValueDictionaries) (toNSNumber expectedValueIntervalMs) completion

-- | @- updateNodeWithParams:expectedValues:expectedValueInterval:completion:@
updateNodeWithParams_expectedValues_expectedValueInterval_completion :: (IsMTRClusterJointFabricDatastore mtrClusterJointFabricDatastore, IsMTRJointFabricDatastoreClusterUpdateNodeParams params, IsNSArray expectedDataValueDictionaries, IsNSNumber expectedValueIntervalMs) => mtrClusterJointFabricDatastore -> params -> expectedDataValueDictionaries -> expectedValueIntervalMs -> Ptr () -> IO ()
updateNodeWithParams_expectedValues_expectedValueInterval_completion mtrClusterJointFabricDatastore params expectedDataValueDictionaries expectedValueIntervalMs completion =
  sendMessage mtrClusterJointFabricDatastore updateNodeWithParams_expectedValues_expectedValueInterval_completionSelector (toMTRJointFabricDatastoreClusterUpdateNodeParams params) (toNSArray expectedDataValueDictionaries) (toNSNumber expectedValueIntervalMs) completion

-- | @- removeNodeWithParams:expectedValues:expectedValueInterval:completion:@
removeNodeWithParams_expectedValues_expectedValueInterval_completion :: (IsMTRClusterJointFabricDatastore mtrClusterJointFabricDatastore, IsMTRJointFabricDatastoreClusterRemoveNodeParams params, IsNSArray expectedDataValueDictionaries, IsNSNumber expectedValueIntervalMs) => mtrClusterJointFabricDatastore -> params -> expectedDataValueDictionaries -> expectedValueIntervalMs -> Ptr () -> IO ()
removeNodeWithParams_expectedValues_expectedValueInterval_completion mtrClusterJointFabricDatastore params expectedDataValueDictionaries expectedValueIntervalMs completion =
  sendMessage mtrClusterJointFabricDatastore removeNodeWithParams_expectedValues_expectedValueInterval_completionSelector (toMTRJointFabricDatastoreClusterRemoveNodeParams params) (toNSArray expectedDataValueDictionaries) (toNSNumber expectedValueIntervalMs) completion

-- | @- updateEndpointForNodeWithParams:expectedValues:expectedValueInterval:completion:@
updateEndpointForNodeWithParams_expectedValues_expectedValueInterval_completion :: (IsMTRClusterJointFabricDatastore mtrClusterJointFabricDatastore, IsMTRJointFabricDatastoreClusterUpdateEndpointForNodeParams params, IsNSArray expectedDataValueDictionaries, IsNSNumber expectedValueIntervalMs) => mtrClusterJointFabricDatastore -> params -> expectedDataValueDictionaries -> expectedValueIntervalMs -> Ptr () -> IO ()
updateEndpointForNodeWithParams_expectedValues_expectedValueInterval_completion mtrClusterJointFabricDatastore params expectedDataValueDictionaries expectedValueIntervalMs completion =
  sendMessage mtrClusterJointFabricDatastore updateEndpointForNodeWithParams_expectedValues_expectedValueInterval_completionSelector (toMTRJointFabricDatastoreClusterUpdateEndpointForNodeParams params) (toNSArray expectedDataValueDictionaries) (toNSNumber expectedValueIntervalMs) completion

-- | @- addGroupIDToEndpointForNodeWithParams:expectedValues:expectedValueInterval:completion:@
addGroupIDToEndpointForNodeWithParams_expectedValues_expectedValueInterval_completion :: (IsMTRClusterJointFabricDatastore mtrClusterJointFabricDatastore, IsMTRJointFabricDatastoreClusterAddGroupIDToEndpointForNodeParams params, IsNSArray expectedDataValueDictionaries, IsNSNumber expectedValueIntervalMs) => mtrClusterJointFabricDatastore -> params -> expectedDataValueDictionaries -> expectedValueIntervalMs -> Ptr () -> IO ()
addGroupIDToEndpointForNodeWithParams_expectedValues_expectedValueInterval_completion mtrClusterJointFabricDatastore params expectedDataValueDictionaries expectedValueIntervalMs completion =
  sendMessage mtrClusterJointFabricDatastore addGroupIDToEndpointForNodeWithParams_expectedValues_expectedValueInterval_completionSelector (toMTRJointFabricDatastoreClusterAddGroupIDToEndpointForNodeParams params) (toNSArray expectedDataValueDictionaries) (toNSNumber expectedValueIntervalMs) completion

-- | @- removeGroupIDFromEndpointForNodeWithParams:expectedValues:expectedValueInterval:completion:@
removeGroupIDFromEndpointForNodeWithParams_expectedValues_expectedValueInterval_completion :: (IsMTRClusterJointFabricDatastore mtrClusterJointFabricDatastore, IsMTRJointFabricDatastoreClusterRemoveGroupIDFromEndpointForNodeParams params, IsNSArray expectedDataValueDictionaries, IsNSNumber expectedValueIntervalMs) => mtrClusterJointFabricDatastore -> params -> expectedDataValueDictionaries -> expectedValueIntervalMs -> Ptr () -> IO ()
removeGroupIDFromEndpointForNodeWithParams_expectedValues_expectedValueInterval_completion mtrClusterJointFabricDatastore params expectedDataValueDictionaries expectedValueIntervalMs completion =
  sendMessage mtrClusterJointFabricDatastore removeGroupIDFromEndpointForNodeWithParams_expectedValues_expectedValueInterval_completionSelector (toMTRJointFabricDatastoreClusterRemoveGroupIDFromEndpointForNodeParams params) (toNSArray expectedDataValueDictionaries) (toNSNumber expectedValueIntervalMs) completion

-- | @- addBindingToEndpointForNodeWithParams:expectedValues:expectedValueInterval:completion:@
addBindingToEndpointForNodeWithParams_expectedValues_expectedValueInterval_completion :: (IsMTRClusterJointFabricDatastore mtrClusterJointFabricDatastore, IsMTRJointFabricDatastoreClusterAddBindingToEndpointForNodeParams params, IsNSArray expectedDataValueDictionaries, IsNSNumber expectedValueIntervalMs) => mtrClusterJointFabricDatastore -> params -> expectedDataValueDictionaries -> expectedValueIntervalMs -> Ptr () -> IO ()
addBindingToEndpointForNodeWithParams_expectedValues_expectedValueInterval_completion mtrClusterJointFabricDatastore params expectedDataValueDictionaries expectedValueIntervalMs completion =
  sendMessage mtrClusterJointFabricDatastore addBindingToEndpointForNodeWithParams_expectedValues_expectedValueInterval_completionSelector (toMTRJointFabricDatastoreClusterAddBindingToEndpointForNodeParams params) (toNSArray expectedDataValueDictionaries) (toNSNumber expectedValueIntervalMs) completion

-- | @- removeBindingFromEndpointForNodeWithParams:expectedValues:expectedValueInterval:completion:@
removeBindingFromEndpointForNodeWithParams_expectedValues_expectedValueInterval_completion :: (IsMTRClusterJointFabricDatastore mtrClusterJointFabricDatastore, IsMTRJointFabricDatastoreClusterRemoveBindingFromEndpointForNodeParams params, IsNSArray expectedDataValueDictionaries, IsNSNumber expectedValueIntervalMs) => mtrClusterJointFabricDatastore -> params -> expectedDataValueDictionaries -> expectedValueIntervalMs -> Ptr () -> IO ()
removeBindingFromEndpointForNodeWithParams_expectedValues_expectedValueInterval_completion mtrClusterJointFabricDatastore params expectedDataValueDictionaries expectedValueIntervalMs completion =
  sendMessage mtrClusterJointFabricDatastore removeBindingFromEndpointForNodeWithParams_expectedValues_expectedValueInterval_completionSelector (toMTRJointFabricDatastoreClusterRemoveBindingFromEndpointForNodeParams params) (toNSArray expectedDataValueDictionaries) (toNSNumber expectedValueIntervalMs) completion

-- | @- addACLToNodeWithParams:expectedValues:expectedValueInterval:completion:@
addACLToNodeWithParams_expectedValues_expectedValueInterval_completion :: (IsMTRClusterJointFabricDatastore mtrClusterJointFabricDatastore, IsMTRJointFabricDatastoreClusterAddACLToNodeParams params, IsNSArray expectedDataValueDictionaries, IsNSNumber expectedValueIntervalMs) => mtrClusterJointFabricDatastore -> params -> expectedDataValueDictionaries -> expectedValueIntervalMs -> Ptr () -> IO ()
addACLToNodeWithParams_expectedValues_expectedValueInterval_completion mtrClusterJointFabricDatastore params expectedDataValueDictionaries expectedValueIntervalMs completion =
  sendMessage mtrClusterJointFabricDatastore addACLToNodeWithParams_expectedValues_expectedValueInterval_completionSelector (toMTRJointFabricDatastoreClusterAddACLToNodeParams params) (toNSArray expectedDataValueDictionaries) (toNSNumber expectedValueIntervalMs) completion

-- | @- removeACLFromNodeWithParams:expectedValues:expectedValueInterval:completion:@
removeACLFromNodeWithParams_expectedValues_expectedValueInterval_completion :: (IsMTRClusterJointFabricDatastore mtrClusterJointFabricDatastore, IsMTRJointFabricDatastoreClusterRemoveACLFromNodeParams params, IsNSArray expectedDataValueDictionaries, IsNSNumber expectedValueIntervalMs) => mtrClusterJointFabricDatastore -> params -> expectedDataValueDictionaries -> expectedValueIntervalMs -> Ptr () -> IO ()
removeACLFromNodeWithParams_expectedValues_expectedValueInterval_completion mtrClusterJointFabricDatastore params expectedDataValueDictionaries expectedValueIntervalMs completion =
  sendMessage mtrClusterJointFabricDatastore removeACLFromNodeWithParams_expectedValues_expectedValueInterval_completionSelector (toMTRJointFabricDatastoreClusterRemoveACLFromNodeParams params) (toNSArray expectedDataValueDictionaries) (toNSNumber expectedValueIntervalMs) completion

-- | @- readAttributeAnchorRootCAWithParams:@
readAttributeAnchorRootCAWithParams :: (IsMTRClusterJointFabricDatastore mtrClusterJointFabricDatastore, IsMTRReadParams params) => mtrClusterJointFabricDatastore -> params -> IO (Id NSDictionary)
readAttributeAnchorRootCAWithParams mtrClusterJointFabricDatastore params =
  sendMessage mtrClusterJointFabricDatastore readAttributeAnchorRootCAWithParamsSelector (toMTRReadParams params)

-- | @- readAttributeAnchorNodeIDWithParams:@
readAttributeAnchorNodeIDWithParams :: (IsMTRClusterJointFabricDatastore mtrClusterJointFabricDatastore, IsMTRReadParams params) => mtrClusterJointFabricDatastore -> params -> IO (Id NSDictionary)
readAttributeAnchorNodeIDWithParams mtrClusterJointFabricDatastore params =
  sendMessage mtrClusterJointFabricDatastore readAttributeAnchorNodeIDWithParamsSelector (toMTRReadParams params)

-- | @- readAttributeAnchorVendorIDWithParams:@
readAttributeAnchorVendorIDWithParams :: (IsMTRClusterJointFabricDatastore mtrClusterJointFabricDatastore, IsMTRReadParams params) => mtrClusterJointFabricDatastore -> params -> IO (Id NSDictionary)
readAttributeAnchorVendorIDWithParams mtrClusterJointFabricDatastore params =
  sendMessage mtrClusterJointFabricDatastore readAttributeAnchorVendorIDWithParamsSelector (toMTRReadParams params)

-- | @- readAttributeFriendlyNameWithParams:@
readAttributeFriendlyNameWithParams :: (IsMTRClusterJointFabricDatastore mtrClusterJointFabricDatastore, IsMTRReadParams params) => mtrClusterJointFabricDatastore -> params -> IO (Id NSDictionary)
readAttributeFriendlyNameWithParams mtrClusterJointFabricDatastore params =
  sendMessage mtrClusterJointFabricDatastore readAttributeFriendlyNameWithParamsSelector (toMTRReadParams params)

-- | @- readAttributeGroupKeySetListWithParams:@
readAttributeGroupKeySetListWithParams :: (IsMTRClusterJointFabricDatastore mtrClusterJointFabricDatastore, IsMTRReadParams params) => mtrClusterJointFabricDatastore -> params -> IO (Id NSDictionary)
readAttributeGroupKeySetListWithParams mtrClusterJointFabricDatastore params =
  sendMessage mtrClusterJointFabricDatastore readAttributeGroupKeySetListWithParamsSelector (toMTRReadParams params)

-- | @- readAttributeGroupListWithParams:@
readAttributeGroupListWithParams :: (IsMTRClusterJointFabricDatastore mtrClusterJointFabricDatastore, IsMTRReadParams params) => mtrClusterJointFabricDatastore -> params -> IO (Id NSDictionary)
readAttributeGroupListWithParams mtrClusterJointFabricDatastore params =
  sendMessage mtrClusterJointFabricDatastore readAttributeGroupListWithParamsSelector (toMTRReadParams params)

-- | @- readAttributeNodeListWithParams:@
readAttributeNodeListWithParams :: (IsMTRClusterJointFabricDatastore mtrClusterJointFabricDatastore, IsMTRReadParams params) => mtrClusterJointFabricDatastore -> params -> IO (Id NSDictionary)
readAttributeNodeListWithParams mtrClusterJointFabricDatastore params =
  sendMessage mtrClusterJointFabricDatastore readAttributeNodeListWithParamsSelector (toMTRReadParams params)

-- | @- readAttributeAdminListWithParams:@
readAttributeAdminListWithParams :: (IsMTRClusterJointFabricDatastore mtrClusterJointFabricDatastore, IsMTRReadParams params) => mtrClusterJointFabricDatastore -> params -> IO (Id NSDictionary)
readAttributeAdminListWithParams mtrClusterJointFabricDatastore params =
  sendMessage mtrClusterJointFabricDatastore readAttributeAdminListWithParamsSelector (toMTRReadParams params)

-- | @- readAttributeStatusWithParams:@
readAttributeStatusWithParams :: (IsMTRClusterJointFabricDatastore mtrClusterJointFabricDatastore, IsMTRReadParams params) => mtrClusterJointFabricDatastore -> params -> IO (Id NSDictionary)
readAttributeStatusWithParams mtrClusterJointFabricDatastore params =
  sendMessage mtrClusterJointFabricDatastore readAttributeStatusWithParamsSelector (toMTRReadParams params)

-- | @- readAttributeEndpointGroupIDListWithParams:@
readAttributeEndpointGroupIDListWithParams :: (IsMTRClusterJointFabricDatastore mtrClusterJointFabricDatastore, IsMTRReadParams params) => mtrClusterJointFabricDatastore -> params -> IO (Id NSDictionary)
readAttributeEndpointGroupIDListWithParams mtrClusterJointFabricDatastore params =
  sendMessage mtrClusterJointFabricDatastore readAttributeEndpointGroupIDListWithParamsSelector (toMTRReadParams params)

-- | @- readAttributeEndpointBindingListWithParams:@
readAttributeEndpointBindingListWithParams :: (IsMTRClusterJointFabricDatastore mtrClusterJointFabricDatastore, IsMTRReadParams params) => mtrClusterJointFabricDatastore -> params -> IO (Id NSDictionary)
readAttributeEndpointBindingListWithParams mtrClusterJointFabricDatastore params =
  sendMessage mtrClusterJointFabricDatastore readAttributeEndpointBindingListWithParamsSelector (toMTRReadParams params)

-- | @- readAttributeNodeKeySetListWithParams:@
readAttributeNodeKeySetListWithParams :: (IsMTRClusterJointFabricDatastore mtrClusterJointFabricDatastore, IsMTRReadParams params) => mtrClusterJointFabricDatastore -> params -> IO (Id NSDictionary)
readAttributeNodeKeySetListWithParams mtrClusterJointFabricDatastore params =
  sendMessage mtrClusterJointFabricDatastore readAttributeNodeKeySetListWithParamsSelector (toMTRReadParams params)

-- | @- readAttributeNodeACLListWithParams:@
readAttributeNodeACLListWithParams :: (IsMTRClusterJointFabricDatastore mtrClusterJointFabricDatastore, IsMTRReadParams params) => mtrClusterJointFabricDatastore -> params -> IO (Id NSDictionary)
readAttributeNodeACLListWithParams mtrClusterJointFabricDatastore params =
  sendMessage mtrClusterJointFabricDatastore readAttributeNodeACLListWithParamsSelector (toMTRReadParams params)

-- | @- readAttributeNodeEndpointListWithParams:@
readAttributeNodeEndpointListWithParams :: (IsMTRClusterJointFabricDatastore mtrClusterJointFabricDatastore, IsMTRReadParams params) => mtrClusterJointFabricDatastore -> params -> IO (Id NSDictionary)
readAttributeNodeEndpointListWithParams mtrClusterJointFabricDatastore params =
  sendMessage mtrClusterJointFabricDatastore readAttributeNodeEndpointListWithParamsSelector (toMTRReadParams params)

-- | @- readAttributeGeneratedCommandListWithParams:@
readAttributeGeneratedCommandListWithParams :: (IsMTRClusterJointFabricDatastore mtrClusterJointFabricDatastore, IsMTRReadParams params) => mtrClusterJointFabricDatastore -> params -> IO (Id NSDictionary)
readAttributeGeneratedCommandListWithParams mtrClusterJointFabricDatastore params =
  sendMessage mtrClusterJointFabricDatastore readAttributeGeneratedCommandListWithParamsSelector (toMTRReadParams params)

-- | @- readAttributeAcceptedCommandListWithParams:@
readAttributeAcceptedCommandListWithParams :: (IsMTRClusterJointFabricDatastore mtrClusterJointFabricDatastore, IsMTRReadParams params) => mtrClusterJointFabricDatastore -> params -> IO (Id NSDictionary)
readAttributeAcceptedCommandListWithParams mtrClusterJointFabricDatastore params =
  sendMessage mtrClusterJointFabricDatastore readAttributeAcceptedCommandListWithParamsSelector (toMTRReadParams params)

-- | @- readAttributeAttributeListWithParams:@
readAttributeAttributeListWithParams :: (IsMTRClusterJointFabricDatastore mtrClusterJointFabricDatastore, IsMTRReadParams params) => mtrClusterJointFabricDatastore -> params -> IO (Id NSDictionary)
readAttributeAttributeListWithParams mtrClusterJointFabricDatastore params =
  sendMessage mtrClusterJointFabricDatastore readAttributeAttributeListWithParamsSelector (toMTRReadParams params)

-- | @- readAttributeFeatureMapWithParams:@
readAttributeFeatureMapWithParams :: (IsMTRClusterJointFabricDatastore mtrClusterJointFabricDatastore, IsMTRReadParams params) => mtrClusterJointFabricDatastore -> params -> IO (Id NSDictionary)
readAttributeFeatureMapWithParams mtrClusterJointFabricDatastore params =
  sendMessage mtrClusterJointFabricDatastore readAttributeFeatureMapWithParamsSelector (toMTRReadParams params)

-- | @- readAttributeClusterRevisionWithParams:@
readAttributeClusterRevisionWithParams :: (IsMTRClusterJointFabricDatastore mtrClusterJointFabricDatastore, IsMTRReadParams params) => mtrClusterJointFabricDatastore -> params -> IO (Id NSDictionary)
readAttributeClusterRevisionWithParams mtrClusterJointFabricDatastore params =
  sendMessage mtrClusterJointFabricDatastore readAttributeClusterRevisionWithParamsSelector (toMTRReadParams params)

-- | @- init@
init_ :: IsMTRClusterJointFabricDatastore mtrClusterJointFabricDatastore => mtrClusterJointFabricDatastore -> IO (Id MTRClusterJointFabricDatastore)
init_ mtrClusterJointFabricDatastore =
  sendOwnedMessage mtrClusterJointFabricDatastore initSelector

-- | @+ new@
new :: IO (Id MTRClusterJointFabricDatastore)
new  =
  do
    cls' <- getRequiredClass "MTRClusterJointFabricDatastore"
    sendOwnedClassMessage cls' newSelector

-- | For all instance methods that take a completion (i.e. command invocations), the completion will be called on the provided queue.
--
-- ObjC selector: @- initWithDevice:endpointID:queue:@
initWithDevice_endpointID_queue :: (IsMTRClusterJointFabricDatastore mtrClusterJointFabricDatastore, IsMTRDevice device, IsNSNumber endpointID, IsNSObject queue) => mtrClusterJointFabricDatastore -> device -> endpointID -> queue -> IO (Id MTRClusterJointFabricDatastore)
initWithDevice_endpointID_queue mtrClusterJointFabricDatastore device endpointID queue =
  sendOwnedMessage mtrClusterJointFabricDatastore initWithDevice_endpointID_queueSelector (toMTRDevice device) (toNSNumber endpointID) (toNSObject queue)

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @addKeySetWithParams:expectedValues:expectedValueInterval:completion:@
addKeySetWithParams_expectedValues_expectedValueInterval_completionSelector :: Selector '[Id MTRJointFabricDatastoreClusterAddKeySetParams, Id NSArray, Id NSNumber, Ptr ()] ()
addKeySetWithParams_expectedValues_expectedValueInterval_completionSelector = mkSelector "addKeySetWithParams:expectedValues:expectedValueInterval:completion:"

-- | @Selector@ for @updateKeySetWithParams:expectedValues:expectedValueInterval:completion:@
updateKeySetWithParams_expectedValues_expectedValueInterval_completionSelector :: Selector '[Id MTRJointFabricDatastoreClusterUpdateKeySetParams, Id NSArray, Id NSNumber, Ptr ()] ()
updateKeySetWithParams_expectedValues_expectedValueInterval_completionSelector = mkSelector "updateKeySetWithParams:expectedValues:expectedValueInterval:completion:"

-- | @Selector@ for @removeKeySetWithParams:expectedValues:expectedValueInterval:completion:@
removeKeySetWithParams_expectedValues_expectedValueInterval_completionSelector :: Selector '[Id MTRJointFabricDatastoreClusterRemoveKeySetParams, Id NSArray, Id NSNumber, Ptr ()] ()
removeKeySetWithParams_expectedValues_expectedValueInterval_completionSelector = mkSelector "removeKeySetWithParams:expectedValues:expectedValueInterval:completion:"

-- | @Selector@ for @addGroupWithParams:expectedValues:expectedValueInterval:completion:@
addGroupWithParams_expectedValues_expectedValueInterval_completionSelector :: Selector '[Id MTRJointFabricDatastoreClusterAddGroupParams, Id NSArray, Id NSNumber, Ptr ()] ()
addGroupWithParams_expectedValues_expectedValueInterval_completionSelector = mkSelector "addGroupWithParams:expectedValues:expectedValueInterval:completion:"

-- | @Selector@ for @updateGroupWithParams:expectedValues:expectedValueInterval:completion:@
updateGroupWithParams_expectedValues_expectedValueInterval_completionSelector :: Selector '[Id MTRJointFabricDatastoreClusterUpdateGroupParams, Id NSArray, Id NSNumber, Ptr ()] ()
updateGroupWithParams_expectedValues_expectedValueInterval_completionSelector = mkSelector "updateGroupWithParams:expectedValues:expectedValueInterval:completion:"

-- | @Selector@ for @removeGroupWithParams:expectedValues:expectedValueInterval:completion:@
removeGroupWithParams_expectedValues_expectedValueInterval_completionSelector :: Selector '[Id MTRJointFabricDatastoreClusterRemoveGroupParams, Id NSArray, Id NSNumber, Ptr ()] ()
removeGroupWithParams_expectedValues_expectedValueInterval_completionSelector = mkSelector "removeGroupWithParams:expectedValues:expectedValueInterval:completion:"

-- | @Selector@ for @addAdminWithParams:expectedValues:expectedValueInterval:completion:@
addAdminWithParams_expectedValues_expectedValueInterval_completionSelector :: Selector '[Id MTRJointFabricDatastoreClusterAddAdminParams, Id NSArray, Id NSNumber, Ptr ()] ()
addAdminWithParams_expectedValues_expectedValueInterval_completionSelector = mkSelector "addAdminWithParams:expectedValues:expectedValueInterval:completion:"

-- | @Selector@ for @updateAdminWithParams:expectedValues:expectedValueInterval:completion:@
updateAdminWithParams_expectedValues_expectedValueInterval_completionSelector :: Selector '[Id MTRJointFabricDatastoreClusterUpdateAdminParams, Id NSArray, Id NSNumber, Ptr ()] ()
updateAdminWithParams_expectedValues_expectedValueInterval_completionSelector = mkSelector "updateAdminWithParams:expectedValues:expectedValueInterval:completion:"

-- | @Selector@ for @removeAdminWithParams:expectedValues:expectedValueInterval:completion:@
removeAdminWithParams_expectedValues_expectedValueInterval_completionSelector :: Selector '[Id MTRJointFabricDatastoreClusterRemoveAdminParams, Id NSArray, Id NSNumber, Ptr ()] ()
removeAdminWithParams_expectedValues_expectedValueInterval_completionSelector = mkSelector "removeAdminWithParams:expectedValues:expectedValueInterval:completion:"

-- | @Selector@ for @addPendingNodeWithParams:expectedValues:expectedValueInterval:completion:@
addPendingNodeWithParams_expectedValues_expectedValueInterval_completionSelector :: Selector '[Id MTRJointFabricDatastoreClusterAddPendingNodeParams, Id NSArray, Id NSNumber, Ptr ()] ()
addPendingNodeWithParams_expectedValues_expectedValueInterval_completionSelector = mkSelector "addPendingNodeWithParams:expectedValues:expectedValueInterval:completion:"

-- | @Selector@ for @refreshNodeWithParams:expectedValues:expectedValueInterval:completion:@
refreshNodeWithParams_expectedValues_expectedValueInterval_completionSelector :: Selector '[Id MTRJointFabricDatastoreClusterRefreshNodeParams, Id NSArray, Id NSNumber, Ptr ()] ()
refreshNodeWithParams_expectedValues_expectedValueInterval_completionSelector = mkSelector "refreshNodeWithParams:expectedValues:expectedValueInterval:completion:"

-- | @Selector@ for @updateNodeWithParams:expectedValues:expectedValueInterval:completion:@
updateNodeWithParams_expectedValues_expectedValueInterval_completionSelector :: Selector '[Id MTRJointFabricDatastoreClusterUpdateNodeParams, Id NSArray, Id NSNumber, Ptr ()] ()
updateNodeWithParams_expectedValues_expectedValueInterval_completionSelector = mkSelector "updateNodeWithParams:expectedValues:expectedValueInterval:completion:"

-- | @Selector@ for @removeNodeWithParams:expectedValues:expectedValueInterval:completion:@
removeNodeWithParams_expectedValues_expectedValueInterval_completionSelector :: Selector '[Id MTRJointFabricDatastoreClusterRemoveNodeParams, Id NSArray, Id NSNumber, Ptr ()] ()
removeNodeWithParams_expectedValues_expectedValueInterval_completionSelector = mkSelector "removeNodeWithParams:expectedValues:expectedValueInterval:completion:"

-- | @Selector@ for @updateEndpointForNodeWithParams:expectedValues:expectedValueInterval:completion:@
updateEndpointForNodeWithParams_expectedValues_expectedValueInterval_completionSelector :: Selector '[Id MTRJointFabricDatastoreClusterUpdateEndpointForNodeParams, Id NSArray, Id NSNumber, Ptr ()] ()
updateEndpointForNodeWithParams_expectedValues_expectedValueInterval_completionSelector = mkSelector "updateEndpointForNodeWithParams:expectedValues:expectedValueInterval:completion:"

-- | @Selector@ for @addGroupIDToEndpointForNodeWithParams:expectedValues:expectedValueInterval:completion:@
addGroupIDToEndpointForNodeWithParams_expectedValues_expectedValueInterval_completionSelector :: Selector '[Id MTRJointFabricDatastoreClusterAddGroupIDToEndpointForNodeParams, Id NSArray, Id NSNumber, Ptr ()] ()
addGroupIDToEndpointForNodeWithParams_expectedValues_expectedValueInterval_completionSelector = mkSelector "addGroupIDToEndpointForNodeWithParams:expectedValues:expectedValueInterval:completion:"

-- | @Selector@ for @removeGroupIDFromEndpointForNodeWithParams:expectedValues:expectedValueInterval:completion:@
removeGroupIDFromEndpointForNodeWithParams_expectedValues_expectedValueInterval_completionSelector :: Selector '[Id MTRJointFabricDatastoreClusterRemoveGroupIDFromEndpointForNodeParams, Id NSArray, Id NSNumber, Ptr ()] ()
removeGroupIDFromEndpointForNodeWithParams_expectedValues_expectedValueInterval_completionSelector = mkSelector "removeGroupIDFromEndpointForNodeWithParams:expectedValues:expectedValueInterval:completion:"

-- | @Selector@ for @addBindingToEndpointForNodeWithParams:expectedValues:expectedValueInterval:completion:@
addBindingToEndpointForNodeWithParams_expectedValues_expectedValueInterval_completionSelector :: Selector '[Id MTRJointFabricDatastoreClusterAddBindingToEndpointForNodeParams, Id NSArray, Id NSNumber, Ptr ()] ()
addBindingToEndpointForNodeWithParams_expectedValues_expectedValueInterval_completionSelector = mkSelector "addBindingToEndpointForNodeWithParams:expectedValues:expectedValueInterval:completion:"

-- | @Selector@ for @removeBindingFromEndpointForNodeWithParams:expectedValues:expectedValueInterval:completion:@
removeBindingFromEndpointForNodeWithParams_expectedValues_expectedValueInterval_completionSelector :: Selector '[Id MTRJointFabricDatastoreClusterRemoveBindingFromEndpointForNodeParams, Id NSArray, Id NSNumber, Ptr ()] ()
removeBindingFromEndpointForNodeWithParams_expectedValues_expectedValueInterval_completionSelector = mkSelector "removeBindingFromEndpointForNodeWithParams:expectedValues:expectedValueInterval:completion:"

-- | @Selector@ for @addACLToNodeWithParams:expectedValues:expectedValueInterval:completion:@
addACLToNodeWithParams_expectedValues_expectedValueInterval_completionSelector :: Selector '[Id MTRJointFabricDatastoreClusterAddACLToNodeParams, Id NSArray, Id NSNumber, Ptr ()] ()
addACLToNodeWithParams_expectedValues_expectedValueInterval_completionSelector = mkSelector "addACLToNodeWithParams:expectedValues:expectedValueInterval:completion:"

-- | @Selector@ for @removeACLFromNodeWithParams:expectedValues:expectedValueInterval:completion:@
removeACLFromNodeWithParams_expectedValues_expectedValueInterval_completionSelector :: Selector '[Id MTRJointFabricDatastoreClusterRemoveACLFromNodeParams, Id NSArray, Id NSNumber, Ptr ()] ()
removeACLFromNodeWithParams_expectedValues_expectedValueInterval_completionSelector = mkSelector "removeACLFromNodeWithParams:expectedValues:expectedValueInterval:completion:"

-- | @Selector@ for @readAttributeAnchorRootCAWithParams:@
readAttributeAnchorRootCAWithParamsSelector :: Selector '[Id MTRReadParams] (Id NSDictionary)
readAttributeAnchorRootCAWithParamsSelector = mkSelector "readAttributeAnchorRootCAWithParams:"

-- | @Selector@ for @readAttributeAnchorNodeIDWithParams:@
readAttributeAnchorNodeIDWithParamsSelector :: Selector '[Id MTRReadParams] (Id NSDictionary)
readAttributeAnchorNodeIDWithParamsSelector = mkSelector "readAttributeAnchorNodeIDWithParams:"

-- | @Selector@ for @readAttributeAnchorVendorIDWithParams:@
readAttributeAnchorVendorIDWithParamsSelector :: Selector '[Id MTRReadParams] (Id NSDictionary)
readAttributeAnchorVendorIDWithParamsSelector = mkSelector "readAttributeAnchorVendorIDWithParams:"

-- | @Selector@ for @readAttributeFriendlyNameWithParams:@
readAttributeFriendlyNameWithParamsSelector :: Selector '[Id MTRReadParams] (Id NSDictionary)
readAttributeFriendlyNameWithParamsSelector = mkSelector "readAttributeFriendlyNameWithParams:"

-- | @Selector@ for @readAttributeGroupKeySetListWithParams:@
readAttributeGroupKeySetListWithParamsSelector :: Selector '[Id MTRReadParams] (Id NSDictionary)
readAttributeGroupKeySetListWithParamsSelector = mkSelector "readAttributeGroupKeySetListWithParams:"

-- | @Selector@ for @readAttributeGroupListWithParams:@
readAttributeGroupListWithParamsSelector :: Selector '[Id MTRReadParams] (Id NSDictionary)
readAttributeGroupListWithParamsSelector = mkSelector "readAttributeGroupListWithParams:"

-- | @Selector@ for @readAttributeNodeListWithParams:@
readAttributeNodeListWithParamsSelector :: Selector '[Id MTRReadParams] (Id NSDictionary)
readAttributeNodeListWithParamsSelector = mkSelector "readAttributeNodeListWithParams:"

-- | @Selector@ for @readAttributeAdminListWithParams:@
readAttributeAdminListWithParamsSelector :: Selector '[Id MTRReadParams] (Id NSDictionary)
readAttributeAdminListWithParamsSelector = mkSelector "readAttributeAdminListWithParams:"

-- | @Selector@ for @readAttributeStatusWithParams:@
readAttributeStatusWithParamsSelector :: Selector '[Id MTRReadParams] (Id NSDictionary)
readAttributeStatusWithParamsSelector = mkSelector "readAttributeStatusWithParams:"

-- | @Selector@ for @readAttributeEndpointGroupIDListWithParams:@
readAttributeEndpointGroupIDListWithParamsSelector :: Selector '[Id MTRReadParams] (Id NSDictionary)
readAttributeEndpointGroupIDListWithParamsSelector = mkSelector "readAttributeEndpointGroupIDListWithParams:"

-- | @Selector@ for @readAttributeEndpointBindingListWithParams:@
readAttributeEndpointBindingListWithParamsSelector :: Selector '[Id MTRReadParams] (Id NSDictionary)
readAttributeEndpointBindingListWithParamsSelector = mkSelector "readAttributeEndpointBindingListWithParams:"

-- | @Selector@ for @readAttributeNodeKeySetListWithParams:@
readAttributeNodeKeySetListWithParamsSelector :: Selector '[Id MTRReadParams] (Id NSDictionary)
readAttributeNodeKeySetListWithParamsSelector = mkSelector "readAttributeNodeKeySetListWithParams:"

-- | @Selector@ for @readAttributeNodeACLListWithParams:@
readAttributeNodeACLListWithParamsSelector :: Selector '[Id MTRReadParams] (Id NSDictionary)
readAttributeNodeACLListWithParamsSelector = mkSelector "readAttributeNodeACLListWithParams:"

-- | @Selector@ for @readAttributeNodeEndpointListWithParams:@
readAttributeNodeEndpointListWithParamsSelector :: Selector '[Id MTRReadParams] (Id NSDictionary)
readAttributeNodeEndpointListWithParamsSelector = mkSelector "readAttributeNodeEndpointListWithParams:"

-- | @Selector@ for @readAttributeGeneratedCommandListWithParams:@
readAttributeGeneratedCommandListWithParamsSelector :: Selector '[Id MTRReadParams] (Id NSDictionary)
readAttributeGeneratedCommandListWithParamsSelector = mkSelector "readAttributeGeneratedCommandListWithParams:"

-- | @Selector@ for @readAttributeAcceptedCommandListWithParams:@
readAttributeAcceptedCommandListWithParamsSelector :: Selector '[Id MTRReadParams] (Id NSDictionary)
readAttributeAcceptedCommandListWithParamsSelector = mkSelector "readAttributeAcceptedCommandListWithParams:"

-- | @Selector@ for @readAttributeAttributeListWithParams:@
readAttributeAttributeListWithParamsSelector :: Selector '[Id MTRReadParams] (Id NSDictionary)
readAttributeAttributeListWithParamsSelector = mkSelector "readAttributeAttributeListWithParams:"

-- | @Selector@ for @readAttributeFeatureMapWithParams:@
readAttributeFeatureMapWithParamsSelector :: Selector '[Id MTRReadParams] (Id NSDictionary)
readAttributeFeatureMapWithParamsSelector = mkSelector "readAttributeFeatureMapWithParams:"

-- | @Selector@ for @readAttributeClusterRevisionWithParams:@
readAttributeClusterRevisionWithParamsSelector :: Selector '[Id MTRReadParams] (Id NSDictionary)
readAttributeClusterRevisionWithParamsSelector = mkSelector "readAttributeClusterRevisionWithParams:"

-- | @Selector@ for @init@
initSelector :: Selector '[] (Id MTRClusterJointFabricDatastore)
initSelector = mkSelector "init"

-- | @Selector@ for @new@
newSelector :: Selector '[] (Id MTRClusterJointFabricDatastore)
newSelector = mkSelector "new"

-- | @Selector@ for @initWithDevice:endpointID:queue:@
initWithDevice_endpointID_queueSelector :: Selector '[Id MTRDevice, Id NSNumber, Id NSObject] (Id MTRClusterJointFabricDatastore)
initWithDevice_endpointID_queueSelector = mkSelector "initWithDevice:endpointID:queue:"

