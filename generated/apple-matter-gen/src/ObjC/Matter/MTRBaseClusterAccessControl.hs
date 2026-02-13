{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Cluster Access Control
--
-- The Access Control Cluster exposes a data model view of a      Node's Access Control List (ACL), which codifies the rules used to manage      and enforce Access Control for the Node's endpoints and their associated      cluster instances.
--
-- Generated bindings for @MTRBaseClusterAccessControl@.
module ObjC.Matter.MTRBaseClusterAccessControl
  ( MTRBaseClusterAccessControl
  , IsMTRBaseClusterAccessControl(..)
  , reviewFabricRestrictionsWithParams_completion
  , readAttributeACLWithParams_completion
  , writeAttributeACLWithValue_completion
  , writeAttributeACLWithValue_params_completion
  , subscribeAttributeACLWithParams_subscriptionEstablished_reportHandler
  , readAttributeACLWithClusterStateCache_endpoint_queue_completion
  , readAttributeExtensionWithParams_completion
  , writeAttributeExtensionWithValue_completion
  , writeAttributeExtensionWithValue_params_completion
  , subscribeAttributeExtensionWithParams_subscriptionEstablished_reportHandler
  , readAttributeExtensionWithClusterStateCache_endpoint_queue_completion
  , readAttributeSubjectsPerAccessControlEntryWithCompletion
  , subscribeAttributeSubjectsPerAccessControlEntryWithParams_subscriptionEstablished_reportHandler
  , readAttributeSubjectsPerAccessControlEntryWithClusterStateCache_endpoint_queue_completion
  , readAttributeTargetsPerAccessControlEntryWithCompletion
  , subscribeAttributeTargetsPerAccessControlEntryWithParams_subscriptionEstablished_reportHandler
  , readAttributeTargetsPerAccessControlEntryWithClusterStateCache_endpoint_queue_completion
  , readAttributeAccessControlEntriesPerFabricWithCompletion
  , subscribeAttributeAccessControlEntriesPerFabricWithParams_subscriptionEstablished_reportHandler
  , readAttributeAccessControlEntriesPerFabricWithClusterStateCache_endpoint_queue_completion
  , readAttributeCommissioningARLWithCompletion
  , subscribeAttributeCommissioningARLWithParams_subscriptionEstablished_reportHandler
  , readAttributeCommissioningARLWithClusterStateCache_endpoint_queue_completion
  , readAttributeARLWithParams_completion
  , subscribeAttributeARLWithParams_subscriptionEstablished_reportHandler
  , readAttributeARLWithClusterStateCache_endpoint_queue_completion
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
  , readAttributeAclWithParams_completionHandler
  , writeAttributeAclWithValue_completionHandler
  , writeAttributeAclWithValue_params_completionHandler
  , subscribeAttributeAclWithMinInterval_maxInterval_params_subscriptionEstablished_reportHandler
  , readAttributeAclWithAttributeCache_endpoint_queue_completionHandler
  , readAttributeExtensionWithParams_completionHandler
  , writeAttributeExtensionWithValue_completionHandler
  , writeAttributeExtensionWithValue_params_completionHandler
  , subscribeAttributeExtensionWithMinInterval_maxInterval_params_subscriptionEstablished_reportHandler
  , readAttributeExtensionWithAttributeCache_endpoint_queue_completionHandler
  , readAttributeSubjectsPerAccessControlEntryWithCompletionHandler
  , subscribeAttributeSubjectsPerAccessControlEntryWithMinInterval_maxInterval_params_subscriptionEstablished_reportHandler
  , readAttributeSubjectsPerAccessControlEntryWithAttributeCache_endpoint_queue_completionHandler
  , readAttributeTargetsPerAccessControlEntryWithCompletionHandler
  , subscribeAttributeTargetsPerAccessControlEntryWithMinInterval_maxInterval_params_subscriptionEstablished_reportHandler
  , readAttributeTargetsPerAccessControlEntryWithAttributeCache_endpoint_queue_completionHandler
  , readAttributeAccessControlEntriesPerFabricWithCompletionHandler
  , subscribeAttributeAccessControlEntriesPerFabricWithMinInterval_maxInterval_params_subscriptionEstablished_reportHandler
  , readAttributeAccessControlEntriesPerFabricWithAttributeCache_endpoint_queue_completionHandler
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
  , newSelector
  , readAttributeACLWithClusterStateCache_endpoint_queue_completionSelector
  , readAttributeACLWithParams_completionSelector
  , readAttributeARLWithClusterStateCache_endpoint_queue_completionSelector
  , readAttributeARLWithParams_completionSelector
  , readAttributeAcceptedCommandListWithAttributeCache_endpoint_queue_completionHandlerSelector
  , readAttributeAcceptedCommandListWithClusterStateCache_endpoint_queue_completionSelector
  , readAttributeAcceptedCommandListWithCompletionHandlerSelector
  , readAttributeAcceptedCommandListWithCompletionSelector
  , readAttributeAccessControlEntriesPerFabricWithAttributeCache_endpoint_queue_completionHandlerSelector
  , readAttributeAccessControlEntriesPerFabricWithClusterStateCache_endpoint_queue_completionSelector
  , readAttributeAccessControlEntriesPerFabricWithCompletionHandlerSelector
  , readAttributeAccessControlEntriesPerFabricWithCompletionSelector
  , readAttributeAclWithAttributeCache_endpoint_queue_completionHandlerSelector
  , readAttributeAclWithParams_completionHandlerSelector
  , readAttributeAttributeListWithAttributeCache_endpoint_queue_completionHandlerSelector
  , readAttributeAttributeListWithClusterStateCache_endpoint_queue_completionSelector
  , readAttributeAttributeListWithCompletionHandlerSelector
  , readAttributeAttributeListWithCompletionSelector
  , readAttributeClusterRevisionWithAttributeCache_endpoint_queue_completionHandlerSelector
  , readAttributeClusterRevisionWithClusterStateCache_endpoint_queue_completionSelector
  , readAttributeClusterRevisionWithCompletionHandlerSelector
  , readAttributeClusterRevisionWithCompletionSelector
  , readAttributeCommissioningARLWithClusterStateCache_endpoint_queue_completionSelector
  , readAttributeCommissioningARLWithCompletionSelector
  , readAttributeExtensionWithAttributeCache_endpoint_queue_completionHandlerSelector
  , readAttributeExtensionWithClusterStateCache_endpoint_queue_completionSelector
  , readAttributeExtensionWithParams_completionHandlerSelector
  , readAttributeExtensionWithParams_completionSelector
  , readAttributeFeatureMapWithAttributeCache_endpoint_queue_completionHandlerSelector
  , readAttributeFeatureMapWithClusterStateCache_endpoint_queue_completionSelector
  , readAttributeFeatureMapWithCompletionHandlerSelector
  , readAttributeFeatureMapWithCompletionSelector
  , readAttributeGeneratedCommandListWithAttributeCache_endpoint_queue_completionHandlerSelector
  , readAttributeGeneratedCommandListWithClusterStateCache_endpoint_queue_completionSelector
  , readAttributeGeneratedCommandListWithCompletionHandlerSelector
  , readAttributeGeneratedCommandListWithCompletionSelector
  , readAttributeSubjectsPerAccessControlEntryWithAttributeCache_endpoint_queue_completionHandlerSelector
  , readAttributeSubjectsPerAccessControlEntryWithClusterStateCache_endpoint_queue_completionSelector
  , readAttributeSubjectsPerAccessControlEntryWithCompletionHandlerSelector
  , readAttributeSubjectsPerAccessControlEntryWithCompletionSelector
  , readAttributeTargetsPerAccessControlEntryWithAttributeCache_endpoint_queue_completionHandlerSelector
  , readAttributeTargetsPerAccessControlEntryWithClusterStateCache_endpoint_queue_completionSelector
  , readAttributeTargetsPerAccessControlEntryWithCompletionHandlerSelector
  , readAttributeTargetsPerAccessControlEntryWithCompletionSelector
  , reviewFabricRestrictionsWithParams_completionSelector
  , subscribeAttributeACLWithParams_subscriptionEstablished_reportHandlerSelector
  , subscribeAttributeARLWithParams_subscriptionEstablished_reportHandlerSelector
  , subscribeAttributeAcceptedCommandListWithMinInterval_maxInterval_params_subscriptionEstablished_reportHandlerSelector
  , subscribeAttributeAcceptedCommandListWithParams_subscriptionEstablished_reportHandlerSelector
  , subscribeAttributeAccessControlEntriesPerFabricWithMinInterval_maxInterval_params_subscriptionEstablished_reportHandlerSelector
  , subscribeAttributeAccessControlEntriesPerFabricWithParams_subscriptionEstablished_reportHandlerSelector
  , subscribeAttributeAclWithMinInterval_maxInterval_params_subscriptionEstablished_reportHandlerSelector
  , subscribeAttributeAttributeListWithMinInterval_maxInterval_params_subscriptionEstablished_reportHandlerSelector
  , subscribeAttributeAttributeListWithParams_subscriptionEstablished_reportHandlerSelector
  , subscribeAttributeClusterRevisionWithMinInterval_maxInterval_params_subscriptionEstablished_reportHandlerSelector
  , subscribeAttributeClusterRevisionWithParams_subscriptionEstablished_reportHandlerSelector
  , subscribeAttributeCommissioningARLWithParams_subscriptionEstablished_reportHandlerSelector
  , subscribeAttributeExtensionWithMinInterval_maxInterval_params_subscriptionEstablished_reportHandlerSelector
  , subscribeAttributeExtensionWithParams_subscriptionEstablished_reportHandlerSelector
  , subscribeAttributeFeatureMapWithMinInterval_maxInterval_params_subscriptionEstablished_reportHandlerSelector
  , subscribeAttributeFeatureMapWithParams_subscriptionEstablished_reportHandlerSelector
  , subscribeAttributeGeneratedCommandListWithMinInterval_maxInterval_params_subscriptionEstablished_reportHandlerSelector
  , subscribeAttributeGeneratedCommandListWithParams_subscriptionEstablished_reportHandlerSelector
  , subscribeAttributeSubjectsPerAccessControlEntryWithMinInterval_maxInterval_params_subscriptionEstablished_reportHandlerSelector
  , subscribeAttributeSubjectsPerAccessControlEntryWithParams_subscriptionEstablished_reportHandlerSelector
  , subscribeAttributeTargetsPerAccessControlEntryWithMinInterval_maxInterval_params_subscriptionEstablished_reportHandlerSelector
  , subscribeAttributeTargetsPerAccessControlEntryWithParams_subscriptionEstablished_reportHandlerSelector
  , writeAttributeACLWithValue_completionSelector
  , writeAttributeACLWithValue_params_completionSelector
  , writeAttributeAclWithValue_completionHandlerSelector
  , writeAttributeAclWithValue_params_completionHandlerSelector
  , writeAttributeExtensionWithValue_completionHandlerSelector
  , writeAttributeExtensionWithValue_completionSelector
  , writeAttributeExtensionWithValue_params_completionHandlerSelector
  , writeAttributeExtensionWithValue_params_completionSelector


  ) where

import Foreign.Ptr (Ptr, FunPtr)
import Foreign.C.Types

import ObjC.Runtime.Types
import ObjC.Runtime.Message (sendMessage, sendOwnedMessage, sendClassMessage, sendOwnedClassMessage)
import ObjC.Runtime.Selector (mkSelector)
import ObjC.Runtime.Class (getRequiredClass)

import ObjC.Matter.Internal.Classes
import ObjC.Foundation.Internal.Classes

-- | Command ReviewFabricRestrictions
--
-- This command signals to the service associated with the device vendor that the fabric administrator would like a review of the current restrictions on the accessing fabric.
--
-- ObjC selector: @- reviewFabricRestrictionsWithParams:completion:@
reviewFabricRestrictionsWithParams_completion :: (IsMTRBaseClusterAccessControl mtrBaseClusterAccessControl, IsMTRAccessControlClusterReviewFabricRestrictionsParams params) => mtrBaseClusterAccessControl -> params -> Ptr () -> IO ()
reviewFabricRestrictionsWithParams_completion mtrBaseClusterAccessControl params completion =
  sendMessage mtrBaseClusterAccessControl reviewFabricRestrictionsWithParams_completionSelector (toMTRAccessControlClusterReviewFabricRestrictionsParams params) completion

-- | @- readAttributeACLWithParams:completion:@
readAttributeACLWithParams_completion :: (IsMTRBaseClusterAccessControl mtrBaseClusterAccessControl, IsMTRReadParams params) => mtrBaseClusterAccessControl -> params -> Ptr () -> IO ()
readAttributeACLWithParams_completion mtrBaseClusterAccessControl params completion =
  sendMessage mtrBaseClusterAccessControl readAttributeACLWithParams_completionSelector (toMTRReadParams params) completion

-- | @- writeAttributeACLWithValue:completion:@
writeAttributeACLWithValue_completion :: (IsMTRBaseClusterAccessControl mtrBaseClusterAccessControl, IsNSArray value) => mtrBaseClusterAccessControl -> value -> Ptr () -> IO ()
writeAttributeACLWithValue_completion mtrBaseClusterAccessControl value completion =
  sendMessage mtrBaseClusterAccessControl writeAttributeACLWithValue_completionSelector (toNSArray value) completion

-- | @- writeAttributeACLWithValue:params:completion:@
writeAttributeACLWithValue_params_completion :: (IsMTRBaseClusterAccessControl mtrBaseClusterAccessControl, IsNSArray value, IsMTRWriteParams params) => mtrBaseClusterAccessControl -> value -> params -> Ptr () -> IO ()
writeAttributeACLWithValue_params_completion mtrBaseClusterAccessControl value params completion =
  sendMessage mtrBaseClusterAccessControl writeAttributeACLWithValue_params_completionSelector (toNSArray value) (toMTRWriteParams params) completion

-- | @- subscribeAttributeACLWithParams:subscriptionEstablished:reportHandler:@
subscribeAttributeACLWithParams_subscriptionEstablished_reportHandler :: (IsMTRBaseClusterAccessControl mtrBaseClusterAccessControl, IsMTRSubscribeParams params) => mtrBaseClusterAccessControl -> params -> Ptr () -> Ptr () -> IO ()
subscribeAttributeACLWithParams_subscriptionEstablished_reportHandler mtrBaseClusterAccessControl params subscriptionEstablished reportHandler =
  sendMessage mtrBaseClusterAccessControl subscribeAttributeACLWithParams_subscriptionEstablished_reportHandlerSelector (toMTRSubscribeParams params) subscriptionEstablished reportHandler

-- | @+ readAttributeACLWithClusterStateCache:endpoint:queue:completion:@
readAttributeACLWithClusterStateCache_endpoint_queue_completion :: (IsMTRClusterStateCacheContainer clusterStateCacheContainer, IsNSNumber endpoint, IsNSObject queue) => clusterStateCacheContainer -> endpoint -> queue -> Ptr () -> IO ()
readAttributeACLWithClusterStateCache_endpoint_queue_completion clusterStateCacheContainer endpoint queue completion =
  do
    cls' <- getRequiredClass "MTRBaseClusterAccessControl"
    sendClassMessage cls' readAttributeACLWithClusterStateCache_endpoint_queue_completionSelector (toMTRClusterStateCacheContainer clusterStateCacheContainer) (toNSNumber endpoint) (toNSObject queue) completion

-- | @- readAttributeExtensionWithParams:completion:@
readAttributeExtensionWithParams_completion :: (IsMTRBaseClusterAccessControl mtrBaseClusterAccessControl, IsMTRReadParams params) => mtrBaseClusterAccessControl -> params -> Ptr () -> IO ()
readAttributeExtensionWithParams_completion mtrBaseClusterAccessControl params completion =
  sendMessage mtrBaseClusterAccessControl readAttributeExtensionWithParams_completionSelector (toMTRReadParams params) completion

-- | @- writeAttributeExtensionWithValue:completion:@
writeAttributeExtensionWithValue_completion :: (IsMTRBaseClusterAccessControl mtrBaseClusterAccessControl, IsNSArray value) => mtrBaseClusterAccessControl -> value -> Ptr () -> IO ()
writeAttributeExtensionWithValue_completion mtrBaseClusterAccessControl value completion =
  sendMessage mtrBaseClusterAccessControl writeAttributeExtensionWithValue_completionSelector (toNSArray value) completion

-- | @- writeAttributeExtensionWithValue:params:completion:@
writeAttributeExtensionWithValue_params_completion :: (IsMTRBaseClusterAccessControl mtrBaseClusterAccessControl, IsNSArray value, IsMTRWriteParams params) => mtrBaseClusterAccessControl -> value -> params -> Ptr () -> IO ()
writeAttributeExtensionWithValue_params_completion mtrBaseClusterAccessControl value params completion =
  sendMessage mtrBaseClusterAccessControl writeAttributeExtensionWithValue_params_completionSelector (toNSArray value) (toMTRWriteParams params) completion

-- | @- subscribeAttributeExtensionWithParams:subscriptionEstablished:reportHandler:@
subscribeAttributeExtensionWithParams_subscriptionEstablished_reportHandler :: (IsMTRBaseClusterAccessControl mtrBaseClusterAccessControl, IsMTRSubscribeParams params) => mtrBaseClusterAccessControl -> params -> Ptr () -> Ptr () -> IO ()
subscribeAttributeExtensionWithParams_subscriptionEstablished_reportHandler mtrBaseClusterAccessControl params subscriptionEstablished reportHandler =
  sendMessage mtrBaseClusterAccessControl subscribeAttributeExtensionWithParams_subscriptionEstablished_reportHandlerSelector (toMTRSubscribeParams params) subscriptionEstablished reportHandler

-- | @+ readAttributeExtensionWithClusterStateCache:endpoint:queue:completion:@
readAttributeExtensionWithClusterStateCache_endpoint_queue_completion :: (IsMTRClusterStateCacheContainer clusterStateCacheContainer, IsNSNumber endpoint, IsNSObject queue) => clusterStateCacheContainer -> endpoint -> queue -> Ptr () -> IO ()
readAttributeExtensionWithClusterStateCache_endpoint_queue_completion clusterStateCacheContainer endpoint queue completion =
  do
    cls' <- getRequiredClass "MTRBaseClusterAccessControl"
    sendClassMessage cls' readAttributeExtensionWithClusterStateCache_endpoint_queue_completionSelector (toMTRClusterStateCacheContainer clusterStateCacheContainer) (toNSNumber endpoint) (toNSObject queue) completion

-- | @- readAttributeSubjectsPerAccessControlEntryWithCompletion:@
readAttributeSubjectsPerAccessControlEntryWithCompletion :: IsMTRBaseClusterAccessControl mtrBaseClusterAccessControl => mtrBaseClusterAccessControl -> Ptr () -> IO ()
readAttributeSubjectsPerAccessControlEntryWithCompletion mtrBaseClusterAccessControl completion =
  sendMessage mtrBaseClusterAccessControl readAttributeSubjectsPerAccessControlEntryWithCompletionSelector completion

-- | @- subscribeAttributeSubjectsPerAccessControlEntryWithParams:subscriptionEstablished:reportHandler:@
subscribeAttributeSubjectsPerAccessControlEntryWithParams_subscriptionEstablished_reportHandler :: (IsMTRBaseClusterAccessControl mtrBaseClusterAccessControl, IsMTRSubscribeParams params) => mtrBaseClusterAccessControl -> params -> Ptr () -> Ptr () -> IO ()
subscribeAttributeSubjectsPerAccessControlEntryWithParams_subscriptionEstablished_reportHandler mtrBaseClusterAccessControl params subscriptionEstablished reportHandler =
  sendMessage mtrBaseClusterAccessControl subscribeAttributeSubjectsPerAccessControlEntryWithParams_subscriptionEstablished_reportHandlerSelector (toMTRSubscribeParams params) subscriptionEstablished reportHandler

-- | @+ readAttributeSubjectsPerAccessControlEntryWithClusterStateCache:endpoint:queue:completion:@
readAttributeSubjectsPerAccessControlEntryWithClusterStateCache_endpoint_queue_completion :: (IsMTRClusterStateCacheContainer clusterStateCacheContainer, IsNSNumber endpoint, IsNSObject queue) => clusterStateCacheContainer -> endpoint -> queue -> Ptr () -> IO ()
readAttributeSubjectsPerAccessControlEntryWithClusterStateCache_endpoint_queue_completion clusterStateCacheContainer endpoint queue completion =
  do
    cls' <- getRequiredClass "MTRBaseClusterAccessControl"
    sendClassMessage cls' readAttributeSubjectsPerAccessControlEntryWithClusterStateCache_endpoint_queue_completionSelector (toMTRClusterStateCacheContainer clusterStateCacheContainer) (toNSNumber endpoint) (toNSObject queue) completion

-- | @- readAttributeTargetsPerAccessControlEntryWithCompletion:@
readAttributeTargetsPerAccessControlEntryWithCompletion :: IsMTRBaseClusterAccessControl mtrBaseClusterAccessControl => mtrBaseClusterAccessControl -> Ptr () -> IO ()
readAttributeTargetsPerAccessControlEntryWithCompletion mtrBaseClusterAccessControl completion =
  sendMessage mtrBaseClusterAccessControl readAttributeTargetsPerAccessControlEntryWithCompletionSelector completion

-- | @- subscribeAttributeTargetsPerAccessControlEntryWithParams:subscriptionEstablished:reportHandler:@
subscribeAttributeTargetsPerAccessControlEntryWithParams_subscriptionEstablished_reportHandler :: (IsMTRBaseClusterAccessControl mtrBaseClusterAccessControl, IsMTRSubscribeParams params) => mtrBaseClusterAccessControl -> params -> Ptr () -> Ptr () -> IO ()
subscribeAttributeTargetsPerAccessControlEntryWithParams_subscriptionEstablished_reportHandler mtrBaseClusterAccessControl params subscriptionEstablished reportHandler =
  sendMessage mtrBaseClusterAccessControl subscribeAttributeTargetsPerAccessControlEntryWithParams_subscriptionEstablished_reportHandlerSelector (toMTRSubscribeParams params) subscriptionEstablished reportHandler

-- | @+ readAttributeTargetsPerAccessControlEntryWithClusterStateCache:endpoint:queue:completion:@
readAttributeTargetsPerAccessControlEntryWithClusterStateCache_endpoint_queue_completion :: (IsMTRClusterStateCacheContainer clusterStateCacheContainer, IsNSNumber endpoint, IsNSObject queue) => clusterStateCacheContainer -> endpoint -> queue -> Ptr () -> IO ()
readAttributeTargetsPerAccessControlEntryWithClusterStateCache_endpoint_queue_completion clusterStateCacheContainer endpoint queue completion =
  do
    cls' <- getRequiredClass "MTRBaseClusterAccessControl"
    sendClassMessage cls' readAttributeTargetsPerAccessControlEntryWithClusterStateCache_endpoint_queue_completionSelector (toMTRClusterStateCacheContainer clusterStateCacheContainer) (toNSNumber endpoint) (toNSObject queue) completion

-- | @- readAttributeAccessControlEntriesPerFabricWithCompletion:@
readAttributeAccessControlEntriesPerFabricWithCompletion :: IsMTRBaseClusterAccessControl mtrBaseClusterAccessControl => mtrBaseClusterAccessControl -> Ptr () -> IO ()
readAttributeAccessControlEntriesPerFabricWithCompletion mtrBaseClusterAccessControl completion =
  sendMessage mtrBaseClusterAccessControl readAttributeAccessControlEntriesPerFabricWithCompletionSelector completion

-- | @- subscribeAttributeAccessControlEntriesPerFabricWithParams:subscriptionEstablished:reportHandler:@
subscribeAttributeAccessControlEntriesPerFabricWithParams_subscriptionEstablished_reportHandler :: (IsMTRBaseClusterAccessControl mtrBaseClusterAccessControl, IsMTRSubscribeParams params) => mtrBaseClusterAccessControl -> params -> Ptr () -> Ptr () -> IO ()
subscribeAttributeAccessControlEntriesPerFabricWithParams_subscriptionEstablished_reportHandler mtrBaseClusterAccessControl params subscriptionEstablished reportHandler =
  sendMessage mtrBaseClusterAccessControl subscribeAttributeAccessControlEntriesPerFabricWithParams_subscriptionEstablished_reportHandlerSelector (toMTRSubscribeParams params) subscriptionEstablished reportHandler

-- | @+ readAttributeAccessControlEntriesPerFabricWithClusterStateCache:endpoint:queue:completion:@
readAttributeAccessControlEntriesPerFabricWithClusterStateCache_endpoint_queue_completion :: (IsMTRClusterStateCacheContainer clusterStateCacheContainer, IsNSNumber endpoint, IsNSObject queue) => clusterStateCacheContainer -> endpoint -> queue -> Ptr () -> IO ()
readAttributeAccessControlEntriesPerFabricWithClusterStateCache_endpoint_queue_completion clusterStateCacheContainer endpoint queue completion =
  do
    cls' <- getRequiredClass "MTRBaseClusterAccessControl"
    sendClassMessage cls' readAttributeAccessControlEntriesPerFabricWithClusterStateCache_endpoint_queue_completionSelector (toMTRClusterStateCacheContainer clusterStateCacheContainer) (toNSNumber endpoint) (toNSObject queue) completion

-- | @- readAttributeCommissioningARLWithCompletion:@
readAttributeCommissioningARLWithCompletion :: IsMTRBaseClusterAccessControl mtrBaseClusterAccessControl => mtrBaseClusterAccessControl -> Ptr () -> IO ()
readAttributeCommissioningARLWithCompletion mtrBaseClusterAccessControl completion =
  sendMessage mtrBaseClusterAccessControl readAttributeCommissioningARLWithCompletionSelector completion

-- | @- subscribeAttributeCommissioningARLWithParams:subscriptionEstablished:reportHandler:@
subscribeAttributeCommissioningARLWithParams_subscriptionEstablished_reportHandler :: (IsMTRBaseClusterAccessControl mtrBaseClusterAccessControl, IsMTRSubscribeParams params) => mtrBaseClusterAccessControl -> params -> Ptr () -> Ptr () -> IO ()
subscribeAttributeCommissioningARLWithParams_subscriptionEstablished_reportHandler mtrBaseClusterAccessControl params subscriptionEstablished reportHandler =
  sendMessage mtrBaseClusterAccessControl subscribeAttributeCommissioningARLWithParams_subscriptionEstablished_reportHandlerSelector (toMTRSubscribeParams params) subscriptionEstablished reportHandler

-- | @+ readAttributeCommissioningARLWithClusterStateCache:endpoint:queue:completion:@
readAttributeCommissioningARLWithClusterStateCache_endpoint_queue_completion :: (IsMTRClusterStateCacheContainer clusterStateCacheContainer, IsNSNumber endpoint, IsNSObject queue) => clusterStateCacheContainer -> endpoint -> queue -> Ptr () -> IO ()
readAttributeCommissioningARLWithClusterStateCache_endpoint_queue_completion clusterStateCacheContainer endpoint queue completion =
  do
    cls' <- getRequiredClass "MTRBaseClusterAccessControl"
    sendClassMessage cls' readAttributeCommissioningARLWithClusterStateCache_endpoint_queue_completionSelector (toMTRClusterStateCacheContainer clusterStateCacheContainer) (toNSNumber endpoint) (toNSObject queue) completion

-- | @- readAttributeARLWithParams:completion:@
readAttributeARLWithParams_completion :: (IsMTRBaseClusterAccessControl mtrBaseClusterAccessControl, IsMTRReadParams params) => mtrBaseClusterAccessControl -> params -> Ptr () -> IO ()
readAttributeARLWithParams_completion mtrBaseClusterAccessControl params completion =
  sendMessage mtrBaseClusterAccessControl readAttributeARLWithParams_completionSelector (toMTRReadParams params) completion

-- | @- subscribeAttributeARLWithParams:subscriptionEstablished:reportHandler:@
subscribeAttributeARLWithParams_subscriptionEstablished_reportHandler :: (IsMTRBaseClusterAccessControl mtrBaseClusterAccessControl, IsMTRSubscribeParams params) => mtrBaseClusterAccessControl -> params -> Ptr () -> Ptr () -> IO ()
subscribeAttributeARLWithParams_subscriptionEstablished_reportHandler mtrBaseClusterAccessControl params subscriptionEstablished reportHandler =
  sendMessage mtrBaseClusterAccessControl subscribeAttributeARLWithParams_subscriptionEstablished_reportHandlerSelector (toMTRSubscribeParams params) subscriptionEstablished reportHandler

-- | @+ readAttributeARLWithClusterStateCache:endpoint:queue:completion:@
readAttributeARLWithClusterStateCache_endpoint_queue_completion :: (IsMTRClusterStateCacheContainer clusterStateCacheContainer, IsNSNumber endpoint, IsNSObject queue) => clusterStateCacheContainer -> endpoint -> queue -> Ptr () -> IO ()
readAttributeARLWithClusterStateCache_endpoint_queue_completion clusterStateCacheContainer endpoint queue completion =
  do
    cls' <- getRequiredClass "MTRBaseClusterAccessControl"
    sendClassMessage cls' readAttributeARLWithClusterStateCache_endpoint_queue_completionSelector (toMTRClusterStateCacheContainer clusterStateCacheContainer) (toNSNumber endpoint) (toNSObject queue) completion

-- | @- readAttributeGeneratedCommandListWithCompletion:@
readAttributeGeneratedCommandListWithCompletion :: IsMTRBaseClusterAccessControl mtrBaseClusterAccessControl => mtrBaseClusterAccessControl -> Ptr () -> IO ()
readAttributeGeneratedCommandListWithCompletion mtrBaseClusterAccessControl completion =
  sendMessage mtrBaseClusterAccessControl readAttributeGeneratedCommandListWithCompletionSelector completion

-- | @- subscribeAttributeGeneratedCommandListWithParams:subscriptionEstablished:reportHandler:@
subscribeAttributeGeneratedCommandListWithParams_subscriptionEstablished_reportHandler :: (IsMTRBaseClusterAccessControl mtrBaseClusterAccessControl, IsMTRSubscribeParams params) => mtrBaseClusterAccessControl -> params -> Ptr () -> Ptr () -> IO ()
subscribeAttributeGeneratedCommandListWithParams_subscriptionEstablished_reportHandler mtrBaseClusterAccessControl params subscriptionEstablished reportHandler =
  sendMessage mtrBaseClusterAccessControl subscribeAttributeGeneratedCommandListWithParams_subscriptionEstablished_reportHandlerSelector (toMTRSubscribeParams params) subscriptionEstablished reportHandler

-- | @+ readAttributeGeneratedCommandListWithClusterStateCache:endpoint:queue:completion:@
readAttributeGeneratedCommandListWithClusterStateCache_endpoint_queue_completion :: (IsMTRClusterStateCacheContainer clusterStateCacheContainer, IsNSNumber endpoint, IsNSObject queue) => clusterStateCacheContainer -> endpoint -> queue -> Ptr () -> IO ()
readAttributeGeneratedCommandListWithClusterStateCache_endpoint_queue_completion clusterStateCacheContainer endpoint queue completion =
  do
    cls' <- getRequiredClass "MTRBaseClusterAccessControl"
    sendClassMessage cls' readAttributeGeneratedCommandListWithClusterStateCache_endpoint_queue_completionSelector (toMTRClusterStateCacheContainer clusterStateCacheContainer) (toNSNumber endpoint) (toNSObject queue) completion

-- | @- readAttributeAcceptedCommandListWithCompletion:@
readAttributeAcceptedCommandListWithCompletion :: IsMTRBaseClusterAccessControl mtrBaseClusterAccessControl => mtrBaseClusterAccessControl -> Ptr () -> IO ()
readAttributeAcceptedCommandListWithCompletion mtrBaseClusterAccessControl completion =
  sendMessage mtrBaseClusterAccessControl readAttributeAcceptedCommandListWithCompletionSelector completion

-- | @- subscribeAttributeAcceptedCommandListWithParams:subscriptionEstablished:reportHandler:@
subscribeAttributeAcceptedCommandListWithParams_subscriptionEstablished_reportHandler :: (IsMTRBaseClusterAccessControl mtrBaseClusterAccessControl, IsMTRSubscribeParams params) => mtrBaseClusterAccessControl -> params -> Ptr () -> Ptr () -> IO ()
subscribeAttributeAcceptedCommandListWithParams_subscriptionEstablished_reportHandler mtrBaseClusterAccessControl params subscriptionEstablished reportHandler =
  sendMessage mtrBaseClusterAccessControl subscribeAttributeAcceptedCommandListWithParams_subscriptionEstablished_reportHandlerSelector (toMTRSubscribeParams params) subscriptionEstablished reportHandler

-- | @+ readAttributeAcceptedCommandListWithClusterStateCache:endpoint:queue:completion:@
readAttributeAcceptedCommandListWithClusterStateCache_endpoint_queue_completion :: (IsMTRClusterStateCacheContainer clusterStateCacheContainer, IsNSNumber endpoint, IsNSObject queue) => clusterStateCacheContainer -> endpoint -> queue -> Ptr () -> IO ()
readAttributeAcceptedCommandListWithClusterStateCache_endpoint_queue_completion clusterStateCacheContainer endpoint queue completion =
  do
    cls' <- getRequiredClass "MTRBaseClusterAccessControl"
    sendClassMessage cls' readAttributeAcceptedCommandListWithClusterStateCache_endpoint_queue_completionSelector (toMTRClusterStateCacheContainer clusterStateCacheContainer) (toNSNumber endpoint) (toNSObject queue) completion

-- | @- readAttributeAttributeListWithCompletion:@
readAttributeAttributeListWithCompletion :: IsMTRBaseClusterAccessControl mtrBaseClusterAccessControl => mtrBaseClusterAccessControl -> Ptr () -> IO ()
readAttributeAttributeListWithCompletion mtrBaseClusterAccessControl completion =
  sendMessage mtrBaseClusterAccessControl readAttributeAttributeListWithCompletionSelector completion

-- | @- subscribeAttributeAttributeListWithParams:subscriptionEstablished:reportHandler:@
subscribeAttributeAttributeListWithParams_subscriptionEstablished_reportHandler :: (IsMTRBaseClusterAccessControl mtrBaseClusterAccessControl, IsMTRSubscribeParams params) => mtrBaseClusterAccessControl -> params -> Ptr () -> Ptr () -> IO ()
subscribeAttributeAttributeListWithParams_subscriptionEstablished_reportHandler mtrBaseClusterAccessControl params subscriptionEstablished reportHandler =
  sendMessage mtrBaseClusterAccessControl subscribeAttributeAttributeListWithParams_subscriptionEstablished_reportHandlerSelector (toMTRSubscribeParams params) subscriptionEstablished reportHandler

-- | @+ readAttributeAttributeListWithClusterStateCache:endpoint:queue:completion:@
readAttributeAttributeListWithClusterStateCache_endpoint_queue_completion :: (IsMTRClusterStateCacheContainer clusterStateCacheContainer, IsNSNumber endpoint, IsNSObject queue) => clusterStateCacheContainer -> endpoint -> queue -> Ptr () -> IO ()
readAttributeAttributeListWithClusterStateCache_endpoint_queue_completion clusterStateCacheContainer endpoint queue completion =
  do
    cls' <- getRequiredClass "MTRBaseClusterAccessControl"
    sendClassMessage cls' readAttributeAttributeListWithClusterStateCache_endpoint_queue_completionSelector (toMTRClusterStateCacheContainer clusterStateCacheContainer) (toNSNumber endpoint) (toNSObject queue) completion

-- | @- readAttributeFeatureMapWithCompletion:@
readAttributeFeatureMapWithCompletion :: IsMTRBaseClusterAccessControl mtrBaseClusterAccessControl => mtrBaseClusterAccessControl -> Ptr () -> IO ()
readAttributeFeatureMapWithCompletion mtrBaseClusterAccessControl completion =
  sendMessage mtrBaseClusterAccessControl readAttributeFeatureMapWithCompletionSelector completion

-- | @- subscribeAttributeFeatureMapWithParams:subscriptionEstablished:reportHandler:@
subscribeAttributeFeatureMapWithParams_subscriptionEstablished_reportHandler :: (IsMTRBaseClusterAccessControl mtrBaseClusterAccessControl, IsMTRSubscribeParams params) => mtrBaseClusterAccessControl -> params -> Ptr () -> Ptr () -> IO ()
subscribeAttributeFeatureMapWithParams_subscriptionEstablished_reportHandler mtrBaseClusterAccessControl params subscriptionEstablished reportHandler =
  sendMessage mtrBaseClusterAccessControl subscribeAttributeFeatureMapWithParams_subscriptionEstablished_reportHandlerSelector (toMTRSubscribeParams params) subscriptionEstablished reportHandler

-- | @+ readAttributeFeatureMapWithClusterStateCache:endpoint:queue:completion:@
readAttributeFeatureMapWithClusterStateCache_endpoint_queue_completion :: (IsMTRClusterStateCacheContainer clusterStateCacheContainer, IsNSNumber endpoint, IsNSObject queue) => clusterStateCacheContainer -> endpoint -> queue -> Ptr () -> IO ()
readAttributeFeatureMapWithClusterStateCache_endpoint_queue_completion clusterStateCacheContainer endpoint queue completion =
  do
    cls' <- getRequiredClass "MTRBaseClusterAccessControl"
    sendClassMessage cls' readAttributeFeatureMapWithClusterStateCache_endpoint_queue_completionSelector (toMTRClusterStateCacheContainer clusterStateCacheContainer) (toNSNumber endpoint) (toNSObject queue) completion

-- | @- readAttributeClusterRevisionWithCompletion:@
readAttributeClusterRevisionWithCompletion :: IsMTRBaseClusterAccessControl mtrBaseClusterAccessControl => mtrBaseClusterAccessControl -> Ptr () -> IO ()
readAttributeClusterRevisionWithCompletion mtrBaseClusterAccessControl completion =
  sendMessage mtrBaseClusterAccessControl readAttributeClusterRevisionWithCompletionSelector completion

-- | @- subscribeAttributeClusterRevisionWithParams:subscriptionEstablished:reportHandler:@
subscribeAttributeClusterRevisionWithParams_subscriptionEstablished_reportHandler :: (IsMTRBaseClusterAccessControl mtrBaseClusterAccessControl, IsMTRSubscribeParams params) => mtrBaseClusterAccessControl -> params -> Ptr () -> Ptr () -> IO ()
subscribeAttributeClusterRevisionWithParams_subscriptionEstablished_reportHandler mtrBaseClusterAccessControl params subscriptionEstablished reportHandler =
  sendMessage mtrBaseClusterAccessControl subscribeAttributeClusterRevisionWithParams_subscriptionEstablished_reportHandlerSelector (toMTRSubscribeParams params) subscriptionEstablished reportHandler

-- | @+ readAttributeClusterRevisionWithClusterStateCache:endpoint:queue:completion:@
readAttributeClusterRevisionWithClusterStateCache_endpoint_queue_completion :: (IsMTRClusterStateCacheContainer clusterStateCacheContainer, IsNSNumber endpoint, IsNSObject queue) => clusterStateCacheContainer -> endpoint -> queue -> Ptr () -> IO ()
readAttributeClusterRevisionWithClusterStateCache_endpoint_queue_completion clusterStateCacheContainer endpoint queue completion =
  do
    cls' <- getRequiredClass "MTRBaseClusterAccessControl"
    sendClassMessage cls' readAttributeClusterRevisionWithClusterStateCache_endpoint_queue_completionSelector (toMTRClusterStateCacheContainer clusterStateCacheContainer) (toNSNumber endpoint) (toNSObject queue) completion

-- | @- init@
init_ :: IsMTRBaseClusterAccessControl mtrBaseClusterAccessControl => mtrBaseClusterAccessControl -> IO (Id MTRBaseClusterAccessControl)
init_ mtrBaseClusterAccessControl =
  sendOwnedMessage mtrBaseClusterAccessControl initSelector

-- | @+ new@
new :: IO (Id MTRBaseClusterAccessControl)
new  =
  do
    cls' <- getRequiredClass "MTRBaseClusterAccessControl"
    sendOwnedClassMessage cls' newSelector

-- | @- initWithDevice:endpoint:queue:@
initWithDevice_endpoint_queue :: (IsMTRBaseClusterAccessControl mtrBaseClusterAccessControl, IsMTRBaseDevice device, IsNSObject queue) => mtrBaseClusterAccessControl -> device -> CUShort -> queue -> IO (Id MTRBaseClusterAccessControl)
initWithDevice_endpoint_queue mtrBaseClusterAccessControl device endpoint queue =
  sendOwnedMessage mtrBaseClusterAccessControl initWithDevice_endpoint_queueSelector (toMTRBaseDevice device) endpoint (toNSObject queue)

-- | @- readAttributeAclWithParams:completionHandler:@
readAttributeAclWithParams_completionHandler :: (IsMTRBaseClusterAccessControl mtrBaseClusterAccessControl, IsMTRReadParams params) => mtrBaseClusterAccessControl -> params -> Ptr () -> IO ()
readAttributeAclWithParams_completionHandler mtrBaseClusterAccessControl params completionHandler =
  sendMessage mtrBaseClusterAccessControl readAttributeAclWithParams_completionHandlerSelector (toMTRReadParams params) completionHandler

-- | @- writeAttributeAclWithValue:completionHandler:@
writeAttributeAclWithValue_completionHandler :: (IsMTRBaseClusterAccessControl mtrBaseClusterAccessControl, IsNSArray value) => mtrBaseClusterAccessControl -> value -> Ptr () -> IO ()
writeAttributeAclWithValue_completionHandler mtrBaseClusterAccessControl value completionHandler =
  sendMessage mtrBaseClusterAccessControl writeAttributeAclWithValue_completionHandlerSelector (toNSArray value) completionHandler

-- | @- writeAttributeAclWithValue:params:completionHandler:@
writeAttributeAclWithValue_params_completionHandler :: (IsMTRBaseClusterAccessControl mtrBaseClusterAccessControl, IsNSArray value, IsMTRWriteParams params) => mtrBaseClusterAccessControl -> value -> params -> Ptr () -> IO ()
writeAttributeAclWithValue_params_completionHandler mtrBaseClusterAccessControl value params completionHandler =
  sendMessage mtrBaseClusterAccessControl writeAttributeAclWithValue_params_completionHandlerSelector (toNSArray value) (toMTRWriteParams params) completionHandler

-- | @- subscribeAttributeAclWithMinInterval:maxInterval:params:subscriptionEstablished:reportHandler:@
subscribeAttributeAclWithMinInterval_maxInterval_params_subscriptionEstablished_reportHandler :: (IsMTRBaseClusterAccessControl mtrBaseClusterAccessControl, IsNSNumber minInterval, IsNSNumber maxInterval, IsMTRSubscribeParams params) => mtrBaseClusterAccessControl -> minInterval -> maxInterval -> params -> Ptr () -> Ptr () -> IO ()
subscribeAttributeAclWithMinInterval_maxInterval_params_subscriptionEstablished_reportHandler mtrBaseClusterAccessControl minInterval maxInterval params subscriptionEstablishedHandler reportHandler =
  sendMessage mtrBaseClusterAccessControl subscribeAttributeAclWithMinInterval_maxInterval_params_subscriptionEstablished_reportHandlerSelector (toNSNumber minInterval) (toNSNumber maxInterval) (toMTRSubscribeParams params) subscriptionEstablishedHandler reportHandler

-- | @+ readAttributeAclWithAttributeCache:endpoint:queue:completionHandler:@
readAttributeAclWithAttributeCache_endpoint_queue_completionHandler :: (IsMTRAttributeCacheContainer attributeCacheContainer, IsNSNumber endpoint, IsNSObject queue) => attributeCacheContainer -> endpoint -> queue -> Ptr () -> IO ()
readAttributeAclWithAttributeCache_endpoint_queue_completionHandler attributeCacheContainer endpoint queue completionHandler =
  do
    cls' <- getRequiredClass "MTRBaseClusterAccessControl"
    sendClassMessage cls' readAttributeAclWithAttributeCache_endpoint_queue_completionHandlerSelector (toMTRAttributeCacheContainer attributeCacheContainer) (toNSNumber endpoint) (toNSObject queue) completionHandler

-- | @- readAttributeExtensionWithParams:completionHandler:@
readAttributeExtensionWithParams_completionHandler :: (IsMTRBaseClusterAccessControl mtrBaseClusterAccessControl, IsMTRReadParams params) => mtrBaseClusterAccessControl -> params -> Ptr () -> IO ()
readAttributeExtensionWithParams_completionHandler mtrBaseClusterAccessControl params completionHandler =
  sendMessage mtrBaseClusterAccessControl readAttributeExtensionWithParams_completionHandlerSelector (toMTRReadParams params) completionHandler

-- | @- writeAttributeExtensionWithValue:completionHandler:@
writeAttributeExtensionWithValue_completionHandler :: (IsMTRBaseClusterAccessControl mtrBaseClusterAccessControl, IsNSArray value) => mtrBaseClusterAccessControl -> value -> Ptr () -> IO ()
writeAttributeExtensionWithValue_completionHandler mtrBaseClusterAccessControl value completionHandler =
  sendMessage mtrBaseClusterAccessControl writeAttributeExtensionWithValue_completionHandlerSelector (toNSArray value) completionHandler

-- | @- writeAttributeExtensionWithValue:params:completionHandler:@
writeAttributeExtensionWithValue_params_completionHandler :: (IsMTRBaseClusterAccessControl mtrBaseClusterAccessControl, IsNSArray value, IsMTRWriteParams params) => mtrBaseClusterAccessControl -> value -> params -> Ptr () -> IO ()
writeAttributeExtensionWithValue_params_completionHandler mtrBaseClusterAccessControl value params completionHandler =
  sendMessage mtrBaseClusterAccessControl writeAttributeExtensionWithValue_params_completionHandlerSelector (toNSArray value) (toMTRWriteParams params) completionHandler

-- | @- subscribeAttributeExtensionWithMinInterval:maxInterval:params:subscriptionEstablished:reportHandler:@
subscribeAttributeExtensionWithMinInterval_maxInterval_params_subscriptionEstablished_reportHandler :: (IsMTRBaseClusterAccessControl mtrBaseClusterAccessControl, IsNSNumber minInterval, IsNSNumber maxInterval, IsMTRSubscribeParams params) => mtrBaseClusterAccessControl -> minInterval -> maxInterval -> params -> Ptr () -> Ptr () -> IO ()
subscribeAttributeExtensionWithMinInterval_maxInterval_params_subscriptionEstablished_reportHandler mtrBaseClusterAccessControl minInterval maxInterval params subscriptionEstablishedHandler reportHandler =
  sendMessage mtrBaseClusterAccessControl subscribeAttributeExtensionWithMinInterval_maxInterval_params_subscriptionEstablished_reportHandlerSelector (toNSNumber minInterval) (toNSNumber maxInterval) (toMTRSubscribeParams params) subscriptionEstablishedHandler reportHandler

-- | @+ readAttributeExtensionWithAttributeCache:endpoint:queue:completionHandler:@
readAttributeExtensionWithAttributeCache_endpoint_queue_completionHandler :: (IsMTRAttributeCacheContainer attributeCacheContainer, IsNSNumber endpoint, IsNSObject queue) => attributeCacheContainer -> endpoint -> queue -> Ptr () -> IO ()
readAttributeExtensionWithAttributeCache_endpoint_queue_completionHandler attributeCacheContainer endpoint queue completionHandler =
  do
    cls' <- getRequiredClass "MTRBaseClusterAccessControl"
    sendClassMessage cls' readAttributeExtensionWithAttributeCache_endpoint_queue_completionHandlerSelector (toMTRAttributeCacheContainer attributeCacheContainer) (toNSNumber endpoint) (toNSObject queue) completionHandler

-- | @- readAttributeSubjectsPerAccessControlEntryWithCompletionHandler:@
readAttributeSubjectsPerAccessControlEntryWithCompletionHandler :: IsMTRBaseClusterAccessControl mtrBaseClusterAccessControl => mtrBaseClusterAccessControl -> Ptr () -> IO ()
readAttributeSubjectsPerAccessControlEntryWithCompletionHandler mtrBaseClusterAccessControl completionHandler =
  sendMessage mtrBaseClusterAccessControl readAttributeSubjectsPerAccessControlEntryWithCompletionHandlerSelector completionHandler

-- | @- subscribeAttributeSubjectsPerAccessControlEntryWithMinInterval:maxInterval:params:subscriptionEstablished:reportHandler:@
subscribeAttributeSubjectsPerAccessControlEntryWithMinInterval_maxInterval_params_subscriptionEstablished_reportHandler :: (IsMTRBaseClusterAccessControl mtrBaseClusterAccessControl, IsNSNumber minInterval, IsNSNumber maxInterval, IsMTRSubscribeParams params) => mtrBaseClusterAccessControl -> minInterval -> maxInterval -> params -> Ptr () -> Ptr () -> IO ()
subscribeAttributeSubjectsPerAccessControlEntryWithMinInterval_maxInterval_params_subscriptionEstablished_reportHandler mtrBaseClusterAccessControl minInterval maxInterval params subscriptionEstablishedHandler reportHandler =
  sendMessage mtrBaseClusterAccessControl subscribeAttributeSubjectsPerAccessControlEntryWithMinInterval_maxInterval_params_subscriptionEstablished_reportHandlerSelector (toNSNumber minInterval) (toNSNumber maxInterval) (toMTRSubscribeParams params) subscriptionEstablishedHandler reportHandler

-- | @+ readAttributeSubjectsPerAccessControlEntryWithAttributeCache:endpoint:queue:completionHandler:@
readAttributeSubjectsPerAccessControlEntryWithAttributeCache_endpoint_queue_completionHandler :: (IsMTRAttributeCacheContainer attributeCacheContainer, IsNSNumber endpoint, IsNSObject queue) => attributeCacheContainer -> endpoint -> queue -> Ptr () -> IO ()
readAttributeSubjectsPerAccessControlEntryWithAttributeCache_endpoint_queue_completionHandler attributeCacheContainer endpoint queue completionHandler =
  do
    cls' <- getRequiredClass "MTRBaseClusterAccessControl"
    sendClassMessage cls' readAttributeSubjectsPerAccessControlEntryWithAttributeCache_endpoint_queue_completionHandlerSelector (toMTRAttributeCacheContainer attributeCacheContainer) (toNSNumber endpoint) (toNSObject queue) completionHandler

-- | @- readAttributeTargetsPerAccessControlEntryWithCompletionHandler:@
readAttributeTargetsPerAccessControlEntryWithCompletionHandler :: IsMTRBaseClusterAccessControl mtrBaseClusterAccessControl => mtrBaseClusterAccessControl -> Ptr () -> IO ()
readAttributeTargetsPerAccessControlEntryWithCompletionHandler mtrBaseClusterAccessControl completionHandler =
  sendMessage mtrBaseClusterAccessControl readAttributeTargetsPerAccessControlEntryWithCompletionHandlerSelector completionHandler

-- | @- subscribeAttributeTargetsPerAccessControlEntryWithMinInterval:maxInterval:params:subscriptionEstablished:reportHandler:@
subscribeAttributeTargetsPerAccessControlEntryWithMinInterval_maxInterval_params_subscriptionEstablished_reportHandler :: (IsMTRBaseClusterAccessControl mtrBaseClusterAccessControl, IsNSNumber minInterval, IsNSNumber maxInterval, IsMTRSubscribeParams params) => mtrBaseClusterAccessControl -> minInterval -> maxInterval -> params -> Ptr () -> Ptr () -> IO ()
subscribeAttributeTargetsPerAccessControlEntryWithMinInterval_maxInterval_params_subscriptionEstablished_reportHandler mtrBaseClusterAccessControl minInterval maxInterval params subscriptionEstablishedHandler reportHandler =
  sendMessage mtrBaseClusterAccessControl subscribeAttributeTargetsPerAccessControlEntryWithMinInterval_maxInterval_params_subscriptionEstablished_reportHandlerSelector (toNSNumber minInterval) (toNSNumber maxInterval) (toMTRSubscribeParams params) subscriptionEstablishedHandler reportHandler

-- | @+ readAttributeTargetsPerAccessControlEntryWithAttributeCache:endpoint:queue:completionHandler:@
readAttributeTargetsPerAccessControlEntryWithAttributeCache_endpoint_queue_completionHandler :: (IsMTRAttributeCacheContainer attributeCacheContainer, IsNSNumber endpoint, IsNSObject queue) => attributeCacheContainer -> endpoint -> queue -> Ptr () -> IO ()
readAttributeTargetsPerAccessControlEntryWithAttributeCache_endpoint_queue_completionHandler attributeCacheContainer endpoint queue completionHandler =
  do
    cls' <- getRequiredClass "MTRBaseClusterAccessControl"
    sendClassMessage cls' readAttributeTargetsPerAccessControlEntryWithAttributeCache_endpoint_queue_completionHandlerSelector (toMTRAttributeCacheContainer attributeCacheContainer) (toNSNumber endpoint) (toNSObject queue) completionHandler

-- | @- readAttributeAccessControlEntriesPerFabricWithCompletionHandler:@
readAttributeAccessControlEntriesPerFabricWithCompletionHandler :: IsMTRBaseClusterAccessControl mtrBaseClusterAccessControl => mtrBaseClusterAccessControl -> Ptr () -> IO ()
readAttributeAccessControlEntriesPerFabricWithCompletionHandler mtrBaseClusterAccessControl completionHandler =
  sendMessage mtrBaseClusterAccessControl readAttributeAccessControlEntriesPerFabricWithCompletionHandlerSelector completionHandler

-- | @- subscribeAttributeAccessControlEntriesPerFabricWithMinInterval:maxInterval:params:subscriptionEstablished:reportHandler:@
subscribeAttributeAccessControlEntriesPerFabricWithMinInterval_maxInterval_params_subscriptionEstablished_reportHandler :: (IsMTRBaseClusterAccessControl mtrBaseClusterAccessControl, IsNSNumber minInterval, IsNSNumber maxInterval, IsMTRSubscribeParams params) => mtrBaseClusterAccessControl -> minInterval -> maxInterval -> params -> Ptr () -> Ptr () -> IO ()
subscribeAttributeAccessControlEntriesPerFabricWithMinInterval_maxInterval_params_subscriptionEstablished_reportHandler mtrBaseClusterAccessControl minInterval maxInterval params subscriptionEstablishedHandler reportHandler =
  sendMessage mtrBaseClusterAccessControl subscribeAttributeAccessControlEntriesPerFabricWithMinInterval_maxInterval_params_subscriptionEstablished_reportHandlerSelector (toNSNumber minInterval) (toNSNumber maxInterval) (toMTRSubscribeParams params) subscriptionEstablishedHandler reportHandler

-- | @+ readAttributeAccessControlEntriesPerFabricWithAttributeCache:endpoint:queue:completionHandler:@
readAttributeAccessControlEntriesPerFabricWithAttributeCache_endpoint_queue_completionHandler :: (IsMTRAttributeCacheContainer attributeCacheContainer, IsNSNumber endpoint, IsNSObject queue) => attributeCacheContainer -> endpoint -> queue -> Ptr () -> IO ()
readAttributeAccessControlEntriesPerFabricWithAttributeCache_endpoint_queue_completionHandler attributeCacheContainer endpoint queue completionHandler =
  do
    cls' <- getRequiredClass "MTRBaseClusterAccessControl"
    sendClassMessage cls' readAttributeAccessControlEntriesPerFabricWithAttributeCache_endpoint_queue_completionHandlerSelector (toMTRAttributeCacheContainer attributeCacheContainer) (toNSNumber endpoint) (toNSObject queue) completionHandler

-- | @- readAttributeGeneratedCommandListWithCompletionHandler:@
readAttributeGeneratedCommandListWithCompletionHandler :: IsMTRBaseClusterAccessControl mtrBaseClusterAccessControl => mtrBaseClusterAccessControl -> Ptr () -> IO ()
readAttributeGeneratedCommandListWithCompletionHandler mtrBaseClusterAccessControl completionHandler =
  sendMessage mtrBaseClusterAccessControl readAttributeGeneratedCommandListWithCompletionHandlerSelector completionHandler

-- | @- subscribeAttributeGeneratedCommandListWithMinInterval:maxInterval:params:subscriptionEstablished:reportHandler:@
subscribeAttributeGeneratedCommandListWithMinInterval_maxInterval_params_subscriptionEstablished_reportHandler :: (IsMTRBaseClusterAccessControl mtrBaseClusterAccessControl, IsNSNumber minInterval, IsNSNumber maxInterval, IsMTRSubscribeParams params) => mtrBaseClusterAccessControl -> minInterval -> maxInterval -> params -> Ptr () -> Ptr () -> IO ()
subscribeAttributeGeneratedCommandListWithMinInterval_maxInterval_params_subscriptionEstablished_reportHandler mtrBaseClusterAccessControl minInterval maxInterval params subscriptionEstablishedHandler reportHandler =
  sendMessage mtrBaseClusterAccessControl subscribeAttributeGeneratedCommandListWithMinInterval_maxInterval_params_subscriptionEstablished_reportHandlerSelector (toNSNumber minInterval) (toNSNumber maxInterval) (toMTRSubscribeParams params) subscriptionEstablishedHandler reportHandler

-- | @+ readAttributeGeneratedCommandListWithAttributeCache:endpoint:queue:completionHandler:@
readAttributeGeneratedCommandListWithAttributeCache_endpoint_queue_completionHandler :: (IsMTRAttributeCacheContainer attributeCacheContainer, IsNSNumber endpoint, IsNSObject queue) => attributeCacheContainer -> endpoint -> queue -> Ptr () -> IO ()
readAttributeGeneratedCommandListWithAttributeCache_endpoint_queue_completionHandler attributeCacheContainer endpoint queue completionHandler =
  do
    cls' <- getRequiredClass "MTRBaseClusterAccessControl"
    sendClassMessage cls' readAttributeGeneratedCommandListWithAttributeCache_endpoint_queue_completionHandlerSelector (toMTRAttributeCacheContainer attributeCacheContainer) (toNSNumber endpoint) (toNSObject queue) completionHandler

-- | @- readAttributeAcceptedCommandListWithCompletionHandler:@
readAttributeAcceptedCommandListWithCompletionHandler :: IsMTRBaseClusterAccessControl mtrBaseClusterAccessControl => mtrBaseClusterAccessControl -> Ptr () -> IO ()
readAttributeAcceptedCommandListWithCompletionHandler mtrBaseClusterAccessControl completionHandler =
  sendMessage mtrBaseClusterAccessControl readAttributeAcceptedCommandListWithCompletionHandlerSelector completionHandler

-- | @- subscribeAttributeAcceptedCommandListWithMinInterval:maxInterval:params:subscriptionEstablished:reportHandler:@
subscribeAttributeAcceptedCommandListWithMinInterval_maxInterval_params_subscriptionEstablished_reportHandler :: (IsMTRBaseClusterAccessControl mtrBaseClusterAccessControl, IsNSNumber minInterval, IsNSNumber maxInterval, IsMTRSubscribeParams params) => mtrBaseClusterAccessControl -> minInterval -> maxInterval -> params -> Ptr () -> Ptr () -> IO ()
subscribeAttributeAcceptedCommandListWithMinInterval_maxInterval_params_subscriptionEstablished_reportHandler mtrBaseClusterAccessControl minInterval maxInterval params subscriptionEstablishedHandler reportHandler =
  sendMessage mtrBaseClusterAccessControl subscribeAttributeAcceptedCommandListWithMinInterval_maxInterval_params_subscriptionEstablished_reportHandlerSelector (toNSNumber minInterval) (toNSNumber maxInterval) (toMTRSubscribeParams params) subscriptionEstablishedHandler reportHandler

-- | @+ readAttributeAcceptedCommandListWithAttributeCache:endpoint:queue:completionHandler:@
readAttributeAcceptedCommandListWithAttributeCache_endpoint_queue_completionHandler :: (IsMTRAttributeCacheContainer attributeCacheContainer, IsNSNumber endpoint, IsNSObject queue) => attributeCacheContainer -> endpoint -> queue -> Ptr () -> IO ()
readAttributeAcceptedCommandListWithAttributeCache_endpoint_queue_completionHandler attributeCacheContainer endpoint queue completionHandler =
  do
    cls' <- getRequiredClass "MTRBaseClusterAccessControl"
    sendClassMessage cls' readAttributeAcceptedCommandListWithAttributeCache_endpoint_queue_completionHandlerSelector (toMTRAttributeCacheContainer attributeCacheContainer) (toNSNumber endpoint) (toNSObject queue) completionHandler

-- | @- readAttributeAttributeListWithCompletionHandler:@
readAttributeAttributeListWithCompletionHandler :: IsMTRBaseClusterAccessControl mtrBaseClusterAccessControl => mtrBaseClusterAccessControl -> Ptr () -> IO ()
readAttributeAttributeListWithCompletionHandler mtrBaseClusterAccessControl completionHandler =
  sendMessage mtrBaseClusterAccessControl readAttributeAttributeListWithCompletionHandlerSelector completionHandler

-- | @- subscribeAttributeAttributeListWithMinInterval:maxInterval:params:subscriptionEstablished:reportHandler:@
subscribeAttributeAttributeListWithMinInterval_maxInterval_params_subscriptionEstablished_reportHandler :: (IsMTRBaseClusterAccessControl mtrBaseClusterAccessControl, IsNSNumber minInterval, IsNSNumber maxInterval, IsMTRSubscribeParams params) => mtrBaseClusterAccessControl -> minInterval -> maxInterval -> params -> Ptr () -> Ptr () -> IO ()
subscribeAttributeAttributeListWithMinInterval_maxInterval_params_subscriptionEstablished_reportHandler mtrBaseClusterAccessControl minInterval maxInterval params subscriptionEstablishedHandler reportHandler =
  sendMessage mtrBaseClusterAccessControl subscribeAttributeAttributeListWithMinInterval_maxInterval_params_subscriptionEstablished_reportHandlerSelector (toNSNumber minInterval) (toNSNumber maxInterval) (toMTRSubscribeParams params) subscriptionEstablishedHandler reportHandler

-- | @+ readAttributeAttributeListWithAttributeCache:endpoint:queue:completionHandler:@
readAttributeAttributeListWithAttributeCache_endpoint_queue_completionHandler :: (IsMTRAttributeCacheContainer attributeCacheContainer, IsNSNumber endpoint, IsNSObject queue) => attributeCacheContainer -> endpoint -> queue -> Ptr () -> IO ()
readAttributeAttributeListWithAttributeCache_endpoint_queue_completionHandler attributeCacheContainer endpoint queue completionHandler =
  do
    cls' <- getRequiredClass "MTRBaseClusterAccessControl"
    sendClassMessage cls' readAttributeAttributeListWithAttributeCache_endpoint_queue_completionHandlerSelector (toMTRAttributeCacheContainer attributeCacheContainer) (toNSNumber endpoint) (toNSObject queue) completionHandler

-- | @- readAttributeFeatureMapWithCompletionHandler:@
readAttributeFeatureMapWithCompletionHandler :: IsMTRBaseClusterAccessControl mtrBaseClusterAccessControl => mtrBaseClusterAccessControl -> Ptr () -> IO ()
readAttributeFeatureMapWithCompletionHandler mtrBaseClusterAccessControl completionHandler =
  sendMessage mtrBaseClusterAccessControl readAttributeFeatureMapWithCompletionHandlerSelector completionHandler

-- | @- subscribeAttributeFeatureMapWithMinInterval:maxInterval:params:subscriptionEstablished:reportHandler:@
subscribeAttributeFeatureMapWithMinInterval_maxInterval_params_subscriptionEstablished_reportHandler :: (IsMTRBaseClusterAccessControl mtrBaseClusterAccessControl, IsNSNumber minInterval, IsNSNumber maxInterval, IsMTRSubscribeParams params) => mtrBaseClusterAccessControl -> minInterval -> maxInterval -> params -> Ptr () -> Ptr () -> IO ()
subscribeAttributeFeatureMapWithMinInterval_maxInterval_params_subscriptionEstablished_reportHandler mtrBaseClusterAccessControl minInterval maxInterval params subscriptionEstablishedHandler reportHandler =
  sendMessage mtrBaseClusterAccessControl subscribeAttributeFeatureMapWithMinInterval_maxInterval_params_subscriptionEstablished_reportHandlerSelector (toNSNumber minInterval) (toNSNumber maxInterval) (toMTRSubscribeParams params) subscriptionEstablishedHandler reportHandler

-- | @+ readAttributeFeatureMapWithAttributeCache:endpoint:queue:completionHandler:@
readAttributeFeatureMapWithAttributeCache_endpoint_queue_completionHandler :: (IsMTRAttributeCacheContainer attributeCacheContainer, IsNSNumber endpoint, IsNSObject queue) => attributeCacheContainer -> endpoint -> queue -> Ptr () -> IO ()
readAttributeFeatureMapWithAttributeCache_endpoint_queue_completionHandler attributeCacheContainer endpoint queue completionHandler =
  do
    cls' <- getRequiredClass "MTRBaseClusterAccessControl"
    sendClassMessage cls' readAttributeFeatureMapWithAttributeCache_endpoint_queue_completionHandlerSelector (toMTRAttributeCacheContainer attributeCacheContainer) (toNSNumber endpoint) (toNSObject queue) completionHandler

-- | @- readAttributeClusterRevisionWithCompletionHandler:@
readAttributeClusterRevisionWithCompletionHandler :: IsMTRBaseClusterAccessControl mtrBaseClusterAccessControl => mtrBaseClusterAccessControl -> Ptr () -> IO ()
readAttributeClusterRevisionWithCompletionHandler mtrBaseClusterAccessControl completionHandler =
  sendMessage mtrBaseClusterAccessControl readAttributeClusterRevisionWithCompletionHandlerSelector completionHandler

-- | @- subscribeAttributeClusterRevisionWithMinInterval:maxInterval:params:subscriptionEstablished:reportHandler:@
subscribeAttributeClusterRevisionWithMinInterval_maxInterval_params_subscriptionEstablished_reportHandler :: (IsMTRBaseClusterAccessControl mtrBaseClusterAccessControl, IsNSNumber minInterval, IsNSNumber maxInterval, IsMTRSubscribeParams params) => mtrBaseClusterAccessControl -> minInterval -> maxInterval -> params -> Ptr () -> Ptr () -> IO ()
subscribeAttributeClusterRevisionWithMinInterval_maxInterval_params_subscriptionEstablished_reportHandler mtrBaseClusterAccessControl minInterval maxInterval params subscriptionEstablishedHandler reportHandler =
  sendMessage mtrBaseClusterAccessControl subscribeAttributeClusterRevisionWithMinInterval_maxInterval_params_subscriptionEstablished_reportHandlerSelector (toNSNumber minInterval) (toNSNumber maxInterval) (toMTRSubscribeParams params) subscriptionEstablishedHandler reportHandler

-- | @+ readAttributeClusterRevisionWithAttributeCache:endpoint:queue:completionHandler:@
readAttributeClusterRevisionWithAttributeCache_endpoint_queue_completionHandler :: (IsMTRAttributeCacheContainer attributeCacheContainer, IsNSNumber endpoint, IsNSObject queue) => attributeCacheContainer -> endpoint -> queue -> Ptr () -> IO ()
readAttributeClusterRevisionWithAttributeCache_endpoint_queue_completionHandler attributeCacheContainer endpoint queue completionHandler =
  do
    cls' <- getRequiredClass "MTRBaseClusterAccessControl"
    sendClassMessage cls' readAttributeClusterRevisionWithAttributeCache_endpoint_queue_completionHandlerSelector (toMTRAttributeCacheContainer attributeCacheContainer) (toNSNumber endpoint) (toNSObject queue) completionHandler

-- | For all instance methods (reads, writes, commands) that take a completion, the completion will be called on the provided queue.
--
-- ObjC selector: @- initWithDevice:endpointID:queue:@
initWithDevice_endpointID_queue :: (IsMTRBaseClusterAccessControl mtrBaseClusterAccessControl, IsMTRBaseDevice device, IsNSNumber endpointID, IsNSObject queue) => mtrBaseClusterAccessControl -> device -> endpointID -> queue -> IO (Id MTRBaseClusterAccessControl)
initWithDevice_endpointID_queue mtrBaseClusterAccessControl device endpointID queue =
  sendOwnedMessage mtrBaseClusterAccessControl initWithDevice_endpointID_queueSelector (toMTRBaseDevice device) (toNSNumber endpointID) (toNSObject queue)

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @reviewFabricRestrictionsWithParams:completion:@
reviewFabricRestrictionsWithParams_completionSelector :: Selector '[Id MTRAccessControlClusterReviewFabricRestrictionsParams, Ptr ()] ()
reviewFabricRestrictionsWithParams_completionSelector = mkSelector "reviewFabricRestrictionsWithParams:completion:"

-- | @Selector@ for @readAttributeACLWithParams:completion:@
readAttributeACLWithParams_completionSelector :: Selector '[Id MTRReadParams, Ptr ()] ()
readAttributeACLWithParams_completionSelector = mkSelector "readAttributeACLWithParams:completion:"

-- | @Selector@ for @writeAttributeACLWithValue:completion:@
writeAttributeACLWithValue_completionSelector :: Selector '[Id NSArray, Ptr ()] ()
writeAttributeACLWithValue_completionSelector = mkSelector "writeAttributeACLWithValue:completion:"

-- | @Selector@ for @writeAttributeACLWithValue:params:completion:@
writeAttributeACLWithValue_params_completionSelector :: Selector '[Id NSArray, Id MTRWriteParams, Ptr ()] ()
writeAttributeACLWithValue_params_completionSelector = mkSelector "writeAttributeACLWithValue:params:completion:"

-- | @Selector@ for @subscribeAttributeACLWithParams:subscriptionEstablished:reportHandler:@
subscribeAttributeACLWithParams_subscriptionEstablished_reportHandlerSelector :: Selector '[Id MTRSubscribeParams, Ptr (), Ptr ()] ()
subscribeAttributeACLWithParams_subscriptionEstablished_reportHandlerSelector = mkSelector "subscribeAttributeACLWithParams:subscriptionEstablished:reportHandler:"

-- | @Selector@ for @readAttributeACLWithClusterStateCache:endpoint:queue:completion:@
readAttributeACLWithClusterStateCache_endpoint_queue_completionSelector :: Selector '[Id MTRClusterStateCacheContainer, Id NSNumber, Id NSObject, Ptr ()] ()
readAttributeACLWithClusterStateCache_endpoint_queue_completionSelector = mkSelector "readAttributeACLWithClusterStateCache:endpoint:queue:completion:"

-- | @Selector@ for @readAttributeExtensionWithParams:completion:@
readAttributeExtensionWithParams_completionSelector :: Selector '[Id MTRReadParams, Ptr ()] ()
readAttributeExtensionWithParams_completionSelector = mkSelector "readAttributeExtensionWithParams:completion:"

-- | @Selector@ for @writeAttributeExtensionWithValue:completion:@
writeAttributeExtensionWithValue_completionSelector :: Selector '[Id NSArray, Ptr ()] ()
writeAttributeExtensionWithValue_completionSelector = mkSelector "writeAttributeExtensionWithValue:completion:"

-- | @Selector@ for @writeAttributeExtensionWithValue:params:completion:@
writeAttributeExtensionWithValue_params_completionSelector :: Selector '[Id NSArray, Id MTRWriteParams, Ptr ()] ()
writeAttributeExtensionWithValue_params_completionSelector = mkSelector "writeAttributeExtensionWithValue:params:completion:"

-- | @Selector@ for @subscribeAttributeExtensionWithParams:subscriptionEstablished:reportHandler:@
subscribeAttributeExtensionWithParams_subscriptionEstablished_reportHandlerSelector :: Selector '[Id MTRSubscribeParams, Ptr (), Ptr ()] ()
subscribeAttributeExtensionWithParams_subscriptionEstablished_reportHandlerSelector = mkSelector "subscribeAttributeExtensionWithParams:subscriptionEstablished:reportHandler:"

-- | @Selector@ for @readAttributeExtensionWithClusterStateCache:endpoint:queue:completion:@
readAttributeExtensionWithClusterStateCache_endpoint_queue_completionSelector :: Selector '[Id MTRClusterStateCacheContainer, Id NSNumber, Id NSObject, Ptr ()] ()
readAttributeExtensionWithClusterStateCache_endpoint_queue_completionSelector = mkSelector "readAttributeExtensionWithClusterStateCache:endpoint:queue:completion:"

-- | @Selector@ for @readAttributeSubjectsPerAccessControlEntryWithCompletion:@
readAttributeSubjectsPerAccessControlEntryWithCompletionSelector :: Selector '[Ptr ()] ()
readAttributeSubjectsPerAccessControlEntryWithCompletionSelector = mkSelector "readAttributeSubjectsPerAccessControlEntryWithCompletion:"

-- | @Selector@ for @subscribeAttributeSubjectsPerAccessControlEntryWithParams:subscriptionEstablished:reportHandler:@
subscribeAttributeSubjectsPerAccessControlEntryWithParams_subscriptionEstablished_reportHandlerSelector :: Selector '[Id MTRSubscribeParams, Ptr (), Ptr ()] ()
subscribeAttributeSubjectsPerAccessControlEntryWithParams_subscriptionEstablished_reportHandlerSelector = mkSelector "subscribeAttributeSubjectsPerAccessControlEntryWithParams:subscriptionEstablished:reportHandler:"

-- | @Selector@ for @readAttributeSubjectsPerAccessControlEntryWithClusterStateCache:endpoint:queue:completion:@
readAttributeSubjectsPerAccessControlEntryWithClusterStateCache_endpoint_queue_completionSelector :: Selector '[Id MTRClusterStateCacheContainer, Id NSNumber, Id NSObject, Ptr ()] ()
readAttributeSubjectsPerAccessControlEntryWithClusterStateCache_endpoint_queue_completionSelector = mkSelector "readAttributeSubjectsPerAccessControlEntryWithClusterStateCache:endpoint:queue:completion:"

-- | @Selector@ for @readAttributeTargetsPerAccessControlEntryWithCompletion:@
readAttributeTargetsPerAccessControlEntryWithCompletionSelector :: Selector '[Ptr ()] ()
readAttributeTargetsPerAccessControlEntryWithCompletionSelector = mkSelector "readAttributeTargetsPerAccessControlEntryWithCompletion:"

-- | @Selector@ for @subscribeAttributeTargetsPerAccessControlEntryWithParams:subscriptionEstablished:reportHandler:@
subscribeAttributeTargetsPerAccessControlEntryWithParams_subscriptionEstablished_reportHandlerSelector :: Selector '[Id MTRSubscribeParams, Ptr (), Ptr ()] ()
subscribeAttributeTargetsPerAccessControlEntryWithParams_subscriptionEstablished_reportHandlerSelector = mkSelector "subscribeAttributeTargetsPerAccessControlEntryWithParams:subscriptionEstablished:reportHandler:"

-- | @Selector@ for @readAttributeTargetsPerAccessControlEntryWithClusterStateCache:endpoint:queue:completion:@
readAttributeTargetsPerAccessControlEntryWithClusterStateCache_endpoint_queue_completionSelector :: Selector '[Id MTRClusterStateCacheContainer, Id NSNumber, Id NSObject, Ptr ()] ()
readAttributeTargetsPerAccessControlEntryWithClusterStateCache_endpoint_queue_completionSelector = mkSelector "readAttributeTargetsPerAccessControlEntryWithClusterStateCache:endpoint:queue:completion:"

-- | @Selector@ for @readAttributeAccessControlEntriesPerFabricWithCompletion:@
readAttributeAccessControlEntriesPerFabricWithCompletionSelector :: Selector '[Ptr ()] ()
readAttributeAccessControlEntriesPerFabricWithCompletionSelector = mkSelector "readAttributeAccessControlEntriesPerFabricWithCompletion:"

-- | @Selector@ for @subscribeAttributeAccessControlEntriesPerFabricWithParams:subscriptionEstablished:reportHandler:@
subscribeAttributeAccessControlEntriesPerFabricWithParams_subscriptionEstablished_reportHandlerSelector :: Selector '[Id MTRSubscribeParams, Ptr (), Ptr ()] ()
subscribeAttributeAccessControlEntriesPerFabricWithParams_subscriptionEstablished_reportHandlerSelector = mkSelector "subscribeAttributeAccessControlEntriesPerFabricWithParams:subscriptionEstablished:reportHandler:"

-- | @Selector@ for @readAttributeAccessControlEntriesPerFabricWithClusterStateCache:endpoint:queue:completion:@
readAttributeAccessControlEntriesPerFabricWithClusterStateCache_endpoint_queue_completionSelector :: Selector '[Id MTRClusterStateCacheContainer, Id NSNumber, Id NSObject, Ptr ()] ()
readAttributeAccessControlEntriesPerFabricWithClusterStateCache_endpoint_queue_completionSelector = mkSelector "readAttributeAccessControlEntriesPerFabricWithClusterStateCache:endpoint:queue:completion:"

-- | @Selector@ for @readAttributeCommissioningARLWithCompletion:@
readAttributeCommissioningARLWithCompletionSelector :: Selector '[Ptr ()] ()
readAttributeCommissioningARLWithCompletionSelector = mkSelector "readAttributeCommissioningARLWithCompletion:"

-- | @Selector@ for @subscribeAttributeCommissioningARLWithParams:subscriptionEstablished:reportHandler:@
subscribeAttributeCommissioningARLWithParams_subscriptionEstablished_reportHandlerSelector :: Selector '[Id MTRSubscribeParams, Ptr (), Ptr ()] ()
subscribeAttributeCommissioningARLWithParams_subscriptionEstablished_reportHandlerSelector = mkSelector "subscribeAttributeCommissioningARLWithParams:subscriptionEstablished:reportHandler:"

-- | @Selector@ for @readAttributeCommissioningARLWithClusterStateCache:endpoint:queue:completion:@
readAttributeCommissioningARLWithClusterStateCache_endpoint_queue_completionSelector :: Selector '[Id MTRClusterStateCacheContainer, Id NSNumber, Id NSObject, Ptr ()] ()
readAttributeCommissioningARLWithClusterStateCache_endpoint_queue_completionSelector = mkSelector "readAttributeCommissioningARLWithClusterStateCache:endpoint:queue:completion:"

-- | @Selector@ for @readAttributeARLWithParams:completion:@
readAttributeARLWithParams_completionSelector :: Selector '[Id MTRReadParams, Ptr ()] ()
readAttributeARLWithParams_completionSelector = mkSelector "readAttributeARLWithParams:completion:"

-- | @Selector@ for @subscribeAttributeARLWithParams:subscriptionEstablished:reportHandler:@
subscribeAttributeARLWithParams_subscriptionEstablished_reportHandlerSelector :: Selector '[Id MTRSubscribeParams, Ptr (), Ptr ()] ()
subscribeAttributeARLWithParams_subscriptionEstablished_reportHandlerSelector = mkSelector "subscribeAttributeARLWithParams:subscriptionEstablished:reportHandler:"

-- | @Selector@ for @readAttributeARLWithClusterStateCache:endpoint:queue:completion:@
readAttributeARLWithClusterStateCache_endpoint_queue_completionSelector :: Selector '[Id MTRClusterStateCacheContainer, Id NSNumber, Id NSObject, Ptr ()] ()
readAttributeARLWithClusterStateCache_endpoint_queue_completionSelector = mkSelector "readAttributeARLWithClusterStateCache:endpoint:queue:completion:"

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
initSelector :: Selector '[] (Id MTRBaseClusterAccessControl)
initSelector = mkSelector "init"

-- | @Selector@ for @new@
newSelector :: Selector '[] (Id MTRBaseClusterAccessControl)
newSelector = mkSelector "new"

-- | @Selector@ for @initWithDevice:endpoint:queue:@
initWithDevice_endpoint_queueSelector :: Selector '[Id MTRBaseDevice, CUShort, Id NSObject] (Id MTRBaseClusterAccessControl)
initWithDevice_endpoint_queueSelector = mkSelector "initWithDevice:endpoint:queue:"

-- | @Selector@ for @readAttributeAclWithParams:completionHandler:@
readAttributeAclWithParams_completionHandlerSelector :: Selector '[Id MTRReadParams, Ptr ()] ()
readAttributeAclWithParams_completionHandlerSelector = mkSelector "readAttributeAclWithParams:completionHandler:"

-- | @Selector@ for @writeAttributeAclWithValue:completionHandler:@
writeAttributeAclWithValue_completionHandlerSelector :: Selector '[Id NSArray, Ptr ()] ()
writeAttributeAclWithValue_completionHandlerSelector = mkSelector "writeAttributeAclWithValue:completionHandler:"

-- | @Selector@ for @writeAttributeAclWithValue:params:completionHandler:@
writeAttributeAclWithValue_params_completionHandlerSelector :: Selector '[Id NSArray, Id MTRWriteParams, Ptr ()] ()
writeAttributeAclWithValue_params_completionHandlerSelector = mkSelector "writeAttributeAclWithValue:params:completionHandler:"

-- | @Selector@ for @subscribeAttributeAclWithMinInterval:maxInterval:params:subscriptionEstablished:reportHandler:@
subscribeAttributeAclWithMinInterval_maxInterval_params_subscriptionEstablished_reportHandlerSelector :: Selector '[Id NSNumber, Id NSNumber, Id MTRSubscribeParams, Ptr (), Ptr ()] ()
subscribeAttributeAclWithMinInterval_maxInterval_params_subscriptionEstablished_reportHandlerSelector = mkSelector "subscribeAttributeAclWithMinInterval:maxInterval:params:subscriptionEstablished:reportHandler:"

-- | @Selector@ for @readAttributeAclWithAttributeCache:endpoint:queue:completionHandler:@
readAttributeAclWithAttributeCache_endpoint_queue_completionHandlerSelector :: Selector '[Id MTRAttributeCacheContainer, Id NSNumber, Id NSObject, Ptr ()] ()
readAttributeAclWithAttributeCache_endpoint_queue_completionHandlerSelector = mkSelector "readAttributeAclWithAttributeCache:endpoint:queue:completionHandler:"

-- | @Selector@ for @readAttributeExtensionWithParams:completionHandler:@
readAttributeExtensionWithParams_completionHandlerSelector :: Selector '[Id MTRReadParams, Ptr ()] ()
readAttributeExtensionWithParams_completionHandlerSelector = mkSelector "readAttributeExtensionWithParams:completionHandler:"

-- | @Selector@ for @writeAttributeExtensionWithValue:completionHandler:@
writeAttributeExtensionWithValue_completionHandlerSelector :: Selector '[Id NSArray, Ptr ()] ()
writeAttributeExtensionWithValue_completionHandlerSelector = mkSelector "writeAttributeExtensionWithValue:completionHandler:"

-- | @Selector@ for @writeAttributeExtensionWithValue:params:completionHandler:@
writeAttributeExtensionWithValue_params_completionHandlerSelector :: Selector '[Id NSArray, Id MTRWriteParams, Ptr ()] ()
writeAttributeExtensionWithValue_params_completionHandlerSelector = mkSelector "writeAttributeExtensionWithValue:params:completionHandler:"

-- | @Selector@ for @subscribeAttributeExtensionWithMinInterval:maxInterval:params:subscriptionEstablished:reportHandler:@
subscribeAttributeExtensionWithMinInterval_maxInterval_params_subscriptionEstablished_reportHandlerSelector :: Selector '[Id NSNumber, Id NSNumber, Id MTRSubscribeParams, Ptr (), Ptr ()] ()
subscribeAttributeExtensionWithMinInterval_maxInterval_params_subscriptionEstablished_reportHandlerSelector = mkSelector "subscribeAttributeExtensionWithMinInterval:maxInterval:params:subscriptionEstablished:reportHandler:"

-- | @Selector@ for @readAttributeExtensionWithAttributeCache:endpoint:queue:completionHandler:@
readAttributeExtensionWithAttributeCache_endpoint_queue_completionHandlerSelector :: Selector '[Id MTRAttributeCacheContainer, Id NSNumber, Id NSObject, Ptr ()] ()
readAttributeExtensionWithAttributeCache_endpoint_queue_completionHandlerSelector = mkSelector "readAttributeExtensionWithAttributeCache:endpoint:queue:completionHandler:"

-- | @Selector@ for @readAttributeSubjectsPerAccessControlEntryWithCompletionHandler:@
readAttributeSubjectsPerAccessControlEntryWithCompletionHandlerSelector :: Selector '[Ptr ()] ()
readAttributeSubjectsPerAccessControlEntryWithCompletionHandlerSelector = mkSelector "readAttributeSubjectsPerAccessControlEntryWithCompletionHandler:"

-- | @Selector@ for @subscribeAttributeSubjectsPerAccessControlEntryWithMinInterval:maxInterval:params:subscriptionEstablished:reportHandler:@
subscribeAttributeSubjectsPerAccessControlEntryWithMinInterval_maxInterval_params_subscriptionEstablished_reportHandlerSelector :: Selector '[Id NSNumber, Id NSNumber, Id MTRSubscribeParams, Ptr (), Ptr ()] ()
subscribeAttributeSubjectsPerAccessControlEntryWithMinInterval_maxInterval_params_subscriptionEstablished_reportHandlerSelector = mkSelector "subscribeAttributeSubjectsPerAccessControlEntryWithMinInterval:maxInterval:params:subscriptionEstablished:reportHandler:"

-- | @Selector@ for @readAttributeSubjectsPerAccessControlEntryWithAttributeCache:endpoint:queue:completionHandler:@
readAttributeSubjectsPerAccessControlEntryWithAttributeCache_endpoint_queue_completionHandlerSelector :: Selector '[Id MTRAttributeCacheContainer, Id NSNumber, Id NSObject, Ptr ()] ()
readAttributeSubjectsPerAccessControlEntryWithAttributeCache_endpoint_queue_completionHandlerSelector = mkSelector "readAttributeSubjectsPerAccessControlEntryWithAttributeCache:endpoint:queue:completionHandler:"

-- | @Selector@ for @readAttributeTargetsPerAccessControlEntryWithCompletionHandler:@
readAttributeTargetsPerAccessControlEntryWithCompletionHandlerSelector :: Selector '[Ptr ()] ()
readAttributeTargetsPerAccessControlEntryWithCompletionHandlerSelector = mkSelector "readAttributeTargetsPerAccessControlEntryWithCompletionHandler:"

-- | @Selector@ for @subscribeAttributeTargetsPerAccessControlEntryWithMinInterval:maxInterval:params:subscriptionEstablished:reportHandler:@
subscribeAttributeTargetsPerAccessControlEntryWithMinInterval_maxInterval_params_subscriptionEstablished_reportHandlerSelector :: Selector '[Id NSNumber, Id NSNumber, Id MTRSubscribeParams, Ptr (), Ptr ()] ()
subscribeAttributeTargetsPerAccessControlEntryWithMinInterval_maxInterval_params_subscriptionEstablished_reportHandlerSelector = mkSelector "subscribeAttributeTargetsPerAccessControlEntryWithMinInterval:maxInterval:params:subscriptionEstablished:reportHandler:"

-- | @Selector@ for @readAttributeTargetsPerAccessControlEntryWithAttributeCache:endpoint:queue:completionHandler:@
readAttributeTargetsPerAccessControlEntryWithAttributeCache_endpoint_queue_completionHandlerSelector :: Selector '[Id MTRAttributeCacheContainer, Id NSNumber, Id NSObject, Ptr ()] ()
readAttributeTargetsPerAccessControlEntryWithAttributeCache_endpoint_queue_completionHandlerSelector = mkSelector "readAttributeTargetsPerAccessControlEntryWithAttributeCache:endpoint:queue:completionHandler:"

-- | @Selector@ for @readAttributeAccessControlEntriesPerFabricWithCompletionHandler:@
readAttributeAccessControlEntriesPerFabricWithCompletionHandlerSelector :: Selector '[Ptr ()] ()
readAttributeAccessControlEntriesPerFabricWithCompletionHandlerSelector = mkSelector "readAttributeAccessControlEntriesPerFabricWithCompletionHandler:"

-- | @Selector@ for @subscribeAttributeAccessControlEntriesPerFabricWithMinInterval:maxInterval:params:subscriptionEstablished:reportHandler:@
subscribeAttributeAccessControlEntriesPerFabricWithMinInterval_maxInterval_params_subscriptionEstablished_reportHandlerSelector :: Selector '[Id NSNumber, Id NSNumber, Id MTRSubscribeParams, Ptr (), Ptr ()] ()
subscribeAttributeAccessControlEntriesPerFabricWithMinInterval_maxInterval_params_subscriptionEstablished_reportHandlerSelector = mkSelector "subscribeAttributeAccessControlEntriesPerFabricWithMinInterval:maxInterval:params:subscriptionEstablished:reportHandler:"

-- | @Selector@ for @readAttributeAccessControlEntriesPerFabricWithAttributeCache:endpoint:queue:completionHandler:@
readAttributeAccessControlEntriesPerFabricWithAttributeCache_endpoint_queue_completionHandlerSelector :: Selector '[Id MTRAttributeCacheContainer, Id NSNumber, Id NSObject, Ptr ()] ()
readAttributeAccessControlEntriesPerFabricWithAttributeCache_endpoint_queue_completionHandlerSelector = mkSelector "readAttributeAccessControlEntriesPerFabricWithAttributeCache:endpoint:queue:completionHandler:"

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
initWithDevice_endpointID_queueSelector :: Selector '[Id MTRBaseDevice, Id NSNumber, Id NSObject] (Id MTRBaseClusterAccessControl)
initWithDevice_endpointID_queueSelector = mkSelector "initWithDevice:endpointID:queue:"

