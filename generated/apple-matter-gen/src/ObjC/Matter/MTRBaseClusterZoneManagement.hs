{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Cluster Zone Management
--
-- This cluster provides an interface to manage regions of interest, or Zones, which can be either manufacturer or user defined.
--
-- Generated bindings for @MTRBaseClusterZoneManagement@.
module ObjC.Matter.MTRBaseClusterZoneManagement
  ( MTRBaseClusterZoneManagement
  , IsMTRBaseClusterZoneManagement(..)
  , createTwoDCartesianZoneWithParams_completion
  , updateTwoDCartesianZoneWithParams_completion
  , removeZoneWithParams_completion
  , createOrUpdateTriggerWithParams_completion
  , removeTriggerWithParams_completion
  , readAttributeMaxUserDefinedZonesWithCompletion
  , subscribeAttributeMaxUserDefinedZonesWithParams_subscriptionEstablished_reportHandler
  , readAttributeMaxUserDefinedZonesWithClusterStateCache_endpoint_queue_completion
  , readAttributeMaxZonesWithCompletion
  , subscribeAttributeMaxZonesWithParams_subscriptionEstablished_reportHandler
  , readAttributeMaxZonesWithClusterStateCache_endpoint_queue_completion
  , readAttributeZonesWithCompletion
  , subscribeAttributeZonesWithParams_subscriptionEstablished_reportHandler
  , readAttributeZonesWithClusterStateCache_endpoint_queue_completion
  , readAttributeTriggersWithCompletion
  , subscribeAttributeTriggersWithParams_subscriptionEstablished_reportHandler
  , readAttributeTriggersWithClusterStateCache_endpoint_queue_completion
  , readAttributeSensitivityMaxWithCompletion
  , subscribeAttributeSensitivityMaxWithParams_subscriptionEstablished_reportHandler
  , readAttributeSensitivityMaxWithClusterStateCache_endpoint_queue_completion
  , readAttributeSensitivityWithCompletion
  , writeAttributeSensitivityWithValue_completion
  , writeAttributeSensitivityWithValue_params_completion
  , subscribeAttributeSensitivityWithParams_subscriptionEstablished_reportHandler
  , readAttributeSensitivityWithClusterStateCache_endpoint_queue_completion
  , readAttributeTwoDCartesianMaxWithCompletion
  , subscribeAttributeTwoDCartesianMaxWithParams_subscriptionEstablished_reportHandler
  , readAttributeTwoDCartesianMaxWithClusterStateCache_endpoint_queue_completion
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
  , createOrUpdateTriggerWithParams_completionSelector
  , createTwoDCartesianZoneWithParams_completionSelector
  , initSelector
  , initWithDevice_endpointID_queueSelector
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
  , readAttributeMaxUserDefinedZonesWithClusterStateCache_endpoint_queue_completionSelector
  , readAttributeMaxUserDefinedZonesWithCompletionSelector
  , readAttributeMaxZonesWithClusterStateCache_endpoint_queue_completionSelector
  , readAttributeMaxZonesWithCompletionSelector
  , readAttributeSensitivityMaxWithClusterStateCache_endpoint_queue_completionSelector
  , readAttributeSensitivityMaxWithCompletionSelector
  , readAttributeSensitivityWithClusterStateCache_endpoint_queue_completionSelector
  , readAttributeSensitivityWithCompletionSelector
  , readAttributeTriggersWithClusterStateCache_endpoint_queue_completionSelector
  , readAttributeTriggersWithCompletionSelector
  , readAttributeTwoDCartesianMaxWithClusterStateCache_endpoint_queue_completionSelector
  , readAttributeTwoDCartesianMaxWithCompletionSelector
  , readAttributeZonesWithClusterStateCache_endpoint_queue_completionSelector
  , readAttributeZonesWithCompletionSelector
  , removeTriggerWithParams_completionSelector
  , removeZoneWithParams_completionSelector
  , subscribeAttributeAcceptedCommandListWithParams_subscriptionEstablished_reportHandlerSelector
  , subscribeAttributeAttributeListWithParams_subscriptionEstablished_reportHandlerSelector
  , subscribeAttributeClusterRevisionWithParams_subscriptionEstablished_reportHandlerSelector
  , subscribeAttributeFeatureMapWithParams_subscriptionEstablished_reportHandlerSelector
  , subscribeAttributeGeneratedCommandListWithParams_subscriptionEstablished_reportHandlerSelector
  , subscribeAttributeMaxUserDefinedZonesWithParams_subscriptionEstablished_reportHandlerSelector
  , subscribeAttributeMaxZonesWithParams_subscriptionEstablished_reportHandlerSelector
  , subscribeAttributeSensitivityMaxWithParams_subscriptionEstablished_reportHandlerSelector
  , subscribeAttributeSensitivityWithParams_subscriptionEstablished_reportHandlerSelector
  , subscribeAttributeTriggersWithParams_subscriptionEstablished_reportHandlerSelector
  , subscribeAttributeTwoDCartesianMaxWithParams_subscriptionEstablished_reportHandlerSelector
  , subscribeAttributeZonesWithParams_subscriptionEstablished_reportHandlerSelector
  , updateTwoDCartesianZoneWithParams_completionSelector
  , writeAttributeSensitivityWithValue_completionSelector
  , writeAttributeSensitivityWithValue_params_completionSelector


  ) where

import Foreign.Ptr (Ptr, FunPtr)
import Foreign.C.Types

import ObjC.Runtime.Types
import ObjC.Runtime.Message (sendMessage, sendOwnedMessage, sendClassMessage, sendOwnedClassMessage)
import ObjC.Runtime.Selector (mkSelector)
import ObjC.Runtime.Class (getRequiredClass)

import ObjC.Matter.Internal.Classes
import ObjC.Foundation.Internal.Classes

-- | Command CreateTwoDCartesianZone
--
-- This command SHALL create and store a TwoD Cartesian Zone.
--
-- ObjC selector: @- createTwoDCartesianZoneWithParams:completion:@
createTwoDCartesianZoneWithParams_completion :: (IsMTRBaseClusterZoneManagement mtrBaseClusterZoneManagement, IsMTRZoneManagementClusterCreateTwoDCartesianZoneParams params) => mtrBaseClusterZoneManagement -> params -> Ptr () -> IO ()
createTwoDCartesianZoneWithParams_completion mtrBaseClusterZoneManagement params completion =
  sendMessage mtrBaseClusterZoneManagement createTwoDCartesianZoneWithParams_completionSelector (toMTRZoneManagementClusterCreateTwoDCartesianZoneParams params) completion

-- | Command UpdateTwoDCartesianZone
--
-- The UpdateTwoDCartesianZone SHALL update a stored TwoD Cartesian Zone.
--
-- ObjC selector: @- updateTwoDCartesianZoneWithParams:completion:@
updateTwoDCartesianZoneWithParams_completion :: (IsMTRBaseClusterZoneManagement mtrBaseClusterZoneManagement, IsMTRZoneManagementClusterUpdateTwoDCartesianZoneParams params) => mtrBaseClusterZoneManagement -> params -> Ptr () -> IO ()
updateTwoDCartesianZoneWithParams_completion mtrBaseClusterZoneManagement params completion =
  sendMessage mtrBaseClusterZoneManagement updateTwoDCartesianZoneWithParams_completionSelector (toMTRZoneManagementClusterUpdateTwoDCartesianZoneParams params) completion

-- | Command RemoveZone
--
-- This command SHALL remove the user-defined Zone indicated by ZoneID.
--
-- ObjC selector: @- removeZoneWithParams:completion:@
removeZoneWithParams_completion :: (IsMTRBaseClusterZoneManagement mtrBaseClusterZoneManagement, IsMTRZoneManagementClusterRemoveZoneParams params) => mtrBaseClusterZoneManagement -> params -> Ptr () -> IO ()
removeZoneWithParams_completion mtrBaseClusterZoneManagement params completion =
  sendMessage mtrBaseClusterZoneManagement removeZoneWithParams_completionSelector (toMTRZoneManagementClusterRemoveZoneParams params) completion

-- | Command CreateOrUpdateTrigger
--
-- This command is used to create or update a Trigger for the specified motion Zone.
--
-- ObjC selector: @- createOrUpdateTriggerWithParams:completion:@
createOrUpdateTriggerWithParams_completion :: (IsMTRBaseClusterZoneManagement mtrBaseClusterZoneManagement, IsMTRZoneManagementClusterCreateOrUpdateTriggerParams params) => mtrBaseClusterZoneManagement -> params -> Ptr () -> IO ()
createOrUpdateTriggerWithParams_completion mtrBaseClusterZoneManagement params completion =
  sendMessage mtrBaseClusterZoneManagement createOrUpdateTriggerWithParams_completionSelector (toMTRZoneManagementClusterCreateOrUpdateTriggerParams params) completion

-- | Command RemoveTrigger
--
-- This command SHALL remove the Trigger for the provided ZoneID.
--
-- ObjC selector: @- removeTriggerWithParams:completion:@
removeTriggerWithParams_completion :: (IsMTRBaseClusterZoneManagement mtrBaseClusterZoneManagement, IsMTRZoneManagementClusterRemoveTriggerParams params) => mtrBaseClusterZoneManagement -> params -> Ptr () -> IO ()
removeTriggerWithParams_completion mtrBaseClusterZoneManagement params completion =
  sendMessage mtrBaseClusterZoneManagement removeTriggerWithParams_completionSelector (toMTRZoneManagementClusterRemoveTriggerParams params) completion

-- | @- readAttributeMaxUserDefinedZonesWithCompletion:@
readAttributeMaxUserDefinedZonesWithCompletion :: IsMTRBaseClusterZoneManagement mtrBaseClusterZoneManagement => mtrBaseClusterZoneManagement -> Ptr () -> IO ()
readAttributeMaxUserDefinedZonesWithCompletion mtrBaseClusterZoneManagement completion =
  sendMessage mtrBaseClusterZoneManagement readAttributeMaxUserDefinedZonesWithCompletionSelector completion

-- | @- subscribeAttributeMaxUserDefinedZonesWithParams:subscriptionEstablished:reportHandler:@
subscribeAttributeMaxUserDefinedZonesWithParams_subscriptionEstablished_reportHandler :: (IsMTRBaseClusterZoneManagement mtrBaseClusterZoneManagement, IsMTRSubscribeParams params) => mtrBaseClusterZoneManagement -> params -> Ptr () -> Ptr () -> IO ()
subscribeAttributeMaxUserDefinedZonesWithParams_subscriptionEstablished_reportHandler mtrBaseClusterZoneManagement params subscriptionEstablished reportHandler =
  sendMessage mtrBaseClusterZoneManagement subscribeAttributeMaxUserDefinedZonesWithParams_subscriptionEstablished_reportHandlerSelector (toMTRSubscribeParams params) subscriptionEstablished reportHandler

-- | @+ readAttributeMaxUserDefinedZonesWithClusterStateCache:endpoint:queue:completion:@
readAttributeMaxUserDefinedZonesWithClusterStateCache_endpoint_queue_completion :: (IsMTRClusterStateCacheContainer clusterStateCacheContainer, IsNSNumber endpoint, IsNSObject queue) => clusterStateCacheContainer -> endpoint -> queue -> Ptr () -> IO ()
readAttributeMaxUserDefinedZonesWithClusterStateCache_endpoint_queue_completion clusterStateCacheContainer endpoint queue completion =
  do
    cls' <- getRequiredClass "MTRBaseClusterZoneManagement"
    sendClassMessage cls' readAttributeMaxUserDefinedZonesWithClusterStateCache_endpoint_queue_completionSelector (toMTRClusterStateCacheContainer clusterStateCacheContainer) (toNSNumber endpoint) (toNSObject queue) completion

-- | @- readAttributeMaxZonesWithCompletion:@
readAttributeMaxZonesWithCompletion :: IsMTRBaseClusterZoneManagement mtrBaseClusterZoneManagement => mtrBaseClusterZoneManagement -> Ptr () -> IO ()
readAttributeMaxZonesWithCompletion mtrBaseClusterZoneManagement completion =
  sendMessage mtrBaseClusterZoneManagement readAttributeMaxZonesWithCompletionSelector completion

-- | @- subscribeAttributeMaxZonesWithParams:subscriptionEstablished:reportHandler:@
subscribeAttributeMaxZonesWithParams_subscriptionEstablished_reportHandler :: (IsMTRBaseClusterZoneManagement mtrBaseClusterZoneManagement, IsMTRSubscribeParams params) => mtrBaseClusterZoneManagement -> params -> Ptr () -> Ptr () -> IO ()
subscribeAttributeMaxZonesWithParams_subscriptionEstablished_reportHandler mtrBaseClusterZoneManagement params subscriptionEstablished reportHandler =
  sendMessage mtrBaseClusterZoneManagement subscribeAttributeMaxZonesWithParams_subscriptionEstablished_reportHandlerSelector (toMTRSubscribeParams params) subscriptionEstablished reportHandler

-- | @+ readAttributeMaxZonesWithClusterStateCache:endpoint:queue:completion:@
readAttributeMaxZonesWithClusterStateCache_endpoint_queue_completion :: (IsMTRClusterStateCacheContainer clusterStateCacheContainer, IsNSNumber endpoint, IsNSObject queue) => clusterStateCacheContainer -> endpoint -> queue -> Ptr () -> IO ()
readAttributeMaxZonesWithClusterStateCache_endpoint_queue_completion clusterStateCacheContainer endpoint queue completion =
  do
    cls' <- getRequiredClass "MTRBaseClusterZoneManagement"
    sendClassMessage cls' readAttributeMaxZonesWithClusterStateCache_endpoint_queue_completionSelector (toMTRClusterStateCacheContainer clusterStateCacheContainer) (toNSNumber endpoint) (toNSObject queue) completion

-- | @- readAttributeZonesWithCompletion:@
readAttributeZonesWithCompletion :: IsMTRBaseClusterZoneManagement mtrBaseClusterZoneManagement => mtrBaseClusterZoneManagement -> Ptr () -> IO ()
readAttributeZonesWithCompletion mtrBaseClusterZoneManagement completion =
  sendMessage mtrBaseClusterZoneManagement readAttributeZonesWithCompletionSelector completion

-- | @- subscribeAttributeZonesWithParams:subscriptionEstablished:reportHandler:@
subscribeAttributeZonesWithParams_subscriptionEstablished_reportHandler :: (IsMTRBaseClusterZoneManagement mtrBaseClusterZoneManagement, IsMTRSubscribeParams params) => mtrBaseClusterZoneManagement -> params -> Ptr () -> Ptr () -> IO ()
subscribeAttributeZonesWithParams_subscriptionEstablished_reportHandler mtrBaseClusterZoneManagement params subscriptionEstablished reportHandler =
  sendMessage mtrBaseClusterZoneManagement subscribeAttributeZonesWithParams_subscriptionEstablished_reportHandlerSelector (toMTRSubscribeParams params) subscriptionEstablished reportHandler

-- | @+ readAttributeZonesWithClusterStateCache:endpoint:queue:completion:@
readAttributeZonesWithClusterStateCache_endpoint_queue_completion :: (IsMTRClusterStateCacheContainer clusterStateCacheContainer, IsNSNumber endpoint, IsNSObject queue) => clusterStateCacheContainer -> endpoint -> queue -> Ptr () -> IO ()
readAttributeZonesWithClusterStateCache_endpoint_queue_completion clusterStateCacheContainer endpoint queue completion =
  do
    cls' <- getRequiredClass "MTRBaseClusterZoneManagement"
    sendClassMessage cls' readAttributeZonesWithClusterStateCache_endpoint_queue_completionSelector (toMTRClusterStateCacheContainer clusterStateCacheContainer) (toNSNumber endpoint) (toNSObject queue) completion

-- | @- readAttributeTriggersWithCompletion:@
readAttributeTriggersWithCompletion :: IsMTRBaseClusterZoneManagement mtrBaseClusterZoneManagement => mtrBaseClusterZoneManagement -> Ptr () -> IO ()
readAttributeTriggersWithCompletion mtrBaseClusterZoneManagement completion =
  sendMessage mtrBaseClusterZoneManagement readAttributeTriggersWithCompletionSelector completion

-- | @- subscribeAttributeTriggersWithParams:subscriptionEstablished:reportHandler:@
subscribeAttributeTriggersWithParams_subscriptionEstablished_reportHandler :: (IsMTRBaseClusterZoneManagement mtrBaseClusterZoneManagement, IsMTRSubscribeParams params) => mtrBaseClusterZoneManagement -> params -> Ptr () -> Ptr () -> IO ()
subscribeAttributeTriggersWithParams_subscriptionEstablished_reportHandler mtrBaseClusterZoneManagement params subscriptionEstablished reportHandler =
  sendMessage mtrBaseClusterZoneManagement subscribeAttributeTriggersWithParams_subscriptionEstablished_reportHandlerSelector (toMTRSubscribeParams params) subscriptionEstablished reportHandler

-- | @+ readAttributeTriggersWithClusterStateCache:endpoint:queue:completion:@
readAttributeTriggersWithClusterStateCache_endpoint_queue_completion :: (IsMTRClusterStateCacheContainer clusterStateCacheContainer, IsNSNumber endpoint, IsNSObject queue) => clusterStateCacheContainer -> endpoint -> queue -> Ptr () -> IO ()
readAttributeTriggersWithClusterStateCache_endpoint_queue_completion clusterStateCacheContainer endpoint queue completion =
  do
    cls' <- getRequiredClass "MTRBaseClusterZoneManagement"
    sendClassMessage cls' readAttributeTriggersWithClusterStateCache_endpoint_queue_completionSelector (toMTRClusterStateCacheContainer clusterStateCacheContainer) (toNSNumber endpoint) (toNSObject queue) completion

-- | @- readAttributeSensitivityMaxWithCompletion:@
readAttributeSensitivityMaxWithCompletion :: IsMTRBaseClusterZoneManagement mtrBaseClusterZoneManagement => mtrBaseClusterZoneManagement -> Ptr () -> IO ()
readAttributeSensitivityMaxWithCompletion mtrBaseClusterZoneManagement completion =
  sendMessage mtrBaseClusterZoneManagement readAttributeSensitivityMaxWithCompletionSelector completion

-- | @- subscribeAttributeSensitivityMaxWithParams:subscriptionEstablished:reportHandler:@
subscribeAttributeSensitivityMaxWithParams_subscriptionEstablished_reportHandler :: (IsMTRBaseClusterZoneManagement mtrBaseClusterZoneManagement, IsMTRSubscribeParams params) => mtrBaseClusterZoneManagement -> params -> Ptr () -> Ptr () -> IO ()
subscribeAttributeSensitivityMaxWithParams_subscriptionEstablished_reportHandler mtrBaseClusterZoneManagement params subscriptionEstablished reportHandler =
  sendMessage mtrBaseClusterZoneManagement subscribeAttributeSensitivityMaxWithParams_subscriptionEstablished_reportHandlerSelector (toMTRSubscribeParams params) subscriptionEstablished reportHandler

-- | @+ readAttributeSensitivityMaxWithClusterStateCache:endpoint:queue:completion:@
readAttributeSensitivityMaxWithClusterStateCache_endpoint_queue_completion :: (IsMTRClusterStateCacheContainer clusterStateCacheContainer, IsNSNumber endpoint, IsNSObject queue) => clusterStateCacheContainer -> endpoint -> queue -> Ptr () -> IO ()
readAttributeSensitivityMaxWithClusterStateCache_endpoint_queue_completion clusterStateCacheContainer endpoint queue completion =
  do
    cls' <- getRequiredClass "MTRBaseClusterZoneManagement"
    sendClassMessage cls' readAttributeSensitivityMaxWithClusterStateCache_endpoint_queue_completionSelector (toMTRClusterStateCacheContainer clusterStateCacheContainer) (toNSNumber endpoint) (toNSObject queue) completion

-- | @- readAttributeSensitivityWithCompletion:@
readAttributeSensitivityWithCompletion :: IsMTRBaseClusterZoneManagement mtrBaseClusterZoneManagement => mtrBaseClusterZoneManagement -> Ptr () -> IO ()
readAttributeSensitivityWithCompletion mtrBaseClusterZoneManagement completion =
  sendMessage mtrBaseClusterZoneManagement readAttributeSensitivityWithCompletionSelector completion

-- | @- writeAttributeSensitivityWithValue:completion:@
writeAttributeSensitivityWithValue_completion :: (IsMTRBaseClusterZoneManagement mtrBaseClusterZoneManagement, IsNSNumber value) => mtrBaseClusterZoneManagement -> value -> Ptr () -> IO ()
writeAttributeSensitivityWithValue_completion mtrBaseClusterZoneManagement value completion =
  sendMessage mtrBaseClusterZoneManagement writeAttributeSensitivityWithValue_completionSelector (toNSNumber value) completion

-- | @- writeAttributeSensitivityWithValue:params:completion:@
writeAttributeSensitivityWithValue_params_completion :: (IsMTRBaseClusterZoneManagement mtrBaseClusterZoneManagement, IsNSNumber value, IsMTRWriteParams params) => mtrBaseClusterZoneManagement -> value -> params -> Ptr () -> IO ()
writeAttributeSensitivityWithValue_params_completion mtrBaseClusterZoneManagement value params completion =
  sendMessage mtrBaseClusterZoneManagement writeAttributeSensitivityWithValue_params_completionSelector (toNSNumber value) (toMTRWriteParams params) completion

-- | @- subscribeAttributeSensitivityWithParams:subscriptionEstablished:reportHandler:@
subscribeAttributeSensitivityWithParams_subscriptionEstablished_reportHandler :: (IsMTRBaseClusterZoneManagement mtrBaseClusterZoneManagement, IsMTRSubscribeParams params) => mtrBaseClusterZoneManagement -> params -> Ptr () -> Ptr () -> IO ()
subscribeAttributeSensitivityWithParams_subscriptionEstablished_reportHandler mtrBaseClusterZoneManagement params subscriptionEstablished reportHandler =
  sendMessage mtrBaseClusterZoneManagement subscribeAttributeSensitivityWithParams_subscriptionEstablished_reportHandlerSelector (toMTRSubscribeParams params) subscriptionEstablished reportHandler

-- | @+ readAttributeSensitivityWithClusterStateCache:endpoint:queue:completion:@
readAttributeSensitivityWithClusterStateCache_endpoint_queue_completion :: (IsMTRClusterStateCacheContainer clusterStateCacheContainer, IsNSNumber endpoint, IsNSObject queue) => clusterStateCacheContainer -> endpoint -> queue -> Ptr () -> IO ()
readAttributeSensitivityWithClusterStateCache_endpoint_queue_completion clusterStateCacheContainer endpoint queue completion =
  do
    cls' <- getRequiredClass "MTRBaseClusterZoneManagement"
    sendClassMessage cls' readAttributeSensitivityWithClusterStateCache_endpoint_queue_completionSelector (toMTRClusterStateCacheContainer clusterStateCacheContainer) (toNSNumber endpoint) (toNSObject queue) completion

-- | @- readAttributeTwoDCartesianMaxWithCompletion:@
readAttributeTwoDCartesianMaxWithCompletion :: IsMTRBaseClusterZoneManagement mtrBaseClusterZoneManagement => mtrBaseClusterZoneManagement -> Ptr () -> IO ()
readAttributeTwoDCartesianMaxWithCompletion mtrBaseClusterZoneManagement completion =
  sendMessage mtrBaseClusterZoneManagement readAttributeTwoDCartesianMaxWithCompletionSelector completion

-- | @- subscribeAttributeTwoDCartesianMaxWithParams:subscriptionEstablished:reportHandler:@
subscribeAttributeTwoDCartesianMaxWithParams_subscriptionEstablished_reportHandler :: (IsMTRBaseClusterZoneManagement mtrBaseClusterZoneManagement, IsMTRSubscribeParams params) => mtrBaseClusterZoneManagement -> params -> Ptr () -> Ptr () -> IO ()
subscribeAttributeTwoDCartesianMaxWithParams_subscriptionEstablished_reportHandler mtrBaseClusterZoneManagement params subscriptionEstablished reportHandler =
  sendMessage mtrBaseClusterZoneManagement subscribeAttributeTwoDCartesianMaxWithParams_subscriptionEstablished_reportHandlerSelector (toMTRSubscribeParams params) subscriptionEstablished reportHandler

-- | @+ readAttributeTwoDCartesianMaxWithClusterStateCache:endpoint:queue:completion:@
readAttributeTwoDCartesianMaxWithClusterStateCache_endpoint_queue_completion :: (IsMTRClusterStateCacheContainer clusterStateCacheContainer, IsNSNumber endpoint, IsNSObject queue) => clusterStateCacheContainer -> endpoint -> queue -> Ptr () -> IO ()
readAttributeTwoDCartesianMaxWithClusterStateCache_endpoint_queue_completion clusterStateCacheContainer endpoint queue completion =
  do
    cls' <- getRequiredClass "MTRBaseClusterZoneManagement"
    sendClassMessage cls' readAttributeTwoDCartesianMaxWithClusterStateCache_endpoint_queue_completionSelector (toMTRClusterStateCacheContainer clusterStateCacheContainer) (toNSNumber endpoint) (toNSObject queue) completion

-- | @- readAttributeGeneratedCommandListWithCompletion:@
readAttributeGeneratedCommandListWithCompletion :: IsMTRBaseClusterZoneManagement mtrBaseClusterZoneManagement => mtrBaseClusterZoneManagement -> Ptr () -> IO ()
readAttributeGeneratedCommandListWithCompletion mtrBaseClusterZoneManagement completion =
  sendMessage mtrBaseClusterZoneManagement readAttributeGeneratedCommandListWithCompletionSelector completion

-- | @- subscribeAttributeGeneratedCommandListWithParams:subscriptionEstablished:reportHandler:@
subscribeAttributeGeneratedCommandListWithParams_subscriptionEstablished_reportHandler :: (IsMTRBaseClusterZoneManagement mtrBaseClusterZoneManagement, IsMTRSubscribeParams params) => mtrBaseClusterZoneManagement -> params -> Ptr () -> Ptr () -> IO ()
subscribeAttributeGeneratedCommandListWithParams_subscriptionEstablished_reportHandler mtrBaseClusterZoneManagement params subscriptionEstablished reportHandler =
  sendMessage mtrBaseClusterZoneManagement subscribeAttributeGeneratedCommandListWithParams_subscriptionEstablished_reportHandlerSelector (toMTRSubscribeParams params) subscriptionEstablished reportHandler

-- | @+ readAttributeGeneratedCommandListWithClusterStateCache:endpoint:queue:completion:@
readAttributeGeneratedCommandListWithClusterStateCache_endpoint_queue_completion :: (IsMTRClusterStateCacheContainer clusterStateCacheContainer, IsNSNumber endpoint, IsNSObject queue) => clusterStateCacheContainer -> endpoint -> queue -> Ptr () -> IO ()
readAttributeGeneratedCommandListWithClusterStateCache_endpoint_queue_completion clusterStateCacheContainer endpoint queue completion =
  do
    cls' <- getRequiredClass "MTRBaseClusterZoneManagement"
    sendClassMessage cls' readAttributeGeneratedCommandListWithClusterStateCache_endpoint_queue_completionSelector (toMTRClusterStateCacheContainer clusterStateCacheContainer) (toNSNumber endpoint) (toNSObject queue) completion

-- | @- readAttributeAcceptedCommandListWithCompletion:@
readAttributeAcceptedCommandListWithCompletion :: IsMTRBaseClusterZoneManagement mtrBaseClusterZoneManagement => mtrBaseClusterZoneManagement -> Ptr () -> IO ()
readAttributeAcceptedCommandListWithCompletion mtrBaseClusterZoneManagement completion =
  sendMessage mtrBaseClusterZoneManagement readAttributeAcceptedCommandListWithCompletionSelector completion

-- | @- subscribeAttributeAcceptedCommandListWithParams:subscriptionEstablished:reportHandler:@
subscribeAttributeAcceptedCommandListWithParams_subscriptionEstablished_reportHandler :: (IsMTRBaseClusterZoneManagement mtrBaseClusterZoneManagement, IsMTRSubscribeParams params) => mtrBaseClusterZoneManagement -> params -> Ptr () -> Ptr () -> IO ()
subscribeAttributeAcceptedCommandListWithParams_subscriptionEstablished_reportHandler mtrBaseClusterZoneManagement params subscriptionEstablished reportHandler =
  sendMessage mtrBaseClusterZoneManagement subscribeAttributeAcceptedCommandListWithParams_subscriptionEstablished_reportHandlerSelector (toMTRSubscribeParams params) subscriptionEstablished reportHandler

-- | @+ readAttributeAcceptedCommandListWithClusterStateCache:endpoint:queue:completion:@
readAttributeAcceptedCommandListWithClusterStateCache_endpoint_queue_completion :: (IsMTRClusterStateCacheContainer clusterStateCacheContainer, IsNSNumber endpoint, IsNSObject queue) => clusterStateCacheContainer -> endpoint -> queue -> Ptr () -> IO ()
readAttributeAcceptedCommandListWithClusterStateCache_endpoint_queue_completion clusterStateCacheContainer endpoint queue completion =
  do
    cls' <- getRequiredClass "MTRBaseClusterZoneManagement"
    sendClassMessage cls' readAttributeAcceptedCommandListWithClusterStateCache_endpoint_queue_completionSelector (toMTRClusterStateCacheContainer clusterStateCacheContainer) (toNSNumber endpoint) (toNSObject queue) completion

-- | @- readAttributeAttributeListWithCompletion:@
readAttributeAttributeListWithCompletion :: IsMTRBaseClusterZoneManagement mtrBaseClusterZoneManagement => mtrBaseClusterZoneManagement -> Ptr () -> IO ()
readAttributeAttributeListWithCompletion mtrBaseClusterZoneManagement completion =
  sendMessage mtrBaseClusterZoneManagement readAttributeAttributeListWithCompletionSelector completion

-- | @- subscribeAttributeAttributeListWithParams:subscriptionEstablished:reportHandler:@
subscribeAttributeAttributeListWithParams_subscriptionEstablished_reportHandler :: (IsMTRBaseClusterZoneManagement mtrBaseClusterZoneManagement, IsMTRSubscribeParams params) => mtrBaseClusterZoneManagement -> params -> Ptr () -> Ptr () -> IO ()
subscribeAttributeAttributeListWithParams_subscriptionEstablished_reportHandler mtrBaseClusterZoneManagement params subscriptionEstablished reportHandler =
  sendMessage mtrBaseClusterZoneManagement subscribeAttributeAttributeListWithParams_subscriptionEstablished_reportHandlerSelector (toMTRSubscribeParams params) subscriptionEstablished reportHandler

-- | @+ readAttributeAttributeListWithClusterStateCache:endpoint:queue:completion:@
readAttributeAttributeListWithClusterStateCache_endpoint_queue_completion :: (IsMTRClusterStateCacheContainer clusterStateCacheContainer, IsNSNumber endpoint, IsNSObject queue) => clusterStateCacheContainer -> endpoint -> queue -> Ptr () -> IO ()
readAttributeAttributeListWithClusterStateCache_endpoint_queue_completion clusterStateCacheContainer endpoint queue completion =
  do
    cls' <- getRequiredClass "MTRBaseClusterZoneManagement"
    sendClassMessage cls' readAttributeAttributeListWithClusterStateCache_endpoint_queue_completionSelector (toMTRClusterStateCacheContainer clusterStateCacheContainer) (toNSNumber endpoint) (toNSObject queue) completion

-- | @- readAttributeFeatureMapWithCompletion:@
readAttributeFeatureMapWithCompletion :: IsMTRBaseClusterZoneManagement mtrBaseClusterZoneManagement => mtrBaseClusterZoneManagement -> Ptr () -> IO ()
readAttributeFeatureMapWithCompletion mtrBaseClusterZoneManagement completion =
  sendMessage mtrBaseClusterZoneManagement readAttributeFeatureMapWithCompletionSelector completion

-- | @- subscribeAttributeFeatureMapWithParams:subscriptionEstablished:reportHandler:@
subscribeAttributeFeatureMapWithParams_subscriptionEstablished_reportHandler :: (IsMTRBaseClusterZoneManagement mtrBaseClusterZoneManagement, IsMTRSubscribeParams params) => mtrBaseClusterZoneManagement -> params -> Ptr () -> Ptr () -> IO ()
subscribeAttributeFeatureMapWithParams_subscriptionEstablished_reportHandler mtrBaseClusterZoneManagement params subscriptionEstablished reportHandler =
  sendMessage mtrBaseClusterZoneManagement subscribeAttributeFeatureMapWithParams_subscriptionEstablished_reportHandlerSelector (toMTRSubscribeParams params) subscriptionEstablished reportHandler

-- | @+ readAttributeFeatureMapWithClusterStateCache:endpoint:queue:completion:@
readAttributeFeatureMapWithClusterStateCache_endpoint_queue_completion :: (IsMTRClusterStateCacheContainer clusterStateCacheContainer, IsNSNumber endpoint, IsNSObject queue) => clusterStateCacheContainer -> endpoint -> queue -> Ptr () -> IO ()
readAttributeFeatureMapWithClusterStateCache_endpoint_queue_completion clusterStateCacheContainer endpoint queue completion =
  do
    cls' <- getRequiredClass "MTRBaseClusterZoneManagement"
    sendClassMessage cls' readAttributeFeatureMapWithClusterStateCache_endpoint_queue_completionSelector (toMTRClusterStateCacheContainer clusterStateCacheContainer) (toNSNumber endpoint) (toNSObject queue) completion

-- | @- readAttributeClusterRevisionWithCompletion:@
readAttributeClusterRevisionWithCompletion :: IsMTRBaseClusterZoneManagement mtrBaseClusterZoneManagement => mtrBaseClusterZoneManagement -> Ptr () -> IO ()
readAttributeClusterRevisionWithCompletion mtrBaseClusterZoneManagement completion =
  sendMessage mtrBaseClusterZoneManagement readAttributeClusterRevisionWithCompletionSelector completion

-- | @- subscribeAttributeClusterRevisionWithParams:subscriptionEstablished:reportHandler:@
subscribeAttributeClusterRevisionWithParams_subscriptionEstablished_reportHandler :: (IsMTRBaseClusterZoneManagement mtrBaseClusterZoneManagement, IsMTRSubscribeParams params) => mtrBaseClusterZoneManagement -> params -> Ptr () -> Ptr () -> IO ()
subscribeAttributeClusterRevisionWithParams_subscriptionEstablished_reportHandler mtrBaseClusterZoneManagement params subscriptionEstablished reportHandler =
  sendMessage mtrBaseClusterZoneManagement subscribeAttributeClusterRevisionWithParams_subscriptionEstablished_reportHandlerSelector (toMTRSubscribeParams params) subscriptionEstablished reportHandler

-- | @+ readAttributeClusterRevisionWithClusterStateCache:endpoint:queue:completion:@
readAttributeClusterRevisionWithClusterStateCache_endpoint_queue_completion :: (IsMTRClusterStateCacheContainer clusterStateCacheContainer, IsNSNumber endpoint, IsNSObject queue) => clusterStateCacheContainer -> endpoint -> queue -> Ptr () -> IO ()
readAttributeClusterRevisionWithClusterStateCache_endpoint_queue_completion clusterStateCacheContainer endpoint queue completion =
  do
    cls' <- getRequiredClass "MTRBaseClusterZoneManagement"
    sendClassMessage cls' readAttributeClusterRevisionWithClusterStateCache_endpoint_queue_completionSelector (toMTRClusterStateCacheContainer clusterStateCacheContainer) (toNSNumber endpoint) (toNSObject queue) completion

-- | @- init@
init_ :: IsMTRBaseClusterZoneManagement mtrBaseClusterZoneManagement => mtrBaseClusterZoneManagement -> IO (Id MTRBaseClusterZoneManagement)
init_ mtrBaseClusterZoneManagement =
  sendOwnedMessage mtrBaseClusterZoneManagement initSelector

-- | @+ new@
new :: IO (Id MTRBaseClusterZoneManagement)
new  =
  do
    cls' <- getRequiredClass "MTRBaseClusterZoneManagement"
    sendOwnedClassMessage cls' newSelector

-- | For all instance methods (reads, writes, commands) that take a completion, the completion will be called on the provided queue.
--
-- ObjC selector: @- initWithDevice:endpointID:queue:@
initWithDevice_endpointID_queue :: (IsMTRBaseClusterZoneManagement mtrBaseClusterZoneManagement, IsMTRBaseDevice device, IsNSNumber endpointID, IsNSObject queue) => mtrBaseClusterZoneManagement -> device -> endpointID -> queue -> IO (Id MTRBaseClusterZoneManagement)
initWithDevice_endpointID_queue mtrBaseClusterZoneManagement device endpointID queue =
  sendOwnedMessage mtrBaseClusterZoneManagement initWithDevice_endpointID_queueSelector (toMTRBaseDevice device) (toNSNumber endpointID) (toNSObject queue)

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @createTwoDCartesianZoneWithParams:completion:@
createTwoDCartesianZoneWithParams_completionSelector :: Selector '[Id MTRZoneManagementClusterCreateTwoDCartesianZoneParams, Ptr ()] ()
createTwoDCartesianZoneWithParams_completionSelector = mkSelector "createTwoDCartesianZoneWithParams:completion:"

-- | @Selector@ for @updateTwoDCartesianZoneWithParams:completion:@
updateTwoDCartesianZoneWithParams_completionSelector :: Selector '[Id MTRZoneManagementClusterUpdateTwoDCartesianZoneParams, Ptr ()] ()
updateTwoDCartesianZoneWithParams_completionSelector = mkSelector "updateTwoDCartesianZoneWithParams:completion:"

-- | @Selector@ for @removeZoneWithParams:completion:@
removeZoneWithParams_completionSelector :: Selector '[Id MTRZoneManagementClusterRemoveZoneParams, Ptr ()] ()
removeZoneWithParams_completionSelector = mkSelector "removeZoneWithParams:completion:"

-- | @Selector@ for @createOrUpdateTriggerWithParams:completion:@
createOrUpdateTriggerWithParams_completionSelector :: Selector '[Id MTRZoneManagementClusterCreateOrUpdateTriggerParams, Ptr ()] ()
createOrUpdateTriggerWithParams_completionSelector = mkSelector "createOrUpdateTriggerWithParams:completion:"

-- | @Selector@ for @removeTriggerWithParams:completion:@
removeTriggerWithParams_completionSelector :: Selector '[Id MTRZoneManagementClusterRemoveTriggerParams, Ptr ()] ()
removeTriggerWithParams_completionSelector = mkSelector "removeTriggerWithParams:completion:"

-- | @Selector@ for @readAttributeMaxUserDefinedZonesWithCompletion:@
readAttributeMaxUserDefinedZonesWithCompletionSelector :: Selector '[Ptr ()] ()
readAttributeMaxUserDefinedZonesWithCompletionSelector = mkSelector "readAttributeMaxUserDefinedZonesWithCompletion:"

-- | @Selector@ for @subscribeAttributeMaxUserDefinedZonesWithParams:subscriptionEstablished:reportHandler:@
subscribeAttributeMaxUserDefinedZonesWithParams_subscriptionEstablished_reportHandlerSelector :: Selector '[Id MTRSubscribeParams, Ptr (), Ptr ()] ()
subscribeAttributeMaxUserDefinedZonesWithParams_subscriptionEstablished_reportHandlerSelector = mkSelector "subscribeAttributeMaxUserDefinedZonesWithParams:subscriptionEstablished:reportHandler:"

-- | @Selector@ for @readAttributeMaxUserDefinedZonesWithClusterStateCache:endpoint:queue:completion:@
readAttributeMaxUserDefinedZonesWithClusterStateCache_endpoint_queue_completionSelector :: Selector '[Id MTRClusterStateCacheContainer, Id NSNumber, Id NSObject, Ptr ()] ()
readAttributeMaxUserDefinedZonesWithClusterStateCache_endpoint_queue_completionSelector = mkSelector "readAttributeMaxUserDefinedZonesWithClusterStateCache:endpoint:queue:completion:"

-- | @Selector@ for @readAttributeMaxZonesWithCompletion:@
readAttributeMaxZonesWithCompletionSelector :: Selector '[Ptr ()] ()
readAttributeMaxZonesWithCompletionSelector = mkSelector "readAttributeMaxZonesWithCompletion:"

-- | @Selector@ for @subscribeAttributeMaxZonesWithParams:subscriptionEstablished:reportHandler:@
subscribeAttributeMaxZonesWithParams_subscriptionEstablished_reportHandlerSelector :: Selector '[Id MTRSubscribeParams, Ptr (), Ptr ()] ()
subscribeAttributeMaxZonesWithParams_subscriptionEstablished_reportHandlerSelector = mkSelector "subscribeAttributeMaxZonesWithParams:subscriptionEstablished:reportHandler:"

-- | @Selector@ for @readAttributeMaxZonesWithClusterStateCache:endpoint:queue:completion:@
readAttributeMaxZonesWithClusterStateCache_endpoint_queue_completionSelector :: Selector '[Id MTRClusterStateCacheContainer, Id NSNumber, Id NSObject, Ptr ()] ()
readAttributeMaxZonesWithClusterStateCache_endpoint_queue_completionSelector = mkSelector "readAttributeMaxZonesWithClusterStateCache:endpoint:queue:completion:"

-- | @Selector@ for @readAttributeZonesWithCompletion:@
readAttributeZonesWithCompletionSelector :: Selector '[Ptr ()] ()
readAttributeZonesWithCompletionSelector = mkSelector "readAttributeZonesWithCompletion:"

-- | @Selector@ for @subscribeAttributeZonesWithParams:subscriptionEstablished:reportHandler:@
subscribeAttributeZonesWithParams_subscriptionEstablished_reportHandlerSelector :: Selector '[Id MTRSubscribeParams, Ptr (), Ptr ()] ()
subscribeAttributeZonesWithParams_subscriptionEstablished_reportHandlerSelector = mkSelector "subscribeAttributeZonesWithParams:subscriptionEstablished:reportHandler:"

-- | @Selector@ for @readAttributeZonesWithClusterStateCache:endpoint:queue:completion:@
readAttributeZonesWithClusterStateCache_endpoint_queue_completionSelector :: Selector '[Id MTRClusterStateCacheContainer, Id NSNumber, Id NSObject, Ptr ()] ()
readAttributeZonesWithClusterStateCache_endpoint_queue_completionSelector = mkSelector "readAttributeZonesWithClusterStateCache:endpoint:queue:completion:"

-- | @Selector@ for @readAttributeTriggersWithCompletion:@
readAttributeTriggersWithCompletionSelector :: Selector '[Ptr ()] ()
readAttributeTriggersWithCompletionSelector = mkSelector "readAttributeTriggersWithCompletion:"

-- | @Selector@ for @subscribeAttributeTriggersWithParams:subscriptionEstablished:reportHandler:@
subscribeAttributeTriggersWithParams_subscriptionEstablished_reportHandlerSelector :: Selector '[Id MTRSubscribeParams, Ptr (), Ptr ()] ()
subscribeAttributeTriggersWithParams_subscriptionEstablished_reportHandlerSelector = mkSelector "subscribeAttributeTriggersWithParams:subscriptionEstablished:reportHandler:"

-- | @Selector@ for @readAttributeTriggersWithClusterStateCache:endpoint:queue:completion:@
readAttributeTriggersWithClusterStateCache_endpoint_queue_completionSelector :: Selector '[Id MTRClusterStateCacheContainer, Id NSNumber, Id NSObject, Ptr ()] ()
readAttributeTriggersWithClusterStateCache_endpoint_queue_completionSelector = mkSelector "readAttributeTriggersWithClusterStateCache:endpoint:queue:completion:"

-- | @Selector@ for @readAttributeSensitivityMaxWithCompletion:@
readAttributeSensitivityMaxWithCompletionSelector :: Selector '[Ptr ()] ()
readAttributeSensitivityMaxWithCompletionSelector = mkSelector "readAttributeSensitivityMaxWithCompletion:"

-- | @Selector@ for @subscribeAttributeSensitivityMaxWithParams:subscriptionEstablished:reportHandler:@
subscribeAttributeSensitivityMaxWithParams_subscriptionEstablished_reportHandlerSelector :: Selector '[Id MTRSubscribeParams, Ptr (), Ptr ()] ()
subscribeAttributeSensitivityMaxWithParams_subscriptionEstablished_reportHandlerSelector = mkSelector "subscribeAttributeSensitivityMaxWithParams:subscriptionEstablished:reportHandler:"

-- | @Selector@ for @readAttributeSensitivityMaxWithClusterStateCache:endpoint:queue:completion:@
readAttributeSensitivityMaxWithClusterStateCache_endpoint_queue_completionSelector :: Selector '[Id MTRClusterStateCacheContainer, Id NSNumber, Id NSObject, Ptr ()] ()
readAttributeSensitivityMaxWithClusterStateCache_endpoint_queue_completionSelector = mkSelector "readAttributeSensitivityMaxWithClusterStateCache:endpoint:queue:completion:"

-- | @Selector@ for @readAttributeSensitivityWithCompletion:@
readAttributeSensitivityWithCompletionSelector :: Selector '[Ptr ()] ()
readAttributeSensitivityWithCompletionSelector = mkSelector "readAttributeSensitivityWithCompletion:"

-- | @Selector@ for @writeAttributeSensitivityWithValue:completion:@
writeAttributeSensitivityWithValue_completionSelector :: Selector '[Id NSNumber, Ptr ()] ()
writeAttributeSensitivityWithValue_completionSelector = mkSelector "writeAttributeSensitivityWithValue:completion:"

-- | @Selector@ for @writeAttributeSensitivityWithValue:params:completion:@
writeAttributeSensitivityWithValue_params_completionSelector :: Selector '[Id NSNumber, Id MTRWriteParams, Ptr ()] ()
writeAttributeSensitivityWithValue_params_completionSelector = mkSelector "writeAttributeSensitivityWithValue:params:completion:"

-- | @Selector@ for @subscribeAttributeSensitivityWithParams:subscriptionEstablished:reportHandler:@
subscribeAttributeSensitivityWithParams_subscriptionEstablished_reportHandlerSelector :: Selector '[Id MTRSubscribeParams, Ptr (), Ptr ()] ()
subscribeAttributeSensitivityWithParams_subscriptionEstablished_reportHandlerSelector = mkSelector "subscribeAttributeSensitivityWithParams:subscriptionEstablished:reportHandler:"

-- | @Selector@ for @readAttributeSensitivityWithClusterStateCache:endpoint:queue:completion:@
readAttributeSensitivityWithClusterStateCache_endpoint_queue_completionSelector :: Selector '[Id MTRClusterStateCacheContainer, Id NSNumber, Id NSObject, Ptr ()] ()
readAttributeSensitivityWithClusterStateCache_endpoint_queue_completionSelector = mkSelector "readAttributeSensitivityWithClusterStateCache:endpoint:queue:completion:"

-- | @Selector@ for @readAttributeTwoDCartesianMaxWithCompletion:@
readAttributeTwoDCartesianMaxWithCompletionSelector :: Selector '[Ptr ()] ()
readAttributeTwoDCartesianMaxWithCompletionSelector = mkSelector "readAttributeTwoDCartesianMaxWithCompletion:"

-- | @Selector@ for @subscribeAttributeTwoDCartesianMaxWithParams:subscriptionEstablished:reportHandler:@
subscribeAttributeTwoDCartesianMaxWithParams_subscriptionEstablished_reportHandlerSelector :: Selector '[Id MTRSubscribeParams, Ptr (), Ptr ()] ()
subscribeAttributeTwoDCartesianMaxWithParams_subscriptionEstablished_reportHandlerSelector = mkSelector "subscribeAttributeTwoDCartesianMaxWithParams:subscriptionEstablished:reportHandler:"

-- | @Selector@ for @readAttributeTwoDCartesianMaxWithClusterStateCache:endpoint:queue:completion:@
readAttributeTwoDCartesianMaxWithClusterStateCache_endpoint_queue_completionSelector :: Selector '[Id MTRClusterStateCacheContainer, Id NSNumber, Id NSObject, Ptr ()] ()
readAttributeTwoDCartesianMaxWithClusterStateCache_endpoint_queue_completionSelector = mkSelector "readAttributeTwoDCartesianMaxWithClusterStateCache:endpoint:queue:completion:"

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
initSelector :: Selector '[] (Id MTRBaseClusterZoneManagement)
initSelector = mkSelector "init"

-- | @Selector@ for @new@
newSelector :: Selector '[] (Id MTRBaseClusterZoneManagement)
newSelector = mkSelector "new"

-- | @Selector@ for @initWithDevice:endpointID:queue:@
initWithDevice_endpointID_queueSelector :: Selector '[Id MTRBaseDevice, Id NSNumber, Id NSObject] (Id MTRBaseClusterZoneManagement)
initWithDevice_endpointID_queueSelector = mkSelector "initWithDevice:endpointID:queue:"

