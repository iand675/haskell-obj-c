{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Cluster Water Heater Management
--
-- This cluster is used to allow clients to control the operation of a hot water heating appliance so that it can be used with energy management.
--
-- Generated bindings for @MTRBaseClusterWaterHeaterManagement@.
module ObjC.Matter.MTRBaseClusterWaterHeaterManagement
  ( MTRBaseClusterWaterHeaterManagement
  , IsMTRBaseClusterWaterHeaterManagement(..)
  , boostWithParams_completion
  , cancelBoostWithParams_completion
  , cancelBoostWithCompletion
  , readAttributeHeaterTypesWithCompletion
  , subscribeAttributeHeaterTypesWithParams_subscriptionEstablished_reportHandler
  , readAttributeHeaterTypesWithClusterStateCache_endpoint_queue_completion
  , readAttributeHeatDemandWithCompletion
  , subscribeAttributeHeatDemandWithParams_subscriptionEstablished_reportHandler
  , readAttributeHeatDemandWithClusterStateCache_endpoint_queue_completion
  , readAttributeTankVolumeWithCompletion
  , subscribeAttributeTankVolumeWithParams_subscriptionEstablished_reportHandler
  , readAttributeTankVolumeWithClusterStateCache_endpoint_queue_completion
  , readAttributeEstimatedHeatRequiredWithCompletion
  , subscribeAttributeEstimatedHeatRequiredWithParams_subscriptionEstablished_reportHandler
  , readAttributeEstimatedHeatRequiredWithClusterStateCache_endpoint_queue_completion
  , readAttributeTankPercentageWithCompletion
  , subscribeAttributeTankPercentageWithParams_subscriptionEstablished_reportHandler
  , readAttributeTankPercentageWithClusterStateCache_endpoint_queue_completion
  , readAttributeBoostStateWithCompletion
  , subscribeAttributeBoostStateWithParams_subscriptionEstablished_reportHandler
  , readAttributeBoostStateWithClusterStateCache_endpoint_queue_completion
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
  , boostWithParams_completionSelector
  , cancelBoostWithCompletionSelector
  , cancelBoostWithParams_completionSelector
  , initSelector
  , initWithDevice_endpointID_queueSelector
  , newSelector
  , readAttributeAcceptedCommandListWithClusterStateCache_endpoint_queue_completionSelector
  , readAttributeAcceptedCommandListWithCompletionSelector
  , readAttributeAttributeListWithClusterStateCache_endpoint_queue_completionSelector
  , readAttributeAttributeListWithCompletionSelector
  , readAttributeBoostStateWithClusterStateCache_endpoint_queue_completionSelector
  , readAttributeBoostStateWithCompletionSelector
  , readAttributeClusterRevisionWithClusterStateCache_endpoint_queue_completionSelector
  , readAttributeClusterRevisionWithCompletionSelector
  , readAttributeEstimatedHeatRequiredWithClusterStateCache_endpoint_queue_completionSelector
  , readAttributeEstimatedHeatRequiredWithCompletionSelector
  , readAttributeFeatureMapWithClusterStateCache_endpoint_queue_completionSelector
  , readAttributeFeatureMapWithCompletionSelector
  , readAttributeGeneratedCommandListWithClusterStateCache_endpoint_queue_completionSelector
  , readAttributeGeneratedCommandListWithCompletionSelector
  , readAttributeHeatDemandWithClusterStateCache_endpoint_queue_completionSelector
  , readAttributeHeatDemandWithCompletionSelector
  , readAttributeHeaterTypesWithClusterStateCache_endpoint_queue_completionSelector
  , readAttributeHeaterTypesWithCompletionSelector
  , readAttributeTankPercentageWithClusterStateCache_endpoint_queue_completionSelector
  , readAttributeTankPercentageWithCompletionSelector
  , readAttributeTankVolumeWithClusterStateCache_endpoint_queue_completionSelector
  , readAttributeTankVolumeWithCompletionSelector
  , subscribeAttributeAcceptedCommandListWithParams_subscriptionEstablished_reportHandlerSelector
  , subscribeAttributeAttributeListWithParams_subscriptionEstablished_reportHandlerSelector
  , subscribeAttributeBoostStateWithParams_subscriptionEstablished_reportHandlerSelector
  , subscribeAttributeClusterRevisionWithParams_subscriptionEstablished_reportHandlerSelector
  , subscribeAttributeEstimatedHeatRequiredWithParams_subscriptionEstablished_reportHandlerSelector
  , subscribeAttributeFeatureMapWithParams_subscriptionEstablished_reportHandlerSelector
  , subscribeAttributeGeneratedCommandListWithParams_subscriptionEstablished_reportHandlerSelector
  , subscribeAttributeHeatDemandWithParams_subscriptionEstablished_reportHandlerSelector
  , subscribeAttributeHeaterTypesWithParams_subscriptionEstablished_reportHandlerSelector
  , subscribeAttributeTankPercentageWithParams_subscriptionEstablished_reportHandlerSelector
  , subscribeAttributeTankVolumeWithParams_subscriptionEstablished_reportHandlerSelector


  ) where

import Foreign.Ptr (Ptr, FunPtr)
import Foreign.C.Types

import ObjC.Runtime.Types
import ObjC.Runtime.Message (sendMessage, sendOwnedMessage, sendClassMessage, sendOwnedClassMessage)
import ObjC.Runtime.Selector (mkSelector)
import ObjC.Runtime.Class (getRequiredClass)

import ObjC.Matter.Internal.Classes
import ObjC.Foundation.Internal.Classes

-- | Command Boost
--
-- Allows a client to request that the water heater is put into a Boost state.
--
-- ObjC selector: @- boostWithParams:completion:@
boostWithParams_completion :: (IsMTRBaseClusterWaterHeaterManagement mtrBaseClusterWaterHeaterManagement, IsMTRWaterHeaterManagementClusterBoostParams params) => mtrBaseClusterWaterHeaterManagement -> params -> Ptr () -> IO ()
boostWithParams_completion mtrBaseClusterWaterHeaterManagement params completion =
  sendMessage mtrBaseClusterWaterHeaterManagement boostWithParams_completionSelector (toMTRWaterHeaterManagementClusterBoostParams params) completion

-- | Command CancelBoost
--
-- Allows a client to cancel an ongoing Boost operation.
--
-- ObjC selector: @- cancelBoostWithParams:completion:@
cancelBoostWithParams_completion :: (IsMTRBaseClusterWaterHeaterManagement mtrBaseClusterWaterHeaterManagement, IsMTRWaterHeaterManagementClusterCancelBoostParams params) => mtrBaseClusterWaterHeaterManagement -> params -> Ptr () -> IO ()
cancelBoostWithParams_completion mtrBaseClusterWaterHeaterManagement params completion =
  sendMessage mtrBaseClusterWaterHeaterManagement cancelBoostWithParams_completionSelector (toMTRWaterHeaterManagementClusterCancelBoostParams params) completion

-- | @- cancelBoostWithCompletion:@
cancelBoostWithCompletion :: IsMTRBaseClusterWaterHeaterManagement mtrBaseClusterWaterHeaterManagement => mtrBaseClusterWaterHeaterManagement -> Ptr () -> IO ()
cancelBoostWithCompletion mtrBaseClusterWaterHeaterManagement completion =
  sendMessage mtrBaseClusterWaterHeaterManagement cancelBoostWithCompletionSelector completion

-- | @- readAttributeHeaterTypesWithCompletion:@
readAttributeHeaterTypesWithCompletion :: IsMTRBaseClusterWaterHeaterManagement mtrBaseClusterWaterHeaterManagement => mtrBaseClusterWaterHeaterManagement -> Ptr () -> IO ()
readAttributeHeaterTypesWithCompletion mtrBaseClusterWaterHeaterManagement completion =
  sendMessage mtrBaseClusterWaterHeaterManagement readAttributeHeaterTypesWithCompletionSelector completion

-- | @- subscribeAttributeHeaterTypesWithParams:subscriptionEstablished:reportHandler:@
subscribeAttributeHeaterTypesWithParams_subscriptionEstablished_reportHandler :: (IsMTRBaseClusterWaterHeaterManagement mtrBaseClusterWaterHeaterManagement, IsMTRSubscribeParams params) => mtrBaseClusterWaterHeaterManagement -> params -> Ptr () -> Ptr () -> IO ()
subscribeAttributeHeaterTypesWithParams_subscriptionEstablished_reportHandler mtrBaseClusterWaterHeaterManagement params subscriptionEstablished reportHandler =
  sendMessage mtrBaseClusterWaterHeaterManagement subscribeAttributeHeaterTypesWithParams_subscriptionEstablished_reportHandlerSelector (toMTRSubscribeParams params) subscriptionEstablished reportHandler

-- | @+ readAttributeHeaterTypesWithClusterStateCache:endpoint:queue:completion:@
readAttributeHeaterTypesWithClusterStateCache_endpoint_queue_completion :: (IsMTRClusterStateCacheContainer clusterStateCacheContainer, IsNSNumber endpoint, IsNSObject queue) => clusterStateCacheContainer -> endpoint -> queue -> Ptr () -> IO ()
readAttributeHeaterTypesWithClusterStateCache_endpoint_queue_completion clusterStateCacheContainer endpoint queue completion =
  do
    cls' <- getRequiredClass "MTRBaseClusterWaterHeaterManagement"
    sendClassMessage cls' readAttributeHeaterTypesWithClusterStateCache_endpoint_queue_completionSelector (toMTRClusterStateCacheContainer clusterStateCacheContainer) (toNSNumber endpoint) (toNSObject queue) completion

-- | @- readAttributeHeatDemandWithCompletion:@
readAttributeHeatDemandWithCompletion :: IsMTRBaseClusterWaterHeaterManagement mtrBaseClusterWaterHeaterManagement => mtrBaseClusterWaterHeaterManagement -> Ptr () -> IO ()
readAttributeHeatDemandWithCompletion mtrBaseClusterWaterHeaterManagement completion =
  sendMessage mtrBaseClusterWaterHeaterManagement readAttributeHeatDemandWithCompletionSelector completion

-- | @- subscribeAttributeHeatDemandWithParams:subscriptionEstablished:reportHandler:@
subscribeAttributeHeatDemandWithParams_subscriptionEstablished_reportHandler :: (IsMTRBaseClusterWaterHeaterManagement mtrBaseClusterWaterHeaterManagement, IsMTRSubscribeParams params) => mtrBaseClusterWaterHeaterManagement -> params -> Ptr () -> Ptr () -> IO ()
subscribeAttributeHeatDemandWithParams_subscriptionEstablished_reportHandler mtrBaseClusterWaterHeaterManagement params subscriptionEstablished reportHandler =
  sendMessage mtrBaseClusterWaterHeaterManagement subscribeAttributeHeatDemandWithParams_subscriptionEstablished_reportHandlerSelector (toMTRSubscribeParams params) subscriptionEstablished reportHandler

-- | @+ readAttributeHeatDemandWithClusterStateCache:endpoint:queue:completion:@
readAttributeHeatDemandWithClusterStateCache_endpoint_queue_completion :: (IsMTRClusterStateCacheContainer clusterStateCacheContainer, IsNSNumber endpoint, IsNSObject queue) => clusterStateCacheContainer -> endpoint -> queue -> Ptr () -> IO ()
readAttributeHeatDemandWithClusterStateCache_endpoint_queue_completion clusterStateCacheContainer endpoint queue completion =
  do
    cls' <- getRequiredClass "MTRBaseClusterWaterHeaterManagement"
    sendClassMessage cls' readAttributeHeatDemandWithClusterStateCache_endpoint_queue_completionSelector (toMTRClusterStateCacheContainer clusterStateCacheContainer) (toNSNumber endpoint) (toNSObject queue) completion

-- | @- readAttributeTankVolumeWithCompletion:@
readAttributeTankVolumeWithCompletion :: IsMTRBaseClusterWaterHeaterManagement mtrBaseClusterWaterHeaterManagement => mtrBaseClusterWaterHeaterManagement -> Ptr () -> IO ()
readAttributeTankVolumeWithCompletion mtrBaseClusterWaterHeaterManagement completion =
  sendMessage mtrBaseClusterWaterHeaterManagement readAttributeTankVolumeWithCompletionSelector completion

-- | @- subscribeAttributeTankVolumeWithParams:subscriptionEstablished:reportHandler:@
subscribeAttributeTankVolumeWithParams_subscriptionEstablished_reportHandler :: (IsMTRBaseClusterWaterHeaterManagement mtrBaseClusterWaterHeaterManagement, IsMTRSubscribeParams params) => mtrBaseClusterWaterHeaterManagement -> params -> Ptr () -> Ptr () -> IO ()
subscribeAttributeTankVolumeWithParams_subscriptionEstablished_reportHandler mtrBaseClusterWaterHeaterManagement params subscriptionEstablished reportHandler =
  sendMessage mtrBaseClusterWaterHeaterManagement subscribeAttributeTankVolumeWithParams_subscriptionEstablished_reportHandlerSelector (toMTRSubscribeParams params) subscriptionEstablished reportHandler

-- | @+ readAttributeTankVolumeWithClusterStateCache:endpoint:queue:completion:@
readAttributeTankVolumeWithClusterStateCache_endpoint_queue_completion :: (IsMTRClusterStateCacheContainer clusterStateCacheContainer, IsNSNumber endpoint, IsNSObject queue) => clusterStateCacheContainer -> endpoint -> queue -> Ptr () -> IO ()
readAttributeTankVolumeWithClusterStateCache_endpoint_queue_completion clusterStateCacheContainer endpoint queue completion =
  do
    cls' <- getRequiredClass "MTRBaseClusterWaterHeaterManagement"
    sendClassMessage cls' readAttributeTankVolumeWithClusterStateCache_endpoint_queue_completionSelector (toMTRClusterStateCacheContainer clusterStateCacheContainer) (toNSNumber endpoint) (toNSObject queue) completion

-- | @- readAttributeEstimatedHeatRequiredWithCompletion:@
readAttributeEstimatedHeatRequiredWithCompletion :: IsMTRBaseClusterWaterHeaterManagement mtrBaseClusterWaterHeaterManagement => mtrBaseClusterWaterHeaterManagement -> Ptr () -> IO ()
readAttributeEstimatedHeatRequiredWithCompletion mtrBaseClusterWaterHeaterManagement completion =
  sendMessage mtrBaseClusterWaterHeaterManagement readAttributeEstimatedHeatRequiredWithCompletionSelector completion

-- | @- subscribeAttributeEstimatedHeatRequiredWithParams:subscriptionEstablished:reportHandler:@
subscribeAttributeEstimatedHeatRequiredWithParams_subscriptionEstablished_reportHandler :: (IsMTRBaseClusterWaterHeaterManagement mtrBaseClusterWaterHeaterManagement, IsMTRSubscribeParams params) => mtrBaseClusterWaterHeaterManagement -> params -> Ptr () -> Ptr () -> IO ()
subscribeAttributeEstimatedHeatRequiredWithParams_subscriptionEstablished_reportHandler mtrBaseClusterWaterHeaterManagement params subscriptionEstablished reportHandler =
  sendMessage mtrBaseClusterWaterHeaterManagement subscribeAttributeEstimatedHeatRequiredWithParams_subscriptionEstablished_reportHandlerSelector (toMTRSubscribeParams params) subscriptionEstablished reportHandler

-- | @+ readAttributeEstimatedHeatRequiredWithClusterStateCache:endpoint:queue:completion:@
readAttributeEstimatedHeatRequiredWithClusterStateCache_endpoint_queue_completion :: (IsMTRClusterStateCacheContainer clusterStateCacheContainer, IsNSNumber endpoint, IsNSObject queue) => clusterStateCacheContainer -> endpoint -> queue -> Ptr () -> IO ()
readAttributeEstimatedHeatRequiredWithClusterStateCache_endpoint_queue_completion clusterStateCacheContainer endpoint queue completion =
  do
    cls' <- getRequiredClass "MTRBaseClusterWaterHeaterManagement"
    sendClassMessage cls' readAttributeEstimatedHeatRequiredWithClusterStateCache_endpoint_queue_completionSelector (toMTRClusterStateCacheContainer clusterStateCacheContainer) (toNSNumber endpoint) (toNSObject queue) completion

-- | @- readAttributeTankPercentageWithCompletion:@
readAttributeTankPercentageWithCompletion :: IsMTRBaseClusterWaterHeaterManagement mtrBaseClusterWaterHeaterManagement => mtrBaseClusterWaterHeaterManagement -> Ptr () -> IO ()
readAttributeTankPercentageWithCompletion mtrBaseClusterWaterHeaterManagement completion =
  sendMessage mtrBaseClusterWaterHeaterManagement readAttributeTankPercentageWithCompletionSelector completion

-- | @- subscribeAttributeTankPercentageWithParams:subscriptionEstablished:reportHandler:@
subscribeAttributeTankPercentageWithParams_subscriptionEstablished_reportHandler :: (IsMTRBaseClusterWaterHeaterManagement mtrBaseClusterWaterHeaterManagement, IsMTRSubscribeParams params) => mtrBaseClusterWaterHeaterManagement -> params -> Ptr () -> Ptr () -> IO ()
subscribeAttributeTankPercentageWithParams_subscriptionEstablished_reportHandler mtrBaseClusterWaterHeaterManagement params subscriptionEstablished reportHandler =
  sendMessage mtrBaseClusterWaterHeaterManagement subscribeAttributeTankPercentageWithParams_subscriptionEstablished_reportHandlerSelector (toMTRSubscribeParams params) subscriptionEstablished reportHandler

-- | @+ readAttributeTankPercentageWithClusterStateCache:endpoint:queue:completion:@
readAttributeTankPercentageWithClusterStateCache_endpoint_queue_completion :: (IsMTRClusterStateCacheContainer clusterStateCacheContainer, IsNSNumber endpoint, IsNSObject queue) => clusterStateCacheContainer -> endpoint -> queue -> Ptr () -> IO ()
readAttributeTankPercentageWithClusterStateCache_endpoint_queue_completion clusterStateCacheContainer endpoint queue completion =
  do
    cls' <- getRequiredClass "MTRBaseClusterWaterHeaterManagement"
    sendClassMessage cls' readAttributeTankPercentageWithClusterStateCache_endpoint_queue_completionSelector (toMTRClusterStateCacheContainer clusterStateCacheContainer) (toNSNumber endpoint) (toNSObject queue) completion

-- | @- readAttributeBoostStateWithCompletion:@
readAttributeBoostStateWithCompletion :: IsMTRBaseClusterWaterHeaterManagement mtrBaseClusterWaterHeaterManagement => mtrBaseClusterWaterHeaterManagement -> Ptr () -> IO ()
readAttributeBoostStateWithCompletion mtrBaseClusterWaterHeaterManagement completion =
  sendMessage mtrBaseClusterWaterHeaterManagement readAttributeBoostStateWithCompletionSelector completion

-- | @- subscribeAttributeBoostStateWithParams:subscriptionEstablished:reportHandler:@
subscribeAttributeBoostStateWithParams_subscriptionEstablished_reportHandler :: (IsMTRBaseClusterWaterHeaterManagement mtrBaseClusterWaterHeaterManagement, IsMTRSubscribeParams params) => mtrBaseClusterWaterHeaterManagement -> params -> Ptr () -> Ptr () -> IO ()
subscribeAttributeBoostStateWithParams_subscriptionEstablished_reportHandler mtrBaseClusterWaterHeaterManagement params subscriptionEstablished reportHandler =
  sendMessage mtrBaseClusterWaterHeaterManagement subscribeAttributeBoostStateWithParams_subscriptionEstablished_reportHandlerSelector (toMTRSubscribeParams params) subscriptionEstablished reportHandler

-- | @+ readAttributeBoostStateWithClusterStateCache:endpoint:queue:completion:@
readAttributeBoostStateWithClusterStateCache_endpoint_queue_completion :: (IsMTRClusterStateCacheContainer clusterStateCacheContainer, IsNSNumber endpoint, IsNSObject queue) => clusterStateCacheContainer -> endpoint -> queue -> Ptr () -> IO ()
readAttributeBoostStateWithClusterStateCache_endpoint_queue_completion clusterStateCacheContainer endpoint queue completion =
  do
    cls' <- getRequiredClass "MTRBaseClusterWaterHeaterManagement"
    sendClassMessage cls' readAttributeBoostStateWithClusterStateCache_endpoint_queue_completionSelector (toMTRClusterStateCacheContainer clusterStateCacheContainer) (toNSNumber endpoint) (toNSObject queue) completion

-- | @- readAttributeGeneratedCommandListWithCompletion:@
readAttributeGeneratedCommandListWithCompletion :: IsMTRBaseClusterWaterHeaterManagement mtrBaseClusterWaterHeaterManagement => mtrBaseClusterWaterHeaterManagement -> Ptr () -> IO ()
readAttributeGeneratedCommandListWithCompletion mtrBaseClusterWaterHeaterManagement completion =
  sendMessage mtrBaseClusterWaterHeaterManagement readAttributeGeneratedCommandListWithCompletionSelector completion

-- | @- subscribeAttributeGeneratedCommandListWithParams:subscriptionEstablished:reportHandler:@
subscribeAttributeGeneratedCommandListWithParams_subscriptionEstablished_reportHandler :: (IsMTRBaseClusterWaterHeaterManagement mtrBaseClusterWaterHeaterManagement, IsMTRSubscribeParams params) => mtrBaseClusterWaterHeaterManagement -> params -> Ptr () -> Ptr () -> IO ()
subscribeAttributeGeneratedCommandListWithParams_subscriptionEstablished_reportHandler mtrBaseClusterWaterHeaterManagement params subscriptionEstablished reportHandler =
  sendMessage mtrBaseClusterWaterHeaterManagement subscribeAttributeGeneratedCommandListWithParams_subscriptionEstablished_reportHandlerSelector (toMTRSubscribeParams params) subscriptionEstablished reportHandler

-- | @+ readAttributeGeneratedCommandListWithClusterStateCache:endpoint:queue:completion:@
readAttributeGeneratedCommandListWithClusterStateCache_endpoint_queue_completion :: (IsMTRClusterStateCacheContainer clusterStateCacheContainer, IsNSNumber endpoint, IsNSObject queue) => clusterStateCacheContainer -> endpoint -> queue -> Ptr () -> IO ()
readAttributeGeneratedCommandListWithClusterStateCache_endpoint_queue_completion clusterStateCacheContainer endpoint queue completion =
  do
    cls' <- getRequiredClass "MTRBaseClusterWaterHeaterManagement"
    sendClassMessage cls' readAttributeGeneratedCommandListWithClusterStateCache_endpoint_queue_completionSelector (toMTRClusterStateCacheContainer clusterStateCacheContainer) (toNSNumber endpoint) (toNSObject queue) completion

-- | @- readAttributeAcceptedCommandListWithCompletion:@
readAttributeAcceptedCommandListWithCompletion :: IsMTRBaseClusterWaterHeaterManagement mtrBaseClusterWaterHeaterManagement => mtrBaseClusterWaterHeaterManagement -> Ptr () -> IO ()
readAttributeAcceptedCommandListWithCompletion mtrBaseClusterWaterHeaterManagement completion =
  sendMessage mtrBaseClusterWaterHeaterManagement readAttributeAcceptedCommandListWithCompletionSelector completion

-- | @- subscribeAttributeAcceptedCommandListWithParams:subscriptionEstablished:reportHandler:@
subscribeAttributeAcceptedCommandListWithParams_subscriptionEstablished_reportHandler :: (IsMTRBaseClusterWaterHeaterManagement mtrBaseClusterWaterHeaterManagement, IsMTRSubscribeParams params) => mtrBaseClusterWaterHeaterManagement -> params -> Ptr () -> Ptr () -> IO ()
subscribeAttributeAcceptedCommandListWithParams_subscriptionEstablished_reportHandler mtrBaseClusterWaterHeaterManagement params subscriptionEstablished reportHandler =
  sendMessage mtrBaseClusterWaterHeaterManagement subscribeAttributeAcceptedCommandListWithParams_subscriptionEstablished_reportHandlerSelector (toMTRSubscribeParams params) subscriptionEstablished reportHandler

-- | @+ readAttributeAcceptedCommandListWithClusterStateCache:endpoint:queue:completion:@
readAttributeAcceptedCommandListWithClusterStateCache_endpoint_queue_completion :: (IsMTRClusterStateCacheContainer clusterStateCacheContainer, IsNSNumber endpoint, IsNSObject queue) => clusterStateCacheContainer -> endpoint -> queue -> Ptr () -> IO ()
readAttributeAcceptedCommandListWithClusterStateCache_endpoint_queue_completion clusterStateCacheContainer endpoint queue completion =
  do
    cls' <- getRequiredClass "MTRBaseClusterWaterHeaterManagement"
    sendClassMessage cls' readAttributeAcceptedCommandListWithClusterStateCache_endpoint_queue_completionSelector (toMTRClusterStateCacheContainer clusterStateCacheContainer) (toNSNumber endpoint) (toNSObject queue) completion

-- | @- readAttributeAttributeListWithCompletion:@
readAttributeAttributeListWithCompletion :: IsMTRBaseClusterWaterHeaterManagement mtrBaseClusterWaterHeaterManagement => mtrBaseClusterWaterHeaterManagement -> Ptr () -> IO ()
readAttributeAttributeListWithCompletion mtrBaseClusterWaterHeaterManagement completion =
  sendMessage mtrBaseClusterWaterHeaterManagement readAttributeAttributeListWithCompletionSelector completion

-- | @- subscribeAttributeAttributeListWithParams:subscriptionEstablished:reportHandler:@
subscribeAttributeAttributeListWithParams_subscriptionEstablished_reportHandler :: (IsMTRBaseClusterWaterHeaterManagement mtrBaseClusterWaterHeaterManagement, IsMTRSubscribeParams params) => mtrBaseClusterWaterHeaterManagement -> params -> Ptr () -> Ptr () -> IO ()
subscribeAttributeAttributeListWithParams_subscriptionEstablished_reportHandler mtrBaseClusterWaterHeaterManagement params subscriptionEstablished reportHandler =
  sendMessage mtrBaseClusterWaterHeaterManagement subscribeAttributeAttributeListWithParams_subscriptionEstablished_reportHandlerSelector (toMTRSubscribeParams params) subscriptionEstablished reportHandler

-- | @+ readAttributeAttributeListWithClusterStateCache:endpoint:queue:completion:@
readAttributeAttributeListWithClusterStateCache_endpoint_queue_completion :: (IsMTRClusterStateCacheContainer clusterStateCacheContainer, IsNSNumber endpoint, IsNSObject queue) => clusterStateCacheContainer -> endpoint -> queue -> Ptr () -> IO ()
readAttributeAttributeListWithClusterStateCache_endpoint_queue_completion clusterStateCacheContainer endpoint queue completion =
  do
    cls' <- getRequiredClass "MTRBaseClusterWaterHeaterManagement"
    sendClassMessage cls' readAttributeAttributeListWithClusterStateCache_endpoint_queue_completionSelector (toMTRClusterStateCacheContainer clusterStateCacheContainer) (toNSNumber endpoint) (toNSObject queue) completion

-- | @- readAttributeFeatureMapWithCompletion:@
readAttributeFeatureMapWithCompletion :: IsMTRBaseClusterWaterHeaterManagement mtrBaseClusterWaterHeaterManagement => mtrBaseClusterWaterHeaterManagement -> Ptr () -> IO ()
readAttributeFeatureMapWithCompletion mtrBaseClusterWaterHeaterManagement completion =
  sendMessage mtrBaseClusterWaterHeaterManagement readAttributeFeatureMapWithCompletionSelector completion

-- | @- subscribeAttributeFeatureMapWithParams:subscriptionEstablished:reportHandler:@
subscribeAttributeFeatureMapWithParams_subscriptionEstablished_reportHandler :: (IsMTRBaseClusterWaterHeaterManagement mtrBaseClusterWaterHeaterManagement, IsMTRSubscribeParams params) => mtrBaseClusterWaterHeaterManagement -> params -> Ptr () -> Ptr () -> IO ()
subscribeAttributeFeatureMapWithParams_subscriptionEstablished_reportHandler mtrBaseClusterWaterHeaterManagement params subscriptionEstablished reportHandler =
  sendMessage mtrBaseClusterWaterHeaterManagement subscribeAttributeFeatureMapWithParams_subscriptionEstablished_reportHandlerSelector (toMTRSubscribeParams params) subscriptionEstablished reportHandler

-- | @+ readAttributeFeatureMapWithClusterStateCache:endpoint:queue:completion:@
readAttributeFeatureMapWithClusterStateCache_endpoint_queue_completion :: (IsMTRClusterStateCacheContainer clusterStateCacheContainer, IsNSNumber endpoint, IsNSObject queue) => clusterStateCacheContainer -> endpoint -> queue -> Ptr () -> IO ()
readAttributeFeatureMapWithClusterStateCache_endpoint_queue_completion clusterStateCacheContainer endpoint queue completion =
  do
    cls' <- getRequiredClass "MTRBaseClusterWaterHeaterManagement"
    sendClassMessage cls' readAttributeFeatureMapWithClusterStateCache_endpoint_queue_completionSelector (toMTRClusterStateCacheContainer clusterStateCacheContainer) (toNSNumber endpoint) (toNSObject queue) completion

-- | @- readAttributeClusterRevisionWithCompletion:@
readAttributeClusterRevisionWithCompletion :: IsMTRBaseClusterWaterHeaterManagement mtrBaseClusterWaterHeaterManagement => mtrBaseClusterWaterHeaterManagement -> Ptr () -> IO ()
readAttributeClusterRevisionWithCompletion mtrBaseClusterWaterHeaterManagement completion =
  sendMessage mtrBaseClusterWaterHeaterManagement readAttributeClusterRevisionWithCompletionSelector completion

-- | @- subscribeAttributeClusterRevisionWithParams:subscriptionEstablished:reportHandler:@
subscribeAttributeClusterRevisionWithParams_subscriptionEstablished_reportHandler :: (IsMTRBaseClusterWaterHeaterManagement mtrBaseClusterWaterHeaterManagement, IsMTRSubscribeParams params) => mtrBaseClusterWaterHeaterManagement -> params -> Ptr () -> Ptr () -> IO ()
subscribeAttributeClusterRevisionWithParams_subscriptionEstablished_reportHandler mtrBaseClusterWaterHeaterManagement params subscriptionEstablished reportHandler =
  sendMessage mtrBaseClusterWaterHeaterManagement subscribeAttributeClusterRevisionWithParams_subscriptionEstablished_reportHandlerSelector (toMTRSubscribeParams params) subscriptionEstablished reportHandler

-- | @+ readAttributeClusterRevisionWithClusterStateCache:endpoint:queue:completion:@
readAttributeClusterRevisionWithClusterStateCache_endpoint_queue_completion :: (IsMTRClusterStateCacheContainer clusterStateCacheContainer, IsNSNumber endpoint, IsNSObject queue) => clusterStateCacheContainer -> endpoint -> queue -> Ptr () -> IO ()
readAttributeClusterRevisionWithClusterStateCache_endpoint_queue_completion clusterStateCacheContainer endpoint queue completion =
  do
    cls' <- getRequiredClass "MTRBaseClusterWaterHeaterManagement"
    sendClassMessage cls' readAttributeClusterRevisionWithClusterStateCache_endpoint_queue_completionSelector (toMTRClusterStateCacheContainer clusterStateCacheContainer) (toNSNumber endpoint) (toNSObject queue) completion

-- | @- init@
init_ :: IsMTRBaseClusterWaterHeaterManagement mtrBaseClusterWaterHeaterManagement => mtrBaseClusterWaterHeaterManagement -> IO (Id MTRBaseClusterWaterHeaterManagement)
init_ mtrBaseClusterWaterHeaterManagement =
  sendOwnedMessage mtrBaseClusterWaterHeaterManagement initSelector

-- | @+ new@
new :: IO (Id MTRBaseClusterWaterHeaterManagement)
new  =
  do
    cls' <- getRequiredClass "MTRBaseClusterWaterHeaterManagement"
    sendOwnedClassMessage cls' newSelector

-- | For all instance methods (reads, writes, commands) that take a completion, the completion will be called on the provided queue.
--
-- ObjC selector: @- initWithDevice:endpointID:queue:@
initWithDevice_endpointID_queue :: (IsMTRBaseClusterWaterHeaterManagement mtrBaseClusterWaterHeaterManagement, IsMTRBaseDevice device, IsNSNumber endpointID, IsNSObject queue) => mtrBaseClusterWaterHeaterManagement -> device -> endpointID -> queue -> IO (Id MTRBaseClusterWaterHeaterManagement)
initWithDevice_endpointID_queue mtrBaseClusterWaterHeaterManagement device endpointID queue =
  sendOwnedMessage mtrBaseClusterWaterHeaterManagement initWithDevice_endpointID_queueSelector (toMTRBaseDevice device) (toNSNumber endpointID) (toNSObject queue)

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @boostWithParams:completion:@
boostWithParams_completionSelector :: Selector '[Id MTRWaterHeaterManagementClusterBoostParams, Ptr ()] ()
boostWithParams_completionSelector = mkSelector "boostWithParams:completion:"

-- | @Selector@ for @cancelBoostWithParams:completion:@
cancelBoostWithParams_completionSelector :: Selector '[Id MTRWaterHeaterManagementClusterCancelBoostParams, Ptr ()] ()
cancelBoostWithParams_completionSelector = mkSelector "cancelBoostWithParams:completion:"

-- | @Selector@ for @cancelBoostWithCompletion:@
cancelBoostWithCompletionSelector :: Selector '[Ptr ()] ()
cancelBoostWithCompletionSelector = mkSelector "cancelBoostWithCompletion:"

-- | @Selector@ for @readAttributeHeaterTypesWithCompletion:@
readAttributeHeaterTypesWithCompletionSelector :: Selector '[Ptr ()] ()
readAttributeHeaterTypesWithCompletionSelector = mkSelector "readAttributeHeaterTypesWithCompletion:"

-- | @Selector@ for @subscribeAttributeHeaterTypesWithParams:subscriptionEstablished:reportHandler:@
subscribeAttributeHeaterTypesWithParams_subscriptionEstablished_reportHandlerSelector :: Selector '[Id MTRSubscribeParams, Ptr (), Ptr ()] ()
subscribeAttributeHeaterTypesWithParams_subscriptionEstablished_reportHandlerSelector = mkSelector "subscribeAttributeHeaterTypesWithParams:subscriptionEstablished:reportHandler:"

-- | @Selector@ for @readAttributeHeaterTypesWithClusterStateCache:endpoint:queue:completion:@
readAttributeHeaterTypesWithClusterStateCache_endpoint_queue_completionSelector :: Selector '[Id MTRClusterStateCacheContainer, Id NSNumber, Id NSObject, Ptr ()] ()
readAttributeHeaterTypesWithClusterStateCache_endpoint_queue_completionSelector = mkSelector "readAttributeHeaterTypesWithClusterStateCache:endpoint:queue:completion:"

-- | @Selector@ for @readAttributeHeatDemandWithCompletion:@
readAttributeHeatDemandWithCompletionSelector :: Selector '[Ptr ()] ()
readAttributeHeatDemandWithCompletionSelector = mkSelector "readAttributeHeatDemandWithCompletion:"

-- | @Selector@ for @subscribeAttributeHeatDemandWithParams:subscriptionEstablished:reportHandler:@
subscribeAttributeHeatDemandWithParams_subscriptionEstablished_reportHandlerSelector :: Selector '[Id MTRSubscribeParams, Ptr (), Ptr ()] ()
subscribeAttributeHeatDemandWithParams_subscriptionEstablished_reportHandlerSelector = mkSelector "subscribeAttributeHeatDemandWithParams:subscriptionEstablished:reportHandler:"

-- | @Selector@ for @readAttributeHeatDemandWithClusterStateCache:endpoint:queue:completion:@
readAttributeHeatDemandWithClusterStateCache_endpoint_queue_completionSelector :: Selector '[Id MTRClusterStateCacheContainer, Id NSNumber, Id NSObject, Ptr ()] ()
readAttributeHeatDemandWithClusterStateCache_endpoint_queue_completionSelector = mkSelector "readAttributeHeatDemandWithClusterStateCache:endpoint:queue:completion:"

-- | @Selector@ for @readAttributeTankVolumeWithCompletion:@
readAttributeTankVolumeWithCompletionSelector :: Selector '[Ptr ()] ()
readAttributeTankVolumeWithCompletionSelector = mkSelector "readAttributeTankVolumeWithCompletion:"

-- | @Selector@ for @subscribeAttributeTankVolumeWithParams:subscriptionEstablished:reportHandler:@
subscribeAttributeTankVolumeWithParams_subscriptionEstablished_reportHandlerSelector :: Selector '[Id MTRSubscribeParams, Ptr (), Ptr ()] ()
subscribeAttributeTankVolumeWithParams_subscriptionEstablished_reportHandlerSelector = mkSelector "subscribeAttributeTankVolumeWithParams:subscriptionEstablished:reportHandler:"

-- | @Selector@ for @readAttributeTankVolumeWithClusterStateCache:endpoint:queue:completion:@
readAttributeTankVolumeWithClusterStateCache_endpoint_queue_completionSelector :: Selector '[Id MTRClusterStateCacheContainer, Id NSNumber, Id NSObject, Ptr ()] ()
readAttributeTankVolumeWithClusterStateCache_endpoint_queue_completionSelector = mkSelector "readAttributeTankVolumeWithClusterStateCache:endpoint:queue:completion:"

-- | @Selector@ for @readAttributeEstimatedHeatRequiredWithCompletion:@
readAttributeEstimatedHeatRequiredWithCompletionSelector :: Selector '[Ptr ()] ()
readAttributeEstimatedHeatRequiredWithCompletionSelector = mkSelector "readAttributeEstimatedHeatRequiredWithCompletion:"

-- | @Selector@ for @subscribeAttributeEstimatedHeatRequiredWithParams:subscriptionEstablished:reportHandler:@
subscribeAttributeEstimatedHeatRequiredWithParams_subscriptionEstablished_reportHandlerSelector :: Selector '[Id MTRSubscribeParams, Ptr (), Ptr ()] ()
subscribeAttributeEstimatedHeatRequiredWithParams_subscriptionEstablished_reportHandlerSelector = mkSelector "subscribeAttributeEstimatedHeatRequiredWithParams:subscriptionEstablished:reportHandler:"

-- | @Selector@ for @readAttributeEstimatedHeatRequiredWithClusterStateCache:endpoint:queue:completion:@
readAttributeEstimatedHeatRequiredWithClusterStateCache_endpoint_queue_completionSelector :: Selector '[Id MTRClusterStateCacheContainer, Id NSNumber, Id NSObject, Ptr ()] ()
readAttributeEstimatedHeatRequiredWithClusterStateCache_endpoint_queue_completionSelector = mkSelector "readAttributeEstimatedHeatRequiredWithClusterStateCache:endpoint:queue:completion:"

-- | @Selector@ for @readAttributeTankPercentageWithCompletion:@
readAttributeTankPercentageWithCompletionSelector :: Selector '[Ptr ()] ()
readAttributeTankPercentageWithCompletionSelector = mkSelector "readAttributeTankPercentageWithCompletion:"

-- | @Selector@ for @subscribeAttributeTankPercentageWithParams:subscriptionEstablished:reportHandler:@
subscribeAttributeTankPercentageWithParams_subscriptionEstablished_reportHandlerSelector :: Selector '[Id MTRSubscribeParams, Ptr (), Ptr ()] ()
subscribeAttributeTankPercentageWithParams_subscriptionEstablished_reportHandlerSelector = mkSelector "subscribeAttributeTankPercentageWithParams:subscriptionEstablished:reportHandler:"

-- | @Selector@ for @readAttributeTankPercentageWithClusterStateCache:endpoint:queue:completion:@
readAttributeTankPercentageWithClusterStateCache_endpoint_queue_completionSelector :: Selector '[Id MTRClusterStateCacheContainer, Id NSNumber, Id NSObject, Ptr ()] ()
readAttributeTankPercentageWithClusterStateCache_endpoint_queue_completionSelector = mkSelector "readAttributeTankPercentageWithClusterStateCache:endpoint:queue:completion:"

-- | @Selector@ for @readAttributeBoostStateWithCompletion:@
readAttributeBoostStateWithCompletionSelector :: Selector '[Ptr ()] ()
readAttributeBoostStateWithCompletionSelector = mkSelector "readAttributeBoostStateWithCompletion:"

-- | @Selector@ for @subscribeAttributeBoostStateWithParams:subscriptionEstablished:reportHandler:@
subscribeAttributeBoostStateWithParams_subscriptionEstablished_reportHandlerSelector :: Selector '[Id MTRSubscribeParams, Ptr (), Ptr ()] ()
subscribeAttributeBoostStateWithParams_subscriptionEstablished_reportHandlerSelector = mkSelector "subscribeAttributeBoostStateWithParams:subscriptionEstablished:reportHandler:"

-- | @Selector@ for @readAttributeBoostStateWithClusterStateCache:endpoint:queue:completion:@
readAttributeBoostStateWithClusterStateCache_endpoint_queue_completionSelector :: Selector '[Id MTRClusterStateCacheContainer, Id NSNumber, Id NSObject, Ptr ()] ()
readAttributeBoostStateWithClusterStateCache_endpoint_queue_completionSelector = mkSelector "readAttributeBoostStateWithClusterStateCache:endpoint:queue:completion:"

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
initSelector :: Selector '[] (Id MTRBaseClusterWaterHeaterManagement)
initSelector = mkSelector "init"

-- | @Selector@ for @new@
newSelector :: Selector '[] (Id MTRBaseClusterWaterHeaterManagement)
newSelector = mkSelector "new"

-- | @Selector@ for @initWithDevice:endpointID:queue:@
initWithDevice_endpointID_queueSelector :: Selector '[Id MTRBaseDevice, Id NSNumber, Id NSObject] (Id MTRBaseClusterWaterHeaterManagement)
initWithDevice_endpointID_queueSelector = mkSelector "initWithDevice:endpointID:queue:"

