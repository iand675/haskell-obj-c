{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Cluster Descriptor
--
-- The Descriptor Cluster is meant to replace the support from the Zigbee Device Object (ZDO) for describing a node, its endpoints and clusters.
--
-- Generated bindings for @MTRBaseClusterDescriptor@.
module ObjC.Matter.MTRBaseClusterDescriptor
  ( MTRBaseClusterDescriptor
  , IsMTRBaseClusterDescriptor(..)
  , readAttributeDeviceTypeListWithCompletion
  , subscribeAttributeDeviceTypeListWithParams_subscriptionEstablished_reportHandler
  , readAttributeDeviceTypeListWithClusterStateCache_endpoint_queue_completion
  , readAttributeServerListWithCompletion
  , subscribeAttributeServerListWithParams_subscriptionEstablished_reportHandler
  , readAttributeServerListWithClusterStateCache_endpoint_queue_completion
  , readAttributeClientListWithCompletion
  , subscribeAttributeClientListWithParams_subscriptionEstablished_reportHandler
  , readAttributeClientListWithClusterStateCache_endpoint_queue_completion
  , readAttributePartsListWithCompletion
  , subscribeAttributePartsListWithParams_subscriptionEstablished_reportHandler
  , readAttributePartsListWithClusterStateCache_endpoint_queue_completion
  , readAttributeTagListWithCompletion
  , subscribeAttributeTagListWithParams_subscriptionEstablished_reportHandler
  , readAttributeTagListWithClusterStateCache_endpoint_queue_completion
  , readAttributeEndpointUniqueIDWithCompletion
  , subscribeAttributeEndpointUniqueIDWithParams_subscriptionEstablished_reportHandler
  , readAttributeEndpointUniqueIDWithClusterStateCache_endpoint_queue_completion
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
  , readAttributeDeviceListWithCompletionHandler
  , subscribeAttributeDeviceListWithMinInterval_maxInterval_params_subscriptionEstablished_reportHandler
  , readAttributeDeviceListWithAttributeCache_endpoint_queue_completionHandler
  , readAttributeServerListWithCompletionHandler
  , subscribeAttributeServerListWithMinInterval_maxInterval_params_subscriptionEstablished_reportHandler
  , readAttributeServerListWithAttributeCache_endpoint_queue_completionHandler
  , readAttributeClientListWithCompletionHandler
  , subscribeAttributeClientListWithMinInterval_maxInterval_params_subscriptionEstablished_reportHandler
  , readAttributeClientListWithAttributeCache_endpoint_queue_completionHandler
  , readAttributePartsListWithCompletionHandler
  , subscribeAttributePartsListWithMinInterval_maxInterval_params_subscriptionEstablished_reportHandler
  , readAttributePartsListWithAttributeCache_endpoint_queue_completionHandler
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
  , readAttributeAcceptedCommandListWithAttributeCache_endpoint_queue_completionHandlerSelector
  , readAttributeAcceptedCommandListWithClusterStateCache_endpoint_queue_completionSelector
  , readAttributeAcceptedCommandListWithCompletionHandlerSelector
  , readAttributeAcceptedCommandListWithCompletionSelector
  , readAttributeAttributeListWithAttributeCache_endpoint_queue_completionHandlerSelector
  , readAttributeAttributeListWithClusterStateCache_endpoint_queue_completionSelector
  , readAttributeAttributeListWithCompletionHandlerSelector
  , readAttributeAttributeListWithCompletionSelector
  , readAttributeClientListWithAttributeCache_endpoint_queue_completionHandlerSelector
  , readAttributeClientListWithClusterStateCache_endpoint_queue_completionSelector
  , readAttributeClientListWithCompletionHandlerSelector
  , readAttributeClientListWithCompletionSelector
  , readAttributeClusterRevisionWithAttributeCache_endpoint_queue_completionHandlerSelector
  , readAttributeClusterRevisionWithClusterStateCache_endpoint_queue_completionSelector
  , readAttributeClusterRevisionWithCompletionHandlerSelector
  , readAttributeClusterRevisionWithCompletionSelector
  , readAttributeDeviceListWithAttributeCache_endpoint_queue_completionHandlerSelector
  , readAttributeDeviceListWithCompletionHandlerSelector
  , readAttributeDeviceTypeListWithClusterStateCache_endpoint_queue_completionSelector
  , readAttributeDeviceTypeListWithCompletionSelector
  , readAttributeEndpointUniqueIDWithClusterStateCache_endpoint_queue_completionSelector
  , readAttributeEndpointUniqueIDWithCompletionSelector
  , readAttributeFeatureMapWithAttributeCache_endpoint_queue_completionHandlerSelector
  , readAttributeFeatureMapWithClusterStateCache_endpoint_queue_completionSelector
  , readAttributeFeatureMapWithCompletionHandlerSelector
  , readAttributeFeatureMapWithCompletionSelector
  , readAttributeGeneratedCommandListWithAttributeCache_endpoint_queue_completionHandlerSelector
  , readAttributeGeneratedCommandListWithClusterStateCache_endpoint_queue_completionSelector
  , readAttributeGeneratedCommandListWithCompletionHandlerSelector
  , readAttributeGeneratedCommandListWithCompletionSelector
  , readAttributePartsListWithAttributeCache_endpoint_queue_completionHandlerSelector
  , readAttributePartsListWithClusterStateCache_endpoint_queue_completionSelector
  , readAttributePartsListWithCompletionHandlerSelector
  , readAttributePartsListWithCompletionSelector
  , readAttributeServerListWithAttributeCache_endpoint_queue_completionHandlerSelector
  , readAttributeServerListWithClusterStateCache_endpoint_queue_completionSelector
  , readAttributeServerListWithCompletionHandlerSelector
  , readAttributeServerListWithCompletionSelector
  , readAttributeTagListWithClusterStateCache_endpoint_queue_completionSelector
  , readAttributeTagListWithCompletionSelector
  , subscribeAttributeAcceptedCommandListWithMinInterval_maxInterval_params_subscriptionEstablished_reportHandlerSelector
  , subscribeAttributeAcceptedCommandListWithParams_subscriptionEstablished_reportHandlerSelector
  , subscribeAttributeAttributeListWithMinInterval_maxInterval_params_subscriptionEstablished_reportHandlerSelector
  , subscribeAttributeAttributeListWithParams_subscriptionEstablished_reportHandlerSelector
  , subscribeAttributeClientListWithMinInterval_maxInterval_params_subscriptionEstablished_reportHandlerSelector
  , subscribeAttributeClientListWithParams_subscriptionEstablished_reportHandlerSelector
  , subscribeAttributeClusterRevisionWithMinInterval_maxInterval_params_subscriptionEstablished_reportHandlerSelector
  , subscribeAttributeClusterRevisionWithParams_subscriptionEstablished_reportHandlerSelector
  , subscribeAttributeDeviceListWithMinInterval_maxInterval_params_subscriptionEstablished_reportHandlerSelector
  , subscribeAttributeDeviceTypeListWithParams_subscriptionEstablished_reportHandlerSelector
  , subscribeAttributeEndpointUniqueIDWithParams_subscriptionEstablished_reportHandlerSelector
  , subscribeAttributeFeatureMapWithMinInterval_maxInterval_params_subscriptionEstablished_reportHandlerSelector
  , subscribeAttributeFeatureMapWithParams_subscriptionEstablished_reportHandlerSelector
  , subscribeAttributeGeneratedCommandListWithMinInterval_maxInterval_params_subscriptionEstablished_reportHandlerSelector
  , subscribeAttributeGeneratedCommandListWithParams_subscriptionEstablished_reportHandlerSelector
  , subscribeAttributePartsListWithMinInterval_maxInterval_params_subscriptionEstablished_reportHandlerSelector
  , subscribeAttributePartsListWithParams_subscriptionEstablished_reportHandlerSelector
  , subscribeAttributeServerListWithMinInterval_maxInterval_params_subscriptionEstablished_reportHandlerSelector
  , subscribeAttributeServerListWithParams_subscriptionEstablished_reportHandlerSelector
  , subscribeAttributeTagListWithParams_subscriptionEstablished_reportHandlerSelector


  ) where

import Foreign.Ptr (Ptr, FunPtr)
import Foreign.C.Types

import ObjC.Runtime.Types
import ObjC.Runtime.Message (sendMessage, sendOwnedMessage, sendClassMessage, sendOwnedClassMessage)
import ObjC.Runtime.Selector (mkSelector)
import ObjC.Runtime.Class (getRequiredClass)

import ObjC.Matter.Internal.Classes
import ObjC.Foundation.Internal.Classes

-- | @- readAttributeDeviceTypeListWithCompletion:@
readAttributeDeviceTypeListWithCompletion :: IsMTRBaseClusterDescriptor mtrBaseClusterDescriptor => mtrBaseClusterDescriptor -> Ptr () -> IO ()
readAttributeDeviceTypeListWithCompletion mtrBaseClusterDescriptor completion =
  sendMessage mtrBaseClusterDescriptor readAttributeDeviceTypeListWithCompletionSelector completion

-- | @- subscribeAttributeDeviceTypeListWithParams:subscriptionEstablished:reportHandler:@
subscribeAttributeDeviceTypeListWithParams_subscriptionEstablished_reportHandler :: (IsMTRBaseClusterDescriptor mtrBaseClusterDescriptor, IsMTRSubscribeParams params) => mtrBaseClusterDescriptor -> params -> Ptr () -> Ptr () -> IO ()
subscribeAttributeDeviceTypeListWithParams_subscriptionEstablished_reportHandler mtrBaseClusterDescriptor params subscriptionEstablished reportHandler =
  sendMessage mtrBaseClusterDescriptor subscribeAttributeDeviceTypeListWithParams_subscriptionEstablished_reportHandlerSelector (toMTRSubscribeParams params) subscriptionEstablished reportHandler

-- | @+ readAttributeDeviceTypeListWithClusterStateCache:endpoint:queue:completion:@
readAttributeDeviceTypeListWithClusterStateCache_endpoint_queue_completion :: (IsMTRClusterStateCacheContainer clusterStateCacheContainer, IsNSNumber endpoint, IsNSObject queue) => clusterStateCacheContainer -> endpoint -> queue -> Ptr () -> IO ()
readAttributeDeviceTypeListWithClusterStateCache_endpoint_queue_completion clusterStateCacheContainer endpoint queue completion =
  do
    cls' <- getRequiredClass "MTRBaseClusterDescriptor"
    sendClassMessage cls' readAttributeDeviceTypeListWithClusterStateCache_endpoint_queue_completionSelector (toMTRClusterStateCacheContainer clusterStateCacheContainer) (toNSNumber endpoint) (toNSObject queue) completion

-- | @- readAttributeServerListWithCompletion:@
readAttributeServerListWithCompletion :: IsMTRBaseClusterDescriptor mtrBaseClusterDescriptor => mtrBaseClusterDescriptor -> Ptr () -> IO ()
readAttributeServerListWithCompletion mtrBaseClusterDescriptor completion =
  sendMessage mtrBaseClusterDescriptor readAttributeServerListWithCompletionSelector completion

-- | @- subscribeAttributeServerListWithParams:subscriptionEstablished:reportHandler:@
subscribeAttributeServerListWithParams_subscriptionEstablished_reportHandler :: (IsMTRBaseClusterDescriptor mtrBaseClusterDescriptor, IsMTRSubscribeParams params) => mtrBaseClusterDescriptor -> params -> Ptr () -> Ptr () -> IO ()
subscribeAttributeServerListWithParams_subscriptionEstablished_reportHandler mtrBaseClusterDescriptor params subscriptionEstablished reportHandler =
  sendMessage mtrBaseClusterDescriptor subscribeAttributeServerListWithParams_subscriptionEstablished_reportHandlerSelector (toMTRSubscribeParams params) subscriptionEstablished reportHandler

-- | @+ readAttributeServerListWithClusterStateCache:endpoint:queue:completion:@
readAttributeServerListWithClusterStateCache_endpoint_queue_completion :: (IsMTRClusterStateCacheContainer clusterStateCacheContainer, IsNSNumber endpoint, IsNSObject queue) => clusterStateCacheContainer -> endpoint -> queue -> Ptr () -> IO ()
readAttributeServerListWithClusterStateCache_endpoint_queue_completion clusterStateCacheContainer endpoint queue completion =
  do
    cls' <- getRequiredClass "MTRBaseClusterDescriptor"
    sendClassMessage cls' readAttributeServerListWithClusterStateCache_endpoint_queue_completionSelector (toMTRClusterStateCacheContainer clusterStateCacheContainer) (toNSNumber endpoint) (toNSObject queue) completion

-- | @- readAttributeClientListWithCompletion:@
readAttributeClientListWithCompletion :: IsMTRBaseClusterDescriptor mtrBaseClusterDescriptor => mtrBaseClusterDescriptor -> Ptr () -> IO ()
readAttributeClientListWithCompletion mtrBaseClusterDescriptor completion =
  sendMessage mtrBaseClusterDescriptor readAttributeClientListWithCompletionSelector completion

-- | @- subscribeAttributeClientListWithParams:subscriptionEstablished:reportHandler:@
subscribeAttributeClientListWithParams_subscriptionEstablished_reportHandler :: (IsMTRBaseClusterDescriptor mtrBaseClusterDescriptor, IsMTRSubscribeParams params) => mtrBaseClusterDescriptor -> params -> Ptr () -> Ptr () -> IO ()
subscribeAttributeClientListWithParams_subscriptionEstablished_reportHandler mtrBaseClusterDescriptor params subscriptionEstablished reportHandler =
  sendMessage mtrBaseClusterDescriptor subscribeAttributeClientListWithParams_subscriptionEstablished_reportHandlerSelector (toMTRSubscribeParams params) subscriptionEstablished reportHandler

-- | @+ readAttributeClientListWithClusterStateCache:endpoint:queue:completion:@
readAttributeClientListWithClusterStateCache_endpoint_queue_completion :: (IsMTRClusterStateCacheContainer clusterStateCacheContainer, IsNSNumber endpoint, IsNSObject queue) => clusterStateCacheContainer -> endpoint -> queue -> Ptr () -> IO ()
readAttributeClientListWithClusterStateCache_endpoint_queue_completion clusterStateCacheContainer endpoint queue completion =
  do
    cls' <- getRequiredClass "MTRBaseClusterDescriptor"
    sendClassMessage cls' readAttributeClientListWithClusterStateCache_endpoint_queue_completionSelector (toMTRClusterStateCacheContainer clusterStateCacheContainer) (toNSNumber endpoint) (toNSObject queue) completion

-- | @- readAttributePartsListWithCompletion:@
readAttributePartsListWithCompletion :: IsMTRBaseClusterDescriptor mtrBaseClusterDescriptor => mtrBaseClusterDescriptor -> Ptr () -> IO ()
readAttributePartsListWithCompletion mtrBaseClusterDescriptor completion =
  sendMessage mtrBaseClusterDescriptor readAttributePartsListWithCompletionSelector completion

-- | @- subscribeAttributePartsListWithParams:subscriptionEstablished:reportHandler:@
subscribeAttributePartsListWithParams_subscriptionEstablished_reportHandler :: (IsMTRBaseClusterDescriptor mtrBaseClusterDescriptor, IsMTRSubscribeParams params) => mtrBaseClusterDescriptor -> params -> Ptr () -> Ptr () -> IO ()
subscribeAttributePartsListWithParams_subscriptionEstablished_reportHandler mtrBaseClusterDescriptor params subscriptionEstablished reportHandler =
  sendMessage mtrBaseClusterDescriptor subscribeAttributePartsListWithParams_subscriptionEstablished_reportHandlerSelector (toMTRSubscribeParams params) subscriptionEstablished reportHandler

-- | @+ readAttributePartsListWithClusterStateCache:endpoint:queue:completion:@
readAttributePartsListWithClusterStateCache_endpoint_queue_completion :: (IsMTRClusterStateCacheContainer clusterStateCacheContainer, IsNSNumber endpoint, IsNSObject queue) => clusterStateCacheContainer -> endpoint -> queue -> Ptr () -> IO ()
readAttributePartsListWithClusterStateCache_endpoint_queue_completion clusterStateCacheContainer endpoint queue completion =
  do
    cls' <- getRequiredClass "MTRBaseClusterDescriptor"
    sendClassMessage cls' readAttributePartsListWithClusterStateCache_endpoint_queue_completionSelector (toMTRClusterStateCacheContainer clusterStateCacheContainer) (toNSNumber endpoint) (toNSObject queue) completion

-- | @- readAttributeTagListWithCompletion:@
readAttributeTagListWithCompletion :: IsMTRBaseClusterDescriptor mtrBaseClusterDescriptor => mtrBaseClusterDescriptor -> Ptr () -> IO ()
readAttributeTagListWithCompletion mtrBaseClusterDescriptor completion =
  sendMessage mtrBaseClusterDescriptor readAttributeTagListWithCompletionSelector completion

-- | @- subscribeAttributeTagListWithParams:subscriptionEstablished:reportHandler:@
subscribeAttributeTagListWithParams_subscriptionEstablished_reportHandler :: (IsMTRBaseClusterDescriptor mtrBaseClusterDescriptor, IsMTRSubscribeParams params) => mtrBaseClusterDescriptor -> params -> Ptr () -> Ptr () -> IO ()
subscribeAttributeTagListWithParams_subscriptionEstablished_reportHandler mtrBaseClusterDescriptor params subscriptionEstablished reportHandler =
  sendMessage mtrBaseClusterDescriptor subscribeAttributeTagListWithParams_subscriptionEstablished_reportHandlerSelector (toMTRSubscribeParams params) subscriptionEstablished reportHandler

-- | @+ readAttributeTagListWithClusterStateCache:endpoint:queue:completion:@
readAttributeTagListWithClusterStateCache_endpoint_queue_completion :: (IsMTRClusterStateCacheContainer clusterStateCacheContainer, IsNSNumber endpoint, IsNSObject queue) => clusterStateCacheContainer -> endpoint -> queue -> Ptr () -> IO ()
readAttributeTagListWithClusterStateCache_endpoint_queue_completion clusterStateCacheContainer endpoint queue completion =
  do
    cls' <- getRequiredClass "MTRBaseClusterDescriptor"
    sendClassMessage cls' readAttributeTagListWithClusterStateCache_endpoint_queue_completionSelector (toMTRClusterStateCacheContainer clusterStateCacheContainer) (toNSNumber endpoint) (toNSObject queue) completion

-- | @- readAttributeEndpointUniqueIDWithCompletion:@
readAttributeEndpointUniqueIDWithCompletion :: IsMTRBaseClusterDescriptor mtrBaseClusterDescriptor => mtrBaseClusterDescriptor -> Ptr () -> IO ()
readAttributeEndpointUniqueIDWithCompletion mtrBaseClusterDescriptor completion =
  sendMessage mtrBaseClusterDescriptor readAttributeEndpointUniqueIDWithCompletionSelector completion

-- | @- subscribeAttributeEndpointUniqueIDWithParams:subscriptionEstablished:reportHandler:@
subscribeAttributeEndpointUniqueIDWithParams_subscriptionEstablished_reportHandler :: (IsMTRBaseClusterDescriptor mtrBaseClusterDescriptor, IsMTRSubscribeParams params) => mtrBaseClusterDescriptor -> params -> Ptr () -> Ptr () -> IO ()
subscribeAttributeEndpointUniqueIDWithParams_subscriptionEstablished_reportHandler mtrBaseClusterDescriptor params subscriptionEstablished reportHandler =
  sendMessage mtrBaseClusterDescriptor subscribeAttributeEndpointUniqueIDWithParams_subscriptionEstablished_reportHandlerSelector (toMTRSubscribeParams params) subscriptionEstablished reportHandler

-- | @+ readAttributeEndpointUniqueIDWithClusterStateCache:endpoint:queue:completion:@
readAttributeEndpointUniqueIDWithClusterStateCache_endpoint_queue_completion :: (IsMTRClusterStateCacheContainer clusterStateCacheContainer, IsNSNumber endpoint, IsNSObject queue) => clusterStateCacheContainer -> endpoint -> queue -> Ptr () -> IO ()
readAttributeEndpointUniqueIDWithClusterStateCache_endpoint_queue_completion clusterStateCacheContainer endpoint queue completion =
  do
    cls' <- getRequiredClass "MTRBaseClusterDescriptor"
    sendClassMessage cls' readAttributeEndpointUniqueIDWithClusterStateCache_endpoint_queue_completionSelector (toMTRClusterStateCacheContainer clusterStateCacheContainer) (toNSNumber endpoint) (toNSObject queue) completion

-- | @- readAttributeGeneratedCommandListWithCompletion:@
readAttributeGeneratedCommandListWithCompletion :: IsMTRBaseClusterDescriptor mtrBaseClusterDescriptor => mtrBaseClusterDescriptor -> Ptr () -> IO ()
readAttributeGeneratedCommandListWithCompletion mtrBaseClusterDescriptor completion =
  sendMessage mtrBaseClusterDescriptor readAttributeGeneratedCommandListWithCompletionSelector completion

-- | @- subscribeAttributeGeneratedCommandListWithParams:subscriptionEstablished:reportHandler:@
subscribeAttributeGeneratedCommandListWithParams_subscriptionEstablished_reportHandler :: (IsMTRBaseClusterDescriptor mtrBaseClusterDescriptor, IsMTRSubscribeParams params) => mtrBaseClusterDescriptor -> params -> Ptr () -> Ptr () -> IO ()
subscribeAttributeGeneratedCommandListWithParams_subscriptionEstablished_reportHandler mtrBaseClusterDescriptor params subscriptionEstablished reportHandler =
  sendMessage mtrBaseClusterDescriptor subscribeAttributeGeneratedCommandListWithParams_subscriptionEstablished_reportHandlerSelector (toMTRSubscribeParams params) subscriptionEstablished reportHandler

-- | @+ readAttributeGeneratedCommandListWithClusterStateCache:endpoint:queue:completion:@
readAttributeGeneratedCommandListWithClusterStateCache_endpoint_queue_completion :: (IsMTRClusterStateCacheContainer clusterStateCacheContainer, IsNSNumber endpoint, IsNSObject queue) => clusterStateCacheContainer -> endpoint -> queue -> Ptr () -> IO ()
readAttributeGeneratedCommandListWithClusterStateCache_endpoint_queue_completion clusterStateCacheContainer endpoint queue completion =
  do
    cls' <- getRequiredClass "MTRBaseClusterDescriptor"
    sendClassMessage cls' readAttributeGeneratedCommandListWithClusterStateCache_endpoint_queue_completionSelector (toMTRClusterStateCacheContainer clusterStateCacheContainer) (toNSNumber endpoint) (toNSObject queue) completion

-- | @- readAttributeAcceptedCommandListWithCompletion:@
readAttributeAcceptedCommandListWithCompletion :: IsMTRBaseClusterDescriptor mtrBaseClusterDescriptor => mtrBaseClusterDescriptor -> Ptr () -> IO ()
readAttributeAcceptedCommandListWithCompletion mtrBaseClusterDescriptor completion =
  sendMessage mtrBaseClusterDescriptor readAttributeAcceptedCommandListWithCompletionSelector completion

-- | @- subscribeAttributeAcceptedCommandListWithParams:subscriptionEstablished:reportHandler:@
subscribeAttributeAcceptedCommandListWithParams_subscriptionEstablished_reportHandler :: (IsMTRBaseClusterDescriptor mtrBaseClusterDescriptor, IsMTRSubscribeParams params) => mtrBaseClusterDescriptor -> params -> Ptr () -> Ptr () -> IO ()
subscribeAttributeAcceptedCommandListWithParams_subscriptionEstablished_reportHandler mtrBaseClusterDescriptor params subscriptionEstablished reportHandler =
  sendMessage mtrBaseClusterDescriptor subscribeAttributeAcceptedCommandListWithParams_subscriptionEstablished_reportHandlerSelector (toMTRSubscribeParams params) subscriptionEstablished reportHandler

-- | @+ readAttributeAcceptedCommandListWithClusterStateCache:endpoint:queue:completion:@
readAttributeAcceptedCommandListWithClusterStateCache_endpoint_queue_completion :: (IsMTRClusterStateCacheContainer clusterStateCacheContainer, IsNSNumber endpoint, IsNSObject queue) => clusterStateCacheContainer -> endpoint -> queue -> Ptr () -> IO ()
readAttributeAcceptedCommandListWithClusterStateCache_endpoint_queue_completion clusterStateCacheContainer endpoint queue completion =
  do
    cls' <- getRequiredClass "MTRBaseClusterDescriptor"
    sendClassMessage cls' readAttributeAcceptedCommandListWithClusterStateCache_endpoint_queue_completionSelector (toMTRClusterStateCacheContainer clusterStateCacheContainer) (toNSNumber endpoint) (toNSObject queue) completion

-- | @- readAttributeAttributeListWithCompletion:@
readAttributeAttributeListWithCompletion :: IsMTRBaseClusterDescriptor mtrBaseClusterDescriptor => mtrBaseClusterDescriptor -> Ptr () -> IO ()
readAttributeAttributeListWithCompletion mtrBaseClusterDescriptor completion =
  sendMessage mtrBaseClusterDescriptor readAttributeAttributeListWithCompletionSelector completion

-- | @- subscribeAttributeAttributeListWithParams:subscriptionEstablished:reportHandler:@
subscribeAttributeAttributeListWithParams_subscriptionEstablished_reportHandler :: (IsMTRBaseClusterDescriptor mtrBaseClusterDescriptor, IsMTRSubscribeParams params) => mtrBaseClusterDescriptor -> params -> Ptr () -> Ptr () -> IO ()
subscribeAttributeAttributeListWithParams_subscriptionEstablished_reportHandler mtrBaseClusterDescriptor params subscriptionEstablished reportHandler =
  sendMessage mtrBaseClusterDescriptor subscribeAttributeAttributeListWithParams_subscriptionEstablished_reportHandlerSelector (toMTRSubscribeParams params) subscriptionEstablished reportHandler

-- | @+ readAttributeAttributeListWithClusterStateCache:endpoint:queue:completion:@
readAttributeAttributeListWithClusterStateCache_endpoint_queue_completion :: (IsMTRClusterStateCacheContainer clusterStateCacheContainer, IsNSNumber endpoint, IsNSObject queue) => clusterStateCacheContainer -> endpoint -> queue -> Ptr () -> IO ()
readAttributeAttributeListWithClusterStateCache_endpoint_queue_completion clusterStateCacheContainer endpoint queue completion =
  do
    cls' <- getRequiredClass "MTRBaseClusterDescriptor"
    sendClassMessage cls' readAttributeAttributeListWithClusterStateCache_endpoint_queue_completionSelector (toMTRClusterStateCacheContainer clusterStateCacheContainer) (toNSNumber endpoint) (toNSObject queue) completion

-- | @- readAttributeFeatureMapWithCompletion:@
readAttributeFeatureMapWithCompletion :: IsMTRBaseClusterDescriptor mtrBaseClusterDescriptor => mtrBaseClusterDescriptor -> Ptr () -> IO ()
readAttributeFeatureMapWithCompletion mtrBaseClusterDescriptor completion =
  sendMessage mtrBaseClusterDescriptor readAttributeFeatureMapWithCompletionSelector completion

-- | @- subscribeAttributeFeatureMapWithParams:subscriptionEstablished:reportHandler:@
subscribeAttributeFeatureMapWithParams_subscriptionEstablished_reportHandler :: (IsMTRBaseClusterDescriptor mtrBaseClusterDescriptor, IsMTRSubscribeParams params) => mtrBaseClusterDescriptor -> params -> Ptr () -> Ptr () -> IO ()
subscribeAttributeFeatureMapWithParams_subscriptionEstablished_reportHandler mtrBaseClusterDescriptor params subscriptionEstablished reportHandler =
  sendMessage mtrBaseClusterDescriptor subscribeAttributeFeatureMapWithParams_subscriptionEstablished_reportHandlerSelector (toMTRSubscribeParams params) subscriptionEstablished reportHandler

-- | @+ readAttributeFeatureMapWithClusterStateCache:endpoint:queue:completion:@
readAttributeFeatureMapWithClusterStateCache_endpoint_queue_completion :: (IsMTRClusterStateCacheContainer clusterStateCacheContainer, IsNSNumber endpoint, IsNSObject queue) => clusterStateCacheContainer -> endpoint -> queue -> Ptr () -> IO ()
readAttributeFeatureMapWithClusterStateCache_endpoint_queue_completion clusterStateCacheContainer endpoint queue completion =
  do
    cls' <- getRequiredClass "MTRBaseClusterDescriptor"
    sendClassMessage cls' readAttributeFeatureMapWithClusterStateCache_endpoint_queue_completionSelector (toMTRClusterStateCacheContainer clusterStateCacheContainer) (toNSNumber endpoint) (toNSObject queue) completion

-- | @- readAttributeClusterRevisionWithCompletion:@
readAttributeClusterRevisionWithCompletion :: IsMTRBaseClusterDescriptor mtrBaseClusterDescriptor => mtrBaseClusterDescriptor -> Ptr () -> IO ()
readAttributeClusterRevisionWithCompletion mtrBaseClusterDescriptor completion =
  sendMessage mtrBaseClusterDescriptor readAttributeClusterRevisionWithCompletionSelector completion

-- | @- subscribeAttributeClusterRevisionWithParams:subscriptionEstablished:reportHandler:@
subscribeAttributeClusterRevisionWithParams_subscriptionEstablished_reportHandler :: (IsMTRBaseClusterDescriptor mtrBaseClusterDescriptor, IsMTRSubscribeParams params) => mtrBaseClusterDescriptor -> params -> Ptr () -> Ptr () -> IO ()
subscribeAttributeClusterRevisionWithParams_subscriptionEstablished_reportHandler mtrBaseClusterDescriptor params subscriptionEstablished reportHandler =
  sendMessage mtrBaseClusterDescriptor subscribeAttributeClusterRevisionWithParams_subscriptionEstablished_reportHandlerSelector (toMTRSubscribeParams params) subscriptionEstablished reportHandler

-- | @+ readAttributeClusterRevisionWithClusterStateCache:endpoint:queue:completion:@
readAttributeClusterRevisionWithClusterStateCache_endpoint_queue_completion :: (IsMTRClusterStateCacheContainer clusterStateCacheContainer, IsNSNumber endpoint, IsNSObject queue) => clusterStateCacheContainer -> endpoint -> queue -> Ptr () -> IO ()
readAttributeClusterRevisionWithClusterStateCache_endpoint_queue_completion clusterStateCacheContainer endpoint queue completion =
  do
    cls' <- getRequiredClass "MTRBaseClusterDescriptor"
    sendClassMessage cls' readAttributeClusterRevisionWithClusterStateCache_endpoint_queue_completionSelector (toMTRClusterStateCacheContainer clusterStateCacheContainer) (toNSNumber endpoint) (toNSObject queue) completion

-- | @- init@
init_ :: IsMTRBaseClusterDescriptor mtrBaseClusterDescriptor => mtrBaseClusterDescriptor -> IO (Id MTRBaseClusterDescriptor)
init_ mtrBaseClusterDescriptor =
  sendOwnedMessage mtrBaseClusterDescriptor initSelector

-- | @+ new@
new :: IO (Id MTRBaseClusterDescriptor)
new  =
  do
    cls' <- getRequiredClass "MTRBaseClusterDescriptor"
    sendOwnedClassMessage cls' newSelector

-- | @- initWithDevice:endpoint:queue:@
initWithDevice_endpoint_queue :: (IsMTRBaseClusterDescriptor mtrBaseClusterDescriptor, IsMTRBaseDevice device, IsNSObject queue) => mtrBaseClusterDescriptor -> device -> CUShort -> queue -> IO (Id MTRBaseClusterDescriptor)
initWithDevice_endpoint_queue mtrBaseClusterDescriptor device endpoint queue =
  sendOwnedMessage mtrBaseClusterDescriptor initWithDevice_endpoint_queueSelector (toMTRBaseDevice device) endpoint (toNSObject queue)

-- | @- readAttributeDeviceListWithCompletionHandler:@
readAttributeDeviceListWithCompletionHandler :: IsMTRBaseClusterDescriptor mtrBaseClusterDescriptor => mtrBaseClusterDescriptor -> Ptr () -> IO ()
readAttributeDeviceListWithCompletionHandler mtrBaseClusterDescriptor completionHandler =
  sendMessage mtrBaseClusterDescriptor readAttributeDeviceListWithCompletionHandlerSelector completionHandler

-- | @- subscribeAttributeDeviceListWithMinInterval:maxInterval:params:subscriptionEstablished:reportHandler:@
subscribeAttributeDeviceListWithMinInterval_maxInterval_params_subscriptionEstablished_reportHandler :: (IsMTRBaseClusterDescriptor mtrBaseClusterDescriptor, IsNSNumber minInterval, IsNSNumber maxInterval, IsMTRSubscribeParams params) => mtrBaseClusterDescriptor -> minInterval -> maxInterval -> params -> Ptr () -> Ptr () -> IO ()
subscribeAttributeDeviceListWithMinInterval_maxInterval_params_subscriptionEstablished_reportHandler mtrBaseClusterDescriptor minInterval maxInterval params subscriptionEstablishedHandler reportHandler =
  sendMessage mtrBaseClusterDescriptor subscribeAttributeDeviceListWithMinInterval_maxInterval_params_subscriptionEstablished_reportHandlerSelector (toNSNumber minInterval) (toNSNumber maxInterval) (toMTRSubscribeParams params) subscriptionEstablishedHandler reportHandler

-- | @+ readAttributeDeviceListWithAttributeCache:endpoint:queue:completionHandler:@
readAttributeDeviceListWithAttributeCache_endpoint_queue_completionHandler :: (IsMTRAttributeCacheContainer attributeCacheContainer, IsNSNumber endpoint, IsNSObject queue) => attributeCacheContainer -> endpoint -> queue -> Ptr () -> IO ()
readAttributeDeviceListWithAttributeCache_endpoint_queue_completionHandler attributeCacheContainer endpoint queue completionHandler =
  do
    cls' <- getRequiredClass "MTRBaseClusterDescriptor"
    sendClassMessage cls' readAttributeDeviceListWithAttributeCache_endpoint_queue_completionHandlerSelector (toMTRAttributeCacheContainer attributeCacheContainer) (toNSNumber endpoint) (toNSObject queue) completionHandler

-- | @- readAttributeServerListWithCompletionHandler:@
readAttributeServerListWithCompletionHandler :: IsMTRBaseClusterDescriptor mtrBaseClusterDescriptor => mtrBaseClusterDescriptor -> Ptr () -> IO ()
readAttributeServerListWithCompletionHandler mtrBaseClusterDescriptor completionHandler =
  sendMessage mtrBaseClusterDescriptor readAttributeServerListWithCompletionHandlerSelector completionHandler

-- | @- subscribeAttributeServerListWithMinInterval:maxInterval:params:subscriptionEstablished:reportHandler:@
subscribeAttributeServerListWithMinInterval_maxInterval_params_subscriptionEstablished_reportHandler :: (IsMTRBaseClusterDescriptor mtrBaseClusterDescriptor, IsNSNumber minInterval, IsNSNumber maxInterval, IsMTRSubscribeParams params) => mtrBaseClusterDescriptor -> minInterval -> maxInterval -> params -> Ptr () -> Ptr () -> IO ()
subscribeAttributeServerListWithMinInterval_maxInterval_params_subscriptionEstablished_reportHandler mtrBaseClusterDescriptor minInterval maxInterval params subscriptionEstablishedHandler reportHandler =
  sendMessage mtrBaseClusterDescriptor subscribeAttributeServerListWithMinInterval_maxInterval_params_subscriptionEstablished_reportHandlerSelector (toNSNumber minInterval) (toNSNumber maxInterval) (toMTRSubscribeParams params) subscriptionEstablishedHandler reportHandler

-- | @+ readAttributeServerListWithAttributeCache:endpoint:queue:completionHandler:@
readAttributeServerListWithAttributeCache_endpoint_queue_completionHandler :: (IsMTRAttributeCacheContainer attributeCacheContainer, IsNSNumber endpoint, IsNSObject queue) => attributeCacheContainer -> endpoint -> queue -> Ptr () -> IO ()
readAttributeServerListWithAttributeCache_endpoint_queue_completionHandler attributeCacheContainer endpoint queue completionHandler =
  do
    cls' <- getRequiredClass "MTRBaseClusterDescriptor"
    sendClassMessage cls' readAttributeServerListWithAttributeCache_endpoint_queue_completionHandlerSelector (toMTRAttributeCacheContainer attributeCacheContainer) (toNSNumber endpoint) (toNSObject queue) completionHandler

-- | @- readAttributeClientListWithCompletionHandler:@
readAttributeClientListWithCompletionHandler :: IsMTRBaseClusterDescriptor mtrBaseClusterDescriptor => mtrBaseClusterDescriptor -> Ptr () -> IO ()
readAttributeClientListWithCompletionHandler mtrBaseClusterDescriptor completionHandler =
  sendMessage mtrBaseClusterDescriptor readAttributeClientListWithCompletionHandlerSelector completionHandler

-- | @- subscribeAttributeClientListWithMinInterval:maxInterval:params:subscriptionEstablished:reportHandler:@
subscribeAttributeClientListWithMinInterval_maxInterval_params_subscriptionEstablished_reportHandler :: (IsMTRBaseClusterDescriptor mtrBaseClusterDescriptor, IsNSNumber minInterval, IsNSNumber maxInterval, IsMTRSubscribeParams params) => mtrBaseClusterDescriptor -> minInterval -> maxInterval -> params -> Ptr () -> Ptr () -> IO ()
subscribeAttributeClientListWithMinInterval_maxInterval_params_subscriptionEstablished_reportHandler mtrBaseClusterDescriptor minInterval maxInterval params subscriptionEstablishedHandler reportHandler =
  sendMessage mtrBaseClusterDescriptor subscribeAttributeClientListWithMinInterval_maxInterval_params_subscriptionEstablished_reportHandlerSelector (toNSNumber minInterval) (toNSNumber maxInterval) (toMTRSubscribeParams params) subscriptionEstablishedHandler reportHandler

-- | @+ readAttributeClientListWithAttributeCache:endpoint:queue:completionHandler:@
readAttributeClientListWithAttributeCache_endpoint_queue_completionHandler :: (IsMTRAttributeCacheContainer attributeCacheContainer, IsNSNumber endpoint, IsNSObject queue) => attributeCacheContainer -> endpoint -> queue -> Ptr () -> IO ()
readAttributeClientListWithAttributeCache_endpoint_queue_completionHandler attributeCacheContainer endpoint queue completionHandler =
  do
    cls' <- getRequiredClass "MTRBaseClusterDescriptor"
    sendClassMessage cls' readAttributeClientListWithAttributeCache_endpoint_queue_completionHandlerSelector (toMTRAttributeCacheContainer attributeCacheContainer) (toNSNumber endpoint) (toNSObject queue) completionHandler

-- | @- readAttributePartsListWithCompletionHandler:@
readAttributePartsListWithCompletionHandler :: IsMTRBaseClusterDescriptor mtrBaseClusterDescriptor => mtrBaseClusterDescriptor -> Ptr () -> IO ()
readAttributePartsListWithCompletionHandler mtrBaseClusterDescriptor completionHandler =
  sendMessage mtrBaseClusterDescriptor readAttributePartsListWithCompletionHandlerSelector completionHandler

-- | @- subscribeAttributePartsListWithMinInterval:maxInterval:params:subscriptionEstablished:reportHandler:@
subscribeAttributePartsListWithMinInterval_maxInterval_params_subscriptionEstablished_reportHandler :: (IsMTRBaseClusterDescriptor mtrBaseClusterDescriptor, IsNSNumber minInterval, IsNSNumber maxInterval, IsMTRSubscribeParams params) => mtrBaseClusterDescriptor -> minInterval -> maxInterval -> params -> Ptr () -> Ptr () -> IO ()
subscribeAttributePartsListWithMinInterval_maxInterval_params_subscriptionEstablished_reportHandler mtrBaseClusterDescriptor minInterval maxInterval params subscriptionEstablishedHandler reportHandler =
  sendMessage mtrBaseClusterDescriptor subscribeAttributePartsListWithMinInterval_maxInterval_params_subscriptionEstablished_reportHandlerSelector (toNSNumber minInterval) (toNSNumber maxInterval) (toMTRSubscribeParams params) subscriptionEstablishedHandler reportHandler

-- | @+ readAttributePartsListWithAttributeCache:endpoint:queue:completionHandler:@
readAttributePartsListWithAttributeCache_endpoint_queue_completionHandler :: (IsMTRAttributeCacheContainer attributeCacheContainer, IsNSNumber endpoint, IsNSObject queue) => attributeCacheContainer -> endpoint -> queue -> Ptr () -> IO ()
readAttributePartsListWithAttributeCache_endpoint_queue_completionHandler attributeCacheContainer endpoint queue completionHandler =
  do
    cls' <- getRequiredClass "MTRBaseClusterDescriptor"
    sendClassMessage cls' readAttributePartsListWithAttributeCache_endpoint_queue_completionHandlerSelector (toMTRAttributeCacheContainer attributeCacheContainer) (toNSNumber endpoint) (toNSObject queue) completionHandler

-- | @- readAttributeGeneratedCommandListWithCompletionHandler:@
readAttributeGeneratedCommandListWithCompletionHandler :: IsMTRBaseClusterDescriptor mtrBaseClusterDescriptor => mtrBaseClusterDescriptor -> Ptr () -> IO ()
readAttributeGeneratedCommandListWithCompletionHandler mtrBaseClusterDescriptor completionHandler =
  sendMessage mtrBaseClusterDescriptor readAttributeGeneratedCommandListWithCompletionHandlerSelector completionHandler

-- | @- subscribeAttributeGeneratedCommandListWithMinInterval:maxInterval:params:subscriptionEstablished:reportHandler:@
subscribeAttributeGeneratedCommandListWithMinInterval_maxInterval_params_subscriptionEstablished_reportHandler :: (IsMTRBaseClusterDescriptor mtrBaseClusterDescriptor, IsNSNumber minInterval, IsNSNumber maxInterval, IsMTRSubscribeParams params) => mtrBaseClusterDescriptor -> minInterval -> maxInterval -> params -> Ptr () -> Ptr () -> IO ()
subscribeAttributeGeneratedCommandListWithMinInterval_maxInterval_params_subscriptionEstablished_reportHandler mtrBaseClusterDescriptor minInterval maxInterval params subscriptionEstablishedHandler reportHandler =
  sendMessage mtrBaseClusterDescriptor subscribeAttributeGeneratedCommandListWithMinInterval_maxInterval_params_subscriptionEstablished_reportHandlerSelector (toNSNumber minInterval) (toNSNumber maxInterval) (toMTRSubscribeParams params) subscriptionEstablishedHandler reportHandler

-- | @+ readAttributeGeneratedCommandListWithAttributeCache:endpoint:queue:completionHandler:@
readAttributeGeneratedCommandListWithAttributeCache_endpoint_queue_completionHandler :: (IsMTRAttributeCacheContainer attributeCacheContainer, IsNSNumber endpoint, IsNSObject queue) => attributeCacheContainer -> endpoint -> queue -> Ptr () -> IO ()
readAttributeGeneratedCommandListWithAttributeCache_endpoint_queue_completionHandler attributeCacheContainer endpoint queue completionHandler =
  do
    cls' <- getRequiredClass "MTRBaseClusterDescriptor"
    sendClassMessage cls' readAttributeGeneratedCommandListWithAttributeCache_endpoint_queue_completionHandlerSelector (toMTRAttributeCacheContainer attributeCacheContainer) (toNSNumber endpoint) (toNSObject queue) completionHandler

-- | @- readAttributeAcceptedCommandListWithCompletionHandler:@
readAttributeAcceptedCommandListWithCompletionHandler :: IsMTRBaseClusterDescriptor mtrBaseClusterDescriptor => mtrBaseClusterDescriptor -> Ptr () -> IO ()
readAttributeAcceptedCommandListWithCompletionHandler mtrBaseClusterDescriptor completionHandler =
  sendMessage mtrBaseClusterDescriptor readAttributeAcceptedCommandListWithCompletionHandlerSelector completionHandler

-- | @- subscribeAttributeAcceptedCommandListWithMinInterval:maxInterval:params:subscriptionEstablished:reportHandler:@
subscribeAttributeAcceptedCommandListWithMinInterval_maxInterval_params_subscriptionEstablished_reportHandler :: (IsMTRBaseClusterDescriptor mtrBaseClusterDescriptor, IsNSNumber minInterval, IsNSNumber maxInterval, IsMTRSubscribeParams params) => mtrBaseClusterDescriptor -> minInterval -> maxInterval -> params -> Ptr () -> Ptr () -> IO ()
subscribeAttributeAcceptedCommandListWithMinInterval_maxInterval_params_subscriptionEstablished_reportHandler mtrBaseClusterDescriptor minInterval maxInterval params subscriptionEstablishedHandler reportHandler =
  sendMessage mtrBaseClusterDescriptor subscribeAttributeAcceptedCommandListWithMinInterval_maxInterval_params_subscriptionEstablished_reportHandlerSelector (toNSNumber minInterval) (toNSNumber maxInterval) (toMTRSubscribeParams params) subscriptionEstablishedHandler reportHandler

-- | @+ readAttributeAcceptedCommandListWithAttributeCache:endpoint:queue:completionHandler:@
readAttributeAcceptedCommandListWithAttributeCache_endpoint_queue_completionHandler :: (IsMTRAttributeCacheContainer attributeCacheContainer, IsNSNumber endpoint, IsNSObject queue) => attributeCacheContainer -> endpoint -> queue -> Ptr () -> IO ()
readAttributeAcceptedCommandListWithAttributeCache_endpoint_queue_completionHandler attributeCacheContainer endpoint queue completionHandler =
  do
    cls' <- getRequiredClass "MTRBaseClusterDescriptor"
    sendClassMessage cls' readAttributeAcceptedCommandListWithAttributeCache_endpoint_queue_completionHandlerSelector (toMTRAttributeCacheContainer attributeCacheContainer) (toNSNumber endpoint) (toNSObject queue) completionHandler

-- | @- readAttributeAttributeListWithCompletionHandler:@
readAttributeAttributeListWithCompletionHandler :: IsMTRBaseClusterDescriptor mtrBaseClusterDescriptor => mtrBaseClusterDescriptor -> Ptr () -> IO ()
readAttributeAttributeListWithCompletionHandler mtrBaseClusterDescriptor completionHandler =
  sendMessage mtrBaseClusterDescriptor readAttributeAttributeListWithCompletionHandlerSelector completionHandler

-- | @- subscribeAttributeAttributeListWithMinInterval:maxInterval:params:subscriptionEstablished:reportHandler:@
subscribeAttributeAttributeListWithMinInterval_maxInterval_params_subscriptionEstablished_reportHandler :: (IsMTRBaseClusterDescriptor mtrBaseClusterDescriptor, IsNSNumber minInterval, IsNSNumber maxInterval, IsMTRSubscribeParams params) => mtrBaseClusterDescriptor -> minInterval -> maxInterval -> params -> Ptr () -> Ptr () -> IO ()
subscribeAttributeAttributeListWithMinInterval_maxInterval_params_subscriptionEstablished_reportHandler mtrBaseClusterDescriptor minInterval maxInterval params subscriptionEstablishedHandler reportHandler =
  sendMessage mtrBaseClusterDescriptor subscribeAttributeAttributeListWithMinInterval_maxInterval_params_subscriptionEstablished_reportHandlerSelector (toNSNumber minInterval) (toNSNumber maxInterval) (toMTRSubscribeParams params) subscriptionEstablishedHandler reportHandler

-- | @+ readAttributeAttributeListWithAttributeCache:endpoint:queue:completionHandler:@
readAttributeAttributeListWithAttributeCache_endpoint_queue_completionHandler :: (IsMTRAttributeCacheContainer attributeCacheContainer, IsNSNumber endpoint, IsNSObject queue) => attributeCacheContainer -> endpoint -> queue -> Ptr () -> IO ()
readAttributeAttributeListWithAttributeCache_endpoint_queue_completionHandler attributeCacheContainer endpoint queue completionHandler =
  do
    cls' <- getRequiredClass "MTRBaseClusterDescriptor"
    sendClassMessage cls' readAttributeAttributeListWithAttributeCache_endpoint_queue_completionHandlerSelector (toMTRAttributeCacheContainer attributeCacheContainer) (toNSNumber endpoint) (toNSObject queue) completionHandler

-- | @- readAttributeFeatureMapWithCompletionHandler:@
readAttributeFeatureMapWithCompletionHandler :: IsMTRBaseClusterDescriptor mtrBaseClusterDescriptor => mtrBaseClusterDescriptor -> Ptr () -> IO ()
readAttributeFeatureMapWithCompletionHandler mtrBaseClusterDescriptor completionHandler =
  sendMessage mtrBaseClusterDescriptor readAttributeFeatureMapWithCompletionHandlerSelector completionHandler

-- | @- subscribeAttributeFeatureMapWithMinInterval:maxInterval:params:subscriptionEstablished:reportHandler:@
subscribeAttributeFeatureMapWithMinInterval_maxInterval_params_subscriptionEstablished_reportHandler :: (IsMTRBaseClusterDescriptor mtrBaseClusterDescriptor, IsNSNumber minInterval, IsNSNumber maxInterval, IsMTRSubscribeParams params) => mtrBaseClusterDescriptor -> minInterval -> maxInterval -> params -> Ptr () -> Ptr () -> IO ()
subscribeAttributeFeatureMapWithMinInterval_maxInterval_params_subscriptionEstablished_reportHandler mtrBaseClusterDescriptor minInterval maxInterval params subscriptionEstablishedHandler reportHandler =
  sendMessage mtrBaseClusterDescriptor subscribeAttributeFeatureMapWithMinInterval_maxInterval_params_subscriptionEstablished_reportHandlerSelector (toNSNumber minInterval) (toNSNumber maxInterval) (toMTRSubscribeParams params) subscriptionEstablishedHandler reportHandler

-- | @+ readAttributeFeatureMapWithAttributeCache:endpoint:queue:completionHandler:@
readAttributeFeatureMapWithAttributeCache_endpoint_queue_completionHandler :: (IsMTRAttributeCacheContainer attributeCacheContainer, IsNSNumber endpoint, IsNSObject queue) => attributeCacheContainer -> endpoint -> queue -> Ptr () -> IO ()
readAttributeFeatureMapWithAttributeCache_endpoint_queue_completionHandler attributeCacheContainer endpoint queue completionHandler =
  do
    cls' <- getRequiredClass "MTRBaseClusterDescriptor"
    sendClassMessage cls' readAttributeFeatureMapWithAttributeCache_endpoint_queue_completionHandlerSelector (toMTRAttributeCacheContainer attributeCacheContainer) (toNSNumber endpoint) (toNSObject queue) completionHandler

-- | @- readAttributeClusterRevisionWithCompletionHandler:@
readAttributeClusterRevisionWithCompletionHandler :: IsMTRBaseClusterDescriptor mtrBaseClusterDescriptor => mtrBaseClusterDescriptor -> Ptr () -> IO ()
readAttributeClusterRevisionWithCompletionHandler mtrBaseClusterDescriptor completionHandler =
  sendMessage mtrBaseClusterDescriptor readAttributeClusterRevisionWithCompletionHandlerSelector completionHandler

-- | @- subscribeAttributeClusterRevisionWithMinInterval:maxInterval:params:subscriptionEstablished:reportHandler:@
subscribeAttributeClusterRevisionWithMinInterval_maxInterval_params_subscriptionEstablished_reportHandler :: (IsMTRBaseClusterDescriptor mtrBaseClusterDescriptor, IsNSNumber minInterval, IsNSNumber maxInterval, IsMTRSubscribeParams params) => mtrBaseClusterDescriptor -> minInterval -> maxInterval -> params -> Ptr () -> Ptr () -> IO ()
subscribeAttributeClusterRevisionWithMinInterval_maxInterval_params_subscriptionEstablished_reportHandler mtrBaseClusterDescriptor minInterval maxInterval params subscriptionEstablishedHandler reportHandler =
  sendMessage mtrBaseClusterDescriptor subscribeAttributeClusterRevisionWithMinInterval_maxInterval_params_subscriptionEstablished_reportHandlerSelector (toNSNumber minInterval) (toNSNumber maxInterval) (toMTRSubscribeParams params) subscriptionEstablishedHandler reportHandler

-- | @+ readAttributeClusterRevisionWithAttributeCache:endpoint:queue:completionHandler:@
readAttributeClusterRevisionWithAttributeCache_endpoint_queue_completionHandler :: (IsMTRAttributeCacheContainer attributeCacheContainer, IsNSNumber endpoint, IsNSObject queue) => attributeCacheContainer -> endpoint -> queue -> Ptr () -> IO ()
readAttributeClusterRevisionWithAttributeCache_endpoint_queue_completionHandler attributeCacheContainer endpoint queue completionHandler =
  do
    cls' <- getRequiredClass "MTRBaseClusterDescriptor"
    sendClassMessage cls' readAttributeClusterRevisionWithAttributeCache_endpoint_queue_completionHandlerSelector (toMTRAttributeCacheContainer attributeCacheContainer) (toNSNumber endpoint) (toNSObject queue) completionHandler

-- | For all instance methods (reads, writes, commands) that take a completion, the completion will be called on the provided queue.
--
-- ObjC selector: @- initWithDevice:endpointID:queue:@
initWithDevice_endpointID_queue :: (IsMTRBaseClusterDescriptor mtrBaseClusterDescriptor, IsMTRBaseDevice device, IsNSNumber endpointID, IsNSObject queue) => mtrBaseClusterDescriptor -> device -> endpointID -> queue -> IO (Id MTRBaseClusterDescriptor)
initWithDevice_endpointID_queue mtrBaseClusterDescriptor device endpointID queue =
  sendOwnedMessage mtrBaseClusterDescriptor initWithDevice_endpointID_queueSelector (toMTRBaseDevice device) (toNSNumber endpointID) (toNSObject queue)

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @readAttributeDeviceTypeListWithCompletion:@
readAttributeDeviceTypeListWithCompletionSelector :: Selector '[Ptr ()] ()
readAttributeDeviceTypeListWithCompletionSelector = mkSelector "readAttributeDeviceTypeListWithCompletion:"

-- | @Selector@ for @subscribeAttributeDeviceTypeListWithParams:subscriptionEstablished:reportHandler:@
subscribeAttributeDeviceTypeListWithParams_subscriptionEstablished_reportHandlerSelector :: Selector '[Id MTRSubscribeParams, Ptr (), Ptr ()] ()
subscribeAttributeDeviceTypeListWithParams_subscriptionEstablished_reportHandlerSelector = mkSelector "subscribeAttributeDeviceTypeListWithParams:subscriptionEstablished:reportHandler:"

-- | @Selector@ for @readAttributeDeviceTypeListWithClusterStateCache:endpoint:queue:completion:@
readAttributeDeviceTypeListWithClusterStateCache_endpoint_queue_completionSelector :: Selector '[Id MTRClusterStateCacheContainer, Id NSNumber, Id NSObject, Ptr ()] ()
readAttributeDeviceTypeListWithClusterStateCache_endpoint_queue_completionSelector = mkSelector "readAttributeDeviceTypeListWithClusterStateCache:endpoint:queue:completion:"

-- | @Selector@ for @readAttributeServerListWithCompletion:@
readAttributeServerListWithCompletionSelector :: Selector '[Ptr ()] ()
readAttributeServerListWithCompletionSelector = mkSelector "readAttributeServerListWithCompletion:"

-- | @Selector@ for @subscribeAttributeServerListWithParams:subscriptionEstablished:reportHandler:@
subscribeAttributeServerListWithParams_subscriptionEstablished_reportHandlerSelector :: Selector '[Id MTRSubscribeParams, Ptr (), Ptr ()] ()
subscribeAttributeServerListWithParams_subscriptionEstablished_reportHandlerSelector = mkSelector "subscribeAttributeServerListWithParams:subscriptionEstablished:reportHandler:"

-- | @Selector@ for @readAttributeServerListWithClusterStateCache:endpoint:queue:completion:@
readAttributeServerListWithClusterStateCache_endpoint_queue_completionSelector :: Selector '[Id MTRClusterStateCacheContainer, Id NSNumber, Id NSObject, Ptr ()] ()
readAttributeServerListWithClusterStateCache_endpoint_queue_completionSelector = mkSelector "readAttributeServerListWithClusterStateCache:endpoint:queue:completion:"

-- | @Selector@ for @readAttributeClientListWithCompletion:@
readAttributeClientListWithCompletionSelector :: Selector '[Ptr ()] ()
readAttributeClientListWithCompletionSelector = mkSelector "readAttributeClientListWithCompletion:"

-- | @Selector@ for @subscribeAttributeClientListWithParams:subscriptionEstablished:reportHandler:@
subscribeAttributeClientListWithParams_subscriptionEstablished_reportHandlerSelector :: Selector '[Id MTRSubscribeParams, Ptr (), Ptr ()] ()
subscribeAttributeClientListWithParams_subscriptionEstablished_reportHandlerSelector = mkSelector "subscribeAttributeClientListWithParams:subscriptionEstablished:reportHandler:"

-- | @Selector@ for @readAttributeClientListWithClusterStateCache:endpoint:queue:completion:@
readAttributeClientListWithClusterStateCache_endpoint_queue_completionSelector :: Selector '[Id MTRClusterStateCacheContainer, Id NSNumber, Id NSObject, Ptr ()] ()
readAttributeClientListWithClusterStateCache_endpoint_queue_completionSelector = mkSelector "readAttributeClientListWithClusterStateCache:endpoint:queue:completion:"

-- | @Selector@ for @readAttributePartsListWithCompletion:@
readAttributePartsListWithCompletionSelector :: Selector '[Ptr ()] ()
readAttributePartsListWithCompletionSelector = mkSelector "readAttributePartsListWithCompletion:"

-- | @Selector@ for @subscribeAttributePartsListWithParams:subscriptionEstablished:reportHandler:@
subscribeAttributePartsListWithParams_subscriptionEstablished_reportHandlerSelector :: Selector '[Id MTRSubscribeParams, Ptr (), Ptr ()] ()
subscribeAttributePartsListWithParams_subscriptionEstablished_reportHandlerSelector = mkSelector "subscribeAttributePartsListWithParams:subscriptionEstablished:reportHandler:"

-- | @Selector@ for @readAttributePartsListWithClusterStateCache:endpoint:queue:completion:@
readAttributePartsListWithClusterStateCache_endpoint_queue_completionSelector :: Selector '[Id MTRClusterStateCacheContainer, Id NSNumber, Id NSObject, Ptr ()] ()
readAttributePartsListWithClusterStateCache_endpoint_queue_completionSelector = mkSelector "readAttributePartsListWithClusterStateCache:endpoint:queue:completion:"

-- | @Selector@ for @readAttributeTagListWithCompletion:@
readAttributeTagListWithCompletionSelector :: Selector '[Ptr ()] ()
readAttributeTagListWithCompletionSelector = mkSelector "readAttributeTagListWithCompletion:"

-- | @Selector@ for @subscribeAttributeTagListWithParams:subscriptionEstablished:reportHandler:@
subscribeAttributeTagListWithParams_subscriptionEstablished_reportHandlerSelector :: Selector '[Id MTRSubscribeParams, Ptr (), Ptr ()] ()
subscribeAttributeTagListWithParams_subscriptionEstablished_reportHandlerSelector = mkSelector "subscribeAttributeTagListWithParams:subscriptionEstablished:reportHandler:"

-- | @Selector@ for @readAttributeTagListWithClusterStateCache:endpoint:queue:completion:@
readAttributeTagListWithClusterStateCache_endpoint_queue_completionSelector :: Selector '[Id MTRClusterStateCacheContainer, Id NSNumber, Id NSObject, Ptr ()] ()
readAttributeTagListWithClusterStateCache_endpoint_queue_completionSelector = mkSelector "readAttributeTagListWithClusterStateCache:endpoint:queue:completion:"

-- | @Selector@ for @readAttributeEndpointUniqueIDWithCompletion:@
readAttributeEndpointUniqueIDWithCompletionSelector :: Selector '[Ptr ()] ()
readAttributeEndpointUniqueIDWithCompletionSelector = mkSelector "readAttributeEndpointUniqueIDWithCompletion:"

-- | @Selector@ for @subscribeAttributeEndpointUniqueIDWithParams:subscriptionEstablished:reportHandler:@
subscribeAttributeEndpointUniqueIDWithParams_subscriptionEstablished_reportHandlerSelector :: Selector '[Id MTRSubscribeParams, Ptr (), Ptr ()] ()
subscribeAttributeEndpointUniqueIDWithParams_subscriptionEstablished_reportHandlerSelector = mkSelector "subscribeAttributeEndpointUniqueIDWithParams:subscriptionEstablished:reportHandler:"

-- | @Selector@ for @readAttributeEndpointUniqueIDWithClusterStateCache:endpoint:queue:completion:@
readAttributeEndpointUniqueIDWithClusterStateCache_endpoint_queue_completionSelector :: Selector '[Id MTRClusterStateCacheContainer, Id NSNumber, Id NSObject, Ptr ()] ()
readAttributeEndpointUniqueIDWithClusterStateCache_endpoint_queue_completionSelector = mkSelector "readAttributeEndpointUniqueIDWithClusterStateCache:endpoint:queue:completion:"

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
initSelector :: Selector '[] (Id MTRBaseClusterDescriptor)
initSelector = mkSelector "init"

-- | @Selector@ for @new@
newSelector :: Selector '[] (Id MTRBaseClusterDescriptor)
newSelector = mkSelector "new"

-- | @Selector@ for @initWithDevice:endpoint:queue:@
initWithDevice_endpoint_queueSelector :: Selector '[Id MTRBaseDevice, CUShort, Id NSObject] (Id MTRBaseClusterDescriptor)
initWithDevice_endpoint_queueSelector = mkSelector "initWithDevice:endpoint:queue:"

-- | @Selector@ for @readAttributeDeviceListWithCompletionHandler:@
readAttributeDeviceListWithCompletionHandlerSelector :: Selector '[Ptr ()] ()
readAttributeDeviceListWithCompletionHandlerSelector = mkSelector "readAttributeDeviceListWithCompletionHandler:"

-- | @Selector@ for @subscribeAttributeDeviceListWithMinInterval:maxInterval:params:subscriptionEstablished:reportHandler:@
subscribeAttributeDeviceListWithMinInterval_maxInterval_params_subscriptionEstablished_reportHandlerSelector :: Selector '[Id NSNumber, Id NSNumber, Id MTRSubscribeParams, Ptr (), Ptr ()] ()
subscribeAttributeDeviceListWithMinInterval_maxInterval_params_subscriptionEstablished_reportHandlerSelector = mkSelector "subscribeAttributeDeviceListWithMinInterval:maxInterval:params:subscriptionEstablished:reportHandler:"

-- | @Selector@ for @readAttributeDeviceListWithAttributeCache:endpoint:queue:completionHandler:@
readAttributeDeviceListWithAttributeCache_endpoint_queue_completionHandlerSelector :: Selector '[Id MTRAttributeCacheContainer, Id NSNumber, Id NSObject, Ptr ()] ()
readAttributeDeviceListWithAttributeCache_endpoint_queue_completionHandlerSelector = mkSelector "readAttributeDeviceListWithAttributeCache:endpoint:queue:completionHandler:"

-- | @Selector@ for @readAttributeServerListWithCompletionHandler:@
readAttributeServerListWithCompletionHandlerSelector :: Selector '[Ptr ()] ()
readAttributeServerListWithCompletionHandlerSelector = mkSelector "readAttributeServerListWithCompletionHandler:"

-- | @Selector@ for @subscribeAttributeServerListWithMinInterval:maxInterval:params:subscriptionEstablished:reportHandler:@
subscribeAttributeServerListWithMinInterval_maxInterval_params_subscriptionEstablished_reportHandlerSelector :: Selector '[Id NSNumber, Id NSNumber, Id MTRSubscribeParams, Ptr (), Ptr ()] ()
subscribeAttributeServerListWithMinInterval_maxInterval_params_subscriptionEstablished_reportHandlerSelector = mkSelector "subscribeAttributeServerListWithMinInterval:maxInterval:params:subscriptionEstablished:reportHandler:"

-- | @Selector@ for @readAttributeServerListWithAttributeCache:endpoint:queue:completionHandler:@
readAttributeServerListWithAttributeCache_endpoint_queue_completionHandlerSelector :: Selector '[Id MTRAttributeCacheContainer, Id NSNumber, Id NSObject, Ptr ()] ()
readAttributeServerListWithAttributeCache_endpoint_queue_completionHandlerSelector = mkSelector "readAttributeServerListWithAttributeCache:endpoint:queue:completionHandler:"

-- | @Selector@ for @readAttributeClientListWithCompletionHandler:@
readAttributeClientListWithCompletionHandlerSelector :: Selector '[Ptr ()] ()
readAttributeClientListWithCompletionHandlerSelector = mkSelector "readAttributeClientListWithCompletionHandler:"

-- | @Selector@ for @subscribeAttributeClientListWithMinInterval:maxInterval:params:subscriptionEstablished:reportHandler:@
subscribeAttributeClientListWithMinInterval_maxInterval_params_subscriptionEstablished_reportHandlerSelector :: Selector '[Id NSNumber, Id NSNumber, Id MTRSubscribeParams, Ptr (), Ptr ()] ()
subscribeAttributeClientListWithMinInterval_maxInterval_params_subscriptionEstablished_reportHandlerSelector = mkSelector "subscribeAttributeClientListWithMinInterval:maxInterval:params:subscriptionEstablished:reportHandler:"

-- | @Selector@ for @readAttributeClientListWithAttributeCache:endpoint:queue:completionHandler:@
readAttributeClientListWithAttributeCache_endpoint_queue_completionHandlerSelector :: Selector '[Id MTRAttributeCacheContainer, Id NSNumber, Id NSObject, Ptr ()] ()
readAttributeClientListWithAttributeCache_endpoint_queue_completionHandlerSelector = mkSelector "readAttributeClientListWithAttributeCache:endpoint:queue:completionHandler:"

-- | @Selector@ for @readAttributePartsListWithCompletionHandler:@
readAttributePartsListWithCompletionHandlerSelector :: Selector '[Ptr ()] ()
readAttributePartsListWithCompletionHandlerSelector = mkSelector "readAttributePartsListWithCompletionHandler:"

-- | @Selector@ for @subscribeAttributePartsListWithMinInterval:maxInterval:params:subscriptionEstablished:reportHandler:@
subscribeAttributePartsListWithMinInterval_maxInterval_params_subscriptionEstablished_reportHandlerSelector :: Selector '[Id NSNumber, Id NSNumber, Id MTRSubscribeParams, Ptr (), Ptr ()] ()
subscribeAttributePartsListWithMinInterval_maxInterval_params_subscriptionEstablished_reportHandlerSelector = mkSelector "subscribeAttributePartsListWithMinInterval:maxInterval:params:subscriptionEstablished:reportHandler:"

-- | @Selector@ for @readAttributePartsListWithAttributeCache:endpoint:queue:completionHandler:@
readAttributePartsListWithAttributeCache_endpoint_queue_completionHandlerSelector :: Selector '[Id MTRAttributeCacheContainer, Id NSNumber, Id NSObject, Ptr ()] ()
readAttributePartsListWithAttributeCache_endpoint_queue_completionHandlerSelector = mkSelector "readAttributePartsListWithAttributeCache:endpoint:queue:completionHandler:"

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
initWithDevice_endpointID_queueSelector :: Selector '[Id MTRBaseDevice, Id NSNumber, Id NSObject] (Id MTRBaseClusterDescriptor)
initWithDevice_endpointID_queueSelector = mkSelector "initWithDevice:endpointID:queue:"

