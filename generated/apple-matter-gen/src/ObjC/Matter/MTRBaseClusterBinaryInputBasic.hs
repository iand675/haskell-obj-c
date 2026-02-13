{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Cluster Binary Input (Basic)
--
-- An interface for reading the value of a binary measurement and accessing various characteristics of that measurement.
--
-- Generated bindings for @MTRBaseClusterBinaryInputBasic@.
module ObjC.Matter.MTRBaseClusterBinaryInputBasic
  ( MTRBaseClusterBinaryInputBasic
  , IsMTRBaseClusterBinaryInputBasic(..)
  , readAttributeActiveTextWithCompletion
  , writeAttributeActiveTextWithValue_completion
  , writeAttributeActiveTextWithValue_params_completion
  , subscribeAttributeActiveTextWithParams_subscriptionEstablished_reportHandler
  , readAttributeActiveTextWithClusterStateCache_endpoint_queue_completion
  , readAttributeDescriptionWithCompletion
  , writeAttributeDescriptionWithValue_completion
  , writeAttributeDescriptionWithValue_params_completion
  , subscribeAttributeDescriptionWithParams_subscriptionEstablished_reportHandler
  , readAttributeDescriptionWithClusterStateCache_endpoint_queue_completion
  , readAttributeInactiveTextWithCompletion
  , writeAttributeInactiveTextWithValue_completion
  , writeAttributeInactiveTextWithValue_params_completion
  , subscribeAttributeInactiveTextWithParams_subscriptionEstablished_reportHandler
  , readAttributeInactiveTextWithClusterStateCache_endpoint_queue_completion
  , readAttributeOutOfServiceWithCompletion
  , writeAttributeOutOfServiceWithValue_completion
  , writeAttributeOutOfServiceWithValue_params_completion
  , subscribeAttributeOutOfServiceWithParams_subscriptionEstablished_reportHandler
  , readAttributeOutOfServiceWithClusterStateCache_endpoint_queue_completion
  , readAttributePolarityWithCompletion
  , subscribeAttributePolarityWithParams_subscriptionEstablished_reportHandler
  , readAttributePolarityWithClusterStateCache_endpoint_queue_completion
  , readAttributePresentValueWithCompletion
  , writeAttributePresentValueWithValue_completion
  , writeAttributePresentValueWithValue_params_completion
  , subscribeAttributePresentValueWithParams_subscriptionEstablished_reportHandler
  , readAttributePresentValueWithClusterStateCache_endpoint_queue_completion
  , readAttributeReliabilityWithCompletion
  , writeAttributeReliabilityWithValue_completion
  , writeAttributeReliabilityWithValue_params_completion
  , subscribeAttributeReliabilityWithParams_subscriptionEstablished_reportHandler
  , readAttributeReliabilityWithClusterStateCache_endpoint_queue_completion
  , readAttributeStatusFlagsWithCompletion
  , subscribeAttributeStatusFlagsWithParams_subscriptionEstablished_reportHandler
  , readAttributeStatusFlagsWithClusterStateCache_endpoint_queue_completion
  , readAttributeApplicationTypeWithCompletion
  , subscribeAttributeApplicationTypeWithParams_subscriptionEstablished_reportHandler
  , readAttributeApplicationTypeWithClusterStateCache_endpoint_queue_completion
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
  , readAttributeActiveTextWithCompletionHandler
  , writeAttributeActiveTextWithValue_completionHandler
  , writeAttributeActiveTextWithValue_params_completionHandler
  , subscribeAttributeActiveTextWithMinInterval_maxInterval_params_subscriptionEstablished_reportHandler
  , readAttributeActiveTextWithAttributeCache_endpoint_queue_completionHandler
  , readAttributeDescriptionWithCompletionHandler
  , writeAttributeDescriptionWithValue_completionHandler
  , writeAttributeDescriptionWithValue_params_completionHandler
  , subscribeAttributeDescriptionWithMinInterval_maxInterval_params_subscriptionEstablished_reportHandler
  , readAttributeDescriptionWithAttributeCache_endpoint_queue_completionHandler
  , readAttributeInactiveTextWithCompletionHandler
  , writeAttributeInactiveTextWithValue_completionHandler
  , writeAttributeInactiveTextWithValue_params_completionHandler
  , subscribeAttributeInactiveTextWithMinInterval_maxInterval_params_subscriptionEstablished_reportHandler
  , readAttributeInactiveTextWithAttributeCache_endpoint_queue_completionHandler
  , readAttributeOutOfServiceWithCompletionHandler
  , writeAttributeOutOfServiceWithValue_completionHandler
  , writeAttributeOutOfServiceWithValue_params_completionHandler
  , subscribeAttributeOutOfServiceWithMinInterval_maxInterval_params_subscriptionEstablished_reportHandler
  , readAttributeOutOfServiceWithAttributeCache_endpoint_queue_completionHandler
  , readAttributePolarityWithCompletionHandler
  , subscribeAttributePolarityWithMinInterval_maxInterval_params_subscriptionEstablished_reportHandler
  , readAttributePolarityWithAttributeCache_endpoint_queue_completionHandler
  , readAttributePresentValueWithCompletionHandler
  , writeAttributePresentValueWithValue_completionHandler
  , writeAttributePresentValueWithValue_params_completionHandler
  , subscribeAttributePresentValueWithMinInterval_maxInterval_params_subscriptionEstablished_reportHandler
  , readAttributePresentValueWithAttributeCache_endpoint_queue_completionHandler
  , readAttributeReliabilityWithCompletionHandler
  , writeAttributeReliabilityWithValue_completionHandler
  , writeAttributeReliabilityWithValue_params_completionHandler
  , subscribeAttributeReliabilityWithMinInterval_maxInterval_params_subscriptionEstablished_reportHandler
  , readAttributeReliabilityWithAttributeCache_endpoint_queue_completionHandler
  , readAttributeStatusFlagsWithCompletionHandler
  , subscribeAttributeStatusFlagsWithMinInterval_maxInterval_params_subscriptionEstablished_reportHandler
  , readAttributeStatusFlagsWithAttributeCache_endpoint_queue_completionHandler
  , readAttributeApplicationTypeWithCompletionHandler
  , subscribeAttributeApplicationTypeWithMinInterval_maxInterval_params_subscriptionEstablished_reportHandler
  , readAttributeApplicationTypeWithAttributeCache_endpoint_queue_completionHandler
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
  , readAttributeActiveTextWithAttributeCache_endpoint_queue_completionHandlerSelector
  , readAttributeActiveTextWithClusterStateCache_endpoint_queue_completionSelector
  , readAttributeActiveTextWithCompletionHandlerSelector
  , readAttributeActiveTextWithCompletionSelector
  , readAttributeApplicationTypeWithAttributeCache_endpoint_queue_completionHandlerSelector
  , readAttributeApplicationTypeWithClusterStateCache_endpoint_queue_completionSelector
  , readAttributeApplicationTypeWithCompletionHandlerSelector
  , readAttributeApplicationTypeWithCompletionSelector
  , readAttributeAttributeListWithAttributeCache_endpoint_queue_completionHandlerSelector
  , readAttributeAttributeListWithClusterStateCache_endpoint_queue_completionSelector
  , readAttributeAttributeListWithCompletionHandlerSelector
  , readAttributeAttributeListWithCompletionSelector
  , readAttributeClusterRevisionWithAttributeCache_endpoint_queue_completionHandlerSelector
  , readAttributeClusterRevisionWithClusterStateCache_endpoint_queue_completionSelector
  , readAttributeClusterRevisionWithCompletionHandlerSelector
  , readAttributeClusterRevisionWithCompletionSelector
  , readAttributeDescriptionWithAttributeCache_endpoint_queue_completionHandlerSelector
  , readAttributeDescriptionWithClusterStateCache_endpoint_queue_completionSelector
  , readAttributeDescriptionWithCompletionHandlerSelector
  , readAttributeDescriptionWithCompletionSelector
  , readAttributeFeatureMapWithAttributeCache_endpoint_queue_completionHandlerSelector
  , readAttributeFeatureMapWithClusterStateCache_endpoint_queue_completionSelector
  , readAttributeFeatureMapWithCompletionHandlerSelector
  , readAttributeFeatureMapWithCompletionSelector
  , readAttributeGeneratedCommandListWithAttributeCache_endpoint_queue_completionHandlerSelector
  , readAttributeGeneratedCommandListWithClusterStateCache_endpoint_queue_completionSelector
  , readAttributeGeneratedCommandListWithCompletionHandlerSelector
  , readAttributeGeneratedCommandListWithCompletionSelector
  , readAttributeInactiveTextWithAttributeCache_endpoint_queue_completionHandlerSelector
  , readAttributeInactiveTextWithClusterStateCache_endpoint_queue_completionSelector
  , readAttributeInactiveTextWithCompletionHandlerSelector
  , readAttributeInactiveTextWithCompletionSelector
  , readAttributeOutOfServiceWithAttributeCache_endpoint_queue_completionHandlerSelector
  , readAttributeOutOfServiceWithClusterStateCache_endpoint_queue_completionSelector
  , readAttributeOutOfServiceWithCompletionHandlerSelector
  , readAttributeOutOfServiceWithCompletionSelector
  , readAttributePolarityWithAttributeCache_endpoint_queue_completionHandlerSelector
  , readAttributePolarityWithClusterStateCache_endpoint_queue_completionSelector
  , readAttributePolarityWithCompletionHandlerSelector
  , readAttributePolarityWithCompletionSelector
  , readAttributePresentValueWithAttributeCache_endpoint_queue_completionHandlerSelector
  , readAttributePresentValueWithClusterStateCache_endpoint_queue_completionSelector
  , readAttributePresentValueWithCompletionHandlerSelector
  , readAttributePresentValueWithCompletionSelector
  , readAttributeReliabilityWithAttributeCache_endpoint_queue_completionHandlerSelector
  , readAttributeReliabilityWithClusterStateCache_endpoint_queue_completionSelector
  , readAttributeReliabilityWithCompletionHandlerSelector
  , readAttributeReliabilityWithCompletionSelector
  , readAttributeStatusFlagsWithAttributeCache_endpoint_queue_completionHandlerSelector
  , readAttributeStatusFlagsWithClusterStateCache_endpoint_queue_completionSelector
  , readAttributeStatusFlagsWithCompletionHandlerSelector
  , readAttributeStatusFlagsWithCompletionSelector
  , subscribeAttributeAcceptedCommandListWithMinInterval_maxInterval_params_subscriptionEstablished_reportHandlerSelector
  , subscribeAttributeAcceptedCommandListWithParams_subscriptionEstablished_reportHandlerSelector
  , subscribeAttributeActiveTextWithMinInterval_maxInterval_params_subscriptionEstablished_reportHandlerSelector
  , subscribeAttributeActiveTextWithParams_subscriptionEstablished_reportHandlerSelector
  , subscribeAttributeApplicationTypeWithMinInterval_maxInterval_params_subscriptionEstablished_reportHandlerSelector
  , subscribeAttributeApplicationTypeWithParams_subscriptionEstablished_reportHandlerSelector
  , subscribeAttributeAttributeListWithMinInterval_maxInterval_params_subscriptionEstablished_reportHandlerSelector
  , subscribeAttributeAttributeListWithParams_subscriptionEstablished_reportHandlerSelector
  , subscribeAttributeClusterRevisionWithMinInterval_maxInterval_params_subscriptionEstablished_reportHandlerSelector
  , subscribeAttributeClusterRevisionWithParams_subscriptionEstablished_reportHandlerSelector
  , subscribeAttributeDescriptionWithMinInterval_maxInterval_params_subscriptionEstablished_reportHandlerSelector
  , subscribeAttributeDescriptionWithParams_subscriptionEstablished_reportHandlerSelector
  , subscribeAttributeFeatureMapWithMinInterval_maxInterval_params_subscriptionEstablished_reportHandlerSelector
  , subscribeAttributeFeatureMapWithParams_subscriptionEstablished_reportHandlerSelector
  , subscribeAttributeGeneratedCommandListWithMinInterval_maxInterval_params_subscriptionEstablished_reportHandlerSelector
  , subscribeAttributeGeneratedCommandListWithParams_subscriptionEstablished_reportHandlerSelector
  , subscribeAttributeInactiveTextWithMinInterval_maxInterval_params_subscriptionEstablished_reportHandlerSelector
  , subscribeAttributeInactiveTextWithParams_subscriptionEstablished_reportHandlerSelector
  , subscribeAttributeOutOfServiceWithMinInterval_maxInterval_params_subscriptionEstablished_reportHandlerSelector
  , subscribeAttributeOutOfServiceWithParams_subscriptionEstablished_reportHandlerSelector
  , subscribeAttributePolarityWithMinInterval_maxInterval_params_subscriptionEstablished_reportHandlerSelector
  , subscribeAttributePolarityWithParams_subscriptionEstablished_reportHandlerSelector
  , subscribeAttributePresentValueWithMinInterval_maxInterval_params_subscriptionEstablished_reportHandlerSelector
  , subscribeAttributePresentValueWithParams_subscriptionEstablished_reportHandlerSelector
  , subscribeAttributeReliabilityWithMinInterval_maxInterval_params_subscriptionEstablished_reportHandlerSelector
  , subscribeAttributeReliabilityWithParams_subscriptionEstablished_reportHandlerSelector
  , subscribeAttributeStatusFlagsWithMinInterval_maxInterval_params_subscriptionEstablished_reportHandlerSelector
  , subscribeAttributeStatusFlagsWithParams_subscriptionEstablished_reportHandlerSelector
  , writeAttributeActiveTextWithValue_completionHandlerSelector
  , writeAttributeActiveTextWithValue_completionSelector
  , writeAttributeActiveTextWithValue_params_completionHandlerSelector
  , writeAttributeActiveTextWithValue_params_completionSelector
  , writeAttributeDescriptionWithValue_completionHandlerSelector
  , writeAttributeDescriptionWithValue_completionSelector
  , writeAttributeDescriptionWithValue_params_completionHandlerSelector
  , writeAttributeDescriptionWithValue_params_completionSelector
  , writeAttributeInactiveTextWithValue_completionHandlerSelector
  , writeAttributeInactiveTextWithValue_completionSelector
  , writeAttributeInactiveTextWithValue_params_completionHandlerSelector
  , writeAttributeInactiveTextWithValue_params_completionSelector
  , writeAttributeOutOfServiceWithValue_completionHandlerSelector
  , writeAttributeOutOfServiceWithValue_completionSelector
  , writeAttributeOutOfServiceWithValue_params_completionHandlerSelector
  , writeAttributeOutOfServiceWithValue_params_completionSelector
  , writeAttributePresentValueWithValue_completionHandlerSelector
  , writeAttributePresentValueWithValue_completionSelector
  , writeAttributePresentValueWithValue_params_completionHandlerSelector
  , writeAttributePresentValueWithValue_params_completionSelector
  , writeAttributeReliabilityWithValue_completionHandlerSelector
  , writeAttributeReliabilityWithValue_completionSelector
  , writeAttributeReliabilityWithValue_params_completionHandlerSelector
  , writeAttributeReliabilityWithValue_params_completionSelector


  ) where

import Foreign.Ptr (Ptr, FunPtr)
import Foreign.C.Types

import ObjC.Runtime.Types
import ObjC.Runtime.Message (sendMessage, sendOwnedMessage, sendClassMessage, sendOwnedClassMessage)
import ObjC.Runtime.Selector (mkSelector)
import ObjC.Runtime.Class (getRequiredClass)

import ObjC.Matter.Internal.Classes
import ObjC.Foundation.Internal.Classes

-- | @- readAttributeActiveTextWithCompletion:@
readAttributeActiveTextWithCompletion :: IsMTRBaseClusterBinaryInputBasic mtrBaseClusterBinaryInputBasic => mtrBaseClusterBinaryInputBasic -> Ptr () -> IO ()
readAttributeActiveTextWithCompletion mtrBaseClusterBinaryInputBasic completion =
  sendMessage mtrBaseClusterBinaryInputBasic readAttributeActiveTextWithCompletionSelector completion

-- | @- writeAttributeActiveTextWithValue:completion:@
writeAttributeActiveTextWithValue_completion :: (IsMTRBaseClusterBinaryInputBasic mtrBaseClusterBinaryInputBasic, IsNSString value) => mtrBaseClusterBinaryInputBasic -> value -> Ptr () -> IO ()
writeAttributeActiveTextWithValue_completion mtrBaseClusterBinaryInputBasic value completion =
  sendMessage mtrBaseClusterBinaryInputBasic writeAttributeActiveTextWithValue_completionSelector (toNSString value) completion

-- | @- writeAttributeActiveTextWithValue:params:completion:@
writeAttributeActiveTextWithValue_params_completion :: (IsMTRBaseClusterBinaryInputBasic mtrBaseClusterBinaryInputBasic, IsNSString value, IsMTRWriteParams params) => mtrBaseClusterBinaryInputBasic -> value -> params -> Ptr () -> IO ()
writeAttributeActiveTextWithValue_params_completion mtrBaseClusterBinaryInputBasic value params completion =
  sendMessage mtrBaseClusterBinaryInputBasic writeAttributeActiveTextWithValue_params_completionSelector (toNSString value) (toMTRWriteParams params) completion

-- | @- subscribeAttributeActiveTextWithParams:subscriptionEstablished:reportHandler:@
subscribeAttributeActiveTextWithParams_subscriptionEstablished_reportHandler :: (IsMTRBaseClusterBinaryInputBasic mtrBaseClusterBinaryInputBasic, IsMTRSubscribeParams params) => mtrBaseClusterBinaryInputBasic -> params -> Ptr () -> Ptr () -> IO ()
subscribeAttributeActiveTextWithParams_subscriptionEstablished_reportHandler mtrBaseClusterBinaryInputBasic params subscriptionEstablished reportHandler =
  sendMessage mtrBaseClusterBinaryInputBasic subscribeAttributeActiveTextWithParams_subscriptionEstablished_reportHandlerSelector (toMTRSubscribeParams params) subscriptionEstablished reportHandler

-- | @+ readAttributeActiveTextWithClusterStateCache:endpoint:queue:completion:@
readAttributeActiveTextWithClusterStateCache_endpoint_queue_completion :: (IsMTRClusterStateCacheContainer clusterStateCacheContainer, IsNSNumber endpoint, IsNSObject queue) => clusterStateCacheContainer -> endpoint -> queue -> Ptr () -> IO ()
readAttributeActiveTextWithClusterStateCache_endpoint_queue_completion clusterStateCacheContainer endpoint queue completion =
  do
    cls' <- getRequiredClass "MTRBaseClusterBinaryInputBasic"
    sendClassMessage cls' readAttributeActiveTextWithClusterStateCache_endpoint_queue_completionSelector (toMTRClusterStateCacheContainer clusterStateCacheContainer) (toNSNumber endpoint) (toNSObject queue) completion

-- | @- readAttributeDescriptionWithCompletion:@
readAttributeDescriptionWithCompletion :: IsMTRBaseClusterBinaryInputBasic mtrBaseClusterBinaryInputBasic => mtrBaseClusterBinaryInputBasic -> Ptr () -> IO ()
readAttributeDescriptionWithCompletion mtrBaseClusterBinaryInputBasic completion =
  sendMessage mtrBaseClusterBinaryInputBasic readAttributeDescriptionWithCompletionSelector completion

-- | @- writeAttributeDescriptionWithValue:completion:@
writeAttributeDescriptionWithValue_completion :: (IsMTRBaseClusterBinaryInputBasic mtrBaseClusterBinaryInputBasic, IsNSString value) => mtrBaseClusterBinaryInputBasic -> value -> Ptr () -> IO ()
writeAttributeDescriptionWithValue_completion mtrBaseClusterBinaryInputBasic value completion =
  sendMessage mtrBaseClusterBinaryInputBasic writeAttributeDescriptionWithValue_completionSelector (toNSString value) completion

-- | @- writeAttributeDescriptionWithValue:params:completion:@
writeAttributeDescriptionWithValue_params_completion :: (IsMTRBaseClusterBinaryInputBasic mtrBaseClusterBinaryInputBasic, IsNSString value, IsMTRWriteParams params) => mtrBaseClusterBinaryInputBasic -> value -> params -> Ptr () -> IO ()
writeAttributeDescriptionWithValue_params_completion mtrBaseClusterBinaryInputBasic value params completion =
  sendMessage mtrBaseClusterBinaryInputBasic writeAttributeDescriptionWithValue_params_completionSelector (toNSString value) (toMTRWriteParams params) completion

-- | @- subscribeAttributeDescriptionWithParams:subscriptionEstablished:reportHandler:@
subscribeAttributeDescriptionWithParams_subscriptionEstablished_reportHandler :: (IsMTRBaseClusterBinaryInputBasic mtrBaseClusterBinaryInputBasic, IsMTRSubscribeParams params) => mtrBaseClusterBinaryInputBasic -> params -> Ptr () -> Ptr () -> IO ()
subscribeAttributeDescriptionWithParams_subscriptionEstablished_reportHandler mtrBaseClusterBinaryInputBasic params subscriptionEstablished reportHandler =
  sendMessage mtrBaseClusterBinaryInputBasic subscribeAttributeDescriptionWithParams_subscriptionEstablished_reportHandlerSelector (toMTRSubscribeParams params) subscriptionEstablished reportHandler

-- | @+ readAttributeDescriptionWithClusterStateCache:endpoint:queue:completion:@
readAttributeDescriptionWithClusterStateCache_endpoint_queue_completion :: (IsMTRClusterStateCacheContainer clusterStateCacheContainer, IsNSNumber endpoint, IsNSObject queue) => clusterStateCacheContainer -> endpoint -> queue -> Ptr () -> IO ()
readAttributeDescriptionWithClusterStateCache_endpoint_queue_completion clusterStateCacheContainer endpoint queue completion =
  do
    cls' <- getRequiredClass "MTRBaseClusterBinaryInputBasic"
    sendClassMessage cls' readAttributeDescriptionWithClusterStateCache_endpoint_queue_completionSelector (toMTRClusterStateCacheContainer clusterStateCacheContainer) (toNSNumber endpoint) (toNSObject queue) completion

-- | @- readAttributeInactiveTextWithCompletion:@
readAttributeInactiveTextWithCompletion :: IsMTRBaseClusterBinaryInputBasic mtrBaseClusterBinaryInputBasic => mtrBaseClusterBinaryInputBasic -> Ptr () -> IO ()
readAttributeInactiveTextWithCompletion mtrBaseClusterBinaryInputBasic completion =
  sendMessage mtrBaseClusterBinaryInputBasic readAttributeInactiveTextWithCompletionSelector completion

-- | @- writeAttributeInactiveTextWithValue:completion:@
writeAttributeInactiveTextWithValue_completion :: (IsMTRBaseClusterBinaryInputBasic mtrBaseClusterBinaryInputBasic, IsNSString value) => mtrBaseClusterBinaryInputBasic -> value -> Ptr () -> IO ()
writeAttributeInactiveTextWithValue_completion mtrBaseClusterBinaryInputBasic value completion =
  sendMessage mtrBaseClusterBinaryInputBasic writeAttributeInactiveTextWithValue_completionSelector (toNSString value) completion

-- | @- writeAttributeInactiveTextWithValue:params:completion:@
writeAttributeInactiveTextWithValue_params_completion :: (IsMTRBaseClusterBinaryInputBasic mtrBaseClusterBinaryInputBasic, IsNSString value, IsMTRWriteParams params) => mtrBaseClusterBinaryInputBasic -> value -> params -> Ptr () -> IO ()
writeAttributeInactiveTextWithValue_params_completion mtrBaseClusterBinaryInputBasic value params completion =
  sendMessage mtrBaseClusterBinaryInputBasic writeAttributeInactiveTextWithValue_params_completionSelector (toNSString value) (toMTRWriteParams params) completion

-- | @- subscribeAttributeInactiveTextWithParams:subscriptionEstablished:reportHandler:@
subscribeAttributeInactiveTextWithParams_subscriptionEstablished_reportHandler :: (IsMTRBaseClusterBinaryInputBasic mtrBaseClusterBinaryInputBasic, IsMTRSubscribeParams params) => mtrBaseClusterBinaryInputBasic -> params -> Ptr () -> Ptr () -> IO ()
subscribeAttributeInactiveTextWithParams_subscriptionEstablished_reportHandler mtrBaseClusterBinaryInputBasic params subscriptionEstablished reportHandler =
  sendMessage mtrBaseClusterBinaryInputBasic subscribeAttributeInactiveTextWithParams_subscriptionEstablished_reportHandlerSelector (toMTRSubscribeParams params) subscriptionEstablished reportHandler

-- | @+ readAttributeInactiveTextWithClusterStateCache:endpoint:queue:completion:@
readAttributeInactiveTextWithClusterStateCache_endpoint_queue_completion :: (IsMTRClusterStateCacheContainer clusterStateCacheContainer, IsNSNumber endpoint, IsNSObject queue) => clusterStateCacheContainer -> endpoint -> queue -> Ptr () -> IO ()
readAttributeInactiveTextWithClusterStateCache_endpoint_queue_completion clusterStateCacheContainer endpoint queue completion =
  do
    cls' <- getRequiredClass "MTRBaseClusterBinaryInputBasic"
    sendClassMessage cls' readAttributeInactiveTextWithClusterStateCache_endpoint_queue_completionSelector (toMTRClusterStateCacheContainer clusterStateCacheContainer) (toNSNumber endpoint) (toNSObject queue) completion

-- | @- readAttributeOutOfServiceWithCompletion:@
readAttributeOutOfServiceWithCompletion :: IsMTRBaseClusterBinaryInputBasic mtrBaseClusterBinaryInputBasic => mtrBaseClusterBinaryInputBasic -> Ptr () -> IO ()
readAttributeOutOfServiceWithCompletion mtrBaseClusterBinaryInputBasic completion =
  sendMessage mtrBaseClusterBinaryInputBasic readAttributeOutOfServiceWithCompletionSelector completion

-- | @- writeAttributeOutOfServiceWithValue:completion:@
writeAttributeOutOfServiceWithValue_completion :: (IsMTRBaseClusterBinaryInputBasic mtrBaseClusterBinaryInputBasic, IsNSNumber value) => mtrBaseClusterBinaryInputBasic -> value -> Ptr () -> IO ()
writeAttributeOutOfServiceWithValue_completion mtrBaseClusterBinaryInputBasic value completion =
  sendMessage mtrBaseClusterBinaryInputBasic writeAttributeOutOfServiceWithValue_completionSelector (toNSNumber value) completion

-- | @- writeAttributeOutOfServiceWithValue:params:completion:@
writeAttributeOutOfServiceWithValue_params_completion :: (IsMTRBaseClusterBinaryInputBasic mtrBaseClusterBinaryInputBasic, IsNSNumber value, IsMTRWriteParams params) => mtrBaseClusterBinaryInputBasic -> value -> params -> Ptr () -> IO ()
writeAttributeOutOfServiceWithValue_params_completion mtrBaseClusterBinaryInputBasic value params completion =
  sendMessage mtrBaseClusterBinaryInputBasic writeAttributeOutOfServiceWithValue_params_completionSelector (toNSNumber value) (toMTRWriteParams params) completion

-- | @- subscribeAttributeOutOfServiceWithParams:subscriptionEstablished:reportHandler:@
subscribeAttributeOutOfServiceWithParams_subscriptionEstablished_reportHandler :: (IsMTRBaseClusterBinaryInputBasic mtrBaseClusterBinaryInputBasic, IsMTRSubscribeParams params) => mtrBaseClusterBinaryInputBasic -> params -> Ptr () -> Ptr () -> IO ()
subscribeAttributeOutOfServiceWithParams_subscriptionEstablished_reportHandler mtrBaseClusterBinaryInputBasic params subscriptionEstablished reportHandler =
  sendMessage mtrBaseClusterBinaryInputBasic subscribeAttributeOutOfServiceWithParams_subscriptionEstablished_reportHandlerSelector (toMTRSubscribeParams params) subscriptionEstablished reportHandler

-- | @+ readAttributeOutOfServiceWithClusterStateCache:endpoint:queue:completion:@
readAttributeOutOfServiceWithClusterStateCache_endpoint_queue_completion :: (IsMTRClusterStateCacheContainer clusterStateCacheContainer, IsNSNumber endpoint, IsNSObject queue) => clusterStateCacheContainer -> endpoint -> queue -> Ptr () -> IO ()
readAttributeOutOfServiceWithClusterStateCache_endpoint_queue_completion clusterStateCacheContainer endpoint queue completion =
  do
    cls' <- getRequiredClass "MTRBaseClusterBinaryInputBasic"
    sendClassMessage cls' readAttributeOutOfServiceWithClusterStateCache_endpoint_queue_completionSelector (toMTRClusterStateCacheContainer clusterStateCacheContainer) (toNSNumber endpoint) (toNSObject queue) completion

-- | @- readAttributePolarityWithCompletion:@
readAttributePolarityWithCompletion :: IsMTRBaseClusterBinaryInputBasic mtrBaseClusterBinaryInputBasic => mtrBaseClusterBinaryInputBasic -> Ptr () -> IO ()
readAttributePolarityWithCompletion mtrBaseClusterBinaryInputBasic completion =
  sendMessage mtrBaseClusterBinaryInputBasic readAttributePolarityWithCompletionSelector completion

-- | @- subscribeAttributePolarityWithParams:subscriptionEstablished:reportHandler:@
subscribeAttributePolarityWithParams_subscriptionEstablished_reportHandler :: (IsMTRBaseClusterBinaryInputBasic mtrBaseClusterBinaryInputBasic, IsMTRSubscribeParams params) => mtrBaseClusterBinaryInputBasic -> params -> Ptr () -> Ptr () -> IO ()
subscribeAttributePolarityWithParams_subscriptionEstablished_reportHandler mtrBaseClusterBinaryInputBasic params subscriptionEstablished reportHandler =
  sendMessage mtrBaseClusterBinaryInputBasic subscribeAttributePolarityWithParams_subscriptionEstablished_reportHandlerSelector (toMTRSubscribeParams params) subscriptionEstablished reportHandler

-- | @+ readAttributePolarityWithClusterStateCache:endpoint:queue:completion:@
readAttributePolarityWithClusterStateCache_endpoint_queue_completion :: (IsMTRClusterStateCacheContainer clusterStateCacheContainer, IsNSNumber endpoint, IsNSObject queue) => clusterStateCacheContainer -> endpoint -> queue -> Ptr () -> IO ()
readAttributePolarityWithClusterStateCache_endpoint_queue_completion clusterStateCacheContainer endpoint queue completion =
  do
    cls' <- getRequiredClass "MTRBaseClusterBinaryInputBasic"
    sendClassMessage cls' readAttributePolarityWithClusterStateCache_endpoint_queue_completionSelector (toMTRClusterStateCacheContainer clusterStateCacheContainer) (toNSNumber endpoint) (toNSObject queue) completion

-- | @- readAttributePresentValueWithCompletion:@
readAttributePresentValueWithCompletion :: IsMTRBaseClusterBinaryInputBasic mtrBaseClusterBinaryInputBasic => mtrBaseClusterBinaryInputBasic -> Ptr () -> IO ()
readAttributePresentValueWithCompletion mtrBaseClusterBinaryInputBasic completion =
  sendMessage mtrBaseClusterBinaryInputBasic readAttributePresentValueWithCompletionSelector completion

-- | @- writeAttributePresentValueWithValue:completion:@
writeAttributePresentValueWithValue_completion :: (IsMTRBaseClusterBinaryInputBasic mtrBaseClusterBinaryInputBasic, IsNSNumber value) => mtrBaseClusterBinaryInputBasic -> value -> Ptr () -> IO ()
writeAttributePresentValueWithValue_completion mtrBaseClusterBinaryInputBasic value completion =
  sendMessage mtrBaseClusterBinaryInputBasic writeAttributePresentValueWithValue_completionSelector (toNSNumber value) completion

-- | @- writeAttributePresentValueWithValue:params:completion:@
writeAttributePresentValueWithValue_params_completion :: (IsMTRBaseClusterBinaryInputBasic mtrBaseClusterBinaryInputBasic, IsNSNumber value, IsMTRWriteParams params) => mtrBaseClusterBinaryInputBasic -> value -> params -> Ptr () -> IO ()
writeAttributePresentValueWithValue_params_completion mtrBaseClusterBinaryInputBasic value params completion =
  sendMessage mtrBaseClusterBinaryInputBasic writeAttributePresentValueWithValue_params_completionSelector (toNSNumber value) (toMTRWriteParams params) completion

-- | @- subscribeAttributePresentValueWithParams:subscriptionEstablished:reportHandler:@
subscribeAttributePresentValueWithParams_subscriptionEstablished_reportHandler :: (IsMTRBaseClusterBinaryInputBasic mtrBaseClusterBinaryInputBasic, IsMTRSubscribeParams params) => mtrBaseClusterBinaryInputBasic -> params -> Ptr () -> Ptr () -> IO ()
subscribeAttributePresentValueWithParams_subscriptionEstablished_reportHandler mtrBaseClusterBinaryInputBasic params subscriptionEstablished reportHandler =
  sendMessage mtrBaseClusterBinaryInputBasic subscribeAttributePresentValueWithParams_subscriptionEstablished_reportHandlerSelector (toMTRSubscribeParams params) subscriptionEstablished reportHandler

-- | @+ readAttributePresentValueWithClusterStateCache:endpoint:queue:completion:@
readAttributePresentValueWithClusterStateCache_endpoint_queue_completion :: (IsMTRClusterStateCacheContainer clusterStateCacheContainer, IsNSNumber endpoint, IsNSObject queue) => clusterStateCacheContainer -> endpoint -> queue -> Ptr () -> IO ()
readAttributePresentValueWithClusterStateCache_endpoint_queue_completion clusterStateCacheContainer endpoint queue completion =
  do
    cls' <- getRequiredClass "MTRBaseClusterBinaryInputBasic"
    sendClassMessage cls' readAttributePresentValueWithClusterStateCache_endpoint_queue_completionSelector (toMTRClusterStateCacheContainer clusterStateCacheContainer) (toNSNumber endpoint) (toNSObject queue) completion

-- | @- readAttributeReliabilityWithCompletion:@
readAttributeReliabilityWithCompletion :: IsMTRBaseClusterBinaryInputBasic mtrBaseClusterBinaryInputBasic => mtrBaseClusterBinaryInputBasic -> Ptr () -> IO ()
readAttributeReliabilityWithCompletion mtrBaseClusterBinaryInputBasic completion =
  sendMessage mtrBaseClusterBinaryInputBasic readAttributeReliabilityWithCompletionSelector completion

-- | @- writeAttributeReliabilityWithValue:completion:@
writeAttributeReliabilityWithValue_completion :: (IsMTRBaseClusterBinaryInputBasic mtrBaseClusterBinaryInputBasic, IsNSNumber value) => mtrBaseClusterBinaryInputBasic -> value -> Ptr () -> IO ()
writeAttributeReliabilityWithValue_completion mtrBaseClusterBinaryInputBasic value completion =
  sendMessage mtrBaseClusterBinaryInputBasic writeAttributeReliabilityWithValue_completionSelector (toNSNumber value) completion

-- | @- writeAttributeReliabilityWithValue:params:completion:@
writeAttributeReliabilityWithValue_params_completion :: (IsMTRBaseClusterBinaryInputBasic mtrBaseClusterBinaryInputBasic, IsNSNumber value, IsMTRWriteParams params) => mtrBaseClusterBinaryInputBasic -> value -> params -> Ptr () -> IO ()
writeAttributeReliabilityWithValue_params_completion mtrBaseClusterBinaryInputBasic value params completion =
  sendMessage mtrBaseClusterBinaryInputBasic writeAttributeReliabilityWithValue_params_completionSelector (toNSNumber value) (toMTRWriteParams params) completion

-- | @- subscribeAttributeReliabilityWithParams:subscriptionEstablished:reportHandler:@
subscribeAttributeReliabilityWithParams_subscriptionEstablished_reportHandler :: (IsMTRBaseClusterBinaryInputBasic mtrBaseClusterBinaryInputBasic, IsMTRSubscribeParams params) => mtrBaseClusterBinaryInputBasic -> params -> Ptr () -> Ptr () -> IO ()
subscribeAttributeReliabilityWithParams_subscriptionEstablished_reportHandler mtrBaseClusterBinaryInputBasic params subscriptionEstablished reportHandler =
  sendMessage mtrBaseClusterBinaryInputBasic subscribeAttributeReliabilityWithParams_subscriptionEstablished_reportHandlerSelector (toMTRSubscribeParams params) subscriptionEstablished reportHandler

-- | @+ readAttributeReliabilityWithClusterStateCache:endpoint:queue:completion:@
readAttributeReliabilityWithClusterStateCache_endpoint_queue_completion :: (IsMTRClusterStateCacheContainer clusterStateCacheContainer, IsNSNumber endpoint, IsNSObject queue) => clusterStateCacheContainer -> endpoint -> queue -> Ptr () -> IO ()
readAttributeReliabilityWithClusterStateCache_endpoint_queue_completion clusterStateCacheContainer endpoint queue completion =
  do
    cls' <- getRequiredClass "MTRBaseClusterBinaryInputBasic"
    sendClassMessage cls' readAttributeReliabilityWithClusterStateCache_endpoint_queue_completionSelector (toMTRClusterStateCacheContainer clusterStateCacheContainer) (toNSNumber endpoint) (toNSObject queue) completion

-- | @- readAttributeStatusFlagsWithCompletion:@
readAttributeStatusFlagsWithCompletion :: IsMTRBaseClusterBinaryInputBasic mtrBaseClusterBinaryInputBasic => mtrBaseClusterBinaryInputBasic -> Ptr () -> IO ()
readAttributeStatusFlagsWithCompletion mtrBaseClusterBinaryInputBasic completion =
  sendMessage mtrBaseClusterBinaryInputBasic readAttributeStatusFlagsWithCompletionSelector completion

-- | @- subscribeAttributeStatusFlagsWithParams:subscriptionEstablished:reportHandler:@
subscribeAttributeStatusFlagsWithParams_subscriptionEstablished_reportHandler :: (IsMTRBaseClusterBinaryInputBasic mtrBaseClusterBinaryInputBasic, IsMTRSubscribeParams params) => mtrBaseClusterBinaryInputBasic -> params -> Ptr () -> Ptr () -> IO ()
subscribeAttributeStatusFlagsWithParams_subscriptionEstablished_reportHandler mtrBaseClusterBinaryInputBasic params subscriptionEstablished reportHandler =
  sendMessage mtrBaseClusterBinaryInputBasic subscribeAttributeStatusFlagsWithParams_subscriptionEstablished_reportHandlerSelector (toMTRSubscribeParams params) subscriptionEstablished reportHandler

-- | @+ readAttributeStatusFlagsWithClusterStateCache:endpoint:queue:completion:@
readAttributeStatusFlagsWithClusterStateCache_endpoint_queue_completion :: (IsMTRClusterStateCacheContainer clusterStateCacheContainer, IsNSNumber endpoint, IsNSObject queue) => clusterStateCacheContainer -> endpoint -> queue -> Ptr () -> IO ()
readAttributeStatusFlagsWithClusterStateCache_endpoint_queue_completion clusterStateCacheContainer endpoint queue completion =
  do
    cls' <- getRequiredClass "MTRBaseClusterBinaryInputBasic"
    sendClassMessage cls' readAttributeStatusFlagsWithClusterStateCache_endpoint_queue_completionSelector (toMTRClusterStateCacheContainer clusterStateCacheContainer) (toNSNumber endpoint) (toNSObject queue) completion

-- | @- readAttributeApplicationTypeWithCompletion:@
readAttributeApplicationTypeWithCompletion :: IsMTRBaseClusterBinaryInputBasic mtrBaseClusterBinaryInputBasic => mtrBaseClusterBinaryInputBasic -> Ptr () -> IO ()
readAttributeApplicationTypeWithCompletion mtrBaseClusterBinaryInputBasic completion =
  sendMessage mtrBaseClusterBinaryInputBasic readAttributeApplicationTypeWithCompletionSelector completion

-- | @- subscribeAttributeApplicationTypeWithParams:subscriptionEstablished:reportHandler:@
subscribeAttributeApplicationTypeWithParams_subscriptionEstablished_reportHandler :: (IsMTRBaseClusterBinaryInputBasic mtrBaseClusterBinaryInputBasic, IsMTRSubscribeParams params) => mtrBaseClusterBinaryInputBasic -> params -> Ptr () -> Ptr () -> IO ()
subscribeAttributeApplicationTypeWithParams_subscriptionEstablished_reportHandler mtrBaseClusterBinaryInputBasic params subscriptionEstablished reportHandler =
  sendMessage mtrBaseClusterBinaryInputBasic subscribeAttributeApplicationTypeWithParams_subscriptionEstablished_reportHandlerSelector (toMTRSubscribeParams params) subscriptionEstablished reportHandler

-- | @+ readAttributeApplicationTypeWithClusterStateCache:endpoint:queue:completion:@
readAttributeApplicationTypeWithClusterStateCache_endpoint_queue_completion :: (IsMTRClusterStateCacheContainer clusterStateCacheContainer, IsNSNumber endpoint, IsNSObject queue) => clusterStateCacheContainer -> endpoint -> queue -> Ptr () -> IO ()
readAttributeApplicationTypeWithClusterStateCache_endpoint_queue_completion clusterStateCacheContainer endpoint queue completion =
  do
    cls' <- getRequiredClass "MTRBaseClusterBinaryInputBasic"
    sendClassMessage cls' readAttributeApplicationTypeWithClusterStateCache_endpoint_queue_completionSelector (toMTRClusterStateCacheContainer clusterStateCacheContainer) (toNSNumber endpoint) (toNSObject queue) completion

-- | @- readAttributeGeneratedCommandListWithCompletion:@
readAttributeGeneratedCommandListWithCompletion :: IsMTRBaseClusterBinaryInputBasic mtrBaseClusterBinaryInputBasic => mtrBaseClusterBinaryInputBasic -> Ptr () -> IO ()
readAttributeGeneratedCommandListWithCompletion mtrBaseClusterBinaryInputBasic completion =
  sendMessage mtrBaseClusterBinaryInputBasic readAttributeGeneratedCommandListWithCompletionSelector completion

-- | @- subscribeAttributeGeneratedCommandListWithParams:subscriptionEstablished:reportHandler:@
subscribeAttributeGeneratedCommandListWithParams_subscriptionEstablished_reportHandler :: (IsMTRBaseClusterBinaryInputBasic mtrBaseClusterBinaryInputBasic, IsMTRSubscribeParams params) => mtrBaseClusterBinaryInputBasic -> params -> Ptr () -> Ptr () -> IO ()
subscribeAttributeGeneratedCommandListWithParams_subscriptionEstablished_reportHandler mtrBaseClusterBinaryInputBasic params subscriptionEstablished reportHandler =
  sendMessage mtrBaseClusterBinaryInputBasic subscribeAttributeGeneratedCommandListWithParams_subscriptionEstablished_reportHandlerSelector (toMTRSubscribeParams params) subscriptionEstablished reportHandler

-- | @+ readAttributeGeneratedCommandListWithClusterStateCache:endpoint:queue:completion:@
readAttributeGeneratedCommandListWithClusterStateCache_endpoint_queue_completion :: (IsMTRClusterStateCacheContainer clusterStateCacheContainer, IsNSNumber endpoint, IsNSObject queue) => clusterStateCacheContainer -> endpoint -> queue -> Ptr () -> IO ()
readAttributeGeneratedCommandListWithClusterStateCache_endpoint_queue_completion clusterStateCacheContainer endpoint queue completion =
  do
    cls' <- getRequiredClass "MTRBaseClusterBinaryInputBasic"
    sendClassMessage cls' readAttributeGeneratedCommandListWithClusterStateCache_endpoint_queue_completionSelector (toMTRClusterStateCacheContainer clusterStateCacheContainer) (toNSNumber endpoint) (toNSObject queue) completion

-- | @- readAttributeAcceptedCommandListWithCompletion:@
readAttributeAcceptedCommandListWithCompletion :: IsMTRBaseClusterBinaryInputBasic mtrBaseClusterBinaryInputBasic => mtrBaseClusterBinaryInputBasic -> Ptr () -> IO ()
readAttributeAcceptedCommandListWithCompletion mtrBaseClusterBinaryInputBasic completion =
  sendMessage mtrBaseClusterBinaryInputBasic readAttributeAcceptedCommandListWithCompletionSelector completion

-- | @- subscribeAttributeAcceptedCommandListWithParams:subscriptionEstablished:reportHandler:@
subscribeAttributeAcceptedCommandListWithParams_subscriptionEstablished_reportHandler :: (IsMTRBaseClusterBinaryInputBasic mtrBaseClusterBinaryInputBasic, IsMTRSubscribeParams params) => mtrBaseClusterBinaryInputBasic -> params -> Ptr () -> Ptr () -> IO ()
subscribeAttributeAcceptedCommandListWithParams_subscriptionEstablished_reportHandler mtrBaseClusterBinaryInputBasic params subscriptionEstablished reportHandler =
  sendMessage mtrBaseClusterBinaryInputBasic subscribeAttributeAcceptedCommandListWithParams_subscriptionEstablished_reportHandlerSelector (toMTRSubscribeParams params) subscriptionEstablished reportHandler

-- | @+ readAttributeAcceptedCommandListWithClusterStateCache:endpoint:queue:completion:@
readAttributeAcceptedCommandListWithClusterStateCache_endpoint_queue_completion :: (IsMTRClusterStateCacheContainer clusterStateCacheContainer, IsNSNumber endpoint, IsNSObject queue) => clusterStateCacheContainer -> endpoint -> queue -> Ptr () -> IO ()
readAttributeAcceptedCommandListWithClusterStateCache_endpoint_queue_completion clusterStateCacheContainer endpoint queue completion =
  do
    cls' <- getRequiredClass "MTRBaseClusterBinaryInputBasic"
    sendClassMessage cls' readAttributeAcceptedCommandListWithClusterStateCache_endpoint_queue_completionSelector (toMTRClusterStateCacheContainer clusterStateCacheContainer) (toNSNumber endpoint) (toNSObject queue) completion

-- | @- readAttributeAttributeListWithCompletion:@
readAttributeAttributeListWithCompletion :: IsMTRBaseClusterBinaryInputBasic mtrBaseClusterBinaryInputBasic => mtrBaseClusterBinaryInputBasic -> Ptr () -> IO ()
readAttributeAttributeListWithCompletion mtrBaseClusterBinaryInputBasic completion =
  sendMessage mtrBaseClusterBinaryInputBasic readAttributeAttributeListWithCompletionSelector completion

-- | @- subscribeAttributeAttributeListWithParams:subscriptionEstablished:reportHandler:@
subscribeAttributeAttributeListWithParams_subscriptionEstablished_reportHandler :: (IsMTRBaseClusterBinaryInputBasic mtrBaseClusterBinaryInputBasic, IsMTRSubscribeParams params) => mtrBaseClusterBinaryInputBasic -> params -> Ptr () -> Ptr () -> IO ()
subscribeAttributeAttributeListWithParams_subscriptionEstablished_reportHandler mtrBaseClusterBinaryInputBasic params subscriptionEstablished reportHandler =
  sendMessage mtrBaseClusterBinaryInputBasic subscribeAttributeAttributeListWithParams_subscriptionEstablished_reportHandlerSelector (toMTRSubscribeParams params) subscriptionEstablished reportHandler

-- | @+ readAttributeAttributeListWithClusterStateCache:endpoint:queue:completion:@
readAttributeAttributeListWithClusterStateCache_endpoint_queue_completion :: (IsMTRClusterStateCacheContainer clusterStateCacheContainer, IsNSNumber endpoint, IsNSObject queue) => clusterStateCacheContainer -> endpoint -> queue -> Ptr () -> IO ()
readAttributeAttributeListWithClusterStateCache_endpoint_queue_completion clusterStateCacheContainer endpoint queue completion =
  do
    cls' <- getRequiredClass "MTRBaseClusterBinaryInputBasic"
    sendClassMessage cls' readAttributeAttributeListWithClusterStateCache_endpoint_queue_completionSelector (toMTRClusterStateCacheContainer clusterStateCacheContainer) (toNSNumber endpoint) (toNSObject queue) completion

-- | @- readAttributeFeatureMapWithCompletion:@
readAttributeFeatureMapWithCompletion :: IsMTRBaseClusterBinaryInputBasic mtrBaseClusterBinaryInputBasic => mtrBaseClusterBinaryInputBasic -> Ptr () -> IO ()
readAttributeFeatureMapWithCompletion mtrBaseClusterBinaryInputBasic completion =
  sendMessage mtrBaseClusterBinaryInputBasic readAttributeFeatureMapWithCompletionSelector completion

-- | @- subscribeAttributeFeatureMapWithParams:subscriptionEstablished:reportHandler:@
subscribeAttributeFeatureMapWithParams_subscriptionEstablished_reportHandler :: (IsMTRBaseClusterBinaryInputBasic mtrBaseClusterBinaryInputBasic, IsMTRSubscribeParams params) => mtrBaseClusterBinaryInputBasic -> params -> Ptr () -> Ptr () -> IO ()
subscribeAttributeFeatureMapWithParams_subscriptionEstablished_reportHandler mtrBaseClusterBinaryInputBasic params subscriptionEstablished reportHandler =
  sendMessage mtrBaseClusterBinaryInputBasic subscribeAttributeFeatureMapWithParams_subscriptionEstablished_reportHandlerSelector (toMTRSubscribeParams params) subscriptionEstablished reportHandler

-- | @+ readAttributeFeatureMapWithClusterStateCache:endpoint:queue:completion:@
readAttributeFeatureMapWithClusterStateCache_endpoint_queue_completion :: (IsMTRClusterStateCacheContainer clusterStateCacheContainer, IsNSNumber endpoint, IsNSObject queue) => clusterStateCacheContainer -> endpoint -> queue -> Ptr () -> IO ()
readAttributeFeatureMapWithClusterStateCache_endpoint_queue_completion clusterStateCacheContainer endpoint queue completion =
  do
    cls' <- getRequiredClass "MTRBaseClusterBinaryInputBasic"
    sendClassMessage cls' readAttributeFeatureMapWithClusterStateCache_endpoint_queue_completionSelector (toMTRClusterStateCacheContainer clusterStateCacheContainer) (toNSNumber endpoint) (toNSObject queue) completion

-- | @- readAttributeClusterRevisionWithCompletion:@
readAttributeClusterRevisionWithCompletion :: IsMTRBaseClusterBinaryInputBasic mtrBaseClusterBinaryInputBasic => mtrBaseClusterBinaryInputBasic -> Ptr () -> IO ()
readAttributeClusterRevisionWithCompletion mtrBaseClusterBinaryInputBasic completion =
  sendMessage mtrBaseClusterBinaryInputBasic readAttributeClusterRevisionWithCompletionSelector completion

-- | @- subscribeAttributeClusterRevisionWithParams:subscriptionEstablished:reportHandler:@
subscribeAttributeClusterRevisionWithParams_subscriptionEstablished_reportHandler :: (IsMTRBaseClusterBinaryInputBasic mtrBaseClusterBinaryInputBasic, IsMTRSubscribeParams params) => mtrBaseClusterBinaryInputBasic -> params -> Ptr () -> Ptr () -> IO ()
subscribeAttributeClusterRevisionWithParams_subscriptionEstablished_reportHandler mtrBaseClusterBinaryInputBasic params subscriptionEstablished reportHandler =
  sendMessage mtrBaseClusterBinaryInputBasic subscribeAttributeClusterRevisionWithParams_subscriptionEstablished_reportHandlerSelector (toMTRSubscribeParams params) subscriptionEstablished reportHandler

-- | @+ readAttributeClusterRevisionWithClusterStateCache:endpoint:queue:completion:@
readAttributeClusterRevisionWithClusterStateCache_endpoint_queue_completion :: (IsMTRClusterStateCacheContainer clusterStateCacheContainer, IsNSNumber endpoint, IsNSObject queue) => clusterStateCacheContainer -> endpoint -> queue -> Ptr () -> IO ()
readAttributeClusterRevisionWithClusterStateCache_endpoint_queue_completion clusterStateCacheContainer endpoint queue completion =
  do
    cls' <- getRequiredClass "MTRBaseClusterBinaryInputBasic"
    sendClassMessage cls' readAttributeClusterRevisionWithClusterStateCache_endpoint_queue_completionSelector (toMTRClusterStateCacheContainer clusterStateCacheContainer) (toNSNumber endpoint) (toNSObject queue) completion

-- | @- init@
init_ :: IsMTRBaseClusterBinaryInputBasic mtrBaseClusterBinaryInputBasic => mtrBaseClusterBinaryInputBasic -> IO (Id MTRBaseClusterBinaryInputBasic)
init_ mtrBaseClusterBinaryInputBasic =
  sendOwnedMessage mtrBaseClusterBinaryInputBasic initSelector

-- | @+ new@
new :: IO (Id MTRBaseClusterBinaryInputBasic)
new  =
  do
    cls' <- getRequiredClass "MTRBaseClusterBinaryInputBasic"
    sendOwnedClassMessage cls' newSelector

-- | @- initWithDevice:endpoint:queue:@
initWithDevice_endpoint_queue :: (IsMTRBaseClusterBinaryInputBasic mtrBaseClusterBinaryInputBasic, IsMTRBaseDevice device, IsNSObject queue) => mtrBaseClusterBinaryInputBasic -> device -> CUShort -> queue -> IO (Id MTRBaseClusterBinaryInputBasic)
initWithDevice_endpoint_queue mtrBaseClusterBinaryInputBasic device endpoint queue =
  sendOwnedMessage mtrBaseClusterBinaryInputBasic initWithDevice_endpoint_queueSelector (toMTRBaseDevice device) endpoint (toNSObject queue)

-- | @- readAttributeActiveTextWithCompletionHandler:@
readAttributeActiveTextWithCompletionHandler :: IsMTRBaseClusterBinaryInputBasic mtrBaseClusterBinaryInputBasic => mtrBaseClusterBinaryInputBasic -> Ptr () -> IO ()
readAttributeActiveTextWithCompletionHandler mtrBaseClusterBinaryInputBasic completionHandler =
  sendMessage mtrBaseClusterBinaryInputBasic readAttributeActiveTextWithCompletionHandlerSelector completionHandler

-- | @- writeAttributeActiveTextWithValue:completionHandler:@
writeAttributeActiveTextWithValue_completionHandler :: (IsMTRBaseClusterBinaryInputBasic mtrBaseClusterBinaryInputBasic, IsNSString value) => mtrBaseClusterBinaryInputBasic -> value -> Ptr () -> IO ()
writeAttributeActiveTextWithValue_completionHandler mtrBaseClusterBinaryInputBasic value completionHandler =
  sendMessage mtrBaseClusterBinaryInputBasic writeAttributeActiveTextWithValue_completionHandlerSelector (toNSString value) completionHandler

-- | @- writeAttributeActiveTextWithValue:params:completionHandler:@
writeAttributeActiveTextWithValue_params_completionHandler :: (IsMTRBaseClusterBinaryInputBasic mtrBaseClusterBinaryInputBasic, IsNSString value, IsMTRWriteParams params) => mtrBaseClusterBinaryInputBasic -> value -> params -> Ptr () -> IO ()
writeAttributeActiveTextWithValue_params_completionHandler mtrBaseClusterBinaryInputBasic value params completionHandler =
  sendMessage mtrBaseClusterBinaryInputBasic writeAttributeActiveTextWithValue_params_completionHandlerSelector (toNSString value) (toMTRWriteParams params) completionHandler

-- | @- subscribeAttributeActiveTextWithMinInterval:maxInterval:params:subscriptionEstablished:reportHandler:@
subscribeAttributeActiveTextWithMinInterval_maxInterval_params_subscriptionEstablished_reportHandler :: (IsMTRBaseClusterBinaryInputBasic mtrBaseClusterBinaryInputBasic, IsNSNumber minInterval, IsNSNumber maxInterval, IsMTRSubscribeParams params) => mtrBaseClusterBinaryInputBasic -> minInterval -> maxInterval -> params -> Ptr () -> Ptr () -> IO ()
subscribeAttributeActiveTextWithMinInterval_maxInterval_params_subscriptionEstablished_reportHandler mtrBaseClusterBinaryInputBasic minInterval maxInterval params subscriptionEstablishedHandler reportHandler =
  sendMessage mtrBaseClusterBinaryInputBasic subscribeAttributeActiveTextWithMinInterval_maxInterval_params_subscriptionEstablished_reportHandlerSelector (toNSNumber minInterval) (toNSNumber maxInterval) (toMTRSubscribeParams params) subscriptionEstablishedHandler reportHandler

-- | @+ readAttributeActiveTextWithAttributeCache:endpoint:queue:completionHandler:@
readAttributeActiveTextWithAttributeCache_endpoint_queue_completionHandler :: (IsMTRAttributeCacheContainer attributeCacheContainer, IsNSNumber endpoint, IsNSObject queue) => attributeCacheContainer -> endpoint -> queue -> Ptr () -> IO ()
readAttributeActiveTextWithAttributeCache_endpoint_queue_completionHandler attributeCacheContainer endpoint queue completionHandler =
  do
    cls' <- getRequiredClass "MTRBaseClusterBinaryInputBasic"
    sendClassMessage cls' readAttributeActiveTextWithAttributeCache_endpoint_queue_completionHandlerSelector (toMTRAttributeCacheContainer attributeCacheContainer) (toNSNumber endpoint) (toNSObject queue) completionHandler

-- | @- readAttributeDescriptionWithCompletionHandler:@
readAttributeDescriptionWithCompletionHandler :: IsMTRBaseClusterBinaryInputBasic mtrBaseClusterBinaryInputBasic => mtrBaseClusterBinaryInputBasic -> Ptr () -> IO ()
readAttributeDescriptionWithCompletionHandler mtrBaseClusterBinaryInputBasic completionHandler =
  sendMessage mtrBaseClusterBinaryInputBasic readAttributeDescriptionWithCompletionHandlerSelector completionHandler

-- | @- writeAttributeDescriptionWithValue:completionHandler:@
writeAttributeDescriptionWithValue_completionHandler :: (IsMTRBaseClusterBinaryInputBasic mtrBaseClusterBinaryInputBasic, IsNSString value) => mtrBaseClusterBinaryInputBasic -> value -> Ptr () -> IO ()
writeAttributeDescriptionWithValue_completionHandler mtrBaseClusterBinaryInputBasic value completionHandler =
  sendMessage mtrBaseClusterBinaryInputBasic writeAttributeDescriptionWithValue_completionHandlerSelector (toNSString value) completionHandler

-- | @- writeAttributeDescriptionWithValue:params:completionHandler:@
writeAttributeDescriptionWithValue_params_completionHandler :: (IsMTRBaseClusterBinaryInputBasic mtrBaseClusterBinaryInputBasic, IsNSString value, IsMTRWriteParams params) => mtrBaseClusterBinaryInputBasic -> value -> params -> Ptr () -> IO ()
writeAttributeDescriptionWithValue_params_completionHandler mtrBaseClusterBinaryInputBasic value params completionHandler =
  sendMessage mtrBaseClusterBinaryInputBasic writeAttributeDescriptionWithValue_params_completionHandlerSelector (toNSString value) (toMTRWriteParams params) completionHandler

-- | @- subscribeAttributeDescriptionWithMinInterval:maxInterval:params:subscriptionEstablished:reportHandler:@
subscribeAttributeDescriptionWithMinInterval_maxInterval_params_subscriptionEstablished_reportHandler :: (IsMTRBaseClusterBinaryInputBasic mtrBaseClusterBinaryInputBasic, IsNSNumber minInterval, IsNSNumber maxInterval, IsMTRSubscribeParams params) => mtrBaseClusterBinaryInputBasic -> minInterval -> maxInterval -> params -> Ptr () -> Ptr () -> IO ()
subscribeAttributeDescriptionWithMinInterval_maxInterval_params_subscriptionEstablished_reportHandler mtrBaseClusterBinaryInputBasic minInterval maxInterval params subscriptionEstablishedHandler reportHandler =
  sendMessage mtrBaseClusterBinaryInputBasic subscribeAttributeDescriptionWithMinInterval_maxInterval_params_subscriptionEstablished_reportHandlerSelector (toNSNumber minInterval) (toNSNumber maxInterval) (toMTRSubscribeParams params) subscriptionEstablishedHandler reportHandler

-- | @+ readAttributeDescriptionWithAttributeCache:endpoint:queue:completionHandler:@
readAttributeDescriptionWithAttributeCache_endpoint_queue_completionHandler :: (IsMTRAttributeCacheContainer attributeCacheContainer, IsNSNumber endpoint, IsNSObject queue) => attributeCacheContainer -> endpoint -> queue -> Ptr () -> IO ()
readAttributeDescriptionWithAttributeCache_endpoint_queue_completionHandler attributeCacheContainer endpoint queue completionHandler =
  do
    cls' <- getRequiredClass "MTRBaseClusterBinaryInputBasic"
    sendClassMessage cls' readAttributeDescriptionWithAttributeCache_endpoint_queue_completionHandlerSelector (toMTRAttributeCacheContainer attributeCacheContainer) (toNSNumber endpoint) (toNSObject queue) completionHandler

-- | @- readAttributeInactiveTextWithCompletionHandler:@
readAttributeInactiveTextWithCompletionHandler :: IsMTRBaseClusterBinaryInputBasic mtrBaseClusterBinaryInputBasic => mtrBaseClusterBinaryInputBasic -> Ptr () -> IO ()
readAttributeInactiveTextWithCompletionHandler mtrBaseClusterBinaryInputBasic completionHandler =
  sendMessage mtrBaseClusterBinaryInputBasic readAttributeInactiveTextWithCompletionHandlerSelector completionHandler

-- | @- writeAttributeInactiveTextWithValue:completionHandler:@
writeAttributeInactiveTextWithValue_completionHandler :: (IsMTRBaseClusterBinaryInputBasic mtrBaseClusterBinaryInputBasic, IsNSString value) => mtrBaseClusterBinaryInputBasic -> value -> Ptr () -> IO ()
writeAttributeInactiveTextWithValue_completionHandler mtrBaseClusterBinaryInputBasic value completionHandler =
  sendMessage mtrBaseClusterBinaryInputBasic writeAttributeInactiveTextWithValue_completionHandlerSelector (toNSString value) completionHandler

-- | @- writeAttributeInactiveTextWithValue:params:completionHandler:@
writeAttributeInactiveTextWithValue_params_completionHandler :: (IsMTRBaseClusterBinaryInputBasic mtrBaseClusterBinaryInputBasic, IsNSString value, IsMTRWriteParams params) => mtrBaseClusterBinaryInputBasic -> value -> params -> Ptr () -> IO ()
writeAttributeInactiveTextWithValue_params_completionHandler mtrBaseClusterBinaryInputBasic value params completionHandler =
  sendMessage mtrBaseClusterBinaryInputBasic writeAttributeInactiveTextWithValue_params_completionHandlerSelector (toNSString value) (toMTRWriteParams params) completionHandler

-- | @- subscribeAttributeInactiveTextWithMinInterval:maxInterval:params:subscriptionEstablished:reportHandler:@
subscribeAttributeInactiveTextWithMinInterval_maxInterval_params_subscriptionEstablished_reportHandler :: (IsMTRBaseClusterBinaryInputBasic mtrBaseClusterBinaryInputBasic, IsNSNumber minInterval, IsNSNumber maxInterval, IsMTRSubscribeParams params) => mtrBaseClusterBinaryInputBasic -> minInterval -> maxInterval -> params -> Ptr () -> Ptr () -> IO ()
subscribeAttributeInactiveTextWithMinInterval_maxInterval_params_subscriptionEstablished_reportHandler mtrBaseClusterBinaryInputBasic minInterval maxInterval params subscriptionEstablishedHandler reportHandler =
  sendMessage mtrBaseClusterBinaryInputBasic subscribeAttributeInactiveTextWithMinInterval_maxInterval_params_subscriptionEstablished_reportHandlerSelector (toNSNumber minInterval) (toNSNumber maxInterval) (toMTRSubscribeParams params) subscriptionEstablishedHandler reportHandler

-- | @+ readAttributeInactiveTextWithAttributeCache:endpoint:queue:completionHandler:@
readAttributeInactiveTextWithAttributeCache_endpoint_queue_completionHandler :: (IsMTRAttributeCacheContainer attributeCacheContainer, IsNSNumber endpoint, IsNSObject queue) => attributeCacheContainer -> endpoint -> queue -> Ptr () -> IO ()
readAttributeInactiveTextWithAttributeCache_endpoint_queue_completionHandler attributeCacheContainer endpoint queue completionHandler =
  do
    cls' <- getRequiredClass "MTRBaseClusterBinaryInputBasic"
    sendClassMessage cls' readAttributeInactiveTextWithAttributeCache_endpoint_queue_completionHandlerSelector (toMTRAttributeCacheContainer attributeCacheContainer) (toNSNumber endpoint) (toNSObject queue) completionHandler

-- | @- readAttributeOutOfServiceWithCompletionHandler:@
readAttributeOutOfServiceWithCompletionHandler :: IsMTRBaseClusterBinaryInputBasic mtrBaseClusterBinaryInputBasic => mtrBaseClusterBinaryInputBasic -> Ptr () -> IO ()
readAttributeOutOfServiceWithCompletionHandler mtrBaseClusterBinaryInputBasic completionHandler =
  sendMessage mtrBaseClusterBinaryInputBasic readAttributeOutOfServiceWithCompletionHandlerSelector completionHandler

-- | @- writeAttributeOutOfServiceWithValue:completionHandler:@
writeAttributeOutOfServiceWithValue_completionHandler :: (IsMTRBaseClusterBinaryInputBasic mtrBaseClusterBinaryInputBasic, IsNSNumber value) => mtrBaseClusterBinaryInputBasic -> value -> Ptr () -> IO ()
writeAttributeOutOfServiceWithValue_completionHandler mtrBaseClusterBinaryInputBasic value completionHandler =
  sendMessage mtrBaseClusterBinaryInputBasic writeAttributeOutOfServiceWithValue_completionHandlerSelector (toNSNumber value) completionHandler

-- | @- writeAttributeOutOfServiceWithValue:params:completionHandler:@
writeAttributeOutOfServiceWithValue_params_completionHandler :: (IsMTRBaseClusterBinaryInputBasic mtrBaseClusterBinaryInputBasic, IsNSNumber value, IsMTRWriteParams params) => mtrBaseClusterBinaryInputBasic -> value -> params -> Ptr () -> IO ()
writeAttributeOutOfServiceWithValue_params_completionHandler mtrBaseClusterBinaryInputBasic value params completionHandler =
  sendMessage mtrBaseClusterBinaryInputBasic writeAttributeOutOfServiceWithValue_params_completionHandlerSelector (toNSNumber value) (toMTRWriteParams params) completionHandler

-- | @- subscribeAttributeOutOfServiceWithMinInterval:maxInterval:params:subscriptionEstablished:reportHandler:@
subscribeAttributeOutOfServiceWithMinInterval_maxInterval_params_subscriptionEstablished_reportHandler :: (IsMTRBaseClusterBinaryInputBasic mtrBaseClusterBinaryInputBasic, IsNSNumber minInterval, IsNSNumber maxInterval, IsMTRSubscribeParams params) => mtrBaseClusterBinaryInputBasic -> minInterval -> maxInterval -> params -> Ptr () -> Ptr () -> IO ()
subscribeAttributeOutOfServiceWithMinInterval_maxInterval_params_subscriptionEstablished_reportHandler mtrBaseClusterBinaryInputBasic minInterval maxInterval params subscriptionEstablishedHandler reportHandler =
  sendMessage mtrBaseClusterBinaryInputBasic subscribeAttributeOutOfServiceWithMinInterval_maxInterval_params_subscriptionEstablished_reportHandlerSelector (toNSNumber minInterval) (toNSNumber maxInterval) (toMTRSubscribeParams params) subscriptionEstablishedHandler reportHandler

-- | @+ readAttributeOutOfServiceWithAttributeCache:endpoint:queue:completionHandler:@
readAttributeOutOfServiceWithAttributeCache_endpoint_queue_completionHandler :: (IsMTRAttributeCacheContainer attributeCacheContainer, IsNSNumber endpoint, IsNSObject queue) => attributeCacheContainer -> endpoint -> queue -> Ptr () -> IO ()
readAttributeOutOfServiceWithAttributeCache_endpoint_queue_completionHandler attributeCacheContainer endpoint queue completionHandler =
  do
    cls' <- getRequiredClass "MTRBaseClusterBinaryInputBasic"
    sendClassMessage cls' readAttributeOutOfServiceWithAttributeCache_endpoint_queue_completionHandlerSelector (toMTRAttributeCacheContainer attributeCacheContainer) (toNSNumber endpoint) (toNSObject queue) completionHandler

-- | @- readAttributePolarityWithCompletionHandler:@
readAttributePolarityWithCompletionHandler :: IsMTRBaseClusterBinaryInputBasic mtrBaseClusterBinaryInputBasic => mtrBaseClusterBinaryInputBasic -> Ptr () -> IO ()
readAttributePolarityWithCompletionHandler mtrBaseClusterBinaryInputBasic completionHandler =
  sendMessage mtrBaseClusterBinaryInputBasic readAttributePolarityWithCompletionHandlerSelector completionHandler

-- | @- subscribeAttributePolarityWithMinInterval:maxInterval:params:subscriptionEstablished:reportHandler:@
subscribeAttributePolarityWithMinInterval_maxInterval_params_subscriptionEstablished_reportHandler :: (IsMTRBaseClusterBinaryInputBasic mtrBaseClusterBinaryInputBasic, IsNSNumber minInterval, IsNSNumber maxInterval, IsMTRSubscribeParams params) => mtrBaseClusterBinaryInputBasic -> minInterval -> maxInterval -> params -> Ptr () -> Ptr () -> IO ()
subscribeAttributePolarityWithMinInterval_maxInterval_params_subscriptionEstablished_reportHandler mtrBaseClusterBinaryInputBasic minInterval maxInterval params subscriptionEstablishedHandler reportHandler =
  sendMessage mtrBaseClusterBinaryInputBasic subscribeAttributePolarityWithMinInterval_maxInterval_params_subscriptionEstablished_reportHandlerSelector (toNSNumber minInterval) (toNSNumber maxInterval) (toMTRSubscribeParams params) subscriptionEstablishedHandler reportHandler

-- | @+ readAttributePolarityWithAttributeCache:endpoint:queue:completionHandler:@
readAttributePolarityWithAttributeCache_endpoint_queue_completionHandler :: (IsMTRAttributeCacheContainer attributeCacheContainer, IsNSNumber endpoint, IsNSObject queue) => attributeCacheContainer -> endpoint -> queue -> Ptr () -> IO ()
readAttributePolarityWithAttributeCache_endpoint_queue_completionHandler attributeCacheContainer endpoint queue completionHandler =
  do
    cls' <- getRequiredClass "MTRBaseClusterBinaryInputBasic"
    sendClassMessage cls' readAttributePolarityWithAttributeCache_endpoint_queue_completionHandlerSelector (toMTRAttributeCacheContainer attributeCacheContainer) (toNSNumber endpoint) (toNSObject queue) completionHandler

-- | @- readAttributePresentValueWithCompletionHandler:@
readAttributePresentValueWithCompletionHandler :: IsMTRBaseClusterBinaryInputBasic mtrBaseClusterBinaryInputBasic => mtrBaseClusterBinaryInputBasic -> Ptr () -> IO ()
readAttributePresentValueWithCompletionHandler mtrBaseClusterBinaryInputBasic completionHandler =
  sendMessage mtrBaseClusterBinaryInputBasic readAttributePresentValueWithCompletionHandlerSelector completionHandler

-- | @- writeAttributePresentValueWithValue:completionHandler:@
writeAttributePresentValueWithValue_completionHandler :: (IsMTRBaseClusterBinaryInputBasic mtrBaseClusterBinaryInputBasic, IsNSNumber value) => mtrBaseClusterBinaryInputBasic -> value -> Ptr () -> IO ()
writeAttributePresentValueWithValue_completionHandler mtrBaseClusterBinaryInputBasic value completionHandler =
  sendMessage mtrBaseClusterBinaryInputBasic writeAttributePresentValueWithValue_completionHandlerSelector (toNSNumber value) completionHandler

-- | @- writeAttributePresentValueWithValue:params:completionHandler:@
writeAttributePresentValueWithValue_params_completionHandler :: (IsMTRBaseClusterBinaryInputBasic mtrBaseClusterBinaryInputBasic, IsNSNumber value, IsMTRWriteParams params) => mtrBaseClusterBinaryInputBasic -> value -> params -> Ptr () -> IO ()
writeAttributePresentValueWithValue_params_completionHandler mtrBaseClusterBinaryInputBasic value params completionHandler =
  sendMessage mtrBaseClusterBinaryInputBasic writeAttributePresentValueWithValue_params_completionHandlerSelector (toNSNumber value) (toMTRWriteParams params) completionHandler

-- | @- subscribeAttributePresentValueWithMinInterval:maxInterval:params:subscriptionEstablished:reportHandler:@
subscribeAttributePresentValueWithMinInterval_maxInterval_params_subscriptionEstablished_reportHandler :: (IsMTRBaseClusterBinaryInputBasic mtrBaseClusterBinaryInputBasic, IsNSNumber minInterval, IsNSNumber maxInterval, IsMTRSubscribeParams params) => mtrBaseClusterBinaryInputBasic -> minInterval -> maxInterval -> params -> Ptr () -> Ptr () -> IO ()
subscribeAttributePresentValueWithMinInterval_maxInterval_params_subscriptionEstablished_reportHandler mtrBaseClusterBinaryInputBasic minInterval maxInterval params subscriptionEstablishedHandler reportHandler =
  sendMessage mtrBaseClusterBinaryInputBasic subscribeAttributePresentValueWithMinInterval_maxInterval_params_subscriptionEstablished_reportHandlerSelector (toNSNumber minInterval) (toNSNumber maxInterval) (toMTRSubscribeParams params) subscriptionEstablishedHandler reportHandler

-- | @+ readAttributePresentValueWithAttributeCache:endpoint:queue:completionHandler:@
readAttributePresentValueWithAttributeCache_endpoint_queue_completionHandler :: (IsMTRAttributeCacheContainer attributeCacheContainer, IsNSNumber endpoint, IsNSObject queue) => attributeCacheContainer -> endpoint -> queue -> Ptr () -> IO ()
readAttributePresentValueWithAttributeCache_endpoint_queue_completionHandler attributeCacheContainer endpoint queue completionHandler =
  do
    cls' <- getRequiredClass "MTRBaseClusterBinaryInputBasic"
    sendClassMessage cls' readAttributePresentValueWithAttributeCache_endpoint_queue_completionHandlerSelector (toMTRAttributeCacheContainer attributeCacheContainer) (toNSNumber endpoint) (toNSObject queue) completionHandler

-- | @- readAttributeReliabilityWithCompletionHandler:@
readAttributeReliabilityWithCompletionHandler :: IsMTRBaseClusterBinaryInputBasic mtrBaseClusterBinaryInputBasic => mtrBaseClusterBinaryInputBasic -> Ptr () -> IO ()
readAttributeReliabilityWithCompletionHandler mtrBaseClusterBinaryInputBasic completionHandler =
  sendMessage mtrBaseClusterBinaryInputBasic readAttributeReliabilityWithCompletionHandlerSelector completionHandler

-- | @- writeAttributeReliabilityWithValue:completionHandler:@
writeAttributeReliabilityWithValue_completionHandler :: (IsMTRBaseClusterBinaryInputBasic mtrBaseClusterBinaryInputBasic, IsNSNumber value) => mtrBaseClusterBinaryInputBasic -> value -> Ptr () -> IO ()
writeAttributeReliabilityWithValue_completionHandler mtrBaseClusterBinaryInputBasic value completionHandler =
  sendMessage mtrBaseClusterBinaryInputBasic writeAttributeReliabilityWithValue_completionHandlerSelector (toNSNumber value) completionHandler

-- | @- writeAttributeReliabilityWithValue:params:completionHandler:@
writeAttributeReliabilityWithValue_params_completionHandler :: (IsMTRBaseClusterBinaryInputBasic mtrBaseClusterBinaryInputBasic, IsNSNumber value, IsMTRWriteParams params) => mtrBaseClusterBinaryInputBasic -> value -> params -> Ptr () -> IO ()
writeAttributeReliabilityWithValue_params_completionHandler mtrBaseClusterBinaryInputBasic value params completionHandler =
  sendMessage mtrBaseClusterBinaryInputBasic writeAttributeReliabilityWithValue_params_completionHandlerSelector (toNSNumber value) (toMTRWriteParams params) completionHandler

-- | @- subscribeAttributeReliabilityWithMinInterval:maxInterval:params:subscriptionEstablished:reportHandler:@
subscribeAttributeReliabilityWithMinInterval_maxInterval_params_subscriptionEstablished_reportHandler :: (IsMTRBaseClusterBinaryInputBasic mtrBaseClusterBinaryInputBasic, IsNSNumber minInterval, IsNSNumber maxInterval, IsMTRSubscribeParams params) => mtrBaseClusterBinaryInputBasic -> minInterval -> maxInterval -> params -> Ptr () -> Ptr () -> IO ()
subscribeAttributeReliabilityWithMinInterval_maxInterval_params_subscriptionEstablished_reportHandler mtrBaseClusterBinaryInputBasic minInterval maxInterval params subscriptionEstablishedHandler reportHandler =
  sendMessage mtrBaseClusterBinaryInputBasic subscribeAttributeReliabilityWithMinInterval_maxInterval_params_subscriptionEstablished_reportHandlerSelector (toNSNumber minInterval) (toNSNumber maxInterval) (toMTRSubscribeParams params) subscriptionEstablishedHandler reportHandler

-- | @+ readAttributeReliabilityWithAttributeCache:endpoint:queue:completionHandler:@
readAttributeReliabilityWithAttributeCache_endpoint_queue_completionHandler :: (IsMTRAttributeCacheContainer attributeCacheContainer, IsNSNumber endpoint, IsNSObject queue) => attributeCacheContainer -> endpoint -> queue -> Ptr () -> IO ()
readAttributeReliabilityWithAttributeCache_endpoint_queue_completionHandler attributeCacheContainer endpoint queue completionHandler =
  do
    cls' <- getRequiredClass "MTRBaseClusterBinaryInputBasic"
    sendClassMessage cls' readAttributeReliabilityWithAttributeCache_endpoint_queue_completionHandlerSelector (toMTRAttributeCacheContainer attributeCacheContainer) (toNSNumber endpoint) (toNSObject queue) completionHandler

-- | @- readAttributeStatusFlagsWithCompletionHandler:@
readAttributeStatusFlagsWithCompletionHandler :: IsMTRBaseClusterBinaryInputBasic mtrBaseClusterBinaryInputBasic => mtrBaseClusterBinaryInputBasic -> Ptr () -> IO ()
readAttributeStatusFlagsWithCompletionHandler mtrBaseClusterBinaryInputBasic completionHandler =
  sendMessage mtrBaseClusterBinaryInputBasic readAttributeStatusFlagsWithCompletionHandlerSelector completionHandler

-- | @- subscribeAttributeStatusFlagsWithMinInterval:maxInterval:params:subscriptionEstablished:reportHandler:@
subscribeAttributeStatusFlagsWithMinInterval_maxInterval_params_subscriptionEstablished_reportHandler :: (IsMTRBaseClusterBinaryInputBasic mtrBaseClusterBinaryInputBasic, IsNSNumber minInterval, IsNSNumber maxInterval, IsMTRSubscribeParams params) => mtrBaseClusterBinaryInputBasic -> minInterval -> maxInterval -> params -> Ptr () -> Ptr () -> IO ()
subscribeAttributeStatusFlagsWithMinInterval_maxInterval_params_subscriptionEstablished_reportHandler mtrBaseClusterBinaryInputBasic minInterval maxInterval params subscriptionEstablishedHandler reportHandler =
  sendMessage mtrBaseClusterBinaryInputBasic subscribeAttributeStatusFlagsWithMinInterval_maxInterval_params_subscriptionEstablished_reportHandlerSelector (toNSNumber minInterval) (toNSNumber maxInterval) (toMTRSubscribeParams params) subscriptionEstablishedHandler reportHandler

-- | @+ readAttributeStatusFlagsWithAttributeCache:endpoint:queue:completionHandler:@
readAttributeStatusFlagsWithAttributeCache_endpoint_queue_completionHandler :: (IsMTRAttributeCacheContainer attributeCacheContainer, IsNSNumber endpoint, IsNSObject queue) => attributeCacheContainer -> endpoint -> queue -> Ptr () -> IO ()
readAttributeStatusFlagsWithAttributeCache_endpoint_queue_completionHandler attributeCacheContainer endpoint queue completionHandler =
  do
    cls' <- getRequiredClass "MTRBaseClusterBinaryInputBasic"
    sendClassMessage cls' readAttributeStatusFlagsWithAttributeCache_endpoint_queue_completionHandlerSelector (toMTRAttributeCacheContainer attributeCacheContainer) (toNSNumber endpoint) (toNSObject queue) completionHandler

-- | @- readAttributeApplicationTypeWithCompletionHandler:@
readAttributeApplicationTypeWithCompletionHandler :: IsMTRBaseClusterBinaryInputBasic mtrBaseClusterBinaryInputBasic => mtrBaseClusterBinaryInputBasic -> Ptr () -> IO ()
readAttributeApplicationTypeWithCompletionHandler mtrBaseClusterBinaryInputBasic completionHandler =
  sendMessage mtrBaseClusterBinaryInputBasic readAttributeApplicationTypeWithCompletionHandlerSelector completionHandler

-- | @- subscribeAttributeApplicationTypeWithMinInterval:maxInterval:params:subscriptionEstablished:reportHandler:@
subscribeAttributeApplicationTypeWithMinInterval_maxInterval_params_subscriptionEstablished_reportHandler :: (IsMTRBaseClusterBinaryInputBasic mtrBaseClusterBinaryInputBasic, IsNSNumber minInterval, IsNSNumber maxInterval, IsMTRSubscribeParams params) => mtrBaseClusterBinaryInputBasic -> minInterval -> maxInterval -> params -> Ptr () -> Ptr () -> IO ()
subscribeAttributeApplicationTypeWithMinInterval_maxInterval_params_subscriptionEstablished_reportHandler mtrBaseClusterBinaryInputBasic minInterval maxInterval params subscriptionEstablishedHandler reportHandler =
  sendMessage mtrBaseClusterBinaryInputBasic subscribeAttributeApplicationTypeWithMinInterval_maxInterval_params_subscriptionEstablished_reportHandlerSelector (toNSNumber minInterval) (toNSNumber maxInterval) (toMTRSubscribeParams params) subscriptionEstablishedHandler reportHandler

-- | @+ readAttributeApplicationTypeWithAttributeCache:endpoint:queue:completionHandler:@
readAttributeApplicationTypeWithAttributeCache_endpoint_queue_completionHandler :: (IsMTRAttributeCacheContainer attributeCacheContainer, IsNSNumber endpoint, IsNSObject queue) => attributeCacheContainer -> endpoint -> queue -> Ptr () -> IO ()
readAttributeApplicationTypeWithAttributeCache_endpoint_queue_completionHandler attributeCacheContainer endpoint queue completionHandler =
  do
    cls' <- getRequiredClass "MTRBaseClusterBinaryInputBasic"
    sendClassMessage cls' readAttributeApplicationTypeWithAttributeCache_endpoint_queue_completionHandlerSelector (toMTRAttributeCacheContainer attributeCacheContainer) (toNSNumber endpoint) (toNSObject queue) completionHandler

-- | @- readAttributeGeneratedCommandListWithCompletionHandler:@
readAttributeGeneratedCommandListWithCompletionHandler :: IsMTRBaseClusterBinaryInputBasic mtrBaseClusterBinaryInputBasic => mtrBaseClusterBinaryInputBasic -> Ptr () -> IO ()
readAttributeGeneratedCommandListWithCompletionHandler mtrBaseClusterBinaryInputBasic completionHandler =
  sendMessage mtrBaseClusterBinaryInputBasic readAttributeGeneratedCommandListWithCompletionHandlerSelector completionHandler

-- | @- subscribeAttributeGeneratedCommandListWithMinInterval:maxInterval:params:subscriptionEstablished:reportHandler:@
subscribeAttributeGeneratedCommandListWithMinInterval_maxInterval_params_subscriptionEstablished_reportHandler :: (IsMTRBaseClusterBinaryInputBasic mtrBaseClusterBinaryInputBasic, IsNSNumber minInterval, IsNSNumber maxInterval, IsMTRSubscribeParams params) => mtrBaseClusterBinaryInputBasic -> minInterval -> maxInterval -> params -> Ptr () -> Ptr () -> IO ()
subscribeAttributeGeneratedCommandListWithMinInterval_maxInterval_params_subscriptionEstablished_reportHandler mtrBaseClusterBinaryInputBasic minInterval maxInterval params subscriptionEstablishedHandler reportHandler =
  sendMessage mtrBaseClusterBinaryInputBasic subscribeAttributeGeneratedCommandListWithMinInterval_maxInterval_params_subscriptionEstablished_reportHandlerSelector (toNSNumber minInterval) (toNSNumber maxInterval) (toMTRSubscribeParams params) subscriptionEstablishedHandler reportHandler

-- | @+ readAttributeGeneratedCommandListWithAttributeCache:endpoint:queue:completionHandler:@
readAttributeGeneratedCommandListWithAttributeCache_endpoint_queue_completionHandler :: (IsMTRAttributeCacheContainer attributeCacheContainer, IsNSNumber endpoint, IsNSObject queue) => attributeCacheContainer -> endpoint -> queue -> Ptr () -> IO ()
readAttributeGeneratedCommandListWithAttributeCache_endpoint_queue_completionHandler attributeCacheContainer endpoint queue completionHandler =
  do
    cls' <- getRequiredClass "MTRBaseClusterBinaryInputBasic"
    sendClassMessage cls' readAttributeGeneratedCommandListWithAttributeCache_endpoint_queue_completionHandlerSelector (toMTRAttributeCacheContainer attributeCacheContainer) (toNSNumber endpoint) (toNSObject queue) completionHandler

-- | @- readAttributeAcceptedCommandListWithCompletionHandler:@
readAttributeAcceptedCommandListWithCompletionHandler :: IsMTRBaseClusterBinaryInputBasic mtrBaseClusterBinaryInputBasic => mtrBaseClusterBinaryInputBasic -> Ptr () -> IO ()
readAttributeAcceptedCommandListWithCompletionHandler mtrBaseClusterBinaryInputBasic completionHandler =
  sendMessage mtrBaseClusterBinaryInputBasic readAttributeAcceptedCommandListWithCompletionHandlerSelector completionHandler

-- | @- subscribeAttributeAcceptedCommandListWithMinInterval:maxInterval:params:subscriptionEstablished:reportHandler:@
subscribeAttributeAcceptedCommandListWithMinInterval_maxInterval_params_subscriptionEstablished_reportHandler :: (IsMTRBaseClusterBinaryInputBasic mtrBaseClusterBinaryInputBasic, IsNSNumber minInterval, IsNSNumber maxInterval, IsMTRSubscribeParams params) => mtrBaseClusterBinaryInputBasic -> minInterval -> maxInterval -> params -> Ptr () -> Ptr () -> IO ()
subscribeAttributeAcceptedCommandListWithMinInterval_maxInterval_params_subscriptionEstablished_reportHandler mtrBaseClusterBinaryInputBasic minInterval maxInterval params subscriptionEstablishedHandler reportHandler =
  sendMessage mtrBaseClusterBinaryInputBasic subscribeAttributeAcceptedCommandListWithMinInterval_maxInterval_params_subscriptionEstablished_reportHandlerSelector (toNSNumber minInterval) (toNSNumber maxInterval) (toMTRSubscribeParams params) subscriptionEstablishedHandler reportHandler

-- | @+ readAttributeAcceptedCommandListWithAttributeCache:endpoint:queue:completionHandler:@
readAttributeAcceptedCommandListWithAttributeCache_endpoint_queue_completionHandler :: (IsMTRAttributeCacheContainer attributeCacheContainer, IsNSNumber endpoint, IsNSObject queue) => attributeCacheContainer -> endpoint -> queue -> Ptr () -> IO ()
readAttributeAcceptedCommandListWithAttributeCache_endpoint_queue_completionHandler attributeCacheContainer endpoint queue completionHandler =
  do
    cls' <- getRequiredClass "MTRBaseClusterBinaryInputBasic"
    sendClassMessage cls' readAttributeAcceptedCommandListWithAttributeCache_endpoint_queue_completionHandlerSelector (toMTRAttributeCacheContainer attributeCacheContainer) (toNSNumber endpoint) (toNSObject queue) completionHandler

-- | @- readAttributeAttributeListWithCompletionHandler:@
readAttributeAttributeListWithCompletionHandler :: IsMTRBaseClusterBinaryInputBasic mtrBaseClusterBinaryInputBasic => mtrBaseClusterBinaryInputBasic -> Ptr () -> IO ()
readAttributeAttributeListWithCompletionHandler mtrBaseClusterBinaryInputBasic completionHandler =
  sendMessage mtrBaseClusterBinaryInputBasic readAttributeAttributeListWithCompletionHandlerSelector completionHandler

-- | @- subscribeAttributeAttributeListWithMinInterval:maxInterval:params:subscriptionEstablished:reportHandler:@
subscribeAttributeAttributeListWithMinInterval_maxInterval_params_subscriptionEstablished_reportHandler :: (IsMTRBaseClusterBinaryInputBasic mtrBaseClusterBinaryInputBasic, IsNSNumber minInterval, IsNSNumber maxInterval, IsMTRSubscribeParams params) => mtrBaseClusterBinaryInputBasic -> minInterval -> maxInterval -> params -> Ptr () -> Ptr () -> IO ()
subscribeAttributeAttributeListWithMinInterval_maxInterval_params_subscriptionEstablished_reportHandler mtrBaseClusterBinaryInputBasic minInterval maxInterval params subscriptionEstablishedHandler reportHandler =
  sendMessage mtrBaseClusterBinaryInputBasic subscribeAttributeAttributeListWithMinInterval_maxInterval_params_subscriptionEstablished_reportHandlerSelector (toNSNumber minInterval) (toNSNumber maxInterval) (toMTRSubscribeParams params) subscriptionEstablishedHandler reportHandler

-- | @+ readAttributeAttributeListWithAttributeCache:endpoint:queue:completionHandler:@
readAttributeAttributeListWithAttributeCache_endpoint_queue_completionHandler :: (IsMTRAttributeCacheContainer attributeCacheContainer, IsNSNumber endpoint, IsNSObject queue) => attributeCacheContainer -> endpoint -> queue -> Ptr () -> IO ()
readAttributeAttributeListWithAttributeCache_endpoint_queue_completionHandler attributeCacheContainer endpoint queue completionHandler =
  do
    cls' <- getRequiredClass "MTRBaseClusterBinaryInputBasic"
    sendClassMessage cls' readAttributeAttributeListWithAttributeCache_endpoint_queue_completionHandlerSelector (toMTRAttributeCacheContainer attributeCacheContainer) (toNSNumber endpoint) (toNSObject queue) completionHandler

-- | @- readAttributeFeatureMapWithCompletionHandler:@
readAttributeFeatureMapWithCompletionHandler :: IsMTRBaseClusterBinaryInputBasic mtrBaseClusterBinaryInputBasic => mtrBaseClusterBinaryInputBasic -> Ptr () -> IO ()
readAttributeFeatureMapWithCompletionHandler mtrBaseClusterBinaryInputBasic completionHandler =
  sendMessage mtrBaseClusterBinaryInputBasic readAttributeFeatureMapWithCompletionHandlerSelector completionHandler

-- | @- subscribeAttributeFeatureMapWithMinInterval:maxInterval:params:subscriptionEstablished:reportHandler:@
subscribeAttributeFeatureMapWithMinInterval_maxInterval_params_subscriptionEstablished_reportHandler :: (IsMTRBaseClusterBinaryInputBasic mtrBaseClusterBinaryInputBasic, IsNSNumber minInterval, IsNSNumber maxInterval, IsMTRSubscribeParams params) => mtrBaseClusterBinaryInputBasic -> minInterval -> maxInterval -> params -> Ptr () -> Ptr () -> IO ()
subscribeAttributeFeatureMapWithMinInterval_maxInterval_params_subscriptionEstablished_reportHandler mtrBaseClusterBinaryInputBasic minInterval maxInterval params subscriptionEstablishedHandler reportHandler =
  sendMessage mtrBaseClusterBinaryInputBasic subscribeAttributeFeatureMapWithMinInterval_maxInterval_params_subscriptionEstablished_reportHandlerSelector (toNSNumber minInterval) (toNSNumber maxInterval) (toMTRSubscribeParams params) subscriptionEstablishedHandler reportHandler

-- | @+ readAttributeFeatureMapWithAttributeCache:endpoint:queue:completionHandler:@
readAttributeFeatureMapWithAttributeCache_endpoint_queue_completionHandler :: (IsMTRAttributeCacheContainer attributeCacheContainer, IsNSNumber endpoint, IsNSObject queue) => attributeCacheContainer -> endpoint -> queue -> Ptr () -> IO ()
readAttributeFeatureMapWithAttributeCache_endpoint_queue_completionHandler attributeCacheContainer endpoint queue completionHandler =
  do
    cls' <- getRequiredClass "MTRBaseClusterBinaryInputBasic"
    sendClassMessage cls' readAttributeFeatureMapWithAttributeCache_endpoint_queue_completionHandlerSelector (toMTRAttributeCacheContainer attributeCacheContainer) (toNSNumber endpoint) (toNSObject queue) completionHandler

-- | @- readAttributeClusterRevisionWithCompletionHandler:@
readAttributeClusterRevisionWithCompletionHandler :: IsMTRBaseClusterBinaryInputBasic mtrBaseClusterBinaryInputBasic => mtrBaseClusterBinaryInputBasic -> Ptr () -> IO ()
readAttributeClusterRevisionWithCompletionHandler mtrBaseClusterBinaryInputBasic completionHandler =
  sendMessage mtrBaseClusterBinaryInputBasic readAttributeClusterRevisionWithCompletionHandlerSelector completionHandler

-- | @- subscribeAttributeClusterRevisionWithMinInterval:maxInterval:params:subscriptionEstablished:reportHandler:@
subscribeAttributeClusterRevisionWithMinInterval_maxInterval_params_subscriptionEstablished_reportHandler :: (IsMTRBaseClusterBinaryInputBasic mtrBaseClusterBinaryInputBasic, IsNSNumber minInterval, IsNSNumber maxInterval, IsMTRSubscribeParams params) => mtrBaseClusterBinaryInputBasic -> minInterval -> maxInterval -> params -> Ptr () -> Ptr () -> IO ()
subscribeAttributeClusterRevisionWithMinInterval_maxInterval_params_subscriptionEstablished_reportHandler mtrBaseClusterBinaryInputBasic minInterval maxInterval params subscriptionEstablishedHandler reportHandler =
  sendMessage mtrBaseClusterBinaryInputBasic subscribeAttributeClusterRevisionWithMinInterval_maxInterval_params_subscriptionEstablished_reportHandlerSelector (toNSNumber minInterval) (toNSNumber maxInterval) (toMTRSubscribeParams params) subscriptionEstablishedHandler reportHandler

-- | @+ readAttributeClusterRevisionWithAttributeCache:endpoint:queue:completionHandler:@
readAttributeClusterRevisionWithAttributeCache_endpoint_queue_completionHandler :: (IsMTRAttributeCacheContainer attributeCacheContainer, IsNSNumber endpoint, IsNSObject queue) => attributeCacheContainer -> endpoint -> queue -> Ptr () -> IO ()
readAttributeClusterRevisionWithAttributeCache_endpoint_queue_completionHandler attributeCacheContainer endpoint queue completionHandler =
  do
    cls' <- getRequiredClass "MTRBaseClusterBinaryInputBasic"
    sendClassMessage cls' readAttributeClusterRevisionWithAttributeCache_endpoint_queue_completionHandlerSelector (toMTRAttributeCacheContainer attributeCacheContainer) (toNSNumber endpoint) (toNSObject queue) completionHandler

-- | For all instance methods (reads, writes, commands) that take a completion, the completion will be called on the provided queue.
--
-- ObjC selector: @- initWithDevice:endpointID:queue:@
initWithDevice_endpointID_queue :: (IsMTRBaseClusterBinaryInputBasic mtrBaseClusterBinaryInputBasic, IsMTRBaseDevice device, IsNSNumber endpointID, IsNSObject queue) => mtrBaseClusterBinaryInputBasic -> device -> endpointID -> queue -> IO (Id MTRBaseClusterBinaryInputBasic)
initWithDevice_endpointID_queue mtrBaseClusterBinaryInputBasic device endpointID queue =
  sendOwnedMessage mtrBaseClusterBinaryInputBasic initWithDevice_endpointID_queueSelector (toMTRBaseDevice device) (toNSNumber endpointID) (toNSObject queue)

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @readAttributeActiveTextWithCompletion:@
readAttributeActiveTextWithCompletionSelector :: Selector '[Ptr ()] ()
readAttributeActiveTextWithCompletionSelector = mkSelector "readAttributeActiveTextWithCompletion:"

-- | @Selector@ for @writeAttributeActiveTextWithValue:completion:@
writeAttributeActiveTextWithValue_completionSelector :: Selector '[Id NSString, Ptr ()] ()
writeAttributeActiveTextWithValue_completionSelector = mkSelector "writeAttributeActiveTextWithValue:completion:"

-- | @Selector@ for @writeAttributeActiveTextWithValue:params:completion:@
writeAttributeActiveTextWithValue_params_completionSelector :: Selector '[Id NSString, Id MTRWriteParams, Ptr ()] ()
writeAttributeActiveTextWithValue_params_completionSelector = mkSelector "writeAttributeActiveTextWithValue:params:completion:"

-- | @Selector@ for @subscribeAttributeActiveTextWithParams:subscriptionEstablished:reportHandler:@
subscribeAttributeActiveTextWithParams_subscriptionEstablished_reportHandlerSelector :: Selector '[Id MTRSubscribeParams, Ptr (), Ptr ()] ()
subscribeAttributeActiveTextWithParams_subscriptionEstablished_reportHandlerSelector = mkSelector "subscribeAttributeActiveTextWithParams:subscriptionEstablished:reportHandler:"

-- | @Selector@ for @readAttributeActiveTextWithClusterStateCache:endpoint:queue:completion:@
readAttributeActiveTextWithClusterStateCache_endpoint_queue_completionSelector :: Selector '[Id MTRClusterStateCacheContainer, Id NSNumber, Id NSObject, Ptr ()] ()
readAttributeActiveTextWithClusterStateCache_endpoint_queue_completionSelector = mkSelector "readAttributeActiveTextWithClusterStateCache:endpoint:queue:completion:"

-- | @Selector@ for @readAttributeDescriptionWithCompletion:@
readAttributeDescriptionWithCompletionSelector :: Selector '[Ptr ()] ()
readAttributeDescriptionWithCompletionSelector = mkSelector "readAttributeDescriptionWithCompletion:"

-- | @Selector@ for @writeAttributeDescriptionWithValue:completion:@
writeAttributeDescriptionWithValue_completionSelector :: Selector '[Id NSString, Ptr ()] ()
writeAttributeDescriptionWithValue_completionSelector = mkSelector "writeAttributeDescriptionWithValue:completion:"

-- | @Selector@ for @writeAttributeDescriptionWithValue:params:completion:@
writeAttributeDescriptionWithValue_params_completionSelector :: Selector '[Id NSString, Id MTRWriteParams, Ptr ()] ()
writeAttributeDescriptionWithValue_params_completionSelector = mkSelector "writeAttributeDescriptionWithValue:params:completion:"

-- | @Selector@ for @subscribeAttributeDescriptionWithParams:subscriptionEstablished:reportHandler:@
subscribeAttributeDescriptionWithParams_subscriptionEstablished_reportHandlerSelector :: Selector '[Id MTRSubscribeParams, Ptr (), Ptr ()] ()
subscribeAttributeDescriptionWithParams_subscriptionEstablished_reportHandlerSelector = mkSelector "subscribeAttributeDescriptionWithParams:subscriptionEstablished:reportHandler:"

-- | @Selector@ for @readAttributeDescriptionWithClusterStateCache:endpoint:queue:completion:@
readAttributeDescriptionWithClusterStateCache_endpoint_queue_completionSelector :: Selector '[Id MTRClusterStateCacheContainer, Id NSNumber, Id NSObject, Ptr ()] ()
readAttributeDescriptionWithClusterStateCache_endpoint_queue_completionSelector = mkSelector "readAttributeDescriptionWithClusterStateCache:endpoint:queue:completion:"

-- | @Selector@ for @readAttributeInactiveTextWithCompletion:@
readAttributeInactiveTextWithCompletionSelector :: Selector '[Ptr ()] ()
readAttributeInactiveTextWithCompletionSelector = mkSelector "readAttributeInactiveTextWithCompletion:"

-- | @Selector@ for @writeAttributeInactiveTextWithValue:completion:@
writeAttributeInactiveTextWithValue_completionSelector :: Selector '[Id NSString, Ptr ()] ()
writeAttributeInactiveTextWithValue_completionSelector = mkSelector "writeAttributeInactiveTextWithValue:completion:"

-- | @Selector@ for @writeAttributeInactiveTextWithValue:params:completion:@
writeAttributeInactiveTextWithValue_params_completionSelector :: Selector '[Id NSString, Id MTRWriteParams, Ptr ()] ()
writeAttributeInactiveTextWithValue_params_completionSelector = mkSelector "writeAttributeInactiveTextWithValue:params:completion:"

-- | @Selector@ for @subscribeAttributeInactiveTextWithParams:subscriptionEstablished:reportHandler:@
subscribeAttributeInactiveTextWithParams_subscriptionEstablished_reportHandlerSelector :: Selector '[Id MTRSubscribeParams, Ptr (), Ptr ()] ()
subscribeAttributeInactiveTextWithParams_subscriptionEstablished_reportHandlerSelector = mkSelector "subscribeAttributeInactiveTextWithParams:subscriptionEstablished:reportHandler:"

-- | @Selector@ for @readAttributeInactiveTextWithClusterStateCache:endpoint:queue:completion:@
readAttributeInactiveTextWithClusterStateCache_endpoint_queue_completionSelector :: Selector '[Id MTRClusterStateCacheContainer, Id NSNumber, Id NSObject, Ptr ()] ()
readAttributeInactiveTextWithClusterStateCache_endpoint_queue_completionSelector = mkSelector "readAttributeInactiveTextWithClusterStateCache:endpoint:queue:completion:"

-- | @Selector@ for @readAttributeOutOfServiceWithCompletion:@
readAttributeOutOfServiceWithCompletionSelector :: Selector '[Ptr ()] ()
readAttributeOutOfServiceWithCompletionSelector = mkSelector "readAttributeOutOfServiceWithCompletion:"

-- | @Selector@ for @writeAttributeOutOfServiceWithValue:completion:@
writeAttributeOutOfServiceWithValue_completionSelector :: Selector '[Id NSNumber, Ptr ()] ()
writeAttributeOutOfServiceWithValue_completionSelector = mkSelector "writeAttributeOutOfServiceWithValue:completion:"

-- | @Selector@ for @writeAttributeOutOfServiceWithValue:params:completion:@
writeAttributeOutOfServiceWithValue_params_completionSelector :: Selector '[Id NSNumber, Id MTRWriteParams, Ptr ()] ()
writeAttributeOutOfServiceWithValue_params_completionSelector = mkSelector "writeAttributeOutOfServiceWithValue:params:completion:"

-- | @Selector@ for @subscribeAttributeOutOfServiceWithParams:subscriptionEstablished:reportHandler:@
subscribeAttributeOutOfServiceWithParams_subscriptionEstablished_reportHandlerSelector :: Selector '[Id MTRSubscribeParams, Ptr (), Ptr ()] ()
subscribeAttributeOutOfServiceWithParams_subscriptionEstablished_reportHandlerSelector = mkSelector "subscribeAttributeOutOfServiceWithParams:subscriptionEstablished:reportHandler:"

-- | @Selector@ for @readAttributeOutOfServiceWithClusterStateCache:endpoint:queue:completion:@
readAttributeOutOfServiceWithClusterStateCache_endpoint_queue_completionSelector :: Selector '[Id MTRClusterStateCacheContainer, Id NSNumber, Id NSObject, Ptr ()] ()
readAttributeOutOfServiceWithClusterStateCache_endpoint_queue_completionSelector = mkSelector "readAttributeOutOfServiceWithClusterStateCache:endpoint:queue:completion:"

-- | @Selector@ for @readAttributePolarityWithCompletion:@
readAttributePolarityWithCompletionSelector :: Selector '[Ptr ()] ()
readAttributePolarityWithCompletionSelector = mkSelector "readAttributePolarityWithCompletion:"

-- | @Selector@ for @subscribeAttributePolarityWithParams:subscriptionEstablished:reportHandler:@
subscribeAttributePolarityWithParams_subscriptionEstablished_reportHandlerSelector :: Selector '[Id MTRSubscribeParams, Ptr (), Ptr ()] ()
subscribeAttributePolarityWithParams_subscriptionEstablished_reportHandlerSelector = mkSelector "subscribeAttributePolarityWithParams:subscriptionEstablished:reportHandler:"

-- | @Selector@ for @readAttributePolarityWithClusterStateCache:endpoint:queue:completion:@
readAttributePolarityWithClusterStateCache_endpoint_queue_completionSelector :: Selector '[Id MTRClusterStateCacheContainer, Id NSNumber, Id NSObject, Ptr ()] ()
readAttributePolarityWithClusterStateCache_endpoint_queue_completionSelector = mkSelector "readAttributePolarityWithClusterStateCache:endpoint:queue:completion:"

-- | @Selector@ for @readAttributePresentValueWithCompletion:@
readAttributePresentValueWithCompletionSelector :: Selector '[Ptr ()] ()
readAttributePresentValueWithCompletionSelector = mkSelector "readAttributePresentValueWithCompletion:"

-- | @Selector@ for @writeAttributePresentValueWithValue:completion:@
writeAttributePresentValueWithValue_completionSelector :: Selector '[Id NSNumber, Ptr ()] ()
writeAttributePresentValueWithValue_completionSelector = mkSelector "writeAttributePresentValueWithValue:completion:"

-- | @Selector@ for @writeAttributePresentValueWithValue:params:completion:@
writeAttributePresentValueWithValue_params_completionSelector :: Selector '[Id NSNumber, Id MTRWriteParams, Ptr ()] ()
writeAttributePresentValueWithValue_params_completionSelector = mkSelector "writeAttributePresentValueWithValue:params:completion:"

-- | @Selector@ for @subscribeAttributePresentValueWithParams:subscriptionEstablished:reportHandler:@
subscribeAttributePresentValueWithParams_subscriptionEstablished_reportHandlerSelector :: Selector '[Id MTRSubscribeParams, Ptr (), Ptr ()] ()
subscribeAttributePresentValueWithParams_subscriptionEstablished_reportHandlerSelector = mkSelector "subscribeAttributePresentValueWithParams:subscriptionEstablished:reportHandler:"

-- | @Selector@ for @readAttributePresentValueWithClusterStateCache:endpoint:queue:completion:@
readAttributePresentValueWithClusterStateCache_endpoint_queue_completionSelector :: Selector '[Id MTRClusterStateCacheContainer, Id NSNumber, Id NSObject, Ptr ()] ()
readAttributePresentValueWithClusterStateCache_endpoint_queue_completionSelector = mkSelector "readAttributePresentValueWithClusterStateCache:endpoint:queue:completion:"

-- | @Selector@ for @readAttributeReliabilityWithCompletion:@
readAttributeReliabilityWithCompletionSelector :: Selector '[Ptr ()] ()
readAttributeReliabilityWithCompletionSelector = mkSelector "readAttributeReliabilityWithCompletion:"

-- | @Selector@ for @writeAttributeReliabilityWithValue:completion:@
writeAttributeReliabilityWithValue_completionSelector :: Selector '[Id NSNumber, Ptr ()] ()
writeAttributeReliabilityWithValue_completionSelector = mkSelector "writeAttributeReliabilityWithValue:completion:"

-- | @Selector@ for @writeAttributeReliabilityWithValue:params:completion:@
writeAttributeReliabilityWithValue_params_completionSelector :: Selector '[Id NSNumber, Id MTRWriteParams, Ptr ()] ()
writeAttributeReliabilityWithValue_params_completionSelector = mkSelector "writeAttributeReliabilityWithValue:params:completion:"

-- | @Selector@ for @subscribeAttributeReliabilityWithParams:subscriptionEstablished:reportHandler:@
subscribeAttributeReliabilityWithParams_subscriptionEstablished_reportHandlerSelector :: Selector '[Id MTRSubscribeParams, Ptr (), Ptr ()] ()
subscribeAttributeReliabilityWithParams_subscriptionEstablished_reportHandlerSelector = mkSelector "subscribeAttributeReliabilityWithParams:subscriptionEstablished:reportHandler:"

-- | @Selector@ for @readAttributeReliabilityWithClusterStateCache:endpoint:queue:completion:@
readAttributeReliabilityWithClusterStateCache_endpoint_queue_completionSelector :: Selector '[Id MTRClusterStateCacheContainer, Id NSNumber, Id NSObject, Ptr ()] ()
readAttributeReliabilityWithClusterStateCache_endpoint_queue_completionSelector = mkSelector "readAttributeReliabilityWithClusterStateCache:endpoint:queue:completion:"

-- | @Selector@ for @readAttributeStatusFlagsWithCompletion:@
readAttributeStatusFlagsWithCompletionSelector :: Selector '[Ptr ()] ()
readAttributeStatusFlagsWithCompletionSelector = mkSelector "readAttributeStatusFlagsWithCompletion:"

-- | @Selector@ for @subscribeAttributeStatusFlagsWithParams:subscriptionEstablished:reportHandler:@
subscribeAttributeStatusFlagsWithParams_subscriptionEstablished_reportHandlerSelector :: Selector '[Id MTRSubscribeParams, Ptr (), Ptr ()] ()
subscribeAttributeStatusFlagsWithParams_subscriptionEstablished_reportHandlerSelector = mkSelector "subscribeAttributeStatusFlagsWithParams:subscriptionEstablished:reportHandler:"

-- | @Selector@ for @readAttributeStatusFlagsWithClusterStateCache:endpoint:queue:completion:@
readAttributeStatusFlagsWithClusterStateCache_endpoint_queue_completionSelector :: Selector '[Id MTRClusterStateCacheContainer, Id NSNumber, Id NSObject, Ptr ()] ()
readAttributeStatusFlagsWithClusterStateCache_endpoint_queue_completionSelector = mkSelector "readAttributeStatusFlagsWithClusterStateCache:endpoint:queue:completion:"

-- | @Selector@ for @readAttributeApplicationTypeWithCompletion:@
readAttributeApplicationTypeWithCompletionSelector :: Selector '[Ptr ()] ()
readAttributeApplicationTypeWithCompletionSelector = mkSelector "readAttributeApplicationTypeWithCompletion:"

-- | @Selector@ for @subscribeAttributeApplicationTypeWithParams:subscriptionEstablished:reportHandler:@
subscribeAttributeApplicationTypeWithParams_subscriptionEstablished_reportHandlerSelector :: Selector '[Id MTRSubscribeParams, Ptr (), Ptr ()] ()
subscribeAttributeApplicationTypeWithParams_subscriptionEstablished_reportHandlerSelector = mkSelector "subscribeAttributeApplicationTypeWithParams:subscriptionEstablished:reportHandler:"

-- | @Selector@ for @readAttributeApplicationTypeWithClusterStateCache:endpoint:queue:completion:@
readAttributeApplicationTypeWithClusterStateCache_endpoint_queue_completionSelector :: Selector '[Id MTRClusterStateCacheContainer, Id NSNumber, Id NSObject, Ptr ()] ()
readAttributeApplicationTypeWithClusterStateCache_endpoint_queue_completionSelector = mkSelector "readAttributeApplicationTypeWithClusterStateCache:endpoint:queue:completion:"

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
initSelector :: Selector '[] (Id MTRBaseClusterBinaryInputBasic)
initSelector = mkSelector "init"

-- | @Selector@ for @new@
newSelector :: Selector '[] (Id MTRBaseClusterBinaryInputBasic)
newSelector = mkSelector "new"

-- | @Selector@ for @initWithDevice:endpoint:queue:@
initWithDevice_endpoint_queueSelector :: Selector '[Id MTRBaseDevice, CUShort, Id NSObject] (Id MTRBaseClusterBinaryInputBasic)
initWithDevice_endpoint_queueSelector = mkSelector "initWithDevice:endpoint:queue:"

-- | @Selector@ for @readAttributeActiveTextWithCompletionHandler:@
readAttributeActiveTextWithCompletionHandlerSelector :: Selector '[Ptr ()] ()
readAttributeActiveTextWithCompletionHandlerSelector = mkSelector "readAttributeActiveTextWithCompletionHandler:"

-- | @Selector@ for @writeAttributeActiveTextWithValue:completionHandler:@
writeAttributeActiveTextWithValue_completionHandlerSelector :: Selector '[Id NSString, Ptr ()] ()
writeAttributeActiveTextWithValue_completionHandlerSelector = mkSelector "writeAttributeActiveTextWithValue:completionHandler:"

-- | @Selector@ for @writeAttributeActiveTextWithValue:params:completionHandler:@
writeAttributeActiveTextWithValue_params_completionHandlerSelector :: Selector '[Id NSString, Id MTRWriteParams, Ptr ()] ()
writeAttributeActiveTextWithValue_params_completionHandlerSelector = mkSelector "writeAttributeActiveTextWithValue:params:completionHandler:"

-- | @Selector@ for @subscribeAttributeActiveTextWithMinInterval:maxInterval:params:subscriptionEstablished:reportHandler:@
subscribeAttributeActiveTextWithMinInterval_maxInterval_params_subscriptionEstablished_reportHandlerSelector :: Selector '[Id NSNumber, Id NSNumber, Id MTRSubscribeParams, Ptr (), Ptr ()] ()
subscribeAttributeActiveTextWithMinInterval_maxInterval_params_subscriptionEstablished_reportHandlerSelector = mkSelector "subscribeAttributeActiveTextWithMinInterval:maxInterval:params:subscriptionEstablished:reportHandler:"

-- | @Selector@ for @readAttributeActiveTextWithAttributeCache:endpoint:queue:completionHandler:@
readAttributeActiveTextWithAttributeCache_endpoint_queue_completionHandlerSelector :: Selector '[Id MTRAttributeCacheContainer, Id NSNumber, Id NSObject, Ptr ()] ()
readAttributeActiveTextWithAttributeCache_endpoint_queue_completionHandlerSelector = mkSelector "readAttributeActiveTextWithAttributeCache:endpoint:queue:completionHandler:"

-- | @Selector@ for @readAttributeDescriptionWithCompletionHandler:@
readAttributeDescriptionWithCompletionHandlerSelector :: Selector '[Ptr ()] ()
readAttributeDescriptionWithCompletionHandlerSelector = mkSelector "readAttributeDescriptionWithCompletionHandler:"

-- | @Selector@ for @writeAttributeDescriptionWithValue:completionHandler:@
writeAttributeDescriptionWithValue_completionHandlerSelector :: Selector '[Id NSString, Ptr ()] ()
writeAttributeDescriptionWithValue_completionHandlerSelector = mkSelector "writeAttributeDescriptionWithValue:completionHandler:"

-- | @Selector@ for @writeAttributeDescriptionWithValue:params:completionHandler:@
writeAttributeDescriptionWithValue_params_completionHandlerSelector :: Selector '[Id NSString, Id MTRWriteParams, Ptr ()] ()
writeAttributeDescriptionWithValue_params_completionHandlerSelector = mkSelector "writeAttributeDescriptionWithValue:params:completionHandler:"

-- | @Selector@ for @subscribeAttributeDescriptionWithMinInterval:maxInterval:params:subscriptionEstablished:reportHandler:@
subscribeAttributeDescriptionWithMinInterval_maxInterval_params_subscriptionEstablished_reportHandlerSelector :: Selector '[Id NSNumber, Id NSNumber, Id MTRSubscribeParams, Ptr (), Ptr ()] ()
subscribeAttributeDescriptionWithMinInterval_maxInterval_params_subscriptionEstablished_reportHandlerSelector = mkSelector "subscribeAttributeDescriptionWithMinInterval:maxInterval:params:subscriptionEstablished:reportHandler:"

-- | @Selector@ for @readAttributeDescriptionWithAttributeCache:endpoint:queue:completionHandler:@
readAttributeDescriptionWithAttributeCache_endpoint_queue_completionHandlerSelector :: Selector '[Id MTRAttributeCacheContainer, Id NSNumber, Id NSObject, Ptr ()] ()
readAttributeDescriptionWithAttributeCache_endpoint_queue_completionHandlerSelector = mkSelector "readAttributeDescriptionWithAttributeCache:endpoint:queue:completionHandler:"

-- | @Selector@ for @readAttributeInactiveTextWithCompletionHandler:@
readAttributeInactiveTextWithCompletionHandlerSelector :: Selector '[Ptr ()] ()
readAttributeInactiveTextWithCompletionHandlerSelector = mkSelector "readAttributeInactiveTextWithCompletionHandler:"

-- | @Selector@ for @writeAttributeInactiveTextWithValue:completionHandler:@
writeAttributeInactiveTextWithValue_completionHandlerSelector :: Selector '[Id NSString, Ptr ()] ()
writeAttributeInactiveTextWithValue_completionHandlerSelector = mkSelector "writeAttributeInactiveTextWithValue:completionHandler:"

-- | @Selector@ for @writeAttributeInactiveTextWithValue:params:completionHandler:@
writeAttributeInactiveTextWithValue_params_completionHandlerSelector :: Selector '[Id NSString, Id MTRWriteParams, Ptr ()] ()
writeAttributeInactiveTextWithValue_params_completionHandlerSelector = mkSelector "writeAttributeInactiveTextWithValue:params:completionHandler:"

-- | @Selector@ for @subscribeAttributeInactiveTextWithMinInterval:maxInterval:params:subscriptionEstablished:reportHandler:@
subscribeAttributeInactiveTextWithMinInterval_maxInterval_params_subscriptionEstablished_reportHandlerSelector :: Selector '[Id NSNumber, Id NSNumber, Id MTRSubscribeParams, Ptr (), Ptr ()] ()
subscribeAttributeInactiveTextWithMinInterval_maxInterval_params_subscriptionEstablished_reportHandlerSelector = mkSelector "subscribeAttributeInactiveTextWithMinInterval:maxInterval:params:subscriptionEstablished:reportHandler:"

-- | @Selector@ for @readAttributeInactiveTextWithAttributeCache:endpoint:queue:completionHandler:@
readAttributeInactiveTextWithAttributeCache_endpoint_queue_completionHandlerSelector :: Selector '[Id MTRAttributeCacheContainer, Id NSNumber, Id NSObject, Ptr ()] ()
readAttributeInactiveTextWithAttributeCache_endpoint_queue_completionHandlerSelector = mkSelector "readAttributeInactiveTextWithAttributeCache:endpoint:queue:completionHandler:"

-- | @Selector@ for @readAttributeOutOfServiceWithCompletionHandler:@
readAttributeOutOfServiceWithCompletionHandlerSelector :: Selector '[Ptr ()] ()
readAttributeOutOfServiceWithCompletionHandlerSelector = mkSelector "readAttributeOutOfServiceWithCompletionHandler:"

-- | @Selector@ for @writeAttributeOutOfServiceWithValue:completionHandler:@
writeAttributeOutOfServiceWithValue_completionHandlerSelector :: Selector '[Id NSNumber, Ptr ()] ()
writeAttributeOutOfServiceWithValue_completionHandlerSelector = mkSelector "writeAttributeOutOfServiceWithValue:completionHandler:"

-- | @Selector@ for @writeAttributeOutOfServiceWithValue:params:completionHandler:@
writeAttributeOutOfServiceWithValue_params_completionHandlerSelector :: Selector '[Id NSNumber, Id MTRWriteParams, Ptr ()] ()
writeAttributeOutOfServiceWithValue_params_completionHandlerSelector = mkSelector "writeAttributeOutOfServiceWithValue:params:completionHandler:"

-- | @Selector@ for @subscribeAttributeOutOfServiceWithMinInterval:maxInterval:params:subscriptionEstablished:reportHandler:@
subscribeAttributeOutOfServiceWithMinInterval_maxInterval_params_subscriptionEstablished_reportHandlerSelector :: Selector '[Id NSNumber, Id NSNumber, Id MTRSubscribeParams, Ptr (), Ptr ()] ()
subscribeAttributeOutOfServiceWithMinInterval_maxInterval_params_subscriptionEstablished_reportHandlerSelector = mkSelector "subscribeAttributeOutOfServiceWithMinInterval:maxInterval:params:subscriptionEstablished:reportHandler:"

-- | @Selector@ for @readAttributeOutOfServiceWithAttributeCache:endpoint:queue:completionHandler:@
readAttributeOutOfServiceWithAttributeCache_endpoint_queue_completionHandlerSelector :: Selector '[Id MTRAttributeCacheContainer, Id NSNumber, Id NSObject, Ptr ()] ()
readAttributeOutOfServiceWithAttributeCache_endpoint_queue_completionHandlerSelector = mkSelector "readAttributeOutOfServiceWithAttributeCache:endpoint:queue:completionHandler:"

-- | @Selector@ for @readAttributePolarityWithCompletionHandler:@
readAttributePolarityWithCompletionHandlerSelector :: Selector '[Ptr ()] ()
readAttributePolarityWithCompletionHandlerSelector = mkSelector "readAttributePolarityWithCompletionHandler:"

-- | @Selector@ for @subscribeAttributePolarityWithMinInterval:maxInterval:params:subscriptionEstablished:reportHandler:@
subscribeAttributePolarityWithMinInterval_maxInterval_params_subscriptionEstablished_reportHandlerSelector :: Selector '[Id NSNumber, Id NSNumber, Id MTRSubscribeParams, Ptr (), Ptr ()] ()
subscribeAttributePolarityWithMinInterval_maxInterval_params_subscriptionEstablished_reportHandlerSelector = mkSelector "subscribeAttributePolarityWithMinInterval:maxInterval:params:subscriptionEstablished:reportHandler:"

-- | @Selector@ for @readAttributePolarityWithAttributeCache:endpoint:queue:completionHandler:@
readAttributePolarityWithAttributeCache_endpoint_queue_completionHandlerSelector :: Selector '[Id MTRAttributeCacheContainer, Id NSNumber, Id NSObject, Ptr ()] ()
readAttributePolarityWithAttributeCache_endpoint_queue_completionHandlerSelector = mkSelector "readAttributePolarityWithAttributeCache:endpoint:queue:completionHandler:"

-- | @Selector@ for @readAttributePresentValueWithCompletionHandler:@
readAttributePresentValueWithCompletionHandlerSelector :: Selector '[Ptr ()] ()
readAttributePresentValueWithCompletionHandlerSelector = mkSelector "readAttributePresentValueWithCompletionHandler:"

-- | @Selector@ for @writeAttributePresentValueWithValue:completionHandler:@
writeAttributePresentValueWithValue_completionHandlerSelector :: Selector '[Id NSNumber, Ptr ()] ()
writeAttributePresentValueWithValue_completionHandlerSelector = mkSelector "writeAttributePresentValueWithValue:completionHandler:"

-- | @Selector@ for @writeAttributePresentValueWithValue:params:completionHandler:@
writeAttributePresentValueWithValue_params_completionHandlerSelector :: Selector '[Id NSNumber, Id MTRWriteParams, Ptr ()] ()
writeAttributePresentValueWithValue_params_completionHandlerSelector = mkSelector "writeAttributePresentValueWithValue:params:completionHandler:"

-- | @Selector@ for @subscribeAttributePresentValueWithMinInterval:maxInterval:params:subscriptionEstablished:reportHandler:@
subscribeAttributePresentValueWithMinInterval_maxInterval_params_subscriptionEstablished_reportHandlerSelector :: Selector '[Id NSNumber, Id NSNumber, Id MTRSubscribeParams, Ptr (), Ptr ()] ()
subscribeAttributePresentValueWithMinInterval_maxInterval_params_subscriptionEstablished_reportHandlerSelector = mkSelector "subscribeAttributePresentValueWithMinInterval:maxInterval:params:subscriptionEstablished:reportHandler:"

-- | @Selector@ for @readAttributePresentValueWithAttributeCache:endpoint:queue:completionHandler:@
readAttributePresentValueWithAttributeCache_endpoint_queue_completionHandlerSelector :: Selector '[Id MTRAttributeCacheContainer, Id NSNumber, Id NSObject, Ptr ()] ()
readAttributePresentValueWithAttributeCache_endpoint_queue_completionHandlerSelector = mkSelector "readAttributePresentValueWithAttributeCache:endpoint:queue:completionHandler:"

-- | @Selector@ for @readAttributeReliabilityWithCompletionHandler:@
readAttributeReliabilityWithCompletionHandlerSelector :: Selector '[Ptr ()] ()
readAttributeReliabilityWithCompletionHandlerSelector = mkSelector "readAttributeReliabilityWithCompletionHandler:"

-- | @Selector@ for @writeAttributeReliabilityWithValue:completionHandler:@
writeAttributeReliabilityWithValue_completionHandlerSelector :: Selector '[Id NSNumber, Ptr ()] ()
writeAttributeReliabilityWithValue_completionHandlerSelector = mkSelector "writeAttributeReliabilityWithValue:completionHandler:"

-- | @Selector@ for @writeAttributeReliabilityWithValue:params:completionHandler:@
writeAttributeReliabilityWithValue_params_completionHandlerSelector :: Selector '[Id NSNumber, Id MTRWriteParams, Ptr ()] ()
writeAttributeReliabilityWithValue_params_completionHandlerSelector = mkSelector "writeAttributeReliabilityWithValue:params:completionHandler:"

-- | @Selector@ for @subscribeAttributeReliabilityWithMinInterval:maxInterval:params:subscriptionEstablished:reportHandler:@
subscribeAttributeReliabilityWithMinInterval_maxInterval_params_subscriptionEstablished_reportHandlerSelector :: Selector '[Id NSNumber, Id NSNumber, Id MTRSubscribeParams, Ptr (), Ptr ()] ()
subscribeAttributeReliabilityWithMinInterval_maxInterval_params_subscriptionEstablished_reportHandlerSelector = mkSelector "subscribeAttributeReliabilityWithMinInterval:maxInterval:params:subscriptionEstablished:reportHandler:"

-- | @Selector@ for @readAttributeReliabilityWithAttributeCache:endpoint:queue:completionHandler:@
readAttributeReliabilityWithAttributeCache_endpoint_queue_completionHandlerSelector :: Selector '[Id MTRAttributeCacheContainer, Id NSNumber, Id NSObject, Ptr ()] ()
readAttributeReliabilityWithAttributeCache_endpoint_queue_completionHandlerSelector = mkSelector "readAttributeReliabilityWithAttributeCache:endpoint:queue:completionHandler:"

-- | @Selector@ for @readAttributeStatusFlagsWithCompletionHandler:@
readAttributeStatusFlagsWithCompletionHandlerSelector :: Selector '[Ptr ()] ()
readAttributeStatusFlagsWithCompletionHandlerSelector = mkSelector "readAttributeStatusFlagsWithCompletionHandler:"

-- | @Selector@ for @subscribeAttributeStatusFlagsWithMinInterval:maxInterval:params:subscriptionEstablished:reportHandler:@
subscribeAttributeStatusFlagsWithMinInterval_maxInterval_params_subscriptionEstablished_reportHandlerSelector :: Selector '[Id NSNumber, Id NSNumber, Id MTRSubscribeParams, Ptr (), Ptr ()] ()
subscribeAttributeStatusFlagsWithMinInterval_maxInterval_params_subscriptionEstablished_reportHandlerSelector = mkSelector "subscribeAttributeStatusFlagsWithMinInterval:maxInterval:params:subscriptionEstablished:reportHandler:"

-- | @Selector@ for @readAttributeStatusFlagsWithAttributeCache:endpoint:queue:completionHandler:@
readAttributeStatusFlagsWithAttributeCache_endpoint_queue_completionHandlerSelector :: Selector '[Id MTRAttributeCacheContainer, Id NSNumber, Id NSObject, Ptr ()] ()
readAttributeStatusFlagsWithAttributeCache_endpoint_queue_completionHandlerSelector = mkSelector "readAttributeStatusFlagsWithAttributeCache:endpoint:queue:completionHandler:"

-- | @Selector@ for @readAttributeApplicationTypeWithCompletionHandler:@
readAttributeApplicationTypeWithCompletionHandlerSelector :: Selector '[Ptr ()] ()
readAttributeApplicationTypeWithCompletionHandlerSelector = mkSelector "readAttributeApplicationTypeWithCompletionHandler:"

-- | @Selector@ for @subscribeAttributeApplicationTypeWithMinInterval:maxInterval:params:subscriptionEstablished:reportHandler:@
subscribeAttributeApplicationTypeWithMinInterval_maxInterval_params_subscriptionEstablished_reportHandlerSelector :: Selector '[Id NSNumber, Id NSNumber, Id MTRSubscribeParams, Ptr (), Ptr ()] ()
subscribeAttributeApplicationTypeWithMinInterval_maxInterval_params_subscriptionEstablished_reportHandlerSelector = mkSelector "subscribeAttributeApplicationTypeWithMinInterval:maxInterval:params:subscriptionEstablished:reportHandler:"

-- | @Selector@ for @readAttributeApplicationTypeWithAttributeCache:endpoint:queue:completionHandler:@
readAttributeApplicationTypeWithAttributeCache_endpoint_queue_completionHandlerSelector :: Selector '[Id MTRAttributeCacheContainer, Id NSNumber, Id NSObject, Ptr ()] ()
readAttributeApplicationTypeWithAttributeCache_endpoint_queue_completionHandlerSelector = mkSelector "readAttributeApplicationTypeWithAttributeCache:endpoint:queue:completionHandler:"

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
initWithDevice_endpointID_queueSelector :: Selector '[Id MTRBaseDevice, Id NSNumber, Id NSObject] (Id MTRBaseClusterBinaryInputBasic)
initWithDevice_endpointID_queueSelector = mkSelector "initWithDevice:endpointID:queue:"

