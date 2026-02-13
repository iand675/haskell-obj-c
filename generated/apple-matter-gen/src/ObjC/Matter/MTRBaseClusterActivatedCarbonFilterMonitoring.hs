{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Cluster Activated Carbon Filter Monitoring
--
-- Attributes and commands for monitoring activated carbon filters in a device
--
-- Generated bindings for @MTRBaseClusterActivatedCarbonFilterMonitoring@.
module ObjC.Matter.MTRBaseClusterActivatedCarbonFilterMonitoring
  ( MTRBaseClusterActivatedCarbonFilterMonitoring
  , IsMTRBaseClusterActivatedCarbonFilterMonitoring(..)
  , resetConditionWithParams_completion
  , resetConditionWithCompletion
  , readAttributeConditionWithCompletion
  , subscribeAttributeConditionWithParams_subscriptionEstablished_reportHandler
  , readAttributeConditionWithClusterStateCache_endpoint_queue_completion
  , readAttributeDegradationDirectionWithCompletion
  , subscribeAttributeDegradationDirectionWithParams_subscriptionEstablished_reportHandler
  , readAttributeDegradationDirectionWithClusterStateCache_endpoint_queue_completion
  , readAttributeChangeIndicationWithCompletion
  , subscribeAttributeChangeIndicationWithParams_subscriptionEstablished_reportHandler
  , readAttributeChangeIndicationWithClusterStateCache_endpoint_queue_completion
  , readAttributeInPlaceIndicatorWithCompletion
  , subscribeAttributeInPlaceIndicatorWithParams_subscriptionEstablished_reportHandler
  , readAttributeInPlaceIndicatorWithClusterStateCache_endpoint_queue_completion
  , readAttributeLastChangedTimeWithCompletion
  , writeAttributeLastChangedTimeWithValue_completion
  , writeAttributeLastChangedTimeWithValue_params_completion
  , subscribeAttributeLastChangedTimeWithParams_subscriptionEstablished_reportHandler
  , readAttributeLastChangedTimeWithClusterStateCache_endpoint_queue_completion
  , readAttributeReplacementProductListWithCompletion
  , subscribeAttributeReplacementProductListWithParams_subscriptionEstablished_reportHandler
  , readAttributeReplacementProductListWithClusterStateCache_endpoint_queue_completion
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
  , initSelector
  , initWithDevice_endpointID_queueSelector
  , newSelector
  , readAttributeAcceptedCommandListWithClusterStateCache_endpoint_queue_completionSelector
  , readAttributeAcceptedCommandListWithCompletionSelector
  , readAttributeAttributeListWithClusterStateCache_endpoint_queue_completionSelector
  , readAttributeAttributeListWithCompletionSelector
  , readAttributeChangeIndicationWithClusterStateCache_endpoint_queue_completionSelector
  , readAttributeChangeIndicationWithCompletionSelector
  , readAttributeClusterRevisionWithClusterStateCache_endpoint_queue_completionSelector
  , readAttributeClusterRevisionWithCompletionSelector
  , readAttributeConditionWithClusterStateCache_endpoint_queue_completionSelector
  , readAttributeConditionWithCompletionSelector
  , readAttributeDegradationDirectionWithClusterStateCache_endpoint_queue_completionSelector
  , readAttributeDegradationDirectionWithCompletionSelector
  , readAttributeFeatureMapWithClusterStateCache_endpoint_queue_completionSelector
  , readAttributeFeatureMapWithCompletionSelector
  , readAttributeGeneratedCommandListWithClusterStateCache_endpoint_queue_completionSelector
  , readAttributeGeneratedCommandListWithCompletionSelector
  , readAttributeInPlaceIndicatorWithClusterStateCache_endpoint_queue_completionSelector
  , readAttributeInPlaceIndicatorWithCompletionSelector
  , readAttributeLastChangedTimeWithClusterStateCache_endpoint_queue_completionSelector
  , readAttributeLastChangedTimeWithCompletionSelector
  , readAttributeReplacementProductListWithClusterStateCache_endpoint_queue_completionSelector
  , readAttributeReplacementProductListWithCompletionSelector
  , resetConditionWithCompletionSelector
  , resetConditionWithParams_completionSelector
  , subscribeAttributeAcceptedCommandListWithParams_subscriptionEstablished_reportHandlerSelector
  , subscribeAttributeAttributeListWithParams_subscriptionEstablished_reportHandlerSelector
  , subscribeAttributeChangeIndicationWithParams_subscriptionEstablished_reportHandlerSelector
  , subscribeAttributeClusterRevisionWithParams_subscriptionEstablished_reportHandlerSelector
  , subscribeAttributeConditionWithParams_subscriptionEstablished_reportHandlerSelector
  , subscribeAttributeDegradationDirectionWithParams_subscriptionEstablished_reportHandlerSelector
  , subscribeAttributeFeatureMapWithParams_subscriptionEstablished_reportHandlerSelector
  , subscribeAttributeGeneratedCommandListWithParams_subscriptionEstablished_reportHandlerSelector
  , subscribeAttributeInPlaceIndicatorWithParams_subscriptionEstablished_reportHandlerSelector
  , subscribeAttributeLastChangedTimeWithParams_subscriptionEstablished_reportHandlerSelector
  , subscribeAttributeReplacementProductListWithParams_subscriptionEstablished_reportHandlerSelector
  , writeAttributeLastChangedTimeWithValue_completionSelector
  , writeAttributeLastChangedTimeWithValue_params_completionSelector


  ) where

import Foreign.Ptr (Ptr, FunPtr)
import Foreign.C.Types

import ObjC.Runtime.Types
import ObjC.Runtime.Message (sendMessage, sendOwnedMessage, sendClassMessage, sendOwnedClassMessage)
import ObjC.Runtime.Selector (mkSelector)
import ObjC.Runtime.Class (getRequiredClass)

import ObjC.Matter.Internal.Classes
import ObjC.Foundation.Internal.Classes

-- | Command ResetCondition
--
-- Reset the condition of the replaceable to the non degraded state
--
-- ObjC selector: @- resetConditionWithParams:completion:@
resetConditionWithParams_completion :: (IsMTRBaseClusterActivatedCarbonFilterMonitoring mtrBaseClusterActivatedCarbonFilterMonitoring, IsMTRActivatedCarbonFilterMonitoringClusterResetConditionParams params) => mtrBaseClusterActivatedCarbonFilterMonitoring -> params -> Ptr () -> IO ()
resetConditionWithParams_completion mtrBaseClusterActivatedCarbonFilterMonitoring params completion =
  sendMessage mtrBaseClusterActivatedCarbonFilterMonitoring resetConditionWithParams_completionSelector (toMTRActivatedCarbonFilterMonitoringClusterResetConditionParams params) completion

-- | @- resetConditionWithCompletion:@
resetConditionWithCompletion :: IsMTRBaseClusterActivatedCarbonFilterMonitoring mtrBaseClusterActivatedCarbonFilterMonitoring => mtrBaseClusterActivatedCarbonFilterMonitoring -> Ptr () -> IO ()
resetConditionWithCompletion mtrBaseClusterActivatedCarbonFilterMonitoring completion =
  sendMessage mtrBaseClusterActivatedCarbonFilterMonitoring resetConditionWithCompletionSelector completion

-- | @- readAttributeConditionWithCompletion:@
readAttributeConditionWithCompletion :: IsMTRBaseClusterActivatedCarbonFilterMonitoring mtrBaseClusterActivatedCarbonFilterMonitoring => mtrBaseClusterActivatedCarbonFilterMonitoring -> Ptr () -> IO ()
readAttributeConditionWithCompletion mtrBaseClusterActivatedCarbonFilterMonitoring completion =
  sendMessage mtrBaseClusterActivatedCarbonFilterMonitoring readAttributeConditionWithCompletionSelector completion

-- | @- subscribeAttributeConditionWithParams:subscriptionEstablished:reportHandler:@
subscribeAttributeConditionWithParams_subscriptionEstablished_reportHandler :: (IsMTRBaseClusterActivatedCarbonFilterMonitoring mtrBaseClusterActivatedCarbonFilterMonitoring, IsMTRSubscribeParams params) => mtrBaseClusterActivatedCarbonFilterMonitoring -> params -> Ptr () -> Ptr () -> IO ()
subscribeAttributeConditionWithParams_subscriptionEstablished_reportHandler mtrBaseClusterActivatedCarbonFilterMonitoring params subscriptionEstablished reportHandler =
  sendMessage mtrBaseClusterActivatedCarbonFilterMonitoring subscribeAttributeConditionWithParams_subscriptionEstablished_reportHandlerSelector (toMTRSubscribeParams params) subscriptionEstablished reportHandler

-- | @+ readAttributeConditionWithClusterStateCache:endpoint:queue:completion:@
readAttributeConditionWithClusterStateCache_endpoint_queue_completion :: (IsMTRClusterStateCacheContainer clusterStateCacheContainer, IsNSNumber endpoint, IsNSObject queue) => clusterStateCacheContainer -> endpoint -> queue -> Ptr () -> IO ()
readAttributeConditionWithClusterStateCache_endpoint_queue_completion clusterStateCacheContainer endpoint queue completion =
  do
    cls' <- getRequiredClass "MTRBaseClusterActivatedCarbonFilterMonitoring"
    sendClassMessage cls' readAttributeConditionWithClusterStateCache_endpoint_queue_completionSelector (toMTRClusterStateCacheContainer clusterStateCacheContainer) (toNSNumber endpoint) (toNSObject queue) completion

-- | @- readAttributeDegradationDirectionWithCompletion:@
readAttributeDegradationDirectionWithCompletion :: IsMTRBaseClusterActivatedCarbonFilterMonitoring mtrBaseClusterActivatedCarbonFilterMonitoring => mtrBaseClusterActivatedCarbonFilterMonitoring -> Ptr () -> IO ()
readAttributeDegradationDirectionWithCompletion mtrBaseClusterActivatedCarbonFilterMonitoring completion =
  sendMessage mtrBaseClusterActivatedCarbonFilterMonitoring readAttributeDegradationDirectionWithCompletionSelector completion

-- | @- subscribeAttributeDegradationDirectionWithParams:subscriptionEstablished:reportHandler:@
subscribeAttributeDegradationDirectionWithParams_subscriptionEstablished_reportHandler :: (IsMTRBaseClusterActivatedCarbonFilterMonitoring mtrBaseClusterActivatedCarbonFilterMonitoring, IsMTRSubscribeParams params) => mtrBaseClusterActivatedCarbonFilterMonitoring -> params -> Ptr () -> Ptr () -> IO ()
subscribeAttributeDegradationDirectionWithParams_subscriptionEstablished_reportHandler mtrBaseClusterActivatedCarbonFilterMonitoring params subscriptionEstablished reportHandler =
  sendMessage mtrBaseClusterActivatedCarbonFilterMonitoring subscribeAttributeDegradationDirectionWithParams_subscriptionEstablished_reportHandlerSelector (toMTRSubscribeParams params) subscriptionEstablished reportHandler

-- | @+ readAttributeDegradationDirectionWithClusterStateCache:endpoint:queue:completion:@
readAttributeDegradationDirectionWithClusterStateCache_endpoint_queue_completion :: (IsMTRClusterStateCacheContainer clusterStateCacheContainer, IsNSNumber endpoint, IsNSObject queue) => clusterStateCacheContainer -> endpoint -> queue -> Ptr () -> IO ()
readAttributeDegradationDirectionWithClusterStateCache_endpoint_queue_completion clusterStateCacheContainer endpoint queue completion =
  do
    cls' <- getRequiredClass "MTRBaseClusterActivatedCarbonFilterMonitoring"
    sendClassMessage cls' readAttributeDegradationDirectionWithClusterStateCache_endpoint_queue_completionSelector (toMTRClusterStateCacheContainer clusterStateCacheContainer) (toNSNumber endpoint) (toNSObject queue) completion

-- | @- readAttributeChangeIndicationWithCompletion:@
readAttributeChangeIndicationWithCompletion :: IsMTRBaseClusterActivatedCarbonFilterMonitoring mtrBaseClusterActivatedCarbonFilterMonitoring => mtrBaseClusterActivatedCarbonFilterMonitoring -> Ptr () -> IO ()
readAttributeChangeIndicationWithCompletion mtrBaseClusterActivatedCarbonFilterMonitoring completion =
  sendMessage mtrBaseClusterActivatedCarbonFilterMonitoring readAttributeChangeIndicationWithCompletionSelector completion

-- | @- subscribeAttributeChangeIndicationWithParams:subscriptionEstablished:reportHandler:@
subscribeAttributeChangeIndicationWithParams_subscriptionEstablished_reportHandler :: (IsMTRBaseClusterActivatedCarbonFilterMonitoring mtrBaseClusterActivatedCarbonFilterMonitoring, IsMTRSubscribeParams params) => mtrBaseClusterActivatedCarbonFilterMonitoring -> params -> Ptr () -> Ptr () -> IO ()
subscribeAttributeChangeIndicationWithParams_subscriptionEstablished_reportHandler mtrBaseClusterActivatedCarbonFilterMonitoring params subscriptionEstablished reportHandler =
  sendMessage mtrBaseClusterActivatedCarbonFilterMonitoring subscribeAttributeChangeIndicationWithParams_subscriptionEstablished_reportHandlerSelector (toMTRSubscribeParams params) subscriptionEstablished reportHandler

-- | @+ readAttributeChangeIndicationWithClusterStateCache:endpoint:queue:completion:@
readAttributeChangeIndicationWithClusterStateCache_endpoint_queue_completion :: (IsMTRClusterStateCacheContainer clusterStateCacheContainer, IsNSNumber endpoint, IsNSObject queue) => clusterStateCacheContainer -> endpoint -> queue -> Ptr () -> IO ()
readAttributeChangeIndicationWithClusterStateCache_endpoint_queue_completion clusterStateCacheContainer endpoint queue completion =
  do
    cls' <- getRequiredClass "MTRBaseClusterActivatedCarbonFilterMonitoring"
    sendClassMessage cls' readAttributeChangeIndicationWithClusterStateCache_endpoint_queue_completionSelector (toMTRClusterStateCacheContainer clusterStateCacheContainer) (toNSNumber endpoint) (toNSObject queue) completion

-- | @- readAttributeInPlaceIndicatorWithCompletion:@
readAttributeInPlaceIndicatorWithCompletion :: IsMTRBaseClusterActivatedCarbonFilterMonitoring mtrBaseClusterActivatedCarbonFilterMonitoring => mtrBaseClusterActivatedCarbonFilterMonitoring -> Ptr () -> IO ()
readAttributeInPlaceIndicatorWithCompletion mtrBaseClusterActivatedCarbonFilterMonitoring completion =
  sendMessage mtrBaseClusterActivatedCarbonFilterMonitoring readAttributeInPlaceIndicatorWithCompletionSelector completion

-- | @- subscribeAttributeInPlaceIndicatorWithParams:subscriptionEstablished:reportHandler:@
subscribeAttributeInPlaceIndicatorWithParams_subscriptionEstablished_reportHandler :: (IsMTRBaseClusterActivatedCarbonFilterMonitoring mtrBaseClusterActivatedCarbonFilterMonitoring, IsMTRSubscribeParams params) => mtrBaseClusterActivatedCarbonFilterMonitoring -> params -> Ptr () -> Ptr () -> IO ()
subscribeAttributeInPlaceIndicatorWithParams_subscriptionEstablished_reportHandler mtrBaseClusterActivatedCarbonFilterMonitoring params subscriptionEstablished reportHandler =
  sendMessage mtrBaseClusterActivatedCarbonFilterMonitoring subscribeAttributeInPlaceIndicatorWithParams_subscriptionEstablished_reportHandlerSelector (toMTRSubscribeParams params) subscriptionEstablished reportHandler

-- | @+ readAttributeInPlaceIndicatorWithClusterStateCache:endpoint:queue:completion:@
readAttributeInPlaceIndicatorWithClusterStateCache_endpoint_queue_completion :: (IsMTRClusterStateCacheContainer clusterStateCacheContainer, IsNSNumber endpoint, IsNSObject queue) => clusterStateCacheContainer -> endpoint -> queue -> Ptr () -> IO ()
readAttributeInPlaceIndicatorWithClusterStateCache_endpoint_queue_completion clusterStateCacheContainer endpoint queue completion =
  do
    cls' <- getRequiredClass "MTRBaseClusterActivatedCarbonFilterMonitoring"
    sendClassMessage cls' readAttributeInPlaceIndicatorWithClusterStateCache_endpoint_queue_completionSelector (toMTRClusterStateCacheContainer clusterStateCacheContainer) (toNSNumber endpoint) (toNSObject queue) completion

-- | @- readAttributeLastChangedTimeWithCompletion:@
readAttributeLastChangedTimeWithCompletion :: IsMTRBaseClusterActivatedCarbonFilterMonitoring mtrBaseClusterActivatedCarbonFilterMonitoring => mtrBaseClusterActivatedCarbonFilterMonitoring -> Ptr () -> IO ()
readAttributeLastChangedTimeWithCompletion mtrBaseClusterActivatedCarbonFilterMonitoring completion =
  sendMessage mtrBaseClusterActivatedCarbonFilterMonitoring readAttributeLastChangedTimeWithCompletionSelector completion

-- | @- writeAttributeLastChangedTimeWithValue:completion:@
writeAttributeLastChangedTimeWithValue_completion :: (IsMTRBaseClusterActivatedCarbonFilterMonitoring mtrBaseClusterActivatedCarbonFilterMonitoring, IsNSNumber value) => mtrBaseClusterActivatedCarbonFilterMonitoring -> value -> Ptr () -> IO ()
writeAttributeLastChangedTimeWithValue_completion mtrBaseClusterActivatedCarbonFilterMonitoring value completion =
  sendMessage mtrBaseClusterActivatedCarbonFilterMonitoring writeAttributeLastChangedTimeWithValue_completionSelector (toNSNumber value) completion

-- | @- writeAttributeLastChangedTimeWithValue:params:completion:@
writeAttributeLastChangedTimeWithValue_params_completion :: (IsMTRBaseClusterActivatedCarbonFilterMonitoring mtrBaseClusterActivatedCarbonFilterMonitoring, IsNSNumber value, IsMTRWriteParams params) => mtrBaseClusterActivatedCarbonFilterMonitoring -> value -> params -> Ptr () -> IO ()
writeAttributeLastChangedTimeWithValue_params_completion mtrBaseClusterActivatedCarbonFilterMonitoring value params completion =
  sendMessage mtrBaseClusterActivatedCarbonFilterMonitoring writeAttributeLastChangedTimeWithValue_params_completionSelector (toNSNumber value) (toMTRWriteParams params) completion

-- | @- subscribeAttributeLastChangedTimeWithParams:subscriptionEstablished:reportHandler:@
subscribeAttributeLastChangedTimeWithParams_subscriptionEstablished_reportHandler :: (IsMTRBaseClusterActivatedCarbonFilterMonitoring mtrBaseClusterActivatedCarbonFilterMonitoring, IsMTRSubscribeParams params) => mtrBaseClusterActivatedCarbonFilterMonitoring -> params -> Ptr () -> Ptr () -> IO ()
subscribeAttributeLastChangedTimeWithParams_subscriptionEstablished_reportHandler mtrBaseClusterActivatedCarbonFilterMonitoring params subscriptionEstablished reportHandler =
  sendMessage mtrBaseClusterActivatedCarbonFilterMonitoring subscribeAttributeLastChangedTimeWithParams_subscriptionEstablished_reportHandlerSelector (toMTRSubscribeParams params) subscriptionEstablished reportHandler

-- | @+ readAttributeLastChangedTimeWithClusterStateCache:endpoint:queue:completion:@
readAttributeLastChangedTimeWithClusterStateCache_endpoint_queue_completion :: (IsMTRClusterStateCacheContainer clusterStateCacheContainer, IsNSNumber endpoint, IsNSObject queue) => clusterStateCacheContainer -> endpoint -> queue -> Ptr () -> IO ()
readAttributeLastChangedTimeWithClusterStateCache_endpoint_queue_completion clusterStateCacheContainer endpoint queue completion =
  do
    cls' <- getRequiredClass "MTRBaseClusterActivatedCarbonFilterMonitoring"
    sendClassMessage cls' readAttributeLastChangedTimeWithClusterStateCache_endpoint_queue_completionSelector (toMTRClusterStateCacheContainer clusterStateCacheContainer) (toNSNumber endpoint) (toNSObject queue) completion

-- | @- readAttributeReplacementProductListWithCompletion:@
readAttributeReplacementProductListWithCompletion :: IsMTRBaseClusterActivatedCarbonFilterMonitoring mtrBaseClusterActivatedCarbonFilterMonitoring => mtrBaseClusterActivatedCarbonFilterMonitoring -> Ptr () -> IO ()
readAttributeReplacementProductListWithCompletion mtrBaseClusterActivatedCarbonFilterMonitoring completion =
  sendMessage mtrBaseClusterActivatedCarbonFilterMonitoring readAttributeReplacementProductListWithCompletionSelector completion

-- | @- subscribeAttributeReplacementProductListWithParams:subscriptionEstablished:reportHandler:@
subscribeAttributeReplacementProductListWithParams_subscriptionEstablished_reportHandler :: (IsMTRBaseClusterActivatedCarbonFilterMonitoring mtrBaseClusterActivatedCarbonFilterMonitoring, IsMTRSubscribeParams params) => mtrBaseClusterActivatedCarbonFilterMonitoring -> params -> Ptr () -> Ptr () -> IO ()
subscribeAttributeReplacementProductListWithParams_subscriptionEstablished_reportHandler mtrBaseClusterActivatedCarbonFilterMonitoring params subscriptionEstablished reportHandler =
  sendMessage mtrBaseClusterActivatedCarbonFilterMonitoring subscribeAttributeReplacementProductListWithParams_subscriptionEstablished_reportHandlerSelector (toMTRSubscribeParams params) subscriptionEstablished reportHandler

-- | @+ readAttributeReplacementProductListWithClusterStateCache:endpoint:queue:completion:@
readAttributeReplacementProductListWithClusterStateCache_endpoint_queue_completion :: (IsMTRClusterStateCacheContainer clusterStateCacheContainer, IsNSNumber endpoint, IsNSObject queue) => clusterStateCacheContainer -> endpoint -> queue -> Ptr () -> IO ()
readAttributeReplacementProductListWithClusterStateCache_endpoint_queue_completion clusterStateCacheContainer endpoint queue completion =
  do
    cls' <- getRequiredClass "MTRBaseClusterActivatedCarbonFilterMonitoring"
    sendClassMessage cls' readAttributeReplacementProductListWithClusterStateCache_endpoint_queue_completionSelector (toMTRClusterStateCacheContainer clusterStateCacheContainer) (toNSNumber endpoint) (toNSObject queue) completion

-- | @- readAttributeGeneratedCommandListWithCompletion:@
readAttributeGeneratedCommandListWithCompletion :: IsMTRBaseClusterActivatedCarbonFilterMonitoring mtrBaseClusterActivatedCarbonFilterMonitoring => mtrBaseClusterActivatedCarbonFilterMonitoring -> Ptr () -> IO ()
readAttributeGeneratedCommandListWithCompletion mtrBaseClusterActivatedCarbonFilterMonitoring completion =
  sendMessage mtrBaseClusterActivatedCarbonFilterMonitoring readAttributeGeneratedCommandListWithCompletionSelector completion

-- | @- subscribeAttributeGeneratedCommandListWithParams:subscriptionEstablished:reportHandler:@
subscribeAttributeGeneratedCommandListWithParams_subscriptionEstablished_reportHandler :: (IsMTRBaseClusterActivatedCarbonFilterMonitoring mtrBaseClusterActivatedCarbonFilterMonitoring, IsMTRSubscribeParams params) => mtrBaseClusterActivatedCarbonFilterMonitoring -> params -> Ptr () -> Ptr () -> IO ()
subscribeAttributeGeneratedCommandListWithParams_subscriptionEstablished_reportHandler mtrBaseClusterActivatedCarbonFilterMonitoring params subscriptionEstablished reportHandler =
  sendMessage mtrBaseClusterActivatedCarbonFilterMonitoring subscribeAttributeGeneratedCommandListWithParams_subscriptionEstablished_reportHandlerSelector (toMTRSubscribeParams params) subscriptionEstablished reportHandler

-- | @+ readAttributeGeneratedCommandListWithClusterStateCache:endpoint:queue:completion:@
readAttributeGeneratedCommandListWithClusterStateCache_endpoint_queue_completion :: (IsMTRClusterStateCacheContainer clusterStateCacheContainer, IsNSNumber endpoint, IsNSObject queue) => clusterStateCacheContainer -> endpoint -> queue -> Ptr () -> IO ()
readAttributeGeneratedCommandListWithClusterStateCache_endpoint_queue_completion clusterStateCacheContainer endpoint queue completion =
  do
    cls' <- getRequiredClass "MTRBaseClusterActivatedCarbonFilterMonitoring"
    sendClassMessage cls' readAttributeGeneratedCommandListWithClusterStateCache_endpoint_queue_completionSelector (toMTRClusterStateCacheContainer clusterStateCacheContainer) (toNSNumber endpoint) (toNSObject queue) completion

-- | @- readAttributeAcceptedCommandListWithCompletion:@
readAttributeAcceptedCommandListWithCompletion :: IsMTRBaseClusterActivatedCarbonFilterMonitoring mtrBaseClusterActivatedCarbonFilterMonitoring => mtrBaseClusterActivatedCarbonFilterMonitoring -> Ptr () -> IO ()
readAttributeAcceptedCommandListWithCompletion mtrBaseClusterActivatedCarbonFilterMonitoring completion =
  sendMessage mtrBaseClusterActivatedCarbonFilterMonitoring readAttributeAcceptedCommandListWithCompletionSelector completion

-- | @- subscribeAttributeAcceptedCommandListWithParams:subscriptionEstablished:reportHandler:@
subscribeAttributeAcceptedCommandListWithParams_subscriptionEstablished_reportHandler :: (IsMTRBaseClusterActivatedCarbonFilterMonitoring mtrBaseClusterActivatedCarbonFilterMonitoring, IsMTRSubscribeParams params) => mtrBaseClusterActivatedCarbonFilterMonitoring -> params -> Ptr () -> Ptr () -> IO ()
subscribeAttributeAcceptedCommandListWithParams_subscriptionEstablished_reportHandler mtrBaseClusterActivatedCarbonFilterMonitoring params subscriptionEstablished reportHandler =
  sendMessage mtrBaseClusterActivatedCarbonFilterMonitoring subscribeAttributeAcceptedCommandListWithParams_subscriptionEstablished_reportHandlerSelector (toMTRSubscribeParams params) subscriptionEstablished reportHandler

-- | @+ readAttributeAcceptedCommandListWithClusterStateCache:endpoint:queue:completion:@
readAttributeAcceptedCommandListWithClusterStateCache_endpoint_queue_completion :: (IsMTRClusterStateCacheContainer clusterStateCacheContainer, IsNSNumber endpoint, IsNSObject queue) => clusterStateCacheContainer -> endpoint -> queue -> Ptr () -> IO ()
readAttributeAcceptedCommandListWithClusterStateCache_endpoint_queue_completion clusterStateCacheContainer endpoint queue completion =
  do
    cls' <- getRequiredClass "MTRBaseClusterActivatedCarbonFilterMonitoring"
    sendClassMessage cls' readAttributeAcceptedCommandListWithClusterStateCache_endpoint_queue_completionSelector (toMTRClusterStateCacheContainer clusterStateCacheContainer) (toNSNumber endpoint) (toNSObject queue) completion

-- | @- readAttributeAttributeListWithCompletion:@
readAttributeAttributeListWithCompletion :: IsMTRBaseClusterActivatedCarbonFilterMonitoring mtrBaseClusterActivatedCarbonFilterMonitoring => mtrBaseClusterActivatedCarbonFilterMonitoring -> Ptr () -> IO ()
readAttributeAttributeListWithCompletion mtrBaseClusterActivatedCarbonFilterMonitoring completion =
  sendMessage mtrBaseClusterActivatedCarbonFilterMonitoring readAttributeAttributeListWithCompletionSelector completion

-- | @- subscribeAttributeAttributeListWithParams:subscriptionEstablished:reportHandler:@
subscribeAttributeAttributeListWithParams_subscriptionEstablished_reportHandler :: (IsMTRBaseClusterActivatedCarbonFilterMonitoring mtrBaseClusterActivatedCarbonFilterMonitoring, IsMTRSubscribeParams params) => mtrBaseClusterActivatedCarbonFilterMonitoring -> params -> Ptr () -> Ptr () -> IO ()
subscribeAttributeAttributeListWithParams_subscriptionEstablished_reportHandler mtrBaseClusterActivatedCarbonFilterMonitoring params subscriptionEstablished reportHandler =
  sendMessage mtrBaseClusterActivatedCarbonFilterMonitoring subscribeAttributeAttributeListWithParams_subscriptionEstablished_reportHandlerSelector (toMTRSubscribeParams params) subscriptionEstablished reportHandler

-- | @+ readAttributeAttributeListWithClusterStateCache:endpoint:queue:completion:@
readAttributeAttributeListWithClusterStateCache_endpoint_queue_completion :: (IsMTRClusterStateCacheContainer clusterStateCacheContainer, IsNSNumber endpoint, IsNSObject queue) => clusterStateCacheContainer -> endpoint -> queue -> Ptr () -> IO ()
readAttributeAttributeListWithClusterStateCache_endpoint_queue_completion clusterStateCacheContainer endpoint queue completion =
  do
    cls' <- getRequiredClass "MTRBaseClusterActivatedCarbonFilterMonitoring"
    sendClassMessage cls' readAttributeAttributeListWithClusterStateCache_endpoint_queue_completionSelector (toMTRClusterStateCacheContainer clusterStateCacheContainer) (toNSNumber endpoint) (toNSObject queue) completion

-- | @- readAttributeFeatureMapWithCompletion:@
readAttributeFeatureMapWithCompletion :: IsMTRBaseClusterActivatedCarbonFilterMonitoring mtrBaseClusterActivatedCarbonFilterMonitoring => mtrBaseClusterActivatedCarbonFilterMonitoring -> Ptr () -> IO ()
readAttributeFeatureMapWithCompletion mtrBaseClusterActivatedCarbonFilterMonitoring completion =
  sendMessage mtrBaseClusterActivatedCarbonFilterMonitoring readAttributeFeatureMapWithCompletionSelector completion

-- | @- subscribeAttributeFeatureMapWithParams:subscriptionEstablished:reportHandler:@
subscribeAttributeFeatureMapWithParams_subscriptionEstablished_reportHandler :: (IsMTRBaseClusterActivatedCarbonFilterMonitoring mtrBaseClusterActivatedCarbonFilterMonitoring, IsMTRSubscribeParams params) => mtrBaseClusterActivatedCarbonFilterMonitoring -> params -> Ptr () -> Ptr () -> IO ()
subscribeAttributeFeatureMapWithParams_subscriptionEstablished_reportHandler mtrBaseClusterActivatedCarbonFilterMonitoring params subscriptionEstablished reportHandler =
  sendMessage mtrBaseClusterActivatedCarbonFilterMonitoring subscribeAttributeFeatureMapWithParams_subscriptionEstablished_reportHandlerSelector (toMTRSubscribeParams params) subscriptionEstablished reportHandler

-- | @+ readAttributeFeatureMapWithClusterStateCache:endpoint:queue:completion:@
readAttributeFeatureMapWithClusterStateCache_endpoint_queue_completion :: (IsMTRClusterStateCacheContainer clusterStateCacheContainer, IsNSNumber endpoint, IsNSObject queue) => clusterStateCacheContainer -> endpoint -> queue -> Ptr () -> IO ()
readAttributeFeatureMapWithClusterStateCache_endpoint_queue_completion clusterStateCacheContainer endpoint queue completion =
  do
    cls' <- getRequiredClass "MTRBaseClusterActivatedCarbonFilterMonitoring"
    sendClassMessage cls' readAttributeFeatureMapWithClusterStateCache_endpoint_queue_completionSelector (toMTRClusterStateCacheContainer clusterStateCacheContainer) (toNSNumber endpoint) (toNSObject queue) completion

-- | @- readAttributeClusterRevisionWithCompletion:@
readAttributeClusterRevisionWithCompletion :: IsMTRBaseClusterActivatedCarbonFilterMonitoring mtrBaseClusterActivatedCarbonFilterMonitoring => mtrBaseClusterActivatedCarbonFilterMonitoring -> Ptr () -> IO ()
readAttributeClusterRevisionWithCompletion mtrBaseClusterActivatedCarbonFilterMonitoring completion =
  sendMessage mtrBaseClusterActivatedCarbonFilterMonitoring readAttributeClusterRevisionWithCompletionSelector completion

-- | @- subscribeAttributeClusterRevisionWithParams:subscriptionEstablished:reportHandler:@
subscribeAttributeClusterRevisionWithParams_subscriptionEstablished_reportHandler :: (IsMTRBaseClusterActivatedCarbonFilterMonitoring mtrBaseClusterActivatedCarbonFilterMonitoring, IsMTRSubscribeParams params) => mtrBaseClusterActivatedCarbonFilterMonitoring -> params -> Ptr () -> Ptr () -> IO ()
subscribeAttributeClusterRevisionWithParams_subscriptionEstablished_reportHandler mtrBaseClusterActivatedCarbonFilterMonitoring params subscriptionEstablished reportHandler =
  sendMessage mtrBaseClusterActivatedCarbonFilterMonitoring subscribeAttributeClusterRevisionWithParams_subscriptionEstablished_reportHandlerSelector (toMTRSubscribeParams params) subscriptionEstablished reportHandler

-- | @+ readAttributeClusterRevisionWithClusterStateCache:endpoint:queue:completion:@
readAttributeClusterRevisionWithClusterStateCache_endpoint_queue_completion :: (IsMTRClusterStateCacheContainer clusterStateCacheContainer, IsNSNumber endpoint, IsNSObject queue) => clusterStateCacheContainer -> endpoint -> queue -> Ptr () -> IO ()
readAttributeClusterRevisionWithClusterStateCache_endpoint_queue_completion clusterStateCacheContainer endpoint queue completion =
  do
    cls' <- getRequiredClass "MTRBaseClusterActivatedCarbonFilterMonitoring"
    sendClassMessage cls' readAttributeClusterRevisionWithClusterStateCache_endpoint_queue_completionSelector (toMTRClusterStateCacheContainer clusterStateCacheContainer) (toNSNumber endpoint) (toNSObject queue) completion

-- | @- init@
init_ :: IsMTRBaseClusterActivatedCarbonFilterMonitoring mtrBaseClusterActivatedCarbonFilterMonitoring => mtrBaseClusterActivatedCarbonFilterMonitoring -> IO (Id MTRBaseClusterActivatedCarbonFilterMonitoring)
init_ mtrBaseClusterActivatedCarbonFilterMonitoring =
  sendOwnedMessage mtrBaseClusterActivatedCarbonFilterMonitoring initSelector

-- | @+ new@
new :: IO (Id MTRBaseClusterActivatedCarbonFilterMonitoring)
new  =
  do
    cls' <- getRequiredClass "MTRBaseClusterActivatedCarbonFilterMonitoring"
    sendOwnedClassMessage cls' newSelector

-- | For all instance methods (reads, writes, commands) that take a completion, the completion will be called on the provided queue.
--
-- ObjC selector: @- initWithDevice:endpointID:queue:@
initWithDevice_endpointID_queue :: (IsMTRBaseClusterActivatedCarbonFilterMonitoring mtrBaseClusterActivatedCarbonFilterMonitoring, IsMTRBaseDevice device, IsNSNumber endpointID, IsNSObject queue) => mtrBaseClusterActivatedCarbonFilterMonitoring -> device -> endpointID -> queue -> IO (Id MTRBaseClusterActivatedCarbonFilterMonitoring)
initWithDevice_endpointID_queue mtrBaseClusterActivatedCarbonFilterMonitoring device endpointID queue =
  sendOwnedMessage mtrBaseClusterActivatedCarbonFilterMonitoring initWithDevice_endpointID_queueSelector (toMTRBaseDevice device) (toNSNumber endpointID) (toNSObject queue)

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @resetConditionWithParams:completion:@
resetConditionWithParams_completionSelector :: Selector '[Id MTRActivatedCarbonFilterMonitoringClusterResetConditionParams, Ptr ()] ()
resetConditionWithParams_completionSelector = mkSelector "resetConditionWithParams:completion:"

-- | @Selector@ for @resetConditionWithCompletion:@
resetConditionWithCompletionSelector :: Selector '[Ptr ()] ()
resetConditionWithCompletionSelector = mkSelector "resetConditionWithCompletion:"

-- | @Selector@ for @readAttributeConditionWithCompletion:@
readAttributeConditionWithCompletionSelector :: Selector '[Ptr ()] ()
readAttributeConditionWithCompletionSelector = mkSelector "readAttributeConditionWithCompletion:"

-- | @Selector@ for @subscribeAttributeConditionWithParams:subscriptionEstablished:reportHandler:@
subscribeAttributeConditionWithParams_subscriptionEstablished_reportHandlerSelector :: Selector '[Id MTRSubscribeParams, Ptr (), Ptr ()] ()
subscribeAttributeConditionWithParams_subscriptionEstablished_reportHandlerSelector = mkSelector "subscribeAttributeConditionWithParams:subscriptionEstablished:reportHandler:"

-- | @Selector@ for @readAttributeConditionWithClusterStateCache:endpoint:queue:completion:@
readAttributeConditionWithClusterStateCache_endpoint_queue_completionSelector :: Selector '[Id MTRClusterStateCacheContainer, Id NSNumber, Id NSObject, Ptr ()] ()
readAttributeConditionWithClusterStateCache_endpoint_queue_completionSelector = mkSelector "readAttributeConditionWithClusterStateCache:endpoint:queue:completion:"

-- | @Selector@ for @readAttributeDegradationDirectionWithCompletion:@
readAttributeDegradationDirectionWithCompletionSelector :: Selector '[Ptr ()] ()
readAttributeDegradationDirectionWithCompletionSelector = mkSelector "readAttributeDegradationDirectionWithCompletion:"

-- | @Selector@ for @subscribeAttributeDegradationDirectionWithParams:subscriptionEstablished:reportHandler:@
subscribeAttributeDegradationDirectionWithParams_subscriptionEstablished_reportHandlerSelector :: Selector '[Id MTRSubscribeParams, Ptr (), Ptr ()] ()
subscribeAttributeDegradationDirectionWithParams_subscriptionEstablished_reportHandlerSelector = mkSelector "subscribeAttributeDegradationDirectionWithParams:subscriptionEstablished:reportHandler:"

-- | @Selector@ for @readAttributeDegradationDirectionWithClusterStateCache:endpoint:queue:completion:@
readAttributeDegradationDirectionWithClusterStateCache_endpoint_queue_completionSelector :: Selector '[Id MTRClusterStateCacheContainer, Id NSNumber, Id NSObject, Ptr ()] ()
readAttributeDegradationDirectionWithClusterStateCache_endpoint_queue_completionSelector = mkSelector "readAttributeDegradationDirectionWithClusterStateCache:endpoint:queue:completion:"

-- | @Selector@ for @readAttributeChangeIndicationWithCompletion:@
readAttributeChangeIndicationWithCompletionSelector :: Selector '[Ptr ()] ()
readAttributeChangeIndicationWithCompletionSelector = mkSelector "readAttributeChangeIndicationWithCompletion:"

-- | @Selector@ for @subscribeAttributeChangeIndicationWithParams:subscriptionEstablished:reportHandler:@
subscribeAttributeChangeIndicationWithParams_subscriptionEstablished_reportHandlerSelector :: Selector '[Id MTRSubscribeParams, Ptr (), Ptr ()] ()
subscribeAttributeChangeIndicationWithParams_subscriptionEstablished_reportHandlerSelector = mkSelector "subscribeAttributeChangeIndicationWithParams:subscriptionEstablished:reportHandler:"

-- | @Selector@ for @readAttributeChangeIndicationWithClusterStateCache:endpoint:queue:completion:@
readAttributeChangeIndicationWithClusterStateCache_endpoint_queue_completionSelector :: Selector '[Id MTRClusterStateCacheContainer, Id NSNumber, Id NSObject, Ptr ()] ()
readAttributeChangeIndicationWithClusterStateCache_endpoint_queue_completionSelector = mkSelector "readAttributeChangeIndicationWithClusterStateCache:endpoint:queue:completion:"

-- | @Selector@ for @readAttributeInPlaceIndicatorWithCompletion:@
readAttributeInPlaceIndicatorWithCompletionSelector :: Selector '[Ptr ()] ()
readAttributeInPlaceIndicatorWithCompletionSelector = mkSelector "readAttributeInPlaceIndicatorWithCompletion:"

-- | @Selector@ for @subscribeAttributeInPlaceIndicatorWithParams:subscriptionEstablished:reportHandler:@
subscribeAttributeInPlaceIndicatorWithParams_subscriptionEstablished_reportHandlerSelector :: Selector '[Id MTRSubscribeParams, Ptr (), Ptr ()] ()
subscribeAttributeInPlaceIndicatorWithParams_subscriptionEstablished_reportHandlerSelector = mkSelector "subscribeAttributeInPlaceIndicatorWithParams:subscriptionEstablished:reportHandler:"

-- | @Selector@ for @readAttributeInPlaceIndicatorWithClusterStateCache:endpoint:queue:completion:@
readAttributeInPlaceIndicatorWithClusterStateCache_endpoint_queue_completionSelector :: Selector '[Id MTRClusterStateCacheContainer, Id NSNumber, Id NSObject, Ptr ()] ()
readAttributeInPlaceIndicatorWithClusterStateCache_endpoint_queue_completionSelector = mkSelector "readAttributeInPlaceIndicatorWithClusterStateCache:endpoint:queue:completion:"

-- | @Selector@ for @readAttributeLastChangedTimeWithCompletion:@
readAttributeLastChangedTimeWithCompletionSelector :: Selector '[Ptr ()] ()
readAttributeLastChangedTimeWithCompletionSelector = mkSelector "readAttributeLastChangedTimeWithCompletion:"

-- | @Selector@ for @writeAttributeLastChangedTimeWithValue:completion:@
writeAttributeLastChangedTimeWithValue_completionSelector :: Selector '[Id NSNumber, Ptr ()] ()
writeAttributeLastChangedTimeWithValue_completionSelector = mkSelector "writeAttributeLastChangedTimeWithValue:completion:"

-- | @Selector@ for @writeAttributeLastChangedTimeWithValue:params:completion:@
writeAttributeLastChangedTimeWithValue_params_completionSelector :: Selector '[Id NSNumber, Id MTRWriteParams, Ptr ()] ()
writeAttributeLastChangedTimeWithValue_params_completionSelector = mkSelector "writeAttributeLastChangedTimeWithValue:params:completion:"

-- | @Selector@ for @subscribeAttributeLastChangedTimeWithParams:subscriptionEstablished:reportHandler:@
subscribeAttributeLastChangedTimeWithParams_subscriptionEstablished_reportHandlerSelector :: Selector '[Id MTRSubscribeParams, Ptr (), Ptr ()] ()
subscribeAttributeLastChangedTimeWithParams_subscriptionEstablished_reportHandlerSelector = mkSelector "subscribeAttributeLastChangedTimeWithParams:subscriptionEstablished:reportHandler:"

-- | @Selector@ for @readAttributeLastChangedTimeWithClusterStateCache:endpoint:queue:completion:@
readAttributeLastChangedTimeWithClusterStateCache_endpoint_queue_completionSelector :: Selector '[Id MTRClusterStateCacheContainer, Id NSNumber, Id NSObject, Ptr ()] ()
readAttributeLastChangedTimeWithClusterStateCache_endpoint_queue_completionSelector = mkSelector "readAttributeLastChangedTimeWithClusterStateCache:endpoint:queue:completion:"

-- | @Selector@ for @readAttributeReplacementProductListWithCompletion:@
readAttributeReplacementProductListWithCompletionSelector :: Selector '[Ptr ()] ()
readAttributeReplacementProductListWithCompletionSelector = mkSelector "readAttributeReplacementProductListWithCompletion:"

-- | @Selector@ for @subscribeAttributeReplacementProductListWithParams:subscriptionEstablished:reportHandler:@
subscribeAttributeReplacementProductListWithParams_subscriptionEstablished_reportHandlerSelector :: Selector '[Id MTRSubscribeParams, Ptr (), Ptr ()] ()
subscribeAttributeReplacementProductListWithParams_subscriptionEstablished_reportHandlerSelector = mkSelector "subscribeAttributeReplacementProductListWithParams:subscriptionEstablished:reportHandler:"

-- | @Selector@ for @readAttributeReplacementProductListWithClusterStateCache:endpoint:queue:completion:@
readAttributeReplacementProductListWithClusterStateCache_endpoint_queue_completionSelector :: Selector '[Id MTRClusterStateCacheContainer, Id NSNumber, Id NSObject, Ptr ()] ()
readAttributeReplacementProductListWithClusterStateCache_endpoint_queue_completionSelector = mkSelector "readAttributeReplacementProductListWithClusterStateCache:endpoint:queue:completion:"

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
initSelector :: Selector '[] (Id MTRBaseClusterActivatedCarbonFilterMonitoring)
initSelector = mkSelector "init"

-- | @Selector@ for @new@
newSelector :: Selector '[] (Id MTRBaseClusterActivatedCarbonFilterMonitoring)
newSelector = mkSelector "new"

-- | @Selector@ for @initWithDevice:endpointID:queue:@
initWithDevice_endpointID_queueSelector :: Selector '[Id MTRBaseDevice, Id NSNumber, Id NSObject] (Id MTRBaseClusterActivatedCarbonFilterMonitoring)
initWithDevice_endpointID_queueSelector = mkSelector "initWithDevice:endpointID:queue:"

