{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Cluster HEPA Filter Monitoring
--
-- Attributes and commands for monitoring HEPA filters in a device
--
-- Generated bindings for @MTRBaseClusterHEPAFilterMonitoring@.
module ObjC.Matter.MTRBaseClusterHEPAFilterMonitoring
  ( MTRBaseClusterHEPAFilterMonitoring
  , IsMTRBaseClusterHEPAFilterMonitoring(..)
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
resetConditionWithParams_completion :: (IsMTRBaseClusterHEPAFilterMonitoring mtrBaseClusterHEPAFilterMonitoring, IsMTRHEPAFilterMonitoringClusterResetConditionParams params) => mtrBaseClusterHEPAFilterMonitoring -> params -> Ptr () -> IO ()
resetConditionWithParams_completion mtrBaseClusterHEPAFilterMonitoring params completion =
  sendMessage mtrBaseClusterHEPAFilterMonitoring resetConditionWithParams_completionSelector (toMTRHEPAFilterMonitoringClusterResetConditionParams params) completion

-- | @- resetConditionWithCompletion:@
resetConditionWithCompletion :: IsMTRBaseClusterHEPAFilterMonitoring mtrBaseClusterHEPAFilterMonitoring => mtrBaseClusterHEPAFilterMonitoring -> Ptr () -> IO ()
resetConditionWithCompletion mtrBaseClusterHEPAFilterMonitoring completion =
  sendMessage mtrBaseClusterHEPAFilterMonitoring resetConditionWithCompletionSelector completion

-- | @- readAttributeConditionWithCompletion:@
readAttributeConditionWithCompletion :: IsMTRBaseClusterHEPAFilterMonitoring mtrBaseClusterHEPAFilterMonitoring => mtrBaseClusterHEPAFilterMonitoring -> Ptr () -> IO ()
readAttributeConditionWithCompletion mtrBaseClusterHEPAFilterMonitoring completion =
  sendMessage mtrBaseClusterHEPAFilterMonitoring readAttributeConditionWithCompletionSelector completion

-- | @- subscribeAttributeConditionWithParams:subscriptionEstablished:reportHandler:@
subscribeAttributeConditionWithParams_subscriptionEstablished_reportHandler :: (IsMTRBaseClusterHEPAFilterMonitoring mtrBaseClusterHEPAFilterMonitoring, IsMTRSubscribeParams params) => mtrBaseClusterHEPAFilterMonitoring -> params -> Ptr () -> Ptr () -> IO ()
subscribeAttributeConditionWithParams_subscriptionEstablished_reportHandler mtrBaseClusterHEPAFilterMonitoring params subscriptionEstablished reportHandler =
  sendMessage mtrBaseClusterHEPAFilterMonitoring subscribeAttributeConditionWithParams_subscriptionEstablished_reportHandlerSelector (toMTRSubscribeParams params) subscriptionEstablished reportHandler

-- | @+ readAttributeConditionWithClusterStateCache:endpoint:queue:completion:@
readAttributeConditionWithClusterStateCache_endpoint_queue_completion :: (IsMTRClusterStateCacheContainer clusterStateCacheContainer, IsNSNumber endpoint, IsNSObject queue) => clusterStateCacheContainer -> endpoint -> queue -> Ptr () -> IO ()
readAttributeConditionWithClusterStateCache_endpoint_queue_completion clusterStateCacheContainer endpoint queue completion =
  do
    cls' <- getRequiredClass "MTRBaseClusterHEPAFilterMonitoring"
    sendClassMessage cls' readAttributeConditionWithClusterStateCache_endpoint_queue_completionSelector (toMTRClusterStateCacheContainer clusterStateCacheContainer) (toNSNumber endpoint) (toNSObject queue) completion

-- | @- readAttributeDegradationDirectionWithCompletion:@
readAttributeDegradationDirectionWithCompletion :: IsMTRBaseClusterHEPAFilterMonitoring mtrBaseClusterHEPAFilterMonitoring => mtrBaseClusterHEPAFilterMonitoring -> Ptr () -> IO ()
readAttributeDegradationDirectionWithCompletion mtrBaseClusterHEPAFilterMonitoring completion =
  sendMessage mtrBaseClusterHEPAFilterMonitoring readAttributeDegradationDirectionWithCompletionSelector completion

-- | @- subscribeAttributeDegradationDirectionWithParams:subscriptionEstablished:reportHandler:@
subscribeAttributeDegradationDirectionWithParams_subscriptionEstablished_reportHandler :: (IsMTRBaseClusterHEPAFilterMonitoring mtrBaseClusterHEPAFilterMonitoring, IsMTRSubscribeParams params) => mtrBaseClusterHEPAFilterMonitoring -> params -> Ptr () -> Ptr () -> IO ()
subscribeAttributeDegradationDirectionWithParams_subscriptionEstablished_reportHandler mtrBaseClusterHEPAFilterMonitoring params subscriptionEstablished reportHandler =
  sendMessage mtrBaseClusterHEPAFilterMonitoring subscribeAttributeDegradationDirectionWithParams_subscriptionEstablished_reportHandlerSelector (toMTRSubscribeParams params) subscriptionEstablished reportHandler

-- | @+ readAttributeDegradationDirectionWithClusterStateCache:endpoint:queue:completion:@
readAttributeDegradationDirectionWithClusterStateCache_endpoint_queue_completion :: (IsMTRClusterStateCacheContainer clusterStateCacheContainer, IsNSNumber endpoint, IsNSObject queue) => clusterStateCacheContainer -> endpoint -> queue -> Ptr () -> IO ()
readAttributeDegradationDirectionWithClusterStateCache_endpoint_queue_completion clusterStateCacheContainer endpoint queue completion =
  do
    cls' <- getRequiredClass "MTRBaseClusterHEPAFilterMonitoring"
    sendClassMessage cls' readAttributeDegradationDirectionWithClusterStateCache_endpoint_queue_completionSelector (toMTRClusterStateCacheContainer clusterStateCacheContainer) (toNSNumber endpoint) (toNSObject queue) completion

-- | @- readAttributeChangeIndicationWithCompletion:@
readAttributeChangeIndicationWithCompletion :: IsMTRBaseClusterHEPAFilterMonitoring mtrBaseClusterHEPAFilterMonitoring => mtrBaseClusterHEPAFilterMonitoring -> Ptr () -> IO ()
readAttributeChangeIndicationWithCompletion mtrBaseClusterHEPAFilterMonitoring completion =
  sendMessage mtrBaseClusterHEPAFilterMonitoring readAttributeChangeIndicationWithCompletionSelector completion

-- | @- subscribeAttributeChangeIndicationWithParams:subscriptionEstablished:reportHandler:@
subscribeAttributeChangeIndicationWithParams_subscriptionEstablished_reportHandler :: (IsMTRBaseClusterHEPAFilterMonitoring mtrBaseClusterHEPAFilterMonitoring, IsMTRSubscribeParams params) => mtrBaseClusterHEPAFilterMonitoring -> params -> Ptr () -> Ptr () -> IO ()
subscribeAttributeChangeIndicationWithParams_subscriptionEstablished_reportHandler mtrBaseClusterHEPAFilterMonitoring params subscriptionEstablished reportHandler =
  sendMessage mtrBaseClusterHEPAFilterMonitoring subscribeAttributeChangeIndicationWithParams_subscriptionEstablished_reportHandlerSelector (toMTRSubscribeParams params) subscriptionEstablished reportHandler

-- | @+ readAttributeChangeIndicationWithClusterStateCache:endpoint:queue:completion:@
readAttributeChangeIndicationWithClusterStateCache_endpoint_queue_completion :: (IsMTRClusterStateCacheContainer clusterStateCacheContainer, IsNSNumber endpoint, IsNSObject queue) => clusterStateCacheContainer -> endpoint -> queue -> Ptr () -> IO ()
readAttributeChangeIndicationWithClusterStateCache_endpoint_queue_completion clusterStateCacheContainer endpoint queue completion =
  do
    cls' <- getRequiredClass "MTRBaseClusterHEPAFilterMonitoring"
    sendClassMessage cls' readAttributeChangeIndicationWithClusterStateCache_endpoint_queue_completionSelector (toMTRClusterStateCacheContainer clusterStateCacheContainer) (toNSNumber endpoint) (toNSObject queue) completion

-- | @- readAttributeInPlaceIndicatorWithCompletion:@
readAttributeInPlaceIndicatorWithCompletion :: IsMTRBaseClusterHEPAFilterMonitoring mtrBaseClusterHEPAFilterMonitoring => mtrBaseClusterHEPAFilterMonitoring -> Ptr () -> IO ()
readAttributeInPlaceIndicatorWithCompletion mtrBaseClusterHEPAFilterMonitoring completion =
  sendMessage mtrBaseClusterHEPAFilterMonitoring readAttributeInPlaceIndicatorWithCompletionSelector completion

-- | @- subscribeAttributeInPlaceIndicatorWithParams:subscriptionEstablished:reportHandler:@
subscribeAttributeInPlaceIndicatorWithParams_subscriptionEstablished_reportHandler :: (IsMTRBaseClusterHEPAFilterMonitoring mtrBaseClusterHEPAFilterMonitoring, IsMTRSubscribeParams params) => mtrBaseClusterHEPAFilterMonitoring -> params -> Ptr () -> Ptr () -> IO ()
subscribeAttributeInPlaceIndicatorWithParams_subscriptionEstablished_reportHandler mtrBaseClusterHEPAFilterMonitoring params subscriptionEstablished reportHandler =
  sendMessage mtrBaseClusterHEPAFilterMonitoring subscribeAttributeInPlaceIndicatorWithParams_subscriptionEstablished_reportHandlerSelector (toMTRSubscribeParams params) subscriptionEstablished reportHandler

-- | @+ readAttributeInPlaceIndicatorWithClusterStateCache:endpoint:queue:completion:@
readAttributeInPlaceIndicatorWithClusterStateCache_endpoint_queue_completion :: (IsMTRClusterStateCacheContainer clusterStateCacheContainer, IsNSNumber endpoint, IsNSObject queue) => clusterStateCacheContainer -> endpoint -> queue -> Ptr () -> IO ()
readAttributeInPlaceIndicatorWithClusterStateCache_endpoint_queue_completion clusterStateCacheContainer endpoint queue completion =
  do
    cls' <- getRequiredClass "MTRBaseClusterHEPAFilterMonitoring"
    sendClassMessage cls' readAttributeInPlaceIndicatorWithClusterStateCache_endpoint_queue_completionSelector (toMTRClusterStateCacheContainer clusterStateCacheContainer) (toNSNumber endpoint) (toNSObject queue) completion

-- | @- readAttributeLastChangedTimeWithCompletion:@
readAttributeLastChangedTimeWithCompletion :: IsMTRBaseClusterHEPAFilterMonitoring mtrBaseClusterHEPAFilterMonitoring => mtrBaseClusterHEPAFilterMonitoring -> Ptr () -> IO ()
readAttributeLastChangedTimeWithCompletion mtrBaseClusterHEPAFilterMonitoring completion =
  sendMessage mtrBaseClusterHEPAFilterMonitoring readAttributeLastChangedTimeWithCompletionSelector completion

-- | @- writeAttributeLastChangedTimeWithValue:completion:@
writeAttributeLastChangedTimeWithValue_completion :: (IsMTRBaseClusterHEPAFilterMonitoring mtrBaseClusterHEPAFilterMonitoring, IsNSNumber value) => mtrBaseClusterHEPAFilterMonitoring -> value -> Ptr () -> IO ()
writeAttributeLastChangedTimeWithValue_completion mtrBaseClusterHEPAFilterMonitoring value completion =
  sendMessage mtrBaseClusterHEPAFilterMonitoring writeAttributeLastChangedTimeWithValue_completionSelector (toNSNumber value) completion

-- | @- writeAttributeLastChangedTimeWithValue:params:completion:@
writeAttributeLastChangedTimeWithValue_params_completion :: (IsMTRBaseClusterHEPAFilterMonitoring mtrBaseClusterHEPAFilterMonitoring, IsNSNumber value, IsMTRWriteParams params) => mtrBaseClusterHEPAFilterMonitoring -> value -> params -> Ptr () -> IO ()
writeAttributeLastChangedTimeWithValue_params_completion mtrBaseClusterHEPAFilterMonitoring value params completion =
  sendMessage mtrBaseClusterHEPAFilterMonitoring writeAttributeLastChangedTimeWithValue_params_completionSelector (toNSNumber value) (toMTRWriteParams params) completion

-- | @- subscribeAttributeLastChangedTimeWithParams:subscriptionEstablished:reportHandler:@
subscribeAttributeLastChangedTimeWithParams_subscriptionEstablished_reportHandler :: (IsMTRBaseClusterHEPAFilterMonitoring mtrBaseClusterHEPAFilterMonitoring, IsMTRSubscribeParams params) => mtrBaseClusterHEPAFilterMonitoring -> params -> Ptr () -> Ptr () -> IO ()
subscribeAttributeLastChangedTimeWithParams_subscriptionEstablished_reportHandler mtrBaseClusterHEPAFilterMonitoring params subscriptionEstablished reportHandler =
  sendMessage mtrBaseClusterHEPAFilterMonitoring subscribeAttributeLastChangedTimeWithParams_subscriptionEstablished_reportHandlerSelector (toMTRSubscribeParams params) subscriptionEstablished reportHandler

-- | @+ readAttributeLastChangedTimeWithClusterStateCache:endpoint:queue:completion:@
readAttributeLastChangedTimeWithClusterStateCache_endpoint_queue_completion :: (IsMTRClusterStateCacheContainer clusterStateCacheContainer, IsNSNumber endpoint, IsNSObject queue) => clusterStateCacheContainer -> endpoint -> queue -> Ptr () -> IO ()
readAttributeLastChangedTimeWithClusterStateCache_endpoint_queue_completion clusterStateCacheContainer endpoint queue completion =
  do
    cls' <- getRequiredClass "MTRBaseClusterHEPAFilterMonitoring"
    sendClassMessage cls' readAttributeLastChangedTimeWithClusterStateCache_endpoint_queue_completionSelector (toMTRClusterStateCacheContainer clusterStateCacheContainer) (toNSNumber endpoint) (toNSObject queue) completion

-- | @- readAttributeReplacementProductListWithCompletion:@
readAttributeReplacementProductListWithCompletion :: IsMTRBaseClusterHEPAFilterMonitoring mtrBaseClusterHEPAFilterMonitoring => mtrBaseClusterHEPAFilterMonitoring -> Ptr () -> IO ()
readAttributeReplacementProductListWithCompletion mtrBaseClusterHEPAFilterMonitoring completion =
  sendMessage mtrBaseClusterHEPAFilterMonitoring readAttributeReplacementProductListWithCompletionSelector completion

-- | @- subscribeAttributeReplacementProductListWithParams:subscriptionEstablished:reportHandler:@
subscribeAttributeReplacementProductListWithParams_subscriptionEstablished_reportHandler :: (IsMTRBaseClusterHEPAFilterMonitoring mtrBaseClusterHEPAFilterMonitoring, IsMTRSubscribeParams params) => mtrBaseClusterHEPAFilterMonitoring -> params -> Ptr () -> Ptr () -> IO ()
subscribeAttributeReplacementProductListWithParams_subscriptionEstablished_reportHandler mtrBaseClusterHEPAFilterMonitoring params subscriptionEstablished reportHandler =
  sendMessage mtrBaseClusterHEPAFilterMonitoring subscribeAttributeReplacementProductListWithParams_subscriptionEstablished_reportHandlerSelector (toMTRSubscribeParams params) subscriptionEstablished reportHandler

-- | @+ readAttributeReplacementProductListWithClusterStateCache:endpoint:queue:completion:@
readAttributeReplacementProductListWithClusterStateCache_endpoint_queue_completion :: (IsMTRClusterStateCacheContainer clusterStateCacheContainer, IsNSNumber endpoint, IsNSObject queue) => clusterStateCacheContainer -> endpoint -> queue -> Ptr () -> IO ()
readAttributeReplacementProductListWithClusterStateCache_endpoint_queue_completion clusterStateCacheContainer endpoint queue completion =
  do
    cls' <- getRequiredClass "MTRBaseClusterHEPAFilterMonitoring"
    sendClassMessage cls' readAttributeReplacementProductListWithClusterStateCache_endpoint_queue_completionSelector (toMTRClusterStateCacheContainer clusterStateCacheContainer) (toNSNumber endpoint) (toNSObject queue) completion

-- | @- readAttributeGeneratedCommandListWithCompletion:@
readAttributeGeneratedCommandListWithCompletion :: IsMTRBaseClusterHEPAFilterMonitoring mtrBaseClusterHEPAFilterMonitoring => mtrBaseClusterHEPAFilterMonitoring -> Ptr () -> IO ()
readAttributeGeneratedCommandListWithCompletion mtrBaseClusterHEPAFilterMonitoring completion =
  sendMessage mtrBaseClusterHEPAFilterMonitoring readAttributeGeneratedCommandListWithCompletionSelector completion

-- | @- subscribeAttributeGeneratedCommandListWithParams:subscriptionEstablished:reportHandler:@
subscribeAttributeGeneratedCommandListWithParams_subscriptionEstablished_reportHandler :: (IsMTRBaseClusterHEPAFilterMonitoring mtrBaseClusterHEPAFilterMonitoring, IsMTRSubscribeParams params) => mtrBaseClusterHEPAFilterMonitoring -> params -> Ptr () -> Ptr () -> IO ()
subscribeAttributeGeneratedCommandListWithParams_subscriptionEstablished_reportHandler mtrBaseClusterHEPAFilterMonitoring params subscriptionEstablished reportHandler =
  sendMessage mtrBaseClusterHEPAFilterMonitoring subscribeAttributeGeneratedCommandListWithParams_subscriptionEstablished_reportHandlerSelector (toMTRSubscribeParams params) subscriptionEstablished reportHandler

-- | @+ readAttributeGeneratedCommandListWithClusterStateCache:endpoint:queue:completion:@
readAttributeGeneratedCommandListWithClusterStateCache_endpoint_queue_completion :: (IsMTRClusterStateCacheContainer clusterStateCacheContainer, IsNSNumber endpoint, IsNSObject queue) => clusterStateCacheContainer -> endpoint -> queue -> Ptr () -> IO ()
readAttributeGeneratedCommandListWithClusterStateCache_endpoint_queue_completion clusterStateCacheContainer endpoint queue completion =
  do
    cls' <- getRequiredClass "MTRBaseClusterHEPAFilterMonitoring"
    sendClassMessage cls' readAttributeGeneratedCommandListWithClusterStateCache_endpoint_queue_completionSelector (toMTRClusterStateCacheContainer clusterStateCacheContainer) (toNSNumber endpoint) (toNSObject queue) completion

-- | @- readAttributeAcceptedCommandListWithCompletion:@
readAttributeAcceptedCommandListWithCompletion :: IsMTRBaseClusterHEPAFilterMonitoring mtrBaseClusterHEPAFilterMonitoring => mtrBaseClusterHEPAFilterMonitoring -> Ptr () -> IO ()
readAttributeAcceptedCommandListWithCompletion mtrBaseClusterHEPAFilterMonitoring completion =
  sendMessage mtrBaseClusterHEPAFilterMonitoring readAttributeAcceptedCommandListWithCompletionSelector completion

-- | @- subscribeAttributeAcceptedCommandListWithParams:subscriptionEstablished:reportHandler:@
subscribeAttributeAcceptedCommandListWithParams_subscriptionEstablished_reportHandler :: (IsMTRBaseClusterHEPAFilterMonitoring mtrBaseClusterHEPAFilterMonitoring, IsMTRSubscribeParams params) => mtrBaseClusterHEPAFilterMonitoring -> params -> Ptr () -> Ptr () -> IO ()
subscribeAttributeAcceptedCommandListWithParams_subscriptionEstablished_reportHandler mtrBaseClusterHEPAFilterMonitoring params subscriptionEstablished reportHandler =
  sendMessage mtrBaseClusterHEPAFilterMonitoring subscribeAttributeAcceptedCommandListWithParams_subscriptionEstablished_reportHandlerSelector (toMTRSubscribeParams params) subscriptionEstablished reportHandler

-- | @+ readAttributeAcceptedCommandListWithClusterStateCache:endpoint:queue:completion:@
readAttributeAcceptedCommandListWithClusterStateCache_endpoint_queue_completion :: (IsMTRClusterStateCacheContainer clusterStateCacheContainer, IsNSNumber endpoint, IsNSObject queue) => clusterStateCacheContainer -> endpoint -> queue -> Ptr () -> IO ()
readAttributeAcceptedCommandListWithClusterStateCache_endpoint_queue_completion clusterStateCacheContainer endpoint queue completion =
  do
    cls' <- getRequiredClass "MTRBaseClusterHEPAFilterMonitoring"
    sendClassMessage cls' readAttributeAcceptedCommandListWithClusterStateCache_endpoint_queue_completionSelector (toMTRClusterStateCacheContainer clusterStateCacheContainer) (toNSNumber endpoint) (toNSObject queue) completion

-- | @- readAttributeAttributeListWithCompletion:@
readAttributeAttributeListWithCompletion :: IsMTRBaseClusterHEPAFilterMonitoring mtrBaseClusterHEPAFilterMonitoring => mtrBaseClusterHEPAFilterMonitoring -> Ptr () -> IO ()
readAttributeAttributeListWithCompletion mtrBaseClusterHEPAFilterMonitoring completion =
  sendMessage mtrBaseClusterHEPAFilterMonitoring readAttributeAttributeListWithCompletionSelector completion

-- | @- subscribeAttributeAttributeListWithParams:subscriptionEstablished:reportHandler:@
subscribeAttributeAttributeListWithParams_subscriptionEstablished_reportHandler :: (IsMTRBaseClusterHEPAFilterMonitoring mtrBaseClusterHEPAFilterMonitoring, IsMTRSubscribeParams params) => mtrBaseClusterHEPAFilterMonitoring -> params -> Ptr () -> Ptr () -> IO ()
subscribeAttributeAttributeListWithParams_subscriptionEstablished_reportHandler mtrBaseClusterHEPAFilterMonitoring params subscriptionEstablished reportHandler =
  sendMessage mtrBaseClusterHEPAFilterMonitoring subscribeAttributeAttributeListWithParams_subscriptionEstablished_reportHandlerSelector (toMTRSubscribeParams params) subscriptionEstablished reportHandler

-- | @+ readAttributeAttributeListWithClusterStateCache:endpoint:queue:completion:@
readAttributeAttributeListWithClusterStateCache_endpoint_queue_completion :: (IsMTRClusterStateCacheContainer clusterStateCacheContainer, IsNSNumber endpoint, IsNSObject queue) => clusterStateCacheContainer -> endpoint -> queue -> Ptr () -> IO ()
readAttributeAttributeListWithClusterStateCache_endpoint_queue_completion clusterStateCacheContainer endpoint queue completion =
  do
    cls' <- getRequiredClass "MTRBaseClusterHEPAFilterMonitoring"
    sendClassMessage cls' readAttributeAttributeListWithClusterStateCache_endpoint_queue_completionSelector (toMTRClusterStateCacheContainer clusterStateCacheContainer) (toNSNumber endpoint) (toNSObject queue) completion

-- | @- readAttributeFeatureMapWithCompletion:@
readAttributeFeatureMapWithCompletion :: IsMTRBaseClusterHEPAFilterMonitoring mtrBaseClusterHEPAFilterMonitoring => mtrBaseClusterHEPAFilterMonitoring -> Ptr () -> IO ()
readAttributeFeatureMapWithCompletion mtrBaseClusterHEPAFilterMonitoring completion =
  sendMessage mtrBaseClusterHEPAFilterMonitoring readAttributeFeatureMapWithCompletionSelector completion

-- | @- subscribeAttributeFeatureMapWithParams:subscriptionEstablished:reportHandler:@
subscribeAttributeFeatureMapWithParams_subscriptionEstablished_reportHandler :: (IsMTRBaseClusterHEPAFilterMonitoring mtrBaseClusterHEPAFilterMonitoring, IsMTRSubscribeParams params) => mtrBaseClusterHEPAFilterMonitoring -> params -> Ptr () -> Ptr () -> IO ()
subscribeAttributeFeatureMapWithParams_subscriptionEstablished_reportHandler mtrBaseClusterHEPAFilterMonitoring params subscriptionEstablished reportHandler =
  sendMessage mtrBaseClusterHEPAFilterMonitoring subscribeAttributeFeatureMapWithParams_subscriptionEstablished_reportHandlerSelector (toMTRSubscribeParams params) subscriptionEstablished reportHandler

-- | @+ readAttributeFeatureMapWithClusterStateCache:endpoint:queue:completion:@
readAttributeFeatureMapWithClusterStateCache_endpoint_queue_completion :: (IsMTRClusterStateCacheContainer clusterStateCacheContainer, IsNSNumber endpoint, IsNSObject queue) => clusterStateCacheContainer -> endpoint -> queue -> Ptr () -> IO ()
readAttributeFeatureMapWithClusterStateCache_endpoint_queue_completion clusterStateCacheContainer endpoint queue completion =
  do
    cls' <- getRequiredClass "MTRBaseClusterHEPAFilterMonitoring"
    sendClassMessage cls' readAttributeFeatureMapWithClusterStateCache_endpoint_queue_completionSelector (toMTRClusterStateCacheContainer clusterStateCacheContainer) (toNSNumber endpoint) (toNSObject queue) completion

-- | @- readAttributeClusterRevisionWithCompletion:@
readAttributeClusterRevisionWithCompletion :: IsMTRBaseClusterHEPAFilterMonitoring mtrBaseClusterHEPAFilterMonitoring => mtrBaseClusterHEPAFilterMonitoring -> Ptr () -> IO ()
readAttributeClusterRevisionWithCompletion mtrBaseClusterHEPAFilterMonitoring completion =
  sendMessage mtrBaseClusterHEPAFilterMonitoring readAttributeClusterRevisionWithCompletionSelector completion

-- | @- subscribeAttributeClusterRevisionWithParams:subscriptionEstablished:reportHandler:@
subscribeAttributeClusterRevisionWithParams_subscriptionEstablished_reportHandler :: (IsMTRBaseClusterHEPAFilterMonitoring mtrBaseClusterHEPAFilterMonitoring, IsMTRSubscribeParams params) => mtrBaseClusterHEPAFilterMonitoring -> params -> Ptr () -> Ptr () -> IO ()
subscribeAttributeClusterRevisionWithParams_subscriptionEstablished_reportHandler mtrBaseClusterHEPAFilterMonitoring params subscriptionEstablished reportHandler =
  sendMessage mtrBaseClusterHEPAFilterMonitoring subscribeAttributeClusterRevisionWithParams_subscriptionEstablished_reportHandlerSelector (toMTRSubscribeParams params) subscriptionEstablished reportHandler

-- | @+ readAttributeClusterRevisionWithClusterStateCache:endpoint:queue:completion:@
readAttributeClusterRevisionWithClusterStateCache_endpoint_queue_completion :: (IsMTRClusterStateCacheContainer clusterStateCacheContainer, IsNSNumber endpoint, IsNSObject queue) => clusterStateCacheContainer -> endpoint -> queue -> Ptr () -> IO ()
readAttributeClusterRevisionWithClusterStateCache_endpoint_queue_completion clusterStateCacheContainer endpoint queue completion =
  do
    cls' <- getRequiredClass "MTRBaseClusterHEPAFilterMonitoring"
    sendClassMessage cls' readAttributeClusterRevisionWithClusterStateCache_endpoint_queue_completionSelector (toMTRClusterStateCacheContainer clusterStateCacheContainer) (toNSNumber endpoint) (toNSObject queue) completion

-- | @- init@
init_ :: IsMTRBaseClusterHEPAFilterMonitoring mtrBaseClusterHEPAFilterMonitoring => mtrBaseClusterHEPAFilterMonitoring -> IO (Id MTRBaseClusterHEPAFilterMonitoring)
init_ mtrBaseClusterHEPAFilterMonitoring =
  sendOwnedMessage mtrBaseClusterHEPAFilterMonitoring initSelector

-- | @+ new@
new :: IO (Id MTRBaseClusterHEPAFilterMonitoring)
new  =
  do
    cls' <- getRequiredClass "MTRBaseClusterHEPAFilterMonitoring"
    sendOwnedClassMessage cls' newSelector

-- | For all instance methods (reads, writes, commands) that take a completion, the completion will be called on the provided queue.
--
-- ObjC selector: @- initWithDevice:endpointID:queue:@
initWithDevice_endpointID_queue :: (IsMTRBaseClusterHEPAFilterMonitoring mtrBaseClusterHEPAFilterMonitoring, IsMTRBaseDevice device, IsNSNumber endpointID, IsNSObject queue) => mtrBaseClusterHEPAFilterMonitoring -> device -> endpointID -> queue -> IO (Id MTRBaseClusterHEPAFilterMonitoring)
initWithDevice_endpointID_queue mtrBaseClusterHEPAFilterMonitoring device endpointID queue =
  sendOwnedMessage mtrBaseClusterHEPAFilterMonitoring initWithDevice_endpointID_queueSelector (toMTRBaseDevice device) (toNSNumber endpointID) (toNSObject queue)

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @resetConditionWithParams:completion:@
resetConditionWithParams_completionSelector :: Selector '[Id MTRHEPAFilterMonitoringClusterResetConditionParams, Ptr ()] ()
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
initSelector :: Selector '[] (Id MTRBaseClusterHEPAFilterMonitoring)
initSelector = mkSelector "init"

-- | @Selector@ for @new@
newSelector :: Selector '[] (Id MTRBaseClusterHEPAFilterMonitoring)
newSelector = mkSelector "new"

-- | @Selector@ for @initWithDevice:endpointID:queue:@
initWithDevice_endpointID_queueSelector :: Selector '[Id MTRBaseDevice, Id NSNumber, Id NSObject] (Id MTRBaseClusterHEPAFilterMonitoring)
initWithDevice_endpointID_queueSelector = mkSelector "initWithDevice:endpointID:queue:"

