{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Cluster Software Diagnostics
--
-- The Software Diagnostics Cluster provides a means to acquire standardized diagnostics metrics that MAY be used by a Node to assist a user or Administrative Node in diagnosing potential problems.
--
-- Generated bindings for @MTRBaseClusterSoftwareDiagnostics@.
module ObjC.Matter.MTRBaseClusterSoftwareDiagnostics
  ( MTRBaseClusterSoftwareDiagnostics
  , IsMTRBaseClusterSoftwareDiagnostics(..)
  , resetWatermarksWithParams_completion
  , resetWatermarksWithCompletion
  , readAttributeThreadMetricsWithCompletion
  , subscribeAttributeThreadMetricsWithParams_subscriptionEstablished_reportHandler
  , readAttributeThreadMetricsWithClusterStateCache_endpoint_queue_completion
  , readAttributeCurrentHeapFreeWithCompletion
  , subscribeAttributeCurrentHeapFreeWithParams_subscriptionEstablished_reportHandler
  , readAttributeCurrentHeapFreeWithClusterStateCache_endpoint_queue_completion
  , readAttributeCurrentHeapUsedWithCompletion
  , subscribeAttributeCurrentHeapUsedWithParams_subscriptionEstablished_reportHandler
  , readAttributeCurrentHeapUsedWithClusterStateCache_endpoint_queue_completion
  , readAttributeCurrentHeapHighWatermarkWithCompletion
  , subscribeAttributeCurrentHeapHighWatermarkWithParams_subscriptionEstablished_reportHandler
  , readAttributeCurrentHeapHighWatermarkWithClusterStateCache_endpoint_queue_completion
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
  , resetWatermarksWithParams_completionHandler
  , resetWatermarksWithCompletionHandler
  , readAttributeThreadMetricsWithCompletionHandler
  , subscribeAttributeThreadMetricsWithMinInterval_maxInterval_params_subscriptionEstablished_reportHandler
  , readAttributeThreadMetricsWithAttributeCache_endpoint_queue_completionHandler
  , readAttributeCurrentHeapFreeWithCompletionHandler
  , subscribeAttributeCurrentHeapFreeWithMinInterval_maxInterval_params_subscriptionEstablished_reportHandler
  , readAttributeCurrentHeapFreeWithAttributeCache_endpoint_queue_completionHandler
  , readAttributeCurrentHeapUsedWithCompletionHandler
  , subscribeAttributeCurrentHeapUsedWithMinInterval_maxInterval_params_subscriptionEstablished_reportHandler
  , readAttributeCurrentHeapUsedWithAttributeCache_endpoint_queue_completionHandler
  , readAttributeCurrentHeapHighWatermarkWithCompletionHandler
  , subscribeAttributeCurrentHeapHighWatermarkWithMinInterval_maxInterval_params_subscriptionEstablished_reportHandler
  , readAttributeCurrentHeapHighWatermarkWithAttributeCache_endpoint_queue_completionHandler
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
  , readAttributeClusterRevisionWithAttributeCache_endpoint_queue_completionHandlerSelector
  , readAttributeClusterRevisionWithClusterStateCache_endpoint_queue_completionSelector
  , readAttributeClusterRevisionWithCompletionHandlerSelector
  , readAttributeClusterRevisionWithCompletionSelector
  , readAttributeCurrentHeapFreeWithAttributeCache_endpoint_queue_completionHandlerSelector
  , readAttributeCurrentHeapFreeWithClusterStateCache_endpoint_queue_completionSelector
  , readAttributeCurrentHeapFreeWithCompletionHandlerSelector
  , readAttributeCurrentHeapFreeWithCompletionSelector
  , readAttributeCurrentHeapHighWatermarkWithAttributeCache_endpoint_queue_completionHandlerSelector
  , readAttributeCurrentHeapHighWatermarkWithClusterStateCache_endpoint_queue_completionSelector
  , readAttributeCurrentHeapHighWatermarkWithCompletionHandlerSelector
  , readAttributeCurrentHeapHighWatermarkWithCompletionSelector
  , readAttributeCurrentHeapUsedWithAttributeCache_endpoint_queue_completionHandlerSelector
  , readAttributeCurrentHeapUsedWithClusterStateCache_endpoint_queue_completionSelector
  , readAttributeCurrentHeapUsedWithCompletionHandlerSelector
  , readAttributeCurrentHeapUsedWithCompletionSelector
  , readAttributeFeatureMapWithAttributeCache_endpoint_queue_completionHandlerSelector
  , readAttributeFeatureMapWithClusterStateCache_endpoint_queue_completionSelector
  , readAttributeFeatureMapWithCompletionHandlerSelector
  , readAttributeFeatureMapWithCompletionSelector
  , readAttributeGeneratedCommandListWithAttributeCache_endpoint_queue_completionHandlerSelector
  , readAttributeGeneratedCommandListWithClusterStateCache_endpoint_queue_completionSelector
  , readAttributeGeneratedCommandListWithCompletionHandlerSelector
  , readAttributeGeneratedCommandListWithCompletionSelector
  , readAttributeThreadMetricsWithAttributeCache_endpoint_queue_completionHandlerSelector
  , readAttributeThreadMetricsWithClusterStateCache_endpoint_queue_completionSelector
  , readAttributeThreadMetricsWithCompletionHandlerSelector
  , readAttributeThreadMetricsWithCompletionSelector
  , resetWatermarksWithCompletionHandlerSelector
  , resetWatermarksWithCompletionSelector
  , resetWatermarksWithParams_completionHandlerSelector
  , resetWatermarksWithParams_completionSelector
  , subscribeAttributeAcceptedCommandListWithMinInterval_maxInterval_params_subscriptionEstablished_reportHandlerSelector
  , subscribeAttributeAcceptedCommandListWithParams_subscriptionEstablished_reportHandlerSelector
  , subscribeAttributeAttributeListWithMinInterval_maxInterval_params_subscriptionEstablished_reportHandlerSelector
  , subscribeAttributeAttributeListWithParams_subscriptionEstablished_reportHandlerSelector
  , subscribeAttributeClusterRevisionWithMinInterval_maxInterval_params_subscriptionEstablished_reportHandlerSelector
  , subscribeAttributeClusterRevisionWithParams_subscriptionEstablished_reportHandlerSelector
  , subscribeAttributeCurrentHeapFreeWithMinInterval_maxInterval_params_subscriptionEstablished_reportHandlerSelector
  , subscribeAttributeCurrentHeapFreeWithParams_subscriptionEstablished_reportHandlerSelector
  , subscribeAttributeCurrentHeapHighWatermarkWithMinInterval_maxInterval_params_subscriptionEstablished_reportHandlerSelector
  , subscribeAttributeCurrentHeapHighWatermarkWithParams_subscriptionEstablished_reportHandlerSelector
  , subscribeAttributeCurrentHeapUsedWithMinInterval_maxInterval_params_subscriptionEstablished_reportHandlerSelector
  , subscribeAttributeCurrentHeapUsedWithParams_subscriptionEstablished_reportHandlerSelector
  , subscribeAttributeFeatureMapWithMinInterval_maxInterval_params_subscriptionEstablished_reportHandlerSelector
  , subscribeAttributeFeatureMapWithParams_subscriptionEstablished_reportHandlerSelector
  , subscribeAttributeGeneratedCommandListWithMinInterval_maxInterval_params_subscriptionEstablished_reportHandlerSelector
  , subscribeAttributeGeneratedCommandListWithParams_subscriptionEstablished_reportHandlerSelector
  , subscribeAttributeThreadMetricsWithMinInterval_maxInterval_params_subscriptionEstablished_reportHandlerSelector
  , subscribeAttributeThreadMetricsWithParams_subscriptionEstablished_reportHandlerSelector


  ) where

import Foreign.Ptr (Ptr, FunPtr)
import Foreign.C.Types

import ObjC.Runtime.Types
import ObjC.Runtime.Message (sendMessage, sendOwnedMessage, sendClassMessage, sendOwnedClassMessage)
import ObjC.Runtime.Selector (mkSelector)
import ObjC.Runtime.Class (getRequiredClass)

import ObjC.Matter.Internal.Classes
import ObjC.Foundation.Internal.Classes

-- | Command ResetWatermarks
--
-- This command is used to reset the high watermarks for heap and stack memory.
--
-- ObjC selector: @- resetWatermarksWithParams:completion:@
resetWatermarksWithParams_completion :: (IsMTRBaseClusterSoftwareDiagnostics mtrBaseClusterSoftwareDiagnostics, IsMTRSoftwareDiagnosticsClusterResetWatermarksParams params) => mtrBaseClusterSoftwareDiagnostics -> params -> Ptr () -> IO ()
resetWatermarksWithParams_completion mtrBaseClusterSoftwareDiagnostics params completion =
  sendMessage mtrBaseClusterSoftwareDiagnostics resetWatermarksWithParams_completionSelector (toMTRSoftwareDiagnosticsClusterResetWatermarksParams params) completion

-- | @- resetWatermarksWithCompletion:@
resetWatermarksWithCompletion :: IsMTRBaseClusterSoftwareDiagnostics mtrBaseClusterSoftwareDiagnostics => mtrBaseClusterSoftwareDiagnostics -> Ptr () -> IO ()
resetWatermarksWithCompletion mtrBaseClusterSoftwareDiagnostics completion =
  sendMessage mtrBaseClusterSoftwareDiagnostics resetWatermarksWithCompletionSelector completion

-- | @- readAttributeThreadMetricsWithCompletion:@
readAttributeThreadMetricsWithCompletion :: IsMTRBaseClusterSoftwareDiagnostics mtrBaseClusterSoftwareDiagnostics => mtrBaseClusterSoftwareDiagnostics -> Ptr () -> IO ()
readAttributeThreadMetricsWithCompletion mtrBaseClusterSoftwareDiagnostics completion =
  sendMessage mtrBaseClusterSoftwareDiagnostics readAttributeThreadMetricsWithCompletionSelector completion

-- | @- subscribeAttributeThreadMetricsWithParams:subscriptionEstablished:reportHandler:@
subscribeAttributeThreadMetricsWithParams_subscriptionEstablished_reportHandler :: (IsMTRBaseClusterSoftwareDiagnostics mtrBaseClusterSoftwareDiagnostics, IsMTRSubscribeParams params) => mtrBaseClusterSoftwareDiagnostics -> params -> Ptr () -> Ptr () -> IO ()
subscribeAttributeThreadMetricsWithParams_subscriptionEstablished_reportHandler mtrBaseClusterSoftwareDiagnostics params subscriptionEstablished reportHandler =
  sendMessage mtrBaseClusterSoftwareDiagnostics subscribeAttributeThreadMetricsWithParams_subscriptionEstablished_reportHandlerSelector (toMTRSubscribeParams params) subscriptionEstablished reportHandler

-- | @+ readAttributeThreadMetricsWithClusterStateCache:endpoint:queue:completion:@
readAttributeThreadMetricsWithClusterStateCache_endpoint_queue_completion :: (IsMTRClusterStateCacheContainer clusterStateCacheContainer, IsNSNumber endpoint, IsNSObject queue) => clusterStateCacheContainer -> endpoint -> queue -> Ptr () -> IO ()
readAttributeThreadMetricsWithClusterStateCache_endpoint_queue_completion clusterStateCacheContainer endpoint queue completion =
  do
    cls' <- getRequiredClass "MTRBaseClusterSoftwareDiagnostics"
    sendClassMessage cls' readAttributeThreadMetricsWithClusterStateCache_endpoint_queue_completionSelector (toMTRClusterStateCacheContainer clusterStateCacheContainer) (toNSNumber endpoint) (toNSObject queue) completion

-- | @- readAttributeCurrentHeapFreeWithCompletion:@
readAttributeCurrentHeapFreeWithCompletion :: IsMTRBaseClusterSoftwareDiagnostics mtrBaseClusterSoftwareDiagnostics => mtrBaseClusterSoftwareDiagnostics -> Ptr () -> IO ()
readAttributeCurrentHeapFreeWithCompletion mtrBaseClusterSoftwareDiagnostics completion =
  sendMessage mtrBaseClusterSoftwareDiagnostics readAttributeCurrentHeapFreeWithCompletionSelector completion

-- | @- subscribeAttributeCurrentHeapFreeWithParams:subscriptionEstablished:reportHandler:@
subscribeAttributeCurrentHeapFreeWithParams_subscriptionEstablished_reportHandler :: (IsMTRBaseClusterSoftwareDiagnostics mtrBaseClusterSoftwareDiagnostics, IsMTRSubscribeParams params) => mtrBaseClusterSoftwareDiagnostics -> params -> Ptr () -> Ptr () -> IO ()
subscribeAttributeCurrentHeapFreeWithParams_subscriptionEstablished_reportHandler mtrBaseClusterSoftwareDiagnostics params subscriptionEstablished reportHandler =
  sendMessage mtrBaseClusterSoftwareDiagnostics subscribeAttributeCurrentHeapFreeWithParams_subscriptionEstablished_reportHandlerSelector (toMTRSubscribeParams params) subscriptionEstablished reportHandler

-- | @+ readAttributeCurrentHeapFreeWithClusterStateCache:endpoint:queue:completion:@
readAttributeCurrentHeapFreeWithClusterStateCache_endpoint_queue_completion :: (IsMTRClusterStateCacheContainer clusterStateCacheContainer, IsNSNumber endpoint, IsNSObject queue) => clusterStateCacheContainer -> endpoint -> queue -> Ptr () -> IO ()
readAttributeCurrentHeapFreeWithClusterStateCache_endpoint_queue_completion clusterStateCacheContainer endpoint queue completion =
  do
    cls' <- getRequiredClass "MTRBaseClusterSoftwareDiagnostics"
    sendClassMessage cls' readAttributeCurrentHeapFreeWithClusterStateCache_endpoint_queue_completionSelector (toMTRClusterStateCacheContainer clusterStateCacheContainer) (toNSNumber endpoint) (toNSObject queue) completion

-- | @- readAttributeCurrentHeapUsedWithCompletion:@
readAttributeCurrentHeapUsedWithCompletion :: IsMTRBaseClusterSoftwareDiagnostics mtrBaseClusterSoftwareDiagnostics => mtrBaseClusterSoftwareDiagnostics -> Ptr () -> IO ()
readAttributeCurrentHeapUsedWithCompletion mtrBaseClusterSoftwareDiagnostics completion =
  sendMessage mtrBaseClusterSoftwareDiagnostics readAttributeCurrentHeapUsedWithCompletionSelector completion

-- | @- subscribeAttributeCurrentHeapUsedWithParams:subscriptionEstablished:reportHandler:@
subscribeAttributeCurrentHeapUsedWithParams_subscriptionEstablished_reportHandler :: (IsMTRBaseClusterSoftwareDiagnostics mtrBaseClusterSoftwareDiagnostics, IsMTRSubscribeParams params) => mtrBaseClusterSoftwareDiagnostics -> params -> Ptr () -> Ptr () -> IO ()
subscribeAttributeCurrentHeapUsedWithParams_subscriptionEstablished_reportHandler mtrBaseClusterSoftwareDiagnostics params subscriptionEstablished reportHandler =
  sendMessage mtrBaseClusterSoftwareDiagnostics subscribeAttributeCurrentHeapUsedWithParams_subscriptionEstablished_reportHandlerSelector (toMTRSubscribeParams params) subscriptionEstablished reportHandler

-- | @+ readAttributeCurrentHeapUsedWithClusterStateCache:endpoint:queue:completion:@
readAttributeCurrentHeapUsedWithClusterStateCache_endpoint_queue_completion :: (IsMTRClusterStateCacheContainer clusterStateCacheContainer, IsNSNumber endpoint, IsNSObject queue) => clusterStateCacheContainer -> endpoint -> queue -> Ptr () -> IO ()
readAttributeCurrentHeapUsedWithClusterStateCache_endpoint_queue_completion clusterStateCacheContainer endpoint queue completion =
  do
    cls' <- getRequiredClass "MTRBaseClusterSoftwareDiagnostics"
    sendClassMessage cls' readAttributeCurrentHeapUsedWithClusterStateCache_endpoint_queue_completionSelector (toMTRClusterStateCacheContainer clusterStateCacheContainer) (toNSNumber endpoint) (toNSObject queue) completion

-- | @- readAttributeCurrentHeapHighWatermarkWithCompletion:@
readAttributeCurrentHeapHighWatermarkWithCompletion :: IsMTRBaseClusterSoftwareDiagnostics mtrBaseClusterSoftwareDiagnostics => mtrBaseClusterSoftwareDiagnostics -> Ptr () -> IO ()
readAttributeCurrentHeapHighWatermarkWithCompletion mtrBaseClusterSoftwareDiagnostics completion =
  sendMessage mtrBaseClusterSoftwareDiagnostics readAttributeCurrentHeapHighWatermarkWithCompletionSelector completion

-- | @- subscribeAttributeCurrentHeapHighWatermarkWithParams:subscriptionEstablished:reportHandler:@
subscribeAttributeCurrentHeapHighWatermarkWithParams_subscriptionEstablished_reportHandler :: (IsMTRBaseClusterSoftwareDiagnostics mtrBaseClusterSoftwareDiagnostics, IsMTRSubscribeParams params) => mtrBaseClusterSoftwareDiagnostics -> params -> Ptr () -> Ptr () -> IO ()
subscribeAttributeCurrentHeapHighWatermarkWithParams_subscriptionEstablished_reportHandler mtrBaseClusterSoftwareDiagnostics params subscriptionEstablished reportHandler =
  sendMessage mtrBaseClusterSoftwareDiagnostics subscribeAttributeCurrentHeapHighWatermarkWithParams_subscriptionEstablished_reportHandlerSelector (toMTRSubscribeParams params) subscriptionEstablished reportHandler

-- | @+ readAttributeCurrentHeapHighWatermarkWithClusterStateCache:endpoint:queue:completion:@
readAttributeCurrentHeapHighWatermarkWithClusterStateCache_endpoint_queue_completion :: (IsMTRClusterStateCacheContainer clusterStateCacheContainer, IsNSNumber endpoint, IsNSObject queue) => clusterStateCacheContainer -> endpoint -> queue -> Ptr () -> IO ()
readAttributeCurrentHeapHighWatermarkWithClusterStateCache_endpoint_queue_completion clusterStateCacheContainer endpoint queue completion =
  do
    cls' <- getRequiredClass "MTRBaseClusterSoftwareDiagnostics"
    sendClassMessage cls' readAttributeCurrentHeapHighWatermarkWithClusterStateCache_endpoint_queue_completionSelector (toMTRClusterStateCacheContainer clusterStateCacheContainer) (toNSNumber endpoint) (toNSObject queue) completion

-- | @- readAttributeGeneratedCommandListWithCompletion:@
readAttributeGeneratedCommandListWithCompletion :: IsMTRBaseClusterSoftwareDiagnostics mtrBaseClusterSoftwareDiagnostics => mtrBaseClusterSoftwareDiagnostics -> Ptr () -> IO ()
readAttributeGeneratedCommandListWithCompletion mtrBaseClusterSoftwareDiagnostics completion =
  sendMessage mtrBaseClusterSoftwareDiagnostics readAttributeGeneratedCommandListWithCompletionSelector completion

-- | @- subscribeAttributeGeneratedCommandListWithParams:subscriptionEstablished:reportHandler:@
subscribeAttributeGeneratedCommandListWithParams_subscriptionEstablished_reportHandler :: (IsMTRBaseClusterSoftwareDiagnostics mtrBaseClusterSoftwareDiagnostics, IsMTRSubscribeParams params) => mtrBaseClusterSoftwareDiagnostics -> params -> Ptr () -> Ptr () -> IO ()
subscribeAttributeGeneratedCommandListWithParams_subscriptionEstablished_reportHandler mtrBaseClusterSoftwareDiagnostics params subscriptionEstablished reportHandler =
  sendMessage mtrBaseClusterSoftwareDiagnostics subscribeAttributeGeneratedCommandListWithParams_subscriptionEstablished_reportHandlerSelector (toMTRSubscribeParams params) subscriptionEstablished reportHandler

-- | @+ readAttributeGeneratedCommandListWithClusterStateCache:endpoint:queue:completion:@
readAttributeGeneratedCommandListWithClusterStateCache_endpoint_queue_completion :: (IsMTRClusterStateCacheContainer clusterStateCacheContainer, IsNSNumber endpoint, IsNSObject queue) => clusterStateCacheContainer -> endpoint -> queue -> Ptr () -> IO ()
readAttributeGeneratedCommandListWithClusterStateCache_endpoint_queue_completion clusterStateCacheContainer endpoint queue completion =
  do
    cls' <- getRequiredClass "MTRBaseClusterSoftwareDiagnostics"
    sendClassMessage cls' readAttributeGeneratedCommandListWithClusterStateCache_endpoint_queue_completionSelector (toMTRClusterStateCacheContainer clusterStateCacheContainer) (toNSNumber endpoint) (toNSObject queue) completion

-- | @- readAttributeAcceptedCommandListWithCompletion:@
readAttributeAcceptedCommandListWithCompletion :: IsMTRBaseClusterSoftwareDiagnostics mtrBaseClusterSoftwareDiagnostics => mtrBaseClusterSoftwareDiagnostics -> Ptr () -> IO ()
readAttributeAcceptedCommandListWithCompletion mtrBaseClusterSoftwareDiagnostics completion =
  sendMessage mtrBaseClusterSoftwareDiagnostics readAttributeAcceptedCommandListWithCompletionSelector completion

-- | @- subscribeAttributeAcceptedCommandListWithParams:subscriptionEstablished:reportHandler:@
subscribeAttributeAcceptedCommandListWithParams_subscriptionEstablished_reportHandler :: (IsMTRBaseClusterSoftwareDiagnostics mtrBaseClusterSoftwareDiagnostics, IsMTRSubscribeParams params) => mtrBaseClusterSoftwareDiagnostics -> params -> Ptr () -> Ptr () -> IO ()
subscribeAttributeAcceptedCommandListWithParams_subscriptionEstablished_reportHandler mtrBaseClusterSoftwareDiagnostics params subscriptionEstablished reportHandler =
  sendMessage mtrBaseClusterSoftwareDiagnostics subscribeAttributeAcceptedCommandListWithParams_subscriptionEstablished_reportHandlerSelector (toMTRSubscribeParams params) subscriptionEstablished reportHandler

-- | @+ readAttributeAcceptedCommandListWithClusterStateCache:endpoint:queue:completion:@
readAttributeAcceptedCommandListWithClusterStateCache_endpoint_queue_completion :: (IsMTRClusterStateCacheContainer clusterStateCacheContainer, IsNSNumber endpoint, IsNSObject queue) => clusterStateCacheContainer -> endpoint -> queue -> Ptr () -> IO ()
readAttributeAcceptedCommandListWithClusterStateCache_endpoint_queue_completion clusterStateCacheContainer endpoint queue completion =
  do
    cls' <- getRequiredClass "MTRBaseClusterSoftwareDiagnostics"
    sendClassMessage cls' readAttributeAcceptedCommandListWithClusterStateCache_endpoint_queue_completionSelector (toMTRClusterStateCacheContainer clusterStateCacheContainer) (toNSNumber endpoint) (toNSObject queue) completion

-- | @- readAttributeAttributeListWithCompletion:@
readAttributeAttributeListWithCompletion :: IsMTRBaseClusterSoftwareDiagnostics mtrBaseClusterSoftwareDiagnostics => mtrBaseClusterSoftwareDiagnostics -> Ptr () -> IO ()
readAttributeAttributeListWithCompletion mtrBaseClusterSoftwareDiagnostics completion =
  sendMessage mtrBaseClusterSoftwareDiagnostics readAttributeAttributeListWithCompletionSelector completion

-- | @- subscribeAttributeAttributeListWithParams:subscriptionEstablished:reportHandler:@
subscribeAttributeAttributeListWithParams_subscriptionEstablished_reportHandler :: (IsMTRBaseClusterSoftwareDiagnostics mtrBaseClusterSoftwareDiagnostics, IsMTRSubscribeParams params) => mtrBaseClusterSoftwareDiagnostics -> params -> Ptr () -> Ptr () -> IO ()
subscribeAttributeAttributeListWithParams_subscriptionEstablished_reportHandler mtrBaseClusterSoftwareDiagnostics params subscriptionEstablished reportHandler =
  sendMessage mtrBaseClusterSoftwareDiagnostics subscribeAttributeAttributeListWithParams_subscriptionEstablished_reportHandlerSelector (toMTRSubscribeParams params) subscriptionEstablished reportHandler

-- | @+ readAttributeAttributeListWithClusterStateCache:endpoint:queue:completion:@
readAttributeAttributeListWithClusterStateCache_endpoint_queue_completion :: (IsMTRClusterStateCacheContainer clusterStateCacheContainer, IsNSNumber endpoint, IsNSObject queue) => clusterStateCacheContainer -> endpoint -> queue -> Ptr () -> IO ()
readAttributeAttributeListWithClusterStateCache_endpoint_queue_completion clusterStateCacheContainer endpoint queue completion =
  do
    cls' <- getRequiredClass "MTRBaseClusterSoftwareDiagnostics"
    sendClassMessage cls' readAttributeAttributeListWithClusterStateCache_endpoint_queue_completionSelector (toMTRClusterStateCacheContainer clusterStateCacheContainer) (toNSNumber endpoint) (toNSObject queue) completion

-- | @- readAttributeFeatureMapWithCompletion:@
readAttributeFeatureMapWithCompletion :: IsMTRBaseClusterSoftwareDiagnostics mtrBaseClusterSoftwareDiagnostics => mtrBaseClusterSoftwareDiagnostics -> Ptr () -> IO ()
readAttributeFeatureMapWithCompletion mtrBaseClusterSoftwareDiagnostics completion =
  sendMessage mtrBaseClusterSoftwareDiagnostics readAttributeFeatureMapWithCompletionSelector completion

-- | @- subscribeAttributeFeatureMapWithParams:subscriptionEstablished:reportHandler:@
subscribeAttributeFeatureMapWithParams_subscriptionEstablished_reportHandler :: (IsMTRBaseClusterSoftwareDiagnostics mtrBaseClusterSoftwareDiagnostics, IsMTRSubscribeParams params) => mtrBaseClusterSoftwareDiagnostics -> params -> Ptr () -> Ptr () -> IO ()
subscribeAttributeFeatureMapWithParams_subscriptionEstablished_reportHandler mtrBaseClusterSoftwareDiagnostics params subscriptionEstablished reportHandler =
  sendMessage mtrBaseClusterSoftwareDiagnostics subscribeAttributeFeatureMapWithParams_subscriptionEstablished_reportHandlerSelector (toMTRSubscribeParams params) subscriptionEstablished reportHandler

-- | @+ readAttributeFeatureMapWithClusterStateCache:endpoint:queue:completion:@
readAttributeFeatureMapWithClusterStateCache_endpoint_queue_completion :: (IsMTRClusterStateCacheContainer clusterStateCacheContainer, IsNSNumber endpoint, IsNSObject queue) => clusterStateCacheContainer -> endpoint -> queue -> Ptr () -> IO ()
readAttributeFeatureMapWithClusterStateCache_endpoint_queue_completion clusterStateCacheContainer endpoint queue completion =
  do
    cls' <- getRequiredClass "MTRBaseClusterSoftwareDiagnostics"
    sendClassMessage cls' readAttributeFeatureMapWithClusterStateCache_endpoint_queue_completionSelector (toMTRClusterStateCacheContainer clusterStateCacheContainer) (toNSNumber endpoint) (toNSObject queue) completion

-- | @- readAttributeClusterRevisionWithCompletion:@
readAttributeClusterRevisionWithCompletion :: IsMTRBaseClusterSoftwareDiagnostics mtrBaseClusterSoftwareDiagnostics => mtrBaseClusterSoftwareDiagnostics -> Ptr () -> IO ()
readAttributeClusterRevisionWithCompletion mtrBaseClusterSoftwareDiagnostics completion =
  sendMessage mtrBaseClusterSoftwareDiagnostics readAttributeClusterRevisionWithCompletionSelector completion

-- | @- subscribeAttributeClusterRevisionWithParams:subscriptionEstablished:reportHandler:@
subscribeAttributeClusterRevisionWithParams_subscriptionEstablished_reportHandler :: (IsMTRBaseClusterSoftwareDiagnostics mtrBaseClusterSoftwareDiagnostics, IsMTRSubscribeParams params) => mtrBaseClusterSoftwareDiagnostics -> params -> Ptr () -> Ptr () -> IO ()
subscribeAttributeClusterRevisionWithParams_subscriptionEstablished_reportHandler mtrBaseClusterSoftwareDiagnostics params subscriptionEstablished reportHandler =
  sendMessage mtrBaseClusterSoftwareDiagnostics subscribeAttributeClusterRevisionWithParams_subscriptionEstablished_reportHandlerSelector (toMTRSubscribeParams params) subscriptionEstablished reportHandler

-- | @+ readAttributeClusterRevisionWithClusterStateCache:endpoint:queue:completion:@
readAttributeClusterRevisionWithClusterStateCache_endpoint_queue_completion :: (IsMTRClusterStateCacheContainer clusterStateCacheContainer, IsNSNumber endpoint, IsNSObject queue) => clusterStateCacheContainer -> endpoint -> queue -> Ptr () -> IO ()
readAttributeClusterRevisionWithClusterStateCache_endpoint_queue_completion clusterStateCacheContainer endpoint queue completion =
  do
    cls' <- getRequiredClass "MTRBaseClusterSoftwareDiagnostics"
    sendClassMessage cls' readAttributeClusterRevisionWithClusterStateCache_endpoint_queue_completionSelector (toMTRClusterStateCacheContainer clusterStateCacheContainer) (toNSNumber endpoint) (toNSObject queue) completion

-- | @- init@
init_ :: IsMTRBaseClusterSoftwareDiagnostics mtrBaseClusterSoftwareDiagnostics => mtrBaseClusterSoftwareDiagnostics -> IO (Id MTRBaseClusterSoftwareDiagnostics)
init_ mtrBaseClusterSoftwareDiagnostics =
  sendOwnedMessage mtrBaseClusterSoftwareDiagnostics initSelector

-- | @+ new@
new :: IO (Id MTRBaseClusterSoftwareDiagnostics)
new  =
  do
    cls' <- getRequiredClass "MTRBaseClusterSoftwareDiagnostics"
    sendOwnedClassMessage cls' newSelector

-- | @- initWithDevice:endpoint:queue:@
initWithDevice_endpoint_queue :: (IsMTRBaseClusterSoftwareDiagnostics mtrBaseClusterSoftwareDiagnostics, IsMTRBaseDevice device, IsNSObject queue) => mtrBaseClusterSoftwareDiagnostics -> device -> CUShort -> queue -> IO (Id MTRBaseClusterSoftwareDiagnostics)
initWithDevice_endpoint_queue mtrBaseClusterSoftwareDiagnostics device endpoint queue =
  sendOwnedMessage mtrBaseClusterSoftwareDiagnostics initWithDevice_endpoint_queueSelector (toMTRBaseDevice device) endpoint (toNSObject queue)

-- | @- resetWatermarksWithParams:completionHandler:@
resetWatermarksWithParams_completionHandler :: (IsMTRBaseClusterSoftwareDiagnostics mtrBaseClusterSoftwareDiagnostics, IsMTRSoftwareDiagnosticsClusterResetWatermarksParams params) => mtrBaseClusterSoftwareDiagnostics -> params -> Ptr () -> IO ()
resetWatermarksWithParams_completionHandler mtrBaseClusterSoftwareDiagnostics params completionHandler =
  sendMessage mtrBaseClusterSoftwareDiagnostics resetWatermarksWithParams_completionHandlerSelector (toMTRSoftwareDiagnosticsClusterResetWatermarksParams params) completionHandler

-- | @- resetWatermarksWithCompletionHandler:@
resetWatermarksWithCompletionHandler :: IsMTRBaseClusterSoftwareDiagnostics mtrBaseClusterSoftwareDiagnostics => mtrBaseClusterSoftwareDiagnostics -> Ptr () -> IO ()
resetWatermarksWithCompletionHandler mtrBaseClusterSoftwareDiagnostics completionHandler =
  sendMessage mtrBaseClusterSoftwareDiagnostics resetWatermarksWithCompletionHandlerSelector completionHandler

-- | @- readAttributeThreadMetricsWithCompletionHandler:@
readAttributeThreadMetricsWithCompletionHandler :: IsMTRBaseClusterSoftwareDiagnostics mtrBaseClusterSoftwareDiagnostics => mtrBaseClusterSoftwareDiagnostics -> Ptr () -> IO ()
readAttributeThreadMetricsWithCompletionHandler mtrBaseClusterSoftwareDiagnostics completionHandler =
  sendMessage mtrBaseClusterSoftwareDiagnostics readAttributeThreadMetricsWithCompletionHandlerSelector completionHandler

-- | @- subscribeAttributeThreadMetricsWithMinInterval:maxInterval:params:subscriptionEstablished:reportHandler:@
subscribeAttributeThreadMetricsWithMinInterval_maxInterval_params_subscriptionEstablished_reportHandler :: (IsMTRBaseClusterSoftwareDiagnostics mtrBaseClusterSoftwareDiagnostics, IsNSNumber minInterval, IsNSNumber maxInterval, IsMTRSubscribeParams params) => mtrBaseClusterSoftwareDiagnostics -> minInterval -> maxInterval -> params -> Ptr () -> Ptr () -> IO ()
subscribeAttributeThreadMetricsWithMinInterval_maxInterval_params_subscriptionEstablished_reportHandler mtrBaseClusterSoftwareDiagnostics minInterval maxInterval params subscriptionEstablishedHandler reportHandler =
  sendMessage mtrBaseClusterSoftwareDiagnostics subscribeAttributeThreadMetricsWithMinInterval_maxInterval_params_subscriptionEstablished_reportHandlerSelector (toNSNumber minInterval) (toNSNumber maxInterval) (toMTRSubscribeParams params) subscriptionEstablishedHandler reportHandler

-- | @+ readAttributeThreadMetricsWithAttributeCache:endpoint:queue:completionHandler:@
readAttributeThreadMetricsWithAttributeCache_endpoint_queue_completionHandler :: (IsMTRAttributeCacheContainer attributeCacheContainer, IsNSNumber endpoint, IsNSObject queue) => attributeCacheContainer -> endpoint -> queue -> Ptr () -> IO ()
readAttributeThreadMetricsWithAttributeCache_endpoint_queue_completionHandler attributeCacheContainer endpoint queue completionHandler =
  do
    cls' <- getRequiredClass "MTRBaseClusterSoftwareDiagnostics"
    sendClassMessage cls' readAttributeThreadMetricsWithAttributeCache_endpoint_queue_completionHandlerSelector (toMTRAttributeCacheContainer attributeCacheContainer) (toNSNumber endpoint) (toNSObject queue) completionHandler

-- | @- readAttributeCurrentHeapFreeWithCompletionHandler:@
readAttributeCurrentHeapFreeWithCompletionHandler :: IsMTRBaseClusterSoftwareDiagnostics mtrBaseClusterSoftwareDiagnostics => mtrBaseClusterSoftwareDiagnostics -> Ptr () -> IO ()
readAttributeCurrentHeapFreeWithCompletionHandler mtrBaseClusterSoftwareDiagnostics completionHandler =
  sendMessage mtrBaseClusterSoftwareDiagnostics readAttributeCurrentHeapFreeWithCompletionHandlerSelector completionHandler

-- | @- subscribeAttributeCurrentHeapFreeWithMinInterval:maxInterval:params:subscriptionEstablished:reportHandler:@
subscribeAttributeCurrentHeapFreeWithMinInterval_maxInterval_params_subscriptionEstablished_reportHandler :: (IsMTRBaseClusterSoftwareDiagnostics mtrBaseClusterSoftwareDiagnostics, IsNSNumber minInterval, IsNSNumber maxInterval, IsMTRSubscribeParams params) => mtrBaseClusterSoftwareDiagnostics -> minInterval -> maxInterval -> params -> Ptr () -> Ptr () -> IO ()
subscribeAttributeCurrentHeapFreeWithMinInterval_maxInterval_params_subscriptionEstablished_reportHandler mtrBaseClusterSoftwareDiagnostics minInterval maxInterval params subscriptionEstablishedHandler reportHandler =
  sendMessage mtrBaseClusterSoftwareDiagnostics subscribeAttributeCurrentHeapFreeWithMinInterval_maxInterval_params_subscriptionEstablished_reportHandlerSelector (toNSNumber minInterval) (toNSNumber maxInterval) (toMTRSubscribeParams params) subscriptionEstablishedHandler reportHandler

-- | @+ readAttributeCurrentHeapFreeWithAttributeCache:endpoint:queue:completionHandler:@
readAttributeCurrentHeapFreeWithAttributeCache_endpoint_queue_completionHandler :: (IsMTRAttributeCacheContainer attributeCacheContainer, IsNSNumber endpoint, IsNSObject queue) => attributeCacheContainer -> endpoint -> queue -> Ptr () -> IO ()
readAttributeCurrentHeapFreeWithAttributeCache_endpoint_queue_completionHandler attributeCacheContainer endpoint queue completionHandler =
  do
    cls' <- getRequiredClass "MTRBaseClusterSoftwareDiagnostics"
    sendClassMessage cls' readAttributeCurrentHeapFreeWithAttributeCache_endpoint_queue_completionHandlerSelector (toMTRAttributeCacheContainer attributeCacheContainer) (toNSNumber endpoint) (toNSObject queue) completionHandler

-- | @- readAttributeCurrentHeapUsedWithCompletionHandler:@
readAttributeCurrentHeapUsedWithCompletionHandler :: IsMTRBaseClusterSoftwareDiagnostics mtrBaseClusterSoftwareDiagnostics => mtrBaseClusterSoftwareDiagnostics -> Ptr () -> IO ()
readAttributeCurrentHeapUsedWithCompletionHandler mtrBaseClusterSoftwareDiagnostics completionHandler =
  sendMessage mtrBaseClusterSoftwareDiagnostics readAttributeCurrentHeapUsedWithCompletionHandlerSelector completionHandler

-- | @- subscribeAttributeCurrentHeapUsedWithMinInterval:maxInterval:params:subscriptionEstablished:reportHandler:@
subscribeAttributeCurrentHeapUsedWithMinInterval_maxInterval_params_subscriptionEstablished_reportHandler :: (IsMTRBaseClusterSoftwareDiagnostics mtrBaseClusterSoftwareDiagnostics, IsNSNumber minInterval, IsNSNumber maxInterval, IsMTRSubscribeParams params) => mtrBaseClusterSoftwareDiagnostics -> minInterval -> maxInterval -> params -> Ptr () -> Ptr () -> IO ()
subscribeAttributeCurrentHeapUsedWithMinInterval_maxInterval_params_subscriptionEstablished_reportHandler mtrBaseClusterSoftwareDiagnostics minInterval maxInterval params subscriptionEstablishedHandler reportHandler =
  sendMessage mtrBaseClusterSoftwareDiagnostics subscribeAttributeCurrentHeapUsedWithMinInterval_maxInterval_params_subscriptionEstablished_reportHandlerSelector (toNSNumber minInterval) (toNSNumber maxInterval) (toMTRSubscribeParams params) subscriptionEstablishedHandler reportHandler

-- | @+ readAttributeCurrentHeapUsedWithAttributeCache:endpoint:queue:completionHandler:@
readAttributeCurrentHeapUsedWithAttributeCache_endpoint_queue_completionHandler :: (IsMTRAttributeCacheContainer attributeCacheContainer, IsNSNumber endpoint, IsNSObject queue) => attributeCacheContainer -> endpoint -> queue -> Ptr () -> IO ()
readAttributeCurrentHeapUsedWithAttributeCache_endpoint_queue_completionHandler attributeCacheContainer endpoint queue completionHandler =
  do
    cls' <- getRequiredClass "MTRBaseClusterSoftwareDiagnostics"
    sendClassMessage cls' readAttributeCurrentHeapUsedWithAttributeCache_endpoint_queue_completionHandlerSelector (toMTRAttributeCacheContainer attributeCacheContainer) (toNSNumber endpoint) (toNSObject queue) completionHandler

-- | @- readAttributeCurrentHeapHighWatermarkWithCompletionHandler:@
readAttributeCurrentHeapHighWatermarkWithCompletionHandler :: IsMTRBaseClusterSoftwareDiagnostics mtrBaseClusterSoftwareDiagnostics => mtrBaseClusterSoftwareDiagnostics -> Ptr () -> IO ()
readAttributeCurrentHeapHighWatermarkWithCompletionHandler mtrBaseClusterSoftwareDiagnostics completionHandler =
  sendMessage mtrBaseClusterSoftwareDiagnostics readAttributeCurrentHeapHighWatermarkWithCompletionHandlerSelector completionHandler

-- | @- subscribeAttributeCurrentHeapHighWatermarkWithMinInterval:maxInterval:params:subscriptionEstablished:reportHandler:@
subscribeAttributeCurrentHeapHighWatermarkWithMinInterval_maxInterval_params_subscriptionEstablished_reportHandler :: (IsMTRBaseClusterSoftwareDiagnostics mtrBaseClusterSoftwareDiagnostics, IsNSNumber minInterval, IsNSNumber maxInterval, IsMTRSubscribeParams params) => mtrBaseClusterSoftwareDiagnostics -> minInterval -> maxInterval -> params -> Ptr () -> Ptr () -> IO ()
subscribeAttributeCurrentHeapHighWatermarkWithMinInterval_maxInterval_params_subscriptionEstablished_reportHandler mtrBaseClusterSoftwareDiagnostics minInterval maxInterval params subscriptionEstablishedHandler reportHandler =
  sendMessage mtrBaseClusterSoftwareDiagnostics subscribeAttributeCurrentHeapHighWatermarkWithMinInterval_maxInterval_params_subscriptionEstablished_reportHandlerSelector (toNSNumber minInterval) (toNSNumber maxInterval) (toMTRSubscribeParams params) subscriptionEstablishedHandler reportHandler

-- | @+ readAttributeCurrentHeapHighWatermarkWithAttributeCache:endpoint:queue:completionHandler:@
readAttributeCurrentHeapHighWatermarkWithAttributeCache_endpoint_queue_completionHandler :: (IsMTRAttributeCacheContainer attributeCacheContainer, IsNSNumber endpoint, IsNSObject queue) => attributeCacheContainer -> endpoint -> queue -> Ptr () -> IO ()
readAttributeCurrentHeapHighWatermarkWithAttributeCache_endpoint_queue_completionHandler attributeCacheContainer endpoint queue completionHandler =
  do
    cls' <- getRequiredClass "MTRBaseClusterSoftwareDiagnostics"
    sendClassMessage cls' readAttributeCurrentHeapHighWatermarkWithAttributeCache_endpoint_queue_completionHandlerSelector (toMTRAttributeCacheContainer attributeCacheContainer) (toNSNumber endpoint) (toNSObject queue) completionHandler

-- | @- readAttributeGeneratedCommandListWithCompletionHandler:@
readAttributeGeneratedCommandListWithCompletionHandler :: IsMTRBaseClusterSoftwareDiagnostics mtrBaseClusterSoftwareDiagnostics => mtrBaseClusterSoftwareDiagnostics -> Ptr () -> IO ()
readAttributeGeneratedCommandListWithCompletionHandler mtrBaseClusterSoftwareDiagnostics completionHandler =
  sendMessage mtrBaseClusterSoftwareDiagnostics readAttributeGeneratedCommandListWithCompletionHandlerSelector completionHandler

-- | @- subscribeAttributeGeneratedCommandListWithMinInterval:maxInterval:params:subscriptionEstablished:reportHandler:@
subscribeAttributeGeneratedCommandListWithMinInterval_maxInterval_params_subscriptionEstablished_reportHandler :: (IsMTRBaseClusterSoftwareDiagnostics mtrBaseClusterSoftwareDiagnostics, IsNSNumber minInterval, IsNSNumber maxInterval, IsMTRSubscribeParams params) => mtrBaseClusterSoftwareDiagnostics -> minInterval -> maxInterval -> params -> Ptr () -> Ptr () -> IO ()
subscribeAttributeGeneratedCommandListWithMinInterval_maxInterval_params_subscriptionEstablished_reportHandler mtrBaseClusterSoftwareDiagnostics minInterval maxInterval params subscriptionEstablishedHandler reportHandler =
  sendMessage mtrBaseClusterSoftwareDiagnostics subscribeAttributeGeneratedCommandListWithMinInterval_maxInterval_params_subscriptionEstablished_reportHandlerSelector (toNSNumber minInterval) (toNSNumber maxInterval) (toMTRSubscribeParams params) subscriptionEstablishedHandler reportHandler

-- | @+ readAttributeGeneratedCommandListWithAttributeCache:endpoint:queue:completionHandler:@
readAttributeGeneratedCommandListWithAttributeCache_endpoint_queue_completionHandler :: (IsMTRAttributeCacheContainer attributeCacheContainer, IsNSNumber endpoint, IsNSObject queue) => attributeCacheContainer -> endpoint -> queue -> Ptr () -> IO ()
readAttributeGeneratedCommandListWithAttributeCache_endpoint_queue_completionHandler attributeCacheContainer endpoint queue completionHandler =
  do
    cls' <- getRequiredClass "MTRBaseClusterSoftwareDiagnostics"
    sendClassMessage cls' readAttributeGeneratedCommandListWithAttributeCache_endpoint_queue_completionHandlerSelector (toMTRAttributeCacheContainer attributeCacheContainer) (toNSNumber endpoint) (toNSObject queue) completionHandler

-- | @- readAttributeAcceptedCommandListWithCompletionHandler:@
readAttributeAcceptedCommandListWithCompletionHandler :: IsMTRBaseClusterSoftwareDiagnostics mtrBaseClusterSoftwareDiagnostics => mtrBaseClusterSoftwareDiagnostics -> Ptr () -> IO ()
readAttributeAcceptedCommandListWithCompletionHandler mtrBaseClusterSoftwareDiagnostics completionHandler =
  sendMessage mtrBaseClusterSoftwareDiagnostics readAttributeAcceptedCommandListWithCompletionHandlerSelector completionHandler

-- | @- subscribeAttributeAcceptedCommandListWithMinInterval:maxInterval:params:subscriptionEstablished:reportHandler:@
subscribeAttributeAcceptedCommandListWithMinInterval_maxInterval_params_subscriptionEstablished_reportHandler :: (IsMTRBaseClusterSoftwareDiagnostics mtrBaseClusterSoftwareDiagnostics, IsNSNumber minInterval, IsNSNumber maxInterval, IsMTRSubscribeParams params) => mtrBaseClusterSoftwareDiagnostics -> minInterval -> maxInterval -> params -> Ptr () -> Ptr () -> IO ()
subscribeAttributeAcceptedCommandListWithMinInterval_maxInterval_params_subscriptionEstablished_reportHandler mtrBaseClusterSoftwareDiagnostics minInterval maxInterval params subscriptionEstablishedHandler reportHandler =
  sendMessage mtrBaseClusterSoftwareDiagnostics subscribeAttributeAcceptedCommandListWithMinInterval_maxInterval_params_subscriptionEstablished_reportHandlerSelector (toNSNumber minInterval) (toNSNumber maxInterval) (toMTRSubscribeParams params) subscriptionEstablishedHandler reportHandler

-- | @+ readAttributeAcceptedCommandListWithAttributeCache:endpoint:queue:completionHandler:@
readAttributeAcceptedCommandListWithAttributeCache_endpoint_queue_completionHandler :: (IsMTRAttributeCacheContainer attributeCacheContainer, IsNSNumber endpoint, IsNSObject queue) => attributeCacheContainer -> endpoint -> queue -> Ptr () -> IO ()
readAttributeAcceptedCommandListWithAttributeCache_endpoint_queue_completionHandler attributeCacheContainer endpoint queue completionHandler =
  do
    cls' <- getRequiredClass "MTRBaseClusterSoftwareDiagnostics"
    sendClassMessage cls' readAttributeAcceptedCommandListWithAttributeCache_endpoint_queue_completionHandlerSelector (toMTRAttributeCacheContainer attributeCacheContainer) (toNSNumber endpoint) (toNSObject queue) completionHandler

-- | @- readAttributeAttributeListWithCompletionHandler:@
readAttributeAttributeListWithCompletionHandler :: IsMTRBaseClusterSoftwareDiagnostics mtrBaseClusterSoftwareDiagnostics => mtrBaseClusterSoftwareDiagnostics -> Ptr () -> IO ()
readAttributeAttributeListWithCompletionHandler mtrBaseClusterSoftwareDiagnostics completionHandler =
  sendMessage mtrBaseClusterSoftwareDiagnostics readAttributeAttributeListWithCompletionHandlerSelector completionHandler

-- | @- subscribeAttributeAttributeListWithMinInterval:maxInterval:params:subscriptionEstablished:reportHandler:@
subscribeAttributeAttributeListWithMinInterval_maxInterval_params_subscriptionEstablished_reportHandler :: (IsMTRBaseClusterSoftwareDiagnostics mtrBaseClusterSoftwareDiagnostics, IsNSNumber minInterval, IsNSNumber maxInterval, IsMTRSubscribeParams params) => mtrBaseClusterSoftwareDiagnostics -> minInterval -> maxInterval -> params -> Ptr () -> Ptr () -> IO ()
subscribeAttributeAttributeListWithMinInterval_maxInterval_params_subscriptionEstablished_reportHandler mtrBaseClusterSoftwareDiagnostics minInterval maxInterval params subscriptionEstablishedHandler reportHandler =
  sendMessage mtrBaseClusterSoftwareDiagnostics subscribeAttributeAttributeListWithMinInterval_maxInterval_params_subscriptionEstablished_reportHandlerSelector (toNSNumber minInterval) (toNSNumber maxInterval) (toMTRSubscribeParams params) subscriptionEstablishedHandler reportHandler

-- | @+ readAttributeAttributeListWithAttributeCache:endpoint:queue:completionHandler:@
readAttributeAttributeListWithAttributeCache_endpoint_queue_completionHandler :: (IsMTRAttributeCacheContainer attributeCacheContainer, IsNSNumber endpoint, IsNSObject queue) => attributeCacheContainer -> endpoint -> queue -> Ptr () -> IO ()
readAttributeAttributeListWithAttributeCache_endpoint_queue_completionHandler attributeCacheContainer endpoint queue completionHandler =
  do
    cls' <- getRequiredClass "MTRBaseClusterSoftwareDiagnostics"
    sendClassMessage cls' readAttributeAttributeListWithAttributeCache_endpoint_queue_completionHandlerSelector (toMTRAttributeCacheContainer attributeCacheContainer) (toNSNumber endpoint) (toNSObject queue) completionHandler

-- | @- readAttributeFeatureMapWithCompletionHandler:@
readAttributeFeatureMapWithCompletionHandler :: IsMTRBaseClusterSoftwareDiagnostics mtrBaseClusterSoftwareDiagnostics => mtrBaseClusterSoftwareDiagnostics -> Ptr () -> IO ()
readAttributeFeatureMapWithCompletionHandler mtrBaseClusterSoftwareDiagnostics completionHandler =
  sendMessage mtrBaseClusterSoftwareDiagnostics readAttributeFeatureMapWithCompletionHandlerSelector completionHandler

-- | @- subscribeAttributeFeatureMapWithMinInterval:maxInterval:params:subscriptionEstablished:reportHandler:@
subscribeAttributeFeatureMapWithMinInterval_maxInterval_params_subscriptionEstablished_reportHandler :: (IsMTRBaseClusterSoftwareDiagnostics mtrBaseClusterSoftwareDiagnostics, IsNSNumber minInterval, IsNSNumber maxInterval, IsMTRSubscribeParams params) => mtrBaseClusterSoftwareDiagnostics -> minInterval -> maxInterval -> params -> Ptr () -> Ptr () -> IO ()
subscribeAttributeFeatureMapWithMinInterval_maxInterval_params_subscriptionEstablished_reportHandler mtrBaseClusterSoftwareDiagnostics minInterval maxInterval params subscriptionEstablishedHandler reportHandler =
  sendMessage mtrBaseClusterSoftwareDiagnostics subscribeAttributeFeatureMapWithMinInterval_maxInterval_params_subscriptionEstablished_reportHandlerSelector (toNSNumber minInterval) (toNSNumber maxInterval) (toMTRSubscribeParams params) subscriptionEstablishedHandler reportHandler

-- | @+ readAttributeFeatureMapWithAttributeCache:endpoint:queue:completionHandler:@
readAttributeFeatureMapWithAttributeCache_endpoint_queue_completionHandler :: (IsMTRAttributeCacheContainer attributeCacheContainer, IsNSNumber endpoint, IsNSObject queue) => attributeCacheContainer -> endpoint -> queue -> Ptr () -> IO ()
readAttributeFeatureMapWithAttributeCache_endpoint_queue_completionHandler attributeCacheContainer endpoint queue completionHandler =
  do
    cls' <- getRequiredClass "MTRBaseClusterSoftwareDiagnostics"
    sendClassMessage cls' readAttributeFeatureMapWithAttributeCache_endpoint_queue_completionHandlerSelector (toMTRAttributeCacheContainer attributeCacheContainer) (toNSNumber endpoint) (toNSObject queue) completionHandler

-- | @- readAttributeClusterRevisionWithCompletionHandler:@
readAttributeClusterRevisionWithCompletionHandler :: IsMTRBaseClusterSoftwareDiagnostics mtrBaseClusterSoftwareDiagnostics => mtrBaseClusterSoftwareDiagnostics -> Ptr () -> IO ()
readAttributeClusterRevisionWithCompletionHandler mtrBaseClusterSoftwareDiagnostics completionHandler =
  sendMessage mtrBaseClusterSoftwareDiagnostics readAttributeClusterRevisionWithCompletionHandlerSelector completionHandler

-- | @- subscribeAttributeClusterRevisionWithMinInterval:maxInterval:params:subscriptionEstablished:reportHandler:@
subscribeAttributeClusterRevisionWithMinInterval_maxInterval_params_subscriptionEstablished_reportHandler :: (IsMTRBaseClusterSoftwareDiagnostics mtrBaseClusterSoftwareDiagnostics, IsNSNumber minInterval, IsNSNumber maxInterval, IsMTRSubscribeParams params) => mtrBaseClusterSoftwareDiagnostics -> minInterval -> maxInterval -> params -> Ptr () -> Ptr () -> IO ()
subscribeAttributeClusterRevisionWithMinInterval_maxInterval_params_subscriptionEstablished_reportHandler mtrBaseClusterSoftwareDiagnostics minInterval maxInterval params subscriptionEstablishedHandler reportHandler =
  sendMessage mtrBaseClusterSoftwareDiagnostics subscribeAttributeClusterRevisionWithMinInterval_maxInterval_params_subscriptionEstablished_reportHandlerSelector (toNSNumber minInterval) (toNSNumber maxInterval) (toMTRSubscribeParams params) subscriptionEstablishedHandler reportHandler

-- | @+ readAttributeClusterRevisionWithAttributeCache:endpoint:queue:completionHandler:@
readAttributeClusterRevisionWithAttributeCache_endpoint_queue_completionHandler :: (IsMTRAttributeCacheContainer attributeCacheContainer, IsNSNumber endpoint, IsNSObject queue) => attributeCacheContainer -> endpoint -> queue -> Ptr () -> IO ()
readAttributeClusterRevisionWithAttributeCache_endpoint_queue_completionHandler attributeCacheContainer endpoint queue completionHandler =
  do
    cls' <- getRequiredClass "MTRBaseClusterSoftwareDiagnostics"
    sendClassMessage cls' readAttributeClusterRevisionWithAttributeCache_endpoint_queue_completionHandlerSelector (toMTRAttributeCacheContainer attributeCacheContainer) (toNSNumber endpoint) (toNSObject queue) completionHandler

-- | For all instance methods (reads, writes, commands) that take a completion, the completion will be called on the provided queue.
--
-- ObjC selector: @- initWithDevice:endpointID:queue:@
initWithDevice_endpointID_queue :: (IsMTRBaseClusterSoftwareDiagnostics mtrBaseClusterSoftwareDiagnostics, IsMTRBaseDevice device, IsNSNumber endpointID, IsNSObject queue) => mtrBaseClusterSoftwareDiagnostics -> device -> endpointID -> queue -> IO (Id MTRBaseClusterSoftwareDiagnostics)
initWithDevice_endpointID_queue mtrBaseClusterSoftwareDiagnostics device endpointID queue =
  sendOwnedMessage mtrBaseClusterSoftwareDiagnostics initWithDevice_endpointID_queueSelector (toMTRBaseDevice device) (toNSNumber endpointID) (toNSObject queue)

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @resetWatermarksWithParams:completion:@
resetWatermarksWithParams_completionSelector :: Selector '[Id MTRSoftwareDiagnosticsClusterResetWatermarksParams, Ptr ()] ()
resetWatermarksWithParams_completionSelector = mkSelector "resetWatermarksWithParams:completion:"

-- | @Selector@ for @resetWatermarksWithCompletion:@
resetWatermarksWithCompletionSelector :: Selector '[Ptr ()] ()
resetWatermarksWithCompletionSelector = mkSelector "resetWatermarksWithCompletion:"

-- | @Selector@ for @readAttributeThreadMetricsWithCompletion:@
readAttributeThreadMetricsWithCompletionSelector :: Selector '[Ptr ()] ()
readAttributeThreadMetricsWithCompletionSelector = mkSelector "readAttributeThreadMetricsWithCompletion:"

-- | @Selector@ for @subscribeAttributeThreadMetricsWithParams:subscriptionEstablished:reportHandler:@
subscribeAttributeThreadMetricsWithParams_subscriptionEstablished_reportHandlerSelector :: Selector '[Id MTRSubscribeParams, Ptr (), Ptr ()] ()
subscribeAttributeThreadMetricsWithParams_subscriptionEstablished_reportHandlerSelector = mkSelector "subscribeAttributeThreadMetricsWithParams:subscriptionEstablished:reportHandler:"

-- | @Selector@ for @readAttributeThreadMetricsWithClusterStateCache:endpoint:queue:completion:@
readAttributeThreadMetricsWithClusterStateCache_endpoint_queue_completionSelector :: Selector '[Id MTRClusterStateCacheContainer, Id NSNumber, Id NSObject, Ptr ()] ()
readAttributeThreadMetricsWithClusterStateCache_endpoint_queue_completionSelector = mkSelector "readAttributeThreadMetricsWithClusterStateCache:endpoint:queue:completion:"

-- | @Selector@ for @readAttributeCurrentHeapFreeWithCompletion:@
readAttributeCurrentHeapFreeWithCompletionSelector :: Selector '[Ptr ()] ()
readAttributeCurrentHeapFreeWithCompletionSelector = mkSelector "readAttributeCurrentHeapFreeWithCompletion:"

-- | @Selector@ for @subscribeAttributeCurrentHeapFreeWithParams:subscriptionEstablished:reportHandler:@
subscribeAttributeCurrentHeapFreeWithParams_subscriptionEstablished_reportHandlerSelector :: Selector '[Id MTRSubscribeParams, Ptr (), Ptr ()] ()
subscribeAttributeCurrentHeapFreeWithParams_subscriptionEstablished_reportHandlerSelector = mkSelector "subscribeAttributeCurrentHeapFreeWithParams:subscriptionEstablished:reportHandler:"

-- | @Selector@ for @readAttributeCurrentHeapFreeWithClusterStateCache:endpoint:queue:completion:@
readAttributeCurrentHeapFreeWithClusterStateCache_endpoint_queue_completionSelector :: Selector '[Id MTRClusterStateCacheContainer, Id NSNumber, Id NSObject, Ptr ()] ()
readAttributeCurrentHeapFreeWithClusterStateCache_endpoint_queue_completionSelector = mkSelector "readAttributeCurrentHeapFreeWithClusterStateCache:endpoint:queue:completion:"

-- | @Selector@ for @readAttributeCurrentHeapUsedWithCompletion:@
readAttributeCurrentHeapUsedWithCompletionSelector :: Selector '[Ptr ()] ()
readAttributeCurrentHeapUsedWithCompletionSelector = mkSelector "readAttributeCurrentHeapUsedWithCompletion:"

-- | @Selector@ for @subscribeAttributeCurrentHeapUsedWithParams:subscriptionEstablished:reportHandler:@
subscribeAttributeCurrentHeapUsedWithParams_subscriptionEstablished_reportHandlerSelector :: Selector '[Id MTRSubscribeParams, Ptr (), Ptr ()] ()
subscribeAttributeCurrentHeapUsedWithParams_subscriptionEstablished_reportHandlerSelector = mkSelector "subscribeAttributeCurrentHeapUsedWithParams:subscriptionEstablished:reportHandler:"

-- | @Selector@ for @readAttributeCurrentHeapUsedWithClusterStateCache:endpoint:queue:completion:@
readAttributeCurrentHeapUsedWithClusterStateCache_endpoint_queue_completionSelector :: Selector '[Id MTRClusterStateCacheContainer, Id NSNumber, Id NSObject, Ptr ()] ()
readAttributeCurrentHeapUsedWithClusterStateCache_endpoint_queue_completionSelector = mkSelector "readAttributeCurrentHeapUsedWithClusterStateCache:endpoint:queue:completion:"

-- | @Selector@ for @readAttributeCurrentHeapHighWatermarkWithCompletion:@
readAttributeCurrentHeapHighWatermarkWithCompletionSelector :: Selector '[Ptr ()] ()
readAttributeCurrentHeapHighWatermarkWithCompletionSelector = mkSelector "readAttributeCurrentHeapHighWatermarkWithCompletion:"

-- | @Selector@ for @subscribeAttributeCurrentHeapHighWatermarkWithParams:subscriptionEstablished:reportHandler:@
subscribeAttributeCurrentHeapHighWatermarkWithParams_subscriptionEstablished_reportHandlerSelector :: Selector '[Id MTRSubscribeParams, Ptr (), Ptr ()] ()
subscribeAttributeCurrentHeapHighWatermarkWithParams_subscriptionEstablished_reportHandlerSelector = mkSelector "subscribeAttributeCurrentHeapHighWatermarkWithParams:subscriptionEstablished:reportHandler:"

-- | @Selector@ for @readAttributeCurrentHeapHighWatermarkWithClusterStateCache:endpoint:queue:completion:@
readAttributeCurrentHeapHighWatermarkWithClusterStateCache_endpoint_queue_completionSelector :: Selector '[Id MTRClusterStateCacheContainer, Id NSNumber, Id NSObject, Ptr ()] ()
readAttributeCurrentHeapHighWatermarkWithClusterStateCache_endpoint_queue_completionSelector = mkSelector "readAttributeCurrentHeapHighWatermarkWithClusterStateCache:endpoint:queue:completion:"

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
initSelector :: Selector '[] (Id MTRBaseClusterSoftwareDiagnostics)
initSelector = mkSelector "init"

-- | @Selector@ for @new@
newSelector :: Selector '[] (Id MTRBaseClusterSoftwareDiagnostics)
newSelector = mkSelector "new"

-- | @Selector@ for @initWithDevice:endpoint:queue:@
initWithDevice_endpoint_queueSelector :: Selector '[Id MTRBaseDevice, CUShort, Id NSObject] (Id MTRBaseClusterSoftwareDiagnostics)
initWithDevice_endpoint_queueSelector = mkSelector "initWithDevice:endpoint:queue:"

-- | @Selector@ for @resetWatermarksWithParams:completionHandler:@
resetWatermarksWithParams_completionHandlerSelector :: Selector '[Id MTRSoftwareDiagnosticsClusterResetWatermarksParams, Ptr ()] ()
resetWatermarksWithParams_completionHandlerSelector = mkSelector "resetWatermarksWithParams:completionHandler:"

-- | @Selector@ for @resetWatermarksWithCompletionHandler:@
resetWatermarksWithCompletionHandlerSelector :: Selector '[Ptr ()] ()
resetWatermarksWithCompletionHandlerSelector = mkSelector "resetWatermarksWithCompletionHandler:"

-- | @Selector@ for @readAttributeThreadMetricsWithCompletionHandler:@
readAttributeThreadMetricsWithCompletionHandlerSelector :: Selector '[Ptr ()] ()
readAttributeThreadMetricsWithCompletionHandlerSelector = mkSelector "readAttributeThreadMetricsWithCompletionHandler:"

-- | @Selector@ for @subscribeAttributeThreadMetricsWithMinInterval:maxInterval:params:subscriptionEstablished:reportHandler:@
subscribeAttributeThreadMetricsWithMinInterval_maxInterval_params_subscriptionEstablished_reportHandlerSelector :: Selector '[Id NSNumber, Id NSNumber, Id MTRSubscribeParams, Ptr (), Ptr ()] ()
subscribeAttributeThreadMetricsWithMinInterval_maxInterval_params_subscriptionEstablished_reportHandlerSelector = mkSelector "subscribeAttributeThreadMetricsWithMinInterval:maxInterval:params:subscriptionEstablished:reportHandler:"

-- | @Selector@ for @readAttributeThreadMetricsWithAttributeCache:endpoint:queue:completionHandler:@
readAttributeThreadMetricsWithAttributeCache_endpoint_queue_completionHandlerSelector :: Selector '[Id MTRAttributeCacheContainer, Id NSNumber, Id NSObject, Ptr ()] ()
readAttributeThreadMetricsWithAttributeCache_endpoint_queue_completionHandlerSelector = mkSelector "readAttributeThreadMetricsWithAttributeCache:endpoint:queue:completionHandler:"

-- | @Selector@ for @readAttributeCurrentHeapFreeWithCompletionHandler:@
readAttributeCurrentHeapFreeWithCompletionHandlerSelector :: Selector '[Ptr ()] ()
readAttributeCurrentHeapFreeWithCompletionHandlerSelector = mkSelector "readAttributeCurrentHeapFreeWithCompletionHandler:"

-- | @Selector@ for @subscribeAttributeCurrentHeapFreeWithMinInterval:maxInterval:params:subscriptionEstablished:reportHandler:@
subscribeAttributeCurrentHeapFreeWithMinInterval_maxInterval_params_subscriptionEstablished_reportHandlerSelector :: Selector '[Id NSNumber, Id NSNumber, Id MTRSubscribeParams, Ptr (), Ptr ()] ()
subscribeAttributeCurrentHeapFreeWithMinInterval_maxInterval_params_subscriptionEstablished_reportHandlerSelector = mkSelector "subscribeAttributeCurrentHeapFreeWithMinInterval:maxInterval:params:subscriptionEstablished:reportHandler:"

-- | @Selector@ for @readAttributeCurrentHeapFreeWithAttributeCache:endpoint:queue:completionHandler:@
readAttributeCurrentHeapFreeWithAttributeCache_endpoint_queue_completionHandlerSelector :: Selector '[Id MTRAttributeCacheContainer, Id NSNumber, Id NSObject, Ptr ()] ()
readAttributeCurrentHeapFreeWithAttributeCache_endpoint_queue_completionHandlerSelector = mkSelector "readAttributeCurrentHeapFreeWithAttributeCache:endpoint:queue:completionHandler:"

-- | @Selector@ for @readAttributeCurrentHeapUsedWithCompletionHandler:@
readAttributeCurrentHeapUsedWithCompletionHandlerSelector :: Selector '[Ptr ()] ()
readAttributeCurrentHeapUsedWithCompletionHandlerSelector = mkSelector "readAttributeCurrentHeapUsedWithCompletionHandler:"

-- | @Selector@ for @subscribeAttributeCurrentHeapUsedWithMinInterval:maxInterval:params:subscriptionEstablished:reportHandler:@
subscribeAttributeCurrentHeapUsedWithMinInterval_maxInterval_params_subscriptionEstablished_reportHandlerSelector :: Selector '[Id NSNumber, Id NSNumber, Id MTRSubscribeParams, Ptr (), Ptr ()] ()
subscribeAttributeCurrentHeapUsedWithMinInterval_maxInterval_params_subscriptionEstablished_reportHandlerSelector = mkSelector "subscribeAttributeCurrentHeapUsedWithMinInterval:maxInterval:params:subscriptionEstablished:reportHandler:"

-- | @Selector@ for @readAttributeCurrentHeapUsedWithAttributeCache:endpoint:queue:completionHandler:@
readAttributeCurrentHeapUsedWithAttributeCache_endpoint_queue_completionHandlerSelector :: Selector '[Id MTRAttributeCacheContainer, Id NSNumber, Id NSObject, Ptr ()] ()
readAttributeCurrentHeapUsedWithAttributeCache_endpoint_queue_completionHandlerSelector = mkSelector "readAttributeCurrentHeapUsedWithAttributeCache:endpoint:queue:completionHandler:"

-- | @Selector@ for @readAttributeCurrentHeapHighWatermarkWithCompletionHandler:@
readAttributeCurrentHeapHighWatermarkWithCompletionHandlerSelector :: Selector '[Ptr ()] ()
readAttributeCurrentHeapHighWatermarkWithCompletionHandlerSelector = mkSelector "readAttributeCurrentHeapHighWatermarkWithCompletionHandler:"

-- | @Selector@ for @subscribeAttributeCurrentHeapHighWatermarkWithMinInterval:maxInterval:params:subscriptionEstablished:reportHandler:@
subscribeAttributeCurrentHeapHighWatermarkWithMinInterval_maxInterval_params_subscriptionEstablished_reportHandlerSelector :: Selector '[Id NSNumber, Id NSNumber, Id MTRSubscribeParams, Ptr (), Ptr ()] ()
subscribeAttributeCurrentHeapHighWatermarkWithMinInterval_maxInterval_params_subscriptionEstablished_reportHandlerSelector = mkSelector "subscribeAttributeCurrentHeapHighWatermarkWithMinInterval:maxInterval:params:subscriptionEstablished:reportHandler:"

-- | @Selector@ for @readAttributeCurrentHeapHighWatermarkWithAttributeCache:endpoint:queue:completionHandler:@
readAttributeCurrentHeapHighWatermarkWithAttributeCache_endpoint_queue_completionHandlerSelector :: Selector '[Id MTRAttributeCacheContainer, Id NSNumber, Id NSObject, Ptr ()] ()
readAttributeCurrentHeapHighWatermarkWithAttributeCache_endpoint_queue_completionHandlerSelector = mkSelector "readAttributeCurrentHeapHighWatermarkWithAttributeCache:endpoint:queue:completionHandler:"

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
initWithDevice_endpointID_queueSelector :: Selector '[Id MTRBaseDevice, Id NSNumber, Id NSObject] (Id MTRBaseClusterSoftwareDiagnostics)
initWithDevice_endpointID_queueSelector = mkSelector "initWithDevice:endpointID:queue:"

