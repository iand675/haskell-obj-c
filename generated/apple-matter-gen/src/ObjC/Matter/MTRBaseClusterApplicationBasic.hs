{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Cluster Application Basic
--
-- This cluster provides information about an application running on a TV or media player device which is represented as an endpoint.
--
-- Generated bindings for @MTRBaseClusterApplicationBasic@.
module ObjC.Matter.MTRBaseClusterApplicationBasic
  ( MTRBaseClusterApplicationBasic
  , IsMTRBaseClusterApplicationBasic(..)
  , readAttributeVendorNameWithCompletion
  , subscribeAttributeVendorNameWithParams_subscriptionEstablished_reportHandler
  , readAttributeVendorNameWithClusterStateCache_endpoint_queue_completion
  , readAttributeVendorIDWithCompletion
  , subscribeAttributeVendorIDWithParams_subscriptionEstablished_reportHandler
  , readAttributeVendorIDWithClusterStateCache_endpoint_queue_completion
  , readAttributeApplicationNameWithCompletion
  , subscribeAttributeApplicationNameWithParams_subscriptionEstablished_reportHandler
  , readAttributeApplicationNameWithClusterStateCache_endpoint_queue_completion
  , readAttributeProductIDWithCompletion
  , subscribeAttributeProductIDWithParams_subscriptionEstablished_reportHandler
  , readAttributeProductIDWithClusterStateCache_endpoint_queue_completion
  , readAttributeApplicationWithCompletion
  , subscribeAttributeApplicationWithParams_subscriptionEstablished_reportHandler
  , readAttributeApplicationWithClusterStateCache_endpoint_queue_completion
  , readAttributeStatusWithCompletion
  , subscribeAttributeStatusWithParams_subscriptionEstablished_reportHandler
  , readAttributeStatusWithClusterStateCache_endpoint_queue_completion
  , readAttributeApplicationVersionWithCompletion
  , subscribeAttributeApplicationVersionWithParams_subscriptionEstablished_reportHandler
  , readAttributeApplicationVersionWithClusterStateCache_endpoint_queue_completion
  , readAttributeAllowedVendorListWithCompletion
  , subscribeAttributeAllowedVendorListWithParams_subscriptionEstablished_reportHandler
  , readAttributeAllowedVendorListWithClusterStateCache_endpoint_queue_completion
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
  , readAttributeVendorNameWithCompletionHandler
  , subscribeAttributeVendorNameWithMinInterval_maxInterval_params_subscriptionEstablished_reportHandler
  , readAttributeVendorNameWithAttributeCache_endpoint_queue_completionHandler
  , readAttributeVendorIDWithCompletionHandler
  , subscribeAttributeVendorIDWithMinInterval_maxInterval_params_subscriptionEstablished_reportHandler
  , readAttributeVendorIDWithAttributeCache_endpoint_queue_completionHandler
  , readAttributeApplicationNameWithCompletionHandler
  , subscribeAttributeApplicationNameWithMinInterval_maxInterval_params_subscriptionEstablished_reportHandler
  , readAttributeApplicationNameWithAttributeCache_endpoint_queue_completionHandler
  , readAttributeProductIDWithCompletionHandler
  , subscribeAttributeProductIDWithMinInterval_maxInterval_params_subscriptionEstablished_reportHandler
  , readAttributeProductIDWithAttributeCache_endpoint_queue_completionHandler
  , readAttributeApplicationWithCompletionHandler
  , subscribeAttributeApplicationWithMinInterval_maxInterval_params_subscriptionEstablished_reportHandler
  , readAttributeApplicationWithAttributeCache_endpoint_queue_completionHandler
  , readAttributeStatusWithCompletionHandler
  , subscribeAttributeStatusWithMinInterval_maxInterval_params_subscriptionEstablished_reportHandler
  , readAttributeStatusWithAttributeCache_endpoint_queue_completionHandler
  , readAttributeApplicationVersionWithCompletionHandler
  , subscribeAttributeApplicationVersionWithMinInterval_maxInterval_params_subscriptionEstablished_reportHandler
  , readAttributeApplicationVersionWithAttributeCache_endpoint_queue_completionHandler
  , readAttributeAllowedVendorListWithCompletionHandler
  , subscribeAttributeAllowedVendorListWithMinInterval_maxInterval_params_subscriptionEstablished_reportHandler
  , readAttributeAllowedVendorListWithAttributeCache_endpoint_queue_completionHandler
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
  , readAttributeAllowedVendorListWithAttributeCache_endpoint_queue_completionHandlerSelector
  , readAttributeAllowedVendorListWithClusterStateCache_endpoint_queue_completionSelector
  , readAttributeAllowedVendorListWithCompletionHandlerSelector
  , readAttributeAllowedVendorListWithCompletionSelector
  , readAttributeApplicationNameWithAttributeCache_endpoint_queue_completionHandlerSelector
  , readAttributeApplicationNameWithClusterStateCache_endpoint_queue_completionSelector
  , readAttributeApplicationNameWithCompletionHandlerSelector
  , readAttributeApplicationNameWithCompletionSelector
  , readAttributeApplicationVersionWithAttributeCache_endpoint_queue_completionHandlerSelector
  , readAttributeApplicationVersionWithClusterStateCache_endpoint_queue_completionSelector
  , readAttributeApplicationVersionWithCompletionHandlerSelector
  , readAttributeApplicationVersionWithCompletionSelector
  , readAttributeApplicationWithAttributeCache_endpoint_queue_completionHandlerSelector
  , readAttributeApplicationWithClusterStateCache_endpoint_queue_completionSelector
  , readAttributeApplicationWithCompletionHandlerSelector
  , readAttributeApplicationWithCompletionSelector
  , readAttributeAttributeListWithAttributeCache_endpoint_queue_completionHandlerSelector
  , readAttributeAttributeListWithClusterStateCache_endpoint_queue_completionSelector
  , readAttributeAttributeListWithCompletionHandlerSelector
  , readAttributeAttributeListWithCompletionSelector
  , readAttributeClusterRevisionWithAttributeCache_endpoint_queue_completionHandlerSelector
  , readAttributeClusterRevisionWithClusterStateCache_endpoint_queue_completionSelector
  , readAttributeClusterRevisionWithCompletionHandlerSelector
  , readAttributeClusterRevisionWithCompletionSelector
  , readAttributeFeatureMapWithAttributeCache_endpoint_queue_completionHandlerSelector
  , readAttributeFeatureMapWithClusterStateCache_endpoint_queue_completionSelector
  , readAttributeFeatureMapWithCompletionHandlerSelector
  , readAttributeFeatureMapWithCompletionSelector
  , readAttributeGeneratedCommandListWithAttributeCache_endpoint_queue_completionHandlerSelector
  , readAttributeGeneratedCommandListWithClusterStateCache_endpoint_queue_completionSelector
  , readAttributeGeneratedCommandListWithCompletionHandlerSelector
  , readAttributeGeneratedCommandListWithCompletionSelector
  , readAttributeProductIDWithAttributeCache_endpoint_queue_completionHandlerSelector
  , readAttributeProductIDWithClusterStateCache_endpoint_queue_completionSelector
  , readAttributeProductIDWithCompletionHandlerSelector
  , readAttributeProductIDWithCompletionSelector
  , readAttributeStatusWithAttributeCache_endpoint_queue_completionHandlerSelector
  , readAttributeStatusWithClusterStateCache_endpoint_queue_completionSelector
  , readAttributeStatusWithCompletionHandlerSelector
  , readAttributeStatusWithCompletionSelector
  , readAttributeVendorIDWithAttributeCache_endpoint_queue_completionHandlerSelector
  , readAttributeVendorIDWithClusterStateCache_endpoint_queue_completionSelector
  , readAttributeVendorIDWithCompletionHandlerSelector
  , readAttributeVendorIDWithCompletionSelector
  , readAttributeVendorNameWithAttributeCache_endpoint_queue_completionHandlerSelector
  , readAttributeVendorNameWithClusterStateCache_endpoint_queue_completionSelector
  , readAttributeVendorNameWithCompletionHandlerSelector
  , readAttributeVendorNameWithCompletionSelector
  , subscribeAttributeAcceptedCommandListWithMinInterval_maxInterval_params_subscriptionEstablished_reportHandlerSelector
  , subscribeAttributeAcceptedCommandListWithParams_subscriptionEstablished_reportHandlerSelector
  , subscribeAttributeAllowedVendorListWithMinInterval_maxInterval_params_subscriptionEstablished_reportHandlerSelector
  , subscribeAttributeAllowedVendorListWithParams_subscriptionEstablished_reportHandlerSelector
  , subscribeAttributeApplicationNameWithMinInterval_maxInterval_params_subscriptionEstablished_reportHandlerSelector
  , subscribeAttributeApplicationNameWithParams_subscriptionEstablished_reportHandlerSelector
  , subscribeAttributeApplicationVersionWithMinInterval_maxInterval_params_subscriptionEstablished_reportHandlerSelector
  , subscribeAttributeApplicationVersionWithParams_subscriptionEstablished_reportHandlerSelector
  , subscribeAttributeApplicationWithMinInterval_maxInterval_params_subscriptionEstablished_reportHandlerSelector
  , subscribeAttributeApplicationWithParams_subscriptionEstablished_reportHandlerSelector
  , subscribeAttributeAttributeListWithMinInterval_maxInterval_params_subscriptionEstablished_reportHandlerSelector
  , subscribeAttributeAttributeListWithParams_subscriptionEstablished_reportHandlerSelector
  , subscribeAttributeClusterRevisionWithMinInterval_maxInterval_params_subscriptionEstablished_reportHandlerSelector
  , subscribeAttributeClusterRevisionWithParams_subscriptionEstablished_reportHandlerSelector
  , subscribeAttributeFeatureMapWithMinInterval_maxInterval_params_subscriptionEstablished_reportHandlerSelector
  , subscribeAttributeFeatureMapWithParams_subscriptionEstablished_reportHandlerSelector
  , subscribeAttributeGeneratedCommandListWithMinInterval_maxInterval_params_subscriptionEstablished_reportHandlerSelector
  , subscribeAttributeGeneratedCommandListWithParams_subscriptionEstablished_reportHandlerSelector
  , subscribeAttributeProductIDWithMinInterval_maxInterval_params_subscriptionEstablished_reportHandlerSelector
  , subscribeAttributeProductIDWithParams_subscriptionEstablished_reportHandlerSelector
  , subscribeAttributeStatusWithMinInterval_maxInterval_params_subscriptionEstablished_reportHandlerSelector
  , subscribeAttributeStatusWithParams_subscriptionEstablished_reportHandlerSelector
  , subscribeAttributeVendorIDWithMinInterval_maxInterval_params_subscriptionEstablished_reportHandlerSelector
  , subscribeAttributeVendorIDWithParams_subscriptionEstablished_reportHandlerSelector
  , subscribeAttributeVendorNameWithMinInterval_maxInterval_params_subscriptionEstablished_reportHandlerSelector
  , subscribeAttributeVendorNameWithParams_subscriptionEstablished_reportHandlerSelector


  ) where

import Foreign.Ptr (Ptr, FunPtr)
import Foreign.C.Types

import ObjC.Runtime.Types
import ObjC.Runtime.Message (sendMessage, sendOwnedMessage, sendClassMessage, sendOwnedClassMessage)
import ObjC.Runtime.Selector (mkSelector)
import ObjC.Runtime.Class (getRequiredClass)

import ObjC.Matter.Internal.Classes
import ObjC.Foundation.Internal.Classes

-- | @- readAttributeVendorNameWithCompletion:@
readAttributeVendorNameWithCompletion :: IsMTRBaseClusterApplicationBasic mtrBaseClusterApplicationBasic => mtrBaseClusterApplicationBasic -> Ptr () -> IO ()
readAttributeVendorNameWithCompletion mtrBaseClusterApplicationBasic completion =
  sendMessage mtrBaseClusterApplicationBasic readAttributeVendorNameWithCompletionSelector completion

-- | @- subscribeAttributeVendorNameWithParams:subscriptionEstablished:reportHandler:@
subscribeAttributeVendorNameWithParams_subscriptionEstablished_reportHandler :: (IsMTRBaseClusterApplicationBasic mtrBaseClusterApplicationBasic, IsMTRSubscribeParams params) => mtrBaseClusterApplicationBasic -> params -> Ptr () -> Ptr () -> IO ()
subscribeAttributeVendorNameWithParams_subscriptionEstablished_reportHandler mtrBaseClusterApplicationBasic params subscriptionEstablished reportHandler =
  sendMessage mtrBaseClusterApplicationBasic subscribeAttributeVendorNameWithParams_subscriptionEstablished_reportHandlerSelector (toMTRSubscribeParams params) subscriptionEstablished reportHandler

-- | @+ readAttributeVendorNameWithClusterStateCache:endpoint:queue:completion:@
readAttributeVendorNameWithClusterStateCache_endpoint_queue_completion :: (IsMTRClusterStateCacheContainer clusterStateCacheContainer, IsNSNumber endpoint, IsNSObject queue) => clusterStateCacheContainer -> endpoint -> queue -> Ptr () -> IO ()
readAttributeVendorNameWithClusterStateCache_endpoint_queue_completion clusterStateCacheContainer endpoint queue completion =
  do
    cls' <- getRequiredClass "MTRBaseClusterApplicationBasic"
    sendClassMessage cls' readAttributeVendorNameWithClusterStateCache_endpoint_queue_completionSelector (toMTRClusterStateCacheContainer clusterStateCacheContainer) (toNSNumber endpoint) (toNSObject queue) completion

-- | @- readAttributeVendorIDWithCompletion:@
readAttributeVendorIDWithCompletion :: IsMTRBaseClusterApplicationBasic mtrBaseClusterApplicationBasic => mtrBaseClusterApplicationBasic -> Ptr () -> IO ()
readAttributeVendorIDWithCompletion mtrBaseClusterApplicationBasic completion =
  sendMessage mtrBaseClusterApplicationBasic readAttributeVendorIDWithCompletionSelector completion

-- | @- subscribeAttributeVendorIDWithParams:subscriptionEstablished:reportHandler:@
subscribeAttributeVendorIDWithParams_subscriptionEstablished_reportHandler :: (IsMTRBaseClusterApplicationBasic mtrBaseClusterApplicationBasic, IsMTRSubscribeParams params) => mtrBaseClusterApplicationBasic -> params -> Ptr () -> Ptr () -> IO ()
subscribeAttributeVendorIDWithParams_subscriptionEstablished_reportHandler mtrBaseClusterApplicationBasic params subscriptionEstablished reportHandler =
  sendMessage mtrBaseClusterApplicationBasic subscribeAttributeVendorIDWithParams_subscriptionEstablished_reportHandlerSelector (toMTRSubscribeParams params) subscriptionEstablished reportHandler

-- | @+ readAttributeVendorIDWithClusterStateCache:endpoint:queue:completion:@
readAttributeVendorIDWithClusterStateCache_endpoint_queue_completion :: (IsMTRClusterStateCacheContainer clusterStateCacheContainer, IsNSNumber endpoint, IsNSObject queue) => clusterStateCacheContainer -> endpoint -> queue -> Ptr () -> IO ()
readAttributeVendorIDWithClusterStateCache_endpoint_queue_completion clusterStateCacheContainer endpoint queue completion =
  do
    cls' <- getRequiredClass "MTRBaseClusterApplicationBasic"
    sendClassMessage cls' readAttributeVendorIDWithClusterStateCache_endpoint_queue_completionSelector (toMTRClusterStateCacheContainer clusterStateCacheContainer) (toNSNumber endpoint) (toNSObject queue) completion

-- | @- readAttributeApplicationNameWithCompletion:@
readAttributeApplicationNameWithCompletion :: IsMTRBaseClusterApplicationBasic mtrBaseClusterApplicationBasic => mtrBaseClusterApplicationBasic -> Ptr () -> IO ()
readAttributeApplicationNameWithCompletion mtrBaseClusterApplicationBasic completion =
  sendMessage mtrBaseClusterApplicationBasic readAttributeApplicationNameWithCompletionSelector completion

-- | @- subscribeAttributeApplicationNameWithParams:subscriptionEstablished:reportHandler:@
subscribeAttributeApplicationNameWithParams_subscriptionEstablished_reportHandler :: (IsMTRBaseClusterApplicationBasic mtrBaseClusterApplicationBasic, IsMTRSubscribeParams params) => mtrBaseClusterApplicationBasic -> params -> Ptr () -> Ptr () -> IO ()
subscribeAttributeApplicationNameWithParams_subscriptionEstablished_reportHandler mtrBaseClusterApplicationBasic params subscriptionEstablished reportHandler =
  sendMessage mtrBaseClusterApplicationBasic subscribeAttributeApplicationNameWithParams_subscriptionEstablished_reportHandlerSelector (toMTRSubscribeParams params) subscriptionEstablished reportHandler

-- | @+ readAttributeApplicationNameWithClusterStateCache:endpoint:queue:completion:@
readAttributeApplicationNameWithClusterStateCache_endpoint_queue_completion :: (IsMTRClusterStateCacheContainer clusterStateCacheContainer, IsNSNumber endpoint, IsNSObject queue) => clusterStateCacheContainer -> endpoint -> queue -> Ptr () -> IO ()
readAttributeApplicationNameWithClusterStateCache_endpoint_queue_completion clusterStateCacheContainer endpoint queue completion =
  do
    cls' <- getRequiredClass "MTRBaseClusterApplicationBasic"
    sendClassMessage cls' readAttributeApplicationNameWithClusterStateCache_endpoint_queue_completionSelector (toMTRClusterStateCacheContainer clusterStateCacheContainer) (toNSNumber endpoint) (toNSObject queue) completion

-- | @- readAttributeProductIDWithCompletion:@
readAttributeProductIDWithCompletion :: IsMTRBaseClusterApplicationBasic mtrBaseClusterApplicationBasic => mtrBaseClusterApplicationBasic -> Ptr () -> IO ()
readAttributeProductIDWithCompletion mtrBaseClusterApplicationBasic completion =
  sendMessage mtrBaseClusterApplicationBasic readAttributeProductIDWithCompletionSelector completion

-- | @- subscribeAttributeProductIDWithParams:subscriptionEstablished:reportHandler:@
subscribeAttributeProductIDWithParams_subscriptionEstablished_reportHandler :: (IsMTRBaseClusterApplicationBasic mtrBaseClusterApplicationBasic, IsMTRSubscribeParams params) => mtrBaseClusterApplicationBasic -> params -> Ptr () -> Ptr () -> IO ()
subscribeAttributeProductIDWithParams_subscriptionEstablished_reportHandler mtrBaseClusterApplicationBasic params subscriptionEstablished reportHandler =
  sendMessage mtrBaseClusterApplicationBasic subscribeAttributeProductIDWithParams_subscriptionEstablished_reportHandlerSelector (toMTRSubscribeParams params) subscriptionEstablished reportHandler

-- | @+ readAttributeProductIDWithClusterStateCache:endpoint:queue:completion:@
readAttributeProductIDWithClusterStateCache_endpoint_queue_completion :: (IsMTRClusterStateCacheContainer clusterStateCacheContainer, IsNSNumber endpoint, IsNSObject queue) => clusterStateCacheContainer -> endpoint -> queue -> Ptr () -> IO ()
readAttributeProductIDWithClusterStateCache_endpoint_queue_completion clusterStateCacheContainer endpoint queue completion =
  do
    cls' <- getRequiredClass "MTRBaseClusterApplicationBasic"
    sendClassMessage cls' readAttributeProductIDWithClusterStateCache_endpoint_queue_completionSelector (toMTRClusterStateCacheContainer clusterStateCacheContainer) (toNSNumber endpoint) (toNSObject queue) completion

-- | @- readAttributeApplicationWithCompletion:@
readAttributeApplicationWithCompletion :: IsMTRBaseClusterApplicationBasic mtrBaseClusterApplicationBasic => mtrBaseClusterApplicationBasic -> Ptr () -> IO ()
readAttributeApplicationWithCompletion mtrBaseClusterApplicationBasic completion =
  sendMessage mtrBaseClusterApplicationBasic readAttributeApplicationWithCompletionSelector completion

-- | @- subscribeAttributeApplicationWithParams:subscriptionEstablished:reportHandler:@
subscribeAttributeApplicationWithParams_subscriptionEstablished_reportHandler :: (IsMTRBaseClusterApplicationBasic mtrBaseClusterApplicationBasic, IsMTRSubscribeParams params) => mtrBaseClusterApplicationBasic -> params -> Ptr () -> Ptr () -> IO ()
subscribeAttributeApplicationWithParams_subscriptionEstablished_reportHandler mtrBaseClusterApplicationBasic params subscriptionEstablished reportHandler =
  sendMessage mtrBaseClusterApplicationBasic subscribeAttributeApplicationWithParams_subscriptionEstablished_reportHandlerSelector (toMTRSubscribeParams params) subscriptionEstablished reportHandler

-- | @+ readAttributeApplicationWithClusterStateCache:endpoint:queue:completion:@
readAttributeApplicationWithClusterStateCache_endpoint_queue_completion :: (IsMTRClusterStateCacheContainer clusterStateCacheContainer, IsNSNumber endpoint, IsNSObject queue) => clusterStateCacheContainer -> endpoint -> queue -> Ptr () -> IO ()
readAttributeApplicationWithClusterStateCache_endpoint_queue_completion clusterStateCacheContainer endpoint queue completion =
  do
    cls' <- getRequiredClass "MTRBaseClusterApplicationBasic"
    sendClassMessage cls' readAttributeApplicationWithClusterStateCache_endpoint_queue_completionSelector (toMTRClusterStateCacheContainer clusterStateCacheContainer) (toNSNumber endpoint) (toNSObject queue) completion

-- | @- readAttributeStatusWithCompletion:@
readAttributeStatusWithCompletion :: IsMTRBaseClusterApplicationBasic mtrBaseClusterApplicationBasic => mtrBaseClusterApplicationBasic -> Ptr () -> IO ()
readAttributeStatusWithCompletion mtrBaseClusterApplicationBasic completion =
  sendMessage mtrBaseClusterApplicationBasic readAttributeStatusWithCompletionSelector completion

-- | @- subscribeAttributeStatusWithParams:subscriptionEstablished:reportHandler:@
subscribeAttributeStatusWithParams_subscriptionEstablished_reportHandler :: (IsMTRBaseClusterApplicationBasic mtrBaseClusterApplicationBasic, IsMTRSubscribeParams params) => mtrBaseClusterApplicationBasic -> params -> Ptr () -> Ptr () -> IO ()
subscribeAttributeStatusWithParams_subscriptionEstablished_reportHandler mtrBaseClusterApplicationBasic params subscriptionEstablished reportHandler =
  sendMessage mtrBaseClusterApplicationBasic subscribeAttributeStatusWithParams_subscriptionEstablished_reportHandlerSelector (toMTRSubscribeParams params) subscriptionEstablished reportHandler

-- | @+ readAttributeStatusWithClusterStateCache:endpoint:queue:completion:@
readAttributeStatusWithClusterStateCache_endpoint_queue_completion :: (IsMTRClusterStateCacheContainer clusterStateCacheContainer, IsNSNumber endpoint, IsNSObject queue) => clusterStateCacheContainer -> endpoint -> queue -> Ptr () -> IO ()
readAttributeStatusWithClusterStateCache_endpoint_queue_completion clusterStateCacheContainer endpoint queue completion =
  do
    cls' <- getRequiredClass "MTRBaseClusterApplicationBasic"
    sendClassMessage cls' readAttributeStatusWithClusterStateCache_endpoint_queue_completionSelector (toMTRClusterStateCacheContainer clusterStateCacheContainer) (toNSNumber endpoint) (toNSObject queue) completion

-- | @- readAttributeApplicationVersionWithCompletion:@
readAttributeApplicationVersionWithCompletion :: IsMTRBaseClusterApplicationBasic mtrBaseClusterApplicationBasic => mtrBaseClusterApplicationBasic -> Ptr () -> IO ()
readAttributeApplicationVersionWithCompletion mtrBaseClusterApplicationBasic completion =
  sendMessage mtrBaseClusterApplicationBasic readAttributeApplicationVersionWithCompletionSelector completion

-- | @- subscribeAttributeApplicationVersionWithParams:subscriptionEstablished:reportHandler:@
subscribeAttributeApplicationVersionWithParams_subscriptionEstablished_reportHandler :: (IsMTRBaseClusterApplicationBasic mtrBaseClusterApplicationBasic, IsMTRSubscribeParams params) => mtrBaseClusterApplicationBasic -> params -> Ptr () -> Ptr () -> IO ()
subscribeAttributeApplicationVersionWithParams_subscriptionEstablished_reportHandler mtrBaseClusterApplicationBasic params subscriptionEstablished reportHandler =
  sendMessage mtrBaseClusterApplicationBasic subscribeAttributeApplicationVersionWithParams_subscriptionEstablished_reportHandlerSelector (toMTRSubscribeParams params) subscriptionEstablished reportHandler

-- | @+ readAttributeApplicationVersionWithClusterStateCache:endpoint:queue:completion:@
readAttributeApplicationVersionWithClusterStateCache_endpoint_queue_completion :: (IsMTRClusterStateCacheContainer clusterStateCacheContainer, IsNSNumber endpoint, IsNSObject queue) => clusterStateCacheContainer -> endpoint -> queue -> Ptr () -> IO ()
readAttributeApplicationVersionWithClusterStateCache_endpoint_queue_completion clusterStateCacheContainer endpoint queue completion =
  do
    cls' <- getRequiredClass "MTRBaseClusterApplicationBasic"
    sendClassMessage cls' readAttributeApplicationVersionWithClusterStateCache_endpoint_queue_completionSelector (toMTRClusterStateCacheContainer clusterStateCacheContainer) (toNSNumber endpoint) (toNSObject queue) completion

-- | @- readAttributeAllowedVendorListWithCompletion:@
readAttributeAllowedVendorListWithCompletion :: IsMTRBaseClusterApplicationBasic mtrBaseClusterApplicationBasic => mtrBaseClusterApplicationBasic -> Ptr () -> IO ()
readAttributeAllowedVendorListWithCompletion mtrBaseClusterApplicationBasic completion =
  sendMessage mtrBaseClusterApplicationBasic readAttributeAllowedVendorListWithCompletionSelector completion

-- | @- subscribeAttributeAllowedVendorListWithParams:subscriptionEstablished:reportHandler:@
subscribeAttributeAllowedVendorListWithParams_subscriptionEstablished_reportHandler :: (IsMTRBaseClusterApplicationBasic mtrBaseClusterApplicationBasic, IsMTRSubscribeParams params) => mtrBaseClusterApplicationBasic -> params -> Ptr () -> Ptr () -> IO ()
subscribeAttributeAllowedVendorListWithParams_subscriptionEstablished_reportHandler mtrBaseClusterApplicationBasic params subscriptionEstablished reportHandler =
  sendMessage mtrBaseClusterApplicationBasic subscribeAttributeAllowedVendorListWithParams_subscriptionEstablished_reportHandlerSelector (toMTRSubscribeParams params) subscriptionEstablished reportHandler

-- | @+ readAttributeAllowedVendorListWithClusterStateCache:endpoint:queue:completion:@
readAttributeAllowedVendorListWithClusterStateCache_endpoint_queue_completion :: (IsMTRClusterStateCacheContainer clusterStateCacheContainer, IsNSNumber endpoint, IsNSObject queue) => clusterStateCacheContainer -> endpoint -> queue -> Ptr () -> IO ()
readAttributeAllowedVendorListWithClusterStateCache_endpoint_queue_completion clusterStateCacheContainer endpoint queue completion =
  do
    cls' <- getRequiredClass "MTRBaseClusterApplicationBasic"
    sendClassMessage cls' readAttributeAllowedVendorListWithClusterStateCache_endpoint_queue_completionSelector (toMTRClusterStateCacheContainer clusterStateCacheContainer) (toNSNumber endpoint) (toNSObject queue) completion

-- | @- readAttributeGeneratedCommandListWithCompletion:@
readAttributeGeneratedCommandListWithCompletion :: IsMTRBaseClusterApplicationBasic mtrBaseClusterApplicationBasic => mtrBaseClusterApplicationBasic -> Ptr () -> IO ()
readAttributeGeneratedCommandListWithCompletion mtrBaseClusterApplicationBasic completion =
  sendMessage mtrBaseClusterApplicationBasic readAttributeGeneratedCommandListWithCompletionSelector completion

-- | @- subscribeAttributeGeneratedCommandListWithParams:subscriptionEstablished:reportHandler:@
subscribeAttributeGeneratedCommandListWithParams_subscriptionEstablished_reportHandler :: (IsMTRBaseClusterApplicationBasic mtrBaseClusterApplicationBasic, IsMTRSubscribeParams params) => mtrBaseClusterApplicationBasic -> params -> Ptr () -> Ptr () -> IO ()
subscribeAttributeGeneratedCommandListWithParams_subscriptionEstablished_reportHandler mtrBaseClusterApplicationBasic params subscriptionEstablished reportHandler =
  sendMessage mtrBaseClusterApplicationBasic subscribeAttributeGeneratedCommandListWithParams_subscriptionEstablished_reportHandlerSelector (toMTRSubscribeParams params) subscriptionEstablished reportHandler

-- | @+ readAttributeGeneratedCommandListWithClusterStateCache:endpoint:queue:completion:@
readAttributeGeneratedCommandListWithClusterStateCache_endpoint_queue_completion :: (IsMTRClusterStateCacheContainer clusterStateCacheContainer, IsNSNumber endpoint, IsNSObject queue) => clusterStateCacheContainer -> endpoint -> queue -> Ptr () -> IO ()
readAttributeGeneratedCommandListWithClusterStateCache_endpoint_queue_completion clusterStateCacheContainer endpoint queue completion =
  do
    cls' <- getRequiredClass "MTRBaseClusterApplicationBasic"
    sendClassMessage cls' readAttributeGeneratedCommandListWithClusterStateCache_endpoint_queue_completionSelector (toMTRClusterStateCacheContainer clusterStateCacheContainer) (toNSNumber endpoint) (toNSObject queue) completion

-- | @- readAttributeAcceptedCommandListWithCompletion:@
readAttributeAcceptedCommandListWithCompletion :: IsMTRBaseClusterApplicationBasic mtrBaseClusterApplicationBasic => mtrBaseClusterApplicationBasic -> Ptr () -> IO ()
readAttributeAcceptedCommandListWithCompletion mtrBaseClusterApplicationBasic completion =
  sendMessage mtrBaseClusterApplicationBasic readAttributeAcceptedCommandListWithCompletionSelector completion

-- | @- subscribeAttributeAcceptedCommandListWithParams:subscriptionEstablished:reportHandler:@
subscribeAttributeAcceptedCommandListWithParams_subscriptionEstablished_reportHandler :: (IsMTRBaseClusterApplicationBasic mtrBaseClusterApplicationBasic, IsMTRSubscribeParams params) => mtrBaseClusterApplicationBasic -> params -> Ptr () -> Ptr () -> IO ()
subscribeAttributeAcceptedCommandListWithParams_subscriptionEstablished_reportHandler mtrBaseClusterApplicationBasic params subscriptionEstablished reportHandler =
  sendMessage mtrBaseClusterApplicationBasic subscribeAttributeAcceptedCommandListWithParams_subscriptionEstablished_reportHandlerSelector (toMTRSubscribeParams params) subscriptionEstablished reportHandler

-- | @+ readAttributeAcceptedCommandListWithClusterStateCache:endpoint:queue:completion:@
readAttributeAcceptedCommandListWithClusterStateCache_endpoint_queue_completion :: (IsMTRClusterStateCacheContainer clusterStateCacheContainer, IsNSNumber endpoint, IsNSObject queue) => clusterStateCacheContainer -> endpoint -> queue -> Ptr () -> IO ()
readAttributeAcceptedCommandListWithClusterStateCache_endpoint_queue_completion clusterStateCacheContainer endpoint queue completion =
  do
    cls' <- getRequiredClass "MTRBaseClusterApplicationBasic"
    sendClassMessage cls' readAttributeAcceptedCommandListWithClusterStateCache_endpoint_queue_completionSelector (toMTRClusterStateCacheContainer clusterStateCacheContainer) (toNSNumber endpoint) (toNSObject queue) completion

-- | @- readAttributeAttributeListWithCompletion:@
readAttributeAttributeListWithCompletion :: IsMTRBaseClusterApplicationBasic mtrBaseClusterApplicationBasic => mtrBaseClusterApplicationBasic -> Ptr () -> IO ()
readAttributeAttributeListWithCompletion mtrBaseClusterApplicationBasic completion =
  sendMessage mtrBaseClusterApplicationBasic readAttributeAttributeListWithCompletionSelector completion

-- | @- subscribeAttributeAttributeListWithParams:subscriptionEstablished:reportHandler:@
subscribeAttributeAttributeListWithParams_subscriptionEstablished_reportHandler :: (IsMTRBaseClusterApplicationBasic mtrBaseClusterApplicationBasic, IsMTRSubscribeParams params) => mtrBaseClusterApplicationBasic -> params -> Ptr () -> Ptr () -> IO ()
subscribeAttributeAttributeListWithParams_subscriptionEstablished_reportHandler mtrBaseClusterApplicationBasic params subscriptionEstablished reportHandler =
  sendMessage mtrBaseClusterApplicationBasic subscribeAttributeAttributeListWithParams_subscriptionEstablished_reportHandlerSelector (toMTRSubscribeParams params) subscriptionEstablished reportHandler

-- | @+ readAttributeAttributeListWithClusterStateCache:endpoint:queue:completion:@
readAttributeAttributeListWithClusterStateCache_endpoint_queue_completion :: (IsMTRClusterStateCacheContainer clusterStateCacheContainer, IsNSNumber endpoint, IsNSObject queue) => clusterStateCacheContainer -> endpoint -> queue -> Ptr () -> IO ()
readAttributeAttributeListWithClusterStateCache_endpoint_queue_completion clusterStateCacheContainer endpoint queue completion =
  do
    cls' <- getRequiredClass "MTRBaseClusterApplicationBasic"
    sendClassMessage cls' readAttributeAttributeListWithClusterStateCache_endpoint_queue_completionSelector (toMTRClusterStateCacheContainer clusterStateCacheContainer) (toNSNumber endpoint) (toNSObject queue) completion

-- | @- readAttributeFeatureMapWithCompletion:@
readAttributeFeatureMapWithCompletion :: IsMTRBaseClusterApplicationBasic mtrBaseClusterApplicationBasic => mtrBaseClusterApplicationBasic -> Ptr () -> IO ()
readAttributeFeatureMapWithCompletion mtrBaseClusterApplicationBasic completion =
  sendMessage mtrBaseClusterApplicationBasic readAttributeFeatureMapWithCompletionSelector completion

-- | @- subscribeAttributeFeatureMapWithParams:subscriptionEstablished:reportHandler:@
subscribeAttributeFeatureMapWithParams_subscriptionEstablished_reportHandler :: (IsMTRBaseClusterApplicationBasic mtrBaseClusterApplicationBasic, IsMTRSubscribeParams params) => mtrBaseClusterApplicationBasic -> params -> Ptr () -> Ptr () -> IO ()
subscribeAttributeFeatureMapWithParams_subscriptionEstablished_reportHandler mtrBaseClusterApplicationBasic params subscriptionEstablished reportHandler =
  sendMessage mtrBaseClusterApplicationBasic subscribeAttributeFeatureMapWithParams_subscriptionEstablished_reportHandlerSelector (toMTRSubscribeParams params) subscriptionEstablished reportHandler

-- | @+ readAttributeFeatureMapWithClusterStateCache:endpoint:queue:completion:@
readAttributeFeatureMapWithClusterStateCache_endpoint_queue_completion :: (IsMTRClusterStateCacheContainer clusterStateCacheContainer, IsNSNumber endpoint, IsNSObject queue) => clusterStateCacheContainer -> endpoint -> queue -> Ptr () -> IO ()
readAttributeFeatureMapWithClusterStateCache_endpoint_queue_completion clusterStateCacheContainer endpoint queue completion =
  do
    cls' <- getRequiredClass "MTRBaseClusterApplicationBasic"
    sendClassMessage cls' readAttributeFeatureMapWithClusterStateCache_endpoint_queue_completionSelector (toMTRClusterStateCacheContainer clusterStateCacheContainer) (toNSNumber endpoint) (toNSObject queue) completion

-- | @- readAttributeClusterRevisionWithCompletion:@
readAttributeClusterRevisionWithCompletion :: IsMTRBaseClusterApplicationBasic mtrBaseClusterApplicationBasic => mtrBaseClusterApplicationBasic -> Ptr () -> IO ()
readAttributeClusterRevisionWithCompletion mtrBaseClusterApplicationBasic completion =
  sendMessage mtrBaseClusterApplicationBasic readAttributeClusterRevisionWithCompletionSelector completion

-- | @- subscribeAttributeClusterRevisionWithParams:subscriptionEstablished:reportHandler:@
subscribeAttributeClusterRevisionWithParams_subscriptionEstablished_reportHandler :: (IsMTRBaseClusterApplicationBasic mtrBaseClusterApplicationBasic, IsMTRSubscribeParams params) => mtrBaseClusterApplicationBasic -> params -> Ptr () -> Ptr () -> IO ()
subscribeAttributeClusterRevisionWithParams_subscriptionEstablished_reportHandler mtrBaseClusterApplicationBasic params subscriptionEstablished reportHandler =
  sendMessage mtrBaseClusterApplicationBasic subscribeAttributeClusterRevisionWithParams_subscriptionEstablished_reportHandlerSelector (toMTRSubscribeParams params) subscriptionEstablished reportHandler

-- | @+ readAttributeClusterRevisionWithClusterStateCache:endpoint:queue:completion:@
readAttributeClusterRevisionWithClusterStateCache_endpoint_queue_completion :: (IsMTRClusterStateCacheContainer clusterStateCacheContainer, IsNSNumber endpoint, IsNSObject queue) => clusterStateCacheContainer -> endpoint -> queue -> Ptr () -> IO ()
readAttributeClusterRevisionWithClusterStateCache_endpoint_queue_completion clusterStateCacheContainer endpoint queue completion =
  do
    cls' <- getRequiredClass "MTRBaseClusterApplicationBasic"
    sendClassMessage cls' readAttributeClusterRevisionWithClusterStateCache_endpoint_queue_completionSelector (toMTRClusterStateCacheContainer clusterStateCacheContainer) (toNSNumber endpoint) (toNSObject queue) completion

-- | @- init@
init_ :: IsMTRBaseClusterApplicationBasic mtrBaseClusterApplicationBasic => mtrBaseClusterApplicationBasic -> IO (Id MTRBaseClusterApplicationBasic)
init_ mtrBaseClusterApplicationBasic =
  sendOwnedMessage mtrBaseClusterApplicationBasic initSelector

-- | @+ new@
new :: IO (Id MTRBaseClusterApplicationBasic)
new  =
  do
    cls' <- getRequiredClass "MTRBaseClusterApplicationBasic"
    sendOwnedClassMessage cls' newSelector

-- | @- initWithDevice:endpoint:queue:@
initWithDevice_endpoint_queue :: (IsMTRBaseClusterApplicationBasic mtrBaseClusterApplicationBasic, IsMTRBaseDevice device, IsNSObject queue) => mtrBaseClusterApplicationBasic -> device -> CUShort -> queue -> IO (Id MTRBaseClusterApplicationBasic)
initWithDevice_endpoint_queue mtrBaseClusterApplicationBasic device endpoint queue =
  sendOwnedMessage mtrBaseClusterApplicationBasic initWithDevice_endpoint_queueSelector (toMTRBaseDevice device) endpoint (toNSObject queue)

-- | @- readAttributeVendorNameWithCompletionHandler:@
readAttributeVendorNameWithCompletionHandler :: IsMTRBaseClusterApplicationBasic mtrBaseClusterApplicationBasic => mtrBaseClusterApplicationBasic -> Ptr () -> IO ()
readAttributeVendorNameWithCompletionHandler mtrBaseClusterApplicationBasic completionHandler =
  sendMessage mtrBaseClusterApplicationBasic readAttributeVendorNameWithCompletionHandlerSelector completionHandler

-- | @- subscribeAttributeVendorNameWithMinInterval:maxInterval:params:subscriptionEstablished:reportHandler:@
subscribeAttributeVendorNameWithMinInterval_maxInterval_params_subscriptionEstablished_reportHandler :: (IsMTRBaseClusterApplicationBasic mtrBaseClusterApplicationBasic, IsNSNumber minInterval, IsNSNumber maxInterval, IsMTRSubscribeParams params) => mtrBaseClusterApplicationBasic -> minInterval -> maxInterval -> params -> Ptr () -> Ptr () -> IO ()
subscribeAttributeVendorNameWithMinInterval_maxInterval_params_subscriptionEstablished_reportHandler mtrBaseClusterApplicationBasic minInterval maxInterval params subscriptionEstablishedHandler reportHandler =
  sendMessage mtrBaseClusterApplicationBasic subscribeAttributeVendorNameWithMinInterval_maxInterval_params_subscriptionEstablished_reportHandlerSelector (toNSNumber minInterval) (toNSNumber maxInterval) (toMTRSubscribeParams params) subscriptionEstablishedHandler reportHandler

-- | @+ readAttributeVendorNameWithAttributeCache:endpoint:queue:completionHandler:@
readAttributeVendorNameWithAttributeCache_endpoint_queue_completionHandler :: (IsMTRAttributeCacheContainer attributeCacheContainer, IsNSNumber endpoint, IsNSObject queue) => attributeCacheContainer -> endpoint -> queue -> Ptr () -> IO ()
readAttributeVendorNameWithAttributeCache_endpoint_queue_completionHandler attributeCacheContainer endpoint queue completionHandler =
  do
    cls' <- getRequiredClass "MTRBaseClusterApplicationBasic"
    sendClassMessage cls' readAttributeVendorNameWithAttributeCache_endpoint_queue_completionHandlerSelector (toMTRAttributeCacheContainer attributeCacheContainer) (toNSNumber endpoint) (toNSObject queue) completionHandler

-- | @- readAttributeVendorIDWithCompletionHandler:@
readAttributeVendorIDWithCompletionHandler :: IsMTRBaseClusterApplicationBasic mtrBaseClusterApplicationBasic => mtrBaseClusterApplicationBasic -> Ptr () -> IO ()
readAttributeVendorIDWithCompletionHandler mtrBaseClusterApplicationBasic completionHandler =
  sendMessage mtrBaseClusterApplicationBasic readAttributeVendorIDWithCompletionHandlerSelector completionHandler

-- | @- subscribeAttributeVendorIDWithMinInterval:maxInterval:params:subscriptionEstablished:reportHandler:@
subscribeAttributeVendorIDWithMinInterval_maxInterval_params_subscriptionEstablished_reportHandler :: (IsMTRBaseClusterApplicationBasic mtrBaseClusterApplicationBasic, IsNSNumber minInterval, IsNSNumber maxInterval, IsMTRSubscribeParams params) => mtrBaseClusterApplicationBasic -> minInterval -> maxInterval -> params -> Ptr () -> Ptr () -> IO ()
subscribeAttributeVendorIDWithMinInterval_maxInterval_params_subscriptionEstablished_reportHandler mtrBaseClusterApplicationBasic minInterval maxInterval params subscriptionEstablishedHandler reportHandler =
  sendMessage mtrBaseClusterApplicationBasic subscribeAttributeVendorIDWithMinInterval_maxInterval_params_subscriptionEstablished_reportHandlerSelector (toNSNumber minInterval) (toNSNumber maxInterval) (toMTRSubscribeParams params) subscriptionEstablishedHandler reportHandler

-- | @+ readAttributeVendorIDWithAttributeCache:endpoint:queue:completionHandler:@
readAttributeVendorIDWithAttributeCache_endpoint_queue_completionHandler :: (IsMTRAttributeCacheContainer attributeCacheContainer, IsNSNumber endpoint, IsNSObject queue) => attributeCacheContainer -> endpoint -> queue -> Ptr () -> IO ()
readAttributeVendorIDWithAttributeCache_endpoint_queue_completionHandler attributeCacheContainer endpoint queue completionHandler =
  do
    cls' <- getRequiredClass "MTRBaseClusterApplicationBasic"
    sendClassMessage cls' readAttributeVendorIDWithAttributeCache_endpoint_queue_completionHandlerSelector (toMTRAttributeCacheContainer attributeCacheContainer) (toNSNumber endpoint) (toNSObject queue) completionHandler

-- | @- readAttributeApplicationNameWithCompletionHandler:@
readAttributeApplicationNameWithCompletionHandler :: IsMTRBaseClusterApplicationBasic mtrBaseClusterApplicationBasic => mtrBaseClusterApplicationBasic -> Ptr () -> IO ()
readAttributeApplicationNameWithCompletionHandler mtrBaseClusterApplicationBasic completionHandler =
  sendMessage mtrBaseClusterApplicationBasic readAttributeApplicationNameWithCompletionHandlerSelector completionHandler

-- | @- subscribeAttributeApplicationNameWithMinInterval:maxInterval:params:subscriptionEstablished:reportHandler:@
subscribeAttributeApplicationNameWithMinInterval_maxInterval_params_subscriptionEstablished_reportHandler :: (IsMTRBaseClusterApplicationBasic mtrBaseClusterApplicationBasic, IsNSNumber minInterval, IsNSNumber maxInterval, IsMTRSubscribeParams params) => mtrBaseClusterApplicationBasic -> minInterval -> maxInterval -> params -> Ptr () -> Ptr () -> IO ()
subscribeAttributeApplicationNameWithMinInterval_maxInterval_params_subscriptionEstablished_reportHandler mtrBaseClusterApplicationBasic minInterval maxInterval params subscriptionEstablishedHandler reportHandler =
  sendMessage mtrBaseClusterApplicationBasic subscribeAttributeApplicationNameWithMinInterval_maxInterval_params_subscriptionEstablished_reportHandlerSelector (toNSNumber minInterval) (toNSNumber maxInterval) (toMTRSubscribeParams params) subscriptionEstablishedHandler reportHandler

-- | @+ readAttributeApplicationNameWithAttributeCache:endpoint:queue:completionHandler:@
readAttributeApplicationNameWithAttributeCache_endpoint_queue_completionHandler :: (IsMTRAttributeCacheContainer attributeCacheContainer, IsNSNumber endpoint, IsNSObject queue) => attributeCacheContainer -> endpoint -> queue -> Ptr () -> IO ()
readAttributeApplicationNameWithAttributeCache_endpoint_queue_completionHandler attributeCacheContainer endpoint queue completionHandler =
  do
    cls' <- getRequiredClass "MTRBaseClusterApplicationBasic"
    sendClassMessage cls' readAttributeApplicationNameWithAttributeCache_endpoint_queue_completionHandlerSelector (toMTRAttributeCacheContainer attributeCacheContainer) (toNSNumber endpoint) (toNSObject queue) completionHandler

-- | @- readAttributeProductIDWithCompletionHandler:@
readAttributeProductIDWithCompletionHandler :: IsMTRBaseClusterApplicationBasic mtrBaseClusterApplicationBasic => mtrBaseClusterApplicationBasic -> Ptr () -> IO ()
readAttributeProductIDWithCompletionHandler mtrBaseClusterApplicationBasic completionHandler =
  sendMessage mtrBaseClusterApplicationBasic readAttributeProductIDWithCompletionHandlerSelector completionHandler

-- | @- subscribeAttributeProductIDWithMinInterval:maxInterval:params:subscriptionEstablished:reportHandler:@
subscribeAttributeProductIDWithMinInterval_maxInterval_params_subscriptionEstablished_reportHandler :: (IsMTRBaseClusterApplicationBasic mtrBaseClusterApplicationBasic, IsNSNumber minInterval, IsNSNumber maxInterval, IsMTRSubscribeParams params) => mtrBaseClusterApplicationBasic -> minInterval -> maxInterval -> params -> Ptr () -> Ptr () -> IO ()
subscribeAttributeProductIDWithMinInterval_maxInterval_params_subscriptionEstablished_reportHandler mtrBaseClusterApplicationBasic minInterval maxInterval params subscriptionEstablishedHandler reportHandler =
  sendMessage mtrBaseClusterApplicationBasic subscribeAttributeProductIDWithMinInterval_maxInterval_params_subscriptionEstablished_reportHandlerSelector (toNSNumber minInterval) (toNSNumber maxInterval) (toMTRSubscribeParams params) subscriptionEstablishedHandler reportHandler

-- | @+ readAttributeProductIDWithAttributeCache:endpoint:queue:completionHandler:@
readAttributeProductIDWithAttributeCache_endpoint_queue_completionHandler :: (IsMTRAttributeCacheContainer attributeCacheContainer, IsNSNumber endpoint, IsNSObject queue) => attributeCacheContainer -> endpoint -> queue -> Ptr () -> IO ()
readAttributeProductIDWithAttributeCache_endpoint_queue_completionHandler attributeCacheContainer endpoint queue completionHandler =
  do
    cls' <- getRequiredClass "MTRBaseClusterApplicationBasic"
    sendClassMessage cls' readAttributeProductIDWithAttributeCache_endpoint_queue_completionHandlerSelector (toMTRAttributeCacheContainer attributeCacheContainer) (toNSNumber endpoint) (toNSObject queue) completionHandler

-- | @- readAttributeApplicationWithCompletionHandler:@
readAttributeApplicationWithCompletionHandler :: IsMTRBaseClusterApplicationBasic mtrBaseClusterApplicationBasic => mtrBaseClusterApplicationBasic -> Ptr () -> IO ()
readAttributeApplicationWithCompletionHandler mtrBaseClusterApplicationBasic completionHandler =
  sendMessage mtrBaseClusterApplicationBasic readAttributeApplicationWithCompletionHandlerSelector completionHandler

-- | @- subscribeAttributeApplicationWithMinInterval:maxInterval:params:subscriptionEstablished:reportHandler:@
subscribeAttributeApplicationWithMinInterval_maxInterval_params_subscriptionEstablished_reportHandler :: (IsMTRBaseClusterApplicationBasic mtrBaseClusterApplicationBasic, IsNSNumber minInterval, IsNSNumber maxInterval, IsMTRSubscribeParams params) => mtrBaseClusterApplicationBasic -> minInterval -> maxInterval -> params -> Ptr () -> Ptr () -> IO ()
subscribeAttributeApplicationWithMinInterval_maxInterval_params_subscriptionEstablished_reportHandler mtrBaseClusterApplicationBasic minInterval maxInterval params subscriptionEstablishedHandler reportHandler =
  sendMessage mtrBaseClusterApplicationBasic subscribeAttributeApplicationWithMinInterval_maxInterval_params_subscriptionEstablished_reportHandlerSelector (toNSNumber minInterval) (toNSNumber maxInterval) (toMTRSubscribeParams params) subscriptionEstablishedHandler reportHandler

-- | @+ readAttributeApplicationWithAttributeCache:endpoint:queue:completionHandler:@
readAttributeApplicationWithAttributeCache_endpoint_queue_completionHandler :: (IsMTRAttributeCacheContainer attributeCacheContainer, IsNSNumber endpoint, IsNSObject queue) => attributeCacheContainer -> endpoint -> queue -> Ptr () -> IO ()
readAttributeApplicationWithAttributeCache_endpoint_queue_completionHandler attributeCacheContainer endpoint queue completionHandler =
  do
    cls' <- getRequiredClass "MTRBaseClusterApplicationBasic"
    sendClassMessage cls' readAttributeApplicationWithAttributeCache_endpoint_queue_completionHandlerSelector (toMTRAttributeCacheContainer attributeCacheContainer) (toNSNumber endpoint) (toNSObject queue) completionHandler

-- | @- readAttributeStatusWithCompletionHandler:@
readAttributeStatusWithCompletionHandler :: IsMTRBaseClusterApplicationBasic mtrBaseClusterApplicationBasic => mtrBaseClusterApplicationBasic -> Ptr () -> IO ()
readAttributeStatusWithCompletionHandler mtrBaseClusterApplicationBasic completionHandler =
  sendMessage mtrBaseClusterApplicationBasic readAttributeStatusWithCompletionHandlerSelector completionHandler

-- | @- subscribeAttributeStatusWithMinInterval:maxInterval:params:subscriptionEstablished:reportHandler:@
subscribeAttributeStatusWithMinInterval_maxInterval_params_subscriptionEstablished_reportHandler :: (IsMTRBaseClusterApplicationBasic mtrBaseClusterApplicationBasic, IsNSNumber minInterval, IsNSNumber maxInterval, IsMTRSubscribeParams params) => mtrBaseClusterApplicationBasic -> minInterval -> maxInterval -> params -> Ptr () -> Ptr () -> IO ()
subscribeAttributeStatusWithMinInterval_maxInterval_params_subscriptionEstablished_reportHandler mtrBaseClusterApplicationBasic minInterval maxInterval params subscriptionEstablishedHandler reportHandler =
  sendMessage mtrBaseClusterApplicationBasic subscribeAttributeStatusWithMinInterval_maxInterval_params_subscriptionEstablished_reportHandlerSelector (toNSNumber minInterval) (toNSNumber maxInterval) (toMTRSubscribeParams params) subscriptionEstablishedHandler reportHandler

-- | @+ readAttributeStatusWithAttributeCache:endpoint:queue:completionHandler:@
readAttributeStatusWithAttributeCache_endpoint_queue_completionHandler :: (IsMTRAttributeCacheContainer attributeCacheContainer, IsNSNumber endpoint, IsNSObject queue) => attributeCacheContainer -> endpoint -> queue -> Ptr () -> IO ()
readAttributeStatusWithAttributeCache_endpoint_queue_completionHandler attributeCacheContainer endpoint queue completionHandler =
  do
    cls' <- getRequiredClass "MTRBaseClusterApplicationBasic"
    sendClassMessage cls' readAttributeStatusWithAttributeCache_endpoint_queue_completionHandlerSelector (toMTRAttributeCacheContainer attributeCacheContainer) (toNSNumber endpoint) (toNSObject queue) completionHandler

-- | @- readAttributeApplicationVersionWithCompletionHandler:@
readAttributeApplicationVersionWithCompletionHandler :: IsMTRBaseClusterApplicationBasic mtrBaseClusterApplicationBasic => mtrBaseClusterApplicationBasic -> Ptr () -> IO ()
readAttributeApplicationVersionWithCompletionHandler mtrBaseClusterApplicationBasic completionHandler =
  sendMessage mtrBaseClusterApplicationBasic readAttributeApplicationVersionWithCompletionHandlerSelector completionHandler

-- | @- subscribeAttributeApplicationVersionWithMinInterval:maxInterval:params:subscriptionEstablished:reportHandler:@
subscribeAttributeApplicationVersionWithMinInterval_maxInterval_params_subscriptionEstablished_reportHandler :: (IsMTRBaseClusterApplicationBasic mtrBaseClusterApplicationBasic, IsNSNumber minInterval, IsNSNumber maxInterval, IsMTRSubscribeParams params) => mtrBaseClusterApplicationBasic -> minInterval -> maxInterval -> params -> Ptr () -> Ptr () -> IO ()
subscribeAttributeApplicationVersionWithMinInterval_maxInterval_params_subscriptionEstablished_reportHandler mtrBaseClusterApplicationBasic minInterval maxInterval params subscriptionEstablishedHandler reportHandler =
  sendMessage mtrBaseClusterApplicationBasic subscribeAttributeApplicationVersionWithMinInterval_maxInterval_params_subscriptionEstablished_reportHandlerSelector (toNSNumber minInterval) (toNSNumber maxInterval) (toMTRSubscribeParams params) subscriptionEstablishedHandler reportHandler

-- | @+ readAttributeApplicationVersionWithAttributeCache:endpoint:queue:completionHandler:@
readAttributeApplicationVersionWithAttributeCache_endpoint_queue_completionHandler :: (IsMTRAttributeCacheContainer attributeCacheContainer, IsNSNumber endpoint, IsNSObject queue) => attributeCacheContainer -> endpoint -> queue -> Ptr () -> IO ()
readAttributeApplicationVersionWithAttributeCache_endpoint_queue_completionHandler attributeCacheContainer endpoint queue completionHandler =
  do
    cls' <- getRequiredClass "MTRBaseClusterApplicationBasic"
    sendClassMessage cls' readAttributeApplicationVersionWithAttributeCache_endpoint_queue_completionHandlerSelector (toMTRAttributeCacheContainer attributeCacheContainer) (toNSNumber endpoint) (toNSObject queue) completionHandler

-- | @- readAttributeAllowedVendorListWithCompletionHandler:@
readAttributeAllowedVendorListWithCompletionHandler :: IsMTRBaseClusterApplicationBasic mtrBaseClusterApplicationBasic => mtrBaseClusterApplicationBasic -> Ptr () -> IO ()
readAttributeAllowedVendorListWithCompletionHandler mtrBaseClusterApplicationBasic completionHandler =
  sendMessage mtrBaseClusterApplicationBasic readAttributeAllowedVendorListWithCompletionHandlerSelector completionHandler

-- | @- subscribeAttributeAllowedVendorListWithMinInterval:maxInterval:params:subscriptionEstablished:reportHandler:@
subscribeAttributeAllowedVendorListWithMinInterval_maxInterval_params_subscriptionEstablished_reportHandler :: (IsMTRBaseClusterApplicationBasic mtrBaseClusterApplicationBasic, IsNSNumber minInterval, IsNSNumber maxInterval, IsMTRSubscribeParams params) => mtrBaseClusterApplicationBasic -> minInterval -> maxInterval -> params -> Ptr () -> Ptr () -> IO ()
subscribeAttributeAllowedVendorListWithMinInterval_maxInterval_params_subscriptionEstablished_reportHandler mtrBaseClusterApplicationBasic minInterval maxInterval params subscriptionEstablishedHandler reportHandler =
  sendMessage mtrBaseClusterApplicationBasic subscribeAttributeAllowedVendorListWithMinInterval_maxInterval_params_subscriptionEstablished_reportHandlerSelector (toNSNumber minInterval) (toNSNumber maxInterval) (toMTRSubscribeParams params) subscriptionEstablishedHandler reportHandler

-- | @+ readAttributeAllowedVendorListWithAttributeCache:endpoint:queue:completionHandler:@
readAttributeAllowedVendorListWithAttributeCache_endpoint_queue_completionHandler :: (IsMTRAttributeCacheContainer attributeCacheContainer, IsNSNumber endpoint, IsNSObject queue) => attributeCacheContainer -> endpoint -> queue -> Ptr () -> IO ()
readAttributeAllowedVendorListWithAttributeCache_endpoint_queue_completionHandler attributeCacheContainer endpoint queue completionHandler =
  do
    cls' <- getRequiredClass "MTRBaseClusterApplicationBasic"
    sendClassMessage cls' readAttributeAllowedVendorListWithAttributeCache_endpoint_queue_completionHandlerSelector (toMTRAttributeCacheContainer attributeCacheContainer) (toNSNumber endpoint) (toNSObject queue) completionHandler

-- | @- readAttributeGeneratedCommandListWithCompletionHandler:@
readAttributeGeneratedCommandListWithCompletionHandler :: IsMTRBaseClusterApplicationBasic mtrBaseClusterApplicationBasic => mtrBaseClusterApplicationBasic -> Ptr () -> IO ()
readAttributeGeneratedCommandListWithCompletionHandler mtrBaseClusterApplicationBasic completionHandler =
  sendMessage mtrBaseClusterApplicationBasic readAttributeGeneratedCommandListWithCompletionHandlerSelector completionHandler

-- | @- subscribeAttributeGeneratedCommandListWithMinInterval:maxInterval:params:subscriptionEstablished:reportHandler:@
subscribeAttributeGeneratedCommandListWithMinInterval_maxInterval_params_subscriptionEstablished_reportHandler :: (IsMTRBaseClusterApplicationBasic mtrBaseClusterApplicationBasic, IsNSNumber minInterval, IsNSNumber maxInterval, IsMTRSubscribeParams params) => mtrBaseClusterApplicationBasic -> minInterval -> maxInterval -> params -> Ptr () -> Ptr () -> IO ()
subscribeAttributeGeneratedCommandListWithMinInterval_maxInterval_params_subscriptionEstablished_reportHandler mtrBaseClusterApplicationBasic minInterval maxInterval params subscriptionEstablishedHandler reportHandler =
  sendMessage mtrBaseClusterApplicationBasic subscribeAttributeGeneratedCommandListWithMinInterval_maxInterval_params_subscriptionEstablished_reportHandlerSelector (toNSNumber minInterval) (toNSNumber maxInterval) (toMTRSubscribeParams params) subscriptionEstablishedHandler reportHandler

-- | @+ readAttributeGeneratedCommandListWithAttributeCache:endpoint:queue:completionHandler:@
readAttributeGeneratedCommandListWithAttributeCache_endpoint_queue_completionHandler :: (IsMTRAttributeCacheContainer attributeCacheContainer, IsNSNumber endpoint, IsNSObject queue) => attributeCacheContainer -> endpoint -> queue -> Ptr () -> IO ()
readAttributeGeneratedCommandListWithAttributeCache_endpoint_queue_completionHandler attributeCacheContainer endpoint queue completionHandler =
  do
    cls' <- getRequiredClass "MTRBaseClusterApplicationBasic"
    sendClassMessage cls' readAttributeGeneratedCommandListWithAttributeCache_endpoint_queue_completionHandlerSelector (toMTRAttributeCacheContainer attributeCacheContainer) (toNSNumber endpoint) (toNSObject queue) completionHandler

-- | @- readAttributeAcceptedCommandListWithCompletionHandler:@
readAttributeAcceptedCommandListWithCompletionHandler :: IsMTRBaseClusterApplicationBasic mtrBaseClusterApplicationBasic => mtrBaseClusterApplicationBasic -> Ptr () -> IO ()
readAttributeAcceptedCommandListWithCompletionHandler mtrBaseClusterApplicationBasic completionHandler =
  sendMessage mtrBaseClusterApplicationBasic readAttributeAcceptedCommandListWithCompletionHandlerSelector completionHandler

-- | @- subscribeAttributeAcceptedCommandListWithMinInterval:maxInterval:params:subscriptionEstablished:reportHandler:@
subscribeAttributeAcceptedCommandListWithMinInterval_maxInterval_params_subscriptionEstablished_reportHandler :: (IsMTRBaseClusterApplicationBasic mtrBaseClusterApplicationBasic, IsNSNumber minInterval, IsNSNumber maxInterval, IsMTRSubscribeParams params) => mtrBaseClusterApplicationBasic -> minInterval -> maxInterval -> params -> Ptr () -> Ptr () -> IO ()
subscribeAttributeAcceptedCommandListWithMinInterval_maxInterval_params_subscriptionEstablished_reportHandler mtrBaseClusterApplicationBasic minInterval maxInterval params subscriptionEstablishedHandler reportHandler =
  sendMessage mtrBaseClusterApplicationBasic subscribeAttributeAcceptedCommandListWithMinInterval_maxInterval_params_subscriptionEstablished_reportHandlerSelector (toNSNumber minInterval) (toNSNumber maxInterval) (toMTRSubscribeParams params) subscriptionEstablishedHandler reportHandler

-- | @+ readAttributeAcceptedCommandListWithAttributeCache:endpoint:queue:completionHandler:@
readAttributeAcceptedCommandListWithAttributeCache_endpoint_queue_completionHandler :: (IsMTRAttributeCacheContainer attributeCacheContainer, IsNSNumber endpoint, IsNSObject queue) => attributeCacheContainer -> endpoint -> queue -> Ptr () -> IO ()
readAttributeAcceptedCommandListWithAttributeCache_endpoint_queue_completionHandler attributeCacheContainer endpoint queue completionHandler =
  do
    cls' <- getRequiredClass "MTRBaseClusterApplicationBasic"
    sendClassMessage cls' readAttributeAcceptedCommandListWithAttributeCache_endpoint_queue_completionHandlerSelector (toMTRAttributeCacheContainer attributeCacheContainer) (toNSNumber endpoint) (toNSObject queue) completionHandler

-- | @- readAttributeAttributeListWithCompletionHandler:@
readAttributeAttributeListWithCompletionHandler :: IsMTRBaseClusterApplicationBasic mtrBaseClusterApplicationBasic => mtrBaseClusterApplicationBasic -> Ptr () -> IO ()
readAttributeAttributeListWithCompletionHandler mtrBaseClusterApplicationBasic completionHandler =
  sendMessage mtrBaseClusterApplicationBasic readAttributeAttributeListWithCompletionHandlerSelector completionHandler

-- | @- subscribeAttributeAttributeListWithMinInterval:maxInterval:params:subscriptionEstablished:reportHandler:@
subscribeAttributeAttributeListWithMinInterval_maxInterval_params_subscriptionEstablished_reportHandler :: (IsMTRBaseClusterApplicationBasic mtrBaseClusterApplicationBasic, IsNSNumber minInterval, IsNSNumber maxInterval, IsMTRSubscribeParams params) => mtrBaseClusterApplicationBasic -> minInterval -> maxInterval -> params -> Ptr () -> Ptr () -> IO ()
subscribeAttributeAttributeListWithMinInterval_maxInterval_params_subscriptionEstablished_reportHandler mtrBaseClusterApplicationBasic minInterval maxInterval params subscriptionEstablishedHandler reportHandler =
  sendMessage mtrBaseClusterApplicationBasic subscribeAttributeAttributeListWithMinInterval_maxInterval_params_subscriptionEstablished_reportHandlerSelector (toNSNumber minInterval) (toNSNumber maxInterval) (toMTRSubscribeParams params) subscriptionEstablishedHandler reportHandler

-- | @+ readAttributeAttributeListWithAttributeCache:endpoint:queue:completionHandler:@
readAttributeAttributeListWithAttributeCache_endpoint_queue_completionHandler :: (IsMTRAttributeCacheContainer attributeCacheContainer, IsNSNumber endpoint, IsNSObject queue) => attributeCacheContainer -> endpoint -> queue -> Ptr () -> IO ()
readAttributeAttributeListWithAttributeCache_endpoint_queue_completionHandler attributeCacheContainer endpoint queue completionHandler =
  do
    cls' <- getRequiredClass "MTRBaseClusterApplicationBasic"
    sendClassMessage cls' readAttributeAttributeListWithAttributeCache_endpoint_queue_completionHandlerSelector (toMTRAttributeCacheContainer attributeCacheContainer) (toNSNumber endpoint) (toNSObject queue) completionHandler

-- | @- readAttributeFeatureMapWithCompletionHandler:@
readAttributeFeatureMapWithCompletionHandler :: IsMTRBaseClusterApplicationBasic mtrBaseClusterApplicationBasic => mtrBaseClusterApplicationBasic -> Ptr () -> IO ()
readAttributeFeatureMapWithCompletionHandler mtrBaseClusterApplicationBasic completionHandler =
  sendMessage mtrBaseClusterApplicationBasic readAttributeFeatureMapWithCompletionHandlerSelector completionHandler

-- | @- subscribeAttributeFeatureMapWithMinInterval:maxInterval:params:subscriptionEstablished:reportHandler:@
subscribeAttributeFeatureMapWithMinInterval_maxInterval_params_subscriptionEstablished_reportHandler :: (IsMTRBaseClusterApplicationBasic mtrBaseClusterApplicationBasic, IsNSNumber minInterval, IsNSNumber maxInterval, IsMTRSubscribeParams params) => mtrBaseClusterApplicationBasic -> minInterval -> maxInterval -> params -> Ptr () -> Ptr () -> IO ()
subscribeAttributeFeatureMapWithMinInterval_maxInterval_params_subscriptionEstablished_reportHandler mtrBaseClusterApplicationBasic minInterval maxInterval params subscriptionEstablishedHandler reportHandler =
  sendMessage mtrBaseClusterApplicationBasic subscribeAttributeFeatureMapWithMinInterval_maxInterval_params_subscriptionEstablished_reportHandlerSelector (toNSNumber minInterval) (toNSNumber maxInterval) (toMTRSubscribeParams params) subscriptionEstablishedHandler reportHandler

-- | @+ readAttributeFeatureMapWithAttributeCache:endpoint:queue:completionHandler:@
readAttributeFeatureMapWithAttributeCache_endpoint_queue_completionHandler :: (IsMTRAttributeCacheContainer attributeCacheContainer, IsNSNumber endpoint, IsNSObject queue) => attributeCacheContainer -> endpoint -> queue -> Ptr () -> IO ()
readAttributeFeatureMapWithAttributeCache_endpoint_queue_completionHandler attributeCacheContainer endpoint queue completionHandler =
  do
    cls' <- getRequiredClass "MTRBaseClusterApplicationBasic"
    sendClassMessage cls' readAttributeFeatureMapWithAttributeCache_endpoint_queue_completionHandlerSelector (toMTRAttributeCacheContainer attributeCacheContainer) (toNSNumber endpoint) (toNSObject queue) completionHandler

-- | @- readAttributeClusterRevisionWithCompletionHandler:@
readAttributeClusterRevisionWithCompletionHandler :: IsMTRBaseClusterApplicationBasic mtrBaseClusterApplicationBasic => mtrBaseClusterApplicationBasic -> Ptr () -> IO ()
readAttributeClusterRevisionWithCompletionHandler mtrBaseClusterApplicationBasic completionHandler =
  sendMessage mtrBaseClusterApplicationBasic readAttributeClusterRevisionWithCompletionHandlerSelector completionHandler

-- | @- subscribeAttributeClusterRevisionWithMinInterval:maxInterval:params:subscriptionEstablished:reportHandler:@
subscribeAttributeClusterRevisionWithMinInterval_maxInterval_params_subscriptionEstablished_reportHandler :: (IsMTRBaseClusterApplicationBasic mtrBaseClusterApplicationBasic, IsNSNumber minInterval, IsNSNumber maxInterval, IsMTRSubscribeParams params) => mtrBaseClusterApplicationBasic -> minInterval -> maxInterval -> params -> Ptr () -> Ptr () -> IO ()
subscribeAttributeClusterRevisionWithMinInterval_maxInterval_params_subscriptionEstablished_reportHandler mtrBaseClusterApplicationBasic minInterval maxInterval params subscriptionEstablishedHandler reportHandler =
  sendMessage mtrBaseClusterApplicationBasic subscribeAttributeClusterRevisionWithMinInterval_maxInterval_params_subscriptionEstablished_reportHandlerSelector (toNSNumber minInterval) (toNSNumber maxInterval) (toMTRSubscribeParams params) subscriptionEstablishedHandler reportHandler

-- | @+ readAttributeClusterRevisionWithAttributeCache:endpoint:queue:completionHandler:@
readAttributeClusterRevisionWithAttributeCache_endpoint_queue_completionHandler :: (IsMTRAttributeCacheContainer attributeCacheContainer, IsNSNumber endpoint, IsNSObject queue) => attributeCacheContainer -> endpoint -> queue -> Ptr () -> IO ()
readAttributeClusterRevisionWithAttributeCache_endpoint_queue_completionHandler attributeCacheContainer endpoint queue completionHandler =
  do
    cls' <- getRequiredClass "MTRBaseClusterApplicationBasic"
    sendClassMessage cls' readAttributeClusterRevisionWithAttributeCache_endpoint_queue_completionHandlerSelector (toMTRAttributeCacheContainer attributeCacheContainer) (toNSNumber endpoint) (toNSObject queue) completionHandler

-- | For all instance methods (reads, writes, commands) that take a completion, the completion will be called on the provided queue.
--
-- ObjC selector: @- initWithDevice:endpointID:queue:@
initWithDevice_endpointID_queue :: (IsMTRBaseClusterApplicationBasic mtrBaseClusterApplicationBasic, IsMTRBaseDevice device, IsNSNumber endpointID, IsNSObject queue) => mtrBaseClusterApplicationBasic -> device -> endpointID -> queue -> IO (Id MTRBaseClusterApplicationBasic)
initWithDevice_endpointID_queue mtrBaseClusterApplicationBasic device endpointID queue =
  sendOwnedMessage mtrBaseClusterApplicationBasic initWithDevice_endpointID_queueSelector (toMTRBaseDevice device) (toNSNumber endpointID) (toNSObject queue)

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @readAttributeVendorNameWithCompletion:@
readAttributeVendorNameWithCompletionSelector :: Selector '[Ptr ()] ()
readAttributeVendorNameWithCompletionSelector = mkSelector "readAttributeVendorNameWithCompletion:"

-- | @Selector@ for @subscribeAttributeVendorNameWithParams:subscriptionEstablished:reportHandler:@
subscribeAttributeVendorNameWithParams_subscriptionEstablished_reportHandlerSelector :: Selector '[Id MTRSubscribeParams, Ptr (), Ptr ()] ()
subscribeAttributeVendorNameWithParams_subscriptionEstablished_reportHandlerSelector = mkSelector "subscribeAttributeVendorNameWithParams:subscriptionEstablished:reportHandler:"

-- | @Selector@ for @readAttributeVendorNameWithClusterStateCache:endpoint:queue:completion:@
readAttributeVendorNameWithClusterStateCache_endpoint_queue_completionSelector :: Selector '[Id MTRClusterStateCacheContainer, Id NSNumber, Id NSObject, Ptr ()] ()
readAttributeVendorNameWithClusterStateCache_endpoint_queue_completionSelector = mkSelector "readAttributeVendorNameWithClusterStateCache:endpoint:queue:completion:"

-- | @Selector@ for @readAttributeVendorIDWithCompletion:@
readAttributeVendorIDWithCompletionSelector :: Selector '[Ptr ()] ()
readAttributeVendorIDWithCompletionSelector = mkSelector "readAttributeVendorIDWithCompletion:"

-- | @Selector@ for @subscribeAttributeVendorIDWithParams:subscriptionEstablished:reportHandler:@
subscribeAttributeVendorIDWithParams_subscriptionEstablished_reportHandlerSelector :: Selector '[Id MTRSubscribeParams, Ptr (), Ptr ()] ()
subscribeAttributeVendorIDWithParams_subscriptionEstablished_reportHandlerSelector = mkSelector "subscribeAttributeVendorIDWithParams:subscriptionEstablished:reportHandler:"

-- | @Selector@ for @readAttributeVendorIDWithClusterStateCache:endpoint:queue:completion:@
readAttributeVendorIDWithClusterStateCache_endpoint_queue_completionSelector :: Selector '[Id MTRClusterStateCacheContainer, Id NSNumber, Id NSObject, Ptr ()] ()
readAttributeVendorIDWithClusterStateCache_endpoint_queue_completionSelector = mkSelector "readAttributeVendorIDWithClusterStateCache:endpoint:queue:completion:"

-- | @Selector@ for @readAttributeApplicationNameWithCompletion:@
readAttributeApplicationNameWithCompletionSelector :: Selector '[Ptr ()] ()
readAttributeApplicationNameWithCompletionSelector = mkSelector "readAttributeApplicationNameWithCompletion:"

-- | @Selector@ for @subscribeAttributeApplicationNameWithParams:subscriptionEstablished:reportHandler:@
subscribeAttributeApplicationNameWithParams_subscriptionEstablished_reportHandlerSelector :: Selector '[Id MTRSubscribeParams, Ptr (), Ptr ()] ()
subscribeAttributeApplicationNameWithParams_subscriptionEstablished_reportHandlerSelector = mkSelector "subscribeAttributeApplicationNameWithParams:subscriptionEstablished:reportHandler:"

-- | @Selector@ for @readAttributeApplicationNameWithClusterStateCache:endpoint:queue:completion:@
readAttributeApplicationNameWithClusterStateCache_endpoint_queue_completionSelector :: Selector '[Id MTRClusterStateCacheContainer, Id NSNumber, Id NSObject, Ptr ()] ()
readAttributeApplicationNameWithClusterStateCache_endpoint_queue_completionSelector = mkSelector "readAttributeApplicationNameWithClusterStateCache:endpoint:queue:completion:"

-- | @Selector@ for @readAttributeProductIDWithCompletion:@
readAttributeProductIDWithCompletionSelector :: Selector '[Ptr ()] ()
readAttributeProductIDWithCompletionSelector = mkSelector "readAttributeProductIDWithCompletion:"

-- | @Selector@ for @subscribeAttributeProductIDWithParams:subscriptionEstablished:reportHandler:@
subscribeAttributeProductIDWithParams_subscriptionEstablished_reportHandlerSelector :: Selector '[Id MTRSubscribeParams, Ptr (), Ptr ()] ()
subscribeAttributeProductIDWithParams_subscriptionEstablished_reportHandlerSelector = mkSelector "subscribeAttributeProductIDWithParams:subscriptionEstablished:reportHandler:"

-- | @Selector@ for @readAttributeProductIDWithClusterStateCache:endpoint:queue:completion:@
readAttributeProductIDWithClusterStateCache_endpoint_queue_completionSelector :: Selector '[Id MTRClusterStateCacheContainer, Id NSNumber, Id NSObject, Ptr ()] ()
readAttributeProductIDWithClusterStateCache_endpoint_queue_completionSelector = mkSelector "readAttributeProductIDWithClusterStateCache:endpoint:queue:completion:"

-- | @Selector@ for @readAttributeApplicationWithCompletion:@
readAttributeApplicationWithCompletionSelector :: Selector '[Ptr ()] ()
readAttributeApplicationWithCompletionSelector = mkSelector "readAttributeApplicationWithCompletion:"

-- | @Selector@ for @subscribeAttributeApplicationWithParams:subscriptionEstablished:reportHandler:@
subscribeAttributeApplicationWithParams_subscriptionEstablished_reportHandlerSelector :: Selector '[Id MTRSubscribeParams, Ptr (), Ptr ()] ()
subscribeAttributeApplicationWithParams_subscriptionEstablished_reportHandlerSelector = mkSelector "subscribeAttributeApplicationWithParams:subscriptionEstablished:reportHandler:"

-- | @Selector@ for @readAttributeApplicationWithClusterStateCache:endpoint:queue:completion:@
readAttributeApplicationWithClusterStateCache_endpoint_queue_completionSelector :: Selector '[Id MTRClusterStateCacheContainer, Id NSNumber, Id NSObject, Ptr ()] ()
readAttributeApplicationWithClusterStateCache_endpoint_queue_completionSelector = mkSelector "readAttributeApplicationWithClusterStateCache:endpoint:queue:completion:"

-- | @Selector@ for @readAttributeStatusWithCompletion:@
readAttributeStatusWithCompletionSelector :: Selector '[Ptr ()] ()
readAttributeStatusWithCompletionSelector = mkSelector "readAttributeStatusWithCompletion:"

-- | @Selector@ for @subscribeAttributeStatusWithParams:subscriptionEstablished:reportHandler:@
subscribeAttributeStatusWithParams_subscriptionEstablished_reportHandlerSelector :: Selector '[Id MTRSubscribeParams, Ptr (), Ptr ()] ()
subscribeAttributeStatusWithParams_subscriptionEstablished_reportHandlerSelector = mkSelector "subscribeAttributeStatusWithParams:subscriptionEstablished:reportHandler:"

-- | @Selector@ for @readAttributeStatusWithClusterStateCache:endpoint:queue:completion:@
readAttributeStatusWithClusterStateCache_endpoint_queue_completionSelector :: Selector '[Id MTRClusterStateCacheContainer, Id NSNumber, Id NSObject, Ptr ()] ()
readAttributeStatusWithClusterStateCache_endpoint_queue_completionSelector = mkSelector "readAttributeStatusWithClusterStateCache:endpoint:queue:completion:"

-- | @Selector@ for @readAttributeApplicationVersionWithCompletion:@
readAttributeApplicationVersionWithCompletionSelector :: Selector '[Ptr ()] ()
readAttributeApplicationVersionWithCompletionSelector = mkSelector "readAttributeApplicationVersionWithCompletion:"

-- | @Selector@ for @subscribeAttributeApplicationVersionWithParams:subscriptionEstablished:reportHandler:@
subscribeAttributeApplicationVersionWithParams_subscriptionEstablished_reportHandlerSelector :: Selector '[Id MTRSubscribeParams, Ptr (), Ptr ()] ()
subscribeAttributeApplicationVersionWithParams_subscriptionEstablished_reportHandlerSelector = mkSelector "subscribeAttributeApplicationVersionWithParams:subscriptionEstablished:reportHandler:"

-- | @Selector@ for @readAttributeApplicationVersionWithClusterStateCache:endpoint:queue:completion:@
readAttributeApplicationVersionWithClusterStateCache_endpoint_queue_completionSelector :: Selector '[Id MTRClusterStateCacheContainer, Id NSNumber, Id NSObject, Ptr ()] ()
readAttributeApplicationVersionWithClusterStateCache_endpoint_queue_completionSelector = mkSelector "readAttributeApplicationVersionWithClusterStateCache:endpoint:queue:completion:"

-- | @Selector@ for @readAttributeAllowedVendorListWithCompletion:@
readAttributeAllowedVendorListWithCompletionSelector :: Selector '[Ptr ()] ()
readAttributeAllowedVendorListWithCompletionSelector = mkSelector "readAttributeAllowedVendorListWithCompletion:"

-- | @Selector@ for @subscribeAttributeAllowedVendorListWithParams:subscriptionEstablished:reportHandler:@
subscribeAttributeAllowedVendorListWithParams_subscriptionEstablished_reportHandlerSelector :: Selector '[Id MTRSubscribeParams, Ptr (), Ptr ()] ()
subscribeAttributeAllowedVendorListWithParams_subscriptionEstablished_reportHandlerSelector = mkSelector "subscribeAttributeAllowedVendorListWithParams:subscriptionEstablished:reportHandler:"

-- | @Selector@ for @readAttributeAllowedVendorListWithClusterStateCache:endpoint:queue:completion:@
readAttributeAllowedVendorListWithClusterStateCache_endpoint_queue_completionSelector :: Selector '[Id MTRClusterStateCacheContainer, Id NSNumber, Id NSObject, Ptr ()] ()
readAttributeAllowedVendorListWithClusterStateCache_endpoint_queue_completionSelector = mkSelector "readAttributeAllowedVendorListWithClusterStateCache:endpoint:queue:completion:"

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
initSelector :: Selector '[] (Id MTRBaseClusterApplicationBasic)
initSelector = mkSelector "init"

-- | @Selector@ for @new@
newSelector :: Selector '[] (Id MTRBaseClusterApplicationBasic)
newSelector = mkSelector "new"

-- | @Selector@ for @initWithDevice:endpoint:queue:@
initWithDevice_endpoint_queueSelector :: Selector '[Id MTRBaseDevice, CUShort, Id NSObject] (Id MTRBaseClusterApplicationBasic)
initWithDevice_endpoint_queueSelector = mkSelector "initWithDevice:endpoint:queue:"

-- | @Selector@ for @readAttributeVendorNameWithCompletionHandler:@
readAttributeVendorNameWithCompletionHandlerSelector :: Selector '[Ptr ()] ()
readAttributeVendorNameWithCompletionHandlerSelector = mkSelector "readAttributeVendorNameWithCompletionHandler:"

-- | @Selector@ for @subscribeAttributeVendorNameWithMinInterval:maxInterval:params:subscriptionEstablished:reportHandler:@
subscribeAttributeVendorNameWithMinInterval_maxInterval_params_subscriptionEstablished_reportHandlerSelector :: Selector '[Id NSNumber, Id NSNumber, Id MTRSubscribeParams, Ptr (), Ptr ()] ()
subscribeAttributeVendorNameWithMinInterval_maxInterval_params_subscriptionEstablished_reportHandlerSelector = mkSelector "subscribeAttributeVendorNameWithMinInterval:maxInterval:params:subscriptionEstablished:reportHandler:"

-- | @Selector@ for @readAttributeVendorNameWithAttributeCache:endpoint:queue:completionHandler:@
readAttributeVendorNameWithAttributeCache_endpoint_queue_completionHandlerSelector :: Selector '[Id MTRAttributeCacheContainer, Id NSNumber, Id NSObject, Ptr ()] ()
readAttributeVendorNameWithAttributeCache_endpoint_queue_completionHandlerSelector = mkSelector "readAttributeVendorNameWithAttributeCache:endpoint:queue:completionHandler:"

-- | @Selector@ for @readAttributeVendorIDWithCompletionHandler:@
readAttributeVendorIDWithCompletionHandlerSelector :: Selector '[Ptr ()] ()
readAttributeVendorIDWithCompletionHandlerSelector = mkSelector "readAttributeVendorIDWithCompletionHandler:"

-- | @Selector@ for @subscribeAttributeVendorIDWithMinInterval:maxInterval:params:subscriptionEstablished:reportHandler:@
subscribeAttributeVendorIDWithMinInterval_maxInterval_params_subscriptionEstablished_reportHandlerSelector :: Selector '[Id NSNumber, Id NSNumber, Id MTRSubscribeParams, Ptr (), Ptr ()] ()
subscribeAttributeVendorIDWithMinInterval_maxInterval_params_subscriptionEstablished_reportHandlerSelector = mkSelector "subscribeAttributeVendorIDWithMinInterval:maxInterval:params:subscriptionEstablished:reportHandler:"

-- | @Selector@ for @readAttributeVendorIDWithAttributeCache:endpoint:queue:completionHandler:@
readAttributeVendorIDWithAttributeCache_endpoint_queue_completionHandlerSelector :: Selector '[Id MTRAttributeCacheContainer, Id NSNumber, Id NSObject, Ptr ()] ()
readAttributeVendorIDWithAttributeCache_endpoint_queue_completionHandlerSelector = mkSelector "readAttributeVendorIDWithAttributeCache:endpoint:queue:completionHandler:"

-- | @Selector@ for @readAttributeApplicationNameWithCompletionHandler:@
readAttributeApplicationNameWithCompletionHandlerSelector :: Selector '[Ptr ()] ()
readAttributeApplicationNameWithCompletionHandlerSelector = mkSelector "readAttributeApplicationNameWithCompletionHandler:"

-- | @Selector@ for @subscribeAttributeApplicationNameWithMinInterval:maxInterval:params:subscriptionEstablished:reportHandler:@
subscribeAttributeApplicationNameWithMinInterval_maxInterval_params_subscriptionEstablished_reportHandlerSelector :: Selector '[Id NSNumber, Id NSNumber, Id MTRSubscribeParams, Ptr (), Ptr ()] ()
subscribeAttributeApplicationNameWithMinInterval_maxInterval_params_subscriptionEstablished_reportHandlerSelector = mkSelector "subscribeAttributeApplicationNameWithMinInterval:maxInterval:params:subscriptionEstablished:reportHandler:"

-- | @Selector@ for @readAttributeApplicationNameWithAttributeCache:endpoint:queue:completionHandler:@
readAttributeApplicationNameWithAttributeCache_endpoint_queue_completionHandlerSelector :: Selector '[Id MTRAttributeCacheContainer, Id NSNumber, Id NSObject, Ptr ()] ()
readAttributeApplicationNameWithAttributeCache_endpoint_queue_completionHandlerSelector = mkSelector "readAttributeApplicationNameWithAttributeCache:endpoint:queue:completionHandler:"

-- | @Selector@ for @readAttributeProductIDWithCompletionHandler:@
readAttributeProductIDWithCompletionHandlerSelector :: Selector '[Ptr ()] ()
readAttributeProductIDWithCompletionHandlerSelector = mkSelector "readAttributeProductIDWithCompletionHandler:"

-- | @Selector@ for @subscribeAttributeProductIDWithMinInterval:maxInterval:params:subscriptionEstablished:reportHandler:@
subscribeAttributeProductIDWithMinInterval_maxInterval_params_subscriptionEstablished_reportHandlerSelector :: Selector '[Id NSNumber, Id NSNumber, Id MTRSubscribeParams, Ptr (), Ptr ()] ()
subscribeAttributeProductIDWithMinInterval_maxInterval_params_subscriptionEstablished_reportHandlerSelector = mkSelector "subscribeAttributeProductIDWithMinInterval:maxInterval:params:subscriptionEstablished:reportHandler:"

-- | @Selector@ for @readAttributeProductIDWithAttributeCache:endpoint:queue:completionHandler:@
readAttributeProductIDWithAttributeCache_endpoint_queue_completionHandlerSelector :: Selector '[Id MTRAttributeCacheContainer, Id NSNumber, Id NSObject, Ptr ()] ()
readAttributeProductIDWithAttributeCache_endpoint_queue_completionHandlerSelector = mkSelector "readAttributeProductIDWithAttributeCache:endpoint:queue:completionHandler:"

-- | @Selector@ for @readAttributeApplicationWithCompletionHandler:@
readAttributeApplicationWithCompletionHandlerSelector :: Selector '[Ptr ()] ()
readAttributeApplicationWithCompletionHandlerSelector = mkSelector "readAttributeApplicationWithCompletionHandler:"

-- | @Selector@ for @subscribeAttributeApplicationWithMinInterval:maxInterval:params:subscriptionEstablished:reportHandler:@
subscribeAttributeApplicationWithMinInterval_maxInterval_params_subscriptionEstablished_reportHandlerSelector :: Selector '[Id NSNumber, Id NSNumber, Id MTRSubscribeParams, Ptr (), Ptr ()] ()
subscribeAttributeApplicationWithMinInterval_maxInterval_params_subscriptionEstablished_reportHandlerSelector = mkSelector "subscribeAttributeApplicationWithMinInterval:maxInterval:params:subscriptionEstablished:reportHandler:"

-- | @Selector@ for @readAttributeApplicationWithAttributeCache:endpoint:queue:completionHandler:@
readAttributeApplicationWithAttributeCache_endpoint_queue_completionHandlerSelector :: Selector '[Id MTRAttributeCacheContainer, Id NSNumber, Id NSObject, Ptr ()] ()
readAttributeApplicationWithAttributeCache_endpoint_queue_completionHandlerSelector = mkSelector "readAttributeApplicationWithAttributeCache:endpoint:queue:completionHandler:"

-- | @Selector@ for @readAttributeStatusWithCompletionHandler:@
readAttributeStatusWithCompletionHandlerSelector :: Selector '[Ptr ()] ()
readAttributeStatusWithCompletionHandlerSelector = mkSelector "readAttributeStatusWithCompletionHandler:"

-- | @Selector@ for @subscribeAttributeStatusWithMinInterval:maxInterval:params:subscriptionEstablished:reportHandler:@
subscribeAttributeStatusWithMinInterval_maxInterval_params_subscriptionEstablished_reportHandlerSelector :: Selector '[Id NSNumber, Id NSNumber, Id MTRSubscribeParams, Ptr (), Ptr ()] ()
subscribeAttributeStatusWithMinInterval_maxInterval_params_subscriptionEstablished_reportHandlerSelector = mkSelector "subscribeAttributeStatusWithMinInterval:maxInterval:params:subscriptionEstablished:reportHandler:"

-- | @Selector@ for @readAttributeStatusWithAttributeCache:endpoint:queue:completionHandler:@
readAttributeStatusWithAttributeCache_endpoint_queue_completionHandlerSelector :: Selector '[Id MTRAttributeCacheContainer, Id NSNumber, Id NSObject, Ptr ()] ()
readAttributeStatusWithAttributeCache_endpoint_queue_completionHandlerSelector = mkSelector "readAttributeStatusWithAttributeCache:endpoint:queue:completionHandler:"

-- | @Selector@ for @readAttributeApplicationVersionWithCompletionHandler:@
readAttributeApplicationVersionWithCompletionHandlerSelector :: Selector '[Ptr ()] ()
readAttributeApplicationVersionWithCompletionHandlerSelector = mkSelector "readAttributeApplicationVersionWithCompletionHandler:"

-- | @Selector@ for @subscribeAttributeApplicationVersionWithMinInterval:maxInterval:params:subscriptionEstablished:reportHandler:@
subscribeAttributeApplicationVersionWithMinInterval_maxInterval_params_subscriptionEstablished_reportHandlerSelector :: Selector '[Id NSNumber, Id NSNumber, Id MTRSubscribeParams, Ptr (), Ptr ()] ()
subscribeAttributeApplicationVersionWithMinInterval_maxInterval_params_subscriptionEstablished_reportHandlerSelector = mkSelector "subscribeAttributeApplicationVersionWithMinInterval:maxInterval:params:subscriptionEstablished:reportHandler:"

-- | @Selector@ for @readAttributeApplicationVersionWithAttributeCache:endpoint:queue:completionHandler:@
readAttributeApplicationVersionWithAttributeCache_endpoint_queue_completionHandlerSelector :: Selector '[Id MTRAttributeCacheContainer, Id NSNumber, Id NSObject, Ptr ()] ()
readAttributeApplicationVersionWithAttributeCache_endpoint_queue_completionHandlerSelector = mkSelector "readAttributeApplicationVersionWithAttributeCache:endpoint:queue:completionHandler:"

-- | @Selector@ for @readAttributeAllowedVendorListWithCompletionHandler:@
readAttributeAllowedVendorListWithCompletionHandlerSelector :: Selector '[Ptr ()] ()
readAttributeAllowedVendorListWithCompletionHandlerSelector = mkSelector "readAttributeAllowedVendorListWithCompletionHandler:"

-- | @Selector@ for @subscribeAttributeAllowedVendorListWithMinInterval:maxInterval:params:subscriptionEstablished:reportHandler:@
subscribeAttributeAllowedVendorListWithMinInterval_maxInterval_params_subscriptionEstablished_reportHandlerSelector :: Selector '[Id NSNumber, Id NSNumber, Id MTRSubscribeParams, Ptr (), Ptr ()] ()
subscribeAttributeAllowedVendorListWithMinInterval_maxInterval_params_subscriptionEstablished_reportHandlerSelector = mkSelector "subscribeAttributeAllowedVendorListWithMinInterval:maxInterval:params:subscriptionEstablished:reportHandler:"

-- | @Selector@ for @readAttributeAllowedVendorListWithAttributeCache:endpoint:queue:completionHandler:@
readAttributeAllowedVendorListWithAttributeCache_endpoint_queue_completionHandlerSelector :: Selector '[Id MTRAttributeCacheContainer, Id NSNumber, Id NSObject, Ptr ()] ()
readAttributeAllowedVendorListWithAttributeCache_endpoint_queue_completionHandlerSelector = mkSelector "readAttributeAllowedVendorListWithAttributeCache:endpoint:queue:completionHandler:"

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
initWithDevice_endpointID_queueSelector :: Selector '[Id MTRBaseDevice, Id NSNumber, Id NSObject] (Id MTRBaseClusterApplicationBasic)
initWithDevice_endpointID_queueSelector = mkSelector "initWithDevice:endpointID:queue:"

