{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Cluster Mode Select
--
-- Attributes and commands for selecting a mode from a list of supported options.
--
-- Generated bindings for @MTRBaseClusterModeSelect@.
module ObjC.Matter.MTRBaseClusterModeSelect
  ( MTRBaseClusterModeSelect
  , IsMTRBaseClusterModeSelect(..)
  , changeToModeWithParams_completion
  , readAttributeDescriptionWithCompletion
  , subscribeAttributeDescriptionWithParams_subscriptionEstablished_reportHandler
  , readAttributeDescriptionWithClusterStateCache_endpoint_queue_completion
  , readAttributeStandardNamespaceWithCompletion
  , subscribeAttributeStandardNamespaceWithParams_subscriptionEstablished_reportHandler
  , readAttributeStandardNamespaceWithClusterStateCache_endpoint_queue_completion
  , readAttributeSupportedModesWithCompletion
  , subscribeAttributeSupportedModesWithParams_subscriptionEstablished_reportHandler
  , readAttributeSupportedModesWithClusterStateCache_endpoint_queue_completion
  , readAttributeCurrentModeWithCompletion
  , subscribeAttributeCurrentModeWithParams_subscriptionEstablished_reportHandler
  , readAttributeCurrentModeWithClusterStateCache_endpoint_queue_completion
  , readAttributeStartUpModeWithCompletion
  , writeAttributeStartUpModeWithValue_completion
  , writeAttributeStartUpModeWithValue_params_completion
  , subscribeAttributeStartUpModeWithParams_subscriptionEstablished_reportHandler
  , readAttributeStartUpModeWithClusterStateCache_endpoint_queue_completion
  , readAttributeOnModeWithCompletion
  , writeAttributeOnModeWithValue_completion
  , writeAttributeOnModeWithValue_params_completion
  , subscribeAttributeOnModeWithParams_subscriptionEstablished_reportHandler
  , readAttributeOnModeWithClusterStateCache_endpoint_queue_completion
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
  , changeToModeWithParams_completionHandler
  , readAttributeDescriptionWithCompletionHandler
  , subscribeAttributeDescriptionWithMinInterval_maxInterval_params_subscriptionEstablished_reportHandler
  , readAttributeDescriptionWithAttributeCache_endpoint_queue_completionHandler
  , readAttributeStandardNamespaceWithCompletionHandler
  , subscribeAttributeStandardNamespaceWithMinInterval_maxInterval_params_subscriptionEstablished_reportHandler
  , readAttributeStandardNamespaceWithAttributeCache_endpoint_queue_completionHandler
  , readAttributeSupportedModesWithCompletionHandler
  , subscribeAttributeSupportedModesWithMinInterval_maxInterval_params_subscriptionEstablished_reportHandler
  , readAttributeSupportedModesWithAttributeCache_endpoint_queue_completionHandler
  , readAttributeCurrentModeWithCompletionHandler
  , subscribeAttributeCurrentModeWithMinInterval_maxInterval_params_subscriptionEstablished_reportHandler
  , readAttributeCurrentModeWithAttributeCache_endpoint_queue_completionHandler
  , readAttributeStartUpModeWithCompletionHandler
  , writeAttributeStartUpModeWithValue_completionHandler
  , writeAttributeStartUpModeWithValue_params_completionHandler
  , subscribeAttributeStartUpModeWithMinInterval_maxInterval_params_subscriptionEstablished_reportHandler
  , readAttributeStartUpModeWithAttributeCache_endpoint_queue_completionHandler
  , readAttributeOnModeWithCompletionHandler
  , writeAttributeOnModeWithValue_completionHandler
  , writeAttributeOnModeWithValue_params_completionHandler
  , subscribeAttributeOnModeWithMinInterval_maxInterval_params_subscriptionEstablished_reportHandler
  , readAttributeOnModeWithAttributeCache_endpoint_queue_completionHandler
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
  , changeToModeWithParams_completionHandlerSelector
  , changeToModeWithParams_completionSelector
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
  , readAttributeCurrentModeWithAttributeCache_endpoint_queue_completionHandlerSelector
  , readAttributeCurrentModeWithClusterStateCache_endpoint_queue_completionSelector
  , readAttributeCurrentModeWithCompletionHandlerSelector
  , readAttributeCurrentModeWithCompletionSelector
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
  , readAttributeOnModeWithAttributeCache_endpoint_queue_completionHandlerSelector
  , readAttributeOnModeWithClusterStateCache_endpoint_queue_completionSelector
  , readAttributeOnModeWithCompletionHandlerSelector
  , readAttributeOnModeWithCompletionSelector
  , readAttributeStandardNamespaceWithAttributeCache_endpoint_queue_completionHandlerSelector
  , readAttributeStandardNamespaceWithClusterStateCache_endpoint_queue_completionSelector
  , readAttributeStandardNamespaceWithCompletionHandlerSelector
  , readAttributeStandardNamespaceWithCompletionSelector
  , readAttributeStartUpModeWithAttributeCache_endpoint_queue_completionHandlerSelector
  , readAttributeStartUpModeWithClusterStateCache_endpoint_queue_completionSelector
  , readAttributeStartUpModeWithCompletionHandlerSelector
  , readAttributeStartUpModeWithCompletionSelector
  , readAttributeSupportedModesWithAttributeCache_endpoint_queue_completionHandlerSelector
  , readAttributeSupportedModesWithClusterStateCache_endpoint_queue_completionSelector
  , readAttributeSupportedModesWithCompletionHandlerSelector
  , readAttributeSupportedModesWithCompletionSelector
  , subscribeAttributeAcceptedCommandListWithMinInterval_maxInterval_params_subscriptionEstablished_reportHandlerSelector
  , subscribeAttributeAcceptedCommandListWithParams_subscriptionEstablished_reportHandlerSelector
  , subscribeAttributeAttributeListWithMinInterval_maxInterval_params_subscriptionEstablished_reportHandlerSelector
  , subscribeAttributeAttributeListWithParams_subscriptionEstablished_reportHandlerSelector
  , subscribeAttributeClusterRevisionWithMinInterval_maxInterval_params_subscriptionEstablished_reportHandlerSelector
  , subscribeAttributeClusterRevisionWithParams_subscriptionEstablished_reportHandlerSelector
  , subscribeAttributeCurrentModeWithMinInterval_maxInterval_params_subscriptionEstablished_reportHandlerSelector
  , subscribeAttributeCurrentModeWithParams_subscriptionEstablished_reportHandlerSelector
  , subscribeAttributeDescriptionWithMinInterval_maxInterval_params_subscriptionEstablished_reportHandlerSelector
  , subscribeAttributeDescriptionWithParams_subscriptionEstablished_reportHandlerSelector
  , subscribeAttributeFeatureMapWithMinInterval_maxInterval_params_subscriptionEstablished_reportHandlerSelector
  , subscribeAttributeFeatureMapWithParams_subscriptionEstablished_reportHandlerSelector
  , subscribeAttributeGeneratedCommandListWithMinInterval_maxInterval_params_subscriptionEstablished_reportHandlerSelector
  , subscribeAttributeGeneratedCommandListWithParams_subscriptionEstablished_reportHandlerSelector
  , subscribeAttributeOnModeWithMinInterval_maxInterval_params_subscriptionEstablished_reportHandlerSelector
  , subscribeAttributeOnModeWithParams_subscriptionEstablished_reportHandlerSelector
  , subscribeAttributeStandardNamespaceWithMinInterval_maxInterval_params_subscriptionEstablished_reportHandlerSelector
  , subscribeAttributeStandardNamespaceWithParams_subscriptionEstablished_reportHandlerSelector
  , subscribeAttributeStartUpModeWithMinInterval_maxInterval_params_subscriptionEstablished_reportHandlerSelector
  , subscribeAttributeStartUpModeWithParams_subscriptionEstablished_reportHandlerSelector
  , subscribeAttributeSupportedModesWithMinInterval_maxInterval_params_subscriptionEstablished_reportHandlerSelector
  , subscribeAttributeSupportedModesWithParams_subscriptionEstablished_reportHandlerSelector
  , writeAttributeOnModeWithValue_completionHandlerSelector
  , writeAttributeOnModeWithValue_completionSelector
  , writeAttributeOnModeWithValue_params_completionHandlerSelector
  , writeAttributeOnModeWithValue_params_completionSelector
  , writeAttributeStartUpModeWithValue_completionHandlerSelector
  , writeAttributeStartUpModeWithValue_completionSelector
  , writeAttributeStartUpModeWithValue_params_completionHandlerSelector
  , writeAttributeStartUpModeWithValue_params_completionSelector


  ) where

import Foreign.Ptr (Ptr, FunPtr)
import Foreign.C.Types

import ObjC.Runtime.Types
import ObjC.Runtime.Message (sendMessage, sendOwnedMessage, sendClassMessage, sendOwnedClassMessage)
import ObjC.Runtime.Selector (mkSelector)
import ObjC.Runtime.Class (getRequiredClass)

import ObjC.Matter.Internal.Classes
import ObjC.Foundation.Internal.Classes

-- | Command ChangeToMode
--
-- On receipt of this command, if the NewMode field indicates a valid mode transition within the supported list, the server SHALL set the CurrentMode attribute to the NewMode value, otherwise, the server SHALL respond with an INVALID_COMMAND status response.
--
-- ObjC selector: @- changeToModeWithParams:completion:@
changeToModeWithParams_completion :: (IsMTRBaseClusterModeSelect mtrBaseClusterModeSelect, IsMTRModeSelectClusterChangeToModeParams params) => mtrBaseClusterModeSelect -> params -> Ptr () -> IO ()
changeToModeWithParams_completion mtrBaseClusterModeSelect params completion =
  sendMessage mtrBaseClusterModeSelect changeToModeWithParams_completionSelector (toMTRModeSelectClusterChangeToModeParams params) completion

-- | @- readAttributeDescriptionWithCompletion:@
readAttributeDescriptionWithCompletion :: IsMTRBaseClusterModeSelect mtrBaseClusterModeSelect => mtrBaseClusterModeSelect -> Ptr () -> IO ()
readAttributeDescriptionWithCompletion mtrBaseClusterModeSelect completion =
  sendMessage mtrBaseClusterModeSelect readAttributeDescriptionWithCompletionSelector completion

-- | @- subscribeAttributeDescriptionWithParams:subscriptionEstablished:reportHandler:@
subscribeAttributeDescriptionWithParams_subscriptionEstablished_reportHandler :: (IsMTRBaseClusterModeSelect mtrBaseClusterModeSelect, IsMTRSubscribeParams params) => mtrBaseClusterModeSelect -> params -> Ptr () -> Ptr () -> IO ()
subscribeAttributeDescriptionWithParams_subscriptionEstablished_reportHandler mtrBaseClusterModeSelect params subscriptionEstablished reportHandler =
  sendMessage mtrBaseClusterModeSelect subscribeAttributeDescriptionWithParams_subscriptionEstablished_reportHandlerSelector (toMTRSubscribeParams params) subscriptionEstablished reportHandler

-- | @+ readAttributeDescriptionWithClusterStateCache:endpoint:queue:completion:@
readAttributeDescriptionWithClusterStateCache_endpoint_queue_completion :: (IsMTRClusterStateCacheContainer clusterStateCacheContainer, IsNSNumber endpoint, IsNSObject queue) => clusterStateCacheContainer -> endpoint -> queue -> Ptr () -> IO ()
readAttributeDescriptionWithClusterStateCache_endpoint_queue_completion clusterStateCacheContainer endpoint queue completion =
  do
    cls' <- getRequiredClass "MTRBaseClusterModeSelect"
    sendClassMessage cls' readAttributeDescriptionWithClusterStateCache_endpoint_queue_completionSelector (toMTRClusterStateCacheContainer clusterStateCacheContainer) (toNSNumber endpoint) (toNSObject queue) completion

-- | @- readAttributeStandardNamespaceWithCompletion:@
readAttributeStandardNamespaceWithCompletion :: IsMTRBaseClusterModeSelect mtrBaseClusterModeSelect => mtrBaseClusterModeSelect -> Ptr () -> IO ()
readAttributeStandardNamespaceWithCompletion mtrBaseClusterModeSelect completion =
  sendMessage mtrBaseClusterModeSelect readAttributeStandardNamespaceWithCompletionSelector completion

-- | @- subscribeAttributeStandardNamespaceWithParams:subscriptionEstablished:reportHandler:@
subscribeAttributeStandardNamespaceWithParams_subscriptionEstablished_reportHandler :: (IsMTRBaseClusterModeSelect mtrBaseClusterModeSelect, IsMTRSubscribeParams params) => mtrBaseClusterModeSelect -> params -> Ptr () -> Ptr () -> IO ()
subscribeAttributeStandardNamespaceWithParams_subscriptionEstablished_reportHandler mtrBaseClusterModeSelect params subscriptionEstablished reportHandler =
  sendMessage mtrBaseClusterModeSelect subscribeAttributeStandardNamespaceWithParams_subscriptionEstablished_reportHandlerSelector (toMTRSubscribeParams params) subscriptionEstablished reportHandler

-- | @+ readAttributeStandardNamespaceWithClusterStateCache:endpoint:queue:completion:@
readAttributeStandardNamespaceWithClusterStateCache_endpoint_queue_completion :: (IsMTRClusterStateCacheContainer clusterStateCacheContainer, IsNSNumber endpoint, IsNSObject queue) => clusterStateCacheContainer -> endpoint -> queue -> Ptr () -> IO ()
readAttributeStandardNamespaceWithClusterStateCache_endpoint_queue_completion clusterStateCacheContainer endpoint queue completion =
  do
    cls' <- getRequiredClass "MTRBaseClusterModeSelect"
    sendClassMessage cls' readAttributeStandardNamespaceWithClusterStateCache_endpoint_queue_completionSelector (toMTRClusterStateCacheContainer clusterStateCacheContainer) (toNSNumber endpoint) (toNSObject queue) completion

-- | @- readAttributeSupportedModesWithCompletion:@
readAttributeSupportedModesWithCompletion :: IsMTRBaseClusterModeSelect mtrBaseClusterModeSelect => mtrBaseClusterModeSelect -> Ptr () -> IO ()
readAttributeSupportedModesWithCompletion mtrBaseClusterModeSelect completion =
  sendMessage mtrBaseClusterModeSelect readAttributeSupportedModesWithCompletionSelector completion

-- | @- subscribeAttributeSupportedModesWithParams:subscriptionEstablished:reportHandler:@
subscribeAttributeSupportedModesWithParams_subscriptionEstablished_reportHandler :: (IsMTRBaseClusterModeSelect mtrBaseClusterModeSelect, IsMTRSubscribeParams params) => mtrBaseClusterModeSelect -> params -> Ptr () -> Ptr () -> IO ()
subscribeAttributeSupportedModesWithParams_subscriptionEstablished_reportHandler mtrBaseClusterModeSelect params subscriptionEstablished reportHandler =
  sendMessage mtrBaseClusterModeSelect subscribeAttributeSupportedModesWithParams_subscriptionEstablished_reportHandlerSelector (toMTRSubscribeParams params) subscriptionEstablished reportHandler

-- | @+ readAttributeSupportedModesWithClusterStateCache:endpoint:queue:completion:@
readAttributeSupportedModesWithClusterStateCache_endpoint_queue_completion :: (IsMTRClusterStateCacheContainer clusterStateCacheContainer, IsNSNumber endpoint, IsNSObject queue) => clusterStateCacheContainer -> endpoint -> queue -> Ptr () -> IO ()
readAttributeSupportedModesWithClusterStateCache_endpoint_queue_completion clusterStateCacheContainer endpoint queue completion =
  do
    cls' <- getRequiredClass "MTRBaseClusterModeSelect"
    sendClassMessage cls' readAttributeSupportedModesWithClusterStateCache_endpoint_queue_completionSelector (toMTRClusterStateCacheContainer clusterStateCacheContainer) (toNSNumber endpoint) (toNSObject queue) completion

-- | @- readAttributeCurrentModeWithCompletion:@
readAttributeCurrentModeWithCompletion :: IsMTRBaseClusterModeSelect mtrBaseClusterModeSelect => mtrBaseClusterModeSelect -> Ptr () -> IO ()
readAttributeCurrentModeWithCompletion mtrBaseClusterModeSelect completion =
  sendMessage mtrBaseClusterModeSelect readAttributeCurrentModeWithCompletionSelector completion

-- | @- subscribeAttributeCurrentModeWithParams:subscriptionEstablished:reportHandler:@
subscribeAttributeCurrentModeWithParams_subscriptionEstablished_reportHandler :: (IsMTRBaseClusterModeSelect mtrBaseClusterModeSelect, IsMTRSubscribeParams params) => mtrBaseClusterModeSelect -> params -> Ptr () -> Ptr () -> IO ()
subscribeAttributeCurrentModeWithParams_subscriptionEstablished_reportHandler mtrBaseClusterModeSelect params subscriptionEstablished reportHandler =
  sendMessage mtrBaseClusterModeSelect subscribeAttributeCurrentModeWithParams_subscriptionEstablished_reportHandlerSelector (toMTRSubscribeParams params) subscriptionEstablished reportHandler

-- | @+ readAttributeCurrentModeWithClusterStateCache:endpoint:queue:completion:@
readAttributeCurrentModeWithClusterStateCache_endpoint_queue_completion :: (IsMTRClusterStateCacheContainer clusterStateCacheContainer, IsNSNumber endpoint, IsNSObject queue) => clusterStateCacheContainer -> endpoint -> queue -> Ptr () -> IO ()
readAttributeCurrentModeWithClusterStateCache_endpoint_queue_completion clusterStateCacheContainer endpoint queue completion =
  do
    cls' <- getRequiredClass "MTRBaseClusterModeSelect"
    sendClassMessage cls' readAttributeCurrentModeWithClusterStateCache_endpoint_queue_completionSelector (toMTRClusterStateCacheContainer clusterStateCacheContainer) (toNSNumber endpoint) (toNSObject queue) completion

-- | @- readAttributeStartUpModeWithCompletion:@
readAttributeStartUpModeWithCompletion :: IsMTRBaseClusterModeSelect mtrBaseClusterModeSelect => mtrBaseClusterModeSelect -> Ptr () -> IO ()
readAttributeStartUpModeWithCompletion mtrBaseClusterModeSelect completion =
  sendMessage mtrBaseClusterModeSelect readAttributeStartUpModeWithCompletionSelector completion

-- | @- writeAttributeStartUpModeWithValue:completion:@
writeAttributeStartUpModeWithValue_completion :: (IsMTRBaseClusterModeSelect mtrBaseClusterModeSelect, IsNSNumber value) => mtrBaseClusterModeSelect -> value -> Ptr () -> IO ()
writeAttributeStartUpModeWithValue_completion mtrBaseClusterModeSelect value completion =
  sendMessage mtrBaseClusterModeSelect writeAttributeStartUpModeWithValue_completionSelector (toNSNumber value) completion

-- | @- writeAttributeStartUpModeWithValue:params:completion:@
writeAttributeStartUpModeWithValue_params_completion :: (IsMTRBaseClusterModeSelect mtrBaseClusterModeSelect, IsNSNumber value, IsMTRWriteParams params) => mtrBaseClusterModeSelect -> value -> params -> Ptr () -> IO ()
writeAttributeStartUpModeWithValue_params_completion mtrBaseClusterModeSelect value params completion =
  sendMessage mtrBaseClusterModeSelect writeAttributeStartUpModeWithValue_params_completionSelector (toNSNumber value) (toMTRWriteParams params) completion

-- | @- subscribeAttributeStartUpModeWithParams:subscriptionEstablished:reportHandler:@
subscribeAttributeStartUpModeWithParams_subscriptionEstablished_reportHandler :: (IsMTRBaseClusterModeSelect mtrBaseClusterModeSelect, IsMTRSubscribeParams params) => mtrBaseClusterModeSelect -> params -> Ptr () -> Ptr () -> IO ()
subscribeAttributeStartUpModeWithParams_subscriptionEstablished_reportHandler mtrBaseClusterModeSelect params subscriptionEstablished reportHandler =
  sendMessage mtrBaseClusterModeSelect subscribeAttributeStartUpModeWithParams_subscriptionEstablished_reportHandlerSelector (toMTRSubscribeParams params) subscriptionEstablished reportHandler

-- | @+ readAttributeStartUpModeWithClusterStateCache:endpoint:queue:completion:@
readAttributeStartUpModeWithClusterStateCache_endpoint_queue_completion :: (IsMTRClusterStateCacheContainer clusterStateCacheContainer, IsNSNumber endpoint, IsNSObject queue) => clusterStateCacheContainer -> endpoint -> queue -> Ptr () -> IO ()
readAttributeStartUpModeWithClusterStateCache_endpoint_queue_completion clusterStateCacheContainer endpoint queue completion =
  do
    cls' <- getRequiredClass "MTRBaseClusterModeSelect"
    sendClassMessage cls' readAttributeStartUpModeWithClusterStateCache_endpoint_queue_completionSelector (toMTRClusterStateCacheContainer clusterStateCacheContainer) (toNSNumber endpoint) (toNSObject queue) completion

-- | @- readAttributeOnModeWithCompletion:@
readAttributeOnModeWithCompletion :: IsMTRBaseClusterModeSelect mtrBaseClusterModeSelect => mtrBaseClusterModeSelect -> Ptr () -> IO ()
readAttributeOnModeWithCompletion mtrBaseClusterModeSelect completion =
  sendMessage mtrBaseClusterModeSelect readAttributeOnModeWithCompletionSelector completion

-- | @- writeAttributeOnModeWithValue:completion:@
writeAttributeOnModeWithValue_completion :: (IsMTRBaseClusterModeSelect mtrBaseClusterModeSelect, IsNSNumber value) => mtrBaseClusterModeSelect -> value -> Ptr () -> IO ()
writeAttributeOnModeWithValue_completion mtrBaseClusterModeSelect value completion =
  sendMessage mtrBaseClusterModeSelect writeAttributeOnModeWithValue_completionSelector (toNSNumber value) completion

-- | @- writeAttributeOnModeWithValue:params:completion:@
writeAttributeOnModeWithValue_params_completion :: (IsMTRBaseClusterModeSelect mtrBaseClusterModeSelect, IsNSNumber value, IsMTRWriteParams params) => mtrBaseClusterModeSelect -> value -> params -> Ptr () -> IO ()
writeAttributeOnModeWithValue_params_completion mtrBaseClusterModeSelect value params completion =
  sendMessage mtrBaseClusterModeSelect writeAttributeOnModeWithValue_params_completionSelector (toNSNumber value) (toMTRWriteParams params) completion

-- | @- subscribeAttributeOnModeWithParams:subscriptionEstablished:reportHandler:@
subscribeAttributeOnModeWithParams_subscriptionEstablished_reportHandler :: (IsMTRBaseClusterModeSelect mtrBaseClusterModeSelect, IsMTRSubscribeParams params) => mtrBaseClusterModeSelect -> params -> Ptr () -> Ptr () -> IO ()
subscribeAttributeOnModeWithParams_subscriptionEstablished_reportHandler mtrBaseClusterModeSelect params subscriptionEstablished reportHandler =
  sendMessage mtrBaseClusterModeSelect subscribeAttributeOnModeWithParams_subscriptionEstablished_reportHandlerSelector (toMTRSubscribeParams params) subscriptionEstablished reportHandler

-- | @+ readAttributeOnModeWithClusterStateCache:endpoint:queue:completion:@
readAttributeOnModeWithClusterStateCache_endpoint_queue_completion :: (IsMTRClusterStateCacheContainer clusterStateCacheContainer, IsNSNumber endpoint, IsNSObject queue) => clusterStateCacheContainer -> endpoint -> queue -> Ptr () -> IO ()
readAttributeOnModeWithClusterStateCache_endpoint_queue_completion clusterStateCacheContainer endpoint queue completion =
  do
    cls' <- getRequiredClass "MTRBaseClusterModeSelect"
    sendClassMessage cls' readAttributeOnModeWithClusterStateCache_endpoint_queue_completionSelector (toMTRClusterStateCacheContainer clusterStateCacheContainer) (toNSNumber endpoint) (toNSObject queue) completion

-- | @- readAttributeGeneratedCommandListWithCompletion:@
readAttributeGeneratedCommandListWithCompletion :: IsMTRBaseClusterModeSelect mtrBaseClusterModeSelect => mtrBaseClusterModeSelect -> Ptr () -> IO ()
readAttributeGeneratedCommandListWithCompletion mtrBaseClusterModeSelect completion =
  sendMessage mtrBaseClusterModeSelect readAttributeGeneratedCommandListWithCompletionSelector completion

-- | @- subscribeAttributeGeneratedCommandListWithParams:subscriptionEstablished:reportHandler:@
subscribeAttributeGeneratedCommandListWithParams_subscriptionEstablished_reportHandler :: (IsMTRBaseClusterModeSelect mtrBaseClusterModeSelect, IsMTRSubscribeParams params) => mtrBaseClusterModeSelect -> params -> Ptr () -> Ptr () -> IO ()
subscribeAttributeGeneratedCommandListWithParams_subscriptionEstablished_reportHandler mtrBaseClusterModeSelect params subscriptionEstablished reportHandler =
  sendMessage mtrBaseClusterModeSelect subscribeAttributeGeneratedCommandListWithParams_subscriptionEstablished_reportHandlerSelector (toMTRSubscribeParams params) subscriptionEstablished reportHandler

-- | @+ readAttributeGeneratedCommandListWithClusterStateCache:endpoint:queue:completion:@
readAttributeGeneratedCommandListWithClusterStateCache_endpoint_queue_completion :: (IsMTRClusterStateCacheContainer clusterStateCacheContainer, IsNSNumber endpoint, IsNSObject queue) => clusterStateCacheContainer -> endpoint -> queue -> Ptr () -> IO ()
readAttributeGeneratedCommandListWithClusterStateCache_endpoint_queue_completion clusterStateCacheContainer endpoint queue completion =
  do
    cls' <- getRequiredClass "MTRBaseClusterModeSelect"
    sendClassMessage cls' readAttributeGeneratedCommandListWithClusterStateCache_endpoint_queue_completionSelector (toMTRClusterStateCacheContainer clusterStateCacheContainer) (toNSNumber endpoint) (toNSObject queue) completion

-- | @- readAttributeAcceptedCommandListWithCompletion:@
readAttributeAcceptedCommandListWithCompletion :: IsMTRBaseClusterModeSelect mtrBaseClusterModeSelect => mtrBaseClusterModeSelect -> Ptr () -> IO ()
readAttributeAcceptedCommandListWithCompletion mtrBaseClusterModeSelect completion =
  sendMessage mtrBaseClusterModeSelect readAttributeAcceptedCommandListWithCompletionSelector completion

-- | @- subscribeAttributeAcceptedCommandListWithParams:subscriptionEstablished:reportHandler:@
subscribeAttributeAcceptedCommandListWithParams_subscriptionEstablished_reportHandler :: (IsMTRBaseClusterModeSelect mtrBaseClusterModeSelect, IsMTRSubscribeParams params) => mtrBaseClusterModeSelect -> params -> Ptr () -> Ptr () -> IO ()
subscribeAttributeAcceptedCommandListWithParams_subscriptionEstablished_reportHandler mtrBaseClusterModeSelect params subscriptionEstablished reportHandler =
  sendMessage mtrBaseClusterModeSelect subscribeAttributeAcceptedCommandListWithParams_subscriptionEstablished_reportHandlerSelector (toMTRSubscribeParams params) subscriptionEstablished reportHandler

-- | @+ readAttributeAcceptedCommandListWithClusterStateCache:endpoint:queue:completion:@
readAttributeAcceptedCommandListWithClusterStateCache_endpoint_queue_completion :: (IsMTRClusterStateCacheContainer clusterStateCacheContainer, IsNSNumber endpoint, IsNSObject queue) => clusterStateCacheContainer -> endpoint -> queue -> Ptr () -> IO ()
readAttributeAcceptedCommandListWithClusterStateCache_endpoint_queue_completion clusterStateCacheContainer endpoint queue completion =
  do
    cls' <- getRequiredClass "MTRBaseClusterModeSelect"
    sendClassMessage cls' readAttributeAcceptedCommandListWithClusterStateCache_endpoint_queue_completionSelector (toMTRClusterStateCacheContainer clusterStateCacheContainer) (toNSNumber endpoint) (toNSObject queue) completion

-- | @- readAttributeAttributeListWithCompletion:@
readAttributeAttributeListWithCompletion :: IsMTRBaseClusterModeSelect mtrBaseClusterModeSelect => mtrBaseClusterModeSelect -> Ptr () -> IO ()
readAttributeAttributeListWithCompletion mtrBaseClusterModeSelect completion =
  sendMessage mtrBaseClusterModeSelect readAttributeAttributeListWithCompletionSelector completion

-- | @- subscribeAttributeAttributeListWithParams:subscriptionEstablished:reportHandler:@
subscribeAttributeAttributeListWithParams_subscriptionEstablished_reportHandler :: (IsMTRBaseClusterModeSelect mtrBaseClusterModeSelect, IsMTRSubscribeParams params) => mtrBaseClusterModeSelect -> params -> Ptr () -> Ptr () -> IO ()
subscribeAttributeAttributeListWithParams_subscriptionEstablished_reportHandler mtrBaseClusterModeSelect params subscriptionEstablished reportHandler =
  sendMessage mtrBaseClusterModeSelect subscribeAttributeAttributeListWithParams_subscriptionEstablished_reportHandlerSelector (toMTRSubscribeParams params) subscriptionEstablished reportHandler

-- | @+ readAttributeAttributeListWithClusterStateCache:endpoint:queue:completion:@
readAttributeAttributeListWithClusterStateCache_endpoint_queue_completion :: (IsMTRClusterStateCacheContainer clusterStateCacheContainer, IsNSNumber endpoint, IsNSObject queue) => clusterStateCacheContainer -> endpoint -> queue -> Ptr () -> IO ()
readAttributeAttributeListWithClusterStateCache_endpoint_queue_completion clusterStateCacheContainer endpoint queue completion =
  do
    cls' <- getRequiredClass "MTRBaseClusterModeSelect"
    sendClassMessage cls' readAttributeAttributeListWithClusterStateCache_endpoint_queue_completionSelector (toMTRClusterStateCacheContainer clusterStateCacheContainer) (toNSNumber endpoint) (toNSObject queue) completion

-- | @- readAttributeFeatureMapWithCompletion:@
readAttributeFeatureMapWithCompletion :: IsMTRBaseClusterModeSelect mtrBaseClusterModeSelect => mtrBaseClusterModeSelect -> Ptr () -> IO ()
readAttributeFeatureMapWithCompletion mtrBaseClusterModeSelect completion =
  sendMessage mtrBaseClusterModeSelect readAttributeFeatureMapWithCompletionSelector completion

-- | @- subscribeAttributeFeatureMapWithParams:subscriptionEstablished:reportHandler:@
subscribeAttributeFeatureMapWithParams_subscriptionEstablished_reportHandler :: (IsMTRBaseClusterModeSelect mtrBaseClusterModeSelect, IsMTRSubscribeParams params) => mtrBaseClusterModeSelect -> params -> Ptr () -> Ptr () -> IO ()
subscribeAttributeFeatureMapWithParams_subscriptionEstablished_reportHandler mtrBaseClusterModeSelect params subscriptionEstablished reportHandler =
  sendMessage mtrBaseClusterModeSelect subscribeAttributeFeatureMapWithParams_subscriptionEstablished_reportHandlerSelector (toMTRSubscribeParams params) subscriptionEstablished reportHandler

-- | @+ readAttributeFeatureMapWithClusterStateCache:endpoint:queue:completion:@
readAttributeFeatureMapWithClusterStateCache_endpoint_queue_completion :: (IsMTRClusterStateCacheContainer clusterStateCacheContainer, IsNSNumber endpoint, IsNSObject queue) => clusterStateCacheContainer -> endpoint -> queue -> Ptr () -> IO ()
readAttributeFeatureMapWithClusterStateCache_endpoint_queue_completion clusterStateCacheContainer endpoint queue completion =
  do
    cls' <- getRequiredClass "MTRBaseClusterModeSelect"
    sendClassMessage cls' readAttributeFeatureMapWithClusterStateCache_endpoint_queue_completionSelector (toMTRClusterStateCacheContainer clusterStateCacheContainer) (toNSNumber endpoint) (toNSObject queue) completion

-- | @- readAttributeClusterRevisionWithCompletion:@
readAttributeClusterRevisionWithCompletion :: IsMTRBaseClusterModeSelect mtrBaseClusterModeSelect => mtrBaseClusterModeSelect -> Ptr () -> IO ()
readAttributeClusterRevisionWithCompletion mtrBaseClusterModeSelect completion =
  sendMessage mtrBaseClusterModeSelect readAttributeClusterRevisionWithCompletionSelector completion

-- | @- subscribeAttributeClusterRevisionWithParams:subscriptionEstablished:reportHandler:@
subscribeAttributeClusterRevisionWithParams_subscriptionEstablished_reportHandler :: (IsMTRBaseClusterModeSelect mtrBaseClusterModeSelect, IsMTRSubscribeParams params) => mtrBaseClusterModeSelect -> params -> Ptr () -> Ptr () -> IO ()
subscribeAttributeClusterRevisionWithParams_subscriptionEstablished_reportHandler mtrBaseClusterModeSelect params subscriptionEstablished reportHandler =
  sendMessage mtrBaseClusterModeSelect subscribeAttributeClusterRevisionWithParams_subscriptionEstablished_reportHandlerSelector (toMTRSubscribeParams params) subscriptionEstablished reportHandler

-- | @+ readAttributeClusterRevisionWithClusterStateCache:endpoint:queue:completion:@
readAttributeClusterRevisionWithClusterStateCache_endpoint_queue_completion :: (IsMTRClusterStateCacheContainer clusterStateCacheContainer, IsNSNumber endpoint, IsNSObject queue) => clusterStateCacheContainer -> endpoint -> queue -> Ptr () -> IO ()
readAttributeClusterRevisionWithClusterStateCache_endpoint_queue_completion clusterStateCacheContainer endpoint queue completion =
  do
    cls' <- getRequiredClass "MTRBaseClusterModeSelect"
    sendClassMessage cls' readAttributeClusterRevisionWithClusterStateCache_endpoint_queue_completionSelector (toMTRClusterStateCacheContainer clusterStateCacheContainer) (toNSNumber endpoint) (toNSObject queue) completion

-- | @- init@
init_ :: IsMTRBaseClusterModeSelect mtrBaseClusterModeSelect => mtrBaseClusterModeSelect -> IO (Id MTRBaseClusterModeSelect)
init_ mtrBaseClusterModeSelect =
  sendOwnedMessage mtrBaseClusterModeSelect initSelector

-- | @+ new@
new :: IO (Id MTRBaseClusterModeSelect)
new  =
  do
    cls' <- getRequiredClass "MTRBaseClusterModeSelect"
    sendOwnedClassMessage cls' newSelector

-- | @- initWithDevice:endpoint:queue:@
initWithDevice_endpoint_queue :: (IsMTRBaseClusterModeSelect mtrBaseClusterModeSelect, IsMTRBaseDevice device, IsNSObject queue) => mtrBaseClusterModeSelect -> device -> CUShort -> queue -> IO (Id MTRBaseClusterModeSelect)
initWithDevice_endpoint_queue mtrBaseClusterModeSelect device endpoint queue =
  sendOwnedMessage mtrBaseClusterModeSelect initWithDevice_endpoint_queueSelector (toMTRBaseDevice device) endpoint (toNSObject queue)

-- | @- changeToModeWithParams:completionHandler:@
changeToModeWithParams_completionHandler :: (IsMTRBaseClusterModeSelect mtrBaseClusterModeSelect, IsMTRModeSelectClusterChangeToModeParams params) => mtrBaseClusterModeSelect -> params -> Ptr () -> IO ()
changeToModeWithParams_completionHandler mtrBaseClusterModeSelect params completionHandler =
  sendMessage mtrBaseClusterModeSelect changeToModeWithParams_completionHandlerSelector (toMTRModeSelectClusterChangeToModeParams params) completionHandler

-- | @- readAttributeDescriptionWithCompletionHandler:@
readAttributeDescriptionWithCompletionHandler :: IsMTRBaseClusterModeSelect mtrBaseClusterModeSelect => mtrBaseClusterModeSelect -> Ptr () -> IO ()
readAttributeDescriptionWithCompletionHandler mtrBaseClusterModeSelect completionHandler =
  sendMessage mtrBaseClusterModeSelect readAttributeDescriptionWithCompletionHandlerSelector completionHandler

-- | @- subscribeAttributeDescriptionWithMinInterval:maxInterval:params:subscriptionEstablished:reportHandler:@
subscribeAttributeDescriptionWithMinInterval_maxInterval_params_subscriptionEstablished_reportHandler :: (IsMTRBaseClusterModeSelect mtrBaseClusterModeSelect, IsNSNumber minInterval, IsNSNumber maxInterval, IsMTRSubscribeParams params) => mtrBaseClusterModeSelect -> minInterval -> maxInterval -> params -> Ptr () -> Ptr () -> IO ()
subscribeAttributeDescriptionWithMinInterval_maxInterval_params_subscriptionEstablished_reportHandler mtrBaseClusterModeSelect minInterval maxInterval params subscriptionEstablishedHandler reportHandler =
  sendMessage mtrBaseClusterModeSelect subscribeAttributeDescriptionWithMinInterval_maxInterval_params_subscriptionEstablished_reportHandlerSelector (toNSNumber minInterval) (toNSNumber maxInterval) (toMTRSubscribeParams params) subscriptionEstablishedHandler reportHandler

-- | @+ readAttributeDescriptionWithAttributeCache:endpoint:queue:completionHandler:@
readAttributeDescriptionWithAttributeCache_endpoint_queue_completionHandler :: (IsMTRAttributeCacheContainer attributeCacheContainer, IsNSNumber endpoint, IsNSObject queue) => attributeCacheContainer -> endpoint -> queue -> Ptr () -> IO ()
readAttributeDescriptionWithAttributeCache_endpoint_queue_completionHandler attributeCacheContainer endpoint queue completionHandler =
  do
    cls' <- getRequiredClass "MTRBaseClusterModeSelect"
    sendClassMessage cls' readAttributeDescriptionWithAttributeCache_endpoint_queue_completionHandlerSelector (toMTRAttributeCacheContainer attributeCacheContainer) (toNSNumber endpoint) (toNSObject queue) completionHandler

-- | @- readAttributeStandardNamespaceWithCompletionHandler:@
readAttributeStandardNamespaceWithCompletionHandler :: IsMTRBaseClusterModeSelect mtrBaseClusterModeSelect => mtrBaseClusterModeSelect -> Ptr () -> IO ()
readAttributeStandardNamespaceWithCompletionHandler mtrBaseClusterModeSelect completionHandler =
  sendMessage mtrBaseClusterModeSelect readAttributeStandardNamespaceWithCompletionHandlerSelector completionHandler

-- | @- subscribeAttributeStandardNamespaceWithMinInterval:maxInterval:params:subscriptionEstablished:reportHandler:@
subscribeAttributeStandardNamespaceWithMinInterval_maxInterval_params_subscriptionEstablished_reportHandler :: (IsMTRBaseClusterModeSelect mtrBaseClusterModeSelect, IsNSNumber minInterval, IsNSNumber maxInterval, IsMTRSubscribeParams params) => mtrBaseClusterModeSelect -> minInterval -> maxInterval -> params -> Ptr () -> Ptr () -> IO ()
subscribeAttributeStandardNamespaceWithMinInterval_maxInterval_params_subscriptionEstablished_reportHandler mtrBaseClusterModeSelect minInterval maxInterval params subscriptionEstablishedHandler reportHandler =
  sendMessage mtrBaseClusterModeSelect subscribeAttributeStandardNamespaceWithMinInterval_maxInterval_params_subscriptionEstablished_reportHandlerSelector (toNSNumber minInterval) (toNSNumber maxInterval) (toMTRSubscribeParams params) subscriptionEstablishedHandler reportHandler

-- | @+ readAttributeStandardNamespaceWithAttributeCache:endpoint:queue:completionHandler:@
readAttributeStandardNamespaceWithAttributeCache_endpoint_queue_completionHandler :: (IsMTRAttributeCacheContainer attributeCacheContainer, IsNSNumber endpoint, IsNSObject queue) => attributeCacheContainer -> endpoint -> queue -> Ptr () -> IO ()
readAttributeStandardNamespaceWithAttributeCache_endpoint_queue_completionHandler attributeCacheContainer endpoint queue completionHandler =
  do
    cls' <- getRequiredClass "MTRBaseClusterModeSelect"
    sendClassMessage cls' readAttributeStandardNamespaceWithAttributeCache_endpoint_queue_completionHandlerSelector (toMTRAttributeCacheContainer attributeCacheContainer) (toNSNumber endpoint) (toNSObject queue) completionHandler

-- | @- readAttributeSupportedModesWithCompletionHandler:@
readAttributeSupportedModesWithCompletionHandler :: IsMTRBaseClusterModeSelect mtrBaseClusterModeSelect => mtrBaseClusterModeSelect -> Ptr () -> IO ()
readAttributeSupportedModesWithCompletionHandler mtrBaseClusterModeSelect completionHandler =
  sendMessage mtrBaseClusterModeSelect readAttributeSupportedModesWithCompletionHandlerSelector completionHandler

-- | @- subscribeAttributeSupportedModesWithMinInterval:maxInterval:params:subscriptionEstablished:reportHandler:@
subscribeAttributeSupportedModesWithMinInterval_maxInterval_params_subscriptionEstablished_reportHandler :: (IsMTRBaseClusterModeSelect mtrBaseClusterModeSelect, IsNSNumber minInterval, IsNSNumber maxInterval, IsMTRSubscribeParams params) => mtrBaseClusterModeSelect -> minInterval -> maxInterval -> params -> Ptr () -> Ptr () -> IO ()
subscribeAttributeSupportedModesWithMinInterval_maxInterval_params_subscriptionEstablished_reportHandler mtrBaseClusterModeSelect minInterval maxInterval params subscriptionEstablishedHandler reportHandler =
  sendMessage mtrBaseClusterModeSelect subscribeAttributeSupportedModesWithMinInterval_maxInterval_params_subscriptionEstablished_reportHandlerSelector (toNSNumber minInterval) (toNSNumber maxInterval) (toMTRSubscribeParams params) subscriptionEstablishedHandler reportHandler

-- | @+ readAttributeSupportedModesWithAttributeCache:endpoint:queue:completionHandler:@
readAttributeSupportedModesWithAttributeCache_endpoint_queue_completionHandler :: (IsMTRAttributeCacheContainer attributeCacheContainer, IsNSNumber endpoint, IsNSObject queue) => attributeCacheContainer -> endpoint -> queue -> Ptr () -> IO ()
readAttributeSupportedModesWithAttributeCache_endpoint_queue_completionHandler attributeCacheContainer endpoint queue completionHandler =
  do
    cls' <- getRequiredClass "MTRBaseClusterModeSelect"
    sendClassMessage cls' readAttributeSupportedModesWithAttributeCache_endpoint_queue_completionHandlerSelector (toMTRAttributeCacheContainer attributeCacheContainer) (toNSNumber endpoint) (toNSObject queue) completionHandler

-- | @- readAttributeCurrentModeWithCompletionHandler:@
readAttributeCurrentModeWithCompletionHandler :: IsMTRBaseClusterModeSelect mtrBaseClusterModeSelect => mtrBaseClusterModeSelect -> Ptr () -> IO ()
readAttributeCurrentModeWithCompletionHandler mtrBaseClusterModeSelect completionHandler =
  sendMessage mtrBaseClusterModeSelect readAttributeCurrentModeWithCompletionHandlerSelector completionHandler

-- | @- subscribeAttributeCurrentModeWithMinInterval:maxInterval:params:subscriptionEstablished:reportHandler:@
subscribeAttributeCurrentModeWithMinInterval_maxInterval_params_subscriptionEstablished_reportHandler :: (IsMTRBaseClusterModeSelect mtrBaseClusterModeSelect, IsNSNumber minInterval, IsNSNumber maxInterval, IsMTRSubscribeParams params) => mtrBaseClusterModeSelect -> minInterval -> maxInterval -> params -> Ptr () -> Ptr () -> IO ()
subscribeAttributeCurrentModeWithMinInterval_maxInterval_params_subscriptionEstablished_reportHandler mtrBaseClusterModeSelect minInterval maxInterval params subscriptionEstablishedHandler reportHandler =
  sendMessage mtrBaseClusterModeSelect subscribeAttributeCurrentModeWithMinInterval_maxInterval_params_subscriptionEstablished_reportHandlerSelector (toNSNumber minInterval) (toNSNumber maxInterval) (toMTRSubscribeParams params) subscriptionEstablishedHandler reportHandler

-- | @+ readAttributeCurrentModeWithAttributeCache:endpoint:queue:completionHandler:@
readAttributeCurrentModeWithAttributeCache_endpoint_queue_completionHandler :: (IsMTRAttributeCacheContainer attributeCacheContainer, IsNSNumber endpoint, IsNSObject queue) => attributeCacheContainer -> endpoint -> queue -> Ptr () -> IO ()
readAttributeCurrentModeWithAttributeCache_endpoint_queue_completionHandler attributeCacheContainer endpoint queue completionHandler =
  do
    cls' <- getRequiredClass "MTRBaseClusterModeSelect"
    sendClassMessage cls' readAttributeCurrentModeWithAttributeCache_endpoint_queue_completionHandlerSelector (toMTRAttributeCacheContainer attributeCacheContainer) (toNSNumber endpoint) (toNSObject queue) completionHandler

-- | @- readAttributeStartUpModeWithCompletionHandler:@
readAttributeStartUpModeWithCompletionHandler :: IsMTRBaseClusterModeSelect mtrBaseClusterModeSelect => mtrBaseClusterModeSelect -> Ptr () -> IO ()
readAttributeStartUpModeWithCompletionHandler mtrBaseClusterModeSelect completionHandler =
  sendMessage mtrBaseClusterModeSelect readAttributeStartUpModeWithCompletionHandlerSelector completionHandler

-- | @- writeAttributeStartUpModeWithValue:completionHandler:@
writeAttributeStartUpModeWithValue_completionHandler :: (IsMTRBaseClusterModeSelect mtrBaseClusterModeSelect, IsNSNumber value) => mtrBaseClusterModeSelect -> value -> Ptr () -> IO ()
writeAttributeStartUpModeWithValue_completionHandler mtrBaseClusterModeSelect value completionHandler =
  sendMessage mtrBaseClusterModeSelect writeAttributeStartUpModeWithValue_completionHandlerSelector (toNSNumber value) completionHandler

-- | @- writeAttributeStartUpModeWithValue:params:completionHandler:@
writeAttributeStartUpModeWithValue_params_completionHandler :: (IsMTRBaseClusterModeSelect mtrBaseClusterModeSelect, IsNSNumber value, IsMTRWriteParams params) => mtrBaseClusterModeSelect -> value -> params -> Ptr () -> IO ()
writeAttributeStartUpModeWithValue_params_completionHandler mtrBaseClusterModeSelect value params completionHandler =
  sendMessage mtrBaseClusterModeSelect writeAttributeStartUpModeWithValue_params_completionHandlerSelector (toNSNumber value) (toMTRWriteParams params) completionHandler

-- | @- subscribeAttributeStartUpModeWithMinInterval:maxInterval:params:subscriptionEstablished:reportHandler:@
subscribeAttributeStartUpModeWithMinInterval_maxInterval_params_subscriptionEstablished_reportHandler :: (IsMTRBaseClusterModeSelect mtrBaseClusterModeSelect, IsNSNumber minInterval, IsNSNumber maxInterval, IsMTRSubscribeParams params) => mtrBaseClusterModeSelect -> minInterval -> maxInterval -> params -> Ptr () -> Ptr () -> IO ()
subscribeAttributeStartUpModeWithMinInterval_maxInterval_params_subscriptionEstablished_reportHandler mtrBaseClusterModeSelect minInterval maxInterval params subscriptionEstablishedHandler reportHandler =
  sendMessage mtrBaseClusterModeSelect subscribeAttributeStartUpModeWithMinInterval_maxInterval_params_subscriptionEstablished_reportHandlerSelector (toNSNumber minInterval) (toNSNumber maxInterval) (toMTRSubscribeParams params) subscriptionEstablishedHandler reportHandler

-- | @+ readAttributeStartUpModeWithAttributeCache:endpoint:queue:completionHandler:@
readAttributeStartUpModeWithAttributeCache_endpoint_queue_completionHandler :: (IsMTRAttributeCacheContainer attributeCacheContainer, IsNSNumber endpoint, IsNSObject queue) => attributeCacheContainer -> endpoint -> queue -> Ptr () -> IO ()
readAttributeStartUpModeWithAttributeCache_endpoint_queue_completionHandler attributeCacheContainer endpoint queue completionHandler =
  do
    cls' <- getRequiredClass "MTRBaseClusterModeSelect"
    sendClassMessage cls' readAttributeStartUpModeWithAttributeCache_endpoint_queue_completionHandlerSelector (toMTRAttributeCacheContainer attributeCacheContainer) (toNSNumber endpoint) (toNSObject queue) completionHandler

-- | @- readAttributeOnModeWithCompletionHandler:@
readAttributeOnModeWithCompletionHandler :: IsMTRBaseClusterModeSelect mtrBaseClusterModeSelect => mtrBaseClusterModeSelect -> Ptr () -> IO ()
readAttributeOnModeWithCompletionHandler mtrBaseClusterModeSelect completionHandler =
  sendMessage mtrBaseClusterModeSelect readAttributeOnModeWithCompletionHandlerSelector completionHandler

-- | @- writeAttributeOnModeWithValue:completionHandler:@
writeAttributeOnModeWithValue_completionHandler :: (IsMTRBaseClusterModeSelect mtrBaseClusterModeSelect, IsNSNumber value) => mtrBaseClusterModeSelect -> value -> Ptr () -> IO ()
writeAttributeOnModeWithValue_completionHandler mtrBaseClusterModeSelect value completionHandler =
  sendMessage mtrBaseClusterModeSelect writeAttributeOnModeWithValue_completionHandlerSelector (toNSNumber value) completionHandler

-- | @- writeAttributeOnModeWithValue:params:completionHandler:@
writeAttributeOnModeWithValue_params_completionHandler :: (IsMTRBaseClusterModeSelect mtrBaseClusterModeSelect, IsNSNumber value, IsMTRWriteParams params) => mtrBaseClusterModeSelect -> value -> params -> Ptr () -> IO ()
writeAttributeOnModeWithValue_params_completionHandler mtrBaseClusterModeSelect value params completionHandler =
  sendMessage mtrBaseClusterModeSelect writeAttributeOnModeWithValue_params_completionHandlerSelector (toNSNumber value) (toMTRWriteParams params) completionHandler

-- | @- subscribeAttributeOnModeWithMinInterval:maxInterval:params:subscriptionEstablished:reportHandler:@
subscribeAttributeOnModeWithMinInterval_maxInterval_params_subscriptionEstablished_reportHandler :: (IsMTRBaseClusterModeSelect mtrBaseClusterModeSelect, IsNSNumber minInterval, IsNSNumber maxInterval, IsMTRSubscribeParams params) => mtrBaseClusterModeSelect -> minInterval -> maxInterval -> params -> Ptr () -> Ptr () -> IO ()
subscribeAttributeOnModeWithMinInterval_maxInterval_params_subscriptionEstablished_reportHandler mtrBaseClusterModeSelect minInterval maxInterval params subscriptionEstablishedHandler reportHandler =
  sendMessage mtrBaseClusterModeSelect subscribeAttributeOnModeWithMinInterval_maxInterval_params_subscriptionEstablished_reportHandlerSelector (toNSNumber minInterval) (toNSNumber maxInterval) (toMTRSubscribeParams params) subscriptionEstablishedHandler reportHandler

-- | @+ readAttributeOnModeWithAttributeCache:endpoint:queue:completionHandler:@
readAttributeOnModeWithAttributeCache_endpoint_queue_completionHandler :: (IsMTRAttributeCacheContainer attributeCacheContainer, IsNSNumber endpoint, IsNSObject queue) => attributeCacheContainer -> endpoint -> queue -> Ptr () -> IO ()
readAttributeOnModeWithAttributeCache_endpoint_queue_completionHandler attributeCacheContainer endpoint queue completionHandler =
  do
    cls' <- getRequiredClass "MTRBaseClusterModeSelect"
    sendClassMessage cls' readAttributeOnModeWithAttributeCache_endpoint_queue_completionHandlerSelector (toMTRAttributeCacheContainer attributeCacheContainer) (toNSNumber endpoint) (toNSObject queue) completionHandler

-- | @- readAttributeGeneratedCommandListWithCompletionHandler:@
readAttributeGeneratedCommandListWithCompletionHandler :: IsMTRBaseClusterModeSelect mtrBaseClusterModeSelect => mtrBaseClusterModeSelect -> Ptr () -> IO ()
readAttributeGeneratedCommandListWithCompletionHandler mtrBaseClusterModeSelect completionHandler =
  sendMessage mtrBaseClusterModeSelect readAttributeGeneratedCommandListWithCompletionHandlerSelector completionHandler

-- | @- subscribeAttributeGeneratedCommandListWithMinInterval:maxInterval:params:subscriptionEstablished:reportHandler:@
subscribeAttributeGeneratedCommandListWithMinInterval_maxInterval_params_subscriptionEstablished_reportHandler :: (IsMTRBaseClusterModeSelect mtrBaseClusterModeSelect, IsNSNumber minInterval, IsNSNumber maxInterval, IsMTRSubscribeParams params) => mtrBaseClusterModeSelect -> minInterval -> maxInterval -> params -> Ptr () -> Ptr () -> IO ()
subscribeAttributeGeneratedCommandListWithMinInterval_maxInterval_params_subscriptionEstablished_reportHandler mtrBaseClusterModeSelect minInterval maxInterval params subscriptionEstablishedHandler reportHandler =
  sendMessage mtrBaseClusterModeSelect subscribeAttributeGeneratedCommandListWithMinInterval_maxInterval_params_subscriptionEstablished_reportHandlerSelector (toNSNumber minInterval) (toNSNumber maxInterval) (toMTRSubscribeParams params) subscriptionEstablishedHandler reportHandler

-- | @+ readAttributeGeneratedCommandListWithAttributeCache:endpoint:queue:completionHandler:@
readAttributeGeneratedCommandListWithAttributeCache_endpoint_queue_completionHandler :: (IsMTRAttributeCacheContainer attributeCacheContainer, IsNSNumber endpoint, IsNSObject queue) => attributeCacheContainer -> endpoint -> queue -> Ptr () -> IO ()
readAttributeGeneratedCommandListWithAttributeCache_endpoint_queue_completionHandler attributeCacheContainer endpoint queue completionHandler =
  do
    cls' <- getRequiredClass "MTRBaseClusterModeSelect"
    sendClassMessage cls' readAttributeGeneratedCommandListWithAttributeCache_endpoint_queue_completionHandlerSelector (toMTRAttributeCacheContainer attributeCacheContainer) (toNSNumber endpoint) (toNSObject queue) completionHandler

-- | @- readAttributeAcceptedCommandListWithCompletionHandler:@
readAttributeAcceptedCommandListWithCompletionHandler :: IsMTRBaseClusterModeSelect mtrBaseClusterModeSelect => mtrBaseClusterModeSelect -> Ptr () -> IO ()
readAttributeAcceptedCommandListWithCompletionHandler mtrBaseClusterModeSelect completionHandler =
  sendMessage mtrBaseClusterModeSelect readAttributeAcceptedCommandListWithCompletionHandlerSelector completionHandler

-- | @- subscribeAttributeAcceptedCommandListWithMinInterval:maxInterval:params:subscriptionEstablished:reportHandler:@
subscribeAttributeAcceptedCommandListWithMinInterval_maxInterval_params_subscriptionEstablished_reportHandler :: (IsMTRBaseClusterModeSelect mtrBaseClusterModeSelect, IsNSNumber minInterval, IsNSNumber maxInterval, IsMTRSubscribeParams params) => mtrBaseClusterModeSelect -> minInterval -> maxInterval -> params -> Ptr () -> Ptr () -> IO ()
subscribeAttributeAcceptedCommandListWithMinInterval_maxInterval_params_subscriptionEstablished_reportHandler mtrBaseClusterModeSelect minInterval maxInterval params subscriptionEstablishedHandler reportHandler =
  sendMessage mtrBaseClusterModeSelect subscribeAttributeAcceptedCommandListWithMinInterval_maxInterval_params_subscriptionEstablished_reportHandlerSelector (toNSNumber minInterval) (toNSNumber maxInterval) (toMTRSubscribeParams params) subscriptionEstablishedHandler reportHandler

-- | @+ readAttributeAcceptedCommandListWithAttributeCache:endpoint:queue:completionHandler:@
readAttributeAcceptedCommandListWithAttributeCache_endpoint_queue_completionHandler :: (IsMTRAttributeCacheContainer attributeCacheContainer, IsNSNumber endpoint, IsNSObject queue) => attributeCacheContainer -> endpoint -> queue -> Ptr () -> IO ()
readAttributeAcceptedCommandListWithAttributeCache_endpoint_queue_completionHandler attributeCacheContainer endpoint queue completionHandler =
  do
    cls' <- getRequiredClass "MTRBaseClusterModeSelect"
    sendClassMessage cls' readAttributeAcceptedCommandListWithAttributeCache_endpoint_queue_completionHandlerSelector (toMTRAttributeCacheContainer attributeCacheContainer) (toNSNumber endpoint) (toNSObject queue) completionHandler

-- | @- readAttributeAttributeListWithCompletionHandler:@
readAttributeAttributeListWithCompletionHandler :: IsMTRBaseClusterModeSelect mtrBaseClusterModeSelect => mtrBaseClusterModeSelect -> Ptr () -> IO ()
readAttributeAttributeListWithCompletionHandler mtrBaseClusterModeSelect completionHandler =
  sendMessage mtrBaseClusterModeSelect readAttributeAttributeListWithCompletionHandlerSelector completionHandler

-- | @- subscribeAttributeAttributeListWithMinInterval:maxInterval:params:subscriptionEstablished:reportHandler:@
subscribeAttributeAttributeListWithMinInterval_maxInterval_params_subscriptionEstablished_reportHandler :: (IsMTRBaseClusterModeSelect mtrBaseClusterModeSelect, IsNSNumber minInterval, IsNSNumber maxInterval, IsMTRSubscribeParams params) => mtrBaseClusterModeSelect -> minInterval -> maxInterval -> params -> Ptr () -> Ptr () -> IO ()
subscribeAttributeAttributeListWithMinInterval_maxInterval_params_subscriptionEstablished_reportHandler mtrBaseClusterModeSelect minInterval maxInterval params subscriptionEstablishedHandler reportHandler =
  sendMessage mtrBaseClusterModeSelect subscribeAttributeAttributeListWithMinInterval_maxInterval_params_subscriptionEstablished_reportHandlerSelector (toNSNumber minInterval) (toNSNumber maxInterval) (toMTRSubscribeParams params) subscriptionEstablishedHandler reportHandler

-- | @+ readAttributeAttributeListWithAttributeCache:endpoint:queue:completionHandler:@
readAttributeAttributeListWithAttributeCache_endpoint_queue_completionHandler :: (IsMTRAttributeCacheContainer attributeCacheContainer, IsNSNumber endpoint, IsNSObject queue) => attributeCacheContainer -> endpoint -> queue -> Ptr () -> IO ()
readAttributeAttributeListWithAttributeCache_endpoint_queue_completionHandler attributeCacheContainer endpoint queue completionHandler =
  do
    cls' <- getRequiredClass "MTRBaseClusterModeSelect"
    sendClassMessage cls' readAttributeAttributeListWithAttributeCache_endpoint_queue_completionHandlerSelector (toMTRAttributeCacheContainer attributeCacheContainer) (toNSNumber endpoint) (toNSObject queue) completionHandler

-- | @- readAttributeFeatureMapWithCompletionHandler:@
readAttributeFeatureMapWithCompletionHandler :: IsMTRBaseClusterModeSelect mtrBaseClusterModeSelect => mtrBaseClusterModeSelect -> Ptr () -> IO ()
readAttributeFeatureMapWithCompletionHandler mtrBaseClusterModeSelect completionHandler =
  sendMessage mtrBaseClusterModeSelect readAttributeFeatureMapWithCompletionHandlerSelector completionHandler

-- | @- subscribeAttributeFeatureMapWithMinInterval:maxInterval:params:subscriptionEstablished:reportHandler:@
subscribeAttributeFeatureMapWithMinInterval_maxInterval_params_subscriptionEstablished_reportHandler :: (IsMTRBaseClusterModeSelect mtrBaseClusterModeSelect, IsNSNumber minInterval, IsNSNumber maxInterval, IsMTRSubscribeParams params) => mtrBaseClusterModeSelect -> minInterval -> maxInterval -> params -> Ptr () -> Ptr () -> IO ()
subscribeAttributeFeatureMapWithMinInterval_maxInterval_params_subscriptionEstablished_reportHandler mtrBaseClusterModeSelect minInterval maxInterval params subscriptionEstablishedHandler reportHandler =
  sendMessage mtrBaseClusterModeSelect subscribeAttributeFeatureMapWithMinInterval_maxInterval_params_subscriptionEstablished_reportHandlerSelector (toNSNumber minInterval) (toNSNumber maxInterval) (toMTRSubscribeParams params) subscriptionEstablishedHandler reportHandler

-- | @+ readAttributeFeatureMapWithAttributeCache:endpoint:queue:completionHandler:@
readAttributeFeatureMapWithAttributeCache_endpoint_queue_completionHandler :: (IsMTRAttributeCacheContainer attributeCacheContainer, IsNSNumber endpoint, IsNSObject queue) => attributeCacheContainer -> endpoint -> queue -> Ptr () -> IO ()
readAttributeFeatureMapWithAttributeCache_endpoint_queue_completionHandler attributeCacheContainer endpoint queue completionHandler =
  do
    cls' <- getRequiredClass "MTRBaseClusterModeSelect"
    sendClassMessage cls' readAttributeFeatureMapWithAttributeCache_endpoint_queue_completionHandlerSelector (toMTRAttributeCacheContainer attributeCacheContainer) (toNSNumber endpoint) (toNSObject queue) completionHandler

-- | @- readAttributeClusterRevisionWithCompletionHandler:@
readAttributeClusterRevisionWithCompletionHandler :: IsMTRBaseClusterModeSelect mtrBaseClusterModeSelect => mtrBaseClusterModeSelect -> Ptr () -> IO ()
readAttributeClusterRevisionWithCompletionHandler mtrBaseClusterModeSelect completionHandler =
  sendMessage mtrBaseClusterModeSelect readAttributeClusterRevisionWithCompletionHandlerSelector completionHandler

-- | @- subscribeAttributeClusterRevisionWithMinInterval:maxInterval:params:subscriptionEstablished:reportHandler:@
subscribeAttributeClusterRevisionWithMinInterval_maxInterval_params_subscriptionEstablished_reportHandler :: (IsMTRBaseClusterModeSelect mtrBaseClusterModeSelect, IsNSNumber minInterval, IsNSNumber maxInterval, IsMTRSubscribeParams params) => mtrBaseClusterModeSelect -> minInterval -> maxInterval -> params -> Ptr () -> Ptr () -> IO ()
subscribeAttributeClusterRevisionWithMinInterval_maxInterval_params_subscriptionEstablished_reportHandler mtrBaseClusterModeSelect minInterval maxInterval params subscriptionEstablishedHandler reportHandler =
  sendMessage mtrBaseClusterModeSelect subscribeAttributeClusterRevisionWithMinInterval_maxInterval_params_subscriptionEstablished_reportHandlerSelector (toNSNumber minInterval) (toNSNumber maxInterval) (toMTRSubscribeParams params) subscriptionEstablishedHandler reportHandler

-- | @+ readAttributeClusterRevisionWithAttributeCache:endpoint:queue:completionHandler:@
readAttributeClusterRevisionWithAttributeCache_endpoint_queue_completionHandler :: (IsMTRAttributeCacheContainer attributeCacheContainer, IsNSNumber endpoint, IsNSObject queue) => attributeCacheContainer -> endpoint -> queue -> Ptr () -> IO ()
readAttributeClusterRevisionWithAttributeCache_endpoint_queue_completionHandler attributeCacheContainer endpoint queue completionHandler =
  do
    cls' <- getRequiredClass "MTRBaseClusterModeSelect"
    sendClassMessage cls' readAttributeClusterRevisionWithAttributeCache_endpoint_queue_completionHandlerSelector (toMTRAttributeCacheContainer attributeCacheContainer) (toNSNumber endpoint) (toNSObject queue) completionHandler

-- | For all instance methods (reads, writes, commands) that take a completion, the completion will be called on the provided queue.
--
-- ObjC selector: @- initWithDevice:endpointID:queue:@
initWithDevice_endpointID_queue :: (IsMTRBaseClusterModeSelect mtrBaseClusterModeSelect, IsMTRBaseDevice device, IsNSNumber endpointID, IsNSObject queue) => mtrBaseClusterModeSelect -> device -> endpointID -> queue -> IO (Id MTRBaseClusterModeSelect)
initWithDevice_endpointID_queue mtrBaseClusterModeSelect device endpointID queue =
  sendOwnedMessage mtrBaseClusterModeSelect initWithDevice_endpointID_queueSelector (toMTRBaseDevice device) (toNSNumber endpointID) (toNSObject queue)

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @changeToModeWithParams:completion:@
changeToModeWithParams_completionSelector :: Selector '[Id MTRModeSelectClusterChangeToModeParams, Ptr ()] ()
changeToModeWithParams_completionSelector = mkSelector "changeToModeWithParams:completion:"

-- | @Selector@ for @readAttributeDescriptionWithCompletion:@
readAttributeDescriptionWithCompletionSelector :: Selector '[Ptr ()] ()
readAttributeDescriptionWithCompletionSelector = mkSelector "readAttributeDescriptionWithCompletion:"

-- | @Selector@ for @subscribeAttributeDescriptionWithParams:subscriptionEstablished:reportHandler:@
subscribeAttributeDescriptionWithParams_subscriptionEstablished_reportHandlerSelector :: Selector '[Id MTRSubscribeParams, Ptr (), Ptr ()] ()
subscribeAttributeDescriptionWithParams_subscriptionEstablished_reportHandlerSelector = mkSelector "subscribeAttributeDescriptionWithParams:subscriptionEstablished:reportHandler:"

-- | @Selector@ for @readAttributeDescriptionWithClusterStateCache:endpoint:queue:completion:@
readAttributeDescriptionWithClusterStateCache_endpoint_queue_completionSelector :: Selector '[Id MTRClusterStateCacheContainer, Id NSNumber, Id NSObject, Ptr ()] ()
readAttributeDescriptionWithClusterStateCache_endpoint_queue_completionSelector = mkSelector "readAttributeDescriptionWithClusterStateCache:endpoint:queue:completion:"

-- | @Selector@ for @readAttributeStandardNamespaceWithCompletion:@
readAttributeStandardNamespaceWithCompletionSelector :: Selector '[Ptr ()] ()
readAttributeStandardNamespaceWithCompletionSelector = mkSelector "readAttributeStandardNamespaceWithCompletion:"

-- | @Selector@ for @subscribeAttributeStandardNamespaceWithParams:subscriptionEstablished:reportHandler:@
subscribeAttributeStandardNamespaceWithParams_subscriptionEstablished_reportHandlerSelector :: Selector '[Id MTRSubscribeParams, Ptr (), Ptr ()] ()
subscribeAttributeStandardNamespaceWithParams_subscriptionEstablished_reportHandlerSelector = mkSelector "subscribeAttributeStandardNamespaceWithParams:subscriptionEstablished:reportHandler:"

-- | @Selector@ for @readAttributeStandardNamespaceWithClusterStateCache:endpoint:queue:completion:@
readAttributeStandardNamespaceWithClusterStateCache_endpoint_queue_completionSelector :: Selector '[Id MTRClusterStateCacheContainer, Id NSNumber, Id NSObject, Ptr ()] ()
readAttributeStandardNamespaceWithClusterStateCache_endpoint_queue_completionSelector = mkSelector "readAttributeStandardNamespaceWithClusterStateCache:endpoint:queue:completion:"

-- | @Selector@ for @readAttributeSupportedModesWithCompletion:@
readAttributeSupportedModesWithCompletionSelector :: Selector '[Ptr ()] ()
readAttributeSupportedModesWithCompletionSelector = mkSelector "readAttributeSupportedModesWithCompletion:"

-- | @Selector@ for @subscribeAttributeSupportedModesWithParams:subscriptionEstablished:reportHandler:@
subscribeAttributeSupportedModesWithParams_subscriptionEstablished_reportHandlerSelector :: Selector '[Id MTRSubscribeParams, Ptr (), Ptr ()] ()
subscribeAttributeSupportedModesWithParams_subscriptionEstablished_reportHandlerSelector = mkSelector "subscribeAttributeSupportedModesWithParams:subscriptionEstablished:reportHandler:"

-- | @Selector@ for @readAttributeSupportedModesWithClusterStateCache:endpoint:queue:completion:@
readAttributeSupportedModesWithClusterStateCache_endpoint_queue_completionSelector :: Selector '[Id MTRClusterStateCacheContainer, Id NSNumber, Id NSObject, Ptr ()] ()
readAttributeSupportedModesWithClusterStateCache_endpoint_queue_completionSelector = mkSelector "readAttributeSupportedModesWithClusterStateCache:endpoint:queue:completion:"

-- | @Selector@ for @readAttributeCurrentModeWithCompletion:@
readAttributeCurrentModeWithCompletionSelector :: Selector '[Ptr ()] ()
readAttributeCurrentModeWithCompletionSelector = mkSelector "readAttributeCurrentModeWithCompletion:"

-- | @Selector@ for @subscribeAttributeCurrentModeWithParams:subscriptionEstablished:reportHandler:@
subscribeAttributeCurrentModeWithParams_subscriptionEstablished_reportHandlerSelector :: Selector '[Id MTRSubscribeParams, Ptr (), Ptr ()] ()
subscribeAttributeCurrentModeWithParams_subscriptionEstablished_reportHandlerSelector = mkSelector "subscribeAttributeCurrentModeWithParams:subscriptionEstablished:reportHandler:"

-- | @Selector@ for @readAttributeCurrentModeWithClusterStateCache:endpoint:queue:completion:@
readAttributeCurrentModeWithClusterStateCache_endpoint_queue_completionSelector :: Selector '[Id MTRClusterStateCacheContainer, Id NSNumber, Id NSObject, Ptr ()] ()
readAttributeCurrentModeWithClusterStateCache_endpoint_queue_completionSelector = mkSelector "readAttributeCurrentModeWithClusterStateCache:endpoint:queue:completion:"

-- | @Selector@ for @readAttributeStartUpModeWithCompletion:@
readAttributeStartUpModeWithCompletionSelector :: Selector '[Ptr ()] ()
readAttributeStartUpModeWithCompletionSelector = mkSelector "readAttributeStartUpModeWithCompletion:"

-- | @Selector@ for @writeAttributeStartUpModeWithValue:completion:@
writeAttributeStartUpModeWithValue_completionSelector :: Selector '[Id NSNumber, Ptr ()] ()
writeAttributeStartUpModeWithValue_completionSelector = mkSelector "writeAttributeStartUpModeWithValue:completion:"

-- | @Selector@ for @writeAttributeStartUpModeWithValue:params:completion:@
writeAttributeStartUpModeWithValue_params_completionSelector :: Selector '[Id NSNumber, Id MTRWriteParams, Ptr ()] ()
writeAttributeStartUpModeWithValue_params_completionSelector = mkSelector "writeAttributeStartUpModeWithValue:params:completion:"

-- | @Selector@ for @subscribeAttributeStartUpModeWithParams:subscriptionEstablished:reportHandler:@
subscribeAttributeStartUpModeWithParams_subscriptionEstablished_reportHandlerSelector :: Selector '[Id MTRSubscribeParams, Ptr (), Ptr ()] ()
subscribeAttributeStartUpModeWithParams_subscriptionEstablished_reportHandlerSelector = mkSelector "subscribeAttributeStartUpModeWithParams:subscriptionEstablished:reportHandler:"

-- | @Selector@ for @readAttributeStartUpModeWithClusterStateCache:endpoint:queue:completion:@
readAttributeStartUpModeWithClusterStateCache_endpoint_queue_completionSelector :: Selector '[Id MTRClusterStateCacheContainer, Id NSNumber, Id NSObject, Ptr ()] ()
readAttributeStartUpModeWithClusterStateCache_endpoint_queue_completionSelector = mkSelector "readAttributeStartUpModeWithClusterStateCache:endpoint:queue:completion:"

-- | @Selector@ for @readAttributeOnModeWithCompletion:@
readAttributeOnModeWithCompletionSelector :: Selector '[Ptr ()] ()
readAttributeOnModeWithCompletionSelector = mkSelector "readAttributeOnModeWithCompletion:"

-- | @Selector@ for @writeAttributeOnModeWithValue:completion:@
writeAttributeOnModeWithValue_completionSelector :: Selector '[Id NSNumber, Ptr ()] ()
writeAttributeOnModeWithValue_completionSelector = mkSelector "writeAttributeOnModeWithValue:completion:"

-- | @Selector@ for @writeAttributeOnModeWithValue:params:completion:@
writeAttributeOnModeWithValue_params_completionSelector :: Selector '[Id NSNumber, Id MTRWriteParams, Ptr ()] ()
writeAttributeOnModeWithValue_params_completionSelector = mkSelector "writeAttributeOnModeWithValue:params:completion:"

-- | @Selector@ for @subscribeAttributeOnModeWithParams:subscriptionEstablished:reportHandler:@
subscribeAttributeOnModeWithParams_subscriptionEstablished_reportHandlerSelector :: Selector '[Id MTRSubscribeParams, Ptr (), Ptr ()] ()
subscribeAttributeOnModeWithParams_subscriptionEstablished_reportHandlerSelector = mkSelector "subscribeAttributeOnModeWithParams:subscriptionEstablished:reportHandler:"

-- | @Selector@ for @readAttributeOnModeWithClusterStateCache:endpoint:queue:completion:@
readAttributeOnModeWithClusterStateCache_endpoint_queue_completionSelector :: Selector '[Id MTRClusterStateCacheContainer, Id NSNumber, Id NSObject, Ptr ()] ()
readAttributeOnModeWithClusterStateCache_endpoint_queue_completionSelector = mkSelector "readAttributeOnModeWithClusterStateCache:endpoint:queue:completion:"

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
initSelector :: Selector '[] (Id MTRBaseClusterModeSelect)
initSelector = mkSelector "init"

-- | @Selector@ for @new@
newSelector :: Selector '[] (Id MTRBaseClusterModeSelect)
newSelector = mkSelector "new"

-- | @Selector@ for @initWithDevice:endpoint:queue:@
initWithDevice_endpoint_queueSelector :: Selector '[Id MTRBaseDevice, CUShort, Id NSObject] (Id MTRBaseClusterModeSelect)
initWithDevice_endpoint_queueSelector = mkSelector "initWithDevice:endpoint:queue:"

-- | @Selector@ for @changeToModeWithParams:completionHandler:@
changeToModeWithParams_completionHandlerSelector :: Selector '[Id MTRModeSelectClusterChangeToModeParams, Ptr ()] ()
changeToModeWithParams_completionHandlerSelector = mkSelector "changeToModeWithParams:completionHandler:"

-- | @Selector@ for @readAttributeDescriptionWithCompletionHandler:@
readAttributeDescriptionWithCompletionHandlerSelector :: Selector '[Ptr ()] ()
readAttributeDescriptionWithCompletionHandlerSelector = mkSelector "readAttributeDescriptionWithCompletionHandler:"

-- | @Selector@ for @subscribeAttributeDescriptionWithMinInterval:maxInterval:params:subscriptionEstablished:reportHandler:@
subscribeAttributeDescriptionWithMinInterval_maxInterval_params_subscriptionEstablished_reportHandlerSelector :: Selector '[Id NSNumber, Id NSNumber, Id MTRSubscribeParams, Ptr (), Ptr ()] ()
subscribeAttributeDescriptionWithMinInterval_maxInterval_params_subscriptionEstablished_reportHandlerSelector = mkSelector "subscribeAttributeDescriptionWithMinInterval:maxInterval:params:subscriptionEstablished:reportHandler:"

-- | @Selector@ for @readAttributeDescriptionWithAttributeCache:endpoint:queue:completionHandler:@
readAttributeDescriptionWithAttributeCache_endpoint_queue_completionHandlerSelector :: Selector '[Id MTRAttributeCacheContainer, Id NSNumber, Id NSObject, Ptr ()] ()
readAttributeDescriptionWithAttributeCache_endpoint_queue_completionHandlerSelector = mkSelector "readAttributeDescriptionWithAttributeCache:endpoint:queue:completionHandler:"

-- | @Selector@ for @readAttributeStandardNamespaceWithCompletionHandler:@
readAttributeStandardNamespaceWithCompletionHandlerSelector :: Selector '[Ptr ()] ()
readAttributeStandardNamespaceWithCompletionHandlerSelector = mkSelector "readAttributeStandardNamespaceWithCompletionHandler:"

-- | @Selector@ for @subscribeAttributeStandardNamespaceWithMinInterval:maxInterval:params:subscriptionEstablished:reportHandler:@
subscribeAttributeStandardNamespaceWithMinInterval_maxInterval_params_subscriptionEstablished_reportHandlerSelector :: Selector '[Id NSNumber, Id NSNumber, Id MTRSubscribeParams, Ptr (), Ptr ()] ()
subscribeAttributeStandardNamespaceWithMinInterval_maxInterval_params_subscriptionEstablished_reportHandlerSelector = mkSelector "subscribeAttributeStandardNamespaceWithMinInterval:maxInterval:params:subscriptionEstablished:reportHandler:"

-- | @Selector@ for @readAttributeStandardNamespaceWithAttributeCache:endpoint:queue:completionHandler:@
readAttributeStandardNamespaceWithAttributeCache_endpoint_queue_completionHandlerSelector :: Selector '[Id MTRAttributeCacheContainer, Id NSNumber, Id NSObject, Ptr ()] ()
readAttributeStandardNamespaceWithAttributeCache_endpoint_queue_completionHandlerSelector = mkSelector "readAttributeStandardNamespaceWithAttributeCache:endpoint:queue:completionHandler:"

-- | @Selector@ for @readAttributeSupportedModesWithCompletionHandler:@
readAttributeSupportedModesWithCompletionHandlerSelector :: Selector '[Ptr ()] ()
readAttributeSupportedModesWithCompletionHandlerSelector = mkSelector "readAttributeSupportedModesWithCompletionHandler:"

-- | @Selector@ for @subscribeAttributeSupportedModesWithMinInterval:maxInterval:params:subscriptionEstablished:reportHandler:@
subscribeAttributeSupportedModesWithMinInterval_maxInterval_params_subscriptionEstablished_reportHandlerSelector :: Selector '[Id NSNumber, Id NSNumber, Id MTRSubscribeParams, Ptr (), Ptr ()] ()
subscribeAttributeSupportedModesWithMinInterval_maxInterval_params_subscriptionEstablished_reportHandlerSelector = mkSelector "subscribeAttributeSupportedModesWithMinInterval:maxInterval:params:subscriptionEstablished:reportHandler:"

-- | @Selector@ for @readAttributeSupportedModesWithAttributeCache:endpoint:queue:completionHandler:@
readAttributeSupportedModesWithAttributeCache_endpoint_queue_completionHandlerSelector :: Selector '[Id MTRAttributeCacheContainer, Id NSNumber, Id NSObject, Ptr ()] ()
readAttributeSupportedModesWithAttributeCache_endpoint_queue_completionHandlerSelector = mkSelector "readAttributeSupportedModesWithAttributeCache:endpoint:queue:completionHandler:"

-- | @Selector@ for @readAttributeCurrentModeWithCompletionHandler:@
readAttributeCurrentModeWithCompletionHandlerSelector :: Selector '[Ptr ()] ()
readAttributeCurrentModeWithCompletionHandlerSelector = mkSelector "readAttributeCurrentModeWithCompletionHandler:"

-- | @Selector@ for @subscribeAttributeCurrentModeWithMinInterval:maxInterval:params:subscriptionEstablished:reportHandler:@
subscribeAttributeCurrentModeWithMinInterval_maxInterval_params_subscriptionEstablished_reportHandlerSelector :: Selector '[Id NSNumber, Id NSNumber, Id MTRSubscribeParams, Ptr (), Ptr ()] ()
subscribeAttributeCurrentModeWithMinInterval_maxInterval_params_subscriptionEstablished_reportHandlerSelector = mkSelector "subscribeAttributeCurrentModeWithMinInterval:maxInterval:params:subscriptionEstablished:reportHandler:"

-- | @Selector@ for @readAttributeCurrentModeWithAttributeCache:endpoint:queue:completionHandler:@
readAttributeCurrentModeWithAttributeCache_endpoint_queue_completionHandlerSelector :: Selector '[Id MTRAttributeCacheContainer, Id NSNumber, Id NSObject, Ptr ()] ()
readAttributeCurrentModeWithAttributeCache_endpoint_queue_completionHandlerSelector = mkSelector "readAttributeCurrentModeWithAttributeCache:endpoint:queue:completionHandler:"

-- | @Selector@ for @readAttributeStartUpModeWithCompletionHandler:@
readAttributeStartUpModeWithCompletionHandlerSelector :: Selector '[Ptr ()] ()
readAttributeStartUpModeWithCompletionHandlerSelector = mkSelector "readAttributeStartUpModeWithCompletionHandler:"

-- | @Selector@ for @writeAttributeStartUpModeWithValue:completionHandler:@
writeAttributeStartUpModeWithValue_completionHandlerSelector :: Selector '[Id NSNumber, Ptr ()] ()
writeAttributeStartUpModeWithValue_completionHandlerSelector = mkSelector "writeAttributeStartUpModeWithValue:completionHandler:"

-- | @Selector@ for @writeAttributeStartUpModeWithValue:params:completionHandler:@
writeAttributeStartUpModeWithValue_params_completionHandlerSelector :: Selector '[Id NSNumber, Id MTRWriteParams, Ptr ()] ()
writeAttributeStartUpModeWithValue_params_completionHandlerSelector = mkSelector "writeAttributeStartUpModeWithValue:params:completionHandler:"

-- | @Selector@ for @subscribeAttributeStartUpModeWithMinInterval:maxInterval:params:subscriptionEstablished:reportHandler:@
subscribeAttributeStartUpModeWithMinInterval_maxInterval_params_subscriptionEstablished_reportHandlerSelector :: Selector '[Id NSNumber, Id NSNumber, Id MTRSubscribeParams, Ptr (), Ptr ()] ()
subscribeAttributeStartUpModeWithMinInterval_maxInterval_params_subscriptionEstablished_reportHandlerSelector = mkSelector "subscribeAttributeStartUpModeWithMinInterval:maxInterval:params:subscriptionEstablished:reportHandler:"

-- | @Selector@ for @readAttributeStartUpModeWithAttributeCache:endpoint:queue:completionHandler:@
readAttributeStartUpModeWithAttributeCache_endpoint_queue_completionHandlerSelector :: Selector '[Id MTRAttributeCacheContainer, Id NSNumber, Id NSObject, Ptr ()] ()
readAttributeStartUpModeWithAttributeCache_endpoint_queue_completionHandlerSelector = mkSelector "readAttributeStartUpModeWithAttributeCache:endpoint:queue:completionHandler:"

-- | @Selector@ for @readAttributeOnModeWithCompletionHandler:@
readAttributeOnModeWithCompletionHandlerSelector :: Selector '[Ptr ()] ()
readAttributeOnModeWithCompletionHandlerSelector = mkSelector "readAttributeOnModeWithCompletionHandler:"

-- | @Selector@ for @writeAttributeOnModeWithValue:completionHandler:@
writeAttributeOnModeWithValue_completionHandlerSelector :: Selector '[Id NSNumber, Ptr ()] ()
writeAttributeOnModeWithValue_completionHandlerSelector = mkSelector "writeAttributeOnModeWithValue:completionHandler:"

-- | @Selector@ for @writeAttributeOnModeWithValue:params:completionHandler:@
writeAttributeOnModeWithValue_params_completionHandlerSelector :: Selector '[Id NSNumber, Id MTRWriteParams, Ptr ()] ()
writeAttributeOnModeWithValue_params_completionHandlerSelector = mkSelector "writeAttributeOnModeWithValue:params:completionHandler:"

-- | @Selector@ for @subscribeAttributeOnModeWithMinInterval:maxInterval:params:subscriptionEstablished:reportHandler:@
subscribeAttributeOnModeWithMinInterval_maxInterval_params_subscriptionEstablished_reportHandlerSelector :: Selector '[Id NSNumber, Id NSNumber, Id MTRSubscribeParams, Ptr (), Ptr ()] ()
subscribeAttributeOnModeWithMinInterval_maxInterval_params_subscriptionEstablished_reportHandlerSelector = mkSelector "subscribeAttributeOnModeWithMinInterval:maxInterval:params:subscriptionEstablished:reportHandler:"

-- | @Selector@ for @readAttributeOnModeWithAttributeCache:endpoint:queue:completionHandler:@
readAttributeOnModeWithAttributeCache_endpoint_queue_completionHandlerSelector :: Selector '[Id MTRAttributeCacheContainer, Id NSNumber, Id NSObject, Ptr ()] ()
readAttributeOnModeWithAttributeCache_endpoint_queue_completionHandlerSelector = mkSelector "readAttributeOnModeWithAttributeCache:endpoint:queue:completionHandler:"

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
initWithDevice_endpointID_queueSelector :: Selector '[Id MTRBaseDevice, Id NSNumber, Id NSObject] (Id MTRBaseClusterModeSelect)
initWithDevice_endpointID_queueSelector = mkSelector "initWithDevice:endpointID:queue:"

