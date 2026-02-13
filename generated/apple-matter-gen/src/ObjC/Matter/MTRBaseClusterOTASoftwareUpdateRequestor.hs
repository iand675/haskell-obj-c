{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Cluster OTA Software Update Requestor
--
-- Provides an interface for downloading and applying OTA software updates
--
-- Generated bindings for @MTRBaseClusterOTASoftwareUpdateRequestor@.
module ObjC.Matter.MTRBaseClusterOTASoftwareUpdateRequestor
  ( MTRBaseClusterOTASoftwareUpdateRequestor
  , IsMTRBaseClusterOTASoftwareUpdateRequestor(..)
  , announceOTAProviderWithParams_completion
  , readAttributeDefaultOTAProvidersWithParams_completion
  , writeAttributeDefaultOTAProvidersWithValue_completion
  , writeAttributeDefaultOTAProvidersWithValue_params_completion
  , subscribeAttributeDefaultOTAProvidersWithParams_subscriptionEstablished_reportHandler
  , readAttributeDefaultOTAProvidersWithClusterStateCache_endpoint_queue_completion
  , readAttributeUpdatePossibleWithCompletion
  , subscribeAttributeUpdatePossibleWithParams_subscriptionEstablished_reportHandler
  , readAttributeUpdatePossibleWithClusterStateCache_endpoint_queue_completion
  , readAttributeUpdateStateWithCompletion
  , subscribeAttributeUpdateStateWithParams_subscriptionEstablished_reportHandler
  , readAttributeUpdateStateWithClusterStateCache_endpoint_queue_completion
  , readAttributeUpdateStateProgressWithCompletion
  , subscribeAttributeUpdateStateProgressWithParams_subscriptionEstablished_reportHandler
  , readAttributeUpdateStateProgressWithClusterStateCache_endpoint_queue_completion
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
  , announceOTAProviderWithParams_completionSelector
  , initSelector
  , initWithDevice_endpointID_queueSelector
  , newSelector
  , readAttributeAcceptedCommandListWithClusterStateCache_endpoint_queue_completionSelector
  , readAttributeAcceptedCommandListWithCompletionSelector
  , readAttributeAttributeListWithClusterStateCache_endpoint_queue_completionSelector
  , readAttributeAttributeListWithCompletionSelector
  , readAttributeClusterRevisionWithClusterStateCache_endpoint_queue_completionSelector
  , readAttributeClusterRevisionWithCompletionSelector
  , readAttributeDefaultOTAProvidersWithClusterStateCache_endpoint_queue_completionSelector
  , readAttributeDefaultOTAProvidersWithParams_completionSelector
  , readAttributeFeatureMapWithClusterStateCache_endpoint_queue_completionSelector
  , readAttributeFeatureMapWithCompletionSelector
  , readAttributeGeneratedCommandListWithClusterStateCache_endpoint_queue_completionSelector
  , readAttributeGeneratedCommandListWithCompletionSelector
  , readAttributeUpdatePossibleWithClusterStateCache_endpoint_queue_completionSelector
  , readAttributeUpdatePossibleWithCompletionSelector
  , readAttributeUpdateStateProgressWithClusterStateCache_endpoint_queue_completionSelector
  , readAttributeUpdateStateProgressWithCompletionSelector
  , readAttributeUpdateStateWithClusterStateCache_endpoint_queue_completionSelector
  , readAttributeUpdateStateWithCompletionSelector
  , subscribeAttributeAcceptedCommandListWithParams_subscriptionEstablished_reportHandlerSelector
  , subscribeAttributeAttributeListWithParams_subscriptionEstablished_reportHandlerSelector
  , subscribeAttributeClusterRevisionWithParams_subscriptionEstablished_reportHandlerSelector
  , subscribeAttributeDefaultOTAProvidersWithParams_subscriptionEstablished_reportHandlerSelector
  , subscribeAttributeFeatureMapWithParams_subscriptionEstablished_reportHandlerSelector
  , subscribeAttributeGeneratedCommandListWithParams_subscriptionEstablished_reportHandlerSelector
  , subscribeAttributeUpdatePossibleWithParams_subscriptionEstablished_reportHandlerSelector
  , subscribeAttributeUpdateStateProgressWithParams_subscriptionEstablished_reportHandlerSelector
  , subscribeAttributeUpdateStateWithParams_subscriptionEstablished_reportHandlerSelector
  , writeAttributeDefaultOTAProvidersWithValue_completionSelector
  , writeAttributeDefaultOTAProvidersWithValue_params_completionSelector


  ) where

import Foreign.Ptr (Ptr, FunPtr)
import Foreign.C.Types

import ObjC.Runtime.Types
import ObjC.Runtime.Message (sendMessage, sendOwnedMessage, sendClassMessage, sendOwnedClassMessage)
import ObjC.Runtime.Selector (mkSelector)
import ObjC.Runtime.Class (getRequiredClass)

import ObjC.Matter.Internal.Classes
import ObjC.Foundation.Internal.Classes

-- | Command AnnounceOTAProvider
--
-- Announce the presence of an OTA Provider
--
-- ObjC selector: @- announceOTAProviderWithParams:completion:@
announceOTAProviderWithParams_completion :: (IsMTRBaseClusterOTASoftwareUpdateRequestor mtrBaseClusterOTASoftwareUpdateRequestor, IsMTROTASoftwareUpdateRequestorClusterAnnounceOTAProviderParams params) => mtrBaseClusterOTASoftwareUpdateRequestor -> params -> Ptr () -> IO ()
announceOTAProviderWithParams_completion mtrBaseClusterOTASoftwareUpdateRequestor params completion =
  sendMessage mtrBaseClusterOTASoftwareUpdateRequestor announceOTAProviderWithParams_completionSelector (toMTROTASoftwareUpdateRequestorClusterAnnounceOTAProviderParams params) completion

-- | @- readAttributeDefaultOTAProvidersWithParams:completion:@
readAttributeDefaultOTAProvidersWithParams_completion :: (IsMTRBaseClusterOTASoftwareUpdateRequestor mtrBaseClusterOTASoftwareUpdateRequestor, IsMTRReadParams params) => mtrBaseClusterOTASoftwareUpdateRequestor -> params -> Ptr () -> IO ()
readAttributeDefaultOTAProvidersWithParams_completion mtrBaseClusterOTASoftwareUpdateRequestor params completion =
  sendMessage mtrBaseClusterOTASoftwareUpdateRequestor readAttributeDefaultOTAProvidersWithParams_completionSelector (toMTRReadParams params) completion

-- | @- writeAttributeDefaultOTAProvidersWithValue:completion:@
writeAttributeDefaultOTAProvidersWithValue_completion :: (IsMTRBaseClusterOTASoftwareUpdateRequestor mtrBaseClusterOTASoftwareUpdateRequestor, IsNSArray value) => mtrBaseClusterOTASoftwareUpdateRequestor -> value -> Ptr () -> IO ()
writeAttributeDefaultOTAProvidersWithValue_completion mtrBaseClusterOTASoftwareUpdateRequestor value completion =
  sendMessage mtrBaseClusterOTASoftwareUpdateRequestor writeAttributeDefaultOTAProvidersWithValue_completionSelector (toNSArray value) completion

-- | @- writeAttributeDefaultOTAProvidersWithValue:params:completion:@
writeAttributeDefaultOTAProvidersWithValue_params_completion :: (IsMTRBaseClusterOTASoftwareUpdateRequestor mtrBaseClusterOTASoftwareUpdateRequestor, IsNSArray value, IsMTRWriteParams params) => mtrBaseClusterOTASoftwareUpdateRequestor -> value -> params -> Ptr () -> IO ()
writeAttributeDefaultOTAProvidersWithValue_params_completion mtrBaseClusterOTASoftwareUpdateRequestor value params completion =
  sendMessage mtrBaseClusterOTASoftwareUpdateRequestor writeAttributeDefaultOTAProvidersWithValue_params_completionSelector (toNSArray value) (toMTRWriteParams params) completion

-- | @- subscribeAttributeDefaultOTAProvidersWithParams:subscriptionEstablished:reportHandler:@
subscribeAttributeDefaultOTAProvidersWithParams_subscriptionEstablished_reportHandler :: (IsMTRBaseClusterOTASoftwareUpdateRequestor mtrBaseClusterOTASoftwareUpdateRequestor, IsMTRSubscribeParams params) => mtrBaseClusterOTASoftwareUpdateRequestor -> params -> Ptr () -> Ptr () -> IO ()
subscribeAttributeDefaultOTAProvidersWithParams_subscriptionEstablished_reportHandler mtrBaseClusterOTASoftwareUpdateRequestor params subscriptionEstablished reportHandler =
  sendMessage mtrBaseClusterOTASoftwareUpdateRequestor subscribeAttributeDefaultOTAProvidersWithParams_subscriptionEstablished_reportHandlerSelector (toMTRSubscribeParams params) subscriptionEstablished reportHandler

-- | @+ readAttributeDefaultOTAProvidersWithClusterStateCache:endpoint:queue:completion:@
readAttributeDefaultOTAProvidersWithClusterStateCache_endpoint_queue_completion :: (IsMTRClusterStateCacheContainer clusterStateCacheContainer, IsNSNumber endpoint, IsNSObject queue) => clusterStateCacheContainer -> endpoint -> queue -> Ptr () -> IO ()
readAttributeDefaultOTAProvidersWithClusterStateCache_endpoint_queue_completion clusterStateCacheContainer endpoint queue completion =
  do
    cls' <- getRequiredClass "MTRBaseClusterOTASoftwareUpdateRequestor"
    sendClassMessage cls' readAttributeDefaultOTAProvidersWithClusterStateCache_endpoint_queue_completionSelector (toMTRClusterStateCacheContainer clusterStateCacheContainer) (toNSNumber endpoint) (toNSObject queue) completion

-- | @- readAttributeUpdatePossibleWithCompletion:@
readAttributeUpdatePossibleWithCompletion :: IsMTRBaseClusterOTASoftwareUpdateRequestor mtrBaseClusterOTASoftwareUpdateRequestor => mtrBaseClusterOTASoftwareUpdateRequestor -> Ptr () -> IO ()
readAttributeUpdatePossibleWithCompletion mtrBaseClusterOTASoftwareUpdateRequestor completion =
  sendMessage mtrBaseClusterOTASoftwareUpdateRequestor readAttributeUpdatePossibleWithCompletionSelector completion

-- | @- subscribeAttributeUpdatePossibleWithParams:subscriptionEstablished:reportHandler:@
subscribeAttributeUpdatePossibleWithParams_subscriptionEstablished_reportHandler :: (IsMTRBaseClusterOTASoftwareUpdateRequestor mtrBaseClusterOTASoftwareUpdateRequestor, IsMTRSubscribeParams params) => mtrBaseClusterOTASoftwareUpdateRequestor -> params -> Ptr () -> Ptr () -> IO ()
subscribeAttributeUpdatePossibleWithParams_subscriptionEstablished_reportHandler mtrBaseClusterOTASoftwareUpdateRequestor params subscriptionEstablished reportHandler =
  sendMessage mtrBaseClusterOTASoftwareUpdateRequestor subscribeAttributeUpdatePossibleWithParams_subscriptionEstablished_reportHandlerSelector (toMTRSubscribeParams params) subscriptionEstablished reportHandler

-- | @+ readAttributeUpdatePossibleWithClusterStateCache:endpoint:queue:completion:@
readAttributeUpdatePossibleWithClusterStateCache_endpoint_queue_completion :: (IsMTRClusterStateCacheContainer clusterStateCacheContainer, IsNSNumber endpoint, IsNSObject queue) => clusterStateCacheContainer -> endpoint -> queue -> Ptr () -> IO ()
readAttributeUpdatePossibleWithClusterStateCache_endpoint_queue_completion clusterStateCacheContainer endpoint queue completion =
  do
    cls' <- getRequiredClass "MTRBaseClusterOTASoftwareUpdateRequestor"
    sendClassMessage cls' readAttributeUpdatePossibleWithClusterStateCache_endpoint_queue_completionSelector (toMTRClusterStateCacheContainer clusterStateCacheContainer) (toNSNumber endpoint) (toNSObject queue) completion

-- | @- readAttributeUpdateStateWithCompletion:@
readAttributeUpdateStateWithCompletion :: IsMTRBaseClusterOTASoftwareUpdateRequestor mtrBaseClusterOTASoftwareUpdateRequestor => mtrBaseClusterOTASoftwareUpdateRequestor -> Ptr () -> IO ()
readAttributeUpdateStateWithCompletion mtrBaseClusterOTASoftwareUpdateRequestor completion =
  sendMessage mtrBaseClusterOTASoftwareUpdateRequestor readAttributeUpdateStateWithCompletionSelector completion

-- | @- subscribeAttributeUpdateStateWithParams:subscriptionEstablished:reportHandler:@
subscribeAttributeUpdateStateWithParams_subscriptionEstablished_reportHandler :: (IsMTRBaseClusterOTASoftwareUpdateRequestor mtrBaseClusterOTASoftwareUpdateRequestor, IsMTRSubscribeParams params) => mtrBaseClusterOTASoftwareUpdateRequestor -> params -> Ptr () -> Ptr () -> IO ()
subscribeAttributeUpdateStateWithParams_subscriptionEstablished_reportHandler mtrBaseClusterOTASoftwareUpdateRequestor params subscriptionEstablished reportHandler =
  sendMessage mtrBaseClusterOTASoftwareUpdateRequestor subscribeAttributeUpdateStateWithParams_subscriptionEstablished_reportHandlerSelector (toMTRSubscribeParams params) subscriptionEstablished reportHandler

-- | @+ readAttributeUpdateStateWithClusterStateCache:endpoint:queue:completion:@
readAttributeUpdateStateWithClusterStateCache_endpoint_queue_completion :: (IsMTRClusterStateCacheContainer clusterStateCacheContainer, IsNSNumber endpoint, IsNSObject queue) => clusterStateCacheContainer -> endpoint -> queue -> Ptr () -> IO ()
readAttributeUpdateStateWithClusterStateCache_endpoint_queue_completion clusterStateCacheContainer endpoint queue completion =
  do
    cls' <- getRequiredClass "MTRBaseClusterOTASoftwareUpdateRequestor"
    sendClassMessage cls' readAttributeUpdateStateWithClusterStateCache_endpoint_queue_completionSelector (toMTRClusterStateCacheContainer clusterStateCacheContainer) (toNSNumber endpoint) (toNSObject queue) completion

-- | @- readAttributeUpdateStateProgressWithCompletion:@
readAttributeUpdateStateProgressWithCompletion :: IsMTRBaseClusterOTASoftwareUpdateRequestor mtrBaseClusterOTASoftwareUpdateRequestor => mtrBaseClusterOTASoftwareUpdateRequestor -> Ptr () -> IO ()
readAttributeUpdateStateProgressWithCompletion mtrBaseClusterOTASoftwareUpdateRequestor completion =
  sendMessage mtrBaseClusterOTASoftwareUpdateRequestor readAttributeUpdateStateProgressWithCompletionSelector completion

-- | @- subscribeAttributeUpdateStateProgressWithParams:subscriptionEstablished:reportHandler:@
subscribeAttributeUpdateStateProgressWithParams_subscriptionEstablished_reportHandler :: (IsMTRBaseClusterOTASoftwareUpdateRequestor mtrBaseClusterOTASoftwareUpdateRequestor, IsMTRSubscribeParams params) => mtrBaseClusterOTASoftwareUpdateRequestor -> params -> Ptr () -> Ptr () -> IO ()
subscribeAttributeUpdateStateProgressWithParams_subscriptionEstablished_reportHandler mtrBaseClusterOTASoftwareUpdateRequestor params subscriptionEstablished reportHandler =
  sendMessage mtrBaseClusterOTASoftwareUpdateRequestor subscribeAttributeUpdateStateProgressWithParams_subscriptionEstablished_reportHandlerSelector (toMTRSubscribeParams params) subscriptionEstablished reportHandler

-- | @+ readAttributeUpdateStateProgressWithClusterStateCache:endpoint:queue:completion:@
readAttributeUpdateStateProgressWithClusterStateCache_endpoint_queue_completion :: (IsMTRClusterStateCacheContainer clusterStateCacheContainer, IsNSNumber endpoint, IsNSObject queue) => clusterStateCacheContainer -> endpoint -> queue -> Ptr () -> IO ()
readAttributeUpdateStateProgressWithClusterStateCache_endpoint_queue_completion clusterStateCacheContainer endpoint queue completion =
  do
    cls' <- getRequiredClass "MTRBaseClusterOTASoftwareUpdateRequestor"
    sendClassMessage cls' readAttributeUpdateStateProgressWithClusterStateCache_endpoint_queue_completionSelector (toMTRClusterStateCacheContainer clusterStateCacheContainer) (toNSNumber endpoint) (toNSObject queue) completion

-- | @- readAttributeGeneratedCommandListWithCompletion:@
readAttributeGeneratedCommandListWithCompletion :: IsMTRBaseClusterOTASoftwareUpdateRequestor mtrBaseClusterOTASoftwareUpdateRequestor => mtrBaseClusterOTASoftwareUpdateRequestor -> Ptr () -> IO ()
readAttributeGeneratedCommandListWithCompletion mtrBaseClusterOTASoftwareUpdateRequestor completion =
  sendMessage mtrBaseClusterOTASoftwareUpdateRequestor readAttributeGeneratedCommandListWithCompletionSelector completion

-- | @- subscribeAttributeGeneratedCommandListWithParams:subscriptionEstablished:reportHandler:@
subscribeAttributeGeneratedCommandListWithParams_subscriptionEstablished_reportHandler :: (IsMTRBaseClusterOTASoftwareUpdateRequestor mtrBaseClusterOTASoftwareUpdateRequestor, IsMTRSubscribeParams params) => mtrBaseClusterOTASoftwareUpdateRequestor -> params -> Ptr () -> Ptr () -> IO ()
subscribeAttributeGeneratedCommandListWithParams_subscriptionEstablished_reportHandler mtrBaseClusterOTASoftwareUpdateRequestor params subscriptionEstablished reportHandler =
  sendMessage mtrBaseClusterOTASoftwareUpdateRequestor subscribeAttributeGeneratedCommandListWithParams_subscriptionEstablished_reportHandlerSelector (toMTRSubscribeParams params) subscriptionEstablished reportHandler

-- | @+ readAttributeGeneratedCommandListWithClusterStateCache:endpoint:queue:completion:@
readAttributeGeneratedCommandListWithClusterStateCache_endpoint_queue_completion :: (IsMTRClusterStateCacheContainer clusterStateCacheContainer, IsNSNumber endpoint, IsNSObject queue) => clusterStateCacheContainer -> endpoint -> queue -> Ptr () -> IO ()
readAttributeGeneratedCommandListWithClusterStateCache_endpoint_queue_completion clusterStateCacheContainer endpoint queue completion =
  do
    cls' <- getRequiredClass "MTRBaseClusterOTASoftwareUpdateRequestor"
    sendClassMessage cls' readAttributeGeneratedCommandListWithClusterStateCache_endpoint_queue_completionSelector (toMTRClusterStateCacheContainer clusterStateCacheContainer) (toNSNumber endpoint) (toNSObject queue) completion

-- | @- readAttributeAcceptedCommandListWithCompletion:@
readAttributeAcceptedCommandListWithCompletion :: IsMTRBaseClusterOTASoftwareUpdateRequestor mtrBaseClusterOTASoftwareUpdateRequestor => mtrBaseClusterOTASoftwareUpdateRequestor -> Ptr () -> IO ()
readAttributeAcceptedCommandListWithCompletion mtrBaseClusterOTASoftwareUpdateRequestor completion =
  sendMessage mtrBaseClusterOTASoftwareUpdateRequestor readAttributeAcceptedCommandListWithCompletionSelector completion

-- | @- subscribeAttributeAcceptedCommandListWithParams:subscriptionEstablished:reportHandler:@
subscribeAttributeAcceptedCommandListWithParams_subscriptionEstablished_reportHandler :: (IsMTRBaseClusterOTASoftwareUpdateRequestor mtrBaseClusterOTASoftwareUpdateRequestor, IsMTRSubscribeParams params) => mtrBaseClusterOTASoftwareUpdateRequestor -> params -> Ptr () -> Ptr () -> IO ()
subscribeAttributeAcceptedCommandListWithParams_subscriptionEstablished_reportHandler mtrBaseClusterOTASoftwareUpdateRequestor params subscriptionEstablished reportHandler =
  sendMessage mtrBaseClusterOTASoftwareUpdateRequestor subscribeAttributeAcceptedCommandListWithParams_subscriptionEstablished_reportHandlerSelector (toMTRSubscribeParams params) subscriptionEstablished reportHandler

-- | @+ readAttributeAcceptedCommandListWithClusterStateCache:endpoint:queue:completion:@
readAttributeAcceptedCommandListWithClusterStateCache_endpoint_queue_completion :: (IsMTRClusterStateCacheContainer clusterStateCacheContainer, IsNSNumber endpoint, IsNSObject queue) => clusterStateCacheContainer -> endpoint -> queue -> Ptr () -> IO ()
readAttributeAcceptedCommandListWithClusterStateCache_endpoint_queue_completion clusterStateCacheContainer endpoint queue completion =
  do
    cls' <- getRequiredClass "MTRBaseClusterOTASoftwareUpdateRequestor"
    sendClassMessage cls' readAttributeAcceptedCommandListWithClusterStateCache_endpoint_queue_completionSelector (toMTRClusterStateCacheContainer clusterStateCacheContainer) (toNSNumber endpoint) (toNSObject queue) completion

-- | @- readAttributeAttributeListWithCompletion:@
readAttributeAttributeListWithCompletion :: IsMTRBaseClusterOTASoftwareUpdateRequestor mtrBaseClusterOTASoftwareUpdateRequestor => mtrBaseClusterOTASoftwareUpdateRequestor -> Ptr () -> IO ()
readAttributeAttributeListWithCompletion mtrBaseClusterOTASoftwareUpdateRequestor completion =
  sendMessage mtrBaseClusterOTASoftwareUpdateRequestor readAttributeAttributeListWithCompletionSelector completion

-- | @- subscribeAttributeAttributeListWithParams:subscriptionEstablished:reportHandler:@
subscribeAttributeAttributeListWithParams_subscriptionEstablished_reportHandler :: (IsMTRBaseClusterOTASoftwareUpdateRequestor mtrBaseClusterOTASoftwareUpdateRequestor, IsMTRSubscribeParams params) => mtrBaseClusterOTASoftwareUpdateRequestor -> params -> Ptr () -> Ptr () -> IO ()
subscribeAttributeAttributeListWithParams_subscriptionEstablished_reportHandler mtrBaseClusterOTASoftwareUpdateRequestor params subscriptionEstablished reportHandler =
  sendMessage mtrBaseClusterOTASoftwareUpdateRequestor subscribeAttributeAttributeListWithParams_subscriptionEstablished_reportHandlerSelector (toMTRSubscribeParams params) subscriptionEstablished reportHandler

-- | @+ readAttributeAttributeListWithClusterStateCache:endpoint:queue:completion:@
readAttributeAttributeListWithClusterStateCache_endpoint_queue_completion :: (IsMTRClusterStateCacheContainer clusterStateCacheContainer, IsNSNumber endpoint, IsNSObject queue) => clusterStateCacheContainer -> endpoint -> queue -> Ptr () -> IO ()
readAttributeAttributeListWithClusterStateCache_endpoint_queue_completion clusterStateCacheContainer endpoint queue completion =
  do
    cls' <- getRequiredClass "MTRBaseClusterOTASoftwareUpdateRequestor"
    sendClassMessage cls' readAttributeAttributeListWithClusterStateCache_endpoint_queue_completionSelector (toMTRClusterStateCacheContainer clusterStateCacheContainer) (toNSNumber endpoint) (toNSObject queue) completion

-- | @- readAttributeFeatureMapWithCompletion:@
readAttributeFeatureMapWithCompletion :: IsMTRBaseClusterOTASoftwareUpdateRequestor mtrBaseClusterOTASoftwareUpdateRequestor => mtrBaseClusterOTASoftwareUpdateRequestor -> Ptr () -> IO ()
readAttributeFeatureMapWithCompletion mtrBaseClusterOTASoftwareUpdateRequestor completion =
  sendMessage mtrBaseClusterOTASoftwareUpdateRequestor readAttributeFeatureMapWithCompletionSelector completion

-- | @- subscribeAttributeFeatureMapWithParams:subscriptionEstablished:reportHandler:@
subscribeAttributeFeatureMapWithParams_subscriptionEstablished_reportHandler :: (IsMTRBaseClusterOTASoftwareUpdateRequestor mtrBaseClusterOTASoftwareUpdateRequestor, IsMTRSubscribeParams params) => mtrBaseClusterOTASoftwareUpdateRequestor -> params -> Ptr () -> Ptr () -> IO ()
subscribeAttributeFeatureMapWithParams_subscriptionEstablished_reportHandler mtrBaseClusterOTASoftwareUpdateRequestor params subscriptionEstablished reportHandler =
  sendMessage mtrBaseClusterOTASoftwareUpdateRequestor subscribeAttributeFeatureMapWithParams_subscriptionEstablished_reportHandlerSelector (toMTRSubscribeParams params) subscriptionEstablished reportHandler

-- | @+ readAttributeFeatureMapWithClusterStateCache:endpoint:queue:completion:@
readAttributeFeatureMapWithClusterStateCache_endpoint_queue_completion :: (IsMTRClusterStateCacheContainer clusterStateCacheContainer, IsNSNumber endpoint, IsNSObject queue) => clusterStateCacheContainer -> endpoint -> queue -> Ptr () -> IO ()
readAttributeFeatureMapWithClusterStateCache_endpoint_queue_completion clusterStateCacheContainer endpoint queue completion =
  do
    cls' <- getRequiredClass "MTRBaseClusterOTASoftwareUpdateRequestor"
    sendClassMessage cls' readAttributeFeatureMapWithClusterStateCache_endpoint_queue_completionSelector (toMTRClusterStateCacheContainer clusterStateCacheContainer) (toNSNumber endpoint) (toNSObject queue) completion

-- | @- readAttributeClusterRevisionWithCompletion:@
readAttributeClusterRevisionWithCompletion :: IsMTRBaseClusterOTASoftwareUpdateRequestor mtrBaseClusterOTASoftwareUpdateRequestor => mtrBaseClusterOTASoftwareUpdateRequestor -> Ptr () -> IO ()
readAttributeClusterRevisionWithCompletion mtrBaseClusterOTASoftwareUpdateRequestor completion =
  sendMessage mtrBaseClusterOTASoftwareUpdateRequestor readAttributeClusterRevisionWithCompletionSelector completion

-- | @- subscribeAttributeClusterRevisionWithParams:subscriptionEstablished:reportHandler:@
subscribeAttributeClusterRevisionWithParams_subscriptionEstablished_reportHandler :: (IsMTRBaseClusterOTASoftwareUpdateRequestor mtrBaseClusterOTASoftwareUpdateRequestor, IsMTRSubscribeParams params) => mtrBaseClusterOTASoftwareUpdateRequestor -> params -> Ptr () -> Ptr () -> IO ()
subscribeAttributeClusterRevisionWithParams_subscriptionEstablished_reportHandler mtrBaseClusterOTASoftwareUpdateRequestor params subscriptionEstablished reportHandler =
  sendMessage mtrBaseClusterOTASoftwareUpdateRequestor subscribeAttributeClusterRevisionWithParams_subscriptionEstablished_reportHandlerSelector (toMTRSubscribeParams params) subscriptionEstablished reportHandler

-- | @+ readAttributeClusterRevisionWithClusterStateCache:endpoint:queue:completion:@
readAttributeClusterRevisionWithClusterStateCache_endpoint_queue_completion :: (IsMTRClusterStateCacheContainer clusterStateCacheContainer, IsNSNumber endpoint, IsNSObject queue) => clusterStateCacheContainer -> endpoint -> queue -> Ptr () -> IO ()
readAttributeClusterRevisionWithClusterStateCache_endpoint_queue_completion clusterStateCacheContainer endpoint queue completion =
  do
    cls' <- getRequiredClass "MTRBaseClusterOTASoftwareUpdateRequestor"
    sendClassMessage cls' readAttributeClusterRevisionWithClusterStateCache_endpoint_queue_completionSelector (toMTRClusterStateCacheContainer clusterStateCacheContainer) (toNSNumber endpoint) (toNSObject queue) completion

-- | @- init@
init_ :: IsMTRBaseClusterOTASoftwareUpdateRequestor mtrBaseClusterOTASoftwareUpdateRequestor => mtrBaseClusterOTASoftwareUpdateRequestor -> IO (Id MTRBaseClusterOTASoftwareUpdateRequestor)
init_ mtrBaseClusterOTASoftwareUpdateRequestor =
  sendOwnedMessage mtrBaseClusterOTASoftwareUpdateRequestor initSelector

-- | @+ new@
new :: IO (Id MTRBaseClusterOTASoftwareUpdateRequestor)
new  =
  do
    cls' <- getRequiredClass "MTRBaseClusterOTASoftwareUpdateRequestor"
    sendOwnedClassMessage cls' newSelector

-- | For all instance methods (reads, writes, commands) that take a completion, the completion will be called on the provided queue.
--
-- ObjC selector: @- initWithDevice:endpointID:queue:@
initWithDevice_endpointID_queue :: (IsMTRBaseClusterOTASoftwareUpdateRequestor mtrBaseClusterOTASoftwareUpdateRequestor, IsMTRBaseDevice device, IsNSNumber endpointID, IsNSObject queue) => mtrBaseClusterOTASoftwareUpdateRequestor -> device -> endpointID -> queue -> IO (Id MTRBaseClusterOTASoftwareUpdateRequestor)
initWithDevice_endpointID_queue mtrBaseClusterOTASoftwareUpdateRequestor device endpointID queue =
  sendOwnedMessage mtrBaseClusterOTASoftwareUpdateRequestor initWithDevice_endpointID_queueSelector (toMTRBaseDevice device) (toNSNumber endpointID) (toNSObject queue)

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @announceOTAProviderWithParams:completion:@
announceOTAProviderWithParams_completionSelector :: Selector '[Id MTROTASoftwareUpdateRequestorClusterAnnounceOTAProviderParams, Ptr ()] ()
announceOTAProviderWithParams_completionSelector = mkSelector "announceOTAProviderWithParams:completion:"

-- | @Selector@ for @readAttributeDefaultOTAProvidersWithParams:completion:@
readAttributeDefaultOTAProvidersWithParams_completionSelector :: Selector '[Id MTRReadParams, Ptr ()] ()
readAttributeDefaultOTAProvidersWithParams_completionSelector = mkSelector "readAttributeDefaultOTAProvidersWithParams:completion:"

-- | @Selector@ for @writeAttributeDefaultOTAProvidersWithValue:completion:@
writeAttributeDefaultOTAProvidersWithValue_completionSelector :: Selector '[Id NSArray, Ptr ()] ()
writeAttributeDefaultOTAProvidersWithValue_completionSelector = mkSelector "writeAttributeDefaultOTAProvidersWithValue:completion:"

-- | @Selector@ for @writeAttributeDefaultOTAProvidersWithValue:params:completion:@
writeAttributeDefaultOTAProvidersWithValue_params_completionSelector :: Selector '[Id NSArray, Id MTRWriteParams, Ptr ()] ()
writeAttributeDefaultOTAProvidersWithValue_params_completionSelector = mkSelector "writeAttributeDefaultOTAProvidersWithValue:params:completion:"

-- | @Selector@ for @subscribeAttributeDefaultOTAProvidersWithParams:subscriptionEstablished:reportHandler:@
subscribeAttributeDefaultOTAProvidersWithParams_subscriptionEstablished_reportHandlerSelector :: Selector '[Id MTRSubscribeParams, Ptr (), Ptr ()] ()
subscribeAttributeDefaultOTAProvidersWithParams_subscriptionEstablished_reportHandlerSelector = mkSelector "subscribeAttributeDefaultOTAProvidersWithParams:subscriptionEstablished:reportHandler:"

-- | @Selector@ for @readAttributeDefaultOTAProvidersWithClusterStateCache:endpoint:queue:completion:@
readAttributeDefaultOTAProvidersWithClusterStateCache_endpoint_queue_completionSelector :: Selector '[Id MTRClusterStateCacheContainer, Id NSNumber, Id NSObject, Ptr ()] ()
readAttributeDefaultOTAProvidersWithClusterStateCache_endpoint_queue_completionSelector = mkSelector "readAttributeDefaultOTAProvidersWithClusterStateCache:endpoint:queue:completion:"

-- | @Selector@ for @readAttributeUpdatePossibleWithCompletion:@
readAttributeUpdatePossibleWithCompletionSelector :: Selector '[Ptr ()] ()
readAttributeUpdatePossibleWithCompletionSelector = mkSelector "readAttributeUpdatePossibleWithCompletion:"

-- | @Selector@ for @subscribeAttributeUpdatePossibleWithParams:subscriptionEstablished:reportHandler:@
subscribeAttributeUpdatePossibleWithParams_subscriptionEstablished_reportHandlerSelector :: Selector '[Id MTRSubscribeParams, Ptr (), Ptr ()] ()
subscribeAttributeUpdatePossibleWithParams_subscriptionEstablished_reportHandlerSelector = mkSelector "subscribeAttributeUpdatePossibleWithParams:subscriptionEstablished:reportHandler:"

-- | @Selector@ for @readAttributeUpdatePossibleWithClusterStateCache:endpoint:queue:completion:@
readAttributeUpdatePossibleWithClusterStateCache_endpoint_queue_completionSelector :: Selector '[Id MTRClusterStateCacheContainer, Id NSNumber, Id NSObject, Ptr ()] ()
readAttributeUpdatePossibleWithClusterStateCache_endpoint_queue_completionSelector = mkSelector "readAttributeUpdatePossibleWithClusterStateCache:endpoint:queue:completion:"

-- | @Selector@ for @readAttributeUpdateStateWithCompletion:@
readAttributeUpdateStateWithCompletionSelector :: Selector '[Ptr ()] ()
readAttributeUpdateStateWithCompletionSelector = mkSelector "readAttributeUpdateStateWithCompletion:"

-- | @Selector@ for @subscribeAttributeUpdateStateWithParams:subscriptionEstablished:reportHandler:@
subscribeAttributeUpdateStateWithParams_subscriptionEstablished_reportHandlerSelector :: Selector '[Id MTRSubscribeParams, Ptr (), Ptr ()] ()
subscribeAttributeUpdateStateWithParams_subscriptionEstablished_reportHandlerSelector = mkSelector "subscribeAttributeUpdateStateWithParams:subscriptionEstablished:reportHandler:"

-- | @Selector@ for @readAttributeUpdateStateWithClusterStateCache:endpoint:queue:completion:@
readAttributeUpdateStateWithClusterStateCache_endpoint_queue_completionSelector :: Selector '[Id MTRClusterStateCacheContainer, Id NSNumber, Id NSObject, Ptr ()] ()
readAttributeUpdateStateWithClusterStateCache_endpoint_queue_completionSelector = mkSelector "readAttributeUpdateStateWithClusterStateCache:endpoint:queue:completion:"

-- | @Selector@ for @readAttributeUpdateStateProgressWithCompletion:@
readAttributeUpdateStateProgressWithCompletionSelector :: Selector '[Ptr ()] ()
readAttributeUpdateStateProgressWithCompletionSelector = mkSelector "readAttributeUpdateStateProgressWithCompletion:"

-- | @Selector@ for @subscribeAttributeUpdateStateProgressWithParams:subscriptionEstablished:reportHandler:@
subscribeAttributeUpdateStateProgressWithParams_subscriptionEstablished_reportHandlerSelector :: Selector '[Id MTRSubscribeParams, Ptr (), Ptr ()] ()
subscribeAttributeUpdateStateProgressWithParams_subscriptionEstablished_reportHandlerSelector = mkSelector "subscribeAttributeUpdateStateProgressWithParams:subscriptionEstablished:reportHandler:"

-- | @Selector@ for @readAttributeUpdateStateProgressWithClusterStateCache:endpoint:queue:completion:@
readAttributeUpdateStateProgressWithClusterStateCache_endpoint_queue_completionSelector :: Selector '[Id MTRClusterStateCacheContainer, Id NSNumber, Id NSObject, Ptr ()] ()
readAttributeUpdateStateProgressWithClusterStateCache_endpoint_queue_completionSelector = mkSelector "readAttributeUpdateStateProgressWithClusterStateCache:endpoint:queue:completion:"

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
initSelector :: Selector '[] (Id MTRBaseClusterOTASoftwareUpdateRequestor)
initSelector = mkSelector "init"

-- | @Selector@ for @new@
newSelector :: Selector '[] (Id MTRBaseClusterOTASoftwareUpdateRequestor)
newSelector = mkSelector "new"

-- | @Selector@ for @initWithDevice:endpointID:queue:@
initWithDevice_endpointID_queueSelector :: Selector '[Id MTRBaseDevice, Id NSNumber, Id NSObject] (Id MTRBaseClusterOTASoftwareUpdateRequestor)
initWithDevice_endpointID_queueSelector = mkSelector "initWithDevice:endpointID:queue:"

