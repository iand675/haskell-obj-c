{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Cluster OTA Software Update Provider
--
-- Provides an interface for providing OTA software updates
--
-- Generated bindings for @MTRBaseClusterOTASoftwareUpdateProvider@.
module ObjC.Matter.MTRBaseClusterOTASoftwareUpdateProvider
  ( MTRBaseClusterOTASoftwareUpdateProvider
  , IsMTRBaseClusterOTASoftwareUpdateProvider(..)
  , queryImageWithParams_completion
  , applyUpdateRequestWithParams_completion
  , notifyUpdateAppliedWithParams_completion
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
  , applyUpdateRequestWithParams_completionSelector
  , initSelector
  , initWithDevice_endpointID_queueSelector
  , newSelector
  , notifyUpdateAppliedWithParams_completionSelector
  , queryImageWithParams_completionSelector
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
  , subscribeAttributeAcceptedCommandListWithParams_subscriptionEstablished_reportHandlerSelector
  , subscribeAttributeAttributeListWithParams_subscriptionEstablished_reportHandlerSelector
  , subscribeAttributeClusterRevisionWithParams_subscriptionEstablished_reportHandlerSelector
  , subscribeAttributeFeatureMapWithParams_subscriptionEstablished_reportHandlerSelector
  , subscribeAttributeGeneratedCommandListWithParams_subscriptionEstablished_reportHandlerSelector


  ) where

import Foreign.Ptr (Ptr, FunPtr)
import Foreign.C.Types

import ObjC.Runtime.Types
import ObjC.Runtime.Message (sendMessage, sendOwnedMessage, sendClassMessage, sendOwnedClassMessage)
import ObjC.Runtime.Selector (mkSelector)
import ObjC.Runtime.Class (getRequiredClass)

import ObjC.Matter.Internal.Classes
import ObjC.Foundation.Internal.Classes

-- | Command QueryImage
--
-- Determine availability of a new Software Image
--
-- ObjC selector: @- queryImageWithParams:completion:@
queryImageWithParams_completion :: (IsMTRBaseClusterOTASoftwareUpdateProvider mtrBaseClusterOTASoftwareUpdateProvider, IsMTROTASoftwareUpdateProviderClusterQueryImageParams params) => mtrBaseClusterOTASoftwareUpdateProvider -> params -> Ptr () -> IO ()
queryImageWithParams_completion mtrBaseClusterOTASoftwareUpdateProvider params completion =
  sendMessage mtrBaseClusterOTASoftwareUpdateProvider queryImageWithParams_completionSelector (toMTROTASoftwareUpdateProviderClusterQueryImageParams params) completion

-- | Command ApplyUpdateRequest
--
-- Determine next action to take for a downloaded Software Image
--
-- ObjC selector: @- applyUpdateRequestWithParams:completion:@
applyUpdateRequestWithParams_completion :: (IsMTRBaseClusterOTASoftwareUpdateProvider mtrBaseClusterOTASoftwareUpdateProvider, IsMTROTASoftwareUpdateProviderClusterApplyUpdateRequestParams params) => mtrBaseClusterOTASoftwareUpdateProvider -> params -> Ptr () -> IO ()
applyUpdateRequestWithParams_completion mtrBaseClusterOTASoftwareUpdateProvider params completion =
  sendMessage mtrBaseClusterOTASoftwareUpdateProvider applyUpdateRequestWithParams_completionSelector (toMTROTASoftwareUpdateProviderClusterApplyUpdateRequestParams params) completion

-- | Command NotifyUpdateApplied
--
-- Notify OTA Provider that an update was applied
--
-- ObjC selector: @- notifyUpdateAppliedWithParams:completion:@
notifyUpdateAppliedWithParams_completion :: (IsMTRBaseClusterOTASoftwareUpdateProvider mtrBaseClusterOTASoftwareUpdateProvider, IsMTROTASoftwareUpdateProviderClusterNotifyUpdateAppliedParams params) => mtrBaseClusterOTASoftwareUpdateProvider -> params -> Ptr () -> IO ()
notifyUpdateAppliedWithParams_completion mtrBaseClusterOTASoftwareUpdateProvider params completion =
  sendMessage mtrBaseClusterOTASoftwareUpdateProvider notifyUpdateAppliedWithParams_completionSelector (toMTROTASoftwareUpdateProviderClusterNotifyUpdateAppliedParams params) completion

-- | @- readAttributeGeneratedCommandListWithCompletion:@
readAttributeGeneratedCommandListWithCompletion :: IsMTRBaseClusterOTASoftwareUpdateProvider mtrBaseClusterOTASoftwareUpdateProvider => mtrBaseClusterOTASoftwareUpdateProvider -> Ptr () -> IO ()
readAttributeGeneratedCommandListWithCompletion mtrBaseClusterOTASoftwareUpdateProvider completion =
  sendMessage mtrBaseClusterOTASoftwareUpdateProvider readAttributeGeneratedCommandListWithCompletionSelector completion

-- | @- subscribeAttributeGeneratedCommandListWithParams:subscriptionEstablished:reportHandler:@
subscribeAttributeGeneratedCommandListWithParams_subscriptionEstablished_reportHandler :: (IsMTRBaseClusterOTASoftwareUpdateProvider mtrBaseClusterOTASoftwareUpdateProvider, IsMTRSubscribeParams params) => mtrBaseClusterOTASoftwareUpdateProvider -> params -> Ptr () -> Ptr () -> IO ()
subscribeAttributeGeneratedCommandListWithParams_subscriptionEstablished_reportHandler mtrBaseClusterOTASoftwareUpdateProvider params subscriptionEstablished reportHandler =
  sendMessage mtrBaseClusterOTASoftwareUpdateProvider subscribeAttributeGeneratedCommandListWithParams_subscriptionEstablished_reportHandlerSelector (toMTRSubscribeParams params) subscriptionEstablished reportHandler

-- | @+ readAttributeGeneratedCommandListWithClusterStateCache:endpoint:queue:completion:@
readAttributeGeneratedCommandListWithClusterStateCache_endpoint_queue_completion :: (IsMTRClusterStateCacheContainer clusterStateCacheContainer, IsNSNumber endpoint, IsNSObject queue) => clusterStateCacheContainer -> endpoint -> queue -> Ptr () -> IO ()
readAttributeGeneratedCommandListWithClusterStateCache_endpoint_queue_completion clusterStateCacheContainer endpoint queue completion =
  do
    cls' <- getRequiredClass "MTRBaseClusterOTASoftwareUpdateProvider"
    sendClassMessage cls' readAttributeGeneratedCommandListWithClusterStateCache_endpoint_queue_completionSelector (toMTRClusterStateCacheContainer clusterStateCacheContainer) (toNSNumber endpoint) (toNSObject queue) completion

-- | @- readAttributeAcceptedCommandListWithCompletion:@
readAttributeAcceptedCommandListWithCompletion :: IsMTRBaseClusterOTASoftwareUpdateProvider mtrBaseClusterOTASoftwareUpdateProvider => mtrBaseClusterOTASoftwareUpdateProvider -> Ptr () -> IO ()
readAttributeAcceptedCommandListWithCompletion mtrBaseClusterOTASoftwareUpdateProvider completion =
  sendMessage mtrBaseClusterOTASoftwareUpdateProvider readAttributeAcceptedCommandListWithCompletionSelector completion

-- | @- subscribeAttributeAcceptedCommandListWithParams:subscriptionEstablished:reportHandler:@
subscribeAttributeAcceptedCommandListWithParams_subscriptionEstablished_reportHandler :: (IsMTRBaseClusterOTASoftwareUpdateProvider mtrBaseClusterOTASoftwareUpdateProvider, IsMTRSubscribeParams params) => mtrBaseClusterOTASoftwareUpdateProvider -> params -> Ptr () -> Ptr () -> IO ()
subscribeAttributeAcceptedCommandListWithParams_subscriptionEstablished_reportHandler mtrBaseClusterOTASoftwareUpdateProvider params subscriptionEstablished reportHandler =
  sendMessage mtrBaseClusterOTASoftwareUpdateProvider subscribeAttributeAcceptedCommandListWithParams_subscriptionEstablished_reportHandlerSelector (toMTRSubscribeParams params) subscriptionEstablished reportHandler

-- | @+ readAttributeAcceptedCommandListWithClusterStateCache:endpoint:queue:completion:@
readAttributeAcceptedCommandListWithClusterStateCache_endpoint_queue_completion :: (IsMTRClusterStateCacheContainer clusterStateCacheContainer, IsNSNumber endpoint, IsNSObject queue) => clusterStateCacheContainer -> endpoint -> queue -> Ptr () -> IO ()
readAttributeAcceptedCommandListWithClusterStateCache_endpoint_queue_completion clusterStateCacheContainer endpoint queue completion =
  do
    cls' <- getRequiredClass "MTRBaseClusterOTASoftwareUpdateProvider"
    sendClassMessage cls' readAttributeAcceptedCommandListWithClusterStateCache_endpoint_queue_completionSelector (toMTRClusterStateCacheContainer clusterStateCacheContainer) (toNSNumber endpoint) (toNSObject queue) completion

-- | @- readAttributeAttributeListWithCompletion:@
readAttributeAttributeListWithCompletion :: IsMTRBaseClusterOTASoftwareUpdateProvider mtrBaseClusterOTASoftwareUpdateProvider => mtrBaseClusterOTASoftwareUpdateProvider -> Ptr () -> IO ()
readAttributeAttributeListWithCompletion mtrBaseClusterOTASoftwareUpdateProvider completion =
  sendMessage mtrBaseClusterOTASoftwareUpdateProvider readAttributeAttributeListWithCompletionSelector completion

-- | @- subscribeAttributeAttributeListWithParams:subscriptionEstablished:reportHandler:@
subscribeAttributeAttributeListWithParams_subscriptionEstablished_reportHandler :: (IsMTRBaseClusterOTASoftwareUpdateProvider mtrBaseClusterOTASoftwareUpdateProvider, IsMTRSubscribeParams params) => mtrBaseClusterOTASoftwareUpdateProvider -> params -> Ptr () -> Ptr () -> IO ()
subscribeAttributeAttributeListWithParams_subscriptionEstablished_reportHandler mtrBaseClusterOTASoftwareUpdateProvider params subscriptionEstablished reportHandler =
  sendMessage mtrBaseClusterOTASoftwareUpdateProvider subscribeAttributeAttributeListWithParams_subscriptionEstablished_reportHandlerSelector (toMTRSubscribeParams params) subscriptionEstablished reportHandler

-- | @+ readAttributeAttributeListWithClusterStateCache:endpoint:queue:completion:@
readAttributeAttributeListWithClusterStateCache_endpoint_queue_completion :: (IsMTRClusterStateCacheContainer clusterStateCacheContainer, IsNSNumber endpoint, IsNSObject queue) => clusterStateCacheContainer -> endpoint -> queue -> Ptr () -> IO ()
readAttributeAttributeListWithClusterStateCache_endpoint_queue_completion clusterStateCacheContainer endpoint queue completion =
  do
    cls' <- getRequiredClass "MTRBaseClusterOTASoftwareUpdateProvider"
    sendClassMessage cls' readAttributeAttributeListWithClusterStateCache_endpoint_queue_completionSelector (toMTRClusterStateCacheContainer clusterStateCacheContainer) (toNSNumber endpoint) (toNSObject queue) completion

-- | @- readAttributeFeatureMapWithCompletion:@
readAttributeFeatureMapWithCompletion :: IsMTRBaseClusterOTASoftwareUpdateProvider mtrBaseClusterOTASoftwareUpdateProvider => mtrBaseClusterOTASoftwareUpdateProvider -> Ptr () -> IO ()
readAttributeFeatureMapWithCompletion mtrBaseClusterOTASoftwareUpdateProvider completion =
  sendMessage mtrBaseClusterOTASoftwareUpdateProvider readAttributeFeatureMapWithCompletionSelector completion

-- | @- subscribeAttributeFeatureMapWithParams:subscriptionEstablished:reportHandler:@
subscribeAttributeFeatureMapWithParams_subscriptionEstablished_reportHandler :: (IsMTRBaseClusterOTASoftwareUpdateProvider mtrBaseClusterOTASoftwareUpdateProvider, IsMTRSubscribeParams params) => mtrBaseClusterOTASoftwareUpdateProvider -> params -> Ptr () -> Ptr () -> IO ()
subscribeAttributeFeatureMapWithParams_subscriptionEstablished_reportHandler mtrBaseClusterOTASoftwareUpdateProvider params subscriptionEstablished reportHandler =
  sendMessage mtrBaseClusterOTASoftwareUpdateProvider subscribeAttributeFeatureMapWithParams_subscriptionEstablished_reportHandlerSelector (toMTRSubscribeParams params) subscriptionEstablished reportHandler

-- | @+ readAttributeFeatureMapWithClusterStateCache:endpoint:queue:completion:@
readAttributeFeatureMapWithClusterStateCache_endpoint_queue_completion :: (IsMTRClusterStateCacheContainer clusterStateCacheContainer, IsNSNumber endpoint, IsNSObject queue) => clusterStateCacheContainer -> endpoint -> queue -> Ptr () -> IO ()
readAttributeFeatureMapWithClusterStateCache_endpoint_queue_completion clusterStateCacheContainer endpoint queue completion =
  do
    cls' <- getRequiredClass "MTRBaseClusterOTASoftwareUpdateProvider"
    sendClassMessage cls' readAttributeFeatureMapWithClusterStateCache_endpoint_queue_completionSelector (toMTRClusterStateCacheContainer clusterStateCacheContainer) (toNSNumber endpoint) (toNSObject queue) completion

-- | @- readAttributeClusterRevisionWithCompletion:@
readAttributeClusterRevisionWithCompletion :: IsMTRBaseClusterOTASoftwareUpdateProvider mtrBaseClusterOTASoftwareUpdateProvider => mtrBaseClusterOTASoftwareUpdateProvider -> Ptr () -> IO ()
readAttributeClusterRevisionWithCompletion mtrBaseClusterOTASoftwareUpdateProvider completion =
  sendMessage mtrBaseClusterOTASoftwareUpdateProvider readAttributeClusterRevisionWithCompletionSelector completion

-- | @- subscribeAttributeClusterRevisionWithParams:subscriptionEstablished:reportHandler:@
subscribeAttributeClusterRevisionWithParams_subscriptionEstablished_reportHandler :: (IsMTRBaseClusterOTASoftwareUpdateProvider mtrBaseClusterOTASoftwareUpdateProvider, IsMTRSubscribeParams params) => mtrBaseClusterOTASoftwareUpdateProvider -> params -> Ptr () -> Ptr () -> IO ()
subscribeAttributeClusterRevisionWithParams_subscriptionEstablished_reportHandler mtrBaseClusterOTASoftwareUpdateProvider params subscriptionEstablished reportHandler =
  sendMessage mtrBaseClusterOTASoftwareUpdateProvider subscribeAttributeClusterRevisionWithParams_subscriptionEstablished_reportHandlerSelector (toMTRSubscribeParams params) subscriptionEstablished reportHandler

-- | @+ readAttributeClusterRevisionWithClusterStateCache:endpoint:queue:completion:@
readAttributeClusterRevisionWithClusterStateCache_endpoint_queue_completion :: (IsMTRClusterStateCacheContainer clusterStateCacheContainer, IsNSNumber endpoint, IsNSObject queue) => clusterStateCacheContainer -> endpoint -> queue -> Ptr () -> IO ()
readAttributeClusterRevisionWithClusterStateCache_endpoint_queue_completion clusterStateCacheContainer endpoint queue completion =
  do
    cls' <- getRequiredClass "MTRBaseClusterOTASoftwareUpdateProvider"
    sendClassMessage cls' readAttributeClusterRevisionWithClusterStateCache_endpoint_queue_completionSelector (toMTRClusterStateCacheContainer clusterStateCacheContainer) (toNSNumber endpoint) (toNSObject queue) completion

-- | @- init@
init_ :: IsMTRBaseClusterOTASoftwareUpdateProvider mtrBaseClusterOTASoftwareUpdateProvider => mtrBaseClusterOTASoftwareUpdateProvider -> IO (Id MTRBaseClusterOTASoftwareUpdateProvider)
init_ mtrBaseClusterOTASoftwareUpdateProvider =
  sendOwnedMessage mtrBaseClusterOTASoftwareUpdateProvider initSelector

-- | @+ new@
new :: IO (Id MTRBaseClusterOTASoftwareUpdateProvider)
new  =
  do
    cls' <- getRequiredClass "MTRBaseClusterOTASoftwareUpdateProvider"
    sendOwnedClassMessage cls' newSelector

-- | For all instance methods (reads, writes, commands) that take a completion, the completion will be called on the provided queue.
--
-- ObjC selector: @- initWithDevice:endpointID:queue:@
initWithDevice_endpointID_queue :: (IsMTRBaseClusterOTASoftwareUpdateProvider mtrBaseClusterOTASoftwareUpdateProvider, IsMTRBaseDevice device, IsNSNumber endpointID, IsNSObject queue) => mtrBaseClusterOTASoftwareUpdateProvider -> device -> endpointID -> queue -> IO (Id MTRBaseClusterOTASoftwareUpdateProvider)
initWithDevice_endpointID_queue mtrBaseClusterOTASoftwareUpdateProvider device endpointID queue =
  sendOwnedMessage mtrBaseClusterOTASoftwareUpdateProvider initWithDevice_endpointID_queueSelector (toMTRBaseDevice device) (toNSNumber endpointID) (toNSObject queue)

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @queryImageWithParams:completion:@
queryImageWithParams_completionSelector :: Selector '[Id MTROTASoftwareUpdateProviderClusterQueryImageParams, Ptr ()] ()
queryImageWithParams_completionSelector = mkSelector "queryImageWithParams:completion:"

-- | @Selector@ for @applyUpdateRequestWithParams:completion:@
applyUpdateRequestWithParams_completionSelector :: Selector '[Id MTROTASoftwareUpdateProviderClusterApplyUpdateRequestParams, Ptr ()] ()
applyUpdateRequestWithParams_completionSelector = mkSelector "applyUpdateRequestWithParams:completion:"

-- | @Selector@ for @notifyUpdateAppliedWithParams:completion:@
notifyUpdateAppliedWithParams_completionSelector :: Selector '[Id MTROTASoftwareUpdateProviderClusterNotifyUpdateAppliedParams, Ptr ()] ()
notifyUpdateAppliedWithParams_completionSelector = mkSelector "notifyUpdateAppliedWithParams:completion:"

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
initSelector :: Selector '[] (Id MTRBaseClusterOTASoftwareUpdateProvider)
initSelector = mkSelector "init"

-- | @Selector@ for @new@
newSelector :: Selector '[] (Id MTRBaseClusterOTASoftwareUpdateProvider)
newSelector = mkSelector "new"

-- | @Selector@ for @initWithDevice:endpointID:queue:@
initWithDevice_endpointID_queueSelector :: Selector '[Id MTRBaseDevice, Id NSNumber, Id NSObject] (Id MTRBaseClusterOTASoftwareUpdateProvider)
initWithDevice_endpointID_queueSelector = mkSelector "initWithDevice:endpointID:queue:"

