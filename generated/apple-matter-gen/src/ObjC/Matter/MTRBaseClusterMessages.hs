{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Cluster Messages
--
-- This cluster provides an interface for passing messages to be presented by a device.
--
-- Generated bindings for @MTRBaseClusterMessages@.
module ObjC.Matter.MTRBaseClusterMessages
  ( MTRBaseClusterMessages
  , IsMTRBaseClusterMessages(..)
  , presentMessagesRequestWithParams_completion
  , cancelMessagesRequestWithParams_completion
  , readAttributeMessagesWithCompletion
  , subscribeAttributeMessagesWithParams_subscriptionEstablished_reportHandler
  , readAttributeMessagesWithClusterStateCache_endpoint_queue_completion
  , readAttributeActiveMessageIDsWithCompletion
  , subscribeAttributeActiveMessageIDsWithParams_subscriptionEstablished_reportHandler
  , readAttributeActiveMessageIDsWithClusterStateCache_endpoint_queue_completion
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
  , cancelMessagesRequestWithParams_completionSelector
  , initSelector
  , initWithDevice_endpointID_queueSelector
  , newSelector
  , presentMessagesRequestWithParams_completionSelector
  , readAttributeAcceptedCommandListWithClusterStateCache_endpoint_queue_completionSelector
  , readAttributeAcceptedCommandListWithCompletionSelector
  , readAttributeActiveMessageIDsWithClusterStateCache_endpoint_queue_completionSelector
  , readAttributeActiveMessageIDsWithCompletionSelector
  , readAttributeAttributeListWithClusterStateCache_endpoint_queue_completionSelector
  , readAttributeAttributeListWithCompletionSelector
  , readAttributeClusterRevisionWithClusterStateCache_endpoint_queue_completionSelector
  , readAttributeClusterRevisionWithCompletionSelector
  , readAttributeFeatureMapWithClusterStateCache_endpoint_queue_completionSelector
  , readAttributeFeatureMapWithCompletionSelector
  , readAttributeGeneratedCommandListWithClusterStateCache_endpoint_queue_completionSelector
  , readAttributeGeneratedCommandListWithCompletionSelector
  , readAttributeMessagesWithClusterStateCache_endpoint_queue_completionSelector
  , readAttributeMessagesWithCompletionSelector
  , subscribeAttributeAcceptedCommandListWithParams_subscriptionEstablished_reportHandlerSelector
  , subscribeAttributeActiveMessageIDsWithParams_subscriptionEstablished_reportHandlerSelector
  , subscribeAttributeAttributeListWithParams_subscriptionEstablished_reportHandlerSelector
  , subscribeAttributeClusterRevisionWithParams_subscriptionEstablished_reportHandlerSelector
  , subscribeAttributeFeatureMapWithParams_subscriptionEstablished_reportHandlerSelector
  , subscribeAttributeGeneratedCommandListWithParams_subscriptionEstablished_reportHandlerSelector
  , subscribeAttributeMessagesWithParams_subscriptionEstablished_reportHandlerSelector


  ) where

import Foreign.Ptr (Ptr, FunPtr)
import Foreign.C.Types

import ObjC.Runtime.Types
import ObjC.Runtime.Message (sendMessage, sendOwnedMessage, sendClassMessage, sendOwnedClassMessage)
import ObjC.Runtime.Selector (mkSelector)
import ObjC.Runtime.Class (getRequiredClass)

import ObjC.Matter.Internal.Classes
import ObjC.Foundation.Internal.Classes

-- | Command PresentMessagesRequest
--
-- Command for requesting messages be presented
--
-- ObjC selector: @- presentMessagesRequestWithParams:completion:@
presentMessagesRequestWithParams_completion :: (IsMTRBaseClusterMessages mtrBaseClusterMessages, IsMTRMessagesClusterPresentMessagesRequestParams params) => mtrBaseClusterMessages -> params -> Ptr () -> IO ()
presentMessagesRequestWithParams_completion mtrBaseClusterMessages params completion =
  sendMessage mtrBaseClusterMessages presentMessagesRequestWithParams_completionSelector (toMTRMessagesClusterPresentMessagesRequestParams params) completion

-- | Command CancelMessagesRequest
--
-- Command for cancelling message present requests
--
-- ObjC selector: @- cancelMessagesRequestWithParams:completion:@
cancelMessagesRequestWithParams_completion :: (IsMTRBaseClusterMessages mtrBaseClusterMessages, IsMTRMessagesClusterCancelMessagesRequestParams params) => mtrBaseClusterMessages -> params -> Ptr () -> IO ()
cancelMessagesRequestWithParams_completion mtrBaseClusterMessages params completion =
  sendMessage mtrBaseClusterMessages cancelMessagesRequestWithParams_completionSelector (toMTRMessagesClusterCancelMessagesRequestParams params) completion

-- | @- readAttributeMessagesWithCompletion:@
readAttributeMessagesWithCompletion :: IsMTRBaseClusterMessages mtrBaseClusterMessages => mtrBaseClusterMessages -> Ptr () -> IO ()
readAttributeMessagesWithCompletion mtrBaseClusterMessages completion =
  sendMessage mtrBaseClusterMessages readAttributeMessagesWithCompletionSelector completion

-- | @- subscribeAttributeMessagesWithParams:subscriptionEstablished:reportHandler:@
subscribeAttributeMessagesWithParams_subscriptionEstablished_reportHandler :: (IsMTRBaseClusterMessages mtrBaseClusterMessages, IsMTRSubscribeParams params) => mtrBaseClusterMessages -> params -> Ptr () -> Ptr () -> IO ()
subscribeAttributeMessagesWithParams_subscriptionEstablished_reportHandler mtrBaseClusterMessages params subscriptionEstablished reportHandler =
  sendMessage mtrBaseClusterMessages subscribeAttributeMessagesWithParams_subscriptionEstablished_reportHandlerSelector (toMTRSubscribeParams params) subscriptionEstablished reportHandler

-- | @+ readAttributeMessagesWithClusterStateCache:endpoint:queue:completion:@
readAttributeMessagesWithClusterStateCache_endpoint_queue_completion :: (IsMTRClusterStateCacheContainer clusterStateCacheContainer, IsNSNumber endpoint, IsNSObject queue) => clusterStateCacheContainer -> endpoint -> queue -> Ptr () -> IO ()
readAttributeMessagesWithClusterStateCache_endpoint_queue_completion clusterStateCacheContainer endpoint queue completion =
  do
    cls' <- getRequiredClass "MTRBaseClusterMessages"
    sendClassMessage cls' readAttributeMessagesWithClusterStateCache_endpoint_queue_completionSelector (toMTRClusterStateCacheContainer clusterStateCacheContainer) (toNSNumber endpoint) (toNSObject queue) completion

-- | @- readAttributeActiveMessageIDsWithCompletion:@
readAttributeActiveMessageIDsWithCompletion :: IsMTRBaseClusterMessages mtrBaseClusterMessages => mtrBaseClusterMessages -> Ptr () -> IO ()
readAttributeActiveMessageIDsWithCompletion mtrBaseClusterMessages completion =
  sendMessage mtrBaseClusterMessages readAttributeActiveMessageIDsWithCompletionSelector completion

-- | @- subscribeAttributeActiveMessageIDsWithParams:subscriptionEstablished:reportHandler:@
subscribeAttributeActiveMessageIDsWithParams_subscriptionEstablished_reportHandler :: (IsMTRBaseClusterMessages mtrBaseClusterMessages, IsMTRSubscribeParams params) => mtrBaseClusterMessages -> params -> Ptr () -> Ptr () -> IO ()
subscribeAttributeActiveMessageIDsWithParams_subscriptionEstablished_reportHandler mtrBaseClusterMessages params subscriptionEstablished reportHandler =
  sendMessage mtrBaseClusterMessages subscribeAttributeActiveMessageIDsWithParams_subscriptionEstablished_reportHandlerSelector (toMTRSubscribeParams params) subscriptionEstablished reportHandler

-- | @+ readAttributeActiveMessageIDsWithClusterStateCache:endpoint:queue:completion:@
readAttributeActiveMessageIDsWithClusterStateCache_endpoint_queue_completion :: (IsMTRClusterStateCacheContainer clusterStateCacheContainer, IsNSNumber endpoint, IsNSObject queue) => clusterStateCacheContainer -> endpoint -> queue -> Ptr () -> IO ()
readAttributeActiveMessageIDsWithClusterStateCache_endpoint_queue_completion clusterStateCacheContainer endpoint queue completion =
  do
    cls' <- getRequiredClass "MTRBaseClusterMessages"
    sendClassMessage cls' readAttributeActiveMessageIDsWithClusterStateCache_endpoint_queue_completionSelector (toMTRClusterStateCacheContainer clusterStateCacheContainer) (toNSNumber endpoint) (toNSObject queue) completion

-- | @- readAttributeGeneratedCommandListWithCompletion:@
readAttributeGeneratedCommandListWithCompletion :: IsMTRBaseClusterMessages mtrBaseClusterMessages => mtrBaseClusterMessages -> Ptr () -> IO ()
readAttributeGeneratedCommandListWithCompletion mtrBaseClusterMessages completion =
  sendMessage mtrBaseClusterMessages readAttributeGeneratedCommandListWithCompletionSelector completion

-- | @- subscribeAttributeGeneratedCommandListWithParams:subscriptionEstablished:reportHandler:@
subscribeAttributeGeneratedCommandListWithParams_subscriptionEstablished_reportHandler :: (IsMTRBaseClusterMessages mtrBaseClusterMessages, IsMTRSubscribeParams params) => mtrBaseClusterMessages -> params -> Ptr () -> Ptr () -> IO ()
subscribeAttributeGeneratedCommandListWithParams_subscriptionEstablished_reportHandler mtrBaseClusterMessages params subscriptionEstablished reportHandler =
  sendMessage mtrBaseClusterMessages subscribeAttributeGeneratedCommandListWithParams_subscriptionEstablished_reportHandlerSelector (toMTRSubscribeParams params) subscriptionEstablished reportHandler

-- | @+ readAttributeGeneratedCommandListWithClusterStateCache:endpoint:queue:completion:@
readAttributeGeneratedCommandListWithClusterStateCache_endpoint_queue_completion :: (IsMTRClusterStateCacheContainer clusterStateCacheContainer, IsNSNumber endpoint, IsNSObject queue) => clusterStateCacheContainer -> endpoint -> queue -> Ptr () -> IO ()
readAttributeGeneratedCommandListWithClusterStateCache_endpoint_queue_completion clusterStateCacheContainer endpoint queue completion =
  do
    cls' <- getRequiredClass "MTRBaseClusterMessages"
    sendClassMessage cls' readAttributeGeneratedCommandListWithClusterStateCache_endpoint_queue_completionSelector (toMTRClusterStateCacheContainer clusterStateCacheContainer) (toNSNumber endpoint) (toNSObject queue) completion

-- | @- readAttributeAcceptedCommandListWithCompletion:@
readAttributeAcceptedCommandListWithCompletion :: IsMTRBaseClusterMessages mtrBaseClusterMessages => mtrBaseClusterMessages -> Ptr () -> IO ()
readAttributeAcceptedCommandListWithCompletion mtrBaseClusterMessages completion =
  sendMessage mtrBaseClusterMessages readAttributeAcceptedCommandListWithCompletionSelector completion

-- | @- subscribeAttributeAcceptedCommandListWithParams:subscriptionEstablished:reportHandler:@
subscribeAttributeAcceptedCommandListWithParams_subscriptionEstablished_reportHandler :: (IsMTRBaseClusterMessages mtrBaseClusterMessages, IsMTRSubscribeParams params) => mtrBaseClusterMessages -> params -> Ptr () -> Ptr () -> IO ()
subscribeAttributeAcceptedCommandListWithParams_subscriptionEstablished_reportHandler mtrBaseClusterMessages params subscriptionEstablished reportHandler =
  sendMessage mtrBaseClusterMessages subscribeAttributeAcceptedCommandListWithParams_subscriptionEstablished_reportHandlerSelector (toMTRSubscribeParams params) subscriptionEstablished reportHandler

-- | @+ readAttributeAcceptedCommandListWithClusterStateCache:endpoint:queue:completion:@
readAttributeAcceptedCommandListWithClusterStateCache_endpoint_queue_completion :: (IsMTRClusterStateCacheContainer clusterStateCacheContainer, IsNSNumber endpoint, IsNSObject queue) => clusterStateCacheContainer -> endpoint -> queue -> Ptr () -> IO ()
readAttributeAcceptedCommandListWithClusterStateCache_endpoint_queue_completion clusterStateCacheContainer endpoint queue completion =
  do
    cls' <- getRequiredClass "MTRBaseClusterMessages"
    sendClassMessage cls' readAttributeAcceptedCommandListWithClusterStateCache_endpoint_queue_completionSelector (toMTRClusterStateCacheContainer clusterStateCacheContainer) (toNSNumber endpoint) (toNSObject queue) completion

-- | @- readAttributeAttributeListWithCompletion:@
readAttributeAttributeListWithCompletion :: IsMTRBaseClusterMessages mtrBaseClusterMessages => mtrBaseClusterMessages -> Ptr () -> IO ()
readAttributeAttributeListWithCompletion mtrBaseClusterMessages completion =
  sendMessage mtrBaseClusterMessages readAttributeAttributeListWithCompletionSelector completion

-- | @- subscribeAttributeAttributeListWithParams:subscriptionEstablished:reportHandler:@
subscribeAttributeAttributeListWithParams_subscriptionEstablished_reportHandler :: (IsMTRBaseClusterMessages mtrBaseClusterMessages, IsMTRSubscribeParams params) => mtrBaseClusterMessages -> params -> Ptr () -> Ptr () -> IO ()
subscribeAttributeAttributeListWithParams_subscriptionEstablished_reportHandler mtrBaseClusterMessages params subscriptionEstablished reportHandler =
  sendMessage mtrBaseClusterMessages subscribeAttributeAttributeListWithParams_subscriptionEstablished_reportHandlerSelector (toMTRSubscribeParams params) subscriptionEstablished reportHandler

-- | @+ readAttributeAttributeListWithClusterStateCache:endpoint:queue:completion:@
readAttributeAttributeListWithClusterStateCache_endpoint_queue_completion :: (IsMTRClusterStateCacheContainer clusterStateCacheContainer, IsNSNumber endpoint, IsNSObject queue) => clusterStateCacheContainer -> endpoint -> queue -> Ptr () -> IO ()
readAttributeAttributeListWithClusterStateCache_endpoint_queue_completion clusterStateCacheContainer endpoint queue completion =
  do
    cls' <- getRequiredClass "MTRBaseClusterMessages"
    sendClassMessage cls' readAttributeAttributeListWithClusterStateCache_endpoint_queue_completionSelector (toMTRClusterStateCacheContainer clusterStateCacheContainer) (toNSNumber endpoint) (toNSObject queue) completion

-- | @- readAttributeFeatureMapWithCompletion:@
readAttributeFeatureMapWithCompletion :: IsMTRBaseClusterMessages mtrBaseClusterMessages => mtrBaseClusterMessages -> Ptr () -> IO ()
readAttributeFeatureMapWithCompletion mtrBaseClusterMessages completion =
  sendMessage mtrBaseClusterMessages readAttributeFeatureMapWithCompletionSelector completion

-- | @- subscribeAttributeFeatureMapWithParams:subscriptionEstablished:reportHandler:@
subscribeAttributeFeatureMapWithParams_subscriptionEstablished_reportHandler :: (IsMTRBaseClusterMessages mtrBaseClusterMessages, IsMTRSubscribeParams params) => mtrBaseClusterMessages -> params -> Ptr () -> Ptr () -> IO ()
subscribeAttributeFeatureMapWithParams_subscriptionEstablished_reportHandler mtrBaseClusterMessages params subscriptionEstablished reportHandler =
  sendMessage mtrBaseClusterMessages subscribeAttributeFeatureMapWithParams_subscriptionEstablished_reportHandlerSelector (toMTRSubscribeParams params) subscriptionEstablished reportHandler

-- | @+ readAttributeFeatureMapWithClusterStateCache:endpoint:queue:completion:@
readAttributeFeatureMapWithClusterStateCache_endpoint_queue_completion :: (IsMTRClusterStateCacheContainer clusterStateCacheContainer, IsNSNumber endpoint, IsNSObject queue) => clusterStateCacheContainer -> endpoint -> queue -> Ptr () -> IO ()
readAttributeFeatureMapWithClusterStateCache_endpoint_queue_completion clusterStateCacheContainer endpoint queue completion =
  do
    cls' <- getRequiredClass "MTRBaseClusterMessages"
    sendClassMessage cls' readAttributeFeatureMapWithClusterStateCache_endpoint_queue_completionSelector (toMTRClusterStateCacheContainer clusterStateCacheContainer) (toNSNumber endpoint) (toNSObject queue) completion

-- | @- readAttributeClusterRevisionWithCompletion:@
readAttributeClusterRevisionWithCompletion :: IsMTRBaseClusterMessages mtrBaseClusterMessages => mtrBaseClusterMessages -> Ptr () -> IO ()
readAttributeClusterRevisionWithCompletion mtrBaseClusterMessages completion =
  sendMessage mtrBaseClusterMessages readAttributeClusterRevisionWithCompletionSelector completion

-- | @- subscribeAttributeClusterRevisionWithParams:subscriptionEstablished:reportHandler:@
subscribeAttributeClusterRevisionWithParams_subscriptionEstablished_reportHandler :: (IsMTRBaseClusterMessages mtrBaseClusterMessages, IsMTRSubscribeParams params) => mtrBaseClusterMessages -> params -> Ptr () -> Ptr () -> IO ()
subscribeAttributeClusterRevisionWithParams_subscriptionEstablished_reportHandler mtrBaseClusterMessages params subscriptionEstablished reportHandler =
  sendMessage mtrBaseClusterMessages subscribeAttributeClusterRevisionWithParams_subscriptionEstablished_reportHandlerSelector (toMTRSubscribeParams params) subscriptionEstablished reportHandler

-- | @+ readAttributeClusterRevisionWithClusterStateCache:endpoint:queue:completion:@
readAttributeClusterRevisionWithClusterStateCache_endpoint_queue_completion :: (IsMTRClusterStateCacheContainer clusterStateCacheContainer, IsNSNumber endpoint, IsNSObject queue) => clusterStateCacheContainer -> endpoint -> queue -> Ptr () -> IO ()
readAttributeClusterRevisionWithClusterStateCache_endpoint_queue_completion clusterStateCacheContainer endpoint queue completion =
  do
    cls' <- getRequiredClass "MTRBaseClusterMessages"
    sendClassMessage cls' readAttributeClusterRevisionWithClusterStateCache_endpoint_queue_completionSelector (toMTRClusterStateCacheContainer clusterStateCacheContainer) (toNSNumber endpoint) (toNSObject queue) completion

-- | @- init@
init_ :: IsMTRBaseClusterMessages mtrBaseClusterMessages => mtrBaseClusterMessages -> IO (Id MTRBaseClusterMessages)
init_ mtrBaseClusterMessages =
  sendOwnedMessage mtrBaseClusterMessages initSelector

-- | @+ new@
new :: IO (Id MTRBaseClusterMessages)
new  =
  do
    cls' <- getRequiredClass "MTRBaseClusterMessages"
    sendOwnedClassMessage cls' newSelector

-- | For all instance methods (reads, writes, commands) that take a completion, the completion will be called on the provided queue.
--
-- ObjC selector: @- initWithDevice:endpointID:queue:@
initWithDevice_endpointID_queue :: (IsMTRBaseClusterMessages mtrBaseClusterMessages, IsMTRBaseDevice device, IsNSNumber endpointID, IsNSObject queue) => mtrBaseClusterMessages -> device -> endpointID -> queue -> IO (Id MTRBaseClusterMessages)
initWithDevice_endpointID_queue mtrBaseClusterMessages device endpointID queue =
  sendOwnedMessage mtrBaseClusterMessages initWithDevice_endpointID_queueSelector (toMTRBaseDevice device) (toNSNumber endpointID) (toNSObject queue)

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @presentMessagesRequestWithParams:completion:@
presentMessagesRequestWithParams_completionSelector :: Selector '[Id MTRMessagesClusterPresentMessagesRequestParams, Ptr ()] ()
presentMessagesRequestWithParams_completionSelector = mkSelector "presentMessagesRequestWithParams:completion:"

-- | @Selector@ for @cancelMessagesRequestWithParams:completion:@
cancelMessagesRequestWithParams_completionSelector :: Selector '[Id MTRMessagesClusterCancelMessagesRequestParams, Ptr ()] ()
cancelMessagesRequestWithParams_completionSelector = mkSelector "cancelMessagesRequestWithParams:completion:"

-- | @Selector@ for @readAttributeMessagesWithCompletion:@
readAttributeMessagesWithCompletionSelector :: Selector '[Ptr ()] ()
readAttributeMessagesWithCompletionSelector = mkSelector "readAttributeMessagesWithCompletion:"

-- | @Selector@ for @subscribeAttributeMessagesWithParams:subscriptionEstablished:reportHandler:@
subscribeAttributeMessagesWithParams_subscriptionEstablished_reportHandlerSelector :: Selector '[Id MTRSubscribeParams, Ptr (), Ptr ()] ()
subscribeAttributeMessagesWithParams_subscriptionEstablished_reportHandlerSelector = mkSelector "subscribeAttributeMessagesWithParams:subscriptionEstablished:reportHandler:"

-- | @Selector@ for @readAttributeMessagesWithClusterStateCache:endpoint:queue:completion:@
readAttributeMessagesWithClusterStateCache_endpoint_queue_completionSelector :: Selector '[Id MTRClusterStateCacheContainer, Id NSNumber, Id NSObject, Ptr ()] ()
readAttributeMessagesWithClusterStateCache_endpoint_queue_completionSelector = mkSelector "readAttributeMessagesWithClusterStateCache:endpoint:queue:completion:"

-- | @Selector@ for @readAttributeActiveMessageIDsWithCompletion:@
readAttributeActiveMessageIDsWithCompletionSelector :: Selector '[Ptr ()] ()
readAttributeActiveMessageIDsWithCompletionSelector = mkSelector "readAttributeActiveMessageIDsWithCompletion:"

-- | @Selector@ for @subscribeAttributeActiveMessageIDsWithParams:subscriptionEstablished:reportHandler:@
subscribeAttributeActiveMessageIDsWithParams_subscriptionEstablished_reportHandlerSelector :: Selector '[Id MTRSubscribeParams, Ptr (), Ptr ()] ()
subscribeAttributeActiveMessageIDsWithParams_subscriptionEstablished_reportHandlerSelector = mkSelector "subscribeAttributeActiveMessageIDsWithParams:subscriptionEstablished:reportHandler:"

-- | @Selector@ for @readAttributeActiveMessageIDsWithClusterStateCache:endpoint:queue:completion:@
readAttributeActiveMessageIDsWithClusterStateCache_endpoint_queue_completionSelector :: Selector '[Id MTRClusterStateCacheContainer, Id NSNumber, Id NSObject, Ptr ()] ()
readAttributeActiveMessageIDsWithClusterStateCache_endpoint_queue_completionSelector = mkSelector "readAttributeActiveMessageIDsWithClusterStateCache:endpoint:queue:completion:"

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
initSelector :: Selector '[] (Id MTRBaseClusterMessages)
initSelector = mkSelector "init"

-- | @Selector@ for @new@
newSelector :: Selector '[] (Id MTRBaseClusterMessages)
newSelector = mkSelector "new"

-- | @Selector@ for @initWithDevice:endpointID:queue:@
initWithDevice_endpointID_queueSelector :: Selector '[Id MTRBaseDevice, Id NSNumber, Id NSObject] (Id MTRBaseClusterMessages)
initWithDevice_endpointID_queueSelector = mkSelector "initWithDevice:endpointID:queue:"

