{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Cluster WebRTC Transport Requestor
--
-- The WebRTC transport requestor cluster provides a way for stream consumers (e.g. Matter Stream Viewer) to establish a WebRTC connection with a stream provider.
--
-- Generated bindings for @MTRBaseClusterWebRTCTransportRequestor@.
module ObjC.Matter.MTRBaseClusterWebRTCTransportRequestor
  ( MTRBaseClusterWebRTCTransportRequestor
  , IsMTRBaseClusterWebRTCTransportRequestor(..)
  , offerWithParams_completion
  , answerWithParams_completion
  , iceCandidatesWithParams_completion
  , endWithParams_completion
  , readAttributeCurrentSessionsWithParams_completion
  , subscribeAttributeCurrentSessionsWithParams_subscriptionEstablished_reportHandler
  , readAttributeCurrentSessionsWithClusterStateCache_endpoint_queue_completion
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
  , answerWithParams_completionSelector
  , endWithParams_completionSelector
  , iceCandidatesWithParams_completionSelector
  , initSelector
  , initWithDevice_endpointID_queueSelector
  , newSelector
  , offerWithParams_completionSelector
  , readAttributeAcceptedCommandListWithClusterStateCache_endpoint_queue_completionSelector
  , readAttributeAcceptedCommandListWithCompletionSelector
  , readAttributeAttributeListWithClusterStateCache_endpoint_queue_completionSelector
  , readAttributeAttributeListWithCompletionSelector
  , readAttributeClusterRevisionWithClusterStateCache_endpoint_queue_completionSelector
  , readAttributeClusterRevisionWithCompletionSelector
  , readAttributeCurrentSessionsWithClusterStateCache_endpoint_queue_completionSelector
  , readAttributeCurrentSessionsWithParams_completionSelector
  , readAttributeFeatureMapWithClusterStateCache_endpoint_queue_completionSelector
  , readAttributeFeatureMapWithCompletionSelector
  , readAttributeGeneratedCommandListWithClusterStateCache_endpoint_queue_completionSelector
  , readAttributeGeneratedCommandListWithCompletionSelector
  , subscribeAttributeAcceptedCommandListWithParams_subscriptionEstablished_reportHandlerSelector
  , subscribeAttributeAttributeListWithParams_subscriptionEstablished_reportHandlerSelector
  , subscribeAttributeClusterRevisionWithParams_subscriptionEstablished_reportHandlerSelector
  , subscribeAttributeCurrentSessionsWithParams_subscriptionEstablished_reportHandlerSelector
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

-- | Command Offer
--
-- This command provides the stream requestor with WebRTC session details.
--
-- ObjC selector: @- offerWithParams:completion:@
offerWithParams_completion :: (IsMTRBaseClusterWebRTCTransportRequestor mtrBaseClusterWebRTCTransportRequestor, IsMTRWebRTCTransportRequestorClusterOfferParams params) => mtrBaseClusterWebRTCTransportRequestor -> params -> Ptr () -> IO ()
offerWithParams_completion mtrBaseClusterWebRTCTransportRequestor params completion =
  sendMessage mtrBaseClusterWebRTCTransportRequestor offerWithParams_completionSelector (toMTRWebRTCTransportRequestorClusterOfferParams params) completion

-- | Command Answer
--
-- This command provides the stream requestor with the WebRTC session details (i.e. Session ID and SDP answer), It is the next command in the Offer/Answer flow to the ProvideOffer command.
--
-- ObjC selector: @- answerWithParams:completion:@
answerWithParams_completion :: (IsMTRBaseClusterWebRTCTransportRequestor mtrBaseClusterWebRTCTransportRequestor, IsMTRWebRTCTransportRequestorClusterAnswerParams params) => mtrBaseClusterWebRTCTransportRequestor -> params -> Ptr () -> IO ()
answerWithParams_completion mtrBaseClusterWebRTCTransportRequestor params completion =
  sendMessage mtrBaseClusterWebRTCTransportRequestor answerWithParams_completionSelector (toMTRWebRTCTransportRequestorClusterAnswerParams params) completion

-- | Command ICECandidates
--
-- This command allows for the object based ICE candidates generated after the initial Offer / Answer exchange, via a JSEP onicecandidate event, a DOM rtcpeerconnectioniceevent event, or other WebRTC compliant implementations, to be added to a session during the gathering phase.
--
-- ObjC selector: @- ICECandidatesWithParams:completion:@
iceCandidatesWithParams_completion :: (IsMTRBaseClusterWebRTCTransportRequestor mtrBaseClusterWebRTCTransportRequestor, IsMTRWebRTCTransportRequestorClusterICECandidatesParams params) => mtrBaseClusterWebRTCTransportRequestor -> params -> Ptr () -> IO ()
iceCandidatesWithParams_completion mtrBaseClusterWebRTCTransportRequestor params completion =
  sendMessage mtrBaseClusterWebRTCTransportRequestor iceCandidatesWithParams_completionSelector (toMTRWebRTCTransportRequestorClusterICECandidatesParams params) completion

-- | Command End
--
-- This command notifies the stream requestor that the provider has ended the WebRTC session.
--
-- ObjC selector: @- endWithParams:completion:@
endWithParams_completion :: (IsMTRBaseClusterWebRTCTransportRequestor mtrBaseClusterWebRTCTransportRequestor, IsMTRWebRTCTransportRequestorClusterEndParams params) => mtrBaseClusterWebRTCTransportRequestor -> params -> Ptr () -> IO ()
endWithParams_completion mtrBaseClusterWebRTCTransportRequestor params completion =
  sendMessage mtrBaseClusterWebRTCTransportRequestor endWithParams_completionSelector (toMTRWebRTCTransportRequestorClusterEndParams params) completion

-- | @- readAttributeCurrentSessionsWithParams:completion:@
readAttributeCurrentSessionsWithParams_completion :: (IsMTRBaseClusterWebRTCTransportRequestor mtrBaseClusterWebRTCTransportRequestor, IsMTRReadParams params) => mtrBaseClusterWebRTCTransportRequestor -> params -> Ptr () -> IO ()
readAttributeCurrentSessionsWithParams_completion mtrBaseClusterWebRTCTransportRequestor params completion =
  sendMessage mtrBaseClusterWebRTCTransportRequestor readAttributeCurrentSessionsWithParams_completionSelector (toMTRReadParams params) completion

-- | @- subscribeAttributeCurrentSessionsWithParams:subscriptionEstablished:reportHandler:@
subscribeAttributeCurrentSessionsWithParams_subscriptionEstablished_reportHandler :: (IsMTRBaseClusterWebRTCTransportRequestor mtrBaseClusterWebRTCTransportRequestor, IsMTRSubscribeParams params) => mtrBaseClusterWebRTCTransportRequestor -> params -> Ptr () -> Ptr () -> IO ()
subscribeAttributeCurrentSessionsWithParams_subscriptionEstablished_reportHandler mtrBaseClusterWebRTCTransportRequestor params subscriptionEstablished reportHandler =
  sendMessage mtrBaseClusterWebRTCTransportRequestor subscribeAttributeCurrentSessionsWithParams_subscriptionEstablished_reportHandlerSelector (toMTRSubscribeParams params) subscriptionEstablished reportHandler

-- | @+ readAttributeCurrentSessionsWithClusterStateCache:endpoint:queue:completion:@
readAttributeCurrentSessionsWithClusterStateCache_endpoint_queue_completion :: (IsMTRClusterStateCacheContainer clusterStateCacheContainer, IsNSNumber endpoint, IsNSObject queue) => clusterStateCacheContainer -> endpoint -> queue -> Ptr () -> IO ()
readAttributeCurrentSessionsWithClusterStateCache_endpoint_queue_completion clusterStateCacheContainer endpoint queue completion =
  do
    cls' <- getRequiredClass "MTRBaseClusterWebRTCTransportRequestor"
    sendClassMessage cls' readAttributeCurrentSessionsWithClusterStateCache_endpoint_queue_completionSelector (toMTRClusterStateCacheContainer clusterStateCacheContainer) (toNSNumber endpoint) (toNSObject queue) completion

-- | @- readAttributeGeneratedCommandListWithCompletion:@
readAttributeGeneratedCommandListWithCompletion :: IsMTRBaseClusterWebRTCTransportRequestor mtrBaseClusterWebRTCTransportRequestor => mtrBaseClusterWebRTCTransportRequestor -> Ptr () -> IO ()
readAttributeGeneratedCommandListWithCompletion mtrBaseClusterWebRTCTransportRequestor completion =
  sendMessage mtrBaseClusterWebRTCTransportRequestor readAttributeGeneratedCommandListWithCompletionSelector completion

-- | @- subscribeAttributeGeneratedCommandListWithParams:subscriptionEstablished:reportHandler:@
subscribeAttributeGeneratedCommandListWithParams_subscriptionEstablished_reportHandler :: (IsMTRBaseClusterWebRTCTransportRequestor mtrBaseClusterWebRTCTransportRequestor, IsMTRSubscribeParams params) => mtrBaseClusterWebRTCTransportRequestor -> params -> Ptr () -> Ptr () -> IO ()
subscribeAttributeGeneratedCommandListWithParams_subscriptionEstablished_reportHandler mtrBaseClusterWebRTCTransportRequestor params subscriptionEstablished reportHandler =
  sendMessage mtrBaseClusterWebRTCTransportRequestor subscribeAttributeGeneratedCommandListWithParams_subscriptionEstablished_reportHandlerSelector (toMTRSubscribeParams params) subscriptionEstablished reportHandler

-- | @+ readAttributeGeneratedCommandListWithClusterStateCache:endpoint:queue:completion:@
readAttributeGeneratedCommandListWithClusterStateCache_endpoint_queue_completion :: (IsMTRClusterStateCacheContainer clusterStateCacheContainer, IsNSNumber endpoint, IsNSObject queue) => clusterStateCacheContainer -> endpoint -> queue -> Ptr () -> IO ()
readAttributeGeneratedCommandListWithClusterStateCache_endpoint_queue_completion clusterStateCacheContainer endpoint queue completion =
  do
    cls' <- getRequiredClass "MTRBaseClusterWebRTCTransportRequestor"
    sendClassMessage cls' readAttributeGeneratedCommandListWithClusterStateCache_endpoint_queue_completionSelector (toMTRClusterStateCacheContainer clusterStateCacheContainer) (toNSNumber endpoint) (toNSObject queue) completion

-- | @- readAttributeAcceptedCommandListWithCompletion:@
readAttributeAcceptedCommandListWithCompletion :: IsMTRBaseClusterWebRTCTransportRequestor mtrBaseClusterWebRTCTransportRequestor => mtrBaseClusterWebRTCTransportRequestor -> Ptr () -> IO ()
readAttributeAcceptedCommandListWithCompletion mtrBaseClusterWebRTCTransportRequestor completion =
  sendMessage mtrBaseClusterWebRTCTransportRequestor readAttributeAcceptedCommandListWithCompletionSelector completion

-- | @- subscribeAttributeAcceptedCommandListWithParams:subscriptionEstablished:reportHandler:@
subscribeAttributeAcceptedCommandListWithParams_subscriptionEstablished_reportHandler :: (IsMTRBaseClusterWebRTCTransportRequestor mtrBaseClusterWebRTCTransportRequestor, IsMTRSubscribeParams params) => mtrBaseClusterWebRTCTransportRequestor -> params -> Ptr () -> Ptr () -> IO ()
subscribeAttributeAcceptedCommandListWithParams_subscriptionEstablished_reportHandler mtrBaseClusterWebRTCTransportRequestor params subscriptionEstablished reportHandler =
  sendMessage mtrBaseClusterWebRTCTransportRequestor subscribeAttributeAcceptedCommandListWithParams_subscriptionEstablished_reportHandlerSelector (toMTRSubscribeParams params) subscriptionEstablished reportHandler

-- | @+ readAttributeAcceptedCommandListWithClusterStateCache:endpoint:queue:completion:@
readAttributeAcceptedCommandListWithClusterStateCache_endpoint_queue_completion :: (IsMTRClusterStateCacheContainer clusterStateCacheContainer, IsNSNumber endpoint, IsNSObject queue) => clusterStateCacheContainer -> endpoint -> queue -> Ptr () -> IO ()
readAttributeAcceptedCommandListWithClusterStateCache_endpoint_queue_completion clusterStateCacheContainer endpoint queue completion =
  do
    cls' <- getRequiredClass "MTRBaseClusterWebRTCTransportRequestor"
    sendClassMessage cls' readAttributeAcceptedCommandListWithClusterStateCache_endpoint_queue_completionSelector (toMTRClusterStateCacheContainer clusterStateCacheContainer) (toNSNumber endpoint) (toNSObject queue) completion

-- | @- readAttributeAttributeListWithCompletion:@
readAttributeAttributeListWithCompletion :: IsMTRBaseClusterWebRTCTransportRequestor mtrBaseClusterWebRTCTransportRequestor => mtrBaseClusterWebRTCTransportRequestor -> Ptr () -> IO ()
readAttributeAttributeListWithCompletion mtrBaseClusterWebRTCTransportRequestor completion =
  sendMessage mtrBaseClusterWebRTCTransportRequestor readAttributeAttributeListWithCompletionSelector completion

-- | @- subscribeAttributeAttributeListWithParams:subscriptionEstablished:reportHandler:@
subscribeAttributeAttributeListWithParams_subscriptionEstablished_reportHandler :: (IsMTRBaseClusterWebRTCTransportRequestor mtrBaseClusterWebRTCTransportRequestor, IsMTRSubscribeParams params) => mtrBaseClusterWebRTCTransportRequestor -> params -> Ptr () -> Ptr () -> IO ()
subscribeAttributeAttributeListWithParams_subscriptionEstablished_reportHandler mtrBaseClusterWebRTCTransportRequestor params subscriptionEstablished reportHandler =
  sendMessage mtrBaseClusterWebRTCTransportRequestor subscribeAttributeAttributeListWithParams_subscriptionEstablished_reportHandlerSelector (toMTRSubscribeParams params) subscriptionEstablished reportHandler

-- | @+ readAttributeAttributeListWithClusterStateCache:endpoint:queue:completion:@
readAttributeAttributeListWithClusterStateCache_endpoint_queue_completion :: (IsMTRClusterStateCacheContainer clusterStateCacheContainer, IsNSNumber endpoint, IsNSObject queue) => clusterStateCacheContainer -> endpoint -> queue -> Ptr () -> IO ()
readAttributeAttributeListWithClusterStateCache_endpoint_queue_completion clusterStateCacheContainer endpoint queue completion =
  do
    cls' <- getRequiredClass "MTRBaseClusterWebRTCTransportRequestor"
    sendClassMessage cls' readAttributeAttributeListWithClusterStateCache_endpoint_queue_completionSelector (toMTRClusterStateCacheContainer clusterStateCacheContainer) (toNSNumber endpoint) (toNSObject queue) completion

-- | @- readAttributeFeatureMapWithCompletion:@
readAttributeFeatureMapWithCompletion :: IsMTRBaseClusterWebRTCTransportRequestor mtrBaseClusterWebRTCTransportRequestor => mtrBaseClusterWebRTCTransportRequestor -> Ptr () -> IO ()
readAttributeFeatureMapWithCompletion mtrBaseClusterWebRTCTransportRequestor completion =
  sendMessage mtrBaseClusterWebRTCTransportRequestor readAttributeFeatureMapWithCompletionSelector completion

-- | @- subscribeAttributeFeatureMapWithParams:subscriptionEstablished:reportHandler:@
subscribeAttributeFeatureMapWithParams_subscriptionEstablished_reportHandler :: (IsMTRBaseClusterWebRTCTransportRequestor mtrBaseClusterWebRTCTransportRequestor, IsMTRSubscribeParams params) => mtrBaseClusterWebRTCTransportRequestor -> params -> Ptr () -> Ptr () -> IO ()
subscribeAttributeFeatureMapWithParams_subscriptionEstablished_reportHandler mtrBaseClusterWebRTCTransportRequestor params subscriptionEstablished reportHandler =
  sendMessage mtrBaseClusterWebRTCTransportRequestor subscribeAttributeFeatureMapWithParams_subscriptionEstablished_reportHandlerSelector (toMTRSubscribeParams params) subscriptionEstablished reportHandler

-- | @+ readAttributeFeatureMapWithClusterStateCache:endpoint:queue:completion:@
readAttributeFeatureMapWithClusterStateCache_endpoint_queue_completion :: (IsMTRClusterStateCacheContainer clusterStateCacheContainer, IsNSNumber endpoint, IsNSObject queue) => clusterStateCacheContainer -> endpoint -> queue -> Ptr () -> IO ()
readAttributeFeatureMapWithClusterStateCache_endpoint_queue_completion clusterStateCacheContainer endpoint queue completion =
  do
    cls' <- getRequiredClass "MTRBaseClusterWebRTCTransportRequestor"
    sendClassMessage cls' readAttributeFeatureMapWithClusterStateCache_endpoint_queue_completionSelector (toMTRClusterStateCacheContainer clusterStateCacheContainer) (toNSNumber endpoint) (toNSObject queue) completion

-- | @- readAttributeClusterRevisionWithCompletion:@
readAttributeClusterRevisionWithCompletion :: IsMTRBaseClusterWebRTCTransportRequestor mtrBaseClusterWebRTCTransportRequestor => mtrBaseClusterWebRTCTransportRequestor -> Ptr () -> IO ()
readAttributeClusterRevisionWithCompletion mtrBaseClusterWebRTCTransportRequestor completion =
  sendMessage mtrBaseClusterWebRTCTransportRequestor readAttributeClusterRevisionWithCompletionSelector completion

-- | @- subscribeAttributeClusterRevisionWithParams:subscriptionEstablished:reportHandler:@
subscribeAttributeClusterRevisionWithParams_subscriptionEstablished_reportHandler :: (IsMTRBaseClusterWebRTCTransportRequestor mtrBaseClusterWebRTCTransportRequestor, IsMTRSubscribeParams params) => mtrBaseClusterWebRTCTransportRequestor -> params -> Ptr () -> Ptr () -> IO ()
subscribeAttributeClusterRevisionWithParams_subscriptionEstablished_reportHandler mtrBaseClusterWebRTCTransportRequestor params subscriptionEstablished reportHandler =
  sendMessage mtrBaseClusterWebRTCTransportRequestor subscribeAttributeClusterRevisionWithParams_subscriptionEstablished_reportHandlerSelector (toMTRSubscribeParams params) subscriptionEstablished reportHandler

-- | @+ readAttributeClusterRevisionWithClusterStateCache:endpoint:queue:completion:@
readAttributeClusterRevisionWithClusterStateCache_endpoint_queue_completion :: (IsMTRClusterStateCacheContainer clusterStateCacheContainer, IsNSNumber endpoint, IsNSObject queue) => clusterStateCacheContainer -> endpoint -> queue -> Ptr () -> IO ()
readAttributeClusterRevisionWithClusterStateCache_endpoint_queue_completion clusterStateCacheContainer endpoint queue completion =
  do
    cls' <- getRequiredClass "MTRBaseClusterWebRTCTransportRequestor"
    sendClassMessage cls' readAttributeClusterRevisionWithClusterStateCache_endpoint_queue_completionSelector (toMTRClusterStateCacheContainer clusterStateCacheContainer) (toNSNumber endpoint) (toNSObject queue) completion

-- | @- init@
init_ :: IsMTRBaseClusterWebRTCTransportRequestor mtrBaseClusterWebRTCTransportRequestor => mtrBaseClusterWebRTCTransportRequestor -> IO (Id MTRBaseClusterWebRTCTransportRequestor)
init_ mtrBaseClusterWebRTCTransportRequestor =
  sendOwnedMessage mtrBaseClusterWebRTCTransportRequestor initSelector

-- | @+ new@
new :: IO (Id MTRBaseClusterWebRTCTransportRequestor)
new  =
  do
    cls' <- getRequiredClass "MTRBaseClusterWebRTCTransportRequestor"
    sendOwnedClassMessage cls' newSelector

-- | For all instance methods (reads, writes, commands) that take a completion, the completion will be called on the provided queue.
--
-- ObjC selector: @- initWithDevice:endpointID:queue:@
initWithDevice_endpointID_queue :: (IsMTRBaseClusterWebRTCTransportRequestor mtrBaseClusterWebRTCTransportRequestor, IsMTRBaseDevice device, IsNSNumber endpointID, IsNSObject queue) => mtrBaseClusterWebRTCTransportRequestor -> device -> endpointID -> queue -> IO (Id MTRBaseClusterWebRTCTransportRequestor)
initWithDevice_endpointID_queue mtrBaseClusterWebRTCTransportRequestor device endpointID queue =
  sendOwnedMessage mtrBaseClusterWebRTCTransportRequestor initWithDevice_endpointID_queueSelector (toMTRBaseDevice device) (toNSNumber endpointID) (toNSObject queue)

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @offerWithParams:completion:@
offerWithParams_completionSelector :: Selector '[Id MTRWebRTCTransportRequestorClusterOfferParams, Ptr ()] ()
offerWithParams_completionSelector = mkSelector "offerWithParams:completion:"

-- | @Selector@ for @answerWithParams:completion:@
answerWithParams_completionSelector :: Selector '[Id MTRWebRTCTransportRequestorClusterAnswerParams, Ptr ()] ()
answerWithParams_completionSelector = mkSelector "answerWithParams:completion:"

-- | @Selector@ for @ICECandidatesWithParams:completion:@
iceCandidatesWithParams_completionSelector :: Selector '[Id MTRWebRTCTransportRequestorClusterICECandidatesParams, Ptr ()] ()
iceCandidatesWithParams_completionSelector = mkSelector "ICECandidatesWithParams:completion:"

-- | @Selector@ for @endWithParams:completion:@
endWithParams_completionSelector :: Selector '[Id MTRWebRTCTransportRequestorClusterEndParams, Ptr ()] ()
endWithParams_completionSelector = mkSelector "endWithParams:completion:"

-- | @Selector@ for @readAttributeCurrentSessionsWithParams:completion:@
readAttributeCurrentSessionsWithParams_completionSelector :: Selector '[Id MTRReadParams, Ptr ()] ()
readAttributeCurrentSessionsWithParams_completionSelector = mkSelector "readAttributeCurrentSessionsWithParams:completion:"

-- | @Selector@ for @subscribeAttributeCurrentSessionsWithParams:subscriptionEstablished:reportHandler:@
subscribeAttributeCurrentSessionsWithParams_subscriptionEstablished_reportHandlerSelector :: Selector '[Id MTRSubscribeParams, Ptr (), Ptr ()] ()
subscribeAttributeCurrentSessionsWithParams_subscriptionEstablished_reportHandlerSelector = mkSelector "subscribeAttributeCurrentSessionsWithParams:subscriptionEstablished:reportHandler:"

-- | @Selector@ for @readAttributeCurrentSessionsWithClusterStateCache:endpoint:queue:completion:@
readAttributeCurrentSessionsWithClusterStateCache_endpoint_queue_completionSelector :: Selector '[Id MTRClusterStateCacheContainer, Id NSNumber, Id NSObject, Ptr ()] ()
readAttributeCurrentSessionsWithClusterStateCache_endpoint_queue_completionSelector = mkSelector "readAttributeCurrentSessionsWithClusterStateCache:endpoint:queue:completion:"

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
initSelector :: Selector '[] (Id MTRBaseClusterWebRTCTransportRequestor)
initSelector = mkSelector "init"

-- | @Selector@ for @new@
newSelector :: Selector '[] (Id MTRBaseClusterWebRTCTransportRequestor)
newSelector = mkSelector "new"

-- | @Selector@ for @initWithDevice:endpointID:queue:@
initWithDevice_endpointID_queueSelector :: Selector '[Id MTRBaseDevice, Id NSNumber, Id NSObject] (Id MTRBaseClusterWebRTCTransportRequestor)
initWithDevice_endpointID_queueSelector = mkSelector "initWithDevice:endpointID:queue:"

