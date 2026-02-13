{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Cluster WebRTC Transport Provider
--
-- The WebRTC transport provider cluster provides a way for stream providers (e.g. Cameras) to stream or receive their data through WebRTC.
--
-- Generated bindings for @MTRBaseClusterWebRTCTransportProvider@.
module ObjC.Matter.MTRBaseClusterWebRTCTransportProvider
  ( MTRBaseClusterWebRTCTransportProvider
  , IsMTRBaseClusterWebRTCTransportProvider(..)
  , solicitOfferWithParams_completion
  , provideOfferWithParams_completion
  , provideAnswerWithParams_completion
  , provideICECandidatesWithParams_completion
  , endSessionWithParams_completion
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
  , endSessionWithParams_completionSelector
  , initSelector
  , initWithDevice_endpointID_queueSelector
  , newSelector
  , provideAnswerWithParams_completionSelector
  , provideICECandidatesWithParams_completionSelector
  , provideOfferWithParams_completionSelector
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
  , solicitOfferWithParams_completionSelector
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

-- | Command SolicitOffer
--
-- Requests that the Provider initiates a new session with the Offer / Answer flow in a way that allows for options to be passed and work with devices needing the standby flow.
--
-- ObjC selector: @- solicitOfferWithParams:completion:@
solicitOfferWithParams_completion :: (IsMTRBaseClusterWebRTCTransportProvider mtrBaseClusterWebRTCTransportProvider, IsMTRWebRTCTransportProviderClusterSolicitOfferParams params) => mtrBaseClusterWebRTCTransportProvider -> params -> Ptr () -> IO ()
solicitOfferWithParams_completion mtrBaseClusterWebRTCTransportProvider params completion =
  sendMessage mtrBaseClusterWebRTCTransportProvider solicitOfferWithParams_completionSelector (toMTRWebRTCTransportProviderClusterSolicitOfferParams params) completion

-- | Command ProvideOffer
--
-- This command allows an SDP Offer to be set and start a new session.
--
-- ObjC selector: @- provideOfferWithParams:completion:@
provideOfferWithParams_completion :: (IsMTRBaseClusterWebRTCTransportProvider mtrBaseClusterWebRTCTransportProvider, IsMTRWebRTCTransportProviderClusterProvideOfferParams params) => mtrBaseClusterWebRTCTransportProvider -> params -> Ptr () -> IO ()
provideOfferWithParams_completion mtrBaseClusterWebRTCTransportProvider params completion =
  sendMessage mtrBaseClusterWebRTCTransportProvider provideOfferWithParams_completionSelector (toMTRWebRTCTransportProviderClusterProvideOfferParams params) completion

-- | Command ProvideAnswer
--
-- This command SHALL be initiated from a Node in response to an Offer that was previously received from a remote peer.
--
-- ObjC selector: @- provideAnswerWithParams:completion:@
provideAnswerWithParams_completion :: (IsMTRBaseClusterWebRTCTransportProvider mtrBaseClusterWebRTCTransportProvider, IsMTRWebRTCTransportProviderClusterProvideAnswerParams params) => mtrBaseClusterWebRTCTransportProvider -> params -> Ptr () -> IO ()
provideAnswerWithParams_completion mtrBaseClusterWebRTCTransportProvider params completion =
  sendMessage mtrBaseClusterWebRTCTransportProvider provideAnswerWithParams_completionSelector (toMTRWebRTCTransportProviderClusterProvideAnswerParams params) completion

-- | Command ProvideICECandidates
--
-- This command allows for string based ICE candidates generated after the initial Offer / Answer exchange, via a JSEP onicecandidate event, a DOM rtcpeerconnectioniceevent event, or other WebRTC compliant implementations, to be added to a session during the gathering phase.
--
-- ObjC selector: @- provideICECandidatesWithParams:completion:@
provideICECandidatesWithParams_completion :: (IsMTRBaseClusterWebRTCTransportProvider mtrBaseClusterWebRTCTransportProvider, IsMTRWebRTCTransportProviderClusterProvideICECandidatesParams params) => mtrBaseClusterWebRTCTransportProvider -> params -> Ptr () -> IO ()
provideICECandidatesWithParams_completion mtrBaseClusterWebRTCTransportProvider params completion =
  sendMessage mtrBaseClusterWebRTCTransportProvider provideICECandidatesWithParams_completionSelector (toMTRWebRTCTransportProviderClusterProvideICECandidatesParams params) completion

-- | Command EndSession
--
-- This command instructs the stream provider to end the WebRTC session.
--
-- ObjC selector: @- endSessionWithParams:completion:@
endSessionWithParams_completion :: (IsMTRBaseClusterWebRTCTransportProvider mtrBaseClusterWebRTCTransportProvider, IsMTRWebRTCTransportProviderClusterEndSessionParams params) => mtrBaseClusterWebRTCTransportProvider -> params -> Ptr () -> IO ()
endSessionWithParams_completion mtrBaseClusterWebRTCTransportProvider params completion =
  sendMessage mtrBaseClusterWebRTCTransportProvider endSessionWithParams_completionSelector (toMTRWebRTCTransportProviderClusterEndSessionParams params) completion

-- | @- readAttributeCurrentSessionsWithParams:completion:@
readAttributeCurrentSessionsWithParams_completion :: (IsMTRBaseClusterWebRTCTransportProvider mtrBaseClusterWebRTCTransportProvider, IsMTRReadParams params) => mtrBaseClusterWebRTCTransportProvider -> params -> Ptr () -> IO ()
readAttributeCurrentSessionsWithParams_completion mtrBaseClusterWebRTCTransportProvider params completion =
  sendMessage mtrBaseClusterWebRTCTransportProvider readAttributeCurrentSessionsWithParams_completionSelector (toMTRReadParams params) completion

-- | @- subscribeAttributeCurrentSessionsWithParams:subscriptionEstablished:reportHandler:@
subscribeAttributeCurrentSessionsWithParams_subscriptionEstablished_reportHandler :: (IsMTRBaseClusterWebRTCTransportProvider mtrBaseClusterWebRTCTransportProvider, IsMTRSubscribeParams params) => mtrBaseClusterWebRTCTransportProvider -> params -> Ptr () -> Ptr () -> IO ()
subscribeAttributeCurrentSessionsWithParams_subscriptionEstablished_reportHandler mtrBaseClusterWebRTCTransportProvider params subscriptionEstablished reportHandler =
  sendMessage mtrBaseClusterWebRTCTransportProvider subscribeAttributeCurrentSessionsWithParams_subscriptionEstablished_reportHandlerSelector (toMTRSubscribeParams params) subscriptionEstablished reportHandler

-- | @+ readAttributeCurrentSessionsWithClusterStateCache:endpoint:queue:completion:@
readAttributeCurrentSessionsWithClusterStateCache_endpoint_queue_completion :: (IsMTRClusterStateCacheContainer clusterStateCacheContainer, IsNSNumber endpoint, IsNSObject queue) => clusterStateCacheContainer -> endpoint -> queue -> Ptr () -> IO ()
readAttributeCurrentSessionsWithClusterStateCache_endpoint_queue_completion clusterStateCacheContainer endpoint queue completion =
  do
    cls' <- getRequiredClass "MTRBaseClusterWebRTCTransportProvider"
    sendClassMessage cls' readAttributeCurrentSessionsWithClusterStateCache_endpoint_queue_completionSelector (toMTRClusterStateCacheContainer clusterStateCacheContainer) (toNSNumber endpoint) (toNSObject queue) completion

-- | @- readAttributeGeneratedCommandListWithCompletion:@
readAttributeGeneratedCommandListWithCompletion :: IsMTRBaseClusterWebRTCTransportProvider mtrBaseClusterWebRTCTransportProvider => mtrBaseClusterWebRTCTransportProvider -> Ptr () -> IO ()
readAttributeGeneratedCommandListWithCompletion mtrBaseClusterWebRTCTransportProvider completion =
  sendMessage mtrBaseClusterWebRTCTransportProvider readAttributeGeneratedCommandListWithCompletionSelector completion

-- | @- subscribeAttributeGeneratedCommandListWithParams:subscriptionEstablished:reportHandler:@
subscribeAttributeGeneratedCommandListWithParams_subscriptionEstablished_reportHandler :: (IsMTRBaseClusterWebRTCTransportProvider mtrBaseClusterWebRTCTransportProvider, IsMTRSubscribeParams params) => mtrBaseClusterWebRTCTransportProvider -> params -> Ptr () -> Ptr () -> IO ()
subscribeAttributeGeneratedCommandListWithParams_subscriptionEstablished_reportHandler mtrBaseClusterWebRTCTransportProvider params subscriptionEstablished reportHandler =
  sendMessage mtrBaseClusterWebRTCTransportProvider subscribeAttributeGeneratedCommandListWithParams_subscriptionEstablished_reportHandlerSelector (toMTRSubscribeParams params) subscriptionEstablished reportHandler

-- | @+ readAttributeGeneratedCommandListWithClusterStateCache:endpoint:queue:completion:@
readAttributeGeneratedCommandListWithClusterStateCache_endpoint_queue_completion :: (IsMTRClusterStateCacheContainer clusterStateCacheContainer, IsNSNumber endpoint, IsNSObject queue) => clusterStateCacheContainer -> endpoint -> queue -> Ptr () -> IO ()
readAttributeGeneratedCommandListWithClusterStateCache_endpoint_queue_completion clusterStateCacheContainer endpoint queue completion =
  do
    cls' <- getRequiredClass "MTRBaseClusterWebRTCTransportProvider"
    sendClassMessage cls' readAttributeGeneratedCommandListWithClusterStateCache_endpoint_queue_completionSelector (toMTRClusterStateCacheContainer clusterStateCacheContainer) (toNSNumber endpoint) (toNSObject queue) completion

-- | @- readAttributeAcceptedCommandListWithCompletion:@
readAttributeAcceptedCommandListWithCompletion :: IsMTRBaseClusterWebRTCTransportProvider mtrBaseClusterWebRTCTransportProvider => mtrBaseClusterWebRTCTransportProvider -> Ptr () -> IO ()
readAttributeAcceptedCommandListWithCompletion mtrBaseClusterWebRTCTransportProvider completion =
  sendMessage mtrBaseClusterWebRTCTransportProvider readAttributeAcceptedCommandListWithCompletionSelector completion

-- | @- subscribeAttributeAcceptedCommandListWithParams:subscriptionEstablished:reportHandler:@
subscribeAttributeAcceptedCommandListWithParams_subscriptionEstablished_reportHandler :: (IsMTRBaseClusterWebRTCTransportProvider mtrBaseClusterWebRTCTransportProvider, IsMTRSubscribeParams params) => mtrBaseClusterWebRTCTransportProvider -> params -> Ptr () -> Ptr () -> IO ()
subscribeAttributeAcceptedCommandListWithParams_subscriptionEstablished_reportHandler mtrBaseClusterWebRTCTransportProvider params subscriptionEstablished reportHandler =
  sendMessage mtrBaseClusterWebRTCTransportProvider subscribeAttributeAcceptedCommandListWithParams_subscriptionEstablished_reportHandlerSelector (toMTRSubscribeParams params) subscriptionEstablished reportHandler

-- | @+ readAttributeAcceptedCommandListWithClusterStateCache:endpoint:queue:completion:@
readAttributeAcceptedCommandListWithClusterStateCache_endpoint_queue_completion :: (IsMTRClusterStateCacheContainer clusterStateCacheContainer, IsNSNumber endpoint, IsNSObject queue) => clusterStateCacheContainer -> endpoint -> queue -> Ptr () -> IO ()
readAttributeAcceptedCommandListWithClusterStateCache_endpoint_queue_completion clusterStateCacheContainer endpoint queue completion =
  do
    cls' <- getRequiredClass "MTRBaseClusterWebRTCTransportProvider"
    sendClassMessage cls' readAttributeAcceptedCommandListWithClusterStateCache_endpoint_queue_completionSelector (toMTRClusterStateCacheContainer clusterStateCacheContainer) (toNSNumber endpoint) (toNSObject queue) completion

-- | @- readAttributeAttributeListWithCompletion:@
readAttributeAttributeListWithCompletion :: IsMTRBaseClusterWebRTCTransportProvider mtrBaseClusterWebRTCTransportProvider => mtrBaseClusterWebRTCTransportProvider -> Ptr () -> IO ()
readAttributeAttributeListWithCompletion mtrBaseClusterWebRTCTransportProvider completion =
  sendMessage mtrBaseClusterWebRTCTransportProvider readAttributeAttributeListWithCompletionSelector completion

-- | @- subscribeAttributeAttributeListWithParams:subscriptionEstablished:reportHandler:@
subscribeAttributeAttributeListWithParams_subscriptionEstablished_reportHandler :: (IsMTRBaseClusterWebRTCTransportProvider mtrBaseClusterWebRTCTransportProvider, IsMTRSubscribeParams params) => mtrBaseClusterWebRTCTransportProvider -> params -> Ptr () -> Ptr () -> IO ()
subscribeAttributeAttributeListWithParams_subscriptionEstablished_reportHandler mtrBaseClusterWebRTCTransportProvider params subscriptionEstablished reportHandler =
  sendMessage mtrBaseClusterWebRTCTransportProvider subscribeAttributeAttributeListWithParams_subscriptionEstablished_reportHandlerSelector (toMTRSubscribeParams params) subscriptionEstablished reportHandler

-- | @+ readAttributeAttributeListWithClusterStateCache:endpoint:queue:completion:@
readAttributeAttributeListWithClusterStateCache_endpoint_queue_completion :: (IsMTRClusterStateCacheContainer clusterStateCacheContainer, IsNSNumber endpoint, IsNSObject queue) => clusterStateCacheContainer -> endpoint -> queue -> Ptr () -> IO ()
readAttributeAttributeListWithClusterStateCache_endpoint_queue_completion clusterStateCacheContainer endpoint queue completion =
  do
    cls' <- getRequiredClass "MTRBaseClusterWebRTCTransportProvider"
    sendClassMessage cls' readAttributeAttributeListWithClusterStateCache_endpoint_queue_completionSelector (toMTRClusterStateCacheContainer clusterStateCacheContainer) (toNSNumber endpoint) (toNSObject queue) completion

-- | @- readAttributeFeatureMapWithCompletion:@
readAttributeFeatureMapWithCompletion :: IsMTRBaseClusterWebRTCTransportProvider mtrBaseClusterWebRTCTransportProvider => mtrBaseClusterWebRTCTransportProvider -> Ptr () -> IO ()
readAttributeFeatureMapWithCompletion mtrBaseClusterWebRTCTransportProvider completion =
  sendMessage mtrBaseClusterWebRTCTransportProvider readAttributeFeatureMapWithCompletionSelector completion

-- | @- subscribeAttributeFeatureMapWithParams:subscriptionEstablished:reportHandler:@
subscribeAttributeFeatureMapWithParams_subscriptionEstablished_reportHandler :: (IsMTRBaseClusterWebRTCTransportProvider mtrBaseClusterWebRTCTransportProvider, IsMTRSubscribeParams params) => mtrBaseClusterWebRTCTransportProvider -> params -> Ptr () -> Ptr () -> IO ()
subscribeAttributeFeatureMapWithParams_subscriptionEstablished_reportHandler mtrBaseClusterWebRTCTransportProvider params subscriptionEstablished reportHandler =
  sendMessage mtrBaseClusterWebRTCTransportProvider subscribeAttributeFeatureMapWithParams_subscriptionEstablished_reportHandlerSelector (toMTRSubscribeParams params) subscriptionEstablished reportHandler

-- | @+ readAttributeFeatureMapWithClusterStateCache:endpoint:queue:completion:@
readAttributeFeatureMapWithClusterStateCache_endpoint_queue_completion :: (IsMTRClusterStateCacheContainer clusterStateCacheContainer, IsNSNumber endpoint, IsNSObject queue) => clusterStateCacheContainer -> endpoint -> queue -> Ptr () -> IO ()
readAttributeFeatureMapWithClusterStateCache_endpoint_queue_completion clusterStateCacheContainer endpoint queue completion =
  do
    cls' <- getRequiredClass "MTRBaseClusterWebRTCTransportProvider"
    sendClassMessage cls' readAttributeFeatureMapWithClusterStateCache_endpoint_queue_completionSelector (toMTRClusterStateCacheContainer clusterStateCacheContainer) (toNSNumber endpoint) (toNSObject queue) completion

-- | @- readAttributeClusterRevisionWithCompletion:@
readAttributeClusterRevisionWithCompletion :: IsMTRBaseClusterWebRTCTransportProvider mtrBaseClusterWebRTCTransportProvider => mtrBaseClusterWebRTCTransportProvider -> Ptr () -> IO ()
readAttributeClusterRevisionWithCompletion mtrBaseClusterWebRTCTransportProvider completion =
  sendMessage mtrBaseClusterWebRTCTransportProvider readAttributeClusterRevisionWithCompletionSelector completion

-- | @- subscribeAttributeClusterRevisionWithParams:subscriptionEstablished:reportHandler:@
subscribeAttributeClusterRevisionWithParams_subscriptionEstablished_reportHandler :: (IsMTRBaseClusterWebRTCTransportProvider mtrBaseClusterWebRTCTransportProvider, IsMTRSubscribeParams params) => mtrBaseClusterWebRTCTransportProvider -> params -> Ptr () -> Ptr () -> IO ()
subscribeAttributeClusterRevisionWithParams_subscriptionEstablished_reportHandler mtrBaseClusterWebRTCTransportProvider params subscriptionEstablished reportHandler =
  sendMessage mtrBaseClusterWebRTCTransportProvider subscribeAttributeClusterRevisionWithParams_subscriptionEstablished_reportHandlerSelector (toMTRSubscribeParams params) subscriptionEstablished reportHandler

-- | @+ readAttributeClusterRevisionWithClusterStateCache:endpoint:queue:completion:@
readAttributeClusterRevisionWithClusterStateCache_endpoint_queue_completion :: (IsMTRClusterStateCacheContainer clusterStateCacheContainer, IsNSNumber endpoint, IsNSObject queue) => clusterStateCacheContainer -> endpoint -> queue -> Ptr () -> IO ()
readAttributeClusterRevisionWithClusterStateCache_endpoint_queue_completion clusterStateCacheContainer endpoint queue completion =
  do
    cls' <- getRequiredClass "MTRBaseClusterWebRTCTransportProvider"
    sendClassMessage cls' readAttributeClusterRevisionWithClusterStateCache_endpoint_queue_completionSelector (toMTRClusterStateCacheContainer clusterStateCacheContainer) (toNSNumber endpoint) (toNSObject queue) completion

-- | @- init@
init_ :: IsMTRBaseClusterWebRTCTransportProvider mtrBaseClusterWebRTCTransportProvider => mtrBaseClusterWebRTCTransportProvider -> IO (Id MTRBaseClusterWebRTCTransportProvider)
init_ mtrBaseClusterWebRTCTransportProvider =
  sendOwnedMessage mtrBaseClusterWebRTCTransportProvider initSelector

-- | @+ new@
new :: IO (Id MTRBaseClusterWebRTCTransportProvider)
new  =
  do
    cls' <- getRequiredClass "MTRBaseClusterWebRTCTransportProvider"
    sendOwnedClassMessage cls' newSelector

-- | For all instance methods (reads, writes, commands) that take a completion, the completion will be called on the provided queue.
--
-- ObjC selector: @- initWithDevice:endpointID:queue:@
initWithDevice_endpointID_queue :: (IsMTRBaseClusterWebRTCTransportProvider mtrBaseClusterWebRTCTransportProvider, IsMTRBaseDevice device, IsNSNumber endpointID, IsNSObject queue) => mtrBaseClusterWebRTCTransportProvider -> device -> endpointID -> queue -> IO (Id MTRBaseClusterWebRTCTransportProvider)
initWithDevice_endpointID_queue mtrBaseClusterWebRTCTransportProvider device endpointID queue =
  sendOwnedMessage mtrBaseClusterWebRTCTransportProvider initWithDevice_endpointID_queueSelector (toMTRBaseDevice device) (toNSNumber endpointID) (toNSObject queue)

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @solicitOfferWithParams:completion:@
solicitOfferWithParams_completionSelector :: Selector '[Id MTRWebRTCTransportProviderClusterSolicitOfferParams, Ptr ()] ()
solicitOfferWithParams_completionSelector = mkSelector "solicitOfferWithParams:completion:"

-- | @Selector@ for @provideOfferWithParams:completion:@
provideOfferWithParams_completionSelector :: Selector '[Id MTRWebRTCTransportProviderClusterProvideOfferParams, Ptr ()] ()
provideOfferWithParams_completionSelector = mkSelector "provideOfferWithParams:completion:"

-- | @Selector@ for @provideAnswerWithParams:completion:@
provideAnswerWithParams_completionSelector :: Selector '[Id MTRWebRTCTransportProviderClusterProvideAnswerParams, Ptr ()] ()
provideAnswerWithParams_completionSelector = mkSelector "provideAnswerWithParams:completion:"

-- | @Selector@ for @provideICECandidatesWithParams:completion:@
provideICECandidatesWithParams_completionSelector :: Selector '[Id MTRWebRTCTransportProviderClusterProvideICECandidatesParams, Ptr ()] ()
provideICECandidatesWithParams_completionSelector = mkSelector "provideICECandidatesWithParams:completion:"

-- | @Selector@ for @endSessionWithParams:completion:@
endSessionWithParams_completionSelector :: Selector '[Id MTRWebRTCTransportProviderClusterEndSessionParams, Ptr ()] ()
endSessionWithParams_completionSelector = mkSelector "endSessionWithParams:completion:"

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
initSelector :: Selector '[] (Id MTRBaseClusterWebRTCTransportProvider)
initSelector = mkSelector "init"

-- | @Selector@ for @new@
newSelector :: Selector '[] (Id MTRBaseClusterWebRTCTransportProvider)
newSelector = mkSelector "new"

-- | @Selector@ for @initWithDevice:endpointID:queue:@
initWithDevice_endpointID_queueSelector :: Selector '[Id MTRBaseDevice, Id NSNumber, Id NSObject] (Id MTRBaseClusterWebRTCTransportProvider)
initWithDevice_endpointID_queueSelector = mkSelector "initWithDevice:endpointID:queue:"

