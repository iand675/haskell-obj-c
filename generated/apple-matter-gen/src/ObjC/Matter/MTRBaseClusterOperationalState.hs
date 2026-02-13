{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Cluster Operational State
--
-- This cluster supports remotely monitoring and, where supported, changing the operational state of any device where a state machine is a part of the operation.
--
-- Generated bindings for @MTRBaseClusterOperationalState@.
module ObjC.Matter.MTRBaseClusterOperationalState
  ( MTRBaseClusterOperationalState
  , IsMTRBaseClusterOperationalState(..)
  , pauseWithParams_completion
  , pauseWithCompletion
  , stopWithParams_completion
  , stopWithCompletion
  , startWithParams_completion
  , startWithCompletion
  , resumeWithParams_completion
  , resumeWithCompletion
  , readAttributePhaseListWithCompletion
  , subscribeAttributePhaseListWithParams_subscriptionEstablished_reportHandler
  , readAttributePhaseListWithClusterStateCache_endpoint_queue_completion
  , readAttributeCurrentPhaseWithCompletion
  , subscribeAttributeCurrentPhaseWithParams_subscriptionEstablished_reportHandler
  , readAttributeCurrentPhaseWithClusterStateCache_endpoint_queue_completion
  , readAttributeCountdownTimeWithCompletion
  , subscribeAttributeCountdownTimeWithParams_subscriptionEstablished_reportHandler
  , readAttributeCountdownTimeWithClusterStateCache_endpoint_queue_completion
  , readAttributeOperationalStateListWithCompletion
  , subscribeAttributeOperationalStateListWithParams_subscriptionEstablished_reportHandler
  , readAttributeOperationalStateListWithClusterStateCache_endpoint_queue_completion
  , readAttributeOperationalStateWithCompletion
  , subscribeAttributeOperationalStateWithParams_subscriptionEstablished_reportHandler
  , readAttributeOperationalStateWithClusterStateCache_endpoint_queue_completion
  , readAttributeOperationalErrorWithCompletion
  , subscribeAttributeOperationalErrorWithParams_subscriptionEstablished_reportHandler
  , readAttributeOperationalErrorWithClusterStateCache_endpoint_queue_completion
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
  , pauseWithCompletionSelector
  , pauseWithParams_completionSelector
  , readAttributeAcceptedCommandListWithClusterStateCache_endpoint_queue_completionSelector
  , readAttributeAcceptedCommandListWithCompletionSelector
  , readAttributeAttributeListWithClusterStateCache_endpoint_queue_completionSelector
  , readAttributeAttributeListWithCompletionSelector
  , readAttributeClusterRevisionWithClusterStateCache_endpoint_queue_completionSelector
  , readAttributeClusterRevisionWithCompletionSelector
  , readAttributeCountdownTimeWithClusterStateCache_endpoint_queue_completionSelector
  , readAttributeCountdownTimeWithCompletionSelector
  , readAttributeCurrentPhaseWithClusterStateCache_endpoint_queue_completionSelector
  , readAttributeCurrentPhaseWithCompletionSelector
  , readAttributeFeatureMapWithClusterStateCache_endpoint_queue_completionSelector
  , readAttributeFeatureMapWithCompletionSelector
  , readAttributeGeneratedCommandListWithClusterStateCache_endpoint_queue_completionSelector
  , readAttributeGeneratedCommandListWithCompletionSelector
  , readAttributeOperationalErrorWithClusterStateCache_endpoint_queue_completionSelector
  , readAttributeOperationalErrorWithCompletionSelector
  , readAttributeOperationalStateListWithClusterStateCache_endpoint_queue_completionSelector
  , readAttributeOperationalStateListWithCompletionSelector
  , readAttributeOperationalStateWithClusterStateCache_endpoint_queue_completionSelector
  , readAttributeOperationalStateWithCompletionSelector
  , readAttributePhaseListWithClusterStateCache_endpoint_queue_completionSelector
  , readAttributePhaseListWithCompletionSelector
  , resumeWithCompletionSelector
  , resumeWithParams_completionSelector
  , startWithCompletionSelector
  , startWithParams_completionSelector
  , stopWithCompletionSelector
  , stopWithParams_completionSelector
  , subscribeAttributeAcceptedCommandListWithParams_subscriptionEstablished_reportHandlerSelector
  , subscribeAttributeAttributeListWithParams_subscriptionEstablished_reportHandlerSelector
  , subscribeAttributeClusterRevisionWithParams_subscriptionEstablished_reportHandlerSelector
  , subscribeAttributeCountdownTimeWithParams_subscriptionEstablished_reportHandlerSelector
  , subscribeAttributeCurrentPhaseWithParams_subscriptionEstablished_reportHandlerSelector
  , subscribeAttributeFeatureMapWithParams_subscriptionEstablished_reportHandlerSelector
  , subscribeAttributeGeneratedCommandListWithParams_subscriptionEstablished_reportHandlerSelector
  , subscribeAttributeOperationalErrorWithParams_subscriptionEstablished_reportHandlerSelector
  , subscribeAttributeOperationalStateListWithParams_subscriptionEstablished_reportHandlerSelector
  , subscribeAttributeOperationalStateWithParams_subscriptionEstablished_reportHandlerSelector
  , subscribeAttributePhaseListWithParams_subscriptionEstablished_reportHandlerSelector


  ) where

import Foreign.Ptr (Ptr, FunPtr)
import Foreign.C.Types

import ObjC.Runtime.Types
import ObjC.Runtime.Message (sendMessage, sendOwnedMessage, sendClassMessage, sendOwnedClassMessage)
import ObjC.Runtime.Selector (mkSelector)
import ObjC.Runtime.Class (getRequiredClass)

import ObjC.Matter.Internal.Classes
import ObjC.Foundation.Internal.Classes

-- | Command Pause
--
-- Upon receipt, the device SHALL pause its operation if it is possible based on the current function of the server.
--
-- ObjC selector: @- pauseWithParams:completion:@
pauseWithParams_completion :: (IsMTRBaseClusterOperationalState mtrBaseClusterOperationalState, IsMTROperationalStateClusterPauseParams params) => mtrBaseClusterOperationalState -> params -> Ptr () -> IO ()
pauseWithParams_completion mtrBaseClusterOperationalState params completion =
  sendMessage mtrBaseClusterOperationalState pauseWithParams_completionSelector (toMTROperationalStateClusterPauseParams params) completion

-- | @- pauseWithCompletion:@
pauseWithCompletion :: IsMTRBaseClusterOperationalState mtrBaseClusterOperationalState => mtrBaseClusterOperationalState -> Ptr () -> IO ()
pauseWithCompletion mtrBaseClusterOperationalState completion =
  sendMessage mtrBaseClusterOperationalState pauseWithCompletionSelector completion

-- | Command Stop
--
-- Upon receipt, the device SHALL stop its operation if it is at a position where it is safe to do so and/or permitted.
--
-- ObjC selector: @- stopWithParams:completion:@
stopWithParams_completion :: (IsMTRBaseClusterOperationalState mtrBaseClusterOperationalState, IsMTROperationalStateClusterStopParams params) => mtrBaseClusterOperationalState -> params -> Ptr () -> IO ()
stopWithParams_completion mtrBaseClusterOperationalState params completion =
  sendMessage mtrBaseClusterOperationalState stopWithParams_completionSelector (toMTROperationalStateClusterStopParams params) completion

-- | @- stopWithCompletion:@
stopWithCompletion :: IsMTRBaseClusterOperationalState mtrBaseClusterOperationalState => mtrBaseClusterOperationalState -> Ptr () -> IO ()
stopWithCompletion mtrBaseClusterOperationalState completion =
  sendMessage mtrBaseClusterOperationalState stopWithCompletionSelector completion

-- | Command Start
--
-- Upon receipt, the device SHALL start its operation if it is safe to do so and the device is in an operational state from which it can be started.
--
-- ObjC selector: @- startWithParams:completion:@
startWithParams_completion :: (IsMTRBaseClusterOperationalState mtrBaseClusterOperationalState, IsMTROperationalStateClusterStartParams params) => mtrBaseClusterOperationalState -> params -> Ptr () -> IO ()
startWithParams_completion mtrBaseClusterOperationalState params completion =
  sendMessage mtrBaseClusterOperationalState startWithParams_completionSelector (toMTROperationalStateClusterStartParams params) completion

-- | @- startWithCompletion:@
startWithCompletion :: IsMTRBaseClusterOperationalState mtrBaseClusterOperationalState => mtrBaseClusterOperationalState -> Ptr () -> IO ()
startWithCompletion mtrBaseClusterOperationalState completion =
  sendMessage mtrBaseClusterOperationalState startWithCompletionSelector completion

-- | Command Resume
--
-- Upon receipt, the device SHALL resume its operation from the point it was at when it received the Pause command, or from the point when it was paused by means outside of this cluster (for example by manual button press).
--
-- ObjC selector: @- resumeWithParams:completion:@
resumeWithParams_completion :: (IsMTRBaseClusterOperationalState mtrBaseClusterOperationalState, IsMTROperationalStateClusterResumeParams params) => mtrBaseClusterOperationalState -> params -> Ptr () -> IO ()
resumeWithParams_completion mtrBaseClusterOperationalState params completion =
  sendMessage mtrBaseClusterOperationalState resumeWithParams_completionSelector (toMTROperationalStateClusterResumeParams params) completion

-- | @- resumeWithCompletion:@
resumeWithCompletion :: IsMTRBaseClusterOperationalState mtrBaseClusterOperationalState => mtrBaseClusterOperationalState -> Ptr () -> IO ()
resumeWithCompletion mtrBaseClusterOperationalState completion =
  sendMessage mtrBaseClusterOperationalState resumeWithCompletionSelector completion

-- | @- readAttributePhaseListWithCompletion:@
readAttributePhaseListWithCompletion :: IsMTRBaseClusterOperationalState mtrBaseClusterOperationalState => mtrBaseClusterOperationalState -> Ptr () -> IO ()
readAttributePhaseListWithCompletion mtrBaseClusterOperationalState completion =
  sendMessage mtrBaseClusterOperationalState readAttributePhaseListWithCompletionSelector completion

-- | @- subscribeAttributePhaseListWithParams:subscriptionEstablished:reportHandler:@
subscribeAttributePhaseListWithParams_subscriptionEstablished_reportHandler :: (IsMTRBaseClusterOperationalState mtrBaseClusterOperationalState, IsMTRSubscribeParams params) => mtrBaseClusterOperationalState -> params -> Ptr () -> Ptr () -> IO ()
subscribeAttributePhaseListWithParams_subscriptionEstablished_reportHandler mtrBaseClusterOperationalState params subscriptionEstablished reportHandler =
  sendMessage mtrBaseClusterOperationalState subscribeAttributePhaseListWithParams_subscriptionEstablished_reportHandlerSelector (toMTRSubscribeParams params) subscriptionEstablished reportHandler

-- | @+ readAttributePhaseListWithClusterStateCache:endpoint:queue:completion:@
readAttributePhaseListWithClusterStateCache_endpoint_queue_completion :: (IsMTRClusterStateCacheContainer clusterStateCacheContainer, IsNSNumber endpoint, IsNSObject queue) => clusterStateCacheContainer -> endpoint -> queue -> Ptr () -> IO ()
readAttributePhaseListWithClusterStateCache_endpoint_queue_completion clusterStateCacheContainer endpoint queue completion =
  do
    cls' <- getRequiredClass "MTRBaseClusterOperationalState"
    sendClassMessage cls' readAttributePhaseListWithClusterStateCache_endpoint_queue_completionSelector (toMTRClusterStateCacheContainer clusterStateCacheContainer) (toNSNumber endpoint) (toNSObject queue) completion

-- | @- readAttributeCurrentPhaseWithCompletion:@
readAttributeCurrentPhaseWithCompletion :: IsMTRBaseClusterOperationalState mtrBaseClusterOperationalState => mtrBaseClusterOperationalState -> Ptr () -> IO ()
readAttributeCurrentPhaseWithCompletion mtrBaseClusterOperationalState completion =
  sendMessage mtrBaseClusterOperationalState readAttributeCurrentPhaseWithCompletionSelector completion

-- | @- subscribeAttributeCurrentPhaseWithParams:subscriptionEstablished:reportHandler:@
subscribeAttributeCurrentPhaseWithParams_subscriptionEstablished_reportHandler :: (IsMTRBaseClusterOperationalState mtrBaseClusterOperationalState, IsMTRSubscribeParams params) => mtrBaseClusterOperationalState -> params -> Ptr () -> Ptr () -> IO ()
subscribeAttributeCurrentPhaseWithParams_subscriptionEstablished_reportHandler mtrBaseClusterOperationalState params subscriptionEstablished reportHandler =
  sendMessage mtrBaseClusterOperationalState subscribeAttributeCurrentPhaseWithParams_subscriptionEstablished_reportHandlerSelector (toMTRSubscribeParams params) subscriptionEstablished reportHandler

-- | @+ readAttributeCurrentPhaseWithClusterStateCache:endpoint:queue:completion:@
readAttributeCurrentPhaseWithClusterStateCache_endpoint_queue_completion :: (IsMTRClusterStateCacheContainer clusterStateCacheContainer, IsNSNumber endpoint, IsNSObject queue) => clusterStateCacheContainer -> endpoint -> queue -> Ptr () -> IO ()
readAttributeCurrentPhaseWithClusterStateCache_endpoint_queue_completion clusterStateCacheContainer endpoint queue completion =
  do
    cls' <- getRequiredClass "MTRBaseClusterOperationalState"
    sendClassMessage cls' readAttributeCurrentPhaseWithClusterStateCache_endpoint_queue_completionSelector (toMTRClusterStateCacheContainer clusterStateCacheContainer) (toNSNumber endpoint) (toNSObject queue) completion

-- | @- readAttributeCountdownTimeWithCompletion:@
readAttributeCountdownTimeWithCompletion :: IsMTRBaseClusterOperationalState mtrBaseClusterOperationalState => mtrBaseClusterOperationalState -> Ptr () -> IO ()
readAttributeCountdownTimeWithCompletion mtrBaseClusterOperationalState completion =
  sendMessage mtrBaseClusterOperationalState readAttributeCountdownTimeWithCompletionSelector completion

-- | @- subscribeAttributeCountdownTimeWithParams:subscriptionEstablished:reportHandler:@
subscribeAttributeCountdownTimeWithParams_subscriptionEstablished_reportHandler :: (IsMTRBaseClusterOperationalState mtrBaseClusterOperationalState, IsMTRSubscribeParams params) => mtrBaseClusterOperationalState -> params -> Ptr () -> Ptr () -> IO ()
subscribeAttributeCountdownTimeWithParams_subscriptionEstablished_reportHandler mtrBaseClusterOperationalState params subscriptionEstablished reportHandler =
  sendMessage mtrBaseClusterOperationalState subscribeAttributeCountdownTimeWithParams_subscriptionEstablished_reportHandlerSelector (toMTRSubscribeParams params) subscriptionEstablished reportHandler

-- | @+ readAttributeCountdownTimeWithClusterStateCache:endpoint:queue:completion:@
readAttributeCountdownTimeWithClusterStateCache_endpoint_queue_completion :: (IsMTRClusterStateCacheContainer clusterStateCacheContainer, IsNSNumber endpoint, IsNSObject queue) => clusterStateCacheContainer -> endpoint -> queue -> Ptr () -> IO ()
readAttributeCountdownTimeWithClusterStateCache_endpoint_queue_completion clusterStateCacheContainer endpoint queue completion =
  do
    cls' <- getRequiredClass "MTRBaseClusterOperationalState"
    sendClassMessage cls' readAttributeCountdownTimeWithClusterStateCache_endpoint_queue_completionSelector (toMTRClusterStateCacheContainer clusterStateCacheContainer) (toNSNumber endpoint) (toNSObject queue) completion

-- | @- readAttributeOperationalStateListWithCompletion:@
readAttributeOperationalStateListWithCompletion :: IsMTRBaseClusterOperationalState mtrBaseClusterOperationalState => mtrBaseClusterOperationalState -> Ptr () -> IO ()
readAttributeOperationalStateListWithCompletion mtrBaseClusterOperationalState completion =
  sendMessage mtrBaseClusterOperationalState readAttributeOperationalStateListWithCompletionSelector completion

-- | @- subscribeAttributeOperationalStateListWithParams:subscriptionEstablished:reportHandler:@
subscribeAttributeOperationalStateListWithParams_subscriptionEstablished_reportHandler :: (IsMTRBaseClusterOperationalState mtrBaseClusterOperationalState, IsMTRSubscribeParams params) => mtrBaseClusterOperationalState -> params -> Ptr () -> Ptr () -> IO ()
subscribeAttributeOperationalStateListWithParams_subscriptionEstablished_reportHandler mtrBaseClusterOperationalState params subscriptionEstablished reportHandler =
  sendMessage mtrBaseClusterOperationalState subscribeAttributeOperationalStateListWithParams_subscriptionEstablished_reportHandlerSelector (toMTRSubscribeParams params) subscriptionEstablished reportHandler

-- | @+ readAttributeOperationalStateListWithClusterStateCache:endpoint:queue:completion:@
readAttributeOperationalStateListWithClusterStateCache_endpoint_queue_completion :: (IsMTRClusterStateCacheContainer clusterStateCacheContainer, IsNSNumber endpoint, IsNSObject queue) => clusterStateCacheContainer -> endpoint -> queue -> Ptr () -> IO ()
readAttributeOperationalStateListWithClusterStateCache_endpoint_queue_completion clusterStateCacheContainer endpoint queue completion =
  do
    cls' <- getRequiredClass "MTRBaseClusterOperationalState"
    sendClassMessage cls' readAttributeOperationalStateListWithClusterStateCache_endpoint_queue_completionSelector (toMTRClusterStateCacheContainer clusterStateCacheContainer) (toNSNumber endpoint) (toNSObject queue) completion

-- | @- readAttributeOperationalStateWithCompletion:@
readAttributeOperationalStateWithCompletion :: IsMTRBaseClusterOperationalState mtrBaseClusterOperationalState => mtrBaseClusterOperationalState -> Ptr () -> IO ()
readAttributeOperationalStateWithCompletion mtrBaseClusterOperationalState completion =
  sendMessage mtrBaseClusterOperationalState readAttributeOperationalStateWithCompletionSelector completion

-- | @- subscribeAttributeOperationalStateWithParams:subscriptionEstablished:reportHandler:@
subscribeAttributeOperationalStateWithParams_subscriptionEstablished_reportHandler :: (IsMTRBaseClusterOperationalState mtrBaseClusterOperationalState, IsMTRSubscribeParams params) => mtrBaseClusterOperationalState -> params -> Ptr () -> Ptr () -> IO ()
subscribeAttributeOperationalStateWithParams_subscriptionEstablished_reportHandler mtrBaseClusterOperationalState params subscriptionEstablished reportHandler =
  sendMessage mtrBaseClusterOperationalState subscribeAttributeOperationalStateWithParams_subscriptionEstablished_reportHandlerSelector (toMTRSubscribeParams params) subscriptionEstablished reportHandler

-- | @+ readAttributeOperationalStateWithClusterStateCache:endpoint:queue:completion:@
readAttributeOperationalStateWithClusterStateCache_endpoint_queue_completion :: (IsMTRClusterStateCacheContainer clusterStateCacheContainer, IsNSNumber endpoint, IsNSObject queue) => clusterStateCacheContainer -> endpoint -> queue -> Ptr () -> IO ()
readAttributeOperationalStateWithClusterStateCache_endpoint_queue_completion clusterStateCacheContainer endpoint queue completion =
  do
    cls' <- getRequiredClass "MTRBaseClusterOperationalState"
    sendClassMessage cls' readAttributeOperationalStateWithClusterStateCache_endpoint_queue_completionSelector (toMTRClusterStateCacheContainer clusterStateCacheContainer) (toNSNumber endpoint) (toNSObject queue) completion

-- | @- readAttributeOperationalErrorWithCompletion:@
readAttributeOperationalErrorWithCompletion :: IsMTRBaseClusterOperationalState mtrBaseClusterOperationalState => mtrBaseClusterOperationalState -> Ptr () -> IO ()
readAttributeOperationalErrorWithCompletion mtrBaseClusterOperationalState completion =
  sendMessage mtrBaseClusterOperationalState readAttributeOperationalErrorWithCompletionSelector completion

-- | @- subscribeAttributeOperationalErrorWithParams:subscriptionEstablished:reportHandler:@
subscribeAttributeOperationalErrorWithParams_subscriptionEstablished_reportHandler :: (IsMTRBaseClusterOperationalState mtrBaseClusterOperationalState, IsMTRSubscribeParams params) => mtrBaseClusterOperationalState -> params -> Ptr () -> Ptr () -> IO ()
subscribeAttributeOperationalErrorWithParams_subscriptionEstablished_reportHandler mtrBaseClusterOperationalState params subscriptionEstablished reportHandler =
  sendMessage mtrBaseClusterOperationalState subscribeAttributeOperationalErrorWithParams_subscriptionEstablished_reportHandlerSelector (toMTRSubscribeParams params) subscriptionEstablished reportHandler

-- | @+ readAttributeOperationalErrorWithClusterStateCache:endpoint:queue:completion:@
readAttributeOperationalErrorWithClusterStateCache_endpoint_queue_completion :: (IsMTRClusterStateCacheContainer clusterStateCacheContainer, IsNSNumber endpoint, IsNSObject queue) => clusterStateCacheContainer -> endpoint -> queue -> Ptr () -> IO ()
readAttributeOperationalErrorWithClusterStateCache_endpoint_queue_completion clusterStateCacheContainer endpoint queue completion =
  do
    cls' <- getRequiredClass "MTRBaseClusterOperationalState"
    sendClassMessage cls' readAttributeOperationalErrorWithClusterStateCache_endpoint_queue_completionSelector (toMTRClusterStateCacheContainer clusterStateCacheContainer) (toNSNumber endpoint) (toNSObject queue) completion

-- | @- readAttributeGeneratedCommandListWithCompletion:@
readAttributeGeneratedCommandListWithCompletion :: IsMTRBaseClusterOperationalState mtrBaseClusterOperationalState => mtrBaseClusterOperationalState -> Ptr () -> IO ()
readAttributeGeneratedCommandListWithCompletion mtrBaseClusterOperationalState completion =
  sendMessage mtrBaseClusterOperationalState readAttributeGeneratedCommandListWithCompletionSelector completion

-- | @- subscribeAttributeGeneratedCommandListWithParams:subscriptionEstablished:reportHandler:@
subscribeAttributeGeneratedCommandListWithParams_subscriptionEstablished_reportHandler :: (IsMTRBaseClusterOperationalState mtrBaseClusterOperationalState, IsMTRSubscribeParams params) => mtrBaseClusterOperationalState -> params -> Ptr () -> Ptr () -> IO ()
subscribeAttributeGeneratedCommandListWithParams_subscriptionEstablished_reportHandler mtrBaseClusterOperationalState params subscriptionEstablished reportHandler =
  sendMessage mtrBaseClusterOperationalState subscribeAttributeGeneratedCommandListWithParams_subscriptionEstablished_reportHandlerSelector (toMTRSubscribeParams params) subscriptionEstablished reportHandler

-- | @+ readAttributeGeneratedCommandListWithClusterStateCache:endpoint:queue:completion:@
readAttributeGeneratedCommandListWithClusterStateCache_endpoint_queue_completion :: (IsMTRClusterStateCacheContainer clusterStateCacheContainer, IsNSNumber endpoint, IsNSObject queue) => clusterStateCacheContainer -> endpoint -> queue -> Ptr () -> IO ()
readAttributeGeneratedCommandListWithClusterStateCache_endpoint_queue_completion clusterStateCacheContainer endpoint queue completion =
  do
    cls' <- getRequiredClass "MTRBaseClusterOperationalState"
    sendClassMessage cls' readAttributeGeneratedCommandListWithClusterStateCache_endpoint_queue_completionSelector (toMTRClusterStateCacheContainer clusterStateCacheContainer) (toNSNumber endpoint) (toNSObject queue) completion

-- | @- readAttributeAcceptedCommandListWithCompletion:@
readAttributeAcceptedCommandListWithCompletion :: IsMTRBaseClusterOperationalState mtrBaseClusterOperationalState => mtrBaseClusterOperationalState -> Ptr () -> IO ()
readAttributeAcceptedCommandListWithCompletion mtrBaseClusterOperationalState completion =
  sendMessage mtrBaseClusterOperationalState readAttributeAcceptedCommandListWithCompletionSelector completion

-- | @- subscribeAttributeAcceptedCommandListWithParams:subscriptionEstablished:reportHandler:@
subscribeAttributeAcceptedCommandListWithParams_subscriptionEstablished_reportHandler :: (IsMTRBaseClusterOperationalState mtrBaseClusterOperationalState, IsMTRSubscribeParams params) => mtrBaseClusterOperationalState -> params -> Ptr () -> Ptr () -> IO ()
subscribeAttributeAcceptedCommandListWithParams_subscriptionEstablished_reportHandler mtrBaseClusterOperationalState params subscriptionEstablished reportHandler =
  sendMessage mtrBaseClusterOperationalState subscribeAttributeAcceptedCommandListWithParams_subscriptionEstablished_reportHandlerSelector (toMTRSubscribeParams params) subscriptionEstablished reportHandler

-- | @+ readAttributeAcceptedCommandListWithClusterStateCache:endpoint:queue:completion:@
readAttributeAcceptedCommandListWithClusterStateCache_endpoint_queue_completion :: (IsMTRClusterStateCacheContainer clusterStateCacheContainer, IsNSNumber endpoint, IsNSObject queue) => clusterStateCacheContainer -> endpoint -> queue -> Ptr () -> IO ()
readAttributeAcceptedCommandListWithClusterStateCache_endpoint_queue_completion clusterStateCacheContainer endpoint queue completion =
  do
    cls' <- getRequiredClass "MTRBaseClusterOperationalState"
    sendClassMessage cls' readAttributeAcceptedCommandListWithClusterStateCache_endpoint_queue_completionSelector (toMTRClusterStateCacheContainer clusterStateCacheContainer) (toNSNumber endpoint) (toNSObject queue) completion

-- | @- readAttributeAttributeListWithCompletion:@
readAttributeAttributeListWithCompletion :: IsMTRBaseClusterOperationalState mtrBaseClusterOperationalState => mtrBaseClusterOperationalState -> Ptr () -> IO ()
readAttributeAttributeListWithCompletion mtrBaseClusterOperationalState completion =
  sendMessage mtrBaseClusterOperationalState readAttributeAttributeListWithCompletionSelector completion

-- | @- subscribeAttributeAttributeListWithParams:subscriptionEstablished:reportHandler:@
subscribeAttributeAttributeListWithParams_subscriptionEstablished_reportHandler :: (IsMTRBaseClusterOperationalState mtrBaseClusterOperationalState, IsMTRSubscribeParams params) => mtrBaseClusterOperationalState -> params -> Ptr () -> Ptr () -> IO ()
subscribeAttributeAttributeListWithParams_subscriptionEstablished_reportHandler mtrBaseClusterOperationalState params subscriptionEstablished reportHandler =
  sendMessage mtrBaseClusterOperationalState subscribeAttributeAttributeListWithParams_subscriptionEstablished_reportHandlerSelector (toMTRSubscribeParams params) subscriptionEstablished reportHandler

-- | @+ readAttributeAttributeListWithClusterStateCache:endpoint:queue:completion:@
readAttributeAttributeListWithClusterStateCache_endpoint_queue_completion :: (IsMTRClusterStateCacheContainer clusterStateCacheContainer, IsNSNumber endpoint, IsNSObject queue) => clusterStateCacheContainer -> endpoint -> queue -> Ptr () -> IO ()
readAttributeAttributeListWithClusterStateCache_endpoint_queue_completion clusterStateCacheContainer endpoint queue completion =
  do
    cls' <- getRequiredClass "MTRBaseClusterOperationalState"
    sendClassMessage cls' readAttributeAttributeListWithClusterStateCache_endpoint_queue_completionSelector (toMTRClusterStateCacheContainer clusterStateCacheContainer) (toNSNumber endpoint) (toNSObject queue) completion

-- | @- readAttributeFeatureMapWithCompletion:@
readAttributeFeatureMapWithCompletion :: IsMTRBaseClusterOperationalState mtrBaseClusterOperationalState => mtrBaseClusterOperationalState -> Ptr () -> IO ()
readAttributeFeatureMapWithCompletion mtrBaseClusterOperationalState completion =
  sendMessage mtrBaseClusterOperationalState readAttributeFeatureMapWithCompletionSelector completion

-- | @- subscribeAttributeFeatureMapWithParams:subscriptionEstablished:reportHandler:@
subscribeAttributeFeatureMapWithParams_subscriptionEstablished_reportHandler :: (IsMTRBaseClusterOperationalState mtrBaseClusterOperationalState, IsMTRSubscribeParams params) => mtrBaseClusterOperationalState -> params -> Ptr () -> Ptr () -> IO ()
subscribeAttributeFeatureMapWithParams_subscriptionEstablished_reportHandler mtrBaseClusterOperationalState params subscriptionEstablished reportHandler =
  sendMessage mtrBaseClusterOperationalState subscribeAttributeFeatureMapWithParams_subscriptionEstablished_reportHandlerSelector (toMTRSubscribeParams params) subscriptionEstablished reportHandler

-- | @+ readAttributeFeatureMapWithClusterStateCache:endpoint:queue:completion:@
readAttributeFeatureMapWithClusterStateCache_endpoint_queue_completion :: (IsMTRClusterStateCacheContainer clusterStateCacheContainer, IsNSNumber endpoint, IsNSObject queue) => clusterStateCacheContainer -> endpoint -> queue -> Ptr () -> IO ()
readAttributeFeatureMapWithClusterStateCache_endpoint_queue_completion clusterStateCacheContainer endpoint queue completion =
  do
    cls' <- getRequiredClass "MTRBaseClusterOperationalState"
    sendClassMessage cls' readAttributeFeatureMapWithClusterStateCache_endpoint_queue_completionSelector (toMTRClusterStateCacheContainer clusterStateCacheContainer) (toNSNumber endpoint) (toNSObject queue) completion

-- | @- readAttributeClusterRevisionWithCompletion:@
readAttributeClusterRevisionWithCompletion :: IsMTRBaseClusterOperationalState mtrBaseClusterOperationalState => mtrBaseClusterOperationalState -> Ptr () -> IO ()
readAttributeClusterRevisionWithCompletion mtrBaseClusterOperationalState completion =
  sendMessage mtrBaseClusterOperationalState readAttributeClusterRevisionWithCompletionSelector completion

-- | @- subscribeAttributeClusterRevisionWithParams:subscriptionEstablished:reportHandler:@
subscribeAttributeClusterRevisionWithParams_subscriptionEstablished_reportHandler :: (IsMTRBaseClusterOperationalState mtrBaseClusterOperationalState, IsMTRSubscribeParams params) => mtrBaseClusterOperationalState -> params -> Ptr () -> Ptr () -> IO ()
subscribeAttributeClusterRevisionWithParams_subscriptionEstablished_reportHandler mtrBaseClusterOperationalState params subscriptionEstablished reportHandler =
  sendMessage mtrBaseClusterOperationalState subscribeAttributeClusterRevisionWithParams_subscriptionEstablished_reportHandlerSelector (toMTRSubscribeParams params) subscriptionEstablished reportHandler

-- | @+ readAttributeClusterRevisionWithClusterStateCache:endpoint:queue:completion:@
readAttributeClusterRevisionWithClusterStateCache_endpoint_queue_completion :: (IsMTRClusterStateCacheContainer clusterStateCacheContainer, IsNSNumber endpoint, IsNSObject queue) => clusterStateCacheContainer -> endpoint -> queue -> Ptr () -> IO ()
readAttributeClusterRevisionWithClusterStateCache_endpoint_queue_completion clusterStateCacheContainer endpoint queue completion =
  do
    cls' <- getRequiredClass "MTRBaseClusterOperationalState"
    sendClassMessage cls' readAttributeClusterRevisionWithClusterStateCache_endpoint_queue_completionSelector (toMTRClusterStateCacheContainer clusterStateCacheContainer) (toNSNumber endpoint) (toNSObject queue) completion

-- | @- init@
init_ :: IsMTRBaseClusterOperationalState mtrBaseClusterOperationalState => mtrBaseClusterOperationalState -> IO (Id MTRBaseClusterOperationalState)
init_ mtrBaseClusterOperationalState =
  sendOwnedMessage mtrBaseClusterOperationalState initSelector

-- | @+ new@
new :: IO (Id MTRBaseClusterOperationalState)
new  =
  do
    cls' <- getRequiredClass "MTRBaseClusterOperationalState"
    sendOwnedClassMessage cls' newSelector

-- | For all instance methods (reads, writes, commands) that take a completion, the completion will be called on the provided queue.
--
-- ObjC selector: @- initWithDevice:endpointID:queue:@
initWithDevice_endpointID_queue :: (IsMTRBaseClusterOperationalState mtrBaseClusterOperationalState, IsMTRBaseDevice device, IsNSNumber endpointID, IsNSObject queue) => mtrBaseClusterOperationalState -> device -> endpointID -> queue -> IO (Id MTRBaseClusterOperationalState)
initWithDevice_endpointID_queue mtrBaseClusterOperationalState device endpointID queue =
  sendOwnedMessage mtrBaseClusterOperationalState initWithDevice_endpointID_queueSelector (toMTRBaseDevice device) (toNSNumber endpointID) (toNSObject queue)

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @pauseWithParams:completion:@
pauseWithParams_completionSelector :: Selector '[Id MTROperationalStateClusterPauseParams, Ptr ()] ()
pauseWithParams_completionSelector = mkSelector "pauseWithParams:completion:"

-- | @Selector@ for @pauseWithCompletion:@
pauseWithCompletionSelector :: Selector '[Ptr ()] ()
pauseWithCompletionSelector = mkSelector "pauseWithCompletion:"

-- | @Selector@ for @stopWithParams:completion:@
stopWithParams_completionSelector :: Selector '[Id MTROperationalStateClusterStopParams, Ptr ()] ()
stopWithParams_completionSelector = mkSelector "stopWithParams:completion:"

-- | @Selector@ for @stopWithCompletion:@
stopWithCompletionSelector :: Selector '[Ptr ()] ()
stopWithCompletionSelector = mkSelector "stopWithCompletion:"

-- | @Selector@ for @startWithParams:completion:@
startWithParams_completionSelector :: Selector '[Id MTROperationalStateClusterStartParams, Ptr ()] ()
startWithParams_completionSelector = mkSelector "startWithParams:completion:"

-- | @Selector@ for @startWithCompletion:@
startWithCompletionSelector :: Selector '[Ptr ()] ()
startWithCompletionSelector = mkSelector "startWithCompletion:"

-- | @Selector@ for @resumeWithParams:completion:@
resumeWithParams_completionSelector :: Selector '[Id MTROperationalStateClusterResumeParams, Ptr ()] ()
resumeWithParams_completionSelector = mkSelector "resumeWithParams:completion:"

-- | @Selector@ for @resumeWithCompletion:@
resumeWithCompletionSelector :: Selector '[Ptr ()] ()
resumeWithCompletionSelector = mkSelector "resumeWithCompletion:"

-- | @Selector@ for @readAttributePhaseListWithCompletion:@
readAttributePhaseListWithCompletionSelector :: Selector '[Ptr ()] ()
readAttributePhaseListWithCompletionSelector = mkSelector "readAttributePhaseListWithCompletion:"

-- | @Selector@ for @subscribeAttributePhaseListWithParams:subscriptionEstablished:reportHandler:@
subscribeAttributePhaseListWithParams_subscriptionEstablished_reportHandlerSelector :: Selector '[Id MTRSubscribeParams, Ptr (), Ptr ()] ()
subscribeAttributePhaseListWithParams_subscriptionEstablished_reportHandlerSelector = mkSelector "subscribeAttributePhaseListWithParams:subscriptionEstablished:reportHandler:"

-- | @Selector@ for @readAttributePhaseListWithClusterStateCache:endpoint:queue:completion:@
readAttributePhaseListWithClusterStateCache_endpoint_queue_completionSelector :: Selector '[Id MTRClusterStateCacheContainer, Id NSNumber, Id NSObject, Ptr ()] ()
readAttributePhaseListWithClusterStateCache_endpoint_queue_completionSelector = mkSelector "readAttributePhaseListWithClusterStateCache:endpoint:queue:completion:"

-- | @Selector@ for @readAttributeCurrentPhaseWithCompletion:@
readAttributeCurrentPhaseWithCompletionSelector :: Selector '[Ptr ()] ()
readAttributeCurrentPhaseWithCompletionSelector = mkSelector "readAttributeCurrentPhaseWithCompletion:"

-- | @Selector@ for @subscribeAttributeCurrentPhaseWithParams:subscriptionEstablished:reportHandler:@
subscribeAttributeCurrentPhaseWithParams_subscriptionEstablished_reportHandlerSelector :: Selector '[Id MTRSubscribeParams, Ptr (), Ptr ()] ()
subscribeAttributeCurrentPhaseWithParams_subscriptionEstablished_reportHandlerSelector = mkSelector "subscribeAttributeCurrentPhaseWithParams:subscriptionEstablished:reportHandler:"

-- | @Selector@ for @readAttributeCurrentPhaseWithClusterStateCache:endpoint:queue:completion:@
readAttributeCurrentPhaseWithClusterStateCache_endpoint_queue_completionSelector :: Selector '[Id MTRClusterStateCacheContainer, Id NSNumber, Id NSObject, Ptr ()] ()
readAttributeCurrentPhaseWithClusterStateCache_endpoint_queue_completionSelector = mkSelector "readAttributeCurrentPhaseWithClusterStateCache:endpoint:queue:completion:"

-- | @Selector@ for @readAttributeCountdownTimeWithCompletion:@
readAttributeCountdownTimeWithCompletionSelector :: Selector '[Ptr ()] ()
readAttributeCountdownTimeWithCompletionSelector = mkSelector "readAttributeCountdownTimeWithCompletion:"

-- | @Selector@ for @subscribeAttributeCountdownTimeWithParams:subscriptionEstablished:reportHandler:@
subscribeAttributeCountdownTimeWithParams_subscriptionEstablished_reportHandlerSelector :: Selector '[Id MTRSubscribeParams, Ptr (), Ptr ()] ()
subscribeAttributeCountdownTimeWithParams_subscriptionEstablished_reportHandlerSelector = mkSelector "subscribeAttributeCountdownTimeWithParams:subscriptionEstablished:reportHandler:"

-- | @Selector@ for @readAttributeCountdownTimeWithClusterStateCache:endpoint:queue:completion:@
readAttributeCountdownTimeWithClusterStateCache_endpoint_queue_completionSelector :: Selector '[Id MTRClusterStateCacheContainer, Id NSNumber, Id NSObject, Ptr ()] ()
readAttributeCountdownTimeWithClusterStateCache_endpoint_queue_completionSelector = mkSelector "readAttributeCountdownTimeWithClusterStateCache:endpoint:queue:completion:"

-- | @Selector@ for @readAttributeOperationalStateListWithCompletion:@
readAttributeOperationalStateListWithCompletionSelector :: Selector '[Ptr ()] ()
readAttributeOperationalStateListWithCompletionSelector = mkSelector "readAttributeOperationalStateListWithCompletion:"

-- | @Selector@ for @subscribeAttributeOperationalStateListWithParams:subscriptionEstablished:reportHandler:@
subscribeAttributeOperationalStateListWithParams_subscriptionEstablished_reportHandlerSelector :: Selector '[Id MTRSubscribeParams, Ptr (), Ptr ()] ()
subscribeAttributeOperationalStateListWithParams_subscriptionEstablished_reportHandlerSelector = mkSelector "subscribeAttributeOperationalStateListWithParams:subscriptionEstablished:reportHandler:"

-- | @Selector@ for @readAttributeOperationalStateListWithClusterStateCache:endpoint:queue:completion:@
readAttributeOperationalStateListWithClusterStateCache_endpoint_queue_completionSelector :: Selector '[Id MTRClusterStateCacheContainer, Id NSNumber, Id NSObject, Ptr ()] ()
readAttributeOperationalStateListWithClusterStateCache_endpoint_queue_completionSelector = mkSelector "readAttributeOperationalStateListWithClusterStateCache:endpoint:queue:completion:"

-- | @Selector@ for @readAttributeOperationalStateWithCompletion:@
readAttributeOperationalStateWithCompletionSelector :: Selector '[Ptr ()] ()
readAttributeOperationalStateWithCompletionSelector = mkSelector "readAttributeOperationalStateWithCompletion:"

-- | @Selector@ for @subscribeAttributeOperationalStateWithParams:subscriptionEstablished:reportHandler:@
subscribeAttributeOperationalStateWithParams_subscriptionEstablished_reportHandlerSelector :: Selector '[Id MTRSubscribeParams, Ptr (), Ptr ()] ()
subscribeAttributeOperationalStateWithParams_subscriptionEstablished_reportHandlerSelector = mkSelector "subscribeAttributeOperationalStateWithParams:subscriptionEstablished:reportHandler:"

-- | @Selector@ for @readAttributeOperationalStateWithClusterStateCache:endpoint:queue:completion:@
readAttributeOperationalStateWithClusterStateCache_endpoint_queue_completionSelector :: Selector '[Id MTRClusterStateCacheContainer, Id NSNumber, Id NSObject, Ptr ()] ()
readAttributeOperationalStateWithClusterStateCache_endpoint_queue_completionSelector = mkSelector "readAttributeOperationalStateWithClusterStateCache:endpoint:queue:completion:"

-- | @Selector@ for @readAttributeOperationalErrorWithCompletion:@
readAttributeOperationalErrorWithCompletionSelector :: Selector '[Ptr ()] ()
readAttributeOperationalErrorWithCompletionSelector = mkSelector "readAttributeOperationalErrorWithCompletion:"

-- | @Selector@ for @subscribeAttributeOperationalErrorWithParams:subscriptionEstablished:reportHandler:@
subscribeAttributeOperationalErrorWithParams_subscriptionEstablished_reportHandlerSelector :: Selector '[Id MTRSubscribeParams, Ptr (), Ptr ()] ()
subscribeAttributeOperationalErrorWithParams_subscriptionEstablished_reportHandlerSelector = mkSelector "subscribeAttributeOperationalErrorWithParams:subscriptionEstablished:reportHandler:"

-- | @Selector@ for @readAttributeOperationalErrorWithClusterStateCache:endpoint:queue:completion:@
readAttributeOperationalErrorWithClusterStateCache_endpoint_queue_completionSelector :: Selector '[Id MTRClusterStateCacheContainer, Id NSNumber, Id NSObject, Ptr ()] ()
readAttributeOperationalErrorWithClusterStateCache_endpoint_queue_completionSelector = mkSelector "readAttributeOperationalErrorWithClusterStateCache:endpoint:queue:completion:"

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
initSelector :: Selector '[] (Id MTRBaseClusterOperationalState)
initSelector = mkSelector "init"

-- | @Selector@ for @new@
newSelector :: Selector '[] (Id MTRBaseClusterOperationalState)
newSelector = mkSelector "new"

-- | @Selector@ for @initWithDevice:endpointID:queue:@
initWithDevice_endpointID_queueSelector :: Selector '[Id MTRBaseDevice, Id NSNumber, Id NSObject] (Id MTRBaseClusterOperationalState)
initWithDevice_endpointID_queueSelector = mkSelector "initWithDevice:endpointID:queue:"

