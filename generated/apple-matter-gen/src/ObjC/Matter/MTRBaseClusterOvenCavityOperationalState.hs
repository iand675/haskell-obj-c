{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Cluster Oven Cavity Operational State
--
-- This cluster supports remotely monitoring and, where supported, changing the operational state of an Oven.
--
-- Generated bindings for @MTRBaseClusterOvenCavityOperationalState@.
module ObjC.Matter.MTRBaseClusterOvenCavityOperationalState
  ( MTRBaseClusterOvenCavityOperationalState
  , IsMTRBaseClusterOvenCavityOperationalState(..)
  , stopWithParams_completion
  , stopWithCompletion
  , startWithParams_completion
  , startWithCompletion
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

-- | Command Stop
--
-- Upon receipt, the device SHALL stop its operation if it is at a position where it is safe to do so and/or permitted.
--
-- ObjC selector: @- stopWithParams:completion:@
stopWithParams_completion :: (IsMTRBaseClusterOvenCavityOperationalState mtrBaseClusterOvenCavityOperationalState, IsMTROvenCavityOperationalStateClusterStopParams params) => mtrBaseClusterOvenCavityOperationalState -> params -> Ptr () -> IO ()
stopWithParams_completion mtrBaseClusterOvenCavityOperationalState params completion =
  sendMessage mtrBaseClusterOvenCavityOperationalState stopWithParams_completionSelector (toMTROvenCavityOperationalStateClusterStopParams params) completion

-- | @- stopWithCompletion:@
stopWithCompletion :: IsMTRBaseClusterOvenCavityOperationalState mtrBaseClusterOvenCavityOperationalState => mtrBaseClusterOvenCavityOperationalState -> Ptr () -> IO ()
stopWithCompletion mtrBaseClusterOvenCavityOperationalState completion =
  sendMessage mtrBaseClusterOvenCavityOperationalState stopWithCompletionSelector completion

-- | Command Start
--
-- Upon receipt, the device SHALL start its operation if it is safe to do so and the device is in an operational state from which it can be started.
--
-- ObjC selector: @- startWithParams:completion:@
startWithParams_completion :: (IsMTRBaseClusterOvenCavityOperationalState mtrBaseClusterOvenCavityOperationalState, IsMTROvenCavityOperationalStateClusterStartParams params) => mtrBaseClusterOvenCavityOperationalState -> params -> Ptr () -> IO ()
startWithParams_completion mtrBaseClusterOvenCavityOperationalState params completion =
  sendMessage mtrBaseClusterOvenCavityOperationalState startWithParams_completionSelector (toMTROvenCavityOperationalStateClusterStartParams params) completion

-- | @- startWithCompletion:@
startWithCompletion :: IsMTRBaseClusterOvenCavityOperationalState mtrBaseClusterOvenCavityOperationalState => mtrBaseClusterOvenCavityOperationalState -> Ptr () -> IO ()
startWithCompletion mtrBaseClusterOvenCavityOperationalState completion =
  sendMessage mtrBaseClusterOvenCavityOperationalState startWithCompletionSelector completion

-- | @- readAttributePhaseListWithCompletion:@
readAttributePhaseListWithCompletion :: IsMTRBaseClusterOvenCavityOperationalState mtrBaseClusterOvenCavityOperationalState => mtrBaseClusterOvenCavityOperationalState -> Ptr () -> IO ()
readAttributePhaseListWithCompletion mtrBaseClusterOvenCavityOperationalState completion =
  sendMessage mtrBaseClusterOvenCavityOperationalState readAttributePhaseListWithCompletionSelector completion

-- | @- subscribeAttributePhaseListWithParams:subscriptionEstablished:reportHandler:@
subscribeAttributePhaseListWithParams_subscriptionEstablished_reportHandler :: (IsMTRBaseClusterOvenCavityOperationalState mtrBaseClusterOvenCavityOperationalState, IsMTRSubscribeParams params) => mtrBaseClusterOvenCavityOperationalState -> params -> Ptr () -> Ptr () -> IO ()
subscribeAttributePhaseListWithParams_subscriptionEstablished_reportHandler mtrBaseClusterOvenCavityOperationalState params subscriptionEstablished reportHandler =
  sendMessage mtrBaseClusterOvenCavityOperationalState subscribeAttributePhaseListWithParams_subscriptionEstablished_reportHandlerSelector (toMTRSubscribeParams params) subscriptionEstablished reportHandler

-- | @+ readAttributePhaseListWithClusterStateCache:endpoint:queue:completion:@
readAttributePhaseListWithClusterStateCache_endpoint_queue_completion :: (IsMTRClusterStateCacheContainer clusterStateCacheContainer, IsNSNumber endpoint, IsNSObject queue) => clusterStateCacheContainer -> endpoint -> queue -> Ptr () -> IO ()
readAttributePhaseListWithClusterStateCache_endpoint_queue_completion clusterStateCacheContainer endpoint queue completion =
  do
    cls' <- getRequiredClass "MTRBaseClusterOvenCavityOperationalState"
    sendClassMessage cls' readAttributePhaseListWithClusterStateCache_endpoint_queue_completionSelector (toMTRClusterStateCacheContainer clusterStateCacheContainer) (toNSNumber endpoint) (toNSObject queue) completion

-- | @- readAttributeCurrentPhaseWithCompletion:@
readAttributeCurrentPhaseWithCompletion :: IsMTRBaseClusterOvenCavityOperationalState mtrBaseClusterOvenCavityOperationalState => mtrBaseClusterOvenCavityOperationalState -> Ptr () -> IO ()
readAttributeCurrentPhaseWithCompletion mtrBaseClusterOvenCavityOperationalState completion =
  sendMessage mtrBaseClusterOvenCavityOperationalState readAttributeCurrentPhaseWithCompletionSelector completion

-- | @- subscribeAttributeCurrentPhaseWithParams:subscriptionEstablished:reportHandler:@
subscribeAttributeCurrentPhaseWithParams_subscriptionEstablished_reportHandler :: (IsMTRBaseClusterOvenCavityOperationalState mtrBaseClusterOvenCavityOperationalState, IsMTRSubscribeParams params) => mtrBaseClusterOvenCavityOperationalState -> params -> Ptr () -> Ptr () -> IO ()
subscribeAttributeCurrentPhaseWithParams_subscriptionEstablished_reportHandler mtrBaseClusterOvenCavityOperationalState params subscriptionEstablished reportHandler =
  sendMessage mtrBaseClusterOvenCavityOperationalState subscribeAttributeCurrentPhaseWithParams_subscriptionEstablished_reportHandlerSelector (toMTRSubscribeParams params) subscriptionEstablished reportHandler

-- | @+ readAttributeCurrentPhaseWithClusterStateCache:endpoint:queue:completion:@
readAttributeCurrentPhaseWithClusterStateCache_endpoint_queue_completion :: (IsMTRClusterStateCacheContainer clusterStateCacheContainer, IsNSNumber endpoint, IsNSObject queue) => clusterStateCacheContainer -> endpoint -> queue -> Ptr () -> IO ()
readAttributeCurrentPhaseWithClusterStateCache_endpoint_queue_completion clusterStateCacheContainer endpoint queue completion =
  do
    cls' <- getRequiredClass "MTRBaseClusterOvenCavityOperationalState"
    sendClassMessage cls' readAttributeCurrentPhaseWithClusterStateCache_endpoint_queue_completionSelector (toMTRClusterStateCacheContainer clusterStateCacheContainer) (toNSNumber endpoint) (toNSObject queue) completion

-- | @- readAttributeCountdownTimeWithCompletion:@
readAttributeCountdownTimeWithCompletion :: IsMTRBaseClusterOvenCavityOperationalState mtrBaseClusterOvenCavityOperationalState => mtrBaseClusterOvenCavityOperationalState -> Ptr () -> IO ()
readAttributeCountdownTimeWithCompletion mtrBaseClusterOvenCavityOperationalState completion =
  sendMessage mtrBaseClusterOvenCavityOperationalState readAttributeCountdownTimeWithCompletionSelector completion

-- | @- subscribeAttributeCountdownTimeWithParams:subscriptionEstablished:reportHandler:@
subscribeAttributeCountdownTimeWithParams_subscriptionEstablished_reportHandler :: (IsMTRBaseClusterOvenCavityOperationalState mtrBaseClusterOvenCavityOperationalState, IsMTRSubscribeParams params) => mtrBaseClusterOvenCavityOperationalState -> params -> Ptr () -> Ptr () -> IO ()
subscribeAttributeCountdownTimeWithParams_subscriptionEstablished_reportHandler mtrBaseClusterOvenCavityOperationalState params subscriptionEstablished reportHandler =
  sendMessage mtrBaseClusterOvenCavityOperationalState subscribeAttributeCountdownTimeWithParams_subscriptionEstablished_reportHandlerSelector (toMTRSubscribeParams params) subscriptionEstablished reportHandler

-- | @+ readAttributeCountdownTimeWithClusterStateCache:endpoint:queue:completion:@
readAttributeCountdownTimeWithClusterStateCache_endpoint_queue_completion :: (IsMTRClusterStateCacheContainer clusterStateCacheContainer, IsNSNumber endpoint, IsNSObject queue) => clusterStateCacheContainer -> endpoint -> queue -> Ptr () -> IO ()
readAttributeCountdownTimeWithClusterStateCache_endpoint_queue_completion clusterStateCacheContainer endpoint queue completion =
  do
    cls' <- getRequiredClass "MTRBaseClusterOvenCavityOperationalState"
    sendClassMessage cls' readAttributeCountdownTimeWithClusterStateCache_endpoint_queue_completionSelector (toMTRClusterStateCacheContainer clusterStateCacheContainer) (toNSNumber endpoint) (toNSObject queue) completion

-- | @- readAttributeOperationalStateListWithCompletion:@
readAttributeOperationalStateListWithCompletion :: IsMTRBaseClusterOvenCavityOperationalState mtrBaseClusterOvenCavityOperationalState => mtrBaseClusterOvenCavityOperationalState -> Ptr () -> IO ()
readAttributeOperationalStateListWithCompletion mtrBaseClusterOvenCavityOperationalState completion =
  sendMessage mtrBaseClusterOvenCavityOperationalState readAttributeOperationalStateListWithCompletionSelector completion

-- | @- subscribeAttributeOperationalStateListWithParams:subscriptionEstablished:reportHandler:@
subscribeAttributeOperationalStateListWithParams_subscriptionEstablished_reportHandler :: (IsMTRBaseClusterOvenCavityOperationalState mtrBaseClusterOvenCavityOperationalState, IsMTRSubscribeParams params) => mtrBaseClusterOvenCavityOperationalState -> params -> Ptr () -> Ptr () -> IO ()
subscribeAttributeOperationalStateListWithParams_subscriptionEstablished_reportHandler mtrBaseClusterOvenCavityOperationalState params subscriptionEstablished reportHandler =
  sendMessage mtrBaseClusterOvenCavityOperationalState subscribeAttributeOperationalStateListWithParams_subscriptionEstablished_reportHandlerSelector (toMTRSubscribeParams params) subscriptionEstablished reportHandler

-- | @+ readAttributeOperationalStateListWithClusterStateCache:endpoint:queue:completion:@
readAttributeOperationalStateListWithClusterStateCache_endpoint_queue_completion :: (IsMTRClusterStateCacheContainer clusterStateCacheContainer, IsNSNumber endpoint, IsNSObject queue) => clusterStateCacheContainer -> endpoint -> queue -> Ptr () -> IO ()
readAttributeOperationalStateListWithClusterStateCache_endpoint_queue_completion clusterStateCacheContainer endpoint queue completion =
  do
    cls' <- getRequiredClass "MTRBaseClusterOvenCavityOperationalState"
    sendClassMessage cls' readAttributeOperationalStateListWithClusterStateCache_endpoint_queue_completionSelector (toMTRClusterStateCacheContainer clusterStateCacheContainer) (toNSNumber endpoint) (toNSObject queue) completion

-- | @- readAttributeOperationalStateWithCompletion:@
readAttributeOperationalStateWithCompletion :: IsMTRBaseClusterOvenCavityOperationalState mtrBaseClusterOvenCavityOperationalState => mtrBaseClusterOvenCavityOperationalState -> Ptr () -> IO ()
readAttributeOperationalStateWithCompletion mtrBaseClusterOvenCavityOperationalState completion =
  sendMessage mtrBaseClusterOvenCavityOperationalState readAttributeOperationalStateWithCompletionSelector completion

-- | @- subscribeAttributeOperationalStateWithParams:subscriptionEstablished:reportHandler:@
subscribeAttributeOperationalStateWithParams_subscriptionEstablished_reportHandler :: (IsMTRBaseClusterOvenCavityOperationalState mtrBaseClusterOvenCavityOperationalState, IsMTRSubscribeParams params) => mtrBaseClusterOvenCavityOperationalState -> params -> Ptr () -> Ptr () -> IO ()
subscribeAttributeOperationalStateWithParams_subscriptionEstablished_reportHandler mtrBaseClusterOvenCavityOperationalState params subscriptionEstablished reportHandler =
  sendMessage mtrBaseClusterOvenCavityOperationalState subscribeAttributeOperationalStateWithParams_subscriptionEstablished_reportHandlerSelector (toMTRSubscribeParams params) subscriptionEstablished reportHandler

-- | @+ readAttributeOperationalStateWithClusterStateCache:endpoint:queue:completion:@
readAttributeOperationalStateWithClusterStateCache_endpoint_queue_completion :: (IsMTRClusterStateCacheContainer clusterStateCacheContainer, IsNSNumber endpoint, IsNSObject queue) => clusterStateCacheContainer -> endpoint -> queue -> Ptr () -> IO ()
readAttributeOperationalStateWithClusterStateCache_endpoint_queue_completion clusterStateCacheContainer endpoint queue completion =
  do
    cls' <- getRequiredClass "MTRBaseClusterOvenCavityOperationalState"
    sendClassMessage cls' readAttributeOperationalStateWithClusterStateCache_endpoint_queue_completionSelector (toMTRClusterStateCacheContainer clusterStateCacheContainer) (toNSNumber endpoint) (toNSObject queue) completion

-- | @- readAttributeOperationalErrorWithCompletion:@
readAttributeOperationalErrorWithCompletion :: IsMTRBaseClusterOvenCavityOperationalState mtrBaseClusterOvenCavityOperationalState => mtrBaseClusterOvenCavityOperationalState -> Ptr () -> IO ()
readAttributeOperationalErrorWithCompletion mtrBaseClusterOvenCavityOperationalState completion =
  sendMessage mtrBaseClusterOvenCavityOperationalState readAttributeOperationalErrorWithCompletionSelector completion

-- | @- subscribeAttributeOperationalErrorWithParams:subscriptionEstablished:reportHandler:@
subscribeAttributeOperationalErrorWithParams_subscriptionEstablished_reportHandler :: (IsMTRBaseClusterOvenCavityOperationalState mtrBaseClusterOvenCavityOperationalState, IsMTRSubscribeParams params) => mtrBaseClusterOvenCavityOperationalState -> params -> Ptr () -> Ptr () -> IO ()
subscribeAttributeOperationalErrorWithParams_subscriptionEstablished_reportHandler mtrBaseClusterOvenCavityOperationalState params subscriptionEstablished reportHandler =
  sendMessage mtrBaseClusterOvenCavityOperationalState subscribeAttributeOperationalErrorWithParams_subscriptionEstablished_reportHandlerSelector (toMTRSubscribeParams params) subscriptionEstablished reportHandler

-- | @+ readAttributeOperationalErrorWithClusterStateCache:endpoint:queue:completion:@
readAttributeOperationalErrorWithClusterStateCache_endpoint_queue_completion :: (IsMTRClusterStateCacheContainer clusterStateCacheContainer, IsNSNumber endpoint, IsNSObject queue) => clusterStateCacheContainer -> endpoint -> queue -> Ptr () -> IO ()
readAttributeOperationalErrorWithClusterStateCache_endpoint_queue_completion clusterStateCacheContainer endpoint queue completion =
  do
    cls' <- getRequiredClass "MTRBaseClusterOvenCavityOperationalState"
    sendClassMessage cls' readAttributeOperationalErrorWithClusterStateCache_endpoint_queue_completionSelector (toMTRClusterStateCacheContainer clusterStateCacheContainer) (toNSNumber endpoint) (toNSObject queue) completion

-- | @- readAttributeGeneratedCommandListWithCompletion:@
readAttributeGeneratedCommandListWithCompletion :: IsMTRBaseClusterOvenCavityOperationalState mtrBaseClusterOvenCavityOperationalState => mtrBaseClusterOvenCavityOperationalState -> Ptr () -> IO ()
readAttributeGeneratedCommandListWithCompletion mtrBaseClusterOvenCavityOperationalState completion =
  sendMessage mtrBaseClusterOvenCavityOperationalState readAttributeGeneratedCommandListWithCompletionSelector completion

-- | @- subscribeAttributeGeneratedCommandListWithParams:subscriptionEstablished:reportHandler:@
subscribeAttributeGeneratedCommandListWithParams_subscriptionEstablished_reportHandler :: (IsMTRBaseClusterOvenCavityOperationalState mtrBaseClusterOvenCavityOperationalState, IsMTRSubscribeParams params) => mtrBaseClusterOvenCavityOperationalState -> params -> Ptr () -> Ptr () -> IO ()
subscribeAttributeGeneratedCommandListWithParams_subscriptionEstablished_reportHandler mtrBaseClusterOvenCavityOperationalState params subscriptionEstablished reportHandler =
  sendMessage mtrBaseClusterOvenCavityOperationalState subscribeAttributeGeneratedCommandListWithParams_subscriptionEstablished_reportHandlerSelector (toMTRSubscribeParams params) subscriptionEstablished reportHandler

-- | @+ readAttributeGeneratedCommandListWithClusterStateCache:endpoint:queue:completion:@
readAttributeGeneratedCommandListWithClusterStateCache_endpoint_queue_completion :: (IsMTRClusterStateCacheContainer clusterStateCacheContainer, IsNSNumber endpoint, IsNSObject queue) => clusterStateCacheContainer -> endpoint -> queue -> Ptr () -> IO ()
readAttributeGeneratedCommandListWithClusterStateCache_endpoint_queue_completion clusterStateCacheContainer endpoint queue completion =
  do
    cls' <- getRequiredClass "MTRBaseClusterOvenCavityOperationalState"
    sendClassMessage cls' readAttributeGeneratedCommandListWithClusterStateCache_endpoint_queue_completionSelector (toMTRClusterStateCacheContainer clusterStateCacheContainer) (toNSNumber endpoint) (toNSObject queue) completion

-- | @- readAttributeAcceptedCommandListWithCompletion:@
readAttributeAcceptedCommandListWithCompletion :: IsMTRBaseClusterOvenCavityOperationalState mtrBaseClusterOvenCavityOperationalState => mtrBaseClusterOvenCavityOperationalState -> Ptr () -> IO ()
readAttributeAcceptedCommandListWithCompletion mtrBaseClusterOvenCavityOperationalState completion =
  sendMessage mtrBaseClusterOvenCavityOperationalState readAttributeAcceptedCommandListWithCompletionSelector completion

-- | @- subscribeAttributeAcceptedCommandListWithParams:subscriptionEstablished:reportHandler:@
subscribeAttributeAcceptedCommandListWithParams_subscriptionEstablished_reportHandler :: (IsMTRBaseClusterOvenCavityOperationalState mtrBaseClusterOvenCavityOperationalState, IsMTRSubscribeParams params) => mtrBaseClusterOvenCavityOperationalState -> params -> Ptr () -> Ptr () -> IO ()
subscribeAttributeAcceptedCommandListWithParams_subscriptionEstablished_reportHandler mtrBaseClusterOvenCavityOperationalState params subscriptionEstablished reportHandler =
  sendMessage mtrBaseClusterOvenCavityOperationalState subscribeAttributeAcceptedCommandListWithParams_subscriptionEstablished_reportHandlerSelector (toMTRSubscribeParams params) subscriptionEstablished reportHandler

-- | @+ readAttributeAcceptedCommandListWithClusterStateCache:endpoint:queue:completion:@
readAttributeAcceptedCommandListWithClusterStateCache_endpoint_queue_completion :: (IsMTRClusterStateCacheContainer clusterStateCacheContainer, IsNSNumber endpoint, IsNSObject queue) => clusterStateCacheContainer -> endpoint -> queue -> Ptr () -> IO ()
readAttributeAcceptedCommandListWithClusterStateCache_endpoint_queue_completion clusterStateCacheContainer endpoint queue completion =
  do
    cls' <- getRequiredClass "MTRBaseClusterOvenCavityOperationalState"
    sendClassMessage cls' readAttributeAcceptedCommandListWithClusterStateCache_endpoint_queue_completionSelector (toMTRClusterStateCacheContainer clusterStateCacheContainer) (toNSNumber endpoint) (toNSObject queue) completion

-- | @- readAttributeAttributeListWithCompletion:@
readAttributeAttributeListWithCompletion :: IsMTRBaseClusterOvenCavityOperationalState mtrBaseClusterOvenCavityOperationalState => mtrBaseClusterOvenCavityOperationalState -> Ptr () -> IO ()
readAttributeAttributeListWithCompletion mtrBaseClusterOvenCavityOperationalState completion =
  sendMessage mtrBaseClusterOvenCavityOperationalState readAttributeAttributeListWithCompletionSelector completion

-- | @- subscribeAttributeAttributeListWithParams:subscriptionEstablished:reportHandler:@
subscribeAttributeAttributeListWithParams_subscriptionEstablished_reportHandler :: (IsMTRBaseClusterOvenCavityOperationalState mtrBaseClusterOvenCavityOperationalState, IsMTRSubscribeParams params) => mtrBaseClusterOvenCavityOperationalState -> params -> Ptr () -> Ptr () -> IO ()
subscribeAttributeAttributeListWithParams_subscriptionEstablished_reportHandler mtrBaseClusterOvenCavityOperationalState params subscriptionEstablished reportHandler =
  sendMessage mtrBaseClusterOvenCavityOperationalState subscribeAttributeAttributeListWithParams_subscriptionEstablished_reportHandlerSelector (toMTRSubscribeParams params) subscriptionEstablished reportHandler

-- | @+ readAttributeAttributeListWithClusterStateCache:endpoint:queue:completion:@
readAttributeAttributeListWithClusterStateCache_endpoint_queue_completion :: (IsMTRClusterStateCacheContainer clusterStateCacheContainer, IsNSNumber endpoint, IsNSObject queue) => clusterStateCacheContainer -> endpoint -> queue -> Ptr () -> IO ()
readAttributeAttributeListWithClusterStateCache_endpoint_queue_completion clusterStateCacheContainer endpoint queue completion =
  do
    cls' <- getRequiredClass "MTRBaseClusterOvenCavityOperationalState"
    sendClassMessage cls' readAttributeAttributeListWithClusterStateCache_endpoint_queue_completionSelector (toMTRClusterStateCacheContainer clusterStateCacheContainer) (toNSNumber endpoint) (toNSObject queue) completion

-- | @- readAttributeFeatureMapWithCompletion:@
readAttributeFeatureMapWithCompletion :: IsMTRBaseClusterOvenCavityOperationalState mtrBaseClusterOvenCavityOperationalState => mtrBaseClusterOvenCavityOperationalState -> Ptr () -> IO ()
readAttributeFeatureMapWithCompletion mtrBaseClusterOvenCavityOperationalState completion =
  sendMessage mtrBaseClusterOvenCavityOperationalState readAttributeFeatureMapWithCompletionSelector completion

-- | @- subscribeAttributeFeatureMapWithParams:subscriptionEstablished:reportHandler:@
subscribeAttributeFeatureMapWithParams_subscriptionEstablished_reportHandler :: (IsMTRBaseClusterOvenCavityOperationalState mtrBaseClusterOvenCavityOperationalState, IsMTRSubscribeParams params) => mtrBaseClusterOvenCavityOperationalState -> params -> Ptr () -> Ptr () -> IO ()
subscribeAttributeFeatureMapWithParams_subscriptionEstablished_reportHandler mtrBaseClusterOvenCavityOperationalState params subscriptionEstablished reportHandler =
  sendMessage mtrBaseClusterOvenCavityOperationalState subscribeAttributeFeatureMapWithParams_subscriptionEstablished_reportHandlerSelector (toMTRSubscribeParams params) subscriptionEstablished reportHandler

-- | @+ readAttributeFeatureMapWithClusterStateCache:endpoint:queue:completion:@
readAttributeFeatureMapWithClusterStateCache_endpoint_queue_completion :: (IsMTRClusterStateCacheContainer clusterStateCacheContainer, IsNSNumber endpoint, IsNSObject queue) => clusterStateCacheContainer -> endpoint -> queue -> Ptr () -> IO ()
readAttributeFeatureMapWithClusterStateCache_endpoint_queue_completion clusterStateCacheContainer endpoint queue completion =
  do
    cls' <- getRequiredClass "MTRBaseClusterOvenCavityOperationalState"
    sendClassMessage cls' readAttributeFeatureMapWithClusterStateCache_endpoint_queue_completionSelector (toMTRClusterStateCacheContainer clusterStateCacheContainer) (toNSNumber endpoint) (toNSObject queue) completion

-- | @- readAttributeClusterRevisionWithCompletion:@
readAttributeClusterRevisionWithCompletion :: IsMTRBaseClusterOvenCavityOperationalState mtrBaseClusterOvenCavityOperationalState => mtrBaseClusterOvenCavityOperationalState -> Ptr () -> IO ()
readAttributeClusterRevisionWithCompletion mtrBaseClusterOvenCavityOperationalState completion =
  sendMessage mtrBaseClusterOvenCavityOperationalState readAttributeClusterRevisionWithCompletionSelector completion

-- | @- subscribeAttributeClusterRevisionWithParams:subscriptionEstablished:reportHandler:@
subscribeAttributeClusterRevisionWithParams_subscriptionEstablished_reportHandler :: (IsMTRBaseClusterOvenCavityOperationalState mtrBaseClusterOvenCavityOperationalState, IsMTRSubscribeParams params) => mtrBaseClusterOvenCavityOperationalState -> params -> Ptr () -> Ptr () -> IO ()
subscribeAttributeClusterRevisionWithParams_subscriptionEstablished_reportHandler mtrBaseClusterOvenCavityOperationalState params subscriptionEstablished reportHandler =
  sendMessage mtrBaseClusterOvenCavityOperationalState subscribeAttributeClusterRevisionWithParams_subscriptionEstablished_reportHandlerSelector (toMTRSubscribeParams params) subscriptionEstablished reportHandler

-- | @+ readAttributeClusterRevisionWithClusterStateCache:endpoint:queue:completion:@
readAttributeClusterRevisionWithClusterStateCache_endpoint_queue_completion :: (IsMTRClusterStateCacheContainer clusterStateCacheContainer, IsNSNumber endpoint, IsNSObject queue) => clusterStateCacheContainer -> endpoint -> queue -> Ptr () -> IO ()
readAttributeClusterRevisionWithClusterStateCache_endpoint_queue_completion clusterStateCacheContainer endpoint queue completion =
  do
    cls' <- getRequiredClass "MTRBaseClusterOvenCavityOperationalState"
    sendClassMessage cls' readAttributeClusterRevisionWithClusterStateCache_endpoint_queue_completionSelector (toMTRClusterStateCacheContainer clusterStateCacheContainer) (toNSNumber endpoint) (toNSObject queue) completion

-- | @- init@
init_ :: IsMTRBaseClusterOvenCavityOperationalState mtrBaseClusterOvenCavityOperationalState => mtrBaseClusterOvenCavityOperationalState -> IO (Id MTRBaseClusterOvenCavityOperationalState)
init_ mtrBaseClusterOvenCavityOperationalState =
  sendOwnedMessage mtrBaseClusterOvenCavityOperationalState initSelector

-- | @+ new@
new :: IO (Id MTRBaseClusterOvenCavityOperationalState)
new  =
  do
    cls' <- getRequiredClass "MTRBaseClusterOvenCavityOperationalState"
    sendOwnedClassMessage cls' newSelector

-- | For all instance methods (reads, writes, commands) that take a completion, the completion will be called on the provided queue.
--
-- ObjC selector: @- initWithDevice:endpointID:queue:@
initWithDevice_endpointID_queue :: (IsMTRBaseClusterOvenCavityOperationalState mtrBaseClusterOvenCavityOperationalState, IsMTRBaseDevice device, IsNSNumber endpointID, IsNSObject queue) => mtrBaseClusterOvenCavityOperationalState -> device -> endpointID -> queue -> IO (Id MTRBaseClusterOvenCavityOperationalState)
initWithDevice_endpointID_queue mtrBaseClusterOvenCavityOperationalState device endpointID queue =
  sendOwnedMessage mtrBaseClusterOvenCavityOperationalState initWithDevice_endpointID_queueSelector (toMTRBaseDevice device) (toNSNumber endpointID) (toNSObject queue)

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @stopWithParams:completion:@
stopWithParams_completionSelector :: Selector '[Id MTROvenCavityOperationalStateClusterStopParams, Ptr ()] ()
stopWithParams_completionSelector = mkSelector "stopWithParams:completion:"

-- | @Selector@ for @stopWithCompletion:@
stopWithCompletionSelector :: Selector '[Ptr ()] ()
stopWithCompletionSelector = mkSelector "stopWithCompletion:"

-- | @Selector@ for @startWithParams:completion:@
startWithParams_completionSelector :: Selector '[Id MTROvenCavityOperationalStateClusterStartParams, Ptr ()] ()
startWithParams_completionSelector = mkSelector "startWithParams:completion:"

-- | @Selector@ for @startWithCompletion:@
startWithCompletionSelector :: Selector '[Ptr ()] ()
startWithCompletionSelector = mkSelector "startWithCompletion:"

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
initSelector :: Selector '[] (Id MTRBaseClusterOvenCavityOperationalState)
initSelector = mkSelector "init"

-- | @Selector@ for @new@
newSelector :: Selector '[] (Id MTRBaseClusterOvenCavityOperationalState)
newSelector = mkSelector "new"

-- | @Selector@ for @initWithDevice:endpointID:queue:@
initWithDevice_endpointID_queueSelector :: Selector '[Id MTRBaseDevice, Id NSNumber, Id NSObject] (Id MTRBaseClusterOvenCavityOperationalState)
initWithDevice_endpointID_queueSelector = mkSelector "initWithDevice:endpointID:queue:"

