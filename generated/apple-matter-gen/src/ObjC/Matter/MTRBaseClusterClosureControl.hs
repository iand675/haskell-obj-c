{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Cluster Closure Control
--
-- This cluster provides an interface for controlling a Closure.
--
-- Generated bindings for @MTRBaseClusterClosureControl@.
module ObjC.Matter.MTRBaseClusterClosureControl
  ( MTRBaseClusterClosureControl
  , IsMTRBaseClusterClosureControl(..)
  , stopWithParams_completion
  , stopWithCompletion
  , moveToWithParams_completion
  , moveToWithCompletion
  , calibrateWithParams_completion
  , calibrateWithCompletion
  , readAttributeCountdownTimeWithCompletion
  , subscribeAttributeCountdownTimeWithParams_subscriptionEstablished_reportHandler
  , readAttributeCountdownTimeWithClusterStateCache_endpoint_queue_completion
  , readAttributeMainStateWithCompletion
  , subscribeAttributeMainStateWithParams_subscriptionEstablished_reportHandler
  , readAttributeMainStateWithClusterStateCache_endpoint_queue_completion
  , readAttributeCurrentErrorListWithCompletion
  , subscribeAttributeCurrentErrorListWithParams_subscriptionEstablished_reportHandler
  , readAttributeCurrentErrorListWithClusterStateCache_endpoint_queue_completion
  , readAttributeOverallCurrentStateWithCompletion
  , subscribeAttributeOverallCurrentStateWithParams_subscriptionEstablished_reportHandler
  , readAttributeOverallCurrentStateWithClusterStateCache_endpoint_queue_completion
  , readAttributeOverallTargetStateWithCompletion
  , subscribeAttributeOverallTargetStateWithParams_subscriptionEstablished_reportHandler
  , readAttributeOverallTargetStateWithClusterStateCache_endpoint_queue_completion
  , readAttributeLatchControlModesWithCompletion
  , subscribeAttributeLatchControlModesWithParams_subscriptionEstablished_reportHandler
  , readAttributeLatchControlModesWithClusterStateCache_endpoint_queue_completion
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
  , calibrateWithCompletionSelector
  , calibrateWithParams_completionSelector
  , initSelector
  , initWithDevice_endpointID_queueSelector
  , moveToWithCompletionSelector
  , moveToWithParams_completionSelector
  , newSelector
  , readAttributeAcceptedCommandListWithClusterStateCache_endpoint_queue_completionSelector
  , readAttributeAcceptedCommandListWithCompletionSelector
  , readAttributeAttributeListWithClusterStateCache_endpoint_queue_completionSelector
  , readAttributeAttributeListWithCompletionSelector
  , readAttributeClusterRevisionWithClusterStateCache_endpoint_queue_completionSelector
  , readAttributeClusterRevisionWithCompletionSelector
  , readAttributeCountdownTimeWithClusterStateCache_endpoint_queue_completionSelector
  , readAttributeCountdownTimeWithCompletionSelector
  , readAttributeCurrentErrorListWithClusterStateCache_endpoint_queue_completionSelector
  , readAttributeCurrentErrorListWithCompletionSelector
  , readAttributeFeatureMapWithClusterStateCache_endpoint_queue_completionSelector
  , readAttributeFeatureMapWithCompletionSelector
  , readAttributeGeneratedCommandListWithClusterStateCache_endpoint_queue_completionSelector
  , readAttributeGeneratedCommandListWithCompletionSelector
  , readAttributeLatchControlModesWithClusterStateCache_endpoint_queue_completionSelector
  , readAttributeLatchControlModesWithCompletionSelector
  , readAttributeMainStateWithClusterStateCache_endpoint_queue_completionSelector
  , readAttributeMainStateWithCompletionSelector
  , readAttributeOverallCurrentStateWithClusterStateCache_endpoint_queue_completionSelector
  , readAttributeOverallCurrentStateWithCompletionSelector
  , readAttributeOverallTargetStateWithClusterStateCache_endpoint_queue_completionSelector
  , readAttributeOverallTargetStateWithCompletionSelector
  , stopWithCompletionSelector
  , stopWithParams_completionSelector
  , subscribeAttributeAcceptedCommandListWithParams_subscriptionEstablished_reportHandlerSelector
  , subscribeAttributeAttributeListWithParams_subscriptionEstablished_reportHandlerSelector
  , subscribeAttributeClusterRevisionWithParams_subscriptionEstablished_reportHandlerSelector
  , subscribeAttributeCountdownTimeWithParams_subscriptionEstablished_reportHandlerSelector
  , subscribeAttributeCurrentErrorListWithParams_subscriptionEstablished_reportHandlerSelector
  , subscribeAttributeFeatureMapWithParams_subscriptionEstablished_reportHandlerSelector
  , subscribeAttributeGeneratedCommandListWithParams_subscriptionEstablished_reportHandlerSelector
  , subscribeAttributeLatchControlModesWithParams_subscriptionEstablished_reportHandlerSelector
  , subscribeAttributeMainStateWithParams_subscriptionEstablished_reportHandlerSelector
  , subscribeAttributeOverallCurrentStateWithParams_subscriptionEstablished_reportHandlerSelector
  , subscribeAttributeOverallTargetStateWithParams_subscriptionEstablished_reportHandlerSelector


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
-- On receipt of this command, the closure SHALL stop its movement as fast as the closure is able too.
--
-- ObjC selector: @- stopWithParams:completion:@
stopWithParams_completion :: (IsMTRBaseClusterClosureControl mtrBaseClusterClosureControl, IsMTRClosureControlClusterStopParams params) => mtrBaseClusterClosureControl -> params -> Ptr () -> IO ()
stopWithParams_completion mtrBaseClusterClosureControl params completion =
  sendMessage mtrBaseClusterClosureControl stopWithParams_completionSelector (toMTRClosureControlClusterStopParams params) completion

-- | @- stopWithCompletion:@
stopWithCompletion :: IsMTRBaseClusterClosureControl mtrBaseClusterClosureControl => mtrBaseClusterClosureControl -> Ptr () -> IO ()
stopWithCompletion mtrBaseClusterClosureControl completion =
  sendMessage mtrBaseClusterClosureControl stopWithCompletionSelector completion

-- | Command MoveTo
--
-- On receipt of this command, the closure SHALL operate to update its position, latch state and/or motion speed.
--
-- ObjC selector: @- moveToWithParams:completion:@
moveToWithParams_completion :: (IsMTRBaseClusterClosureControl mtrBaseClusterClosureControl, IsMTRClosureControlClusterMoveToParams params) => mtrBaseClusterClosureControl -> params -> Ptr () -> IO ()
moveToWithParams_completion mtrBaseClusterClosureControl params completion =
  sendMessage mtrBaseClusterClosureControl moveToWithParams_completionSelector (toMTRClosureControlClusterMoveToParams params) completion

-- | @- moveToWithCompletion:@
moveToWithCompletion :: IsMTRBaseClusterClosureControl mtrBaseClusterClosureControl => mtrBaseClusterClosureControl -> Ptr () -> IO ()
moveToWithCompletion mtrBaseClusterClosureControl completion =
  sendMessage mtrBaseClusterClosureControl moveToWithCompletionSelector completion

-- | Command Calibrate
--
-- This command is used to trigger a calibration of the closure.
--
-- ObjC selector: @- calibrateWithParams:completion:@
calibrateWithParams_completion :: (IsMTRBaseClusterClosureControl mtrBaseClusterClosureControl, IsMTRClosureControlClusterCalibrateParams params) => mtrBaseClusterClosureControl -> params -> Ptr () -> IO ()
calibrateWithParams_completion mtrBaseClusterClosureControl params completion =
  sendMessage mtrBaseClusterClosureControl calibrateWithParams_completionSelector (toMTRClosureControlClusterCalibrateParams params) completion

-- | @- calibrateWithCompletion:@
calibrateWithCompletion :: IsMTRBaseClusterClosureControl mtrBaseClusterClosureControl => mtrBaseClusterClosureControl -> Ptr () -> IO ()
calibrateWithCompletion mtrBaseClusterClosureControl completion =
  sendMessage mtrBaseClusterClosureControl calibrateWithCompletionSelector completion

-- | @- readAttributeCountdownTimeWithCompletion:@
readAttributeCountdownTimeWithCompletion :: IsMTRBaseClusterClosureControl mtrBaseClusterClosureControl => mtrBaseClusterClosureControl -> Ptr () -> IO ()
readAttributeCountdownTimeWithCompletion mtrBaseClusterClosureControl completion =
  sendMessage mtrBaseClusterClosureControl readAttributeCountdownTimeWithCompletionSelector completion

-- | @- subscribeAttributeCountdownTimeWithParams:subscriptionEstablished:reportHandler:@
subscribeAttributeCountdownTimeWithParams_subscriptionEstablished_reportHandler :: (IsMTRBaseClusterClosureControl mtrBaseClusterClosureControl, IsMTRSubscribeParams params) => mtrBaseClusterClosureControl -> params -> Ptr () -> Ptr () -> IO ()
subscribeAttributeCountdownTimeWithParams_subscriptionEstablished_reportHandler mtrBaseClusterClosureControl params subscriptionEstablished reportHandler =
  sendMessage mtrBaseClusterClosureControl subscribeAttributeCountdownTimeWithParams_subscriptionEstablished_reportHandlerSelector (toMTRSubscribeParams params) subscriptionEstablished reportHandler

-- | @+ readAttributeCountdownTimeWithClusterStateCache:endpoint:queue:completion:@
readAttributeCountdownTimeWithClusterStateCache_endpoint_queue_completion :: (IsMTRClusterStateCacheContainer clusterStateCacheContainer, IsNSNumber endpoint, IsNSObject queue) => clusterStateCacheContainer -> endpoint -> queue -> Ptr () -> IO ()
readAttributeCountdownTimeWithClusterStateCache_endpoint_queue_completion clusterStateCacheContainer endpoint queue completion =
  do
    cls' <- getRequiredClass "MTRBaseClusterClosureControl"
    sendClassMessage cls' readAttributeCountdownTimeWithClusterStateCache_endpoint_queue_completionSelector (toMTRClusterStateCacheContainer clusterStateCacheContainer) (toNSNumber endpoint) (toNSObject queue) completion

-- | @- readAttributeMainStateWithCompletion:@
readAttributeMainStateWithCompletion :: IsMTRBaseClusterClosureControl mtrBaseClusterClosureControl => mtrBaseClusterClosureControl -> Ptr () -> IO ()
readAttributeMainStateWithCompletion mtrBaseClusterClosureControl completion =
  sendMessage mtrBaseClusterClosureControl readAttributeMainStateWithCompletionSelector completion

-- | @- subscribeAttributeMainStateWithParams:subscriptionEstablished:reportHandler:@
subscribeAttributeMainStateWithParams_subscriptionEstablished_reportHandler :: (IsMTRBaseClusterClosureControl mtrBaseClusterClosureControl, IsMTRSubscribeParams params) => mtrBaseClusterClosureControl -> params -> Ptr () -> Ptr () -> IO ()
subscribeAttributeMainStateWithParams_subscriptionEstablished_reportHandler mtrBaseClusterClosureControl params subscriptionEstablished reportHandler =
  sendMessage mtrBaseClusterClosureControl subscribeAttributeMainStateWithParams_subscriptionEstablished_reportHandlerSelector (toMTRSubscribeParams params) subscriptionEstablished reportHandler

-- | @+ readAttributeMainStateWithClusterStateCache:endpoint:queue:completion:@
readAttributeMainStateWithClusterStateCache_endpoint_queue_completion :: (IsMTRClusterStateCacheContainer clusterStateCacheContainer, IsNSNumber endpoint, IsNSObject queue) => clusterStateCacheContainer -> endpoint -> queue -> Ptr () -> IO ()
readAttributeMainStateWithClusterStateCache_endpoint_queue_completion clusterStateCacheContainer endpoint queue completion =
  do
    cls' <- getRequiredClass "MTRBaseClusterClosureControl"
    sendClassMessage cls' readAttributeMainStateWithClusterStateCache_endpoint_queue_completionSelector (toMTRClusterStateCacheContainer clusterStateCacheContainer) (toNSNumber endpoint) (toNSObject queue) completion

-- | @- readAttributeCurrentErrorListWithCompletion:@
readAttributeCurrentErrorListWithCompletion :: IsMTRBaseClusterClosureControl mtrBaseClusterClosureControl => mtrBaseClusterClosureControl -> Ptr () -> IO ()
readAttributeCurrentErrorListWithCompletion mtrBaseClusterClosureControl completion =
  sendMessage mtrBaseClusterClosureControl readAttributeCurrentErrorListWithCompletionSelector completion

-- | @- subscribeAttributeCurrentErrorListWithParams:subscriptionEstablished:reportHandler:@
subscribeAttributeCurrentErrorListWithParams_subscriptionEstablished_reportHandler :: (IsMTRBaseClusterClosureControl mtrBaseClusterClosureControl, IsMTRSubscribeParams params) => mtrBaseClusterClosureControl -> params -> Ptr () -> Ptr () -> IO ()
subscribeAttributeCurrentErrorListWithParams_subscriptionEstablished_reportHandler mtrBaseClusterClosureControl params subscriptionEstablished reportHandler =
  sendMessage mtrBaseClusterClosureControl subscribeAttributeCurrentErrorListWithParams_subscriptionEstablished_reportHandlerSelector (toMTRSubscribeParams params) subscriptionEstablished reportHandler

-- | @+ readAttributeCurrentErrorListWithClusterStateCache:endpoint:queue:completion:@
readAttributeCurrentErrorListWithClusterStateCache_endpoint_queue_completion :: (IsMTRClusterStateCacheContainer clusterStateCacheContainer, IsNSNumber endpoint, IsNSObject queue) => clusterStateCacheContainer -> endpoint -> queue -> Ptr () -> IO ()
readAttributeCurrentErrorListWithClusterStateCache_endpoint_queue_completion clusterStateCacheContainer endpoint queue completion =
  do
    cls' <- getRequiredClass "MTRBaseClusterClosureControl"
    sendClassMessage cls' readAttributeCurrentErrorListWithClusterStateCache_endpoint_queue_completionSelector (toMTRClusterStateCacheContainer clusterStateCacheContainer) (toNSNumber endpoint) (toNSObject queue) completion

-- | @- readAttributeOverallCurrentStateWithCompletion:@
readAttributeOverallCurrentStateWithCompletion :: IsMTRBaseClusterClosureControl mtrBaseClusterClosureControl => mtrBaseClusterClosureControl -> Ptr () -> IO ()
readAttributeOverallCurrentStateWithCompletion mtrBaseClusterClosureControl completion =
  sendMessage mtrBaseClusterClosureControl readAttributeOverallCurrentStateWithCompletionSelector completion

-- | @- subscribeAttributeOverallCurrentStateWithParams:subscriptionEstablished:reportHandler:@
subscribeAttributeOverallCurrentStateWithParams_subscriptionEstablished_reportHandler :: (IsMTRBaseClusterClosureControl mtrBaseClusterClosureControl, IsMTRSubscribeParams params) => mtrBaseClusterClosureControl -> params -> Ptr () -> Ptr () -> IO ()
subscribeAttributeOverallCurrentStateWithParams_subscriptionEstablished_reportHandler mtrBaseClusterClosureControl params subscriptionEstablished reportHandler =
  sendMessage mtrBaseClusterClosureControl subscribeAttributeOverallCurrentStateWithParams_subscriptionEstablished_reportHandlerSelector (toMTRSubscribeParams params) subscriptionEstablished reportHandler

-- | @+ readAttributeOverallCurrentStateWithClusterStateCache:endpoint:queue:completion:@
readAttributeOverallCurrentStateWithClusterStateCache_endpoint_queue_completion :: (IsMTRClusterStateCacheContainer clusterStateCacheContainer, IsNSNumber endpoint, IsNSObject queue) => clusterStateCacheContainer -> endpoint -> queue -> Ptr () -> IO ()
readAttributeOverallCurrentStateWithClusterStateCache_endpoint_queue_completion clusterStateCacheContainer endpoint queue completion =
  do
    cls' <- getRequiredClass "MTRBaseClusterClosureControl"
    sendClassMessage cls' readAttributeOverallCurrentStateWithClusterStateCache_endpoint_queue_completionSelector (toMTRClusterStateCacheContainer clusterStateCacheContainer) (toNSNumber endpoint) (toNSObject queue) completion

-- | @- readAttributeOverallTargetStateWithCompletion:@
readAttributeOverallTargetStateWithCompletion :: IsMTRBaseClusterClosureControl mtrBaseClusterClosureControl => mtrBaseClusterClosureControl -> Ptr () -> IO ()
readAttributeOverallTargetStateWithCompletion mtrBaseClusterClosureControl completion =
  sendMessage mtrBaseClusterClosureControl readAttributeOverallTargetStateWithCompletionSelector completion

-- | @- subscribeAttributeOverallTargetStateWithParams:subscriptionEstablished:reportHandler:@
subscribeAttributeOverallTargetStateWithParams_subscriptionEstablished_reportHandler :: (IsMTRBaseClusterClosureControl mtrBaseClusterClosureControl, IsMTRSubscribeParams params) => mtrBaseClusterClosureControl -> params -> Ptr () -> Ptr () -> IO ()
subscribeAttributeOverallTargetStateWithParams_subscriptionEstablished_reportHandler mtrBaseClusterClosureControl params subscriptionEstablished reportHandler =
  sendMessage mtrBaseClusterClosureControl subscribeAttributeOverallTargetStateWithParams_subscriptionEstablished_reportHandlerSelector (toMTRSubscribeParams params) subscriptionEstablished reportHandler

-- | @+ readAttributeOverallTargetStateWithClusterStateCache:endpoint:queue:completion:@
readAttributeOverallTargetStateWithClusterStateCache_endpoint_queue_completion :: (IsMTRClusterStateCacheContainer clusterStateCacheContainer, IsNSNumber endpoint, IsNSObject queue) => clusterStateCacheContainer -> endpoint -> queue -> Ptr () -> IO ()
readAttributeOverallTargetStateWithClusterStateCache_endpoint_queue_completion clusterStateCacheContainer endpoint queue completion =
  do
    cls' <- getRequiredClass "MTRBaseClusterClosureControl"
    sendClassMessage cls' readAttributeOverallTargetStateWithClusterStateCache_endpoint_queue_completionSelector (toMTRClusterStateCacheContainer clusterStateCacheContainer) (toNSNumber endpoint) (toNSObject queue) completion

-- | @- readAttributeLatchControlModesWithCompletion:@
readAttributeLatchControlModesWithCompletion :: IsMTRBaseClusterClosureControl mtrBaseClusterClosureControl => mtrBaseClusterClosureControl -> Ptr () -> IO ()
readAttributeLatchControlModesWithCompletion mtrBaseClusterClosureControl completion =
  sendMessage mtrBaseClusterClosureControl readAttributeLatchControlModesWithCompletionSelector completion

-- | @- subscribeAttributeLatchControlModesWithParams:subscriptionEstablished:reportHandler:@
subscribeAttributeLatchControlModesWithParams_subscriptionEstablished_reportHandler :: (IsMTRBaseClusterClosureControl mtrBaseClusterClosureControl, IsMTRSubscribeParams params) => mtrBaseClusterClosureControl -> params -> Ptr () -> Ptr () -> IO ()
subscribeAttributeLatchControlModesWithParams_subscriptionEstablished_reportHandler mtrBaseClusterClosureControl params subscriptionEstablished reportHandler =
  sendMessage mtrBaseClusterClosureControl subscribeAttributeLatchControlModesWithParams_subscriptionEstablished_reportHandlerSelector (toMTRSubscribeParams params) subscriptionEstablished reportHandler

-- | @+ readAttributeLatchControlModesWithClusterStateCache:endpoint:queue:completion:@
readAttributeLatchControlModesWithClusterStateCache_endpoint_queue_completion :: (IsMTRClusterStateCacheContainer clusterStateCacheContainer, IsNSNumber endpoint, IsNSObject queue) => clusterStateCacheContainer -> endpoint -> queue -> Ptr () -> IO ()
readAttributeLatchControlModesWithClusterStateCache_endpoint_queue_completion clusterStateCacheContainer endpoint queue completion =
  do
    cls' <- getRequiredClass "MTRBaseClusterClosureControl"
    sendClassMessage cls' readAttributeLatchControlModesWithClusterStateCache_endpoint_queue_completionSelector (toMTRClusterStateCacheContainer clusterStateCacheContainer) (toNSNumber endpoint) (toNSObject queue) completion

-- | @- readAttributeGeneratedCommandListWithCompletion:@
readAttributeGeneratedCommandListWithCompletion :: IsMTRBaseClusterClosureControl mtrBaseClusterClosureControl => mtrBaseClusterClosureControl -> Ptr () -> IO ()
readAttributeGeneratedCommandListWithCompletion mtrBaseClusterClosureControl completion =
  sendMessage mtrBaseClusterClosureControl readAttributeGeneratedCommandListWithCompletionSelector completion

-- | @- subscribeAttributeGeneratedCommandListWithParams:subscriptionEstablished:reportHandler:@
subscribeAttributeGeneratedCommandListWithParams_subscriptionEstablished_reportHandler :: (IsMTRBaseClusterClosureControl mtrBaseClusterClosureControl, IsMTRSubscribeParams params) => mtrBaseClusterClosureControl -> params -> Ptr () -> Ptr () -> IO ()
subscribeAttributeGeneratedCommandListWithParams_subscriptionEstablished_reportHandler mtrBaseClusterClosureControl params subscriptionEstablished reportHandler =
  sendMessage mtrBaseClusterClosureControl subscribeAttributeGeneratedCommandListWithParams_subscriptionEstablished_reportHandlerSelector (toMTRSubscribeParams params) subscriptionEstablished reportHandler

-- | @+ readAttributeGeneratedCommandListWithClusterStateCache:endpoint:queue:completion:@
readAttributeGeneratedCommandListWithClusterStateCache_endpoint_queue_completion :: (IsMTRClusterStateCacheContainer clusterStateCacheContainer, IsNSNumber endpoint, IsNSObject queue) => clusterStateCacheContainer -> endpoint -> queue -> Ptr () -> IO ()
readAttributeGeneratedCommandListWithClusterStateCache_endpoint_queue_completion clusterStateCacheContainer endpoint queue completion =
  do
    cls' <- getRequiredClass "MTRBaseClusterClosureControl"
    sendClassMessage cls' readAttributeGeneratedCommandListWithClusterStateCache_endpoint_queue_completionSelector (toMTRClusterStateCacheContainer clusterStateCacheContainer) (toNSNumber endpoint) (toNSObject queue) completion

-- | @- readAttributeAcceptedCommandListWithCompletion:@
readAttributeAcceptedCommandListWithCompletion :: IsMTRBaseClusterClosureControl mtrBaseClusterClosureControl => mtrBaseClusterClosureControl -> Ptr () -> IO ()
readAttributeAcceptedCommandListWithCompletion mtrBaseClusterClosureControl completion =
  sendMessage mtrBaseClusterClosureControl readAttributeAcceptedCommandListWithCompletionSelector completion

-- | @- subscribeAttributeAcceptedCommandListWithParams:subscriptionEstablished:reportHandler:@
subscribeAttributeAcceptedCommandListWithParams_subscriptionEstablished_reportHandler :: (IsMTRBaseClusterClosureControl mtrBaseClusterClosureControl, IsMTRSubscribeParams params) => mtrBaseClusterClosureControl -> params -> Ptr () -> Ptr () -> IO ()
subscribeAttributeAcceptedCommandListWithParams_subscriptionEstablished_reportHandler mtrBaseClusterClosureControl params subscriptionEstablished reportHandler =
  sendMessage mtrBaseClusterClosureControl subscribeAttributeAcceptedCommandListWithParams_subscriptionEstablished_reportHandlerSelector (toMTRSubscribeParams params) subscriptionEstablished reportHandler

-- | @+ readAttributeAcceptedCommandListWithClusterStateCache:endpoint:queue:completion:@
readAttributeAcceptedCommandListWithClusterStateCache_endpoint_queue_completion :: (IsMTRClusterStateCacheContainer clusterStateCacheContainer, IsNSNumber endpoint, IsNSObject queue) => clusterStateCacheContainer -> endpoint -> queue -> Ptr () -> IO ()
readAttributeAcceptedCommandListWithClusterStateCache_endpoint_queue_completion clusterStateCacheContainer endpoint queue completion =
  do
    cls' <- getRequiredClass "MTRBaseClusterClosureControl"
    sendClassMessage cls' readAttributeAcceptedCommandListWithClusterStateCache_endpoint_queue_completionSelector (toMTRClusterStateCacheContainer clusterStateCacheContainer) (toNSNumber endpoint) (toNSObject queue) completion

-- | @- readAttributeAttributeListWithCompletion:@
readAttributeAttributeListWithCompletion :: IsMTRBaseClusterClosureControl mtrBaseClusterClosureControl => mtrBaseClusterClosureControl -> Ptr () -> IO ()
readAttributeAttributeListWithCompletion mtrBaseClusterClosureControl completion =
  sendMessage mtrBaseClusterClosureControl readAttributeAttributeListWithCompletionSelector completion

-- | @- subscribeAttributeAttributeListWithParams:subscriptionEstablished:reportHandler:@
subscribeAttributeAttributeListWithParams_subscriptionEstablished_reportHandler :: (IsMTRBaseClusterClosureControl mtrBaseClusterClosureControl, IsMTRSubscribeParams params) => mtrBaseClusterClosureControl -> params -> Ptr () -> Ptr () -> IO ()
subscribeAttributeAttributeListWithParams_subscriptionEstablished_reportHandler mtrBaseClusterClosureControl params subscriptionEstablished reportHandler =
  sendMessage mtrBaseClusterClosureControl subscribeAttributeAttributeListWithParams_subscriptionEstablished_reportHandlerSelector (toMTRSubscribeParams params) subscriptionEstablished reportHandler

-- | @+ readAttributeAttributeListWithClusterStateCache:endpoint:queue:completion:@
readAttributeAttributeListWithClusterStateCache_endpoint_queue_completion :: (IsMTRClusterStateCacheContainer clusterStateCacheContainer, IsNSNumber endpoint, IsNSObject queue) => clusterStateCacheContainer -> endpoint -> queue -> Ptr () -> IO ()
readAttributeAttributeListWithClusterStateCache_endpoint_queue_completion clusterStateCacheContainer endpoint queue completion =
  do
    cls' <- getRequiredClass "MTRBaseClusterClosureControl"
    sendClassMessage cls' readAttributeAttributeListWithClusterStateCache_endpoint_queue_completionSelector (toMTRClusterStateCacheContainer clusterStateCacheContainer) (toNSNumber endpoint) (toNSObject queue) completion

-- | @- readAttributeFeatureMapWithCompletion:@
readAttributeFeatureMapWithCompletion :: IsMTRBaseClusterClosureControl mtrBaseClusterClosureControl => mtrBaseClusterClosureControl -> Ptr () -> IO ()
readAttributeFeatureMapWithCompletion mtrBaseClusterClosureControl completion =
  sendMessage mtrBaseClusterClosureControl readAttributeFeatureMapWithCompletionSelector completion

-- | @- subscribeAttributeFeatureMapWithParams:subscriptionEstablished:reportHandler:@
subscribeAttributeFeatureMapWithParams_subscriptionEstablished_reportHandler :: (IsMTRBaseClusterClosureControl mtrBaseClusterClosureControl, IsMTRSubscribeParams params) => mtrBaseClusterClosureControl -> params -> Ptr () -> Ptr () -> IO ()
subscribeAttributeFeatureMapWithParams_subscriptionEstablished_reportHandler mtrBaseClusterClosureControl params subscriptionEstablished reportHandler =
  sendMessage mtrBaseClusterClosureControl subscribeAttributeFeatureMapWithParams_subscriptionEstablished_reportHandlerSelector (toMTRSubscribeParams params) subscriptionEstablished reportHandler

-- | @+ readAttributeFeatureMapWithClusterStateCache:endpoint:queue:completion:@
readAttributeFeatureMapWithClusterStateCache_endpoint_queue_completion :: (IsMTRClusterStateCacheContainer clusterStateCacheContainer, IsNSNumber endpoint, IsNSObject queue) => clusterStateCacheContainer -> endpoint -> queue -> Ptr () -> IO ()
readAttributeFeatureMapWithClusterStateCache_endpoint_queue_completion clusterStateCacheContainer endpoint queue completion =
  do
    cls' <- getRequiredClass "MTRBaseClusterClosureControl"
    sendClassMessage cls' readAttributeFeatureMapWithClusterStateCache_endpoint_queue_completionSelector (toMTRClusterStateCacheContainer clusterStateCacheContainer) (toNSNumber endpoint) (toNSObject queue) completion

-- | @- readAttributeClusterRevisionWithCompletion:@
readAttributeClusterRevisionWithCompletion :: IsMTRBaseClusterClosureControl mtrBaseClusterClosureControl => mtrBaseClusterClosureControl -> Ptr () -> IO ()
readAttributeClusterRevisionWithCompletion mtrBaseClusterClosureControl completion =
  sendMessage mtrBaseClusterClosureControl readAttributeClusterRevisionWithCompletionSelector completion

-- | @- subscribeAttributeClusterRevisionWithParams:subscriptionEstablished:reportHandler:@
subscribeAttributeClusterRevisionWithParams_subscriptionEstablished_reportHandler :: (IsMTRBaseClusterClosureControl mtrBaseClusterClosureControl, IsMTRSubscribeParams params) => mtrBaseClusterClosureControl -> params -> Ptr () -> Ptr () -> IO ()
subscribeAttributeClusterRevisionWithParams_subscriptionEstablished_reportHandler mtrBaseClusterClosureControl params subscriptionEstablished reportHandler =
  sendMessage mtrBaseClusterClosureControl subscribeAttributeClusterRevisionWithParams_subscriptionEstablished_reportHandlerSelector (toMTRSubscribeParams params) subscriptionEstablished reportHandler

-- | @+ readAttributeClusterRevisionWithClusterStateCache:endpoint:queue:completion:@
readAttributeClusterRevisionWithClusterStateCache_endpoint_queue_completion :: (IsMTRClusterStateCacheContainer clusterStateCacheContainer, IsNSNumber endpoint, IsNSObject queue) => clusterStateCacheContainer -> endpoint -> queue -> Ptr () -> IO ()
readAttributeClusterRevisionWithClusterStateCache_endpoint_queue_completion clusterStateCacheContainer endpoint queue completion =
  do
    cls' <- getRequiredClass "MTRBaseClusterClosureControl"
    sendClassMessage cls' readAttributeClusterRevisionWithClusterStateCache_endpoint_queue_completionSelector (toMTRClusterStateCacheContainer clusterStateCacheContainer) (toNSNumber endpoint) (toNSObject queue) completion

-- | @- init@
init_ :: IsMTRBaseClusterClosureControl mtrBaseClusterClosureControl => mtrBaseClusterClosureControl -> IO (Id MTRBaseClusterClosureControl)
init_ mtrBaseClusterClosureControl =
  sendOwnedMessage mtrBaseClusterClosureControl initSelector

-- | @+ new@
new :: IO (Id MTRBaseClusterClosureControl)
new  =
  do
    cls' <- getRequiredClass "MTRBaseClusterClosureControl"
    sendOwnedClassMessage cls' newSelector

-- | For all instance methods (reads, writes, commands) that take a completion, the completion will be called on the provided queue.
--
-- ObjC selector: @- initWithDevice:endpointID:queue:@
initWithDevice_endpointID_queue :: (IsMTRBaseClusterClosureControl mtrBaseClusterClosureControl, IsMTRBaseDevice device, IsNSNumber endpointID, IsNSObject queue) => mtrBaseClusterClosureControl -> device -> endpointID -> queue -> IO (Id MTRBaseClusterClosureControl)
initWithDevice_endpointID_queue mtrBaseClusterClosureControl device endpointID queue =
  sendOwnedMessage mtrBaseClusterClosureControl initWithDevice_endpointID_queueSelector (toMTRBaseDevice device) (toNSNumber endpointID) (toNSObject queue)

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @stopWithParams:completion:@
stopWithParams_completionSelector :: Selector '[Id MTRClosureControlClusterStopParams, Ptr ()] ()
stopWithParams_completionSelector = mkSelector "stopWithParams:completion:"

-- | @Selector@ for @stopWithCompletion:@
stopWithCompletionSelector :: Selector '[Ptr ()] ()
stopWithCompletionSelector = mkSelector "stopWithCompletion:"

-- | @Selector@ for @moveToWithParams:completion:@
moveToWithParams_completionSelector :: Selector '[Id MTRClosureControlClusterMoveToParams, Ptr ()] ()
moveToWithParams_completionSelector = mkSelector "moveToWithParams:completion:"

-- | @Selector@ for @moveToWithCompletion:@
moveToWithCompletionSelector :: Selector '[Ptr ()] ()
moveToWithCompletionSelector = mkSelector "moveToWithCompletion:"

-- | @Selector@ for @calibrateWithParams:completion:@
calibrateWithParams_completionSelector :: Selector '[Id MTRClosureControlClusterCalibrateParams, Ptr ()] ()
calibrateWithParams_completionSelector = mkSelector "calibrateWithParams:completion:"

-- | @Selector@ for @calibrateWithCompletion:@
calibrateWithCompletionSelector :: Selector '[Ptr ()] ()
calibrateWithCompletionSelector = mkSelector "calibrateWithCompletion:"

-- | @Selector@ for @readAttributeCountdownTimeWithCompletion:@
readAttributeCountdownTimeWithCompletionSelector :: Selector '[Ptr ()] ()
readAttributeCountdownTimeWithCompletionSelector = mkSelector "readAttributeCountdownTimeWithCompletion:"

-- | @Selector@ for @subscribeAttributeCountdownTimeWithParams:subscriptionEstablished:reportHandler:@
subscribeAttributeCountdownTimeWithParams_subscriptionEstablished_reportHandlerSelector :: Selector '[Id MTRSubscribeParams, Ptr (), Ptr ()] ()
subscribeAttributeCountdownTimeWithParams_subscriptionEstablished_reportHandlerSelector = mkSelector "subscribeAttributeCountdownTimeWithParams:subscriptionEstablished:reportHandler:"

-- | @Selector@ for @readAttributeCountdownTimeWithClusterStateCache:endpoint:queue:completion:@
readAttributeCountdownTimeWithClusterStateCache_endpoint_queue_completionSelector :: Selector '[Id MTRClusterStateCacheContainer, Id NSNumber, Id NSObject, Ptr ()] ()
readAttributeCountdownTimeWithClusterStateCache_endpoint_queue_completionSelector = mkSelector "readAttributeCountdownTimeWithClusterStateCache:endpoint:queue:completion:"

-- | @Selector@ for @readAttributeMainStateWithCompletion:@
readAttributeMainStateWithCompletionSelector :: Selector '[Ptr ()] ()
readAttributeMainStateWithCompletionSelector = mkSelector "readAttributeMainStateWithCompletion:"

-- | @Selector@ for @subscribeAttributeMainStateWithParams:subscriptionEstablished:reportHandler:@
subscribeAttributeMainStateWithParams_subscriptionEstablished_reportHandlerSelector :: Selector '[Id MTRSubscribeParams, Ptr (), Ptr ()] ()
subscribeAttributeMainStateWithParams_subscriptionEstablished_reportHandlerSelector = mkSelector "subscribeAttributeMainStateWithParams:subscriptionEstablished:reportHandler:"

-- | @Selector@ for @readAttributeMainStateWithClusterStateCache:endpoint:queue:completion:@
readAttributeMainStateWithClusterStateCache_endpoint_queue_completionSelector :: Selector '[Id MTRClusterStateCacheContainer, Id NSNumber, Id NSObject, Ptr ()] ()
readAttributeMainStateWithClusterStateCache_endpoint_queue_completionSelector = mkSelector "readAttributeMainStateWithClusterStateCache:endpoint:queue:completion:"

-- | @Selector@ for @readAttributeCurrentErrorListWithCompletion:@
readAttributeCurrentErrorListWithCompletionSelector :: Selector '[Ptr ()] ()
readAttributeCurrentErrorListWithCompletionSelector = mkSelector "readAttributeCurrentErrorListWithCompletion:"

-- | @Selector@ for @subscribeAttributeCurrentErrorListWithParams:subscriptionEstablished:reportHandler:@
subscribeAttributeCurrentErrorListWithParams_subscriptionEstablished_reportHandlerSelector :: Selector '[Id MTRSubscribeParams, Ptr (), Ptr ()] ()
subscribeAttributeCurrentErrorListWithParams_subscriptionEstablished_reportHandlerSelector = mkSelector "subscribeAttributeCurrentErrorListWithParams:subscriptionEstablished:reportHandler:"

-- | @Selector@ for @readAttributeCurrentErrorListWithClusterStateCache:endpoint:queue:completion:@
readAttributeCurrentErrorListWithClusterStateCache_endpoint_queue_completionSelector :: Selector '[Id MTRClusterStateCacheContainer, Id NSNumber, Id NSObject, Ptr ()] ()
readAttributeCurrentErrorListWithClusterStateCache_endpoint_queue_completionSelector = mkSelector "readAttributeCurrentErrorListWithClusterStateCache:endpoint:queue:completion:"

-- | @Selector@ for @readAttributeOverallCurrentStateWithCompletion:@
readAttributeOverallCurrentStateWithCompletionSelector :: Selector '[Ptr ()] ()
readAttributeOverallCurrentStateWithCompletionSelector = mkSelector "readAttributeOverallCurrentStateWithCompletion:"

-- | @Selector@ for @subscribeAttributeOverallCurrentStateWithParams:subscriptionEstablished:reportHandler:@
subscribeAttributeOverallCurrentStateWithParams_subscriptionEstablished_reportHandlerSelector :: Selector '[Id MTRSubscribeParams, Ptr (), Ptr ()] ()
subscribeAttributeOverallCurrentStateWithParams_subscriptionEstablished_reportHandlerSelector = mkSelector "subscribeAttributeOverallCurrentStateWithParams:subscriptionEstablished:reportHandler:"

-- | @Selector@ for @readAttributeOverallCurrentStateWithClusterStateCache:endpoint:queue:completion:@
readAttributeOverallCurrentStateWithClusterStateCache_endpoint_queue_completionSelector :: Selector '[Id MTRClusterStateCacheContainer, Id NSNumber, Id NSObject, Ptr ()] ()
readAttributeOverallCurrentStateWithClusterStateCache_endpoint_queue_completionSelector = mkSelector "readAttributeOverallCurrentStateWithClusterStateCache:endpoint:queue:completion:"

-- | @Selector@ for @readAttributeOverallTargetStateWithCompletion:@
readAttributeOverallTargetStateWithCompletionSelector :: Selector '[Ptr ()] ()
readAttributeOverallTargetStateWithCompletionSelector = mkSelector "readAttributeOverallTargetStateWithCompletion:"

-- | @Selector@ for @subscribeAttributeOverallTargetStateWithParams:subscriptionEstablished:reportHandler:@
subscribeAttributeOverallTargetStateWithParams_subscriptionEstablished_reportHandlerSelector :: Selector '[Id MTRSubscribeParams, Ptr (), Ptr ()] ()
subscribeAttributeOverallTargetStateWithParams_subscriptionEstablished_reportHandlerSelector = mkSelector "subscribeAttributeOverallTargetStateWithParams:subscriptionEstablished:reportHandler:"

-- | @Selector@ for @readAttributeOverallTargetStateWithClusterStateCache:endpoint:queue:completion:@
readAttributeOverallTargetStateWithClusterStateCache_endpoint_queue_completionSelector :: Selector '[Id MTRClusterStateCacheContainer, Id NSNumber, Id NSObject, Ptr ()] ()
readAttributeOverallTargetStateWithClusterStateCache_endpoint_queue_completionSelector = mkSelector "readAttributeOverallTargetStateWithClusterStateCache:endpoint:queue:completion:"

-- | @Selector@ for @readAttributeLatchControlModesWithCompletion:@
readAttributeLatchControlModesWithCompletionSelector :: Selector '[Ptr ()] ()
readAttributeLatchControlModesWithCompletionSelector = mkSelector "readAttributeLatchControlModesWithCompletion:"

-- | @Selector@ for @subscribeAttributeLatchControlModesWithParams:subscriptionEstablished:reportHandler:@
subscribeAttributeLatchControlModesWithParams_subscriptionEstablished_reportHandlerSelector :: Selector '[Id MTRSubscribeParams, Ptr (), Ptr ()] ()
subscribeAttributeLatchControlModesWithParams_subscriptionEstablished_reportHandlerSelector = mkSelector "subscribeAttributeLatchControlModesWithParams:subscriptionEstablished:reportHandler:"

-- | @Selector@ for @readAttributeLatchControlModesWithClusterStateCache:endpoint:queue:completion:@
readAttributeLatchControlModesWithClusterStateCache_endpoint_queue_completionSelector :: Selector '[Id MTRClusterStateCacheContainer, Id NSNumber, Id NSObject, Ptr ()] ()
readAttributeLatchControlModesWithClusterStateCache_endpoint_queue_completionSelector = mkSelector "readAttributeLatchControlModesWithClusterStateCache:endpoint:queue:completion:"

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
initSelector :: Selector '[] (Id MTRBaseClusterClosureControl)
initSelector = mkSelector "init"

-- | @Selector@ for @new@
newSelector :: Selector '[] (Id MTRBaseClusterClosureControl)
newSelector = mkSelector "new"

-- | @Selector@ for @initWithDevice:endpointID:queue:@
initWithDevice_endpointID_queueSelector :: Selector '[Id MTRBaseDevice, Id NSNumber, Id NSObject] (Id MTRBaseClusterClosureControl)
initWithDevice_endpointID_queueSelector = mkSelector "initWithDevice:endpointID:queue:"

