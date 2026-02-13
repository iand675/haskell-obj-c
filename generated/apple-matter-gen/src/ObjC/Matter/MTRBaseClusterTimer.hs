{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Cluster Timer
--
-- This cluster supports creating a simple timer functionality.
--
-- Generated bindings for @MTRBaseClusterTimer@.
module ObjC.Matter.MTRBaseClusterTimer
  ( MTRBaseClusterTimer
  , IsMTRBaseClusterTimer(..)
  , setTimerWithParams_completion
  , resetTimerWithParams_completion
  , resetTimerWithCompletion
  , addTimeWithParams_completion
  , reduceTimeWithParams_completion
  , readAttributeSetTimeWithCompletion
  , subscribeAttributeSetTimeWithParams_subscriptionEstablished_reportHandler
  , readAttributeSetTimeWithClusterStateCache_endpoint_queue_completion
  , readAttributeTimeRemainingWithCompletion
  , subscribeAttributeTimeRemainingWithParams_subscriptionEstablished_reportHandler
  , readAttributeTimeRemainingWithClusterStateCache_endpoint_queue_completion
  , readAttributeTimerStateWithCompletion
  , subscribeAttributeTimerStateWithParams_subscriptionEstablished_reportHandler
  , readAttributeTimerStateWithClusterStateCache_endpoint_queue_completion
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
  , addTimeWithParams_completionSelector
  , initSelector
  , initWithDevice_endpointID_queueSelector
  , newSelector
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
  , readAttributeSetTimeWithClusterStateCache_endpoint_queue_completionSelector
  , readAttributeSetTimeWithCompletionSelector
  , readAttributeTimeRemainingWithClusterStateCache_endpoint_queue_completionSelector
  , readAttributeTimeRemainingWithCompletionSelector
  , readAttributeTimerStateWithClusterStateCache_endpoint_queue_completionSelector
  , readAttributeTimerStateWithCompletionSelector
  , reduceTimeWithParams_completionSelector
  , resetTimerWithCompletionSelector
  , resetTimerWithParams_completionSelector
  , setTimerWithParams_completionSelector
  , subscribeAttributeAcceptedCommandListWithParams_subscriptionEstablished_reportHandlerSelector
  , subscribeAttributeAttributeListWithParams_subscriptionEstablished_reportHandlerSelector
  , subscribeAttributeClusterRevisionWithParams_subscriptionEstablished_reportHandlerSelector
  , subscribeAttributeFeatureMapWithParams_subscriptionEstablished_reportHandlerSelector
  , subscribeAttributeGeneratedCommandListWithParams_subscriptionEstablished_reportHandlerSelector
  , subscribeAttributeSetTimeWithParams_subscriptionEstablished_reportHandlerSelector
  , subscribeAttributeTimeRemainingWithParams_subscriptionEstablished_reportHandlerSelector
  , subscribeAttributeTimerStateWithParams_subscriptionEstablished_reportHandlerSelector


  ) where

import Foreign.Ptr (Ptr, FunPtr)
import Foreign.C.Types

import ObjC.Runtime.Types
import ObjC.Runtime.Message (sendMessage, sendOwnedMessage, sendClassMessage, sendOwnedClassMessage)
import ObjC.Runtime.Selector (mkSelector)
import ObjC.Runtime.Class (getRequiredClass)

import ObjC.Matter.Internal.Classes
import ObjC.Foundation.Internal.Classes

-- | Command SetTimer
--
-- This command is used to set the timer.
--
-- ObjC selector: @- setTimerWithParams:completion:@
setTimerWithParams_completion :: (IsMTRBaseClusterTimer mtrBaseClusterTimer, IsMTRTimerClusterSetTimerParams params) => mtrBaseClusterTimer -> params -> Ptr () -> IO ()
setTimerWithParams_completion mtrBaseClusterTimer params completion =
  sendMessage mtrBaseClusterTimer setTimerWithParams_completionSelector (toMTRTimerClusterSetTimerParams params) completion

-- | Command ResetTimer
--
-- This command is used to reset the timer to the original value.
--
-- ObjC selector: @- resetTimerWithParams:completion:@
resetTimerWithParams_completion :: (IsMTRBaseClusterTimer mtrBaseClusterTimer, IsMTRTimerClusterResetTimerParams params) => mtrBaseClusterTimer -> params -> Ptr () -> IO ()
resetTimerWithParams_completion mtrBaseClusterTimer params completion =
  sendMessage mtrBaseClusterTimer resetTimerWithParams_completionSelector (toMTRTimerClusterResetTimerParams params) completion

-- | @- resetTimerWithCompletion:@
resetTimerWithCompletion :: IsMTRBaseClusterTimer mtrBaseClusterTimer => mtrBaseClusterTimer -> Ptr () -> IO ()
resetTimerWithCompletion mtrBaseClusterTimer completion =
  sendMessage mtrBaseClusterTimer resetTimerWithCompletionSelector completion

-- | Command AddTime
--
-- This command is used to add time to the existing timer.
--
-- ObjC selector: @- addTimeWithParams:completion:@
addTimeWithParams_completion :: (IsMTRBaseClusterTimer mtrBaseClusterTimer, IsMTRTimerClusterAddTimeParams params) => mtrBaseClusterTimer -> params -> Ptr () -> IO ()
addTimeWithParams_completion mtrBaseClusterTimer params completion =
  sendMessage mtrBaseClusterTimer addTimeWithParams_completionSelector (toMTRTimerClusterAddTimeParams params) completion

-- | Command ReduceTime
--
-- This command is used to reduce time on the existing timer.
--
-- ObjC selector: @- reduceTimeWithParams:completion:@
reduceTimeWithParams_completion :: (IsMTRBaseClusterTimer mtrBaseClusterTimer, IsMTRTimerClusterReduceTimeParams params) => mtrBaseClusterTimer -> params -> Ptr () -> IO ()
reduceTimeWithParams_completion mtrBaseClusterTimer params completion =
  sendMessage mtrBaseClusterTimer reduceTimeWithParams_completionSelector (toMTRTimerClusterReduceTimeParams params) completion

-- | @- readAttributeSetTimeWithCompletion:@
readAttributeSetTimeWithCompletion :: IsMTRBaseClusterTimer mtrBaseClusterTimer => mtrBaseClusterTimer -> Ptr () -> IO ()
readAttributeSetTimeWithCompletion mtrBaseClusterTimer completion =
  sendMessage mtrBaseClusterTimer readAttributeSetTimeWithCompletionSelector completion

-- | @- subscribeAttributeSetTimeWithParams:subscriptionEstablished:reportHandler:@
subscribeAttributeSetTimeWithParams_subscriptionEstablished_reportHandler :: (IsMTRBaseClusterTimer mtrBaseClusterTimer, IsMTRSubscribeParams params) => mtrBaseClusterTimer -> params -> Ptr () -> Ptr () -> IO ()
subscribeAttributeSetTimeWithParams_subscriptionEstablished_reportHandler mtrBaseClusterTimer params subscriptionEstablished reportHandler =
  sendMessage mtrBaseClusterTimer subscribeAttributeSetTimeWithParams_subscriptionEstablished_reportHandlerSelector (toMTRSubscribeParams params) subscriptionEstablished reportHandler

-- | @+ readAttributeSetTimeWithClusterStateCache:endpoint:queue:completion:@
readAttributeSetTimeWithClusterStateCache_endpoint_queue_completion :: (IsMTRClusterStateCacheContainer clusterStateCacheContainer, IsNSNumber endpoint, IsNSObject queue) => clusterStateCacheContainer -> endpoint -> queue -> Ptr () -> IO ()
readAttributeSetTimeWithClusterStateCache_endpoint_queue_completion clusterStateCacheContainer endpoint queue completion =
  do
    cls' <- getRequiredClass "MTRBaseClusterTimer"
    sendClassMessage cls' readAttributeSetTimeWithClusterStateCache_endpoint_queue_completionSelector (toMTRClusterStateCacheContainer clusterStateCacheContainer) (toNSNumber endpoint) (toNSObject queue) completion

-- | @- readAttributeTimeRemainingWithCompletion:@
readAttributeTimeRemainingWithCompletion :: IsMTRBaseClusterTimer mtrBaseClusterTimer => mtrBaseClusterTimer -> Ptr () -> IO ()
readAttributeTimeRemainingWithCompletion mtrBaseClusterTimer completion =
  sendMessage mtrBaseClusterTimer readAttributeTimeRemainingWithCompletionSelector completion

-- | @- subscribeAttributeTimeRemainingWithParams:subscriptionEstablished:reportHandler:@
subscribeAttributeTimeRemainingWithParams_subscriptionEstablished_reportHandler :: (IsMTRBaseClusterTimer mtrBaseClusterTimer, IsMTRSubscribeParams params) => mtrBaseClusterTimer -> params -> Ptr () -> Ptr () -> IO ()
subscribeAttributeTimeRemainingWithParams_subscriptionEstablished_reportHandler mtrBaseClusterTimer params subscriptionEstablished reportHandler =
  sendMessage mtrBaseClusterTimer subscribeAttributeTimeRemainingWithParams_subscriptionEstablished_reportHandlerSelector (toMTRSubscribeParams params) subscriptionEstablished reportHandler

-- | @+ readAttributeTimeRemainingWithClusterStateCache:endpoint:queue:completion:@
readAttributeTimeRemainingWithClusterStateCache_endpoint_queue_completion :: (IsMTRClusterStateCacheContainer clusterStateCacheContainer, IsNSNumber endpoint, IsNSObject queue) => clusterStateCacheContainer -> endpoint -> queue -> Ptr () -> IO ()
readAttributeTimeRemainingWithClusterStateCache_endpoint_queue_completion clusterStateCacheContainer endpoint queue completion =
  do
    cls' <- getRequiredClass "MTRBaseClusterTimer"
    sendClassMessage cls' readAttributeTimeRemainingWithClusterStateCache_endpoint_queue_completionSelector (toMTRClusterStateCacheContainer clusterStateCacheContainer) (toNSNumber endpoint) (toNSObject queue) completion

-- | @- readAttributeTimerStateWithCompletion:@
readAttributeTimerStateWithCompletion :: IsMTRBaseClusterTimer mtrBaseClusterTimer => mtrBaseClusterTimer -> Ptr () -> IO ()
readAttributeTimerStateWithCompletion mtrBaseClusterTimer completion =
  sendMessage mtrBaseClusterTimer readAttributeTimerStateWithCompletionSelector completion

-- | @- subscribeAttributeTimerStateWithParams:subscriptionEstablished:reportHandler:@
subscribeAttributeTimerStateWithParams_subscriptionEstablished_reportHandler :: (IsMTRBaseClusterTimer mtrBaseClusterTimer, IsMTRSubscribeParams params) => mtrBaseClusterTimer -> params -> Ptr () -> Ptr () -> IO ()
subscribeAttributeTimerStateWithParams_subscriptionEstablished_reportHandler mtrBaseClusterTimer params subscriptionEstablished reportHandler =
  sendMessage mtrBaseClusterTimer subscribeAttributeTimerStateWithParams_subscriptionEstablished_reportHandlerSelector (toMTRSubscribeParams params) subscriptionEstablished reportHandler

-- | @+ readAttributeTimerStateWithClusterStateCache:endpoint:queue:completion:@
readAttributeTimerStateWithClusterStateCache_endpoint_queue_completion :: (IsMTRClusterStateCacheContainer clusterStateCacheContainer, IsNSNumber endpoint, IsNSObject queue) => clusterStateCacheContainer -> endpoint -> queue -> Ptr () -> IO ()
readAttributeTimerStateWithClusterStateCache_endpoint_queue_completion clusterStateCacheContainer endpoint queue completion =
  do
    cls' <- getRequiredClass "MTRBaseClusterTimer"
    sendClassMessage cls' readAttributeTimerStateWithClusterStateCache_endpoint_queue_completionSelector (toMTRClusterStateCacheContainer clusterStateCacheContainer) (toNSNumber endpoint) (toNSObject queue) completion

-- | @- readAttributeGeneratedCommandListWithCompletion:@
readAttributeGeneratedCommandListWithCompletion :: IsMTRBaseClusterTimer mtrBaseClusterTimer => mtrBaseClusterTimer -> Ptr () -> IO ()
readAttributeGeneratedCommandListWithCompletion mtrBaseClusterTimer completion =
  sendMessage mtrBaseClusterTimer readAttributeGeneratedCommandListWithCompletionSelector completion

-- | @- subscribeAttributeGeneratedCommandListWithParams:subscriptionEstablished:reportHandler:@
subscribeAttributeGeneratedCommandListWithParams_subscriptionEstablished_reportHandler :: (IsMTRBaseClusterTimer mtrBaseClusterTimer, IsMTRSubscribeParams params) => mtrBaseClusterTimer -> params -> Ptr () -> Ptr () -> IO ()
subscribeAttributeGeneratedCommandListWithParams_subscriptionEstablished_reportHandler mtrBaseClusterTimer params subscriptionEstablished reportHandler =
  sendMessage mtrBaseClusterTimer subscribeAttributeGeneratedCommandListWithParams_subscriptionEstablished_reportHandlerSelector (toMTRSubscribeParams params) subscriptionEstablished reportHandler

-- | @+ readAttributeGeneratedCommandListWithClusterStateCache:endpoint:queue:completion:@
readAttributeGeneratedCommandListWithClusterStateCache_endpoint_queue_completion :: (IsMTRClusterStateCacheContainer clusterStateCacheContainer, IsNSNumber endpoint, IsNSObject queue) => clusterStateCacheContainer -> endpoint -> queue -> Ptr () -> IO ()
readAttributeGeneratedCommandListWithClusterStateCache_endpoint_queue_completion clusterStateCacheContainer endpoint queue completion =
  do
    cls' <- getRequiredClass "MTRBaseClusterTimer"
    sendClassMessage cls' readAttributeGeneratedCommandListWithClusterStateCache_endpoint_queue_completionSelector (toMTRClusterStateCacheContainer clusterStateCacheContainer) (toNSNumber endpoint) (toNSObject queue) completion

-- | @- readAttributeAcceptedCommandListWithCompletion:@
readAttributeAcceptedCommandListWithCompletion :: IsMTRBaseClusterTimer mtrBaseClusterTimer => mtrBaseClusterTimer -> Ptr () -> IO ()
readAttributeAcceptedCommandListWithCompletion mtrBaseClusterTimer completion =
  sendMessage mtrBaseClusterTimer readAttributeAcceptedCommandListWithCompletionSelector completion

-- | @- subscribeAttributeAcceptedCommandListWithParams:subscriptionEstablished:reportHandler:@
subscribeAttributeAcceptedCommandListWithParams_subscriptionEstablished_reportHandler :: (IsMTRBaseClusterTimer mtrBaseClusterTimer, IsMTRSubscribeParams params) => mtrBaseClusterTimer -> params -> Ptr () -> Ptr () -> IO ()
subscribeAttributeAcceptedCommandListWithParams_subscriptionEstablished_reportHandler mtrBaseClusterTimer params subscriptionEstablished reportHandler =
  sendMessage mtrBaseClusterTimer subscribeAttributeAcceptedCommandListWithParams_subscriptionEstablished_reportHandlerSelector (toMTRSubscribeParams params) subscriptionEstablished reportHandler

-- | @+ readAttributeAcceptedCommandListWithClusterStateCache:endpoint:queue:completion:@
readAttributeAcceptedCommandListWithClusterStateCache_endpoint_queue_completion :: (IsMTRClusterStateCacheContainer clusterStateCacheContainer, IsNSNumber endpoint, IsNSObject queue) => clusterStateCacheContainer -> endpoint -> queue -> Ptr () -> IO ()
readAttributeAcceptedCommandListWithClusterStateCache_endpoint_queue_completion clusterStateCacheContainer endpoint queue completion =
  do
    cls' <- getRequiredClass "MTRBaseClusterTimer"
    sendClassMessage cls' readAttributeAcceptedCommandListWithClusterStateCache_endpoint_queue_completionSelector (toMTRClusterStateCacheContainer clusterStateCacheContainer) (toNSNumber endpoint) (toNSObject queue) completion

-- | @- readAttributeAttributeListWithCompletion:@
readAttributeAttributeListWithCompletion :: IsMTRBaseClusterTimer mtrBaseClusterTimer => mtrBaseClusterTimer -> Ptr () -> IO ()
readAttributeAttributeListWithCompletion mtrBaseClusterTimer completion =
  sendMessage mtrBaseClusterTimer readAttributeAttributeListWithCompletionSelector completion

-- | @- subscribeAttributeAttributeListWithParams:subscriptionEstablished:reportHandler:@
subscribeAttributeAttributeListWithParams_subscriptionEstablished_reportHandler :: (IsMTRBaseClusterTimer mtrBaseClusterTimer, IsMTRSubscribeParams params) => mtrBaseClusterTimer -> params -> Ptr () -> Ptr () -> IO ()
subscribeAttributeAttributeListWithParams_subscriptionEstablished_reportHandler mtrBaseClusterTimer params subscriptionEstablished reportHandler =
  sendMessage mtrBaseClusterTimer subscribeAttributeAttributeListWithParams_subscriptionEstablished_reportHandlerSelector (toMTRSubscribeParams params) subscriptionEstablished reportHandler

-- | @+ readAttributeAttributeListWithClusterStateCache:endpoint:queue:completion:@
readAttributeAttributeListWithClusterStateCache_endpoint_queue_completion :: (IsMTRClusterStateCacheContainer clusterStateCacheContainer, IsNSNumber endpoint, IsNSObject queue) => clusterStateCacheContainer -> endpoint -> queue -> Ptr () -> IO ()
readAttributeAttributeListWithClusterStateCache_endpoint_queue_completion clusterStateCacheContainer endpoint queue completion =
  do
    cls' <- getRequiredClass "MTRBaseClusterTimer"
    sendClassMessage cls' readAttributeAttributeListWithClusterStateCache_endpoint_queue_completionSelector (toMTRClusterStateCacheContainer clusterStateCacheContainer) (toNSNumber endpoint) (toNSObject queue) completion

-- | @- readAttributeFeatureMapWithCompletion:@
readAttributeFeatureMapWithCompletion :: IsMTRBaseClusterTimer mtrBaseClusterTimer => mtrBaseClusterTimer -> Ptr () -> IO ()
readAttributeFeatureMapWithCompletion mtrBaseClusterTimer completion =
  sendMessage mtrBaseClusterTimer readAttributeFeatureMapWithCompletionSelector completion

-- | @- subscribeAttributeFeatureMapWithParams:subscriptionEstablished:reportHandler:@
subscribeAttributeFeatureMapWithParams_subscriptionEstablished_reportHandler :: (IsMTRBaseClusterTimer mtrBaseClusterTimer, IsMTRSubscribeParams params) => mtrBaseClusterTimer -> params -> Ptr () -> Ptr () -> IO ()
subscribeAttributeFeatureMapWithParams_subscriptionEstablished_reportHandler mtrBaseClusterTimer params subscriptionEstablished reportHandler =
  sendMessage mtrBaseClusterTimer subscribeAttributeFeatureMapWithParams_subscriptionEstablished_reportHandlerSelector (toMTRSubscribeParams params) subscriptionEstablished reportHandler

-- | @+ readAttributeFeatureMapWithClusterStateCache:endpoint:queue:completion:@
readAttributeFeatureMapWithClusterStateCache_endpoint_queue_completion :: (IsMTRClusterStateCacheContainer clusterStateCacheContainer, IsNSNumber endpoint, IsNSObject queue) => clusterStateCacheContainer -> endpoint -> queue -> Ptr () -> IO ()
readAttributeFeatureMapWithClusterStateCache_endpoint_queue_completion clusterStateCacheContainer endpoint queue completion =
  do
    cls' <- getRequiredClass "MTRBaseClusterTimer"
    sendClassMessage cls' readAttributeFeatureMapWithClusterStateCache_endpoint_queue_completionSelector (toMTRClusterStateCacheContainer clusterStateCacheContainer) (toNSNumber endpoint) (toNSObject queue) completion

-- | @- readAttributeClusterRevisionWithCompletion:@
readAttributeClusterRevisionWithCompletion :: IsMTRBaseClusterTimer mtrBaseClusterTimer => mtrBaseClusterTimer -> Ptr () -> IO ()
readAttributeClusterRevisionWithCompletion mtrBaseClusterTimer completion =
  sendMessage mtrBaseClusterTimer readAttributeClusterRevisionWithCompletionSelector completion

-- | @- subscribeAttributeClusterRevisionWithParams:subscriptionEstablished:reportHandler:@
subscribeAttributeClusterRevisionWithParams_subscriptionEstablished_reportHandler :: (IsMTRBaseClusterTimer mtrBaseClusterTimer, IsMTRSubscribeParams params) => mtrBaseClusterTimer -> params -> Ptr () -> Ptr () -> IO ()
subscribeAttributeClusterRevisionWithParams_subscriptionEstablished_reportHandler mtrBaseClusterTimer params subscriptionEstablished reportHandler =
  sendMessage mtrBaseClusterTimer subscribeAttributeClusterRevisionWithParams_subscriptionEstablished_reportHandlerSelector (toMTRSubscribeParams params) subscriptionEstablished reportHandler

-- | @+ readAttributeClusterRevisionWithClusterStateCache:endpoint:queue:completion:@
readAttributeClusterRevisionWithClusterStateCache_endpoint_queue_completion :: (IsMTRClusterStateCacheContainer clusterStateCacheContainer, IsNSNumber endpoint, IsNSObject queue) => clusterStateCacheContainer -> endpoint -> queue -> Ptr () -> IO ()
readAttributeClusterRevisionWithClusterStateCache_endpoint_queue_completion clusterStateCacheContainer endpoint queue completion =
  do
    cls' <- getRequiredClass "MTRBaseClusterTimer"
    sendClassMessage cls' readAttributeClusterRevisionWithClusterStateCache_endpoint_queue_completionSelector (toMTRClusterStateCacheContainer clusterStateCacheContainer) (toNSNumber endpoint) (toNSObject queue) completion

-- | @- init@
init_ :: IsMTRBaseClusterTimer mtrBaseClusterTimer => mtrBaseClusterTimer -> IO (Id MTRBaseClusterTimer)
init_ mtrBaseClusterTimer =
  sendOwnedMessage mtrBaseClusterTimer initSelector

-- | @+ new@
new :: IO (Id MTRBaseClusterTimer)
new  =
  do
    cls' <- getRequiredClass "MTRBaseClusterTimer"
    sendOwnedClassMessage cls' newSelector

-- | For all instance methods (reads, writes, commands) that take a completion, the completion will be called on the provided queue.
--
-- ObjC selector: @- initWithDevice:endpointID:queue:@
initWithDevice_endpointID_queue :: (IsMTRBaseClusterTimer mtrBaseClusterTimer, IsMTRBaseDevice device, IsNSNumber endpointID, IsNSObject queue) => mtrBaseClusterTimer -> device -> endpointID -> queue -> IO (Id MTRBaseClusterTimer)
initWithDevice_endpointID_queue mtrBaseClusterTimer device endpointID queue =
  sendOwnedMessage mtrBaseClusterTimer initWithDevice_endpointID_queueSelector (toMTRBaseDevice device) (toNSNumber endpointID) (toNSObject queue)

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @setTimerWithParams:completion:@
setTimerWithParams_completionSelector :: Selector '[Id MTRTimerClusterSetTimerParams, Ptr ()] ()
setTimerWithParams_completionSelector = mkSelector "setTimerWithParams:completion:"

-- | @Selector@ for @resetTimerWithParams:completion:@
resetTimerWithParams_completionSelector :: Selector '[Id MTRTimerClusterResetTimerParams, Ptr ()] ()
resetTimerWithParams_completionSelector = mkSelector "resetTimerWithParams:completion:"

-- | @Selector@ for @resetTimerWithCompletion:@
resetTimerWithCompletionSelector :: Selector '[Ptr ()] ()
resetTimerWithCompletionSelector = mkSelector "resetTimerWithCompletion:"

-- | @Selector@ for @addTimeWithParams:completion:@
addTimeWithParams_completionSelector :: Selector '[Id MTRTimerClusterAddTimeParams, Ptr ()] ()
addTimeWithParams_completionSelector = mkSelector "addTimeWithParams:completion:"

-- | @Selector@ for @reduceTimeWithParams:completion:@
reduceTimeWithParams_completionSelector :: Selector '[Id MTRTimerClusterReduceTimeParams, Ptr ()] ()
reduceTimeWithParams_completionSelector = mkSelector "reduceTimeWithParams:completion:"

-- | @Selector@ for @readAttributeSetTimeWithCompletion:@
readAttributeSetTimeWithCompletionSelector :: Selector '[Ptr ()] ()
readAttributeSetTimeWithCompletionSelector = mkSelector "readAttributeSetTimeWithCompletion:"

-- | @Selector@ for @subscribeAttributeSetTimeWithParams:subscriptionEstablished:reportHandler:@
subscribeAttributeSetTimeWithParams_subscriptionEstablished_reportHandlerSelector :: Selector '[Id MTRSubscribeParams, Ptr (), Ptr ()] ()
subscribeAttributeSetTimeWithParams_subscriptionEstablished_reportHandlerSelector = mkSelector "subscribeAttributeSetTimeWithParams:subscriptionEstablished:reportHandler:"

-- | @Selector@ for @readAttributeSetTimeWithClusterStateCache:endpoint:queue:completion:@
readAttributeSetTimeWithClusterStateCache_endpoint_queue_completionSelector :: Selector '[Id MTRClusterStateCacheContainer, Id NSNumber, Id NSObject, Ptr ()] ()
readAttributeSetTimeWithClusterStateCache_endpoint_queue_completionSelector = mkSelector "readAttributeSetTimeWithClusterStateCache:endpoint:queue:completion:"

-- | @Selector@ for @readAttributeTimeRemainingWithCompletion:@
readAttributeTimeRemainingWithCompletionSelector :: Selector '[Ptr ()] ()
readAttributeTimeRemainingWithCompletionSelector = mkSelector "readAttributeTimeRemainingWithCompletion:"

-- | @Selector@ for @subscribeAttributeTimeRemainingWithParams:subscriptionEstablished:reportHandler:@
subscribeAttributeTimeRemainingWithParams_subscriptionEstablished_reportHandlerSelector :: Selector '[Id MTRSubscribeParams, Ptr (), Ptr ()] ()
subscribeAttributeTimeRemainingWithParams_subscriptionEstablished_reportHandlerSelector = mkSelector "subscribeAttributeTimeRemainingWithParams:subscriptionEstablished:reportHandler:"

-- | @Selector@ for @readAttributeTimeRemainingWithClusterStateCache:endpoint:queue:completion:@
readAttributeTimeRemainingWithClusterStateCache_endpoint_queue_completionSelector :: Selector '[Id MTRClusterStateCacheContainer, Id NSNumber, Id NSObject, Ptr ()] ()
readAttributeTimeRemainingWithClusterStateCache_endpoint_queue_completionSelector = mkSelector "readAttributeTimeRemainingWithClusterStateCache:endpoint:queue:completion:"

-- | @Selector@ for @readAttributeTimerStateWithCompletion:@
readAttributeTimerStateWithCompletionSelector :: Selector '[Ptr ()] ()
readAttributeTimerStateWithCompletionSelector = mkSelector "readAttributeTimerStateWithCompletion:"

-- | @Selector@ for @subscribeAttributeTimerStateWithParams:subscriptionEstablished:reportHandler:@
subscribeAttributeTimerStateWithParams_subscriptionEstablished_reportHandlerSelector :: Selector '[Id MTRSubscribeParams, Ptr (), Ptr ()] ()
subscribeAttributeTimerStateWithParams_subscriptionEstablished_reportHandlerSelector = mkSelector "subscribeAttributeTimerStateWithParams:subscriptionEstablished:reportHandler:"

-- | @Selector@ for @readAttributeTimerStateWithClusterStateCache:endpoint:queue:completion:@
readAttributeTimerStateWithClusterStateCache_endpoint_queue_completionSelector :: Selector '[Id MTRClusterStateCacheContainer, Id NSNumber, Id NSObject, Ptr ()] ()
readAttributeTimerStateWithClusterStateCache_endpoint_queue_completionSelector = mkSelector "readAttributeTimerStateWithClusterStateCache:endpoint:queue:completion:"

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
initSelector :: Selector '[] (Id MTRBaseClusterTimer)
initSelector = mkSelector "init"

-- | @Selector@ for @new@
newSelector :: Selector '[] (Id MTRBaseClusterTimer)
newSelector = mkSelector "new"

-- | @Selector@ for @initWithDevice:endpointID:queue:@
initWithDevice_endpointID_queueSelector :: Selector '[Id MTRBaseDevice, Id NSNumber, Id NSObject] (Id MTRBaseClusterTimer)
initWithDevice_endpointID_queueSelector = mkSelector "initWithDevice:endpointID:queue:"

