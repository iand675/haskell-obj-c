{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Cluster Temperature Control
--
-- Attributes and commands for configuring the temperature control, and reporting temperature.
--
-- Generated bindings for @MTRBaseClusterTemperatureControl@.
module ObjC.Matter.MTRBaseClusterTemperatureControl
  ( MTRBaseClusterTemperatureControl
  , IsMTRBaseClusterTemperatureControl(..)
  , setTemperatureWithParams_completion
  , setTemperatureWithCompletion
  , readAttributeTemperatureSetpointWithCompletion
  , subscribeAttributeTemperatureSetpointWithParams_subscriptionEstablished_reportHandler
  , readAttributeTemperatureSetpointWithClusterStateCache_endpoint_queue_completion
  , readAttributeMinTemperatureWithCompletion
  , subscribeAttributeMinTemperatureWithParams_subscriptionEstablished_reportHandler
  , readAttributeMinTemperatureWithClusterStateCache_endpoint_queue_completion
  , readAttributeMaxTemperatureWithCompletion
  , subscribeAttributeMaxTemperatureWithParams_subscriptionEstablished_reportHandler
  , readAttributeMaxTemperatureWithClusterStateCache_endpoint_queue_completion
  , readAttributeStepWithCompletion
  , subscribeAttributeStepWithParams_subscriptionEstablished_reportHandler
  , readAttributeStepWithClusterStateCache_endpoint_queue_completion
  , readAttributeSelectedTemperatureLevelWithCompletion
  , subscribeAttributeSelectedTemperatureLevelWithParams_subscriptionEstablished_reportHandler
  , readAttributeSelectedTemperatureLevelWithClusterStateCache_endpoint_queue_completion
  , readAttributeSupportedTemperatureLevelsWithCompletion
  , subscribeAttributeSupportedTemperatureLevelsWithParams_subscriptionEstablished_reportHandler
  , readAttributeSupportedTemperatureLevelsWithClusterStateCache_endpoint_queue_completion
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
  , readAttributeFeatureMapWithClusterStateCache_endpoint_queue_completionSelector
  , readAttributeFeatureMapWithCompletionSelector
  , readAttributeGeneratedCommandListWithClusterStateCache_endpoint_queue_completionSelector
  , readAttributeGeneratedCommandListWithCompletionSelector
  , readAttributeMaxTemperatureWithClusterStateCache_endpoint_queue_completionSelector
  , readAttributeMaxTemperatureWithCompletionSelector
  , readAttributeMinTemperatureWithClusterStateCache_endpoint_queue_completionSelector
  , readAttributeMinTemperatureWithCompletionSelector
  , readAttributeSelectedTemperatureLevelWithClusterStateCache_endpoint_queue_completionSelector
  , readAttributeSelectedTemperatureLevelWithCompletionSelector
  , readAttributeStepWithClusterStateCache_endpoint_queue_completionSelector
  , readAttributeStepWithCompletionSelector
  , readAttributeSupportedTemperatureLevelsWithClusterStateCache_endpoint_queue_completionSelector
  , readAttributeSupportedTemperatureLevelsWithCompletionSelector
  , readAttributeTemperatureSetpointWithClusterStateCache_endpoint_queue_completionSelector
  , readAttributeTemperatureSetpointWithCompletionSelector
  , setTemperatureWithCompletionSelector
  , setTemperatureWithParams_completionSelector
  , subscribeAttributeAcceptedCommandListWithParams_subscriptionEstablished_reportHandlerSelector
  , subscribeAttributeAttributeListWithParams_subscriptionEstablished_reportHandlerSelector
  , subscribeAttributeClusterRevisionWithParams_subscriptionEstablished_reportHandlerSelector
  , subscribeAttributeFeatureMapWithParams_subscriptionEstablished_reportHandlerSelector
  , subscribeAttributeGeneratedCommandListWithParams_subscriptionEstablished_reportHandlerSelector
  , subscribeAttributeMaxTemperatureWithParams_subscriptionEstablished_reportHandlerSelector
  , subscribeAttributeMinTemperatureWithParams_subscriptionEstablished_reportHandlerSelector
  , subscribeAttributeSelectedTemperatureLevelWithParams_subscriptionEstablished_reportHandlerSelector
  , subscribeAttributeStepWithParams_subscriptionEstablished_reportHandlerSelector
  , subscribeAttributeSupportedTemperatureLevelsWithParams_subscriptionEstablished_reportHandlerSelector
  , subscribeAttributeTemperatureSetpointWithParams_subscriptionEstablished_reportHandlerSelector


  ) where

import Foreign.Ptr (Ptr, FunPtr)
import Foreign.C.Types

import ObjC.Runtime.Types
import ObjC.Runtime.Message (sendMessage, sendOwnedMessage, sendClassMessage, sendOwnedClassMessage)
import ObjC.Runtime.Selector (mkSelector)
import ObjC.Runtime.Class (getRequiredClass)

import ObjC.Matter.Internal.Classes
import ObjC.Foundation.Internal.Classes

-- | Command SetTemperature
--
-- The SetTemperature command SHALL have the following data fields:
--
-- ObjC selector: @- setTemperatureWithParams:completion:@
setTemperatureWithParams_completion :: (IsMTRBaseClusterTemperatureControl mtrBaseClusterTemperatureControl, IsMTRTemperatureControlClusterSetTemperatureParams params) => mtrBaseClusterTemperatureControl -> params -> Ptr () -> IO ()
setTemperatureWithParams_completion mtrBaseClusterTemperatureControl params completion =
  sendMessage mtrBaseClusterTemperatureControl setTemperatureWithParams_completionSelector (toMTRTemperatureControlClusterSetTemperatureParams params) completion

-- | @- setTemperatureWithCompletion:@
setTemperatureWithCompletion :: IsMTRBaseClusterTemperatureControl mtrBaseClusterTemperatureControl => mtrBaseClusterTemperatureControl -> Ptr () -> IO ()
setTemperatureWithCompletion mtrBaseClusterTemperatureControl completion =
  sendMessage mtrBaseClusterTemperatureControl setTemperatureWithCompletionSelector completion

-- | @- readAttributeTemperatureSetpointWithCompletion:@
readAttributeTemperatureSetpointWithCompletion :: IsMTRBaseClusterTemperatureControl mtrBaseClusterTemperatureControl => mtrBaseClusterTemperatureControl -> Ptr () -> IO ()
readAttributeTemperatureSetpointWithCompletion mtrBaseClusterTemperatureControl completion =
  sendMessage mtrBaseClusterTemperatureControl readAttributeTemperatureSetpointWithCompletionSelector completion

-- | @- subscribeAttributeTemperatureSetpointWithParams:subscriptionEstablished:reportHandler:@
subscribeAttributeTemperatureSetpointWithParams_subscriptionEstablished_reportHandler :: (IsMTRBaseClusterTemperatureControl mtrBaseClusterTemperatureControl, IsMTRSubscribeParams params) => mtrBaseClusterTemperatureControl -> params -> Ptr () -> Ptr () -> IO ()
subscribeAttributeTemperatureSetpointWithParams_subscriptionEstablished_reportHandler mtrBaseClusterTemperatureControl params subscriptionEstablished reportHandler =
  sendMessage mtrBaseClusterTemperatureControl subscribeAttributeTemperatureSetpointWithParams_subscriptionEstablished_reportHandlerSelector (toMTRSubscribeParams params) subscriptionEstablished reportHandler

-- | @+ readAttributeTemperatureSetpointWithClusterStateCache:endpoint:queue:completion:@
readAttributeTemperatureSetpointWithClusterStateCache_endpoint_queue_completion :: (IsMTRClusterStateCacheContainer clusterStateCacheContainer, IsNSNumber endpoint, IsNSObject queue) => clusterStateCacheContainer -> endpoint -> queue -> Ptr () -> IO ()
readAttributeTemperatureSetpointWithClusterStateCache_endpoint_queue_completion clusterStateCacheContainer endpoint queue completion =
  do
    cls' <- getRequiredClass "MTRBaseClusterTemperatureControl"
    sendClassMessage cls' readAttributeTemperatureSetpointWithClusterStateCache_endpoint_queue_completionSelector (toMTRClusterStateCacheContainer clusterStateCacheContainer) (toNSNumber endpoint) (toNSObject queue) completion

-- | @- readAttributeMinTemperatureWithCompletion:@
readAttributeMinTemperatureWithCompletion :: IsMTRBaseClusterTemperatureControl mtrBaseClusterTemperatureControl => mtrBaseClusterTemperatureControl -> Ptr () -> IO ()
readAttributeMinTemperatureWithCompletion mtrBaseClusterTemperatureControl completion =
  sendMessage mtrBaseClusterTemperatureControl readAttributeMinTemperatureWithCompletionSelector completion

-- | @- subscribeAttributeMinTemperatureWithParams:subscriptionEstablished:reportHandler:@
subscribeAttributeMinTemperatureWithParams_subscriptionEstablished_reportHandler :: (IsMTRBaseClusterTemperatureControl mtrBaseClusterTemperatureControl, IsMTRSubscribeParams params) => mtrBaseClusterTemperatureControl -> params -> Ptr () -> Ptr () -> IO ()
subscribeAttributeMinTemperatureWithParams_subscriptionEstablished_reportHandler mtrBaseClusterTemperatureControl params subscriptionEstablished reportHandler =
  sendMessage mtrBaseClusterTemperatureControl subscribeAttributeMinTemperatureWithParams_subscriptionEstablished_reportHandlerSelector (toMTRSubscribeParams params) subscriptionEstablished reportHandler

-- | @+ readAttributeMinTemperatureWithClusterStateCache:endpoint:queue:completion:@
readAttributeMinTemperatureWithClusterStateCache_endpoint_queue_completion :: (IsMTRClusterStateCacheContainer clusterStateCacheContainer, IsNSNumber endpoint, IsNSObject queue) => clusterStateCacheContainer -> endpoint -> queue -> Ptr () -> IO ()
readAttributeMinTemperatureWithClusterStateCache_endpoint_queue_completion clusterStateCacheContainer endpoint queue completion =
  do
    cls' <- getRequiredClass "MTRBaseClusterTemperatureControl"
    sendClassMessage cls' readAttributeMinTemperatureWithClusterStateCache_endpoint_queue_completionSelector (toMTRClusterStateCacheContainer clusterStateCacheContainer) (toNSNumber endpoint) (toNSObject queue) completion

-- | @- readAttributeMaxTemperatureWithCompletion:@
readAttributeMaxTemperatureWithCompletion :: IsMTRBaseClusterTemperatureControl mtrBaseClusterTemperatureControl => mtrBaseClusterTemperatureControl -> Ptr () -> IO ()
readAttributeMaxTemperatureWithCompletion mtrBaseClusterTemperatureControl completion =
  sendMessage mtrBaseClusterTemperatureControl readAttributeMaxTemperatureWithCompletionSelector completion

-- | @- subscribeAttributeMaxTemperatureWithParams:subscriptionEstablished:reportHandler:@
subscribeAttributeMaxTemperatureWithParams_subscriptionEstablished_reportHandler :: (IsMTRBaseClusterTemperatureControl mtrBaseClusterTemperatureControl, IsMTRSubscribeParams params) => mtrBaseClusterTemperatureControl -> params -> Ptr () -> Ptr () -> IO ()
subscribeAttributeMaxTemperatureWithParams_subscriptionEstablished_reportHandler mtrBaseClusterTemperatureControl params subscriptionEstablished reportHandler =
  sendMessage mtrBaseClusterTemperatureControl subscribeAttributeMaxTemperatureWithParams_subscriptionEstablished_reportHandlerSelector (toMTRSubscribeParams params) subscriptionEstablished reportHandler

-- | @+ readAttributeMaxTemperatureWithClusterStateCache:endpoint:queue:completion:@
readAttributeMaxTemperatureWithClusterStateCache_endpoint_queue_completion :: (IsMTRClusterStateCacheContainer clusterStateCacheContainer, IsNSNumber endpoint, IsNSObject queue) => clusterStateCacheContainer -> endpoint -> queue -> Ptr () -> IO ()
readAttributeMaxTemperatureWithClusterStateCache_endpoint_queue_completion clusterStateCacheContainer endpoint queue completion =
  do
    cls' <- getRequiredClass "MTRBaseClusterTemperatureControl"
    sendClassMessage cls' readAttributeMaxTemperatureWithClusterStateCache_endpoint_queue_completionSelector (toMTRClusterStateCacheContainer clusterStateCacheContainer) (toNSNumber endpoint) (toNSObject queue) completion

-- | @- readAttributeStepWithCompletion:@
readAttributeStepWithCompletion :: IsMTRBaseClusterTemperatureControl mtrBaseClusterTemperatureControl => mtrBaseClusterTemperatureControl -> Ptr () -> IO ()
readAttributeStepWithCompletion mtrBaseClusterTemperatureControl completion =
  sendMessage mtrBaseClusterTemperatureControl readAttributeStepWithCompletionSelector completion

-- | @- subscribeAttributeStepWithParams:subscriptionEstablished:reportHandler:@
subscribeAttributeStepWithParams_subscriptionEstablished_reportHandler :: (IsMTRBaseClusterTemperatureControl mtrBaseClusterTemperatureControl, IsMTRSubscribeParams params) => mtrBaseClusterTemperatureControl -> params -> Ptr () -> Ptr () -> IO ()
subscribeAttributeStepWithParams_subscriptionEstablished_reportHandler mtrBaseClusterTemperatureControl params subscriptionEstablished reportHandler =
  sendMessage mtrBaseClusterTemperatureControl subscribeAttributeStepWithParams_subscriptionEstablished_reportHandlerSelector (toMTRSubscribeParams params) subscriptionEstablished reportHandler

-- | @+ readAttributeStepWithClusterStateCache:endpoint:queue:completion:@
readAttributeStepWithClusterStateCache_endpoint_queue_completion :: (IsMTRClusterStateCacheContainer clusterStateCacheContainer, IsNSNumber endpoint, IsNSObject queue) => clusterStateCacheContainer -> endpoint -> queue -> Ptr () -> IO ()
readAttributeStepWithClusterStateCache_endpoint_queue_completion clusterStateCacheContainer endpoint queue completion =
  do
    cls' <- getRequiredClass "MTRBaseClusterTemperatureControl"
    sendClassMessage cls' readAttributeStepWithClusterStateCache_endpoint_queue_completionSelector (toMTRClusterStateCacheContainer clusterStateCacheContainer) (toNSNumber endpoint) (toNSObject queue) completion

-- | @- readAttributeSelectedTemperatureLevelWithCompletion:@
readAttributeSelectedTemperatureLevelWithCompletion :: IsMTRBaseClusterTemperatureControl mtrBaseClusterTemperatureControl => mtrBaseClusterTemperatureControl -> Ptr () -> IO ()
readAttributeSelectedTemperatureLevelWithCompletion mtrBaseClusterTemperatureControl completion =
  sendMessage mtrBaseClusterTemperatureControl readAttributeSelectedTemperatureLevelWithCompletionSelector completion

-- | @- subscribeAttributeSelectedTemperatureLevelWithParams:subscriptionEstablished:reportHandler:@
subscribeAttributeSelectedTemperatureLevelWithParams_subscriptionEstablished_reportHandler :: (IsMTRBaseClusterTemperatureControl mtrBaseClusterTemperatureControl, IsMTRSubscribeParams params) => mtrBaseClusterTemperatureControl -> params -> Ptr () -> Ptr () -> IO ()
subscribeAttributeSelectedTemperatureLevelWithParams_subscriptionEstablished_reportHandler mtrBaseClusterTemperatureControl params subscriptionEstablished reportHandler =
  sendMessage mtrBaseClusterTemperatureControl subscribeAttributeSelectedTemperatureLevelWithParams_subscriptionEstablished_reportHandlerSelector (toMTRSubscribeParams params) subscriptionEstablished reportHandler

-- | @+ readAttributeSelectedTemperatureLevelWithClusterStateCache:endpoint:queue:completion:@
readAttributeSelectedTemperatureLevelWithClusterStateCache_endpoint_queue_completion :: (IsMTRClusterStateCacheContainer clusterStateCacheContainer, IsNSNumber endpoint, IsNSObject queue) => clusterStateCacheContainer -> endpoint -> queue -> Ptr () -> IO ()
readAttributeSelectedTemperatureLevelWithClusterStateCache_endpoint_queue_completion clusterStateCacheContainer endpoint queue completion =
  do
    cls' <- getRequiredClass "MTRBaseClusterTemperatureControl"
    sendClassMessage cls' readAttributeSelectedTemperatureLevelWithClusterStateCache_endpoint_queue_completionSelector (toMTRClusterStateCacheContainer clusterStateCacheContainer) (toNSNumber endpoint) (toNSObject queue) completion

-- | @- readAttributeSupportedTemperatureLevelsWithCompletion:@
readAttributeSupportedTemperatureLevelsWithCompletion :: IsMTRBaseClusterTemperatureControl mtrBaseClusterTemperatureControl => mtrBaseClusterTemperatureControl -> Ptr () -> IO ()
readAttributeSupportedTemperatureLevelsWithCompletion mtrBaseClusterTemperatureControl completion =
  sendMessage mtrBaseClusterTemperatureControl readAttributeSupportedTemperatureLevelsWithCompletionSelector completion

-- | @- subscribeAttributeSupportedTemperatureLevelsWithParams:subscriptionEstablished:reportHandler:@
subscribeAttributeSupportedTemperatureLevelsWithParams_subscriptionEstablished_reportHandler :: (IsMTRBaseClusterTemperatureControl mtrBaseClusterTemperatureControl, IsMTRSubscribeParams params) => mtrBaseClusterTemperatureControl -> params -> Ptr () -> Ptr () -> IO ()
subscribeAttributeSupportedTemperatureLevelsWithParams_subscriptionEstablished_reportHandler mtrBaseClusterTemperatureControl params subscriptionEstablished reportHandler =
  sendMessage mtrBaseClusterTemperatureControl subscribeAttributeSupportedTemperatureLevelsWithParams_subscriptionEstablished_reportHandlerSelector (toMTRSubscribeParams params) subscriptionEstablished reportHandler

-- | @+ readAttributeSupportedTemperatureLevelsWithClusterStateCache:endpoint:queue:completion:@
readAttributeSupportedTemperatureLevelsWithClusterStateCache_endpoint_queue_completion :: (IsMTRClusterStateCacheContainer clusterStateCacheContainer, IsNSNumber endpoint, IsNSObject queue) => clusterStateCacheContainer -> endpoint -> queue -> Ptr () -> IO ()
readAttributeSupportedTemperatureLevelsWithClusterStateCache_endpoint_queue_completion clusterStateCacheContainer endpoint queue completion =
  do
    cls' <- getRequiredClass "MTRBaseClusterTemperatureControl"
    sendClassMessage cls' readAttributeSupportedTemperatureLevelsWithClusterStateCache_endpoint_queue_completionSelector (toMTRClusterStateCacheContainer clusterStateCacheContainer) (toNSNumber endpoint) (toNSObject queue) completion

-- | @- readAttributeGeneratedCommandListWithCompletion:@
readAttributeGeneratedCommandListWithCompletion :: IsMTRBaseClusterTemperatureControl mtrBaseClusterTemperatureControl => mtrBaseClusterTemperatureControl -> Ptr () -> IO ()
readAttributeGeneratedCommandListWithCompletion mtrBaseClusterTemperatureControl completion =
  sendMessage mtrBaseClusterTemperatureControl readAttributeGeneratedCommandListWithCompletionSelector completion

-- | @- subscribeAttributeGeneratedCommandListWithParams:subscriptionEstablished:reportHandler:@
subscribeAttributeGeneratedCommandListWithParams_subscriptionEstablished_reportHandler :: (IsMTRBaseClusterTemperatureControl mtrBaseClusterTemperatureControl, IsMTRSubscribeParams params) => mtrBaseClusterTemperatureControl -> params -> Ptr () -> Ptr () -> IO ()
subscribeAttributeGeneratedCommandListWithParams_subscriptionEstablished_reportHandler mtrBaseClusterTemperatureControl params subscriptionEstablished reportHandler =
  sendMessage mtrBaseClusterTemperatureControl subscribeAttributeGeneratedCommandListWithParams_subscriptionEstablished_reportHandlerSelector (toMTRSubscribeParams params) subscriptionEstablished reportHandler

-- | @+ readAttributeGeneratedCommandListWithClusterStateCache:endpoint:queue:completion:@
readAttributeGeneratedCommandListWithClusterStateCache_endpoint_queue_completion :: (IsMTRClusterStateCacheContainer clusterStateCacheContainer, IsNSNumber endpoint, IsNSObject queue) => clusterStateCacheContainer -> endpoint -> queue -> Ptr () -> IO ()
readAttributeGeneratedCommandListWithClusterStateCache_endpoint_queue_completion clusterStateCacheContainer endpoint queue completion =
  do
    cls' <- getRequiredClass "MTRBaseClusterTemperatureControl"
    sendClassMessage cls' readAttributeGeneratedCommandListWithClusterStateCache_endpoint_queue_completionSelector (toMTRClusterStateCacheContainer clusterStateCacheContainer) (toNSNumber endpoint) (toNSObject queue) completion

-- | @- readAttributeAcceptedCommandListWithCompletion:@
readAttributeAcceptedCommandListWithCompletion :: IsMTRBaseClusterTemperatureControl mtrBaseClusterTemperatureControl => mtrBaseClusterTemperatureControl -> Ptr () -> IO ()
readAttributeAcceptedCommandListWithCompletion mtrBaseClusterTemperatureControl completion =
  sendMessage mtrBaseClusterTemperatureControl readAttributeAcceptedCommandListWithCompletionSelector completion

-- | @- subscribeAttributeAcceptedCommandListWithParams:subscriptionEstablished:reportHandler:@
subscribeAttributeAcceptedCommandListWithParams_subscriptionEstablished_reportHandler :: (IsMTRBaseClusterTemperatureControl mtrBaseClusterTemperatureControl, IsMTRSubscribeParams params) => mtrBaseClusterTemperatureControl -> params -> Ptr () -> Ptr () -> IO ()
subscribeAttributeAcceptedCommandListWithParams_subscriptionEstablished_reportHandler mtrBaseClusterTemperatureControl params subscriptionEstablished reportHandler =
  sendMessage mtrBaseClusterTemperatureControl subscribeAttributeAcceptedCommandListWithParams_subscriptionEstablished_reportHandlerSelector (toMTRSubscribeParams params) subscriptionEstablished reportHandler

-- | @+ readAttributeAcceptedCommandListWithClusterStateCache:endpoint:queue:completion:@
readAttributeAcceptedCommandListWithClusterStateCache_endpoint_queue_completion :: (IsMTRClusterStateCacheContainer clusterStateCacheContainer, IsNSNumber endpoint, IsNSObject queue) => clusterStateCacheContainer -> endpoint -> queue -> Ptr () -> IO ()
readAttributeAcceptedCommandListWithClusterStateCache_endpoint_queue_completion clusterStateCacheContainer endpoint queue completion =
  do
    cls' <- getRequiredClass "MTRBaseClusterTemperatureControl"
    sendClassMessage cls' readAttributeAcceptedCommandListWithClusterStateCache_endpoint_queue_completionSelector (toMTRClusterStateCacheContainer clusterStateCacheContainer) (toNSNumber endpoint) (toNSObject queue) completion

-- | @- readAttributeAttributeListWithCompletion:@
readAttributeAttributeListWithCompletion :: IsMTRBaseClusterTemperatureControl mtrBaseClusterTemperatureControl => mtrBaseClusterTemperatureControl -> Ptr () -> IO ()
readAttributeAttributeListWithCompletion mtrBaseClusterTemperatureControl completion =
  sendMessage mtrBaseClusterTemperatureControl readAttributeAttributeListWithCompletionSelector completion

-- | @- subscribeAttributeAttributeListWithParams:subscriptionEstablished:reportHandler:@
subscribeAttributeAttributeListWithParams_subscriptionEstablished_reportHandler :: (IsMTRBaseClusterTemperatureControl mtrBaseClusterTemperatureControl, IsMTRSubscribeParams params) => mtrBaseClusterTemperatureControl -> params -> Ptr () -> Ptr () -> IO ()
subscribeAttributeAttributeListWithParams_subscriptionEstablished_reportHandler mtrBaseClusterTemperatureControl params subscriptionEstablished reportHandler =
  sendMessage mtrBaseClusterTemperatureControl subscribeAttributeAttributeListWithParams_subscriptionEstablished_reportHandlerSelector (toMTRSubscribeParams params) subscriptionEstablished reportHandler

-- | @+ readAttributeAttributeListWithClusterStateCache:endpoint:queue:completion:@
readAttributeAttributeListWithClusterStateCache_endpoint_queue_completion :: (IsMTRClusterStateCacheContainer clusterStateCacheContainer, IsNSNumber endpoint, IsNSObject queue) => clusterStateCacheContainer -> endpoint -> queue -> Ptr () -> IO ()
readAttributeAttributeListWithClusterStateCache_endpoint_queue_completion clusterStateCacheContainer endpoint queue completion =
  do
    cls' <- getRequiredClass "MTRBaseClusterTemperatureControl"
    sendClassMessage cls' readAttributeAttributeListWithClusterStateCache_endpoint_queue_completionSelector (toMTRClusterStateCacheContainer clusterStateCacheContainer) (toNSNumber endpoint) (toNSObject queue) completion

-- | @- readAttributeFeatureMapWithCompletion:@
readAttributeFeatureMapWithCompletion :: IsMTRBaseClusterTemperatureControl mtrBaseClusterTemperatureControl => mtrBaseClusterTemperatureControl -> Ptr () -> IO ()
readAttributeFeatureMapWithCompletion mtrBaseClusterTemperatureControl completion =
  sendMessage mtrBaseClusterTemperatureControl readAttributeFeatureMapWithCompletionSelector completion

-- | @- subscribeAttributeFeatureMapWithParams:subscriptionEstablished:reportHandler:@
subscribeAttributeFeatureMapWithParams_subscriptionEstablished_reportHandler :: (IsMTRBaseClusterTemperatureControl mtrBaseClusterTemperatureControl, IsMTRSubscribeParams params) => mtrBaseClusterTemperatureControl -> params -> Ptr () -> Ptr () -> IO ()
subscribeAttributeFeatureMapWithParams_subscriptionEstablished_reportHandler mtrBaseClusterTemperatureControl params subscriptionEstablished reportHandler =
  sendMessage mtrBaseClusterTemperatureControl subscribeAttributeFeatureMapWithParams_subscriptionEstablished_reportHandlerSelector (toMTRSubscribeParams params) subscriptionEstablished reportHandler

-- | @+ readAttributeFeatureMapWithClusterStateCache:endpoint:queue:completion:@
readAttributeFeatureMapWithClusterStateCache_endpoint_queue_completion :: (IsMTRClusterStateCacheContainer clusterStateCacheContainer, IsNSNumber endpoint, IsNSObject queue) => clusterStateCacheContainer -> endpoint -> queue -> Ptr () -> IO ()
readAttributeFeatureMapWithClusterStateCache_endpoint_queue_completion clusterStateCacheContainer endpoint queue completion =
  do
    cls' <- getRequiredClass "MTRBaseClusterTemperatureControl"
    sendClassMessage cls' readAttributeFeatureMapWithClusterStateCache_endpoint_queue_completionSelector (toMTRClusterStateCacheContainer clusterStateCacheContainer) (toNSNumber endpoint) (toNSObject queue) completion

-- | @- readAttributeClusterRevisionWithCompletion:@
readAttributeClusterRevisionWithCompletion :: IsMTRBaseClusterTemperatureControl mtrBaseClusterTemperatureControl => mtrBaseClusterTemperatureControl -> Ptr () -> IO ()
readAttributeClusterRevisionWithCompletion mtrBaseClusterTemperatureControl completion =
  sendMessage mtrBaseClusterTemperatureControl readAttributeClusterRevisionWithCompletionSelector completion

-- | @- subscribeAttributeClusterRevisionWithParams:subscriptionEstablished:reportHandler:@
subscribeAttributeClusterRevisionWithParams_subscriptionEstablished_reportHandler :: (IsMTRBaseClusterTemperatureControl mtrBaseClusterTemperatureControl, IsMTRSubscribeParams params) => mtrBaseClusterTemperatureControl -> params -> Ptr () -> Ptr () -> IO ()
subscribeAttributeClusterRevisionWithParams_subscriptionEstablished_reportHandler mtrBaseClusterTemperatureControl params subscriptionEstablished reportHandler =
  sendMessage mtrBaseClusterTemperatureControl subscribeAttributeClusterRevisionWithParams_subscriptionEstablished_reportHandlerSelector (toMTRSubscribeParams params) subscriptionEstablished reportHandler

-- | @+ readAttributeClusterRevisionWithClusterStateCache:endpoint:queue:completion:@
readAttributeClusterRevisionWithClusterStateCache_endpoint_queue_completion :: (IsMTRClusterStateCacheContainer clusterStateCacheContainer, IsNSNumber endpoint, IsNSObject queue) => clusterStateCacheContainer -> endpoint -> queue -> Ptr () -> IO ()
readAttributeClusterRevisionWithClusterStateCache_endpoint_queue_completion clusterStateCacheContainer endpoint queue completion =
  do
    cls' <- getRequiredClass "MTRBaseClusterTemperatureControl"
    sendClassMessage cls' readAttributeClusterRevisionWithClusterStateCache_endpoint_queue_completionSelector (toMTRClusterStateCacheContainer clusterStateCacheContainer) (toNSNumber endpoint) (toNSObject queue) completion

-- | @- init@
init_ :: IsMTRBaseClusterTemperatureControl mtrBaseClusterTemperatureControl => mtrBaseClusterTemperatureControl -> IO (Id MTRBaseClusterTemperatureControl)
init_ mtrBaseClusterTemperatureControl =
  sendOwnedMessage mtrBaseClusterTemperatureControl initSelector

-- | @+ new@
new :: IO (Id MTRBaseClusterTemperatureControl)
new  =
  do
    cls' <- getRequiredClass "MTRBaseClusterTemperatureControl"
    sendOwnedClassMessage cls' newSelector

-- | For all instance methods (reads, writes, commands) that take a completion, the completion will be called on the provided queue.
--
-- ObjC selector: @- initWithDevice:endpointID:queue:@
initWithDevice_endpointID_queue :: (IsMTRBaseClusterTemperatureControl mtrBaseClusterTemperatureControl, IsMTRBaseDevice device, IsNSNumber endpointID, IsNSObject queue) => mtrBaseClusterTemperatureControl -> device -> endpointID -> queue -> IO (Id MTRBaseClusterTemperatureControl)
initWithDevice_endpointID_queue mtrBaseClusterTemperatureControl device endpointID queue =
  sendOwnedMessage mtrBaseClusterTemperatureControl initWithDevice_endpointID_queueSelector (toMTRBaseDevice device) (toNSNumber endpointID) (toNSObject queue)

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @setTemperatureWithParams:completion:@
setTemperatureWithParams_completionSelector :: Selector '[Id MTRTemperatureControlClusterSetTemperatureParams, Ptr ()] ()
setTemperatureWithParams_completionSelector = mkSelector "setTemperatureWithParams:completion:"

-- | @Selector@ for @setTemperatureWithCompletion:@
setTemperatureWithCompletionSelector :: Selector '[Ptr ()] ()
setTemperatureWithCompletionSelector = mkSelector "setTemperatureWithCompletion:"

-- | @Selector@ for @readAttributeTemperatureSetpointWithCompletion:@
readAttributeTemperatureSetpointWithCompletionSelector :: Selector '[Ptr ()] ()
readAttributeTemperatureSetpointWithCompletionSelector = mkSelector "readAttributeTemperatureSetpointWithCompletion:"

-- | @Selector@ for @subscribeAttributeTemperatureSetpointWithParams:subscriptionEstablished:reportHandler:@
subscribeAttributeTemperatureSetpointWithParams_subscriptionEstablished_reportHandlerSelector :: Selector '[Id MTRSubscribeParams, Ptr (), Ptr ()] ()
subscribeAttributeTemperatureSetpointWithParams_subscriptionEstablished_reportHandlerSelector = mkSelector "subscribeAttributeTemperatureSetpointWithParams:subscriptionEstablished:reportHandler:"

-- | @Selector@ for @readAttributeTemperatureSetpointWithClusterStateCache:endpoint:queue:completion:@
readAttributeTemperatureSetpointWithClusterStateCache_endpoint_queue_completionSelector :: Selector '[Id MTRClusterStateCacheContainer, Id NSNumber, Id NSObject, Ptr ()] ()
readAttributeTemperatureSetpointWithClusterStateCache_endpoint_queue_completionSelector = mkSelector "readAttributeTemperatureSetpointWithClusterStateCache:endpoint:queue:completion:"

-- | @Selector@ for @readAttributeMinTemperatureWithCompletion:@
readAttributeMinTemperatureWithCompletionSelector :: Selector '[Ptr ()] ()
readAttributeMinTemperatureWithCompletionSelector = mkSelector "readAttributeMinTemperatureWithCompletion:"

-- | @Selector@ for @subscribeAttributeMinTemperatureWithParams:subscriptionEstablished:reportHandler:@
subscribeAttributeMinTemperatureWithParams_subscriptionEstablished_reportHandlerSelector :: Selector '[Id MTRSubscribeParams, Ptr (), Ptr ()] ()
subscribeAttributeMinTemperatureWithParams_subscriptionEstablished_reportHandlerSelector = mkSelector "subscribeAttributeMinTemperatureWithParams:subscriptionEstablished:reportHandler:"

-- | @Selector@ for @readAttributeMinTemperatureWithClusterStateCache:endpoint:queue:completion:@
readAttributeMinTemperatureWithClusterStateCache_endpoint_queue_completionSelector :: Selector '[Id MTRClusterStateCacheContainer, Id NSNumber, Id NSObject, Ptr ()] ()
readAttributeMinTemperatureWithClusterStateCache_endpoint_queue_completionSelector = mkSelector "readAttributeMinTemperatureWithClusterStateCache:endpoint:queue:completion:"

-- | @Selector@ for @readAttributeMaxTemperatureWithCompletion:@
readAttributeMaxTemperatureWithCompletionSelector :: Selector '[Ptr ()] ()
readAttributeMaxTemperatureWithCompletionSelector = mkSelector "readAttributeMaxTemperatureWithCompletion:"

-- | @Selector@ for @subscribeAttributeMaxTemperatureWithParams:subscriptionEstablished:reportHandler:@
subscribeAttributeMaxTemperatureWithParams_subscriptionEstablished_reportHandlerSelector :: Selector '[Id MTRSubscribeParams, Ptr (), Ptr ()] ()
subscribeAttributeMaxTemperatureWithParams_subscriptionEstablished_reportHandlerSelector = mkSelector "subscribeAttributeMaxTemperatureWithParams:subscriptionEstablished:reportHandler:"

-- | @Selector@ for @readAttributeMaxTemperatureWithClusterStateCache:endpoint:queue:completion:@
readAttributeMaxTemperatureWithClusterStateCache_endpoint_queue_completionSelector :: Selector '[Id MTRClusterStateCacheContainer, Id NSNumber, Id NSObject, Ptr ()] ()
readAttributeMaxTemperatureWithClusterStateCache_endpoint_queue_completionSelector = mkSelector "readAttributeMaxTemperatureWithClusterStateCache:endpoint:queue:completion:"

-- | @Selector@ for @readAttributeStepWithCompletion:@
readAttributeStepWithCompletionSelector :: Selector '[Ptr ()] ()
readAttributeStepWithCompletionSelector = mkSelector "readAttributeStepWithCompletion:"

-- | @Selector@ for @subscribeAttributeStepWithParams:subscriptionEstablished:reportHandler:@
subscribeAttributeStepWithParams_subscriptionEstablished_reportHandlerSelector :: Selector '[Id MTRSubscribeParams, Ptr (), Ptr ()] ()
subscribeAttributeStepWithParams_subscriptionEstablished_reportHandlerSelector = mkSelector "subscribeAttributeStepWithParams:subscriptionEstablished:reportHandler:"

-- | @Selector@ for @readAttributeStepWithClusterStateCache:endpoint:queue:completion:@
readAttributeStepWithClusterStateCache_endpoint_queue_completionSelector :: Selector '[Id MTRClusterStateCacheContainer, Id NSNumber, Id NSObject, Ptr ()] ()
readAttributeStepWithClusterStateCache_endpoint_queue_completionSelector = mkSelector "readAttributeStepWithClusterStateCache:endpoint:queue:completion:"

-- | @Selector@ for @readAttributeSelectedTemperatureLevelWithCompletion:@
readAttributeSelectedTemperatureLevelWithCompletionSelector :: Selector '[Ptr ()] ()
readAttributeSelectedTemperatureLevelWithCompletionSelector = mkSelector "readAttributeSelectedTemperatureLevelWithCompletion:"

-- | @Selector@ for @subscribeAttributeSelectedTemperatureLevelWithParams:subscriptionEstablished:reportHandler:@
subscribeAttributeSelectedTemperatureLevelWithParams_subscriptionEstablished_reportHandlerSelector :: Selector '[Id MTRSubscribeParams, Ptr (), Ptr ()] ()
subscribeAttributeSelectedTemperatureLevelWithParams_subscriptionEstablished_reportHandlerSelector = mkSelector "subscribeAttributeSelectedTemperatureLevelWithParams:subscriptionEstablished:reportHandler:"

-- | @Selector@ for @readAttributeSelectedTemperatureLevelWithClusterStateCache:endpoint:queue:completion:@
readAttributeSelectedTemperatureLevelWithClusterStateCache_endpoint_queue_completionSelector :: Selector '[Id MTRClusterStateCacheContainer, Id NSNumber, Id NSObject, Ptr ()] ()
readAttributeSelectedTemperatureLevelWithClusterStateCache_endpoint_queue_completionSelector = mkSelector "readAttributeSelectedTemperatureLevelWithClusterStateCache:endpoint:queue:completion:"

-- | @Selector@ for @readAttributeSupportedTemperatureLevelsWithCompletion:@
readAttributeSupportedTemperatureLevelsWithCompletionSelector :: Selector '[Ptr ()] ()
readAttributeSupportedTemperatureLevelsWithCompletionSelector = mkSelector "readAttributeSupportedTemperatureLevelsWithCompletion:"

-- | @Selector@ for @subscribeAttributeSupportedTemperatureLevelsWithParams:subscriptionEstablished:reportHandler:@
subscribeAttributeSupportedTemperatureLevelsWithParams_subscriptionEstablished_reportHandlerSelector :: Selector '[Id MTRSubscribeParams, Ptr (), Ptr ()] ()
subscribeAttributeSupportedTemperatureLevelsWithParams_subscriptionEstablished_reportHandlerSelector = mkSelector "subscribeAttributeSupportedTemperatureLevelsWithParams:subscriptionEstablished:reportHandler:"

-- | @Selector@ for @readAttributeSupportedTemperatureLevelsWithClusterStateCache:endpoint:queue:completion:@
readAttributeSupportedTemperatureLevelsWithClusterStateCache_endpoint_queue_completionSelector :: Selector '[Id MTRClusterStateCacheContainer, Id NSNumber, Id NSObject, Ptr ()] ()
readAttributeSupportedTemperatureLevelsWithClusterStateCache_endpoint_queue_completionSelector = mkSelector "readAttributeSupportedTemperatureLevelsWithClusterStateCache:endpoint:queue:completion:"

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
initSelector :: Selector '[] (Id MTRBaseClusterTemperatureControl)
initSelector = mkSelector "init"

-- | @Selector@ for @new@
newSelector :: Selector '[] (Id MTRBaseClusterTemperatureControl)
newSelector = mkSelector "new"

-- | @Selector@ for @initWithDevice:endpointID:queue:@
initWithDevice_endpointID_queueSelector :: Selector '[Id MTRBaseDevice, Id NSNumber, Id NSObject] (Id MTRBaseClusterTemperatureControl)
initWithDevice_endpointID_queueSelector = mkSelector "initWithDevice:endpointID:queue:"

