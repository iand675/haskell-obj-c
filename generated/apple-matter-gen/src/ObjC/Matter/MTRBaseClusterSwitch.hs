{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Cluster Switch
--
-- This cluster exposes interactions with a switch device, for the purpose of using those interactions by other devices.Two types of switch devices are supported: latching switch (e.g. rocker switch) and momentary switch (e.g. push button), distinguished with their feature flags.Interactions with the switch device are exposed as attributes (for the latching switch) and as events (for both types of switches). An interested party MAY subscribe to these attributes/events and thus be informed of the interactions, and can perform actions based on this, for example by sending commands to perform an action such as controlling a light or a window shade.
--
-- Generated bindings for @MTRBaseClusterSwitch@.
module ObjC.Matter.MTRBaseClusterSwitch
  ( MTRBaseClusterSwitch
  , IsMTRBaseClusterSwitch(..)
  , readAttributeNumberOfPositionsWithCompletion
  , subscribeAttributeNumberOfPositionsWithParams_subscriptionEstablished_reportHandler
  , readAttributeNumberOfPositionsWithClusterStateCache_endpoint_queue_completion
  , readAttributeCurrentPositionWithCompletion
  , subscribeAttributeCurrentPositionWithParams_subscriptionEstablished_reportHandler
  , readAttributeCurrentPositionWithClusterStateCache_endpoint_queue_completion
  , readAttributeMultiPressMaxWithCompletion
  , subscribeAttributeMultiPressMaxWithParams_subscriptionEstablished_reportHandler
  , readAttributeMultiPressMaxWithClusterStateCache_endpoint_queue_completion
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
  , initWithDevice_endpoint_queue
  , readAttributeNumberOfPositionsWithCompletionHandler
  , subscribeAttributeNumberOfPositionsWithMinInterval_maxInterval_params_subscriptionEstablished_reportHandler
  , readAttributeNumberOfPositionsWithAttributeCache_endpoint_queue_completionHandler
  , readAttributeCurrentPositionWithCompletionHandler
  , subscribeAttributeCurrentPositionWithMinInterval_maxInterval_params_subscriptionEstablished_reportHandler
  , readAttributeCurrentPositionWithAttributeCache_endpoint_queue_completionHandler
  , readAttributeMultiPressMaxWithCompletionHandler
  , subscribeAttributeMultiPressMaxWithMinInterval_maxInterval_params_subscriptionEstablished_reportHandler
  , readAttributeMultiPressMaxWithAttributeCache_endpoint_queue_completionHandler
  , readAttributeGeneratedCommandListWithCompletionHandler
  , subscribeAttributeGeneratedCommandListWithMinInterval_maxInterval_params_subscriptionEstablished_reportHandler
  , readAttributeGeneratedCommandListWithAttributeCache_endpoint_queue_completionHandler
  , readAttributeAcceptedCommandListWithCompletionHandler
  , subscribeAttributeAcceptedCommandListWithMinInterval_maxInterval_params_subscriptionEstablished_reportHandler
  , readAttributeAcceptedCommandListWithAttributeCache_endpoint_queue_completionHandler
  , readAttributeAttributeListWithCompletionHandler
  , subscribeAttributeAttributeListWithMinInterval_maxInterval_params_subscriptionEstablished_reportHandler
  , readAttributeAttributeListWithAttributeCache_endpoint_queue_completionHandler
  , readAttributeFeatureMapWithCompletionHandler
  , subscribeAttributeFeatureMapWithMinInterval_maxInterval_params_subscriptionEstablished_reportHandler
  , readAttributeFeatureMapWithAttributeCache_endpoint_queue_completionHandler
  , readAttributeClusterRevisionWithCompletionHandler
  , subscribeAttributeClusterRevisionWithMinInterval_maxInterval_params_subscriptionEstablished_reportHandler
  , readAttributeClusterRevisionWithAttributeCache_endpoint_queue_completionHandler
  , initWithDevice_endpointID_queue
  , initSelector
  , initWithDevice_endpointID_queueSelector
  , initWithDevice_endpoint_queueSelector
  , newSelector
  , readAttributeAcceptedCommandListWithAttributeCache_endpoint_queue_completionHandlerSelector
  , readAttributeAcceptedCommandListWithClusterStateCache_endpoint_queue_completionSelector
  , readAttributeAcceptedCommandListWithCompletionHandlerSelector
  , readAttributeAcceptedCommandListWithCompletionSelector
  , readAttributeAttributeListWithAttributeCache_endpoint_queue_completionHandlerSelector
  , readAttributeAttributeListWithClusterStateCache_endpoint_queue_completionSelector
  , readAttributeAttributeListWithCompletionHandlerSelector
  , readAttributeAttributeListWithCompletionSelector
  , readAttributeClusterRevisionWithAttributeCache_endpoint_queue_completionHandlerSelector
  , readAttributeClusterRevisionWithClusterStateCache_endpoint_queue_completionSelector
  , readAttributeClusterRevisionWithCompletionHandlerSelector
  , readAttributeClusterRevisionWithCompletionSelector
  , readAttributeCurrentPositionWithAttributeCache_endpoint_queue_completionHandlerSelector
  , readAttributeCurrentPositionWithClusterStateCache_endpoint_queue_completionSelector
  , readAttributeCurrentPositionWithCompletionHandlerSelector
  , readAttributeCurrentPositionWithCompletionSelector
  , readAttributeFeatureMapWithAttributeCache_endpoint_queue_completionHandlerSelector
  , readAttributeFeatureMapWithClusterStateCache_endpoint_queue_completionSelector
  , readAttributeFeatureMapWithCompletionHandlerSelector
  , readAttributeFeatureMapWithCompletionSelector
  , readAttributeGeneratedCommandListWithAttributeCache_endpoint_queue_completionHandlerSelector
  , readAttributeGeneratedCommandListWithClusterStateCache_endpoint_queue_completionSelector
  , readAttributeGeneratedCommandListWithCompletionHandlerSelector
  , readAttributeGeneratedCommandListWithCompletionSelector
  , readAttributeMultiPressMaxWithAttributeCache_endpoint_queue_completionHandlerSelector
  , readAttributeMultiPressMaxWithClusterStateCache_endpoint_queue_completionSelector
  , readAttributeMultiPressMaxWithCompletionHandlerSelector
  , readAttributeMultiPressMaxWithCompletionSelector
  , readAttributeNumberOfPositionsWithAttributeCache_endpoint_queue_completionHandlerSelector
  , readAttributeNumberOfPositionsWithClusterStateCache_endpoint_queue_completionSelector
  , readAttributeNumberOfPositionsWithCompletionHandlerSelector
  , readAttributeNumberOfPositionsWithCompletionSelector
  , subscribeAttributeAcceptedCommandListWithMinInterval_maxInterval_params_subscriptionEstablished_reportHandlerSelector
  , subscribeAttributeAcceptedCommandListWithParams_subscriptionEstablished_reportHandlerSelector
  , subscribeAttributeAttributeListWithMinInterval_maxInterval_params_subscriptionEstablished_reportHandlerSelector
  , subscribeAttributeAttributeListWithParams_subscriptionEstablished_reportHandlerSelector
  , subscribeAttributeClusterRevisionWithMinInterval_maxInterval_params_subscriptionEstablished_reportHandlerSelector
  , subscribeAttributeClusterRevisionWithParams_subscriptionEstablished_reportHandlerSelector
  , subscribeAttributeCurrentPositionWithMinInterval_maxInterval_params_subscriptionEstablished_reportHandlerSelector
  , subscribeAttributeCurrentPositionWithParams_subscriptionEstablished_reportHandlerSelector
  , subscribeAttributeFeatureMapWithMinInterval_maxInterval_params_subscriptionEstablished_reportHandlerSelector
  , subscribeAttributeFeatureMapWithParams_subscriptionEstablished_reportHandlerSelector
  , subscribeAttributeGeneratedCommandListWithMinInterval_maxInterval_params_subscriptionEstablished_reportHandlerSelector
  , subscribeAttributeGeneratedCommandListWithParams_subscriptionEstablished_reportHandlerSelector
  , subscribeAttributeMultiPressMaxWithMinInterval_maxInterval_params_subscriptionEstablished_reportHandlerSelector
  , subscribeAttributeMultiPressMaxWithParams_subscriptionEstablished_reportHandlerSelector
  , subscribeAttributeNumberOfPositionsWithMinInterval_maxInterval_params_subscriptionEstablished_reportHandlerSelector
  , subscribeAttributeNumberOfPositionsWithParams_subscriptionEstablished_reportHandlerSelector


  ) where

import Foreign.Ptr (Ptr, FunPtr)
import Foreign.C.Types

import ObjC.Runtime.Types
import ObjC.Runtime.Message (sendMessage, sendOwnedMessage, sendClassMessage, sendOwnedClassMessage)
import ObjC.Runtime.Selector (mkSelector)
import ObjC.Runtime.Class (getRequiredClass)

import ObjC.Matter.Internal.Classes
import ObjC.Foundation.Internal.Classes

-- | @- readAttributeNumberOfPositionsWithCompletion:@
readAttributeNumberOfPositionsWithCompletion :: IsMTRBaseClusterSwitch mtrBaseClusterSwitch => mtrBaseClusterSwitch -> Ptr () -> IO ()
readAttributeNumberOfPositionsWithCompletion mtrBaseClusterSwitch completion =
  sendMessage mtrBaseClusterSwitch readAttributeNumberOfPositionsWithCompletionSelector completion

-- | @- subscribeAttributeNumberOfPositionsWithParams:subscriptionEstablished:reportHandler:@
subscribeAttributeNumberOfPositionsWithParams_subscriptionEstablished_reportHandler :: (IsMTRBaseClusterSwitch mtrBaseClusterSwitch, IsMTRSubscribeParams params) => mtrBaseClusterSwitch -> params -> Ptr () -> Ptr () -> IO ()
subscribeAttributeNumberOfPositionsWithParams_subscriptionEstablished_reportHandler mtrBaseClusterSwitch params subscriptionEstablished reportHandler =
  sendMessage mtrBaseClusterSwitch subscribeAttributeNumberOfPositionsWithParams_subscriptionEstablished_reportHandlerSelector (toMTRSubscribeParams params) subscriptionEstablished reportHandler

-- | @+ readAttributeNumberOfPositionsWithClusterStateCache:endpoint:queue:completion:@
readAttributeNumberOfPositionsWithClusterStateCache_endpoint_queue_completion :: (IsMTRClusterStateCacheContainer clusterStateCacheContainer, IsNSNumber endpoint, IsNSObject queue) => clusterStateCacheContainer -> endpoint -> queue -> Ptr () -> IO ()
readAttributeNumberOfPositionsWithClusterStateCache_endpoint_queue_completion clusterStateCacheContainer endpoint queue completion =
  do
    cls' <- getRequiredClass "MTRBaseClusterSwitch"
    sendClassMessage cls' readAttributeNumberOfPositionsWithClusterStateCache_endpoint_queue_completionSelector (toMTRClusterStateCacheContainer clusterStateCacheContainer) (toNSNumber endpoint) (toNSObject queue) completion

-- | @- readAttributeCurrentPositionWithCompletion:@
readAttributeCurrentPositionWithCompletion :: IsMTRBaseClusterSwitch mtrBaseClusterSwitch => mtrBaseClusterSwitch -> Ptr () -> IO ()
readAttributeCurrentPositionWithCompletion mtrBaseClusterSwitch completion =
  sendMessage mtrBaseClusterSwitch readAttributeCurrentPositionWithCompletionSelector completion

-- | @- subscribeAttributeCurrentPositionWithParams:subscriptionEstablished:reportHandler:@
subscribeAttributeCurrentPositionWithParams_subscriptionEstablished_reportHandler :: (IsMTRBaseClusterSwitch mtrBaseClusterSwitch, IsMTRSubscribeParams params) => mtrBaseClusterSwitch -> params -> Ptr () -> Ptr () -> IO ()
subscribeAttributeCurrentPositionWithParams_subscriptionEstablished_reportHandler mtrBaseClusterSwitch params subscriptionEstablished reportHandler =
  sendMessage mtrBaseClusterSwitch subscribeAttributeCurrentPositionWithParams_subscriptionEstablished_reportHandlerSelector (toMTRSubscribeParams params) subscriptionEstablished reportHandler

-- | @+ readAttributeCurrentPositionWithClusterStateCache:endpoint:queue:completion:@
readAttributeCurrentPositionWithClusterStateCache_endpoint_queue_completion :: (IsMTRClusterStateCacheContainer clusterStateCacheContainer, IsNSNumber endpoint, IsNSObject queue) => clusterStateCacheContainer -> endpoint -> queue -> Ptr () -> IO ()
readAttributeCurrentPositionWithClusterStateCache_endpoint_queue_completion clusterStateCacheContainer endpoint queue completion =
  do
    cls' <- getRequiredClass "MTRBaseClusterSwitch"
    sendClassMessage cls' readAttributeCurrentPositionWithClusterStateCache_endpoint_queue_completionSelector (toMTRClusterStateCacheContainer clusterStateCacheContainer) (toNSNumber endpoint) (toNSObject queue) completion

-- | @- readAttributeMultiPressMaxWithCompletion:@
readAttributeMultiPressMaxWithCompletion :: IsMTRBaseClusterSwitch mtrBaseClusterSwitch => mtrBaseClusterSwitch -> Ptr () -> IO ()
readAttributeMultiPressMaxWithCompletion mtrBaseClusterSwitch completion =
  sendMessage mtrBaseClusterSwitch readAttributeMultiPressMaxWithCompletionSelector completion

-- | @- subscribeAttributeMultiPressMaxWithParams:subscriptionEstablished:reportHandler:@
subscribeAttributeMultiPressMaxWithParams_subscriptionEstablished_reportHandler :: (IsMTRBaseClusterSwitch mtrBaseClusterSwitch, IsMTRSubscribeParams params) => mtrBaseClusterSwitch -> params -> Ptr () -> Ptr () -> IO ()
subscribeAttributeMultiPressMaxWithParams_subscriptionEstablished_reportHandler mtrBaseClusterSwitch params subscriptionEstablished reportHandler =
  sendMessage mtrBaseClusterSwitch subscribeAttributeMultiPressMaxWithParams_subscriptionEstablished_reportHandlerSelector (toMTRSubscribeParams params) subscriptionEstablished reportHandler

-- | @+ readAttributeMultiPressMaxWithClusterStateCache:endpoint:queue:completion:@
readAttributeMultiPressMaxWithClusterStateCache_endpoint_queue_completion :: (IsMTRClusterStateCacheContainer clusterStateCacheContainer, IsNSNumber endpoint, IsNSObject queue) => clusterStateCacheContainer -> endpoint -> queue -> Ptr () -> IO ()
readAttributeMultiPressMaxWithClusterStateCache_endpoint_queue_completion clusterStateCacheContainer endpoint queue completion =
  do
    cls' <- getRequiredClass "MTRBaseClusterSwitch"
    sendClassMessage cls' readAttributeMultiPressMaxWithClusterStateCache_endpoint_queue_completionSelector (toMTRClusterStateCacheContainer clusterStateCacheContainer) (toNSNumber endpoint) (toNSObject queue) completion

-- | @- readAttributeGeneratedCommandListWithCompletion:@
readAttributeGeneratedCommandListWithCompletion :: IsMTRBaseClusterSwitch mtrBaseClusterSwitch => mtrBaseClusterSwitch -> Ptr () -> IO ()
readAttributeGeneratedCommandListWithCompletion mtrBaseClusterSwitch completion =
  sendMessage mtrBaseClusterSwitch readAttributeGeneratedCommandListWithCompletionSelector completion

-- | @- subscribeAttributeGeneratedCommandListWithParams:subscriptionEstablished:reportHandler:@
subscribeAttributeGeneratedCommandListWithParams_subscriptionEstablished_reportHandler :: (IsMTRBaseClusterSwitch mtrBaseClusterSwitch, IsMTRSubscribeParams params) => mtrBaseClusterSwitch -> params -> Ptr () -> Ptr () -> IO ()
subscribeAttributeGeneratedCommandListWithParams_subscriptionEstablished_reportHandler mtrBaseClusterSwitch params subscriptionEstablished reportHandler =
  sendMessage mtrBaseClusterSwitch subscribeAttributeGeneratedCommandListWithParams_subscriptionEstablished_reportHandlerSelector (toMTRSubscribeParams params) subscriptionEstablished reportHandler

-- | @+ readAttributeGeneratedCommandListWithClusterStateCache:endpoint:queue:completion:@
readAttributeGeneratedCommandListWithClusterStateCache_endpoint_queue_completion :: (IsMTRClusterStateCacheContainer clusterStateCacheContainer, IsNSNumber endpoint, IsNSObject queue) => clusterStateCacheContainer -> endpoint -> queue -> Ptr () -> IO ()
readAttributeGeneratedCommandListWithClusterStateCache_endpoint_queue_completion clusterStateCacheContainer endpoint queue completion =
  do
    cls' <- getRequiredClass "MTRBaseClusterSwitch"
    sendClassMessage cls' readAttributeGeneratedCommandListWithClusterStateCache_endpoint_queue_completionSelector (toMTRClusterStateCacheContainer clusterStateCacheContainer) (toNSNumber endpoint) (toNSObject queue) completion

-- | @- readAttributeAcceptedCommandListWithCompletion:@
readAttributeAcceptedCommandListWithCompletion :: IsMTRBaseClusterSwitch mtrBaseClusterSwitch => mtrBaseClusterSwitch -> Ptr () -> IO ()
readAttributeAcceptedCommandListWithCompletion mtrBaseClusterSwitch completion =
  sendMessage mtrBaseClusterSwitch readAttributeAcceptedCommandListWithCompletionSelector completion

-- | @- subscribeAttributeAcceptedCommandListWithParams:subscriptionEstablished:reportHandler:@
subscribeAttributeAcceptedCommandListWithParams_subscriptionEstablished_reportHandler :: (IsMTRBaseClusterSwitch mtrBaseClusterSwitch, IsMTRSubscribeParams params) => mtrBaseClusterSwitch -> params -> Ptr () -> Ptr () -> IO ()
subscribeAttributeAcceptedCommandListWithParams_subscriptionEstablished_reportHandler mtrBaseClusterSwitch params subscriptionEstablished reportHandler =
  sendMessage mtrBaseClusterSwitch subscribeAttributeAcceptedCommandListWithParams_subscriptionEstablished_reportHandlerSelector (toMTRSubscribeParams params) subscriptionEstablished reportHandler

-- | @+ readAttributeAcceptedCommandListWithClusterStateCache:endpoint:queue:completion:@
readAttributeAcceptedCommandListWithClusterStateCache_endpoint_queue_completion :: (IsMTRClusterStateCacheContainer clusterStateCacheContainer, IsNSNumber endpoint, IsNSObject queue) => clusterStateCacheContainer -> endpoint -> queue -> Ptr () -> IO ()
readAttributeAcceptedCommandListWithClusterStateCache_endpoint_queue_completion clusterStateCacheContainer endpoint queue completion =
  do
    cls' <- getRequiredClass "MTRBaseClusterSwitch"
    sendClassMessage cls' readAttributeAcceptedCommandListWithClusterStateCache_endpoint_queue_completionSelector (toMTRClusterStateCacheContainer clusterStateCacheContainer) (toNSNumber endpoint) (toNSObject queue) completion

-- | @- readAttributeAttributeListWithCompletion:@
readAttributeAttributeListWithCompletion :: IsMTRBaseClusterSwitch mtrBaseClusterSwitch => mtrBaseClusterSwitch -> Ptr () -> IO ()
readAttributeAttributeListWithCompletion mtrBaseClusterSwitch completion =
  sendMessage mtrBaseClusterSwitch readAttributeAttributeListWithCompletionSelector completion

-- | @- subscribeAttributeAttributeListWithParams:subscriptionEstablished:reportHandler:@
subscribeAttributeAttributeListWithParams_subscriptionEstablished_reportHandler :: (IsMTRBaseClusterSwitch mtrBaseClusterSwitch, IsMTRSubscribeParams params) => mtrBaseClusterSwitch -> params -> Ptr () -> Ptr () -> IO ()
subscribeAttributeAttributeListWithParams_subscriptionEstablished_reportHandler mtrBaseClusterSwitch params subscriptionEstablished reportHandler =
  sendMessage mtrBaseClusterSwitch subscribeAttributeAttributeListWithParams_subscriptionEstablished_reportHandlerSelector (toMTRSubscribeParams params) subscriptionEstablished reportHandler

-- | @+ readAttributeAttributeListWithClusterStateCache:endpoint:queue:completion:@
readAttributeAttributeListWithClusterStateCache_endpoint_queue_completion :: (IsMTRClusterStateCacheContainer clusterStateCacheContainer, IsNSNumber endpoint, IsNSObject queue) => clusterStateCacheContainer -> endpoint -> queue -> Ptr () -> IO ()
readAttributeAttributeListWithClusterStateCache_endpoint_queue_completion clusterStateCacheContainer endpoint queue completion =
  do
    cls' <- getRequiredClass "MTRBaseClusterSwitch"
    sendClassMessage cls' readAttributeAttributeListWithClusterStateCache_endpoint_queue_completionSelector (toMTRClusterStateCacheContainer clusterStateCacheContainer) (toNSNumber endpoint) (toNSObject queue) completion

-- | @- readAttributeFeatureMapWithCompletion:@
readAttributeFeatureMapWithCompletion :: IsMTRBaseClusterSwitch mtrBaseClusterSwitch => mtrBaseClusterSwitch -> Ptr () -> IO ()
readAttributeFeatureMapWithCompletion mtrBaseClusterSwitch completion =
  sendMessage mtrBaseClusterSwitch readAttributeFeatureMapWithCompletionSelector completion

-- | @- subscribeAttributeFeatureMapWithParams:subscriptionEstablished:reportHandler:@
subscribeAttributeFeatureMapWithParams_subscriptionEstablished_reportHandler :: (IsMTRBaseClusterSwitch mtrBaseClusterSwitch, IsMTRSubscribeParams params) => mtrBaseClusterSwitch -> params -> Ptr () -> Ptr () -> IO ()
subscribeAttributeFeatureMapWithParams_subscriptionEstablished_reportHandler mtrBaseClusterSwitch params subscriptionEstablished reportHandler =
  sendMessage mtrBaseClusterSwitch subscribeAttributeFeatureMapWithParams_subscriptionEstablished_reportHandlerSelector (toMTRSubscribeParams params) subscriptionEstablished reportHandler

-- | @+ readAttributeFeatureMapWithClusterStateCache:endpoint:queue:completion:@
readAttributeFeatureMapWithClusterStateCache_endpoint_queue_completion :: (IsMTRClusterStateCacheContainer clusterStateCacheContainer, IsNSNumber endpoint, IsNSObject queue) => clusterStateCacheContainer -> endpoint -> queue -> Ptr () -> IO ()
readAttributeFeatureMapWithClusterStateCache_endpoint_queue_completion clusterStateCacheContainer endpoint queue completion =
  do
    cls' <- getRequiredClass "MTRBaseClusterSwitch"
    sendClassMessage cls' readAttributeFeatureMapWithClusterStateCache_endpoint_queue_completionSelector (toMTRClusterStateCacheContainer clusterStateCacheContainer) (toNSNumber endpoint) (toNSObject queue) completion

-- | @- readAttributeClusterRevisionWithCompletion:@
readAttributeClusterRevisionWithCompletion :: IsMTRBaseClusterSwitch mtrBaseClusterSwitch => mtrBaseClusterSwitch -> Ptr () -> IO ()
readAttributeClusterRevisionWithCompletion mtrBaseClusterSwitch completion =
  sendMessage mtrBaseClusterSwitch readAttributeClusterRevisionWithCompletionSelector completion

-- | @- subscribeAttributeClusterRevisionWithParams:subscriptionEstablished:reportHandler:@
subscribeAttributeClusterRevisionWithParams_subscriptionEstablished_reportHandler :: (IsMTRBaseClusterSwitch mtrBaseClusterSwitch, IsMTRSubscribeParams params) => mtrBaseClusterSwitch -> params -> Ptr () -> Ptr () -> IO ()
subscribeAttributeClusterRevisionWithParams_subscriptionEstablished_reportHandler mtrBaseClusterSwitch params subscriptionEstablished reportHandler =
  sendMessage mtrBaseClusterSwitch subscribeAttributeClusterRevisionWithParams_subscriptionEstablished_reportHandlerSelector (toMTRSubscribeParams params) subscriptionEstablished reportHandler

-- | @+ readAttributeClusterRevisionWithClusterStateCache:endpoint:queue:completion:@
readAttributeClusterRevisionWithClusterStateCache_endpoint_queue_completion :: (IsMTRClusterStateCacheContainer clusterStateCacheContainer, IsNSNumber endpoint, IsNSObject queue) => clusterStateCacheContainer -> endpoint -> queue -> Ptr () -> IO ()
readAttributeClusterRevisionWithClusterStateCache_endpoint_queue_completion clusterStateCacheContainer endpoint queue completion =
  do
    cls' <- getRequiredClass "MTRBaseClusterSwitch"
    sendClassMessage cls' readAttributeClusterRevisionWithClusterStateCache_endpoint_queue_completionSelector (toMTRClusterStateCacheContainer clusterStateCacheContainer) (toNSNumber endpoint) (toNSObject queue) completion

-- | @- init@
init_ :: IsMTRBaseClusterSwitch mtrBaseClusterSwitch => mtrBaseClusterSwitch -> IO (Id MTRBaseClusterSwitch)
init_ mtrBaseClusterSwitch =
  sendOwnedMessage mtrBaseClusterSwitch initSelector

-- | @+ new@
new :: IO (Id MTRBaseClusterSwitch)
new  =
  do
    cls' <- getRequiredClass "MTRBaseClusterSwitch"
    sendOwnedClassMessage cls' newSelector

-- | @- initWithDevice:endpoint:queue:@
initWithDevice_endpoint_queue :: (IsMTRBaseClusterSwitch mtrBaseClusterSwitch, IsMTRBaseDevice device, IsNSObject queue) => mtrBaseClusterSwitch -> device -> CUShort -> queue -> IO (Id MTRBaseClusterSwitch)
initWithDevice_endpoint_queue mtrBaseClusterSwitch device endpoint queue =
  sendOwnedMessage mtrBaseClusterSwitch initWithDevice_endpoint_queueSelector (toMTRBaseDevice device) endpoint (toNSObject queue)

-- | @- readAttributeNumberOfPositionsWithCompletionHandler:@
readAttributeNumberOfPositionsWithCompletionHandler :: IsMTRBaseClusterSwitch mtrBaseClusterSwitch => mtrBaseClusterSwitch -> Ptr () -> IO ()
readAttributeNumberOfPositionsWithCompletionHandler mtrBaseClusterSwitch completionHandler =
  sendMessage mtrBaseClusterSwitch readAttributeNumberOfPositionsWithCompletionHandlerSelector completionHandler

-- | @- subscribeAttributeNumberOfPositionsWithMinInterval:maxInterval:params:subscriptionEstablished:reportHandler:@
subscribeAttributeNumberOfPositionsWithMinInterval_maxInterval_params_subscriptionEstablished_reportHandler :: (IsMTRBaseClusterSwitch mtrBaseClusterSwitch, IsNSNumber minInterval, IsNSNumber maxInterval, IsMTRSubscribeParams params) => mtrBaseClusterSwitch -> minInterval -> maxInterval -> params -> Ptr () -> Ptr () -> IO ()
subscribeAttributeNumberOfPositionsWithMinInterval_maxInterval_params_subscriptionEstablished_reportHandler mtrBaseClusterSwitch minInterval maxInterval params subscriptionEstablishedHandler reportHandler =
  sendMessage mtrBaseClusterSwitch subscribeAttributeNumberOfPositionsWithMinInterval_maxInterval_params_subscriptionEstablished_reportHandlerSelector (toNSNumber minInterval) (toNSNumber maxInterval) (toMTRSubscribeParams params) subscriptionEstablishedHandler reportHandler

-- | @+ readAttributeNumberOfPositionsWithAttributeCache:endpoint:queue:completionHandler:@
readAttributeNumberOfPositionsWithAttributeCache_endpoint_queue_completionHandler :: (IsMTRAttributeCacheContainer attributeCacheContainer, IsNSNumber endpoint, IsNSObject queue) => attributeCacheContainer -> endpoint -> queue -> Ptr () -> IO ()
readAttributeNumberOfPositionsWithAttributeCache_endpoint_queue_completionHandler attributeCacheContainer endpoint queue completionHandler =
  do
    cls' <- getRequiredClass "MTRBaseClusterSwitch"
    sendClassMessage cls' readAttributeNumberOfPositionsWithAttributeCache_endpoint_queue_completionHandlerSelector (toMTRAttributeCacheContainer attributeCacheContainer) (toNSNumber endpoint) (toNSObject queue) completionHandler

-- | @- readAttributeCurrentPositionWithCompletionHandler:@
readAttributeCurrentPositionWithCompletionHandler :: IsMTRBaseClusterSwitch mtrBaseClusterSwitch => mtrBaseClusterSwitch -> Ptr () -> IO ()
readAttributeCurrentPositionWithCompletionHandler mtrBaseClusterSwitch completionHandler =
  sendMessage mtrBaseClusterSwitch readAttributeCurrentPositionWithCompletionHandlerSelector completionHandler

-- | @- subscribeAttributeCurrentPositionWithMinInterval:maxInterval:params:subscriptionEstablished:reportHandler:@
subscribeAttributeCurrentPositionWithMinInterval_maxInterval_params_subscriptionEstablished_reportHandler :: (IsMTRBaseClusterSwitch mtrBaseClusterSwitch, IsNSNumber minInterval, IsNSNumber maxInterval, IsMTRSubscribeParams params) => mtrBaseClusterSwitch -> minInterval -> maxInterval -> params -> Ptr () -> Ptr () -> IO ()
subscribeAttributeCurrentPositionWithMinInterval_maxInterval_params_subscriptionEstablished_reportHandler mtrBaseClusterSwitch minInterval maxInterval params subscriptionEstablishedHandler reportHandler =
  sendMessage mtrBaseClusterSwitch subscribeAttributeCurrentPositionWithMinInterval_maxInterval_params_subscriptionEstablished_reportHandlerSelector (toNSNumber minInterval) (toNSNumber maxInterval) (toMTRSubscribeParams params) subscriptionEstablishedHandler reportHandler

-- | @+ readAttributeCurrentPositionWithAttributeCache:endpoint:queue:completionHandler:@
readAttributeCurrentPositionWithAttributeCache_endpoint_queue_completionHandler :: (IsMTRAttributeCacheContainer attributeCacheContainer, IsNSNumber endpoint, IsNSObject queue) => attributeCacheContainer -> endpoint -> queue -> Ptr () -> IO ()
readAttributeCurrentPositionWithAttributeCache_endpoint_queue_completionHandler attributeCacheContainer endpoint queue completionHandler =
  do
    cls' <- getRequiredClass "MTRBaseClusterSwitch"
    sendClassMessage cls' readAttributeCurrentPositionWithAttributeCache_endpoint_queue_completionHandlerSelector (toMTRAttributeCacheContainer attributeCacheContainer) (toNSNumber endpoint) (toNSObject queue) completionHandler

-- | @- readAttributeMultiPressMaxWithCompletionHandler:@
readAttributeMultiPressMaxWithCompletionHandler :: IsMTRBaseClusterSwitch mtrBaseClusterSwitch => mtrBaseClusterSwitch -> Ptr () -> IO ()
readAttributeMultiPressMaxWithCompletionHandler mtrBaseClusterSwitch completionHandler =
  sendMessage mtrBaseClusterSwitch readAttributeMultiPressMaxWithCompletionHandlerSelector completionHandler

-- | @- subscribeAttributeMultiPressMaxWithMinInterval:maxInterval:params:subscriptionEstablished:reportHandler:@
subscribeAttributeMultiPressMaxWithMinInterval_maxInterval_params_subscriptionEstablished_reportHandler :: (IsMTRBaseClusterSwitch mtrBaseClusterSwitch, IsNSNumber minInterval, IsNSNumber maxInterval, IsMTRSubscribeParams params) => mtrBaseClusterSwitch -> minInterval -> maxInterval -> params -> Ptr () -> Ptr () -> IO ()
subscribeAttributeMultiPressMaxWithMinInterval_maxInterval_params_subscriptionEstablished_reportHandler mtrBaseClusterSwitch minInterval maxInterval params subscriptionEstablishedHandler reportHandler =
  sendMessage mtrBaseClusterSwitch subscribeAttributeMultiPressMaxWithMinInterval_maxInterval_params_subscriptionEstablished_reportHandlerSelector (toNSNumber minInterval) (toNSNumber maxInterval) (toMTRSubscribeParams params) subscriptionEstablishedHandler reportHandler

-- | @+ readAttributeMultiPressMaxWithAttributeCache:endpoint:queue:completionHandler:@
readAttributeMultiPressMaxWithAttributeCache_endpoint_queue_completionHandler :: (IsMTRAttributeCacheContainer attributeCacheContainer, IsNSNumber endpoint, IsNSObject queue) => attributeCacheContainer -> endpoint -> queue -> Ptr () -> IO ()
readAttributeMultiPressMaxWithAttributeCache_endpoint_queue_completionHandler attributeCacheContainer endpoint queue completionHandler =
  do
    cls' <- getRequiredClass "MTRBaseClusterSwitch"
    sendClassMessage cls' readAttributeMultiPressMaxWithAttributeCache_endpoint_queue_completionHandlerSelector (toMTRAttributeCacheContainer attributeCacheContainer) (toNSNumber endpoint) (toNSObject queue) completionHandler

-- | @- readAttributeGeneratedCommandListWithCompletionHandler:@
readAttributeGeneratedCommandListWithCompletionHandler :: IsMTRBaseClusterSwitch mtrBaseClusterSwitch => mtrBaseClusterSwitch -> Ptr () -> IO ()
readAttributeGeneratedCommandListWithCompletionHandler mtrBaseClusterSwitch completionHandler =
  sendMessage mtrBaseClusterSwitch readAttributeGeneratedCommandListWithCompletionHandlerSelector completionHandler

-- | @- subscribeAttributeGeneratedCommandListWithMinInterval:maxInterval:params:subscriptionEstablished:reportHandler:@
subscribeAttributeGeneratedCommandListWithMinInterval_maxInterval_params_subscriptionEstablished_reportHandler :: (IsMTRBaseClusterSwitch mtrBaseClusterSwitch, IsNSNumber minInterval, IsNSNumber maxInterval, IsMTRSubscribeParams params) => mtrBaseClusterSwitch -> minInterval -> maxInterval -> params -> Ptr () -> Ptr () -> IO ()
subscribeAttributeGeneratedCommandListWithMinInterval_maxInterval_params_subscriptionEstablished_reportHandler mtrBaseClusterSwitch minInterval maxInterval params subscriptionEstablishedHandler reportHandler =
  sendMessage mtrBaseClusterSwitch subscribeAttributeGeneratedCommandListWithMinInterval_maxInterval_params_subscriptionEstablished_reportHandlerSelector (toNSNumber minInterval) (toNSNumber maxInterval) (toMTRSubscribeParams params) subscriptionEstablishedHandler reportHandler

-- | @+ readAttributeGeneratedCommandListWithAttributeCache:endpoint:queue:completionHandler:@
readAttributeGeneratedCommandListWithAttributeCache_endpoint_queue_completionHandler :: (IsMTRAttributeCacheContainer attributeCacheContainer, IsNSNumber endpoint, IsNSObject queue) => attributeCacheContainer -> endpoint -> queue -> Ptr () -> IO ()
readAttributeGeneratedCommandListWithAttributeCache_endpoint_queue_completionHandler attributeCacheContainer endpoint queue completionHandler =
  do
    cls' <- getRequiredClass "MTRBaseClusterSwitch"
    sendClassMessage cls' readAttributeGeneratedCommandListWithAttributeCache_endpoint_queue_completionHandlerSelector (toMTRAttributeCacheContainer attributeCacheContainer) (toNSNumber endpoint) (toNSObject queue) completionHandler

-- | @- readAttributeAcceptedCommandListWithCompletionHandler:@
readAttributeAcceptedCommandListWithCompletionHandler :: IsMTRBaseClusterSwitch mtrBaseClusterSwitch => mtrBaseClusterSwitch -> Ptr () -> IO ()
readAttributeAcceptedCommandListWithCompletionHandler mtrBaseClusterSwitch completionHandler =
  sendMessage mtrBaseClusterSwitch readAttributeAcceptedCommandListWithCompletionHandlerSelector completionHandler

-- | @- subscribeAttributeAcceptedCommandListWithMinInterval:maxInterval:params:subscriptionEstablished:reportHandler:@
subscribeAttributeAcceptedCommandListWithMinInterval_maxInterval_params_subscriptionEstablished_reportHandler :: (IsMTRBaseClusterSwitch mtrBaseClusterSwitch, IsNSNumber minInterval, IsNSNumber maxInterval, IsMTRSubscribeParams params) => mtrBaseClusterSwitch -> minInterval -> maxInterval -> params -> Ptr () -> Ptr () -> IO ()
subscribeAttributeAcceptedCommandListWithMinInterval_maxInterval_params_subscriptionEstablished_reportHandler mtrBaseClusterSwitch minInterval maxInterval params subscriptionEstablishedHandler reportHandler =
  sendMessage mtrBaseClusterSwitch subscribeAttributeAcceptedCommandListWithMinInterval_maxInterval_params_subscriptionEstablished_reportHandlerSelector (toNSNumber minInterval) (toNSNumber maxInterval) (toMTRSubscribeParams params) subscriptionEstablishedHandler reportHandler

-- | @+ readAttributeAcceptedCommandListWithAttributeCache:endpoint:queue:completionHandler:@
readAttributeAcceptedCommandListWithAttributeCache_endpoint_queue_completionHandler :: (IsMTRAttributeCacheContainer attributeCacheContainer, IsNSNumber endpoint, IsNSObject queue) => attributeCacheContainer -> endpoint -> queue -> Ptr () -> IO ()
readAttributeAcceptedCommandListWithAttributeCache_endpoint_queue_completionHandler attributeCacheContainer endpoint queue completionHandler =
  do
    cls' <- getRequiredClass "MTRBaseClusterSwitch"
    sendClassMessage cls' readAttributeAcceptedCommandListWithAttributeCache_endpoint_queue_completionHandlerSelector (toMTRAttributeCacheContainer attributeCacheContainer) (toNSNumber endpoint) (toNSObject queue) completionHandler

-- | @- readAttributeAttributeListWithCompletionHandler:@
readAttributeAttributeListWithCompletionHandler :: IsMTRBaseClusterSwitch mtrBaseClusterSwitch => mtrBaseClusterSwitch -> Ptr () -> IO ()
readAttributeAttributeListWithCompletionHandler mtrBaseClusterSwitch completionHandler =
  sendMessage mtrBaseClusterSwitch readAttributeAttributeListWithCompletionHandlerSelector completionHandler

-- | @- subscribeAttributeAttributeListWithMinInterval:maxInterval:params:subscriptionEstablished:reportHandler:@
subscribeAttributeAttributeListWithMinInterval_maxInterval_params_subscriptionEstablished_reportHandler :: (IsMTRBaseClusterSwitch mtrBaseClusterSwitch, IsNSNumber minInterval, IsNSNumber maxInterval, IsMTRSubscribeParams params) => mtrBaseClusterSwitch -> minInterval -> maxInterval -> params -> Ptr () -> Ptr () -> IO ()
subscribeAttributeAttributeListWithMinInterval_maxInterval_params_subscriptionEstablished_reportHandler mtrBaseClusterSwitch minInterval maxInterval params subscriptionEstablishedHandler reportHandler =
  sendMessage mtrBaseClusterSwitch subscribeAttributeAttributeListWithMinInterval_maxInterval_params_subscriptionEstablished_reportHandlerSelector (toNSNumber minInterval) (toNSNumber maxInterval) (toMTRSubscribeParams params) subscriptionEstablishedHandler reportHandler

-- | @+ readAttributeAttributeListWithAttributeCache:endpoint:queue:completionHandler:@
readAttributeAttributeListWithAttributeCache_endpoint_queue_completionHandler :: (IsMTRAttributeCacheContainer attributeCacheContainer, IsNSNumber endpoint, IsNSObject queue) => attributeCacheContainer -> endpoint -> queue -> Ptr () -> IO ()
readAttributeAttributeListWithAttributeCache_endpoint_queue_completionHandler attributeCacheContainer endpoint queue completionHandler =
  do
    cls' <- getRequiredClass "MTRBaseClusterSwitch"
    sendClassMessage cls' readAttributeAttributeListWithAttributeCache_endpoint_queue_completionHandlerSelector (toMTRAttributeCacheContainer attributeCacheContainer) (toNSNumber endpoint) (toNSObject queue) completionHandler

-- | @- readAttributeFeatureMapWithCompletionHandler:@
readAttributeFeatureMapWithCompletionHandler :: IsMTRBaseClusterSwitch mtrBaseClusterSwitch => mtrBaseClusterSwitch -> Ptr () -> IO ()
readAttributeFeatureMapWithCompletionHandler mtrBaseClusterSwitch completionHandler =
  sendMessage mtrBaseClusterSwitch readAttributeFeatureMapWithCompletionHandlerSelector completionHandler

-- | @- subscribeAttributeFeatureMapWithMinInterval:maxInterval:params:subscriptionEstablished:reportHandler:@
subscribeAttributeFeatureMapWithMinInterval_maxInterval_params_subscriptionEstablished_reportHandler :: (IsMTRBaseClusterSwitch mtrBaseClusterSwitch, IsNSNumber minInterval, IsNSNumber maxInterval, IsMTRSubscribeParams params) => mtrBaseClusterSwitch -> minInterval -> maxInterval -> params -> Ptr () -> Ptr () -> IO ()
subscribeAttributeFeatureMapWithMinInterval_maxInterval_params_subscriptionEstablished_reportHandler mtrBaseClusterSwitch minInterval maxInterval params subscriptionEstablishedHandler reportHandler =
  sendMessage mtrBaseClusterSwitch subscribeAttributeFeatureMapWithMinInterval_maxInterval_params_subscriptionEstablished_reportHandlerSelector (toNSNumber minInterval) (toNSNumber maxInterval) (toMTRSubscribeParams params) subscriptionEstablishedHandler reportHandler

-- | @+ readAttributeFeatureMapWithAttributeCache:endpoint:queue:completionHandler:@
readAttributeFeatureMapWithAttributeCache_endpoint_queue_completionHandler :: (IsMTRAttributeCacheContainer attributeCacheContainer, IsNSNumber endpoint, IsNSObject queue) => attributeCacheContainer -> endpoint -> queue -> Ptr () -> IO ()
readAttributeFeatureMapWithAttributeCache_endpoint_queue_completionHandler attributeCacheContainer endpoint queue completionHandler =
  do
    cls' <- getRequiredClass "MTRBaseClusterSwitch"
    sendClassMessage cls' readAttributeFeatureMapWithAttributeCache_endpoint_queue_completionHandlerSelector (toMTRAttributeCacheContainer attributeCacheContainer) (toNSNumber endpoint) (toNSObject queue) completionHandler

-- | @- readAttributeClusterRevisionWithCompletionHandler:@
readAttributeClusterRevisionWithCompletionHandler :: IsMTRBaseClusterSwitch mtrBaseClusterSwitch => mtrBaseClusterSwitch -> Ptr () -> IO ()
readAttributeClusterRevisionWithCompletionHandler mtrBaseClusterSwitch completionHandler =
  sendMessage mtrBaseClusterSwitch readAttributeClusterRevisionWithCompletionHandlerSelector completionHandler

-- | @- subscribeAttributeClusterRevisionWithMinInterval:maxInterval:params:subscriptionEstablished:reportHandler:@
subscribeAttributeClusterRevisionWithMinInterval_maxInterval_params_subscriptionEstablished_reportHandler :: (IsMTRBaseClusterSwitch mtrBaseClusterSwitch, IsNSNumber minInterval, IsNSNumber maxInterval, IsMTRSubscribeParams params) => mtrBaseClusterSwitch -> minInterval -> maxInterval -> params -> Ptr () -> Ptr () -> IO ()
subscribeAttributeClusterRevisionWithMinInterval_maxInterval_params_subscriptionEstablished_reportHandler mtrBaseClusterSwitch minInterval maxInterval params subscriptionEstablishedHandler reportHandler =
  sendMessage mtrBaseClusterSwitch subscribeAttributeClusterRevisionWithMinInterval_maxInterval_params_subscriptionEstablished_reportHandlerSelector (toNSNumber minInterval) (toNSNumber maxInterval) (toMTRSubscribeParams params) subscriptionEstablishedHandler reportHandler

-- | @+ readAttributeClusterRevisionWithAttributeCache:endpoint:queue:completionHandler:@
readAttributeClusterRevisionWithAttributeCache_endpoint_queue_completionHandler :: (IsMTRAttributeCacheContainer attributeCacheContainer, IsNSNumber endpoint, IsNSObject queue) => attributeCacheContainer -> endpoint -> queue -> Ptr () -> IO ()
readAttributeClusterRevisionWithAttributeCache_endpoint_queue_completionHandler attributeCacheContainer endpoint queue completionHandler =
  do
    cls' <- getRequiredClass "MTRBaseClusterSwitch"
    sendClassMessage cls' readAttributeClusterRevisionWithAttributeCache_endpoint_queue_completionHandlerSelector (toMTRAttributeCacheContainer attributeCacheContainer) (toNSNumber endpoint) (toNSObject queue) completionHandler

-- | For all instance methods (reads, writes, commands) that take a completion, the completion will be called on the provided queue.
--
-- ObjC selector: @- initWithDevice:endpointID:queue:@
initWithDevice_endpointID_queue :: (IsMTRBaseClusterSwitch mtrBaseClusterSwitch, IsMTRBaseDevice device, IsNSNumber endpointID, IsNSObject queue) => mtrBaseClusterSwitch -> device -> endpointID -> queue -> IO (Id MTRBaseClusterSwitch)
initWithDevice_endpointID_queue mtrBaseClusterSwitch device endpointID queue =
  sendOwnedMessage mtrBaseClusterSwitch initWithDevice_endpointID_queueSelector (toMTRBaseDevice device) (toNSNumber endpointID) (toNSObject queue)

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @readAttributeNumberOfPositionsWithCompletion:@
readAttributeNumberOfPositionsWithCompletionSelector :: Selector '[Ptr ()] ()
readAttributeNumberOfPositionsWithCompletionSelector = mkSelector "readAttributeNumberOfPositionsWithCompletion:"

-- | @Selector@ for @subscribeAttributeNumberOfPositionsWithParams:subscriptionEstablished:reportHandler:@
subscribeAttributeNumberOfPositionsWithParams_subscriptionEstablished_reportHandlerSelector :: Selector '[Id MTRSubscribeParams, Ptr (), Ptr ()] ()
subscribeAttributeNumberOfPositionsWithParams_subscriptionEstablished_reportHandlerSelector = mkSelector "subscribeAttributeNumberOfPositionsWithParams:subscriptionEstablished:reportHandler:"

-- | @Selector@ for @readAttributeNumberOfPositionsWithClusterStateCache:endpoint:queue:completion:@
readAttributeNumberOfPositionsWithClusterStateCache_endpoint_queue_completionSelector :: Selector '[Id MTRClusterStateCacheContainer, Id NSNumber, Id NSObject, Ptr ()] ()
readAttributeNumberOfPositionsWithClusterStateCache_endpoint_queue_completionSelector = mkSelector "readAttributeNumberOfPositionsWithClusterStateCache:endpoint:queue:completion:"

-- | @Selector@ for @readAttributeCurrentPositionWithCompletion:@
readAttributeCurrentPositionWithCompletionSelector :: Selector '[Ptr ()] ()
readAttributeCurrentPositionWithCompletionSelector = mkSelector "readAttributeCurrentPositionWithCompletion:"

-- | @Selector@ for @subscribeAttributeCurrentPositionWithParams:subscriptionEstablished:reportHandler:@
subscribeAttributeCurrentPositionWithParams_subscriptionEstablished_reportHandlerSelector :: Selector '[Id MTRSubscribeParams, Ptr (), Ptr ()] ()
subscribeAttributeCurrentPositionWithParams_subscriptionEstablished_reportHandlerSelector = mkSelector "subscribeAttributeCurrentPositionWithParams:subscriptionEstablished:reportHandler:"

-- | @Selector@ for @readAttributeCurrentPositionWithClusterStateCache:endpoint:queue:completion:@
readAttributeCurrentPositionWithClusterStateCache_endpoint_queue_completionSelector :: Selector '[Id MTRClusterStateCacheContainer, Id NSNumber, Id NSObject, Ptr ()] ()
readAttributeCurrentPositionWithClusterStateCache_endpoint_queue_completionSelector = mkSelector "readAttributeCurrentPositionWithClusterStateCache:endpoint:queue:completion:"

-- | @Selector@ for @readAttributeMultiPressMaxWithCompletion:@
readAttributeMultiPressMaxWithCompletionSelector :: Selector '[Ptr ()] ()
readAttributeMultiPressMaxWithCompletionSelector = mkSelector "readAttributeMultiPressMaxWithCompletion:"

-- | @Selector@ for @subscribeAttributeMultiPressMaxWithParams:subscriptionEstablished:reportHandler:@
subscribeAttributeMultiPressMaxWithParams_subscriptionEstablished_reportHandlerSelector :: Selector '[Id MTRSubscribeParams, Ptr (), Ptr ()] ()
subscribeAttributeMultiPressMaxWithParams_subscriptionEstablished_reportHandlerSelector = mkSelector "subscribeAttributeMultiPressMaxWithParams:subscriptionEstablished:reportHandler:"

-- | @Selector@ for @readAttributeMultiPressMaxWithClusterStateCache:endpoint:queue:completion:@
readAttributeMultiPressMaxWithClusterStateCache_endpoint_queue_completionSelector :: Selector '[Id MTRClusterStateCacheContainer, Id NSNumber, Id NSObject, Ptr ()] ()
readAttributeMultiPressMaxWithClusterStateCache_endpoint_queue_completionSelector = mkSelector "readAttributeMultiPressMaxWithClusterStateCache:endpoint:queue:completion:"

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
initSelector :: Selector '[] (Id MTRBaseClusterSwitch)
initSelector = mkSelector "init"

-- | @Selector@ for @new@
newSelector :: Selector '[] (Id MTRBaseClusterSwitch)
newSelector = mkSelector "new"

-- | @Selector@ for @initWithDevice:endpoint:queue:@
initWithDevice_endpoint_queueSelector :: Selector '[Id MTRBaseDevice, CUShort, Id NSObject] (Id MTRBaseClusterSwitch)
initWithDevice_endpoint_queueSelector = mkSelector "initWithDevice:endpoint:queue:"

-- | @Selector@ for @readAttributeNumberOfPositionsWithCompletionHandler:@
readAttributeNumberOfPositionsWithCompletionHandlerSelector :: Selector '[Ptr ()] ()
readAttributeNumberOfPositionsWithCompletionHandlerSelector = mkSelector "readAttributeNumberOfPositionsWithCompletionHandler:"

-- | @Selector@ for @subscribeAttributeNumberOfPositionsWithMinInterval:maxInterval:params:subscriptionEstablished:reportHandler:@
subscribeAttributeNumberOfPositionsWithMinInterval_maxInterval_params_subscriptionEstablished_reportHandlerSelector :: Selector '[Id NSNumber, Id NSNumber, Id MTRSubscribeParams, Ptr (), Ptr ()] ()
subscribeAttributeNumberOfPositionsWithMinInterval_maxInterval_params_subscriptionEstablished_reportHandlerSelector = mkSelector "subscribeAttributeNumberOfPositionsWithMinInterval:maxInterval:params:subscriptionEstablished:reportHandler:"

-- | @Selector@ for @readAttributeNumberOfPositionsWithAttributeCache:endpoint:queue:completionHandler:@
readAttributeNumberOfPositionsWithAttributeCache_endpoint_queue_completionHandlerSelector :: Selector '[Id MTRAttributeCacheContainer, Id NSNumber, Id NSObject, Ptr ()] ()
readAttributeNumberOfPositionsWithAttributeCache_endpoint_queue_completionHandlerSelector = mkSelector "readAttributeNumberOfPositionsWithAttributeCache:endpoint:queue:completionHandler:"

-- | @Selector@ for @readAttributeCurrentPositionWithCompletionHandler:@
readAttributeCurrentPositionWithCompletionHandlerSelector :: Selector '[Ptr ()] ()
readAttributeCurrentPositionWithCompletionHandlerSelector = mkSelector "readAttributeCurrentPositionWithCompletionHandler:"

-- | @Selector@ for @subscribeAttributeCurrentPositionWithMinInterval:maxInterval:params:subscriptionEstablished:reportHandler:@
subscribeAttributeCurrentPositionWithMinInterval_maxInterval_params_subscriptionEstablished_reportHandlerSelector :: Selector '[Id NSNumber, Id NSNumber, Id MTRSubscribeParams, Ptr (), Ptr ()] ()
subscribeAttributeCurrentPositionWithMinInterval_maxInterval_params_subscriptionEstablished_reportHandlerSelector = mkSelector "subscribeAttributeCurrentPositionWithMinInterval:maxInterval:params:subscriptionEstablished:reportHandler:"

-- | @Selector@ for @readAttributeCurrentPositionWithAttributeCache:endpoint:queue:completionHandler:@
readAttributeCurrentPositionWithAttributeCache_endpoint_queue_completionHandlerSelector :: Selector '[Id MTRAttributeCacheContainer, Id NSNumber, Id NSObject, Ptr ()] ()
readAttributeCurrentPositionWithAttributeCache_endpoint_queue_completionHandlerSelector = mkSelector "readAttributeCurrentPositionWithAttributeCache:endpoint:queue:completionHandler:"

-- | @Selector@ for @readAttributeMultiPressMaxWithCompletionHandler:@
readAttributeMultiPressMaxWithCompletionHandlerSelector :: Selector '[Ptr ()] ()
readAttributeMultiPressMaxWithCompletionHandlerSelector = mkSelector "readAttributeMultiPressMaxWithCompletionHandler:"

-- | @Selector@ for @subscribeAttributeMultiPressMaxWithMinInterval:maxInterval:params:subscriptionEstablished:reportHandler:@
subscribeAttributeMultiPressMaxWithMinInterval_maxInterval_params_subscriptionEstablished_reportHandlerSelector :: Selector '[Id NSNumber, Id NSNumber, Id MTRSubscribeParams, Ptr (), Ptr ()] ()
subscribeAttributeMultiPressMaxWithMinInterval_maxInterval_params_subscriptionEstablished_reportHandlerSelector = mkSelector "subscribeAttributeMultiPressMaxWithMinInterval:maxInterval:params:subscriptionEstablished:reportHandler:"

-- | @Selector@ for @readAttributeMultiPressMaxWithAttributeCache:endpoint:queue:completionHandler:@
readAttributeMultiPressMaxWithAttributeCache_endpoint_queue_completionHandlerSelector :: Selector '[Id MTRAttributeCacheContainer, Id NSNumber, Id NSObject, Ptr ()] ()
readAttributeMultiPressMaxWithAttributeCache_endpoint_queue_completionHandlerSelector = mkSelector "readAttributeMultiPressMaxWithAttributeCache:endpoint:queue:completionHandler:"

-- | @Selector@ for @readAttributeGeneratedCommandListWithCompletionHandler:@
readAttributeGeneratedCommandListWithCompletionHandlerSelector :: Selector '[Ptr ()] ()
readAttributeGeneratedCommandListWithCompletionHandlerSelector = mkSelector "readAttributeGeneratedCommandListWithCompletionHandler:"

-- | @Selector@ for @subscribeAttributeGeneratedCommandListWithMinInterval:maxInterval:params:subscriptionEstablished:reportHandler:@
subscribeAttributeGeneratedCommandListWithMinInterval_maxInterval_params_subscriptionEstablished_reportHandlerSelector :: Selector '[Id NSNumber, Id NSNumber, Id MTRSubscribeParams, Ptr (), Ptr ()] ()
subscribeAttributeGeneratedCommandListWithMinInterval_maxInterval_params_subscriptionEstablished_reportHandlerSelector = mkSelector "subscribeAttributeGeneratedCommandListWithMinInterval:maxInterval:params:subscriptionEstablished:reportHandler:"

-- | @Selector@ for @readAttributeGeneratedCommandListWithAttributeCache:endpoint:queue:completionHandler:@
readAttributeGeneratedCommandListWithAttributeCache_endpoint_queue_completionHandlerSelector :: Selector '[Id MTRAttributeCacheContainer, Id NSNumber, Id NSObject, Ptr ()] ()
readAttributeGeneratedCommandListWithAttributeCache_endpoint_queue_completionHandlerSelector = mkSelector "readAttributeGeneratedCommandListWithAttributeCache:endpoint:queue:completionHandler:"

-- | @Selector@ for @readAttributeAcceptedCommandListWithCompletionHandler:@
readAttributeAcceptedCommandListWithCompletionHandlerSelector :: Selector '[Ptr ()] ()
readAttributeAcceptedCommandListWithCompletionHandlerSelector = mkSelector "readAttributeAcceptedCommandListWithCompletionHandler:"

-- | @Selector@ for @subscribeAttributeAcceptedCommandListWithMinInterval:maxInterval:params:subscriptionEstablished:reportHandler:@
subscribeAttributeAcceptedCommandListWithMinInterval_maxInterval_params_subscriptionEstablished_reportHandlerSelector :: Selector '[Id NSNumber, Id NSNumber, Id MTRSubscribeParams, Ptr (), Ptr ()] ()
subscribeAttributeAcceptedCommandListWithMinInterval_maxInterval_params_subscriptionEstablished_reportHandlerSelector = mkSelector "subscribeAttributeAcceptedCommandListWithMinInterval:maxInterval:params:subscriptionEstablished:reportHandler:"

-- | @Selector@ for @readAttributeAcceptedCommandListWithAttributeCache:endpoint:queue:completionHandler:@
readAttributeAcceptedCommandListWithAttributeCache_endpoint_queue_completionHandlerSelector :: Selector '[Id MTRAttributeCacheContainer, Id NSNumber, Id NSObject, Ptr ()] ()
readAttributeAcceptedCommandListWithAttributeCache_endpoint_queue_completionHandlerSelector = mkSelector "readAttributeAcceptedCommandListWithAttributeCache:endpoint:queue:completionHandler:"

-- | @Selector@ for @readAttributeAttributeListWithCompletionHandler:@
readAttributeAttributeListWithCompletionHandlerSelector :: Selector '[Ptr ()] ()
readAttributeAttributeListWithCompletionHandlerSelector = mkSelector "readAttributeAttributeListWithCompletionHandler:"

-- | @Selector@ for @subscribeAttributeAttributeListWithMinInterval:maxInterval:params:subscriptionEstablished:reportHandler:@
subscribeAttributeAttributeListWithMinInterval_maxInterval_params_subscriptionEstablished_reportHandlerSelector :: Selector '[Id NSNumber, Id NSNumber, Id MTRSubscribeParams, Ptr (), Ptr ()] ()
subscribeAttributeAttributeListWithMinInterval_maxInterval_params_subscriptionEstablished_reportHandlerSelector = mkSelector "subscribeAttributeAttributeListWithMinInterval:maxInterval:params:subscriptionEstablished:reportHandler:"

-- | @Selector@ for @readAttributeAttributeListWithAttributeCache:endpoint:queue:completionHandler:@
readAttributeAttributeListWithAttributeCache_endpoint_queue_completionHandlerSelector :: Selector '[Id MTRAttributeCacheContainer, Id NSNumber, Id NSObject, Ptr ()] ()
readAttributeAttributeListWithAttributeCache_endpoint_queue_completionHandlerSelector = mkSelector "readAttributeAttributeListWithAttributeCache:endpoint:queue:completionHandler:"

-- | @Selector@ for @readAttributeFeatureMapWithCompletionHandler:@
readAttributeFeatureMapWithCompletionHandlerSelector :: Selector '[Ptr ()] ()
readAttributeFeatureMapWithCompletionHandlerSelector = mkSelector "readAttributeFeatureMapWithCompletionHandler:"

-- | @Selector@ for @subscribeAttributeFeatureMapWithMinInterval:maxInterval:params:subscriptionEstablished:reportHandler:@
subscribeAttributeFeatureMapWithMinInterval_maxInterval_params_subscriptionEstablished_reportHandlerSelector :: Selector '[Id NSNumber, Id NSNumber, Id MTRSubscribeParams, Ptr (), Ptr ()] ()
subscribeAttributeFeatureMapWithMinInterval_maxInterval_params_subscriptionEstablished_reportHandlerSelector = mkSelector "subscribeAttributeFeatureMapWithMinInterval:maxInterval:params:subscriptionEstablished:reportHandler:"

-- | @Selector@ for @readAttributeFeatureMapWithAttributeCache:endpoint:queue:completionHandler:@
readAttributeFeatureMapWithAttributeCache_endpoint_queue_completionHandlerSelector :: Selector '[Id MTRAttributeCacheContainer, Id NSNumber, Id NSObject, Ptr ()] ()
readAttributeFeatureMapWithAttributeCache_endpoint_queue_completionHandlerSelector = mkSelector "readAttributeFeatureMapWithAttributeCache:endpoint:queue:completionHandler:"

-- | @Selector@ for @readAttributeClusterRevisionWithCompletionHandler:@
readAttributeClusterRevisionWithCompletionHandlerSelector :: Selector '[Ptr ()] ()
readAttributeClusterRevisionWithCompletionHandlerSelector = mkSelector "readAttributeClusterRevisionWithCompletionHandler:"

-- | @Selector@ for @subscribeAttributeClusterRevisionWithMinInterval:maxInterval:params:subscriptionEstablished:reportHandler:@
subscribeAttributeClusterRevisionWithMinInterval_maxInterval_params_subscriptionEstablished_reportHandlerSelector :: Selector '[Id NSNumber, Id NSNumber, Id MTRSubscribeParams, Ptr (), Ptr ()] ()
subscribeAttributeClusterRevisionWithMinInterval_maxInterval_params_subscriptionEstablished_reportHandlerSelector = mkSelector "subscribeAttributeClusterRevisionWithMinInterval:maxInterval:params:subscriptionEstablished:reportHandler:"

-- | @Selector@ for @readAttributeClusterRevisionWithAttributeCache:endpoint:queue:completionHandler:@
readAttributeClusterRevisionWithAttributeCache_endpoint_queue_completionHandlerSelector :: Selector '[Id MTRAttributeCacheContainer, Id NSNumber, Id NSObject, Ptr ()] ()
readAttributeClusterRevisionWithAttributeCache_endpoint_queue_completionHandlerSelector = mkSelector "readAttributeClusterRevisionWithAttributeCache:endpoint:queue:completionHandler:"

-- | @Selector@ for @initWithDevice:endpointID:queue:@
initWithDevice_endpointID_queueSelector :: Selector '[Id MTRBaseDevice, Id NSNumber, Id NSObject] (Id MTRBaseClusterSwitch)
initWithDevice_endpointID_queueSelector = mkSelector "initWithDevice:endpointID:queue:"

