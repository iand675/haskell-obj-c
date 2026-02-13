{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Cluster Identify
--
-- Attributes and commands for putting a device into Identification mode (e.g. flashing a light).
--
-- Generated bindings for @MTRBaseClusterIdentify@.
module ObjC.Matter.MTRBaseClusterIdentify
  ( MTRBaseClusterIdentify
  , IsMTRBaseClusterIdentify(..)
  , identifyWithParams_completion
  , triggerEffectWithParams_completion
  , readAttributeIdentifyTimeWithCompletion
  , writeAttributeIdentifyTimeWithValue_completion
  , writeAttributeIdentifyTimeWithValue_params_completion
  , subscribeAttributeIdentifyTimeWithParams_subscriptionEstablished_reportHandler
  , readAttributeIdentifyTimeWithClusterStateCache_endpoint_queue_completion
  , readAttributeIdentifyTypeWithCompletion
  , subscribeAttributeIdentifyTypeWithParams_subscriptionEstablished_reportHandler
  , readAttributeIdentifyTypeWithClusterStateCache_endpoint_queue_completion
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
  , identifyWithParams_completionHandler
  , triggerEffectWithParams_completionHandler
  , readAttributeIdentifyTimeWithCompletionHandler
  , writeAttributeIdentifyTimeWithValue_completionHandler
  , writeAttributeIdentifyTimeWithValue_params_completionHandler
  , subscribeAttributeIdentifyTimeWithMinInterval_maxInterval_params_subscriptionEstablished_reportHandler
  , readAttributeIdentifyTimeWithAttributeCache_endpoint_queue_completionHandler
  , readAttributeIdentifyTypeWithCompletionHandler
  , subscribeAttributeIdentifyTypeWithMinInterval_maxInterval_params_subscriptionEstablished_reportHandler
  , readAttributeIdentifyTypeWithAttributeCache_endpoint_queue_completionHandler
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
  , identifyWithParams_completionHandlerSelector
  , identifyWithParams_completionSelector
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
  , readAttributeFeatureMapWithAttributeCache_endpoint_queue_completionHandlerSelector
  , readAttributeFeatureMapWithClusterStateCache_endpoint_queue_completionSelector
  , readAttributeFeatureMapWithCompletionHandlerSelector
  , readAttributeFeatureMapWithCompletionSelector
  , readAttributeGeneratedCommandListWithAttributeCache_endpoint_queue_completionHandlerSelector
  , readAttributeGeneratedCommandListWithClusterStateCache_endpoint_queue_completionSelector
  , readAttributeGeneratedCommandListWithCompletionHandlerSelector
  , readAttributeGeneratedCommandListWithCompletionSelector
  , readAttributeIdentifyTimeWithAttributeCache_endpoint_queue_completionHandlerSelector
  , readAttributeIdentifyTimeWithClusterStateCache_endpoint_queue_completionSelector
  , readAttributeIdentifyTimeWithCompletionHandlerSelector
  , readAttributeIdentifyTimeWithCompletionSelector
  , readAttributeIdentifyTypeWithAttributeCache_endpoint_queue_completionHandlerSelector
  , readAttributeIdentifyTypeWithClusterStateCache_endpoint_queue_completionSelector
  , readAttributeIdentifyTypeWithCompletionHandlerSelector
  , readAttributeIdentifyTypeWithCompletionSelector
  , subscribeAttributeAcceptedCommandListWithMinInterval_maxInterval_params_subscriptionEstablished_reportHandlerSelector
  , subscribeAttributeAcceptedCommandListWithParams_subscriptionEstablished_reportHandlerSelector
  , subscribeAttributeAttributeListWithMinInterval_maxInterval_params_subscriptionEstablished_reportHandlerSelector
  , subscribeAttributeAttributeListWithParams_subscriptionEstablished_reportHandlerSelector
  , subscribeAttributeClusterRevisionWithMinInterval_maxInterval_params_subscriptionEstablished_reportHandlerSelector
  , subscribeAttributeClusterRevisionWithParams_subscriptionEstablished_reportHandlerSelector
  , subscribeAttributeFeatureMapWithMinInterval_maxInterval_params_subscriptionEstablished_reportHandlerSelector
  , subscribeAttributeFeatureMapWithParams_subscriptionEstablished_reportHandlerSelector
  , subscribeAttributeGeneratedCommandListWithMinInterval_maxInterval_params_subscriptionEstablished_reportHandlerSelector
  , subscribeAttributeGeneratedCommandListWithParams_subscriptionEstablished_reportHandlerSelector
  , subscribeAttributeIdentifyTimeWithMinInterval_maxInterval_params_subscriptionEstablished_reportHandlerSelector
  , subscribeAttributeIdentifyTimeWithParams_subscriptionEstablished_reportHandlerSelector
  , subscribeAttributeIdentifyTypeWithMinInterval_maxInterval_params_subscriptionEstablished_reportHandlerSelector
  , subscribeAttributeIdentifyTypeWithParams_subscriptionEstablished_reportHandlerSelector
  , triggerEffectWithParams_completionHandlerSelector
  , triggerEffectWithParams_completionSelector
  , writeAttributeIdentifyTimeWithValue_completionHandlerSelector
  , writeAttributeIdentifyTimeWithValue_completionSelector
  , writeAttributeIdentifyTimeWithValue_params_completionHandlerSelector
  , writeAttributeIdentifyTimeWithValue_params_completionSelector


  ) where

import Foreign.Ptr (Ptr, FunPtr)
import Foreign.C.Types

import ObjC.Runtime.Types
import ObjC.Runtime.Message (sendMessage, sendOwnedMessage, sendClassMessage, sendOwnedClassMessage)
import ObjC.Runtime.Selector (mkSelector)
import ObjC.Runtime.Class (getRequiredClass)

import ObjC.Matter.Internal.Classes
import ObjC.Foundation.Internal.Classes

-- | Command Identify
--
-- This command starts or stops the receiving device identifying itself.
--
-- ObjC selector: @- identifyWithParams:completion:@
identifyWithParams_completion :: (IsMTRBaseClusterIdentify mtrBaseClusterIdentify, IsMTRIdentifyClusterIdentifyParams params) => mtrBaseClusterIdentify -> params -> Ptr () -> IO ()
identifyWithParams_completion mtrBaseClusterIdentify params completion =
  sendMessage mtrBaseClusterIdentify identifyWithParams_completionSelector (toMTRIdentifyClusterIdentifyParams params) completion

-- | Command TriggerEffect
--
-- This command allows the support of feedback to the user, such as a certain light effect.
--
-- ObjC selector: @- triggerEffectWithParams:completion:@
triggerEffectWithParams_completion :: (IsMTRBaseClusterIdentify mtrBaseClusterIdentify, IsMTRIdentifyClusterTriggerEffectParams params) => mtrBaseClusterIdentify -> params -> Ptr () -> IO ()
triggerEffectWithParams_completion mtrBaseClusterIdentify params completion =
  sendMessage mtrBaseClusterIdentify triggerEffectWithParams_completionSelector (toMTRIdentifyClusterTriggerEffectParams params) completion

-- | @- readAttributeIdentifyTimeWithCompletion:@
readAttributeIdentifyTimeWithCompletion :: IsMTRBaseClusterIdentify mtrBaseClusterIdentify => mtrBaseClusterIdentify -> Ptr () -> IO ()
readAttributeIdentifyTimeWithCompletion mtrBaseClusterIdentify completion =
  sendMessage mtrBaseClusterIdentify readAttributeIdentifyTimeWithCompletionSelector completion

-- | @- writeAttributeIdentifyTimeWithValue:completion:@
writeAttributeIdentifyTimeWithValue_completion :: (IsMTRBaseClusterIdentify mtrBaseClusterIdentify, IsNSNumber value) => mtrBaseClusterIdentify -> value -> Ptr () -> IO ()
writeAttributeIdentifyTimeWithValue_completion mtrBaseClusterIdentify value completion =
  sendMessage mtrBaseClusterIdentify writeAttributeIdentifyTimeWithValue_completionSelector (toNSNumber value) completion

-- | @- writeAttributeIdentifyTimeWithValue:params:completion:@
writeAttributeIdentifyTimeWithValue_params_completion :: (IsMTRBaseClusterIdentify mtrBaseClusterIdentify, IsNSNumber value, IsMTRWriteParams params) => mtrBaseClusterIdentify -> value -> params -> Ptr () -> IO ()
writeAttributeIdentifyTimeWithValue_params_completion mtrBaseClusterIdentify value params completion =
  sendMessage mtrBaseClusterIdentify writeAttributeIdentifyTimeWithValue_params_completionSelector (toNSNumber value) (toMTRWriteParams params) completion

-- | @- subscribeAttributeIdentifyTimeWithParams:subscriptionEstablished:reportHandler:@
subscribeAttributeIdentifyTimeWithParams_subscriptionEstablished_reportHandler :: (IsMTRBaseClusterIdentify mtrBaseClusterIdentify, IsMTRSubscribeParams params) => mtrBaseClusterIdentify -> params -> Ptr () -> Ptr () -> IO ()
subscribeAttributeIdentifyTimeWithParams_subscriptionEstablished_reportHandler mtrBaseClusterIdentify params subscriptionEstablished reportHandler =
  sendMessage mtrBaseClusterIdentify subscribeAttributeIdentifyTimeWithParams_subscriptionEstablished_reportHandlerSelector (toMTRSubscribeParams params) subscriptionEstablished reportHandler

-- | @+ readAttributeIdentifyTimeWithClusterStateCache:endpoint:queue:completion:@
readAttributeIdentifyTimeWithClusterStateCache_endpoint_queue_completion :: (IsMTRClusterStateCacheContainer clusterStateCacheContainer, IsNSNumber endpoint, IsNSObject queue) => clusterStateCacheContainer -> endpoint -> queue -> Ptr () -> IO ()
readAttributeIdentifyTimeWithClusterStateCache_endpoint_queue_completion clusterStateCacheContainer endpoint queue completion =
  do
    cls' <- getRequiredClass "MTRBaseClusterIdentify"
    sendClassMessage cls' readAttributeIdentifyTimeWithClusterStateCache_endpoint_queue_completionSelector (toMTRClusterStateCacheContainer clusterStateCacheContainer) (toNSNumber endpoint) (toNSObject queue) completion

-- | @- readAttributeIdentifyTypeWithCompletion:@
readAttributeIdentifyTypeWithCompletion :: IsMTRBaseClusterIdentify mtrBaseClusterIdentify => mtrBaseClusterIdentify -> Ptr () -> IO ()
readAttributeIdentifyTypeWithCompletion mtrBaseClusterIdentify completion =
  sendMessage mtrBaseClusterIdentify readAttributeIdentifyTypeWithCompletionSelector completion

-- | @- subscribeAttributeIdentifyTypeWithParams:subscriptionEstablished:reportHandler:@
subscribeAttributeIdentifyTypeWithParams_subscriptionEstablished_reportHandler :: (IsMTRBaseClusterIdentify mtrBaseClusterIdentify, IsMTRSubscribeParams params) => mtrBaseClusterIdentify -> params -> Ptr () -> Ptr () -> IO ()
subscribeAttributeIdentifyTypeWithParams_subscriptionEstablished_reportHandler mtrBaseClusterIdentify params subscriptionEstablished reportHandler =
  sendMessage mtrBaseClusterIdentify subscribeAttributeIdentifyTypeWithParams_subscriptionEstablished_reportHandlerSelector (toMTRSubscribeParams params) subscriptionEstablished reportHandler

-- | @+ readAttributeIdentifyTypeWithClusterStateCache:endpoint:queue:completion:@
readAttributeIdentifyTypeWithClusterStateCache_endpoint_queue_completion :: (IsMTRClusterStateCacheContainer clusterStateCacheContainer, IsNSNumber endpoint, IsNSObject queue) => clusterStateCacheContainer -> endpoint -> queue -> Ptr () -> IO ()
readAttributeIdentifyTypeWithClusterStateCache_endpoint_queue_completion clusterStateCacheContainer endpoint queue completion =
  do
    cls' <- getRequiredClass "MTRBaseClusterIdentify"
    sendClassMessage cls' readAttributeIdentifyTypeWithClusterStateCache_endpoint_queue_completionSelector (toMTRClusterStateCacheContainer clusterStateCacheContainer) (toNSNumber endpoint) (toNSObject queue) completion

-- | @- readAttributeGeneratedCommandListWithCompletion:@
readAttributeGeneratedCommandListWithCompletion :: IsMTRBaseClusterIdentify mtrBaseClusterIdentify => mtrBaseClusterIdentify -> Ptr () -> IO ()
readAttributeGeneratedCommandListWithCompletion mtrBaseClusterIdentify completion =
  sendMessage mtrBaseClusterIdentify readAttributeGeneratedCommandListWithCompletionSelector completion

-- | @- subscribeAttributeGeneratedCommandListWithParams:subscriptionEstablished:reportHandler:@
subscribeAttributeGeneratedCommandListWithParams_subscriptionEstablished_reportHandler :: (IsMTRBaseClusterIdentify mtrBaseClusterIdentify, IsMTRSubscribeParams params) => mtrBaseClusterIdentify -> params -> Ptr () -> Ptr () -> IO ()
subscribeAttributeGeneratedCommandListWithParams_subscriptionEstablished_reportHandler mtrBaseClusterIdentify params subscriptionEstablished reportHandler =
  sendMessage mtrBaseClusterIdentify subscribeAttributeGeneratedCommandListWithParams_subscriptionEstablished_reportHandlerSelector (toMTRSubscribeParams params) subscriptionEstablished reportHandler

-- | @+ readAttributeGeneratedCommandListWithClusterStateCache:endpoint:queue:completion:@
readAttributeGeneratedCommandListWithClusterStateCache_endpoint_queue_completion :: (IsMTRClusterStateCacheContainer clusterStateCacheContainer, IsNSNumber endpoint, IsNSObject queue) => clusterStateCacheContainer -> endpoint -> queue -> Ptr () -> IO ()
readAttributeGeneratedCommandListWithClusterStateCache_endpoint_queue_completion clusterStateCacheContainer endpoint queue completion =
  do
    cls' <- getRequiredClass "MTRBaseClusterIdentify"
    sendClassMessage cls' readAttributeGeneratedCommandListWithClusterStateCache_endpoint_queue_completionSelector (toMTRClusterStateCacheContainer clusterStateCacheContainer) (toNSNumber endpoint) (toNSObject queue) completion

-- | @- readAttributeAcceptedCommandListWithCompletion:@
readAttributeAcceptedCommandListWithCompletion :: IsMTRBaseClusterIdentify mtrBaseClusterIdentify => mtrBaseClusterIdentify -> Ptr () -> IO ()
readAttributeAcceptedCommandListWithCompletion mtrBaseClusterIdentify completion =
  sendMessage mtrBaseClusterIdentify readAttributeAcceptedCommandListWithCompletionSelector completion

-- | @- subscribeAttributeAcceptedCommandListWithParams:subscriptionEstablished:reportHandler:@
subscribeAttributeAcceptedCommandListWithParams_subscriptionEstablished_reportHandler :: (IsMTRBaseClusterIdentify mtrBaseClusterIdentify, IsMTRSubscribeParams params) => mtrBaseClusterIdentify -> params -> Ptr () -> Ptr () -> IO ()
subscribeAttributeAcceptedCommandListWithParams_subscriptionEstablished_reportHandler mtrBaseClusterIdentify params subscriptionEstablished reportHandler =
  sendMessage mtrBaseClusterIdentify subscribeAttributeAcceptedCommandListWithParams_subscriptionEstablished_reportHandlerSelector (toMTRSubscribeParams params) subscriptionEstablished reportHandler

-- | @+ readAttributeAcceptedCommandListWithClusterStateCache:endpoint:queue:completion:@
readAttributeAcceptedCommandListWithClusterStateCache_endpoint_queue_completion :: (IsMTRClusterStateCacheContainer clusterStateCacheContainer, IsNSNumber endpoint, IsNSObject queue) => clusterStateCacheContainer -> endpoint -> queue -> Ptr () -> IO ()
readAttributeAcceptedCommandListWithClusterStateCache_endpoint_queue_completion clusterStateCacheContainer endpoint queue completion =
  do
    cls' <- getRequiredClass "MTRBaseClusterIdentify"
    sendClassMessage cls' readAttributeAcceptedCommandListWithClusterStateCache_endpoint_queue_completionSelector (toMTRClusterStateCacheContainer clusterStateCacheContainer) (toNSNumber endpoint) (toNSObject queue) completion

-- | @- readAttributeAttributeListWithCompletion:@
readAttributeAttributeListWithCompletion :: IsMTRBaseClusterIdentify mtrBaseClusterIdentify => mtrBaseClusterIdentify -> Ptr () -> IO ()
readAttributeAttributeListWithCompletion mtrBaseClusterIdentify completion =
  sendMessage mtrBaseClusterIdentify readAttributeAttributeListWithCompletionSelector completion

-- | @- subscribeAttributeAttributeListWithParams:subscriptionEstablished:reportHandler:@
subscribeAttributeAttributeListWithParams_subscriptionEstablished_reportHandler :: (IsMTRBaseClusterIdentify mtrBaseClusterIdentify, IsMTRSubscribeParams params) => mtrBaseClusterIdentify -> params -> Ptr () -> Ptr () -> IO ()
subscribeAttributeAttributeListWithParams_subscriptionEstablished_reportHandler mtrBaseClusterIdentify params subscriptionEstablished reportHandler =
  sendMessage mtrBaseClusterIdentify subscribeAttributeAttributeListWithParams_subscriptionEstablished_reportHandlerSelector (toMTRSubscribeParams params) subscriptionEstablished reportHandler

-- | @+ readAttributeAttributeListWithClusterStateCache:endpoint:queue:completion:@
readAttributeAttributeListWithClusterStateCache_endpoint_queue_completion :: (IsMTRClusterStateCacheContainer clusterStateCacheContainer, IsNSNumber endpoint, IsNSObject queue) => clusterStateCacheContainer -> endpoint -> queue -> Ptr () -> IO ()
readAttributeAttributeListWithClusterStateCache_endpoint_queue_completion clusterStateCacheContainer endpoint queue completion =
  do
    cls' <- getRequiredClass "MTRBaseClusterIdentify"
    sendClassMessage cls' readAttributeAttributeListWithClusterStateCache_endpoint_queue_completionSelector (toMTRClusterStateCacheContainer clusterStateCacheContainer) (toNSNumber endpoint) (toNSObject queue) completion

-- | @- readAttributeFeatureMapWithCompletion:@
readAttributeFeatureMapWithCompletion :: IsMTRBaseClusterIdentify mtrBaseClusterIdentify => mtrBaseClusterIdentify -> Ptr () -> IO ()
readAttributeFeatureMapWithCompletion mtrBaseClusterIdentify completion =
  sendMessage mtrBaseClusterIdentify readAttributeFeatureMapWithCompletionSelector completion

-- | @- subscribeAttributeFeatureMapWithParams:subscriptionEstablished:reportHandler:@
subscribeAttributeFeatureMapWithParams_subscriptionEstablished_reportHandler :: (IsMTRBaseClusterIdentify mtrBaseClusterIdentify, IsMTRSubscribeParams params) => mtrBaseClusterIdentify -> params -> Ptr () -> Ptr () -> IO ()
subscribeAttributeFeatureMapWithParams_subscriptionEstablished_reportHandler mtrBaseClusterIdentify params subscriptionEstablished reportHandler =
  sendMessage mtrBaseClusterIdentify subscribeAttributeFeatureMapWithParams_subscriptionEstablished_reportHandlerSelector (toMTRSubscribeParams params) subscriptionEstablished reportHandler

-- | @+ readAttributeFeatureMapWithClusterStateCache:endpoint:queue:completion:@
readAttributeFeatureMapWithClusterStateCache_endpoint_queue_completion :: (IsMTRClusterStateCacheContainer clusterStateCacheContainer, IsNSNumber endpoint, IsNSObject queue) => clusterStateCacheContainer -> endpoint -> queue -> Ptr () -> IO ()
readAttributeFeatureMapWithClusterStateCache_endpoint_queue_completion clusterStateCacheContainer endpoint queue completion =
  do
    cls' <- getRequiredClass "MTRBaseClusterIdentify"
    sendClassMessage cls' readAttributeFeatureMapWithClusterStateCache_endpoint_queue_completionSelector (toMTRClusterStateCacheContainer clusterStateCacheContainer) (toNSNumber endpoint) (toNSObject queue) completion

-- | @- readAttributeClusterRevisionWithCompletion:@
readAttributeClusterRevisionWithCompletion :: IsMTRBaseClusterIdentify mtrBaseClusterIdentify => mtrBaseClusterIdentify -> Ptr () -> IO ()
readAttributeClusterRevisionWithCompletion mtrBaseClusterIdentify completion =
  sendMessage mtrBaseClusterIdentify readAttributeClusterRevisionWithCompletionSelector completion

-- | @- subscribeAttributeClusterRevisionWithParams:subscriptionEstablished:reportHandler:@
subscribeAttributeClusterRevisionWithParams_subscriptionEstablished_reportHandler :: (IsMTRBaseClusterIdentify mtrBaseClusterIdentify, IsMTRSubscribeParams params) => mtrBaseClusterIdentify -> params -> Ptr () -> Ptr () -> IO ()
subscribeAttributeClusterRevisionWithParams_subscriptionEstablished_reportHandler mtrBaseClusterIdentify params subscriptionEstablished reportHandler =
  sendMessage mtrBaseClusterIdentify subscribeAttributeClusterRevisionWithParams_subscriptionEstablished_reportHandlerSelector (toMTRSubscribeParams params) subscriptionEstablished reportHandler

-- | @+ readAttributeClusterRevisionWithClusterStateCache:endpoint:queue:completion:@
readAttributeClusterRevisionWithClusterStateCache_endpoint_queue_completion :: (IsMTRClusterStateCacheContainer clusterStateCacheContainer, IsNSNumber endpoint, IsNSObject queue) => clusterStateCacheContainer -> endpoint -> queue -> Ptr () -> IO ()
readAttributeClusterRevisionWithClusterStateCache_endpoint_queue_completion clusterStateCacheContainer endpoint queue completion =
  do
    cls' <- getRequiredClass "MTRBaseClusterIdentify"
    sendClassMessage cls' readAttributeClusterRevisionWithClusterStateCache_endpoint_queue_completionSelector (toMTRClusterStateCacheContainer clusterStateCacheContainer) (toNSNumber endpoint) (toNSObject queue) completion

-- | @- init@
init_ :: IsMTRBaseClusterIdentify mtrBaseClusterIdentify => mtrBaseClusterIdentify -> IO (Id MTRBaseClusterIdentify)
init_ mtrBaseClusterIdentify =
  sendOwnedMessage mtrBaseClusterIdentify initSelector

-- | @+ new@
new :: IO (Id MTRBaseClusterIdentify)
new  =
  do
    cls' <- getRequiredClass "MTRBaseClusterIdentify"
    sendOwnedClassMessage cls' newSelector

-- | @- initWithDevice:endpoint:queue:@
initWithDevice_endpoint_queue :: (IsMTRBaseClusterIdentify mtrBaseClusterIdentify, IsMTRBaseDevice device, IsNSObject queue) => mtrBaseClusterIdentify -> device -> CUShort -> queue -> IO (Id MTRBaseClusterIdentify)
initWithDevice_endpoint_queue mtrBaseClusterIdentify device endpoint queue =
  sendOwnedMessage mtrBaseClusterIdentify initWithDevice_endpoint_queueSelector (toMTRBaseDevice device) endpoint (toNSObject queue)

-- | @- identifyWithParams:completionHandler:@
identifyWithParams_completionHandler :: (IsMTRBaseClusterIdentify mtrBaseClusterIdentify, IsMTRIdentifyClusterIdentifyParams params) => mtrBaseClusterIdentify -> params -> Ptr () -> IO ()
identifyWithParams_completionHandler mtrBaseClusterIdentify params completionHandler =
  sendMessage mtrBaseClusterIdentify identifyWithParams_completionHandlerSelector (toMTRIdentifyClusterIdentifyParams params) completionHandler

-- | @- triggerEffectWithParams:completionHandler:@
triggerEffectWithParams_completionHandler :: (IsMTRBaseClusterIdentify mtrBaseClusterIdentify, IsMTRIdentifyClusterTriggerEffectParams params) => mtrBaseClusterIdentify -> params -> Ptr () -> IO ()
triggerEffectWithParams_completionHandler mtrBaseClusterIdentify params completionHandler =
  sendMessage mtrBaseClusterIdentify triggerEffectWithParams_completionHandlerSelector (toMTRIdentifyClusterTriggerEffectParams params) completionHandler

-- | @- readAttributeIdentifyTimeWithCompletionHandler:@
readAttributeIdentifyTimeWithCompletionHandler :: IsMTRBaseClusterIdentify mtrBaseClusterIdentify => mtrBaseClusterIdentify -> Ptr () -> IO ()
readAttributeIdentifyTimeWithCompletionHandler mtrBaseClusterIdentify completionHandler =
  sendMessage mtrBaseClusterIdentify readAttributeIdentifyTimeWithCompletionHandlerSelector completionHandler

-- | @- writeAttributeIdentifyTimeWithValue:completionHandler:@
writeAttributeIdentifyTimeWithValue_completionHandler :: (IsMTRBaseClusterIdentify mtrBaseClusterIdentify, IsNSNumber value) => mtrBaseClusterIdentify -> value -> Ptr () -> IO ()
writeAttributeIdentifyTimeWithValue_completionHandler mtrBaseClusterIdentify value completionHandler =
  sendMessage mtrBaseClusterIdentify writeAttributeIdentifyTimeWithValue_completionHandlerSelector (toNSNumber value) completionHandler

-- | @- writeAttributeIdentifyTimeWithValue:params:completionHandler:@
writeAttributeIdentifyTimeWithValue_params_completionHandler :: (IsMTRBaseClusterIdentify mtrBaseClusterIdentify, IsNSNumber value, IsMTRWriteParams params) => mtrBaseClusterIdentify -> value -> params -> Ptr () -> IO ()
writeAttributeIdentifyTimeWithValue_params_completionHandler mtrBaseClusterIdentify value params completionHandler =
  sendMessage mtrBaseClusterIdentify writeAttributeIdentifyTimeWithValue_params_completionHandlerSelector (toNSNumber value) (toMTRWriteParams params) completionHandler

-- | @- subscribeAttributeIdentifyTimeWithMinInterval:maxInterval:params:subscriptionEstablished:reportHandler:@
subscribeAttributeIdentifyTimeWithMinInterval_maxInterval_params_subscriptionEstablished_reportHandler :: (IsMTRBaseClusterIdentify mtrBaseClusterIdentify, IsNSNumber minInterval, IsNSNumber maxInterval, IsMTRSubscribeParams params) => mtrBaseClusterIdentify -> minInterval -> maxInterval -> params -> Ptr () -> Ptr () -> IO ()
subscribeAttributeIdentifyTimeWithMinInterval_maxInterval_params_subscriptionEstablished_reportHandler mtrBaseClusterIdentify minInterval maxInterval params subscriptionEstablishedHandler reportHandler =
  sendMessage mtrBaseClusterIdentify subscribeAttributeIdentifyTimeWithMinInterval_maxInterval_params_subscriptionEstablished_reportHandlerSelector (toNSNumber minInterval) (toNSNumber maxInterval) (toMTRSubscribeParams params) subscriptionEstablishedHandler reportHandler

-- | @+ readAttributeIdentifyTimeWithAttributeCache:endpoint:queue:completionHandler:@
readAttributeIdentifyTimeWithAttributeCache_endpoint_queue_completionHandler :: (IsMTRAttributeCacheContainer attributeCacheContainer, IsNSNumber endpoint, IsNSObject queue) => attributeCacheContainer -> endpoint -> queue -> Ptr () -> IO ()
readAttributeIdentifyTimeWithAttributeCache_endpoint_queue_completionHandler attributeCacheContainer endpoint queue completionHandler =
  do
    cls' <- getRequiredClass "MTRBaseClusterIdentify"
    sendClassMessage cls' readAttributeIdentifyTimeWithAttributeCache_endpoint_queue_completionHandlerSelector (toMTRAttributeCacheContainer attributeCacheContainer) (toNSNumber endpoint) (toNSObject queue) completionHandler

-- | @- readAttributeIdentifyTypeWithCompletionHandler:@
readAttributeIdentifyTypeWithCompletionHandler :: IsMTRBaseClusterIdentify mtrBaseClusterIdentify => mtrBaseClusterIdentify -> Ptr () -> IO ()
readAttributeIdentifyTypeWithCompletionHandler mtrBaseClusterIdentify completionHandler =
  sendMessage mtrBaseClusterIdentify readAttributeIdentifyTypeWithCompletionHandlerSelector completionHandler

-- | @- subscribeAttributeIdentifyTypeWithMinInterval:maxInterval:params:subscriptionEstablished:reportHandler:@
subscribeAttributeIdentifyTypeWithMinInterval_maxInterval_params_subscriptionEstablished_reportHandler :: (IsMTRBaseClusterIdentify mtrBaseClusterIdentify, IsNSNumber minInterval, IsNSNumber maxInterval, IsMTRSubscribeParams params) => mtrBaseClusterIdentify -> minInterval -> maxInterval -> params -> Ptr () -> Ptr () -> IO ()
subscribeAttributeIdentifyTypeWithMinInterval_maxInterval_params_subscriptionEstablished_reportHandler mtrBaseClusterIdentify minInterval maxInterval params subscriptionEstablishedHandler reportHandler =
  sendMessage mtrBaseClusterIdentify subscribeAttributeIdentifyTypeWithMinInterval_maxInterval_params_subscriptionEstablished_reportHandlerSelector (toNSNumber minInterval) (toNSNumber maxInterval) (toMTRSubscribeParams params) subscriptionEstablishedHandler reportHandler

-- | @+ readAttributeIdentifyTypeWithAttributeCache:endpoint:queue:completionHandler:@
readAttributeIdentifyTypeWithAttributeCache_endpoint_queue_completionHandler :: (IsMTRAttributeCacheContainer attributeCacheContainer, IsNSNumber endpoint, IsNSObject queue) => attributeCacheContainer -> endpoint -> queue -> Ptr () -> IO ()
readAttributeIdentifyTypeWithAttributeCache_endpoint_queue_completionHandler attributeCacheContainer endpoint queue completionHandler =
  do
    cls' <- getRequiredClass "MTRBaseClusterIdentify"
    sendClassMessage cls' readAttributeIdentifyTypeWithAttributeCache_endpoint_queue_completionHandlerSelector (toMTRAttributeCacheContainer attributeCacheContainer) (toNSNumber endpoint) (toNSObject queue) completionHandler

-- | @- readAttributeGeneratedCommandListWithCompletionHandler:@
readAttributeGeneratedCommandListWithCompletionHandler :: IsMTRBaseClusterIdentify mtrBaseClusterIdentify => mtrBaseClusterIdentify -> Ptr () -> IO ()
readAttributeGeneratedCommandListWithCompletionHandler mtrBaseClusterIdentify completionHandler =
  sendMessage mtrBaseClusterIdentify readAttributeGeneratedCommandListWithCompletionHandlerSelector completionHandler

-- | @- subscribeAttributeGeneratedCommandListWithMinInterval:maxInterval:params:subscriptionEstablished:reportHandler:@
subscribeAttributeGeneratedCommandListWithMinInterval_maxInterval_params_subscriptionEstablished_reportHandler :: (IsMTRBaseClusterIdentify mtrBaseClusterIdentify, IsNSNumber minInterval, IsNSNumber maxInterval, IsMTRSubscribeParams params) => mtrBaseClusterIdentify -> minInterval -> maxInterval -> params -> Ptr () -> Ptr () -> IO ()
subscribeAttributeGeneratedCommandListWithMinInterval_maxInterval_params_subscriptionEstablished_reportHandler mtrBaseClusterIdentify minInterval maxInterval params subscriptionEstablishedHandler reportHandler =
  sendMessage mtrBaseClusterIdentify subscribeAttributeGeneratedCommandListWithMinInterval_maxInterval_params_subscriptionEstablished_reportHandlerSelector (toNSNumber minInterval) (toNSNumber maxInterval) (toMTRSubscribeParams params) subscriptionEstablishedHandler reportHandler

-- | @+ readAttributeGeneratedCommandListWithAttributeCache:endpoint:queue:completionHandler:@
readAttributeGeneratedCommandListWithAttributeCache_endpoint_queue_completionHandler :: (IsMTRAttributeCacheContainer attributeCacheContainer, IsNSNumber endpoint, IsNSObject queue) => attributeCacheContainer -> endpoint -> queue -> Ptr () -> IO ()
readAttributeGeneratedCommandListWithAttributeCache_endpoint_queue_completionHandler attributeCacheContainer endpoint queue completionHandler =
  do
    cls' <- getRequiredClass "MTRBaseClusterIdentify"
    sendClassMessage cls' readAttributeGeneratedCommandListWithAttributeCache_endpoint_queue_completionHandlerSelector (toMTRAttributeCacheContainer attributeCacheContainer) (toNSNumber endpoint) (toNSObject queue) completionHandler

-- | @- readAttributeAcceptedCommandListWithCompletionHandler:@
readAttributeAcceptedCommandListWithCompletionHandler :: IsMTRBaseClusterIdentify mtrBaseClusterIdentify => mtrBaseClusterIdentify -> Ptr () -> IO ()
readAttributeAcceptedCommandListWithCompletionHandler mtrBaseClusterIdentify completionHandler =
  sendMessage mtrBaseClusterIdentify readAttributeAcceptedCommandListWithCompletionHandlerSelector completionHandler

-- | @- subscribeAttributeAcceptedCommandListWithMinInterval:maxInterval:params:subscriptionEstablished:reportHandler:@
subscribeAttributeAcceptedCommandListWithMinInterval_maxInterval_params_subscriptionEstablished_reportHandler :: (IsMTRBaseClusterIdentify mtrBaseClusterIdentify, IsNSNumber minInterval, IsNSNumber maxInterval, IsMTRSubscribeParams params) => mtrBaseClusterIdentify -> minInterval -> maxInterval -> params -> Ptr () -> Ptr () -> IO ()
subscribeAttributeAcceptedCommandListWithMinInterval_maxInterval_params_subscriptionEstablished_reportHandler mtrBaseClusterIdentify minInterval maxInterval params subscriptionEstablishedHandler reportHandler =
  sendMessage mtrBaseClusterIdentify subscribeAttributeAcceptedCommandListWithMinInterval_maxInterval_params_subscriptionEstablished_reportHandlerSelector (toNSNumber minInterval) (toNSNumber maxInterval) (toMTRSubscribeParams params) subscriptionEstablishedHandler reportHandler

-- | @+ readAttributeAcceptedCommandListWithAttributeCache:endpoint:queue:completionHandler:@
readAttributeAcceptedCommandListWithAttributeCache_endpoint_queue_completionHandler :: (IsMTRAttributeCacheContainer attributeCacheContainer, IsNSNumber endpoint, IsNSObject queue) => attributeCacheContainer -> endpoint -> queue -> Ptr () -> IO ()
readAttributeAcceptedCommandListWithAttributeCache_endpoint_queue_completionHandler attributeCacheContainer endpoint queue completionHandler =
  do
    cls' <- getRequiredClass "MTRBaseClusterIdentify"
    sendClassMessage cls' readAttributeAcceptedCommandListWithAttributeCache_endpoint_queue_completionHandlerSelector (toMTRAttributeCacheContainer attributeCacheContainer) (toNSNumber endpoint) (toNSObject queue) completionHandler

-- | @- readAttributeAttributeListWithCompletionHandler:@
readAttributeAttributeListWithCompletionHandler :: IsMTRBaseClusterIdentify mtrBaseClusterIdentify => mtrBaseClusterIdentify -> Ptr () -> IO ()
readAttributeAttributeListWithCompletionHandler mtrBaseClusterIdentify completionHandler =
  sendMessage mtrBaseClusterIdentify readAttributeAttributeListWithCompletionHandlerSelector completionHandler

-- | @- subscribeAttributeAttributeListWithMinInterval:maxInterval:params:subscriptionEstablished:reportHandler:@
subscribeAttributeAttributeListWithMinInterval_maxInterval_params_subscriptionEstablished_reportHandler :: (IsMTRBaseClusterIdentify mtrBaseClusterIdentify, IsNSNumber minInterval, IsNSNumber maxInterval, IsMTRSubscribeParams params) => mtrBaseClusterIdentify -> minInterval -> maxInterval -> params -> Ptr () -> Ptr () -> IO ()
subscribeAttributeAttributeListWithMinInterval_maxInterval_params_subscriptionEstablished_reportHandler mtrBaseClusterIdentify minInterval maxInterval params subscriptionEstablishedHandler reportHandler =
  sendMessage mtrBaseClusterIdentify subscribeAttributeAttributeListWithMinInterval_maxInterval_params_subscriptionEstablished_reportHandlerSelector (toNSNumber minInterval) (toNSNumber maxInterval) (toMTRSubscribeParams params) subscriptionEstablishedHandler reportHandler

-- | @+ readAttributeAttributeListWithAttributeCache:endpoint:queue:completionHandler:@
readAttributeAttributeListWithAttributeCache_endpoint_queue_completionHandler :: (IsMTRAttributeCacheContainer attributeCacheContainer, IsNSNumber endpoint, IsNSObject queue) => attributeCacheContainer -> endpoint -> queue -> Ptr () -> IO ()
readAttributeAttributeListWithAttributeCache_endpoint_queue_completionHandler attributeCacheContainer endpoint queue completionHandler =
  do
    cls' <- getRequiredClass "MTRBaseClusterIdentify"
    sendClassMessage cls' readAttributeAttributeListWithAttributeCache_endpoint_queue_completionHandlerSelector (toMTRAttributeCacheContainer attributeCacheContainer) (toNSNumber endpoint) (toNSObject queue) completionHandler

-- | @- readAttributeFeatureMapWithCompletionHandler:@
readAttributeFeatureMapWithCompletionHandler :: IsMTRBaseClusterIdentify mtrBaseClusterIdentify => mtrBaseClusterIdentify -> Ptr () -> IO ()
readAttributeFeatureMapWithCompletionHandler mtrBaseClusterIdentify completionHandler =
  sendMessage mtrBaseClusterIdentify readAttributeFeatureMapWithCompletionHandlerSelector completionHandler

-- | @- subscribeAttributeFeatureMapWithMinInterval:maxInterval:params:subscriptionEstablished:reportHandler:@
subscribeAttributeFeatureMapWithMinInterval_maxInterval_params_subscriptionEstablished_reportHandler :: (IsMTRBaseClusterIdentify mtrBaseClusterIdentify, IsNSNumber minInterval, IsNSNumber maxInterval, IsMTRSubscribeParams params) => mtrBaseClusterIdentify -> minInterval -> maxInterval -> params -> Ptr () -> Ptr () -> IO ()
subscribeAttributeFeatureMapWithMinInterval_maxInterval_params_subscriptionEstablished_reportHandler mtrBaseClusterIdentify minInterval maxInterval params subscriptionEstablishedHandler reportHandler =
  sendMessage mtrBaseClusterIdentify subscribeAttributeFeatureMapWithMinInterval_maxInterval_params_subscriptionEstablished_reportHandlerSelector (toNSNumber minInterval) (toNSNumber maxInterval) (toMTRSubscribeParams params) subscriptionEstablishedHandler reportHandler

-- | @+ readAttributeFeatureMapWithAttributeCache:endpoint:queue:completionHandler:@
readAttributeFeatureMapWithAttributeCache_endpoint_queue_completionHandler :: (IsMTRAttributeCacheContainer attributeCacheContainer, IsNSNumber endpoint, IsNSObject queue) => attributeCacheContainer -> endpoint -> queue -> Ptr () -> IO ()
readAttributeFeatureMapWithAttributeCache_endpoint_queue_completionHandler attributeCacheContainer endpoint queue completionHandler =
  do
    cls' <- getRequiredClass "MTRBaseClusterIdentify"
    sendClassMessage cls' readAttributeFeatureMapWithAttributeCache_endpoint_queue_completionHandlerSelector (toMTRAttributeCacheContainer attributeCacheContainer) (toNSNumber endpoint) (toNSObject queue) completionHandler

-- | @- readAttributeClusterRevisionWithCompletionHandler:@
readAttributeClusterRevisionWithCompletionHandler :: IsMTRBaseClusterIdentify mtrBaseClusterIdentify => mtrBaseClusterIdentify -> Ptr () -> IO ()
readAttributeClusterRevisionWithCompletionHandler mtrBaseClusterIdentify completionHandler =
  sendMessage mtrBaseClusterIdentify readAttributeClusterRevisionWithCompletionHandlerSelector completionHandler

-- | @- subscribeAttributeClusterRevisionWithMinInterval:maxInterval:params:subscriptionEstablished:reportHandler:@
subscribeAttributeClusterRevisionWithMinInterval_maxInterval_params_subscriptionEstablished_reportHandler :: (IsMTRBaseClusterIdentify mtrBaseClusterIdentify, IsNSNumber minInterval, IsNSNumber maxInterval, IsMTRSubscribeParams params) => mtrBaseClusterIdentify -> minInterval -> maxInterval -> params -> Ptr () -> Ptr () -> IO ()
subscribeAttributeClusterRevisionWithMinInterval_maxInterval_params_subscriptionEstablished_reportHandler mtrBaseClusterIdentify minInterval maxInterval params subscriptionEstablishedHandler reportHandler =
  sendMessage mtrBaseClusterIdentify subscribeAttributeClusterRevisionWithMinInterval_maxInterval_params_subscriptionEstablished_reportHandlerSelector (toNSNumber minInterval) (toNSNumber maxInterval) (toMTRSubscribeParams params) subscriptionEstablishedHandler reportHandler

-- | @+ readAttributeClusterRevisionWithAttributeCache:endpoint:queue:completionHandler:@
readAttributeClusterRevisionWithAttributeCache_endpoint_queue_completionHandler :: (IsMTRAttributeCacheContainer attributeCacheContainer, IsNSNumber endpoint, IsNSObject queue) => attributeCacheContainer -> endpoint -> queue -> Ptr () -> IO ()
readAttributeClusterRevisionWithAttributeCache_endpoint_queue_completionHandler attributeCacheContainer endpoint queue completionHandler =
  do
    cls' <- getRequiredClass "MTRBaseClusterIdentify"
    sendClassMessage cls' readAttributeClusterRevisionWithAttributeCache_endpoint_queue_completionHandlerSelector (toMTRAttributeCacheContainer attributeCacheContainer) (toNSNumber endpoint) (toNSObject queue) completionHandler

-- | For all instance methods (reads, writes, commands) that take a completion, the completion will be called on the provided queue.
--
-- ObjC selector: @- initWithDevice:endpointID:queue:@
initWithDevice_endpointID_queue :: (IsMTRBaseClusterIdentify mtrBaseClusterIdentify, IsMTRBaseDevice device, IsNSNumber endpointID, IsNSObject queue) => mtrBaseClusterIdentify -> device -> endpointID -> queue -> IO (Id MTRBaseClusterIdentify)
initWithDevice_endpointID_queue mtrBaseClusterIdentify device endpointID queue =
  sendOwnedMessage mtrBaseClusterIdentify initWithDevice_endpointID_queueSelector (toMTRBaseDevice device) (toNSNumber endpointID) (toNSObject queue)

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @identifyWithParams:completion:@
identifyWithParams_completionSelector :: Selector '[Id MTRIdentifyClusterIdentifyParams, Ptr ()] ()
identifyWithParams_completionSelector = mkSelector "identifyWithParams:completion:"

-- | @Selector@ for @triggerEffectWithParams:completion:@
triggerEffectWithParams_completionSelector :: Selector '[Id MTRIdentifyClusterTriggerEffectParams, Ptr ()] ()
triggerEffectWithParams_completionSelector = mkSelector "triggerEffectWithParams:completion:"

-- | @Selector@ for @readAttributeIdentifyTimeWithCompletion:@
readAttributeIdentifyTimeWithCompletionSelector :: Selector '[Ptr ()] ()
readAttributeIdentifyTimeWithCompletionSelector = mkSelector "readAttributeIdentifyTimeWithCompletion:"

-- | @Selector@ for @writeAttributeIdentifyTimeWithValue:completion:@
writeAttributeIdentifyTimeWithValue_completionSelector :: Selector '[Id NSNumber, Ptr ()] ()
writeAttributeIdentifyTimeWithValue_completionSelector = mkSelector "writeAttributeIdentifyTimeWithValue:completion:"

-- | @Selector@ for @writeAttributeIdentifyTimeWithValue:params:completion:@
writeAttributeIdentifyTimeWithValue_params_completionSelector :: Selector '[Id NSNumber, Id MTRWriteParams, Ptr ()] ()
writeAttributeIdentifyTimeWithValue_params_completionSelector = mkSelector "writeAttributeIdentifyTimeWithValue:params:completion:"

-- | @Selector@ for @subscribeAttributeIdentifyTimeWithParams:subscriptionEstablished:reportHandler:@
subscribeAttributeIdentifyTimeWithParams_subscriptionEstablished_reportHandlerSelector :: Selector '[Id MTRSubscribeParams, Ptr (), Ptr ()] ()
subscribeAttributeIdentifyTimeWithParams_subscriptionEstablished_reportHandlerSelector = mkSelector "subscribeAttributeIdentifyTimeWithParams:subscriptionEstablished:reportHandler:"

-- | @Selector@ for @readAttributeIdentifyTimeWithClusterStateCache:endpoint:queue:completion:@
readAttributeIdentifyTimeWithClusterStateCache_endpoint_queue_completionSelector :: Selector '[Id MTRClusterStateCacheContainer, Id NSNumber, Id NSObject, Ptr ()] ()
readAttributeIdentifyTimeWithClusterStateCache_endpoint_queue_completionSelector = mkSelector "readAttributeIdentifyTimeWithClusterStateCache:endpoint:queue:completion:"

-- | @Selector@ for @readAttributeIdentifyTypeWithCompletion:@
readAttributeIdentifyTypeWithCompletionSelector :: Selector '[Ptr ()] ()
readAttributeIdentifyTypeWithCompletionSelector = mkSelector "readAttributeIdentifyTypeWithCompletion:"

-- | @Selector@ for @subscribeAttributeIdentifyTypeWithParams:subscriptionEstablished:reportHandler:@
subscribeAttributeIdentifyTypeWithParams_subscriptionEstablished_reportHandlerSelector :: Selector '[Id MTRSubscribeParams, Ptr (), Ptr ()] ()
subscribeAttributeIdentifyTypeWithParams_subscriptionEstablished_reportHandlerSelector = mkSelector "subscribeAttributeIdentifyTypeWithParams:subscriptionEstablished:reportHandler:"

-- | @Selector@ for @readAttributeIdentifyTypeWithClusterStateCache:endpoint:queue:completion:@
readAttributeIdentifyTypeWithClusterStateCache_endpoint_queue_completionSelector :: Selector '[Id MTRClusterStateCacheContainer, Id NSNumber, Id NSObject, Ptr ()] ()
readAttributeIdentifyTypeWithClusterStateCache_endpoint_queue_completionSelector = mkSelector "readAttributeIdentifyTypeWithClusterStateCache:endpoint:queue:completion:"

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
initSelector :: Selector '[] (Id MTRBaseClusterIdentify)
initSelector = mkSelector "init"

-- | @Selector@ for @new@
newSelector :: Selector '[] (Id MTRBaseClusterIdentify)
newSelector = mkSelector "new"

-- | @Selector@ for @initWithDevice:endpoint:queue:@
initWithDevice_endpoint_queueSelector :: Selector '[Id MTRBaseDevice, CUShort, Id NSObject] (Id MTRBaseClusterIdentify)
initWithDevice_endpoint_queueSelector = mkSelector "initWithDevice:endpoint:queue:"

-- | @Selector@ for @identifyWithParams:completionHandler:@
identifyWithParams_completionHandlerSelector :: Selector '[Id MTRIdentifyClusterIdentifyParams, Ptr ()] ()
identifyWithParams_completionHandlerSelector = mkSelector "identifyWithParams:completionHandler:"

-- | @Selector@ for @triggerEffectWithParams:completionHandler:@
triggerEffectWithParams_completionHandlerSelector :: Selector '[Id MTRIdentifyClusterTriggerEffectParams, Ptr ()] ()
triggerEffectWithParams_completionHandlerSelector = mkSelector "triggerEffectWithParams:completionHandler:"

-- | @Selector@ for @readAttributeIdentifyTimeWithCompletionHandler:@
readAttributeIdentifyTimeWithCompletionHandlerSelector :: Selector '[Ptr ()] ()
readAttributeIdentifyTimeWithCompletionHandlerSelector = mkSelector "readAttributeIdentifyTimeWithCompletionHandler:"

-- | @Selector@ for @writeAttributeIdentifyTimeWithValue:completionHandler:@
writeAttributeIdentifyTimeWithValue_completionHandlerSelector :: Selector '[Id NSNumber, Ptr ()] ()
writeAttributeIdentifyTimeWithValue_completionHandlerSelector = mkSelector "writeAttributeIdentifyTimeWithValue:completionHandler:"

-- | @Selector@ for @writeAttributeIdentifyTimeWithValue:params:completionHandler:@
writeAttributeIdentifyTimeWithValue_params_completionHandlerSelector :: Selector '[Id NSNumber, Id MTRWriteParams, Ptr ()] ()
writeAttributeIdentifyTimeWithValue_params_completionHandlerSelector = mkSelector "writeAttributeIdentifyTimeWithValue:params:completionHandler:"

-- | @Selector@ for @subscribeAttributeIdentifyTimeWithMinInterval:maxInterval:params:subscriptionEstablished:reportHandler:@
subscribeAttributeIdentifyTimeWithMinInterval_maxInterval_params_subscriptionEstablished_reportHandlerSelector :: Selector '[Id NSNumber, Id NSNumber, Id MTRSubscribeParams, Ptr (), Ptr ()] ()
subscribeAttributeIdentifyTimeWithMinInterval_maxInterval_params_subscriptionEstablished_reportHandlerSelector = mkSelector "subscribeAttributeIdentifyTimeWithMinInterval:maxInterval:params:subscriptionEstablished:reportHandler:"

-- | @Selector@ for @readAttributeIdentifyTimeWithAttributeCache:endpoint:queue:completionHandler:@
readAttributeIdentifyTimeWithAttributeCache_endpoint_queue_completionHandlerSelector :: Selector '[Id MTRAttributeCacheContainer, Id NSNumber, Id NSObject, Ptr ()] ()
readAttributeIdentifyTimeWithAttributeCache_endpoint_queue_completionHandlerSelector = mkSelector "readAttributeIdentifyTimeWithAttributeCache:endpoint:queue:completionHandler:"

-- | @Selector@ for @readAttributeIdentifyTypeWithCompletionHandler:@
readAttributeIdentifyTypeWithCompletionHandlerSelector :: Selector '[Ptr ()] ()
readAttributeIdentifyTypeWithCompletionHandlerSelector = mkSelector "readAttributeIdentifyTypeWithCompletionHandler:"

-- | @Selector@ for @subscribeAttributeIdentifyTypeWithMinInterval:maxInterval:params:subscriptionEstablished:reportHandler:@
subscribeAttributeIdentifyTypeWithMinInterval_maxInterval_params_subscriptionEstablished_reportHandlerSelector :: Selector '[Id NSNumber, Id NSNumber, Id MTRSubscribeParams, Ptr (), Ptr ()] ()
subscribeAttributeIdentifyTypeWithMinInterval_maxInterval_params_subscriptionEstablished_reportHandlerSelector = mkSelector "subscribeAttributeIdentifyTypeWithMinInterval:maxInterval:params:subscriptionEstablished:reportHandler:"

-- | @Selector@ for @readAttributeIdentifyTypeWithAttributeCache:endpoint:queue:completionHandler:@
readAttributeIdentifyTypeWithAttributeCache_endpoint_queue_completionHandlerSelector :: Selector '[Id MTRAttributeCacheContainer, Id NSNumber, Id NSObject, Ptr ()] ()
readAttributeIdentifyTypeWithAttributeCache_endpoint_queue_completionHandlerSelector = mkSelector "readAttributeIdentifyTypeWithAttributeCache:endpoint:queue:completionHandler:"

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
initWithDevice_endpointID_queueSelector :: Selector '[Id MTRBaseDevice, Id NSNumber, Id NSObject] (Id MTRBaseClusterIdentify)
initWithDevice_endpointID_queueSelector = mkSelector "initWithDevice:endpointID:queue:"

