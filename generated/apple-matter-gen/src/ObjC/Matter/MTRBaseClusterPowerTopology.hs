{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Cluster Power Topology
--
-- The Power Topology Cluster provides a mechanism for expressing how power is flowing between endpoints.
--
-- Generated bindings for @MTRBaseClusterPowerTopology@.
module ObjC.Matter.MTRBaseClusterPowerTopology
  ( MTRBaseClusterPowerTopology
  , IsMTRBaseClusterPowerTopology(..)
  , readAttributeAvailableEndpointsWithCompletion
  , subscribeAttributeAvailableEndpointsWithParams_subscriptionEstablished_reportHandler
  , readAttributeAvailableEndpointsWithClusterStateCache_endpoint_queue_completion
  , readAttributeActiveEndpointsWithCompletion
  , subscribeAttributeActiveEndpointsWithParams_subscriptionEstablished_reportHandler
  , readAttributeActiveEndpointsWithClusterStateCache_endpoint_queue_completion
  , readAttributeElectricalCircuitNodesWithParams_completion
  , writeAttributeElectricalCircuitNodesWithValue_completion
  , writeAttributeElectricalCircuitNodesWithValue_params_completion
  , subscribeAttributeElectricalCircuitNodesWithParams_subscriptionEstablished_reportHandler
  , readAttributeElectricalCircuitNodesWithClusterStateCache_endpoint_queue_completion
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
  , readAttributeActiveEndpointsWithClusterStateCache_endpoint_queue_completionSelector
  , readAttributeActiveEndpointsWithCompletionSelector
  , readAttributeAttributeListWithClusterStateCache_endpoint_queue_completionSelector
  , readAttributeAttributeListWithCompletionSelector
  , readAttributeAvailableEndpointsWithClusterStateCache_endpoint_queue_completionSelector
  , readAttributeAvailableEndpointsWithCompletionSelector
  , readAttributeClusterRevisionWithClusterStateCache_endpoint_queue_completionSelector
  , readAttributeClusterRevisionWithCompletionSelector
  , readAttributeElectricalCircuitNodesWithClusterStateCache_endpoint_queue_completionSelector
  , readAttributeElectricalCircuitNodesWithParams_completionSelector
  , readAttributeFeatureMapWithClusterStateCache_endpoint_queue_completionSelector
  , readAttributeFeatureMapWithCompletionSelector
  , readAttributeGeneratedCommandListWithClusterStateCache_endpoint_queue_completionSelector
  , readAttributeGeneratedCommandListWithCompletionSelector
  , subscribeAttributeAcceptedCommandListWithParams_subscriptionEstablished_reportHandlerSelector
  , subscribeAttributeActiveEndpointsWithParams_subscriptionEstablished_reportHandlerSelector
  , subscribeAttributeAttributeListWithParams_subscriptionEstablished_reportHandlerSelector
  , subscribeAttributeAvailableEndpointsWithParams_subscriptionEstablished_reportHandlerSelector
  , subscribeAttributeClusterRevisionWithParams_subscriptionEstablished_reportHandlerSelector
  , subscribeAttributeElectricalCircuitNodesWithParams_subscriptionEstablished_reportHandlerSelector
  , subscribeAttributeFeatureMapWithParams_subscriptionEstablished_reportHandlerSelector
  , subscribeAttributeGeneratedCommandListWithParams_subscriptionEstablished_reportHandlerSelector
  , writeAttributeElectricalCircuitNodesWithValue_completionSelector
  , writeAttributeElectricalCircuitNodesWithValue_params_completionSelector


  ) where

import Foreign.Ptr (Ptr, FunPtr)
import Foreign.C.Types

import ObjC.Runtime.Types
import ObjC.Runtime.Message (sendMessage, sendOwnedMessage, sendClassMessage, sendOwnedClassMessage)
import ObjC.Runtime.Selector (mkSelector)
import ObjC.Runtime.Class (getRequiredClass)

import ObjC.Matter.Internal.Classes
import ObjC.Foundation.Internal.Classes

-- | @- readAttributeAvailableEndpointsWithCompletion:@
readAttributeAvailableEndpointsWithCompletion :: IsMTRBaseClusterPowerTopology mtrBaseClusterPowerTopology => mtrBaseClusterPowerTopology -> Ptr () -> IO ()
readAttributeAvailableEndpointsWithCompletion mtrBaseClusterPowerTopology completion =
  sendMessage mtrBaseClusterPowerTopology readAttributeAvailableEndpointsWithCompletionSelector completion

-- | @- subscribeAttributeAvailableEndpointsWithParams:subscriptionEstablished:reportHandler:@
subscribeAttributeAvailableEndpointsWithParams_subscriptionEstablished_reportHandler :: (IsMTRBaseClusterPowerTopology mtrBaseClusterPowerTopology, IsMTRSubscribeParams params) => mtrBaseClusterPowerTopology -> params -> Ptr () -> Ptr () -> IO ()
subscribeAttributeAvailableEndpointsWithParams_subscriptionEstablished_reportHandler mtrBaseClusterPowerTopology params subscriptionEstablished reportHandler =
  sendMessage mtrBaseClusterPowerTopology subscribeAttributeAvailableEndpointsWithParams_subscriptionEstablished_reportHandlerSelector (toMTRSubscribeParams params) subscriptionEstablished reportHandler

-- | @+ readAttributeAvailableEndpointsWithClusterStateCache:endpoint:queue:completion:@
readAttributeAvailableEndpointsWithClusterStateCache_endpoint_queue_completion :: (IsMTRClusterStateCacheContainer clusterStateCacheContainer, IsNSNumber endpoint, IsNSObject queue) => clusterStateCacheContainer -> endpoint -> queue -> Ptr () -> IO ()
readAttributeAvailableEndpointsWithClusterStateCache_endpoint_queue_completion clusterStateCacheContainer endpoint queue completion =
  do
    cls' <- getRequiredClass "MTRBaseClusterPowerTopology"
    sendClassMessage cls' readAttributeAvailableEndpointsWithClusterStateCache_endpoint_queue_completionSelector (toMTRClusterStateCacheContainer clusterStateCacheContainer) (toNSNumber endpoint) (toNSObject queue) completion

-- | @- readAttributeActiveEndpointsWithCompletion:@
readAttributeActiveEndpointsWithCompletion :: IsMTRBaseClusterPowerTopology mtrBaseClusterPowerTopology => mtrBaseClusterPowerTopology -> Ptr () -> IO ()
readAttributeActiveEndpointsWithCompletion mtrBaseClusterPowerTopology completion =
  sendMessage mtrBaseClusterPowerTopology readAttributeActiveEndpointsWithCompletionSelector completion

-- | @- subscribeAttributeActiveEndpointsWithParams:subscriptionEstablished:reportHandler:@
subscribeAttributeActiveEndpointsWithParams_subscriptionEstablished_reportHandler :: (IsMTRBaseClusterPowerTopology mtrBaseClusterPowerTopology, IsMTRSubscribeParams params) => mtrBaseClusterPowerTopology -> params -> Ptr () -> Ptr () -> IO ()
subscribeAttributeActiveEndpointsWithParams_subscriptionEstablished_reportHandler mtrBaseClusterPowerTopology params subscriptionEstablished reportHandler =
  sendMessage mtrBaseClusterPowerTopology subscribeAttributeActiveEndpointsWithParams_subscriptionEstablished_reportHandlerSelector (toMTRSubscribeParams params) subscriptionEstablished reportHandler

-- | @+ readAttributeActiveEndpointsWithClusterStateCache:endpoint:queue:completion:@
readAttributeActiveEndpointsWithClusterStateCache_endpoint_queue_completion :: (IsMTRClusterStateCacheContainer clusterStateCacheContainer, IsNSNumber endpoint, IsNSObject queue) => clusterStateCacheContainer -> endpoint -> queue -> Ptr () -> IO ()
readAttributeActiveEndpointsWithClusterStateCache_endpoint_queue_completion clusterStateCacheContainer endpoint queue completion =
  do
    cls' <- getRequiredClass "MTRBaseClusterPowerTopology"
    sendClassMessage cls' readAttributeActiveEndpointsWithClusterStateCache_endpoint_queue_completionSelector (toMTRClusterStateCacheContainer clusterStateCacheContainer) (toNSNumber endpoint) (toNSObject queue) completion

-- | @- readAttributeElectricalCircuitNodesWithParams:completion:@
readAttributeElectricalCircuitNodesWithParams_completion :: (IsMTRBaseClusterPowerTopology mtrBaseClusterPowerTopology, IsMTRReadParams params) => mtrBaseClusterPowerTopology -> params -> Ptr () -> IO ()
readAttributeElectricalCircuitNodesWithParams_completion mtrBaseClusterPowerTopology params completion =
  sendMessage mtrBaseClusterPowerTopology readAttributeElectricalCircuitNodesWithParams_completionSelector (toMTRReadParams params) completion

-- | @- writeAttributeElectricalCircuitNodesWithValue:completion:@
writeAttributeElectricalCircuitNodesWithValue_completion :: (IsMTRBaseClusterPowerTopology mtrBaseClusterPowerTopology, IsNSArray value) => mtrBaseClusterPowerTopology -> value -> Ptr () -> IO ()
writeAttributeElectricalCircuitNodesWithValue_completion mtrBaseClusterPowerTopology value completion =
  sendMessage mtrBaseClusterPowerTopology writeAttributeElectricalCircuitNodesWithValue_completionSelector (toNSArray value) completion

-- | @- writeAttributeElectricalCircuitNodesWithValue:params:completion:@
writeAttributeElectricalCircuitNodesWithValue_params_completion :: (IsMTRBaseClusterPowerTopology mtrBaseClusterPowerTopology, IsNSArray value, IsMTRWriteParams params) => mtrBaseClusterPowerTopology -> value -> params -> Ptr () -> IO ()
writeAttributeElectricalCircuitNodesWithValue_params_completion mtrBaseClusterPowerTopology value params completion =
  sendMessage mtrBaseClusterPowerTopology writeAttributeElectricalCircuitNodesWithValue_params_completionSelector (toNSArray value) (toMTRWriteParams params) completion

-- | @- subscribeAttributeElectricalCircuitNodesWithParams:subscriptionEstablished:reportHandler:@
subscribeAttributeElectricalCircuitNodesWithParams_subscriptionEstablished_reportHandler :: (IsMTRBaseClusterPowerTopology mtrBaseClusterPowerTopology, IsMTRSubscribeParams params) => mtrBaseClusterPowerTopology -> params -> Ptr () -> Ptr () -> IO ()
subscribeAttributeElectricalCircuitNodesWithParams_subscriptionEstablished_reportHandler mtrBaseClusterPowerTopology params subscriptionEstablished reportHandler =
  sendMessage mtrBaseClusterPowerTopology subscribeAttributeElectricalCircuitNodesWithParams_subscriptionEstablished_reportHandlerSelector (toMTRSubscribeParams params) subscriptionEstablished reportHandler

-- | @+ readAttributeElectricalCircuitNodesWithClusterStateCache:endpoint:queue:completion:@
readAttributeElectricalCircuitNodesWithClusterStateCache_endpoint_queue_completion :: (IsMTRClusterStateCacheContainer clusterStateCacheContainer, IsNSNumber endpoint, IsNSObject queue) => clusterStateCacheContainer -> endpoint -> queue -> Ptr () -> IO ()
readAttributeElectricalCircuitNodesWithClusterStateCache_endpoint_queue_completion clusterStateCacheContainer endpoint queue completion =
  do
    cls' <- getRequiredClass "MTRBaseClusterPowerTopology"
    sendClassMessage cls' readAttributeElectricalCircuitNodesWithClusterStateCache_endpoint_queue_completionSelector (toMTRClusterStateCacheContainer clusterStateCacheContainer) (toNSNumber endpoint) (toNSObject queue) completion

-- | @- readAttributeGeneratedCommandListWithCompletion:@
readAttributeGeneratedCommandListWithCompletion :: IsMTRBaseClusterPowerTopology mtrBaseClusterPowerTopology => mtrBaseClusterPowerTopology -> Ptr () -> IO ()
readAttributeGeneratedCommandListWithCompletion mtrBaseClusterPowerTopology completion =
  sendMessage mtrBaseClusterPowerTopology readAttributeGeneratedCommandListWithCompletionSelector completion

-- | @- subscribeAttributeGeneratedCommandListWithParams:subscriptionEstablished:reportHandler:@
subscribeAttributeGeneratedCommandListWithParams_subscriptionEstablished_reportHandler :: (IsMTRBaseClusterPowerTopology mtrBaseClusterPowerTopology, IsMTRSubscribeParams params) => mtrBaseClusterPowerTopology -> params -> Ptr () -> Ptr () -> IO ()
subscribeAttributeGeneratedCommandListWithParams_subscriptionEstablished_reportHandler mtrBaseClusterPowerTopology params subscriptionEstablished reportHandler =
  sendMessage mtrBaseClusterPowerTopology subscribeAttributeGeneratedCommandListWithParams_subscriptionEstablished_reportHandlerSelector (toMTRSubscribeParams params) subscriptionEstablished reportHandler

-- | @+ readAttributeGeneratedCommandListWithClusterStateCache:endpoint:queue:completion:@
readAttributeGeneratedCommandListWithClusterStateCache_endpoint_queue_completion :: (IsMTRClusterStateCacheContainer clusterStateCacheContainer, IsNSNumber endpoint, IsNSObject queue) => clusterStateCacheContainer -> endpoint -> queue -> Ptr () -> IO ()
readAttributeGeneratedCommandListWithClusterStateCache_endpoint_queue_completion clusterStateCacheContainer endpoint queue completion =
  do
    cls' <- getRequiredClass "MTRBaseClusterPowerTopology"
    sendClassMessage cls' readAttributeGeneratedCommandListWithClusterStateCache_endpoint_queue_completionSelector (toMTRClusterStateCacheContainer clusterStateCacheContainer) (toNSNumber endpoint) (toNSObject queue) completion

-- | @- readAttributeAcceptedCommandListWithCompletion:@
readAttributeAcceptedCommandListWithCompletion :: IsMTRBaseClusterPowerTopology mtrBaseClusterPowerTopology => mtrBaseClusterPowerTopology -> Ptr () -> IO ()
readAttributeAcceptedCommandListWithCompletion mtrBaseClusterPowerTopology completion =
  sendMessage mtrBaseClusterPowerTopology readAttributeAcceptedCommandListWithCompletionSelector completion

-- | @- subscribeAttributeAcceptedCommandListWithParams:subscriptionEstablished:reportHandler:@
subscribeAttributeAcceptedCommandListWithParams_subscriptionEstablished_reportHandler :: (IsMTRBaseClusterPowerTopology mtrBaseClusterPowerTopology, IsMTRSubscribeParams params) => mtrBaseClusterPowerTopology -> params -> Ptr () -> Ptr () -> IO ()
subscribeAttributeAcceptedCommandListWithParams_subscriptionEstablished_reportHandler mtrBaseClusterPowerTopology params subscriptionEstablished reportHandler =
  sendMessage mtrBaseClusterPowerTopology subscribeAttributeAcceptedCommandListWithParams_subscriptionEstablished_reportHandlerSelector (toMTRSubscribeParams params) subscriptionEstablished reportHandler

-- | @+ readAttributeAcceptedCommandListWithClusterStateCache:endpoint:queue:completion:@
readAttributeAcceptedCommandListWithClusterStateCache_endpoint_queue_completion :: (IsMTRClusterStateCacheContainer clusterStateCacheContainer, IsNSNumber endpoint, IsNSObject queue) => clusterStateCacheContainer -> endpoint -> queue -> Ptr () -> IO ()
readAttributeAcceptedCommandListWithClusterStateCache_endpoint_queue_completion clusterStateCacheContainer endpoint queue completion =
  do
    cls' <- getRequiredClass "MTRBaseClusterPowerTopology"
    sendClassMessage cls' readAttributeAcceptedCommandListWithClusterStateCache_endpoint_queue_completionSelector (toMTRClusterStateCacheContainer clusterStateCacheContainer) (toNSNumber endpoint) (toNSObject queue) completion

-- | @- readAttributeAttributeListWithCompletion:@
readAttributeAttributeListWithCompletion :: IsMTRBaseClusterPowerTopology mtrBaseClusterPowerTopology => mtrBaseClusterPowerTopology -> Ptr () -> IO ()
readAttributeAttributeListWithCompletion mtrBaseClusterPowerTopology completion =
  sendMessage mtrBaseClusterPowerTopology readAttributeAttributeListWithCompletionSelector completion

-- | @- subscribeAttributeAttributeListWithParams:subscriptionEstablished:reportHandler:@
subscribeAttributeAttributeListWithParams_subscriptionEstablished_reportHandler :: (IsMTRBaseClusterPowerTopology mtrBaseClusterPowerTopology, IsMTRSubscribeParams params) => mtrBaseClusterPowerTopology -> params -> Ptr () -> Ptr () -> IO ()
subscribeAttributeAttributeListWithParams_subscriptionEstablished_reportHandler mtrBaseClusterPowerTopology params subscriptionEstablished reportHandler =
  sendMessage mtrBaseClusterPowerTopology subscribeAttributeAttributeListWithParams_subscriptionEstablished_reportHandlerSelector (toMTRSubscribeParams params) subscriptionEstablished reportHandler

-- | @+ readAttributeAttributeListWithClusterStateCache:endpoint:queue:completion:@
readAttributeAttributeListWithClusterStateCache_endpoint_queue_completion :: (IsMTRClusterStateCacheContainer clusterStateCacheContainer, IsNSNumber endpoint, IsNSObject queue) => clusterStateCacheContainer -> endpoint -> queue -> Ptr () -> IO ()
readAttributeAttributeListWithClusterStateCache_endpoint_queue_completion clusterStateCacheContainer endpoint queue completion =
  do
    cls' <- getRequiredClass "MTRBaseClusterPowerTopology"
    sendClassMessage cls' readAttributeAttributeListWithClusterStateCache_endpoint_queue_completionSelector (toMTRClusterStateCacheContainer clusterStateCacheContainer) (toNSNumber endpoint) (toNSObject queue) completion

-- | @- readAttributeFeatureMapWithCompletion:@
readAttributeFeatureMapWithCompletion :: IsMTRBaseClusterPowerTopology mtrBaseClusterPowerTopology => mtrBaseClusterPowerTopology -> Ptr () -> IO ()
readAttributeFeatureMapWithCompletion mtrBaseClusterPowerTopology completion =
  sendMessage mtrBaseClusterPowerTopology readAttributeFeatureMapWithCompletionSelector completion

-- | @- subscribeAttributeFeatureMapWithParams:subscriptionEstablished:reportHandler:@
subscribeAttributeFeatureMapWithParams_subscriptionEstablished_reportHandler :: (IsMTRBaseClusterPowerTopology mtrBaseClusterPowerTopology, IsMTRSubscribeParams params) => mtrBaseClusterPowerTopology -> params -> Ptr () -> Ptr () -> IO ()
subscribeAttributeFeatureMapWithParams_subscriptionEstablished_reportHandler mtrBaseClusterPowerTopology params subscriptionEstablished reportHandler =
  sendMessage mtrBaseClusterPowerTopology subscribeAttributeFeatureMapWithParams_subscriptionEstablished_reportHandlerSelector (toMTRSubscribeParams params) subscriptionEstablished reportHandler

-- | @+ readAttributeFeatureMapWithClusterStateCache:endpoint:queue:completion:@
readAttributeFeatureMapWithClusterStateCache_endpoint_queue_completion :: (IsMTRClusterStateCacheContainer clusterStateCacheContainer, IsNSNumber endpoint, IsNSObject queue) => clusterStateCacheContainer -> endpoint -> queue -> Ptr () -> IO ()
readAttributeFeatureMapWithClusterStateCache_endpoint_queue_completion clusterStateCacheContainer endpoint queue completion =
  do
    cls' <- getRequiredClass "MTRBaseClusterPowerTopology"
    sendClassMessage cls' readAttributeFeatureMapWithClusterStateCache_endpoint_queue_completionSelector (toMTRClusterStateCacheContainer clusterStateCacheContainer) (toNSNumber endpoint) (toNSObject queue) completion

-- | @- readAttributeClusterRevisionWithCompletion:@
readAttributeClusterRevisionWithCompletion :: IsMTRBaseClusterPowerTopology mtrBaseClusterPowerTopology => mtrBaseClusterPowerTopology -> Ptr () -> IO ()
readAttributeClusterRevisionWithCompletion mtrBaseClusterPowerTopology completion =
  sendMessage mtrBaseClusterPowerTopology readAttributeClusterRevisionWithCompletionSelector completion

-- | @- subscribeAttributeClusterRevisionWithParams:subscriptionEstablished:reportHandler:@
subscribeAttributeClusterRevisionWithParams_subscriptionEstablished_reportHandler :: (IsMTRBaseClusterPowerTopology mtrBaseClusterPowerTopology, IsMTRSubscribeParams params) => mtrBaseClusterPowerTopology -> params -> Ptr () -> Ptr () -> IO ()
subscribeAttributeClusterRevisionWithParams_subscriptionEstablished_reportHandler mtrBaseClusterPowerTopology params subscriptionEstablished reportHandler =
  sendMessage mtrBaseClusterPowerTopology subscribeAttributeClusterRevisionWithParams_subscriptionEstablished_reportHandlerSelector (toMTRSubscribeParams params) subscriptionEstablished reportHandler

-- | @+ readAttributeClusterRevisionWithClusterStateCache:endpoint:queue:completion:@
readAttributeClusterRevisionWithClusterStateCache_endpoint_queue_completion :: (IsMTRClusterStateCacheContainer clusterStateCacheContainer, IsNSNumber endpoint, IsNSObject queue) => clusterStateCacheContainer -> endpoint -> queue -> Ptr () -> IO ()
readAttributeClusterRevisionWithClusterStateCache_endpoint_queue_completion clusterStateCacheContainer endpoint queue completion =
  do
    cls' <- getRequiredClass "MTRBaseClusterPowerTopology"
    sendClassMessage cls' readAttributeClusterRevisionWithClusterStateCache_endpoint_queue_completionSelector (toMTRClusterStateCacheContainer clusterStateCacheContainer) (toNSNumber endpoint) (toNSObject queue) completion

-- | @- init@
init_ :: IsMTRBaseClusterPowerTopology mtrBaseClusterPowerTopology => mtrBaseClusterPowerTopology -> IO (Id MTRBaseClusterPowerTopology)
init_ mtrBaseClusterPowerTopology =
  sendOwnedMessage mtrBaseClusterPowerTopology initSelector

-- | @+ new@
new :: IO (Id MTRBaseClusterPowerTopology)
new  =
  do
    cls' <- getRequiredClass "MTRBaseClusterPowerTopology"
    sendOwnedClassMessage cls' newSelector

-- | For all instance methods (reads, writes, commands) that take a completion, the completion will be called on the provided queue.
--
-- ObjC selector: @- initWithDevice:endpointID:queue:@
initWithDevice_endpointID_queue :: (IsMTRBaseClusterPowerTopology mtrBaseClusterPowerTopology, IsMTRBaseDevice device, IsNSNumber endpointID, IsNSObject queue) => mtrBaseClusterPowerTopology -> device -> endpointID -> queue -> IO (Id MTRBaseClusterPowerTopology)
initWithDevice_endpointID_queue mtrBaseClusterPowerTopology device endpointID queue =
  sendOwnedMessage mtrBaseClusterPowerTopology initWithDevice_endpointID_queueSelector (toMTRBaseDevice device) (toNSNumber endpointID) (toNSObject queue)

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @readAttributeAvailableEndpointsWithCompletion:@
readAttributeAvailableEndpointsWithCompletionSelector :: Selector '[Ptr ()] ()
readAttributeAvailableEndpointsWithCompletionSelector = mkSelector "readAttributeAvailableEndpointsWithCompletion:"

-- | @Selector@ for @subscribeAttributeAvailableEndpointsWithParams:subscriptionEstablished:reportHandler:@
subscribeAttributeAvailableEndpointsWithParams_subscriptionEstablished_reportHandlerSelector :: Selector '[Id MTRSubscribeParams, Ptr (), Ptr ()] ()
subscribeAttributeAvailableEndpointsWithParams_subscriptionEstablished_reportHandlerSelector = mkSelector "subscribeAttributeAvailableEndpointsWithParams:subscriptionEstablished:reportHandler:"

-- | @Selector@ for @readAttributeAvailableEndpointsWithClusterStateCache:endpoint:queue:completion:@
readAttributeAvailableEndpointsWithClusterStateCache_endpoint_queue_completionSelector :: Selector '[Id MTRClusterStateCacheContainer, Id NSNumber, Id NSObject, Ptr ()] ()
readAttributeAvailableEndpointsWithClusterStateCache_endpoint_queue_completionSelector = mkSelector "readAttributeAvailableEndpointsWithClusterStateCache:endpoint:queue:completion:"

-- | @Selector@ for @readAttributeActiveEndpointsWithCompletion:@
readAttributeActiveEndpointsWithCompletionSelector :: Selector '[Ptr ()] ()
readAttributeActiveEndpointsWithCompletionSelector = mkSelector "readAttributeActiveEndpointsWithCompletion:"

-- | @Selector@ for @subscribeAttributeActiveEndpointsWithParams:subscriptionEstablished:reportHandler:@
subscribeAttributeActiveEndpointsWithParams_subscriptionEstablished_reportHandlerSelector :: Selector '[Id MTRSubscribeParams, Ptr (), Ptr ()] ()
subscribeAttributeActiveEndpointsWithParams_subscriptionEstablished_reportHandlerSelector = mkSelector "subscribeAttributeActiveEndpointsWithParams:subscriptionEstablished:reportHandler:"

-- | @Selector@ for @readAttributeActiveEndpointsWithClusterStateCache:endpoint:queue:completion:@
readAttributeActiveEndpointsWithClusterStateCache_endpoint_queue_completionSelector :: Selector '[Id MTRClusterStateCacheContainer, Id NSNumber, Id NSObject, Ptr ()] ()
readAttributeActiveEndpointsWithClusterStateCache_endpoint_queue_completionSelector = mkSelector "readAttributeActiveEndpointsWithClusterStateCache:endpoint:queue:completion:"

-- | @Selector@ for @readAttributeElectricalCircuitNodesWithParams:completion:@
readAttributeElectricalCircuitNodesWithParams_completionSelector :: Selector '[Id MTRReadParams, Ptr ()] ()
readAttributeElectricalCircuitNodesWithParams_completionSelector = mkSelector "readAttributeElectricalCircuitNodesWithParams:completion:"

-- | @Selector@ for @writeAttributeElectricalCircuitNodesWithValue:completion:@
writeAttributeElectricalCircuitNodesWithValue_completionSelector :: Selector '[Id NSArray, Ptr ()] ()
writeAttributeElectricalCircuitNodesWithValue_completionSelector = mkSelector "writeAttributeElectricalCircuitNodesWithValue:completion:"

-- | @Selector@ for @writeAttributeElectricalCircuitNodesWithValue:params:completion:@
writeAttributeElectricalCircuitNodesWithValue_params_completionSelector :: Selector '[Id NSArray, Id MTRWriteParams, Ptr ()] ()
writeAttributeElectricalCircuitNodesWithValue_params_completionSelector = mkSelector "writeAttributeElectricalCircuitNodesWithValue:params:completion:"

-- | @Selector@ for @subscribeAttributeElectricalCircuitNodesWithParams:subscriptionEstablished:reportHandler:@
subscribeAttributeElectricalCircuitNodesWithParams_subscriptionEstablished_reportHandlerSelector :: Selector '[Id MTRSubscribeParams, Ptr (), Ptr ()] ()
subscribeAttributeElectricalCircuitNodesWithParams_subscriptionEstablished_reportHandlerSelector = mkSelector "subscribeAttributeElectricalCircuitNodesWithParams:subscriptionEstablished:reportHandler:"

-- | @Selector@ for @readAttributeElectricalCircuitNodesWithClusterStateCache:endpoint:queue:completion:@
readAttributeElectricalCircuitNodesWithClusterStateCache_endpoint_queue_completionSelector :: Selector '[Id MTRClusterStateCacheContainer, Id NSNumber, Id NSObject, Ptr ()] ()
readAttributeElectricalCircuitNodesWithClusterStateCache_endpoint_queue_completionSelector = mkSelector "readAttributeElectricalCircuitNodesWithClusterStateCache:endpoint:queue:completion:"

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
initSelector :: Selector '[] (Id MTRBaseClusterPowerTopology)
initSelector = mkSelector "init"

-- | @Selector@ for @new@
newSelector :: Selector '[] (Id MTRBaseClusterPowerTopology)
newSelector = mkSelector "new"

-- | @Selector@ for @initWithDevice:endpointID:queue:@
initWithDevice_endpointID_queueSelector :: Selector '[Id MTRBaseDevice, Id NSNumber, Id NSObject] (Id MTRBaseClusterPowerTopology)
initWithDevice_endpointID_queueSelector = mkSelector "initWithDevice:endpointID:queue:"

