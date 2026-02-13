{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Cluster Sample MEI
--
-- The Sample MEI cluster showcases a cluster manufacturer extensions
--
-- Generated bindings for @MTRBaseClusterSampleMEI@.
module ObjC.Matter.MTRBaseClusterSampleMEI
  ( MTRBaseClusterSampleMEI
  , IsMTRBaseClusterSampleMEI(..)
  , pingWithParams_completion
  , pingWithCompletion
  , addArgumentsWithParams_completion
  , readAttributeFlipFlopWithCompletion
  , writeAttributeFlipFlopWithValue_completion
  , writeAttributeFlipFlopWithValue_params_completion
  , subscribeAttributeFlipFlopWithParams_subscriptionEstablished_reportHandler
  , readAttributeFlipFlopWithClusterStateCache_endpoint_queue_completion
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
  , addArgumentsWithParams_completionSelector
  , initSelector
  , initWithDevice_endpointID_queueSelector
  , newSelector
  , pingWithCompletionSelector
  , pingWithParams_completionSelector
  , readAttributeAcceptedCommandListWithClusterStateCache_endpoint_queue_completionSelector
  , readAttributeAcceptedCommandListWithCompletionSelector
  , readAttributeAttributeListWithClusterStateCache_endpoint_queue_completionSelector
  , readAttributeAttributeListWithCompletionSelector
  , readAttributeClusterRevisionWithClusterStateCache_endpoint_queue_completionSelector
  , readAttributeClusterRevisionWithCompletionSelector
  , readAttributeFeatureMapWithClusterStateCache_endpoint_queue_completionSelector
  , readAttributeFeatureMapWithCompletionSelector
  , readAttributeFlipFlopWithClusterStateCache_endpoint_queue_completionSelector
  , readAttributeFlipFlopWithCompletionSelector
  , readAttributeGeneratedCommandListWithClusterStateCache_endpoint_queue_completionSelector
  , readAttributeGeneratedCommandListWithCompletionSelector
  , subscribeAttributeAcceptedCommandListWithParams_subscriptionEstablished_reportHandlerSelector
  , subscribeAttributeAttributeListWithParams_subscriptionEstablished_reportHandlerSelector
  , subscribeAttributeClusterRevisionWithParams_subscriptionEstablished_reportHandlerSelector
  , subscribeAttributeFeatureMapWithParams_subscriptionEstablished_reportHandlerSelector
  , subscribeAttributeFlipFlopWithParams_subscriptionEstablished_reportHandlerSelector
  , subscribeAttributeGeneratedCommandListWithParams_subscriptionEstablished_reportHandlerSelector
  , writeAttributeFlipFlopWithValue_completionSelector
  , writeAttributeFlipFlopWithValue_params_completionSelector


  ) where

import Foreign.Ptr (Ptr, FunPtr)
import Foreign.C.Types

import ObjC.Runtime.Types
import ObjC.Runtime.Message (sendMessage, sendOwnedMessage, sendClassMessage, sendOwnedClassMessage)
import ObjC.Runtime.Selector (mkSelector)
import ObjC.Runtime.Class (getRequiredClass)

import ObjC.Matter.Internal.Classes
import ObjC.Foundation.Internal.Classes

-- | Command Ping
--
-- Simple command without any parameters and without a response.
--
-- ObjC selector: @- pingWithParams:completion:@
pingWithParams_completion :: (IsMTRBaseClusterSampleMEI mtrBaseClusterSampleMEI, IsMTRSampleMEIClusterPingParams params) => mtrBaseClusterSampleMEI -> params -> Ptr () -> IO ()
pingWithParams_completion mtrBaseClusterSampleMEI params completion =
  sendMessage mtrBaseClusterSampleMEI pingWithParams_completionSelector (toMTRSampleMEIClusterPingParams params) completion

-- | @- pingWithCompletion:@
pingWithCompletion :: IsMTRBaseClusterSampleMEI mtrBaseClusterSampleMEI => mtrBaseClusterSampleMEI -> Ptr () -> IO ()
pingWithCompletion mtrBaseClusterSampleMEI completion =
  sendMessage mtrBaseClusterSampleMEI pingWithCompletionSelector completion

-- | Command AddArguments
--
-- Command that takes two uint8 arguments and returns their sum.
--
-- ObjC selector: @- addArgumentsWithParams:completion:@
addArgumentsWithParams_completion :: (IsMTRBaseClusterSampleMEI mtrBaseClusterSampleMEI, IsMTRSampleMEIClusterAddArgumentsParams params) => mtrBaseClusterSampleMEI -> params -> Ptr () -> IO ()
addArgumentsWithParams_completion mtrBaseClusterSampleMEI params completion =
  sendMessage mtrBaseClusterSampleMEI addArgumentsWithParams_completionSelector (toMTRSampleMEIClusterAddArgumentsParams params) completion

-- | @- readAttributeFlipFlopWithCompletion:@
readAttributeFlipFlopWithCompletion :: IsMTRBaseClusterSampleMEI mtrBaseClusterSampleMEI => mtrBaseClusterSampleMEI -> Ptr () -> IO ()
readAttributeFlipFlopWithCompletion mtrBaseClusterSampleMEI completion =
  sendMessage mtrBaseClusterSampleMEI readAttributeFlipFlopWithCompletionSelector completion

-- | @- writeAttributeFlipFlopWithValue:completion:@
writeAttributeFlipFlopWithValue_completion :: (IsMTRBaseClusterSampleMEI mtrBaseClusterSampleMEI, IsNSNumber value) => mtrBaseClusterSampleMEI -> value -> Ptr () -> IO ()
writeAttributeFlipFlopWithValue_completion mtrBaseClusterSampleMEI value completion =
  sendMessage mtrBaseClusterSampleMEI writeAttributeFlipFlopWithValue_completionSelector (toNSNumber value) completion

-- | @- writeAttributeFlipFlopWithValue:params:completion:@
writeAttributeFlipFlopWithValue_params_completion :: (IsMTRBaseClusterSampleMEI mtrBaseClusterSampleMEI, IsNSNumber value, IsMTRWriteParams params) => mtrBaseClusterSampleMEI -> value -> params -> Ptr () -> IO ()
writeAttributeFlipFlopWithValue_params_completion mtrBaseClusterSampleMEI value params completion =
  sendMessage mtrBaseClusterSampleMEI writeAttributeFlipFlopWithValue_params_completionSelector (toNSNumber value) (toMTRWriteParams params) completion

-- | @- subscribeAttributeFlipFlopWithParams:subscriptionEstablished:reportHandler:@
subscribeAttributeFlipFlopWithParams_subscriptionEstablished_reportHandler :: (IsMTRBaseClusterSampleMEI mtrBaseClusterSampleMEI, IsMTRSubscribeParams params) => mtrBaseClusterSampleMEI -> params -> Ptr () -> Ptr () -> IO ()
subscribeAttributeFlipFlopWithParams_subscriptionEstablished_reportHandler mtrBaseClusterSampleMEI params subscriptionEstablished reportHandler =
  sendMessage mtrBaseClusterSampleMEI subscribeAttributeFlipFlopWithParams_subscriptionEstablished_reportHandlerSelector (toMTRSubscribeParams params) subscriptionEstablished reportHandler

-- | @+ readAttributeFlipFlopWithClusterStateCache:endpoint:queue:completion:@
readAttributeFlipFlopWithClusterStateCache_endpoint_queue_completion :: (IsMTRClusterStateCacheContainer clusterStateCacheContainer, IsNSNumber endpoint, IsNSObject queue) => clusterStateCacheContainer -> endpoint -> queue -> Ptr () -> IO ()
readAttributeFlipFlopWithClusterStateCache_endpoint_queue_completion clusterStateCacheContainer endpoint queue completion =
  do
    cls' <- getRequiredClass "MTRBaseClusterSampleMEI"
    sendClassMessage cls' readAttributeFlipFlopWithClusterStateCache_endpoint_queue_completionSelector (toMTRClusterStateCacheContainer clusterStateCacheContainer) (toNSNumber endpoint) (toNSObject queue) completion

-- | @- readAttributeGeneratedCommandListWithCompletion:@
readAttributeGeneratedCommandListWithCompletion :: IsMTRBaseClusterSampleMEI mtrBaseClusterSampleMEI => mtrBaseClusterSampleMEI -> Ptr () -> IO ()
readAttributeGeneratedCommandListWithCompletion mtrBaseClusterSampleMEI completion =
  sendMessage mtrBaseClusterSampleMEI readAttributeGeneratedCommandListWithCompletionSelector completion

-- | @- subscribeAttributeGeneratedCommandListWithParams:subscriptionEstablished:reportHandler:@
subscribeAttributeGeneratedCommandListWithParams_subscriptionEstablished_reportHandler :: (IsMTRBaseClusterSampleMEI mtrBaseClusterSampleMEI, IsMTRSubscribeParams params) => mtrBaseClusterSampleMEI -> params -> Ptr () -> Ptr () -> IO ()
subscribeAttributeGeneratedCommandListWithParams_subscriptionEstablished_reportHandler mtrBaseClusterSampleMEI params subscriptionEstablished reportHandler =
  sendMessage mtrBaseClusterSampleMEI subscribeAttributeGeneratedCommandListWithParams_subscriptionEstablished_reportHandlerSelector (toMTRSubscribeParams params) subscriptionEstablished reportHandler

-- | @+ readAttributeGeneratedCommandListWithClusterStateCache:endpoint:queue:completion:@
readAttributeGeneratedCommandListWithClusterStateCache_endpoint_queue_completion :: (IsMTRClusterStateCacheContainer clusterStateCacheContainer, IsNSNumber endpoint, IsNSObject queue) => clusterStateCacheContainer -> endpoint -> queue -> Ptr () -> IO ()
readAttributeGeneratedCommandListWithClusterStateCache_endpoint_queue_completion clusterStateCacheContainer endpoint queue completion =
  do
    cls' <- getRequiredClass "MTRBaseClusterSampleMEI"
    sendClassMessage cls' readAttributeGeneratedCommandListWithClusterStateCache_endpoint_queue_completionSelector (toMTRClusterStateCacheContainer clusterStateCacheContainer) (toNSNumber endpoint) (toNSObject queue) completion

-- | @- readAttributeAcceptedCommandListWithCompletion:@
readAttributeAcceptedCommandListWithCompletion :: IsMTRBaseClusterSampleMEI mtrBaseClusterSampleMEI => mtrBaseClusterSampleMEI -> Ptr () -> IO ()
readAttributeAcceptedCommandListWithCompletion mtrBaseClusterSampleMEI completion =
  sendMessage mtrBaseClusterSampleMEI readAttributeAcceptedCommandListWithCompletionSelector completion

-- | @- subscribeAttributeAcceptedCommandListWithParams:subscriptionEstablished:reportHandler:@
subscribeAttributeAcceptedCommandListWithParams_subscriptionEstablished_reportHandler :: (IsMTRBaseClusterSampleMEI mtrBaseClusterSampleMEI, IsMTRSubscribeParams params) => mtrBaseClusterSampleMEI -> params -> Ptr () -> Ptr () -> IO ()
subscribeAttributeAcceptedCommandListWithParams_subscriptionEstablished_reportHandler mtrBaseClusterSampleMEI params subscriptionEstablished reportHandler =
  sendMessage mtrBaseClusterSampleMEI subscribeAttributeAcceptedCommandListWithParams_subscriptionEstablished_reportHandlerSelector (toMTRSubscribeParams params) subscriptionEstablished reportHandler

-- | @+ readAttributeAcceptedCommandListWithClusterStateCache:endpoint:queue:completion:@
readAttributeAcceptedCommandListWithClusterStateCache_endpoint_queue_completion :: (IsMTRClusterStateCacheContainer clusterStateCacheContainer, IsNSNumber endpoint, IsNSObject queue) => clusterStateCacheContainer -> endpoint -> queue -> Ptr () -> IO ()
readAttributeAcceptedCommandListWithClusterStateCache_endpoint_queue_completion clusterStateCacheContainer endpoint queue completion =
  do
    cls' <- getRequiredClass "MTRBaseClusterSampleMEI"
    sendClassMessage cls' readAttributeAcceptedCommandListWithClusterStateCache_endpoint_queue_completionSelector (toMTRClusterStateCacheContainer clusterStateCacheContainer) (toNSNumber endpoint) (toNSObject queue) completion

-- | @- readAttributeAttributeListWithCompletion:@
readAttributeAttributeListWithCompletion :: IsMTRBaseClusterSampleMEI mtrBaseClusterSampleMEI => mtrBaseClusterSampleMEI -> Ptr () -> IO ()
readAttributeAttributeListWithCompletion mtrBaseClusterSampleMEI completion =
  sendMessage mtrBaseClusterSampleMEI readAttributeAttributeListWithCompletionSelector completion

-- | @- subscribeAttributeAttributeListWithParams:subscriptionEstablished:reportHandler:@
subscribeAttributeAttributeListWithParams_subscriptionEstablished_reportHandler :: (IsMTRBaseClusterSampleMEI mtrBaseClusterSampleMEI, IsMTRSubscribeParams params) => mtrBaseClusterSampleMEI -> params -> Ptr () -> Ptr () -> IO ()
subscribeAttributeAttributeListWithParams_subscriptionEstablished_reportHandler mtrBaseClusterSampleMEI params subscriptionEstablished reportHandler =
  sendMessage mtrBaseClusterSampleMEI subscribeAttributeAttributeListWithParams_subscriptionEstablished_reportHandlerSelector (toMTRSubscribeParams params) subscriptionEstablished reportHandler

-- | @+ readAttributeAttributeListWithClusterStateCache:endpoint:queue:completion:@
readAttributeAttributeListWithClusterStateCache_endpoint_queue_completion :: (IsMTRClusterStateCacheContainer clusterStateCacheContainer, IsNSNumber endpoint, IsNSObject queue) => clusterStateCacheContainer -> endpoint -> queue -> Ptr () -> IO ()
readAttributeAttributeListWithClusterStateCache_endpoint_queue_completion clusterStateCacheContainer endpoint queue completion =
  do
    cls' <- getRequiredClass "MTRBaseClusterSampleMEI"
    sendClassMessage cls' readAttributeAttributeListWithClusterStateCache_endpoint_queue_completionSelector (toMTRClusterStateCacheContainer clusterStateCacheContainer) (toNSNumber endpoint) (toNSObject queue) completion

-- | @- readAttributeFeatureMapWithCompletion:@
readAttributeFeatureMapWithCompletion :: IsMTRBaseClusterSampleMEI mtrBaseClusterSampleMEI => mtrBaseClusterSampleMEI -> Ptr () -> IO ()
readAttributeFeatureMapWithCompletion mtrBaseClusterSampleMEI completion =
  sendMessage mtrBaseClusterSampleMEI readAttributeFeatureMapWithCompletionSelector completion

-- | @- subscribeAttributeFeatureMapWithParams:subscriptionEstablished:reportHandler:@
subscribeAttributeFeatureMapWithParams_subscriptionEstablished_reportHandler :: (IsMTRBaseClusterSampleMEI mtrBaseClusterSampleMEI, IsMTRSubscribeParams params) => mtrBaseClusterSampleMEI -> params -> Ptr () -> Ptr () -> IO ()
subscribeAttributeFeatureMapWithParams_subscriptionEstablished_reportHandler mtrBaseClusterSampleMEI params subscriptionEstablished reportHandler =
  sendMessage mtrBaseClusterSampleMEI subscribeAttributeFeatureMapWithParams_subscriptionEstablished_reportHandlerSelector (toMTRSubscribeParams params) subscriptionEstablished reportHandler

-- | @+ readAttributeFeatureMapWithClusterStateCache:endpoint:queue:completion:@
readAttributeFeatureMapWithClusterStateCache_endpoint_queue_completion :: (IsMTRClusterStateCacheContainer clusterStateCacheContainer, IsNSNumber endpoint, IsNSObject queue) => clusterStateCacheContainer -> endpoint -> queue -> Ptr () -> IO ()
readAttributeFeatureMapWithClusterStateCache_endpoint_queue_completion clusterStateCacheContainer endpoint queue completion =
  do
    cls' <- getRequiredClass "MTRBaseClusterSampleMEI"
    sendClassMessage cls' readAttributeFeatureMapWithClusterStateCache_endpoint_queue_completionSelector (toMTRClusterStateCacheContainer clusterStateCacheContainer) (toNSNumber endpoint) (toNSObject queue) completion

-- | @- readAttributeClusterRevisionWithCompletion:@
readAttributeClusterRevisionWithCompletion :: IsMTRBaseClusterSampleMEI mtrBaseClusterSampleMEI => mtrBaseClusterSampleMEI -> Ptr () -> IO ()
readAttributeClusterRevisionWithCompletion mtrBaseClusterSampleMEI completion =
  sendMessage mtrBaseClusterSampleMEI readAttributeClusterRevisionWithCompletionSelector completion

-- | @- subscribeAttributeClusterRevisionWithParams:subscriptionEstablished:reportHandler:@
subscribeAttributeClusterRevisionWithParams_subscriptionEstablished_reportHandler :: (IsMTRBaseClusterSampleMEI mtrBaseClusterSampleMEI, IsMTRSubscribeParams params) => mtrBaseClusterSampleMEI -> params -> Ptr () -> Ptr () -> IO ()
subscribeAttributeClusterRevisionWithParams_subscriptionEstablished_reportHandler mtrBaseClusterSampleMEI params subscriptionEstablished reportHandler =
  sendMessage mtrBaseClusterSampleMEI subscribeAttributeClusterRevisionWithParams_subscriptionEstablished_reportHandlerSelector (toMTRSubscribeParams params) subscriptionEstablished reportHandler

-- | @+ readAttributeClusterRevisionWithClusterStateCache:endpoint:queue:completion:@
readAttributeClusterRevisionWithClusterStateCache_endpoint_queue_completion :: (IsMTRClusterStateCacheContainer clusterStateCacheContainer, IsNSNumber endpoint, IsNSObject queue) => clusterStateCacheContainer -> endpoint -> queue -> Ptr () -> IO ()
readAttributeClusterRevisionWithClusterStateCache_endpoint_queue_completion clusterStateCacheContainer endpoint queue completion =
  do
    cls' <- getRequiredClass "MTRBaseClusterSampleMEI"
    sendClassMessage cls' readAttributeClusterRevisionWithClusterStateCache_endpoint_queue_completionSelector (toMTRClusterStateCacheContainer clusterStateCacheContainer) (toNSNumber endpoint) (toNSObject queue) completion

-- | @- init@
init_ :: IsMTRBaseClusterSampleMEI mtrBaseClusterSampleMEI => mtrBaseClusterSampleMEI -> IO (Id MTRBaseClusterSampleMEI)
init_ mtrBaseClusterSampleMEI =
  sendOwnedMessage mtrBaseClusterSampleMEI initSelector

-- | @+ new@
new :: IO (Id MTRBaseClusterSampleMEI)
new  =
  do
    cls' <- getRequiredClass "MTRBaseClusterSampleMEI"
    sendOwnedClassMessage cls' newSelector

-- | For all instance methods (reads, writes, commands) that take a completion, the completion will be called on the provided queue.
--
-- ObjC selector: @- initWithDevice:endpointID:queue:@
initWithDevice_endpointID_queue :: (IsMTRBaseClusterSampleMEI mtrBaseClusterSampleMEI, IsMTRBaseDevice device, IsNSNumber endpointID, IsNSObject queue) => mtrBaseClusterSampleMEI -> device -> endpointID -> queue -> IO (Id MTRBaseClusterSampleMEI)
initWithDevice_endpointID_queue mtrBaseClusterSampleMEI device endpointID queue =
  sendOwnedMessage mtrBaseClusterSampleMEI initWithDevice_endpointID_queueSelector (toMTRBaseDevice device) (toNSNumber endpointID) (toNSObject queue)

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @pingWithParams:completion:@
pingWithParams_completionSelector :: Selector '[Id MTRSampleMEIClusterPingParams, Ptr ()] ()
pingWithParams_completionSelector = mkSelector "pingWithParams:completion:"

-- | @Selector@ for @pingWithCompletion:@
pingWithCompletionSelector :: Selector '[Ptr ()] ()
pingWithCompletionSelector = mkSelector "pingWithCompletion:"

-- | @Selector@ for @addArgumentsWithParams:completion:@
addArgumentsWithParams_completionSelector :: Selector '[Id MTRSampleMEIClusterAddArgumentsParams, Ptr ()] ()
addArgumentsWithParams_completionSelector = mkSelector "addArgumentsWithParams:completion:"

-- | @Selector@ for @readAttributeFlipFlopWithCompletion:@
readAttributeFlipFlopWithCompletionSelector :: Selector '[Ptr ()] ()
readAttributeFlipFlopWithCompletionSelector = mkSelector "readAttributeFlipFlopWithCompletion:"

-- | @Selector@ for @writeAttributeFlipFlopWithValue:completion:@
writeAttributeFlipFlopWithValue_completionSelector :: Selector '[Id NSNumber, Ptr ()] ()
writeAttributeFlipFlopWithValue_completionSelector = mkSelector "writeAttributeFlipFlopWithValue:completion:"

-- | @Selector@ for @writeAttributeFlipFlopWithValue:params:completion:@
writeAttributeFlipFlopWithValue_params_completionSelector :: Selector '[Id NSNumber, Id MTRWriteParams, Ptr ()] ()
writeAttributeFlipFlopWithValue_params_completionSelector = mkSelector "writeAttributeFlipFlopWithValue:params:completion:"

-- | @Selector@ for @subscribeAttributeFlipFlopWithParams:subscriptionEstablished:reportHandler:@
subscribeAttributeFlipFlopWithParams_subscriptionEstablished_reportHandlerSelector :: Selector '[Id MTRSubscribeParams, Ptr (), Ptr ()] ()
subscribeAttributeFlipFlopWithParams_subscriptionEstablished_reportHandlerSelector = mkSelector "subscribeAttributeFlipFlopWithParams:subscriptionEstablished:reportHandler:"

-- | @Selector@ for @readAttributeFlipFlopWithClusterStateCache:endpoint:queue:completion:@
readAttributeFlipFlopWithClusterStateCache_endpoint_queue_completionSelector :: Selector '[Id MTRClusterStateCacheContainer, Id NSNumber, Id NSObject, Ptr ()] ()
readAttributeFlipFlopWithClusterStateCache_endpoint_queue_completionSelector = mkSelector "readAttributeFlipFlopWithClusterStateCache:endpoint:queue:completion:"

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
initSelector :: Selector '[] (Id MTRBaseClusterSampleMEI)
initSelector = mkSelector "init"

-- | @Selector@ for @new@
newSelector :: Selector '[] (Id MTRBaseClusterSampleMEI)
newSelector = mkSelector "new"

-- | @Selector@ for @initWithDevice:endpointID:queue:@
initWithDevice_endpointID_queueSelector :: Selector '[Id MTRBaseDevice, Id NSNumber, Id NSObject] (Id MTRBaseClusterSampleMEI)
initWithDevice_endpointID_queueSelector = mkSelector "initWithDevice:endpointID:queue:"

