{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Cluster Electrical Grid Conditions
--
-- The Electrical Grid Conditions Cluster provides the mechanism for communicating electricity grid carbon intensity to devices within the premises in units of Grams of CO2e per kWh.
--
-- Generated bindings for @MTRBaseClusterElectricalGridConditions@.
module ObjC.Matter.MTRBaseClusterElectricalGridConditions
  ( MTRBaseClusterElectricalGridConditions
  , IsMTRBaseClusterElectricalGridConditions(..)
  , readAttributeLocalGenerationAvailableWithCompletion
  , writeAttributeLocalGenerationAvailableWithValue_completion
  , writeAttributeLocalGenerationAvailableWithValue_params_completion
  , subscribeAttributeLocalGenerationAvailableWithParams_subscriptionEstablished_reportHandler
  , readAttributeLocalGenerationAvailableWithClusterStateCache_endpoint_queue_completion
  , readAttributeCurrentConditionsWithCompletion
  , subscribeAttributeCurrentConditionsWithParams_subscriptionEstablished_reportHandler
  , readAttributeCurrentConditionsWithClusterStateCache_endpoint_queue_completion
  , readAttributeForecastConditionsWithCompletion
  , subscribeAttributeForecastConditionsWithParams_subscriptionEstablished_reportHandler
  , readAttributeForecastConditionsWithClusterStateCache_endpoint_queue_completion
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
  , readAttributeCurrentConditionsWithClusterStateCache_endpoint_queue_completionSelector
  , readAttributeCurrentConditionsWithCompletionSelector
  , readAttributeFeatureMapWithClusterStateCache_endpoint_queue_completionSelector
  , readAttributeFeatureMapWithCompletionSelector
  , readAttributeForecastConditionsWithClusterStateCache_endpoint_queue_completionSelector
  , readAttributeForecastConditionsWithCompletionSelector
  , readAttributeGeneratedCommandListWithClusterStateCache_endpoint_queue_completionSelector
  , readAttributeGeneratedCommandListWithCompletionSelector
  , readAttributeLocalGenerationAvailableWithClusterStateCache_endpoint_queue_completionSelector
  , readAttributeLocalGenerationAvailableWithCompletionSelector
  , subscribeAttributeAcceptedCommandListWithParams_subscriptionEstablished_reportHandlerSelector
  , subscribeAttributeAttributeListWithParams_subscriptionEstablished_reportHandlerSelector
  , subscribeAttributeClusterRevisionWithParams_subscriptionEstablished_reportHandlerSelector
  , subscribeAttributeCurrentConditionsWithParams_subscriptionEstablished_reportHandlerSelector
  , subscribeAttributeFeatureMapWithParams_subscriptionEstablished_reportHandlerSelector
  , subscribeAttributeForecastConditionsWithParams_subscriptionEstablished_reportHandlerSelector
  , subscribeAttributeGeneratedCommandListWithParams_subscriptionEstablished_reportHandlerSelector
  , subscribeAttributeLocalGenerationAvailableWithParams_subscriptionEstablished_reportHandlerSelector
  , writeAttributeLocalGenerationAvailableWithValue_completionSelector
  , writeAttributeLocalGenerationAvailableWithValue_params_completionSelector


  ) where

import Foreign.Ptr (Ptr, FunPtr)
import Foreign.C.Types

import ObjC.Runtime.Types
import ObjC.Runtime.Message (sendMessage, sendOwnedMessage, sendClassMessage, sendOwnedClassMessage)
import ObjC.Runtime.Selector (mkSelector)
import ObjC.Runtime.Class (getRequiredClass)

import ObjC.Matter.Internal.Classes
import ObjC.Foundation.Internal.Classes

-- | @- readAttributeLocalGenerationAvailableWithCompletion:@
readAttributeLocalGenerationAvailableWithCompletion :: IsMTRBaseClusterElectricalGridConditions mtrBaseClusterElectricalGridConditions => mtrBaseClusterElectricalGridConditions -> Ptr () -> IO ()
readAttributeLocalGenerationAvailableWithCompletion mtrBaseClusterElectricalGridConditions completion =
  sendMessage mtrBaseClusterElectricalGridConditions readAttributeLocalGenerationAvailableWithCompletionSelector completion

-- | @- writeAttributeLocalGenerationAvailableWithValue:completion:@
writeAttributeLocalGenerationAvailableWithValue_completion :: (IsMTRBaseClusterElectricalGridConditions mtrBaseClusterElectricalGridConditions, IsNSNumber value) => mtrBaseClusterElectricalGridConditions -> value -> Ptr () -> IO ()
writeAttributeLocalGenerationAvailableWithValue_completion mtrBaseClusterElectricalGridConditions value completion =
  sendMessage mtrBaseClusterElectricalGridConditions writeAttributeLocalGenerationAvailableWithValue_completionSelector (toNSNumber value) completion

-- | @- writeAttributeLocalGenerationAvailableWithValue:params:completion:@
writeAttributeLocalGenerationAvailableWithValue_params_completion :: (IsMTRBaseClusterElectricalGridConditions mtrBaseClusterElectricalGridConditions, IsNSNumber value, IsMTRWriteParams params) => mtrBaseClusterElectricalGridConditions -> value -> params -> Ptr () -> IO ()
writeAttributeLocalGenerationAvailableWithValue_params_completion mtrBaseClusterElectricalGridConditions value params completion =
  sendMessage mtrBaseClusterElectricalGridConditions writeAttributeLocalGenerationAvailableWithValue_params_completionSelector (toNSNumber value) (toMTRWriteParams params) completion

-- | @- subscribeAttributeLocalGenerationAvailableWithParams:subscriptionEstablished:reportHandler:@
subscribeAttributeLocalGenerationAvailableWithParams_subscriptionEstablished_reportHandler :: (IsMTRBaseClusterElectricalGridConditions mtrBaseClusterElectricalGridConditions, IsMTRSubscribeParams params) => mtrBaseClusterElectricalGridConditions -> params -> Ptr () -> Ptr () -> IO ()
subscribeAttributeLocalGenerationAvailableWithParams_subscriptionEstablished_reportHandler mtrBaseClusterElectricalGridConditions params subscriptionEstablished reportHandler =
  sendMessage mtrBaseClusterElectricalGridConditions subscribeAttributeLocalGenerationAvailableWithParams_subscriptionEstablished_reportHandlerSelector (toMTRSubscribeParams params) subscriptionEstablished reportHandler

-- | @+ readAttributeLocalGenerationAvailableWithClusterStateCache:endpoint:queue:completion:@
readAttributeLocalGenerationAvailableWithClusterStateCache_endpoint_queue_completion :: (IsMTRClusterStateCacheContainer clusterStateCacheContainer, IsNSNumber endpoint, IsNSObject queue) => clusterStateCacheContainer -> endpoint -> queue -> Ptr () -> IO ()
readAttributeLocalGenerationAvailableWithClusterStateCache_endpoint_queue_completion clusterStateCacheContainer endpoint queue completion =
  do
    cls' <- getRequiredClass "MTRBaseClusterElectricalGridConditions"
    sendClassMessage cls' readAttributeLocalGenerationAvailableWithClusterStateCache_endpoint_queue_completionSelector (toMTRClusterStateCacheContainer clusterStateCacheContainer) (toNSNumber endpoint) (toNSObject queue) completion

-- | @- readAttributeCurrentConditionsWithCompletion:@
readAttributeCurrentConditionsWithCompletion :: IsMTRBaseClusterElectricalGridConditions mtrBaseClusterElectricalGridConditions => mtrBaseClusterElectricalGridConditions -> Ptr () -> IO ()
readAttributeCurrentConditionsWithCompletion mtrBaseClusterElectricalGridConditions completion =
  sendMessage mtrBaseClusterElectricalGridConditions readAttributeCurrentConditionsWithCompletionSelector completion

-- | @- subscribeAttributeCurrentConditionsWithParams:subscriptionEstablished:reportHandler:@
subscribeAttributeCurrentConditionsWithParams_subscriptionEstablished_reportHandler :: (IsMTRBaseClusterElectricalGridConditions mtrBaseClusterElectricalGridConditions, IsMTRSubscribeParams params) => mtrBaseClusterElectricalGridConditions -> params -> Ptr () -> Ptr () -> IO ()
subscribeAttributeCurrentConditionsWithParams_subscriptionEstablished_reportHandler mtrBaseClusterElectricalGridConditions params subscriptionEstablished reportHandler =
  sendMessage mtrBaseClusterElectricalGridConditions subscribeAttributeCurrentConditionsWithParams_subscriptionEstablished_reportHandlerSelector (toMTRSubscribeParams params) subscriptionEstablished reportHandler

-- | @+ readAttributeCurrentConditionsWithClusterStateCache:endpoint:queue:completion:@
readAttributeCurrentConditionsWithClusterStateCache_endpoint_queue_completion :: (IsMTRClusterStateCacheContainer clusterStateCacheContainer, IsNSNumber endpoint, IsNSObject queue) => clusterStateCacheContainer -> endpoint -> queue -> Ptr () -> IO ()
readAttributeCurrentConditionsWithClusterStateCache_endpoint_queue_completion clusterStateCacheContainer endpoint queue completion =
  do
    cls' <- getRequiredClass "MTRBaseClusterElectricalGridConditions"
    sendClassMessage cls' readAttributeCurrentConditionsWithClusterStateCache_endpoint_queue_completionSelector (toMTRClusterStateCacheContainer clusterStateCacheContainer) (toNSNumber endpoint) (toNSObject queue) completion

-- | @- readAttributeForecastConditionsWithCompletion:@
readAttributeForecastConditionsWithCompletion :: IsMTRBaseClusterElectricalGridConditions mtrBaseClusterElectricalGridConditions => mtrBaseClusterElectricalGridConditions -> Ptr () -> IO ()
readAttributeForecastConditionsWithCompletion mtrBaseClusterElectricalGridConditions completion =
  sendMessage mtrBaseClusterElectricalGridConditions readAttributeForecastConditionsWithCompletionSelector completion

-- | @- subscribeAttributeForecastConditionsWithParams:subscriptionEstablished:reportHandler:@
subscribeAttributeForecastConditionsWithParams_subscriptionEstablished_reportHandler :: (IsMTRBaseClusterElectricalGridConditions mtrBaseClusterElectricalGridConditions, IsMTRSubscribeParams params) => mtrBaseClusterElectricalGridConditions -> params -> Ptr () -> Ptr () -> IO ()
subscribeAttributeForecastConditionsWithParams_subscriptionEstablished_reportHandler mtrBaseClusterElectricalGridConditions params subscriptionEstablished reportHandler =
  sendMessage mtrBaseClusterElectricalGridConditions subscribeAttributeForecastConditionsWithParams_subscriptionEstablished_reportHandlerSelector (toMTRSubscribeParams params) subscriptionEstablished reportHandler

-- | @+ readAttributeForecastConditionsWithClusterStateCache:endpoint:queue:completion:@
readAttributeForecastConditionsWithClusterStateCache_endpoint_queue_completion :: (IsMTRClusterStateCacheContainer clusterStateCacheContainer, IsNSNumber endpoint, IsNSObject queue) => clusterStateCacheContainer -> endpoint -> queue -> Ptr () -> IO ()
readAttributeForecastConditionsWithClusterStateCache_endpoint_queue_completion clusterStateCacheContainer endpoint queue completion =
  do
    cls' <- getRequiredClass "MTRBaseClusterElectricalGridConditions"
    sendClassMessage cls' readAttributeForecastConditionsWithClusterStateCache_endpoint_queue_completionSelector (toMTRClusterStateCacheContainer clusterStateCacheContainer) (toNSNumber endpoint) (toNSObject queue) completion

-- | @- readAttributeGeneratedCommandListWithCompletion:@
readAttributeGeneratedCommandListWithCompletion :: IsMTRBaseClusterElectricalGridConditions mtrBaseClusterElectricalGridConditions => mtrBaseClusterElectricalGridConditions -> Ptr () -> IO ()
readAttributeGeneratedCommandListWithCompletion mtrBaseClusterElectricalGridConditions completion =
  sendMessage mtrBaseClusterElectricalGridConditions readAttributeGeneratedCommandListWithCompletionSelector completion

-- | @- subscribeAttributeGeneratedCommandListWithParams:subscriptionEstablished:reportHandler:@
subscribeAttributeGeneratedCommandListWithParams_subscriptionEstablished_reportHandler :: (IsMTRBaseClusterElectricalGridConditions mtrBaseClusterElectricalGridConditions, IsMTRSubscribeParams params) => mtrBaseClusterElectricalGridConditions -> params -> Ptr () -> Ptr () -> IO ()
subscribeAttributeGeneratedCommandListWithParams_subscriptionEstablished_reportHandler mtrBaseClusterElectricalGridConditions params subscriptionEstablished reportHandler =
  sendMessage mtrBaseClusterElectricalGridConditions subscribeAttributeGeneratedCommandListWithParams_subscriptionEstablished_reportHandlerSelector (toMTRSubscribeParams params) subscriptionEstablished reportHandler

-- | @+ readAttributeGeneratedCommandListWithClusterStateCache:endpoint:queue:completion:@
readAttributeGeneratedCommandListWithClusterStateCache_endpoint_queue_completion :: (IsMTRClusterStateCacheContainer clusterStateCacheContainer, IsNSNumber endpoint, IsNSObject queue) => clusterStateCacheContainer -> endpoint -> queue -> Ptr () -> IO ()
readAttributeGeneratedCommandListWithClusterStateCache_endpoint_queue_completion clusterStateCacheContainer endpoint queue completion =
  do
    cls' <- getRequiredClass "MTRBaseClusterElectricalGridConditions"
    sendClassMessage cls' readAttributeGeneratedCommandListWithClusterStateCache_endpoint_queue_completionSelector (toMTRClusterStateCacheContainer clusterStateCacheContainer) (toNSNumber endpoint) (toNSObject queue) completion

-- | @- readAttributeAcceptedCommandListWithCompletion:@
readAttributeAcceptedCommandListWithCompletion :: IsMTRBaseClusterElectricalGridConditions mtrBaseClusterElectricalGridConditions => mtrBaseClusterElectricalGridConditions -> Ptr () -> IO ()
readAttributeAcceptedCommandListWithCompletion mtrBaseClusterElectricalGridConditions completion =
  sendMessage mtrBaseClusterElectricalGridConditions readAttributeAcceptedCommandListWithCompletionSelector completion

-- | @- subscribeAttributeAcceptedCommandListWithParams:subscriptionEstablished:reportHandler:@
subscribeAttributeAcceptedCommandListWithParams_subscriptionEstablished_reportHandler :: (IsMTRBaseClusterElectricalGridConditions mtrBaseClusterElectricalGridConditions, IsMTRSubscribeParams params) => mtrBaseClusterElectricalGridConditions -> params -> Ptr () -> Ptr () -> IO ()
subscribeAttributeAcceptedCommandListWithParams_subscriptionEstablished_reportHandler mtrBaseClusterElectricalGridConditions params subscriptionEstablished reportHandler =
  sendMessage mtrBaseClusterElectricalGridConditions subscribeAttributeAcceptedCommandListWithParams_subscriptionEstablished_reportHandlerSelector (toMTRSubscribeParams params) subscriptionEstablished reportHandler

-- | @+ readAttributeAcceptedCommandListWithClusterStateCache:endpoint:queue:completion:@
readAttributeAcceptedCommandListWithClusterStateCache_endpoint_queue_completion :: (IsMTRClusterStateCacheContainer clusterStateCacheContainer, IsNSNumber endpoint, IsNSObject queue) => clusterStateCacheContainer -> endpoint -> queue -> Ptr () -> IO ()
readAttributeAcceptedCommandListWithClusterStateCache_endpoint_queue_completion clusterStateCacheContainer endpoint queue completion =
  do
    cls' <- getRequiredClass "MTRBaseClusterElectricalGridConditions"
    sendClassMessage cls' readAttributeAcceptedCommandListWithClusterStateCache_endpoint_queue_completionSelector (toMTRClusterStateCacheContainer clusterStateCacheContainer) (toNSNumber endpoint) (toNSObject queue) completion

-- | @- readAttributeAttributeListWithCompletion:@
readAttributeAttributeListWithCompletion :: IsMTRBaseClusterElectricalGridConditions mtrBaseClusterElectricalGridConditions => mtrBaseClusterElectricalGridConditions -> Ptr () -> IO ()
readAttributeAttributeListWithCompletion mtrBaseClusterElectricalGridConditions completion =
  sendMessage mtrBaseClusterElectricalGridConditions readAttributeAttributeListWithCompletionSelector completion

-- | @- subscribeAttributeAttributeListWithParams:subscriptionEstablished:reportHandler:@
subscribeAttributeAttributeListWithParams_subscriptionEstablished_reportHandler :: (IsMTRBaseClusterElectricalGridConditions mtrBaseClusterElectricalGridConditions, IsMTRSubscribeParams params) => mtrBaseClusterElectricalGridConditions -> params -> Ptr () -> Ptr () -> IO ()
subscribeAttributeAttributeListWithParams_subscriptionEstablished_reportHandler mtrBaseClusterElectricalGridConditions params subscriptionEstablished reportHandler =
  sendMessage mtrBaseClusterElectricalGridConditions subscribeAttributeAttributeListWithParams_subscriptionEstablished_reportHandlerSelector (toMTRSubscribeParams params) subscriptionEstablished reportHandler

-- | @+ readAttributeAttributeListWithClusterStateCache:endpoint:queue:completion:@
readAttributeAttributeListWithClusterStateCache_endpoint_queue_completion :: (IsMTRClusterStateCacheContainer clusterStateCacheContainer, IsNSNumber endpoint, IsNSObject queue) => clusterStateCacheContainer -> endpoint -> queue -> Ptr () -> IO ()
readAttributeAttributeListWithClusterStateCache_endpoint_queue_completion clusterStateCacheContainer endpoint queue completion =
  do
    cls' <- getRequiredClass "MTRBaseClusterElectricalGridConditions"
    sendClassMessage cls' readAttributeAttributeListWithClusterStateCache_endpoint_queue_completionSelector (toMTRClusterStateCacheContainer clusterStateCacheContainer) (toNSNumber endpoint) (toNSObject queue) completion

-- | @- readAttributeFeatureMapWithCompletion:@
readAttributeFeatureMapWithCompletion :: IsMTRBaseClusterElectricalGridConditions mtrBaseClusterElectricalGridConditions => mtrBaseClusterElectricalGridConditions -> Ptr () -> IO ()
readAttributeFeatureMapWithCompletion mtrBaseClusterElectricalGridConditions completion =
  sendMessage mtrBaseClusterElectricalGridConditions readAttributeFeatureMapWithCompletionSelector completion

-- | @- subscribeAttributeFeatureMapWithParams:subscriptionEstablished:reportHandler:@
subscribeAttributeFeatureMapWithParams_subscriptionEstablished_reportHandler :: (IsMTRBaseClusterElectricalGridConditions mtrBaseClusterElectricalGridConditions, IsMTRSubscribeParams params) => mtrBaseClusterElectricalGridConditions -> params -> Ptr () -> Ptr () -> IO ()
subscribeAttributeFeatureMapWithParams_subscriptionEstablished_reportHandler mtrBaseClusterElectricalGridConditions params subscriptionEstablished reportHandler =
  sendMessage mtrBaseClusterElectricalGridConditions subscribeAttributeFeatureMapWithParams_subscriptionEstablished_reportHandlerSelector (toMTRSubscribeParams params) subscriptionEstablished reportHandler

-- | @+ readAttributeFeatureMapWithClusterStateCache:endpoint:queue:completion:@
readAttributeFeatureMapWithClusterStateCache_endpoint_queue_completion :: (IsMTRClusterStateCacheContainer clusterStateCacheContainer, IsNSNumber endpoint, IsNSObject queue) => clusterStateCacheContainer -> endpoint -> queue -> Ptr () -> IO ()
readAttributeFeatureMapWithClusterStateCache_endpoint_queue_completion clusterStateCacheContainer endpoint queue completion =
  do
    cls' <- getRequiredClass "MTRBaseClusterElectricalGridConditions"
    sendClassMessage cls' readAttributeFeatureMapWithClusterStateCache_endpoint_queue_completionSelector (toMTRClusterStateCacheContainer clusterStateCacheContainer) (toNSNumber endpoint) (toNSObject queue) completion

-- | @- readAttributeClusterRevisionWithCompletion:@
readAttributeClusterRevisionWithCompletion :: IsMTRBaseClusterElectricalGridConditions mtrBaseClusterElectricalGridConditions => mtrBaseClusterElectricalGridConditions -> Ptr () -> IO ()
readAttributeClusterRevisionWithCompletion mtrBaseClusterElectricalGridConditions completion =
  sendMessage mtrBaseClusterElectricalGridConditions readAttributeClusterRevisionWithCompletionSelector completion

-- | @- subscribeAttributeClusterRevisionWithParams:subscriptionEstablished:reportHandler:@
subscribeAttributeClusterRevisionWithParams_subscriptionEstablished_reportHandler :: (IsMTRBaseClusterElectricalGridConditions mtrBaseClusterElectricalGridConditions, IsMTRSubscribeParams params) => mtrBaseClusterElectricalGridConditions -> params -> Ptr () -> Ptr () -> IO ()
subscribeAttributeClusterRevisionWithParams_subscriptionEstablished_reportHandler mtrBaseClusterElectricalGridConditions params subscriptionEstablished reportHandler =
  sendMessage mtrBaseClusterElectricalGridConditions subscribeAttributeClusterRevisionWithParams_subscriptionEstablished_reportHandlerSelector (toMTRSubscribeParams params) subscriptionEstablished reportHandler

-- | @+ readAttributeClusterRevisionWithClusterStateCache:endpoint:queue:completion:@
readAttributeClusterRevisionWithClusterStateCache_endpoint_queue_completion :: (IsMTRClusterStateCacheContainer clusterStateCacheContainer, IsNSNumber endpoint, IsNSObject queue) => clusterStateCacheContainer -> endpoint -> queue -> Ptr () -> IO ()
readAttributeClusterRevisionWithClusterStateCache_endpoint_queue_completion clusterStateCacheContainer endpoint queue completion =
  do
    cls' <- getRequiredClass "MTRBaseClusterElectricalGridConditions"
    sendClassMessage cls' readAttributeClusterRevisionWithClusterStateCache_endpoint_queue_completionSelector (toMTRClusterStateCacheContainer clusterStateCacheContainer) (toNSNumber endpoint) (toNSObject queue) completion

-- | @- init@
init_ :: IsMTRBaseClusterElectricalGridConditions mtrBaseClusterElectricalGridConditions => mtrBaseClusterElectricalGridConditions -> IO (Id MTRBaseClusterElectricalGridConditions)
init_ mtrBaseClusterElectricalGridConditions =
  sendOwnedMessage mtrBaseClusterElectricalGridConditions initSelector

-- | @+ new@
new :: IO (Id MTRBaseClusterElectricalGridConditions)
new  =
  do
    cls' <- getRequiredClass "MTRBaseClusterElectricalGridConditions"
    sendOwnedClassMessage cls' newSelector

-- | For all instance methods (reads, writes, commands) that take a completion, the completion will be called on the provided queue.
--
-- ObjC selector: @- initWithDevice:endpointID:queue:@
initWithDevice_endpointID_queue :: (IsMTRBaseClusterElectricalGridConditions mtrBaseClusterElectricalGridConditions, IsMTRBaseDevice device, IsNSNumber endpointID, IsNSObject queue) => mtrBaseClusterElectricalGridConditions -> device -> endpointID -> queue -> IO (Id MTRBaseClusterElectricalGridConditions)
initWithDevice_endpointID_queue mtrBaseClusterElectricalGridConditions device endpointID queue =
  sendOwnedMessage mtrBaseClusterElectricalGridConditions initWithDevice_endpointID_queueSelector (toMTRBaseDevice device) (toNSNumber endpointID) (toNSObject queue)

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @readAttributeLocalGenerationAvailableWithCompletion:@
readAttributeLocalGenerationAvailableWithCompletionSelector :: Selector '[Ptr ()] ()
readAttributeLocalGenerationAvailableWithCompletionSelector = mkSelector "readAttributeLocalGenerationAvailableWithCompletion:"

-- | @Selector@ for @writeAttributeLocalGenerationAvailableWithValue:completion:@
writeAttributeLocalGenerationAvailableWithValue_completionSelector :: Selector '[Id NSNumber, Ptr ()] ()
writeAttributeLocalGenerationAvailableWithValue_completionSelector = mkSelector "writeAttributeLocalGenerationAvailableWithValue:completion:"

-- | @Selector@ for @writeAttributeLocalGenerationAvailableWithValue:params:completion:@
writeAttributeLocalGenerationAvailableWithValue_params_completionSelector :: Selector '[Id NSNumber, Id MTRWriteParams, Ptr ()] ()
writeAttributeLocalGenerationAvailableWithValue_params_completionSelector = mkSelector "writeAttributeLocalGenerationAvailableWithValue:params:completion:"

-- | @Selector@ for @subscribeAttributeLocalGenerationAvailableWithParams:subscriptionEstablished:reportHandler:@
subscribeAttributeLocalGenerationAvailableWithParams_subscriptionEstablished_reportHandlerSelector :: Selector '[Id MTRSubscribeParams, Ptr (), Ptr ()] ()
subscribeAttributeLocalGenerationAvailableWithParams_subscriptionEstablished_reportHandlerSelector = mkSelector "subscribeAttributeLocalGenerationAvailableWithParams:subscriptionEstablished:reportHandler:"

-- | @Selector@ for @readAttributeLocalGenerationAvailableWithClusterStateCache:endpoint:queue:completion:@
readAttributeLocalGenerationAvailableWithClusterStateCache_endpoint_queue_completionSelector :: Selector '[Id MTRClusterStateCacheContainer, Id NSNumber, Id NSObject, Ptr ()] ()
readAttributeLocalGenerationAvailableWithClusterStateCache_endpoint_queue_completionSelector = mkSelector "readAttributeLocalGenerationAvailableWithClusterStateCache:endpoint:queue:completion:"

-- | @Selector@ for @readAttributeCurrentConditionsWithCompletion:@
readAttributeCurrentConditionsWithCompletionSelector :: Selector '[Ptr ()] ()
readAttributeCurrentConditionsWithCompletionSelector = mkSelector "readAttributeCurrentConditionsWithCompletion:"

-- | @Selector@ for @subscribeAttributeCurrentConditionsWithParams:subscriptionEstablished:reportHandler:@
subscribeAttributeCurrentConditionsWithParams_subscriptionEstablished_reportHandlerSelector :: Selector '[Id MTRSubscribeParams, Ptr (), Ptr ()] ()
subscribeAttributeCurrentConditionsWithParams_subscriptionEstablished_reportHandlerSelector = mkSelector "subscribeAttributeCurrentConditionsWithParams:subscriptionEstablished:reportHandler:"

-- | @Selector@ for @readAttributeCurrentConditionsWithClusterStateCache:endpoint:queue:completion:@
readAttributeCurrentConditionsWithClusterStateCache_endpoint_queue_completionSelector :: Selector '[Id MTRClusterStateCacheContainer, Id NSNumber, Id NSObject, Ptr ()] ()
readAttributeCurrentConditionsWithClusterStateCache_endpoint_queue_completionSelector = mkSelector "readAttributeCurrentConditionsWithClusterStateCache:endpoint:queue:completion:"

-- | @Selector@ for @readAttributeForecastConditionsWithCompletion:@
readAttributeForecastConditionsWithCompletionSelector :: Selector '[Ptr ()] ()
readAttributeForecastConditionsWithCompletionSelector = mkSelector "readAttributeForecastConditionsWithCompletion:"

-- | @Selector@ for @subscribeAttributeForecastConditionsWithParams:subscriptionEstablished:reportHandler:@
subscribeAttributeForecastConditionsWithParams_subscriptionEstablished_reportHandlerSelector :: Selector '[Id MTRSubscribeParams, Ptr (), Ptr ()] ()
subscribeAttributeForecastConditionsWithParams_subscriptionEstablished_reportHandlerSelector = mkSelector "subscribeAttributeForecastConditionsWithParams:subscriptionEstablished:reportHandler:"

-- | @Selector@ for @readAttributeForecastConditionsWithClusterStateCache:endpoint:queue:completion:@
readAttributeForecastConditionsWithClusterStateCache_endpoint_queue_completionSelector :: Selector '[Id MTRClusterStateCacheContainer, Id NSNumber, Id NSObject, Ptr ()] ()
readAttributeForecastConditionsWithClusterStateCache_endpoint_queue_completionSelector = mkSelector "readAttributeForecastConditionsWithClusterStateCache:endpoint:queue:completion:"

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
initSelector :: Selector '[] (Id MTRBaseClusterElectricalGridConditions)
initSelector = mkSelector "init"

-- | @Selector@ for @new@
newSelector :: Selector '[] (Id MTRBaseClusterElectricalGridConditions)
newSelector = mkSelector "new"

-- | @Selector@ for @initWithDevice:endpointID:queue:@
initWithDevice_endpointID_queueSelector :: Selector '[Id MTRBaseDevice, Id NSNumber, Id NSObject] (Id MTRBaseClusterElectricalGridConditions)
initWithDevice_endpointID_queueSelector = mkSelector "initWithDevice:endpointID:queue:"

