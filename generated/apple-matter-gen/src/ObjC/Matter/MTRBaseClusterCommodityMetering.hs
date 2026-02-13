{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Cluster Commodity Metering
--
-- The Commodity Metering Cluster provides the mechanism for communicating commodity consumption information within a premises.
--
-- Generated bindings for @MTRBaseClusterCommodityMetering@.
module ObjC.Matter.MTRBaseClusterCommodityMetering
  ( MTRBaseClusterCommodityMetering
  , IsMTRBaseClusterCommodityMetering(..)
  , readAttributeMeteredQuantityWithCompletion
  , subscribeAttributeMeteredQuantityWithParams_subscriptionEstablished_reportHandler
  , readAttributeMeteredQuantityWithClusterStateCache_endpoint_queue_completion
  , readAttributeMeteredQuantityTimestampWithCompletion
  , subscribeAttributeMeteredQuantityTimestampWithParams_subscriptionEstablished_reportHandler
  , readAttributeMeteredQuantityTimestampWithClusterStateCache_endpoint_queue_completion
  , readAttributeTariffUnitWithCompletion
  , subscribeAttributeTariffUnitWithParams_subscriptionEstablished_reportHandler
  , readAttributeTariffUnitWithClusterStateCache_endpoint_queue_completion
  , readAttributeMaximumMeteredQuantitiesWithCompletion
  , subscribeAttributeMaximumMeteredQuantitiesWithParams_subscriptionEstablished_reportHandler
  , readAttributeMaximumMeteredQuantitiesWithClusterStateCache_endpoint_queue_completion
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
  , readAttributeMaximumMeteredQuantitiesWithClusterStateCache_endpoint_queue_completionSelector
  , readAttributeMaximumMeteredQuantitiesWithCompletionSelector
  , readAttributeMeteredQuantityTimestampWithClusterStateCache_endpoint_queue_completionSelector
  , readAttributeMeteredQuantityTimestampWithCompletionSelector
  , readAttributeMeteredQuantityWithClusterStateCache_endpoint_queue_completionSelector
  , readAttributeMeteredQuantityWithCompletionSelector
  , readAttributeTariffUnitWithClusterStateCache_endpoint_queue_completionSelector
  , readAttributeTariffUnitWithCompletionSelector
  , subscribeAttributeAcceptedCommandListWithParams_subscriptionEstablished_reportHandlerSelector
  , subscribeAttributeAttributeListWithParams_subscriptionEstablished_reportHandlerSelector
  , subscribeAttributeClusterRevisionWithParams_subscriptionEstablished_reportHandlerSelector
  , subscribeAttributeFeatureMapWithParams_subscriptionEstablished_reportHandlerSelector
  , subscribeAttributeGeneratedCommandListWithParams_subscriptionEstablished_reportHandlerSelector
  , subscribeAttributeMaximumMeteredQuantitiesWithParams_subscriptionEstablished_reportHandlerSelector
  , subscribeAttributeMeteredQuantityTimestampWithParams_subscriptionEstablished_reportHandlerSelector
  , subscribeAttributeMeteredQuantityWithParams_subscriptionEstablished_reportHandlerSelector
  , subscribeAttributeTariffUnitWithParams_subscriptionEstablished_reportHandlerSelector


  ) where

import Foreign.Ptr (Ptr, FunPtr)
import Foreign.C.Types

import ObjC.Runtime.Types
import ObjC.Runtime.Message (sendMessage, sendOwnedMessage, sendClassMessage, sendOwnedClassMessage)
import ObjC.Runtime.Selector (mkSelector)
import ObjC.Runtime.Class (getRequiredClass)

import ObjC.Matter.Internal.Classes
import ObjC.Foundation.Internal.Classes

-- | @- readAttributeMeteredQuantityWithCompletion:@
readAttributeMeteredQuantityWithCompletion :: IsMTRBaseClusterCommodityMetering mtrBaseClusterCommodityMetering => mtrBaseClusterCommodityMetering -> Ptr () -> IO ()
readAttributeMeteredQuantityWithCompletion mtrBaseClusterCommodityMetering completion =
  sendMessage mtrBaseClusterCommodityMetering readAttributeMeteredQuantityWithCompletionSelector completion

-- | @- subscribeAttributeMeteredQuantityWithParams:subscriptionEstablished:reportHandler:@
subscribeAttributeMeteredQuantityWithParams_subscriptionEstablished_reportHandler :: (IsMTRBaseClusterCommodityMetering mtrBaseClusterCommodityMetering, IsMTRSubscribeParams params) => mtrBaseClusterCommodityMetering -> params -> Ptr () -> Ptr () -> IO ()
subscribeAttributeMeteredQuantityWithParams_subscriptionEstablished_reportHandler mtrBaseClusterCommodityMetering params subscriptionEstablished reportHandler =
  sendMessage mtrBaseClusterCommodityMetering subscribeAttributeMeteredQuantityWithParams_subscriptionEstablished_reportHandlerSelector (toMTRSubscribeParams params) subscriptionEstablished reportHandler

-- | @+ readAttributeMeteredQuantityWithClusterStateCache:endpoint:queue:completion:@
readAttributeMeteredQuantityWithClusterStateCache_endpoint_queue_completion :: (IsMTRClusterStateCacheContainer clusterStateCacheContainer, IsNSNumber endpoint, IsNSObject queue) => clusterStateCacheContainer -> endpoint -> queue -> Ptr () -> IO ()
readAttributeMeteredQuantityWithClusterStateCache_endpoint_queue_completion clusterStateCacheContainer endpoint queue completion =
  do
    cls' <- getRequiredClass "MTRBaseClusterCommodityMetering"
    sendClassMessage cls' readAttributeMeteredQuantityWithClusterStateCache_endpoint_queue_completionSelector (toMTRClusterStateCacheContainer clusterStateCacheContainer) (toNSNumber endpoint) (toNSObject queue) completion

-- | @- readAttributeMeteredQuantityTimestampWithCompletion:@
readAttributeMeteredQuantityTimestampWithCompletion :: IsMTRBaseClusterCommodityMetering mtrBaseClusterCommodityMetering => mtrBaseClusterCommodityMetering -> Ptr () -> IO ()
readAttributeMeteredQuantityTimestampWithCompletion mtrBaseClusterCommodityMetering completion =
  sendMessage mtrBaseClusterCommodityMetering readAttributeMeteredQuantityTimestampWithCompletionSelector completion

-- | @- subscribeAttributeMeteredQuantityTimestampWithParams:subscriptionEstablished:reportHandler:@
subscribeAttributeMeteredQuantityTimestampWithParams_subscriptionEstablished_reportHandler :: (IsMTRBaseClusterCommodityMetering mtrBaseClusterCommodityMetering, IsMTRSubscribeParams params) => mtrBaseClusterCommodityMetering -> params -> Ptr () -> Ptr () -> IO ()
subscribeAttributeMeteredQuantityTimestampWithParams_subscriptionEstablished_reportHandler mtrBaseClusterCommodityMetering params subscriptionEstablished reportHandler =
  sendMessage mtrBaseClusterCommodityMetering subscribeAttributeMeteredQuantityTimestampWithParams_subscriptionEstablished_reportHandlerSelector (toMTRSubscribeParams params) subscriptionEstablished reportHandler

-- | @+ readAttributeMeteredQuantityTimestampWithClusterStateCache:endpoint:queue:completion:@
readAttributeMeteredQuantityTimestampWithClusterStateCache_endpoint_queue_completion :: (IsMTRClusterStateCacheContainer clusterStateCacheContainer, IsNSNumber endpoint, IsNSObject queue) => clusterStateCacheContainer -> endpoint -> queue -> Ptr () -> IO ()
readAttributeMeteredQuantityTimestampWithClusterStateCache_endpoint_queue_completion clusterStateCacheContainer endpoint queue completion =
  do
    cls' <- getRequiredClass "MTRBaseClusterCommodityMetering"
    sendClassMessage cls' readAttributeMeteredQuantityTimestampWithClusterStateCache_endpoint_queue_completionSelector (toMTRClusterStateCacheContainer clusterStateCacheContainer) (toNSNumber endpoint) (toNSObject queue) completion

-- | @- readAttributeTariffUnitWithCompletion:@
readAttributeTariffUnitWithCompletion :: IsMTRBaseClusterCommodityMetering mtrBaseClusterCommodityMetering => mtrBaseClusterCommodityMetering -> Ptr () -> IO ()
readAttributeTariffUnitWithCompletion mtrBaseClusterCommodityMetering completion =
  sendMessage mtrBaseClusterCommodityMetering readAttributeTariffUnitWithCompletionSelector completion

-- | @- subscribeAttributeTariffUnitWithParams:subscriptionEstablished:reportHandler:@
subscribeAttributeTariffUnitWithParams_subscriptionEstablished_reportHandler :: (IsMTRBaseClusterCommodityMetering mtrBaseClusterCommodityMetering, IsMTRSubscribeParams params) => mtrBaseClusterCommodityMetering -> params -> Ptr () -> Ptr () -> IO ()
subscribeAttributeTariffUnitWithParams_subscriptionEstablished_reportHandler mtrBaseClusterCommodityMetering params subscriptionEstablished reportHandler =
  sendMessage mtrBaseClusterCommodityMetering subscribeAttributeTariffUnitWithParams_subscriptionEstablished_reportHandlerSelector (toMTRSubscribeParams params) subscriptionEstablished reportHandler

-- | @+ readAttributeTariffUnitWithClusterStateCache:endpoint:queue:completion:@
readAttributeTariffUnitWithClusterStateCache_endpoint_queue_completion :: (IsMTRClusterStateCacheContainer clusterStateCacheContainer, IsNSNumber endpoint, IsNSObject queue) => clusterStateCacheContainer -> endpoint -> queue -> Ptr () -> IO ()
readAttributeTariffUnitWithClusterStateCache_endpoint_queue_completion clusterStateCacheContainer endpoint queue completion =
  do
    cls' <- getRequiredClass "MTRBaseClusterCommodityMetering"
    sendClassMessage cls' readAttributeTariffUnitWithClusterStateCache_endpoint_queue_completionSelector (toMTRClusterStateCacheContainer clusterStateCacheContainer) (toNSNumber endpoint) (toNSObject queue) completion

-- | @- readAttributeMaximumMeteredQuantitiesWithCompletion:@
readAttributeMaximumMeteredQuantitiesWithCompletion :: IsMTRBaseClusterCommodityMetering mtrBaseClusterCommodityMetering => mtrBaseClusterCommodityMetering -> Ptr () -> IO ()
readAttributeMaximumMeteredQuantitiesWithCompletion mtrBaseClusterCommodityMetering completion =
  sendMessage mtrBaseClusterCommodityMetering readAttributeMaximumMeteredQuantitiesWithCompletionSelector completion

-- | @- subscribeAttributeMaximumMeteredQuantitiesWithParams:subscriptionEstablished:reportHandler:@
subscribeAttributeMaximumMeteredQuantitiesWithParams_subscriptionEstablished_reportHandler :: (IsMTRBaseClusterCommodityMetering mtrBaseClusterCommodityMetering, IsMTRSubscribeParams params) => mtrBaseClusterCommodityMetering -> params -> Ptr () -> Ptr () -> IO ()
subscribeAttributeMaximumMeteredQuantitiesWithParams_subscriptionEstablished_reportHandler mtrBaseClusterCommodityMetering params subscriptionEstablished reportHandler =
  sendMessage mtrBaseClusterCommodityMetering subscribeAttributeMaximumMeteredQuantitiesWithParams_subscriptionEstablished_reportHandlerSelector (toMTRSubscribeParams params) subscriptionEstablished reportHandler

-- | @+ readAttributeMaximumMeteredQuantitiesWithClusterStateCache:endpoint:queue:completion:@
readAttributeMaximumMeteredQuantitiesWithClusterStateCache_endpoint_queue_completion :: (IsMTRClusterStateCacheContainer clusterStateCacheContainer, IsNSNumber endpoint, IsNSObject queue) => clusterStateCacheContainer -> endpoint -> queue -> Ptr () -> IO ()
readAttributeMaximumMeteredQuantitiesWithClusterStateCache_endpoint_queue_completion clusterStateCacheContainer endpoint queue completion =
  do
    cls' <- getRequiredClass "MTRBaseClusterCommodityMetering"
    sendClassMessage cls' readAttributeMaximumMeteredQuantitiesWithClusterStateCache_endpoint_queue_completionSelector (toMTRClusterStateCacheContainer clusterStateCacheContainer) (toNSNumber endpoint) (toNSObject queue) completion

-- | @- readAttributeGeneratedCommandListWithCompletion:@
readAttributeGeneratedCommandListWithCompletion :: IsMTRBaseClusterCommodityMetering mtrBaseClusterCommodityMetering => mtrBaseClusterCommodityMetering -> Ptr () -> IO ()
readAttributeGeneratedCommandListWithCompletion mtrBaseClusterCommodityMetering completion =
  sendMessage mtrBaseClusterCommodityMetering readAttributeGeneratedCommandListWithCompletionSelector completion

-- | @- subscribeAttributeGeneratedCommandListWithParams:subscriptionEstablished:reportHandler:@
subscribeAttributeGeneratedCommandListWithParams_subscriptionEstablished_reportHandler :: (IsMTRBaseClusterCommodityMetering mtrBaseClusterCommodityMetering, IsMTRSubscribeParams params) => mtrBaseClusterCommodityMetering -> params -> Ptr () -> Ptr () -> IO ()
subscribeAttributeGeneratedCommandListWithParams_subscriptionEstablished_reportHandler mtrBaseClusterCommodityMetering params subscriptionEstablished reportHandler =
  sendMessage mtrBaseClusterCommodityMetering subscribeAttributeGeneratedCommandListWithParams_subscriptionEstablished_reportHandlerSelector (toMTRSubscribeParams params) subscriptionEstablished reportHandler

-- | @+ readAttributeGeneratedCommandListWithClusterStateCache:endpoint:queue:completion:@
readAttributeGeneratedCommandListWithClusterStateCache_endpoint_queue_completion :: (IsMTRClusterStateCacheContainer clusterStateCacheContainer, IsNSNumber endpoint, IsNSObject queue) => clusterStateCacheContainer -> endpoint -> queue -> Ptr () -> IO ()
readAttributeGeneratedCommandListWithClusterStateCache_endpoint_queue_completion clusterStateCacheContainer endpoint queue completion =
  do
    cls' <- getRequiredClass "MTRBaseClusterCommodityMetering"
    sendClassMessage cls' readAttributeGeneratedCommandListWithClusterStateCache_endpoint_queue_completionSelector (toMTRClusterStateCacheContainer clusterStateCacheContainer) (toNSNumber endpoint) (toNSObject queue) completion

-- | @- readAttributeAcceptedCommandListWithCompletion:@
readAttributeAcceptedCommandListWithCompletion :: IsMTRBaseClusterCommodityMetering mtrBaseClusterCommodityMetering => mtrBaseClusterCommodityMetering -> Ptr () -> IO ()
readAttributeAcceptedCommandListWithCompletion mtrBaseClusterCommodityMetering completion =
  sendMessage mtrBaseClusterCommodityMetering readAttributeAcceptedCommandListWithCompletionSelector completion

-- | @- subscribeAttributeAcceptedCommandListWithParams:subscriptionEstablished:reportHandler:@
subscribeAttributeAcceptedCommandListWithParams_subscriptionEstablished_reportHandler :: (IsMTRBaseClusterCommodityMetering mtrBaseClusterCommodityMetering, IsMTRSubscribeParams params) => mtrBaseClusterCommodityMetering -> params -> Ptr () -> Ptr () -> IO ()
subscribeAttributeAcceptedCommandListWithParams_subscriptionEstablished_reportHandler mtrBaseClusterCommodityMetering params subscriptionEstablished reportHandler =
  sendMessage mtrBaseClusterCommodityMetering subscribeAttributeAcceptedCommandListWithParams_subscriptionEstablished_reportHandlerSelector (toMTRSubscribeParams params) subscriptionEstablished reportHandler

-- | @+ readAttributeAcceptedCommandListWithClusterStateCache:endpoint:queue:completion:@
readAttributeAcceptedCommandListWithClusterStateCache_endpoint_queue_completion :: (IsMTRClusterStateCacheContainer clusterStateCacheContainer, IsNSNumber endpoint, IsNSObject queue) => clusterStateCacheContainer -> endpoint -> queue -> Ptr () -> IO ()
readAttributeAcceptedCommandListWithClusterStateCache_endpoint_queue_completion clusterStateCacheContainer endpoint queue completion =
  do
    cls' <- getRequiredClass "MTRBaseClusterCommodityMetering"
    sendClassMessage cls' readAttributeAcceptedCommandListWithClusterStateCache_endpoint_queue_completionSelector (toMTRClusterStateCacheContainer clusterStateCacheContainer) (toNSNumber endpoint) (toNSObject queue) completion

-- | @- readAttributeAttributeListWithCompletion:@
readAttributeAttributeListWithCompletion :: IsMTRBaseClusterCommodityMetering mtrBaseClusterCommodityMetering => mtrBaseClusterCommodityMetering -> Ptr () -> IO ()
readAttributeAttributeListWithCompletion mtrBaseClusterCommodityMetering completion =
  sendMessage mtrBaseClusterCommodityMetering readAttributeAttributeListWithCompletionSelector completion

-- | @- subscribeAttributeAttributeListWithParams:subscriptionEstablished:reportHandler:@
subscribeAttributeAttributeListWithParams_subscriptionEstablished_reportHandler :: (IsMTRBaseClusterCommodityMetering mtrBaseClusterCommodityMetering, IsMTRSubscribeParams params) => mtrBaseClusterCommodityMetering -> params -> Ptr () -> Ptr () -> IO ()
subscribeAttributeAttributeListWithParams_subscriptionEstablished_reportHandler mtrBaseClusterCommodityMetering params subscriptionEstablished reportHandler =
  sendMessage mtrBaseClusterCommodityMetering subscribeAttributeAttributeListWithParams_subscriptionEstablished_reportHandlerSelector (toMTRSubscribeParams params) subscriptionEstablished reportHandler

-- | @+ readAttributeAttributeListWithClusterStateCache:endpoint:queue:completion:@
readAttributeAttributeListWithClusterStateCache_endpoint_queue_completion :: (IsMTRClusterStateCacheContainer clusterStateCacheContainer, IsNSNumber endpoint, IsNSObject queue) => clusterStateCacheContainer -> endpoint -> queue -> Ptr () -> IO ()
readAttributeAttributeListWithClusterStateCache_endpoint_queue_completion clusterStateCacheContainer endpoint queue completion =
  do
    cls' <- getRequiredClass "MTRBaseClusterCommodityMetering"
    sendClassMessage cls' readAttributeAttributeListWithClusterStateCache_endpoint_queue_completionSelector (toMTRClusterStateCacheContainer clusterStateCacheContainer) (toNSNumber endpoint) (toNSObject queue) completion

-- | @- readAttributeFeatureMapWithCompletion:@
readAttributeFeatureMapWithCompletion :: IsMTRBaseClusterCommodityMetering mtrBaseClusterCommodityMetering => mtrBaseClusterCommodityMetering -> Ptr () -> IO ()
readAttributeFeatureMapWithCompletion mtrBaseClusterCommodityMetering completion =
  sendMessage mtrBaseClusterCommodityMetering readAttributeFeatureMapWithCompletionSelector completion

-- | @- subscribeAttributeFeatureMapWithParams:subscriptionEstablished:reportHandler:@
subscribeAttributeFeatureMapWithParams_subscriptionEstablished_reportHandler :: (IsMTRBaseClusterCommodityMetering mtrBaseClusterCommodityMetering, IsMTRSubscribeParams params) => mtrBaseClusterCommodityMetering -> params -> Ptr () -> Ptr () -> IO ()
subscribeAttributeFeatureMapWithParams_subscriptionEstablished_reportHandler mtrBaseClusterCommodityMetering params subscriptionEstablished reportHandler =
  sendMessage mtrBaseClusterCommodityMetering subscribeAttributeFeatureMapWithParams_subscriptionEstablished_reportHandlerSelector (toMTRSubscribeParams params) subscriptionEstablished reportHandler

-- | @+ readAttributeFeatureMapWithClusterStateCache:endpoint:queue:completion:@
readAttributeFeatureMapWithClusterStateCache_endpoint_queue_completion :: (IsMTRClusterStateCacheContainer clusterStateCacheContainer, IsNSNumber endpoint, IsNSObject queue) => clusterStateCacheContainer -> endpoint -> queue -> Ptr () -> IO ()
readAttributeFeatureMapWithClusterStateCache_endpoint_queue_completion clusterStateCacheContainer endpoint queue completion =
  do
    cls' <- getRequiredClass "MTRBaseClusterCommodityMetering"
    sendClassMessage cls' readAttributeFeatureMapWithClusterStateCache_endpoint_queue_completionSelector (toMTRClusterStateCacheContainer clusterStateCacheContainer) (toNSNumber endpoint) (toNSObject queue) completion

-- | @- readAttributeClusterRevisionWithCompletion:@
readAttributeClusterRevisionWithCompletion :: IsMTRBaseClusterCommodityMetering mtrBaseClusterCommodityMetering => mtrBaseClusterCommodityMetering -> Ptr () -> IO ()
readAttributeClusterRevisionWithCompletion mtrBaseClusterCommodityMetering completion =
  sendMessage mtrBaseClusterCommodityMetering readAttributeClusterRevisionWithCompletionSelector completion

-- | @- subscribeAttributeClusterRevisionWithParams:subscriptionEstablished:reportHandler:@
subscribeAttributeClusterRevisionWithParams_subscriptionEstablished_reportHandler :: (IsMTRBaseClusterCommodityMetering mtrBaseClusterCommodityMetering, IsMTRSubscribeParams params) => mtrBaseClusterCommodityMetering -> params -> Ptr () -> Ptr () -> IO ()
subscribeAttributeClusterRevisionWithParams_subscriptionEstablished_reportHandler mtrBaseClusterCommodityMetering params subscriptionEstablished reportHandler =
  sendMessage mtrBaseClusterCommodityMetering subscribeAttributeClusterRevisionWithParams_subscriptionEstablished_reportHandlerSelector (toMTRSubscribeParams params) subscriptionEstablished reportHandler

-- | @+ readAttributeClusterRevisionWithClusterStateCache:endpoint:queue:completion:@
readAttributeClusterRevisionWithClusterStateCache_endpoint_queue_completion :: (IsMTRClusterStateCacheContainer clusterStateCacheContainer, IsNSNumber endpoint, IsNSObject queue) => clusterStateCacheContainer -> endpoint -> queue -> Ptr () -> IO ()
readAttributeClusterRevisionWithClusterStateCache_endpoint_queue_completion clusterStateCacheContainer endpoint queue completion =
  do
    cls' <- getRequiredClass "MTRBaseClusterCommodityMetering"
    sendClassMessage cls' readAttributeClusterRevisionWithClusterStateCache_endpoint_queue_completionSelector (toMTRClusterStateCacheContainer clusterStateCacheContainer) (toNSNumber endpoint) (toNSObject queue) completion

-- | @- init@
init_ :: IsMTRBaseClusterCommodityMetering mtrBaseClusterCommodityMetering => mtrBaseClusterCommodityMetering -> IO (Id MTRBaseClusterCommodityMetering)
init_ mtrBaseClusterCommodityMetering =
  sendOwnedMessage mtrBaseClusterCommodityMetering initSelector

-- | @+ new@
new :: IO (Id MTRBaseClusterCommodityMetering)
new  =
  do
    cls' <- getRequiredClass "MTRBaseClusterCommodityMetering"
    sendOwnedClassMessage cls' newSelector

-- | For all instance methods (reads, writes, commands) that take a completion, the completion will be called on the provided queue.
--
-- ObjC selector: @- initWithDevice:endpointID:queue:@
initWithDevice_endpointID_queue :: (IsMTRBaseClusterCommodityMetering mtrBaseClusterCommodityMetering, IsMTRBaseDevice device, IsNSNumber endpointID, IsNSObject queue) => mtrBaseClusterCommodityMetering -> device -> endpointID -> queue -> IO (Id MTRBaseClusterCommodityMetering)
initWithDevice_endpointID_queue mtrBaseClusterCommodityMetering device endpointID queue =
  sendOwnedMessage mtrBaseClusterCommodityMetering initWithDevice_endpointID_queueSelector (toMTRBaseDevice device) (toNSNumber endpointID) (toNSObject queue)

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @readAttributeMeteredQuantityWithCompletion:@
readAttributeMeteredQuantityWithCompletionSelector :: Selector '[Ptr ()] ()
readAttributeMeteredQuantityWithCompletionSelector = mkSelector "readAttributeMeteredQuantityWithCompletion:"

-- | @Selector@ for @subscribeAttributeMeteredQuantityWithParams:subscriptionEstablished:reportHandler:@
subscribeAttributeMeteredQuantityWithParams_subscriptionEstablished_reportHandlerSelector :: Selector '[Id MTRSubscribeParams, Ptr (), Ptr ()] ()
subscribeAttributeMeteredQuantityWithParams_subscriptionEstablished_reportHandlerSelector = mkSelector "subscribeAttributeMeteredQuantityWithParams:subscriptionEstablished:reportHandler:"

-- | @Selector@ for @readAttributeMeteredQuantityWithClusterStateCache:endpoint:queue:completion:@
readAttributeMeteredQuantityWithClusterStateCache_endpoint_queue_completionSelector :: Selector '[Id MTRClusterStateCacheContainer, Id NSNumber, Id NSObject, Ptr ()] ()
readAttributeMeteredQuantityWithClusterStateCache_endpoint_queue_completionSelector = mkSelector "readAttributeMeteredQuantityWithClusterStateCache:endpoint:queue:completion:"

-- | @Selector@ for @readAttributeMeteredQuantityTimestampWithCompletion:@
readAttributeMeteredQuantityTimestampWithCompletionSelector :: Selector '[Ptr ()] ()
readAttributeMeteredQuantityTimestampWithCompletionSelector = mkSelector "readAttributeMeteredQuantityTimestampWithCompletion:"

-- | @Selector@ for @subscribeAttributeMeteredQuantityTimestampWithParams:subscriptionEstablished:reportHandler:@
subscribeAttributeMeteredQuantityTimestampWithParams_subscriptionEstablished_reportHandlerSelector :: Selector '[Id MTRSubscribeParams, Ptr (), Ptr ()] ()
subscribeAttributeMeteredQuantityTimestampWithParams_subscriptionEstablished_reportHandlerSelector = mkSelector "subscribeAttributeMeteredQuantityTimestampWithParams:subscriptionEstablished:reportHandler:"

-- | @Selector@ for @readAttributeMeteredQuantityTimestampWithClusterStateCache:endpoint:queue:completion:@
readAttributeMeteredQuantityTimestampWithClusterStateCache_endpoint_queue_completionSelector :: Selector '[Id MTRClusterStateCacheContainer, Id NSNumber, Id NSObject, Ptr ()] ()
readAttributeMeteredQuantityTimestampWithClusterStateCache_endpoint_queue_completionSelector = mkSelector "readAttributeMeteredQuantityTimestampWithClusterStateCache:endpoint:queue:completion:"

-- | @Selector@ for @readAttributeTariffUnitWithCompletion:@
readAttributeTariffUnitWithCompletionSelector :: Selector '[Ptr ()] ()
readAttributeTariffUnitWithCompletionSelector = mkSelector "readAttributeTariffUnitWithCompletion:"

-- | @Selector@ for @subscribeAttributeTariffUnitWithParams:subscriptionEstablished:reportHandler:@
subscribeAttributeTariffUnitWithParams_subscriptionEstablished_reportHandlerSelector :: Selector '[Id MTRSubscribeParams, Ptr (), Ptr ()] ()
subscribeAttributeTariffUnitWithParams_subscriptionEstablished_reportHandlerSelector = mkSelector "subscribeAttributeTariffUnitWithParams:subscriptionEstablished:reportHandler:"

-- | @Selector@ for @readAttributeTariffUnitWithClusterStateCache:endpoint:queue:completion:@
readAttributeTariffUnitWithClusterStateCache_endpoint_queue_completionSelector :: Selector '[Id MTRClusterStateCacheContainer, Id NSNumber, Id NSObject, Ptr ()] ()
readAttributeTariffUnitWithClusterStateCache_endpoint_queue_completionSelector = mkSelector "readAttributeTariffUnitWithClusterStateCache:endpoint:queue:completion:"

-- | @Selector@ for @readAttributeMaximumMeteredQuantitiesWithCompletion:@
readAttributeMaximumMeteredQuantitiesWithCompletionSelector :: Selector '[Ptr ()] ()
readAttributeMaximumMeteredQuantitiesWithCompletionSelector = mkSelector "readAttributeMaximumMeteredQuantitiesWithCompletion:"

-- | @Selector@ for @subscribeAttributeMaximumMeteredQuantitiesWithParams:subscriptionEstablished:reportHandler:@
subscribeAttributeMaximumMeteredQuantitiesWithParams_subscriptionEstablished_reportHandlerSelector :: Selector '[Id MTRSubscribeParams, Ptr (), Ptr ()] ()
subscribeAttributeMaximumMeteredQuantitiesWithParams_subscriptionEstablished_reportHandlerSelector = mkSelector "subscribeAttributeMaximumMeteredQuantitiesWithParams:subscriptionEstablished:reportHandler:"

-- | @Selector@ for @readAttributeMaximumMeteredQuantitiesWithClusterStateCache:endpoint:queue:completion:@
readAttributeMaximumMeteredQuantitiesWithClusterStateCache_endpoint_queue_completionSelector :: Selector '[Id MTRClusterStateCacheContainer, Id NSNumber, Id NSObject, Ptr ()] ()
readAttributeMaximumMeteredQuantitiesWithClusterStateCache_endpoint_queue_completionSelector = mkSelector "readAttributeMaximumMeteredQuantitiesWithClusterStateCache:endpoint:queue:completion:"

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
initSelector :: Selector '[] (Id MTRBaseClusterCommodityMetering)
initSelector = mkSelector "init"

-- | @Selector@ for @new@
newSelector :: Selector '[] (Id MTRBaseClusterCommodityMetering)
newSelector = mkSelector "new"

-- | @Selector@ for @initWithDevice:endpointID:queue:@
initWithDevice_endpointID_queueSelector :: Selector '[Id MTRBaseDevice, Id NSNumber, Id NSObject] (Id MTRBaseClusterCommodityMetering)
initWithDevice_endpointID_queueSelector = mkSelector "initWithDevice:endpointID:queue:"

