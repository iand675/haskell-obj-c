{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Cluster Radon Concentration Measurement
--
-- Attributes for reporting radon concentration measurements
--
-- Generated bindings for @MTRBaseClusterRadonConcentrationMeasurement@.
module ObjC.Matter.MTRBaseClusterRadonConcentrationMeasurement
  ( MTRBaseClusterRadonConcentrationMeasurement
  , IsMTRBaseClusterRadonConcentrationMeasurement(..)
  , readAttributeMeasuredValueWithCompletion
  , subscribeAttributeMeasuredValueWithParams_subscriptionEstablished_reportHandler
  , readAttributeMeasuredValueWithClusterStateCache_endpoint_queue_completion
  , readAttributeMinMeasuredValueWithCompletion
  , subscribeAttributeMinMeasuredValueWithParams_subscriptionEstablished_reportHandler
  , readAttributeMinMeasuredValueWithClusterStateCache_endpoint_queue_completion
  , readAttributeMaxMeasuredValueWithCompletion
  , subscribeAttributeMaxMeasuredValueWithParams_subscriptionEstablished_reportHandler
  , readAttributeMaxMeasuredValueWithClusterStateCache_endpoint_queue_completion
  , readAttributePeakMeasuredValueWithCompletion
  , subscribeAttributePeakMeasuredValueWithParams_subscriptionEstablished_reportHandler
  , readAttributePeakMeasuredValueWithClusterStateCache_endpoint_queue_completion
  , readAttributePeakMeasuredValueWindowWithCompletion
  , subscribeAttributePeakMeasuredValueWindowWithParams_subscriptionEstablished_reportHandler
  , readAttributePeakMeasuredValueWindowWithClusterStateCache_endpoint_queue_completion
  , readAttributeAverageMeasuredValueWithCompletion
  , subscribeAttributeAverageMeasuredValueWithParams_subscriptionEstablished_reportHandler
  , readAttributeAverageMeasuredValueWithClusterStateCache_endpoint_queue_completion
  , readAttributeAverageMeasuredValueWindowWithCompletion
  , subscribeAttributeAverageMeasuredValueWindowWithParams_subscriptionEstablished_reportHandler
  , readAttributeAverageMeasuredValueWindowWithClusterStateCache_endpoint_queue_completion
  , readAttributeUncertaintyWithCompletion
  , subscribeAttributeUncertaintyWithParams_subscriptionEstablished_reportHandler
  , readAttributeUncertaintyWithClusterStateCache_endpoint_queue_completion
  , readAttributeMeasurementUnitWithCompletion
  , subscribeAttributeMeasurementUnitWithParams_subscriptionEstablished_reportHandler
  , readAttributeMeasurementUnitWithClusterStateCache_endpoint_queue_completion
  , readAttributeMeasurementMediumWithCompletion
  , subscribeAttributeMeasurementMediumWithParams_subscriptionEstablished_reportHandler
  , readAttributeMeasurementMediumWithClusterStateCache_endpoint_queue_completion
  , readAttributeLevelValueWithCompletion
  , subscribeAttributeLevelValueWithParams_subscriptionEstablished_reportHandler
  , readAttributeLevelValueWithClusterStateCache_endpoint_queue_completion
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
  , readAttributeAverageMeasuredValueWindowWithClusterStateCache_endpoint_queue_completionSelector
  , readAttributeAverageMeasuredValueWindowWithCompletionSelector
  , readAttributeAverageMeasuredValueWithClusterStateCache_endpoint_queue_completionSelector
  , readAttributeAverageMeasuredValueWithCompletionSelector
  , readAttributeClusterRevisionWithClusterStateCache_endpoint_queue_completionSelector
  , readAttributeClusterRevisionWithCompletionSelector
  , readAttributeFeatureMapWithClusterStateCache_endpoint_queue_completionSelector
  , readAttributeFeatureMapWithCompletionSelector
  , readAttributeGeneratedCommandListWithClusterStateCache_endpoint_queue_completionSelector
  , readAttributeGeneratedCommandListWithCompletionSelector
  , readAttributeLevelValueWithClusterStateCache_endpoint_queue_completionSelector
  , readAttributeLevelValueWithCompletionSelector
  , readAttributeMaxMeasuredValueWithClusterStateCache_endpoint_queue_completionSelector
  , readAttributeMaxMeasuredValueWithCompletionSelector
  , readAttributeMeasuredValueWithClusterStateCache_endpoint_queue_completionSelector
  , readAttributeMeasuredValueWithCompletionSelector
  , readAttributeMeasurementMediumWithClusterStateCache_endpoint_queue_completionSelector
  , readAttributeMeasurementMediumWithCompletionSelector
  , readAttributeMeasurementUnitWithClusterStateCache_endpoint_queue_completionSelector
  , readAttributeMeasurementUnitWithCompletionSelector
  , readAttributeMinMeasuredValueWithClusterStateCache_endpoint_queue_completionSelector
  , readAttributeMinMeasuredValueWithCompletionSelector
  , readAttributePeakMeasuredValueWindowWithClusterStateCache_endpoint_queue_completionSelector
  , readAttributePeakMeasuredValueWindowWithCompletionSelector
  , readAttributePeakMeasuredValueWithClusterStateCache_endpoint_queue_completionSelector
  , readAttributePeakMeasuredValueWithCompletionSelector
  , readAttributeUncertaintyWithClusterStateCache_endpoint_queue_completionSelector
  , readAttributeUncertaintyWithCompletionSelector
  , subscribeAttributeAcceptedCommandListWithParams_subscriptionEstablished_reportHandlerSelector
  , subscribeAttributeAttributeListWithParams_subscriptionEstablished_reportHandlerSelector
  , subscribeAttributeAverageMeasuredValueWindowWithParams_subscriptionEstablished_reportHandlerSelector
  , subscribeAttributeAverageMeasuredValueWithParams_subscriptionEstablished_reportHandlerSelector
  , subscribeAttributeClusterRevisionWithParams_subscriptionEstablished_reportHandlerSelector
  , subscribeAttributeFeatureMapWithParams_subscriptionEstablished_reportHandlerSelector
  , subscribeAttributeGeneratedCommandListWithParams_subscriptionEstablished_reportHandlerSelector
  , subscribeAttributeLevelValueWithParams_subscriptionEstablished_reportHandlerSelector
  , subscribeAttributeMaxMeasuredValueWithParams_subscriptionEstablished_reportHandlerSelector
  , subscribeAttributeMeasuredValueWithParams_subscriptionEstablished_reportHandlerSelector
  , subscribeAttributeMeasurementMediumWithParams_subscriptionEstablished_reportHandlerSelector
  , subscribeAttributeMeasurementUnitWithParams_subscriptionEstablished_reportHandlerSelector
  , subscribeAttributeMinMeasuredValueWithParams_subscriptionEstablished_reportHandlerSelector
  , subscribeAttributePeakMeasuredValueWindowWithParams_subscriptionEstablished_reportHandlerSelector
  , subscribeAttributePeakMeasuredValueWithParams_subscriptionEstablished_reportHandlerSelector
  , subscribeAttributeUncertaintyWithParams_subscriptionEstablished_reportHandlerSelector


  ) where

import Foreign.Ptr (Ptr, FunPtr)
import Foreign.C.Types

import ObjC.Runtime.Types
import ObjC.Runtime.Message (sendMessage, sendOwnedMessage, sendClassMessage, sendOwnedClassMessage)
import ObjC.Runtime.Selector (mkSelector)
import ObjC.Runtime.Class (getRequiredClass)

import ObjC.Matter.Internal.Classes
import ObjC.Foundation.Internal.Classes

-- | @- readAttributeMeasuredValueWithCompletion:@
readAttributeMeasuredValueWithCompletion :: IsMTRBaseClusterRadonConcentrationMeasurement mtrBaseClusterRadonConcentrationMeasurement => mtrBaseClusterRadonConcentrationMeasurement -> Ptr () -> IO ()
readAttributeMeasuredValueWithCompletion mtrBaseClusterRadonConcentrationMeasurement completion =
  sendMessage mtrBaseClusterRadonConcentrationMeasurement readAttributeMeasuredValueWithCompletionSelector completion

-- | @- subscribeAttributeMeasuredValueWithParams:subscriptionEstablished:reportHandler:@
subscribeAttributeMeasuredValueWithParams_subscriptionEstablished_reportHandler :: (IsMTRBaseClusterRadonConcentrationMeasurement mtrBaseClusterRadonConcentrationMeasurement, IsMTRSubscribeParams params) => mtrBaseClusterRadonConcentrationMeasurement -> params -> Ptr () -> Ptr () -> IO ()
subscribeAttributeMeasuredValueWithParams_subscriptionEstablished_reportHandler mtrBaseClusterRadonConcentrationMeasurement params subscriptionEstablished reportHandler =
  sendMessage mtrBaseClusterRadonConcentrationMeasurement subscribeAttributeMeasuredValueWithParams_subscriptionEstablished_reportHandlerSelector (toMTRSubscribeParams params) subscriptionEstablished reportHandler

-- | @+ readAttributeMeasuredValueWithClusterStateCache:endpoint:queue:completion:@
readAttributeMeasuredValueWithClusterStateCache_endpoint_queue_completion :: (IsMTRClusterStateCacheContainer clusterStateCacheContainer, IsNSNumber endpoint, IsNSObject queue) => clusterStateCacheContainer -> endpoint -> queue -> Ptr () -> IO ()
readAttributeMeasuredValueWithClusterStateCache_endpoint_queue_completion clusterStateCacheContainer endpoint queue completion =
  do
    cls' <- getRequiredClass "MTRBaseClusterRadonConcentrationMeasurement"
    sendClassMessage cls' readAttributeMeasuredValueWithClusterStateCache_endpoint_queue_completionSelector (toMTRClusterStateCacheContainer clusterStateCacheContainer) (toNSNumber endpoint) (toNSObject queue) completion

-- | @- readAttributeMinMeasuredValueWithCompletion:@
readAttributeMinMeasuredValueWithCompletion :: IsMTRBaseClusterRadonConcentrationMeasurement mtrBaseClusterRadonConcentrationMeasurement => mtrBaseClusterRadonConcentrationMeasurement -> Ptr () -> IO ()
readAttributeMinMeasuredValueWithCompletion mtrBaseClusterRadonConcentrationMeasurement completion =
  sendMessage mtrBaseClusterRadonConcentrationMeasurement readAttributeMinMeasuredValueWithCompletionSelector completion

-- | @- subscribeAttributeMinMeasuredValueWithParams:subscriptionEstablished:reportHandler:@
subscribeAttributeMinMeasuredValueWithParams_subscriptionEstablished_reportHandler :: (IsMTRBaseClusterRadonConcentrationMeasurement mtrBaseClusterRadonConcentrationMeasurement, IsMTRSubscribeParams params) => mtrBaseClusterRadonConcentrationMeasurement -> params -> Ptr () -> Ptr () -> IO ()
subscribeAttributeMinMeasuredValueWithParams_subscriptionEstablished_reportHandler mtrBaseClusterRadonConcentrationMeasurement params subscriptionEstablished reportHandler =
  sendMessage mtrBaseClusterRadonConcentrationMeasurement subscribeAttributeMinMeasuredValueWithParams_subscriptionEstablished_reportHandlerSelector (toMTRSubscribeParams params) subscriptionEstablished reportHandler

-- | @+ readAttributeMinMeasuredValueWithClusterStateCache:endpoint:queue:completion:@
readAttributeMinMeasuredValueWithClusterStateCache_endpoint_queue_completion :: (IsMTRClusterStateCacheContainer clusterStateCacheContainer, IsNSNumber endpoint, IsNSObject queue) => clusterStateCacheContainer -> endpoint -> queue -> Ptr () -> IO ()
readAttributeMinMeasuredValueWithClusterStateCache_endpoint_queue_completion clusterStateCacheContainer endpoint queue completion =
  do
    cls' <- getRequiredClass "MTRBaseClusterRadonConcentrationMeasurement"
    sendClassMessage cls' readAttributeMinMeasuredValueWithClusterStateCache_endpoint_queue_completionSelector (toMTRClusterStateCacheContainer clusterStateCacheContainer) (toNSNumber endpoint) (toNSObject queue) completion

-- | @- readAttributeMaxMeasuredValueWithCompletion:@
readAttributeMaxMeasuredValueWithCompletion :: IsMTRBaseClusterRadonConcentrationMeasurement mtrBaseClusterRadonConcentrationMeasurement => mtrBaseClusterRadonConcentrationMeasurement -> Ptr () -> IO ()
readAttributeMaxMeasuredValueWithCompletion mtrBaseClusterRadonConcentrationMeasurement completion =
  sendMessage mtrBaseClusterRadonConcentrationMeasurement readAttributeMaxMeasuredValueWithCompletionSelector completion

-- | @- subscribeAttributeMaxMeasuredValueWithParams:subscriptionEstablished:reportHandler:@
subscribeAttributeMaxMeasuredValueWithParams_subscriptionEstablished_reportHandler :: (IsMTRBaseClusterRadonConcentrationMeasurement mtrBaseClusterRadonConcentrationMeasurement, IsMTRSubscribeParams params) => mtrBaseClusterRadonConcentrationMeasurement -> params -> Ptr () -> Ptr () -> IO ()
subscribeAttributeMaxMeasuredValueWithParams_subscriptionEstablished_reportHandler mtrBaseClusterRadonConcentrationMeasurement params subscriptionEstablished reportHandler =
  sendMessage mtrBaseClusterRadonConcentrationMeasurement subscribeAttributeMaxMeasuredValueWithParams_subscriptionEstablished_reportHandlerSelector (toMTRSubscribeParams params) subscriptionEstablished reportHandler

-- | @+ readAttributeMaxMeasuredValueWithClusterStateCache:endpoint:queue:completion:@
readAttributeMaxMeasuredValueWithClusterStateCache_endpoint_queue_completion :: (IsMTRClusterStateCacheContainer clusterStateCacheContainer, IsNSNumber endpoint, IsNSObject queue) => clusterStateCacheContainer -> endpoint -> queue -> Ptr () -> IO ()
readAttributeMaxMeasuredValueWithClusterStateCache_endpoint_queue_completion clusterStateCacheContainer endpoint queue completion =
  do
    cls' <- getRequiredClass "MTRBaseClusterRadonConcentrationMeasurement"
    sendClassMessage cls' readAttributeMaxMeasuredValueWithClusterStateCache_endpoint_queue_completionSelector (toMTRClusterStateCacheContainer clusterStateCacheContainer) (toNSNumber endpoint) (toNSObject queue) completion

-- | @- readAttributePeakMeasuredValueWithCompletion:@
readAttributePeakMeasuredValueWithCompletion :: IsMTRBaseClusterRadonConcentrationMeasurement mtrBaseClusterRadonConcentrationMeasurement => mtrBaseClusterRadonConcentrationMeasurement -> Ptr () -> IO ()
readAttributePeakMeasuredValueWithCompletion mtrBaseClusterRadonConcentrationMeasurement completion =
  sendMessage mtrBaseClusterRadonConcentrationMeasurement readAttributePeakMeasuredValueWithCompletionSelector completion

-- | @- subscribeAttributePeakMeasuredValueWithParams:subscriptionEstablished:reportHandler:@
subscribeAttributePeakMeasuredValueWithParams_subscriptionEstablished_reportHandler :: (IsMTRBaseClusterRadonConcentrationMeasurement mtrBaseClusterRadonConcentrationMeasurement, IsMTRSubscribeParams params) => mtrBaseClusterRadonConcentrationMeasurement -> params -> Ptr () -> Ptr () -> IO ()
subscribeAttributePeakMeasuredValueWithParams_subscriptionEstablished_reportHandler mtrBaseClusterRadonConcentrationMeasurement params subscriptionEstablished reportHandler =
  sendMessage mtrBaseClusterRadonConcentrationMeasurement subscribeAttributePeakMeasuredValueWithParams_subscriptionEstablished_reportHandlerSelector (toMTRSubscribeParams params) subscriptionEstablished reportHandler

-- | @+ readAttributePeakMeasuredValueWithClusterStateCache:endpoint:queue:completion:@
readAttributePeakMeasuredValueWithClusterStateCache_endpoint_queue_completion :: (IsMTRClusterStateCacheContainer clusterStateCacheContainer, IsNSNumber endpoint, IsNSObject queue) => clusterStateCacheContainer -> endpoint -> queue -> Ptr () -> IO ()
readAttributePeakMeasuredValueWithClusterStateCache_endpoint_queue_completion clusterStateCacheContainer endpoint queue completion =
  do
    cls' <- getRequiredClass "MTRBaseClusterRadonConcentrationMeasurement"
    sendClassMessage cls' readAttributePeakMeasuredValueWithClusterStateCache_endpoint_queue_completionSelector (toMTRClusterStateCacheContainer clusterStateCacheContainer) (toNSNumber endpoint) (toNSObject queue) completion

-- | @- readAttributePeakMeasuredValueWindowWithCompletion:@
readAttributePeakMeasuredValueWindowWithCompletion :: IsMTRBaseClusterRadonConcentrationMeasurement mtrBaseClusterRadonConcentrationMeasurement => mtrBaseClusterRadonConcentrationMeasurement -> Ptr () -> IO ()
readAttributePeakMeasuredValueWindowWithCompletion mtrBaseClusterRadonConcentrationMeasurement completion =
  sendMessage mtrBaseClusterRadonConcentrationMeasurement readAttributePeakMeasuredValueWindowWithCompletionSelector completion

-- | @- subscribeAttributePeakMeasuredValueWindowWithParams:subscriptionEstablished:reportHandler:@
subscribeAttributePeakMeasuredValueWindowWithParams_subscriptionEstablished_reportHandler :: (IsMTRBaseClusterRadonConcentrationMeasurement mtrBaseClusterRadonConcentrationMeasurement, IsMTRSubscribeParams params) => mtrBaseClusterRadonConcentrationMeasurement -> params -> Ptr () -> Ptr () -> IO ()
subscribeAttributePeakMeasuredValueWindowWithParams_subscriptionEstablished_reportHandler mtrBaseClusterRadonConcentrationMeasurement params subscriptionEstablished reportHandler =
  sendMessage mtrBaseClusterRadonConcentrationMeasurement subscribeAttributePeakMeasuredValueWindowWithParams_subscriptionEstablished_reportHandlerSelector (toMTRSubscribeParams params) subscriptionEstablished reportHandler

-- | @+ readAttributePeakMeasuredValueWindowWithClusterStateCache:endpoint:queue:completion:@
readAttributePeakMeasuredValueWindowWithClusterStateCache_endpoint_queue_completion :: (IsMTRClusterStateCacheContainer clusterStateCacheContainer, IsNSNumber endpoint, IsNSObject queue) => clusterStateCacheContainer -> endpoint -> queue -> Ptr () -> IO ()
readAttributePeakMeasuredValueWindowWithClusterStateCache_endpoint_queue_completion clusterStateCacheContainer endpoint queue completion =
  do
    cls' <- getRequiredClass "MTRBaseClusterRadonConcentrationMeasurement"
    sendClassMessage cls' readAttributePeakMeasuredValueWindowWithClusterStateCache_endpoint_queue_completionSelector (toMTRClusterStateCacheContainer clusterStateCacheContainer) (toNSNumber endpoint) (toNSObject queue) completion

-- | @- readAttributeAverageMeasuredValueWithCompletion:@
readAttributeAverageMeasuredValueWithCompletion :: IsMTRBaseClusterRadonConcentrationMeasurement mtrBaseClusterRadonConcentrationMeasurement => mtrBaseClusterRadonConcentrationMeasurement -> Ptr () -> IO ()
readAttributeAverageMeasuredValueWithCompletion mtrBaseClusterRadonConcentrationMeasurement completion =
  sendMessage mtrBaseClusterRadonConcentrationMeasurement readAttributeAverageMeasuredValueWithCompletionSelector completion

-- | @- subscribeAttributeAverageMeasuredValueWithParams:subscriptionEstablished:reportHandler:@
subscribeAttributeAverageMeasuredValueWithParams_subscriptionEstablished_reportHandler :: (IsMTRBaseClusterRadonConcentrationMeasurement mtrBaseClusterRadonConcentrationMeasurement, IsMTRSubscribeParams params) => mtrBaseClusterRadonConcentrationMeasurement -> params -> Ptr () -> Ptr () -> IO ()
subscribeAttributeAverageMeasuredValueWithParams_subscriptionEstablished_reportHandler mtrBaseClusterRadonConcentrationMeasurement params subscriptionEstablished reportHandler =
  sendMessage mtrBaseClusterRadonConcentrationMeasurement subscribeAttributeAverageMeasuredValueWithParams_subscriptionEstablished_reportHandlerSelector (toMTRSubscribeParams params) subscriptionEstablished reportHandler

-- | @+ readAttributeAverageMeasuredValueWithClusterStateCache:endpoint:queue:completion:@
readAttributeAverageMeasuredValueWithClusterStateCache_endpoint_queue_completion :: (IsMTRClusterStateCacheContainer clusterStateCacheContainer, IsNSNumber endpoint, IsNSObject queue) => clusterStateCacheContainer -> endpoint -> queue -> Ptr () -> IO ()
readAttributeAverageMeasuredValueWithClusterStateCache_endpoint_queue_completion clusterStateCacheContainer endpoint queue completion =
  do
    cls' <- getRequiredClass "MTRBaseClusterRadonConcentrationMeasurement"
    sendClassMessage cls' readAttributeAverageMeasuredValueWithClusterStateCache_endpoint_queue_completionSelector (toMTRClusterStateCacheContainer clusterStateCacheContainer) (toNSNumber endpoint) (toNSObject queue) completion

-- | @- readAttributeAverageMeasuredValueWindowWithCompletion:@
readAttributeAverageMeasuredValueWindowWithCompletion :: IsMTRBaseClusterRadonConcentrationMeasurement mtrBaseClusterRadonConcentrationMeasurement => mtrBaseClusterRadonConcentrationMeasurement -> Ptr () -> IO ()
readAttributeAverageMeasuredValueWindowWithCompletion mtrBaseClusterRadonConcentrationMeasurement completion =
  sendMessage mtrBaseClusterRadonConcentrationMeasurement readAttributeAverageMeasuredValueWindowWithCompletionSelector completion

-- | @- subscribeAttributeAverageMeasuredValueWindowWithParams:subscriptionEstablished:reportHandler:@
subscribeAttributeAverageMeasuredValueWindowWithParams_subscriptionEstablished_reportHandler :: (IsMTRBaseClusterRadonConcentrationMeasurement mtrBaseClusterRadonConcentrationMeasurement, IsMTRSubscribeParams params) => mtrBaseClusterRadonConcentrationMeasurement -> params -> Ptr () -> Ptr () -> IO ()
subscribeAttributeAverageMeasuredValueWindowWithParams_subscriptionEstablished_reportHandler mtrBaseClusterRadonConcentrationMeasurement params subscriptionEstablished reportHandler =
  sendMessage mtrBaseClusterRadonConcentrationMeasurement subscribeAttributeAverageMeasuredValueWindowWithParams_subscriptionEstablished_reportHandlerSelector (toMTRSubscribeParams params) subscriptionEstablished reportHandler

-- | @+ readAttributeAverageMeasuredValueWindowWithClusterStateCache:endpoint:queue:completion:@
readAttributeAverageMeasuredValueWindowWithClusterStateCache_endpoint_queue_completion :: (IsMTRClusterStateCacheContainer clusterStateCacheContainer, IsNSNumber endpoint, IsNSObject queue) => clusterStateCacheContainer -> endpoint -> queue -> Ptr () -> IO ()
readAttributeAverageMeasuredValueWindowWithClusterStateCache_endpoint_queue_completion clusterStateCacheContainer endpoint queue completion =
  do
    cls' <- getRequiredClass "MTRBaseClusterRadonConcentrationMeasurement"
    sendClassMessage cls' readAttributeAverageMeasuredValueWindowWithClusterStateCache_endpoint_queue_completionSelector (toMTRClusterStateCacheContainer clusterStateCacheContainer) (toNSNumber endpoint) (toNSObject queue) completion

-- | @- readAttributeUncertaintyWithCompletion:@
readAttributeUncertaintyWithCompletion :: IsMTRBaseClusterRadonConcentrationMeasurement mtrBaseClusterRadonConcentrationMeasurement => mtrBaseClusterRadonConcentrationMeasurement -> Ptr () -> IO ()
readAttributeUncertaintyWithCompletion mtrBaseClusterRadonConcentrationMeasurement completion =
  sendMessage mtrBaseClusterRadonConcentrationMeasurement readAttributeUncertaintyWithCompletionSelector completion

-- | @- subscribeAttributeUncertaintyWithParams:subscriptionEstablished:reportHandler:@
subscribeAttributeUncertaintyWithParams_subscriptionEstablished_reportHandler :: (IsMTRBaseClusterRadonConcentrationMeasurement mtrBaseClusterRadonConcentrationMeasurement, IsMTRSubscribeParams params) => mtrBaseClusterRadonConcentrationMeasurement -> params -> Ptr () -> Ptr () -> IO ()
subscribeAttributeUncertaintyWithParams_subscriptionEstablished_reportHandler mtrBaseClusterRadonConcentrationMeasurement params subscriptionEstablished reportHandler =
  sendMessage mtrBaseClusterRadonConcentrationMeasurement subscribeAttributeUncertaintyWithParams_subscriptionEstablished_reportHandlerSelector (toMTRSubscribeParams params) subscriptionEstablished reportHandler

-- | @+ readAttributeUncertaintyWithClusterStateCache:endpoint:queue:completion:@
readAttributeUncertaintyWithClusterStateCache_endpoint_queue_completion :: (IsMTRClusterStateCacheContainer clusterStateCacheContainer, IsNSNumber endpoint, IsNSObject queue) => clusterStateCacheContainer -> endpoint -> queue -> Ptr () -> IO ()
readAttributeUncertaintyWithClusterStateCache_endpoint_queue_completion clusterStateCacheContainer endpoint queue completion =
  do
    cls' <- getRequiredClass "MTRBaseClusterRadonConcentrationMeasurement"
    sendClassMessage cls' readAttributeUncertaintyWithClusterStateCache_endpoint_queue_completionSelector (toMTRClusterStateCacheContainer clusterStateCacheContainer) (toNSNumber endpoint) (toNSObject queue) completion

-- | @- readAttributeMeasurementUnitWithCompletion:@
readAttributeMeasurementUnitWithCompletion :: IsMTRBaseClusterRadonConcentrationMeasurement mtrBaseClusterRadonConcentrationMeasurement => mtrBaseClusterRadonConcentrationMeasurement -> Ptr () -> IO ()
readAttributeMeasurementUnitWithCompletion mtrBaseClusterRadonConcentrationMeasurement completion =
  sendMessage mtrBaseClusterRadonConcentrationMeasurement readAttributeMeasurementUnitWithCompletionSelector completion

-- | @- subscribeAttributeMeasurementUnitWithParams:subscriptionEstablished:reportHandler:@
subscribeAttributeMeasurementUnitWithParams_subscriptionEstablished_reportHandler :: (IsMTRBaseClusterRadonConcentrationMeasurement mtrBaseClusterRadonConcentrationMeasurement, IsMTRSubscribeParams params) => mtrBaseClusterRadonConcentrationMeasurement -> params -> Ptr () -> Ptr () -> IO ()
subscribeAttributeMeasurementUnitWithParams_subscriptionEstablished_reportHandler mtrBaseClusterRadonConcentrationMeasurement params subscriptionEstablished reportHandler =
  sendMessage mtrBaseClusterRadonConcentrationMeasurement subscribeAttributeMeasurementUnitWithParams_subscriptionEstablished_reportHandlerSelector (toMTRSubscribeParams params) subscriptionEstablished reportHandler

-- | @+ readAttributeMeasurementUnitWithClusterStateCache:endpoint:queue:completion:@
readAttributeMeasurementUnitWithClusterStateCache_endpoint_queue_completion :: (IsMTRClusterStateCacheContainer clusterStateCacheContainer, IsNSNumber endpoint, IsNSObject queue) => clusterStateCacheContainer -> endpoint -> queue -> Ptr () -> IO ()
readAttributeMeasurementUnitWithClusterStateCache_endpoint_queue_completion clusterStateCacheContainer endpoint queue completion =
  do
    cls' <- getRequiredClass "MTRBaseClusterRadonConcentrationMeasurement"
    sendClassMessage cls' readAttributeMeasurementUnitWithClusterStateCache_endpoint_queue_completionSelector (toMTRClusterStateCacheContainer clusterStateCacheContainer) (toNSNumber endpoint) (toNSObject queue) completion

-- | @- readAttributeMeasurementMediumWithCompletion:@
readAttributeMeasurementMediumWithCompletion :: IsMTRBaseClusterRadonConcentrationMeasurement mtrBaseClusterRadonConcentrationMeasurement => mtrBaseClusterRadonConcentrationMeasurement -> Ptr () -> IO ()
readAttributeMeasurementMediumWithCompletion mtrBaseClusterRadonConcentrationMeasurement completion =
  sendMessage mtrBaseClusterRadonConcentrationMeasurement readAttributeMeasurementMediumWithCompletionSelector completion

-- | @- subscribeAttributeMeasurementMediumWithParams:subscriptionEstablished:reportHandler:@
subscribeAttributeMeasurementMediumWithParams_subscriptionEstablished_reportHandler :: (IsMTRBaseClusterRadonConcentrationMeasurement mtrBaseClusterRadonConcentrationMeasurement, IsMTRSubscribeParams params) => mtrBaseClusterRadonConcentrationMeasurement -> params -> Ptr () -> Ptr () -> IO ()
subscribeAttributeMeasurementMediumWithParams_subscriptionEstablished_reportHandler mtrBaseClusterRadonConcentrationMeasurement params subscriptionEstablished reportHandler =
  sendMessage mtrBaseClusterRadonConcentrationMeasurement subscribeAttributeMeasurementMediumWithParams_subscriptionEstablished_reportHandlerSelector (toMTRSubscribeParams params) subscriptionEstablished reportHandler

-- | @+ readAttributeMeasurementMediumWithClusterStateCache:endpoint:queue:completion:@
readAttributeMeasurementMediumWithClusterStateCache_endpoint_queue_completion :: (IsMTRClusterStateCacheContainer clusterStateCacheContainer, IsNSNumber endpoint, IsNSObject queue) => clusterStateCacheContainer -> endpoint -> queue -> Ptr () -> IO ()
readAttributeMeasurementMediumWithClusterStateCache_endpoint_queue_completion clusterStateCacheContainer endpoint queue completion =
  do
    cls' <- getRequiredClass "MTRBaseClusterRadonConcentrationMeasurement"
    sendClassMessage cls' readAttributeMeasurementMediumWithClusterStateCache_endpoint_queue_completionSelector (toMTRClusterStateCacheContainer clusterStateCacheContainer) (toNSNumber endpoint) (toNSObject queue) completion

-- | @- readAttributeLevelValueWithCompletion:@
readAttributeLevelValueWithCompletion :: IsMTRBaseClusterRadonConcentrationMeasurement mtrBaseClusterRadonConcentrationMeasurement => mtrBaseClusterRadonConcentrationMeasurement -> Ptr () -> IO ()
readAttributeLevelValueWithCompletion mtrBaseClusterRadonConcentrationMeasurement completion =
  sendMessage mtrBaseClusterRadonConcentrationMeasurement readAttributeLevelValueWithCompletionSelector completion

-- | @- subscribeAttributeLevelValueWithParams:subscriptionEstablished:reportHandler:@
subscribeAttributeLevelValueWithParams_subscriptionEstablished_reportHandler :: (IsMTRBaseClusterRadonConcentrationMeasurement mtrBaseClusterRadonConcentrationMeasurement, IsMTRSubscribeParams params) => mtrBaseClusterRadonConcentrationMeasurement -> params -> Ptr () -> Ptr () -> IO ()
subscribeAttributeLevelValueWithParams_subscriptionEstablished_reportHandler mtrBaseClusterRadonConcentrationMeasurement params subscriptionEstablished reportHandler =
  sendMessage mtrBaseClusterRadonConcentrationMeasurement subscribeAttributeLevelValueWithParams_subscriptionEstablished_reportHandlerSelector (toMTRSubscribeParams params) subscriptionEstablished reportHandler

-- | @+ readAttributeLevelValueWithClusterStateCache:endpoint:queue:completion:@
readAttributeLevelValueWithClusterStateCache_endpoint_queue_completion :: (IsMTRClusterStateCacheContainer clusterStateCacheContainer, IsNSNumber endpoint, IsNSObject queue) => clusterStateCacheContainer -> endpoint -> queue -> Ptr () -> IO ()
readAttributeLevelValueWithClusterStateCache_endpoint_queue_completion clusterStateCacheContainer endpoint queue completion =
  do
    cls' <- getRequiredClass "MTRBaseClusterRadonConcentrationMeasurement"
    sendClassMessage cls' readAttributeLevelValueWithClusterStateCache_endpoint_queue_completionSelector (toMTRClusterStateCacheContainer clusterStateCacheContainer) (toNSNumber endpoint) (toNSObject queue) completion

-- | @- readAttributeGeneratedCommandListWithCompletion:@
readAttributeGeneratedCommandListWithCompletion :: IsMTRBaseClusterRadonConcentrationMeasurement mtrBaseClusterRadonConcentrationMeasurement => mtrBaseClusterRadonConcentrationMeasurement -> Ptr () -> IO ()
readAttributeGeneratedCommandListWithCompletion mtrBaseClusterRadonConcentrationMeasurement completion =
  sendMessage mtrBaseClusterRadonConcentrationMeasurement readAttributeGeneratedCommandListWithCompletionSelector completion

-- | @- subscribeAttributeGeneratedCommandListWithParams:subscriptionEstablished:reportHandler:@
subscribeAttributeGeneratedCommandListWithParams_subscriptionEstablished_reportHandler :: (IsMTRBaseClusterRadonConcentrationMeasurement mtrBaseClusterRadonConcentrationMeasurement, IsMTRSubscribeParams params) => mtrBaseClusterRadonConcentrationMeasurement -> params -> Ptr () -> Ptr () -> IO ()
subscribeAttributeGeneratedCommandListWithParams_subscriptionEstablished_reportHandler mtrBaseClusterRadonConcentrationMeasurement params subscriptionEstablished reportHandler =
  sendMessage mtrBaseClusterRadonConcentrationMeasurement subscribeAttributeGeneratedCommandListWithParams_subscriptionEstablished_reportHandlerSelector (toMTRSubscribeParams params) subscriptionEstablished reportHandler

-- | @+ readAttributeGeneratedCommandListWithClusterStateCache:endpoint:queue:completion:@
readAttributeGeneratedCommandListWithClusterStateCache_endpoint_queue_completion :: (IsMTRClusterStateCacheContainer clusterStateCacheContainer, IsNSNumber endpoint, IsNSObject queue) => clusterStateCacheContainer -> endpoint -> queue -> Ptr () -> IO ()
readAttributeGeneratedCommandListWithClusterStateCache_endpoint_queue_completion clusterStateCacheContainer endpoint queue completion =
  do
    cls' <- getRequiredClass "MTRBaseClusterRadonConcentrationMeasurement"
    sendClassMessage cls' readAttributeGeneratedCommandListWithClusterStateCache_endpoint_queue_completionSelector (toMTRClusterStateCacheContainer clusterStateCacheContainer) (toNSNumber endpoint) (toNSObject queue) completion

-- | @- readAttributeAcceptedCommandListWithCompletion:@
readAttributeAcceptedCommandListWithCompletion :: IsMTRBaseClusterRadonConcentrationMeasurement mtrBaseClusterRadonConcentrationMeasurement => mtrBaseClusterRadonConcentrationMeasurement -> Ptr () -> IO ()
readAttributeAcceptedCommandListWithCompletion mtrBaseClusterRadonConcentrationMeasurement completion =
  sendMessage mtrBaseClusterRadonConcentrationMeasurement readAttributeAcceptedCommandListWithCompletionSelector completion

-- | @- subscribeAttributeAcceptedCommandListWithParams:subscriptionEstablished:reportHandler:@
subscribeAttributeAcceptedCommandListWithParams_subscriptionEstablished_reportHandler :: (IsMTRBaseClusterRadonConcentrationMeasurement mtrBaseClusterRadonConcentrationMeasurement, IsMTRSubscribeParams params) => mtrBaseClusterRadonConcentrationMeasurement -> params -> Ptr () -> Ptr () -> IO ()
subscribeAttributeAcceptedCommandListWithParams_subscriptionEstablished_reportHandler mtrBaseClusterRadonConcentrationMeasurement params subscriptionEstablished reportHandler =
  sendMessage mtrBaseClusterRadonConcentrationMeasurement subscribeAttributeAcceptedCommandListWithParams_subscriptionEstablished_reportHandlerSelector (toMTRSubscribeParams params) subscriptionEstablished reportHandler

-- | @+ readAttributeAcceptedCommandListWithClusterStateCache:endpoint:queue:completion:@
readAttributeAcceptedCommandListWithClusterStateCache_endpoint_queue_completion :: (IsMTRClusterStateCacheContainer clusterStateCacheContainer, IsNSNumber endpoint, IsNSObject queue) => clusterStateCacheContainer -> endpoint -> queue -> Ptr () -> IO ()
readAttributeAcceptedCommandListWithClusterStateCache_endpoint_queue_completion clusterStateCacheContainer endpoint queue completion =
  do
    cls' <- getRequiredClass "MTRBaseClusterRadonConcentrationMeasurement"
    sendClassMessage cls' readAttributeAcceptedCommandListWithClusterStateCache_endpoint_queue_completionSelector (toMTRClusterStateCacheContainer clusterStateCacheContainer) (toNSNumber endpoint) (toNSObject queue) completion

-- | @- readAttributeAttributeListWithCompletion:@
readAttributeAttributeListWithCompletion :: IsMTRBaseClusterRadonConcentrationMeasurement mtrBaseClusterRadonConcentrationMeasurement => mtrBaseClusterRadonConcentrationMeasurement -> Ptr () -> IO ()
readAttributeAttributeListWithCompletion mtrBaseClusterRadonConcentrationMeasurement completion =
  sendMessage mtrBaseClusterRadonConcentrationMeasurement readAttributeAttributeListWithCompletionSelector completion

-- | @- subscribeAttributeAttributeListWithParams:subscriptionEstablished:reportHandler:@
subscribeAttributeAttributeListWithParams_subscriptionEstablished_reportHandler :: (IsMTRBaseClusterRadonConcentrationMeasurement mtrBaseClusterRadonConcentrationMeasurement, IsMTRSubscribeParams params) => mtrBaseClusterRadonConcentrationMeasurement -> params -> Ptr () -> Ptr () -> IO ()
subscribeAttributeAttributeListWithParams_subscriptionEstablished_reportHandler mtrBaseClusterRadonConcentrationMeasurement params subscriptionEstablished reportHandler =
  sendMessage mtrBaseClusterRadonConcentrationMeasurement subscribeAttributeAttributeListWithParams_subscriptionEstablished_reportHandlerSelector (toMTRSubscribeParams params) subscriptionEstablished reportHandler

-- | @+ readAttributeAttributeListWithClusterStateCache:endpoint:queue:completion:@
readAttributeAttributeListWithClusterStateCache_endpoint_queue_completion :: (IsMTRClusterStateCacheContainer clusterStateCacheContainer, IsNSNumber endpoint, IsNSObject queue) => clusterStateCacheContainer -> endpoint -> queue -> Ptr () -> IO ()
readAttributeAttributeListWithClusterStateCache_endpoint_queue_completion clusterStateCacheContainer endpoint queue completion =
  do
    cls' <- getRequiredClass "MTRBaseClusterRadonConcentrationMeasurement"
    sendClassMessage cls' readAttributeAttributeListWithClusterStateCache_endpoint_queue_completionSelector (toMTRClusterStateCacheContainer clusterStateCacheContainer) (toNSNumber endpoint) (toNSObject queue) completion

-- | @- readAttributeFeatureMapWithCompletion:@
readAttributeFeatureMapWithCompletion :: IsMTRBaseClusterRadonConcentrationMeasurement mtrBaseClusterRadonConcentrationMeasurement => mtrBaseClusterRadonConcentrationMeasurement -> Ptr () -> IO ()
readAttributeFeatureMapWithCompletion mtrBaseClusterRadonConcentrationMeasurement completion =
  sendMessage mtrBaseClusterRadonConcentrationMeasurement readAttributeFeatureMapWithCompletionSelector completion

-- | @- subscribeAttributeFeatureMapWithParams:subscriptionEstablished:reportHandler:@
subscribeAttributeFeatureMapWithParams_subscriptionEstablished_reportHandler :: (IsMTRBaseClusterRadonConcentrationMeasurement mtrBaseClusterRadonConcentrationMeasurement, IsMTRSubscribeParams params) => mtrBaseClusterRadonConcentrationMeasurement -> params -> Ptr () -> Ptr () -> IO ()
subscribeAttributeFeatureMapWithParams_subscriptionEstablished_reportHandler mtrBaseClusterRadonConcentrationMeasurement params subscriptionEstablished reportHandler =
  sendMessage mtrBaseClusterRadonConcentrationMeasurement subscribeAttributeFeatureMapWithParams_subscriptionEstablished_reportHandlerSelector (toMTRSubscribeParams params) subscriptionEstablished reportHandler

-- | @+ readAttributeFeatureMapWithClusterStateCache:endpoint:queue:completion:@
readAttributeFeatureMapWithClusterStateCache_endpoint_queue_completion :: (IsMTRClusterStateCacheContainer clusterStateCacheContainer, IsNSNumber endpoint, IsNSObject queue) => clusterStateCacheContainer -> endpoint -> queue -> Ptr () -> IO ()
readAttributeFeatureMapWithClusterStateCache_endpoint_queue_completion clusterStateCacheContainer endpoint queue completion =
  do
    cls' <- getRequiredClass "MTRBaseClusterRadonConcentrationMeasurement"
    sendClassMessage cls' readAttributeFeatureMapWithClusterStateCache_endpoint_queue_completionSelector (toMTRClusterStateCacheContainer clusterStateCacheContainer) (toNSNumber endpoint) (toNSObject queue) completion

-- | @- readAttributeClusterRevisionWithCompletion:@
readAttributeClusterRevisionWithCompletion :: IsMTRBaseClusterRadonConcentrationMeasurement mtrBaseClusterRadonConcentrationMeasurement => mtrBaseClusterRadonConcentrationMeasurement -> Ptr () -> IO ()
readAttributeClusterRevisionWithCompletion mtrBaseClusterRadonConcentrationMeasurement completion =
  sendMessage mtrBaseClusterRadonConcentrationMeasurement readAttributeClusterRevisionWithCompletionSelector completion

-- | @- subscribeAttributeClusterRevisionWithParams:subscriptionEstablished:reportHandler:@
subscribeAttributeClusterRevisionWithParams_subscriptionEstablished_reportHandler :: (IsMTRBaseClusterRadonConcentrationMeasurement mtrBaseClusterRadonConcentrationMeasurement, IsMTRSubscribeParams params) => mtrBaseClusterRadonConcentrationMeasurement -> params -> Ptr () -> Ptr () -> IO ()
subscribeAttributeClusterRevisionWithParams_subscriptionEstablished_reportHandler mtrBaseClusterRadonConcentrationMeasurement params subscriptionEstablished reportHandler =
  sendMessage mtrBaseClusterRadonConcentrationMeasurement subscribeAttributeClusterRevisionWithParams_subscriptionEstablished_reportHandlerSelector (toMTRSubscribeParams params) subscriptionEstablished reportHandler

-- | @+ readAttributeClusterRevisionWithClusterStateCache:endpoint:queue:completion:@
readAttributeClusterRevisionWithClusterStateCache_endpoint_queue_completion :: (IsMTRClusterStateCacheContainer clusterStateCacheContainer, IsNSNumber endpoint, IsNSObject queue) => clusterStateCacheContainer -> endpoint -> queue -> Ptr () -> IO ()
readAttributeClusterRevisionWithClusterStateCache_endpoint_queue_completion clusterStateCacheContainer endpoint queue completion =
  do
    cls' <- getRequiredClass "MTRBaseClusterRadonConcentrationMeasurement"
    sendClassMessage cls' readAttributeClusterRevisionWithClusterStateCache_endpoint_queue_completionSelector (toMTRClusterStateCacheContainer clusterStateCacheContainer) (toNSNumber endpoint) (toNSObject queue) completion

-- | @- init@
init_ :: IsMTRBaseClusterRadonConcentrationMeasurement mtrBaseClusterRadonConcentrationMeasurement => mtrBaseClusterRadonConcentrationMeasurement -> IO (Id MTRBaseClusterRadonConcentrationMeasurement)
init_ mtrBaseClusterRadonConcentrationMeasurement =
  sendOwnedMessage mtrBaseClusterRadonConcentrationMeasurement initSelector

-- | @+ new@
new :: IO (Id MTRBaseClusterRadonConcentrationMeasurement)
new  =
  do
    cls' <- getRequiredClass "MTRBaseClusterRadonConcentrationMeasurement"
    sendOwnedClassMessage cls' newSelector

-- | For all instance methods (reads, writes, commands) that take a completion, the completion will be called on the provided queue.
--
-- ObjC selector: @- initWithDevice:endpointID:queue:@
initWithDevice_endpointID_queue :: (IsMTRBaseClusterRadonConcentrationMeasurement mtrBaseClusterRadonConcentrationMeasurement, IsMTRBaseDevice device, IsNSNumber endpointID, IsNSObject queue) => mtrBaseClusterRadonConcentrationMeasurement -> device -> endpointID -> queue -> IO (Id MTRBaseClusterRadonConcentrationMeasurement)
initWithDevice_endpointID_queue mtrBaseClusterRadonConcentrationMeasurement device endpointID queue =
  sendOwnedMessage mtrBaseClusterRadonConcentrationMeasurement initWithDevice_endpointID_queueSelector (toMTRBaseDevice device) (toNSNumber endpointID) (toNSObject queue)

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @readAttributeMeasuredValueWithCompletion:@
readAttributeMeasuredValueWithCompletionSelector :: Selector '[Ptr ()] ()
readAttributeMeasuredValueWithCompletionSelector = mkSelector "readAttributeMeasuredValueWithCompletion:"

-- | @Selector@ for @subscribeAttributeMeasuredValueWithParams:subscriptionEstablished:reportHandler:@
subscribeAttributeMeasuredValueWithParams_subscriptionEstablished_reportHandlerSelector :: Selector '[Id MTRSubscribeParams, Ptr (), Ptr ()] ()
subscribeAttributeMeasuredValueWithParams_subscriptionEstablished_reportHandlerSelector = mkSelector "subscribeAttributeMeasuredValueWithParams:subscriptionEstablished:reportHandler:"

-- | @Selector@ for @readAttributeMeasuredValueWithClusterStateCache:endpoint:queue:completion:@
readAttributeMeasuredValueWithClusterStateCache_endpoint_queue_completionSelector :: Selector '[Id MTRClusterStateCacheContainer, Id NSNumber, Id NSObject, Ptr ()] ()
readAttributeMeasuredValueWithClusterStateCache_endpoint_queue_completionSelector = mkSelector "readAttributeMeasuredValueWithClusterStateCache:endpoint:queue:completion:"

-- | @Selector@ for @readAttributeMinMeasuredValueWithCompletion:@
readAttributeMinMeasuredValueWithCompletionSelector :: Selector '[Ptr ()] ()
readAttributeMinMeasuredValueWithCompletionSelector = mkSelector "readAttributeMinMeasuredValueWithCompletion:"

-- | @Selector@ for @subscribeAttributeMinMeasuredValueWithParams:subscriptionEstablished:reportHandler:@
subscribeAttributeMinMeasuredValueWithParams_subscriptionEstablished_reportHandlerSelector :: Selector '[Id MTRSubscribeParams, Ptr (), Ptr ()] ()
subscribeAttributeMinMeasuredValueWithParams_subscriptionEstablished_reportHandlerSelector = mkSelector "subscribeAttributeMinMeasuredValueWithParams:subscriptionEstablished:reportHandler:"

-- | @Selector@ for @readAttributeMinMeasuredValueWithClusterStateCache:endpoint:queue:completion:@
readAttributeMinMeasuredValueWithClusterStateCache_endpoint_queue_completionSelector :: Selector '[Id MTRClusterStateCacheContainer, Id NSNumber, Id NSObject, Ptr ()] ()
readAttributeMinMeasuredValueWithClusterStateCache_endpoint_queue_completionSelector = mkSelector "readAttributeMinMeasuredValueWithClusterStateCache:endpoint:queue:completion:"

-- | @Selector@ for @readAttributeMaxMeasuredValueWithCompletion:@
readAttributeMaxMeasuredValueWithCompletionSelector :: Selector '[Ptr ()] ()
readAttributeMaxMeasuredValueWithCompletionSelector = mkSelector "readAttributeMaxMeasuredValueWithCompletion:"

-- | @Selector@ for @subscribeAttributeMaxMeasuredValueWithParams:subscriptionEstablished:reportHandler:@
subscribeAttributeMaxMeasuredValueWithParams_subscriptionEstablished_reportHandlerSelector :: Selector '[Id MTRSubscribeParams, Ptr (), Ptr ()] ()
subscribeAttributeMaxMeasuredValueWithParams_subscriptionEstablished_reportHandlerSelector = mkSelector "subscribeAttributeMaxMeasuredValueWithParams:subscriptionEstablished:reportHandler:"

-- | @Selector@ for @readAttributeMaxMeasuredValueWithClusterStateCache:endpoint:queue:completion:@
readAttributeMaxMeasuredValueWithClusterStateCache_endpoint_queue_completionSelector :: Selector '[Id MTRClusterStateCacheContainer, Id NSNumber, Id NSObject, Ptr ()] ()
readAttributeMaxMeasuredValueWithClusterStateCache_endpoint_queue_completionSelector = mkSelector "readAttributeMaxMeasuredValueWithClusterStateCache:endpoint:queue:completion:"

-- | @Selector@ for @readAttributePeakMeasuredValueWithCompletion:@
readAttributePeakMeasuredValueWithCompletionSelector :: Selector '[Ptr ()] ()
readAttributePeakMeasuredValueWithCompletionSelector = mkSelector "readAttributePeakMeasuredValueWithCompletion:"

-- | @Selector@ for @subscribeAttributePeakMeasuredValueWithParams:subscriptionEstablished:reportHandler:@
subscribeAttributePeakMeasuredValueWithParams_subscriptionEstablished_reportHandlerSelector :: Selector '[Id MTRSubscribeParams, Ptr (), Ptr ()] ()
subscribeAttributePeakMeasuredValueWithParams_subscriptionEstablished_reportHandlerSelector = mkSelector "subscribeAttributePeakMeasuredValueWithParams:subscriptionEstablished:reportHandler:"

-- | @Selector@ for @readAttributePeakMeasuredValueWithClusterStateCache:endpoint:queue:completion:@
readAttributePeakMeasuredValueWithClusterStateCache_endpoint_queue_completionSelector :: Selector '[Id MTRClusterStateCacheContainer, Id NSNumber, Id NSObject, Ptr ()] ()
readAttributePeakMeasuredValueWithClusterStateCache_endpoint_queue_completionSelector = mkSelector "readAttributePeakMeasuredValueWithClusterStateCache:endpoint:queue:completion:"

-- | @Selector@ for @readAttributePeakMeasuredValueWindowWithCompletion:@
readAttributePeakMeasuredValueWindowWithCompletionSelector :: Selector '[Ptr ()] ()
readAttributePeakMeasuredValueWindowWithCompletionSelector = mkSelector "readAttributePeakMeasuredValueWindowWithCompletion:"

-- | @Selector@ for @subscribeAttributePeakMeasuredValueWindowWithParams:subscriptionEstablished:reportHandler:@
subscribeAttributePeakMeasuredValueWindowWithParams_subscriptionEstablished_reportHandlerSelector :: Selector '[Id MTRSubscribeParams, Ptr (), Ptr ()] ()
subscribeAttributePeakMeasuredValueWindowWithParams_subscriptionEstablished_reportHandlerSelector = mkSelector "subscribeAttributePeakMeasuredValueWindowWithParams:subscriptionEstablished:reportHandler:"

-- | @Selector@ for @readAttributePeakMeasuredValueWindowWithClusterStateCache:endpoint:queue:completion:@
readAttributePeakMeasuredValueWindowWithClusterStateCache_endpoint_queue_completionSelector :: Selector '[Id MTRClusterStateCacheContainer, Id NSNumber, Id NSObject, Ptr ()] ()
readAttributePeakMeasuredValueWindowWithClusterStateCache_endpoint_queue_completionSelector = mkSelector "readAttributePeakMeasuredValueWindowWithClusterStateCache:endpoint:queue:completion:"

-- | @Selector@ for @readAttributeAverageMeasuredValueWithCompletion:@
readAttributeAverageMeasuredValueWithCompletionSelector :: Selector '[Ptr ()] ()
readAttributeAverageMeasuredValueWithCompletionSelector = mkSelector "readAttributeAverageMeasuredValueWithCompletion:"

-- | @Selector@ for @subscribeAttributeAverageMeasuredValueWithParams:subscriptionEstablished:reportHandler:@
subscribeAttributeAverageMeasuredValueWithParams_subscriptionEstablished_reportHandlerSelector :: Selector '[Id MTRSubscribeParams, Ptr (), Ptr ()] ()
subscribeAttributeAverageMeasuredValueWithParams_subscriptionEstablished_reportHandlerSelector = mkSelector "subscribeAttributeAverageMeasuredValueWithParams:subscriptionEstablished:reportHandler:"

-- | @Selector@ for @readAttributeAverageMeasuredValueWithClusterStateCache:endpoint:queue:completion:@
readAttributeAverageMeasuredValueWithClusterStateCache_endpoint_queue_completionSelector :: Selector '[Id MTRClusterStateCacheContainer, Id NSNumber, Id NSObject, Ptr ()] ()
readAttributeAverageMeasuredValueWithClusterStateCache_endpoint_queue_completionSelector = mkSelector "readAttributeAverageMeasuredValueWithClusterStateCache:endpoint:queue:completion:"

-- | @Selector@ for @readAttributeAverageMeasuredValueWindowWithCompletion:@
readAttributeAverageMeasuredValueWindowWithCompletionSelector :: Selector '[Ptr ()] ()
readAttributeAverageMeasuredValueWindowWithCompletionSelector = mkSelector "readAttributeAverageMeasuredValueWindowWithCompletion:"

-- | @Selector@ for @subscribeAttributeAverageMeasuredValueWindowWithParams:subscriptionEstablished:reportHandler:@
subscribeAttributeAverageMeasuredValueWindowWithParams_subscriptionEstablished_reportHandlerSelector :: Selector '[Id MTRSubscribeParams, Ptr (), Ptr ()] ()
subscribeAttributeAverageMeasuredValueWindowWithParams_subscriptionEstablished_reportHandlerSelector = mkSelector "subscribeAttributeAverageMeasuredValueWindowWithParams:subscriptionEstablished:reportHandler:"

-- | @Selector@ for @readAttributeAverageMeasuredValueWindowWithClusterStateCache:endpoint:queue:completion:@
readAttributeAverageMeasuredValueWindowWithClusterStateCache_endpoint_queue_completionSelector :: Selector '[Id MTRClusterStateCacheContainer, Id NSNumber, Id NSObject, Ptr ()] ()
readAttributeAverageMeasuredValueWindowWithClusterStateCache_endpoint_queue_completionSelector = mkSelector "readAttributeAverageMeasuredValueWindowWithClusterStateCache:endpoint:queue:completion:"

-- | @Selector@ for @readAttributeUncertaintyWithCompletion:@
readAttributeUncertaintyWithCompletionSelector :: Selector '[Ptr ()] ()
readAttributeUncertaintyWithCompletionSelector = mkSelector "readAttributeUncertaintyWithCompletion:"

-- | @Selector@ for @subscribeAttributeUncertaintyWithParams:subscriptionEstablished:reportHandler:@
subscribeAttributeUncertaintyWithParams_subscriptionEstablished_reportHandlerSelector :: Selector '[Id MTRSubscribeParams, Ptr (), Ptr ()] ()
subscribeAttributeUncertaintyWithParams_subscriptionEstablished_reportHandlerSelector = mkSelector "subscribeAttributeUncertaintyWithParams:subscriptionEstablished:reportHandler:"

-- | @Selector@ for @readAttributeUncertaintyWithClusterStateCache:endpoint:queue:completion:@
readAttributeUncertaintyWithClusterStateCache_endpoint_queue_completionSelector :: Selector '[Id MTRClusterStateCacheContainer, Id NSNumber, Id NSObject, Ptr ()] ()
readAttributeUncertaintyWithClusterStateCache_endpoint_queue_completionSelector = mkSelector "readAttributeUncertaintyWithClusterStateCache:endpoint:queue:completion:"

-- | @Selector@ for @readAttributeMeasurementUnitWithCompletion:@
readAttributeMeasurementUnitWithCompletionSelector :: Selector '[Ptr ()] ()
readAttributeMeasurementUnitWithCompletionSelector = mkSelector "readAttributeMeasurementUnitWithCompletion:"

-- | @Selector@ for @subscribeAttributeMeasurementUnitWithParams:subscriptionEstablished:reportHandler:@
subscribeAttributeMeasurementUnitWithParams_subscriptionEstablished_reportHandlerSelector :: Selector '[Id MTRSubscribeParams, Ptr (), Ptr ()] ()
subscribeAttributeMeasurementUnitWithParams_subscriptionEstablished_reportHandlerSelector = mkSelector "subscribeAttributeMeasurementUnitWithParams:subscriptionEstablished:reportHandler:"

-- | @Selector@ for @readAttributeMeasurementUnitWithClusterStateCache:endpoint:queue:completion:@
readAttributeMeasurementUnitWithClusterStateCache_endpoint_queue_completionSelector :: Selector '[Id MTRClusterStateCacheContainer, Id NSNumber, Id NSObject, Ptr ()] ()
readAttributeMeasurementUnitWithClusterStateCache_endpoint_queue_completionSelector = mkSelector "readAttributeMeasurementUnitWithClusterStateCache:endpoint:queue:completion:"

-- | @Selector@ for @readAttributeMeasurementMediumWithCompletion:@
readAttributeMeasurementMediumWithCompletionSelector :: Selector '[Ptr ()] ()
readAttributeMeasurementMediumWithCompletionSelector = mkSelector "readAttributeMeasurementMediumWithCompletion:"

-- | @Selector@ for @subscribeAttributeMeasurementMediumWithParams:subscriptionEstablished:reportHandler:@
subscribeAttributeMeasurementMediumWithParams_subscriptionEstablished_reportHandlerSelector :: Selector '[Id MTRSubscribeParams, Ptr (), Ptr ()] ()
subscribeAttributeMeasurementMediumWithParams_subscriptionEstablished_reportHandlerSelector = mkSelector "subscribeAttributeMeasurementMediumWithParams:subscriptionEstablished:reportHandler:"

-- | @Selector@ for @readAttributeMeasurementMediumWithClusterStateCache:endpoint:queue:completion:@
readAttributeMeasurementMediumWithClusterStateCache_endpoint_queue_completionSelector :: Selector '[Id MTRClusterStateCacheContainer, Id NSNumber, Id NSObject, Ptr ()] ()
readAttributeMeasurementMediumWithClusterStateCache_endpoint_queue_completionSelector = mkSelector "readAttributeMeasurementMediumWithClusterStateCache:endpoint:queue:completion:"

-- | @Selector@ for @readAttributeLevelValueWithCompletion:@
readAttributeLevelValueWithCompletionSelector :: Selector '[Ptr ()] ()
readAttributeLevelValueWithCompletionSelector = mkSelector "readAttributeLevelValueWithCompletion:"

-- | @Selector@ for @subscribeAttributeLevelValueWithParams:subscriptionEstablished:reportHandler:@
subscribeAttributeLevelValueWithParams_subscriptionEstablished_reportHandlerSelector :: Selector '[Id MTRSubscribeParams, Ptr (), Ptr ()] ()
subscribeAttributeLevelValueWithParams_subscriptionEstablished_reportHandlerSelector = mkSelector "subscribeAttributeLevelValueWithParams:subscriptionEstablished:reportHandler:"

-- | @Selector@ for @readAttributeLevelValueWithClusterStateCache:endpoint:queue:completion:@
readAttributeLevelValueWithClusterStateCache_endpoint_queue_completionSelector :: Selector '[Id MTRClusterStateCacheContainer, Id NSNumber, Id NSObject, Ptr ()] ()
readAttributeLevelValueWithClusterStateCache_endpoint_queue_completionSelector = mkSelector "readAttributeLevelValueWithClusterStateCache:endpoint:queue:completion:"

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
initSelector :: Selector '[] (Id MTRBaseClusterRadonConcentrationMeasurement)
initSelector = mkSelector "init"

-- | @Selector@ for @new@
newSelector :: Selector '[] (Id MTRBaseClusterRadonConcentrationMeasurement)
newSelector = mkSelector "new"

-- | @Selector@ for @initWithDevice:endpointID:queue:@
initWithDevice_endpointID_queueSelector :: Selector '[Id MTRBaseDevice, Id NSNumber, Id NSObject] (Id MTRBaseClusterRadonConcentrationMeasurement)
initWithDevice_endpointID_queueSelector = mkSelector "initWithDevice:endpointID:queue:"

