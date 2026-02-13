{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Cluster Electrical Energy Measurement
--
-- This cluster provides a mechanism for querying data about the electrical energy imported or provided by the server.
--
-- Generated bindings for @MTRBaseClusterElectricalEnergyMeasurement@.
module ObjC.Matter.MTRBaseClusterElectricalEnergyMeasurement
  ( MTRBaseClusterElectricalEnergyMeasurement
  , IsMTRBaseClusterElectricalEnergyMeasurement(..)
  , readAttributeAccuracyWithCompletion
  , subscribeAttributeAccuracyWithParams_subscriptionEstablished_reportHandler
  , readAttributeAccuracyWithClusterStateCache_endpoint_queue_completion
  , readAttributeCumulativeEnergyImportedWithCompletion
  , subscribeAttributeCumulativeEnergyImportedWithParams_subscriptionEstablished_reportHandler
  , readAttributeCumulativeEnergyImportedWithClusterStateCache_endpoint_queue_completion
  , readAttributeCumulativeEnergyExportedWithCompletion
  , subscribeAttributeCumulativeEnergyExportedWithParams_subscriptionEstablished_reportHandler
  , readAttributeCumulativeEnergyExportedWithClusterStateCache_endpoint_queue_completion
  , readAttributePeriodicEnergyImportedWithCompletion
  , subscribeAttributePeriodicEnergyImportedWithParams_subscriptionEstablished_reportHandler
  , readAttributePeriodicEnergyImportedWithClusterStateCache_endpoint_queue_completion
  , readAttributePeriodicEnergyExportedWithCompletion
  , subscribeAttributePeriodicEnergyExportedWithParams_subscriptionEstablished_reportHandler
  , readAttributePeriodicEnergyExportedWithClusterStateCache_endpoint_queue_completion
  , readAttributeCumulativeEnergyResetWithCompletion
  , subscribeAttributeCumulativeEnergyResetWithParams_subscriptionEstablished_reportHandler
  , readAttributeCumulativeEnergyResetWithClusterStateCache_endpoint_queue_completion
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
  , readAttributeAccuracyWithClusterStateCache_endpoint_queue_completionSelector
  , readAttributeAccuracyWithCompletionSelector
  , readAttributeAttributeListWithClusterStateCache_endpoint_queue_completionSelector
  , readAttributeAttributeListWithCompletionSelector
  , readAttributeClusterRevisionWithClusterStateCache_endpoint_queue_completionSelector
  , readAttributeClusterRevisionWithCompletionSelector
  , readAttributeCumulativeEnergyExportedWithClusterStateCache_endpoint_queue_completionSelector
  , readAttributeCumulativeEnergyExportedWithCompletionSelector
  , readAttributeCumulativeEnergyImportedWithClusterStateCache_endpoint_queue_completionSelector
  , readAttributeCumulativeEnergyImportedWithCompletionSelector
  , readAttributeCumulativeEnergyResetWithClusterStateCache_endpoint_queue_completionSelector
  , readAttributeCumulativeEnergyResetWithCompletionSelector
  , readAttributeFeatureMapWithClusterStateCache_endpoint_queue_completionSelector
  , readAttributeFeatureMapWithCompletionSelector
  , readAttributeGeneratedCommandListWithClusterStateCache_endpoint_queue_completionSelector
  , readAttributeGeneratedCommandListWithCompletionSelector
  , readAttributePeriodicEnergyExportedWithClusterStateCache_endpoint_queue_completionSelector
  , readAttributePeriodicEnergyExportedWithCompletionSelector
  , readAttributePeriodicEnergyImportedWithClusterStateCache_endpoint_queue_completionSelector
  , readAttributePeriodicEnergyImportedWithCompletionSelector
  , subscribeAttributeAcceptedCommandListWithParams_subscriptionEstablished_reportHandlerSelector
  , subscribeAttributeAccuracyWithParams_subscriptionEstablished_reportHandlerSelector
  , subscribeAttributeAttributeListWithParams_subscriptionEstablished_reportHandlerSelector
  , subscribeAttributeClusterRevisionWithParams_subscriptionEstablished_reportHandlerSelector
  , subscribeAttributeCumulativeEnergyExportedWithParams_subscriptionEstablished_reportHandlerSelector
  , subscribeAttributeCumulativeEnergyImportedWithParams_subscriptionEstablished_reportHandlerSelector
  , subscribeAttributeCumulativeEnergyResetWithParams_subscriptionEstablished_reportHandlerSelector
  , subscribeAttributeFeatureMapWithParams_subscriptionEstablished_reportHandlerSelector
  , subscribeAttributeGeneratedCommandListWithParams_subscriptionEstablished_reportHandlerSelector
  , subscribeAttributePeriodicEnergyExportedWithParams_subscriptionEstablished_reportHandlerSelector
  , subscribeAttributePeriodicEnergyImportedWithParams_subscriptionEstablished_reportHandlerSelector


  ) where

import Foreign.Ptr (Ptr, FunPtr)
import Foreign.C.Types

import ObjC.Runtime.Types
import ObjC.Runtime.Message (sendMessage, sendOwnedMessage, sendClassMessage, sendOwnedClassMessage)
import ObjC.Runtime.Selector (mkSelector)
import ObjC.Runtime.Class (getRequiredClass)

import ObjC.Matter.Internal.Classes
import ObjC.Foundation.Internal.Classes

-- | @- readAttributeAccuracyWithCompletion:@
readAttributeAccuracyWithCompletion :: IsMTRBaseClusterElectricalEnergyMeasurement mtrBaseClusterElectricalEnergyMeasurement => mtrBaseClusterElectricalEnergyMeasurement -> Ptr () -> IO ()
readAttributeAccuracyWithCompletion mtrBaseClusterElectricalEnergyMeasurement completion =
  sendMessage mtrBaseClusterElectricalEnergyMeasurement readAttributeAccuracyWithCompletionSelector completion

-- | @- subscribeAttributeAccuracyWithParams:subscriptionEstablished:reportHandler:@
subscribeAttributeAccuracyWithParams_subscriptionEstablished_reportHandler :: (IsMTRBaseClusterElectricalEnergyMeasurement mtrBaseClusterElectricalEnergyMeasurement, IsMTRSubscribeParams params) => mtrBaseClusterElectricalEnergyMeasurement -> params -> Ptr () -> Ptr () -> IO ()
subscribeAttributeAccuracyWithParams_subscriptionEstablished_reportHandler mtrBaseClusterElectricalEnergyMeasurement params subscriptionEstablished reportHandler =
  sendMessage mtrBaseClusterElectricalEnergyMeasurement subscribeAttributeAccuracyWithParams_subscriptionEstablished_reportHandlerSelector (toMTRSubscribeParams params) subscriptionEstablished reportHandler

-- | @+ readAttributeAccuracyWithClusterStateCache:endpoint:queue:completion:@
readAttributeAccuracyWithClusterStateCache_endpoint_queue_completion :: (IsMTRClusterStateCacheContainer clusterStateCacheContainer, IsNSNumber endpoint, IsNSObject queue) => clusterStateCacheContainer -> endpoint -> queue -> Ptr () -> IO ()
readAttributeAccuracyWithClusterStateCache_endpoint_queue_completion clusterStateCacheContainer endpoint queue completion =
  do
    cls' <- getRequiredClass "MTRBaseClusterElectricalEnergyMeasurement"
    sendClassMessage cls' readAttributeAccuracyWithClusterStateCache_endpoint_queue_completionSelector (toMTRClusterStateCacheContainer clusterStateCacheContainer) (toNSNumber endpoint) (toNSObject queue) completion

-- | @- readAttributeCumulativeEnergyImportedWithCompletion:@
readAttributeCumulativeEnergyImportedWithCompletion :: IsMTRBaseClusterElectricalEnergyMeasurement mtrBaseClusterElectricalEnergyMeasurement => mtrBaseClusterElectricalEnergyMeasurement -> Ptr () -> IO ()
readAttributeCumulativeEnergyImportedWithCompletion mtrBaseClusterElectricalEnergyMeasurement completion =
  sendMessage mtrBaseClusterElectricalEnergyMeasurement readAttributeCumulativeEnergyImportedWithCompletionSelector completion

-- | @- subscribeAttributeCumulativeEnergyImportedWithParams:subscriptionEstablished:reportHandler:@
subscribeAttributeCumulativeEnergyImportedWithParams_subscriptionEstablished_reportHandler :: (IsMTRBaseClusterElectricalEnergyMeasurement mtrBaseClusterElectricalEnergyMeasurement, IsMTRSubscribeParams params) => mtrBaseClusterElectricalEnergyMeasurement -> params -> Ptr () -> Ptr () -> IO ()
subscribeAttributeCumulativeEnergyImportedWithParams_subscriptionEstablished_reportHandler mtrBaseClusterElectricalEnergyMeasurement params subscriptionEstablished reportHandler =
  sendMessage mtrBaseClusterElectricalEnergyMeasurement subscribeAttributeCumulativeEnergyImportedWithParams_subscriptionEstablished_reportHandlerSelector (toMTRSubscribeParams params) subscriptionEstablished reportHandler

-- | @+ readAttributeCumulativeEnergyImportedWithClusterStateCache:endpoint:queue:completion:@
readAttributeCumulativeEnergyImportedWithClusterStateCache_endpoint_queue_completion :: (IsMTRClusterStateCacheContainer clusterStateCacheContainer, IsNSNumber endpoint, IsNSObject queue) => clusterStateCacheContainer -> endpoint -> queue -> Ptr () -> IO ()
readAttributeCumulativeEnergyImportedWithClusterStateCache_endpoint_queue_completion clusterStateCacheContainer endpoint queue completion =
  do
    cls' <- getRequiredClass "MTRBaseClusterElectricalEnergyMeasurement"
    sendClassMessage cls' readAttributeCumulativeEnergyImportedWithClusterStateCache_endpoint_queue_completionSelector (toMTRClusterStateCacheContainer clusterStateCacheContainer) (toNSNumber endpoint) (toNSObject queue) completion

-- | @- readAttributeCumulativeEnergyExportedWithCompletion:@
readAttributeCumulativeEnergyExportedWithCompletion :: IsMTRBaseClusterElectricalEnergyMeasurement mtrBaseClusterElectricalEnergyMeasurement => mtrBaseClusterElectricalEnergyMeasurement -> Ptr () -> IO ()
readAttributeCumulativeEnergyExportedWithCompletion mtrBaseClusterElectricalEnergyMeasurement completion =
  sendMessage mtrBaseClusterElectricalEnergyMeasurement readAttributeCumulativeEnergyExportedWithCompletionSelector completion

-- | @- subscribeAttributeCumulativeEnergyExportedWithParams:subscriptionEstablished:reportHandler:@
subscribeAttributeCumulativeEnergyExportedWithParams_subscriptionEstablished_reportHandler :: (IsMTRBaseClusterElectricalEnergyMeasurement mtrBaseClusterElectricalEnergyMeasurement, IsMTRSubscribeParams params) => mtrBaseClusterElectricalEnergyMeasurement -> params -> Ptr () -> Ptr () -> IO ()
subscribeAttributeCumulativeEnergyExportedWithParams_subscriptionEstablished_reportHandler mtrBaseClusterElectricalEnergyMeasurement params subscriptionEstablished reportHandler =
  sendMessage mtrBaseClusterElectricalEnergyMeasurement subscribeAttributeCumulativeEnergyExportedWithParams_subscriptionEstablished_reportHandlerSelector (toMTRSubscribeParams params) subscriptionEstablished reportHandler

-- | @+ readAttributeCumulativeEnergyExportedWithClusterStateCache:endpoint:queue:completion:@
readAttributeCumulativeEnergyExportedWithClusterStateCache_endpoint_queue_completion :: (IsMTRClusterStateCacheContainer clusterStateCacheContainer, IsNSNumber endpoint, IsNSObject queue) => clusterStateCacheContainer -> endpoint -> queue -> Ptr () -> IO ()
readAttributeCumulativeEnergyExportedWithClusterStateCache_endpoint_queue_completion clusterStateCacheContainer endpoint queue completion =
  do
    cls' <- getRequiredClass "MTRBaseClusterElectricalEnergyMeasurement"
    sendClassMessage cls' readAttributeCumulativeEnergyExportedWithClusterStateCache_endpoint_queue_completionSelector (toMTRClusterStateCacheContainer clusterStateCacheContainer) (toNSNumber endpoint) (toNSObject queue) completion

-- | @- readAttributePeriodicEnergyImportedWithCompletion:@
readAttributePeriodicEnergyImportedWithCompletion :: IsMTRBaseClusterElectricalEnergyMeasurement mtrBaseClusterElectricalEnergyMeasurement => mtrBaseClusterElectricalEnergyMeasurement -> Ptr () -> IO ()
readAttributePeriodicEnergyImportedWithCompletion mtrBaseClusterElectricalEnergyMeasurement completion =
  sendMessage mtrBaseClusterElectricalEnergyMeasurement readAttributePeriodicEnergyImportedWithCompletionSelector completion

-- | @- subscribeAttributePeriodicEnergyImportedWithParams:subscriptionEstablished:reportHandler:@
subscribeAttributePeriodicEnergyImportedWithParams_subscriptionEstablished_reportHandler :: (IsMTRBaseClusterElectricalEnergyMeasurement mtrBaseClusterElectricalEnergyMeasurement, IsMTRSubscribeParams params) => mtrBaseClusterElectricalEnergyMeasurement -> params -> Ptr () -> Ptr () -> IO ()
subscribeAttributePeriodicEnergyImportedWithParams_subscriptionEstablished_reportHandler mtrBaseClusterElectricalEnergyMeasurement params subscriptionEstablished reportHandler =
  sendMessage mtrBaseClusterElectricalEnergyMeasurement subscribeAttributePeriodicEnergyImportedWithParams_subscriptionEstablished_reportHandlerSelector (toMTRSubscribeParams params) subscriptionEstablished reportHandler

-- | @+ readAttributePeriodicEnergyImportedWithClusterStateCache:endpoint:queue:completion:@
readAttributePeriodicEnergyImportedWithClusterStateCache_endpoint_queue_completion :: (IsMTRClusterStateCacheContainer clusterStateCacheContainer, IsNSNumber endpoint, IsNSObject queue) => clusterStateCacheContainer -> endpoint -> queue -> Ptr () -> IO ()
readAttributePeriodicEnergyImportedWithClusterStateCache_endpoint_queue_completion clusterStateCacheContainer endpoint queue completion =
  do
    cls' <- getRequiredClass "MTRBaseClusterElectricalEnergyMeasurement"
    sendClassMessage cls' readAttributePeriodicEnergyImportedWithClusterStateCache_endpoint_queue_completionSelector (toMTRClusterStateCacheContainer clusterStateCacheContainer) (toNSNumber endpoint) (toNSObject queue) completion

-- | @- readAttributePeriodicEnergyExportedWithCompletion:@
readAttributePeriodicEnergyExportedWithCompletion :: IsMTRBaseClusterElectricalEnergyMeasurement mtrBaseClusterElectricalEnergyMeasurement => mtrBaseClusterElectricalEnergyMeasurement -> Ptr () -> IO ()
readAttributePeriodicEnergyExportedWithCompletion mtrBaseClusterElectricalEnergyMeasurement completion =
  sendMessage mtrBaseClusterElectricalEnergyMeasurement readAttributePeriodicEnergyExportedWithCompletionSelector completion

-- | @- subscribeAttributePeriodicEnergyExportedWithParams:subscriptionEstablished:reportHandler:@
subscribeAttributePeriodicEnergyExportedWithParams_subscriptionEstablished_reportHandler :: (IsMTRBaseClusterElectricalEnergyMeasurement mtrBaseClusterElectricalEnergyMeasurement, IsMTRSubscribeParams params) => mtrBaseClusterElectricalEnergyMeasurement -> params -> Ptr () -> Ptr () -> IO ()
subscribeAttributePeriodicEnergyExportedWithParams_subscriptionEstablished_reportHandler mtrBaseClusterElectricalEnergyMeasurement params subscriptionEstablished reportHandler =
  sendMessage mtrBaseClusterElectricalEnergyMeasurement subscribeAttributePeriodicEnergyExportedWithParams_subscriptionEstablished_reportHandlerSelector (toMTRSubscribeParams params) subscriptionEstablished reportHandler

-- | @+ readAttributePeriodicEnergyExportedWithClusterStateCache:endpoint:queue:completion:@
readAttributePeriodicEnergyExportedWithClusterStateCache_endpoint_queue_completion :: (IsMTRClusterStateCacheContainer clusterStateCacheContainer, IsNSNumber endpoint, IsNSObject queue) => clusterStateCacheContainer -> endpoint -> queue -> Ptr () -> IO ()
readAttributePeriodicEnergyExportedWithClusterStateCache_endpoint_queue_completion clusterStateCacheContainer endpoint queue completion =
  do
    cls' <- getRequiredClass "MTRBaseClusterElectricalEnergyMeasurement"
    sendClassMessage cls' readAttributePeriodicEnergyExportedWithClusterStateCache_endpoint_queue_completionSelector (toMTRClusterStateCacheContainer clusterStateCacheContainer) (toNSNumber endpoint) (toNSObject queue) completion

-- | @- readAttributeCumulativeEnergyResetWithCompletion:@
readAttributeCumulativeEnergyResetWithCompletion :: IsMTRBaseClusterElectricalEnergyMeasurement mtrBaseClusterElectricalEnergyMeasurement => mtrBaseClusterElectricalEnergyMeasurement -> Ptr () -> IO ()
readAttributeCumulativeEnergyResetWithCompletion mtrBaseClusterElectricalEnergyMeasurement completion =
  sendMessage mtrBaseClusterElectricalEnergyMeasurement readAttributeCumulativeEnergyResetWithCompletionSelector completion

-- | @- subscribeAttributeCumulativeEnergyResetWithParams:subscriptionEstablished:reportHandler:@
subscribeAttributeCumulativeEnergyResetWithParams_subscriptionEstablished_reportHandler :: (IsMTRBaseClusterElectricalEnergyMeasurement mtrBaseClusterElectricalEnergyMeasurement, IsMTRSubscribeParams params) => mtrBaseClusterElectricalEnergyMeasurement -> params -> Ptr () -> Ptr () -> IO ()
subscribeAttributeCumulativeEnergyResetWithParams_subscriptionEstablished_reportHandler mtrBaseClusterElectricalEnergyMeasurement params subscriptionEstablished reportHandler =
  sendMessage mtrBaseClusterElectricalEnergyMeasurement subscribeAttributeCumulativeEnergyResetWithParams_subscriptionEstablished_reportHandlerSelector (toMTRSubscribeParams params) subscriptionEstablished reportHandler

-- | @+ readAttributeCumulativeEnergyResetWithClusterStateCache:endpoint:queue:completion:@
readAttributeCumulativeEnergyResetWithClusterStateCache_endpoint_queue_completion :: (IsMTRClusterStateCacheContainer clusterStateCacheContainer, IsNSNumber endpoint, IsNSObject queue) => clusterStateCacheContainer -> endpoint -> queue -> Ptr () -> IO ()
readAttributeCumulativeEnergyResetWithClusterStateCache_endpoint_queue_completion clusterStateCacheContainer endpoint queue completion =
  do
    cls' <- getRequiredClass "MTRBaseClusterElectricalEnergyMeasurement"
    sendClassMessage cls' readAttributeCumulativeEnergyResetWithClusterStateCache_endpoint_queue_completionSelector (toMTRClusterStateCacheContainer clusterStateCacheContainer) (toNSNumber endpoint) (toNSObject queue) completion

-- | @- readAttributeGeneratedCommandListWithCompletion:@
readAttributeGeneratedCommandListWithCompletion :: IsMTRBaseClusterElectricalEnergyMeasurement mtrBaseClusterElectricalEnergyMeasurement => mtrBaseClusterElectricalEnergyMeasurement -> Ptr () -> IO ()
readAttributeGeneratedCommandListWithCompletion mtrBaseClusterElectricalEnergyMeasurement completion =
  sendMessage mtrBaseClusterElectricalEnergyMeasurement readAttributeGeneratedCommandListWithCompletionSelector completion

-- | @- subscribeAttributeGeneratedCommandListWithParams:subscriptionEstablished:reportHandler:@
subscribeAttributeGeneratedCommandListWithParams_subscriptionEstablished_reportHandler :: (IsMTRBaseClusterElectricalEnergyMeasurement mtrBaseClusterElectricalEnergyMeasurement, IsMTRSubscribeParams params) => mtrBaseClusterElectricalEnergyMeasurement -> params -> Ptr () -> Ptr () -> IO ()
subscribeAttributeGeneratedCommandListWithParams_subscriptionEstablished_reportHandler mtrBaseClusterElectricalEnergyMeasurement params subscriptionEstablished reportHandler =
  sendMessage mtrBaseClusterElectricalEnergyMeasurement subscribeAttributeGeneratedCommandListWithParams_subscriptionEstablished_reportHandlerSelector (toMTRSubscribeParams params) subscriptionEstablished reportHandler

-- | @+ readAttributeGeneratedCommandListWithClusterStateCache:endpoint:queue:completion:@
readAttributeGeneratedCommandListWithClusterStateCache_endpoint_queue_completion :: (IsMTRClusterStateCacheContainer clusterStateCacheContainer, IsNSNumber endpoint, IsNSObject queue) => clusterStateCacheContainer -> endpoint -> queue -> Ptr () -> IO ()
readAttributeGeneratedCommandListWithClusterStateCache_endpoint_queue_completion clusterStateCacheContainer endpoint queue completion =
  do
    cls' <- getRequiredClass "MTRBaseClusterElectricalEnergyMeasurement"
    sendClassMessage cls' readAttributeGeneratedCommandListWithClusterStateCache_endpoint_queue_completionSelector (toMTRClusterStateCacheContainer clusterStateCacheContainer) (toNSNumber endpoint) (toNSObject queue) completion

-- | @- readAttributeAcceptedCommandListWithCompletion:@
readAttributeAcceptedCommandListWithCompletion :: IsMTRBaseClusterElectricalEnergyMeasurement mtrBaseClusterElectricalEnergyMeasurement => mtrBaseClusterElectricalEnergyMeasurement -> Ptr () -> IO ()
readAttributeAcceptedCommandListWithCompletion mtrBaseClusterElectricalEnergyMeasurement completion =
  sendMessage mtrBaseClusterElectricalEnergyMeasurement readAttributeAcceptedCommandListWithCompletionSelector completion

-- | @- subscribeAttributeAcceptedCommandListWithParams:subscriptionEstablished:reportHandler:@
subscribeAttributeAcceptedCommandListWithParams_subscriptionEstablished_reportHandler :: (IsMTRBaseClusterElectricalEnergyMeasurement mtrBaseClusterElectricalEnergyMeasurement, IsMTRSubscribeParams params) => mtrBaseClusterElectricalEnergyMeasurement -> params -> Ptr () -> Ptr () -> IO ()
subscribeAttributeAcceptedCommandListWithParams_subscriptionEstablished_reportHandler mtrBaseClusterElectricalEnergyMeasurement params subscriptionEstablished reportHandler =
  sendMessage mtrBaseClusterElectricalEnergyMeasurement subscribeAttributeAcceptedCommandListWithParams_subscriptionEstablished_reportHandlerSelector (toMTRSubscribeParams params) subscriptionEstablished reportHandler

-- | @+ readAttributeAcceptedCommandListWithClusterStateCache:endpoint:queue:completion:@
readAttributeAcceptedCommandListWithClusterStateCache_endpoint_queue_completion :: (IsMTRClusterStateCacheContainer clusterStateCacheContainer, IsNSNumber endpoint, IsNSObject queue) => clusterStateCacheContainer -> endpoint -> queue -> Ptr () -> IO ()
readAttributeAcceptedCommandListWithClusterStateCache_endpoint_queue_completion clusterStateCacheContainer endpoint queue completion =
  do
    cls' <- getRequiredClass "MTRBaseClusterElectricalEnergyMeasurement"
    sendClassMessage cls' readAttributeAcceptedCommandListWithClusterStateCache_endpoint_queue_completionSelector (toMTRClusterStateCacheContainer clusterStateCacheContainer) (toNSNumber endpoint) (toNSObject queue) completion

-- | @- readAttributeAttributeListWithCompletion:@
readAttributeAttributeListWithCompletion :: IsMTRBaseClusterElectricalEnergyMeasurement mtrBaseClusterElectricalEnergyMeasurement => mtrBaseClusterElectricalEnergyMeasurement -> Ptr () -> IO ()
readAttributeAttributeListWithCompletion mtrBaseClusterElectricalEnergyMeasurement completion =
  sendMessage mtrBaseClusterElectricalEnergyMeasurement readAttributeAttributeListWithCompletionSelector completion

-- | @- subscribeAttributeAttributeListWithParams:subscriptionEstablished:reportHandler:@
subscribeAttributeAttributeListWithParams_subscriptionEstablished_reportHandler :: (IsMTRBaseClusterElectricalEnergyMeasurement mtrBaseClusterElectricalEnergyMeasurement, IsMTRSubscribeParams params) => mtrBaseClusterElectricalEnergyMeasurement -> params -> Ptr () -> Ptr () -> IO ()
subscribeAttributeAttributeListWithParams_subscriptionEstablished_reportHandler mtrBaseClusterElectricalEnergyMeasurement params subscriptionEstablished reportHandler =
  sendMessage mtrBaseClusterElectricalEnergyMeasurement subscribeAttributeAttributeListWithParams_subscriptionEstablished_reportHandlerSelector (toMTRSubscribeParams params) subscriptionEstablished reportHandler

-- | @+ readAttributeAttributeListWithClusterStateCache:endpoint:queue:completion:@
readAttributeAttributeListWithClusterStateCache_endpoint_queue_completion :: (IsMTRClusterStateCacheContainer clusterStateCacheContainer, IsNSNumber endpoint, IsNSObject queue) => clusterStateCacheContainer -> endpoint -> queue -> Ptr () -> IO ()
readAttributeAttributeListWithClusterStateCache_endpoint_queue_completion clusterStateCacheContainer endpoint queue completion =
  do
    cls' <- getRequiredClass "MTRBaseClusterElectricalEnergyMeasurement"
    sendClassMessage cls' readAttributeAttributeListWithClusterStateCache_endpoint_queue_completionSelector (toMTRClusterStateCacheContainer clusterStateCacheContainer) (toNSNumber endpoint) (toNSObject queue) completion

-- | @- readAttributeFeatureMapWithCompletion:@
readAttributeFeatureMapWithCompletion :: IsMTRBaseClusterElectricalEnergyMeasurement mtrBaseClusterElectricalEnergyMeasurement => mtrBaseClusterElectricalEnergyMeasurement -> Ptr () -> IO ()
readAttributeFeatureMapWithCompletion mtrBaseClusterElectricalEnergyMeasurement completion =
  sendMessage mtrBaseClusterElectricalEnergyMeasurement readAttributeFeatureMapWithCompletionSelector completion

-- | @- subscribeAttributeFeatureMapWithParams:subscriptionEstablished:reportHandler:@
subscribeAttributeFeatureMapWithParams_subscriptionEstablished_reportHandler :: (IsMTRBaseClusterElectricalEnergyMeasurement mtrBaseClusterElectricalEnergyMeasurement, IsMTRSubscribeParams params) => mtrBaseClusterElectricalEnergyMeasurement -> params -> Ptr () -> Ptr () -> IO ()
subscribeAttributeFeatureMapWithParams_subscriptionEstablished_reportHandler mtrBaseClusterElectricalEnergyMeasurement params subscriptionEstablished reportHandler =
  sendMessage mtrBaseClusterElectricalEnergyMeasurement subscribeAttributeFeatureMapWithParams_subscriptionEstablished_reportHandlerSelector (toMTRSubscribeParams params) subscriptionEstablished reportHandler

-- | @+ readAttributeFeatureMapWithClusterStateCache:endpoint:queue:completion:@
readAttributeFeatureMapWithClusterStateCache_endpoint_queue_completion :: (IsMTRClusterStateCacheContainer clusterStateCacheContainer, IsNSNumber endpoint, IsNSObject queue) => clusterStateCacheContainer -> endpoint -> queue -> Ptr () -> IO ()
readAttributeFeatureMapWithClusterStateCache_endpoint_queue_completion clusterStateCacheContainer endpoint queue completion =
  do
    cls' <- getRequiredClass "MTRBaseClusterElectricalEnergyMeasurement"
    sendClassMessage cls' readAttributeFeatureMapWithClusterStateCache_endpoint_queue_completionSelector (toMTRClusterStateCacheContainer clusterStateCacheContainer) (toNSNumber endpoint) (toNSObject queue) completion

-- | @- readAttributeClusterRevisionWithCompletion:@
readAttributeClusterRevisionWithCompletion :: IsMTRBaseClusterElectricalEnergyMeasurement mtrBaseClusterElectricalEnergyMeasurement => mtrBaseClusterElectricalEnergyMeasurement -> Ptr () -> IO ()
readAttributeClusterRevisionWithCompletion mtrBaseClusterElectricalEnergyMeasurement completion =
  sendMessage mtrBaseClusterElectricalEnergyMeasurement readAttributeClusterRevisionWithCompletionSelector completion

-- | @- subscribeAttributeClusterRevisionWithParams:subscriptionEstablished:reportHandler:@
subscribeAttributeClusterRevisionWithParams_subscriptionEstablished_reportHandler :: (IsMTRBaseClusterElectricalEnergyMeasurement mtrBaseClusterElectricalEnergyMeasurement, IsMTRSubscribeParams params) => mtrBaseClusterElectricalEnergyMeasurement -> params -> Ptr () -> Ptr () -> IO ()
subscribeAttributeClusterRevisionWithParams_subscriptionEstablished_reportHandler mtrBaseClusterElectricalEnergyMeasurement params subscriptionEstablished reportHandler =
  sendMessage mtrBaseClusterElectricalEnergyMeasurement subscribeAttributeClusterRevisionWithParams_subscriptionEstablished_reportHandlerSelector (toMTRSubscribeParams params) subscriptionEstablished reportHandler

-- | @+ readAttributeClusterRevisionWithClusterStateCache:endpoint:queue:completion:@
readAttributeClusterRevisionWithClusterStateCache_endpoint_queue_completion :: (IsMTRClusterStateCacheContainer clusterStateCacheContainer, IsNSNumber endpoint, IsNSObject queue) => clusterStateCacheContainer -> endpoint -> queue -> Ptr () -> IO ()
readAttributeClusterRevisionWithClusterStateCache_endpoint_queue_completion clusterStateCacheContainer endpoint queue completion =
  do
    cls' <- getRequiredClass "MTRBaseClusterElectricalEnergyMeasurement"
    sendClassMessage cls' readAttributeClusterRevisionWithClusterStateCache_endpoint_queue_completionSelector (toMTRClusterStateCacheContainer clusterStateCacheContainer) (toNSNumber endpoint) (toNSObject queue) completion

-- | @- init@
init_ :: IsMTRBaseClusterElectricalEnergyMeasurement mtrBaseClusterElectricalEnergyMeasurement => mtrBaseClusterElectricalEnergyMeasurement -> IO (Id MTRBaseClusterElectricalEnergyMeasurement)
init_ mtrBaseClusterElectricalEnergyMeasurement =
  sendOwnedMessage mtrBaseClusterElectricalEnergyMeasurement initSelector

-- | @+ new@
new :: IO (Id MTRBaseClusterElectricalEnergyMeasurement)
new  =
  do
    cls' <- getRequiredClass "MTRBaseClusterElectricalEnergyMeasurement"
    sendOwnedClassMessage cls' newSelector

-- | For all instance methods (reads, writes, commands) that take a completion, the completion will be called on the provided queue.
--
-- ObjC selector: @- initWithDevice:endpointID:queue:@
initWithDevice_endpointID_queue :: (IsMTRBaseClusterElectricalEnergyMeasurement mtrBaseClusterElectricalEnergyMeasurement, IsMTRBaseDevice device, IsNSNumber endpointID, IsNSObject queue) => mtrBaseClusterElectricalEnergyMeasurement -> device -> endpointID -> queue -> IO (Id MTRBaseClusterElectricalEnergyMeasurement)
initWithDevice_endpointID_queue mtrBaseClusterElectricalEnergyMeasurement device endpointID queue =
  sendOwnedMessage mtrBaseClusterElectricalEnergyMeasurement initWithDevice_endpointID_queueSelector (toMTRBaseDevice device) (toNSNumber endpointID) (toNSObject queue)

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @readAttributeAccuracyWithCompletion:@
readAttributeAccuracyWithCompletionSelector :: Selector '[Ptr ()] ()
readAttributeAccuracyWithCompletionSelector = mkSelector "readAttributeAccuracyWithCompletion:"

-- | @Selector@ for @subscribeAttributeAccuracyWithParams:subscriptionEstablished:reportHandler:@
subscribeAttributeAccuracyWithParams_subscriptionEstablished_reportHandlerSelector :: Selector '[Id MTRSubscribeParams, Ptr (), Ptr ()] ()
subscribeAttributeAccuracyWithParams_subscriptionEstablished_reportHandlerSelector = mkSelector "subscribeAttributeAccuracyWithParams:subscriptionEstablished:reportHandler:"

-- | @Selector@ for @readAttributeAccuracyWithClusterStateCache:endpoint:queue:completion:@
readAttributeAccuracyWithClusterStateCache_endpoint_queue_completionSelector :: Selector '[Id MTRClusterStateCacheContainer, Id NSNumber, Id NSObject, Ptr ()] ()
readAttributeAccuracyWithClusterStateCache_endpoint_queue_completionSelector = mkSelector "readAttributeAccuracyWithClusterStateCache:endpoint:queue:completion:"

-- | @Selector@ for @readAttributeCumulativeEnergyImportedWithCompletion:@
readAttributeCumulativeEnergyImportedWithCompletionSelector :: Selector '[Ptr ()] ()
readAttributeCumulativeEnergyImportedWithCompletionSelector = mkSelector "readAttributeCumulativeEnergyImportedWithCompletion:"

-- | @Selector@ for @subscribeAttributeCumulativeEnergyImportedWithParams:subscriptionEstablished:reportHandler:@
subscribeAttributeCumulativeEnergyImportedWithParams_subscriptionEstablished_reportHandlerSelector :: Selector '[Id MTRSubscribeParams, Ptr (), Ptr ()] ()
subscribeAttributeCumulativeEnergyImportedWithParams_subscriptionEstablished_reportHandlerSelector = mkSelector "subscribeAttributeCumulativeEnergyImportedWithParams:subscriptionEstablished:reportHandler:"

-- | @Selector@ for @readAttributeCumulativeEnergyImportedWithClusterStateCache:endpoint:queue:completion:@
readAttributeCumulativeEnergyImportedWithClusterStateCache_endpoint_queue_completionSelector :: Selector '[Id MTRClusterStateCacheContainer, Id NSNumber, Id NSObject, Ptr ()] ()
readAttributeCumulativeEnergyImportedWithClusterStateCache_endpoint_queue_completionSelector = mkSelector "readAttributeCumulativeEnergyImportedWithClusterStateCache:endpoint:queue:completion:"

-- | @Selector@ for @readAttributeCumulativeEnergyExportedWithCompletion:@
readAttributeCumulativeEnergyExportedWithCompletionSelector :: Selector '[Ptr ()] ()
readAttributeCumulativeEnergyExportedWithCompletionSelector = mkSelector "readAttributeCumulativeEnergyExportedWithCompletion:"

-- | @Selector@ for @subscribeAttributeCumulativeEnergyExportedWithParams:subscriptionEstablished:reportHandler:@
subscribeAttributeCumulativeEnergyExportedWithParams_subscriptionEstablished_reportHandlerSelector :: Selector '[Id MTRSubscribeParams, Ptr (), Ptr ()] ()
subscribeAttributeCumulativeEnergyExportedWithParams_subscriptionEstablished_reportHandlerSelector = mkSelector "subscribeAttributeCumulativeEnergyExportedWithParams:subscriptionEstablished:reportHandler:"

-- | @Selector@ for @readAttributeCumulativeEnergyExportedWithClusterStateCache:endpoint:queue:completion:@
readAttributeCumulativeEnergyExportedWithClusterStateCache_endpoint_queue_completionSelector :: Selector '[Id MTRClusterStateCacheContainer, Id NSNumber, Id NSObject, Ptr ()] ()
readAttributeCumulativeEnergyExportedWithClusterStateCache_endpoint_queue_completionSelector = mkSelector "readAttributeCumulativeEnergyExportedWithClusterStateCache:endpoint:queue:completion:"

-- | @Selector@ for @readAttributePeriodicEnergyImportedWithCompletion:@
readAttributePeriodicEnergyImportedWithCompletionSelector :: Selector '[Ptr ()] ()
readAttributePeriodicEnergyImportedWithCompletionSelector = mkSelector "readAttributePeriodicEnergyImportedWithCompletion:"

-- | @Selector@ for @subscribeAttributePeriodicEnergyImportedWithParams:subscriptionEstablished:reportHandler:@
subscribeAttributePeriodicEnergyImportedWithParams_subscriptionEstablished_reportHandlerSelector :: Selector '[Id MTRSubscribeParams, Ptr (), Ptr ()] ()
subscribeAttributePeriodicEnergyImportedWithParams_subscriptionEstablished_reportHandlerSelector = mkSelector "subscribeAttributePeriodicEnergyImportedWithParams:subscriptionEstablished:reportHandler:"

-- | @Selector@ for @readAttributePeriodicEnergyImportedWithClusterStateCache:endpoint:queue:completion:@
readAttributePeriodicEnergyImportedWithClusterStateCache_endpoint_queue_completionSelector :: Selector '[Id MTRClusterStateCacheContainer, Id NSNumber, Id NSObject, Ptr ()] ()
readAttributePeriodicEnergyImportedWithClusterStateCache_endpoint_queue_completionSelector = mkSelector "readAttributePeriodicEnergyImportedWithClusterStateCache:endpoint:queue:completion:"

-- | @Selector@ for @readAttributePeriodicEnergyExportedWithCompletion:@
readAttributePeriodicEnergyExportedWithCompletionSelector :: Selector '[Ptr ()] ()
readAttributePeriodicEnergyExportedWithCompletionSelector = mkSelector "readAttributePeriodicEnergyExportedWithCompletion:"

-- | @Selector@ for @subscribeAttributePeriodicEnergyExportedWithParams:subscriptionEstablished:reportHandler:@
subscribeAttributePeriodicEnergyExportedWithParams_subscriptionEstablished_reportHandlerSelector :: Selector '[Id MTRSubscribeParams, Ptr (), Ptr ()] ()
subscribeAttributePeriodicEnergyExportedWithParams_subscriptionEstablished_reportHandlerSelector = mkSelector "subscribeAttributePeriodicEnergyExportedWithParams:subscriptionEstablished:reportHandler:"

-- | @Selector@ for @readAttributePeriodicEnergyExportedWithClusterStateCache:endpoint:queue:completion:@
readAttributePeriodicEnergyExportedWithClusterStateCache_endpoint_queue_completionSelector :: Selector '[Id MTRClusterStateCacheContainer, Id NSNumber, Id NSObject, Ptr ()] ()
readAttributePeriodicEnergyExportedWithClusterStateCache_endpoint_queue_completionSelector = mkSelector "readAttributePeriodicEnergyExportedWithClusterStateCache:endpoint:queue:completion:"

-- | @Selector@ for @readAttributeCumulativeEnergyResetWithCompletion:@
readAttributeCumulativeEnergyResetWithCompletionSelector :: Selector '[Ptr ()] ()
readAttributeCumulativeEnergyResetWithCompletionSelector = mkSelector "readAttributeCumulativeEnergyResetWithCompletion:"

-- | @Selector@ for @subscribeAttributeCumulativeEnergyResetWithParams:subscriptionEstablished:reportHandler:@
subscribeAttributeCumulativeEnergyResetWithParams_subscriptionEstablished_reportHandlerSelector :: Selector '[Id MTRSubscribeParams, Ptr (), Ptr ()] ()
subscribeAttributeCumulativeEnergyResetWithParams_subscriptionEstablished_reportHandlerSelector = mkSelector "subscribeAttributeCumulativeEnergyResetWithParams:subscriptionEstablished:reportHandler:"

-- | @Selector@ for @readAttributeCumulativeEnergyResetWithClusterStateCache:endpoint:queue:completion:@
readAttributeCumulativeEnergyResetWithClusterStateCache_endpoint_queue_completionSelector :: Selector '[Id MTRClusterStateCacheContainer, Id NSNumber, Id NSObject, Ptr ()] ()
readAttributeCumulativeEnergyResetWithClusterStateCache_endpoint_queue_completionSelector = mkSelector "readAttributeCumulativeEnergyResetWithClusterStateCache:endpoint:queue:completion:"

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
initSelector :: Selector '[] (Id MTRBaseClusterElectricalEnergyMeasurement)
initSelector = mkSelector "init"

-- | @Selector@ for @new@
newSelector :: Selector '[] (Id MTRBaseClusterElectricalEnergyMeasurement)
newSelector = mkSelector "new"

-- | @Selector@ for @initWithDevice:endpointID:queue:@
initWithDevice_endpointID_queueSelector :: Selector '[Id MTRBaseDevice, Id NSNumber, Id NSObject] (Id MTRBaseClusterElectricalEnergyMeasurement)
initWithDevice_endpointID_queueSelector = mkSelector "initWithDevice:endpointID:queue:"

