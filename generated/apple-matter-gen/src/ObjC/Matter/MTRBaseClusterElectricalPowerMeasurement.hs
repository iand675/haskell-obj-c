{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Cluster Electrical Power Measurement
--
-- This cluster provides a mechanism for querying data about electrical power as measured by the server.
--
-- Generated bindings for @MTRBaseClusterElectricalPowerMeasurement@.
module ObjC.Matter.MTRBaseClusterElectricalPowerMeasurement
  ( MTRBaseClusterElectricalPowerMeasurement
  , IsMTRBaseClusterElectricalPowerMeasurement(..)
  , readAttributePowerModeWithCompletion
  , subscribeAttributePowerModeWithParams_subscriptionEstablished_reportHandler
  , readAttributePowerModeWithClusterStateCache_endpoint_queue_completion
  , readAttributeNumberOfMeasurementTypesWithCompletion
  , subscribeAttributeNumberOfMeasurementTypesWithParams_subscriptionEstablished_reportHandler
  , readAttributeNumberOfMeasurementTypesWithClusterStateCache_endpoint_queue_completion
  , readAttributeAccuracyWithCompletion
  , subscribeAttributeAccuracyWithParams_subscriptionEstablished_reportHandler
  , readAttributeAccuracyWithClusterStateCache_endpoint_queue_completion
  , readAttributeRangesWithCompletion
  , subscribeAttributeRangesWithParams_subscriptionEstablished_reportHandler
  , readAttributeRangesWithClusterStateCache_endpoint_queue_completion
  , readAttributeVoltageWithCompletion
  , subscribeAttributeVoltageWithParams_subscriptionEstablished_reportHandler
  , readAttributeVoltageWithClusterStateCache_endpoint_queue_completion
  , readAttributeActiveCurrentWithCompletion
  , subscribeAttributeActiveCurrentWithParams_subscriptionEstablished_reportHandler
  , readAttributeActiveCurrentWithClusterStateCache_endpoint_queue_completion
  , readAttributeReactiveCurrentWithCompletion
  , subscribeAttributeReactiveCurrentWithParams_subscriptionEstablished_reportHandler
  , readAttributeReactiveCurrentWithClusterStateCache_endpoint_queue_completion
  , readAttributeApparentCurrentWithCompletion
  , subscribeAttributeApparentCurrentWithParams_subscriptionEstablished_reportHandler
  , readAttributeApparentCurrentWithClusterStateCache_endpoint_queue_completion
  , readAttributeActivePowerWithCompletion
  , subscribeAttributeActivePowerWithParams_subscriptionEstablished_reportHandler
  , readAttributeActivePowerWithClusterStateCache_endpoint_queue_completion
  , readAttributeReactivePowerWithCompletion
  , subscribeAttributeReactivePowerWithParams_subscriptionEstablished_reportHandler
  , readAttributeReactivePowerWithClusterStateCache_endpoint_queue_completion
  , readAttributeApparentPowerWithCompletion
  , subscribeAttributeApparentPowerWithParams_subscriptionEstablished_reportHandler
  , readAttributeApparentPowerWithClusterStateCache_endpoint_queue_completion
  , readAttributeRMSVoltageWithCompletion
  , subscribeAttributeRMSVoltageWithParams_subscriptionEstablished_reportHandler
  , readAttributeRMSVoltageWithClusterStateCache_endpoint_queue_completion
  , readAttributeRMSCurrentWithCompletion
  , subscribeAttributeRMSCurrentWithParams_subscriptionEstablished_reportHandler
  , readAttributeRMSCurrentWithClusterStateCache_endpoint_queue_completion
  , readAttributeRMSPowerWithCompletion
  , subscribeAttributeRMSPowerWithParams_subscriptionEstablished_reportHandler
  , readAttributeRMSPowerWithClusterStateCache_endpoint_queue_completion
  , readAttributeFrequencyWithCompletion
  , subscribeAttributeFrequencyWithParams_subscriptionEstablished_reportHandler
  , readAttributeFrequencyWithClusterStateCache_endpoint_queue_completion
  , readAttributeHarmonicCurrentsWithCompletion
  , subscribeAttributeHarmonicCurrentsWithParams_subscriptionEstablished_reportHandler
  , readAttributeHarmonicCurrentsWithClusterStateCache_endpoint_queue_completion
  , readAttributeHarmonicPhasesWithCompletion
  , subscribeAttributeHarmonicPhasesWithParams_subscriptionEstablished_reportHandler
  , readAttributeHarmonicPhasesWithClusterStateCache_endpoint_queue_completion
  , readAttributePowerFactorWithCompletion
  , subscribeAttributePowerFactorWithParams_subscriptionEstablished_reportHandler
  , readAttributePowerFactorWithClusterStateCache_endpoint_queue_completion
  , readAttributeNeutralCurrentWithCompletion
  , subscribeAttributeNeutralCurrentWithParams_subscriptionEstablished_reportHandler
  , readAttributeNeutralCurrentWithClusterStateCache_endpoint_queue_completion
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
  , readAttributeActiveCurrentWithClusterStateCache_endpoint_queue_completionSelector
  , readAttributeActiveCurrentWithCompletionSelector
  , readAttributeActivePowerWithClusterStateCache_endpoint_queue_completionSelector
  , readAttributeActivePowerWithCompletionSelector
  , readAttributeApparentCurrentWithClusterStateCache_endpoint_queue_completionSelector
  , readAttributeApparentCurrentWithCompletionSelector
  , readAttributeApparentPowerWithClusterStateCache_endpoint_queue_completionSelector
  , readAttributeApparentPowerWithCompletionSelector
  , readAttributeAttributeListWithClusterStateCache_endpoint_queue_completionSelector
  , readAttributeAttributeListWithCompletionSelector
  , readAttributeClusterRevisionWithClusterStateCache_endpoint_queue_completionSelector
  , readAttributeClusterRevisionWithCompletionSelector
  , readAttributeFeatureMapWithClusterStateCache_endpoint_queue_completionSelector
  , readAttributeFeatureMapWithCompletionSelector
  , readAttributeFrequencyWithClusterStateCache_endpoint_queue_completionSelector
  , readAttributeFrequencyWithCompletionSelector
  , readAttributeGeneratedCommandListWithClusterStateCache_endpoint_queue_completionSelector
  , readAttributeGeneratedCommandListWithCompletionSelector
  , readAttributeHarmonicCurrentsWithClusterStateCache_endpoint_queue_completionSelector
  , readAttributeHarmonicCurrentsWithCompletionSelector
  , readAttributeHarmonicPhasesWithClusterStateCache_endpoint_queue_completionSelector
  , readAttributeHarmonicPhasesWithCompletionSelector
  , readAttributeNeutralCurrentWithClusterStateCache_endpoint_queue_completionSelector
  , readAttributeNeutralCurrentWithCompletionSelector
  , readAttributeNumberOfMeasurementTypesWithClusterStateCache_endpoint_queue_completionSelector
  , readAttributeNumberOfMeasurementTypesWithCompletionSelector
  , readAttributePowerFactorWithClusterStateCache_endpoint_queue_completionSelector
  , readAttributePowerFactorWithCompletionSelector
  , readAttributePowerModeWithClusterStateCache_endpoint_queue_completionSelector
  , readAttributePowerModeWithCompletionSelector
  , readAttributeRMSCurrentWithClusterStateCache_endpoint_queue_completionSelector
  , readAttributeRMSCurrentWithCompletionSelector
  , readAttributeRMSPowerWithClusterStateCache_endpoint_queue_completionSelector
  , readAttributeRMSPowerWithCompletionSelector
  , readAttributeRMSVoltageWithClusterStateCache_endpoint_queue_completionSelector
  , readAttributeRMSVoltageWithCompletionSelector
  , readAttributeRangesWithClusterStateCache_endpoint_queue_completionSelector
  , readAttributeRangesWithCompletionSelector
  , readAttributeReactiveCurrentWithClusterStateCache_endpoint_queue_completionSelector
  , readAttributeReactiveCurrentWithCompletionSelector
  , readAttributeReactivePowerWithClusterStateCache_endpoint_queue_completionSelector
  , readAttributeReactivePowerWithCompletionSelector
  , readAttributeVoltageWithClusterStateCache_endpoint_queue_completionSelector
  , readAttributeVoltageWithCompletionSelector
  , subscribeAttributeAcceptedCommandListWithParams_subscriptionEstablished_reportHandlerSelector
  , subscribeAttributeAccuracyWithParams_subscriptionEstablished_reportHandlerSelector
  , subscribeAttributeActiveCurrentWithParams_subscriptionEstablished_reportHandlerSelector
  , subscribeAttributeActivePowerWithParams_subscriptionEstablished_reportHandlerSelector
  , subscribeAttributeApparentCurrentWithParams_subscriptionEstablished_reportHandlerSelector
  , subscribeAttributeApparentPowerWithParams_subscriptionEstablished_reportHandlerSelector
  , subscribeAttributeAttributeListWithParams_subscriptionEstablished_reportHandlerSelector
  , subscribeAttributeClusterRevisionWithParams_subscriptionEstablished_reportHandlerSelector
  , subscribeAttributeFeatureMapWithParams_subscriptionEstablished_reportHandlerSelector
  , subscribeAttributeFrequencyWithParams_subscriptionEstablished_reportHandlerSelector
  , subscribeAttributeGeneratedCommandListWithParams_subscriptionEstablished_reportHandlerSelector
  , subscribeAttributeHarmonicCurrentsWithParams_subscriptionEstablished_reportHandlerSelector
  , subscribeAttributeHarmonicPhasesWithParams_subscriptionEstablished_reportHandlerSelector
  , subscribeAttributeNeutralCurrentWithParams_subscriptionEstablished_reportHandlerSelector
  , subscribeAttributeNumberOfMeasurementTypesWithParams_subscriptionEstablished_reportHandlerSelector
  , subscribeAttributePowerFactorWithParams_subscriptionEstablished_reportHandlerSelector
  , subscribeAttributePowerModeWithParams_subscriptionEstablished_reportHandlerSelector
  , subscribeAttributeRMSCurrentWithParams_subscriptionEstablished_reportHandlerSelector
  , subscribeAttributeRMSPowerWithParams_subscriptionEstablished_reportHandlerSelector
  , subscribeAttributeRMSVoltageWithParams_subscriptionEstablished_reportHandlerSelector
  , subscribeAttributeRangesWithParams_subscriptionEstablished_reportHandlerSelector
  , subscribeAttributeReactiveCurrentWithParams_subscriptionEstablished_reportHandlerSelector
  , subscribeAttributeReactivePowerWithParams_subscriptionEstablished_reportHandlerSelector
  , subscribeAttributeVoltageWithParams_subscriptionEstablished_reportHandlerSelector


  ) where

import Foreign.Ptr (Ptr, FunPtr)
import Foreign.C.Types

import ObjC.Runtime.Types
import ObjC.Runtime.Message (sendMessage, sendOwnedMessage, sendClassMessage, sendOwnedClassMessage)
import ObjC.Runtime.Selector (mkSelector)
import ObjC.Runtime.Class (getRequiredClass)

import ObjC.Matter.Internal.Classes
import ObjC.Foundation.Internal.Classes

-- | @- readAttributePowerModeWithCompletion:@
readAttributePowerModeWithCompletion :: IsMTRBaseClusterElectricalPowerMeasurement mtrBaseClusterElectricalPowerMeasurement => mtrBaseClusterElectricalPowerMeasurement -> Ptr () -> IO ()
readAttributePowerModeWithCompletion mtrBaseClusterElectricalPowerMeasurement completion =
  sendMessage mtrBaseClusterElectricalPowerMeasurement readAttributePowerModeWithCompletionSelector completion

-- | @- subscribeAttributePowerModeWithParams:subscriptionEstablished:reportHandler:@
subscribeAttributePowerModeWithParams_subscriptionEstablished_reportHandler :: (IsMTRBaseClusterElectricalPowerMeasurement mtrBaseClusterElectricalPowerMeasurement, IsMTRSubscribeParams params) => mtrBaseClusterElectricalPowerMeasurement -> params -> Ptr () -> Ptr () -> IO ()
subscribeAttributePowerModeWithParams_subscriptionEstablished_reportHandler mtrBaseClusterElectricalPowerMeasurement params subscriptionEstablished reportHandler =
  sendMessage mtrBaseClusterElectricalPowerMeasurement subscribeAttributePowerModeWithParams_subscriptionEstablished_reportHandlerSelector (toMTRSubscribeParams params) subscriptionEstablished reportHandler

-- | @+ readAttributePowerModeWithClusterStateCache:endpoint:queue:completion:@
readAttributePowerModeWithClusterStateCache_endpoint_queue_completion :: (IsMTRClusterStateCacheContainer clusterStateCacheContainer, IsNSNumber endpoint, IsNSObject queue) => clusterStateCacheContainer -> endpoint -> queue -> Ptr () -> IO ()
readAttributePowerModeWithClusterStateCache_endpoint_queue_completion clusterStateCacheContainer endpoint queue completion =
  do
    cls' <- getRequiredClass "MTRBaseClusterElectricalPowerMeasurement"
    sendClassMessage cls' readAttributePowerModeWithClusterStateCache_endpoint_queue_completionSelector (toMTRClusterStateCacheContainer clusterStateCacheContainer) (toNSNumber endpoint) (toNSObject queue) completion

-- | @- readAttributeNumberOfMeasurementTypesWithCompletion:@
readAttributeNumberOfMeasurementTypesWithCompletion :: IsMTRBaseClusterElectricalPowerMeasurement mtrBaseClusterElectricalPowerMeasurement => mtrBaseClusterElectricalPowerMeasurement -> Ptr () -> IO ()
readAttributeNumberOfMeasurementTypesWithCompletion mtrBaseClusterElectricalPowerMeasurement completion =
  sendMessage mtrBaseClusterElectricalPowerMeasurement readAttributeNumberOfMeasurementTypesWithCompletionSelector completion

-- | @- subscribeAttributeNumberOfMeasurementTypesWithParams:subscriptionEstablished:reportHandler:@
subscribeAttributeNumberOfMeasurementTypesWithParams_subscriptionEstablished_reportHandler :: (IsMTRBaseClusterElectricalPowerMeasurement mtrBaseClusterElectricalPowerMeasurement, IsMTRSubscribeParams params) => mtrBaseClusterElectricalPowerMeasurement -> params -> Ptr () -> Ptr () -> IO ()
subscribeAttributeNumberOfMeasurementTypesWithParams_subscriptionEstablished_reportHandler mtrBaseClusterElectricalPowerMeasurement params subscriptionEstablished reportHandler =
  sendMessage mtrBaseClusterElectricalPowerMeasurement subscribeAttributeNumberOfMeasurementTypesWithParams_subscriptionEstablished_reportHandlerSelector (toMTRSubscribeParams params) subscriptionEstablished reportHandler

-- | @+ readAttributeNumberOfMeasurementTypesWithClusterStateCache:endpoint:queue:completion:@
readAttributeNumberOfMeasurementTypesWithClusterStateCache_endpoint_queue_completion :: (IsMTRClusterStateCacheContainer clusterStateCacheContainer, IsNSNumber endpoint, IsNSObject queue) => clusterStateCacheContainer -> endpoint -> queue -> Ptr () -> IO ()
readAttributeNumberOfMeasurementTypesWithClusterStateCache_endpoint_queue_completion clusterStateCacheContainer endpoint queue completion =
  do
    cls' <- getRequiredClass "MTRBaseClusterElectricalPowerMeasurement"
    sendClassMessage cls' readAttributeNumberOfMeasurementTypesWithClusterStateCache_endpoint_queue_completionSelector (toMTRClusterStateCacheContainer clusterStateCacheContainer) (toNSNumber endpoint) (toNSObject queue) completion

-- | @- readAttributeAccuracyWithCompletion:@
readAttributeAccuracyWithCompletion :: IsMTRBaseClusterElectricalPowerMeasurement mtrBaseClusterElectricalPowerMeasurement => mtrBaseClusterElectricalPowerMeasurement -> Ptr () -> IO ()
readAttributeAccuracyWithCompletion mtrBaseClusterElectricalPowerMeasurement completion =
  sendMessage mtrBaseClusterElectricalPowerMeasurement readAttributeAccuracyWithCompletionSelector completion

-- | @- subscribeAttributeAccuracyWithParams:subscriptionEstablished:reportHandler:@
subscribeAttributeAccuracyWithParams_subscriptionEstablished_reportHandler :: (IsMTRBaseClusterElectricalPowerMeasurement mtrBaseClusterElectricalPowerMeasurement, IsMTRSubscribeParams params) => mtrBaseClusterElectricalPowerMeasurement -> params -> Ptr () -> Ptr () -> IO ()
subscribeAttributeAccuracyWithParams_subscriptionEstablished_reportHandler mtrBaseClusterElectricalPowerMeasurement params subscriptionEstablished reportHandler =
  sendMessage mtrBaseClusterElectricalPowerMeasurement subscribeAttributeAccuracyWithParams_subscriptionEstablished_reportHandlerSelector (toMTRSubscribeParams params) subscriptionEstablished reportHandler

-- | @+ readAttributeAccuracyWithClusterStateCache:endpoint:queue:completion:@
readAttributeAccuracyWithClusterStateCache_endpoint_queue_completion :: (IsMTRClusterStateCacheContainer clusterStateCacheContainer, IsNSNumber endpoint, IsNSObject queue) => clusterStateCacheContainer -> endpoint -> queue -> Ptr () -> IO ()
readAttributeAccuracyWithClusterStateCache_endpoint_queue_completion clusterStateCacheContainer endpoint queue completion =
  do
    cls' <- getRequiredClass "MTRBaseClusterElectricalPowerMeasurement"
    sendClassMessage cls' readAttributeAccuracyWithClusterStateCache_endpoint_queue_completionSelector (toMTRClusterStateCacheContainer clusterStateCacheContainer) (toNSNumber endpoint) (toNSObject queue) completion

-- | @- readAttributeRangesWithCompletion:@
readAttributeRangesWithCompletion :: IsMTRBaseClusterElectricalPowerMeasurement mtrBaseClusterElectricalPowerMeasurement => mtrBaseClusterElectricalPowerMeasurement -> Ptr () -> IO ()
readAttributeRangesWithCompletion mtrBaseClusterElectricalPowerMeasurement completion =
  sendMessage mtrBaseClusterElectricalPowerMeasurement readAttributeRangesWithCompletionSelector completion

-- | @- subscribeAttributeRangesWithParams:subscriptionEstablished:reportHandler:@
subscribeAttributeRangesWithParams_subscriptionEstablished_reportHandler :: (IsMTRBaseClusterElectricalPowerMeasurement mtrBaseClusterElectricalPowerMeasurement, IsMTRSubscribeParams params) => mtrBaseClusterElectricalPowerMeasurement -> params -> Ptr () -> Ptr () -> IO ()
subscribeAttributeRangesWithParams_subscriptionEstablished_reportHandler mtrBaseClusterElectricalPowerMeasurement params subscriptionEstablished reportHandler =
  sendMessage mtrBaseClusterElectricalPowerMeasurement subscribeAttributeRangesWithParams_subscriptionEstablished_reportHandlerSelector (toMTRSubscribeParams params) subscriptionEstablished reportHandler

-- | @+ readAttributeRangesWithClusterStateCache:endpoint:queue:completion:@
readAttributeRangesWithClusterStateCache_endpoint_queue_completion :: (IsMTRClusterStateCacheContainer clusterStateCacheContainer, IsNSNumber endpoint, IsNSObject queue) => clusterStateCacheContainer -> endpoint -> queue -> Ptr () -> IO ()
readAttributeRangesWithClusterStateCache_endpoint_queue_completion clusterStateCacheContainer endpoint queue completion =
  do
    cls' <- getRequiredClass "MTRBaseClusterElectricalPowerMeasurement"
    sendClassMessage cls' readAttributeRangesWithClusterStateCache_endpoint_queue_completionSelector (toMTRClusterStateCacheContainer clusterStateCacheContainer) (toNSNumber endpoint) (toNSObject queue) completion

-- | @- readAttributeVoltageWithCompletion:@
readAttributeVoltageWithCompletion :: IsMTRBaseClusterElectricalPowerMeasurement mtrBaseClusterElectricalPowerMeasurement => mtrBaseClusterElectricalPowerMeasurement -> Ptr () -> IO ()
readAttributeVoltageWithCompletion mtrBaseClusterElectricalPowerMeasurement completion =
  sendMessage mtrBaseClusterElectricalPowerMeasurement readAttributeVoltageWithCompletionSelector completion

-- | @- subscribeAttributeVoltageWithParams:subscriptionEstablished:reportHandler:@
subscribeAttributeVoltageWithParams_subscriptionEstablished_reportHandler :: (IsMTRBaseClusterElectricalPowerMeasurement mtrBaseClusterElectricalPowerMeasurement, IsMTRSubscribeParams params) => mtrBaseClusterElectricalPowerMeasurement -> params -> Ptr () -> Ptr () -> IO ()
subscribeAttributeVoltageWithParams_subscriptionEstablished_reportHandler mtrBaseClusterElectricalPowerMeasurement params subscriptionEstablished reportHandler =
  sendMessage mtrBaseClusterElectricalPowerMeasurement subscribeAttributeVoltageWithParams_subscriptionEstablished_reportHandlerSelector (toMTRSubscribeParams params) subscriptionEstablished reportHandler

-- | @+ readAttributeVoltageWithClusterStateCache:endpoint:queue:completion:@
readAttributeVoltageWithClusterStateCache_endpoint_queue_completion :: (IsMTRClusterStateCacheContainer clusterStateCacheContainer, IsNSNumber endpoint, IsNSObject queue) => clusterStateCacheContainer -> endpoint -> queue -> Ptr () -> IO ()
readAttributeVoltageWithClusterStateCache_endpoint_queue_completion clusterStateCacheContainer endpoint queue completion =
  do
    cls' <- getRequiredClass "MTRBaseClusterElectricalPowerMeasurement"
    sendClassMessage cls' readAttributeVoltageWithClusterStateCache_endpoint_queue_completionSelector (toMTRClusterStateCacheContainer clusterStateCacheContainer) (toNSNumber endpoint) (toNSObject queue) completion

-- | @- readAttributeActiveCurrentWithCompletion:@
readAttributeActiveCurrentWithCompletion :: IsMTRBaseClusterElectricalPowerMeasurement mtrBaseClusterElectricalPowerMeasurement => mtrBaseClusterElectricalPowerMeasurement -> Ptr () -> IO ()
readAttributeActiveCurrentWithCompletion mtrBaseClusterElectricalPowerMeasurement completion =
  sendMessage mtrBaseClusterElectricalPowerMeasurement readAttributeActiveCurrentWithCompletionSelector completion

-- | @- subscribeAttributeActiveCurrentWithParams:subscriptionEstablished:reportHandler:@
subscribeAttributeActiveCurrentWithParams_subscriptionEstablished_reportHandler :: (IsMTRBaseClusterElectricalPowerMeasurement mtrBaseClusterElectricalPowerMeasurement, IsMTRSubscribeParams params) => mtrBaseClusterElectricalPowerMeasurement -> params -> Ptr () -> Ptr () -> IO ()
subscribeAttributeActiveCurrentWithParams_subscriptionEstablished_reportHandler mtrBaseClusterElectricalPowerMeasurement params subscriptionEstablished reportHandler =
  sendMessage mtrBaseClusterElectricalPowerMeasurement subscribeAttributeActiveCurrentWithParams_subscriptionEstablished_reportHandlerSelector (toMTRSubscribeParams params) subscriptionEstablished reportHandler

-- | @+ readAttributeActiveCurrentWithClusterStateCache:endpoint:queue:completion:@
readAttributeActiveCurrentWithClusterStateCache_endpoint_queue_completion :: (IsMTRClusterStateCacheContainer clusterStateCacheContainer, IsNSNumber endpoint, IsNSObject queue) => clusterStateCacheContainer -> endpoint -> queue -> Ptr () -> IO ()
readAttributeActiveCurrentWithClusterStateCache_endpoint_queue_completion clusterStateCacheContainer endpoint queue completion =
  do
    cls' <- getRequiredClass "MTRBaseClusterElectricalPowerMeasurement"
    sendClassMessage cls' readAttributeActiveCurrentWithClusterStateCache_endpoint_queue_completionSelector (toMTRClusterStateCacheContainer clusterStateCacheContainer) (toNSNumber endpoint) (toNSObject queue) completion

-- | @- readAttributeReactiveCurrentWithCompletion:@
readAttributeReactiveCurrentWithCompletion :: IsMTRBaseClusterElectricalPowerMeasurement mtrBaseClusterElectricalPowerMeasurement => mtrBaseClusterElectricalPowerMeasurement -> Ptr () -> IO ()
readAttributeReactiveCurrentWithCompletion mtrBaseClusterElectricalPowerMeasurement completion =
  sendMessage mtrBaseClusterElectricalPowerMeasurement readAttributeReactiveCurrentWithCompletionSelector completion

-- | @- subscribeAttributeReactiveCurrentWithParams:subscriptionEstablished:reportHandler:@
subscribeAttributeReactiveCurrentWithParams_subscriptionEstablished_reportHandler :: (IsMTRBaseClusterElectricalPowerMeasurement mtrBaseClusterElectricalPowerMeasurement, IsMTRSubscribeParams params) => mtrBaseClusterElectricalPowerMeasurement -> params -> Ptr () -> Ptr () -> IO ()
subscribeAttributeReactiveCurrentWithParams_subscriptionEstablished_reportHandler mtrBaseClusterElectricalPowerMeasurement params subscriptionEstablished reportHandler =
  sendMessage mtrBaseClusterElectricalPowerMeasurement subscribeAttributeReactiveCurrentWithParams_subscriptionEstablished_reportHandlerSelector (toMTRSubscribeParams params) subscriptionEstablished reportHandler

-- | @+ readAttributeReactiveCurrentWithClusterStateCache:endpoint:queue:completion:@
readAttributeReactiveCurrentWithClusterStateCache_endpoint_queue_completion :: (IsMTRClusterStateCacheContainer clusterStateCacheContainer, IsNSNumber endpoint, IsNSObject queue) => clusterStateCacheContainer -> endpoint -> queue -> Ptr () -> IO ()
readAttributeReactiveCurrentWithClusterStateCache_endpoint_queue_completion clusterStateCacheContainer endpoint queue completion =
  do
    cls' <- getRequiredClass "MTRBaseClusterElectricalPowerMeasurement"
    sendClassMessage cls' readAttributeReactiveCurrentWithClusterStateCache_endpoint_queue_completionSelector (toMTRClusterStateCacheContainer clusterStateCacheContainer) (toNSNumber endpoint) (toNSObject queue) completion

-- | @- readAttributeApparentCurrentWithCompletion:@
readAttributeApparentCurrentWithCompletion :: IsMTRBaseClusterElectricalPowerMeasurement mtrBaseClusterElectricalPowerMeasurement => mtrBaseClusterElectricalPowerMeasurement -> Ptr () -> IO ()
readAttributeApparentCurrentWithCompletion mtrBaseClusterElectricalPowerMeasurement completion =
  sendMessage mtrBaseClusterElectricalPowerMeasurement readAttributeApparentCurrentWithCompletionSelector completion

-- | @- subscribeAttributeApparentCurrentWithParams:subscriptionEstablished:reportHandler:@
subscribeAttributeApparentCurrentWithParams_subscriptionEstablished_reportHandler :: (IsMTRBaseClusterElectricalPowerMeasurement mtrBaseClusterElectricalPowerMeasurement, IsMTRSubscribeParams params) => mtrBaseClusterElectricalPowerMeasurement -> params -> Ptr () -> Ptr () -> IO ()
subscribeAttributeApparentCurrentWithParams_subscriptionEstablished_reportHandler mtrBaseClusterElectricalPowerMeasurement params subscriptionEstablished reportHandler =
  sendMessage mtrBaseClusterElectricalPowerMeasurement subscribeAttributeApparentCurrentWithParams_subscriptionEstablished_reportHandlerSelector (toMTRSubscribeParams params) subscriptionEstablished reportHandler

-- | @+ readAttributeApparentCurrentWithClusterStateCache:endpoint:queue:completion:@
readAttributeApparentCurrentWithClusterStateCache_endpoint_queue_completion :: (IsMTRClusterStateCacheContainer clusterStateCacheContainer, IsNSNumber endpoint, IsNSObject queue) => clusterStateCacheContainer -> endpoint -> queue -> Ptr () -> IO ()
readAttributeApparentCurrentWithClusterStateCache_endpoint_queue_completion clusterStateCacheContainer endpoint queue completion =
  do
    cls' <- getRequiredClass "MTRBaseClusterElectricalPowerMeasurement"
    sendClassMessage cls' readAttributeApparentCurrentWithClusterStateCache_endpoint_queue_completionSelector (toMTRClusterStateCacheContainer clusterStateCacheContainer) (toNSNumber endpoint) (toNSObject queue) completion

-- | @- readAttributeActivePowerWithCompletion:@
readAttributeActivePowerWithCompletion :: IsMTRBaseClusterElectricalPowerMeasurement mtrBaseClusterElectricalPowerMeasurement => mtrBaseClusterElectricalPowerMeasurement -> Ptr () -> IO ()
readAttributeActivePowerWithCompletion mtrBaseClusterElectricalPowerMeasurement completion =
  sendMessage mtrBaseClusterElectricalPowerMeasurement readAttributeActivePowerWithCompletionSelector completion

-- | @- subscribeAttributeActivePowerWithParams:subscriptionEstablished:reportHandler:@
subscribeAttributeActivePowerWithParams_subscriptionEstablished_reportHandler :: (IsMTRBaseClusterElectricalPowerMeasurement mtrBaseClusterElectricalPowerMeasurement, IsMTRSubscribeParams params) => mtrBaseClusterElectricalPowerMeasurement -> params -> Ptr () -> Ptr () -> IO ()
subscribeAttributeActivePowerWithParams_subscriptionEstablished_reportHandler mtrBaseClusterElectricalPowerMeasurement params subscriptionEstablished reportHandler =
  sendMessage mtrBaseClusterElectricalPowerMeasurement subscribeAttributeActivePowerWithParams_subscriptionEstablished_reportHandlerSelector (toMTRSubscribeParams params) subscriptionEstablished reportHandler

-- | @+ readAttributeActivePowerWithClusterStateCache:endpoint:queue:completion:@
readAttributeActivePowerWithClusterStateCache_endpoint_queue_completion :: (IsMTRClusterStateCacheContainer clusterStateCacheContainer, IsNSNumber endpoint, IsNSObject queue) => clusterStateCacheContainer -> endpoint -> queue -> Ptr () -> IO ()
readAttributeActivePowerWithClusterStateCache_endpoint_queue_completion clusterStateCacheContainer endpoint queue completion =
  do
    cls' <- getRequiredClass "MTRBaseClusterElectricalPowerMeasurement"
    sendClassMessage cls' readAttributeActivePowerWithClusterStateCache_endpoint_queue_completionSelector (toMTRClusterStateCacheContainer clusterStateCacheContainer) (toNSNumber endpoint) (toNSObject queue) completion

-- | @- readAttributeReactivePowerWithCompletion:@
readAttributeReactivePowerWithCompletion :: IsMTRBaseClusterElectricalPowerMeasurement mtrBaseClusterElectricalPowerMeasurement => mtrBaseClusterElectricalPowerMeasurement -> Ptr () -> IO ()
readAttributeReactivePowerWithCompletion mtrBaseClusterElectricalPowerMeasurement completion =
  sendMessage mtrBaseClusterElectricalPowerMeasurement readAttributeReactivePowerWithCompletionSelector completion

-- | @- subscribeAttributeReactivePowerWithParams:subscriptionEstablished:reportHandler:@
subscribeAttributeReactivePowerWithParams_subscriptionEstablished_reportHandler :: (IsMTRBaseClusterElectricalPowerMeasurement mtrBaseClusterElectricalPowerMeasurement, IsMTRSubscribeParams params) => mtrBaseClusterElectricalPowerMeasurement -> params -> Ptr () -> Ptr () -> IO ()
subscribeAttributeReactivePowerWithParams_subscriptionEstablished_reportHandler mtrBaseClusterElectricalPowerMeasurement params subscriptionEstablished reportHandler =
  sendMessage mtrBaseClusterElectricalPowerMeasurement subscribeAttributeReactivePowerWithParams_subscriptionEstablished_reportHandlerSelector (toMTRSubscribeParams params) subscriptionEstablished reportHandler

-- | @+ readAttributeReactivePowerWithClusterStateCache:endpoint:queue:completion:@
readAttributeReactivePowerWithClusterStateCache_endpoint_queue_completion :: (IsMTRClusterStateCacheContainer clusterStateCacheContainer, IsNSNumber endpoint, IsNSObject queue) => clusterStateCacheContainer -> endpoint -> queue -> Ptr () -> IO ()
readAttributeReactivePowerWithClusterStateCache_endpoint_queue_completion clusterStateCacheContainer endpoint queue completion =
  do
    cls' <- getRequiredClass "MTRBaseClusterElectricalPowerMeasurement"
    sendClassMessage cls' readAttributeReactivePowerWithClusterStateCache_endpoint_queue_completionSelector (toMTRClusterStateCacheContainer clusterStateCacheContainer) (toNSNumber endpoint) (toNSObject queue) completion

-- | @- readAttributeApparentPowerWithCompletion:@
readAttributeApparentPowerWithCompletion :: IsMTRBaseClusterElectricalPowerMeasurement mtrBaseClusterElectricalPowerMeasurement => mtrBaseClusterElectricalPowerMeasurement -> Ptr () -> IO ()
readAttributeApparentPowerWithCompletion mtrBaseClusterElectricalPowerMeasurement completion =
  sendMessage mtrBaseClusterElectricalPowerMeasurement readAttributeApparentPowerWithCompletionSelector completion

-- | @- subscribeAttributeApparentPowerWithParams:subscriptionEstablished:reportHandler:@
subscribeAttributeApparentPowerWithParams_subscriptionEstablished_reportHandler :: (IsMTRBaseClusterElectricalPowerMeasurement mtrBaseClusterElectricalPowerMeasurement, IsMTRSubscribeParams params) => mtrBaseClusterElectricalPowerMeasurement -> params -> Ptr () -> Ptr () -> IO ()
subscribeAttributeApparentPowerWithParams_subscriptionEstablished_reportHandler mtrBaseClusterElectricalPowerMeasurement params subscriptionEstablished reportHandler =
  sendMessage mtrBaseClusterElectricalPowerMeasurement subscribeAttributeApparentPowerWithParams_subscriptionEstablished_reportHandlerSelector (toMTRSubscribeParams params) subscriptionEstablished reportHandler

-- | @+ readAttributeApparentPowerWithClusterStateCache:endpoint:queue:completion:@
readAttributeApparentPowerWithClusterStateCache_endpoint_queue_completion :: (IsMTRClusterStateCacheContainer clusterStateCacheContainer, IsNSNumber endpoint, IsNSObject queue) => clusterStateCacheContainer -> endpoint -> queue -> Ptr () -> IO ()
readAttributeApparentPowerWithClusterStateCache_endpoint_queue_completion clusterStateCacheContainer endpoint queue completion =
  do
    cls' <- getRequiredClass "MTRBaseClusterElectricalPowerMeasurement"
    sendClassMessage cls' readAttributeApparentPowerWithClusterStateCache_endpoint_queue_completionSelector (toMTRClusterStateCacheContainer clusterStateCacheContainer) (toNSNumber endpoint) (toNSObject queue) completion

-- | @- readAttributeRMSVoltageWithCompletion:@
readAttributeRMSVoltageWithCompletion :: IsMTRBaseClusterElectricalPowerMeasurement mtrBaseClusterElectricalPowerMeasurement => mtrBaseClusterElectricalPowerMeasurement -> Ptr () -> IO ()
readAttributeRMSVoltageWithCompletion mtrBaseClusterElectricalPowerMeasurement completion =
  sendMessage mtrBaseClusterElectricalPowerMeasurement readAttributeRMSVoltageWithCompletionSelector completion

-- | @- subscribeAttributeRMSVoltageWithParams:subscriptionEstablished:reportHandler:@
subscribeAttributeRMSVoltageWithParams_subscriptionEstablished_reportHandler :: (IsMTRBaseClusterElectricalPowerMeasurement mtrBaseClusterElectricalPowerMeasurement, IsMTRSubscribeParams params) => mtrBaseClusterElectricalPowerMeasurement -> params -> Ptr () -> Ptr () -> IO ()
subscribeAttributeRMSVoltageWithParams_subscriptionEstablished_reportHandler mtrBaseClusterElectricalPowerMeasurement params subscriptionEstablished reportHandler =
  sendMessage mtrBaseClusterElectricalPowerMeasurement subscribeAttributeRMSVoltageWithParams_subscriptionEstablished_reportHandlerSelector (toMTRSubscribeParams params) subscriptionEstablished reportHandler

-- | @+ readAttributeRMSVoltageWithClusterStateCache:endpoint:queue:completion:@
readAttributeRMSVoltageWithClusterStateCache_endpoint_queue_completion :: (IsMTRClusterStateCacheContainer clusterStateCacheContainer, IsNSNumber endpoint, IsNSObject queue) => clusterStateCacheContainer -> endpoint -> queue -> Ptr () -> IO ()
readAttributeRMSVoltageWithClusterStateCache_endpoint_queue_completion clusterStateCacheContainer endpoint queue completion =
  do
    cls' <- getRequiredClass "MTRBaseClusterElectricalPowerMeasurement"
    sendClassMessage cls' readAttributeRMSVoltageWithClusterStateCache_endpoint_queue_completionSelector (toMTRClusterStateCacheContainer clusterStateCacheContainer) (toNSNumber endpoint) (toNSObject queue) completion

-- | @- readAttributeRMSCurrentWithCompletion:@
readAttributeRMSCurrentWithCompletion :: IsMTRBaseClusterElectricalPowerMeasurement mtrBaseClusterElectricalPowerMeasurement => mtrBaseClusterElectricalPowerMeasurement -> Ptr () -> IO ()
readAttributeRMSCurrentWithCompletion mtrBaseClusterElectricalPowerMeasurement completion =
  sendMessage mtrBaseClusterElectricalPowerMeasurement readAttributeRMSCurrentWithCompletionSelector completion

-- | @- subscribeAttributeRMSCurrentWithParams:subscriptionEstablished:reportHandler:@
subscribeAttributeRMSCurrentWithParams_subscriptionEstablished_reportHandler :: (IsMTRBaseClusterElectricalPowerMeasurement mtrBaseClusterElectricalPowerMeasurement, IsMTRSubscribeParams params) => mtrBaseClusterElectricalPowerMeasurement -> params -> Ptr () -> Ptr () -> IO ()
subscribeAttributeRMSCurrentWithParams_subscriptionEstablished_reportHandler mtrBaseClusterElectricalPowerMeasurement params subscriptionEstablished reportHandler =
  sendMessage mtrBaseClusterElectricalPowerMeasurement subscribeAttributeRMSCurrentWithParams_subscriptionEstablished_reportHandlerSelector (toMTRSubscribeParams params) subscriptionEstablished reportHandler

-- | @+ readAttributeRMSCurrentWithClusterStateCache:endpoint:queue:completion:@
readAttributeRMSCurrentWithClusterStateCache_endpoint_queue_completion :: (IsMTRClusterStateCacheContainer clusterStateCacheContainer, IsNSNumber endpoint, IsNSObject queue) => clusterStateCacheContainer -> endpoint -> queue -> Ptr () -> IO ()
readAttributeRMSCurrentWithClusterStateCache_endpoint_queue_completion clusterStateCacheContainer endpoint queue completion =
  do
    cls' <- getRequiredClass "MTRBaseClusterElectricalPowerMeasurement"
    sendClassMessage cls' readAttributeRMSCurrentWithClusterStateCache_endpoint_queue_completionSelector (toMTRClusterStateCacheContainer clusterStateCacheContainer) (toNSNumber endpoint) (toNSObject queue) completion

-- | @- readAttributeRMSPowerWithCompletion:@
readAttributeRMSPowerWithCompletion :: IsMTRBaseClusterElectricalPowerMeasurement mtrBaseClusterElectricalPowerMeasurement => mtrBaseClusterElectricalPowerMeasurement -> Ptr () -> IO ()
readAttributeRMSPowerWithCompletion mtrBaseClusterElectricalPowerMeasurement completion =
  sendMessage mtrBaseClusterElectricalPowerMeasurement readAttributeRMSPowerWithCompletionSelector completion

-- | @- subscribeAttributeRMSPowerWithParams:subscriptionEstablished:reportHandler:@
subscribeAttributeRMSPowerWithParams_subscriptionEstablished_reportHandler :: (IsMTRBaseClusterElectricalPowerMeasurement mtrBaseClusterElectricalPowerMeasurement, IsMTRSubscribeParams params) => mtrBaseClusterElectricalPowerMeasurement -> params -> Ptr () -> Ptr () -> IO ()
subscribeAttributeRMSPowerWithParams_subscriptionEstablished_reportHandler mtrBaseClusterElectricalPowerMeasurement params subscriptionEstablished reportHandler =
  sendMessage mtrBaseClusterElectricalPowerMeasurement subscribeAttributeRMSPowerWithParams_subscriptionEstablished_reportHandlerSelector (toMTRSubscribeParams params) subscriptionEstablished reportHandler

-- | @+ readAttributeRMSPowerWithClusterStateCache:endpoint:queue:completion:@
readAttributeRMSPowerWithClusterStateCache_endpoint_queue_completion :: (IsMTRClusterStateCacheContainer clusterStateCacheContainer, IsNSNumber endpoint, IsNSObject queue) => clusterStateCacheContainer -> endpoint -> queue -> Ptr () -> IO ()
readAttributeRMSPowerWithClusterStateCache_endpoint_queue_completion clusterStateCacheContainer endpoint queue completion =
  do
    cls' <- getRequiredClass "MTRBaseClusterElectricalPowerMeasurement"
    sendClassMessage cls' readAttributeRMSPowerWithClusterStateCache_endpoint_queue_completionSelector (toMTRClusterStateCacheContainer clusterStateCacheContainer) (toNSNumber endpoint) (toNSObject queue) completion

-- | @- readAttributeFrequencyWithCompletion:@
readAttributeFrequencyWithCompletion :: IsMTRBaseClusterElectricalPowerMeasurement mtrBaseClusterElectricalPowerMeasurement => mtrBaseClusterElectricalPowerMeasurement -> Ptr () -> IO ()
readAttributeFrequencyWithCompletion mtrBaseClusterElectricalPowerMeasurement completion =
  sendMessage mtrBaseClusterElectricalPowerMeasurement readAttributeFrequencyWithCompletionSelector completion

-- | @- subscribeAttributeFrequencyWithParams:subscriptionEstablished:reportHandler:@
subscribeAttributeFrequencyWithParams_subscriptionEstablished_reportHandler :: (IsMTRBaseClusterElectricalPowerMeasurement mtrBaseClusterElectricalPowerMeasurement, IsMTRSubscribeParams params) => mtrBaseClusterElectricalPowerMeasurement -> params -> Ptr () -> Ptr () -> IO ()
subscribeAttributeFrequencyWithParams_subscriptionEstablished_reportHandler mtrBaseClusterElectricalPowerMeasurement params subscriptionEstablished reportHandler =
  sendMessage mtrBaseClusterElectricalPowerMeasurement subscribeAttributeFrequencyWithParams_subscriptionEstablished_reportHandlerSelector (toMTRSubscribeParams params) subscriptionEstablished reportHandler

-- | @+ readAttributeFrequencyWithClusterStateCache:endpoint:queue:completion:@
readAttributeFrequencyWithClusterStateCache_endpoint_queue_completion :: (IsMTRClusterStateCacheContainer clusterStateCacheContainer, IsNSNumber endpoint, IsNSObject queue) => clusterStateCacheContainer -> endpoint -> queue -> Ptr () -> IO ()
readAttributeFrequencyWithClusterStateCache_endpoint_queue_completion clusterStateCacheContainer endpoint queue completion =
  do
    cls' <- getRequiredClass "MTRBaseClusterElectricalPowerMeasurement"
    sendClassMessage cls' readAttributeFrequencyWithClusterStateCache_endpoint_queue_completionSelector (toMTRClusterStateCacheContainer clusterStateCacheContainer) (toNSNumber endpoint) (toNSObject queue) completion

-- | @- readAttributeHarmonicCurrentsWithCompletion:@
readAttributeHarmonicCurrentsWithCompletion :: IsMTRBaseClusterElectricalPowerMeasurement mtrBaseClusterElectricalPowerMeasurement => mtrBaseClusterElectricalPowerMeasurement -> Ptr () -> IO ()
readAttributeHarmonicCurrentsWithCompletion mtrBaseClusterElectricalPowerMeasurement completion =
  sendMessage mtrBaseClusterElectricalPowerMeasurement readAttributeHarmonicCurrentsWithCompletionSelector completion

-- | @- subscribeAttributeHarmonicCurrentsWithParams:subscriptionEstablished:reportHandler:@
subscribeAttributeHarmonicCurrentsWithParams_subscriptionEstablished_reportHandler :: (IsMTRBaseClusterElectricalPowerMeasurement mtrBaseClusterElectricalPowerMeasurement, IsMTRSubscribeParams params) => mtrBaseClusterElectricalPowerMeasurement -> params -> Ptr () -> Ptr () -> IO ()
subscribeAttributeHarmonicCurrentsWithParams_subscriptionEstablished_reportHandler mtrBaseClusterElectricalPowerMeasurement params subscriptionEstablished reportHandler =
  sendMessage mtrBaseClusterElectricalPowerMeasurement subscribeAttributeHarmonicCurrentsWithParams_subscriptionEstablished_reportHandlerSelector (toMTRSubscribeParams params) subscriptionEstablished reportHandler

-- | @+ readAttributeHarmonicCurrentsWithClusterStateCache:endpoint:queue:completion:@
readAttributeHarmonicCurrentsWithClusterStateCache_endpoint_queue_completion :: (IsMTRClusterStateCacheContainer clusterStateCacheContainer, IsNSNumber endpoint, IsNSObject queue) => clusterStateCacheContainer -> endpoint -> queue -> Ptr () -> IO ()
readAttributeHarmonicCurrentsWithClusterStateCache_endpoint_queue_completion clusterStateCacheContainer endpoint queue completion =
  do
    cls' <- getRequiredClass "MTRBaseClusterElectricalPowerMeasurement"
    sendClassMessage cls' readAttributeHarmonicCurrentsWithClusterStateCache_endpoint_queue_completionSelector (toMTRClusterStateCacheContainer clusterStateCacheContainer) (toNSNumber endpoint) (toNSObject queue) completion

-- | @- readAttributeHarmonicPhasesWithCompletion:@
readAttributeHarmonicPhasesWithCompletion :: IsMTRBaseClusterElectricalPowerMeasurement mtrBaseClusterElectricalPowerMeasurement => mtrBaseClusterElectricalPowerMeasurement -> Ptr () -> IO ()
readAttributeHarmonicPhasesWithCompletion mtrBaseClusterElectricalPowerMeasurement completion =
  sendMessage mtrBaseClusterElectricalPowerMeasurement readAttributeHarmonicPhasesWithCompletionSelector completion

-- | @- subscribeAttributeHarmonicPhasesWithParams:subscriptionEstablished:reportHandler:@
subscribeAttributeHarmonicPhasesWithParams_subscriptionEstablished_reportHandler :: (IsMTRBaseClusterElectricalPowerMeasurement mtrBaseClusterElectricalPowerMeasurement, IsMTRSubscribeParams params) => mtrBaseClusterElectricalPowerMeasurement -> params -> Ptr () -> Ptr () -> IO ()
subscribeAttributeHarmonicPhasesWithParams_subscriptionEstablished_reportHandler mtrBaseClusterElectricalPowerMeasurement params subscriptionEstablished reportHandler =
  sendMessage mtrBaseClusterElectricalPowerMeasurement subscribeAttributeHarmonicPhasesWithParams_subscriptionEstablished_reportHandlerSelector (toMTRSubscribeParams params) subscriptionEstablished reportHandler

-- | @+ readAttributeHarmonicPhasesWithClusterStateCache:endpoint:queue:completion:@
readAttributeHarmonicPhasesWithClusterStateCache_endpoint_queue_completion :: (IsMTRClusterStateCacheContainer clusterStateCacheContainer, IsNSNumber endpoint, IsNSObject queue) => clusterStateCacheContainer -> endpoint -> queue -> Ptr () -> IO ()
readAttributeHarmonicPhasesWithClusterStateCache_endpoint_queue_completion clusterStateCacheContainer endpoint queue completion =
  do
    cls' <- getRequiredClass "MTRBaseClusterElectricalPowerMeasurement"
    sendClassMessage cls' readAttributeHarmonicPhasesWithClusterStateCache_endpoint_queue_completionSelector (toMTRClusterStateCacheContainer clusterStateCacheContainer) (toNSNumber endpoint) (toNSObject queue) completion

-- | @- readAttributePowerFactorWithCompletion:@
readAttributePowerFactorWithCompletion :: IsMTRBaseClusterElectricalPowerMeasurement mtrBaseClusterElectricalPowerMeasurement => mtrBaseClusterElectricalPowerMeasurement -> Ptr () -> IO ()
readAttributePowerFactorWithCompletion mtrBaseClusterElectricalPowerMeasurement completion =
  sendMessage mtrBaseClusterElectricalPowerMeasurement readAttributePowerFactorWithCompletionSelector completion

-- | @- subscribeAttributePowerFactorWithParams:subscriptionEstablished:reportHandler:@
subscribeAttributePowerFactorWithParams_subscriptionEstablished_reportHandler :: (IsMTRBaseClusterElectricalPowerMeasurement mtrBaseClusterElectricalPowerMeasurement, IsMTRSubscribeParams params) => mtrBaseClusterElectricalPowerMeasurement -> params -> Ptr () -> Ptr () -> IO ()
subscribeAttributePowerFactorWithParams_subscriptionEstablished_reportHandler mtrBaseClusterElectricalPowerMeasurement params subscriptionEstablished reportHandler =
  sendMessage mtrBaseClusterElectricalPowerMeasurement subscribeAttributePowerFactorWithParams_subscriptionEstablished_reportHandlerSelector (toMTRSubscribeParams params) subscriptionEstablished reportHandler

-- | @+ readAttributePowerFactorWithClusterStateCache:endpoint:queue:completion:@
readAttributePowerFactorWithClusterStateCache_endpoint_queue_completion :: (IsMTRClusterStateCacheContainer clusterStateCacheContainer, IsNSNumber endpoint, IsNSObject queue) => clusterStateCacheContainer -> endpoint -> queue -> Ptr () -> IO ()
readAttributePowerFactorWithClusterStateCache_endpoint_queue_completion clusterStateCacheContainer endpoint queue completion =
  do
    cls' <- getRequiredClass "MTRBaseClusterElectricalPowerMeasurement"
    sendClassMessage cls' readAttributePowerFactorWithClusterStateCache_endpoint_queue_completionSelector (toMTRClusterStateCacheContainer clusterStateCacheContainer) (toNSNumber endpoint) (toNSObject queue) completion

-- | @- readAttributeNeutralCurrentWithCompletion:@
readAttributeNeutralCurrentWithCompletion :: IsMTRBaseClusterElectricalPowerMeasurement mtrBaseClusterElectricalPowerMeasurement => mtrBaseClusterElectricalPowerMeasurement -> Ptr () -> IO ()
readAttributeNeutralCurrentWithCompletion mtrBaseClusterElectricalPowerMeasurement completion =
  sendMessage mtrBaseClusterElectricalPowerMeasurement readAttributeNeutralCurrentWithCompletionSelector completion

-- | @- subscribeAttributeNeutralCurrentWithParams:subscriptionEstablished:reportHandler:@
subscribeAttributeNeutralCurrentWithParams_subscriptionEstablished_reportHandler :: (IsMTRBaseClusterElectricalPowerMeasurement mtrBaseClusterElectricalPowerMeasurement, IsMTRSubscribeParams params) => mtrBaseClusterElectricalPowerMeasurement -> params -> Ptr () -> Ptr () -> IO ()
subscribeAttributeNeutralCurrentWithParams_subscriptionEstablished_reportHandler mtrBaseClusterElectricalPowerMeasurement params subscriptionEstablished reportHandler =
  sendMessage mtrBaseClusterElectricalPowerMeasurement subscribeAttributeNeutralCurrentWithParams_subscriptionEstablished_reportHandlerSelector (toMTRSubscribeParams params) subscriptionEstablished reportHandler

-- | @+ readAttributeNeutralCurrentWithClusterStateCache:endpoint:queue:completion:@
readAttributeNeutralCurrentWithClusterStateCache_endpoint_queue_completion :: (IsMTRClusterStateCacheContainer clusterStateCacheContainer, IsNSNumber endpoint, IsNSObject queue) => clusterStateCacheContainer -> endpoint -> queue -> Ptr () -> IO ()
readAttributeNeutralCurrentWithClusterStateCache_endpoint_queue_completion clusterStateCacheContainer endpoint queue completion =
  do
    cls' <- getRequiredClass "MTRBaseClusterElectricalPowerMeasurement"
    sendClassMessage cls' readAttributeNeutralCurrentWithClusterStateCache_endpoint_queue_completionSelector (toMTRClusterStateCacheContainer clusterStateCacheContainer) (toNSNumber endpoint) (toNSObject queue) completion

-- | @- readAttributeGeneratedCommandListWithCompletion:@
readAttributeGeneratedCommandListWithCompletion :: IsMTRBaseClusterElectricalPowerMeasurement mtrBaseClusterElectricalPowerMeasurement => mtrBaseClusterElectricalPowerMeasurement -> Ptr () -> IO ()
readAttributeGeneratedCommandListWithCompletion mtrBaseClusterElectricalPowerMeasurement completion =
  sendMessage mtrBaseClusterElectricalPowerMeasurement readAttributeGeneratedCommandListWithCompletionSelector completion

-- | @- subscribeAttributeGeneratedCommandListWithParams:subscriptionEstablished:reportHandler:@
subscribeAttributeGeneratedCommandListWithParams_subscriptionEstablished_reportHandler :: (IsMTRBaseClusterElectricalPowerMeasurement mtrBaseClusterElectricalPowerMeasurement, IsMTRSubscribeParams params) => mtrBaseClusterElectricalPowerMeasurement -> params -> Ptr () -> Ptr () -> IO ()
subscribeAttributeGeneratedCommandListWithParams_subscriptionEstablished_reportHandler mtrBaseClusterElectricalPowerMeasurement params subscriptionEstablished reportHandler =
  sendMessage mtrBaseClusterElectricalPowerMeasurement subscribeAttributeGeneratedCommandListWithParams_subscriptionEstablished_reportHandlerSelector (toMTRSubscribeParams params) subscriptionEstablished reportHandler

-- | @+ readAttributeGeneratedCommandListWithClusterStateCache:endpoint:queue:completion:@
readAttributeGeneratedCommandListWithClusterStateCache_endpoint_queue_completion :: (IsMTRClusterStateCacheContainer clusterStateCacheContainer, IsNSNumber endpoint, IsNSObject queue) => clusterStateCacheContainer -> endpoint -> queue -> Ptr () -> IO ()
readAttributeGeneratedCommandListWithClusterStateCache_endpoint_queue_completion clusterStateCacheContainer endpoint queue completion =
  do
    cls' <- getRequiredClass "MTRBaseClusterElectricalPowerMeasurement"
    sendClassMessage cls' readAttributeGeneratedCommandListWithClusterStateCache_endpoint_queue_completionSelector (toMTRClusterStateCacheContainer clusterStateCacheContainer) (toNSNumber endpoint) (toNSObject queue) completion

-- | @- readAttributeAcceptedCommandListWithCompletion:@
readAttributeAcceptedCommandListWithCompletion :: IsMTRBaseClusterElectricalPowerMeasurement mtrBaseClusterElectricalPowerMeasurement => mtrBaseClusterElectricalPowerMeasurement -> Ptr () -> IO ()
readAttributeAcceptedCommandListWithCompletion mtrBaseClusterElectricalPowerMeasurement completion =
  sendMessage mtrBaseClusterElectricalPowerMeasurement readAttributeAcceptedCommandListWithCompletionSelector completion

-- | @- subscribeAttributeAcceptedCommandListWithParams:subscriptionEstablished:reportHandler:@
subscribeAttributeAcceptedCommandListWithParams_subscriptionEstablished_reportHandler :: (IsMTRBaseClusterElectricalPowerMeasurement mtrBaseClusterElectricalPowerMeasurement, IsMTRSubscribeParams params) => mtrBaseClusterElectricalPowerMeasurement -> params -> Ptr () -> Ptr () -> IO ()
subscribeAttributeAcceptedCommandListWithParams_subscriptionEstablished_reportHandler mtrBaseClusterElectricalPowerMeasurement params subscriptionEstablished reportHandler =
  sendMessage mtrBaseClusterElectricalPowerMeasurement subscribeAttributeAcceptedCommandListWithParams_subscriptionEstablished_reportHandlerSelector (toMTRSubscribeParams params) subscriptionEstablished reportHandler

-- | @+ readAttributeAcceptedCommandListWithClusterStateCache:endpoint:queue:completion:@
readAttributeAcceptedCommandListWithClusterStateCache_endpoint_queue_completion :: (IsMTRClusterStateCacheContainer clusterStateCacheContainer, IsNSNumber endpoint, IsNSObject queue) => clusterStateCacheContainer -> endpoint -> queue -> Ptr () -> IO ()
readAttributeAcceptedCommandListWithClusterStateCache_endpoint_queue_completion clusterStateCacheContainer endpoint queue completion =
  do
    cls' <- getRequiredClass "MTRBaseClusterElectricalPowerMeasurement"
    sendClassMessage cls' readAttributeAcceptedCommandListWithClusterStateCache_endpoint_queue_completionSelector (toMTRClusterStateCacheContainer clusterStateCacheContainer) (toNSNumber endpoint) (toNSObject queue) completion

-- | @- readAttributeAttributeListWithCompletion:@
readAttributeAttributeListWithCompletion :: IsMTRBaseClusterElectricalPowerMeasurement mtrBaseClusterElectricalPowerMeasurement => mtrBaseClusterElectricalPowerMeasurement -> Ptr () -> IO ()
readAttributeAttributeListWithCompletion mtrBaseClusterElectricalPowerMeasurement completion =
  sendMessage mtrBaseClusterElectricalPowerMeasurement readAttributeAttributeListWithCompletionSelector completion

-- | @- subscribeAttributeAttributeListWithParams:subscriptionEstablished:reportHandler:@
subscribeAttributeAttributeListWithParams_subscriptionEstablished_reportHandler :: (IsMTRBaseClusterElectricalPowerMeasurement mtrBaseClusterElectricalPowerMeasurement, IsMTRSubscribeParams params) => mtrBaseClusterElectricalPowerMeasurement -> params -> Ptr () -> Ptr () -> IO ()
subscribeAttributeAttributeListWithParams_subscriptionEstablished_reportHandler mtrBaseClusterElectricalPowerMeasurement params subscriptionEstablished reportHandler =
  sendMessage mtrBaseClusterElectricalPowerMeasurement subscribeAttributeAttributeListWithParams_subscriptionEstablished_reportHandlerSelector (toMTRSubscribeParams params) subscriptionEstablished reportHandler

-- | @+ readAttributeAttributeListWithClusterStateCache:endpoint:queue:completion:@
readAttributeAttributeListWithClusterStateCache_endpoint_queue_completion :: (IsMTRClusterStateCacheContainer clusterStateCacheContainer, IsNSNumber endpoint, IsNSObject queue) => clusterStateCacheContainer -> endpoint -> queue -> Ptr () -> IO ()
readAttributeAttributeListWithClusterStateCache_endpoint_queue_completion clusterStateCacheContainer endpoint queue completion =
  do
    cls' <- getRequiredClass "MTRBaseClusterElectricalPowerMeasurement"
    sendClassMessage cls' readAttributeAttributeListWithClusterStateCache_endpoint_queue_completionSelector (toMTRClusterStateCacheContainer clusterStateCacheContainer) (toNSNumber endpoint) (toNSObject queue) completion

-- | @- readAttributeFeatureMapWithCompletion:@
readAttributeFeatureMapWithCompletion :: IsMTRBaseClusterElectricalPowerMeasurement mtrBaseClusterElectricalPowerMeasurement => mtrBaseClusterElectricalPowerMeasurement -> Ptr () -> IO ()
readAttributeFeatureMapWithCompletion mtrBaseClusterElectricalPowerMeasurement completion =
  sendMessage mtrBaseClusterElectricalPowerMeasurement readAttributeFeatureMapWithCompletionSelector completion

-- | @- subscribeAttributeFeatureMapWithParams:subscriptionEstablished:reportHandler:@
subscribeAttributeFeatureMapWithParams_subscriptionEstablished_reportHandler :: (IsMTRBaseClusterElectricalPowerMeasurement mtrBaseClusterElectricalPowerMeasurement, IsMTRSubscribeParams params) => mtrBaseClusterElectricalPowerMeasurement -> params -> Ptr () -> Ptr () -> IO ()
subscribeAttributeFeatureMapWithParams_subscriptionEstablished_reportHandler mtrBaseClusterElectricalPowerMeasurement params subscriptionEstablished reportHandler =
  sendMessage mtrBaseClusterElectricalPowerMeasurement subscribeAttributeFeatureMapWithParams_subscriptionEstablished_reportHandlerSelector (toMTRSubscribeParams params) subscriptionEstablished reportHandler

-- | @+ readAttributeFeatureMapWithClusterStateCache:endpoint:queue:completion:@
readAttributeFeatureMapWithClusterStateCache_endpoint_queue_completion :: (IsMTRClusterStateCacheContainer clusterStateCacheContainer, IsNSNumber endpoint, IsNSObject queue) => clusterStateCacheContainer -> endpoint -> queue -> Ptr () -> IO ()
readAttributeFeatureMapWithClusterStateCache_endpoint_queue_completion clusterStateCacheContainer endpoint queue completion =
  do
    cls' <- getRequiredClass "MTRBaseClusterElectricalPowerMeasurement"
    sendClassMessage cls' readAttributeFeatureMapWithClusterStateCache_endpoint_queue_completionSelector (toMTRClusterStateCacheContainer clusterStateCacheContainer) (toNSNumber endpoint) (toNSObject queue) completion

-- | @- readAttributeClusterRevisionWithCompletion:@
readAttributeClusterRevisionWithCompletion :: IsMTRBaseClusterElectricalPowerMeasurement mtrBaseClusterElectricalPowerMeasurement => mtrBaseClusterElectricalPowerMeasurement -> Ptr () -> IO ()
readAttributeClusterRevisionWithCompletion mtrBaseClusterElectricalPowerMeasurement completion =
  sendMessage mtrBaseClusterElectricalPowerMeasurement readAttributeClusterRevisionWithCompletionSelector completion

-- | @- subscribeAttributeClusterRevisionWithParams:subscriptionEstablished:reportHandler:@
subscribeAttributeClusterRevisionWithParams_subscriptionEstablished_reportHandler :: (IsMTRBaseClusterElectricalPowerMeasurement mtrBaseClusterElectricalPowerMeasurement, IsMTRSubscribeParams params) => mtrBaseClusterElectricalPowerMeasurement -> params -> Ptr () -> Ptr () -> IO ()
subscribeAttributeClusterRevisionWithParams_subscriptionEstablished_reportHandler mtrBaseClusterElectricalPowerMeasurement params subscriptionEstablished reportHandler =
  sendMessage mtrBaseClusterElectricalPowerMeasurement subscribeAttributeClusterRevisionWithParams_subscriptionEstablished_reportHandlerSelector (toMTRSubscribeParams params) subscriptionEstablished reportHandler

-- | @+ readAttributeClusterRevisionWithClusterStateCache:endpoint:queue:completion:@
readAttributeClusterRevisionWithClusterStateCache_endpoint_queue_completion :: (IsMTRClusterStateCacheContainer clusterStateCacheContainer, IsNSNumber endpoint, IsNSObject queue) => clusterStateCacheContainer -> endpoint -> queue -> Ptr () -> IO ()
readAttributeClusterRevisionWithClusterStateCache_endpoint_queue_completion clusterStateCacheContainer endpoint queue completion =
  do
    cls' <- getRequiredClass "MTRBaseClusterElectricalPowerMeasurement"
    sendClassMessage cls' readAttributeClusterRevisionWithClusterStateCache_endpoint_queue_completionSelector (toMTRClusterStateCacheContainer clusterStateCacheContainer) (toNSNumber endpoint) (toNSObject queue) completion

-- | @- init@
init_ :: IsMTRBaseClusterElectricalPowerMeasurement mtrBaseClusterElectricalPowerMeasurement => mtrBaseClusterElectricalPowerMeasurement -> IO (Id MTRBaseClusterElectricalPowerMeasurement)
init_ mtrBaseClusterElectricalPowerMeasurement =
  sendOwnedMessage mtrBaseClusterElectricalPowerMeasurement initSelector

-- | @+ new@
new :: IO (Id MTRBaseClusterElectricalPowerMeasurement)
new  =
  do
    cls' <- getRequiredClass "MTRBaseClusterElectricalPowerMeasurement"
    sendOwnedClassMessage cls' newSelector

-- | For all instance methods (reads, writes, commands) that take a completion, the completion will be called on the provided queue.
--
-- ObjC selector: @- initWithDevice:endpointID:queue:@
initWithDevice_endpointID_queue :: (IsMTRBaseClusterElectricalPowerMeasurement mtrBaseClusterElectricalPowerMeasurement, IsMTRBaseDevice device, IsNSNumber endpointID, IsNSObject queue) => mtrBaseClusterElectricalPowerMeasurement -> device -> endpointID -> queue -> IO (Id MTRBaseClusterElectricalPowerMeasurement)
initWithDevice_endpointID_queue mtrBaseClusterElectricalPowerMeasurement device endpointID queue =
  sendOwnedMessage mtrBaseClusterElectricalPowerMeasurement initWithDevice_endpointID_queueSelector (toMTRBaseDevice device) (toNSNumber endpointID) (toNSObject queue)

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @readAttributePowerModeWithCompletion:@
readAttributePowerModeWithCompletionSelector :: Selector '[Ptr ()] ()
readAttributePowerModeWithCompletionSelector = mkSelector "readAttributePowerModeWithCompletion:"

-- | @Selector@ for @subscribeAttributePowerModeWithParams:subscriptionEstablished:reportHandler:@
subscribeAttributePowerModeWithParams_subscriptionEstablished_reportHandlerSelector :: Selector '[Id MTRSubscribeParams, Ptr (), Ptr ()] ()
subscribeAttributePowerModeWithParams_subscriptionEstablished_reportHandlerSelector = mkSelector "subscribeAttributePowerModeWithParams:subscriptionEstablished:reportHandler:"

-- | @Selector@ for @readAttributePowerModeWithClusterStateCache:endpoint:queue:completion:@
readAttributePowerModeWithClusterStateCache_endpoint_queue_completionSelector :: Selector '[Id MTRClusterStateCacheContainer, Id NSNumber, Id NSObject, Ptr ()] ()
readAttributePowerModeWithClusterStateCache_endpoint_queue_completionSelector = mkSelector "readAttributePowerModeWithClusterStateCache:endpoint:queue:completion:"

-- | @Selector@ for @readAttributeNumberOfMeasurementTypesWithCompletion:@
readAttributeNumberOfMeasurementTypesWithCompletionSelector :: Selector '[Ptr ()] ()
readAttributeNumberOfMeasurementTypesWithCompletionSelector = mkSelector "readAttributeNumberOfMeasurementTypesWithCompletion:"

-- | @Selector@ for @subscribeAttributeNumberOfMeasurementTypesWithParams:subscriptionEstablished:reportHandler:@
subscribeAttributeNumberOfMeasurementTypesWithParams_subscriptionEstablished_reportHandlerSelector :: Selector '[Id MTRSubscribeParams, Ptr (), Ptr ()] ()
subscribeAttributeNumberOfMeasurementTypesWithParams_subscriptionEstablished_reportHandlerSelector = mkSelector "subscribeAttributeNumberOfMeasurementTypesWithParams:subscriptionEstablished:reportHandler:"

-- | @Selector@ for @readAttributeNumberOfMeasurementTypesWithClusterStateCache:endpoint:queue:completion:@
readAttributeNumberOfMeasurementTypesWithClusterStateCache_endpoint_queue_completionSelector :: Selector '[Id MTRClusterStateCacheContainer, Id NSNumber, Id NSObject, Ptr ()] ()
readAttributeNumberOfMeasurementTypesWithClusterStateCache_endpoint_queue_completionSelector = mkSelector "readAttributeNumberOfMeasurementTypesWithClusterStateCache:endpoint:queue:completion:"

-- | @Selector@ for @readAttributeAccuracyWithCompletion:@
readAttributeAccuracyWithCompletionSelector :: Selector '[Ptr ()] ()
readAttributeAccuracyWithCompletionSelector = mkSelector "readAttributeAccuracyWithCompletion:"

-- | @Selector@ for @subscribeAttributeAccuracyWithParams:subscriptionEstablished:reportHandler:@
subscribeAttributeAccuracyWithParams_subscriptionEstablished_reportHandlerSelector :: Selector '[Id MTRSubscribeParams, Ptr (), Ptr ()] ()
subscribeAttributeAccuracyWithParams_subscriptionEstablished_reportHandlerSelector = mkSelector "subscribeAttributeAccuracyWithParams:subscriptionEstablished:reportHandler:"

-- | @Selector@ for @readAttributeAccuracyWithClusterStateCache:endpoint:queue:completion:@
readAttributeAccuracyWithClusterStateCache_endpoint_queue_completionSelector :: Selector '[Id MTRClusterStateCacheContainer, Id NSNumber, Id NSObject, Ptr ()] ()
readAttributeAccuracyWithClusterStateCache_endpoint_queue_completionSelector = mkSelector "readAttributeAccuracyWithClusterStateCache:endpoint:queue:completion:"

-- | @Selector@ for @readAttributeRangesWithCompletion:@
readAttributeRangesWithCompletionSelector :: Selector '[Ptr ()] ()
readAttributeRangesWithCompletionSelector = mkSelector "readAttributeRangesWithCompletion:"

-- | @Selector@ for @subscribeAttributeRangesWithParams:subscriptionEstablished:reportHandler:@
subscribeAttributeRangesWithParams_subscriptionEstablished_reportHandlerSelector :: Selector '[Id MTRSubscribeParams, Ptr (), Ptr ()] ()
subscribeAttributeRangesWithParams_subscriptionEstablished_reportHandlerSelector = mkSelector "subscribeAttributeRangesWithParams:subscriptionEstablished:reportHandler:"

-- | @Selector@ for @readAttributeRangesWithClusterStateCache:endpoint:queue:completion:@
readAttributeRangesWithClusterStateCache_endpoint_queue_completionSelector :: Selector '[Id MTRClusterStateCacheContainer, Id NSNumber, Id NSObject, Ptr ()] ()
readAttributeRangesWithClusterStateCache_endpoint_queue_completionSelector = mkSelector "readAttributeRangesWithClusterStateCache:endpoint:queue:completion:"

-- | @Selector@ for @readAttributeVoltageWithCompletion:@
readAttributeVoltageWithCompletionSelector :: Selector '[Ptr ()] ()
readAttributeVoltageWithCompletionSelector = mkSelector "readAttributeVoltageWithCompletion:"

-- | @Selector@ for @subscribeAttributeVoltageWithParams:subscriptionEstablished:reportHandler:@
subscribeAttributeVoltageWithParams_subscriptionEstablished_reportHandlerSelector :: Selector '[Id MTRSubscribeParams, Ptr (), Ptr ()] ()
subscribeAttributeVoltageWithParams_subscriptionEstablished_reportHandlerSelector = mkSelector "subscribeAttributeVoltageWithParams:subscriptionEstablished:reportHandler:"

-- | @Selector@ for @readAttributeVoltageWithClusterStateCache:endpoint:queue:completion:@
readAttributeVoltageWithClusterStateCache_endpoint_queue_completionSelector :: Selector '[Id MTRClusterStateCacheContainer, Id NSNumber, Id NSObject, Ptr ()] ()
readAttributeVoltageWithClusterStateCache_endpoint_queue_completionSelector = mkSelector "readAttributeVoltageWithClusterStateCache:endpoint:queue:completion:"

-- | @Selector@ for @readAttributeActiveCurrentWithCompletion:@
readAttributeActiveCurrentWithCompletionSelector :: Selector '[Ptr ()] ()
readAttributeActiveCurrentWithCompletionSelector = mkSelector "readAttributeActiveCurrentWithCompletion:"

-- | @Selector@ for @subscribeAttributeActiveCurrentWithParams:subscriptionEstablished:reportHandler:@
subscribeAttributeActiveCurrentWithParams_subscriptionEstablished_reportHandlerSelector :: Selector '[Id MTRSubscribeParams, Ptr (), Ptr ()] ()
subscribeAttributeActiveCurrentWithParams_subscriptionEstablished_reportHandlerSelector = mkSelector "subscribeAttributeActiveCurrentWithParams:subscriptionEstablished:reportHandler:"

-- | @Selector@ for @readAttributeActiveCurrentWithClusterStateCache:endpoint:queue:completion:@
readAttributeActiveCurrentWithClusterStateCache_endpoint_queue_completionSelector :: Selector '[Id MTRClusterStateCacheContainer, Id NSNumber, Id NSObject, Ptr ()] ()
readAttributeActiveCurrentWithClusterStateCache_endpoint_queue_completionSelector = mkSelector "readAttributeActiveCurrentWithClusterStateCache:endpoint:queue:completion:"

-- | @Selector@ for @readAttributeReactiveCurrentWithCompletion:@
readAttributeReactiveCurrentWithCompletionSelector :: Selector '[Ptr ()] ()
readAttributeReactiveCurrentWithCompletionSelector = mkSelector "readAttributeReactiveCurrentWithCompletion:"

-- | @Selector@ for @subscribeAttributeReactiveCurrentWithParams:subscriptionEstablished:reportHandler:@
subscribeAttributeReactiveCurrentWithParams_subscriptionEstablished_reportHandlerSelector :: Selector '[Id MTRSubscribeParams, Ptr (), Ptr ()] ()
subscribeAttributeReactiveCurrentWithParams_subscriptionEstablished_reportHandlerSelector = mkSelector "subscribeAttributeReactiveCurrentWithParams:subscriptionEstablished:reportHandler:"

-- | @Selector@ for @readAttributeReactiveCurrentWithClusterStateCache:endpoint:queue:completion:@
readAttributeReactiveCurrentWithClusterStateCache_endpoint_queue_completionSelector :: Selector '[Id MTRClusterStateCacheContainer, Id NSNumber, Id NSObject, Ptr ()] ()
readAttributeReactiveCurrentWithClusterStateCache_endpoint_queue_completionSelector = mkSelector "readAttributeReactiveCurrentWithClusterStateCache:endpoint:queue:completion:"

-- | @Selector@ for @readAttributeApparentCurrentWithCompletion:@
readAttributeApparentCurrentWithCompletionSelector :: Selector '[Ptr ()] ()
readAttributeApparentCurrentWithCompletionSelector = mkSelector "readAttributeApparentCurrentWithCompletion:"

-- | @Selector@ for @subscribeAttributeApparentCurrentWithParams:subscriptionEstablished:reportHandler:@
subscribeAttributeApparentCurrentWithParams_subscriptionEstablished_reportHandlerSelector :: Selector '[Id MTRSubscribeParams, Ptr (), Ptr ()] ()
subscribeAttributeApparentCurrentWithParams_subscriptionEstablished_reportHandlerSelector = mkSelector "subscribeAttributeApparentCurrentWithParams:subscriptionEstablished:reportHandler:"

-- | @Selector@ for @readAttributeApparentCurrentWithClusterStateCache:endpoint:queue:completion:@
readAttributeApparentCurrentWithClusterStateCache_endpoint_queue_completionSelector :: Selector '[Id MTRClusterStateCacheContainer, Id NSNumber, Id NSObject, Ptr ()] ()
readAttributeApparentCurrentWithClusterStateCache_endpoint_queue_completionSelector = mkSelector "readAttributeApparentCurrentWithClusterStateCache:endpoint:queue:completion:"

-- | @Selector@ for @readAttributeActivePowerWithCompletion:@
readAttributeActivePowerWithCompletionSelector :: Selector '[Ptr ()] ()
readAttributeActivePowerWithCompletionSelector = mkSelector "readAttributeActivePowerWithCompletion:"

-- | @Selector@ for @subscribeAttributeActivePowerWithParams:subscriptionEstablished:reportHandler:@
subscribeAttributeActivePowerWithParams_subscriptionEstablished_reportHandlerSelector :: Selector '[Id MTRSubscribeParams, Ptr (), Ptr ()] ()
subscribeAttributeActivePowerWithParams_subscriptionEstablished_reportHandlerSelector = mkSelector "subscribeAttributeActivePowerWithParams:subscriptionEstablished:reportHandler:"

-- | @Selector@ for @readAttributeActivePowerWithClusterStateCache:endpoint:queue:completion:@
readAttributeActivePowerWithClusterStateCache_endpoint_queue_completionSelector :: Selector '[Id MTRClusterStateCacheContainer, Id NSNumber, Id NSObject, Ptr ()] ()
readAttributeActivePowerWithClusterStateCache_endpoint_queue_completionSelector = mkSelector "readAttributeActivePowerWithClusterStateCache:endpoint:queue:completion:"

-- | @Selector@ for @readAttributeReactivePowerWithCompletion:@
readAttributeReactivePowerWithCompletionSelector :: Selector '[Ptr ()] ()
readAttributeReactivePowerWithCompletionSelector = mkSelector "readAttributeReactivePowerWithCompletion:"

-- | @Selector@ for @subscribeAttributeReactivePowerWithParams:subscriptionEstablished:reportHandler:@
subscribeAttributeReactivePowerWithParams_subscriptionEstablished_reportHandlerSelector :: Selector '[Id MTRSubscribeParams, Ptr (), Ptr ()] ()
subscribeAttributeReactivePowerWithParams_subscriptionEstablished_reportHandlerSelector = mkSelector "subscribeAttributeReactivePowerWithParams:subscriptionEstablished:reportHandler:"

-- | @Selector@ for @readAttributeReactivePowerWithClusterStateCache:endpoint:queue:completion:@
readAttributeReactivePowerWithClusterStateCache_endpoint_queue_completionSelector :: Selector '[Id MTRClusterStateCacheContainer, Id NSNumber, Id NSObject, Ptr ()] ()
readAttributeReactivePowerWithClusterStateCache_endpoint_queue_completionSelector = mkSelector "readAttributeReactivePowerWithClusterStateCache:endpoint:queue:completion:"

-- | @Selector@ for @readAttributeApparentPowerWithCompletion:@
readAttributeApparentPowerWithCompletionSelector :: Selector '[Ptr ()] ()
readAttributeApparentPowerWithCompletionSelector = mkSelector "readAttributeApparentPowerWithCompletion:"

-- | @Selector@ for @subscribeAttributeApparentPowerWithParams:subscriptionEstablished:reportHandler:@
subscribeAttributeApparentPowerWithParams_subscriptionEstablished_reportHandlerSelector :: Selector '[Id MTRSubscribeParams, Ptr (), Ptr ()] ()
subscribeAttributeApparentPowerWithParams_subscriptionEstablished_reportHandlerSelector = mkSelector "subscribeAttributeApparentPowerWithParams:subscriptionEstablished:reportHandler:"

-- | @Selector@ for @readAttributeApparentPowerWithClusterStateCache:endpoint:queue:completion:@
readAttributeApparentPowerWithClusterStateCache_endpoint_queue_completionSelector :: Selector '[Id MTRClusterStateCacheContainer, Id NSNumber, Id NSObject, Ptr ()] ()
readAttributeApparentPowerWithClusterStateCache_endpoint_queue_completionSelector = mkSelector "readAttributeApparentPowerWithClusterStateCache:endpoint:queue:completion:"

-- | @Selector@ for @readAttributeRMSVoltageWithCompletion:@
readAttributeRMSVoltageWithCompletionSelector :: Selector '[Ptr ()] ()
readAttributeRMSVoltageWithCompletionSelector = mkSelector "readAttributeRMSVoltageWithCompletion:"

-- | @Selector@ for @subscribeAttributeRMSVoltageWithParams:subscriptionEstablished:reportHandler:@
subscribeAttributeRMSVoltageWithParams_subscriptionEstablished_reportHandlerSelector :: Selector '[Id MTRSubscribeParams, Ptr (), Ptr ()] ()
subscribeAttributeRMSVoltageWithParams_subscriptionEstablished_reportHandlerSelector = mkSelector "subscribeAttributeRMSVoltageWithParams:subscriptionEstablished:reportHandler:"

-- | @Selector@ for @readAttributeRMSVoltageWithClusterStateCache:endpoint:queue:completion:@
readAttributeRMSVoltageWithClusterStateCache_endpoint_queue_completionSelector :: Selector '[Id MTRClusterStateCacheContainer, Id NSNumber, Id NSObject, Ptr ()] ()
readAttributeRMSVoltageWithClusterStateCache_endpoint_queue_completionSelector = mkSelector "readAttributeRMSVoltageWithClusterStateCache:endpoint:queue:completion:"

-- | @Selector@ for @readAttributeRMSCurrentWithCompletion:@
readAttributeRMSCurrentWithCompletionSelector :: Selector '[Ptr ()] ()
readAttributeRMSCurrentWithCompletionSelector = mkSelector "readAttributeRMSCurrentWithCompletion:"

-- | @Selector@ for @subscribeAttributeRMSCurrentWithParams:subscriptionEstablished:reportHandler:@
subscribeAttributeRMSCurrentWithParams_subscriptionEstablished_reportHandlerSelector :: Selector '[Id MTRSubscribeParams, Ptr (), Ptr ()] ()
subscribeAttributeRMSCurrentWithParams_subscriptionEstablished_reportHandlerSelector = mkSelector "subscribeAttributeRMSCurrentWithParams:subscriptionEstablished:reportHandler:"

-- | @Selector@ for @readAttributeRMSCurrentWithClusterStateCache:endpoint:queue:completion:@
readAttributeRMSCurrentWithClusterStateCache_endpoint_queue_completionSelector :: Selector '[Id MTRClusterStateCacheContainer, Id NSNumber, Id NSObject, Ptr ()] ()
readAttributeRMSCurrentWithClusterStateCache_endpoint_queue_completionSelector = mkSelector "readAttributeRMSCurrentWithClusterStateCache:endpoint:queue:completion:"

-- | @Selector@ for @readAttributeRMSPowerWithCompletion:@
readAttributeRMSPowerWithCompletionSelector :: Selector '[Ptr ()] ()
readAttributeRMSPowerWithCompletionSelector = mkSelector "readAttributeRMSPowerWithCompletion:"

-- | @Selector@ for @subscribeAttributeRMSPowerWithParams:subscriptionEstablished:reportHandler:@
subscribeAttributeRMSPowerWithParams_subscriptionEstablished_reportHandlerSelector :: Selector '[Id MTRSubscribeParams, Ptr (), Ptr ()] ()
subscribeAttributeRMSPowerWithParams_subscriptionEstablished_reportHandlerSelector = mkSelector "subscribeAttributeRMSPowerWithParams:subscriptionEstablished:reportHandler:"

-- | @Selector@ for @readAttributeRMSPowerWithClusterStateCache:endpoint:queue:completion:@
readAttributeRMSPowerWithClusterStateCache_endpoint_queue_completionSelector :: Selector '[Id MTRClusterStateCacheContainer, Id NSNumber, Id NSObject, Ptr ()] ()
readAttributeRMSPowerWithClusterStateCache_endpoint_queue_completionSelector = mkSelector "readAttributeRMSPowerWithClusterStateCache:endpoint:queue:completion:"

-- | @Selector@ for @readAttributeFrequencyWithCompletion:@
readAttributeFrequencyWithCompletionSelector :: Selector '[Ptr ()] ()
readAttributeFrequencyWithCompletionSelector = mkSelector "readAttributeFrequencyWithCompletion:"

-- | @Selector@ for @subscribeAttributeFrequencyWithParams:subscriptionEstablished:reportHandler:@
subscribeAttributeFrequencyWithParams_subscriptionEstablished_reportHandlerSelector :: Selector '[Id MTRSubscribeParams, Ptr (), Ptr ()] ()
subscribeAttributeFrequencyWithParams_subscriptionEstablished_reportHandlerSelector = mkSelector "subscribeAttributeFrequencyWithParams:subscriptionEstablished:reportHandler:"

-- | @Selector@ for @readAttributeFrequencyWithClusterStateCache:endpoint:queue:completion:@
readAttributeFrequencyWithClusterStateCache_endpoint_queue_completionSelector :: Selector '[Id MTRClusterStateCacheContainer, Id NSNumber, Id NSObject, Ptr ()] ()
readAttributeFrequencyWithClusterStateCache_endpoint_queue_completionSelector = mkSelector "readAttributeFrequencyWithClusterStateCache:endpoint:queue:completion:"

-- | @Selector@ for @readAttributeHarmonicCurrentsWithCompletion:@
readAttributeHarmonicCurrentsWithCompletionSelector :: Selector '[Ptr ()] ()
readAttributeHarmonicCurrentsWithCompletionSelector = mkSelector "readAttributeHarmonicCurrentsWithCompletion:"

-- | @Selector@ for @subscribeAttributeHarmonicCurrentsWithParams:subscriptionEstablished:reportHandler:@
subscribeAttributeHarmonicCurrentsWithParams_subscriptionEstablished_reportHandlerSelector :: Selector '[Id MTRSubscribeParams, Ptr (), Ptr ()] ()
subscribeAttributeHarmonicCurrentsWithParams_subscriptionEstablished_reportHandlerSelector = mkSelector "subscribeAttributeHarmonicCurrentsWithParams:subscriptionEstablished:reportHandler:"

-- | @Selector@ for @readAttributeHarmonicCurrentsWithClusterStateCache:endpoint:queue:completion:@
readAttributeHarmonicCurrentsWithClusterStateCache_endpoint_queue_completionSelector :: Selector '[Id MTRClusterStateCacheContainer, Id NSNumber, Id NSObject, Ptr ()] ()
readAttributeHarmonicCurrentsWithClusterStateCache_endpoint_queue_completionSelector = mkSelector "readAttributeHarmonicCurrentsWithClusterStateCache:endpoint:queue:completion:"

-- | @Selector@ for @readAttributeHarmonicPhasesWithCompletion:@
readAttributeHarmonicPhasesWithCompletionSelector :: Selector '[Ptr ()] ()
readAttributeHarmonicPhasesWithCompletionSelector = mkSelector "readAttributeHarmonicPhasesWithCompletion:"

-- | @Selector@ for @subscribeAttributeHarmonicPhasesWithParams:subscriptionEstablished:reportHandler:@
subscribeAttributeHarmonicPhasesWithParams_subscriptionEstablished_reportHandlerSelector :: Selector '[Id MTRSubscribeParams, Ptr (), Ptr ()] ()
subscribeAttributeHarmonicPhasesWithParams_subscriptionEstablished_reportHandlerSelector = mkSelector "subscribeAttributeHarmonicPhasesWithParams:subscriptionEstablished:reportHandler:"

-- | @Selector@ for @readAttributeHarmonicPhasesWithClusterStateCache:endpoint:queue:completion:@
readAttributeHarmonicPhasesWithClusterStateCache_endpoint_queue_completionSelector :: Selector '[Id MTRClusterStateCacheContainer, Id NSNumber, Id NSObject, Ptr ()] ()
readAttributeHarmonicPhasesWithClusterStateCache_endpoint_queue_completionSelector = mkSelector "readAttributeHarmonicPhasesWithClusterStateCache:endpoint:queue:completion:"

-- | @Selector@ for @readAttributePowerFactorWithCompletion:@
readAttributePowerFactorWithCompletionSelector :: Selector '[Ptr ()] ()
readAttributePowerFactorWithCompletionSelector = mkSelector "readAttributePowerFactorWithCompletion:"

-- | @Selector@ for @subscribeAttributePowerFactorWithParams:subscriptionEstablished:reportHandler:@
subscribeAttributePowerFactorWithParams_subscriptionEstablished_reportHandlerSelector :: Selector '[Id MTRSubscribeParams, Ptr (), Ptr ()] ()
subscribeAttributePowerFactorWithParams_subscriptionEstablished_reportHandlerSelector = mkSelector "subscribeAttributePowerFactorWithParams:subscriptionEstablished:reportHandler:"

-- | @Selector@ for @readAttributePowerFactorWithClusterStateCache:endpoint:queue:completion:@
readAttributePowerFactorWithClusterStateCache_endpoint_queue_completionSelector :: Selector '[Id MTRClusterStateCacheContainer, Id NSNumber, Id NSObject, Ptr ()] ()
readAttributePowerFactorWithClusterStateCache_endpoint_queue_completionSelector = mkSelector "readAttributePowerFactorWithClusterStateCache:endpoint:queue:completion:"

-- | @Selector@ for @readAttributeNeutralCurrentWithCompletion:@
readAttributeNeutralCurrentWithCompletionSelector :: Selector '[Ptr ()] ()
readAttributeNeutralCurrentWithCompletionSelector = mkSelector "readAttributeNeutralCurrentWithCompletion:"

-- | @Selector@ for @subscribeAttributeNeutralCurrentWithParams:subscriptionEstablished:reportHandler:@
subscribeAttributeNeutralCurrentWithParams_subscriptionEstablished_reportHandlerSelector :: Selector '[Id MTRSubscribeParams, Ptr (), Ptr ()] ()
subscribeAttributeNeutralCurrentWithParams_subscriptionEstablished_reportHandlerSelector = mkSelector "subscribeAttributeNeutralCurrentWithParams:subscriptionEstablished:reportHandler:"

-- | @Selector@ for @readAttributeNeutralCurrentWithClusterStateCache:endpoint:queue:completion:@
readAttributeNeutralCurrentWithClusterStateCache_endpoint_queue_completionSelector :: Selector '[Id MTRClusterStateCacheContainer, Id NSNumber, Id NSObject, Ptr ()] ()
readAttributeNeutralCurrentWithClusterStateCache_endpoint_queue_completionSelector = mkSelector "readAttributeNeutralCurrentWithClusterStateCache:endpoint:queue:completion:"

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
initSelector :: Selector '[] (Id MTRBaseClusterElectricalPowerMeasurement)
initSelector = mkSelector "init"

-- | @Selector@ for @new@
newSelector :: Selector '[] (Id MTRBaseClusterElectricalPowerMeasurement)
newSelector = mkSelector "new"

-- | @Selector@ for @initWithDevice:endpointID:queue:@
initWithDevice_endpointID_queueSelector :: Selector '[Id MTRBaseDevice, Id NSNumber, Id NSObject] (Id MTRBaseClusterElectricalPowerMeasurement)
initWithDevice_endpointID_queueSelector = mkSelector "initWithDevice:endpointID:queue:"

