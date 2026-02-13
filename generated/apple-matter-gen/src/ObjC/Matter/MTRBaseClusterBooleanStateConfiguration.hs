{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Cluster Boolean State Configuration
--
-- This cluster is used to configure a boolean sensor.
--
-- Generated bindings for @MTRBaseClusterBooleanStateConfiguration@.
module ObjC.Matter.MTRBaseClusterBooleanStateConfiguration
  ( MTRBaseClusterBooleanStateConfiguration
  , IsMTRBaseClusterBooleanStateConfiguration(..)
  , suppressAlarmWithParams_completion
  , enableDisableAlarmWithParams_completion
  , readAttributeCurrentSensitivityLevelWithCompletion
  , writeAttributeCurrentSensitivityLevelWithValue_completion
  , writeAttributeCurrentSensitivityLevelWithValue_params_completion
  , subscribeAttributeCurrentSensitivityLevelWithParams_subscriptionEstablished_reportHandler
  , readAttributeCurrentSensitivityLevelWithClusterStateCache_endpoint_queue_completion
  , readAttributeSupportedSensitivityLevelsWithCompletion
  , subscribeAttributeSupportedSensitivityLevelsWithParams_subscriptionEstablished_reportHandler
  , readAttributeSupportedSensitivityLevelsWithClusterStateCache_endpoint_queue_completion
  , readAttributeDefaultSensitivityLevelWithCompletion
  , subscribeAttributeDefaultSensitivityLevelWithParams_subscriptionEstablished_reportHandler
  , readAttributeDefaultSensitivityLevelWithClusterStateCache_endpoint_queue_completion
  , readAttributeAlarmsActiveWithCompletion
  , subscribeAttributeAlarmsActiveWithParams_subscriptionEstablished_reportHandler
  , readAttributeAlarmsActiveWithClusterStateCache_endpoint_queue_completion
  , readAttributeAlarmsSuppressedWithCompletion
  , subscribeAttributeAlarmsSuppressedWithParams_subscriptionEstablished_reportHandler
  , readAttributeAlarmsSuppressedWithClusterStateCache_endpoint_queue_completion
  , readAttributeAlarmsEnabledWithCompletion
  , subscribeAttributeAlarmsEnabledWithParams_subscriptionEstablished_reportHandler
  , readAttributeAlarmsEnabledWithClusterStateCache_endpoint_queue_completion
  , readAttributeAlarmsSupportedWithCompletion
  , subscribeAttributeAlarmsSupportedWithParams_subscriptionEstablished_reportHandler
  , readAttributeAlarmsSupportedWithClusterStateCache_endpoint_queue_completion
  , readAttributeSensorFaultWithCompletion
  , subscribeAttributeSensorFaultWithParams_subscriptionEstablished_reportHandler
  , readAttributeSensorFaultWithClusterStateCache_endpoint_queue_completion
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
  , enableDisableAlarmWithParams_completionSelector
  , initSelector
  , initWithDevice_endpointID_queueSelector
  , newSelector
  , readAttributeAcceptedCommandListWithClusterStateCache_endpoint_queue_completionSelector
  , readAttributeAcceptedCommandListWithCompletionSelector
  , readAttributeAlarmsActiveWithClusterStateCache_endpoint_queue_completionSelector
  , readAttributeAlarmsActiveWithCompletionSelector
  , readAttributeAlarmsEnabledWithClusterStateCache_endpoint_queue_completionSelector
  , readAttributeAlarmsEnabledWithCompletionSelector
  , readAttributeAlarmsSupportedWithClusterStateCache_endpoint_queue_completionSelector
  , readAttributeAlarmsSupportedWithCompletionSelector
  , readAttributeAlarmsSuppressedWithClusterStateCache_endpoint_queue_completionSelector
  , readAttributeAlarmsSuppressedWithCompletionSelector
  , readAttributeAttributeListWithClusterStateCache_endpoint_queue_completionSelector
  , readAttributeAttributeListWithCompletionSelector
  , readAttributeClusterRevisionWithClusterStateCache_endpoint_queue_completionSelector
  , readAttributeClusterRevisionWithCompletionSelector
  , readAttributeCurrentSensitivityLevelWithClusterStateCache_endpoint_queue_completionSelector
  , readAttributeCurrentSensitivityLevelWithCompletionSelector
  , readAttributeDefaultSensitivityLevelWithClusterStateCache_endpoint_queue_completionSelector
  , readAttributeDefaultSensitivityLevelWithCompletionSelector
  , readAttributeFeatureMapWithClusterStateCache_endpoint_queue_completionSelector
  , readAttributeFeatureMapWithCompletionSelector
  , readAttributeGeneratedCommandListWithClusterStateCache_endpoint_queue_completionSelector
  , readAttributeGeneratedCommandListWithCompletionSelector
  , readAttributeSensorFaultWithClusterStateCache_endpoint_queue_completionSelector
  , readAttributeSensorFaultWithCompletionSelector
  , readAttributeSupportedSensitivityLevelsWithClusterStateCache_endpoint_queue_completionSelector
  , readAttributeSupportedSensitivityLevelsWithCompletionSelector
  , subscribeAttributeAcceptedCommandListWithParams_subscriptionEstablished_reportHandlerSelector
  , subscribeAttributeAlarmsActiveWithParams_subscriptionEstablished_reportHandlerSelector
  , subscribeAttributeAlarmsEnabledWithParams_subscriptionEstablished_reportHandlerSelector
  , subscribeAttributeAlarmsSupportedWithParams_subscriptionEstablished_reportHandlerSelector
  , subscribeAttributeAlarmsSuppressedWithParams_subscriptionEstablished_reportHandlerSelector
  , subscribeAttributeAttributeListWithParams_subscriptionEstablished_reportHandlerSelector
  , subscribeAttributeClusterRevisionWithParams_subscriptionEstablished_reportHandlerSelector
  , subscribeAttributeCurrentSensitivityLevelWithParams_subscriptionEstablished_reportHandlerSelector
  , subscribeAttributeDefaultSensitivityLevelWithParams_subscriptionEstablished_reportHandlerSelector
  , subscribeAttributeFeatureMapWithParams_subscriptionEstablished_reportHandlerSelector
  , subscribeAttributeGeneratedCommandListWithParams_subscriptionEstablished_reportHandlerSelector
  , subscribeAttributeSensorFaultWithParams_subscriptionEstablished_reportHandlerSelector
  , subscribeAttributeSupportedSensitivityLevelsWithParams_subscriptionEstablished_reportHandlerSelector
  , suppressAlarmWithParams_completionSelector
  , writeAttributeCurrentSensitivityLevelWithValue_completionSelector
  , writeAttributeCurrentSensitivityLevelWithValue_params_completionSelector


  ) where

import Foreign.Ptr (Ptr, FunPtr)
import Foreign.C.Types

import ObjC.Runtime.Types
import ObjC.Runtime.Message (sendMessage, sendOwnedMessage, sendClassMessage, sendOwnedClassMessage)
import ObjC.Runtime.Selector (mkSelector)
import ObjC.Runtime.Class (getRequiredClass)

import ObjC.Matter.Internal.Classes
import ObjC.Foundation.Internal.Classes

-- | Command SuppressAlarm
--
-- This command is used to suppress the specified alarm mode.
--
-- ObjC selector: @- suppressAlarmWithParams:completion:@
suppressAlarmWithParams_completion :: (IsMTRBaseClusterBooleanStateConfiguration mtrBaseClusterBooleanStateConfiguration, IsMTRBooleanStateConfigurationClusterSuppressAlarmParams params) => mtrBaseClusterBooleanStateConfiguration -> params -> Ptr () -> IO ()
suppressAlarmWithParams_completion mtrBaseClusterBooleanStateConfiguration params completion =
  sendMessage mtrBaseClusterBooleanStateConfiguration suppressAlarmWithParams_completionSelector (toMTRBooleanStateConfigurationClusterSuppressAlarmParams params) completion

-- | Command EnableDisableAlarm
--
-- This command is used to enable or disable the specified alarm mode.
--
-- ObjC selector: @- enableDisableAlarmWithParams:completion:@
enableDisableAlarmWithParams_completion :: (IsMTRBaseClusterBooleanStateConfiguration mtrBaseClusterBooleanStateConfiguration, IsMTRBooleanStateConfigurationClusterEnableDisableAlarmParams params) => mtrBaseClusterBooleanStateConfiguration -> params -> Ptr () -> IO ()
enableDisableAlarmWithParams_completion mtrBaseClusterBooleanStateConfiguration params completion =
  sendMessage mtrBaseClusterBooleanStateConfiguration enableDisableAlarmWithParams_completionSelector (toMTRBooleanStateConfigurationClusterEnableDisableAlarmParams params) completion

-- | @- readAttributeCurrentSensitivityLevelWithCompletion:@
readAttributeCurrentSensitivityLevelWithCompletion :: IsMTRBaseClusterBooleanStateConfiguration mtrBaseClusterBooleanStateConfiguration => mtrBaseClusterBooleanStateConfiguration -> Ptr () -> IO ()
readAttributeCurrentSensitivityLevelWithCompletion mtrBaseClusterBooleanStateConfiguration completion =
  sendMessage mtrBaseClusterBooleanStateConfiguration readAttributeCurrentSensitivityLevelWithCompletionSelector completion

-- | @- writeAttributeCurrentSensitivityLevelWithValue:completion:@
writeAttributeCurrentSensitivityLevelWithValue_completion :: (IsMTRBaseClusterBooleanStateConfiguration mtrBaseClusterBooleanStateConfiguration, IsNSNumber value) => mtrBaseClusterBooleanStateConfiguration -> value -> Ptr () -> IO ()
writeAttributeCurrentSensitivityLevelWithValue_completion mtrBaseClusterBooleanStateConfiguration value completion =
  sendMessage mtrBaseClusterBooleanStateConfiguration writeAttributeCurrentSensitivityLevelWithValue_completionSelector (toNSNumber value) completion

-- | @- writeAttributeCurrentSensitivityLevelWithValue:params:completion:@
writeAttributeCurrentSensitivityLevelWithValue_params_completion :: (IsMTRBaseClusterBooleanStateConfiguration mtrBaseClusterBooleanStateConfiguration, IsNSNumber value, IsMTRWriteParams params) => mtrBaseClusterBooleanStateConfiguration -> value -> params -> Ptr () -> IO ()
writeAttributeCurrentSensitivityLevelWithValue_params_completion mtrBaseClusterBooleanStateConfiguration value params completion =
  sendMessage mtrBaseClusterBooleanStateConfiguration writeAttributeCurrentSensitivityLevelWithValue_params_completionSelector (toNSNumber value) (toMTRWriteParams params) completion

-- | @- subscribeAttributeCurrentSensitivityLevelWithParams:subscriptionEstablished:reportHandler:@
subscribeAttributeCurrentSensitivityLevelWithParams_subscriptionEstablished_reportHandler :: (IsMTRBaseClusterBooleanStateConfiguration mtrBaseClusterBooleanStateConfiguration, IsMTRSubscribeParams params) => mtrBaseClusterBooleanStateConfiguration -> params -> Ptr () -> Ptr () -> IO ()
subscribeAttributeCurrentSensitivityLevelWithParams_subscriptionEstablished_reportHandler mtrBaseClusterBooleanStateConfiguration params subscriptionEstablished reportHandler =
  sendMessage mtrBaseClusterBooleanStateConfiguration subscribeAttributeCurrentSensitivityLevelWithParams_subscriptionEstablished_reportHandlerSelector (toMTRSubscribeParams params) subscriptionEstablished reportHandler

-- | @+ readAttributeCurrentSensitivityLevelWithClusterStateCache:endpoint:queue:completion:@
readAttributeCurrentSensitivityLevelWithClusterStateCache_endpoint_queue_completion :: (IsMTRClusterStateCacheContainer clusterStateCacheContainer, IsNSNumber endpoint, IsNSObject queue) => clusterStateCacheContainer -> endpoint -> queue -> Ptr () -> IO ()
readAttributeCurrentSensitivityLevelWithClusterStateCache_endpoint_queue_completion clusterStateCacheContainer endpoint queue completion =
  do
    cls' <- getRequiredClass "MTRBaseClusterBooleanStateConfiguration"
    sendClassMessage cls' readAttributeCurrentSensitivityLevelWithClusterStateCache_endpoint_queue_completionSelector (toMTRClusterStateCacheContainer clusterStateCacheContainer) (toNSNumber endpoint) (toNSObject queue) completion

-- | @- readAttributeSupportedSensitivityLevelsWithCompletion:@
readAttributeSupportedSensitivityLevelsWithCompletion :: IsMTRBaseClusterBooleanStateConfiguration mtrBaseClusterBooleanStateConfiguration => mtrBaseClusterBooleanStateConfiguration -> Ptr () -> IO ()
readAttributeSupportedSensitivityLevelsWithCompletion mtrBaseClusterBooleanStateConfiguration completion =
  sendMessage mtrBaseClusterBooleanStateConfiguration readAttributeSupportedSensitivityLevelsWithCompletionSelector completion

-- | @- subscribeAttributeSupportedSensitivityLevelsWithParams:subscriptionEstablished:reportHandler:@
subscribeAttributeSupportedSensitivityLevelsWithParams_subscriptionEstablished_reportHandler :: (IsMTRBaseClusterBooleanStateConfiguration mtrBaseClusterBooleanStateConfiguration, IsMTRSubscribeParams params) => mtrBaseClusterBooleanStateConfiguration -> params -> Ptr () -> Ptr () -> IO ()
subscribeAttributeSupportedSensitivityLevelsWithParams_subscriptionEstablished_reportHandler mtrBaseClusterBooleanStateConfiguration params subscriptionEstablished reportHandler =
  sendMessage mtrBaseClusterBooleanStateConfiguration subscribeAttributeSupportedSensitivityLevelsWithParams_subscriptionEstablished_reportHandlerSelector (toMTRSubscribeParams params) subscriptionEstablished reportHandler

-- | @+ readAttributeSupportedSensitivityLevelsWithClusterStateCache:endpoint:queue:completion:@
readAttributeSupportedSensitivityLevelsWithClusterStateCache_endpoint_queue_completion :: (IsMTRClusterStateCacheContainer clusterStateCacheContainer, IsNSNumber endpoint, IsNSObject queue) => clusterStateCacheContainer -> endpoint -> queue -> Ptr () -> IO ()
readAttributeSupportedSensitivityLevelsWithClusterStateCache_endpoint_queue_completion clusterStateCacheContainer endpoint queue completion =
  do
    cls' <- getRequiredClass "MTRBaseClusterBooleanStateConfiguration"
    sendClassMessage cls' readAttributeSupportedSensitivityLevelsWithClusterStateCache_endpoint_queue_completionSelector (toMTRClusterStateCacheContainer clusterStateCacheContainer) (toNSNumber endpoint) (toNSObject queue) completion

-- | @- readAttributeDefaultSensitivityLevelWithCompletion:@
readAttributeDefaultSensitivityLevelWithCompletion :: IsMTRBaseClusterBooleanStateConfiguration mtrBaseClusterBooleanStateConfiguration => mtrBaseClusterBooleanStateConfiguration -> Ptr () -> IO ()
readAttributeDefaultSensitivityLevelWithCompletion mtrBaseClusterBooleanStateConfiguration completion =
  sendMessage mtrBaseClusterBooleanStateConfiguration readAttributeDefaultSensitivityLevelWithCompletionSelector completion

-- | @- subscribeAttributeDefaultSensitivityLevelWithParams:subscriptionEstablished:reportHandler:@
subscribeAttributeDefaultSensitivityLevelWithParams_subscriptionEstablished_reportHandler :: (IsMTRBaseClusterBooleanStateConfiguration mtrBaseClusterBooleanStateConfiguration, IsMTRSubscribeParams params) => mtrBaseClusterBooleanStateConfiguration -> params -> Ptr () -> Ptr () -> IO ()
subscribeAttributeDefaultSensitivityLevelWithParams_subscriptionEstablished_reportHandler mtrBaseClusterBooleanStateConfiguration params subscriptionEstablished reportHandler =
  sendMessage mtrBaseClusterBooleanStateConfiguration subscribeAttributeDefaultSensitivityLevelWithParams_subscriptionEstablished_reportHandlerSelector (toMTRSubscribeParams params) subscriptionEstablished reportHandler

-- | @+ readAttributeDefaultSensitivityLevelWithClusterStateCache:endpoint:queue:completion:@
readAttributeDefaultSensitivityLevelWithClusterStateCache_endpoint_queue_completion :: (IsMTRClusterStateCacheContainer clusterStateCacheContainer, IsNSNumber endpoint, IsNSObject queue) => clusterStateCacheContainer -> endpoint -> queue -> Ptr () -> IO ()
readAttributeDefaultSensitivityLevelWithClusterStateCache_endpoint_queue_completion clusterStateCacheContainer endpoint queue completion =
  do
    cls' <- getRequiredClass "MTRBaseClusterBooleanStateConfiguration"
    sendClassMessage cls' readAttributeDefaultSensitivityLevelWithClusterStateCache_endpoint_queue_completionSelector (toMTRClusterStateCacheContainer clusterStateCacheContainer) (toNSNumber endpoint) (toNSObject queue) completion

-- | @- readAttributeAlarmsActiveWithCompletion:@
readAttributeAlarmsActiveWithCompletion :: IsMTRBaseClusterBooleanStateConfiguration mtrBaseClusterBooleanStateConfiguration => mtrBaseClusterBooleanStateConfiguration -> Ptr () -> IO ()
readAttributeAlarmsActiveWithCompletion mtrBaseClusterBooleanStateConfiguration completion =
  sendMessage mtrBaseClusterBooleanStateConfiguration readAttributeAlarmsActiveWithCompletionSelector completion

-- | @- subscribeAttributeAlarmsActiveWithParams:subscriptionEstablished:reportHandler:@
subscribeAttributeAlarmsActiveWithParams_subscriptionEstablished_reportHandler :: (IsMTRBaseClusterBooleanStateConfiguration mtrBaseClusterBooleanStateConfiguration, IsMTRSubscribeParams params) => mtrBaseClusterBooleanStateConfiguration -> params -> Ptr () -> Ptr () -> IO ()
subscribeAttributeAlarmsActiveWithParams_subscriptionEstablished_reportHandler mtrBaseClusterBooleanStateConfiguration params subscriptionEstablished reportHandler =
  sendMessage mtrBaseClusterBooleanStateConfiguration subscribeAttributeAlarmsActiveWithParams_subscriptionEstablished_reportHandlerSelector (toMTRSubscribeParams params) subscriptionEstablished reportHandler

-- | @+ readAttributeAlarmsActiveWithClusterStateCache:endpoint:queue:completion:@
readAttributeAlarmsActiveWithClusterStateCache_endpoint_queue_completion :: (IsMTRClusterStateCacheContainer clusterStateCacheContainer, IsNSNumber endpoint, IsNSObject queue) => clusterStateCacheContainer -> endpoint -> queue -> Ptr () -> IO ()
readAttributeAlarmsActiveWithClusterStateCache_endpoint_queue_completion clusterStateCacheContainer endpoint queue completion =
  do
    cls' <- getRequiredClass "MTRBaseClusterBooleanStateConfiguration"
    sendClassMessage cls' readAttributeAlarmsActiveWithClusterStateCache_endpoint_queue_completionSelector (toMTRClusterStateCacheContainer clusterStateCacheContainer) (toNSNumber endpoint) (toNSObject queue) completion

-- | @- readAttributeAlarmsSuppressedWithCompletion:@
readAttributeAlarmsSuppressedWithCompletion :: IsMTRBaseClusterBooleanStateConfiguration mtrBaseClusterBooleanStateConfiguration => mtrBaseClusterBooleanStateConfiguration -> Ptr () -> IO ()
readAttributeAlarmsSuppressedWithCompletion mtrBaseClusterBooleanStateConfiguration completion =
  sendMessage mtrBaseClusterBooleanStateConfiguration readAttributeAlarmsSuppressedWithCompletionSelector completion

-- | @- subscribeAttributeAlarmsSuppressedWithParams:subscriptionEstablished:reportHandler:@
subscribeAttributeAlarmsSuppressedWithParams_subscriptionEstablished_reportHandler :: (IsMTRBaseClusterBooleanStateConfiguration mtrBaseClusterBooleanStateConfiguration, IsMTRSubscribeParams params) => mtrBaseClusterBooleanStateConfiguration -> params -> Ptr () -> Ptr () -> IO ()
subscribeAttributeAlarmsSuppressedWithParams_subscriptionEstablished_reportHandler mtrBaseClusterBooleanStateConfiguration params subscriptionEstablished reportHandler =
  sendMessage mtrBaseClusterBooleanStateConfiguration subscribeAttributeAlarmsSuppressedWithParams_subscriptionEstablished_reportHandlerSelector (toMTRSubscribeParams params) subscriptionEstablished reportHandler

-- | @+ readAttributeAlarmsSuppressedWithClusterStateCache:endpoint:queue:completion:@
readAttributeAlarmsSuppressedWithClusterStateCache_endpoint_queue_completion :: (IsMTRClusterStateCacheContainer clusterStateCacheContainer, IsNSNumber endpoint, IsNSObject queue) => clusterStateCacheContainer -> endpoint -> queue -> Ptr () -> IO ()
readAttributeAlarmsSuppressedWithClusterStateCache_endpoint_queue_completion clusterStateCacheContainer endpoint queue completion =
  do
    cls' <- getRequiredClass "MTRBaseClusterBooleanStateConfiguration"
    sendClassMessage cls' readAttributeAlarmsSuppressedWithClusterStateCache_endpoint_queue_completionSelector (toMTRClusterStateCacheContainer clusterStateCacheContainer) (toNSNumber endpoint) (toNSObject queue) completion

-- | @- readAttributeAlarmsEnabledWithCompletion:@
readAttributeAlarmsEnabledWithCompletion :: IsMTRBaseClusterBooleanStateConfiguration mtrBaseClusterBooleanStateConfiguration => mtrBaseClusterBooleanStateConfiguration -> Ptr () -> IO ()
readAttributeAlarmsEnabledWithCompletion mtrBaseClusterBooleanStateConfiguration completion =
  sendMessage mtrBaseClusterBooleanStateConfiguration readAttributeAlarmsEnabledWithCompletionSelector completion

-- | @- subscribeAttributeAlarmsEnabledWithParams:subscriptionEstablished:reportHandler:@
subscribeAttributeAlarmsEnabledWithParams_subscriptionEstablished_reportHandler :: (IsMTRBaseClusterBooleanStateConfiguration mtrBaseClusterBooleanStateConfiguration, IsMTRSubscribeParams params) => mtrBaseClusterBooleanStateConfiguration -> params -> Ptr () -> Ptr () -> IO ()
subscribeAttributeAlarmsEnabledWithParams_subscriptionEstablished_reportHandler mtrBaseClusterBooleanStateConfiguration params subscriptionEstablished reportHandler =
  sendMessage mtrBaseClusterBooleanStateConfiguration subscribeAttributeAlarmsEnabledWithParams_subscriptionEstablished_reportHandlerSelector (toMTRSubscribeParams params) subscriptionEstablished reportHandler

-- | @+ readAttributeAlarmsEnabledWithClusterStateCache:endpoint:queue:completion:@
readAttributeAlarmsEnabledWithClusterStateCache_endpoint_queue_completion :: (IsMTRClusterStateCacheContainer clusterStateCacheContainer, IsNSNumber endpoint, IsNSObject queue) => clusterStateCacheContainer -> endpoint -> queue -> Ptr () -> IO ()
readAttributeAlarmsEnabledWithClusterStateCache_endpoint_queue_completion clusterStateCacheContainer endpoint queue completion =
  do
    cls' <- getRequiredClass "MTRBaseClusterBooleanStateConfiguration"
    sendClassMessage cls' readAttributeAlarmsEnabledWithClusterStateCache_endpoint_queue_completionSelector (toMTRClusterStateCacheContainer clusterStateCacheContainer) (toNSNumber endpoint) (toNSObject queue) completion

-- | @- readAttributeAlarmsSupportedWithCompletion:@
readAttributeAlarmsSupportedWithCompletion :: IsMTRBaseClusterBooleanStateConfiguration mtrBaseClusterBooleanStateConfiguration => mtrBaseClusterBooleanStateConfiguration -> Ptr () -> IO ()
readAttributeAlarmsSupportedWithCompletion mtrBaseClusterBooleanStateConfiguration completion =
  sendMessage mtrBaseClusterBooleanStateConfiguration readAttributeAlarmsSupportedWithCompletionSelector completion

-- | @- subscribeAttributeAlarmsSupportedWithParams:subscriptionEstablished:reportHandler:@
subscribeAttributeAlarmsSupportedWithParams_subscriptionEstablished_reportHandler :: (IsMTRBaseClusterBooleanStateConfiguration mtrBaseClusterBooleanStateConfiguration, IsMTRSubscribeParams params) => mtrBaseClusterBooleanStateConfiguration -> params -> Ptr () -> Ptr () -> IO ()
subscribeAttributeAlarmsSupportedWithParams_subscriptionEstablished_reportHandler mtrBaseClusterBooleanStateConfiguration params subscriptionEstablished reportHandler =
  sendMessage mtrBaseClusterBooleanStateConfiguration subscribeAttributeAlarmsSupportedWithParams_subscriptionEstablished_reportHandlerSelector (toMTRSubscribeParams params) subscriptionEstablished reportHandler

-- | @+ readAttributeAlarmsSupportedWithClusterStateCache:endpoint:queue:completion:@
readAttributeAlarmsSupportedWithClusterStateCache_endpoint_queue_completion :: (IsMTRClusterStateCacheContainer clusterStateCacheContainer, IsNSNumber endpoint, IsNSObject queue) => clusterStateCacheContainer -> endpoint -> queue -> Ptr () -> IO ()
readAttributeAlarmsSupportedWithClusterStateCache_endpoint_queue_completion clusterStateCacheContainer endpoint queue completion =
  do
    cls' <- getRequiredClass "MTRBaseClusterBooleanStateConfiguration"
    sendClassMessage cls' readAttributeAlarmsSupportedWithClusterStateCache_endpoint_queue_completionSelector (toMTRClusterStateCacheContainer clusterStateCacheContainer) (toNSNumber endpoint) (toNSObject queue) completion

-- | @- readAttributeSensorFaultWithCompletion:@
readAttributeSensorFaultWithCompletion :: IsMTRBaseClusterBooleanStateConfiguration mtrBaseClusterBooleanStateConfiguration => mtrBaseClusterBooleanStateConfiguration -> Ptr () -> IO ()
readAttributeSensorFaultWithCompletion mtrBaseClusterBooleanStateConfiguration completion =
  sendMessage mtrBaseClusterBooleanStateConfiguration readAttributeSensorFaultWithCompletionSelector completion

-- | @- subscribeAttributeSensorFaultWithParams:subscriptionEstablished:reportHandler:@
subscribeAttributeSensorFaultWithParams_subscriptionEstablished_reportHandler :: (IsMTRBaseClusterBooleanStateConfiguration mtrBaseClusterBooleanStateConfiguration, IsMTRSubscribeParams params) => mtrBaseClusterBooleanStateConfiguration -> params -> Ptr () -> Ptr () -> IO ()
subscribeAttributeSensorFaultWithParams_subscriptionEstablished_reportHandler mtrBaseClusterBooleanStateConfiguration params subscriptionEstablished reportHandler =
  sendMessage mtrBaseClusterBooleanStateConfiguration subscribeAttributeSensorFaultWithParams_subscriptionEstablished_reportHandlerSelector (toMTRSubscribeParams params) subscriptionEstablished reportHandler

-- | @+ readAttributeSensorFaultWithClusterStateCache:endpoint:queue:completion:@
readAttributeSensorFaultWithClusterStateCache_endpoint_queue_completion :: (IsMTRClusterStateCacheContainer clusterStateCacheContainer, IsNSNumber endpoint, IsNSObject queue) => clusterStateCacheContainer -> endpoint -> queue -> Ptr () -> IO ()
readAttributeSensorFaultWithClusterStateCache_endpoint_queue_completion clusterStateCacheContainer endpoint queue completion =
  do
    cls' <- getRequiredClass "MTRBaseClusterBooleanStateConfiguration"
    sendClassMessage cls' readAttributeSensorFaultWithClusterStateCache_endpoint_queue_completionSelector (toMTRClusterStateCacheContainer clusterStateCacheContainer) (toNSNumber endpoint) (toNSObject queue) completion

-- | @- readAttributeGeneratedCommandListWithCompletion:@
readAttributeGeneratedCommandListWithCompletion :: IsMTRBaseClusterBooleanStateConfiguration mtrBaseClusterBooleanStateConfiguration => mtrBaseClusterBooleanStateConfiguration -> Ptr () -> IO ()
readAttributeGeneratedCommandListWithCompletion mtrBaseClusterBooleanStateConfiguration completion =
  sendMessage mtrBaseClusterBooleanStateConfiguration readAttributeGeneratedCommandListWithCompletionSelector completion

-- | @- subscribeAttributeGeneratedCommandListWithParams:subscriptionEstablished:reportHandler:@
subscribeAttributeGeneratedCommandListWithParams_subscriptionEstablished_reportHandler :: (IsMTRBaseClusterBooleanStateConfiguration mtrBaseClusterBooleanStateConfiguration, IsMTRSubscribeParams params) => mtrBaseClusterBooleanStateConfiguration -> params -> Ptr () -> Ptr () -> IO ()
subscribeAttributeGeneratedCommandListWithParams_subscriptionEstablished_reportHandler mtrBaseClusterBooleanStateConfiguration params subscriptionEstablished reportHandler =
  sendMessage mtrBaseClusterBooleanStateConfiguration subscribeAttributeGeneratedCommandListWithParams_subscriptionEstablished_reportHandlerSelector (toMTRSubscribeParams params) subscriptionEstablished reportHandler

-- | @+ readAttributeGeneratedCommandListWithClusterStateCache:endpoint:queue:completion:@
readAttributeGeneratedCommandListWithClusterStateCache_endpoint_queue_completion :: (IsMTRClusterStateCacheContainer clusterStateCacheContainer, IsNSNumber endpoint, IsNSObject queue) => clusterStateCacheContainer -> endpoint -> queue -> Ptr () -> IO ()
readAttributeGeneratedCommandListWithClusterStateCache_endpoint_queue_completion clusterStateCacheContainer endpoint queue completion =
  do
    cls' <- getRequiredClass "MTRBaseClusterBooleanStateConfiguration"
    sendClassMessage cls' readAttributeGeneratedCommandListWithClusterStateCache_endpoint_queue_completionSelector (toMTRClusterStateCacheContainer clusterStateCacheContainer) (toNSNumber endpoint) (toNSObject queue) completion

-- | @- readAttributeAcceptedCommandListWithCompletion:@
readAttributeAcceptedCommandListWithCompletion :: IsMTRBaseClusterBooleanStateConfiguration mtrBaseClusterBooleanStateConfiguration => mtrBaseClusterBooleanStateConfiguration -> Ptr () -> IO ()
readAttributeAcceptedCommandListWithCompletion mtrBaseClusterBooleanStateConfiguration completion =
  sendMessage mtrBaseClusterBooleanStateConfiguration readAttributeAcceptedCommandListWithCompletionSelector completion

-- | @- subscribeAttributeAcceptedCommandListWithParams:subscriptionEstablished:reportHandler:@
subscribeAttributeAcceptedCommandListWithParams_subscriptionEstablished_reportHandler :: (IsMTRBaseClusterBooleanStateConfiguration mtrBaseClusterBooleanStateConfiguration, IsMTRSubscribeParams params) => mtrBaseClusterBooleanStateConfiguration -> params -> Ptr () -> Ptr () -> IO ()
subscribeAttributeAcceptedCommandListWithParams_subscriptionEstablished_reportHandler mtrBaseClusterBooleanStateConfiguration params subscriptionEstablished reportHandler =
  sendMessage mtrBaseClusterBooleanStateConfiguration subscribeAttributeAcceptedCommandListWithParams_subscriptionEstablished_reportHandlerSelector (toMTRSubscribeParams params) subscriptionEstablished reportHandler

-- | @+ readAttributeAcceptedCommandListWithClusterStateCache:endpoint:queue:completion:@
readAttributeAcceptedCommandListWithClusterStateCache_endpoint_queue_completion :: (IsMTRClusterStateCacheContainer clusterStateCacheContainer, IsNSNumber endpoint, IsNSObject queue) => clusterStateCacheContainer -> endpoint -> queue -> Ptr () -> IO ()
readAttributeAcceptedCommandListWithClusterStateCache_endpoint_queue_completion clusterStateCacheContainer endpoint queue completion =
  do
    cls' <- getRequiredClass "MTRBaseClusterBooleanStateConfiguration"
    sendClassMessage cls' readAttributeAcceptedCommandListWithClusterStateCache_endpoint_queue_completionSelector (toMTRClusterStateCacheContainer clusterStateCacheContainer) (toNSNumber endpoint) (toNSObject queue) completion

-- | @- readAttributeAttributeListWithCompletion:@
readAttributeAttributeListWithCompletion :: IsMTRBaseClusterBooleanStateConfiguration mtrBaseClusterBooleanStateConfiguration => mtrBaseClusterBooleanStateConfiguration -> Ptr () -> IO ()
readAttributeAttributeListWithCompletion mtrBaseClusterBooleanStateConfiguration completion =
  sendMessage mtrBaseClusterBooleanStateConfiguration readAttributeAttributeListWithCompletionSelector completion

-- | @- subscribeAttributeAttributeListWithParams:subscriptionEstablished:reportHandler:@
subscribeAttributeAttributeListWithParams_subscriptionEstablished_reportHandler :: (IsMTRBaseClusterBooleanStateConfiguration mtrBaseClusterBooleanStateConfiguration, IsMTRSubscribeParams params) => mtrBaseClusterBooleanStateConfiguration -> params -> Ptr () -> Ptr () -> IO ()
subscribeAttributeAttributeListWithParams_subscriptionEstablished_reportHandler mtrBaseClusterBooleanStateConfiguration params subscriptionEstablished reportHandler =
  sendMessage mtrBaseClusterBooleanStateConfiguration subscribeAttributeAttributeListWithParams_subscriptionEstablished_reportHandlerSelector (toMTRSubscribeParams params) subscriptionEstablished reportHandler

-- | @+ readAttributeAttributeListWithClusterStateCache:endpoint:queue:completion:@
readAttributeAttributeListWithClusterStateCache_endpoint_queue_completion :: (IsMTRClusterStateCacheContainer clusterStateCacheContainer, IsNSNumber endpoint, IsNSObject queue) => clusterStateCacheContainer -> endpoint -> queue -> Ptr () -> IO ()
readAttributeAttributeListWithClusterStateCache_endpoint_queue_completion clusterStateCacheContainer endpoint queue completion =
  do
    cls' <- getRequiredClass "MTRBaseClusterBooleanStateConfiguration"
    sendClassMessage cls' readAttributeAttributeListWithClusterStateCache_endpoint_queue_completionSelector (toMTRClusterStateCacheContainer clusterStateCacheContainer) (toNSNumber endpoint) (toNSObject queue) completion

-- | @- readAttributeFeatureMapWithCompletion:@
readAttributeFeatureMapWithCompletion :: IsMTRBaseClusterBooleanStateConfiguration mtrBaseClusterBooleanStateConfiguration => mtrBaseClusterBooleanStateConfiguration -> Ptr () -> IO ()
readAttributeFeatureMapWithCompletion mtrBaseClusterBooleanStateConfiguration completion =
  sendMessage mtrBaseClusterBooleanStateConfiguration readAttributeFeatureMapWithCompletionSelector completion

-- | @- subscribeAttributeFeatureMapWithParams:subscriptionEstablished:reportHandler:@
subscribeAttributeFeatureMapWithParams_subscriptionEstablished_reportHandler :: (IsMTRBaseClusterBooleanStateConfiguration mtrBaseClusterBooleanStateConfiguration, IsMTRSubscribeParams params) => mtrBaseClusterBooleanStateConfiguration -> params -> Ptr () -> Ptr () -> IO ()
subscribeAttributeFeatureMapWithParams_subscriptionEstablished_reportHandler mtrBaseClusterBooleanStateConfiguration params subscriptionEstablished reportHandler =
  sendMessage mtrBaseClusterBooleanStateConfiguration subscribeAttributeFeatureMapWithParams_subscriptionEstablished_reportHandlerSelector (toMTRSubscribeParams params) subscriptionEstablished reportHandler

-- | @+ readAttributeFeatureMapWithClusterStateCache:endpoint:queue:completion:@
readAttributeFeatureMapWithClusterStateCache_endpoint_queue_completion :: (IsMTRClusterStateCacheContainer clusterStateCacheContainer, IsNSNumber endpoint, IsNSObject queue) => clusterStateCacheContainer -> endpoint -> queue -> Ptr () -> IO ()
readAttributeFeatureMapWithClusterStateCache_endpoint_queue_completion clusterStateCacheContainer endpoint queue completion =
  do
    cls' <- getRequiredClass "MTRBaseClusterBooleanStateConfiguration"
    sendClassMessage cls' readAttributeFeatureMapWithClusterStateCache_endpoint_queue_completionSelector (toMTRClusterStateCacheContainer clusterStateCacheContainer) (toNSNumber endpoint) (toNSObject queue) completion

-- | @- readAttributeClusterRevisionWithCompletion:@
readAttributeClusterRevisionWithCompletion :: IsMTRBaseClusterBooleanStateConfiguration mtrBaseClusterBooleanStateConfiguration => mtrBaseClusterBooleanStateConfiguration -> Ptr () -> IO ()
readAttributeClusterRevisionWithCompletion mtrBaseClusterBooleanStateConfiguration completion =
  sendMessage mtrBaseClusterBooleanStateConfiguration readAttributeClusterRevisionWithCompletionSelector completion

-- | @- subscribeAttributeClusterRevisionWithParams:subscriptionEstablished:reportHandler:@
subscribeAttributeClusterRevisionWithParams_subscriptionEstablished_reportHandler :: (IsMTRBaseClusterBooleanStateConfiguration mtrBaseClusterBooleanStateConfiguration, IsMTRSubscribeParams params) => mtrBaseClusterBooleanStateConfiguration -> params -> Ptr () -> Ptr () -> IO ()
subscribeAttributeClusterRevisionWithParams_subscriptionEstablished_reportHandler mtrBaseClusterBooleanStateConfiguration params subscriptionEstablished reportHandler =
  sendMessage mtrBaseClusterBooleanStateConfiguration subscribeAttributeClusterRevisionWithParams_subscriptionEstablished_reportHandlerSelector (toMTRSubscribeParams params) subscriptionEstablished reportHandler

-- | @+ readAttributeClusterRevisionWithClusterStateCache:endpoint:queue:completion:@
readAttributeClusterRevisionWithClusterStateCache_endpoint_queue_completion :: (IsMTRClusterStateCacheContainer clusterStateCacheContainer, IsNSNumber endpoint, IsNSObject queue) => clusterStateCacheContainer -> endpoint -> queue -> Ptr () -> IO ()
readAttributeClusterRevisionWithClusterStateCache_endpoint_queue_completion clusterStateCacheContainer endpoint queue completion =
  do
    cls' <- getRequiredClass "MTRBaseClusterBooleanStateConfiguration"
    sendClassMessage cls' readAttributeClusterRevisionWithClusterStateCache_endpoint_queue_completionSelector (toMTRClusterStateCacheContainer clusterStateCacheContainer) (toNSNumber endpoint) (toNSObject queue) completion

-- | @- init@
init_ :: IsMTRBaseClusterBooleanStateConfiguration mtrBaseClusterBooleanStateConfiguration => mtrBaseClusterBooleanStateConfiguration -> IO (Id MTRBaseClusterBooleanStateConfiguration)
init_ mtrBaseClusterBooleanStateConfiguration =
  sendOwnedMessage mtrBaseClusterBooleanStateConfiguration initSelector

-- | @+ new@
new :: IO (Id MTRBaseClusterBooleanStateConfiguration)
new  =
  do
    cls' <- getRequiredClass "MTRBaseClusterBooleanStateConfiguration"
    sendOwnedClassMessage cls' newSelector

-- | For all instance methods (reads, writes, commands) that take a completion, the completion will be called on the provided queue.
--
-- ObjC selector: @- initWithDevice:endpointID:queue:@
initWithDevice_endpointID_queue :: (IsMTRBaseClusterBooleanStateConfiguration mtrBaseClusterBooleanStateConfiguration, IsMTRBaseDevice device, IsNSNumber endpointID, IsNSObject queue) => mtrBaseClusterBooleanStateConfiguration -> device -> endpointID -> queue -> IO (Id MTRBaseClusterBooleanStateConfiguration)
initWithDevice_endpointID_queue mtrBaseClusterBooleanStateConfiguration device endpointID queue =
  sendOwnedMessage mtrBaseClusterBooleanStateConfiguration initWithDevice_endpointID_queueSelector (toMTRBaseDevice device) (toNSNumber endpointID) (toNSObject queue)

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @suppressAlarmWithParams:completion:@
suppressAlarmWithParams_completionSelector :: Selector '[Id MTRBooleanStateConfigurationClusterSuppressAlarmParams, Ptr ()] ()
suppressAlarmWithParams_completionSelector = mkSelector "suppressAlarmWithParams:completion:"

-- | @Selector@ for @enableDisableAlarmWithParams:completion:@
enableDisableAlarmWithParams_completionSelector :: Selector '[Id MTRBooleanStateConfigurationClusterEnableDisableAlarmParams, Ptr ()] ()
enableDisableAlarmWithParams_completionSelector = mkSelector "enableDisableAlarmWithParams:completion:"

-- | @Selector@ for @readAttributeCurrentSensitivityLevelWithCompletion:@
readAttributeCurrentSensitivityLevelWithCompletionSelector :: Selector '[Ptr ()] ()
readAttributeCurrentSensitivityLevelWithCompletionSelector = mkSelector "readAttributeCurrentSensitivityLevelWithCompletion:"

-- | @Selector@ for @writeAttributeCurrentSensitivityLevelWithValue:completion:@
writeAttributeCurrentSensitivityLevelWithValue_completionSelector :: Selector '[Id NSNumber, Ptr ()] ()
writeAttributeCurrentSensitivityLevelWithValue_completionSelector = mkSelector "writeAttributeCurrentSensitivityLevelWithValue:completion:"

-- | @Selector@ for @writeAttributeCurrentSensitivityLevelWithValue:params:completion:@
writeAttributeCurrentSensitivityLevelWithValue_params_completionSelector :: Selector '[Id NSNumber, Id MTRWriteParams, Ptr ()] ()
writeAttributeCurrentSensitivityLevelWithValue_params_completionSelector = mkSelector "writeAttributeCurrentSensitivityLevelWithValue:params:completion:"

-- | @Selector@ for @subscribeAttributeCurrentSensitivityLevelWithParams:subscriptionEstablished:reportHandler:@
subscribeAttributeCurrentSensitivityLevelWithParams_subscriptionEstablished_reportHandlerSelector :: Selector '[Id MTRSubscribeParams, Ptr (), Ptr ()] ()
subscribeAttributeCurrentSensitivityLevelWithParams_subscriptionEstablished_reportHandlerSelector = mkSelector "subscribeAttributeCurrentSensitivityLevelWithParams:subscriptionEstablished:reportHandler:"

-- | @Selector@ for @readAttributeCurrentSensitivityLevelWithClusterStateCache:endpoint:queue:completion:@
readAttributeCurrentSensitivityLevelWithClusterStateCache_endpoint_queue_completionSelector :: Selector '[Id MTRClusterStateCacheContainer, Id NSNumber, Id NSObject, Ptr ()] ()
readAttributeCurrentSensitivityLevelWithClusterStateCache_endpoint_queue_completionSelector = mkSelector "readAttributeCurrentSensitivityLevelWithClusterStateCache:endpoint:queue:completion:"

-- | @Selector@ for @readAttributeSupportedSensitivityLevelsWithCompletion:@
readAttributeSupportedSensitivityLevelsWithCompletionSelector :: Selector '[Ptr ()] ()
readAttributeSupportedSensitivityLevelsWithCompletionSelector = mkSelector "readAttributeSupportedSensitivityLevelsWithCompletion:"

-- | @Selector@ for @subscribeAttributeSupportedSensitivityLevelsWithParams:subscriptionEstablished:reportHandler:@
subscribeAttributeSupportedSensitivityLevelsWithParams_subscriptionEstablished_reportHandlerSelector :: Selector '[Id MTRSubscribeParams, Ptr (), Ptr ()] ()
subscribeAttributeSupportedSensitivityLevelsWithParams_subscriptionEstablished_reportHandlerSelector = mkSelector "subscribeAttributeSupportedSensitivityLevelsWithParams:subscriptionEstablished:reportHandler:"

-- | @Selector@ for @readAttributeSupportedSensitivityLevelsWithClusterStateCache:endpoint:queue:completion:@
readAttributeSupportedSensitivityLevelsWithClusterStateCache_endpoint_queue_completionSelector :: Selector '[Id MTRClusterStateCacheContainer, Id NSNumber, Id NSObject, Ptr ()] ()
readAttributeSupportedSensitivityLevelsWithClusterStateCache_endpoint_queue_completionSelector = mkSelector "readAttributeSupportedSensitivityLevelsWithClusterStateCache:endpoint:queue:completion:"

-- | @Selector@ for @readAttributeDefaultSensitivityLevelWithCompletion:@
readAttributeDefaultSensitivityLevelWithCompletionSelector :: Selector '[Ptr ()] ()
readAttributeDefaultSensitivityLevelWithCompletionSelector = mkSelector "readAttributeDefaultSensitivityLevelWithCompletion:"

-- | @Selector@ for @subscribeAttributeDefaultSensitivityLevelWithParams:subscriptionEstablished:reportHandler:@
subscribeAttributeDefaultSensitivityLevelWithParams_subscriptionEstablished_reportHandlerSelector :: Selector '[Id MTRSubscribeParams, Ptr (), Ptr ()] ()
subscribeAttributeDefaultSensitivityLevelWithParams_subscriptionEstablished_reportHandlerSelector = mkSelector "subscribeAttributeDefaultSensitivityLevelWithParams:subscriptionEstablished:reportHandler:"

-- | @Selector@ for @readAttributeDefaultSensitivityLevelWithClusterStateCache:endpoint:queue:completion:@
readAttributeDefaultSensitivityLevelWithClusterStateCache_endpoint_queue_completionSelector :: Selector '[Id MTRClusterStateCacheContainer, Id NSNumber, Id NSObject, Ptr ()] ()
readAttributeDefaultSensitivityLevelWithClusterStateCache_endpoint_queue_completionSelector = mkSelector "readAttributeDefaultSensitivityLevelWithClusterStateCache:endpoint:queue:completion:"

-- | @Selector@ for @readAttributeAlarmsActiveWithCompletion:@
readAttributeAlarmsActiveWithCompletionSelector :: Selector '[Ptr ()] ()
readAttributeAlarmsActiveWithCompletionSelector = mkSelector "readAttributeAlarmsActiveWithCompletion:"

-- | @Selector@ for @subscribeAttributeAlarmsActiveWithParams:subscriptionEstablished:reportHandler:@
subscribeAttributeAlarmsActiveWithParams_subscriptionEstablished_reportHandlerSelector :: Selector '[Id MTRSubscribeParams, Ptr (), Ptr ()] ()
subscribeAttributeAlarmsActiveWithParams_subscriptionEstablished_reportHandlerSelector = mkSelector "subscribeAttributeAlarmsActiveWithParams:subscriptionEstablished:reportHandler:"

-- | @Selector@ for @readAttributeAlarmsActiveWithClusterStateCache:endpoint:queue:completion:@
readAttributeAlarmsActiveWithClusterStateCache_endpoint_queue_completionSelector :: Selector '[Id MTRClusterStateCacheContainer, Id NSNumber, Id NSObject, Ptr ()] ()
readAttributeAlarmsActiveWithClusterStateCache_endpoint_queue_completionSelector = mkSelector "readAttributeAlarmsActiveWithClusterStateCache:endpoint:queue:completion:"

-- | @Selector@ for @readAttributeAlarmsSuppressedWithCompletion:@
readAttributeAlarmsSuppressedWithCompletionSelector :: Selector '[Ptr ()] ()
readAttributeAlarmsSuppressedWithCompletionSelector = mkSelector "readAttributeAlarmsSuppressedWithCompletion:"

-- | @Selector@ for @subscribeAttributeAlarmsSuppressedWithParams:subscriptionEstablished:reportHandler:@
subscribeAttributeAlarmsSuppressedWithParams_subscriptionEstablished_reportHandlerSelector :: Selector '[Id MTRSubscribeParams, Ptr (), Ptr ()] ()
subscribeAttributeAlarmsSuppressedWithParams_subscriptionEstablished_reportHandlerSelector = mkSelector "subscribeAttributeAlarmsSuppressedWithParams:subscriptionEstablished:reportHandler:"

-- | @Selector@ for @readAttributeAlarmsSuppressedWithClusterStateCache:endpoint:queue:completion:@
readAttributeAlarmsSuppressedWithClusterStateCache_endpoint_queue_completionSelector :: Selector '[Id MTRClusterStateCacheContainer, Id NSNumber, Id NSObject, Ptr ()] ()
readAttributeAlarmsSuppressedWithClusterStateCache_endpoint_queue_completionSelector = mkSelector "readAttributeAlarmsSuppressedWithClusterStateCache:endpoint:queue:completion:"

-- | @Selector@ for @readAttributeAlarmsEnabledWithCompletion:@
readAttributeAlarmsEnabledWithCompletionSelector :: Selector '[Ptr ()] ()
readAttributeAlarmsEnabledWithCompletionSelector = mkSelector "readAttributeAlarmsEnabledWithCompletion:"

-- | @Selector@ for @subscribeAttributeAlarmsEnabledWithParams:subscriptionEstablished:reportHandler:@
subscribeAttributeAlarmsEnabledWithParams_subscriptionEstablished_reportHandlerSelector :: Selector '[Id MTRSubscribeParams, Ptr (), Ptr ()] ()
subscribeAttributeAlarmsEnabledWithParams_subscriptionEstablished_reportHandlerSelector = mkSelector "subscribeAttributeAlarmsEnabledWithParams:subscriptionEstablished:reportHandler:"

-- | @Selector@ for @readAttributeAlarmsEnabledWithClusterStateCache:endpoint:queue:completion:@
readAttributeAlarmsEnabledWithClusterStateCache_endpoint_queue_completionSelector :: Selector '[Id MTRClusterStateCacheContainer, Id NSNumber, Id NSObject, Ptr ()] ()
readAttributeAlarmsEnabledWithClusterStateCache_endpoint_queue_completionSelector = mkSelector "readAttributeAlarmsEnabledWithClusterStateCache:endpoint:queue:completion:"

-- | @Selector@ for @readAttributeAlarmsSupportedWithCompletion:@
readAttributeAlarmsSupportedWithCompletionSelector :: Selector '[Ptr ()] ()
readAttributeAlarmsSupportedWithCompletionSelector = mkSelector "readAttributeAlarmsSupportedWithCompletion:"

-- | @Selector@ for @subscribeAttributeAlarmsSupportedWithParams:subscriptionEstablished:reportHandler:@
subscribeAttributeAlarmsSupportedWithParams_subscriptionEstablished_reportHandlerSelector :: Selector '[Id MTRSubscribeParams, Ptr (), Ptr ()] ()
subscribeAttributeAlarmsSupportedWithParams_subscriptionEstablished_reportHandlerSelector = mkSelector "subscribeAttributeAlarmsSupportedWithParams:subscriptionEstablished:reportHandler:"

-- | @Selector@ for @readAttributeAlarmsSupportedWithClusterStateCache:endpoint:queue:completion:@
readAttributeAlarmsSupportedWithClusterStateCache_endpoint_queue_completionSelector :: Selector '[Id MTRClusterStateCacheContainer, Id NSNumber, Id NSObject, Ptr ()] ()
readAttributeAlarmsSupportedWithClusterStateCache_endpoint_queue_completionSelector = mkSelector "readAttributeAlarmsSupportedWithClusterStateCache:endpoint:queue:completion:"

-- | @Selector@ for @readAttributeSensorFaultWithCompletion:@
readAttributeSensorFaultWithCompletionSelector :: Selector '[Ptr ()] ()
readAttributeSensorFaultWithCompletionSelector = mkSelector "readAttributeSensorFaultWithCompletion:"

-- | @Selector@ for @subscribeAttributeSensorFaultWithParams:subscriptionEstablished:reportHandler:@
subscribeAttributeSensorFaultWithParams_subscriptionEstablished_reportHandlerSelector :: Selector '[Id MTRSubscribeParams, Ptr (), Ptr ()] ()
subscribeAttributeSensorFaultWithParams_subscriptionEstablished_reportHandlerSelector = mkSelector "subscribeAttributeSensorFaultWithParams:subscriptionEstablished:reportHandler:"

-- | @Selector@ for @readAttributeSensorFaultWithClusterStateCache:endpoint:queue:completion:@
readAttributeSensorFaultWithClusterStateCache_endpoint_queue_completionSelector :: Selector '[Id MTRClusterStateCacheContainer, Id NSNumber, Id NSObject, Ptr ()] ()
readAttributeSensorFaultWithClusterStateCache_endpoint_queue_completionSelector = mkSelector "readAttributeSensorFaultWithClusterStateCache:endpoint:queue:completion:"

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
initSelector :: Selector '[] (Id MTRBaseClusterBooleanStateConfiguration)
initSelector = mkSelector "init"

-- | @Selector@ for @new@
newSelector :: Selector '[] (Id MTRBaseClusterBooleanStateConfiguration)
newSelector = mkSelector "new"

-- | @Selector@ for @initWithDevice:endpointID:queue:@
initWithDevice_endpointID_queueSelector :: Selector '[Id MTRBaseDevice, Id NSNumber, Id NSObject] (Id MTRBaseClusterBooleanStateConfiguration)
initWithDevice_endpointID_queueSelector = mkSelector "initWithDevice:endpointID:queue:"

