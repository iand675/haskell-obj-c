{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Cluster Smoke CO Alarm
--
-- This cluster provides an interface for observing and managing the state of smoke and CO alarms.
--
-- Generated bindings for @MTRBaseClusterSmokeCOAlarm@.
module ObjC.Matter.MTRBaseClusterSmokeCOAlarm
  ( MTRBaseClusterSmokeCOAlarm
  , IsMTRBaseClusterSmokeCOAlarm(..)
  , selfTestRequestWithParams_completion
  , selfTestRequestWithCompletion
  , readAttributeExpressedStateWithCompletion
  , subscribeAttributeExpressedStateWithParams_subscriptionEstablished_reportHandler
  , readAttributeExpressedStateWithClusterStateCache_endpoint_queue_completion
  , readAttributeSmokeStateWithCompletion
  , subscribeAttributeSmokeStateWithParams_subscriptionEstablished_reportHandler
  , readAttributeSmokeStateWithClusterStateCache_endpoint_queue_completion
  , readAttributeCOStateWithCompletion
  , subscribeAttributeCOStateWithParams_subscriptionEstablished_reportHandler
  , readAttributeCOStateWithClusterStateCache_endpoint_queue_completion
  , readAttributeBatteryAlertWithCompletion
  , subscribeAttributeBatteryAlertWithParams_subscriptionEstablished_reportHandler
  , readAttributeBatteryAlertWithClusterStateCache_endpoint_queue_completion
  , readAttributeDeviceMutedWithCompletion
  , subscribeAttributeDeviceMutedWithParams_subscriptionEstablished_reportHandler
  , readAttributeDeviceMutedWithClusterStateCache_endpoint_queue_completion
  , readAttributeTestInProgressWithCompletion
  , subscribeAttributeTestInProgressWithParams_subscriptionEstablished_reportHandler
  , readAttributeTestInProgressWithClusterStateCache_endpoint_queue_completion
  , readAttributeHardwareFaultAlertWithCompletion
  , subscribeAttributeHardwareFaultAlertWithParams_subscriptionEstablished_reportHandler
  , readAttributeHardwareFaultAlertWithClusterStateCache_endpoint_queue_completion
  , readAttributeEndOfServiceAlertWithCompletion
  , subscribeAttributeEndOfServiceAlertWithParams_subscriptionEstablished_reportHandler
  , readAttributeEndOfServiceAlertWithClusterStateCache_endpoint_queue_completion
  , readAttributeInterconnectSmokeAlarmWithCompletion
  , subscribeAttributeInterconnectSmokeAlarmWithParams_subscriptionEstablished_reportHandler
  , readAttributeInterconnectSmokeAlarmWithClusterStateCache_endpoint_queue_completion
  , readAttributeInterconnectCOAlarmWithCompletion
  , subscribeAttributeInterconnectCOAlarmWithParams_subscriptionEstablished_reportHandler
  , readAttributeInterconnectCOAlarmWithClusterStateCache_endpoint_queue_completion
  , readAttributeContaminationStateWithCompletion
  , subscribeAttributeContaminationStateWithParams_subscriptionEstablished_reportHandler
  , readAttributeContaminationStateWithClusterStateCache_endpoint_queue_completion
  , readAttributeSmokeSensitivityLevelWithCompletion
  , writeAttributeSmokeSensitivityLevelWithValue_completion
  , writeAttributeSmokeSensitivityLevelWithValue_params_completion
  , subscribeAttributeSmokeSensitivityLevelWithParams_subscriptionEstablished_reportHandler
  , readAttributeSmokeSensitivityLevelWithClusterStateCache_endpoint_queue_completion
  , readAttributeExpiryDateWithCompletion
  , subscribeAttributeExpiryDateWithParams_subscriptionEstablished_reportHandler
  , readAttributeExpiryDateWithClusterStateCache_endpoint_queue_completion
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
  , readAttributeBatteryAlertWithClusterStateCache_endpoint_queue_completionSelector
  , readAttributeBatteryAlertWithCompletionSelector
  , readAttributeCOStateWithClusterStateCache_endpoint_queue_completionSelector
  , readAttributeCOStateWithCompletionSelector
  , readAttributeClusterRevisionWithClusterStateCache_endpoint_queue_completionSelector
  , readAttributeClusterRevisionWithCompletionSelector
  , readAttributeContaminationStateWithClusterStateCache_endpoint_queue_completionSelector
  , readAttributeContaminationStateWithCompletionSelector
  , readAttributeDeviceMutedWithClusterStateCache_endpoint_queue_completionSelector
  , readAttributeDeviceMutedWithCompletionSelector
  , readAttributeEndOfServiceAlertWithClusterStateCache_endpoint_queue_completionSelector
  , readAttributeEndOfServiceAlertWithCompletionSelector
  , readAttributeExpiryDateWithClusterStateCache_endpoint_queue_completionSelector
  , readAttributeExpiryDateWithCompletionSelector
  , readAttributeExpressedStateWithClusterStateCache_endpoint_queue_completionSelector
  , readAttributeExpressedStateWithCompletionSelector
  , readAttributeFeatureMapWithClusterStateCache_endpoint_queue_completionSelector
  , readAttributeFeatureMapWithCompletionSelector
  , readAttributeGeneratedCommandListWithClusterStateCache_endpoint_queue_completionSelector
  , readAttributeGeneratedCommandListWithCompletionSelector
  , readAttributeHardwareFaultAlertWithClusterStateCache_endpoint_queue_completionSelector
  , readAttributeHardwareFaultAlertWithCompletionSelector
  , readAttributeInterconnectCOAlarmWithClusterStateCache_endpoint_queue_completionSelector
  , readAttributeInterconnectCOAlarmWithCompletionSelector
  , readAttributeInterconnectSmokeAlarmWithClusterStateCache_endpoint_queue_completionSelector
  , readAttributeInterconnectSmokeAlarmWithCompletionSelector
  , readAttributeSmokeSensitivityLevelWithClusterStateCache_endpoint_queue_completionSelector
  , readAttributeSmokeSensitivityLevelWithCompletionSelector
  , readAttributeSmokeStateWithClusterStateCache_endpoint_queue_completionSelector
  , readAttributeSmokeStateWithCompletionSelector
  , readAttributeTestInProgressWithClusterStateCache_endpoint_queue_completionSelector
  , readAttributeTestInProgressWithCompletionSelector
  , selfTestRequestWithCompletionSelector
  , selfTestRequestWithParams_completionSelector
  , subscribeAttributeAcceptedCommandListWithParams_subscriptionEstablished_reportHandlerSelector
  , subscribeAttributeAttributeListWithParams_subscriptionEstablished_reportHandlerSelector
  , subscribeAttributeBatteryAlertWithParams_subscriptionEstablished_reportHandlerSelector
  , subscribeAttributeCOStateWithParams_subscriptionEstablished_reportHandlerSelector
  , subscribeAttributeClusterRevisionWithParams_subscriptionEstablished_reportHandlerSelector
  , subscribeAttributeContaminationStateWithParams_subscriptionEstablished_reportHandlerSelector
  , subscribeAttributeDeviceMutedWithParams_subscriptionEstablished_reportHandlerSelector
  , subscribeAttributeEndOfServiceAlertWithParams_subscriptionEstablished_reportHandlerSelector
  , subscribeAttributeExpiryDateWithParams_subscriptionEstablished_reportHandlerSelector
  , subscribeAttributeExpressedStateWithParams_subscriptionEstablished_reportHandlerSelector
  , subscribeAttributeFeatureMapWithParams_subscriptionEstablished_reportHandlerSelector
  , subscribeAttributeGeneratedCommandListWithParams_subscriptionEstablished_reportHandlerSelector
  , subscribeAttributeHardwareFaultAlertWithParams_subscriptionEstablished_reportHandlerSelector
  , subscribeAttributeInterconnectCOAlarmWithParams_subscriptionEstablished_reportHandlerSelector
  , subscribeAttributeInterconnectSmokeAlarmWithParams_subscriptionEstablished_reportHandlerSelector
  , subscribeAttributeSmokeSensitivityLevelWithParams_subscriptionEstablished_reportHandlerSelector
  , subscribeAttributeSmokeStateWithParams_subscriptionEstablished_reportHandlerSelector
  , subscribeAttributeTestInProgressWithParams_subscriptionEstablished_reportHandlerSelector
  , writeAttributeSmokeSensitivityLevelWithValue_completionSelector
  , writeAttributeSmokeSensitivityLevelWithValue_params_completionSelector


  ) where

import Foreign.Ptr (Ptr, FunPtr)
import Foreign.C.Types

import ObjC.Runtime.Types
import ObjC.Runtime.Message (sendMessage, sendOwnedMessage, sendClassMessage, sendOwnedClassMessage)
import ObjC.Runtime.Selector (mkSelector)
import ObjC.Runtime.Class (getRequiredClass)

import ObjC.Matter.Internal.Classes
import ObjC.Foundation.Internal.Classes

-- | Command SelfTestRequest
--
-- This command SHALL initiate a device self-test.
--
-- ObjC selector: @- selfTestRequestWithParams:completion:@
selfTestRequestWithParams_completion :: (IsMTRBaseClusterSmokeCOAlarm mtrBaseClusterSmokeCOAlarm, IsMTRSmokeCOAlarmClusterSelfTestRequestParams params) => mtrBaseClusterSmokeCOAlarm -> params -> Ptr () -> IO ()
selfTestRequestWithParams_completion mtrBaseClusterSmokeCOAlarm params completion =
  sendMessage mtrBaseClusterSmokeCOAlarm selfTestRequestWithParams_completionSelector (toMTRSmokeCOAlarmClusterSelfTestRequestParams params) completion

-- | @- selfTestRequestWithCompletion:@
selfTestRequestWithCompletion :: IsMTRBaseClusterSmokeCOAlarm mtrBaseClusterSmokeCOAlarm => mtrBaseClusterSmokeCOAlarm -> Ptr () -> IO ()
selfTestRequestWithCompletion mtrBaseClusterSmokeCOAlarm completion =
  sendMessage mtrBaseClusterSmokeCOAlarm selfTestRequestWithCompletionSelector completion

-- | @- readAttributeExpressedStateWithCompletion:@
readAttributeExpressedStateWithCompletion :: IsMTRBaseClusterSmokeCOAlarm mtrBaseClusterSmokeCOAlarm => mtrBaseClusterSmokeCOAlarm -> Ptr () -> IO ()
readAttributeExpressedStateWithCompletion mtrBaseClusterSmokeCOAlarm completion =
  sendMessage mtrBaseClusterSmokeCOAlarm readAttributeExpressedStateWithCompletionSelector completion

-- | @- subscribeAttributeExpressedStateWithParams:subscriptionEstablished:reportHandler:@
subscribeAttributeExpressedStateWithParams_subscriptionEstablished_reportHandler :: (IsMTRBaseClusterSmokeCOAlarm mtrBaseClusterSmokeCOAlarm, IsMTRSubscribeParams params) => mtrBaseClusterSmokeCOAlarm -> params -> Ptr () -> Ptr () -> IO ()
subscribeAttributeExpressedStateWithParams_subscriptionEstablished_reportHandler mtrBaseClusterSmokeCOAlarm params subscriptionEstablished reportHandler =
  sendMessage mtrBaseClusterSmokeCOAlarm subscribeAttributeExpressedStateWithParams_subscriptionEstablished_reportHandlerSelector (toMTRSubscribeParams params) subscriptionEstablished reportHandler

-- | @+ readAttributeExpressedStateWithClusterStateCache:endpoint:queue:completion:@
readAttributeExpressedStateWithClusterStateCache_endpoint_queue_completion :: (IsMTRClusterStateCacheContainer clusterStateCacheContainer, IsNSNumber endpoint, IsNSObject queue) => clusterStateCacheContainer -> endpoint -> queue -> Ptr () -> IO ()
readAttributeExpressedStateWithClusterStateCache_endpoint_queue_completion clusterStateCacheContainer endpoint queue completion =
  do
    cls' <- getRequiredClass "MTRBaseClusterSmokeCOAlarm"
    sendClassMessage cls' readAttributeExpressedStateWithClusterStateCache_endpoint_queue_completionSelector (toMTRClusterStateCacheContainer clusterStateCacheContainer) (toNSNumber endpoint) (toNSObject queue) completion

-- | @- readAttributeSmokeStateWithCompletion:@
readAttributeSmokeStateWithCompletion :: IsMTRBaseClusterSmokeCOAlarm mtrBaseClusterSmokeCOAlarm => mtrBaseClusterSmokeCOAlarm -> Ptr () -> IO ()
readAttributeSmokeStateWithCompletion mtrBaseClusterSmokeCOAlarm completion =
  sendMessage mtrBaseClusterSmokeCOAlarm readAttributeSmokeStateWithCompletionSelector completion

-- | @- subscribeAttributeSmokeStateWithParams:subscriptionEstablished:reportHandler:@
subscribeAttributeSmokeStateWithParams_subscriptionEstablished_reportHandler :: (IsMTRBaseClusterSmokeCOAlarm mtrBaseClusterSmokeCOAlarm, IsMTRSubscribeParams params) => mtrBaseClusterSmokeCOAlarm -> params -> Ptr () -> Ptr () -> IO ()
subscribeAttributeSmokeStateWithParams_subscriptionEstablished_reportHandler mtrBaseClusterSmokeCOAlarm params subscriptionEstablished reportHandler =
  sendMessage mtrBaseClusterSmokeCOAlarm subscribeAttributeSmokeStateWithParams_subscriptionEstablished_reportHandlerSelector (toMTRSubscribeParams params) subscriptionEstablished reportHandler

-- | @+ readAttributeSmokeStateWithClusterStateCache:endpoint:queue:completion:@
readAttributeSmokeStateWithClusterStateCache_endpoint_queue_completion :: (IsMTRClusterStateCacheContainer clusterStateCacheContainer, IsNSNumber endpoint, IsNSObject queue) => clusterStateCacheContainer -> endpoint -> queue -> Ptr () -> IO ()
readAttributeSmokeStateWithClusterStateCache_endpoint_queue_completion clusterStateCacheContainer endpoint queue completion =
  do
    cls' <- getRequiredClass "MTRBaseClusterSmokeCOAlarm"
    sendClassMessage cls' readAttributeSmokeStateWithClusterStateCache_endpoint_queue_completionSelector (toMTRClusterStateCacheContainer clusterStateCacheContainer) (toNSNumber endpoint) (toNSObject queue) completion

-- | @- readAttributeCOStateWithCompletion:@
readAttributeCOStateWithCompletion :: IsMTRBaseClusterSmokeCOAlarm mtrBaseClusterSmokeCOAlarm => mtrBaseClusterSmokeCOAlarm -> Ptr () -> IO ()
readAttributeCOStateWithCompletion mtrBaseClusterSmokeCOAlarm completion =
  sendMessage mtrBaseClusterSmokeCOAlarm readAttributeCOStateWithCompletionSelector completion

-- | @- subscribeAttributeCOStateWithParams:subscriptionEstablished:reportHandler:@
subscribeAttributeCOStateWithParams_subscriptionEstablished_reportHandler :: (IsMTRBaseClusterSmokeCOAlarm mtrBaseClusterSmokeCOAlarm, IsMTRSubscribeParams params) => mtrBaseClusterSmokeCOAlarm -> params -> Ptr () -> Ptr () -> IO ()
subscribeAttributeCOStateWithParams_subscriptionEstablished_reportHandler mtrBaseClusterSmokeCOAlarm params subscriptionEstablished reportHandler =
  sendMessage mtrBaseClusterSmokeCOAlarm subscribeAttributeCOStateWithParams_subscriptionEstablished_reportHandlerSelector (toMTRSubscribeParams params) subscriptionEstablished reportHandler

-- | @+ readAttributeCOStateWithClusterStateCache:endpoint:queue:completion:@
readAttributeCOStateWithClusterStateCache_endpoint_queue_completion :: (IsMTRClusterStateCacheContainer clusterStateCacheContainer, IsNSNumber endpoint, IsNSObject queue) => clusterStateCacheContainer -> endpoint -> queue -> Ptr () -> IO ()
readAttributeCOStateWithClusterStateCache_endpoint_queue_completion clusterStateCacheContainer endpoint queue completion =
  do
    cls' <- getRequiredClass "MTRBaseClusterSmokeCOAlarm"
    sendClassMessage cls' readAttributeCOStateWithClusterStateCache_endpoint_queue_completionSelector (toMTRClusterStateCacheContainer clusterStateCacheContainer) (toNSNumber endpoint) (toNSObject queue) completion

-- | @- readAttributeBatteryAlertWithCompletion:@
readAttributeBatteryAlertWithCompletion :: IsMTRBaseClusterSmokeCOAlarm mtrBaseClusterSmokeCOAlarm => mtrBaseClusterSmokeCOAlarm -> Ptr () -> IO ()
readAttributeBatteryAlertWithCompletion mtrBaseClusterSmokeCOAlarm completion =
  sendMessage mtrBaseClusterSmokeCOAlarm readAttributeBatteryAlertWithCompletionSelector completion

-- | @- subscribeAttributeBatteryAlertWithParams:subscriptionEstablished:reportHandler:@
subscribeAttributeBatteryAlertWithParams_subscriptionEstablished_reportHandler :: (IsMTRBaseClusterSmokeCOAlarm mtrBaseClusterSmokeCOAlarm, IsMTRSubscribeParams params) => mtrBaseClusterSmokeCOAlarm -> params -> Ptr () -> Ptr () -> IO ()
subscribeAttributeBatteryAlertWithParams_subscriptionEstablished_reportHandler mtrBaseClusterSmokeCOAlarm params subscriptionEstablished reportHandler =
  sendMessage mtrBaseClusterSmokeCOAlarm subscribeAttributeBatteryAlertWithParams_subscriptionEstablished_reportHandlerSelector (toMTRSubscribeParams params) subscriptionEstablished reportHandler

-- | @+ readAttributeBatteryAlertWithClusterStateCache:endpoint:queue:completion:@
readAttributeBatteryAlertWithClusterStateCache_endpoint_queue_completion :: (IsMTRClusterStateCacheContainer clusterStateCacheContainer, IsNSNumber endpoint, IsNSObject queue) => clusterStateCacheContainer -> endpoint -> queue -> Ptr () -> IO ()
readAttributeBatteryAlertWithClusterStateCache_endpoint_queue_completion clusterStateCacheContainer endpoint queue completion =
  do
    cls' <- getRequiredClass "MTRBaseClusterSmokeCOAlarm"
    sendClassMessage cls' readAttributeBatteryAlertWithClusterStateCache_endpoint_queue_completionSelector (toMTRClusterStateCacheContainer clusterStateCacheContainer) (toNSNumber endpoint) (toNSObject queue) completion

-- | @- readAttributeDeviceMutedWithCompletion:@
readAttributeDeviceMutedWithCompletion :: IsMTRBaseClusterSmokeCOAlarm mtrBaseClusterSmokeCOAlarm => mtrBaseClusterSmokeCOAlarm -> Ptr () -> IO ()
readAttributeDeviceMutedWithCompletion mtrBaseClusterSmokeCOAlarm completion =
  sendMessage mtrBaseClusterSmokeCOAlarm readAttributeDeviceMutedWithCompletionSelector completion

-- | @- subscribeAttributeDeviceMutedWithParams:subscriptionEstablished:reportHandler:@
subscribeAttributeDeviceMutedWithParams_subscriptionEstablished_reportHandler :: (IsMTRBaseClusterSmokeCOAlarm mtrBaseClusterSmokeCOAlarm, IsMTRSubscribeParams params) => mtrBaseClusterSmokeCOAlarm -> params -> Ptr () -> Ptr () -> IO ()
subscribeAttributeDeviceMutedWithParams_subscriptionEstablished_reportHandler mtrBaseClusterSmokeCOAlarm params subscriptionEstablished reportHandler =
  sendMessage mtrBaseClusterSmokeCOAlarm subscribeAttributeDeviceMutedWithParams_subscriptionEstablished_reportHandlerSelector (toMTRSubscribeParams params) subscriptionEstablished reportHandler

-- | @+ readAttributeDeviceMutedWithClusterStateCache:endpoint:queue:completion:@
readAttributeDeviceMutedWithClusterStateCache_endpoint_queue_completion :: (IsMTRClusterStateCacheContainer clusterStateCacheContainer, IsNSNumber endpoint, IsNSObject queue) => clusterStateCacheContainer -> endpoint -> queue -> Ptr () -> IO ()
readAttributeDeviceMutedWithClusterStateCache_endpoint_queue_completion clusterStateCacheContainer endpoint queue completion =
  do
    cls' <- getRequiredClass "MTRBaseClusterSmokeCOAlarm"
    sendClassMessage cls' readAttributeDeviceMutedWithClusterStateCache_endpoint_queue_completionSelector (toMTRClusterStateCacheContainer clusterStateCacheContainer) (toNSNumber endpoint) (toNSObject queue) completion

-- | @- readAttributeTestInProgressWithCompletion:@
readAttributeTestInProgressWithCompletion :: IsMTRBaseClusterSmokeCOAlarm mtrBaseClusterSmokeCOAlarm => mtrBaseClusterSmokeCOAlarm -> Ptr () -> IO ()
readAttributeTestInProgressWithCompletion mtrBaseClusterSmokeCOAlarm completion =
  sendMessage mtrBaseClusterSmokeCOAlarm readAttributeTestInProgressWithCompletionSelector completion

-- | @- subscribeAttributeTestInProgressWithParams:subscriptionEstablished:reportHandler:@
subscribeAttributeTestInProgressWithParams_subscriptionEstablished_reportHandler :: (IsMTRBaseClusterSmokeCOAlarm mtrBaseClusterSmokeCOAlarm, IsMTRSubscribeParams params) => mtrBaseClusterSmokeCOAlarm -> params -> Ptr () -> Ptr () -> IO ()
subscribeAttributeTestInProgressWithParams_subscriptionEstablished_reportHandler mtrBaseClusterSmokeCOAlarm params subscriptionEstablished reportHandler =
  sendMessage mtrBaseClusterSmokeCOAlarm subscribeAttributeTestInProgressWithParams_subscriptionEstablished_reportHandlerSelector (toMTRSubscribeParams params) subscriptionEstablished reportHandler

-- | @+ readAttributeTestInProgressWithClusterStateCache:endpoint:queue:completion:@
readAttributeTestInProgressWithClusterStateCache_endpoint_queue_completion :: (IsMTRClusterStateCacheContainer clusterStateCacheContainer, IsNSNumber endpoint, IsNSObject queue) => clusterStateCacheContainer -> endpoint -> queue -> Ptr () -> IO ()
readAttributeTestInProgressWithClusterStateCache_endpoint_queue_completion clusterStateCacheContainer endpoint queue completion =
  do
    cls' <- getRequiredClass "MTRBaseClusterSmokeCOAlarm"
    sendClassMessage cls' readAttributeTestInProgressWithClusterStateCache_endpoint_queue_completionSelector (toMTRClusterStateCacheContainer clusterStateCacheContainer) (toNSNumber endpoint) (toNSObject queue) completion

-- | @- readAttributeHardwareFaultAlertWithCompletion:@
readAttributeHardwareFaultAlertWithCompletion :: IsMTRBaseClusterSmokeCOAlarm mtrBaseClusterSmokeCOAlarm => mtrBaseClusterSmokeCOAlarm -> Ptr () -> IO ()
readAttributeHardwareFaultAlertWithCompletion mtrBaseClusterSmokeCOAlarm completion =
  sendMessage mtrBaseClusterSmokeCOAlarm readAttributeHardwareFaultAlertWithCompletionSelector completion

-- | @- subscribeAttributeHardwareFaultAlertWithParams:subscriptionEstablished:reportHandler:@
subscribeAttributeHardwareFaultAlertWithParams_subscriptionEstablished_reportHandler :: (IsMTRBaseClusterSmokeCOAlarm mtrBaseClusterSmokeCOAlarm, IsMTRSubscribeParams params) => mtrBaseClusterSmokeCOAlarm -> params -> Ptr () -> Ptr () -> IO ()
subscribeAttributeHardwareFaultAlertWithParams_subscriptionEstablished_reportHandler mtrBaseClusterSmokeCOAlarm params subscriptionEstablished reportHandler =
  sendMessage mtrBaseClusterSmokeCOAlarm subscribeAttributeHardwareFaultAlertWithParams_subscriptionEstablished_reportHandlerSelector (toMTRSubscribeParams params) subscriptionEstablished reportHandler

-- | @+ readAttributeHardwareFaultAlertWithClusterStateCache:endpoint:queue:completion:@
readAttributeHardwareFaultAlertWithClusterStateCache_endpoint_queue_completion :: (IsMTRClusterStateCacheContainer clusterStateCacheContainer, IsNSNumber endpoint, IsNSObject queue) => clusterStateCacheContainer -> endpoint -> queue -> Ptr () -> IO ()
readAttributeHardwareFaultAlertWithClusterStateCache_endpoint_queue_completion clusterStateCacheContainer endpoint queue completion =
  do
    cls' <- getRequiredClass "MTRBaseClusterSmokeCOAlarm"
    sendClassMessage cls' readAttributeHardwareFaultAlertWithClusterStateCache_endpoint_queue_completionSelector (toMTRClusterStateCacheContainer clusterStateCacheContainer) (toNSNumber endpoint) (toNSObject queue) completion

-- | @- readAttributeEndOfServiceAlertWithCompletion:@
readAttributeEndOfServiceAlertWithCompletion :: IsMTRBaseClusterSmokeCOAlarm mtrBaseClusterSmokeCOAlarm => mtrBaseClusterSmokeCOAlarm -> Ptr () -> IO ()
readAttributeEndOfServiceAlertWithCompletion mtrBaseClusterSmokeCOAlarm completion =
  sendMessage mtrBaseClusterSmokeCOAlarm readAttributeEndOfServiceAlertWithCompletionSelector completion

-- | @- subscribeAttributeEndOfServiceAlertWithParams:subscriptionEstablished:reportHandler:@
subscribeAttributeEndOfServiceAlertWithParams_subscriptionEstablished_reportHandler :: (IsMTRBaseClusterSmokeCOAlarm mtrBaseClusterSmokeCOAlarm, IsMTRSubscribeParams params) => mtrBaseClusterSmokeCOAlarm -> params -> Ptr () -> Ptr () -> IO ()
subscribeAttributeEndOfServiceAlertWithParams_subscriptionEstablished_reportHandler mtrBaseClusterSmokeCOAlarm params subscriptionEstablished reportHandler =
  sendMessage mtrBaseClusterSmokeCOAlarm subscribeAttributeEndOfServiceAlertWithParams_subscriptionEstablished_reportHandlerSelector (toMTRSubscribeParams params) subscriptionEstablished reportHandler

-- | @+ readAttributeEndOfServiceAlertWithClusterStateCache:endpoint:queue:completion:@
readAttributeEndOfServiceAlertWithClusterStateCache_endpoint_queue_completion :: (IsMTRClusterStateCacheContainer clusterStateCacheContainer, IsNSNumber endpoint, IsNSObject queue) => clusterStateCacheContainer -> endpoint -> queue -> Ptr () -> IO ()
readAttributeEndOfServiceAlertWithClusterStateCache_endpoint_queue_completion clusterStateCacheContainer endpoint queue completion =
  do
    cls' <- getRequiredClass "MTRBaseClusterSmokeCOAlarm"
    sendClassMessage cls' readAttributeEndOfServiceAlertWithClusterStateCache_endpoint_queue_completionSelector (toMTRClusterStateCacheContainer clusterStateCacheContainer) (toNSNumber endpoint) (toNSObject queue) completion

-- | @- readAttributeInterconnectSmokeAlarmWithCompletion:@
readAttributeInterconnectSmokeAlarmWithCompletion :: IsMTRBaseClusterSmokeCOAlarm mtrBaseClusterSmokeCOAlarm => mtrBaseClusterSmokeCOAlarm -> Ptr () -> IO ()
readAttributeInterconnectSmokeAlarmWithCompletion mtrBaseClusterSmokeCOAlarm completion =
  sendMessage mtrBaseClusterSmokeCOAlarm readAttributeInterconnectSmokeAlarmWithCompletionSelector completion

-- | @- subscribeAttributeInterconnectSmokeAlarmWithParams:subscriptionEstablished:reportHandler:@
subscribeAttributeInterconnectSmokeAlarmWithParams_subscriptionEstablished_reportHandler :: (IsMTRBaseClusterSmokeCOAlarm mtrBaseClusterSmokeCOAlarm, IsMTRSubscribeParams params) => mtrBaseClusterSmokeCOAlarm -> params -> Ptr () -> Ptr () -> IO ()
subscribeAttributeInterconnectSmokeAlarmWithParams_subscriptionEstablished_reportHandler mtrBaseClusterSmokeCOAlarm params subscriptionEstablished reportHandler =
  sendMessage mtrBaseClusterSmokeCOAlarm subscribeAttributeInterconnectSmokeAlarmWithParams_subscriptionEstablished_reportHandlerSelector (toMTRSubscribeParams params) subscriptionEstablished reportHandler

-- | @+ readAttributeInterconnectSmokeAlarmWithClusterStateCache:endpoint:queue:completion:@
readAttributeInterconnectSmokeAlarmWithClusterStateCache_endpoint_queue_completion :: (IsMTRClusterStateCacheContainer clusterStateCacheContainer, IsNSNumber endpoint, IsNSObject queue) => clusterStateCacheContainer -> endpoint -> queue -> Ptr () -> IO ()
readAttributeInterconnectSmokeAlarmWithClusterStateCache_endpoint_queue_completion clusterStateCacheContainer endpoint queue completion =
  do
    cls' <- getRequiredClass "MTRBaseClusterSmokeCOAlarm"
    sendClassMessage cls' readAttributeInterconnectSmokeAlarmWithClusterStateCache_endpoint_queue_completionSelector (toMTRClusterStateCacheContainer clusterStateCacheContainer) (toNSNumber endpoint) (toNSObject queue) completion

-- | @- readAttributeInterconnectCOAlarmWithCompletion:@
readAttributeInterconnectCOAlarmWithCompletion :: IsMTRBaseClusterSmokeCOAlarm mtrBaseClusterSmokeCOAlarm => mtrBaseClusterSmokeCOAlarm -> Ptr () -> IO ()
readAttributeInterconnectCOAlarmWithCompletion mtrBaseClusterSmokeCOAlarm completion =
  sendMessage mtrBaseClusterSmokeCOAlarm readAttributeInterconnectCOAlarmWithCompletionSelector completion

-- | @- subscribeAttributeInterconnectCOAlarmWithParams:subscriptionEstablished:reportHandler:@
subscribeAttributeInterconnectCOAlarmWithParams_subscriptionEstablished_reportHandler :: (IsMTRBaseClusterSmokeCOAlarm mtrBaseClusterSmokeCOAlarm, IsMTRSubscribeParams params) => mtrBaseClusterSmokeCOAlarm -> params -> Ptr () -> Ptr () -> IO ()
subscribeAttributeInterconnectCOAlarmWithParams_subscriptionEstablished_reportHandler mtrBaseClusterSmokeCOAlarm params subscriptionEstablished reportHandler =
  sendMessage mtrBaseClusterSmokeCOAlarm subscribeAttributeInterconnectCOAlarmWithParams_subscriptionEstablished_reportHandlerSelector (toMTRSubscribeParams params) subscriptionEstablished reportHandler

-- | @+ readAttributeInterconnectCOAlarmWithClusterStateCache:endpoint:queue:completion:@
readAttributeInterconnectCOAlarmWithClusterStateCache_endpoint_queue_completion :: (IsMTRClusterStateCacheContainer clusterStateCacheContainer, IsNSNumber endpoint, IsNSObject queue) => clusterStateCacheContainer -> endpoint -> queue -> Ptr () -> IO ()
readAttributeInterconnectCOAlarmWithClusterStateCache_endpoint_queue_completion clusterStateCacheContainer endpoint queue completion =
  do
    cls' <- getRequiredClass "MTRBaseClusterSmokeCOAlarm"
    sendClassMessage cls' readAttributeInterconnectCOAlarmWithClusterStateCache_endpoint_queue_completionSelector (toMTRClusterStateCacheContainer clusterStateCacheContainer) (toNSNumber endpoint) (toNSObject queue) completion

-- | @- readAttributeContaminationStateWithCompletion:@
readAttributeContaminationStateWithCompletion :: IsMTRBaseClusterSmokeCOAlarm mtrBaseClusterSmokeCOAlarm => mtrBaseClusterSmokeCOAlarm -> Ptr () -> IO ()
readAttributeContaminationStateWithCompletion mtrBaseClusterSmokeCOAlarm completion =
  sendMessage mtrBaseClusterSmokeCOAlarm readAttributeContaminationStateWithCompletionSelector completion

-- | @- subscribeAttributeContaminationStateWithParams:subscriptionEstablished:reportHandler:@
subscribeAttributeContaminationStateWithParams_subscriptionEstablished_reportHandler :: (IsMTRBaseClusterSmokeCOAlarm mtrBaseClusterSmokeCOAlarm, IsMTRSubscribeParams params) => mtrBaseClusterSmokeCOAlarm -> params -> Ptr () -> Ptr () -> IO ()
subscribeAttributeContaminationStateWithParams_subscriptionEstablished_reportHandler mtrBaseClusterSmokeCOAlarm params subscriptionEstablished reportHandler =
  sendMessage mtrBaseClusterSmokeCOAlarm subscribeAttributeContaminationStateWithParams_subscriptionEstablished_reportHandlerSelector (toMTRSubscribeParams params) subscriptionEstablished reportHandler

-- | @+ readAttributeContaminationStateWithClusterStateCache:endpoint:queue:completion:@
readAttributeContaminationStateWithClusterStateCache_endpoint_queue_completion :: (IsMTRClusterStateCacheContainer clusterStateCacheContainer, IsNSNumber endpoint, IsNSObject queue) => clusterStateCacheContainer -> endpoint -> queue -> Ptr () -> IO ()
readAttributeContaminationStateWithClusterStateCache_endpoint_queue_completion clusterStateCacheContainer endpoint queue completion =
  do
    cls' <- getRequiredClass "MTRBaseClusterSmokeCOAlarm"
    sendClassMessage cls' readAttributeContaminationStateWithClusterStateCache_endpoint_queue_completionSelector (toMTRClusterStateCacheContainer clusterStateCacheContainer) (toNSNumber endpoint) (toNSObject queue) completion

-- | @- readAttributeSmokeSensitivityLevelWithCompletion:@
readAttributeSmokeSensitivityLevelWithCompletion :: IsMTRBaseClusterSmokeCOAlarm mtrBaseClusterSmokeCOAlarm => mtrBaseClusterSmokeCOAlarm -> Ptr () -> IO ()
readAttributeSmokeSensitivityLevelWithCompletion mtrBaseClusterSmokeCOAlarm completion =
  sendMessage mtrBaseClusterSmokeCOAlarm readAttributeSmokeSensitivityLevelWithCompletionSelector completion

-- | @- writeAttributeSmokeSensitivityLevelWithValue:completion:@
writeAttributeSmokeSensitivityLevelWithValue_completion :: (IsMTRBaseClusterSmokeCOAlarm mtrBaseClusterSmokeCOAlarm, IsNSNumber value) => mtrBaseClusterSmokeCOAlarm -> value -> Ptr () -> IO ()
writeAttributeSmokeSensitivityLevelWithValue_completion mtrBaseClusterSmokeCOAlarm value completion =
  sendMessage mtrBaseClusterSmokeCOAlarm writeAttributeSmokeSensitivityLevelWithValue_completionSelector (toNSNumber value) completion

-- | @- writeAttributeSmokeSensitivityLevelWithValue:params:completion:@
writeAttributeSmokeSensitivityLevelWithValue_params_completion :: (IsMTRBaseClusterSmokeCOAlarm mtrBaseClusterSmokeCOAlarm, IsNSNumber value, IsMTRWriteParams params) => mtrBaseClusterSmokeCOAlarm -> value -> params -> Ptr () -> IO ()
writeAttributeSmokeSensitivityLevelWithValue_params_completion mtrBaseClusterSmokeCOAlarm value params completion =
  sendMessage mtrBaseClusterSmokeCOAlarm writeAttributeSmokeSensitivityLevelWithValue_params_completionSelector (toNSNumber value) (toMTRWriteParams params) completion

-- | @- subscribeAttributeSmokeSensitivityLevelWithParams:subscriptionEstablished:reportHandler:@
subscribeAttributeSmokeSensitivityLevelWithParams_subscriptionEstablished_reportHandler :: (IsMTRBaseClusterSmokeCOAlarm mtrBaseClusterSmokeCOAlarm, IsMTRSubscribeParams params) => mtrBaseClusterSmokeCOAlarm -> params -> Ptr () -> Ptr () -> IO ()
subscribeAttributeSmokeSensitivityLevelWithParams_subscriptionEstablished_reportHandler mtrBaseClusterSmokeCOAlarm params subscriptionEstablished reportHandler =
  sendMessage mtrBaseClusterSmokeCOAlarm subscribeAttributeSmokeSensitivityLevelWithParams_subscriptionEstablished_reportHandlerSelector (toMTRSubscribeParams params) subscriptionEstablished reportHandler

-- | @+ readAttributeSmokeSensitivityLevelWithClusterStateCache:endpoint:queue:completion:@
readAttributeSmokeSensitivityLevelWithClusterStateCache_endpoint_queue_completion :: (IsMTRClusterStateCacheContainer clusterStateCacheContainer, IsNSNumber endpoint, IsNSObject queue) => clusterStateCacheContainer -> endpoint -> queue -> Ptr () -> IO ()
readAttributeSmokeSensitivityLevelWithClusterStateCache_endpoint_queue_completion clusterStateCacheContainer endpoint queue completion =
  do
    cls' <- getRequiredClass "MTRBaseClusterSmokeCOAlarm"
    sendClassMessage cls' readAttributeSmokeSensitivityLevelWithClusterStateCache_endpoint_queue_completionSelector (toMTRClusterStateCacheContainer clusterStateCacheContainer) (toNSNumber endpoint) (toNSObject queue) completion

-- | @- readAttributeExpiryDateWithCompletion:@
readAttributeExpiryDateWithCompletion :: IsMTRBaseClusterSmokeCOAlarm mtrBaseClusterSmokeCOAlarm => mtrBaseClusterSmokeCOAlarm -> Ptr () -> IO ()
readAttributeExpiryDateWithCompletion mtrBaseClusterSmokeCOAlarm completion =
  sendMessage mtrBaseClusterSmokeCOAlarm readAttributeExpiryDateWithCompletionSelector completion

-- | @- subscribeAttributeExpiryDateWithParams:subscriptionEstablished:reportHandler:@
subscribeAttributeExpiryDateWithParams_subscriptionEstablished_reportHandler :: (IsMTRBaseClusterSmokeCOAlarm mtrBaseClusterSmokeCOAlarm, IsMTRSubscribeParams params) => mtrBaseClusterSmokeCOAlarm -> params -> Ptr () -> Ptr () -> IO ()
subscribeAttributeExpiryDateWithParams_subscriptionEstablished_reportHandler mtrBaseClusterSmokeCOAlarm params subscriptionEstablished reportHandler =
  sendMessage mtrBaseClusterSmokeCOAlarm subscribeAttributeExpiryDateWithParams_subscriptionEstablished_reportHandlerSelector (toMTRSubscribeParams params) subscriptionEstablished reportHandler

-- | @+ readAttributeExpiryDateWithClusterStateCache:endpoint:queue:completion:@
readAttributeExpiryDateWithClusterStateCache_endpoint_queue_completion :: (IsMTRClusterStateCacheContainer clusterStateCacheContainer, IsNSNumber endpoint, IsNSObject queue) => clusterStateCacheContainer -> endpoint -> queue -> Ptr () -> IO ()
readAttributeExpiryDateWithClusterStateCache_endpoint_queue_completion clusterStateCacheContainer endpoint queue completion =
  do
    cls' <- getRequiredClass "MTRBaseClusterSmokeCOAlarm"
    sendClassMessage cls' readAttributeExpiryDateWithClusterStateCache_endpoint_queue_completionSelector (toMTRClusterStateCacheContainer clusterStateCacheContainer) (toNSNumber endpoint) (toNSObject queue) completion

-- | @- readAttributeGeneratedCommandListWithCompletion:@
readAttributeGeneratedCommandListWithCompletion :: IsMTRBaseClusterSmokeCOAlarm mtrBaseClusterSmokeCOAlarm => mtrBaseClusterSmokeCOAlarm -> Ptr () -> IO ()
readAttributeGeneratedCommandListWithCompletion mtrBaseClusterSmokeCOAlarm completion =
  sendMessage mtrBaseClusterSmokeCOAlarm readAttributeGeneratedCommandListWithCompletionSelector completion

-- | @- subscribeAttributeGeneratedCommandListWithParams:subscriptionEstablished:reportHandler:@
subscribeAttributeGeneratedCommandListWithParams_subscriptionEstablished_reportHandler :: (IsMTRBaseClusterSmokeCOAlarm mtrBaseClusterSmokeCOAlarm, IsMTRSubscribeParams params) => mtrBaseClusterSmokeCOAlarm -> params -> Ptr () -> Ptr () -> IO ()
subscribeAttributeGeneratedCommandListWithParams_subscriptionEstablished_reportHandler mtrBaseClusterSmokeCOAlarm params subscriptionEstablished reportHandler =
  sendMessage mtrBaseClusterSmokeCOAlarm subscribeAttributeGeneratedCommandListWithParams_subscriptionEstablished_reportHandlerSelector (toMTRSubscribeParams params) subscriptionEstablished reportHandler

-- | @+ readAttributeGeneratedCommandListWithClusterStateCache:endpoint:queue:completion:@
readAttributeGeneratedCommandListWithClusterStateCache_endpoint_queue_completion :: (IsMTRClusterStateCacheContainer clusterStateCacheContainer, IsNSNumber endpoint, IsNSObject queue) => clusterStateCacheContainer -> endpoint -> queue -> Ptr () -> IO ()
readAttributeGeneratedCommandListWithClusterStateCache_endpoint_queue_completion clusterStateCacheContainer endpoint queue completion =
  do
    cls' <- getRequiredClass "MTRBaseClusterSmokeCOAlarm"
    sendClassMessage cls' readAttributeGeneratedCommandListWithClusterStateCache_endpoint_queue_completionSelector (toMTRClusterStateCacheContainer clusterStateCacheContainer) (toNSNumber endpoint) (toNSObject queue) completion

-- | @- readAttributeAcceptedCommandListWithCompletion:@
readAttributeAcceptedCommandListWithCompletion :: IsMTRBaseClusterSmokeCOAlarm mtrBaseClusterSmokeCOAlarm => mtrBaseClusterSmokeCOAlarm -> Ptr () -> IO ()
readAttributeAcceptedCommandListWithCompletion mtrBaseClusterSmokeCOAlarm completion =
  sendMessage mtrBaseClusterSmokeCOAlarm readAttributeAcceptedCommandListWithCompletionSelector completion

-- | @- subscribeAttributeAcceptedCommandListWithParams:subscriptionEstablished:reportHandler:@
subscribeAttributeAcceptedCommandListWithParams_subscriptionEstablished_reportHandler :: (IsMTRBaseClusterSmokeCOAlarm mtrBaseClusterSmokeCOAlarm, IsMTRSubscribeParams params) => mtrBaseClusterSmokeCOAlarm -> params -> Ptr () -> Ptr () -> IO ()
subscribeAttributeAcceptedCommandListWithParams_subscriptionEstablished_reportHandler mtrBaseClusterSmokeCOAlarm params subscriptionEstablished reportHandler =
  sendMessage mtrBaseClusterSmokeCOAlarm subscribeAttributeAcceptedCommandListWithParams_subscriptionEstablished_reportHandlerSelector (toMTRSubscribeParams params) subscriptionEstablished reportHandler

-- | @+ readAttributeAcceptedCommandListWithClusterStateCache:endpoint:queue:completion:@
readAttributeAcceptedCommandListWithClusterStateCache_endpoint_queue_completion :: (IsMTRClusterStateCacheContainer clusterStateCacheContainer, IsNSNumber endpoint, IsNSObject queue) => clusterStateCacheContainer -> endpoint -> queue -> Ptr () -> IO ()
readAttributeAcceptedCommandListWithClusterStateCache_endpoint_queue_completion clusterStateCacheContainer endpoint queue completion =
  do
    cls' <- getRequiredClass "MTRBaseClusterSmokeCOAlarm"
    sendClassMessage cls' readAttributeAcceptedCommandListWithClusterStateCache_endpoint_queue_completionSelector (toMTRClusterStateCacheContainer clusterStateCacheContainer) (toNSNumber endpoint) (toNSObject queue) completion

-- | @- readAttributeAttributeListWithCompletion:@
readAttributeAttributeListWithCompletion :: IsMTRBaseClusterSmokeCOAlarm mtrBaseClusterSmokeCOAlarm => mtrBaseClusterSmokeCOAlarm -> Ptr () -> IO ()
readAttributeAttributeListWithCompletion mtrBaseClusterSmokeCOAlarm completion =
  sendMessage mtrBaseClusterSmokeCOAlarm readAttributeAttributeListWithCompletionSelector completion

-- | @- subscribeAttributeAttributeListWithParams:subscriptionEstablished:reportHandler:@
subscribeAttributeAttributeListWithParams_subscriptionEstablished_reportHandler :: (IsMTRBaseClusterSmokeCOAlarm mtrBaseClusterSmokeCOAlarm, IsMTRSubscribeParams params) => mtrBaseClusterSmokeCOAlarm -> params -> Ptr () -> Ptr () -> IO ()
subscribeAttributeAttributeListWithParams_subscriptionEstablished_reportHandler mtrBaseClusterSmokeCOAlarm params subscriptionEstablished reportHandler =
  sendMessage mtrBaseClusterSmokeCOAlarm subscribeAttributeAttributeListWithParams_subscriptionEstablished_reportHandlerSelector (toMTRSubscribeParams params) subscriptionEstablished reportHandler

-- | @+ readAttributeAttributeListWithClusterStateCache:endpoint:queue:completion:@
readAttributeAttributeListWithClusterStateCache_endpoint_queue_completion :: (IsMTRClusterStateCacheContainer clusterStateCacheContainer, IsNSNumber endpoint, IsNSObject queue) => clusterStateCacheContainer -> endpoint -> queue -> Ptr () -> IO ()
readAttributeAttributeListWithClusterStateCache_endpoint_queue_completion clusterStateCacheContainer endpoint queue completion =
  do
    cls' <- getRequiredClass "MTRBaseClusterSmokeCOAlarm"
    sendClassMessage cls' readAttributeAttributeListWithClusterStateCache_endpoint_queue_completionSelector (toMTRClusterStateCacheContainer clusterStateCacheContainer) (toNSNumber endpoint) (toNSObject queue) completion

-- | @- readAttributeFeatureMapWithCompletion:@
readAttributeFeatureMapWithCompletion :: IsMTRBaseClusterSmokeCOAlarm mtrBaseClusterSmokeCOAlarm => mtrBaseClusterSmokeCOAlarm -> Ptr () -> IO ()
readAttributeFeatureMapWithCompletion mtrBaseClusterSmokeCOAlarm completion =
  sendMessage mtrBaseClusterSmokeCOAlarm readAttributeFeatureMapWithCompletionSelector completion

-- | @- subscribeAttributeFeatureMapWithParams:subscriptionEstablished:reportHandler:@
subscribeAttributeFeatureMapWithParams_subscriptionEstablished_reportHandler :: (IsMTRBaseClusterSmokeCOAlarm mtrBaseClusterSmokeCOAlarm, IsMTRSubscribeParams params) => mtrBaseClusterSmokeCOAlarm -> params -> Ptr () -> Ptr () -> IO ()
subscribeAttributeFeatureMapWithParams_subscriptionEstablished_reportHandler mtrBaseClusterSmokeCOAlarm params subscriptionEstablished reportHandler =
  sendMessage mtrBaseClusterSmokeCOAlarm subscribeAttributeFeatureMapWithParams_subscriptionEstablished_reportHandlerSelector (toMTRSubscribeParams params) subscriptionEstablished reportHandler

-- | @+ readAttributeFeatureMapWithClusterStateCache:endpoint:queue:completion:@
readAttributeFeatureMapWithClusterStateCache_endpoint_queue_completion :: (IsMTRClusterStateCacheContainer clusterStateCacheContainer, IsNSNumber endpoint, IsNSObject queue) => clusterStateCacheContainer -> endpoint -> queue -> Ptr () -> IO ()
readAttributeFeatureMapWithClusterStateCache_endpoint_queue_completion clusterStateCacheContainer endpoint queue completion =
  do
    cls' <- getRequiredClass "MTRBaseClusterSmokeCOAlarm"
    sendClassMessage cls' readAttributeFeatureMapWithClusterStateCache_endpoint_queue_completionSelector (toMTRClusterStateCacheContainer clusterStateCacheContainer) (toNSNumber endpoint) (toNSObject queue) completion

-- | @- readAttributeClusterRevisionWithCompletion:@
readAttributeClusterRevisionWithCompletion :: IsMTRBaseClusterSmokeCOAlarm mtrBaseClusterSmokeCOAlarm => mtrBaseClusterSmokeCOAlarm -> Ptr () -> IO ()
readAttributeClusterRevisionWithCompletion mtrBaseClusterSmokeCOAlarm completion =
  sendMessage mtrBaseClusterSmokeCOAlarm readAttributeClusterRevisionWithCompletionSelector completion

-- | @- subscribeAttributeClusterRevisionWithParams:subscriptionEstablished:reportHandler:@
subscribeAttributeClusterRevisionWithParams_subscriptionEstablished_reportHandler :: (IsMTRBaseClusterSmokeCOAlarm mtrBaseClusterSmokeCOAlarm, IsMTRSubscribeParams params) => mtrBaseClusterSmokeCOAlarm -> params -> Ptr () -> Ptr () -> IO ()
subscribeAttributeClusterRevisionWithParams_subscriptionEstablished_reportHandler mtrBaseClusterSmokeCOAlarm params subscriptionEstablished reportHandler =
  sendMessage mtrBaseClusterSmokeCOAlarm subscribeAttributeClusterRevisionWithParams_subscriptionEstablished_reportHandlerSelector (toMTRSubscribeParams params) subscriptionEstablished reportHandler

-- | @+ readAttributeClusterRevisionWithClusterStateCache:endpoint:queue:completion:@
readAttributeClusterRevisionWithClusterStateCache_endpoint_queue_completion :: (IsMTRClusterStateCacheContainer clusterStateCacheContainer, IsNSNumber endpoint, IsNSObject queue) => clusterStateCacheContainer -> endpoint -> queue -> Ptr () -> IO ()
readAttributeClusterRevisionWithClusterStateCache_endpoint_queue_completion clusterStateCacheContainer endpoint queue completion =
  do
    cls' <- getRequiredClass "MTRBaseClusterSmokeCOAlarm"
    sendClassMessage cls' readAttributeClusterRevisionWithClusterStateCache_endpoint_queue_completionSelector (toMTRClusterStateCacheContainer clusterStateCacheContainer) (toNSNumber endpoint) (toNSObject queue) completion

-- | @- init@
init_ :: IsMTRBaseClusterSmokeCOAlarm mtrBaseClusterSmokeCOAlarm => mtrBaseClusterSmokeCOAlarm -> IO (Id MTRBaseClusterSmokeCOAlarm)
init_ mtrBaseClusterSmokeCOAlarm =
  sendOwnedMessage mtrBaseClusterSmokeCOAlarm initSelector

-- | @+ new@
new :: IO (Id MTRBaseClusterSmokeCOAlarm)
new  =
  do
    cls' <- getRequiredClass "MTRBaseClusterSmokeCOAlarm"
    sendOwnedClassMessage cls' newSelector

-- | For all instance methods (reads, writes, commands) that take a completion, the completion will be called on the provided queue.
--
-- ObjC selector: @- initWithDevice:endpointID:queue:@
initWithDevice_endpointID_queue :: (IsMTRBaseClusterSmokeCOAlarm mtrBaseClusterSmokeCOAlarm, IsMTRBaseDevice device, IsNSNumber endpointID, IsNSObject queue) => mtrBaseClusterSmokeCOAlarm -> device -> endpointID -> queue -> IO (Id MTRBaseClusterSmokeCOAlarm)
initWithDevice_endpointID_queue mtrBaseClusterSmokeCOAlarm device endpointID queue =
  sendOwnedMessage mtrBaseClusterSmokeCOAlarm initWithDevice_endpointID_queueSelector (toMTRBaseDevice device) (toNSNumber endpointID) (toNSObject queue)

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @selfTestRequestWithParams:completion:@
selfTestRequestWithParams_completionSelector :: Selector '[Id MTRSmokeCOAlarmClusterSelfTestRequestParams, Ptr ()] ()
selfTestRequestWithParams_completionSelector = mkSelector "selfTestRequestWithParams:completion:"

-- | @Selector@ for @selfTestRequestWithCompletion:@
selfTestRequestWithCompletionSelector :: Selector '[Ptr ()] ()
selfTestRequestWithCompletionSelector = mkSelector "selfTestRequestWithCompletion:"

-- | @Selector@ for @readAttributeExpressedStateWithCompletion:@
readAttributeExpressedStateWithCompletionSelector :: Selector '[Ptr ()] ()
readAttributeExpressedStateWithCompletionSelector = mkSelector "readAttributeExpressedStateWithCompletion:"

-- | @Selector@ for @subscribeAttributeExpressedStateWithParams:subscriptionEstablished:reportHandler:@
subscribeAttributeExpressedStateWithParams_subscriptionEstablished_reportHandlerSelector :: Selector '[Id MTRSubscribeParams, Ptr (), Ptr ()] ()
subscribeAttributeExpressedStateWithParams_subscriptionEstablished_reportHandlerSelector = mkSelector "subscribeAttributeExpressedStateWithParams:subscriptionEstablished:reportHandler:"

-- | @Selector@ for @readAttributeExpressedStateWithClusterStateCache:endpoint:queue:completion:@
readAttributeExpressedStateWithClusterStateCache_endpoint_queue_completionSelector :: Selector '[Id MTRClusterStateCacheContainer, Id NSNumber, Id NSObject, Ptr ()] ()
readAttributeExpressedStateWithClusterStateCache_endpoint_queue_completionSelector = mkSelector "readAttributeExpressedStateWithClusterStateCache:endpoint:queue:completion:"

-- | @Selector@ for @readAttributeSmokeStateWithCompletion:@
readAttributeSmokeStateWithCompletionSelector :: Selector '[Ptr ()] ()
readAttributeSmokeStateWithCompletionSelector = mkSelector "readAttributeSmokeStateWithCompletion:"

-- | @Selector@ for @subscribeAttributeSmokeStateWithParams:subscriptionEstablished:reportHandler:@
subscribeAttributeSmokeStateWithParams_subscriptionEstablished_reportHandlerSelector :: Selector '[Id MTRSubscribeParams, Ptr (), Ptr ()] ()
subscribeAttributeSmokeStateWithParams_subscriptionEstablished_reportHandlerSelector = mkSelector "subscribeAttributeSmokeStateWithParams:subscriptionEstablished:reportHandler:"

-- | @Selector@ for @readAttributeSmokeStateWithClusterStateCache:endpoint:queue:completion:@
readAttributeSmokeStateWithClusterStateCache_endpoint_queue_completionSelector :: Selector '[Id MTRClusterStateCacheContainer, Id NSNumber, Id NSObject, Ptr ()] ()
readAttributeSmokeStateWithClusterStateCache_endpoint_queue_completionSelector = mkSelector "readAttributeSmokeStateWithClusterStateCache:endpoint:queue:completion:"

-- | @Selector@ for @readAttributeCOStateWithCompletion:@
readAttributeCOStateWithCompletionSelector :: Selector '[Ptr ()] ()
readAttributeCOStateWithCompletionSelector = mkSelector "readAttributeCOStateWithCompletion:"

-- | @Selector@ for @subscribeAttributeCOStateWithParams:subscriptionEstablished:reportHandler:@
subscribeAttributeCOStateWithParams_subscriptionEstablished_reportHandlerSelector :: Selector '[Id MTRSubscribeParams, Ptr (), Ptr ()] ()
subscribeAttributeCOStateWithParams_subscriptionEstablished_reportHandlerSelector = mkSelector "subscribeAttributeCOStateWithParams:subscriptionEstablished:reportHandler:"

-- | @Selector@ for @readAttributeCOStateWithClusterStateCache:endpoint:queue:completion:@
readAttributeCOStateWithClusterStateCache_endpoint_queue_completionSelector :: Selector '[Id MTRClusterStateCacheContainer, Id NSNumber, Id NSObject, Ptr ()] ()
readAttributeCOStateWithClusterStateCache_endpoint_queue_completionSelector = mkSelector "readAttributeCOStateWithClusterStateCache:endpoint:queue:completion:"

-- | @Selector@ for @readAttributeBatteryAlertWithCompletion:@
readAttributeBatteryAlertWithCompletionSelector :: Selector '[Ptr ()] ()
readAttributeBatteryAlertWithCompletionSelector = mkSelector "readAttributeBatteryAlertWithCompletion:"

-- | @Selector@ for @subscribeAttributeBatteryAlertWithParams:subscriptionEstablished:reportHandler:@
subscribeAttributeBatteryAlertWithParams_subscriptionEstablished_reportHandlerSelector :: Selector '[Id MTRSubscribeParams, Ptr (), Ptr ()] ()
subscribeAttributeBatteryAlertWithParams_subscriptionEstablished_reportHandlerSelector = mkSelector "subscribeAttributeBatteryAlertWithParams:subscriptionEstablished:reportHandler:"

-- | @Selector@ for @readAttributeBatteryAlertWithClusterStateCache:endpoint:queue:completion:@
readAttributeBatteryAlertWithClusterStateCache_endpoint_queue_completionSelector :: Selector '[Id MTRClusterStateCacheContainer, Id NSNumber, Id NSObject, Ptr ()] ()
readAttributeBatteryAlertWithClusterStateCache_endpoint_queue_completionSelector = mkSelector "readAttributeBatteryAlertWithClusterStateCache:endpoint:queue:completion:"

-- | @Selector@ for @readAttributeDeviceMutedWithCompletion:@
readAttributeDeviceMutedWithCompletionSelector :: Selector '[Ptr ()] ()
readAttributeDeviceMutedWithCompletionSelector = mkSelector "readAttributeDeviceMutedWithCompletion:"

-- | @Selector@ for @subscribeAttributeDeviceMutedWithParams:subscriptionEstablished:reportHandler:@
subscribeAttributeDeviceMutedWithParams_subscriptionEstablished_reportHandlerSelector :: Selector '[Id MTRSubscribeParams, Ptr (), Ptr ()] ()
subscribeAttributeDeviceMutedWithParams_subscriptionEstablished_reportHandlerSelector = mkSelector "subscribeAttributeDeviceMutedWithParams:subscriptionEstablished:reportHandler:"

-- | @Selector@ for @readAttributeDeviceMutedWithClusterStateCache:endpoint:queue:completion:@
readAttributeDeviceMutedWithClusterStateCache_endpoint_queue_completionSelector :: Selector '[Id MTRClusterStateCacheContainer, Id NSNumber, Id NSObject, Ptr ()] ()
readAttributeDeviceMutedWithClusterStateCache_endpoint_queue_completionSelector = mkSelector "readAttributeDeviceMutedWithClusterStateCache:endpoint:queue:completion:"

-- | @Selector@ for @readAttributeTestInProgressWithCompletion:@
readAttributeTestInProgressWithCompletionSelector :: Selector '[Ptr ()] ()
readAttributeTestInProgressWithCompletionSelector = mkSelector "readAttributeTestInProgressWithCompletion:"

-- | @Selector@ for @subscribeAttributeTestInProgressWithParams:subscriptionEstablished:reportHandler:@
subscribeAttributeTestInProgressWithParams_subscriptionEstablished_reportHandlerSelector :: Selector '[Id MTRSubscribeParams, Ptr (), Ptr ()] ()
subscribeAttributeTestInProgressWithParams_subscriptionEstablished_reportHandlerSelector = mkSelector "subscribeAttributeTestInProgressWithParams:subscriptionEstablished:reportHandler:"

-- | @Selector@ for @readAttributeTestInProgressWithClusterStateCache:endpoint:queue:completion:@
readAttributeTestInProgressWithClusterStateCache_endpoint_queue_completionSelector :: Selector '[Id MTRClusterStateCacheContainer, Id NSNumber, Id NSObject, Ptr ()] ()
readAttributeTestInProgressWithClusterStateCache_endpoint_queue_completionSelector = mkSelector "readAttributeTestInProgressWithClusterStateCache:endpoint:queue:completion:"

-- | @Selector@ for @readAttributeHardwareFaultAlertWithCompletion:@
readAttributeHardwareFaultAlertWithCompletionSelector :: Selector '[Ptr ()] ()
readAttributeHardwareFaultAlertWithCompletionSelector = mkSelector "readAttributeHardwareFaultAlertWithCompletion:"

-- | @Selector@ for @subscribeAttributeHardwareFaultAlertWithParams:subscriptionEstablished:reportHandler:@
subscribeAttributeHardwareFaultAlertWithParams_subscriptionEstablished_reportHandlerSelector :: Selector '[Id MTRSubscribeParams, Ptr (), Ptr ()] ()
subscribeAttributeHardwareFaultAlertWithParams_subscriptionEstablished_reportHandlerSelector = mkSelector "subscribeAttributeHardwareFaultAlertWithParams:subscriptionEstablished:reportHandler:"

-- | @Selector@ for @readAttributeHardwareFaultAlertWithClusterStateCache:endpoint:queue:completion:@
readAttributeHardwareFaultAlertWithClusterStateCache_endpoint_queue_completionSelector :: Selector '[Id MTRClusterStateCacheContainer, Id NSNumber, Id NSObject, Ptr ()] ()
readAttributeHardwareFaultAlertWithClusterStateCache_endpoint_queue_completionSelector = mkSelector "readAttributeHardwareFaultAlertWithClusterStateCache:endpoint:queue:completion:"

-- | @Selector@ for @readAttributeEndOfServiceAlertWithCompletion:@
readAttributeEndOfServiceAlertWithCompletionSelector :: Selector '[Ptr ()] ()
readAttributeEndOfServiceAlertWithCompletionSelector = mkSelector "readAttributeEndOfServiceAlertWithCompletion:"

-- | @Selector@ for @subscribeAttributeEndOfServiceAlertWithParams:subscriptionEstablished:reportHandler:@
subscribeAttributeEndOfServiceAlertWithParams_subscriptionEstablished_reportHandlerSelector :: Selector '[Id MTRSubscribeParams, Ptr (), Ptr ()] ()
subscribeAttributeEndOfServiceAlertWithParams_subscriptionEstablished_reportHandlerSelector = mkSelector "subscribeAttributeEndOfServiceAlertWithParams:subscriptionEstablished:reportHandler:"

-- | @Selector@ for @readAttributeEndOfServiceAlertWithClusterStateCache:endpoint:queue:completion:@
readAttributeEndOfServiceAlertWithClusterStateCache_endpoint_queue_completionSelector :: Selector '[Id MTRClusterStateCacheContainer, Id NSNumber, Id NSObject, Ptr ()] ()
readAttributeEndOfServiceAlertWithClusterStateCache_endpoint_queue_completionSelector = mkSelector "readAttributeEndOfServiceAlertWithClusterStateCache:endpoint:queue:completion:"

-- | @Selector@ for @readAttributeInterconnectSmokeAlarmWithCompletion:@
readAttributeInterconnectSmokeAlarmWithCompletionSelector :: Selector '[Ptr ()] ()
readAttributeInterconnectSmokeAlarmWithCompletionSelector = mkSelector "readAttributeInterconnectSmokeAlarmWithCompletion:"

-- | @Selector@ for @subscribeAttributeInterconnectSmokeAlarmWithParams:subscriptionEstablished:reportHandler:@
subscribeAttributeInterconnectSmokeAlarmWithParams_subscriptionEstablished_reportHandlerSelector :: Selector '[Id MTRSubscribeParams, Ptr (), Ptr ()] ()
subscribeAttributeInterconnectSmokeAlarmWithParams_subscriptionEstablished_reportHandlerSelector = mkSelector "subscribeAttributeInterconnectSmokeAlarmWithParams:subscriptionEstablished:reportHandler:"

-- | @Selector@ for @readAttributeInterconnectSmokeAlarmWithClusterStateCache:endpoint:queue:completion:@
readAttributeInterconnectSmokeAlarmWithClusterStateCache_endpoint_queue_completionSelector :: Selector '[Id MTRClusterStateCacheContainer, Id NSNumber, Id NSObject, Ptr ()] ()
readAttributeInterconnectSmokeAlarmWithClusterStateCache_endpoint_queue_completionSelector = mkSelector "readAttributeInterconnectSmokeAlarmWithClusterStateCache:endpoint:queue:completion:"

-- | @Selector@ for @readAttributeInterconnectCOAlarmWithCompletion:@
readAttributeInterconnectCOAlarmWithCompletionSelector :: Selector '[Ptr ()] ()
readAttributeInterconnectCOAlarmWithCompletionSelector = mkSelector "readAttributeInterconnectCOAlarmWithCompletion:"

-- | @Selector@ for @subscribeAttributeInterconnectCOAlarmWithParams:subscriptionEstablished:reportHandler:@
subscribeAttributeInterconnectCOAlarmWithParams_subscriptionEstablished_reportHandlerSelector :: Selector '[Id MTRSubscribeParams, Ptr (), Ptr ()] ()
subscribeAttributeInterconnectCOAlarmWithParams_subscriptionEstablished_reportHandlerSelector = mkSelector "subscribeAttributeInterconnectCOAlarmWithParams:subscriptionEstablished:reportHandler:"

-- | @Selector@ for @readAttributeInterconnectCOAlarmWithClusterStateCache:endpoint:queue:completion:@
readAttributeInterconnectCOAlarmWithClusterStateCache_endpoint_queue_completionSelector :: Selector '[Id MTRClusterStateCacheContainer, Id NSNumber, Id NSObject, Ptr ()] ()
readAttributeInterconnectCOAlarmWithClusterStateCache_endpoint_queue_completionSelector = mkSelector "readAttributeInterconnectCOAlarmWithClusterStateCache:endpoint:queue:completion:"

-- | @Selector@ for @readAttributeContaminationStateWithCompletion:@
readAttributeContaminationStateWithCompletionSelector :: Selector '[Ptr ()] ()
readAttributeContaminationStateWithCompletionSelector = mkSelector "readAttributeContaminationStateWithCompletion:"

-- | @Selector@ for @subscribeAttributeContaminationStateWithParams:subscriptionEstablished:reportHandler:@
subscribeAttributeContaminationStateWithParams_subscriptionEstablished_reportHandlerSelector :: Selector '[Id MTRSubscribeParams, Ptr (), Ptr ()] ()
subscribeAttributeContaminationStateWithParams_subscriptionEstablished_reportHandlerSelector = mkSelector "subscribeAttributeContaminationStateWithParams:subscriptionEstablished:reportHandler:"

-- | @Selector@ for @readAttributeContaminationStateWithClusterStateCache:endpoint:queue:completion:@
readAttributeContaminationStateWithClusterStateCache_endpoint_queue_completionSelector :: Selector '[Id MTRClusterStateCacheContainer, Id NSNumber, Id NSObject, Ptr ()] ()
readAttributeContaminationStateWithClusterStateCache_endpoint_queue_completionSelector = mkSelector "readAttributeContaminationStateWithClusterStateCache:endpoint:queue:completion:"

-- | @Selector@ for @readAttributeSmokeSensitivityLevelWithCompletion:@
readAttributeSmokeSensitivityLevelWithCompletionSelector :: Selector '[Ptr ()] ()
readAttributeSmokeSensitivityLevelWithCompletionSelector = mkSelector "readAttributeSmokeSensitivityLevelWithCompletion:"

-- | @Selector@ for @writeAttributeSmokeSensitivityLevelWithValue:completion:@
writeAttributeSmokeSensitivityLevelWithValue_completionSelector :: Selector '[Id NSNumber, Ptr ()] ()
writeAttributeSmokeSensitivityLevelWithValue_completionSelector = mkSelector "writeAttributeSmokeSensitivityLevelWithValue:completion:"

-- | @Selector@ for @writeAttributeSmokeSensitivityLevelWithValue:params:completion:@
writeAttributeSmokeSensitivityLevelWithValue_params_completionSelector :: Selector '[Id NSNumber, Id MTRWriteParams, Ptr ()] ()
writeAttributeSmokeSensitivityLevelWithValue_params_completionSelector = mkSelector "writeAttributeSmokeSensitivityLevelWithValue:params:completion:"

-- | @Selector@ for @subscribeAttributeSmokeSensitivityLevelWithParams:subscriptionEstablished:reportHandler:@
subscribeAttributeSmokeSensitivityLevelWithParams_subscriptionEstablished_reportHandlerSelector :: Selector '[Id MTRSubscribeParams, Ptr (), Ptr ()] ()
subscribeAttributeSmokeSensitivityLevelWithParams_subscriptionEstablished_reportHandlerSelector = mkSelector "subscribeAttributeSmokeSensitivityLevelWithParams:subscriptionEstablished:reportHandler:"

-- | @Selector@ for @readAttributeSmokeSensitivityLevelWithClusterStateCache:endpoint:queue:completion:@
readAttributeSmokeSensitivityLevelWithClusterStateCache_endpoint_queue_completionSelector :: Selector '[Id MTRClusterStateCacheContainer, Id NSNumber, Id NSObject, Ptr ()] ()
readAttributeSmokeSensitivityLevelWithClusterStateCache_endpoint_queue_completionSelector = mkSelector "readAttributeSmokeSensitivityLevelWithClusterStateCache:endpoint:queue:completion:"

-- | @Selector@ for @readAttributeExpiryDateWithCompletion:@
readAttributeExpiryDateWithCompletionSelector :: Selector '[Ptr ()] ()
readAttributeExpiryDateWithCompletionSelector = mkSelector "readAttributeExpiryDateWithCompletion:"

-- | @Selector@ for @subscribeAttributeExpiryDateWithParams:subscriptionEstablished:reportHandler:@
subscribeAttributeExpiryDateWithParams_subscriptionEstablished_reportHandlerSelector :: Selector '[Id MTRSubscribeParams, Ptr (), Ptr ()] ()
subscribeAttributeExpiryDateWithParams_subscriptionEstablished_reportHandlerSelector = mkSelector "subscribeAttributeExpiryDateWithParams:subscriptionEstablished:reportHandler:"

-- | @Selector@ for @readAttributeExpiryDateWithClusterStateCache:endpoint:queue:completion:@
readAttributeExpiryDateWithClusterStateCache_endpoint_queue_completionSelector :: Selector '[Id MTRClusterStateCacheContainer, Id NSNumber, Id NSObject, Ptr ()] ()
readAttributeExpiryDateWithClusterStateCache_endpoint_queue_completionSelector = mkSelector "readAttributeExpiryDateWithClusterStateCache:endpoint:queue:completion:"

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
initSelector :: Selector '[] (Id MTRBaseClusterSmokeCOAlarm)
initSelector = mkSelector "init"

-- | @Selector@ for @new@
newSelector :: Selector '[] (Id MTRBaseClusterSmokeCOAlarm)
newSelector = mkSelector "new"

-- | @Selector@ for @initWithDevice:endpointID:queue:@
initWithDevice_endpointID_queueSelector :: Selector '[Id MTRBaseDevice, Id NSNumber, Id NSObject] (Id MTRBaseClusterSmokeCOAlarm)
initWithDevice_endpointID_queueSelector = mkSelector "initWithDevice:endpointID:queue:"

