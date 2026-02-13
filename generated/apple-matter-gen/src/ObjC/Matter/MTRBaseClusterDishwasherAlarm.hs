{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Cluster Dishwasher Alarm
--
-- Attributes and commands for configuring the Dishwasher alarm.
--
-- Generated bindings for @MTRBaseClusterDishwasherAlarm@.
module ObjC.Matter.MTRBaseClusterDishwasherAlarm
  ( MTRBaseClusterDishwasherAlarm
  , IsMTRBaseClusterDishwasherAlarm(..)
  , resetWithParams_completion
  , modifyEnabledAlarmsWithParams_completion
  , readAttributeMaskWithCompletion
  , subscribeAttributeMaskWithParams_subscriptionEstablished_reportHandler
  , readAttributeMaskWithClusterStateCache_endpoint_queue_completion
  , readAttributeLatchWithCompletion
  , subscribeAttributeLatchWithParams_subscriptionEstablished_reportHandler
  , readAttributeLatchWithClusterStateCache_endpoint_queue_completion
  , readAttributeStateWithCompletion
  , subscribeAttributeStateWithParams_subscriptionEstablished_reportHandler
  , readAttributeStateWithClusterStateCache_endpoint_queue_completion
  , readAttributeSupportedWithCompletion
  , subscribeAttributeSupportedWithParams_subscriptionEstablished_reportHandler
  , readAttributeSupportedWithClusterStateCache_endpoint_queue_completion
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
  , modifyEnabledAlarmsWithParams_completionSelector
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
  , readAttributeLatchWithClusterStateCache_endpoint_queue_completionSelector
  , readAttributeLatchWithCompletionSelector
  , readAttributeMaskWithClusterStateCache_endpoint_queue_completionSelector
  , readAttributeMaskWithCompletionSelector
  , readAttributeStateWithClusterStateCache_endpoint_queue_completionSelector
  , readAttributeStateWithCompletionSelector
  , readAttributeSupportedWithClusterStateCache_endpoint_queue_completionSelector
  , readAttributeSupportedWithCompletionSelector
  , resetWithParams_completionSelector
  , subscribeAttributeAcceptedCommandListWithParams_subscriptionEstablished_reportHandlerSelector
  , subscribeAttributeAttributeListWithParams_subscriptionEstablished_reportHandlerSelector
  , subscribeAttributeClusterRevisionWithParams_subscriptionEstablished_reportHandlerSelector
  , subscribeAttributeFeatureMapWithParams_subscriptionEstablished_reportHandlerSelector
  , subscribeAttributeGeneratedCommandListWithParams_subscriptionEstablished_reportHandlerSelector
  , subscribeAttributeLatchWithParams_subscriptionEstablished_reportHandlerSelector
  , subscribeAttributeMaskWithParams_subscriptionEstablished_reportHandlerSelector
  , subscribeAttributeStateWithParams_subscriptionEstablished_reportHandlerSelector
  , subscribeAttributeSupportedWithParams_subscriptionEstablished_reportHandlerSelector


  ) where

import Foreign.Ptr (Ptr, FunPtr)
import Foreign.C.Types

import ObjC.Runtime.Types
import ObjC.Runtime.Message (sendMessage, sendOwnedMessage, sendClassMessage, sendOwnedClassMessage)
import ObjC.Runtime.Selector (mkSelector)
import ObjC.Runtime.Class (getRequiredClass)

import ObjC.Matter.Internal.Classes
import ObjC.Foundation.Internal.Classes

-- | Command Reset
--
-- This command resets active and latched alarms (if possible).
--
-- ObjC selector: @- resetWithParams:completion:@
resetWithParams_completion :: (IsMTRBaseClusterDishwasherAlarm mtrBaseClusterDishwasherAlarm, IsMTRDishwasherAlarmClusterResetParams params) => mtrBaseClusterDishwasherAlarm -> params -> Ptr () -> IO ()
resetWithParams_completion mtrBaseClusterDishwasherAlarm params completion =
  sendMessage mtrBaseClusterDishwasherAlarm resetWithParams_completionSelector (toMTRDishwasherAlarmClusterResetParams params) completion

-- | Command ModifyEnabledAlarms
--
-- This command allows a client to request that an alarm be enabled or suppressed at the server.
--
-- ObjC selector: @- modifyEnabledAlarmsWithParams:completion:@
modifyEnabledAlarmsWithParams_completion :: (IsMTRBaseClusterDishwasherAlarm mtrBaseClusterDishwasherAlarm, IsMTRDishwasherAlarmClusterModifyEnabledAlarmsParams params) => mtrBaseClusterDishwasherAlarm -> params -> Ptr () -> IO ()
modifyEnabledAlarmsWithParams_completion mtrBaseClusterDishwasherAlarm params completion =
  sendMessage mtrBaseClusterDishwasherAlarm modifyEnabledAlarmsWithParams_completionSelector (toMTRDishwasherAlarmClusterModifyEnabledAlarmsParams params) completion

-- | @- readAttributeMaskWithCompletion:@
readAttributeMaskWithCompletion :: IsMTRBaseClusterDishwasherAlarm mtrBaseClusterDishwasherAlarm => mtrBaseClusterDishwasherAlarm -> Ptr () -> IO ()
readAttributeMaskWithCompletion mtrBaseClusterDishwasherAlarm completion =
  sendMessage mtrBaseClusterDishwasherAlarm readAttributeMaskWithCompletionSelector completion

-- | @- subscribeAttributeMaskWithParams:subscriptionEstablished:reportHandler:@
subscribeAttributeMaskWithParams_subscriptionEstablished_reportHandler :: (IsMTRBaseClusterDishwasherAlarm mtrBaseClusterDishwasherAlarm, IsMTRSubscribeParams params) => mtrBaseClusterDishwasherAlarm -> params -> Ptr () -> Ptr () -> IO ()
subscribeAttributeMaskWithParams_subscriptionEstablished_reportHandler mtrBaseClusterDishwasherAlarm params subscriptionEstablished reportHandler =
  sendMessage mtrBaseClusterDishwasherAlarm subscribeAttributeMaskWithParams_subscriptionEstablished_reportHandlerSelector (toMTRSubscribeParams params) subscriptionEstablished reportHandler

-- | @+ readAttributeMaskWithClusterStateCache:endpoint:queue:completion:@
readAttributeMaskWithClusterStateCache_endpoint_queue_completion :: (IsMTRClusterStateCacheContainer clusterStateCacheContainer, IsNSNumber endpoint, IsNSObject queue) => clusterStateCacheContainer -> endpoint -> queue -> Ptr () -> IO ()
readAttributeMaskWithClusterStateCache_endpoint_queue_completion clusterStateCacheContainer endpoint queue completion =
  do
    cls' <- getRequiredClass "MTRBaseClusterDishwasherAlarm"
    sendClassMessage cls' readAttributeMaskWithClusterStateCache_endpoint_queue_completionSelector (toMTRClusterStateCacheContainer clusterStateCacheContainer) (toNSNumber endpoint) (toNSObject queue) completion

-- | @- readAttributeLatchWithCompletion:@
readAttributeLatchWithCompletion :: IsMTRBaseClusterDishwasherAlarm mtrBaseClusterDishwasherAlarm => mtrBaseClusterDishwasherAlarm -> Ptr () -> IO ()
readAttributeLatchWithCompletion mtrBaseClusterDishwasherAlarm completion =
  sendMessage mtrBaseClusterDishwasherAlarm readAttributeLatchWithCompletionSelector completion

-- | @- subscribeAttributeLatchWithParams:subscriptionEstablished:reportHandler:@
subscribeAttributeLatchWithParams_subscriptionEstablished_reportHandler :: (IsMTRBaseClusterDishwasherAlarm mtrBaseClusterDishwasherAlarm, IsMTRSubscribeParams params) => mtrBaseClusterDishwasherAlarm -> params -> Ptr () -> Ptr () -> IO ()
subscribeAttributeLatchWithParams_subscriptionEstablished_reportHandler mtrBaseClusterDishwasherAlarm params subscriptionEstablished reportHandler =
  sendMessage mtrBaseClusterDishwasherAlarm subscribeAttributeLatchWithParams_subscriptionEstablished_reportHandlerSelector (toMTRSubscribeParams params) subscriptionEstablished reportHandler

-- | @+ readAttributeLatchWithClusterStateCache:endpoint:queue:completion:@
readAttributeLatchWithClusterStateCache_endpoint_queue_completion :: (IsMTRClusterStateCacheContainer clusterStateCacheContainer, IsNSNumber endpoint, IsNSObject queue) => clusterStateCacheContainer -> endpoint -> queue -> Ptr () -> IO ()
readAttributeLatchWithClusterStateCache_endpoint_queue_completion clusterStateCacheContainer endpoint queue completion =
  do
    cls' <- getRequiredClass "MTRBaseClusterDishwasherAlarm"
    sendClassMessage cls' readAttributeLatchWithClusterStateCache_endpoint_queue_completionSelector (toMTRClusterStateCacheContainer clusterStateCacheContainer) (toNSNumber endpoint) (toNSObject queue) completion

-- | @- readAttributeStateWithCompletion:@
readAttributeStateWithCompletion :: IsMTRBaseClusterDishwasherAlarm mtrBaseClusterDishwasherAlarm => mtrBaseClusterDishwasherAlarm -> Ptr () -> IO ()
readAttributeStateWithCompletion mtrBaseClusterDishwasherAlarm completion =
  sendMessage mtrBaseClusterDishwasherAlarm readAttributeStateWithCompletionSelector completion

-- | @- subscribeAttributeStateWithParams:subscriptionEstablished:reportHandler:@
subscribeAttributeStateWithParams_subscriptionEstablished_reportHandler :: (IsMTRBaseClusterDishwasherAlarm mtrBaseClusterDishwasherAlarm, IsMTRSubscribeParams params) => mtrBaseClusterDishwasherAlarm -> params -> Ptr () -> Ptr () -> IO ()
subscribeAttributeStateWithParams_subscriptionEstablished_reportHandler mtrBaseClusterDishwasherAlarm params subscriptionEstablished reportHandler =
  sendMessage mtrBaseClusterDishwasherAlarm subscribeAttributeStateWithParams_subscriptionEstablished_reportHandlerSelector (toMTRSubscribeParams params) subscriptionEstablished reportHandler

-- | @+ readAttributeStateWithClusterStateCache:endpoint:queue:completion:@
readAttributeStateWithClusterStateCache_endpoint_queue_completion :: (IsMTRClusterStateCacheContainer clusterStateCacheContainer, IsNSNumber endpoint, IsNSObject queue) => clusterStateCacheContainer -> endpoint -> queue -> Ptr () -> IO ()
readAttributeStateWithClusterStateCache_endpoint_queue_completion clusterStateCacheContainer endpoint queue completion =
  do
    cls' <- getRequiredClass "MTRBaseClusterDishwasherAlarm"
    sendClassMessage cls' readAttributeStateWithClusterStateCache_endpoint_queue_completionSelector (toMTRClusterStateCacheContainer clusterStateCacheContainer) (toNSNumber endpoint) (toNSObject queue) completion

-- | @- readAttributeSupportedWithCompletion:@
readAttributeSupportedWithCompletion :: IsMTRBaseClusterDishwasherAlarm mtrBaseClusterDishwasherAlarm => mtrBaseClusterDishwasherAlarm -> Ptr () -> IO ()
readAttributeSupportedWithCompletion mtrBaseClusterDishwasherAlarm completion =
  sendMessage mtrBaseClusterDishwasherAlarm readAttributeSupportedWithCompletionSelector completion

-- | @- subscribeAttributeSupportedWithParams:subscriptionEstablished:reportHandler:@
subscribeAttributeSupportedWithParams_subscriptionEstablished_reportHandler :: (IsMTRBaseClusterDishwasherAlarm mtrBaseClusterDishwasherAlarm, IsMTRSubscribeParams params) => mtrBaseClusterDishwasherAlarm -> params -> Ptr () -> Ptr () -> IO ()
subscribeAttributeSupportedWithParams_subscriptionEstablished_reportHandler mtrBaseClusterDishwasherAlarm params subscriptionEstablished reportHandler =
  sendMessage mtrBaseClusterDishwasherAlarm subscribeAttributeSupportedWithParams_subscriptionEstablished_reportHandlerSelector (toMTRSubscribeParams params) subscriptionEstablished reportHandler

-- | @+ readAttributeSupportedWithClusterStateCache:endpoint:queue:completion:@
readAttributeSupportedWithClusterStateCache_endpoint_queue_completion :: (IsMTRClusterStateCacheContainer clusterStateCacheContainer, IsNSNumber endpoint, IsNSObject queue) => clusterStateCacheContainer -> endpoint -> queue -> Ptr () -> IO ()
readAttributeSupportedWithClusterStateCache_endpoint_queue_completion clusterStateCacheContainer endpoint queue completion =
  do
    cls' <- getRequiredClass "MTRBaseClusterDishwasherAlarm"
    sendClassMessage cls' readAttributeSupportedWithClusterStateCache_endpoint_queue_completionSelector (toMTRClusterStateCacheContainer clusterStateCacheContainer) (toNSNumber endpoint) (toNSObject queue) completion

-- | @- readAttributeGeneratedCommandListWithCompletion:@
readAttributeGeneratedCommandListWithCompletion :: IsMTRBaseClusterDishwasherAlarm mtrBaseClusterDishwasherAlarm => mtrBaseClusterDishwasherAlarm -> Ptr () -> IO ()
readAttributeGeneratedCommandListWithCompletion mtrBaseClusterDishwasherAlarm completion =
  sendMessage mtrBaseClusterDishwasherAlarm readAttributeGeneratedCommandListWithCompletionSelector completion

-- | @- subscribeAttributeGeneratedCommandListWithParams:subscriptionEstablished:reportHandler:@
subscribeAttributeGeneratedCommandListWithParams_subscriptionEstablished_reportHandler :: (IsMTRBaseClusterDishwasherAlarm mtrBaseClusterDishwasherAlarm, IsMTRSubscribeParams params) => mtrBaseClusterDishwasherAlarm -> params -> Ptr () -> Ptr () -> IO ()
subscribeAttributeGeneratedCommandListWithParams_subscriptionEstablished_reportHandler mtrBaseClusterDishwasherAlarm params subscriptionEstablished reportHandler =
  sendMessage mtrBaseClusterDishwasherAlarm subscribeAttributeGeneratedCommandListWithParams_subscriptionEstablished_reportHandlerSelector (toMTRSubscribeParams params) subscriptionEstablished reportHandler

-- | @+ readAttributeGeneratedCommandListWithClusterStateCache:endpoint:queue:completion:@
readAttributeGeneratedCommandListWithClusterStateCache_endpoint_queue_completion :: (IsMTRClusterStateCacheContainer clusterStateCacheContainer, IsNSNumber endpoint, IsNSObject queue) => clusterStateCacheContainer -> endpoint -> queue -> Ptr () -> IO ()
readAttributeGeneratedCommandListWithClusterStateCache_endpoint_queue_completion clusterStateCacheContainer endpoint queue completion =
  do
    cls' <- getRequiredClass "MTRBaseClusterDishwasherAlarm"
    sendClassMessage cls' readAttributeGeneratedCommandListWithClusterStateCache_endpoint_queue_completionSelector (toMTRClusterStateCacheContainer clusterStateCacheContainer) (toNSNumber endpoint) (toNSObject queue) completion

-- | @- readAttributeAcceptedCommandListWithCompletion:@
readAttributeAcceptedCommandListWithCompletion :: IsMTRBaseClusterDishwasherAlarm mtrBaseClusterDishwasherAlarm => mtrBaseClusterDishwasherAlarm -> Ptr () -> IO ()
readAttributeAcceptedCommandListWithCompletion mtrBaseClusterDishwasherAlarm completion =
  sendMessage mtrBaseClusterDishwasherAlarm readAttributeAcceptedCommandListWithCompletionSelector completion

-- | @- subscribeAttributeAcceptedCommandListWithParams:subscriptionEstablished:reportHandler:@
subscribeAttributeAcceptedCommandListWithParams_subscriptionEstablished_reportHandler :: (IsMTRBaseClusterDishwasherAlarm mtrBaseClusterDishwasherAlarm, IsMTRSubscribeParams params) => mtrBaseClusterDishwasherAlarm -> params -> Ptr () -> Ptr () -> IO ()
subscribeAttributeAcceptedCommandListWithParams_subscriptionEstablished_reportHandler mtrBaseClusterDishwasherAlarm params subscriptionEstablished reportHandler =
  sendMessage mtrBaseClusterDishwasherAlarm subscribeAttributeAcceptedCommandListWithParams_subscriptionEstablished_reportHandlerSelector (toMTRSubscribeParams params) subscriptionEstablished reportHandler

-- | @+ readAttributeAcceptedCommandListWithClusterStateCache:endpoint:queue:completion:@
readAttributeAcceptedCommandListWithClusterStateCache_endpoint_queue_completion :: (IsMTRClusterStateCacheContainer clusterStateCacheContainer, IsNSNumber endpoint, IsNSObject queue) => clusterStateCacheContainer -> endpoint -> queue -> Ptr () -> IO ()
readAttributeAcceptedCommandListWithClusterStateCache_endpoint_queue_completion clusterStateCacheContainer endpoint queue completion =
  do
    cls' <- getRequiredClass "MTRBaseClusterDishwasherAlarm"
    sendClassMessage cls' readAttributeAcceptedCommandListWithClusterStateCache_endpoint_queue_completionSelector (toMTRClusterStateCacheContainer clusterStateCacheContainer) (toNSNumber endpoint) (toNSObject queue) completion

-- | @- readAttributeAttributeListWithCompletion:@
readAttributeAttributeListWithCompletion :: IsMTRBaseClusterDishwasherAlarm mtrBaseClusterDishwasherAlarm => mtrBaseClusterDishwasherAlarm -> Ptr () -> IO ()
readAttributeAttributeListWithCompletion mtrBaseClusterDishwasherAlarm completion =
  sendMessage mtrBaseClusterDishwasherAlarm readAttributeAttributeListWithCompletionSelector completion

-- | @- subscribeAttributeAttributeListWithParams:subscriptionEstablished:reportHandler:@
subscribeAttributeAttributeListWithParams_subscriptionEstablished_reportHandler :: (IsMTRBaseClusterDishwasherAlarm mtrBaseClusterDishwasherAlarm, IsMTRSubscribeParams params) => mtrBaseClusterDishwasherAlarm -> params -> Ptr () -> Ptr () -> IO ()
subscribeAttributeAttributeListWithParams_subscriptionEstablished_reportHandler mtrBaseClusterDishwasherAlarm params subscriptionEstablished reportHandler =
  sendMessage mtrBaseClusterDishwasherAlarm subscribeAttributeAttributeListWithParams_subscriptionEstablished_reportHandlerSelector (toMTRSubscribeParams params) subscriptionEstablished reportHandler

-- | @+ readAttributeAttributeListWithClusterStateCache:endpoint:queue:completion:@
readAttributeAttributeListWithClusterStateCache_endpoint_queue_completion :: (IsMTRClusterStateCacheContainer clusterStateCacheContainer, IsNSNumber endpoint, IsNSObject queue) => clusterStateCacheContainer -> endpoint -> queue -> Ptr () -> IO ()
readAttributeAttributeListWithClusterStateCache_endpoint_queue_completion clusterStateCacheContainer endpoint queue completion =
  do
    cls' <- getRequiredClass "MTRBaseClusterDishwasherAlarm"
    sendClassMessage cls' readAttributeAttributeListWithClusterStateCache_endpoint_queue_completionSelector (toMTRClusterStateCacheContainer clusterStateCacheContainer) (toNSNumber endpoint) (toNSObject queue) completion

-- | @- readAttributeFeatureMapWithCompletion:@
readAttributeFeatureMapWithCompletion :: IsMTRBaseClusterDishwasherAlarm mtrBaseClusterDishwasherAlarm => mtrBaseClusterDishwasherAlarm -> Ptr () -> IO ()
readAttributeFeatureMapWithCompletion mtrBaseClusterDishwasherAlarm completion =
  sendMessage mtrBaseClusterDishwasherAlarm readAttributeFeatureMapWithCompletionSelector completion

-- | @- subscribeAttributeFeatureMapWithParams:subscriptionEstablished:reportHandler:@
subscribeAttributeFeatureMapWithParams_subscriptionEstablished_reportHandler :: (IsMTRBaseClusterDishwasherAlarm mtrBaseClusterDishwasherAlarm, IsMTRSubscribeParams params) => mtrBaseClusterDishwasherAlarm -> params -> Ptr () -> Ptr () -> IO ()
subscribeAttributeFeatureMapWithParams_subscriptionEstablished_reportHandler mtrBaseClusterDishwasherAlarm params subscriptionEstablished reportHandler =
  sendMessage mtrBaseClusterDishwasherAlarm subscribeAttributeFeatureMapWithParams_subscriptionEstablished_reportHandlerSelector (toMTRSubscribeParams params) subscriptionEstablished reportHandler

-- | @+ readAttributeFeatureMapWithClusterStateCache:endpoint:queue:completion:@
readAttributeFeatureMapWithClusterStateCache_endpoint_queue_completion :: (IsMTRClusterStateCacheContainer clusterStateCacheContainer, IsNSNumber endpoint, IsNSObject queue) => clusterStateCacheContainer -> endpoint -> queue -> Ptr () -> IO ()
readAttributeFeatureMapWithClusterStateCache_endpoint_queue_completion clusterStateCacheContainer endpoint queue completion =
  do
    cls' <- getRequiredClass "MTRBaseClusterDishwasherAlarm"
    sendClassMessage cls' readAttributeFeatureMapWithClusterStateCache_endpoint_queue_completionSelector (toMTRClusterStateCacheContainer clusterStateCacheContainer) (toNSNumber endpoint) (toNSObject queue) completion

-- | @- readAttributeClusterRevisionWithCompletion:@
readAttributeClusterRevisionWithCompletion :: IsMTRBaseClusterDishwasherAlarm mtrBaseClusterDishwasherAlarm => mtrBaseClusterDishwasherAlarm -> Ptr () -> IO ()
readAttributeClusterRevisionWithCompletion mtrBaseClusterDishwasherAlarm completion =
  sendMessage mtrBaseClusterDishwasherAlarm readAttributeClusterRevisionWithCompletionSelector completion

-- | @- subscribeAttributeClusterRevisionWithParams:subscriptionEstablished:reportHandler:@
subscribeAttributeClusterRevisionWithParams_subscriptionEstablished_reportHandler :: (IsMTRBaseClusterDishwasherAlarm mtrBaseClusterDishwasherAlarm, IsMTRSubscribeParams params) => mtrBaseClusterDishwasherAlarm -> params -> Ptr () -> Ptr () -> IO ()
subscribeAttributeClusterRevisionWithParams_subscriptionEstablished_reportHandler mtrBaseClusterDishwasherAlarm params subscriptionEstablished reportHandler =
  sendMessage mtrBaseClusterDishwasherAlarm subscribeAttributeClusterRevisionWithParams_subscriptionEstablished_reportHandlerSelector (toMTRSubscribeParams params) subscriptionEstablished reportHandler

-- | @+ readAttributeClusterRevisionWithClusterStateCache:endpoint:queue:completion:@
readAttributeClusterRevisionWithClusterStateCache_endpoint_queue_completion :: (IsMTRClusterStateCacheContainer clusterStateCacheContainer, IsNSNumber endpoint, IsNSObject queue) => clusterStateCacheContainer -> endpoint -> queue -> Ptr () -> IO ()
readAttributeClusterRevisionWithClusterStateCache_endpoint_queue_completion clusterStateCacheContainer endpoint queue completion =
  do
    cls' <- getRequiredClass "MTRBaseClusterDishwasherAlarm"
    sendClassMessage cls' readAttributeClusterRevisionWithClusterStateCache_endpoint_queue_completionSelector (toMTRClusterStateCacheContainer clusterStateCacheContainer) (toNSNumber endpoint) (toNSObject queue) completion

-- | @- init@
init_ :: IsMTRBaseClusterDishwasherAlarm mtrBaseClusterDishwasherAlarm => mtrBaseClusterDishwasherAlarm -> IO (Id MTRBaseClusterDishwasherAlarm)
init_ mtrBaseClusterDishwasherAlarm =
  sendOwnedMessage mtrBaseClusterDishwasherAlarm initSelector

-- | @+ new@
new :: IO (Id MTRBaseClusterDishwasherAlarm)
new  =
  do
    cls' <- getRequiredClass "MTRBaseClusterDishwasherAlarm"
    sendOwnedClassMessage cls' newSelector

-- | For all instance methods (reads, writes, commands) that take a completion, the completion will be called on the provided queue.
--
-- ObjC selector: @- initWithDevice:endpointID:queue:@
initWithDevice_endpointID_queue :: (IsMTRBaseClusterDishwasherAlarm mtrBaseClusterDishwasherAlarm, IsMTRBaseDevice device, IsNSNumber endpointID, IsNSObject queue) => mtrBaseClusterDishwasherAlarm -> device -> endpointID -> queue -> IO (Id MTRBaseClusterDishwasherAlarm)
initWithDevice_endpointID_queue mtrBaseClusterDishwasherAlarm device endpointID queue =
  sendOwnedMessage mtrBaseClusterDishwasherAlarm initWithDevice_endpointID_queueSelector (toMTRBaseDevice device) (toNSNumber endpointID) (toNSObject queue)

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @resetWithParams:completion:@
resetWithParams_completionSelector :: Selector '[Id MTRDishwasherAlarmClusterResetParams, Ptr ()] ()
resetWithParams_completionSelector = mkSelector "resetWithParams:completion:"

-- | @Selector@ for @modifyEnabledAlarmsWithParams:completion:@
modifyEnabledAlarmsWithParams_completionSelector :: Selector '[Id MTRDishwasherAlarmClusterModifyEnabledAlarmsParams, Ptr ()] ()
modifyEnabledAlarmsWithParams_completionSelector = mkSelector "modifyEnabledAlarmsWithParams:completion:"

-- | @Selector@ for @readAttributeMaskWithCompletion:@
readAttributeMaskWithCompletionSelector :: Selector '[Ptr ()] ()
readAttributeMaskWithCompletionSelector = mkSelector "readAttributeMaskWithCompletion:"

-- | @Selector@ for @subscribeAttributeMaskWithParams:subscriptionEstablished:reportHandler:@
subscribeAttributeMaskWithParams_subscriptionEstablished_reportHandlerSelector :: Selector '[Id MTRSubscribeParams, Ptr (), Ptr ()] ()
subscribeAttributeMaskWithParams_subscriptionEstablished_reportHandlerSelector = mkSelector "subscribeAttributeMaskWithParams:subscriptionEstablished:reportHandler:"

-- | @Selector@ for @readAttributeMaskWithClusterStateCache:endpoint:queue:completion:@
readAttributeMaskWithClusterStateCache_endpoint_queue_completionSelector :: Selector '[Id MTRClusterStateCacheContainer, Id NSNumber, Id NSObject, Ptr ()] ()
readAttributeMaskWithClusterStateCache_endpoint_queue_completionSelector = mkSelector "readAttributeMaskWithClusterStateCache:endpoint:queue:completion:"

-- | @Selector@ for @readAttributeLatchWithCompletion:@
readAttributeLatchWithCompletionSelector :: Selector '[Ptr ()] ()
readAttributeLatchWithCompletionSelector = mkSelector "readAttributeLatchWithCompletion:"

-- | @Selector@ for @subscribeAttributeLatchWithParams:subscriptionEstablished:reportHandler:@
subscribeAttributeLatchWithParams_subscriptionEstablished_reportHandlerSelector :: Selector '[Id MTRSubscribeParams, Ptr (), Ptr ()] ()
subscribeAttributeLatchWithParams_subscriptionEstablished_reportHandlerSelector = mkSelector "subscribeAttributeLatchWithParams:subscriptionEstablished:reportHandler:"

-- | @Selector@ for @readAttributeLatchWithClusterStateCache:endpoint:queue:completion:@
readAttributeLatchWithClusterStateCache_endpoint_queue_completionSelector :: Selector '[Id MTRClusterStateCacheContainer, Id NSNumber, Id NSObject, Ptr ()] ()
readAttributeLatchWithClusterStateCache_endpoint_queue_completionSelector = mkSelector "readAttributeLatchWithClusterStateCache:endpoint:queue:completion:"

-- | @Selector@ for @readAttributeStateWithCompletion:@
readAttributeStateWithCompletionSelector :: Selector '[Ptr ()] ()
readAttributeStateWithCompletionSelector = mkSelector "readAttributeStateWithCompletion:"

-- | @Selector@ for @subscribeAttributeStateWithParams:subscriptionEstablished:reportHandler:@
subscribeAttributeStateWithParams_subscriptionEstablished_reportHandlerSelector :: Selector '[Id MTRSubscribeParams, Ptr (), Ptr ()] ()
subscribeAttributeStateWithParams_subscriptionEstablished_reportHandlerSelector = mkSelector "subscribeAttributeStateWithParams:subscriptionEstablished:reportHandler:"

-- | @Selector@ for @readAttributeStateWithClusterStateCache:endpoint:queue:completion:@
readAttributeStateWithClusterStateCache_endpoint_queue_completionSelector :: Selector '[Id MTRClusterStateCacheContainer, Id NSNumber, Id NSObject, Ptr ()] ()
readAttributeStateWithClusterStateCache_endpoint_queue_completionSelector = mkSelector "readAttributeStateWithClusterStateCache:endpoint:queue:completion:"

-- | @Selector@ for @readAttributeSupportedWithCompletion:@
readAttributeSupportedWithCompletionSelector :: Selector '[Ptr ()] ()
readAttributeSupportedWithCompletionSelector = mkSelector "readAttributeSupportedWithCompletion:"

-- | @Selector@ for @subscribeAttributeSupportedWithParams:subscriptionEstablished:reportHandler:@
subscribeAttributeSupportedWithParams_subscriptionEstablished_reportHandlerSelector :: Selector '[Id MTRSubscribeParams, Ptr (), Ptr ()] ()
subscribeAttributeSupportedWithParams_subscriptionEstablished_reportHandlerSelector = mkSelector "subscribeAttributeSupportedWithParams:subscriptionEstablished:reportHandler:"

-- | @Selector@ for @readAttributeSupportedWithClusterStateCache:endpoint:queue:completion:@
readAttributeSupportedWithClusterStateCache_endpoint_queue_completionSelector :: Selector '[Id MTRClusterStateCacheContainer, Id NSNumber, Id NSObject, Ptr ()] ()
readAttributeSupportedWithClusterStateCache_endpoint_queue_completionSelector = mkSelector "readAttributeSupportedWithClusterStateCache:endpoint:queue:completion:"

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
initSelector :: Selector '[] (Id MTRBaseClusterDishwasherAlarm)
initSelector = mkSelector "init"

-- | @Selector@ for @new@
newSelector :: Selector '[] (Id MTRBaseClusterDishwasherAlarm)
newSelector = mkSelector "new"

-- | @Selector@ for @initWithDevice:endpointID:queue:@
initWithDevice_endpointID_queueSelector :: Selector '[Id MTRBaseDevice, Id NSNumber, Id NSObject] (Id MTRBaseClusterDishwasherAlarm)
initWithDevice_endpointID_queueSelector = mkSelector "initWithDevice:endpointID:queue:"

