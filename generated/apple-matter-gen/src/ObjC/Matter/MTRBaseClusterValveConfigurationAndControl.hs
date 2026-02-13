{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Cluster Valve Configuration and Control
--
-- This cluster is used to configure a valve.
--
-- Generated bindings for @MTRBaseClusterValveConfigurationAndControl@.
module ObjC.Matter.MTRBaseClusterValveConfigurationAndControl
  ( MTRBaseClusterValveConfigurationAndControl
  , IsMTRBaseClusterValveConfigurationAndControl(..)
  , openWithParams_completion
  , openWithCompletion
  , closeWithParams_completion
  , closeWithCompletion
  , readAttributeOpenDurationWithCompletion
  , subscribeAttributeOpenDurationWithParams_subscriptionEstablished_reportHandler
  , readAttributeOpenDurationWithClusterStateCache_endpoint_queue_completion
  , readAttributeDefaultOpenDurationWithCompletion
  , writeAttributeDefaultOpenDurationWithValue_completion
  , writeAttributeDefaultOpenDurationWithValue_params_completion
  , subscribeAttributeDefaultOpenDurationWithParams_subscriptionEstablished_reportHandler
  , readAttributeDefaultOpenDurationWithClusterStateCache_endpoint_queue_completion
  , readAttributeAutoCloseTimeWithCompletion
  , subscribeAttributeAutoCloseTimeWithParams_subscriptionEstablished_reportHandler
  , readAttributeAutoCloseTimeWithClusterStateCache_endpoint_queue_completion
  , readAttributeRemainingDurationWithCompletion
  , subscribeAttributeRemainingDurationWithParams_subscriptionEstablished_reportHandler
  , readAttributeRemainingDurationWithClusterStateCache_endpoint_queue_completion
  , readAttributeCurrentStateWithCompletion
  , subscribeAttributeCurrentStateWithParams_subscriptionEstablished_reportHandler
  , readAttributeCurrentStateWithClusterStateCache_endpoint_queue_completion
  , readAttributeTargetStateWithCompletion
  , subscribeAttributeTargetStateWithParams_subscriptionEstablished_reportHandler
  , readAttributeTargetStateWithClusterStateCache_endpoint_queue_completion
  , readAttributeCurrentLevelWithCompletion
  , subscribeAttributeCurrentLevelWithParams_subscriptionEstablished_reportHandler
  , readAttributeCurrentLevelWithClusterStateCache_endpoint_queue_completion
  , readAttributeTargetLevelWithCompletion
  , subscribeAttributeTargetLevelWithParams_subscriptionEstablished_reportHandler
  , readAttributeTargetLevelWithClusterStateCache_endpoint_queue_completion
  , readAttributeDefaultOpenLevelWithCompletion
  , writeAttributeDefaultOpenLevelWithValue_completion
  , writeAttributeDefaultOpenLevelWithValue_params_completion
  , subscribeAttributeDefaultOpenLevelWithParams_subscriptionEstablished_reportHandler
  , readAttributeDefaultOpenLevelWithClusterStateCache_endpoint_queue_completion
  , readAttributeValveFaultWithCompletion
  , subscribeAttributeValveFaultWithParams_subscriptionEstablished_reportHandler
  , readAttributeValveFaultWithClusterStateCache_endpoint_queue_completion
  , readAttributeLevelStepWithCompletion
  , subscribeAttributeLevelStepWithParams_subscriptionEstablished_reportHandler
  , readAttributeLevelStepWithClusterStateCache_endpoint_queue_completion
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
  , closeWithCompletionSelector
  , closeWithParams_completionSelector
  , initSelector
  , initWithDevice_endpointID_queueSelector
  , newSelector
  , openWithCompletionSelector
  , openWithParams_completionSelector
  , readAttributeAcceptedCommandListWithClusterStateCache_endpoint_queue_completionSelector
  , readAttributeAcceptedCommandListWithCompletionSelector
  , readAttributeAttributeListWithClusterStateCache_endpoint_queue_completionSelector
  , readAttributeAttributeListWithCompletionSelector
  , readAttributeAutoCloseTimeWithClusterStateCache_endpoint_queue_completionSelector
  , readAttributeAutoCloseTimeWithCompletionSelector
  , readAttributeClusterRevisionWithClusterStateCache_endpoint_queue_completionSelector
  , readAttributeClusterRevisionWithCompletionSelector
  , readAttributeCurrentLevelWithClusterStateCache_endpoint_queue_completionSelector
  , readAttributeCurrentLevelWithCompletionSelector
  , readAttributeCurrentStateWithClusterStateCache_endpoint_queue_completionSelector
  , readAttributeCurrentStateWithCompletionSelector
  , readAttributeDefaultOpenDurationWithClusterStateCache_endpoint_queue_completionSelector
  , readAttributeDefaultOpenDurationWithCompletionSelector
  , readAttributeDefaultOpenLevelWithClusterStateCache_endpoint_queue_completionSelector
  , readAttributeDefaultOpenLevelWithCompletionSelector
  , readAttributeFeatureMapWithClusterStateCache_endpoint_queue_completionSelector
  , readAttributeFeatureMapWithCompletionSelector
  , readAttributeGeneratedCommandListWithClusterStateCache_endpoint_queue_completionSelector
  , readAttributeGeneratedCommandListWithCompletionSelector
  , readAttributeLevelStepWithClusterStateCache_endpoint_queue_completionSelector
  , readAttributeLevelStepWithCompletionSelector
  , readAttributeOpenDurationWithClusterStateCache_endpoint_queue_completionSelector
  , readAttributeOpenDurationWithCompletionSelector
  , readAttributeRemainingDurationWithClusterStateCache_endpoint_queue_completionSelector
  , readAttributeRemainingDurationWithCompletionSelector
  , readAttributeTargetLevelWithClusterStateCache_endpoint_queue_completionSelector
  , readAttributeTargetLevelWithCompletionSelector
  , readAttributeTargetStateWithClusterStateCache_endpoint_queue_completionSelector
  , readAttributeTargetStateWithCompletionSelector
  , readAttributeValveFaultWithClusterStateCache_endpoint_queue_completionSelector
  , readAttributeValveFaultWithCompletionSelector
  , subscribeAttributeAcceptedCommandListWithParams_subscriptionEstablished_reportHandlerSelector
  , subscribeAttributeAttributeListWithParams_subscriptionEstablished_reportHandlerSelector
  , subscribeAttributeAutoCloseTimeWithParams_subscriptionEstablished_reportHandlerSelector
  , subscribeAttributeClusterRevisionWithParams_subscriptionEstablished_reportHandlerSelector
  , subscribeAttributeCurrentLevelWithParams_subscriptionEstablished_reportHandlerSelector
  , subscribeAttributeCurrentStateWithParams_subscriptionEstablished_reportHandlerSelector
  , subscribeAttributeDefaultOpenDurationWithParams_subscriptionEstablished_reportHandlerSelector
  , subscribeAttributeDefaultOpenLevelWithParams_subscriptionEstablished_reportHandlerSelector
  , subscribeAttributeFeatureMapWithParams_subscriptionEstablished_reportHandlerSelector
  , subscribeAttributeGeneratedCommandListWithParams_subscriptionEstablished_reportHandlerSelector
  , subscribeAttributeLevelStepWithParams_subscriptionEstablished_reportHandlerSelector
  , subscribeAttributeOpenDurationWithParams_subscriptionEstablished_reportHandlerSelector
  , subscribeAttributeRemainingDurationWithParams_subscriptionEstablished_reportHandlerSelector
  , subscribeAttributeTargetLevelWithParams_subscriptionEstablished_reportHandlerSelector
  , subscribeAttributeTargetStateWithParams_subscriptionEstablished_reportHandlerSelector
  , subscribeAttributeValveFaultWithParams_subscriptionEstablished_reportHandlerSelector
  , writeAttributeDefaultOpenDurationWithValue_completionSelector
  , writeAttributeDefaultOpenDurationWithValue_params_completionSelector
  , writeAttributeDefaultOpenLevelWithValue_completionSelector
  , writeAttributeDefaultOpenLevelWithValue_params_completionSelector


  ) where

import Foreign.Ptr (Ptr, FunPtr)
import Foreign.C.Types

import ObjC.Runtime.Types
import ObjC.Runtime.Message (sendMessage, sendOwnedMessage, sendClassMessage, sendOwnedClassMessage)
import ObjC.Runtime.Selector (mkSelector)
import ObjC.Runtime.Class (getRequiredClass)

import ObjC.Matter.Internal.Classes
import ObjC.Foundation.Internal.Classes

-- | Command Open
--
-- This command is used to set the valve to its open position.
--
-- ObjC selector: @- openWithParams:completion:@
openWithParams_completion :: (IsMTRBaseClusterValveConfigurationAndControl mtrBaseClusterValveConfigurationAndControl, IsMTRValveConfigurationAndControlClusterOpenParams params) => mtrBaseClusterValveConfigurationAndControl -> params -> Ptr () -> IO ()
openWithParams_completion mtrBaseClusterValveConfigurationAndControl params completion =
  sendMessage mtrBaseClusterValveConfigurationAndControl openWithParams_completionSelector (toMTRValveConfigurationAndControlClusterOpenParams params) completion

-- | @- openWithCompletion:@
openWithCompletion :: IsMTRBaseClusterValveConfigurationAndControl mtrBaseClusterValveConfigurationAndControl => mtrBaseClusterValveConfigurationAndControl -> Ptr () -> IO ()
openWithCompletion mtrBaseClusterValveConfigurationAndControl completion =
  sendMessage mtrBaseClusterValveConfigurationAndControl openWithCompletionSelector completion

-- | Command Close
--
-- This command is used to set the valve to its closed position.
--
-- ObjC selector: @- closeWithParams:completion:@
closeWithParams_completion :: (IsMTRBaseClusterValveConfigurationAndControl mtrBaseClusterValveConfigurationAndControl, IsMTRValveConfigurationAndControlClusterCloseParams params) => mtrBaseClusterValveConfigurationAndControl -> params -> Ptr () -> IO ()
closeWithParams_completion mtrBaseClusterValveConfigurationAndControl params completion =
  sendMessage mtrBaseClusterValveConfigurationAndControl closeWithParams_completionSelector (toMTRValveConfigurationAndControlClusterCloseParams params) completion

-- | @- closeWithCompletion:@
closeWithCompletion :: IsMTRBaseClusterValveConfigurationAndControl mtrBaseClusterValveConfigurationAndControl => mtrBaseClusterValveConfigurationAndControl -> Ptr () -> IO ()
closeWithCompletion mtrBaseClusterValveConfigurationAndControl completion =
  sendMessage mtrBaseClusterValveConfigurationAndControl closeWithCompletionSelector completion

-- | @- readAttributeOpenDurationWithCompletion:@
readAttributeOpenDurationWithCompletion :: IsMTRBaseClusterValveConfigurationAndControl mtrBaseClusterValveConfigurationAndControl => mtrBaseClusterValveConfigurationAndControl -> Ptr () -> IO ()
readAttributeOpenDurationWithCompletion mtrBaseClusterValveConfigurationAndControl completion =
  sendMessage mtrBaseClusterValveConfigurationAndControl readAttributeOpenDurationWithCompletionSelector completion

-- | @- subscribeAttributeOpenDurationWithParams:subscriptionEstablished:reportHandler:@
subscribeAttributeOpenDurationWithParams_subscriptionEstablished_reportHandler :: (IsMTRBaseClusterValveConfigurationAndControl mtrBaseClusterValveConfigurationAndControl, IsMTRSubscribeParams params) => mtrBaseClusterValveConfigurationAndControl -> params -> Ptr () -> Ptr () -> IO ()
subscribeAttributeOpenDurationWithParams_subscriptionEstablished_reportHandler mtrBaseClusterValveConfigurationAndControl params subscriptionEstablished reportHandler =
  sendMessage mtrBaseClusterValveConfigurationAndControl subscribeAttributeOpenDurationWithParams_subscriptionEstablished_reportHandlerSelector (toMTRSubscribeParams params) subscriptionEstablished reportHandler

-- | @+ readAttributeOpenDurationWithClusterStateCache:endpoint:queue:completion:@
readAttributeOpenDurationWithClusterStateCache_endpoint_queue_completion :: (IsMTRClusterStateCacheContainer clusterStateCacheContainer, IsNSNumber endpoint, IsNSObject queue) => clusterStateCacheContainer -> endpoint -> queue -> Ptr () -> IO ()
readAttributeOpenDurationWithClusterStateCache_endpoint_queue_completion clusterStateCacheContainer endpoint queue completion =
  do
    cls' <- getRequiredClass "MTRBaseClusterValveConfigurationAndControl"
    sendClassMessage cls' readAttributeOpenDurationWithClusterStateCache_endpoint_queue_completionSelector (toMTRClusterStateCacheContainer clusterStateCacheContainer) (toNSNumber endpoint) (toNSObject queue) completion

-- | @- readAttributeDefaultOpenDurationWithCompletion:@
readAttributeDefaultOpenDurationWithCompletion :: IsMTRBaseClusterValveConfigurationAndControl mtrBaseClusterValveConfigurationAndControl => mtrBaseClusterValveConfigurationAndControl -> Ptr () -> IO ()
readAttributeDefaultOpenDurationWithCompletion mtrBaseClusterValveConfigurationAndControl completion =
  sendMessage mtrBaseClusterValveConfigurationAndControl readAttributeDefaultOpenDurationWithCompletionSelector completion

-- | @- writeAttributeDefaultOpenDurationWithValue:completion:@
writeAttributeDefaultOpenDurationWithValue_completion :: (IsMTRBaseClusterValveConfigurationAndControl mtrBaseClusterValveConfigurationAndControl, IsNSNumber value) => mtrBaseClusterValveConfigurationAndControl -> value -> Ptr () -> IO ()
writeAttributeDefaultOpenDurationWithValue_completion mtrBaseClusterValveConfigurationAndControl value completion =
  sendMessage mtrBaseClusterValveConfigurationAndControl writeAttributeDefaultOpenDurationWithValue_completionSelector (toNSNumber value) completion

-- | @- writeAttributeDefaultOpenDurationWithValue:params:completion:@
writeAttributeDefaultOpenDurationWithValue_params_completion :: (IsMTRBaseClusterValveConfigurationAndControl mtrBaseClusterValveConfigurationAndControl, IsNSNumber value, IsMTRWriteParams params) => mtrBaseClusterValveConfigurationAndControl -> value -> params -> Ptr () -> IO ()
writeAttributeDefaultOpenDurationWithValue_params_completion mtrBaseClusterValveConfigurationAndControl value params completion =
  sendMessage mtrBaseClusterValveConfigurationAndControl writeAttributeDefaultOpenDurationWithValue_params_completionSelector (toNSNumber value) (toMTRWriteParams params) completion

-- | @- subscribeAttributeDefaultOpenDurationWithParams:subscriptionEstablished:reportHandler:@
subscribeAttributeDefaultOpenDurationWithParams_subscriptionEstablished_reportHandler :: (IsMTRBaseClusterValveConfigurationAndControl mtrBaseClusterValveConfigurationAndControl, IsMTRSubscribeParams params) => mtrBaseClusterValveConfigurationAndControl -> params -> Ptr () -> Ptr () -> IO ()
subscribeAttributeDefaultOpenDurationWithParams_subscriptionEstablished_reportHandler mtrBaseClusterValveConfigurationAndControl params subscriptionEstablished reportHandler =
  sendMessage mtrBaseClusterValveConfigurationAndControl subscribeAttributeDefaultOpenDurationWithParams_subscriptionEstablished_reportHandlerSelector (toMTRSubscribeParams params) subscriptionEstablished reportHandler

-- | @+ readAttributeDefaultOpenDurationWithClusterStateCache:endpoint:queue:completion:@
readAttributeDefaultOpenDurationWithClusterStateCache_endpoint_queue_completion :: (IsMTRClusterStateCacheContainer clusterStateCacheContainer, IsNSNumber endpoint, IsNSObject queue) => clusterStateCacheContainer -> endpoint -> queue -> Ptr () -> IO ()
readAttributeDefaultOpenDurationWithClusterStateCache_endpoint_queue_completion clusterStateCacheContainer endpoint queue completion =
  do
    cls' <- getRequiredClass "MTRBaseClusterValveConfigurationAndControl"
    sendClassMessage cls' readAttributeDefaultOpenDurationWithClusterStateCache_endpoint_queue_completionSelector (toMTRClusterStateCacheContainer clusterStateCacheContainer) (toNSNumber endpoint) (toNSObject queue) completion

-- | @- readAttributeAutoCloseTimeWithCompletion:@
readAttributeAutoCloseTimeWithCompletion :: IsMTRBaseClusterValveConfigurationAndControl mtrBaseClusterValveConfigurationAndControl => mtrBaseClusterValveConfigurationAndControl -> Ptr () -> IO ()
readAttributeAutoCloseTimeWithCompletion mtrBaseClusterValveConfigurationAndControl completion =
  sendMessage mtrBaseClusterValveConfigurationAndControl readAttributeAutoCloseTimeWithCompletionSelector completion

-- | @- subscribeAttributeAutoCloseTimeWithParams:subscriptionEstablished:reportHandler:@
subscribeAttributeAutoCloseTimeWithParams_subscriptionEstablished_reportHandler :: (IsMTRBaseClusterValveConfigurationAndControl mtrBaseClusterValveConfigurationAndControl, IsMTRSubscribeParams params) => mtrBaseClusterValveConfigurationAndControl -> params -> Ptr () -> Ptr () -> IO ()
subscribeAttributeAutoCloseTimeWithParams_subscriptionEstablished_reportHandler mtrBaseClusterValveConfigurationAndControl params subscriptionEstablished reportHandler =
  sendMessage mtrBaseClusterValveConfigurationAndControl subscribeAttributeAutoCloseTimeWithParams_subscriptionEstablished_reportHandlerSelector (toMTRSubscribeParams params) subscriptionEstablished reportHandler

-- | @+ readAttributeAutoCloseTimeWithClusterStateCache:endpoint:queue:completion:@
readAttributeAutoCloseTimeWithClusterStateCache_endpoint_queue_completion :: (IsMTRClusterStateCacheContainer clusterStateCacheContainer, IsNSNumber endpoint, IsNSObject queue) => clusterStateCacheContainer -> endpoint -> queue -> Ptr () -> IO ()
readAttributeAutoCloseTimeWithClusterStateCache_endpoint_queue_completion clusterStateCacheContainer endpoint queue completion =
  do
    cls' <- getRequiredClass "MTRBaseClusterValveConfigurationAndControl"
    sendClassMessage cls' readAttributeAutoCloseTimeWithClusterStateCache_endpoint_queue_completionSelector (toMTRClusterStateCacheContainer clusterStateCacheContainer) (toNSNumber endpoint) (toNSObject queue) completion

-- | @- readAttributeRemainingDurationWithCompletion:@
readAttributeRemainingDurationWithCompletion :: IsMTRBaseClusterValveConfigurationAndControl mtrBaseClusterValveConfigurationAndControl => mtrBaseClusterValveConfigurationAndControl -> Ptr () -> IO ()
readAttributeRemainingDurationWithCompletion mtrBaseClusterValveConfigurationAndControl completion =
  sendMessage mtrBaseClusterValveConfigurationAndControl readAttributeRemainingDurationWithCompletionSelector completion

-- | @- subscribeAttributeRemainingDurationWithParams:subscriptionEstablished:reportHandler:@
subscribeAttributeRemainingDurationWithParams_subscriptionEstablished_reportHandler :: (IsMTRBaseClusterValveConfigurationAndControl mtrBaseClusterValveConfigurationAndControl, IsMTRSubscribeParams params) => mtrBaseClusterValveConfigurationAndControl -> params -> Ptr () -> Ptr () -> IO ()
subscribeAttributeRemainingDurationWithParams_subscriptionEstablished_reportHandler mtrBaseClusterValveConfigurationAndControl params subscriptionEstablished reportHandler =
  sendMessage mtrBaseClusterValveConfigurationAndControl subscribeAttributeRemainingDurationWithParams_subscriptionEstablished_reportHandlerSelector (toMTRSubscribeParams params) subscriptionEstablished reportHandler

-- | @+ readAttributeRemainingDurationWithClusterStateCache:endpoint:queue:completion:@
readAttributeRemainingDurationWithClusterStateCache_endpoint_queue_completion :: (IsMTRClusterStateCacheContainer clusterStateCacheContainer, IsNSNumber endpoint, IsNSObject queue) => clusterStateCacheContainer -> endpoint -> queue -> Ptr () -> IO ()
readAttributeRemainingDurationWithClusterStateCache_endpoint_queue_completion clusterStateCacheContainer endpoint queue completion =
  do
    cls' <- getRequiredClass "MTRBaseClusterValveConfigurationAndControl"
    sendClassMessage cls' readAttributeRemainingDurationWithClusterStateCache_endpoint_queue_completionSelector (toMTRClusterStateCacheContainer clusterStateCacheContainer) (toNSNumber endpoint) (toNSObject queue) completion

-- | @- readAttributeCurrentStateWithCompletion:@
readAttributeCurrentStateWithCompletion :: IsMTRBaseClusterValveConfigurationAndControl mtrBaseClusterValveConfigurationAndControl => mtrBaseClusterValveConfigurationAndControl -> Ptr () -> IO ()
readAttributeCurrentStateWithCompletion mtrBaseClusterValveConfigurationAndControl completion =
  sendMessage mtrBaseClusterValveConfigurationAndControl readAttributeCurrentStateWithCompletionSelector completion

-- | @- subscribeAttributeCurrentStateWithParams:subscriptionEstablished:reportHandler:@
subscribeAttributeCurrentStateWithParams_subscriptionEstablished_reportHandler :: (IsMTRBaseClusterValveConfigurationAndControl mtrBaseClusterValveConfigurationAndControl, IsMTRSubscribeParams params) => mtrBaseClusterValveConfigurationAndControl -> params -> Ptr () -> Ptr () -> IO ()
subscribeAttributeCurrentStateWithParams_subscriptionEstablished_reportHandler mtrBaseClusterValveConfigurationAndControl params subscriptionEstablished reportHandler =
  sendMessage mtrBaseClusterValveConfigurationAndControl subscribeAttributeCurrentStateWithParams_subscriptionEstablished_reportHandlerSelector (toMTRSubscribeParams params) subscriptionEstablished reportHandler

-- | @+ readAttributeCurrentStateWithClusterStateCache:endpoint:queue:completion:@
readAttributeCurrentStateWithClusterStateCache_endpoint_queue_completion :: (IsMTRClusterStateCacheContainer clusterStateCacheContainer, IsNSNumber endpoint, IsNSObject queue) => clusterStateCacheContainer -> endpoint -> queue -> Ptr () -> IO ()
readAttributeCurrentStateWithClusterStateCache_endpoint_queue_completion clusterStateCacheContainer endpoint queue completion =
  do
    cls' <- getRequiredClass "MTRBaseClusterValveConfigurationAndControl"
    sendClassMessage cls' readAttributeCurrentStateWithClusterStateCache_endpoint_queue_completionSelector (toMTRClusterStateCacheContainer clusterStateCacheContainer) (toNSNumber endpoint) (toNSObject queue) completion

-- | @- readAttributeTargetStateWithCompletion:@
readAttributeTargetStateWithCompletion :: IsMTRBaseClusterValveConfigurationAndControl mtrBaseClusterValveConfigurationAndControl => mtrBaseClusterValveConfigurationAndControl -> Ptr () -> IO ()
readAttributeTargetStateWithCompletion mtrBaseClusterValveConfigurationAndControl completion =
  sendMessage mtrBaseClusterValveConfigurationAndControl readAttributeTargetStateWithCompletionSelector completion

-- | @- subscribeAttributeTargetStateWithParams:subscriptionEstablished:reportHandler:@
subscribeAttributeTargetStateWithParams_subscriptionEstablished_reportHandler :: (IsMTRBaseClusterValveConfigurationAndControl mtrBaseClusterValveConfigurationAndControl, IsMTRSubscribeParams params) => mtrBaseClusterValveConfigurationAndControl -> params -> Ptr () -> Ptr () -> IO ()
subscribeAttributeTargetStateWithParams_subscriptionEstablished_reportHandler mtrBaseClusterValveConfigurationAndControl params subscriptionEstablished reportHandler =
  sendMessage mtrBaseClusterValveConfigurationAndControl subscribeAttributeTargetStateWithParams_subscriptionEstablished_reportHandlerSelector (toMTRSubscribeParams params) subscriptionEstablished reportHandler

-- | @+ readAttributeTargetStateWithClusterStateCache:endpoint:queue:completion:@
readAttributeTargetStateWithClusterStateCache_endpoint_queue_completion :: (IsMTRClusterStateCacheContainer clusterStateCacheContainer, IsNSNumber endpoint, IsNSObject queue) => clusterStateCacheContainer -> endpoint -> queue -> Ptr () -> IO ()
readAttributeTargetStateWithClusterStateCache_endpoint_queue_completion clusterStateCacheContainer endpoint queue completion =
  do
    cls' <- getRequiredClass "MTRBaseClusterValveConfigurationAndControl"
    sendClassMessage cls' readAttributeTargetStateWithClusterStateCache_endpoint_queue_completionSelector (toMTRClusterStateCacheContainer clusterStateCacheContainer) (toNSNumber endpoint) (toNSObject queue) completion

-- | @- readAttributeCurrentLevelWithCompletion:@
readAttributeCurrentLevelWithCompletion :: IsMTRBaseClusterValveConfigurationAndControl mtrBaseClusterValveConfigurationAndControl => mtrBaseClusterValveConfigurationAndControl -> Ptr () -> IO ()
readAttributeCurrentLevelWithCompletion mtrBaseClusterValveConfigurationAndControl completion =
  sendMessage mtrBaseClusterValveConfigurationAndControl readAttributeCurrentLevelWithCompletionSelector completion

-- | @- subscribeAttributeCurrentLevelWithParams:subscriptionEstablished:reportHandler:@
subscribeAttributeCurrentLevelWithParams_subscriptionEstablished_reportHandler :: (IsMTRBaseClusterValveConfigurationAndControl mtrBaseClusterValveConfigurationAndControl, IsMTRSubscribeParams params) => mtrBaseClusterValveConfigurationAndControl -> params -> Ptr () -> Ptr () -> IO ()
subscribeAttributeCurrentLevelWithParams_subscriptionEstablished_reportHandler mtrBaseClusterValveConfigurationAndControl params subscriptionEstablished reportHandler =
  sendMessage mtrBaseClusterValveConfigurationAndControl subscribeAttributeCurrentLevelWithParams_subscriptionEstablished_reportHandlerSelector (toMTRSubscribeParams params) subscriptionEstablished reportHandler

-- | @+ readAttributeCurrentLevelWithClusterStateCache:endpoint:queue:completion:@
readAttributeCurrentLevelWithClusterStateCache_endpoint_queue_completion :: (IsMTRClusterStateCacheContainer clusterStateCacheContainer, IsNSNumber endpoint, IsNSObject queue) => clusterStateCacheContainer -> endpoint -> queue -> Ptr () -> IO ()
readAttributeCurrentLevelWithClusterStateCache_endpoint_queue_completion clusterStateCacheContainer endpoint queue completion =
  do
    cls' <- getRequiredClass "MTRBaseClusterValveConfigurationAndControl"
    sendClassMessage cls' readAttributeCurrentLevelWithClusterStateCache_endpoint_queue_completionSelector (toMTRClusterStateCacheContainer clusterStateCacheContainer) (toNSNumber endpoint) (toNSObject queue) completion

-- | @- readAttributeTargetLevelWithCompletion:@
readAttributeTargetLevelWithCompletion :: IsMTRBaseClusterValveConfigurationAndControl mtrBaseClusterValveConfigurationAndControl => mtrBaseClusterValveConfigurationAndControl -> Ptr () -> IO ()
readAttributeTargetLevelWithCompletion mtrBaseClusterValveConfigurationAndControl completion =
  sendMessage mtrBaseClusterValveConfigurationAndControl readAttributeTargetLevelWithCompletionSelector completion

-- | @- subscribeAttributeTargetLevelWithParams:subscriptionEstablished:reportHandler:@
subscribeAttributeTargetLevelWithParams_subscriptionEstablished_reportHandler :: (IsMTRBaseClusterValveConfigurationAndControl mtrBaseClusterValveConfigurationAndControl, IsMTRSubscribeParams params) => mtrBaseClusterValveConfigurationAndControl -> params -> Ptr () -> Ptr () -> IO ()
subscribeAttributeTargetLevelWithParams_subscriptionEstablished_reportHandler mtrBaseClusterValveConfigurationAndControl params subscriptionEstablished reportHandler =
  sendMessage mtrBaseClusterValveConfigurationAndControl subscribeAttributeTargetLevelWithParams_subscriptionEstablished_reportHandlerSelector (toMTRSubscribeParams params) subscriptionEstablished reportHandler

-- | @+ readAttributeTargetLevelWithClusterStateCache:endpoint:queue:completion:@
readAttributeTargetLevelWithClusterStateCache_endpoint_queue_completion :: (IsMTRClusterStateCacheContainer clusterStateCacheContainer, IsNSNumber endpoint, IsNSObject queue) => clusterStateCacheContainer -> endpoint -> queue -> Ptr () -> IO ()
readAttributeTargetLevelWithClusterStateCache_endpoint_queue_completion clusterStateCacheContainer endpoint queue completion =
  do
    cls' <- getRequiredClass "MTRBaseClusterValveConfigurationAndControl"
    sendClassMessage cls' readAttributeTargetLevelWithClusterStateCache_endpoint_queue_completionSelector (toMTRClusterStateCacheContainer clusterStateCacheContainer) (toNSNumber endpoint) (toNSObject queue) completion

-- | @- readAttributeDefaultOpenLevelWithCompletion:@
readAttributeDefaultOpenLevelWithCompletion :: IsMTRBaseClusterValveConfigurationAndControl mtrBaseClusterValveConfigurationAndControl => mtrBaseClusterValveConfigurationAndControl -> Ptr () -> IO ()
readAttributeDefaultOpenLevelWithCompletion mtrBaseClusterValveConfigurationAndControl completion =
  sendMessage mtrBaseClusterValveConfigurationAndControl readAttributeDefaultOpenLevelWithCompletionSelector completion

-- | @- writeAttributeDefaultOpenLevelWithValue:completion:@
writeAttributeDefaultOpenLevelWithValue_completion :: (IsMTRBaseClusterValveConfigurationAndControl mtrBaseClusterValveConfigurationAndControl, IsNSNumber value) => mtrBaseClusterValveConfigurationAndControl -> value -> Ptr () -> IO ()
writeAttributeDefaultOpenLevelWithValue_completion mtrBaseClusterValveConfigurationAndControl value completion =
  sendMessage mtrBaseClusterValveConfigurationAndControl writeAttributeDefaultOpenLevelWithValue_completionSelector (toNSNumber value) completion

-- | @- writeAttributeDefaultOpenLevelWithValue:params:completion:@
writeAttributeDefaultOpenLevelWithValue_params_completion :: (IsMTRBaseClusterValveConfigurationAndControl mtrBaseClusterValveConfigurationAndControl, IsNSNumber value, IsMTRWriteParams params) => mtrBaseClusterValveConfigurationAndControl -> value -> params -> Ptr () -> IO ()
writeAttributeDefaultOpenLevelWithValue_params_completion mtrBaseClusterValveConfigurationAndControl value params completion =
  sendMessage mtrBaseClusterValveConfigurationAndControl writeAttributeDefaultOpenLevelWithValue_params_completionSelector (toNSNumber value) (toMTRWriteParams params) completion

-- | @- subscribeAttributeDefaultOpenLevelWithParams:subscriptionEstablished:reportHandler:@
subscribeAttributeDefaultOpenLevelWithParams_subscriptionEstablished_reportHandler :: (IsMTRBaseClusterValveConfigurationAndControl mtrBaseClusterValveConfigurationAndControl, IsMTRSubscribeParams params) => mtrBaseClusterValveConfigurationAndControl -> params -> Ptr () -> Ptr () -> IO ()
subscribeAttributeDefaultOpenLevelWithParams_subscriptionEstablished_reportHandler mtrBaseClusterValveConfigurationAndControl params subscriptionEstablished reportHandler =
  sendMessage mtrBaseClusterValveConfigurationAndControl subscribeAttributeDefaultOpenLevelWithParams_subscriptionEstablished_reportHandlerSelector (toMTRSubscribeParams params) subscriptionEstablished reportHandler

-- | @+ readAttributeDefaultOpenLevelWithClusterStateCache:endpoint:queue:completion:@
readAttributeDefaultOpenLevelWithClusterStateCache_endpoint_queue_completion :: (IsMTRClusterStateCacheContainer clusterStateCacheContainer, IsNSNumber endpoint, IsNSObject queue) => clusterStateCacheContainer -> endpoint -> queue -> Ptr () -> IO ()
readAttributeDefaultOpenLevelWithClusterStateCache_endpoint_queue_completion clusterStateCacheContainer endpoint queue completion =
  do
    cls' <- getRequiredClass "MTRBaseClusterValveConfigurationAndControl"
    sendClassMessage cls' readAttributeDefaultOpenLevelWithClusterStateCache_endpoint_queue_completionSelector (toMTRClusterStateCacheContainer clusterStateCacheContainer) (toNSNumber endpoint) (toNSObject queue) completion

-- | @- readAttributeValveFaultWithCompletion:@
readAttributeValveFaultWithCompletion :: IsMTRBaseClusterValveConfigurationAndControl mtrBaseClusterValveConfigurationAndControl => mtrBaseClusterValveConfigurationAndControl -> Ptr () -> IO ()
readAttributeValveFaultWithCompletion mtrBaseClusterValveConfigurationAndControl completion =
  sendMessage mtrBaseClusterValveConfigurationAndControl readAttributeValveFaultWithCompletionSelector completion

-- | @- subscribeAttributeValveFaultWithParams:subscriptionEstablished:reportHandler:@
subscribeAttributeValveFaultWithParams_subscriptionEstablished_reportHandler :: (IsMTRBaseClusterValveConfigurationAndControl mtrBaseClusterValveConfigurationAndControl, IsMTRSubscribeParams params) => mtrBaseClusterValveConfigurationAndControl -> params -> Ptr () -> Ptr () -> IO ()
subscribeAttributeValveFaultWithParams_subscriptionEstablished_reportHandler mtrBaseClusterValveConfigurationAndControl params subscriptionEstablished reportHandler =
  sendMessage mtrBaseClusterValveConfigurationAndControl subscribeAttributeValveFaultWithParams_subscriptionEstablished_reportHandlerSelector (toMTRSubscribeParams params) subscriptionEstablished reportHandler

-- | @+ readAttributeValveFaultWithClusterStateCache:endpoint:queue:completion:@
readAttributeValveFaultWithClusterStateCache_endpoint_queue_completion :: (IsMTRClusterStateCacheContainer clusterStateCacheContainer, IsNSNumber endpoint, IsNSObject queue) => clusterStateCacheContainer -> endpoint -> queue -> Ptr () -> IO ()
readAttributeValveFaultWithClusterStateCache_endpoint_queue_completion clusterStateCacheContainer endpoint queue completion =
  do
    cls' <- getRequiredClass "MTRBaseClusterValveConfigurationAndControl"
    sendClassMessage cls' readAttributeValveFaultWithClusterStateCache_endpoint_queue_completionSelector (toMTRClusterStateCacheContainer clusterStateCacheContainer) (toNSNumber endpoint) (toNSObject queue) completion

-- | @- readAttributeLevelStepWithCompletion:@
readAttributeLevelStepWithCompletion :: IsMTRBaseClusterValveConfigurationAndControl mtrBaseClusterValveConfigurationAndControl => mtrBaseClusterValveConfigurationAndControl -> Ptr () -> IO ()
readAttributeLevelStepWithCompletion mtrBaseClusterValveConfigurationAndControl completion =
  sendMessage mtrBaseClusterValveConfigurationAndControl readAttributeLevelStepWithCompletionSelector completion

-- | @- subscribeAttributeLevelStepWithParams:subscriptionEstablished:reportHandler:@
subscribeAttributeLevelStepWithParams_subscriptionEstablished_reportHandler :: (IsMTRBaseClusterValveConfigurationAndControl mtrBaseClusterValveConfigurationAndControl, IsMTRSubscribeParams params) => mtrBaseClusterValveConfigurationAndControl -> params -> Ptr () -> Ptr () -> IO ()
subscribeAttributeLevelStepWithParams_subscriptionEstablished_reportHandler mtrBaseClusterValveConfigurationAndControl params subscriptionEstablished reportHandler =
  sendMessage mtrBaseClusterValveConfigurationAndControl subscribeAttributeLevelStepWithParams_subscriptionEstablished_reportHandlerSelector (toMTRSubscribeParams params) subscriptionEstablished reportHandler

-- | @+ readAttributeLevelStepWithClusterStateCache:endpoint:queue:completion:@
readAttributeLevelStepWithClusterStateCache_endpoint_queue_completion :: (IsMTRClusterStateCacheContainer clusterStateCacheContainer, IsNSNumber endpoint, IsNSObject queue) => clusterStateCacheContainer -> endpoint -> queue -> Ptr () -> IO ()
readAttributeLevelStepWithClusterStateCache_endpoint_queue_completion clusterStateCacheContainer endpoint queue completion =
  do
    cls' <- getRequiredClass "MTRBaseClusterValveConfigurationAndControl"
    sendClassMessage cls' readAttributeLevelStepWithClusterStateCache_endpoint_queue_completionSelector (toMTRClusterStateCacheContainer clusterStateCacheContainer) (toNSNumber endpoint) (toNSObject queue) completion

-- | @- readAttributeGeneratedCommandListWithCompletion:@
readAttributeGeneratedCommandListWithCompletion :: IsMTRBaseClusterValveConfigurationAndControl mtrBaseClusterValveConfigurationAndControl => mtrBaseClusterValveConfigurationAndControl -> Ptr () -> IO ()
readAttributeGeneratedCommandListWithCompletion mtrBaseClusterValveConfigurationAndControl completion =
  sendMessage mtrBaseClusterValveConfigurationAndControl readAttributeGeneratedCommandListWithCompletionSelector completion

-- | @- subscribeAttributeGeneratedCommandListWithParams:subscriptionEstablished:reportHandler:@
subscribeAttributeGeneratedCommandListWithParams_subscriptionEstablished_reportHandler :: (IsMTRBaseClusterValveConfigurationAndControl mtrBaseClusterValveConfigurationAndControl, IsMTRSubscribeParams params) => mtrBaseClusterValveConfigurationAndControl -> params -> Ptr () -> Ptr () -> IO ()
subscribeAttributeGeneratedCommandListWithParams_subscriptionEstablished_reportHandler mtrBaseClusterValveConfigurationAndControl params subscriptionEstablished reportHandler =
  sendMessage mtrBaseClusterValveConfigurationAndControl subscribeAttributeGeneratedCommandListWithParams_subscriptionEstablished_reportHandlerSelector (toMTRSubscribeParams params) subscriptionEstablished reportHandler

-- | @+ readAttributeGeneratedCommandListWithClusterStateCache:endpoint:queue:completion:@
readAttributeGeneratedCommandListWithClusterStateCache_endpoint_queue_completion :: (IsMTRClusterStateCacheContainer clusterStateCacheContainer, IsNSNumber endpoint, IsNSObject queue) => clusterStateCacheContainer -> endpoint -> queue -> Ptr () -> IO ()
readAttributeGeneratedCommandListWithClusterStateCache_endpoint_queue_completion clusterStateCacheContainer endpoint queue completion =
  do
    cls' <- getRequiredClass "MTRBaseClusterValveConfigurationAndControl"
    sendClassMessage cls' readAttributeGeneratedCommandListWithClusterStateCache_endpoint_queue_completionSelector (toMTRClusterStateCacheContainer clusterStateCacheContainer) (toNSNumber endpoint) (toNSObject queue) completion

-- | @- readAttributeAcceptedCommandListWithCompletion:@
readAttributeAcceptedCommandListWithCompletion :: IsMTRBaseClusterValveConfigurationAndControl mtrBaseClusterValveConfigurationAndControl => mtrBaseClusterValveConfigurationAndControl -> Ptr () -> IO ()
readAttributeAcceptedCommandListWithCompletion mtrBaseClusterValveConfigurationAndControl completion =
  sendMessage mtrBaseClusterValveConfigurationAndControl readAttributeAcceptedCommandListWithCompletionSelector completion

-- | @- subscribeAttributeAcceptedCommandListWithParams:subscriptionEstablished:reportHandler:@
subscribeAttributeAcceptedCommandListWithParams_subscriptionEstablished_reportHandler :: (IsMTRBaseClusterValveConfigurationAndControl mtrBaseClusterValveConfigurationAndControl, IsMTRSubscribeParams params) => mtrBaseClusterValveConfigurationAndControl -> params -> Ptr () -> Ptr () -> IO ()
subscribeAttributeAcceptedCommandListWithParams_subscriptionEstablished_reportHandler mtrBaseClusterValveConfigurationAndControl params subscriptionEstablished reportHandler =
  sendMessage mtrBaseClusterValveConfigurationAndControl subscribeAttributeAcceptedCommandListWithParams_subscriptionEstablished_reportHandlerSelector (toMTRSubscribeParams params) subscriptionEstablished reportHandler

-- | @+ readAttributeAcceptedCommandListWithClusterStateCache:endpoint:queue:completion:@
readAttributeAcceptedCommandListWithClusterStateCache_endpoint_queue_completion :: (IsMTRClusterStateCacheContainer clusterStateCacheContainer, IsNSNumber endpoint, IsNSObject queue) => clusterStateCacheContainer -> endpoint -> queue -> Ptr () -> IO ()
readAttributeAcceptedCommandListWithClusterStateCache_endpoint_queue_completion clusterStateCacheContainer endpoint queue completion =
  do
    cls' <- getRequiredClass "MTRBaseClusterValveConfigurationAndControl"
    sendClassMessage cls' readAttributeAcceptedCommandListWithClusterStateCache_endpoint_queue_completionSelector (toMTRClusterStateCacheContainer clusterStateCacheContainer) (toNSNumber endpoint) (toNSObject queue) completion

-- | @- readAttributeAttributeListWithCompletion:@
readAttributeAttributeListWithCompletion :: IsMTRBaseClusterValveConfigurationAndControl mtrBaseClusterValveConfigurationAndControl => mtrBaseClusterValveConfigurationAndControl -> Ptr () -> IO ()
readAttributeAttributeListWithCompletion mtrBaseClusterValveConfigurationAndControl completion =
  sendMessage mtrBaseClusterValveConfigurationAndControl readAttributeAttributeListWithCompletionSelector completion

-- | @- subscribeAttributeAttributeListWithParams:subscriptionEstablished:reportHandler:@
subscribeAttributeAttributeListWithParams_subscriptionEstablished_reportHandler :: (IsMTRBaseClusterValveConfigurationAndControl mtrBaseClusterValveConfigurationAndControl, IsMTRSubscribeParams params) => mtrBaseClusterValveConfigurationAndControl -> params -> Ptr () -> Ptr () -> IO ()
subscribeAttributeAttributeListWithParams_subscriptionEstablished_reportHandler mtrBaseClusterValveConfigurationAndControl params subscriptionEstablished reportHandler =
  sendMessage mtrBaseClusterValveConfigurationAndControl subscribeAttributeAttributeListWithParams_subscriptionEstablished_reportHandlerSelector (toMTRSubscribeParams params) subscriptionEstablished reportHandler

-- | @+ readAttributeAttributeListWithClusterStateCache:endpoint:queue:completion:@
readAttributeAttributeListWithClusterStateCache_endpoint_queue_completion :: (IsMTRClusterStateCacheContainer clusterStateCacheContainer, IsNSNumber endpoint, IsNSObject queue) => clusterStateCacheContainer -> endpoint -> queue -> Ptr () -> IO ()
readAttributeAttributeListWithClusterStateCache_endpoint_queue_completion clusterStateCacheContainer endpoint queue completion =
  do
    cls' <- getRequiredClass "MTRBaseClusterValveConfigurationAndControl"
    sendClassMessage cls' readAttributeAttributeListWithClusterStateCache_endpoint_queue_completionSelector (toMTRClusterStateCacheContainer clusterStateCacheContainer) (toNSNumber endpoint) (toNSObject queue) completion

-- | @- readAttributeFeatureMapWithCompletion:@
readAttributeFeatureMapWithCompletion :: IsMTRBaseClusterValveConfigurationAndControl mtrBaseClusterValveConfigurationAndControl => mtrBaseClusterValveConfigurationAndControl -> Ptr () -> IO ()
readAttributeFeatureMapWithCompletion mtrBaseClusterValveConfigurationAndControl completion =
  sendMessage mtrBaseClusterValveConfigurationAndControl readAttributeFeatureMapWithCompletionSelector completion

-- | @- subscribeAttributeFeatureMapWithParams:subscriptionEstablished:reportHandler:@
subscribeAttributeFeatureMapWithParams_subscriptionEstablished_reportHandler :: (IsMTRBaseClusterValveConfigurationAndControl mtrBaseClusterValveConfigurationAndControl, IsMTRSubscribeParams params) => mtrBaseClusterValveConfigurationAndControl -> params -> Ptr () -> Ptr () -> IO ()
subscribeAttributeFeatureMapWithParams_subscriptionEstablished_reportHandler mtrBaseClusterValveConfigurationAndControl params subscriptionEstablished reportHandler =
  sendMessage mtrBaseClusterValveConfigurationAndControl subscribeAttributeFeatureMapWithParams_subscriptionEstablished_reportHandlerSelector (toMTRSubscribeParams params) subscriptionEstablished reportHandler

-- | @+ readAttributeFeatureMapWithClusterStateCache:endpoint:queue:completion:@
readAttributeFeatureMapWithClusterStateCache_endpoint_queue_completion :: (IsMTRClusterStateCacheContainer clusterStateCacheContainer, IsNSNumber endpoint, IsNSObject queue) => clusterStateCacheContainer -> endpoint -> queue -> Ptr () -> IO ()
readAttributeFeatureMapWithClusterStateCache_endpoint_queue_completion clusterStateCacheContainer endpoint queue completion =
  do
    cls' <- getRequiredClass "MTRBaseClusterValveConfigurationAndControl"
    sendClassMessage cls' readAttributeFeatureMapWithClusterStateCache_endpoint_queue_completionSelector (toMTRClusterStateCacheContainer clusterStateCacheContainer) (toNSNumber endpoint) (toNSObject queue) completion

-- | @- readAttributeClusterRevisionWithCompletion:@
readAttributeClusterRevisionWithCompletion :: IsMTRBaseClusterValveConfigurationAndControl mtrBaseClusterValveConfigurationAndControl => mtrBaseClusterValveConfigurationAndControl -> Ptr () -> IO ()
readAttributeClusterRevisionWithCompletion mtrBaseClusterValveConfigurationAndControl completion =
  sendMessage mtrBaseClusterValveConfigurationAndControl readAttributeClusterRevisionWithCompletionSelector completion

-- | @- subscribeAttributeClusterRevisionWithParams:subscriptionEstablished:reportHandler:@
subscribeAttributeClusterRevisionWithParams_subscriptionEstablished_reportHandler :: (IsMTRBaseClusterValveConfigurationAndControl mtrBaseClusterValveConfigurationAndControl, IsMTRSubscribeParams params) => mtrBaseClusterValveConfigurationAndControl -> params -> Ptr () -> Ptr () -> IO ()
subscribeAttributeClusterRevisionWithParams_subscriptionEstablished_reportHandler mtrBaseClusterValveConfigurationAndControl params subscriptionEstablished reportHandler =
  sendMessage mtrBaseClusterValveConfigurationAndControl subscribeAttributeClusterRevisionWithParams_subscriptionEstablished_reportHandlerSelector (toMTRSubscribeParams params) subscriptionEstablished reportHandler

-- | @+ readAttributeClusterRevisionWithClusterStateCache:endpoint:queue:completion:@
readAttributeClusterRevisionWithClusterStateCache_endpoint_queue_completion :: (IsMTRClusterStateCacheContainer clusterStateCacheContainer, IsNSNumber endpoint, IsNSObject queue) => clusterStateCacheContainer -> endpoint -> queue -> Ptr () -> IO ()
readAttributeClusterRevisionWithClusterStateCache_endpoint_queue_completion clusterStateCacheContainer endpoint queue completion =
  do
    cls' <- getRequiredClass "MTRBaseClusterValveConfigurationAndControl"
    sendClassMessage cls' readAttributeClusterRevisionWithClusterStateCache_endpoint_queue_completionSelector (toMTRClusterStateCacheContainer clusterStateCacheContainer) (toNSNumber endpoint) (toNSObject queue) completion

-- | @- init@
init_ :: IsMTRBaseClusterValveConfigurationAndControl mtrBaseClusterValveConfigurationAndControl => mtrBaseClusterValveConfigurationAndControl -> IO (Id MTRBaseClusterValveConfigurationAndControl)
init_ mtrBaseClusterValveConfigurationAndControl =
  sendOwnedMessage mtrBaseClusterValveConfigurationAndControl initSelector

-- | @+ new@
new :: IO (Id MTRBaseClusterValveConfigurationAndControl)
new  =
  do
    cls' <- getRequiredClass "MTRBaseClusterValveConfigurationAndControl"
    sendOwnedClassMessage cls' newSelector

-- | For all instance methods (reads, writes, commands) that take a completion, the completion will be called on the provided queue.
--
-- ObjC selector: @- initWithDevice:endpointID:queue:@
initWithDevice_endpointID_queue :: (IsMTRBaseClusterValveConfigurationAndControl mtrBaseClusterValveConfigurationAndControl, IsMTRBaseDevice device, IsNSNumber endpointID, IsNSObject queue) => mtrBaseClusterValveConfigurationAndControl -> device -> endpointID -> queue -> IO (Id MTRBaseClusterValveConfigurationAndControl)
initWithDevice_endpointID_queue mtrBaseClusterValveConfigurationAndControl device endpointID queue =
  sendOwnedMessage mtrBaseClusterValveConfigurationAndControl initWithDevice_endpointID_queueSelector (toMTRBaseDevice device) (toNSNumber endpointID) (toNSObject queue)

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @openWithParams:completion:@
openWithParams_completionSelector :: Selector '[Id MTRValveConfigurationAndControlClusterOpenParams, Ptr ()] ()
openWithParams_completionSelector = mkSelector "openWithParams:completion:"

-- | @Selector@ for @openWithCompletion:@
openWithCompletionSelector :: Selector '[Ptr ()] ()
openWithCompletionSelector = mkSelector "openWithCompletion:"

-- | @Selector@ for @closeWithParams:completion:@
closeWithParams_completionSelector :: Selector '[Id MTRValveConfigurationAndControlClusterCloseParams, Ptr ()] ()
closeWithParams_completionSelector = mkSelector "closeWithParams:completion:"

-- | @Selector@ for @closeWithCompletion:@
closeWithCompletionSelector :: Selector '[Ptr ()] ()
closeWithCompletionSelector = mkSelector "closeWithCompletion:"

-- | @Selector@ for @readAttributeOpenDurationWithCompletion:@
readAttributeOpenDurationWithCompletionSelector :: Selector '[Ptr ()] ()
readAttributeOpenDurationWithCompletionSelector = mkSelector "readAttributeOpenDurationWithCompletion:"

-- | @Selector@ for @subscribeAttributeOpenDurationWithParams:subscriptionEstablished:reportHandler:@
subscribeAttributeOpenDurationWithParams_subscriptionEstablished_reportHandlerSelector :: Selector '[Id MTRSubscribeParams, Ptr (), Ptr ()] ()
subscribeAttributeOpenDurationWithParams_subscriptionEstablished_reportHandlerSelector = mkSelector "subscribeAttributeOpenDurationWithParams:subscriptionEstablished:reportHandler:"

-- | @Selector@ for @readAttributeOpenDurationWithClusterStateCache:endpoint:queue:completion:@
readAttributeOpenDurationWithClusterStateCache_endpoint_queue_completionSelector :: Selector '[Id MTRClusterStateCacheContainer, Id NSNumber, Id NSObject, Ptr ()] ()
readAttributeOpenDurationWithClusterStateCache_endpoint_queue_completionSelector = mkSelector "readAttributeOpenDurationWithClusterStateCache:endpoint:queue:completion:"

-- | @Selector@ for @readAttributeDefaultOpenDurationWithCompletion:@
readAttributeDefaultOpenDurationWithCompletionSelector :: Selector '[Ptr ()] ()
readAttributeDefaultOpenDurationWithCompletionSelector = mkSelector "readAttributeDefaultOpenDurationWithCompletion:"

-- | @Selector@ for @writeAttributeDefaultOpenDurationWithValue:completion:@
writeAttributeDefaultOpenDurationWithValue_completionSelector :: Selector '[Id NSNumber, Ptr ()] ()
writeAttributeDefaultOpenDurationWithValue_completionSelector = mkSelector "writeAttributeDefaultOpenDurationWithValue:completion:"

-- | @Selector@ for @writeAttributeDefaultOpenDurationWithValue:params:completion:@
writeAttributeDefaultOpenDurationWithValue_params_completionSelector :: Selector '[Id NSNumber, Id MTRWriteParams, Ptr ()] ()
writeAttributeDefaultOpenDurationWithValue_params_completionSelector = mkSelector "writeAttributeDefaultOpenDurationWithValue:params:completion:"

-- | @Selector@ for @subscribeAttributeDefaultOpenDurationWithParams:subscriptionEstablished:reportHandler:@
subscribeAttributeDefaultOpenDurationWithParams_subscriptionEstablished_reportHandlerSelector :: Selector '[Id MTRSubscribeParams, Ptr (), Ptr ()] ()
subscribeAttributeDefaultOpenDurationWithParams_subscriptionEstablished_reportHandlerSelector = mkSelector "subscribeAttributeDefaultOpenDurationWithParams:subscriptionEstablished:reportHandler:"

-- | @Selector@ for @readAttributeDefaultOpenDurationWithClusterStateCache:endpoint:queue:completion:@
readAttributeDefaultOpenDurationWithClusterStateCache_endpoint_queue_completionSelector :: Selector '[Id MTRClusterStateCacheContainer, Id NSNumber, Id NSObject, Ptr ()] ()
readAttributeDefaultOpenDurationWithClusterStateCache_endpoint_queue_completionSelector = mkSelector "readAttributeDefaultOpenDurationWithClusterStateCache:endpoint:queue:completion:"

-- | @Selector@ for @readAttributeAutoCloseTimeWithCompletion:@
readAttributeAutoCloseTimeWithCompletionSelector :: Selector '[Ptr ()] ()
readAttributeAutoCloseTimeWithCompletionSelector = mkSelector "readAttributeAutoCloseTimeWithCompletion:"

-- | @Selector@ for @subscribeAttributeAutoCloseTimeWithParams:subscriptionEstablished:reportHandler:@
subscribeAttributeAutoCloseTimeWithParams_subscriptionEstablished_reportHandlerSelector :: Selector '[Id MTRSubscribeParams, Ptr (), Ptr ()] ()
subscribeAttributeAutoCloseTimeWithParams_subscriptionEstablished_reportHandlerSelector = mkSelector "subscribeAttributeAutoCloseTimeWithParams:subscriptionEstablished:reportHandler:"

-- | @Selector@ for @readAttributeAutoCloseTimeWithClusterStateCache:endpoint:queue:completion:@
readAttributeAutoCloseTimeWithClusterStateCache_endpoint_queue_completionSelector :: Selector '[Id MTRClusterStateCacheContainer, Id NSNumber, Id NSObject, Ptr ()] ()
readAttributeAutoCloseTimeWithClusterStateCache_endpoint_queue_completionSelector = mkSelector "readAttributeAutoCloseTimeWithClusterStateCache:endpoint:queue:completion:"

-- | @Selector@ for @readAttributeRemainingDurationWithCompletion:@
readAttributeRemainingDurationWithCompletionSelector :: Selector '[Ptr ()] ()
readAttributeRemainingDurationWithCompletionSelector = mkSelector "readAttributeRemainingDurationWithCompletion:"

-- | @Selector@ for @subscribeAttributeRemainingDurationWithParams:subscriptionEstablished:reportHandler:@
subscribeAttributeRemainingDurationWithParams_subscriptionEstablished_reportHandlerSelector :: Selector '[Id MTRSubscribeParams, Ptr (), Ptr ()] ()
subscribeAttributeRemainingDurationWithParams_subscriptionEstablished_reportHandlerSelector = mkSelector "subscribeAttributeRemainingDurationWithParams:subscriptionEstablished:reportHandler:"

-- | @Selector@ for @readAttributeRemainingDurationWithClusterStateCache:endpoint:queue:completion:@
readAttributeRemainingDurationWithClusterStateCache_endpoint_queue_completionSelector :: Selector '[Id MTRClusterStateCacheContainer, Id NSNumber, Id NSObject, Ptr ()] ()
readAttributeRemainingDurationWithClusterStateCache_endpoint_queue_completionSelector = mkSelector "readAttributeRemainingDurationWithClusterStateCache:endpoint:queue:completion:"

-- | @Selector@ for @readAttributeCurrentStateWithCompletion:@
readAttributeCurrentStateWithCompletionSelector :: Selector '[Ptr ()] ()
readAttributeCurrentStateWithCompletionSelector = mkSelector "readAttributeCurrentStateWithCompletion:"

-- | @Selector@ for @subscribeAttributeCurrentStateWithParams:subscriptionEstablished:reportHandler:@
subscribeAttributeCurrentStateWithParams_subscriptionEstablished_reportHandlerSelector :: Selector '[Id MTRSubscribeParams, Ptr (), Ptr ()] ()
subscribeAttributeCurrentStateWithParams_subscriptionEstablished_reportHandlerSelector = mkSelector "subscribeAttributeCurrentStateWithParams:subscriptionEstablished:reportHandler:"

-- | @Selector@ for @readAttributeCurrentStateWithClusterStateCache:endpoint:queue:completion:@
readAttributeCurrentStateWithClusterStateCache_endpoint_queue_completionSelector :: Selector '[Id MTRClusterStateCacheContainer, Id NSNumber, Id NSObject, Ptr ()] ()
readAttributeCurrentStateWithClusterStateCache_endpoint_queue_completionSelector = mkSelector "readAttributeCurrentStateWithClusterStateCache:endpoint:queue:completion:"

-- | @Selector@ for @readAttributeTargetStateWithCompletion:@
readAttributeTargetStateWithCompletionSelector :: Selector '[Ptr ()] ()
readAttributeTargetStateWithCompletionSelector = mkSelector "readAttributeTargetStateWithCompletion:"

-- | @Selector@ for @subscribeAttributeTargetStateWithParams:subscriptionEstablished:reportHandler:@
subscribeAttributeTargetStateWithParams_subscriptionEstablished_reportHandlerSelector :: Selector '[Id MTRSubscribeParams, Ptr (), Ptr ()] ()
subscribeAttributeTargetStateWithParams_subscriptionEstablished_reportHandlerSelector = mkSelector "subscribeAttributeTargetStateWithParams:subscriptionEstablished:reportHandler:"

-- | @Selector@ for @readAttributeTargetStateWithClusterStateCache:endpoint:queue:completion:@
readAttributeTargetStateWithClusterStateCache_endpoint_queue_completionSelector :: Selector '[Id MTRClusterStateCacheContainer, Id NSNumber, Id NSObject, Ptr ()] ()
readAttributeTargetStateWithClusterStateCache_endpoint_queue_completionSelector = mkSelector "readAttributeTargetStateWithClusterStateCache:endpoint:queue:completion:"

-- | @Selector@ for @readAttributeCurrentLevelWithCompletion:@
readAttributeCurrentLevelWithCompletionSelector :: Selector '[Ptr ()] ()
readAttributeCurrentLevelWithCompletionSelector = mkSelector "readAttributeCurrentLevelWithCompletion:"

-- | @Selector@ for @subscribeAttributeCurrentLevelWithParams:subscriptionEstablished:reportHandler:@
subscribeAttributeCurrentLevelWithParams_subscriptionEstablished_reportHandlerSelector :: Selector '[Id MTRSubscribeParams, Ptr (), Ptr ()] ()
subscribeAttributeCurrentLevelWithParams_subscriptionEstablished_reportHandlerSelector = mkSelector "subscribeAttributeCurrentLevelWithParams:subscriptionEstablished:reportHandler:"

-- | @Selector@ for @readAttributeCurrentLevelWithClusterStateCache:endpoint:queue:completion:@
readAttributeCurrentLevelWithClusterStateCache_endpoint_queue_completionSelector :: Selector '[Id MTRClusterStateCacheContainer, Id NSNumber, Id NSObject, Ptr ()] ()
readAttributeCurrentLevelWithClusterStateCache_endpoint_queue_completionSelector = mkSelector "readAttributeCurrentLevelWithClusterStateCache:endpoint:queue:completion:"

-- | @Selector@ for @readAttributeTargetLevelWithCompletion:@
readAttributeTargetLevelWithCompletionSelector :: Selector '[Ptr ()] ()
readAttributeTargetLevelWithCompletionSelector = mkSelector "readAttributeTargetLevelWithCompletion:"

-- | @Selector@ for @subscribeAttributeTargetLevelWithParams:subscriptionEstablished:reportHandler:@
subscribeAttributeTargetLevelWithParams_subscriptionEstablished_reportHandlerSelector :: Selector '[Id MTRSubscribeParams, Ptr (), Ptr ()] ()
subscribeAttributeTargetLevelWithParams_subscriptionEstablished_reportHandlerSelector = mkSelector "subscribeAttributeTargetLevelWithParams:subscriptionEstablished:reportHandler:"

-- | @Selector@ for @readAttributeTargetLevelWithClusterStateCache:endpoint:queue:completion:@
readAttributeTargetLevelWithClusterStateCache_endpoint_queue_completionSelector :: Selector '[Id MTRClusterStateCacheContainer, Id NSNumber, Id NSObject, Ptr ()] ()
readAttributeTargetLevelWithClusterStateCache_endpoint_queue_completionSelector = mkSelector "readAttributeTargetLevelWithClusterStateCache:endpoint:queue:completion:"

-- | @Selector@ for @readAttributeDefaultOpenLevelWithCompletion:@
readAttributeDefaultOpenLevelWithCompletionSelector :: Selector '[Ptr ()] ()
readAttributeDefaultOpenLevelWithCompletionSelector = mkSelector "readAttributeDefaultOpenLevelWithCompletion:"

-- | @Selector@ for @writeAttributeDefaultOpenLevelWithValue:completion:@
writeAttributeDefaultOpenLevelWithValue_completionSelector :: Selector '[Id NSNumber, Ptr ()] ()
writeAttributeDefaultOpenLevelWithValue_completionSelector = mkSelector "writeAttributeDefaultOpenLevelWithValue:completion:"

-- | @Selector@ for @writeAttributeDefaultOpenLevelWithValue:params:completion:@
writeAttributeDefaultOpenLevelWithValue_params_completionSelector :: Selector '[Id NSNumber, Id MTRWriteParams, Ptr ()] ()
writeAttributeDefaultOpenLevelWithValue_params_completionSelector = mkSelector "writeAttributeDefaultOpenLevelWithValue:params:completion:"

-- | @Selector@ for @subscribeAttributeDefaultOpenLevelWithParams:subscriptionEstablished:reportHandler:@
subscribeAttributeDefaultOpenLevelWithParams_subscriptionEstablished_reportHandlerSelector :: Selector '[Id MTRSubscribeParams, Ptr (), Ptr ()] ()
subscribeAttributeDefaultOpenLevelWithParams_subscriptionEstablished_reportHandlerSelector = mkSelector "subscribeAttributeDefaultOpenLevelWithParams:subscriptionEstablished:reportHandler:"

-- | @Selector@ for @readAttributeDefaultOpenLevelWithClusterStateCache:endpoint:queue:completion:@
readAttributeDefaultOpenLevelWithClusterStateCache_endpoint_queue_completionSelector :: Selector '[Id MTRClusterStateCacheContainer, Id NSNumber, Id NSObject, Ptr ()] ()
readAttributeDefaultOpenLevelWithClusterStateCache_endpoint_queue_completionSelector = mkSelector "readAttributeDefaultOpenLevelWithClusterStateCache:endpoint:queue:completion:"

-- | @Selector@ for @readAttributeValveFaultWithCompletion:@
readAttributeValveFaultWithCompletionSelector :: Selector '[Ptr ()] ()
readAttributeValveFaultWithCompletionSelector = mkSelector "readAttributeValveFaultWithCompletion:"

-- | @Selector@ for @subscribeAttributeValveFaultWithParams:subscriptionEstablished:reportHandler:@
subscribeAttributeValveFaultWithParams_subscriptionEstablished_reportHandlerSelector :: Selector '[Id MTRSubscribeParams, Ptr (), Ptr ()] ()
subscribeAttributeValveFaultWithParams_subscriptionEstablished_reportHandlerSelector = mkSelector "subscribeAttributeValveFaultWithParams:subscriptionEstablished:reportHandler:"

-- | @Selector@ for @readAttributeValveFaultWithClusterStateCache:endpoint:queue:completion:@
readAttributeValveFaultWithClusterStateCache_endpoint_queue_completionSelector :: Selector '[Id MTRClusterStateCacheContainer, Id NSNumber, Id NSObject, Ptr ()] ()
readAttributeValveFaultWithClusterStateCache_endpoint_queue_completionSelector = mkSelector "readAttributeValveFaultWithClusterStateCache:endpoint:queue:completion:"

-- | @Selector@ for @readAttributeLevelStepWithCompletion:@
readAttributeLevelStepWithCompletionSelector :: Selector '[Ptr ()] ()
readAttributeLevelStepWithCompletionSelector = mkSelector "readAttributeLevelStepWithCompletion:"

-- | @Selector@ for @subscribeAttributeLevelStepWithParams:subscriptionEstablished:reportHandler:@
subscribeAttributeLevelStepWithParams_subscriptionEstablished_reportHandlerSelector :: Selector '[Id MTRSubscribeParams, Ptr (), Ptr ()] ()
subscribeAttributeLevelStepWithParams_subscriptionEstablished_reportHandlerSelector = mkSelector "subscribeAttributeLevelStepWithParams:subscriptionEstablished:reportHandler:"

-- | @Selector@ for @readAttributeLevelStepWithClusterStateCache:endpoint:queue:completion:@
readAttributeLevelStepWithClusterStateCache_endpoint_queue_completionSelector :: Selector '[Id MTRClusterStateCacheContainer, Id NSNumber, Id NSObject, Ptr ()] ()
readAttributeLevelStepWithClusterStateCache_endpoint_queue_completionSelector = mkSelector "readAttributeLevelStepWithClusterStateCache:endpoint:queue:completion:"

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
initSelector :: Selector '[] (Id MTRBaseClusterValveConfigurationAndControl)
initSelector = mkSelector "init"

-- | @Selector@ for @new@
newSelector :: Selector '[] (Id MTRBaseClusterValveConfigurationAndControl)
newSelector = mkSelector "new"

-- | @Selector@ for @initWithDevice:endpointID:queue:@
initWithDevice_endpointID_queueSelector :: Selector '[Id MTRBaseDevice, Id NSNumber, Id NSObject] (Id MTRBaseClusterValveConfigurationAndControl)
initWithDevice_endpointID_queueSelector = mkSelector "initWithDevice:endpointID:queue:"

