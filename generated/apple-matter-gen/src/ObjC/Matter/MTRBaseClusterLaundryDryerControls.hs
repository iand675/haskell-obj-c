{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Cluster Laundry Dryer Controls
--
-- This cluster provides a way to access options associated with the operation of            a laundry dryer device type.
--
-- Generated bindings for @MTRBaseClusterLaundryDryerControls@.
module ObjC.Matter.MTRBaseClusterLaundryDryerControls
  ( MTRBaseClusterLaundryDryerControls
  , IsMTRBaseClusterLaundryDryerControls(..)
  , readAttributeSupportedDrynessLevelsWithCompletion
  , subscribeAttributeSupportedDrynessLevelsWithParams_subscriptionEstablished_reportHandler
  , readAttributeSupportedDrynessLevelsWithClusterStateCache_endpoint_queue_completion
  , readAttributeSelectedDrynessLevelWithCompletion
  , writeAttributeSelectedDrynessLevelWithValue_completion
  , writeAttributeSelectedDrynessLevelWithValue_params_completion
  , subscribeAttributeSelectedDrynessLevelWithParams_subscriptionEstablished_reportHandler
  , readAttributeSelectedDrynessLevelWithClusterStateCache_endpoint_queue_completion
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
  , readAttributeSelectedDrynessLevelWithClusterStateCache_endpoint_queue_completionSelector
  , readAttributeSelectedDrynessLevelWithCompletionSelector
  , readAttributeSupportedDrynessLevelsWithClusterStateCache_endpoint_queue_completionSelector
  , readAttributeSupportedDrynessLevelsWithCompletionSelector
  , subscribeAttributeAcceptedCommandListWithParams_subscriptionEstablished_reportHandlerSelector
  , subscribeAttributeAttributeListWithParams_subscriptionEstablished_reportHandlerSelector
  , subscribeAttributeClusterRevisionWithParams_subscriptionEstablished_reportHandlerSelector
  , subscribeAttributeFeatureMapWithParams_subscriptionEstablished_reportHandlerSelector
  , subscribeAttributeGeneratedCommandListWithParams_subscriptionEstablished_reportHandlerSelector
  , subscribeAttributeSelectedDrynessLevelWithParams_subscriptionEstablished_reportHandlerSelector
  , subscribeAttributeSupportedDrynessLevelsWithParams_subscriptionEstablished_reportHandlerSelector
  , writeAttributeSelectedDrynessLevelWithValue_completionSelector
  , writeAttributeSelectedDrynessLevelWithValue_params_completionSelector


  ) where

import Foreign.Ptr (Ptr, FunPtr)
import Foreign.C.Types

import ObjC.Runtime.Types
import ObjC.Runtime.Message (sendMessage, sendOwnedMessage, sendClassMessage, sendOwnedClassMessage)
import ObjC.Runtime.Selector (mkSelector)
import ObjC.Runtime.Class (getRequiredClass)

import ObjC.Matter.Internal.Classes
import ObjC.Foundation.Internal.Classes

-- | @- readAttributeSupportedDrynessLevelsWithCompletion:@
readAttributeSupportedDrynessLevelsWithCompletion :: IsMTRBaseClusterLaundryDryerControls mtrBaseClusterLaundryDryerControls => mtrBaseClusterLaundryDryerControls -> Ptr () -> IO ()
readAttributeSupportedDrynessLevelsWithCompletion mtrBaseClusterLaundryDryerControls completion =
  sendMessage mtrBaseClusterLaundryDryerControls readAttributeSupportedDrynessLevelsWithCompletionSelector completion

-- | @- subscribeAttributeSupportedDrynessLevelsWithParams:subscriptionEstablished:reportHandler:@
subscribeAttributeSupportedDrynessLevelsWithParams_subscriptionEstablished_reportHandler :: (IsMTRBaseClusterLaundryDryerControls mtrBaseClusterLaundryDryerControls, IsMTRSubscribeParams params) => mtrBaseClusterLaundryDryerControls -> params -> Ptr () -> Ptr () -> IO ()
subscribeAttributeSupportedDrynessLevelsWithParams_subscriptionEstablished_reportHandler mtrBaseClusterLaundryDryerControls params subscriptionEstablished reportHandler =
  sendMessage mtrBaseClusterLaundryDryerControls subscribeAttributeSupportedDrynessLevelsWithParams_subscriptionEstablished_reportHandlerSelector (toMTRSubscribeParams params) subscriptionEstablished reportHandler

-- | @+ readAttributeSupportedDrynessLevelsWithClusterStateCache:endpoint:queue:completion:@
readAttributeSupportedDrynessLevelsWithClusterStateCache_endpoint_queue_completion :: (IsMTRClusterStateCacheContainer clusterStateCacheContainer, IsNSNumber endpoint, IsNSObject queue) => clusterStateCacheContainer -> endpoint -> queue -> Ptr () -> IO ()
readAttributeSupportedDrynessLevelsWithClusterStateCache_endpoint_queue_completion clusterStateCacheContainer endpoint queue completion =
  do
    cls' <- getRequiredClass "MTRBaseClusterLaundryDryerControls"
    sendClassMessage cls' readAttributeSupportedDrynessLevelsWithClusterStateCache_endpoint_queue_completionSelector (toMTRClusterStateCacheContainer clusterStateCacheContainer) (toNSNumber endpoint) (toNSObject queue) completion

-- | @- readAttributeSelectedDrynessLevelWithCompletion:@
readAttributeSelectedDrynessLevelWithCompletion :: IsMTRBaseClusterLaundryDryerControls mtrBaseClusterLaundryDryerControls => mtrBaseClusterLaundryDryerControls -> Ptr () -> IO ()
readAttributeSelectedDrynessLevelWithCompletion mtrBaseClusterLaundryDryerControls completion =
  sendMessage mtrBaseClusterLaundryDryerControls readAttributeSelectedDrynessLevelWithCompletionSelector completion

-- | @- writeAttributeSelectedDrynessLevelWithValue:completion:@
writeAttributeSelectedDrynessLevelWithValue_completion :: (IsMTRBaseClusterLaundryDryerControls mtrBaseClusterLaundryDryerControls, IsNSNumber value) => mtrBaseClusterLaundryDryerControls -> value -> Ptr () -> IO ()
writeAttributeSelectedDrynessLevelWithValue_completion mtrBaseClusterLaundryDryerControls value completion =
  sendMessage mtrBaseClusterLaundryDryerControls writeAttributeSelectedDrynessLevelWithValue_completionSelector (toNSNumber value) completion

-- | @- writeAttributeSelectedDrynessLevelWithValue:params:completion:@
writeAttributeSelectedDrynessLevelWithValue_params_completion :: (IsMTRBaseClusterLaundryDryerControls mtrBaseClusterLaundryDryerControls, IsNSNumber value, IsMTRWriteParams params) => mtrBaseClusterLaundryDryerControls -> value -> params -> Ptr () -> IO ()
writeAttributeSelectedDrynessLevelWithValue_params_completion mtrBaseClusterLaundryDryerControls value params completion =
  sendMessage mtrBaseClusterLaundryDryerControls writeAttributeSelectedDrynessLevelWithValue_params_completionSelector (toNSNumber value) (toMTRWriteParams params) completion

-- | @- subscribeAttributeSelectedDrynessLevelWithParams:subscriptionEstablished:reportHandler:@
subscribeAttributeSelectedDrynessLevelWithParams_subscriptionEstablished_reportHandler :: (IsMTRBaseClusterLaundryDryerControls mtrBaseClusterLaundryDryerControls, IsMTRSubscribeParams params) => mtrBaseClusterLaundryDryerControls -> params -> Ptr () -> Ptr () -> IO ()
subscribeAttributeSelectedDrynessLevelWithParams_subscriptionEstablished_reportHandler mtrBaseClusterLaundryDryerControls params subscriptionEstablished reportHandler =
  sendMessage mtrBaseClusterLaundryDryerControls subscribeAttributeSelectedDrynessLevelWithParams_subscriptionEstablished_reportHandlerSelector (toMTRSubscribeParams params) subscriptionEstablished reportHandler

-- | @+ readAttributeSelectedDrynessLevelWithClusterStateCache:endpoint:queue:completion:@
readAttributeSelectedDrynessLevelWithClusterStateCache_endpoint_queue_completion :: (IsMTRClusterStateCacheContainer clusterStateCacheContainer, IsNSNumber endpoint, IsNSObject queue) => clusterStateCacheContainer -> endpoint -> queue -> Ptr () -> IO ()
readAttributeSelectedDrynessLevelWithClusterStateCache_endpoint_queue_completion clusterStateCacheContainer endpoint queue completion =
  do
    cls' <- getRequiredClass "MTRBaseClusterLaundryDryerControls"
    sendClassMessage cls' readAttributeSelectedDrynessLevelWithClusterStateCache_endpoint_queue_completionSelector (toMTRClusterStateCacheContainer clusterStateCacheContainer) (toNSNumber endpoint) (toNSObject queue) completion

-- | @- readAttributeGeneratedCommandListWithCompletion:@
readAttributeGeneratedCommandListWithCompletion :: IsMTRBaseClusterLaundryDryerControls mtrBaseClusterLaundryDryerControls => mtrBaseClusterLaundryDryerControls -> Ptr () -> IO ()
readAttributeGeneratedCommandListWithCompletion mtrBaseClusterLaundryDryerControls completion =
  sendMessage mtrBaseClusterLaundryDryerControls readAttributeGeneratedCommandListWithCompletionSelector completion

-- | @- subscribeAttributeGeneratedCommandListWithParams:subscriptionEstablished:reportHandler:@
subscribeAttributeGeneratedCommandListWithParams_subscriptionEstablished_reportHandler :: (IsMTRBaseClusterLaundryDryerControls mtrBaseClusterLaundryDryerControls, IsMTRSubscribeParams params) => mtrBaseClusterLaundryDryerControls -> params -> Ptr () -> Ptr () -> IO ()
subscribeAttributeGeneratedCommandListWithParams_subscriptionEstablished_reportHandler mtrBaseClusterLaundryDryerControls params subscriptionEstablished reportHandler =
  sendMessage mtrBaseClusterLaundryDryerControls subscribeAttributeGeneratedCommandListWithParams_subscriptionEstablished_reportHandlerSelector (toMTRSubscribeParams params) subscriptionEstablished reportHandler

-- | @+ readAttributeGeneratedCommandListWithClusterStateCache:endpoint:queue:completion:@
readAttributeGeneratedCommandListWithClusterStateCache_endpoint_queue_completion :: (IsMTRClusterStateCacheContainer clusterStateCacheContainer, IsNSNumber endpoint, IsNSObject queue) => clusterStateCacheContainer -> endpoint -> queue -> Ptr () -> IO ()
readAttributeGeneratedCommandListWithClusterStateCache_endpoint_queue_completion clusterStateCacheContainer endpoint queue completion =
  do
    cls' <- getRequiredClass "MTRBaseClusterLaundryDryerControls"
    sendClassMessage cls' readAttributeGeneratedCommandListWithClusterStateCache_endpoint_queue_completionSelector (toMTRClusterStateCacheContainer clusterStateCacheContainer) (toNSNumber endpoint) (toNSObject queue) completion

-- | @- readAttributeAcceptedCommandListWithCompletion:@
readAttributeAcceptedCommandListWithCompletion :: IsMTRBaseClusterLaundryDryerControls mtrBaseClusterLaundryDryerControls => mtrBaseClusterLaundryDryerControls -> Ptr () -> IO ()
readAttributeAcceptedCommandListWithCompletion mtrBaseClusterLaundryDryerControls completion =
  sendMessage mtrBaseClusterLaundryDryerControls readAttributeAcceptedCommandListWithCompletionSelector completion

-- | @- subscribeAttributeAcceptedCommandListWithParams:subscriptionEstablished:reportHandler:@
subscribeAttributeAcceptedCommandListWithParams_subscriptionEstablished_reportHandler :: (IsMTRBaseClusterLaundryDryerControls mtrBaseClusterLaundryDryerControls, IsMTRSubscribeParams params) => mtrBaseClusterLaundryDryerControls -> params -> Ptr () -> Ptr () -> IO ()
subscribeAttributeAcceptedCommandListWithParams_subscriptionEstablished_reportHandler mtrBaseClusterLaundryDryerControls params subscriptionEstablished reportHandler =
  sendMessage mtrBaseClusterLaundryDryerControls subscribeAttributeAcceptedCommandListWithParams_subscriptionEstablished_reportHandlerSelector (toMTRSubscribeParams params) subscriptionEstablished reportHandler

-- | @+ readAttributeAcceptedCommandListWithClusterStateCache:endpoint:queue:completion:@
readAttributeAcceptedCommandListWithClusterStateCache_endpoint_queue_completion :: (IsMTRClusterStateCacheContainer clusterStateCacheContainer, IsNSNumber endpoint, IsNSObject queue) => clusterStateCacheContainer -> endpoint -> queue -> Ptr () -> IO ()
readAttributeAcceptedCommandListWithClusterStateCache_endpoint_queue_completion clusterStateCacheContainer endpoint queue completion =
  do
    cls' <- getRequiredClass "MTRBaseClusterLaundryDryerControls"
    sendClassMessage cls' readAttributeAcceptedCommandListWithClusterStateCache_endpoint_queue_completionSelector (toMTRClusterStateCacheContainer clusterStateCacheContainer) (toNSNumber endpoint) (toNSObject queue) completion

-- | @- readAttributeAttributeListWithCompletion:@
readAttributeAttributeListWithCompletion :: IsMTRBaseClusterLaundryDryerControls mtrBaseClusterLaundryDryerControls => mtrBaseClusterLaundryDryerControls -> Ptr () -> IO ()
readAttributeAttributeListWithCompletion mtrBaseClusterLaundryDryerControls completion =
  sendMessage mtrBaseClusterLaundryDryerControls readAttributeAttributeListWithCompletionSelector completion

-- | @- subscribeAttributeAttributeListWithParams:subscriptionEstablished:reportHandler:@
subscribeAttributeAttributeListWithParams_subscriptionEstablished_reportHandler :: (IsMTRBaseClusterLaundryDryerControls mtrBaseClusterLaundryDryerControls, IsMTRSubscribeParams params) => mtrBaseClusterLaundryDryerControls -> params -> Ptr () -> Ptr () -> IO ()
subscribeAttributeAttributeListWithParams_subscriptionEstablished_reportHandler mtrBaseClusterLaundryDryerControls params subscriptionEstablished reportHandler =
  sendMessage mtrBaseClusterLaundryDryerControls subscribeAttributeAttributeListWithParams_subscriptionEstablished_reportHandlerSelector (toMTRSubscribeParams params) subscriptionEstablished reportHandler

-- | @+ readAttributeAttributeListWithClusterStateCache:endpoint:queue:completion:@
readAttributeAttributeListWithClusterStateCache_endpoint_queue_completion :: (IsMTRClusterStateCacheContainer clusterStateCacheContainer, IsNSNumber endpoint, IsNSObject queue) => clusterStateCacheContainer -> endpoint -> queue -> Ptr () -> IO ()
readAttributeAttributeListWithClusterStateCache_endpoint_queue_completion clusterStateCacheContainer endpoint queue completion =
  do
    cls' <- getRequiredClass "MTRBaseClusterLaundryDryerControls"
    sendClassMessage cls' readAttributeAttributeListWithClusterStateCache_endpoint_queue_completionSelector (toMTRClusterStateCacheContainer clusterStateCacheContainer) (toNSNumber endpoint) (toNSObject queue) completion

-- | @- readAttributeFeatureMapWithCompletion:@
readAttributeFeatureMapWithCompletion :: IsMTRBaseClusterLaundryDryerControls mtrBaseClusterLaundryDryerControls => mtrBaseClusterLaundryDryerControls -> Ptr () -> IO ()
readAttributeFeatureMapWithCompletion mtrBaseClusterLaundryDryerControls completion =
  sendMessage mtrBaseClusterLaundryDryerControls readAttributeFeatureMapWithCompletionSelector completion

-- | @- subscribeAttributeFeatureMapWithParams:subscriptionEstablished:reportHandler:@
subscribeAttributeFeatureMapWithParams_subscriptionEstablished_reportHandler :: (IsMTRBaseClusterLaundryDryerControls mtrBaseClusterLaundryDryerControls, IsMTRSubscribeParams params) => mtrBaseClusterLaundryDryerControls -> params -> Ptr () -> Ptr () -> IO ()
subscribeAttributeFeatureMapWithParams_subscriptionEstablished_reportHandler mtrBaseClusterLaundryDryerControls params subscriptionEstablished reportHandler =
  sendMessage mtrBaseClusterLaundryDryerControls subscribeAttributeFeatureMapWithParams_subscriptionEstablished_reportHandlerSelector (toMTRSubscribeParams params) subscriptionEstablished reportHandler

-- | @+ readAttributeFeatureMapWithClusterStateCache:endpoint:queue:completion:@
readAttributeFeatureMapWithClusterStateCache_endpoint_queue_completion :: (IsMTRClusterStateCacheContainer clusterStateCacheContainer, IsNSNumber endpoint, IsNSObject queue) => clusterStateCacheContainer -> endpoint -> queue -> Ptr () -> IO ()
readAttributeFeatureMapWithClusterStateCache_endpoint_queue_completion clusterStateCacheContainer endpoint queue completion =
  do
    cls' <- getRequiredClass "MTRBaseClusterLaundryDryerControls"
    sendClassMessage cls' readAttributeFeatureMapWithClusterStateCache_endpoint_queue_completionSelector (toMTRClusterStateCacheContainer clusterStateCacheContainer) (toNSNumber endpoint) (toNSObject queue) completion

-- | @- readAttributeClusterRevisionWithCompletion:@
readAttributeClusterRevisionWithCompletion :: IsMTRBaseClusterLaundryDryerControls mtrBaseClusterLaundryDryerControls => mtrBaseClusterLaundryDryerControls -> Ptr () -> IO ()
readAttributeClusterRevisionWithCompletion mtrBaseClusterLaundryDryerControls completion =
  sendMessage mtrBaseClusterLaundryDryerControls readAttributeClusterRevisionWithCompletionSelector completion

-- | @- subscribeAttributeClusterRevisionWithParams:subscriptionEstablished:reportHandler:@
subscribeAttributeClusterRevisionWithParams_subscriptionEstablished_reportHandler :: (IsMTRBaseClusterLaundryDryerControls mtrBaseClusterLaundryDryerControls, IsMTRSubscribeParams params) => mtrBaseClusterLaundryDryerControls -> params -> Ptr () -> Ptr () -> IO ()
subscribeAttributeClusterRevisionWithParams_subscriptionEstablished_reportHandler mtrBaseClusterLaundryDryerControls params subscriptionEstablished reportHandler =
  sendMessage mtrBaseClusterLaundryDryerControls subscribeAttributeClusterRevisionWithParams_subscriptionEstablished_reportHandlerSelector (toMTRSubscribeParams params) subscriptionEstablished reportHandler

-- | @+ readAttributeClusterRevisionWithClusterStateCache:endpoint:queue:completion:@
readAttributeClusterRevisionWithClusterStateCache_endpoint_queue_completion :: (IsMTRClusterStateCacheContainer clusterStateCacheContainer, IsNSNumber endpoint, IsNSObject queue) => clusterStateCacheContainer -> endpoint -> queue -> Ptr () -> IO ()
readAttributeClusterRevisionWithClusterStateCache_endpoint_queue_completion clusterStateCacheContainer endpoint queue completion =
  do
    cls' <- getRequiredClass "MTRBaseClusterLaundryDryerControls"
    sendClassMessage cls' readAttributeClusterRevisionWithClusterStateCache_endpoint_queue_completionSelector (toMTRClusterStateCacheContainer clusterStateCacheContainer) (toNSNumber endpoint) (toNSObject queue) completion

-- | @- init@
init_ :: IsMTRBaseClusterLaundryDryerControls mtrBaseClusterLaundryDryerControls => mtrBaseClusterLaundryDryerControls -> IO (Id MTRBaseClusterLaundryDryerControls)
init_ mtrBaseClusterLaundryDryerControls =
  sendOwnedMessage mtrBaseClusterLaundryDryerControls initSelector

-- | @+ new@
new :: IO (Id MTRBaseClusterLaundryDryerControls)
new  =
  do
    cls' <- getRequiredClass "MTRBaseClusterLaundryDryerControls"
    sendOwnedClassMessage cls' newSelector

-- | For all instance methods (reads, writes, commands) that take a completion, the completion will be called on the provided queue.
--
-- ObjC selector: @- initWithDevice:endpointID:queue:@
initWithDevice_endpointID_queue :: (IsMTRBaseClusterLaundryDryerControls mtrBaseClusterLaundryDryerControls, IsMTRBaseDevice device, IsNSNumber endpointID, IsNSObject queue) => mtrBaseClusterLaundryDryerControls -> device -> endpointID -> queue -> IO (Id MTRBaseClusterLaundryDryerControls)
initWithDevice_endpointID_queue mtrBaseClusterLaundryDryerControls device endpointID queue =
  sendOwnedMessage mtrBaseClusterLaundryDryerControls initWithDevice_endpointID_queueSelector (toMTRBaseDevice device) (toNSNumber endpointID) (toNSObject queue)

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @readAttributeSupportedDrynessLevelsWithCompletion:@
readAttributeSupportedDrynessLevelsWithCompletionSelector :: Selector '[Ptr ()] ()
readAttributeSupportedDrynessLevelsWithCompletionSelector = mkSelector "readAttributeSupportedDrynessLevelsWithCompletion:"

-- | @Selector@ for @subscribeAttributeSupportedDrynessLevelsWithParams:subscriptionEstablished:reportHandler:@
subscribeAttributeSupportedDrynessLevelsWithParams_subscriptionEstablished_reportHandlerSelector :: Selector '[Id MTRSubscribeParams, Ptr (), Ptr ()] ()
subscribeAttributeSupportedDrynessLevelsWithParams_subscriptionEstablished_reportHandlerSelector = mkSelector "subscribeAttributeSupportedDrynessLevelsWithParams:subscriptionEstablished:reportHandler:"

-- | @Selector@ for @readAttributeSupportedDrynessLevelsWithClusterStateCache:endpoint:queue:completion:@
readAttributeSupportedDrynessLevelsWithClusterStateCache_endpoint_queue_completionSelector :: Selector '[Id MTRClusterStateCacheContainer, Id NSNumber, Id NSObject, Ptr ()] ()
readAttributeSupportedDrynessLevelsWithClusterStateCache_endpoint_queue_completionSelector = mkSelector "readAttributeSupportedDrynessLevelsWithClusterStateCache:endpoint:queue:completion:"

-- | @Selector@ for @readAttributeSelectedDrynessLevelWithCompletion:@
readAttributeSelectedDrynessLevelWithCompletionSelector :: Selector '[Ptr ()] ()
readAttributeSelectedDrynessLevelWithCompletionSelector = mkSelector "readAttributeSelectedDrynessLevelWithCompletion:"

-- | @Selector@ for @writeAttributeSelectedDrynessLevelWithValue:completion:@
writeAttributeSelectedDrynessLevelWithValue_completionSelector :: Selector '[Id NSNumber, Ptr ()] ()
writeAttributeSelectedDrynessLevelWithValue_completionSelector = mkSelector "writeAttributeSelectedDrynessLevelWithValue:completion:"

-- | @Selector@ for @writeAttributeSelectedDrynessLevelWithValue:params:completion:@
writeAttributeSelectedDrynessLevelWithValue_params_completionSelector :: Selector '[Id NSNumber, Id MTRWriteParams, Ptr ()] ()
writeAttributeSelectedDrynessLevelWithValue_params_completionSelector = mkSelector "writeAttributeSelectedDrynessLevelWithValue:params:completion:"

-- | @Selector@ for @subscribeAttributeSelectedDrynessLevelWithParams:subscriptionEstablished:reportHandler:@
subscribeAttributeSelectedDrynessLevelWithParams_subscriptionEstablished_reportHandlerSelector :: Selector '[Id MTRSubscribeParams, Ptr (), Ptr ()] ()
subscribeAttributeSelectedDrynessLevelWithParams_subscriptionEstablished_reportHandlerSelector = mkSelector "subscribeAttributeSelectedDrynessLevelWithParams:subscriptionEstablished:reportHandler:"

-- | @Selector@ for @readAttributeSelectedDrynessLevelWithClusterStateCache:endpoint:queue:completion:@
readAttributeSelectedDrynessLevelWithClusterStateCache_endpoint_queue_completionSelector :: Selector '[Id MTRClusterStateCacheContainer, Id NSNumber, Id NSObject, Ptr ()] ()
readAttributeSelectedDrynessLevelWithClusterStateCache_endpoint_queue_completionSelector = mkSelector "readAttributeSelectedDrynessLevelWithClusterStateCache:endpoint:queue:completion:"

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
initSelector :: Selector '[] (Id MTRBaseClusterLaundryDryerControls)
initSelector = mkSelector "init"

-- | @Selector@ for @new@
newSelector :: Selector '[] (Id MTRBaseClusterLaundryDryerControls)
newSelector = mkSelector "new"

-- | @Selector@ for @initWithDevice:endpointID:queue:@
initWithDevice_endpointID_queueSelector :: Selector '[Id MTRBaseDevice, Id NSNumber, Id NSObject] (Id MTRBaseClusterLaundryDryerControls)
initWithDevice_endpointID_queueSelector = mkSelector "initWithDevice:endpointID:queue:"

