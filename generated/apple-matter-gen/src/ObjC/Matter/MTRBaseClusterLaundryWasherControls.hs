{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Cluster Laundry Washer Controls
--
-- This cluster supports remotely monitoring and controlling the different types of functionality available to a washing device, such as a washing machine.
--
-- Generated bindings for @MTRBaseClusterLaundryWasherControls@.
module ObjC.Matter.MTRBaseClusterLaundryWasherControls
  ( MTRBaseClusterLaundryWasherControls
  , IsMTRBaseClusterLaundryWasherControls(..)
  , readAttributeSpinSpeedsWithCompletion
  , subscribeAttributeSpinSpeedsWithParams_subscriptionEstablished_reportHandler
  , readAttributeSpinSpeedsWithClusterStateCache_endpoint_queue_completion
  , readAttributeSpinSpeedCurrentWithCompletion
  , writeAttributeSpinSpeedCurrentWithValue_completion
  , writeAttributeSpinSpeedCurrentWithValue_params_completion
  , subscribeAttributeSpinSpeedCurrentWithParams_subscriptionEstablished_reportHandler
  , readAttributeSpinSpeedCurrentWithClusterStateCache_endpoint_queue_completion
  , readAttributeNumberOfRinsesWithCompletion
  , writeAttributeNumberOfRinsesWithValue_completion
  , writeAttributeNumberOfRinsesWithValue_params_completion
  , subscribeAttributeNumberOfRinsesWithParams_subscriptionEstablished_reportHandler
  , readAttributeNumberOfRinsesWithClusterStateCache_endpoint_queue_completion
  , readAttributeSupportedRinsesWithCompletion
  , subscribeAttributeSupportedRinsesWithParams_subscriptionEstablished_reportHandler
  , readAttributeSupportedRinsesWithClusterStateCache_endpoint_queue_completion
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
  , readAttributeNumberOfRinsesWithClusterStateCache_endpoint_queue_completionSelector
  , readAttributeNumberOfRinsesWithCompletionSelector
  , readAttributeSpinSpeedCurrentWithClusterStateCache_endpoint_queue_completionSelector
  , readAttributeSpinSpeedCurrentWithCompletionSelector
  , readAttributeSpinSpeedsWithClusterStateCache_endpoint_queue_completionSelector
  , readAttributeSpinSpeedsWithCompletionSelector
  , readAttributeSupportedRinsesWithClusterStateCache_endpoint_queue_completionSelector
  , readAttributeSupportedRinsesWithCompletionSelector
  , subscribeAttributeAcceptedCommandListWithParams_subscriptionEstablished_reportHandlerSelector
  , subscribeAttributeAttributeListWithParams_subscriptionEstablished_reportHandlerSelector
  , subscribeAttributeClusterRevisionWithParams_subscriptionEstablished_reportHandlerSelector
  , subscribeAttributeFeatureMapWithParams_subscriptionEstablished_reportHandlerSelector
  , subscribeAttributeGeneratedCommandListWithParams_subscriptionEstablished_reportHandlerSelector
  , subscribeAttributeNumberOfRinsesWithParams_subscriptionEstablished_reportHandlerSelector
  , subscribeAttributeSpinSpeedCurrentWithParams_subscriptionEstablished_reportHandlerSelector
  , subscribeAttributeSpinSpeedsWithParams_subscriptionEstablished_reportHandlerSelector
  , subscribeAttributeSupportedRinsesWithParams_subscriptionEstablished_reportHandlerSelector
  , writeAttributeNumberOfRinsesWithValue_completionSelector
  , writeAttributeNumberOfRinsesWithValue_params_completionSelector
  , writeAttributeSpinSpeedCurrentWithValue_completionSelector
  , writeAttributeSpinSpeedCurrentWithValue_params_completionSelector


  ) where

import Foreign.Ptr (Ptr, FunPtr)
import Foreign.C.Types

import ObjC.Runtime.Types
import ObjC.Runtime.Message (sendMessage, sendOwnedMessage, sendClassMessage, sendOwnedClassMessage)
import ObjC.Runtime.Selector (mkSelector)
import ObjC.Runtime.Class (getRequiredClass)

import ObjC.Matter.Internal.Classes
import ObjC.Foundation.Internal.Classes

-- | @- readAttributeSpinSpeedsWithCompletion:@
readAttributeSpinSpeedsWithCompletion :: IsMTRBaseClusterLaundryWasherControls mtrBaseClusterLaundryWasherControls => mtrBaseClusterLaundryWasherControls -> Ptr () -> IO ()
readAttributeSpinSpeedsWithCompletion mtrBaseClusterLaundryWasherControls completion =
  sendMessage mtrBaseClusterLaundryWasherControls readAttributeSpinSpeedsWithCompletionSelector completion

-- | @- subscribeAttributeSpinSpeedsWithParams:subscriptionEstablished:reportHandler:@
subscribeAttributeSpinSpeedsWithParams_subscriptionEstablished_reportHandler :: (IsMTRBaseClusterLaundryWasherControls mtrBaseClusterLaundryWasherControls, IsMTRSubscribeParams params) => mtrBaseClusterLaundryWasherControls -> params -> Ptr () -> Ptr () -> IO ()
subscribeAttributeSpinSpeedsWithParams_subscriptionEstablished_reportHandler mtrBaseClusterLaundryWasherControls params subscriptionEstablished reportHandler =
  sendMessage mtrBaseClusterLaundryWasherControls subscribeAttributeSpinSpeedsWithParams_subscriptionEstablished_reportHandlerSelector (toMTRSubscribeParams params) subscriptionEstablished reportHandler

-- | @+ readAttributeSpinSpeedsWithClusterStateCache:endpoint:queue:completion:@
readAttributeSpinSpeedsWithClusterStateCache_endpoint_queue_completion :: (IsMTRClusterStateCacheContainer clusterStateCacheContainer, IsNSNumber endpoint, IsNSObject queue) => clusterStateCacheContainer -> endpoint -> queue -> Ptr () -> IO ()
readAttributeSpinSpeedsWithClusterStateCache_endpoint_queue_completion clusterStateCacheContainer endpoint queue completion =
  do
    cls' <- getRequiredClass "MTRBaseClusterLaundryWasherControls"
    sendClassMessage cls' readAttributeSpinSpeedsWithClusterStateCache_endpoint_queue_completionSelector (toMTRClusterStateCacheContainer clusterStateCacheContainer) (toNSNumber endpoint) (toNSObject queue) completion

-- | @- readAttributeSpinSpeedCurrentWithCompletion:@
readAttributeSpinSpeedCurrentWithCompletion :: IsMTRBaseClusterLaundryWasherControls mtrBaseClusterLaundryWasherControls => mtrBaseClusterLaundryWasherControls -> Ptr () -> IO ()
readAttributeSpinSpeedCurrentWithCompletion mtrBaseClusterLaundryWasherControls completion =
  sendMessage mtrBaseClusterLaundryWasherControls readAttributeSpinSpeedCurrentWithCompletionSelector completion

-- | @- writeAttributeSpinSpeedCurrentWithValue:completion:@
writeAttributeSpinSpeedCurrentWithValue_completion :: (IsMTRBaseClusterLaundryWasherControls mtrBaseClusterLaundryWasherControls, IsNSNumber value) => mtrBaseClusterLaundryWasherControls -> value -> Ptr () -> IO ()
writeAttributeSpinSpeedCurrentWithValue_completion mtrBaseClusterLaundryWasherControls value completion =
  sendMessage mtrBaseClusterLaundryWasherControls writeAttributeSpinSpeedCurrentWithValue_completionSelector (toNSNumber value) completion

-- | @- writeAttributeSpinSpeedCurrentWithValue:params:completion:@
writeAttributeSpinSpeedCurrentWithValue_params_completion :: (IsMTRBaseClusterLaundryWasherControls mtrBaseClusterLaundryWasherControls, IsNSNumber value, IsMTRWriteParams params) => mtrBaseClusterLaundryWasherControls -> value -> params -> Ptr () -> IO ()
writeAttributeSpinSpeedCurrentWithValue_params_completion mtrBaseClusterLaundryWasherControls value params completion =
  sendMessage mtrBaseClusterLaundryWasherControls writeAttributeSpinSpeedCurrentWithValue_params_completionSelector (toNSNumber value) (toMTRWriteParams params) completion

-- | @- subscribeAttributeSpinSpeedCurrentWithParams:subscriptionEstablished:reportHandler:@
subscribeAttributeSpinSpeedCurrentWithParams_subscriptionEstablished_reportHandler :: (IsMTRBaseClusterLaundryWasherControls mtrBaseClusterLaundryWasherControls, IsMTRSubscribeParams params) => mtrBaseClusterLaundryWasherControls -> params -> Ptr () -> Ptr () -> IO ()
subscribeAttributeSpinSpeedCurrentWithParams_subscriptionEstablished_reportHandler mtrBaseClusterLaundryWasherControls params subscriptionEstablished reportHandler =
  sendMessage mtrBaseClusterLaundryWasherControls subscribeAttributeSpinSpeedCurrentWithParams_subscriptionEstablished_reportHandlerSelector (toMTRSubscribeParams params) subscriptionEstablished reportHandler

-- | @+ readAttributeSpinSpeedCurrentWithClusterStateCache:endpoint:queue:completion:@
readAttributeSpinSpeedCurrentWithClusterStateCache_endpoint_queue_completion :: (IsMTRClusterStateCacheContainer clusterStateCacheContainer, IsNSNumber endpoint, IsNSObject queue) => clusterStateCacheContainer -> endpoint -> queue -> Ptr () -> IO ()
readAttributeSpinSpeedCurrentWithClusterStateCache_endpoint_queue_completion clusterStateCacheContainer endpoint queue completion =
  do
    cls' <- getRequiredClass "MTRBaseClusterLaundryWasherControls"
    sendClassMessage cls' readAttributeSpinSpeedCurrentWithClusterStateCache_endpoint_queue_completionSelector (toMTRClusterStateCacheContainer clusterStateCacheContainer) (toNSNumber endpoint) (toNSObject queue) completion

-- | @- readAttributeNumberOfRinsesWithCompletion:@
readAttributeNumberOfRinsesWithCompletion :: IsMTRBaseClusterLaundryWasherControls mtrBaseClusterLaundryWasherControls => mtrBaseClusterLaundryWasherControls -> Ptr () -> IO ()
readAttributeNumberOfRinsesWithCompletion mtrBaseClusterLaundryWasherControls completion =
  sendMessage mtrBaseClusterLaundryWasherControls readAttributeNumberOfRinsesWithCompletionSelector completion

-- | @- writeAttributeNumberOfRinsesWithValue:completion:@
writeAttributeNumberOfRinsesWithValue_completion :: (IsMTRBaseClusterLaundryWasherControls mtrBaseClusterLaundryWasherControls, IsNSNumber value) => mtrBaseClusterLaundryWasherControls -> value -> Ptr () -> IO ()
writeAttributeNumberOfRinsesWithValue_completion mtrBaseClusterLaundryWasherControls value completion =
  sendMessage mtrBaseClusterLaundryWasherControls writeAttributeNumberOfRinsesWithValue_completionSelector (toNSNumber value) completion

-- | @- writeAttributeNumberOfRinsesWithValue:params:completion:@
writeAttributeNumberOfRinsesWithValue_params_completion :: (IsMTRBaseClusterLaundryWasherControls mtrBaseClusterLaundryWasherControls, IsNSNumber value, IsMTRWriteParams params) => mtrBaseClusterLaundryWasherControls -> value -> params -> Ptr () -> IO ()
writeAttributeNumberOfRinsesWithValue_params_completion mtrBaseClusterLaundryWasherControls value params completion =
  sendMessage mtrBaseClusterLaundryWasherControls writeAttributeNumberOfRinsesWithValue_params_completionSelector (toNSNumber value) (toMTRWriteParams params) completion

-- | @- subscribeAttributeNumberOfRinsesWithParams:subscriptionEstablished:reportHandler:@
subscribeAttributeNumberOfRinsesWithParams_subscriptionEstablished_reportHandler :: (IsMTRBaseClusterLaundryWasherControls mtrBaseClusterLaundryWasherControls, IsMTRSubscribeParams params) => mtrBaseClusterLaundryWasherControls -> params -> Ptr () -> Ptr () -> IO ()
subscribeAttributeNumberOfRinsesWithParams_subscriptionEstablished_reportHandler mtrBaseClusterLaundryWasherControls params subscriptionEstablished reportHandler =
  sendMessage mtrBaseClusterLaundryWasherControls subscribeAttributeNumberOfRinsesWithParams_subscriptionEstablished_reportHandlerSelector (toMTRSubscribeParams params) subscriptionEstablished reportHandler

-- | @+ readAttributeNumberOfRinsesWithClusterStateCache:endpoint:queue:completion:@
readAttributeNumberOfRinsesWithClusterStateCache_endpoint_queue_completion :: (IsMTRClusterStateCacheContainer clusterStateCacheContainer, IsNSNumber endpoint, IsNSObject queue) => clusterStateCacheContainer -> endpoint -> queue -> Ptr () -> IO ()
readAttributeNumberOfRinsesWithClusterStateCache_endpoint_queue_completion clusterStateCacheContainer endpoint queue completion =
  do
    cls' <- getRequiredClass "MTRBaseClusterLaundryWasherControls"
    sendClassMessage cls' readAttributeNumberOfRinsesWithClusterStateCache_endpoint_queue_completionSelector (toMTRClusterStateCacheContainer clusterStateCacheContainer) (toNSNumber endpoint) (toNSObject queue) completion

-- | @- readAttributeSupportedRinsesWithCompletion:@
readAttributeSupportedRinsesWithCompletion :: IsMTRBaseClusterLaundryWasherControls mtrBaseClusterLaundryWasherControls => mtrBaseClusterLaundryWasherControls -> Ptr () -> IO ()
readAttributeSupportedRinsesWithCompletion mtrBaseClusterLaundryWasherControls completion =
  sendMessage mtrBaseClusterLaundryWasherControls readAttributeSupportedRinsesWithCompletionSelector completion

-- | @- subscribeAttributeSupportedRinsesWithParams:subscriptionEstablished:reportHandler:@
subscribeAttributeSupportedRinsesWithParams_subscriptionEstablished_reportHandler :: (IsMTRBaseClusterLaundryWasherControls mtrBaseClusterLaundryWasherControls, IsMTRSubscribeParams params) => mtrBaseClusterLaundryWasherControls -> params -> Ptr () -> Ptr () -> IO ()
subscribeAttributeSupportedRinsesWithParams_subscriptionEstablished_reportHandler mtrBaseClusterLaundryWasherControls params subscriptionEstablished reportHandler =
  sendMessage mtrBaseClusterLaundryWasherControls subscribeAttributeSupportedRinsesWithParams_subscriptionEstablished_reportHandlerSelector (toMTRSubscribeParams params) subscriptionEstablished reportHandler

-- | @+ readAttributeSupportedRinsesWithClusterStateCache:endpoint:queue:completion:@
readAttributeSupportedRinsesWithClusterStateCache_endpoint_queue_completion :: (IsMTRClusterStateCacheContainer clusterStateCacheContainer, IsNSNumber endpoint, IsNSObject queue) => clusterStateCacheContainer -> endpoint -> queue -> Ptr () -> IO ()
readAttributeSupportedRinsesWithClusterStateCache_endpoint_queue_completion clusterStateCacheContainer endpoint queue completion =
  do
    cls' <- getRequiredClass "MTRBaseClusterLaundryWasherControls"
    sendClassMessage cls' readAttributeSupportedRinsesWithClusterStateCache_endpoint_queue_completionSelector (toMTRClusterStateCacheContainer clusterStateCacheContainer) (toNSNumber endpoint) (toNSObject queue) completion

-- | @- readAttributeGeneratedCommandListWithCompletion:@
readAttributeGeneratedCommandListWithCompletion :: IsMTRBaseClusterLaundryWasherControls mtrBaseClusterLaundryWasherControls => mtrBaseClusterLaundryWasherControls -> Ptr () -> IO ()
readAttributeGeneratedCommandListWithCompletion mtrBaseClusterLaundryWasherControls completion =
  sendMessage mtrBaseClusterLaundryWasherControls readAttributeGeneratedCommandListWithCompletionSelector completion

-- | @- subscribeAttributeGeneratedCommandListWithParams:subscriptionEstablished:reportHandler:@
subscribeAttributeGeneratedCommandListWithParams_subscriptionEstablished_reportHandler :: (IsMTRBaseClusterLaundryWasherControls mtrBaseClusterLaundryWasherControls, IsMTRSubscribeParams params) => mtrBaseClusterLaundryWasherControls -> params -> Ptr () -> Ptr () -> IO ()
subscribeAttributeGeneratedCommandListWithParams_subscriptionEstablished_reportHandler mtrBaseClusterLaundryWasherControls params subscriptionEstablished reportHandler =
  sendMessage mtrBaseClusterLaundryWasherControls subscribeAttributeGeneratedCommandListWithParams_subscriptionEstablished_reportHandlerSelector (toMTRSubscribeParams params) subscriptionEstablished reportHandler

-- | @+ readAttributeGeneratedCommandListWithClusterStateCache:endpoint:queue:completion:@
readAttributeGeneratedCommandListWithClusterStateCache_endpoint_queue_completion :: (IsMTRClusterStateCacheContainer clusterStateCacheContainer, IsNSNumber endpoint, IsNSObject queue) => clusterStateCacheContainer -> endpoint -> queue -> Ptr () -> IO ()
readAttributeGeneratedCommandListWithClusterStateCache_endpoint_queue_completion clusterStateCacheContainer endpoint queue completion =
  do
    cls' <- getRequiredClass "MTRBaseClusterLaundryWasherControls"
    sendClassMessage cls' readAttributeGeneratedCommandListWithClusterStateCache_endpoint_queue_completionSelector (toMTRClusterStateCacheContainer clusterStateCacheContainer) (toNSNumber endpoint) (toNSObject queue) completion

-- | @- readAttributeAcceptedCommandListWithCompletion:@
readAttributeAcceptedCommandListWithCompletion :: IsMTRBaseClusterLaundryWasherControls mtrBaseClusterLaundryWasherControls => mtrBaseClusterLaundryWasherControls -> Ptr () -> IO ()
readAttributeAcceptedCommandListWithCompletion mtrBaseClusterLaundryWasherControls completion =
  sendMessage mtrBaseClusterLaundryWasherControls readAttributeAcceptedCommandListWithCompletionSelector completion

-- | @- subscribeAttributeAcceptedCommandListWithParams:subscriptionEstablished:reportHandler:@
subscribeAttributeAcceptedCommandListWithParams_subscriptionEstablished_reportHandler :: (IsMTRBaseClusterLaundryWasherControls mtrBaseClusterLaundryWasherControls, IsMTRSubscribeParams params) => mtrBaseClusterLaundryWasherControls -> params -> Ptr () -> Ptr () -> IO ()
subscribeAttributeAcceptedCommandListWithParams_subscriptionEstablished_reportHandler mtrBaseClusterLaundryWasherControls params subscriptionEstablished reportHandler =
  sendMessage mtrBaseClusterLaundryWasherControls subscribeAttributeAcceptedCommandListWithParams_subscriptionEstablished_reportHandlerSelector (toMTRSubscribeParams params) subscriptionEstablished reportHandler

-- | @+ readAttributeAcceptedCommandListWithClusterStateCache:endpoint:queue:completion:@
readAttributeAcceptedCommandListWithClusterStateCache_endpoint_queue_completion :: (IsMTRClusterStateCacheContainer clusterStateCacheContainer, IsNSNumber endpoint, IsNSObject queue) => clusterStateCacheContainer -> endpoint -> queue -> Ptr () -> IO ()
readAttributeAcceptedCommandListWithClusterStateCache_endpoint_queue_completion clusterStateCacheContainer endpoint queue completion =
  do
    cls' <- getRequiredClass "MTRBaseClusterLaundryWasherControls"
    sendClassMessage cls' readAttributeAcceptedCommandListWithClusterStateCache_endpoint_queue_completionSelector (toMTRClusterStateCacheContainer clusterStateCacheContainer) (toNSNumber endpoint) (toNSObject queue) completion

-- | @- readAttributeAttributeListWithCompletion:@
readAttributeAttributeListWithCompletion :: IsMTRBaseClusterLaundryWasherControls mtrBaseClusterLaundryWasherControls => mtrBaseClusterLaundryWasherControls -> Ptr () -> IO ()
readAttributeAttributeListWithCompletion mtrBaseClusterLaundryWasherControls completion =
  sendMessage mtrBaseClusterLaundryWasherControls readAttributeAttributeListWithCompletionSelector completion

-- | @- subscribeAttributeAttributeListWithParams:subscriptionEstablished:reportHandler:@
subscribeAttributeAttributeListWithParams_subscriptionEstablished_reportHandler :: (IsMTRBaseClusterLaundryWasherControls mtrBaseClusterLaundryWasherControls, IsMTRSubscribeParams params) => mtrBaseClusterLaundryWasherControls -> params -> Ptr () -> Ptr () -> IO ()
subscribeAttributeAttributeListWithParams_subscriptionEstablished_reportHandler mtrBaseClusterLaundryWasherControls params subscriptionEstablished reportHandler =
  sendMessage mtrBaseClusterLaundryWasherControls subscribeAttributeAttributeListWithParams_subscriptionEstablished_reportHandlerSelector (toMTRSubscribeParams params) subscriptionEstablished reportHandler

-- | @+ readAttributeAttributeListWithClusterStateCache:endpoint:queue:completion:@
readAttributeAttributeListWithClusterStateCache_endpoint_queue_completion :: (IsMTRClusterStateCacheContainer clusterStateCacheContainer, IsNSNumber endpoint, IsNSObject queue) => clusterStateCacheContainer -> endpoint -> queue -> Ptr () -> IO ()
readAttributeAttributeListWithClusterStateCache_endpoint_queue_completion clusterStateCacheContainer endpoint queue completion =
  do
    cls' <- getRequiredClass "MTRBaseClusterLaundryWasherControls"
    sendClassMessage cls' readAttributeAttributeListWithClusterStateCache_endpoint_queue_completionSelector (toMTRClusterStateCacheContainer clusterStateCacheContainer) (toNSNumber endpoint) (toNSObject queue) completion

-- | @- readAttributeFeatureMapWithCompletion:@
readAttributeFeatureMapWithCompletion :: IsMTRBaseClusterLaundryWasherControls mtrBaseClusterLaundryWasherControls => mtrBaseClusterLaundryWasherControls -> Ptr () -> IO ()
readAttributeFeatureMapWithCompletion mtrBaseClusterLaundryWasherControls completion =
  sendMessage mtrBaseClusterLaundryWasherControls readAttributeFeatureMapWithCompletionSelector completion

-- | @- subscribeAttributeFeatureMapWithParams:subscriptionEstablished:reportHandler:@
subscribeAttributeFeatureMapWithParams_subscriptionEstablished_reportHandler :: (IsMTRBaseClusterLaundryWasherControls mtrBaseClusterLaundryWasherControls, IsMTRSubscribeParams params) => mtrBaseClusterLaundryWasherControls -> params -> Ptr () -> Ptr () -> IO ()
subscribeAttributeFeatureMapWithParams_subscriptionEstablished_reportHandler mtrBaseClusterLaundryWasherControls params subscriptionEstablished reportHandler =
  sendMessage mtrBaseClusterLaundryWasherControls subscribeAttributeFeatureMapWithParams_subscriptionEstablished_reportHandlerSelector (toMTRSubscribeParams params) subscriptionEstablished reportHandler

-- | @+ readAttributeFeatureMapWithClusterStateCache:endpoint:queue:completion:@
readAttributeFeatureMapWithClusterStateCache_endpoint_queue_completion :: (IsMTRClusterStateCacheContainer clusterStateCacheContainer, IsNSNumber endpoint, IsNSObject queue) => clusterStateCacheContainer -> endpoint -> queue -> Ptr () -> IO ()
readAttributeFeatureMapWithClusterStateCache_endpoint_queue_completion clusterStateCacheContainer endpoint queue completion =
  do
    cls' <- getRequiredClass "MTRBaseClusterLaundryWasherControls"
    sendClassMessage cls' readAttributeFeatureMapWithClusterStateCache_endpoint_queue_completionSelector (toMTRClusterStateCacheContainer clusterStateCacheContainer) (toNSNumber endpoint) (toNSObject queue) completion

-- | @- readAttributeClusterRevisionWithCompletion:@
readAttributeClusterRevisionWithCompletion :: IsMTRBaseClusterLaundryWasherControls mtrBaseClusterLaundryWasherControls => mtrBaseClusterLaundryWasherControls -> Ptr () -> IO ()
readAttributeClusterRevisionWithCompletion mtrBaseClusterLaundryWasherControls completion =
  sendMessage mtrBaseClusterLaundryWasherControls readAttributeClusterRevisionWithCompletionSelector completion

-- | @- subscribeAttributeClusterRevisionWithParams:subscriptionEstablished:reportHandler:@
subscribeAttributeClusterRevisionWithParams_subscriptionEstablished_reportHandler :: (IsMTRBaseClusterLaundryWasherControls mtrBaseClusterLaundryWasherControls, IsMTRSubscribeParams params) => mtrBaseClusterLaundryWasherControls -> params -> Ptr () -> Ptr () -> IO ()
subscribeAttributeClusterRevisionWithParams_subscriptionEstablished_reportHandler mtrBaseClusterLaundryWasherControls params subscriptionEstablished reportHandler =
  sendMessage mtrBaseClusterLaundryWasherControls subscribeAttributeClusterRevisionWithParams_subscriptionEstablished_reportHandlerSelector (toMTRSubscribeParams params) subscriptionEstablished reportHandler

-- | @+ readAttributeClusterRevisionWithClusterStateCache:endpoint:queue:completion:@
readAttributeClusterRevisionWithClusterStateCache_endpoint_queue_completion :: (IsMTRClusterStateCacheContainer clusterStateCacheContainer, IsNSNumber endpoint, IsNSObject queue) => clusterStateCacheContainer -> endpoint -> queue -> Ptr () -> IO ()
readAttributeClusterRevisionWithClusterStateCache_endpoint_queue_completion clusterStateCacheContainer endpoint queue completion =
  do
    cls' <- getRequiredClass "MTRBaseClusterLaundryWasherControls"
    sendClassMessage cls' readAttributeClusterRevisionWithClusterStateCache_endpoint_queue_completionSelector (toMTRClusterStateCacheContainer clusterStateCacheContainer) (toNSNumber endpoint) (toNSObject queue) completion

-- | @- init@
init_ :: IsMTRBaseClusterLaundryWasherControls mtrBaseClusterLaundryWasherControls => mtrBaseClusterLaundryWasherControls -> IO (Id MTRBaseClusterLaundryWasherControls)
init_ mtrBaseClusterLaundryWasherControls =
  sendOwnedMessage mtrBaseClusterLaundryWasherControls initSelector

-- | @+ new@
new :: IO (Id MTRBaseClusterLaundryWasherControls)
new  =
  do
    cls' <- getRequiredClass "MTRBaseClusterLaundryWasherControls"
    sendOwnedClassMessage cls' newSelector

-- | For all instance methods (reads, writes, commands) that take a completion, the completion will be called on the provided queue.
--
-- ObjC selector: @- initWithDevice:endpointID:queue:@
initWithDevice_endpointID_queue :: (IsMTRBaseClusterLaundryWasherControls mtrBaseClusterLaundryWasherControls, IsMTRBaseDevice device, IsNSNumber endpointID, IsNSObject queue) => mtrBaseClusterLaundryWasherControls -> device -> endpointID -> queue -> IO (Id MTRBaseClusterLaundryWasherControls)
initWithDevice_endpointID_queue mtrBaseClusterLaundryWasherControls device endpointID queue =
  sendOwnedMessage mtrBaseClusterLaundryWasherControls initWithDevice_endpointID_queueSelector (toMTRBaseDevice device) (toNSNumber endpointID) (toNSObject queue)

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @readAttributeSpinSpeedsWithCompletion:@
readAttributeSpinSpeedsWithCompletionSelector :: Selector '[Ptr ()] ()
readAttributeSpinSpeedsWithCompletionSelector = mkSelector "readAttributeSpinSpeedsWithCompletion:"

-- | @Selector@ for @subscribeAttributeSpinSpeedsWithParams:subscriptionEstablished:reportHandler:@
subscribeAttributeSpinSpeedsWithParams_subscriptionEstablished_reportHandlerSelector :: Selector '[Id MTRSubscribeParams, Ptr (), Ptr ()] ()
subscribeAttributeSpinSpeedsWithParams_subscriptionEstablished_reportHandlerSelector = mkSelector "subscribeAttributeSpinSpeedsWithParams:subscriptionEstablished:reportHandler:"

-- | @Selector@ for @readAttributeSpinSpeedsWithClusterStateCache:endpoint:queue:completion:@
readAttributeSpinSpeedsWithClusterStateCache_endpoint_queue_completionSelector :: Selector '[Id MTRClusterStateCacheContainer, Id NSNumber, Id NSObject, Ptr ()] ()
readAttributeSpinSpeedsWithClusterStateCache_endpoint_queue_completionSelector = mkSelector "readAttributeSpinSpeedsWithClusterStateCache:endpoint:queue:completion:"

-- | @Selector@ for @readAttributeSpinSpeedCurrentWithCompletion:@
readAttributeSpinSpeedCurrentWithCompletionSelector :: Selector '[Ptr ()] ()
readAttributeSpinSpeedCurrentWithCompletionSelector = mkSelector "readAttributeSpinSpeedCurrentWithCompletion:"

-- | @Selector@ for @writeAttributeSpinSpeedCurrentWithValue:completion:@
writeAttributeSpinSpeedCurrentWithValue_completionSelector :: Selector '[Id NSNumber, Ptr ()] ()
writeAttributeSpinSpeedCurrentWithValue_completionSelector = mkSelector "writeAttributeSpinSpeedCurrentWithValue:completion:"

-- | @Selector@ for @writeAttributeSpinSpeedCurrentWithValue:params:completion:@
writeAttributeSpinSpeedCurrentWithValue_params_completionSelector :: Selector '[Id NSNumber, Id MTRWriteParams, Ptr ()] ()
writeAttributeSpinSpeedCurrentWithValue_params_completionSelector = mkSelector "writeAttributeSpinSpeedCurrentWithValue:params:completion:"

-- | @Selector@ for @subscribeAttributeSpinSpeedCurrentWithParams:subscriptionEstablished:reportHandler:@
subscribeAttributeSpinSpeedCurrentWithParams_subscriptionEstablished_reportHandlerSelector :: Selector '[Id MTRSubscribeParams, Ptr (), Ptr ()] ()
subscribeAttributeSpinSpeedCurrentWithParams_subscriptionEstablished_reportHandlerSelector = mkSelector "subscribeAttributeSpinSpeedCurrentWithParams:subscriptionEstablished:reportHandler:"

-- | @Selector@ for @readAttributeSpinSpeedCurrentWithClusterStateCache:endpoint:queue:completion:@
readAttributeSpinSpeedCurrentWithClusterStateCache_endpoint_queue_completionSelector :: Selector '[Id MTRClusterStateCacheContainer, Id NSNumber, Id NSObject, Ptr ()] ()
readAttributeSpinSpeedCurrentWithClusterStateCache_endpoint_queue_completionSelector = mkSelector "readAttributeSpinSpeedCurrentWithClusterStateCache:endpoint:queue:completion:"

-- | @Selector@ for @readAttributeNumberOfRinsesWithCompletion:@
readAttributeNumberOfRinsesWithCompletionSelector :: Selector '[Ptr ()] ()
readAttributeNumberOfRinsesWithCompletionSelector = mkSelector "readAttributeNumberOfRinsesWithCompletion:"

-- | @Selector@ for @writeAttributeNumberOfRinsesWithValue:completion:@
writeAttributeNumberOfRinsesWithValue_completionSelector :: Selector '[Id NSNumber, Ptr ()] ()
writeAttributeNumberOfRinsesWithValue_completionSelector = mkSelector "writeAttributeNumberOfRinsesWithValue:completion:"

-- | @Selector@ for @writeAttributeNumberOfRinsesWithValue:params:completion:@
writeAttributeNumberOfRinsesWithValue_params_completionSelector :: Selector '[Id NSNumber, Id MTRWriteParams, Ptr ()] ()
writeAttributeNumberOfRinsesWithValue_params_completionSelector = mkSelector "writeAttributeNumberOfRinsesWithValue:params:completion:"

-- | @Selector@ for @subscribeAttributeNumberOfRinsesWithParams:subscriptionEstablished:reportHandler:@
subscribeAttributeNumberOfRinsesWithParams_subscriptionEstablished_reportHandlerSelector :: Selector '[Id MTRSubscribeParams, Ptr (), Ptr ()] ()
subscribeAttributeNumberOfRinsesWithParams_subscriptionEstablished_reportHandlerSelector = mkSelector "subscribeAttributeNumberOfRinsesWithParams:subscriptionEstablished:reportHandler:"

-- | @Selector@ for @readAttributeNumberOfRinsesWithClusterStateCache:endpoint:queue:completion:@
readAttributeNumberOfRinsesWithClusterStateCache_endpoint_queue_completionSelector :: Selector '[Id MTRClusterStateCacheContainer, Id NSNumber, Id NSObject, Ptr ()] ()
readAttributeNumberOfRinsesWithClusterStateCache_endpoint_queue_completionSelector = mkSelector "readAttributeNumberOfRinsesWithClusterStateCache:endpoint:queue:completion:"

-- | @Selector@ for @readAttributeSupportedRinsesWithCompletion:@
readAttributeSupportedRinsesWithCompletionSelector :: Selector '[Ptr ()] ()
readAttributeSupportedRinsesWithCompletionSelector = mkSelector "readAttributeSupportedRinsesWithCompletion:"

-- | @Selector@ for @subscribeAttributeSupportedRinsesWithParams:subscriptionEstablished:reportHandler:@
subscribeAttributeSupportedRinsesWithParams_subscriptionEstablished_reportHandlerSelector :: Selector '[Id MTRSubscribeParams, Ptr (), Ptr ()] ()
subscribeAttributeSupportedRinsesWithParams_subscriptionEstablished_reportHandlerSelector = mkSelector "subscribeAttributeSupportedRinsesWithParams:subscriptionEstablished:reportHandler:"

-- | @Selector@ for @readAttributeSupportedRinsesWithClusterStateCache:endpoint:queue:completion:@
readAttributeSupportedRinsesWithClusterStateCache_endpoint_queue_completionSelector :: Selector '[Id MTRClusterStateCacheContainer, Id NSNumber, Id NSObject, Ptr ()] ()
readAttributeSupportedRinsesWithClusterStateCache_endpoint_queue_completionSelector = mkSelector "readAttributeSupportedRinsesWithClusterStateCache:endpoint:queue:completion:"

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
initSelector :: Selector '[] (Id MTRBaseClusterLaundryWasherControls)
initSelector = mkSelector "init"

-- | @Selector@ for @new@
newSelector :: Selector '[] (Id MTRBaseClusterLaundryWasherControls)
newSelector = mkSelector "new"

-- | @Selector@ for @initWithDevice:endpointID:queue:@
initWithDevice_endpointID_queueSelector :: Selector '[Id MTRBaseDevice, Id NSNumber, Id NSObject] (Id MTRBaseClusterLaundryWasherControls)
initWithDevice_endpointID_queueSelector = mkSelector "initWithDevice:endpointID:queue:"

