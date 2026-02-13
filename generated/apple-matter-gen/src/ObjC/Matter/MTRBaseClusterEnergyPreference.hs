{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Cluster Energy Preference
--
-- This cluster provides an interface to specify preferences for how devices should consume energy.
--
-- Generated bindings for @MTRBaseClusterEnergyPreference@.
module ObjC.Matter.MTRBaseClusterEnergyPreference
  ( MTRBaseClusterEnergyPreference
  , IsMTRBaseClusterEnergyPreference(..)
  , readAttributeEnergyBalancesWithCompletion
  , subscribeAttributeEnergyBalancesWithParams_subscriptionEstablished_reportHandler
  , readAttributeEnergyBalancesWithClusterStateCache_endpoint_queue_completion
  , readAttributeCurrentEnergyBalanceWithCompletion
  , writeAttributeCurrentEnergyBalanceWithValue_completion
  , writeAttributeCurrentEnergyBalanceWithValue_params_completion
  , subscribeAttributeCurrentEnergyBalanceWithParams_subscriptionEstablished_reportHandler
  , readAttributeCurrentEnergyBalanceWithClusterStateCache_endpoint_queue_completion
  , readAttributeEnergyPrioritiesWithCompletion
  , subscribeAttributeEnergyPrioritiesWithParams_subscriptionEstablished_reportHandler
  , readAttributeEnergyPrioritiesWithClusterStateCache_endpoint_queue_completion
  , readAttributeLowPowerModeSensitivitiesWithCompletion
  , subscribeAttributeLowPowerModeSensitivitiesWithParams_subscriptionEstablished_reportHandler
  , readAttributeLowPowerModeSensitivitiesWithClusterStateCache_endpoint_queue_completion
  , readAttributeCurrentLowPowerModeSensitivityWithCompletion
  , writeAttributeCurrentLowPowerModeSensitivityWithValue_completion
  , writeAttributeCurrentLowPowerModeSensitivityWithValue_params_completion
  , subscribeAttributeCurrentLowPowerModeSensitivityWithParams_subscriptionEstablished_reportHandler
  , readAttributeCurrentLowPowerModeSensitivityWithClusterStateCache_endpoint_queue_completion
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
  , readAttributeCurrentEnergyBalanceWithClusterStateCache_endpoint_queue_completionSelector
  , readAttributeCurrentEnergyBalanceWithCompletionSelector
  , readAttributeCurrentLowPowerModeSensitivityWithClusterStateCache_endpoint_queue_completionSelector
  , readAttributeCurrentLowPowerModeSensitivityWithCompletionSelector
  , readAttributeEnergyBalancesWithClusterStateCache_endpoint_queue_completionSelector
  , readAttributeEnergyBalancesWithCompletionSelector
  , readAttributeEnergyPrioritiesWithClusterStateCache_endpoint_queue_completionSelector
  , readAttributeEnergyPrioritiesWithCompletionSelector
  , readAttributeFeatureMapWithClusterStateCache_endpoint_queue_completionSelector
  , readAttributeFeatureMapWithCompletionSelector
  , readAttributeGeneratedCommandListWithClusterStateCache_endpoint_queue_completionSelector
  , readAttributeGeneratedCommandListWithCompletionSelector
  , readAttributeLowPowerModeSensitivitiesWithClusterStateCache_endpoint_queue_completionSelector
  , readAttributeLowPowerModeSensitivitiesWithCompletionSelector
  , subscribeAttributeAcceptedCommandListWithParams_subscriptionEstablished_reportHandlerSelector
  , subscribeAttributeAttributeListWithParams_subscriptionEstablished_reportHandlerSelector
  , subscribeAttributeClusterRevisionWithParams_subscriptionEstablished_reportHandlerSelector
  , subscribeAttributeCurrentEnergyBalanceWithParams_subscriptionEstablished_reportHandlerSelector
  , subscribeAttributeCurrentLowPowerModeSensitivityWithParams_subscriptionEstablished_reportHandlerSelector
  , subscribeAttributeEnergyBalancesWithParams_subscriptionEstablished_reportHandlerSelector
  , subscribeAttributeEnergyPrioritiesWithParams_subscriptionEstablished_reportHandlerSelector
  , subscribeAttributeFeatureMapWithParams_subscriptionEstablished_reportHandlerSelector
  , subscribeAttributeGeneratedCommandListWithParams_subscriptionEstablished_reportHandlerSelector
  , subscribeAttributeLowPowerModeSensitivitiesWithParams_subscriptionEstablished_reportHandlerSelector
  , writeAttributeCurrentEnergyBalanceWithValue_completionSelector
  , writeAttributeCurrentEnergyBalanceWithValue_params_completionSelector
  , writeAttributeCurrentLowPowerModeSensitivityWithValue_completionSelector
  , writeAttributeCurrentLowPowerModeSensitivityWithValue_params_completionSelector


  ) where

import Foreign.Ptr (Ptr, FunPtr)
import Foreign.C.Types

import ObjC.Runtime.Types
import ObjC.Runtime.Message (sendMessage, sendOwnedMessage, sendClassMessage, sendOwnedClassMessage)
import ObjC.Runtime.Selector (mkSelector)
import ObjC.Runtime.Class (getRequiredClass)

import ObjC.Matter.Internal.Classes
import ObjC.Foundation.Internal.Classes

-- | @- readAttributeEnergyBalancesWithCompletion:@
readAttributeEnergyBalancesWithCompletion :: IsMTRBaseClusterEnergyPreference mtrBaseClusterEnergyPreference => mtrBaseClusterEnergyPreference -> Ptr () -> IO ()
readAttributeEnergyBalancesWithCompletion mtrBaseClusterEnergyPreference completion =
  sendMessage mtrBaseClusterEnergyPreference readAttributeEnergyBalancesWithCompletionSelector completion

-- | @- subscribeAttributeEnergyBalancesWithParams:subscriptionEstablished:reportHandler:@
subscribeAttributeEnergyBalancesWithParams_subscriptionEstablished_reportHandler :: (IsMTRBaseClusterEnergyPreference mtrBaseClusterEnergyPreference, IsMTRSubscribeParams params) => mtrBaseClusterEnergyPreference -> params -> Ptr () -> Ptr () -> IO ()
subscribeAttributeEnergyBalancesWithParams_subscriptionEstablished_reportHandler mtrBaseClusterEnergyPreference params subscriptionEstablished reportHandler =
  sendMessage mtrBaseClusterEnergyPreference subscribeAttributeEnergyBalancesWithParams_subscriptionEstablished_reportHandlerSelector (toMTRSubscribeParams params) subscriptionEstablished reportHandler

-- | @+ readAttributeEnergyBalancesWithClusterStateCache:endpoint:queue:completion:@
readAttributeEnergyBalancesWithClusterStateCache_endpoint_queue_completion :: (IsMTRClusterStateCacheContainer clusterStateCacheContainer, IsNSNumber endpoint, IsNSObject queue) => clusterStateCacheContainer -> endpoint -> queue -> Ptr () -> IO ()
readAttributeEnergyBalancesWithClusterStateCache_endpoint_queue_completion clusterStateCacheContainer endpoint queue completion =
  do
    cls' <- getRequiredClass "MTRBaseClusterEnergyPreference"
    sendClassMessage cls' readAttributeEnergyBalancesWithClusterStateCache_endpoint_queue_completionSelector (toMTRClusterStateCacheContainer clusterStateCacheContainer) (toNSNumber endpoint) (toNSObject queue) completion

-- | @- readAttributeCurrentEnergyBalanceWithCompletion:@
readAttributeCurrentEnergyBalanceWithCompletion :: IsMTRBaseClusterEnergyPreference mtrBaseClusterEnergyPreference => mtrBaseClusterEnergyPreference -> Ptr () -> IO ()
readAttributeCurrentEnergyBalanceWithCompletion mtrBaseClusterEnergyPreference completion =
  sendMessage mtrBaseClusterEnergyPreference readAttributeCurrentEnergyBalanceWithCompletionSelector completion

-- | @- writeAttributeCurrentEnergyBalanceWithValue:completion:@
writeAttributeCurrentEnergyBalanceWithValue_completion :: (IsMTRBaseClusterEnergyPreference mtrBaseClusterEnergyPreference, IsNSNumber value) => mtrBaseClusterEnergyPreference -> value -> Ptr () -> IO ()
writeAttributeCurrentEnergyBalanceWithValue_completion mtrBaseClusterEnergyPreference value completion =
  sendMessage mtrBaseClusterEnergyPreference writeAttributeCurrentEnergyBalanceWithValue_completionSelector (toNSNumber value) completion

-- | @- writeAttributeCurrentEnergyBalanceWithValue:params:completion:@
writeAttributeCurrentEnergyBalanceWithValue_params_completion :: (IsMTRBaseClusterEnergyPreference mtrBaseClusterEnergyPreference, IsNSNumber value, IsMTRWriteParams params) => mtrBaseClusterEnergyPreference -> value -> params -> Ptr () -> IO ()
writeAttributeCurrentEnergyBalanceWithValue_params_completion mtrBaseClusterEnergyPreference value params completion =
  sendMessage mtrBaseClusterEnergyPreference writeAttributeCurrentEnergyBalanceWithValue_params_completionSelector (toNSNumber value) (toMTRWriteParams params) completion

-- | @- subscribeAttributeCurrentEnergyBalanceWithParams:subscriptionEstablished:reportHandler:@
subscribeAttributeCurrentEnergyBalanceWithParams_subscriptionEstablished_reportHandler :: (IsMTRBaseClusterEnergyPreference mtrBaseClusterEnergyPreference, IsMTRSubscribeParams params) => mtrBaseClusterEnergyPreference -> params -> Ptr () -> Ptr () -> IO ()
subscribeAttributeCurrentEnergyBalanceWithParams_subscriptionEstablished_reportHandler mtrBaseClusterEnergyPreference params subscriptionEstablished reportHandler =
  sendMessage mtrBaseClusterEnergyPreference subscribeAttributeCurrentEnergyBalanceWithParams_subscriptionEstablished_reportHandlerSelector (toMTRSubscribeParams params) subscriptionEstablished reportHandler

-- | @+ readAttributeCurrentEnergyBalanceWithClusterStateCache:endpoint:queue:completion:@
readAttributeCurrentEnergyBalanceWithClusterStateCache_endpoint_queue_completion :: (IsMTRClusterStateCacheContainer clusterStateCacheContainer, IsNSNumber endpoint, IsNSObject queue) => clusterStateCacheContainer -> endpoint -> queue -> Ptr () -> IO ()
readAttributeCurrentEnergyBalanceWithClusterStateCache_endpoint_queue_completion clusterStateCacheContainer endpoint queue completion =
  do
    cls' <- getRequiredClass "MTRBaseClusterEnergyPreference"
    sendClassMessage cls' readAttributeCurrentEnergyBalanceWithClusterStateCache_endpoint_queue_completionSelector (toMTRClusterStateCacheContainer clusterStateCacheContainer) (toNSNumber endpoint) (toNSObject queue) completion

-- | @- readAttributeEnergyPrioritiesWithCompletion:@
readAttributeEnergyPrioritiesWithCompletion :: IsMTRBaseClusterEnergyPreference mtrBaseClusterEnergyPreference => mtrBaseClusterEnergyPreference -> Ptr () -> IO ()
readAttributeEnergyPrioritiesWithCompletion mtrBaseClusterEnergyPreference completion =
  sendMessage mtrBaseClusterEnergyPreference readAttributeEnergyPrioritiesWithCompletionSelector completion

-- | @- subscribeAttributeEnergyPrioritiesWithParams:subscriptionEstablished:reportHandler:@
subscribeAttributeEnergyPrioritiesWithParams_subscriptionEstablished_reportHandler :: (IsMTRBaseClusterEnergyPreference mtrBaseClusterEnergyPreference, IsMTRSubscribeParams params) => mtrBaseClusterEnergyPreference -> params -> Ptr () -> Ptr () -> IO ()
subscribeAttributeEnergyPrioritiesWithParams_subscriptionEstablished_reportHandler mtrBaseClusterEnergyPreference params subscriptionEstablished reportHandler =
  sendMessage mtrBaseClusterEnergyPreference subscribeAttributeEnergyPrioritiesWithParams_subscriptionEstablished_reportHandlerSelector (toMTRSubscribeParams params) subscriptionEstablished reportHandler

-- | @+ readAttributeEnergyPrioritiesWithClusterStateCache:endpoint:queue:completion:@
readAttributeEnergyPrioritiesWithClusterStateCache_endpoint_queue_completion :: (IsMTRClusterStateCacheContainer clusterStateCacheContainer, IsNSNumber endpoint, IsNSObject queue) => clusterStateCacheContainer -> endpoint -> queue -> Ptr () -> IO ()
readAttributeEnergyPrioritiesWithClusterStateCache_endpoint_queue_completion clusterStateCacheContainer endpoint queue completion =
  do
    cls' <- getRequiredClass "MTRBaseClusterEnergyPreference"
    sendClassMessage cls' readAttributeEnergyPrioritiesWithClusterStateCache_endpoint_queue_completionSelector (toMTRClusterStateCacheContainer clusterStateCacheContainer) (toNSNumber endpoint) (toNSObject queue) completion

-- | @- readAttributeLowPowerModeSensitivitiesWithCompletion:@
readAttributeLowPowerModeSensitivitiesWithCompletion :: IsMTRBaseClusterEnergyPreference mtrBaseClusterEnergyPreference => mtrBaseClusterEnergyPreference -> Ptr () -> IO ()
readAttributeLowPowerModeSensitivitiesWithCompletion mtrBaseClusterEnergyPreference completion =
  sendMessage mtrBaseClusterEnergyPreference readAttributeLowPowerModeSensitivitiesWithCompletionSelector completion

-- | @- subscribeAttributeLowPowerModeSensitivitiesWithParams:subscriptionEstablished:reportHandler:@
subscribeAttributeLowPowerModeSensitivitiesWithParams_subscriptionEstablished_reportHandler :: (IsMTRBaseClusterEnergyPreference mtrBaseClusterEnergyPreference, IsMTRSubscribeParams params) => mtrBaseClusterEnergyPreference -> params -> Ptr () -> Ptr () -> IO ()
subscribeAttributeLowPowerModeSensitivitiesWithParams_subscriptionEstablished_reportHandler mtrBaseClusterEnergyPreference params subscriptionEstablished reportHandler =
  sendMessage mtrBaseClusterEnergyPreference subscribeAttributeLowPowerModeSensitivitiesWithParams_subscriptionEstablished_reportHandlerSelector (toMTRSubscribeParams params) subscriptionEstablished reportHandler

-- | @+ readAttributeLowPowerModeSensitivitiesWithClusterStateCache:endpoint:queue:completion:@
readAttributeLowPowerModeSensitivitiesWithClusterStateCache_endpoint_queue_completion :: (IsMTRClusterStateCacheContainer clusterStateCacheContainer, IsNSNumber endpoint, IsNSObject queue) => clusterStateCacheContainer -> endpoint -> queue -> Ptr () -> IO ()
readAttributeLowPowerModeSensitivitiesWithClusterStateCache_endpoint_queue_completion clusterStateCacheContainer endpoint queue completion =
  do
    cls' <- getRequiredClass "MTRBaseClusterEnergyPreference"
    sendClassMessage cls' readAttributeLowPowerModeSensitivitiesWithClusterStateCache_endpoint_queue_completionSelector (toMTRClusterStateCacheContainer clusterStateCacheContainer) (toNSNumber endpoint) (toNSObject queue) completion

-- | @- readAttributeCurrentLowPowerModeSensitivityWithCompletion:@
readAttributeCurrentLowPowerModeSensitivityWithCompletion :: IsMTRBaseClusterEnergyPreference mtrBaseClusterEnergyPreference => mtrBaseClusterEnergyPreference -> Ptr () -> IO ()
readAttributeCurrentLowPowerModeSensitivityWithCompletion mtrBaseClusterEnergyPreference completion =
  sendMessage mtrBaseClusterEnergyPreference readAttributeCurrentLowPowerModeSensitivityWithCompletionSelector completion

-- | @- writeAttributeCurrentLowPowerModeSensitivityWithValue:completion:@
writeAttributeCurrentLowPowerModeSensitivityWithValue_completion :: (IsMTRBaseClusterEnergyPreference mtrBaseClusterEnergyPreference, IsNSNumber value) => mtrBaseClusterEnergyPreference -> value -> Ptr () -> IO ()
writeAttributeCurrentLowPowerModeSensitivityWithValue_completion mtrBaseClusterEnergyPreference value completion =
  sendMessage mtrBaseClusterEnergyPreference writeAttributeCurrentLowPowerModeSensitivityWithValue_completionSelector (toNSNumber value) completion

-- | @- writeAttributeCurrentLowPowerModeSensitivityWithValue:params:completion:@
writeAttributeCurrentLowPowerModeSensitivityWithValue_params_completion :: (IsMTRBaseClusterEnergyPreference mtrBaseClusterEnergyPreference, IsNSNumber value, IsMTRWriteParams params) => mtrBaseClusterEnergyPreference -> value -> params -> Ptr () -> IO ()
writeAttributeCurrentLowPowerModeSensitivityWithValue_params_completion mtrBaseClusterEnergyPreference value params completion =
  sendMessage mtrBaseClusterEnergyPreference writeAttributeCurrentLowPowerModeSensitivityWithValue_params_completionSelector (toNSNumber value) (toMTRWriteParams params) completion

-- | @- subscribeAttributeCurrentLowPowerModeSensitivityWithParams:subscriptionEstablished:reportHandler:@
subscribeAttributeCurrentLowPowerModeSensitivityWithParams_subscriptionEstablished_reportHandler :: (IsMTRBaseClusterEnergyPreference mtrBaseClusterEnergyPreference, IsMTRSubscribeParams params) => mtrBaseClusterEnergyPreference -> params -> Ptr () -> Ptr () -> IO ()
subscribeAttributeCurrentLowPowerModeSensitivityWithParams_subscriptionEstablished_reportHandler mtrBaseClusterEnergyPreference params subscriptionEstablished reportHandler =
  sendMessage mtrBaseClusterEnergyPreference subscribeAttributeCurrentLowPowerModeSensitivityWithParams_subscriptionEstablished_reportHandlerSelector (toMTRSubscribeParams params) subscriptionEstablished reportHandler

-- | @+ readAttributeCurrentLowPowerModeSensitivityWithClusterStateCache:endpoint:queue:completion:@
readAttributeCurrentLowPowerModeSensitivityWithClusterStateCache_endpoint_queue_completion :: (IsMTRClusterStateCacheContainer clusterStateCacheContainer, IsNSNumber endpoint, IsNSObject queue) => clusterStateCacheContainer -> endpoint -> queue -> Ptr () -> IO ()
readAttributeCurrentLowPowerModeSensitivityWithClusterStateCache_endpoint_queue_completion clusterStateCacheContainer endpoint queue completion =
  do
    cls' <- getRequiredClass "MTRBaseClusterEnergyPreference"
    sendClassMessage cls' readAttributeCurrentLowPowerModeSensitivityWithClusterStateCache_endpoint_queue_completionSelector (toMTRClusterStateCacheContainer clusterStateCacheContainer) (toNSNumber endpoint) (toNSObject queue) completion

-- | @- readAttributeGeneratedCommandListWithCompletion:@
readAttributeGeneratedCommandListWithCompletion :: IsMTRBaseClusterEnergyPreference mtrBaseClusterEnergyPreference => mtrBaseClusterEnergyPreference -> Ptr () -> IO ()
readAttributeGeneratedCommandListWithCompletion mtrBaseClusterEnergyPreference completion =
  sendMessage mtrBaseClusterEnergyPreference readAttributeGeneratedCommandListWithCompletionSelector completion

-- | @- subscribeAttributeGeneratedCommandListWithParams:subscriptionEstablished:reportHandler:@
subscribeAttributeGeneratedCommandListWithParams_subscriptionEstablished_reportHandler :: (IsMTRBaseClusterEnergyPreference mtrBaseClusterEnergyPreference, IsMTRSubscribeParams params) => mtrBaseClusterEnergyPreference -> params -> Ptr () -> Ptr () -> IO ()
subscribeAttributeGeneratedCommandListWithParams_subscriptionEstablished_reportHandler mtrBaseClusterEnergyPreference params subscriptionEstablished reportHandler =
  sendMessage mtrBaseClusterEnergyPreference subscribeAttributeGeneratedCommandListWithParams_subscriptionEstablished_reportHandlerSelector (toMTRSubscribeParams params) subscriptionEstablished reportHandler

-- | @+ readAttributeGeneratedCommandListWithClusterStateCache:endpoint:queue:completion:@
readAttributeGeneratedCommandListWithClusterStateCache_endpoint_queue_completion :: (IsMTRClusterStateCacheContainer clusterStateCacheContainer, IsNSNumber endpoint, IsNSObject queue) => clusterStateCacheContainer -> endpoint -> queue -> Ptr () -> IO ()
readAttributeGeneratedCommandListWithClusterStateCache_endpoint_queue_completion clusterStateCacheContainer endpoint queue completion =
  do
    cls' <- getRequiredClass "MTRBaseClusterEnergyPreference"
    sendClassMessage cls' readAttributeGeneratedCommandListWithClusterStateCache_endpoint_queue_completionSelector (toMTRClusterStateCacheContainer clusterStateCacheContainer) (toNSNumber endpoint) (toNSObject queue) completion

-- | @- readAttributeAcceptedCommandListWithCompletion:@
readAttributeAcceptedCommandListWithCompletion :: IsMTRBaseClusterEnergyPreference mtrBaseClusterEnergyPreference => mtrBaseClusterEnergyPreference -> Ptr () -> IO ()
readAttributeAcceptedCommandListWithCompletion mtrBaseClusterEnergyPreference completion =
  sendMessage mtrBaseClusterEnergyPreference readAttributeAcceptedCommandListWithCompletionSelector completion

-- | @- subscribeAttributeAcceptedCommandListWithParams:subscriptionEstablished:reportHandler:@
subscribeAttributeAcceptedCommandListWithParams_subscriptionEstablished_reportHandler :: (IsMTRBaseClusterEnergyPreference mtrBaseClusterEnergyPreference, IsMTRSubscribeParams params) => mtrBaseClusterEnergyPreference -> params -> Ptr () -> Ptr () -> IO ()
subscribeAttributeAcceptedCommandListWithParams_subscriptionEstablished_reportHandler mtrBaseClusterEnergyPreference params subscriptionEstablished reportHandler =
  sendMessage mtrBaseClusterEnergyPreference subscribeAttributeAcceptedCommandListWithParams_subscriptionEstablished_reportHandlerSelector (toMTRSubscribeParams params) subscriptionEstablished reportHandler

-- | @+ readAttributeAcceptedCommandListWithClusterStateCache:endpoint:queue:completion:@
readAttributeAcceptedCommandListWithClusterStateCache_endpoint_queue_completion :: (IsMTRClusterStateCacheContainer clusterStateCacheContainer, IsNSNumber endpoint, IsNSObject queue) => clusterStateCacheContainer -> endpoint -> queue -> Ptr () -> IO ()
readAttributeAcceptedCommandListWithClusterStateCache_endpoint_queue_completion clusterStateCacheContainer endpoint queue completion =
  do
    cls' <- getRequiredClass "MTRBaseClusterEnergyPreference"
    sendClassMessage cls' readAttributeAcceptedCommandListWithClusterStateCache_endpoint_queue_completionSelector (toMTRClusterStateCacheContainer clusterStateCacheContainer) (toNSNumber endpoint) (toNSObject queue) completion

-- | @- readAttributeAttributeListWithCompletion:@
readAttributeAttributeListWithCompletion :: IsMTRBaseClusterEnergyPreference mtrBaseClusterEnergyPreference => mtrBaseClusterEnergyPreference -> Ptr () -> IO ()
readAttributeAttributeListWithCompletion mtrBaseClusterEnergyPreference completion =
  sendMessage mtrBaseClusterEnergyPreference readAttributeAttributeListWithCompletionSelector completion

-- | @- subscribeAttributeAttributeListWithParams:subscriptionEstablished:reportHandler:@
subscribeAttributeAttributeListWithParams_subscriptionEstablished_reportHandler :: (IsMTRBaseClusterEnergyPreference mtrBaseClusterEnergyPreference, IsMTRSubscribeParams params) => mtrBaseClusterEnergyPreference -> params -> Ptr () -> Ptr () -> IO ()
subscribeAttributeAttributeListWithParams_subscriptionEstablished_reportHandler mtrBaseClusterEnergyPreference params subscriptionEstablished reportHandler =
  sendMessage mtrBaseClusterEnergyPreference subscribeAttributeAttributeListWithParams_subscriptionEstablished_reportHandlerSelector (toMTRSubscribeParams params) subscriptionEstablished reportHandler

-- | @+ readAttributeAttributeListWithClusterStateCache:endpoint:queue:completion:@
readAttributeAttributeListWithClusterStateCache_endpoint_queue_completion :: (IsMTRClusterStateCacheContainer clusterStateCacheContainer, IsNSNumber endpoint, IsNSObject queue) => clusterStateCacheContainer -> endpoint -> queue -> Ptr () -> IO ()
readAttributeAttributeListWithClusterStateCache_endpoint_queue_completion clusterStateCacheContainer endpoint queue completion =
  do
    cls' <- getRequiredClass "MTRBaseClusterEnergyPreference"
    sendClassMessage cls' readAttributeAttributeListWithClusterStateCache_endpoint_queue_completionSelector (toMTRClusterStateCacheContainer clusterStateCacheContainer) (toNSNumber endpoint) (toNSObject queue) completion

-- | @- readAttributeFeatureMapWithCompletion:@
readAttributeFeatureMapWithCompletion :: IsMTRBaseClusterEnergyPreference mtrBaseClusterEnergyPreference => mtrBaseClusterEnergyPreference -> Ptr () -> IO ()
readAttributeFeatureMapWithCompletion mtrBaseClusterEnergyPreference completion =
  sendMessage mtrBaseClusterEnergyPreference readAttributeFeatureMapWithCompletionSelector completion

-- | @- subscribeAttributeFeatureMapWithParams:subscriptionEstablished:reportHandler:@
subscribeAttributeFeatureMapWithParams_subscriptionEstablished_reportHandler :: (IsMTRBaseClusterEnergyPreference mtrBaseClusterEnergyPreference, IsMTRSubscribeParams params) => mtrBaseClusterEnergyPreference -> params -> Ptr () -> Ptr () -> IO ()
subscribeAttributeFeatureMapWithParams_subscriptionEstablished_reportHandler mtrBaseClusterEnergyPreference params subscriptionEstablished reportHandler =
  sendMessage mtrBaseClusterEnergyPreference subscribeAttributeFeatureMapWithParams_subscriptionEstablished_reportHandlerSelector (toMTRSubscribeParams params) subscriptionEstablished reportHandler

-- | @+ readAttributeFeatureMapWithClusterStateCache:endpoint:queue:completion:@
readAttributeFeatureMapWithClusterStateCache_endpoint_queue_completion :: (IsMTRClusterStateCacheContainer clusterStateCacheContainer, IsNSNumber endpoint, IsNSObject queue) => clusterStateCacheContainer -> endpoint -> queue -> Ptr () -> IO ()
readAttributeFeatureMapWithClusterStateCache_endpoint_queue_completion clusterStateCacheContainer endpoint queue completion =
  do
    cls' <- getRequiredClass "MTRBaseClusterEnergyPreference"
    sendClassMessage cls' readAttributeFeatureMapWithClusterStateCache_endpoint_queue_completionSelector (toMTRClusterStateCacheContainer clusterStateCacheContainer) (toNSNumber endpoint) (toNSObject queue) completion

-- | @- readAttributeClusterRevisionWithCompletion:@
readAttributeClusterRevisionWithCompletion :: IsMTRBaseClusterEnergyPreference mtrBaseClusterEnergyPreference => mtrBaseClusterEnergyPreference -> Ptr () -> IO ()
readAttributeClusterRevisionWithCompletion mtrBaseClusterEnergyPreference completion =
  sendMessage mtrBaseClusterEnergyPreference readAttributeClusterRevisionWithCompletionSelector completion

-- | @- subscribeAttributeClusterRevisionWithParams:subscriptionEstablished:reportHandler:@
subscribeAttributeClusterRevisionWithParams_subscriptionEstablished_reportHandler :: (IsMTRBaseClusterEnergyPreference mtrBaseClusterEnergyPreference, IsMTRSubscribeParams params) => mtrBaseClusterEnergyPreference -> params -> Ptr () -> Ptr () -> IO ()
subscribeAttributeClusterRevisionWithParams_subscriptionEstablished_reportHandler mtrBaseClusterEnergyPreference params subscriptionEstablished reportHandler =
  sendMessage mtrBaseClusterEnergyPreference subscribeAttributeClusterRevisionWithParams_subscriptionEstablished_reportHandlerSelector (toMTRSubscribeParams params) subscriptionEstablished reportHandler

-- | @+ readAttributeClusterRevisionWithClusterStateCache:endpoint:queue:completion:@
readAttributeClusterRevisionWithClusterStateCache_endpoint_queue_completion :: (IsMTRClusterStateCacheContainer clusterStateCacheContainer, IsNSNumber endpoint, IsNSObject queue) => clusterStateCacheContainer -> endpoint -> queue -> Ptr () -> IO ()
readAttributeClusterRevisionWithClusterStateCache_endpoint_queue_completion clusterStateCacheContainer endpoint queue completion =
  do
    cls' <- getRequiredClass "MTRBaseClusterEnergyPreference"
    sendClassMessage cls' readAttributeClusterRevisionWithClusterStateCache_endpoint_queue_completionSelector (toMTRClusterStateCacheContainer clusterStateCacheContainer) (toNSNumber endpoint) (toNSObject queue) completion

-- | @- init@
init_ :: IsMTRBaseClusterEnergyPreference mtrBaseClusterEnergyPreference => mtrBaseClusterEnergyPreference -> IO (Id MTRBaseClusterEnergyPreference)
init_ mtrBaseClusterEnergyPreference =
  sendOwnedMessage mtrBaseClusterEnergyPreference initSelector

-- | @+ new@
new :: IO (Id MTRBaseClusterEnergyPreference)
new  =
  do
    cls' <- getRequiredClass "MTRBaseClusterEnergyPreference"
    sendOwnedClassMessage cls' newSelector

-- | For all instance methods (reads, writes, commands) that take a completion, the completion will be called on the provided queue.
--
-- ObjC selector: @- initWithDevice:endpointID:queue:@
initWithDevice_endpointID_queue :: (IsMTRBaseClusterEnergyPreference mtrBaseClusterEnergyPreference, IsMTRBaseDevice device, IsNSNumber endpointID, IsNSObject queue) => mtrBaseClusterEnergyPreference -> device -> endpointID -> queue -> IO (Id MTRBaseClusterEnergyPreference)
initWithDevice_endpointID_queue mtrBaseClusterEnergyPreference device endpointID queue =
  sendOwnedMessage mtrBaseClusterEnergyPreference initWithDevice_endpointID_queueSelector (toMTRBaseDevice device) (toNSNumber endpointID) (toNSObject queue)

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @readAttributeEnergyBalancesWithCompletion:@
readAttributeEnergyBalancesWithCompletionSelector :: Selector '[Ptr ()] ()
readAttributeEnergyBalancesWithCompletionSelector = mkSelector "readAttributeEnergyBalancesWithCompletion:"

-- | @Selector@ for @subscribeAttributeEnergyBalancesWithParams:subscriptionEstablished:reportHandler:@
subscribeAttributeEnergyBalancesWithParams_subscriptionEstablished_reportHandlerSelector :: Selector '[Id MTRSubscribeParams, Ptr (), Ptr ()] ()
subscribeAttributeEnergyBalancesWithParams_subscriptionEstablished_reportHandlerSelector = mkSelector "subscribeAttributeEnergyBalancesWithParams:subscriptionEstablished:reportHandler:"

-- | @Selector@ for @readAttributeEnergyBalancesWithClusterStateCache:endpoint:queue:completion:@
readAttributeEnergyBalancesWithClusterStateCache_endpoint_queue_completionSelector :: Selector '[Id MTRClusterStateCacheContainer, Id NSNumber, Id NSObject, Ptr ()] ()
readAttributeEnergyBalancesWithClusterStateCache_endpoint_queue_completionSelector = mkSelector "readAttributeEnergyBalancesWithClusterStateCache:endpoint:queue:completion:"

-- | @Selector@ for @readAttributeCurrentEnergyBalanceWithCompletion:@
readAttributeCurrentEnergyBalanceWithCompletionSelector :: Selector '[Ptr ()] ()
readAttributeCurrentEnergyBalanceWithCompletionSelector = mkSelector "readAttributeCurrentEnergyBalanceWithCompletion:"

-- | @Selector@ for @writeAttributeCurrentEnergyBalanceWithValue:completion:@
writeAttributeCurrentEnergyBalanceWithValue_completionSelector :: Selector '[Id NSNumber, Ptr ()] ()
writeAttributeCurrentEnergyBalanceWithValue_completionSelector = mkSelector "writeAttributeCurrentEnergyBalanceWithValue:completion:"

-- | @Selector@ for @writeAttributeCurrentEnergyBalanceWithValue:params:completion:@
writeAttributeCurrentEnergyBalanceWithValue_params_completionSelector :: Selector '[Id NSNumber, Id MTRWriteParams, Ptr ()] ()
writeAttributeCurrentEnergyBalanceWithValue_params_completionSelector = mkSelector "writeAttributeCurrentEnergyBalanceWithValue:params:completion:"

-- | @Selector@ for @subscribeAttributeCurrentEnergyBalanceWithParams:subscriptionEstablished:reportHandler:@
subscribeAttributeCurrentEnergyBalanceWithParams_subscriptionEstablished_reportHandlerSelector :: Selector '[Id MTRSubscribeParams, Ptr (), Ptr ()] ()
subscribeAttributeCurrentEnergyBalanceWithParams_subscriptionEstablished_reportHandlerSelector = mkSelector "subscribeAttributeCurrentEnergyBalanceWithParams:subscriptionEstablished:reportHandler:"

-- | @Selector@ for @readAttributeCurrentEnergyBalanceWithClusterStateCache:endpoint:queue:completion:@
readAttributeCurrentEnergyBalanceWithClusterStateCache_endpoint_queue_completionSelector :: Selector '[Id MTRClusterStateCacheContainer, Id NSNumber, Id NSObject, Ptr ()] ()
readAttributeCurrentEnergyBalanceWithClusterStateCache_endpoint_queue_completionSelector = mkSelector "readAttributeCurrentEnergyBalanceWithClusterStateCache:endpoint:queue:completion:"

-- | @Selector@ for @readAttributeEnergyPrioritiesWithCompletion:@
readAttributeEnergyPrioritiesWithCompletionSelector :: Selector '[Ptr ()] ()
readAttributeEnergyPrioritiesWithCompletionSelector = mkSelector "readAttributeEnergyPrioritiesWithCompletion:"

-- | @Selector@ for @subscribeAttributeEnergyPrioritiesWithParams:subscriptionEstablished:reportHandler:@
subscribeAttributeEnergyPrioritiesWithParams_subscriptionEstablished_reportHandlerSelector :: Selector '[Id MTRSubscribeParams, Ptr (), Ptr ()] ()
subscribeAttributeEnergyPrioritiesWithParams_subscriptionEstablished_reportHandlerSelector = mkSelector "subscribeAttributeEnergyPrioritiesWithParams:subscriptionEstablished:reportHandler:"

-- | @Selector@ for @readAttributeEnergyPrioritiesWithClusterStateCache:endpoint:queue:completion:@
readAttributeEnergyPrioritiesWithClusterStateCache_endpoint_queue_completionSelector :: Selector '[Id MTRClusterStateCacheContainer, Id NSNumber, Id NSObject, Ptr ()] ()
readAttributeEnergyPrioritiesWithClusterStateCache_endpoint_queue_completionSelector = mkSelector "readAttributeEnergyPrioritiesWithClusterStateCache:endpoint:queue:completion:"

-- | @Selector@ for @readAttributeLowPowerModeSensitivitiesWithCompletion:@
readAttributeLowPowerModeSensitivitiesWithCompletionSelector :: Selector '[Ptr ()] ()
readAttributeLowPowerModeSensitivitiesWithCompletionSelector = mkSelector "readAttributeLowPowerModeSensitivitiesWithCompletion:"

-- | @Selector@ for @subscribeAttributeLowPowerModeSensitivitiesWithParams:subscriptionEstablished:reportHandler:@
subscribeAttributeLowPowerModeSensitivitiesWithParams_subscriptionEstablished_reportHandlerSelector :: Selector '[Id MTRSubscribeParams, Ptr (), Ptr ()] ()
subscribeAttributeLowPowerModeSensitivitiesWithParams_subscriptionEstablished_reportHandlerSelector = mkSelector "subscribeAttributeLowPowerModeSensitivitiesWithParams:subscriptionEstablished:reportHandler:"

-- | @Selector@ for @readAttributeLowPowerModeSensitivitiesWithClusterStateCache:endpoint:queue:completion:@
readAttributeLowPowerModeSensitivitiesWithClusterStateCache_endpoint_queue_completionSelector :: Selector '[Id MTRClusterStateCacheContainer, Id NSNumber, Id NSObject, Ptr ()] ()
readAttributeLowPowerModeSensitivitiesWithClusterStateCache_endpoint_queue_completionSelector = mkSelector "readAttributeLowPowerModeSensitivitiesWithClusterStateCache:endpoint:queue:completion:"

-- | @Selector@ for @readAttributeCurrentLowPowerModeSensitivityWithCompletion:@
readAttributeCurrentLowPowerModeSensitivityWithCompletionSelector :: Selector '[Ptr ()] ()
readAttributeCurrentLowPowerModeSensitivityWithCompletionSelector = mkSelector "readAttributeCurrentLowPowerModeSensitivityWithCompletion:"

-- | @Selector@ for @writeAttributeCurrentLowPowerModeSensitivityWithValue:completion:@
writeAttributeCurrentLowPowerModeSensitivityWithValue_completionSelector :: Selector '[Id NSNumber, Ptr ()] ()
writeAttributeCurrentLowPowerModeSensitivityWithValue_completionSelector = mkSelector "writeAttributeCurrentLowPowerModeSensitivityWithValue:completion:"

-- | @Selector@ for @writeAttributeCurrentLowPowerModeSensitivityWithValue:params:completion:@
writeAttributeCurrentLowPowerModeSensitivityWithValue_params_completionSelector :: Selector '[Id NSNumber, Id MTRWriteParams, Ptr ()] ()
writeAttributeCurrentLowPowerModeSensitivityWithValue_params_completionSelector = mkSelector "writeAttributeCurrentLowPowerModeSensitivityWithValue:params:completion:"

-- | @Selector@ for @subscribeAttributeCurrentLowPowerModeSensitivityWithParams:subscriptionEstablished:reportHandler:@
subscribeAttributeCurrentLowPowerModeSensitivityWithParams_subscriptionEstablished_reportHandlerSelector :: Selector '[Id MTRSubscribeParams, Ptr (), Ptr ()] ()
subscribeAttributeCurrentLowPowerModeSensitivityWithParams_subscriptionEstablished_reportHandlerSelector = mkSelector "subscribeAttributeCurrentLowPowerModeSensitivityWithParams:subscriptionEstablished:reportHandler:"

-- | @Selector@ for @readAttributeCurrentLowPowerModeSensitivityWithClusterStateCache:endpoint:queue:completion:@
readAttributeCurrentLowPowerModeSensitivityWithClusterStateCache_endpoint_queue_completionSelector :: Selector '[Id MTRClusterStateCacheContainer, Id NSNumber, Id NSObject, Ptr ()] ()
readAttributeCurrentLowPowerModeSensitivityWithClusterStateCache_endpoint_queue_completionSelector = mkSelector "readAttributeCurrentLowPowerModeSensitivityWithClusterStateCache:endpoint:queue:completion:"

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
initSelector :: Selector '[] (Id MTRBaseClusterEnergyPreference)
initSelector = mkSelector "init"

-- | @Selector@ for @new@
newSelector :: Selector '[] (Id MTRBaseClusterEnergyPreference)
newSelector = mkSelector "new"

-- | @Selector@ for @initWithDevice:endpointID:queue:@
initWithDevice_endpointID_queueSelector :: Selector '[Id MTRBaseDevice, Id NSNumber, Id NSObject] (Id MTRBaseClusterEnergyPreference)
initWithDevice_endpointID_queueSelector = mkSelector "initWithDevice:endpointID:queue:"

