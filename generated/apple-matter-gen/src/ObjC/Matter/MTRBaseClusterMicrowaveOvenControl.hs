{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Cluster Microwave Oven Control
--
-- Attributes and commands for configuring the microwave oven control, and reporting cooking stats.
--
-- Generated bindings for @MTRBaseClusterMicrowaveOvenControl@.
module ObjC.Matter.MTRBaseClusterMicrowaveOvenControl
  ( MTRBaseClusterMicrowaveOvenControl
  , IsMTRBaseClusterMicrowaveOvenControl(..)
  , setCookingParametersWithParams_completion
  , setCookingParametersWithCompletion
  , addMoreTimeWithParams_completion
  , readAttributeCookTimeWithCompletion
  , subscribeAttributeCookTimeWithParams_subscriptionEstablished_reportHandler
  , readAttributeCookTimeWithClusterStateCache_endpoint_queue_completion
  , readAttributeMaxCookTimeWithCompletion
  , subscribeAttributeMaxCookTimeWithParams_subscriptionEstablished_reportHandler
  , readAttributeMaxCookTimeWithClusterStateCache_endpoint_queue_completion
  , readAttributePowerSettingWithCompletion
  , subscribeAttributePowerSettingWithParams_subscriptionEstablished_reportHandler
  , readAttributePowerSettingWithClusterStateCache_endpoint_queue_completion
  , readAttributeMinPowerWithCompletion
  , subscribeAttributeMinPowerWithParams_subscriptionEstablished_reportHandler
  , readAttributeMinPowerWithClusterStateCache_endpoint_queue_completion
  , readAttributeMaxPowerWithCompletion
  , subscribeAttributeMaxPowerWithParams_subscriptionEstablished_reportHandler
  , readAttributeMaxPowerWithClusterStateCache_endpoint_queue_completion
  , readAttributePowerStepWithCompletion
  , subscribeAttributePowerStepWithParams_subscriptionEstablished_reportHandler
  , readAttributePowerStepWithClusterStateCache_endpoint_queue_completion
  , readAttributeSupportedWattsWithCompletion
  , subscribeAttributeSupportedWattsWithParams_subscriptionEstablished_reportHandler
  , readAttributeSupportedWattsWithClusterStateCache_endpoint_queue_completion
  , readAttributeSelectedWattIndexWithCompletion
  , subscribeAttributeSelectedWattIndexWithParams_subscriptionEstablished_reportHandler
  , readAttributeSelectedWattIndexWithClusterStateCache_endpoint_queue_completion
  , readAttributeWattRatingWithCompletion
  , subscribeAttributeWattRatingWithParams_subscriptionEstablished_reportHandler
  , readAttributeWattRatingWithClusterStateCache_endpoint_queue_completion
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
  , addMoreTimeWithParams_completionSelector
  , initSelector
  , initWithDevice_endpointID_queueSelector
  , newSelector
  , readAttributeAcceptedCommandListWithClusterStateCache_endpoint_queue_completionSelector
  , readAttributeAcceptedCommandListWithCompletionSelector
  , readAttributeAttributeListWithClusterStateCache_endpoint_queue_completionSelector
  , readAttributeAttributeListWithCompletionSelector
  , readAttributeClusterRevisionWithClusterStateCache_endpoint_queue_completionSelector
  , readAttributeClusterRevisionWithCompletionSelector
  , readAttributeCookTimeWithClusterStateCache_endpoint_queue_completionSelector
  , readAttributeCookTimeWithCompletionSelector
  , readAttributeFeatureMapWithClusterStateCache_endpoint_queue_completionSelector
  , readAttributeFeatureMapWithCompletionSelector
  , readAttributeGeneratedCommandListWithClusterStateCache_endpoint_queue_completionSelector
  , readAttributeGeneratedCommandListWithCompletionSelector
  , readAttributeMaxCookTimeWithClusterStateCache_endpoint_queue_completionSelector
  , readAttributeMaxCookTimeWithCompletionSelector
  , readAttributeMaxPowerWithClusterStateCache_endpoint_queue_completionSelector
  , readAttributeMaxPowerWithCompletionSelector
  , readAttributeMinPowerWithClusterStateCache_endpoint_queue_completionSelector
  , readAttributeMinPowerWithCompletionSelector
  , readAttributePowerSettingWithClusterStateCache_endpoint_queue_completionSelector
  , readAttributePowerSettingWithCompletionSelector
  , readAttributePowerStepWithClusterStateCache_endpoint_queue_completionSelector
  , readAttributePowerStepWithCompletionSelector
  , readAttributeSelectedWattIndexWithClusterStateCache_endpoint_queue_completionSelector
  , readAttributeSelectedWattIndexWithCompletionSelector
  , readAttributeSupportedWattsWithClusterStateCache_endpoint_queue_completionSelector
  , readAttributeSupportedWattsWithCompletionSelector
  , readAttributeWattRatingWithClusterStateCache_endpoint_queue_completionSelector
  , readAttributeWattRatingWithCompletionSelector
  , setCookingParametersWithCompletionSelector
  , setCookingParametersWithParams_completionSelector
  , subscribeAttributeAcceptedCommandListWithParams_subscriptionEstablished_reportHandlerSelector
  , subscribeAttributeAttributeListWithParams_subscriptionEstablished_reportHandlerSelector
  , subscribeAttributeClusterRevisionWithParams_subscriptionEstablished_reportHandlerSelector
  , subscribeAttributeCookTimeWithParams_subscriptionEstablished_reportHandlerSelector
  , subscribeAttributeFeatureMapWithParams_subscriptionEstablished_reportHandlerSelector
  , subscribeAttributeGeneratedCommandListWithParams_subscriptionEstablished_reportHandlerSelector
  , subscribeAttributeMaxCookTimeWithParams_subscriptionEstablished_reportHandlerSelector
  , subscribeAttributeMaxPowerWithParams_subscriptionEstablished_reportHandlerSelector
  , subscribeAttributeMinPowerWithParams_subscriptionEstablished_reportHandlerSelector
  , subscribeAttributePowerSettingWithParams_subscriptionEstablished_reportHandlerSelector
  , subscribeAttributePowerStepWithParams_subscriptionEstablished_reportHandlerSelector
  , subscribeAttributeSelectedWattIndexWithParams_subscriptionEstablished_reportHandlerSelector
  , subscribeAttributeSupportedWattsWithParams_subscriptionEstablished_reportHandlerSelector
  , subscribeAttributeWattRatingWithParams_subscriptionEstablished_reportHandlerSelector


  ) where

import Foreign.Ptr (Ptr, FunPtr)
import Foreign.C.Types

import ObjC.Runtime.Types
import ObjC.Runtime.Message (sendMessage, sendOwnedMessage, sendClassMessage, sendOwnedClassMessage)
import ObjC.Runtime.Selector (mkSelector)
import ObjC.Runtime.Class (getRequiredClass)

import ObjC.Matter.Internal.Classes
import ObjC.Foundation.Internal.Classes

-- | Command SetCookingParameters
--
-- This command is used to set the cooking parameters associated with the operation of the device.
--
-- ObjC selector: @- setCookingParametersWithParams:completion:@
setCookingParametersWithParams_completion :: (IsMTRBaseClusterMicrowaveOvenControl mtrBaseClusterMicrowaveOvenControl, IsMTRMicrowaveOvenControlClusterSetCookingParametersParams params) => mtrBaseClusterMicrowaveOvenControl -> params -> Ptr () -> IO ()
setCookingParametersWithParams_completion mtrBaseClusterMicrowaveOvenControl params completion =
  sendMessage mtrBaseClusterMicrowaveOvenControl setCookingParametersWithParams_completionSelector (toMTRMicrowaveOvenControlClusterSetCookingParametersParams params) completion

-- | @- setCookingParametersWithCompletion:@
setCookingParametersWithCompletion :: IsMTRBaseClusterMicrowaveOvenControl mtrBaseClusterMicrowaveOvenControl => mtrBaseClusterMicrowaveOvenControl -> Ptr () -> IO ()
setCookingParametersWithCompletion mtrBaseClusterMicrowaveOvenControl completion =
  sendMessage mtrBaseClusterMicrowaveOvenControl setCookingParametersWithCompletionSelector completion

-- | Command AddMoreTime
--
-- This command is used to add more time to the CookTime attribute of the server.
--
-- ObjC selector: @- addMoreTimeWithParams:completion:@
addMoreTimeWithParams_completion :: (IsMTRBaseClusterMicrowaveOvenControl mtrBaseClusterMicrowaveOvenControl, IsMTRMicrowaveOvenControlClusterAddMoreTimeParams params) => mtrBaseClusterMicrowaveOvenControl -> params -> Ptr () -> IO ()
addMoreTimeWithParams_completion mtrBaseClusterMicrowaveOvenControl params completion =
  sendMessage mtrBaseClusterMicrowaveOvenControl addMoreTimeWithParams_completionSelector (toMTRMicrowaveOvenControlClusterAddMoreTimeParams params) completion

-- | @- readAttributeCookTimeWithCompletion:@
readAttributeCookTimeWithCompletion :: IsMTRBaseClusterMicrowaveOvenControl mtrBaseClusterMicrowaveOvenControl => mtrBaseClusterMicrowaveOvenControl -> Ptr () -> IO ()
readAttributeCookTimeWithCompletion mtrBaseClusterMicrowaveOvenControl completion =
  sendMessage mtrBaseClusterMicrowaveOvenControl readAttributeCookTimeWithCompletionSelector completion

-- | @- subscribeAttributeCookTimeWithParams:subscriptionEstablished:reportHandler:@
subscribeAttributeCookTimeWithParams_subscriptionEstablished_reportHandler :: (IsMTRBaseClusterMicrowaveOvenControl mtrBaseClusterMicrowaveOvenControl, IsMTRSubscribeParams params) => mtrBaseClusterMicrowaveOvenControl -> params -> Ptr () -> Ptr () -> IO ()
subscribeAttributeCookTimeWithParams_subscriptionEstablished_reportHandler mtrBaseClusterMicrowaveOvenControl params subscriptionEstablished reportHandler =
  sendMessage mtrBaseClusterMicrowaveOvenControl subscribeAttributeCookTimeWithParams_subscriptionEstablished_reportHandlerSelector (toMTRSubscribeParams params) subscriptionEstablished reportHandler

-- | @+ readAttributeCookTimeWithClusterStateCache:endpoint:queue:completion:@
readAttributeCookTimeWithClusterStateCache_endpoint_queue_completion :: (IsMTRClusterStateCacheContainer clusterStateCacheContainer, IsNSNumber endpoint, IsNSObject queue) => clusterStateCacheContainer -> endpoint -> queue -> Ptr () -> IO ()
readAttributeCookTimeWithClusterStateCache_endpoint_queue_completion clusterStateCacheContainer endpoint queue completion =
  do
    cls' <- getRequiredClass "MTRBaseClusterMicrowaveOvenControl"
    sendClassMessage cls' readAttributeCookTimeWithClusterStateCache_endpoint_queue_completionSelector (toMTRClusterStateCacheContainer clusterStateCacheContainer) (toNSNumber endpoint) (toNSObject queue) completion

-- | @- readAttributeMaxCookTimeWithCompletion:@
readAttributeMaxCookTimeWithCompletion :: IsMTRBaseClusterMicrowaveOvenControl mtrBaseClusterMicrowaveOvenControl => mtrBaseClusterMicrowaveOvenControl -> Ptr () -> IO ()
readAttributeMaxCookTimeWithCompletion mtrBaseClusterMicrowaveOvenControl completion =
  sendMessage mtrBaseClusterMicrowaveOvenControl readAttributeMaxCookTimeWithCompletionSelector completion

-- | @- subscribeAttributeMaxCookTimeWithParams:subscriptionEstablished:reportHandler:@
subscribeAttributeMaxCookTimeWithParams_subscriptionEstablished_reportHandler :: (IsMTRBaseClusterMicrowaveOvenControl mtrBaseClusterMicrowaveOvenControl, IsMTRSubscribeParams params) => mtrBaseClusterMicrowaveOvenControl -> params -> Ptr () -> Ptr () -> IO ()
subscribeAttributeMaxCookTimeWithParams_subscriptionEstablished_reportHandler mtrBaseClusterMicrowaveOvenControl params subscriptionEstablished reportHandler =
  sendMessage mtrBaseClusterMicrowaveOvenControl subscribeAttributeMaxCookTimeWithParams_subscriptionEstablished_reportHandlerSelector (toMTRSubscribeParams params) subscriptionEstablished reportHandler

-- | @+ readAttributeMaxCookTimeWithClusterStateCache:endpoint:queue:completion:@
readAttributeMaxCookTimeWithClusterStateCache_endpoint_queue_completion :: (IsMTRClusterStateCacheContainer clusterStateCacheContainer, IsNSNumber endpoint, IsNSObject queue) => clusterStateCacheContainer -> endpoint -> queue -> Ptr () -> IO ()
readAttributeMaxCookTimeWithClusterStateCache_endpoint_queue_completion clusterStateCacheContainer endpoint queue completion =
  do
    cls' <- getRequiredClass "MTRBaseClusterMicrowaveOvenControl"
    sendClassMessage cls' readAttributeMaxCookTimeWithClusterStateCache_endpoint_queue_completionSelector (toMTRClusterStateCacheContainer clusterStateCacheContainer) (toNSNumber endpoint) (toNSObject queue) completion

-- | @- readAttributePowerSettingWithCompletion:@
readAttributePowerSettingWithCompletion :: IsMTRBaseClusterMicrowaveOvenControl mtrBaseClusterMicrowaveOvenControl => mtrBaseClusterMicrowaveOvenControl -> Ptr () -> IO ()
readAttributePowerSettingWithCompletion mtrBaseClusterMicrowaveOvenControl completion =
  sendMessage mtrBaseClusterMicrowaveOvenControl readAttributePowerSettingWithCompletionSelector completion

-- | @- subscribeAttributePowerSettingWithParams:subscriptionEstablished:reportHandler:@
subscribeAttributePowerSettingWithParams_subscriptionEstablished_reportHandler :: (IsMTRBaseClusterMicrowaveOvenControl mtrBaseClusterMicrowaveOvenControl, IsMTRSubscribeParams params) => mtrBaseClusterMicrowaveOvenControl -> params -> Ptr () -> Ptr () -> IO ()
subscribeAttributePowerSettingWithParams_subscriptionEstablished_reportHandler mtrBaseClusterMicrowaveOvenControl params subscriptionEstablished reportHandler =
  sendMessage mtrBaseClusterMicrowaveOvenControl subscribeAttributePowerSettingWithParams_subscriptionEstablished_reportHandlerSelector (toMTRSubscribeParams params) subscriptionEstablished reportHandler

-- | @+ readAttributePowerSettingWithClusterStateCache:endpoint:queue:completion:@
readAttributePowerSettingWithClusterStateCache_endpoint_queue_completion :: (IsMTRClusterStateCacheContainer clusterStateCacheContainer, IsNSNumber endpoint, IsNSObject queue) => clusterStateCacheContainer -> endpoint -> queue -> Ptr () -> IO ()
readAttributePowerSettingWithClusterStateCache_endpoint_queue_completion clusterStateCacheContainer endpoint queue completion =
  do
    cls' <- getRequiredClass "MTRBaseClusterMicrowaveOvenControl"
    sendClassMessage cls' readAttributePowerSettingWithClusterStateCache_endpoint_queue_completionSelector (toMTRClusterStateCacheContainer clusterStateCacheContainer) (toNSNumber endpoint) (toNSObject queue) completion

-- | @- readAttributeMinPowerWithCompletion:@
readAttributeMinPowerWithCompletion :: IsMTRBaseClusterMicrowaveOvenControl mtrBaseClusterMicrowaveOvenControl => mtrBaseClusterMicrowaveOvenControl -> Ptr () -> IO ()
readAttributeMinPowerWithCompletion mtrBaseClusterMicrowaveOvenControl completion =
  sendMessage mtrBaseClusterMicrowaveOvenControl readAttributeMinPowerWithCompletionSelector completion

-- | @- subscribeAttributeMinPowerWithParams:subscriptionEstablished:reportHandler:@
subscribeAttributeMinPowerWithParams_subscriptionEstablished_reportHandler :: (IsMTRBaseClusterMicrowaveOvenControl mtrBaseClusterMicrowaveOvenControl, IsMTRSubscribeParams params) => mtrBaseClusterMicrowaveOvenControl -> params -> Ptr () -> Ptr () -> IO ()
subscribeAttributeMinPowerWithParams_subscriptionEstablished_reportHandler mtrBaseClusterMicrowaveOvenControl params subscriptionEstablished reportHandler =
  sendMessage mtrBaseClusterMicrowaveOvenControl subscribeAttributeMinPowerWithParams_subscriptionEstablished_reportHandlerSelector (toMTRSubscribeParams params) subscriptionEstablished reportHandler

-- | @+ readAttributeMinPowerWithClusterStateCache:endpoint:queue:completion:@
readAttributeMinPowerWithClusterStateCache_endpoint_queue_completion :: (IsMTRClusterStateCacheContainer clusterStateCacheContainer, IsNSNumber endpoint, IsNSObject queue) => clusterStateCacheContainer -> endpoint -> queue -> Ptr () -> IO ()
readAttributeMinPowerWithClusterStateCache_endpoint_queue_completion clusterStateCacheContainer endpoint queue completion =
  do
    cls' <- getRequiredClass "MTRBaseClusterMicrowaveOvenControl"
    sendClassMessage cls' readAttributeMinPowerWithClusterStateCache_endpoint_queue_completionSelector (toMTRClusterStateCacheContainer clusterStateCacheContainer) (toNSNumber endpoint) (toNSObject queue) completion

-- | @- readAttributeMaxPowerWithCompletion:@
readAttributeMaxPowerWithCompletion :: IsMTRBaseClusterMicrowaveOvenControl mtrBaseClusterMicrowaveOvenControl => mtrBaseClusterMicrowaveOvenControl -> Ptr () -> IO ()
readAttributeMaxPowerWithCompletion mtrBaseClusterMicrowaveOvenControl completion =
  sendMessage mtrBaseClusterMicrowaveOvenControl readAttributeMaxPowerWithCompletionSelector completion

-- | @- subscribeAttributeMaxPowerWithParams:subscriptionEstablished:reportHandler:@
subscribeAttributeMaxPowerWithParams_subscriptionEstablished_reportHandler :: (IsMTRBaseClusterMicrowaveOvenControl mtrBaseClusterMicrowaveOvenControl, IsMTRSubscribeParams params) => mtrBaseClusterMicrowaveOvenControl -> params -> Ptr () -> Ptr () -> IO ()
subscribeAttributeMaxPowerWithParams_subscriptionEstablished_reportHandler mtrBaseClusterMicrowaveOvenControl params subscriptionEstablished reportHandler =
  sendMessage mtrBaseClusterMicrowaveOvenControl subscribeAttributeMaxPowerWithParams_subscriptionEstablished_reportHandlerSelector (toMTRSubscribeParams params) subscriptionEstablished reportHandler

-- | @+ readAttributeMaxPowerWithClusterStateCache:endpoint:queue:completion:@
readAttributeMaxPowerWithClusterStateCache_endpoint_queue_completion :: (IsMTRClusterStateCacheContainer clusterStateCacheContainer, IsNSNumber endpoint, IsNSObject queue) => clusterStateCacheContainer -> endpoint -> queue -> Ptr () -> IO ()
readAttributeMaxPowerWithClusterStateCache_endpoint_queue_completion clusterStateCacheContainer endpoint queue completion =
  do
    cls' <- getRequiredClass "MTRBaseClusterMicrowaveOvenControl"
    sendClassMessage cls' readAttributeMaxPowerWithClusterStateCache_endpoint_queue_completionSelector (toMTRClusterStateCacheContainer clusterStateCacheContainer) (toNSNumber endpoint) (toNSObject queue) completion

-- | @- readAttributePowerStepWithCompletion:@
readAttributePowerStepWithCompletion :: IsMTRBaseClusterMicrowaveOvenControl mtrBaseClusterMicrowaveOvenControl => mtrBaseClusterMicrowaveOvenControl -> Ptr () -> IO ()
readAttributePowerStepWithCompletion mtrBaseClusterMicrowaveOvenControl completion =
  sendMessage mtrBaseClusterMicrowaveOvenControl readAttributePowerStepWithCompletionSelector completion

-- | @- subscribeAttributePowerStepWithParams:subscriptionEstablished:reportHandler:@
subscribeAttributePowerStepWithParams_subscriptionEstablished_reportHandler :: (IsMTRBaseClusterMicrowaveOvenControl mtrBaseClusterMicrowaveOvenControl, IsMTRSubscribeParams params) => mtrBaseClusterMicrowaveOvenControl -> params -> Ptr () -> Ptr () -> IO ()
subscribeAttributePowerStepWithParams_subscriptionEstablished_reportHandler mtrBaseClusterMicrowaveOvenControl params subscriptionEstablished reportHandler =
  sendMessage mtrBaseClusterMicrowaveOvenControl subscribeAttributePowerStepWithParams_subscriptionEstablished_reportHandlerSelector (toMTRSubscribeParams params) subscriptionEstablished reportHandler

-- | @+ readAttributePowerStepWithClusterStateCache:endpoint:queue:completion:@
readAttributePowerStepWithClusterStateCache_endpoint_queue_completion :: (IsMTRClusterStateCacheContainer clusterStateCacheContainer, IsNSNumber endpoint, IsNSObject queue) => clusterStateCacheContainer -> endpoint -> queue -> Ptr () -> IO ()
readAttributePowerStepWithClusterStateCache_endpoint_queue_completion clusterStateCacheContainer endpoint queue completion =
  do
    cls' <- getRequiredClass "MTRBaseClusterMicrowaveOvenControl"
    sendClassMessage cls' readAttributePowerStepWithClusterStateCache_endpoint_queue_completionSelector (toMTRClusterStateCacheContainer clusterStateCacheContainer) (toNSNumber endpoint) (toNSObject queue) completion

-- | @- readAttributeSupportedWattsWithCompletion:@
readAttributeSupportedWattsWithCompletion :: IsMTRBaseClusterMicrowaveOvenControl mtrBaseClusterMicrowaveOvenControl => mtrBaseClusterMicrowaveOvenControl -> Ptr () -> IO ()
readAttributeSupportedWattsWithCompletion mtrBaseClusterMicrowaveOvenControl completion =
  sendMessage mtrBaseClusterMicrowaveOvenControl readAttributeSupportedWattsWithCompletionSelector completion

-- | @- subscribeAttributeSupportedWattsWithParams:subscriptionEstablished:reportHandler:@
subscribeAttributeSupportedWattsWithParams_subscriptionEstablished_reportHandler :: (IsMTRBaseClusterMicrowaveOvenControl mtrBaseClusterMicrowaveOvenControl, IsMTRSubscribeParams params) => mtrBaseClusterMicrowaveOvenControl -> params -> Ptr () -> Ptr () -> IO ()
subscribeAttributeSupportedWattsWithParams_subscriptionEstablished_reportHandler mtrBaseClusterMicrowaveOvenControl params subscriptionEstablished reportHandler =
  sendMessage mtrBaseClusterMicrowaveOvenControl subscribeAttributeSupportedWattsWithParams_subscriptionEstablished_reportHandlerSelector (toMTRSubscribeParams params) subscriptionEstablished reportHandler

-- | @+ readAttributeSupportedWattsWithClusterStateCache:endpoint:queue:completion:@
readAttributeSupportedWattsWithClusterStateCache_endpoint_queue_completion :: (IsMTRClusterStateCacheContainer clusterStateCacheContainer, IsNSNumber endpoint, IsNSObject queue) => clusterStateCacheContainer -> endpoint -> queue -> Ptr () -> IO ()
readAttributeSupportedWattsWithClusterStateCache_endpoint_queue_completion clusterStateCacheContainer endpoint queue completion =
  do
    cls' <- getRequiredClass "MTRBaseClusterMicrowaveOvenControl"
    sendClassMessage cls' readAttributeSupportedWattsWithClusterStateCache_endpoint_queue_completionSelector (toMTRClusterStateCacheContainer clusterStateCacheContainer) (toNSNumber endpoint) (toNSObject queue) completion

-- | @- readAttributeSelectedWattIndexWithCompletion:@
readAttributeSelectedWattIndexWithCompletion :: IsMTRBaseClusterMicrowaveOvenControl mtrBaseClusterMicrowaveOvenControl => mtrBaseClusterMicrowaveOvenControl -> Ptr () -> IO ()
readAttributeSelectedWattIndexWithCompletion mtrBaseClusterMicrowaveOvenControl completion =
  sendMessage mtrBaseClusterMicrowaveOvenControl readAttributeSelectedWattIndexWithCompletionSelector completion

-- | @- subscribeAttributeSelectedWattIndexWithParams:subscriptionEstablished:reportHandler:@
subscribeAttributeSelectedWattIndexWithParams_subscriptionEstablished_reportHandler :: (IsMTRBaseClusterMicrowaveOvenControl mtrBaseClusterMicrowaveOvenControl, IsMTRSubscribeParams params) => mtrBaseClusterMicrowaveOvenControl -> params -> Ptr () -> Ptr () -> IO ()
subscribeAttributeSelectedWattIndexWithParams_subscriptionEstablished_reportHandler mtrBaseClusterMicrowaveOvenControl params subscriptionEstablished reportHandler =
  sendMessage mtrBaseClusterMicrowaveOvenControl subscribeAttributeSelectedWattIndexWithParams_subscriptionEstablished_reportHandlerSelector (toMTRSubscribeParams params) subscriptionEstablished reportHandler

-- | @+ readAttributeSelectedWattIndexWithClusterStateCache:endpoint:queue:completion:@
readAttributeSelectedWattIndexWithClusterStateCache_endpoint_queue_completion :: (IsMTRClusterStateCacheContainer clusterStateCacheContainer, IsNSNumber endpoint, IsNSObject queue) => clusterStateCacheContainer -> endpoint -> queue -> Ptr () -> IO ()
readAttributeSelectedWattIndexWithClusterStateCache_endpoint_queue_completion clusterStateCacheContainer endpoint queue completion =
  do
    cls' <- getRequiredClass "MTRBaseClusterMicrowaveOvenControl"
    sendClassMessage cls' readAttributeSelectedWattIndexWithClusterStateCache_endpoint_queue_completionSelector (toMTRClusterStateCacheContainer clusterStateCacheContainer) (toNSNumber endpoint) (toNSObject queue) completion

-- | @- readAttributeWattRatingWithCompletion:@
readAttributeWattRatingWithCompletion :: IsMTRBaseClusterMicrowaveOvenControl mtrBaseClusterMicrowaveOvenControl => mtrBaseClusterMicrowaveOvenControl -> Ptr () -> IO ()
readAttributeWattRatingWithCompletion mtrBaseClusterMicrowaveOvenControl completion =
  sendMessage mtrBaseClusterMicrowaveOvenControl readAttributeWattRatingWithCompletionSelector completion

-- | @- subscribeAttributeWattRatingWithParams:subscriptionEstablished:reportHandler:@
subscribeAttributeWattRatingWithParams_subscriptionEstablished_reportHandler :: (IsMTRBaseClusterMicrowaveOvenControl mtrBaseClusterMicrowaveOvenControl, IsMTRSubscribeParams params) => mtrBaseClusterMicrowaveOvenControl -> params -> Ptr () -> Ptr () -> IO ()
subscribeAttributeWattRatingWithParams_subscriptionEstablished_reportHandler mtrBaseClusterMicrowaveOvenControl params subscriptionEstablished reportHandler =
  sendMessage mtrBaseClusterMicrowaveOvenControl subscribeAttributeWattRatingWithParams_subscriptionEstablished_reportHandlerSelector (toMTRSubscribeParams params) subscriptionEstablished reportHandler

-- | @+ readAttributeWattRatingWithClusterStateCache:endpoint:queue:completion:@
readAttributeWattRatingWithClusterStateCache_endpoint_queue_completion :: (IsMTRClusterStateCacheContainer clusterStateCacheContainer, IsNSNumber endpoint, IsNSObject queue) => clusterStateCacheContainer -> endpoint -> queue -> Ptr () -> IO ()
readAttributeWattRatingWithClusterStateCache_endpoint_queue_completion clusterStateCacheContainer endpoint queue completion =
  do
    cls' <- getRequiredClass "MTRBaseClusterMicrowaveOvenControl"
    sendClassMessage cls' readAttributeWattRatingWithClusterStateCache_endpoint_queue_completionSelector (toMTRClusterStateCacheContainer clusterStateCacheContainer) (toNSNumber endpoint) (toNSObject queue) completion

-- | @- readAttributeGeneratedCommandListWithCompletion:@
readAttributeGeneratedCommandListWithCompletion :: IsMTRBaseClusterMicrowaveOvenControl mtrBaseClusterMicrowaveOvenControl => mtrBaseClusterMicrowaveOvenControl -> Ptr () -> IO ()
readAttributeGeneratedCommandListWithCompletion mtrBaseClusterMicrowaveOvenControl completion =
  sendMessage mtrBaseClusterMicrowaveOvenControl readAttributeGeneratedCommandListWithCompletionSelector completion

-- | @- subscribeAttributeGeneratedCommandListWithParams:subscriptionEstablished:reportHandler:@
subscribeAttributeGeneratedCommandListWithParams_subscriptionEstablished_reportHandler :: (IsMTRBaseClusterMicrowaveOvenControl mtrBaseClusterMicrowaveOvenControl, IsMTRSubscribeParams params) => mtrBaseClusterMicrowaveOvenControl -> params -> Ptr () -> Ptr () -> IO ()
subscribeAttributeGeneratedCommandListWithParams_subscriptionEstablished_reportHandler mtrBaseClusterMicrowaveOvenControl params subscriptionEstablished reportHandler =
  sendMessage mtrBaseClusterMicrowaveOvenControl subscribeAttributeGeneratedCommandListWithParams_subscriptionEstablished_reportHandlerSelector (toMTRSubscribeParams params) subscriptionEstablished reportHandler

-- | @+ readAttributeGeneratedCommandListWithClusterStateCache:endpoint:queue:completion:@
readAttributeGeneratedCommandListWithClusterStateCache_endpoint_queue_completion :: (IsMTRClusterStateCacheContainer clusterStateCacheContainer, IsNSNumber endpoint, IsNSObject queue) => clusterStateCacheContainer -> endpoint -> queue -> Ptr () -> IO ()
readAttributeGeneratedCommandListWithClusterStateCache_endpoint_queue_completion clusterStateCacheContainer endpoint queue completion =
  do
    cls' <- getRequiredClass "MTRBaseClusterMicrowaveOvenControl"
    sendClassMessage cls' readAttributeGeneratedCommandListWithClusterStateCache_endpoint_queue_completionSelector (toMTRClusterStateCacheContainer clusterStateCacheContainer) (toNSNumber endpoint) (toNSObject queue) completion

-- | @- readAttributeAcceptedCommandListWithCompletion:@
readAttributeAcceptedCommandListWithCompletion :: IsMTRBaseClusterMicrowaveOvenControl mtrBaseClusterMicrowaveOvenControl => mtrBaseClusterMicrowaveOvenControl -> Ptr () -> IO ()
readAttributeAcceptedCommandListWithCompletion mtrBaseClusterMicrowaveOvenControl completion =
  sendMessage mtrBaseClusterMicrowaveOvenControl readAttributeAcceptedCommandListWithCompletionSelector completion

-- | @- subscribeAttributeAcceptedCommandListWithParams:subscriptionEstablished:reportHandler:@
subscribeAttributeAcceptedCommandListWithParams_subscriptionEstablished_reportHandler :: (IsMTRBaseClusterMicrowaveOvenControl mtrBaseClusterMicrowaveOvenControl, IsMTRSubscribeParams params) => mtrBaseClusterMicrowaveOvenControl -> params -> Ptr () -> Ptr () -> IO ()
subscribeAttributeAcceptedCommandListWithParams_subscriptionEstablished_reportHandler mtrBaseClusterMicrowaveOvenControl params subscriptionEstablished reportHandler =
  sendMessage mtrBaseClusterMicrowaveOvenControl subscribeAttributeAcceptedCommandListWithParams_subscriptionEstablished_reportHandlerSelector (toMTRSubscribeParams params) subscriptionEstablished reportHandler

-- | @+ readAttributeAcceptedCommandListWithClusterStateCache:endpoint:queue:completion:@
readAttributeAcceptedCommandListWithClusterStateCache_endpoint_queue_completion :: (IsMTRClusterStateCacheContainer clusterStateCacheContainer, IsNSNumber endpoint, IsNSObject queue) => clusterStateCacheContainer -> endpoint -> queue -> Ptr () -> IO ()
readAttributeAcceptedCommandListWithClusterStateCache_endpoint_queue_completion clusterStateCacheContainer endpoint queue completion =
  do
    cls' <- getRequiredClass "MTRBaseClusterMicrowaveOvenControl"
    sendClassMessage cls' readAttributeAcceptedCommandListWithClusterStateCache_endpoint_queue_completionSelector (toMTRClusterStateCacheContainer clusterStateCacheContainer) (toNSNumber endpoint) (toNSObject queue) completion

-- | @- readAttributeAttributeListWithCompletion:@
readAttributeAttributeListWithCompletion :: IsMTRBaseClusterMicrowaveOvenControl mtrBaseClusterMicrowaveOvenControl => mtrBaseClusterMicrowaveOvenControl -> Ptr () -> IO ()
readAttributeAttributeListWithCompletion mtrBaseClusterMicrowaveOvenControl completion =
  sendMessage mtrBaseClusterMicrowaveOvenControl readAttributeAttributeListWithCompletionSelector completion

-- | @- subscribeAttributeAttributeListWithParams:subscriptionEstablished:reportHandler:@
subscribeAttributeAttributeListWithParams_subscriptionEstablished_reportHandler :: (IsMTRBaseClusterMicrowaveOvenControl mtrBaseClusterMicrowaveOvenControl, IsMTRSubscribeParams params) => mtrBaseClusterMicrowaveOvenControl -> params -> Ptr () -> Ptr () -> IO ()
subscribeAttributeAttributeListWithParams_subscriptionEstablished_reportHandler mtrBaseClusterMicrowaveOvenControl params subscriptionEstablished reportHandler =
  sendMessage mtrBaseClusterMicrowaveOvenControl subscribeAttributeAttributeListWithParams_subscriptionEstablished_reportHandlerSelector (toMTRSubscribeParams params) subscriptionEstablished reportHandler

-- | @+ readAttributeAttributeListWithClusterStateCache:endpoint:queue:completion:@
readAttributeAttributeListWithClusterStateCache_endpoint_queue_completion :: (IsMTRClusterStateCacheContainer clusterStateCacheContainer, IsNSNumber endpoint, IsNSObject queue) => clusterStateCacheContainer -> endpoint -> queue -> Ptr () -> IO ()
readAttributeAttributeListWithClusterStateCache_endpoint_queue_completion clusterStateCacheContainer endpoint queue completion =
  do
    cls' <- getRequiredClass "MTRBaseClusterMicrowaveOvenControl"
    sendClassMessage cls' readAttributeAttributeListWithClusterStateCache_endpoint_queue_completionSelector (toMTRClusterStateCacheContainer clusterStateCacheContainer) (toNSNumber endpoint) (toNSObject queue) completion

-- | @- readAttributeFeatureMapWithCompletion:@
readAttributeFeatureMapWithCompletion :: IsMTRBaseClusterMicrowaveOvenControl mtrBaseClusterMicrowaveOvenControl => mtrBaseClusterMicrowaveOvenControl -> Ptr () -> IO ()
readAttributeFeatureMapWithCompletion mtrBaseClusterMicrowaveOvenControl completion =
  sendMessage mtrBaseClusterMicrowaveOvenControl readAttributeFeatureMapWithCompletionSelector completion

-- | @- subscribeAttributeFeatureMapWithParams:subscriptionEstablished:reportHandler:@
subscribeAttributeFeatureMapWithParams_subscriptionEstablished_reportHandler :: (IsMTRBaseClusterMicrowaveOvenControl mtrBaseClusterMicrowaveOvenControl, IsMTRSubscribeParams params) => mtrBaseClusterMicrowaveOvenControl -> params -> Ptr () -> Ptr () -> IO ()
subscribeAttributeFeatureMapWithParams_subscriptionEstablished_reportHandler mtrBaseClusterMicrowaveOvenControl params subscriptionEstablished reportHandler =
  sendMessage mtrBaseClusterMicrowaveOvenControl subscribeAttributeFeatureMapWithParams_subscriptionEstablished_reportHandlerSelector (toMTRSubscribeParams params) subscriptionEstablished reportHandler

-- | @+ readAttributeFeatureMapWithClusterStateCache:endpoint:queue:completion:@
readAttributeFeatureMapWithClusterStateCache_endpoint_queue_completion :: (IsMTRClusterStateCacheContainer clusterStateCacheContainer, IsNSNumber endpoint, IsNSObject queue) => clusterStateCacheContainer -> endpoint -> queue -> Ptr () -> IO ()
readAttributeFeatureMapWithClusterStateCache_endpoint_queue_completion clusterStateCacheContainer endpoint queue completion =
  do
    cls' <- getRequiredClass "MTRBaseClusterMicrowaveOvenControl"
    sendClassMessage cls' readAttributeFeatureMapWithClusterStateCache_endpoint_queue_completionSelector (toMTRClusterStateCacheContainer clusterStateCacheContainer) (toNSNumber endpoint) (toNSObject queue) completion

-- | @- readAttributeClusterRevisionWithCompletion:@
readAttributeClusterRevisionWithCompletion :: IsMTRBaseClusterMicrowaveOvenControl mtrBaseClusterMicrowaveOvenControl => mtrBaseClusterMicrowaveOvenControl -> Ptr () -> IO ()
readAttributeClusterRevisionWithCompletion mtrBaseClusterMicrowaveOvenControl completion =
  sendMessage mtrBaseClusterMicrowaveOvenControl readAttributeClusterRevisionWithCompletionSelector completion

-- | @- subscribeAttributeClusterRevisionWithParams:subscriptionEstablished:reportHandler:@
subscribeAttributeClusterRevisionWithParams_subscriptionEstablished_reportHandler :: (IsMTRBaseClusterMicrowaveOvenControl mtrBaseClusterMicrowaveOvenControl, IsMTRSubscribeParams params) => mtrBaseClusterMicrowaveOvenControl -> params -> Ptr () -> Ptr () -> IO ()
subscribeAttributeClusterRevisionWithParams_subscriptionEstablished_reportHandler mtrBaseClusterMicrowaveOvenControl params subscriptionEstablished reportHandler =
  sendMessage mtrBaseClusterMicrowaveOvenControl subscribeAttributeClusterRevisionWithParams_subscriptionEstablished_reportHandlerSelector (toMTRSubscribeParams params) subscriptionEstablished reportHandler

-- | @+ readAttributeClusterRevisionWithClusterStateCache:endpoint:queue:completion:@
readAttributeClusterRevisionWithClusterStateCache_endpoint_queue_completion :: (IsMTRClusterStateCacheContainer clusterStateCacheContainer, IsNSNumber endpoint, IsNSObject queue) => clusterStateCacheContainer -> endpoint -> queue -> Ptr () -> IO ()
readAttributeClusterRevisionWithClusterStateCache_endpoint_queue_completion clusterStateCacheContainer endpoint queue completion =
  do
    cls' <- getRequiredClass "MTRBaseClusterMicrowaveOvenControl"
    sendClassMessage cls' readAttributeClusterRevisionWithClusterStateCache_endpoint_queue_completionSelector (toMTRClusterStateCacheContainer clusterStateCacheContainer) (toNSNumber endpoint) (toNSObject queue) completion

-- | @- init@
init_ :: IsMTRBaseClusterMicrowaveOvenControl mtrBaseClusterMicrowaveOvenControl => mtrBaseClusterMicrowaveOvenControl -> IO (Id MTRBaseClusterMicrowaveOvenControl)
init_ mtrBaseClusterMicrowaveOvenControl =
  sendOwnedMessage mtrBaseClusterMicrowaveOvenControl initSelector

-- | @+ new@
new :: IO (Id MTRBaseClusterMicrowaveOvenControl)
new  =
  do
    cls' <- getRequiredClass "MTRBaseClusterMicrowaveOvenControl"
    sendOwnedClassMessage cls' newSelector

-- | For all instance methods (reads, writes, commands) that take a completion, the completion will be called on the provided queue.
--
-- ObjC selector: @- initWithDevice:endpointID:queue:@
initWithDevice_endpointID_queue :: (IsMTRBaseClusterMicrowaveOvenControl mtrBaseClusterMicrowaveOvenControl, IsMTRBaseDevice device, IsNSNumber endpointID, IsNSObject queue) => mtrBaseClusterMicrowaveOvenControl -> device -> endpointID -> queue -> IO (Id MTRBaseClusterMicrowaveOvenControl)
initWithDevice_endpointID_queue mtrBaseClusterMicrowaveOvenControl device endpointID queue =
  sendOwnedMessage mtrBaseClusterMicrowaveOvenControl initWithDevice_endpointID_queueSelector (toMTRBaseDevice device) (toNSNumber endpointID) (toNSObject queue)

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @setCookingParametersWithParams:completion:@
setCookingParametersWithParams_completionSelector :: Selector '[Id MTRMicrowaveOvenControlClusterSetCookingParametersParams, Ptr ()] ()
setCookingParametersWithParams_completionSelector = mkSelector "setCookingParametersWithParams:completion:"

-- | @Selector@ for @setCookingParametersWithCompletion:@
setCookingParametersWithCompletionSelector :: Selector '[Ptr ()] ()
setCookingParametersWithCompletionSelector = mkSelector "setCookingParametersWithCompletion:"

-- | @Selector@ for @addMoreTimeWithParams:completion:@
addMoreTimeWithParams_completionSelector :: Selector '[Id MTRMicrowaveOvenControlClusterAddMoreTimeParams, Ptr ()] ()
addMoreTimeWithParams_completionSelector = mkSelector "addMoreTimeWithParams:completion:"

-- | @Selector@ for @readAttributeCookTimeWithCompletion:@
readAttributeCookTimeWithCompletionSelector :: Selector '[Ptr ()] ()
readAttributeCookTimeWithCompletionSelector = mkSelector "readAttributeCookTimeWithCompletion:"

-- | @Selector@ for @subscribeAttributeCookTimeWithParams:subscriptionEstablished:reportHandler:@
subscribeAttributeCookTimeWithParams_subscriptionEstablished_reportHandlerSelector :: Selector '[Id MTRSubscribeParams, Ptr (), Ptr ()] ()
subscribeAttributeCookTimeWithParams_subscriptionEstablished_reportHandlerSelector = mkSelector "subscribeAttributeCookTimeWithParams:subscriptionEstablished:reportHandler:"

-- | @Selector@ for @readAttributeCookTimeWithClusterStateCache:endpoint:queue:completion:@
readAttributeCookTimeWithClusterStateCache_endpoint_queue_completionSelector :: Selector '[Id MTRClusterStateCacheContainer, Id NSNumber, Id NSObject, Ptr ()] ()
readAttributeCookTimeWithClusterStateCache_endpoint_queue_completionSelector = mkSelector "readAttributeCookTimeWithClusterStateCache:endpoint:queue:completion:"

-- | @Selector@ for @readAttributeMaxCookTimeWithCompletion:@
readAttributeMaxCookTimeWithCompletionSelector :: Selector '[Ptr ()] ()
readAttributeMaxCookTimeWithCompletionSelector = mkSelector "readAttributeMaxCookTimeWithCompletion:"

-- | @Selector@ for @subscribeAttributeMaxCookTimeWithParams:subscriptionEstablished:reportHandler:@
subscribeAttributeMaxCookTimeWithParams_subscriptionEstablished_reportHandlerSelector :: Selector '[Id MTRSubscribeParams, Ptr (), Ptr ()] ()
subscribeAttributeMaxCookTimeWithParams_subscriptionEstablished_reportHandlerSelector = mkSelector "subscribeAttributeMaxCookTimeWithParams:subscriptionEstablished:reportHandler:"

-- | @Selector@ for @readAttributeMaxCookTimeWithClusterStateCache:endpoint:queue:completion:@
readAttributeMaxCookTimeWithClusterStateCache_endpoint_queue_completionSelector :: Selector '[Id MTRClusterStateCacheContainer, Id NSNumber, Id NSObject, Ptr ()] ()
readAttributeMaxCookTimeWithClusterStateCache_endpoint_queue_completionSelector = mkSelector "readAttributeMaxCookTimeWithClusterStateCache:endpoint:queue:completion:"

-- | @Selector@ for @readAttributePowerSettingWithCompletion:@
readAttributePowerSettingWithCompletionSelector :: Selector '[Ptr ()] ()
readAttributePowerSettingWithCompletionSelector = mkSelector "readAttributePowerSettingWithCompletion:"

-- | @Selector@ for @subscribeAttributePowerSettingWithParams:subscriptionEstablished:reportHandler:@
subscribeAttributePowerSettingWithParams_subscriptionEstablished_reportHandlerSelector :: Selector '[Id MTRSubscribeParams, Ptr (), Ptr ()] ()
subscribeAttributePowerSettingWithParams_subscriptionEstablished_reportHandlerSelector = mkSelector "subscribeAttributePowerSettingWithParams:subscriptionEstablished:reportHandler:"

-- | @Selector@ for @readAttributePowerSettingWithClusterStateCache:endpoint:queue:completion:@
readAttributePowerSettingWithClusterStateCache_endpoint_queue_completionSelector :: Selector '[Id MTRClusterStateCacheContainer, Id NSNumber, Id NSObject, Ptr ()] ()
readAttributePowerSettingWithClusterStateCache_endpoint_queue_completionSelector = mkSelector "readAttributePowerSettingWithClusterStateCache:endpoint:queue:completion:"

-- | @Selector@ for @readAttributeMinPowerWithCompletion:@
readAttributeMinPowerWithCompletionSelector :: Selector '[Ptr ()] ()
readAttributeMinPowerWithCompletionSelector = mkSelector "readAttributeMinPowerWithCompletion:"

-- | @Selector@ for @subscribeAttributeMinPowerWithParams:subscriptionEstablished:reportHandler:@
subscribeAttributeMinPowerWithParams_subscriptionEstablished_reportHandlerSelector :: Selector '[Id MTRSubscribeParams, Ptr (), Ptr ()] ()
subscribeAttributeMinPowerWithParams_subscriptionEstablished_reportHandlerSelector = mkSelector "subscribeAttributeMinPowerWithParams:subscriptionEstablished:reportHandler:"

-- | @Selector@ for @readAttributeMinPowerWithClusterStateCache:endpoint:queue:completion:@
readAttributeMinPowerWithClusterStateCache_endpoint_queue_completionSelector :: Selector '[Id MTRClusterStateCacheContainer, Id NSNumber, Id NSObject, Ptr ()] ()
readAttributeMinPowerWithClusterStateCache_endpoint_queue_completionSelector = mkSelector "readAttributeMinPowerWithClusterStateCache:endpoint:queue:completion:"

-- | @Selector@ for @readAttributeMaxPowerWithCompletion:@
readAttributeMaxPowerWithCompletionSelector :: Selector '[Ptr ()] ()
readAttributeMaxPowerWithCompletionSelector = mkSelector "readAttributeMaxPowerWithCompletion:"

-- | @Selector@ for @subscribeAttributeMaxPowerWithParams:subscriptionEstablished:reportHandler:@
subscribeAttributeMaxPowerWithParams_subscriptionEstablished_reportHandlerSelector :: Selector '[Id MTRSubscribeParams, Ptr (), Ptr ()] ()
subscribeAttributeMaxPowerWithParams_subscriptionEstablished_reportHandlerSelector = mkSelector "subscribeAttributeMaxPowerWithParams:subscriptionEstablished:reportHandler:"

-- | @Selector@ for @readAttributeMaxPowerWithClusterStateCache:endpoint:queue:completion:@
readAttributeMaxPowerWithClusterStateCache_endpoint_queue_completionSelector :: Selector '[Id MTRClusterStateCacheContainer, Id NSNumber, Id NSObject, Ptr ()] ()
readAttributeMaxPowerWithClusterStateCache_endpoint_queue_completionSelector = mkSelector "readAttributeMaxPowerWithClusterStateCache:endpoint:queue:completion:"

-- | @Selector@ for @readAttributePowerStepWithCompletion:@
readAttributePowerStepWithCompletionSelector :: Selector '[Ptr ()] ()
readAttributePowerStepWithCompletionSelector = mkSelector "readAttributePowerStepWithCompletion:"

-- | @Selector@ for @subscribeAttributePowerStepWithParams:subscriptionEstablished:reportHandler:@
subscribeAttributePowerStepWithParams_subscriptionEstablished_reportHandlerSelector :: Selector '[Id MTRSubscribeParams, Ptr (), Ptr ()] ()
subscribeAttributePowerStepWithParams_subscriptionEstablished_reportHandlerSelector = mkSelector "subscribeAttributePowerStepWithParams:subscriptionEstablished:reportHandler:"

-- | @Selector@ for @readAttributePowerStepWithClusterStateCache:endpoint:queue:completion:@
readAttributePowerStepWithClusterStateCache_endpoint_queue_completionSelector :: Selector '[Id MTRClusterStateCacheContainer, Id NSNumber, Id NSObject, Ptr ()] ()
readAttributePowerStepWithClusterStateCache_endpoint_queue_completionSelector = mkSelector "readAttributePowerStepWithClusterStateCache:endpoint:queue:completion:"

-- | @Selector@ for @readAttributeSupportedWattsWithCompletion:@
readAttributeSupportedWattsWithCompletionSelector :: Selector '[Ptr ()] ()
readAttributeSupportedWattsWithCompletionSelector = mkSelector "readAttributeSupportedWattsWithCompletion:"

-- | @Selector@ for @subscribeAttributeSupportedWattsWithParams:subscriptionEstablished:reportHandler:@
subscribeAttributeSupportedWattsWithParams_subscriptionEstablished_reportHandlerSelector :: Selector '[Id MTRSubscribeParams, Ptr (), Ptr ()] ()
subscribeAttributeSupportedWattsWithParams_subscriptionEstablished_reportHandlerSelector = mkSelector "subscribeAttributeSupportedWattsWithParams:subscriptionEstablished:reportHandler:"

-- | @Selector@ for @readAttributeSupportedWattsWithClusterStateCache:endpoint:queue:completion:@
readAttributeSupportedWattsWithClusterStateCache_endpoint_queue_completionSelector :: Selector '[Id MTRClusterStateCacheContainer, Id NSNumber, Id NSObject, Ptr ()] ()
readAttributeSupportedWattsWithClusterStateCache_endpoint_queue_completionSelector = mkSelector "readAttributeSupportedWattsWithClusterStateCache:endpoint:queue:completion:"

-- | @Selector@ for @readAttributeSelectedWattIndexWithCompletion:@
readAttributeSelectedWattIndexWithCompletionSelector :: Selector '[Ptr ()] ()
readAttributeSelectedWattIndexWithCompletionSelector = mkSelector "readAttributeSelectedWattIndexWithCompletion:"

-- | @Selector@ for @subscribeAttributeSelectedWattIndexWithParams:subscriptionEstablished:reportHandler:@
subscribeAttributeSelectedWattIndexWithParams_subscriptionEstablished_reportHandlerSelector :: Selector '[Id MTRSubscribeParams, Ptr (), Ptr ()] ()
subscribeAttributeSelectedWattIndexWithParams_subscriptionEstablished_reportHandlerSelector = mkSelector "subscribeAttributeSelectedWattIndexWithParams:subscriptionEstablished:reportHandler:"

-- | @Selector@ for @readAttributeSelectedWattIndexWithClusterStateCache:endpoint:queue:completion:@
readAttributeSelectedWattIndexWithClusterStateCache_endpoint_queue_completionSelector :: Selector '[Id MTRClusterStateCacheContainer, Id NSNumber, Id NSObject, Ptr ()] ()
readAttributeSelectedWattIndexWithClusterStateCache_endpoint_queue_completionSelector = mkSelector "readAttributeSelectedWattIndexWithClusterStateCache:endpoint:queue:completion:"

-- | @Selector@ for @readAttributeWattRatingWithCompletion:@
readAttributeWattRatingWithCompletionSelector :: Selector '[Ptr ()] ()
readAttributeWattRatingWithCompletionSelector = mkSelector "readAttributeWattRatingWithCompletion:"

-- | @Selector@ for @subscribeAttributeWattRatingWithParams:subscriptionEstablished:reportHandler:@
subscribeAttributeWattRatingWithParams_subscriptionEstablished_reportHandlerSelector :: Selector '[Id MTRSubscribeParams, Ptr (), Ptr ()] ()
subscribeAttributeWattRatingWithParams_subscriptionEstablished_reportHandlerSelector = mkSelector "subscribeAttributeWattRatingWithParams:subscriptionEstablished:reportHandler:"

-- | @Selector@ for @readAttributeWattRatingWithClusterStateCache:endpoint:queue:completion:@
readAttributeWattRatingWithClusterStateCache_endpoint_queue_completionSelector :: Selector '[Id MTRClusterStateCacheContainer, Id NSNumber, Id NSObject, Ptr ()] ()
readAttributeWattRatingWithClusterStateCache_endpoint_queue_completionSelector = mkSelector "readAttributeWattRatingWithClusterStateCache:endpoint:queue:completion:"

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
initSelector :: Selector '[] (Id MTRBaseClusterMicrowaveOvenControl)
initSelector = mkSelector "init"

-- | @Selector@ for @new@
newSelector :: Selector '[] (Id MTRBaseClusterMicrowaveOvenControl)
newSelector = mkSelector "new"

-- | @Selector@ for @initWithDevice:endpointID:queue:@
initWithDevice_endpointID_queueSelector :: Selector '[Id MTRBaseDevice, Id NSNumber, Id NSObject] (Id MTRBaseClusterMicrowaveOvenControl)
initWithDevice_endpointID_queueSelector = mkSelector "initWithDevice:endpointID:queue:"

