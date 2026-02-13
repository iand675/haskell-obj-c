{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Cluster Closure Dimension
--
-- This cluster provides an interface to reflect and control a closure's range of movement, usually involving a panel, by using 6-axis framework.
--
-- Generated bindings for @MTRBaseClusterClosureDimension@.
module ObjC.Matter.MTRBaseClusterClosureDimension
  ( MTRBaseClusterClosureDimension
  , IsMTRBaseClusterClosureDimension(..)
  , setTargetWithParams_completion
  , setTargetWithCompletion
  , stepWithParams_completion
  , readAttributeCurrentStateWithCompletion
  , subscribeAttributeCurrentStateWithParams_subscriptionEstablished_reportHandler
  , readAttributeCurrentStateWithClusterStateCache_endpoint_queue_completion
  , readAttributeTargetStateWithCompletion
  , subscribeAttributeTargetStateWithParams_subscriptionEstablished_reportHandler
  , readAttributeTargetStateWithClusterStateCache_endpoint_queue_completion
  , readAttributeResolutionWithCompletion
  , subscribeAttributeResolutionWithParams_subscriptionEstablished_reportHandler
  , readAttributeResolutionWithClusterStateCache_endpoint_queue_completion
  , readAttributeStepValueWithCompletion
  , subscribeAttributeStepValueWithParams_subscriptionEstablished_reportHandler
  , readAttributeStepValueWithClusterStateCache_endpoint_queue_completion
  , readAttributeUnitWithCompletion
  , subscribeAttributeUnitWithParams_subscriptionEstablished_reportHandler
  , readAttributeUnitWithClusterStateCache_endpoint_queue_completion
  , readAttributeUnitRangeWithCompletion
  , subscribeAttributeUnitRangeWithParams_subscriptionEstablished_reportHandler
  , readAttributeUnitRangeWithClusterStateCache_endpoint_queue_completion
  , readAttributeLimitRangeWithCompletion
  , subscribeAttributeLimitRangeWithParams_subscriptionEstablished_reportHandler
  , readAttributeLimitRangeWithClusterStateCache_endpoint_queue_completion
  , readAttributeTranslationDirectionWithCompletion
  , subscribeAttributeTranslationDirectionWithParams_subscriptionEstablished_reportHandler
  , readAttributeTranslationDirectionWithClusterStateCache_endpoint_queue_completion
  , readAttributeRotationAxisWithCompletion
  , subscribeAttributeRotationAxisWithParams_subscriptionEstablished_reportHandler
  , readAttributeRotationAxisWithClusterStateCache_endpoint_queue_completion
  , readAttributeOverflowWithCompletion
  , subscribeAttributeOverflowWithParams_subscriptionEstablished_reportHandler
  , readAttributeOverflowWithClusterStateCache_endpoint_queue_completion
  , readAttributeModulationTypeWithCompletion
  , subscribeAttributeModulationTypeWithParams_subscriptionEstablished_reportHandler
  , readAttributeModulationTypeWithClusterStateCache_endpoint_queue_completion
  , readAttributeLatchControlModesWithCompletion
  , subscribeAttributeLatchControlModesWithParams_subscriptionEstablished_reportHandler
  , readAttributeLatchControlModesWithClusterStateCache_endpoint_queue_completion
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
  , readAttributeCurrentStateWithClusterStateCache_endpoint_queue_completionSelector
  , readAttributeCurrentStateWithCompletionSelector
  , readAttributeFeatureMapWithClusterStateCache_endpoint_queue_completionSelector
  , readAttributeFeatureMapWithCompletionSelector
  , readAttributeGeneratedCommandListWithClusterStateCache_endpoint_queue_completionSelector
  , readAttributeGeneratedCommandListWithCompletionSelector
  , readAttributeLatchControlModesWithClusterStateCache_endpoint_queue_completionSelector
  , readAttributeLatchControlModesWithCompletionSelector
  , readAttributeLimitRangeWithClusterStateCache_endpoint_queue_completionSelector
  , readAttributeLimitRangeWithCompletionSelector
  , readAttributeModulationTypeWithClusterStateCache_endpoint_queue_completionSelector
  , readAttributeModulationTypeWithCompletionSelector
  , readAttributeOverflowWithClusterStateCache_endpoint_queue_completionSelector
  , readAttributeOverflowWithCompletionSelector
  , readAttributeResolutionWithClusterStateCache_endpoint_queue_completionSelector
  , readAttributeResolutionWithCompletionSelector
  , readAttributeRotationAxisWithClusterStateCache_endpoint_queue_completionSelector
  , readAttributeRotationAxisWithCompletionSelector
  , readAttributeStepValueWithClusterStateCache_endpoint_queue_completionSelector
  , readAttributeStepValueWithCompletionSelector
  , readAttributeTargetStateWithClusterStateCache_endpoint_queue_completionSelector
  , readAttributeTargetStateWithCompletionSelector
  , readAttributeTranslationDirectionWithClusterStateCache_endpoint_queue_completionSelector
  , readAttributeTranslationDirectionWithCompletionSelector
  , readAttributeUnitRangeWithClusterStateCache_endpoint_queue_completionSelector
  , readAttributeUnitRangeWithCompletionSelector
  , readAttributeUnitWithClusterStateCache_endpoint_queue_completionSelector
  , readAttributeUnitWithCompletionSelector
  , setTargetWithCompletionSelector
  , setTargetWithParams_completionSelector
  , stepWithParams_completionSelector
  , subscribeAttributeAcceptedCommandListWithParams_subscriptionEstablished_reportHandlerSelector
  , subscribeAttributeAttributeListWithParams_subscriptionEstablished_reportHandlerSelector
  , subscribeAttributeClusterRevisionWithParams_subscriptionEstablished_reportHandlerSelector
  , subscribeAttributeCurrentStateWithParams_subscriptionEstablished_reportHandlerSelector
  , subscribeAttributeFeatureMapWithParams_subscriptionEstablished_reportHandlerSelector
  , subscribeAttributeGeneratedCommandListWithParams_subscriptionEstablished_reportHandlerSelector
  , subscribeAttributeLatchControlModesWithParams_subscriptionEstablished_reportHandlerSelector
  , subscribeAttributeLimitRangeWithParams_subscriptionEstablished_reportHandlerSelector
  , subscribeAttributeModulationTypeWithParams_subscriptionEstablished_reportHandlerSelector
  , subscribeAttributeOverflowWithParams_subscriptionEstablished_reportHandlerSelector
  , subscribeAttributeResolutionWithParams_subscriptionEstablished_reportHandlerSelector
  , subscribeAttributeRotationAxisWithParams_subscriptionEstablished_reportHandlerSelector
  , subscribeAttributeStepValueWithParams_subscriptionEstablished_reportHandlerSelector
  , subscribeAttributeTargetStateWithParams_subscriptionEstablished_reportHandlerSelector
  , subscribeAttributeTranslationDirectionWithParams_subscriptionEstablished_reportHandlerSelector
  , subscribeAttributeUnitRangeWithParams_subscriptionEstablished_reportHandlerSelector
  , subscribeAttributeUnitWithParams_subscriptionEstablished_reportHandlerSelector


  ) where

import Foreign.Ptr (Ptr, FunPtr)
import Foreign.C.Types

import ObjC.Runtime.Types
import ObjC.Runtime.Message (sendMessage, sendOwnedMessage, sendClassMessage, sendOwnedClassMessage)
import ObjC.Runtime.Selector (mkSelector)
import ObjC.Runtime.Class (getRequiredClass)

import ObjC.Matter.Internal.Classes
import ObjC.Foundation.Internal.Classes

-- | Command SetTarget
--
-- This command is used to move a dimension of the closure to a target position.
--
-- ObjC selector: @- setTargetWithParams:completion:@
setTargetWithParams_completion :: (IsMTRBaseClusterClosureDimension mtrBaseClusterClosureDimension, IsMTRClosureDimensionClusterSetTargetParams params) => mtrBaseClusterClosureDimension -> params -> Ptr () -> IO ()
setTargetWithParams_completion mtrBaseClusterClosureDimension params completion =
  sendMessage mtrBaseClusterClosureDimension setTargetWithParams_completionSelector (toMTRClosureDimensionClusterSetTargetParams params) completion

-- | @- setTargetWithCompletion:@
setTargetWithCompletion :: IsMTRBaseClusterClosureDimension mtrBaseClusterClosureDimension => mtrBaseClusterClosureDimension -> Ptr () -> IO ()
setTargetWithCompletion mtrBaseClusterClosureDimension completion =
  sendMessage mtrBaseClusterClosureDimension setTargetWithCompletionSelector completion

-- | Command Step
--
-- This command is used to move a dimension of the closure to a target position by a number of steps.
--
-- ObjC selector: @- stepWithParams:completion:@
stepWithParams_completion :: (IsMTRBaseClusterClosureDimension mtrBaseClusterClosureDimension, IsMTRClosureDimensionClusterStepParams params) => mtrBaseClusterClosureDimension -> params -> Ptr () -> IO ()
stepWithParams_completion mtrBaseClusterClosureDimension params completion =
  sendMessage mtrBaseClusterClosureDimension stepWithParams_completionSelector (toMTRClosureDimensionClusterStepParams params) completion

-- | @- readAttributeCurrentStateWithCompletion:@
readAttributeCurrentStateWithCompletion :: IsMTRBaseClusterClosureDimension mtrBaseClusterClosureDimension => mtrBaseClusterClosureDimension -> Ptr () -> IO ()
readAttributeCurrentStateWithCompletion mtrBaseClusterClosureDimension completion =
  sendMessage mtrBaseClusterClosureDimension readAttributeCurrentStateWithCompletionSelector completion

-- | @- subscribeAttributeCurrentStateWithParams:subscriptionEstablished:reportHandler:@
subscribeAttributeCurrentStateWithParams_subscriptionEstablished_reportHandler :: (IsMTRBaseClusterClosureDimension mtrBaseClusterClosureDimension, IsMTRSubscribeParams params) => mtrBaseClusterClosureDimension -> params -> Ptr () -> Ptr () -> IO ()
subscribeAttributeCurrentStateWithParams_subscriptionEstablished_reportHandler mtrBaseClusterClosureDimension params subscriptionEstablished reportHandler =
  sendMessage mtrBaseClusterClosureDimension subscribeAttributeCurrentStateWithParams_subscriptionEstablished_reportHandlerSelector (toMTRSubscribeParams params) subscriptionEstablished reportHandler

-- | @+ readAttributeCurrentStateWithClusterStateCache:endpoint:queue:completion:@
readAttributeCurrentStateWithClusterStateCache_endpoint_queue_completion :: (IsMTRClusterStateCacheContainer clusterStateCacheContainer, IsNSNumber endpoint, IsNSObject queue) => clusterStateCacheContainer -> endpoint -> queue -> Ptr () -> IO ()
readAttributeCurrentStateWithClusterStateCache_endpoint_queue_completion clusterStateCacheContainer endpoint queue completion =
  do
    cls' <- getRequiredClass "MTRBaseClusterClosureDimension"
    sendClassMessage cls' readAttributeCurrentStateWithClusterStateCache_endpoint_queue_completionSelector (toMTRClusterStateCacheContainer clusterStateCacheContainer) (toNSNumber endpoint) (toNSObject queue) completion

-- | @- readAttributeTargetStateWithCompletion:@
readAttributeTargetStateWithCompletion :: IsMTRBaseClusterClosureDimension mtrBaseClusterClosureDimension => mtrBaseClusterClosureDimension -> Ptr () -> IO ()
readAttributeTargetStateWithCompletion mtrBaseClusterClosureDimension completion =
  sendMessage mtrBaseClusterClosureDimension readAttributeTargetStateWithCompletionSelector completion

-- | @- subscribeAttributeTargetStateWithParams:subscriptionEstablished:reportHandler:@
subscribeAttributeTargetStateWithParams_subscriptionEstablished_reportHandler :: (IsMTRBaseClusterClosureDimension mtrBaseClusterClosureDimension, IsMTRSubscribeParams params) => mtrBaseClusterClosureDimension -> params -> Ptr () -> Ptr () -> IO ()
subscribeAttributeTargetStateWithParams_subscriptionEstablished_reportHandler mtrBaseClusterClosureDimension params subscriptionEstablished reportHandler =
  sendMessage mtrBaseClusterClosureDimension subscribeAttributeTargetStateWithParams_subscriptionEstablished_reportHandlerSelector (toMTRSubscribeParams params) subscriptionEstablished reportHandler

-- | @+ readAttributeTargetStateWithClusterStateCache:endpoint:queue:completion:@
readAttributeTargetStateWithClusterStateCache_endpoint_queue_completion :: (IsMTRClusterStateCacheContainer clusterStateCacheContainer, IsNSNumber endpoint, IsNSObject queue) => clusterStateCacheContainer -> endpoint -> queue -> Ptr () -> IO ()
readAttributeTargetStateWithClusterStateCache_endpoint_queue_completion clusterStateCacheContainer endpoint queue completion =
  do
    cls' <- getRequiredClass "MTRBaseClusterClosureDimension"
    sendClassMessage cls' readAttributeTargetStateWithClusterStateCache_endpoint_queue_completionSelector (toMTRClusterStateCacheContainer clusterStateCacheContainer) (toNSNumber endpoint) (toNSObject queue) completion

-- | @- readAttributeResolutionWithCompletion:@
readAttributeResolutionWithCompletion :: IsMTRBaseClusterClosureDimension mtrBaseClusterClosureDimension => mtrBaseClusterClosureDimension -> Ptr () -> IO ()
readAttributeResolutionWithCompletion mtrBaseClusterClosureDimension completion =
  sendMessage mtrBaseClusterClosureDimension readAttributeResolutionWithCompletionSelector completion

-- | @- subscribeAttributeResolutionWithParams:subscriptionEstablished:reportHandler:@
subscribeAttributeResolutionWithParams_subscriptionEstablished_reportHandler :: (IsMTRBaseClusterClosureDimension mtrBaseClusterClosureDimension, IsMTRSubscribeParams params) => mtrBaseClusterClosureDimension -> params -> Ptr () -> Ptr () -> IO ()
subscribeAttributeResolutionWithParams_subscriptionEstablished_reportHandler mtrBaseClusterClosureDimension params subscriptionEstablished reportHandler =
  sendMessage mtrBaseClusterClosureDimension subscribeAttributeResolutionWithParams_subscriptionEstablished_reportHandlerSelector (toMTRSubscribeParams params) subscriptionEstablished reportHandler

-- | @+ readAttributeResolutionWithClusterStateCache:endpoint:queue:completion:@
readAttributeResolutionWithClusterStateCache_endpoint_queue_completion :: (IsMTRClusterStateCacheContainer clusterStateCacheContainer, IsNSNumber endpoint, IsNSObject queue) => clusterStateCacheContainer -> endpoint -> queue -> Ptr () -> IO ()
readAttributeResolutionWithClusterStateCache_endpoint_queue_completion clusterStateCacheContainer endpoint queue completion =
  do
    cls' <- getRequiredClass "MTRBaseClusterClosureDimension"
    sendClassMessage cls' readAttributeResolutionWithClusterStateCache_endpoint_queue_completionSelector (toMTRClusterStateCacheContainer clusterStateCacheContainer) (toNSNumber endpoint) (toNSObject queue) completion

-- | @- readAttributeStepValueWithCompletion:@
readAttributeStepValueWithCompletion :: IsMTRBaseClusterClosureDimension mtrBaseClusterClosureDimension => mtrBaseClusterClosureDimension -> Ptr () -> IO ()
readAttributeStepValueWithCompletion mtrBaseClusterClosureDimension completion =
  sendMessage mtrBaseClusterClosureDimension readAttributeStepValueWithCompletionSelector completion

-- | @- subscribeAttributeStepValueWithParams:subscriptionEstablished:reportHandler:@
subscribeAttributeStepValueWithParams_subscriptionEstablished_reportHandler :: (IsMTRBaseClusterClosureDimension mtrBaseClusterClosureDimension, IsMTRSubscribeParams params) => mtrBaseClusterClosureDimension -> params -> Ptr () -> Ptr () -> IO ()
subscribeAttributeStepValueWithParams_subscriptionEstablished_reportHandler mtrBaseClusterClosureDimension params subscriptionEstablished reportHandler =
  sendMessage mtrBaseClusterClosureDimension subscribeAttributeStepValueWithParams_subscriptionEstablished_reportHandlerSelector (toMTRSubscribeParams params) subscriptionEstablished reportHandler

-- | @+ readAttributeStepValueWithClusterStateCache:endpoint:queue:completion:@
readAttributeStepValueWithClusterStateCache_endpoint_queue_completion :: (IsMTRClusterStateCacheContainer clusterStateCacheContainer, IsNSNumber endpoint, IsNSObject queue) => clusterStateCacheContainer -> endpoint -> queue -> Ptr () -> IO ()
readAttributeStepValueWithClusterStateCache_endpoint_queue_completion clusterStateCacheContainer endpoint queue completion =
  do
    cls' <- getRequiredClass "MTRBaseClusterClosureDimension"
    sendClassMessage cls' readAttributeStepValueWithClusterStateCache_endpoint_queue_completionSelector (toMTRClusterStateCacheContainer clusterStateCacheContainer) (toNSNumber endpoint) (toNSObject queue) completion

-- | @- readAttributeUnitWithCompletion:@
readAttributeUnitWithCompletion :: IsMTRBaseClusterClosureDimension mtrBaseClusterClosureDimension => mtrBaseClusterClosureDimension -> Ptr () -> IO ()
readAttributeUnitWithCompletion mtrBaseClusterClosureDimension completion =
  sendMessage mtrBaseClusterClosureDimension readAttributeUnitWithCompletionSelector completion

-- | @- subscribeAttributeUnitWithParams:subscriptionEstablished:reportHandler:@
subscribeAttributeUnitWithParams_subscriptionEstablished_reportHandler :: (IsMTRBaseClusterClosureDimension mtrBaseClusterClosureDimension, IsMTRSubscribeParams params) => mtrBaseClusterClosureDimension -> params -> Ptr () -> Ptr () -> IO ()
subscribeAttributeUnitWithParams_subscriptionEstablished_reportHandler mtrBaseClusterClosureDimension params subscriptionEstablished reportHandler =
  sendMessage mtrBaseClusterClosureDimension subscribeAttributeUnitWithParams_subscriptionEstablished_reportHandlerSelector (toMTRSubscribeParams params) subscriptionEstablished reportHandler

-- | @+ readAttributeUnitWithClusterStateCache:endpoint:queue:completion:@
readAttributeUnitWithClusterStateCache_endpoint_queue_completion :: (IsMTRClusterStateCacheContainer clusterStateCacheContainer, IsNSNumber endpoint, IsNSObject queue) => clusterStateCacheContainer -> endpoint -> queue -> Ptr () -> IO ()
readAttributeUnitWithClusterStateCache_endpoint_queue_completion clusterStateCacheContainer endpoint queue completion =
  do
    cls' <- getRequiredClass "MTRBaseClusterClosureDimension"
    sendClassMessage cls' readAttributeUnitWithClusterStateCache_endpoint_queue_completionSelector (toMTRClusterStateCacheContainer clusterStateCacheContainer) (toNSNumber endpoint) (toNSObject queue) completion

-- | @- readAttributeUnitRangeWithCompletion:@
readAttributeUnitRangeWithCompletion :: IsMTRBaseClusterClosureDimension mtrBaseClusterClosureDimension => mtrBaseClusterClosureDimension -> Ptr () -> IO ()
readAttributeUnitRangeWithCompletion mtrBaseClusterClosureDimension completion =
  sendMessage mtrBaseClusterClosureDimension readAttributeUnitRangeWithCompletionSelector completion

-- | @- subscribeAttributeUnitRangeWithParams:subscriptionEstablished:reportHandler:@
subscribeAttributeUnitRangeWithParams_subscriptionEstablished_reportHandler :: (IsMTRBaseClusterClosureDimension mtrBaseClusterClosureDimension, IsMTRSubscribeParams params) => mtrBaseClusterClosureDimension -> params -> Ptr () -> Ptr () -> IO ()
subscribeAttributeUnitRangeWithParams_subscriptionEstablished_reportHandler mtrBaseClusterClosureDimension params subscriptionEstablished reportHandler =
  sendMessage mtrBaseClusterClosureDimension subscribeAttributeUnitRangeWithParams_subscriptionEstablished_reportHandlerSelector (toMTRSubscribeParams params) subscriptionEstablished reportHandler

-- | @+ readAttributeUnitRangeWithClusterStateCache:endpoint:queue:completion:@
readAttributeUnitRangeWithClusterStateCache_endpoint_queue_completion :: (IsMTRClusterStateCacheContainer clusterStateCacheContainer, IsNSNumber endpoint, IsNSObject queue) => clusterStateCacheContainer -> endpoint -> queue -> Ptr () -> IO ()
readAttributeUnitRangeWithClusterStateCache_endpoint_queue_completion clusterStateCacheContainer endpoint queue completion =
  do
    cls' <- getRequiredClass "MTRBaseClusterClosureDimension"
    sendClassMessage cls' readAttributeUnitRangeWithClusterStateCache_endpoint_queue_completionSelector (toMTRClusterStateCacheContainer clusterStateCacheContainer) (toNSNumber endpoint) (toNSObject queue) completion

-- | @- readAttributeLimitRangeWithCompletion:@
readAttributeLimitRangeWithCompletion :: IsMTRBaseClusterClosureDimension mtrBaseClusterClosureDimension => mtrBaseClusterClosureDimension -> Ptr () -> IO ()
readAttributeLimitRangeWithCompletion mtrBaseClusterClosureDimension completion =
  sendMessage mtrBaseClusterClosureDimension readAttributeLimitRangeWithCompletionSelector completion

-- | @- subscribeAttributeLimitRangeWithParams:subscriptionEstablished:reportHandler:@
subscribeAttributeLimitRangeWithParams_subscriptionEstablished_reportHandler :: (IsMTRBaseClusterClosureDimension mtrBaseClusterClosureDimension, IsMTRSubscribeParams params) => mtrBaseClusterClosureDimension -> params -> Ptr () -> Ptr () -> IO ()
subscribeAttributeLimitRangeWithParams_subscriptionEstablished_reportHandler mtrBaseClusterClosureDimension params subscriptionEstablished reportHandler =
  sendMessage mtrBaseClusterClosureDimension subscribeAttributeLimitRangeWithParams_subscriptionEstablished_reportHandlerSelector (toMTRSubscribeParams params) subscriptionEstablished reportHandler

-- | @+ readAttributeLimitRangeWithClusterStateCache:endpoint:queue:completion:@
readAttributeLimitRangeWithClusterStateCache_endpoint_queue_completion :: (IsMTRClusterStateCacheContainer clusterStateCacheContainer, IsNSNumber endpoint, IsNSObject queue) => clusterStateCacheContainer -> endpoint -> queue -> Ptr () -> IO ()
readAttributeLimitRangeWithClusterStateCache_endpoint_queue_completion clusterStateCacheContainer endpoint queue completion =
  do
    cls' <- getRequiredClass "MTRBaseClusterClosureDimension"
    sendClassMessage cls' readAttributeLimitRangeWithClusterStateCache_endpoint_queue_completionSelector (toMTRClusterStateCacheContainer clusterStateCacheContainer) (toNSNumber endpoint) (toNSObject queue) completion

-- | @- readAttributeTranslationDirectionWithCompletion:@
readAttributeTranslationDirectionWithCompletion :: IsMTRBaseClusterClosureDimension mtrBaseClusterClosureDimension => mtrBaseClusterClosureDimension -> Ptr () -> IO ()
readAttributeTranslationDirectionWithCompletion mtrBaseClusterClosureDimension completion =
  sendMessage mtrBaseClusterClosureDimension readAttributeTranslationDirectionWithCompletionSelector completion

-- | @- subscribeAttributeTranslationDirectionWithParams:subscriptionEstablished:reportHandler:@
subscribeAttributeTranslationDirectionWithParams_subscriptionEstablished_reportHandler :: (IsMTRBaseClusterClosureDimension mtrBaseClusterClosureDimension, IsMTRSubscribeParams params) => mtrBaseClusterClosureDimension -> params -> Ptr () -> Ptr () -> IO ()
subscribeAttributeTranslationDirectionWithParams_subscriptionEstablished_reportHandler mtrBaseClusterClosureDimension params subscriptionEstablished reportHandler =
  sendMessage mtrBaseClusterClosureDimension subscribeAttributeTranslationDirectionWithParams_subscriptionEstablished_reportHandlerSelector (toMTRSubscribeParams params) subscriptionEstablished reportHandler

-- | @+ readAttributeTranslationDirectionWithClusterStateCache:endpoint:queue:completion:@
readAttributeTranslationDirectionWithClusterStateCache_endpoint_queue_completion :: (IsMTRClusterStateCacheContainer clusterStateCacheContainer, IsNSNumber endpoint, IsNSObject queue) => clusterStateCacheContainer -> endpoint -> queue -> Ptr () -> IO ()
readAttributeTranslationDirectionWithClusterStateCache_endpoint_queue_completion clusterStateCacheContainer endpoint queue completion =
  do
    cls' <- getRequiredClass "MTRBaseClusterClosureDimension"
    sendClassMessage cls' readAttributeTranslationDirectionWithClusterStateCache_endpoint_queue_completionSelector (toMTRClusterStateCacheContainer clusterStateCacheContainer) (toNSNumber endpoint) (toNSObject queue) completion

-- | @- readAttributeRotationAxisWithCompletion:@
readAttributeRotationAxisWithCompletion :: IsMTRBaseClusterClosureDimension mtrBaseClusterClosureDimension => mtrBaseClusterClosureDimension -> Ptr () -> IO ()
readAttributeRotationAxisWithCompletion mtrBaseClusterClosureDimension completion =
  sendMessage mtrBaseClusterClosureDimension readAttributeRotationAxisWithCompletionSelector completion

-- | @- subscribeAttributeRotationAxisWithParams:subscriptionEstablished:reportHandler:@
subscribeAttributeRotationAxisWithParams_subscriptionEstablished_reportHandler :: (IsMTRBaseClusterClosureDimension mtrBaseClusterClosureDimension, IsMTRSubscribeParams params) => mtrBaseClusterClosureDimension -> params -> Ptr () -> Ptr () -> IO ()
subscribeAttributeRotationAxisWithParams_subscriptionEstablished_reportHandler mtrBaseClusterClosureDimension params subscriptionEstablished reportHandler =
  sendMessage mtrBaseClusterClosureDimension subscribeAttributeRotationAxisWithParams_subscriptionEstablished_reportHandlerSelector (toMTRSubscribeParams params) subscriptionEstablished reportHandler

-- | @+ readAttributeRotationAxisWithClusterStateCache:endpoint:queue:completion:@
readAttributeRotationAxisWithClusterStateCache_endpoint_queue_completion :: (IsMTRClusterStateCacheContainer clusterStateCacheContainer, IsNSNumber endpoint, IsNSObject queue) => clusterStateCacheContainer -> endpoint -> queue -> Ptr () -> IO ()
readAttributeRotationAxisWithClusterStateCache_endpoint_queue_completion clusterStateCacheContainer endpoint queue completion =
  do
    cls' <- getRequiredClass "MTRBaseClusterClosureDimension"
    sendClassMessage cls' readAttributeRotationAxisWithClusterStateCache_endpoint_queue_completionSelector (toMTRClusterStateCacheContainer clusterStateCacheContainer) (toNSNumber endpoint) (toNSObject queue) completion

-- | @- readAttributeOverflowWithCompletion:@
readAttributeOverflowWithCompletion :: IsMTRBaseClusterClosureDimension mtrBaseClusterClosureDimension => mtrBaseClusterClosureDimension -> Ptr () -> IO ()
readAttributeOverflowWithCompletion mtrBaseClusterClosureDimension completion =
  sendMessage mtrBaseClusterClosureDimension readAttributeOverflowWithCompletionSelector completion

-- | @- subscribeAttributeOverflowWithParams:subscriptionEstablished:reportHandler:@
subscribeAttributeOverflowWithParams_subscriptionEstablished_reportHandler :: (IsMTRBaseClusterClosureDimension mtrBaseClusterClosureDimension, IsMTRSubscribeParams params) => mtrBaseClusterClosureDimension -> params -> Ptr () -> Ptr () -> IO ()
subscribeAttributeOverflowWithParams_subscriptionEstablished_reportHandler mtrBaseClusterClosureDimension params subscriptionEstablished reportHandler =
  sendMessage mtrBaseClusterClosureDimension subscribeAttributeOverflowWithParams_subscriptionEstablished_reportHandlerSelector (toMTRSubscribeParams params) subscriptionEstablished reportHandler

-- | @+ readAttributeOverflowWithClusterStateCache:endpoint:queue:completion:@
readAttributeOverflowWithClusterStateCache_endpoint_queue_completion :: (IsMTRClusterStateCacheContainer clusterStateCacheContainer, IsNSNumber endpoint, IsNSObject queue) => clusterStateCacheContainer -> endpoint -> queue -> Ptr () -> IO ()
readAttributeOverflowWithClusterStateCache_endpoint_queue_completion clusterStateCacheContainer endpoint queue completion =
  do
    cls' <- getRequiredClass "MTRBaseClusterClosureDimension"
    sendClassMessage cls' readAttributeOverflowWithClusterStateCache_endpoint_queue_completionSelector (toMTRClusterStateCacheContainer clusterStateCacheContainer) (toNSNumber endpoint) (toNSObject queue) completion

-- | @- readAttributeModulationTypeWithCompletion:@
readAttributeModulationTypeWithCompletion :: IsMTRBaseClusterClosureDimension mtrBaseClusterClosureDimension => mtrBaseClusterClosureDimension -> Ptr () -> IO ()
readAttributeModulationTypeWithCompletion mtrBaseClusterClosureDimension completion =
  sendMessage mtrBaseClusterClosureDimension readAttributeModulationTypeWithCompletionSelector completion

-- | @- subscribeAttributeModulationTypeWithParams:subscriptionEstablished:reportHandler:@
subscribeAttributeModulationTypeWithParams_subscriptionEstablished_reportHandler :: (IsMTRBaseClusterClosureDimension mtrBaseClusterClosureDimension, IsMTRSubscribeParams params) => mtrBaseClusterClosureDimension -> params -> Ptr () -> Ptr () -> IO ()
subscribeAttributeModulationTypeWithParams_subscriptionEstablished_reportHandler mtrBaseClusterClosureDimension params subscriptionEstablished reportHandler =
  sendMessage mtrBaseClusterClosureDimension subscribeAttributeModulationTypeWithParams_subscriptionEstablished_reportHandlerSelector (toMTRSubscribeParams params) subscriptionEstablished reportHandler

-- | @+ readAttributeModulationTypeWithClusterStateCache:endpoint:queue:completion:@
readAttributeModulationTypeWithClusterStateCache_endpoint_queue_completion :: (IsMTRClusterStateCacheContainer clusterStateCacheContainer, IsNSNumber endpoint, IsNSObject queue) => clusterStateCacheContainer -> endpoint -> queue -> Ptr () -> IO ()
readAttributeModulationTypeWithClusterStateCache_endpoint_queue_completion clusterStateCacheContainer endpoint queue completion =
  do
    cls' <- getRequiredClass "MTRBaseClusterClosureDimension"
    sendClassMessage cls' readAttributeModulationTypeWithClusterStateCache_endpoint_queue_completionSelector (toMTRClusterStateCacheContainer clusterStateCacheContainer) (toNSNumber endpoint) (toNSObject queue) completion

-- | @- readAttributeLatchControlModesWithCompletion:@
readAttributeLatchControlModesWithCompletion :: IsMTRBaseClusterClosureDimension mtrBaseClusterClosureDimension => mtrBaseClusterClosureDimension -> Ptr () -> IO ()
readAttributeLatchControlModesWithCompletion mtrBaseClusterClosureDimension completion =
  sendMessage mtrBaseClusterClosureDimension readAttributeLatchControlModesWithCompletionSelector completion

-- | @- subscribeAttributeLatchControlModesWithParams:subscriptionEstablished:reportHandler:@
subscribeAttributeLatchControlModesWithParams_subscriptionEstablished_reportHandler :: (IsMTRBaseClusterClosureDimension mtrBaseClusterClosureDimension, IsMTRSubscribeParams params) => mtrBaseClusterClosureDimension -> params -> Ptr () -> Ptr () -> IO ()
subscribeAttributeLatchControlModesWithParams_subscriptionEstablished_reportHandler mtrBaseClusterClosureDimension params subscriptionEstablished reportHandler =
  sendMessage mtrBaseClusterClosureDimension subscribeAttributeLatchControlModesWithParams_subscriptionEstablished_reportHandlerSelector (toMTRSubscribeParams params) subscriptionEstablished reportHandler

-- | @+ readAttributeLatchControlModesWithClusterStateCache:endpoint:queue:completion:@
readAttributeLatchControlModesWithClusterStateCache_endpoint_queue_completion :: (IsMTRClusterStateCacheContainer clusterStateCacheContainer, IsNSNumber endpoint, IsNSObject queue) => clusterStateCacheContainer -> endpoint -> queue -> Ptr () -> IO ()
readAttributeLatchControlModesWithClusterStateCache_endpoint_queue_completion clusterStateCacheContainer endpoint queue completion =
  do
    cls' <- getRequiredClass "MTRBaseClusterClosureDimension"
    sendClassMessage cls' readAttributeLatchControlModesWithClusterStateCache_endpoint_queue_completionSelector (toMTRClusterStateCacheContainer clusterStateCacheContainer) (toNSNumber endpoint) (toNSObject queue) completion

-- | @- readAttributeGeneratedCommandListWithCompletion:@
readAttributeGeneratedCommandListWithCompletion :: IsMTRBaseClusterClosureDimension mtrBaseClusterClosureDimension => mtrBaseClusterClosureDimension -> Ptr () -> IO ()
readAttributeGeneratedCommandListWithCompletion mtrBaseClusterClosureDimension completion =
  sendMessage mtrBaseClusterClosureDimension readAttributeGeneratedCommandListWithCompletionSelector completion

-- | @- subscribeAttributeGeneratedCommandListWithParams:subscriptionEstablished:reportHandler:@
subscribeAttributeGeneratedCommandListWithParams_subscriptionEstablished_reportHandler :: (IsMTRBaseClusterClosureDimension mtrBaseClusterClosureDimension, IsMTRSubscribeParams params) => mtrBaseClusterClosureDimension -> params -> Ptr () -> Ptr () -> IO ()
subscribeAttributeGeneratedCommandListWithParams_subscriptionEstablished_reportHandler mtrBaseClusterClosureDimension params subscriptionEstablished reportHandler =
  sendMessage mtrBaseClusterClosureDimension subscribeAttributeGeneratedCommandListWithParams_subscriptionEstablished_reportHandlerSelector (toMTRSubscribeParams params) subscriptionEstablished reportHandler

-- | @+ readAttributeGeneratedCommandListWithClusterStateCache:endpoint:queue:completion:@
readAttributeGeneratedCommandListWithClusterStateCache_endpoint_queue_completion :: (IsMTRClusterStateCacheContainer clusterStateCacheContainer, IsNSNumber endpoint, IsNSObject queue) => clusterStateCacheContainer -> endpoint -> queue -> Ptr () -> IO ()
readAttributeGeneratedCommandListWithClusterStateCache_endpoint_queue_completion clusterStateCacheContainer endpoint queue completion =
  do
    cls' <- getRequiredClass "MTRBaseClusterClosureDimension"
    sendClassMessage cls' readAttributeGeneratedCommandListWithClusterStateCache_endpoint_queue_completionSelector (toMTRClusterStateCacheContainer clusterStateCacheContainer) (toNSNumber endpoint) (toNSObject queue) completion

-- | @- readAttributeAcceptedCommandListWithCompletion:@
readAttributeAcceptedCommandListWithCompletion :: IsMTRBaseClusterClosureDimension mtrBaseClusterClosureDimension => mtrBaseClusterClosureDimension -> Ptr () -> IO ()
readAttributeAcceptedCommandListWithCompletion mtrBaseClusterClosureDimension completion =
  sendMessage mtrBaseClusterClosureDimension readAttributeAcceptedCommandListWithCompletionSelector completion

-- | @- subscribeAttributeAcceptedCommandListWithParams:subscriptionEstablished:reportHandler:@
subscribeAttributeAcceptedCommandListWithParams_subscriptionEstablished_reportHandler :: (IsMTRBaseClusterClosureDimension mtrBaseClusterClosureDimension, IsMTRSubscribeParams params) => mtrBaseClusterClosureDimension -> params -> Ptr () -> Ptr () -> IO ()
subscribeAttributeAcceptedCommandListWithParams_subscriptionEstablished_reportHandler mtrBaseClusterClosureDimension params subscriptionEstablished reportHandler =
  sendMessage mtrBaseClusterClosureDimension subscribeAttributeAcceptedCommandListWithParams_subscriptionEstablished_reportHandlerSelector (toMTRSubscribeParams params) subscriptionEstablished reportHandler

-- | @+ readAttributeAcceptedCommandListWithClusterStateCache:endpoint:queue:completion:@
readAttributeAcceptedCommandListWithClusterStateCache_endpoint_queue_completion :: (IsMTRClusterStateCacheContainer clusterStateCacheContainer, IsNSNumber endpoint, IsNSObject queue) => clusterStateCacheContainer -> endpoint -> queue -> Ptr () -> IO ()
readAttributeAcceptedCommandListWithClusterStateCache_endpoint_queue_completion clusterStateCacheContainer endpoint queue completion =
  do
    cls' <- getRequiredClass "MTRBaseClusterClosureDimension"
    sendClassMessage cls' readAttributeAcceptedCommandListWithClusterStateCache_endpoint_queue_completionSelector (toMTRClusterStateCacheContainer clusterStateCacheContainer) (toNSNumber endpoint) (toNSObject queue) completion

-- | @- readAttributeAttributeListWithCompletion:@
readAttributeAttributeListWithCompletion :: IsMTRBaseClusterClosureDimension mtrBaseClusterClosureDimension => mtrBaseClusterClosureDimension -> Ptr () -> IO ()
readAttributeAttributeListWithCompletion mtrBaseClusterClosureDimension completion =
  sendMessage mtrBaseClusterClosureDimension readAttributeAttributeListWithCompletionSelector completion

-- | @- subscribeAttributeAttributeListWithParams:subscriptionEstablished:reportHandler:@
subscribeAttributeAttributeListWithParams_subscriptionEstablished_reportHandler :: (IsMTRBaseClusterClosureDimension mtrBaseClusterClosureDimension, IsMTRSubscribeParams params) => mtrBaseClusterClosureDimension -> params -> Ptr () -> Ptr () -> IO ()
subscribeAttributeAttributeListWithParams_subscriptionEstablished_reportHandler mtrBaseClusterClosureDimension params subscriptionEstablished reportHandler =
  sendMessage mtrBaseClusterClosureDimension subscribeAttributeAttributeListWithParams_subscriptionEstablished_reportHandlerSelector (toMTRSubscribeParams params) subscriptionEstablished reportHandler

-- | @+ readAttributeAttributeListWithClusterStateCache:endpoint:queue:completion:@
readAttributeAttributeListWithClusterStateCache_endpoint_queue_completion :: (IsMTRClusterStateCacheContainer clusterStateCacheContainer, IsNSNumber endpoint, IsNSObject queue) => clusterStateCacheContainer -> endpoint -> queue -> Ptr () -> IO ()
readAttributeAttributeListWithClusterStateCache_endpoint_queue_completion clusterStateCacheContainer endpoint queue completion =
  do
    cls' <- getRequiredClass "MTRBaseClusterClosureDimension"
    sendClassMessage cls' readAttributeAttributeListWithClusterStateCache_endpoint_queue_completionSelector (toMTRClusterStateCacheContainer clusterStateCacheContainer) (toNSNumber endpoint) (toNSObject queue) completion

-- | @- readAttributeFeatureMapWithCompletion:@
readAttributeFeatureMapWithCompletion :: IsMTRBaseClusterClosureDimension mtrBaseClusterClosureDimension => mtrBaseClusterClosureDimension -> Ptr () -> IO ()
readAttributeFeatureMapWithCompletion mtrBaseClusterClosureDimension completion =
  sendMessage mtrBaseClusterClosureDimension readAttributeFeatureMapWithCompletionSelector completion

-- | @- subscribeAttributeFeatureMapWithParams:subscriptionEstablished:reportHandler:@
subscribeAttributeFeatureMapWithParams_subscriptionEstablished_reportHandler :: (IsMTRBaseClusterClosureDimension mtrBaseClusterClosureDimension, IsMTRSubscribeParams params) => mtrBaseClusterClosureDimension -> params -> Ptr () -> Ptr () -> IO ()
subscribeAttributeFeatureMapWithParams_subscriptionEstablished_reportHandler mtrBaseClusterClosureDimension params subscriptionEstablished reportHandler =
  sendMessage mtrBaseClusterClosureDimension subscribeAttributeFeatureMapWithParams_subscriptionEstablished_reportHandlerSelector (toMTRSubscribeParams params) subscriptionEstablished reportHandler

-- | @+ readAttributeFeatureMapWithClusterStateCache:endpoint:queue:completion:@
readAttributeFeatureMapWithClusterStateCache_endpoint_queue_completion :: (IsMTRClusterStateCacheContainer clusterStateCacheContainer, IsNSNumber endpoint, IsNSObject queue) => clusterStateCacheContainer -> endpoint -> queue -> Ptr () -> IO ()
readAttributeFeatureMapWithClusterStateCache_endpoint_queue_completion clusterStateCacheContainer endpoint queue completion =
  do
    cls' <- getRequiredClass "MTRBaseClusterClosureDimension"
    sendClassMessage cls' readAttributeFeatureMapWithClusterStateCache_endpoint_queue_completionSelector (toMTRClusterStateCacheContainer clusterStateCacheContainer) (toNSNumber endpoint) (toNSObject queue) completion

-- | @- readAttributeClusterRevisionWithCompletion:@
readAttributeClusterRevisionWithCompletion :: IsMTRBaseClusterClosureDimension mtrBaseClusterClosureDimension => mtrBaseClusterClosureDimension -> Ptr () -> IO ()
readAttributeClusterRevisionWithCompletion mtrBaseClusterClosureDimension completion =
  sendMessage mtrBaseClusterClosureDimension readAttributeClusterRevisionWithCompletionSelector completion

-- | @- subscribeAttributeClusterRevisionWithParams:subscriptionEstablished:reportHandler:@
subscribeAttributeClusterRevisionWithParams_subscriptionEstablished_reportHandler :: (IsMTRBaseClusterClosureDimension mtrBaseClusterClosureDimension, IsMTRSubscribeParams params) => mtrBaseClusterClosureDimension -> params -> Ptr () -> Ptr () -> IO ()
subscribeAttributeClusterRevisionWithParams_subscriptionEstablished_reportHandler mtrBaseClusterClosureDimension params subscriptionEstablished reportHandler =
  sendMessage mtrBaseClusterClosureDimension subscribeAttributeClusterRevisionWithParams_subscriptionEstablished_reportHandlerSelector (toMTRSubscribeParams params) subscriptionEstablished reportHandler

-- | @+ readAttributeClusterRevisionWithClusterStateCache:endpoint:queue:completion:@
readAttributeClusterRevisionWithClusterStateCache_endpoint_queue_completion :: (IsMTRClusterStateCacheContainer clusterStateCacheContainer, IsNSNumber endpoint, IsNSObject queue) => clusterStateCacheContainer -> endpoint -> queue -> Ptr () -> IO ()
readAttributeClusterRevisionWithClusterStateCache_endpoint_queue_completion clusterStateCacheContainer endpoint queue completion =
  do
    cls' <- getRequiredClass "MTRBaseClusterClosureDimension"
    sendClassMessage cls' readAttributeClusterRevisionWithClusterStateCache_endpoint_queue_completionSelector (toMTRClusterStateCacheContainer clusterStateCacheContainer) (toNSNumber endpoint) (toNSObject queue) completion

-- | @- init@
init_ :: IsMTRBaseClusterClosureDimension mtrBaseClusterClosureDimension => mtrBaseClusterClosureDimension -> IO (Id MTRBaseClusterClosureDimension)
init_ mtrBaseClusterClosureDimension =
  sendOwnedMessage mtrBaseClusterClosureDimension initSelector

-- | @+ new@
new :: IO (Id MTRBaseClusterClosureDimension)
new  =
  do
    cls' <- getRequiredClass "MTRBaseClusterClosureDimension"
    sendOwnedClassMessage cls' newSelector

-- | For all instance methods (reads, writes, commands) that take a completion, the completion will be called on the provided queue.
--
-- ObjC selector: @- initWithDevice:endpointID:queue:@
initWithDevice_endpointID_queue :: (IsMTRBaseClusterClosureDimension mtrBaseClusterClosureDimension, IsMTRBaseDevice device, IsNSNumber endpointID, IsNSObject queue) => mtrBaseClusterClosureDimension -> device -> endpointID -> queue -> IO (Id MTRBaseClusterClosureDimension)
initWithDevice_endpointID_queue mtrBaseClusterClosureDimension device endpointID queue =
  sendOwnedMessage mtrBaseClusterClosureDimension initWithDevice_endpointID_queueSelector (toMTRBaseDevice device) (toNSNumber endpointID) (toNSObject queue)

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @setTargetWithParams:completion:@
setTargetWithParams_completionSelector :: Selector '[Id MTRClosureDimensionClusterSetTargetParams, Ptr ()] ()
setTargetWithParams_completionSelector = mkSelector "setTargetWithParams:completion:"

-- | @Selector@ for @setTargetWithCompletion:@
setTargetWithCompletionSelector :: Selector '[Ptr ()] ()
setTargetWithCompletionSelector = mkSelector "setTargetWithCompletion:"

-- | @Selector@ for @stepWithParams:completion:@
stepWithParams_completionSelector :: Selector '[Id MTRClosureDimensionClusterStepParams, Ptr ()] ()
stepWithParams_completionSelector = mkSelector "stepWithParams:completion:"

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

-- | @Selector@ for @readAttributeResolutionWithCompletion:@
readAttributeResolutionWithCompletionSelector :: Selector '[Ptr ()] ()
readAttributeResolutionWithCompletionSelector = mkSelector "readAttributeResolutionWithCompletion:"

-- | @Selector@ for @subscribeAttributeResolutionWithParams:subscriptionEstablished:reportHandler:@
subscribeAttributeResolutionWithParams_subscriptionEstablished_reportHandlerSelector :: Selector '[Id MTRSubscribeParams, Ptr (), Ptr ()] ()
subscribeAttributeResolutionWithParams_subscriptionEstablished_reportHandlerSelector = mkSelector "subscribeAttributeResolutionWithParams:subscriptionEstablished:reportHandler:"

-- | @Selector@ for @readAttributeResolutionWithClusterStateCache:endpoint:queue:completion:@
readAttributeResolutionWithClusterStateCache_endpoint_queue_completionSelector :: Selector '[Id MTRClusterStateCacheContainer, Id NSNumber, Id NSObject, Ptr ()] ()
readAttributeResolutionWithClusterStateCache_endpoint_queue_completionSelector = mkSelector "readAttributeResolutionWithClusterStateCache:endpoint:queue:completion:"

-- | @Selector@ for @readAttributeStepValueWithCompletion:@
readAttributeStepValueWithCompletionSelector :: Selector '[Ptr ()] ()
readAttributeStepValueWithCompletionSelector = mkSelector "readAttributeStepValueWithCompletion:"

-- | @Selector@ for @subscribeAttributeStepValueWithParams:subscriptionEstablished:reportHandler:@
subscribeAttributeStepValueWithParams_subscriptionEstablished_reportHandlerSelector :: Selector '[Id MTRSubscribeParams, Ptr (), Ptr ()] ()
subscribeAttributeStepValueWithParams_subscriptionEstablished_reportHandlerSelector = mkSelector "subscribeAttributeStepValueWithParams:subscriptionEstablished:reportHandler:"

-- | @Selector@ for @readAttributeStepValueWithClusterStateCache:endpoint:queue:completion:@
readAttributeStepValueWithClusterStateCache_endpoint_queue_completionSelector :: Selector '[Id MTRClusterStateCacheContainer, Id NSNumber, Id NSObject, Ptr ()] ()
readAttributeStepValueWithClusterStateCache_endpoint_queue_completionSelector = mkSelector "readAttributeStepValueWithClusterStateCache:endpoint:queue:completion:"

-- | @Selector@ for @readAttributeUnitWithCompletion:@
readAttributeUnitWithCompletionSelector :: Selector '[Ptr ()] ()
readAttributeUnitWithCompletionSelector = mkSelector "readAttributeUnitWithCompletion:"

-- | @Selector@ for @subscribeAttributeUnitWithParams:subscriptionEstablished:reportHandler:@
subscribeAttributeUnitWithParams_subscriptionEstablished_reportHandlerSelector :: Selector '[Id MTRSubscribeParams, Ptr (), Ptr ()] ()
subscribeAttributeUnitWithParams_subscriptionEstablished_reportHandlerSelector = mkSelector "subscribeAttributeUnitWithParams:subscriptionEstablished:reportHandler:"

-- | @Selector@ for @readAttributeUnitWithClusterStateCache:endpoint:queue:completion:@
readAttributeUnitWithClusterStateCache_endpoint_queue_completionSelector :: Selector '[Id MTRClusterStateCacheContainer, Id NSNumber, Id NSObject, Ptr ()] ()
readAttributeUnitWithClusterStateCache_endpoint_queue_completionSelector = mkSelector "readAttributeUnitWithClusterStateCache:endpoint:queue:completion:"

-- | @Selector@ for @readAttributeUnitRangeWithCompletion:@
readAttributeUnitRangeWithCompletionSelector :: Selector '[Ptr ()] ()
readAttributeUnitRangeWithCompletionSelector = mkSelector "readAttributeUnitRangeWithCompletion:"

-- | @Selector@ for @subscribeAttributeUnitRangeWithParams:subscriptionEstablished:reportHandler:@
subscribeAttributeUnitRangeWithParams_subscriptionEstablished_reportHandlerSelector :: Selector '[Id MTRSubscribeParams, Ptr (), Ptr ()] ()
subscribeAttributeUnitRangeWithParams_subscriptionEstablished_reportHandlerSelector = mkSelector "subscribeAttributeUnitRangeWithParams:subscriptionEstablished:reportHandler:"

-- | @Selector@ for @readAttributeUnitRangeWithClusterStateCache:endpoint:queue:completion:@
readAttributeUnitRangeWithClusterStateCache_endpoint_queue_completionSelector :: Selector '[Id MTRClusterStateCacheContainer, Id NSNumber, Id NSObject, Ptr ()] ()
readAttributeUnitRangeWithClusterStateCache_endpoint_queue_completionSelector = mkSelector "readAttributeUnitRangeWithClusterStateCache:endpoint:queue:completion:"

-- | @Selector@ for @readAttributeLimitRangeWithCompletion:@
readAttributeLimitRangeWithCompletionSelector :: Selector '[Ptr ()] ()
readAttributeLimitRangeWithCompletionSelector = mkSelector "readAttributeLimitRangeWithCompletion:"

-- | @Selector@ for @subscribeAttributeLimitRangeWithParams:subscriptionEstablished:reportHandler:@
subscribeAttributeLimitRangeWithParams_subscriptionEstablished_reportHandlerSelector :: Selector '[Id MTRSubscribeParams, Ptr (), Ptr ()] ()
subscribeAttributeLimitRangeWithParams_subscriptionEstablished_reportHandlerSelector = mkSelector "subscribeAttributeLimitRangeWithParams:subscriptionEstablished:reportHandler:"

-- | @Selector@ for @readAttributeLimitRangeWithClusterStateCache:endpoint:queue:completion:@
readAttributeLimitRangeWithClusterStateCache_endpoint_queue_completionSelector :: Selector '[Id MTRClusterStateCacheContainer, Id NSNumber, Id NSObject, Ptr ()] ()
readAttributeLimitRangeWithClusterStateCache_endpoint_queue_completionSelector = mkSelector "readAttributeLimitRangeWithClusterStateCache:endpoint:queue:completion:"

-- | @Selector@ for @readAttributeTranslationDirectionWithCompletion:@
readAttributeTranslationDirectionWithCompletionSelector :: Selector '[Ptr ()] ()
readAttributeTranslationDirectionWithCompletionSelector = mkSelector "readAttributeTranslationDirectionWithCompletion:"

-- | @Selector@ for @subscribeAttributeTranslationDirectionWithParams:subscriptionEstablished:reportHandler:@
subscribeAttributeTranslationDirectionWithParams_subscriptionEstablished_reportHandlerSelector :: Selector '[Id MTRSubscribeParams, Ptr (), Ptr ()] ()
subscribeAttributeTranslationDirectionWithParams_subscriptionEstablished_reportHandlerSelector = mkSelector "subscribeAttributeTranslationDirectionWithParams:subscriptionEstablished:reportHandler:"

-- | @Selector@ for @readAttributeTranslationDirectionWithClusterStateCache:endpoint:queue:completion:@
readAttributeTranslationDirectionWithClusterStateCache_endpoint_queue_completionSelector :: Selector '[Id MTRClusterStateCacheContainer, Id NSNumber, Id NSObject, Ptr ()] ()
readAttributeTranslationDirectionWithClusterStateCache_endpoint_queue_completionSelector = mkSelector "readAttributeTranslationDirectionWithClusterStateCache:endpoint:queue:completion:"

-- | @Selector@ for @readAttributeRotationAxisWithCompletion:@
readAttributeRotationAxisWithCompletionSelector :: Selector '[Ptr ()] ()
readAttributeRotationAxisWithCompletionSelector = mkSelector "readAttributeRotationAxisWithCompletion:"

-- | @Selector@ for @subscribeAttributeRotationAxisWithParams:subscriptionEstablished:reportHandler:@
subscribeAttributeRotationAxisWithParams_subscriptionEstablished_reportHandlerSelector :: Selector '[Id MTRSubscribeParams, Ptr (), Ptr ()] ()
subscribeAttributeRotationAxisWithParams_subscriptionEstablished_reportHandlerSelector = mkSelector "subscribeAttributeRotationAxisWithParams:subscriptionEstablished:reportHandler:"

-- | @Selector@ for @readAttributeRotationAxisWithClusterStateCache:endpoint:queue:completion:@
readAttributeRotationAxisWithClusterStateCache_endpoint_queue_completionSelector :: Selector '[Id MTRClusterStateCacheContainer, Id NSNumber, Id NSObject, Ptr ()] ()
readAttributeRotationAxisWithClusterStateCache_endpoint_queue_completionSelector = mkSelector "readAttributeRotationAxisWithClusterStateCache:endpoint:queue:completion:"

-- | @Selector@ for @readAttributeOverflowWithCompletion:@
readAttributeOverflowWithCompletionSelector :: Selector '[Ptr ()] ()
readAttributeOverflowWithCompletionSelector = mkSelector "readAttributeOverflowWithCompletion:"

-- | @Selector@ for @subscribeAttributeOverflowWithParams:subscriptionEstablished:reportHandler:@
subscribeAttributeOverflowWithParams_subscriptionEstablished_reportHandlerSelector :: Selector '[Id MTRSubscribeParams, Ptr (), Ptr ()] ()
subscribeAttributeOverflowWithParams_subscriptionEstablished_reportHandlerSelector = mkSelector "subscribeAttributeOverflowWithParams:subscriptionEstablished:reportHandler:"

-- | @Selector@ for @readAttributeOverflowWithClusterStateCache:endpoint:queue:completion:@
readAttributeOverflowWithClusterStateCache_endpoint_queue_completionSelector :: Selector '[Id MTRClusterStateCacheContainer, Id NSNumber, Id NSObject, Ptr ()] ()
readAttributeOverflowWithClusterStateCache_endpoint_queue_completionSelector = mkSelector "readAttributeOverflowWithClusterStateCache:endpoint:queue:completion:"

-- | @Selector@ for @readAttributeModulationTypeWithCompletion:@
readAttributeModulationTypeWithCompletionSelector :: Selector '[Ptr ()] ()
readAttributeModulationTypeWithCompletionSelector = mkSelector "readAttributeModulationTypeWithCompletion:"

-- | @Selector@ for @subscribeAttributeModulationTypeWithParams:subscriptionEstablished:reportHandler:@
subscribeAttributeModulationTypeWithParams_subscriptionEstablished_reportHandlerSelector :: Selector '[Id MTRSubscribeParams, Ptr (), Ptr ()] ()
subscribeAttributeModulationTypeWithParams_subscriptionEstablished_reportHandlerSelector = mkSelector "subscribeAttributeModulationTypeWithParams:subscriptionEstablished:reportHandler:"

-- | @Selector@ for @readAttributeModulationTypeWithClusterStateCache:endpoint:queue:completion:@
readAttributeModulationTypeWithClusterStateCache_endpoint_queue_completionSelector :: Selector '[Id MTRClusterStateCacheContainer, Id NSNumber, Id NSObject, Ptr ()] ()
readAttributeModulationTypeWithClusterStateCache_endpoint_queue_completionSelector = mkSelector "readAttributeModulationTypeWithClusterStateCache:endpoint:queue:completion:"

-- | @Selector@ for @readAttributeLatchControlModesWithCompletion:@
readAttributeLatchControlModesWithCompletionSelector :: Selector '[Ptr ()] ()
readAttributeLatchControlModesWithCompletionSelector = mkSelector "readAttributeLatchControlModesWithCompletion:"

-- | @Selector@ for @subscribeAttributeLatchControlModesWithParams:subscriptionEstablished:reportHandler:@
subscribeAttributeLatchControlModesWithParams_subscriptionEstablished_reportHandlerSelector :: Selector '[Id MTRSubscribeParams, Ptr (), Ptr ()] ()
subscribeAttributeLatchControlModesWithParams_subscriptionEstablished_reportHandlerSelector = mkSelector "subscribeAttributeLatchControlModesWithParams:subscriptionEstablished:reportHandler:"

-- | @Selector@ for @readAttributeLatchControlModesWithClusterStateCache:endpoint:queue:completion:@
readAttributeLatchControlModesWithClusterStateCache_endpoint_queue_completionSelector :: Selector '[Id MTRClusterStateCacheContainer, Id NSNumber, Id NSObject, Ptr ()] ()
readAttributeLatchControlModesWithClusterStateCache_endpoint_queue_completionSelector = mkSelector "readAttributeLatchControlModesWithClusterStateCache:endpoint:queue:completion:"

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
initSelector :: Selector '[] (Id MTRBaseClusterClosureDimension)
initSelector = mkSelector "init"

-- | @Selector@ for @new@
newSelector :: Selector '[] (Id MTRBaseClusterClosureDimension)
newSelector = mkSelector "new"

-- | @Selector@ for @initWithDevice:endpointID:queue:@
initWithDevice_endpointID_queueSelector :: Selector '[Id MTRBaseDevice, Id NSNumber, Id NSObject] (Id MTRBaseClusterClosureDimension)
initWithDevice_endpointID_queueSelector = mkSelector "initWithDevice:endpointID:queue:"

