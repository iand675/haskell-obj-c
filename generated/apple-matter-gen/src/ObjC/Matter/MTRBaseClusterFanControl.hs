{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Cluster Fan Control
--
-- An interface for controlling a fan in a heating/cooling system.
--
-- Generated bindings for @MTRBaseClusterFanControl@.
module ObjC.Matter.MTRBaseClusterFanControl
  ( MTRBaseClusterFanControl
  , IsMTRBaseClusterFanControl(..)
  , stepWithParams_completion
  , readAttributeFanModeWithCompletion
  , writeAttributeFanModeWithValue_completion
  , writeAttributeFanModeWithValue_params_completion
  , subscribeAttributeFanModeWithParams_subscriptionEstablished_reportHandler
  , readAttributeFanModeWithClusterStateCache_endpoint_queue_completion
  , readAttributeFanModeSequenceWithCompletion
  , writeAttributeFanModeSequenceWithValue_completion
  , writeAttributeFanModeSequenceWithValue_params_completion
  , subscribeAttributeFanModeSequenceWithParams_subscriptionEstablished_reportHandler
  , readAttributeFanModeSequenceWithClusterStateCache_endpoint_queue_completion
  , readAttributePercentSettingWithCompletion
  , writeAttributePercentSettingWithValue_completion
  , writeAttributePercentSettingWithValue_params_completion
  , subscribeAttributePercentSettingWithParams_subscriptionEstablished_reportHandler
  , readAttributePercentSettingWithClusterStateCache_endpoint_queue_completion
  , readAttributePercentCurrentWithCompletion
  , subscribeAttributePercentCurrentWithParams_subscriptionEstablished_reportHandler
  , readAttributePercentCurrentWithClusterStateCache_endpoint_queue_completion
  , readAttributeSpeedMaxWithCompletion
  , subscribeAttributeSpeedMaxWithParams_subscriptionEstablished_reportHandler
  , readAttributeSpeedMaxWithClusterStateCache_endpoint_queue_completion
  , readAttributeSpeedSettingWithCompletion
  , writeAttributeSpeedSettingWithValue_completion
  , writeAttributeSpeedSettingWithValue_params_completion
  , subscribeAttributeSpeedSettingWithParams_subscriptionEstablished_reportHandler
  , readAttributeSpeedSettingWithClusterStateCache_endpoint_queue_completion
  , readAttributeSpeedCurrentWithCompletion
  , subscribeAttributeSpeedCurrentWithParams_subscriptionEstablished_reportHandler
  , readAttributeSpeedCurrentWithClusterStateCache_endpoint_queue_completion
  , readAttributeRockSupportWithCompletion
  , subscribeAttributeRockSupportWithParams_subscriptionEstablished_reportHandler
  , readAttributeRockSupportWithClusterStateCache_endpoint_queue_completion
  , readAttributeRockSettingWithCompletion
  , writeAttributeRockSettingWithValue_completion
  , writeAttributeRockSettingWithValue_params_completion
  , subscribeAttributeRockSettingWithParams_subscriptionEstablished_reportHandler
  , readAttributeRockSettingWithClusterStateCache_endpoint_queue_completion
  , readAttributeWindSupportWithCompletion
  , subscribeAttributeWindSupportWithParams_subscriptionEstablished_reportHandler
  , readAttributeWindSupportWithClusterStateCache_endpoint_queue_completion
  , readAttributeWindSettingWithCompletion
  , writeAttributeWindSettingWithValue_completion
  , writeAttributeWindSettingWithValue_params_completion
  , subscribeAttributeWindSettingWithParams_subscriptionEstablished_reportHandler
  , readAttributeWindSettingWithClusterStateCache_endpoint_queue_completion
  , readAttributeAirflowDirectionWithCompletion
  , writeAttributeAirflowDirectionWithValue_completion
  , writeAttributeAirflowDirectionWithValue_params_completion
  , subscribeAttributeAirflowDirectionWithParams_subscriptionEstablished_reportHandler
  , readAttributeAirflowDirectionWithClusterStateCache_endpoint_queue_completion
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
  , initWithDevice_endpoint_queue
  , readAttributeFanModeWithCompletionHandler
  , writeAttributeFanModeWithValue_completionHandler
  , writeAttributeFanModeWithValue_params_completionHandler
  , subscribeAttributeFanModeWithMinInterval_maxInterval_params_subscriptionEstablished_reportHandler
  , readAttributeFanModeWithAttributeCache_endpoint_queue_completionHandler
  , readAttributeFanModeSequenceWithCompletionHandler
  , writeAttributeFanModeSequenceWithValue_completionHandler
  , writeAttributeFanModeSequenceWithValue_params_completionHandler
  , subscribeAttributeFanModeSequenceWithMinInterval_maxInterval_params_subscriptionEstablished_reportHandler
  , readAttributeFanModeSequenceWithAttributeCache_endpoint_queue_completionHandler
  , readAttributePercentSettingWithCompletionHandler
  , writeAttributePercentSettingWithValue_completionHandler
  , writeAttributePercentSettingWithValue_params_completionHandler
  , subscribeAttributePercentSettingWithMinInterval_maxInterval_params_subscriptionEstablished_reportHandler
  , readAttributePercentSettingWithAttributeCache_endpoint_queue_completionHandler
  , readAttributePercentCurrentWithCompletionHandler
  , subscribeAttributePercentCurrentWithMinInterval_maxInterval_params_subscriptionEstablished_reportHandler
  , readAttributePercentCurrentWithAttributeCache_endpoint_queue_completionHandler
  , readAttributeSpeedMaxWithCompletionHandler
  , subscribeAttributeSpeedMaxWithMinInterval_maxInterval_params_subscriptionEstablished_reportHandler
  , readAttributeSpeedMaxWithAttributeCache_endpoint_queue_completionHandler
  , readAttributeSpeedSettingWithCompletionHandler
  , writeAttributeSpeedSettingWithValue_completionHandler
  , writeAttributeSpeedSettingWithValue_params_completionHandler
  , subscribeAttributeSpeedSettingWithMinInterval_maxInterval_params_subscriptionEstablished_reportHandler
  , readAttributeSpeedSettingWithAttributeCache_endpoint_queue_completionHandler
  , readAttributeSpeedCurrentWithCompletionHandler
  , subscribeAttributeSpeedCurrentWithMinInterval_maxInterval_params_subscriptionEstablished_reportHandler
  , readAttributeSpeedCurrentWithAttributeCache_endpoint_queue_completionHandler
  , readAttributeRockSupportWithCompletionHandler
  , subscribeAttributeRockSupportWithMinInterval_maxInterval_params_subscriptionEstablished_reportHandler
  , readAttributeRockSupportWithAttributeCache_endpoint_queue_completionHandler
  , readAttributeRockSettingWithCompletionHandler
  , writeAttributeRockSettingWithValue_completionHandler
  , writeAttributeRockSettingWithValue_params_completionHandler
  , subscribeAttributeRockSettingWithMinInterval_maxInterval_params_subscriptionEstablished_reportHandler
  , readAttributeRockSettingWithAttributeCache_endpoint_queue_completionHandler
  , readAttributeWindSupportWithCompletionHandler
  , subscribeAttributeWindSupportWithMinInterval_maxInterval_params_subscriptionEstablished_reportHandler
  , readAttributeWindSupportWithAttributeCache_endpoint_queue_completionHandler
  , readAttributeWindSettingWithCompletionHandler
  , writeAttributeWindSettingWithValue_completionHandler
  , writeAttributeWindSettingWithValue_params_completionHandler
  , subscribeAttributeWindSettingWithMinInterval_maxInterval_params_subscriptionEstablished_reportHandler
  , readAttributeWindSettingWithAttributeCache_endpoint_queue_completionHandler
  , readAttributeGeneratedCommandListWithCompletionHandler
  , subscribeAttributeGeneratedCommandListWithMinInterval_maxInterval_params_subscriptionEstablished_reportHandler
  , readAttributeGeneratedCommandListWithAttributeCache_endpoint_queue_completionHandler
  , readAttributeAcceptedCommandListWithCompletionHandler
  , subscribeAttributeAcceptedCommandListWithMinInterval_maxInterval_params_subscriptionEstablished_reportHandler
  , readAttributeAcceptedCommandListWithAttributeCache_endpoint_queue_completionHandler
  , readAttributeAttributeListWithCompletionHandler
  , subscribeAttributeAttributeListWithMinInterval_maxInterval_params_subscriptionEstablished_reportHandler
  , readAttributeAttributeListWithAttributeCache_endpoint_queue_completionHandler
  , readAttributeFeatureMapWithCompletionHandler
  , subscribeAttributeFeatureMapWithMinInterval_maxInterval_params_subscriptionEstablished_reportHandler
  , readAttributeFeatureMapWithAttributeCache_endpoint_queue_completionHandler
  , readAttributeClusterRevisionWithCompletionHandler
  , subscribeAttributeClusterRevisionWithMinInterval_maxInterval_params_subscriptionEstablished_reportHandler
  , readAttributeClusterRevisionWithAttributeCache_endpoint_queue_completionHandler
  , initWithDevice_endpointID_queue
  , initSelector
  , initWithDevice_endpointID_queueSelector
  , initWithDevice_endpoint_queueSelector
  , newSelector
  , readAttributeAcceptedCommandListWithAttributeCache_endpoint_queue_completionHandlerSelector
  , readAttributeAcceptedCommandListWithClusterStateCache_endpoint_queue_completionSelector
  , readAttributeAcceptedCommandListWithCompletionHandlerSelector
  , readAttributeAcceptedCommandListWithCompletionSelector
  , readAttributeAirflowDirectionWithClusterStateCache_endpoint_queue_completionSelector
  , readAttributeAirflowDirectionWithCompletionSelector
  , readAttributeAttributeListWithAttributeCache_endpoint_queue_completionHandlerSelector
  , readAttributeAttributeListWithClusterStateCache_endpoint_queue_completionSelector
  , readAttributeAttributeListWithCompletionHandlerSelector
  , readAttributeAttributeListWithCompletionSelector
  , readAttributeClusterRevisionWithAttributeCache_endpoint_queue_completionHandlerSelector
  , readAttributeClusterRevisionWithClusterStateCache_endpoint_queue_completionSelector
  , readAttributeClusterRevisionWithCompletionHandlerSelector
  , readAttributeClusterRevisionWithCompletionSelector
  , readAttributeFanModeSequenceWithAttributeCache_endpoint_queue_completionHandlerSelector
  , readAttributeFanModeSequenceWithClusterStateCache_endpoint_queue_completionSelector
  , readAttributeFanModeSequenceWithCompletionHandlerSelector
  , readAttributeFanModeSequenceWithCompletionSelector
  , readAttributeFanModeWithAttributeCache_endpoint_queue_completionHandlerSelector
  , readAttributeFanModeWithClusterStateCache_endpoint_queue_completionSelector
  , readAttributeFanModeWithCompletionHandlerSelector
  , readAttributeFanModeWithCompletionSelector
  , readAttributeFeatureMapWithAttributeCache_endpoint_queue_completionHandlerSelector
  , readAttributeFeatureMapWithClusterStateCache_endpoint_queue_completionSelector
  , readAttributeFeatureMapWithCompletionHandlerSelector
  , readAttributeFeatureMapWithCompletionSelector
  , readAttributeGeneratedCommandListWithAttributeCache_endpoint_queue_completionHandlerSelector
  , readAttributeGeneratedCommandListWithClusterStateCache_endpoint_queue_completionSelector
  , readAttributeGeneratedCommandListWithCompletionHandlerSelector
  , readAttributeGeneratedCommandListWithCompletionSelector
  , readAttributePercentCurrentWithAttributeCache_endpoint_queue_completionHandlerSelector
  , readAttributePercentCurrentWithClusterStateCache_endpoint_queue_completionSelector
  , readAttributePercentCurrentWithCompletionHandlerSelector
  , readAttributePercentCurrentWithCompletionSelector
  , readAttributePercentSettingWithAttributeCache_endpoint_queue_completionHandlerSelector
  , readAttributePercentSettingWithClusterStateCache_endpoint_queue_completionSelector
  , readAttributePercentSettingWithCompletionHandlerSelector
  , readAttributePercentSettingWithCompletionSelector
  , readAttributeRockSettingWithAttributeCache_endpoint_queue_completionHandlerSelector
  , readAttributeRockSettingWithClusterStateCache_endpoint_queue_completionSelector
  , readAttributeRockSettingWithCompletionHandlerSelector
  , readAttributeRockSettingWithCompletionSelector
  , readAttributeRockSupportWithAttributeCache_endpoint_queue_completionHandlerSelector
  , readAttributeRockSupportWithClusterStateCache_endpoint_queue_completionSelector
  , readAttributeRockSupportWithCompletionHandlerSelector
  , readAttributeRockSupportWithCompletionSelector
  , readAttributeSpeedCurrentWithAttributeCache_endpoint_queue_completionHandlerSelector
  , readAttributeSpeedCurrentWithClusterStateCache_endpoint_queue_completionSelector
  , readAttributeSpeedCurrentWithCompletionHandlerSelector
  , readAttributeSpeedCurrentWithCompletionSelector
  , readAttributeSpeedMaxWithAttributeCache_endpoint_queue_completionHandlerSelector
  , readAttributeSpeedMaxWithClusterStateCache_endpoint_queue_completionSelector
  , readAttributeSpeedMaxWithCompletionHandlerSelector
  , readAttributeSpeedMaxWithCompletionSelector
  , readAttributeSpeedSettingWithAttributeCache_endpoint_queue_completionHandlerSelector
  , readAttributeSpeedSettingWithClusterStateCache_endpoint_queue_completionSelector
  , readAttributeSpeedSettingWithCompletionHandlerSelector
  , readAttributeSpeedSettingWithCompletionSelector
  , readAttributeWindSettingWithAttributeCache_endpoint_queue_completionHandlerSelector
  , readAttributeWindSettingWithClusterStateCache_endpoint_queue_completionSelector
  , readAttributeWindSettingWithCompletionHandlerSelector
  , readAttributeWindSettingWithCompletionSelector
  , readAttributeWindSupportWithAttributeCache_endpoint_queue_completionHandlerSelector
  , readAttributeWindSupportWithClusterStateCache_endpoint_queue_completionSelector
  , readAttributeWindSupportWithCompletionHandlerSelector
  , readAttributeWindSupportWithCompletionSelector
  , stepWithParams_completionSelector
  , subscribeAttributeAcceptedCommandListWithMinInterval_maxInterval_params_subscriptionEstablished_reportHandlerSelector
  , subscribeAttributeAcceptedCommandListWithParams_subscriptionEstablished_reportHandlerSelector
  , subscribeAttributeAirflowDirectionWithParams_subscriptionEstablished_reportHandlerSelector
  , subscribeAttributeAttributeListWithMinInterval_maxInterval_params_subscriptionEstablished_reportHandlerSelector
  , subscribeAttributeAttributeListWithParams_subscriptionEstablished_reportHandlerSelector
  , subscribeAttributeClusterRevisionWithMinInterval_maxInterval_params_subscriptionEstablished_reportHandlerSelector
  , subscribeAttributeClusterRevisionWithParams_subscriptionEstablished_reportHandlerSelector
  , subscribeAttributeFanModeSequenceWithMinInterval_maxInterval_params_subscriptionEstablished_reportHandlerSelector
  , subscribeAttributeFanModeSequenceWithParams_subscriptionEstablished_reportHandlerSelector
  , subscribeAttributeFanModeWithMinInterval_maxInterval_params_subscriptionEstablished_reportHandlerSelector
  , subscribeAttributeFanModeWithParams_subscriptionEstablished_reportHandlerSelector
  , subscribeAttributeFeatureMapWithMinInterval_maxInterval_params_subscriptionEstablished_reportHandlerSelector
  , subscribeAttributeFeatureMapWithParams_subscriptionEstablished_reportHandlerSelector
  , subscribeAttributeGeneratedCommandListWithMinInterval_maxInterval_params_subscriptionEstablished_reportHandlerSelector
  , subscribeAttributeGeneratedCommandListWithParams_subscriptionEstablished_reportHandlerSelector
  , subscribeAttributePercentCurrentWithMinInterval_maxInterval_params_subscriptionEstablished_reportHandlerSelector
  , subscribeAttributePercentCurrentWithParams_subscriptionEstablished_reportHandlerSelector
  , subscribeAttributePercentSettingWithMinInterval_maxInterval_params_subscriptionEstablished_reportHandlerSelector
  , subscribeAttributePercentSettingWithParams_subscriptionEstablished_reportHandlerSelector
  , subscribeAttributeRockSettingWithMinInterval_maxInterval_params_subscriptionEstablished_reportHandlerSelector
  , subscribeAttributeRockSettingWithParams_subscriptionEstablished_reportHandlerSelector
  , subscribeAttributeRockSupportWithMinInterval_maxInterval_params_subscriptionEstablished_reportHandlerSelector
  , subscribeAttributeRockSupportWithParams_subscriptionEstablished_reportHandlerSelector
  , subscribeAttributeSpeedCurrentWithMinInterval_maxInterval_params_subscriptionEstablished_reportHandlerSelector
  , subscribeAttributeSpeedCurrentWithParams_subscriptionEstablished_reportHandlerSelector
  , subscribeAttributeSpeedMaxWithMinInterval_maxInterval_params_subscriptionEstablished_reportHandlerSelector
  , subscribeAttributeSpeedMaxWithParams_subscriptionEstablished_reportHandlerSelector
  , subscribeAttributeSpeedSettingWithMinInterval_maxInterval_params_subscriptionEstablished_reportHandlerSelector
  , subscribeAttributeSpeedSettingWithParams_subscriptionEstablished_reportHandlerSelector
  , subscribeAttributeWindSettingWithMinInterval_maxInterval_params_subscriptionEstablished_reportHandlerSelector
  , subscribeAttributeWindSettingWithParams_subscriptionEstablished_reportHandlerSelector
  , subscribeAttributeWindSupportWithMinInterval_maxInterval_params_subscriptionEstablished_reportHandlerSelector
  , subscribeAttributeWindSupportWithParams_subscriptionEstablished_reportHandlerSelector
  , writeAttributeAirflowDirectionWithValue_completionSelector
  , writeAttributeAirflowDirectionWithValue_params_completionSelector
  , writeAttributeFanModeSequenceWithValue_completionHandlerSelector
  , writeAttributeFanModeSequenceWithValue_completionSelector
  , writeAttributeFanModeSequenceWithValue_params_completionHandlerSelector
  , writeAttributeFanModeSequenceWithValue_params_completionSelector
  , writeAttributeFanModeWithValue_completionHandlerSelector
  , writeAttributeFanModeWithValue_completionSelector
  , writeAttributeFanModeWithValue_params_completionHandlerSelector
  , writeAttributeFanModeWithValue_params_completionSelector
  , writeAttributePercentSettingWithValue_completionHandlerSelector
  , writeAttributePercentSettingWithValue_completionSelector
  , writeAttributePercentSettingWithValue_params_completionHandlerSelector
  , writeAttributePercentSettingWithValue_params_completionSelector
  , writeAttributeRockSettingWithValue_completionHandlerSelector
  , writeAttributeRockSettingWithValue_completionSelector
  , writeAttributeRockSettingWithValue_params_completionHandlerSelector
  , writeAttributeRockSettingWithValue_params_completionSelector
  , writeAttributeSpeedSettingWithValue_completionHandlerSelector
  , writeAttributeSpeedSettingWithValue_completionSelector
  , writeAttributeSpeedSettingWithValue_params_completionHandlerSelector
  , writeAttributeSpeedSettingWithValue_params_completionSelector
  , writeAttributeWindSettingWithValue_completionHandlerSelector
  , writeAttributeWindSettingWithValue_completionSelector
  , writeAttributeWindSettingWithValue_params_completionHandlerSelector
  , writeAttributeWindSettingWithValue_params_completionSelector


  ) where

import Foreign.Ptr (Ptr, FunPtr)
import Foreign.C.Types

import ObjC.Runtime.Types
import ObjC.Runtime.Message (sendMessage, sendOwnedMessage, sendClassMessage, sendOwnedClassMessage)
import ObjC.Runtime.Selector (mkSelector)
import ObjC.Runtime.Class (getRequiredClass)

import ObjC.Matter.Internal.Classes
import ObjC.Foundation.Internal.Classes

-- | Command Step
--
-- This command speeds up or slows down the fan, in steps, without a client having to know the fan speed.
--
-- ObjC selector: @- stepWithParams:completion:@
stepWithParams_completion :: (IsMTRBaseClusterFanControl mtrBaseClusterFanControl, IsMTRFanControlClusterStepParams params) => mtrBaseClusterFanControl -> params -> Ptr () -> IO ()
stepWithParams_completion mtrBaseClusterFanControl params completion =
  sendMessage mtrBaseClusterFanControl stepWithParams_completionSelector (toMTRFanControlClusterStepParams params) completion

-- | @- readAttributeFanModeWithCompletion:@
readAttributeFanModeWithCompletion :: IsMTRBaseClusterFanControl mtrBaseClusterFanControl => mtrBaseClusterFanControl -> Ptr () -> IO ()
readAttributeFanModeWithCompletion mtrBaseClusterFanControl completion =
  sendMessage mtrBaseClusterFanControl readAttributeFanModeWithCompletionSelector completion

-- | @- writeAttributeFanModeWithValue:completion:@
writeAttributeFanModeWithValue_completion :: (IsMTRBaseClusterFanControl mtrBaseClusterFanControl, IsNSNumber value) => mtrBaseClusterFanControl -> value -> Ptr () -> IO ()
writeAttributeFanModeWithValue_completion mtrBaseClusterFanControl value completion =
  sendMessage mtrBaseClusterFanControl writeAttributeFanModeWithValue_completionSelector (toNSNumber value) completion

-- | @- writeAttributeFanModeWithValue:params:completion:@
writeAttributeFanModeWithValue_params_completion :: (IsMTRBaseClusterFanControl mtrBaseClusterFanControl, IsNSNumber value, IsMTRWriteParams params) => mtrBaseClusterFanControl -> value -> params -> Ptr () -> IO ()
writeAttributeFanModeWithValue_params_completion mtrBaseClusterFanControl value params completion =
  sendMessage mtrBaseClusterFanControl writeAttributeFanModeWithValue_params_completionSelector (toNSNumber value) (toMTRWriteParams params) completion

-- | @- subscribeAttributeFanModeWithParams:subscriptionEstablished:reportHandler:@
subscribeAttributeFanModeWithParams_subscriptionEstablished_reportHandler :: (IsMTRBaseClusterFanControl mtrBaseClusterFanControl, IsMTRSubscribeParams params) => mtrBaseClusterFanControl -> params -> Ptr () -> Ptr () -> IO ()
subscribeAttributeFanModeWithParams_subscriptionEstablished_reportHandler mtrBaseClusterFanControl params subscriptionEstablished reportHandler =
  sendMessage mtrBaseClusterFanControl subscribeAttributeFanModeWithParams_subscriptionEstablished_reportHandlerSelector (toMTRSubscribeParams params) subscriptionEstablished reportHandler

-- | @+ readAttributeFanModeWithClusterStateCache:endpoint:queue:completion:@
readAttributeFanModeWithClusterStateCache_endpoint_queue_completion :: (IsMTRClusterStateCacheContainer clusterStateCacheContainer, IsNSNumber endpoint, IsNSObject queue) => clusterStateCacheContainer -> endpoint -> queue -> Ptr () -> IO ()
readAttributeFanModeWithClusterStateCache_endpoint_queue_completion clusterStateCacheContainer endpoint queue completion =
  do
    cls' <- getRequiredClass "MTRBaseClusterFanControl"
    sendClassMessage cls' readAttributeFanModeWithClusterStateCache_endpoint_queue_completionSelector (toMTRClusterStateCacheContainer clusterStateCacheContainer) (toNSNumber endpoint) (toNSObject queue) completion

-- | @- readAttributeFanModeSequenceWithCompletion:@
readAttributeFanModeSequenceWithCompletion :: IsMTRBaseClusterFanControl mtrBaseClusterFanControl => mtrBaseClusterFanControl -> Ptr () -> IO ()
readAttributeFanModeSequenceWithCompletion mtrBaseClusterFanControl completion =
  sendMessage mtrBaseClusterFanControl readAttributeFanModeSequenceWithCompletionSelector completion

-- | @- writeAttributeFanModeSequenceWithValue:completion:@
writeAttributeFanModeSequenceWithValue_completion :: (IsMTRBaseClusterFanControl mtrBaseClusterFanControl, IsNSNumber value) => mtrBaseClusterFanControl -> value -> Ptr () -> IO ()
writeAttributeFanModeSequenceWithValue_completion mtrBaseClusterFanControl value completion =
  sendMessage mtrBaseClusterFanControl writeAttributeFanModeSequenceWithValue_completionSelector (toNSNumber value) completion

-- | @- writeAttributeFanModeSequenceWithValue:params:completion:@
writeAttributeFanModeSequenceWithValue_params_completion :: (IsMTRBaseClusterFanControl mtrBaseClusterFanControl, IsNSNumber value, IsMTRWriteParams params) => mtrBaseClusterFanControl -> value -> params -> Ptr () -> IO ()
writeAttributeFanModeSequenceWithValue_params_completion mtrBaseClusterFanControl value params completion =
  sendMessage mtrBaseClusterFanControl writeAttributeFanModeSequenceWithValue_params_completionSelector (toNSNumber value) (toMTRWriteParams params) completion

-- | @- subscribeAttributeFanModeSequenceWithParams:subscriptionEstablished:reportHandler:@
subscribeAttributeFanModeSequenceWithParams_subscriptionEstablished_reportHandler :: (IsMTRBaseClusterFanControl mtrBaseClusterFanControl, IsMTRSubscribeParams params) => mtrBaseClusterFanControl -> params -> Ptr () -> Ptr () -> IO ()
subscribeAttributeFanModeSequenceWithParams_subscriptionEstablished_reportHandler mtrBaseClusterFanControl params subscriptionEstablished reportHandler =
  sendMessage mtrBaseClusterFanControl subscribeAttributeFanModeSequenceWithParams_subscriptionEstablished_reportHandlerSelector (toMTRSubscribeParams params) subscriptionEstablished reportHandler

-- | @+ readAttributeFanModeSequenceWithClusterStateCache:endpoint:queue:completion:@
readAttributeFanModeSequenceWithClusterStateCache_endpoint_queue_completion :: (IsMTRClusterStateCacheContainer clusterStateCacheContainer, IsNSNumber endpoint, IsNSObject queue) => clusterStateCacheContainer -> endpoint -> queue -> Ptr () -> IO ()
readAttributeFanModeSequenceWithClusterStateCache_endpoint_queue_completion clusterStateCacheContainer endpoint queue completion =
  do
    cls' <- getRequiredClass "MTRBaseClusterFanControl"
    sendClassMessage cls' readAttributeFanModeSequenceWithClusterStateCache_endpoint_queue_completionSelector (toMTRClusterStateCacheContainer clusterStateCacheContainer) (toNSNumber endpoint) (toNSObject queue) completion

-- | @- readAttributePercentSettingWithCompletion:@
readAttributePercentSettingWithCompletion :: IsMTRBaseClusterFanControl mtrBaseClusterFanControl => mtrBaseClusterFanControl -> Ptr () -> IO ()
readAttributePercentSettingWithCompletion mtrBaseClusterFanControl completion =
  sendMessage mtrBaseClusterFanControl readAttributePercentSettingWithCompletionSelector completion

-- | @- writeAttributePercentSettingWithValue:completion:@
writeAttributePercentSettingWithValue_completion :: (IsMTRBaseClusterFanControl mtrBaseClusterFanControl, IsNSNumber value) => mtrBaseClusterFanControl -> value -> Ptr () -> IO ()
writeAttributePercentSettingWithValue_completion mtrBaseClusterFanControl value completion =
  sendMessage mtrBaseClusterFanControl writeAttributePercentSettingWithValue_completionSelector (toNSNumber value) completion

-- | @- writeAttributePercentSettingWithValue:params:completion:@
writeAttributePercentSettingWithValue_params_completion :: (IsMTRBaseClusterFanControl mtrBaseClusterFanControl, IsNSNumber value, IsMTRWriteParams params) => mtrBaseClusterFanControl -> value -> params -> Ptr () -> IO ()
writeAttributePercentSettingWithValue_params_completion mtrBaseClusterFanControl value params completion =
  sendMessage mtrBaseClusterFanControl writeAttributePercentSettingWithValue_params_completionSelector (toNSNumber value) (toMTRWriteParams params) completion

-- | @- subscribeAttributePercentSettingWithParams:subscriptionEstablished:reportHandler:@
subscribeAttributePercentSettingWithParams_subscriptionEstablished_reportHandler :: (IsMTRBaseClusterFanControl mtrBaseClusterFanControl, IsMTRSubscribeParams params) => mtrBaseClusterFanControl -> params -> Ptr () -> Ptr () -> IO ()
subscribeAttributePercentSettingWithParams_subscriptionEstablished_reportHandler mtrBaseClusterFanControl params subscriptionEstablished reportHandler =
  sendMessage mtrBaseClusterFanControl subscribeAttributePercentSettingWithParams_subscriptionEstablished_reportHandlerSelector (toMTRSubscribeParams params) subscriptionEstablished reportHandler

-- | @+ readAttributePercentSettingWithClusterStateCache:endpoint:queue:completion:@
readAttributePercentSettingWithClusterStateCache_endpoint_queue_completion :: (IsMTRClusterStateCacheContainer clusterStateCacheContainer, IsNSNumber endpoint, IsNSObject queue) => clusterStateCacheContainer -> endpoint -> queue -> Ptr () -> IO ()
readAttributePercentSettingWithClusterStateCache_endpoint_queue_completion clusterStateCacheContainer endpoint queue completion =
  do
    cls' <- getRequiredClass "MTRBaseClusterFanControl"
    sendClassMessage cls' readAttributePercentSettingWithClusterStateCache_endpoint_queue_completionSelector (toMTRClusterStateCacheContainer clusterStateCacheContainer) (toNSNumber endpoint) (toNSObject queue) completion

-- | @- readAttributePercentCurrentWithCompletion:@
readAttributePercentCurrentWithCompletion :: IsMTRBaseClusterFanControl mtrBaseClusterFanControl => mtrBaseClusterFanControl -> Ptr () -> IO ()
readAttributePercentCurrentWithCompletion mtrBaseClusterFanControl completion =
  sendMessage mtrBaseClusterFanControl readAttributePercentCurrentWithCompletionSelector completion

-- | @- subscribeAttributePercentCurrentWithParams:subscriptionEstablished:reportHandler:@
subscribeAttributePercentCurrentWithParams_subscriptionEstablished_reportHandler :: (IsMTRBaseClusterFanControl mtrBaseClusterFanControl, IsMTRSubscribeParams params) => mtrBaseClusterFanControl -> params -> Ptr () -> Ptr () -> IO ()
subscribeAttributePercentCurrentWithParams_subscriptionEstablished_reportHandler mtrBaseClusterFanControl params subscriptionEstablished reportHandler =
  sendMessage mtrBaseClusterFanControl subscribeAttributePercentCurrentWithParams_subscriptionEstablished_reportHandlerSelector (toMTRSubscribeParams params) subscriptionEstablished reportHandler

-- | @+ readAttributePercentCurrentWithClusterStateCache:endpoint:queue:completion:@
readAttributePercentCurrentWithClusterStateCache_endpoint_queue_completion :: (IsMTRClusterStateCacheContainer clusterStateCacheContainer, IsNSNumber endpoint, IsNSObject queue) => clusterStateCacheContainer -> endpoint -> queue -> Ptr () -> IO ()
readAttributePercentCurrentWithClusterStateCache_endpoint_queue_completion clusterStateCacheContainer endpoint queue completion =
  do
    cls' <- getRequiredClass "MTRBaseClusterFanControl"
    sendClassMessage cls' readAttributePercentCurrentWithClusterStateCache_endpoint_queue_completionSelector (toMTRClusterStateCacheContainer clusterStateCacheContainer) (toNSNumber endpoint) (toNSObject queue) completion

-- | @- readAttributeSpeedMaxWithCompletion:@
readAttributeSpeedMaxWithCompletion :: IsMTRBaseClusterFanControl mtrBaseClusterFanControl => mtrBaseClusterFanControl -> Ptr () -> IO ()
readAttributeSpeedMaxWithCompletion mtrBaseClusterFanControl completion =
  sendMessage mtrBaseClusterFanControl readAttributeSpeedMaxWithCompletionSelector completion

-- | @- subscribeAttributeSpeedMaxWithParams:subscriptionEstablished:reportHandler:@
subscribeAttributeSpeedMaxWithParams_subscriptionEstablished_reportHandler :: (IsMTRBaseClusterFanControl mtrBaseClusterFanControl, IsMTRSubscribeParams params) => mtrBaseClusterFanControl -> params -> Ptr () -> Ptr () -> IO ()
subscribeAttributeSpeedMaxWithParams_subscriptionEstablished_reportHandler mtrBaseClusterFanControl params subscriptionEstablished reportHandler =
  sendMessage mtrBaseClusterFanControl subscribeAttributeSpeedMaxWithParams_subscriptionEstablished_reportHandlerSelector (toMTRSubscribeParams params) subscriptionEstablished reportHandler

-- | @+ readAttributeSpeedMaxWithClusterStateCache:endpoint:queue:completion:@
readAttributeSpeedMaxWithClusterStateCache_endpoint_queue_completion :: (IsMTRClusterStateCacheContainer clusterStateCacheContainer, IsNSNumber endpoint, IsNSObject queue) => clusterStateCacheContainer -> endpoint -> queue -> Ptr () -> IO ()
readAttributeSpeedMaxWithClusterStateCache_endpoint_queue_completion clusterStateCacheContainer endpoint queue completion =
  do
    cls' <- getRequiredClass "MTRBaseClusterFanControl"
    sendClassMessage cls' readAttributeSpeedMaxWithClusterStateCache_endpoint_queue_completionSelector (toMTRClusterStateCacheContainer clusterStateCacheContainer) (toNSNumber endpoint) (toNSObject queue) completion

-- | @- readAttributeSpeedSettingWithCompletion:@
readAttributeSpeedSettingWithCompletion :: IsMTRBaseClusterFanControl mtrBaseClusterFanControl => mtrBaseClusterFanControl -> Ptr () -> IO ()
readAttributeSpeedSettingWithCompletion mtrBaseClusterFanControl completion =
  sendMessage mtrBaseClusterFanControl readAttributeSpeedSettingWithCompletionSelector completion

-- | @- writeAttributeSpeedSettingWithValue:completion:@
writeAttributeSpeedSettingWithValue_completion :: (IsMTRBaseClusterFanControl mtrBaseClusterFanControl, IsNSNumber value) => mtrBaseClusterFanControl -> value -> Ptr () -> IO ()
writeAttributeSpeedSettingWithValue_completion mtrBaseClusterFanControl value completion =
  sendMessage mtrBaseClusterFanControl writeAttributeSpeedSettingWithValue_completionSelector (toNSNumber value) completion

-- | @- writeAttributeSpeedSettingWithValue:params:completion:@
writeAttributeSpeedSettingWithValue_params_completion :: (IsMTRBaseClusterFanControl mtrBaseClusterFanControl, IsNSNumber value, IsMTRWriteParams params) => mtrBaseClusterFanControl -> value -> params -> Ptr () -> IO ()
writeAttributeSpeedSettingWithValue_params_completion mtrBaseClusterFanControl value params completion =
  sendMessage mtrBaseClusterFanControl writeAttributeSpeedSettingWithValue_params_completionSelector (toNSNumber value) (toMTRWriteParams params) completion

-- | @- subscribeAttributeSpeedSettingWithParams:subscriptionEstablished:reportHandler:@
subscribeAttributeSpeedSettingWithParams_subscriptionEstablished_reportHandler :: (IsMTRBaseClusterFanControl mtrBaseClusterFanControl, IsMTRSubscribeParams params) => mtrBaseClusterFanControl -> params -> Ptr () -> Ptr () -> IO ()
subscribeAttributeSpeedSettingWithParams_subscriptionEstablished_reportHandler mtrBaseClusterFanControl params subscriptionEstablished reportHandler =
  sendMessage mtrBaseClusterFanControl subscribeAttributeSpeedSettingWithParams_subscriptionEstablished_reportHandlerSelector (toMTRSubscribeParams params) subscriptionEstablished reportHandler

-- | @+ readAttributeSpeedSettingWithClusterStateCache:endpoint:queue:completion:@
readAttributeSpeedSettingWithClusterStateCache_endpoint_queue_completion :: (IsMTRClusterStateCacheContainer clusterStateCacheContainer, IsNSNumber endpoint, IsNSObject queue) => clusterStateCacheContainer -> endpoint -> queue -> Ptr () -> IO ()
readAttributeSpeedSettingWithClusterStateCache_endpoint_queue_completion clusterStateCacheContainer endpoint queue completion =
  do
    cls' <- getRequiredClass "MTRBaseClusterFanControl"
    sendClassMessage cls' readAttributeSpeedSettingWithClusterStateCache_endpoint_queue_completionSelector (toMTRClusterStateCacheContainer clusterStateCacheContainer) (toNSNumber endpoint) (toNSObject queue) completion

-- | @- readAttributeSpeedCurrentWithCompletion:@
readAttributeSpeedCurrentWithCompletion :: IsMTRBaseClusterFanControl mtrBaseClusterFanControl => mtrBaseClusterFanControl -> Ptr () -> IO ()
readAttributeSpeedCurrentWithCompletion mtrBaseClusterFanControl completion =
  sendMessage mtrBaseClusterFanControl readAttributeSpeedCurrentWithCompletionSelector completion

-- | @- subscribeAttributeSpeedCurrentWithParams:subscriptionEstablished:reportHandler:@
subscribeAttributeSpeedCurrentWithParams_subscriptionEstablished_reportHandler :: (IsMTRBaseClusterFanControl mtrBaseClusterFanControl, IsMTRSubscribeParams params) => mtrBaseClusterFanControl -> params -> Ptr () -> Ptr () -> IO ()
subscribeAttributeSpeedCurrentWithParams_subscriptionEstablished_reportHandler mtrBaseClusterFanControl params subscriptionEstablished reportHandler =
  sendMessage mtrBaseClusterFanControl subscribeAttributeSpeedCurrentWithParams_subscriptionEstablished_reportHandlerSelector (toMTRSubscribeParams params) subscriptionEstablished reportHandler

-- | @+ readAttributeSpeedCurrentWithClusterStateCache:endpoint:queue:completion:@
readAttributeSpeedCurrentWithClusterStateCache_endpoint_queue_completion :: (IsMTRClusterStateCacheContainer clusterStateCacheContainer, IsNSNumber endpoint, IsNSObject queue) => clusterStateCacheContainer -> endpoint -> queue -> Ptr () -> IO ()
readAttributeSpeedCurrentWithClusterStateCache_endpoint_queue_completion clusterStateCacheContainer endpoint queue completion =
  do
    cls' <- getRequiredClass "MTRBaseClusterFanControl"
    sendClassMessage cls' readAttributeSpeedCurrentWithClusterStateCache_endpoint_queue_completionSelector (toMTRClusterStateCacheContainer clusterStateCacheContainer) (toNSNumber endpoint) (toNSObject queue) completion

-- | @- readAttributeRockSupportWithCompletion:@
readAttributeRockSupportWithCompletion :: IsMTRBaseClusterFanControl mtrBaseClusterFanControl => mtrBaseClusterFanControl -> Ptr () -> IO ()
readAttributeRockSupportWithCompletion mtrBaseClusterFanControl completion =
  sendMessage mtrBaseClusterFanControl readAttributeRockSupportWithCompletionSelector completion

-- | @- subscribeAttributeRockSupportWithParams:subscriptionEstablished:reportHandler:@
subscribeAttributeRockSupportWithParams_subscriptionEstablished_reportHandler :: (IsMTRBaseClusterFanControl mtrBaseClusterFanControl, IsMTRSubscribeParams params) => mtrBaseClusterFanControl -> params -> Ptr () -> Ptr () -> IO ()
subscribeAttributeRockSupportWithParams_subscriptionEstablished_reportHandler mtrBaseClusterFanControl params subscriptionEstablished reportHandler =
  sendMessage mtrBaseClusterFanControl subscribeAttributeRockSupportWithParams_subscriptionEstablished_reportHandlerSelector (toMTRSubscribeParams params) subscriptionEstablished reportHandler

-- | @+ readAttributeRockSupportWithClusterStateCache:endpoint:queue:completion:@
readAttributeRockSupportWithClusterStateCache_endpoint_queue_completion :: (IsMTRClusterStateCacheContainer clusterStateCacheContainer, IsNSNumber endpoint, IsNSObject queue) => clusterStateCacheContainer -> endpoint -> queue -> Ptr () -> IO ()
readAttributeRockSupportWithClusterStateCache_endpoint_queue_completion clusterStateCacheContainer endpoint queue completion =
  do
    cls' <- getRequiredClass "MTRBaseClusterFanControl"
    sendClassMessage cls' readAttributeRockSupportWithClusterStateCache_endpoint_queue_completionSelector (toMTRClusterStateCacheContainer clusterStateCacheContainer) (toNSNumber endpoint) (toNSObject queue) completion

-- | @- readAttributeRockSettingWithCompletion:@
readAttributeRockSettingWithCompletion :: IsMTRBaseClusterFanControl mtrBaseClusterFanControl => mtrBaseClusterFanControl -> Ptr () -> IO ()
readAttributeRockSettingWithCompletion mtrBaseClusterFanControl completion =
  sendMessage mtrBaseClusterFanControl readAttributeRockSettingWithCompletionSelector completion

-- | @- writeAttributeRockSettingWithValue:completion:@
writeAttributeRockSettingWithValue_completion :: (IsMTRBaseClusterFanControl mtrBaseClusterFanControl, IsNSNumber value) => mtrBaseClusterFanControl -> value -> Ptr () -> IO ()
writeAttributeRockSettingWithValue_completion mtrBaseClusterFanControl value completion =
  sendMessage mtrBaseClusterFanControl writeAttributeRockSettingWithValue_completionSelector (toNSNumber value) completion

-- | @- writeAttributeRockSettingWithValue:params:completion:@
writeAttributeRockSettingWithValue_params_completion :: (IsMTRBaseClusterFanControl mtrBaseClusterFanControl, IsNSNumber value, IsMTRWriteParams params) => mtrBaseClusterFanControl -> value -> params -> Ptr () -> IO ()
writeAttributeRockSettingWithValue_params_completion mtrBaseClusterFanControl value params completion =
  sendMessage mtrBaseClusterFanControl writeAttributeRockSettingWithValue_params_completionSelector (toNSNumber value) (toMTRWriteParams params) completion

-- | @- subscribeAttributeRockSettingWithParams:subscriptionEstablished:reportHandler:@
subscribeAttributeRockSettingWithParams_subscriptionEstablished_reportHandler :: (IsMTRBaseClusterFanControl mtrBaseClusterFanControl, IsMTRSubscribeParams params) => mtrBaseClusterFanControl -> params -> Ptr () -> Ptr () -> IO ()
subscribeAttributeRockSettingWithParams_subscriptionEstablished_reportHandler mtrBaseClusterFanControl params subscriptionEstablished reportHandler =
  sendMessage mtrBaseClusterFanControl subscribeAttributeRockSettingWithParams_subscriptionEstablished_reportHandlerSelector (toMTRSubscribeParams params) subscriptionEstablished reportHandler

-- | @+ readAttributeRockSettingWithClusterStateCache:endpoint:queue:completion:@
readAttributeRockSettingWithClusterStateCache_endpoint_queue_completion :: (IsMTRClusterStateCacheContainer clusterStateCacheContainer, IsNSNumber endpoint, IsNSObject queue) => clusterStateCacheContainer -> endpoint -> queue -> Ptr () -> IO ()
readAttributeRockSettingWithClusterStateCache_endpoint_queue_completion clusterStateCacheContainer endpoint queue completion =
  do
    cls' <- getRequiredClass "MTRBaseClusterFanControl"
    sendClassMessage cls' readAttributeRockSettingWithClusterStateCache_endpoint_queue_completionSelector (toMTRClusterStateCacheContainer clusterStateCacheContainer) (toNSNumber endpoint) (toNSObject queue) completion

-- | @- readAttributeWindSupportWithCompletion:@
readAttributeWindSupportWithCompletion :: IsMTRBaseClusterFanControl mtrBaseClusterFanControl => mtrBaseClusterFanControl -> Ptr () -> IO ()
readAttributeWindSupportWithCompletion mtrBaseClusterFanControl completion =
  sendMessage mtrBaseClusterFanControl readAttributeWindSupportWithCompletionSelector completion

-- | @- subscribeAttributeWindSupportWithParams:subscriptionEstablished:reportHandler:@
subscribeAttributeWindSupportWithParams_subscriptionEstablished_reportHandler :: (IsMTRBaseClusterFanControl mtrBaseClusterFanControl, IsMTRSubscribeParams params) => mtrBaseClusterFanControl -> params -> Ptr () -> Ptr () -> IO ()
subscribeAttributeWindSupportWithParams_subscriptionEstablished_reportHandler mtrBaseClusterFanControl params subscriptionEstablished reportHandler =
  sendMessage mtrBaseClusterFanControl subscribeAttributeWindSupportWithParams_subscriptionEstablished_reportHandlerSelector (toMTRSubscribeParams params) subscriptionEstablished reportHandler

-- | @+ readAttributeWindSupportWithClusterStateCache:endpoint:queue:completion:@
readAttributeWindSupportWithClusterStateCache_endpoint_queue_completion :: (IsMTRClusterStateCacheContainer clusterStateCacheContainer, IsNSNumber endpoint, IsNSObject queue) => clusterStateCacheContainer -> endpoint -> queue -> Ptr () -> IO ()
readAttributeWindSupportWithClusterStateCache_endpoint_queue_completion clusterStateCacheContainer endpoint queue completion =
  do
    cls' <- getRequiredClass "MTRBaseClusterFanControl"
    sendClassMessage cls' readAttributeWindSupportWithClusterStateCache_endpoint_queue_completionSelector (toMTRClusterStateCacheContainer clusterStateCacheContainer) (toNSNumber endpoint) (toNSObject queue) completion

-- | @- readAttributeWindSettingWithCompletion:@
readAttributeWindSettingWithCompletion :: IsMTRBaseClusterFanControl mtrBaseClusterFanControl => mtrBaseClusterFanControl -> Ptr () -> IO ()
readAttributeWindSettingWithCompletion mtrBaseClusterFanControl completion =
  sendMessage mtrBaseClusterFanControl readAttributeWindSettingWithCompletionSelector completion

-- | @- writeAttributeWindSettingWithValue:completion:@
writeAttributeWindSettingWithValue_completion :: (IsMTRBaseClusterFanControl mtrBaseClusterFanControl, IsNSNumber value) => mtrBaseClusterFanControl -> value -> Ptr () -> IO ()
writeAttributeWindSettingWithValue_completion mtrBaseClusterFanControl value completion =
  sendMessage mtrBaseClusterFanControl writeAttributeWindSettingWithValue_completionSelector (toNSNumber value) completion

-- | @- writeAttributeWindSettingWithValue:params:completion:@
writeAttributeWindSettingWithValue_params_completion :: (IsMTRBaseClusterFanControl mtrBaseClusterFanControl, IsNSNumber value, IsMTRWriteParams params) => mtrBaseClusterFanControl -> value -> params -> Ptr () -> IO ()
writeAttributeWindSettingWithValue_params_completion mtrBaseClusterFanControl value params completion =
  sendMessage mtrBaseClusterFanControl writeAttributeWindSettingWithValue_params_completionSelector (toNSNumber value) (toMTRWriteParams params) completion

-- | @- subscribeAttributeWindSettingWithParams:subscriptionEstablished:reportHandler:@
subscribeAttributeWindSettingWithParams_subscriptionEstablished_reportHandler :: (IsMTRBaseClusterFanControl mtrBaseClusterFanControl, IsMTRSubscribeParams params) => mtrBaseClusterFanControl -> params -> Ptr () -> Ptr () -> IO ()
subscribeAttributeWindSettingWithParams_subscriptionEstablished_reportHandler mtrBaseClusterFanControl params subscriptionEstablished reportHandler =
  sendMessage mtrBaseClusterFanControl subscribeAttributeWindSettingWithParams_subscriptionEstablished_reportHandlerSelector (toMTRSubscribeParams params) subscriptionEstablished reportHandler

-- | @+ readAttributeWindSettingWithClusterStateCache:endpoint:queue:completion:@
readAttributeWindSettingWithClusterStateCache_endpoint_queue_completion :: (IsMTRClusterStateCacheContainer clusterStateCacheContainer, IsNSNumber endpoint, IsNSObject queue) => clusterStateCacheContainer -> endpoint -> queue -> Ptr () -> IO ()
readAttributeWindSettingWithClusterStateCache_endpoint_queue_completion clusterStateCacheContainer endpoint queue completion =
  do
    cls' <- getRequiredClass "MTRBaseClusterFanControl"
    sendClassMessage cls' readAttributeWindSettingWithClusterStateCache_endpoint_queue_completionSelector (toMTRClusterStateCacheContainer clusterStateCacheContainer) (toNSNumber endpoint) (toNSObject queue) completion

-- | @- readAttributeAirflowDirectionWithCompletion:@
readAttributeAirflowDirectionWithCompletion :: IsMTRBaseClusterFanControl mtrBaseClusterFanControl => mtrBaseClusterFanControl -> Ptr () -> IO ()
readAttributeAirflowDirectionWithCompletion mtrBaseClusterFanControl completion =
  sendMessage mtrBaseClusterFanControl readAttributeAirflowDirectionWithCompletionSelector completion

-- | @- writeAttributeAirflowDirectionWithValue:completion:@
writeAttributeAirflowDirectionWithValue_completion :: (IsMTRBaseClusterFanControl mtrBaseClusterFanControl, IsNSNumber value) => mtrBaseClusterFanControl -> value -> Ptr () -> IO ()
writeAttributeAirflowDirectionWithValue_completion mtrBaseClusterFanControl value completion =
  sendMessage mtrBaseClusterFanControl writeAttributeAirflowDirectionWithValue_completionSelector (toNSNumber value) completion

-- | @- writeAttributeAirflowDirectionWithValue:params:completion:@
writeAttributeAirflowDirectionWithValue_params_completion :: (IsMTRBaseClusterFanControl mtrBaseClusterFanControl, IsNSNumber value, IsMTRWriteParams params) => mtrBaseClusterFanControl -> value -> params -> Ptr () -> IO ()
writeAttributeAirflowDirectionWithValue_params_completion mtrBaseClusterFanControl value params completion =
  sendMessage mtrBaseClusterFanControl writeAttributeAirflowDirectionWithValue_params_completionSelector (toNSNumber value) (toMTRWriteParams params) completion

-- | @- subscribeAttributeAirflowDirectionWithParams:subscriptionEstablished:reportHandler:@
subscribeAttributeAirflowDirectionWithParams_subscriptionEstablished_reportHandler :: (IsMTRBaseClusterFanControl mtrBaseClusterFanControl, IsMTRSubscribeParams params) => mtrBaseClusterFanControl -> params -> Ptr () -> Ptr () -> IO ()
subscribeAttributeAirflowDirectionWithParams_subscriptionEstablished_reportHandler mtrBaseClusterFanControl params subscriptionEstablished reportHandler =
  sendMessage mtrBaseClusterFanControl subscribeAttributeAirflowDirectionWithParams_subscriptionEstablished_reportHandlerSelector (toMTRSubscribeParams params) subscriptionEstablished reportHandler

-- | @+ readAttributeAirflowDirectionWithClusterStateCache:endpoint:queue:completion:@
readAttributeAirflowDirectionWithClusterStateCache_endpoint_queue_completion :: (IsMTRClusterStateCacheContainer clusterStateCacheContainer, IsNSNumber endpoint, IsNSObject queue) => clusterStateCacheContainer -> endpoint -> queue -> Ptr () -> IO ()
readAttributeAirflowDirectionWithClusterStateCache_endpoint_queue_completion clusterStateCacheContainer endpoint queue completion =
  do
    cls' <- getRequiredClass "MTRBaseClusterFanControl"
    sendClassMessage cls' readAttributeAirflowDirectionWithClusterStateCache_endpoint_queue_completionSelector (toMTRClusterStateCacheContainer clusterStateCacheContainer) (toNSNumber endpoint) (toNSObject queue) completion

-- | @- readAttributeGeneratedCommandListWithCompletion:@
readAttributeGeneratedCommandListWithCompletion :: IsMTRBaseClusterFanControl mtrBaseClusterFanControl => mtrBaseClusterFanControl -> Ptr () -> IO ()
readAttributeGeneratedCommandListWithCompletion mtrBaseClusterFanControl completion =
  sendMessage mtrBaseClusterFanControl readAttributeGeneratedCommandListWithCompletionSelector completion

-- | @- subscribeAttributeGeneratedCommandListWithParams:subscriptionEstablished:reportHandler:@
subscribeAttributeGeneratedCommandListWithParams_subscriptionEstablished_reportHandler :: (IsMTRBaseClusterFanControl mtrBaseClusterFanControl, IsMTRSubscribeParams params) => mtrBaseClusterFanControl -> params -> Ptr () -> Ptr () -> IO ()
subscribeAttributeGeneratedCommandListWithParams_subscriptionEstablished_reportHandler mtrBaseClusterFanControl params subscriptionEstablished reportHandler =
  sendMessage mtrBaseClusterFanControl subscribeAttributeGeneratedCommandListWithParams_subscriptionEstablished_reportHandlerSelector (toMTRSubscribeParams params) subscriptionEstablished reportHandler

-- | @+ readAttributeGeneratedCommandListWithClusterStateCache:endpoint:queue:completion:@
readAttributeGeneratedCommandListWithClusterStateCache_endpoint_queue_completion :: (IsMTRClusterStateCacheContainer clusterStateCacheContainer, IsNSNumber endpoint, IsNSObject queue) => clusterStateCacheContainer -> endpoint -> queue -> Ptr () -> IO ()
readAttributeGeneratedCommandListWithClusterStateCache_endpoint_queue_completion clusterStateCacheContainer endpoint queue completion =
  do
    cls' <- getRequiredClass "MTRBaseClusterFanControl"
    sendClassMessage cls' readAttributeGeneratedCommandListWithClusterStateCache_endpoint_queue_completionSelector (toMTRClusterStateCacheContainer clusterStateCacheContainer) (toNSNumber endpoint) (toNSObject queue) completion

-- | @- readAttributeAcceptedCommandListWithCompletion:@
readAttributeAcceptedCommandListWithCompletion :: IsMTRBaseClusterFanControl mtrBaseClusterFanControl => mtrBaseClusterFanControl -> Ptr () -> IO ()
readAttributeAcceptedCommandListWithCompletion mtrBaseClusterFanControl completion =
  sendMessage mtrBaseClusterFanControl readAttributeAcceptedCommandListWithCompletionSelector completion

-- | @- subscribeAttributeAcceptedCommandListWithParams:subscriptionEstablished:reportHandler:@
subscribeAttributeAcceptedCommandListWithParams_subscriptionEstablished_reportHandler :: (IsMTRBaseClusterFanControl mtrBaseClusterFanControl, IsMTRSubscribeParams params) => mtrBaseClusterFanControl -> params -> Ptr () -> Ptr () -> IO ()
subscribeAttributeAcceptedCommandListWithParams_subscriptionEstablished_reportHandler mtrBaseClusterFanControl params subscriptionEstablished reportHandler =
  sendMessage mtrBaseClusterFanControl subscribeAttributeAcceptedCommandListWithParams_subscriptionEstablished_reportHandlerSelector (toMTRSubscribeParams params) subscriptionEstablished reportHandler

-- | @+ readAttributeAcceptedCommandListWithClusterStateCache:endpoint:queue:completion:@
readAttributeAcceptedCommandListWithClusterStateCache_endpoint_queue_completion :: (IsMTRClusterStateCacheContainer clusterStateCacheContainer, IsNSNumber endpoint, IsNSObject queue) => clusterStateCacheContainer -> endpoint -> queue -> Ptr () -> IO ()
readAttributeAcceptedCommandListWithClusterStateCache_endpoint_queue_completion clusterStateCacheContainer endpoint queue completion =
  do
    cls' <- getRequiredClass "MTRBaseClusterFanControl"
    sendClassMessage cls' readAttributeAcceptedCommandListWithClusterStateCache_endpoint_queue_completionSelector (toMTRClusterStateCacheContainer clusterStateCacheContainer) (toNSNumber endpoint) (toNSObject queue) completion

-- | @- readAttributeAttributeListWithCompletion:@
readAttributeAttributeListWithCompletion :: IsMTRBaseClusterFanControl mtrBaseClusterFanControl => mtrBaseClusterFanControl -> Ptr () -> IO ()
readAttributeAttributeListWithCompletion mtrBaseClusterFanControl completion =
  sendMessage mtrBaseClusterFanControl readAttributeAttributeListWithCompletionSelector completion

-- | @- subscribeAttributeAttributeListWithParams:subscriptionEstablished:reportHandler:@
subscribeAttributeAttributeListWithParams_subscriptionEstablished_reportHandler :: (IsMTRBaseClusterFanControl mtrBaseClusterFanControl, IsMTRSubscribeParams params) => mtrBaseClusterFanControl -> params -> Ptr () -> Ptr () -> IO ()
subscribeAttributeAttributeListWithParams_subscriptionEstablished_reportHandler mtrBaseClusterFanControl params subscriptionEstablished reportHandler =
  sendMessage mtrBaseClusterFanControl subscribeAttributeAttributeListWithParams_subscriptionEstablished_reportHandlerSelector (toMTRSubscribeParams params) subscriptionEstablished reportHandler

-- | @+ readAttributeAttributeListWithClusterStateCache:endpoint:queue:completion:@
readAttributeAttributeListWithClusterStateCache_endpoint_queue_completion :: (IsMTRClusterStateCacheContainer clusterStateCacheContainer, IsNSNumber endpoint, IsNSObject queue) => clusterStateCacheContainer -> endpoint -> queue -> Ptr () -> IO ()
readAttributeAttributeListWithClusterStateCache_endpoint_queue_completion clusterStateCacheContainer endpoint queue completion =
  do
    cls' <- getRequiredClass "MTRBaseClusterFanControl"
    sendClassMessage cls' readAttributeAttributeListWithClusterStateCache_endpoint_queue_completionSelector (toMTRClusterStateCacheContainer clusterStateCacheContainer) (toNSNumber endpoint) (toNSObject queue) completion

-- | @- readAttributeFeatureMapWithCompletion:@
readAttributeFeatureMapWithCompletion :: IsMTRBaseClusterFanControl mtrBaseClusterFanControl => mtrBaseClusterFanControl -> Ptr () -> IO ()
readAttributeFeatureMapWithCompletion mtrBaseClusterFanControl completion =
  sendMessage mtrBaseClusterFanControl readAttributeFeatureMapWithCompletionSelector completion

-- | @- subscribeAttributeFeatureMapWithParams:subscriptionEstablished:reportHandler:@
subscribeAttributeFeatureMapWithParams_subscriptionEstablished_reportHandler :: (IsMTRBaseClusterFanControl mtrBaseClusterFanControl, IsMTRSubscribeParams params) => mtrBaseClusterFanControl -> params -> Ptr () -> Ptr () -> IO ()
subscribeAttributeFeatureMapWithParams_subscriptionEstablished_reportHandler mtrBaseClusterFanControl params subscriptionEstablished reportHandler =
  sendMessage mtrBaseClusterFanControl subscribeAttributeFeatureMapWithParams_subscriptionEstablished_reportHandlerSelector (toMTRSubscribeParams params) subscriptionEstablished reportHandler

-- | @+ readAttributeFeatureMapWithClusterStateCache:endpoint:queue:completion:@
readAttributeFeatureMapWithClusterStateCache_endpoint_queue_completion :: (IsMTRClusterStateCacheContainer clusterStateCacheContainer, IsNSNumber endpoint, IsNSObject queue) => clusterStateCacheContainer -> endpoint -> queue -> Ptr () -> IO ()
readAttributeFeatureMapWithClusterStateCache_endpoint_queue_completion clusterStateCacheContainer endpoint queue completion =
  do
    cls' <- getRequiredClass "MTRBaseClusterFanControl"
    sendClassMessage cls' readAttributeFeatureMapWithClusterStateCache_endpoint_queue_completionSelector (toMTRClusterStateCacheContainer clusterStateCacheContainer) (toNSNumber endpoint) (toNSObject queue) completion

-- | @- readAttributeClusterRevisionWithCompletion:@
readAttributeClusterRevisionWithCompletion :: IsMTRBaseClusterFanControl mtrBaseClusterFanControl => mtrBaseClusterFanControl -> Ptr () -> IO ()
readAttributeClusterRevisionWithCompletion mtrBaseClusterFanControl completion =
  sendMessage mtrBaseClusterFanControl readAttributeClusterRevisionWithCompletionSelector completion

-- | @- subscribeAttributeClusterRevisionWithParams:subscriptionEstablished:reportHandler:@
subscribeAttributeClusterRevisionWithParams_subscriptionEstablished_reportHandler :: (IsMTRBaseClusterFanControl mtrBaseClusterFanControl, IsMTRSubscribeParams params) => mtrBaseClusterFanControl -> params -> Ptr () -> Ptr () -> IO ()
subscribeAttributeClusterRevisionWithParams_subscriptionEstablished_reportHandler mtrBaseClusterFanControl params subscriptionEstablished reportHandler =
  sendMessage mtrBaseClusterFanControl subscribeAttributeClusterRevisionWithParams_subscriptionEstablished_reportHandlerSelector (toMTRSubscribeParams params) subscriptionEstablished reportHandler

-- | @+ readAttributeClusterRevisionWithClusterStateCache:endpoint:queue:completion:@
readAttributeClusterRevisionWithClusterStateCache_endpoint_queue_completion :: (IsMTRClusterStateCacheContainer clusterStateCacheContainer, IsNSNumber endpoint, IsNSObject queue) => clusterStateCacheContainer -> endpoint -> queue -> Ptr () -> IO ()
readAttributeClusterRevisionWithClusterStateCache_endpoint_queue_completion clusterStateCacheContainer endpoint queue completion =
  do
    cls' <- getRequiredClass "MTRBaseClusterFanControl"
    sendClassMessage cls' readAttributeClusterRevisionWithClusterStateCache_endpoint_queue_completionSelector (toMTRClusterStateCacheContainer clusterStateCacheContainer) (toNSNumber endpoint) (toNSObject queue) completion

-- | @- init@
init_ :: IsMTRBaseClusterFanControl mtrBaseClusterFanControl => mtrBaseClusterFanControl -> IO (Id MTRBaseClusterFanControl)
init_ mtrBaseClusterFanControl =
  sendOwnedMessage mtrBaseClusterFanControl initSelector

-- | @+ new@
new :: IO (Id MTRBaseClusterFanControl)
new  =
  do
    cls' <- getRequiredClass "MTRBaseClusterFanControl"
    sendOwnedClassMessage cls' newSelector

-- | @- initWithDevice:endpoint:queue:@
initWithDevice_endpoint_queue :: (IsMTRBaseClusterFanControl mtrBaseClusterFanControl, IsMTRBaseDevice device, IsNSObject queue) => mtrBaseClusterFanControl -> device -> CUShort -> queue -> IO (Id MTRBaseClusterFanControl)
initWithDevice_endpoint_queue mtrBaseClusterFanControl device endpoint queue =
  sendOwnedMessage mtrBaseClusterFanControl initWithDevice_endpoint_queueSelector (toMTRBaseDevice device) endpoint (toNSObject queue)

-- | @- readAttributeFanModeWithCompletionHandler:@
readAttributeFanModeWithCompletionHandler :: IsMTRBaseClusterFanControl mtrBaseClusterFanControl => mtrBaseClusterFanControl -> Ptr () -> IO ()
readAttributeFanModeWithCompletionHandler mtrBaseClusterFanControl completionHandler =
  sendMessage mtrBaseClusterFanControl readAttributeFanModeWithCompletionHandlerSelector completionHandler

-- | @- writeAttributeFanModeWithValue:completionHandler:@
writeAttributeFanModeWithValue_completionHandler :: (IsMTRBaseClusterFanControl mtrBaseClusterFanControl, IsNSNumber value) => mtrBaseClusterFanControl -> value -> Ptr () -> IO ()
writeAttributeFanModeWithValue_completionHandler mtrBaseClusterFanControl value completionHandler =
  sendMessage mtrBaseClusterFanControl writeAttributeFanModeWithValue_completionHandlerSelector (toNSNumber value) completionHandler

-- | @- writeAttributeFanModeWithValue:params:completionHandler:@
writeAttributeFanModeWithValue_params_completionHandler :: (IsMTRBaseClusterFanControl mtrBaseClusterFanControl, IsNSNumber value, IsMTRWriteParams params) => mtrBaseClusterFanControl -> value -> params -> Ptr () -> IO ()
writeAttributeFanModeWithValue_params_completionHandler mtrBaseClusterFanControl value params completionHandler =
  sendMessage mtrBaseClusterFanControl writeAttributeFanModeWithValue_params_completionHandlerSelector (toNSNumber value) (toMTRWriteParams params) completionHandler

-- | @- subscribeAttributeFanModeWithMinInterval:maxInterval:params:subscriptionEstablished:reportHandler:@
subscribeAttributeFanModeWithMinInterval_maxInterval_params_subscriptionEstablished_reportHandler :: (IsMTRBaseClusterFanControl mtrBaseClusterFanControl, IsNSNumber minInterval, IsNSNumber maxInterval, IsMTRSubscribeParams params) => mtrBaseClusterFanControl -> minInterval -> maxInterval -> params -> Ptr () -> Ptr () -> IO ()
subscribeAttributeFanModeWithMinInterval_maxInterval_params_subscriptionEstablished_reportHandler mtrBaseClusterFanControl minInterval maxInterval params subscriptionEstablishedHandler reportHandler =
  sendMessage mtrBaseClusterFanControl subscribeAttributeFanModeWithMinInterval_maxInterval_params_subscriptionEstablished_reportHandlerSelector (toNSNumber minInterval) (toNSNumber maxInterval) (toMTRSubscribeParams params) subscriptionEstablishedHandler reportHandler

-- | @+ readAttributeFanModeWithAttributeCache:endpoint:queue:completionHandler:@
readAttributeFanModeWithAttributeCache_endpoint_queue_completionHandler :: (IsMTRAttributeCacheContainer attributeCacheContainer, IsNSNumber endpoint, IsNSObject queue) => attributeCacheContainer -> endpoint -> queue -> Ptr () -> IO ()
readAttributeFanModeWithAttributeCache_endpoint_queue_completionHandler attributeCacheContainer endpoint queue completionHandler =
  do
    cls' <- getRequiredClass "MTRBaseClusterFanControl"
    sendClassMessage cls' readAttributeFanModeWithAttributeCache_endpoint_queue_completionHandlerSelector (toMTRAttributeCacheContainer attributeCacheContainer) (toNSNumber endpoint) (toNSObject queue) completionHandler

-- | @- readAttributeFanModeSequenceWithCompletionHandler:@
readAttributeFanModeSequenceWithCompletionHandler :: IsMTRBaseClusterFanControl mtrBaseClusterFanControl => mtrBaseClusterFanControl -> Ptr () -> IO ()
readAttributeFanModeSequenceWithCompletionHandler mtrBaseClusterFanControl completionHandler =
  sendMessage mtrBaseClusterFanControl readAttributeFanModeSequenceWithCompletionHandlerSelector completionHandler

-- | @- writeAttributeFanModeSequenceWithValue:completionHandler:@
writeAttributeFanModeSequenceWithValue_completionHandler :: (IsMTRBaseClusterFanControl mtrBaseClusterFanControl, IsNSNumber value) => mtrBaseClusterFanControl -> value -> Ptr () -> IO ()
writeAttributeFanModeSequenceWithValue_completionHandler mtrBaseClusterFanControl value completionHandler =
  sendMessage mtrBaseClusterFanControl writeAttributeFanModeSequenceWithValue_completionHandlerSelector (toNSNumber value) completionHandler

-- | @- writeAttributeFanModeSequenceWithValue:params:completionHandler:@
writeAttributeFanModeSequenceWithValue_params_completionHandler :: (IsMTRBaseClusterFanControl mtrBaseClusterFanControl, IsNSNumber value, IsMTRWriteParams params) => mtrBaseClusterFanControl -> value -> params -> Ptr () -> IO ()
writeAttributeFanModeSequenceWithValue_params_completionHandler mtrBaseClusterFanControl value params completionHandler =
  sendMessage mtrBaseClusterFanControl writeAttributeFanModeSequenceWithValue_params_completionHandlerSelector (toNSNumber value) (toMTRWriteParams params) completionHandler

-- | @- subscribeAttributeFanModeSequenceWithMinInterval:maxInterval:params:subscriptionEstablished:reportHandler:@
subscribeAttributeFanModeSequenceWithMinInterval_maxInterval_params_subscriptionEstablished_reportHandler :: (IsMTRBaseClusterFanControl mtrBaseClusterFanControl, IsNSNumber minInterval, IsNSNumber maxInterval, IsMTRSubscribeParams params) => mtrBaseClusterFanControl -> minInterval -> maxInterval -> params -> Ptr () -> Ptr () -> IO ()
subscribeAttributeFanModeSequenceWithMinInterval_maxInterval_params_subscriptionEstablished_reportHandler mtrBaseClusterFanControl minInterval maxInterval params subscriptionEstablishedHandler reportHandler =
  sendMessage mtrBaseClusterFanControl subscribeAttributeFanModeSequenceWithMinInterval_maxInterval_params_subscriptionEstablished_reportHandlerSelector (toNSNumber minInterval) (toNSNumber maxInterval) (toMTRSubscribeParams params) subscriptionEstablishedHandler reportHandler

-- | @+ readAttributeFanModeSequenceWithAttributeCache:endpoint:queue:completionHandler:@
readAttributeFanModeSequenceWithAttributeCache_endpoint_queue_completionHandler :: (IsMTRAttributeCacheContainer attributeCacheContainer, IsNSNumber endpoint, IsNSObject queue) => attributeCacheContainer -> endpoint -> queue -> Ptr () -> IO ()
readAttributeFanModeSequenceWithAttributeCache_endpoint_queue_completionHandler attributeCacheContainer endpoint queue completionHandler =
  do
    cls' <- getRequiredClass "MTRBaseClusterFanControl"
    sendClassMessage cls' readAttributeFanModeSequenceWithAttributeCache_endpoint_queue_completionHandlerSelector (toMTRAttributeCacheContainer attributeCacheContainer) (toNSNumber endpoint) (toNSObject queue) completionHandler

-- | @- readAttributePercentSettingWithCompletionHandler:@
readAttributePercentSettingWithCompletionHandler :: IsMTRBaseClusterFanControl mtrBaseClusterFanControl => mtrBaseClusterFanControl -> Ptr () -> IO ()
readAttributePercentSettingWithCompletionHandler mtrBaseClusterFanControl completionHandler =
  sendMessage mtrBaseClusterFanControl readAttributePercentSettingWithCompletionHandlerSelector completionHandler

-- | @- writeAttributePercentSettingWithValue:completionHandler:@
writeAttributePercentSettingWithValue_completionHandler :: (IsMTRBaseClusterFanControl mtrBaseClusterFanControl, IsNSNumber value) => mtrBaseClusterFanControl -> value -> Ptr () -> IO ()
writeAttributePercentSettingWithValue_completionHandler mtrBaseClusterFanControl value completionHandler =
  sendMessage mtrBaseClusterFanControl writeAttributePercentSettingWithValue_completionHandlerSelector (toNSNumber value) completionHandler

-- | @- writeAttributePercentSettingWithValue:params:completionHandler:@
writeAttributePercentSettingWithValue_params_completionHandler :: (IsMTRBaseClusterFanControl mtrBaseClusterFanControl, IsNSNumber value, IsMTRWriteParams params) => mtrBaseClusterFanControl -> value -> params -> Ptr () -> IO ()
writeAttributePercentSettingWithValue_params_completionHandler mtrBaseClusterFanControl value params completionHandler =
  sendMessage mtrBaseClusterFanControl writeAttributePercentSettingWithValue_params_completionHandlerSelector (toNSNumber value) (toMTRWriteParams params) completionHandler

-- | @- subscribeAttributePercentSettingWithMinInterval:maxInterval:params:subscriptionEstablished:reportHandler:@
subscribeAttributePercentSettingWithMinInterval_maxInterval_params_subscriptionEstablished_reportHandler :: (IsMTRBaseClusterFanControl mtrBaseClusterFanControl, IsNSNumber minInterval, IsNSNumber maxInterval, IsMTRSubscribeParams params) => mtrBaseClusterFanControl -> minInterval -> maxInterval -> params -> Ptr () -> Ptr () -> IO ()
subscribeAttributePercentSettingWithMinInterval_maxInterval_params_subscriptionEstablished_reportHandler mtrBaseClusterFanControl minInterval maxInterval params subscriptionEstablishedHandler reportHandler =
  sendMessage mtrBaseClusterFanControl subscribeAttributePercentSettingWithMinInterval_maxInterval_params_subscriptionEstablished_reportHandlerSelector (toNSNumber minInterval) (toNSNumber maxInterval) (toMTRSubscribeParams params) subscriptionEstablishedHandler reportHandler

-- | @+ readAttributePercentSettingWithAttributeCache:endpoint:queue:completionHandler:@
readAttributePercentSettingWithAttributeCache_endpoint_queue_completionHandler :: (IsMTRAttributeCacheContainer attributeCacheContainer, IsNSNumber endpoint, IsNSObject queue) => attributeCacheContainer -> endpoint -> queue -> Ptr () -> IO ()
readAttributePercentSettingWithAttributeCache_endpoint_queue_completionHandler attributeCacheContainer endpoint queue completionHandler =
  do
    cls' <- getRequiredClass "MTRBaseClusterFanControl"
    sendClassMessage cls' readAttributePercentSettingWithAttributeCache_endpoint_queue_completionHandlerSelector (toMTRAttributeCacheContainer attributeCacheContainer) (toNSNumber endpoint) (toNSObject queue) completionHandler

-- | @- readAttributePercentCurrentWithCompletionHandler:@
readAttributePercentCurrentWithCompletionHandler :: IsMTRBaseClusterFanControl mtrBaseClusterFanControl => mtrBaseClusterFanControl -> Ptr () -> IO ()
readAttributePercentCurrentWithCompletionHandler mtrBaseClusterFanControl completionHandler =
  sendMessage mtrBaseClusterFanControl readAttributePercentCurrentWithCompletionHandlerSelector completionHandler

-- | @- subscribeAttributePercentCurrentWithMinInterval:maxInterval:params:subscriptionEstablished:reportHandler:@
subscribeAttributePercentCurrentWithMinInterval_maxInterval_params_subscriptionEstablished_reportHandler :: (IsMTRBaseClusterFanControl mtrBaseClusterFanControl, IsNSNumber minInterval, IsNSNumber maxInterval, IsMTRSubscribeParams params) => mtrBaseClusterFanControl -> minInterval -> maxInterval -> params -> Ptr () -> Ptr () -> IO ()
subscribeAttributePercentCurrentWithMinInterval_maxInterval_params_subscriptionEstablished_reportHandler mtrBaseClusterFanControl minInterval maxInterval params subscriptionEstablishedHandler reportHandler =
  sendMessage mtrBaseClusterFanControl subscribeAttributePercentCurrentWithMinInterval_maxInterval_params_subscriptionEstablished_reportHandlerSelector (toNSNumber minInterval) (toNSNumber maxInterval) (toMTRSubscribeParams params) subscriptionEstablishedHandler reportHandler

-- | @+ readAttributePercentCurrentWithAttributeCache:endpoint:queue:completionHandler:@
readAttributePercentCurrentWithAttributeCache_endpoint_queue_completionHandler :: (IsMTRAttributeCacheContainer attributeCacheContainer, IsNSNumber endpoint, IsNSObject queue) => attributeCacheContainer -> endpoint -> queue -> Ptr () -> IO ()
readAttributePercentCurrentWithAttributeCache_endpoint_queue_completionHandler attributeCacheContainer endpoint queue completionHandler =
  do
    cls' <- getRequiredClass "MTRBaseClusterFanControl"
    sendClassMessage cls' readAttributePercentCurrentWithAttributeCache_endpoint_queue_completionHandlerSelector (toMTRAttributeCacheContainer attributeCacheContainer) (toNSNumber endpoint) (toNSObject queue) completionHandler

-- | @- readAttributeSpeedMaxWithCompletionHandler:@
readAttributeSpeedMaxWithCompletionHandler :: IsMTRBaseClusterFanControl mtrBaseClusterFanControl => mtrBaseClusterFanControl -> Ptr () -> IO ()
readAttributeSpeedMaxWithCompletionHandler mtrBaseClusterFanControl completionHandler =
  sendMessage mtrBaseClusterFanControl readAttributeSpeedMaxWithCompletionHandlerSelector completionHandler

-- | @- subscribeAttributeSpeedMaxWithMinInterval:maxInterval:params:subscriptionEstablished:reportHandler:@
subscribeAttributeSpeedMaxWithMinInterval_maxInterval_params_subscriptionEstablished_reportHandler :: (IsMTRBaseClusterFanControl mtrBaseClusterFanControl, IsNSNumber minInterval, IsNSNumber maxInterval, IsMTRSubscribeParams params) => mtrBaseClusterFanControl -> minInterval -> maxInterval -> params -> Ptr () -> Ptr () -> IO ()
subscribeAttributeSpeedMaxWithMinInterval_maxInterval_params_subscriptionEstablished_reportHandler mtrBaseClusterFanControl minInterval maxInterval params subscriptionEstablishedHandler reportHandler =
  sendMessage mtrBaseClusterFanControl subscribeAttributeSpeedMaxWithMinInterval_maxInterval_params_subscriptionEstablished_reportHandlerSelector (toNSNumber minInterval) (toNSNumber maxInterval) (toMTRSubscribeParams params) subscriptionEstablishedHandler reportHandler

-- | @+ readAttributeSpeedMaxWithAttributeCache:endpoint:queue:completionHandler:@
readAttributeSpeedMaxWithAttributeCache_endpoint_queue_completionHandler :: (IsMTRAttributeCacheContainer attributeCacheContainer, IsNSNumber endpoint, IsNSObject queue) => attributeCacheContainer -> endpoint -> queue -> Ptr () -> IO ()
readAttributeSpeedMaxWithAttributeCache_endpoint_queue_completionHandler attributeCacheContainer endpoint queue completionHandler =
  do
    cls' <- getRequiredClass "MTRBaseClusterFanControl"
    sendClassMessage cls' readAttributeSpeedMaxWithAttributeCache_endpoint_queue_completionHandlerSelector (toMTRAttributeCacheContainer attributeCacheContainer) (toNSNumber endpoint) (toNSObject queue) completionHandler

-- | @- readAttributeSpeedSettingWithCompletionHandler:@
readAttributeSpeedSettingWithCompletionHandler :: IsMTRBaseClusterFanControl mtrBaseClusterFanControl => mtrBaseClusterFanControl -> Ptr () -> IO ()
readAttributeSpeedSettingWithCompletionHandler mtrBaseClusterFanControl completionHandler =
  sendMessage mtrBaseClusterFanControl readAttributeSpeedSettingWithCompletionHandlerSelector completionHandler

-- | @- writeAttributeSpeedSettingWithValue:completionHandler:@
writeAttributeSpeedSettingWithValue_completionHandler :: (IsMTRBaseClusterFanControl mtrBaseClusterFanControl, IsNSNumber value) => mtrBaseClusterFanControl -> value -> Ptr () -> IO ()
writeAttributeSpeedSettingWithValue_completionHandler mtrBaseClusterFanControl value completionHandler =
  sendMessage mtrBaseClusterFanControl writeAttributeSpeedSettingWithValue_completionHandlerSelector (toNSNumber value) completionHandler

-- | @- writeAttributeSpeedSettingWithValue:params:completionHandler:@
writeAttributeSpeedSettingWithValue_params_completionHandler :: (IsMTRBaseClusterFanControl mtrBaseClusterFanControl, IsNSNumber value, IsMTRWriteParams params) => mtrBaseClusterFanControl -> value -> params -> Ptr () -> IO ()
writeAttributeSpeedSettingWithValue_params_completionHandler mtrBaseClusterFanControl value params completionHandler =
  sendMessage mtrBaseClusterFanControl writeAttributeSpeedSettingWithValue_params_completionHandlerSelector (toNSNumber value) (toMTRWriteParams params) completionHandler

-- | @- subscribeAttributeSpeedSettingWithMinInterval:maxInterval:params:subscriptionEstablished:reportHandler:@
subscribeAttributeSpeedSettingWithMinInterval_maxInterval_params_subscriptionEstablished_reportHandler :: (IsMTRBaseClusterFanControl mtrBaseClusterFanControl, IsNSNumber minInterval, IsNSNumber maxInterval, IsMTRSubscribeParams params) => mtrBaseClusterFanControl -> minInterval -> maxInterval -> params -> Ptr () -> Ptr () -> IO ()
subscribeAttributeSpeedSettingWithMinInterval_maxInterval_params_subscriptionEstablished_reportHandler mtrBaseClusterFanControl minInterval maxInterval params subscriptionEstablishedHandler reportHandler =
  sendMessage mtrBaseClusterFanControl subscribeAttributeSpeedSettingWithMinInterval_maxInterval_params_subscriptionEstablished_reportHandlerSelector (toNSNumber minInterval) (toNSNumber maxInterval) (toMTRSubscribeParams params) subscriptionEstablishedHandler reportHandler

-- | @+ readAttributeSpeedSettingWithAttributeCache:endpoint:queue:completionHandler:@
readAttributeSpeedSettingWithAttributeCache_endpoint_queue_completionHandler :: (IsMTRAttributeCacheContainer attributeCacheContainer, IsNSNumber endpoint, IsNSObject queue) => attributeCacheContainer -> endpoint -> queue -> Ptr () -> IO ()
readAttributeSpeedSettingWithAttributeCache_endpoint_queue_completionHandler attributeCacheContainer endpoint queue completionHandler =
  do
    cls' <- getRequiredClass "MTRBaseClusterFanControl"
    sendClassMessage cls' readAttributeSpeedSettingWithAttributeCache_endpoint_queue_completionHandlerSelector (toMTRAttributeCacheContainer attributeCacheContainer) (toNSNumber endpoint) (toNSObject queue) completionHandler

-- | @- readAttributeSpeedCurrentWithCompletionHandler:@
readAttributeSpeedCurrentWithCompletionHandler :: IsMTRBaseClusterFanControl mtrBaseClusterFanControl => mtrBaseClusterFanControl -> Ptr () -> IO ()
readAttributeSpeedCurrentWithCompletionHandler mtrBaseClusterFanControl completionHandler =
  sendMessage mtrBaseClusterFanControl readAttributeSpeedCurrentWithCompletionHandlerSelector completionHandler

-- | @- subscribeAttributeSpeedCurrentWithMinInterval:maxInterval:params:subscriptionEstablished:reportHandler:@
subscribeAttributeSpeedCurrentWithMinInterval_maxInterval_params_subscriptionEstablished_reportHandler :: (IsMTRBaseClusterFanControl mtrBaseClusterFanControl, IsNSNumber minInterval, IsNSNumber maxInterval, IsMTRSubscribeParams params) => mtrBaseClusterFanControl -> minInterval -> maxInterval -> params -> Ptr () -> Ptr () -> IO ()
subscribeAttributeSpeedCurrentWithMinInterval_maxInterval_params_subscriptionEstablished_reportHandler mtrBaseClusterFanControl minInterval maxInterval params subscriptionEstablishedHandler reportHandler =
  sendMessage mtrBaseClusterFanControl subscribeAttributeSpeedCurrentWithMinInterval_maxInterval_params_subscriptionEstablished_reportHandlerSelector (toNSNumber minInterval) (toNSNumber maxInterval) (toMTRSubscribeParams params) subscriptionEstablishedHandler reportHandler

-- | @+ readAttributeSpeedCurrentWithAttributeCache:endpoint:queue:completionHandler:@
readAttributeSpeedCurrentWithAttributeCache_endpoint_queue_completionHandler :: (IsMTRAttributeCacheContainer attributeCacheContainer, IsNSNumber endpoint, IsNSObject queue) => attributeCacheContainer -> endpoint -> queue -> Ptr () -> IO ()
readAttributeSpeedCurrentWithAttributeCache_endpoint_queue_completionHandler attributeCacheContainer endpoint queue completionHandler =
  do
    cls' <- getRequiredClass "MTRBaseClusterFanControl"
    sendClassMessage cls' readAttributeSpeedCurrentWithAttributeCache_endpoint_queue_completionHandlerSelector (toMTRAttributeCacheContainer attributeCacheContainer) (toNSNumber endpoint) (toNSObject queue) completionHandler

-- | @- readAttributeRockSupportWithCompletionHandler:@
readAttributeRockSupportWithCompletionHandler :: IsMTRBaseClusterFanControl mtrBaseClusterFanControl => mtrBaseClusterFanControl -> Ptr () -> IO ()
readAttributeRockSupportWithCompletionHandler mtrBaseClusterFanControl completionHandler =
  sendMessage mtrBaseClusterFanControl readAttributeRockSupportWithCompletionHandlerSelector completionHandler

-- | @- subscribeAttributeRockSupportWithMinInterval:maxInterval:params:subscriptionEstablished:reportHandler:@
subscribeAttributeRockSupportWithMinInterval_maxInterval_params_subscriptionEstablished_reportHandler :: (IsMTRBaseClusterFanControl mtrBaseClusterFanControl, IsNSNumber minInterval, IsNSNumber maxInterval, IsMTRSubscribeParams params) => mtrBaseClusterFanControl -> minInterval -> maxInterval -> params -> Ptr () -> Ptr () -> IO ()
subscribeAttributeRockSupportWithMinInterval_maxInterval_params_subscriptionEstablished_reportHandler mtrBaseClusterFanControl minInterval maxInterval params subscriptionEstablishedHandler reportHandler =
  sendMessage mtrBaseClusterFanControl subscribeAttributeRockSupportWithMinInterval_maxInterval_params_subscriptionEstablished_reportHandlerSelector (toNSNumber minInterval) (toNSNumber maxInterval) (toMTRSubscribeParams params) subscriptionEstablishedHandler reportHandler

-- | @+ readAttributeRockSupportWithAttributeCache:endpoint:queue:completionHandler:@
readAttributeRockSupportWithAttributeCache_endpoint_queue_completionHandler :: (IsMTRAttributeCacheContainer attributeCacheContainer, IsNSNumber endpoint, IsNSObject queue) => attributeCacheContainer -> endpoint -> queue -> Ptr () -> IO ()
readAttributeRockSupportWithAttributeCache_endpoint_queue_completionHandler attributeCacheContainer endpoint queue completionHandler =
  do
    cls' <- getRequiredClass "MTRBaseClusterFanControl"
    sendClassMessage cls' readAttributeRockSupportWithAttributeCache_endpoint_queue_completionHandlerSelector (toMTRAttributeCacheContainer attributeCacheContainer) (toNSNumber endpoint) (toNSObject queue) completionHandler

-- | @- readAttributeRockSettingWithCompletionHandler:@
readAttributeRockSettingWithCompletionHandler :: IsMTRBaseClusterFanControl mtrBaseClusterFanControl => mtrBaseClusterFanControl -> Ptr () -> IO ()
readAttributeRockSettingWithCompletionHandler mtrBaseClusterFanControl completionHandler =
  sendMessage mtrBaseClusterFanControl readAttributeRockSettingWithCompletionHandlerSelector completionHandler

-- | @- writeAttributeRockSettingWithValue:completionHandler:@
writeAttributeRockSettingWithValue_completionHandler :: (IsMTRBaseClusterFanControl mtrBaseClusterFanControl, IsNSNumber value) => mtrBaseClusterFanControl -> value -> Ptr () -> IO ()
writeAttributeRockSettingWithValue_completionHandler mtrBaseClusterFanControl value completionHandler =
  sendMessage mtrBaseClusterFanControl writeAttributeRockSettingWithValue_completionHandlerSelector (toNSNumber value) completionHandler

-- | @- writeAttributeRockSettingWithValue:params:completionHandler:@
writeAttributeRockSettingWithValue_params_completionHandler :: (IsMTRBaseClusterFanControl mtrBaseClusterFanControl, IsNSNumber value, IsMTRWriteParams params) => mtrBaseClusterFanControl -> value -> params -> Ptr () -> IO ()
writeAttributeRockSettingWithValue_params_completionHandler mtrBaseClusterFanControl value params completionHandler =
  sendMessage mtrBaseClusterFanControl writeAttributeRockSettingWithValue_params_completionHandlerSelector (toNSNumber value) (toMTRWriteParams params) completionHandler

-- | @- subscribeAttributeRockSettingWithMinInterval:maxInterval:params:subscriptionEstablished:reportHandler:@
subscribeAttributeRockSettingWithMinInterval_maxInterval_params_subscriptionEstablished_reportHandler :: (IsMTRBaseClusterFanControl mtrBaseClusterFanControl, IsNSNumber minInterval, IsNSNumber maxInterval, IsMTRSubscribeParams params) => mtrBaseClusterFanControl -> minInterval -> maxInterval -> params -> Ptr () -> Ptr () -> IO ()
subscribeAttributeRockSettingWithMinInterval_maxInterval_params_subscriptionEstablished_reportHandler mtrBaseClusterFanControl minInterval maxInterval params subscriptionEstablishedHandler reportHandler =
  sendMessage mtrBaseClusterFanControl subscribeAttributeRockSettingWithMinInterval_maxInterval_params_subscriptionEstablished_reportHandlerSelector (toNSNumber minInterval) (toNSNumber maxInterval) (toMTRSubscribeParams params) subscriptionEstablishedHandler reportHandler

-- | @+ readAttributeRockSettingWithAttributeCache:endpoint:queue:completionHandler:@
readAttributeRockSettingWithAttributeCache_endpoint_queue_completionHandler :: (IsMTRAttributeCacheContainer attributeCacheContainer, IsNSNumber endpoint, IsNSObject queue) => attributeCacheContainer -> endpoint -> queue -> Ptr () -> IO ()
readAttributeRockSettingWithAttributeCache_endpoint_queue_completionHandler attributeCacheContainer endpoint queue completionHandler =
  do
    cls' <- getRequiredClass "MTRBaseClusterFanControl"
    sendClassMessage cls' readAttributeRockSettingWithAttributeCache_endpoint_queue_completionHandlerSelector (toMTRAttributeCacheContainer attributeCacheContainer) (toNSNumber endpoint) (toNSObject queue) completionHandler

-- | @- readAttributeWindSupportWithCompletionHandler:@
readAttributeWindSupportWithCompletionHandler :: IsMTRBaseClusterFanControl mtrBaseClusterFanControl => mtrBaseClusterFanControl -> Ptr () -> IO ()
readAttributeWindSupportWithCompletionHandler mtrBaseClusterFanControl completionHandler =
  sendMessage mtrBaseClusterFanControl readAttributeWindSupportWithCompletionHandlerSelector completionHandler

-- | @- subscribeAttributeWindSupportWithMinInterval:maxInterval:params:subscriptionEstablished:reportHandler:@
subscribeAttributeWindSupportWithMinInterval_maxInterval_params_subscriptionEstablished_reportHandler :: (IsMTRBaseClusterFanControl mtrBaseClusterFanControl, IsNSNumber minInterval, IsNSNumber maxInterval, IsMTRSubscribeParams params) => mtrBaseClusterFanControl -> minInterval -> maxInterval -> params -> Ptr () -> Ptr () -> IO ()
subscribeAttributeWindSupportWithMinInterval_maxInterval_params_subscriptionEstablished_reportHandler mtrBaseClusterFanControl minInterval maxInterval params subscriptionEstablishedHandler reportHandler =
  sendMessage mtrBaseClusterFanControl subscribeAttributeWindSupportWithMinInterval_maxInterval_params_subscriptionEstablished_reportHandlerSelector (toNSNumber minInterval) (toNSNumber maxInterval) (toMTRSubscribeParams params) subscriptionEstablishedHandler reportHandler

-- | @+ readAttributeWindSupportWithAttributeCache:endpoint:queue:completionHandler:@
readAttributeWindSupportWithAttributeCache_endpoint_queue_completionHandler :: (IsMTRAttributeCacheContainer attributeCacheContainer, IsNSNumber endpoint, IsNSObject queue) => attributeCacheContainer -> endpoint -> queue -> Ptr () -> IO ()
readAttributeWindSupportWithAttributeCache_endpoint_queue_completionHandler attributeCacheContainer endpoint queue completionHandler =
  do
    cls' <- getRequiredClass "MTRBaseClusterFanControl"
    sendClassMessage cls' readAttributeWindSupportWithAttributeCache_endpoint_queue_completionHandlerSelector (toMTRAttributeCacheContainer attributeCacheContainer) (toNSNumber endpoint) (toNSObject queue) completionHandler

-- | @- readAttributeWindSettingWithCompletionHandler:@
readAttributeWindSettingWithCompletionHandler :: IsMTRBaseClusterFanControl mtrBaseClusterFanControl => mtrBaseClusterFanControl -> Ptr () -> IO ()
readAttributeWindSettingWithCompletionHandler mtrBaseClusterFanControl completionHandler =
  sendMessage mtrBaseClusterFanControl readAttributeWindSettingWithCompletionHandlerSelector completionHandler

-- | @- writeAttributeWindSettingWithValue:completionHandler:@
writeAttributeWindSettingWithValue_completionHandler :: (IsMTRBaseClusterFanControl mtrBaseClusterFanControl, IsNSNumber value) => mtrBaseClusterFanControl -> value -> Ptr () -> IO ()
writeAttributeWindSettingWithValue_completionHandler mtrBaseClusterFanControl value completionHandler =
  sendMessage mtrBaseClusterFanControl writeAttributeWindSettingWithValue_completionHandlerSelector (toNSNumber value) completionHandler

-- | @- writeAttributeWindSettingWithValue:params:completionHandler:@
writeAttributeWindSettingWithValue_params_completionHandler :: (IsMTRBaseClusterFanControl mtrBaseClusterFanControl, IsNSNumber value, IsMTRWriteParams params) => mtrBaseClusterFanControl -> value -> params -> Ptr () -> IO ()
writeAttributeWindSettingWithValue_params_completionHandler mtrBaseClusterFanControl value params completionHandler =
  sendMessage mtrBaseClusterFanControl writeAttributeWindSettingWithValue_params_completionHandlerSelector (toNSNumber value) (toMTRWriteParams params) completionHandler

-- | @- subscribeAttributeWindSettingWithMinInterval:maxInterval:params:subscriptionEstablished:reportHandler:@
subscribeAttributeWindSettingWithMinInterval_maxInterval_params_subscriptionEstablished_reportHandler :: (IsMTRBaseClusterFanControl mtrBaseClusterFanControl, IsNSNumber minInterval, IsNSNumber maxInterval, IsMTRSubscribeParams params) => mtrBaseClusterFanControl -> minInterval -> maxInterval -> params -> Ptr () -> Ptr () -> IO ()
subscribeAttributeWindSettingWithMinInterval_maxInterval_params_subscriptionEstablished_reportHandler mtrBaseClusterFanControl minInterval maxInterval params subscriptionEstablishedHandler reportHandler =
  sendMessage mtrBaseClusterFanControl subscribeAttributeWindSettingWithMinInterval_maxInterval_params_subscriptionEstablished_reportHandlerSelector (toNSNumber minInterval) (toNSNumber maxInterval) (toMTRSubscribeParams params) subscriptionEstablishedHandler reportHandler

-- | @+ readAttributeWindSettingWithAttributeCache:endpoint:queue:completionHandler:@
readAttributeWindSettingWithAttributeCache_endpoint_queue_completionHandler :: (IsMTRAttributeCacheContainer attributeCacheContainer, IsNSNumber endpoint, IsNSObject queue) => attributeCacheContainer -> endpoint -> queue -> Ptr () -> IO ()
readAttributeWindSettingWithAttributeCache_endpoint_queue_completionHandler attributeCacheContainer endpoint queue completionHandler =
  do
    cls' <- getRequiredClass "MTRBaseClusterFanControl"
    sendClassMessage cls' readAttributeWindSettingWithAttributeCache_endpoint_queue_completionHandlerSelector (toMTRAttributeCacheContainer attributeCacheContainer) (toNSNumber endpoint) (toNSObject queue) completionHandler

-- | @- readAttributeGeneratedCommandListWithCompletionHandler:@
readAttributeGeneratedCommandListWithCompletionHandler :: IsMTRBaseClusterFanControl mtrBaseClusterFanControl => mtrBaseClusterFanControl -> Ptr () -> IO ()
readAttributeGeneratedCommandListWithCompletionHandler mtrBaseClusterFanControl completionHandler =
  sendMessage mtrBaseClusterFanControl readAttributeGeneratedCommandListWithCompletionHandlerSelector completionHandler

-- | @- subscribeAttributeGeneratedCommandListWithMinInterval:maxInterval:params:subscriptionEstablished:reportHandler:@
subscribeAttributeGeneratedCommandListWithMinInterval_maxInterval_params_subscriptionEstablished_reportHandler :: (IsMTRBaseClusterFanControl mtrBaseClusterFanControl, IsNSNumber minInterval, IsNSNumber maxInterval, IsMTRSubscribeParams params) => mtrBaseClusterFanControl -> minInterval -> maxInterval -> params -> Ptr () -> Ptr () -> IO ()
subscribeAttributeGeneratedCommandListWithMinInterval_maxInterval_params_subscriptionEstablished_reportHandler mtrBaseClusterFanControl minInterval maxInterval params subscriptionEstablishedHandler reportHandler =
  sendMessage mtrBaseClusterFanControl subscribeAttributeGeneratedCommandListWithMinInterval_maxInterval_params_subscriptionEstablished_reportHandlerSelector (toNSNumber minInterval) (toNSNumber maxInterval) (toMTRSubscribeParams params) subscriptionEstablishedHandler reportHandler

-- | @+ readAttributeGeneratedCommandListWithAttributeCache:endpoint:queue:completionHandler:@
readAttributeGeneratedCommandListWithAttributeCache_endpoint_queue_completionHandler :: (IsMTRAttributeCacheContainer attributeCacheContainer, IsNSNumber endpoint, IsNSObject queue) => attributeCacheContainer -> endpoint -> queue -> Ptr () -> IO ()
readAttributeGeneratedCommandListWithAttributeCache_endpoint_queue_completionHandler attributeCacheContainer endpoint queue completionHandler =
  do
    cls' <- getRequiredClass "MTRBaseClusterFanControl"
    sendClassMessage cls' readAttributeGeneratedCommandListWithAttributeCache_endpoint_queue_completionHandlerSelector (toMTRAttributeCacheContainer attributeCacheContainer) (toNSNumber endpoint) (toNSObject queue) completionHandler

-- | @- readAttributeAcceptedCommandListWithCompletionHandler:@
readAttributeAcceptedCommandListWithCompletionHandler :: IsMTRBaseClusterFanControl mtrBaseClusterFanControl => mtrBaseClusterFanControl -> Ptr () -> IO ()
readAttributeAcceptedCommandListWithCompletionHandler mtrBaseClusterFanControl completionHandler =
  sendMessage mtrBaseClusterFanControl readAttributeAcceptedCommandListWithCompletionHandlerSelector completionHandler

-- | @- subscribeAttributeAcceptedCommandListWithMinInterval:maxInterval:params:subscriptionEstablished:reportHandler:@
subscribeAttributeAcceptedCommandListWithMinInterval_maxInterval_params_subscriptionEstablished_reportHandler :: (IsMTRBaseClusterFanControl mtrBaseClusterFanControl, IsNSNumber minInterval, IsNSNumber maxInterval, IsMTRSubscribeParams params) => mtrBaseClusterFanControl -> minInterval -> maxInterval -> params -> Ptr () -> Ptr () -> IO ()
subscribeAttributeAcceptedCommandListWithMinInterval_maxInterval_params_subscriptionEstablished_reportHandler mtrBaseClusterFanControl minInterval maxInterval params subscriptionEstablishedHandler reportHandler =
  sendMessage mtrBaseClusterFanControl subscribeAttributeAcceptedCommandListWithMinInterval_maxInterval_params_subscriptionEstablished_reportHandlerSelector (toNSNumber minInterval) (toNSNumber maxInterval) (toMTRSubscribeParams params) subscriptionEstablishedHandler reportHandler

-- | @+ readAttributeAcceptedCommandListWithAttributeCache:endpoint:queue:completionHandler:@
readAttributeAcceptedCommandListWithAttributeCache_endpoint_queue_completionHandler :: (IsMTRAttributeCacheContainer attributeCacheContainer, IsNSNumber endpoint, IsNSObject queue) => attributeCacheContainer -> endpoint -> queue -> Ptr () -> IO ()
readAttributeAcceptedCommandListWithAttributeCache_endpoint_queue_completionHandler attributeCacheContainer endpoint queue completionHandler =
  do
    cls' <- getRequiredClass "MTRBaseClusterFanControl"
    sendClassMessage cls' readAttributeAcceptedCommandListWithAttributeCache_endpoint_queue_completionHandlerSelector (toMTRAttributeCacheContainer attributeCacheContainer) (toNSNumber endpoint) (toNSObject queue) completionHandler

-- | @- readAttributeAttributeListWithCompletionHandler:@
readAttributeAttributeListWithCompletionHandler :: IsMTRBaseClusterFanControl mtrBaseClusterFanControl => mtrBaseClusterFanControl -> Ptr () -> IO ()
readAttributeAttributeListWithCompletionHandler mtrBaseClusterFanControl completionHandler =
  sendMessage mtrBaseClusterFanControl readAttributeAttributeListWithCompletionHandlerSelector completionHandler

-- | @- subscribeAttributeAttributeListWithMinInterval:maxInterval:params:subscriptionEstablished:reportHandler:@
subscribeAttributeAttributeListWithMinInterval_maxInterval_params_subscriptionEstablished_reportHandler :: (IsMTRBaseClusterFanControl mtrBaseClusterFanControl, IsNSNumber minInterval, IsNSNumber maxInterval, IsMTRSubscribeParams params) => mtrBaseClusterFanControl -> minInterval -> maxInterval -> params -> Ptr () -> Ptr () -> IO ()
subscribeAttributeAttributeListWithMinInterval_maxInterval_params_subscriptionEstablished_reportHandler mtrBaseClusterFanControl minInterval maxInterval params subscriptionEstablishedHandler reportHandler =
  sendMessage mtrBaseClusterFanControl subscribeAttributeAttributeListWithMinInterval_maxInterval_params_subscriptionEstablished_reportHandlerSelector (toNSNumber minInterval) (toNSNumber maxInterval) (toMTRSubscribeParams params) subscriptionEstablishedHandler reportHandler

-- | @+ readAttributeAttributeListWithAttributeCache:endpoint:queue:completionHandler:@
readAttributeAttributeListWithAttributeCache_endpoint_queue_completionHandler :: (IsMTRAttributeCacheContainer attributeCacheContainer, IsNSNumber endpoint, IsNSObject queue) => attributeCacheContainer -> endpoint -> queue -> Ptr () -> IO ()
readAttributeAttributeListWithAttributeCache_endpoint_queue_completionHandler attributeCacheContainer endpoint queue completionHandler =
  do
    cls' <- getRequiredClass "MTRBaseClusterFanControl"
    sendClassMessage cls' readAttributeAttributeListWithAttributeCache_endpoint_queue_completionHandlerSelector (toMTRAttributeCacheContainer attributeCacheContainer) (toNSNumber endpoint) (toNSObject queue) completionHandler

-- | @- readAttributeFeatureMapWithCompletionHandler:@
readAttributeFeatureMapWithCompletionHandler :: IsMTRBaseClusterFanControl mtrBaseClusterFanControl => mtrBaseClusterFanControl -> Ptr () -> IO ()
readAttributeFeatureMapWithCompletionHandler mtrBaseClusterFanControl completionHandler =
  sendMessage mtrBaseClusterFanControl readAttributeFeatureMapWithCompletionHandlerSelector completionHandler

-- | @- subscribeAttributeFeatureMapWithMinInterval:maxInterval:params:subscriptionEstablished:reportHandler:@
subscribeAttributeFeatureMapWithMinInterval_maxInterval_params_subscriptionEstablished_reportHandler :: (IsMTRBaseClusterFanControl mtrBaseClusterFanControl, IsNSNumber minInterval, IsNSNumber maxInterval, IsMTRSubscribeParams params) => mtrBaseClusterFanControl -> minInterval -> maxInterval -> params -> Ptr () -> Ptr () -> IO ()
subscribeAttributeFeatureMapWithMinInterval_maxInterval_params_subscriptionEstablished_reportHandler mtrBaseClusterFanControl minInterval maxInterval params subscriptionEstablishedHandler reportHandler =
  sendMessage mtrBaseClusterFanControl subscribeAttributeFeatureMapWithMinInterval_maxInterval_params_subscriptionEstablished_reportHandlerSelector (toNSNumber minInterval) (toNSNumber maxInterval) (toMTRSubscribeParams params) subscriptionEstablishedHandler reportHandler

-- | @+ readAttributeFeatureMapWithAttributeCache:endpoint:queue:completionHandler:@
readAttributeFeatureMapWithAttributeCache_endpoint_queue_completionHandler :: (IsMTRAttributeCacheContainer attributeCacheContainer, IsNSNumber endpoint, IsNSObject queue) => attributeCacheContainer -> endpoint -> queue -> Ptr () -> IO ()
readAttributeFeatureMapWithAttributeCache_endpoint_queue_completionHandler attributeCacheContainer endpoint queue completionHandler =
  do
    cls' <- getRequiredClass "MTRBaseClusterFanControl"
    sendClassMessage cls' readAttributeFeatureMapWithAttributeCache_endpoint_queue_completionHandlerSelector (toMTRAttributeCacheContainer attributeCacheContainer) (toNSNumber endpoint) (toNSObject queue) completionHandler

-- | @- readAttributeClusterRevisionWithCompletionHandler:@
readAttributeClusterRevisionWithCompletionHandler :: IsMTRBaseClusterFanControl mtrBaseClusterFanControl => mtrBaseClusterFanControl -> Ptr () -> IO ()
readAttributeClusterRevisionWithCompletionHandler mtrBaseClusterFanControl completionHandler =
  sendMessage mtrBaseClusterFanControl readAttributeClusterRevisionWithCompletionHandlerSelector completionHandler

-- | @- subscribeAttributeClusterRevisionWithMinInterval:maxInterval:params:subscriptionEstablished:reportHandler:@
subscribeAttributeClusterRevisionWithMinInterval_maxInterval_params_subscriptionEstablished_reportHandler :: (IsMTRBaseClusterFanControl mtrBaseClusterFanControl, IsNSNumber minInterval, IsNSNumber maxInterval, IsMTRSubscribeParams params) => mtrBaseClusterFanControl -> minInterval -> maxInterval -> params -> Ptr () -> Ptr () -> IO ()
subscribeAttributeClusterRevisionWithMinInterval_maxInterval_params_subscriptionEstablished_reportHandler mtrBaseClusterFanControl minInterval maxInterval params subscriptionEstablishedHandler reportHandler =
  sendMessage mtrBaseClusterFanControl subscribeAttributeClusterRevisionWithMinInterval_maxInterval_params_subscriptionEstablished_reportHandlerSelector (toNSNumber minInterval) (toNSNumber maxInterval) (toMTRSubscribeParams params) subscriptionEstablishedHandler reportHandler

-- | @+ readAttributeClusterRevisionWithAttributeCache:endpoint:queue:completionHandler:@
readAttributeClusterRevisionWithAttributeCache_endpoint_queue_completionHandler :: (IsMTRAttributeCacheContainer attributeCacheContainer, IsNSNumber endpoint, IsNSObject queue) => attributeCacheContainer -> endpoint -> queue -> Ptr () -> IO ()
readAttributeClusterRevisionWithAttributeCache_endpoint_queue_completionHandler attributeCacheContainer endpoint queue completionHandler =
  do
    cls' <- getRequiredClass "MTRBaseClusterFanControl"
    sendClassMessage cls' readAttributeClusterRevisionWithAttributeCache_endpoint_queue_completionHandlerSelector (toMTRAttributeCacheContainer attributeCacheContainer) (toNSNumber endpoint) (toNSObject queue) completionHandler

-- | For all instance methods (reads, writes, commands) that take a completion, the completion will be called on the provided queue.
--
-- ObjC selector: @- initWithDevice:endpointID:queue:@
initWithDevice_endpointID_queue :: (IsMTRBaseClusterFanControl mtrBaseClusterFanControl, IsMTRBaseDevice device, IsNSNumber endpointID, IsNSObject queue) => mtrBaseClusterFanControl -> device -> endpointID -> queue -> IO (Id MTRBaseClusterFanControl)
initWithDevice_endpointID_queue mtrBaseClusterFanControl device endpointID queue =
  sendOwnedMessage mtrBaseClusterFanControl initWithDevice_endpointID_queueSelector (toMTRBaseDevice device) (toNSNumber endpointID) (toNSObject queue)

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @stepWithParams:completion:@
stepWithParams_completionSelector :: Selector '[Id MTRFanControlClusterStepParams, Ptr ()] ()
stepWithParams_completionSelector = mkSelector "stepWithParams:completion:"

-- | @Selector@ for @readAttributeFanModeWithCompletion:@
readAttributeFanModeWithCompletionSelector :: Selector '[Ptr ()] ()
readAttributeFanModeWithCompletionSelector = mkSelector "readAttributeFanModeWithCompletion:"

-- | @Selector@ for @writeAttributeFanModeWithValue:completion:@
writeAttributeFanModeWithValue_completionSelector :: Selector '[Id NSNumber, Ptr ()] ()
writeAttributeFanModeWithValue_completionSelector = mkSelector "writeAttributeFanModeWithValue:completion:"

-- | @Selector@ for @writeAttributeFanModeWithValue:params:completion:@
writeAttributeFanModeWithValue_params_completionSelector :: Selector '[Id NSNumber, Id MTRWriteParams, Ptr ()] ()
writeAttributeFanModeWithValue_params_completionSelector = mkSelector "writeAttributeFanModeWithValue:params:completion:"

-- | @Selector@ for @subscribeAttributeFanModeWithParams:subscriptionEstablished:reportHandler:@
subscribeAttributeFanModeWithParams_subscriptionEstablished_reportHandlerSelector :: Selector '[Id MTRSubscribeParams, Ptr (), Ptr ()] ()
subscribeAttributeFanModeWithParams_subscriptionEstablished_reportHandlerSelector = mkSelector "subscribeAttributeFanModeWithParams:subscriptionEstablished:reportHandler:"

-- | @Selector@ for @readAttributeFanModeWithClusterStateCache:endpoint:queue:completion:@
readAttributeFanModeWithClusterStateCache_endpoint_queue_completionSelector :: Selector '[Id MTRClusterStateCacheContainer, Id NSNumber, Id NSObject, Ptr ()] ()
readAttributeFanModeWithClusterStateCache_endpoint_queue_completionSelector = mkSelector "readAttributeFanModeWithClusterStateCache:endpoint:queue:completion:"

-- | @Selector@ for @readAttributeFanModeSequenceWithCompletion:@
readAttributeFanModeSequenceWithCompletionSelector :: Selector '[Ptr ()] ()
readAttributeFanModeSequenceWithCompletionSelector = mkSelector "readAttributeFanModeSequenceWithCompletion:"

-- | @Selector@ for @writeAttributeFanModeSequenceWithValue:completion:@
writeAttributeFanModeSequenceWithValue_completionSelector :: Selector '[Id NSNumber, Ptr ()] ()
writeAttributeFanModeSequenceWithValue_completionSelector = mkSelector "writeAttributeFanModeSequenceWithValue:completion:"

-- | @Selector@ for @writeAttributeFanModeSequenceWithValue:params:completion:@
writeAttributeFanModeSequenceWithValue_params_completionSelector :: Selector '[Id NSNumber, Id MTRWriteParams, Ptr ()] ()
writeAttributeFanModeSequenceWithValue_params_completionSelector = mkSelector "writeAttributeFanModeSequenceWithValue:params:completion:"

-- | @Selector@ for @subscribeAttributeFanModeSequenceWithParams:subscriptionEstablished:reportHandler:@
subscribeAttributeFanModeSequenceWithParams_subscriptionEstablished_reportHandlerSelector :: Selector '[Id MTRSubscribeParams, Ptr (), Ptr ()] ()
subscribeAttributeFanModeSequenceWithParams_subscriptionEstablished_reportHandlerSelector = mkSelector "subscribeAttributeFanModeSequenceWithParams:subscriptionEstablished:reportHandler:"

-- | @Selector@ for @readAttributeFanModeSequenceWithClusterStateCache:endpoint:queue:completion:@
readAttributeFanModeSequenceWithClusterStateCache_endpoint_queue_completionSelector :: Selector '[Id MTRClusterStateCacheContainer, Id NSNumber, Id NSObject, Ptr ()] ()
readAttributeFanModeSequenceWithClusterStateCache_endpoint_queue_completionSelector = mkSelector "readAttributeFanModeSequenceWithClusterStateCache:endpoint:queue:completion:"

-- | @Selector@ for @readAttributePercentSettingWithCompletion:@
readAttributePercentSettingWithCompletionSelector :: Selector '[Ptr ()] ()
readAttributePercentSettingWithCompletionSelector = mkSelector "readAttributePercentSettingWithCompletion:"

-- | @Selector@ for @writeAttributePercentSettingWithValue:completion:@
writeAttributePercentSettingWithValue_completionSelector :: Selector '[Id NSNumber, Ptr ()] ()
writeAttributePercentSettingWithValue_completionSelector = mkSelector "writeAttributePercentSettingWithValue:completion:"

-- | @Selector@ for @writeAttributePercentSettingWithValue:params:completion:@
writeAttributePercentSettingWithValue_params_completionSelector :: Selector '[Id NSNumber, Id MTRWriteParams, Ptr ()] ()
writeAttributePercentSettingWithValue_params_completionSelector = mkSelector "writeAttributePercentSettingWithValue:params:completion:"

-- | @Selector@ for @subscribeAttributePercentSettingWithParams:subscriptionEstablished:reportHandler:@
subscribeAttributePercentSettingWithParams_subscriptionEstablished_reportHandlerSelector :: Selector '[Id MTRSubscribeParams, Ptr (), Ptr ()] ()
subscribeAttributePercentSettingWithParams_subscriptionEstablished_reportHandlerSelector = mkSelector "subscribeAttributePercentSettingWithParams:subscriptionEstablished:reportHandler:"

-- | @Selector@ for @readAttributePercentSettingWithClusterStateCache:endpoint:queue:completion:@
readAttributePercentSettingWithClusterStateCache_endpoint_queue_completionSelector :: Selector '[Id MTRClusterStateCacheContainer, Id NSNumber, Id NSObject, Ptr ()] ()
readAttributePercentSettingWithClusterStateCache_endpoint_queue_completionSelector = mkSelector "readAttributePercentSettingWithClusterStateCache:endpoint:queue:completion:"

-- | @Selector@ for @readAttributePercentCurrentWithCompletion:@
readAttributePercentCurrentWithCompletionSelector :: Selector '[Ptr ()] ()
readAttributePercentCurrentWithCompletionSelector = mkSelector "readAttributePercentCurrentWithCompletion:"

-- | @Selector@ for @subscribeAttributePercentCurrentWithParams:subscriptionEstablished:reportHandler:@
subscribeAttributePercentCurrentWithParams_subscriptionEstablished_reportHandlerSelector :: Selector '[Id MTRSubscribeParams, Ptr (), Ptr ()] ()
subscribeAttributePercentCurrentWithParams_subscriptionEstablished_reportHandlerSelector = mkSelector "subscribeAttributePercentCurrentWithParams:subscriptionEstablished:reportHandler:"

-- | @Selector@ for @readAttributePercentCurrentWithClusterStateCache:endpoint:queue:completion:@
readAttributePercentCurrentWithClusterStateCache_endpoint_queue_completionSelector :: Selector '[Id MTRClusterStateCacheContainer, Id NSNumber, Id NSObject, Ptr ()] ()
readAttributePercentCurrentWithClusterStateCache_endpoint_queue_completionSelector = mkSelector "readAttributePercentCurrentWithClusterStateCache:endpoint:queue:completion:"

-- | @Selector@ for @readAttributeSpeedMaxWithCompletion:@
readAttributeSpeedMaxWithCompletionSelector :: Selector '[Ptr ()] ()
readAttributeSpeedMaxWithCompletionSelector = mkSelector "readAttributeSpeedMaxWithCompletion:"

-- | @Selector@ for @subscribeAttributeSpeedMaxWithParams:subscriptionEstablished:reportHandler:@
subscribeAttributeSpeedMaxWithParams_subscriptionEstablished_reportHandlerSelector :: Selector '[Id MTRSubscribeParams, Ptr (), Ptr ()] ()
subscribeAttributeSpeedMaxWithParams_subscriptionEstablished_reportHandlerSelector = mkSelector "subscribeAttributeSpeedMaxWithParams:subscriptionEstablished:reportHandler:"

-- | @Selector@ for @readAttributeSpeedMaxWithClusterStateCache:endpoint:queue:completion:@
readAttributeSpeedMaxWithClusterStateCache_endpoint_queue_completionSelector :: Selector '[Id MTRClusterStateCacheContainer, Id NSNumber, Id NSObject, Ptr ()] ()
readAttributeSpeedMaxWithClusterStateCache_endpoint_queue_completionSelector = mkSelector "readAttributeSpeedMaxWithClusterStateCache:endpoint:queue:completion:"

-- | @Selector@ for @readAttributeSpeedSettingWithCompletion:@
readAttributeSpeedSettingWithCompletionSelector :: Selector '[Ptr ()] ()
readAttributeSpeedSettingWithCompletionSelector = mkSelector "readAttributeSpeedSettingWithCompletion:"

-- | @Selector@ for @writeAttributeSpeedSettingWithValue:completion:@
writeAttributeSpeedSettingWithValue_completionSelector :: Selector '[Id NSNumber, Ptr ()] ()
writeAttributeSpeedSettingWithValue_completionSelector = mkSelector "writeAttributeSpeedSettingWithValue:completion:"

-- | @Selector@ for @writeAttributeSpeedSettingWithValue:params:completion:@
writeAttributeSpeedSettingWithValue_params_completionSelector :: Selector '[Id NSNumber, Id MTRWriteParams, Ptr ()] ()
writeAttributeSpeedSettingWithValue_params_completionSelector = mkSelector "writeAttributeSpeedSettingWithValue:params:completion:"

-- | @Selector@ for @subscribeAttributeSpeedSettingWithParams:subscriptionEstablished:reportHandler:@
subscribeAttributeSpeedSettingWithParams_subscriptionEstablished_reportHandlerSelector :: Selector '[Id MTRSubscribeParams, Ptr (), Ptr ()] ()
subscribeAttributeSpeedSettingWithParams_subscriptionEstablished_reportHandlerSelector = mkSelector "subscribeAttributeSpeedSettingWithParams:subscriptionEstablished:reportHandler:"

-- | @Selector@ for @readAttributeSpeedSettingWithClusterStateCache:endpoint:queue:completion:@
readAttributeSpeedSettingWithClusterStateCache_endpoint_queue_completionSelector :: Selector '[Id MTRClusterStateCacheContainer, Id NSNumber, Id NSObject, Ptr ()] ()
readAttributeSpeedSettingWithClusterStateCache_endpoint_queue_completionSelector = mkSelector "readAttributeSpeedSettingWithClusterStateCache:endpoint:queue:completion:"

-- | @Selector@ for @readAttributeSpeedCurrentWithCompletion:@
readAttributeSpeedCurrentWithCompletionSelector :: Selector '[Ptr ()] ()
readAttributeSpeedCurrentWithCompletionSelector = mkSelector "readAttributeSpeedCurrentWithCompletion:"

-- | @Selector@ for @subscribeAttributeSpeedCurrentWithParams:subscriptionEstablished:reportHandler:@
subscribeAttributeSpeedCurrentWithParams_subscriptionEstablished_reportHandlerSelector :: Selector '[Id MTRSubscribeParams, Ptr (), Ptr ()] ()
subscribeAttributeSpeedCurrentWithParams_subscriptionEstablished_reportHandlerSelector = mkSelector "subscribeAttributeSpeedCurrentWithParams:subscriptionEstablished:reportHandler:"

-- | @Selector@ for @readAttributeSpeedCurrentWithClusterStateCache:endpoint:queue:completion:@
readAttributeSpeedCurrentWithClusterStateCache_endpoint_queue_completionSelector :: Selector '[Id MTRClusterStateCacheContainer, Id NSNumber, Id NSObject, Ptr ()] ()
readAttributeSpeedCurrentWithClusterStateCache_endpoint_queue_completionSelector = mkSelector "readAttributeSpeedCurrentWithClusterStateCache:endpoint:queue:completion:"

-- | @Selector@ for @readAttributeRockSupportWithCompletion:@
readAttributeRockSupportWithCompletionSelector :: Selector '[Ptr ()] ()
readAttributeRockSupportWithCompletionSelector = mkSelector "readAttributeRockSupportWithCompletion:"

-- | @Selector@ for @subscribeAttributeRockSupportWithParams:subscriptionEstablished:reportHandler:@
subscribeAttributeRockSupportWithParams_subscriptionEstablished_reportHandlerSelector :: Selector '[Id MTRSubscribeParams, Ptr (), Ptr ()] ()
subscribeAttributeRockSupportWithParams_subscriptionEstablished_reportHandlerSelector = mkSelector "subscribeAttributeRockSupportWithParams:subscriptionEstablished:reportHandler:"

-- | @Selector@ for @readAttributeRockSupportWithClusterStateCache:endpoint:queue:completion:@
readAttributeRockSupportWithClusterStateCache_endpoint_queue_completionSelector :: Selector '[Id MTRClusterStateCacheContainer, Id NSNumber, Id NSObject, Ptr ()] ()
readAttributeRockSupportWithClusterStateCache_endpoint_queue_completionSelector = mkSelector "readAttributeRockSupportWithClusterStateCache:endpoint:queue:completion:"

-- | @Selector@ for @readAttributeRockSettingWithCompletion:@
readAttributeRockSettingWithCompletionSelector :: Selector '[Ptr ()] ()
readAttributeRockSettingWithCompletionSelector = mkSelector "readAttributeRockSettingWithCompletion:"

-- | @Selector@ for @writeAttributeRockSettingWithValue:completion:@
writeAttributeRockSettingWithValue_completionSelector :: Selector '[Id NSNumber, Ptr ()] ()
writeAttributeRockSettingWithValue_completionSelector = mkSelector "writeAttributeRockSettingWithValue:completion:"

-- | @Selector@ for @writeAttributeRockSettingWithValue:params:completion:@
writeAttributeRockSettingWithValue_params_completionSelector :: Selector '[Id NSNumber, Id MTRWriteParams, Ptr ()] ()
writeAttributeRockSettingWithValue_params_completionSelector = mkSelector "writeAttributeRockSettingWithValue:params:completion:"

-- | @Selector@ for @subscribeAttributeRockSettingWithParams:subscriptionEstablished:reportHandler:@
subscribeAttributeRockSettingWithParams_subscriptionEstablished_reportHandlerSelector :: Selector '[Id MTRSubscribeParams, Ptr (), Ptr ()] ()
subscribeAttributeRockSettingWithParams_subscriptionEstablished_reportHandlerSelector = mkSelector "subscribeAttributeRockSettingWithParams:subscriptionEstablished:reportHandler:"

-- | @Selector@ for @readAttributeRockSettingWithClusterStateCache:endpoint:queue:completion:@
readAttributeRockSettingWithClusterStateCache_endpoint_queue_completionSelector :: Selector '[Id MTRClusterStateCacheContainer, Id NSNumber, Id NSObject, Ptr ()] ()
readAttributeRockSettingWithClusterStateCache_endpoint_queue_completionSelector = mkSelector "readAttributeRockSettingWithClusterStateCache:endpoint:queue:completion:"

-- | @Selector@ for @readAttributeWindSupportWithCompletion:@
readAttributeWindSupportWithCompletionSelector :: Selector '[Ptr ()] ()
readAttributeWindSupportWithCompletionSelector = mkSelector "readAttributeWindSupportWithCompletion:"

-- | @Selector@ for @subscribeAttributeWindSupportWithParams:subscriptionEstablished:reportHandler:@
subscribeAttributeWindSupportWithParams_subscriptionEstablished_reportHandlerSelector :: Selector '[Id MTRSubscribeParams, Ptr (), Ptr ()] ()
subscribeAttributeWindSupportWithParams_subscriptionEstablished_reportHandlerSelector = mkSelector "subscribeAttributeWindSupportWithParams:subscriptionEstablished:reportHandler:"

-- | @Selector@ for @readAttributeWindSupportWithClusterStateCache:endpoint:queue:completion:@
readAttributeWindSupportWithClusterStateCache_endpoint_queue_completionSelector :: Selector '[Id MTRClusterStateCacheContainer, Id NSNumber, Id NSObject, Ptr ()] ()
readAttributeWindSupportWithClusterStateCache_endpoint_queue_completionSelector = mkSelector "readAttributeWindSupportWithClusterStateCache:endpoint:queue:completion:"

-- | @Selector@ for @readAttributeWindSettingWithCompletion:@
readAttributeWindSettingWithCompletionSelector :: Selector '[Ptr ()] ()
readAttributeWindSettingWithCompletionSelector = mkSelector "readAttributeWindSettingWithCompletion:"

-- | @Selector@ for @writeAttributeWindSettingWithValue:completion:@
writeAttributeWindSettingWithValue_completionSelector :: Selector '[Id NSNumber, Ptr ()] ()
writeAttributeWindSettingWithValue_completionSelector = mkSelector "writeAttributeWindSettingWithValue:completion:"

-- | @Selector@ for @writeAttributeWindSettingWithValue:params:completion:@
writeAttributeWindSettingWithValue_params_completionSelector :: Selector '[Id NSNumber, Id MTRWriteParams, Ptr ()] ()
writeAttributeWindSettingWithValue_params_completionSelector = mkSelector "writeAttributeWindSettingWithValue:params:completion:"

-- | @Selector@ for @subscribeAttributeWindSettingWithParams:subscriptionEstablished:reportHandler:@
subscribeAttributeWindSettingWithParams_subscriptionEstablished_reportHandlerSelector :: Selector '[Id MTRSubscribeParams, Ptr (), Ptr ()] ()
subscribeAttributeWindSettingWithParams_subscriptionEstablished_reportHandlerSelector = mkSelector "subscribeAttributeWindSettingWithParams:subscriptionEstablished:reportHandler:"

-- | @Selector@ for @readAttributeWindSettingWithClusterStateCache:endpoint:queue:completion:@
readAttributeWindSettingWithClusterStateCache_endpoint_queue_completionSelector :: Selector '[Id MTRClusterStateCacheContainer, Id NSNumber, Id NSObject, Ptr ()] ()
readAttributeWindSettingWithClusterStateCache_endpoint_queue_completionSelector = mkSelector "readAttributeWindSettingWithClusterStateCache:endpoint:queue:completion:"

-- | @Selector@ for @readAttributeAirflowDirectionWithCompletion:@
readAttributeAirflowDirectionWithCompletionSelector :: Selector '[Ptr ()] ()
readAttributeAirflowDirectionWithCompletionSelector = mkSelector "readAttributeAirflowDirectionWithCompletion:"

-- | @Selector@ for @writeAttributeAirflowDirectionWithValue:completion:@
writeAttributeAirflowDirectionWithValue_completionSelector :: Selector '[Id NSNumber, Ptr ()] ()
writeAttributeAirflowDirectionWithValue_completionSelector = mkSelector "writeAttributeAirflowDirectionWithValue:completion:"

-- | @Selector@ for @writeAttributeAirflowDirectionWithValue:params:completion:@
writeAttributeAirflowDirectionWithValue_params_completionSelector :: Selector '[Id NSNumber, Id MTRWriteParams, Ptr ()] ()
writeAttributeAirflowDirectionWithValue_params_completionSelector = mkSelector "writeAttributeAirflowDirectionWithValue:params:completion:"

-- | @Selector@ for @subscribeAttributeAirflowDirectionWithParams:subscriptionEstablished:reportHandler:@
subscribeAttributeAirflowDirectionWithParams_subscriptionEstablished_reportHandlerSelector :: Selector '[Id MTRSubscribeParams, Ptr (), Ptr ()] ()
subscribeAttributeAirflowDirectionWithParams_subscriptionEstablished_reportHandlerSelector = mkSelector "subscribeAttributeAirflowDirectionWithParams:subscriptionEstablished:reportHandler:"

-- | @Selector@ for @readAttributeAirflowDirectionWithClusterStateCache:endpoint:queue:completion:@
readAttributeAirflowDirectionWithClusterStateCache_endpoint_queue_completionSelector :: Selector '[Id MTRClusterStateCacheContainer, Id NSNumber, Id NSObject, Ptr ()] ()
readAttributeAirflowDirectionWithClusterStateCache_endpoint_queue_completionSelector = mkSelector "readAttributeAirflowDirectionWithClusterStateCache:endpoint:queue:completion:"

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
initSelector :: Selector '[] (Id MTRBaseClusterFanControl)
initSelector = mkSelector "init"

-- | @Selector@ for @new@
newSelector :: Selector '[] (Id MTRBaseClusterFanControl)
newSelector = mkSelector "new"

-- | @Selector@ for @initWithDevice:endpoint:queue:@
initWithDevice_endpoint_queueSelector :: Selector '[Id MTRBaseDevice, CUShort, Id NSObject] (Id MTRBaseClusterFanControl)
initWithDevice_endpoint_queueSelector = mkSelector "initWithDevice:endpoint:queue:"

-- | @Selector@ for @readAttributeFanModeWithCompletionHandler:@
readAttributeFanModeWithCompletionHandlerSelector :: Selector '[Ptr ()] ()
readAttributeFanModeWithCompletionHandlerSelector = mkSelector "readAttributeFanModeWithCompletionHandler:"

-- | @Selector@ for @writeAttributeFanModeWithValue:completionHandler:@
writeAttributeFanModeWithValue_completionHandlerSelector :: Selector '[Id NSNumber, Ptr ()] ()
writeAttributeFanModeWithValue_completionHandlerSelector = mkSelector "writeAttributeFanModeWithValue:completionHandler:"

-- | @Selector@ for @writeAttributeFanModeWithValue:params:completionHandler:@
writeAttributeFanModeWithValue_params_completionHandlerSelector :: Selector '[Id NSNumber, Id MTRWriteParams, Ptr ()] ()
writeAttributeFanModeWithValue_params_completionHandlerSelector = mkSelector "writeAttributeFanModeWithValue:params:completionHandler:"

-- | @Selector@ for @subscribeAttributeFanModeWithMinInterval:maxInterval:params:subscriptionEstablished:reportHandler:@
subscribeAttributeFanModeWithMinInterval_maxInterval_params_subscriptionEstablished_reportHandlerSelector :: Selector '[Id NSNumber, Id NSNumber, Id MTRSubscribeParams, Ptr (), Ptr ()] ()
subscribeAttributeFanModeWithMinInterval_maxInterval_params_subscriptionEstablished_reportHandlerSelector = mkSelector "subscribeAttributeFanModeWithMinInterval:maxInterval:params:subscriptionEstablished:reportHandler:"

-- | @Selector@ for @readAttributeFanModeWithAttributeCache:endpoint:queue:completionHandler:@
readAttributeFanModeWithAttributeCache_endpoint_queue_completionHandlerSelector :: Selector '[Id MTRAttributeCacheContainer, Id NSNumber, Id NSObject, Ptr ()] ()
readAttributeFanModeWithAttributeCache_endpoint_queue_completionHandlerSelector = mkSelector "readAttributeFanModeWithAttributeCache:endpoint:queue:completionHandler:"

-- | @Selector@ for @readAttributeFanModeSequenceWithCompletionHandler:@
readAttributeFanModeSequenceWithCompletionHandlerSelector :: Selector '[Ptr ()] ()
readAttributeFanModeSequenceWithCompletionHandlerSelector = mkSelector "readAttributeFanModeSequenceWithCompletionHandler:"

-- | @Selector@ for @writeAttributeFanModeSequenceWithValue:completionHandler:@
writeAttributeFanModeSequenceWithValue_completionHandlerSelector :: Selector '[Id NSNumber, Ptr ()] ()
writeAttributeFanModeSequenceWithValue_completionHandlerSelector = mkSelector "writeAttributeFanModeSequenceWithValue:completionHandler:"

-- | @Selector@ for @writeAttributeFanModeSequenceWithValue:params:completionHandler:@
writeAttributeFanModeSequenceWithValue_params_completionHandlerSelector :: Selector '[Id NSNumber, Id MTRWriteParams, Ptr ()] ()
writeAttributeFanModeSequenceWithValue_params_completionHandlerSelector = mkSelector "writeAttributeFanModeSequenceWithValue:params:completionHandler:"

-- | @Selector@ for @subscribeAttributeFanModeSequenceWithMinInterval:maxInterval:params:subscriptionEstablished:reportHandler:@
subscribeAttributeFanModeSequenceWithMinInterval_maxInterval_params_subscriptionEstablished_reportHandlerSelector :: Selector '[Id NSNumber, Id NSNumber, Id MTRSubscribeParams, Ptr (), Ptr ()] ()
subscribeAttributeFanModeSequenceWithMinInterval_maxInterval_params_subscriptionEstablished_reportHandlerSelector = mkSelector "subscribeAttributeFanModeSequenceWithMinInterval:maxInterval:params:subscriptionEstablished:reportHandler:"

-- | @Selector@ for @readAttributeFanModeSequenceWithAttributeCache:endpoint:queue:completionHandler:@
readAttributeFanModeSequenceWithAttributeCache_endpoint_queue_completionHandlerSelector :: Selector '[Id MTRAttributeCacheContainer, Id NSNumber, Id NSObject, Ptr ()] ()
readAttributeFanModeSequenceWithAttributeCache_endpoint_queue_completionHandlerSelector = mkSelector "readAttributeFanModeSequenceWithAttributeCache:endpoint:queue:completionHandler:"

-- | @Selector@ for @readAttributePercentSettingWithCompletionHandler:@
readAttributePercentSettingWithCompletionHandlerSelector :: Selector '[Ptr ()] ()
readAttributePercentSettingWithCompletionHandlerSelector = mkSelector "readAttributePercentSettingWithCompletionHandler:"

-- | @Selector@ for @writeAttributePercentSettingWithValue:completionHandler:@
writeAttributePercentSettingWithValue_completionHandlerSelector :: Selector '[Id NSNumber, Ptr ()] ()
writeAttributePercentSettingWithValue_completionHandlerSelector = mkSelector "writeAttributePercentSettingWithValue:completionHandler:"

-- | @Selector@ for @writeAttributePercentSettingWithValue:params:completionHandler:@
writeAttributePercentSettingWithValue_params_completionHandlerSelector :: Selector '[Id NSNumber, Id MTRWriteParams, Ptr ()] ()
writeAttributePercentSettingWithValue_params_completionHandlerSelector = mkSelector "writeAttributePercentSettingWithValue:params:completionHandler:"

-- | @Selector@ for @subscribeAttributePercentSettingWithMinInterval:maxInterval:params:subscriptionEstablished:reportHandler:@
subscribeAttributePercentSettingWithMinInterval_maxInterval_params_subscriptionEstablished_reportHandlerSelector :: Selector '[Id NSNumber, Id NSNumber, Id MTRSubscribeParams, Ptr (), Ptr ()] ()
subscribeAttributePercentSettingWithMinInterval_maxInterval_params_subscriptionEstablished_reportHandlerSelector = mkSelector "subscribeAttributePercentSettingWithMinInterval:maxInterval:params:subscriptionEstablished:reportHandler:"

-- | @Selector@ for @readAttributePercentSettingWithAttributeCache:endpoint:queue:completionHandler:@
readAttributePercentSettingWithAttributeCache_endpoint_queue_completionHandlerSelector :: Selector '[Id MTRAttributeCacheContainer, Id NSNumber, Id NSObject, Ptr ()] ()
readAttributePercentSettingWithAttributeCache_endpoint_queue_completionHandlerSelector = mkSelector "readAttributePercentSettingWithAttributeCache:endpoint:queue:completionHandler:"

-- | @Selector@ for @readAttributePercentCurrentWithCompletionHandler:@
readAttributePercentCurrentWithCompletionHandlerSelector :: Selector '[Ptr ()] ()
readAttributePercentCurrentWithCompletionHandlerSelector = mkSelector "readAttributePercentCurrentWithCompletionHandler:"

-- | @Selector@ for @subscribeAttributePercentCurrentWithMinInterval:maxInterval:params:subscriptionEstablished:reportHandler:@
subscribeAttributePercentCurrentWithMinInterval_maxInterval_params_subscriptionEstablished_reportHandlerSelector :: Selector '[Id NSNumber, Id NSNumber, Id MTRSubscribeParams, Ptr (), Ptr ()] ()
subscribeAttributePercentCurrentWithMinInterval_maxInterval_params_subscriptionEstablished_reportHandlerSelector = mkSelector "subscribeAttributePercentCurrentWithMinInterval:maxInterval:params:subscriptionEstablished:reportHandler:"

-- | @Selector@ for @readAttributePercentCurrentWithAttributeCache:endpoint:queue:completionHandler:@
readAttributePercentCurrentWithAttributeCache_endpoint_queue_completionHandlerSelector :: Selector '[Id MTRAttributeCacheContainer, Id NSNumber, Id NSObject, Ptr ()] ()
readAttributePercentCurrentWithAttributeCache_endpoint_queue_completionHandlerSelector = mkSelector "readAttributePercentCurrentWithAttributeCache:endpoint:queue:completionHandler:"

-- | @Selector@ for @readAttributeSpeedMaxWithCompletionHandler:@
readAttributeSpeedMaxWithCompletionHandlerSelector :: Selector '[Ptr ()] ()
readAttributeSpeedMaxWithCompletionHandlerSelector = mkSelector "readAttributeSpeedMaxWithCompletionHandler:"

-- | @Selector@ for @subscribeAttributeSpeedMaxWithMinInterval:maxInterval:params:subscriptionEstablished:reportHandler:@
subscribeAttributeSpeedMaxWithMinInterval_maxInterval_params_subscriptionEstablished_reportHandlerSelector :: Selector '[Id NSNumber, Id NSNumber, Id MTRSubscribeParams, Ptr (), Ptr ()] ()
subscribeAttributeSpeedMaxWithMinInterval_maxInterval_params_subscriptionEstablished_reportHandlerSelector = mkSelector "subscribeAttributeSpeedMaxWithMinInterval:maxInterval:params:subscriptionEstablished:reportHandler:"

-- | @Selector@ for @readAttributeSpeedMaxWithAttributeCache:endpoint:queue:completionHandler:@
readAttributeSpeedMaxWithAttributeCache_endpoint_queue_completionHandlerSelector :: Selector '[Id MTRAttributeCacheContainer, Id NSNumber, Id NSObject, Ptr ()] ()
readAttributeSpeedMaxWithAttributeCache_endpoint_queue_completionHandlerSelector = mkSelector "readAttributeSpeedMaxWithAttributeCache:endpoint:queue:completionHandler:"

-- | @Selector@ for @readAttributeSpeedSettingWithCompletionHandler:@
readAttributeSpeedSettingWithCompletionHandlerSelector :: Selector '[Ptr ()] ()
readAttributeSpeedSettingWithCompletionHandlerSelector = mkSelector "readAttributeSpeedSettingWithCompletionHandler:"

-- | @Selector@ for @writeAttributeSpeedSettingWithValue:completionHandler:@
writeAttributeSpeedSettingWithValue_completionHandlerSelector :: Selector '[Id NSNumber, Ptr ()] ()
writeAttributeSpeedSettingWithValue_completionHandlerSelector = mkSelector "writeAttributeSpeedSettingWithValue:completionHandler:"

-- | @Selector@ for @writeAttributeSpeedSettingWithValue:params:completionHandler:@
writeAttributeSpeedSettingWithValue_params_completionHandlerSelector :: Selector '[Id NSNumber, Id MTRWriteParams, Ptr ()] ()
writeAttributeSpeedSettingWithValue_params_completionHandlerSelector = mkSelector "writeAttributeSpeedSettingWithValue:params:completionHandler:"

-- | @Selector@ for @subscribeAttributeSpeedSettingWithMinInterval:maxInterval:params:subscriptionEstablished:reportHandler:@
subscribeAttributeSpeedSettingWithMinInterval_maxInterval_params_subscriptionEstablished_reportHandlerSelector :: Selector '[Id NSNumber, Id NSNumber, Id MTRSubscribeParams, Ptr (), Ptr ()] ()
subscribeAttributeSpeedSettingWithMinInterval_maxInterval_params_subscriptionEstablished_reportHandlerSelector = mkSelector "subscribeAttributeSpeedSettingWithMinInterval:maxInterval:params:subscriptionEstablished:reportHandler:"

-- | @Selector@ for @readAttributeSpeedSettingWithAttributeCache:endpoint:queue:completionHandler:@
readAttributeSpeedSettingWithAttributeCache_endpoint_queue_completionHandlerSelector :: Selector '[Id MTRAttributeCacheContainer, Id NSNumber, Id NSObject, Ptr ()] ()
readAttributeSpeedSettingWithAttributeCache_endpoint_queue_completionHandlerSelector = mkSelector "readAttributeSpeedSettingWithAttributeCache:endpoint:queue:completionHandler:"

-- | @Selector@ for @readAttributeSpeedCurrentWithCompletionHandler:@
readAttributeSpeedCurrentWithCompletionHandlerSelector :: Selector '[Ptr ()] ()
readAttributeSpeedCurrentWithCompletionHandlerSelector = mkSelector "readAttributeSpeedCurrentWithCompletionHandler:"

-- | @Selector@ for @subscribeAttributeSpeedCurrentWithMinInterval:maxInterval:params:subscriptionEstablished:reportHandler:@
subscribeAttributeSpeedCurrentWithMinInterval_maxInterval_params_subscriptionEstablished_reportHandlerSelector :: Selector '[Id NSNumber, Id NSNumber, Id MTRSubscribeParams, Ptr (), Ptr ()] ()
subscribeAttributeSpeedCurrentWithMinInterval_maxInterval_params_subscriptionEstablished_reportHandlerSelector = mkSelector "subscribeAttributeSpeedCurrentWithMinInterval:maxInterval:params:subscriptionEstablished:reportHandler:"

-- | @Selector@ for @readAttributeSpeedCurrentWithAttributeCache:endpoint:queue:completionHandler:@
readAttributeSpeedCurrentWithAttributeCache_endpoint_queue_completionHandlerSelector :: Selector '[Id MTRAttributeCacheContainer, Id NSNumber, Id NSObject, Ptr ()] ()
readAttributeSpeedCurrentWithAttributeCache_endpoint_queue_completionHandlerSelector = mkSelector "readAttributeSpeedCurrentWithAttributeCache:endpoint:queue:completionHandler:"

-- | @Selector@ for @readAttributeRockSupportWithCompletionHandler:@
readAttributeRockSupportWithCompletionHandlerSelector :: Selector '[Ptr ()] ()
readAttributeRockSupportWithCompletionHandlerSelector = mkSelector "readAttributeRockSupportWithCompletionHandler:"

-- | @Selector@ for @subscribeAttributeRockSupportWithMinInterval:maxInterval:params:subscriptionEstablished:reportHandler:@
subscribeAttributeRockSupportWithMinInterval_maxInterval_params_subscriptionEstablished_reportHandlerSelector :: Selector '[Id NSNumber, Id NSNumber, Id MTRSubscribeParams, Ptr (), Ptr ()] ()
subscribeAttributeRockSupportWithMinInterval_maxInterval_params_subscriptionEstablished_reportHandlerSelector = mkSelector "subscribeAttributeRockSupportWithMinInterval:maxInterval:params:subscriptionEstablished:reportHandler:"

-- | @Selector@ for @readAttributeRockSupportWithAttributeCache:endpoint:queue:completionHandler:@
readAttributeRockSupportWithAttributeCache_endpoint_queue_completionHandlerSelector :: Selector '[Id MTRAttributeCacheContainer, Id NSNumber, Id NSObject, Ptr ()] ()
readAttributeRockSupportWithAttributeCache_endpoint_queue_completionHandlerSelector = mkSelector "readAttributeRockSupportWithAttributeCache:endpoint:queue:completionHandler:"

-- | @Selector@ for @readAttributeRockSettingWithCompletionHandler:@
readAttributeRockSettingWithCompletionHandlerSelector :: Selector '[Ptr ()] ()
readAttributeRockSettingWithCompletionHandlerSelector = mkSelector "readAttributeRockSettingWithCompletionHandler:"

-- | @Selector@ for @writeAttributeRockSettingWithValue:completionHandler:@
writeAttributeRockSettingWithValue_completionHandlerSelector :: Selector '[Id NSNumber, Ptr ()] ()
writeAttributeRockSettingWithValue_completionHandlerSelector = mkSelector "writeAttributeRockSettingWithValue:completionHandler:"

-- | @Selector@ for @writeAttributeRockSettingWithValue:params:completionHandler:@
writeAttributeRockSettingWithValue_params_completionHandlerSelector :: Selector '[Id NSNumber, Id MTRWriteParams, Ptr ()] ()
writeAttributeRockSettingWithValue_params_completionHandlerSelector = mkSelector "writeAttributeRockSettingWithValue:params:completionHandler:"

-- | @Selector@ for @subscribeAttributeRockSettingWithMinInterval:maxInterval:params:subscriptionEstablished:reportHandler:@
subscribeAttributeRockSettingWithMinInterval_maxInterval_params_subscriptionEstablished_reportHandlerSelector :: Selector '[Id NSNumber, Id NSNumber, Id MTRSubscribeParams, Ptr (), Ptr ()] ()
subscribeAttributeRockSettingWithMinInterval_maxInterval_params_subscriptionEstablished_reportHandlerSelector = mkSelector "subscribeAttributeRockSettingWithMinInterval:maxInterval:params:subscriptionEstablished:reportHandler:"

-- | @Selector@ for @readAttributeRockSettingWithAttributeCache:endpoint:queue:completionHandler:@
readAttributeRockSettingWithAttributeCache_endpoint_queue_completionHandlerSelector :: Selector '[Id MTRAttributeCacheContainer, Id NSNumber, Id NSObject, Ptr ()] ()
readAttributeRockSettingWithAttributeCache_endpoint_queue_completionHandlerSelector = mkSelector "readAttributeRockSettingWithAttributeCache:endpoint:queue:completionHandler:"

-- | @Selector@ for @readAttributeWindSupportWithCompletionHandler:@
readAttributeWindSupportWithCompletionHandlerSelector :: Selector '[Ptr ()] ()
readAttributeWindSupportWithCompletionHandlerSelector = mkSelector "readAttributeWindSupportWithCompletionHandler:"

-- | @Selector@ for @subscribeAttributeWindSupportWithMinInterval:maxInterval:params:subscriptionEstablished:reportHandler:@
subscribeAttributeWindSupportWithMinInterval_maxInterval_params_subscriptionEstablished_reportHandlerSelector :: Selector '[Id NSNumber, Id NSNumber, Id MTRSubscribeParams, Ptr (), Ptr ()] ()
subscribeAttributeWindSupportWithMinInterval_maxInterval_params_subscriptionEstablished_reportHandlerSelector = mkSelector "subscribeAttributeWindSupportWithMinInterval:maxInterval:params:subscriptionEstablished:reportHandler:"

-- | @Selector@ for @readAttributeWindSupportWithAttributeCache:endpoint:queue:completionHandler:@
readAttributeWindSupportWithAttributeCache_endpoint_queue_completionHandlerSelector :: Selector '[Id MTRAttributeCacheContainer, Id NSNumber, Id NSObject, Ptr ()] ()
readAttributeWindSupportWithAttributeCache_endpoint_queue_completionHandlerSelector = mkSelector "readAttributeWindSupportWithAttributeCache:endpoint:queue:completionHandler:"

-- | @Selector@ for @readAttributeWindSettingWithCompletionHandler:@
readAttributeWindSettingWithCompletionHandlerSelector :: Selector '[Ptr ()] ()
readAttributeWindSettingWithCompletionHandlerSelector = mkSelector "readAttributeWindSettingWithCompletionHandler:"

-- | @Selector@ for @writeAttributeWindSettingWithValue:completionHandler:@
writeAttributeWindSettingWithValue_completionHandlerSelector :: Selector '[Id NSNumber, Ptr ()] ()
writeAttributeWindSettingWithValue_completionHandlerSelector = mkSelector "writeAttributeWindSettingWithValue:completionHandler:"

-- | @Selector@ for @writeAttributeWindSettingWithValue:params:completionHandler:@
writeAttributeWindSettingWithValue_params_completionHandlerSelector :: Selector '[Id NSNumber, Id MTRWriteParams, Ptr ()] ()
writeAttributeWindSettingWithValue_params_completionHandlerSelector = mkSelector "writeAttributeWindSettingWithValue:params:completionHandler:"

-- | @Selector@ for @subscribeAttributeWindSettingWithMinInterval:maxInterval:params:subscriptionEstablished:reportHandler:@
subscribeAttributeWindSettingWithMinInterval_maxInterval_params_subscriptionEstablished_reportHandlerSelector :: Selector '[Id NSNumber, Id NSNumber, Id MTRSubscribeParams, Ptr (), Ptr ()] ()
subscribeAttributeWindSettingWithMinInterval_maxInterval_params_subscriptionEstablished_reportHandlerSelector = mkSelector "subscribeAttributeWindSettingWithMinInterval:maxInterval:params:subscriptionEstablished:reportHandler:"

-- | @Selector@ for @readAttributeWindSettingWithAttributeCache:endpoint:queue:completionHandler:@
readAttributeWindSettingWithAttributeCache_endpoint_queue_completionHandlerSelector :: Selector '[Id MTRAttributeCacheContainer, Id NSNumber, Id NSObject, Ptr ()] ()
readAttributeWindSettingWithAttributeCache_endpoint_queue_completionHandlerSelector = mkSelector "readAttributeWindSettingWithAttributeCache:endpoint:queue:completionHandler:"

-- | @Selector@ for @readAttributeGeneratedCommandListWithCompletionHandler:@
readAttributeGeneratedCommandListWithCompletionHandlerSelector :: Selector '[Ptr ()] ()
readAttributeGeneratedCommandListWithCompletionHandlerSelector = mkSelector "readAttributeGeneratedCommandListWithCompletionHandler:"

-- | @Selector@ for @subscribeAttributeGeneratedCommandListWithMinInterval:maxInterval:params:subscriptionEstablished:reportHandler:@
subscribeAttributeGeneratedCommandListWithMinInterval_maxInterval_params_subscriptionEstablished_reportHandlerSelector :: Selector '[Id NSNumber, Id NSNumber, Id MTRSubscribeParams, Ptr (), Ptr ()] ()
subscribeAttributeGeneratedCommandListWithMinInterval_maxInterval_params_subscriptionEstablished_reportHandlerSelector = mkSelector "subscribeAttributeGeneratedCommandListWithMinInterval:maxInterval:params:subscriptionEstablished:reportHandler:"

-- | @Selector@ for @readAttributeGeneratedCommandListWithAttributeCache:endpoint:queue:completionHandler:@
readAttributeGeneratedCommandListWithAttributeCache_endpoint_queue_completionHandlerSelector :: Selector '[Id MTRAttributeCacheContainer, Id NSNumber, Id NSObject, Ptr ()] ()
readAttributeGeneratedCommandListWithAttributeCache_endpoint_queue_completionHandlerSelector = mkSelector "readAttributeGeneratedCommandListWithAttributeCache:endpoint:queue:completionHandler:"

-- | @Selector@ for @readAttributeAcceptedCommandListWithCompletionHandler:@
readAttributeAcceptedCommandListWithCompletionHandlerSelector :: Selector '[Ptr ()] ()
readAttributeAcceptedCommandListWithCompletionHandlerSelector = mkSelector "readAttributeAcceptedCommandListWithCompletionHandler:"

-- | @Selector@ for @subscribeAttributeAcceptedCommandListWithMinInterval:maxInterval:params:subscriptionEstablished:reportHandler:@
subscribeAttributeAcceptedCommandListWithMinInterval_maxInterval_params_subscriptionEstablished_reportHandlerSelector :: Selector '[Id NSNumber, Id NSNumber, Id MTRSubscribeParams, Ptr (), Ptr ()] ()
subscribeAttributeAcceptedCommandListWithMinInterval_maxInterval_params_subscriptionEstablished_reportHandlerSelector = mkSelector "subscribeAttributeAcceptedCommandListWithMinInterval:maxInterval:params:subscriptionEstablished:reportHandler:"

-- | @Selector@ for @readAttributeAcceptedCommandListWithAttributeCache:endpoint:queue:completionHandler:@
readAttributeAcceptedCommandListWithAttributeCache_endpoint_queue_completionHandlerSelector :: Selector '[Id MTRAttributeCacheContainer, Id NSNumber, Id NSObject, Ptr ()] ()
readAttributeAcceptedCommandListWithAttributeCache_endpoint_queue_completionHandlerSelector = mkSelector "readAttributeAcceptedCommandListWithAttributeCache:endpoint:queue:completionHandler:"

-- | @Selector@ for @readAttributeAttributeListWithCompletionHandler:@
readAttributeAttributeListWithCompletionHandlerSelector :: Selector '[Ptr ()] ()
readAttributeAttributeListWithCompletionHandlerSelector = mkSelector "readAttributeAttributeListWithCompletionHandler:"

-- | @Selector@ for @subscribeAttributeAttributeListWithMinInterval:maxInterval:params:subscriptionEstablished:reportHandler:@
subscribeAttributeAttributeListWithMinInterval_maxInterval_params_subscriptionEstablished_reportHandlerSelector :: Selector '[Id NSNumber, Id NSNumber, Id MTRSubscribeParams, Ptr (), Ptr ()] ()
subscribeAttributeAttributeListWithMinInterval_maxInterval_params_subscriptionEstablished_reportHandlerSelector = mkSelector "subscribeAttributeAttributeListWithMinInterval:maxInterval:params:subscriptionEstablished:reportHandler:"

-- | @Selector@ for @readAttributeAttributeListWithAttributeCache:endpoint:queue:completionHandler:@
readAttributeAttributeListWithAttributeCache_endpoint_queue_completionHandlerSelector :: Selector '[Id MTRAttributeCacheContainer, Id NSNumber, Id NSObject, Ptr ()] ()
readAttributeAttributeListWithAttributeCache_endpoint_queue_completionHandlerSelector = mkSelector "readAttributeAttributeListWithAttributeCache:endpoint:queue:completionHandler:"

-- | @Selector@ for @readAttributeFeatureMapWithCompletionHandler:@
readAttributeFeatureMapWithCompletionHandlerSelector :: Selector '[Ptr ()] ()
readAttributeFeatureMapWithCompletionHandlerSelector = mkSelector "readAttributeFeatureMapWithCompletionHandler:"

-- | @Selector@ for @subscribeAttributeFeatureMapWithMinInterval:maxInterval:params:subscriptionEstablished:reportHandler:@
subscribeAttributeFeatureMapWithMinInterval_maxInterval_params_subscriptionEstablished_reportHandlerSelector :: Selector '[Id NSNumber, Id NSNumber, Id MTRSubscribeParams, Ptr (), Ptr ()] ()
subscribeAttributeFeatureMapWithMinInterval_maxInterval_params_subscriptionEstablished_reportHandlerSelector = mkSelector "subscribeAttributeFeatureMapWithMinInterval:maxInterval:params:subscriptionEstablished:reportHandler:"

-- | @Selector@ for @readAttributeFeatureMapWithAttributeCache:endpoint:queue:completionHandler:@
readAttributeFeatureMapWithAttributeCache_endpoint_queue_completionHandlerSelector :: Selector '[Id MTRAttributeCacheContainer, Id NSNumber, Id NSObject, Ptr ()] ()
readAttributeFeatureMapWithAttributeCache_endpoint_queue_completionHandlerSelector = mkSelector "readAttributeFeatureMapWithAttributeCache:endpoint:queue:completionHandler:"

-- | @Selector@ for @readAttributeClusterRevisionWithCompletionHandler:@
readAttributeClusterRevisionWithCompletionHandlerSelector :: Selector '[Ptr ()] ()
readAttributeClusterRevisionWithCompletionHandlerSelector = mkSelector "readAttributeClusterRevisionWithCompletionHandler:"

-- | @Selector@ for @subscribeAttributeClusterRevisionWithMinInterval:maxInterval:params:subscriptionEstablished:reportHandler:@
subscribeAttributeClusterRevisionWithMinInterval_maxInterval_params_subscriptionEstablished_reportHandlerSelector :: Selector '[Id NSNumber, Id NSNumber, Id MTRSubscribeParams, Ptr (), Ptr ()] ()
subscribeAttributeClusterRevisionWithMinInterval_maxInterval_params_subscriptionEstablished_reportHandlerSelector = mkSelector "subscribeAttributeClusterRevisionWithMinInterval:maxInterval:params:subscriptionEstablished:reportHandler:"

-- | @Selector@ for @readAttributeClusterRevisionWithAttributeCache:endpoint:queue:completionHandler:@
readAttributeClusterRevisionWithAttributeCache_endpoint_queue_completionHandlerSelector :: Selector '[Id MTRAttributeCacheContainer, Id NSNumber, Id NSObject, Ptr ()] ()
readAttributeClusterRevisionWithAttributeCache_endpoint_queue_completionHandlerSelector = mkSelector "readAttributeClusterRevisionWithAttributeCache:endpoint:queue:completionHandler:"

-- | @Selector@ for @initWithDevice:endpointID:queue:@
initWithDevice_endpointID_queueSelector :: Selector '[Id MTRBaseDevice, Id NSNumber, Id NSObject] (Id MTRBaseClusterFanControl)
initWithDevice_endpointID_queueSelector = mkSelector "initWithDevice:endpointID:queue:"

