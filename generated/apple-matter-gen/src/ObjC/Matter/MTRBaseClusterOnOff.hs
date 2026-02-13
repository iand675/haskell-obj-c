{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Cluster On/Off
--
-- Attributes and commands for switching devices between 'On' and 'Off' states.
--
-- Generated bindings for @MTRBaseClusterOnOff@.
module ObjC.Matter.MTRBaseClusterOnOff
  ( MTRBaseClusterOnOff
  , IsMTRBaseClusterOnOff(..)
  , offWithParams_completion
  , offWithCompletion
  , onWithParams_completion
  , onWithCompletion
  , toggleWithParams_completion
  , toggleWithCompletion
  , offWithEffectWithParams_completion
  , onWithRecallGlobalSceneWithParams_completion
  , onWithRecallGlobalSceneWithCompletion
  , onWithTimedOffWithParams_completion
  , readAttributeOnOffWithCompletion
  , subscribeAttributeOnOffWithParams_subscriptionEstablished_reportHandler
  , readAttributeOnOffWithClusterStateCache_endpoint_queue_completion
  , readAttributeGlobalSceneControlWithCompletion
  , subscribeAttributeGlobalSceneControlWithParams_subscriptionEstablished_reportHandler
  , readAttributeGlobalSceneControlWithClusterStateCache_endpoint_queue_completion
  , readAttributeOnTimeWithCompletion
  , writeAttributeOnTimeWithValue_completion
  , writeAttributeOnTimeWithValue_params_completion
  , subscribeAttributeOnTimeWithParams_subscriptionEstablished_reportHandler
  , readAttributeOnTimeWithClusterStateCache_endpoint_queue_completion
  , readAttributeOffWaitTimeWithCompletion
  , writeAttributeOffWaitTimeWithValue_completion
  , writeAttributeOffWaitTimeWithValue_params_completion
  , subscribeAttributeOffWaitTimeWithParams_subscriptionEstablished_reportHandler
  , readAttributeOffWaitTimeWithClusterStateCache_endpoint_queue_completion
  , readAttributeStartUpOnOffWithCompletion
  , writeAttributeStartUpOnOffWithValue_completion
  , writeAttributeStartUpOnOffWithValue_params_completion
  , subscribeAttributeStartUpOnOffWithParams_subscriptionEstablished_reportHandler
  , readAttributeStartUpOnOffWithClusterStateCache_endpoint_queue_completion
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
  , offWithParams_completionHandler
  , offWithCompletionHandler
  , onWithParams_completionHandler
  , onWithCompletionHandler
  , toggleWithParams_completionHandler
  , toggleWithCompletionHandler
  , offWithEffectWithParams_completionHandler
  , onWithRecallGlobalSceneWithParams_completionHandler
  , onWithRecallGlobalSceneWithCompletionHandler
  , onWithTimedOffWithParams_completionHandler
  , readAttributeOnOffWithCompletionHandler
  , subscribeAttributeOnOffWithMinInterval_maxInterval_params_subscriptionEstablished_reportHandler
  , readAttributeOnOffWithAttributeCache_endpoint_queue_completionHandler
  , readAttributeGlobalSceneControlWithCompletionHandler
  , subscribeAttributeGlobalSceneControlWithMinInterval_maxInterval_params_subscriptionEstablished_reportHandler
  , readAttributeGlobalSceneControlWithAttributeCache_endpoint_queue_completionHandler
  , readAttributeOnTimeWithCompletionHandler
  , writeAttributeOnTimeWithValue_completionHandler
  , writeAttributeOnTimeWithValue_params_completionHandler
  , subscribeAttributeOnTimeWithMinInterval_maxInterval_params_subscriptionEstablished_reportHandler
  , readAttributeOnTimeWithAttributeCache_endpoint_queue_completionHandler
  , readAttributeOffWaitTimeWithCompletionHandler
  , writeAttributeOffWaitTimeWithValue_completionHandler
  , writeAttributeOffWaitTimeWithValue_params_completionHandler
  , subscribeAttributeOffWaitTimeWithMinInterval_maxInterval_params_subscriptionEstablished_reportHandler
  , readAttributeOffWaitTimeWithAttributeCache_endpoint_queue_completionHandler
  , readAttributeStartUpOnOffWithCompletionHandler
  , writeAttributeStartUpOnOffWithValue_completionHandler
  , writeAttributeStartUpOnOffWithValue_params_completionHandler
  , subscribeAttributeStartUpOnOffWithMinInterval_maxInterval_params_subscriptionEstablished_reportHandler
  , readAttributeStartUpOnOffWithAttributeCache_endpoint_queue_completionHandler
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
  , offWithCompletionHandlerSelector
  , offWithCompletionSelector
  , offWithEffectWithParams_completionHandlerSelector
  , offWithEffectWithParams_completionSelector
  , offWithParams_completionHandlerSelector
  , offWithParams_completionSelector
  , onWithCompletionHandlerSelector
  , onWithCompletionSelector
  , onWithParams_completionHandlerSelector
  , onWithParams_completionSelector
  , onWithRecallGlobalSceneWithCompletionHandlerSelector
  , onWithRecallGlobalSceneWithCompletionSelector
  , onWithRecallGlobalSceneWithParams_completionHandlerSelector
  , onWithRecallGlobalSceneWithParams_completionSelector
  , onWithTimedOffWithParams_completionHandlerSelector
  , onWithTimedOffWithParams_completionSelector
  , readAttributeAcceptedCommandListWithAttributeCache_endpoint_queue_completionHandlerSelector
  , readAttributeAcceptedCommandListWithClusterStateCache_endpoint_queue_completionSelector
  , readAttributeAcceptedCommandListWithCompletionHandlerSelector
  , readAttributeAcceptedCommandListWithCompletionSelector
  , readAttributeAttributeListWithAttributeCache_endpoint_queue_completionHandlerSelector
  , readAttributeAttributeListWithClusterStateCache_endpoint_queue_completionSelector
  , readAttributeAttributeListWithCompletionHandlerSelector
  , readAttributeAttributeListWithCompletionSelector
  , readAttributeClusterRevisionWithAttributeCache_endpoint_queue_completionHandlerSelector
  , readAttributeClusterRevisionWithClusterStateCache_endpoint_queue_completionSelector
  , readAttributeClusterRevisionWithCompletionHandlerSelector
  , readAttributeClusterRevisionWithCompletionSelector
  , readAttributeFeatureMapWithAttributeCache_endpoint_queue_completionHandlerSelector
  , readAttributeFeatureMapWithClusterStateCache_endpoint_queue_completionSelector
  , readAttributeFeatureMapWithCompletionHandlerSelector
  , readAttributeFeatureMapWithCompletionSelector
  , readAttributeGeneratedCommandListWithAttributeCache_endpoint_queue_completionHandlerSelector
  , readAttributeGeneratedCommandListWithClusterStateCache_endpoint_queue_completionSelector
  , readAttributeGeneratedCommandListWithCompletionHandlerSelector
  , readAttributeGeneratedCommandListWithCompletionSelector
  , readAttributeGlobalSceneControlWithAttributeCache_endpoint_queue_completionHandlerSelector
  , readAttributeGlobalSceneControlWithClusterStateCache_endpoint_queue_completionSelector
  , readAttributeGlobalSceneControlWithCompletionHandlerSelector
  , readAttributeGlobalSceneControlWithCompletionSelector
  , readAttributeOffWaitTimeWithAttributeCache_endpoint_queue_completionHandlerSelector
  , readAttributeOffWaitTimeWithClusterStateCache_endpoint_queue_completionSelector
  , readAttributeOffWaitTimeWithCompletionHandlerSelector
  , readAttributeOffWaitTimeWithCompletionSelector
  , readAttributeOnOffWithAttributeCache_endpoint_queue_completionHandlerSelector
  , readAttributeOnOffWithClusterStateCache_endpoint_queue_completionSelector
  , readAttributeOnOffWithCompletionHandlerSelector
  , readAttributeOnOffWithCompletionSelector
  , readAttributeOnTimeWithAttributeCache_endpoint_queue_completionHandlerSelector
  , readAttributeOnTimeWithClusterStateCache_endpoint_queue_completionSelector
  , readAttributeOnTimeWithCompletionHandlerSelector
  , readAttributeOnTimeWithCompletionSelector
  , readAttributeStartUpOnOffWithAttributeCache_endpoint_queue_completionHandlerSelector
  , readAttributeStartUpOnOffWithClusterStateCache_endpoint_queue_completionSelector
  , readAttributeStartUpOnOffWithCompletionHandlerSelector
  , readAttributeStartUpOnOffWithCompletionSelector
  , subscribeAttributeAcceptedCommandListWithMinInterval_maxInterval_params_subscriptionEstablished_reportHandlerSelector
  , subscribeAttributeAcceptedCommandListWithParams_subscriptionEstablished_reportHandlerSelector
  , subscribeAttributeAttributeListWithMinInterval_maxInterval_params_subscriptionEstablished_reportHandlerSelector
  , subscribeAttributeAttributeListWithParams_subscriptionEstablished_reportHandlerSelector
  , subscribeAttributeClusterRevisionWithMinInterval_maxInterval_params_subscriptionEstablished_reportHandlerSelector
  , subscribeAttributeClusterRevisionWithParams_subscriptionEstablished_reportHandlerSelector
  , subscribeAttributeFeatureMapWithMinInterval_maxInterval_params_subscriptionEstablished_reportHandlerSelector
  , subscribeAttributeFeatureMapWithParams_subscriptionEstablished_reportHandlerSelector
  , subscribeAttributeGeneratedCommandListWithMinInterval_maxInterval_params_subscriptionEstablished_reportHandlerSelector
  , subscribeAttributeGeneratedCommandListWithParams_subscriptionEstablished_reportHandlerSelector
  , subscribeAttributeGlobalSceneControlWithMinInterval_maxInterval_params_subscriptionEstablished_reportHandlerSelector
  , subscribeAttributeGlobalSceneControlWithParams_subscriptionEstablished_reportHandlerSelector
  , subscribeAttributeOffWaitTimeWithMinInterval_maxInterval_params_subscriptionEstablished_reportHandlerSelector
  , subscribeAttributeOffWaitTimeWithParams_subscriptionEstablished_reportHandlerSelector
  , subscribeAttributeOnOffWithMinInterval_maxInterval_params_subscriptionEstablished_reportHandlerSelector
  , subscribeAttributeOnOffWithParams_subscriptionEstablished_reportHandlerSelector
  , subscribeAttributeOnTimeWithMinInterval_maxInterval_params_subscriptionEstablished_reportHandlerSelector
  , subscribeAttributeOnTimeWithParams_subscriptionEstablished_reportHandlerSelector
  , subscribeAttributeStartUpOnOffWithMinInterval_maxInterval_params_subscriptionEstablished_reportHandlerSelector
  , subscribeAttributeStartUpOnOffWithParams_subscriptionEstablished_reportHandlerSelector
  , toggleWithCompletionHandlerSelector
  , toggleWithCompletionSelector
  , toggleWithParams_completionHandlerSelector
  , toggleWithParams_completionSelector
  , writeAttributeOffWaitTimeWithValue_completionHandlerSelector
  , writeAttributeOffWaitTimeWithValue_completionSelector
  , writeAttributeOffWaitTimeWithValue_params_completionHandlerSelector
  , writeAttributeOffWaitTimeWithValue_params_completionSelector
  , writeAttributeOnTimeWithValue_completionHandlerSelector
  , writeAttributeOnTimeWithValue_completionSelector
  , writeAttributeOnTimeWithValue_params_completionHandlerSelector
  , writeAttributeOnTimeWithValue_params_completionSelector
  , writeAttributeStartUpOnOffWithValue_completionHandlerSelector
  , writeAttributeStartUpOnOffWithValue_completionSelector
  , writeAttributeStartUpOnOffWithValue_params_completionHandlerSelector
  , writeAttributeStartUpOnOffWithValue_params_completionSelector


  ) where

import Foreign.Ptr (Ptr, FunPtr)
import Foreign.C.Types

import ObjC.Runtime.Types
import ObjC.Runtime.Message (sendMessage, sendOwnedMessage, sendClassMessage, sendOwnedClassMessage)
import ObjC.Runtime.Selector (mkSelector)
import ObjC.Runtime.Class (getRequiredClass)

import ObjC.Matter.Internal.Classes
import ObjC.Foundation.Internal.Classes

-- | Command Off
--
-- On receipt of this command, a device SHALL enter its ‘Off’ state. This state is device dependent, but it is recommended that it is used for power off or similar functions. On receipt of the Off command, the OnTime attribute SHALL be set to 0.
--
-- ObjC selector: @- offWithParams:completion:@
offWithParams_completion :: (IsMTRBaseClusterOnOff mtrBaseClusterOnOff, IsMTROnOffClusterOffParams params) => mtrBaseClusterOnOff -> params -> Ptr () -> IO ()
offWithParams_completion mtrBaseClusterOnOff params completion =
  sendMessage mtrBaseClusterOnOff offWithParams_completionSelector (toMTROnOffClusterOffParams params) completion

-- | @- offWithCompletion:@
offWithCompletion :: IsMTRBaseClusterOnOff mtrBaseClusterOnOff => mtrBaseClusterOnOff -> Ptr () -> IO ()
offWithCompletion mtrBaseClusterOnOff completion =
  sendMessage mtrBaseClusterOnOff offWithCompletionSelector completion

-- | Command On
--
-- On receipt of this command, a device SHALL enter its ‘On’ state. This state is device dependent, but it is recommended that it is used for power on or similar functions. On receipt of the On command, if the value of the OnTime attribute is equal to 0, the device SHALL set the OffWaitTime attribute to 0.
--
-- ObjC selector: @- onWithParams:completion:@
onWithParams_completion :: (IsMTRBaseClusterOnOff mtrBaseClusterOnOff, IsMTROnOffClusterOnParams params) => mtrBaseClusterOnOff -> params -> Ptr () -> IO ()
onWithParams_completion mtrBaseClusterOnOff params completion =
  sendMessage mtrBaseClusterOnOff onWithParams_completionSelector (toMTROnOffClusterOnParams params) completion

-- | @- onWithCompletion:@
onWithCompletion :: IsMTRBaseClusterOnOff mtrBaseClusterOnOff => mtrBaseClusterOnOff -> Ptr () -> IO ()
onWithCompletion mtrBaseClusterOnOff completion =
  sendMessage mtrBaseClusterOnOff onWithCompletionSelector completion

-- | Command Toggle
--
-- On receipt of this command, if a device is in its ‘Off’ state it SHALL enter its ‘On’ state. Otherwise, if it is in its ‘On’ state it SHALL enter its ‘Off’ state. On receipt of the Toggle command, if the value of the OnOff attribute is equal to FALSE and if the value of the OnTime attribute is equal to 0, the device SHALL set the OffWaitTime attribute to 0. If the value of the OnOff attribute is equal to TRUE, the OnTime attribute SHALL be set to 0.
--
-- ObjC selector: @- toggleWithParams:completion:@
toggleWithParams_completion :: (IsMTRBaseClusterOnOff mtrBaseClusterOnOff, IsMTROnOffClusterToggleParams params) => mtrBaseClusterOnOff -> params -> Ptr () -> IO ()
toggleWithParams_completion mtrBaseClusterOnOff params completion =
  sendMessage mtrBaseClusterOnOff toggleWithParams_completionSelector (toMTROnOffClusterToggleParams params) completion

-- | @- toggleWithCompletion:@
toggleWithCompletion :: IsMTRBaseClusterOnOff mtrBaseClusterOnOff => mtrBaseClusterOnOff -> Ptr () -> IO ()
toggleWithCompletion mtrBaseClusterOnOff completion =
  sendMessage mtrBaseClusterOnOff toggleWithCompletionSelector completion

-- | Command OffWithEffect
--
-- The OffWithEffect command allows devices to be turned off using enhanced ways of fading.
--
-- ObjC selector: @- offWithEffectWithParams:completion:@
offWithEffectWithParams_completion :: (IsMTRBaseClusterOnOff mtrBaseClusterOnOff, IsMTROnOffClusterOffWithEffectParams params) => mtrBaseClusterOnOff -> params -> Ptr () -> IO ()
offWithEffectWithParams_completion mtrBaseClusterOnOff params completion =
  sendMessage mtrBaseClusterOnOff offWithEffectWithParams_completionSelector (toMTROnOffClusterOffWithEffectParams params) completion

-- | Command OnWithRecallGlobalScene
--
-- This command allows the recall of the settings when the device was turned off.
--
-- ObjC selector: @- onWithRecallGlobalSceneWithParams:completion:@
onWithRecallGlobalSceneWithParams_completion :: (IsMTRBaseClusterOnOff mtrBaseClusterOnOff, IsMTROnOffClusterOnWithRecallGlobalSceneParams params) => mtrBaseClusterOnOff -> params -> Ptr () -> IO ()
onWithRecallGlobalSceneWithParams_completion mtrBaseClusterOnOff params completion =
  sendMessage mtrBaseClusterOnOff onWithRecallGlobalSceneWithParams_completionSelector (toMTROnOffClusterOnWithRecallGlobalSceneParams params) completion

-- | @- onWithRecallGlobalSceneWithCompletion:@
onWithRecallGlobalSceneWithCompletion :: IsMTRBaseClusterOnOff mtrBaseClusterOnOff => mtrBaseClusterOnOff -> Ptr () -> IO ()
onWithRecallGlobalSceneWithCompletion mtrBaseClusterOnOff completion =
  sendMessage mtrBaseClusterOnOff onWithRecallGlobalSceneWithCompletionSelector completion

-- | Command OnWithTimedOff
--
-- This command allows devices to be turned on for a specific duration with a guarded off duration so that SHOULD the device be subsequently turned off, further OnWithTimedOff commands, received during this time, are prevented from turning the devices back on.
--
-- ObjC selector: @- onWithTimedOffWithParams:completion:@
onWithTimedOffWithParams_completion :: (IsMTRBaseClusterOnOff mtrBaseClusterOnOff, IsMTROnOffClusterOnWithTimedOffParams params) => mtrBaseClusterOnOff -> params -> Ptr () -> IO ()
onWithTimedOffWithParams_completion mtrBaseClusterOnOff params completion =
  sendMessage mtrBaseClusterOnOff onWithTimedOffWithParams_completionSelector (toMTROnOffClusterOnWithTimedOffParams params) completion

-- | @- readAttributeOnOffWithCompletion:@
readAttributeOnOffWithCompletion :: IsMTRBaseClusterOnOff mtrBaseClusterOnOff => mtrBaseClusterOnOff -> Ptr () -> IO ()
readAttributeOnOffWithCompletion mtrBaseClusterOnOff completion =
  sendMessage mtrBaseClusterOnOff readAttributeOnOffWithCompletionSelector completion

-- | @- subscribeAttributeOnOffWithParams:subscriptionEstablished:reportHandler:@
subscribeAttributeOnOffWithParams_subscriptionEstablished_reportHandler :: (IsMTRBaseClusterOnOff mtrBaseClusterOnOff, IsMTRSubscribeParams params) => mtrBaseClusterOnOff -> params -> Ptr () -> Ptr () -> IO ()
subscribeAttributeOnOffWithParams_subscriptionEstablished_reportHandler mtrBaseClusterOnOff params subscriptionEstablished reportHandler =
  sendMessage mtrBaseClusterOnOff subscribeAttributeOnOffWithParams_subscriptionEstablished_reportHandlerSelector (toMTRSubscribeParams params) subscriptionEstablished reportHandler

-- | @+ readAttributeOnOffWithClusterStateCache:endpoint:queue:completion:@
readAttributeOnOffWithClusterStateCache_endpoint_queue_completion :: (IsMTRClusterStateCacheContainer clusterStateCacheContainer, IsNSNumber endpoint, IsNSObject queue) => clusterStateCacheContainer -> endpoint -> queue -> Ptr () -> IO ()
readAttributeOnOffWithClusterStateCache_endpoint_queue_completion clusterStateCacheContainer endpoint queue completion =
  do
    cls' <- getRequiredClass "MTRBaseClusterOnOff"
    sendClassMessage cls' readAttributeOnOffWithClusterStateCache_endpoint_queue_completionSelector (toMTRClusterStateCacheContainer clusterStateCacheContainer) (toNSNumber endpoint) (toNSObject queue) completion

-- | @- readAttributeGlobalSceneControlWithCompletion:@
readAttributeGlobalSceneControlWithCompletion :: IsMTRBaseClusterOnOff mtrBaseClusterOnOff => mtrBaseClusterOnOff -> Ptr () -> IO ()
readAttributeGlobalSceneControlWithCompletion mtrBaseClusterOnOff completion =
  sendMessage mtrBaseClusterOnOff readAttributeGlobalSceneControlWithCompletionSelector completion

-- | @- subscribeAttributeGlobalSceneControlWithParams:subscriptionEstablished:reportHandler:@
subscribeAttributeGlobalSceneControlWithParams_subscriptionEstablished_reportHandler :: (IsMTRBaseClusterOnOff mtrBaseClusterOnOff, IsMTRSubscribeParams params) => mtrBaseClusterOnOff -> params -> Ptr () -> Ptr () -> IO ()
subscribeAttributeGlobalSceneControlWithParams_subscriptionEstablished_reportHandler mtrBaseClusterOnOff params subscriptionEstablished reportHandler =
  sendMessage mtrBaseClusterOnOff subscribeAttributeGlobalSceneControlWithParams_subscriptionEstablished_reportHandlerSelector (toMTRSubscribeParams params) subscriptionEstablished reportHandler

-- | @+ readAttributeGlobalSceneControlWithClusterStateCache:endpoint:queue:completion:@
readAttributeGlobalSceneControlWithClusterStateCache_endpoint_queue_completion :: (IsMTRClusterStateCacheContainer clusterStateCacheContainer, IsNSNumber endpoint, IsNSObject queue) => clusterStateCacheContainer -> endpoint -> queue -> Ptr () -> IO ()
readAttributeGlobalSceneControlWithClusterStateCache_endpoint_queue_completion clusterStateCacheContainer endpoint queue completion =
  do
    cls' <- getRequiredClass "MTRBaseClusterOnOff"
    sendClassMessage cls' readAttributeGlobalSceneControlWithClusterStateCache_endpoint_queue_completionSelector (toMTRClusterStateCacheContainer clusterStateCacheContainer) (toNSNumber endpoint) (toNSObject queue) completion

-- | @- readAttributeOnTimeWithCompletion:@
readAttributeOnTimeWithCompletion :: IsMTRBaseClusterOnOff mtrBaseClusterOnOff => mtrBaseClusterOnOff -> Ptr () -> IO ()
readAttributeOnTimeWithCompletion mtrBaseClusterOnOff completion =
  sendMessage mtrBaseClusterOnOff readAttributeOnTimeWithCompletionSelector completion

-- | @- writeAttributeOnTimeWithValue:completion:@
writeAttributeOnTimeWithValue_completion :: (IsMTRBaseClusterOnOff mtrBaseClusterOnOff, IsNSNumber value) => mtrBaseClusterOnOff -> value -> Ptr () -> IO ()
writeAttributeOnTimeWithValue_completion mtrBaseClusterOnOff value completion =
  sendMessage mtrBaseClusterOnOff writeAttributeOnTimeWithValue_completionSelector (toNSNumber value) completion

-- | @- writeAttributeOnTimeWithValue:params:completion:@
writeAttributeOnTimeWithValue_params_completion :: (IsMTRBaseClusterOnOff mtrBaseClusterOnOff, IsNSNumber value, IsMTRWriteParams params) => mtrBaseClusterOnOff -> value -> params -> Ptr () -> IO ()
writeAttributeOnTimeWithValue_params_completion mtrBaseClusterOnOff value params completion =
  sendMessage mtrBaseClusterOnOff writeAttributeOnTimeWithValue_params_completionSelector (toNSNumber value) (toMTRWriteParams params) completion

-- | @- subscribeAttributeOnTimeWithParams:subscriptionEstablished:reportHandler:@
subscribeAttributeOnTimeWithParams_subscriptionEstablished_reportHandler :: (IsMTRBaseClusterOnOff mtrBaseClusterOnOff, IsMTRSubscribeParams params) => mtrBaseClusterOnOff -> params -> Ptr () -> Ptr () -> IO ()
subscribeAttributeOnTimeWithParams_subscriptionEstablished_reportHandler mtrBaseClusterOnOff params subscriptionEstablished reportHandler =
  sendMessage mtrBaseClusterOnOff subscribeAttributeOnTimeWithParams_subscriptionEstablished_reportHandlerSelector (toMTRSubscribeParams params) subscriptionEstablished reportHandler

-- | @+ readAttributeOnTimeWithClusterStateCache:endpoint:queue:completion:@
readAttributeOnTimeWithClusterStateCache_endpoint_queue_completion :: (IsMTRClusterStateCacheContainer clusterStateCacheContainer, IsNSNumber endpoint, IsNSObject queue) => clusterStateCacheContainer -> endpoint -> queue -> Ptr () -> IO ()
readAttributeOnTimeWithClusterStateCache_endpoint_queue_completion clusterStateCacheContainer endpoint queue completion =
  do
    cls' <- getRequiredClass "MTRBaseClusterOnOff"
    sendClassMessage cls' readAttributeOnTimeWithClusterStateCache_endpoint_queue_completionSelector (toMTRClusterStateCacheContainer clusterStateCacheContainer) (toNSNumber endpoint) (toNSObject queue) completion

-- | @- readAttributeOffWaitTimeWithCompletion:@
readAttributeOffWaitTimeWithCompletion :: IsMTRBaseClusterOnOff mtrBaseClusterOnOff => mtrBaseClusterOnOff -> Ptr () -> IO ()
readAttributeOffWaitTimeWithCompletion mtrBaseClusterOnOff completion =
  sendMessage mtrBaseClusterOnOff readAttributeOffWaitTimeWithCompletionSelector completion

-- | @- writeAttributeOffWaitTimeWithValue:completion:@
writeAttributeOffWaitTimeWithValue_completion :: (IsMTRBaseClusterOnOff mtrBaseClusterOnOff, IsNSNumber value) => mtrBaseClusterOnOff -> value -> Ptr () -> IO ()
writeAttributeOffWaitTimeWithValue_completion mtrBaseClusterOnOff value completion =
  sendMessage mtrBaseClusterOnOff writeAttributeOffWaitTimeWithValue_completionSelector (toNSNumber value) completion

-- | @- writeAttributeOffWaitTimeWithValue:params:completion:@
writeAttributeOffWaitTimeWithValue_params_completion :: (IsMTRBaseClusterOnOff mtrBaseClusterOnOff, IsNSNumber value, IsMTRWriteParams params) => mtrBaseClusterOnOff -> value -> params -> Ptr () -> IO ()
writeAttributeOffWaitTimeWithValue_params_completion mtrBaseClusterOnOff value params completion =
  sendMessage mtrBaseClusterOnOff writeAttributeOffWaitTimeWithValue_params_completionSelector (toNSNumber value) (toMTRWriteParams params) completion

-- | @- subscribeAttributeOffWaitTimeWithParams:subscriptionEstablished:reportHandler:@
subscribeAttributeOffWaitTimeWithParams_subscriptionEstablished_reportHandler :: (IsMTRBaseClusterOnOff mtrBaseClusterOnOff, IsMTRSubscribeParams params) => mtrBaseClusterOnOff -> params -> Ptr () -> Ptr () -> IO ()
subscribeAttributeOffWaitTimeWithParams_subscriptionEstablished_reportHandler mtrBaseClusterOnOff params subscriptionEstablished reportHandler =
  sendMessage mtrBaseClusterOnOff subscribeAttributeOffWaitTimeWithParams_subscriptionEstablished_reportHandlerSelector (toMTRSubscribeParams params) subscriptionEstablished reportHandler

-- | @+ readAttributeOffWaitTimeWithClusterStateCache:endpoint:queue:completion:@
readAttributeOffWaitTimeWithClusterStateCache_endpoint_queue_completion :: (IsMTRClusterStateCacheContainer clusterStateCacheContainer, IsNSNumber endpoint, IsNSObject queue) => clusterStateCacheContainer -> endpoint -> queue -> Ptr () -> IO ()
readAttributeOffWaitTimeWithClusterStateCache_endpoint_queue_completion clusterStateCacheContainer endpoint queue completion =
  do
    cls' <- getRequiredClass "MTRBaseClusterOnOff"
    sendClassMessage cls' readAttributeOffWaitTimeWithClusterStateCache_endpoint_queue_completionSelector (toMTRClusterStateCacheContainer clusterStateCacheContainer) (toNSNumber endpoint) (toNSObject queue) completion

-- | @- readAttributeStartUpOnOffWithCompletion:@
readAttributeStartUpOnOffWithCompletion :: IsMTRBaseClusterOnOff mtrBaseClusterOnOff => mtrBaseClusterOnOff -> Ptr () -> IO ()
readAttributeStartUpOnOffWithCompletion mtrBaseClusterOnOff completion =
  sendMessage mtrBaseClusterOnOff readAttributeStartUpOnOffWithCompletionSelector completion

-- | @- writeAttributeStartUpOnOffWithValue:completion:@
writeAttributeStartUpOnOffWithValue_completion :: (IsMTRBaseClusterOnOff mtrBaseClusterOnOff, IsNSNumber value) => mtrBaseClusterOnOff -> value -> Ptr () -> IO ()
writeAttributeStartUpOnOffWithValue_completion mtrBaseClusterOnOff value completion =
  sendMessage mtrBaseClusterOnOff writeAttributeStartUpOnOffWithValue_completionSelector (toNSNumber value) completion

-- | @- writeAttributeStartUpOnOffWithValue:params:completion:@
writeAttributeStartUpOnOffWithValue_params_completion :: (IsMTRBaseClusterOnOff mtrBaseClusterOnOff, IsNSNumber value, IsMTRWriteParams params) => mtrBaseClusterOnOff -> value -> params -> Ptr () -> IO ()
writeAttributeStartUpOnOffWithValue_params_completion mtrBaseClusterOnOff value params completion =
  sendMessage mtrBaseClusterOnOff writeAttributeStartUpOnOffWithValue_params_completionSelector (toNSNumber value) (toMTRWriteParams params) completion

-- | @- subscribeAttributeStartUpOnOffWithParams:subscriptionEstablished:reportHandler:@
subscribeAttributeStartUpOnOffWithParams_subscriptionEstablished_reportHandler :: (IsMTRBaseClusterOnOff mtrBaseClusterOnOff, IsMTRSubscribeParams params) => mtrBaseClusterOnOff -> params -> Ptr () -> Ptr () -> IO ()
subscribeAttributeStartUpOnOffWithParams_subscriptionEstablished_reportHandler mtrBaseClusterOnOff params subscriptionEstablished reportHandler =
  sendMessage mtrBaseClusterOnOff subscribeAttributeStartUpOnOffWithParams_subscriptionEstablished_reportHandlerSelector (toMTRSubscribeParams params) subscriptionEstablished reportHandler

-- | @+ readAttributeStartUpOnOffWithClusterStateCache:endpoint:queue:completion:@
readAttributeStartUpOnOffWithClusterStateCache_endpoint_queue_completion :: (IsMTRClusterStateCacheContainer clusterStateCacheContainer, IsNSNumber endpoint, IsNSObject queue) => clusterStateCacheContainer -> endpoint -> queue -> Ptr () -> IO ()
readAttributeStartUpOnOffWithClusterStateCache_endpoint_queue_completion clusterStateCacheContainer endpoint queue completion =
  do
    cls' <- getRequiredClass "MTRBaseClusterOnOff"
    sendClassMessage cls' readAttributeStartUpOnOffWithClusterStateCache_endpoint_queue_completionSelector (toMTRClusterStateCacheContainer clusterStateCacheContainer) (toNSNumber endpoint) (toNSObject queue) completion

-- | @- readAttributeGeneratedCommandListWithCompletion:@
readAttributeGeneratedCommandListWithCompletion :: IsMTRBaseClusterOnOff mtrBaseClusterOnOff => mtrBaseClusterOnOff -> Ptr () -> IO ()
readAttributeGeneratedCommandListWithCompletion mtrBaseClusterOnOff completion =
  sendMessage mtrBaseClusterOnOff readAttributeGeneratedCommandListWithCompletionSelector completion

-- | @- subscribeAttributeGeneratedCommandListWithParams:subscriptionEstablished:reportHandler:@
subscribeAttributeGeneratedCommandListWithParams_subscriptionEstablished_reportHandler :: (IsMTRBaseClusterOnOff mtrBaseClusterOnOff, IsMTRSubscribeParams params) => mtrBaseClusterOnOff -> params -> Ptr () -> Ptr () -> IO ()
subscribeAttributeGeneratedCommandListWithParams_subscriptionEstablished_reportHandler mtrBaseClusterOnOff params subscriptionEstablished reportHandler =
  sendMessage mtrBaseClusterOnOff subscribeAttributeGeneratedCommandListWithParams_subscriptionEstablished_reportHandlerSelector (toMTRSubscribeParams params) subscriptionEstablished reportHandler

-- | @+ readAttributeGeneratedCommandListWithClusterStateCache:endpoint:queue:completion:@
readAttributeGeneratedCommandListWithClusterStateCache_endpoint_queue_completion :: (IsMTRClusterStateCacheContainer clusterStateCacheContainer, IsNSNumber endpoint, IsNSObject queue) => clusterStateCacheContainer -> endpoint -> queue -> Ptr () -> IO ()
readAttributeGeneratedCommandListWithClusterStateCache_endpoint_queue_completion clusterStateCacheContainer endpoint queue completion =
  do
    cls' <- getRequiredClass "MTRBaseClusterOnOff"
    sendClassMessage cls' readAttributeGeneratedCommandListWithClusterStateCache_endpoint_queue_completionSelector (toMTRClusterStateCacheContainer clusterStateCacheContainer) (toNSNumber endpoint) (toNSObject queue) completion

-- | @- readAttributeAcceptedCommandListWithCompletion:@
readAttributeAcceptedCommandListWithCompletion :: IsMTRBaseClusterOnOff mtrBaseClusterOnOff => mtrBaseClusterOnOff -> Ptr () -> IO ()
readAttributeAcceptedCommandListWithCompletion mtrBaseClusterOnOff completion =
  sendMessage mtrBaseClusterOnOff readAttributeAcceptedCommandListWithCompletionSelector completion

-- | @- subscribeAttributeAcceptedCommandListWithParams:subscriptionEstablished:reportHandler:@
subscribeAttributeAcceptedCommandListWithParams_subscriptionEstablished_reportHandler :: (IsMTRBaseClusterOnOff mtrBaseClusterOnOff, IsMTRSubscribeParams params) => mtrBaseClusterOnOff -> params -> Ptr () -> Ptr () -> IO ()
subscribeAttributeAcceptedCommandListWithParams_subscriptionEstablished_reportHandler mtrBaseClusterOnOff params subscriptionEstablished reportHandler =
  sendMessage mtrBaseClusterOnOff subscribeAttributeAcceptedCommandListWithParams_subscriptionEstablished_reportHandlerSelector (toMTRSubscribeParams params) subscriptionEstablished reportHandler

-- | @+ readAttributeAcceptedCommandListWithClusterStateCache:endpoint:queue:completion:@
readAttributeAcceptedCommandListWithClusterStateCache_endpoint_queue_completion :: (IsMTRClusterStateCacheContainer clusterStateCacheContainer, IsNSNumber endpoint, IsNSObject queue) => clusterStateCacheContainer -> endpoint -> queue -> Ptr () -> IO ()
readAttributeAcceptedCommandListWithClusterStateCache_endpoint_queue_completion clusterStateCacheContainer endpoint queue completion =
  do
    cls' <- getRequiredClass "MTRBaseClusterOnOff"
    sendClassMessage cls' readAttributeAcceptedCommandListWithClusterStateCache_endpoint_queue_completionSelector (toMTRClusterStateCacheContainer clusterStateCacheContainer) (toNSNumber endpoint) (toNSObject queue) completion

-- | @- readAttributeAttributeListWithCompletion:@
readAttributeAttributeListWithCompletion :: IsMTRBaseClusterOnOff mtrBaseClusterOnOff => mtrBaseClusterOnOff -> Ptr () -> IO ()
readAttributeAttributeListWithCompletion mtrBaseClusterOnOff completion =
  sendMessage mtrBaseClusterOnOff readAttributeAttributeListWithCompletionSelector completion

-- | @- subscribeAttributeAttributeListWithParams:subscriptionEstablished:reportHandler:@
subscribeAttributeAttributeListWithParams_subscriptionEstablished_reportHandler :: (IsMTRBaseClusterOnOff mtrBaseClusterOnOff, IsMTRSubscribeParams params) => mtrBaseClusterOnOff -> params -> Ptr () -> Ptr () -> IO ()
subscribeAttributeAttributeListWithParams_subscriptionEstablished_reportHandler mtrBaseClusterOnOff params subscriptionEstablished reportHandler =
  sendMessage mtrBaseClusterOnOff subscribeAttributeAttributeListWithParams_subscriptionEstablished_reportHandlerSelector (toMTRSubscribeParams params) subscriptionEstablished reportHandler

-- | @+ readAttributeAttributeListWithClusterStateCache:endpoint:queue:completion:@
readAttributeAttributeListWithClusterStateCache_endpoint_queue_completion :: (IsMTRClusterStateCacheContainer clusterStateCacheContainer, IsNSNumber endpoint, IsNSObject queue) => clusterStateCacheContainer -> endpoint -> queue -> Ptr () -> IO ()
readAttributeAttributeListWithClusterStateCache_endpoint_queue_completion clusterStateCacheContainer endpoint queue completion =
  do
    cls' <- getRequiredClass "MTRBaseClusterOnOff"
    sendClassMessage cls' readAttributeAttributeListWithClusterStateCache_endpoint_queue_completionSelector (toMTRClusterStateCacheContainer clusterStateCacheContainer) (toNSNumber endpoint) (toNSObject queue) completion

-- | @- readAttributeFeatureMapWithCompletion:@
readAttributeFeatureMapWithCompletion :: IsMTRBaseClusterOnOff mtrBaseClusterOnOff => mtrBaseClusterOnOff -> Ptr () -> IO ()
readAttributeFeatureMapWithCompletion mtrBaseClusterOnOff completion =
  sendMessage mtrBaseClusterOnOff readAttributeFeatureMapWithCompletionSelector completion

-- | @- subscribeAttributeFeatureMapWithParams:subscriptionEstablished:reportHandler:@
subscribeAttributeFeatureMapWithParams_subscriptionEstablished_reportHandler :: (IsMTRBaseClusterOnOff mtrBaseClusterOnOff, IsMTRSubscribeParams params) => mtrBaseClusterOnOff -> params -> Ptr () -> Ptr () -> IO ()
subscribeAttributeFeatureMapWithParams_subscriptionEstablished_reportHandler mtrBaseClusterOnOff params subscriptionEstablished reportHandler =
  sendMessage mtrBaseClusterOnOff subscribeAttributeFeatureMapWithParams_subscriptionEstablished_reportHandlerSelector (toMTRSubscribeParams params) subscriptionEstablished reportHandler

-- | @+ readAttributeFeatureMapWithClusterStateCache:endpoint:queue:completion:@
readAttributeFeatureMapWithClusterStateCache_endpoint_queue_completion :: (IsMTRClusterStateCacheContainer clusterStateCacheContainer, IsNSNumber endpoint, IsNSObject queue) => clusterStateCacheContainer -> endpoint -> queue -> Ptr () -> IO ()
readAttributeFeatureMapWithClusterStateCache_endpoint_queue_completion clusterStateCacheContainer endpoint queue completion =
  do
    cls' <- getRequiredClass "MTRBaseClusterOnOff"
    sendClassMessage cls' readAttributeFeatureMapWithClusterStateCache_endpoint_queue_completionSelector (toMTRClusterStateCacheContainer clusterStateCacheContainer) (toNSNumber endpoint) (toNSObject queue) completion

-- | @- readAttributeClusterRevisionWithCompletion:@
readAttributeClusterRevisionWithCompletion :: IsMTRBaseClusterOnOff mtrBaseClusterOnOff => mtrBaseClusterOnOff -> Ptr () -> IO ()
readAttributeClusterRevisionWithCompletion mtrBaseClusterOnOff completion =
  sendMessage mtrBaseClusterOnOff readAttributeClusterRevisionWithCompletionSelector completion

-- | @- subscribeAttributeClusterRevisionWithParams:subscriptionEstablished:reportHandler:@
subscribeAttributeClusterRevisionWithParams_subscriptionEstablished_reportHandler :: (IsMTRBaseClusterOnOff mtrBaseClusterOnOff, IsMTRSubscribeParams params) => mtrBaseClusterOnOff -> params -> Ptr () -> Ptr () -> IO ()
subscribeAttributeClusterRevisionWithParams_subscriptionEstablished_reportHandler mtrBaseClusterOnOff params subscriptionEstablished reportHandler =
  sendMessage mtrBaseClusterOnOff subscribeAttributeClusterRevisionWithParams_subscriptionEstablished_reportHandlerSelector (toMTRSubscribeParams params) subscriptionEstablished reportHandler

-- | @+ readAttributeClusterRevisionWithClusterStateCache:endpoint:queue:completion:@
readAttributeClusterRevisionWithClusterStateCache_endpoint_queue_completion :: (IsMTRClusterStateCacheContainer clusterStateCacheContainer, IsNSNumber endpoint, IsNSObject queue) => clusterStateCacheContainer -> endpoint -> queue -> Ptr () -> IO ()
readAttributeClusterRevisionWithClusterStateCache_endpoint_queue_completion clusterStateCacheContainer endpoint queue completion =
  do
    cls' <- getRequiredClass "MTRBaseClusterOnOff"
    sendClassMessage cls' readAttributeClusterRevisionWithClusterStateCache_endpoint_queue_completionSelector (toMTRClusterStateCacheContainer clusterStateCacheContainer) (toNSNumber endpoint) (toNSObject queue) completion

-- | @- init@
init_ :: IsMTRBaseClusterOnOff mtrBaseClusterOnOff => mtrBaseClusterOnOff -> IO (Id MTRBaseClusterOnOff)
init_ mtrBaseClusterOnOff =
  sendOwnedMessage mtrBaseClusterOnOff initSelector

-- | @+ new@
new :: IO (Id MTRBaseClusterOnOff)
new  =
  do
    cls' <- getRequiredClass "MTRBaseClusterOnOff"
    sendOwnedClassMessage cls' newSelector

-- | @- initWithDevice:endpoint:queue:@
initWithDevice_endpoint_queue :: (IsMTRBaseClusterOnOff mtrBaseClusterOnOff, IsMTRBaseDevice device, IsNSObject queue) => mtrBaseClusterOnOff -> device -> CUShort -> queue -> IO (Id MTRBaseClusterOnOff)
initWithDevice_endpoint_queue mtrBaseClusterOnOff device endpoint queue =
  sendOwnedMessage mtrBaseClusterOnOff initWithDevice_endpoint_queueSelector (toMTRBaseDevice device) endpoint (toNSObject queue)

-- | @- offWithParams:completionHandler:@
offWithParams_completionHandler :: (IsMTRBaseClusterOnOff mtrBaseClusterOnOff, IsMTROnOffClusterOffParams params) => mtrBaseClusterOnOff -> params -> Ptr () -> IO ()
offWithParams_completionHandler mtrBaseClusterOnOff params completionHandler =
  sendMessage mtrBaseClusterOnOff offWithParams_completionHandlerSelector (toMTROnOffClusterOffParams params) completionHandler

-- | @- offWithCompletionHandler:@
offWithCompletionHandler :: IsMTRBaseClusterOnOff mtrBaseClusterOnOff => mtrBaseClusterOnOff -> Ptr () -> IO ()
offWithCompletionHandler mtrBaseClusterOnOff completionHandler =
  sendMessage mtrBaseClusterOnOff offWithCompletionHandlerSelector completionHandler

-- | @- onWithParams:completionHandler:@
onWithParams_completionHandler :: (IsMTRBaseClusterOnOff mtrBaseClusterOnOff, IsMTROnOffClusterOnParams params) => mtrBaseClusterOnOff -> params -> Ptr () -> IO ()
onWithParams_completionHandler mtrBaseClusterOnOff params completionHandler =
  sendMessage mtrBaseClusterOnOff onWithParams_completionHandlerSelector (toMTROnOffClusterOnParams params) completionHandler

-- | @- onWithCompletionHandler:@
onWithCompletionHandler :: IsMTRBaseClusterOnOff mtrBaseClusterOnOff => mtrBaseClusterOnOff -> Ptr () -> IO ()
onWithCompletionHandler mtrBaseClusterOnOff completionHandler =
  sendMessage mtrBaseClusterOnOff onWithCompletionHandlerSelector completionHandler

-- | @- toggleWithParams:completionHandler:@
toggleWithParams_completionHandler :: (IsMTRBaseClusterOnOff mtrBaseClusterOnOff, IsMTROnOffClusterToggleParams params) => mtrBaseClusterOnOff -> params -> Ptr () -> IO ()
toggleWithParams_completionHandler mtrBaseClusterOnOff params completionHandler =
  sendMessage mtrBaseClusterOnOff toggleWithParams_completionHandlerSelector (toMTROnOffClusterToggleParams params) completionHandler

-- | @- toggleWithCompletionHandler:@
toggleWithCompletionHandler :: IsMTRBaseClusterOnOff mtrBaseClusterOnOff => mtrBaseClusterOnOff -> Ptr () -> IO ()
toggleWithCompletionHandler mtrBaseClusterOnOff completionHandler =
  sendMessage mtrBaseClusterOnOff toggleWithCompletionHandlerSelector completionHandler

-- | @- offWithEffectWithParams:completionHandler:@
offWithEffectWithParams_completionHandler :: (IsMTRBaseClusterOnOff mtrBaseClusterOnOff, IsMTROnOffClusterOffWithEffectParams params) => mtrBaseClusterOnOff -> params -> Ptr () -> IO ()
offWithEffectWithParams_completionHandler mtrBaseClusterOnOff params completionHandler =
  sendMessage mtrBaseClusterOnOff offWithEffectWithParams_completionHandlerSelector (toMTROnOffClusterOffWithEffectParams params) completionHandler

-- | @- onWithRecallGlobalSceneWithParams:completionHandler:@
onWithRecallGlobalSceneWithParams_completionHandler :: (IsMTRBaseClusterOnOff mtrBaseClusterOnOff, IsMTROnOffClusterOnWithRecallGlobalSceneParams params) => mtrBaseClusterOnOff -> params -> Ptr () -> IO ()
onWithRecallGlobalSceneWithParams_completionHandler mtrBaseClusterOnOff params completionHandler =
  sendMessage mtrBaseClusterOnOff onWithRecallGlobalSceneWithParams_completionHandlerSelector (toMTROnOffClusterOnWithRecallGlobalSceneParams params) completionHandler

-- | @- onWithRecallGlobalSceneWithCompletionHandler:@
onWithRecallGlobalSceneWithCompletionHandler :: IsMTRBaseClusterOnOff mtrBaseClusterOnOff => mtrBaseClusterOnOff -> Ptr () -> IO ()
onWithRecallGlobalSceneWithCompletionHandler mtrBaseClusterOnOff completionHandler =
  sendMessage mtrBaseClusterOnOff onWithRecallGlobalSceneWithCompletionHandlerSelector completionHandler

-- | @- onWithTimedOffWithParams:completionHandler:@
onWithTimedOffWithParams_completionHandler :: (IsMTRBaseClusterOnOff mtrBaseClusterOnOff, IsMTROnOffClusterOnWithTimedOffParams params) => mtrBaseClusterOnOff -> params -> Ptr () -> IO ()
onWithTimedOffWithParams_completionHandler mtrBaseClusterOnOff params completionHandler =
  sendMessage mtrBaseClusterOnOff onWithTimedOffWithParams_completionHandlerSelector (toMTROnOffClusterOnWithTimedOffParams params) completionHandler

-- | @- readAttributeOnOffWithCompletionHandler:@
readAttributeOnOffWithCompletionHandler :: IsMTRBaseClusterOnOff mtrBaseClusterOnOff => mtrBaseClusterOnOff -> Ptr () -> IO ()
readAttributeOnOffWithCompletionHandler mtrBaseClusterOnOff completionHandler =
  sendMessage mtrBaseClusterOnOff readAttributeOnOffWithCompletionHandlerSelector completionHandler

-- | @- subscribeAttributeOnOffWithMinInterval:maxInterval:params:subscriptionEstablished:reportHandler:@
subscribeAttributeOnOffWithMinInterval_maxInterval_params_subscriptionEstablished_reportHandler :: (IsMTRBaseClusterOnOff mtrBaseClusterOnOff, IsNSNumber minInterval, IsNSNumber maxInterval, IsMTRSubscribeParams params) => mtrBaseClusterOnOff -> minInterval -> maxInterval -> params -> Ptr () -> Ptr () -> IO ()
subscribeAttributeOnOffWithMinInterval_maxInterval_params_subscriptionEstablished_reportHandler mtrBaseClusterOnOff minInterval maxInterval params subscriptionEstablishedHandler reportHandler =
  sendMessage mtrBaseClusterOnOff subscribeAttributeOnOffWithMinInterval_maxInterval_params_subscriptionEstablished_reportHandlerSelector (toNSNumber minInterval) (toNSNumber maxInterval) (toMTRSubscribeParams params) subscriptionEstablishedHandler reportHandler

-- | @+ readAttributeOnOffWithAttributeCache:endpoint:queue:completionHandler:@
readAttributeOnOffWithAttributeCache_endpoint_queue_completionHandler :: (IsMTRAttributeCacheContainer attributeCacheContainer, IsNSNumber endpoint, IsNSObject queue) => attributeCacheContainer -> endpoint -> queue -> Ptr () -> IO ()
readAttributeOnOffWithAttributeCache_endpoint_queue_completionHandler attributeCacheContainer endpoint queue completionHandler =
  do
    cls' <- getRequiredClass "MTRBaseClusterOnOff"
    sendClassMessage cls' readAttributeOnOffWithAttributeCache_endpoint_queue_completionHandlerSelector (toMTRAttributeCacheContainer attributeCacheContainer) (toNSNumber endpoint) (toNSObject queue) completionHandler

-- | @- readAttributeGlobalSceneControlWithCompletionHandler:@
readAttributeGlobalSceneControlWithCompletionHandler :: IsMTRBaseClusterOnOff mtrBaseClusterOnOff => mtrBaseClusterOnOff -> Ptr () -> IO ()
readAttributeGlobalSceneControlWithCompletionHandler mtrBaseClusterOnOff completionHandler =
  sendMessage mtrBaseClusterOnOff readAttributeGlobalSceneControlWithCompletionHandlerSelector completionHandler

-- | @- subscribeAttributeGlobalSceneControlWithMinInterval:maxInterval:params:subscriptionEstablished:reportHandler:@
subscribeAttributeGlobalSceneControlWithMinInterval_maxInterval_params_subscriptionEstablished_reportHandler :: (IsMTRBaseClusterOnOff mtrBaseClusterOnOff, IsNSNumber minInterval, IsNSNumber maxInterval, IsMTRSubscribeParams params) => mtrBaseClusterOnOff -> minInterval -> maxInterval -> params -> Ptr () -> Ptr () -> IO ()
subscribeAttributeGlobalSceneControlWithMinInterval_maxInterval_params_subscriptionEstablished_reportHandler mtrBaseClusterOnOff minInterval maxInterval params subscriptionEstablishedHandler reportHandler =
  sendMessage mtrBaseClusterOnOff subscribeAttributeGlobalSceneControlWithMinInterval_maxInterval_params_subscriptionEstablished_reportHandlerSelector (toNSNumber minInterval) (toNSNumber maxInterval) (toMTRSubscribeParams params) subscriptionEstablishedHandler reportHandler

-- | @+ readAttributeGlobalSceneControlWithAttributeCache:endpoint:queue:completionHandler:@
readAttributeGlobalSceneControlWithAttributeCache_endpoint_queue_completionHandler :: (IsMTRAttributeCacheContainer attributeCacheContainer, IsNSNumber endpoint, IsNSObject queue) => attributeCacheContainer -> endpoint -> queue -> Ptr () -> IO ()
readAttributeGlobalSceneControlWithAttributeCache_endpoint_queue_completionHandler attributeCacheContainer endpoint queue completionHandler =
  do
    cls' <- getRequiredClass "MTRBaseClusterOnOff"
    sendClassMessage cls' readAttributeGlobalSceneControlWithAttributeCache_endpoint_queue_completionHandlerSelector (toMTRAttributeCacheContainer attributeCacheContainer) (toNSNumber endpoint) (toNSObject queue) completionHandler

-- | @- readAttributeOnTimeWithCompletionHandler:@
readAttributeOnTimeWithCompletionHandler :: IsMTRBaseClusterOnOff mtrBaseClusterOnOff => mtrBaseClusterOnOff -> Ptr () -> IO ()
readAttributeOnTimeWithCompletionHandler mtrBaseClusterOnOff completionHandler =
  sendMessage mtrBaseClusterOnOff readAttributeOnTimeWithCompletionHandlerSelector completionHandler

-- | @- writeAttributeOnTimeWithValue:completionHandler:@
writeAttributeOnTimeWithValue_completionHandler :: (IsMTRBaseClusterOnOff mtrBaseClusterOnOff, IsNSNumber value) => mtrBaseClusterOnOff -> value -> Ptr () -> IO ()
writeAttributeOnTimeWithValue_completionHandler mtrBaseClusterOnOff value completionHandler =
  sendMessage mtrBaseClusterOnOff writeAttributeOnTimeWithValue_completionHandlerSelector (toNSNumber value) completionHandler

-- | @- writeAttributeOnTimeWithValue:params:completionHandler:@
writeAttributeOnTimeWithValue_params_completionHandler :: (IsMTRBaseClusterOnOff mtrBaseClusterOnOff, IsNSNumber value, IsMTRWriteParams params) => mtrBaseClusterOnOff -> value -> params -> Ptr () -> IO ()
writeAttributeOnTimeWithValue_params_completionHandler mtrBaseClusterOnOff value params completionHandler =
  sendMessage mtrBaseClusterOnOff writeAttributeOnTimeWithValue_params_completionHandlerSelector (toNSNumber value) (toMTRWriteParams params) completionHandler

-- | @- subscribeAttributeOnTimeWithMinInterval:maxInterval:params:subscriptionEstablished:reportHandler:@
subscribeAttributeOnTimeWithMinInterval_maxInterval_params_subscriptionEstablished_reportHandler :: (IsMTRBaseClusterOnOff mtrBaseClusterOnOff, IsNSNumber minInterval, IsNSNumber maxInterval, IsMTRSubscribeParams params) => mtrBaseClusterOnOff -> minInterval -> maxInterval -> params -> Ptr () -> Ptr () -> IO ()
subscribeAttributeOnTimeWithMinInterval_maxInterval_params_subscriptionEstablished_reportHandler mtrBaseClusterOnOff minInterval maxInterval params subscriptionEstablishedHandler reportHandler =
  sendMessage mtrBaseClusterOnOff subscribeAttributeOnTimeWithMinInterval_maxInterval_params_subscriptionEstablished_reportHandlerSelector (toNSNumber minInterval) (toNSNumber maxInterval) (toMTRSubscribeParams params) subscriptionEstablishedHandler reportHandler

-- | @+ readAttributeOnTimeWithAttributeCache:endpoint:queue:completionHandler:@
readAttributeOnTimeWithAttributeCache_endpoint_queue_completionHandler :: (IsMTRAttributeCacheContainer attributeCacheContainer, IsNSNumber endpoint, IsNSObject queue) => attributeCacheContainer -> endpoint -> queue -> Ptr () -> IO ()
readAttributeOnTimeWithAttributeCache_endpoint_queue_completionHandler attributeCacheContainer endpoint queue completionHandler =
  do
    cls' <- getRequiredClass "MTRBaseClusterOnOff"
    sendClassMessage cls' readAttributeOnTimeWithAttributeCache_endpoint_queue_completionHandlerSelector (toMTRAttributeCacheContainer attributeCacheContainer) (toNSNumber endpoint) (toNSObject queue) completionHandler

-- | @- readAttributeOffWaitTimeWithCompletionHandler:@
readAttributeOffWaitTimeWithCompletionHandler :: IsMTRBaseClusterOnOff mtrBaseClusterOnOff => mtrBaseClusterOnOff -> Ptr () -> IO ()
readAttributeOffWaitTimeWithCompletionHandler mtrBaseClusterOnOff completionHandler =
  sendMessage mtrBaseClusterOnOff readAttributeOffWaitTimeWithCompletionHandlerSelector completionHandler

-- | @- writeAttributeOffWaitTimeWithValue:completionHandler:@
writeAttributeOffWaitTimeWithValue_completionHandler :: (IsMTRBaseClusterOnOff mtrBaseClusterOnOff, IsNSNumber value) => mtrBaseClusterOnOff -> value -> Ptr () -> IO ()
writeAttributeOffWaitTimeWithValue_completionHandler mtrBaseClusterOnOff value completionHandler =
  sendMessage mtrBaseClusterOnOff writeAttributeOffWaitTimeWithValue_completionHandlerSelector (toNSNumber value) completionHandler

-- | @- writeAttributeOffWaitTimeWithValue:params:completionHandler:@
writeAttributeOffWaitTimeWithValue_params_completionHandler :: (IsMTRBaseClusterOnOff mtrBaseClusterOnOff, IsNSNumber value, IsMTRWriteParams params) => mtrBaseClusterOnOff -> value -> params -> Ptr () -> IO ()
writeAttributeOffWaitTimeWithValue_params_completionHandler mtrBaseClusterOnOff value params completionHandler =
  sendMessage mtrBaseClusterOnOff writeAttributeOffWaitTimeWithValue_params_completionHandlerSelector (toNSNumber value) (toMTRWriteParams params) completionHandler

-- | @- subscribeAttributeOffWaitTimeWithMinInterval:maxInterval:params:subscriptionEstablished:reportHandler:@
subscribeAttributeOffWaitTimeWithMinInterval_maxInterval_params_subscriptionEstablished_reportHandler :: (IsMTRBaseClusterOnOff mtrBaseClusterOnOff, IsNSNumber minInterval, IsNSNumber maxInterval, IsMTRSubscribeParams params) => mtrBaseClusterOnOff -> minInterval -> maxInterval -> params -> Ptr () -> Ptr () -> IO ()
subscribeAttributeOffWaitTimeWithMinInterval_maxInterval_params_subscriptionEstablished_reportHandler mtrBaseClusterOnOff minInterval maxInterval params subscriptionEstablishedHandler reportHandler =
  sendMessage mtrBaseClusterOnOff subscribeAttributeOffWaitTimeWithMinInterval_maxInterval_params_subscriptionEstablished_reportHandlerSelector (toNSNumber minInterval) (toNSNumber maxInterval) (toMTRSubscribeParams params) subscriptionEstablishedHandler reportHandler

-- | @+ readAttributeOffWaitTimeWithAttributeCache:endpoint:queue:completionHandler:@
readAttributeOffWaitTimeWithAttributeCache_endpoint_queue_completionHandler :: (IsMTRAttributeCacheContainer attributeCacheContainer, IsNSNumber endpoint, IsNSObject queue) => attributeCacheContainer -> endpoint -> queue -> Ptr () -> IO ()
readAttributeOffWaitTimeWithAttributeCache_endpoint_queue_completionHandler attributeCacheContainer endpoint queue completionHandler =
  do
    cls' <- getRequiredClass "MTRBaseClusterOnOff"
    sendClassMessage cls' readAttributeOffWaitTimeWithAttributeCache_endpoint_queue_completionHandlerSelector (toMTRAttributeCacheContainer attributeCacheContainer) (toNSNumber endpoint) (toNSObject queue) completionHandler

-- | @- readAttributeStartUpOnOffWithCompletionHandler:@
readAttributeStartUpOnOffWithCompletionHandler :: IsMTRBaseClusterOnOff mtrBaseClusterOnOff => mtrBaseClusterOnOff -> Ptr () -> IO ()
readAttributeStartUpOnOffWithCompletionHandler mtrBaseClusterOnOff completionHandler =
  sendMessage mtrBaseClusterOnOff readAttributeStartUpOnOffWithCompletionHandlerSelector completionHandler

-- | @- writeAttributeStartUpOnOffWithValue:completionHandler:@
writeAttributeStartUpOnOffWithValue_completionHandler :: (IsMTRBaseClusterOnOff mtrBaseClusterOnOff, IsNSNumber value) => mtrBaseClusterOnOff -> value -> Ptr () -> IO ()
writeAttributeStartUpOnOffWithValue_completionHandler mtrBaseClusterOnOff value completionHandler =
  sendMessage mtrBaseClusterOnOff writeAttributeStartUpOnOffWithValue_completionHandlerSelector (toNSNumber value) completionHandler

-- | @- writeAttributeStartUpOnOffWithValue:params:completionHandler:@
writeAttributeStartUpOnOffWithValue_params_completionHandler :: (IsMTRBaseClusterOnOff mtrBaseClusterOnOff, IsNSNumber value, IsMTRWriteParams params) => mtrBaseClusterOnOff -> value -> params -> Ptr () -> IO ()
writeAttributeStartUpOnOffWithValue_params_completionHandler mtrBaseClusterOnOff value params completionHandler =
  sendMessage mtrBaseClusterOnOff writeAttributeStartUpOnOffWithValue_params_completionHandlerSelector (toNSNumber value) (toMTRWriteParams params) completionHandler

-- | @- subscribeAttributeStartUpOnOffWithMinInterval:maxInterval:params:subscriptionEstablished:reportHandler:@
subscribeAttributeStartUpOnOffWithMinInterval_maxInterval_params_subscriptionEstablished_reportHandler :: (IsMTRBaseClusterOnOff mtrBaseClusterOnOff, IsNSNumber minInterval, IsNSNumber maxInterval, IsMTRSubscribeParams params) => mtrBaseClusterOnOff -> minInterval -> maxInterval -> params -> Ptr () -> Ptr () -> IO ()
subscribeAttributeStartUpOnOffWithMinInterval_maxInterval_params_subscriptionEstablished_reportHandler mtrBaseClusterOnOff minInterval maxInterval params subscriptionEstablishedHandler reportHandler =
  sendMessage mtrBaseClusterOnOff subscribeAttributeStartUpOnOffWithMinInterval_maxInterval_params_subscriptionEstablished_reportHandlerSelector (toNSNumber minInterval) (toNSNumber maxInterval) (toMTRSubscribeParams params) subscriptionEstablishedHandler reportHandler

-- | @+ readAttributeStartUpOnOffWithAttributeCache:endpoint:queue:completionHandler:@
readAttributeStartUpOnOffWithAttributeCache_endpoint_queue_completionHandler :: (IsMTRAttributeCacheContainer attributeCacheContainer, IsNSNumber endpoint, IsNSObject queue) => attributeCacheContainer -> endpoint -> queue -> Ptr () -> IO ()
readAttributeStartUpOnOffWithAttributeCache_endpoint_queue_completionHandler attributeCacheContainer endpoint queue completionHandler =
  do
    cls' <- getRequiredClass "MTRBaseClusterOnOff"
    sendClassMessage cls' readAttributeStartUpOnOffWithAttributeCache_endpoint_queue_completionHandlerSelector (toMTRAttributeCacheContainer attributeCacheContainer) (toNSNumber endpoint) (toNSObject queue) completionHandler

-- | @- readAttributeGeneratedCommandListWithCompletionHandler:@
readAttributeGeneratedCommandListWithCompletionHandler :: IsMTRBaseClusterOnOff mtrBaseClusterOnOff => mtrBaseClusterOnOff -> Ptr () -> IO ()
readAttributeGeneratedCommandListWithCompletionHandler mtrBaseClusterOnOff completionHandler =
  sendMessage mtrBaseClusterOnOff readAttributeGeneratedCommandListWithCompletionHandlerSelector completionHandler

-- | @- subscribeAttributeGeneratedCommandListWithMinInterval:maxInterval:params:subscriptionEstablished:reportHandler:@
subscribeAttributeGeneratedCommandListWithMinInterval_maxInterval_params_subscriptionEstablished_reportHandler :: (IsMTRBaseClusterOnOff mtrBaseClusterOnOff, IsNSNumber minInterval, IsNSNumber maxInterval, IsMTRSubscribeParams params) => mtrBaseClusterOnOff -> minInterval -> maxInterval -> params -> Ptr () -> Ptr () -> IO ()
subscribeAttributeGeneratedCommandListWithMinInterval_maxInterval_params_subscriptionEstablished_reportHandler mtrBaseClusterOnOff minInterval maxInterval params subscriptionEstablishedHandler reportHandler =
  sendMessage mtrBaseClusterOnOff subscribeAttributeGeneratedCommandListWithMinInterval_maxInterval_params_subscriptionEstablished_reportHandlerSelector (toNSNumber minInterval) (toNSNumber maxInterval) (toMTRSubscribeParams params) subscriptionEstablishedHandler reportHandler

-- | @+ readAttributeGeneratedCommandListWithAttributeCache:endpoint:queue:completionHandler:@
readAttributeGeneratedCommandListWithAttributeCache_endpoint_queue_completionHandler :: (IsMTRAttributeCacheContainer attributeCacheContainer, IsNSNumber endpoint, IsNSObject queue) => attributeCacheContainer -> endpoint -> queue -> Ptr () -> IO ()
readAttributeGeneratedCommandListWithAttributeCache_endpoint_queue_completionHandler attributeCacheContainer endpoint queue completionHandler =
  do
    cls' <- getRequiredClass "MTRBaseClusterOnOff"
    sendClassMessage cls' readAttributeGeneratedCommandListWithAttributeCache_endpoint_queue_completionHandlerSelector (toMTRAttributeCacheContainer attributeCacheContainer) (toNSNumber endpoint) (toNSObject queue) completionHandler

-- | @- readAttributeAcceptedCommandListWithCompletionHandler:@
readAttributeAcceptedCommandListWithCompletionHandler :: IsMTRBaseClusterOnOff mtrBaseClusterOnOff => mtrBaseClusterOnOff -> Ptr () -> IO ()
readAttributeAcceptedCommandListWithCompletionHandler mtrBaseClusterOnOff completionHandler =
  sendMessage mtrBaseClusterOnOff readAttributeAcceptedCommandListWithCompletionHandlerSelector completionHandler

-- | @- subscribeAttributeAcceptedCommandListWithMinInterval:maxInterval:params:subscriptionEstablished:reportHandler:@
subscribeAttributeAcceptedCommandListWithMinInterval_maxInterval_params_subscriptionEstablished_reportHandler :: (IsMTRBaseClusterOnOff mtrBaseClusterOnOff, IsNSNumber minInterval, IsNSNumber maxInterval, IsMTRSubscribeParams params) => mtrBaseClusterOnOff -> minInterval -> maxInterval -> params -> Ptr () -> Ptr () -> IO ()
subscribeAttributeAcceptedCommandListWithMinInterval_maxInterval_params_subscriptionEstablished_reportHandler mtrBaseClusterOnOff minInterval maxInterval params subscriptionEstablishedHandler reportHandler =
  sendMessage mtrBaseClusterOnOff subscribeAttributeAcceptedCommandListWithMinInterval_maxInterval_params_subscriptionEstablished_reportHandlerSelector (toNSNumber minInterval) (toNSNumber maxInterval) (toMTRSubscribeParams params) subscriptionEstablishedHandler reportHandler

-- | @+ readAttributeAcceptedCommandListWithAttributeCache:endpoint:queue:completionHandler:@
readAttributeAcceptedCommandListWithAttributeCache_endpoint_queue_completionHandler :: (IsMTRAttributeCacheContainer attributeCacheContainer, IsNSNumber endpoint, IsNSObject queue) => attributeCacheContainer -> endpoint -> queue -> Ptr () -> IO ()
readAttributeAcceptedCommandListWithAttributeCache_endpoint_queue_completionHandler attributeCacheContainer endpoint queue completionHandler =
  do
    cls' <- getRequiredClass "MTRBaseClusterOnOff"
    sendClassMessage cls' readAttributeAcceptedCommandListWithAttributeCache_endpoint_queue_completionHandlerSelector (toMTRAttributeCacheContainer attributeCacheContainer) (toNSNumber endpoint) (toNSObject queue) completionHandler

-- | @- readAttributeAttributeListWithCompletionHandler:@
readAttributeAttributeListWithCompletionHandler :: IsMTRBaseClusterOnOff mtrBaseClusterOnOff => mtrBaseClusterOnOff -> Ptr () -> IO ()
readAttributeAttributeListWithCompletionHandler mtrBaseClusterOnOff completionHandler =
  sendMessage mtrBaseClusterOnOff readAttributeAttributeListWithCompletionHandlerSelector completionHandler

-- | @- subscribeAttributeAttributeListWithMinInterval:maxInterval:params:subscriptionEstablished:reportHandler:@
subscribeAttributeAttributeListWithMinInterval_maxInterval_params_subscriptionEstablished_reportHandler :: (IsMTRBaseClusterOnOff mtrBaseClusterOnOff, IsNSNumber minInterval, IsNSNumber maxInterval, IsMTRSubscribeParams params) => mtrBaseClusterOnOff -> minInterval -> maxInterval -> params -> Ptr () -> Ptr () -> IO ()
subscribeAttributeAttributeListWithMinInterval_maxInterval_params_subscriptionEstablished_reportHandler mtrBaseClusterOnOff minInterval maxInterval params subscriptionEstablishedHandler reportHandler =
  sendMessage mtrBaseClusterOnOff subscribeAttributeAttributeListWithMinInterval_maxInterval_params_subscriptionEstablished_reportHandlerSelector (toNSNumber minInterval) (toNSNumber maxInterval) (toMTRSubscribeParams params) subscriptionEstablishedHandler reportHandler

-- | @+ readAttributeAttributeListWithAttributeCache:endpoint:queue:completionHandler:@
readAttributeAttributeListWithAttributeCache_endpoint_queue_completionHandler :: (IsMTRAttributeCacheContainer attributeCacheContainer, IsNSNumber endpoint, IsNSObject queue) => attributeCacheContainer -> endpoint -> queue -> Ptr () -> IO ()
readAttributeAttributeListWithAttributeCache_endpoint_queue_completionHandler attributeCacheContainer endpoint queue completionHandler =
  do
    cls' <- getRequiredClass "MTRBaseClusterOnOff"
    sendClassMessage cls' readAttributeAttributeListWithAttributeCache_endpoint_queue_completionHandlerSelector (toMTRAttributeCacheContainer attributeCacheContainer) (toNSNumber endpoint) (toNSObject queue) completionHandler

-- | @- readAttributeFeatureMapWithCompletionHandler:@
readAttributeFeatureMapWithCompletionHandler :: IsMTRBaseClusterOnOff mtrBaseClusterOnOff => mtrBaseClusterOnOff -> Ptr () -> IO ()
readAttributeFeatureMapWithCompletionHandler mtrBaseClusterOnOff completionHandler =
  sendMessage mtrBaseClusterOnOff readAttributeFeatureMapWithCompletionHandlerSelector completionHandler

-- | @- subscribeAttributeFeatureMapWithMinInterval:maxInterval:params:subscriptionEstablished:reportHandler:@
subscribeAttributeFeatureMapWithMinInterval_maxInterval_params_subscriptionEstablished_reportHandler :: (IsMTRBaseClusterOnOff mtrBaseClusterOnOff, IsNSNumber minInterval, IsNSNumber maxInterval, IsMTRSubscribeParams params) => mtrBaseClusterOnOff -> minInterval -> maxInterval -> params -> Ptr () -> Ptr () -> IO ()
subscribeAttributeFeatureMapWithMinInterval_maxInterval_params_subscriptionEstablished_reportHandler mtrBaseClusterOnOff minInterval maxInterval params subscriptionEstablishedHandler reportHandler =
  sendMessage mtrBaseClusterOnOff subscribeAttributeFeatureMapWithMinInterval_maxInterval_params_subscriptionEstablished_reportHandlerSelector (toNSNumber minInterval) (toNSNumber maxInterval) (toMTRSubscribeParams params) subscriptionEstablishedHandler reportHandler

-- | @+ readAttributeFeatureMapWithAttributeCache:endpoint:queue:completionHandler:@
readAttributeFeatureMapWithAttributeCache_endpoint_queue_completionHandler :: (IsMTRAttributeCacheContainer attributeCacheContainer, IsNSNumber endpoint, IsNSObject queue) => attributeCacheContainer -> endpoint -> queue -> Ptr () -> IO ()
readAttributeFeatureMapWithAttributeCache_endpoint_queue_completionHandler attributeCacheContainer endpoint queue completionHandler =
  do
    cls' <- getRequiredClass "MTRBaseClusterOnOff"
    sendClassMessage cls' readAttributeFeatureMapWithAttributeCache_endpoint_queue_completionHandlerSelector (toMTRAttributeCacheContainer attributeCacheContainer) (toNSNumber endpoint) (toNSObject queue) completionHandler

-- | @- readAttributeClusterRevisionWithCompletionHandler:@
readAttributeClusterRevisionWithCompletionHandler :: IsMTRBaseClusterOnOff mtrBaseClusterOnOff => mtrBaseClusterOnOff -> Ptr () -> IO ()
readAttributeClusterRevisionWithCompletionHandler mtrBaseClusterOnOff completionHandler =
  sendMessage mtrBaseClusterOnOff readAttributeClusterRevisionWithCompletionHandlerSelector completionHandler

-- | @- subscribeAttributeClusterRevisionWithMinInterval:maxInterval:params:subscriptionEstablished:reportHandler:@
subscribeAttributeClusterRevisionWithMinInterval_maxInterval_params_subscriptionEstablished_reportHandler :: (IsMTRBaseClusterOnOff mtrBaseClusterOnOff, IsNSNumber minInterval, IsNSNumber maxInterval, IsMTRSubscribeParams params) => mtrBaseClusterOnOff -> minInterval -> maxInterval -> params -> Ptr () -> Ptr () -> IO ()
subscribeAttributeClusterRevisionWithMinInterval_maxInterval_params_subscriptionEstablished_reportHandler mtrBaseClusterOnOff minInterval maxInterval params subscriptionEstablishedHandler reportHandler =
  sendMessage mtrBaseClusterOnOff subscribeAttributeClusterRevisionWithMinInterval_maxInterval_params_subscriptionEstablished_reportHandlerSelector (toNSNumber minInterval) (toNSNumber maxInterval) (toMTRSubscribeParams params) subscriptionEstablishedHandler reportHandler

-- | @+ readAttributeClusterRevisionWithAttributeCache:endpoint:queue:completionHandler:@
readAttributeClusterRevisionWithAttributeCache_endpoint_queue_completionHandler :: (IsMTRAttributeCacheContainer attributeCacheContainer, IsNSNumber endpoint, IsNSObject queue) => attributeCacheContainer -> endpoint -> queue -> Ptr () -> IO ()
readAttributeClusterRevisionWithAttributeCache_endpoint_queue_completionHandler attributeCacheContainer endpoint queue completionHandler =
  do
    cls' <- getRequiredClass "MTRBaseClusterOnOff"
    sendClassMessage cls' readAttributeClusterRevisionWithAttributeCache_endpoint_queue_completionHandlerSelector (toMTRAttributeCacheContainer attributeCacheContainer) (toNSNumber endpoint) (toNSObject queue) completionHandler

-- | For all instance methods (reads, writes, commands) that take a completion, the completion will be called on the provided queue.
--
-- ObjC selector: @- initWithDevice:endpointID:queue:@
initWithDevice_endpointID_queue :: (IsMTRBaseClusterOnOff mtrBaseClusterOnOff, IsMTRBaseDevice device, IsNSNumber endpointID, IsNSObject queue) => mtrBaseClusterOnOff -> device -> endpointID -> queue -> IO (Id MTRBaseClusterOnOff)
initWithDevice_endpointID_queue mtrBaseClusterOnOff device endpointID queue =
  sendOwnedMessage mtrBaseClusterOnOff initWithDevice_endpointID_queueSelector (toMTRBaseDevice device) (toNSNumber endpointID) (toNSObject queue)

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @offWithParams:completion:@
offWithParams_completionSelector :: Selector '[Id MTROnOffClusterOffParams, Ptr ()] ()
offWithParams_completionSelector = mkSelector "offWithParams:completion:"

-- | @Selector@ for @offWithCompletion:@
offWithCompletionSelector :: Selector '[Ptr ()] ()
offWithCompletionSelector = mkSelector "offWithCompletion:"

-- | @Selector@ for @onWithParams:completion:@
onWithParams_completionSelector :: Selector '[Id MTROnOffClusterOnParams, Ptr ()] ()
onWithParams_completionSelector = mkSelector "onWithParams:completion:"

-- | @Selector@ for @onWithCompletion:@
onWithCompletionSelector :: Selector '[Ptr ()] ()
onWithCompletionSelector = mkSelector "onWithCompletion:"

-- | @Selector@ for @toggleWithParams:completion:@
toggleWithParams_completionSelector :: Selector '[Id MTROnOffClusterToggleParams, Ptr ()] ()
toggleWithParams_completionSelector = mkSelector "toggleWithParams:completion:"

-- | @Selector@ for @toggleWithCompletion:@
toggleWithCompletionSelector :: Selector '[Ptr ()] ()
toggleWithCompletionSelector = mkSelector "toggleWithCompletion:"

-- | @Selector@ for @offWithEffectWithParams:completion:@
offWithEffectWithParams_completionSelector :: Selector '[Id MTROnOffClusterOffWithEffectParams, Ptr ()] ()
offWithEffectWithParams_completionSelector = mkSelector "offWithEffectWithParams:completion:"

-- | @Selector@ for @onWithRecallGlobalSceneWithParams:completion:@
onWithRecallGlobalSceneWithParams_completionSelector :: Selector '[Id MTROnOffClusterOnWithRecallGlobalSceneParams, Ptr ()] ()
onWithRecallGlobalSceneWithParams_completionSelector = mkSelector "onWithRecallGlobalSceneWithParams:completion:"

-- | @Selector@ for @onWithRecallGlobalSceneWithCompletion:@
onWithRecallGlobalSceneWithCompletionSelector :: Selector '[Ptr ()] ()
onWithRecallGlobalSceneWithCompletionSelector = mkSelector "onWithRecallGlobalSceneWithCompletion:"

-- | @Selector@ for @onWithTimedOffWithParams:completion:@
onWithTimedOffWithParams_completionSelector :: Selector '[Id MTROnOffClusterOnWithTimedOffParams, Ptr ()] ()
onWithTimedOffWithParams_completionSelector = mkSelector "onWithTimedOffWithParams:completion:"

-- | @Selector@ for @readAttributeOnOffWithCompletion:@
readAttributeOnOffWithCompletionSelector :: Selector '[Ptr ()] ()
readAttributeOnOffWithCompletionSelector = mkSelector "readAttributeOnOffWithCompletion:"

-- | @Selector@ for @subscribeAttributeOnOffWithParams:subscriptionEstablished:reportHandler:@
subscribeAttributeOnOffWithParams_subscriptionEstablished_reportHandlerSelector :: Selector '[Id MTRSubscribeParams, Ptr (), Ptr ()] ()
subscribeAttributeOnOffWithParams_subscriptionEstablished_reportHandlerSelector = mkSelector "subscribeAttributeOnOffWithParams:subscriptionEstablished:reportHandler:"

-- | @Selector@ for @readAttributeOnOffWithClusterStateCache:endpoint:queue:completion:@
readAttributeOnOffWithClusterStateCache_endpoint_queue_completionSelector :: Selector '[Id MTRClusterStateCacheContainer, Id NSNumber, Id NSObject, Ptr ()] ()
readAttributeOnOffWithClusterStateCache_endpoint_queue_completionSelector = mkSelector "readAttributeOnOffWithClusterStateCache:endpoint:queue:completion:"

-- | @Selector@ for @readAttributeGlobalSceneControlWithCompletion:@
readAttributeGlobalSceneControlWithCompletionSelector :: Selector '[Ptr ()] ()
readAttributeGlobalSceneControlWithCompletionSelector = mkSelector "readAttributeGlobalSceneControlWithCompletion:"

-- | @Selector@ for @subscribeAttributeGlobalSceneControlWithParams:subscriptionEstablished:reportHandler:@
subscribeAttributeGlobalSceneControlWithParams_subscriptionEstablished_reportHandlerSelector :: Selector '[Id MTRSubscribeParams, Ptr (), Ptr ()] ()
subscribeAttributeGlobalSceneControlWithParams_subscriptionEstablished_reportHandlerSelector = mkSelector "subscribeAttributeGlobalSceneControlWithParams:subscriptionEstablished:reportHandler:"

-- | @Selector@ for @readAttributeGlobalSceneControlWithClusterStateCache:endpoint:queue:completion:@
readAttributeGlobalSceneControlWithClusterStateCache_endpoint_queue_completionSelector :: Selector '[Id MTRClusterStateCacheContainer, Id NSNumber, Id NSObject, Ptr ()] ()
readAttributeGlobalSceneControlWithClusterStateCache_endpoint_queue_completionSelector = mkSelector "readAttributeGlobalSceneControlWithClusterStateCache:endpoint:queue:completion:"

-- | @Selector@ for @readAttributeOnTimeWithCompletion:@
readAttributeOnTimeWithCompletionSelector :: Selector '[Ptr ()] ()
readAttributeOnTimeWithCompletionSelector = mkSelector "readAttributeOnTimeWithCompletion:"

-- | @Selector@ for @writeAttributeOnTimeWithValue:completion:@
writeAttributeOnTimeWithValue_completionSelector :: Selector '[Id NSNumber, Ptr ()] ()
writeAttributeOnTimeWithValue_completionSelector = mkSelector "writeAttributeOnTimeWithValue:completion:"

-- | @Selector@ for @writeAttributeOnTimeWithValue:params:completion:@
writeAttributeOnTimeWithValue_params_completionSelector :: Selector '[Id NSNumber, Id MTRWriteParams, Ptr ()] ()
writeAttributeOnTimeWithValue_params_completionSelector = mkSelector "writeAttributeOnTimeWithValue:params:completion:"

-- | @Selector@ for @subscribeAttributeOnTimeWithParams:subscriptionEstablished:reportHandler:@
subscribeAttributeOnTimeWithParams_subscriptionEstablished_reportHandlerSelector :: Selector '[Id MTRSubscribeParams, Ptr (), Ptr ()] ()
subscribeAttributeOnTimeWithParams_subscriptionEstablished_reportHandlerSelector = mkSelector "subscribeAttributeOnTimeWithParams:subscriptionEstablished:reportHandler:"

-- | @Selector@ for @readAttributeOnTimeWithClusterStateCache:endpoint:queue:completion:@
readAttributeOnTimeWithClusterStateCache_endpoint_queue_completionSelector :: Selector '[Id MTRClusterStateCacheContainer, Id NSNumber, Id NSObject, Ptr ()] ()
readAttributeOnTimeWithClusterStateCache_endpoint_queue_completionSelector = mkSelector "readAttributeOnTimeWithClusterStateCache:endpoint:queue:completion:"

-- | @Selector@ for @readAttributeOffWaitTimeWithCompletion:@
readAttributeOffWaitTimeWithCompletionSelector :: Selector '[Ptr ()] ()
readAttributeOffWaitTimeWithCompletionSelector = mkSelector "readAttributeOffWaitTimeWithCompletion:"

-- | @Selector@ for @writeAttributeOffWaitTimeWithValue:completion:@
writeAttributeOffWaitTimeWithValue_completionSelector :: Selector '[Id NSNumber, Ptr ()] ()
writeAttributeOffWaitTimeWithValue_completionSelector = mkSelector "writeAttributeOffWaitTimeWithValue:completion:"

-- | @Selector@ for @writeAttributeOffWaitTimeWithValue:params:completion:@
writeAttributeOffWaitTimeWithValue_params_completionSelector :: Selector '[Id NSNumber, Id MTRWriteParams, Ptr ()] ()
writeAttributeOffWaitTimeWithValue_params_completionSelector = mkSelector "writeAttributeOffWaitTimeWithValue:params:completion:"

-- | @Selector@ for @subscribeAttributeOffWaitTimeWithParams:subscriptionEstablished:reportHandler:@
subscribeAttributeOffWaitTimeWithParams_subscriptionEstablished_reportHandlerSelector :: Selector '[Id MTRSubscribeParams, Ptr (), Ptr ()] ()
subscribeAttributeOffWaitTimeWithParams_subscriptionEstablished_reportHandlerSelector = mkSelector "subscribeAttributeOffWaitTimeWithParams:subscriptionEstablished:reportHandler:"

-- | @Selector@ for @readAttributeOffWaitTimeWithClusterStateCache:endpoint:queue:completion:@
readAttributeOffWaitTimeWithClusterStateCache_endpoint_queue_completionSelector :: Selector '[Id MTRClusterStateCacheContainer, Id NSNumber, Id NSObject, Ptr ()] ()
readAttributeOffWaitTimeWithClusterStateCache_endpoint_queue_completionSelector = mkSelector "readAttributeOffWaitTimeWithClusterStateCache:endpoint:queue:completion:"

-- | @Selector@ for @readAttributeStartUpOnOffWithCompletion:@
readAttributeStartUpOnOffWithCompletionSelector :: Selector '[Ptr ()] ()
readAttributeStartUpOnOffWithCompletionSelector = mkSelector "readAttributeStartUpOnOffWithCompletion:"

-- | @Selector@ for @writeAttributeStartUpOnOffWithValue:completion:@
writeAttributeStartUpOnOffWithValue_completionSelector :: Selector '[Id NSNumber, Ptr ()] ()
writeAttributeStartUpOnOffWithValue_completionSelector = mkSelector "writeAttributeStartUpOnOffWithValue:completion:"

-- | @Selector@ for @writeAttributeStartUpOnOffWithValue:params:completion:@
writeAttributeStartUpOnOffWithValue_params_completionSelector :: Selector '[Id NSNumber, Id MTRWriteParams, Ptr ()] ()
writeAttributeStartUpOnOffWithValue_params_completionSelector = mkSelector "writeAttributeStartUpOnOffWithValue:params:completion:"

-- | @Selector@ for @subscribeAttributeStartUpOnOffWithParams:subscriptionEstablished:reportHandler:@
subscribeAttributeStartUpOnOffWithParams_subscriptionEstablished_reportHandlerSelector :: Selector '[Id MTRSubscribeParams, Ptr (), Ptr ()] ()
subscribeAttributeStartUpOnOffWithParams_subscriptionEstablished_reportHandlerSelector = mkSelector "subscribeAttributeStartUpOnOffWithParams:subscriptionEstablished:reportHandler:"

-- | @Selector@ for @readAttributeStartUpOnOffWithClusterStateCache:endpoint:queue:completion:@
readAttributeStartUpOnOffWithClusterStateCache_endpoint_queue_completionSelector :: Selector '[Id MTRClusterStateCacheContainer, Id NSNumber, Id NSObject, Ptr ()] ()
readAttributeStartUpOnOffWithClusterStateCache_endpoint_queue_completionSelector = mkSelector "readAttributeStartUpOnOffWithClusterStateCache:endpoint:queue:completion:"

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
initSelector :: Selector '[] (Id MTRBaseClusterOnOff)
initSelector = mkSelector "init"

-- | @Selector@ for @new@
newSelector :: Selector '[] (Id MTRBaseClusterOnOff)
newSelector = mkSelector "new"

-- | @Selector@ for @initWithDevice:endpoint:queue:@
initWithDevice_endpoint_queueSelector :: Selector '[Id MTRBaseDevice, CUShort, Id NSObject] (Id MTRBaseClusterOnOff)
initWithDevice_endpoint_queueSelector = mkSelector "initWithDevice:endpoint:queue:"

-- | @Selector@ for @offWithParams:completionHandler:@
offWithParams_completionHandlerSelector :: Selector '[Id MTROnOffClusterOffParams, Ptr ()] ()
offWithParams_completionHandlerSelector = mkSelector "offWithParams:completionHandler:"

-- | @Selector@ for @offWithCompletionHandler:@
offWithCompletionHandlerSelector :: Selector '[Ptr ()] ()
offWithCompletionHandlerSelector = mkSelector "offWithCompletionHandler:"

-- | @Selector@ for @onWithParams:completionHandler:@
onWithParams_completionHandlerSelector :: Selector '[Id MTROnOffClusterOnParams, Ptr ()] ()
onWithParams_completionHandlerSelector = mkSelector "onWithParams:completionHandler:"

-- | @Selector@ for @onWithCompletionHandler:@
onWithCompletionHandlerSelector :: Selector '[Ptr ()] ()
onWithCompletionHandlerSelector = mkSelector "onWithCompletionHandler:"

-- | @Selector@ for @toggleWithParams:completionHandler:@
toggleWithParams_completionHandlerSelector :: Selector '[Id MTROnOffClusterToggleParams, Ptr ()] ()
toggleWithParams_completionHandlerSelector = mkSelector "toggleWithParams:completionHandler:"

-- | @Selector@ for @toggleWithCompletionHandler:@
toggleWithCompletionHandlerSelector :: Selector '[Ptr ()] ()
toggleWithCompletionHandlerSelector = mkSelector "toggleWithCompletionHandler:"

-- | @Selector@ for @offWithEffectWithParams:completionHandler:@
offWithEffectWithParams_completionHandlerSelector :: Selector '[Id MTROnOffClusterOffWithEffectParams, Ptr ()] ()
offWithEffectWithParams_completionHandlerSelector = mkSelector "offWithEffectWithParams:completionHandler:"

-- | @Selector@ for @onWithRecallGlobalSceneWithParams:completionHandler:@
onWithRecallGlobalSceneWithParams_completionHandlerSelector :: Selector '[Id MTROnOffClusterOnWithRecallGlobalSceneParams, Ptr ()] ()
onWithRecallGlobalSceneWithParams_completionHandlerSelector = mkSelector "onWithRecallGlobalSceneWithParams:completionHandler:"

-- | @Selector@ for @onWithRecallGlobalSceneWithCompletionHandler:@
onWithRecallGlobalSceneWithCompletionHandlerSelector :: Selector '[Ptr ()] ()
onWithRecallGlobalSceneWithCompletionHandlerSelector = mkSelector "onWithRecallGlobalSceneWithCompletionHandler:"

-- | @Selector@ for @onWithTimedOffWithParams:completionHandler:@
onWithTimedOffWithParams_completionHandlerSelector :: Selector '[Id MTROnOffClusterOnWithTimedOffParams, Ptr ()] ()
onWithTimedOffWithParams_completionHandlerSelector = mkSelector "onWithTimedOffWithParams:completionHandler:"

-- | @Selector@ for @readAttributeOnOffWithCompletionHandler:@
readAttributeOnOffWithCompletionHandlerSelector :: Selector '[Ptr ()] ()
readAttributeOnOffWithCompletionHandlerSelector = mkSelector "readAttributeOnOffWithCompletionHandler:"

-- | @Selector@ for @subscribeAttributeOnOffWithMinInterval:maxInterval:params:subscriptionEstablished:reportHandler:@
subscribeAttributeOnOffWithMinInterval_maxInterval_params_subscriptionEstablished_reportHandlerSelector :: Selector '[Id NSNumber, Id NSNumber, Id MTRSubscribeParams, Ptr (), Ptr ()] ()
subscribeAttributeOnOffWithMinInterval_maxInterval_params_subscriptionEstablished_reportHandlerSelector = mkSelector "subscribeAttributeOnOffWithMinInterval:maxInterval:params:subscriptionEstablished:reportHandler:"

-- | @Selector@ for @readAttributeOnOffWithAttributeCache:endpoint:queue:completionHandler:@
readAttributeOnOffWithAttributeCache_endpoint_queue_completionHandlerSelector :: Selector '[Id MTRAttributeCacheContainer, Id NSNumber, Id NSObject, Ptr ()] ()
readAttributeOnOffWithAttributeCache_endpoint_queue_completionHandlerSelector = mkSelector "readAttributeOnOffWithAttributeCache:endpoint:queue:completionHandler:"

-- | @Selector@ for @readAttributeGlobalSceneControlWithCompletionHandler:@
readAttributeGlobalSceneControlWithCompletionHandlerSelector :: Selector '[Ptr ()] ()
readAttributeGlobalSceneControlWithCompletionHandlerSelector = mkSelector "readAttributeGlobalSceneControlWithCompletionHandler:"

-- | @Selector@ for @subscribeAttributeGlobalSceneControlWithMinInterval:maxInterval:params:subscriptionEstablished:reportHandler:@
subscribeAttributeGlobalSceneControlWithMinInterval_maxInterval_params_subscriptionEstablished_reportHandlerSelector :: Selector '[Id NSNumber, Id NSNumber, Id MTRSubscribeParams, Ptr (), Ptr ()] ()
subscribeAttributeGlobalSceneControlWithMinInterval_maxInterval_params_subscriptionEstablished_reportHandlerSelector = mkSelector "subscribeAttributeGlobalSceneControlWithMinInterval:maxInterval:params:subscriptionEstablished:reportHandler:"

-- | @Selector@ for @readAttributeGlobalSceneControlWithAttributeCache:endpoint:queue:completionHandler:@
readAttributeGlobalSceneControlWithAttributeCache_endpoint_queue_completionHandlerSelector :: Selector '[Id MTRAttributeCacheContainer, Id NSNumber, Id NSObject, Ptr ()] ()
readAttributeGlobalSceneControlWithAttributeCache_endpoint_queue_completionHandlerSelector = mkSelector "readAttributeGlobalSceneControlWithAttributeCache:endpoint:queue:completionHandler:"

-- | @Selector@ for @readAttributeOnTimeWithCompletionHandler:@
readAttributeOnTimeWithCompletionHandlerSelector :: Selector '[Ptr ()] ()
readAttributeOnTimeWithCompletionHandlerSelector = mkSelector "readAttributeOnTimeWithCompletionHandler:"

-- | @Selector@ for @writeAttributeOnTimeWithValue:completionHandler:@
writeAttributeOnTimeWithValue_completionHandlerSelector :: Selector '[Id NSNumber, Ptr ()] ()
writeAttributeOnTimeWithValue_completionHandlerSelector = mkSelector "writeAttributeOnTimeWithValue:completionHandler:"

-- | @Selector@ for @writeAttributeOnTimeWithValue:params:completionHandler:@
writeAttributeOnTimeWithValue_params_completionHandlerSelector :: Selector '[Id NSNumber, Id MTRWriteParams, Ptr ()] ()
writeAttributeOnTimeWithValue_params_completionHandlerSelector = mkSelector "writeAttributeOnTimeWithValue:params:completionHandler:"

-- | @Selector@ for @subscribeAttributeOnTimeWithMinInterval:maxInterval:params:subscriptionEstablished:reportHandler:@
subscribeAttributeOnTimeWithMinInterval_maxInterval_params_subscriptionEstablished_reportHandlerSelector :: Selector '[Id NSNumber, Id NSNumber, Id MTRSubscribeParams, Ptr (), Ptr ()] ()
subscribeAttributeOnTimeWithMinInterval_maxInterval_params_subscriptionEstablished_reportHandlerSelector = mkSelector "subscribeAttributeOnTimeWithMinInterval:maxInterval:params:subscriptionEstablished:reportHandler:"

-- | @Selector@ for @readAttributeOnTimeWithAttributeCache:endpoint:queue:completionHandler:@
readAttributeOnTimeWithAttributeCache_endpoint_queue_completionHandlerSelector :: Selector '[Id MTRAttributeCacheContainer, Id NSNumber, Id NSObject, Ptr ()] ()
readAttributeOnTimeWithAttributeCache_endpoint_queue_completionHandlerSelector = mkSelector "readAttributeOnTimeWithAttributeCache:endpoint:queue:completionHandler:"

-- | @Selector@ for @readAttributeOffWaitTimeWithCompletionHandler:@
readAttributeOffWaitTimeWithCompletionHandlerSelector :: Selector '[Ptr ()] ()
readAttributeOffWaitTimeWithCompletionHandlerSelector = mkSelector "readAttributeOffWaitTimeWithCompletionHandler:"

-- | @Selector@ for @writeAttributeOffWaitTimeWithValue:completionHandler:@
writeAttributeOffWaitTimeWithValue_completionHandlerSelector :: Selector '[Id NSNumber, Ptr ()] ()
writeAttributeOffWaitTimeWithValue_completionHandlerSelector = mkSelector "writeAttributeOffWaitTimeWithValue:completionHandler:"

-- | @Selector@ for @writeAttributeOffWaitTimeWithValue:params:completionHandler:@
writeAttributeOffWaitTimeWithValue_params_completionHandlerSelector :: Selector '[Id NSNumber, Id MTRWriteParams, Ptr ()] ()
writeAttributeOffWaitTimeWithValue_params_completionHandlerSelector = mkSelector "writeAttributeOffWaitTimeWithValue:params:completionHandler:"

-- | @Selector@ for @subscribeAttributeOffWaitTimeWithMinInterval:maxInterval:params:subscriptionEstablished:reportHandler:@
subscribeAttributeOffWaitTimeWithMinInterval_maxInterval_params_subscriptionEstablished_reportHandlerSelector :: Selector '[Id NSNumber, Id NSNumber, Id MTRSubscribeParams, Ptr (), Ptr ()] ()
subscribeAttributeOffWaitTimeWithMinInterval_maxInterval_params_subscriptionEstablished_reportHandlerSelector = mkSelector "subscribeAttributeOffWaitTimeWithMinInterval:maxInterval:params:subscriptionEstablished:reportHandler:"

-- | @Selector@ for @readAttributeOffWaitTimeWithAttributeCache:endpoint:queue:completionHandler:@
readAttributeOffWaitTimeWithAttributeCache_endpoint_queue_completionHandlerSelector :: Selector '[Id MTRAttributeCacheContainer, Id NSNumber, Id NSObject, Ptr ()] ()
readAttributeOffWaitTimeWithAttributeCache_endpoint_queue_completionHandlerSelector = mkSelector "readAttributeOffWaitTimeWithAttributeCache:endpoint:queue:completionHandler:"

-- | @Selector@ for @readAttributeStartUpOnOffWithCompletionHandler:@
readAttributeStartUpOnOffWithCompletionHandlerSelector :: Selector '[Ptr ()] ()
readAttributeStartUpOnOffWithCompletionHandlerSelector = mkSelector "readAttributeStartUpOnOffWithCompletionHandler:"

-- | @Selector@ for @writeAttributeStartUpOnOffWithValue:completionHandler:@
writeAttributeStartUpOnOffWithValue_completionHandlerSelector :: Selector '[Id NSNumber, Ptr ()] ()
writeAttributeStartUpOnOffWithValue_completionHandlerSelector = mkSelector "writeAttributeStartUpOnOffWithValue:completionHandler:"

-- | @Selector@ for @writeAttributeStartUpOnOffWithValue:params:completionHandler:@
writeAttributeStartUpOnOffWithValue_params_completionHandlerSelector :: Selector '[Id NSNumber, Id MTRWriteParams, Ptr ()] ()
writeAttributeStartUpOnOffWithValue_params_completionHandlerSelector = mkSelector "writeAttributeStartUpOnOffWithValue:params:completionHandler:"

-- | @Selector@ for @subscribeAttributeStartUpOnOffWithMinInterval:maxInterval:params:subscriptionEstablished:reportHandler:@
subscribeAttributeStartUpOnOffWithMinInterval_maxInterval_params_subscriptionEstablished_reportHandlerSelector :: Selector '[Id NSNumber, Id NSNumber, Id MTRSubscribeParams, Ptr (), Ptr ()] ()
subscribeAttributeStartUpOnOffWithMinInterval_maxInterval_params_subscriptionEstablished_reportHandlerSelector = mkSelector "subscribeAttributeStartUpOnOffWithMinInterval:maxInterval:params:subscriptionEstablished:reportHandler:"

-- | @Selector@ for @readAttributeStartUpOnOffWithAttributeCache:endpoint:queue:completionHandler:@
readAttributeStartUpOnOffWithAttributeCache_endpoint_queue_completionHandlerSelector :: Selector '[Id MTRAttributeCacheContainer, Id NSNumber, Id NSObject, Ptr ()] ()
readAttributeStartUpOnOffWithAttributeCache_endpoint_queue_completionHandlerSelector = mkSelector "readAttributeStartUpOnOffWithAttributeCache:endpoint:queue:completionHandler:"

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
initWithDevice_endpointID_queueSelector :: Selector '[Id MTRBaseDevice, Id NSNumber, Id NSObject] (Id MTRBaseClusterOnOff)
initWithDevice_endpointID_queueSelector = mkSelector "initWithDevice:endpointID:queue:"

