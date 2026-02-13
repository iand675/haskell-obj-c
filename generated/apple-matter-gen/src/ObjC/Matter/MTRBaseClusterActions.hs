{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Cluster Actions
--
-- This cluster provides a standardized way for a Node (typically a Bridge, but could be any Node) to expose action information.
--
-- Generated bindings for @MTRBaseClusterActions@.
module ObjC.Matter.MTRBaseClusterActions
  ( MTRBaseClusterActions
  , IsMTRBaseClusterActions(..)
  , instantActionWithParams_completion
  , instantActionWithTransitionWithParams_completion
  , startActionWithParams_completion
  , startActionWithDurationWithParams_completion
  , stopActionWithParams_completion
  , pauseActionWithParams_completion
  , pauseActionWithDurationWithParams_completion
  , resumeActionWithParams_completion
  , enableActionWithParams_completion
  , enableActionWithDurationWithParams_completion
  , disableActionWithParams_completion
  , disableActionWithDurationWithParams_completion
  , readAttributeActionListWithCompletion
  , subscribeAttributeActionListWithParams_subscriptionEstablished_reportHandler
  , readAttributeActionListWithClusterStateCache_endpoint_queue_completion
  , readAttributeEndpointListsWithCompletion
  , subscribeAttributeEndpointListsWithParams_subscriptionEstablished_reportHandler
  , readAttributeEndpointListsWithClusterStateCache_endpoint_queue_completion
  , readAttributeSetupURLWithCompletion
  , subscribeAttributeSetupURLWithParams_subscriptionEstablished_reportHandler
  , readAttributeSetupURLWithClusterStateCache_endpoint_queue_completion
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
  , instantActionWithParams_completionHandler
  , instantActionWithTransitionWithParams_completionHandler
  , startActionWithParams_completionHandler
  , startActionWithDurationWithParams_completionHandler
  , stopActionWithParams_completionHandler
  , pauseActionWithParams_completionHandler
  , pauseActionWithDurationWithParams_completionHandler
  , resumeActionWithParams_completionHandler
  , enableActionWithParams_completionHandler
  , enableActionWithDurationWithParams_completionHandler
  , disableActionWithParams_completionHandler
  , disableActionWithDurationWithParams_completionHandler
  , readAttributeActionListWithCompletionHandler
  , subscribeAttributeActionListWithMinInterval_maxInterval_params_subscriptionEstablished_reportHandler
  , readAttributeActionListWithAttributeCache_endpoint_queue_completionHandler
  , readAttributeEndpointListsWithCompletionHandler
  , subscribeAttributeEndpointListsWithMinInterval_maxInterval_params_subscriptionEstablished_reportHandler
  , readAttributeEndpointListsWithAttributeCache_endpoint_queue_completionHandler
  , readAttributeSetupURLWithCompletionHandler
  , subscribeAttributeSetupURLWithMinInterval_maxInterval_params_subscriptionEstablished_reportHandler
  , readAttributeSetupURLWithAttributeCache_endpoint_queue_completionHandler
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
  , disableActionWithDurationWithParams_completionHandlerSelector
  , disableActionWithDurationWithParams_completionSelector
  , disableActionWithParams_completionHandlerSelector
  , disableActionWithParams_completionSelector
  , enableActionWithDurationWithParams_completionHandlerSelector
  , enableActionWithDurationWithParams_completionSelector
  , enableActionWithParams_completionHandlerSelector
  , enableActionWithParams_completionSelector
  , initSelector
  , initWithDevice_endpointID_queueSelector
  , initWithDevice_endpoint_queueSelector
  , instantActionWithParams_completionHandlerSelector
  , instantActionWithParams_completionSelector
  , instantActionWithTransitionWithParams_completionHandlerSelector
  , instantActionWithTransitionWithParams_completionSelector
  , newSelector
  , pauseActionWithDurationWithParams_completionHandlerSelector
  , pauseActionWithDurationWithParams_completionSelector
  , pauseActionWithParams_completionHandlerSelector
  , pauseActionWithParams_completionSelector
  , readAttributeAcceptedCommandListWithAttributeCache_endpoint_queue_completionHandlerSelector
  , readAttributeAcceptedCommandListWithClusterStateCache_endpoint_queue_completionSelector
  , readAttributeAcceptedCommandListWithCompletionHandlerSelector
  , readAttributeAcceptedCommandListWithCompletionSelector
  , readAttributeActionListWithAttributeCache_endpoint_queue_completionHandlerSelector
  , readAttributeActionListWithClusterStateCache_endpoint_queue_completionSelector
  , readAttributeActionListWithCompletionHandlerSelector
  , readAttributeActionListWithCompletionSelector
  , readAttributeAttributeListWithAttributeCache_endpoint_queue_completionHandlerSelector
  , readAttributeAttributeListWithClusterStateCache_endpoint_queue_completionSelector
  , readAttributeAttributeListWithCompletionHandlerSelector
  , readAttributeAttributeListWithCompletionSelector
  , readAttributeClusterRevisionWithAttributeCache_endpoint_queue_completionHandlerSelector
  , readAttributeClusterRevisionWithClusterStateCache_endpoint_queue_completionSelector
  , readAttributeClusterRevisionWithCompletionHandlerSelector
  , readAttributeClusterRevisionWithCompletionSelector
  , readAttributeEndpointListsWithAttributeCache_endpoint_queue_completionHandlerSelector
  , readAttributeEndpointListsWithClusterStateCache_endpoint_queue_completionSelector
  , readAttributeEndpointListsWithCompletionHandlerSelector
  , readAttributeEndpointListsWithCompletionSelector
  , readAttributeFeatureMapWithAttributeCache_endpoint_queue_completionHandlerSelector
  , readAttributeFeatureMapWithClusterStateCache_endpoint_queue_completionSelector
  , readAttributeFeatureMapWithCompletionHandlerSelector
  , readAttributeFeatureMapWithCompletionSelector
  , readAttributeGeneratedCommandListWithAttributeCache_endpoint_queue_completionHandlerSelector
  , readAttributeGeneratedCommandListWithClusterStateCache_endpoint_queue_completionSelector
  , readAttributeGeneratedCommandListWithCompletionHandlerSelector
  , readAttributeGeneratedCommandListWithCompletionSelector
  , readAttributeSetupURLWithAttributeCache_endpoint_queue_completionHandlerSelector
  , readAttributeSetupURLWithClusterStateCache_endpoint_queue_completionSelector
  , readAttributeSetupURLWithCompletionHandlerSelector
  , readAttributeSetupURLWithCompletionSelector
  , resumeActionWithParams_completionHandlerSelector
  , resumeActionWithParams_completionSelector
  , startActionWithDurationWithParams_completionHandlerSelector
  , startActionWithDurationWithParams_completionSelector
  , startActionWithParams_completionHandlerSelector
  , startActionWithParams_completionSelector
  , stopActionWithParams_completionHandlerSelector
  , stopActionWithParams_completionSelector
  , subscribeAttributeAcceptedCommandListWithMinInterval_maxInterval_params_subscriptionEstablished_reportHandlerSelector
  , subscribeAttributeAcceptedCommandListWithParams_subscriptionEstablished_reportHandlerSelector
  , subscribeAttributeActionListWithMinInterval_maxInterval_params_subscriptionEstablished_reportHandlerSelector
  , subscribeAttributeActionListWithParams_subscriptionEstablished_reportHandlerSelector
  , subscribeAttributeAttributeListWithMinInterval_maxInterval_params_subscriptionEstablished_reportHandlerSelector
  , subscribeAttributeAttributeListWithParams_subscriptionEstablished_reportHandlerSelector
  , subscribeAttributeClusterRevisionWithMinInterval_maxInterval_params_subscriptionEstablished_reportHandlerSelector
  , subscribeAttributeClusterRevisionWithParams_subscriptionEstablished_reportHandlerSelector
  , subscribeAttributeEndpointListsWithMinInterval_maxInterval_params_subscriptionEstablished_reportHandlerSelector
  , subscribeAttributeEndpointListsWithParams_subscriptionEstablished_reportHandlerSelector
  , subscribeAttributeFeatureMapWithMinInterval_maxInterval_params_subscriptionEstablished_reportHandlerSelector
  , subscribeAttributeFeatureMapWithParams_subscriptionEstablished_reportHandlerSelector
  , subscribeAttributeGeneratedCommandListWithMinInterval_maxInterval_params_subscriptionEstablished_reportHandlerSelector
  , subscribeAttributeGeneratedCommandListWithParams_subscriptionEstablished_reportHandlerSelector
  , subscribeAttributeSetupURLWithMinInterval_maxInterval_params_subscriptionEstablished_reportHandlerSelector
  , subscribeAttributeSetupURLWithParams_subscriptionEstablished_reportHandlerSelector


  ) where

import Foreign.Ptr (Ptr, FunPtr)
import Foreign.C.Types

import ObjC.Runtime.Types
import ObjC.Runtime.Message (sendMessage, sendOwnedMessage, sendClassMessage, sendOwnedClassMessage)
import ObjC.Runtime.Selector (mkSelector)
import ObjC.Runtime.Class (getRequiredClass)

import ObjC.Matter.Internal.Classes
import ObjC.Foundation.Internal.Classes

-- | Command InstantAction
--
-- This command is used to trigger an instantaneous action.
--
-- ObjC selector: @- instantActionWithParams:completion:@
instantActionWithParams_completion :: (IsMTRBaseClusterActions mtrBaseClusterActions, IsMTRActionsClusterInstantActionParams params) => mtrBaseClusterActions -> params -> Ptr () -> IO ()
instantActionWithParams_completion mtrBaseClusterActions params completion =
  sendMessage mtrBaseClusterActions instantActionWithParams_completionSelector (toMTRActionsClusterInstantActionParams params) completion

-- | Command InstantActionWithTransition
--
-- This command is used to trigger an instantaneous action with a transition over a given time.
--
-- ObjC selector: @- instantActionWithTransitionWithParams:completion:@
instantActionWithTransitionWithParams_completion :: (IsMTRBaseClusterActions mtrBaseClusterActions, IsMTRActionsClusterInstantActionWithTransitionParams params) => mtrBaseClusterActions -> params -> Ptr () -> IO ()
instantActionWithTransitionWithParams_completion mtrBaseClusterActions params completion =
  sendMessage mtrBaseClusterActions instantActionWithTransitionWithParams_completionSelector (toMTRActionsClusterInstantActionWithTransitionParams params) completion

-- | Command StartAction
--
-- This command is used to trigger the commencement of an action.
--
-- ObjC selector: @- startActionWithParams:completion:@
startActionWithParams_completion :: (IsMTRBaseClusterActions mtrBaseClusterActions, IsMTRActionsClusterStartActionParams params) => mtrBaseClusterActions -> params -> Ptr () -> IO ()
startActionWithParams_completion mtrBaseClusterActions params completion =
  sendMessage mtrBaseClusterActions startActionWithParams_completionSelector (toMTRActionsClusterStartActionParams params) completion

-- | Command StartActionWithDuration
--
-- This command is used to trigger the commencement of an action with a duration.
--
-- ObjC selector: @- startActionWithDurationWithParams:completion:@
startActionWithDurationWithParams_completion :: (IsMTRBaseClusterActions mtrBaseClusterActions, IsMTRActionsClusterStartActionWithDurationParams params) => mtrBaseClusterActions -> params -> Ptr () -> IO ()
startActionWithDurationWithParams_completion mtrBaseClusterActions params completion =
  sendMessage mtrBaseClusterActions startActionWithDurationWithParams_completionSelector (toMTRActionsClusterStartActionWithDurationParams params) completion

-- | Command StopAction
--
-- This command is used to stop an action.
--
-- ObjC selector: @- stopActionWithParams:completion:@
stopActionWithParams_completion :: (IsMTRBaseClusterActions mtrBaseClusterActions, IsMTRActionsClusterStopActionParams params) => mtrBaseClusterActions -> params -> Ptr () -> IO ()
stopActionWithParams_completion mtrBaseClusterActions params completion =
  sendMessage mtrBaseClusterActions stopActionWithParams_completionSelector (toMTRActionsClusterStopActionParams params) completion

-- | Command PauseAction
--
-- This command is used to pause an action.
--
-- ObjC selector: @- pauseActionWithParams:completion:@
pauseActionWithParams_completion :: (IsMTRBaseClusterActions mtrBaseClusterActions, IsMTRActionsClusterPauseActionParams params) => mtrBaseClusterActions -> params -> Ptr () -> IO ()
pauseActionWithParams_completion mtrBaseClusterActions params completion =
  sendMessage mtrBaseClusterActions pauseActionWithParams_completionSelector (toMTRActionsClusterPauseActionParams params) completion

-- | Command PauseActionWithDuration
--
-- This command is used to pause an action with a duration.
--
-- ObjC selector: @- pauseActionWithDurationWithParams:completion:@
pauseActionWithDurationWithParams_completion :: (IsMTRBaseClusterActions mtrBaseClusterActions, IsMTRActionsClusterPauseActionWithDurationParams params) => mtrBaseClusterActions -> params -> Ptr () -> IO ()
pauseActionWithDurationWithParams_completion mtrBaseClusterActions params completion =
  sendMessage mtrBaseClusterActions pauseActionWithDurationWithParams_completionSelector (toMTRActionsClusterPauseActionWithDurationParams params) completion

-- | Command ResumeAction
--
-- This command is used to resume an action.
--
-- ObjC selector: @- resumeActionWithParams:completion:@
resumeActionWithParams_completion :: (IsMTRBaseClusterActions mtrBaseClusterActions, IsMTRActionsClusterResumeActionParams params) => mtrBaseClusterActions -> params -> Ptr () -> IO ()
resumeActionWithParams_completion mtrBaseClusterActions params completion =
  sendMessage mtrBaseClusterActions resumeActionWithParams_completionSelector (toMTRActionsClusterResumeActionParams params) completion

-- | Command EnableAction
--
-- This command is used to enable an action.
--
-- ObjC selector: @- enableActionWithParams:completion:@
enableActionWithParams_completion :: (IsMTRBaseClusterActions mtrBaseClusterActions, IsMTRActionsClusterEnableActionParams params) => mtrBaseClusterActions -> params -> Ptr () -> IO ()
enableActionWithParams_completion mtrBaseClusterActions params completion =
  sendMessage mtrBaseClusterActions enableActionWithParams_completionSelector (toMTRActionsClusterEnableActionParams params) completion

-- | Command EnableActionWithDuration
--
-- This command is used to enable an action with a duration.
--
-- ObjC selector: @- enableActionWithDurationWithParams:completion:@
enableActionWithDurationWithParams_completion :: (IsMTRBaseClusterActions mtrBaseClusterActions, IsMTRActionsClusterEnableActionWithDurationParams params) => mtrBaseClusterActions -> params -> Ptr () -> IO ()
enableActionWithDurationWithParams_completion mtrBaseClusterActions params completion =
  sendMessage mtrBaseClusterActions enableActionWithDurationWithParams_completionSelector (toMTRActionsClusterEnableActionWithDurationParams params) completion

-- | Command DisableAction
--
-- This command is used to disable an action.
--
-- ObjC selector: @- disableActionWithParams:completion:@
disableActionWithParams_completion :: (IsMTRBaseClusterActions mtrBaseClusterActions, IsMTRActionsClusterDisableActionParams params) => mtrBaseClusterActions -> params -> Ptr () -> IO ()
disableActionWithParams_completion mtrBaseClusterActions params completion =
  sendMessage mtrBaseClusterActions disableActionWithParams_completionSelector (toMTRActionsClusterDisableActionParams params) completion

-- | Command DisableActionWithDuration
--
-- This command is used to disable an action with a duration.
--
-- ObjC selector: @- disableActionWithDurationWithParams:completion:@
disableActionWithDurationWithParams_completion :: (IsMTRBaseClusterActions mtrBaseClusterActions, IsMTRActionsClusterDisableActionWithDurationParams params) => mtrBaseClusterActions -> params -> Ptr () -> IO ()
disableActionWithDurationWithParams_completion mtrBaseClusterActions params completion =
  sendMessage mtrBaseClusterActions disableActionWithDurationWithParams_completionSelector (toMTRActionsClusterDisableActionWithDurationParams params) completion

-- | @- readAttributeActionListWithCompletion:@
readAttributeActionListWithCompletion :: IsMTRBaseClusterActions mtrBaseClusterActions => mtrBaseClusterActions -> Ptr () -> IO ()
readAttributeActionListWithCompletion mtrBaseClusterActions completion =
  sendMessage mtrBaseClusterActions readAttributeActionListWithCompletionSelector completion

-- | @- subscribeAttributeActionListWithParams:subscriptionEstablished:reportHandler:@
subscribeAttributeActionListWithParams_subscriptionEstablished_reportHandler :: (IsMTRBaseClusterActions mtrBaseClusterActions, IsMTRSubscribeParams params) => mtrBaseClusterActions -> params -> Ptr () -> Ptr () -> IO ()
subscribeAttributeActionListWithParams_subscriptionEstablished_reportHandler mtrBaseClusterActions params subscriptionEstablished reportHandler =
  sendMessage mtrBaseClusterActions subscribeAttributeActionListWithParams_subscriptionEstablished_reportHandlerSelector (toMTRSubscribeParams params) subscriptionEstablished reportHandler

-- | @+ readAttributeActionListWithClusterStateCache:endpoint:queue:completion:@
readAttributeActionListWithClusterStateCache_endpoint_queue_completion :: (IsMTRClusterStateCacheContainer clusterStateCacheContainer, IsNSNumber endpoint, IsNSObject queue) => clusterStateCacheContainer -> endpoint -> queue -> Ptr () -> IO ()
readAttributeActionListWithClusterStateCache_endpoint_queue_completion clusterStateCacheContainer endpoint queue completion =
  do
    cls' <- getRequiredClass "MTRBaseClusterActions"
    sendClassMessage cls' readAttributeActionListWithClusterStateCache_endpoint_queue_completionSelector (toMTRClusterStateCacheContainer clusterStateCacheContainer) (toNSNumber endpoint) (toNSObject queue) completion

-- | @- readAttributeEndpointListsWithCompletion:@
readAttributeEndpointListsWithCompletion :: IsMTRBaseClusterActions mtrBaseClusterActions => mtrBaseClusterActions -> Ptr () -> IO ()
readAttributeEndpointListsWithCompletion mtrBaseClusterActions completion =
  sendMessage mtrBaseClusterActions readAttributeEndpointListsWithCompletionSelector completion

-- | @- subscribeAttributeEndpointListsWithParams:subscriptionEstablished:reportHandler:@
subscribeAttributeEndpointListsWithParams_subscriptionEstablished_reportHandler :: (IsMTRBaseClusterActions mtrBaseClusterActions, IsMTRSubscribeParams params) => mtrBaseClusterActions -> params -> Ptr () -> Ptr () -> IO ()
subscribeAttributeEndpointListsWithParams_subscriptionEstablished_reportHandler mtrBaseClusterActions params subscriptionEstablished reportHandler =
  sendMessage mtrBaseClusterActions subscribeAttributeEndpointListsWithParams_subscriptionEstablished_reportHandlerSelector (toMTRSubscribeParams params) subscriptionEstablished reportHandler

-- | @+ readAttributeEndpointListsWithClusterStateCache:endpoint:queue:completion:@
readAttributeEndpointListsWithClusterStateCache_endpoint_queue_completion :: (IsMTRClusterStateCacheContainer clusterStateCacheContainer, IsNSNumber endpoint, IsNSObject queue) => clusterStateCacheContainer -> endpoint -> queue -> Ptr () -> IO ()
readAttributeEndpointListsWithClusterStateCache_endpoint_queue_completion clusterStateCacheContainer endpoint queue completion =
  do
    cls' <- getRequiredClass "MTRBaseClusterActions"
    sendClassMessage cls' readAttributeEndpointListsWithClusterStateCache_endpoint_queue_completionSelector (toMTRClusterStateCacheContainer clusterStateCacheContainer) (toNSNumber endpoint) (toNSObject queue) completion

-- | @- readAttributeSetupURLWithCompletion:@
readAttributeSetupURLWithCompletion :: IsMTRBaseClusterActions mtrBaseClusterActions => mtrBaseClusterActions -> Ptr () -> IO ()
readAttributeSetupURLWithCompletion mtrBaseClusterActions completion =
  sendMessage mtrBaseClusterActions readAttributeSetupURLWithCompletionSelector completion

-- | @- subscribeAttributeSetupURLWithParams:subscriptionEstablished:reportHandler:@
subscribeAttributeSetupURLWithParams_subscriptionEstablished_reportHandler :: (IsMTRBaseClusterActions mtrBaseClusterActions, IsMTRSubscribeParams params) => mtrBaseClusterActions -> params -> Ptr () -> Ptr () -> IO ()
subscribeAttributeSetupURLWithParams_subscriptionEstablished_reportHandler mtrBaseClusterActions params subscriptionEstablished reportHandler =
  sendMessage mtrBaseClusterActions subscribeAttributeSetupURLWithParams_subscriptionEstablished_reportHandlerSelector (toMTRSubscribeParams params) subscriptionEstablished reportHandler

-- | @+ readAttributeSetupURLWithClusterStateCache:endpoint:queue:completion:@
readAttributeSetupURLWithClusterStateCache_endpoint_queue_completion :: (IsMTRClusterStateCacheContainer clusterStateCacheContainer, IsNSNumber endpoint, IsNSObject queue) => clusterStateCacheContainer -> endpoint -> queue -> Ptr () -> IO ()
readAttributeSetupURLWithClusterStateCache_endpoint_queue_completion clusterStateCacheContainer endpoint queue completion =
  do
    cls' <- getRequiredClass "MTRBaseClusterActions"
    sendClassMessage cls' readAttributeSetupURLWithClusterStateCache_endpoint_queue_completionSelector (toMTRClusterStateCacheContainer clusterStateCacheContainer) (toNSNumber endpoint) (toNSObject queue) completion

-- | @- readAttributeGeneratedCommandListWithCompletion:@
readAttributeGeneratedCommandListWithCompletion :: IsMTRBaseClusterActions mtrBaseClusterActions => mtrBaseClusterActions -> Ptr () -> IO ()
readAttributeGeneratedCommandListWithCompletion mtrBaseClusterActions completion =
  sendMessage mtrBaseClusterActions readAttributeGeneratedCommandListWithCompletionSelector completion

-- | @- subscribeAttributeGeneratedCommandListWithParams:subscriptionEstablished:reportHandler:@
subscribeAttributeGeneratedCommandListWithParams_subscriptionEstablished_reportHandler :: (IsMTRBaseClusterActions mtrBaseClusterActions, IsMTRSubscribeParams params) => mtrBaseClusterActions -> params -> Ptr () -> Ptr () -> IO ()
subscribeAttributeGeneratedCommandListWithParams_subscriptionEstablished_reportHandler mtrBaseClusterActions params subscriptionEstablished reportHandler =
  sendMessage mtrBaseClusterActions subscribeAttributeGeneratedCommandListWithParams_subscriptionEstablished_reportHandlerSelector (toMTRSubscribeParams params) subscriptionEstablished reportHandler

-- | @+ readAttributeGeneratedCommandListWithClusterStateCache:endpoint:queue:completion:@
readAttributeGeneratedCommandListWithClusterStateCache_endpoint_queue_completion :: (IsMTRClusterStateCacheContainer clusterStateCacheContainer, IsNSNumber endpoint, IsNSObject queue) => clusterStateCacheContainer -> endpoint -> queue -> Ptr () -> IO ()
readAttributeGeneratedCommandListWithClusterStateCache_endpoint_queue_completion clusterStateCacheContainer endpoint queue completion =
  do
    cls' <- getRequiredClass "MTRBaseClusterActions"
    sendClassMessage cls' readAttributeGeneratedCommandListWithClusterStateCache_endpoint_queue_completionSelector (toMTRClusterStateCacheContainer clusterStateCacheContainer) (toNSNumber endpoint) (toNSObject queue) completion

-- | @- readAttributeAcceptedCommandListWithCompletion:@
readAttributeAcceptedCommandListWithCompletion :: IsMTRBaseClusterActions mtrBaseClusterActions => mtrBaseClusterActions -> Ptr () -> IO ()
readAttributeAcceptedCommandListWithCompletion mtrBaseClusterActions completion =
  sendMessage mtrBaseClusterActions readAttributeAcceptedCommandListWithCompletionSelector completion

-- | @- subscribeAttributeAcceptedCommandListWithParams:subscriptionEstablished:reportHandler:@
subscribeAttributeAcceptedCommandListWithParams_subscriptionEstablished_reportHandler :: (IsMTRBaseClusterActions mtrBaseClusterActions, IsMTRSubscribeParams params) => mtrBaseClusterActions -> params -> Ptr () -> Ptr () -> IO ()
subscribeAttributeAcceptedCommandListWithParams_subscriptionEstablished_reportHandler mtrBaseClusterActions params subscriptionEstablished reportHandler =
  sendMessage mtrBaseClusterActions subscribeAttributeAcceptedCommandListWithParams_subscriptionEstablished_reportHandlerSelector (toMTRSubscribeParams params) subscriptionEstablished reportHandler

-- | @+ readAttributeAcceptedCommandListWithClusterStateCache:endpoint:queue:completion:@
readAttributeAcceptedCommandListWithClusterStateCache_endpoint_queue_completion :: (IsMTRClusterStateCacheContainer clusterStateCacheContainer, IsNSNumber endpoint, IsNSObject queue) => clusterStateCacheContainer -> endpoint -> queue -> Ptr () -> IO ()
readAttributeAcceptedCommandListWithClusterStateCache_endpoint_queue_completion clusterStateCacheContainer endpoint queue completion =
  do
    cls' <- getRequiredClass "MTRBaseClusterActions"
    sendClassMessage cls' readAttributeAcceptedCommandListWithClusterStateCache_endpoint_queue_completionSelector (toMTRClusterStateCacheContainer clusterStateCacheContainer) (toNSNumber endpoint) (toNSObject queue) completion

-- | @- readAttributeAttributeListWithCompletion:@
readAttributeAttributeListWithCompletion :: IsMTRBaseClusterActions mtrBaseClusterActions => mtrBaseClusterActions -> Ptr () -> IO ()
readAttributeAttributeListWithCompletion mtrBaseClusterActions completion =
  sendMessage mtrBaseClusterActions readAttributeAttributeListWithCompletionSelector completion

-- | @- subscribeAttributeAttributeListWithParams:subscriptionEstablished:reportHandler:@
subscribeAttributeAttributeListWithParams_subscriptionEstablished_reportHandler :: (IsMTRBaseClusterActions mtrBaseClusterActions, IsMTRSubscribeParams params) => mtrBaseClusterActions -> params -> Ptr () -> Ptr () -> IO ()
subscribeAttributeAttributeListWithParams_subscriptionEstablished_reportHandler mtrBaseClusterActions params subscriptionEstablished reportHandler =
  sendMessage mtrBaseClusterActions subscribeAttributeAttributeListWithParams_subscriptionEstablished_reportHandlerSelector (toMTRSubscribeParams params) subscriptionEstablished reportHandler

-- | @+ readAttributeAttributeListWithClusterStateCache:endpoint:queue:completion:@
readAttributeAttributeListWithClusterStateCache_endpoint_queue_completion :: (IsMTRClusterStateCacheContainer clusterStateCacheContainer, IsNSNumber endpoint, IsNSObject queue) => clusterStateCacheContainer -> endpoint -> queue -> Ptr () -> IO ()
readAttributeAttributeListWithClusterStateCache_endpoint_queue_completion clusterStateCacheContainer endpoint queue completion =
  do
    cls' <- getRequiredClass "MTRBaseClusterActions"
    sendClassMessage cls' readAttributeAttributeListWithClusterStateCache_endpoint_queue_completionSelector (toMTRClusterStateCacheContainer clusterStateCacheContainer) (toNSNumber endpoint) (toNSObject queue) completion

-- | @- readAttributeFeatureMapWithCompletion:@
readAttributeFeatureMapWithCompletion :: IsMTRBaseClusterActions mtrBaseClusterActions => mtrBaseClusterActions -> Ptr () -> IO ()
readAttributeFeatureMapWithCompletion mtrBaseClusterActions completion =
  sendMessage mtrBaseClusterActions readAttributeFeatureMapWithCompletionSelector completion

-- | @- subscribeAttributeFeatureMapWithParams:subscriptionEstablished:reportHandler:@
subscribeAttributeFeatureMapWithParams_subscriptionEstablished_reportHandler :: (IsMTRBaseClusterActions mtrBaseClusterActions, IsMTRSubscribeParams params) => mtrBaseClusterActions -> params -> Ptr () -> Ptr () -> IO ()
subscribeAttributeFeatureMapWithParams_subscriptionEstablished_reportHandler mtrBaseClusterActions params subscriptionEstablished reportHandler =
  sendMessage mtrBaseClusterActions subscribeAttributeFeatureMapWithParams_subscriptionEstablished_reportHandlerSelector (toMTRSubscribeParams params) subscriptionEstablished reportHandler

-- | @+ readAttributeFeatureMapWithClusterStateCache:endpoint:queue:completion:@
readAttributeFeatureMapWithClusterStateCache_endpoint_queue_completion :: (IsMTRClusterStateCacheContainer clusterStateCacheContainer, IsNSNumber endpoint, IsNSObject queue) => clusterStateCacheContainer -> endpoint -> queue -> Ptr () -> IO ()
readAttributeFeatureMapWithClusterStateCache_endpoint_queue_completion clusterStateCacheContainer endpoint queue completion =
  do
    cls' <- getRequiredClass "MTRBaseClusterActions"
    sendClassMessage cls' readAttributeFeatureMapWithClusterStateCache_endpoint_queue_completionSelector (toMTRClusterStateCacheContainer clusterStateCacheContainer) (toNSNumber endpoint) (toNSObject queue) completion

-- | @- readAttributeClusterRevisionWithCompletion:@
readAttributeClusterRevisionWithCompletion :: IsMTRBaseClusterActions mtrBaseClusterActions => mtrBaseClusterActions -> Ptr () -> IO ()
readAttributeClusterRevisionWithCompletion mtrBaseClusterActions completion =
  sendMessage mtrBaseClusterActions readAttributeClusterRevisionWithCompletionSelector completion

-- | @- subscribeAttributeClusterRevisionWithParams:subscriptionEstablished:reportHandler:@
subscribeAttributeClusterRevisionWithParams_subscriptionEstablished_reportHandler :: (IsMTRBaseClusterActions mtrBaseClusterActions, IsMTRSubscribeParams params) => mtrBaseClusterActions -> params -> Ptr () -> Ptr () -> IO ()
subscribeAttributeClusterRevisionWithParams_subscriptionEstablished_reportHandler mtrBaseClusterActions params subscriptionEstablished reportHandler =
  sendMessage mtrBaseClusterActions subscribeAttributeClusterRevisionWithParams_subscriptionEstablished_reportHandlerSelector (toMTRSubscribeParams params) subscriptionEstablished reportHandler

-- | @+ readAttributeClusterRevisionWithClusterStateCache:endpoint:queue:completion:@
readAttributeClusterRevisionWithClusterStateCache_endpoint_queue_completion :: (IsMTRClusterStateCacheContainer clusterStateCacheContainer, IsNSNumber endpoint, IsNSObject queue) => clusterStateCacheContainer -> endpoint -> queue -> Ptr () -> IO ()
readAttributeClusterRevisionWithClusterStateCache_endpoint_queue_completion clusterStateCacheContainer endpoint queue completion =
  do
    cls' <- getRequiredClass "MTRBaseClusterActions"
    sendClassMessage cls' readAttributeClusterRevisionWithClusterStateCache_endpoint_queue_completionSelector (toMTRClusterStateCacheContainer clusterStateCacheContainer) (toNSNumber endpoint) (toNSObject queue) completion

-- | @- init@
init_ :: IsMTRBaseClusterActions mtrBaseClusterActions => mtrBaseClusterActions -> IO (Id MTRBaseClusterActions)
init_ mtrBaseClusterActions =
  sendOwnedMessage mtrBaseClusterActions initSelector

-- | @+ new@
new :: IO (Id MTRBaseClusterActions)
new  =
  do
    cls' <- getRequiredClass "MTRBaseClusterActions"
    sendOwnedClassMessage cls' newSelector

-- | @- initWithDevice:endpoint:queue:@
initWithDevice_endpoint_queue :: (IsMTRBaseClusterActions mtrBaseClusterActions, IsMTRBaseDevice device, IsNSObject queue) => mtrBaseClusterActions -> device -> CUShort -> queue -> IO (Id MTRBaseClusterActions)
initWithDevice_endpoint_queue mtrBaseClusterActions device endpoint queue =
  sendOwnedMessage mtrBaseClusterActions initWithDevice_endpoint_queueSelector (toMTRBaseDevice device) endpoint (toNSObject queue)

-- | @- instantActionWithParams:completionHandler:@
instantActionWithParams_completionHandler :: (IsMTRBaseClusterActions mtrBaseClusterActions, IsMTRActionsClusterInstantActionParams params) => mtrBaseClusterActions -> params -> Ptr () -> IO ()
instantActionWithParams_completionHandler mtrBaseClusterActions params completionHandler =
  sendMessage mtrBaseClusterActions instantActionWithParams_completionHandlerSelector (toMTRActionsClusterInstantActionParams params) completionHandler

-- | @- instantActionWithTransitionWithParams:completionHandler:@
instantActionWithTransitionWithParams_completionHandler :: (IsMTRBaseClusterActions mtrBaseClusterActions, IsMTRActionsClusterInstantActionWithTransitionParams params) => mtrBaseClusterActions -> params -> Ptr () -> IO ()
instantActionWithTransitionWithParams_completionHandler mtrBaseClusterActions params completionHandler =
  sendMessage mtrBaseClusterActions instantActionWithTransitionWithParams_completionHandlerSelector (toMTRActionsClusterInstantActionWithTransitionParams params) completionHandler

-- | @- startActionWithParams:completionHandler:@
startActionWithParams_completionHandler :: (IsMTRBaseClusterActions mtrBaseClusterActions, IsMTRActionsClusterStartActionParams params) => mtrBaseClusterActions -> params -> Ptr () -> IO ()
startActionWithParams_completionHandler mtrBaseClusterActions params completionHandler =
  sendMessage mtrBaseClusterActions startActionWithParams_completionHandlerSelector (toMTRActionsClusterStartActionParams params) completionHandler

-- | @- startActionWithDurationWithParams:completionHandler:@
startActionWithDurationWithParams_completionHandler :: (IsMTRBaseClusterActions mtrBaseClusterActions, IsMTRActionsClusterStartActionWithDurationParams params) => mtrBaseClusterActions -> params -> Ptr () -> IO ()
startActionWithDurationWithParams_completionHandler mtrBaseClusterActions params completionHandler =
  sendMessage mtrBaseClusterActions startActionWithDurationWithParams_completionHandlerSelector (toMTRActionsClusterStartActionWithDurationParams params) completionHandler

-- | @- stopActionWithParams:completionHandler:@
stopActionWithParams_completionHandler :: (IsMTRBaseClusterActions mtrBaseClusterActions, IsMTRActionsClusterStopActionParams params) => mtrBaseClusterActions -> params -> Ptr () -> IO ()
stopActionWithParams_completionHandler mtrBaseClusterActions params completionHandler =
  sendMessage mtrBaseClusterActions stopActionWithParams_completionHandlerSelector (toMTRActionsClusterStopActionParams params) completionHandler

-- | @- pauseActionWithParams:completionHandler:@
pauseActionWithParams_completionHandler :: (IsMTRBaseClusterActions mtrBaseClusterActions, IsMTRActionsClusterPauseActionParams params) => mtrBaseClusterActions -> params -> Ptr () -> IO ()
pauseActionWithParams_completionHandler mtrBaseClusterActions params completionHandler =
  sendMessage mtrBaseClusterActions pauseActionWithParams_completionHandlerSelector (toMTRActionsClusterPauseActionParams params) completionHandler

-- | @- pauseActionWithDurationWithParams:completionHandler:@
pauseActionWithDurationWithParams_completionHandler :: (IsMTRBaseClusterActions mtrBaseClusterActions, IsMTRActionsClusterPauseActionWithDurationParams params) => mtrBaseClusterActions -> params -> Ptr () -> IO ()
pauseActionWithDurationWithParams_completionHandler mtrBaseClusterActions params completionHandler =
  sendMessage mtrBaseClusterActions pauseActionWithDurationWithParams_completionHandlerSelector (toMTRActionsClusterPauseActionWithDurationParams params) completionHandler

-- | @- resumeActionWithParams:completionHandler:@
resumeActionWithParams_completionHandler :: (IsMTRBaseClusterActions mtrBaseClusterActions, IsMTRActionsClusterResumeActionParams params) => mtrBaseClusterActions -> params -> Ptr () -> IO ()
resumeActionWithParams_completionHandler mtrBaseClusterActions params completionHandler =
  sendMessage mtrBaseClusterActions resumeActionWithParams_completionHandlerSelector (toMTRActionsClusterResumeActionParams params) completionHandler

-- | @- enableActionWithParams:completionHandler:@
enableActionWithParams_completionHandler :: (IsMTRBaseClusterActions mtrBaseClusterActions, IsMTRActionsClusterEnableActionParams params) => mtrBaseClusterActions -> params -> Ptr () -> IO ()
enableActionWithParams_completionHandler mtrBaseClusterActions params completionHandler =
  sendMessage mtrBaseClusterActions enableActionWithParams_completionHandlerSelector (toMTRActionsClusterEnableActionParams params) completionHandler

-- | @- enableActionWithDurationWithParams:completionHandler:@
enableActionWithDurationWithParams_completionHandler :: (IsMTRBaseClusterActions mtrBaseClusterActions, IsMTRActionsClusterEnableActionWithDurationParams params) => mtrBaseClusterActions -> params -> Ptr () -> IO ()
enableActionWithDurationWithParams_completionHandler mtrBaseClusterActions params completionHandler =
  sendMessage mtrBaseClusterActions enableActionWithDurationWithParams_completionHandlerSelector (toMTRActionsClusterEnableActionWithDurationParams params) completionHandler

-- | @- disableActionWithParams:completionHandler:@
disableActionWithParams_completionHandler :: (IsMTRBaseClusterActions mtrBaseClusterActions, IsMTRActionsClusterDisableActionParams params) => mtrBaseClusterActions -> params -> Ptr () -> IO ()
disableActionWithParams_completionHandler mtrBaseClusterActions params completionHandler =
  sendMessage mtrBaseClusterActions disableActionWithParams_completionHandlerSelector (toMTRActionsClusterDisableActionParams params) completionHandler

-- | @- disableActionWithDurationWithParams:completionHandler:@
disableActionWithDurationWithParams_completionHandler :: (IsMTRBaseClusterActions mtrBaseClusterActions, IsMTRActionsClusterDisableActionWithDurationParams params) => mtrBaseClusterActions -> params -> Ptr () -> IO ()
disableActionWithDurationWithParams_completionHandler mtrBaseClusterActions params completionHandler =
  sendMessage mtrBaseClusterActions disableActionWithDurationWithParams_completionHandlerSelector (toMTRActionsClusterDisableActionWithDurationParams params) completionHandler

-- | @- readAttributeActionListWithCompletionHandler:@
readAttributeActionListWithCompletionHandler :: IsMTRBaseClusterActions mtrBaseClusterActions => mtrBaseClusterActions -> Ptr () -> IO ()
readAttributeActionListWithCompletionHandler mtrBaseClusterActions completionHandler =
  sendMessage mtrBaseClusterActions readAttributeActionListWithCompletionHandlerSelector completionHandler

-- | @- subscribeAttributeActionListWithMinInterval:maxInterval:params:subscriptionEstablished:reportHandler:@
subscribeAttributeActionListWithMinInterval_maxInterval_params_subscriptionEstablished_reportHandler :: (IsMTRBaseClusterActions mtrBaseClusterActions, IsNSNumber minInterval, IsNSNumber maxInterval, IsMTRSubscribeParams params) => mtrBaseClusterActions -> minInterval -> maxInterval -> params -> Ptr () -> Ptr () -> IO ()
subscribeAttributeActionListWithMinInterval_maxInterval_params_subscriptionEstablished_reportHandler mtrBaseClusterActions minInterval maxInterval params subscriptionEstablishedHandler reportHandler =
  sendMessage mtrBaseClusterActions subscribeAttributeActionListWithMinInterval_maxInterval_params_subscriptionEstablished_reportHandlerSelector (toNSNumber minInterval) (toNSNumber maxInterval) (toMTRSubscribeParams params) subscriptionEstablishedHandler reportHandler

-- | @+ readAttributeActionListWithAttributeCache:endpoint:queue:completionHandler:@
readAttributeActionListWithAttributeCache_endpoint_queue_completionHandler :: (IsMTRAttributeCacheContainer attributeCacheContainer, IsNSNumber endpoint, IsNSObject queue) => attributeCacheContainer -> endpoint -> queue -> Ptr () -> IO ()
readAttributeActionListWithAttributeCache_endpoint_queue_completionHandler attributeCacheContainer endpoint queue completionHandler =
  do
    cls' <- getRequiredClass "MTRBaseClusterActions"
    sendClassMessage cls' readAttributeActionListWithAttributeCache_endpoint_queue_completionHandlerSelector (toMTRAttributeCacheContainer attributeCacheContainer) (toNSNumber endpoint) (toNSObject queue) completionHandler

-- | @- readAttributeEndpointListsWithCompletionHandler:@
readAttributeEndpointListsWithCompletionHandler :: IsMTRBaseClusterActions mtrBaseClusterActions => mtrBaseClusterActions -> Ptr () -> IO ()
readAttributeEndpointListsWithCompletionHandler mtrBaseClusterActions completionHandler =
  sendMessage mtrBaseClusterActions readAttributeEndpointListsWithCompletionHandlerSelector completionHandler

-- | @- subscribeAttributeEndpointListsWithMinInterval:maxInterval:params:subscriptionEstablished:reportHandler:@
subscribeAttributeEndpointListsWithMinInterval_maxInterval_params_subscriptionEstablished_reportHandler :: (IsMTRBaseClusterActions mtrBaseClusterActions, IsNSNumber minInterval, IsNSNumber maxInterval, IsMTRSubscribeParams params) => mtrBaseClusterActions -> minInterval -> maxInterval -> params -> Ptr () -> Ptr () -> IO ()
subscribeAttributeEndpointListsWithMinInterval_maxInterval_params_subscriptionEstablished_reportHandler mtrBaseClusterActions minInterval maxInterval params subscriptionEstablishedHandler reportHandler =
  sendMessage mtrBaseClusterActions subscribeAttributeEndpointListsWithMinInterval_maxInterval_params_subscriptionEstablished_reportHandlerSelector (toNSNumber minInterval) (toNSNumber maxInterval) (toMTRSubscribeParams params) subscriptionEstablishedHandler reportHandler

-- | @+ readAttributeEndpointListsWithAttributeCache:endpoint:queue:completionHandler:@
readAttributeEndpointListsWithAttributeCache_endpoint_queue_completionHandler :: (IsMTRAttributeCacheContainer attributeCacheContainer, IsNSNumber endpoint, IsNSObject queue) => attributeCacheContainer -> endpoint -> queue -> Ptr () -> IO ()
readAttributeEndpointListsWithAttributeCache_endpoint_queue_completionHandler attributeCacheContainer endpoint queue completionHandler =
  do
    cls' <- getRequiredClass "MTRBaseClusterActions"
    sendClassMessage cls' readAttributeEndpointListsWithAttributeCache_endpoint_queue_completionHandlerSelector (toMTRAttributeCacheContainer attributeCacheContainer) (toNSNumber endpoint) (toNSObject queue) completionHandler

-- | @- readAttributeSetupURLWithCompletionHandler:@
readAttributeSetupURLWithCompletionHandler :: IsMTRBaseClusterActions mtrBaseClusterActions => mtrBaseClusterActions -> Ptr () -> IO ()
readAttributeSetupURLWithCompletionHandler mtrBaseClusterActions completionHandler =
  sendMessage mtrBaseClusterActions readAttributeSetupURLWithCompletionHandlerSelector completionHandler

-- | @- subscribeAttributeSetupURLWithMinInterval:maxInterval:params:subscriptionEstablished:reportHandler:@
subscribeAttributeSetupURLWithMinInterval_maxInterval_params_subscriptionEstablished_reportHandler :: (IsMTRBaseClusterActions mtrBaseClusterActions, IsNSNumber minInterval, IsNSNumber maxInterval, IsMTRSubscribeParams params) => mtrBaseClusterActions -> minInterval -> maxInterval -> params -> Ptr () -> Ptr () -> IO ()
subscribeAttributeSetupURLWithMinInterval_maxInterval_params_subscriptionEstablished_reportHandler mtrBaseClusterActions minInterval maxInterval params subscriptionEstablishedHandler reportHandler =
  sendMessage mtrBaseClusterActions subscribeAttributeSetupURLWithMinInterval_maxInterval_params_subscriptionEstablished_reportHandlerSelector (toNSNumber minInterval) (toNSNumber maxInterval) (toMTRSubscribeParams params) subscriptionEstablishedHandler reportHandler

-- | @+ readAttributeSetupURLWithAttributeCache:endpoint:queue:completionHandler:@
readAttributeSetupURLWithAttributeCache_endpoint_queue_completionHandler :: (IsMTRAttributeCacheContainer attributeCacheContainer, IsNSNumber endpoint, IsNSObject queue) => attributeCacheContainer -> endpoint -> queue -> Ptr () -> IO ()
readAttributeSetupURLWithAttributeCache_endpoint_queue_completionHandler attributeCacheContainer endpoint queue completionHandler =
  do
    cls' <- getRequiredClass "MTRBaseClusterActions"
    sendClassMessage cls' readAttributeSetupURLWithAttributeCache_endpoint_queue_completionHandlerSelector (toMTRAttributeCacheContainer attributeCacheContainer) (toNSNumber endpoint) (toNSObject queue) completionHandler

-- | @- readAttributeGeneratedCommandListWithCompletionHandler:@
readAttributeGeneratedCommandListWithCompletionHandler :: IsMTRBaseClusterActions mtrBaseClusterActions => mtrBaseClusterActions -> Ptr () -> IO ()
readAttributeGeneratedCommandListWithCompletionHandler mtrBaseClusterActions completionHandler =
  sendMessage mtrBaseClusterActions readAttributeGeneratedCommandListWithCompletionHandlerSelector completionHandler

-- | @- subscribeAttributeGeneratedCommandListWithMinInterval:maxInterval:params:subscriptionEstablished:reportHandler:@
subscribeAttributeGeneratedCommandListWithMinInterval_maxInterval_params_subscriptionEstablished_reportHandler :: (IsMTRBaseClusterActions mtrBaseClusterActions, IsNSNumber minInterval, IsNSNumber maxInterval, IsMTRSubscribeParams params) => mtrBaseClusterActions -> minInterval -> maxInterval -> params -> Ptr () -> Ptr () -> IO ()
subscribeAttributeGeneratedCommandListWithMinInterval_maxInterval_params_subscriptionEstablished_reportHandler mtrBaseClusterActions minInterval maxInterval params subscriptionEstablishedHandler reportHandler =
  sendMessage mtrBaseClusterActions subscribeAttributeGeneratedCommandListWithMinInterval_maxInterval_params_subscriptionEstablished_reportHandlerSelector (toNSNumber minInterval) (toNSNumber maxInterval) (toMTRSubscribeParams params) subscriptionEstablishedHandler reportHandler

-- | @+ readAttributeGeneratedCommandListWithAttributeCache:endpoint:queue:completionHandler:@
readAttributeGeneratedCommandListWithAttributeCache_endpoint_queue_completionHandler :: (IsMTRAttributeCacheContainer attributeCacheContainer, IsNSNumber endpoint, IsNSObject queue) => attributeCacheContainer -> endpoint -> queue -> Ptr () -> IO ()
readAttributeGeneratedCommandListWithAttributeCache_endpoint_queue_completionHandler attributeCacheContainer endpoint queue completionHandler =
  do
    cls' <- getRequiredClass "MTRBaseClusterActions"
    sendClassMessage cls' readAttributeGeneratedCommandListWithAttributeCache_endpoint_queue_completionHandlerSelector (toMTRAttributeCacheContainer attributeCacheContainer) (toNSNumber endpoint) (toNSObject queue) completionHandler

-- | @- readAttributeAcceptedCommandListWithCompletionHandler:@
readAttributeAcceptedCommandListWithCompletionHandler :: IsMTRBaseClusterActions mtrBaseClusterActions => mtrBaseClusterActions -> Ptr () -> IO ()
readAttributeAcceptedCommandListWithCompletionHandler mtrBaseClusterActions completionHandler =
  sendMessage mtrBaseClusterActions readAttributeAcceptedCommandListWithCompletionHandlerSelector completionHandler

-- | @- subscribeAttributeAcceptedCommandListWithMinInterval:maxInterval:params:subscriptionEstablished:reportHandler:@
subscribeAttributeAcceptedCommandListWithMinInterval_maxInterval_params_subscriptionEstablished_reportHandler :: (IsMTRBaseClusterActions mtrBaseClusterActions, IsNSNumber minInterval, IsNSNumber maxInterval, IsMTRSubscribeParams params) => mtrBaseClusterActions -> minInterval -> maxInterval -> params -> Ptr () -> Ptr () -> IO ()
subscribeAttributeAcceptedCommandListWithMinInterval_maxInterval_params_subscriptionEstablished_reportHandler mtrBaseClusterActions minInterval maxInterval params subscriptionEstablishedHandler reportHandler =
  sendMessage mtrBaseClusterActions subscribeAttributeAcceptedCommandListWithMinInterval_maxInterval_params_subscriptionEstablished_reportHandlerSelector (toNSNumber minInterval) (toNSNumber maxInterval) (toMTRSubscribeParams params) subscriptionEstablishedHandler reportHandler

-- | @+ readAttributeAcceptedCommandListWithAttributeCache:endpoint:queue:completionHandler:@
readAttributeAcceptedCommandListWithAttributeCache_endpoint_queue_completionHandler :: (IsMTRAttributeCacheContainer attributeCacheContainer, IsNSNumber endpoint, IsNSObject queue) => attributeCacheContainer -> endpoint -> queue -> Ptr () -> IO ()
readAttributeAcceptedCommandListWithAttributeCache_endpoint_queue_completionHandler attributeCacheContainer endpoint queue completionHandler =
  do
    cls' <- getRequiredClass "MTRBaseClusterActions"
    sendClassMessage cls' readAttributeAcceptedCommandListWithAttributeCache_endpoint_queue_completionHandlerSelector (toMTRAttributeCacheContainer attributeCacheContainer) (toNSNumber endpoint) (toNSObject queue) completionHandler

-- | @- readAttributeAttributeListWithCompletionHandler:@
readAttributeAttributeListWithCompletionHandler :: IsMTRBaseClusterActions mtrBaseClusterActions => mtrBaseClusterActions -> Ptr () -> IO ()
readAttributeAttributeListWithCompletionHandler mtrBaseClusterActions completionHandler =
  sendMessage mtrBaseClusterActions readAttributeAttributeListWithCompletionHandlerSelector completionHandler

-- | @- subscribeAttributeAttributeListWithMinInterval:maxInterval:params:subscriptionEstablished:reportHandler:@
subscribeAttributeAttributeListWithMinInterval_maxInterval_params_subscriptionEstablished_reportHandler :: (IsMTRBaseClusterActions mtrBaseClusterActions, IsNSNumber minInterval, IsNSNumber maxInterval, IsMTRSubscribeParams params) => mtrBaseClusterActions -> minInterval -> maxInterval -> params -> Ptr () -> Ptr () -> IO ()
subscribeAttributeAttributeListWithMinInterval_maxInterval_params_subscriptionEstablished_reportHandler mtrBaseClusterActions minInterval maxInterval params subscriptionEstablishedHandler reportHandler =
  sendMessage mtrBaseClusterActions subscribeAttributeAttributeListWithMinInterval_maxInterval_params_subscriptionEstablished_reportHandlerSelector (toNSNumber minInterval) (toNSNumber maxInterval) (toMTRSubscribeParams params) subscriptionEstablishedHandler reportHandler

-- | @+ readAttributeAttributeListWithAttributeCache:endpoint:queue:completionHandler:@
readAttributeAttributeListWithAttributeCache_endpoint_queue_completionHandler :: (IsMTRAttributeCacheContainer attributeCacheContainer, IsNSNumber endpoint, IsNSObject queue) => attributeCacheContainer -> endpoint -> queue -> Ptr () -> IO ()
readAttributeAttributeListWithAttributeCache_endpoint_queue_completionHandler attributeCacheContainer endpoint queue completionHandler =
  do
    cls' <- getRequiredClass "MTRBaseClusterActions"
    sendClassMessage cls' readAttributeAttributeListWithAttributeCache_endpoint_queue_completionHandlerSelector (toMTRAttributeCacheContainer attributeCacheContainer) (toNSNumber endpoint) (toNSObject queue) completionHandler

-- | @- readAttributeFeatureMapWithCompletionHandler:@
readAttributeFeatureMapWithCompletionHandler :: IsMTRBaseClusterActions mtrBaseClusterActions => mtrBaseClusterActions -> Ptr () -> IO ()
readAttributeFeatureMapWithCompletionHandler mtrBaseClusterActions completionHandler =
  sendMessage mtrBaseClusterActions readAttributeFeatureMapWithCompletionHandlerSelector completionHandler

-- | @- subscribeAttributeFeatureMapWithMinInterval:maxInterval:params:subscriptionEstablished:reportHandler:@
subscribeAttributeFeatureMapWithMinInterval_maxInterval_params_subscriptionEstablished_reportHandler :: (IsMTRBaseClusterActions mtrBaseClusterActions, IsNSNumber minInterval, IsNSNumber maxInterval, IsMTRSubscribeParams params) => mtrBaseClusterActions -> minInterval -> maxInterval -> params -> Ptr () -> Ptr () -> IO ()
subscribeAttributeFeatureMapWithMinInterval_maxInterval_params_subscriptionEstablished_reportHandler mtrBaseClusterActions minInterval maxInterval params subscriptionEstablishedHandler reportHandler =
  sendMessage mtrBaseClusterActions subscribeAttributeFeatureMapWithMinInterval_maxInterval_params_subscriptionEstablished_reportHandlerSelector (toNSNumber minInterval) (toNSNumber maxInterval) (toMTRSubscribeParams params) subscriptionEstablishedHandler reportHandler

-- | @+ readAttributeFeatureMapWithAttributeCache:endpoint:queue:completionHandler:@
readAttributeFeatureMapWithAttributeCache_endpoint_queue_completionHandler :: (IsMTRAttributeCacheContainer attributeCacheContainer, IsNSNumber endpoint, IsNSObject queue) => attributeCacheContainer -> endpoint -> queue -> Ptr () -> IO ()
readAttributeFeatureMapWithAttributeCache_endpoint_queue_completionHandler attributeCacheContainer endpoint queue completionHandler =
  do
    cls' <- getRequiredClass "MTRBaseClusterActions"
    sendClassMessage cls' readAttributeFeatureMapWithAttributeCache_endpoint_queue_completionHandlerSelector (toMTRAttributeCacheContainer attributeCacheContainer) (toNSNumber endpoint) (toNSObject queue) completionHandler

-- | @- readAttributeClusterRevisionWithCompletionHandler:@
readAttributeClusterRevisionWithCompletionHandler :: IsMTRBaseClusterActions mtrBaseClusterActions => mtrBaseClusterActions -> Ptr () -> IO ()
readAttributeClusterRevisionWithCompletionHandler mtrBaseClusterActions completionHandler =
  sendMessage mtrBaseClusterActions readAttributeClusterRevisionWithCompletionHandlerSelector completionHandler

-- | @- subscribeAttributeClusterRevisionWithMinInterval:maxInterval:params:subscriptionEstablished:reportHandler:@
subscribeAttributeClusterRevisionWithMinInterval_maxInterval_params_subscriptionEstablished_reportHandler :: (IsMTRBaseClusterActions mtrBaseClusterActions, IsNSNumber minInterval, IsNSNumber maxInterval, IsMTRSubscribeParams params) => mtrBaseClusterActions -> minInterval -> maxInterval -> params -> Ptr () -> Ptr () -> IO ()
subscribeAttributeClusterRevisionWithMinInterval_maxInterval_params_subscriptionEstablished_reportHandler mtrBaseClusterActions minInterval maxInterval params subscriptionEstablishedHandler reportHandler =
  sendMessage mtrBaseClusterActions subscribeAttributeClusterRevisionWithMinInterval_maxInterval_params_subscriptionEstablished_reportHandlerSelector (toNSNumber minInterval) (toNSNumber maxInterval) (toMTRSubscribeParams params) subscriptionEstablishedHandler reportHandler

-- | @+ readAttributeClusterRevisionWithAttributeCache:endpoint:queue:completionHandler:@
readAttributeClusterRevisionWithAttributeCache_endpoint_queue_completionHandler :: (IsMTRAttributeCacheContainer attributeCacheContainer, IsNSNumber endpoint, IsNSObject queue) => attributeCacheContainer -> endpoint -> queue -> Ptr () -> IO ()
readAttributeClusterRevisionWithAttributeCache_endpoint_queue_completionHandler attributeCacheContainer endpoint queue completionHandler =
  do
    cls' <- getRequiredClass "MTRBaseClusterActions"
    sendClassMessage cls' readAttributeClusterRevisionWithAttributeCache_endpoint_queue_completionHandlerSelector (toMTRAttributeCacheContainer attributeCacheContainer) (toNSNumber endpoint) (toNSObject queue) completionHandler

-- | For all instance methods (reads, writes, commands) that take a completion, the completion will be called on the provided queue.
--
-- ObjC selector: @- initWithDevice:endpointID:queue:@
initWithDevice_endpointID_queue :: (IsMTRBaseClusterActions mtrBaseClusterActions, IsMTRBaseDevice device, IsNSNumber endpointID, IsNSObject queue) => mtrBaseClusterActions -> device -> endpointID -> queue -> IO (Id MTRBaseClusterActions)
initWithDevice_endpointID_queue mtrBaseClusterActions device endpointID queue =
  sendOwnedMessage mtrBaseClusterActions initWithDevice_endpointID_queueSelector (toMTRBaseDevice device) (toNSNumber endpointID) (toNSObject queue)

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @instantActionWithParams:completion:@
instantActionWithParams_completionSelector :: Selector '[Id MTRActionsClusterInstantActionParams, Ptr ()] ()
instantActionWithParams_completionSelector = mkSelector "instantActionWithParams:completion:"

-- | @Selector@ for @instantActionWithTransitionWithParams:completion:@
instantActionWithTransitionWithParams_completionSelector :: Selector '[Id MTRActionsClusterInstantActionWithTransitionParams, Ptr ()] ()
instantActionWithTransitionWithParams_completionSelector = mkSelector "instantActionWithTransitionWithParams:completion:"

-- | @Selector@ for @startActionWithParams:completion:@
startActionWithParams_completionSelector :: Selector '[Id MTRActionsClusterStartActionParams, Ptr ()] ()
startActionWithParams_completionSelector = mkSelector "startActionWithParams:completion:"

-- | @Selector@ for @startActionWithDurationWithParams:completion:@
startActionWithDurationWithParams_completionSelector :: Selector '[Id MTRActionsClusterStartActionWithDurationParams, Ptr ()] ()
startActionWithDurationWithParams_completionSelector = mkSelector "startActionWithDurationWithParams:completion:"

-- | @Selector@ for @stopActionWithParams:completion:@
stopActionWithParams_completionSelector :: Selector '[Id MTRActionsClusterStopActionParams, Ptr ()] ()
stopActionWithParams_completionSelector = mkSelector "stopActionWithParams:completion:"

-- | @Selector@ for @pauseActionWithParams:completion:@
pauseActionWithParams_completionSelector :: Selector '[Id MTRActionsClusterPauseActionParams, Ptr ()] ()
pauseActionWithParams_completionSelector = mkSelector "pauseActionWithParams:completion:"

-- | @Selector@ for @pauseActionWithDurationWithParams:completion:@
pauseActionWithDurationWithParams_completionSelector :: Selector '[Id MTRActionsClusterPauseActionWithDurationParams, Ptr ()] ()
pauseActionWithDurationWithParams_completionSelector = mkSelector "pauseActionWithDurationWithParams:completion:"

-- | @Selector@ for @resumeActionWithParams:completion:@
resumeActionWithParams_completionSelector :: Selector '[Id MTRActionsClusterResumeActionParams, Ptr ()] ()
resumeActionWithParams_completionSelector = mkSelector "resumeActionWithParams:completion:"

-- | @Selector@ for @enableActionWithParams:completion:@
enableActionWithParams_completionSelector :: Selector '[Id MTRActionsClusterEnableActionParams, Ptr ()] ()
enableActionWithParams_completionSelector = mkSelector "enableActionWithParams:completion:"

-- | @Selector@ for @enableActionWithDurationWithParams:completion:@
enableActionWithDurationWithParams_completionSelector :: Selector '[Id MTRActionsClusterEnableActionWithDurationParams, Ptr ()] ()
enableActionWithDurationWithParams_completionSelector = mkSelector "enableActionWithDurationWithParams:completion:"

-- | @Selector@ for @disableActionWithParams:completion:@
disableActionWithParams_completionSelector :: Selector '[Id MTRActionsClusterDisableActionParams, Ptr ()] ()
disableActionWithParams_completionSelector = mkSelector "disableActionWithParams:completion:"

-- | @Selector@ for @disableActionWithDurationWithParams:completion:@
disableActionWithDurationWithParams_completionSelector :: Selector '[Id MTRActionsClusterDisableActionWithDurationParams, Ptr ()] ()
disableActionWithDurationWithParams_completionSelector = mkSelector "disableActionWithDurationWithParams:completion:"

-- | @Selector@ for @readAttributeActionListWithCompletion:@
readAttributeActionListWithCompletionSelector :: Selector '[Ptr ()] ()
readAttributeActionListWithCompletionSelector = mkSelector "readAttributeActionListWithCompletion:"

-- | @Selector@ for @subscribeAttributeActionListWithParams:subscriptionEstablished:reportHandler:@
subscribeAttributeActionListWithParams_subscriptionEstablished_reportHandlerSelector :: Selector '[Id MTRSubscribeParams, Ptr (), Ptr ()] ()
subscribeAttributeActionListWithParams_subscriptionEstablished_reportHandlerSelector = mkSelector "subscribeAttributeActionListWithParams:subscriptionEstablished:reportHandler:"

-- | @Selector@ for @readAttributeActionListWithClusterStateCache:endpoint:queue:completion:@
readAttributeActionListWithClusterStateCache_endpoint_queue_completionSelector :: Selector '[Id MTRClusterStateCacheContainer, Id NSNumber, Id NSObject, Ptr ()] ()
readAttributeActionListWithClusterStateCache_endpoint_queue_completionSelector = mkSelector "readAttributeActionListWithClusterStateCache:endpoint:queue:completion:"

-- | @Selector@ for @readAttributeEndpointListsWithCompletion:@
readAttributeEndpointListsWithCompletionSelector :: Selector '[Ptr ()] ()
readAttributeEndpointListsWithCompletionSelector = mkSelector "readAttributeEndpointListsWithCompletion:"

-- | @Selector@ for @subscribeAttributeEndpointListsWithParams:subscriptionEstablished:reportHandler:@
subscribeAttributeEndpointListsWithParams_subscriptionEstablished_reportHandlerSelector :: Selector '[Id MTRSubscribeParams, Ptr (), Ptr ()] ()
subscribeAttributeEndpointListsWithParams_subscriptionEstablished_reportHandlerSelector = mkSelector "subscribeAttributeEndpointListsWithParams:subscriptionEstablished:reportHandler:"

-- | @Selector@ for @readAttributeEndpointListsWithClusterStateCache:endpoint:queue:completion:@
readAttributeEndpointListsWithClusterStateCache_endpoint_queue_completionSelector :: Selector '[Id MTRClusterStateCacheContainer, Id NSNumber, Id NSObject, Ptr ()] ()
readAttributeEndpointListsWithClusterStateCache_endpoint_queue_completionSelector = mkSelector "readAttributeEndpointListsWithClusterStateCache:endpoint:queue:completion:"

-- | @Selector@ for @readAttributeSetupURLWithCompletion:@
readAttributeSetupURLWithCompletionSelector :: Selector '[Ptr ()] ()
readAttributeSetupURLWithCompletionSelector = mkSelector "readAttributeSetupURLWithCompletion:"

-- | @Selector@ for @subscribeAttributeSetupURLWithParams:subscriptionEstablished:reportHandler:@
subscribeAttributeSetupURLWithParams_subscriptionEstablished_reportHandlerSelector :: Selector '[Id MTRSubscribeParams, Ptr (), Ptr ()] ()
subscribeAttributeSetupURLWithParams_subscriptionEstablished_reportHandlerSelector = mkSelector "subscribeAttributeSetupURLWithParams:subscriptionEstablished:reportHandler:"

-- | @Selector@ for @readAttributeSetupURLWithClusterStateCache:endpoint:queue:completion:@
readAttributeSetupURLWithClusterStateCache_endpoint_queue_completionSelector :: Selector '[Id MTRClusterStateCacheContainer, Id NSNumber, Id NSObject, Ptr ()] ()
readAttributeSetupURLWithClusterStateCache_endpoint_queue_completionSelector = mkSelector "readAttributeSetupURLWithClusterStateCache:endpoint:queue:completion:"

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
initSelector :: Selector '[] (Id MTRBaseClusterActions)
initSelector = mkSelector "init"

-- | @Selector@ for @new@
newSelector :: Selector '[] (Id MTRBaseClusterActions)
newSelector = mkSelector "new"

-- | @Selector@ for @initWithDevice:endpoint:queue:@
initWithDevice_endpoint_queueSelector :: Selector '[Id MTRBaseDevice, CUShort, Id NSObject] (Id MTRBaseClusterActions)
initWithDevice_endpoint_queueSelector = mkSelector "initWithDevice:endpoint:queue:"

-- | @Selector@ for @instantActionWithParams:completionHandler:@
instantActionWithParams_completionHandlerSelector :: Selector '[Id MTRActionsClusterInstantActionParams, Ptr ()] ()
instantActionWithParams_completionHandlerSelector = mkSelector "instantActionWithParams:completionHandler:"

-- | @Selector@ for @instantActionWithTransitionWithParams:completionHandler:@
instantActionWithTransitionWithParams_completionHandlerSelector :: Selector '[Id MTRActionsClusterInstantActionWithTransitionParams, Ptr ()] ()
instantActionWithTransitionWithParams_completionHandlerSelector = mkSelector "instantActionWithTransitionWithParams:completionHandler:"

-- | @Selector@ for @startActionWithParams:completionHandler:@
startActionWithParams_completionHandlerSelector :: Selector '[Id MTRActionsClusterStartActionParams, Ptr ()] ()
startActionWithParams_completionHandlerSelector = mkSelector "startActionWithParams:completionHandler:"

-- | @Selector@ for @startActionWithDurationWithParams:completionHandler:@
startActionWithDurationWithParams_completionHandlerSelector :: Selector '[Id MTRActionsClusterStartActionWithDurationParams, Ptr ()] ()
startActionWithDurationWithParams_completionHandlerSelector = mkSelector "startActionWithDurationWithParams:completionHandler:"

-- | @Selector@ for @stopActionWithParams:completionHandler:@
stopActionWithParams_completionHandlerSelector :: Selector '[Id MTRActionsClusterStopActionParams, Ptr ()] ()
stopActionWithParams_completionHandlerSelector = mkSelector "stopActionWithParams:completionHandler:"

-- | @Selector@ for @pauseActionWithParams:completionHandler:@
pauseActionWithParams_completionHandlerSelector :: Selector '[Id MTRActionsClusterPauseActionParams, Ptr ()] ()
pauseActionWithParams_completionHandlerSelector = mkSelector "pauseActionWithParams:completionHandler:"

-- | @Selector@ for @pauseActionWithDurationWithParams:completionHandler:@
pauseActionWithDurationWithParams_completionHandlerSelector :: Selector '[Id MTRActionsClusterPauseActionWithDurationParams, Ptr ()] ()
pauseActionWithDurationWithParams_completionHandlerSelector = mkSelector "pauseActionWithDurationWithParams:completionHandler:"

-- | @Selector@ for @resumeActionWithParams:completionHandler:@
resumeActionWithParams_completionHandlerSelector :: Selector '[Id MTRActionsClusterResumeActionParams, Ptr ()] ()
resumeActionWithParams_completionHandlerSelector = mkSelector "resumeActionWithParams:completionHandler:"

-- | @Selector@ for @enableActionWithParams:completionHandler:@
enableActionWithParams_completionHandlerSelector :: Selector '[Id MTRActionsClusterEnableActionParams, Ptr ()] ()
enableActionWithParams_completionHandlerSelector = mkSelector "enableActionWithParams:completionHandler:"

-- | @Selector@ for @enableActionWithDurationWithParams:completionHandler:@
enableActionWithDurationWithParams_completionHandlerSelector :: Selector '[Id MTRActionsClusterEnableActionWithDurationParams, Ptr ()] ()
enableActionWithDurationWithParams_completionHandlerSelector = mkSelector "enableActionWithDurationWithParams:completionHandler:"

-- | @Selector@ for @disableActionWithParams:completionHandler:@
disableActionWithParams_completionHandlerSelector :: Selector '[Id MTRActionsClusterDisableActionParams, Ptr ()] ()
disableActionWithParams_completionHandlerSelector = mkSelector "disableActionWithParams:completionHandler:"

-- | @Selector@ for @disableActionWithDurationWithParams:completionHandler:@
disableActionWithDurationWithParams_completionHandlerSelector :: Selector '[Id MTRActionsClusterDisableActionWithDurationParams, Ptr ()] ()
disableActionWithDurationWithParams_completionHandlerSelector = mkSelector "disableActionWithDurationWithParams:completionHandler:"

-- | @Selector@ for @readAttributeActionListWithCompletionHandler:@
readAttributeActionListWithCompletionHandlerSelector :: Selector '[Ptr ()] ()
readAttributeActionListWithCompletionHandlerSelector = mkSelector "readAttributeActionListWithCompletionHandler:"

-- | @Selector@ for @subscribeAttributeActionListWithMinInterval:maxInterval:params:subscriptionEstablished:reportHandler:@
subscribeAttributeActionListWithMinInterval_maxInterval_params_subscriptionEstablished_reportHandlerSelector :: Selector '[Id NSNumber, Id NSNumber, Id MTRSubscribeParams, Ptr (), Ptr ()] ()
subscribeAttributeActionListWithMinInterval_maxInterval_params_subscriptionEstablished_reportHandlerSelector = mkSelector "subscribeAttributeActionListWithMinInterval:maxInterval:params:subscriptionEstablished:reportHandler:"

-- | @Selector@ for @readAttributeActionListWithAttributeCache:endpoint:queue:completionHandler:@
readAttributeActionListWithAttributeCache_endpoint_queue_completionHandlerSelector :: Selector '[Id MTRAttributeCacheContainer, Id NSNumber, Id NSObject, Ptr ()] ()
readAttributeActionListWithAttributeCache_endpoint_queue_completionHandlerSelector = mkSelector "readAttributeActionListWithAttributeCache:endpoint:queue:completionHandler:"

-- | @Selector@ for @readAttributeEndpointListsWithCompletionHandler:@
readAttributeEndpointListsWithCompletionHandlerSelector :: Selector '[Ptr ()] ()
readAttributeEndpointListsWithCompletionHandlerSelector = mkSelector "readAttributeEndpointListsWithCompletionHandler:"

-- | @Selector@ for @subscribeAttributeEndpointListsWithMinInterval:maxInterval:params:subscriptionEstablished:reportHandler:@
subscribeAttributeEndpointListsWithMinInterval_maxInterval_params_subscriptionEstablished_reportHandlerSelector :: Selector '[Id NSNumber, Id NSNumber, Id MTRSubscribeParams, Ptr (), Ptr ()] ()
subscribeAttributeEndpointListsWithMinInterval_maxInterval_params_subscriptionEstablished_reportHandlerSelector = mkSelector "subscribeAttributeEndpointListsWithMinInterval:maxInterval:params:subscriptionEstablished:reportHandler:"

-- | @Selector@ for @readAttributeEndpointListsWithAttributeCache:endpoint:queue:completionHandler:@
readAttributeEndpointListsWithAttributeCache_endpoint_queue_completionHandlerSelector :: Selector '[Id MTRAttributeCacheContainer, Id NSNumber, Id NSObject, Ptr ()] ()
readAttributeEndpointListsWithAttributeCache_endpoint_queue_completionHandlerSelector = mkSelector "readAttributeEndpointListsWithAttributeCache:endpoint:queue:completionHandler:"

-- | @Selector@ for @readAttributeSetupURLWithCompletionHandler:@
readAttributeSetupURLWithCompletionHandlerSelector :: Selector '[Ptr ()] ()
readAttributeSetupURLWithCompletionHandlerSelector = mkSelector "readAttributeSetupURLWithCompletionHandler:"

-- | @Selector@ for @subscribeAttributeSetupURLWithMinInterval:maxInterval:params:subscriptionEstablished:reportHandler:@
subscribeAttributeSetupURLWithMinInterval_maxInterval_params_subscriptionEstablished_reportHandlerSelector :: Selector '[Id NSNumber, Id NSNumber, Id MTRSubscribeParams, Ptr (), Ptr ()] ()
subscribeAttributeSetupURLWithMinInterval_maxInterval_params_subscriptionEstablished_reportHandlerSelector = mkSelector "subscribeAttributeSetupURLWithMinInterval:maxInterval:params:subscriptionEstablished:reportHandler:"

-- | @Selector@ for @readAttributeSetupURLWithAttributeCache:endpoint:queue:completionHandler:@
readAttributeSetupURLWithAttributeCache_endpoint_queue_completionHandlerSelector :: Selector '[Id MTRAttributeCacheContainer, Id NSNumber, Id NSObject, Ptr ()] ()
readAttributeSetupURLWithAttributeCache_endpoint_queue_completionHandlerSelector = mkSelector "readAttributeSetupURLWithAttributeCache:endpoint:queue:completionHandler:"

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
initWithDevice_endpointID_queueSelector :: Selector '[Id MTRBaseDevice, Id NSNumber, Id NSObject] (Id MTRBaseClusterActions)
initWithDevice_endpointID_queueSelector = mkSelector "initWithDevice:endpointID:queue:"

