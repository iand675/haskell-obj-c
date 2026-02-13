{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Cluster Media Input
--
-- This cluster provides an interface for controlling the Input Selector on a media device such as a TV.
--
-- Generated bindings for @MTRBaseClusterMediaInput@.
module ObjC.Matter.MTRBaseClusterMediaInput
  ( MTRBaseClusterMediaInput
  , IsMTRBaseClusterMediaInput(..)
  , selectInputWithParams_completion
  , showInputStatusWithParams_completion
  , showInputStatusWithCompletion
  , hideInputStatusWithParams_completion
  , hideInputStatusWithCompletion
  , renameInputWithParams_completion
  , readAttributeInputListWithCompletion
  , subscribeAttributeInputListWithParams_subscriptionEstablished_reportHandler
  , readAttributeInputListWithClusterStateCache_endpoint_queue_completion
  , readAttributeCurrentInputWithCompletion
  , subscribeAttributeCurrentInputWithParams_subscriptionEstablished_reportHandler
  , readAttributeCurrentInputWithClusterStateCache_endpoint_queue_completion
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
  , selectInputWithParams_completionHandler
  , showInputStatusWithParams_completionHandler
  , showInputStatusWithCompletionHandler
  , hideInputStatusWithParams_completionHandler
  , hideInputStatusWithCompletionHandler
  , renameInputWithParams_completionHandler
  , readAttributeInputListWithCompletionHandler
  , subscribeAttributeInputListWithMinInterval_maxInterval_params_subscriptionEstablished_reportHandler
  , readAttributeInputListWithAttributeCache_endpoint_queue_completionHandler
  , readAttributeCurrentInputWithCompletionHandler
  , subscribeAttributeCurrentInputWithMinInterval_maxInterval_params_subscriptionEstablished_reportHandler
  , readAttributeCurrentInputWithAttributeCache_endpoint_queue_completionHandler
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
  , hideInputStatusWithCompletionHandlerSelector
  , hideInputStatusWithCompletionSelector
  , hideInputStatusWithParams_completionHandlerSelector
  , hideInputStatusWithParams_completionSelector
  , initSelector
  , initWithDevice_endpointID_queueSelector
  , initWithDevice_endpoint_queueSelector
  , newSelector
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
  , readAttributeCurrentInputWithAttributeCache_endpoint_queue_completionHandlerSelector
  , readAttributeCurrentInputWithClusterStateCache_endpoint_queue_completionSelector
  , readAttributeCurrentInputWithCompletionHandlerSelector
  , readAttributeCurrentInputWithCompletionSelector
  , readAttributeFeatureMapWithAttributeCache_endpoint_queue_completionHandlerSelector
  , readAttributeFeatureMapWithClusterStateCache_endpoint_queue_completionSelector
  , readAttributeFeatureMapWithCompletionHandlerSelector
  , readAttributeFeatureMapWithCompletionSelector
  , readAttributeGeneratedCommandListWithAttributeCache_endpoint_queue_completionHandlerSelector
  , readAttributeGeneratedCommandListWithClusterStateCache_endpoint_queue_completionSelector
  , readAttributeGeneratedCommandListWithCompletionHandlerSelector
  , readAttributeGeneratedCommandListWithCompletionSelector
  , readAttributeInputListWithAttributeCache_endpoint_queue_completionHandlerSelector
  , readAttributeInputListWithClusterStateCache_endpoint_queue_completionSelector
  , readAttributeInputListWithCompletionHandlerSelector
  , readAttributeInputListWithCompletionSelector
  , renameInputWithParams_completionHandlerSelector
  , renameInputWithParams_completionSelector
  , selectInputWithParams_completionHandlerSelector
  , selectInputWithParams_completionSelector
  , showInputStatusWithCompletionHandlerSelector
  , showInputStatusWithCompletionSelector
  , showInputStatusWithParams_completionHandlerSelector
  , showInputStatusWithParams_completionSelector
  , subscribeAttributeAcceptedCommandListWithMinInterval_maxInterval_params_subscriptionEstablished_reportHandlerSelector
  , subscribeAttributeAcceptedCommandListWithParams_subscriptionEstablished_reportHandlerSelector
  , subscribeAttributeAttributeListWithMinInterval_maxInterval_params_subscriptionEstablished_reportHandlerSelector
  , subscribeAttributeAttributeListWithParams_subscriptionEstablished_reportHandlerSelector
  , subscribeAttributeClusterRevisionWithMinInterval_maxInterval_params_subscriptionEstablished_reportHandlerSelector
  , subscribeAttributeClusterRevisionWithParams_subscriptionEstablished_reportHandlerSelector
  , subscribeAttributeCurrentInputWithMinInterval_maxInterval_params_subscriptionEstablished_reportHandlerSelector
  , subscribeAttributeCurrentInputWithParams_subscriptionEstablished_reportHandlerSelector
  , subscribeAttributeFeatureMapWithMinInterval_maxInterval_params_subscriptionEstablished_reportHandlerSelector
  , subscribeAttributeFeatureMapWithParams_subscriptionEstablished_reportHandlerSelector
  , subscribeAttributeGeneratedCommandListWithMinInterval_maxInterval_params_subscriptionEstablished_reportHandlerSelector
  , subscribeAttributeGeneratedCommandListWithParams_subscriptionEstablished_reportHandlerSelector
  , subscribeAttributeInputListWithMinInterval_maxInterval_params_subscriptionEstablished_reportHandlerSelector
  , subscribeAttributeInputListWithParams_subscriptionEstablished_reportHandlerSelector


  ) where

import Foreign.Ptr (Ptr, FunPtr)
import Foreign.C.Types

import ObjC.Runtime.Types
import ObjC.Runtime.Message (sendMessage, sendOwnedMessage, sendClassMessage, sendOwnedClassMessage)
import ObjC.Runtime.Selector (mkSelector)
import ObjC.Runtime.Class (getRequiredClass)

import ObjC.Matter.Internal.Classes
import ObjC.Foundation.Internal.Classes

-- | Command SelectInput
--
-- Upon receipt, this command SHALL change the media input on the device to the input at a specific index in the Input List.
--
-- ObjC selector: @- selectInputWithParams:completion:@
selectInputWithParams_completion :: (IsMTRBaseClusterMediaInput mtrBaseClusterMediaInput, IsMTRMediaInputClusterSelectInputParams params) => mtrBaseClusterMediaInput -> params -> Ptr () -> IO ()
selectInputWithParams_completion mtrBaseClusterMediaInput params completion =
  sendMessage mtrBaseClusterMediaInput selectInputWithParams_completionSelector (toMTRMediaInputClusterSelectInputParams params) completion

-- | Command ShowInputStatus
--
-- Upon receipt, this command SHALL display the active status of the input list on screen.
--
-- ObjC selector: @- showInputStatusWithParams:completion:@
showInputStatusWithParams_completion :: (IsMTRBaseClusterMediaInput mtrBaseClusterMediaInput, IsMTRMediaInputClusterShowInputStatusParams params) => mtrBaseClusterMediaInput -> params -> Ptr () -> IO ()
showInputStatusWithParams_completion mtrBaseClusterMediaInput params completion =
  sendMessage mtrBaseClusterMediaInput showInputStatusWithParams_completionSelector (toMTRMediaInputClusterShowInputStatusParams params) completion

-- | @- showInputStatusWithCompletion:@
showInputStatusWithCompletion :: IsMTRBaseClusterMediaInput mtrBaseClusterMediaInput => mtrBaseClusterMediaInput -> Ptr () -> IO ()
showInputStatusWithCompletion mtrBaseClusterMediaInput completion =
  sendMessage mtrBaseClusterMediaInput showInputStatusWithCompletionSelector completion

-- | Command HideInputStatus
--
-- Upon receipt, this command SHALL hide the input list from the screen.
--
-- ObjC selector: @- hideInputStatusWithParams:completion:@
hideInputStatusWithParams_completion :: (IsMTRBaseClusterMediaInput mtrBaseClusterMediaInput, IsMTRMediaInputClusterHideInputStatusParams params) => mtrBaseClusterMediaInput -> params -> Ptr () -> IO ()
hideInputStatusWithParams_completion mtrBaseClusterMediaInput params completion =
  sendMessage mtrBaseClusterMediaInput hideInputStatusWithParams_completionSelector (toMTRMediaInputClusterHideInputStatusParams params) completion

-- | @- hideInputStatusWithCompletion:@
hideInputStatusWithCompletion :: IsMTRBaseClusterMediaInput mtrBaseClusterMediaInput => mtrBaseClusterMediaInput -> Ptr () -> IO ()
hideInputStatusWithCompletion mtrBaseClusterMediaInput completion =
  sendMessage mtrBaseClusterMediaInput hideInputStatusWithCompletionSelector completion

-- | Command RenameInput
--
-- Upon receipt, this command SHALL rename the input at a specific index in the Input List.
--
-- ObjC selector: @- renameInputWithParams:completion:@
renameInputWithParams_completion :: (IsMTRBaseClusterMediaInput mtrBaseClusterMediaInput, IsMTRMediaInputClusterRenameInputParams params) => mtrBaseClusterMediaInput -> params -> Ptr () -> IO ()
renameInputWithParams_completion mtrBaseClusterMediaInput params completion =
  sendMessage mtrBaseClusterMediaInput renameInputWithParams_completionSelector (toMTRMediaInputClusterRenameInputParams params) completion

-- | @- readAttributeInputListWithCompletion:@
readAttributeInputListWithCompletion :: IsMTRBaseClusterMediaInput mtrBaseClusterMediaInput => mtrBaseClusterMediaInput -> Ptr () -> IO ()
readAttributeInputListWithCompletion mtrBaseClusterMediaInput completion =
  sendMessage mtrBaseClusterMediaInput readAttributeInputListWithCompletionSelector completion

-- | @- subscribeAttributeInputListWithParams:subscriptionEstablished:reportHandler:@
subscribeAttributeInputListWithParams_subscriptionEstablished_reportHandler :: (IsMTRBaseClusterMediaInput mtrBaseClusterMediaInput, IsMTRSubscribeParams params) => mtrBaseClusterMediaInput -> params -> Ptr () -> Ptr () -> IO ()
subscribeAttributeInputListWithParams_subscriptionEstablished_reportHandler mtrBaseClusterMediaInput params subscriptionEstablished reportHandler =
  sendMessage mtrBaseClusterMediaInput subscribeAttributeInputListWithParams_subscriptionEstablished_reportHandlerSelector (toMTRSubscribeParams params) subscriptionEstablished reportHandler

-- | @+ readAttributeInputListWithClusterStateCache:endpoint:queue:completion:@
readAttributeInputListWithClusterStateCache_endpoint_queue_completion :: (IsMTRClusterStateCacheContainer clusterStateCacheContainer, IsNSNumber endpoint, IsNSObject queue) => clusterStateCacheContainer -> endpoint -> queue -> Ptr () -> IO ()
readAttributeInputListWithClusterStateCache_endpoint_queue_completion clusterStateCacheContainer endpoint queue completion =
  do
    cls' <- getRequiredClass "MTRBaseClusterMediaInput"
    sendClassMessage cls' readAttributeInputListWithClusterStateCache_endpoint_queue_completionSelector (toMTRClusterStateCacheContainer clusterStateCacheContainer) (toNSNumber endpoint) (toNSObject queue) completion

-- | @- readAttributeCurrentInputWithCompletion:@
readAttributeCurrentInputWithCompletion :: IsMTRBaseClusterMediaInput mtrBaseClusterMediaInput => mtrBaseClusterMediaInput -> Ptr () -> IO ()
readAttributeCurrentInputWithCompletion mtrBaseClusterMediaInput completion =
  sendMessage mtrBaseClusterMediaInput readAttributeCurrentInputWithCompletionSelector completion

-- | @- subscribeAttributeCurrentInputWithParams:subscriptionEstablished:reportHandler:@
subscribeAttributeCurrentInputWithParams_subscriptionEstablished_reportHandler :: (IsMTRBaseClusterMediaInput mtrBaseClusterMediaInput, IsMTRSubscribeParams params) => mtrBaseClusterMediaInput -> params -> Ptr () -> Ptr () -> IO ()
subscribeAttributeCurrentInputWithParams_subscriptionEstablished_reportHandler mtrBaseClusterMediaInput params subscriptionEstablished reportHandler =
  sendMessage mtrBaseClusterMediaInput subscribeAttributeCurrentInputWithParams_subscriptionEstablished_reportHandlerSelector (toMTRSubscribeParams params) subscriptionEstablished reportHandler

-- | @+ readAttributeCurrentInputWithClusterStateCache:endpoint:queue:completion:@
readAttributeCurrentInputWithClusterStateCache_endpoint_queue_completion :: (IsMTRClusterStateCacheContainer clusterStateCacheContainer, IsNSNumber endpoint, IsNSObject queue) => clusterStateCacheContainer -> endpoint -> queue -> Ptr () -> IO ()
readAttributeCurrentInputWithClusterStateCache_endpoint_queue_completion clusterStateCacheContainer endpoint queue completion =
  do
    cls' <- getRequiredClass "MTRBaseClusterMediaInput"
    sendClassMessage cls' readAttributeCurrentInputWithClusterStateCache_endpoint_queue_completionSelector (toMTRClusterStateCacheContainer clusterStateCacheContainer) (toNSNumber endpoint) (toNSObject queue) completion

-- | @- readAttributeGeneratedCommandListWithCompletion:@
readAttributeGeneratedCommandListWithCompletion :: IsMTRBaseClusterMediaInput mtrBaseClusterMediaInput => mtrBaseClusterMediaInput -> Ptr () -> IO ()
readAttributeGeneratedCommandListWithCompletion mtrBaseClusterMediaInput completion =
  sendMessage mtrBaseClusterMediaInput readAttributeGeneratedCommandListWithCompletionSelector completion

-- | @- subscribeAttributeGeneratedCommandListWithParams:subscriptionEstablished:reportHandler:@
subscribeAttributeGeneratedCommandListWithParams_subscriptionEstablished_reportHandler :: (IsMTRBaseClusterMediaInput mtrBaseClusterMediaInput, IsMTRSubscribeParams params) => mtrBaseClusterMediaInput -> params -> Ptr () -> Ptr () -> IO ()
subscribeAttributeGeneratedCommandListWithParams_subscriptionEstablished_reportHandler mtrBaseClusterMediaInput params subscriptionEstablished reportHandler =
  sendMessage mtrBaseClusterMediaInput subscribeAttributeGeneratedCommandListWithParams_subscriptionEstablished_reportHandlerSelector (toMTRSubscribeParams params) subscriptionEstablished reportHandler

-- | @+ readAttributeGeneratedCommandListWithClusterStateCache:endpoint:queue:completion:@
readAttributeGeneratedCommandListWithClusterStateCache_endpoint_queue_completion :: (IsMTRClusterStateCacheContainer clusterStateCacheContainer, IsNSNumber endpoint, IsNSObject queue) => clusterStateCacheContainer -> endpoint -> queue -> Ptr () -> IO ()
readAttributeGeneratedCommandListWithClusterStateCache_endpoint_queue_completion clusterStateCacheContainer endpoint queue completion =
  do
    cls' <- getRequiredClass "MTRBaseClusterMediaInput"
    sendClassMessage cls' readAttributeGeneratedCommandListWithClusterStateCache_endpoint_queue_completionSelector (toMTRClusterStateCacheContainer clusterStateCacheContainer) (toNSNumber endpoint) (toNSObject queue) completion

-- | @- readAttributeAcceptedCommandListWithCompletion:@
readAttributeAcceptedCommandListWithCompletion :: IsMTRBaseClusterMediaInput mtrBaseClusterMediaInput => mtrBaseClusterMediaInput -> Ptr () -> IO ()
readAttributeAcceptedCommandListWithCompletion mtrBaseClusterMediaInput completion =
  sendMessage mtrBaseClusterMediaInput readAttributeAcceptedCommandListWithCompletionSelector completion

-- | @- subscribeAttributeAcceptedCommandListWithParams:subscriptionEstablished:reportHandler:@
subscribeAttributeAcceptedCommandListWithParams_subscriptionEstablished_reportHandler :: (IsMTRBaseClusterMediaInput mtrBaseClusterMediaInput, IsMTRSubscribeParams params) => mtrBaseClusterMediaInput -> params -> Ptr () -> Ptr () -> IO ()
subscribeAttributeAcceptedCommandListWithParams_subscriptionEstablished_reportHandler mtrBaseClusterMediaInput params subscriptionEstablished reportHandler =
  sendMessage mtrBaseClusterMediaInput subscribeAttributeAcceptedCommandListWithParams_subscriptionEstablished_reportHandlerSelector (toMTRSubscribeParams params) subscriptionEstablished reportHandler

-- | @+ readAttributeAcceptedCommandListWithClusterStateCache:endpoint:queue:completion:@
readAttributeAcceptedCommandListWithClusterStateCache_endpoint_queue_completion :: (IsMTRClusterStateCacheContainer clusterStateCacheContainer, IsNSNumber endpoint, IsNSObject queue) => clusterStateCacheContainer -> endpoint -> queue -> Ptr () -> IO ()
readAttributeAcceptedCommandListWithClusterStateCache_endpoint_queue_completion clusterStateCacheContainer endpoint queue completion =
  do
    cls' <- getRequiredClass "MTRBaseClusterMediaInput"
    sendClassMessage cls' readAttributeAcceptedCommandListWithClusterStateCache_endpoint_queue_completionSelector (toMTRClusterStateCacheContainer clusterStateCacheContainer) (toNSNumber endpoint) (toNSObject queue) completion

-- | @- readAttributeAttributeListWithCompletion:@
readAttributeAttributeListWithCompletion :: IsMTRBaseClusterMediaInput mtrBaseClusterMediaInput => mtrBaseClusterMediaInput -> Ptr () -> IO ()
readAttributeAttributeListWithCompletion mtrBaseClusterMediaInput completion =
  sendMessage mtrBaseClusterMediaInput readAttributeAttributeListWithCompletionSelector completion

-- | @- subscribeAttributeAttributeListWithParams:subscriptionEstablished:reportHandler:@
subscribeAttributeAttributeListWithParams_subscriptionEstablished_reportHandler :: (IsMTRBaseClusterMediaInput mtrBaseClusterMediaInput, IsMTRSubscribeParams params) => mtrBaseClusterMediaInput -> params -> Ptr () -> Ptr () -> IO ()
subscribeAttributeAttributeListWithParams_subscriptionEstablished_reportHandler mtrBaseClusterMediaInput params subscriptionEstablished reportHandler =
  sendMessage mtrBaseClusterMediaInput subscribeAttributeAttributeListWithParams_subscriptionEstablished_reportHandlerSelector (toMTRSubscribeParams params) subscriptionEstablished reportHandler

-- | @+ readAttributeAttributeListWithClusterStateCache:endpoint:queue:completion:@
readAttributeAttributeListWithClusterStateCache_endpoint_queue_completion :: (IsMTRClusterStateCacheContainer clusterStateCacheContainer, IsNSNumber endpoint, IsNSObject queue) => clusterStateCacheContainer -> endpoint -> queue -> Ptr () -> IO ()
readAttributeAttributeListWithClusterStateCache_endpoint_queue_completion clusterStateCacheContainer endpoint queue completion =
  do
    cls' <- getRequiredClass "MTRBaseClusterMediaInput"
    sendClassMessage cls' readAttributeAttributeListWithClusterStateCache_endpoint_queue_completionSelector (toMTRClusterStateCacheContainer clusterStateCacheContainer) (toNSNumber endpoint) (toNSObject queue) completion

-- | @- readAttributeFeatureMapWithCompletion:@
readAttributeFeatureMapWithCompletion :: IsMTRBaseClusterMediaInput mtrBaseClusterMediaInput => mtrBaseClusterMediaInput -> Ptr () -> IO ()
readAttributeFeatureMapWithCompletion mtrBaseClusterMediaInput completion =
  sendMessage mtrBaseClusterMediaInput readAttributeFeatureMapWithCompletionSelector completion

-- | @- subscribeAttributeFeatureMapWithParams:subscriptionEstablished:reportHandler:@
subscribeAttributeFeatureMapWithParams_subscriptionEstablished_reportHandler :: (IsMTRBaseClusterMediaInput mtrBaseClusterMediaInput, IsMTRSubscribeParams params) => mtrBaseClusterMediaInput -> params -> Ptr () -> Ptr () -> IO ()
subscribeAttributeFeatureMapWithParams_subscriptionEstablished_reportHandler mtrBaseClusterMediaInput params subscriptionEstablished reportHandler =
  sendMessage mtrBaseClusterMediaInput subscribeAttributeFeatureMapWithParams_subscriptionEstablished_reportHandlerSelector (toMTRSubscribeParams params) subscriptionEstablished reportHandler

-- | @+ readAttributeFeatureMapWithClusterStateCache:endpoint:queue:completion:@
readAttributeFeatureMapWithClusterStateCache_endpoint_queue_completion :: (IsMTRClusterStateCacheContainer clusterStateCacheContainer, IsNSNumber endpoint, IsNSObject queue) => clusterStateCacheContainer -> endpoint -> queue -> Ptr () -> IO ()
readAttributeFeatureMapWithClusterStateCache_endpoint_queue_completion clusterStateCacheContainer endpoint queue completion =
  do
    cls' <- getRequiredClass "MTRBaseClusterMediaInput"
    sendClassMessage cls' readAttributeFeatureMapWithClusterStateCache_endpoint_queue_completionSelector (toMTRClusterStateCacheContainer clusterStateCacheContainer) (toNSNumber endpoint) (toNSObject queue) completion

-- | @- readAttributeClusterRevisionWithCompletion:@
readAttributeClusterRevisionWithCompletion :: IsMTRBaseClusterMediaInput mtrBaseClusterMediaInput => mtrBaseClusterMediaInput -> Ptr () -> IO ()
readAttributeClusterRevisionWithCompletion mtrBaseClusterMediaInput completion =
  sendMessage mtrBaseClusterMediaInput readAttributeClusterRevisionWithCompletionSelector completion

-- | @- subscribeAttributeClusterRevisionWithParams:subscriptionEstablished:reportHandler:@
subscribeAttributeClusterRevisionWithParams_subscriptionEstablished_reportHandler :: (IsMTRBaseClusterMediaInput mtrBaseClusterMediaInput, IsMTRSubscribeParams params) => mtrBaseClusterMediaInput -> params -> Ptr () -> Ptr () -> IO ()
subscribeAttributeClusterRevisionWithParams_subscriptionEstablished_reportHandler mtrBaseClusterMediaInput params subscriptionEstablished reportHandler =
  sendMessage mtrBaseClusterMediaInput subscribeAttributeClusterRevisionWithParams_subscriptionEstablished_reportHandlerSelector (toMTRSubscribeParams params) subscriptionEstablished reportHandler

-- | @+ readAttributeClusterRevisionWithClusterStateCache:endpoint:queue:completion:@
readAttributeClusterRevisionWithClusterStateCache_endpoint_queue_completion :: (IsMTRClusterStateCacheContainer clusterStateCacheContainer, IsNSNumber endpoint, IsNSObject queue) => clusterStateCacheContainer -> endpoint -> queue -> Ptr () -> IO ()
readAttributeClusterRevisionWithClusterStateCache_endpoint_queue_completion clusterStateCacheContainer endpoint queue completion =
  do
    cls' <- getRequiredClass "MTRBaseClusterMediaInput"
    sendClassMessage cls' readAttributeClusterRevisionWithClusterStateCache_endpoint_queue_completionSelector (toMTRClusterStateCacheContainer clusterStateCacheContainer) (toNSNumber endpoint) (toNSObject queue) completion

-- | @- init@
init_ :: IsMTRBaseClusterMediaInput mtrBaseClusterMediaInput => mtrBaseClusterMediaInput -> IO (Id MTRBaseClusterMediaInput)
init_ mtrBaseClusterMediaInput =
  sendOwnedMessage mtrBaseClusterMediaInput initSelector

-- | @+ new@
new :: IO (Id MTRBaseClusterMediaInput)
new  =
  do
    cls' <- getRequiredClass "MTRBaseClusterMediaInput"
    sendOwnedClassMessage cls' newSelector

-- | @- initWithDevice:endpoint:queue:@
initWithDevice_endpoint_queue :: (IsMTRBaseClusterMediaInput mtrBaseClusterMediaInput, IsMTRBaseDevice device, IsNSObject queue) => mtrBaseClusterMediaInput -> device -> CUShort -> queue -> IO (Id MTRBaseClusterMediaInput)
initWithDevice_endpoint_queue mtrBaseClusterMediaInput device endpoint queue =
  sendOwnedMessage mtrBaseClusterMediaInput initWithDevice_endpoint_queueSelector (toMTRBaseDevice device) endpoint (toNSObject queue)

-- | @- selectInputWithParams:completionHandler:@
selectInputWithParams_completionHandler :: (IsMTRBaseClusterMediaInput mtrBaseClusterMediaInput, IsMTRMediaInputClusterSelectInputParams params) => mtrBaseClusterMediaInput -> params -> Ptr () -> IO ()
selectInputWithParams_completionHandler mtrBaseClusterMediaInput params completionHandler =
  sendMessage mtrBaseClusterMediaInput selectInputWithParams_completionHandlerSelector (toMTRMediaInputClusterSelectInputParams params) completionHandler

-- | @- showInputStatusWithParams:completionHandler:@
showInputStatusWithParams_completionHandler :: (IsMTRBaseClusterMediaInput mtrBaseClusterMediaInput, IsMTRMediaInputClusterShowInputStatusParams params) => mtrBaseClusterMediaInput -> params -> Ptr () -> IO ()
showInputStatusWithParams_completionHandler mtrBaseClusterMediaInput params completionHandler =
  sendMessage mtrBaseClusterMediaInput showInputStatusWithParams_completionHandlerSelector (toMTRMediaInputClusterShowInputStatusParams params) completionHandler

-- | @- showInputStatusWithCompletionHandler:@
showInputStatusWithCompletionHandler :: IsMTRBaseClusterMediaInput mtrBaseClusterMediaInput => mtrBaseClusterMediaInput -> Ptr () -> IO ()
showInputStatusWithCompletionHandler mtrBaseClusterMediaInput completionHandler =
  sendMessage mtrBaseClusterMediaInput showInputStatusWithCompletionHandlerSelector completionHandler

-- | @- hideInputStatusWithParams:completionHandler:@
hideInputStatusWithParams_completionHandler :: (IsMTRBaseClusterMediaInput mtrBaseClusterMediaInput, IsMTRMediaInputClusterHideInputStatusParams params) => mtrBaseClusterMediaInput -> params -> Ptr () -> IO ()
hideInputStatusWithParams_completionHandler mtrBaseClusterMediaInput params completionHandler =
  sendMessage mtrBaseClusterMediaInput hideInputStatusWithParams_completionHandlerSelector (toMTRMediaInputClusterHideInputStatusParams params) completionHandler

-- | @- hideInputStatusWithCompletionHandler:@
hideInputStatusWithCompletionHandler :: IsMTRBaseClusterMediaInput mtrBaseClusterMediaInput => mtrBaseClusterMediaInput -> Ptr () -> IO ()
hideInputStatusWithCompletionHandler mtrBaseClusterMediaInput completionHandler =
  sendMessage mtrBaseClusterMediaInput hideInputStatusWithCompletionHandlerSelector completionHandler

-- | @- renameInputWithParams:completionHandler:@
renameInputWithParams_completionHandler :: (IsMTRBaseClusterMediaInput mtrBaseClusterMediaInput, IsMTRMediaInputClusterRenameInputParams params) => mtrBaseClusterMediaInput -> params -> Ptr () -> IO ()
renameInputWithParams_completionHandler mtrBaseClusterMediaInput params completionHandler =
  sendMessage mtrBaseClusterMediaInput renameInputWithParams_completionHandlerSelector (toMTRMediaInputClusterRenameInputParams params) completionHandler

-- | @- readAttributeInputListWithCompletionHandler:@
readAttributeInputListWithCompletionHandler :: IsMTRBaseClusterMediaInput mtrBaseClusterMediaInput => mtrBaseClusterMediaInput -> Ptr () -> IO ()
readAttributeInputListWithCompletionHandler mtrBaseClusterMediaInput completionHandler =
  sendMessage mtrBaseClusterMediaInput readAttributeInputListWithCompletionHandlerSelector completionHandler

-- | @- subscribeAttributeInputListWithMinInterval:maxInterval:params:subscriptionEstablished:reportHandler:@
subscribeAttributeInputListWithMinInterval_maxInterval_params_subscriptionEstablished_reportHandler :: (IsMTRBaseClusterMediaInput mtrBaseClusterMediaInput, IsNSNumber minInterval, IsNSNumber maxInterval, IsMTRSubscribeParams params) => mtrBaseClusterMediaInput -> minInterval -> maxInterval -> params -> Ptr () -> Ptr () -> IO ()
subscribeAttributeInputListWithMinInterval_maxInterval_params_subscriptionEstablished_reportHandler mtrBaseClusterMediaInput minInterval maxInterval params subscriptionEstablishedHandler reportHandler =
  sendMessage mtrBaseClusterMediaInput subscribeAttributeInputListWithMinInterval_maxInterval_params_subscriptionEstablished_reportHandlerSelector (toNSNumber minInterval) (toNSNumber maxInterval) (toMTRSubscribeParams params) subscriptionEstablishedHandler reportHandler

-- | @+ readAttributeInputListWithAttributeCache:endpoint:queue:completionHandler:@
readAttributeInputListWithAttributeCache_endpoint_queue_completionHandler :: (IsMTRAttributeCacheContainer attributeCacheContainer, IsNSNumber endpoint, IsNSObject queue) => attributeCacheContainer -> endpoint -> queue -> Ptr () -> IO ()
readAttributeInputListWithAttributeCache_endpoint_queue_completionHandler attributeCacheContainer endpoint queue completionHandler =
  do
    cls' <- getRequiredClass "MTRBaseClusterMediaInput"
    sendClassMessage cls' readAttributeInputListWithAttributeCache_endpoint_queue_completionHandlerSelector (toMTRAttributeCacheContainer attributeCacheContainer) (toNSNumber endpoint) (toNSObject queue) completionHandler

-- | @- readAttributeCurrentInputWithCompletionHandler:@
readAttributeCurrentInputWithCompletionHandler :: IsMTRBaseClusterMediaInput mtrBaseClusterMediaInput => mtrBaseClusterMediaInput -> Ptr () -> IO ()
readAttributeCurrentInputWithCompletionHandler mtrBaseClusterMediaInput completionHandler =
  sendMessage mtrBaseClusterMediaInput readAttributeCurrentInputWithCompletionHandlerSelector completionHandler

-- | @- subscribeAttributeCurrentInputWithMinInterval:maxInterval:params:subscriptionEstablished:reportHandler:@
subscribeAttributeCurrentInputWithMinInterval_maxInterval_params_subscriptionEstablished_reportHandler :: (IsMTRBaseClusterMediaInput mtrBaseClusterMediaInput, IsNSNumber minInterval, IsNSNumber maxInterval, IsMTRSubscribeParams params) => mtrBaseClusterMediaInput -> minInterval -> maxInterval -> params -> Ptr () -> Ptr () -> IO ()
subscribeAttributeCurrentInputWithMinInterval_maxInterval_params_subscriptionEstablished_reportHandler mtrBaseClusterMediaInput minInterval maxInterval params subscriptionEstablishedHandler reportHandler =
  sendMessage mtrBaseClusterMediaInput subscribeAttributeCurrentInputWithMinInterval_maxInterval_params_subscriptionEstablished_reportHandlerSelector (toNSNumber minInterval) (toNSNumber maxInterval) (toMTRSubscribeParams params) subscriptionEstablishedHandler reportHandler

-- | @+ readAttributeCurrentInputWithAttributeCache:endpoint:queue:completionHandler:@
readAttributeCurrentInputWithAttributeCache_endpoint_queue_completionHandler :: (IsMTRAttributeCacheContainer attributeCacheContainer, IsNSNumber endpoint, IsNSObject queue) => attributeCacheContainer -> endpoint -> queue -> Ptr () -> IO ()
readAttributeCurrentInputWithAttributeCache_endpoint_queue_completionHandler attributeCacheContainer endpoint queue completionHandler =
  do
    cls' <- getRequiredClass "MTRBaseClusterMediaInput"
    sendClassMessage cls' readAttributeCurrentInputWithAttributeCache_endpoint_queue_completionHandlerSelector (toMTRAttributeCacheContainer attributeCacheContainer) (toNSNumber endpoint) (toNSObject queue) completionHandler

-- | @- readAttributeGeneratedCommandListWithCompletionHandler:@
readAttributeGeneratedCommandListWithCompletionHandler :: IsMTRBaseClusterMediaInput mtrBaseClusterMediaInput => mtrBaseClusterMediaInput -> Ptr () -> IO ()
readAttributeGeneratedCommandListWithCompletionHandler mtrBaseClusterMediaInput completionHandler =
  sendMessage mtrBaseClusterMediaInput readAttributeGeneratedCommandListWithCompletionHandlerSelector completionHandler

-- | @- subscribeAttributeGeneratedCommandListWithMinInterval:maxInterval:params:subscriptionEstablished:reportHandler:@
subscribeAttributeGeneratedCommandListWithMinInterval_maxInterval_params_subscriptionEstablished_reportHandler :: (IsMTRBaseClusterMediaInput mtrBaseClusterMediaInput, IsNSNumber minInterval, IsNSNumber maxInterval, IsMTRSubscribeParams params) => mtrBaseClusterMediaInput -> minInterval -> maxInterval -> params -> Ptr () -> Ptr () -> IO ()
subscribeAttributeGeneratedCommandListWithMinInterval_maxInterval_params_subscriptionEstablished_reportHandler mtrBaseClusterMediaInput minInterval maxInterval params subscriptionEstablishedHandler reportHandler =
  sendMessage mtrBaseClusterMediaInput subscribeAttributeGeneratedCommandListWithMinInterval_maxInterval_params_subscriptionEstablished_reportHandlerSelector (toNSNumber minInterval) (toNSNumber maxInterval) (toMTRSubscribeParams params) subscriptionEstablishedHandler reportHandler

-- | @+ readAttributeGeneratedCommandListWithAttributeCache:endpoint:queue:completionHandler:@
readAttributeGeneratedCommandListWithAttributeCache_endpoint_queue_completionHandler :: (IsMTRAttributeCacheContainer attributeCacheContainer, IsNSNumber endpoint, IsNSObject queue) => attributeCacheContainer -> endpoint -> queue -> Ptr () -> IO ()
readAttributeGeneratedCommandListWithAttributeCache_endpoint_queue_completionHandler attributeCacheContainer endpoint queue completionHandler =
  do
    cls' <- getRequiredClass "MTRBaseClusterMediaInput"
    sendClassMessage cls' readAttributeGeneratedCommandListWithAttributeCache_endpoint_queue_completionHandlerSelector (toMTRAttributeCacheContainer attributeCacheContainer) (toNSNumber endpoint) (toNSObject queue) completionHandler

-- | @- readAttributeAcceptedCommandListWithCompletionHandler:@
readAttributeAcceptedCommandListWithCompletionHandler :: IsMTRBaseClusterMediaInput mtrBaseClusterMediaInput => mtrBaseClusterMediaInput -> Ptr () -> IO ()
readAttributeAcceptedCommandListWithCompletionHandler mtrBaseClusterMediaInput completionHandler =
  sendMessage mtrBaseClusterMediaInput readAttributeAcceptedCommandListWithCompletionHandlerSelector completionHandler

-- | @- subscribeAttributeAcceptedCommandListWithMinInterval:maxInterval:params:subscriptionEstablished:reportHandler:@
subscribeAttributeAcceptedCommandListWithMinInterval_maxInterval_params_subscriptionEstablished_reportHandler :: (IsMTRBaseClusterMediaInput mtrBaseClusterMediaInput, IsNSNumber minInterval, IsNSNumber maxInterval, IsMTRSubscribeParams params) => mtrBaseClusterMediaInput -> minInterval -> maxInterval -> params -> Ptr () -> Ptr () -> IO ()
subscribeAttributeAcceptedCommandListWithMinInterval_maxInterval_params_subscriptionEstablished_reportHandler mtrBaseClusterMediaInput minInterval maxInterval params subscriptionEstablishedHandler reportHandler =
  sendMessage mtrBaseClusterMediaInput subscribeAttributeAcceptedCommandListWithMinInterval_maxInterval_params_subscriptionEstablished_reportHandlerSelector (toNSNumber minInterval) (toNSNumber maxInterval) (toMTRSubscribeParams params) subscriptionEstablishedHandler reportHandler

-- | @+ readAttributeAcceptedCommandListWithAttributeCache:endpoint:queue:completionHandler:@
readAttributeAcceptedCommandListWithAttributeCache_endpoint_queue_completionHandler :: (IsMTRAttributeCacheContainer attributeCacheContainer, IsNSNumber endpoint, IsNSObject queue) => attributeCacheContainer -> endpoint -> queue -> Ptr () -> IO ()
readAttributeAcceptedCommandListWithAttributeCache_endpoint_queue_completionHandler attributeCacheContainer endpoint queue completionHandler =
  do
    cls' <- getRequiredClass "MTRBaseClusterMediaInput"
    sendClassMessage cls' readAttributeAcceptedCommandListWithAttributeCache_endpoint_queue_completionHandlerSelector (toMTRAttributeCacheContainer attributeCacheContainer) (toNSNumber endpoint) (toNSObject queue) completionHandler

-- | @- readAttributeAttributeListWithCompletionHandler:@
readAttributeAttributeListWithCompletionHandler :: IsMTRBaseClusterMediaInput mtrBaseClusterMediaInput => mtrBaseClusterMediaInput -> Ptr () -> IO ()
readAttributeAttributeListWithCompletionHandler mtrBaseClusterMediaInput completionHandler =
  sendMessage mtrBaseClusterMediaInput readAttributeAttributeListWithCompletionHandlerSelector completionHandler

-- | @- subscribeAttributeAttributeListWithMinInterval:maxInterval:params:subscriptionEstablished:reportHandler:@
subscribeAttributeAttributeListWithMinInterval_maxInterval_params_subscriptionEstablished_reportHandler :: (IsMTRBaseClusterMediaInput mtrBaseClusterMediaInput, IsNSNumber minInterval, IsNSNumber maxInterval, IsMTRSubscribeParams params) => mtrBaseClusterMediaInput -> minInterval -> maxInterval -> params -> Ptr () -> Ptr () -> IO ()
subscribeAttributeAttributeListWithMinInterval_maxInterval_params_subscriptionEstablished_reportHandler mtrBaseClusterMediaInput minInterval maxInterval params subscriptionEstablishedHandler reportHandler =
  sendMessage mtrBaseClusterMediaInput subscribeAttributeAttributeListWithMinInterval_maxInterval_params_subscriptionEstablished_reportHandlerSelector (toNSNumber minInterval) (toNSNumber maxInterval) (toMTRSubscribeParams params) subscriptionEstablishedHandler reportHandler

-- | @+ readAttributeAttributeListWithAttributeCache:endpoint:queue:completionHandler:@
readAttributeAttributeListWithAttributeCache_endpoint_queue_completionHandler :: (IsMTRAttributeCacheContainer attributeCacheContainer, IsNSNumber endpoint, IsNSObject queue) => attributeCacheContainer -> endpoint -> queue -> Ptr () -> IO ()
readAttributeAttributeListWithAttributeCache_endpoint_queue_completionHandler attributeCacheContainer endpoint queue completionHandler =
  do
    cls' <- getRequiredClass "MTRBaseClusterMediaInput"
    sendClassMessage cls' readAttributeAttributeListWithAttributeCache_endpoint_queue_completionHandlerSelector (toMTRAttributeCacheContainer attributeCacheContainer) (toNSNumber endpoint) (toNSObject queue) completionHandler

-- | @- readAttributeFeatureMapWithCompletionHandler:@
readAttributeFeatureMapWithCompletionHandler :: IsMTRBaseClusterMediaInput mtrBaseClusterMediaInput => mtrBaseClusterMediaInput -> Ptr () -> IO ()
readAttributeFeatureMapWithCompletionHandler mtrBaseClusterMediaInput completionHandler =
  sendMessage mtrBaseClusterMediaInput readAttributeFeatureMapWithCompletionHandlerSelector completionHandler

-- | @- subscribeAttributeFeatureMapWithMinInterval:maxInterval:params:subscriptionEstablished:reportHandler:@
subscribeAttributeFeatureMapWithMinInterval_maxInterval_params_subscriptionEstablished_reportHandler :: (IsMTRBaseClusterMediaInput mtrBaseClusterMediaInput, IsNSNumber minInterval, IsNSNumber maxInterval, IsMTRSubscribeParams params) => mtrBaseClusterMediaInput -> minInterval -> maxInterval -> params -> Ptr () -> Ptr () -> IO ()
subscribeAttributeFeatureMapWithMinInterval_maxInterval_params_subscriptionEstablished_reportHandler mtrBaseClusterMediaInput minInterval maxInterval params subscriptionEstablishedHandler reportHandler =
  sendMessage mtrBaseClusterMediaInput subscribeAttributeFeatureMapWithMinInterval_maxInterval_params_subscriptionEstablished_reportHandlerSelector (toNSNumber minInterval) (toNSNumber maxInterval) (toMTRSubscribeParams params) subscriptionEstablishedHandler reportHandler

-- | @+ readAttributeFeatureMapWithAttributeCache:endpoint:queue:completionHandler:@
readAttributeFeatureMapWithAttributeCache_endpoint_queue_completionHandler :: (IsMTRAttributeCacheContainer attributeCacheContainer, IsNSNumber endpoint, IsNSObject queue) => attributeCacheContainer -> endpoint -> queue -> Ptr () -> IO ()
readAttributeFeatureMapWithAttributeCache_endpoint_queue_completionHandler attributeCacheContainer endpoint queue completionHandler =
  do
    cls' <- getRequiredClass "MTRBaseClusterMediaInput"
    sendClassMessage cls' readAttributeFeatureMapWithAttributeCache_endpoint_queue_completionHandlerSelector (toMTRAttributeCacheContainer attributeCacheContainer) (toNSNumber endpoint) (toNSObject queue) completionHandler

-- | @- readAttributeClusterRevisionWithCompletionHandler:@
readAttributeClusterRevisionWithCompletionHandler :: IsMTRBaseClusterMediaInput mtrBaseClusterMediaInput => mtrBaseClusterMediaInput -> Ptr () -> IO ()
readAttributeClusterRevisionWithCompletionHandler mtrBaseClusterMediaInput completionHandler =
  sendMessage mtrBaseClusterMediaInput readAttributeClusterRevisionWithCompletionHandlerSelector completionHandler

-- | @- subscribeAttributeClusterRevisionWithMinInterval:maxInterval:params:subscriptionEstablished:reportHandler:@
subscribeAttributeClusterRevisionWithMinInterval_maxInterval_params_subscriptionEstablished_reportHandler :: (IsMTRBaseClusterMediaInput mtrBaseClusterMediaInput, IsNSNumber minInterval, IsNSNumber maxInterval, IsMTRSubscribeParams params) => mtrBaseClusterMediaInput -> minInterval -> maxInterval -> params -> Ptr () -> Ptr () -> IO ()
subscribeAttributeClusterRevisionWithMinInterval_maxInterval_params_subscriptionEstablished_reportHandler mtrBaseClusterMediaInput minInterval maxInterval params subscriptionEstablishedHandler reportHandler =
  sendMessage mtrBaseClusterMediaInput subscribeAttributeClusterRevisionWithMinInterval_maxInterval_params_subscriptionEstablished_reportHandlerSelector (toNSNumber minInterval) (toNSNumber maxInterval) (toMTRSubscribeParams params) subscriptionEstablishedHandler reportHandler

-- | @+ readAttributeClusterRevisionWithAttributeCache:endpoint:queue:completionHandler:@
readAttributeClusterRevisionWithAttributeCache_endpoint_queue_completionHandler :: (IsMTRAttributeCacheContainer attributeCacheContainer, IsNSNumber endpoint, IsNSObject queue) => attributeCacheContainer -> endpoint -> queue -> Ptr () -> IO ()
readAttributeClusterRevisionWithAttributeCache_endpoint_queue_completionHandler attributeCacheContainer endpoint queue completionHandler =
  do
    cls' <- getRequiredClass "MTRBaseClusterMediaInput"
    sendClassMessage cls' readAttributeClusterRevisionWithAttributeCache_endpoint_queue_completionHandlerSelector (toMTRAttributeCacheContainer attributeCacheContainer) (toNSNumber endpoint) (toNSObject queue) completionHandler

-- | For all instance methods (reads, writes, commands) that take a completion, the completion will be called on the provided queue.
--
-- ObjC selector: @- initWithDevice:endpointID:queue:@
initWithDevice_endpointID_queue :: (IsMTRBaseClusterMediaInput mtrBaseClusterMediaInput, IsMTRBaseDevice device, IsNSNumber endpointID, IsNSObject queue) => mtrBaseClusterMediaInput -> device -> endpointID -> queue -> IO (Id MTRBaseClusterMediaInput)
initWithDevice_endpointID_queue mtrBaseClusterMediaInput device endpointID queue =
  sendOwnedMessage mtrBaseClusterMediaInput initWithDevice_endpointID_queueSelector (toMTRBaseDevice device) (toNSNumber endpointID) (toNSObject queue)

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @selectInputWithParams:completion:@
selectInputWithParams_completionSelector :: Selector '[Id MTRMediaInputClusterSelectInputParams, Ptr ()] ()
selectInputWithParams_completionSelector = mkSelector "selectInputWithParams:completion:"

-- | @Selector@ for @showInputStatusWithParams:completion:@
showInputStatusWithParams_completionSelector :: Selector '[Id MTRMediaInputClusterShowInputStatusParams, Ptr ()] ()
showInputStatusWithParams_completionSelector = mkSelector "showInputStatusWithParams:completion:"

-- | @Selector@ for @showInputStatusWithCompletion:@
showInputStatusWithCompletionSelector :: Selector '[Ptr ()] ()
showInputStatusWithCompletionSelector = mkSelector "showInputStatusWithCompletion:"

-- | @Selector@ for @hideInputStatusWithParams:completion:@
hideInputStatusWithParams_completionSelector :: Selector '[Id MTRMediaInputClusterHideInputStatusParams, Ptr ()] ()
hideInputStatusWithParams_completionSelector = mkSelector "hideInputStatusWithParams:completion:"

-- | @Selector@ for @hideInputStatusWithCompletion:@
hideInputStatusWithCompletionSelector :: Selector '[Ptr ()] ()
hideInputStatusWithCompletionSelector = mkSelector "hideInputStatusWithCompletion:"

-- | @Selector@ for @renameInputWithParams:completion:@
renameInputWithParams_completionSelector :: Selector '[Id MTRMediaInputClusterRenameInputParams, Ptr ()] ()
renameInputWithParams_completionSelector = mkSelector "renameInputWithParams:completion:"

-- | @Selector@ for @readAttributeInputListWithCompletion:@
readAttributeInputListWithCompletionSelector :: Selector '[Ptr ()] ()
readAttributeInputListWithCompletionSelector = mkSelector "readAttributeInputListWithCompletion:"

-- | @Selector@ for @subscribeAttributeInputListWithParams:subscriptionEstablished:reportHandler:@
subscribeAttributeInputListWithParams_subscriptionEstablished_reportHandlerSelector :: Selector '[Id MTRSubscribeParams, Ptr (), Ptr ()] ()
subscribeAttributeInputListWithParams_subscriptionEstablished_reportHandlerSelector = mkSelector "subscribeAttributeInputListWithParams:subscriptionEstablished:reportHandler:"

-- | @Selector@ for @readAttributeInputListWithClusterStateCache:endpoint:queue:completion:@
readAttributeInputListWithClusterStateCache_endpoint_queue_completionSelector :: Selector '[Id MTRClusterStateCacheContainer, Id NSNumber, Id NSObject, Ptr ()] ()
readAttributeInputListWithClusterStateCache_endpoint_queue_completionSelector = mkSelector "readAttributeInputListWithClusterStateCache:endpoint:queue:completion:"

-- | @Selector@ for @readAttributeCurrentInputWithCompletion:@
readAttributeCurrentInputWithCompletionSelector :: Selector '[Ptr ()] ()
readAttributeCurrentInputWithCompletionSelector = mkSelector "readAttributeCurrentInputWithCompletion:"

-- | @Selector@ for @subscribeAttributeCurrentInputWithParams:subscriptionEstablished:reportHandler:@
subscribeAttributeCurrentInputWithParams_subscriptionEstablished_reportHandlerSelector :: Selector '[Id MTRSubscribeParams, Ptr (), Ptr ()] ()
subscribeAttributeCurrentInputWithParams_subscriptionEstablished_reportHandlerSelector = mkSelector "subscribeAttributeCurrentInputWithParams:subscriptionEstablished:reportHandler:"

-- | @Selector@ for @readAttributeCurrentInputWithClusterStateCache:endpoint:queue:completion:@
readAttributeCurrentInputWithClusterStateCache_endpoint_queue_completionSelector :: Selector '[Id MTRClusterStateCacheContainer, Id NSNumber, Id NSObject, Ptr ()] ()
readAttributeCurrentInputWithClusterStateCache_endpoint_queue_completionSelector = mkSelector "readAttributeCurrentInputWithClusterStateCache:endpoint:queue:completion:"

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
initSelector :: Selector '[] (Id MTRBaseClusterMediaInput)
initSelector = mkSelector "init"

-- | @Selector@ for @new@
newSelector :: Selector '[] (Id MTRBaseClusterMediaInput)
newSelector = mkSelector "new"

-- | @Selector@ for @initWithDevice:endpoint:queue:@
initWithDevice_endpoint_queueSelector :: Selector '[Id MTRBaseDevice, CUShort, Id NSObject] (Id MTRBaseClusterMediaInput)
initWithDevice_endpoint_queueSelector = mkSelector "initWithDevice:endpoint:queue:"

-- | @Selector@ for @selectInputWithParams:completionHandler:@
selectInputWithParams_completionHandlerSelector :: Selector '[Id MTRMediaInputClusterSelectInputParams, Ptr ()] ()
selectInputWithParams_completionHandlerSelector = mkSelector "selectInputWithParams:completionHandler:"

-- | @Selector@ for @showInputStatusWithParams:completionHandler:@
showInputStatusWithParams_completionHandlerSelector :: Selector '[Id MTRMediaInputClusterShowInputStatusParams, Ptr ()] ()
showInputStatusWithParams_completionHandlerSelector = mkSelector "showInputStatusWithParams:completionHandler:"

-- | @Selector@ for @showInputStatusWithCompletionHandler:@
showInputStatusWithCompletionHandlerSelector :: Selector '[Ptr ()] ()
showInputStatusWithCompletionHandlerSelector = mkSelector "showInputStatusWithCompletionHandler:"

-- | @Selector@ for @hideInputStatusWithParams:completionHandler:@
hideInputStatusWithParams_completionHandlerSelector :: Selector '[Id MTRMediaInputClusterHideInputStatusParams, Ptr ()] ()
hideInputStatusWithParams_completionHandlerSelector = mkSelector "hideInputStatusWithParams:completionHandler:"

-- | @Selector@ for @hideInputStatusWithCompletionHandler:@
hideInputStatusWithCompletionHandlerSelector :: Selector '[Ptr ()] ()
hideInputStatusWithCompletionHandlerSelector = mkSelector "hideInputStatusWithCompletionHandler:"

-- | @Selector@ for @renameInputWithParams:completionHandler:@
renameInputWithParams_completionHandlerSelector :: Selector '[Id MTRMediaInputClusterRenameInputParams, Ptr ()] ()
renameInputWithParams_completionHandlerSelector = mkSelector "renameInputWithParams:completionHandler:"

-- | @Selector@ for @readAttributeInputListWithCompletionHandler:@
readAttributeInputListWithCompletionHandlerSelector :: Selector '[Ptr ()] ()
readAttributeInputListWithCompletionHandlerSelector = mkSelector "readAttributeInputListWithCompletionHandler:"

-- | @Selector@ for @subscribeAttributeInputListWithMinInterval:maxInterval:params:subscriptionEstablished:reportHandler:@
subscribeAttributeInputListWithMinInterval_maxInterval_params_subscriptionEstablished_reportHandlerSelector :: Selector '[Id NSNumber, Id NSNumber, Id MTRSubscribeParams, Ptr (), Ptr ()] ()
subscribeAttributeInputListWithMinInterval_maxInterval_params_subscriptionEstablished_reportHandlerSelector = mkSelector "subscribeAttributeInputListWithMinInterval:maxInterval:params:subscriptionEstablished:reportHandler:"

-- | @Selector@ for @readAttributeInputListWithAttributeCache:endpoint:queue:completionHandler:@
readAttributeInputListWithAttributeCache_endpoint_queue_completionHandlerSelector :: Selector '[Id MTRAttributeCacheContainer, Id NSNumber, Id NSObject, Ptr ()] ()
readAttributeInputListWithAttributeCache_endpoint_queue_completionHandlerSelector = mkSelector "readAttributeInputListWithAttributeCache:endpoint:queue:completionHandler:"

-- | @Selector@ for @readAttributeCurrentInputWithCompletionHandler:@
readAttributeCurrentInputWithCompletionHandlerSelector :: Selector '[Ptr ()] ()
readAttributeCurrentInputWithCompletionHandlerSelector = mkSelector "readAttributeCurrentInputWithCompletionHandler:"

-- | @Selector@ for @subscribeAttributeCurrentInputWithMinInterval:maxInterval:params:subscriptionEstablished:reportHandler:@
subscribeAttributeCurrentInputWithMinInterval_maxInterval_params_subscriptionEstablished_reportHandlerSelector :: Selector '[Id NSNumber, Id NSNumber, Id MTRSubscribeParams, Ptr (), Ptr ()] ()
subscribeAttributeCurrentInputWithMinInterval_maxInterval_params_subscriptionEstablished_reportHandlerSelector = mkSelector "subscribeAttributeCurrentInputWithMinInterval:maxInterval:params:subscriptionEstablished:reportHandler:"

-- | @Selector@ for @readAttributeCurrentInputWithAttributeCache:endpoint:queue:completionHandler:@
readAttributeCurrentInputWithAttributeCache_endpoint_queue_completionHandlerSelector :: Selector '[Id MTRAttributeCacheContainer, Id NSNumber, Id NSObject, Ptr ()] ()
readAttributeCurrentInputWithAttributeCache_endpoint_queue_completionHandlerSelector = mkSelector "readAttributeCurrentInputWithAttributeCache:endpoint:queue:completionHandler:"

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
initWithDevice_endpointID_queueSelector :: Selector '[Id MTRBaseDevice, Id NSNumber, Id NSObject] (Id MTRBaseClusterMediaInput)
initWithDevice_endpointID_queueSelector = mkSelector "initWithDevice:endpointID:queue:"

