{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Cluster Refrigerator And Temperature Controlled Cabinet Mode
--
-- Attributes and commands for selecting a mode from a list of supported options.
--
-- Generated bindings for @MTRBaseClusterRefrigeratorAndTemperatureControlledCabinetMode@.
module ObjC.Matter.MTRBaseClusterRefrigeratorAndTemperatureControlledCabinetMode
  ( MTRBaseClusterRefrigeratorAndTemperatureControlledCabinetMode
  , IsMTRBaseClusterRefrigeratorAndTemperatureControlledCabinetMode(..)
  , changeToModeWithParams_completion
  , readAttributeSupportedModesWithCompletion
  , subscribeAttributeSupportedModesWithParams_subscriptionEstablished_reportHandler
  , readAttributeSupportedModesWithClusterStateCache_endpoint_queue_completion
  , readAttributeCurrentModeWithCompletion
  , subscribeAttributeCurrentModeWithParams_subscriptionEstablished_reportHandler
  , readAttributeCurrentModeWithClusterStateCache_endpoint_queue_completion
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
  , changeToModeWithParams_completionSelector
  , initSelector
  , initWithDevice_endpointID_queueSelector
  , newSelector
  , readAttributeAcceptedCommandListWithClusterStateCache_endpoint_queue_completionSelector
  , readAttributeAcceptedCommandListWithCompletionSelector
  , readAttributeAttributeListWithClusterStateCache_endpoint_queue_completionSelector
  , readAttributeAttributeListWithCompletionSelector
  , readAttributeClusterRevisionWithClusterStateCache_endpoint_queue_completionSelector
  , readAttributeClusterRevisionWithCompletionSelector
  , readAttributeCurrentModeWithClusterStateCache_endpoint_queue_completionSelector
  , readAttributeCurrentModeWithCompletionSelector
  , readAttributeFeatureMapWithClusterStateCache_endpoint_queue_completionSelector
  , readAttributeFeatureMapWithCompletionSelector
  , readAttributeGeneratedCommandListWithClusterStateCache_endpoint_queue_completionSelector
  , readAttributeGeneratedCommandListWithCompletionSelector
  , readAttributeSupportedModesWithClusterStateCache_endpoint_queue_completionSelector
  , readAttributeSupportedModesWithCompletionSelector
  , subscribeAttributeAcceptedCommandListWithParams_subscriptionEstablished_reportHandlerSelector
  , subscribeAttributeAttributeListWithParams_subscriptionEstablished_reportHandlerSelector
  , subscribeAttributeClusterRevisionWithParams_subscriptionEstablished_reportHandlerSelector
  , subscribeAttributeCurrentModeWithParams_subscriptionEstablished_reportHandlerSelector
  , subscribeAttributeFeatureMapWithParams_subscriptionEstablished_reportHandlerSelector
  , subscribeAttributeGeneratedCommandListWithParams_subscriptionEstablished_reportHandlerSelector
  , subscribeAttributeSupportedModesWithParams_subscriptionEstablished_reportHandlerSelector


  ) where

import Foreign.Ptr (Ptr, FunPtr)
import Foreign.C.Types

import ObjC.Runtime.Types
import ObjC.Runtime.Message (sendMessage, sendOwnedMessage, sendClassMessage, sendOwnedClassMessage)
import ObjC.Runtime.Selector (mkSelector)
import ObjC.Runtime.Class (getRequiredClass)

import ObjC.Matter.Internal.Classes
import ObjC.Foundation.Internal.Classes

-- | Command ChangeToMode
--
-- This command is used to change device modes.        On receipt of this command the device SHALL respond with a ChangeToModeResponse command.
--
-- ObjC selector: @- changeToModeWithParams:completion:@
changeToModeWithParams_completion :: (IsMTRBaseClusterRefrigeratorAndTemperatureControlledCabinetMode mtrBaseClusterRefrigeratorAndTemperatureControlledCabinetMode, IsMTRRefrigeratorAndTemperatureControlledCabinetModeClusterChangeToModeParams params) => mtrBaseClusterRefrigeratorAndTemperatureControlledCabinetMode -> params -> Ptr () -> IO ()
changeToModeWithParams_completion mtrBaseClusterRefrigeratorAndTemperatureControlledCabinetMode params completion =
  sendMessage mtrBaseClusterRefrigeratorAndTemperatureControlledCabinetMode changeToModeWithParams_completionSelector (toMTRRefrigeratorAndTemperatureControlledCabinetModeClusterChangeToModeParams params) completion

-- | @- readAttributeSupportedModesWithCompletion:@
readAttributeSupportedModesWithCompletion :: IsMTRBaseClusterRefrigeratorAndTemperatureControlledCabinetMode mtrBaseClusterRefrigeratorAndTemperatureControlledCabinetMode => mtrBaseClusterRefrigeratorAndTemperatureControlledCabinetMode -> Ptr () -> IO ()
readAttributeSupportedModesWithCompletion mtrBaseClusterRefrigeratorAndTemperatureControlledCabinetMode completion =
  sendMessage mtrBaseClusterRefrigeratorAndTemperatureControlledCabinetMode readAttributeSupportedModesWithCompletionSelector completion

-- | @- subscribeAttributeSupportedModesWithParams:subscriptionEstablished:reportHandler:@
subscribeAttributeSupportedModesWithParams_subscriptionEstablished_reportHandler :: (IsMTRBaseClusterRefrigeratorAndTemperatureControlledCabinetMode mtrBaseClusterRefrigeratorAndTemperatureControlledCabinetMode, IsMTRSubscribeParams params) => mtrBaseClusterRefrigeratorAndTemperatureControlledCabinetMode -> params -> Ptr () -> Ptr () -> IO ()
subscribeAttributeSupportedModesWithParams_subscriptionEstablished_reportHandler mtrBaseClusterRefrigeratorAndTemperatureControlledCabinetMode params subscriptionEstablished reportHandler =
  sendMessage mtrBaseClusterRefrigeratorAndTemperatureControlledCabinetMode subscribeAttributeSupportedModesWithParams_subscriptionEstablished_reportHandlerSelector (toMTRSubscribeParams params) subscriptionEstablished reportHandler

-- | @+ readAttributeSupportedModesWithClusterStateCache:endpoint:queue:completion:@
readAttributeSupportedModesWithClusterStateCache_endpoint_queue_completion :: (IsMTRClusterStateCacheContainer clusterStateCacheContainer, IsNSNumber endpoint, IsNSObject queue) => clusterStateCacheContainer -> endpoint -> queue -> Ptr () -> IO ()
readAttributeSupportedModesWithClusterStateCache_endpoint_queue_completion clusterStateCacheContainer endpoint queue completion =
  do
    cls' <- getRequiredClass "MTRBaseClusterRefrigeratorAndTemperatureControlledCabinetMode"
    sendClassMessage cls' readAttributeSupportedModesWithClusterStateCache_endpoint_queue_completionSelector (toMTRClusterStateCacheContainer clusterStateCacheContainer) (toNSNumber endpoint) (toNSObject queue) completion

-- | @- readAttributeCurrentModeWithCompletion:@
readAttributeCurrentModeWithCompletion :: IsMTRBaseClusterRefrigeratorAndTemperatureControlledCabinetMode mtrBaseClusterRefrigeratorAndTemperatureControlledCabinetMode => mtrBaseClusterRefrigeratorAndTemperatureControlledCabinetMode -> Ptr () -> IO ()
readAttributeCurrentModeWithCompletion mtrBaseClusterRefrigeratorAndTemperatureControlledCabinetMode completion =
  sendMessage mtrBaseClusterRefrigeratorAndTemperatureControlledCabinetMode readAttributeCurrentModeWithCompletionSelector completion

-- | @- subscribeAttributeCurrentModeWithParams:subscriptionEstablished:reportHandler:@
subscribeAttributeCurrentModeWithParams_subscriptionEstablished_reportHandler :: (IsMTRBaseClusterRefrigeratorAndTemperatureControlledCabinetMode mtrBaseClusterRefrigeratorAndTemperatureControlledCabinetMode, IsMTRSubscribeParams params) => mtrBaseClusterRefrigeratorAndTemperatureControlledCabinetMode -> params -> Ptr () -> Ptr () -> IO ()
subscribeAttributeCurrentModeWithParams_subscriptionEstablished_reportHandler mtrBaseClusterRefrigeratorAndTemperatureControlledCabinetMode params subscriptionEstablished reportHandler =
  sendMessage mtrBaseClusterRefrigeratorAndTemperatureControlledCabinetMode subscribeAttributeCurrentModeWithParams_subscriptionEstablished_reportHandlerSelector (toMTRSubscribeParams params) subscriptionEstablished reportHandler

-- | @+ readAttributeCurrentModeWithClusterStateCache:endpoint:queue:completion:@
readAttributeCurrentModeWithClusterStateCache_endpoint_queue_completion :: (IsMTRClusterStateCacheContainer clusterStateCacheContainer, IsNSNumber endpoint, IsNSObject queue) => clusterStateCacheContainer -> endpoint -> queue -> Ptr () -> IO ()
readAttributeCurrentModeWithClusterStateCache_endpoint_queue_completion clusterStateCacheContainer endpoint queue completion =
  do
    cls' <- getRequiredClass "MTRBaseClusterRefrigeratorAndTemperatureControlledCabinetMode"
    sendClassMessage cls' readAttributeCurrentModeWithClusterStateCache_endpoint_queue_completionSelector (toMTRClusterStateCacheContainer clusterStateCacheContainer) (toNSNumber endpoint) (toNSObject queue) completion

-- | @- readAttributeGeneratedCommandListWithCompletion:@
readAttributeGeneratedCommandListWithCompletion :: IsMTRBaseClusterRefrigeratorAndTemperatureControlledCabinetMode mtrBaseClusterRefrigeratorAndTemperatureControlledCabinetMode => mtrBaseClusterRefrigeratorAndTemperatureControlledCabinetMode -> Ptr () -> IO ()
readAttributeGeneratedCommandListWithCompletion mtrBaseClusterRefrigeratorAndTemperatureControlledCabinetMode completion =
  sendMessage mtrBaseClusterRefrigeratorAndTemperatureControlledCabinetMode readAttributeGeneratedCommandListWithCompletionSelector completion

-- | @- subscribeAttributeGeneratedCommandListWithParams:subscriptionEstablished:reportHandler:@
subscribeAttributeGeneratedCommandListWithParams_subscriptionEstablished_reportHandler :: (IsMTRBaseClusterRefrigeratorAndTemperatureControlledCabinetMode mtrBaseClusterRefrigeratorAndTemperatureControlledCabinetMode, IsMTRSubscribeParams params) => mtrBaseClusterRefrigeratorAndTemperatureControlledCabinetMode -> params -> Ptr () -> Ptr () -> IO ()
subscribeAttributeGeneratedCommandListWithParams_subscriptionEstablished_reportHandler mtrBaseClusterRefrigeratorAndTemperatureControlledCabinetMode params subscriptionEstablished reportHandler =
  sendMessage mtrBaseClusterRefrigeratorAndTemperatureControlledCabinetMode subscribeAttributeGeneratedCommandListWithParams_subscriptionEstablished_reportHandlerSelector (toMTRSubscribeParams params) subscriptionEstablished reportHandler

-- | @+ readAttributeGeneratedCommandListWithClusterStateCache:endpoint:queue:completion:@
readAttributeGeneratedCommandListWithClusterStateCache_endpoint_queue_completion :: (IsMTRClusterStateCacheContainer clusterStateCacheContainer, IsNSNumber endpoint, IsNSObject queue) => clusterStateCacheContainer -> endpoint -> queue -> Ptr () -> IO ()
readAttributeGeneratedCommandListWithClusterStateCache_endpoint_queue_completion clusterStateCacheContainer endpoint queue completion =
  do
    cls' <- getRequiredClass "MTRBaseClusterRefrigeratorAndTemperatureControlledCabinetMode"
    sendClassMessage cls' readAttributeGeneratedCommandListWithClusterStateCache_endpoint_queue_completionSelector (toMTRClusterStateCacheContainer clusterStateCacheContainer) (toNSNumber endpoint) (toNSObject queue) completion

-- | @- readAttributeAcceptedCommandListWithCompletion:@
readAttributeAcceptedCommandListWithCompletion :: IsMTRBaseClusterRefrigeratorAndTemperatureControlledCabinetMode mtrBaseClusterRefrigeratorAndTemperatureControlledCabinetMode => mtrBaseClusterRefrigeratorAndTemperatureControlledCabinetMode -> Ptr () -> IO ()
readAttributeAcceptedCommandListWithCompletion mtrBaseClusterRefrigeratorAndTemperatureControlledCabinetMode completion =
  sendMessage mtrBaseClusterRefrigeratorAndTemperatureControlledCabinetMode readAttributeAcceptedCommandListWithCompletionSelector completion

-- | @- subscribeAttributeAcceptedCommandListWithParams:subscriptionEstablished:reportHandler:@
subscribeAttributeAcceptedCommandListWithParams_subscriptionEstablished_reportHandler :: (IsMTRBaseClusterRefrigeratorAndTemperatureControlledCabinetMode mtrBaseClusterRefrigeratorAndTemperatureControlledCabinetMode, IsMTRSubscribeParams params) => mtrBaseClusterRefrigeratorAndTemperatureControlledCabinetMode -> params -> Ptr () -> Ptr () -> IO ()
subscribeAttributeAcceptedCommandListWithParams_subscriptionEstablished_reportHandler mtrBaseClusterRefrigeratorAndTemperatureControlledCabinetMode params subscriptionEstablished reportHandler =
  sendMessage mtrBaseClusterRefrigeratorAndTemperatureControlledCabinetMode subscribeAttributeAcceptedCommandListWithParams_subscriptionEstablished_reportHandlerSelector (toMTRSubscribeParams params) subscriptionEstablished reportHandler

-- | @+ readAttributeAcceptedCommandListWithClusterStateCache:endpoint:queue:completion:@
readAttributeAcceptedCommandListWithClusterStateCache_endpoint_queue_completion :: (IsMTRClusterStateCacheContainer clusterStateCacheContainer, IsNSNumber endpoint, IsNSObject queue) => clusterStateCacheContainer -> endpoint -> queue -> Ptr () -> IO ()
readAttributeAcceptedCommandListWithClusterStateCache_endpoint_queue_completion clusterStateCacheContainer endpoint queue completion =
  do
    cls' <- getRequiredClass "MTRBaseClusterRefrigeratorAndTemperatureControlledCabinetMode"
    sendClassMessage cls' readAttributeAcceptedCommandListWithClusterStateCache_endpoint_queue_completionSelector (toMTRClusterStateCacheContainer clusterStateCacheContainer) (toNSNumber endpoint) (toNSObject queue) completion

-- | @- readAttributeAttributeListWithCompletion:@
readAttributeAttributeListWithCompletion :: IsMTRBaseClusterRefrigeratorAndTemperatureControlledCabinetMode mtrBaseClusterRefrigeratorAndTemperatureControlledCabinetMode => mtrBaseClusterRefrigeratorAndTemperatureControlledCabinetMode -> Ptr () -> IO ()
readAttributeAttributeListWithCompletion mtrBaseClusterRefrigeratorAndTemperatureControlledCabinetMode completion =
  sendMessage mtrBaseClusterRefrigeratorAndTemperatureControlledCabinetMode readAttributeAttributeListWithCompletionSelector completion

-- | @- subscribeAttributeAttributeListWithParams:subscriptionEstablished:reportHandler:@
subscribeAttributeAttributeListWithParams_subscriptionEstablished_reportHandler :: (IsMTRBaseClusterRefrigeratorAndTemperatureControlledCabinetMode mtrBaseClusterRefrigeratorAndTemperatureControlledCabinetMode, IsMTRSubscribeParams params) => mtrBaseClusterRefrigeratorAndTemperatureControlledCabinetMode -> params -> Ptr () -> Ptr () -> IO ()
subscribeAttributeAttributeListWithParams_subscriptionEstablished_reportHandler mtrBaseClusterRefrigeratorAndTemperatureControlledCabinetMode params subscriptionEstablished reportHandler =
  sendMessage mtrBaseClusterRefrigeratorAndTemperatureControlledCabinetMode subscribeAttributeAttributeListWithParams_subscriptionEstablished_reportHandlerSelector (toMTRSubscribeParams params) subscriptionEstablished reportHandler

-- | @+ readAttributeAttributeListWithClusterStateCache:endpoint:queue:completion:@
readAttributeAttributeListWithClusterStateCache_endpoint_queue_completion :: (IsMTRClusterStateCacheContainer clusterStateCacheContainer, IsNSNumber endpoint, IsNSObject queue) => clusterStateCacheContainer -> endpoint -> queue -> Ptr () -> IO ()
readAttributeAttributeListWithClusterStateCache_endpoint_queue_completion clusterStateCacheContainer endpoint queue completion =
  do
    cls' <- getRequiredClass "MTRBaseClusterRefrigeratorAndTemperatureControlledCabinetMode"
    sendClassMessage cls' readAttributeAttributeListWithClusterStateCache_endpoint_queue_completionSelector (toMTRClusterStateCacheContainer clusterStateCacheContainer) (toNSNumber endpoint) (toNSObject queue) completion

-- | @- readAttributeFeatureMapWithCompletion:@
readAttributeFeatureMapWithCompletion :: IsMTRBaseClusterRefrigeratorAndTemperatureControlledCabinetMode mtrBaseClusterRefrigeratorAndTemperatureControlledCabinetMode => mtrBaseClusterRefrigeratorAndTemperatureControlledCabinetMode -> Ptr () -> IO ()
readAttributeFeatureMapWithCompletion mtrBaseClusterRefrigeratorAndTemperatureControlledCabinetMode completion =
  sendMessage mtrBaseClusterRefrigeratorAndTemperatureControlledCabinetMode readAttributeFeatureMapWithCompletionSelector completion

-- | @- subscribeAttributeFeatureMapWithParams:subscriptionEstablished:reportHandler:@
subscribeAttributeFeatureMapWithParams_subscriptionEstablished_reportHandler :: (IsMTRBaseClusterRefrigeratorAndTemperatureControlledCabinetMode mtrBaseClusterRefrigeratorAndTemperatureControlledCabinetMode, IsMTRSubscribeParams params) => mtrBaseClusterRefrigeratorAndTemperatureControlledCabinetMode -> params -> Ptr () -> Ptr () -> IO ()
subscribeAttributeFeatureMapWithParams_subscriptionEstablished_reportHandler mtrBaseClusterRefrigeratorAndTemperatureControlledCabinetMode params subscriptionEstablished reportHandler =
  sendMessage mtrBaseClusterRefrigeratorAndTemperatureControlledCabinetMode subscribeAttributeFeatureMapWithParams_subscriptionEstablished_reportHandlerSelector (toMTRSubscribeParams params) subscriptionEstablished reportHandler

-- | @+ readAttributeFeatureMapWithClusterStateCache:endpoint:queue:completion:@
readAttributeFeatureMapWithClusterStateCache_endpoint_queue_completion :: (IsMTRClusterStateCacheContainer clusterStateCacheContainer, IsNSNumber endpoint, IsNSObject queue) => clusterStateCacheContainer -> endpoint -> queue -> Ptr () -> IO ()
readAttributeFeatureMapWithClusterStateCache_endpoint_queue_completion clusterStateCacheContainer endpoint queue completion =
  do
    cls' <- getRequiredClass "MTRBaseClusterRefrigeratorAndTemperatureControlledCabinetMode"
    sendClassMessage cls' readAttributeFeatureMapWithClusterStateCache_endpoint_queue_completionSelector (toMTRClusterStateCacheContainer clusterStateCacheContainer) (toNSNumber endpoint) (toNSObject queue) completion

-- | @- readAttributeClusterRevisionWithCompletion:@
readAttributeClusterRevisionWithCompletion :: IsMTRBaseClusterRefrigeratorAndTemperatureControlledCabinetMode mtrBaseClusterRefrigeratorAndTemperatureControlledCabinetMode => mtrBaseClusterRefrigeratorAndTemperatureControlledCabinetMode -> Ptr () -> IO ()
readAttributeClusterRevisionWithCompletion mtrBaseClusterRefrigeratorAndTemperatureControlledCabinetMode completion =
  sendMessage mtrBaseClusterRefrigeratorAndTemperatureControlledCabinetMode readAttributeClusterRevisionWithCompletionSelector completion

-- | @- subscribeAttributeClusterRevisionWithParams:subscriptionEstablished:reportHandler:@
subscribeAttributeClusterRevisionWithParams_subscriptionEstablished_reportHandler :: (IsMTRBaseClusterRefrigeratorAndTemperatureControlledCabinetMode mtrBaseClusterRefrigeratorAndTemperatureControlledCabinetMode, IsMTRSubscribeParams params) => mtrBaseClusterRefrigeratorAndTemperatureControlledCabinetMode -> params -> Ptr () -> Ptr () -> IO ()
subscribeAttributeClusterRevisionWithParams_subscriptionEstablished_reportHandler mtrBaseClusterRefrigeratorAndTemperatureControlledCabinetMode params subscriptionEstablished reportHandler =
  sendMessage mtrBaseClusterRefrigeratorAndTemperatureControlledCabinetMode subscribeAttributeClusterRevisionWithParams_subscriptionEstablished_reportHandlerSelector (toMTRSubscribeParams params) subscriptionEstablished reportHandler

-- | @+ readAttributeClusterRevisionWithClusterStateCache:endpoint:queue:completion:@
readAttributeClusterRevisionWithClusterStateCache_endpoint_queue_completion :: (IsMTRClusterStateCacheContainer clusterStateCacheContainer, IsNSNumber endpoint, IsNSObject queue) => clusterStateCacheContainer -> endpoint -> queue -> Ptr () -> IO ()
readAttributeClusterRevisionWithClusterStateCache_endpoint_queue_completion clusterStateCacheContainer endpoint queue completion =
  do
    cls' <- getRequiredClass "MTRBaseClusterRefrigeratorAndTemperatureControlledCabinetMode"
    sendClassMessage cls' readAttributeClusterRevisionWithClusterStateCache_endpoint_queue_completionSelector (toMTRClusterStateCacheContainer clusterStateCacheContainer) (toNSNumber endpoint) (toNSObject queue) completion

-- | @- init@
init_ :: IsMTRBaseClusterRefrigeratorAndTemperatureControlledCabinetMode mtrBaseClusterRefrigeratorAndTemperatureControlledCabinetMode => mtrBaseClusterRefrigeratorAndTemperatureControlledCabinetMode -> IO (Id MTRBaseClusterRefrigeratorAndTemperatureControlledCabinetMode)
init_ mtrBaseClusterRefrigeratorAndTemperatureControlledCabinetMode =
  sendOwnedMessage mtrBaseClusterRefrigeratorAndTemperatureControlledCabinetMode initSelector

-- | @+ new@
new :: IO (Id MTRBaseClusterRefrigeratorAndTemperatureControlledCabinetMode)
new  =
  do
    cls' <- getRequiredClass "MTRBaseClusterRefrigeratorAndTemperatureControlledCabinetMode"
    sendOwnedClassMessage cls' newSelector

-- | For all instance methods (reads, writes, commands) that take a completion, the completion will be called on the provided queue.
--
-- ObjC selector: @- initWithDevice:endpointID:queue:@
initWithDevice_endpointID_queue :: (IsMTRBaseClusterRefrigeratorAndTemperatureControlledCabinetMode mtrBaseClusterRefrigeratorAndTemperatureControlledCabinetMode, IsMTRBaseDevice device, IsNSNumber endpointID, IsNSObject queue) => mtrBaseClusterRefrigeratorAndTemperatureControlledCabinetMode -> device -> endpointID -> queue -> IO (Id MTRBaseClusterRefrigeratorAndTemperatureControlledCabinetMode)
initWithDevice_endpointID_queue mtrBaseClusterRefrigeratorAndTemperatureControlledCabinetMode device endpointID queue =
  sendOwnedMessage mtrBaseClusterRefrigeratorAndTemperatureControlledCabinetMode initWithDevice_endpointID_queueSelector (toMTRBaseDevice device) (toNSNumber endpointID) (toNSObject queue)

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @changeToModeWithParams:completion:@
changeToModeWithParams_completionSelector :: Selector '[Id MTRRefrigeratorAndTemperatureControlledCabinetModeClusterChangeToModeParams, Ptr ()] ()
changeToModeWithParams_completionSelector = mkSelector "changeToModeWithParams:completion:"

-- | @Selector@ for @readAttributeSupportedModesWithCompletion:@
readAttributeSupportedModesWithCompletionSelector :: Selector '[Ptr ()] ()
readAttributeSupportedModesWithCompletionSelector = mkSelector "readAttributeSupportedModesWithCompletion:"

-- | @Selector@ for @subscribeAttributeSupportedModesWithParams:subscriptionEstablished:reportHandler:@
subscribeAttributeSupportedModesWithParams_subscriptionEstablished_reportHandlerSelector :: Selector '[Id MTRSubscribeParams, Ptr (), Ptr ()] ()
subscribeAttributeSupportedModesWithParams_subscriptionEstablished_reportHandlerSelector = mkSelector "subscribeAttributeSupportedModesWithParams:subscriptionEstablished:reportHandler:"

-- | @Selector@ for @readAttributeSupportedModesWithClusterStateCache:endpoint:queue:completion:@
readAttributeSupportedModesWithClusterStateCache_endpoint_queue_completionSelector :: Selector '[Id MTRClusterStateCacheContainer, Id NSNumber, Id NSObject, Ptr ()] ()
readAttributeSupportedModesWithClusterStateCache_endpoint_queue_completionSelector = mkSelector "readAttributeSupportedModesWithClusterStateCache:endpoint:queue:completion:"

-- | @Selector@ for @readAttributeCurrentModeWithCompletion:@
readAttributeCurrentModeWithCompletionSelector :: Selector '[Ptr ()] ()
readAttributeCurrentModeWithCompletionSelector = mkSelector "readAttributeCurrentModeWithCompletion:"

-- | @Selector@ for @subscribeAttributeCurrentModeWithParams:subscriptionEstablished:reportHandler:@
subscribeAttributeCurrentModeWithParams_subscriptionEstablished_reportHandlerSelector :: Selector '[Id MTRSubscribeParams, Ptr (), Ptr ()] ()
subscribeAttributeCurrentModeWithParams_subscriptionEstablished_reportHandlerSelector = mkSelector "subscribeAttributeCurrentModeWithParams:subscriptionEstablished:reportHandler:"

-- | @Selector@ for @readAttributeCurrentModeWithClusterStateCache:endpoint:queue:completion:@
readAttributeCurrentModeWithClusterStateCache_endpoint_queue_completionSelector :: Selector '[Id MTRClusterStateCacheContainer, Id NSNumber, Id NSObject, Ptr ()] ()
readAttributeCurrentModeWithClusterStateCache_endpoint_queue_completionSelector = mkSelector "readAttributeCurrentModeWithClusterStateCache:endpoint:queue:completion:"

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
initSelector :: Selector '[] (Id MTRBaseClusterRefrigeratorAndTemperatureControlledCabinetMode)
initSelector = mkSelector "init"

-- | @Selector@ for @new@
newSelector :: Selector '[] (Id MTRBaseClusterRefrigeratorAndTemperatureControlledCabinetMode)
newSelector = mkSelector "new"

-- | @Selector@ for @initWithDevice:endpointID:queue:@
initWithDevice_endpointID_queueSelector :: Selector '[Id MTRBaseDevice, Id NSNumber, Id NSObject] (Id MTRBaseClusterRefrigeratorAndTemperatureControlledCabinetMode)
initWithDevice_endpointID_queueSelector = mkSelector "initWithDevice:endpointID:queue:"

