{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Cluster Apple Device Information
--
-- This cluster provides Apple-specific information about the device.
--
-- Generated bindings for @MTRBaseClusterAppleDeviceInformation@.
module ObjC.Matter.MTRBaseClusterAppleDeviceInformation
  ( MTRBaseClusterAppleDeviceInformation
  , IsMTRBaseClusterAppleDeviceInformation(..)
  , readAttributeSupportsTapToUnlockWithCompletion
  , subscribeAttributeSupportsTapToUnlockWithParams_subscriptionEstablished_reportHandler
  , readAttributeSupportsTapToUnlockWithClusterStateCache_endpoint_queue_completion
  , readAttributeSupportsWEDWithCompletion
  , subscribeAttributeSupportsWEDWithParams_subscriptionEstablished_reportHandler
  , readAttributeSupportsWEDWithClusterStateCache_endpoint_queue_completion
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
  , readAttributeSupportsTapToUnlockWithClusterStateCache_endpoint_queue_completionSelector
  , readAttributeSupportsTapToUnlockWithCompletionSelector
  , readAttributeSupportsWEDWithClusterStateCache_endpoint_queue_completionSelector
  , readAttributeSupportsWEDWithCompletionSelector
  , subscribeAttributeAcceptedCommandListWithParams_subscriptionEstablished_reportHandlerSelector
  , subscribeAttributeAttributeListWithParams_subscriptionEstablished_reportHandlerSelector
  , subscribeAttributeClusterRevisionWithParams_subscriptionEstablished_reportHandlerSelector
  , subscribeAttributeFeatureMapWithParams_subscriptionEstablished_reportHandlerSelector
  , subscribeAttributeGeneratedCommandListWithParams_subscriptionEstablished_reportHandlerSelector
  , subscribeAttributeSupportsTapToUnlockWithParams_subscriptionEstablished_reportHandlerSelector
  , subscribeAttributeSupportsWEDWithParams_subscriptionEstablished_reportHandlerSelector


  ) where

import Foreign.Ptr (Ptr, FunPtr)
import Foreign.C.Types

import ObjC.Runtime.Types
import ObjC.Runtime.Message (sendMessage, sendOwnedMessage, sendClassMessage, sendOwnedClassMessage)
import ObjC.Runtime.Selector (mkSelector)
import ObjC.Runtime.Class (getRequiredClass)

import ObjC.Matter.Internal.Classes
import ObjC.Foundation.Internal.Classes

-- | @- readAttributeSupportsTapToUnlockWithCompletion:@
readAttributeSupportsTapToUnlockWithCompletion :: IsMTRBaseClusterAppleDeviceInformation mtrBaseClusterAppleDeviceInformation => mtrBaseClusterAppleDeviceInformation -> Ptr () -> IO ()
readAttributeSupportsTapToUnlockWithCompletion mtrBaseClusterAppleDeviceInformation completion =
  sendMessage mtrBaseClusterAppleDeviceInformation readAttributeSupportsTapToUnlockWithCompletionSelector completion

-- | @- subscribeAttributeSupportsTapToUnlockWithParams:subscriptionEstablished:reportHandler:@
subscribeAttributeSupportsTapToUnlockWithParams_subscriptionEstablished_reportHandler :: (IsMTRBaseClusterAppleDeviceInformation mtrBaseClusterAppleDeviceInformation, IsMTRSubscribeParams params) => mtrBaseClusterAppleDeviceInformation -> params -> Ptr () -> Ptr () -> IO ()
subscribeAttributeSupportsTapToUnlockWithParams_subscriptionEstablished_reportHandler mtrBaseClusterAppleDeviceInformation params subscriptionEstablished reportHandler =
  sendMessage mtrBaseClusterAppleDeviceInformation subscribeAttributeSupportsTapToUnlockWithParams_subscriptionEstablished_reportHandlerSelector (toMTRSubscribeParams params) subscriptionEstablished reportHandler

-- | @+ readAttributeSupportsTapToUnlockWithClusterStateCache:endpoint:queue:completion:@
readAttributeSupportsTapToUnlockWithClusterStateCache_endpoint_queue_completion :: (IsMTRClusterStateCacheContainer clusterStateCacheContainer, IsNSNumber endpoint, IsNSObject queue) => clusterStateCacheContainer -> endpoint -> queue -> Ptr () -> IO ()
readAttributeSupportsTapToUnlockWithClusterStateCache_endpoint_queue_completion clusterStateCacheContainer endpoint queue completion =
  do
    cls' <- getRequiredClass "MTRBaseClusterAppleDeviceInformation"
    sendClassMessage cls' readAttributeSupportsTapToUnlockWithClusterStateCache_endpoint_queue_completionSelector (toMTRClusterStateCacheContainer clusterStateCacheContainer) (toNSNumber endpoint) (toNSObject queue) completion

-- | @- readAttributeSupportsWEDWithCompletion:@
readAttributeSupportsWEDWithCompletion :: IsMTRBaseClusterAppleDeviceInformation mtrBaseClusterAppleDeviceInformation => mtrBaseClusterAppleDeviceInformation -> Ptr () -> IO ()
readAttributeSupportsWEDWithCompletion mtrBaseClusterAppleDeviceInformation completion =
  sendMessage mtrBaseClusterAppleDeviceInformation readAttributeSupportsWEDWithCompletionSelector completion

-- | @- subscribeAttributeSupportsWEDWithParams:subscriptionEstablished:reportHandler:@
subscribeAttributeSupportsWEDWithParams_subscriptionEstablished_reportHandler :: (IsMTRBaseClusterAppleDeviceInformation mtrBaseClusterAppleDeviceInformation, IsMTRSubscribeParams params) => mtrBaseClusterAppleDeviceInformation -> params -> Ptr () -> Ptr () -> IO ()
subscribeAttributeSupportsWEDWithParams_subscriptionEstablished_reportHandler mtrBaseClusterAppleDeviceInformation params subscriptionEstablished reportHandler =
  sendMessage mtrBaseClusterAppleDeviceInformation subscribeAttributeSupportsWEDWithParams_subscriptionEstablished_reportHandlerSelector (toMTRSubscribeParams params) subscriptionEstablished reportHandler

-- | @+ readAttributeSupportsWEDWithClusterStateCache:endpoint:queue:completion:@
readAttributeSupportsWEDWithClusterStateCache_endpoint_queue_completion :: (IsMTRClusterStateCacheContainer clusterStateCacheContainer, IsNSNumber endpoint, IsNSObject queue) => clusterStateCacheContainer -> endpoint -> queue -> Ptr () -> IO ()
readAttributeSupportsWEDWithClusterStateCache_endpoint_queue_completion clusterStateCacheContainer endpoint queue completion =
  do
    cls' <- getRequiredClass "MTRBaseClusterAppleDeviceInformation"
    sendClassMessage cls' readAttributeSupportsWEDWithClusterStateCache_endpoint_queue_completionSelector (toMTRClusterStateCacheContainer clusterStateCacheContainer) (toNSNumber endpoint) (toNSObject queue) completion

-- | @- readAttributeGeneratedCommandListWithCompletion:@
readAttributeGeneratedCommandListWithCompletion :: IsMTRBaseClusterAppleDeviceInformation mtrBaseClusterAppleDeviceInformation => mtrBaseClusterAppleDeviceInformation -> Ptr () -> IO ()
readAttributeGeneratedCommandListWithCompletion mtrBaseClusterAppleDeviceInformation completion =
  sendMessage mtrBaseClusterAppleDeviceInformation readAttributeGeneratedCommandListWithCompletionSelector completion

-- | @- subscribeAttributeGeneratedCommandListWithParams:subscriptionEstablished:reportHandler:@
subscribeAttributeGeneratedCommandListWithParams_subscriptionEstablished_reportHandler :: (IsMTRBaseClusterAppleDeviceInformation mtrBaseClusterAppleDeviceInformation, IsMTRSubscribeParams params) => mtrBaseClusterAppleDeviceInformation -> params -> Ptr () -> Ptr () -> IO ()
subscribeAttributeGeneratedCommandListWithParams_subscriptionEstablished_reportHandler mtrBaseClusterAppleDeviceInformation params subscriptionEstablished reportHandler =
  sendMessage mtrBaseClusterAppleDeviceInformation subscribeAttributeGeneratedCommandListWithParams_subscriptionEstablished_reportHandlerSelector (toMTRSubscribeParams params) subscriptionEstablished reportHandler

-- | @+ readAttributeGeneratedCommandListWithClusterStateCache:endpoint:queue:completion:@
readAttributeGeneratedCommandListWithClusterStateCache_endpoint_queue_completion :: (IsMTRClusterStateCacheContainer clusterStateCacheContainer, IsNSNumber endpoint, IsNSObject queue) => clusterStateCacheContainer -> endpoint -> queue -> Ptr () -> IO ()
readAttributeGeneratedCommandListWithClusterStateCache_endpoint_queue_completion clusterStateCacheContainer endpoint queue completion =
  do
    cls' <- getRequiredClass "MTRBaseClusterAppleDeviceInformation"
    sendClassMessage cls' readAttributeGeneratedCommandListWithClusterStateCache_endpoint_queue_completionSelector (toMTRClusterStateCacheContainer clusterStateCacheContainer) (toNSNumber endpoint) (toNSObject queue) completion

-- | @- readAttributeAcceptedCommandListWithCompletion:@
readAttributeAcceptedCommandListWithCompletion :: IsMTRBaseClusterAppleDeviceInformation mtrBaseClusterAppleDeviceInformation => mtrBaseClusterAppleDeviceInformation -> Ptr () -> IO ()
readAttributeAcceptedCommandListWithCompletion mtrBaseClusterAppleDeviceInformation completion =
  sendMessage mtrBaseClusterAppleDeviceInformation readAttributeAcceptedCommandListWithCompletionSelector completion

-- | @- subscribeAttributeAcceptedCommandListWithParams:subscriptionEstablished:reportHandler:@
subscribeAttributeAcceptedCommandListWithParams_subscriptionEstablished_reportHandler :: (IsMTRBaseClusterAppleDeviceInformation mtrBaseClusterAppleDeviceInformation, IsMTRSubscribeParams params) => mtrBaseClusterAppleDeviceInformation -> params -> Ptr () -> Ptr () -> IO ()
subscribeAttributeAcceptedCommandListWithParams_subscriptionEstablished_reportHandler mtrBaseClusterAppleDeviceInformation params subscriptionEstablished reportHandler =
  sendMessage mtrBaseClusterAppleDeviceInformation subscribeAttributeAcceptedCommandListWithParams_subscriptionEstablished_reportHandlerSelector (toMTRSubscribeParams params) subscriptionEstablished reportHandler

-- | @+ readAttributeAcceptedCommandListWithClusterStateCache:endpoint:queue:completion:@
readAttributeAcceptedCommandListWithClusterStateCache_endpoint_queue_completion :: (IsMTRClusterStateCacheContainer clusterStateCacheContainer, IsNSNumber endpoint, IsNSObject queue) => clusterStateCacheContainer -> endpoint -> queue -> Ptr () -> IO ()
readAttributeAcceptedCommandListWithClusterStateCache_endpoint_queue_completion clusterStateCacheContainer endpoint queue completion =
  do
    cls' <- getRequiredClass "MTRBaseClusterAppleDeviceInformation"
    sendClassMessage cls' readAttributeAcceptedCommandListWithClusterStateCache_endpoint_queue_completionSelector (toMTRClusterStateCacheContainer clusterStateCacheContainer) (toNSNumber endpoint) (toNSObject queue) completion

-- | @- readAttributeAttributeListWithCompletion:@
readAttributeAttributeListWithCompletion :: IsMTRBaseClusterAppleDeviceInformation mtrBaseClusterAppleDeviceInformation => mtrBaseClusterAppleDeviceInformation -> Ptr () -> IO ()
readAttributeAttributeListWithCompletion mtrBaseClusterAppleDeviceInformation completion =
  sendMessage mtrBaseClusterAppleDeviceInformation readAttributeAttributeListWithCompletionSelector completion

-- | @- subscribeAttributeAttributeListWithParams:subscriptionEstablished:reportHandler:@
subscribeAttributeAttributeListWithParams_subscriptionEstablished_reportHandler :: (IsMTRBaseClusterAppleDeviceInformation mtrBaseClusterAppleDeviceInformation, IsMTRSubscribeParams params) => mtrBaseClusterAppleDeviceInformation -> params -> Ptr () -> Ptr () -> IO ()
subscribeAttributeAttributeListWithParams_subscriptionEstablished_reportHandler mtrBaseClusterAppleDeviceInformation params subscriptionEstablished reportHandler =
  sendMessage mtrBaseClusterAppleDeviceInformation subscribeAttributeAttributeListWithParams_subscriptionEstablished_reportHandlerSelector (toMTRSubscribeParams params) subscriptionEstablished reportHandler

-- | @+ readAttributeAttributeListWithClusterStateCache:endpoint:queue:completion:@
readAttributeAttributeListWithClusterStateCache_endpoint_queue_completion :: (IsMTRClusterStateCacheContainer clusterStateCacheContainer, IsNSNumber endpoint, IsNSObject queue) => clusterStateCacheContainer -> endpoint -> queue -> Ptr () -> IO ()
readAttributeAttributeListWithClusterStateCache_endpoint_queue_completion clusterStateCacheContainer endpoint queue completion =
  do
    cls' <- getRequiredClass "MTRBaseClusterAppleDeviceInformation"
    sendClassMessage cls' readAttributeAttributeListWithClusterStateCache_endpoint_queue_completionSelector (toMTRClusterStateCacheContainer clusterStateCacheContainer) (toNSNumber endpoint) (toNSObject queue) completion

-- | @- readAttributeFeatureMapWithCompletion:@
readAttributeFeatureMapWithCompletion :: IsMTRBaseClusterAppleDeviceInformation mtrBaseClusterAppleDeviceInformation => mtrBaseClusterAppleDeviceInformation -> Ptr () -> IO ()
readAttributeFeatureMapWithCompletion mtrBaseClusterAppleDeviceInformation completion =
  sendMessage mtrBaseClusterAppleDeviceInformation readAttributeFeatureMapWithCompletionSelector completion

-- | @- subscribeAttributeFeatureMapWithParams:subscriptionEstablished:reportHandler:@
subscribeAttributeFeatureMapWithParams_subscriptionEstablished_reportHandler :: (IsMTRBaseClusterAppleDeviceInformation mtrBaseClusterAppleDeviceInformation, IsMTRSubscribeParams params) => mtrBaseClusterAppleDeviceInformation -> params -> Ptr () -> Ptr () -> IO ()
subscribeAttributeFeatureMapWithParams_subscriptionEstablished_reportHandler mtrBaseClusterAppleDeviceInformation params subscriptionEstablished reportHandler =
  sendMessage mtrBaseClusterAppleDeviceInformation subscribeAttributeFeatureMapWithParams_subscriptionEstablished_reportHandlerSelector (toMTRSubscribeParams params) subscriptionEstablished reportHandler

-- | @+ readAttributeFeatureMapWithClusterStateCache:endpoint:queue:completion:@
readAttributeFeatureMapWithClusterStateCache_endpoint_queue_completion :: (IsMTRClusterStateCacheContainer clusterStateCacheContainer, IsNSNumber endpoint, IsNSObject queue) => clusterStateCacheContainer -> endpoint -> queue -> Ptr () -> IO ()
readAttributeFeatureMapWithClusterStateCache_endpoint_queue_completion clusterStateCacheContainer endpoint queue completion =
  do
    cls' <- getRequiredClass "MTRBaseClusterAppleDeviceInformation"
    sendClassMessage cls' readAttributeFeatureMapWithClusterStateCache_endpoint_queue_completionSelector (toMTRClusterStateCacheContainer clusterStateCacheContainer) (toNSNumber endpoint) (toNSObject queue) completion

-- | @- readAttributeClusterRevisionWithCompletion:@
readAttributeClusterRevisionWithCompletion :: IsMTRBaseClusterAppleDeviceInformation mtrBaseClusterAppleDeviceInformation => mtrBaseClusterAppleDeviceInformation -> Ptr () -> IO ()
readAttributeClusterRevisionWithCompletion mtrBaseClusterAppleDeviceInformation completion =
  sendMessage mtrBaseClusterAppleDeviceInformation readAttributeClusterRevisionWithCompletionSelector completion

-- | @- subscribeAttributeClusterRevisionWithParams:subscriptionEstablished:reportHandler:@
subscribeAttributeClusterRevisionWithParams_subscriptionEstablished_reportHandler :: (IsMTRBaseClusterAppleDeviceInformation mtrBaseClusterAppleDeviceInformation, IsMTRSubscribeParams params) => mtrBaseClusterAppleDeviceInformation -> params -> Ptr () -> Ptr () -> IO ()
subscribeAttributeClusterRevisionWithParams_subscriptionEstablished_reportHandler mtrBaseClusterAppleDeviceInformation params subscriptionEstablished reportHandler =
  sendMessage mtrBaseClusterAppleDeviceInformation subscribeAttributeClusterRevisionWithParams_subscriptionEstablished_reportHandlerSelector (toMTRSubscribeParams params) subscriptionEstablished reportHandler

-- | @+ readAttributeClusterRevisionWithClusterStateCache:endpoint:queue:completion:@
readAttributeClusterRevisionWithClusterStateCache_endpoint_queue_completion :: (IsMTRClusterStateCacheContainer clusterStateCacheContainer, IsNSNumber endpoint, IsNSObject queue) => clusterStateCacheContainer -> endpoint -> queue -> Ptr () -> IO ()
readAttributeClusterRevisionWithClusterStateCache_endpoint_queue_completion clusterStateCacheContainer endpoint queue completion =
  do
    cls' <- getRequiredClass "MTRBaseClusterAppleDeviceInformation"
    sendClassMessage cls' readAttributeClusterRevisionWithClusterStateCache_endpoint_queue_completionSelector (toMTRClusterStateCacheContainer clusterStateCacheContainer) (toNSNumber endpoint) (toNSObject queue) completion

-- | @- init@
init_ :: IsMTRBaseClusterAppleDeviceInformation mtrBaseClusterAppleDeviceInformation => mtrBaseClusterAppleDeviceInformation -> IO (Id MTRBaseClusterAppleDeviceInformation)
init_ mtrBaseClusterAppleDeviceInformation =
  sendOwnedMessage mtrBaseClusterAppleDeviceInformation initSelector

-- | @+ new@
new :: IO (Id MTRBaseClusterAppleDeviceInformation)
new  =
  do
    cls' <- getRequiredClass "MTRBaseClusterAppleDeviceInformation"
    sendOwnedClassMessage cls' newSelector

-- | For all instance methods (reads, writes, commands) that take a completion, the completion will be called on the provided queue.
--
-- ObjC selector: @- initWithDevice:endpointID:queue:@
initWithDevice_endpointID_queue :: (IsMTRBaseClusterAppleDeviceInformation mtrBaseClusterAppleDeviceInformation, IsMTRBaseDevice device, IsNSNumber endpointID, IsNSObject queue) => mtrBaseClusterAppleDeviceInformation -> device -> endpointID -> queue -> IO (Id MTRBaseClusterAppleDeviceInformation)
initWithDevice_endpointID_queue mtrBaseClusterAppleDeviceInformation device endpointID queue =
  sendOwnedMessage mtrBaseClusterAppleDeviceInformation initWithDevice_endpointID_queueSelector (toMTRBaseDevice device) (toNSNumber endpointID) (toNSObject queue)

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @readAttributeSupportsTapToUnlockWithCompletion:@
readAttributeSupportsTapToUnlockWithCompletionSelector :: Selector '[Ptr ()] ()
readAttributeSupportsTapToUnlockWithCompletionSelector = mkSelector "readAttributeSupportsTapToUnlockWithCompletion:"

-- | @Selector@ for @subscribeAttributeSupportsTapToUnlockWithParams:subscriptionEstablished:reportHandler:@
subscribeAttributeSupportsTapToUnlockWithParams_subscriptionEstablished_reportHandlerSelector :: Selector '[Id MTRSubscribeParams, Ptr (), Ptr ()] ()
subscribeAttributeSupportsTapToUnlockWithParams_subscriptionEstablished_reportHandlerSelector = mkSelector "subscribeAttributeSupportsTapToUnlockWithParams:subscriptionEstablished:reportHandler:"

-- | @Selector@ for @readAttributeSupportsTapToUnlockWithClusterStateCache:endpoint:queue:completion:@
readAttributeSupportsTapToUnlockWithClusterStateCache_endpoint_queue_completionSelector :: Selector '[Id MTRClusterStateCacheContainer, Id NSNumber, Id NSObject, Ptr ()] ()
readAttributeSupportsTapToUnlockWithClusterStateCache_endpoint_queue_completionSelector = mkSelector "readAttributeSupportsTapToUnlockWithClusterStateCache:endpoint:queue:completion:"

-- | @Selector@ for @readAttributeSupportsWEDWithCompletion:@
readAttributeSupportsWEDWithCompletionSelector :: Selector '[Ptr ()] ()
readAttributeSupportsWEDWithCompletionSelector = mkSelector "readAttributeSupportsWEDWithCompletion:"

-- | @Selector@ for @subscribeAttributeSupportsWEDWithParams:subscriptionEstablished:reportHandler:@
subscribeAttributeSupportsWEDWithParams_subscriptionEstablished_reportHandlerSelector :: Selector '[Id MTRSubscribeParams, Ptr (), Ptr ()] ()
subscribeAttributeSupportsWEDWithParams_subscriptionEstablished_reportHandlerSelector = mkSelector "subscribeAttributeSupportsWEDWithParams:subscriptionEstablished:reportHandler:"

-- | @Selector@ for @readAttributeSupportsWEDWithClusterStateCache:endpoint:queue:completion:@
readAttributeSupportsWEDWithClusterStateCache_endpoint_queue_completionSelector :: Selector '[Id MTRClusterStateCacheContainer, Id NSNumber, Id NSObject, Ptr ()] ()
readAttributeSupportsWEDWithClusterStateCache_endpoint_queue_completionSelector = mkSelector "readAttributeSupportsWEDWithClusterStateCache:endpoint:queue:completion:"

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
initSelector :: Selector '[] (Id MTRBaseClusterAppleDeviceInformation)
initSelector = mkSelector "init"

-- | @Selector@ for @new@
newSelector :: Selector '[] (Id MTRBaseClusterAppleDeviceInformation)
newSelector = mkSelector "new"

-- | @Selector@ for @initWithDevice:endpointID:queue:@
initWithDevice_endpointID_queueSelector :: Selector '[Id MTRBaseDevice, Id NSNumber, Id NSObject] (Id MTRBaseClusterAppleDeviceInformation)
initWithDevice_endpointID_queueSelector = mkSelector "initWithDevice:endpointID:queue:"

