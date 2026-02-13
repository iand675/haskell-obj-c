{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Cluster Wake on LAN
--
-- This cluster provides an interface for managing low power mode on a device that supports the Wake On LAN protocol.
--
-- Generated bindings for @MTRBaseClusterWakeOnLAN@.
module ObjC.Matter.MTRBaseClusterWakeOnLAN
  ( MTRBaseClusterWakeOnLAN
  , IsMTRBaseClusterWakeOnLAN(..)
  , readAttributeMACAddressWithCompletion
  , subscribeAttributeMACAddressWithParams_subscriptionEstablished_reportHandler
  , readAttributeMACAddressWithClusterStateCache_endpoint_queue_completion
  , readAttributeLinkLocalAddressWithCompletion
  , subscribeAttributeLinkLocalAddressWithParams_subscriptionEstablished_reportHandler
  , readAttributeLinkLocalAddressWithClusterStateCache_endpoint_queue_completion
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
  , readAttributeLinkLocalAddressWithClusterStateCache_endpoint_queue_completionSelector
  , readAttributeLinkLocalAddressWithCompletionSelector
  , readAttributeMACAddressWithClusterStateCache_endpoint_queue_completionSelector
  , readAttributeMACAddressWithCompletionSelector
  , subscribeAttributeAcceptedCommandListWithParams_subscriptionEstablished_reportHandlerSelector
  , subscribeAttributeAttributeListWithParams_subscriptionEstablished_reportHandlerSelector
  , subscribeAttributeClusterRevisionWithParams_subscriptionEstablished_reportHandlerSelector
  , subscribeAttributeFeatureMapWithParams_subscriptionEstablished_reportHandlerSelector
  , subscribeAttributeGeneratedCommandListWithParams_subscriptionEstablished_reportHandlerSelector
  , subscribeAttributeLinkLocalAddressWithParams_subscriptionEstablished_reportHandlerSelector
  , subscribeAttributeMACAddressWithParams_subscriptionEstablished_reportHandlerSelector


  ) where

import Foreign.Ptr (Ptr, FunPtr)
import Foreign.C.Types

import ObjC.Runtime.Types
import ObjC.Runtime.Message (sendMessage, sendOwnedMessage, sendClassMessage, sendOwnedClassMessage)
import ObjC.Runtime.Selector (mkSelector)
import ObjC.Runtime.Class (getRequiredClass)

import ObjC.Matter.Internal.Classes
import ObjC.Foundation.Internal.Classes

-- | @- readAttributeMACAddressWithCompletion:@
readAttributeMACAddressWithCompletion :: IsMTRBaseClusterWakeOnLAN mtrBaseClusterWakeOnLAN => mtrBaseClusterWakeOnLAN -> Ptr () -> IO ()
readAttributeMACAddressWithCompletion mtrBaseClusterWakeOnLAN completion =
  sendMessage mtrBaseClusterWakeOnLAN readAttributeMACAddressWithCompletionSelector completion

-- | @- subscribeAttributeMACAddressWithParams:subscriptionEstablished:reportHandler:@
subscribeAttributeMACAddressWithParams_subscriptionEstablished_reportHandler :: (IsMTRBaseClusterWakeOnLAN mtrBaseClusterWakeOnLAN, IsMTRSubscribeParams params) => mtrBaseClusterWakeOnLAN -> params -> Ptr () -> Ptr () -> IO ()
subscribeAttributeMACAddressWithParams_subscriptionEstablished_reportHandler mtrBaseClusterWakeOnLAN params subscriptionEstablished reportHandler =
  sendMessage mtrBaseClusterWakeOnLAN subscribeAttributeMACAddressWithParams_subscriptionEstablished_reportHandlerSelector (toMTRSubscribeParams params) subscriptionEstablished reportHandler

-- | @+ readAttributeMACAddressWithClusterStateCache:endpoint:queue:completion:@
readAttributeMACAddressWithClusterStateCache_endpoint_queue_completion :: (IsMTRClusterStateCacheContainer clusterStateCacheContainer, IsNSNumber endpoint, IsNSObject queue) => clusterStateCacheContainer -> endpoint -> queue -> Ptr () -> IO ()
readAttributeMACAddressWithClusterStateCache_endpoint_queue_completion clusterStateCacheContainer endpoint queue completion =
  do
    cls' <- getRequiredClass "MTRBaseClusterWakeOnLAN"
    sendClassMessage cls' readAttributeMACAddressWithClusterStateCache_endpoint_queue_completionSelector (toMTRClusterStateCacheContainer clusterStateCacheContainer) (toNSNumber endpoint) (toNSObject queue) completion

-- | @- readAttributeLinkLocalAddressWithCompletion:@
readAttributeLinkLocalAddressWithCompletion :: IsMTRBaseClusterWakeOnLAN mtrBaseClusterWakeOnLAN => mtrBaseClusterWakeOnLAN -> Ptr () -> IO ()
readAttributeLinkLocalAddressWithCompletion mtrBaseClusterWakeOnLAN completion =
  sendMessage mtrBaseClusterWakeOnLAN readAttributeLinkLocalAddressWithCompletionSelector completion

-- | @- subscribeAttributeLinkLocalAddressWithParams:subscriptionEstablished:reportHandler:@
subscribeAttributeLinkLocalAddressWithParams_subscriptionEstablished_reportHandler :: (IsMTRBaseClusterWakeOnLAN mtrBaseClusterWakeOnLAN, IsMTRSubscribeParams params) => mtrBaseClusterWakeOnLAN -> params -> Ptr () -> Ptr () -> IO ()
subscribeAttributeLinkLocalAddressWithParams_subscriptionEstablished_reportHandler mtrBaseClusterWakeOnLAN params subscriptionEstablished reportHandler =
  sendMessage mtrBaseClusterWakeOnLAN subscribeAttributeLinkLocalAddressWithParams_subscriptionEstablished_reportHandlerSelector (toMTRSubscribeParams params) subscriptionEstablished reportHandler

-- | @+ readAttributeLinkLocalAddressWithClusterStateCache:endpoint:queue:completion:@
readAttributeLinkLocalAddressWithClusterStateCache_endpoint_queue_completion :: (IsMTRClusterStateCacheContainer clusterStateCacheContainer, IsNSNumber endpoint, IsNSObject queue) => clusterStateCacheContainer -> endpoint -> queue -> Ptr () -> IO ()
readAttributeLinkLocalAddressWithClusterStateCache_endpoint_queue_completion clusterStateCacheContainer endpoint queue completion =
  do
    cls' <- getRequiredClass "MTRBaseClusterWakeOnLAN"
    sendClassMessage cls' readAttributeLinkLocalAddressWithClusterStateCache_endpoint_queue_completionSelector (toMTRClusterStateCacheContainer clusterStateCacheContainer) (toNSNumber endpoint) (toNSObject queue) completion

-- | @- readAttributeGeneratedCommandListWithCompletion:@
readAttributeGeneratedCommandListWithCompletion :: IsMTRBaseClusterWakeOnLAN mtrBaseClusterWakeOnLAN => mtrBaseClusterWakeOnLAN -> Ptr () -> IO ()
readAttributeGeneratedCommandListWithCompletion mtrBaseClusterWakeOnLAN completion =
  sendMessage mtrBaseClusterWakeOnLAN readAttributeGeneratedCommandListWithCompletionSelector completion

-- | @- subscribeAttributeGeneratedCommandListWithParams:subscriptionEstablished:reportHandler:@
subscribeAttributeGeneratedCommandListWithParams_subscriptionEstablished_reportHandler :: (IsMTRBaseClusterWakeOnLAN mtrBaseClusterWakeOnLAN, IsMTRSubscribeParams params) => mtrBaseClusterWakeOnLAN -> params -> Ptr () -> Ptr () -> IO ()
subscribeAttributeGeneratedCommandListWithParams_subscriptionEstablished_reportHandler mtrBaseClusterWakeOnLAN params subscriptionEstablished reportHandler =
  sendMessage mtrBaseClusterWakeOnLAN subscribeAttributeGeneratedCommandListWithParams_subscriptionEstablished_reportHandlerSelector (toMTRSubscribeParams params) subscriptionEstablished reportHandler

-- | @+ readAttributeGeneratedCommandListWithClusterStateCache:endpoint:queue:completion:@
readAttributeGeneratedCommandListWithClusterStateCache_endpoint_queue_completion :: (IsMTRClusterStateCacheContainer clusterStateCacheContainer, IsNSNumber endpoint, IsNSObject queue) => clusterStateCacheContainer -> endpoint -> queue -> Ptr () -> IO ()
readAttributeGeneratedCommandListWithClusterStateCache_endpoint_queue_completion clusterStateCacheContainer endpoint queue completion =
  do
    cls' <- getRequiredClass "MTRBaseClusterWakeOnLAN"
    sendClassMessage cls' readAttributeGeneratedCommandListWithClusterStateCache_endpoint_queue_completionSelector (toMTRClusterStateCacheContainer clusterStateCacheContainer) (toNSNumber endpoint) (toNSObject queue) completion

-- | @- readAttributeAcceptedCommandListWithCompletion:@
readAttributeAcceptedCommandListWithCompletion :: IsMTRBaseClusterWakeOnLAN mtrBaseClusterWakeOnLAN => mtrBaseClusterWakeOnLAN -> Ptr () -> IO ()
readAttributeAcceptedCommandListWithCompletion mtrBaseClusterWakeOnLAN completion =
  sendMessage mtrBaseClusterWakeOnLAN readAttributeAcceptedCommandListWithCompletionSelector completion

-- | @- subscribeAttributeAcceptedCommandListWithParams:subscriptionEstablished:reportHandler:@
subscribeAttributeAcceptedCommandListWithParams_subscriptionEstablished_reportHandler :: (IsMTRBaseClusterWakeOnLAN mtrBaseClusterWakeOnLAN, IsMTRSubscribeParams params) => mtrBaseClusterWakeOnLAN -> params -> Ptr () -> Ptr () -> IO ()
subscribeAttributeAcceptedCommandListWithParams_subscriptionEstablished_reportHandler mtrBaseClusterWakeOnLAN params subscriptionEstablished reportHandler =
  sendMessage mtrBaseClusterWakeOnLAN subscribeAttributeAcceptedCommandListWithParams_subscriptionEstablished_reportHandlerSelector (toMTRSubscribeParams params) subscriptionEstablished reportHandler

-- | @+ readAttributeAcceptedCommandListWithClusterStateCache:endpoint:queue:completion:@
readAttributeAcceptedCommandListWithClusterStateCache_endpoint_queue_completion :: (IsMTRClusterStateCacheContainer clusterStateCacheContainer, IsNSNumber endpoint, IsNSObject queue) => clusterStateCacheContainer -> endpoint -> queue -> Ptr () -> IO ()
readAttributeAcceptedCommandListWithClusterStateCache_endpoint_queue_completion clusterStateCacheContainer endpoint queue completion =
  do
    cls' <- getRequiredClass "MTRBaseClusterWakeOnLAN"
    sendClassMessage cls' readAttributeAcceptedCommandListWithClusterStateCache_endpoint_queue_completionSelector (toMTRClusterStateCacheContainer clusterStateCacheContainer) (toNSNumber endpoint) (toNSObject queue) completion

-- | @- readAttributeAttributeListWithCompletion:@
readAttributeAttributeListWithCompletion :: IsMTRBaseClusterWakeOnLAN mtrBaseClusterWakeOnLAN => mtrBaseClusterWakeOnLAN -> Ptr () -> IO ()
readAttributeAttributeListWithCompletion mtrBaseClusterWakeOnLAN completion =
  sendMessage mtrBaseClusterWakeOnLAN readAttributeAttributeListWithCompletionSelector completion

-- | @- subscribeAttributeAttributeListWithParams:subscriptionEstablished:reportHandler:@
subscribeAttributeAttributeListWithParams_subscriptionEstablished_reportHandler :: (IsMTRBaseClusterWakeOnLAN mtrBaseClusterWakeOnLAN, IsMTRSubscribeParams params) => mtrBaseClusterWakeOnLAN -> params -> Ptr () -> Ptr () -> IO ()
subscribeAttributeAttributeListWithParams_subscriptionEstablished_reportHandler mtrBaseClusterWakeOnLAN params subscriptionEstablished reportHandler =
  sendMessage mtrBaseClusterWakeOnLAN subscribeAttributeAttributeListWithParams_subscriptionEstablished_reportHandlerSelector (toMTRSubscribeParams params) subscriptionEstablished reportHandler

-- | @+ readAttributeAttributeListWithClusterStateCache:endpoint:queue:completion:@
readAttributeAttributeListWithClusterStateCache_endpoint_queue_completion :: (IsMTRClusterStateCacheContainer clusterStateCacheContainer, IsNSNumber endpoint, IsNSObject queue) => clusterStateCacheContainer -> endpoint -> queue -> Ptr () -> IO ()
readAttributeAttributeListWithClusterStateCache_endpoint_queue_completion clusterStateCacheContainer endpoint queue completion =
  do
    cls' <- getRequiredClass "MTRBaseClusterWakeOnLAN"
    sendClassMessage cls' readAttributeAttributeListWithClusterStateCache_endpoint_queue_completionSelector (toMTRClusterStateCacheContainer clusterStateCacheContainer) (toNSNumber endpoint) (toNSObject queue) completion

-- | @- readAttributeFeatureMapWithCompletion:@
readAttributeFeatureMapWithCompletion :: IsMTRBaseClusterWakeOnLAN mtrBaseClusterWakeOnLAN => mtrBaseClusterWakeOnLAN -> Ptr () -> IO ()
readAttributeFeatureMapWithCompletion mtrBaseClusterWakeOnLAN completion =
  sendMessage mtrBaseClusterWakeOnLAN readAttributeFeatureMapWithCompletionSelector completion

-- | @- subscribeAttributeFeatureMapWithParams:subscriptionEstablished:reportHandler:@
subscribeAttributeFeatureMapWithParams_subscriptionEstablished_reportHandler :: (IsMTRBaseClusterWakeOnLAN mtrBaseClusterWakeOnLAN, IsMTRSubscribeParams params) => mtrBaseClusterWakeOnLAN -> params -> Ptr () -> Ptr () -> IO ()
subscribeAttributeFeatureMapWithParams_subscriptionEstablished_reportHandler mtrBaseClusterWakeOnLAN params subscriptionEstablished reportHandler =
  sendMessage mtrBaseClusterWakeOnLAN subscribeAttributeFeatureMapWithParams_subscriptionEstablished_reportHandlerSelector (toMTRSubscribeParams params) subscriptionEstablished reportHandler

-- | @+ readAttributeFeatureMapWithClusterStateCache:endpoint:queue:completion:@
readAttributeFeatureMapWithClusterStateCache_endpoint_queue_completion :: (IsMTRClusterStateCacheContainer clusterStateCacheContainer, IsNSNumber endpoint, IsNSObject queue) => clusterStateCacheContainer -> endpoint -> queue -> Ptr () -> IO ()
readAttributeFeatureMapWithClusterStateCache_endpoint_queue_completion clusterStateCacheContainer endpoint queue completion =
  do
    cls' <- getRequiredClass "MTRBaseClusterWakeOnLAN"
    sendClassMessage cls' readAttributeFeatureMapWithClusterStateCache_endpoint_queue_completionSelector (toMTRClusterStateCacheContainer clusterStateCacheContainer) (toNSNumber endpoint) (toNSObject queue) completion

-- | @- readAttributeClusterRevisionWithCompletion:@
readAttributeClusterRevisionWithCompletion :: IsMTRBaseClusterWakeOnLAN mtrBaseClusterWakeOnLAN => mtrBaseClusterWakeOnLAN -> Ptr () -> IO ()
readAttributeClusterRevisionWithCompletion mtrBaseClusterWakeOnLAN completion =
  sendMessage mtrBaseClusterWakeOnLAN readAttributeClusterRevisionWithCompletionSelector completion

-- | @- subscribeAttributeClusterRevisionWithParams:subscriptionEstablished:reportHandler:@
subscribeAttributeClusterRevisionWithParams_subscriptionEstablished_reportHandler :: (IsMTRBaseClusterWakeOnLAN mtrBaseClusterWakeOnLAN, IsMTRSubscribeParams params) => mtrBaseClusterWakeOnLAN -> params -> Ptr () -> Ptr () -> IO ()
subscribeAttributeClusterRevisionWithParams_subscriptionEstablished_reportHandler mtrBaseClusterWakeOnLAN params subscriptionEstablished reportHandler =
  sendMessage mtrBaseClusterWakeOnLAN subscribeAttributeClusterRevisionWithParams_subscriptionEstablished_reportHandlerSelector (toMTRSubscribeParams params) subscriptionEstablished reportHandler

-- | @+ readAttributeClusterRevisionWithClusterStateCache:endpoint:queue:completion:@
readAttributeClusterRevisionWithClusterStateCache_endpoint_queue_completion :: (IsMTRClusterStateCacheContainer clusterStateCacheContainer, IsNSNumber endpoint, IsNSObject queue) => clusterStateCacheContainer -> endpoint -> queue -> Ptr () -> IO ()
readAttributeClusterRevisionWithClusterStateCache_endpoint_queue_completion clusterStateCacheContainer endpoint queue completion =
  do
    cls' <- getRequiredClass "MTRBaseClusterWakeOnLAN"
    sendClassMessage cls' readAttributeClusterRevisionWithClusterStateCache_endpoint_queue_completionSelector (toMTRClusterStateCacheContainer clusterStateCacheContainer) (toNSNumber endpoint) (toNSObject queue) completion

-- | @- init@
init_ :: IsMTRBaseClusterWakeOnLAN mtrBaseClusterWakeOnLAN => mtrBaseClusterWakeOnLAN -> IO (Id MTRBaseClusterWakeOnLAN)
init_ mtrBaseClusterWakeOnLAN =
  sendOwnedMessage mtrBaseClusterWakeOnLAN initSelector

-- | @+ new@
new :: IO (Id MTRBaseClusterWakeOnLAN)
new  =
  do
    cls' <- getRequiredClass "MTRBaseClusterWakeOnLAN"
    sendOwnedClassMessage cls' newSelector

-- | For all instance methods (reads, writes, commands) that take a completion, the completion will be called on the provided queue.
--
-- ObjC selector: @- initWithDevice:endpointID:queue:@
initWithDevice_endpointID_queue :: (IsMTRBaseClusterWakeOnLAN mtrBaseClusterWakeOnLAN, IsMTRBaseDevice device, IsNSNumber endpointID, IsNSObject queue) => mtrBaseClusterWakeOnLAN -> device -> endpointID -> queue -> IO (Id MTRBaseClusterWakeOnLAN)
initWithDevice_endpointID_queue mtrBaseClusterWakeOnLAN device endpointID queue =
  sendOwnedMessage mtrBaseClusterWakeOnLAN initWithDevice_endpointID_queueSelector (toMTRBaseDevice device) (toNSNumber endpointID) (toNSObject queue)

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @readAttributeMACAddressWithCompletion:@
readAttributeMACAddressWithCompletionSelector :: Selector '[Ptr ()] ()
readAttributeMACAddressWithCompletionSelector = mkSelector "readAttributeMACAddressWithCompletion:"

-- | @Selector@ for @subscribeAttributeMACAddressWithParams:subscriptionEstablished:reportHandler:@
subscribeAttributeMACAddressWithParams_subscriptionEstablished_reportHandlerSelector :: Selector '[Id MTRSubscribeParams, Ptr (), Ptr ()] ()
subscribeAttributeMACAddressWithParams_subscriptionEstablished_reportHandlerSelector = mkSelector "subscribeAttributeMACAddressWithParams:subscriptionEstablished:reportHandler:"

-- | @Selector@ for @readAttributeMACAddressWithClusterStateCache:endpoint:queue:completion:@
readAttributeMACAddressWithClusterStateCache_endpoint_queue_completionSelector :: Selector '[Id MTRClusterStateCacheContainer, Id NSNumber, Id NSObject, Ptr ()] ()
readAttributeMACAddressWithClusterStateCache_endpoint_queue_completionSelector = mkSelector "readAttributeMACAddressWithClusterStateCache:endpoint:queue:completion:"

-- | @Selector@ for @readAttributeLinkLocalAddressWithCompletion:@
readAttributeLinkLocalAddressWithCompletionSelector :: Selector '[Ptr ()] ()
readAttributeLinkLocalAddressWithCompletionSelector = mkSelector "readAttributeLinkLocalAddressWithCompletion:"

-- | @Selector@ for @subscribeAttributeLinkLocalAddressWithParams:subscriptionEstablished:reportHandler:@
subscribeAttributeLinkLocalAddressWithParams_subscriptionEstablished_reportHandlerSelector :: Selector '[Id MTRSubscribeParams, Ptr (), Ptr ()] ()
subscribeAttributeLinkLocalAddressWithParams_subscriptionEstablished_reportHandlerSelector = mkSelector "subscribeAttributeLinkLocalAddressWithParams:subscriptionEstablished:reportHandler:"

-- | @Selector@ for @readAttributeLinkLocalAddressWithClusterStateCache:endpoint:queue:completion:@
readAttributeLinkLocalAddressWithClusterStateCache_endpoint_queue_completionSelector :: Selector '[Id MTRClusterStateCacheContainer, Id NSNumber, Id NSObject, Ptr ()] ()
readAttributeLinkLocalAddressWithClusterStateCache_endpoint_queue_completionSelector = mkSelector "readAttributeLinkLocalAddressWithClusterStateCache:endpoint:queue:completion:"

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
initSelector :: Selector '[] (Id MTRBaseClusterWakeOnLAN)
initSelector = mkSelector "init"

-- | @Selector@ for @new@
newSelector :: Selector '[] (Id MTRBaseClusterWakeOnLAN)
newSelector = mkSelector "new"

-- | @Selector@ for @initWithDevice:endpointID:queue:@
initWithDevice_endpointID_queueSelector :: Selector '[Id MTRBaseDevice, Id NSNumber, Id NSObject] (Id MTRBaseClusterWakeOnLAN)
initWithDevice_endpointID_queueSelector = mkSelector "initWithDevice:endpointID:queue:"

