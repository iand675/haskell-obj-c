{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Cluster Push AV Stream Transport
--
-- This cluster implements the upload of Audio and Video streams from the Push AV Stream Transport Cluster using suitable push-based transports.
--
-- Generated bindings for @MTRBaseClusterPushAVStreamTransport@.
module ObjC.Matter.MTRBaseClusterPushAVStreamTransport
  ( MTRBaseClusterPushAVStreamTransport
  , IsMTRBaseClusterPushAVStreamTransport(..)
  , allocatePushTransportWithParams_completion
  , deallocatePushTransportWithParams_completion
  , modifyPushTransportWithParams_completion
  , setTransportStatusWithParams_completion
  , manuallyTriggerTransportWithParams_completion
  , findTransportWithParams_completion
  , readAttributeSupportedFormatsWithCompletion
  , subscribeAttributeSupportedFormatsWithParams_subscriptionEstablished_reportHandler
  , readAttributeSupportedFormatsWithClusterStateCache_endpoint_queue_completion
  , readAttributeCurrentConnectionsWithParams_completion
  , subscribeAttributeCurrentConnectionsWithParams_subscriptionEstablished_reportHandler
  , readAttributeCurrentConnectionsWithClusterStateCache_endpoint_queue_completion
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
  , allocatePushTransportWithParams_completionSelector
  , deallocatePushTransportWithParams_completionSelector
  , findTransportWithParams_completionSelector
  , initSelector
  , initWithDevice_endpointID_queueSelector
  , manuallyTriggerTransportWithParams_completionSelector
  , modifyPushTransportWithParams_completionSelector
  , newSelector
  , readAttributeAcceptedCommandListWithClusterStateCache_endpoint_queue_completionSelector
  , readAttributeAcceptedCommandListWithCompletionSelector
  , readAttributeAttributeListWithClusterStateCache_endpoint_queue_completionSelector
  , readAttributeAttributeListWithCompletionSelector
  , readAttributeClusterRevisionWithClusterStateCache_endpoint_queue_completionSelector
  , readAttributeClusterRevisionWithCompletionSelector
  , readAttributeCurrentConnectionsWithClusterStateCache_endpoint_queue_completionSelector
  , readAttributeCurrentConnectionsWithParams_completionSelector
  , readAttributeFeatureMapWithClusterStateCache_endpoint_queue_completionSelector
  , readAttributeFeatureMapWithCompletionSelector
  , readAttributeGeneratedCommandListWithClusterStateCache_endpoint_queue_completionSelector
  , readAttributeGeneratedCommandListWithCompletionSelector
  , readAttributeSupportedFormatsWithClusterStateCache_endpoint_queue_completionSelector
  , readAttributeSupportedFormatsWithCompletionSelector
  , setTransportStatusWithParams_completionSelector
  , subscribeAttributeAcceptedCommandListWithParams_subscriptionEstablished_reportHandlerSelector
  , subscribeAttributeAttributeListWithParams_subscriptionEstablished_reportHandlerSelector
  , subscribeAttributeClusterRevisionWithParams_subscriptionEstablished_reportHandlerSelector
  , subscribeAttributeCurrentConnectionsWithParams_subscriptionEstablished_reportHandlerSelector
  , subscribeAttributeFeatureMapWithParams_subscriptionEstablished_reportHandlerSelector
  , subscribeAttributeGeneratedCommandListWithParams_subscriptionEstablished_reportHandlerSelector
  , subscribeAttributeSupportedFormatsWithParams_subscriptionEstablished_reportHandlerSelector


  ) where

import Foreign.Ptr (Ptr, FunPtr)
import Foreign.C.Types

import ObjC.Runtime.Types
import ObjC.Runtime.Message (sendMessage, sendOwnedMessage, sendClassMessage, sendOwnedClassMessage)
import ObjC.Runtime.Selector (mkSelector)
import ObjC.Runtime.Class (getRequiredClass)

import ObjC.Matter.Internal.Classes
import ObjC.Foundation.Internal.Classes

-- | Command AllocatePushTransport
--
-- This command SHALL allocate a transport and return a PushTransportConnectionID.
--
-- ObjC selector: @- allocatePushTransportWithParams:completion:@
allocatePushTransportWithParams_completion :: (IsMTRBaseClusterPushAVStreamTransport mtrBaseClusterPushAVStreamTransport, IsMTRPushAVStreamTransportClusterAllocatePushTransportParams params) => mtrBaseClusterPushAVStreamTransport -> params -> Ptr () -> IO ()
allocatePushTransportWithParams_completion mtrBaseClusterPushAVStreamTransport params completion =
  sendOwnedMessage mtrBaseClusterPushAVStreamTransport allocatePushTransportWithParams_completionSelector (toMTRPushAVStreamTransportClusterAllocatePushTransportParams params) completion

-- | Command DeallocatePushTransport
--
-- This command SHALL be generated to request the Node deallocates the specified transport.
--
-- ObjC selector: @- deallocatePushTransportWithParams:completion:@
deallocatePushTransportWithParams_completion :: (IsMTRBaseClusterPushAVStreamTransport mtrBaseClusterPushAVStreamTransport, IsMTRPushAVStreamTransportClusterDeallocatePushTransportParams params) => mtrBaseClusterPushAVStreamTransport -> params -> Ptr () -> IO ()
deallocatePushTransportWithParams_completion mtrBaseClusterPushAVStreamTransport params completion =
  sendMessage mtrBaseClusterPushAVStreamTransport deallocatePushTransportWithParams_completionSelector (toMTRPushAVStreamTransportClusterDeallocatePushTransportParams params) completion

-- | Command ModifyPushTransport
--
-- This command is used to request the Node modifies the configuration of the specified push transport.
--
-- ObjC selector: @- modifyPushTransportWithParams:completion:@
modifyPushTransportWithParams_completion :: (IsMTRBaseClusterPushAVStreamTransport mtrBaseClusterPushAVStreamTransport, IsMTRPushAVStreamTransportClusterModifyPushTransportParams params) => mtrBaseClusterPushAVStreamTransport -> params -> Ptr () -> IO ()
modifyPushTransportWithParams_completion mtrBaseClusterPushAVStreamTransport params completion =
  sendMessage mtrBaseClusterPushAVStreamTransport modifyPushTransportWithParams_completionSelector (toMTRPushAVStreamTransportClusterModifyPushTransportParams params) completion

-- | Command SetTransportStatus
--
-- This command SHALL be generated to request the Node modifies the Transport Status of a specified transport or all transports.
--
-- ObjC selector: @- setTransportStatusWithParams:completion:@
setTransportStatusWithParams_completion :: (IsMTRBaseClusterPushAVStreamTransport mtrBaseClusterPushAVStreamTransport, IsMTRPushAVStreamTransportClusterSetTransportStatusParams params) => mtrBaseClusterPushAVStreamTransport -> params -> Ptr () -> IO ()
setTransportStatusWithParams_completion mtrBaseClusterPushAVStreamTransport params completion =
  sendMessage mtrBaseClusterPushAVStreamTransport setTransportStatusWithParams_completionSelector (toMTRPushAVStreamTransportClusterSetTransportStatusParams params) completion

-- | Command ManuallyTriggerTransport
--
-- This command SHALL be generated to request the Node to manually start the specified push transport.
--
-- ObjC selector: @- manuallyTriggerTransportWithParams:completion:@
manuallyTriggerTransportWithParams_completion :: (IsMTRBaseClusterPushAVStreamTransport mtrBaseClusterPushAVStreamTransport, IsMTRPushAVStreamTransportClusterManuallyTriggerTransportParams params) => mtrBaseClusterPushAVStreamTransport -> params -> Ptr () -> IO ()
manuallyTriggerTransportWithParams_completion mtrBaseClusterPushAVStreamTransport params completion =
  sendMessage mtrBaseClusterPushAVStreamTransport manuallyTriggerTransportWithParams_completionSelector (toMTRPushAVStreamTransportClusterManuallyTriggerTransportParams params) completion

-- | Command FindTransport
--
-- This command SHALL return the Transport Configuration for the specified push transport or all allocated transports for the fabric if null.
--
-- ObjC selector: @- findTransportWithParams:completion:@
findTransportWithParams_completion :: (IsMTRBaseClusterPushAVStreamTransport mtrBaseClusterPushAVStreamTransport, IsMTRPushAVStreamTransportClusterFindTransportParams params) => mtrBaseClusterPushAVStreamTransport -> params -> Ptr () -> IO ()
findTransportWithParams_completion mtrBaseClusterPushAVStreamTransport params completion =
  sendMessage mtrBaseClusterPushAVStreamTransport findTransportWithParams_completionSelector (toMTRPushAVStreamTransportClusterFindTransportParams params) completion

-- | @- readAttributeSupportedFormatsWithCompletion:@
readAttributeSupportedFormatsWithCompletion :: IsMTRBaseClusterPushAVStreamTransport mtrBaseClusterPushAVStreamTransport => mtrBaseClusterPushAVStreamTransport -> Ptr () -> IO ()
readAttributeSupportedFormatsWithCompletion mtrBaseClusterPushAVStreamTransport completion =
  sendMessage mtrBaseClusterPushAVStreamTransport readAttributeSupportedFormatsWithCompletionSelector completion

-- | @- subscribeAttributeSupportedFormatsWithParams:subscriptionEstablished:reportHandler:@
subscribeAttributeSupportedFormatsWithParams_subscriptionEstablished_reportHandler :: (IsMTRBaseClusterPushAVStreamTransport mtrBaseClusterPushAVStreamTransport, IsMTRSubscribeParams params) => mtrBaseClusterPushAVStreamTransport -> params -> Ptr () -> Ptr () -> IO ()
subscribeAttributeSupportedFormatsWithParams_subscriptionEstablished_reportHandler mtrBaseClusterPushAVStreamTransport params subscriptionEstablished reportHandler =
  sendMessage mtrBaseClusterPushAVStreamTransport subscribeAttributeSupportedFormatsWithParams_subscriptionEstablished_reportHandlerSelector (toMTRSubscribeParams params) subscriptionEstablished reportHandler

-- | @+ readAttributeSupportedFormatsWithClusterStateCache:endpoint:queue:completion:@
readAttributeSupportedFormatsWithClusterStateCache_endpoint_queue_completion :: (IsMTRClusterStateCacheContainer clusterStateCacheContainer, IsNSNumber endpoint, IsNSObject queue) => clusterStateCacheContainer -> endpoint -> queue -> Ptr () -> IO ()
readAttributeSupportedFormatsWithClusterStateCache_endpoint_queue_completion clusterStateCacheContainer endpoint queue completion =
  do
    cls' <- getRequiredClass "MTRBaseClusterPushAVStreamTransport"
    sendClassMessage cls' readAttributeSupportedFormatsWithClusterStateCache_endpoint_queue_completionSelector (toMTRClusterStateCacheContainer clusterStateCacheContainer) (toNSNumber endpoint) (toNSObject queue) completion

-- | @- readAttributeCurrentConnectionsWithParams:completion:@
readAttributeCurrentConnectionsWithParams_completion :: (IsMTRBaseClusterPushAVStreamTransport mtrBaseClusterPushAVStreamTransport, IsMTRReadParams params) => mtrBaseClusterPushAVStreamTransport -> params -> Ptr () -> IO ()
readAttributeCurrentConnectionsWithParams_completion mtrBaseClusterPushAVStreamTransport params completion =
  sendMessage mtrBaseClusterPushAVStreamTransport readAttributeCurrentConnectionsWithParams_completionSelector (toMTRReadParams params) completion

-- | @- subscribeAttributeCurrentConnectionsWithParams:subscriptionEstablished:reportHandler:@
subscribeAttributeCurrentConnectionsWithParams_subscriptionEstablished_reportHandler :: (IsMTRBaseClusterPushAVStreamTransport mtrBaseClusterPushAVStreamTransport, IsMTRSubscribeParams params) => mtrBaseClusterPushAVStreamTransport -> params -> Ptr () -> Ptr () -> IO ()
subscribeAttributeCurrentConnectionsWithParams_subscriptionEstablished_reportHandler mtrBaseClusterPushAVStreamTransport params subscriptionEstablished reportHandler =
  sendMessage mtrBaseClusterPushAVStreamTransport subscribeAttributeCurrentConnectionsWithParams_subscriptionEstablished_reportHandlerSelector (toMTRSubscribeParams params) subscriptionEstablished reportHandler

-- | @+ readAttributeCurrentConnectionsWithClusterStateCache:endpoint:queue:completion:@
readAttributeCurrentConnectionsWithClusterStateCache_endpoint_queue_completion :: (IsMTRClusterStateCacheContainer clusterStateCacheContainer, IsNSNumber endpoint, IsNSObject queue) => clusterStateCacheContainer -> endpoint -> queue -> Ptr () -> IO ()
readAttributeCurrentConnectionsWithClusterStateCache_endpoint_queue_completion clusterStateCacheContainer endpoint queue completion =
  do
    cls' <- getRequiredClass "MTRBaseClusterPushAVStreamTransport"
    sendClassMessage cls' readAttributeCurrentConnectionsWithClusterStateCache_endpoint_queue_completionSelector (toMTRClusterStateCacheContainer clusterStateCacheContainer) (toNSNumber endpoint) (toNSObject queue) completion

-- | @- readAttributeGeneratedCommandListWithCompletion:@
readAttributeGeneratedCommandListWithCompletion :: IsMTRBaseClusterPushAVStreamTransport mtrBaseClusterPushAVStreamTransport => mtrBaseClusterPushAVStreamTransport -> Ptr () -> IO ()
readAttributeGeneratedCommandListWithCompletion mtrBaseClusterPushAVStreamTransport completion =
  sendMessage mtrBaseClusterPushAVStreamTransport readAttributeGeneratedCommandListWithCompletionSelector completion

-- | @- subscribeAttributeGeneratedCommandListWithParams:subscriptionEstablished:reportHandler:@
subscribeAttributeGeneratedCommandListWithParams_subscriptionEstablished_reportHandler :: (IsMTRBaseClusterPushAVStreamTransport mtrBaseClusterPushAVStreamTransport, IsMTRSubscribeParams params) => mtrBaseClusterPushAVStreamTransport -> params -> Ptr () -> Ptr () -> IO ()
subscribeAttributeGeneratedCommandListWithParams_subscriptionEstablished_reportHandler mtrBaseClusterPushAVStreamTransport params subscriptionEstablished reportHandler =
  sendMessage mtrBaseClusterPushAVStreamTransport subscribeAttributeGeneratedCommandListWithParams_subscriptionEstablished_reportHandlerSelector (toMTRSubscribeParams params) subscriptionEstablished reportHandler

-- | @+ readAttributeGeneratedCommandListWithClusterStateCache:endpoint:queue:completion:@
readAttributeGeneratedCommandListWithClusterStateCache_endpoint_queue_completion :: (IsMTRClusterStateCacheContainer clusterStateCacheContainer, IsNSNumber endpoint, IsNSObject queue) => clusterStateCacheContainer -> endpoint -> queue -> Ptr () -> IO ()
readAttributeGeneratedCommandListWithClusterStateCache_endpoint_queue_completion clusterStateCacheContainer endpoint queue completion =
  do
    cls' <- getRequiredClass "MTRBaseClusterPushAVStreamTransport"
    sendClassMessage cls' readAttributeGeneratedCommandListWithClusterStateCache_endpoint_queue_completionSelector (toMTRClusterStateCacheContainer clusterStateCacheContainer) (toNSNumber endpoint) (toNSObject queue) completion

-- | @- readAttributeAcceptedCommandListWithCompletion:@
readAttributeAcceptedCommandListWithCompletion :: IsMTRBaseClusterPushAVStreamTransport mtrBaseClusterPushAVStreamTransport => mtrBaseClusterPushAVStreamTransport -> Ptr () -> IO ()
readAttributeAcceptedCommandListWithCompletion mtrBaseClusterPushAVStreamTransport completion =
  sendMessage mtrBaseClusterPushAVStreamTransport readAttributeAcceptedCommandListWithCompletionSelector completion

-- | @- subscribeAttributeAcceptedCommandListWithParams:subscriptionEstablished:reportHandler:@
subscribeAttributeAcceptedCommandListWithParams_subscriptionEstablished_reportHandler :: (IsMTRBaseClusterPushAVStreamTransport mtrBaseClusterPushAVStreamTransport, IsMTRSubscribeParams params) => mtrBaseClusterPushAVStreamTransport -> params -> Ptr () -> Ptr () -> IO ()
subscribeAttributeAcceptedCommandListWithParams_subscriptionEstablished_reportHandler mtrBaseClusterPushAVStreamTransport params subscriptionEstablished reportHandler =
  sendMessage mtrBaseClusterPushAVStreamTransport subscribeAttributeAcceptedCommandListWithParams_subscriptionEstablished_reportHandlerSelector (toMTRSubscribeParams params) subscriptionEstablished reportHandler

-- | @+ readAttributeAcceptedCommandListWithClusterStateCache:endpoint:queue:completion:@
readAttributeAcceptedCommandListWithClusterStateCache_endpoint_queue_completion :: (IsMTRClusterStateCacheContainer clusterStateCacheContainer, IsNSNumber endpoint, IsNSObject queue) => clusterStateCacheContainer -> endpoint -> queue -> Ptr () -> IO ()
readAttributeAcceptedCommandListWithClusterStateCache_endpoint_queue_completion clusterStateCacheContainer endpoint queue completion =
  do
    cls' <- getRequiredClass "MTRBaseClusterPushAVStreamTransport"
    sendClassMessage cls' readAttributeAcceptedCommandListWithClusterStateCache_endpoint_queue_completionSelector (toMTRClusterStateCacheContainer clusterStateCacheContainer) (toNSNumber endpoint) (toNSObject queue) completion

-- | @- readAttributeAttributeListWithCompletion:@
readAttributeAttributeListWithCompletion :: IsMTRBaseClusterPushAVStreamTransport mtrBaseClusterPushAVStreamTransport => mtrBaseClusterPushAVStreamTransport -> Ptr () -> IO ()
readAttributeAttributeListWithCompletion mtrBaseClusterPushAVStreamTransport completion =
  sendMessage mtrBaseClusterPushAVStreamTransport readAttributeAttributeListWithCompletionSelector completion

-- | @- subscribeAttributeAttributeListWithParams:subscriptionEstablished:reportHandler:@
subscribeAttributeAttributeListWithParams_subscriptionEstablished_reportHandler :: (IsMTRBaseClusterPushAVStreamTransport mtrBaseClusterPushAVStreamTransport, IsMTRSubscribeParams params) => mtrBaseClusterPushAVStreamTransport -> params -> Ptr () -> Ptr () -> IO ()
subscribeAttributeAttributeListWithParams_subscriptionEstablished_reportHandler mtrBaseClusterPushAVStreamTransport params subscriptionEstablished reportHandler =
  sendMessage mtrBaseClusterPushAVStreamTransport subscribeAttributeAttributeListWithParams_subscriptionEstablished_reportHandlerSelector (toMTRSubscribeParams params) subscriptionEstablished reportHandler

-- | @+ readAttributeAttributeListWithClusterStateCache:endpoint:queue:completion:@
readAttributeAttributeListWithClusterStateCache_endpoint_queue_completion :: (IsMTRClusterStateCacheContainer clusterStateCacheContainer, IsNSNumber endpoint, IsNSObject queue) => clusterStateCacheContainer -> endpoint -> queue -> Ptr () -> IO ()
readAttributeAttributeListWithClusterStateCache_endpoint_queue_completion clusterStateCacheContainer endpoint queue completion =
  do
    cls' <- getRequiredClass "MTRBaseClusterPushAVStreamTransport"
    sendClassMessage cls' readAttributeAttributeListWithClusterStateCache_endpoint_queue_completionSelector (toMTRClusterStateCacheContainer clusterStateCacheContainer) (toNSNumber endpoint) (toNSObject queue) completion

-- | @- readAttributeFeatureMapWithCompletion:@
readAttributeFeatureMapWithCompletion :: IsMTRBaseClusterPushAVStreamTransport mtrBaseClusterPushAVStreamTransport => mtrBaseClusterPushAVStreamTransport -> Ptr () -> IO ()
readAttributeFeatureMapWithCompletion mtrBaseClusterPushAVStreamTransport completion =
  sendMessage mtrBaseClusterPushAVStreamTransport readAttributeFeatureMapWithCompletionSelector completion

-- | @- subscribeAttributeFeatureMapWithParams:subscriptionEstablished:reportHandler:@
subscribeAttributeFeatureMapWithParams_subscriptionEstablished_reportHandler :: (IsMTRBaseClusterPushAVStreamTransport mtrBaseClusterPushAVStreamTransport, IsMTRSubscribeParams params) => mtrBaseClusterPushAVStreamTransport -> params -> Ptr () -> Ptr () -> IO ()
subscribeAttributeFeatureMapWithParams_subscriptionEstablished_reportHandler mtrBaseClusterPushAVStreamTransport params subscriptionEstablished reportHandler =
  sendMessage mtrBaseClusterPushAVStreamTransport subscribeAttributeFeatureMapWithParams_subscriptionEstablished_reportHandlerSelector (toMTRSubscribeParams params) subscriptionEstablished reportHandler

-- | @+ readAttributeFeatureMapWithClusterStateCache:endpoint:queue:completion:@
readAttributeFeatureMapWithClusterStateCache_endpoint_queue_completion :: (IsMTRClusterStateCacheContainer clusterStateCacheContainer, IsNSNumber endpoint, IsNSObject queue) => clusterStateCacheContainer -> endpoint -> queue -> Ptr () -> IO ()
readAttributeFeatureMapWithClusterStateCache_endpoint_queue_completion clusterStateCacheContainer endpoint queue completion =
  do
    cls' <- getRequiredClass "MTRBaseClusterPushAVStreamTransport"
    sendClassMessage cls' readAttributeFeatureMapWithClusterStateCache_endpoint_queue_completionSelector (toMTRClusterStateCacheContainer clusterStateCacheContainer) (toNSNumber endpoint) (toNSObject queue) completion

-- | @- readAttributeClusterRevisionWithCompletion:@
readAttributeClusterRevisionWithCompletion :: IsMTRBaseClusterPushAVStreamTransport mtrBaseClusterPushAVStreamTransport => mtrBaseClusterPushAVStreamTransport -> Ptr () -> IO ()
readAttributeClusterRevisionWithCompletion mtrBaseClusterPushAVStreamTransport completion =
  sendMessage mtrBaseClusterPushAVStreamTransport readAttributeClusterRevisionWithCompletionSelector completion

-- | @- subscribeAttributeClusterRevisionWithParams:subscriptionEstablished:reportHandler:@
subscribeAttributeClusterRevisionWithParams_subscriptionEstablished_reportHandler :: (IsMTRBaseClusterPushAVStreamTransport mtrBaseClusterPushAVStreamTransport, IsMTRSubscribeParams params) => mtrBaseClusterPushAVStreamTransport -> params -> Ptr () -> Ptr () -> IO ()
subscribeAttributeClusterRevisionWithParams_subscriptionEstablished_reportHandler mtrBaseClusterPushAVStreamTransport params subscriptionEstablished reportHandler =
  sendMessage mtrBaseClusterPushAVStreamTransport subscribeAttributeClusterRevisionWithParams_subscriptionEstablished_reportHandlerSelector (toMTRSubscribeParams params) subscriptionEstablished reportHandler

-- | @+ readAttributeClusterRevisionWithClusterStateCache:endpoint:queue:completion:@
readAttributeClusterRevisionWithClusterStateCache_endpoint_queue_completion :: (IsMTRClusterStateCacheContainer clusterStateCacheContainer, IsNSNumber endpoint, IsNSObject queue) => clusterStateCacheContainer -> endpoint -> queue -> Ptr () -> IO ()
readAttributeClusterRevisionWithClusterStateCache_endpoint_queue_completion clusterStateCacheContainer endpoint queue completion =
  do
    cls' <- getRequiredClass "MTRBaseClusterPushAVStreamTransport"
    sendClassMessage cls' readAttributeClusterRevisionWithClusterStateCache_endpoint_queue_completionSelector (toMTRClusterStateCacheContainer clusterStateCacheContainer) (toNSNumber endpoint) (toNSObject queue) completion

-- | @- init@
init_ :: IsMTRBaseClusterPushAVStreamTransport mtrBaseClusterPushAVStreamTransport => mtrBaseClusterPushAVStreamTransport -> IO (Id MTRBaseClusterPushAVStreamTransport)
init_ mtrBaseClusterPushAVStreamTransport =
  sendOwnedMessage mtrBaseClusterPushAVStreamTransport initSelector

-- | @+ new@
new :: IO (Id MTRBaseClusterPushAVStreamTransport)
new  =
  do
    cls' <- getRequiredClass "MTRBaseClusterPushAVStreamTransport"
    sendOwnedClassMessage cls' newSelector

-- | For all instance methods (reads, writes, commands) that take a completion, the completion will be called on the provided queue.
--
-- ObjC selector: @- initWithDevice:endpointID:queue:@
initWithDevice_endpointID_queue :: (IsMTRBaseClusterPushAVStreamTransport mtrBaseClusterPushAVStreamTransport, IsMTRBaseDevice device, IsNSNumber endpointID, IsNSObject queue) => mtrBaseClusterPushAVStreamTransport -> device -> endpointID -> queue -> IO (Id MTRBaseClusterPushAVStreamTransport)
initWithDevice_endpointID_queue mtrBaseClusterPushAVStreamTransport device endpointID queue =
  sendOwnedMessage mtrBaseClusterPushAVStreamTransport initWithDevice_endpointID_queueSelector (toMTRBaseDevice device) (toNSNumber endpointID) (toNSObject queue)

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @allocatePushTransportWithParams:completion:@
allocatePushTransportWithParams_completionSelector :: Selector '[Id MTRPushAVStreamTransportClusterAllocatePushTransportParams, Ptr ()] ()
allocatePushTransportWithParams_completionSelector = mkSelector "allocatePushTransportWithParams:completion:"

-- | @Selector@ for @deallocatePushTransportWithParams:completion:@
deallocatePushTransportWithParams_completionSelector :: Selector '[Id MTRPushAVStreamTransportClusterDeallocatePushTransportParams, Ptr ()] ()
deallocatePushTransportWithParams_completionSelector = mkSelector "deallocatePushTransportWithParams:completion:"

-- | @Selector@ for @modifyPushTransportWithParams:completion:@
modifyPushTransportWithParams_completionSelector :: Selector '[Id MTRPushAVStreamTransportClusterModifyPushTransportParams, Ptr ()] ()
modifyPushTransportWithParams_completionSelector = mkSelector "modifyPushTransportWithParams:completion:"

-- | @Selector@ for @setTransportStatusWithParams:completion:@
setTransportStatusWithParams_completionSelector :: Selector '[Id MTRPushAVStreamTransportClusterSetTransportStatusParams, Ptr ()] ()
setTransportStatusWithParams_completionSelector = mkSelector "setTransportStatusWithParams:completion:"

-- | @Selector@ for @manuallyTriggerTransportWithParams:completion:@
manuallyTriggerTransportWithParams_completionSelector :: Selector '[Id MTRPushAVStreamTransportClusterManuallyTriggerTransportParams, Ptr ()] ()
manuallyTriggerTransportWithParams_completionSelector = mkSelector "manuallyTriggerTransportWithParams:completion:"

-- | @Selector@ for @findTransportWithParams:completion:@
findTransportWithParams_completionSelector :: Selector '[Id MTRPushAVStreamTransportClusterFindTransportParams, Ptr ()] ()
findTransportWithParams_completionSelector = mkSelector "findTransportWithParams:completion:"

-- | @Selector@ for @readAttributeSupportedFormatsWithCompletion:@
readAttributeSupportedFormatsWithCompletionSelector :: Selector '[Ptr ()] ()
readAttributeSupportedFormatsWithCompletionSelector = mkSelector "readAttributeSupportedFormatsWithCompletion:"

-- | @Selector@ for @subscribeAttributeSupportedFormatsWithParams:subscriptionEstablished:reportHandler:@
subscribeAttributeSupportedFormatsWithParams_subscriptionEstablished_reportHandlerSelector :: Selector '[Id MTRSubscribeParams, Ptr (), Ptr ()] ()
subscribeAttributeSupportedFormatsWithParams_subscriptionEstablished_reportHandlerSelector = mkSelector "subscribeAttributeSupportedFormatsWithParams:subscriptionEstablished:reportHandler:"

-- | @Selector@ for @readAttributeSupportedFormatsWithClusterStateCache:endpoint:queue:completion:@
readAttributeSupportedFormatsWithClusterStateCache_endpoint_queue_completionSelector :: Selector '[Id MTRClusterStateCacheContainer, Id NSNumber, Id NSObject, Ptr ()] ()
readAttributeSupportedFormatsWithClusterStateCache_endpoint_queue_completionSelector = mkSelector "readAttributeSupportedFormatsWithClusterStateCache:endpoint:queue:completion:"

-- | @Selector@ for @readAttributeCurrentConnectionsWithParams:completion:@
readAttributeCurrentConnectionsWithParams_completionSelector :: Selector '[Id MTRReadParams, Ptr ()] ()
readAttributeCurrentConnectionsWithParams_completionSelector = mkSelector "readAttributeCurrentConnectionsWithParams:completion:"

-- | @Selector@ for @subscribeAttributeCurrentConnectionsWithParams:subscriptionEstablished:reportHandler:@
subscribeAttributeCurrentConnectionsWithParams_subscriptionEstablished_reportHandlerSelector :: Selector '[Id MTRSubscribeParams, Ptr (), Ptr ()] ()
subscribeAttributeCurrentConnectionsWithParams_subscriptionEstablished_reportHandlerSelector = mkSelector "subscribeAttributeCurrentConnectionsWithParams:subscriptionEstablished:reportHandler:"

-- | @Selector@ for @readAttributeCurrentConnectionsWithClusterStateCache:endpoint:queue:completion:@
readAttributeCurrentConnectionsWithClusterStateCache_endpoint_queue_completionSelector :: Selector '[Id MTRClusterStateCacheContainer, Id NSNumber, Id NSObject, Ptr ()] ()
readAttributeCurrentConnectionsWithClusterStateCache_endpoint_queue_completionSelector = mkSelector "readAttributeCurrentConnectionsWithClusterStateCache:endpoint:queue:completion:"

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
initSelector :: Selector '[] (Id MTRBaseClusterPushAVStreamTransport)
initSelector = mkSelector "init"

-- | @Selector@ for @new@
newSelector :: Selector '[] (Id MTRBaseClusterPushAVStreamTransport)
newSelector = mkSelector "new"

-- | @Selector@ for @initWithDevice:endpointID:queue:@
initWithDevice_endpointID_queueSelector :: Selector '[Id MTRBaseDevice, Id NSNumber, Id NSObject] (Id MTRBaseClusterPushAVStreamTransport)
initWithDevice_endpointID_queueSelector = mkSelector "initWithDevice:endpointID:queue:"

