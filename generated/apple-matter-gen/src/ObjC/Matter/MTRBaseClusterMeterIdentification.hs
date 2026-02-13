{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Cluster Meter Identification
--
-- This Meter Identification Cluster provides attributes for determining advanced information about utility metering device.
--
-- Generated bindings for @MTRBaseClusterMeterIdentification@.
module ObjC.Matter.MTRBaseClusterMeterIdentification
  ( MTRBaseClusterMeterIdentification
  , IsMTRBaseClusterMeterIdentification(..)
  , readAttributeMeterTypeWithCompletion
  , subscribeAttributeMeterTypeWithParams_subscriptionEstablished_reportHandler
  , readAttributeMeterTypeWithClusterStateCache_endpoint_queue_completion
  , readAttributePointOfDeliveryWithCompletion
  , subscribeAttributePointOfDeliveryWithParams_subscriptionEstablished_reportHandler
  , readAttributePointOfDeliveryWithClusterStateCache_endpoint_queue_completion
  , readAttributeMeterSerialNumberWithCompletion
  , subscribeAttributeMeterSerialNumberWithParams_subscriptionEstablished_reportHandler
  , readAttributeMeterSerialNumberWithClusterStateCache_endpoint_queue_completion
  , readAttributeProtocolVersionWithCompletion
  , subscribeAttributeProtocolVersionWithParams_subscriptionEstablished_reportHandler
  , readAttributeProtocolVersionWithClusterStateCache_endpoint_queue_completion
  , readAttributePowerThresholdWithCompletion
  , subscribeAttributePowerThresholdWithParams_subscriptionEstablished_reportHandler
  , readAttributePowerThresholdWithClusterStateCache_endpoint_queue_completion
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
  , readAttributeMeterSerialNumberWithClusterStateCache_endpoint_queue_completionSelector
  , readAttributeMeterSerialNumberWithCompletionSelector
  , readAttributeMeterTypeWithClusterStateCache_endpoint_queue_completionSelector
  , readAttributeMeterTypeWithCompletionSelector
  , readAttributePointOfDeliveryWithClusterStateCache_endpoint_queue_completionSelector
  , readAttributePointOfDeliveryWithCompletionSelector
  , readAttributePowerThresholdWithClusterStateCache_endpoint_queue_completionSelector
  , readAttributePowerThresholdWithCompletionSelector
  , readAttributeProtocolVersionWithClusterStateCache_endpoint_queue_completionSelector
  , readAttributeProtocolVersionWithCompletionSelector
  , subscribeAttributeAcceptedCommandListWithParams_subscriptionEstablished_reportHandlerSelector
  , subscribeAttributeAttributeListWithParams_subscriptionEstablished_reportHandlerSelector
  , subscribeAttributeClusterRevisionWithParams_subscriptionEstablished_reportHandlerSelector
  , subscribeAttributeFeatureMapWithParams_subscriptionEstablished_reportHandlerSelector
  , subscribeAttributeGeneratedCommandListWithParams_subscriptionEstablished_reportHandlerSelector
  , subscribeAttributeMeterSerialNumberWithParams_subscriptionEstablished_reportHandlerSelector
  , subscribeAttributeMeterTypeWithParams_subscriptionEstablished_reportHandlerSelector
  , subscribeAttributePointOfDeliveryWithParams_subscriptionEstablished_reportHandlerSelector
  , subscribeAttributePowerThresholdWithParams_subscriptionEstablished_reportHandlerSelector
  , subscribeAttributeProtocolVersionWithParams_subscriptionEstablished_reportHandlerSelector


  ) where

import Foreign.Ptr (Ptr, FunPtr)
import Foreign.C.Types

import ObjC.Runtime.Types
import ObjC.Runtime.Message (sendMessage, sendOwnedMessage, sendClassMessage, sendOwnedClassMessage)
import ObjC.Runtime.Selector (mkSelector)
import ObjC.Runtime.Class (getRequiredClass)

import ObjC.Matter.Internal.Classes
import ObjC.Foundation.Internal.Classes

-- | @- readAttributeMeterTypeWithCompletion:@
readAttributeMeterTypeWithCompletion :: IsMTRBaseClusterMeterIdentification mtrBaseClusterMeterIdentification => mtrBaseClusterMeterIdentification -> Ptr () -> IO ()
readAttributeMeterTypeWithCompletion mtrBaseClusterMeterIdentification completion =
  sendMessage mtrBaseClusterMeterIdentification readAttributeMeterTypeWithCompletionSelector completion

-- | @- subscribeAttributeMeterTypeWithParams:subscriptionEstablished:reportHandler:@
subscribeAttributeMeterTypeWithParams_subscriptionEstablished_reportHandler :: (IsMTRBaseClusterMeterIdentification mtrBaseClusterMeterIdentification, IsMTRSubscribeParams params) => mtrBaseClusterMeterIdentification -> params -> Ptr () -> Ptr () -> IO ()
subscribeAttributeMeterTypeWithParams_subscriptionEstablished_reportHandler mtrBaseClusterMeterIdentification params subscriptionEstablished reportHandler =
  sendMessage mtrBaseClusterMeterIdentification subscribeAttributeMeterTypeWithParams_subscriptionEstablished_reportHandlerSelector (toMTRSubscribeParams params) subscriptionEstablished reportHandler

-- | @+ readAttributeMeterTypeWithClusterStateCache:endpoint:queue:completion:@
readAttributeMeterTypeWithClusterStateCache_endpoint_queue_completion :: (IsMTRClusterStateCacheContainer clusterStateCacheContainer, IsNSNumber endpoint, IsNSObject queue) => clusterStateCacheContainer -> endpoint -> queue -> Ptr () -> IO ()
readAttributeMeterTypeWithClusterStateCache_endpoint_queue_completion clusterStateCacheContainer endpoint queue completion =
  do
    cls' <- getRequiredClass "MTRBaseClusterMeterIdentification"
    sendClassMessage cls' readAttributeMeterTypeWithClusterStateCache_endpoint_queue_completionSelector (toMTRClusterStateCacheContainer clusterStateCacheContainer) (toNSNumber endpoint) (toNSObject queue) completion

-- | @- readAttributePointOfDeliveryWithCompletion:@
readAttributePointOfDeliveryWithCompletion :: IsMTRBaseClusterMeterIdentification mtrBaseClusterMeterIdentification => mtrBaseClusterMeterIdentification -> Ptr () -> IO ()
readAttributePointOfDeliveryWithCompletion mtrBaseClusterMeterIdentification completion =
  sendMessage mtrBaseClusterMeterIdentification readAttributePointOfDeliveryWithCompletionSelector completion

-- | @- subscribeAttributePointOfDeliveryWithParams:subscriptionEstablished:reportHandler:@
subscribeAttributePointOfDeliveryWithParams_subscriptionEstablished_reportHandler :: (IsMTRBaseClusterMeterIdentification mtrBaseClusterMeterIdentification, IsMTRSubscribeParams params) => mtrBaseClusterMeterIdentification -> params -> Ptr () -> Ptr () -> IO ()
subscribeAttributePointOfDeliveryWithParams_subscriptionEstablished_reportHandler mtrBaseClusterMeterIdentification params subscriptionEstablished reportHandler =
  sendMessage mtrBaseClusterMeterIdentification subscribeAttributePointOfDeliveryWithParams_subscriptionEstablished_reportHandlerSelector (toMTRSubscribeParams params) subscriptionEstablished reportHandler

-- | @+ readAttributePointOfDeliveryWithClusterStateCache:endpoint:queue:completion:@
readAttributePointOfDeliveryWithClusterStateCache_endpoint_queue_completion :: (IsMTRClusterStateCacheContainer clusterStateCacheContainer, IsNSNumber endpoint, IsNSObject queue) => clusterStateCacheContainer -> endpoint -> queue -> Ptr () -> IO ()
readAttributePointOfDeliveryWithClusterStateCache_endpoint_queue_completion clusterStateCacheContainer endpoint queue completion =
  do
    cls' <- getRequiredClass "MTRBaseClusterMeterIdentification"
    sendClassMessage cls' readAttributePointOfDeliveryWithClusterStateCache_endpoint_queue_completionSelector (toMTRClusterStateCacheContainer clusterStateCacheContainer) (toNSNumber endpoint) (toNSObject queue) completion

-- | @- readAttributeMeterSerialNumberWithCompletion:@
readAttributeMeterSerialNumberWithCompletion :: IsMTRBaseClusterMeterIdentification mtrBaseClusterMeterIdentification => mtrBaseClusterMeterIdentification -> Ptr () -> IO ()
readAttributeMeterSerialNumberWithCompletion mtrBaseClusterMeterIdentification completion =
  sendMessage mtrBaseClusterMeterIdentification readAttributeMeterSerialNumberWithCompletionSelector completion

-- | @- subscribeAttributeMeterSerialNumberWithParams:subscriptionEstablished:reportHandler:@
subscribeAttributeMeterSerialNumberWithParams_subscriptionEstablished_reportHandler :: (IsMTRBaseClusterMeterIdentification mtrBaseClusterMeterIdentification, IsMTRSubscribeParams params) => mtrBaseClusterMeterIdentification -> params -> Ptr () -> Ptr () -> IO ()
subscribeAttributeMeterSerialNumberWithParams_subscriptionEstablished_reportHandler mtrBaseClusterMeterIdentification params subscriptionEstablished reportHandler =
  sendMessage mtrBaseClusterMeterIdentification subscribeAttributeMeterSerialNumberWithParams_subscriptionEstablished_reportHandlerSelector (toMTRSubscribeParams params) subscriptionEstablished reportHandler

-- | @+ readAttributeMeterSerialNumberWithClusterStateCache:endpoint:queue:completion:@
readAttributeMeterSerialNumberWithClusterStateCache_endpoint_queue_completion :: (IsMTRClusterStateCacheContainer clusterStateCacheContainer, IsNSNumber endpoint, IsNSObject queue) => clusterStateCacheContainer -> endpoint -> queue -> Ptr () -> IO ()
readAttributeMeterSerialNumberWithClusterStateCache_endpoint_queue_completion clusterStateCacheContainer endpoint queue completion =
  do
    cls' <- getRequiredClass "MTRBaseClusterMeterIdentification"
    sendClassMessage cls' readAttributeMeterSerialNumberWithClusterStateCache_endpoint_queue_completionSelector (toMTRClusterStateCacheContainer clusterStateCacheContainer) (toNSNumber endpoint) (toNSObject queue) completion

-- | @- readAttributeProtocolVersionWithCompletion:@
readAttributeProtocolVersionWithCompletion :: IsMTRBaseClusterMeterIdentification mtrBaseClusterMeterIdentification => mtrBaseClusterMeterIdentification -> Ptr () -> IO ()
readAttributeProtocolVersionWithCompletion mtrBaseClusterMeterIdentification completion =
  sendMessage mtrBaseClusterMeterIdentification readAttributeProtocolVersionWithCompletionSelector completion

-- | @- subscribeAttributeProtocolVersionWithParams:subscriptionEstablished:reportHandler:@
subscribeAttributeProtocolVersionWithParams_subscriptionEstablished_reportHandler :: (IsMTRBaseClusterMeterIdentification mtrBaseClusterMeterIdentification, IsMTRSubscribeParams params) => mtrBaseClusterMeterIdentification -> params -> Ptr () -> Ptr () -> IO ()
subscribeAttributeProtocolVersionWithParams_subscriptionEstablished_reportHandler mtrBaseClusterMeterIdentification params subscriptionEstablished reportHandler =
  sendMessage mtrBaseClusterMeterIdentification subscribeAttributeProtocolVersionWithParams_subscriptionEstablished_reportHandlerSelector (toMTRSubscribeParams params) subscriptionEstablished reportHandler

-- | @+ readAttributeProtocolVersionWithClusterStateCache:endpoint:queue:completion:@
readAttributeProtocolVersionWithClusterStateCache_endpoint_queue_completion :: (IsMTRClusterStateCacheContainer clusterStateCacheContainer, IsNSNumber endpoint, IsNSObject queue) => clusterStateCacheContainer -> endpoint -> queue -> Ptr () -> IO ()
readAttributeProtocolVersionWithClusterStateCache_endpoint_queue_completion clusterStateCacheContainer endpoint queue completion =
  do
    cls' <- getRequiredClass "MTRBaseClusterMeterIdentification"
    sendClassMessage cls' readAttributeProtocolVersionWithClusterStateCache_endpoint_queue_completionSelector (toMTRClusterStateCacheContainer clusterStateCacheContainer) (toNSNumber endpoint) (toNSObject queue) completion

-- | @- readAttributePowerThresholdWithCompletion:@
readAttributePowerThresholdWithCompletion :: IsMTRBaseClusterMeterIdentification mtrBaseClusterMeterIdentification => mtrBaseClusterMeterIdentification -> Ptr () -> IO ()
readAttributePowerThresholdWithCompletion mtrBaseClusterMeterIdentification completion =
  sendMessage mtrBaseClusterMeterIdentification readAttributePowerThresholdWithCompletionSelector completion

-- | @- subscribeAttributePowerThresholdWithParams:subscriptionEstablished:reportHandler:@
subscribeAttributePowerThresholdWithParams_subscriptionEstablished_reportHandler :: (IsMTRBaseClusterMeterIdentification mtrBaseClusterMeterIdentification, IsMTRSubscribeParams params) => mtrBaseClusterMeterIdentification -> params -> Ptr () -> Ptr () -> IO ()
subscribeAttributePowerThresholdWithParams_subscriptionEstablished_reportHandler mtrBaseClusterMeterIdentification params subscriptionEstablished reportHandler =
  sendMessage mtrBaseClusterMeterIdentification subscribeAttributePowerThresholdWithParams_subscriptionEstablished_reportHandlerSelector (toMTRSubscribeParams params) subscriptionEstablished reportHandler

-- | @+ readAttributePowerThresholdWithClusterStateCache:endpoint:queue:completion:@
readAttributePowerThresholdWithClusterStateCache_endpoint_queue_completion :: (IsMTRClusterStateCacheContainer clusterStateCacheContainer, IsNSNumber endpoint, IsNSObject queue) => clusterStateCacheContainer -> endpoint -> queue -> Ptr () -> IO ()
readAttributePowerThresholdWithClusterStateCache_endpoint_queue_completion clusterStateCacheContainer endpoint queue completion =
  do
    cls' <- getRequiredClass "MTRBaseClusterMeterIdentification"
    sendClassMessage cls' readAttributePowerThresholdWithClusterStateCache_endpoint_queue_completionSelector (toMTRClusterStateCacheContainer clusterStateCacheContainer) (toNSNumber endpoint) (toNSObject queue) completion

-- | @- readAttributeGeneratedCommandListWithCompletion:@
readAttributeGeneratedCommandListWithCompletion :: IsMTRBaseClusterMeterIdentification mtrBaseClusterMeterIdentification => mtrBaseClusterMeterIdentification -> Ptr () -> IO ()
readAttributeGeneratedCommandListWithCompletion mtrBaseClusterMeterIdentification completion =
  sendMessage mtrBaseClusterMeterIdentification readAttributeGeneratedCommandListWithCompletionSelector completion

-- | @- subscribeAttributeGeneratedCommandListWithParams:subscriptionEstablished:reportHandler:@
subscribeAttributeGeneratedCommandListWithParams_subscriptionEstablished_reportHandler :: (IsMTRBaseClusterMeterIdentification mtrBaseClusterMeterIdentification, IsMTRSubscribeParams params) => mtrBaseClusterMeterIdentification -> params -> Ptr () -> Ptr () -> IO ()
subscribeAttributeGeneratedCommandListWithParams_subscriptionEstablished_reportHandler mtrBaseClusterMeterIdentification params subscriptionEstablished reportHandler =
  sendMessage mtrBaseClusterMeterIdentification subscribeAttributeGeneratedCommandListWithParams_subscriptionEstablished_reportHandlerSelector (toMTRSubscribeParams params) subscriptionEstablished reportHandler

-- | @+ readAttributeGeneratedCommandListWithClusterStateCache:endpoint:queue:completion:@
readAttributeGeneratedCommandListWithClusterStateCache_endpoint_queue_completion :: (IsMTRClusterStateCacheContainer clusterStateCacheContainer, IsNSNumber endpoint, IsNSObject queue) => clusterStateCacheContainer -> endpoint -> queue -> Ptr () -> IO ()
readAttributeGeneratedCommandListWithClusterStateCache_endpoint_queue_completion clusterStateCacheContainer endpoint queue completion =
  do
    cls' <- getRequiredClass "MTRBaseClusterMeterIdentification"
    sendClassMessage cls' readAttributeGeneratedCommandListWithClusterStateCache_endpoint_queue_completionSelector (toMTRClusterStateCacheContainer clusterStateCacheContainer) (toNSNumber endpoint) (toNSObject queue) completion

-- | @- readAttributeAcceptedCommandListWithCompletion:@
readAttributeAcceptedCommandListWithCompletion :: IsMTRBaseClusterMeterIdentification mtrBaseClusterMeterIdentification => mtrBaseClusterMeterIdentification -> Ptr () -> IO ()
readAttributeAcceptedCommandListWithCompletion mtrBaseClusterMeterIdentification completion =
  sendMessage mtrBaseClusterMeterIdentification readAttributeAcceptedCommandListWithCompletionSelector completion

-- | @- subscribeAttributeAcceptedCommandListWithParams:subscriptionEstablished:reportHandler:@
subscribeAttributeAcceptedCommandListWithParams_subscriptionEstablished_reportHandler :: (IsMTRBaseClusterMeterIdentification mtrBaseClusterMeterIdentification, IsMTRSubscribeParams params) => mtrBaseClusterMeterIdentification -> params -> Ptr () -> Ptr () -> IO ()
subscribeAttributeAcceptedCommandListWithParams_subscriptionEstablished_reportHandler mtrBaseClusterMeterIdentification params subscriptionEstablished reportHandler =
  sendMessage mtrBaseClusterMeterIdentification subscribeAttributeAcceptedCommandListWithParams_subscriptionEstablished_reportHandlerSelector (toMTRSubscribeParams params) subscriptionEstablished reportHandler

-- | @+ readAttributeAcceptedCommandListWithClusterStateCache:endpoint:queue:completion:@
readAttributeAcceptedCommandListWithClusterStateCache_endpoint_queue_completion :: (IsMTRClusterStateCacheContainer clusterStateCacheContainer, IsNSNumber endpoint, IsNSObject queue) => clusterStateCacheContainer -> endpoint -> queue -> Ptr () -> IO ()
readAttributeAcceptedCommandListWithClusterStateCache_endpoint_queue_completion clusterStateCacheContainer endpoint queue completion =
  do
    cls' <- getRequiredClass "MTRBaseClusterMeterIdentification"
    sendClassMessage cls' readAttributeAcceptedCommandListWithClusterStateCache_endpoint_queue_completionSelector (toMTRClusterStateCacheContainer clusterStateCacheContainer) (toNSNumber endpoint) (toNSObject queue) completion

-- | @- readAttributeAttributeListWithCompletion:@
readAttributeAttributeListWithCompletion :: IsMTRBaseClusterMeterIdentification mtrBaseClusterMeterIdentification => mtrBaseClusterMeterIdentification -> Ptr () -> IO ()
readAttributeAttributeListWithCompletion mtrBaseClusterMeterIdentification completion =
  sendMessage mtrBaseClusterMeterIdentification readAttributeAttributeListWithCompletionSelector completion

-- | @- subscribeAttributeAttributeListWithParams:subscriptionEstablished:reportHandler:@
subscribeAttributeAttributeListWithParams_subscriptionEstablished_reportHandler :: (IsMTRBaseClusterMeterIdentification mtrBaseClusterMeterIdentification, IsMTRSubscribeParams params) => mtrBaseClusterMeterIdentification -> params -> Ptr () -> Ptr () -> IO ()
subscribeAttributeAttributeListWithParams_subscriptionEstablished_reportHandler mtrBaseClusterMeterIdentification params subscriptionEstablished reportHandler =
  sendMessage mtrBaseClusterMeterIdentification subscribeAttributeAttributeListWithParams_subscriptionEstablished_reportHandlerSelector (toMTRSubscribeParams params) subscriptionEstablished reportHandler

-- | @+ readAttributeAttributeListWithClusterStateCache:endpoint:queue:completion:@
readAttributeAttributeListWithClusterStateCache_endpoint_queue_completion :: (IsMTRClusterStateCacheContainer clusterStateCacheContainer, IsNSNumber endpoint, IsNSObject queue) => clusterStateCacheContainer -> endpoint -> queue -> Ptr () -> IO ()
readAttributeAttributeListWithClusterStateCache_endpoint_queue_completion clusterStateCacheContainer endpoint queue completion =
  do
    cls' <- getRequiredClass "MTRBaseClusterMeterIdentification"
    sendClassMessage cls' readAttributeAttributeListWithClusterStateCache_endpoint_queue_completionSelector (toMTRClusterStateCacheContainer clusterStateCacheContainer) (toNSNumber endpoint) (toNSObject queue) completion

-- | @- readAttributeFeatureMapWithCompletion:@
readAttributeFeatureMapWithCompletion :: IsMTRBaseClusterMeterIdentification mtrBaseClusterMeterIdentification => mtrBaseClusterMeterIdentification -> Ptr () -> IO ()
readAttributeFeatureMapWithCompletion mtrBaseClusterMeterIdentification completion =
  sendMessage mtrBaseClusterMeterIdentification readAttributeFeatureMapWithCompletionSelector completion

-- | @- subscribeAttributeFeatureMapWithParams:subscriptionEstablished:reportHandler:@
subscribeAttributeFeatureMapWithParams_subscriptionEstablished_reportHandler :: (IsMTRBaseClusterMeterIdentification mtrBaseClusterMeterIdentification, IsMTRSubscribeParams params) => mtrBaseClusterMeterIdentification -> params -> Ptr () -> Ptr () -> IO ()
subscribeAttributeFeatureMapWithParams_subscriptionEstablished_reportHandler mtrBaseClusterMeterIdentification params subscriptionEstablished reportHandler =
  sendMessage mtrBaseClusterMeterIdentification subscribeAttributeFeatureMapWithParams_subscriptionEstablished_reportHandlerSelector (toMTRSubscribeParams params) subscriptionEstablished reportHandler

-- | @+ readAttributeFeatureMapWithClusterStateCache:endpoint:queue:completion:@
readAttributeFeatureMapWithClusterStateCache_endpoint_queue_completion :: (IsMTRClusterStateCacheContainer clusterStateCacheContainer, IsNSNumber endpoint, IsNSObject queue) => clusterStateCacheContainer -> endpoint -> queue -> Ptr () -> IO ()
readAttributeFeatureMapWithClusterStateCache_endpoint_queue_completion clusterStateCacheContainer endpoint queue completion =
  do
    cls' <- getRequiredClass "MTRBaseClusterMeterIdentification"
    sendClassMessage cls' readAttributeFeatureMapWithClusterStateCache_endpoint_queue_completionSelector (toMTRClusterStateCacheContainer clusterStateCacheContainer) (toNSNumber endpoint) (toNSObject queue) completion

-- | @- readAttributeClusterRevisionWithCompletion:@
readAttributeClusterRevisionWithCompletion :: IsMTRBaseClusterMeterIdentification mtrBaseClusterMeterIdentification => mtrBaseClusterMeterIdentification -> Ptr () -> IO ()
readAttributeClusterRevisionWithCompletion mtrBaseClusterMeterIdentification completion =
  sendMessage mtrBaseClusterMeterIdentification readAttributeClusterRevisionWithCompletionSelector completion

-- | @- subscribeAttributeClusterRevisionWithParams:subscriptionEstablished:reportHandler:@
subscribeAttributeClusterRevisionWithParams_subscriptionEstablished_reportHandler :: (IsMTRBaseClusterMeterIdentification mtrBaseClusterMeterIdentification, IsMTRSubscribeParams params) => mtrBaseClusterMeterIdentification -> params -> Ptr () -> Ptr () -> IO ()
subscribeAttributeClusterRevisionWithParams_subscriptionEstablished_reportHandler mtrBaseClusterMeterIdentification params subscriptionEstablished reportHandler =
  sendMessage mtrBaseClusterMeterIdentification subscribeAttributeClusterRevisionWithParams_subscriptionEstablished_reportHandlerSelector (toMTRSubscribeParams params) subscriptionEstablished reportHandler

-- | @+ readAttributeClusterRevisionWithClusterStateCache:endpoint:queue:completion:@
readAttributeClusterRevisionWithClusterStateCache_endpoint_queue_completion :: (IsMTRClusterStateCacheContainer clusterStateCacheContainer, IsNSNumber endpoint, IsNSObject queue) => clusterStateCacheContainer -> endpoint -> queue -> Ptr () -> IO ()
readAttributeClusterRevisionWithClusterStateCache_endpoint_queue_completion clusterStateCacheContainer endpoint queue completion =
  do
    cls' <- getRequiredClass "MTRBaseClusterMeterIdentification"
    sendClassMessage cls' readAttributeClusterRevisionWithClusterStateCache_endpoint_queue_completionSelector (toMTRClusterStateCacheContainer clusterStateCacheContainer) (toNSNumber endpoint) (toNSObject queue) completion

-- | @- init@
init_ :: IsMTRBaseClusterMeterIdentification mtrBaseClusterMeterIdentification => mtrBaseClusterMeterIdentification -> IO (Id MTRBaseClusterMeterIdentification)
init_ mtrBaseClusterMeterIdentification =
  sendOwnedMessage mtrBaseClusterMeterIdentification initSelector

-- | @+ new@
new :: IO (Id MTRBaseClusterMeterIdentification)
new  =
  do
    cls' <- getRequiredClass "MTRBaseClusterMeterIdentification"
    sendOwnedClassMessage cls' newSelector

-- | For all instance methods (reads, writes, commands) that take a completion, the completion will be called on the provided queue.
--
-- ObjC selector: @- initWithDevice:endpointID:queue:@
initWithDevice_endpointID_queue :: (IsMTRBaseClusterMeterIdentification mtrBaseClusterMeterIdentification, IsMTRBaseDevice device, IsNSNumber endpointID, IsNSObject queue) => mtrBaseClusterMeterIdentification -> device -> endpointID -> queue -> IO (Id MTRBaseClusterMeterIdentification)
initWithDevice_endpointID_queue mtrBaseClusterMeterIdentification device endpointID queue =
  sendOwnedMessage mtrBaseClusterMeterIdentification initWithDevice_endpointID_queueSelector (toMTRBaseDevice device) (toNSNumber endpointID) (toNSObject queue)

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @readAttributeMeterTypeWithCompletion:@
readAttributeMeterTypeWithCompletionSelector :: Selector '[Ptr ()] ()
readAttributeMeterTypeWithCompletionSelector = mkSelector "readAttributeMeterTypeWithCompletion:"

-- | @Selector@ for @subscribeAttributeMeterTypeWithParams:subscriptionEstablished:reportHandler:@
subscribeAttributeMeterTypeWithParams_subscriptionEstablished_reportHandlerSelector :: Selector '[Id MTRSubscribeParams, Ptr (), Ptr ()] ()
subscribeAttributeMeterTypeWithParams_subscriptionEstablished_reportHandlerSelector = mkSelector "subscribeAttributeMeterTypeWithParams:subscriptionEstablished:reportHandler:"

-- | @Selector@ for @readAttributeMeterTypeWithClusterStateCache:endpoint:queue:completion:@
readAttributeMeterTypeWithClusterStateCache_endpoint_queue_completionSelector :: Selector '[Id MTRClusterStateCacheContainer, Id NSNumber, Id NSObject, Ptr ()] ()
readAttributeMeterTypeWithClusterStateCache_endpoint_queue_completionSelector = mkSelector "readAttributeMeterTypeWithClusterStateCache:endpoint:queue:completion:"

-- | @Selector@ for @readAttributePointOfDeliveryWithCompletion:@
readAttributePointOfDeliveryWithCompletionSelector :: Selector '[Ptr ()] ()
readAttributePointOfDeliveryWithCompletionSelector = mkSelector "readAttributePointOfDeliveryWithCompletion:"

-- | @Selector@ for @subscribeAttributePointOfDeliveryWithParams:subscriptionEstablished:reportHandler:@
subscribeAttributePointOfDeliveryWithParams_subscriptionEstablished_reportHandlerSelector :: Selector '[Id MTRSubscribeParams, Ptr (), Ptr ()] ()
subscribeAttributePointOfDeliveryWithParams_subscriptionEstablished_reportHandlerSelector = mkSelector "subscribeAttributePointOfDeliveryWithParams:subscriptionEstablished:reportHandler:"

-- | @Selector@ for @readAttributePointOfDeliveryWithClusterStateCache:endpoint:queue:completion:@
readAttributePointOfDeliveryWithClusterStateCache_endpoint_queue_completionSelector :: Selector '[Id MTRClusterStateCacheContainer, Id NSNumber, Id NSObject, Ptr ()] ()
readAttributePointOfDeliveryWithClusterStateCache_endpoint_queue_completionSelector = mkSelector "readAttributePointOfDeliveryWithClusterStateCache:endpoint:queue:completion:"

-- | @Selector@ for @readAttributeMeterSerialNumberWithCompletion:@
readAttributeMeterSerialNumberWithCompletionSelector :: Selector '[Ptr ()] ()
readAttributeMeterSerialNumberWithCompletionSelector = mkSelector "readAttributeMeterSerialNumberWithCompletion:"

-- | @Selector@ for @subscribeAttributeMeterSerialNumberWithParams:subscriptionEstablished:reportHandler:@
subscribeAttributeMeterSerialNumberWithParams_subscriptionEstablished_reportHandlerSelector :: Selector '[Id MTRSubscribeParams, Ptr (), Ptr ()] ()
subscribeAttributeMeterSerialNumberWithParams_subscriptionEstablished_reportHandlerSelector = mkSelector "subscribeAttributeMeterSerialNumberWithParams:subscriptionEstablished:reportHandler:"

-- | @Selector@ for @readAttributeMeterSerialNumberWithClusterStateCache:endpoint:queue:completion:@
readAttributeMeterSerialNumberWithClusterStateCache_endpoint_queue_completionSelector :: Selector '[Id MTRClusterStateCacheContainer, Id NSNumber, Id NSObject, Ptr ()] ()
readAttributeMeterSerialNumberWithClusterStateCache_endpoint_queue_completionSelector = mkSelector "readAttributeMeterSerialNumberWithClusterStateCache:endpoint:queue:completion:"

-- | @Selector@ for @readAttributeProtocolVersionWithCompletion:@
readAttributeProtocolVersionWithCompletionSelector :: Selector '[Ptr ()] ()
readAttributeProtocolVersionWithCompletionSelector = mkSelector "readAttributeProtocolVersionWithCompletion:"

-- | @Selector@ for @subscribeAttributeProtocolVersionWithParams:subscriptionEstablished:reportHandler:@
subscribeAttributeProtocolVersionWithParams_subscriptionEstablished_reportHandlerSelector :: Selector '[Id MTRSubscribeParams, Ptr (), Ptr ()] ()
subscribeAttributeProtocolVersionWithParams_subscriptionEstablished_reportHandlerSelector = mkSelector "subscribeAttributeProtocolVersionWithParams:subscriptionEstablished:reportHandler:"

-- | @Selector@ for @readAttributeProtocolVersionWithClusterStateCache:endpoint:queue:completion:@
readAttributeProtocolVersionWithClusterStateCache_endpoint_queue_completionSelector :: Selector '[Id MTRClusterStateCacheContainer, Id NSNumber, Id NSObject, Ptr ()] ()
readAttributeProtocolVersionWithClusterStateCache_endpoint_queue_completionSelector = mkSelector "readAttributeProtocolVersionWithClusterStateCache:endpoint:queue:completion:"

-- | @Selector@ for @readAttributePowerThresholdWithCompletion:@
readAttributePowerThresholdWithCompletionSelector :: Selector '[Ptr ()] ()
readAttributePowerThresholdWithCompletionSelector = mkSelector "readAttributePowerThresholdWithCompletion:"

-- | @Selector@ for @subscribeAttributePowerThresholdWithParams:subscriptionEstablished:reportHandler:@
subscribeAttributePowerThresholdWithParams_subscriptionEstablished_reportHandlerSelector :: Selector '[Id MTRSubscribeParams, Ptr (), Ptr ()] ()
subscribeAttributePowerThresholdWithParams_subscriptionEstablished_reportHandlerSelector = mkSelector "subscribeAttributePowerThresholdWithParams:subscriptionEstablished:reportHandler:"

-- | @Selector@ for @readAttributePowerThresholdWithClusterStateCache:endpoint:queue:completion:@
readAttributePowerThresholdWithClusterStateCache_endpoint_queue_completionSelector :: Selector '[Id MTRClusterStateCacheContainer, Id NSNumber, Id NSObject, Ptr ()] ()
readAttributePowerThresholdWithClusterStateCache_endpoint_queue_completionSelector = mkSelector "readAttributePowerThresholdWithClusterStateCache:endpoint:queue:completion:"

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
initSelector :: Selector '[] (Id MTRBaseClusterMeterIdentification)
initSelector = mkSelector "init"

-- | @Selector@ for @new@
newSelector :: Selector '[] (Id MTRBaseClusterMeterIdentification)
newSelector = mkSelector "new"

-- | @Selector@ for @initWithDevice:endpointID:queue:@
initWithDevice_endpointID_queueSelector :: Selector '[Id MTRBaseDevice, Id NSNumber, Id NSObject] (Id MTRBaseClusterMeterIdentification)
initWithDevice_endpointID_queueSelector = mkSelector "initWithDevice:endpointID:queue:"

