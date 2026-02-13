{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Cluster Service Area
--
-- The Service Area cluster provides an interface for controlling the areas where a device should operate, and for querying the current area being serviced.
--
-- Generated bindings for @MTRBaseClusterServiceArea@.
module ObjC.Matter.MTRBaseClusterServiceArea
  ( MTRBaseClusterServiceArea
  , IsMTRBaseClusterServiceArea(..)
  , selectAreasWithParams_completion
  , skipAreaWithParams_completion
  , readAttributeSupportedAreasWithCompletion
  , subscribeAttributeSupportedAreasWithParams_subscriptionEstablished_reportHandler
  , readAttributeSupportedAreasWithClusterStateCache_endpoint_queue_completion
  , readAttributeSupportedMapsWithCompletion
  , subscribeAttributeSupportedMapsWithParams_subscriptionEstablished_reportHandler
  , readAttributeSupportedMapsWithClusterStateCache_endpoint_queue_completion
  , readAttributeSelectedAreasWithCompletion
  , subscribeAttributeSelectedAreasWithParams_subscriptionEstablished_reportHandler
  , readAttributeSelectedAreasWithClusterStateCache_endpoint_queue_completion
  , readAttributeCurrentAreaWithCompletion
  , subscribeAttributeCurrentAreaWithParams_subscriptionEstablished_reportHandler
  , readAttributeCurrentAreaWithClusterStateCache_endpoint_queue_completion
  , readAttributeEstimatedEndTimeWithCompletion
  , subscribeAttributeEstimatedEndTimeWithParams_subscriptionEstablished_reportHandler
  , readAttributeEstimatedEndTimeWithClusterStateCache_endpoint_queue_completion
  , readAttributeProgressWithCompletion
  , subscribeAttributeProgressWithParams_subscriptionEstablished_reportHandler
  , readAttributeProgressWithClusterStateCache_endpoint_queue_completion
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
  , readAttributeCurrentAreaWithClusterStateCache_endpoint_queue_completionSelector
  , readAttributeCurrentAreaWithCompletionSelector
  , readAttributeEstimatedEndTimeWithClusterStateCache_endpoint_queue_completionSelector
  , readAttributeEstimatedEndTimeWithCompletionSelector
  , readAttributeFeatureMapWithClusterStateCache_endpoint_queue_completionSelector
  , readAttributeFeatureMapWithCompletionSelector
  , readAttributeGeneratedCommandListWithClusterStateCache_endpoint_queue_completionSelector
  , readAttributeGeneratedCommandListWithCompletionSelector
  , readAttributeProgressWithClusterStateCache_endpoint_queue_completionSelector
  , readAttributeProgressWithCompletionSelector
  , readAttributeSelectedAreasWithClusterStateCache_endpoint_queue_completionSelector
  , readAttributeSelectedAreasWithCompletionSelector
  , readAttributeSupportedAreasWithClusterStateCache_endpoint_queue_completionSelector
  , readAttributeSupportedAreasWithCompletionSelector
  , readAttributeSupportedMapsWithClusterStateCache_endpoint_queue_completionSelector
  , readAttributeSupportedMapsWithCompletionSelector
  , selectAreasWithParams_completionSelector
  , skipAreaWithParams_completionSelector
  , subscribeAttributeAcceptedCommandListWithParams_subscriptionEstablished_reportHandlerSelector
  , subscribeAttributeAttributeListWithParams_subscriptionEstablished_reportHandlerSelector
  , subscribeAttributeClusterRevisionWithParams_subscriptionEstablished_reportHandlerSelector
  , subscribeAttributeCurrentAreaWithParams_subscriptionEstablished_reportHandlerSelector
  , subscribeAttributeEstimatedEndTimeWithParams_subscriptionEstablished_reportHandlerSelector
  , subscribeAttributeFeatureMapWithParams_subscriptionEstablished_reportHandlerSelector
  , subscribeAttributeGeneratedCommandListWithParams_subscriptionEstablished_reportHandlerSelector
  , subscribeAttributeProgressWithParams_subscriptionEstablished_reportHandlerSelector
  , subscribeAttributeSelectedAreasWithParams_subscriptionEstablished_reportHandlerSelector
  , subscribeAttributeSupportedAreasWithParams_subscriptionEstablished_reportHandlerSelector
  , subscribeAttributeSupportedMapsWithParams_subscriptionEstablished_reportHandlerSelector


  ) where

import Foreign.Ptr (Ptr, FunPtr)
import Foreign.C.Types

import ObjC.Runtime.Types
import ObjC.Runtime.Message (sendMessage, sendOwnedMessage, sendClassMessage, sendOwnedClassMessage)
import ObjC.Runtime.Selector (mkSelector)
import ObjC.Runtime.Class (getRequiredClass)

import ObjC.Matter.Internal.Classes
import ObjC.Foundation.Internal.Classes

-- | Command SelectAreas
--
-- This command is used to select a set of device areas, where the device is to operate.
--
-- ObjC selector: @- selectAreasWithParams:completion:@
selectAreasWithParams_completion :: (IsMTRBaseClusterServiceArea mtrBaseClusterServiceArea, IsMTRServiceAreaClusterSelectAreasParams params) => mtrBaseClusterServiceArea -> params -> Ptr () -> IO ()
selectAreasWithParams_completion mtrBaseClusterServiceArea params completion =
  sendMessage mtrBaseClusterServiceArea selectAreasWithParams_completionSelector (toMTRServiceAreaClusterSelectAreasParams params) completion

-- | Command SkipArea
--
-- This command is used to skip the given area, and to attempt operating at other areas on the SupportedAreas attribute list.
--
-- ObjC selector: @- skipAreaWithParams:completion:@
skipAreaWithParams_completion :: (IsMTRBaseClusterServiceArea mtrBaseClusterServiceArea, IsMTRServiceAreaClusterSkipAreaParams params) => mtrBaseClusterServiceArea -> params -> Ptr () -> IO ()
skipAreaWithParams_completion mtrBaseClusterServiceArea params completion =
  sendMessage mtrBaseClusterServiceArea skipAreaWithParams_completionSelector (toMTRServiceAreaClusterSkipAreaParams params) completion

-- | @- readAttributeSupportedAreasWithCompletion:@
readAttributeSupportedAreasWithCompletion :: IsMTRBaseClusterServiceArea mtrBaseClusterServiceArea => mtrBaseClusterServiceArea -> Ptr () -> IO ()
readAttributeSupportedAreasWithCompletion mtrBaseClusterServiceArea completion =
  sendMessage mtrBaseClusterServiceArea readAttributeSupportedAreasWithCompletionSelector completion

-- | @- subscribeAttributeSupportedAreasWithParams:subscriptionEstablished:reportHandler:@
subscribeAttributeSupportedAreasWithParams_subscriptionEstablished_reportHandler :: (IsMTRBaseClusterServiceArea mtrBaseClusterServiceArea, IsMTRSubscribeParams params) => mtrBaseClusterServiceArea -> params -> Ptr () -> Ptr () -> IO ()
subscribeAttributeSupportedAreasWithParams_subscriptionEstablished_reportHandler mtrBaseClusterServiceArea params subscriptionEstablished reportHandler =
  sendMessage mtrBaseClusterServiceArea subscribeAttributeSupportedAreasWithParams_subscriptionEstablished_reportHandlerSelector (toMTRSubscribeParams params) subscriptionEstablished reportHandler

-- | @+ readAttributeSupportedAreasWithClusterStateCache:endpoint:queue:completion:@
readAttributeSupportedAreasWithClusterStateCache_endpoint_queue_completion :: (IsMTRClusterStateCacheContainer clusterStateCacheContainer, IsNSNumber endpoint, IsNSObject queue) => clusterStateCacheContainer -> endpoint -> queue -> Ptr () -> IO ()
readAttributeSupportedAreasWithClusterStateCache_endpoint_queue_completion clusterStateCacheContainer endpoint queue completion =
  do
    cls' <- getRequiredClass "MTRBaseClusterServiceArea"
    sendClassMessage cls' readAttributeSupportedAreasWithClusterStateCache_endpoint_queue_completionSelector (toMTRClusterStateCacheContainer clusterStateCacheContainer) (toNSNumber endpoint) (toNSObject queue) completion

-- | @- readAttributeSupportedMapsWithCompletion:@
readAttributeSupportedMapsWithCompletion :: IsMTRBaseClusterServiceArea mtrBaseClusterServiceArea => mtrBaseClusterServiceArea -> Ptr () -> IO ()
readAttributeSupportedMapsWithCompletion mtrBaseClusterServiceArea completion =
  sendMessage mtrBaseClusterServiceArea readAttributeSupportedMapsWithCompletionSelector completion

-- | @- subscribeAttributeSupportedMapsWithParams:subscriptionEstablished:reportHandler:@
subscribeAttributeSupportedMapsWithParams_subscriptionEstablished_reportHandler :: (IsMTRBaseClusterServiceArea mtrBaseClusterServiceArea, IsMTRSubscribeParams params) => mtrBaseClusterServiceArea -> params -> Ptr () -> Ptr () -> IO ()
subscribeAttributeSupportedMapsWithParams_subscriptionEstablished_reportHandler mtrBaseClusterServiceArea params subscriptionEstablished reportHandler =
  sendMessage mtrBaseClusterServiceArea subscribeAttributeSupportedMapsWithParams_subscriptionEstablished_reportHandlerSelector (toMTRSubscribeParams params) subscriptionEstablished reportHandler

-- | @+ readAttributeSupportedMapsWithClusterStateCache:endpoint:queue:completion:@
readAttributeSupportedMapsWithClusterStateCache_endpoint_queue_completion :: (IsMTRClusterStateCacheContainer clusterStateCacheContainer, IsNSNumber endpoint, IsNSObject queue) => clusterStateCacheContainer -> endpoint -> queue -> Ptr () -> IO ()
readAttributeSupportedMapsWithClusterStateCache_endpoint_queue_completion clusterStateCacheContainer endpoint queue completion =
  do
    cls' <- getRequiredClass "MTRBaseClusterServiceArea"
    sendClassMessage cls' readAttributeSupportedMapsWithClusterStateCache_endpoint_queue_completionSelector (toMTRClusterStateCacheContainer clusterStateCacheContainer) (toNSNumber endpoint) (toNSObject queue) completion

-- | @- readAttributeSelectedAreasWithCompletion:@
readAttributeSelectedAreasWithCompletion :: IsMTRBaseClusterServiceArea mtrBaseClusterServiceArea => mtrBaseClusterServiceArea -> Ptr () -> IO ()
readAttributeSelectedAreasWithCompletion mtrBaseClusterServiceArea completion =
  sendMessage mtrBaseClusterServiceArea readAttributeSelectedAreasWithCompletionSelector completion

-- | @- subscribeAttributeSelectedAreasWithParams:subscriptionEstablished:reportHandler:@
subscribeAttributeSelectedAreasWithParams_subscriptionEstablished_reportHandler :: (IsMTRBaseClusterServiceArea mtrBaseClusterServiceArea, IsMTRSubscribeParams params) => mtrBaseClusterServiceArea -> params -> Ptr () -> Ptr () -> IO ()
subscribeAttributeSelectedAreasWithParams_subscriptionEstablished_reportHandler mtrBaseClusterServiceArea params subscriptionEstablished reportHandler =
  sendMessage mtrBaseClusterServiceArea subscribeAttributeSelectedAreasWithParams_subscriptionEstablished_reportHandlerSelector (toMTRSubscribeParams params) subscriptionEstablished reportHandler

-- | @+ readAttributeSelectedAreasWithClusterStateCache:endpoint:queue:completion:@
readAttributeSelectedAreasWithClusterStateCache_endpoint_queue_completion :: (IsMTRClusterStateCacheContainer clusterStateCacheContainer, IsNSNumber endpoint, IsNSObject queue) => clusterStateCacheContainer -> endpoint -> queue -> Ptr () -> IO ()
readAttributeSelectedAreasWithClusterStateCache_endpoint_queue_completion clusterStateCacheContainer endpoint queue completion =
  do
    cls' <- getRequiredClass "MTRBaseClusterServiceArea"
    sendClassMessage cls' readAttributeSelectedAreasWithClusterStateCache_endpoint_queue_completionSelector (toMTRClusterStateCacheContainer clusterStateCacheContainer) (toNSNumber endpoint) (toNSObject queue) completion

-- | @- readAttributeCurrentAreaWithCompletion:@
readAttributeCurrentAreaWithCompletion :: IsMTRBaseClusterServiceArea mtrBaseClusterServiceArea => mtrBaseClusterServiceArea -> Ptr () -> IO ()
readAttributeCurrentAreaWithCompletion mtrBaseClusterServiceArea completion =
  sendMessage mtrBaseClusterServiceArea readAttributeCurrentAreaWithCompletionSelector completion

-- | @- subscribeAttributeCurrentAreaWithParams:subscriptionEstablished:reportHandler:@
subscribeAttributeCurrentAreaWithParams_subscriptionEstablished_reportHandler :: (IsMTRBaseClusterServiceArea mtrBaseClusterServiceArea, IsMTRSubscribeParams params) => mtrBaseClusterServiceArea -> params -> Ptr () -> Ptr () -> IO ()
subscribeAttributeCurrentAreaWithParams_subscriptionEstablished_reportHandler mtrBaseClusterServiceArea params subscriptionEstablished reportHandler =
  sendMessage mtrBaseClusterServiceArea subscribeAttributeCurrentAreaWithParams_subscriptionEstablished_reportHandlerSelector (toMTRSubscribeParams params) subscriptionEstablished reportHandler

-- | @+ readAttributeCurrentAreaWithClusterStateCache:endpoint:queue:completion:@
readAttributeCurrentAreaWithClusterStateCache_endpoint_queue_completion :: (IsMTRClusterStateCacheContainer clusterStateCacheContainer, IsNSNumber endpoint, IsNSObject queue) => clusterStateCacheContainer -> endpoint -> queue -> Ptr () -> IO ()
readAttributeCurrentAreaWithClusterStateCache_endpoint_queue_completion clusterStateCacheContainer endpoint queue completion =
  do
    cls' <- getRequiredClass "MTRBaseClusterServiceArea"
    sendClassMessage cls' readAttributeCurrentAreaWithClusterStateCache_endpoint_queue_completionSelector (toMTRClusterStateCacheContainer clusterStateCacheContainer) (toNSNumber endpoint) (toNSObject queue) completion

-- | @- readAttributeEstimatedEndTimeWithCompletion:@
readAttributeEstimatedEndTimeWithCompletion :: IsMTRBaseClusterServiceArea mtrBaseClusterServiceArea => mtrBaseClusterServiceArea -> Ptr () -> IO ()
readAttributeEstimatedEndTimeWithCompletion mtrBaseClusterServiceArea completion =
  sendMessage mtrBaseClusterServiceArea readAttributeEstimatedEndTimeWithCompletionSelector completion

-- | @- subscribeAttributeEstimatedEndTimeWithParams:subscriptionEstablished:reportHandler:@
subscribeAttributeEstimatedEndTimeWithParams_subscriptionEstablished_reportHandler :: (IsMTRBaseClusterServiceArea mtrBaseClusterServiceArea, IsMTRSubscribeParams params) => mtrBaseClusterServiceArea -> params -> Ptr () -> Ptr () -> IO ()
subscribeAttributeEstimatedEndTimeWithParams_subscriptionEstablished_reportHandler mtrBaseClusterServiceArea params subscriptionEstablished reportHandler =
  sendMessage mtrBaseClusterServiceArea subscribeAttributeEstimatedEndTimeWithParams_subscriptionEstablished_reportHandlerSelector (toMTRSubscribeParams params) subscriptionEstablished reportHandler

-- | @+ readAttributeEstimatedEndTimeWithClusterStateCache:endpoint:queue:completion:@
readAttributeEstimatedEndTimeWithClusterStateCache_endpoint_queue_completion :: (IsMTRClusterStateCacheContainer clusterStateCacheContainer, IsNSNumber endpoint, IsNSObject queue) => clusterStateCacheContainer -> endpoint -> queue -> Ptr () -> IO ()
readAttributeEstimatedEndTimeWithClusterStateCache_endpoint_queue_completion clusterStateCacheContainer endpoint queue completion =
  do
    cls' <- getRequiredClass "MTRBaseClusterServiceArea"
    sendClassMessage cls' readAttributeEstimatedEndTimeWithClusterStateCache_endpoint_queue_completionSelector (toMTRClusterStateCacheContainer clusterStateCacheContainer) (toNSNumber endpoint) (toNSObject queue) completion

-- | @- readAttributeProgressWithCompletion:@
readAttributeProgressWithCompletion :: IsMTRBaseClusterServiceArea mtrBaseClusterServiceArea => mtrBaseClusterServiceArea -> Ptr () -> IO ()
readAttributeProgressWithCompletion mtrBaseClusterServiceArea completion =
  sendMessage mtrBaseClusterServiceArea readAttributeProgressWithCompletionSelector completion

-- | @- subscribeAttributeProgressWithParams:subscriptionEstablished:reportHandler:@
subscribeAttributeProgressWithParams_subscriptionEstablished_reportHandler :: (IsMTRBaseClusterServiceArea mtrBaseClusterServiceArea, IsMTRSubscribeParams params) => mtrBaseClusterServiceArea -> params -> Ptr () -> Ptr () -> IO ()
subscribeAttributeProgressWithParams_subscriptionEstablished_reportHandler mtrBaseClusterServiceArea params subscriptionEstablished reportHandler =
  sendMessage mtrBaseClusterServiceArea subscribeAttributeProgressWithParams_subscriptionEstablished_reportHandlerSelector (toMTRSubscribeParams params) subscriptionEstablished reportHandler

-- | @+ readAttributeProgressWithClusterStateCache:endpoint:queue:completion:@
readAttributeProgressWithClusterStateCache_endpoint_queue_completion :: (IsMTRClusterStateCacheContainer clusterStateCacheContainer, IsNSNumber endpoint, IsNSObject queue) => clusterStateCacheContainer -> endpoint -> queue -> Ptr () -> IO ()
readAttributeProgressWithClusterStateCache_endpoint_queue_completion clusterStateCacheContainer endpoint queue completion =
  do
    cls' <- getRequiredClass "MTRBaseClusterServiceArea"
    sendClassMessage cls' readAttributeProgressWithClusterStateCache_endpoint_queue_completionSelector (toMTRClusterStateCacheContainer clusterStateCacheContainer) (toNSNumber endpoint) (toNSObject queue) completion

-- | @- readAttributeGeneratedCommandListWithCompletion:@
readAttributeGeneratedCommandListWithCompletion :: IsMTRBaseClusterServiceArea mtrBaseClusterServiceArea => mtrBaseClusterServiceArea -> Ptr () -> IO ()
readAttributeGeneratedCommandListWithCompletion mtrBaseClusterServiceArea completion =
  sendMessage mtrBaseClusterServiceArea readAttributeGeneratedCommandListWithCompletionSelector completion

-- | @- subscribeAttributeGeneratedCommandListWithParams:subscriptionEstablished:reportHandler:@
subscribeAttributeGeneratedCommandListWithParams_subscriptionEstablished_reportHandler :: (IsMTRBaseClusterServiceArea mtrBaseClusterServiceArea, IsMTRSubscribeParams params) => mtrBaseClusterServiceArea -> params -> Ptr () -> Ptr () -> IO ()
subscribeAttributeGeneratedCommandListWithParams_subscriptionEstablished_reportHandler mtrBaseClusterServiceArea params subscriptionEstablished reportHandler =
  sendMessage mtrBaseClusterServiceArea subscribeAttributeGeneratedCommandListWithParams_subscriptionEstablished_reportHandlerSelector (toMTRSubscribeParams params) subscriptionEstablished reportHandler

-- | @+ readAttributeGeneratedCommandListWithClusterStateCache:endpoint:queue:completion:@
readAttributeGeneratedCommandListWithClusterStateCache_endpoint_queue_completion :: (IsMTRClusterStateCacheContainer clusterStateCacheContainer, IsNSNumber endpoint, IsNSObject queue) => clusterStateCacheContainer -> endpoint -> queue -> Ptr () -> IO ()
readAttributeGeneratedCommandListWithClusterStateCache_endpoint_queue_completion clusterStateCacheContainer endpoint queue completion =
  do
    cls' <- getRequiredClass "MTRBaseClusterServiceArea"
    sendClassMessage cls' readAttributeGeneratedCommandListWithClusterStateCache_endpoint_queue_completionSelector (toMTRClusterStateCacheContainer clusterStateCacheContainer) (toNSNumber endpoint) (toNSObject queue) completion

-- | @- readAttributeAcceptedCommandListWithCompletion:@
readAttributeAcceptedCommandListWithCompletion :: IsMTRBaseClusterServiceArea mtrBaseClusterServiceArea => mtrBaseClusterServiceArea -> Ptr () -> IO ()
readAttributeAcceptedCommandListWithCompletion mtrBaseClusterServiceArea completion =
  sendMessage mtrBaseClusterServiceArea readAttributeAcceptedCommandListWithCompletionSelector completion

-- | @- subscribeAttributeAcceptedCommandListWithParams:subscriptionEstablished:reportHandler:@
subscribeAttributeAcceptedCommandListWithParams_subscriptionEstablished_reportHandler :: (IsMTRBaseClusterServiceArea mtrBaseClusterServiceArea, IsMTRSubscribeParams params) => mtrBaseClusterServiceArea -> params -> Ptr () -> Ptr () -> IO ()
subscribeAttributeAcceptedCommandListWithParams_subscriptionEstablished_reportHandler mtrBaseClusterServiceArea params subscriptionEstablished reportHandler =
  sendMessage mtrBaseClusterServiceArea subscribeAttributeAcceptedCommandListWithParams_subscriptionEstablished_reportHandlerSelector (toMTRSubscribeParams params) subscriptionEstablished reportHandler

-- | @+ readAttributeAcceptedCommandListWithClusterStateCache:endpoint:queue:completion:@
readAttributeAcceptedCommandListWithClusterStateCache_endpoint_queue_completion :: (IsMTRClusterStateCacheContainer clusterStateCacheContainer, IsNSNumber endpoint, IsNSObject queue) => clusterStateCacheContainer -> endpoint -> queue -> Ptr () -> IO ()
readAttributeAcceptedCommandListWithClusterStateCache_endpoint_queue_completion clusterStateCacheContainer endpoint queue completion =
  do
    cls' <- getRequiredClass "MTRBaseClusterServiceArea"
    sendClassMessage cls' readAttributeAcceptedCommandListWithClusterStateCache_endpoint_queue_completionSelector (toMTRClusterStateCacheContainer clusterStateCacheContainer) (toNSNumber endpoint) (toNSObject queue) completion

-- | @- readAttributeAttributeListWithCompletion:@
readAttributeAttributeListWithCompletion :: IsMTRBaseClusterServiceArea mtrBaseClusterServiceArea => mtrBaseClusterServiceArea -> Ptr () -> IO ()
readAttributeAttributeListWithCompletion mtrBaseClusterServiceArea completion =
  sendMessage mtrBaseClusterServiceArea readAttributeAttributeListWithCompletionSelector completion

-- | @- subscribeAttributeAttributeListWithParams:subscriptionEstablished:reportHandler:@
subscribeAttributeAttributeListWithParams_subscriptionEstablished_reportHandler :: (IsMTRBaseClusterServiceArea mtrBaseClusterServiceArea, IsMTRSubscribeParams params) => mtrBaseClusterServiceArea -> params -> Ptr () -> Ptr () -> IO ()
subscribeAttributeAttributeListWithParams_subscriptionEstablished_reportHandler mtrBaseClusterServiceArea params subscriptionEstablished reportHandler =
  sendMessage mtrBaseClusterServiceArea subscribeAttributeAttributeListWithParams_subscriptionEstablished_reportHandlerSelector (toMTRSubscribeParams params) subscriptionEstablished reportHandler

-- | @+ readAttributeAttributeListWithClusterStateCache:endpoint:queue:completion:@
readAttributeAttributeListWithClusterStateCache_endpoint_queue_completion :: (IsMTRClusterStateCacheContainer clusterStateCacheContainer, IsNSNumber endpoint, IsNSObject queue) => clusterStateCacheContainer -> endpoint -> queue -> Ptr () -> IO ()
readAttributeAttributeListWithClusterStateCache_endpoint_queue_completion clusterStateCacheContainer endpoint queue completion =
  do
    cls' <- getRequiredClass "MTRBaseClusterServiceArea"
    sendClassMessage cls' readAttributeAttributeListWithClusterStateCache_endpoint_queue_completionSelector (toMTRClusterStateCacheContainer clusterStateCacheContainer) (toNSNumber endpoint) (toNSObject queue) completion

-- | @- readAttributeFeatureMapWithCompletion:@
readAttributeFeatureMapWithCompletion :: IsMTRBaseClusterServiceArea mtrBaseClusterServiceArea => mtrBaseClusterServiceArea -> Ptr () -> IO ()
readAttributeFeatureMapWithCompletion mtrBaseClusterServiceArea completion =
  sendMessage mtrBaseClusterServiceArea readAttributeFeatureMapWithCompletionSelector completion

-- | @- subscribeAttributeFeatureMapWithParams:subscriptionEstablished:reportHandler:@
subscribeAttributeFeatureMapWithParams_subscriptionEstablished_reportHandler :: (IsMTRBaseClusterServiceArea mtrBaseClusterServiceArea, IsMTRSubscribeParams params) => mtrBaseClusterServiceArea -> params -> Ptr () -> Ptr () -> IO ()
subscribeAttributeFeatureMapWithParams_subscriptionEstablished_reportHandler mtrBaseClusterServiceArea params subscriptionEstablished reportHandler =
  sendMessage mtrBaseClusterServiceArea subscribeAttributeFeatureMapWithParams_subscriptionEstablished_reportHandlerSelector (toMTRSubscribeParams params) subscriptionEstablished reportHandler

-- | @+ readAttributeFeatureMapWithClusterStateCache:endpoint:queue:completion:@
readAttributeFeatureMapWithClusterStateCache_endpoint_queue_completion :: (IsMTRClusterStateCacheContainer clusterStateCacheContainer, IsNSNumber endpoint, IsNSObject queue) => clusterStateCacheContainer -> endpoint -> queue -> Ptr () -> IO ()
readAttributeFeatureMapWithClusterStateCache_endpoint_queue_completion clusterStateCacheContainer endpoint queue completion =
  do
    cls' <- getRequiredClass "MTRBaseClusterServiceArea"
    sendClassMessage cls' readAttributeFeatureMapWithClusterStateCache_endpoint_queue_completionSelector (toMTRClusterStateCacheContainer clusterStateCacheContainer) (toNSNumber endpoint) (toNSObject queue) completion

-- | @- readAttributeClusterRevisionWithCompletion:@
readAttributeClusterRevisionWithCompletion :: IsMTRBaseClusterServiceArea mtrBaseClusterServiceArea => mtrBaseClusterServiceArea -> Ptr () -> IO ()
readAttributeClusterRevisionWithCompletion mtrBaseClusterServiceArea completion =
  sendMessage mtrBaseClusterServiceArea readAttributeClusterRevisionWithCompletionSelector completion

-- | @- subscribeAttributeClusterRevisionWithParams:subscriptionEstablished:reportHandler:@
subscribeAttributeClusterRevisionWithParams_subscriptionEstablished_reportHandler :: (IsMTRBaseClusterServiceArea mtrBaseClusterServiceArea, IsMTRSubscribeParams params) => mtrBaseClusterServiceArea -> params -> Ptr () -> Ptr () -> IO ()
subscribeAttributeClusterRevisionWithParams_subscriptionEstablished_reportHandler mtrBaseClusterServiceArea params subscriptionEstablished reportHandler =
  sendMessage mtrBaseClusterServiceArea subscribeAttributeClusterRevisionWithParams_subscriptionEstablished_reportHandlerSelector (toMTRSubscribeParams params) subscriptionEstablished reportHandler

-- | @+ readAttributeClusterRevisionWithClusterStateCache:endpoint:queue:completion:@
readAttributeClusterRevisionWithClusterStateCache_endpoint_queue_completion :: (IsMTRClusterStateCacheContainer clusterStateCacheContainer, IsNSNumber endpoint, IsNSObject queue) => clusterStateCacheContainer -> endpoint -> queue -> Ptr () -> IO ()
readAttributeClusterRevisionWithClusterStateCache_endpoint_queue_completion clusterStateCacheContainer endpoint queue completion =
  do
    cls' <- getRequiredClass "MTRBaseClusterServiceArea"
    sendClassMessage cls' readAttributeClusterRevisionWithClusterStateCache_endpoint_queue_completionSelector (toMTRClusterStateCacheContainer clusterStateCacheContainer) (toNSNumber endpoint) (toNSObject queue) completion

-- | @- init@
init_ :: IsMTRBaseClusterServiceArea mtrBaseClusterServiceArea => mtrBaseClusterServiceArea -> IO (Id MTRBaseClusterServiceArea)
init_ mtrBaseClusterServiceArea =
  sendOwnedMessage mtrBaseClusterServiceArea initSelector

-- | @+ new@
new :: IO (Id MTRBaseClusterServiceArea)
new  =
  do
    cls' <- getRequiredClass "MTRBaseClusterServiceArea"
    sendOwnedClassMessage cls' newSelector

-- | For all instance methods (reads, writes, commands) that take a completion, the completion will be called on the provided queue.
--
-- ObjC selector: @- initWithDevice:endpointID:queue:@
initWithDevice_endpointID_queue :: (IsMTRBaseClusterServiceArea mtrBaseClusterServiceArea, IsMTRBaseDevice device, IsNSNumber endpointID, IsNSObject queue) => mtrBaseClusterServiceArea -> device -> endpointID -> queue -> IO (Id MTRBaseClusterServiceArea)
initWithDevice_endpointID_queue mtrBaseClusterServiceArea device endpointID queue =
  sendOwnedMessage mtrBaseClusterServiceArea initWithDevice_endpointID_queueSelector (toMTRBaseDevice device) (toNSNumber endpointID) (toNSObject queue)

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @selectAreasWithParams:completion:@
selectAreasWithParams_completionSelector :: Selector '[Id MTRServiceAreaClusterSelectAreasParams, Ptr ()] ()
selectAreasWithParams_completionSelector = mkSelector "selectAreasWithParams:completion:"

-- | @Selector@ for @skipAreaWithParams:completion:@
skipAreaWithParams_completionSelector :: Selector '[Id MTRServiceAreaClusterSkipAreaParams, Ptr ()] ()
skipAreaWithParams_completionSelector = mkSelector "skipAreaWithParams:completion:"

-- | @Selector@ for @readAttributeSupportedAreasWithCompletion:@
readAttributeSupportedAreasWithCompletionSelector :: Selector '[Ptr ()] ()
readAttributeSupportedAreasWithCompletionSelector = mkSelector "readAttributeSupportedAreasWithCompletion:"

-- | @Selector@ for @subscribeAttributeSupportedAreasWithParams:subscriptionEstablished:reportHandler:@
subscribeAttributeSupportedAreasWithParams_subscriptionEstablished_reportHandlerSelector :: Selector '[Id MTRSubscribeParams, Ptr (), Ptr ()] ()
subscribeAttributeSupportedAreasWithParams_subscriptionEstablished_reportHandlerSelector = mkSelector "subscribeAttributeSupportedAreasWithParams:subscriptionEstablished:reportHandler:"

-- | @Selector@ for @readAttributeSupportedAreasWithClusterStateCache:endpoint:queue:completion:@
readAttributeSupportedAreasWithClusterStateCache_endpoint_queue_completionSelector :: Selector '[Id MTRClusterStateCacheContainer, Id NSNumber, Id NSObject, Ptr ()] ()
readAttributeSupportedAreasWithClusterStateCache_endpoint_queue_completionSelector = mkSelector "readAttributeSupportedAreasWithClusterStateCache:endpoint:queue:completion:"

-- | @Selector@ for @readAttributeSupportedMapsWithCompletion:@
readAttributeSupportedMapsWithCompletionSelector :: Selector '[Ptr ()] ()
readAttributeSupportedMapsWithCompletionSelector = mkSelector "readAttributeSupportedMapsWithCompletion:"

-- | @Selector@ for @subscribeAttributeSupportedMapsWithParams:subscriptionEstablished:reportHandler:@
subscribeAttributeSupportedMapsWithParams_subscriptionEstablished_reportHandlerSelector :: Selector '[Id MTRSubscribeParams, Ptr (), Ptr ()] ()
subscribeAttributeSupportedMapsWithParams_subscriptionEstablished_reportHandlerSelector = mkSelector "subscribeAttributeSupportedMapsWithParams:subscriptionEstablished:reportHandler:"

-- | @Selector@ for @readAttributeSupportedMapsWithClusterStateCache:endpoint:queue:completion:@
readAttributeSupportedMapsWithClusterStateCache_endpoint_queue_completionSelector :: Selector '[Id MTRClusterStateCacheContainer, Id NSNumber, Id NSObject, Ptr ()] ()
readAttributeSupportedMapsWithClusterStateCache_endpoint_queue_completionSelector = mkSelector "readAttributeSupportedMapsWithClusterStateCache:endpoint:queue:completion:"

-- | @Selector@ for @readAttributeSelectedAreasWithCompletion:@
readAttributeSelectedAreasWithCompletionSelector :: Selector '[Ptr ()] ()
readAttributeSelectedAreasWithCompletionSelector = mkSelector "readAttributeSelectedAreasWithCompletion:"

-- | @Selector@ for @subscribeAttributeSelectedAreasWithParams:subscriptionEstablished:reportHandler:@
subscribeAttributeSelectedAreasWithParams_subscriptionEstablished_reportHandlerSelector :: Selector '[Id MTRSubscribeParams, Ptr (), Ptr ()] ()
subscribeAttributeSelectedAreasWithParams_subscriptionEstablished_reportHandlerSelector = mkSelector "subscribeAttributeSelectedAreasWithParams:subscriptionEstablished:reportHandler:"

-- | @Selector@ for @readAttributeSelectedAreasWithClusterStateCache:endpoint:queue:completion:@
readAttributeSelectedAreasWithClusterStateCache_endpoint_queue_completionSelector :: Selector '[Id MTRClusterStateCacheContainer, Id NSNumber, Id NSObject, Ptr ()] ()
readAttributeSelectedAreasWithClusterStateCache_endpoint_queue_completionSelector = mkSelector "readAttributeSelectedAreasWithClusterStateCache:endpoint:queue:completion:"

-- | @Selector@ for @readAttributeCurrentAreaWithCompletion:@
readAttributeCurrentAreaWithCompletionSelector :: Selector '[Ptr ()] ()
readAttributeCurrentAreaWithCompletionSelector = mkSelector "readAttributeCurrentAreaWithCompletion:"

-- | @Selector@ for @subscribeAttributeCurrentAreaWithParams:subscriptionEstablished:reportHandler:@
subscribeAttributeCurrentAreaWithParams_subscriptionEstablished_reportHandlerSelector :: Selector '[Id MTRSubscribeParams, Ptr (), Ptr ()] ()
subscribeAttributeCurrentAreaWithParams_subscriptionEstablished_reportHandlerSelector = mkSelector "subscribeAttributeCurrentAreaWithParams:subscriptionEstablished:reportHandler:"

-- | @Selector@ for @readAttributeCurrentAreaWithClusterStateCache:endpoint:queue:completion:@
readAttributeCurrentAreaWithClusterStateCache_endpoint_queue_completionSelector :: Selector '[Id MTRClusterStateCacheContainer, Id NSNumber, Id NSObject, Ptr ()] ()
readAttributeCurrentAreaWithClusterStateCache_endpoint_queue_completionSelector = mkSelector "readAttributeCurrentAreaWithClusterStateCache:endpoint:queue:completion:"

-- | @Selector@ for @readAttributeEstimatedEndTimeWithCompletion:@
readAttributeEstimatedEndTimeWithCompletionSelector :: Selector '[Ptr ()] ()
readAttributeEstimatedEndTimeWithCompletionSelector = mkSelector "readAttributeEstimatedEndTimeWithCompletion:"

-- | @Selector@ for @subscribeAttributeEstimatedEndTimeWithParams:subscriptionEstablished:reportHandler:@
subscribeAttributeEstimatedEndTimeWithParams_subscriptionEstablished_reportHandlerSelector :: Selector '[Id MTRSubscribeParams, Ptr (), Ptr ()] ()
subscribeAttributeEstimatedEndTimeWithParams_subscriptionEstablished_reportHandlerSelector = mkSelector "subscribeAttributeEstimatedEndTimeWithParams:subscriptionEstablished:reportHandler:"

-- | @Selector@ for @readAttributeEstimatedEndTimeWithClusterStateCache:endpoint:queue:completion:@
readAttributeEstimatedEndTimeWithClusterStateCache_endpoint_queue_completionSelector :: Selector '[Id MTRClusterStateCacheContainer, Id NSNumber, Id NSObject, Ptr ()] ()
readAttributeEstimatedEndTimeWithClusterStateCache_endpoint_queue_completionSelector = mkSelector "readAttributeEstimatedEndTimeWithClusterStateCache:endpoint:queue:completion:"

-- | @Selector@ for @readAttributeProgressWithCompletion:@
readAttributeProgressWithCompletionSelector :: Selector '[Ptr ()] ()
readAttributeProgressWithCompletionSelector = mkSelector "readAttributeProgressWithCompletion:"

-- | @Selector@ for @subscribeAttributeProgressWithParams:subscriptionEstablished:reportHandler:@
subscribeAttributeProgressWithParams_subscriptionEstablished_reportHandlerSelector :: Selector '[Id MTRSubscribeParams, Ptr (), Ptr ()] ()
subscribeAttributeProgressWithParams_subscriptionEstablished_reportHandlerSelector = mkSelector "subscribeAttributeProgressWithParams:subscriptionEstablished:reportHandler:"

-- | @Selector@ for @readAttributeProgressWithClusterStateCache:endpoint:queue:completion:@
readAttributeProgressWithClusterStateCache_endpoint_queue_completionSelector :: Selector '[Id MTRClusterStateCacheContainer, Id NSNumber, Id NSObject, Ptr ()] ()
readAttributeProgressWithClusterStateCache_endpoint_queue_completionSelector = mkSelector "readAttributeProgressWithClusterStateCache:endpoint:queue:completion:"

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
initSelector :: Selector '[] (Id MTRBaseClusterServiceArea)
initSelector = mkSelector "init"

-- | @Selector@ for @new@
newSelector :: Selector '[] (Id MTRBaseClusterServiceArea)
newSelector = mkSelector "new"

-- | @Selector@ for @initWithDevice:endpointID:queue:@
initWithDevice_endpointID_queueSelector :: Selector '[Id MTRBaseDevice, Id NSNumber, Id NSObject] (Id MTRBaseClusterServiceArea)
initWithDevice_endpointID_queueSelector = mkSelector "initWithDevice:endpointID:queue:"

