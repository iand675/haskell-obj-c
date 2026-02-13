{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Cluster Thread Network Directory
--
-- Manages the names and credentials of Thread networks visible to the user.
--
-- Generated bindings for @MTRBaseClusterThreadNetworkDirectory@.
module ObjC.Matter.MTRBaseClusterThreadNetworkDirectory
  ( MTRBaseClusterThreadNetworkDirectory
  , IsMTRBaseClusterThreadNetworkDirectory(..)
  , addNetworkWithParams_completion
  , removeNetworkWithParams_completion
  , getOperationalDatasetWithParams_completion
  , readAttributePreferredExtendedPanIDWithCompletion
  , writeAttributePreferredExtendedPanIDWithValue_completion
  , writeAttributePreferredExtendedPanIDWithValue_params_completion
  , subscribeAttributePreferredExtendedPanIDWithParams_subscriptionEstablished_reportHandler
  , readAttributePreferredExtendedPanIDWithClusterStateCache_endpoint_queue_completion
  , readAttributeThreadNetworksWithCompletion
  , subscribeAttributeThreadNetworksWithParams_subscriptionEstablished_reportHandler
  , readAttributeThreadNetworksWithClusterStateCache_endpoint_queue_completion
  , readAttributeThreadNetworkTableSizeWithCompletion
  , subscribeAttributeThreadNetworkTableSizeWithParams_subscriptionEstablished_reportHandler
  , readAttributeThreadNetworkTableSizeWithClusterStateCache_endpoint_queue_completion
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
  , addNetworkWithParams_completionSelector
  , getOperationalDatasetWithParams_completionSelector
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
  , readAttributePreferredExtendedPanIDWithClusterStateCache_endpoint_queue_completionSelector
  , readAttributePreferredExtendedPanIDWithCompletionSelector
  , readAttributeThreadNetworkTableSizeWithClusterStateCache_endpoint_queue_completionSelector
  , readAttributeThreadNetworkTableSizeWithCompletionSelector
  , readAttributeThreadNetworksWithClusterStateCache_endpoint_queue_completionSelector
  , readAttributeThreadNetworksWithCompletionSelector
  , removeNetworkWithParams_completionSelector
  , subscribeAttributeAcceptedCommandListWithParams_subscriptionEstablished_reportHandlerSelector
  , subscribeAttributeAttributeListWithParams_subscriptionEstablished_reportHandlerSelector
  , subscribeAttributeClusterRevisionWithParams_subscriptionEstablished_reportHandlerSelector
  , subscribeAttributeFeatureMapWithParams_subscriptionEstablished_reportHandlerSelector
  , subscribeAttributeGeneratedCommandListWithParams_subscriptionEstablished_reportHandlerSelector
  , subscribeAttributePreferredExtendedPanIDWithParams_subscriptionEstablished_reportHandlerSelector
  , subscribeAttributeThreadNetworkTableSizeWithParams_subscriptionEstablished_reportHandlerSelector
  , subscribeAttributeThreadNetworksWithParams_subscriptionEstablished_reportHandlerSelector
  , writeAttributePreferredExtendedPanIDWithValue_completionSelector
  , writeAttributePreferredExtendedPanIDWithValue_params_completionSelector


  ) where

import Foreign.Ptr (Ptr, FunPtr)
import Foreign.C.Types

import ObjC.Runtime.Types
import ObjC.Runtime.Message (sendMessage, sendOwnedMessage, sendClassMessage, sendOwnedClassMessage)
import ObjC.Runtime.Selector (mkSelector)
import ObjC.Runtime.Class (getRequiredClass)

import ObjC.Matter.Internal.Classes
import ObjC.Foundation.Internal.Classes

-- | Command AddNetwork
--
-- Adds an entry to the ThreadNetworks attribute with the specified Thread Operational Dataset.
--
-- ObjC selector: @- addNetworkWithParams:completion:@
addNetworkWithParams_completion :: (IsMTRBaseClusterThreadNetworkDirectory mtrBaseClusterThreadNetworkDirectory, IsMTRThreadNetworkDirectoryClusterAddNetworkParams params) => mtrBaseClusterThreadNetworkDirectory -> params -> Ptr () -> IO ()
addNetworkWithParams_completion mtrBaseClusterThreadNetworkDirectory params completion =
  sendMessage mtrBaseClusterThreadNetworkDirectory addNetworkWithParams_completionSelector (toMTRThreadNetworkDirectoryClusterAddNetworkParams params) completion

-- | Command RemoveNetwork
--
-- Removes the network with the given Extended PAN ID from the ThreadNetworks attribute.
--
-- ObjC selector: @- removeNetworkWithParams:completion:@
removeNetworkWithParams_completion :: (IsMTRBaseClusterThreadNetworkDirectory mtrBaseClusterThreadNetworkDirectory, IsMTRThreadNetworkDirectoryClusterRemoveNetworkParams params) => mtrBaseClusterThreadNetworkDirectory -> params -> Ptr () -> IO ()
removeNetworkWithParams_completion mtrBaseClusterThreadNetworkDirectory params completion =
  sendMessage mtrBaseClusterThreadNetworkDirectory removeNetworkWithParams_completionSelector (toMTRThreadNetworkDirectoryClusterRemoveNetworkParams params) completion

-- | Command GetOperationalDataset
--
-- Retrieves the Thread Operational Dataset with the given Extended PAN ID.
--
-- ObjC selector: @- getOperationalDatasetWithParams:completion:@
getOperationalDatasetWithParams_completion :: (IsMTRBaseClusterThreadNetworkDirectory mtrBaseClusterThreadNetworkDirectory, IsMTRThreadNetworkDirectoryClusterGetOperationalDatasetParams params) => mtrBaseClusterThreadNetworkDirectory -> params -> Ptr () -> IO ()
getOperationalDatasetWithParams_completion mtrBaseClusterThreadNetworkDirectory params completion =
  sendMessage mtrBaseClusterThreadNetworkDirectory getOperationalDatasetWithParams_completionSelector (toMTRThreadNetworkDirectoryClusterGetOperationalDatasetParams params) completion

-- | @- readAttributePreferredExtendedPanIDWithCompletion:@
readAttributePreferredExtendedPanIDWithCompletion :: IsMTRBaseClusterThreadNetworkDirectory mtrBaseClusterThreadNetworkDirectory => mtrBaseClusterThreadNetworkDirectory -> Ptr () -> IO ()
readAttributePreferredExtendedPanIDWithCompletion mtrBaseClusterThreadNetworkDirectory completion =
  sendMessage mtrBaseClusterThreadNetworkDirectory readAttributePreferredExtendedPanIDWithCompletionSelector completion

-- | @- writeAttributePreferredExtendedPanIDWithValue:completion:@
writeAttributePreferredExtendedPanIDWithValue_completion :: (IsMTRBaseClusterThreadNetworkDirectory mtrBaseClusterThreadNetworkDirectory, IsNSData value) => mtrBaseClusterThreadNetworkDirectory -> value -> Ptr () -> IO ()
writeAttributePreferredExtendedPanIDWithValue_completion mtrBaseClusterThreadNetworkDirectory value completion =
  sendMessage mtrBaseClusterThreadNetworkDirectory writeAttributePreferredExtendedPanIDWithValue_completionSelector (toNSData value) completion

-- | @- writeAttributePreferredExtendedPanIDWithValue:params:completion:@
writeAttributePreferredExtendedPanIDWithValue_params_completion :: (IsMTRBaseClusterThreadNetworkDirectory mtrBaseClusterThreadNetworkDirectory, IsNSData value, IsMTRWriteParams params) => mtrBaseClusterThreadNetworkDirectory -> value -> params -> Ptr () -> IO ()
writeAttributePreferredExtendedPanIDWithValue_params_completion mtrBaseClusterThreadNetworkDirectory value params completion =
  sendMessage mtrBaseClusterThreadNetworkDirectory writeAttributePreferredExtendedPanIDWithValue_params_completionSelector (toNSData value) (toMTRWriteParams params) completion

-- | @- subscribeAttributePreferredExtendedPanIDWithParams:subscriptionEstablished:reportHandler:@
subscribeAttributePreferredExtendedPanIDWithParams_subscriptionEstablished_reportHandler :: (IsMTRBaseClusterThreadNetworkDirectory mtrBaseClusterThreadNetworkDirectory, IsMTRSubscribeParams params) => mtrBaseClusterThreadNetworkDirectory -> params -> Ptr () -> Ptr () -> IO ()
subscribeAttributePreferredExtendedPanIDWithParams_subscriptionEstablished_reportHandler mtrBaseClusterThreadNetworkDirectory params subscriptionEstablished reportHandler =
  sendMessage mtrBaseClusterThreadNetworkDirectory subscribeAttributePreferredExtendedPanIDWithParams_subscriptionEstablished_reportHandlerSelector (toMTRSubscribeParams params) subscriptionEstablished reportHandler

-- | @+ readAttributePreferredExtendedPanIDWithClusterStateCache:endpoint:queue:completion:@
readAttributePreferredExtendedPanIDWithClusterStateCache_endpoint_queue_completion :: (IsMTRClusterStateCacheContainer clusterStateCacheContainer, IsNSNumber endpoint, IsNSObject queue) => clusterStateCacheContainer -> endpoint -> queue -> Ptr () -> IO ()
readAttributePreferredExtendedPanIDWithClusterStateCache_endpoint_queue_completion clusterStateCacheContainer endpoint queue completion =
  do
    cls' <- getRequiredClass "MTRBaseClusterThreadNetworkDirectory"
    sendClassMessage cls' readAttributePreferredExtendedPanIDWithClusterStateCache_endpoint_queue_completionSelector (toMTRClusterStateCacheContainer clusterStateCacheContainer) (toNSNumber endpoint) (toNSObject queue) completion

-- | @- readAttributeThreadNetworksWithCompletion:@
readAttributeThreadNetworksWithCompletion :: IsMTRBaseClusterThreadNetworkDirectory mtrBaseClusterThreadNetworkDirectory => mtrBaseClusterThreadNetworkDirectory -> Ptr () -> IO ()
readAttributeThreadNetworksWithCompletion mtrBaseClusterThreadNetworkDirectory completion =
  sendMessage mtrBaseClusterThreadNetworkDirectory readAttributeThreadNetworksWithCompletionSelector completion

-- | @- subscribeAttributeThreadNetworksWithParams:subscriptionEstablished:reportHandler:@
subscribeAttributeThreadNetworksWithParams_subscriptionEstablished_reportHandler :: (IsMTRBaseClusterThreadNetworkDirectory mtrBaseClusterThreadNetworkDirectory, IsMTRSubscribeParams params) => mtrBaseClusterThreadNetworkDirectory -> params -> Ptr () -> Ptr () -> IO ()
subscribeAttributeThreadNetworksWithParams_subscriptionEstablished_reportHandler mtrBaseClusterThreadNetworkDirectory params subscriptionEstablished reportHandler =
  sendMessage mtrBaseClusterThreadNetworkDirectory subscribeAttributeThreadNetworksWithParams_subscriptionEstablished_reportHandlerSelector (toMTRSubscribeParams params) subscriptionEstablished reportHandler

-- | @+ readAttributeThreadNetworksWithClusterStateCache:endpoint:queue:completion:@
readAttributeThreadNetworksWithClusterStateCache_endpoint_queue_completion :: (IsMTRClusterStateCacheContainer clusterStateCacheContainer, IsNSNumber endpoint, IsNSObject queue) => clusterStateCacheContainer -> endpoint -> queue -> Ptr () -> IO ()
readAttributeThreadNetworksWithClusterStateCache_endpoint_queue_completion clusterStateCacheContainer endpoint queue completion =
  do
    cls' <- getRequiredClass "MTRBaseClusterThreadNetworkDirectory"
    sendClassMessage cls' readAttributeThreadNetworksWithClusterStateCache_endpoint_queue_completionSelector (toMTRClusterStateCacheContainer clusterStateCacheContainer) (toNSNumber endpoint) (toNSObject queue) completion

-- | @- readAttributeThreadNetworkTableSizeWithCompletion:@
readAttributeThreadNetworkTableSizeWithCompletion :: IsMTRBaseClusterThreadNetworkDirectory mtrBaseClusterThreadNetworkDirectory => mtrBaseClusterThreadNetworkDirectory -> Ptr () -> IO ()
readAttributeThreadNetworkTableSizeWithCompletion mtrBaseClusterThreadNetworkDirectory completion =
  sendMessage mtrBaseClusterThreadNetworkDirectory readAttributeThreadNetworkTableSizeWithCompletionSelector completion

-- | @- subscribeAttributeThreadNetworkTableSizeWithParams:subscriptionEstablished:reportHandler:@
subscribeAttributeThreadNetworkTableSizeWithParams_subscriptionEstablished_reportHandler :: (IsMTRBaseClusterThreadNetworkDirectory mtrBaseClusterThreadNetworkDirectory, IsMTRSubscribeParams params) => mtrBaseClusterThreadNetworkDirectory -> params -> Ptr () -> Ptr () -> IO ()
subscribeAttributeThreadNetworkTableSizeWithParams_subscriptionEstablished_reportHandler mtrBaseClusterThreadNetworkDirectory params subscriptionEstablished reportHandler =
  sendMessage mtrBaseClusterThreadNetworkDirectory subscribeAttributeThreadNetworkTableSizeWithParams_subscriptionEstablished_reportHandlerSelector (toMTRSubscribeParams params) subscriptionEstablished reportHandler

-- | @+ readAttributeThreadNetworkTableSizeWithClusterStateCache:endpoint:queue:completion:@
readAttributeThreadNetworkTableSizeWithClusterStateCache_endpoint_queue_completion :: (IsMTRClusterStateCacheContainer clusterStateCacheContainer, IsNSNumber endpoint, IsNSObject queue) => clusterStateCacheContainer -> endpoint -> queue -> Ptr () -> IO ()
readAttributeThreadNetworkTableSizeWithClusterStateCache_endpoint_queue_completion clusterStateCacheContainer endpoint queue completion =
  do
    cls' <- getRequiredClass "MTRBaseClusterThreadNetworkDirectory"
    sendClassMessage cls' readAttributeThreadNetworkTableSizeWithClusterStateCache_endpoint_queue_completionSelector (toMTRClusterStateCacheContainer clusterStateCacheContainer) (toNSNumber endpoint) (toNSObject queue) completion

-- | @- readAttributeGeneratedCommandListWithCompletion:@
readAttributeGeneratedCommandListWithCompletion :: IsMTRBaseClusterThreadNetworkDirectory mtrBaseClusterThreadNetworkDirectory => mtrBaseClusterThreadNetworkDirectory -> Ptr () -> IO ()
readAttributeGeneratedCommandListWithCompletion mtrBaseClusterThreadNetworkDirectory completion =
  sendMessage mtrBaseClusterThreadNetworkDirectory readAttributeGeneratedCommandListWithCompletionSelector completion

-- | @- subscribeAttributeGeneratedCommandListWithParams:subscriptionEstablished:reportHandler:@
subscribeAttributeGeneratedCommandListWithParams_subscriptionEstablished_reportHandler :: (IsMTRBaseClusterThreadNetworkDirectory mtrBaseClusterThreadNetworkDirectory, IsMTRSubscribeParams params) => mtrBaseClusterThreadNetworkDirectory -> params -> Ptr () -> Ptr () -> IO ()
subscribeAttributeGeneratedCommandListWithParams_subscriptionEstablished_reportHandler mtrBaseClusterThreadNetworkDirectory params subscriptionEstablished reportHandler =
  sendMessage mtrBaseClusterThreadNetworkDirectory subscribeAttributeGeneratedCommandListWithParams_subscriptionEstablished_reportHandlerSelector (toMTRSubscribeParams params) subscriptionEstablished reportHandler

-- | @+ readAttributeGeneratedCommandListWithClusterStateCache:endpoint:queue:completion:@
readAttributeGeneratedCommandListWithClusterStateCache_endpoint_queue_completion :: (IsMTRClusterStateCacheContainer clusterStateCacheContainer, IsNSNumber endpoint, IsNSObject queue) => clusterStateCacheContainer -> endpoint -> queue -> Ptr () -> IO ()
readAttributeGeneratedCommandListWithClusterStateCache_endpoint_queue_completion clusterStateCacheContainer endpoint queue completion =
  do
    cls' <- getRequiredClass "MTRBaseClusterThreadNetworkDirectory"
    sendClassMessage cls' readAttributeGeneratedCommandListWithClusterStateCache_endpoint_queue_completionSelector (toMTRClusterStateCacheContainer clusterStateCacheContainer) (toNSNumber endpoint) (toNSObject queue) completion

-- | @- readAttributeAcceptedCommandListWithCompletion:@
readAttributeAcceptedCommandListWithCompletion :: IsMTRBaseClusterThreadNetworkDirectory mtrBaseClusterThreadNetworkDirectory => mtrBaseClusterThreadNetworkDirectory -> Ptr () -> IO ()
readAttributeAcceptedCommandListWithCompletion mtrBaseClusterThreadNetworkDirectory completion =
  sendMessage mtrBaseClusterThreadNetworkDirectory readAttributeAcceptedCommandListWithCompletionSelector completion

-- | @- subscribeAttributeAcceptedCommandListWithParams:subscriptionEstablished:reportHandler:@
subscribeAttributeAcceptedCommandListWithParams_subscriptionEstablished_reportHandler :: (IsMTRBaseClusterThreadNetworkDirectory mtrBaseClusterThreadNetworkDirectory, IsMTRSubscribeParams params) => mtrBaseClusterThreadNetworkDirectory -> params -> Ptr () -> Ptr () -> IO ()
subscribeAttributeAcceptedCommandListWithParams_subscriptionEstablished_reportHandler mtrBaseClusterThreadNetworkDirectory params subscriptionEstablished reportHandler =
  sendMessage mtrBaseClusterThreadNetworkDirectory subscribeAttributeAcceptedCommandListWithParams_subscriptionEstablished_reportHandlerSelector (toMTRSubscribeParams params) subscriptionEstablished reportHandler

-- | @+ readAttributeAcceptedCommandListWithClusterStateCache:endpoint:queue:completion:@
readAttributeAcceptedCommandListWithClusterStateCache_endpoint_queue_completion :: (IsMTRClusterStateCacheContainer clusterStateCacheContainer, IsNSNumber endpoint, IsNSObject queue) => clusterStateCacheContainer -> endpoint -> queue -> Ptr () -> IO ()
readAttributeAcceptedCommandListWithClusterStateCache_endpoint_queue_completion clusterStateCacheContainer endpoint queue completion =
  do
    cls' <- getRequiredClass "MTRBaseClusterThreadNetworkDirectory"
    sendClassMessage cls' readAttributeAcceptedCommandListWithClusterStateCache_endpoint_queue_completionSelector (toMTRClusterStateCacheContainer clusterStateCacheContainer) (toNSNumber endpoint) (toNSObject queue) completion

-- | @- readAttributeAttributeListWithCompletion:@
readAttributeAttributeListWithCompletion :: IsMTRBaseClusterThreadNetworkDirectory mtrBaseClusterThreadNetworkDirectory => mtrBaseClusterThreadNetworkDirectory -> Ptr () -> IO ()
readAttributeAttributeListWithCompletion mtrBaseClusterThreadNetworkDirectory completion =
  sendMessage mtrBaseClusterThreadNetworkDirectory readAttributeAttributeListWithCompletionSelector completion

-- | @- subscribeAttributeAttributeListWithParams:subscriptionEstablished:reportHandler:@
subscribeAttributeAttributeListWithParams_subscriptionEstablished_reportHandler :: (IsMTRBaseClusterThreadNetworkDirectory mtrBaseClusterThreadNetworkDirectory, IsMTRSubscribeParams params) => mtrBaseClusterThreadNetworkDirectory -> params -> Ptr () -> Ptr () -> IO ()
subscribeAttributeAttributeListWithParams_subscriptionEstablished_reportHandler mtrBaseClusterThreadNetworkDirectory params subscriptionEstablished reportHandler =
  sendMessage mtrBaseClusterThreadNetworkDirectory subscribeAttributeAttributeListWithParams_subscriptionEstablished_reportHandlerSelector (toMTRSubscribeParams params) subscriptionEstablished reportHandler

-- | @+ readAttributeAttributeListWithClusterStateCache:endpoint:queue:completion:@
readAttributeAttributeListWithClusterStateCache_endpoint_queue_completion :: (IsMTRClusterStateCacheContainer clusterStateCacheContainer, IsNSNumber endpoint, IsNSObject queue) => clusterStateCacheContainer -> endpoint -> queue -> Ptr () -> IO ()
readAttributeAttributeListWithClusterStateCache_endpoint_queue_completion clusterStateCacheContainer endpoint queue completion =
  do
    cls' <- getRequiredClass "MTRBaseClusterThreadNetworkDirectory"
    sendClassMessage cls' readAttributeAttributeListWithClusterStateCache_endpoint_queue_completionSelector (toMTRClusterStateCacheContainer clusterStateCacheContainer) (toNSNumber endpoint) (toNSObject queue) completion

-- | @- readAttributeFeatureMapWithCompletion:@
readAttributeFeatureMapWithCompletion :: IsMTRBaseClusterThreadNetworkDirectory mtrBaseClusterThreadNetworkDirectory => mtrBaseClusterThreadNetworkDirectory -> Ptr () -> IO ()
readAttributeFeatureMapWithCompletion mtrBaseClusterThreadNetworkDirectory completion =
  sendMessage mtrBaseClusterThreadNetworkDirectory readAttributeFeatureMapWithCompletionSelector completion

-- | @- subscribeAttributeFeatureMapWithParams:subscriptionEstablished:reportHandler:@
subscribeAttributeFeatureMapWithParams_subscriptionEstablished_reportHandler :: (IsMTRBaseClusterThreadNetworkDirectory mtrBaseClusterThreadNetworkDirectory, IsMTRSubscribeParams params) => mtrBaseClusterThreadNetworkDirectory -> params -> Ptr () -> Ptr () -> IO ()
subscribeAttributeFeatureMapWithParams_subscriptionEstablished_reportHandler mtrBaseClusterThreadNetworkDirectory params subscriptionEstablished reportHandler =
  sendMessage mtrBaseClusterThreadNetworkDirectory subscribeAttributeFeatureMapWithParams_subscriptionEstablished_reportHandlerSelector (toMTRSubscribeParams params) subscriptionEstablished reportHandler

-- | @+ readAttributeFeatureMapWithClusterStateCache:endpoint:queue:completion:@
readAttributeFeatureMapWithClusterStateCache_endpoint_queue_completion :: (IsMTRClusterStateCacheContainer clusterStateCacheContainer, IsNSNumber endpoint, IsNSObject queue) => clusterStateCacheContainer -> endpoint -> queue -> Ptr () -> IO ()
readAttributeFeatureMapWithClusterStateCache_endpoint_queue_completion clusterStateCacheContainer endpoint queue completion =
  do
    cls' <- getRequiredClass "MTRBaseClusterThreadNetworkDirectory"
    sendClassMessage cls' readAttributeFeatureMapWithClusterStateCache_endpoint_queue_completionSelector (toMTRClusterStateCacheContainer clusterStateCacheContainer) (toNSNumber endpoint) (toNSObject queue) completion

-- | @- readAttributeClusterRevisionWithCompletion:@
readAttributeClusterRevisionWithCompletion :: IsMTRBaseClusterThreadNetworkDirectory mtrBaseClusterThreadNetworkDirectory => mtrBaseClusterThreadNetworkDirectory -> Ptr () -> IO ()
readAttributeClusterRevisionWithCompletion mtrBaseClusterThreadNetworkDirectory completion =
  sendMessage mtrBaseClusterThreadNetworkDirectory readAttributeClusterRevisionWithCompletionSelector completion

-- | @- subscribeAttributeClusterRevisionWithParams:subscriptionEstablished:reportHandler:@
subscribeAttributeClusterRevisionWithParams_subscriptionEstablished_reportHandler :: (IsMTRBaseClusterThreadNetworkDirectory mtrBaseClusterThreadNetworkDirectory, IsMTRSubscribeParams params) => mtrBaseClusterThreadNetworkDirectory -> params -> Ptr () -> Ptr () -> IO ()
subscribeAttributeClusterRevisionWithParams_subscriptionEstablished_reportHandler mtrBaseClusterThreadNetworkDirectory params subscriptionEstablished reportHandler =
  sendMessage mtrBaseClusterThreadNetworkDirectory subscribeAttributeClusterRevisionWithParams_subscriptionEstablished_reportHandlerSelector (toMTRSubscribeParams params) subscriptionEstablished reportHandler

-- | @+ readAttributeClusterRevisionWithClusterStateCache:endpoint:queue:completion:@
readAttributeClusterRevisionWithClusterStateCache_endpoint_queue_completion :: (IsMTRClusterStateCacheContainer clusterStateCacheContainer, IsNSNumber endpoint, IsNSObject queue) => clusterStateCacheContainer -> endpoint -> queue -> Ptr () -> IO ()
readAttributeClusterRevisionWithClusterStateCache_endpoint_queue_completion clusterStateCacheContainer endpoint queue completion =
  do
    cls' <- getRequiredClass "MTRBaseClusterThreadNetworkDirectory"
    sendClassMessage cls' readAttributeClusterRevisionWithClusterStateCache_endpoint_queue_completionSelector (toMTRClusterStateCacheContainer clusterStateCacheContainer) (toNSNumber endpoint) (toNSObject queue) completion

-- | @- init@
init_ :: IsMTRBaseClusterThreadNetworkDirectory mtrBaseClusterThreadNetworkDirectory => mtrBaseClusterThreadNetworkDirectory -> IO (Id MTRBaseClusterThreadNetworkDirectory)
init_ mtrBaseClusterThreadNetworkDirectory =
  sendOwnedMessage mtrBaseClusterThreadNetworkDirectory initSelector

-- | @+ new@
new :: IO (Id MTRBaseClusterThreadNetworkDirectory)
new  =
  do
    cls' <- getRequiredClass "MTRBaseClusterThreadNetworkDirectory"
    sendOwnedClassMessage cls' newSelector

-- | For all instance methods (reads, writes, commands) that take a completion, the completion will be called on the provided queue.
--
-- ObjC selector: @- initWithDevice:endpointID:queue:@
initWithDevice_endpointID_queue :: (IsMTRBaseClusterThreadNetworkDirectory mtrBaseClusterThreadNetworkDirectory, IsMTRBaseDevice device, IsNSNumber endpointID, IsNSObject queue) => mtrBaseClusterThreadNetworkDirectory -> device -> endpointID -> queue -> IO (Id MTRBaseClusterThreadNetworkDirectory)
initWithDevice_endpointID_queue mtrBaseClusterThreadNetworkDirectory device endpointID queue =
  sendOwnedMessage mtrBaseClusterThreadNetworkDirectory initWithDevice_endpointID_queueSelector (toMTRBaseDevice device) (toNSNumber endpointID) (toNSObject queue)

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @addNetworkWithParams:completion:@
addNetworkWithParams_completionSelector :: Selector '[Id MTRThreadNetworkDirectoryClusterAddNetworkParams, Ptr ()] ()
addNetworkWithParams_completionSelector = mkSelector "addNetworkWithParams:completion:"

-- | @Selector@ for @removeNetworkWithParams:completion:@
removeNetworkWithParams_completionSelector :: Selector '[Id MTRThreadNetworkDirectoryClusterRemoveNetworkParams, Ptr ()] ()
removeNetworkWithParams_completionSelector = mkSelector "removeNetworkWithParams:completion:"

-- | @Selector@ for @getOperationalDatasetWithParams:completion:@
getOperationalDatasetWithParams_completionSelector :: Selector '[Id MTRThreadNetworkDirectoryClusterGetOperationalDatasetParams, Ptr ()] ()
getOperationalDatasetWithParams_completionSelector = mkSelector "getOperationalDatasetWithParams:completion:"

-- | @Selector@ for @readAttributePreferredExtendedPanIDWithCompletion:@
readAttributePreferredExtendedPanIDWithCompletionSelector :: Selector '[Ptr ()] ()
readAttributePreferredExtendedPanIDWithCompletionSelector = mkSelector "readAttributePreferredExtendedPanIDWithCompletion:"

-- | @Selector@ for @writeAttributePreferredExtendedPanIDWithValue:completion:@
writeAttributePreferredExtendedPanIDWithValue_completionSelector :: Selector '[Id NSData, Ptr ()] ()
writeAttributePreferredExtendedPanIDWithValue_completionSelector = mkSelector "writeAttributePreferredExtendedPanIDWithValue:completion:"

-- | @Selector@ for @writeAttributePreferredExtendedPanIDWithValue:params:completion:@
writeAttributePreferredExtendedPanIDWithValue_params_completionSelector :: Selector '[Id NSData, Id MTRWriteParams, Ptr ()] ()
writeAttributePreferredExtendedPanIDWithValue_params_completionSelector = mkSelector "writeAttributePreferredExtendedPanIDWithValue:params:completion:"

-- | @Selector@ for @subscribeAttributePreferredExtendedPanIDWithParams:subscriptionEstablished:reportHandler:@
subscribeAttributePreferredExtendedPanIDWithParams_subscriptionEstablished_reportHandlerSelector :: Selector '[Id MTRSubscribeParams, Ptr (), Ptr ()] ()
subscribeAttributePreferredExtendedPanIDWithParams_subscriptionEstablished_reportHandlerSelector = mkSelector "subscribeAttributePreferredExtendedPanIDWithParams:subscriptionEstablished:reportHandler:"

-- | @Selector@ for @readAttributePreferredExtendedPanIDWithClusterStateCache:endpoint:queue:completion:@
readAttributePreferredExtendedPanIDWithClusterStateCache_endpoint_queue_completionSelector :: Selector '[Id MTRClusterStateCacheContainer, Id NSNumber, Id NSObject, Ptr ()] ()
readAttributePreferredExtendedPanIDWithClusterStateCache_endpoint_queue_completionSelector = mkSelector "readAttributePreferredExtendedPanIDWithClusterStateCache:endpoint:queue:completion:"

-- | @Selector@ for @readAttributeThreadNetworksWithCompletion:@
readAttributeThreadNetworksWithCompletionSelector :: Selector '[Ptr ()] ()
readAttributeThreadNetworksWithCompletionSelector = mkSelector "readAttributeThreadNetworksWithCompletion:"

-- | @Selector@ for @subscribeAttributeThreadNetworksWithParams:subscriptionEstablished:reportHandler:@
subscribeAttributeThreadNetworksWithParams_subscriptionEstablished_reportHandlerSelector :: Selector '[Id MTRSubscribeParams, Ptr (), Ptr ()] ()
subscribeAttributeThreadNetworksWithParams_subscriptionEstablished_reportHandlerSelector = mkSelector "subscribeAttributeThreadNetworksWithParams:subscriptionEstablished:reportHandler:"

-- | @Selector@ for @readAttributeThreadNetworksWithClusterStateCache:endpoint:queue:completion:@
readAttributeThreadNetworksWithClusterStateCache_endpoint_queue_completionSelector :: Selector '[Id MTRClusterStateCacheContainer, Id NSNumber, Id NSObject, Ptr ()] ()
readAttributeThreadNetworksWithClusterStateCache_endpoint_queue_completionSelector = mkSelector "readAttributeThreadNetworksWithClusterStateCache:endpoint:queue:completion:"

-- | @Selector@ for @readAttributeThreadNetworkTableSizeWithCompletion:@
readAttributeThreadNetworkTableSizeWithCompletionSelector :: Selector '[Ptr ()] ()
readAttributeThreadNetworkTableSizeWithCompletionSelector = mkSelector "readAttributeThreadNetworkTableSizeWithCompletion:"

-- | @Selector@ for @subscribeAttributeThreadNetworkTableSizeWithParams:subscriptionEstablished:reportHandler:@
subscribeAttributeThreadNetworkTableSizeWithParams_subscriptionEstablished_reportHandlerSelector :: Selector '[Id MTRSubscribeParams, Ptr (), Ptr ()] ()
subscribeAttributeThreadNetworkTableSizeWithParams_subscriptionEstablished_reportHandlerSelector = mkSelector "subscribeAttributeThreadNetworkTableSizeWithParams:subscriptionEstablished:reportHandler:"

-- | @Selector@ for @readAttributeThreadNetworkTableSizeWithClusterStateCache:endpoint:queue:completion:@
readAttributeThreadNetworkTableSizeWithClusterStateCache_endpoint_queue_completionSelector :: Selector '[Id MTRClusterStateCacheContainer, Id NSNumber, Id NSObject, Ptr ()] ()
readAttributeThreadNetworkTableSizeWithClusterStateCache_endpoint_queue_completionSelector = mkSelector "readAttributeThreadNetworkTableSizeWithClusterStateCache:endpoint:queue:completion:"

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
initSelector :: Selector '[] (Id MTRBaseClusterThreadNetworkDirectory)
initSelector = mkSelector "init"

-- | @Selector@ for @new@
newSelector :: Selector '[] (Id MTRBaseClusterThreadNetworkDirectory)
newSelector = mkSelector "new"

-- | @Selector@ for @initWithDevice:endpointID:queue:@
initWithDevice_endpointID_queueSelector :: Selector '[Id MTRBaseDevice, Id NSNumber, Id NSObject] (Id MTRBaseClusterThreadNetworkDirectory)
initWithDevice_endpointID_queueSelector = mkSelector "initWithDevice:endpointID:queue:"

