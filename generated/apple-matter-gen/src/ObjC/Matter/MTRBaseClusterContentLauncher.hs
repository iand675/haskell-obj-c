{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Cluster Content Launcher
--
-- This cluster provides an interface for launching content on a media player device such as a TV or Speaker.
--
-- Generated bindings for @MTRBaseClusterContentLauncher@.
module ObjC.Matter.MTRBaseClusterContentLauncher
  ( MTRBaseClusterContentLauncher
  , IsMTRBaseClusterContentLauncher(..)
  , launchContentWithParams_completion
  , launchURLWithParams_completion
  , readAttributeAcceptHeaderWithCompletion
  , subscribeAttributeAcceptHeaderWithParams_subscriptionEstablished_reportHandler
  , readAttributeAcceptHeaderWithClusterStateCache_endpoint_queue_completion
  , readAttributeSupportedStreamingProtocolsWithCompletion
  , writeAttributeSupportedStreamingProtocolsWithValue_completion
  , writeAttributeSupportedStreamingProtocolsWithValue_params_completion
  , subscribeAttributeSupportedStreamingProtocolsWithParams_subscriptionEstablished_reportHandler
  , readAttributeSupportedStreamingProtocolsWithClusterStateCache_endpoint_queue_completion
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
  , launchContentWithParams_completionHandler
  , launchURLWithParams_completionHandler
  , readAttributeAcceptHeaderWithCompletionHandler
  , subscribeAttributeAcceptHeaderWithMinInterval_maxInterval_params_subscriptionEstablished_reportHandler
  , readAttributeAcceptHeaderWithAttributeCache_endpoint_queue_completionHandler
  , readAttributeSupportedStreamingProtocolsWithCompletionHandler
  , writeAttributeSupportedStreamingProtocolsWithValue_completionHandler
  , writeAttributeSupportedStreamingProtocolsWithValue_params_completionHandler
  , subscribeAttributeSupportedStreamingProtocolsWithMinInterval_maxInterval_params_subscriptionEstablished_reportHandler
  , readAttributeSupportedStreamingProtocolsWithAttributeCache_endpoint_queue_completionHandler
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
  , launchContentWithParams_completionHandlerSelector
  , launchContentWithParams_completionSelector
  , launchURLWithParams_completionHandlerSelector
  , launchURLWithParams_completionSelector
  , newSelector
  , readAttributeAcceptHeaderWithAttributeCache_endpoint_queue_completionHandlerSelector
  , readAttributeAcceptHeaderWithClusterStateCache_endpoint_queue_completionSelector
  , readAttributeAcceptHeaderWithCompletionHandlerSelector
  , readAttributeAcceptHeaderWithCompletionSelector
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
  , readAttributeSupportedStreamingProtocolsWithAttributeCache_endpoint_queue_completionHandlerSelector
  , readAttributeSupportedStreamingProtocolsWithClusterStateCache_endpoint_queue_completionSelector
  , readAttributeSupportedStreamingProtocolsWithCompletionHandlerSelector
  , readAttributeSupportedStreamingProtocolsWithCompletionSelector
  , subscribeAttributeAcceptHeaderWithMinInterval_maxInterval_params_subscriptionEstablished_reportHandlerSelector
  , subscribeAttributeAcceptHeaderWithParams_subscriptionEstablished_reportHandlerSelector
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
  , subscribeAttributeSupportedStreamingProtocolsWithMinInterval_maxInterval_params_subscriptionEstablished_reportHandlerSelector
  , subscribeAttributeSupportedStreamingProtocolsWithParams_subscriptionEstablished_reportHandlerSelector
  , writeAttributeSupportedStreamingProtocolsWithValue_completionHandlerSelector
  , writeAttributeSupportedStreamingProtocolsWithValue_completionSelector
  , writeAttributeSupportedStreamingProtocolsWithValue_params_completionHandlerSelector
  , writeAttributeSupportedStreamingProtocolsWithValue_params_completionSelector


  ) where

import Foreign.Ptr (Ptr, FunPtr)
import Foreign.C.Types

import ObjC.Runtime.Types
import ObjC.Runtime.Message (sendMessage, sendOwnedMessage, sendClassMessage, sendOwnedClassMessage)
import ObjC.Runtime.Selector (mkSelector)
import ObjC.Runtime.Class (getRequiredClass)

import ObjC.Matter.Internal.Classes
import ObjC.Foundation.Internal.Classes

-- | Command LaunchContent
--
-- Upon receipt, this SHALL launch the specified content with optional search criteria.
--
-- ObjC selector: @- launchContentWithParams:completion:@
launchContentWithParams_completion :: (IsMTRBaseClusterContentLauncher mtrBaseClusterContentLauncher, IsMTRContentLauncherClusterLaunchContentParams params) => mtrBaseClusterContentLauncher -> params -> Ptr () -> IO ()
launchContentWithParams_completion mtrBaseClusterContentLauncher params completion =
  sendMessage mtrBaseClusterContentLauncher launchContentWithParams_completionSelector (toMTRContentLauncherClusterLaunchContentParams params) completion

-- | Command LaunchURL
--
-- Upon receipt, this SHALL launch content from the specified URL.
--
-- ObjC selector: @- launchURLWithParams:completion:@
launchURLWithParams_completion :: (IsMTRBaseClusterContentLauncher mtrBaseClusterContentLauncher, IsMTRContentLauncherClusterLaunchURLParams params) => mtrBaseClusterContentLauncher -> params -> Ptr () -> IO ()
launchURLWithParams_completion mtrBaseClusterContentLauncher params completion =
  sendMessage mtrBaseClusterContentLauncher launchURLWithParams_completionSelector (toMTRContentLauncherClusterLaunchURLParams params) completion

-- | @- readAttributeAcceptHeaderWithCompletion:@
readAttributeAcceptHeaderWithCompletion :: IsMTRBaseClusterContentLauncher mtrBaseClusterContentLauncher => mtrBaseClusterContentLauncher -> Ptr () -> IO ()
readAttributeAcceptHeaderWithCompletion mtrBaseClusterContentLauncher completion =
  sendMessage mtrBaseClusterContentLauncher readAttributeAcceptHeaderWithCompletionSelector completion

-- | @- subscribeAttributeAcceptHeaderWithParams:subscriptionEstablished:reportHandler:@
subscribeAttributeAcceptHeaderWithParams_subscriptionEstablished_reportHandler :: (IsMTRBaseClusterContentLauncher mtrBaseClusterContentLauncher, IsMTRSubscribeParams params) => mtrBaseClusterContentLauncher -> params -> Ptr () -> Ptr () -> IO ()
subscribeAttributeAcceptHeaderWithParams_subscriptionEstablished_reportHandler mtrBaseClusterContentLauncher params subscriptionEstablished reportHandler =
  sendMessage mtrBaseClusterContentLauncher subscribeAttributeAcceptHeaderWithParams_subscriptionEstablished_reportHandlerSelector (toMTRSubscribeParams params) subscriptionEstablished reportHandler

-- | @+ readAttributeAcceptHeaderWithClusterStateCache:endpoint:queue:completion:@
readAttributeAcceptHeaderWithClusterStateCache_endpoint_queue_completion :: (IsMTRClusterStateCacheContainer clusterStateCacheContainer, IsNSNumber endpoint, IsNSObject queue) => clusterStateCacheContainer -> endpoint -> queue -> Ptr () -> IO ()
readAttributeAcceptHeaderWithClusterStateCache_endpoint_queue_completion clusterStateCacheContainer endpoint queue completion =
  do
    cls' <- getRequiredClass "MTRBaseClusterContentLauncher"
    sendClassMessage cls' readAttributeAcceptHeaderWithClusterStateCache_endpoint_queue_completionSelector (toMTRClusterStateCacheContainer clusterStateCacheContainer) (toNSNumber endpoint) (toNSObject queue) completion

-- | @- readAttributeSupportedStreamingProtocolsWithCompletion:@
readAttributeSupportedStreamingProtocolsWithCompletion :: IsMTRBaseClusterContentLauncher mtrBaseClusterContentLauncher => mtrBaseClusterContentLauncher -> Ptr () -> IO ()
readAttributeSupportedStreamingProtocolsWithCompletion mtrBaseClusterContentLauncher completion =
  sendMessage mtrBaseClusterContentLauncher readAttributeSupportedStreamingProtocolsWithCompletionSelector completion

-- | @- writeAttributeSupportedStreamingProtocolsWithValue:completion:@
writeAttributeSupportedStreamingProtocolsWithValue_completion :: (IsMTRBaseClusterContentLauncher mtrBaseClusterContentLauncher, IsNSNumber value) => mtrBaseClusterContentLauncher -> value -> Ptr () -> IO ()
writeAttributeSupportedStreamingProtocolsWithValue_completion mtrBaseClusterContentLauncher value completion =
  sendMessage mtrBaseClusterContentLauncher writeAttributeSupportedStreamingProtocolsWithValue_completionSelector (toNSNumber value) completion

-- | @- writeAttributeSupportedStreamingProtocolsWithValue:params:completion:@
writeAttributeSupportedStreamingProtocolsWithValue_params_completion :: (IsMTRBaseClusterContentLauncher mtrBaseClusterContentLauncher, IsNSNumber value, IsMTRWriteParams params) => mtrBaseClusterContentLauncher -> value -> params -> Ptr () -> IO ()
writeAttributeSupportedStreamingProtocolsWithValue_params_completion mtrBaseClusterContentLauncher value params completion =
  sendMessage mtrBaseClusterContentLauncher writeAttributeSupportedStreamingProtocolsWithValue_params_completionSelector (toNSNumber value) (toMTRWriteParams params) completion

-- | @- subscribeAttributeSupportedStreamingProtocolsWithParams:subscriptionEstablished:reportHandler:@
subscribeAttributeSupportedStreamingProtocolsWithParams_subscriptionEstablished_reportHandler :: (IsMTRBaseClusterContentLauncher mtrBaseClusterContentLauncher, IsMTRSubscribeParams params) => mtrBaseClusterContentLauncher -> params -> Ptr () -> Ptr () -> IO ()
subscribeAttributeSupportedStreamingProtocolsWithParams_subscriptionEstablished_reportHandler mtrBaseClusterContentLauncher params subscriptionEstablished reportHandler =
  sendMessage mtrBaseClusterContentLauncher subscribeAttributeSupportedStreamingProtocolsWithParams_subscriptionEstablished_reportHandlerSelector (toMTRSubscribeParams params) subscriptionEstablished reportHandler

-- | @+ readAttributeSupportedStreamingProtocolsWithClusterStateCache:endpoint:queue:completion:@
readAttributeSupportedStreamingProtocolsWithClusterStateCache_endpoint_queue_completion :: (IsMTRClusterStateCacheContainer clusterStateCacheContainer, IsNSNumber endpoint, IsNSObject queue) => clusterStateCacheContainer -> endpoint -> queue -> Ptr () -> IO ()
readAttributeSupportedStreamingProtocolsWithClusterStateCache_endpoint_queue_completion clusterStateCacheContainer endpoint queue completion =
  do
    cls' <- getRequiredClass "MTRBaseClusterContentLauncher"
    sendClassMessage cls' readAttributeSupportedStreamingProtocolsWithClusterStateCache_endpoint_queue_completionSelector (toMTRClusterStateCacheContainer clusterStateCacheContainer) (toNSNumber endpoint) (toNSObject queue) completion

-- | @- readAttributeGeneratedCommandListWithCompletion:@
readAttributeGeneratedCommandListWithCompletion :: IsMTRBaseClusterContentLauncher mtrBaseClusterContentLauncher => mtrBaseClusterContentLauncher -> Ptr () -> IO ()
readAttributeGeneratedCommandListWithCompletion mtrBaseClusterContentLauncher completion =
  sendMessage mtrBaseClusterContentLauncher readAttributeGeneratedCommandListWithCompletionSelector completion

-- | @- subscribeAttributeGeneratedCommandListWithParams:subscriptionEstablished:reportHandler:@
subscribeAttributeGeneratedCommandListWithParams_subscriptionEstablished_reportHandler :: (IsMTRBaseClusterContentLauncher mtrBaseClusterContentLauncher, IsMTRSubscribeParams params) => mtrBaseClusterContentLauncher -> params -> Ptr () -> Ptr () -> IO ()
subscribeAttributeGeneratedCommandListWithParams_subscriptionEstablished_reportHandler mtrBaseClusterContentLauncher params subscriptionEstablished reportHandler =
  sendMessage mtrBaseClusterContentLauncher subscribeAttributeGeneratedCommandListWithParams_subscriptionEstablished_reportHandlerSelector (toMTRSubscribeParams params) subscriptionEstablished reportHandler

-- | @+ readAttributeGeneratedCommandListWithClusterStateCache:endpoint:queue:completion:@
readAttributeGeneratedCommandListWithClusterStateCache_endpoint_queue_completion :: (IsMTRClusterStateCacheContainer clusterStateCacheContainer, IsNSNumber endpoint, IsNSObject queue) => clusterStateCacheContainer -> endpoint -> queue -> Ptr () -> IO ()
readAttributeGeneratedCommandListWithClusterStateCache_endpoint_queue_completion clusterStateCacheContainer endpoint queue completion =
  do
    cls' <- getRequiredClass "MTRBaseClusterContentLauncher"
    sendClassMessage cls' readAttributeGeneratedCommandListWithClusterStateCache_endpoint_queue_completionSelector (toMTRClusterStateCacheContainer clusterStateCacheContainer) (toNSNumber endpoint) (toNSObject queue) completion

-- | @- readAttributeAcceptedCommandListWithCompletion:@
readAttributeAcceptedCommandListWithCompletion :: IsMTRBaseClusterContentLauncher mtrBaseClusterContentLauncher => mtrBaseClusterContentLauncher -> Ptr () -> IO ()
readAttributeAcceptedCommandListWithCompletion mtrBaseClusterContentLauncher completion =
  sendMessage mtrBaseClusterContentLauncher readAttributeAcceptedCommandListWithCompletionSelector completion

-- | @- subscribeAttributeAcceptedCommandListWithParams:subscriptionEstablished:reportHandler:@
subscribeAttributeAcceptedCommandListWithParams_subscriptionEstablished_reportHandler :: (IsMTRBaseClusterContentLauncher mtrBaseClusterContentLauncher, IsMTRSubscribeParams params) => mtrBaseClusterContentLauncher -> params -> Ptr () -> Ptr () -> IO ()
subscribeAttributeAcceptedCommandListWithParams_subscriptionEstablished_reportHandler mtrBaseClusterContentLauncher params subscriptionEstablished reportHandler =
  sendMessage mtrBaseClusterContentLauncher subscribeAttributeAcceptedCommandListWithParams_subscriptionEstablished_reportHandlerSelector (toMTRSubscribeParams params) subscriptionEstablished reportHandler

-- | @+ readAttributeAcceptedCommandListWithClusterStateCache:endpoint:queue:completion:@
readAttributeAcceptedCommandListWithClusterStateCache_endpoint_queue_completion :: (IsMTRClusterStateCacheContainer clusterStateCacheContainer, IsNSNumber endpoint, IsNSObject queue) => clusterStateCacheContainer -> endpoint -> queue -> Ptr () -> IO ()
readAttributeAcceptedCommandListWithClusterStateCache_endpoint_queue_completion clusterStateCacheContainer endpoint queue completion =
  do
    cls' <- getRequiredClass "MTRBaseClusterContentLauncher"
    sendClassMessage cls' readAttributeAcceptedCommandListWithClusterStateCache_endpoint_queue_completionSelector (toMTRClusterStateCacheContainer clusterStateCacheContainer) (toNSNumber endpoint) (toNSObject queue) completion

-- | @- readAttributeAttributeListWithCompletion:@
readAttributeAttributeListWithCompletion :: IsMTRBaseClusterContentLauncher mtrBaseClusterContentLauncher => mtrBaseClusterContentLauncher -> Ptr () -> IO ()
readAttributeAttributeListWithCompletion mtrBaseClusterContentLauncher completion =
  sendMessage mtrBaseClusterContentLauncher readAttributeAttributeListWithCompletionSelector completion

-- | @- subscribeAttributeAttributeListWithParams:subscriptionEstablished:reportHandler:@
subscribeAttributeAttributeListWithParams_subscriptionEstablished_reportHandler :: (IsMTRBaseClusterContentLauncher mtrBaseClusterContentLauncher, IsMTRSubscribeParams params) => mtrBaseClusterContentLauncher -> params -> Ptr () -> Ptr () -> IO ()
subscribeAttributeAttributeListWithParams_subscriptionEstablished_reportHandler mtrBaseClusterContentLauncher params subscriptionEstablished reportHandler =
  sendMessage mtrBaseClusterContentLauncher subscribeAttributeAttributeListWithParams_subscriptionEstablished_reportHandlerSelector (toMTRSubscribeParams params) subscriptionEstablished reportHandler

-- | @+ readAttributeAttributeListWithClusterStateCache:endpoint:queue:completion:@
readAttributeAttributeListWithClusterStateCache_endpoint_queue_completion :: (IsMTRClusterStateCacheContainer clusterStateCacheContainer, IsNSNumber endpoint, IsNSObject queue) => clusterStateCacheContainer -> endpoint -> queue -> Ptr () -> IO ()
readAttributeAttributeListWithClusterStateCache_endpoint_queue_completion clusterStateCacheContainer endpoint queue completion =
  do
    cls' <- getRequiredClass "MTRBaseClusterContentLauncher"
    sendClassMessage cls' readAttributeAttributeListWithClusterStateCache_endpoint_queue_completionSelector (toMTRClusterStateCacheContainer clusterStateCacheContainer) (toNSNumber endpoint) (toNSObject queue) completion

-- | @- readAttributeFeatureMapWithCompletion:@
readAttributeFeatureMapWithCompletion :: IsMTRBaseClusterContentLauncher mtrBaseClusterContentLauncher => mtrBaseClusterContentLauncher -> Ptr () -> IO ()
readAttributeFeatureMapWithCompletion mtrBaseClusterContentLauncher completion =
  sendMessage mtrBaseClusterContentLauncher readAttributeFeatureMapWithCompletionSelector completion

-- | @- subscribeAttributeFeatureMapWithParams:subscriptionEstablished:reportHandler:@
subscribeAttributeFeatureMapWithParams_subscriptionEstablished_reportHandler :: (IsMTRBaseClusterContentLauncher mtrBaseClusterContentLauncher, IsMTRSubscribeParams params) => mtrBaseClusterContentLauncher -> params -> Ptr () -> Ptr () -> IO ()
subscribeAttributeFeatureMapWithParams_subscriptionEstablished_reportHandler mtrBaseClusterContentLauncher params subscriptionEstablished reportHandler =
  sendMessage mtrBaseClusterContentLauncher subscribeAttributeFeatureMapWithParams_subscriptionEstablished_reportHandlerSelector (toMTRSubscribeParams params) subscriptionEstablished reportHandler

-- | @+ readAttributeFeatureMapWithClusterStateCache:endpoint:queue:completion:@
readAttributeFeatureMapWithClusterStateCache_endpoint_queue_completion :: (IsMTRClusterStateCacheContainer clusterStateCacheContainer, IsNSNumber endpoint, IsNSObject queue) => clusterStateCacheContainer -> endpoint -> queue -> Ptr () -> IO ()
readAttributeFeatureMapWithClusterStateCache_endpoint_queue_completion clusterStateCacheContainer endpoint queue completion =
  do
    cls' <- getRequiredClass "MTRBaseClusterContentLauncher"
    sendClassMessage cls' readAttributeFeatureMapWithClusterStateCache_endpoint_queue_completionSelector (toMTRClusterStateCacheContainer clusterStateCacheContainer) (toNSNumber endpoint) (toNSObject queue) completion

-- | @- readAttributeClusterRevisionWithCompletion:@
readAttributeClusterRevisionWithCompletion :: IsMTRBaseClusterContentLauncher mtrBaseClusterContentLauncher => mtrBaseClusterContentLauncher -> Ptr () -> IO ()
readAttributeClusterRevisionWithCompletion mtrBaseClusterContentLauncher completion =
  sendMessage mtrBaseClusterContentLauncher readAttributeClusterRevisionWithCompletionSelector completion

-- | @- subscribeAttributeClusterRevisionWithParams:subscriptionEstablished:reportHandler:@
subscribeAttributeClusterRevisionWithParams_subscriptionEstablished_reportHandler :: (IsMTRBaseClusterContentLauncher mtrBaseClusterContentLauncher, IsMTRSubscribeParams params) => mtrBaseClusterContentLauncher -> params -> Ptr () -> Ptr () -> IO ()
subscribeAttributeClusterRevisionWithParams_subscriptionEstablished_reportHandler mtrBaseClusterContentLauncher params subscriptionEstablished reportHandler =
  sendMessage mtrBaseClusterContentLauncher subscribeAttributeClusterRevisionWithParams_subscriptionEstablished_reportHandlerSelector (toMTRSubscribeParams params) subscriptionEstablished reportHandler

-- | @+ readAttributeClusterRevisionWithClusterStateCache:endpoint:queue:completion:@
readAttributeClusterRevisionWithClusterStateCache_endpoint_queue_completion :: (IsMTRClusterStateCacheContainer clusterStateCacheContainer, IsNSNumber endpoint, IsNSObject queue) => clusterStateCacheContainer -> endpoint -> queue -> Ptr () -> IO ()
readAttributeClusterRevisionWithClusterStateCache_endpoint_queue_completion clusterStateCacheContainer endpoint queue completion =
  do
    cls' <- getRequiredClass "MTRBaseClusterContentLauncher"
    sendClassMessage cls' readAttributeClusterRevisionWithClusterStateCache_endpoint_queue_completionSelector (toMTRClusterStateCacheContainer clusterStateCacheContainer) (toNSNumber endpoint) (toNSObject queue) completion

-- | @- init@
init_ :: IsMTRBaseClusterContentLauncher mtrBaseClusterContentLauncher => mtrBaseClusterContentLauncher -> IO (Id MTRBaseClusterContentLauncher)
init_ mtrBaseClusterContentLauncher =
  sendOwnedMessage mtrBaseClusterContentLauncher initSelector

-- | @+ new@
new :: IO (Id MTRBaseClusterContentLauncher)
new  =
  do
    cls' <- getRequiredClass "MTRBaseClusterContentLauncher"
    sendOwnedClassMessage cls' newSelector

-- | @- initWithDevice:endpoint:queue:@
initWithDevice_endpoint_queue :: (IsMTRBaseClusterContentLauncher mtrBaseClusterContentLauncher, IsMTRBaseDevice device, IsNSObject queue) => mtrBaseClusterContentLauncher -> device -> CUShort -> queue -> IO (Id MTRBaseClusterContentLauncher)
initWithDevice_endpoint_queue mtrBaseClusterContentLauncher device endpoint queue =
  sendOwnedMessage mtrBaseClusterContentLauncher initWithDevice_endpoint_queueSelector (toMTRBaseDevice device) endpoint (toNSObject queue)

-- | @- launchContentWithParams:completionHandler:@
launchContentWithParams_completionHandler :: (IsMTRBaseClusterContentLauncher mtrBaseClusterContentLauncher, IsMTRContentLauncherClusterLaunchContentParams params) => mtrBaseClusterContentLauncher -> params -> Ptr () -> IO ()
launchContentWithParams_completionHandler mtrBaseClusterContentLauncher params completionHandler =
  sendMessage mtrBaseClusterContentLauncher launchContentWithParams_completionHandlerSelector (toMTRContentLauncherClusterLaunchContentParams params) completionHandler

-- | @- launchURLWithParams:completionHandler:@
launchURLWithParams_completionHandler :: (IsMTRBaseClusterContentLauncher mtrBaseClusterContentLauncher, IsMTRContentLauncherClusterLaunchURLParams params) => mtrBaseClusterContentLauncher -> params -> Ptr () -> IO ()
launchURLWithParams_completionHandler mtrBaseClusterContentLauncher params completionHandler =
  sendMessage mtrBaseClusterContentLauncher launchURLWithParams_completionHandlerSelector (toMTRContentLauncherClusterLaunchURLParams params) completionHandler

-- | @- readAttributeAcceptHeaderWithCompletionHandler:@
readAttributeAcceptHeaderWithCompletionHandler :: IsMTRBaseClusterContentLauncher mtrBaseClusterContentLauncher => mtrBaseClusterContentLauncher -> Ptr () -> IO ()
readAttributeAcceptHeaderWithCompletionHandler mtrBaseClusterContentLauncher completionHandler =
  sendMessage mtrBaseClusterContentLauncher readAttributeAcceptHeaderWithCompletionHandlerSelector completionHandler

-- | @- subscribeAttributeAcceptHeaderWithMinInterval:maxInterval:params:subscriptionEstablished:reportHandler:@
subscribeAttributeAcceptHeaderWithMinInterval_maxInterval_params_subscriptionEstablished_reportHandler :: (IsMTRBaseClusterContentLauncher mtrBaseClusterContentLauncher, IsNSNumber minInterval, IsNSNumber maxInterval, IsMTRSubscribeParams params) => mtrBaseClusterContentLauncher -> minInterval -> maxInterval -> params -> Ptr () -> Ptr () -> IO ()
subscribeAttributeAcceptHeaderWithMinInterval_maxInterval_params_subscriptionEstablished_reportHandler mtrBaseClusterContentLauncher minInterval maxInterval params subscriptionEstablishedHandler reportHandler =
  sendMessage mtrBaseClusterContentLauncher subscribeAttributeAcceptHeaderWithMinInterval_maxInterval_params_subscriptionEstablished_reportHandlerSelector (toNSNumber minInterval) (toNSNumber maxInterval) (toMTRSubscribeParams params) subscriptionEstablishedHandler reportHandler

-- | @+ readAttributeAcceptHeaderWithAttributeCache:endpoint:queue:completionHandler:@
readAttributeAcceptHeaderWithAttributeCache_endpoint_queue_completionHandler :: (IsMTRAttributeCacheContainer attributeCacheContainer, IsNSNumber endpoint, IsNSObject queue) => attributeCacheContainer -> endpoint -> queue -> Ptr () -> IO ()
readAttributeAcceptHeaderWithAttributeCache_endpoint_queue_completionHandler attributeCacheContainer endpoint queue completionHandler =
  do
    cls' <- getRequiredClass "MTRBaseClusterContentLauncher"
    sendClassMessage cls' readAttributeAcceptHeaderWithAttributeCache_endpoint_queue_completionHandlerSelector (toMTRAttributeCacheContainer attributeCacheContainer) (toNSNumber endpoint) (toNSObject queue) completionHandler

-- | @- readAttributeSupportedStreamingProtocolsWithCompletionHandler:@
readAttributeSupportedStreamingProtocolsWithCompletionHandler :: IsMTRBaseClusterContentLauncher mtrBaseClusterContentLauncher => mtrBaseClusterContentLauncher -> Ptr () -> IO ()
readAttributeSupportedStreamingProtocolsWithCompletionHandler mtrBaseClusterContentLauncher completionHandler =
  sendMessage mtrBaseClusterContentLauncher readAttributeSupportedStreamingProtocolsWithCompletionHandlerSelector completionHandler

-- | @- writeAttributeSupportedStreamingProtocolsWithValue:completionHandler:@
writeAttributeSupportedStreamingProtocolsWithValue_completionHandler :: (IsMTRBaseClusterContentLauncher mtrBaseClusterContentLauncher, IsNSNumber value) => mtrBaseClusterContentLauncher -> value -> Ptr () -> IO ()
writeAttributeSupportedStreamingProtocolsWithValue_completionHandler mtrBaseClusterContentLauncher value completionHandler =
  sendMessage mtrBaseClusterContentLauncher writeAttributeSupportedStreamingProtocolsWithValue_completionHandlerSelector (toNSNumber value) completionHandler

-- | @- writeAttributeSupportedStreamingProtocolsWithValue:params:completionHandler:@
writeAttributeSupportedStreamingProtocolsWithValue_params_completionHandler :: (IsMTRBaseClusterContentLauncher mtrBaseClusterContentLauncher, IsNSNumber value, IsMTRWriteParams params) => mtrBaseClusterContentLauncher -> value -> params -> Ptr () -> IO ()
writeAttributeSupportedStreamingProtocolsWithValue_params_completionHandler mtrBaseClusterContentLauncher value params completionHandler =
  sendMessage mtrBaseClusterContentLauncher writeAttributeSupportedStreamingProtocolsWithValue_params_completionHandlerSelector (toNSNumber value) (toMTRWriteParams params) completionHandler

-- | @- subscribeAttributeSupportedStreamingProtocolsWithMinInterval:maxInterval:params:subscriptionEstablished:reportHandler:@
subscribeAttributeSupportedStreamingProtocolsWithMinInterval_maxInterval_params_subscriptionEstablished_reportHandler :: (IsMTRBaseClusterContentLauncher mtrBaseClusterContentLauncher, IsNSNumber minInterval, IsNSNumber maxInterval, IsMTRSubscribeParams params) => mtrBaseClusterContentLauncher -> minInterval -> maxInterval -> params -> Ptr () -> Ptr () -> IO ()
subscribeAttributeSupportedStreamingProtocolsWithMinInterval_maxInterval_params_subscriptionEstablished_reportHandler mtrBaseClusterContentLauncher minInterval maxInterval params subscriptionEstablishedHandler reportHandler =
  sendMessage mtrBaseClusterContentLauncher subscribeAttributeSupportedStreamingProtocolsWithMinInterval_maxInterval_params_subscriptionEstablished_reportHandlerSelector (toNSNumber minInterval) (toNSNumber maxInterval) (toMTRSubscribeParams params) subscriptionEstablishedHandler reportHandler

-- | @+ readAttributeSupportedStreamingProtocolsWithAttributeCache:endpoint:queue:completionHandler:@
readAttributeSupportedStreamingProtocolsWithAttributeCache_endpoint_queue_completionHandler :: (IsMTRAttributeCacheContainer attributeCacheContainer, IsNSNumber endpoint, IsNSObject queue) => attributeCacheContainer -> endpoint -> queue -> Ptr () -> IO ()
readAttributeSupportedStreamingProtocolsWithAttributeCache_endpoint_queue_completionHandler attributeCacheContainer endpoint queue completionHandler =
  do
    cls' <- getRequiredClass "MTRBaseClusterContentLauncher"
    sendClassMessage cls' readAttributeSupportedStreamingProtocolsWithAttributeCache_endpoint_queue_completionHandlerSelector (toMTRAttributeCacheContainer attributeCacheContainer) (toNSNumber endpoint) (toNSObject queue) completionHandler

-- | @- readAttributeGeneratedCommandListWithCompletionHandler:@
readAttributeGeneratedCommandListWithCompletionHandler :: IsMTRBaseClusterContentLauncher mtrBaseClusterContentLauncher => mtrBaseClusterContentLauncher -> Ptr () -> IO ()
readAttributeGeneratedCommandListWithCompletionHandler mtrBaseClusterContentLauncher completionHandler =
  sendMessage mtrBaseClusterContentLauncher readAttributeGeneratedCommandListWithCompletionHandlerSelector completionHandler

-- | @- subscribeAttributeGeneratedCommandListWithMinInterval:maxInterval:params:subscriptionEstablished:reportHandler:@
subscribeAttributeGeneratedCommandListWithMinInterval_maxInterval_params_subscriptionEstablished_reportHandler :: (IsMTRBaseClusterContentLauncher mtrBaseClusterContentLauncher, IsNSNumber minInterval, IsNSNumber maxInterval, IsMTRSubscribeParams params) => mtrBaseClusterContentLauncher -> minInterval -> maxInterval -> params -> Ptr () -> Ptr () -> IO ()
subscribeAttributeGeneratedCommandListWithMinInterval_maxInterval_params_subscriptionEstablished_reportHandler mtrBaseClusterContentLauncher minInterval maxInterval params subscriptionEstablishedHandler reportHandler =
  sendMessage mtrBaseClusterContentLauncher subscribeAttributeGeneratedCommandListWithMinInterval_maxInterval_params_subscriptionEstablished_reportHandlerSelector (toNSNumber minInterval) (toNSNumber maxInterval) (toMTRSubscribeParams params) subscriptionEstablishedHandler reportHandler

-- | @+ readAttributeGeneratedCommandListWithAttributeCache:endpoint:queue:completionHandler:@
readAttributeGeneratedCommandListWithAttributeCache_endpoint_queue_completionHandler :: (IsMTRAttributeCacheContainer attributeCacheContainer, IsNSNumber endpoint, IsNSObject queue) => attributeCacheContainer -> endpoint -> queue -> Ptr () -> IO ()
readAttributeGeneratedCommandListWithAttributeCache_endpoint_queue_completionHandler attributeCacheContainer endpoint queue completionHandler =
  do
    cls' <- getRequiredClass "MTRBaseClusterContentLauncher"
    sendClassMessage cls' readAttributeGeneratedCommandListWithAttributeCache_endpoint_queue_completionHandlerSelector (toMTRAttributeCacheContainer attributeCacheContainer) (toNSNumber endpoint) (toNSObject queue) completionHandler

-- | @- readAttributeAcceptedCommandListWithCompletionHandler:@
readAttributeAcceptedCommandListWithCompletionHandler :: IsMTRBaseClusterContentLauncher mtrBaseClusterContentLauncher => mtrBaseClusterContentLauncher -> Ptr () -> IO ()
readAttributeAcceptedCommandListWithCompletionHandler mtrBaseClusterContentLauncher completionHandler =
  sendMessage mtrBaseClusterContentLauncher readAttributeAcceptedCommandListWithCompletionHandlerSelector completionHandler

-- | @- subscribeAttributeAcceptedCommandListWithMinInterval:maxInterval:params:subscriptionEstablished:reportHandler:@
subscribeAttributeAcceptedCommandListWithMinInterval_maxInterval_params_subscriptionEstablished_reportHandler :: (IsMTRBaseClusterContentLauncher mtrBaseClusterContentLauncher, IsNSNumber minInterval, IsNSNumber maxInterval, IsMTRSubscribeParams params) => mtrBaseClusterContentLauncher -> minInterval -> maxInterval -> params -> Ptr () -> Ptr () -> IO ()
subscribeAttributeAcceptedCommandListWithMinInterval_maxInterval_params_subscriptionEstablished_reportHandler mtrBaseClusterContentLauncher minInterval maxInterval params subscriptionEstablishedHandler reportHandler =
  sendMessage mtrBaseClusterContentLauncher subscribeAttributeAcceptedCommandListWithMinInterval_maxInterval_params_subscriptionEstablished_reportHandlerSelector (toNSNumber minInterval) (toNSNumber maxInterval) (toMTRSubscribeParams params) subscriptionEstablishedHandler reportHandler

-- | @+ readAttributeAcceptedCommandListWithAttributeCache:endpoint:queue:completionHandler:@
readAttributeAcceptedCommandListWithAttributeCache_endpoint_queue_completionHandler :: (IsMTRAttributeCacheContainer attributeCacheContainer, IsNSNumber endpoint, IsNSObject queue) => attributeCacheContainer -> endpoint -> queue -> Ptr () -> IO ()
readAttributeAcceptedCommandListWithAttributeCache_endpoint_queue_completionHandler attributeCacheContainer endpoint queue completionHandler =
  do
    cls' <- getRequiredClass "MTRBaseClusterContentLauncher"
    sendClassMessage cls' readAttributeAcceptedCommandListWithAttributeCache_endpoint_queue_completionHandlerSelector (toMTRAttributeCacheContainer attributeCacheContainer) (toNSNumber endpoint) (toNSObject queue) completionHandler

-- | @- readAttributeAttributeListWithCompletionHandler:@
readAttributeAttributeListWithCompletionHandler :: IsMTRBaseClusterContentLauncher mtrBaseClusterContentLauncher => mtrBaseClusterContentLauncher -> Ptr () -> IO ()
readAttributeAttributeListWithCompletionHandler mtrBaseClusterContentLauncher completionHandler =
  sendMessage mtrBaseClusterContentLauncher readAttributeAttributeListWithCompletionHandlerSelector completionHandler

-- | @- subscribeAttributeAttributeListWithMinInterval:maxInterval:params:subscriptionEstablished:reportHandler:@
subscribeAttributeAttributeListWithMinInterval_maxInterval_params_subscriptionEstablished_reportHandler :: (IsMTRBaseClusterContentLauncher mtrBaseClusterContentLauncher, IsNSNumber minInterval, IsNSNumber maxInterval, IsMTRSubscribeParams params) => mtrBaseClusterContentLauncher -> minInterval -> maxInterval -> params -> Ptr () -> Ptr () -> IO ()
subscribeAttributeAttributeListWithMinInterval_maxInterval_params_subscriptionEstablished_reportHandler mtrBaseClusterContentLauncher minInterval maxInterval params subscriptionEstablishedHandler reportHandler =
  sendMessage mtrBaseClusterContentLauncher subscribeAttributeAttributeListWithMinInterval_maxInterval_params_subscriptionEstablished_reportHandlerSelector (toNSNumber minInterval) (toNSNumber maxInterval) (toMTRSubscribeParams params) subscriptionEstablishedHandler reportHandler

-- | @+ readAttributeAttributeListWithAttributeCache:endpoint:queue:completionHandler:@
readAttributeAttributeListWithAttributeCache_endpoint_queue_completionHandler :: (IsMTRAttributeCacheContainer attributeCacheContainer, IsNSNumber endpoint, IsNSObject queue) => attributeCacheContainer -> endpoint -> queue -> Ptr () -> IO ()
readAttributeAttributeListWithAttributeCache_endpoint_queue_completionHandler attributeCacheContainer endpoint queue completionHandler =
  do
    cls' <- getRequiredClass "MTRBaseClusterContentLauncher"
    sendClassMessage cls' readAttributeAttributeListWithAttributeCache_endpoint_queue_completionHandlerSelector (toMTRAttributeCacheContainer attributeCacheContainer) (toNSNumber endpoint) (toNSObject queue) completionHandler

-- | @- readAttributeFeatureMapWithCompletionHandler:@
readAttributeFeatureMapWithCompletionHandler :: IsMTRBaseClusterContentLauncher mtrBaseClusterContentLauncher => mtrBaseClusterContentLauncher -> Ptr () -> IO ()
readAttributeFeatureMapWithCompletionHandler mtrBaseClusterContentLauncher completionHandler =
  sendMessage mtrBaseClusterContentLauncher readAttributeFeatureMapWithCompletionHandlerSelector completionHandler

-- | @- subscribeAttributeFeatureMapWithMinInterval:maxInterval:params:subscriptionEstablished:reportHandler:@
subscribeAttributeFeatureMapWithMinInterval_maxInterval_params_subscriptionEstablished_reportHandler :: (IsMTRBaseClusterContentLauncher mtrBaseClusterContentLauncher, IsNSNumber minInterval, IsNSNumber maxInterval, IsMTRSubscribeParams params) => mtrBaseClusterContentLauncher -> minInterval -> maxInterval -> params -> Ptr () -> Ptr () -> IO ()
subscribeAttributeFeatureMapWithMinInterval_maxInterval_params_subscriptionEstablished_reportHandler mtrBaseClusterContentLauncher minInterval maxInterval params subscriptionEstablishedHandler reportHandler =
  sendMessage mtrBaseClusterContentLauncher subscribeAttributeFeatureMapWithMinInterval_maxInterval_params_subscriptionEstablished_reportHandlerSelector (toNSNumber minInterval) (toNSNumber maxInterval) (toMTRSubscribeParams params) subscriptionEstablishedHandler reportHandler

-- | @+ readAttributeFeatureMapWithAttributeCache:endpoint:queue:completionHandler:@
readAttributeFeatureMapWithAttributeCache_endpoint_queue_completionHandler :: (IsMTRAttributeCacheContainer attributeCacheContainer, IsNSNumber endpoint, IsNSObject queue) => attributeCacheContainer -> endpoint -> queue -> Ptr () -> IO ()
readAttributeFeatureMapWithAttributeCache_endpoint_queue_completionHandler attributeCacheContainer endpoint queue completionHandler =
  do
    cls' <- getRequiredClass "MTRBaseClusterContentLauncher"
    sendClassMessage cls' readAttributeFeatureMapWithAttributeCache_endpoint_queue_completionHandlerSelector (toMTRAttributeCacheContainer attributeCacheContainer) (toNSNumber endpoint) (toNSObject queue) completionHandler

-- | @- readAttributeClusterRevisionWithCompletionHandler:@
readAttributeClusterRevisionWithCompletionHandler :: IsMTRBaseClusterContentLauncher mtrBaseClusterContentLauncher => mtrBaseClusterContentLauncher -> Ptr () -> IO ()
readAttributeClusterRevisionWithCompletionHandler mtrBaseClusterContentLauncher completionHandler =
  sendMessage mtrBaseClusterContentLauncher readAttributeClusterRevisionWithCompletionHandlerSelector completionHandler

-- | @- subscribeAttributeClusterRevisionWithMinInterval:maxInterval:params:subscriptionEstablished:reportHandler:@
subscribeAttributeClusterRevisionWithMinInterval_maxInterval_params_subscriptionEstablished_reportHandler :: (IsMTRBaseClusterContentLauncher mtrBaseClusterContentLauncher, IsNSNumber minInterval, IsNSNumber maxInterval, IsMTRSubscribeParams params) => mtrBaseClusterContentLauncher -> minInterval -> maxInterval -> params -> Ptr () -> Ptr () -> IO ()
subscribeAttributeClusterRevisionWithMinInterval_maxInterval_params_subscriptionEstablished_reportHandler mtrBaseClusterContentLauncher minInterval maxInterval params subscriptionEstablishedHandler reportHandler =
  sendMessage mtrBaseClusterContentLauncher subscribeAttributeClusterRevisionWithMinInterval_maxInterval_params_subscriptionEstablished_reportHandlerSelector (toNSNumber minInterval) (toNSNumber maxInterval) (toMTRSubscribeParams params) subscriptionEstablishedHandler reportHandler

-- | @+ readAttributeClusterRevisionWithAttributeCache:endpoint:queue:completionHandler:@
readAttributeClusterRevisionWithAttributeCache_endpoint_queue_completionHandler :: (IsMTRAttributeCacheContainer attributeCacheContainer, IsNSNumber endpoint, IsNSObject queue) => attributeCacheContainer -> endpoint -> queue -> Ptr () -> IO ()
readAttributeClusterRevisionWithAttributeCache_endpoint_queue_completionHandler attributeCacheContainer endpoint queue completionHandler =
  do
    cls' <- getRequiredClass "MTRBaseClusterContentLauncher"
    sendClassMessage cls' readAttributeClusterRevisionWithAttributeCache_endpoint_queue_completionHandlerSelector (toMTRAttributeCacheContainer attributeCacheContainer) (toNSNumber endpoint) (toNSObject queue) completionHandler

-- | For all instance methods (reads, writes, commands) that take a completion, the completion will be called on the provided queue.
--
-- ObjC selector: @- initWithDevice:endpointID:queue:@
initWithDevice_endpointID_queue :: (IsMTRBaseClusterContentLauncher mtrBaseClusterContentLauncher, IsMTRBaseDevice device, IsNSNumber endpointID, IsNSObject queue) => mtrBaseClusterContentLauncher -> device -> endpointID -> queue -> IO (Id MTRBaseClusterContentLauncher)
initWithDevice_endpointID_queue mtrBaseClusterContentLauncher device endpointID queue =
  sendOwnedMessage mtrBaseClusterContentLauncher initWithDevice_endpointID_queueSelector (toMTRBaseDevice device) (toNSNumber endpointID) (toNSObject queue)

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @launchContentWithParams:completion:@
launchContentWithParams_completionSelector :: Selector '[Id MTRContentLauncherClusterLaunchContentParams, Ptr ()] ()
launchContentWithParams_completionSelector = mkSelector "launchContentWithParams:completion:"

-- | @Selector@ for @launchURLWithParams:completion:@
launchURLWithParams_completionSelector :: Selector '[Id MTRContentLauncherClusterLaunchURLParams, Ptr ()] ()
launchURLWithParams_completionSelector = mkSelector "launchURLWithParams:completion:"

-- | @Selector@ for @readAttributeAcceptHeaderWithCompletion:@
readAttributeAcceptHeaderWithCompletionSelector :: Selector '[Ptr ()] ()
readAttributeAcceptHeaderWithCompletionSelector = mkSelector "readAttributeAcceptHeaderWithCompletion:"

-- | @Selector@ for @subscribeAttributeAcceptHeaderWithParams:subscriptionEstablished:reportHandler:@
subscribeAttributeAcceptHeaderWithParams_subscriptionEstablished_reportHandlerSelector :: Selector '[Id MTRSubscribeParams, Ptr (), Ptr ()] ()
subscribeAttributeAcceptHeaderWithParams_subscriptionEstablished_reportHandlerSelector = mkSelector "subscribeAttributeAcceptHeaderWithParams:subscriptionEstablished:reportHandler:"

-- | @Selector@ for @readAttributeAcceptHeaderWithClusterStateCache:endpoint:queue:completion:@
readAttributeAcceptHeaderWithClusterStateCache_endpoint_queue_completionSelector :: Selector '[Id MTRClusterStateCacheContainer, Id NSNumber, Id NSObject, Ptr ()] ()
readAttributeAcceptHeaderWithClusterStateCache_endpoint_queue_completionSelector = mkSelector "readAttributeAcceptHeaderWithClusterStateCache:endpoint:queue:completion:"

-- | @Selector@ for @readAttributeSupportedStreamingProtocolsWithCompletion:@
readAttributeSupportedStreamingProtocolsWithCompletionSelector :: Selector '[Ptr ()] ()
readAttributeSupportedStreamingProtocolsWithCompletionSelector = mkSelector "readAttributeSupportedStreamingProtocolsWithCompletion:"

-- | @Selector@ for @writeAttributeSupportedStreamingProtocolsWithValue:completion:@
writeAttributeSupportedStreamingProtocolsWithValue_completionSelector :: Selector '[Id NSNumber, Ptr ()] ()
writeAttributeSupportedStreamingProtocolsWithValue_completionSelector = mkSelector "writeAttributeSupportedStreamingProtocolsWithValue:completion:"

-- | @Selector@ for @writeAttributeSupportedStreamingProtocolsWithValue:params:completion:@
writeAttributeSupportedStreamingProtocolsWithValue_params_completionSelector :: Selector '[Id NSNumber, Id MTRWriteParams, Ptr ()] ()
writeAttributeSupportedStreamingProtocolsWithValue_params_completionSelector = mkSelector "writeAttributeSupportedStreamingProtocolsWithValue:params:completion:"

-- | @Selector@ for @subscribeAttributeSupportedStreamingProtocolsWithParams:subscriptionEstablished:reportHandler:@
subscribeAttributeSupportedStreamingProtocolsWithParams_subscriptionEstablished_reportHandlerSelector :: Selector '[Id MTRSubscribeParams, Ptr (), Ptr ()] ()
subscribeAttributeSupportedStreamingProtocolsWithParams_subscriptionEstablished_reportHandlerSelector = mkSelector "subscribeAttributeSupportedStreamingProtocolsWithParams:subscriptionEstablished:reportHandler:"

-- | @Selector@ for @readAttributeSupportedStreamingProtocolsWithClusterStateCache:endpoint:queue:completion:@
readAttributeSupportedStreamingProtocolsWithClusterStateCache_endpoint_queue_completionSelector :: Selector '[Id MTRClusterStateCacheContainer, Id NSNumber, Id NSObject, Ptr ()] ()
readAttributeSupportedStreamingProtocolsWithClusterStateCache_endpoint_queue_completionSelector = mkSelector "readAttributeSupportedStreamingProtocolsWithClusterStateCache:endpoint:queue:completion:"

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
initSelector :: Selector '[] (Id MTRBaseClusterContentLauncher)
initSelector = mkSelector "init"

-- | @Selector@ for @new@
newSelector :: Selector '[] (Id MTRBaseClusterContentLauncher)
newSelector = mkSelector "new"

-- | @Selector@ for @initWithDevice:endpoint:queue:@
initWithDevice_endpoint_queueSelector :: Selector '[Id MTRBaseDevice, CUShort, Id NSObject] (Id MTRBaseClusterContentLauncher)
initWithDevice_endpoint_queueSelector = mkSelector "initWithDevice:endpoint:queue:"

-- | @Selector@ for @launchContentWithParams:completionHandler:@
launchContentWithParams_completionHandlerSelector :: Selector '[Id MTRContentLauncherClusterLaunchContentParams, Ptr ()] ()
launchContentWithParams_completionHandlerSelector = mkSelector "launchContentWithParams:completionHandler:"

-- | @Selector@ for @launchURLWithParams:completionHandler:@
launchURLWithParams_completionHandlerSelector :: Selector '[Id MTRContentLauncherClusterLaunchURLParams, Ptr ()] ()
launchURLWithParams_completionHandlerSelector = mkSelector "launchURLWithParams:completionHandler:"

-- | @Selector@ for @readAttributeAcceptHeaderWithCompletionHandler:@
readAttributeAcceptHeaderWithCompletionHandlerSelector :: Selector '[Ptr ()] ()
readAttributeAcceptHeaderWithCompletionHandlerSelector = mkSelector "readAttributeAcceptHeaderWithCompletionHandler:"

-- | @Selector@ for @subscribeAttributeAcceptHeaderWithMinInterval:maxInterval:params:subscriptionEstablished:reportHandler:@
subscribeAttributeAcceptHeaderWithMinInterval_maxInterval_params_subscriptionEstablished_reportHandlerSelector :: Selector '[Id NSNumber, Id NSNumber, Id MTRSubscribeParams, Ptr (), Ptr ()] ()
subscribeAttributeAcceptHeaderWithMinInterval_maxInterval_params_subscriptionEstablished_reportHandlerSelector = mkSelector "subscribeAttributeAcceptHeaderWithMinInterval:maxInterval:params:subscriptionEstablished:reportHandler:"

-- | @Selector@ for @readAttributeAcceptHeaderWithAttributeCache:endpoint:queue:completionHandler:@
readAttributeAcceptHeaderWithAttributeCache_endpoint_queue_completionHandlerSelector :: Selector '[Id MTRAttributeCacheContainer, Id NSNumber, Id NSObject, Ptr ()] ()
readAttributeAcceptHeaderWithAttributeCache_endpoint_queue_completionHandlerSelector = mkSelector "readAttributeAcceptHeaderWithAttributeCache:endpoint:queue:completionHandler:"

-- | @Selector@ for @readAttributeSupportedStreamingProtocolsWithCompletionHandler:@
readAttributeSupportedStreamingProtocolsWithCompletionHandlerSelector :: Selector '[Ptr ()] ()
readAttributeSupportedStreamingProtocolsWithCompletionHandlerSelector = mkSelector "readAttributeSupportedStreamingProtocolsWithCompletionHandler:"

-- | @Selector@ for @writeAttributeSupportedStreamingProtocolsWithValue:completionHandler:@
writeAttributeSupportedStreamingProtocolsWithValue_completionHandlerSelector :: Selector '[Id NSNumber, Ptr ()] ()
writeAttributeSupportedStreamingProtocolsWithValue_completionHandlerSelector = mkSelector "writeAttributeSupportedStreamingProtocolsWithValue:completionHandler:"

-- | @Selector@ for @writeAttributeSupportedStreamingProtocolsWithValue:params:completionHandler:@
writeAttributeSupportedStreamingProtocolsWithValue_params_completionHandlerSelector :: Selector '[Id NSNumber, Id MTRWriteParams, Ptr ()] ()
writeAttributeSupportedStreamingProtocolsWithValue_params_completionHandlerSelector = mkSelector "writeAttributeSupportedStreamingProtocolsWithValue:params:completionHandler:"

-- | @Selector@ for @subscribeAttributeSupportedStreamingProtocolsWithMinInterval:maxInterval:params:subscriptionEstablished:reportHandler:@
subscribeAttributeSupportedStreamingProtocolsWithMinInterval_maxInterval_params_subscriptionEstablished_reportHandlerSelector :: Selector '[Id NSNumber, Id NSNumber, Id MTRSubscribeParams, Ptr (), Ptr ()] ()
subscribeAttributeSupportedStreamingProtocolsWithMinInterval_maxInterval_params_subscriptionEstablished_reportHandlerSelector = mkSelector "subscribeAttributeSupportedStreamingProtocolsWithMinInterval:maxInterval:params:subscriptionEstablished:reportHandler:"

-- | @Selector@ for @readAttributeSupportedStreamingProtocolsWithAttributeCache:endpoint:queue:completionHandler:@
readAttributeSupportedStreamingProtocolsWithAttributeCache_endpoint_queue_completionHandlerSelector :: Selector '[Id MTRAttributeCacheContainer, Id NSNumber, Id NSObject, Ptr ()] ()
readAttributeSupportedStreamingProtocolsWithAttributeCache_endpoint_queue_completionHandlerSelector = mkSelector "readAttributeSupportedStreamingProtocolsWithAttributeCache:endpoint:queue:completionHandler:"

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
initWithDevice_endpointID_queueSelector :: Selector '[Id MTRBaseDevice, Id NSNumber, Id NSObject] (Id MTRBaseClusterContentLauncher)
initWithDevice_endpointID_queueSelector = mkSelector "initWithDevice:endpointID:queue:"

