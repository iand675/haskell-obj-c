{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Cluster Application Launcher
--
-- This cluster provides an interface for launching content on a media player device such as a TV or Speaker.
--
-- Generated bindings for @MTRBaseClusterApplicationLauncher@.
module ObjC.Matter.MTRBaseClusterApplicationLauncher
  ( MTRBaseClusterApplicationLauncher
  , IsMTRBaseClusterApplicationLauncher(..)
  , launchAppWithParams_completion
  , launchAppWithCompletion
  , stopAppWithParams_completion
  , stopAppWithCompletion
  , hideAppWithParams_completion
  , hideAppWithCompletion
  , readAttributeCatalogListWithCompletion
  , subscribeAttributeCatalogListWithParams_subscriptionEstablished_reportHandler
  , readAttributeCatalogListWithClusterStateCache_endpoint_queue_completion
  , readAttributeCurrentAppWithCompletion
  , writeAttributeCurrentAppWithValue_completion
  , writeAttributeCurrentAppWithValue_params_completion
  , subscribeAttributeCurrentAppWithParams_subscriptionEstablished_reportHandler
  , readAttributeCurrentAppWithClusterStateCache_endpoint_queue_completion
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
  , launchAppWithParams_completionHandler
  , stopAppWithParams_completionHandler
  , hideAppWithParams_completionHandler
  , readAttributeCatalogListWithCompletionHandler
  , subscribeAttributeCatalogListWithMinInterval_maxInterval_params_subscriptionEstablished_reportHandler
  , readAttributeCatalogListWithAttributeCache_endpoint_queue_completionHandler
  , readAttributeCurrentAppWithCompletionHandler
  , writeAttributeCurrentAppWithValue_completionHandler
  , writeAttributeCurrentAppWithValue_params_completionHandler
  , subscribeAttributeCurrentAppWithMinInterval_maxInterval_params_subscriptionEstablished_reportHandler
  , readAttributeCurrentAppWithAttributeCache_endpoint_queue_completionHandler
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
  , hideAppWithCompletionSelector
  , hideAppWithParams_completionHandlerSelector
  , hideAppWithParams_completionSelector
  , initSelector
  , initWithDevice_endpointID_queueSelector
  , initWithDevice_endpoint_queueSelector
  , launchAppWithCompletionSelector
  , launchAppWithParams_completionHandlerSelector
  , launchAppWithParams_completionSelector
  , newSelector
  , readAttributeAcceptedCommandListWithAttributeCache_endpoint_queue_completionHandlerSelector
  , readAttributeAcceptedCommandListWithClusterStateCache_endpoint_queue_completionSelector
  , readAttributeAcceptedCommandListWithCompletionHandlerSelector
  , readAttributeAcceptedCommandListWithCompletionSelector
  , readAttributeAttributeListWithAttributeCache_endpoint_queue_completionHandlerSelector
  , readAttributeAttributeListWithClusterStateCache_endpoint_queue_completionSelector
  , readAttributeAttributeListWithCompletionHandlerSelector
  , readAttributeAttributeListWithCompletionSelector
  , readAttributeCatalogListWithAttributeCache_endpoint_queue_completionHandlerSelector
  , readAttributeCatalogListWithClusterStateCache_endpoint_queue_completionSelector
  , readAttributeCatalogListWithCompletionHandlerSelector
  , readAttributeCatalogListWithCompletionSelector
  , readAttributeClusterRevisionWithAttributeCache_endpoint_queue_completionHandlerSelector
  , readAttributeClusterRevisionWithClusterStateCache_endpoint_queue_completionSelector
  , readAttributeClusterRevisionWithCompletionHandlerSelector
  , readAttributeClusterRevisionWithCompletionSelector
  , readAttributeCurrentAppWithAttributeCache_endpoint_queue_completionHandlerSelector
  , readAttributeCurrentAppWithClusterStateCache_endpoint_queue_completionSelector
  , readAttributeCurrentAppWithCompletionHandlerSelector
  , readAttributeCurrentAppWithCompletionSelector
  , readAttributeFeatureMapWithAttributeCache_endpoint_queue_completionHandlerSelector
  , readAttributeFeatureMapWithClusterStateCache_endpoint_queue_completionSelector
  , readAttributeFeatureMapWithCompletionHandlerSelector
  , readAttributeFeatureMapWithCompletionSelector
  , readAttributeGeneratedCommandListWithAttributeCache_endpoint_queue_completionHandlerSelector
  , readAttributeGeneratedCommandListWithClusterStateCache_endpoint_queue_completionSelector
  , readAttributeGeneratedCommandListWithCompletionHandlerSelector
  , readAttributeGeneratedCommandListWithCompletionSelector
  , stopAppWithCompletionSelector
  , stopAppWithParams_completionHandlerSelector
  , stopAppWithParams_completionSelector
  , subscribeAttributeAcceptedCommandListWithMinInterval_maxInterval_params_subscriptionEstablished_reportHandlerSelector
  , subscribeAttributeAcceptedCommandListWithParams_subscriptionEstablished_reportHandlerSelector
  , subscribeAttributeAttributeListWithMinInterval_maxInterval_params_subscriptionEstablished_reportHandlerSelector
  , subscribeAttributeAttributeListWithParams_subscriptionEstablished_reportHandlerSelector
  , subscribeAttributeCatalogListWithMinInterval_maxInterval_params_subscriptionEstablished_reportHandlerSelector
  , subscribeAttributeCatalogListWithParams_subscriptionEstablished_reportHandlerSelector
  , subscribeAttributeClusterRevisionWithMinInterval_maxInterval_params_subscriptionEstablished_reportHandlerSelector
  , subscribeAttributeClusterRevisionWithParams_subscriptionEstablished_reportHandlerSelector
  , subscribeAttributeCurrentAppWithMinInterval_maxInterval_params_subscriptionEstablished_reportHandlerSelector
  , subscribeAttributeCurrentAppWithParams_subscriptionEstablished_reportHandlerSelector
  , subscribeAttributeFeatureMapWithMinInterval_maxInterval_params_subscriptionEstablished_reportHandlerSelector
  , subscribeAttributeFeatureMapWithParams_subscriptionEstablished_reportHandlerSelector
  , subscribeAttributeGeneratedCommandListWithMinInterval_maxInterval_params_subscriptionEstablished_reportHandlerSelector
  , subscribeAttributeGeneratedCommandListWithParams_subscriptionEstablished_reportHandlerSelector
  , writeAttributeCurrentAppWithValue_completionHandlerSelector
  , writeAttributeCurrentAppWithValue_completionSelector
  , writeAttributeCurrentAppWithValue_params_completionHandlerSelector
  , writeAttributeCurrentAppWithValue_params_completionSelector


  ) where

import Foreign.Ptr (Ptr, FunPtr)
import Foreign.C.Types

import ObjC.Runtime.Types
import ObjC.Runtime.Message (sendMessage, sendOwnedMessage, sendClassMessage, sendOwnedClassMessage)
import ObjC.Runtime.Selector (mkSelector)
import ObjC.Runtime.Class (getRequiredClass)

import ObjC.Matter.Internal.Classes
import ObjC.Foundation.Internal.Classes

-- | Command LaunchApp
--
-- Upon receipt, this SHALL launch the specified app with optional data. The TV Device SHALL launch and bring to foreground the identified application in the command if the application is not already launched and in foreground. The TV Device SHALL update state attribute on the Application Basic cluster of the Endpoint corresponding to the launched application. This command returns a Launch Response.
--
-- ObjC selector: @- launchAppWithParams:completion:@
launchAppWithParams_completion :: (IsMTRBaseClusterApplicationLauncher mtrBaseClusterApplicationLauncher, IsMTRApplicationLauncherClusterLaunchAppParams params) => mtrBaseClusterApplicationLauncher -> params -> Ptr () -> IO ()
launchAppWithParams_completion mtrBaseClusterApplicationLauncher params completion =
  sendMessage mtrBaseClusterApplicationLauncher launchAppWithParams_completionSelector (toMTRApplicationLauncherClusterLaunchAppParams params) completion

-- | @- launchAppWithCompletion:@
launchAppWithCompletion :: IsMTRBaseClusterApplicationLauncher mtrBaseClusterApplicationLauncher => mtrBaseClusterApplicationLauncher -> Ptr () -> IO ()
launchAppWithCompletion mtrBaseClusterApplicationLauncher completion =
  sendMessage mtrBaseClusterApplicationLauncher launchAppWithCompletionSelector completion

-- | Command StopApp
--
-- Upon receipt on a Video Player endpoint this SHALL stop the specified application if it is running.
--
-- ObjC selector: @- stopAppWithParams:completion:@
stopAppWithParams_completion :: (IsMTRBaseClusterApplicationLauncher mtrBaseClusterApplicationLauncher, IsMTRApplicationLauncherClusterStopAppParams params) => mtrBaseClusterApplicationLauncher -> params -> Ptr () -> IO ()
stopAppWithParams_completion mtrBaseClusterApplicationLauncher params completion =
  sendMessage mtrBaseClusterApplicationLauncher stopAppWithParams_completionSelector (toMTRApplicationLauncherClusterStopAppParams params) completion

-- | @- stopAppWithCompletion:@
stopAppWithCompletion :: IsMTRBaseClusterApplicationLauncher mtrBaseClusterApplicationLauncher => mtrBaseClusterApplicationLauncher -> Ptr () -> IO ()
stopAppWithCompletion mtrBaseClusterApplicationLauncher completion =
  sendMessage mtrBaseClusterApplicationLauncher stopAppWithCompletionSelector completion

-- | Command HideApp
--
-- Upon receipt on a Video Player endpoint this SHALL hide the specified application if it is running and visible.
--
-- ObjC selector: @- hideAppWithParams:completion:@
hideAppWithParams_completion :: (IsMTRBaseClusterApplicationLauncher mtrBaseClusterApplicationLauncher, IsMTRApplicationLauncherClusterHideAppParams params) => mtrBaseClusterApplicationLauncher -> params -> Ptr () -> IO ()
hideAppWithParams_completion mtrBaseClusterApplicationLauncher params completion =
  sendMessage mtrBaseClusterApplicationLauncher hideAppWithParams_completionSelector (toMTRApplicationLauncherClusterHideAppParams params) completion

-- | @- hideAppWithCompletion:@
hideAppWithCompletion :: IsMTRBaseClusterApplicationLauncher mtrBaseClusterApplicationLauncher => mtrBaseClusterApplicationLauncher -> Ptr () -> IO ()
hideAppWithCompletion mtrBaseClusterApplicationLauncher completion =
  sendMessage mtrBaseClusterApplicationLauncher hideAppWithCompletionSelector completion

-- | @- readAttributeCatalogListWithCompletion:@
readAttributeCatalogListWithCompletion :: IsMTRBaseClusterApplicationLauncher mtrBaseClusterApplicationLauncher => mtrBaseClusterApplicationLauncher -> Ptr () -> IO ()
readAttributeCatalogListWithCompletion mtrBaseClusterApplicationLauncher completion =
  sendMessage mtrBaseClusterApplicationLauncher readAttributeCatalogListWithCompletionSelector completion

-- | @- subscribeAttributeCatalogListWithParams:subscriptionEstablished:reportHandler:@
subscribeAttributeCatalogListWithParams_subscriptionEstablished_reportHandler :: (IsMTRBaseClusterApplicationLauncher mtrBaseClusterApplicationLauncher, IsMTRSubscribeParams params) => mtrBaseClusterApplicationLauncher -> params -> Ptr () -> Ptr () -> IO ()
subscribeAttributeCatalogListWithParams_subscriptionEstablished_reportHandler mtrBaseClusterApplicationLauncher params subscriptionEstablished reportHandler =
  sendMessage mtrBaseClusterApplicationLauncher subscribeAttributeCatalogListWithParams_subscriptionEstablished_reportHandlerSelector (toMTRSubscribeParams params) subscriptionEstablished reportHandler

-- | @+ readAttributeCatalogListWithClusterStateCache:endpoint:queue:completion:@
readAttributeCatalogListWithClusterStateCache_endpoint_queue_completion :: (IsMTRClusterStateCacheContainer clusterStateCacheContainer, IsNSNumber endpoint, IsNSObject queue) => clusterStateCacheContainer -> endpoint -> queue -> Ptr () -> IO ()
readAttributeCatalogListWithClusterStateCache_endpoint_queue_completion clusterStateCacheContainer endpoint queue completion =
  do
    cls' <- getRequiredClass "MTRBaseClusterApplicationLauncher"
    sendClassMessage cls' readAttributeCatalogListWithClusterStateCache_endpoint_queue_completionSelector (toMTRClusterStateCacheContainer clusterStateCacheContainer) (toNSNumber endpoint) (toNSObject queue) completion

-- | @- readAttributeCurrentAppWithCompletion:@
readAttributeCurrentAppWithCompletion :: IsMTRBaseClusterApplicationLauncher mtrBaseClusterApplicationLauncher => mtrBaseClusterApplicationLauncher -> Ptr () -> IO ()
readAttributeCurrentAppWithCompletion mtrBaseClusterApplicationLauncher completion =
  sendMessage mtrBaseClusterApplicationLauncher readAttributeCurrentAppWithCompletionSelector completion

-- | @- writeAttributeCurrentAppWithValue:completion:@
writeAttributeCurrentAppWithValue_completion :: (IsMTRBaseClusterApplicationLauncher mtrBaseClusterApplicationLauncher, IsMTRApplicationLauncherClusterApplicationEPStruct value) => mtrBaseClusterApplicationLauncher -> value -> Ptr () -> IO ()
writeAttributeCurrentAppWithValue_completion mtrBaseClusterApplicationLauncher value completion =
  sendMessage mtrBaseClusterApplicationLauncher writeAttributeCurrentAppWithValue_completionSelector (toMTRApplicationLauncherClusterApplicationEPStruct value) completion

-- | @- writeAttributeCurrentAppWithValue:params:completion:@
writeAttributeCurrentAppWithValue_params_completion :: (IsMTRBaseClusterApplicationLauncher mtrBaseClusterApplicationLauncher, IsMTRApplicationLauncherClusterApplicationEPStruct value, IsMTRWriteParams params) => mtrBaseClusterApplicationLauncher -> value -> params -> Ptr () -> IO ()
writeAttributeCurrentAppWithValue_params_completion mtrBaseClusterApplicationLauncher value params completion =
  sendMessage mtrBaseClusterApplicationLauncher writeAttributeCurrentAppWithValue_params_completionSelector (toMTRApplicationLauncherClusterApplicationEPStruct value) (toMTRWriteParams params) completion

-- | @- subscribeAttributeCurrentAppWithParams:subscriptionEstablished:reportHandler:@
subscribeAttributeCurrentAppWithParams_subscriptionEstablished_reportHandler :: (IsMTRBaseClusterApplicationLauncher mtrBaseClusterApplicationLauncher, IsMTRSubscribeParams params) => mtrBaseClusterApplicationLauncher -> params -> Ptr () -> Ptr () -> IO ()
subscribeAttributeCurrentAppWithParams_subscriptionEstablished_reportHandler mtrBaseClusterApplicationLauncher params subscriptionEstablished reportHandler =
  sendMessage mtrBaseClusterApplicationLauncher subscribeAttributeCurrentAppWithParams_subscriptionEstablished_reportHandlerSelector (toMTRSubscribeParams params) subscriptionEstablished reportHandler

-- | @+ readAttributeCurrentAppWithClusterStateCache:endpoint:queue:completion:@
readAttributeCurrentAppWithClusterStateCache_endpoint_queue_completion :: (IsMTRClusterStateCacheContainer clusterStateCacheContainer, IsNSNumber endpoint, IsNSObject queue) => clusterStateCacheContainer -> endpoint -> queue -> Ptr () -> IO ()
readAttributeCurrentAppWithClusterStateCache_endpoint_queue_completion clusterStateCacheContainer endpoint queue completion =
  do
    cls' <- getRequiredClass "MTRBaseClusterApplicationLauncher"
    sendClassMessage cls' readAttributeCurrentAppWithClusterStateCache_endpoint_queue_completionSelector (toMTRClusterStateCacheContainer clusterStateCacheContainer) (toNSNumber endpoint) (toNSObject queue) completion

-- | @- readAttributeGeneratedCommandListWithCompletion:@
readAttributeGeneratedCommandListWithCompletion :: IsMTRBaseClusterApplicationLauncher mtrBaseClusterApplicationLauncher => mtrBaseClusterApplicationLauncher -> Ptr () -> IO ()
readAttributeGeneratedCommandListWithCompletion mtrBaseClusterApplicationLauncher completion =
  sendMessage mtrBaseClusterApplicationLauncher readAttributeGeneratedCommandListWithCompletionSelector completion

-- | @- subscribeAttributeGeneratedCommandListWithParams:subscriptionEstablished:reportHandler:@
subscribeAttributeGeneratedCommandListWithParams_subscriptionEstablished_reportHandler :: (IsMTRBaseClusterApplicationLauncher mtrBaseClusterApplicationLauncher, IsMTRSubscribeParams params) => mtrBaseClusterApplicationLauncher -> params -> Ptr () -> Ptr () -> IO ()
subscribeAttributeGeneratedCommandListWithParams_subscriptionEstablished_reportHandler mtrBaseClusterApplicationLauncher params subscriptionEstablished reportHandler =
  sendMessage mtrBaseClusterApplicationLauncher subscribeAttributeGeneratedCommandListWithParams_subscriptionEstablished_reportHandlerSelector (toMTRSubscribeParams params) subscriptionEstablished reportHandler

-- | @+ readAttributeGeneratedCommandListWithClusterStateCache:endpoint:queue:completion:@
readAttributeGeneratedCommandListWithClusterStateCache_endpoint_queue_completion :: (IsMTRClusterStateCacheContainer clusterStateCacheContainer, IsNSNumber endpoint, IsNSObject queue) => clusterStateCacheContainer -> endpoint -> queue -> Ptr () -> IO ()
readAttributeGeneratedCommandListWithClusterStateCache_endpoint_queue_completion clusterStateCacheContainer endpoint queue completion =
  do
    cls' <- getRequiredClass "MTRBaseClusterApplicationLauncher"
    sendClassMessage cls' readAttributeGeneratedCommandListWithClusterStateCache_endpoint_queue_completionSelector (toMTRClusterStateCacheContainer clusterStateCacheContainer) (toNSNumber endpoint) (toNSObject queue) completion

-- | @- readAttributeAcceptedCommandListWithCompletion:@
readAttributeAcceptedCommandListWithCompletion :: IsMTRBaseClusterApplicationLauncher mtrBaseClusterApplicationLauncher => mtrBaseClusterApplicationLauncher -> Ptr () -> IO ()
readAttributeAcceptedCommandListWithCompletion mtrBaseClusterApplicationLauncher completion =
  sendMessage mtrBaseClusterApplicationLauncher readAttributeAcceptedCommandListWithCompletionSelector completion

-- | @- subscribeAttributeAcceptedCommandListWithParams:subscriptionEstablished:reportHandler:@
subscribeAttributeAcceptedCommandListWithParams_subscriptionEstablished_reportHandler :: (IsMTRBaseClusterApplicationLauncher mtrBaseClusterApplicationLauncher, IsMTRSubscribeParams params) => mtrBaseClusterApplicationLauncher -> params -> Ptr () -> Ptr () -> IO ()
subscribeAttributeAcceptedCommandListWithParams_subscriptionEstablished_reportHandler mtrBaseClusterApplicationLauncher params subscriptionEstablished reportHandler =
  sendMessage mtrBaseClusterApplicationLauncher subscribeAttributeAcceptedCommandListWithParams_subscriptionEstablished_reportHandlerSelector (toMTRSubscribeParams params) subscriptionEstablished reportHandler

-- | @+ readAttributeAcceptedCommandListWithClusterStateCache:endpoint:queue:completion:@
readAttributeAcceptedCommandListWithClusterStateCache_endpoint_queue_completion :: (IsMTRClusterStateCacheContainer clusterStateCacheContainer, IsNSNumber endpoint, IsNSObject queue) => clusterStateCacheContainer -> endpoint -> queue -> Ptr () -> IO ()
readAttributeAcceptedCommandListWithClusterStateCache_endpoint_queue_completion clusterStateCacheContainer endpoint queue completion =
  do
    cls' <- getRequiredClass "MTRBaseClusterApplicationLauncher"
    sendClassMessage cls' readAttributeAcceptedCommandListWithClusterStateCache_endpoint_queue_completionSelector (toMTRClusterStateCacheContainer clusterStateCacheContainer) (toNSNumber endpoint) (toNSObject queue) completion

-- | @- readAttributeAttributeListWithCompletion:@
readAttributeAttributeListWithCompletion :: IsMTRBaseClusterApplicationLauncher mtrBaseClusterApplicationLauncher => mtrBaseClusterApplicationLauncher -> Ptr () -> IO ()
readAttributeAttributeListWithCompletion mtrBaseClusterApplicationLauncher completion =
  sendMessage mtrBaseClusterApplicationLauncher readAttributeAttributeListWithCompletionSelector completion

-- | @- subscribeAttributeAttributeListWithParams:subscriptionEstablished:reportHandler:@
subscribeAttributeAttributeListWithParams_subscriptionEstablished_reportHandler :: (IsMTRBaseClusterApplicationLauncher mtrBaseClusterApplicationLauncher, IsMTRSubscribeParams params) => mtrBaseClusterApplicationLauncher -> params -> Ptr () -> Ptr () -> IO ()
subscribeAttributeAttributeListWithParams_subscriptionEstablished_reportHandler mtrBaseClusterApplicationLauncher params subscriptionEstablished reportHandler =
  sendMessage mtrBaseClusterApplicationLauncher subscribeAttributeAttributeListWithParams_subscriptionEstablished_reportHandlerSelector (toMTRSubscribeParams params) subscriptionEstablished reportHandler

-- | @+ readAttributeAttributeListWithClusterStateCache:endpoint:queue:completion:@
readAttributeAttributeListWithClusterStateCache_endpoint_queue_completion :: (IsMTRClusterStateCacheContainer clusterStateCacheContainer, IsNSNumber endpoint, IsNSObject queue) => clusterStateCacheContainer -> endpoint -> queue -> Ptr () -> IO ()
readAttributeAttributeListWithClusterStateCache_endpoint_queue_completion clusterStateCacheContainer endpoint queue completion =
  do
    cls' <- getRequiredClass "MTRBaseClusterApplicationLauncher"
    sendClassMessage cls' readAttributeAttributeListWithClusterStateCache_endpoint_queue_completionSelector (toMTRClusterStateCacheContainer clusterStateCacheContainer) (toNSNumber endpoint) (toNSObject queue) completion

-- | @- readAttributeFeatureMapWithCompletion:@
readAttributeFeatureMapWithCompletion :: IsMTRBaseClusterApplicationLauncher mtrBaseClusterApplicationLauncher => mtrBaseClusterApplicationLauncher -> Ptr () -> IO ()
readAttributeFeatureMapWithCompletion mtrBaseClusterApplicationLauncher completion =
  sendMessage mtrBaseClusterApplicationLauncher readAttributeFeatureMapWithCompletionSelector completion

-- | @- subscribeAttributeFeatureMapWithParams:subscriptionEstablished:reportHandler:@
subscribeAttributeFeatureMapWithParams_subscriptionEstablished_reportHandler :: (IsMTRBaseClusterApplicationLauncher mtrBaseClusterApplicationLauncher, IsMTRSubscribeParams params) => mtrBaseClusterApplicationLauncher -> params -> Ptr () -> Ptr () -> IO ()
subscribeAttributeFeatureMapWithParams_subscriptionEstablished_reportHandler mtrBaseClusterApplicationLauncher params subscriptionEstablished reportHandler =
  sendMessage mtrBaseClusterApplicationLauncher subscribeAttributeFeatureMapWithParams_subscriptionEstablished_reportHandlerSelector (toMTRSubscribeParams params) subscriptionEstablished reportHandler

-- | @+ readAttributeFeatureMapWithClusterStateCache:endpoint:queue:completion:@
readAttributeFeatureMapWithClusterStateCache_endpoint_queue_completion :: (IsMTRClusterStateCacheContainer clusterStateCacheContainer, IsNSNumber endpoint, IsNSObject queue) => clusterStateCacheContainer -> endpoint -> queue -> Ptr () -> IO ()
readAttributeFeatureMapWithClusterStateCache_endpoint_queue_completion clusterStateCacheContainer endpoint queue completion =
  do
    cls' <- getRequiredClass "MTRBaseClusterApplicationLauncher"
    sendClassMessage cls' readAttributeFeatureMapWithClusterStateCache_endpoint_queue_completionSelector (toMTRClusterStateCacheContainer clusterStateCacheContainer) (toNSNumber endpoint) (toNSObject queue) completion

-- | @- readAttributeClusterRevisionWithCompletion:@
readAttributeClusterRevisionWithCompletion :: IsMTRBaseClusterApplicationLauncher mtrBaseClusterApplicationLauncher => mtrBaseClusterApplicationLauncher -> Ptr () -> IO ()
readAttributeClusterRevisionWithCompletion mtrBaseClusterApplicationLauncher completion =
  sendMessage mtrBaseClusterApplicationLauncher readAttributeClusterRevisionWithCompletionSelector completion

-- | @- subscribeAttributeClusterRevisionWithParams:subscriptionEstablished:reportHandler:@
subscribeAttributeClusterRevisionWithParams_subscriptionEstablished_reportHandler :: (IsMTRBaseClusterApplicationLauncher mtrBaseClusterApplicationLauncher, IsMTRSubscribeParams params) => mtrBaseClusterApplicationLauncher -> params -> Ptr () -> Ptr () -> IO ()
subscribeAttributeClusterRevisionWithParams_subscriptionEstablished_reportHandler mtrBaseClusterApplicationLauncher params subscriptionEstablished reportHandler =
  sendMessage mtrBaseClusterApplicationLauncher subscribeAttributeClusterRevisionWithParams_subscriptionEstablished_reportHandlerSelector (toMTRSubscribeParams params) subscriptionEstablished reportHandler

-- | @+ readAttributeClusterRevisionWithClusterStateCache:endpoint:queue:completion:@
readAttributeClusterRevisionWithClusterStateCache_endpoint_queue_completion :: (IsMTRClusterStateCacheContainer clusterStateCacheContainer, IsNSNumber endpoint, IsNSObject queue) => clusterStateCacheContainer -> endpoint -> queue -> Ptr () -> IO ()
readAttributeClusterRevisionWithClusterStateCache_endpoint_queue_completion clusterStateCacheContainer endpoint queue completion =
  do
    cls' <- getRequiredClass "MTRBaseClusterApplicationLauncher"
    sendClassMessage cls' readAttributeClusterRevisionWithClusterStateCache_endpoint_queue_completionSelector (toMTRClusterStateCacheContainer clusterStateCacheContainer) (toNSNumber endpoint) (toNSObject queue) completion

-- | @- init@
init_ :: IsMTRBaseClusterApplicationLauncher mtrBaseClusterApplicationLauncher => mtrBaseClusterApplicationLauncher -> IO (Id MTRBaseClusterApplicationLauncher)
init_ mtrBaseClusterApplicationLauncher =
  sendOwnedMessage mtrBaseClusterApplicationLauncher initSelector

-- | @+ new@
new :: IO (Id MTRBaseClusterApplicationLauncher)
new  =
  do
    cls' <- getRequiredClass "MTRBaseClusterApplicationLauncher"
    sendOwnedClassMessage cls' newSelector

-- | @- initWithDevice:endpoint:queue:@
initWithDevice_endpoint_queue :: (IsMTRBaseClusterApplicationLauncher mtrBaseClusterApplicationLauncher, IsMTRBaseDevice device, IsNSObject queue) => mtrBaseClusterApplicationLauncher -> device -> CUShort -> queue -> IO (Id MTRBaseClusterApplicationLauncher)
initWithDevice_endpoint_queue mtrBaseClusterApplicationLauncher device endpoint queue =
  sendOwnedMessage mtrBaseClusterApplicationLauncher initWithDevice_endpoint_queueSelector (toMTRBaseDevice device) endpoint (toNSObject queue)

-- | @- launchAppWithParams:completionHandler:@
launchAppWithParams_completionHandler :: (IsMTRBaseClusterApplicationLauncher mtrBaseClusterApplicationLauncher, IsMTRApplicationLauncherClusterLaunchAppParams params) => mtrBaseClusterApplicationLauncher -> params -> Ptr () -> IO ()
launchAppWithParams_completionHandler mtrBaseClusterApplicationLauncher params completionHandler =
  sendMessage mtrBaseClusterApplicationLauncher launchAppWithParams_completionHandlerSelector (toMTRApplicationLauncherClusterLaunchAppParams params) completionHandler

-- | @- stopAppWithParams:completionHandler:@
stopAppWithParams_completionHandler :: (IsMTRBaseClusterApplicationLauncher mtrBaseClusterApplicationLauncher, IsMTRApplicationLauncherClusterStopAppParams params) => mtrBaseClusterApplicationLauncher -> params -> Ptr () -> IO ()
stopAppWithParams_completionHandler mtrBaseClusterApplicationLauncher params completionHandler =
  sendMessage mtrBaseClusterApplicationLauncher stopAppWithParams_completionHandlerSelector (toMTRApplicationLauncherClusterStopAppParams params) completionHandler

-- | @- hideAppWithParams:completionHandler:@
hideAppWithParams_completionHandler :: (IsMTRBaseClusterApplicationLauncher mtrBaseClusterApplicationLauncher, IsMTRApplicationLauncherClusterHideAppParams params) => mtrBaseClusterApplicationLauncher -> params -> Ptr () -> IO ()
hideAppWithParams_completionHandler mtrBaseClusterApplicationLauncher params completionHandler =
  sendMessage mtrBaseClusterApplicationLauncher hideAppWithParams_completionHandlerSelector (toMTRApplicationLauncherClusterHideAppParams params) completionHandler

-- | @- readAttributeCatalogListWithCompletionHandler:@
readAttributeCatalogListWithCompletionHandler :: IsMTRBaseClusterApplicationLauncher mtrBaseClusterApplicationLauncher => mtrBaseClusterApplicationLauncher -> Ptr () -> IO ()
readAttributeCatalogListWithCompletionHandler mtrBaseClusterApplicationLauncher completionHandler =
  sendMessage mtrBaseClusterApplicationLauncher readAttributeCatalogListWithCompletionHandlerSelector completionHandler

-- | @- subscribeAttributeCatalogListWithMinInterval:maxInterval:params:subscriptionEstablished:reportHandler:@
subscribeAttributeCatalogListWithMinInterval_maxInterval_params_subscriptionEstablished_reportHandler :: (IsMTRBaseClusterApplicationLauncher mtrBaseClusterApplicationLauncher, IsNSNumber minInterval, IsNSNumber maxInterval, IsMTRSubscribeParams params) => mtrBaseClusterApplicationLauncher -> minInterval -> maxInterval -> params -> Ptr () -> Ptr () -> IO ()
subscribeAttributeCatalogListWithMinInterval_maxInterval_params_subscriptionEstablished_reportHandler mtrBaseClusterApplicationLauncher minInterval maxInterval params subscriptionEstablishedHandler reportHandler =
  sendMessage mtrBaseClusterApplicationLauncher subscribeAttributeCatalogListWithMinInterval_maxInterval_params_subscriptionEstablished_reportHandlerSelector (toNSNumber minInterval) (toNSNumber maxInterval) (toMTRSubscribeParams params) subscriptionEstablishedHandler reportHandler

-- | @+ readAttributeCatalogListWithAttributeCache:endpoint:queue:completionHandler:@
readAttributeCatalogListWithAttributeCache_endpoint_queue_completionHandler :: (IsMTRAttributeCacheContainer attributeCacheContainer, IsNSNumber endpoint, IsNSObject queue) => attributeCacheContainer -> endpoint -> queue -> Ptr () -> IO ()
readAttributeCatalogListWithAttributeCache_endpoint_queue_completionHandler attributeCacheContainer endpoint queue completionHandler =
  do
    cls' <- getRequiredClass "MTRBaseClusterApplicationLauncher"
    sendClassMessage cls' readAttributeCatalogListWithAttributeCache_endpoint_queue_completionHandlerSelector (toMTRAttributeCacheContainer attributeCacheContainer) (toNSNumber endpoint) (toNSObject queue) completionHandler

-- | @- readAttributeCurrentAppWithCompletionHandler:@
readAttributeCurrentAppWithCompletionHandler :: IsMTRBaseClusterApplicationLauncher mtrBaseClusterApplicationLauncher => mtrBaseClusterApplicationLauncher -> Ptr () -> IO ()
readAttributeCurrentAppWithCompletionHandler mtrBaseClusterApplicationLauncher completionHandler =
  sendMessage mtrBaseClusterApplicationLauncher readAttributeCurrentAppWithCompletionHandlerSelector completionHandler

-- | @- writeAttributeCurrentAppWithValue:completionHandler:@
writeAttributeCurrentAppWithValue_completionHandler :: (IsMTRBaseClusterApplicationLauncher mtrBaseClusterApplicationLauncher, IsMTRApplicationLauncherClusterApplicationEP value) => mtrBaseClusterApplicationLauncher -> value -> Ptr () -> IO ()
writeAttributeCurrentAppWithValue_completionHandler mtrBaseClusterApplicationLauncher value completionHandler =
  sendMessage mtrBaseClusterApplicationLauncher writeAttributeCurrentAppWithValue_completionHandlerSelector (toMTRApplicationLauncherClusterApplicationEP value) completionHandler

-- | @- writeAttributeCurrentAppWithValue:params:completionHandler:@
writeAttributeCurrentAppWithValue_params_completionHandler :: (IsMTRBaseClusterApplicationLauncher mtrBaseClusterApplicationLauncher, IsMTRApplicationLauncherClusterApplicationEP value, IsMTRWriteParams params) => mtrBaseClusterApplicationLauncher -> value -> params -> Ptr () -> IO ()
writeAttributeCurrentAppWithValue_params_completionHandler mtrBaseClusterApplicationLauncher value params completionHandler =
  sendMessage mtrBaseClusterApplicationLauncher writeAttributeCurrentAppWithValue_params_completionHandlerSelector (toMTRApplicationLauncherClusterApplicationEP value) (toMTRWriteParams params) completionHandler

-- | @- subscribeAttributeCurrentAppWithMinInterval:maxInterval:params:subscriptionEstablished:reportHandler:@
subscribeAttributeCurrentAppWithMinInterval_maxInterval_params_subscriptionEstablished_reportHandler :: (IsMTRBaseClusterApplicationLauncher mtrBaseClusterApplicationLauncher, IsNSNumber minInterval, IsNSNumber maxInterval, IsMTRSubscribeParams params) => mtrBaseClusterApplicationLauncher -> minInterval -> maxInterval -> params -> Ptr () -> Ptr () -> IO ()
subscribeAttributeCurrentAppWithMinInterval_maxInterval_params_subscriptionEstablished_reportHandler mtrBaseClusterApplicationLauncher minInterval maxInterval params subscriptionEstablishedHandler reportHandler =
  sendMessage mtrBaseClusterApplicationLauncher subscribeAttributeCurrentAppWithMinInterval_maxInterval_params_subscriptionEstablished_reportHandlerSelector (toNSNumber minInterval) (toNSNumber maxInterval) (toMTRSubscribeParams params) subscriptionEstablishedHandler reportHandler

-- | @+ readAttributeCurrentAppWithAttributeCache:endpoint:queue:completionHandler:@
readAttributeCurrentAppWithAttributeCache_endpoint_queue_completionHandler :: (IsMTRAttributeCacheContainer attributeCacheContainer, IsNSNumber endpoint, IsNSObject queue) => attributeCacheContainer -> endpoint -> queue -> Ptr () -> IO ()
readAttributeCurrentAppWithAttributeCache_endpoint_queue_completionHandler attributeCacheContainer endpoint queue completionHandler =
  do
    cls' <- getRequiredClass "MTRBaseClusterApplicationLauncher"
    sendClassMessage cls' readAttributeCurrentAppWithAttributeCache_endpoint_queue_completionHandlerSelector (toMTRAttributeCacheContainer attributeCacheContainer) (toNSNumber endpoint) (toNSObject queue) completionHandler

-- | @- readAttributeGeneratedCommandListWithCompletionHandler:@
readAttributeGeneratedCommandListWithCompletionHandler :: IsMTRBaseClusterApplicationLauncher mtrBaseClusterApplicationLauncher => mtrBaseClusterApplicationLauncher -> Ptr () -> IO ()
readAttributeGeneratedCommandListWithCompletionHandler mtrBaseClusterApplicationLauncher completionHandler =
  sendMessage mtrBaseClusterApplicationLauncher readAttributeGeneratedCommandListWithCompletionHandlerSelector completionHandler

-- | @- subscribeAttributeGeneratedCommandListWithMinInterval:maxInterval:params:subscriptionEstablished:reportHandler:@
subscribeAttributeGeneratedCommandListWithMinInterval_maxInterval_params_subscriptionEstablished_reportHandler :: (IsMTRBaseClusterApplicationLauncher mtrBaseClusterApplicationLauncher, IsNSNumber minInterval, IsNSNumber maxInterval, IsMTRSubscribeParams params) => mtrBaseClusterApplicationLauncher -> minInterval -> maxInterval -> params -> Ptr () -> Ptr () -> IO ()
subscribeAttributeGeneratedCommandListWithMinInterval_maxInterval_params_subscriptionEstablished_reportHandler mtrBaseClusterApplicationLauncher minInterval maxInterval params subscriptionEstablishedHandler reportHandler =
  sendMessage mtrBaseClusterApplicationLauncher subscribeAttributeGeneratedCommandListWithMinInterval_maxInterval_params_subscriptionEstablished_reportHandlerSelector (toNSNumber minInterval) (toNSNumber maxInterval) (toMTRSubscribeParams params) subscriptionEstablishedHandler reportHandler

-- | @+ readAttributeGeneratedCommandListWithAttributeCache:endpoint:queue:completionHandler:@
readAttributeGeneratedCommandListWithAttributeCache_endpoint_queue_completionHandler :: (IsMTRAttributeCacheContainer attributeCacheContainer, IsNSNumber endpoint, IsNSObject queue) => attributeCacheContainer -> endpoint -> queue -> Ptr () -> IO ()
readAttributeGeneratedCommandListWithAttributeCache_endpoint_queue_completionHandler attributeCacheContainer endpoint queue completionHandler =
  do
    cls' <- getRequiredClass "MTRBaseClusterApplicationLauncher"
    sendClassMessage cls' readAttributeGeneratedCommandListWithAttributeCache_endpoint_queue_completionHandlerSelector (toMTRAttributeCacheContainer attributeCacheContainer) (toNSNumber endpoint) (toNSObject queue) completionHandler

-- | @- readAttributeAcceptedCommandListWithCompletionHandler:@
readAttributeAcceptedCommandListWithCompletionHandler :: IsMTRBaseClusterApplicationLauncher mtrBaseClusterApplicationLauncher => mtrBaseClusterApplicationLauncher -> Ptr () -> IO ()
readAttributeAcceptedCommandListWithCompletionHandler mtrBaseClusterApplicationLauncher completionHandler =
  sendMessage mtrBaseClusterApplicationLauncher readAttributeAcceptedCommandListWithCompletionHandlerSelector completionHandler

-- | @- subscribeAttributeAcceptedCommandListWithMinInterval:maxInterval:params:subscriptionEstablished:reportHandler:@
subscribeAttributeAcceptedCommandListWithMinInterval_maxInterval_params_subscriptionEstablished_reportHandler :: (IsMTRBaseClusterApplicationLauncher mtrBaseClusterApplicationLauncher, IsNSNumber minInterval, IsNSNumber maxInterval, IsMTRSubscribeParams params) => mtrBaseClusterApplicationLauncher -> minInterval -> maxInterval -> params -> Ptr () -> Ptr () -> IO ()
subscribeAttributeAcceptedCommandListWithMinInterval_maxInterval_params_subscriptionEstablished_reportHandler mtrBaseClusterApplicationLauncher minInterval maxInterval params subscriptionEstablishedHandler reportHandler =
  sendMessage mtrBaseClusterApplicationLauncher subscribeAttributeAcceptedCommandListWithMinInterval_maxInterval_params_subscriptionEstablished_reportHandlerSelector (toNSNumber minInterval) (toNSNumber maxInterval) (toMTRSubscribeParams params) subscriptionEstablishedHandler reportHandler

-- | @+ readAttributeAcceptedCommandListWithAttributeCache:endpoint:queue:completionHandler:@
readAttributeAcceptedCommandListWithAttributeCache_endpoint_queue_completionHandler :: (IsMTRAttributeCacheContainer attributeCacheContainer, IsNSNumber endpoint, IsNSObject queue) => attributeCacheContainer -> endpoint -> queue -> Ptr () -> IO ()
readAttributeAcceptedCommandListWithAttributeCache_endpoint_queue_completionHandler attributeCacheContainer endpoint queue completionHandler =
  do
    cls' <- getRequiredClass "MTRBaseClusterApplicationLauncher"
    sendClassMessage cls' readAttributeAcceptedCommandListWithAttributeCache_endpoint_queue_completionHandlerSelector (toMTRAttributeCacheContainer attributeCacheContainer) (toNSNumber endpoint) (toNSObject queue) completionHandler

-- | @- readAttributeAttributeListWithCompletionHandler:@
readAttributeAttributeListWithCompletionHandler :: IsMTRBaseClusterApplicationLauncher mtrBaseClusterApplicationLauncher => mtrBaseClusterApplicationLauncher -> Ptr () -> IO ()
readAttributeAttributeListWithCompletionHandler mtrBaseClusterApplicationLauncher completionHandler =
  sendMessage mtrBaseClusterApplicationLauncher readAttributeAttributeListWithCompletionHandlerSelector completionHandler

-- | @- subscribeAttributeAttributeListWithMinInterval:maxInterval:params:subscriptionEstablished:reportHandler:@
subscribeAttributeAttributeListWithMinInterval_maxInterval_params_subscriptionEstablished_reportHandler :: (IsMTRBaseClusterApplicationLauncher mtrBaseClusterApplicationLauncher, IsNSNumber minInterval, IsNSNumber maxInterval, IsMTRSubscribeParams params) => mtrBaseClusterApplicationLauncher -> minInterval -> maxInterval -> params -> Ptr () -> Ptr () -> IO ()
subscribeAttributeAttributeListWithMinInterval_maxInterval_params_subscriptionEstablished_reportHandler mtrBaseClusterApplicationLauncher minInterval maxInterval params subscriptionEstablishedHandler reportHandler =
  sendMessage mtrBaseClusterApplicationLauncher subscribeAttributeAttributeListWithMinInterval_maxInterval_params_subscriptionEstablished_reportHandlerSelector (toNSNumber minInterval) (toNSNumber maxInterval) (toMTRSubscribeParams params) subscriptionEstablishedHandler reportHandler

-- | @+ readAttributeAttributeListWithAttributeCache:endpoint:queue:completionHandler:@
readAttributeAttributeListWithAttributeCache_endpoint_queue_completionHandler :: (IsMTRAttributeCacheContainer attributeCacheContainer, IsNSNumber endpoint, IsNSObject queue) => attributeCacheContainer -> endpoint -> queue -> Ptr () -> IO ()
readAttributeAttributeListWithAttributeCache_endpoint_queue_completionHandler attributeCacheContainer endpoint queue completionHandler =
  do
    cls' <- getRequiredClass "MTRBaseClusterApplicationLauncher"
    sendClassMessage cls' readAttributeAttributeListWithAttributeCache_endpoint_queue_completionHandlerSelector (toMTRAttributeCacheContainer attributeCacheContainer) (toNSNumber endpoint) (toNSObject queue) completionHandler

-- | @- readAttributeFeatureMapWithCompletionHandler:@
readAttributeFeatureMapWithCompletionHandler :: IsMTRBaseClusterApplicationLauncher mtrBaseClusterApplicationLauncher => mtrBaseClusterApplicationLauncher -> Ptr () -> IO ()
readAttributeFeatureMapWithCompletionHandler mtrBaseClusterApplicationLauncher completionHandler =
  sendMessage mtrBaseClusterApplicationLauncher readAttributeFeatureMapWithCompletionHandlerSelector completionHandler

-- | @- subscribeAttributeFeatureMapWithMinInterval:maxInterval:params:subscriptionEstablished:reportHandler:@
subscribeAttributeFeatureMapWithMinInterval_maxInterval_params_subscriptionEstablished_reportHandler :: (IsMTRBaseClusterApplicationLauncher mtrBaseClusterApplicationLauncher, IsNSNumber minInterval, IsNSNumber maxInterval, IsMTRSubscribeParams params) => mtrBaseClusterApplicationLauncher -> minInterval -> maxInterval -> params -> Ptr () -> Ptr () -> IO ()
subscribeAttributeFeatureMapWithMinInterval_maxInterval_params_subscriptionEstablished_reportHandler mtrBaseClusterApplicationLauncher minInterval maxInterval params subscriptionEstablishedHandler reportHandler =
  sendMessage mtrBaseClusterApplicationLauncher subscribeAttributeFeatureMapWithMinInterval_maxInterval_params_subscriptionEstablished_reportHandlerSelector (toNSNumber minInterval) (toNSNumber maxInterval) (toMTRSubscribeParams params) subscriptionEstablishedHandler reportHandler

-- | @+ readAttributeFeatureMapWithAttributeCache:endpoint:queue:completionHandler:@
readAttributeFeatureMapWithAttributeCache_endpoint_queue_completionHandler :: (IsMTRAttributeCacheContainer attributeCacheContainer, IsNSNumber endpoint, IsNSObject queue) => attributeCacheContainer -> endpoint -> queue -> Ptr () -> IO ()
readAttributeFeatureMapWithAttributeCache_endpoint_queue_completionHandler attributeCacheContainer endpoint queue completionHandler =
  do
    cls' <- getRequiredClass "MTRBaseClusterApplicationLauncher"
    sendClassMessage cls' readAttributeFeatureMapWithAttributeCache_endpoint_queue_completionHandlerSelector (toMTRAttributeCacheContainer attributeCacheContainer) (toNSNumber endpoint) (toNSObject queue) completionHandler

-- | @- readAttributeClusterRevisionWithCompletionHandler:@
readAttributeClusterRevisionWithCompletionHandler :: IsMTRBaseClusterApplicationLauncher mtrBaseClusterApplicationLauncher => mtrBaseClusterApplicationLauncher -> Ptr () -> IO ()
readAttributeClusterRevisionWithCompletionHandler mtrBaseClusterApplicationLauncher completionHandler =
  sendMessage mtrBaseClusterApplicationLauncher readAttributeClusterRevisionWithCompletionHandlerSelector completionHandler

-- | @- subscribeAttributeClusterRevisionWithMinInterval:maxInterval:params:subscriptionEstablished:reportHandler:@
subscribeAttributeClusterRevisionWithMinInterval_maxInterval_params_subscriptionEstablished_reportHandler :: (IsMTRBaseClusterApplicationLauncher mtrBaseClusterApplicationLauncher, IsNSNumber minInterval, IsNSNumber maxInterval, IsMTRSubscribeParams params) => mtrBaseClusterApplicationLauncher -> minInterval -> maxInterval -> params -> Ptr () -> Ptr () -> IO ()
subscribeAttributeClusterRevisionWithMinInterval_maxInterval_params_subscriptionEstablished_reportHandler mtrBaseClusterApplicationLauncher minInterval maxInterval params subscriptionEstablishedHandler reportHandler =
  sendMessage mtrBaseClusterApplicationLauncher subscribeAttributeClusterRevisionWithMinInterval_maxInterval_params_subscriptionEstablished_reportHandlerSelector (toNSNumber minInterval) (toNSNumber maxInterval) (toMTRSubscribeParams params) subscriptionEstablishedHandler reportHandler

-- | @+ readAttributeClusterRevisionWithAttributeCache:endpoint:queue:completionHandler:@
readAttributeClusterRevisionWithAttributeCache_endpoint_queue_completionHandler :: (IsMTRAttributeCacheContainer attributeCacheContainer, IsNSNumber endpoint, IsNSObject queue) => attributeCacheContainer -> endpoint -> queue -> Ptr () -> IO ()
readAttributeClusterRevisionWithAttributeCache_endpoint_queue_completionHandler attributeCacheContainer endpoint queue completionHandler =
  do
    cls' <- getRequiredClass "MTRBaseClusterApplicationLauncher"
    sendClassMessage cls' readAttributeClusterRevisionWithAttributeCache_endpoint_queue_completionHandlerSelector (toMTRAttributeCacheContainer attributeCacheContainer) (toNSNumber endpoint) (toNSObject queue) completionHandler

-- | For all instance methods (reads, writes, commands) that take a completion, the completion will be called on the provided queue.
--
-- ObjC selector: @- initWithDevice:endpointID:queue:@
initWithDevice_endpointID_queue :: (IsMTRBaseClusterApplicationLauncher mtrBaseClusterApplicationLauncher, IsMTRBaseDevice device, IsNSNumber endpointID, IsNSObject queue) => mtrBaseClusterApplicationLauncher -> device -> endpointID -> queue -> IO (Id MTRBaseClusterApplicationLauncher)
initWithDevice_endpointID_queue mtrBaseClusterApplicationLauncher device endpointID queue =
  sendOwnedMessage mtrBaseClusterApplicationLauncher initWithDevice_endpointID_queueSelector (toMTRBaseDevice device) (toNSNumber endpointID) (toNSObject queue)

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @launchAppWithParams:completion:@
launchAppWithParams_completionSelector :: Selector '[Id MTRApplicationLauncherClusterLaunchAppParams, Ptr ()] ()
launchAppWithParams_completionSelector = mkSelector "launchAppWithParams:completion:"

-- | @Selector@ for @launchAppWithCompletion:@
launchAppWithCompletionSelector :: Selector '[Ptr ()] ()
launchAppWithCompletionSelector = mkSelector "launchAppWithCompletion:"

-- | @Selector@ for @stopAppWithParams:completion:@
stopAppWithParams_completionSelector :: Selector '[Id MTRApplicationLauncherClusterStopAppParams, Ptr ()] ()
stopAppWithParams_completionSelector = mkSelector "stopAppWithParams:completion:"

-- | @Selector@ for @stopAppWithCompletion:@
stopAppWithCompletionSelector :: Selector '[Ptr ()] ()
stopAppWithCompletionSelector = mkSelector "stopAppWithCompletion:"

-- | @Selector@ for @hideAppWithParams:completion:@
hideAppWithParams_completionSelector :: Selector '[Id MTRApplicationLauncherClusterHideAppParams, Ptr ()] ()
hideAppWithParams_completionSelector = mkSelector "hideAppWithParams:completion:"

-- | @Selector@ for @hideAppWithCompletion:@
hideAppWithCompletionSelector :: Selector '[Ptr ()] ()
hideAppWithCompletionSelector = mkSelector "hideAppWithCompletion:"

-- | @Selector@ for @readAttributeCatalogListWithCompletion:@
readAttributeCatalogListWithCompletionSelector :: Selector '[Ptr ()] ()
readAttributeCatalogListWithCompletionSelector = mkSelector "readAttributeCatalogListWithCompletion:"

-- | @Selector@ for @subscribeAttributeCatalogListWithParams:subscriptionEstablished:reportHandler:@
subscribeAttributeCatalogListWithParams_subscriptionEstablished_reportHandlerSelector :: Selector '[Id MTRSubscribeParams, Ptr (), Ptr ()] ()
subscribeAttributeCatalogListWithParams_subscriptionEstablished_reportHandlerSelector = mkSelector "subscribeAttributeCatalogListWithParams:subscriptionEstablished:reportHandler:"

-- | @Selector@ for @readAttributeCatalogListWithClusterStateCache:endpoint:queue:completion:@
readAttributeCatalogListWithClusterStateCache_endpoint_queue_completionSelector :: Selector '[Id MTRClusterStateCacheContainer, Id NSNumber, Id NSObject, Ptr ()] ()
readAttributeCatalogListWithClusterStateCache_endpoint_queue_completionSelector = mkSelector "readAttributeCatalogListWithClusterStateCache:endpoint:queue:completion:"

-- | @Selector@ for @readAttributeCurrentAppWithCompletion:@
readAttributeCurrentAppWithCompletionSelector :: Selector '[Ptr ()] ()
readAttributeCurrentAppWithCompletionSelector = mkSelector "readAttributeCurrentAppWithCompletion:"

-- | @Selector@ for @writeAttributeCurrentAppWithValue:completion:@
writeAttributeCurrentAppWithValue_completionSelector :: Selector '[Id MTRApplicationLauncherClusterApplicationEPStruct, Ptr ()] ()
writeAttributeCurrentAppWithValue_completionSelector = mkSelector "writeAttributeCurrentAppWithValue:completion:"

-- | @Selector@ for @writeAttributeCurrentAppWithValue:params:completion:@
writeAttributeCurrentAppWithValue_params_completionSelector :: Selector '[Id MTRApplicationLauncherClusterApplicationEPStruct, Id MTRWriteParams, Ptr ()] ()
writeAttributeCurrentAppWithValue_params_completionSelector = mkSelector "writeAttributeCurrentAppWithValue:params:completion:"

-- | @Selector@ for @subscribeAttributeCurrentAppWithParams:subscriptionEstablished:reportHandler:@
subscribeAttributeCurrentAppWithParams_subscriptionEstablished_reportHandlerSelector :: Selector '[Id MTRSubscribeParams, Ptr (), Ptr ()] ()
subscribeAttributeCurrentAppWithParams_subscriptionEstablished_reportHandlerSelector = mkSelector "subscribeAttributeCurrentAppWithParams:subscriptionEstablished:reportHandler:"

-- | @Selector@ for @readAttributeCurrentAppWithClusterStateCache:endpoint:queue:completion:@
readAttributeCurrentAppWithClusterStateCache_endpoint_queue_completionSelector :: Selector '[Id MTRClusterStateCacheContainer, Id NSNumber, Id NSObject, Ptr ()] ()
readAttributeCurrentAppWithClusterStateCache_endpoint_queue_completionSelector = mkSelector "readAttributeCurrentAppWithClusterStateCache:endpoint:queue:completion:"

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
initSelector :: Selector '[] (Id MTRBaseClusterApplicationLauncher)
initSelector = mkSelector "init"

-- | @Selector@ for @new@
newSelector :: Selector '[] (Id MTRBaseClusterApplicationLauncher)
newSelector = mkSelector "new"

-- | @Selector@ for @initWithDevice:endpoint:queue:@
initWithDevice_endpoint_queueSelector :: Selector '[Id MTRBaseDevice, CUShort, Id NSObject] (Id MTRBaseClusterApplicationLauncher)
initWithDevice_endpoint_queueSelector = mkSelector "initWithDevice:endpoint:queue:"

-- | @Selector@ for @launchAppWithParams:completionHandler:@
launchAppWithParams_completionHandlerSelector :: Selector '[Id MTRApplicationLauncherClusterLaunchAppParams, Ptr ()] ()
launchAppWithParams_completionHandlerSelector = mkSelector "launchAppWithParams:completionHandler:"

-- | @Selector@ for @stopAppWithParams:completionHandler:@
stopAppWithParams_completionHandlerSelector :: Selector '[Id MTRApplicationLauncherClusterStopAppParams, Ptr ()] ()
stopAppWithParams_completionHandlerSelector = mkSelector "stopAppWithParams:completionHandler:"

-- | @Selector@ for @hideAppWithParams:completionHandler:@
hideAppWithParams_completionHandlerSelector :: Selector '[Id MTRApplicationLauncherClusterHideAppParams, Ptr ()] ()
hideAppWithParams_completionHandlerSelector = mkSelector "hideAppWithParams:completionHandler:"

-- | @Selector@ for @readAttributeCatalogListWithCompletionHandler:@
readAttributeCatalogListWithCompletionHandlerSelector :: Selector '[Ptr ()] ()
readAttributeCatalogListWithCompletionHandlerSelector = mkSelector "readAttributeCatalogListWithCompletionHandler:"

-- | @Selector@ for @subscribeAttributeCatalogListWithMinInterval:maxInterval:params:subscriptionEstablished:reportHandler:@
subscribeAttributeCatalogListWithMinInterval_maxInterval_params_subscriptionEstablished_reportHandlerSelector :: Selector '[Id NSNumber, Id NSNumber, Id MTRSubscribeParams, Ptr (), Ptr ()] ()
subscribeAttributeCatalogListWithMinInterval_maxInterval_params_subscriptionEstablished_reportHandlerSelector = mkSelector "subscribeAttributeCatalogListWithMinInterval:maxInterval:params:subscriptionEstablished:reportHandler:"

-- | @Selector@ for @readAttributeCatalogListWithAttributeCache:endpoint:queue:completionHandler:@
readAttributeCatalogListWithAttributeCache_endpoint_queue_completionHandlerSelector :: Selector '[Id MTRAttributeCacheContainer, Id NSNumber, Id NSObject, Ptr ()] ()
readAttributeCatalogListWithAttributeCache_endpoint_queue_completionHandlerSelector = mkSelector "readAttributeCatalogListWithAttributeCache:endpoint:queue:completionHandler:"

-- | @Selector@ for @readAttributeCurrentAppWithCompletionHandler:@
readAttributeCurrentAppWithCompletionHandlerSelector :: Selector '[Ptr ()] ()
readAttributeCurrentAppWithCompletionHandlerSelector = mkSelector "readAttributeCurrentAppWithCompletionHandler:"

-- | @Selector@ for @writeAttributeCurrentAppWithValue:completionHandler:@
writeAttributeCurrentAppWithValue_completionHandlerSelector :: Selector '[Id MTRApplicationLauncherClusterApplicationEP, Ptr ()] ()
writeAttributeCurrentAppWithValue_completionHandlerSelector = mkSelector "writeAttributeCurrentAppWithValue:completionHandler:"

-- | @Selector@ for @writeAttributeCurrentAppWithValue:params:completionHandler:@
writeAttributeCurrentAppWithValue_params_completionHandlerSelector :: Selector '[Id MTRApplicationLauncherClusterApplicationEP, Id MTRWriteParams, Ptr ()] ()
writeAttributeCurrentAppWithValue_params_completionHandlerSelector = mkSelector "writeAttributeCurrentAppWithValue:params:completionHandler:"

-- | @Selector@ for @subscribeAttributeCurrentAppWithMinInterval:maxInterval:params:subscriptionEstablished:reportHandler:@
subscribeAttributeCurrentAppWithMinInterval_maxInterval_params_subscriptionEstablished_reportHandlerSelector :: Selector '[Id NSNumber, Id NSNumber, Id MTRSubscribeParams, Ptr (), Ptr ()] ()
subscribeAttributeCurrentAppWithMinInterval_maxInterval_params_subscriptionEstablished_reportHandlerSelector = mkSelector "subscribeAttributeCurrentAppWithMinInterval:maxInterval:params:subscriptionEstablished:reportHandler:"

-- | @Selector@ for @readAttributeCurrentAppWithAttributeCache:endpoint:queue:completionHandler:@
readAttributeCurrentAppWithAttributeCache_endpoint_queue_completionHandlerSelector :: Selector '[Id MTRAttributeCacheContainer, Id NSNumber, Id NSObject, Ptr ()] ()
readAttributeCurrentAppWithAttributeCache_endpoint_queue_completionHandlerSelector = mkSelector "readAttributeCurrentAppWithAttributeCache:endpoint:queue:completionHandler:"

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
initWithDevice_endpointID_queueSelector :: Selector '[Id MTRBaseDevice, Id NSNumber, Id NSObject] (Id MTRBaseClusterApplicationLauncher)
initWithDevice_endpointID_queueSelector = mkSelector "initWithDevice:endpointID:queue:"

