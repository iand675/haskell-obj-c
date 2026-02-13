{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Cluster Chime
--
-- This cluster provides facilities to configure and play Chime sounds, such as those used in a doorbell.
--
-- Generated bindings for @MTRBaseClusterChime@.
module ObjC.Matter.MTRBaseClusterChime
  ( MTRBaseClusterChime
  , IsMTRBaseClusterChime(..)
  , playChimeSoundWithParams_completion
  , playChimeSoundWithCompletion
  , readAttributeInstalledChimeSoundsWithCompletion
  , subscribeAttributeInstalledChimeSoundsWithParams_subscriptionEstablished_reportHandler
  , readAttributeInstalledChimeSoundsWithClusterStateCache_endpoint_queue_completion
  , readAttributeSelectedChimeWithCompletion
  , writeAttributeSelectedChimeWithValue_completion
  , writeAttributeSelectedChimeWithValue_params_completion
  , subscribeAttributeSelectedChimeWithParams_subscriptionEstablished_reportHandler
  , readAttributeSelectedChimeWithClusterStateCache_endpoint_queue_completion
  , readAttributeEnabledWithCompletion
  , writeAttributeEnabledWithValue_completion
  , writeAttributeEnabledWithValue_params_completion
  , subscribeAttributeEnabledWithParams_subscriptionEstablished_reportHandler
  , readAttributeEnabledWithClusterStateCache_endpoint_queue_completion
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
  , playChimeSoundWithCompletionSelector
  , playChimeSoundWithParams_completionSelector
  , readAttributeAcceptedCommandListWithClusterStateCache_endpoint_queue_completionSelector
  , readAttributeAcceptedCommandListWithCompletionSelector
  , readAttributeAttributeListWithClusterStateCache_endpoint_queue_completionSelector
  , readAttributeAttributeListWithCompletionSelector
  , readAttributeClusterRevisionWithClusterStateCache_endpoint_queue_completionSelector
  , readAttributeClusterRevisionWithCompletionSelector
  , readAttributeEnabledWithClusterStateCache_endpoint_queue_completionSelector
  , readAttributeEnabledWithCompletionSelector
  , readAttributeFeatureMapWithClusterStateCache_endpoint_queue_completionSelector
  , readAttributeFeatureMapWithCompletionSelector
  , readAttributeGeneratedCommandListWithClusterStateCache_endpoint_queue_completionSelector
  , readAttributeGeneratedCommandListWithCompletionSelector
  , readAttributeInstalledChimeSoundsWithClusterStateCache_endpoint_queue_completionSelector
  , readAttributeInstalledChimeSoundsWithCompletionSelector
  , readAttributeSelectedChimeWithClusterStateCache_endpoint_queue_completionSelector
  , readAttributeSelectedChimeWithCompletionSelector
  , subscribeAttributeAcceptedCommandListWithParams_subscriptionEstablished_reportHandlerSelector
  , subscribeAttributeAttributeListWithParams_subscriptionEstablished_reportHandlerSelector
  , subscribeAttributeClusterRevisionWithParams_subscriptionEstablished_reportHandlerSelector
  , subscribeAttributeEnabledWithParams_subscriptionEstablished_reportHandlerSelector
  , subscribeAttributeFeatureMapWithParams_subscriptionEstablished_reportHandlerSelector
  , subscribeAttributeGeneratedCommandListWithParams_subscriptionEstablished_reportHandlerSelector
  , subscribeAttributeInstalledChimeSoundsWithParams_subscriptionEstablished_reportHandlerSelector
  , subscribeAttributeSelectedChimeWithParams_subscriptionEstablished_reportHandlerSelector
  , writeAttributeEnabledWithValue_completionSelector
  , writeAttributeEnabledWithValue_params_completionSelector
  , writeAttributeSelectedChimeWithValue_completionSelector
  , writeAttributeSelectedChimeWithValue_params_completionSelector


  ) where

import Foreign.Ptr (Ptr, FunPtr)
import Foreign.C.Types

import ObjC.Runtime.Types
import ObjC.Runtime.Message (sendMessage, sendOwnedMessage, sendClassMessage, sendOwnedClassMessage)
import ObjC.Runtime.Selector (mkSelector)
import ObjC.Runtime.Class (getRequiredClass)

import ObjC.Matter.Internal.Classes
import ObjC.Foundation.Internal.Classes

-- | Command PlayChimeSound
--
-- ObjC selector: @- playChimeSoundWithParams:completion:@
playChimeSoundWithParams_completion :: (IsMTRBaseClusterChime mtrBaseClusterChime, IsMTRChimeClusterPlayChimeSoundParams params) => mtrBaseClusterChime -> params -> Ptr () -> IO ()
playChimeSoundWithParams_completion mtrBaseClusterChime params completion =
  sendMessage mtrBaseClusterChime playChimeSoundWithParams_completionSelector (toMTRChimeClusterPlayChimeSoundParams params) completion

-- | @- playChimeSoundWithCompletion:@
playChimeSoundWithCompletion :: IsMTRBaseClusterChime mtrBaseClusterChime => mtrBaseClusterChime -> Ptr () -> IO ()
playChimeSoundWithCompletion mtrBaseClusterChime completion =
  sendMessage mtrBaseClusterChime playChimeSoundWithCompletionSelector completion

-- | @- readAttributeInstalledChimeSoundsWithCompletion:@
readAttributeInstalledChimeSoundsWithCompletion :: IsMTRBaseClusterChime mtrBaseClusterChime => mtrBaseClusterChime -> Ptr () -> IO ()
readAttributeInstalledChimeSoundsWithCompletion mtrBaseClusterChime completion =
  sendMessage mtrBaseClusterChime readAttributeInstalledChimeSoundsWithCompletionSelector completion

-- | @- subscribeAttributeInstalledChimeSoundsWithParams:subscriptionEstablished:reportHandler:@
subscribeAttributeInstalledChimeSoundsWithParams_subscriptionEstablished_reportHandler :: (IsMTRBaseClusterChime mtrBaseClusterChime, IsMTRSubscribeParams params) => mtrBaseClusterChime -> params -> Ptr () -> Ptr () -> IO ()
subscribeAttributeInstalledChimeSoundsWithParams_subscriptionEstablished_reportHandler mtrBaseClusterChime params subscriptionEstablished reportHandler =
  sendMessage mtrBaseClusterChime subscribeAttributeInstalledChimeSoundsWithParams_subscriptionEstablished_reportHandlerSelector (toMTRSubscribeParams params) subscriptionEstablished reportHandler

-- | @+ readAttributeInstalledChimeSoundsWithClusterStateCache:endpoint:queue:completion:@
readAttributeInstalledChimeSoundsWithClusterStateCache_endpoint_queue_completion :: (IsMTRClusterStateCacheContainer clusterStateCacheContainer, IsNSNumber endpoint, IsNSObject queue) => clusterStateCacheContainer -> endpoint -> queue -> Ptr () -> IO ()
readAttributeInstalledChimeSoundsWithClusterStateCache_endpoint_queue_completion clusterStateCacheContainer endpoint queue completion =
  do
    cls' <- getRequiredClass "MTRBaseClusterChime"
    sendClassMessage cls' readAttributeInstalledChimeSoundsWithClusterStateCache_endpoint_queue_completionSelector (toMTRClusterStateCacheContainer clusterStateCacheContainer) (toNSNumber endpoint) (toNSObject queue) completion

-- | @- readAttributeSelectedChimeWithCompletion:@
readAttributeSelectedChimeWithCompletion :: IsMTRBaseClusterChime mtrBaseClusterChime => mtrBaseClusterChime -> Ptr () -> IO ()
readAttributeSelectedChimeWithCompletion mtrBaseClusterChime completion =
  sendMessage mtrBaseClusterChime readAttributeSelectedChimeWithCompletionSelector completion

-- | @- writeAttributeSelectedChimeWithValue:completion:@
writeAttributeSelectedChimeWithValue_completion :: (IsMTRBaseClusterChime mtrBaseClusterChime, IsNSNumber value) => mtrBaseClusterChime -> value -> Ptr () -> IO ()
writeAttributeSelectedChimeWithValue_completion mtrBaseClusterChime value completion =
  sendMessage mtrBaseClusterChime writeAttributeSelectedChimeWithValue_completionSelector (toNSNumber value) completion

-- | @- writeAttributeSelectedChimeWithValue:params:completion:@
writeAttributeSelectedChimeWithValue_params_completion :: (IsMTRBaseClusterChime mtrBaseClusterChime, IsNSNumber value, IsMTRWriteParams params) => mtrBaseClusterChime -> value -> params -> Ptr () -> IO ()
writeAttributeSelectedChimeWithValue_params_completion mtrBaseClusterChime value params completion =
  sendMessage mtrBaseClusterChime writeAttributeSelectedChimeWithValue_params_completionSelector (toNSNumber value) (toMTRWriteParams params) completion

-- | @- subscribeAttributeSelectedChimeWithParams:subscriptionEstablished:reportHandler:@
subscribeAttributeSelectedChimeWithParams_subscriptionEstablished_reportHandler :: (IsMTRBaseClusterChime mtrBaseClusterChime, IsMTRSubscribeParams params) => mtrBaseClusterChime -> params -> Ptr () -> Ptr () -> IO ()
subscribeAttributeSelectedChimeWithParams_subscriptionEstablished_reportHandler mtrBaseClusterChime params subscriptionEstablished reportHandler =
  sendMessage mtrBaseClusterChime subscribeAttributeSelectedChimeWithParams_subscriptionEstablished_reportHandlerSelector (toMTRSubscribeParams params) subscriptionEstablished reportHandler

-- | @+ readAttributeSelectedChimeWithClusterStateCache:endpoint:queue:completion:@
readAttributeSelectedChimeWithClusterStateCache_endpoint_queue_completion :: (IsMTRClusterStateCacheContainer clusterStateCacheContainer, IsNSNumber endpoint, IsNSObject queue) => clusterStateCacheContainer -> endpoint -> queue -> Ptr () -> IO ()
readAttributeSelectedChimeWithClusterStateCache_endpoint_queue_completion clusterStateCacheContainer endpoint queue completion =
  do
    cls' <- getRequiredClass "MTRBaseClusterChime"
    sendClassMessage cls' readAttributeSelectedChimeWithClusterStateCache_endpoint_queue_completionSelector (toMTRClusterStateCacheContainer clusterStateCacheContainer) (toNSNumber endpoint) (toNSObject queue) completion

-- | @- readAttributeEnabledWithCompletion:@
readAttributeEnabledWithCompletion :: IsMTRBaseClusterChime mtrBaseClusterChime => mtrBaseClusterChime -> Ptr () -> IO ()
readAttributeEnabledWithCompletion mtrBaseClusterChime completion =
  sendMessage mtrBaseClusterChime readAttributeEnabledWithCompletionSelector completion

-- | @- writeAttributeEnabledWithValue:completion:@
writeAttributeEnabledWithValue_completion :: (IsMTRBaseClusterChime mtrBaseClusterChime, IsNSNumber value) => mtrBaseClusterChime -> value -> Ptr () -> IO ()
writeAttributeEnabledWithValue_completion mtrBaseClusterChime value completion =
  sendMessage mtrBaseClusterChime writeAttributeEnabledWithValue_completionSelector (toNSNumber value) completion

-- | @- writeAttributeEnabledWithValue:params:completion:@
writeAttributeEnabledWithValue_params_completion :: (IsMTRBaseClusterChime mtrBaseClusterChime, IsNSNumber value, IsMTRWriteParams params) => mtrBaseClusterChime -> value -> params -> Ptr () -> IO ()
writeAttributeEnabledWithValue_params_completion mtrBaseClusterChime value params completion =
  sendMessage mtrBaseClusterChime writeAttributeEnabledWithValue_params_completionSelector (toNSNumber value) (toMTRWriteParams params) completion

-- | @- subscribeAttributeEnabledWithParams:subscriptionEstablished:reportHandler:@
subscribeAttributeEnabledWithParams_subscriptionEstablished_reportHandler :: (IsMTRBaseClusterChime mtrBaseClusterChime, IsMTRSubscribeParams params) => mtrBaseClusterChime -> params -> Ptr () -> Ptr () -> IO ()
subscribeAttributeEnabledWithParams_subscriptionEstablished_reportHandler mtrBaseClusterChime params subscriptionEstablished reportHandler =
  sendMessage mtrBaseClusterChime subscribeAttributeEnabledWithParams_subscriptionEstablished_reportHandlerSelector (toMTRSubscribeParams params) subscriptionEstablished reportHandler

-- | @+ readAttributeEnabledWithClusterStateCache:endpoint:queue:completion:@
readAttributeEnabledWithClusterStateCache_endpoint_queue_completion :: (IsMTRClusterStateCacheContainer clusterStateCacheContainer, IsNSNumber endpoint, IsNSObject queue) => clusterStateCacheContainer -> endpoint -> queue -> Ptr () -> IO ()
readAttributeEnabledWithClusterStateCache_endpoint_queue_completion clusterStateCacheContainer endpoint queue completion =
  do
    cls' <- getRequiredClass "MTRBaseClusterChime"
    sendClassMessage cls' readAttributeEnabledWithClusterStateCache_endpoint_queue_completionSelector (toMTRClusterStateCacheContainer clusterStateCacheContainer) (toNSNumber endpoint) (toNSObject queue) completion

-- | @- readAttributeGeneratedCommandListWithCompletion:@
readAttributeGeneratedCommandListWithCompletion :: IsMTRBaseClusterChime mtrBaseClusterChime => mtrBaseClusterChime -> Ptr () -> IO ()
readAttributeGeneratedCommandListWithCompletion mtrBaseClusterChime completion =
  sendMessage mtrBaseClusterChime readAttributeGeneratedCommandListWithCompletionSelector completion

-- | @- subscribeAttributeGeneratedCommandListWithParams:subscriptionEstablished:reportHandler:@
subscribeAttributeGeneratedCommandListWithParams_subscriptionEstablished_reportHandler :: (IsMTRBaseClusterChime mtrBaseClusterChime, IsMTRSubscribeParams params) => mtrBaseClusterChime -> params -> Ptr () -> Ptr () -> IO ()
subscribeAttributeGeneratedCommandListWithParams_subscriptionEstablished_reportHandler mtrBaseClusterChime params subscriptionEstablished reportHandler =
  sendMessage mtrBaseClusterChime subscribeAttributeGeneratedCommandListWithParams_subscriptionEstablished_reportHandlerSelector (toMTRSubscribeParams params) subscriptionEstablished reportHandler

-- | @+ readAttributeGeneratedCommandListWithClusterStateCache:endpoint:queue:completion:@
readAttributeGeneratedCommandListWithClusterStateCache_endpoint_queue_completion :: (IsMTRClusterStateCacheContainer clusterStateCacheContainer, IsNSNumber endpoint, IsNSObject queue) => clusterStateCacheContainer -> endpoint -> queue -> Ptr () -> IO ()
readAttributeGeneratedCommandListWithClusterStateCache_endpoint_queue_completion clusterStateCacheContainer endpoint queue completion =
  do
    cls' <- getRequiredClass "MTRBaseClusterChime"
    sendClassMessage cls' readAttributeGeneratedCommandListWithClusterStateCache_endpoint_queue_completionSelector (toMTRClusterStateCacheContainer clusterStateCacheContainer) (toNSNumber endpoint) (toNSObject queue) completion

-- | @- readAttributeAcceptedCommandListWithCompletion:@
readAttributeAcceptedCommandListWithCompletion :: IsMTRBaseClusterChime mtrBaseClusterChime => mtrBaseClusterChime -> Ptr () -> IO ()
readAttributeAcceptedCommandListWithCompletion mtrBaseClusterChime completion =
  sendMessage mtrBaseClusterChime readAttributeAcceptedCommandListWithCompletionSelector completion

-- | @- subscribeAttributeAcceptedCommandListWithParams:subscriptionEstablished:reportHandler:@
subscribeAttributeAcceptedCommandListWithParams_subscriptionEstablished_reportHandler :: (IsMTRBaseClusterChime mtrBaseClusterChime, IsMTRSubscribeParams params) => mtrBaseClusterChime -> params -> Ptr () -> Ptr () -> IO ()
subscribeAttributeAcceptedCommandListWithParams_subscriptionEstablished_reportHandler mtrBaseClusterChime params subscriptionEstablished reportHandler =
  sendMessage mtrBaseClusterChime subscribeAttributeAcceptedCommandListWithParams_subscriptionEstablished_reportHandlerSelector (toMTRSubscribeParams params) subscriptionEstablished reportHandler

-- | @+ readAttributeAcceptedCommandListWithClusterStateCache:endpoint:queue:completion:@
readAttributeAcceptedCommandListWithClusterStateCache_endpoint_queue_completion :: (IsMTRClusterStateCacheContainer clusterStateCacheContainer, IsNSNumber endpoint, IsNSObject queue) => clusterStateCacheContainer -> endpoint -> queue -> Ptr () -> IO ()
readAttributeAcceptedCommandListWithClusterStateCache_endpoint_queue_completion clusterStateCacheContainer endpoint queue completion =
  do
    cls' <- getRequiredClass "MTRBaseClusterChime"
    sendClassMessage cls' readAttributeAcceptedCommandListWithClusterStateCache_endpoint_queue_completionSelector (toMTRClusterStateCacheContainer clusterStateCacheContainer) (toNSNumber endpoint) (toNSObject queue) completion

-- | @- readAttributeAttributeListWithCompletion:@
readAttributeAttributeListWithCompletion :: IsMTRBaseClusterChime mtrBaseClusterChime => mtrBaseClusterChime -> Ptr () -> IO ()
readAttributeAttributeListWithCompletion mtrBaseClusterChime completion =
  sendMessage mtrBaseClusterChime readAttributeAttributeListWithCompletionSelector completion

-- | @- subscribeAttributeAttributeListWithParams:subscriptionEstablished:reportHandler:@
subscribeAttributeAttributeListWithParams_subscriptionEstablished_reportHandler :: (IsMTRBaseClusterChime mtrBaseClusterChime, IsMTRSubscribeParams params) => mtrBaseClusterChime -> params -> Ptr () -> Ptr () -> IO ()
subscribeAttributeAttributeListWithParams_subscriptionEstablished_reportHandler mtrBaseClusterChime params subscriptionEstablished reportHandler =
  sendMessage mtrBaseClusterChime subscribeAttributeAttributeListWithParams_subscriptionEstablished_reportHandlerSelector (toMTRSubscribeParams params) subscriptionEstablished reportHandler

-- | @+ readAttributeAttributeListWithClusterStateCache:endpoint:queue:completion:@
readAttributeAttributeListWithClusterStateCache_endpoint_queue_completion :: (IsMTRClusterStateCacheContainer clusterStateCacheContainer, IsNSNumber endpoint, IsNSObject queue) => clusterStateCacheContainer -> endpoint -> queue -> Ptr () -> IO ()
readAttributeAttributeListWithClusterStateCache_endpoint_queue_completion clusterStateCacheContainer endpoint queue completion =
  do
    cls' <- getRequiredClass "MTRBaseClusterChime"
    sendClassMessage cls' readAttributeAttributeListWithClusterStateCache_endpoint_queue_completionSelector (toMTRClusterStateCacheContainer clusterStateCacheContainer) (toNSNumber endpoint) (toNSObject queue) completion

-- | @- readAttributeFeatureMapWithCompletion:@
readAttributeFeatureMapWithCompletion :: IsMTRBaseClusterChime mtrBaseClusterChime => mtrBaseClusterChime -> Ptr () -> IO ()
readAttributeFeatureMapWithCompletion mtrBaseClusterChime completion =
  sendMessage mtrBaseClusterChime readAttributeFeatureMapWithCompletionSelector completion

-- | @- subscribeAttributeFeatureMapWithParams:subscriptionEstablished:reportHandler:@
subscribeAttributeFeatureMapWithParams_subscriptionEstablished_reportHandler :: (IsMTRBaseClusterChime mtrBaseClusterChime, IsMTRSubscribeParams params) => mtrBaseClusterChime -> params -> Ptr () -> Ptr () -> IO ()
subscribeAttributeFeatureMapWithParams_subscriptionEstablished_reportHandler mtrBaseClusterChime params subscriptionEstablished reportHandler =
  sendMessage mtrBaseClusterChime subscribeAttributeFeatureMapWithParams_subscriptionEstablished_reportHandlerSelector (toMTRSubscribeParams params) subscriptionEstablished reportHandler

-- | @+ readAttributeFeatureMapWithClusterStateCache:endpoint:queue:completion:@
readAttributeFeatureMapWithClusterStateCache_endpoint_queue_completion :: (IsMTRClusterStateCacheContainer clusterStateCacheContainer, IsNSNumber endpoint, IsNSObject queue) => clusterStateCacheContainer -> endpoint -> queue -> Ptr () -> IO ()
readAttributeFeatureMapWithClusterStateCache_endpoint_queue_completion clusterStateCacheContainer endpoint queue completion =
  do
    cls' <- getRequiredClass "MTRBaseClusterChime"
    sendClassMessage cls' readAttributeFeatureMapWithClusterStateCache_endpoint_queue_completionSelector (toMTRClusterStateCacheContainer clusterStateCacheContainer) (toNSNumber endpoint) (toNSObject queue) completion

-- | @- readAttributeClusterRevisionWithCompletion:@
readAttributeClusterRevisionWithCompletion :: IsMTRBaseClusterChime mtrBaseClusterChime => mtrBaseClusterChime -> Ptr () -> IO ()
readAttributeClusterRevisionWithCompletion mtrBaseClusterChime completion =
  sendMessage mtrBaseClusterChime readAttributeClusterRevisionWithCompletionSelector completion

-- | @- subscribeAttributeClusterRevisionWithParams:subscriptionEstablished:reportHandler:@
subscribeAttributeClusterRevisionWithParams_subscriptionEstablished_reportHandler :: (IsMTRBaseClusterChime mtrBaseClusterChime, IsMTRSubscribeParams params) => mtrBaseClusterChime -> params -> Ptr () -> Ptr () -> IO ()
subscribeAttributeClusterRevisionWithParams_subscriptionEstablished_reportHandler mtrBaseClusterChime params subscriptionEstablished reportHandler =
  sendMessage mtrBaseClusterChime subscribeAttributeClusterRevisionWithParams_subscriptionEstablished_reportHandlerSelector (toMTRSubscribeParams params) subscriptionEstablished reportHandler

-- | @+ readAttributeClusterRevisionWithClusterStateCache:endpoint:queue:completion:@
readAttributeClusterRevisionWithClusterStateCache_endpoint_queue_completion :: (IsMTRClusterStateCacheContainer clusterStateCacheContainer, IsNSNumber endpoint, IsNSObject queue) => clusterStateCacheContainer -> endpoint -> queue -> Ptr () -> IO ()
readAttributeClusterRevisionWithClusterStateCache_endpoint_queue_completion clusterStateCacheContainer endpoint queue completion =
  do
    cls' <- getRequiredClass "MTRBaseClusterChime"
    sendClassMessage cls' readAttributeClusterRevisionWithClusterStateCache_endpoint_queue_completionSelector (toMTRClusterStateCacheContainer clusterStateCacheContainer) (toNSNumber endpoint) (toNSObject queue) completion

-- | @- init@
init_ :: IsMTRBaseClusterChime mtrBaseClusterChime => mtrBaseClusterChime -> IO (Id MTRBaseClusterChime)
init_ mtrBaseClusterChime =
  sendOwnedMessage mtrBaseClusterChime initSelector

-- | @+ new@
new :: IO (Id MTRBaseClusterChime)
new  =
  do
    cls' <- getRequiredClass "MTRBaseClusterChime"
    sendOwnedClassMessage cls' newSelector

-- | For all instance methods (reads, writes, commands) that take a completion, the completion will be called on the provided queue.
--
-- ObjC selector: @- initWithDevice:endpointID:queue:@
initWithDevice_endpointID_queue :: (IsMTRBaseClusterChime mtrBaseClusterChime, IsMTRBaseDevice device, IsNSNumber endpointID, IsNSObject queue) => mtrBaseClusterChime -> device -> endpointID -> queue -> IO (Id MTRBaseClusterChime)
initWithDevice_endpointID_queue mtrBaseClusterChime device endpointID queue =
  sendOwnedMessage mtrBaseClusterChime initWithDevice_endpointID_queueSelector (toMTRBaseDevice device) (toNSNumber endpointID) (toNSObject queue)

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @playChimeSoundWithParams:completion:@
playChimeSoundWithParams_completionSelector :: Selector '[Id MTRChimeClusterPlayChimeSoundParams, Ptr ()] ()
playChimeSoundWithParams_completionSelector = mkSelector "playChimeSoundWithParams:completion:"

-- | @Selector@ for @playChimeSoundWithCompletion:@
playChimeSoundWithCompletionSelector :: Selector '[Ptr ()] ()
playChimeSoundWithCompletionSelector = mkSelector "playChimeSoundWithCompletion:"

-- | @Selector@ for @readAttributeInstalledChimeSoundsWithCompletion:@
readAttributeInstalledChimeSoundsWithCompletionSelector :: Selector '[Ptr ()] ()
readAttributeInstalledChimeSoundsWithCompletionSelector = mkSelector "readAttributeInstalledChimeSoundsWithCompletion:"

-- | @Selector@ for @subscribeAttributeInstalledChimeSoundsWithParams:subscriptionEstablished:reportHandler:@
subscribeAttributeInstalledChimeSoundsWithParams_subscriptionEstablished_reportHandlerSelector :: Selector '[Id MTRSubscribeParams, Ptr (), Ptr ()] ()
subscribeAttributeInstalledChimeSoundsWithParams_subscriptionEstablished_reportHandlerSelector = mkSelector "subscribeAttributeInstalledChimeSoundsWithParams:subscriptionEstablished:reportHandler:"

-- | @Selector@ for @readAttributeInstalledChimeSoundsWithClusterStateCache:endpoint:queue:completion:@
readAttributeInstalledChimeSoundsWithClusterStateCache_endpoint_queue_completionSelector :: Selector '[Id MTRClusterStateCacheContainer, Id NSNumber, Id NSObject, Ptr ()] ()
readAttributeInstalledChimeSoundsWithClusterStateCache_endpoint_queue_completionSelector = mkSelector "readAttributeInstalledChimeSoundsWithClusterStateCache:endpoint:queue:completion:"

-- | @Selector@ for @readAttributeSelectedChimeWithCompletion:@
readAttributeSelectedChimeWithCompletionSelector :: Selector '[Ptr ()] ()
readAttributeSelectedChimeWithCompletionSelector = mkSelector "readAttributeSelectedChimeWithCompletion:"

-- | @Selector@ for @writeAttributeSelectedChimeWithValue:completion:@
writeAttributeSelectedChimeWithValue_completionSelector :: Selector '[Id NSNumber, Ptr ()] ()
writeAttributeSelectedChimeWithValue_completionSelector = mkSelector "writeAttributeSelectedChimeWithValue:completion:"

-- | @Selector@ for @writeAttributeSelectedChimeWithValue:params:completion:@
writeAttributeSelectedChimeWithValue_params_completionSelector :: Selector '[Id NSNumber, Id MTRWriteParams, Ptr ()] ()
writeAttributeSelectedChimeWithValue_params_completionSelector = mkSelector "writeAttributeSelectedChimeWithValue:params:completion:"

-- | @Selector@ for @subscribeAttributeSelectedChimeWithParams:subscriptionEstablished:reportHandler:@
subscribeAttributeSelectedChimeWithParams_subscriptionEstablished_reportHandlerSelector :: Selector '[Id MTRSubscribeParams, Ptr (), Ptr ()] ()
subscribeAttributeSelectedChimeWithParams_subscriptionEstablished_reportHandlerSelector = mkSelector "subscribeAttributeSelectedChimeWithParams:subscriptionEstablished:reportHandler:"

-- | @Selector@ for @readAttributeSelectedChimeWithClusterStateCache:endpoint:queue:completion:@
readAttributeSelectedChimeWithClusterStateCache_endpoint_queue_completionSelector :: Selector '[Id MTRClusterStateCacheContainer, Id NSNumber, Id NSObject, Ptr ()] ()
readAttributeSelectedChimeWithClusterStateCache_endpoint_queue_completionSelector = mkSelector "readAttributeSelectedChimeWithClusterStateCache:endpoint:queue:completion:"

-- | @Selector@ for @readAttributeEnabledWithCompletion:@
readAttributeEnabledWithCompletionSelector :: Selector '[Ptr ()] ()
readAttributeEnabledWithCompletionSelector = mkSelector "readAttributeEnabledWithCompletion:"

-- | @Selector@ for @writeAttributeEnabledWithValue:completion:@
writeAttributeEnabledWithValue_completionSelector :: Selector '[Id NSNumber, Ptr ()] ()
writeAttributeEnabledWithValue_completionSelector = mkSelector "writeAttributeEnabledWithValue:completion:"

-- | @Selector@ for @writeAttributeEnabledWithValue:params:completion:@
writeAttributeEnabledWithValue_params_completionSelector :: Selector '[Id NSNumber, Id MTRWriteParams, Ptr ()] ()
writeAttributeEnabledWithValue_params_completionSelector = mkSelector "writeAttributeEnabledWithValue:params:completion:"

-- | @Selector@ for @subscribeAttributeEnabledWithParams:subscriptionEstablished:reportHandler:@
subscribeAttributeEnabledWithParams_subscriptionEstablished_reportHandlerSelector :: Selector '[Id MTRSubscribeParams, Ptr (), Ptr ()] ()
subscribeAttributeEnabledWithParams_subscriptionEstablished_reportHandlerSelector = mkSelector "subscribeAttributeEnabledWithParams:subscriptionEstablished:reportHandler:"

-- | @Selector@ for @readAttributeEnabledWithClusterStateCache:endpoint:queue:completion:@
readAttributeEnabledWithClusterStateCache_endpoint_queue_completionSelector :: Selector '[Id MTRClusterStateCacheContainer, Id NSNumber, Id NSObject, Ptr ()] ()
readAttributeEnabledWithClusterStateCache_endpoint_queue_completionSelector = mkSelector "readAttributeEnabledWithClusterStateCache:endpoint:queue:completion:"

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
initSelector :: Selector '[] (Id MTRBaseClusterChime)
initSelector = mkSelector "init"

-- | @Selector@ for @new@
newSelector :: Selector '[] (Id MTRBaseClusterChime)
newSelector = mkSelector "new"

-- | @Selector@ for @initWithDevice:endpointID:queue:@
initWithDevice_endpointID_queueSelector :: Selector '[Id MTRBaseDevice, Id NSNumber, Id NSObject] (Id MTRBaseClusterChime)
initWithDevice_endpointID_queueSelector = mkSelector "initWithDevice:endpointID:queue:"

