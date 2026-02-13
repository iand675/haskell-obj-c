{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Cluster Time Format Localization
--
-- Nodes should be expected to be deployed to any and all regions of the world. These global regions      may have differing preferences for how dates and times are conveyed. As such, Nodes that visually      or audibly convey time information need a mechanism by which they can be configured to use a      user’s preferred format.
--
-- Generated bindings for @MTRBaseClusterTimeFormatLocalization@.
module ObjC.Matter.MTRBaseClusterTimeFormatLocalization
  ( MTRBaseClusterTimeFormatLocalization
  , IsMTRBaseClusterTimeFormatLocalization(..)
  , readAttributeHourFormatWithCompletion
  , writeAttributeHourFormatWithValue_completion
  , writeAttributeHourFormatWithValue_params_completion
  , subscribeAttributeHourFormatWithParams_subscriptionEstablished_reportHandler
  , readAttributeHourFormatWithClusterStateCache_endpoint_queue_completion
  , readAttributeActiveCalendarTypeWithCompletion
  , writeAttributeActiveCalendarTypeWithValue_completion
  , writeAttributeActiveCalendarTypeWithValue_params_completion
  , subscribeAttributeActiveCalendarTypeWithParams_subscriptionEstablished_reportHandler
  , readAttributeActiveCalendarTypeWithClusterStateCache_endpoint_queue_completion
  , readAttributeSupportedCalendarTypesWithCompletion
  , subscribeAttributeSupportedCalendarTypesWithParams_subscriptionEstablished_reportHandler
  , readAttributeSupportedCalendarTypesWithClusterStateCache_endpoint_queue_completion
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
  , readAttributeHourFormatWithCompletionHandler
  , writeAttributeHourFormatWithValue_completionHandler
  , writeAttributeHourFormatWithValue_params_completionHandler
  , subscribeAttributeHourFormatWithMinInterval_maxInterval_params_subscriptionEstablished_reportHandler
  , readAttributeHourFormatWithAttributeCache_endpoint_queue_completionHandler
  , readAttributeActiveCalendarTypeWithCompletionHandler
  , writeAttributeActiveCalendarTypeWithValue_completionHandler
  , writeAttributeActiveCalendarTypeWithValue_params_completionHandler
  , subscribeAttributeActiveCalendarTypeWithMinInterval_maxInterval_params_subscriptionEstablished_reportHandler
  , readAttributeActiveCalendarTypeWithAttributeCache_endpoint_queue_completionHandler
  , readAttributeSupportedCalendarTypesWithCompletionHandler
  , subscribeAttributeSupportedCalendarTypesWithMinInterval_maxInterval_params_subscriptionEstablished_reportHandler
  , readAttributeSupportedCalendarTypesWithAttributeCache_endpoint_queue_completionHandler
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
  , newSelector
  , readAttributeAcceptedCommandListWithAttributeCache_endpoint_queue_completionHandlerSelector
  , readAttributeAcceptedCommandListWithClusterStateCache_endpoint_queue_completionSelector
  , readAttributeAcceptedCommandListWithCompletionHandlerSelector
  , readAttributeAcceptedCommandListWithCompletionSelector
  , readAttributeActiveCalendarTypeWithAttributeCache_endpoint_queue_completionHandlerSelector
  , readAttributeActiveCalendarTypeWithClusterStateCache_endpoint_queue_completionSelector
  , readAttributeActiveCalendarTypeWithCompletionHandlerSelector
  , readAttributeActiveCalendarTypeWithCompletionSelector
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
  , readAttributeHourFormatWithAttributeCache_endpoint_queue_completionHandlerSelector
  , readAttributeHourFormatWithClusterStateCache_endpoint_queue_completionSelector
  , readAttributeHourFormatWithCompletionHandlerSelector
  , readAttributeHourFormatWithCompletionSelector
  , readAttributeSupportedCalendarTypesWithAttributeCache_endpoint_queue_completionHandlerSelector
  , readAttributeSupportedCalendarTypesWithClusterStateCache_endpoint_queue_completionSelector
  , readAttributeSupportedCalendarTypesWithCompletionHandlerSelector
  , readAttributeSupportedCalendarTypesWithCompletionSelector
  , subscribeAttributeAcceptedCommandListWithMinInterval_maxInterval_params_subscriptionEstablished_reportHandlerSelector
  , subscribeAttributeAcceptedCommandListWithParams_subscriptionEstablished_reportHandlerSelector
  , subscribeAttributeActiveCalendarTypeWithMinInterval_maxInterval_params_subscriptionEstablished_reportHandlerSelector
  , subscribeAttributeActiveCalendarTypeWithParams_subscriptionEstablished_reportHandlerSelector
  , subscribeAttributeAttributeListWithMinInterval_maxInterval_params_subscriptionEstablished_reportHandlerSelector
  , subscribeAttributeAttributeListWithParams_subscriptionEstablished_reportHandlerSelector
  , subscribeAttributeClusterRevisionWithMinInterval_maxInterval_params_subscriptionEstablished_reportHandlerSelector
  , subscribeAttributeClusterRevisionWithParams_subscriptionEstablished_reportHandlerSelector
  , subscribeAttributeFeatureMapWithMinInterval_maxInterval_params_subscriptionEstablished_reportHandlerSelector
  , subscribeAttributeFeatureMapWithParams_subscriptionEstablished_reportHandlerSelector
  , subscribeAttributeGeneratedCommandListWithMinInterval_maxInterval_params_subscriptionEstablished_reportHandlerSelector
  , subscribeAttributeGeneratedCommandListWithParams_subscriptionEstablished_reportHandlerSelector
  , subscribeAttributeHourFormatWithMinInterval_maxInterval_params_subscriptionEstablished_reportHandlerSelector
  , subscribeAttributeHourFormatWithParams_subscriptionEstablished_reportHandlerSelector
  , subscribeAttributeSupportedCalendarTypesWithMinInterval_maxInterval_params_subscriptionEstablished_reportHandlerSelector
  , subscribeAttributeSupportedCalendarTypesWithParams_subscriptionEstablished_reportHandlerSelector
  , writeAttributeActiveCalendarTypeWithValue_completionHandlerSelector
  , writeAttributeActiveCalendarTypeWithValue_completionSelector
  , writeAttributeActiveCalendarTypeWithValue_params_completionHandlerSelector
  , writeAttributeActiveCalendarTypeWithValue_params_completionSelector
  , writeAttributeHourFormatWithValue_completionHandlerSelector
  , writeAttributeHourFormatWithValue_completionSelector
  , writeAttributeHourFormatWithValue_params_completionHandlerSelector
  , writeAttributeHourFormatWithValue_params_completionSelector


  ) where

import Foreign.Ptr (Ptr, FunPtr)
import Foreign.C.Types

import ObjC.Runtime.Types
import ObjC.Runtime.Message (sendMessage, sendOwnedMessage, sendClassMessage, sendOwnedClassMessage)
import ObjC.Runtime.Selector (mkSelector)
import ObjC.Runtime.Class (getRequiredClass)

import ObjC.Matter.Internal.Classes
import ObjC.Foundation.Internal.Classes

-- | @- readAttributeHourFormatWithCompletion:@
readAttributeHourFormatWithCompletion :: IsMTRBaseClusterTimeFormatLocalization mtrBaseClusterTimeFormatLocalization => mtrBaseClusterTimeFormatLocalization -> Ptr () -> IO ()
readAttributeHourFormatWithCompletion mtrBaseClusterTimeFormatLocalization completion =
  sendMessage mtrBaseClusterTimeFormatLocalization readAttributeHourFormatWithCompletionSelector completion

-- | @- writeAttributeHourFormatWithValue:completion:@
writeAttributeHourFormatWithValue_completion :: (IsMTRBaseClusterTimeFormatLocalization mtrBaseClusterTimeFormatLocalization, IsNSNumber value) => mtrBaseClusterTimeFormatLocalization -> value -> Ptr () -> IO ()
writeAttributeHourFormatWithValue_completion mtrBaseClusterTimeFormatLocalization value completion =
  sendMessage mtrBaseClusterTimeFormatLocalization writeAttributeHourFormatWithValue_completionSelector (toNSNumber value) completion

-- | @- writeAttributeHourFormatWithValue:params:completion:@
writeAttributeHourFormatWithValue_params_completion :: (IsMTRBaseClusterTimeFormatLocalization mtrBaseClusterTimeFormatLocalization, IsNSNumber value, IsMTRWriteParams params) => mtrBaseClusterTimeFormatLocalization -> value -> params -> Ptr () -> IO ()
writeAttributeHourFormatWithValue_params_completion mtrBaseClusterTimeFormatLocalization value params completion =
  sendMessage mtrBaseClusterTimeFormatLocalization writeAttributeHourFormatWithValue_params_completionSelector (toNSNumber value) (toMTRWriteParams params) completion

-- | @- subscribeAttributeHourFormatWithParams:subscriptionEstablished:reportHandler:@
subscribeAttributeHourFormatWithParams_subscriptionEstablished_reportHandler :: (IsMTRBaseClusterTimeFormatLocalization mtrBaseClusterTimeFormatLocalization, IsMTRSubscribeParams params) => mtrBaseClusterTimeFormatLocalization -> params -> Ptr () -> Ptr () -> IO ()
subscribeAttributeHourFormatWithParams_subscriptionEstablished_reportHandler mtrBaseClusterTimeFormatLocalization params subscriptionEstablished reportHandler =
  sendMessage mtrBaseClusterTimeFormatLocalization subscribeAttributeHourFormatWithParams_subscriptionEstablished_reportHandlerSelector (toMTRSubscribeParams params) subscriptionEstablished reportHandler

-- | @+ readAttributeHourFormatWithClusterStateCache:endpoint:queue:completion:@
readAttributeHourFormatWithClusterStateCache_endpoint_queue_completion :: (IsMTRClusterStateCacheContainer clusterStateCacheContainer, IsNSNumber endpoint, IsNSObject queue) => clusterStateCacheContainer -> endpoint -> queue -> Ptr () -> IO ()
readAttributeHourFormatWithClusterStateCache_endpoint_queue_completion clusterStateCacheContainer endpoint queue completion =
  do
    cls' <- getRequiredClass "MTRBaseClusterTimeFormatLocalization"
    sendClassMessage cls' readAttributeHourFormatWithClusterStateCache_endpoint_queue_completionSelector (toMTRClusterStateCacheContainer clusterStateCacheContainer) (toNSNumber endpoint) (toNSObject queue) completion

-- | @- readAttributeActiveCalendarTypeWithCompletion:@
readAttributeActiveCalendarTypeWithCompletion :: IsMTRBaseClusterTimeFormatLocalization mtrBaseClusterTimeFormatLocalization => mtrBaseClusterTimeFormatLocalization -> Ptr () -> IO ()
readAttributeActiveCalendarTypeWithCompletion mtrBaseClusterTimeFormatLocalization completion =
  sendMessage mtrBaseClusterTimeFormatLocalization readAttributeActiveCalendarTypeWithCompletionSelector completion

-- | @- writeAttributeActiveCalendarTypeWithValue:completion:@
writeAttributeActiveCalendarTypeWithValue_completion :: (IsMTRBaseClusterTimeFormatLocalization mtrBaseClusterTimeFormatLocalization, IsNSNumber value) => mtrBaseClusterTimeFormatLocalization -> value -> Ptr () -> IO ()
writeAttributeActiveCalendarTypeWithValue_completion mtrBaseClusterTimeFormatLocalization value completion =
  sendMessage mtrBaseClusterTimeFormatLocalization writeAttributeActiveCalendarTypeWithValue_completionSelector (toNSNumber value) completion

-- | @- writeAttributeActiveCalendarTypeWithValue:params:completion:@
writeAttributeActiveCalendarTypeWithValue_params_completion :: (IsMTRBaseClusterTimeFormatLocalization mtrBaseClusterTimeFormatLocalization, IsNSNumber value, IsMTRWriteParams params) => mtrBaseClusterTimeFormatLocalization -> value -> params -> Ptr () -> IO ()
writeAttributeActiveCalendarTypeWithValue_params_completion mtrBaseClusterTimeFormatLocalization value params completion =
  sendMessage mtrBaseClusterTimeFormatLocalization writeAttributeActiveCalendarTypeWithValue_params_completionSelector (toNSNumber value) (toMTRWriteParams params) completion

-- | @- subscribeAttributeActiveCalendarTypeWithParams:subscriptionEstablished:reportHandler:@
subscribeAttributeActiveCalendarTypeWithParams_subscriptionEstablished_reportHandler :: (IsMTRBaseClusterTimeFormatLocalization mtrBaseClusterTimeFormatLocalization, IsMTRSubscribeParams params) => mtrBaseClusterTimeFormatLocalization -> params -> Ptr () -> Ptr () -> IO ()
subscribeAttributeActiveCalendarTypeWithParams_subscriptionEstablished_reportHandler mtrBaseClusterTimeFormatLocalization params subscriptionEstablished reportHandler =
  sendMessage mtrBaseClusterTimeFormatLocalization subscribeAttributeActiveCalendarTypeWithParams_subscriptionEstablished_reportHandlerSelector (toMTRSubscribeParams params) subscriptionEstablished reportHandler

-- | @+ readAttributeActiveCalendarTypeWithClusterStateCache:endpoint:queue:completion:@
readAttributeActiveCalendarTypeWithClusterStateCache_endpoint_queue_completion :: (IsMTRClusterStateCacheContainer clusterStateCacheContainer, IsNSNumber endpoint, IsNSObject queue) => clusterStateCacheContainer -> endpoint -> queue -> Ptr () -> IO ()
readAttributeActiveCalendarTypeWithClusterStateCache_endpoint_queue_completion clusterStateCacheContainer endpoint queue completion =
  do
    cls' <- getRequiredClass "MTRBaseClusterTimeFormatLocalization"
    sendClassMessage cls' readAttributeActiveCalendarTypeWithClusterStateCache_endpoint_queue_completionSelector (toMTRClusterStateCacheContainer clusterStateCacheContainer) (toNSNumber endpoint) (toNSObject queue) completion

-- | @- readAttributeSupportedCalendarTypesWithCompletion:@
readAttributeSupportedCalendarTypesWithCompletion :: IsMTRBaseClusterTimeFormatLocalization mtrBaseClusterTimeFormatLocalization => mtrBaseClusterTimeFormatLocalization -> Ptr () -> IO ()
readAttributeSupportedCalendarTypesWithCompletion mtrBaseClusterTimeFormatLocalization completion =
  sendMessage mtrBaseClusterTimeFormatLocalization readAttributeSupportedCalendarTypesWithCompletionSelector completion

-- | @- subscribeAttributeSupportedCalendarTypesWithParams:subscriptionEstablished:reportHandler:@
subscribeAttributeSupportedCalendarTypesWithParams_subscriptionEstablished_reportHandler :: (IsMTRBaseClusterTimeFormatLocalization mtrBaseClusterTimeFormatLocalization, IsMTRSubscribeParams params) => mtrBaseClusterTimeFormatLocalization -> params -> Ptr () -> Ptr () -> IO ()
subscribeAttributeSupportedCalendarTypesWithParams_subscriptionEstablished_reportHandler mtrBaseClusterTimeFormatLocalization params subscriptionEstablished reportHandler =
  sendMessage mtrBaseClusterTimeFormatLocalization subscribeAttributeSupportedCalendarTypesWithParams_subscriptionEstablished_reportHandlerSelector (toMTRSubscribeParams params) subscriptionEstablished reportHandler

-- | @+ readAttributeSupportedCalendarTypesWithClusterStateCache:endpoint:queue:completion:@
readAttributeSupportedCalendarTypesWithClusterStateCache_endpoint_queue_completion :: (IsMTRClusterStateCacheContainer clusterStateCacheContainer, IsNSNumber endpoint, IsNSObject queue) => clusterStateCacheContainer -> endpoint -> queue -> Ptr () -> IO ()
readAttributeSupportedCalendarTypesWithClusterStateCache_endpoint_queue_completion clusterStateCacheContainer endpoint queue completion =
  do
    cls' <- getRequiredClass "MTRBaseClusterTimeFormatLocalization"
    sendClassMessage cls' readAttributeSupportedCalendarTypesWithClusterStateCache_endpoint_queue_completionSelector (toMTRClusterStateCacheContainer clusterStateCacheContainer) (toNSNumber endpoint) (toNSObject queue) completion

-- | @- readAttributeGeneratedCommandListWithCompletion:@
readAttributeGeneratedCommandListWithCompletion :: IsMTRBaseClusterTimeFormatLocalization mtrBaseClusterTimeFormatLocalization => mtrBaseClusterTimeFormatLocalization -> Ptr () -> IO ()
readAttributeGeneratedCommandListWithCompletion mtrBaseClusterTimeFormatLocalization completion =
  sendMessage mtrBaseClusterTimeFormatLocalization readAttributeGeneratedCommandListWithCompletionSelector completion

-- | @- subscribeAttributeGeneratedCommandListWithParams:subscriptionEstablished:reportHandler:@
subscribeAttributeGeneratedCommandListWithParams_subscriptionEstablished_reportHandler :: (IsMTRBaseClusterTimeFormatLocalization mtrBaseClusterTimeFormatLocalization, IsMTRSubscribeParams params) => mtrBaseClusterTimeFormatLocalization -> params -> Ptr () -> Ptr () -> IO ()
subscribeAttributeGeneratedCommandListWithParams_subscriptionEstablished_reportHandler mtrBaseClusterTimeFormatLocalization params subscriptionEstablished reportHandler =
  sendMessage mtrBaseClusterTimeFormatLocalization subscribeAttributeGeneratedCommandListWithParams_subscriptionEstablished_reportHandlerSelector (toMTRSubscribeParams params) subscriptionEstablished reportHandler

-- | @+ readAttributeGeneratedCommandListWithClusterStateCache:endpoint:queue:completion:@
readAttributeGeneratedCommandListWithClusterStateCache_endpoint_queue_completion :: (IsMTRClusterStateCacheContainer clusterStateCacheContainer, IsNSNumber endpoint, IsNSObject queue) => clusterStateCacheContainer -> endpoint -> queue -> Ptr () -> IO ()
readAttributeGeneratedCommandListWithClusterStateCache_endpoint_queue_completion clusterStateCacheContainer endpoint queue completion =
  do
    cls' <- getRequiredClass "MTRBaseClusterTimeFormatLocalization"
    sendClassMessage cls' readAttributeGeneratedCommandListWithClusterStateCache_endpoint_queue_completionSelector (toMTRClusterStateCacheContainer clusterStateCacheContainer) (toNSNumber endpoint) (toNSObject queue) completion

-- | @- readAttributeAcceptedCommandListWithCompletion:@
readAttributeAcceptedCommandListWithCompletion :: IsMTRBaseClusterTimeFormatLocalization mtrBaseClusterTimeFormatLocalization => mtrBaseClusterTimeFormatLocalization -> Ptr () -> IO ()
readAttributeAcceptedCommandListWithCompletion mtrBaseClusterTimeFormatLocalization completion =
  sendMessage mtrBaseClusterTimeFormatLocalization readAttributeAcceptedCommandListWithCompletionSelector completion

-- | @- subscribeAttributeAcceptedCommandListWithParams:subscriptionEstablished:reportHandler:@
subscribeAttributeAcceptedCommandListWithParams_subscriptionEstablished_reportHandler :: (IsMTRBaseClusterTimeFormatLocalization mtrBaseClusterTimeFormatLocalization, IsMTRSubscribeParams params) => mtrBaseClusterTimeFormatLocalization -> params -> Ptr () -> Ptr () -> IO ()
subscribeAttributeAcceptedCommandListWithParams_subscriptionEstablished_reportHandler mtrBaseClusterTimeFormatLocalization params subscriptionEstablished reportHandler =
  sendMessage mtrBaseClusterTimeFormatLocalization subscribeAttributeAcceptedCommandListWithParams_subscriptionEstablished_reportHandlerSelector (toMTRSubscribeParams params) subscriptionEstablished reportHandler

-- | @+ readAttributeAcceptedCommandListWithClusterStateCache:endpoint:queue:completion:@
readAttributeAcceptedCommandListWithClusterStateCache_endpoint_queue_completion :: (IsMTRClusterStateCacheContainer clusterStateCacheContainer, IsNSNumber endpoint, IsNSObject queue) => clusterStateCacheContainer -> endpoint -> queue -> Ptr () -> IO ()
readAttributeAcceptedCommandListWithClusterStateCache_endpoint_queue_completion clusterStateCacheContainer endpoint queue completion =
  do
    cls' <- getRequiredClass "MTRBaseClusterTimeFormatLocalization"
    sendClassMessage cls' readAttributeAcceptedCommandListWithClusterStateCache_endpoint_queue_completionSelector (toMTRClusterStateCacheContainer clusterStateCacheContainer) (toNSNumber endpoint) (toNSObject queue) completion

-- | @- readAttributeAttributeListWithCompletion:@
readAttributeAttributeListWithCompletion :: IsMTRBaseClusterTimeFormatLocalization mtrBaseClusterTimeFormatLocalization => mtrBaseClusterTimeFormatLocalization -> Ptr () -> IO ()
readAttributeAttributeListWithCompletion mtrBaseClusterTimeFormatLocalization completion =
  sendMessage mtrBaseClusterTimeFormatLocalization readAttributeAttributeListWithCompletionSelector completion

-- | @- subscribeAttributeAttributeListWithParams:subscriptionEstablished:reportHandler:@
subscribeAttributeAttributeListWithParams_subscriptionEstablished_reportHandler :: (IsMTRBaseClusterTimeFormatLocalization mtrBaseClusterTimeFormatLocalization, IsMTRSubscribeParams params) => mtrBaseClusterTimeFormatLocalization -> params -> Ptr () -> Ptr () -> IO ()
subscribeAttributeAttributeListWithParams_subscriptionEstablished_reportHandler mtrBaseClusterTimeFormatLocalization params subscriptionEstablished reportHandler =
  sendMessage mtrBaseClusterTimeFormatLocalization subscribeAttributeAttributeListWithParams_subscriptionEstablished_reportHandlerSelector (toMTRSubscribeParams params) subscriptionEstablished reportHandler

-- | @+ readAttributeAttributeListWithClusterStateCache:endpoint:queue:completion:@
readAttributeAttributeListWithClusterStateCache_endpoint_queue_completion :: (IsMTRClusterStateCacheContainer clusterStateCacheContainer, IsNSNumber endpoint, IsNSObject queue) => clusterStateCacheContainer -> endpoint -> queue -> Ptr () -> IO ()
readAttributeAttributeListWithClusterStateCache_endpoint_queue_completion clusterStateCacheContainer endpoint queue completion =
  do
    cls' <- getRequiredClass "MTRBaseClusterTimeFormatLocalization"
    sendClassMessage cls' readAttributeAttributeListWithClusterStateCache_endpoint_queue_completionSelector (toMTRClusterStateCacheContainer clusterStateCacheContainer) (toNSNumber endpoint) (toNSObject queue) completion

-- | @- readAttributeFeatureMapWithCompletion:@
readAttributeFeatureMapWithCompletion :: IsMTRBaseClusterTimeFormatLocalization mtrBaseClusterTimeFormatLocalization => mtrBaseClusterTimeFormatLocalization -> Ptr () -> IO ()
readAttributeFeatureMapWithCompletion mtrBaseClusterTimeFormatLocalization completion =
  sendMessage mtrBaseClusterTimeFormatLocalization readAttributeFeatureMapWithCompletionSelector completion

-- | @- subscribeAttributeFeatureMapWithParams:subscriptionEstablished:reportHandler:@
subscribeAttributeFeatureMapWithParams_subscriptionEstablished_reportHandler :: (IsMTRBaseClusterTimeFormatLocalization mtrBaseClusterTimeFormatLocalization, IsMTRSubscribeParams params) => mtrBaseClusterTimeFormatLocalization -> params -> Ptr () -> Ptr () -> IO ()
subscribeAttributeFeatureMapWithParams_subscriptionEstablished_reportHandler mtrBaseClusterTimeFormatLocalization params subscriptionEstablished reportHandler =
  sendMessage mtrBaseClusterTimeFormatLocalization subscribeAttributeFeatureMapWithParams_subscriptionEstablished_reportHandlerSelector (toMTRSubscribeParams params) subscriptionEstablished reportHandler

-- | @+ readAttributeFeatureMapWithClusterStateCache:endpoint:queue:completion:@
readAttributeFeatureMapWithClusterStateCache_endpoint_queue_completion :: (IsMTRClusterStateCacheContainer clusterStateCacheContainer, IsNSNumber endpoint, IsNSObject queue) => clusterStateCacheContainer -> endpoint -> queue -> Ptr () -> IO ()
readAttributeFeatureMapWithClusterStateCache_endpoint_queue_completion clusterStateCacheContainer endpoint queue completion =
  do
    cls' <- getRequiredClass "MTRBaseClusterTimeFormatLocalization"
    sendClassMessage cls' readAttributeFeatureMapWithClusterStateCache_endpoint_queue_completionSelector (toMTRClusterStateCacheContainer clusterStateCacheContainer) (toNSNumber endpoint) (toNSObject queue) completion

-- | @- readAttributeClusterRevisionWithCompletion:@
readAttributeClusterRevisionWithCompletion :: IsMTRBaseClusterTimeFormatLocalization mtrBaseClusterTimeFormatLocalization => mtrBaseClusterTimeFormatLocalization -> Ptr () -> IO ()
readAttributeClusterRevisionWithCompletion mtrBaseClusterTimeFormatLocalization completion =
  sendMessage mtrBaseClusterTimeFormatLocalization readAttributeClusterRevisionWithCompletionSelector completion

-- | @- subscribeAttributeClusterRevisionWithParams:subscriptionEstablished:reportHandler:@
subscribeAttributeClusterRevisionWithParams_subscriptionEstablished_reportHandler :: (IsMTRBaseClusterTimeFormatLocalization mtrBaseClusterTimeFormatLocalization, IsMTRSubscribeParams params) => mtrBaseClusterTimeFormatLocalization -> params -> Ptr () -> Ptr () -> IO ()
subscribeAttributeClusterRevisionWithParams_subscriptionEstablished_reportHandler mtrBaseClusterTimeFormatLocalization params subscriptionEstablished reportHandler =
  sendMessage mtrBaseClusterTimeFormatLocalization subscribeAttributeClusterRevisionWithParams_subscriptionEstablished_reportHandlerSelector (toMTRSubscribeParams params) subscriptionEstablished reportHandler

-- | @+ readAttributeClusterRevisionWithClusterStateCache:endpoint:queue:completion:@
readAttributeClusterRevisionWithClusterStateCache_endpoint_queue_completion :: (IsMTRClusterStateCacheContainer clusterStateCacheContainer, IsNSNumber endpoint, IsNSObject queue) => clusterStateCacheContainer -> endpoint -> queue -> Ptr () -> IO ()
readAttributeClusterRevisionWithClusterStateCache_endpoint_queue_completion clusterStateCacheContainer endpoint queue completion =
  do
    cls' <- getRequiredClass "MTRBaseClusterTimeFormatLocalization"
    sendClassMessage cls' readAttributeClusterRevisionWithClusterStateCache_endpoint_queue_completionSelector (toMTRClusterStateCacheContainer clusterStateCacheContainer) (toNSNumber endpoint) (toNSObject queue) completion

-- | @- init@
init_ :: IsMTRBaseClusterTimeFormatLocalization mtrBaseClusterTimeFormatLocalization => mtrBaseClusterTimeFormatLocalization -> IO (Id MTRBaseClusterTimeFormatLocalization)
init_ mtrBaseClusterTimeFormatLocalization =
  sendOwnedMessage mtrBaseClusterTimeFormatLocalization initSelector

-- | @+ new@
new :: IO (Id MTRBaseClusterTimeFormatLocalization)
new  =
  do
    cls' <- getRequiredClass "MTRBaseClusterTimeFormatLocalization"
    sendOwnedClassMessage cls' newSelector

-- | @- initWithDevice:endpoint:queue:@
initWithDevice_endpoint_queue :: (IsMTRBaseClusterTimeFormatLocalization mtrBaseClusterTimeFormatLocalization, IsMTRBaseDevice device, IsNSObject queue) => mtrBaseClusterTimeFormatLocalization -> device -> CUShort -> queue -> IO (Id MTRBaseClusterTimeFormatLocalization)
initWithDevice_endpoint_queue mtrBaseClusterTimeFormatLocalization device endpoint queue =
  sendOwnedMessage mtrBaseClusterTimeFormatLocalization initWithDevice_endpoint_queueSelector (toMTRBaseDevice device) endpoint (toNSObject queue)

-- | @- readAttributeHourFormatWithCompletionHandler:@
readAttributeHourFormatWithCompletionHandler :: IsMTRBaseClusterTimeFormatLocalization mtrBaseClusterTimeFormatLocalization => mtrBaseClusterTimeFormatLocalization -> Ptr () -> IO ()
readAttributeHourFormatWithCompletionHandler mtrBaseClusterTimeFormatLocalization completionHandler =
  sendMessage mtrBaseClusterTimeFormatLocalization readAttributeHourFormatWithCompletionHandlerSelector completionHandler

-- | @- writeAttributeHourFormatWithValue:completionHandler:@
writeAttributeHourFormatWithValue_completionHandler :: (IsMTRBaseClusterTimeFormatLocalization mtrBaseClusterTimeFormatLocalization, IsNSNumber value) => mtrBaseClusterTimeFormatLocalization -> value -> Ptr () -> IO ()
writeAttributeHourFormatWithValue_completionHandler mtrBaseClusterTimeFormatLocalization value completionHandler =
  sendMessage mtrBaseClusterTimeFormatLocalization writeAttributeHourFormatWithValue_completionHandlerSelector (toNSNumber value) completionHandler

-- | @- writeAttributeHourFormatWithValue:params:completionHandler:@
writeAttributeHourFormatWithValue_params_completionHandler :: (IsMTRBaseClusterTimeFormatLocalization mtrBaseClusterTimeFormatLocalization, IsNSNumber value, IsMTRWriteParams params) => mtrBaseClusterTimeFormatLocalization -> value -> params -> Ptr () -> IO ()
writeAttributeHourFormatWithValue_params_completionHandler mtrBaseClusterTimeFormatLocalization value params completionHandler =
  sendMessage mtrBaseClusterTimeFormatLocalization writeAttributeHourFormatWithValue_params_completionHandlerSelector (toNSNumber value) (toMTRWriteParams params) completionHandler

-- | @- subscribeAttributeHourFormatWithMinInterval:maxInterval:params:subscriptionEstablished:reportHandler:@
subscribeAttributeHourFormatWithMinInterval_maxInterval_params_subscriptionEstablished_reportHandler :: (IsMTRBaseClusterTimeFormatLocalization mtrBaseClusterTimeFormatLocalization, IsNSNumber minInterval, IsNSNumber maxInterval, IsMTRSubscribeParams params) => mtrBaseClusterTimeFormatLocalization -> minInterval -> maxInterval -> params -> Ptr () -> Ptr () -> IO ()
subscribeAttributeHourFormatWithMinInterval_maxInterval_params_subscriptionEstablished_reportHandler mtrBaseClusterTimeFormatLocalization minInterval maxInterval params subscriptionEstablishedHandler reportHandler =
  sendMessage mtrBaseClusterTimeFormatLocalization subscribeAttributeHourFormatWithMinInterval_maxInterval_params_subscriptionEstablished_reportHandlerSelector (toNSNumber minInterval) (toNSNumber maxInterval) (toMTRSubscribeParams params) subscriptionEstablishedHandler reportHandler

-- | @+ readAttributeHourFormatWithAttributeCache:endpoint:queue:completionHandler:@
readAttributeHourFormatWithAttributeCache_endpoint_queue_completionHandler :: (IsMTRAttributeCacheContainer attributeCacheContainer, IsNSNumber endpoint, IsNSObject queue) => attributeCacheContainer -> endpoint -> queue -> Ptr () -> IO ()
readAttributeHourFormatWithAttributeCache_endpoint_queue_completionHandler attributeCacheContainer endpoint queue completionHandler =
  do
    cls' <- getRequiredClass "MTRBaseClusterTimeFormatLocalization"
    sendClassMessage cls' readAttributeHourFormatWithAttributeCache_endpoint_queue_completionHandlerSelector (toMTRAttributeCacheContainer attributeCacheContainer) (toNSNumber endpoint) (toNSObject queue) completionHandler

-- | @- readAttributeActiveCalendarTypeWithCompletionHandler:@
readAttributeActiveCalendarTypeWithCompletionHandler :: IsMTRBaseClusterTimeFormatLocalization mtrBaseClusterTimeFormatLocalization => mtrBaseClusterTimeFormatLocalization -> Ptr () -> IO ()
readAttributeActiveCalendarTypeWithCompletionHandler mtrBaseClusterTimeFormatLocalization completionHandler =
  sendMessage mtrBaseClusterTimeFormatLocalization readAttributeActiveCalendarTypeWithCompletionHandlerSelector completionHandler

-- | @- writeAttributeActiveCalendarTypeWithValue:completionHandler:@
writeAttributeActiveCalendarTypeWithValue_completionHandler :: (IsMTRBaseClusterTimeFormatLocalization mtrBaseClusterTimeFormatLocalization, IsNSNumber value) => mtrBaseClusterTimeFormatLocalization -> value -> Ptr () -> IO ()
writeAttributeActiveCalendarTypeWithValue_completionHandler mtrBaseClusterTimeFormatLocalization value completionHandler =
  sendMessage mtrBaseClusterTimeFormatLocalization writeAttributeActiveCalendarTypeWithValue_completionHandlerSelector (toNSNumber value) completionHandler

-- | @- writeAttributeActiveCalendarTypeWithValue:params:completionHandler:@
writeAttributeActiveCalendarTypeWithValue_params_completionHandler :: (IsMTRBaseClusterTimeFormatLocalization mtrBaseClusterTimeFormatLocalization, IsNSNumber value, IsMTRWriteParams params) => mtrBaseClusterTimeFormatLocalization -> value -> params -> Ptr () -> IO ()
writeAttributeActiveCalendarTypeWithValue_params_completionHandler mtrBaseClusterTimeFormatLocalization value params completionHandler =
  sendMessage mtrBaseClusterTimeFormatLocalization writeAttributeActiveCalendarTypeWithValue_params_completionHandlerSelector (toNSNumber value) (toMTRWriteParams params) completionHandler

-- | @- subscribeAttributeActiveCalendarTypeWithMinInterval:maxInterval:params:subscriptionEstablished:reportHandler:@
subscribeAttributeActiveCalendarTypeWithMinInterval_maxInterval_params_subscriptionEstablished_reportHandler :: (IsMTRBaseClusterTimeFormatLocalization mtrBaseClusterTimeFormatLocalization, IsNSNumber minInterval, IsNSNumber maxInterval, IsMTRSubscribeParams params) => mtrBaseClusterTimeFormatLocalization -> minInterval -> maxInterval -> params -> Ptr () -> Ptr () -> IO ()
subscribeAttributeActiveCalendarTypeWithMinInterval_maxInterval_params_subscriptionEstablished_reportHandler mtrBaseClusterTimeFormatLocalization minInterval maxInterval params subscriptionEstablishedHandler reportHandler =
  sendMessage mtrBaseClusterTimeFormatLocalization subscribeAttributeActiveCalendarTypeWithMinInterval_maxInterval_params_subscriptionEstablished_reportHandlerSelector (toNSNumber minInterval) (toNSNumber maxInterval) (toMTRSubscribeParams params) subscriptionEstablishedHandler reportHandler

-- | @+ readAttributeActiveCalendarTypeWithAttributeCache:endpoint:queue:completionHandler:@
readAttributeActiveCalendarTypeWithAttributeCache_endpoint_queue_completionHandler :: (IsMTRAttributeCacheContainer attributeCacheContainer, IsNSNumber endpoint, IsNSObject queue) => attributeCacheContainer -> endpoint -> queue -> Ptr () -> IO ()
readAttributeActiveCalendarTypeWithAttributeCache_endpoint_queue_completionHandler attributeCacheContainer endpoint queue completionHandler =
  do
    cls' <- getRequiredClass "MTRBaseClusterTimeFormatLocalization"
    sendClassMessage cls' readAttributeActiveCalendarTypeWithAttributeCache_endpoint_queue_completionHandlerSelector (toMTRAttributeCacheContainer attributeCacheContainer) (toNSNumber endpoint) (toNSObject queue) completionHandler

-- | @- readAttributeSupportedCalendarTypesWithCompletionHandler:@
readAttributeSupportedCalendarTypesWithCompletionHandler :: IsMTRBaseClusterTimeFormatLocalization mtrBaseClusterTimeFormatLocalization => mtrBaseClusterTimeFormatLocalization -> Ptr () -> IO ()
readAttributeSupportedCalendarTypesWithCompletionHandler mtrBaseClusterTimeFormatLocalization completionHandler =
  sendMessage mtrBaseClusterTimeFormatLocalization readAttributeSupportedCalendarTypesWithCompletionHandlerSelector completionHandler

-- | @- subscribeAttributeSupportedCalendarTypesWithMinInterval:maxInterval:params:subscriptionEstablished:reportHandler:@
subscribeAttributeSupportedCalendarTypesWithMinInterval_maxInterval_params_subscriptionEstablished_reportHandler :: (IsMTRBaseClusterTimeFormatLocalization mtrBaseClusterTimeFormatLocalization, IsNSNumber minInterval, IsNSNumber maxInterval, IsMTRSubscribeParams params) => mtrBaseClusterTimeFormatLocalization -> minInterval -> maxInterval -> params -> Ptr () -> Ptr () -> IO ()
subscribeAttributeSupportedCalendarTypesWithMinInterval_maxInterval_params_subscriptionEstablished_reportHandler mtrBaseClusterTimeFormatLocalization minInterval maxInterval params subscriptionEstablishedHandler reportHandler =
  sendMessage mtrBaseClusterTimeFormatLocalization subscribeAttributeSupportedCalendarTypesWithMinInterval_maxInterval_params_subscriptionEstablished_reportHandlerSelector (toNSNumber minInterval) (toNSNumber maxInterval) (toMTRSubscribeParams params) subscriptionEstablishedHandler reportHandler

-- | @+ readAttributeSupportedCalendarTypesWithAttributeCache:endpoint:queue:completionHandler:@
readAttributeSupportedCalendarTypesWithAttributeCache_endpoint_queue_completionHandler :: (IsMTRAttributeCacheContainer attributeCacheContainer, IsNSNumber endpoint, IsNSObject queue) => attributeCacheContainer -> endpoint -> queue -> Ptr () -> IO ()
readAttributeSupportedCalendarTypesWithAttributeCache_endpoint_queue_completionHandler attributeCacheContainer endpoint queue completionHandler =
  do
    cls' <- getRequiredClass "MTRBaseClusterTimeFormatLocalization"
    sendClassMessage cls' readAttributeSupportedCalendarTypesWithAttributeCache_endpoint_queue_completionHandlerSelector (toMTRAttributeCacheContainer attributeCacheContainer) (toNSNumber endpoint) (toNSObject queue) completionHandler

-- | @- readAttributeGeneratedCommandListWithCompletionHandler:@
readAttributeGeneratedCommandListWithCompletionHandler :: IsMTRBaseClusterTimeFormatLocalization mtrBaseClusterTimeFormatLocalization => mtrBaseClusterTimeFormatLocalization -> Ptr () -> IO ()
readAttributeGeneratedCommandListWithCompletionHandler mtrBaseClusterTimeFormatLocalization completionHandler =
  sendMessage mtrBaseClusterTimeFormatLocalization readAttributeGeneratedCommandListWithCompletionHandlerSelector completionHandler

-- | @- subscribeAttributeGeneratedCommandListWithMinInterval:maxInterval:params:subscriptionEstablished:reportHandler:@
subscribeAttributeGeneratedCommandListWithMinInterval_maxInterval_params_subscriptionEstablished_reportHandler :: (IsMTRBaseClusterTimeFormatLocalization mtrBaseClusterTimeFormatLocalization, IsNSNumber minInterval, IsNSNumber maxInterval, IsMTRSubscribeParams params) => mtrBaseClusterTimeFormatLocalization -> minInterval -> maxInterval -> params -> Ptr () -> Ptr () -> IO ()
subscribeAttributeGeneratedCommandListWithMinInterval_maxInterval_params_subscriptionEstablished_reportHandler mtrBaseClusterTimeFormatLocalization minInterval maxInterval params subscriptionEstablishedHandler reportHandler =
  sendMessage mtrBaseClusterTimeFormatLocalization subscribeAttributeGeneratedCommandListWithMinInterval_maxInterval_params_subscriptionEstablished_reportHandlerSelector (toNSNumber minInterval) (toNSNumber maxInterval) (toMTRSubscribeParams params) subscriptionEstablishedHandler reportHandler

-- | @+ readAttributeGeneratedCommandListWithAttributeCache:endpoint:queue:completionHandler:@
readAttributeGeneratedCommandListWithAttributeCache_endpoint_queue_completionHandler :: (IsMTRAttributeCacheContainer attributeCacheContainer, IsNSNumber endpoint, IsNSObject queue) => attributeCacheContainer -> endpoint -> queue -> Ptr () -> IO ()
readAttributeGeneratedCommandListWithAttributeCache_endpoint_queue_completionHandler attributeCacheContainer endpoint queue completionHandler =
  do
    cls' <- getRequiredClass "MTRBaseClusterTimeFormatLocalization"
    sendClassMessage cls' readAttributeGeneratedCommandListWithAttributeCache_endpoint_queue_completionHandlerSelector (toMTRAttributeCacheContainer attributeCacheContainer) (toNSNumber endpoint) (toNSObject queue) completionHandler

-- | @- readAttributeAcceptedCommandListWithCompletionHandler:@
readAttributeAcceptedCommandListWithCompletionHandler :: IsMTRBaseClusterTimeFormatLocalization mtrBaseClusterTimeFormatLocalization => mtrBaseClusterTimeFormatLocalization -> Ptr () -> IO ()
readAttributeAcceptedCommandListWithCompletionHandler mtrBaseClusterTimeFormatLocalization completionHandler =
  sendMessage mtrBaseClusterTimeFormatLocalization readAttributeAcceptedCommandListWithCompletionHandlerSelector completionHandler

-- | @- subscribeAttributeAcceptedCommandListWithMinInterval:maxInterval:params:subscriptionEstablished:reportHandler:@
subscribeAttributeAcceptedCommandListWithMinInterval_maxInterval_params_subscriptionEstablished_reportHandler :: (IsMTRBaseClusterTimeFormatLocalization mtrBaseClusterTimeFormatLocalization, IsNSNumber minInterval, IsNSNumber maxInterval, IsMTRSubscribeParams params) => mtrBaseClusterTimeFormatLocalization -> minInterval -> maxInterval -> params -> Ptr () -> Ptr () -> IO ()
subscribeAttributeAcceptedCommandListWithMinInterval_maxInterval_params_subscriptionEstablished_reportHandler mtrBaseClusterTimeFormatLocalization minInterval maxInterval params subscriptionEstablishedHandler reportHandler =
  sendMessage mtrBaseClusterTimeFormatLocalization subscribeAttributeAcceptedCommandListWithMinInterval_maxInterval_params_subscriptionEstablished_reportHandlerSelector (toNSNumber minInterval) (toNSNumber maxInterval) (toMTRSubscribeParams params) subscriptionEstablishedHandler reportHandler

-- | @+ readAttributeAcceptedCommandListWithAttributeCache:endpoint:queue:completionHandler:@
readAttributeAcceptedCommandListWithAttributeCache_endpoint_queue_completionHandler :: (IsMTRAttributeCacheContainer attributeCacheContainer, IsNSNumber endpoint, IsNSObject queue) => attributeCacheContainer -> endpoint -> queue -> Ptr () -> IO ()
readAttributeAcceptedCommandListWithAttributeCache_endpoint_queue_completionHandler attributeCacheContainer endpoint queue completionHandler =
  do
    cls' <- getRequiredClass "MTRBaseClusterTimeFormatLocalization"
    sendClassMessage cls' readAttributeAcceptedCommandListWithAttributeCache_endpoint_queue_completionHandlerSelector (toMTRAttributeCacheContainer attributeCacheContainer) (toNSNumber endpoint) (toNSObject queue) completionHandler

-- | @- readAttributeAttributeListWithCompletionHandler:@
readAttributeAttributeListWithCompletionHandler :: IsMTRBaseClusterTimeFormatLocalization mtrBaseClusterTimeFormatLocalization => mtrBaseClusterTimeFormatLocalization -> Ptr () -> IO ()
readAttributeAttributeListWithCompletionHandler mtrBaseClusterTimeFormatLocalization completionHandler =
  sendMessage mtrBaseClusterTimeFormatLocalization readAttributeAttributeListWithCompletionHandlerSelector completionHandler

-- | @- subscribeAttributeAttributeListWithMinInterval:maxInterval:params:subscriptionEstablished:reportHandler:@
subscribeAttributeAttributeListWithMinInterval_maxInterval_params_subscriptionEstablished_reportHandler :: (IsMTRBaseClusterTimeFormatLocalization mtrBaseClusterTimeFormatLocalization, IsNSNumber minInterval, IsNSNumber maxInterval, IsMTRSubscribeParams params) => mtrBaseClusterTimeFormatLocalization -> minInterval -> maxInterval -> params -> Ptr () -> Ptr () -> IO ()
subscribeAttributeAttributeListWithMinInterval_maxInterval_params_subscriptionEstablished_reportHandler mtrBaseClusterTimeFormatLocalization minInterval maxInterval params subscriptionEstablishedHandler reportHandler =
  sendMessage mtrBaseClusterTimeFormatLocalization subscribeAttributeAttributeListWithMinInterval_maxInterval_params_subscriptionEstablished_reportHandlerSelector (toNSNumber minInterval) (toNSNumber maxInterval) (toMTRSubscribeParams params) subscriptionEstablishedHandler reportHandler

-- | @+ readAttributeAttributeListWithAttributeCache:endpoint:queue:completionHandler:@
readAttributeAttributeListWithAttributeCache_endpoint_queue_completionHandler :: (IsMTRAttributeCacheContainer attributeCacheContainer, IsNSNumber endpoint, IsNSObject queue) => attributeCacheContainer -> endpoint -> queue -> Ptr () -> IO ()
readAttributeAttributeListWithAttributeCache_endpoint_queue_completionHandler attributeCacheContainer endpoint queue completionHandler =
  do
    cls' <- getRequiredClass "MTRBaseClusterTimeFormatLocalization"
    sendClassMessage cls' readAttributeAttributeListWithAttributeCache_endpoint_queue_completionHandlerSelector (toMTRAttributeCacheContainer attributeCacheContainer) (toNSNumber endpoint) (toNSObject queue) completionHandler

-- | @- readAttributeFeatureMapWithCompletionHandler:@
readAttributeFeatureMapWithCompletionHandler :: IsMTRBaseClusterTimeFormatLocalization mtrBaseClusterTimeFormatLocalization => mtrBaseClusterTimeFormatLocalization -> Ptr () -> IO ()
readAttributeFeatureMapWithCompletionHandler mtrBaseClusterTimeFormatLocalization completionHandler =
  sendMessage mtrBaseClusterTimeFormatLocalization readAttributeFeatureMapWithCompletionHandlerSelector completionHandler

-- | @- subscribeAttributeFeatureMapWithMinInterval:maxInterval:params:subscriptionEstablished:reportHandler:@
subscribeAttributeFeatureMapWithMinInterval_maxInterval_params_subscriptionEstablished_reportHandler :: (IsMTRBaseClusterTimeFormatLocalization mtrBaseClusterTimeFormatLocalization, IsNSNumber minInterval, IsNSNumber maxInterval, IsMTRSubscribeParams params) => mtrBaseClusterTimeFormatLocalization -> minInterval -> maxInterval -> params -> Ptr () -> Ptr () -> IO ()
subscribeAttributeFeatureMapWithMinInterval_maxInterval_params_subscriptionEstablished_reportHandler mtrBaseClusterTimeFormatLocalization minInterval maxInterval params subscriptionEstablishedHandler reportHandler =
  sendMessage mtrBaseClusterTimeFormatLocalization subscribeAttributeFeatureMapWithMinInterval_maxInterval_params_subscriptionEstablished_reportHandlerSelector (toNSNumber minInterval) (toNSNumber maxInterval) (toMTRSubscribeParams params) subscriptionEstablishedHandler reportHandler

-- | @+ readAttributeFeatureMapWithAttributeCache:endpoint:queue:completionHandler:@
readAttributeFeatureMapWithAttributeCache_endpoint_queue_completionHandler :: (IsMTRAttributeCacheContainer attributeCacheContainer, IsNSNumber endpoint, IsNSObject queue) => attributeCacheContainer -> endpoint -> queue -> Ptr () -> IO ()
readAttributeFeatureMapWithAttributeCache_endpoint_queue_completionHandler attributeCacheContainer endpoint queue completionHandler =
  do
    cls' <- getRequiredClass "MTRBaseClusterTimeFormatLocalization"
    sendClassMessage cls' readAttributeFeatureMapWithAttributeCache_endpoint_queue_completionHandlerSelector (toMTRAttributeCacheContainer attributeCacheContainer) (toNSNumber endpoint) (toNSObject queue) completionHandler

-- | @- readAttributeClusterRevisionWithCompletionHandler:@
readAttributeClusterRevisionWithCompletionHandler :: IsMTRBaseClusterTimeFormatLocalization mtrBaseClusterTimeFormatLocalization => mtrBaseClusterTimeFormatLocalization -> Ptr () -> IO ()
readAttributeClusterRevisionWithCompletionHandler mtrBaseClusterTimeFormatLocalization completionHandler =
  sendMessage mtrBaseClusterTimeFormatLocalization readAttributeClusterRevisionWithCompletionHandlerSelector completionHandler

-- | @- subscribeAttributeClusterRevisionWithMinInterval:maxInterval:params:subscriptionEstablished:reportHandler:@
subscribeAttributeClusterRevisionWithMinInterval_maxInterval_params_subscriptionEstablished_reportHandler :: (IsMTRBaseClusterTimeFormatLocalization mtrBaseClusterTimeFormatLocalization, IsNSNumber minInterval, IsNSNumber maxInterval, IsMTRSubscribeParams params) => mtrBaseClusterTimeFormatLocalization -> minInterval -> maxInterval -> params -> Ptr () -> Ptr () -> IO ()
subscribeAttributeClusterRevisionWithMinInterval_maxInterval_params_subscriptionEstablished_reportHandler mtrBaseClusterTimeFormatLocalization minInterval maxInterval params subscriptionEstablishedHandler reportHandler =
  sendMessage mtrBaseClusterTimeFormatLocalization subscribeAttributeClusterRevisionWithMinInterval_maxInterval_params_subscriptionEstablished_reportHandlerSelector (toNSNumber minInterval) (toNSNumber maxInterval) (toMTRSubscribeParams params) subscriptionEstablishedHandler reportHandler

-- | @+ readAttributeClusterRevisionWithAttributeCache:endpoint:queue:completionHandler:@
readAttributeClusterRevisionWithAttributeCache_endpoint_queue_completionHandler :: (IsMTRAttributeCacheContainer attributeCacheContainer, IsNSNumber endpoint, IsNSObject queue) => attributeCacheContainer -> endpoint -> queue -> Ptr () -> IO ()
readAttributeClusterRevisionWithAttributeCache_endpoint_queue_completionHandler attributeCacheContainer endpoint queue completionHandler =
  do
    cls' <- getRequiredClass "MTRBaseClusterTimeFormatLocalization"
    sendClassMessage cls' readAttributeClusterRevisionWithAttributeCache_endpoint_queue_completionHandlerSelector (toMTRAttributeCacheContainer attributeCacheContainer) (toNSNumber endpoint) (toNSObject queue) completionHandler

-- | For all instance methods (reads, writes, commands) that take a completion, the completion will be called on the provided queue.
--
-- ObjC selector: @- initWithDevice:endpointID:queue:@
initWithDevice_endpointID_queue :: (IsMTRBaseClusterTimeFormatLocalization mtrBaseClusterTimeFormatLocalization, IsMTRBaseDevice device, IsNSNumber endpointID, IsNSObject queue) => mtrBaseClusterTimeFormatLocalization -> device -> endpointID -> queue -> IO (Id MTRBaseClusterTimeFormatLocalization)
initWithDevice_endpointID_queue mtrBaseClusterTimeFormatLocalization device endpointID queue =
  sendOwnedMessage mtrBaseClusterTimeFormatLocalization initWithDevice_endpointID_queueSelector (toMTRBaseDevice device) (toNSNumber endpointID) (toNSObject queue)

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @readAttributeHourFormatWithCompletion:@
readAttributeHourFormatWithCompletionSelector :: Selector '[Ptr ()] ()
readAttributeHourFormatWithCompletionSelector = mkSelector "readAttributeHourFormatWithCompletion:"

-- | @Selector@ for @writeAttributeHourFormatWithValue:completion:@
writeAttributeHourFormatWithValue_completionSelector :: Selector '[Id NSNumber, Ptr ()] ()
writeAttributeHourFormatWithValue_completionSelector = mkSelector "writeAttributeHourFormatWithValue:completion:"

-- | @Selector@ for @writeAttributeHourFormatWithValue:params:completion:@
writeAttributeHourFormatWithValue_params_completionSelector :: Selector '[Id NSNumber, Id MTRWriteParams, Ptr ()] ()
writeAttributeHourFormatWithValue_params_completionSelector = mkSelector "writeAttributeHourFormatWithValue:params:completion:"

-- | @Selector@ for @subscribeAttributeHourFormatWithParams:subscriptionEstablished:reportHandler:@
subscribeAttributeHourFormatWithParams_subscriptionEstablished_reportHandlerSelector :: Selector '[Id MTRSubscribeParams, Ptr (), Ptr ()] ()
subscribeAttributeHourFormatWithParams_subscriptionEstablished_reportHandlerSelector = mkSelector "subscribeAttributeHourFormatWithParams:subscriptionEstablished:reportHandler:"

-- | @Selector@ for @readAttributeHourFormatWithClusterStateCache:endpoint:queue:completion:@
readAttributeHourFormatWithClusterStateCache_endpoint_queue_completionSelector :: Selector '[Id MTRClusterStateCacheContainer, Id NSNumber, Id NSObject, Ptr ()] ()
readAttributeHourFormatWithClusterStateCache_endpoint_queue_completionSelector = mkSelector "readAttributeHourFormatWithClusterStateCache:endpoint:queue:completion:"

-- | @Selector@ for @readAttributeActiveCalendarTypeWithCompletion:@
readAttributeActiveCalendarTypeWithCompletionSelector :: Selector '[Ptr ()] ()
readAttributeActiveCalendarTypeWithCompletionSelector = mkSelector "readAttributeActiveCalendarTypeWithCompletion:"

-- | @Selector@ for @writeAttributeActiveCalendarTypeWithValue:completion:@
writeAttributeActiveCalendarTypeWithValue_completionSelector :: Selector '[Id NSNumber, Ptr ()] ()
writeAttributeActiveCalendarTypeWithValue_completionSelector = mkSelector "writeAttributeActiveCalendarTypeWithValue:completion:"

-- | @Selector@ for @writeAttributeActiveCalendarTypeWithValue:params:completion:@
writeAttributeActiveCalendarTypeWithValue_params_completionSelector :: Selector '[Id NSNumber, Id MTRWriteParams, Ptr ()] ()
writeAttributeActiveCalendarTypeWithValue_params_completionSelector = mkSelector "writeAttributeActiveCalendarTypeWithValue:params:completion:"

-- | @Selector@ for @subscribeAttributeActiveCalendarTypeWithParams:subscriptionEstablished:reportHandler:@
subscribeAttributeActiveCalendarTypeWithParams_subscriptionEstablished_reportHandlerSelector :: Selector '[Id MTRSubscribeParams, Ptr (), Ptr ()] ()
subscribeAttributeActiveCalendarTypeWithParams_subscriptionEstablished_reportHandlerSelector = mkSelector "subscribeAttributeActiveCalendarTypeWithParams:subscriptionEstablished:reportHandler:"

-- | @Selector@ for @readAttributeActiveCalendarTypeWithClusterStateCache:endpoint:queue:completion:@
readAttributeActiveCalendarTypeWithClusterStateCache_endpoint_queue_completionSelector :: Selector '[Id MTRClusterStateCacheContainer, Id NSNumber, Id NSObject, Ptr ()] ()
readAttributeActiveCalendarTypeWithClusterStateCache_endpoint_queue_completionSelector = mkSelector "readAttributeActiveCalendarTypeWithClusterStateCache:endpoint:queue:completion:"

-- | @Selector@ for @readAttributeSupportedCalendarTypesWithCompletion:@
readAttributeSupportedCalendarTypesWithCompletionSelector :: Selector '[Ptr ()] ()
readAttributeSupportedCalendarTypesWithCompletionSelector = mkSelector "readAttributeSupportedCalendarTypesWithCompletion:"

-- | @Selector@ for @subscribeAttributeSupportedCalendarTypesWithParams:subscriptionEstablished:reportHandler:@
subscribeAttributeSupportedCalendarTypesWithParams_subscriptionEstablished_reportHandlerSelector :: Selector '[Id MTRSubscribeParams, Ptr (), Ptr ()] ()
subscribeAttributeSupportedCalendarTypesWithParams_subscriptionEstablished_reportHandlerSelector = mkSelector "subscribeAttributeSupportedCalendarTypesWithParams:subscriptionEstablished:reportHandler:"

-- | @Selector@ for @readAttributeSupportedCalendarTypesWithClusterStateCache:endpoint:queue:completion:@
readAttributeSupportedCalendarTypesWithClusterStateCache_endpoint_queue_completionSelector :: Selector '[Id MTRClusterStateCacheContainer, Id NSNumber, Id NSObject, Ptr ()] ()
readAttributeSupportedCalendarTypesWithClusterStateCache_endpoint_queue_completionSelector = mkSelector "readAttributeSupportedCalendarTypesWithClusterStateCache:endpoint:queue:completion:"

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
initSelector :: Selector '[] (Id MTRBaseClusterTimeFormatLocalization)
initSelector = mkSelector "init"

-- | @Selector@ for @new@
newSelector :: Selector '[] (Id MTRBaseClusterTimeFormatLocalization)
newSelector = mkSelector "new"

-- | @Selector@ for @initWithDevice:endpoint:queue:@
initWithDevice_endpoint_queueSelector :: Selector '[Id MTRBaseDevice, CUShort, Id NSObject] (Id MTRBaseClusterTimeFormatLocalization)
initWithDevice_endpoint_queueSelector = mkSelector "initWithDevice:endpoint:queue:"

-- | @Selector@ for @readAttributeHourFormatWithCompletionHandler:@
readAttributeHourFormatWithCompletionHandlerSelector :: Selector '[Ptr ()] ()
readAttributeHourFormatWithCompletionHandlerSelector = mkSelector "readAttributeHourFormatWithCompletionHandler:"

-- | @Selector@ for @writeAttributeHourFormatWithValue:completionHandler:@
writeAttributeHourFormatWithValue_completionHandlerSelector :: Selector '[Id NSNumber, Ptr ()] ()
writeAttributeHourFormatWithValue_completionHandlerSelector = mkSelector "writeAttributeHourFormatWithValue:completionHandler:"

-- | @Selector@ for @writeAttributeHourFormatWithValue:params:completionHandler:@
writeAttributeHourFormatWithValue_params_completionHandlerSelector :: Selector '[Id NSNumber, Id MTRWriteParams, Ptr ()] ()
writeAttributeHourFormatWithValue_params_completionHandlerSelector = mkSelector "writeAttributeHourFormatWithValue:params:completionHandler:"

-- | @Selector@ for @subscribeAttributeHourFormatWithMinInterval:maxInterval:params:subscriptionEstablished:reportHandler:@
subscribeAttributeHourFormatWithMinInterval_maxInterval_params_subscriptionEstablished_reportHandlerSelector :: Selector '[Id NSNumber, Id NSNumber, Id MTRSubscribeParams, Ptr (), Ptr ()] ()
subscribeAttributeHourFormatWithMinInterval_maxInterval_params_subscriptionEstablished_reportHandlerSelector = mkSelector "subscribeAttributeHourFormatWithMinInterval:maxInterval:params:subscriptionEstablished:reportHandler:"

-- | @Selector@ for @readAttributeHourFormatWithAttributeCache:endpoint:queue:completionHandler:@
readAttributeHourFormatWithAttributeCache_endpoint_queue_completionHandlerSelector :: Selector '[Id MTRAttributeCacheContainer, Id NSNumber, Id NSObject, Ptr ()] ()
readAttributeHourFormatWithAttributeCache_endpoint_queue_completionHandlerSelector = mkSelector "readAttributeHourFormatWithAttributeCache:endpoint:queue:completionHandler:"

-- | @Selector@ for @readAttributeActiveCalendarTypeWithCompletionHandler:@
readAttributeActiveCalendarTypeWithCompletionHandlerSelector :: Selector '[Ptr ()] ()
readAttributeActiveCalendarTypeWithCompletionHandlerSelector = mkSelector "readAttributeActiveCalendarTypeWithCompletionHandler:"

-- | @Selector@ for @writeAttributeActiveCalendarTypeWithValue:completionHandler:@
writeAttributeActiveCalendarTypeWithValue_completionHandlerSelector :: Selector '[Id NSNumber, Ptr ()] ()
writeAttributeActiveCalendarTypeWithValue_completionHandlerSelector = mkSelector "writeAttributeActiveCalendarTypeWithValue:completionHandler:"

-- | @Selector@ for @writeAttributeActiveCalendarTypeWithValue:params:completionHandler:@
writeAttributeActiveCalendarTypeWithValue_params_completionHandlerSelector :: Selector '[Id NSNumber, Id MTRWriteParams, Ptr ()] ()
writeAttributeActiveCalendarTypeWithValue_params_completionHandlerSelector = mkSelector "writeAttributeActiveCalendarTypeWithValue:params:completionHandler:"

-- | @Selector@ for @subscribeAttributeActiveCalendarTypeWithMinInterval:maxInterval:params:subscriptionEstablished:reportHandler:@
subscribeAttributeActiveCalendarTypeWithMinInterval_maxInterval_params_subscriptionEstablished_reportHandlerSelector :: Selector '[Id NSNumber, Id NSNumber, Id MTRSubscribeParams, Ptr (), Ptr ()] ()
subscribeAttributeActiveCalendarTypeWithMinInterval_maxInterval_params_subscriptionEstablished_reportHandlerSelector = mkSelector "subscribeAttributeActiveCalendarTypeWithMinInterval:maxInterval:params:subscriptionEstablished:reportHandler:"

-- | @Selector@ for @readAttributeActiveCalendarTypeWithAttributeCache:endpoint:queue:completionHandler:@
readAttributeActiveCalendarTypeWithAttributeCache_endpoint_queue_completionHandlerSelector :: Selector '[Id MTRAttributeCacheContainer, Id NSNumber, Id NSObject, Ptr ()] ()
readAttributeActiveCalendarTypeWithAttributeCache_endpoint_queue_completionHandlerSelector = mkSelector "readAttributeActiveCalendarTypeWithAttributeCache:endpoint:queue:completionHandler:"

-- | @Selector@ for @readAttributeSupportedCalendarTypesWithCompletionHandler:@
readAttributeSupportedCalendarTypesWithCompletionHandlerSelector :: Selector '[Ptr ()] ()
readAttributeSupportedCalendarTypesWithCompletionHandlerSelector = mkSelector "readAttributeSupportedCalendarTypesWithCompletionHandler:"

-- | @Selector@ for @subscribeAttributeSupportedCalendarTypesWithMinInterval:maxInterval:params:subscriptionEstablished:reportHandler:@
subscribeAttributeSupportedCalendarTypesWithMinInterval_maxInterval_params_subscriptionEstablished_reportHandlerSelector :: Selector '[Id NSNumber, Id NSNumber, Id MTRSubscribeParams, Ptr (), Ptr ()] ()
subscribeAttributeSupportedCalendarTypesWithMinInterval_maxInterval_params_subscriptionEstablished_reportHandlerSelector = mkSelector "subscribeAttributeSupportedCalendarTypesWithMinInterval:maxInterval:params:subscriptionEstablished:reportHandler:"

-- | @Selector@ for @readAttributeSupportedCalendarTypesWithAttributeCache:endpoint:queue:completionHandler:@
readAttributeSupportedCalendarTypesWithAttributeCache_endpoint_queue_completionHandlerSelector :: Selector '[Id MTRAttributeCacheContainer, Id NSNumber, Id NSObject, Ptr ()] ()
readAttributeSupportedCalendarTypesWithAttributeCache_endpoint_queue_completionHandlerSelector = mkSelector "readAttributeSupportedCalendarTypesWithAttributeCache:endpoint:queue:completionHandler:"

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
initWithDevice_endpointID_queueSelector :: Selector '[Id MTRBaseDevice, Id NSNumber, Id NSObject] (Id MTRBaseClusterTimeFormatLocalization)
initWithDevice_endpointID_queueSelector = mkSelector "initWithDevice:endpointID:queue:"

