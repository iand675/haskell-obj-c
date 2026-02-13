{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Cluster Channel
--
-- This cluster provides an interface for controlling the current Channel on a device.
--
-- Generated bindings for @MTRBaseClusterChannel@.
module ObjC.Matter.MTRBaseClusterChannel
  ( MTRBaseClusterChannel
  , IsMTRBaseClusterChannel(..)
  , changeChannelWithParams_completion
  , changeChannelByNumberWithParams_completion
  , skipChannelWithParams_completion
  , getProgramGuideWithParams_completion
  , getProgramGuideWithCompletion
  , recordProgramWithParams_completion
  , cancelRecordProgramWithParams_completion
  , readAttributeChannelListWithCompletion
  , subscribeAttributeChannelListWithParams_subscriptionEstablished_reportHandler
  , readAttributeChannelListWithClusterStateCache_endpoint_queue_completion
  , readAttributeLineupWithCompletion
  , subscribeAttributeLineupWithParams_subscriptionEstablished_reportHandler
  , readAttributeLineupWithClusterStateCache_endpoint_queue_completion
  , readAttributeCurrentChannelWithCompletion
  , subscribeAttributeCurrentChannelWithParams_subscriptionEstablished_reportHandler
  , readAttributeCurrentChannelWithClusterStateCache_endpoint_queue_completion
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
  , changeChannelWithParams_completionHandler
  , changeChannelByNumberWithParams_completionHandler
  , skipChannelWithParams_completionHandler
  , readAttributeChannelListWithCompletionHandler
  , subscribeAttributeChannelListWithMinInterval_maxInterval_params_subscriptionEstablished_reportHandler
  , readAttributeChannelListWithAttributeCache_endpoint_queue_completionHandler
  , readAttributeLineupWithCompletionHandler
  , subscribeAttributeLineupWithMinInterval_maxInterval_params_subscriptionEstablished_reportHandler
  , readAttributeLineupWithAttributeCache_endpoint_queue_completionHandler
  , readAttributeCurrentChannelWithCompletionHandler
  , subscribeAttributeCurrentChannelWithMinInterval_maxInterval_params_subscriptionEstablished_reportHandler
  , readAttributeCurrentChannelWithAttributeCache_endpoint_queue_completionHandler
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
  , cancelRecordProgramWithParams_completionSelector
  , changeChannelByNumberWithParams_completionHandlerSelector
  , changeChannelByNumberWithParams_completionSelector
  , changeChannelWithParams_completionHandlerSelector
  , changeChannelWithParams_completionSelector
  , getProgramGuideWithCompletionSelector
  , getProgramGuideWithParams_completionSelector
  , initSelector
  , initWithDevice_endpointID_queueSelector
  , initWithDevice_endpoint_queueSelector
  , newSelector
  , readAttributeAcceptedCommandListWithAttributeCache_endpoint_queue_completionHandlerSelector
  , readAttributeAcceptedCommandListWithClusterStateCache_endpoint_queue_completionSelector
  , readAttributeAcceptedCommandListWithCompletionHandlerSelector
  , readAttributeAcceptedCommandListWithCompletionSelector
  , readAttributeAttributeListWithAttributeCache_endpoint_queue_completionHandlerSelector
  , readAttributeAttributeListWithClusterStateCache_endpoint_queue_completionSelector
  , readAttributeAttributeListWithCompletionHandlerSelector
  , readAttributeAttributeListWithCompletionSelector
  , readAttributeChannelListWithAttributeCache_endpoint_queue_completionHandlerSelector
  , readAttributeChannelListWithClusterStateCache_endpoint_queue_completionSelector
  , readAttributeChannelListWithCompletionHandlerSelector
  , readAttributeChannelListWithCompletionSelector
  , readAttributeClusterRevisionWithAttributeCache_endpoint_queue_completionHandlerSelector
  , readAttributeClusterRevisionWithClusterStateCache_endpoint_queue_completionSelector
  , readAttributeClusterRevisionWithCompletionHandlerSelector
  , readAttributeClusterRevisionWithCompletionSelector
  , readAttributeCurrentChannelWithAttributeCache_endpoint_queue_completionHandlerSelector
  , readAttributeCurrentChannelWithClusterStateCache_endpoint_queue_completionSelector
  , readAttributeCurrentChannelWithCompletionHandlerSelector
  , readAttributeCurrentChannelWithCompletionSelector
  , readAttributeFeatureMapWithAttributeCache_endpoint_queue_completionHandlerSelector
  , readAttributeFeatureMapWithClusterStateCache_endpoint_queue_completionSelector
  , readAttributeFeatureMapWithCompletionHandlerSelector
  , readAttributeFeatureMapWithCompletionSelector
  , readAttributeGeneratedCommandListWithAttributeCache_endpoint_queue_completionHandlerSelector
  , readAttributeGeneratedCommandListWithClusterStateCache_endpoint_queue_completionSelector
  , readAttributeGeneratedCommandListWithCompletionHandlerSelector
  , readAttributeGeneratedCommandListWithCompletionSelector
  , readAttributeLineupWithAttributeCache_endpoint_queue_completionHandlerSelector
  , readAttributeLineupWithClusterStateCache_endpoint_queue_completionSelector
  , readAttributeLineupWithCompletionHandlerSelector
  , readAttributeLineupWithCompletionSelector
  , recordProgramWithParams_completionSelector
  , skipChannelWithParams_completionHandlerSelector
  , skipChannelWithParams_completionSelector
  , subscribeAttributeAcceptedCommandListWithMinInterval_maxInterval_params_subscriptionEstablished_reportHandlerSelector
  , subscribeAttributeAcceptedCommandListWithParams_subscriptionEstablished_reportHandlerSelector
  , subscribeAttributeAttributeListWithMinInterval_maxInterval_params_subscriptionEstablished_reportHandlerSelector
  , subscribeAttributeAttributeListWithParams_subscriptionEstablished_reportHandlerSelector
  , subscribeAttributeChannelListWithMinInterval_maxInterval_params_subscriptionEstablished_reportHandlerSelector
  , subscribeAttributeChannelListWithParams_subscriptionEstablished_reportHandlerSelector
  , subscribeAttributeClusterRevisionWithMinInterval_maxInterval_params_subscriptionEstablished_reportHandlerSelector
  , subscribeAttributeClusterRevisionWithParams_subscriptionEstablished_reportHandlerSelector
  , subscribeAttributeCurrentChannelWithMinInterval_maxInterval_params_subscriptionEstablished_reportHandlerSelector
  , subscribeAttributeCurrentChannelWithParams_subscriptionEstablished_reportHandlerSelector
  , subscribeAttributeFeatureMapWithMinInterval_maxInterval_params_subscriptionEstablished_reportHandlerSelector
  , subscribeAttributeFeatureMapWithParams_subscriptionEstablished_reportHandlerSelector
  , subscribeAttributeGeneratedCommandListWithMinInterval_maxInterval_params_subscriptionEstablished_reportHandlerSelector
  , subscribeAttributeGeneratedCommandListWithParams_subscriptionEstablished_reportHandlerSelector
  , subscribeAttributeLineupWithMinInterval_maxInterval_params_subscriptionEstablished_reportHandlerSelector
  , subscribeAttributeLineupWithParams_subscriptionEstablished_reportHandlerSelector


  ) where

import Foreign.Ptr (Ptr, FunPtr)
import Foreign.C.Types

import ObjC.Runtime.Types
import ObjC.Runtime.Message (sendMessage, sendOwnedMessage, sendClassMessage, sendOwnedClassMessage)
import ObjC.Runtime.Selector (mkSelector)
import ObjC.Runtime.Class (getRequiredClass)

import ObjC.Matter.Internal.Classes
import ObjC.Foundation.Internal.Classes

-- | Command ChangeChannel
--
-- Change the channel on the media player to the channel case-insensitive exact matching the value passed as an argument.
--
-- ObjC selector: @- changeChannelWithParams:completion:@
changeChannelWithParams_completion :: (IsMTRBaseClusterChannel mtrBaseClusterChannel, IsMTRChannelClusterChangeChannelParams params) => mtrBaseClusterChannel -> params -> Ptr () -> IO ()
changeChannelWithParams_completion mtrBaseClusterChannel params completion =
  sendMessage mtrBaseClusterChannel changeChannelWithParams_completionSelector (toMTRChannelClusterChangeChannelParams params) completion

-- | Command ChangeChannelByNumber
--
-- Change the channel on the media plaeyer to the channel with the given Number in the ChannelList attribute.
--
-- ObjC selector: @- changeChannelByNumberWithParams:completion:@
changeChannelByNumberWithParams_completion :: (IsMTRBaseClusterChannel mtrBaseClusterChannel, IsMTRChannelClusterChangeChannelByNumberParams params) => mtrBaseClusterChannel -> params -> Ptr () -> IO ()
changeChannelByNumberWithParams_completion mtrBaseClusterChannel params completion =
  sendMessage mtrBaseClusterChannel changeChannelByNumberWithParams_completionSelector (toMTRChannelClusterChangeChannelByNumberParams params) completion

-- | Command SkipChannel
--
-- This command provides channel up and channel down functionality, but allows channel index jumps of size Count. When the value of the increase or decrease is larger than the number of channels remaining in the given direction, then the behavior SHALL be to return to the beginning (or end) of the channel list and continue. For example, if the current channel is at index 0 and count value of -1 is given, then the current channel should change to the last channel.
--
-- ObjC selector: @- skipChannelWithParams:completion:@
skipChannelWithParams_completion :: (IsMTRBaseClusterChannel mtrBaseClusterChannel, IsMTRChannelClusterSkipChannelParams params) => mtrBaseClusterChannel -> params -> Ptr () -> IO ()
skipChannelWithParams_completion mtrBaseClusterChannel params completion =
  sendMessage mtrBaseClusterChannel skipChannelWithParams_completionSelector (toMTRChannelClusterSkipChannelParams params) completion

-- | Command GetProgramGuide
--
-- This command retrieves the program guide. It accepts several filter parameters to return specific schedule and program information from a content app. The command shall receive in response a ProgramGuideResponse.
--
-- ObjC selector: @- getProgramGuideWithParams:completion:@
getProgramGuideWithParams_completion :: (IsMTRBaseClusterChannel mtrBaseClusterChannel, IsMTRChannelClusterGetProgramGuideParams params) => mtrBaseClusterChannel -> params -> Ptr () -> IO ()
getProgramGuideWithParams_completion mtrBaseClusterChannel params completion =
  sendMessage mtrBaseClusterChannel getProgramGuideWithParams_completionSelector (toMTRChannelClusterGetProgramGuideParams params) completion

-- | @- getProgramGuideWithCompletion:@
getProgramGuideWithCompletion :: IsMTRBaseClusterChannel mtrBaseClusterChannel => mtrBaseClusterChannel -> Ptr () -> IO ()
getProgramGuideWithCompletion mtrBaseClusterChannel completion =
  sendMessage mtrBaseClusterChannel getProgramGuideWithCompletionSelector completion

-- | Command RecordProgram
--
-- Record a specific program or series when it goes live. This functionality enables DVR recording features.
--
-- ObjC selector: @- recordProgramWithParams:completion:@
recordProgramWithParams_completion :: (IsMTRBaseClusterChannel mtrBaseClusterChannel, IsMTRChannelClusterRecordProgramParams params) => mtrBaseClusterChannel -> params -> Ptr () -> IO ()
recordProgramWithParams_completion mtrBaseClusterChannel params completion =
  sendMessage mtrBaseClusterChannel recordProgramWithParams_completionSelector (toMTRChannelClusterRecordProgramParams params) completion

-- | Command CancelRecordProgram
--
-- Cancel recording for a specific program or series.
--
-- ObjC selector: @- cancelRecordProgramWithParams:completion:@
cancelRecordProgramWithParams_completion :: (IsMTRBaseClusterChannel mtrBaseClusterChannel, IsMTRChannelClusterCancelRecordProgramParams params) => mtrBaseClusterChannel -> params -> Ptr () -> IO ()
cancelRecordProgramWithParams_completion mtrBaseClusterChannel params completion =
  sendMessage mtrBaseClusterChannel cancelRecordProgramWithParams_completionSelector (toMTRChannelClusterCancelRecordProgramParams params) completion

-- | @- readAttributeChannelListWithCompletion:@
readAttributeChannelListWithCompletion :: IsMTRBaseClusterChannel mtrBaseClusterChannel => mtrBaseClusterChannel -> Ptr () -> IO ()
readAttributeChannelListWithCompletion mtrBaseClusterChannel completion =
  sendMessage mtrBaseClusterChannel readAttributeChannelListWithCompletionSelector completion

-- | @- subscribeAttributeChannelListWithParams:subscriptionEstablished:reportHandler:@
subscribeAttributeChannelListWithParams_subscriptionEstablished_reportHandler :: (IsMTRBaseClusterChannel mtrBaseClusterChannel, IsMTRSubscribeParams params) => mtrBaseClusterChannel -> params -> Ptr () -> Ptr () -> IO ()
subscribeAttributeChannelListWithParams_subscriptionEstablished_reportHandler mtrBaseClusterChannel params subscriptionEstablished reportHandler =
  sendMessage mtrBaseClusterChannel subscribeAttributeChannelListWithParams_subscriptionEstablished_reportHandlerSelector (toMTRSubscribeParams params) subscriptionEstablished reportHandler

-- | @+ readAttributeChannelListWithClusterStateCache:endpoint:queue:completion:@
readAttributeChannelListWithClusterStateCache_endpoint_queue_completion :: (IsMTRClusterStateCacheContainer clusterStateCacheContainer, IsNSNumber endpoint, IsNSObject queue) => clusterStateCacheContainer -> endpoint -> queue -> Ptr () -> IO ()
readAttributeChannelListWithClusterStateCache_endpoint_queue_completion clusterStateCacheContainer endpoint queue completion =
  do
    cls' <- getRequiredClass "MTRBaseClusterChannel"
    sendClassMessage cls' readAttributeChannelListWithClusterStateCache_endpoint_queue_completionSelector (toMTRClusterStateCacheContainer clusterStateCacheContainer) (toNSNumber endpoint) (toNSObject queue) completion

-- | @- readAttributeLineupWithCompletion:@
readAttributeLineupWithCompletion :: IsMTRBaseClusterChannel mtrBaseClusterChannel => mtrBaseClusterChannel -> Ptr () -> IO ()
readAttributeLineupWithCompletion mtrBaseClusterChannel completion =
  sendMessage mtrBaseClusterChannel readAttributeLineupWithCompletionSelector completion

-- | @- subscribeAttributeLineupWithParams:subscriptionEstablished:reportHandler:@
subscribeAttributeLineupWithParams_subscriptionEstablished_reportHandler :: (IsMTRBaseClusterChannel mtrBaseClusterChannel, IsMTRSubscribeParams params) => mtrBaseClusterChannel -> params -> Ptr () -> Ptr () -> IO ()
subscribeAttributeLineupWithParams_subscriptionEstablished_reportHandler mtrBaseClusterChannel params subscriptionEstablished reportHandler =
  sendMessage mtrBaseClusterChannel subscribeAttributeLineupWithParams_subscriptionEstablished_reportHandlerSelector (toMTRSubscribeParams params) subscriptionEstablished reportHandler

-- | @+ readAttributeLineupWithClusterStateCache:endpoint:queue:completion:@
readAttributeLineupWithClusterStateCache_endpoint_queue_completion :: (IsMTRClusterStateCacheContainer clusterStateCacheContainer, IsNSNumber endpoint, IsNSObject queue) => clusterStateCacheContainer -> endpoint -> queue -> Ptr () -> IO ()
readAttributeLineupWithClusterStateCache_endpoint_queue_completion clusterStateCacheContainer endpoint queue completion =
  do
    cls' <- getRequiredClass "MTRBaseClusterChannel"
    sendClassMessage cls' readAttributeLineupWithClusterStateCache_endpoint_queue_completionSelector (toMTRClusterStateCacheContainer clusterStateCacheContainer) (toNSNumber endpoint) (toNSObject queue) completion

-- | @- readAttributeCurrentChannelWithCompletion:@
readAttributeCurrentChannelWithCompletion :: IsMTRBaseClusterChannel mtrBaseClusterChannel => mtrBaseClusterChannel -> Ptr () -> IO ()
readAttributeCurrentChannelWithCompletion mtrBaseClusterChannel completion =
  sendMessage mtrBaseClusterChannel readAttributeCurrentChannelWithCompletionSelector completion

-- | @- subscribeAttributeCurrentChannelWithParams:subscriptionEstablished:reportHandler:@
subscribeAttributeCurrentChannelWithParams_subscriptionEstablished_reportHandler :: (IsMTRBaseClusterChannel mtrBaseClusterChannel, IsMTRSubscribeParams params) => mtrBaseClusterChannel -> params -> Ptr () -> Ptr () -> IO ()
subscribeAttributeCurrentChannelWithParams_subscriptionEstablished_reportHandler mtrBaseClusterChannel params subscriptionEstablished reportHandler =
  sendMessage mtrBaseClusterChannel subscribeAttributeCurrentChannelWithParams_subscriptionEstablished_reportHandlerSelector (toMTRSubscribeParams params) subscriptionEstablished reportHandler

-- | @+ readAttributeCurrentChannelWithClusterStateCache:endpoint:queue:completion:@
readAttributeCurrentChannelWithClusterStateCache_endpoint_queue_completion :: (IsMTRClusterStateCacheContainer clusterStateCacheContainer, IsNSNumber endpoint, IsNSObject queue) => clusterStateCacheContainer -> endpoint -> queue -> Ptr () -> IO ()
readAttributeCurrentChannelWithClusterStateCache_endpoint_queue_completion clusterStateCacheContainer endpoint queue completion =
  do
    cls' <- getRequiredClass "MTRBaseClusterChannel"
    sendClassMessage cls' readAttributeCurrentChannelWithClusterStateCache_endpoint_queue_completionSelector (toMTRClusterStateCacheContainer clusterStateCacheContainer) (toNSNumber endpoint) (toNSObject queue) completion

-- | @- readAttributeGeneratedCommandListWithCompletion:@
readAttributeGeneratedCommandListWithCompletion :: IsMTRBaseClusterChannel mtrBaseClusterChannel => mtrBaseClusterChannel -> Ptr () -> IO ()
readAttributeGeneratedCommandListWithCompletion mtrBaseClusterChannel completion =
  sendMessage mtrBaseClusterChannel readAttributeGeneratedCommandListWithCompletionSelector completion

-- | @- subscribeAttributeGeneratedCommandListWithParams:subscriptionEstablished:reportHandler:@
subscribeAttributeGeneratedCommandListWithParams_subscriptionEstablished_reportHandler :: (IsMTRBaseClusterChannel mtrBaseClusterChannel, IsMTRSubscribeParams params) => mtrBaseClusterChannel -> params -> Ptr () -> Ptr () -> IO ()
subscribeAttributeGeneratedCommandListWithParams_subscriptionEstablished_reportHandler mtrBaseClusterChannel params subscriptionEstablished reportHandler =
  sendMessage mtrBaseClusterChannel subscribeAttributeGeneratedCommandListWithParams_subscriptionEstablished_reportHandlerSelector (toMTRSubscribeParams params) subscriptionEstablished reportHandler

-- | @+ readAttributeGeneratedCommandListWithClusterStateCache:endpoint:queue:completion:@
readAttributeGeneratedCommandListWithClusterStateCache_endpoint_queue_completion :: (IsMTRClusterStateCacheContainer clusterStateCacheContainer, IsNSNumber endpoint, IsNSObject queue) => clusterStateCacheContainer -> endpoint -> queue -> Ptr () -> IO ()
readAttributeGeneratedCommandListWithClusterStateCache_endpoint_queue_completion clusterStateCacheContainer endpoint queue completion =
  do
    cls' <- getRequiredClass "MTRBaseClusterChannel"
    sendClassMessage cls' readAttributeGeneratedCommandListWithClusterStateCache_endpoint_queue_completionSelector (toMTRClusterStateCacheContainer clusterStateCacheContainer) (toNSNumber endpoint) (toNSObject queue) completion

-- | @- readAttributeAcceptedCommandListWithCompletion:@
readAttributeAcceptedCommandListWithCompletion :: IsMTRBaseClusterChannel mtrBaseClusterChannel => mtrBaseClusterChannel -> Ptr () -> IO ()
readAttributeAcceptedCommandListWithCompletion mtrBaseClusterChannel completion =
  sendMessage mtrBaseClusterChannel readAttributeAcceptedCommandListWithCompletionSelector completion

-- | @- subscribeAttributeAcceptedCommandListWithParams:subscriptionEstablished:reportHandler:@
subscribeAttributeAcceptedCommandListWithParams_subscriptionEstablished_reportHandler :: (IsMTRBaseClusterChannel mtrBaseClusterChannel, IsMTRSubscribeParams params) => mtrBaseClusterChannel -> params -> Ptr () -> Ptr () -> IO ()
subscribeAttributeAcceptedCommandListWithParams_subscriptionEstablished_reportHandler mtrBaseClusterChannel params subscriptionEstablished reportHandler =
  sendMessage mtrBaseClusterChannel subscribeAttributeAcceptedCommandListWithParams_subscriptionEstablished_reportHandlerSelector (toMTRSubscribeParams params) subscriptionEstablished reportHandler

-- | @+ readAttributeAcceptedCommandListWithClusterStateCache:endpoint:queue:completion:@
readAttributeAcceptedCommandListWithClusterStateCache_endpoint_queue_completion :: (IsMTRClusterStateCacheContainer clusterStateCacheContainer, IsNSNumber endpoint, IsNSObject queue) => clusterStateCacheContainer -> endpoint -> queue -> Ptr () -> IO ()
readAttributeAcceptedCommandListWithClusterStateCache_endpoint_queue_completion clusterStateCacheContainer endpoint queue completion =
  do
    cls' <- getRequiredClass "MTRBaseClusterChannel"
    sendClassMessage cls' readAttributeAcceptedCommandListWithClusterStateCache_endpoint_queue_completionSelector (toMTRClusterStateCacheContainer clusterStateCacheContainer) (toNSNumber endpoint) (toNSObject queue) completion

-- | @- readAttributeAttributeListWithCompletion:@
readAttributeAttributeListWithCompletion :: IsMTRBaseClusterChannel mtrBaseClusterChannel => mtrBaseClusterChannel -> Ptr () -> IO ()
readAttributeAttributeListWithCompletion mtrBaseClusterChannel completion =
  sendMessage mtrBaseClusterChannel readAttributeAttributeListWithCompletionSelector completion

-- | @- subscribeAttributeAttributeListWithParams:subscriptionEstablished:reportHandler:@
subscribeAttributeAttributeListWithParams_subscriptionEstablished_reportHandler :: (IsMTRBaseClusterChannel mtrBaseClusterChannel, IsMTRSubscribeParams params) => mtrBaseClusterChannel -> params -> Ptr () -> Ptr () -> IO ()
subscribeAttributeAttributeListWithParams_subscriptionEstablished_reportHandler mtrBaseClusterChannel params subscriptionEstablished reportHandler =
  sendMessage mtrBaseClusterChannel subscribeAttributeAttributeListWithParams_subscriptionEstablished_reportHandlerSelector (toMTRSubscribeParams params) subscriptionEstablished reportHandler

-- | @+ readAttributeAttributeListWithClusterStateCache:endpoint:queue:completion:@
readAttributeAttributeListWithClusterStateCache_endpoint_queue_completion :: (IsMTRClusterStateCacheContainer clusterStateCacheContainer, IsNSNumber endpoint, IsNSObject queue) => clusterStateCacheContainer -> endpoint -> queue -> Ptr () -> IO ()
readAttributeAttributeListWithClusterStateCache_endpoint_queue_completion clusterStateCacheContainer endpoint queue completion =
  do
    cls' <- getRequiredClass "MTRBaseClusterChannel"
    sendClassMessage cls' readAttributeAttributeListWithClusterStateCache_endpoint_queue_completionSelector (toMTRClusterStateCacheContainer clusterStateCacheContainer) (toNSNumber endpoint) (toNSObject queue) completion

-- | @- readAttributeFeatureMapWithCompletion:@
readAttributeFeatureMapWithCompletion :: IsMTRBaseClusterChannel mtrBaseClusterChannel => mtrBaseClusterChannel -> Ptr () -> IO ()
readAttributeFeatureMapWithCompletion mtrBaseClusterChannel completion =
  sendMessage mtrBaseClusterChannel readAttributeFeatureMapWithCompletionSelector completion

-- | @- subscribeAttributeFeatureMapWithParams:subscriptionEstablished:reportHandler:@
subscribeAttributeFeatureMapWithParams_subscriptionEstablished_reportHandler :: (IsMTRBaseClusterChannel mtrBaseClusterChannel, IsMTRSubscribeParams params) => mtrBaseClusterChannel -> params -> Ptr () -> Ptr () -> IO ()
subscribeAttributeFeatureMapWithParams_subscriptionEstablished_reportHandler mtrBaseClusterChannel params subscriptionEstablished reportHandler =
  sendMessage mtrBaseClusterChannel subscribeAttributeFeatureMapWithParams_subscriptionEstablished_reportHandlerSelector (toMTRSubscribeParams params) subscriptionEstablished reportHandler

-- | @+ readAttributeFeatureMapWithClusterStateCache:endpoint:queue:completion:@
readAttributeFeatureMapWithClusterStateCache_endpoint_queue_completion :: (IsMTRClusterStateCacheContainer clusterStateCacheContainer, IsNSNumber endpoint, IsNSObject queue) => clusterStateCacheContainer -> endpoint -> queue -> Ptr () -> IO ()
readAttributeFeatureMapWithClusterStateCache_endpoint_queue_completion clusterStateCacheContainer endpoint queue completion =
  do
    cls' <- getRequiredClass "MTRBaseClusterChannel"
    sendClassMessage cls' readAttributeFeatureMapWithClusterStateCache_endpoint_queue_completionSelector (toMTRClusterStateCacheContainer clusterStateCacheContainer) (toNSNumber endpoint) (toNSObject queue) completion

-- | @- readAttributeClusterRevisionWithCompletion:@
readAttributeClusterRevisionWithCompletion :: IsMTRBaseClusterChannel mtrBaseClusterChannel => mtrBaseClusterChannel -> Ptr () -> IO ()
readAttributeClusterRevisionWithCompletion mtrBaseClusterChannel completion =
  sendMessage mtrBaseClusterChannel readAttributeClusterRevisionWithCompletionSelector completion

-- | @- subscribeAttributeClusterRevisionWithParams:subscriptionEstablished:reportHandler:@
subscribeAttributeClusterRevisionWithParams_subscriptionEstablished_reportHandler :: (IsMTRBaseClusterChannel mtrBaseClusterChannel, IsMTRSubscribeParams params) => mtrBaseClusterChannel -> params -> Ptr () -> Ptr () -> IO ()
subscribeAttributeClusterRevisionWithParams_subscriptionEstablished_reportHandler mtrBaseClusterChannel params subscriptionEstablished reportHandler =
  sendMessage mtrBaseClusterChannel subscribeAttributeClusterRevisionWithParams_subscriptionEstablished_reportHandlerSelector (toMTRSubscribeParams params) subscriptionEstablished reportHandler

-- | @+ readAttributeClusterRevisionWithClusterStateCache:endpoint:queue:completion:@
readAttributeClusterRevisionWithClusterStateCache_endpoint_queue_completion :: (IsMTRClusterStateCacheContainer clusterStateCacheContainer, IsNSNumber endpoint, IsNSObject queue) => clusterStateCacheContainer -> endpoint -> queue -> Ptr () -> IO ()
readAttributeClusterRevisionWithClusterStateCache_endpoint_queue_completion clusterStateCacheContainer endpoint queue completion =
  do
    cls' <- getRequiredClass "MTRBaseClusterChannel"
    sendClassMessage cls' readAttributeClusterRevisionWithClusterStateCache_endpoint_queue_completionSelector (toMTRClusterStateCacheContainer clusterStateCacheContainer) (toNSNumber endpoint) (toNSObject queue) completion

-- | @- init@
init_ :: IsMTRBaseClusterChannel mtrBaseClusterChannel => mtrBaseClusterChannel -> IO (Id MTRBaseClusterChannel)
init_ mtrBaseClusterChannel =
  sendOwnedMessage mtrBaseClusterChannel initSelector

-- | @+ new@
new :: IO (Id MTRBaseClusterChannel)
new  =
  do
    cls' <- getRequiredClass "MTRBaseClusterChannel"
    sendOwnedClassMessage cls' newSelector

-- | @- initWithDevice:endpoint:queue:@
initWithDevice_endpoint_queue :: (IsMTRBaseClusterChannel mtrBaseClusterChannel, IsMTRBaseDevice device, IsNSObject queue) => mtrBaseClusterChannel -> device -> CUShort -> queue -> IO (Id MTRBaseClusterChannel)
initWithDevice_endpoint_queue mtrBaseClusterChannel device endpoint queue =
  sendOwnedMessage mtrBaseClusterChannel initWithDevice_endpoint_queueSelector (toMTRBaseDevice device) endpoint (toNSObject queue)

-- | @- changeChannelWithParams:completionHandler:@
changeChannelWithParams_completionHandler :: (IsMTRBaseClusterChannel mtrBaseClusterChannel, IsMTRChannelClusterChangeChannelParams params) => mtrBaseClusterChannel -> params -> Ptr () -> IO ()
changeChannelWithParams_completionHandler mtrBaseClusterChannel params completionHandler =
  sendMessage mtrBaseClusterChannel changeChannelWithParams_completionHandlerSelector (toMTRChannelClusterChangeChannelParams params) completionHandler

-- | @- changeChannelByNumberWithParams:completionHandler:@
changeChannelByNumberWithParams_completionHandler :: (IsMTRBaseClusterChannel mtrBaseClusterChannel, IsMTRChannelClusterChangeChannelByNumberParams params) => mtrBaseClusterChannel -> params -> Ptr () -> IO ()
changeChannelByNumberWithParams_completionHandler mtrBaseClusterChannel params completionHandler =
  sendMessage mtrBaseClusterChannel changeChannelByNumberWithParams_completionHandlerSelector (toMTRChannelClusterChangeChannelByNumberParams params) completionHandler

-- | @- skipChannelWithParams:completionHandler:@
skipChannelWithParams_completionHandler :: (IsMTRBaseClusterChannel mtrBaseClusterChannel, IsMTRChannelClusterSkipChannelParams params) => mtrBaseClusterChannel -> params -> Ptr () -> IO ()
skipChannelWithParams_completionHandler mtrBaseClusterChannel params completionHandler =
  sendMessage mtrBaseClusterChannel skipChannelWithParams_completionHandlerSelector (toMTRChannelClusterSkipChannelParams params) completionHandler

-- | @- readAttributeChannelListWithCompletionHandler:@
readAttributeChannelListWithCompletionHandler :: IsMTRBaseClusterChannel mtrBaseClusterChannel => mtrBaseClusterChannel -> Ptr () -> IO ()
readAttributeChannelListWithCompletionHandler mtrBaseClusterChannel completionHandler =
  sendMessage mtrBaseClusterChannel readAttributeChannelListWithCompletionHandlerSelector completionHandler

-- | @- subscribeAttributeChannelListWithMinInterval:maxInterval:params:subscriptionEstablished:reportHandler:@
subscribeAttributeChannelListWithMinInterval_maxInterval_params_subscriptionEstablished_reportHandler :: (IsMTRBaseClusterChannel mtrBaseClusterChannel, IsNSNumber minInterval, IsNSNumber maxInterval, IsMTRSubscribeParams params) => mtrBaseClusterChannel -> minInterval -> maxInterval -> params -> Ptr () -> Ptr () -> IO ()
subscribeAttributeChannelListWithMinInterval_maxInterval_params_subscriptionEstablished_reportHandler mtrBaseClusterChannel minInterval maxInterval params subscriptionEstablishedHandler reportHandler =
  sendMessage mtrBaseClusterChannel subscribeAttributeChannelListWithMinInterval_maxInterval_params_subscriptionEstablished_reportHandlerSelector (toNSNumber minInterval) (toNSNumber maxInterval) (toMTRSubscribeParams params) subscriptionEstablishedHandler reportHandler

-- | @+ readAttributeChannelListWithAttributeCache:endpoint:queue:completionHandler:@
readAttributeChannelListWithAttributeCache_endpoint_queue_completionHandler :: (IsMTRAttributeCacheContainer attributeCacheContainer, IsNSNumber endpoint, IsNSObject queue) => attributeCacheContainer -> endpoint -> queue -> Ptr () -> IO ()
readAttributeChannelListWithAttributeCache_endpoint_queue_completionHandler attributeCacheContainer endpoint queue completionHandler =
  do
    cls' <- getRequiredClass "MTRBaseClusterChannel"
    sendClassMessage cls' readAttributeChannelListWithAttributeCache_endpoint_queue_completionHandlerSelector (toMTRAttributeCacheContainer attributeCacheContainer) (toNSNumber endpoint) (toNSObject queue) completionHandler

-- | @- readAttributeLineupWithCompletionHandler:@
readAttributeLineupWithCompletionHandler :: IsMTRBaseClusterChannel mtrBaseClusterChannel => mtrBaseClusterChannel -> Ptr () -> IO ()
readAttributeLineupWithCompletionHandler mtrBaseClusterChannel completionHandler =
  sendMessage mtrBaseClusterChannel readAttributeLineupWithCompletionHandlerSelector completionHandler

-- | @- subscribeAttributeLineupWithMinInterval:maxInterval:params:subscriptionEstablished:reportHandler:@
subscribeAttributeLineupWithMinInterval_maxInterval_params_subscriptionEstablished_reportHandler :: (IsMTRBaseClusterChannel mtrBaseClusterChannel, IsNSNumber minInterval, IsNSNumber maxInterval, IsMTRSubscribeParams params) => mtrBaseClusterChannel -> minInterval -> maxInterval -> params -> Ptr () -> Ptr () -> IO ()
subscribeAttributeLineupWithMinInterval_maxInterval_params_subscriptionEstablished_reportHandler mtrBaseClusterChannel minInterval maxInterval params subscriptionEstablishedHandler reportHandler =
  sendMessage mtrBaseClusterChannel subscribeAttributeLineupWithMinInterval_maxInterval_params_subscriptionEstablished_reportHandlerSelector (toNSNumber minInterval) (toNSNumber maxInterval) (toMTRSubscribeParams params) subscriptionEstablishedHandler reportHandler

-- | @+ readAttributeLineupWithAttributeCache:endpoint:queue:completionHandler:@
readAttributeLineupWithAttributeCache_endpoint_queue_completionHandler :: (IsMTRAttributeCacheContainer attributeCacheContainer, IsNSNumber endpoint, IsNSObject queue) => attributeCacheContainer -> endpoint -> queue -> Ptr () -> IO ()
readAttributeLineupWithAttributeCache_endpoint_queue_completionHandler attributeCacheContainer endpoint queue completionHandler =
  do
    cls' <- getRequiredClass "MTRBaseClusterChannel"
    sendClassMessage cls' readAttributeLineupWithAttributeCache_endpoint_queue_completionHandlerSelector (toMTRAttributeCacheContainer attributeCacheContainer) (toNSNumber endpoint) (toNSObject queue) completionHandler

-- | @- readAttributeCurrentChannelWithCompletionHandler:@
readAttributeCurrentChannelWithCompletionHandler :: IsMTRBaseClusterChannel mtrBaseClusterChannel => mtrBaseClusterChannel -> Ptr () -> IO ()
readAttributeCurrentChannelWithCompletionHandler mtrBaseClusterChannel completionHandler =
  sendMessage mtrBaseClusterChannel readAttributeCurrentChannelWithCompletionHandlerSelector completionHandler

-- | @- subscribeAttributeCurrentChannelWithMinInterval:maxInterval:params:subscriptionEstablished:reportHandler:@
subscribeAttributeCurrentChannelWithMinInterval_maxInterval_params_subscriptionEstablished_reportHandler :: (IsMTRBaseClusterChannel mtrBaseClusterChannel, IsNSNumber minInterval, IsNSNumber maxInterval, IsMTRSubscribeParams params) => mtrBaseClusterChannel -> minInterval -> maxInterval -> params -> Ptr () -> Ptr () -> IO ()
subscribeAttributeCurrentChannelWithMinInterval_maxInterval_params_subscriptionEstablished_reportHandler mtrBaseClusterChannel minInterval maxInterval params subscriptionEstablishedHandler reportHandler =
  sendMessage mtrBaseClusterChannel subscribeAttributeCurrentChannelWithMinInterval_maxInterval_params_subscriptionEstablished_reportHandlerSelector (toNSNumber minInterval) (toNSNumber maxInterval) (toMTRSubscribeParams params) subscriptionEstablishedHandler reportHandler

-- | @+ readAttributeCurrentChannelWithAttributeCache:endpoint:queue:completionHandler:@
readAttributeCurrentChannelWithAttributeCache_endpoint_queue_completionHandler :: (IsMTRAttributeCacheContainer attributeCacheContainer, IsNSNumber endpoint, IsNSObject queue) => attributeCacheContainer -> endpoint -> queue -> Ptr () -> IO ()
readAttributeCurrentChannelWithAttributeCache_endpoint_queue_completionHandler attributeCacheContainer endpoint queue completionHandler =
  do
    cls' <- getRequiredClass "MTRBaseClusterChannel"
    sendClassMessage cls' readAttributeCurrentChannelWithAttributeCache_endpoint_queue_completionHandlerSelector (toMTRAttributeCacheContainer attributeCacheContainer) (toNSNumber endpoint) (toNSObject queue) completionHandler

-- | @- readAttributeGeneratedCommandListWithCompletionHandler:@
readAttributeGeneratedCommandListWithCompletionHandler :: IsMTRBaseClusterChannel mtrBaseClusterChannel => mtrBaseClusterChannel -> Ptr () -> IO ()
readAttributeGeneratedCommandListWithCompletionHandler mtrBaseClusterChannel completionHandler =
  sendMessage mtrBaseClusterChannel readAttributeGeneratedCommandListWithCompletionHandlerSelector completionHandler

-- | @- subscribeAttributeGeneratedCommandListWithMinInterval:maxInterval:params:subscriptionEstablished:reportHandler:@
subscribeAttributeGeneratedCommandListWithMinInterval_maxInterval_params_subscriptionEstablished_reportHandler :: (IsMTRBaseClusterChannel mtrBaseClusterChannel, IsNSNumber minInterval, IsNSNumber maxInterval, IsMTRSubscribeParams params) => mtrBaseClusterChannel -> minInterval -> maxInterval -> params -> Ptr () -> Ptr () -> IO ()
subscribeAttributeGeneratedCommandListWithMinInterval_maxInterval_params_subscriptionEstablished_reportHandler mtrBaseClusterChannel minInterval maxInterval params subscriptionEstablishedHandler reportHandler =
  sendMessage mtrBaseClusterChannel subscribeAttributeGeneratedCommandListWithMinInterval_maxInterval_params_subscriptionEstablished_reportHandlerSelector (toNSNumber minInterval) (toNSNumber maxInterval) (toMTRSubscribeParams params) subscriptionEstablishedHandler reportHandler

-- | @+ readAttributeGeneratedCommandListWithAttributeCache:endpoint:queue:completionHandler:@
readAttributeGeneratedCommandListWithAttributeCache_endpoint_queue_completionHandler :: (IsMTRAttributeCacheContainer attributeCacheContainer, IsNSNumber endpoint, IsNSObject queue) => attributeCacheContainer -> endpoint -> queue -> Ptr () -> IO ()
readAttributeGeneratedCommandListWithAttributeCache_endpoint_queue_completionHandler attributeCacheContainer endpoint queue completionHandler =
  do
    cls' <- getRequiredClass "MTRBaseClusterChannel"
    sendClassMessage cls' readAttributeGeneratedCommandListWithAttributeCache_endpoint_queue_completionHandlerSelector (toMTRAttributeCacheContainer attributeCacheContainer) (toNSNumber endpoint) (toNSObject queue) completionHandler

-- | @- readAttributeAcceptedCommandListWithCompletionHandler:@
readAttributeAcceptedCommandListWithCompletionHandler :: IsMTRBaseClusterChannel mtrBaseClusterChannel => mtrBaseClusterChannel -> Ptr () -> IO ()
readAttributeAcceptedCommandListWithCompletionHandler mtrBaseClusterChannel completionHandler =
  sendMessage mtrBaseClusterChannel readAttributeAcceptedCommandListWithCompletionHandlerSelector completionHandler

-- | @- subscribeAttributeAcceptedCommandListWithMinInterval:maxInterval:params:subscriptionEstablished:reportHandler:@
subscribeAttributeAcceptedCommandListWithMinInterval_maxInterval_params_subscriptionEstablished_reportHandler :: (IsMTRBaseClusterChannel mtrBaseClusterChannel, IsNSNumber minInterval, IsNSNumber maxInterval, IsMTRSubscribeParams params) => mtrBaseClusterChannel -> minInterval -> maxInterval -> params -> Ptr () -> Ptr () -> IO ()
subscribeAttributeAcceptedCommandListWithMinInterval_maxInterval_params_subscriptionEstablished_reportHandler mtrBaseClusterChannel minInterval maxInterval params subscriptionEstablishedHandler reportHandler =
  sendMessage mtrBaseClusterChannel subscribeAttributeAcceptedCommandListWithMinInterval_maxInterval_params_subscriptionEstablished_reportHandlerSelector (toNSNumber minInterval) (toNSNumber maxInterval) (toMTRSubscribeParams params) subscriptionEstablishedHandler reportHandler

-- | @+ readAttributeAcceptedCommandListWithAttributeCache:endpoint:queue:completionHandler:@
readAttributeAcceptedCommandListWithAttributeCache_endpoint_queue_completionHandler :: (IsMTRAttributeCacheContainer attributeCacheContainer, IsNSNumber endpoint, IsNSObject queue) => attributeCacheContainer -> endpoint -> queue -> Ptr () -> IO ()
readAttributeAcceptedCommandListWithAttributeCache_endpoint_queue_completionHandler attributeCacheContainer endpoint queue completionHandler =
  do
    cls' <- getRequiredClass "MTRBaseClusterChannel"
    sendClassMessage cls' readAttributeAcceptedCommandListWithAttributeCache_endpoint_queue_completionHandlerSelector (toMTRAttributeCacheContainer attributeCacheContainer) (toNSNumber endpoint) (toNSObject queue) completionHandler

-- | @- readAttributeAttributeListWithCompletionHandler:@
readAttributeAttributeListWithCompletionHandler :: IsMTRBaseClusterChannel mtrBaseClusterChannel => mtrBaseClusterChannel -> Ptr () -> IO ()
readAttributeAttributeListWithCompletionHandler mtrBaseClusterChannel completionHandler =
  sendMessage mtrBaseClusterChannel readAttributeAttributeListWithCompletionHandlerSelector completionHandler

-- | @- subscribeAttributeAttributeListWithMinInterval:maxInterval:params:subscriptionEstablished:reportHandler:@
subscribeAttributeAttributeListWithMinInterval_maxInterval_params_subscriptionEstablished_reportHandler :: (IsMTRBaseClusterChannel mtrBaseClusterChannel, IsNSNumber minInterval, IsNSNumber maxInterval, IsMTRSubscribeParams params) => mtrBaseClusterChannel -> minInterval -> maxInterval -> params -> Ptr () -> Ptr () -> IO ()
subscribeAttributeAttributeListWithMinInterval_maxInterval_params_subscriptionEstablished_reportHandler mtrBaseClusterChannel minInterval maxInterval params subscriptionEstablishedHandler reportHandler =
  sendMessage mtrBaseClusterChannel subscribeAttributeAttributeListWithMinInterval_maxInterval_params_subscriptionEstablished_reportHandlerSelector (toNSNumber minInterval) (toNSNumber maxInterval) (toMTRSubscribeParams params) subscriptionEstablishedHandler reportHandler

-- | @+ readAttributeAttributeListWithAttributeCache:endpoint:queue:completionHandler:@
readAttributeAttributeListWithAttributeCache_endpoint_queue_completionHandler :: (IsMTRAttributeCacheContainer attributeCacheContainer, IsNSNumber endpoint, IsNSObject queue) => attributeCacheContainer -> endpoint -> queue -> Ptr () -> IO ()
readAttributeAttributeListWithAttributeCache_endpoint_queue_completionHandler attributeCacheContainer endpoint queue completionHandler =
  do
    cls' <- getRequiredClass "MTRBaseClusterChannel"
    sendClassMessage cls' readAttributeAttributeListWithAttributeCache_endpoint_queue_completionHandlerSelector (toMTRAttributeCacheContainer attributeCacheContainer) (toNSNumber endpoint) (toNSObject queue) completionHandler

-- | @- readAttributeFeatureMapWithCompletionHandler:@
readAttributeFeatureMapWithCompletionHandler :: IsMTRBaseClusterChannel mtrBaseClusterChannel => mtrBaseClusterChannel -> Ptr () -> IO ()
readAttributeFeatureMapWithCompletionHandler mtrBaseClusterChannel completionHandler =
  sendMessage mtrBaseClusterChannel readAttributeFeatureMapWithCompletionHandlerSelector completionHandler

-- | @- subscribeAttributeFeatureMapWithMinInterval:maxInterval:params:subscriptionEstablished:reportHandler:@
subscribeAttributeFeatureMapWithMinInterval_maxInterval_params_subscriptionEstablished_reportHandler :: (IsMTRBaseClusterChannel mtrBaseClusterChannel, IsNSNumber minInterval, IsNSNumber maxInterval, IsMTRSubscribeParams params) => mtrBaseClusterChannel -> minInterval -> maxInterval -> params -> Ptr () -> Ptr () -> IO ()
subscribeAttributeFeatureMapWithMinInterval_maxInterval_params_subscriptionEstablished_reportHandler mtrBaseClusterChannel minInterval maxInterval params subscriptionEstablishedHandler reportHandler =
  sendMessage mtrBaseClusterChannel subscribeAttributeFeatureMapWithMinInterval_maxInterval_params_subscriptionEstablished_reportHandlerSelector (toNSNumber minInterval) (toNSNumber maxInterval) (toMTRSubscribeParams params) subscriptionEstablishedHandler reportHandler

-- | @+ readAttributeFeatureMapWithAttributeCache:endpoint:queue:completionHandler:@
readAttributeFeatureMapWithAttributeCache_endpoint_queue_completionHandler :: (IsMTRAttributeCacheContainer attributeCacheContainer, IsNSNumber endpoint, IsNSObject queue) => attributeCacheContainer -> endpoint -> queue -> Ptr () -> IO ()
readAttributeFeatureMapWithAttributeCache_endpoint_queue_completionHandler attributeCacheContainer endpoint queue completionHandler =
  do
    cls' <- getRequiredClass "MTRBaseClusterChannel"
    sendClassMessage cls' readAttributeFeatureMapWithAttributeCache_endpoint_queue_completionHandlerSelector (toMTRAttributeCacheContainer attributeCacheContainer) (toNSNumber endpoint) (toNSObject queue) completionHandler

-- | @- readAttributeClusterRevisionWithCompletionHandler:@
readAttributeClusterRevisionWithCompletionHandler :: IsMTRBaseClusterChannel mtrBaseClusterChannel => mtrBaseClusterChannel -> Ptr () -> IO ()
readAttributeClusterRevisionWithCompletionHandler mtrBaseClusterChannel completionHandler =
  sendMessage mtrBaseClusterChannel readAttributeClusterRevisionWithCompletionHandlerSelector completionHandler

-- | @- subscribeAttributeClusterRevisionWithMinInterval:maxInterval:params:subscriptionEstablished:reportHandler:@
subscribeAttributeClusterRevisionWithMinInterval_maxInterval_params_subscriptionEstablished_reportHandler :: (IsMTRBaseClusterChannel mtrBaseClusterChannel, IsNSNumber minInterval, IsNSNumber maxInterval, IsMTRSubscribeParams params) => mtrBaseClusterChannel -> minInterval -> maxInterval -> params -> Ptr () -> Ptr () -> IO ()
subscribeAttributeClusterRevisionWithMinInterval_maxInterval_params_subscriptionEstablished_reportHandler mtrBaseClusterChannel minInterval maxInterval params subscriptionEstablishedHandler reportHandler =
  sendMessage mtrBaseClusterChannel subscribeAttributeClusterRevisionWithMinInterval_maxInterval_params_subscriptionEstablished_reportHandlerSelector (toNSNumber minInterval) (toNSNumber maxInterval) (toMTRSubscribeParams params) subscriptionEstablishedHandler reportHandler

-- | @+ readAttributeClusterRevisionWithAttributeCache:endpoint:queue:completionHandler:@
readAttributeClusterRevisionWithAttributeCache_endpoint_queue_completionHandler :: (IsMTRAttributeCacheContainer attributeCacheContainer, IsNSNumber endpoint, IsNSObject queue) => attributeCacheContainer -> endpoint -> queue -> Ptr () -> IO ()
readAttributeClusterRevisionWithAttributeCache_endpoint_queue_completionHandler attributeCacheContainer endpoint queue completionHandler =
  do
    cls' <- getRequiredClass "MTRBaseClusterChannel"
    sendClassMessage cls' readAttributeClusterRevisionWithAttributeCache_endpoint_queue_completionHandlerSelector (toMTRAttributeCacheContainer attributeCacheContainer) (toNSNumber endpoint) (toNSObject queue) completionHandler

-- | For all instance methods (reads, writes, commands) that take a completion, the completion will be called on the provided queue.
--
-- ObjC selector: @- initWithDevice:endpointID:queue:@
initWithDevice_endpointID_queue :: (IsMTRBaseClusterChannel mtrBaseClusterChannel, IsMTRBaseDevice device, IsNSNumber endpointID, IsNSObject queue) => mtrBaseClusterChannel -> device -> endpointID -> queue -> IO (Id MTRBaseClusterChannel)
initWithDevice_endpointID_queue mtrBaseClusterChannel device endpointID queue =
  sendOwnedMessage mtrBaseClusterChannel initWithDevice_endpointID_queueSelector (toMTRBaseDevice device) (toNSNumber endpointID) (toNSObject queue)

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @changeChannelWithParams:completion:@
changeChannelWithParams_completionSelector :: Selector '[Id MTRChannelClusterChangeChannelParams, Ptr ()] ()
changeChannelWithParams_completionSelector = mkSelector "changeChannelWithParams:completion:"

-- | @Selector@ for @changeChannelByNumberWithParams:completion:@
changeChannelByNumberWithParams_completionSelector :: Selector '[Id MTRChannelClusterChangeChannelByNumberParams, Ptr ()] ()
changeChannelByNumberWithParams_completionSelector = mkSelector "changeChannelByNumberWithParams:completion:"

-- | @Selector@ for @skipChannelWithParams:completion:@
skipChannelWithParams_completionSelector :: Selector '[Id MTRChannelClusterSkipChannelParams, Ptr ()] ()
skipChannelWithParams_completionSelector = mkSelector "skipChannelWithParams:completion:"

-- | @Selector@ for @getProgramGuideWithParams:completion:@
getProgramGuideWithParams_completionSelector :: Selector '[Id MTRChannelClusterGetProgramGuideParams, Ptr ()] ()
getProgramGuideWithParams_completionSelector = mkSelector "getProgramGuideWithParams:completion:"

-- | @Selector@ for @getProgramGuideWithCompletion:@
getProgramGuideWithCompletionSelector :: Selector '[Ptr ()] ()
getProgramGuideWithCompletionSelector = mkSelector "getProgramGuideWithCompletion:"

-- | @Selector@ for @recordProgramWithParams:completion:@
recordProgramWithParams_completionSelector :: Selector '[Id MTRChannelClusterRecordProgramParams, Ptr ()] ()
recordProgramWithParams_completionSelector = mkSelector "recordProgramWithParams:completion:"

-- | @Selector@ for @cancelRecordProgramWithParams:completion:@
cancelRecordProgramWithParams_completionSelector :: Selector '[Id MTRChannelClusterCancelRecordProgramParams, Ptr ()] ()
cancelRecordProgramWithParams_completionSelector = mkSelector "cancelRecordProgramWithParams:completion:"

-- | @Selector@ for @readAttributeChannelListWithCompletion:@
readAttributeChannelListWithCompletionSelector :: Selector '[Ptr ()] ()
readAttributeChannelListWithCompletionSelector = mkSelector "readAttributeChannelListWithCompletion:"

-- | @Selector@ for @subscribeAttributeChannelListWithParams:subscriptionEstablished:reportHandler:@
subscribeAttributeChannelListWithParams_subscriptionEstablished_reportHandlerSelector :: Selector '[Id MTRSubscribeParams, Ptr (), Ptr ()] ()
subscribeAttributeChannelListWithParams_subscriptionEstablished_reportHandlerSelector = mkSelector "subscribeAttributeChannelListWithParams:subscriptionEstablished:reportHandler:"

-- | @Selector@ for @readAttributeChannelListWithClusterStateCache:endpoint:queue:completion:@
readAttributeChannelListWithClusterStateCache_endpoint_queue_completionSelector :: Selector '[Id MTRClusterStateCacheContainer, Id NSNumber, Id NSObject, Ptr ()] ()
readAttributeChannelListWithClusterStateCache_endpoint_queue_completionSelector = mkSelector "readAttributeChannelListWithClusterStateCache:endpoint:queue:completion:"

-- | @Selector@ for @readAttributeLineupWithCompletion:@
readAttributeLineupWithCompletionSelector :: Selector '[Ptr ()] ()
readAttributeLineupWithCompletionSelector = mkSelector "readAttributeLineupWithCompletion:"

-- | @Selector@ for @subscribeAttributeLineupWithParams:subscriptionEstablished:reportHandler:@
subscribeAttributeLineupWithParams_subscriptionEstablished_reportHandlerSelector :: Selector '[Id MTRSubscribeParams, Ptr (), Ptr ()] ()
subscribeAttributeLineupWithParams_subscriptionEstablished_reportHandlerSelector = mkSelector "subscribeAttributeLineupWithParams:subscriptionEstablished:reportHandler:"

-- | @Selector@ for @readAttributeLineupWithClusterStateCache:endpoint:queue:completion:@
readAttributeLineupWithClusterStateCache_endpoint_queue_completionSelector :: Selector '[Id MTRClusterStateCacheContainer, Id NSNumber, Id NSObject, Ptr ()] ()
readAttributeLineupWithClusterStateCache_endpoint_queue_completionSelector = mkSelector "readAttributeLineupWithClusterStateCache:endpoint:queue:completion:"

-- | @Selector@ for @readAttributeCurrentChannelWithCompletion:@
readAttributeCurrentChannelWithCompletionSelector :: Selector '[Ptr ()] ()
readAttributeCurrentChannelWithCompletionSelector = mkSelector "readAttributeCurrentChannelWithCompletion:"

-- | @Selector@ for @subscribeAttributeCurrentChannelWithParams:subscriptionEstablished:reportHandler:@
subscribeAttributeCurrentChannelWithParams_subscriptionEstablished_reportHandlerSelector :: Selector '[Id MTRSubscribeParams, Ptr (), Ptr ()] ()
subscribeAttributeCurrentChannelWithParams_subscriptionEstablished_reportHandlerSelector = mkSelector "subscribeAttributeCurrentChannelWithParams:subscriptionEstablished:reportHandler:"

-- | @Selector@ for @readAttributeCurrentChannelWithClusterStateCache:endpoint:queue:completion:@
readAttributeCurrentChannelWithClusterStateCache_endpoint_queue_completionSelector :: Selector '[Id MTRClusterStateCacheContainer, Id NSNumber, Id NSObject, Ptr ()] ()
readAttributeCurrentChannelWithClusterStateCache_endpoint_queue_completionSelector = mkSelector "readAttributeCurrentChannelWithClusterStateCache:endpoint:queue:completion:"

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
initSelector :: Selector '[] (Id MTRBaseClusterChannel)
initSelector = mkSelector "init"

-- | @Selector@ for @new@
newSelector :: Selector '[] (Id MTRBaseClusterChannel)
newSelector = mkSelector "new"

-- | @Selector@ for @initWithDevice:endpoint:queue:@
initWithDevice_endpoint_queueSelector :: Selector '[Id MTRBaseDevice, CUShort, Id NSObject] (Id MTRBaseClusterChannel)
initWithDevice_endpoint_queueSelector = mkSelector "initWithDevice:endpoint:queue:"

-- | @Selector@ for @changeChannelWithParams:completionHandler:@
changeChannelWithParams_completionHandlerSelector :: Selector '[Id MTRChannelClusterChangeChannelParams, Ptr ()] ()
changeChannelWithParams_completionHandlerSelector = mkSelector "changeChannelWithParams:completionHandler:"

-- | @Selector@ for @changeChannelByNumberWithParams:completionHandler:@
changeChannelByNumberWithParams_completionHandlerSelector :: Selector '[Id MTRChannelClusterChangeChannelByNumberParams, Ptr ()] ()
changeChannelByNumberWithParams_completionHandlerSelector = mkSelector "changeChannelByNumberWithParams:completionHandler:"

-- | @Selector@ for @skipChannelWithParams:completionHandler:@
skipChannelWithParams_completionHandlerSelector :: Selector '[Id MTRChannelClusterSkipChannelParams, Ptr ()] ()
skipChannelWithParams_completionHandlerSelector = mkSelector "skipChannelWithParams:completionHandler:"

-- | @Selector@ for @readAttributeChannelListWithCompletionHandler:@
readAttributeChannelListWithCompletionHandlerSelector :: Selector '[Ptr ()] ()
readAttributeChannelListWithCompletionHandlerSelector = mkSelector "readAttributeChannelListWithCompletionHandler:"

-- | @Selector@ for @subscribeAttributeChannelListWithMinInterval:maxInterval:params:subscriptionEstablished:reportHandler:@
subscribeAttributeChannelListWithMinInterval_maxInterval_params_subscriptionEstablished_reportHandlerSelector :: Selector '[Id NSNumber, Id NSNumber, Id MTRSubscribeParams, Ptr (), Ptr ()] ()
subscribeAttributeChannelListWithMinInterval_maxInterval_params_subscriptionEstablished_reportHandlerSelector = mkSelector "subscribeAttributeChannelListWithMinInterval:maxInterval:params:subscriptionEstablished:reportHandler:"

-- | @Selector@ for @readAttributeChannelListWithAttributeCache:endpoint:queue:completionHandler:@
readAttributeChannelListWithAttributeCache_endpoint_queue_completionHandlerSelector :: Selector '[Id MTRAttributeCacheContainer, Id NSNumber, Id NSObject, Ptr ()] ()
readAttributeChannelListWithAttributeCache_endpoint_queue_completionHandlerSelector = mkSelector "readAttributeChannelListWithAttributeCache:endpoint:queue:completionHandler:"

-- | @Selector@ for @readAttributeLineupWithCompletionHandler:@
readAttributeLineupWithCompletionHandlerSelector :: Selector '[Ptr ()] ()
readAttributeLineupWithCompletionHandlerSelector = mkSelector "readAttributeLineupWithCompletionHandler:"

-- | @Selector@ for @subscribeAttributeLineupWithMinInterval:maxInterval:params:subscriptionEstablished:reportHandler:@
subscribeAttributeLineupWithMinInterval_maxInterval_params_subscriptionEstablished_reportHandlerSelector :: Selector '[Id NSNumber, Id NSNumber, Id MTRSubscribeParams, Ptr (), Ptr ()] ()
subscribeAttributeLineupWithMinInterval_maxInterval_params_subscriptionEstablished_reportHandlerSelector = mkSelector "subscribeAttributeLineupWithMinInterval:maxInterval:params:subscriptionEstablished:reportHandler:"

-- | @Selector@ for @readAttributeLineupWithAttributeCache:endpoint:queue:completionHandler:@
readAttributeLineupWithAttributeCache_endpoint_queue_completionHandlerSelector :: Selector '[Id MTRAttributeCacheContainer, Id NSNumber, Id NSObject, Ptr ()] ()
readAttributeLineupWithAttributeCache_endpoint_queue_completionHandlerSelector = mkSelector "readAttributeLineupWithAttributeCache:endpoint:queue:completionHandler:"

-- | @Selector@ for @readAttributeCurrentChannelWithCompletionHandler:@
readAttributeCurrentChannelWithCompletionHandlerSelector :: Selector '[Ptr ()] ()
readAttributeCurrentChannelWithCompletionHandlerSelector = mkSelector "readAttributeCurrentChannelWithCompletionHandler:"

-- | @Selector@ for @subscribeAttributeCurrentChannelWithMinInterval:maxInterval:params:subscriptionEstablished:reportHandler:@
subscribeAttributeCurrentChannelWithMinInterval_maxInterval_params_subscriptionEstablished_reportHandlerSelector :: Selector '[Id NSNumber, Id NSNumber, Id MTRSubscribeParams, Ptr (), Ptr ()] ()
subscribeAttributeCurrentChannelWithMinInterval_maxInterval_params_subscriptionEstablished_reportHandlerSelector = mkSelector "subscribeAttributeCurrentChannelWithMinInterval:maxInterval:params:subscriptionEstablished:reportHandler:"

-- | @Selector@ for @readAttributeCurrentChannelWithAttributeCache:endpoint:queue:completionHandler:@
readAttributeCurrentChannelWithAttributeCache_endpoint_queue_completionHandlerSelector :: Selector '[Id MTRAttributeCacheContainer, Id NSNumber, Id NSObject, Ptr ()] ()
readAttributeCurrentChannelWithAttributeCache_endpoint_queue_completionHandlerSelector = mkSelector "readAttributeCurrentChannelWithAttributeCache:endpoint:queue:completionHandler:"

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
initWithDevice_endpointID_queueSelector :: Selector '[Id MTRBaseDevice, Id NSNumber, Id NSObject] (Id MTRBaseClusterChannel)
initWithDevice_endpointID_queueSelector = mkSelector "initWithDevice:endpointID:queue:"

