{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Cluster Content Control
--
-- This cluster is used for managing the content control (including "parental control") settings on a media device such as a TV, or Set-top Box.
--
-- Generated bindings for @MTRBaseClusterContentControl@.
module ObjC.Matter.MTRBaseClusterContentControl
  ( MTRBaseClusterContentControl
  , IsMTRBaseClusterContentControl(..)
  , updatePINWithParams_completion
  , resetPINWithParams_completion
  , resetPINWithCompletion
  , enableWithParams_completion
  , enableWithCompletion
  , disableWithParams_completion
  , disableWithCompletion
  , addBonusTimeWithParams_completion
  , addBonusTimeWithCompletion
  , setScreenDailyTimeWithParams_completion
  , blockUnratedContentWithParams_completion
  , blockUnratedContentWithCompletion
  , unblockUnratedContentWithParams_completion
  , unblockUnratedContentWithCompletion
  , setOnDemandRatingThresholdWithParams_completion
  , setScheduledContentRatingThresholdWithParams_completion
  , readAttributeEnabledWithCompletion
  , subscribeAttributeEnabledWithParams_subscriptionEstablished_reportHandler
  , readAttributeEnabledWithClusterStateCache_endpoint_queue_completion
  , readAttributeOnDemandRatingsWithCompletion
  , subscribeAttributeOnDemandRatingsWithParams_subscriptionEstablished_reportHandler
  , readAttributeOnDemandRatingsWithClusterStateCache_endpoint_queue_completion
  , readAttributeOnDemandRatingThresholdWithCompletion
  , subscribeAttributeOnDemandRatingThresholdWithParams_subscriptionEstablished_reportHandler
  , readAttributeOnDemandRatingThresholdWithClusterStateCache_endpoint_queue_completion
  , readAttributeScheduledContentRatingsWithCompletion
  , subscribeAttributeScheduledContentRatingsWithParams_subscriptionEstablished_reportHandler
  , readAttributeScheduledContentRatingsWithClusterStateCache_endpoint_queue_completion
  , readAttributeScheduledContentRatingThresholdWithCompletion
  , subscribeAttributeScheduledContentRatingThresholdWithParams_subscriptionEstablished_reportHandler
  , readAttributeScheduledContentRatingThresholdWithClusterStateCache_endpoint_queue_completion
  , readAttributeScreenDailyTimeWithCompletion
  , subscribeAttributeScreenDailyTimeWithParams_subscriptionEstablished_reportHandler
  , readAttributeScreenDailyTimeWithClusterStateCache_endpoint_queue_completion
  , readAttributeRemainingScreenTimeWithCompletion
  , subscribeAttributeRemainingScreenTimeWithParams_subscriptionEstablished_reportHandler
  , readAttributeRemainingScreenTimeWithClusterStateCache_endpoint_queue_completion
  , readAttributeBlockUnratedWithCompletion
  , subscribeAttributeBlockUnratedWithParams_subscriptionEstablished_reportHandler
  , readAttributeBlockUnratedWithClusterStateCache_endpoint_queue_completion
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
  , addBonusTimeWithCompletionSelector
  , addBonusTimeWithParams_completionSelector
  , blockUnratedContentWithCompletionSelector
  , blockUnratedContentWithParams_completionSelector
  , disableWithCompletionSelector
  , disableWithParams_completionSelector
  , enableWithCompletionSelector
  , enableWithParams_completionSelector
  , initSelector
  , initWithDevice_endpointID_queueSelector
  , newSelector
  , readAttributeAcceptedCommandListWithClusterStateCache_endpoint_queue_completionSelector
  , readAttributeAcceptedCommandListWithCompletionSelector
  , readAttributeAttributeListWithClusterStateCache_endpoint_queue_completionSelector
  , readAttributeAttributeListWithCompletionSelector
  , readAttributeBlockUnratedWithClusterStateCache_endpoint_queue_completionSelector
  , readAttributeBlockUnratedWithCompletionSelector
  , readAttributeClusterRevisionWithClusterStateCache_endpoint_queue_completionSelector
  , readAttributeClusterRevisionWithCompletionSelector
  , readAttributeEnabledWithClusterStateCache_endpoint_queue_completionSelector
  , readAttributeEnabledWithCompletionSelector
  , readAttributeFeatureMapWithClusterStateCache_endpoint_queue_completionSelector
  , readAttributeFeatureMapWithCompletionSelector
  , readAttributeGeneratedCommandListWithClusterStateCache_endpoint_queue_completionSelector
  , readAttributeGeneratedCommandListWithCompletionSelector
  , readAttributeOnDemandRatingThresholdWithClusterStateCache_endpoint_queue_completionSelector
  , readAttributeOnDemandRatingThresholdWithCompletionSelector
  , readAttributeOnDemandRatingsWithClusterStateCache_endpoint_queue_completionSelector
  , readAttributeOnDemandRatingsWithCompletionSelector
  , readAttributeRemainingScreenTimeWithClusterStateCache_endpoint_queue_completionSelector
  , readAttributeRemainingScreenTimeWithCompletionSelector
  , readAttributeScheduledContentRatingThresholdWithClusterStateCache_endpoint_queue_completionSelector
  , readAttributeScheduledContentRatingThresholdWithCompletionSelector
  , readAttributeScheduledContentRatingsWithClusterStateCache_endpoint_queue_completionSelector
  , readAttributeScheduledContentRatingsWithCompletionSelector
  , readAttributeScreenDailyTimeWithClusterStateCache_endpoint_queue_completionSelector
  , readAttributeScreenDailyTimeWithCompletionSelector
  , resetPINWithCompletionSelector
  , resetPINWithParams_completionSelector
  , setOnDemandRatingThresholdWithParams_completionSelector
  , setScheduledContentRatingThresholdWithParams_completionSelector
  , setScreenDailyTimeWithParams_completionSelector
  , subscribeAttributeAcceptedCommandListWithParams_subscriptionEstablished_reportHandlerSelector
  , subscribeAttributeAttributeListWithParams_subscriptionEstablished_reportHandlerSelector
  , subscribeAttributeBlockUnratedWithParams_subscriptionEstablished_reportHandlerSelector
  , subscribeAttributeClusterRevisionWithParams_subscriptionEstablished_reportHandlerSelector
  , subscribeAttributeEnabledWithParams_subscriptionEstablished_reportHandlerSelector
  , subscribeAttributeFeatureMapWithParams_subscriptionEstablished_reportHandlerSelector
  , subscribeAttributeGeneratedCommandListWithParams_subscriptionEstablished_reportHandlerSelector
  , subscribeAttributeOnDemandRatingThresholdWithParams_subscriptionEstablished_reportHandlerSelector
  , subscribeAttributeOnDemandRatingsWithParams_subscriptionEstablished_reportHandlerSelector
  , subscribeAttributeRemainingScreenTimeWithParams_subscriptionEstablished_reportHandlerSelector
  , subscribeAttributeScheduledContentRatingThresholdWithParams_subscriptionEstablished_reportHandlerSelector
  , subscribeAttributeScheduledContentRatingsWithParams_subscriptionEstablished_reportHandlerSelector
  , subscribeAttributeScreenDailyTimeWithParams_subscriptionEstablished_reportHandlerSelector
  , unblockUnratedContentWithCompletionSelector
  , unblockUnratedContentWithParams_completionSelector
  , updatePINWithParams_completionSelector


  ) where

import Foreign.Ptr (Ptr, FunPtr)
import Foreign.C.Types

import ObjC.Runtime.Types
import ObjC.Runtime.Message (sendMessage, sendOwnedMessage, sendClassMessage, sendOwnedClassMessage)
import ObjC.Runtime.Selector (mkSelector)
import ObjC.Runtime.Class (getRequiredClass)

import ObjC.Matter.Internal.Classes
import ObjC.Foundation.Internal.Classes

-- | Command UpdatePIN
--
-- The purpose of this command is to update the PIN used for protecting configuration of the content control settings. Upon success, the old PIN SHALL no longer work. The PIN is used to ensure that only the Node (or User) with the PIN code can make changes to the Content Control settings, for example, turn off Content Controls or modify the ScreenDailyTime. The PIN is composed of a numeric string of up to 6 human readable characters (displayable) . Upon receipt of this command, the media device SHALL check if the OldPIN field of this command is the same as the current PIN. If the PINs are the same, then the PIN code SHALL be set to NewPIN. Otherwise a response with InvalidPINCode error status SHALL be returned. The media device MAY provide a default PIN to the User via an out of band mechanism. For security reasons, it is recommended that a client encourage the user to update the PIN from its default value when performing configuration of the Content Control settings exposed by this cluster. The ResetPIN command can also be used to obtain the default PIN.
--
-- ObjC selector: @- updatePINWithParams:completion:@
updatePINWithParams_completion :: (IsMTRBaseClusterContentControl mtrBaseClusterContentControl, IsMTRContentControlClusterUpdatePINParams params) => mtrBaseClusterContentControl -> params -> Ptr () -> IO ()
updatePINWithParams_completion mtrBaseClusterContentControl params completion =
  sendMessage mtrBaseClusterContentControl updatePINWithParams_completionSelector (toMTRContentControlClusterUpdatePINParams params) completion

-- | Command ResetPIN
--
-- The purpose of this command is to reset the PIN. If this command is executed successfully, a ResetPINResponse command with a new PIN SHALL be returned.
--
-- ObjC selector: @- resetPINWithParams:completion:@
resetPINWithParams_completion :: (IsMTRBaseClusterContentControl mtrBaseClusterContentControl, IsMTRContentControlClusterResetPINParams params) => mtrBaseClusterContentControl -> params -> Ptr () -> IO ()
resetPINWithParams_completion mtrBaseClusterContentControl params completion =
  sendMessage mtrBaseClusterContentControl resetPINWithParams_completionSelector (toMTRContentControlClusterResetPINParams params) completion

-- | @- resetPINWithCompletion:@
resetPINWithCompletion :: IsMTRBaseClusterContentControl mtrBaseClusterContentControl => mtrBaseClusterContentControl -> Ptr () -> IO ()
resetPINWithCompletion mtrBaseClusterContentControl completion =
  sendMessage mtrBaseClusterContentControl resetPINWithCompletionSelector completion

-- | Command Enable
--
-- The purpose of this command is to turn on the Content Control feature on a media device. On receipt of the Enable command, the media device SHALL set the Enabled attribute to TRUE.
--
-- ObjC selector: @- enableWithParams:completion:@
enableWithParams_completion :: (IsMTRBaseClusterContentControl mtrBaseClusterContentControl, IsMTRContentControlClusterEnableParams params) => mtrBaseClusterContentControl -> params -> Ptr () -> IO ()
enableWithParams_completion mtrBaseClusterContentControl params completion =
  sendMessage mtrBaseClusterContentControl enableWithParams_completionSelector (toMTRContentControlClusterEnableParams params) completion

-- | @- enableWithCompletion:@
enableWithCompletion :: IsMTRBaseClusterContentControl mtrBaseClusterContentControl => mtrBaseClusterContentControl -> Ptr () -> IO ()
enableWithCompletion mtrBaseClusterContentControl completion =
  sendMessage mtrBaseClusterContentControl enableWithCompletionSelector completion

-- | Command Disable
--
-- The purpose of this command is to turn off the Content Control feature on a media device. On receipt of the Disable command, the media device SHALL set the Enabled attribute to FALSE.
--
-- ObjC selector: @- disableWithParams:completion:@
disableWithParams_completion :: (IsMTRBaseClusterContentControl mtrBaseClusterContentControl, IsMTRContentControlClusterDisableParams params) => mtrBaseClusterContentControl -> params -> Ptr () -> IO ()
disableWithParams_completion mtrBaseClusterContentControl params completion =
  sendMessage mtrBaseClusterContentControl disableWithParams_completionSelector (toMTRContentControlClusterDisableParams params) completion

-- | @- disableWithCompletion:@
disableWithCompletion :: IsMTRBaseClusterContentControl mtrBaseClusterContentControl => mtrBaseClusterContentControl -> Ptr () -> IO ()
disableWithCompletion mtrBaseClusterContentControl completion =
  sendMessage mtrBaseClusterContentControl disableWithCompletionSelector completion

-- | Command AddBonusTime
--
-- The purpose of this command is to add the extra screen time for the user. If a client with Operate privilege invokes this command, the media device SHALL check whether the PINCode passed in the command matches the current PINCode value. If these match, then the RemainingScreenTime attribute SHALL be increased by the specified BonusTime value. If the PINs do not match, then a response with InvalidPINCode error status SHALL be returned, and no changes SHALL be made to RemainingScreenTime. If a client with Manage privilege or greater invokes this command, the media device SHALL ignore the PINCode field and directly increase the RemainingScreenTime attribute by the specified BonusTime value. A server that does not support the PM feature SHALL respond with InvalidPINCode to clients that only have Operate privilege unless: It has been provided with the PIN value to expect via an out of band mechanism, and The client has provided a PINCode that matches the expected PIN value.
--
-- ObjC selector: @- addBonusTimeWithParams:completion:@
addBonusTimeWithParams_completion :: (IsMTRBaseClusterContentControl mtrBaseClusterContentControl, IsMTRContentControlClusterAddBonusTimeParams params) => mtrBaseClusterContentControl -> params -> Ptr () -> IO ()
addBonusTimeWithParams_completion mtrBaseClusterContentControl params completion =
  sendMessage mtrBaseClusterContentControl addBonusTimeWithParams_completionSelector (toMTRContentControlClusterAddBonusTimeParams params) completion

-- | @- addBonusTimeWithCompletion:@
addBonusTimeWithCompletion :: IsMTRBaseClusterContentControl mtrBaseClusterContentControl => mtrBaseClusterContentControl -> Ptr () -> IO ()
addBonusTimeWithCompletion mtrBaseClusterContentControl completion =
  sendMessage mtrBaseClusterContentControl addBonusTimeWithCompletionSelector completion

-- | Command SetScreenDailyTime
--
-- The purpose of this command is to set the ScreenDailyTime attribute. On receipt of the SetScreenDailyTime command, the media device SHALL set the ScreenDailyTime attribute to the ScreenTime value.
--
-- ObjC selector: @- setScreenDailyTimeWithParams:completion:@
setScreenDailyTimeWithParams_completion :: (IsMTRBaseClusterContentControl mtrBaseClusterContentControl, IsMTRContentControlClusterSetScreenDailyTimeParams params) => mtrBaseClusterContentControl -> params -> Ptr () -> IO ()
setScreenDailyTimeWithParams_completion mtrBaseClusterContentControl params completion =
  sendMessage mtrBaseClusterContentControl setScreenDailyTimeWithParams_completionSelector (toMTRContentControlClusterSetScreenDailyTimeParams params) completion

-- | Command BlockUnratedContent
--
-- The purpose of this command is to specify whether programs with no Content rating must be blocked by this media device. On receipt of the BlockUnratedContent command, the media device SHALL set the BlockUnrated attribute to TRUE.
--
-- ObjC selector: @- blockUnratedContentWithParams:completion:@
blockUnratedContentWithParams_completion :: (IsMTRBaseClusterContentControl mtrBaseClusterContentControl, IsMTRContentControlClusterBlockUnratedContentParams params) => mtrBaseClusterContentControl -> params -> Ptr () -> IO ()
blockUnratedContentWithParams_completion mtrBaseClusterContentControl params completion =
  sendMessage mtrBaseClusterContentControl blockUnratedContentWithParams_completionSelector (toMTRContentControlClusterBlockUnratedContentParams params) completion

-- | @- blockUnratedContentWithCompletion:@
blockUnratedContentWithCompletion :: IsMTRBaseClusterContentControl mtrBaseClusterContentControl => mtrBaseClusterContentControl -> Ptr () -> IO ()
blockUnratedContentWithCompletion mtrBaseClusterContentControl completion =
  sendMessage mtrBaseClusterContentControl blockUnratedContentWithCompletionSelector completion

-- | Command UnblockUnratedContent
--
-- The purpose of this command is to specify whether programs with no Content rating must be blocked by this media device. On receipt of the UnblockUnratedContent command, the media device SHALL set the BlockUnrated attribute to FALSE.
--
-- ObjC selector: @- unblockUnratedContentWithParams:completion:@
unblockUnratedContentWithParams_completion :: (IsMTRBaseClusterContentControl mtrBaseClusterContentControl, IsMTRContentControlClusterUnblockUnratedContentParams params) => mtrBaseClusterContentControl -> params -> Ptr () -> IO ()
unblockUnratedContentWithParams_completion mtrBaseClusterContentControl params completion =
  sendMessage mtrBaseClusterContentControl unblockUnratedContentWithParams_completionSelector (toMTRContentControlClusterUnblockUnratedContentParams params) completion

-- | @- unblockUnratedContentWithCompletion:@
unblockUnratedContentWithCompletion :: IsMTRBaseClusterContentControl mtrBaseClusterContentControl => mtrBaseClusterContentControl -> Ptr () -> IO ()
unblockUnratedContentWithCompletion mtrBaseClusterContentControl completion =
  sendMessage mtrBaseClusterContentControl unblockUnratedContentWithCompletionSelector completion

-- | Command SetOnDemandRatingThreshold
--
-- The purpose of this command is to set the OnDemandRatingThreshold attribute. On receipt of the SetOnDemandRatingThreshold command, the media device SHALL check if the Rating field is one of values present in the OnDemandRatings attribute. If not, then a response with InvalidRating error status SHALL be returned.
--
-- ObjC selector: @- setOnDemandRatingThresholdWithParams:completion:@
setOnDemandRatingThresholdWithParams_completion :: (IsMTRBaseClusterContentControl mtrBaseClusterContentControl, IsMTRContentControlClusterSetOnDemandRatingThresholdParams params) => mtrBaseClusterContentControl -> params -> Ptr () -> IO ()
setOnDemandRatingThresholdWithParams_completion mtrBaseClusterContentControl params completion =
  sendMessage mtrBaseClusterContentControl setOnDemandRatingThresholdWithParams_completionSelector (toMTRContentControlClusterSetOnDemandRatingThresholdParams params) completion

-- | Command SetScheduledContentRatingThreshold
--
-- The purpose of this command is to set ScheduledContentRatingThreshold attribute. On receipt of the SetScheduledContentRatingThreshold command, the media device SHALL check if the Rating field is one of values present in the ScheduledContentRatings attribute. If not, then a response with InvalidRating error status SHALL be returned.
--
-- ObjC selector: @- setScheduledContentRatingThresholdWithParams:completion:@
setScheduledContentRatingThresholdWithParams_completion :: (IsMTRBaseClusterContentControl mtrBaseClusterContentControl, IsMTRContentControlClusterSetScheduledContentRatingThresholdParams params) => mtrBaseClusterContentControl -> params -> Ptr () -> IO ()
setScheduledContentRatingThresholdWithParams_completion mtrBaseClusterContentControl params completion =
  sendMessage mtrBaseClusterContentControl setScheduledContentRatingThresholdWithParams_completionSelector (toMTRContentControlClusterSetScheduledContentRatingThresholdParams params) completion

-- | @- readAttributeEnabledWithCompletion:@
readAttributeEnabledWithCompletion :: IsMTRBaseClusterContentControl mtrBaseClusterContentControl => mtrBaseClusterContentControl -> Ptr () -> IO ()
readAttributeEnabledWithCompletion mtrBaseClusterContentControl completion =
  sendMessage mtrBaseClusterContentControl readAttributeEnabledWithCompletionSelector completion

-- | @- subscribeAttributeEnabledWithParams:subscriptionEstablished:reportHandler:@
subscribeAttributeEnabledWithParams_subscriptionEstablished_reportHandler :: (IsMTRBaseClusterContentControl mtrBaseClusterContentControl, IsMTRSubscribeParams params) => mtrBaseClusterContentControl -> params -> Ptr () -> Ptr () -> IO ()
subscribeAttributeEnabledWithParams_subscriptionEstablished_reportHandler mtrBaseClusterContentControl params subscriptionEstablished reportHandler =
  sendMessage mtrBaseClusterContentControl subscribeAttributeEnabledWithParams_subscriptionEstablished_reportHandlerSelector (toMTRSubscribeParams params) subscriptionEstablished reportHandler

-- | @+ readAttributeEnabledWithClusterStateCache:endpoint:queue:completion:@
readAttributeEnabledWithClusterStateCache_endpoint_queue_completion :: (IsMTRClusterStateCacheContainer clusterStateCacheContainer, IsNSNumber endpoint, IsNSObject queue) => clusterStateCacheContainer -> endpoint -> queue -> Ptr () -> IO ()
readAttributeEnabledWithClusterStateCache_endpoint_queue_completion clusterStateCacheContainer endpoint queue completion =
  do
    cls' <- getRequiredClass "MTRBaseClusterContentControl"
    sendClassMessage cls' readAttributeEnabledWithClusterStateCache_endpoint_queue_completionSelector (toMTRClusterStateCacheContainer clusterStateCacheContainer) (toNSNumber endpoint) (toNSObject queue) completion

-- | @- readAttributeOnDemandRatingsWithCompletion:@
readAttributeOnDemandRatingsWithCompletion :: IsMTRBaseClusterContentControl mtrBaseClusterContentControl => mtrBaseClusterContentControl -> Ptr () -> IO ()
readAttributeOnDemandRatingsWithCompletion mtrBaseClusterContentControl completion =
  sendMessage mtrBaseClusterContentControl readAttributeOnDemandRatingsWithCompletionSelector completion

-- | @- subscribeAttributeOnDemandRatingsWithParams:subscriptionEstablished:reportHandler:@
subscribeAttributeOnDemandRatingsWithParams_subscriptionEstablished_reportHandler :: (IsMTRBaseClusterContentControl mtrBaseClusterContentControl, IsMTRSubscribeParams params) => mtrBaseClusterContentControl -> params -> Ptr () -> Ptr () -> IO ()
subscribeAttributeOnDemandRatingsWithParams_subscriptionEstablished_reportHandler mtrBaseClusterContentControl params subscriptionEstablished reportHandler =
  sendMessage mtrBaseClusterContentControl subscribeAttributeOnDemandRatingsWithParams_subscriptionEstablished_reportHandlerSelector (toMTRSubscribeParams params) subscriptionEstablished reportHandler

-- | @+ readAttributeOnDemandRatingsWithClusterStateCache:endpoint:queue:completion:@
readAttributeOnDemandRatingsWithClusterStateCache_endpoint_queue_completion :: (IsMTRClusterStateCacheContainer clusterStateCacheContainer, IsNSNumber endpoint, IsNSObject queue) => clusterStateCacheContainer -> endpoint -> queue -> Ptr () -> IO ()
readAttributeOnDemandRatingsWithClusterStateCache_endpoint_queue_completion clusterStateCacheContainer endpoint queue completion =
  do
    cls' <- getRequiredClass "MTRBaseClusterContentControl"
    sendClassMessage cls' readAttributeOnDemandRatingsWithClusterStateCache_endpoint_queue_completionSelector (toMTRClusterStateCacheContainer clusterStateCacheContainer) (toNSNumber endpoint) (toNSObject queue) completion

-- | @- readAttributeOnDemandRatingThresholdWithCompletion:@
readAttributeOnDemandRatingThresholdWithCompletion :: IsMTRBaseClusterContentControl mtrBaseClusterContentControl => mtrBaseClusterContentControl -> Ptr () -> IO ()
readAttributeOnDemandRatingThresholdWithCompletion mtrBaseClusterContentControl completion =
  sendMessage mtrBaseClusterContentControl readAttributeOnDemandRatingThresholdWithCompletionSelector completion

-- | @- subscribeAttributeOnDemandRatingThresholdWithParams:subscriptionEstablished:reportHandler:@
subscribeAttributeOnDemandRatingThresholdWithParams_subscriptionEstablished_reportHandler :: (IsMTRBaseClusterContentControl mtrBaseClusterContentControl, IsMTRSubscribeParams params) => mtrBaseClusterContentControl -> params -> Ptr () -> Ptr () -> IO ()
subscribeAttributeOnDemandRatingThresholdWithParams_subscriptionEstablished_reportHandler mtrBaseClusterContentControl params subscriptionEstablished reportHandler =
  sendMessage mtrBaseClusterContentControl subscribeAttributeOnDemandRatingThresholdWithParams_subscriptionEstablished_reportHandlerSelector (toMTRSubscribeParams params) subscriptionEstablished reportHandler

-- | @+ readAttributeOnDemandRatingThresholdWithClusterStateCache:endpoint:queue:completion:@
readAttributeOnDemandRatingThresholdWithClusterStateCache_endpoint_queue_completion :: (IsMTRClusterStateCacheContainer clusterStateCacheContainer, IsNSNumber endpoint, IsNSObject queue) => clusterStateCacheContainer -> endpoint -> queue -> Ptr () -> IO ()
readAttributeOnDemandRatingThresholdWithClusterStateCache_endpoint_queue_completion clusterStateCacheContainer endpoint queue completion =
  do
    cls' <- getRequiredClass "MTRBaseClusterContentControl"
    sendClassMessage cls' readAttributeOnDemandRatingThresholdWithClusterStateCache_endpoint_queue_completionSelector (toMTRClusterStateCacheContainer clusterStateCacheContainer) (toNSNumber endpoint) (toNSObject queue) completion

-- | @- readAttributeScheduledContentRatingsWithCompletion:@
readAttributeScheduledContentRatingsWithCompletion :: IsMTRBaseClusterContentControl mtrBaseClusterContentControl => mtrBaseClusterContentControl -> Ptr () -> IO ()
readAttributeScheduledContentRatingsWithCompletion mtrBaseClusterContentControl completion =
  sendMessage mtrBaseClusterContentControl readAttributeScheduledContentRatingsWithCompletionSelector completion

-- | @- subscribeAttributeScheduledContentRatingsWithParams:subscriptionEstablished:reportHandler:@
subscribeAttributeScheduledContentRatingsWithParams_subscriptionEstablished_reportHandler :: (IsMTRBaseClusterContentControl mtrBaseClusterContentControl, IsMTRSubscribeParams params) => mtrBaseClusterContentControl -> params -> Ptr () -> Ptr () -> IO ()
subscribeAttributeScheduledContentRatingsWithParams_subscriptionEstablished_reportHandler mtrBaseClusterContentControl params subscriptionEstablished reportHandler =
  sendMessage mtrBaseClusterContentControl subscribeAttributeScheduledContentRatingsWithParams_subscriptionEstablished_reportHandlerSelector (toMTRSubscribeParams params) subscriptionEstablished reportHandler

-- | @+ readAttributeScheduledContentRatingsWithClusterStateCache:endpoint:queue:completion:@
readAttributeScheduledContentRatingsWithClusterStateCache_endpoint_queue_completion :: (IsMTRClusterStateCacheContainer clusterStateCacheContainer, IsNSNumber endpoint, IsNSObject queue) => clusterStateCacheContainer -> endpoint -> queue -> Ptr () -> IO ()
readAttributeScheduledContentRatingsWithClusterStateCache_endpoint_queue_completion clusterStateCacheContainer endpoint queue completion =
  do
    cls' <- getRequiredClass "MTRBaseClusterContentControl"
    sendClassMessage cls' readAttributeScheduledContentRatingsWithClusterStateCache_endpoint_queue_completionSelector (toMTRClusterStateCacheContainer clusterStateCacheContainer) (toNSNumber endpoint) (toNSObject queue) completion

-- | @- readAttributeScheduledContentRatingThresholdWithCompletion:@
readAttributeScheduledContentRatingThresholdWithCompletion :: IsMTRBaseClusterContentControl mtrBaseClusterContentControl => mtrBaseClusterContentControl -> Ptr () -> IO ()
readAttributeScheduledContentRatingThresholdWithCompletion mtrBaseClusterContentControl completion =
  sendMessage mtrBaseClusterContentControl readAttributeScheduledContentRatingThresholdWithCompletionSelector completion

-- | @- subscribeAttributeScheduledContentRatingThresholdWithParams:subscriptionEstablished:reportHandler:@
subscribeAttributeScheduledContentRatingThresholdWithParams_subscriptionEstablished_reportHandler :: (IsMTRBaseClusterContentControl mtrBaseClusterContentControl, IsMTRSubscribeParams params) => mtrBaseClusterContentControl -> params -> Ptr () -> Ptr () -> IO ()
subscribeAttributeScheduledContentRatingThresholdWithParams_subscriptionEstablished_reportHandler mtrBaseClusterContentControl params subscriptionEstablished reportHandler =
  sendMessage mtrBaseClusterContentControl subscribeAttributeScheduledContentRatingThresholdWithParams_subscriptionEstablished_reportHandlerSelector (toMTRSubscribeParams params) subscriptionEstablished reportHandler

-- | @+ readAttributeScheduledContentRatingThresholdWithClusterStateCache:endpoint:queue:completion:@
readAttributeScheduledContentRatingThresholdWithClusterStateCache_endpoint_queue_completion :: (IsMTRClusterStateCacheContainer clusterStateCacheContainer, IsNSNumber endpoint, IsNSObject queue) => clusterStateCacheContainer -> endpoint -> queue -> Ptr () -> IO ()
readAttributeScheduledContentRatingThresholdWithClusterStateCache_endpoint_queue_completion clusterStateCacheContainer endpoint queue completion =
  do
    cls' <- getRequiredClass "MTRBaseClusterContentControl"
    sendClassMessage cls' readAttributeScheduledContentRatingThresholdWithClusterStateCache_endpoint_queue_completionSelector (toMTRClusterStateCacheContainer clusterStateCacheContainer) (toNSNumber endpoint) (toNSObject queue) completion

-- | @- readAttributeScreenDailyTimeWithCompletion:@
readAttributeScreenDailyTimeWithCompletion :: IsMTRBaseClusterContentControl mtrBaseClusterContentControl => mtrBaseClusterContentControl -> Ptr () -> IO ()
readAttributeScreenDailyTimeWithCompletion mtrBaseClusterContentControl completion =
  sendMessage mtrBaseClusterContentControl readAttributeScreenDailyTimeWithCompletionSelector completion

-- | @- subscribeAttributeScreenDailyTimeWithParams:subscriptionEstablished:reportHandler:@
subscribeAttributeScreenDailyTimeWithParams_subscriptionEstablished_reportHandler :: (IsMTRBaseClusterContentControl mtrBaseClusterContentControl, IsMTRSubscribeParams params) => mtrBaseClusterContentControl -> params -> Ptr () -> Ptr () -> IO ()
subscribeAttributeScreenDailyTimeWithParams_subscriptionEstablished_reportHandler mtrBaseClusterContentControl params subscriptionEstablished reportHandler =
  sendMessage mtrBaseClusterContentControl subscribeAttributeScreenDailyTimeWithParams_subscriptionEstablished_reportHandlerSelector (toMTRSubscribeParams params) subscriptionEstablished reportHandler

-- | @+ readAttributeScreenDailyTimeWithClusterStateCache:endpoint:queue:completion:@
readAttributeScreenDailyTimeWithClusterStateCache_endpoint_queue_completion :: (IsMTRClusterStateCacheContainer clusterStateCacheContainer, IsNSNumber endpoint, IsNSObject queue) => clusterStateCacheContainer -> endpoint -> queue -> Ptr () -> IO ()
readAttributeScreenDailyTimeWithClusterStateCache_endpoint_queue_completion clusterStateCacheContainer endpoint queue completion =
  do
    cls' <- getRequiredClass "MTRBaseClusterContentControl"
    sendClassMessage cls' readAttributeScreenDailyTimeWithClusterStateCache_endpoint_queue_completionSelector (toMTRClusterStateCacheContainer clusterStateCacheContainer) (toNSNumber endpoint) (toNSObject queue) completion

-- | @- readAttributeRemainingScreenTimeWithCompletion:@
readAttributeRemainingScreenTimeWithCompletion :: IsMTRBaseClusterContentControl mtrBaseClusterContentControl => mtrBaseClusterContentControl -> Ptr () -> IO ()
readAttributeRemainingScreenTimeWithCompletion mtrBaseClusterContentControl completion =
  sendMessage mtrBaseClusterContentControl readAttributeRemainingScreenTimeWithCompletionSelector completion

-- | @- subscribeAttributeRemainingScreenTimeWithParams:subscriptionEstablished:reportHandler:@
subscribeAttributeRemainingScreenTimeWithParams_subscriptionEstablished_reportHandler :: (IsMTRBaseClusterContentControl mtrBaseClusterContentControl, IsMTRSubscribeParams params) => mtrBaseClusterContentControl -> params -> Ptr () -> Ptr () -> IO ()
subscribeAttributeRemainingScreenTimeWithParams_subscriptionEstablished_reportHandler mtrBaseClusterContentControl params subscriptionEstablished reportHandler =
  sendMessage mtrBaseClusterContentControl subscribeAttributeRemainingScreenTimeWithParams_subscriptionEstablished_reportHandlerSelector (toMTRSubscribeParams params) subscriptionEstablished reportHandler

-- | @+ readAttributeRemainingScreenTimeWithClusterStateCache:endpoint:queue:completion:@
readAttributeRemainingScreenTimeWithClusterStateCache_endpoint_queue_completion :: (IsMTRClusterStateCacheContainer clusterStateCacheContainer, IsNSNumber endpoint, IsNSObject queue) => clusterStateCacheContainer -> endpoint -> queue -> Ptr () -> IO ()
readAttributeRemainingScreenTimeWithClusterStateCache_endpoint_queue_completion clusterStateCacheContainer endpoint queue completion =
  do
    cls' <- getRequiredClass "MTRBaseClusterContentControl"
    sendClassMessage cls' readAttributeRemainingScreenTimeWithClusterStateCache_endpoint_queue_completionSelector (toMTRClusterStateCacheContainer clusterStateCacheContainer) (toNSNumber endpoint) (toNSObject queue) completion

-- | @- readAttributeBlockUnratedWithCompletion:@
readAttributeBlockUnratedWithCompletion :: IsMTRBaseClusterContentControl mtrBaseClusterContentControl => mtrBaseClusterContentControl -> Ptr () -> IO ()
readAttributeBlockUnratedWithCompletion mtrBaseClusterContentControl completion =
  sendMessage mtrBaseClusterContentControl readAttributeBlockUnratedWithCompletionSelector completion

-- | @- subscribeAttributeBlockUnratedWithParams:subscriptionEstablished:reportHandler:@
subscribeAttributeBlockUnratedWithParams_subscriptionEstablished_reportHandler :: (IsMTRBaseClusterContentControl mtrBaseClusterContentControl, IsMTRSubscribeParams params) => mtrBaseClusterContentControl -> params -> Ptr () -> Ptr () -> IO ()
subscribeAttributeBlockUnratedWithParams_subscriptionEstablished_reportHandler mtrBaseClusterContentControl params subscriptionEstablished reportHandler =
  sendMessage mtrBaseClusterContentControl subscribeAttributeBlockUnratedWithParams_subscriptionEstablished_reportHandlerSelector (toMTRSubscribeParams params) subscriptionEstablished reportHandler

-- | @+ readAttributeBlockUnratedWithClusterStateCache:endpoint:queue:completion:@
readAttributeBlockUnratedWithClusterStateCache_endpoint_queue_completion :: (IsMTRClusterStateCacheContainer clusterStateCacheContainer, IsNSNumber endpoint, IsNSObject queue) => clusterStateCacheContainer -> endpoint -> queue -> Ptr () -> IO ()
readAttributeBlockUnratedWithClusterStateCache_endpoint_queue_completion clusterStateCacheContainer endpoint queue completion =
  do
    cls' <- getRequiredClass "MTRBaseClusterContentControl"
    sendClassMessage cls' readAttributeBlockUnratedWithClusterStateCache_endpoint_queue_completionSelector (toMTRClusterStateCacheContainer clusterStateCacheContainer) (toNSNumber endpoint) (toNSObject queue) completion

-- | @- readAttributeGeneratedCommandListWithCompletion:@
readAttributeGeneratedCommandListWithCompletion :: IsMTRBaseClusterContentControl mtrBaseClusterContentControl => mtrBaseClusterContentControl -> Ptr () -> IO ()
readAttributeGeneratedCommandListWithCompletion mtrBaseClusterContentControl completion =
  sendMessage mtrBaseClusterContentControl readAttributeGeneratedCommandListWithCompletionSelector completion

-- | @- subscribeAttributeGeneratedCommandListWithParams:subscriptionEstablished:reportHandler:@
subscribeAttributeGeneratedCommandListWithParams_subscriptionEstablished_reportHandler :: (IsMTRBaseClusterContentControl mtrBaseClusterContentControl, IsMTRSubscribeParams params) => mtrBaseClusterContentControl -> params -> Ptr () -> Ptr () -> IO ()
subscribeAttributeGeneratedCommandListWithParams_subscriptionEstablished_reportHandler mtrBaseClusterContentControl params subscriptionEstablished reportHandler =
  sendMessage mtrBaseClusterContentControl subscribeAttributeGeneratedCommandListWithParams_subscriptionEstablished_reportHandlerSelector (toMTRSubscribeParams params) subscriptionEstablished reportHandler

-- | @+ readAttributeGeneratedCommandListWithClusterStateCache:endpoint:queue:completion:@
readAttributeGeneratedCommandListWithClusterStateCache_endpoint_queue_completion :: (IsMTRClusterStateCacheContainer clusterStateCacheContainer, IsNSNumber endpoint, IsNSObject queue) => clusterStateCacheContainer -> endpoint -> queue -> Ptr () -> IO ()
readAttributeGeneratedCommandListWithClusterStateCache_endpoint_queue_completion clusterStateCacheContainer endpoint queue completion =
  do
    cls' <- getRequiredClass "MTRBaseClusterContentControl"
    sendClassMessage cls' readAttributeGeneratedCommandListWithClusterStateCache_endpoint_queue_completionSelector (toMTRClusterStateCacheContainer clusterStateCacheContainer) (toNSNumber endpoint) (toNSObject queue) completion

-- | @- readAttributeAcceptedCommandListWithCompletion:@
readAttributeAcceptedCommandListWithCompletion :: IsMTRBaseClusterContentControl mtrBaseClusterContentControl => mtrBaseClusterContentControl -> Ptr () -> IO ()
readAttributeAcceptedCommandListWithCompletion mtrBaseClusterContentControl completion =
  sendMessage mtrBaseClusterContentControl readAttributeAcceptedCommandListWithCompletionSelector completion

-- | @- subscribeAttributeAcceptedCommandListWithParams:subscriptionEstablished:reportHandler:@
subscribeAttributeAcceptedCommandListWithParams_subscriptionEstablished_reportHandler :: (IsMTRBaseClusterContentControl mtrBaseClusterContentControl, IsMTRSubscribeParams params) => mtrBaseClusterContentControl -> params -> Ptr () -> Ptr () -> IO ()
subscribeAttributeAcceptedCommandListWithParams_subscriptionEstablished_reportHandler mtrBaseClusterContentControl params subscriptionEstablished reportHandler =
  sendMessage mtrBaseClusterContentControl subscribeAttributeAcceptedCommandListWithParams_subscriptionEstablished_reportHandlerSelector (toMTRSubscribeParams params) subscriptionEstablished reportHandler

-- | @+ readAttributeAcceptedCommandListWithClusterStateCache:endpoint:queue:completion:@
readAttributeAcceptedCommandListWithClusterStateCache_endpoint_queue_completion :: (IsMTRClusterStateCacheContainer clusterStateCacheContainer, IsNSNumber endpoint, IsNSObject queue) => clusterStateCacheContainer -> endpoint -> queue -> Ptr () -> IO ()
readAttributeAcceptedCommandListWithClusterStateCache_endpoint_queue_completion clusterStateCacheContainer endpoint queue completion =
  do
    cls' <- getRequiredClass "MTRBaseClusterContentControl"
    sendClassMessage cls' readAttributeAcceptedCommandListWithClusterStateCache_endpoint_queue_completionSelector (toMTRClusterStateCacheContainer clusterStateCacheContainer) (toNSNumber endpoint) (toNSObject queue) completion

-- | @- readAttributeAttributeListWithCompletion:@
readAttributeAttributeListWithCompletion :: IsMTRBaseClusterContentControl mtrBaseClusterContentControl => mtrBaseClusterContentControl -> Ptr () -> IO ()
readAttributeAttributeListWithCompletion mtrBaseClusterContentControl completion =
  sendMessage mtrBaseClusterContentControl readAttributeAttributeListWithCompletionSelector completion

-- | @- subscribeAttributeAttributeListWithParams:subscriptionEstablished:reportHandler:@
subscribeAttributeAttributeListWithParams_subscriptionEstablished_reportHandler :: (IsMTRBaseClusterContentControl mtrBaseClusterContentControl, IsMTRSubscribeParams params) => mtrBaseClusterContentControl -> params -> Ptr () -> Ptr () -> IO ()
subscribeAttributeAttributeListWithParams_subscriptionEstablished_reportHandler mtrBaseClusterContentControl params subscriptionEstablished reportHandler =
  sendMessage mtrBaseClusterContentControl subscribeAttributeAttributeListWithParams_subscriptionEstablished_reportHandlerSelector (toMTRSubscribeParams params) subscriptionEstablished reportHandler

-- | @+ readAttributeAttributeListWithClusterStateCache:endpoint:queue:completion:@
readAttributeAttributeListWithClusterStateCache_endpoint_queue_completion :: (IsMTRClusterStateCacheContainer clusterStateCacheContainer, IsNSNumber endpoint, IsNSObject queue) => clusterStateCacheContainer -> endpoint -> queue -> Ptr () -> IO ()
readAttributeAttributeListWithClusterStateCache_endpoint_queue_completion clusterStateCacheContainer endpoint queue completion =
  do
    cls' <- getRequiredClass "MTRBaseClusterContentControl"
    sendClassMessage cls' readAttributeAttributeListWithClusterStateCache_endpoint_queue_completionSelector (toMTRClusterStateCacheContainer clusterStateCacheContainer) (toNSNumber endpoint) (toNSObject queue) completion

-- | @- readAttributeFeatureMapWithCompletion:@
readAttributeFeatureMapWithCompletion :: IsMTRBaseClusterContentControl mtrBaseClusterContentControl => mtrBaseClusterContentControl -> Ptr () -> IO ()
readAttributeFeatureMapWithCompletion mtrBaseClusterContentControl completion =
  sendMessage mtrBaseClusterContentControl readAttributeFeatureMapWithCompletionSelector completion

-- | @- subscribeAttributeFeatureMapWithParams:subscriptionEstablished:reportHandler:@
subscribeAttributeFeatureMapWithParams_subscriptionEstablished_reportHandler :: (IsMTRBaseClusterContentControl mtrBaseClusterContentControl, IsMTRSubscribeParams params) => mtrBaseClusterContentControl -> params -> Ptr () -> Ptr () -> IO ()
subscribeAttributeFeatureMapWithParams_subscriptionEstablished_reportHandler mtrBaseClusterContentControl params subscriptionEstablished reportHandler =
  sendMessage mtrBaseClusterContentControl subscribeAttributeFeatureMapWithParams_subscriptionEstablished_reportHandlerSelector (toMTRSubscribeParams params) subscriptionEstablished reportHandler

-- | @+ readAttributeFeatureMapWithClusterStateCache:endpoint:queue:completion:@
readAttributeFeatureMapWithClusterStateCache_endpoint_queue_completion :: (IsMTRClusterStateCacheContainer clusterStateCacheContainer, IsNSNumber endpoint, IsNSObject queue) => clusterStateCacheContainer -> endpoint -> queue -> Ptr () -> IO ()
readAttributeFeatureMapWithClusterStateCache_endpoint_queue_completion clusterStateCacheContainer endpoint queue completion =
  do
    cls' <- getRequiredClass "MTRBaseClusterContentControl"
    sendClassMessage cls' readAttributeFeatureMapWithClusterStateCache_endpoint_queue_completionSelector (toMTRClusterStateCacheContainer clusterStateCacheContainer) (toNSNumber endpoint) (toNSObject queue) completion

-- | @- readAttributeClusterRevisionWithCompletion:@
readAttributeClusterRevisionWithCompletion :: IsMTRBaseClusterContentControl mtrBaseClusterContentControl => mtrBaseClusterContentControl -> Ptr () -> IO ()
readAttributeClusterRevisionWithCompletion mtrBaseClusterContentControl completion =
  sendMessage mtrBaseClusterContentControl readAttributeClusterRevisionWithCompletionSelector completion

-- | @- subscribeAttributeClusterRevisionWithParams:subscriptionEstablished:reportHandler:@
subscribeAttributeClusterRevisionWithParams_subscriptionEstablished_reportHandler :: (IsMTRBaseClusterContentControl mtrBaseClusterContentControl, IsMTRSubscribeParams params) => mtrBaseClusterContentControl -> params -> Ptr () -> Ptr () -> IO ()
subscribeAttributeClusterRevisionWithParams_subscriptionEstablished_reportHandler mtrBaseClusterContentControl params subscriptionEstablished reportHandler =
  sendMessage mtrBaseClusterContentControl subscribeAttributeClusterRevisionWithParams_subscriptionEstablished_reportHandlerSelector (toMTRSubscribeParams params) subscriptionEstablished reportHandler

-- | @+ readAttributeClusterRevisionWithClusterStateCache:endpoint:queue:completion:@
readAttributeClusterRevisionWithClusterStateCache_endpoint_queue_completion :: (IsMTRClusterStateCacheContainer clusterStateCacheContainer, IsNSNumber endpoint, IsNSObject queue) => clusterStateCacheContainer -> endpoint -> queue -> Ptr () -> IO ()
readAttributeClusterRevisionWithClusterStateCache_endpoint_queue_completion clusterStateCacheContainer endpoint queue completion =
  do
    cls' <- getRequiredClass "MTRBaseClusterContentControl"
    sendClassMessage cls' readAttributeClusterRevisionWithClusterStateCache_endpoint_queue_completionSelector (toMTRClusterStateCacheContainer clusterStateCacheContainer) (toNSNumber endpoint) (toNSObject queue) completion

-- | @- init@
init_ :: IsMTRBaseClusterContentControl mtrBaseClusterContentControl => mtrBaseClusterContentControl -> IO (Id MTRBaseClusterContentControl)
init_ mtrBaseClusterContentControl =
  sendOwnedMessage mtrBaseClusterContentControl initSelector

-- | @+ new@
new :: IO (Id MTRBaseClusterContentControl)
new  =
  do
    cls' <- getRequiredClass "MTRBaseClusterContentControl"
    sendOwnedClassMessage cls' newSelector

-- | For all instance methods (reads, writes, commands) that take a completion, the completion will be called on the provided queue.
--
-- ObjC selector: @- initWithDevice:endpointID:queue:@
initWithDevice_endpointID_queue :: (IsMTRBaseClusterContentControl mtrBaseClusterContentControl, IsMTRBaseDevice device, IsNSNumber endpointID, IsNSObject queue) => mtrBaseClusterContentControl -> device -> endpointID -> queue -> IO (Id MTRBaseClusterContentControl)
initWithDevice_endpointID_queue mtrBaseClusterContentControl device endpointID queue =
  sendOwnedMessage mtrBaseClusterContentControl initWithDevice_endpointID_queueSelector (toMTRBaseDevice device) (toNSNumber endpointID) (toNSObject queue)

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @updatePINWithParams:completion:@
updatePINWithParams_completionSelector :: Selector '[Id MTRContentControlClusterUpdatePINParams, Ptr ()] ()
updatePINWithParams_completionSelector = mkSelector "updatePINWithParams:completion:"

-- | @Selector@ for @resetPINWithParams:completion:@
resetPINWithParams_completionSelector :: Selector '[Id MTRContentControlClusterResetPINParams, Ptr ()] ()
resetPINWithParams_completionSelector = mkSelector "resetPINWithParams:completion:"

-- | @Selector@ for @resetPINWithCompletion:@
resetPINWithCompletionSelector :: Selector '[Ptr ()] ()
resetPINWithCompletionSelector = mkSelector "resetPINWithCompletion:"

-- | @Selector@ for @enableWithParams:completion:@
enableWithParams_completionSelector :: Selector '[Id MTRContentControlClusterEnableParams, Ptr ()] ()
enableWithParams_completionSelector = mkSelector "enableWithParams:completion:"

-- | @Selector@ for @enableWithCompletion:@
enableWithCompletionSelector :: Selector '[Ptr ()] ()
enableWithCompletionSelector = mkSelector "enableWithCompletion:"

-- | @Selector@ for @disableWithParams:completion:@
disableWithParams_completionSelector :: Selector '[Id MTRContentControlClusterDisableParams, Ptr ()] ()
disableWithParams_completionSelector = mkSelector "disableWithParams:completion:"

-- | @Selector@ for @disableWithCompletion:@
disableWithCompletionSelector :: Selector '[Ptr ()] ()
disableWithCompletionSelector = mkSelector "disableWithCompletion:"

-- | @Selector@ for @addBonusTimeWithParams:completion:@
addBonusTimeWithParams_completionSelector :: Selector '[Id MTRContentControlClusterAddBonusTimeParams, Ptr ()] ()
addBonusTimeWithParams_completionSelector = mkSelector "addBonusTimeWithParams:completion:"

-- | @Selector@ for @addBonusTimeWithCompletion:@
addBonusTimeWithCompletionSelector :: Selector '[Ptr ()] ()
addBonusTimeWithCompletionSelector = mkSelector "addBonusTimeWithCompletion:"

-- | @Selector@ for @setScreenDailyTimeWithParams:completion:@
setScreenDailyTimeWithParams_completionSelector :: Selector '[Id MTRContentControlClusterSetScreenDailyTimeParams, Ptr ()] ()
setScreenDailyTimeWithParams_completionSelector = mkSelector "setScreenDailyTimeWithParams:completion:"

-- | @Selector@ for @blockUnratedContentWithParams:completion:@
blockUnratedContentWithParams_completionSelector :: Selector '[Id MTRContentControlClusterBlockUnratedContentParams, Ptr ()] ()
blockUnratedContentWithParams_completionSelector = mkSelector "blockUnratedContentWithParams:completion:"

-- | @Selector@ for @blockUnratedContentWithCompletion:@
blockUnratedContentWithCompletionSelector :: Selector '[Ptr ()] ()
blockUnratedContentWithCompletionSelector = mkSelector "blockUnratedContentWithCompletion:"

-- | @Selector@ for @unblockUnratedContentWithParams:completion:@
unblockUnratedContentWithParams_completionSelector :: Selector '[Id MTRContentControlClusterUnblockUnratedContentParams, Ptr ()] ()
unblockUnratedContentWithParams_completionSelector = mkSelector "unblockUnratedContentWithParams:completion:"

-- | @Selector@ for @unblockUnratedContentWithCompletion:@
unblockUnratedContentWithCompletionSelector :: Selector '[Ptr ()] ()
unblockUnratedContentWithCompletionSelector = mkSelector "unblockUnratedContentWithCompletion:"

-- | @Selector@ for @setOnDemandRatingThresholdWithParams:completion:@
setOnDemandRatingThresholdWithParams_completionSelector :: Selector '[Id MTRContentControlClusterSetOnDemandRatingThresholdParams, Ptr ()] ()
setOnDemandRatingThresholdWithParams_completionSelector = mkSelector "setOnDemandRatingThresholdWithParams:completion:"

-- | @Selector@ for @setScheduledContentRatingThresholdWithParams:completion:@
setScheduledContentRatingThresholdWithParams_completionSelector :: Selector '[Id MTRContentControlClusterSetScheduledContentRatingThresholdParams, Ptr ()] ()
setScheduledContentRatingThresholdWithParams_completionSelector = mkSelector "setScheduledContentRatingThresholdWithParams:completion:"

-- | @Selector@ for @readAttributeEnabledWithCompletion:@
readAttributeEnabledWithCompletionSelector :: Selector '[Ptr ()] ()
readAttributeEnabledWithCompletionSelector = mkSelector "readAttributeEnabledWithCompletion:"

-- | @Selector@ for @subscribeAttributeEnabledWithParams:subscriptionEstablished:reportHandler:@
subscribeAttributeEnabledWithParams_subscriptionEstablished_reportHandlerSelector :: Selector '[Id MTRSubscribeParams, Ptr (), Ptr ()] ()
subscribeAttributeEnabledWithParams_subscriptionEstablished_reportHandlerSelector = mkSelector "subscribeAttributeEnabledWithParams:subscriptionEstablished:reportHandler:"

-- | @Selector@ for @readAttributeEnabledWithClusterStateCache:endpoint:queue:completion:@
readAttributeEnabledWithClusterStateCache_endpoint_queue_completionSelector :: Selector '[Id MTRClusterStateCacheContainer, Id NSNumber, Id NSObject, Ptr ()] ()
readAttributeEnabledWithClusterStateCache_endpoint_queue_completionSelector = mkSelector "readAttributeEnabledWithClusterStateCache:endpoint:queue:completion:"

-- | @Selector@ for @readAttributeOnDemandRatingsWithCompletion:@
readAttributeOnDemandRatingsWithCompletionSelector :: Selector '[Ptr ()] ()
readAttributeOnDemandRatingsWithCompletionSelector = mkSelector "readAttributeOnDemandRatingsWithCompletion:"

-- | @Selector@ for @subscribeAttributeOnDemandRatingsWithParams:subscriptionEstablished:reportHandler:@
subscribeAttributeOnDemandRatingsWithParams_subscriptionEstablished_reportHandlerSelector :: Selector '[Id MTRSubscribeParams, Ptr (), Ptr ()] ()
subscribeAttributeOnDemandRatingsWithParams_subscriptionEstablished_reportHandlerSelector = mkSelector "subscribeAttributeOnDemandRatingsWithParams:subscriptionEstablished:reportHandler:"

-- | @Selector@ for @readAttributeOnDemandRatingsWithClusterStateCache:endpoint:queue:completion:@
readAttributeOnDemandRatingsWithClusterStateCache_endpoint_queue_completionSelector :: Selector '[Id MTRClusterStateCacheContainer, Id NSNumber, Id NSObject, Ptr ()] ()
readAttributeOnDemandRatingsWithClusterStateCache_endpoint_queue_completionSelector = mkSelector "readAttributeOnDemandRatingsWithClusterStateCache:endpoint:queue:completion:"

-- | @Selector@ for @readAttributeOnDemandRatingThresholdWithCompletion:@
readAttributeOnDemandRatingThresholdWithCompletionSelector :: Selector '[Ptr ()] ()
readAttributeOnDemandRatingThresholdWithCompletionSelector = mkSelector "readAttributeOnDemandRatingThresholdWithCompletion:"

-- | @Selector@ for @subscribeAttributeOnDemandRatingThresholdWithParams:subscriptionEstablished:reportHandler:@
subscribeAttributeOnDemandRatingThresholdWithParams_subscriptionEstablished_reportHandlerSelector :: Selector '[Id MTRSubscribeParams, Ptr (), Ptr ()] ()
subscribeAttributeOnDemandRatingThresholdWithParams_subscriptionEstablished_reportHandlerSelector = mkSelector "subscribeAttributeOnDemandRatingThresholdWithParams:subscriptionEstablished:reportHandler:"

-- | @Selector@ for @readAttributeOnDemandRatingThresholdWithClusterStateCache:endpoint:queue:completion:@
readAttributeOnDemandRatingThresholdWithClusterStateCache_endpoint_queue_completionSelector :: Selector '[Id MTRClusterStateCacheContainer, Id NSNumber, Id NSObject, Ptr ()] ()
readAttributeOnDemandRatingThresholdWithClusterStateCache_endpoint_queue_completionSelector = mkSelector "readAttributeOnDemandRatingThresholdWithClusterStateCache:endpoint:queue:completion:"

-- | @Selector@ for @readAttributeScheduledContentRatingsWithCompletion:@
readAttributeScheduledContentRatingsWithCompletionSelector :: Selector '[Ptr ()] ()
readAttributeScheduledContentRatingsWithCompletionSelector = mkSelector "readAttributeScheduledContentRatingsWithCompletion:"

-- | @Selector@ for @subscribeAttributeScheduledContentRatingsWithParams:subscriptionEstablished:reportHandler:@
subscribeAttributeScheduledContentRatingsWithParams_subscriptionEstablished_reportHandlerSelector :: Selector '[Id MTRSubscribeParams, Ptr (), Ptr ()] ()
subscribeAttributeScheduledContentRatingsWithParams_subscriptionEstablished_reportHandlerSelector = mkSelector "subscribeAttributeScheduledContentRatingsWithParams:subscriptionEstablished:reportHandler:"

-- | @Selector@ for @readAttributeScheduledContentRatingsWithClusterStateCache:endpoint:queue:completion:@
readAttributeScheduledContentRatingsWithClusterStateCache_endpoint_queue_completionSelector :: Selector '[Id MTRClusterStateCacheContainer, Id NSNumber, Id NSObject, Ptr ()] ()
readAttributeScheduledContentRatingsWithClusterStateCache_endpoint_queue_completionSelector = mkSelector "readAttributeScheduledContentRatingsWithClusterStateCache:endpoint:queue:completion:"

-- | @Selector@ for @readAttributeScheduledContentRatingThresholdWithCompletion:@
readAttributeScheduledContentRatingThresholdWithCompletionSelector :: Selector '[Ptr ()] ()
readAttributeScheduledContentRatingThresholdWithCompletionSelector = mkSelector "readAttributeScheduledContentRatingThresholdWithCompletion:"

-- | @Selector@ for @subscribeAttributeScheduledContentRatingThresholdWithParams:subscriptionEstablished:reportHandler:@
subscribeAttributeScheduledContentRatingThresholdWithParams_subscriptionEstablished_reportHandlerSelector :: Selector '[Id MTRSubscribeParams, Ptr (), Ptr ()] ()
subscribeAttributeScheduledContentRatingThresholdWithParams_subscriptionEstablished_reportHandlerSelector = mkSelector "subscribeAttributeScheduledContentRatingThresholdWithParams:subscriptionEstablished:reportHandler:"

-- | @Selector@ for @readAttributeScheduledContentRatingThresholdWithClusterStateCache:endpoint:queue:completion:@
readAttributeScheduledContentRatingThresholdWithClusterStateCache_endpoint_queue_completionSelector :: Selector '[Id MTRClusterStateCacheContainer, Id NSNumber, Id NSObject, Ptr ()] ()
readAttributeScheduledContentRatingThresholdWithClusterStateCache_endpoint_queue_completionSelector = mkSelector "readAttributeScheduledContentRatingThresholdWithClusterStateCache:endpoint:queue:completion:"

-- | @Selector@ for @readAttributeScreenDailyTimeWithCompletion:@
readAttributeScreenDailyTimeWithCompletionSelector :: Selector '[Ptr ()] ()
readAttributeScreenDailyTimeWithCompletionSelector = mkSelector "readAttributeScreenDailyTimeWithCompletion:"

-- | @Selector@ for @subscribeAttributeScreenDailyTimeWithParams:subscriptionEstablished:reportHandler:@
subscribeAttributeScreenDailyTimeWithParams_subscriptionEstablished_reportHandlerSelector :: Selector '[Id MTRSubscribeParams, Ptr (), Ptr ()] ()
subscribeAttributeScreenDailyTimeWithParams_subscriptionEstablished_reportHandlerSelector = mkSelector "subscribeAttributeScreenDailyTimeWithParams:subscriptionEstablished:reportHandler:"

-- | @Selector@ for @readAttributeScreenDailyTimeWithClusterStateCache:endpoint:queue:completion:@
readAttributeScreenDailyTimeWithClusterStateCache_endpoint_queue_completionSelector :: Selector '[Id MTRClusterStateCacheContainer, Id NSNumber, Id NSObject, Ptr ()] ()
readAttributeScreenDailyTimeWithClusterStateCache_endpoint_queue_completionSelector = mkSelector "readAttributeScreenDailyTimeWithClusterStateCache:endpoint:queue:completion:"

-- | @Selector@ for @readAttributeRemainingScreenTimeWithCompletion:@
readAttributeRemainingScreenTimeWithCompletionSelector :: Selector '[Ptr ()] ()
readAttributeRemainingScreenTimeWithCompletionSelector = mkSelector "readAttributeRemainingScreenTimeWithCompletion:"

-- | @Selector@ for @subscribeAttributeRemainingScreenTimeWithParams:subscriptionEstablished:reportHandler:@
subscribeAttributeRemainingScreenTimeWithParams_subscriptionEstablished_reportHandlerSelector :: Selector '[Id MTRSubscribeParams, Ptr (), Ptr ()] ()
subscribeAttributeRemainingScreenTimeWithParams_subscriptionEstablished_reportHandlerSelector = mkSelector "subscribeAttributeRemainingScreenTimeWithParams:subscriptionEstablished:reportHandler:"

-- | @Selector@ for @readAttributeRemainingScreenTimeWithClusterStateCache:endpoint:queue:completion:@
readAttributeRemainingScreenTimeWithClusterStateCache_endpoint_queue_completionSelector :: Selector '[Id MTRClusterStateCacheContainer, Id NSNumber, Id NSObject, Ptr ()] ()
readAttributeRemainingScreenTimeWithClusterStateCache_endpoint_queue_completionSelector = mkSelector "readAttributeRemainingScreenTimeWithClusterStateCache:endpoint:queue:completion:"

-- | @Selector@ for @readAttributeBlockUnratedWithCompletion:@
readAttributeBlockUnratedWithCompletionSelector :: Selector '[Ptr ()] ()
readAttributeBlockUnratedWithCompletionSelector = mkSelector "readAttributeBlockUnratedWithCompletion:"

-- | @Selector@ for @subscribeAttributeBlockUnratedWithParams:subscriptionEstablished:reportHandler:@
subscribeAttributeBlockUnratedWithParams_subscriptionEstablished_reportHandlerSelector :: Selector '[Id MTRSubscribeParams, Ptr (), Ptr ()] ()
subscribeAttributeBlockUnratedWithParams_subscriptionEstablished_reportHandlerSelector = mkSelector "subscribeAttributeBlockUnratedWithParams:subscriptionEstablished:reportHandler:"

-- | @Selector@ for @readAttributeBlockUnratedWithClusterStateCache:endpoint:queue:completion:@
readAttributeBlockUnratedWithClusterStateCache_endpoint_queue_completionSelector :: Selector '[Id MTRClusterStateCacheContainer, Id NSNumber, Id NSObject, Ptr ()] ()
readAttributeBlockUnratedWithClusterStateCache_endpoint_queue_completionSelector = mkSelector "readAttributeBlockUnratedWithClusterStateCache:endpoint:queue:completion:"

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
initSelector :: Selector '[] (Id MTRBaseClusterContentControl)
initSelector = mkSelector "init"

-- | @Selector@ for @new@
newSelector :: Selector '[] (Id MTRBaseClusterContentControl)
newSelector = mkSelector "new"

-- | @Selector@ for @initWithDevice:endpointID:queue:@
initWithDevice_endpointID_queueSelector :: Selector '[Id MTRBaseDevice, Id NSNumber, Id NSObject] (Id MTRBaseClusterContentControl)
initWithDevice_endpointID_queueSelector = mkSelector "initWithDevice:endpointID:queue:"

