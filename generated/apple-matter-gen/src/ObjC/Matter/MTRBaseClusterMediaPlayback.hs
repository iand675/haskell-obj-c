{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Cluster Media Playback
--
-- This cluster provides an interface for controlling Media Playback (PLAY, PAUSE, etc) on a media device such as a TV or Speaker.
--
-- Generated bindings for @MTRBaseClusterMediaPlayback@.
module ObjC.Matter.MTRBaseClusterMediaPlayback
  ( MTRBaseClusterMediaPlayback
  , IsMTRBaseClusterMediaPlayback(..)
  , playWithParams_completion
  , playWithCompletion
  , pauseWithParams_completion
  , pauseWithCompletion
  , stopWithParams_completion
  , stopWithCompletion
  , startOverWithParams_completion
  , startOverWithCompletion
  , previousWithParams_completion
  , previousWithCompletion
  , nextWithParams_completion
  , nextWithCompletion
  , rewindWithParams_completion
  , rewindWithCompletion
  , fastForwardWithParams_completion
  , fastForwardWithCompletion
  , skipForwardWithParams_completion
  , skipBackwardWithParams_completion
  , seekWithParams_completion
  , activateAudioTrackWithParams_completion
  , activateTextTrackWithParams_completion
  , deactivateTextTrackWithParams_completion
  , deactivateTextTrackWithCompletion
  , readAttributeCurrentStateWithCompletion
  , subscribeAttributeCurrentStateWithParams_subscriptionEstablished_reportHandler
  , readAttributeCurrentStateWithClusterStateCache_endpoint_queue_completion
  , readAttributeStartTimeWithCompletion
  , subscribeAttributeStartTimeWithParams_subscriptionEstablished_reportHandler
  , readAttributeStartTimeWithClusterStateCache_endpoint_queue_completion
  , readAttributeDurationWithCompletion
  , subscribeAttributeDurationWithParams_subscriptionEstablished_reportHandler
  , readAttributeDurationWithClusterStateCache_endpoint_queue_completion
  , readAttributeSampledPositionWithCompletion
  , subscribeAttributeSampledPositionWithParams_subscriptionEstablished_reportHandler
  , readAttributeSampledPositionWithClusterStateCache_endpoint_queue_completion
  , readAttributePlaybackSpeedWithCompletion
  , subscribeAttributePlaybackSpeedWithParams_subscriptionEstablished_reportHandler
  , readAttributePlaybackSpeedWithClusterStateCache_endpoint_queue_completion
  , readAttributeSeekRangeEndWithCompletion
  , subscribeAttributeSeekRangeEndWithParams_subscriptionEstablished_reportHandler
  , readAttributeSeekRangeEndWithClusterStateCache_endpoint_queue_completion
  , readAttributeSeekRangeStartWithCompletion
  , subscribeAttributeSeekRangeStartWithParams_subscriptionEstablished_reportHandler
  , readAttributeSeekRangeStartWithClusterStateCache_endpoint_queue_completion
  , readAttributeActiveAudioTrackWithCompletion
  , subscribeAttributeActiveAudioTrackWithParams_subscriptionEstablished_reportHandler
  , readAttributeActiveAudioTrackWithClusterStateCache_endpoint_queue_completion
  , readAttributeAvailableAudioTracksWithCompletion
  , subscribeAttributeAvailableAudioTracksWithParams_subscriptionEstablished_reportHandler
  , readAttributeAvailableAudioTracksWithClusterStateCache_endpoint_queue_completion
  , readAttributeActiveTextTrackWithCompletion
  , subscribeAttributeActiveTextTrackWithParams_subscriptionEstablished_reportHandler
  , readAttributeActiveTextTrackWithClusterStateCache_endpoint_queue_completion
  , readAttributeAvailableTextTracksWithCompletion
  , subscribeAttributeAvailableTextTracksWithParams_subscriptionEstablished_reportHandler
  , readAttributeAvailableTextTracksWithClusterStateCache_endpoint_queue_completion
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
  , playWithParams_completionHandler
  , playWithCompletionHandler
  , pauseWithParams_completionHandler
  , pauseWithCompletionHandler
  , stopPlaybackWithParams_completionHandler
  , stopPlaybackWithCompletionHandler
  , startOverWithParams_completionHandler
  , startOverWithCompletionHandler
  , previousWithParams_completionHandler
  , previousWithCompletionHandler
  , nextWithParams_completionHandler
  , nextWithCompletionHandler
  , rewindWithParams_completionHandler
  , rewindWithCompletionHandler
  , fastForwardWithParams_completionHandler
  , fastForwardWithCompletionHandler
  , skipForwardWithParams_completionHandler
  , skipBackwardWithParams_completionHandler
  , seekWithParams_completionHandler
  , readAttributeCurrentStateWithCompletionHandler
  , subscribeAttributeCurrentStateWithMinInterval_maxInterval_params_subscriptionEstablished_reportHandler
  , readAttributeCurrentStateWithAttributeCache_endpoint_queue_completionHandler
  , readAttributeStartTimeWithCompletionHandler
  , subscribeAttributeStartTimeWithMinInterval_maxInterval_params_subscriptionEstablished_reportHandler
  , readAttributeStartTimeWithAttributeCache_endpoint_queue_completionHandler
  , readAttributeDurationWithCompletionHandler
  , subscribeAttributeDurationWithMinInterval_maxInterval_params_subscriptionEstablished_reportHandler
  , readAttributeDurationWithAttributeCache_endpoint_queue_completionHandler
  , readAttributeSampledPositionWithCompletionHandler
  , subscribeAttributeSampledPositionWithMinInterval_maxInterval_params_subscriptionEstablished_reportHandler
  , readAttributeSampledPositionWithAttributeCache_endpoint_queue_completionHandler
  , readAttributePlaybackSpeedWithCompletionHandler
  , subscribeAttributePlaybackSpeedWithMinInterval_maxInterval_params_subscriptionEstablished_reportHandler
  , readAttributePlaybackSpeedWithAttributeCache_endpoint_queue_completionHandler
  , readAttributeSeekRangeEndWithCompletionHandler
  , subscribeAttributeSeekRangeEndWithMinInterval_maxInterval_params_subscriptionEstablished_reportHandler
  , readAttributeSeekRangeEndWithAttributeCache_endpoint_queue_completionHandler
  , readAttributeSeekRangeStartWithCompletionHandler
  , subscribeAttributeSeekRangeStartWithMinInterval_maxInterval_params_subscriptionEstablished_reportHandler
  , readAttributeSeekRangeStartWithAttributeCache_endpoint_queue_completionHandler
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
  , activateAudioTrackWithParams_completionSelector
  , activateTextTrackWithParams_completionSelector
  , deactivateTextTrackWithCompletionSelector
  , deactivateTextTrackWithParams_completionSelector
  , fastForwardWithCompletionHandlerSelector
  , fastForwardWithCompletionSelector
  , fastForwardWithParams_completionHandlerSelector
  , fastForwardWithParams_completionSelector
  , initSelector
  , initWithDevice_endpointID_queueSelector
  , initWithDevice_endpoint_queueSelector
  , newSelector
  , nextWithCompletionHandlerSelector
  , nextWithCompletionSelector
  , nextWithParams_completionHandlerSelector
  , nextWithParams_completionSelector
  , pauseWithCompletionHandlerSelector
  , pauseWithCompletionSelector
  , pauseWithParams_completionHandlerSelector
  , pauseWithParams_completionSelector
  , playWithCompletionHandlerSelector
  , playWithCompletionSelector
  , playWithParams_completionHandlerSelector
  , playWithParams_completionSelector
  , previousWithCompletionHandlerSelector
  , previousWithCompletionSelector
  , previousWithParams_completionHandlerSelector
  , previousWithParams_completionSelector
  , readAttributeAcceptedCommandListWithAttributeCache_endpoint_queue_completionHandlerSelector
  , readAttributeAcceptedCommandListWithClusterStateCache_endpoint_queue_completionSelector
  , readAttributeAcceptedCommandListWithCompletionHandlerSelector
  , readAttributeAcceptedCommandListWithCompletionSelector
  , readAttributeActiveAudioTrackWithClusterStateCache_endpoint_queue_completionSelector
  , readAttributeActiveAudioTrackWithCompletionSelector
  , readAttributeActiveTextTrackWithClusterStateCache_endpoint_queue_completionSelector
  , readAttributeActiveTextTrackWithCompletionSelector
  , readAttributeAttributeListWithAttributeCache_endpoint_queue_completionHandlerSelector
  , readAttributeAttributeListWithClusterStateCache_endpoint_queue_completionSelector
  , readAttributeAttributeListWithCompletionHandlerSelector
  , readAttributeAttributeListWithCompletionSelector
  , readAttributeAvailableAudioTracksWithClusterStateCache_endpoint_queue_completionSelector
  , readAttributeAvailableAudioTracksWithCompletionSelector
  , readAttributeAvailableTextTracksWithClusterStateCache_endpoint_queue_completionSelector
  , readAttributeAvailableTextTracksWithCompletionSelector
  , readAttributeClusterRevisionWithAttributeCache_endpoint_queue_completionHandlerSelector
  , readAttributeClusterRevisionWithClusterStateCache_endpoint_queue_completionSelector
  , readAttributeClusterRevisionWithCompletionHandlerSelector
  , readAttributeClusterRevisionWithCompletionSelector
  , readAttributeCurrentStateWithAttributeCache_endpoint_queue_completionHandlerSelector
  , readAttributeCurrentStateWithClusterStateCache_endpoint_queue_completionSelector
  , readAttributeCurrentStateWithCompletionHandlerSelector
  , readAttributeCurrentStateWithCompletionSelector
  , readAttributeDurationWithAttributeCache_endpoint_queue_completionHandlerSelector
  , readAttributeDurationWithClusterStateCache_endpoint_queue_completionSelector
  , readAttributeDurationWithCompletionHandlerSelector
  , readAttributeDurationWithCompletionSelector
  , readAttributeFeatureMapWithAttributeCache_endpoint_queue_completionHandlerSelector
  , readAttributeFeatureMapWithClusterStateCache_endpoint_queue_completionSelector
  , readAttributeFeatureMapWithCompletionHandlerSelector
  , readAttributeFeatureMapWithCompletionSelector
  , readAttributeGeneratedCommandListWithAttributeCache_endpoint_queue_completionHandlerSelector
  , readAttributeGeneratedCommandListWithClusterStateCache_endpoint_queue_completionSelector
  , readAttributeGeneratedCommandListWithCompletionHandlerSelector
  , readAttributeGeneratedCommandListWithCompletionSelector
  , readAttributePlaybackSpeedWithAttributeCache_endpoint_queue_completionHandlerSelector
  , readAttributePlaybackSpeedWithClusterStateCache_endpoint_queue_completionSelector
  , readAttributePlaybackSpeedWithCompletionHandlerSelector
  , readAttributePlaybackSpeedWithCompletionSelector
  , readAttributeSampledPositionWithAttributeCache_endpoint_queue_completionHandlerSelector
  , readAttributeSampledPositionWithClusterStateCache_endpoint_queue_completionSelector
  , readAttributeSampledPositionWithCompletionHandlerSelector
  , readAttributeSampledPositionWithCompletionSelector
  , readAttributeSeekRangeEndWithAttributeCache_endpoint_queue_completionHandlerSelector
  , readAttributeSeekRangeEndWithClusterStateCache_endpoint_queue_completionSelector
  , readAttributeSeekRangeEndWithCompletionHandlerSelector
  , readAttributeSeekRangeEndWithCompletionSelector
  , readAttributeSeekRangeStartWithAttributeCache_endpoint_queue_completionHandlerSelector
  , readAttributeSeekRangeStartWithClusterStateCache_endpoint_queue_completionSelector
  , readAttributeSeekRangeStartWithCompletionHandlerSelector
  , readAttributeSeekRangeStartWithCompletionSelector
  , readAttributeStartTimeWithAttributeCache_endpoint_queue_completionHandlerSelector
  , readAttributeStartTimeWithClusterStateCache_endpoint_queue_completionSelector
  , readAttributeStartTimeWithCompletionHandlerSelector
  , readAttributeStartTimeWithCompletionSelector
  , rewindWithCompletionHandlerSelector
  , rewindWithCompletionSelector
  , rewindWithParams_completionHandlerSelector
  , rewindWithParams_completionSelector
  , seekWithParams_completionHandlerSelector
  , seekWithParams_completionSelector
  , skipBackwardWithParams_completionHandlerSelector
  , skipBackwardWithParams_completionSelector
  , skipForwardWithParams_completionHandlerSelector
  , skipForwardWithParams_completionSelector
  , startOverWithCompletionHandlerSelector
  , startOverWithCompletionSelector
  , startOverWithParams_completionHandlerSelector
  , startOverWithParams_completionSelector
  , stopPlaybackWithCompletionHandlerSelector
  , stopPlaybackWithParams_completionHandlerSelector
  , stopWithCompletionSelector
  , stopWithParams_completionSelector
  , subscribeAttributeAcceptedCommandListWithMinInterval_maxInterval_params_subscriptionEstablished_reportHandlerSelector
  , subscribeAttributeAcceptedCommandListWithParams_subscriptionEstablished_reportHandlerSelector
  , subscribeAttributeActiveAudioTrackWithParams_subscriptionEstablished_reportHandlerSelector
  , subscribeAttributeActiveTextTrackWithParams_subscriptionEstablished_reportHandlerSelector
  , subscribeAttributeAttributeListWithMinInterval_maxInterval_params_subscriptionEstablished_reportHandlerSelector
  , subscribeAttributeAttributeListWithParams_subscriptionEstablished_reportHandlerSelector
  , subscribeAttributeAvailableAudioTracksWithParams_subscriptionEstablished_reportHandlerSelector
  , subscribeAttributeAvailableTextTracksWithParams_subscriptionEstablished_reportHandlerSelector
  , subscribeAttributeClusterRevisionWithMinInterval_maxInterval_params_subscriptionEstablished_reportHandlerSelector
  , subscribeAttributeClusterRevisionWithParams_subscriptionEstablished_reportHandlerSelector
  , subscribeAttributeCurrentStateWithMinInterval_maxInterval_params_subscriptionEstablished_reportHandlerSelector
  , subscribeAttributeCurrentStateWithParams_subscriptionEstablished_reportHandlerSelector
  , subscribeAttributeDurationWithMinInterval_maxInterval_params_subscriptionEstablished_reportHandlerSelector
  , subscribeAttributeDurationWithParams_subscriptionEstablished_reportHandlerSelector
  , subscribeAttributeFeatureMapWithMinInterval_maxInterval_params_subscriptionEstablished_reportHandlerSelector
  , subscribeAttributeFeatureMapWithParams_subscriptionEstablished_reportHandlerSelector
  , subscribeAttributeGeneratedCommandListWithMinInterval_maxInterval_params_subscriptionEstablished_reportHandlerSelector
  , subscribeAttributeGeneratedCommandListWithParams_subscriptionEstablished_reportHandlerSelector
  , subscribeAttributePlaybackSpeedWithMinInterval_maxInterval_params_subscriptionEstablished_reportHandlerSelector
  , subscribeAttributePlaybackSpeedWithParams_subscriptionEstablished_reportHandlerSelector
  , subscribeAttributeSampledPositionWithMinInterval_maxInterval_params_subscriptionEstablished_reportHandlerSelector
  , subscribeAttributeSampledPositionWithParams_subscriptionEstablished_reportHandlerSelector
  , subscribeAttributeSeekRangeEndWithMinInterval_maxInterval_params_subscriptionEstablished_reportHandlerSelector
  , subscribeAttributeSeekRangeEndWithParams_subscriptionEstablished_reportHandlerSelector
  , subscribeAttributeSeekRangeStartWithMinInterval_maxInterval_params_subscriptionEstablished_reportHandlerSelector
  , subscribeAttributeSeekRangeStartWithParams_subscriptionEstablished_reportHandlerSelector
  , subscribeAttributeStartTimeWithMinInterval_maxInterval_params_subscriptionEstablished_reportHandlerSelector
  , subscribeAttributeStartTimeWithParams_subscriptionEstablished_reportHandlerSelector


  ) where

import Foreign.Ptr (Ptr, FunPtr)
import Foreign.C.Types

import ObjC.Runtime.Types
import ObjC.Runtime.Message (sendMessage, sendOwnedMessage, sendClassMessage, sendOwnedClassMessage)
import ObjC.Runtime.Selector (mkSelector)
import ObjC.Runtime.Class (getRequiredClass)

import ObjC.Matter.Internal.Classes
import ObjC.Foundation.Internal.Classes

-- | Command Play
--
-- Upon receipt, this SHALL play media.
--
-- ObjC selector: @- playWithParams:completion:@
playWithParams_completion :: (IsMTRBaseClusterMediaPlayback mtrBaseClusterMediaPlayback, IsMTRMediaPlaybackClusterPlayParams params) => mtrBaseClusterMediaPlayback -> params -> Ptr () -> IO ()
playWithParams_completion mtrBaseClusterMediaPlayback params completion =
  sendMessage mtrBaseClusterMediaPlayback playWithParams_completionSelector (toMTRMediaPlaybackClusterPlayParams params) completion

-- | @- playWithCompletion:@
playWithCompletion :: IsMTRBaseClusterMediaPlayback mtrBaseClusterMediaPlayback => mtrBaseClusterMediaPlayback -> Ptr () -> IO ()
playWithCompletion mtrBaseClusterMediaPlayback completion =
  sendMessage mtrBaseClusterMediaPlayback playWithCompletionSelector completion

-- | Command Pause
--
-- Upon receipt, this SHALL pause media.
--
-- ObjC selector: @- pauseWithParams:completion:@
pauseWithParams_completion :: (IsMTRBaseClusterMediaPlayback mtrBaseClusterMediaPlayback, IsMTRMediaPlaybackClusterPauseParams params) => mtrBaseClusterMediaPlayback -> params -> Ptr () -> IO ()
pauseWithParams_completion mtrBaseClusterMediaPlayback params completion =
  sendMessage mtrBaseClusterMediaPlayback pauseWithParams_completionSelector (toMTRMediaPlaybackClusterPauseParams params) completion

-- | @- pauseWithCompletion:@
pauseWithCompletion :: IsMTRBaseClusterMediaPlayback mtrBaseClusterMediaPlayback => mtrBaseClusterMediaPlayback -> Ptr () -> IO ()
pauseWithCompletion mtrBaseClusterMediaPlayback completion =
  sendMessage mtrBaseClusterMediaPlayback pauseWithCompletionSelector completion

-- | Command Stop
--
-- Upon receipt, this SHALL stop media. User experience is context-specific. This will often navigate the user back to the location where media was originally launched.
--
-- ObjC selector: @- stopWithParams:completion:@
stopWithParams_completion :: (IsMTRBaseClusterMediaPlayback mtrBaseClusterMediaPlayback, IsMTRMediaPlaybackClusterStopParams params) => mtrBaseClusterMediaPlayback -> params -> Ptr () -> IO ()
stopWithParams_completion mtrBaseClusterMediaPlayback params completion =
  sendMessage mtrBaseClusterMediaPlayback stopWithParams_completionSelector (toMTRMediaPlaybackClusterStopParams params) completion

-- | @- stopWithCompletion:@
stopWithCompletion :: IsMTRBaseClusterMediaPlayback mtrBaseClusterMediaPlayback => mtrBaseClusterMediaPlayback -> Ptr () -> IO ()
stopWithCompletion mtrBaseClusterMediaPlayback completion =
  sendMessage mtrBaseClusterMediaPlayback stopWithCompletionSelector completion

-- | Command StartOver
--
-- Upon receipt, this SHALL Start Over with the current media playback item.
--
-- ObjC selector: @- startOverWithParams:completion:@
startOverWithParams_completion :: (IsMTRBaseClusterMediaPlayback mtrBaseClusterMediaPlayback, IsMTRMediaPlaybackClusterStartOverParams params) => mtrBaseClusterMediaPlayback -> params -> Ptr () -> IO ()
startOverWithParams_completion mtrBaseClusterMediaPlayback params completion =
  sendMessage mtrBaseClusterMediaPlayback startOverWithParams_completionSelector (toMTRMediaPlaybackClusterStartOverParams params) completion

-- | @- startOverWithCompletion:@
startOverWithCompletion :: IsMTRBaseClusterMediaPlayback mtrBaseClusterMediaPlayback => mtrBaseClusterMediaPlayback -> Ptr () -> IO ()
startOverWithCompletion mtrBaseClusterMediaPlayback completion =
  sendMessage mtrBaseClusterMediaPlayback startOverWithCompletionSelector completion

-- | Command Previous
--
-- Upon receipt, this SHALL cause the handler to be invoked for "Previous". User experience is context-specific. This will often Go back to the previous media playback item.
--
-- ObjC selector: @- previousWithParams:completion:@
previousWithParams_completion :: (IsMTRBaseClusterMediaPlayback mtrBaseClusterMediaPlayback, IsMTRMediaPlaybackClusterPreviousParams params) => mtrBaseClusterMediaPlayback -> params -> Ptr () -> IO ()
previousWithParams_completion mtrBaseClusterMediaPlayback params completion =
  sendMessage mtrBaseClusterMediaPlayback previousWithParams_completionSelector (toMTRMediaPlaybackClusterPreviousParams params) completion

-- | @- previousWithCompletion:@
previousWithCompletion :: IsMTRBaseClusterMediaPlayback mtrBaseClusterMediaPlayback => mtrBaseClusterMediaPlayback -> Ptr () -> IO ()
previousWithCompletion mtrBaseClusterMediaPlayback completion =
  sendMessage mtrBaseClusterMediaPlayback previousWithCompletionSelector completion

-- | Command Next
--
-- Upon receipt, this SHALL cause the handler to be invoked for "Next". User experience is context-specific. This will often Go forward to the next media playback item.
--
-- ObjC selector: @- nextWithParams:completion:@
nextWithParams_completion :: (IsMTRBaseClusterMediaPlayback mtrBaseClusterMediaPlayback, IsMTRMediaPlaybackClusterNextParams params) => mtrBaseClusterMediaPlayback -> params -> Ptr () -> IO ()
nextWithParams_completion mtrBaseClusterMediaPlayback params completion =
  sendMessage mtrBaseClusterMediaPlayback nextWithParams_completionSelector (toMTRMediaPlaybackClusterNextParams params) completion

-- | @- nextWithCompletion:@
nextWithCompletion :: IsMTRBaseClusterMediaPlayback mtrBaseClusterMediaPlayback => mtrBaseClusterMediaPlayback -> Ptr () -> IO ()
nextWithCompletion mtrBaseClusterMediaPlayback completion =
  sendMessage mtrBaseClusterMediaPlayback nextWithCompletionSelector completion

-- | Command Rewind
--
-- Upon receipt, this SHALL Rewind through media. Different Rewind speeds can be used on the TV based upon the number of sequential calls to this function. This is to avoid needing to define every speed now (multiple fast, slow motion, etc).
--
-- ObjC selector: @- rewindWithParams:completion:@
rewindWithParams_completion :: (IsMTRBaseClusterMediaPlayback mtrBaseClusterMediaPlayback, IsMTRMediaPlaybackClusterRewindParams params) => mtrBaseClusterMediaPlayback -> params -> Ptr () -> IO ()
rewindWithParams_completion mtrBaseClusterMediaPlayback params completion =
  sendMessage mtrBaseClusterMediaPlayback rewindWithParams_completionSelector (toMTRMediaPlaybackClusterRewindParams params) completion

-- | @- rewindWithCompletion:@
rewindWithCompletion :: IsMTRBaseClusterMediaPlayback mtrBaseClusterMediaPlayback => mtrBaseClusterMediaPlayback -> Ptr () -> IO ()
rewindWithCompletion mtrBaseClusterMediaPlayback completion =
  sendMessage mtrBaseClusterMediaPlayback rewindWithCompletionSelector completion

-- | Command FastForward
--
-- Upon receipt, this SHALL Advance through media. Different FF speeds can be used on the TV based upon the number of sequential calls to this function. This is to avoid needing to define every speed now (multiple fast, slow motion, etc).
--
-- ObjC selector: @- fastForwardWithParams:completion:@
fastForwardWithParams_completion :: (IsMTRBaseClusterMediaPlayback mtrBaseClusterMediaPlayback, IsMTRMediaPlaybackClusterFastForwardParams params) => mtrBaseClusterMediaPlayback -> params -> Ptr () -> IO ()
fastForwardWithParams_completion mtrBaseClusterMediaPlayback params completion =
  sendMessage mtrBaseClusterMediaPlayback fastForwardWithParams_completionSelector (toMTRMediaPlaybackClusterFastForwardParams params) completion

-- | @- fastForwardWithCompletion:@
fastForwardWithCompletion :: IsMTRBaseClusterMediaPlayback mtrBaseClusterMediaPlayback => mtrBaseClusterMediaPlayback -> Ptr () -> IO ()
fastForwardWithCompletion mtrBaseClusterMediaPlayback completion =
  sendMessage mtrBaseClusterMediaPlayback fastForwardWithCompletionSelector completion

-- | Command SkipForward
--
-- Upon receipt, this SHALL Skip forward in the media by the given number of seconds, using the data as follows:
--
-- ObjC selector: @- skipForwardWithParams:completion:@
skipForwardWithParams_completion :: (IsMTRBaseClusterMediaPlayback mtrBaseClusterMediaPlayback, IsMTRMediaPlaybackClusterSkipForwardParams params) => mtrBaseClusterMediaPlayback -> params -> Ptr () -> IO ()
skipForwardWithParams_completion mtrBaseClusterMediaPlayback params completion =
  sendMessage mtrBaseClusterMediaPlayback skipForwardWithParams_completionSelector (toMTRMediaPlaybackClusterSkipForwardParams params) completion

-- | Command SkipBackward
--
-- Upon receipt, this SHALL Skip backward in the media by the given number of seconds, using the data as follows:
--
-- ObjC selector: @- skipBackwardWithParams:completion:@
skipBackwardWithParams_completion :: (IsMTRBaseClusterMediaPlayback mtrBaseClusterMediaPlayback, IsMTRMediaPlaybackClusterSkipBackwardParams params) => mtrBaseClusterMediaPlayback -> params -> Ptr () -> IO ()
skipBackwardWithParams_completion mtrBaseClusterMediaPlayback params completion =
  sendMessage mtrBaseClusterMediaPlayback skipBackwardWithParams_completionSelector (toMTRMediaPlaybackClusterSkipBackwardParams params) completion

-- | Command Seek
--
-- Upon receipt, this SHALL Skip backward in the media by the given number of seconds, using the data as follows:
--
-- ObjC selector: @- seekWithParams:completion:@
seekWithParams_completion :: (IsMTRBaseClusterMediaPlayback mtrBaseClusterMediaPlayback, IsMTRMediaPlaybackClusterSeekParams params) => mtrBaseClusterMediaPlayback -> params -> Ptr () -> IO ()
seekWithParams_completion mtrBaseClusterMediaPlayback params completion =
  sendMessage mtrBaseClusterMediaPlayback seekWithParams_completionSelector (toMTRMediaPlaybackClusterSeekParams params) completion

-- | Command ActivateAudioTrack
--
-- Upon receipt, the server SHALL set the active Audio Track to the one identified by the TrackID in the Track catalog for the streaming media. If the TrackID does not exist in the Track catalog, OR does not correspond to the streaming media OR no media is being streamed at the time of receipt of this command, the server will return an error status of INVALID_ARGUMENT.
--
-- ObjC selector: @- activateAudioTrackWithParams:completion:@
activateAudioTrackWithParams_completion :: (IsMTRBaseClusterMediaPlayback mtrBaseClusterMediaPlayback, IsMTRMediaPlaybackClusterActivateAudioTrackParams params) => mtrBaseClusterMediaPlayback -> params -> Ptr () -> IO ()
activateAudioTrackWithParams_completion mtrBaseClusterMediaPlayback params completion =
  sendMessage mtrBaseClusterMediaPlayback activateAudioTrackWithParams_completionSelector (toMTRMediaPlaybackClusterActivateAudioTrackParams params) completion

-- | Command ActivateTextTrack
--
-- Upon receipt, the server SHALL set the active Text Track to the one identified by the TrackID in the Track catalog for the streaming media. If the TrackID does not exist in the Track catalog, OR does not correspond to the streaming media OR no media is being streamed at the time of receipt of this command, the server SHALL return an error status of INVALID_ARGUMENT.
--
-- ObjC selector: @- activateTextTrackWithParams:completion:@
activateTextTrackWithParams_completion :: (IsMTRBaseClusterMediaPlayback mtrBaseClusterMediaPlayback, IsMTRMediaPlaybackClusterActivateTextTrackParams params) => mtrBaseClusterMediaPlayback -> params -> Ptr () -> IO ()
activateTextTrackWithParams_completion mtrBaseClusterMediaPlayback params completion =
  sendMessage mtrBaseClusterMediaPlayback activateTextTrackWithParams_completionSelector (toMTRMediaPlaybackClusterActivateTextTrackParams params) completion

-- | Command DeactivateTextTrack
--
-- If a Text Track is active (i.e. being displayed), upon receipt of this command, the server SHALL stop displaying it.
--
-- ObjC selector: @- deactivateTextTrackWithParams:completion:@
deactivateTextTrackWithParams_completion :: (IsMTRBaseClusterMediaPlayback mtrBaseClusterMediaPlayback, IsMTRMediaPlaybackClusterDeactivateTextTrackParams params) => mtrBaseClusterMediaPlayback -> params -> Ptr () -> IO ()
deactivateTextTrackWithParams_completion mtrBaseClusterMediaPlayback params completion =
  sendMessage mtrBaseClusterMediaPlayback deactivateTextTrackWithParams_completionSelector (toMTRMediaPlaybackClusterDeactivateTextTrackParams params) completion

-- | @- deactivateTextTrackWithCompletion:@
deactivateTextTrackWithCompletion :: IsMTRBaseClusterMediaPlayback mtrBaseClusterMediaPlayback => mtrBaseClusterMediaPlayback -> Ptr () -> IO ()
deactivateTextTrackWithCompletion mtrBaseClusterMediaPlayback completion =
  sendMessage mtrBaseClusterMediaPlayback deactivateTextTrackWithCompletionSelector completion

-- | @- readAttributeCurrentStateWithCompletion:@
readAttributeCurrentStateWithCompletion :: IsMTRBaseClusterMediaPlayback mtrBaseClusterMediaPlayback => mtrBaseClusterMediaPlayback -> Ptr () -> IO ()
readAttributeCurrentStateWithCompletion mtrBaseClusterMediaPlayback completion =
  sendMessage mtrBaseClusterMediaPlayback readAttributeCurrentStateWithCompletionSelector completion

-- | @- subscribeAttributeCurrentStateWithParams:subscriptionEstablished:reportHandler:@
subscribeAttributeCurrentStateWithParams_subscriptionEstablished_reportHandler :: (IsMTRBaseClusterMediaPlayback mtrBaseClusterMediaPlayback, IsMTRSubscribeParams params) => mtrBaseClusterMediaPlayback -> params -> Ptr () -> Ptr () -> IO ()
subscribeAttributeCurrentStateWithParams_subscriptionEstablished_reportHandler mtrBaseClusterMediaPlayback params subscriptionEstablished reportHandler =
  sendMessage mtrBaseClusterMediaPlayback subscribeAttributeCurrentStateWithParams_subscriptionEstablished_reportHandlerSelector (toMTRSubscribeParams params) subscriptionEstablished reportHandler

-- | @+ readAttributeCurrentStateWithClusterStateCache:endpoint:queue:completion:@
readAttributeCurrentStateWithClusterStateCache_endpoint_queue_completion :: (IsMTRClusterStateCacheContainer clusterStateCacheContainer, IsNSNumber endpoint, IsNSObject queue) => clusterStateCacheContainer -> endpoint -> queue -> Ptr () -> IO ()
readAttributeCurrentStateWithClusterStateCache_endpoint_queue_completion clusterStateCacheContainer endpoint queue completion =
  do
    cls' <- getRequiredClass "MTRBaseClusterMediaPlayback"
    sendClassMessage cls' readAttributeCurrentStateWithClusterStateCache_endpoint_queue_completionSelector (toMTRClusterStateCacheContainer clusterStateCacheContainer) (toNSNumber endpoint) (toNSObject queue) completion

-- | @- readAttributeStartTimeWithCompletion:@
readAttributeStartTimeWithCompletion :: IsMTRBaseClusterMediaPlayback mtrBaseClusterMediaPlayback => mtrBaseClusterMediaPlayback -> Ptr () -> IO ()
readAttributeStartTimeWithCompletion mtrBaseClusterMediaPlayback completion =
  sendMessage mtrBaseClusterMediaPlayback readAttributeStartTimeWithCompletionSelector completion

-- | @- subscribeAttributeStartTimeWithParams:subscriptionEstablished:reportHandler:@
subscribeAttributeStartTimeWithParams_subscriptionEstablished_reportHandler :: (IsMTRBaseClusterMediaPlayback mtrBaseClusterMediaPlayback, IsMTRSubscribeParams params) => mtrBaseClusterMediaPlayback -> params -> Ptr () -> Ptr () -> IO ()
subscribeAttributeStartTimeWithParams_subscriptionEstablished_reportHandler mtrBaseClusterMediaPlayback params subscriptionEstablished reportHandler =
  sendMessage mtrBaseClusterMediaPlayback subscribeAttributeStartTimeWithParams_subscriptionEstablished_reportHandlerSelector (toMTRSubscribeParams params) subscriptionEstablished reportHandler

-- | @+ readAttributeStartTimeWithClusterStateCache:endpoint:queue:completion:@
readAttributeStartTimeWithClusterStateCache_endpoint_queue_completion :: (IsMTRClusterStateCacheContainer clusterStateCacheContainer, IsNSNumber endpoint, IsNSObject queue) => clusterStateCacheContainer -> endpoint -> queue -> Ptr () -> IO ()
readAttributeStartTimeWithClusterStateCache_endpoint_queue_completion clusterStateCacheContainer endpoint queue completion =
  do
    cls' <- getRequiredClass "MTRBaseClusterMediaPlayback"
    sendClassMessage cls' readAttributeStartTimeWithClusterStateCache_endpoint_queue_completionSelector (toMTRClusterStateCacheContainer clusterStateCacheContainer) (toNSNumber endpoint) (toNSObject queue) completion

-- | @- readAttributeDurationWithCompletion:@
readAttributeDurationWithCompletion :: IsMTRBaseClusterMediaPlayback mtrBaseClusterMediaPlayback => mtrBaseClusterMediaPlayback -> Ptr () -> IO ()
readAttributeDurationWithCompletion mtrBaseClusterMediaPlayback completion =
  sendMessage mtrBaseClusterMediaPlayback readAttributeDurationWithCompletionSelector completion

-- | @- subscribeAttributeDurationWithParams:subscriptionEstablished:reportHandler:@
subscribeAttributeDurationWithParams_subscriptionEstablished_reportHandler :: (IsMTRBaseClusterMediaPlayback mtrBaseClusterMediaPlayback, IsMTRSubscribeParams params) => mtrBaseClusterMediaPlayback -> params -> Ptr () -> Ptr () -> IO ()
subscribeAttributeDurationWithParams_subscriptionEstablished_reportHandler mtrBaseClusterMediaPlayback params subscriptionEstablished reportHandler =
  sendMessage mtrBaseClusterMediaPlayback subscribeAttributeDurationWithParams_subscriptionEstablished_reportHandlerSelector (toMTRSubscribeParams params) subscriptionEstablished reportHandler

-- | @+ readAttributeDurationWithClusterStateCache:endpoint:queue:completion:@
readAttributeDurationWithClusterStateCache_endpoint_queue_completion :: (IsMTRClusterStateCacheContainer clusterStateCacheContainer, IsNSNumber endpoint, IsNSObject queue) => clusterStateCacheContainer -> endpoint -> queue -> Ptr () -> IO ()
readAttributeDurationWithClusterStateCache_endpoint_queue_completion clusterStateCacheContainer endpoint queue completion =
  do
    cls' <- getRequiredClass "MTRBaseClusterMediaPlayback"
    sendClassMessage cls' readAttributeDurationWithClusterStateCache_endpoint_queue_completionSelector (toMTRClusterStateCacheContainer clusterStateCacheContainer) (toNSNumber endpoint) (toNSObject queue) completion

-- | @- readAttributeSampledPositionWithCompletion:@
readAttributeSampledPositionWithCompletion :: IsMTRBaseClusterMediaPlayback mtrBaseClusterMediaPlayback => mtrBaseClusterMediaPlayback -> Ptr () -> IO ()
readAttributeSampledPositionWithCompletion mtrBaseClusterMediaPlayback completion =
  sendMessage mtrBaseClusterMediaPlayback readAttributeSampledPositionWithCompletionSelector completion

-- | @- subscribeAttributeSampledPositionWithParams:subscriptionEstablished:reportHandler:@
subscribeAttributeSampledPositionWithParams_subscriptionEstablished_reportHandler :: (IsMTRBaseClusterMediaPlayback mtrBaseClusterMediaPlayback, IsMTRSubscribeParams params) => mtrBaseClusterMediaPlayback -> params -> Ptr () -> Ptr () -> IO ()
subscribeAttributeSampledPositionWithParams_subscriptionEstablished_reportHandler mtrBaseClusterMediaPlayback params subscriptionEstablished reportHandler =
  sendMessage mtrBaseClusterMediaPlayback subscribeAttributeSampledPositionWithParams_subscriptionEstablished_reportHandlerSelector (toMTRSubscribeParams params) subscriptionEstablished reportHandler

-- | @+ readAttributeSampledPositionWithClusterStateCache:endpoint:queue:completion:@
readAttributeSampledPositionWithClusterStateCache_endpoint_queue_completion :: (IsMTRClusterStateCacheContainer clusterStateCacheContainer, IsNSNumber endpoint, IsNSObject queue) => clusterStateCacheContainer -> endpoint -> queue -> Ptr () -> IO ()
readAttributeSampledPositionWithClusterStateCache_endpoint_queue_completion clusterStateCacheContainer endpoint queue completion =
  do
    cls' <- getRequiredClass "MTRBaseClusterMediaPlayback"
    sendClassMessage cls' readAttributeSampledPositionWithClusterStateCache_endpoint_queue_completionSelector (toMTRClusterStateCacheContainer clusterStateCacheContainer) (toNSNumber endpoint) (toNSObject queue) completion

-- | @- readAttributePlaybackSpeedWithCompletion:@
readAttributePlaybackSpeedWithCompletion :: IsMTRBaseClusterMediaPlayback mtrBaseClusterMediaPlayback => mtrBaseClusterMediaPlayback -> Ptr () -> IO ()
readAttributePlaybackSpeedWithCompletion mtrBaseClusterMediaPlayback completion =
  sendMessage mtrBaseClusterMediaPlayback readAttributePlaybackSpeedWithCompletionSelector completion

-- | @- subscribeAttributePlaybackSpeedWithParams:subscriptionEstablished:reportHandler:@
subscribeAttributePlaybackSpeedWithParams_subscriptionEstablished_reportHandler :: (IsMTRBaseClusterMediaPlayback mtrBaseClusterMediaPlayback, IsMTRSubscribeParams params) => mtrBaseClusterMediaPlayback -> params -> Ptr () -> Ptr () -> IO ()
subscribeAttributePlaybackSpeedWithParams_subscriptionEstablished_reportHandler mtrBaseClusterMediaPlayback params subscriptionEstablished reportHandler =
  sendMessage mtrBaseClusterMediaPlayback subscribeAttributePlaybackSpeedWithParams_subscriptionEstablished_reportHandlerSelector (toMTRSubscribeParams params) subscriptionEstablished reportHandler

-- | @+ readAttributePlaybackSpeedWithClusterStateCache:endpoint:queue:completion:@
readAttributePlaybackSpeedWithClusterStateCache_endpoint_queue_completion :: (IsMTRClusterStateCacheContainer clusterStateCacheContainer, IsNSNumber endpoint, IsNSObject queue) => clusterStateCacheContainer -> endpoint -> queue -> Ptr () -> IO ()
readAttributePlaybackSpeedWithClusterStateCache_endpoint_queue_completion clusterStateCacheContainer endpoint queue completion =
  do
    cls' <- getRequiredClass "MTRBaseClusterMediaPlayback"
    sendClassMessage cls' readAttributePlaybackSpeedWithClusterStateCache_endpoint_queue_completionSelector (toMTRClusterStateCacheContainer clusterStateCacheContainer) (toNSNumber endpoint) (toNSObject queue) completion

-- | @- readAttributeSeekRangeEndWithCompletion:@
readAttributeSeekRangeEndWithCompletion :: IsMTRBaseClusterMediaPlayback mtrBaseClusterMediaPlayback => mtrBaseClusterMediaPlayback -> Ptr () -> IO ()
readAttributeSeekRangeEndWithCompletion mtrBaseClusterMediaPlayback completion =
  sendMessage mtrBaseClusterMediaPlayback readAttributeSeekRangeEndWithCompletionSelector completion

-- | @- subscribeAttributeSeekRangeEndWithParams:subscriptionEstablished:reportHandler:@
subscribeAttributeSeekRangeEndWithParams_subscriptionEstablished_reportHandler :: (IsMTRBaseClusterMediaPlayback mtrBaseClusterMediaPlayback, IsMTRSubscribeParams params) => mtrBaseClusterMediaPlayback -> params -> Ptr () -> Ptr () -> IO ()
subscribeAttributeSeekRangeEndWithParams_subscriptionEstablished_reportHandler mtrBaseClusterMediaPlayback params subscriptionEstablished reportHandler =
  sendMessage mtrBaseClusterMediaPlayback subscribeAttributeSeekRangeEndWithParams_subscriptionEstablished_reportHandlerSelector (toMTRSubscribeParams params) subscriptionEstablished reportHandler

-- | @+ readAttributeSeekRangeEndWithClusterStateCache:endpoint:queue:completion:@
readAttributeSeekRangeEndWithClusterStateCache_endpoint_queue_completion :: (IsMTRClusterStateCacheContainer clusterStateCacheContainer, IsNSNumber endpoint, IsNSObject queue) => clusterStateCacheContainer -> endpoint -> queue -> Ptr () -> IO ()
readAttributeSeekRangeEndWithClusterStateCache_endpoint_queue_completion clusterStateCacheContainer endpoint queue completion =
  do
    cls' <- getRequiredClass "MTRBaseClusterMediaPlayback"
    sendClassMessage cls' readAttributeSeekRangeEndWithClusterStateCache_endpoint_queue_completionSelector (toMTRClusterStateCacheContainer clusterStateCacheContainer) (toNSNumber endpoint) (toNSObject queue) completion

-- | @- readAttributeSeekRangeStartWithCompletion:@
readAttributeSeekRangeStartWithCompletion :: IsMTRBaseClusterMediaPlayback mtrBaseClusterMediaPlayback => mtrBaseClusterMediaPlayback -> Ptr () -> IO ()
readAttributeSeekRangeStartWithCompletion mtrBaseClusterMediaPlayback completion =
  sendMessage mtrBaseClusterMediaPlayback readAttributeSeekRangeStartWithCompletionSelector completion

-- | @- subscribeAttributeSeekRangeStartWithParams:subscriptionEstablished:reportHandler:@
subscribeAttributeSeekRangeStartWithParams_subscriptionEstablished_reportHandler :: (IsMTRBaseClusterMediaPlayback mtrBaseClusterMediaPlayback, IsMTRSubscribeParams params) => mtrBaseClusterMediaPlayback -> params -> Ptr () -> Ptr () -> IO ()
subscribeAttributeSeekRangeStartWithParams_subscriptionEstablished_reportHandler mtrBaseClusterMediaPlayback params subscriptionEstablished reportHandler =
  sendMessage mtrBaseClusterMediaPlayback subscribeAttributeSeekRangeStartWithParams_subscriptionEstablished_reportHandlerSelector (toMTRSubscribeParams params) subscriptionEstablished reportHandler

-- | @+ readAttributeSeekRangeStartWithClusterStateCache:endpoint:queue:completion:@
readAttributeSeekRangeStartWithClusterStateCache_endpoint_queue_completion :: (IsMTRClusterStateCacheContainer clusterStateCacheContainer, IsNSNumber endpoint, IsNSObject queue) => clusterStateCacheContainer -> endpoint -> queue -> Ptr () -> IO ()
readAttributeSeekRangeStartWithClusterStateCache_endpoint_queue_completion clusterStateCacheContainer endpoint queue completion =
  do
    cls' <- getRequiredClass "MTRBaseClusterMediaPlayback"
    sendClassMessage cls' readAttributeSeekRangeStartWithClusterStateCache_endpoint_queue_completionSelector (toMTRClusterStateCacheContainer clusterStateCacheContainer) (toNSNumber endpoint) (toNSObject queue) completion

-- | @- readAttributeActiveAudioTrackWithCompletion:@
readAttributeActiveAudioTrackWithCompletion :: IsMTRBaseClusterMediaPlayback mtrBaseClusterMediaPlayback => mtrBaseClusterMediaPlayback -> Ptr () -> IO ()
readAttributeActiveAudioTrackWithCompletion mtrBaseClusterMediaPlayback completion =
  sendMessage mtrBaseClusterMediaPlayback readAttributeActiveAudioTrackWithCompletionSelector completion

-- | @- subscribeAttributeActiveAudioTrackWithParams:subscriptionEstablished:reportHandler:@
subscribeAttributeActiveAudioTrackWithParams_subscriptionEstablished_reportHandler :: (IsMTRBaseClusterMediaPlayback mtrBaseClusterMediaPlayback, IsMTRSubscribeParams params) => mtrBaseClusterMediaPlayback -> params -> Ptr () -> Ptr () -> IO ()
subscribeAttributeActiveAudioTrackWithParams_subscriptionEstablished_reportHandler mtrBaseClusterMediaPlayback params subscriptionEstablished reportHandler =
  sendMessage mtrBaseClusterMediaPlayback subscribeAttributeActiveAudioTrackWithParams_subscriptionEstablished_reportHandlerSelector (toMTRSubscribeParams params) subscriptionEstablished reportHandler

-- | @+ readAttributeActiveAudioTrackWithClusterStateCache:endpoint:queue:completion:@
readAttributeActiveAudioTrackWithClusterStateCache_endpoint_queue_completion :: (IsMTRClusterStateCacheContainer clusterStateCacheContainer, IsNSNumber endpoint, IsNSObject queue) => clusterStateCacheContainer -> endpoint -> queue -> Ptr () -> IO ()
readAttributeActiveAudioTrackWithClusterStateCache_endpoint_queue_completion clusterStateCacheContainer endpoint queue completion =
  do
    cls' <- getRequiredClass "MTRBaseClusterMediaPlayback"
    sendClassMessage cls' readAttributeActiveAudioTrackWithClusterStateCache_endpoint_queue_completionSelector (toMTRClusterStateCacheContainer clusterStateCacheContainer) (toNSNumber endpoint) (toNSObject queue) completion

-- | @- readAttributeAvailableAudioTracksWithCompletion:@
readAttributeAvailableAudioTracksWithCompletion :: IsMTRBaseClusterMediaPlayback mtrBaseClusterMediaPlayback => mtrBaseClusterMediaPlayback -> Ptr () -> IO ()
readAttributeAvailableAudioTracksWithCompletion mtrBaseClusterMediaPlayback completion =
  sendMessage mtrBaseClusterMediaPlayback readAttributeAvailableAudioTracksWithCompletionSelector completion

-- | @- subscribeAttributeAvailableAudioTracksWithParams:subscriptionEstablished:reportHandler:@
subscribeAttributeAvailableAudioTracksWithParams_subscriptionEstablished_reportHandler :: (IsMTRBaseClusterMediaPlayback mtrBaseClusterMediaPlayback, IsMTRSubscribeParams params) => mtrBaseClusterMediaPlayback -> params -> Ptr () -> Ptr () -> IO ()
subscribeAttributeAvailableAudioTracksWithParams_subscriptionEstablished_reportHandler mtrBaseClusterMediaPlayback params subscriptionEstablished reportHandler =
  sendMessage mtrBaseClusterMediaPlayback subscribeAttributeAvailableAudioTracksWithParams_subscriptionEstablished_reportHandlerSelector (toMTRSubscribeParams params) subscriptionEstablished reportHandler

-- | @+ readAttributeAvailableAudioTracksWithClusterStateCache:endpoint:queue:completion:@
readAttributeAvailableAudioTracksWithClusterStateCache_endpoint_queue_completion :: (IsMTRClusterStateCacheContainer clusterStateCacheContainer, IsNSNumber endpoint, IsNSObject queue) => clusterStateCacheContainer -> endpoint -> queue -> Ptr () -> IO ()
readAttributeAvailableAudioTracksWithClusterStateCache_endpoint_queue_completion clusterStateCacheContainer endpoint queue completion =
  do
    cls' <- getRequiredClass "MTRBaseClusterMediaPlayback"
    sendClassMessage cls' readAttributeAvailableAudioTracksWithClusterStateCache_endpoint_queue_completionSelector (toMTRClusterStateCacheContainer clusterStateCacheContainer) (toNSNumber endpoint) (toNSObject queue) completion

-- | @- readAttributeActiveTextTrackWithCompletion:@
readAttributeActiveTextTrackWithCompletion :: IsMTRBaseClusterMediaPlayback mtrBaseClusterMediaPlayback => mtrBaseClusterMediaPlayback -> Ptr () -> IO ()
readAttributeActiveTextTrackWithCompletion mtrBaseClusterMediaPlayback completion =
  sendMessage mtrBaseClusterMediaPlayback readAttributeActiveTextTrackWithCompletionSelector completion

-- | @- subscribeAttributeActiveTextTrackWithParams:subscriptionEstablished:reportHandler:@
subscribeAttributeActiveTextTrackWithParams_subscriptionEstablished_reportHandler :: (IsMTRBaseClusterMediaPlayback mtrBaseClusterMediaPlayback, IsMTRSubscribeParams params) => mtrBaseClusterMediaPlayback -> params -> Ptr () -> Ptr () -> IO ()
subscribeAttributeActiveTextTrackWithParams_subscriptionEstablished_reportHandler mtrBaseClusterMediaPlayback params subscriptionEstablished reportHandler =
  sendMessage mtrBaseClusterMediaPlayback subscribeAttributeActiveTextTrackWithParams_subscriptionEstablished_reportHandlerSelector (toMTRSubscribeParams params) subscriptionEstablished reportHandler

-- | @+ readAttributeActiveTextTrackWithClusterStateCache:endpoint:queue:completion:@
readAttributeActiveTextTrackWithClusterStateCache_endpoint_queue_completion :: (IsMTRClusterStateCacheContainer clusterStateCacheContainer, IsNSNumber endpoint, IsNSObject queue) => clusterStateCacheContainer -> endpoint -> queue -> Ptr () -> IO ()
readAttributeActiveTextTrackWithClusterStateCache_endpoint_queue_completion clusterStateCacheContainer endpoint queue completion =
  do
    cls' <- getRequiredClass "MTRBaseClusterMediaPlayback"
    sendClassMessage cls' readAttributeActiveTextTrackWithClusterStateCache_endpoint_queue_completionSelector (toMTRClusterStateCacheContainer clusterStateCacheContainer) (toNSNumber endpoint) (toNSObject queue) completion

-- | @- readAttributeAvailableTextTracksWithCompletion:@
readAttributeAvailableTextTracksWithCompletion :: IsMTRBaseClusterMediaPlayback mtrBaseClusterMediaPlayback => mtrBaseClusterMediaPlayback -> Ptr () -> IO ()
readAttributeAvailableTextTracksWithCompletion mtrBaseClusterMediaPlayback completion =
  sendMessage mtrBaseClusterMediaPlayback readAttributeAvailableTextTracksWithCompletionSelector completion

-- | @- subscribeAttributeAvailableTextTracksWithParams:subscriptionEstablished:reportHandler:@
subscribeAttributeAvailableTextTracksWithParams_subscriptionEstablished_reportHandler :: (IsMTRBaseClusterMediaPlayback mtrBaseClusterMediaPlayback, IsMTRSubscribeParams params) => mtrBaseClusterMediaPlayback -> params -> Ptr () -> Ptr () -> IO ()
subscribeAttributeAvailableTextTracksWithParams_subscriptionEstablished_reportHandler mtrBaseClusterMediaPlayback params subscriptionEstablished reportHandler =
  sendMessage mtrBaseClusterMediaPlayback subscribeAttributeAvailableTextTracksWithParams_subscriptionEstablished_reportHandlerSelector (toMTRSubscribeParams params) subscriptionEstablished reportHandler

-- | @+ readAttributeAvailableTextTracksWithClusterStateCache:endpoint:queue:completion:@
readAttributeAvailableTextTracksWithClusterStateCache_endpoint_queue_completion :: (IsMTRClusterStateCacheContainer clusterStateCacheContainer, IsNSNumber endpoint, IsNSObject queue) => clusterStateCacheContainer -> endpoint -> queue -> Ptr () -> IO ()
readAttributeAvailableTextTracksWithClusterStateCache_endpoint_queue_completion clusterStateCacheContainer endpoint queue completion =
  do
    cls' <- getRequiredClass "MTRBaseClusterMediaPlayback"
    sendClassMessage cls' readAttributeAvailableTextTracksWithClusterStateCache_endpoint_queue_completionSelector (toMTRClusterStateCacheContainer clusterStateCacheContainer) (toNSNumber endpoint) (toNSObject queue) completion

-- | @- readAttributeGeneratedCommandListWithCompletion:@
readAttributeGeneratedCommandListWithCompletion :: IsMTRBaseClusterMediaPlayback mtrBaseClusterMediaPlayback => mtrBaseClusterMediaPlayback -> Ptr () -> IO ()
readAttributeGeneratedCommandListWithCompletion mtrBaseClusterMediaPlayback completion =
  sendMessage mtrBaseClusterMediaPlayback readAttributeGeneratedCommandListWithCompletionSelector completion

-- | @- subscribeAttributeGeneratedCommandListWithParams:subscriptionEstablished:reportHandler:@
subscribeAttributeGeneratedCommandListWithParams_subscriptionEstablished_reportHandler :: (IsMTRBaseClusterMediaPlayback mtrBaseClusterMediaPlayback, IsMTRSubscribeParams params) => mtrBaseClusterMediaPlayback -> params -> Ptr () -> Ptr () -> IO ()
subscribeAttributeGeneratedCommandListWithParams_subscriptionEstablished_reportHandler mtrBaseClusterMediaPlayback params subscriptionEstablished reportHandler =
  sendMessage mtrBaseClusterMediaPlayback subscribeAttributeGeneratedCommandListWithParams_subscriptionEstablished_reportHandlerSelector (toMTRSubscribeParams params) subscriptionEstablished reportHandler

-- | @+ readAttributeGeneratedCommandListWithClusterStateCache:endpoint:queue:completion:@
readAttributeGeneratedCommandListWithClusterStateCache_endpoint_queue_completion :: (IsMTRClusterStateCacheContainer clusterStateCacheContainer, IsNSNumber endpoint, IsNSObject queue) => clusterStateCacheContainer -> endpoint -> queue -> Ptr () -> IO ()
readAttributeGeneratedCommandListWithClusterStateCache_endpoint_queue_completion clusterStateCacheContainer endpoint queue completion =
  do
    cls' <- getRequiredClass "MTRBaseClusterMediaPlayback"
    sendClassMessage cls' readAttributeGeneratedCommandListWithClusterStateCache_endpoint_queue_completionSelector (toMTRClusterStateCacheContainer clusterStateCacheContainer) (toNSNumber endpoint) (toNSObject queue) completion

-- | @- readAttributeAcceptedCommandListWithCompletion:@
readAttributeAcceptedCommandListWithCompletion :: IsMTRBaseClusterMediaPlayback mtrBaseClusterMediaPlayback => mtrBaseClusterMediaPlayback -> Ptr () -> IO ()
readAttributeAcceptedCommandListWithCompletion mtrBaseClusterMediaPlayback completion =
  sendMessage mtrBaseClusterMediaPlayback readAttributeAcceptedCommandListWithCompletionSelector completion

-- | @- subscribeAttributeAcceptedCommandListWithParams:subscriptionEstablished:reportHandler:@
subscribeAttributeAcceptedCommandListWithParams_subscriptionEstablished_reportHandler :: (IsMTRBaseClusterMediaPlayback mtrBaseClusterMediaPlayback, IsMTRSubscribeParams params) => mtrBaseClusterMediaPlayback -> params -> Ptr () -> Ptr () -> IO ()
subscribeAttributeAcceptedCommandListWithParams_subscriptionEstablished_reportHandler mtrBaseClusterMediaPlayback params subscriptionEstablished reportHandler =
  sendMessage mtrBaseClusterMediaPlayback subscribeAttributeAcceptedCommandListWithParams_subscriptionEstablished_reportHandlerSelector (toMTRSubscribeParams params) subscriptionEstablished reportHandler

-- | @+ readAttributeAcceptedCommandListWithClusterStateCache:endpoint:queue:completion:@
readAttributeAcceptedCommandListWithClusterStateCache_endpoint_queue_completion :: (IsMTRClusterStateCacheContainer clusterStateCacheContainer, IsNSNumber endpoint, IsNSObject queue) => clusterStateCacheContainer -> endpoint -> queue -> Ptr () -> IO ()
readAttributeAcceptedCommandListWithClusterStateCache_endpoint_queue_completion clusterStateCacheContainer endpoint queue completion =
  do
    cls' <- getRequiredClass "MTRBaseClusterMediaPlayback"
    sendClassMessage cls' readAttributeAcceptedCommandListWithClusterStateCache_endpoint_queue_completionSelector (toMTRClusterStateCacheContainer clusterStateCacheContainer) (toNSNumber endpoint) (toNSObject queue) completion

-- | @- readAttributeAttributeListWithCompletion:@
readAttributeAttributeListWithCompletion :: IsMTRBaseClusterMediaPlayback mtrBaseClusterMediaPlayback => mtrBaseClusterMediaPlayback -> Ptr () -> IO ()
readAttributeAttributeListWithCompletion mtrBaseClusterMediaPlayback completion =
  sendMessage mtrBaseClusterMediaPlayback readAttributeAttributeListWithCompletionSelector completion

-- | @- subscribeAttributeAttributeListWithParams:subscriptionEstablished:reportHandler:@
subscribeAttributeAttributeListWithParams_subscriptionEstablished_reportHandler :: (IsMTRBaseClusterMediaPlayback mtrBaseClusterMediaPlayback, IsMTRSubscribeParams params) => mtrBaseClusterMediaPlayback -> params -> Ptr () -> Ptr () -> IO ()
subscribeAttributeAttributeListWithParams_subscriptionEstablished_reportHandler mtrBaseClusterMediaPlayback params subscriptionEstablished reportHandler =
  sendMessage mtrBaseClusterMediaPlayback subscribeAttributeAttributeListWithParams_subscriptionEstablished_reportHandlerSelector (toMTRSubscribeParams params) subscriptionEstablished reportHandler

-- | @+ readAttributeAttributeListWithClusterStateCache:endpoint:queue:completion:@
readAttributeAttributeListWithClusterStateCache_endpoint_queue_completion :: (IsMTRClusterStateCacheContainer clusterStateCacheContainer, IsNSNumber endpoint, IsNSObject queue) => clusterStateCacheContainer -> endpoint -> queue -> Ptr () -> IO ()
readAttributeAttributeListWithClusterStateCache_endpoint_queue_completion clusterStateCacheContainer endpoint queue completion =
  do
    cls' <- getRequiredClass "MTRBaseClusterMediaPlayback"
    sendClassMessage cls' readAttributeAttributeListWithClusterStateCache_endpoint_queue_completionSelector (toMTRClusterStateCacheContainer clusterStateCacheContainer) (toNSNumber endpoint) (toNSObject queue) completion

-- | @- readAttributeFeatureMapWithCompletion:@
readAttributeFeatureMapWithCompletion :: IsMTRBaseClusterMediaPlayback mtrBaseClusterMediaPlayback => mtrBaseClusterMediaPlayback -> Ptr () -> IO ()
readAttributeFeatureMapWithCompletion mtrBaseClusterMediaPlayback completion =
  sendMessage mtrBaseClusterMediaPlayback readAttributeFeatureMapWithCompletionSelector completion

-- | @- subscribeAttributeFeatureMapWithParams:subscriptionEstablished:reportHandler:@
subscribeAttributeFeatureMapWithParams_subscriptionEstablished_reportHandler :: (IsMTRBaseClusterMediaPlayback mtrBaseClusterMediaPlayback, IsMTRSubscribeParams params) => mtrBaseClusterMediaPlayback -> params -> Ptr () -> Ptr () -> IO ()
subscribeAttributeFeatureMapWithParams_subscriptionEstablished_reportHandler mtrBaseClusterMediaPlayback params subscriptionEstablished reportHandler =
  sendMessage mtrBaseClusterMediaPlayback subscribeAttributeFeatureMapWithParams_subscriptionEstablished_reportHandlerSelector (toMTRSubscribeParams params) subscriptionEstablished reportHandler

-- | @+ readAttributeFeatureMapWithClusterStateCache:endpoint:queue:completion:@
readAttributeFeatureMapWithClusterStateCache_endpoint_queue_completion :: (IsMTRClusterStateCacheContainer clusterStateCacheContainer, IsNSNumber endpoint, IsNSObject queue) => clusterStateCacheContainer -> endpoint -> queue -> Ptr () -> IO ()
readAttributeFeatureMapWithClusterStateCache_endpoint_queue_completion clusterStateCacheContainer endpoint queue completion =
  do
    cls' <- getRequiredClass "MTRBaseClusterMediaPlayback"
    sendClassMessage cls' readAttributeFeatureMapWithClusterStateCache_endpoint_queue_completionSelector (toMTRClusterStateCacheContainer clusterStateCacheContainer) (toNSNumber endpoint) (toNSObject queue) completion

-- | @- readAttributeClusterRevisionWithCompletion:@
readAttributeClusterRevisionWithCompletion :: IsMTRBaseClusterMediaPlayback mtrBaseClusterMediaPlayback => mtrBaseClusterMediaPlayback -> Ptr () -> IO ()
readAttributeClusterRevisionWithCompletion mtrBaseClusterMediaPlayback completion =
  sendMessage mtrBaseClusterMediaPlayback readAttributeClusterRevisionWithCompletionSelector completion

-- | @- subscribeAttributeClusterRevisionWithParams:subscriptionEstablished:reportHandler:@
subscribeAttributeClusterRevisionWithParams_subscriptionEstablished_reportHandler :: (IsMTRBaseClusterMediaPlayback mtrBaseClusterMediaPlayback, IsMTRSubscribeParams params) => mtrBaseClusterMediaPlayback -> params -> Ptr () -> Ptr () -> IO ()
subscribeAttributeClusterRevisionWithParams_subscriptionEstablished_reportHandler mtrBaseClusterMediaPlayback params subscriptionEstablished reportHandler =
  sendMessage mtrBaseClusterMediaPlayback subscribeAttributeClusterRevisionWithParams_subscriptionEstablished_reportHandlerSelector (toMTRSubscribeParams params) subscriptionEstablished reportHandler

-- | @+ readAttributeClusterRevisionWithClusterStateCache:endpoint:queue:completion:@
readAttributeClusterRevisionWithClusterStateCache_endpoint_queue_completion :: (IsMTRClusterStateCacheContainer clusterStateCacheContainer, IsNSNumber endpoint, IsNSObject queue) => clusterStateCacheContainer -> endpoint -> queue -> Ptr () -> IO ()
readAttributeClusterRevisionWithClusterStateCache_endpoint_queue_completion clusterStateCacheContainer endpoint queue completion =
  do
    cls' <- getRequiredClass "MTRBaseClusterMediaPlayback"
    sendClassMessage cls' readAttributeClusterRevisionWithClusterStateCache_endpoint_queue_completionSelector (toMTRClusterStateCacheContainer clusterStateCacheContainer) (toNSNumber endpoint) (toNSObject queue) completion

-- | @- init@
init_ :: IsMTRBaseClusterMediaPlayback mtrBaseClusterMediaPlayback => mtrBaseClusterMediaPlayback -> IO (Id MTRBaseClusterMediaPlayback)
init_ mtrBaseClusterMediaPlayback =
  sendOwnedMessage mtrBaseClusterMediaPlayback initSelector

-- | @+ new@
new :: IO (Id MTRBaseClusterMediaPlayback)
new  =
  do
    cls' <- getRequiredClass "MTRBaseClusterMediaPlayback"
    sendOwnedClassMessage cls' newSelector

-- | @- initWithDevice:endpoint:queue:@
initWithDevice_endpoint_queue :: (IsMTRBaseClusterMediaPlayback mtrBaseClusterMediaPlayback, IsMTRBaseDevice device, IsNSObject queue) => mtrBaseClusterMediaPlayback -> device -> CUShort -> queue -> IO (Id MTRBaseClusterMediaPlayback)
initWithDevice_endpoint_queue mtrBaseClusterMediaPlayback device endpoint queue =
  sendOwnedMessage mtrBaseClusterMediaPlayback initWithDevice_endpoint_queueSelector (toMTRBaseDevice device) endpoint (toNSObject queue)

-- | @- playWithParams:completionHandler:@
playWithParams_completionHandler :: (IsMTRBaseClusterMediaPlayback mtrBaseClusterMediaPlayback, IsMTRMediaPlaybackClusterPlayParams params) => mtrBaseClusterMediaPlayback -> params -> Ptr () -> IO ()
playWithParams_completionHandler mtrBaseClusterMediaPlayback params completionHandler =
  sendMessage mtrBaseClusterMediaPlayback playWithParams_completionHandlerSelector (toMTRMediaPlaybackClusterPlayParams params) completionHandler

-- | @- playWithCompletionHandler:@
playWithCompletionHandler :: IsMTRBaseClusterMediaPlayback mtrBaseClusterMediaPlayback => mtrBaseClusterMediaPlayback -> Ptr () -> IO ()
playWithCompletionHandler mtrBaseClusterMediaPlayback completionHandler =
  sendMessage mtrBaseClusterMediaPlayback playWithCompletionHandlerSelector completionHandler

-- | @- pauseWithParams:completionHandler:@
pauseWithParams_completionHandler :: (IsMTRBaseClusterMediaPlayback mtrBaseClusterMediaPlayback, IsMTRMediaPlaybackClusterPauseParams params) => mtrBaseClusterMediaPlayback -> params -> Ptr () -> IO ()
pauseWithParams_completionHandler mtrBaseClusterMediaPlayback params completionHandler =
  sendMessage mtrBaseClusterMediaPlayback pauseWithParams_completionHandlerSelector (toMTRMediaPlaybackClusterPauseParams params) completionHandler

-- | @- pauseWithCompletionHandler:@
pauseWithCompletionHandler :: IsMTRBaseClusterMediaPlayback mtrBaseClusterMediaPlayback => mtrBaseClusterMediaPlayback -> Ptr () -> IO ()
pauseWithCompletionHandler mtrBaseClusterMediaPlayback completionHandler =
  sendMessage mtrBaseClusterMediaPlayback pauseWithCompletionHandlerSelector completionHandler

-- | @- stopPlaybackWithParams:completionHandler:@
stopPlaybackWithParams_completionHandler :: (IsMTRBaseClusterMediaPlayback mtrBaseClusterMediaPlayback, IsMTRMediaPlaybackClusterStopPlaybackParams params) => mtrBaseClusterMediaPlayback -> params -> Ptr () -> IO ()
stopPlaybackWithParams_completionHandler mtrBaseClusterMediaPlayback params completionHandler =
  sendMessage mtrBaseClusterMediaPlayback stopPlaybackWithParams_completionHandlerSelector (toMTRMediaPlaybackClusterStopPlaybackParams params) completionHandler

-- | @- stopPlaybackWithCompletionHandler:@
stopPlaybackWithCompletionHandler :: IsMTRBaseClusterMediaPlayback mtrBaseClusterMediaPlayback => mtrBaseClusterMediaPlayback -> Ptr () -> IO ()
stopPlaybackWithCompletionHandler mtrBaseClusterMediaPlayback completionHandler =
  sendMessage mtrBaseClusterMediaPlayback stopPlaybackWithCompletionHandlerSelector completionHandler

-- | @- startOverWithParams:completionHandler:@
startOverWithParams_completionHandler :: (IsMTRBaseClusterMediaPlayback mtrBaseClusterMediaPlayback, IsMTRMediaPlaybackClusterStartOverParams params) => mtrBaseClusterMediaPlayback -> params -> Ptr () -> IO ()
startOverWithParams_completionHandler mtrBaseClusterMediaPlayback params completionHandler =
  sendMessage mtrBaseClusterMediaPlayback startOverWithParams_completionHandlerSelector (toMTRMediaPlaybackClusterStartOverParams params) completionHandler

-- | @- startOverWithCompletionHandler:@
startOverWithCompletionHandler :: IsMTRBaseClusterMediaPlayback mtrBaseClusterMediaPlayback => mtrBaseClusterMediaPlayback -> Ptr () -> IO ()
startOverWithCompletionHandler mtrBaseClusterMediaPlayback completionHandler =
  sendMessage mtrBaseClusterMediaPlayback startOverWithCompletionHandlerSelector completionHandler

-- | @- previousWithParams:completionHandler:@
previousWithParams_completionHandler :: (IsMTRBaseClusterMediaPlayback mtrBaseClusterMediaPlayback, IsMTRMediaPlaybackClusterPreviousParams params) => mtrBaseClusterMediaPlayback -> params -> Ptr () -> IO ()
previousWithParams_completionHandler mtrBaseClusterMediaPlayback params completionHandler =
  sendMessage mtrBaseClusterMediaPlayback previousWithParams_completionHandlerSelector (toMTRMediaPlaybackClusterPreviousParams params) completionHandler

-- | @- previousWithCompletionHandler:@
previousWithCompletionHandler :: IsMTRBaseClusterMediaPlayback mtrBaseClusterMediaPlayback => mtrBaseClusterMediaPlayback -> Ptr () -> IO ()
previousWithCompletionHandler mtrBaseClusterMediaPlayback completionHandler =
  sendMessage mtrBaseClusterMediaPlayback previousWithCompletionHandlerSelector completionHandler

-- | @- nextWithParams:completionHandler:@
nextWithParams_completionHandler :: (IsMTRBaseClusterMediaPlayback mtrBaseClusterMediaPlayback, IsMTRMediaPlaybackClusterNextParams params) => mtrBaseClusterMediaPlayback -> params -> Ptr () -> IO ()
nextWithParams_completionHandler mtrBaseClusterMediaPlayback params completionHandler =
  sendMessage mtrBaseClusterMediaPlayback nextWithParams_completionHandlerSelector (toMTRMediaPlaybackClusterNextParams params) completionHandler

-- | @- nextWithCompletionHandler:@
nextWithCompletionHandler :: IsMTRBaseClusterMediaPlayback mtrBaseClusterMediaPlayback => mtrBaseClusterMediaPlayback -> Ptr () -> IO ()
nextWithCompletionHandler mtrBaseClusterMediaPlayback completionHandler =
  sendMessage mtrBaseClusterMediaPlayback nextWithCompletionHandlerSelector completionHandler

-- | @- rewindWithParams:completionHandler:@
rewindWithParams_completionHandler :: (IsMTRBaseClusterMediaPlayback mtrBaseClusterMediaPlayback, IsMTRMediaPlaybackClusterRewindParams params) => mtrBaseClusterMediaPlayback -> params -> Ptr () -> IO ()
rewindWithParams_completionHandler mtrBaseClusterMediaPlayback params completionHandler =
  sendMessage mtrBaseClusterMediaPlayback rewindWithParams_completionHandlerSelector (toMTRMediaPlaybackClusterRewindParams params) completionHandler

-- | @- rewindWithCompletionHandler:@
rewindWithCompletionHandler :: IsMTRBaseClusterMediaPlayback mtrBaseClusterMediaPlayback => mtrBaseClusterMediaPlayback -> Ptr () -> IO ()
rewindWithCompletionHandler mtrBaseClusterMediaPlayback completionHandler =
  sendMessage mtrBaseClusterMediaPlayback rewindWithCompletionHandlerSelector completionHandler

-- | @- fastForwardWithParams:completionHandler:@
fastForwardWithParams_completionHandler :: (IsMTRBaseClusterMediaPlayback mtrBaseClusterMediaPlayback, IsMTRMediaPlaybackClusterFastForwardParams params) => mtrBaseClusterMediaPlayback -> params -> Ptr () -> IO ()
fastForwardWithParams_completionHandler mtrBaseClusterMediaPlayback params completionHandler =
  sendMessage mtrBaseClusterMediaPlayback fastForwardWithParams_completionHandlerSelector (toMTRMediaPlaybackClusterFastForwardParams params) completionHandler

-- | @- fastForwardWithCompletionHandler:@
fastForwardWithCompletionHandler :: IsMTRBaseClusterMediaPlayback mtrBaseClusterMediaPlayback => mtrBaseClusterMediaPlayback -> Ptr () -> IO ()
fastForwardWithCompletionHandler mtrBaseClusterMediaPlayback completionHandler =
  sendMessage mtrBaseClusterMediaPlayback fastForwardWithCompletionHandlerSelector completionHandler

-- | @- skipForwardWithParams:completionHandler:@
skipForwardWithParams_completionHandler :: (IsMTRBaseClusterMediaPlayback mtrBaseClusterMediaPlayback, IsMTRMediaPlaybackClusterSkipForwardParams params) => mtrBaseClusterMediaPlayback -> params -> Ptr () -> IO ()
skipForwardWithParams_completionHandler mtrBaseClusterMediaPlayback params completionHandler =
  sendMessage mtrBaseClusterMediaPlayback skipForwardWithParams_completionHandlerSelector (toMTRMediaPlaybackClusterSkipForwardParams params) completionHandler

-- | @- skipBackwardWithParams:completionHandler:@
skipBackwardWithParams_completionHandler :: (IsMTRBaseClusterMediaPlayback mtrBaseClusterMediaPlayback, IsMTRMediaPlaybackClusterSkipBackwardParams params) => mtrBaseClusterMediaPlayback -> params -> Ptr () -> IO ()
skipBackwardWithParams_completionHandler mtrBaseClusterMediaPlayback params completionHandler =
  sendMessage mtrBaseClusterMediaPlayback skipBackwardWithParams_completionHandlerSelector (toMTRMediaPlaybackClusterSkipBackwardParams params) completionHandler

-- | @- seekWithParams:completionHandler:@
seekWithParams_completionHandler :: (IsMTRBaseClusterMediaPlayback mtrBaseClusterMediaPlayback, IsMTRMediaPlaybackClusterSeekParams params) => mtrBaseClusterMediaPlayback -> params -> Ptr () -> IO ()
seekWithParams_completionHandler mtrBaseClusterMediaPlayback params completionHandler =
  sendMessage mtrBaseClusterMediaPlayback seekWithParams_completionHandlerSelector (toMTRMediaPlaybackClusterSeekParams params) completionHandler

-- | @- readAttributeCurrentStateWithCompletionHandler:@
readAttributeCurrentStateWithCompletionHandler :: IsMTRBaseClusterMediaPlayback mtrBaseClusterMediaPlayback => mtrBaseClusterMediaPlayback -> Ptr () -> IO ()
readAttributeCurrentStateWithCompletionHandler mtrBaseClusterMediaPlayback completionHandler =
  sendMessage mtrBaseClusterMediaPlayback readAttributeCurrentStateWithCompletionHandlerSelector completionHandler

-- | @- subscribeAttributeCurrentStateWithMinInterval:maxInterval:params:subscriptionEstablished:reportHandler:@
subscribeAttributeCurrentStateWithMinInterval_maxInterval_params_subscriptionEstablished_reportHandler :: (IsMTRBaseClusterMediaPlayback mtrBaseClusterMediaPlayback, IsNSNumber minInterval, IsNSNumber maxInterval, IsMTRSubscribeParams params) => mtrBaseClusterMediaPlayback -> minInterval -> maxInterval -> params -> Ptr () -> Ptr () -> IO ()
subscribeAttributeCurrentStateWithMinInterval_maxInterval_params_subscriptionEstablished_reportHandler mtrBaseClusterMediaPlayback minInterval maxInterval params subscriptionEstablishedHandler reportHandler =
  sendMessage mtrBaseClusterMediaPlayback subscribeAttributeCurrentStateWithMinInterval_maxInterval_params_subscriptionEstablished_reportHandlerSelector (toNSNumber minInterval) (toNSNumber maxInterval) (toMTRSubscribeParams params) subscriptionEstablishedHandler reportHandler

-- | @+ readAttributeCurrentStateWithAttributeCache:endpoint:queue:completionHandler:@
readAttributeCurrentStateWithAttributeCache_endpoint_queue_completionHandler :: (IsMTRAttributeCacheContainer attributeCacheContainer, IsNSNumber endpoint, IsNSObject queue) => attributeCacheContainer -> endpoint -> queue -> Ptr () -> IO ()
readAttributeCurrentStateWithAttributeCache_endpoint_queue_completionHandler attributeCacheContainer endpoint queue completionHandler =
  do
    cls' <- getRequiredClass "MTRBaseClusterMediaPlayback"
    sendClassMessage cls' readAttributeCurrentStateWithAttributeCache_endpoint_queue_completionHandlerSelector (toMTRAttributeCacheContainer attributeCacheContainer) (toNSNumber endpoint) (toNSObject queue) completionHandler

-- | @- readAttributeStartTimeWithCompletionHandler:@
readAttributeStartTimeWithCompletionHandler :: IsMTRBaseClusterMediaPlayback mtrBaseClusterMediaPlayback => mtrBaseClusterMediaPlayback -> Ptr () -> IO ()
readAttributeStartTimeWithCompletionHandler mtrBaseClusterMediaPlayback completionHandler =
  sendMessage mtrBaseClusterMediaPlayback readAttributeStartTimeWithCompletionHandlerSelector completionHandler

-- | @- subscribeAttributeStartTimeWithMinInterval:maxInterval:params:subscriptionEstablished:reportHandler:@
subscribeAttributeStartTimeWithMinInterval_maxInterval_params_subscriptionEstablished_reportHandler :: (IsMTRBaseClusterMediaPlayback mtrBaseClusterMediaPlayback, IsNSNumber minInterval, IsNSNumber maxInterval, IsMTRSubscribeParams params) => mtrBaseClusterMediaPlayback -> minInterval -> maxInterval -> params -> Ptr () -> Ptr () -> IO ()
subscribeAttributeStartTimeWithMinInterval_maxInterval_params_subscriptionEstablished_reportHandler mtrBaseClusterMediaPlayback minInterval maxInterval params subscriptionEstablishedHandler reportHandler =
  sendMessage mtrBaseClusterMediaPlayback subscribeAttributeStartTimeWithMinInterval_maxInterval_params_subscriptionEstablished_reportHandlerSelector (toNSNumber minInterval) (toNSNumber maxInterval) (toMTRSubscribeParams params) subscriptionEstablishedHandler reportHandler

-- | @+ readAttributeStartTimeWithAttributeCache:endpoint:queue:completionHandler:@
readAttributeStartTimeWithAttributeCache_endpoint_queue_completionHandler :: (IsMTRAttributeCacheContainer attributeCacheContainer, IsNSNumber endpoint, IsNSObject queue) => attributeCacheContainer -> endpoint -> queue -> Ptr () -> IO ()
readAttributeStartTimeWithAttributeCache_endpoint_queue_completionHandler attributeCacheContainer endpoint queue completionHandler =
  do
    cls' <- getRequiredClass "MTRBaseClusterMediaPlayback"
    sendClassMessage cls' readAttributeStartTimeWithAttributeCache_endpoint_queue_completionHandlerSelector (toMTRAttributeCacheContainer attributeCacheContainer) (toNSNumber endpoint) (toNSObject queue) completionHandler

-- | @- readAttributeDurationWithCompletionHandler:@
readAttributeDurationWithCompletionHandler :: IsMTRBaseClusterMediaPlayback mtrBaseClusterMediaPlayback => mtrBaseClusterMediaPlayback -> Ptr () -> IO ()
readAttributeDurationWithCompletionHandler mtrBaseClusterMediaPlayback completionHandler =
  sendMessage mtrBaseClusterMediaPlayback readAttributeDurationWithCompletionHandlerSelector completionHandler

-- | @- subscribeAttributeDurationWithMinInterval:maxInterval:params:subscriptionEstablished:reportHandler:@
subscribeAttributeDurationWithMinInterval_maxInterval_params_subscriptionEstablished_reportHandler :: (IsMTRBaseClusterMediaPlayback mtrBaseClusterMediaPlayback, IsNSNumber minInterval, IsNSNumber maxInterval, IsMTRSubscribeParams params) => mtrBaseClusterMediaPlayback -> minInterval -> maxInterval -> params -> Ptr () -> Ptr () -> IO ()
subscribeAttributeDurationWithMinInterval_maxInterval_params_subscriptionEstablished_reportHandler mtrBaseClusterMediaPlayback minInterval maxInterval params subscriptionEstablishedHandler reportHandler =
  sendMessage mtrBaseClusterMediaPlayback subscribeAttributeDurationWithMinInterval_maxInterval_params_subscriptionEstablished_reportHandlerSelector (toNSNumber minInterval) (toNSNumber maxInterval) (toMTRSubscribeParams params) subscriptionEstablishedHandler reportHandler

-- | @+ readAttributeDurationWithAttributeCache:endpoint:queue:completionHandler:@
readAttributeDurationWithAttributeCache_endpoint_queue_completionHandler :: (IsMTRAttributeCacheContainer attributeCacheContainer, IsNSNumber endpoint, IsNSObject queue) => attributeCacheContainer -> endpoint -> queue -> Ptr () -> IO ()
readAttributeDurationWithAttributeCache_endpoint_queue_completionHandler attributeCacheContainer endpoint queue completionHandler =
  do
    cls' <- getRequiredClass "MTRBaseClusterMediaPlayback"
    sendClassMessage cls' readAttributeDurationWithAttributeCache_endpoint_queue_completionHandlerSelector (toMTRAttributeCacheContainer attributeCacheContainer) (toNSNumber endpoint) (toNSObject queue) completionHandler

-- | @- readAttributeSampledPositionWithCompletionHandler:@
readAttributeSampledPositionWithCompletionHandler :: IsMTRBaseClusterMediaPlayback mtrBaseClusterMediaPlayback => mtrBaseClusterMediaPlayback -> Ptr () -> IO ()
readAttributeSampledPositionWithCompletionHandler mtrBaseClusterMediaPlayback completionHandler =
  sendMessage mtrBaseClusterMediaPlayback readAttributeSampledPositionWithCompletionHandlerSelector completionHandler

-- | @- subscribeAttributeSampledPositionWithMinInterval:maxInterval:params:subscriptionEstablished:reportHandler:@
subscribeAttributeSampledPositionWithMinInterval_maxInterval_params_subscriptionEstablished_reportHandler :: (IsMTRBaseClusterMediaPlayback mtrBaseClusterMediaPlayback, IsNSNumber minInterval, IsNSNumber maxInterval, IsMTRSubscribeParams params) => mtrBaseClusterMediaPlayback -> minInterval -> maxInterval -> params -> Ptr () -> Ptr () -> IO ()
subscribeAttributeSampledPositionWithMinInterval_maxInterval_params_subscriptionEstablished_reportHandler mtrBaseClusterMediaPlayback minInterval maxInterval params subscriptionEstablishedHandler reportHandler =
  sendMessage mtrBaseClusterMediaPlayback subscribeAttributeSampledPositionWithMinInterval_maxInterval_params_subscriptionEstablished_reportHandlerSelector (toNSNumber minInterval) (toNSNumber maxInterval) (toMTRSubscribeParams params) subscriptionEstablishedHandler reportHandler

-- | @+ readAttributeSampledPositionWithAttributeCache:endpoint:queue:completionHandler:@
readAttributeSampledPositionWithAttributeCache_endpoint_queue_completionHandler :: (IsMTRAttributeCacheContainer attributeCacheContainer, IsNSNumber endpoint, IsNSObject queue) => attributeCacheContainer -> endpoint -> queue -> Ptr () -> IO ()
readAttributeSampledPositionWithAttributeCache_endpoint_queue_completionHandler attributeCacheContainer endpoint queue completionHandler =
  do
    cls' <- getRequiredClass "MTRBaseClusterMediaPlayback"
    sendClassMessage cls' readAttributeSampledPositionWithAttributeCache_endpoint_queue_completionHandlerSelector (toMTRAttributeCacheContainer attributeCacheContainer) (toNSNumber endpoint) (toNSObject queue) completionHandler

-- | @- readAttributePlaybackSpeedWithCompletionHandler:@
readAttributePlaybackSpeedWithCompletionHandler :: IsMTRBaseClusterMediaPlayback mtrBaseClusterMediaPlayback => mtrBaseClusterMediaPlayback -> Ptr () -> IO ()
readAttributePlaybackSpeedWithCompletionHandler mtrBaseClusterMediaPlayback completionHandler =
  sendMessage mtrBaseClusterMediaPlayback readAttributePlaybackSpeedWithCompletionHandlerSelector completionHandler

-- | @- subscribeAttributePlaybackSpeedWithMinInterval:maxInterval:params:subscriptionEstablished:reportHandler:@
subscribeAttributePlaybackSpeedWithMinInterval_maxInterval_params_subscriptionEstablished_reportHandler :: (IsMTRBaseClusterMediaPlayback mtrBaseClusterMediaPlayback, IsNSNumber minInterval, IsNSNumber maxInterval, IsMTRSubscribeParams params) => mtrBaseClusterMediaPlayback -> minInterval -> maxInterval -> params -> Ptr () -> Ptr () -> IO ()
subscribeAttributePlaybackSpeedWithMinInterval_maxInterval_params_subscriptionEstablished_reportHandler mtrBaseClusterMediaPlayback minInterval maxInterval params subscriptionEstablishedHandler reportHandler =
  sendMessage mtrBaseClusterMediaPlayback subscribeAttributePlaybackSpeedWithMinInterval_maxInterval_params_subscriptionEstablished_reportHandlerSelector (toNSNumber minInterval) (toNSNumber maxInterval) (toMTRSubscribeParams params) subscriptionEstablishedHandler reportHandler

-- | @+ readAttributePlaybackSpeedWithAttributeCache:endpoint:queue:completionHandler:@
readAttributePlaybackSpeedWithAttributeCache_endpoint_queue_completionHandler :: (IsMTRAttributeCacheContainer attributeCacheContainer, IsNSNumber endpoint, IsNSObject queue) => attributeCacheContainer -> endpoint -> queue -> Ptr () -> IO ()
readAttributePlaybackSpeedWithAttributeCache_endpoint_queue_completionHandler attributeCacheContainer endpoint queue completionHandler =
  do
    cls' <- getRequiredClass "MTRBaseClusterMediaPlayback"
    sendClassMessage cls' readAttributePlaybackSpeedWithAttributeCache_endpoint_queue_completionHandlerSelector (toMTRAttributeCacheContainer attributeCacheContainer) (toNSNumber endpoint) (toNSObject queue) completionHandler

-- | @- readAttributeSeekRangeEndWithCompletionHandler:@
readAttributeSeekRangeEndWithCompletionHandler :: IsMTRBaseClusterMediaPlayback mtrBaseClusterMediaPlayback => mtrBaseClusterMediaPlayback -> Ptr () -> IO ()
readAttributeSeekRangeEndWithCompletionHandler mtrBaseClusterMediaPlayback completionHandler =
  sendMessage mtrBaseClusterMediaPlayback readAttributeSeekRangeEndWithCompletionHandlerSelector completionHandler

-- | @- subscribeAttributeSeekRangeEndWithMinInterval:maxInterval:params:subscriptionEstablished:reportHandler:@
subscribeAttributeSeekRangeEndWithMinInterval_maxInterval_params_subscriptionEstablished_reportHandler :: (IsMTRBaseClusterMediaPlayback mtrBaseClusterMediaPlayback, IsNSNumber minInterval, IsNSNumber maxInterval, IsMTRSubscribeParams params) => mtrBaseClusterMediaPlayback -> minInterval -> maxInterval -> params -> Ptr () -> Ptr () -> IO ()
subscribeAttributeSeekRangeEndWithMinInterval_maxInterval_params_subscriptionEstablished_reportHandler mtrBaseClusterMediaPlayback minInterval maxInterval params subscriptionEstablishedHandler reportHandler =
  sendMessage mtrBaseClusterMediaPlayback subscribeAttributeSeekRangeEndWithMinInterval_maxInterval_params_subscriptionEstablished_reportHandlerSelector (toNSNumber minInterval) (toNSNumber maxInterval) (toMTRSubscribeParams params) subscriptionEstablishedHandler reportHandler

-- | @+ readAttributeSeekRangeEndWithAttributeCache:endpoint:queue:completionHandler:@
readAttributeSeekRangeEndWithAttributeCache_endpoint_queue_completionHandler :: (IsMTRAttributeCacheContainer attributeCacheContainer, IsNSNumber endpoint, IsNSObject queue) => attributeCacheContainer -> endpoint -> queue -> Ptr () -> IO ()
readAttributeSeekRangeEndWithAttributeCache_endpoint_queue_completionHandler attributeCacheContainer endpoint queue completionHandler =
  do
    cls' <- getRequiredClass "MTRBaseClusterMediaPlayback"
    sendClassMessage cls' readAttributeSeekRangeEndWithAttributeCache_endpoint_queue_completionHandlerSelector (toMTRAttributeCacheContainer attributeCacheContainer) (toNSNumber endpoint) (toNSObject queue) completionHandler

-- | @- readAttributeSeekRangeStartWithCompletionHandler:@
readAttributeSeekRangeStartWithCompletionHandler :: IsMTRBaseClusterMediaPlayback mtrBaseClusterMediaPlayback => mtrBaseClusterMediaPlayback -> Ptr () -> IO ()
readAttributeSeekRangeStartWithCompletionHandler mtrBaseClusterMediaPlayback completionHandler =
  sendMessage mtrBaseClusterMediaPlayback readAttributeSeekRangeStartWithCompletionHandlerSelector completionHandler

-- | @- subscribeAttributeSeekRangeStartWithMinInterval:maxInterval:params:subscriptionEstablished:reportHandler:@
subscribeAttributeSeekRangeStartWithMinInterval_maxInterval_params_subscriptionEstablished_reportHandler :: (IsMTRBaseClusterMediaPlayback mtrBaseClusterMediaPlayback, IsNSNumber minInterval, IsNSNumber maxInterval, IsMTRSubscribeParams params) => mtrBaseClusterMediaPlayback -> minInterval -> maxInterval -> params -> Ptr () -> Ptr () -> IO ()
subscribeAttributeSeekRangeStartWithMinInterval_maxInterval_params_subscriptionEstablished_reportHandler mtrBaseClusterMediaPlayback minInterval maxInterval params subscriptionEstablishedHandler reportHandler =
  sendMessage mtrBaseClusterMediaPlayback subscribeAttributeSeekRangeStartWithMinInterval_maxInterval_params_subscriptionEstablished_reportHandlerSelector (toNSNumber minInterval) (toNSNumber maxInterval) (toMTRSubscribeParams params) subscriptionEstablishedHandler reportHandler

-- | @+ readAttributeSeekRangeStartWithAttributeCache:endpoint:queue:completionHandler:@
readAttributeSeekRangeStartWithAttributeCache_endpoint_queue_completionHandler :: (IsMTRAttributeCacheContainer attributeCacheContainer, IsNSNumber endpoint, IsNSObject queue) => attributeCacheContainer -> endpoint -> queue -> Ptr () -> IO ()
readAttributeSeekRangeStartWithAttributeCache_endpoint_queue_completionHandler attributeCacheContainer endpoint queue completionHandler =
  do
    cls' <- getRequiredClass "MTRBaseClusterMediaPlayback"
    sendClassMessage cls' readAttributeSeekRangeStartWithAttributeCache_endpoint_queue_completionHandlerSelector (toMTRAttributeCacheContainer attributeCacheContainer) (toNSNumber endpoint) (toNSObject queue) completionHandler

-- | @- readAttributeGeneratedCommandListWithCompletionHandler:@
readAttributeGeneratedCommandListWithCompletionHandler :: IsMTRBaseClusterMediaPlayback mtrBaseClusterMediaPlayback => mtrBaseClusterMediaPlayback -> Ptr () -> IO ()
readAttributeGeneratedCommandListWithCompletionHandler mtrBaseClusterMediaPlayback completionHandler =
  sendMessage mtrBaseClusterMediaPlayback readAttributeGeneratedCommandListWithCompletionHandlerSelector completionHandler

-- | @- subscribeAttributeGeneratedCommandListWithMinInterval:maxInterval:params:subscriptionEstablished:reportHandler:@
subscribeAttributeGeneratedCommandListWithMinInterval_maxInterval_params_subscriptionEstablished_reportHandler :: (IsMTRBaseClusterMediaPlayback mtrBaseClusterMediaPlayback, IsNSNumber minInterval, IsNSNumber maxInterval, IsMTRSubscribeParams params) => mtrBaseClusterMediaPlayback -> minInterval -> maxInterval -> params -> Ptr () -> Ptr () -> IO ()
subscribeAttributeGeneratedCommandListWithMinInterval_maxInterval_params_subscriptionEstablished_reportHandler mtrBaseClusterMediaPlayback minInterval maxInterval params subscriptionEstablishedHandler reportHandler =
  sendMessage mtrBaseClusterMediaPlayback subscribeAttributeGeneratedCommandListWithMinInterval_maxInterval_params_subscriptionEstablished_reportHandlerSelector (toNSNumber minInterval) (toNSNumber maxInterval) (toMTRSubscribeParams params) subscriptionEstablishedHandler reportHandler

-- | @+ readAttributeGeneratedCommandListWithAttributeCache:endpoint:queue:completionHandler:@
readAttributeGeneratedCommandListWithAttributeCache_endpoint_queue_completionHandler :: (IsMTRAttributeCacheContainer attributeCacheContainer, IsNSNumber endpoint, IsNSObject queue) => attributeCacheContainer -> endpoint -> queue -> Ptr () -> IO ()
readAttributeGeneratedCommandListWithAttributeCache_endpoint_queue_completionHandler attributeCacheContainer endpoint queue completionHandler =
  do
    cls' <- getRequiredClass "MTRBaseClusterMediaPlayback"
    sendClassMessage cls' readAttributeGeneratedCommandListWithAttributeCache_endpoint_queue_completionHandlerSelector (toMTRAttributeCacheContainer attributeCacheContainer) (toNSNumber endpoint) (toNSObject queue) completionHandler

-- | @- readAttributeAcceptedCommandListWithCompletionHandler:@
readAttributeAcceptedCommandListWithCompletionHandler :: IsMTRBaseClusterMediaPlayback mtrBaseClusterMediaPlayback => mtrBaseClusterMediaPlayback -> Ptr () -> IO ()
readAttributeAcceptedCommandListWithCompletionHandler mtrBaseClusterMediaPlayback completionHandler =
  sendMessage mtrBaseClusterMediaPlayback readAttributeAcceptedCommandListWithCompletionHandlerSelector completionHandler

-- | @- subscribeAttributeAcceptedCommandListWithMinInterval:maxInterval:params:subscriptionEstablished:reportHandler:@
subscribeAttributeAcceptedCommandListWithMinInterval_maxInterval_params_subscriptionEstablished_reportHandler :: (IsMTRBaseClusterMediaPlayback mtrBaseClusterMediaPlayback, IsNSNumber minInterval, IsNSNumber maxInterval, IsMTRSubscribeParams params) => mtrBaseClusterMediaPlayback -> minInterval -> maxInterval -> params -> Ptr () -> Ptr () -> IO ()
subscribeAttributeAcceptedCommandListWithMinInterval_maxInterval_params_subscriptionEstablished_reportHandler mtrBaseClusterMediaPlayback minInterval maxInterval params subscriptionEstablishedHandler reportHandler =
  sendMessage mtrBaseClusterMediaPlayback subscribeAttributeAcceptedCommandListWithMinInterval_maxInterval_params_subscriptionEstablished_reportHandlerSelector (toNSNumber minInterval) (toNSNumber maxInterval) (toMTRSubscribeParams params) subscriptionEstablishedHandler reportHandler

-- | @+ readAttributeAcceptedCommandListWithAttributeCache:endpoint:queue:completionHandler:@
readAttributeAcceptedCommandListWithAttributeCache_endpoint_queue_completionHandler :: (IsMTRAttributeCacheContainer attributeCacheContainer, IsNSNumber endpoint, IsNSObject queue) => attributeCacheContainer -> endpoint -> queue -> Ptr () -> IO ()
readAttributeAcceptedCommandListWithAttributeCache_endpoint_queue_completionHandler attributeCacheContainer endpoint queue completionHandler =
  do
    cls' <- getRequiredClass "MTRBaseClusterMediaPlayback"
    sendClassMessage cls' readAttributeAcceptedCommandListWithAttributeCache_endpoint_queue_completionHandlerSelector (toMTRAttributeCacheContainer attributeCacheContainer) (toNSNumber endpoint) (toNSObject queue) completionHandler

-- | @- readAttributeAttributeListWithCompletionHandler:@
readAttributeAttributeListWithCompletionHandler :: IsMTRBaseClusterMediaPlayback mtrBaseClusterMediaPlayback => mtrBaseClusterMediaPlayback -> Ptr () -> IO ()
readAttributeAttributeListWithCompletionHandler mtrBaseClusterMediaPlayback completionHandler =
  sendMessage mtrBaseClusterMediaPlayback readAttributeAttributeListWithCompletionHandlerSelector completionHandler

-- | @- subscribeAttributeAttributeListWithMinInterval:maxInterval:params:subscriptionEstablished:reportHandler:@
subscribeAttributeAttributeListWithMinInterval_maxInterval_params_subscriptionEstablished_reportHandler :: (IsMTRBaseClusterMediaPlayback mtrBaseClusterMediaPlayback, IsNSNumber minInterval, IsNSNumber maxInterval, IsMTRSubscribeParams params) => mtrBaseClusterMediaPlayback -> minInterval -> maxInterval -> params -> Ptr () -> Ptr () -> IO ()
subscribeAttributeAttributeListWithMinInterval_maxInterval_params_subscriptionEstablished_reportHandler mtrBaseClusterMediaPlayback minInterval maxInterval params subscriptionEstablishedHandler reportHandler =
  sendMessage mtrBaseClusterMediaPlayback subscribeAttributeAttributeListWithMinInterval_maxInterval_params_subscriptionEstablished_reportHandlerSelector (toNSNumber minInterval) (toNSNumber maxInterval) (toMTRSubscribeParams params) subscriptionEstablishedHandler reportHandler

-- | @+ readAttributeAttributeListWithAttributeCache:endpoint:queue:completionHandler:@
readAttributeAttributeListWithAttributeCache_endpoint_queue_completionHandler :: (IsMTRAttributeCacheContainer attributeCacheContainer, IsNSNumber endpoint, IsNSObject queue) => attributeCacheContainer -> endpoint -> queue -> Ptr () -> IO ()
readAttributeAttributeListWithAttributeCache_endpoint_queue_completionHandler attributeCacheContainer endpoint queue completionHandler =
  do
    cls' <- getRequiredClass "MTRBaseClusterMediaPlayback"
    sendClassMessage cls' readAttributeAttributeListWithAttributeCache_endpoint_queue_completionHandlerSelector (toMTRAttributeCacheContainer attributeCacheContainer) (toNSNumber endpoint) (toNSObject queue) completionHandler

-- | @- readAttributeFeatureMapWithCompletionHandler:@
readAttributeFeatureMapWithCompletionHandler :: IsMTRBaseClusterMediaPlayback mtrBaseClusterMediaPlayback => mtrBaseClusterMediaPlayback -> Ptr () -> IO ()
readAttributeFeatureMapWithCompletionHandler mtrBaseClusterMediaPlayback completionHandler =
  sendMessage mtrBaseClusterMediaPlayback readAttributeFeatureMapWithCompletionHandlerSelector completionHandler

-- | @- subscribeAttributeFeatureMapWithMinInterval:maxInterval:params:subscriptionEstablished:reportHandler:@
subscribeAttributeFeatureMapWithMinInterval_maxInterval_params_subscriptionEstablished_reportHandler :: (IsMTRBaseClusterMediaPlayback mtrBaseClusterMediaPlayback, IsNSNumber minInterval, IsNSNumber maxInterval, IsMTRSubscribeParams params) => mtrBaseClusterMediaPlayback -> minInterval -> maxInterval -> params -> Ptr () -> Ptr () -> IO ()
subscribeAttributeFeatureMapWithMinInterval_maxInterval_params_subscriptionEstablished_reportHandler mtrBaseClusterMediaPlayback minInterval maxInterval params subscriptionEstablishedHandler reportHandler =
  sendMessage mtrBaseClusterMediaPlayback subscribeAttributeFeatureMapWithMinInterval_maxInterval_params_subscriptionEstablished_reportHandlerSelector (toNSNumber minInterval) (toNSNumber maxInterval) (toMTRSubscribeParams params) subscriptionEstablishedHandler reportHandler

-- | @+ readAttributeFeatureMapWithAttributeCache:endpoint:queue:completionHandler:@
readAttributeFeatureMapWithAttributeCache_endpoint_queue_completionHandler :: (IsMTRAttributeCacheContainer attributeCacheContainer, IsNSNumber endpoint, IsNSObject queue) => attributeCacheContainer -> endpoint -> queue -> Ptr () -> IO ()
readAttributeFeatureMapWithAttributeCache_endpoint_queue_completionHandler attributeCacheContainer endpoint queue completionHandler =
  do
    cls' <- getRequiredClass "MTRBaseClusterMediaPlayback"
    sendClassMessage cls' readAttributeFeatureMapWithAttributeCache_endpoint_queue_completionHandlerSelector (toMTRAttributeCacheContainer attributeCacheContainer) (toNSNumber endpoint) (toNSObject queue) completionHandler

-- | @- readAttributeClusterRevisionWithCompletionHandler:@
readAttributeClusterRevisionWithCompletionHandler :: IsMTRBaseClusterMediaPlayback mtrBaseClusterMediaPlayback => mtrBaseClusterMediaPlayback -> Ptr () -> IO ()
readAttributeClusterRevisionWithCompletionHandler mtrBaseClusterMediaPlayback completionHandler =
  sendMessage mtrBaseClusterMediaPlayback readAttributeClusterRevisionWithCompletionHandlerSelector completionHandler

-- | @- subscribeAttributeClusterRevisionWithMinInterval:maxInterval:params:subscriptionEstablished:reportHandler:@
subscribeAttributeClusterRevisionWithMinInterval_maxInterval_params_subscriptionEstablished_reportHandler :: (IsMTRBaseClusterMediaPlayback mtrBaseClusterMediaPlayback, IsNSNumber minInterval, IsNSNumber maxInterval, IsMTRSubscribeParams params) => mtrBaseClusterMediaPlayback -> minInterval -> maxInterval -> params -> Ptr () -> Ptr () -> IO ()
subscribeAttributeClusterRevisionWithMinInterval_maxInterval_params_subscriptionEstablished_reportHandler mtrBaseClusterMediaPlayback minInterval maxInterval params subscriptionEstablishedHandler reportHandler =
  sendMessage mtrBaseClusterMediaPlayback subscribeAttributeClusterRevisionWithMinInterval_maxInterval_params_subscriptionEstablished_reportHandlerSelector (toNSNumber minInterval) (toNSNumber maxInterval) (toMTRSubscribeParams params) subscriptionEstablishedHandler reportHandler

-- | @+ readAttributeClusterRevisionWithAttributeCache:endpoint:queue:completionHandler:@
readAttributeClusterRevisionWithAttributeCache_endpoint_queue_completionHandler :: (IsMTRAttributeCacheContainer attributeCacheContainer, IsNSNumber endpoint, IsNSObject queue) => attributeCacheContainer -> endpoint -> queue -> Ptr () -> IO ()
readAttributeClusterRevisionWithAttributeCache_endpoint_queue_completionHandler attributeCacheContainer endpoint queue completionHandler =
  do
    cls' <- getRequiredClass "MTRBaseClusterMediaPlayback"
    sendClassMessage cls' readAttributeClusterRevisionWithAttributeCache_endpoint_queue_completionHandlerSelector (toMTRAttributeCacheContainer attributeCacheContainer) (toNSNumber endpoint) (toNSObject queue) completionHandler

-- | For all instance methods (reads, writes, commands) that take a completion, the completion will be called on the provided queue.
--
-- ObjC selector: @- initWithDevice:endpointID:queue:@
initWithDevice_endpointID_queue :: (IsMTRBaseClusterMediaPlayback mtrBaseClusterMediaPlayback, IsMTRBaseDevice device, IsNSNumber endpointID, IsNSObject queue) => mtrBaseClusterMediaPlayback -> device -> endpointID -> queue -> IO (Id MTRBaseClusterMediaPlayback)
initWithDevice_endpointID_queue mtrBaseClusterMediaPlayback device endpointID queue =
  sendOwnedMessage mtrBaseClusterMediaPlayback initWithDevice_endpointID_queueSelector (toMTRBaseDevice device) (toNSNumber endpointID) (toNSObject queue)

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @playWithParams:completion:@
playWithParams_completionSelector :: Selector '[Id MTRMediaPlaybackClusterPlayParams, Ptr ()] ()
playWithParams_completionSelector = mkSelector "playWithParams:completion:"

-- | @Selector@ for @playWithCompletion:@
playWithCompletionSelector :: Selector '[Ptr ()] ()
playWithCompletionSelector = mkSelector "playWithCompletion:"

-- | @Selector@ for @pauseWithParams:completion:@
pauseWithParams_completionSelector :: Selector '[Id MTRMediaPlaybackClusterPauseParams, Ptr ()] ()
pauseWithParams_completionSelector = mkSelector "pauseWithParams:completion:"

-- | @Selector@ for @pauseWithCompletion:@
pauseWithCompletionSelector :: Selector '[Ptr ()] ()
pauseWithCompletionSelector = mkSelector "pauseWithCompletion:"

-- | @Selector@ for @stopWithParams:completion:@
stopWithParams_completionSelector :: Selector '[Id MTRMediaPlaybackClusterStopParams, Ptr ()] ()
stopWithParams_completionSelector = mkSelector "stopWithParams:completion:"

-- | @Selector@ for @stopWithCompletion:@
stopWithCompletionSelector :: Selector '[Ptr ()] ()
stopWithCompletionSelector = mkSelector "stopWithCompletion:"

-- | @Selector@ for @startOverWithParams:completion:@
startOverWithParams_completionSelector :: Selector '[Id MTRMediaPlaybackClusterStartOverParams, Ptr ()] ()
startOverWithParams_completionSelector = mkSelector "startOverWithParams:completion:"

-- | @Selector@ for @startOverWithCompletion:@
startOverWithCompletionSelector :: Selector '[Ptr ()] ()
startOverWithCompletionSelector = mkSelector "startOverWithCompletion:"

-- | @Selector@ for @previousWithParams:completion:@
previousWithParams_completionSelector :: Selector '[Id MTRMediaPlaybackClusterPreviousParams, Ptr ()] ()
previousWithParams_completionSelector = mkSelector "previousWithParams:completion:"

-- | @Selector@ for @previousWithCompletion:@
previousWithCompletionSelector :: Selector '[Ptr ()] ()
previousWithCompletionSelector = mkSelector "previousWithCompletion:"

-- | @Selector@ for @nextWithParams:completion:@
nextWithParams_completionSelector :: Selector '[Id MTRMediaPlaybackClusterNextParams, Ptr ()] ()
nextWithParams_completionSelector = mkSelector "nextWithParams:completion:"

-- | @Selector@ for @nextWithCompletion:@
nextWithCompletionSelector :: Selector '[Ptr ()] ()
nextWithCompletionSelector = mkSelector "nextWithCompletion:"

-- | @Selector@ for @rewindWithParams:completion:@
rewindWithParams_completionSelector :: Selector '[Id MTRMediaPlaybackClusterRewindParams, Ptr ()] ()
rewindWithParams_completionSelector = mkSelector "rewindWithParams:completion:"

-- | @Selector@ for @rewindWithCompletion:@
rewindWithCompletionSelector :: Selector '[Ptr ()] ()
rewindWithCompletionSelector = mkSelector "rewindWithCompletion:"

-- | @Selector@ for @fastForwardWithParams:completion:@
fastForwardWithParams_completionSelector :: Selector '[Id MTRMediaPlaybackClusterFastForwardParams, Ptr ()] ()
fastForwardWithParams_completionSelector = mkSelector "fastForwardWithParams:completion:"

-- | @Selector@ for @fastForwardWithCompletion:@
fastForwardWithCompletionSelector :: Selector '[Ptr ()] ()
fastForwardWithCompletionSelector = mkSelector "fastForwardWithCompletion:"

-- | @Selector@ for @skipForwardWithParams:completion:@
skipForwardWithParams_completionSelector :: Selector '[Id MTRMediaPlaybackClusterSkipForwardParams, Ptr ()] ()
skipForwardWithParams_completionSelector = mkSelector "skipForwardWithParams:completion:"

-- | @Selector@ for @skipBackwardWithParams:completion:@
skipBackwardWithParams_completionSelector :: Selector '[Id MTRMediaPlaybackClusterSkipBackwardParams, Ptr ()] ()
skipBackwardWithParams_completionSelector = mkSelector "skipBackwardWithParams:completion:"

-- | @Selector@ for @seekWithParams:completion:@
seekWithParams_completionSelector :: Selector '[Id MTRMediaPlaybackClusterSeekParams, Ptr ()] ()
seekWithParams_completionSelector = mkSelector "seekWithParams:completion:"

-- | @Selector@ for @activateAudioTrackWithParams:completion:@
activateAudioTrackWithParams_completionSelector :: Selector '[Id MTRMediaPlaybackClusterActivateAudioTrackParams, Ptr ()] ()
activateAudioTrackWithParams_completionSelector = mkSelector "activateAudioTrackWithParams:completion:"

-- | @Selector@ for @activateTextTrackWithParams:completion:@
activateTextTrackWithParams_completionSelector :: Selector '[Id MTRMediaPlaybackClusterActivateTextTrackParams, Ptr ()] ()
activateTextTrackWithParams_completionSelector = mkSelector "activateTextTrackWithParams:completion:"

-- | @Selector@ for @deactivateTextTrackWithParams:completion:@
deactivateTextTrackWithParams_completionSelector :: Selector '[Id MTRMediaPlaybackClusterDeactivateTextTrackParams, Ptr ()] ()
deactivateTextTrackWithParams_completionSelector = mkSelector "deactivateTextTrackWithParams:completion:"

-- | @Selector@ for @deactivateTextTrackWithCompletion:@
deactivateTextTrackWithCompletionSelector :: Selector '[Ptr ()] ()
deactivateTextTrackWithCompletionSelector = mkSelector "deactivateTextTrackWithCompletion:"

-- | @Selector@ for @readAttributeCurrentStateWithCompletion:@
readAttributeCurrentStateWithCompletionSelector :: Selector '[Ptr ()] ()
readAttributeCurrentStateWithCompletionSelector = mkSelector "readAttributeCurrentStateWithCompletion:"

-- | @Selector@ for @subscribeAttributeCurrentStateWithParams:subscriptionEstablished:reportHandler:@
subscribeAttributeCurrentStateWithParams_subscriptionEstablished_reportHandlerSelector :: Selector '[Id MTRSubscribeParams, Ptr (), Ptr ()] ()
subscribeAttributeCurrentStateWithParams_subscriptionEstablished_reportHandlerSelector = mkSelector "subscribeAttributeCurrentStateWithParams:subscriptionEstablished:reportHandler:"

-- | @Selector@ for @readAttributeCurrentStateWithClusterStateCache:endpoint:queue:completion:@
readAttributeCurrentStateWithClusterStateCache_endpoint_queue_completionSelector :: Selector '[Id MTRClusterStateCacheContainer, Id NSNumber, Id NSObject, Ptr ()] ()
readAttributeCurrentStateWithClusterStateCache_endpoint_queue_completionSelector = mkSelector "readAttributeCurrentStateWithClusterStateCache:endpoint:queue:completion:"

-- | @Selector@ for @readAttributeStartTimeWithCompletion:@
readAttributeStartTimeWithCompletionSelector :: Selector '[Ptr ()] ()
readAttributeStartTimeWithCompletionSelector = mkSelector "readAttributeStartTimeWithCompletion:"

-- | @Selector@ for @subscribeAttributeStartTimeWithParams:subscriptionEstablished:reportHandler:@
subscribeAttributeStartTimeWithParams_subscriptionEstablished_reportHandlerSelector :: Selector '[Id MTRSubscribeParams, Ptr (), Ptr ()] ()
subscribeAttributeStartTimeWithParams_subscriptionEstablished_reportHandlerSelector = mkSelector "subscribeAttributeStartTimeWithParams:subscriptionEstablished:reportHandler:"

-- | @Selector@ for @readAttributeStartTimeWithClusterStateCache:endpoint:queue:completion:@
readAttributeStartTimeWithClusterStateCache_endpoint_queue_completionSelector :: Selector '[Id MTRClusterStateCacheContainer, Id NSNumber, Id NSObject, Ptr ()] ()
readAttributeStartTimeWithClusterStateCache_endpoint_queue_completionSelector = mkSelector "readAttributeStartTimeWithClusterStateCache:endpoint:queue:completion:"

-- | @Selector@ for @readAttributeDurationWithCompletion:@
readAttributeDurationWithCompletionSelector :: Selector '[Ptr ()] ()
readAttributeDurationWithCompletionSelector = mkSelector "readAttributeDurationWithCompletion:"

-- | @Selector@ for @subscribeAttributeDurationWithParams:subscriptionEstablished:reportHandler:@
subscribeAttributeDurationWithParams_subscriptionEstablished_reportHandlerSelector :: Selector '[Id MTRSubscribeParams, Ptr (), Ptr ()] ()
subscribeAttributeDurationWithParams_subscriptionEstablished_reportHandlerSelector = mkSelector "subscribeAttributeDurationWithParams:subscriptionEstablished:reportHandler:"

-- | @Selector@ for @readAttributeDurationWithClusterStateCache:endpoint:queue:completion:@
readAttributeDurationWithClusterStateCache_endpoint_queue_completionSelector :: Selector '[Id MTRClusterStateCacheContainer, Id NSNumber, Id NSObject, Ptr ()] ()
readAttributeDurationWithClusterStateCache_endpoint_queue_completionSelector = mkSelector "readAttributeDurationWithClusterStateCache:endpoint:queue:completion:"

-- | @Selector@ for @readAttributeSampledPositionWithCompletion:@
readAttributeSampledPositionWithCompletionSelector :: Selector '[Ptr ()] ()
readAttributeSampledPositionWithCompletionSelector = mkSelector "readAttributeSampledPositionWithCompletion:"

-- | @Selector@ for @subscribeAttributeSampledPositionWithParams:subscriptionEstablished:reportHandler:@
subscribeAttributeSampledPositionWithParams_subscriptionEstablished_reportHandlerSelector :: Selector '[Id MTRSubscribeParams, Ptr (), Ptr ()] ()
subscribeAttributeSampledPositionWithParams_subscriptionEstablished_reportHandlerSelector = mkSelector "subscribeAttributeSampledPositionWithParams:subscriptionEstablished:reportHandler:"

-- | @Selector@ for @readAttributeSampledPositionWithClusterStateCache:endpoint:queue:completion:@
readAttributeSampledPositionWithClusterStateCache_endpoint_queue_completionSelector :: Selector '[Id MTRClusterStateCacheContainer, Id NSNumber, Id NSObject, Ptr ()] ()
readAttributeSampledPositionWithClusterStateCache_endpoint_queue_completionSelector = mkSelector "readAttributeSampledPositionWithClusterStateCache:endpoint:queue:completion:"

-- | @Selector@ for @readAttributePlaybackSpeedWithCompletion:@
readAttributePlaybackSpeedWithCompletionSelector :: Selector '[Ptr ()] ()
readAttributePlaybackSpeedWithCompletionSelector = mkSelector "readAttributePlaybackSpeedWithCompletion:"

-- | @Selector@ for @subscribeAttributePlaybackSpeedWithParams:subscriptionEstablished:reportHandler:@
subscribeAttributePlaybackSpeedWithParams_subscriptionEstablished_reportHandlerSelector :: Selector '[Id MTRSubscribeParams, Ptr (), Ptr ()] ()
subscribeAttributePlaybackSpeedWithParams_subscriptionEstablished_reportHandlerSelector = mkSelector "subscribeAttributePlaybackSpeedWithParams:subscriptionEstablished:reportHandler:"

-- | @Selector@ for @readAttributePlaybackSpeedWithClusterStateCache:endpoint:queue:completion:@
readAttributePlaybackSpeedWithClusterStateCache_endpoint_queue_completionSelector :: Selector '[Id MTRClusterStateCacheContainer, Id NSNumber, Id NSObject, Ptr ()] ()
readAttributePlaybackSpeedWithClusterStateCache_endpoint_queue_completionSelector = mkSelector "readAttributePlaybackSpeedWithClusterStateCache:endpoint:queue:completion:"

-- | @Selector@ for @readAttributeSeekRangeEndWithCompletion:@
readAttributeSeekRangeEndWithCompletionSelector :: Selector '[Ptr ()] ()
readAttributeSeekRangeEndWithCompletionSelector = mkSelector "readAttributeSeekRangeEndWithCompletion:"

-- | @Selector@ for @subscribeAttributeSeekRangeEndWithParams:subscriptionEstablished:reportHandler:@
subscribeAttributeSeekRangeEndWithParams_subscriptionEstablished_reportHandlerSelector :: Selector '[Id MTRSubscribeParams, Ptr (), Ptr ()] ()
subscribeAttributeSeekRangeEndWithParams_subscriptionEstablished_reportHandlerSelector = mkSelector "subscribeAttributeSeekRangeEndWithParams:subscriptionEstablished:reportHandler:"

-- | @Selector@ for @readAttributeSeekRangeEndWithClusterStateCache:endpoint:queue:completion:@
readAttributeSeekRangeEndWithClusterStateCache_endpoint_queue_completionSelector :: Selector '[Id MTRClusterStateCacheContainer, Id NSNumber, Id NSObject, Ptr ()] ()
readAttributeSeekRangeEndWithClusterStateCache_endpoint_queue_completionSelector = mkSelector "readAttributeSeekRangeEndWithClusterStateCache:endpoint:queue:completion:"

-- | @Selector@ for @readAttributeSeekRangeStartWithCompletion:@
readAttributeSeekRangeStartWithCompletionSelector :: Selector '[Ptr ()] ()
readAttributeSeekRangeStartWithCompletionSelector = mkSelector "readAttributeSeekRangeStartWithCompletion:"

-- | @Selector@ for @subscribeAttributeSeekRangeStartWithParams:subscriptionEstablished:reportHandler:@
subscribeAttributeSeekRangeStartWithParams_subscriptionEstablished_reportHandlerSelector :: Selector '[Id MTRSubscribeParams, Ptr (), Ptr ()] ()
subscribeAttributeSeekRangeStartWithParams_subscriptionEstablished_reportHandlerSelector = mkSelector "subscribeAttributeSeekRangeStartWithParams:subscriptionEstablished:reportHandler:"

-- | @Selector@ for @readAttributeSeekRangeStartWithClusterStateCache:endpoint:queue:completion:@
readAttributeSeekRangeStartWithClusterStateCache_endpoint_queue_completionSelector :: Selector '[Id MTRClusterStateCacheContainer, Id NSNumber, Id NSObject, Ptr ()] ()
readAttributeSeekRangeStartWithClusterStateCache_endpoint_queue_completionSelector = mkSelector "readAttributeSeekRangeStartWithClusterStateCache:endpoint:queue:completion:"

-- | @Selector@ for @readAttributeActiveAudioTrackWithCompletion:@
readAttributeActiveAudioTrackWithCompletionSelector :: Selector '[Ptr ()] ()
readAttributeActiveAudioTrackWithCompletionSelector = mkSelector "readAttributeActiveAudioTrackWithCompletion:"

-- | @Selector@ for @subscribeAttributeActiveAudioTrackWithParams:subscriptionEstablished:reportHandler:@
subscribeAttributeActiveAudioTrackWithParams_subscriptionEstablished_reportHandlerSelector :: Selector '[Id MTRSubscribeParams, Ptr (), Ptr ()] ()
subscribeAttributeActiveAudioTrackWithParams_subscriptionEstablished_reportHandlerSelector = mkSelector "subscribeAttributeActiveAudioTrackWithParams:subscriptionEstablished:reportHandler:"

-- | @Selector@ for @readAttributeActiveAudioTrackWithClusterStateCache:endpoint:queue:completion:@
readAttributeActiveAudioTrackWithClusterStateCache_endpoint_queue_completionSelector :: Selector '[Id MTRClusterStateCacheContainer, Id NSNumber, Id NSObject, Ptr ()] ()
readAttributeActiveAudioTrackWithClusterStateCache_endpoint_queue_completionSelector = mkSelector "readAttributeActiveAudioTrackWithClusterStateCache:endpoint:queue:completion:"

-- | @Selector@ for @readAttributeAvailableAudioTracksWithCompletion:@
readAttributeAvailableAudioTracksWithCompletionSelector :: Selector '[Ptr ()] ()
readAttributeAvailableAudioTracksWithCompletionSelector = mkSelector "readAttributeAvailableAudioTracksWithCompletion:"

-- | @Selector@ for @subscribeAttributeAvailableAudioTracksWithParams:subscriptionEstablished:reportHandler:@
subscribeAttributeAvailableAudioTracksWithParams_subscriptionEstablished_reportHandlerSelector :: Selector '[Id MTRSubscribeParams, Ptr (), Ptr ()] ()
subscribeAttributeAvailableAudioTracksWithParams_subscriptionEstablished_reportHandlerSelector = mkSelector "subscribeAttributeAvailableAudioTracksWithParams:subscriptionEstablished:reportHandler:"

-- | @Selector@ for @readAttributeAvailableAudioTracksWithClusterStateCache:endpoint:queue:completion:@
readAttributeAvailableAudioTracksWithClusterStateCache_endpoint_queue_completionSelector :: Selector '[Id MTRClusterStateCacheContainer, Id NSNumber, Id NSObject, Ptr ()] ()
readAttributeAvailableAudioTracksWithClusterStateCache_endpoint_queue_completionSelector = mkSelector "readAttributeAvailableAudioTracksWithClusterStateCache:endpoint:queue:completion:"

-- | @Selector@ for @readAttributeActiveTextTrackWithCompletion:@
readAttributeActiveTextTrackWithCompletionSelector :: Selector '[Ptr ()] ()
readAttributeActiveTextTrackWithCompletionSelector = mkSelector "readAttributeActiveTextTrackWithCompletion:"

-- | @Selector@ for @subscribeAttributeActiveTextTrackWithParams:subscriptionEstablished:reportHandler:@
subscribeAttributeActiveTextTrackWithParams_subscriptionEstablished_reportHandlerSelector :: Selector '[Id MTRSubscribeParams, Ptr (), Ptr ()] ()
subscribeAttributeActiveTextTrackWithParams_subscriptionEstablished_reportHandlerSelector = mkSelector "subscribeAttributeActiveTextTrackWithParams:subscriptionEstablished:reportHandler:"

-- | @Selector@ for @readAttributeActiveTextTrackWithClusterStateCache:endpoint:queue:completion:@
readAttributeActiveTextTrackWithClusterStateCache_endpoint_queue_completionSelector :: Selector '[Id MTRClusterStateCacheContainer, Id NSNumber, Id NSObject, Ptr ()] ()
readAttributeActiveTextTrackWithClusterStateCache_endpoint_queue_completionSelector = mkSelector "readAttributeActiveTextTrackWithClusterStateCache:endpoint:queue:completion:"

-- | @Selector@ for @readAttributeAvailableTextTracksWithCompletion:@
readAttributeAvailableTextTracksWithCompletionSelector :: Selector '[Ptr ()] ()
readAttributeAvailableTextTracksWithCompletionSelector = mkSelector "readAttributeAvailableTextTracksWithCompletion:"

-- | @Selector@ for @subscribeAttributeAvailableTextTracksWithParams:subscriptionEstablished:reportHandler:@
subscribeAttributeAvailableTextTracksWithParams_subscriptionEstablished_reportHandlerSelector :: Selector '[Id MTRSubscribeParams, Ptr (), Ptr ()] ()
subscribeAttributeAvailableTextTracksWithParams_subscriptionEstablished_reportHandlerSelector = mkSelector "subscribeAttributeAvailableTextTracksWithParams:subscriptionEstablished:reportHandler:"

-- | @Selector@ for @readAttributeAvailableTextTracksWithClusterStateCache:endpoint:queue:completion:@
readAttributeAvailableTextTracksWithClusterStateCache_endpoint_queue_completionSelector :: Selector '[Id MTRClusterStateCacheContainer, Id NSNumber, Id NSObject, Ptr ()] ()
readAttributeAvailableTextTracksWithClusterStateCache_endpoint_queue_completionSelector = mkSelector "readAttributeAvailableTextTracksWithClusterStateCache:endpoint:queue:completion:"

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
initSelector :: Selector '[] (Id MTRBaseClusterMediaPlayback)
initSelector = mkSelector "init"

-- | @Selector@ for @new@
newSelector :: Selector '[] (Id MTRBaseClusterMediaPlayback)
newSelector = mkSelector "new"

-- | @Selector@ for @initWithDevice:endpoint:queue:@
initWithDevice_endpoint_queueSelector :: Selector '[Id MTRBaseDevice, CUShort, Id NSObject] (Id MTRBaseClusterMediaPlayback)
initWithDevice_endpoint_queueSelector = mkSelector "initWithDevice:endpoint:queue:"

-- | @Selector@ for @playWithParams:completionHandler:@
playWithParams_completionHandlerSelector :: Selector '[Id MTRMediaPlaybackClusterPlayParams, Ptr ()] ()
playWithParams_completionHandlerSelector = mkSelector "playWithParams:completionHandler:"

-- | @Selector@ for @playWithCompletionHandler:@
playWithCompletionHandlerSelector :: Selector '[Ptr ()] ()
playWithCompletionHandlerSelector = mkSelector "playWithCompletionHandler:"

-- | @Selector@ for @pauseWithParams:completionHandler:@
pauseWithParams_completionHandlerSelector :: Selector '[Id MTRMediaPlaybackClusterPauseParams, Ptr ()] ()
pauseWithParams_completionHandlerSelector = mkSelector "pauseWithParams:completionHandler:"

-- | @Selector@ for @pauseWithCompletionHandler:@
pauseWithCompletionHandlerSelector :: Selector '[Ptr ()] ()
pauseWithCompletionHandlerSelector = mkSelector "pauseWithCompletionHandler:"

-- | @Selector@ for @stopPlaybackWithParams:completionHandler:@
stopPlaybackWithParams_completionHandlerSelector :: Selector '[Id MTRMediaPlaybackClusterStopPlaybackParams, Ptr ()] ()
stopPlaybackWithParams_completionHandlerSelector = mkSelector "stopPlaybackWithParams:completionHandler:"

-- | @Selector@ for @stopPlaybackWithCompletionHandler:@
stopPlaybackWithCompletionHandlerSelector :: Selector '[Ptr ()] ()
stopPlaybackWithCompletionHandlerSelector = mkSelector "stopPlaybackWithCompletionHandler:"

-- | @Selector@ for @startOverWithParams:completionHandler:@
startOverWithParams_completionHandlerSelector :: Selector '[Id MTRMediaPlaybackClusterStartOverParams, Ptr ()] ()
startOverWithParams_completionHandlerSelector = mkSelector "startOverWithParams:completionHandler:"

-- | @Selector@ for @startOverWithCompletionHandler:@
startOverWithCompletionHandlerSelector :: Selector '[Ptr ()] ()
startOverWithCompletionHandlerSelector = mkSelector "startOverWithCompletionHandler:"

-- | @Selector@ for @previousWithParams:completionHandler:@
previousWithParams_completionHandlerSelector :: Selector '[Id MTRMediaPlaybackClusterPreviousParams, Ptr ()] ()
previousWithParams_completionHandlerSelector = mkSelector "previousWithParams:completionHandler:"

-- | @Selector@ for @previousWithCompletionHandler:@
previousWithCompletionHandlerSelector :: Selector '[Ptr ()] ()
previousWithCompletionHandlerSelector = mkSelector "previousWithCompletionHandler:"

-- | @Selector@ for @nextWithParams:completionHandler:@
nextWithParams_completionHandlerSelector :: Selector '[Id MTRMediaPlaybackClusterNextParams, Ptr ()] ()
nextWithParams_completionHandlerSelector = mkSelector "nextWithParams:completionHandler:"

-- | @Selector@ for @nextWithCompletionHandler:@
nextWithCompletionHandlerSelector :: Selector '[Ptr ()] ()
nextWithCompletionHandlerSelector = mkSelector "nextWithCompletionHandler:"

-- | @Selector@ for @rewindWithParams:completionHandler:@
rewindWithParams_completionHandlerSelector :: Selector '[Id MTRMediaPlaybackClusterRewindParams, Ptr ()] ()
rewindWithParams_completionHandlerSelector = mkSelector "rewindWithParams:completionHandler:"

-- | @Selector@ for @rewindWithCompletionHandler:@
rewindWithCompletionHandlerSelector :: Selector '[Ptr ()] ()
rewindWithCompletionHandlerSelector = mkSelector "rewindWithCompletionHandler:"

-- | @Selector@ for @fastForwardWithParams:completionHandler:@
fastForwardWithParams_completionHandlerSelector :: Selector '[Id MTRMediaPlaybackClusterFastForwardParams, Ptr ()] ()
fastForwardWithParams_completionHandlerSelector = mkSelector "fastForwardWithParams:completionHandler:"

-- | @Selector@ for @fastForwardWithCompletionHandler:@
fastForwardWithCompletionHandlerSelector :: Selector '[Ptr ()] ()
fastForwardWithCompletionHandlerSelector = mkSelector "fastForwardWithCompletionHandler:"

-- | @Selector@ for @skipForwardWithParams:completionHandler:@
skipForwardWithParams_completionHandlerSelector :: Selector '[Id MTRMediaPlaybackClusterSkipForwardParams, Ptr ()] ()
skipForwardWithParams_completionHandlerSelector = mkSelector "skipForwardWithParams:completionHandler:"

-- | @Selector@ for @skipBackwardWithParams:completionHandler:@
skipBackwardWithParams_completionHandlerSelector :: Selector '[Id MTRMediaPlaybackClusterSkipBackwardParams, Ptr ()] ()
skipBackwardWithParams_completionHandlerSelector = mkSelector "skipBackwardWithParams:completionHandler:"

-- | @Selector@ for @seekWithParams:completionHandler:@
seekWithParams_completionHandlerSelector :: Selector '[Id MTRMediaPlaybackClusterSeekParams, Ptr ()] ()
seekWithParams_completionHandlerSelector = mkSelector "seekWithParams:completionHandler:"

-- | @Selector@ for @readAttributeCurrentStateWithCompletionHandler:@
readAttributeCurrentStateWithCompletionHandlerSelector :: Selector '[Ptr ()] ()
readAttributeCurrentStateWithCompletionHandlerSelector = mkSelector "readAttributeCurrentStateWithCompletionHandler:"

-- | @Selector@ for @subscribeAttributeCurrentStateWithMinInterval:maxInterval:params:subscriptionEstablished:reportHandler:@
subscribeAttributeCurrentStateWithMinInterval_maxInterval_params_subscriptionEstablished_reportHandlerSelector :: Selector '[Id NSNumber, Id NSNumber, Id MTRSubscribeParams, Ptr (), Ptr ()] ()
subscribeAttributeCurrentStateWithMinInterval_maxInterval_params_subscriptionEstablished_reportHandlerSelector = mkSelector "subscribeAttributeCurrentStateWithMinInterval:maxInterval:params:subscriptionEstablished:reportHandler:"

-- | @Selector@ for @readAttributeCurrentStateWithAttributeCache:endpoint:queue:completionHandler:@
readAttributeCurrentStateWithAttributeCache_endpoint_queue_completionHandlerSelector :: Selector '[Id MTRAttributeCacheContainer, Id NSNumber, Id NSObject, Ptr ()] ()
readAttributeCurrentStateWithAttributeCache_endpoint_queue_completionHandlerSelector = mkSelector "readAttributeCurrentStateWithAttributeCache:endpoint:queue:completionHandler:"

-- | @Selector@ for @readAttributeStartTimeWithCompletionHandler:@
readAttributeStartTimeWithCompletionHandlerSelector :: Selector '[Ptr ()] ()
readAttributeStartTimeWithCompletionHandlerSelector = mkSelector "readAttributeStartTimeWithCompletionHandler:"

-- | @Selector@ for @subscribeAttributeStartTimeWithMinInterval:maxInterval:params:subscriptionEstablished:reportHandler:@
subscribeAttributeStartTimeWithMinInterval_maxInterval_params_subscriptionEstablished_reportHandlerSelector :: Selector '[Id NSNumber, Id NSNumber, Id MTRSubscribeParams, Ptr (), Ptr ()] ()
subscribeAttributeStartTimeWithMinInterval_maxInterval_params_subscriptionEstablished_reportHandlerSelector = mkSelector "subscribeAttributeStartTimeWithMinInterval:maxInterval:params:subscriptionEstablished:reportHandler:"

-- | @Selector@ for @readAttributeStartTimeWithAttributeCache:endpoint:queue:completionHandler:@
readAttributeStartTimeWithAttributeCache_endpoint_queue_completionHandlerSelector :: Selector '[Id MTRAttributeCacheContainer, Id NSNumber, Id NSObject, Ptr ()] ()
readAttributeStartTimeWithAttributeCache_endpoint_queue_completionHandlerSelector = mkSelector "readAttributeStartTimeWithAttributeCache:endpoint:queue:completionHandler:"

-- | @Selector@ for @readAttributeDurationWithCompletionHandler:@
readAttributeDurationWithCompletionHandlerSelector :: Selector '[Ptr ()] ()
readAttributeDurationWithCompletionHandlerSelector = mkSelector "readAttributeDurationWithCompletionHandler:"

-- | @Selector@ for @subscribeAttributeDurationWithMinInterval:maxInterval:params:subscriptionEstablished:reportHandler:@
subscribeAttributeDurationWithMinInterval_maxInterval_params_subscriptionEstablished_reportHandlerSelector :: Selector '[Id NSNumber, Id NSNumber, Id MTRSubscribeParams, Ptr (), Ptr ()] ()
subscribeAttributeDurationWithMinInterval_maxInterval_params_subscriptionEstablished_reportHandlerSelector = mkSelector "subscribeAttributeDurationWithMinInterval:maxInterval:params:subscriptionEstablished:reportHandler:"

-- | @Selector@ for @readAttributeDurationWithAttributeCache:endpoint:queue:completionHandler:@
readAttributeDurationWithAttributeCache_endpoint_queue_completionHandlerSelector :: Selector '[Id MTRAttributeCacheContainer, Id NSNumber, Id NSObject, Ptr ()] ()
readAttributeDurationWithAttributeCache_endpoint_queue_completionHandlerSelector = mkSelector "readAttributeDurationWithAttributeCache:endpoint:queue:completionHandler:"

-- | @Selector@ for @readAttributeSampledPositionWithCompletionHandler:@
readAttributeSampledPositionWithCompletionHandlerSelector :: Selector '[Ptr ()] ()
readAttributeSampledPositionWithCompletionHandlerSelector = mkSelector "readAttributeSampledPositionWithCompletionHandler:"

-- | @Selector@ for @subscribeAttributeSampledPositionWithMinInterval:maxInterval:params:subscriptionEstablished:reportHandler:@
subscribeAttributeSampledPositionWithMinInterval_maxInterval_params_subscriptionEstablished_reportHandlerSelector :: Selector '[Id NSNumber, Id NSNumber, Id MTRSubscribeParams, Ptr (), Ptr ()] ()
subscribeAttributeSampledPositionWithMinInterval_maxInterval_params_subscriptionEstablished_reportHandlerSelector = mkSelector "subscribeAttributeSampledPositionWithMinInterval:maxInterval:params:subscriptionEstablished:reportHandler:"

-- | @Selector@ for @readAttributeSampledPositionWithAttributeCache:endpoint:queue:completionHandler:@
readAttributeSampledPositionWithAttributeCache_endpoint_queue_completionHandlerSelector :: Selector '[Id MTRAttributeCacheContainer, Id NSNumber, Id NSObject, Ptr ()] ()
readAttributeSampledPositionWithAttributeCache_endpoint_queue_completionHandlerSelector = mkSelector "readAttributeSampledPositionWithAttributeCache:endpoint:queue:completionHandler:"

-- | @Selector@ for @readAttributePlaybackSpeedWithCompletionHandler:@
readAttributePlaybackSpeedWithCompletionHandlerSelector :: Selector '[Ptr ()] ()
readAttributePlaybackSpeedWithCompletionHandlerSelector = mkSelector "readAttributePlaybackSpeedWithCompletionHandler:"

-- | @Selector@ for @subscribeAttributePlaybackSpeedWithMinInterval:maxInterval:params:subscriptionEstablished:reportHandler:@
subscribeAttributePlaybackSpeedWithMinInterval_maxInterval_params_subscriptionEstablished_reportHandlerSelector :: Selector '[Id NSNumber, Id NSNumber, Id MTRSubscribeParams, Ptr (), Ptr ()] ()
subscribeAttributePlaybackSpeedWithMinInterval_maxInterval_params_subscriptionEstablished_reportHandlerSelector = mkSelector "subscribeAttributePlaybackSpeedWithMinInterval:maxInterval:params:subscriptionEstablished:reportHandler:"

-- | @Selector@ for @readAttributePlaybackSpeedWithAttributeCache:endpoint:queue:completionHandler:@
readAttributePlaybackSpeedWithAttributeCache_endpoint_queue_completionHandlerSelector :: Selector '[Id MTRAttributeCacheContainer, Id NSNumber, Id NSObject, Ptr ()] ()
readAttributePlaybackSpeedWithAttributeCache_endpoint_queue_completionHandlerSelector = mkSelector "readAttributePlaybackSpeedWithAttributeCache:endpoint:queue:completionHandler:"

-- | @Selector@ for @readAttributeSeekRangeEndWithCompletionHandler:@
readAttributeSeekRangeEndWithCompletionHandlerSelector :: Selector '[Ptr ()] ()
readAttributeSeekRangeEndWithCompletionHandlerSelector = mkSelector "readAttributeSeekRangeEndWithCompletionHandler:"

-- | @Selector@ for @subscribeAttributeSeekRangeEndWithMinInterval:maxInterval:params:subscriptionEstablished:reportHandler:@
subscribeAttributeSeekRangeEndWithMinInterval_maxInterval_params_subscriptionEstablished_reportHandlerSelector :: Selector '[Id NSNumber, Id NSNumber, Id MTRSubscribeParams, Ptr (), Ptr ()] ()
subscribeAttributeSeekRangeEndWithMinInterval_maxInterval_params_subscriptionEstablished_reportHandlerSelector = mkSelector "subscribeAttributeSeekRangeEndWithMinInterval:maxInterval:params:subscriptionEstablished:reportHandler:"

-- | @Selector@ for @readAttributeSeekRangeEndWithAttributeCache:endpoint:queue:completionHandler:@
readAttributeSeekRangeEndWithAttributeCache_endpoint_queue_completionHandlerSelector :: Selector '[Id MTRAttributeCacheContainer, Id NSNumber, Id NSObject, Ptr ()] ()
readAttributeSeekRangeEndWithAttributeCache_endpoint_queue_completionHandlerSelector = mkSelector "readAttributeSeekRangeEndWithAttributeCache:endpoint:queue:completionHandler:"

-- | @Selector@ for @readAttributeSeekRangeStartWithCompletionHandler:@
readAttributeSeekRangeStartWithCompletionHandlerSelector :: Selector '[Ptr ()] ()
readAttributeSeekRangeStartWithCompletionHandlerSelector = mkSelector "readAttributeSeekRangeStartWithCompletionHandler:"

-- | @Selector@ for @subscribeAttributeSeekRangeStartWithMinInterval:maxInterval:params:subscriptionEstablished:reportHandler:@
subscribeAttributeSeekRangeStartWithMinInterval_maxInterval_params_subscriptionEstablished_reportHandlerSelector :: Selector '[Id NSNumber, Id NSNumber, Id MTRSubscribeParams, Ptr (), Ptr ()] ()
subscribeAttributeSeekRangeStartWithMinInterval_maxInterval_params_subscriptionEstablished_reportHandlerSelector = mkSelector "subscribeAttributeSeekRangeStartWithMinInterval:maxInterval:params:subscriptionEstablished:reportHandler:"

-- | @Selector@ for @readAttributeSeekRangeStartWithAttributeCache:endpoint:queue:completionHandler:@
readAttributeSeekRangeStartWithAttributeCache_endpoint_queue_completionHandlerSelector :: Selector '[Id MTRAttributeCacheContainer, Id NSNumber, Id NSObject, Ptr ()] ()
readAttributeSeekRangeStartWithAttributeCache_endpoint_queue_completionHandlerSelector = mkSelector "readAttributeSeekRangeStartWithAttributeCache:endpoint:queue:completionHandler:"

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
initWithDevice_endpointID_queueSelector :: Selector '[Id MTRBaseDevice, Id NSNumber, Id NSObject] (Id MTRBaseClusterMediaPlayback)
initWithDevice_endpointID_queueSelector = mkSelector "initWithDevice:endpointID:queue:"

