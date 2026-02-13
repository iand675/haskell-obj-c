{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Cluster Media Playback    This cluster provides an interface for controlling Media Playback (PLAY, PAUSE, etc) on a media device such as a TV or Speaker.
--
-- Generated bindings for @MTRClusterMediaPlayback@.
module ObjC.Matter.MTRClusterMediaPlayback
  ( MTRClusterMediaPlayback
  , IsMTRClusterMediaPlayback(..)
  , playWithParams_expectedValues_expectedValueInterval_completion
  , playWithExpectedValues_expectedValueInterval_completion
  , pauseWithParams_expectedValues_expectedValueInterval_completion
  , pauseWithExpectedValues_expectedValueInterval_completion
  , stopWithParams_expectedValues_expectedValueInterval_completion
  , stopWithExpectedValues_expectedValueInterval_completion
  , startOverWithParams_expectedValues_expectedValueInterval_completion
  , startOverWithExpectedValues_expectedValueInterval_completion
  , previousWithParams_expectedValues_expectedValueInterval_completion
  , previousWithExpectedValues_expectedValueInterval_completion
  , nextWithParams_expectedValues_expectedValueInterval_completion
  , nextWithExpectedValues_expectedValueInterval_completion
  , rewindWithParams_expectedValues_expectedValueInterval_completion
  , rewindWithExpectedValues_expectedValueInterval_completion
  , fastForwardWithParams_expectedValues_expectedValueInterval_completion
  , fastForwardWithExpectedValues_expectedValueInterval_completion
  , skipForwardWithParams_expectedValues_expectedValueInterval_completion
  , skipBackwardWithParams_expectedValues_expectedValueInterval_completion
  , seekWithParams_expectedValues_expectedValueInterval_completion
  , activateAudioTrackWithParams_expectedValues_expectedValueInterval_completion
  , activateTextTrackWithParams_expectedValues_expectedValueInterval_completion
  , deactivateTextTrackWithParams_expectedValues_expectedValueInterval_completion
  , deactivateTextTrackWithExpectedValues_expectedValueInterval_completion
  , readAttributeCurrentStateWithParams
  , readAttributeStartTimeWithParams
  , readAttributeDurationWithParams
  , readAttributeSampledPositionWithParams
  , readAttributePlaybackSpeedWithParams
  , readAttributeSeekRangeEndWithParams
  , readAttributeSeekRangeStartWithParams
  , readAttributeActiveAudioTrackWithParams
  , readAttributeAvailableAudioTracksWithParams
  , readAttributeActiveTextTrackWithParams
  , readAttributeAvailableTextTracksWithParams
  , readAttributeGeneratedCommandListWithParams
  , readAttributeAcceptedCommandListWithParams
  , readAttributeAttributeListWithParams
  , readAttributeFeatureMapWithParams
  , readAttributeClusterRevisionWithParams
  , init_
  , new
  , initWithDevice_endpoint_queue
  , playWithParams_expectedValues_expectedValueInterval_completionHandler
  , playWithExpectedValues_expectedValueInterval_completionHandler
  , pauseWithParams_expectedValues_expectedValueInterval_completionHandler
  , pauseWithExpectedValues_expectedValueInterval_completionHandler
  , stopPlaybackWithParams_expectedValues_expectedValueInterval_completionHandler
  , stopPlaybackWithExpectedValues_expectedValueInterval_completionHandler
  , startOverWithParams_expectedValues_expectedValueInterval_completionHandler
  , startOverWithExpectedValues_expectedValueInterval_completionHandler
  , previousWithParams_expectedValues_expectedValueInterval_completionHandler
  , previousWithExpectedValues_expectedValueInterval_completionHandler
  , nextWithParams_expectedValues_expectedValueInterval_completionHandler
  , nextWithExpectedValues_expectedValueInterval_completionHandler
  , rewindWithParams_expectedValues_expectedValueInterval_completionHandler
  , rewindWithExpectedValues_expectedValueInterval_completionHandler
  , fastForwardWithParams_expectedValues_expectedValueInterval_completionHandler
  , fastForwardWithExpectedValues_expectedValueInterval_completionHandler
  , skipForwardWithParams_expectedValues_expectedValueInterval_completionHandler
  , skipBackwardWithParams_expectedValues_expectedValueInterval_completionHandler
  , seekWithParams_expectedValues_expectedValueInterval_completionHandler
  , initWithDevice_endpointID_queue
  , activateAudioTrackWithParams_expectedValues_expectedValueInterval_completionSelector
  , activateTextTrackWithParams_expectedValues_expectedValueInterval_completionSelector
  , deactivateTextTrackWithExpectedValues_expectedValueInterval_completionSelector
  , deactivateTextTrackWithParams_expectedValues_expectedValueInterval_completionSelector
  , fastForwardWithExpectedValues_expectedValueInterval_completionHandlerSelector
  , fastForwardWithExpectedValues_expectedValueInterval_completionSelector
  , fastForwardWithParams_expectedValues_expectedValueInterval_completionHandlerSelector
  , fastForwardWithParams_expectedValues_expectedValueInterval_completionSelector
  , initSelector
  , initWithDevice_endpointID_queueSelector
  , initWithDevice_endpoint_queueSelector
  , newSelector
  , nextWithExpectedValues_expectedValueInterval_completionHandlerSelector
  , nextWithExpectedValues_expectedValueInterval_completionSelector
  , nextWithParams_expectedValues_expectedValueInterval_completionHandlerSelector
  , nextWithParams_expectedValues_expectedValueInterval_completionSelector
  , pauseWithExpectedValues_expectedValueInterval_completionHandlerSelector
  , pauseWithExpectedValues_expectedValueInterval_completionSelector
  , pauseWithParams_expectedValues_expectedValueInterval_completionHandlerSelector
  , pauseWithParams_expectedValues_expectedValueInterval_completionSelector
  , playWithExpectedValues_expectedValueInterval_completionHandlerSelector
  , playWithExpectedValues_expectedValueInterval_completionSelector
  , playWithParams_expectedValues_expectedValueInterval_completionHandlerSelector
  , playWithParams_expectedValues_expectedValueInterval_completionSelector
  , previousWithExpectedValues_expectedValueInterval_completionHandlerSelector
  , previousWithExpectedValues_expectedValueInterval_completionSelector
  , previousWithParams_expectedValues_expectedValueInterval_completionHandlerSelector
  , previousWithParams_expectedValues_expectedValueInterval_completionSelector
  , readAttributeAcceptedCommandListWithParamsSelector
  , readAttributeActiveAudioTrackWithParamsSelector
  , readAttributeActiveTextTrackWithParamsSelector
  , readAttributeAttributeListWithParamsSelector
  , readAttributeAvailableAudioTracksWithParamsSelector
  , readAttributeAvailableTextTracksWithParamsSelector
  , readAttributeClusterRevisionWithParamsSelector
  , readAttributeCurrentStateWithParamsSelector
  , readAttributeDurationWithParamsSelector
  , readAttributeFeatureMapWithParamsSelector
  , readAttributeGeneratedCommandListWithParamsSelector
  , readAttributePlaybackSpeedWithParamsSelector
  , readAttributeSampledPositionWithParamsSelector
  , readAttributeSeekRangeEndWithParamsSelector
  , readAttributeSeekRangeStartWithParamsSelector
  , readAttributeStartTimeWithParamsSelector
  , rewindWithExpectedValues_expectedValueInterval_completionHandlerSelector
  , rewindWithExpectedValues_expectedValueInterval_completionSelector
  , rewindWithParams_expectedValues_expectedValueInterval_completionHandlerSelector
  , rewindWithParams_expectedValues_expectedValueInterval_completionSelector
  , seekWithParams_expectedValues_expectedValueInterval_completionHandlerSelector
  , seekWithParams_expectedValues_expectedValueInterval_completionSelector
  , skipBackwardWithParams_expectedValues_expectedValueInterval_completionHandlerSelector
  , skipBackwardWithParams_expectedValues_expectedValueInterval_completionSelector
  , skipForwardWithParams_expectedValues_expectedValueInterval_completionHandlerSelector
  , skipForwardWithParams_expectedValues_expectedValueInterval_completionSelector
  , startOverWithExpectedValues_expectedValueInterval_completionHandlerSelector
  , startOverWithExpectedValues_expectedValueInterval_completionSelector
  , startOverWithParams_expectedValues_expectedValueInterval_completionHandlerSelector
  , startOverWithParams_expectedValues_expectedValueInterval_completionSelector
  , stopPlaybackWithExpectedValues_expectedValueInterval_completionHandlerSelector
  , stopPlaybackWithParams_expectedValues_expectedValueInterval_completionHandlerSelector
  , stopWithExpectedValues_expectedValueInterval_completionSelector
  , stopWithParams_expectedValues_expectedValueInterval_completionSelector


  ) where

import Foreign.Ptr (Ptr, FunPtr)
import Foreign.C.Types

import ObjC.Runtime.Types
import ObjC.Runtime.Message (sendMessage, sendOwnedMessage, sendClassMessage, sendOwnedClassMessage)
import ObjC.Runtime.Selector (mkSelector)
import ObjC.Runtime.Class (getRequiredClass)

import ObjC.Matter.Internal.Classes
import ObjC.Foundation.Internal.Classes

-- | @- playWithParams:expectedValues:expectedValueInterval:completion:@
playWithParams_expectedValues_expectedValueInterval_completion :: (IsMTRClusterMediaPlayback mtrClusterMediaPlayback, IsMTRMediaPlaybackClusterPlayParams params, IsNSArray expectedDataValueDictionaries, IsNSNumber expectedValueIntervalMs) => mtrClusterMediaPlayback -> params -> expectedDataValueDictionaries -> expectedValueIntervalMs -> Ptr () -> IO ()
playWithParams_expectedValues_expectedValueInterval_completion mtrClusterMediaPlayback params expectedDataValueDictionaries expectedValueIntervalMs completion =
  sendMessage mtrClusterMediaPlayback playWithParams_expectedValues_expectedValueInterval_completionSelector (toMTRMediaPlaybackClusterPlayParams params) (toNSArray expectedDataValueDictionaries) (toNSNumber expectedValueIntervalMs) completion

-- | @- playWithExpectedValues:expectedValueInterval:completion:@
playWithExpectedValues_expectedValueInterval_completion :: (IsMTRClusterMediaPlayback mtrClusterMediaPlayback, IsNSArray expectedValues, IsNSNumber expectedValueIntervalMs) => mtrClusterMediaPlayback -> expectedValues -> expectedValueIntervalMs -> Ptr () -> IO ()
playWithExpectedValues_expectedValueInterval_completion mtrClusterMediaPlayback expectedValues expectedValueIntervalMs completion =
  sendMessage mtrClusterMediaPlayback playWithExpectedValues_expectedValueInterval_completionSelector (toNSArray expectedValues) (toNSNumber expectedValueIntervalMs) completion

-- | @- pauseWithParams:expectedValues:expectedValueInterval:completion:@
pauseWithParams_expectedValues_expectedValueInterval_completion :: (IsMTRClusterMediaPlayback mtrClusterMediaPlayback, IsMTRMediaPlaybackClusterPauseParams params, IsNSArray expectedDataValueDictionaries, IsNSNumber expectedValueIntervalMs) => mtrClusterMediaPlayback -> params -> expectedDataValueDictionaries -> expectedValueIntervalMs -> Ptr () -> IO ()
pauseWithParams_expectedValues_expectedValueInterval_completion mtrClusterMediaPlayback params expectedDataValueDictionaries expectedValueIntervalMs completion =
  sendMessage mtrClusterMediaPlayback pauseWithParams_expectedValues_expectedValueInterval_completionSelector (toMTRMediaPlaybackClusterPauseParams params) (toNSArray expectedDataValueDictionaries) (toNSNumber expectedValueIntervalMs) completion

-- | @- pauseWithExpectedValues:expectedValueInterval:completion:@
pauseWithExpectedValues_expectedValueInterval_completion :: (IsMTRClusterMediaPlayback mtrClusterMediaPlayback, IsNSArray expectedValues, IsNSNumber expectedValueIntervalMs) => mtrClusterMediaPlayback -> expectedValues -> expectedValueIntervalMs -> Ptr () -> IO ()
pauseWithExpectedValues_expectedValueInterval_completion mtrClusterMediaPlayback expectedValues expectedValueIntervalMs completion =
  sendMessage mtrClusterMediaPlayback pauseWithExpectedValues_expectedValueInterval_completionSelector (toNSArray expectedValues) (toNSNumber expectedValueIntervalMs) completion

-- | @- stopWithParams:expectedValues:expectedValueInterval:completion:@
stopWithParams_expectedValues_expectedValueInterval_completion :: (IsMTRClusterMediaPlayback mtrClusterMediaPlayback, IsMTRMediaPlaybackClusterStopParams params, IsNSArray expectedDataValueDictionaries, IsNSNumber expectedValueIntervalMs) => mtrClusterMediaPlayback -> params -> expectedDataValueDictionaries -> expectedValueIntervalMs -> Ptr () -> IO ()
stopWithParams_expectedValues_expectedValueInterval_completion mtrClusterMediaPlayback params expectedDataValueDictionaries expectedValueIntervalMs completion =
  sendMessage mtrClusterMediaPlayback stopWithParams_expectedValues_expectedValueInterval_completionSelector (toMTRMediaPlaybackClusterStopParams params) (toNSArray expectedDataValueDictionaries) (toNSNumber expectedValueIntervalMs) completion

-- | @- stopWithExpectedValues:expectedValueInterval:completion:@
stopWithExpectedValues_expectedValueInterval_completion :: (IsMTRClusterMediaPlayback mtrClusterMediaPlayback, IsNSArray expectedValues, IsNSNumber expectedValueIntervalMs) => mtrClusterMediaPlayback -> expectedValues -> expectedValueIntervalMs -> Ptr () -> IO ()
stopWithExpectedValues_expectedValueInterval_completion mtrClusterMediaPlayback expectedValues expectedValueIntervalMs completion =
  sendMessage mtrClusterMediaPlayback stopWithExpectedValues_expectedValueInterval_completionSelector (toNSArray expectedValues) (toNSNumber expectedValueIntervalMs) completion

-- | @- startOverWithParams:expectedValues:expectedValueInterval:completion:@
startOverWithParams_expectedValues_expectedValueInterval_completion :: (IsMTRClusterMediaPlayback mtrClusterMediaPlayback, IsMTRMediaPlaybackClusterStartOverParams params, IsNSArray expectedDataValueDictionaries, IsNSNumber expectedValueIntervalMs) => mtrClusterMediaPlayback -> params -> expectedDataValueDictionaries -> expectedValueIntervalMs -> Ptr () -> IO ()
startOverWithParams_expectedValues_expectedValueInterval_completion mtrClusterMediaPlayback params expectedDataValueDictionaries expectedValueIntervalMs completion =
  sendMessage mtrClusterMediaPlayback startOverWithParams_expectedValues_expectedValueInterval_completionSelector (toMTRMediaPlaybackClusterStartOverParams params) (toNSArray expectedDataValueDictionaries) (toNSNumber expectedValueIntervalMs) completion

-- | @- startOverWithExpectedValues:expectedValueInterval:completion:@
startOverWithExpectedValues_expectedValueInterval_completion :: (IsMTRClusterMediaPlayback mtrClusterMediaPlayback, IsNSArray expectedValues, IsNSNumber expectedValueIntervalMs) => mtrClusterMediaPlayback -> expectedValues -> expectedValueIntervalMs -> Ptr () -> IO ()
startOverWithExpectedValues_expectedValueInterval_completion mtrClusterMediaPlayback expectedValues expectedValueIntervalMs completion =
  sendMessage mtrClusterMediaPlayback startOverWithExpectedValues_expectedValueInterval_completionSelector (toNSArray expectedValues) (toNSNumber expectedValueIntervalMs) completion

-- | @- previousWithParams:expectedValues:expectedValueInterval:completion:@
previousWithParams_expectedValues_expectedValueInterval_completion :: (IsMTRClusterMediaPlayback mtrClusterMediaPlayback, IsMTRMediaPlaybackClusterPreviousParams params, IsNSArray expectedDataValueDictionaries, IsNSNumber expectedValueIntervalMs) => mtrClusterMediaPlayback -> params -> expectedDataValueDictionaries -> expectedValueIntervalMs -> Ptr () -> IO ()
previousWithParams_expectedValues_expectedValueInterval_completion mtrClusterMediaPlayback params expectedDataValueDictionaries expectedValueIntervalMs completion =
  sendMessage mtrClusterMediaPlayback previousWithParams_expectedValues_expectedValueInterval_completionSelector (toMTRMediaPlaybackClusterPreviousParams params) (toNSArray expectedDataValueDictionaries) (toNSNumber expectedValueIntervalMs) completion

-- | @- previousWithExpectedValues:expectedValueInterval:completion:@
previousWithExpectedValues_expectedValueInterval_completion :: (IsMTRClusterMediaPlayback mtrClusterMediaPlayback, IsNSArray expectedValues, IsNSNumber expectedValueIntervalMs) => mtrClusterMediaPlayback -> expectedValues -> expectedValueIntervalMs -> Ptr () -> IO ()
previousWithExpectedValues_expectedValueInterval_completion mtrClusterMediaPlayback expectedValues expectedValueIntervalMs completion =
  sendMessage mtrClusterMediaPlayback previousWithExpectedValues_expectedValueInterval_completionSelector (toNSArray expectedValues) (toNSNumber expectedValueIntervalMs) completion

-- | @- nextWithParams:expectedValues:expectedValueInterval:completion:@
nextWithParams_expectedValues_expectedValueInterval_completion :: (IsMTRClusterMediaPlayback mtrClusterMediaPlayback, IsMTRMediaPlaybackClusterNextParams params, IsNSArray expectedDataValueDictionaries, IsNSNumber expectedValueIntervalMs) => mtrClusterMediaPlayback -> params -> expectedDataValueDictionaries -> expectedValueIntervalMs -> Ptr () -> IO ()
nextWithParams_expectedValues_expectedValueInterval_completion mtrClusterMediaPlayback params expectedDataValueDictionaries expectedValueIntervalMs completion =
  sendMessage mtrClusterMediaPlayback nextWithParams_expectedValues_expectedValueInterval_completionSelector (toMTRMediaPlaybackClusterNextParams params) (toNSArray expectedDataValueDictionaries) (toNSNumber expectedValueIntervalMs) completion

-- | @- nextWithExpectedValues:expectedValueInterval:completion:@
nextWithExpectedValues_expectedValueInterval_completion :: (IsMTRClusterMediaPlayback mtrClusterMediaPlayback, IsNSArray expectedValues, IsNSNumber expectedValueIntervalMs) => mtrClusterMediaPlayback -> expectedValues -> expectedValueIntervalMs -> Ptr () -> IO ()
nextWithExpectedValues_expectedValueInterval_completion mtrClusterMediaPlayback expectedValues expectedValueIntervalMs completion =
  sendMessage mtrClusterMediaPlayback nextWithExpectedValues_expectedValueInterval_completionSelector (toNSArray expectedValues) (toNSNumber expectedValueIntervalMs) completion

-- | @- rewindWithParams:expectedValues:expectedValueInterval:completion:@
rewindWithParams_expectedValues_expectedValueInterval_completion :: (IsMTRClusterMediaPlayback mtrClusterMediaPlayback, IsMTRMediaPlaybackClusterRewindParams params, IsNSArray expectedDataValueDictionaries, IsNSNumber expectedValueIntervalMs) => mtrClusterMediaPlayback -> params -> expectedDataValueDictionaries -> expectedValueIntervalMs -> Ptr () -> IO ()
rewindWithParams_expectedValues_expectedValueInterval_completion mtrClusterMediaPlayback params expectedDataValueDictionaries expectedValueIntervalMs completion =
  sendMessage mtrClusterMediaPlayback rewindWithParams_expectedValues_expectedValueInterval_completionSelector (toMTRMediaPlaybackClusterRewindParams params) (toNSArray expectedDataValueDictionaries) (toNSNumber expectedValueIntervalMs) completion

-- | @- rewindWithExpectedValues:expectedValueInterval:completion:@
rewindWithExpectedValues_expectedValueInterval_completion :: (IsMTRClusterMediaPlayback mtrClusterMediaPlayback, IsNSArray expectedValues, IsNSNumber expectedValueIntervalMs) => mtrClusterMediaPlayback -> expectedValues -> expectedValueIntervalMs -> Ptr () -> IO ()
rewindWithExpectedValues_expectedValueInterval_completion mtrClusterMediaPlayback expectedValues expectedValueIntervalMs completion =
  sendMessage mtrClusterMediaPlayback rewindWithExpectedValues_expectedValueInterval_completionSelector (toNSArray expectedValues) (toNSNumber expectedValueIntervalMs) completion

-- | @- fastForwardWithParams:expectedValues:expectedValueInterval:completion:@
fastForwardWithParams_expectedValues_expectedValueInterval_completion :: (IsMTRClusterMediaPlayback mtrClusterMediaPlayback, IsMTRMediaPlaybackClusterFastForwardParams params, IsNSArray expectedDataValueDictionaries, IsNSNumber expectedValueIntervalMs) => mtrClusterMediaPlayback -> params -> expectedDataValueDictionaries -> expectedValueIntervalMs -> Ptr () -> IO ()
fastForwardWithParams_expectedValues_expectedValueInterval_completion mtrClusterMediaPlayback params expectedDataValueDictionaries expectedValueIntervalMs completion =
  sendMessage mtrClusterMediaPlayback fastForwardWithParams_expectedValues_expectedValueInterval_completionSelector (toMTRMediaPlaybackClusterFastForwardParams params) (toNSArray expectedDataValueDictionaries) (toNSNumber expectedValueIntervalMs) completion

-- | @- fastForwardWithExpectedValues:expectedValueInterval:completion:@
fastForwardWithExpectedValues_expectedValueInterval_completion :: (IsMTRClusterMediaPlayback mtrClusterMediaPlayback, IsNSArray expectedValues, IsNSNumber expectedValueIntervalMs) => mtrClusterMediaPlayback -> expectedValues -> expectedValueIntervalMs -> Ptr () -> IO ()
fastForwardWithExpectedValues_expectedValueInterval_completion mtrClusterMediaPlayback expectedValues expectedValueIntervalMs completion =
  sendMessage mtrClusterMediaPlayback fastForwardWithExpectedValues_expectedValueInterval_completionSelector (toNSArray expectedValues) (toNSNumber expectedValueIntervalMs) completion

-- | @- skipForwardWithParams:expectedValues:expectedValueInterval:completion:@
skipForwardWithParams_expectedValues_expectedValueInterval_completion :: (IsMTRClusterMediaPlayback mtrClusterMediaPlayback, IsMTRMediaPlaybackClusterSkipForwardParams params, IsNSArray expectedDataValueDictionaries, IsNSNumber expectedValueIntervalMs) => mtrClusterMediaPlayback -> params -> expectedDataValueDictionaries -> expectedValueIntervalMs -> Ptr () -> IO ()
skipForwardWithParams_expectedValues_expectedValueInterval_completion mtrClusterMediaPlayback params expectedDataValueDictionaries expectedValueIntervalMs completion =
  sendMessage mtrClusterMediaPlayback skipForwardWithParams_expectedValues_expectedValueInterval_completionSelector (toMTRMediaPlaybackClusterSkipForwardParams params) (toNSArray expectedDataValueDictionaries) (toNSNumber expectedValueIntervalMs) completion

-- | @- skipBackwardWithParams:expectedValues:expectedValueInterval:completion:@
skipBackwardWithParams_expectedValues_expectedValueInterval_completion :: (IsMTRClusterMediaPlayback mtrClusterMediaPlayback, IsMTRMediaPlaybackClusterSkipBackwardParams params, IsNSArray expectedDataValueDictionaries, IsNSNumber expectedValueIntervalMs) => mtrClusterMediaPlayback -> params -> expectedDataValueDictionaries -> expectedValueIntervalMs -> Ptr () -> IO ()
skipBackwardWithParams_expectedValues_expectedValueInterval_completion mtrClusterMediaPlayback params expectedDataValueDictionaries expectedValueIntervalMs completion =
  sendMessage mtrClusterMediaPlayback skipBackwardWithParams_expectedValues_expectedValueInterval_completionSelector (toMTRMediaPlaybackClusterSkipBackwardParams params) (toNSArray expectedDataValueDictionaries) (toNSNumber expectedValueIntervalMs) completion

-- | @- seekWithParams:expectedValues:expectedValueInterval:completion:@
seekWithParams_expectedValues_expectedValueInterval_completion :: (IsMTRClusterMediaPlayback mtrClusterMediaPlayback, IsMTRMediaPlaybackClusterSeekParams params, IsNSArray expectedDataValueDictionaries, IsNSNumber expectedValueIntervalMs) => mtrClusterMediaPlayback -> params -> expectedDataValueDictionaries -> expectedValueIntervalMs -> Ptr () -> IO ()
seekWithParams_expectedValues_expectedValueInterval_completion mtrClusterMediaPlayback params expectedDataValueDictionaries expectedValueIntervalMs completion =
  sendMessage mtrClusterMediaPlayback seekWithParams_expectedValues_expectedValueInterval_completionSelector (toMTRMediaPlaybackClusterSeekParams params) (toNSArray expectedDataValueDictionaries) (toNSNumber expectedValueIntervalMs) completion

-- | @- activateAudioTrackWithParams:expectedValues:expectedValueInterval:completion:@
activateAudioTrackWithParams_expectedValues_expectedValueInterval_completion :: (IsMTRClusterMediaPlayback mtrClusterMediaPlayback, IsMTRMediaPlaybackClusterActivateAudioTrackParams params, IsNSArray expectedDataValueDictionaries, IsNSNumber expectedValueIntervalMs) => mtrClusterMediaPlayback -> params -> expectedDataValueDictionaries -> expectedValueIntervalMs -> Ptr () -> IO ()
activateAudioTrackWithParams_expectedValues_expectedValueInterval_completion mtrClusterMediaPlayback params expectedDataValueDictionaries expectedValueIntervalMs completion =
  sendMessage mtrClusterMediaPlayback activateAudioTrackWithParams_expectedValues_expectedValueInterval_completionSelector (toMTRMediaPlaybackClusterActivateAudioTrackParams params) (toNSArray expectedDataValueDictionaries) (toNSNumber expectedValueIntervalMs) completion

-- | @- activateTextTrackWithParams:expectedValues:expectedValueInterval:completion:@
activateTextTrackWithParams_expectedValues_expectedValueInterval_completion :: (IsMTRClusterMediaPlayback mtrClusterMediaPlayback, IsMTRMediaPlaybackClusterActivateTextTrackParams params, IsNSArray expectedDataValueDictionaries, IsNSNumber expectedValueIntervalMs) => mtrClusterMediaPlayback -> params -> expectedDataValueDictionaries -> expectedValueIntervalMs -> Ptr () -> IO ()
activateTextTrackWithParams_expectedValues_expectedValueInterval_completion mtrClusterMediaPlayback params expectedDataValueDictionaries expectedValueIntervalMs completion =
  sendMessage mtrClusterMediaPlayback activateTextTrackWithParams_expectedValues_expectedValueInterval_completionSelector (toMTRMediaPlaybackClusterActivateTextTrackParams params) (toNSArray expectedDataValueDictionaries) (toNSNumber expectedValueIntervalMs) completion

-- | @- deactivateTextTrackWithParams:expectedValues:expectedValueInterval:completion:@
deactivateTextTrackWithParams_expectedValues_expectedValueInterval_completion :: (IsMTRClusterMediaPlayback mtrClusterMediaPlayback, IsMTRMediaPlaybackClusterDeactivateTextTrackParams params, IsNSArray expectedDataValueDictionaries, IsNSNumber expectedValueIntervalMs) => mtrClusterMediaPlayback -> params -> expectedDataValueDictionaries -> expectedValueIntervalMs -> Ptr () -> IO ()
deactivateTextTrackWithParams_expectedValues_expectedValueInterval_completion mtrClusterMediaPlayback params expectedDataValueDictionaries expectedValueIntervalMs completion =
  sendMessage mtrClusterMediaPlayback deactivateTextTrackWithParams_expectedValues_expectedValueInterval_completionSelector (toMTRMediaPlaybackClusterDeactivateTextTrackParams params) (toNSArray expectedDataValueDictionaries) (toNSNumber expectedValueIntervalMs) completion

-- | @- deactivateTextTrackWithExpectedValues:expectedValueInterval:completion:@
deactivateTextTrackWithExpectedValues_expectedValueInterval_completion :: (IsMTRClusterMediaPlayback mtrClusterMediaPlayback, IsNSArray expectedValues, IsNSNumber expectedValueIntervalMs) => mtrClusterMediaPlayback -> expectedValues -> expectedValueIntervalMs -> Ptr () -> IO ()
deactivateTextTrackWithExpectedValues_expectedValueInterval_completion mtrClusterMediaPlayback expectedValues expectedValueIntervalMs completion =
  sendMessage mtrClusterMediaPlayback deactivateTextTrackWithExpectedValues_expectedValueInterval_completionSelector (toNSArray expectedValues) (toNSNumber expectedValueIntervalMs) completion

-- | @- readAttributeCurrentStateWithParams:@
readAttributeCurrentStateWithParams :: (IsMTRClusterMediaPlayback mtrClusterMediaPlayback, IsMTRReadParams params) => mtrClusterMediaPlayback -> params -> IO (Id NSDictionary)
readAttributeCurrentStateWithParams mtrClusterMediaPlayback params =
  sendMessage mtrClusterMediaPlayback readAttributeCurrentStateWithParamsSelector (toMTRReadParams params)

-- | @- readAttributeStartTimeWithParams:@
readAttributeStartTimeWithParams :: (IsMTRClusterMediaPlayback mtrClusterMediaPlayback, IsMTRReadParams params) => mtrClusterMediaPlayback -> params -> IO (Id NSDictionary)
readAttributeStartTimeWithParams mtrClusterMediaPlayback params =
  sendMessage mtrClusterMediaPlayback readAttributeStartTimeWithParamsSelector (toMTRReadParams params)

-- | @- readAttributeDurationWithParams:@
readAttributeDurationWithParams :: (IsMTRClusterMediaPlayback mtrClusterMediaPlayback, IsMTRReadParams params) => mtrClusterMediaPlayback -> params -> IO (Id NSDictionary)
readAttributeDurationWithParams mtrClusterMediaPlayback params =
  sendMessage mtrClusterMediaPlayback readAttributeDurationWithParamsSelector (toMTRReadParams params)

-- | @- readAttributeSampledPositionWithParams:@
readAttributeSampledPositionWithParams :: (IsMTRClusterMediaPlayback mtrClusterMediaPlayback, IsMTRReadParams params) => mtrClusterMediaPlayback -> params -> IO (Id NSDictionary)
readAttributeSampledPositionWithParams mtrClusterMediaPlayback params =
  sendMessage mtrClusterMediaPlayback readAttributeSampledPositionWithParamsSelector (toMTRReadParams params)

-- | @- readAttributePlaybackSpeedWithParams:@
readAttributePlaybackSpeedWithParams :: (IsMTRClusterMediaPlayback mtrClusterMediaPlayback, IsMTRReadParams params) => mtrClusterMediaPlayback -> params -> IO (Id NSDictionary)
readAttributePlaybackSpeedWithParams mtrClusterMediaPlayback params =
  sendMessage mtrClusterMediaPlayback readAttributePlaybackSpeedWithParamsSelector (toMTRReadParams params)

-- | @- readAttributeSeekRangeEndWithParams:@
readAttributeSeekRangeEndWithParams :: (IsMTRClusterMediaPlayback mtrClusterMediaPlayback, IsMTRReadParams params) => mtrClusterMediaPlayback -> params -> IO (Id NSDictionary)
readAttributeSeekRangeEndWithParams mtrClusterMediaPlayback params =
  sendMessage mtrClusterMediaPlayback readAttributeSeekRangeEndWithParamsSelector (toMTRReadParams params)

-- | @- readAttributeSeekRangeStartWithParams:@
readAttributeSeekRangeStartWithParams :: (IsMTRClusterMediaPlayback mtrClusterMediaPlayback, IsMTRReadParams params) => mtrClusterMediaPlayback -> params -> IO (Id NSDictionary)
readAttributeSeekRangeStartWithParams mtrClusterMediaPlayback params =
  sendMessage mtrClusterMediaPlayback readAttributeSeekRangeStartWithParamsSelector (toMTRReadParams params)

-- | @- readAttributeActiveAudioTrackWithParams:@
readAttributeActiveAudioTrackWithParams :: (IsMTRClusterMediaPlayback mtrClusterMediaPlayback, IsMTRReadParams params) => mtrClusterMediaPlayback -> params -> IO (Id NSDictionary)
readAttributeActiveAudioTrackWithParams mtrClusterMediaPlayback params =
  sendMessage mtrClusterMediaPlayback readAttributeActiveAudioTrackWithParamsSelector (toMTRReadParams params)

-- | @- readAttributeAvailableAudioTracksWithParams:@
readAttributeAvailableAudioTracksWithParams :: (IsMTRClusterMediaPlayback mtrClusterMediaPlayback, IsMTRReadParams params) => mtrClusterMediaPlayback -> params -> IO (Id NSDictionary)
readAttributeAvailableAudioTracksWithParams mtrClusterMediaPlayback params =
  sendMessage mtrClusterMediaPlayback readAttributeAvailableAudioTracksWithParamsSelector (toMTRReadParams params)

-- | @- readAttributeActiveTextTrackWithParams:@
readAttributeActiveTextTrackWithParams :: (IsMTRClusterMediaPlayback mtrClusterMediaPlayback, IsMTRReadParams params) => mtrClusterMediaPlayback -> params -> IO (Id NSDictionary)
readAttributeActiveTextTrackWithParams mtrClusterMediaPlayback params =
  sendMessage mtrClusterMediaPlayback readAttributeActiveTextTrackWithParamsSelector (toMTRReadParams params)

-- | @- readAttributeAvailableTextTracksWithParams:@
readAttributeAvailableTextTracksWithParams :: (IsMTRClusterMediaPlayback mtrClusterMediaPlayback, IsMTRReadParams params) => mtrClusterMediaPlayback -> params -> IO (Id NSDictionary)
readAttributeAvailableTextTracksWithParams mtrClusterMediaPlayback params =
  sendMessage mtrClusterMediaPlayback readAttributeAvailableTextTracksWithParamsSelector (toMTRReadParams params)

-- | @- readAttributeGeneratedCommandListWithParams:@
readAttributeGeneratedCommandListWithParams :: (IsMTRClusterMediaPlayback mtrClusterMediaPlayback, IsMTRReadParams params) => mtrClusterMediaPlayback -> params -> IO (Id NSDictionary)
readAttributeGeneratedCommandListWithParams mtrClusterMediaPlayback params =
  sendMessage mtrClusterMediaPlayback readAttributeGeneratedCommandListWithParamsSelector (toMTRReadParams params)

-- | @- readAttributeAcceptedCommandListWithParams:@
readAttributeAcceptedCommandListWithParams :: (IsMTRClusterMediaPlayback mtrClusterMediaPlayback, IsMTRReadParams params) => mtrClusterMediaPlayback -> params -> IO (Id NSDictionary)
readAttributeAcceptedCommandListWithParams mtrClusterMediaPlayback params =
  sendMessage mtrClusterMediaPlayback readAttributeAcceptedCommandListWithParamsSelector (toMTRReadParams params)

-- | @- readAttributeAttributeListWithParams:@
readAttributeAttributeListWithParams :: (IsMTRClusterMediaPlayback mtrClusterMediaPlayback, IsMTRReadParams params) => mtrClusterMediaPlayback -> params -> IO (Id NSDictionary)
readAttributeAttributeListWithParams mtrClusterMediaPlayback params =
  sendMessage mtrClusterMediaPlayback readAttributeAttributeListWithParamsSelector (toMTRReadParams params)

-- | @- readAttributeFeatureMapWithParams:@
readAttributeFeatureMapWithParams :: (IsMTRClusterMediaPlayback mtrClusterMediaPlayback, IsMTRReadParams params) => mtrClusterMediaPlayback -> params -> IO (Id NSDictionary)
readAttributeFeatureMapWithParams mtrClusterMediaPlayback params =
  sendMessage mtrClusterMediaPlayback readAttributeFeatureMapWithParamsSelector (toMTRReadParams params)

-- | @- readAttributeClusterRevisionWithParams:@
readAttributeClusterRevisionWithParams :: (IsMTRClusterMediaPlayback mtrClusterMediaPlayback, IsMTRReadParams params) => mtrClusterMediaPlayback -> params -> IO (Id NSDictionary)
readAttributeClusterRevisionWithParams mtrClusterMediaPlayback params =
  sendMessage mtrClusterMediaPlayback readAttributeClusterRevisionWithParamsSelector (toMTRReadParams params)

-- | @- init@
init_ :: IsMTRClusterMediaPlayback mtrClusterMediaPlayback => mtrClusterMediaPlayback -> IO (Id MTRClusterMediaPlayback)
init_ mtrClusterMediaPlayback =
  sendOwnedMessage mtrClusterMediaPlayback initSelector

-- | @+ new@
new :: IO (Id MTRClusterMediaPlayback)
new  =
  do
    cls' <- getRequiredClass "MTRClusterMediaPlayback"
    sendOwnedClassMessage cls' newSelector

-- | @- initWithDevice:endpoint:queue:@
initWithDevice_endpoint_queue :: (IsMTRClusterMediaPlayback mtrClusterMediaPlayback, IsMTRDevice device, IsNSObject queue) => mtrClusterMediaPlayback -> device -> CUShort -> queue -> IO (Id MTRClusterMediaPlayback)
initWithDevice_endpoint_queue mtrClusterMediaPlayback device endpoint queue =
  sendOwnedMessage mtrClusterMediaPlayback initWithDevice_endpoint_queueSelector (toMTRDevice device) endpoint (toNSObject queue)

-- | @- playWithParams:expectedValues:expectedValueInterval:completionHandler:@
playWithParams_expectedValues_expectedValueInterval_completionHandler :: (IsMTRClusterMediaPlayback mtrClusterMediaPlayback, IsMTRMediaPlaybackClusterPlayParams params, IsNSArray expectedDataValueDictionaries, IsNSNumber expectedValueIntervalMs) => mtrClusterMediaPlayback -> params -> expectedDataValueDictionaries -> expectedValueIntervalMs -> Ptr () -> IO ()
playWithParams_expectedValues_expectedValueInterval_completionHandler mtrClusterMediaPlayback params expectedDataValueDictionaries expectedValueIntervalMs completionHandler =
  sendMessage mtrClusterMediaPlayback playWithParams_expectedValues_expectedValueInterval_completionHandlerSelector (toMTRMediaPlaybackClusterPlayParams params) (toNSArray expectedDataValueDictionaries) (toNSNumber expectedValueIntervalMs) completionHandler

-- | @- playWithExpectedValues:expectedValueInterval:completionHandler:@
playWithExpectedValues_expectedValueInterval_completionHandler :: (IsMTRClusterMediaPlayback mtrClusterMediaPlayback, IsNSArray expectedValues, IsNSNumber expectedValueIntervalMs) => mtrClusterMediaPlayback -> expectedValues -> expectedValueIntervalMs -> Ptr () -> IO ()
playWithExpectedValues_expectedValueInterval_completionHandler mtrClusterMediaPlayback expectedValues expectedValueIntervalMs completionHandler =
  sendMessage mtrClusterMediaPlayback playWithExpectedValues_expectedValueInterval_completionHandlerSelector (toNSArray expectedValues) (toNSNumber expectedValueIntervalMs) completionHandler

-- | @- pauseWithParams:expectedValues:expectedValueInterval:completionHandler:@
pauseWithParams_expectedValues_expectedValueInterval_completionHandler :: (IsMTRClusterMediaPlayback mtrClusterMediaPlayback, IsMTRMediaPlaybackClusterPauseParams params, IsNSArray expectedDataValueDictionaries, IsNSNumber expectedValueIntervalMs) => mtrClusterMediaPlayback -> params -> expectedDataValueDictionaries -> expectedValueIntervalMs -> Ptr () -> IO ()
pauseWithParams_expectedValues_expectedValueInterval_completionHandler mtrClusterMediaPlayback params expectedDataValueDictionaries expectedValueIntervalMs completionHandler =
  sendMessage mtrClusterMediaPlayback pauseWithParams_expectedValues_expectedValueInterval_completionHandlerSelector (toMTRMediaPlaybackClusterPauseParams params) (toNSArray expectedDataValueDictionaries) (toNSNumber expectedValueIntervalMs) completionHandler

-- | @- pauseWithExpectedValues:expectedValueInterval:completionHandler:@
pauseWithExpectedValues_expectedValueInterval_completionHandler :: (IsMTRClusterMediaPlayback mtrClusterMediaPlayback, IsNSArray expectedValues, IsNSNumber expectedValueIntervalMs) => mtrClusterMediaPlayback -> expectedValues -> expectedValueIntervalMs -> Ptr () -> IO ()
pauseWithExpectedValues_expectedValueInterval_completionHandler mtrClusterMediaPlayback expectedValues expectedValueIntervalMs completionHandler =
  sendMessage mtrClusterMediaPlayback pauseWithExpectedValues_expectedValueInterval_completionHandlerSelector (toNSArray expectedValues) (toNSNumber expectedValueIntervalMs) completionHandler

-- | @- stopPlaybackWithParams:expectedValues:expectedValueInterval:completionHandler:@
stopPlaybackWithParams_expectedValues_expectedValueInterval_completionHandler :: (IsMTRClusterMediaPlayback mtrClusterMediaPlayback, IsMTRMediaPlaybackClusterStopPlaybackParams params, IsNSArray expectedDataValueDictionaries, IsNSNumber expectedValueIntervalMs) => mtrClusterMediaPlayback -> params -> expectedDataValueDictionaries -> expectedValueIntervalMs -> Ptr () -> IO ()
stopPlaybackWithParams_expectedValues_expectedValueInterval_completionHandler mtrClusterMediaPlayback params expectedDataValueDictionaries expectedValueIntervalMs completionHandler =
  sendMessage mtrClusterMediaPlayback stopPlaybackWithParams_expectedValues_expectedValueInterval_completionHandlerSelector (toMTRMediaPlaybackClusterStopPlaybackParams params) (toNSArray expectedDataValueDictionaries) (toNSNumber expectedValueIntervalMs) completionHandler

-- | @- stopPlaybackWithExpectedValues:expectedValueInterval:completionHandler:@
stopPlaybackWithExpectedValues_expectedValueInterval_completionHandler :: (IsMTRClusterMediaPlayback mtrClusterMediaPlayback, IsNSArray expectedValues, IsNSNumber expectedValueIntervalMs) => mtrClusterMediaPlayback -> expectedValues -> expectedValueIntervalMs -> Ptr () -> IO ()
stopPlaybackWithExpectedValues_expectedValueInterval_completionHandler mtrClusterMediaPlayback expectedValues expectedValueIntervalMs completionHandler =
  sendMessage mtrClusterMediaPlayback stopPlaybackWithExpectedValues_expectedValueInterval_completionHandlerSelector (toNSArray expectedValues) (toNSNumber expectedValueIntervalMs) completionHandler

-- | @- startOverWithParams:expectedValues:expectedValueInterval:completionHandler:@
startOverWithParams_expectedValues_expectedValueInterval_completionHandler :: (IsMTRClusterMediaPlayback mtrClusterMediaPlayback, IsMTRMediaPlaybackClusterStartOverParams params, IsNSArray expectedDataValueDictionaries, IsNSNumber expectedValueIntervalMs) => mtrClusterMediaPlayback -> params -> expectedDataValueDictionaries -> expectedValueIntervalMs -> Ptr () -> IO ()
startOverWithParams_expectedValues_expectedValueInterval_completionHandler mtrClusterMediaPlayback params expectedDataValueDictionaries expectedValueIntervalMs completionHandler =
  sendMessage mtrClusterMediaPlayback startOverWithParams_expectedValues_expectedValueInterval_completionHandlerSelector (toMTRMediaPlaybackClusterStartOverParams params) (toNSArray expectedDataValueDictionaries) (toNSNumber expectedValueIntervalMs) completionHandler

-- | @- startOverWithExpectedValues:expectedValueInterval:completionHandler:@
startOverWithExpectedValues_expectedValueInterval_completionHandler :: (IsMTRClusterMediaPlayback mtrClusterMediaPlayback, IsNSArray expectedValues, IsNSNumber expectedValueIntervalMs) => mtrClusterMediaPlayback -> expectedValues -> expectedValueIntervalMs -> Ptr () -> IO ()
startOverWithExpectedValues_expectedValueInterval_completionHandler mtrClusterMediaPlayback expectedValues expectedValueIntervalMs completionHandler =
  sendMessage mtrClusterMediaPlayback startOverWithExpectedValues_expectedValueInterval_completionHandlerSelector (toNSArray expectedValues) (toNSNumber expectedValueIntervalMs) completionHandler

-- | @- previousWithParams:expectedValues:expectedValueInterval:completionHandler:@
previousWithParams_expectedValues_expectedValueInterval_completionHandler :: (IsMTRClusterMediaPlayback mtrClusterMediaPlayback, IsMTRMediaPlaybackClusterPreviousParams params, IsNSArray expectedDataValueDictionaries, IsNSNumber expectedValueIntervalMs) => mtrClusterMediaPlayback -> params -> expectedDataValueDictionaries -> expectedValueIntervalMs -> Ptr () -> IO ()
previousWithParams_expectedValues_expectedValueInterval_completionHandler mtrClusterMediaPlayback params expectedDataValueDictionaries expectedValueIntervalMs completionHandler =
  sendMessage mtrClusterMediaPlayback previousWithParams_expectedValues_expectedValueInterval_completionHandlerSelector (toMTRMediaPlaybackClusterPreviousParams params) (toNSArray expectedDataValueDictionaries) (toNSNumber expectedValueIntervalMs) completionHandler

-- | @- previousWithExpectedValues:expectedValueInterval:completionHandler:@
previousWithExpectedValues_expectedValueInterval_completionHandler :: (IsMTRClusterMediaPlayback mtrClusterMediaPlayback, IsNSArray expectedValues, IsNSNumber expectedValueIntervalMs) => mtrClusterMediaPlayback -> expectedValues -> expectedValueIntervalMs -> Ptr () -> IO ()
previousWithExpectedValues_expectedValueInterval_completionHandler mtrClusterMediaPlayback expectedValues expectedValueIntervalMs completionHandler =
  sendMessage mtrClusterMediaPlayback previousWithExpectedValues_expectedValueInterval_completionHandlerSelector (toNSArray expectedValues) (toNSNumber expectedValueIntervalMs) completionHandler

-- | @- nextWithParams:expectedValues:expectedValueInterval:completionHandler:@
nextWithParams_expectedValues_expectedValueInterval_completionHandler :: (IsMTRClusterMediaPlayback mtrClusterMediaPlayback, IsMTRMediaPlaybackClusterNextParams params, IsNSArray expectedDataValueDictionaries, IsNSNumber expectedValueIntervalMs) => mtrClusterMediaPlayback -> params -> expectedDataValueDictionaries -> expectedValueIntervalMs -> Ptr () -> IO ()
nextWithParams_expectedValues_expectedValueInterval_completionHandler mtrClusterMediaPlayback params expectedDataValueDictionaries expectedValueIntervalMs completionHandler =
  sendMessage mtrClusterMediaPlayback nextWithParams_expectedValues_expectedValueInterval_completionHandlerSelector (toMTRMediaPlaybackClusterNextParams params) (toNSArray expectedDataValueDictionaries) (toNSNumber expectedValueIntervalMs) completionHandler

-- | @- nextWithExpectedValues:expectedValueInterval:completionHandler:@
nextWithExpectedValues_expectedValueInterval_completionHandler :: (IsMTRClusterMediaPlayback mtrClusterMediaPlayback, IsNSArray expectedValues, IsNSNumber expectedValueIntervalMs) => mtrClusterMediaPlayback -> expectedValues -> expectedValueIntervalMs -> Ptr () -> IO ()
nextWithExpectedValues_expectedValueInterval_completionHandler mtrClusterMediaPlayback expectedValues expectedValueIntervalMs completionHandler =
  sendMessage mtrClusterMediaPlayback nextWithExpectedValues_expectedValueInterval_completionHandlerSelector (toNSArray expectedValues) (toNSNumber expectedValueIntervalMs) completionHandler

-- | @- rewindWithParams:expectedValues:expectedValueInterval:completionHandler:@
rewindWithParams_expectedValues_expectedValueInterval_completionHandler :: (IsMTRClusterMediaPlayback mtrClusterMediaPlayback, IsMTRMediaPlaybackClusterRewindParams params, IsNSArray expectedDataValueDictionaries, IsNSNumber expectedValueIntervalMs) => mtrClusterMediaPlayback -> params -> expectedDataValueDictionaries -> expectedValueIntervalMs -> Ptr () -> IO ()
rewindWithParams_expectedValues_expectedValueInterval_completionHandler mtrClusterMediaPlayback params expectedDataValueDictionaries expectedValueIntervalMs completionHandler =
  sendMessage mtrClusterMediaPlayback rewindWithParams_expectedValues_expectedValueInterval_completionHandlerSelector (toMTRMediaPlaybackClusterRewindParams params) (toNSArray expectedDataValueDictionaries) (toNSNumber expectedValueIntervalMs) completionHandler

-- | @- rewindWithExpectedValues:expectedValueInterval:completionHandler:@
rewindWithExpectedValues_expectedValueInterval_completionHandler :: (IsMTRClusterMediaPlayback mtrClusterMediaPlayback, IsNSArray expectedValues, IsNSNumber expectedValueIntervalMs) => mtrClusterMediaPlayback -> expectedValues -> expectedValueIntervalMs -> Ptr () -> IO ()
rewindWithExpectedValues_expectedValueInterval_completionHandler mtrClusterMediaPlayback expectedValues expectedValueIntervalMs completionHandler =
  sendMessage mtrClusterMediaPlayback rewindWithExpectedValues_expectedValueInterval_completionHandlerSelector (toNSArray expectedValues) (toNSNumber expectedValueIntervalMs) completionHandler

-- | @- fastForwardWithParams:expectedValues:expectedValueInterval:completionHandler:@
fastForwardWithParams_expectedValues_expectedValueInterval_completionHandler :: (IsMTRClusterMediaPlayback mtrClusterMediaPlayback, IsMTRMediaPlaybackClusterFastForwardParams params, IsNSArray expectedDataValueDictionaries, IsNSNumber expectedValueIntervalMs) => mtrClusterMediaPlayback -> params -> expectedDataValueDictionaries -> expectedValueIntervalMs -> Ptr () -> IO ()
fastForwardWithParams_expectedValues_expectedValueInterval_completionHandler mtrClusterMediaPlayback params expectedDataValueDictionaries expectedValueIntervalMs completionHandler =
  sendMessage mtrClusterMediaPlayback fastForwardWithParams_expectedValues_expectedValueInterval_completionHandlerSelector (toMTRMediaPlaybackClusterFastForwardParams params) (toNSArray expectedDataValueDictionaries) (toNSNumber expectedValueIntervalMs) completionHandler

-- | @- fastForwardWithExpectedValues:expectedValueInterval:completionHandler:@
fastForwardWithExpectedValues_expectedValueInterval_completionHandler :: (IsMTRClusterMediaPlayback mtrClusterMediaPlayback, IsNSArray expectedValues, IsNSNumber expectedValueIntervalMs) => mtrClusterMediaPlayback -> expectedValues -> expectedValueIntervalMs -> Ptr () -> IO ()
fastForwardWithExpectedValues_expectedValueInterval_completionHandler mtrClusterMediaPlayback expectedValues expectedValueIntervalMs completionHandler =
  sendMessage mtrClusterMediaPlayback fastForwardWithExpectedValues_expectedValueInterval_completionHandlerSelector (toNSArray expectedValues) (toNSNumber expectedValueIntervalMs) completionHandler

-- | @- skipForwardWithParams:expectedValues:expectedValueInterval:completionHandler:@
skipForwardWithParams_expectedValues_expectedValueInterval_completionHandler :: (IsMTRClusterMediaPlayback mtrClusterMediaPlayback, IsMTRMediaPlaybackClusterSkipForwardParams params, IsNSArray expectedDataValueDictionaries, IsNSNumber expectedValueIntervalMs) => mtrClusterMediaPlayback -> params -> expectedDataValueDictionaries -> expectedValueIntervalMs -> Ptr () -> IO ()
skipForwardWithParams_expectedValues_expectedValueInterval_completionHandler mtrClusterMediaPlayback params expectedDataValueDictionaries expectedValueIntervalMs completionHandler =
  sendMessage mtrClusterMediaPlayback skipForwardWithParams_expectedValues_expectedValueInterval_completionHandlerSelector (toMTRMediaPlaybackClusterSkipForwardParams params) (toNSArray expectedDataValueDictionaries) (toNSNumber expectedValueIntervalMs) completionHandler

-- | @- skipBackwardWithParams:expectedValues:expectedValueInterval:completionHandler:@
skipBackwardWithParams_expectedValues_expectedValueInterval_completionHandler :: (IsMTRClusterMediaPlayback mtrClusterMediaPlayback, IsMTRMediaPlaybackClusterSkipBackwardParams params, IsNSArray expectedDataValueDictionaries, IsNSNumber expectedValueIntervalMs) => mtrClusterMediaPlayback -> params -> expectedDataValueDictionaries -> expectedValueIntervalMs -> Ptr () -> IO ()
skipBackwardWithParams_expectedValues_expectedValueInterval_completionHandler mtrClusterMediaPlayback params expectedDataValueDictionaries expectedValueIntervalMs completionHandler =
  sendMessage mtrClusterMediaPlayback skipBackwardWithParams_expectedValues_expectedValueInterval_completionHandlerSelector (toMTRMediaPlaybackClusterSkipBackwardParams params) (toNSArray expectedDataValueDictionaries) (toNSNumber expectedValueIntervalMs) completionHandler

-- | @- seekWithParams:expectedValues:expectedValueInterval:completionHandler:@
seekWithParams_expectedValues_expectedValueInterval_completionHandler :: (IsMTRClusterMediaPlayback mtrClusterMediaPlayback, IsMTRMediaPlaybackClusterSeekParams params, IsNSArray expectedDataValueDictionaries, IsNSNumber expectedValueIntervalMs) => mtrClusterMediaPlayback -> params -> expectedDataValueDictionaries -> expectedValueIntervalMs -> Ptr () -> IO ()
seekWithParams_expectedValues_expectedValueInterval_completionHandler mtrClusterMediaPlayback params expectedDataValueDictionaries expectedValueIntervalMs completionHandler =
  sendMessage mtrClusterMediaPlayback seekWithParams_expectedValues_expectedValueInterval_completionHandlerSelector (toMTRMediaPlaybackClusterSeekParams params) (toNSArray expectedDataValueDictionaries) (toNSNumber expectedValueIntervalMs) completionHandler

-- | For all instance methods that take a completion (i.e. command invocations), the completion will be called on the provided queue.
--
-- ObjC selector: @- initWithDevice:endpointID:queue:@
initWithDevice_endpointID_queue :: (IsMTRClusterMediaPlayback mtrClusterMediaPlayback, IsMTRDevice device, IsNSNumber endpointID, IsNSObject queue) => mtrClusterMediaPlayback -> device -> endpointID -> queue -> IO (Id MTRClusterMediaPlayback)
initWithDevice_endpointID_queue mtrClusterMediaPlayback device endpointID queue =
  sendOwnedMessage mtrClusterMediaPlayback initWithDevice_endpointID_queueSelector (toMTRDevice device) (toNSNumber endpointID) (toNSObject queue)

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @playWithParams:expectedValues:expectedValueInterval:completion:@
playWithParams_expectedValues_expectedValueInterval_completionSelector :: Selector '[Id MTRMediaPlaybackClusterPlayParams, Id NSArray, Id NSNumber, Ptr ()] ()
playWithParams_expectedValues_expectedValueInterval_completionSelector = mkSelector "playWithParams:expectedValues:expectedValueInterval:completion:"

-- | @Selector@ for @playWithExpectedValues:expectedValueInterval:completion:@
playWithExpectedValues_expectedValueInterval_completionSelector :: Selector '[Id NSArray, Id NSNumber, Ptr ()] ()
playWithExpectedValues_expectedValueInterval_completionSelector = mkSelector "playWithExpectedValues:expectedValueInterval:completion:"

-- | @Selector@ for @pauseWithParams:expectedValues:expectedValueInterval:completion:@
pauseWithParams_expectedValues_expectedValueInterval_completionSelector :: Selector '[Id MTRMediaPlaybackClusterPauseParams, Id NSArray, Id NSNumber, Ptr ()] ()
pauseWithParams_expectedValues_expectedValueInterval_completionSelector = mkSelector "pauseWithParams:expectedValues:expectedValueInterval:completion:"

-- | @Selector@ for @pauseWithExpectedValues:expectedValueInterval:completion:@
pauseWithExpectedValues_expectedValueInterval_completionSelector :: Selector '[Id NSArray, Id NSNumber, Ptr ()] ()
pauseWithExpectedValues_expectedValueInterval_completionSelector = mkSelector "pauseWithExpectedValues:expectedValueInterval:completion:"

-- | @Selector@ for @stopWithParams:expectedValues:expectedValueInterval:completion:@
stopWithParams_expectedValues_expectedValueInterval_completionSelector :: Selector '[Id MTRMediaPlaybackClusterStopParams, Id NSArray, Id NSNumber, Ptr ()] ()
stopWithParams_expectedValues_expectedValueInterval_completionSelector = mkSelector "stopWithParams:expectedValues:expectedValueInterval:completion:"

-- | @Selector@ for @stopWithExpectedValues:expectedValueInterval:completion:@
stopWithExpectedValues_expectedValueInterval_completionSelector :: Selector '[Id NSArray, Id NSNumber, Ptr ()] ()
stopWithExpectedValues_expectedValueInterval_completionSelector = mkSelector "stopWithExpectedValues:expectedValueInterval:completion:"

-- | @Selector@ for @startOverWithParams:expectedValues:expectedValueInterval:completion:@
startOverWithParams_expectedValues_expectedValueInterval_completionSelector :: Selector '[Id MTRMediaPlaybackClusterStartOverParams, Id NSArray, Id NSNumber, Ptr ()] ()
startOverWithParams_expectedValues_expectedValueInterval_completionSelector = mkSelector "startOverWithParams:expectedValues:expectedValueInterval:completion:"

-- | @Selector@ for @startOverWithExpectedValues:expectedValueInterval:completion:@
startOverWithExpectedValues_expectedValueInterval_completionSelector :: Selector '[Id NSArray, Id NSNumber, Ptr ()] ()
startOverWithExpectedValues_expectedValueInterval_completionSelector = mkSelector "startOverWithExpectedValues:expectedValueInterval:completion:"

-- | @Selector@ for @previousWithParams:expectedValues:expectedValueInterval:completion:@
previousWithParams_expectedValues_expectedValueInterval_completionSelector :: Selector '[Id MTRMediaPlaybackClusterPreviousParams, Id NSArray, Id NSNumber, Ptr ()] ()
previousWithParams_expectedValues_expectedValueInterval_completionSelector = mkSelector "previousWithParams:expectedValues:expectedValueInterval:completion:"

-- | @Selector@ for @previousWithExpectedValues:expectedValueInterval:completion:@
previousWithExpectedValues_expectedValueInterval_completionSelector :: Selector '[Id NSArray, Id NSNumber, Ptr ()] ()
previousWithExpectedValues_expectedValueInterval_completionSelector = mkSelector "previousWithExpectedValues:expectedValueInterval:completion:"

-- | @Selector@ for @nextWithParams:expectedValues:expectedValueInterval:completion:@
nextWithParams_expectedValues_expectedValueInterval_completionSelector :: Selector '[Id MTRMediaPlaybackClusterNextParams, Id NSArray, Id NSNumber, Ptr ()] ()
nextWithParams_expectedValues_expectedValueInterval_completionSelector = mkSelector "nextWithParams:expectedValues:expectedValueInterval:completion:"

-- | @Selector@ for @nextWithExpectedValues:expectedValueInterval:completion:@
nextWithExpectedValues_expectedValueInterval_completionSelector :: Selector '[Id NSArray, Id NSNumber, Ptr ()] ()
nextWithExpectedValues_expectedValueInterval_completionSelector = mkSelector "nextWithExpectedValues:expectedValueInterval:completion:"

-- | @Selector@ for @rewindWithParams:expectedValues:expectedValueInterval:completion:@
rewindWithParams_expectedValues_expectedValueInterval_completionSelector :: Selector '[Id MTRMediaPlaybackClusterRewindParams, Id NSArray, Id NSNumber, Ptr ()] ()
rewindWithParams_expectedValues_expectedValueInterval_completionSelector = mkSelector "rewindWithParams:expectedValues:expectedValueInterval:completion:"

-- | @Selector@ for @rewindWithExpectedValues:expectedValueInterval:completion:@
rewindWithExpectedValues_expectedValueInterval_completionSelector :: Selector '[Id NSArray, Id NSNumber, Ptr ()] ()
rewindWithExpectedValues_expectedValueInterval_completionSelector = mkSelector "rewindWithExpectedValues:expectedValueInterval:completion:"

-- | @Selector@ for @fastForwardWithParams:expectedValues:expectedValueInterval:completion:@
fastForwardWithParams_expectedValues_expectedValueInterval_completionSelector :: Selector '[Id MTRMediaPlaybackClusterFastForwardParams, Id NSArray, Id NSNumber, Ptr ()] ()
fastForwardWithParams_expectedValues_expectedValueInterval_completionSelector = mkSelector "fastForwardWithParams:expectedValues:expectedValueInterval:completion:"

-- | @Selector@ for @fastForwardWithExpectedValues:expectedValueInterval:completion:@
fastForwardWithExpectedValues_expectedValueInterval_completionSelector :: Selector '[Id NSArray, Id NSNumber, Ptr ()] ()
fastForwardWithExpectedValues_expectedValueInterval_completionSelector = mkSelector "fastForwardWithExpectedValues:expectedValueInterval:completion:"

-- | @Selector@ for @skipForwardWithParams:expectedValues:expectedValueInterval:completion:@
skipForwardWithParams_expectedValues_expectedValueInterval_completionSelector :: Selector '[Id MTRMediaPlaybackClusterSkipForwardParams, Id NSArray, Id NSNumber, Ptr ()] ()
skipForwardWithParams_expectedValues_expectedValueInterval_completionSelector = mkSelector "skipForwardWithParams:expectedValues:expectedValueInterval:completion:"

-- | @Selector@ for @skipBackwardWithParams:expectedValues:expectedValueInterval:completion:@
skipBackwardWithParams_expectedValues_expectedValueInterval_completionSelector :: Selector '[Id MTRMediaPlaybackClusterSkipBackwardParams, Id NSArray, Id NSNumber, Ptr ()] ()
skipBackwardWithParams_expectedValues_expectedValueInterval_completionSelector = mkSelector "skipBackwardWithParams:expectedValues:expectedValueInterval:completion:"

-- | @Selector@ for @seekWithParams:expectedValues:expectedValueInterval:completion:@
seekWithParams_expectedValues_expectedValueInterval_completionSelector :: Selector '[Id MTRMediaPlaybackClusterSeekParams, Id NSArray, Id NSNumber, Ptr ()] ()
seekWithParams_expectedValues_expectedValueInterval_completionSelector = mkSelector "seekWithParams:expectedValues:expectedValueInterval:completion:"

-- | @Selector@ for @activateAudioTrackWithParams:expectedValues:expectedValueInterval:completion:@
activateAudioTrackWithParams_expectedValues_expectedValueInterval_completionSelector :: Selector '[Id MTRMediaPlaybackClusterActivateAudioTrackParams, Id NSArray, Id NSNumber, Ptr ()] ()
activateAudioTrackWithParams_expectedValues_expectedValueInterval_completionSelector = mkSelector "activateAudioTrackWithParams:expectedValues:expectedValueInterval:completion:"

-- | @Selector@ for @activateTextTrackWithParams:expectedValues:expectedValueInterval:completion:@
activateTextTrackWithParams_expectedValues_expectedValueInterval_completionSelector :: Selector '[Id MTRMediaPlaybackClusterActivateTextTrackParams, Id NSArray, Id NSNumber, Ptr ()] ()
activateTextTrackWithParams_expectedValues_expectedValueInterval_completionSelector = mkSelector "activateTextTrackWithParams:expectedValues:expectedValueInterval:completion:"

-- | @Selector@ for @deactivateTextTrackWithParams:expectedValues:expectedValueInterval:completion:@
deactivateTextTrackWithParams_expectedValues_expectedValueInterval_completionSelector :: Selector '[Id MTRMediaPlaybackClusterDeactivateTextTrackParams, Id NSArray, Id NSNumber, Ptr ()] ()
deactivateTextTrackWithParams_expectedValues_expectedValueInterval_completionSelector = mkSelector "deactivateTextTrackWithParams:expectedValues:expectedValueInterval:completion:"

-- | @Selector@ for @deactivateTextTrackWithExpectedValues:expectedValueInterval:completion:@
deactivateTextTrackWithExpectedValues_expectedValueInterval_completionSelector :: Selector '[Id NSArray, Id NSNumber, Ptr ()] ()
deactivateTextTrackWithExpectedValues_expectedValueInterval_completionSelector = mkSelector "deactivateTextTrackWithExpectedValues:expectedValueInterval:completion:"

-- | @Selector@ for @readAttributeCurrentStateWithParams:@
readAttributeCurrentStateWithParamsSelector :: Selector '[Id MTRReadParams] (Id NSDictionary)
readAttributeCurrentStateWithParamsSelector = mkSelector "readAttributeCurrentStateWithParams:"

-- | @Selector@ for @readAttributeStartTimeWithParams:@
readAttributeStartTimeWithParamsSelector :: Selector '[Id MTRReadParams] (Id NSDictionary)
readAttributeStartTimeWithParamsSelector = mkSelector "readAttributeStartTimeWithParams:"

-- | @Selector@ for @readAttributeDurationWithParams:@
readAttributeDurationWithParamsSelector :: Selector '[Id MTRReadParams] (Id NSDictionary)
readAttributeDurationWithParamsSelector = mkSelector "readAttributeDurationWithParams:"

-- | @Selector@ for @readAttributeSampledPositionWithParams:@
readAttributeSampledPositionWithParamsSelector :: Selector '[Id MTRReadParams] (Id NSDictionary)
readAttributeSampledPositionWithParamsSelector = mkSelector "readAttributeSampledPositionWithParams:"

-- | @Selector@ for @readAttributePlaybackSpeedWithParams:@
readAttributePlaybackSpeedWithParamsSelector :: Selector '[Id MTRReadParams] (Id NSDictionary)
readAttributePlaybackSpeedWithParamsSelector = mkSelector "readAttributePlaybackSpeedWithParams:"

-- | @Selector@ for @readAttributeSeekRangeEndWithParams:@
readAttributeSeekRangeEndWithParamsSelector :: Selector '[Id MTRReadParams] (Id NSDictionary)
readAttributeSeekRangeEndWithParamsSelector = mkSelector "readAttributeSeekRangeEndWithParams:"

-- | @Selector@ for @readAttributeSeekRangeStartWithParams:@
readAttributeSeekRangeStartWithParamsSelector :: Selector '[Id MTRReadParams] (Id NSDictionary)
readAttributeSeekRangeStartWithParamsSelector = mkSelector "readAttributeSeekRangeStartWithParams:"

-- | @Selector@ for @readAttributeActiveAudioTrackWithParams:@
readAttributeActiveAudioTrackWithParamsSelector :: Selector '[Id MTRReadParams] (Id NSDictionary)
readAttributeActiveAudioTrackWithParamsSelector = mkSelector "readAttributeActiveAudioTrackWithParams:"

-- | @Selector@ for @readAttributeAvailableAudioTracksWithParams:@
readAttributeAvailableAudioTracksWithParamsSelector :: Selector '[Id MTRReadParams] (Id NSDictionary)
readAttributeAvailableAudioTracksWithParamsSelector = mkSelector "readAttributeAvailableAudioTracksWithParams:"

-- | @Selector@ for @readAttributeActiveTextTrackWithParams:@
readAttributeActiveTextTrackWithParamsSelector :: Selector '[Id MTRReadParams] (Id NSDictionary)
readAttributeActiveTextTrackWithParamsSelector = mkSelector "readAttributeActiveTextTrackWithParams:"

-- | @Selector@ for @readAttributeAvailableTextTracksWithParams:@
readAttributeAvailableTextTracksWithParamsSelector :: Selector '[Id MTRReadParams] (Id NSDictionary)
readAttributeAvailableTextTracksWithParamsSelector = mkSelector "readAttributeAvailableTextTracksWithParams:"

-- | @Selector@ for @readAttributeGeneratedCommandListWithParams:@
readAttributeGeneratedCommandListWithParamsSelector :: Selector '[Id MTRReadParams] (Id NSDictionary)
readAttributeGeneratedCommandListWithParamsSelector = mkSelector "readAttributeGeneratedCommandListWithParams:"

-- | @Selector@ for @readAttributeAcceptedCommandListWithParams:@
readAttributeAcceptedCommandListWithParamsSelector :: Selector '[Id MTRReadParams] (Id NSDictionary)
readAttributeAcceptedCommandListWithParamsSelector = mkSelector "readAttributeAcceptedCommandListWithParams:"

-- | @Selector@ for @readAttributeAttributeListWithParams:@
readAttributeAttributeListWithParamsSelector :: Selector '[Id MTRReadParams] (Id NSDictionary)
readAttributeAttributeListWithParamsSelector = mkSelector "readAttributeAttributeListWithParams:"

-- | @Selector@ for @readAttributeFeatureMapWithParams:@
readAttributeFeatureMapWithParamsSelector :: Selector '[Id MTRReadParams] (Id NSDictionary)
readAttributeFeatureMapWithParamsSelector = mkSelector "readAttributeFeatureMapWithParams:"

-- | @Selector@ for @readAttributeClusterRevisionWithParams:@
readAttributeClusterRevisionWithParamsSelector :: Selector '[Id MTRReadParams] (Id NSDictionary)
readAttributeClusterRevisionWithParamsSelector = mkSelector "readAttributeClusterRevisionWithParams:"

-- | @Selector@ for @init@
initSelector :: Selector '[] (Id MTRClusterMediaPlayback)
initSelector = mkSelector "init"

-- | @Selector@ for @new@
newSelector :: Selector '[] (Id MTRClusterMediaPlayback)
newSelector = mkSelector "new"

-- | @Selector@ for @initWithDevice:endpoint:queue:@
initWithDevice_endpoint_queueSelector :: Selector '[Id MTRDevice, CUShort, Id NSObject] (Id MTRClusterMediaPlayback)
initWithDevice_endpoint_queueSelector = mkSelector "initWithDevice:endpoint:queue:"

-- | @Selector@ for @playWithParams:expectedValues:expectedValueInterval:completionHandler:@
playWithParams_expectedValues_expectedValueInterval_completionHandlerSelector :: Selector '[Id MTRMediaPlaybackClusterPlayParams, Id NSArray, Id NSNumber, Ptr ()] ()
playWithParams_expectedValues_expectedValueInterval_completionHandlerSelector = mkSelector "playWithParams:expectedValues:expectedValueInterval:completionHandler:"

-- | @Selector@ for @playWithExpectedValues:expectedValueInterval:completionHandler:@
playWithExpectedValues_expectedValueInterval_completionHandlerSelector :: Selector '[Id NSArray, Id NSNumber, Ptr ()] ()
playWithExpectedValues_expectedValueInterval_completionHandlerSelector = mkSelector "playWithExpectedValues:expectedValueInterval:completionHandler:"

-- | @Selector@ for @pauseWithParams:expectedValues:expectedValueInterval:completionHandler:@
pauseWithParams_expectedValues_expectedValueInterval_completionHandlerSelector :: Selector '[Id MTRMediaPlaybackClusterPauseParams, Id NSArray, Id NSNumber, Ptr ()] ()
pauseWithParams_expectedValues_expectedValueInterval_completionHandlerSelector = mkSelector "pauseWithParams:expectedValues:expectedValueInterval:completionHandler:"

-- | @Selector@ for @pauseWithExpectedValues:expectedValueInterval:completionHandler:@
pauseWithExpectedValues_expectedValueInterval_completionHandlerSelector :: Selector '[Id NSArray, Id NSNumber, Ptr ()] ()
pauseWithExpectedValues_expectedValueInterval_completionHandlerSelector = mkSelector "pauseWithExpectedValues:expectedValueInterval:completionHandler:"

-- | @Selector@ for @stopPlaybackWithParams:expectedValues:expectedValueInterval:completionHandler:@
stopPlaybackWithParams_expectedValues_expectedValueInterval_completionHandlerSelector :: Selector '[Id MTRMediaPlaybackClusterStopPlaybackParams, Id NSArray, Id NSNumber, Ptr ()] ()
stopPlaybackWithParams_expectedValues_expectedValueInterval_completionHandlerSelector = mkSelector "stopPlaybackWithParams:expectedValues:expectedValueInterval:completionHandler:"

-- | @Selector@ for @stopPlaybackWithExpectedValues:expectedValueInterval:completionHandler:@
stopPlaybackWithExpectedValues_expectedValueInterval_completionHandlerSelector :: Selector '[Id NSArray, Id NSNumber, Ptr ()] ()
stopPlaybackWithExpectedValues_expectedValueInterval_completionHandlerSelector = mkSelector "stopPlaybackWithExpectedValues:expectedValueInterval:completionHandler:"

-- | @Selector@ for @startOverWithParams:expectedValues:expectedValueInterval:completionHandler:@
startOverWithParams_expectedValues_expectedValueInterval_completionHandlerSelector :: Selector '[Id MTRMediaPlaybackClusterStartOverParams, Id NSArray, Id NSNumber, Ptr ()] ()
startOverWithParams_expectedValues_expectedValueInterval_completionHandlerSelector = mkSelector "startOverWithParams:expectedValues:expectedValueInterval:completionHandler:"

-- | @Selector@ for @startOverWithExpectedValues:expectedValueInterval:completionHandler:@
startOverWithExpectedValues_expectedValueInterval_completionHandlerSelector :: Selector '[Id NSArray, Id NSNumber, Ptr ()] ()
startOverWithExpectedValues_expectedValueInterval_completionHandlerSelector = mkSelector "startOverWithExpectedValues:expectedValueInterval:completionHandler:"

-- | @Selector@ for @previousWithParams:expectedValues:expectedValueInterval:completionHandler:@
previousWithParams_expectedValues_expectedValueInterval_completionHandlerSelector :: Selector '[Id MTRMediaPlaybackClusterPreviousParams, Id NSArray, Id NSNumber, Ptr ()] ()
previousWithParams_expectedValues_expectedValueInterval_completionHandlerSelector = mkSelector "previousWithParams:expectedValues:expectedValueInterval:completionHandler:"

-- | @Selector@ for @previousWithExpectedValues:expectedValueInterval:completionHandler:@
previousWithExpectedValues_expectedValueInterval_completionHandlerSelector :: Selector '[Id NSArray, Id NSNumber, Ptr ()] ()
previousWithExpectedValues_expectedValueInterval_completionHandlerSelector = mkSelector "previousWithExpectedValues:expectedValueInterval:completionHandler:"

-- | @Selector@ for @nextWithParams:expectedValues:expectedValueInterval:completionHandler:@
nextWithParams_expectedValues_expectedValueInterval_completionHandlerSelector :: Selector '[Id MTRMediaPlaybackClusterNextParams, Id NSArray, Id NSNumber, Ptr ()] ()
nextWithParams_expectedValues_expectedValueInterval_completionHandlerSelector = mkSelector "nextWithParams:expectedValues:expectedValueInterval:completionHandler:"

-- | @Selector@ for @nextWithExpectedValues:expectedValueInterval:completionHandler:@
nextWithExpectedValues_expectedValueInterval_completionHandlerSelector :: Selector '[Id NSArray, Id NSNumber, Ptr ()] ()
nextWithExpectedValues_expectedValueInterval_completionHandlerSelector = mkSelector "nextWithExpectedValues:expectedValueInterval:completionHandler:"

-- | @Selector@ for @rewindWithParams:expectedValues:expectedValueInterval:completionHandler:@
rewindWithParams_expectedValues_expectedValueInterval_completionHandlerSelector :: Selector '[Id MTRMediaPlaybackClusterRewindParams, Id NSArray, Id NSNumber, Ptr ()] ()
rewindWithParams_expectedValues_expectedValueInterval_completionHandlerSelector = mkSelector "rewindWithParams:expectedValues:expectedValueInterval:completionHandler:"

-- | @Selector@ for @rewindWithExpectedValues:expectedValueInterval:completionHandler:@
rewindWithExpectedValues_expectedValueInterval_completionHandlerSelector :: Selector '[Id NSArray, Id NSNumber, Ptr ()] ()
rewindWithExpectedValues_expectedValueInterval_completionHandlerSelector = mkSelector "rewindWithExpectedValues:expectedValueInterval:completionHandler:"

-- | @Selector@ for @fastForwardWithParams:expectedValues:expectedValueInterval:completionHandler:@
fastForwardWithParams_expectedValues_expectedValueInterval_completionHandlerSelector :: Selector '[Id MTRMediaPlaybackClusterFastForwardParams, Id NSArray, Id NSNumber, Ptr ()] ()
fastForwardWithParams_expectedValues_expectedValueInterval_completionHandlerSelector = mkSelector "fastForwardWithParams:expectedValues:expectedValueInterval:completionHandler:"

-- | @Selector@ for @fastForwardWithExpectedValues:expectedValueInterval:completionHandler:@
fastForwardWithExpectedValues_expectedValueInterval_completionHandlerSelector :: Selector '[Id NSArray, Id NSNumber, Ptr ()] ()
fastForwardWithExpectedValues_expectedValueInterval_completionHandlerSelector = mkSelector "fastForwardWithExpectedValues:expectedValueInterval:completionHandler:"

-- | @Selector@ for @skipForwardWithParams:expectedValues:expectedValueInterval:completionHandler:@
skipForwardWithParams_expectedValues_expectedValueInterval_completionHandlerSelector :: Selector '[Id MTRMediaPlaybackClusterSkipForwardParams, Id NSArray, Id NSNumber, Ptr ()] ()
skipForwardWithParams_expectedValues_expectedValueInterval_completionHandlerSelector = mkSelector "skipForwardWithParams:expectedValues:expectedValueInterval:completionHandler:"

-- | @Selector@ for @skipBackwardWithParams:expectedValues:expectedValueInterval:completionHandler:@
skipBackwardWithParams_expectedValues_expectedValueInterval_completionHandlerSelector :: Selector '[Id MTRMediaPlaybackClusterSkipBackwardParams, Id NSArray, Id NSNumber, Ptr ()] ()
skipBackwardWithParams_expectedValues_expectedValueInterval_completionHandlerSelector = mkSelector "skipBackwardWithParams:expectedValues:expectedValueInterval:completionHandler:"

-- | @Selector@ for @seekWithParams:expectedValues:expectedValueInterval:completionHandler:@
seekWithParams_expectedValues_expectedValueInterval_completionHandlerSelector :: Selector '[Id MTRMediaPlaybackClusterSeekParams, Id NSArray, Id NSNumber, Ptr ()] ()
seekWithParams_expectedValues_expectedValueInterval_completionHandlerSelector = mkSelector "seekWithParams:expectedValues:expectedValueInterval:completionHandler:"

-- | @Selector@ for @initWithDevice:endpointID:queue:@
initWithDevice_endpointID_queueSelector :: Selector '[Id MTRDevice, Id NSNumber, Id NSObject] (Id MTRClusterMediaPlayback)
initWithDevice_endpointID_queueSelector = mkSelector "initWithDevice:endpointID:queue:"

