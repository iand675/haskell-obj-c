{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Represents a summary metric event with aggregated metrics for the entire playback session.
--
-- Subclasses of this type that are used from Swift must fulfill the requirements of a Sendable type.
--
-- Generated bindings for @AVMetricPlayerItemPlaybackSummaryEvent@.
module ObjC.AVFoundation.AVMetricPlayerItemPlaybackSummaryEvent
  ( AVMetricPlayerItemPlaybackSummaryEvent
  , IsAVMetricPlayerItemPlaybackSummaryEvent(..)
  , init_
  , new
  , errorEvent
  , recoverableErrorCount
  , stallCount
  , variantSwitchCount
  , playbackDuration
  , mediaResourceRequestCount
  , timeSpentRecoveringFromStall
  , timeSpentInInitialStartup
  , timeWeightedAverageBitrate
  , timeWeightedPeakBitrate
  , errorEventSelector
  , initSelector
  , mediaResourceRequestCountSelector
  , newSelector
  , playbackDurationSelector
  , recoverableErrorCountSelector
  , stallCountSelector
  , timeSpentInInitialStartupSelector
  , timeSpentRecoveringFromStallSelector
  , timeWeightedAverageBitrateSelector
  , timeWeightedPeakBitrateSelector
  , variantSwitchCountSelector


  ) where

import Foreign.Ptr (Ptr, FunPtr)
import Foreign.C.Types

import ObjC.Runtime.Types
import ObjC.Runtime.Message (sendMessage, sendOwnedMessage, sendClassMessage, sendOwnedClassMessage)
import ObjC.Runtime.Selector (mkSelector)
import ObjC.Runtime.Class (getRequiredClass)

import ObjC.AVFoundation.Internal.Classes
import ObjC.Foundation.Internal.Classes

-- | @- init@
init_ :: IsAVMetricPlayerItemPlaybackSummaryEvent avMetricPlayerItemPlaybackSummaryEvent => avMetricPlayerItemPlaybackSummaryEvent -> IO (Id AVMetricPlayerItemPlaybackSummaryEvent)
init_ avMetricPlayerItemPlaybackSummaryEvent =
  sendOwnedMessage avMetricPlayerItemPlaybackSummaryEvent initSelector

-- | @+ new@
new :: IO (Id AVMetricPlayerItemPlaybackSummaryEvent)
new  =
  do
    cls' <- getRequiredClass "AVMetricPlayerItemPlaybackSummaryEvent"
    sendOwnedClassMessage cls' newSelector

-- | Returns the error event if any. If no value is available, returns nil.
--
-- ObjC selector: @- errorEvent@
errorEvent :: IsAVMetricPlayerItemPlaybackSummaryEvent avMetricPlayerItemPlaybackSummaryEvent => avMetricPlayerItemPlaybackSummaryEvent -> IO (Id AVMetricErrorEvent)
errorEvent avMetricPlayerItemPlaybackSummaryEvent =
  sendMessage avMetricPlayerItemPlaybackSummaryEvent errorEventSelector

-- | Returns the total count of recoverable errors encountered during playback. If no errors were encountered, returns 0.
--
-- ObjC selector: @- recoverableErrorCount@
recoverableErrorCount :: IsAVMetricPlayerItemPlaybackSummaryEvent avMetricPlayerItemPlaybackSummaryEvent => avMetricPlayerItemPlaybackSummaryEvent -> IO CLong
recoverableErrorCount avMetricPlayerItemPlaybackSummaryEvent =
  sendMessage avMetricPlayerItemPlaybackSummaryEvent recoverableErrorCountSelector

-- | Returns the total count of stalls encountered during playback. If no stalls were encountered, returns 0.
--
-- ObjC selector: @- stallCount@
stallCount :: IsAVMetricPlayerItemPlaybackSummaryEvent avMetricPlayerItemPlaybackSummaryEvent => avMetricPlayerItemPlaybackSummaryEvent -> IO CLong
stallCount avMetricPlayerItemPlaybackSummaryEvent =
  sendMessage avMetricPlayerItemPlaybackSummaryEvent stallCountSelector

-- | Returns the total count of variant switch encountered during playback.
--
-- ObjC selector: @- variantSwitchCount@
variantSwitchCount :: IsAVMetricPlayerItemPlaybackSummaryEvent avMetricPlayerItemPlaybackSummaryEvent => avMetricPlayerItemPlaybackSummaryEvent -> IO CLong
variantSwitchCount avMetricPlayerItemPlaybackSummaryEvent =
  sendMessage avMetricPlayerItemPlaybackSummaryEvent variantSwitchCountSelector

-- | Returns the total duration of playback in seconds.
--
-- ObjC selector: @- playbackDuration@
playbackDuration :: IsAVMetricPlayerItemPlaybackSummaryEvent avMetricPlayerItemPlaybackSummaryEvent => avMetricPlayerItemPlaybackSummaryEvent -> IO CLong
playbackDuration avMetricPlayerItemPlaybackSummaryEvent =
  sendMessage avMetricPlayerItemPlaybackSummaryEvent playbackDurationSelector

-- | Returns the total number of media requests performed by the player. This includes playlist requests, media segment requests, and content key requests.
--
-- ObjC selector: @- mediaResourceRequestCount@
mediaResourceRequestCount :: IsAVMetricPlayerItemPlaybackSummaryEvent avMetricPlayerItemPlaybackSummaryEvent => avMetricPlayerItemPlaybackSummaryEvent -> IO CLong
mediaResourceRequestCount avMetricPlayerItemPlaybackSummaryEvent =
  sendMessage avMetricPlayerItemPlaybackSummaryEvent mediaResourceRequestCountSelector

-- | Returns the total time spent recovering from a stall event.
--
-- ObjC selector: @- timeSpentRecoveringFromStall@
timeSpentRecoveringFromStall :: IsAVMetricPlayerItemPlaybackSummaryEvent avMetricPlayerItemPlaybackSummaryEvent => avMetricPlayerItemPlaybackSummaryEvent -> IO CDouble
timeSpentRecoveringFromStall avMetricPlayerItemPlaybackSummaryEvent =
  sendMessage avMetricPlayerItemPlaybackSummaryEvent timeSpentRecoveringFromStallSelector

-- | Returns the total time spent in initial startup of playback.
--
-- ObjC selector: @- timeSpentInInitialStartup@
timeSpentInInitialStartup :: IsAVMetricPlayerItemPlaybackSummaryEvent avMetricPlayerItemPlaybackSummaryEvent => avMetricPlayerItemPlaybackSummaryEvent -> IO CDouble
timeSpentInInitialStartup avMetricPlayerItemPlaybackSummaryEvent =
  sendMessage avMetricPlayerItemPlaybackSummaryEvent timeSpentInInitialStartupSelector

-- | Returns the playtime weighted average bitrate played in bits / second.
--
-- ObjC selector: @- timeWeightedAverageBitrate@
timeWeightedAverageBitrate :: IsAVMetricPlayerItemPlaybackSummaryEvent avMetricPlayerItemPlaybackSummaryEvent => avMetricPlayerItemPlaybackSummaryEvent -> IO CLong
timeWeightedAverageBitrate avMetricPlayerItemPlaybackSummaryEvent =
  sendMessage avMetricPlayerItemPlaybackSummaryEvent timeWeightedAverageBitrateSelector

-- | Returns the playtime weighted peak bitrate played in bits / second.
--
-- ObjC selector: @- timeWeightedPeakBitrate@
timeWeightedPeakBitrate :: IsAVMetricPlayerItemPlaybackSummaryEvent avMetricPlayerItemPlaybackSummaryEvent => avMetricPlayerItemPlaybackSummaryEvent -> IO CLong
timeWeightedPeakBitrate avMetricPlayerItemPlaybackSummaryEvent =
  sendMessage avMetricPlayerItemPlaybackSummaryEvent timeWeightedPeakBitrateSelector

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @init@
initSelector :: Selector '[] (Id AVMetricPlayerItemPlaybackSummaryEvent)
initSelector = mkSelector "init"

-- | @Selector@ for @new@
newSelector :: Selector '[] (Id AVMetricPlayerItemPlaybackSummaryEvent)
newSelector = mkSelector "new"

-- | @Selector@ for @errorEvent@
errorEventSelector :: Selector '[] (Id AVMetricErrorEvent)
errorEventSelector = mkSelector "errorEvent"

-- | @Selector@ for @recoverableErrorCount@
recoverableErrorCountSelector :: Selector '[] CLong
recoverableErrorCountSelector = mkSelector "recoverableErrorCount"

-- | @Selector@ for @stallCount@
stallCountSelector :: Selector '[] CLong
stallCountSelector = mkSelector "stallCount"

-- | @Selector@ for @variantSwitchCount@
variantSwitchCountSelector :: Selector '[] CLong
variantSwitchCountSelector = mkSelector "variantSwitchCount"

-- | @Selector@ for @playbackDuration@
playbackDurationSelector :: Selector '[] CLong
playbackDurationSelector = mkSelector "playbackDuration"

-- | @Selector@ for @mediaResourceRequestCount@
mediaResourceRequestCountSelector :: Selector '[] CLong
mediaResourceRequestCountSelector = mkSelector "mediaResourceRequestCount"

-- | @Selector@ for @timeSpentRecoveringFromStall@
timeSpentRecoveringFromStallSelector :: Selector '[] CDouble
timeSpentRecoveringFromStallSelector = mkSelector "timeSpentRecoveringFromStall"

-- | @Selector@ for @timeSpentInInitialStartup@
timeSpentInInitialStartupSelector :: Selector '[] CDouble
timeSpentInInitialStartupSelector = mkSelector "timeSpentInInitialStartup"

-- | @Selector@ for @timeWeightedAverageBitrate@
timeWeightedAverageBitrateSelector :: Selector '[] CLong
timeWeightedAverageBitrateSelector = mkSelector "timeWeightedAverageBitrate"

-- | @Selector@ for @timeWeightedPeakBitrate@
timeWeightedPeakBitrateSelector :: Selector '[] CLong
timeWeightedPeakBitrateSelector = mkSelector "timeWeightedPeakBitrate"

