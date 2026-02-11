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
  , initSelector
  , newSelector
  , errorEventSelector
  , recoverableErrorCountSelector
  , stallCountSelector
  , variantSwitchCountSelector
  , playbackDurationSelector
  , mediaResourceRequestCountSelector
  , timeSpentRecoveringFromStallSelector
  , timeSpentInInitialStartupSelector
  , timeWeightedAverageBitrateSelector
  , timeWeightedPeakBitrateSelector


  ) where

import Foreign.Ptr (Ptr, nullPtr, castPtr)
import Foreign.LibFFI
import Foreign.C.Types
import Data.Int (Int8, Int16)
import Data.Word (Word16)
import Data.Coerce (coerce)

import ObjC.Runtime.Types
import ObjC.Runtime.MsgSend (sendMsg, sendClassMsg)
import ObjC.Runtime.Selector (mkSelector)
import ObjC.Runtime.Class (getRequiredClass)

import ObjC.AVFoundation.Internal.Classes
import ObjC.Foundation.Internal.Classes

-- | @- init@
init_ :: IsAVMetricPlayerItemPlaybackSummaryEvent avMetricPlayerItemPlaybackSummaryEvent => avMetricPlayerItemPlaybackSummaryEvent -> IO (Id AVMetricPlayerItemPlaybackSummaryEvent)
init_ avMetricPlayerItemPlaybackSummaryEvent  =
  sendMsg avMetricPlayerItemPlaybackSummaryEvent (mkSelector "init") (retPtr retVoid) [] >>= ownedObject . castPtr

-- | @+ new@
new :: IO (Id AVMetricPlayerItemPlaybackSummaryEvent)
new  =
  do
    cls' <- getRequiredClass "AVMetricPlayerItemPlaybackSummaryEvent"
    sendClassMsg cls' (mkSelector "new") (retPtr retVoid) [] >>= ownedObject . castPtr

-- | Returns the error event if any. If no value is available, returns nil.
--
-- ObjC selector: @- errorEvent@
errorEvent :: IsAVMetricPlayerItemPlaybackSummaryEvent avMetricPlayerItemPlaybackSummaryEvent => avMetricPlayerItemPlaybackSummaryEvent -> IO (Id AVMetricErrorEvent)
errorEvent avMetricPlayerItemPlaybackSummaryEvent  =
  sendMsg avMetricPlayerItemPlaybackSummaryEvent (mkSelector "errorEvent") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | Returns the total count of recoverable errors encountered during playback. If no errors were encountered, returns 0.
--
-- ObjC selector: @- recoverableErrorCount@
recoverableErrorCount :: IsAVMetricPlayerItemPlaybackSummaryEvent avMetricPlayerItemPlaybackSummaryEvent => avMetricPlayerItemPlaybackSummaryEvent -> IO CLong
recoverableErrorCount avMetricPlayerItemPlaybackSummaryEvent  =
  sendMsg avMetricPlayerItemPlaybackSummaryEvent (mkSelector "recoverableErrorCount") retCLong []

-- | Returns the total count of stalls encountered during playback. If no stalls were encountered, returns 0.
--
-- ObjC selector: @- stallCount@
stallCount :: IsAVMetricPlayerItemPlaybackSummaryEvent avMetricPlayerItemPlaybackSummaryEvent => avMetricPlayerItemPlaybackSummaryEvent -> IO CLong
stallCount avMetricPlayerItemPlaybackSummaryEvent  =
  sendMsg avMetricPlayerItemPlaybackSummaryEvent (mkSelector "stallCount") retCLong []

-- | Returns the total count of variant switch encountered during playback.
--
-- ObjC selector: @- variantSwitchCount@
variantSwitchCount :: IsAVMetricPlayerItemPlaybackSummaryEvent avMetricPlayerItemPlaybackSummaryEvent => avMetricPlayerItemPlaybackSummaryEvent -> IO CLong
variantSwitchCount avMetricPlayerItemPlaybackSummaryEvent  =
  sendMsg avMetricPlayerItemPlaybackSummaryEvent (mkSelector "variantSwitchCount") retCLong []

-- | Returns the total duration of playback in seconds.
--
-- ObjC selector: @- playbackDuration@
playbackDuration :: IsAVMetricPlayerItemPlaybackSummaryEvent avMetricPlayerItemPlaybackSummaryEvent => avMetricPlayerItemPlaybackSummaryEvent -> IO CLong
playbackDuration avMetricPlayerItemPlaybackSummaryEvent  =
  sendMsg avMetricPlayerItemPlaybackSummaryEvent (mkSelector "playbackDuration") retCLong []

-- | Returns the total number of media requests performed by the player. This includes playlist requests, media segment requests, and content key requests.
--
-- ObjC selector: @- mediaResourceRequestCount@
mediaResourceRequestCount :: IsAVMetricPlayerItemPlaybackSummaryEvent avMetricPlayerItemPlaybackSummaryEvent => avMetricPlayerItemPlaybackSummaryEvent -> IO CLong
mediaResourceRequestCount avMetricPlayerItemPlaybackSummaryEvent  =
  sendMsg avMetricPlayerItemPlaybackSummaryEvent (mkSelector "mediaResourceRequestCount") retCLong []

-- | Returns the total time spent recovering from a stall event.
--
-- ObjC selector: @- timeSpentRecoveringFromStall@
timeSpentRecoveringFromStall :: IsAVMetricPlayerItemPlaybackSummaryEvent avMetricPlayerItemPlaybackSummaryEvent => avMetricPlayerItemPlaybackSummaryEvent -> IO CDouble
timeSpentRecoveringFromStall avMetricPlayerItemPlaybackSummaryEvent  =
  sendMsg avMetricPlayerItemPlaybackSummaryEvent (mkSelector "timeSpentRecoveringFromStall") retCDouble []

-- | Returns the total time spent in initial startup of playback.
--
-- ObjC selector: @- timeSpentInInitialStartup@
timeSpentInInitialStartup :: IsAVMetricPlayerItemPlaybackSummaryEvent avMetricPlayerItemPlaybackSummaryEvent => avMetricPlayerItemPlaybackSummaryEvent -> IO CDouble
timeSpentInInitialStartup avMetricPlayerItemPlaybackSummaryEvent  =
  sendMsg avMetricPlayerItemPlaybackSummaryEvent (mkSelector "timeSpentInInitialStartup") retCDouble []

-- | Returns the playtime weighted average bitrate played in bits / second.
--
-- ObjC selector: @- timeWeightedAverageBitrate@
timeWeightedAverageBitrate :: IsAVMetricPlayerItemPlaybackSummaryEvent avMetricPlayerItemPlaybackSummaryEvent => avMetricPlayerItemPlaybackSummaryEvent -> IO CLong
timeWeightedAverageBitrate avMetricPlayerItemPlaybackSummaryEvent  =
  sendMsg avMetricPlayerItemPlaybackSummaryEvent (mkSelector "timeWeightedAverageBitrate") retCLong []

-- | Returns the playtime weighted peak bitrate played in bits / second.
--
-- ObjC selector: @- timeWeightedPeakBitrate@
timeWeightedPeakBitrate :: IsAVMetricPlayerItemPlaybackSummaryEvent avMetricPlayerItemPlaybackSummaryEvent => avMetricPlayerItemPlaybackSummaryEvent -> IO CLong
timeWeightedPeakBitrate avMetricPlayerItemPlaybackSummaryEvent  =
  sendMsg avMetricPlayerItemPlaybackSummaryEvent (mkSelector "timeWeightedPeakBitrate") retCLong []

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @init@
initSelector :: Selector
initSelector = mkSelector "init"

-- | @Selector@ for @new@
newSelector :: Selector
newSelector = mkSelector "new"

-- | @Selector@ for @errorEvent@
errorEventSelector :: Selector
errorEventSelector = mkSelector "errorEvent"

-- | @Selector@ for @recoverableErrorCount@
recoverableErrorCountSelector :: Selector
recoverableErrorCountSelector = mkSelector "recoverableErrorCount"

-- | @Selector@ for @stallCount@
stallCountSelector :: Selector
stallCountSelector = mkSelector "stallCount"

-- | @Selector@ for @variantSwitchCount@
variantSwitchCountSelector :: Selector
variantSwitchCountSelector = mkSelector "variantSwitchCount"

-- | @Selector@ for @playbackDuration@
playbackDurationSelector :: Selector
playbackDurationSelector = mkSelector "playbackDuration"

-- | @Selector@ for @mediaResourceRequestCount@
mediaResourceRequestCountSelector :: Selector
mediaResourceRequestCountSelector = mkSelector "mediaResourceRequestCount"

-- | @Selector@ for @timeSpentRecoveringFromStall@
timeSpentRecoveringFromStallSelector :: Selector
timeSpentRecoveringFromStallSelector = mkSelector "timeSpentRecoveringFromStall"

-- | @Selector@ for @timeSpentInInitialStartup@
timeSpentInInitialStartupSelector :: Selector
timeSpentInInitialStartupSelector = mkSelector "timeSpentInInitialStartup"

-- | @Selector@ for @timeWeightedAverageBitrate@
timeWeightedAverageBitrateSelector :: Selector
timeWeightedAverageBitrateSelector = mkSelector "timeWeightedAverageBitrate"

-- | @Selector@ for @timeWeightedPeakBitrate@
timeWeightedPeakBitrateSelector :: Selector
timeWeightedPeakBitrateSelector = mkSelector "timeWeightedPeakBitrate"

