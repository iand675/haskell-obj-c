{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | AVPlayerItemIntegratedTimelineSnapshot
--
-- AVPlayerItemIntegratedTimelineSnapshot provides an immutable representation of inspectable details from an AVPlayerItemIntegratedTimeline.
--
-- An instance of AVPlayerItemIntegratedTimelineSnapshot is an immutable snapshot representation of inspectable details from an AVPlayerItemIntegratedTimeline. As playback progresses,AVPlayerItemIntegratedTimelineSnapshot will not reflect the new timeline state. One can request a new snapshot instance from an AVPlayerItemIntegratedTimeline to reflect the latest timeline state.	Subclasses of this type that are used from Swift must fulfill the requirements of a Sendable type.
--
-- Generated bindings for @AVPlayerItemIntegratedTimelineSnapshot@.
module ObjC.AVFoundation.AVPlayerItemIntegratedTimelineSnapshot
  ( AVPlayerItemIntegratedTimelineSnapshot
  , IsAVPlayerItemIntegratedTimelineSnapshot(..)
  , init_
  , new
  , currentSegment
  , segments
  , currentDate
  , currentDateSelector
  , currentSegmentSelector
  , initSelector
  , newSelector
  , segmentsSelector


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
init_ :: IsAVPlayerItemIntegratedTimelineSnapshot avPlayerItemIntegratedTimelineSnapshot => avPlayerItemIntegratedTimelineSnapshot -> IO (Id AVPlayerItemIntegratedTimelineSnapshot)
init_ avPlayerItemIntegratedTimelineSnapshot =
  sendOwnedMessage avPlayerItemIntegratedTimelineSnapshot initSelector

-- | @+ new@
new :: IO (Id AVPlayerItemIntegratedTimelineSnapshot)
new  =
  do
    cls' <- getRequiredClass "AVPlayerItemIntegratedTimelineSnapshot"
    sendOwnedClassMessage cls' newSelector

-- | currentSegment
--
-- Returns the current AVPlayerItemSegment playback is traversing.
--
-- ObjC selector: @- currentSegment@
currentSegment :: IsAVPlayerItemIntegratedTimelineSnapshot avPlayerItemIntegratedTimelineSnapshot => avPlayerItemIntegratedTimelineSnapshot -> IO (Id AVPlayerItemSegment)
currentSegment avPlayerItemIntegratedTimelineSnapshot =
  sendMessage avPlayerItemIntegratedTimelineSnapshot currentSegmentSelector

-- | segments
--
-- Returns an array of AVPlayerItemSegment for the snapshot.
--
-- Returns an array of AVPlayerItemSegment. The segments are presented in chronological order, contiguous from the previous element, and non-overlapping.
--
-- ObjC selector: @- segments@
segments :: IsAVPlayerItemIntegratedTimelineSnapshot avPlayerItemIntegratedTimelineSnapshot => avPlayerItemIntegratedTimelineSnapshot -> IO (Id NSArray)
segments avPlayerItemIntegratedTimelineSnapshot =
  sendMessage avPlayerItemIntegratedTimelineSnapshot segmentsSelector

-- | currentDate
--
-- Returns the  current date when the snapshot was taken, or nil if playback is not mapped to any date.
--
-- ObjC selector: @- currentDate@
currentDate :: IsAVPlayerItemIntegratedTimelineSnapshot avPlayerItemIntegratedTimelineSnapshot => avPlayerItemIntegratedTimelineSnapshot -> IO (Id NSDate)
currentDate avPlayerItemIntegratedTimelineSnapshot =
  sendMessage avPlayerItemIntegratedTimelineSnapshot currentDateSelector

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @init@
initSelector :: Selector '[] (Id AVPlayerItemIntegratedTimelineSnapshot)
initSelector = mkSelector "init"

-- | @Selector@ for @new@
newSelector :: Selector '[] (Id AVPlayerItemIntegratedTimelineSnapshot)
newSelector = mkSelector "new"

-- | @Selector@ for @currentSegment@
currentSegmentSelector :: Selector '[] (Id AVPlayerItemSegment)
currentSegmentSelector = mkSelector "currentSegment"

-- | @Selector@ for @segments@
segmentsSelector :: Selector '[] (Id NSArray)
segmentsSelector = mkSelector "segments"

-- | @Selector@ for @currentDate@
currentDateSelector :: Selector '[] (Id NSDate)
currentDateSelector = mkSelector "currentDate"

