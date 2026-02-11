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
  , initSelector
  , newSelector
  , currentSegmentSelector
  , segmentsSelector
  , currentDateSelector


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
init_ :: IsAVPlayerItemIntegratedTimelineSnapshot avPlayerItemIntegratedTimelineSnapshot => avPlayerItemIntegratedTimelineSnapshot -> IO (Id AVPlayerItemIntegratedTimelineSnapshot)
init_ avPlayerItemIntegratedTimelineSnapshot  =
  sendMsg avPlayerItemIntegratedTimelineSnapshot (mkSelector "init") (retPtr retVoid) [] >>= ownedObject . castPtr

-- | @+ new@
new :: IO (Id AVPlayerItemIntegratedTimelineSnapshot)
new  =
  do
    cls' <- getRequiredClass "AVPlayerItemIntegratedTimelineSnapshot"
    sendClassMsg cls' (mkSelector "new") (retPtr retVoid) [] >>= ownedObject . castPtr

-- | currentSegment
--
-- Returns the current AVPlayerItemSegment playback is traversing.
--
-- ObjC selector: @- currentSegment@
currentSegment :: IsAVPlayerItemIntegratedTimelineSnapshot avPlayerItemIntegratedTimelineSnapshot => avPlayerItemIntegratedTimelineSnapshot -> IO (Id AVPlayerItemSegment)
currentSegment avPlayerItemIntegratedTimelineSnapshot  =
  sendMsg avPlayerItemIntegratedTimelineSnapshot (mkSelector "currentSegment") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | segments
--
-- Returns an array of AVPlayerItemSegment for the snapshot.
--
-- Returns an array of AVPlayerItemSegment. The segments are presented in chronological order, contiguous from the previous element, and non-overlapping.
--
-- ObjC selector: @- segments@
segments :: IsAVPlayerItemIntegratedTimelineSnapshot avPlayerItemIntegratedTimelineSnapshot => avPlayerItemIntegratedTimelineSnapshot -> IO (Id NSArray)
segments avPlayerItemIntegratedTimelineSnapshot  =
  sendMsg avPlayerItemIntegratedTimelineSnapshot (mkSelector "segments") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | currentDate
--
-- Returns the  current date when the snapshot was taken, or nil if playback is not mapped to any date.
--
-- ObjC selector: @- currentDate@
currentDate :: IsAVPlayerItemIntegratedTimelineSnapshot avPlayerItemIntegratedTimelineSnapshot => avPlayerItemIntegratedTimelineSnapshot -> IO (Id NSDate)
currentDate avPlayerItemIntegratedTimelineSnapshot  =
  sendMsg avPlayerItemIntegratedTimelineSnapshot (mkSelector "currentDate") (retPtr retVoid) [] >>= retainedObject . castPtr

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @init@
initSelector :: Selector
initSelector = mkSelector "init"

-- | @Selector@ for @new@
newSelector :: Selector
newSelector = mkSelector "new"

-- | @Selector@ for @currentSegment@
currentSegmentSelector :: Selector
currentSegmentSelector = mkSelector "currentSegment"

-- | @Selector@ for @segments@
segmentsSelector :: Selector
segmentsSelector = mkSelector "segments"

-- | @Selector@ for @currentDate@
currentDateSelector :: Selector
currentDateSelector = mkSelector "currentDate"

