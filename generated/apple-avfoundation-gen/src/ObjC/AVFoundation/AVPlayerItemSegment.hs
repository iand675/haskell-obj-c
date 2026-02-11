{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | AVPlayerItemSegment
--
-- Representing a segment of time on the integrated timeline. Segments are immutable objects.
--
-- Subclasses of this type that are used from Swift must fulfill the requirements of a Sendable type.
--
-- Generated bindings for @AVPlayerItemSegment@.
module ObjC.AVFoundation.AVPlayerItemSegment
  ( AVPlayerItemSegment
  , IsAVPlayerItemSegment(..)
  , init_
  , new
  , segmentType
  , loadedTimeRanges
  , startDate
  , interstitialEvent
  , initSelector
  , newSelector
  , segmentTypeSelector
  , loadedTimeRangesSelector
  , startDateSelector
  , interstitialEventSelector

  -- * Enum types
  , AVPlayerItemSegmentType(AVPlayerItemSegmentType)
  , pattern AVPlayerItemSegmentTypePrimary
  , pattern AVPlayerItemSegmentTypeInterstitial

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
import ObjC.AVFoundation.Internal.Enums
import ObjC.Foundation.Internal.Classes

-- | @- init@
init_ :: IsAVPlayerItemSegment avPlayerItemSegment => avPlayerItemSegment -> IO (Id AVPlayerItemSegment)
init_ avPlayerItemSegment  =
    sendMsg avPlayerItemSegment (mkSelector "init") (retPtr retVoid) [] >>= ownedObject . castPtr

-- | @+ new@
new :: IO (Id AVPlayerItemSegment)
new  =
  do
    cls' <- getRequiredClass "AVPlayerItemSegment"
    sendClassMsg cls' (mkSelector "new") (retPtr retVoid) [] >>= ownedObject . castPtr

-- | segmentType
--
-- The type of content this segment represents.
--
-- ObjC selector: @- segmentType@
segmentType :: IsAVPlayerItemSegment avPlayerItemSegment => avPlayerItemSegment -> IO AVPlayerItemSegmentType
segmentType avPlayerItemSegment  =
    fmap (coerce :: CLong -> AVPlayerItemSegmentType) $ sendMsg avPlayerItemSegment (mkSelector "segmentType") retCLong []

-- | loadedTimeRanges
--
-- This property provides a collection of time ranges for the segment if media data is readily available. The ranges provided might be discontinuous.
--
-- Returns an NSArray of NSValues containing CMTimeRanges. Loaded time ranges will be within the timeMapping's target timeRange. Loaded time ranges will be empty for interstitial events that occupy a single point in time.
--
-- ObjC selector: @- loadedTimeRanges@
loadedTimeRanges :: IsAVPlayerItemSegment avPlayerItemSegment => avPlayerItemSegment -> IO (Id NSArray)
loadedTimeRanges avPlayerItemSegment  =
    sendMsg avPlayerItemSegment (mkSelector "loadedTimeRanges") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | startDate
--
-- The date this segment starts at.
--
-- The date this segment starts at. This value will be nil if the primary item does not contain dates.
--
-- ObjC selector: @- startDate@
startDate :: IsAVPlayerItemSegment avPlayerItemSegment => avPlayerItemSegment -> IO (Id NSDate)
startDate avPlayerItemSegment  =
    sendMsg avPlayerItemSegment (mkSelector "startDate") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | interstitialEvent
--
-- The associated interstitial event for this segment.
--
-- The associated interstitial event for this segment. This value will be nil for segments representing playback of the primary itme.
--
-- ObjC selector: @- interstitialEvent@
interstitialEvent :: IsAVPlayerItemSegment avPlayerItemSegment => avPlayerItemSegment -> IO (Id AVPlayerInterstitialEvent)
interstitialEvent avPlayerItemSegment  =
    sendMsg avPlayerItemSegment (mkSelector "interstitialEvent") (retPtr retVoid) [] >>= retainedObject . castPtr

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @init@
initSelector :: Selector
initSelector = mkSelector "init"

-- | @Selector@ for @new@
newSelector :: Selector
newSelector = mkSelector "new"

-- | @Selector@ for @segmentType@
segmentTypeSelector :: Selector
segmentTypeSelector = mkSelector "segmentType"

-- | @Selector@ for @loadedTimeRanges@
loadedTimeRangesSelector :: Selector
loadedTimeRangesSelector = mkSelector "loadedTimeRanges"

-- | @Selector@ for @startDate@
startDateSelector :: Selector
startDateSelector = mkSelector "startDate"

-- | @Selector@ for @interstitialEvent@
interstitialEventSelector :: Selector
interstitialEventSelector = mkSelector "interstitialEvent"

