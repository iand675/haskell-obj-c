{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE DataKinds #-}
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
  , interstitialEventSelector
  , loadedTimeRangesSelector
  , newSelector
  , segmentTypeSelector
  , startDateSelector

  -- * Enum types
  , AVPlayerItemSegmentType(AVPlayerItemSegmentType)
  , pattern AVPlayerItemSegmentTypePrimary
  , pattern AVPlayerItemSegmentTypeInterstitial

  ) where

import Foreign.Ptr (Ptr, FunPtr)
import Foreign.C.Types

import ObjC.Runtime.Types
import ObjC.Runtime.Message (sendMessage, sendOwnedMessage, sendClassMessage, sendOwnedClassMessage)
import ObjC.Runtime.Selector (mkSelector)
import ObjC.Runtime.Class (getRequiredClass)

import ObjC.AVFoundation.Internal.Classes
import ObjC.AVFoundation.Internal.Enums
import ObjC.Foundation.Internal.Classes

-- | @- init@
init_ :: IsAVPlayerItemSegment avPlayerItemSegment => avPlayerItemSegment -> IO (Id AVPlayerItemSegment)
init_ avPlayerItemSegment =
  sendOwnedMessage avPlayerItemSegment initSelector

-- | @+ new@
new :: IO (Id AVPlayerItemSegment)
new  =
  do
    cls' <- getRequiredClass "AVPlayerItemSegment"
    sendOwnedClassMessage cls' newSelector

-- | segmentType
--
-- The type of content this segment represents.
--
-- ObjC selector: @- segmentType@
segmentType :: IsAVPlayerItemSegment avPlayerItemSegment => avPlayerItemSegment -> IO AVPlayerItemSegmentType
segmentType avPlayerItemSegment =
  sendMessage avPlayerItemSegment segmentTypeSelector

-- | loadedTimeRanges
--
-- This property provides a collection of time ranges for the segment if media data is readily available. The ranges provided might be discontinuous.
--
-- Returns an NSArray of NSValues containing CMTimeRanges. Loaded time ranges will be within the timeMapping's target timeRange. Loaded time ranges will be empty for interstitial events that occupy a single point in time.
--
-- ObjC selector: @- loadedTimeRanges@
loadedTimeRanges :: IsAVPlayerItemSegment avPlayerItemSegment => avPlayerItemSegment -> IO (Id NSArray)
loadedTimeRanges avPlayerItemSegment =
  sendMessage avPlayerItemSegment loadedTimeRangesSelector

-- | startDate
--
-- The date this segment starts at.
--
-- The date this segment starts at. This value will be nil if the primary item does not contain dates.
--
-- ObjC selector: @- startDate@
startDate :: IsAVPlayerItemSegment avPlayerItemSegment => avPlayerItemSegment -> IO (Id NSDate)
startDate avPlayerItemSegment =
  sendMessage avPlayerItemSegment startDateSelector

-- | interstitialEvent
--
-- The associated interstitial event for this segment.
--
-- The associated interstitial event for this segment. This value will be nil for segments representing playback of the primary itme.
--
-- ObjC selector: @- interstitialEvent@
interstitialEvent :: IsAVPlayerItemSegment avPlayerItemSegment => avPlayerItemSegment -> IO (Id AVPlayerInterstitialEvent)
interstitialEvent avPlayerItemSegment =
  sendMessage avPlayerItemSegment interstitialEventSelector

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @init@
initSelector :: Selector '[] (Id AVPlayerItemSegment)
initSelector = mkSelector "init"

-- | @Selector@ for @new@
newSelector :: Selector '[] (Id AVPlayerItemSegment)
newSelector = mkSelector "new"

-- | @Selector@ for @segmentType@
segmentTypeSelector :: Selector '[] AVPlayerItemSegmentType
segmentTypeSelector = mkSelector "segmentType"

-- | @Selector@ for @loadedTimeRanges@
loadedTimeRangesSelector :: Selector '[] (Id NSArray)
loadedTimeRangesSelector = mkSelector "loadedTimeRanges"

-- | @Selector@ for @startDate@
startDateSelector :: Selector '[] (Id NSDate)
startDateSelector = mkSelector "startDate"

-- | @Selector@ for @interstitialEvent@
interstitialEventSelector :: Selector '[] (Id AVPlayerInterstitialEvent)
interstitialEventSelector = mkSelector "interstitialEvent"

