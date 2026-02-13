{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Represents a metric event when playback was likely to play through without stalling.
--
-- Subclasses of this type that are used from Swift must fulfill the requirements of a Sendable type.
--
-- Generated bindings for @AVMetricPlayerItemLikelyToKeepUpEvent@.
module ObjC.AVFoundation.AVMetricPlayerItemLikelyToKeepUpEvent
  ( AVMetricPlayerItemLikelyToKeepUpEvent
  , IsAVMetricPlayerItemLikelyToKeepUpEvent(..)
  , init_
  , new
  , variant
  , timeTaken
  , loadedTimeRanges
  , initSelector
  , loadedTimeRangesSelector
  , newSelector
  , timeTakenSelector
  , variantSelector


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
init_ :: IsAVMetricPlayerItemLikelyToKeepUpEvent avMetricPlayerItemLikelyToKeepUpEvent => avMetricPlayerItemLikelyToKeepUpEvent -> IO (Id AVMetricPlayerItemLikelyToKeepUpEvent)
init_ avMetricPlayerItemLikelyToKeepUpEvent =
  sendOwnedMessage avMetricPlayerItemLikelyToKeepUpEvent initSelector

-- | @+ new@
new :: IO (Id AVMetricPlayerItemLikelyToKeepUpEvent)
new  =
  do
    cls' <- getRequiredClass "AVMetricPlayerItemLikelyToKeepUpEvent"
    sendOwnedClassMessage cls' newSelector

-- | Returns the variant selected at the time likely to keep up is achieved. If no value is present, returns nil.
--
-- ObjC selector: @- variant@
variant :: IsAVMetricPlayerItemLikelyToKeepUpEvent avMetricPlayerItemLikelyToKeepUpEvent => avMetricPlayerItemLikelyToKeepUpEvent -> IO (Id AVAssetVariant)
variant avMetricPlayerItemLikelyToKeepUpEvent =
  sendMessage avMetricPlayerItemLikelyToKeepUpEvent variantSelector

-- | Returns the total time taken to reach likely to keep up.
--
-- ObjC selector: @- timeTaken@
timeTaken :: IsAVMetricPlayerItemLikelyToKeepUpEvent avMetricPlayerItemLikelyToKeepUpEvent => avMetricPlayerItemLikelyToKeepUpEvent -> IO CDouble
timeTaken avMetricPlayerItemLikelyToKeepUpEvent =
  sendMessage avMetricPlayerItemLikelyToKeepUpEvent timeTakenSelector

-- | This property provides a collection of time ranges for which the player has the media data readily available. The ranges provided might be discontinuous.
--
-- Returns an NSArray of NSValues containing CMTimeRanges.
--
-- ObjC selector: @- loadedTimeRanges@
loadedTimeRanges :: IsAVMetricPlayerItemLikelyToKeepUpEvent avMetricPlayerItemLikelyToKeepUpEvent => avMetricPlayerItemLikelyToKeepUpEvent -> IO (Id NSArray)
loadedTimeRanges avMetricPlayerItemLikelyToKeepUpEvent =
  sendMessage avMetricPlayerItemLikelyToKeepUpEvent loadedTimeRangesSelector

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @init@
initSelector :: Selector '[] (Id AVMetricPlayerItemLikelyToKeepUpEvent)
initSelector = mkSelector "init"

-- | @Selector@ for @new@
newSelector :: Selector '[] (Id AVMetricPlayerItemLikelyToKeepUpEvent)
newSelector = mkSelector "new"

-- | @Selector@ for @variant@
variantSelector :: Selector '[] (Id AVAssetVariant)
variantSelector = mkSelector "variant"

-- | @Selector@ for @timeTaken@
timeTakenSelector :: Selector '[] CDouble
timeTakenSelector = mkSelector "timeTaken"

-- | @Selector@ for @loadedTimeRanges@
loadedTimeRangesSelector :: Selector '[] (Id NSArray)
loadedTimeRangesSelector = mkSelector "loadedTimeRanges"

