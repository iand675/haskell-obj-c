{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Represents a metric event when variant switch was attempted.
--
-- Subclasses of this type that are used from Swift must fulfill the requirements of a Sendable type.
--
-- Generated bindings for @AVMetricPlayerItemVariantSwitchStartEvent@.
module ObjC.AVFoundation.AVMetricPlayerItemVariantSwitchStartEvent
  ( AVMetricPlayerItemVariantSwitchStartEvent
  , IsAVMetricPlayerItemVariantSwitchStartEvent(..)
  , init_
  , new
  , fromVariant
  , toVariant
  , loadedTimeRanges
  , videoRendition
  , audioRendition
  , subtitleRendition
  , audioRenditionSelector
  , fromVariantSelector
  , initSelector
  , loadedTimeRangesSelector
  , newSelector
  , subtitleRenditionSelector
  , toVariantSelector
  , videoRenditionSelector


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
init_ :: IsAVMetricPlayerItemVariantSwitchStartEvent avMetricPlayerItemVariantSwitchStartEvent => avMetricPlayerItemVariantSwitchStartEvent -> IO (Id AVMetricPlayerItemVariantSwitchStartEvent)
init_ avMetricPlayerItemVariantSwitchStartEvent =
  sendOwnedMessage avMetricPlayerItemVariantSwitchStartEvent initSelector

-- | @+ new@
new :: IO (Id AVMetricPlayerItemVariantSwitchStartEvent)
new  =
  do
    cls' <- getRequiredClass "AVMetricPlayerItemVariantSwitchStartEvent"
    sendOwnedClassMessage cls' newSelector

-- | Returns the variant from which the switch is attempted. If no value is available, returns nil
--
-- ObjC selector: @- fromVariant@
fromVariant :: IsAVMetricPlayerItemVariantSwitchStartEvent avMetricPlayerItemVariantSwitchStartEvent => avMetricPlayerItemVariantSwitchStartEvent -> IO (Id AVAssetVariant)
fromVariant avMetricPlayerItemVariantSwitchStartEvent =
  sendMessage avMetricPlayerItemVariantSwitchStartEvent fromVariantSelector

-- | Returns the variant to which the switch is attempted.
--
-- ObjC selector: @- toVariant@
toVariant :: IsAVMetricPlayerItemVariantSwitchStartEvent avMetricPlayerItemVariantSwitchStartEvent => avMetricPlayerItemVariantSwitchStartEvent -> IO (Id AVAssetVariant)
toVariant avMetricPlayerItemVariantSwitchStartEvent =
  sendMessage avMetricPlayerItemVariantSwitchStartEvent toVariantSelector

-- | This property provides a collection of time ranges for which the player has the media data readily available. The ranges provided might be discontinuous.
--
-- Returns an NSArray of NSValues containing CMTimeRanges.
--
-- ObjC selector: @- loadedTimeRanges@
loadedTimeRanges :: IsAVMetricPlayerItemVariantSwitchStartEvent avMetricPlayerItemVariantSwitchStartEvent => avMetricPlayerItemVariantSwitchStartEvent -> IO (Id NSArray)
loadedTimeRanges avMetricPlayerItemVariantSwitchStartEvent =
  sendMessage avMetricPlayerItemVariantSwitchStartEvent loadedTimeRangesSelector

-- | videoRendition
--
-- Contains information corresponding to the currently selected video rendition.
--
-- ObjC selector: @- videoRendition@
videoRendition :: IsAVMetricPlayerItemVariantSwitchStartEvent avMetricPlayerItemVariantSwitchStartEvent => avMetricPlayerItemVariantSwitchStartEvent -> IO (Id AVMetricMediaRendition)
videoRendition avMetricPlayerItemVariantSwitchStartEvent =
  sendMessage avMetricPlayerItemVariantSwitchStartEvent videoRenditionSelector

-- | audioRendition
--
-- Contains information corresponding to the currently selected audio rendition.
--
-- ObjC selector: @- audioRendition@
audioRendition :: IsAVMetricPlayerItemVariantSwitchStartEvent avMetricPlayerItemVariantSwitchStartEvent => avMetricPlayerItemVariantSwitchStartEvent -> IO (Id AVMetricMediaRendition)
audioRendition avMetricPlayerItemVariantSwitchStartEvent =
  sendMessage avMetricPlayerItemVariantSwitchStartEvent audioRenditionSelector

-- | subtitleRendition
--
-- Contains information corresponding to the currently selected subtitle rendition.
--
-- ObjC selector: @- subtitleRendition@
subtitleRendition :: IsAVMetricPlayerItemVariantSwitchStartEvent avMetricPlayerItemVariantSwitchStartEvent => avMetricPlayerItemVariantSwitchStartEvent -> IO (Id AVMetricMediaRendition)
subtitleRendition avMetricPlayerItemVariantSwitchStartEvent =
  sendMessage avMetricPlayerItemVariantSwitchStartEvent subtitleRenditionSelector

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @init@
initSelector :: Selector '[] (Id AVMetricPlayerItemVariantSwitchStartEvent)
initSelector = mkSelector "init"

-- | @Selector@ for @new@
newSelector :: Selector '[] (Id AVMetricPlayerItemVariantSwitchStartEvent)
newSelector = mkSelector "new"

-- | @Selector@ for @fromVariant@
fromVariantSelector :: Selector '[] (Id AVAssetVariant)
fromVariantSelector = mkSelector "fromVariant"

-- | @Selector@ for @toVariant@
toVariantSelector :: Selector '[] (Id AVAssetVariant)
toVariantSelector = mkSelector "toVariant"

-- | @Selector@ for @loadedTimeRanges@
loadedTimeRangesSelector :: Selector '[] (Id NSArray)
loadedTimeRangesSelector = mkSelector "loadedTimeRanges"

-- | @Selector@ for @videoRendition@
videoRenditionSelector :: Selector '[] (Id AVMetricMediaRendition)
videoRenditionSelector = mkSelector "videoRendition"

-- | @Selector@ for @audioRendition@
audioRenditionSelector :: Selector '[] (Id AVMetricMediaRendition)
audioRenditionSelector = mkSelector "audioRendition"

-- | @Selector@ for @subtitleRendition@
subtitleRenditionSelector :: Selector '[] (Id AVMetricMediaRendition)
subtitleRenditionSelector = mkSelector "subtitleRendition"

