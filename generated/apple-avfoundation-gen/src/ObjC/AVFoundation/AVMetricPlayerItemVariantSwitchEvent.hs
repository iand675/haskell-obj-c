{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Represents a metric event when variant switch was completed.
--
-- Subclasses of this type that are used from Swift must fulfill the requirements of a Sendable type.
--
-- Generated bindings for @AVMetricPlayerItemVariantSwitchEvent@.
module ObjC.AVFoundation.AVMetricPlayerItemVariantSwitchEvent
  ( AVMetricPlayerItemVariantSwitchEvent
  , IsAVMetricPlayerItemVariantSwitchEvent(..)
  , init_
  , new
  , fromVariant
  , toVariant
  , loadedTimeRanges
  , videoRendition
  , audioRendition
  , subtitleRendition
  , didSucceed
  , audioRenditionSelector
  , didSucceedSelector
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
init_ :: IsAVMetricPlayerItemVariantSwitchEvent avMetricPlayerItemVariantSwitchEvent => avMetricPlayerItemVariantSwitchEvent -> IO (Id AVMetricPlayerItemVariantSwitchEvent)
init_ avMetricPlayerItemVariantSwitchEvent =
  sendOwnedMessage avMetricPlayerItemVariantSwitchEvent initSelector

-- | @+ new@
new :: IO (Id AVMetricPlayerItemVariantSwitchEvent)
new  =
  do
    cls' <- getRequiredClass "AVMetricPlayerItemVariantSwitchEvent"
    sendOwnedClassMessage cls' newSelector

-- | Returns the variant before the switch. If no value is available, returns nil
--
-- ObjC selector: @- fromVariant@
fromVariant :: IsAVMetricPlayerItemVariantSwitchEvent avMetricPlayerItemVariantSwitchEvent => avMetricPlayerItemVariantSwitchEvent -> IO (Id AVAssetVariant)
fromVariant avMetricPlayerItemVariantSwitchEvent =
  sendMessage avMetricPlayerItemVariantSwitchEvent fromVariantSelector

-- | Returns the variant after the switch.
--
-- ObjC selector: @- toVariant@
toVariant :: IsAVMetricPlayerItemVariantSwitchEvent avMetricPlayerItemVariantSwitchEvent => avMetricPlayerItemVariantSwitchEvent -> IO (Id AVAssetVariant)
toVariant avMetricPlayerItemVariantSwitchEvent =
  sendMessage avMetricPlayerItemVariantSwitchEvent toVariantSelector

-- | This property provides a collection of time ranges for which the player has the media data readily available. The ranges provided might be discontinuous.
--
-- Returns an NSArray of NSValues containing CMTimeRanges.
--
-- ObjC selector: @- loadedTimeRanges@
loadedTimeRanges :: IsAVMetricPlayerItemVariantSwitchEvent avMetricPlayerItemVariantSwitchEvent => avMetricPlayerItemVariantSwitchEvent -> IO (Id NSArray)
loadedTimeRanges avMetricPlayerItemVariantSwitchEvent =
  sendMessage avMetricPlayerItemVariantSwitchEvent loadedTimeRangesSelector

-- | Represents the currently selected video rendition's identifiers.
--
-- Subclasses of this type that are used from Swift must fulfill the requirements of a Sendable type.
--
-- ObjC selector: @- videoRendition@
videoRendition :: IsAVMetricPlayerItemVariantSwitchEvent avMetricPlayerItemVariantSwitchEvent => avMetricPlayerItemVariantSwitchEvent -> IO (Id AVMetricMediaRendition)
videoRendition avMetricPlayerItemVariantSwitchEvent =
  sendMessage avMetricPlayerItemVariantSwitchEvent videoRenditionSelector

-- | Represents the currently selected video rendition's identifiers.
--
-- Subclasses of this type that are used from Swift must fulfill the requirements of a Sendable type.
--
-- ObjC selector: @- audioRendition@
audioRendition :: IsAVMetricPlayerItemVariantSwitchEvent avMetricPlayerItemVariantSwitchEvent => avMetricPlayerItemVariantSwitchEvent -> IO (Id AVMetricMediaRendition)
audioRendition avMetricPlayerItemVariantSwitchEvent =
  sendMessage avMetricPlayerItemVariantSwitchEvent audioRenditionSelector

-- | Represents the currently selected audio rendition's identifiers.
--
-- Subclasses of this type that are used from Swift must fulfill the requirements of a Sendable type.
--
-- ObjC selector: @- subtitleRendition@
subtitleRendition :: IsAVMetricPlayerItemVariantSwitchEvent avMetricPlayerItemVariantSwitchEvent => avMetricPlayerItemVariantSwitchEvent -> IO (Id AVMetricMediaRendition)
subtitleRendition avMetricPlayerItemVariantSwitchEvent =
  sendMessage avMetricPlayerItemVariantSwitchEvent subtitleRenditionSelector

-- | Returns if the switch did succeed.
--
-- ObjC selector: @- didSucceed@
didSucceed :: IsAVMetricPlayerItemVariantSwitchEvent avMetricPlayerItemVariantSwitchEvent => avMetricPlayerItemVariantSwitchEvent -> IO Bool
didSucceed avMetricPlayerItemVariantSwitchEvent =
  sendMessage avMetricPlayerItemVariantSwitchEvent didSucceedSelector

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @init@
initSelector :: Selector '[] (Id AVMetricPlayerItemVariantSwitchEvent)
initSelector = mkSelector "init"

-- | @Selector@ for @new@
newSelector :: Selector '[] (Id AVMetricPlayerItemVariantSwitchEvent)
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

-- | @Selector@ for @didSucceed@
didSucceedSelector :: Selector '[] Bool
didSucceedSelector = mkSelector "didSucceed"

