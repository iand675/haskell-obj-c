{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Represents a metric event when playback was first likely to play through without stalling.
--
-- Subclasses of this type that are used from Swift must fulfill the requirements of a Sendable type.
--
-- Generated bindings for @AVMetricPlayerItemInitialLikelyToKeepUpEvent@.
module ObjC.AVFoundation.AVMetricPlayerItemInitialLikelyToKeepUpEvent
  ( AVMetricPlayerItemInitialLikelyToKeepUpEvent
  , IsAVMetricPlayerItemInitialLikelyToKeepUpEvent(..)
  , init_
  , new
  , playlistRequestEvents
  , mediaSegmentRequestEvents
  , contentKeyRequestEvents
  , contentKeyRequestEventsSelector
  , initSelector
  , mediaSegmentRequestEventsSelector
  , newSelector
  , playlistRequestEventsSelector


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
init_ :: IsAVMetricPlayerItemInitialLikelyToKeepUpEvent avMetricPlayerItemInitialLikelyToKeepUpEvent => avMetricPlayerItemInitialLikelyToKeepUpEvent -> IO (Id AVMetricPlayerItemInitialLikelyToKeepUpEvent)
init_ avMetricPlayerItemInitialLikelyToKeepUpEvent =
  sendOwnedMessage avMetricPlayerItemInitialLikelyToKeepUpEvent initSelector

-- | @+ new@
new :: IO (Id AVMetricPlayerItemInitialLikelyToKeepUpEvent)
new  =
  do
    cls' <- getRequiredClass "AVMetricPlayerItemInitialLikelyToKeepUpEvent"
    sendOwnedClassMessage cls' newSelector

-- | Returns the playlist request events required to reach likely to keep up.
--
-- ObjC selector: @- playlistRequestEvents@
playlistRequestEvents :: IsAVMetricPlayerItemInitialLikelyToKeepUpEvent avMetricPlayerItemInitialLikelyToKeepUpEvent => avMetricPlayerItemInitialLikelyToKeepUpEvent -> IO (Id NSArray)
playlistRequestEvents avMetricPlayerItemInitialLikelyToKeepUpEvent =
  sendMessage avMetricPlayerItemInitialLikelyToKeepUpEvent playlistRequestEventsSelector

-- | Returns the media segment request events required to reach likely to keep up.
--
-- ObjC selector: @- mediaSegmentRequestEvents@
mediaSegmentRequestEvents :: IsAVMetricPlayerItemInitialLikelyToKeepUpEvent avMetricPlayerItemInitialLikelyToKeepUpEvent => avMetricPlayerItemInitialLikelyToKeepUpEvent -> IO (Id NSArray)
mediaSegmentRequestEvents avMetricPlayerItemInitialLikelyToKeepUpEvent =
  sendMessage avMetricPlayerItemInitialLikelyToKeepUpEvent mediaSegmentRequestEventsSelector

-- | Returns the content key request required to reach likely to keep up.
--
-- ObjC selector: @- contentKeyRequestEvents@
contentKeyRequestEvents :: IsAVMetricPlayerItemInitialLikelyToKeepUpEvent avMetricPlayerItemInitialLikelyToKeepUpEvent => avMetricPlayerItemInitialLikelyToKeepUpEvent -> IO (Id NSArray)
contentKeyRequestEvents avMetricPlayerItemInitialLikelyToKeepUpEvent =
  sendMessage avMetricPlayerItemInitialLikelyToKeepUpEvent contentKeyRequestEventsSelector

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @init@
initSelector :: Selector '[] (Id AVMetricPlayerItemInitialLikelyToKeepUpEvent)
initSelector = mkSelector "init"

-- | @Selector@ for @new@
newSelector :: Selector '[] (Id AVMetricPlayerItemInitialLikelyToKeepUpEvent)
newSelector = mkSelector "new"

-- | @Selector@ for @playlistRequestEvents@
playlistRequestEventsSelector :: Selector '[] (Id NSArray)
playlistRequestEventsSelector = mkSelector "playlistRequestEvents"

-- | @Selector@ for @mediaSegmentRequestEvents@
mediaSegmentRequestEventsSelector :: Selector '[] (Id NSArray)
mediaSegmentRequestEventsSelector = mkSelector "mediaSegmentRequestEvents"

-- | @Selector@ for @contentKeyRequestEvents@
contentKeyRequestEventsSelector :: Selector '[] (Id NSArray)
contentKeyRequestEventsSelector = mkSelector "contentKeyRequestEvents"

