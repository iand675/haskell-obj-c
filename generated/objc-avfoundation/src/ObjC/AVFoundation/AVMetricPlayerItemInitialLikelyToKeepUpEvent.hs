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
  , initSelector
  , newSelector
  , playlistRequestEventsSelector
  , mediaSegmentRequestEventsSelector
  , contentKeyRequestEventsSelector


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
init_ :: IsAVMetricPlayerItemInitialLikelyToKeepUpEvent avMetricPlayerItemInitialLikelyToKeepUpEvent => avMetricPlayerItemInitialLikelyToKeepUpEvent -> IO (Id AVMetricPlayerItemInitialLikelyToKeepUpEvent)
init_ avMetricPlayerItemInitialLikelyToKeepUpEvent  =
  sendMsg avMetricPlayerItemInitialLikelyToKeepUpEvent (mkSelector "init") (retPtr retVoid) [] >>= ownedObject . castPtr

-- | @+ new@
new :: IO (Id AVMetricPlayerItemInitialLikelyToKeepUpEvent)
new  =
  do
    cls' <- getRequiredClass "AVMetricPlayerItemInitialLikelyToKeepUpEvent"
    sendClassMsg cls' (mkSelector "new") (retPtr retVoid) [] >>= ownedObject . castPtr

-- | Returns the playlist request events required to reach likely to keep up.
--
-- ObjC selector: @- playlistRequestEvents@
playlistRequestEvents :: IsAVMetricPlayerItemInitialLikelyToKeepUpEvent avMetricPlayerItemInitialLikelyToKeepUpEvent => avMetricPlayerItemInitialLikelyToKeepUpEvent -> IO (Id NSArray)
playlistRequestEvents avMetricPlayerItemInitialLikelyToKeepUpEvent  =
  sendMsg avMetricPlayerItemInitialLikelyToKeepUpEvent (mkSelector "playlistRequestEvents") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | Returns the media segment request events required to reach likely to keep up.
--
-- ObjC selector: @- mediaSegmentRequestEvents@
mediaSegmentRequestEvents :: IsAVMetricPlayerItemInitialLikelyToKeepUpEvent avMetricPlayerItemInitialLikelyToKeepUpEvent => avMetricPlayerItemInitialLikelyToKeepUpEvent -> IO (Id NSArray)
mediaSegmentRequestEvents avMetricPlayerItemInitialLikelyToKeepUpEvent  =
  sendMsg avMetricPlayerItemInitialLikelyToKeepUpEvent (mkSelector "mediaSegmentRequestEvents") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | Returns the content key request required to reach likely to keep up.
--
-- ObjC selector: @- contentKeyRequestEvents@
contentKeyRequestEvents :: IsAVMetricPlayerItemInitialLikelyToKeepUpEvent avMetricPlayerItemInitialLikelyToKeepUpEvent => avMetricPlayerItemInitialLikelyToKeepUpEvent -> IO (Id NSArray)
contentKeyRequestEvents avMetricPlayerItemInitialLikelyToKeepUpEvent  =
  sendMsg avMetricPlayerItemInitialLikelyToKeepUpEvent (mkSelector "contentKeyRequestEvents") (retPtr retVoid) [] >>= retainedObject . castPtr

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @init@
initSelector :: Selector
initSelector = mkSelector "init"

-- | @Selector@ for @new@
newSelector :: Selector
newSelector = mkSelector "new"

-- | @Selector@ for @playlistRequestEvents@
playlistRequestEventsSelector :: Selector
playlistRequestEventsSelector = mkSelector "playlistRequestEvents"

-- | @Selector@ for @mediaSegmentRequestEvents@
mediaSegmentRequestEventsSelector :: Selector
mediaSegmentRequestEventsSelector = mkSelector "mediaSegmentRequestEvents"

-- | @Selector@ for @contentKeyRequestEvents@
contentKeyRequestEventsSelector :: Selector
contentKeyRequestEventsSelector = mkSelector "contentKeyRequestEvents"

