{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Represents a metric event when playback seek completed.
--
-- Subclasses of this type that are used from Swift must fulfill the requirements of a Sendable type.
--
-- Generated bindings for @AVMetricPlayerItemSeekDidCompleteEvent@.
module ObjC.AVFoundation.AVMetricPlayerItemSeekDidCompleteEvent
  ( AVMetricPlayerItemSeekDidCompleteEvent
  , IsAVMetricPlayerItemSeekDidCompleteEvent(..)
  , init_
  , new
  , didSeekInBuffer
  , didSeekInBufferSelector
  , initSelector
  , newSelector


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
init_ :: IsAVMetricPlayerItemSeekDidCompleteEvent avMetricPlayerItemSeekDidCompleteEvent => avMetricPlayerItemSeekDidCompleteEvent -> IO (Id AVMetricPlayerItemSeekDidCompleteEvent)
init_ avMetricPlayerItemSeekDidCompleteEvent =
  sendOwnedMessage avMetricPlayerItemSeekDidCompleteEvent initSelector

-- | @+ new@
new :: IO (Id AVMetricPlayerItemSeekDidCompleteEvent)
new  =
  do
    cls' <- getRequiredClass "AVMetricPlayerItemSeekDidCompleteEvent"
    sendOwnedClassMessage cls' newSelector

-- | Returns whether the seek was performed within the available buffer.
--
-- ObjC selector: @- didSeekInBuffer@
didSeekInBuffer :: IsAVMetricPlayerItemSeekDidCompleteEvent avMetricPlayerItemSeekDidCompleteEvent => avMetricPlayerItemSeekDidCompleteEvent -> IO Bool
didSeekInBuffer avMetricPlayerItemSeekDidCompleteEvent =
  sendMessage avMetricPlayerItemSeekDidCompleteEvent didSeekInBufferSelector

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @init@
initSelector :: Selector '[] (Id AVMetricPlayerItemSeekDidCompleteEvent)
initSelector = mkSelector "init"

-- | @Selector@ for @new@
newSelector :: Selector '[] (Id AVMetricPlayerItemSeekDidCompleteEvent)
newSelector = mkSelector "new"

-- | @Selector@ for @didSeekInBuffer@
didSeekInBufferSelector :: Selector '[] Bool
didSeekInBufferSelector = mkSelector "didSeekInBuffer"

