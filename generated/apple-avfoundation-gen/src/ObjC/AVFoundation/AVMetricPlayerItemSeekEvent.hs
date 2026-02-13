{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Represents a metric event when playback seeked.
--
-- Subclasses of this type that are used from Swift must fulfill the requirements of a Sendable type.
--
-- Generated bindings for @AVMetricPlayerItemSeekEvent@.
module ObjC.AVFoundation.AVMetricPlayerItemSeekEvent
  ( AVMetricPlayerItemSeekEvent
  , IsAVMetricPlayerItemSeekEvent(..)
  , init_
  , new
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
init_ :: IsAVMetricPlayerItemSeekEvent avMetricPlayerItemSeekEvent => avMetricPlayerItemSeekEvent -> IO (Id AVMetricPlayerItemSeekEvent)
init_ avMetricPlayerItemSeekEvent =
  sendOwnedMessage avMetricPlayerItemSeekEvent initSelector

-- | @+ new@
new :: IO (Id AVMetricPlayerItemSeekEvent)
new  =
  do
    cls' <- getRequiredClass "AVMetricPlayerItemSeekEvent"
    sendOwnedClassMessage cls' newSelector

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @init@
initSelector :: Selector '[] (Id AVMetricPlayerItemSeekEvent)
initSelector = mkSelector "init"

-- | @Selector@ for @new@
newSelector :: Selector '[] (Id AVMetricPlayerItemSeekEvent)
newSelector = mkSelector "new"

