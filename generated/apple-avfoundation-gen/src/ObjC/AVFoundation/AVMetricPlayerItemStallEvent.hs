{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Represents a metric event when playback stalled.
--
-- Subclasses of this type that are used from Swift must fulfill the requirements of a Sendable type.
--
-- Generated bindings for @AVMetricPlayerItemStallEvent@.
module ObjC.AVFoundation.AVMetricPlayerItemStallEvent
  ( AVMetricPlayerItemStallEvent
  , IsAVMetricPlayerItemStallEvent(..)
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
init_ :: IsAVMetricPlayerItemStallEvent avMetricPlayerItemStallEvent => avMetricPlayerItemStallEvent -> IO (Id AVMetricPlayerItemStallEvent)
init_ avMetricPlayerItemStallEvent =
  sendOwnedMessage avMetricPlayerItemStallEvent initSelector

-- | @+ new@
new :: IO (Id AVMetricPlayerItemStallEvent)
new  =
  do
    cls' <- getRequiredClass "AVMetricPlayerItemStallEvent"
    sendOwnedClassMessage cls' newSelector

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @init@
initSelector :: Selector '[] (Id AVMetricPlayerItemStallEvent)
initSelector = mkSelector "init"

-- | @Selector@ for @new@
newSelector :: Selector '[] (Id AVMetricPlayerItemStallEvent)
newSelector = mkSelector "new"

