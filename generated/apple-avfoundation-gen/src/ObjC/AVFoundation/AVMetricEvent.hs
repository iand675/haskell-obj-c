{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | An abstract base class representing metric events.
--
-- Subclasses of this type that are used from Swift must fulfill the requirements of a Sendable type.
--
-- Generated bindings for @AVMetricEvent@.
module ObjC.AVFoundation.AVMetricEvent
  ( AVMetricEvent
  , IsAVMetricEvent(..)
  , init_
  , new
  , date
  , sessionID
  , dateSelector
  , initSelector
  , newSelector
  , sessionIDSelector


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
init_ :: IsAVMetricEvent avMetricEvent => avMetricEvent -> IO (Id AVMetricEvent)
init_ avMetricEvent =
  sendOwnedMessage avMetricEvent initSelector

-- | @+ new@
new :: IO (Id AVMetricEvent)
new  =
  do
    cls' <- getRequiredClass "AVMetricEvent"
    sendOwnedClassMessage cls' newSelector

-- | Returns the date when the event occurred.
--
-- ObjC selector: @- date@
date :: IsAVMetricEvent avMetricEvent => avMetricEvent -> IO (Id NSDate)
date avMetricEvent =
  sendMessage avMetricEvent dateSelector

-- | A GUID that identifies the media session. If not available, value is nil.
--
-- ObjC selector: @- sessionID@
sessionID :: IsAVMetricEvent avMetricEvent => avMetricEvent -> IO (Id NSString)
sessionID avMetricEvent =
  sendMessage avMetricEvent sessionIDSelector

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @init@
initSelector :: Selector '[] (Id AVMetricEvent)
initSelector = mkSelector "init"

-- | @Selector@ for @new@
newSelector :: Selector '[] (Id AVMetricEvent)
newSelector = mkSelector "new"

-- | @Selector@ for @date@
dateSelector :: Selector '[] (Id NSDate)
dateSelector = mkSelector "date"

-- | @Selector@ for @sessionID@
sessionIDSelector :: Selector '[] (Id NSString)
sessionIDSelector = mkSelector "sessionID"

