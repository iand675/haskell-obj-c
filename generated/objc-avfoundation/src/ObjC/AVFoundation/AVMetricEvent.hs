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
  , initSelector
  , newSelector
  , dateSelector
  , sessionIDSelector


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
init_ :: IsAVMetricEvent avMetricEvent => avMetricEvent -> IO (Id AVMetricEvent)
init_ avMetricEvent  =
  sendMsg avMetricEvent (mkSelector "init") (retPtr retVoid) [] >>= ownedObject . castPtr

-- | @+ new@
new :: IO (Id AVMetricEvent)
new  =
  do
    cls' <- getRequiredClass "AVMetricEvent"
    sendClassMsg cls' (mkSelector "new") (retPtr retVoid) [] >>= ownedObject . castPtr

-- | Returns the date when the event occurred.
--
-- ObjC selector: @- date@
date :: IsAVMetricEvent avMetricEvent => avMetricEvent -> IO (Id NSDate)
date avMetricEvent  =
  sendMsg avMetricEvent (mkSelector "date") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | A GUID that identifies the media session. If not available, value is nil.
--
-- ObjC selector: @- sessionID@
sessionID :: IsAVMetricEvent avMetricEvent => avMetricEvent -> IO (Id NSString)
sessionID avMetricEvent  =
  sendMsg avMetricEvent (mkSelector "sessionID") (retPtr retVoid) [] >>= retainedObject . castPtr

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @init@
initSelector :: Selector
initSelector = mkSelector "init"

-- | @Selector@ for @new@
newSelector :: Selector
newSelector = mkSelector "new"

-- | @Selector@ for @date@
dateSelector :: Selector
dateSelector = mkSelector "date"

-- | @Selector@ for @sessionID@
sessionIDSelector :: Selector
sessionIDSelector = mkSelector "sessionID"

