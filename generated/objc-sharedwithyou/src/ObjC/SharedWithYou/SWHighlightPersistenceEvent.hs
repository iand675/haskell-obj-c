{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | SWHighlightPersistenceEvent
--
-- A model object representing a persistence event that has happened on some content.
--
-- Generated bindings for @SWHighlightPersistenceEvent@.
module ObjC.SharedWithYou.SWHighlightPersistenceEvent
  ( SWHighlightPersistenceEvent
  , IsSWHighlightPersistenceEvent(..)
  , initWithHighlight_trigger
  , init_
  , new
  , persistenceEventTrigger
  , initWithHighlight_triggerSelector
  , initSelector
  , newSelector
  , persistenceEventTriggerSelector

  -- * Enum types
  , SWHighlightPersistenceEventTrigger(SWHighlightPersistenceEventTrigger)
  , pattern SWHighlightPersistenceEventTriggerCreated
  , pattern SWHighlightPersistenceEventTriggerDeleted
  , pattern SWHighlightPersistenceEventTriggerRenamed
  , pattern SWHighlightPersistenceEventTriggerMoved

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

import ObjC.SharedWithYou.Internal.Classes
import ObjC.SharedWithYou.Internal.Enums
import ObjC.Foundation.Internal.Classes

-- | Initializes a highlight persistence event object when the highlight persistence is changed.
--
-- @highlight@ — The object on which the event occurred.
--
-- @trigger@ — The trigger persistence event for the highlight.
--
-- ObjC selector: @- initWithHighlight:trigger:@
initWithHighlight_trigger :: (IsSWHighlightPersistenceEvent swHighlightPersistenceEvent, IsSWHighlight highlight) => swHighlightPersistenceEvent -> highlight -> SWHighlightPersistenceEventTrigger -> IO (Id SWHighlightPersistenceEvent)
initWithHighlight_trigger swHighlightPersistenceEvent  highlight trigger =
withObjCPtr highlight $ \raw_highlight ->
    sendMsg swHighlightPersistenceEvent (mkSelector "initWithHighlight:trigger:") (retPtr retVoid) [argPtr (castPtr raw_highlight :: Ptr ()), argCLong (coerce trigger)] >>= ownedObject . castPtr

-- | @- init@
init_ :: IsSWHighlightPersistenceEvent swHighlightPersistenceEvent => swHighlightPersistenceEvent -> IO (Id SWHighlightPersistenceEvent)
init_ swHighlightPersistenceEvent  =
  sendMsg swHighlightPersistenceEvent (mkSelector "init") (retPtr retVoid) [] >>= ownedObject . castPtr

-- | @+ new@
new :: IO (Id SWHighlightPersistenceEvent)
new  =
  do
    cls' <- getRequiredClass "SWHighlightPersistenceEvent"
    sendClassMsg cls' (mkSelector "new") (retPtr retVoid) [] >>= ownedObject . castPtr

-- | @- persistenceEventTrigger@
persistenceEventTrigger :: IsSWHighlightPersistenceEvent swHighlightPersistenceEvent => swHighlightPersistenceEvent -> IO SWHighlightPersistenceEventTrigger
persistenceEventTrigger swHighlightPersistenceEvent  =
  fmap (coerce :: CLong -> SWHighlightPersistenceEventTrigger) $ sendMsg swHighlightPersistenceEvent (mkSelector "persistenceEventTrigger") retCLong []

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @initWithHighlight:trigger:@
initWithHighlight_triggerSelector :: Selector
initWithHighlight_triggerSelector = mkSelector "initWithHighlight:trigger:"

-- | @Selector@ for @init@
initSelector :: Selector
initSelector = mkSelector "init"

-- | @Selector@ for @new@
newSelector :: Selector
newSelector = mkSelector "new"

-- | @Selector@ for @persistenceEventTrigger@
persistenceEventTriggerSelector :: Selector
persistenceEventTriggerSelector = mkSelector "persistenceEventTrigger"

