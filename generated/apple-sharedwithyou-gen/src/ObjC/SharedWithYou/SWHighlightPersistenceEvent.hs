{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE DataKinds #-}
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
  , initSelector
  , initWithHighlight_triggerSelector
  , newSelector
  , persistenceEventTriggerSelector

  -- * Enum types
  , SWHighlightPersistenceEventTrigger(SWHighlightPersistenceEventTrigger)
  , pattern SWHighlightPersistenceEventTriggerCreated
  , pattern SWHighlightPersistenceEventTriggerDeleted
  , pattern SWHighlightPersistenceEventTriggerRenamed
  , pattern SWHighlightPersistenceEventTriggerMoved

  ) where

import Foreign.Ptr (Ptr, FunPtr)
import Foreign.C.Types

import ObjC.Runtime.Types
import ObjC.Runtime.Message (sendMessage, sendOwnedMessage, sendClassMessage, sendOwnedClassMessage)
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
initWithHighlight_trigger swHighlightPersistenceEvent highlight trigger =
  sendOwnedMessage swHighlightPersistenceEvent initWithHighlight_triggerSelector (toSWHighlight highlight) trigger

-- | @- init@
init_ :: IsSWHighlightPersistenceEvent swHighlightPersistenceEvent => swHighlightPersistenceEvent -> IO (Id SWHighlightPersistenceEvent)
init_ swHighlightPersistenceEvent =
  sendOwnedMessage swHighlightPersistenceEvent initSelector

-- | @+ new@
new :: IO (Id SWHighlightPersistenceEvent)
new  =
  do
    cls' <- getRequiredClass "SWHighlightPersistenceEvent"
    sendOwnedClassMessage cls' newSelector

-- | @- persistenceEventTrigger@
persistenceEventTrigger :: IsSWHighlightPersistenceEvent swHighlightPersistenceEvent => swHighlightPersistenceEvent -> IO SWHighlightPersistenceEventTrigger
persistenceEventTrigger swHighlightPersistenceEvent =
  sendMessage swHighlightPersistenceEvent persistenceEventTriggerSelector

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @initWithHighlight:trigger:@
initWithHighlight_triggerSelector :: Selector '[Id SWHighlight, SWHighlightPersistenceEventTrigger] (Id SWHighlightPersistenceEvent)
initWithHighlight_triggerSelector = mkSelector "initWithHighlight:trigger:"

-- | @Selector@ for @init@
initSelector :: Selector '[] (Id SWHighlightPersistenceEvent)
initSelector = mkSelector "init"

-- | @Selector@ for @new@
newSelector :: Selector '[] (Id SWHighlightPersistenceEvent)
newSelector = mkSelector "new"

-- | @Selector@ for @persistenceEventTrigger@
persistenceEventTriggerSelector :: Selector '[] SWHighlightPersistenceEventTrigger
persistenceEventTriggerSelector = mkSelector "persistenceEventTrigger"

