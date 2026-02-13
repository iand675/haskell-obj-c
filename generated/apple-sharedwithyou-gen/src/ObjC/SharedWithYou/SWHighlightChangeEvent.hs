{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | SWHighlightChangeEvent
--
-- A model object representing activity that has happened on some content.
--
-- Generated bindings for @SWHighlightChangeEvent@.
module ObjC.SharedWithYou.SWHighlightChangeEvent
  ( SWHighlightChangeEvent
  , IsSWHighlightChangeEvent(..)
  , initWithHighlight_trigger
  , init_
  , new
  , changeEventTrigger
  , highlightURL
  , changeEventTriggerSelector
  , highlightURLSelector
  , initSelector
  , initWithHighlight_triggerSelector
  , newSelector

  -- * Enum types
  , SWHighlightChangeEventTrigger(SWHighlightChangeEventTrigger)
  , pattern SWHighlightChangeEventTriggerEdit
  , pattern SWHighlightChangeEventTriggerComment

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

-- | Initializes a highlight change event object to represent changes to a highlight.
--
-- @highlight@ — The object on which the event occurred.
--
-- @trigger@ — The trigger change event for the highlight.
--
-- ObjC selector: @- initWithHighlight:trigger:@
initWithHighlight_trigger :: (IsSWHighlightChangeEvent swHighlightChangeEvent, IsSWHighlight highlight) => swHighlightChangeEvent -> highlight -> SWHighlightChangeEventTrigger -> IO (Id SWHighlightChangeEvent)
initWithHighlight_trigger swHighlightChangeEvent highlight trigger =
  sendOwnedMessage swHighlightChangeEvent initWithHighlight_triggerSelector (toSWHighlight highlight) trigger

-- | @- init@
init_ :: IsSWHighlightChangeEvent swHighlightChangeEvent => swHighlightChangeEvent -> IO (Id SWHighlightChangeEvent)
init_ swHighlightChangeEvent =
  sendOwnedMessage swHighlightChangeEvent initSelector

-- | @+ new@
new :: IO (Id SWHighlightChangeEvent)
new  =
  do
    cls' <- getRequiredClass "SWHighlightChangeEvent"
    sendOwnedClassMessage cls' newSelector

-- | @- changeEventTrigger@
changeEventTrigger :: IsSWHighlightChangeEvent swHighlightChangeEvent => swHighlightChangeEvent -> IO SWHighlightChangeEventTrigger
changeEventTrigger swHighlightChangeEvent =
  sendMessage swHighlightChangeEvent changeEventTriggerSelector

-- | @- highlightURL@
highlightURL :: IsSWHighlightChangeEvent swHighlightChangeEvent => swHighlightChangeEvent -> IO (Id NSURL)
highlightURL swHighlightChangeEvent =
  sendMessage swHighlightChangeEvent highlightURLSelector

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @initWithHighlight:trigger:@
initWithHighlight_triggerSelector :: Selector '[Id SWHighlight, SWHighlightChangeEventTrigger] (Id SWHighlightChangeEvent)
initWithHighlight_triggerSelector = mkSelector "initWithHighlight:trigger:"

-- | @Selector@ for @init@
initSelector :: Selector '[] (Id SWHighlightChangeEvent)
initSelector = mkSelector "init"

-- | @Selector@ for @new@
newSelector :: Selector '[] (Id SWHighlightChangeEvent)
newSelector = mkSelector "new"

-- | @Selector@ for @changeEventTrigger@
changeEventTriggerSelector :: Selector '[] SWHighlightChangeEventTrigger
changeEventTriggerSelector = mkSelector "changeEventTrigger"

-- | @Selector@ for @highlightURL@
highlightURLSelector :: Selector '[] (Id NSURL)
highlightURLSelector = mkSelector "highlightURL"

