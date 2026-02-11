{-# LANGUAGE PatternSynonyms #-}
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
  , initWithHighlight_triggerSelector
  , initSelector
  , newSelector
  , changeEventTriggerSelector
  , highlightURLSelector

  -- * Enum types
  , SWHighlightChangeEventTrigger(SWHighlightChangeEventTrigger)
  , pattern SWHighlightChangeEventTriggerEdit
  , pattern SWHighlightChangeEventTriggerComment

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

-- | Initializes a highlight change event object to represent changes to a highlight.
--
-- @highlight@ — The object on which the event occurred.
--
-- @trigger@ — The trigger change event for the highlight.
--
-- ObjC selector: @- initWithHighlight:trigger:@
initWithHighlight_trigger :: (IsSWHighlightChangeEvent swHighlightChangeEvent, IsSWHighlight highlight) => swHighlightChangeEvent -> highlight -> SWHighlightChangeEventTrigger -> IO (Id SWHighlightChangeEvent)
initWithHighlight_trigger swHighlightChangeEvent  highlight trigger =
withObjCPtr highlight $ \raw_highlight ->
    sendMsg swHighlightChangeEvent (mkSelector "initWithHighlight:trigger:") (retPtr retVoid) [argPtr (castPtr raw_highlight :: Ptr ()), argCLong (coerce trigger)] >>= ownedObject . castPtr

-- | @- init@
init_ :: IsSWHighlightChangeEvent swHighlightChangeEvent => swHighlightChangeEvent -> IO (Id SWHighlightChangeEvent)
init_ swHighlightChangeEvent  =
  sendMsg swHighlightChangeEvent (mkSelector "init") (retPtr retVoid) [] >>= ownedObject . castPtr

-- | @+ new@
new :: IO (Id SWHighlightChangeEvent)
new  =
  do
    cls' <- getRequiredClass "SWHighlightChangeEvent"
    sendClassMsg cls' (mkSelector "new") (retPtr retVoid) [] >>= ownedObject . castPtr

-- | @- changeEventTrigger@
changeEventTrigger :: IsSWHighlightChangeEvent swHighlightChangeEvent => swHighlightChangeEvent -> IO SWHighlightChangeEventTrigger
changeEventTrigger swHighlightChangeEvent  =
  fmap (coerce :: CLong -> SWHighlightChangeEventTrigger) $ sendMsg swHighlightChangeEvent (mkSelector "changeEventTrigger") retCLong []

-- | @- highlightURL@
highlightURL :: IsSWHighlightChangeEvent swHighlightChangeEvent => swHighlightChangeEvent -> IO (Id NSURL)
highlightURL swHighlightChangeEvent  =
  sendMsg swHighlightChangeEvent (mkSelector "highlightURL") (retPtr retVoid) [] >>= retainedObject . castPtr

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

-- | @Selector@ for @changeEventTrigger@
changeEventTriggerSelector :: Selector
changeEventTriggerSelector = mkSelector "changeEventTrigger"

-- | @Selector@ for @highlightURL@
highlightURLSelector :: Selector
highlightURLSelector = mkSelector "highlightURL"

