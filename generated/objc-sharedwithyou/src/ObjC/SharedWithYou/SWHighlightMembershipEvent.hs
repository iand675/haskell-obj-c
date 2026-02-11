{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | SWHighlightMembershipEvent
--
-- A model object representing a membership event that has happened on some content.
--
-- Generated bindings for @SWHighlightMembershipEvent@.
module ObjC.SharedWithYou.SWHighlightMembershipEvent
  ( SWHighlightMembershipEvent
  , IsSWHighlightMembershipEvent(..)
  , initWithHighlight_trigger
  , init_
  , new
  , membershipEventTrigger
  , initWithHighlight_triggerSelector
  , initSelector
  , newSelector
  , membershipEventTriggerSelector

  -- * Enum types
  , SWHighlightMembershipEventTrigger(SWHighlightMembershipEventTrigger)
  , pattern SWHighlightMembershipEventTriggerAddedCollaborator
  , pattern SWHighlightMembershipEventTriggerRemovedCollaborator

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

-- | Initializes a highlight membership event object to represent changes to a highlight membership.
--
-- @highlight@ — The object on which the event occurred.
--
-- @trigger@ — The trigger membership event for the highlight.
--
-- ObjC selector: @- initWithHighlight:trigger:@
initWithHighlight_trigger :: (IsSWHighlightMembershipEvent swHighlightMembershipEvent, IsSWHighlight highlight) => swHighlightMembershipEvent -> highlight -> SWHighlightMembershipEventTrigger -> IO (Id SWHighlightMembershipEvent)
initWithHighlight_trigger swHighlightMembershipEvent  highlight trigger =
withObjCPtr highlight $ \raw_highlight ->
    sendMsg swHighlightMembershipEvent (mkSelector "initWithHighlight:trigger:") (retPtr retVoid) [argPtr (castPtr raw_highlight :: Ptr ()), argCLong (coerce trigger)] >>= ownedObject . castPtr

-- | @- init@
init_ :: IsSWHighlightMembershipEvent swHighlightMembershipEvent => swHighlightMembershipEvent -> IO (Id SWHighlightMembershipEvent)
init_ swHighlightMembershipEvent  =
  sendMsg swHighlightMembershipEvent (mkSelector "init") (retPtr retVoid) [] >>= ownedObject . castPtr

-- | @+ new@
new :: IO (Id SWHighlightMembershipEvent)
new  =
  do
    cls' <- getRequiredClass "SWHighlightMembershipEvent"
    sendClassMsg cls' (mkSelector "new") (retPtr retVoid) [] >>= ownedObject . castPtr

-- | The type of membership event for the highlight.
--
-- ObjC selector: @- membershipEventTrigger@
membershipEventTrigger :: IsSWHighlightMembershipEvent swHighlightMembershipEvent => swHighlightMembershipEvent -> IO SWHighlightMembershipEventTrigger
membershipEventTrigger swHighlightMembershipEvent  =
  fmap (coerce :: CLong -> SWHighlightMembershipEventTrigger) $ sendMsg swHighlightMembershipEvent (mkSelector "membershipEventTrigger") retCLong []

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

-- | @Selector@ for @membershipEventTrigger@
membershipEventTriggerSelector :: Selector
membershipEventTriggerSelector = mkSelector "membershipEventTrigger"

