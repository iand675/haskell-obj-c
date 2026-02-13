{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE DataKinds #-}
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
  , initSelector
  , initWithHighlight_triggerSelector
  , membershipEventTriggerSelector
  , newSelector

  -- * Enum types
  , SWHighlightMembershipEventTrigger(SWHighlightMembershipEventTrigger)
  , pattern SWHighlightMembershipEventTriggerAddedCollaborator
  , pattern SWHighlightMembershipEventTriggerRemovedCollaborator

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

-- | Initializes a highlight membership event object to represent changes to a highlight membership.
--
-- @highlight@ — The object on which the event occurred.
--
-- @trigger@ — The trigger membership event for the highlight.
--
-- ObjC selector: @- initWithHighlight:trigger:@
initWithHighlight_trigger :: (IsSWHighlightMembershipEvent swHighlightMembershipEvent, IsSWHighlight highlight) => swHighlightMembershipEvent -> highlight -> SWHighlightMembershipEventTrigger -> IO (Id SWHighlightMembershipEvent)
initWithHighlight_trigger swHighlightMembershipEvent highlight trigger =
  sendOwnedMessage swHighlightMembershipEvent initWithHighlight_triggerSelector (toSWHighlight highlight) trigger

-- | @- init@
init_ :: IsSWHighlightMembershipEvent swHighlightMembershipEvent => swHighlightMembershipEvent -> IO (Id SWHighlightMembershipEvent)
init_ swHighlightMembershipEvent =
  sendOwnedMessage swHighlightMembershipEvent initSelector

-- | @+ new@
new :: IO (Id SWHighlightMembershipEvent)
new  =
  do
    cls' <- getRequiredClass "SWHighlightMembershipEvent"
    sendOwnedClassMessage cls' newSelector

-- | The type of membership event for the highlight.
--
-- ObjC selector: @- membershipEventTrigger@
membershipEventTrigger :: IsSWHighlightMembershipEvent swHighlightMembershipEvent => swHighlightMembershipEvent -> IO SWHighlightMembershipEventTrigger
membershipEventTrigger swHighlightMembershipEvent =
  sendMessage swHighlightMembershipEvent membershipEventTriggerSelector

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @initWithHighlight:trigger:@
initWithHighlight_triggerSelector :: Selector '[Id SWHighlight, SWHighlightMembershipEventTrigger] (Id SWHighlightMembershipEvent)
initWithHighlight_triggerSelector = mkSelector "initWithHighlight:trigger:"

-- | @Selector@ for @init@
initSelector :: Selector '[] (Id SWHighlightMembershipEvent)
initSelector = mkSelector "init"

-- | @Selector@ for @new@
newSelector :: Selector '[] (Id SWHighlightMembershipEvent)
newSelector = mkSelector "new"

-- | @Selector@ for @membershipEventTrigger@
membershipEventTriggerSelector :: Selector '[] SWHighlightMembershipEventTrigger
membershipEventTriggerSelector = mkSelector "membershipEventTrigger"

