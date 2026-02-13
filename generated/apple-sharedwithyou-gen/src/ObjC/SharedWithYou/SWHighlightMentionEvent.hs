{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | _SWHighlightMentionEvent
--
-- A model object representing a mention event that has happened on some content.
--
-- Generated bindings for @SWHighlightMentionEvent@.
module ObjC.SharedWithYou.SWHighlightMentionEvent
  ( SWHighlightMentionEvent
  , IsSWHighlightMentionEvent(..)
  , initWithHighlight_mentionedPersonCloudKitShareHandle
  , initWithHighlight_mentionedPersonIdentity
  , init_
  , new
  , mentionedPersonHandle
  , initSelector
  , initWithHighlight_mentionedPersonCloudKitShareHandleSelector
  , initWithHighlight_mentionedPersonIdentitySelector
  , mentionedPersonHandleSelector
  , newSelector


  ) where

import Foreign.Ptr (Ptr, FunPtr)
import Foreign.C.Types

import ObjC.Runtime.Types
import ObjC.Runtime.Message (sendMessage, sendOwnedMessage, sendClassMessage, sendOwnedClassMessage)
import ObjC.Runtime.Selector (mkSelector)
import ObjC.Runtime.Class (getRequiredClass)

import ObjC.SharedWithYou.Internal.Classes
import ObjC.Foundation.Internal.Classes
import ObjC.SharedWithYouCore.Internal.Classes

-- | Initializes a highlight mention event object when the sender mentions another participant.
--
-- @highlight@ — The object on which the event occurred.
--
-- @handle@ — The CloudKit handle of the person being mentioned by the sender.
--
-- ObjC selector: @- initWithHighlight:mentionedPersonCloudKitShareHandle:@
initWithHighlight_mentionedPersonCloudKitShareHandle :: (IsSWHighlightMentionEvent swHighlightMentionEvent, IsSWHighlight highlight, IsNSString handle) => swHighlightMentionEvent -> highlight -> handle -> IO (Id SWHighlightMentionEvent)
initWithHighlight_mentionedPersonCloudKitShareHandle swHighlightMentionEvent highlight handle =
  sendOwnedMessage swHighlightMentionEvent initWithHighlight_mentionedPersonCloudKitShareHandleSelector (toSWHighlight highlight) (toNSString handle)

-- | Initializes a highlight mention event object when the sender mentions another participant.
--
-- @highlight@ — The object on which the event occurred.
--
-- @identity@ — The identity of the person being mentioned by the sender.
--
-- ObjC selector: @- initWithHighlight:mentionedPersonIdentity:@
initWithHighlight_mentionedPersonIdentity :: (IsSWHighlightMentionEvent swHighlightMentionEvent, IsSWHighlight highlight, IsSWPersonIdentity identity) => swHighlightMentionEvent -> highlight -> identity -> IO (Id SWHighlightMentionEvent)
initWithHighlight_mentionedPersonIdentity swHighlightMentionEvent highlight identity =
  sendOwnedMessage swHighlightMentionEvent initWithHighlight_mentionedPersonIdentitySelector (toSWHighlight highlight) (toSWPersonIdentity identity)

-- | @- init@
init_ :: IsSWHighlightMentionEvent swHighlightMentionEvent => swHighlightMentionEvent -> IO (Id SWHighlightMentionEvent)
init_ swHighlightMentionEvent =
  sendOwnedMessage swHighlightMentionEvent initSelector

-- | @+ new@
new :: IO (Id SWHighlightMentionEvent)
new  =
  do
    cls' <- getRequiredClass "SWHighlightMentionEvent"
    sendOwnedClassMessage cls' newSelector

-- | The person being mentioned by the sender.
--
-- ObjC selector: @- mentionedPersonHandle@
mentionedPersonHandle :: IsSWHighlightMentionEvent swHighlightMentionEvent => swHighlightMentionEvent -> IO (Id NSString)
mentionedPersonHandle swHighlightMentionEvent =
  sendMessage swHighlightMentionEvent mentionedPersonHandleSelector

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @initWithHighlight:mentionedPersonCloudKitShareHandle:@
initWithHighlight_mentionedPersonCloudKitShareHandleSelector :: Selector '[Id SWHighlight, Id NSString] (Id SWHighlightMentionEvent)
initWithHighlight_mentionedPersonCloudKitShareHandleSelector = mkSelector "initWithHighlight:mentionedPersonCloudKitShareHandle:"

-- | @Selector@ for @initWithHighlight:mentionedPersonIdentity:@
initWithHighlight_mentionedPersonIdentitySelector :: Selector '[Id SWHighlight, Id SWPersonIdentity] (Id SWHighlightMentionEvent)
initWithHighlight_mentionedPersonIdentitySelector = mkSelector "initWithHighlight:mentionedPersonIdentity:"

-- | @Selector@ for @init@
initSelector :: Selector '[] (Id SWHighlightMentionEvent)
initSelector = mkSelector "init"

-- | @Selector@ for @new@
newSelector :: Selector '[] (Id SWHighlightMentionEvent)
newSelector = mkSelector "new"

-- | @Selector@ for @mentionedPersonHandle@
mentionedPersonHandleSelector :: Selector '[] (Id NSString)
mentionedPersonHandleSelector = mkSelector "mentionedPersonHandle"

