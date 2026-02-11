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
  , initWithHighlight_mentionedPersonCloudKitShareHandleSelector
  , initWithHighlight_mentionedPersonIdentitySelector
  , initSelector
  , newSelector
  , mentionedPersonHandleSelector


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
initWithHighlight_mentionedPersonCloudKitShareHandle swHighlightMentionEvent  highlight handle =
withObjCPtr highlight $ \raw_highlight ->
  withObjCPtr handle $ \raw_handle ->
      sendMsg swHighlightMentionEvent (mkSelector "initWithHighlight:mentionedPersonCloudKitShareHandle:") (retPtr retVoid) [argPtr (castPtr raw_highlight :: Ptr ()), argPtr (castPtr raw_handle :: Ptr ())] >>= ownedObject . castPtr

-- | Initializes a highlight mention event object when the sender mentions another participant.
--
-- @highlight@ — The object on which the event occurred.
--
-- @identity@ — The identity of the person being mentioned by the sender.
--
-- ObjC selector: @- initWithHighlight:mentionedPersonIdentity:@
initWithHighlight_mentionedPersonIdentity :: (IsSWHighlightMentionEvent swHighlightMentionEvent, IsSWHighlight highlight, IsSWPersonIdentity identity) => swHighlightMentionEvent -> highlight -> identity -> IO (Id SWHighlightMentionEvent)
initWithHighlight_mentionedPersonIdentity swHighlightMentionEvent  highlight identity =
withObjCPtr highlight $ \raw_highlight ->
  withObjCPtr identity $ \raw_identity ->
      sendMsg swHighlightMentionEvent (mkSelector "initWithHighlight:mentionedPersonIdentity:") (retPtr retVoid) [argPtr (castPtr raw_highlight :: Ptr ()), argPtr (castPtr raw_identity :: Ptr ())] >>= ownedObject . castPtr

-- | @- init@
init_ :: IsSWHighlightMentionEvent swHighlightMentionEvent => swHighlightMentionEvent -> IO (Id SWHighlightMentionEvent)
init_ swHighlightMentionEvent  =
  sendMsg swHighlightMentionEvent (mkSelector "init") (retPtr retVoid) [] >>= ownedObject . castPtr

-- | @+ new@
new :: IO (Id SWHighlightMentionEvent)
new  =
  do
    cls' <- getRequiredClass "SWHighlightMentionEvent"
    sendClassMsg cls' (mkSelector "new") (retPtr retVoid) [] >>= ownedObject . castPtr

-- | The person being mentioned by the sender.
--
-- ObjC selector: @- mentionedPersonHandle@
mentionedPersonHandle :: IsSWHighlightMentionEvent swHighlightMentionEvent => swHighlightMentionEvent -> IO (Id NSString)
mentionedPersonHandle swHighlightMentionEvent  =
  sendMsg swHighlightMentionEvent (mkSelector "mentionedPersonHandle") (retPtr retVoid) [] >>= retainedObject . castPtr

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @initWithHighlight:mentionedPersonCloudKitShareHandle:@
initWithHighlight_mentionedPersonCloudKitShareHandleSelector :: Selector
initWithHighlight_mentionedPersonCloudKitShareHandleSelector = mkSelector "initWithHighlight:mentionedPersonCloudKitShareHandle:"

-- | @Selector@ for @initWithHighlight:mentionedPersonIdentity:@
initWithHighlight_mentionedPersonIdentitySelector :: Selector
initWithHighlight_mentionedPersonIdentitySelector = mkSelector "initWithHighlight:mentionedPersonIdentity:"

-- | @Selector@ for @init@
initSelector :: Selector
initSelector = mkSelector "init"

-- | @Selector@ for @new@
newSelector :: Selector
newSelector = mkSelector "new"

-- | @Selector@ for @mentionedPersonHandle@
mentionedPersonHandleSelector :: Selector
mentionedPersonHandleSelector = mkSelector "mentionedPersonHandle"

