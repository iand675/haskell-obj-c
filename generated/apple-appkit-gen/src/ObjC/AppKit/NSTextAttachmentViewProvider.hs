{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Generated bindings for @NSTextAttachmentViewProvider@.
module ObjC.AppKit.NSTextAttachmentViewProvider
  ( NSTextAttachmentViewProvider
  , IsNSTextAttachmentViewProvider(..)
  , initWithTextAttachment_parentView_textLayoutManager_location
  , init_
  , new
  , loadView
  , textAttachment
  , textLayoutManager
  , location
  , view
  , setView
  , tracksTextAttachmentViewBounds
  , setTracksTextAttachmentViewBounds
  , initSelector
  , initWithTextAttachment_parentView_textLayoutManager_locationSelector
  , loadViewSelector
  , locationSelector
  , newSelector
  , setTracksTextAttachmentViewBoundsSelector
  , setViewSelector
  , textAttachmentSelector
  , textLayoutManagerSelector
  , tracksTextAttachmentViewBoundsSelector
  , viewSelector


  ) where

import Foreign.Ptr (Ptr, FunPtr)
import Foreign.C.Types

import ObjC.Runtime.Types
import ObjC.Runtime.Message (sendMessage, sendOwnedMessage, sendClassMessage, sendOwnedClassMessage)
import ObjC.Runtime.Selector (mkSelector)
import ObjC.Runtime.Class (getRequiredClass)

import ObjC.AppKit.Internal.Classes
import ObjC.Foundation.Internal.Classes

-- | @- initWithTextAttachment:parentView:textLayoutManager:location:@
initWithTextAttachment_parentView_textLayoutManager_location :: (IsNSTextAttachmentViewProvider nsTextAttachmentViewProvider, IsNSTextAttachment textAttachment, IsNSView parentView, IsNSTextLayoutManager textLayoutManager) => nsTextAttachmentViewProvider -> textAttachment -> parentView -> textLayoutManager -> RawId -> IO (Id NSTextAttachmentViewProvider)
initWithTextAttachment_parentView_textLayoutManager_location nsTextAttachmentViewProvider textAttachment parentView textLayoutManager location =
  sendOwnedMessage nsTextAttachmentViewProvider initWithTextAttachment_parentView_textLayoutManager_locationSelector (toNSTextAttachment textAttachment) (toNSView parentView) (toNSTextLayoutManager textLayoutManager) location

-- | @- init@
init_ :: IsNSTextAttachmentViewProvider nsTextAttachmentViewProvider => nsTextAttachmentViewProvider -> IO (Id NSTextAttachmentViewProvider)
init_ nsTextAttachmentViewProvider =
  sendOwnedMessage nsTextAttachmentViewProvider initSelector

-- | @+ new@
new :: IO (Id NSTextAttachmentViewProvider)
new  =
  do
    cls' <- getRequiredClass "NSTextAttachmentViewProvider"
    sendOwnedClassMessage cls' newSelector

-- | @- loadView@
loadView :: IsNSTextAttachmentViewProvider nsTextAttachmentViewProvider => nsTextAttachmentViewProvider -> IO ()
loadView nsTextAttachmentViewProvider =
  sendMessage nsTextAttachmentViewProvider loadViewSelector

-- | @- textAttachment@
textAttachment :: IsNSTextAttachmentViewProvider nsTextAttachmentViewProvider => nsTextAttachmentViewProvider -> IO (Id NSTextAttachment)
textAttachment nsTextAttachmentViewProvider =
  sendMessage nsTextAttachmentViewProvider textAttachmentSelector

-- | @- textLayoutManager@
textLayoutManager :: IsNSTextAttachmentViewProvider nsTextAttachmentViewProvider => nsTextAttachmentViewProvider -> IO (Id NSTextLayoutManager)
textLayoutManager nsTextAttachmentViewProvider =
  sendMessage nsTextAttachmentViewProvider textLayoutManagerSelector

-- | @- location@
location :: IsNSTextAttachmentViewProvider nsTextAttachmentViewProvider => nsTextAttachmentViewProvider -> IO RawId
location nsTextAttachmentViewProvider =
  sendMessage nsTextAttachmentViewProvider locationSelector

-- | @- view@
view :: IsNSTextAttachmentViewProvider nsTextAttachmentViewProvider => nsTextAttachmentViewProvider -> IO (Id NSView)
view nsTextAttachmentViewProvider =
  sendMessage nsTextAttachmentViewProvider viewSelector

-- | @- setView:@
setView :: (IsNSTextAttachmentViewProvider nsTextAttachmentViewProvider, IsNSView value) => nsTextAttachmentViewProvider -> value -> IO ()
setView nsTextAttachmentViewProvider value =
  sendMessage nsTextAttachmentViewProvider setViewSelector (toNSView value)

-- | @- tracksTextAttachmentViewBounds@
tracksTextAttachmentViewBounds :: IsNSTextAttachmentViewProvider nsTextAttachmentViewProvider => nsTextAttachmentViewProvider -> IO Bool
tracksTextAttachmentViewBounds nsTextAttachmentViewProvider =
  sendMessage nsTextAttachmentViewProvider tracksTextAttachmentViewBoundsSelector

-- | @- setTracksTextAttachmentViewBounds:@
setTracksTextAttachmentViewBounds :: IsNSTextAttachmentViewProvider nsTextAttachmentViewProvider => nsTextAttachmentViewProvider -> Bool -> IO ()
setTracksTextAttachmentViewBounds nsTextAttachmentViewProvider value =
  sendMessage nsTextAttachmentViewProvider setTracksTextAttachmentViewBoundsSelector value

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @initWithTextAttachment:parentView:textLayoutManager:location:@
initWithTextAttachment_parentView_textLayoutManager_locationSelector :: Selector '[Id NSTextAttachment, Id NSView, Id NSTextLayoutManager, RawId] (Id NSTextAttachmentViewProvider)
initWithTextAttachment_parentView_textLayoutManager_locationSelector = mkSelector "initWithTextAttachment:parentView:textLayoutManager:location:"

-- | @Selector@ for @init@
initSelector :: Selector '[] (Id NSTextAttachmentViewProvider)
initSelector = mkSelector "init"

-- | @Selector@ for @new@
newSelector :: Selector '[] (Id NSTextAttachmentViewProvider)
newSelector = mkSelector "new"

-- | @Selector@ for @loadView@
loadViewSelector :: Selector '[] ()
loadViewSelector = mkSelector "loadView"

-- | @Selector@ for @textAttachment@
textAttachmentSelector :: Selector '[] (Id NSTextAttachment)
textAttachmentSelector = mkSelector "textAttachment"

-- | @Selector@ for @textLayoutManager@
textLayoutManagerSelector :: Selector '[] (Id NSTextLayoutManager)
textLayoutManagerSelector = mkSelector "textLayoutManager"

-- | @Selector@ for @location@
locationSelector :: Selector '[] RawId
locationSelector = mkSelector "location"

-- | @Selector@ for @view@
viewSelector :: Selector '[] (Id NSView)
viewSelector = mkSelector "view"

-- | @Selector@ for @setView:@
setViewSelector :: Selector '[Id NSView] ()
setViewSelector = mkSelector "setView:"

-- | @Selector@ for @tracksTextAttachmentViewBounds@
tracksTextAttachmentViewBoundsSelector :: Selector '[] Bool
tracksTextAttachmentViewBoundsSelector = mkSelector "tracksTextAttachmentViewBounds"

-- | @Selector@ for @setTracksTextAttachmentViewBounds:@
setTracksTextAttachmentViewBoundsSelector :: Selector '[Bool] ()
setTracksTextAttachmentViewBoundsSelector = mkSelector "setTracksTextAttachmentViewBounds:"

