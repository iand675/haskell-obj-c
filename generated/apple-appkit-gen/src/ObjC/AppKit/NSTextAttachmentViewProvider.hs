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
  , initWithTextAttachment_parentView_textLayoutManager_locationSelector
  , initSelector
  , newSelector
  , loadViewSelector
  , textAttachmentSelector
  , textLayoutManagerSelector
  , locationSelector
  , viewSelector
  , setViewSelector
  , tracksTextAttachmentViewBoundsSelector
  , setTracksTextAttachmentViewBoundsSelector


  ) where

import Foreign.Ptr (Ptr, nullPtr, castPtr)
import Foreign.LibFFI
import Foreign.C.Types
import Data.Int (Int8, Int16)
import Data.Word (Word16)
import Data.Coerce (coerce)

import ObjC.Runtime.Types
import ObjC.Runtime.MsgSend (sendMsg, sendClassMsg, sendMsgStret, sendClassMsgStret)
import ObjC.Runtime.Selector (mkSelector)
import ObjC.Runtime.Class (getRequiredClass)

import ObjC.AppKit.Internal.Classes
import ObjC.Foundation.Internal.Classes

-- | @- initWithTextAttachment:parentView:textLayoutManager:location:@
initWithTextAttachment_parentView_textLayoutManager_location :: (IsNSTextAttachmentViewProvider nsTextAttachmentViewProvider, IsNSTextAttachment textAttachment, IsNSView parentView, IsNSTextLayoutManager textLayoutManager) => nsTextAttachmentViewProvider -> textAttachment -> parentView -> textLayoutManager -> RawId -> IO (Id NSTextAttachmentViewProvider)
initWithTextAttachment_parentView_textLayoutManager_location nsTextAttachmentViewProvider  textAttachment parentView textLayoutManager location =
  withObjCPtr textAttachment $ \raw_textAttachment ->
    withObjCPtr parentView $ \raw_parentView ->
      withObjCPtr textLayoutManager $ \raw_textLayoutManager ->
          sendMsg nsTextAttachmentViewProvider (mkSelector "initWithTextAttachment:parentView:textLayoutManager:location:") (retPtr retVoid) [argPtr (castPtr raw_textAttachment :: Ptr ()), argPtr (castPtr raw_parentView :: Ptr ()), argPtr (castPtr raw_textLayoutManager :: Ptr ()), argPtr (castPtr (unRawId location) :: Ptr ())] >>= ownedObject . castPtr

-- | @- init@
init_ :: IsNSTextAttachmentViewProvider nsTextAttachmentViewProvider => nsTextAttachmentViewProvider -> IO (Id NSTextAttachmentViewProvider)
init_ nsTextAttachmentViewProvider  =
    sendMsg nsTextAttachmentViewProvider (mkSelector "init") (retPtr retVoid) [] >>= ownedObject . castPtr

-- | @+ new@
new :: IO (Id NSTextAttachmentViewProvider)
new  =
  do
    cls' <- getRequiredClass "NSTextAttachmentViewProvider"
    sendClassMsg cls' (mkSelector "new") (retPtr retVoid) [] >>= ownedObject . castPtr

-- | @- loadView@
loadView :: IsNSTextAttachmentViewProvider nsTextAttachmentViewProvider => nsTextAttachmentViewProvider -> IO ()
loadView nsTextAttachmentViewProvider  =
    sendMsg nsTextAttachmentViewProvider (mkSelector "loadView") retVoid []

-- | @- textAttachment@
textAttachment :: IsNSTextAttachmentViewProvider nsTextAttachmentViewProvider => nsTextAttachmentViewProvider -> IO (Id NSTextAttachment)
textAttachment nsTextAttachmentViewProvider  =
    sendMsg nsTextAttachmentViewProvider (mkSelector "textAttachment") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- textLayoutManager@
textLayoutManager :: IsNSTextAttachmentViewProvider nsTextAttachmentViewProvider => nsTextAttachmentViewProvider -> IO (Id NSTextLayoutManager)
textLayoutManager nsTextAttachmentViewProvider  =
    sendMsg nsTextAttachmentViewProvider (mkSelector "textLayoutManager") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- location@
location :: IsNSTextAttachmentViewProvider nsTextAttachmentViewProvider => nsTextAttachmentViewProvider -> IO RawId
location nsTextAttachmentViewProvider  =
    fmap (RawId . castPtr) $ sendMsg nsTextAttachmentViewProvider (mkSelector "location") (retPtr retVoid) []

-- | @- view@
view :: IsNSTextAttachmentViewProvider nsTextAttachmentViewProvider => nsTextAttachmentViewProvider -> IO (Id NSView)
view nsTextAttachmentViewProvider  =
    sendMsg nsTextAttachmentViewProvider (mkSelector "view") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- setView:@
setView :: (IsNSTextAttachmentViewProvider nsTextAttachmentViewProvider, IsNSView value) => nsTextAttachmentViewProvider -> value -> IO ()
setView nsTextAttachmentViewProvider  value =
  withObjCPtr value $ \raw_value ->
      sendMsg nsTextAttachmentViewProvider (mkSelector "setView:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- | @- tracksTextAttachmentViewBounds@
tracksTextAttachmentViewBounds :: IsNSTextAttachmentViewProvider nsTextAttachmentViewProvider => nsTextAttachmentViewProvider -> IO Bool
tracksTextAttachmentViewBounds nsTextAttachmentViewProvider  =
    fmap ((/= 0) :: CULong -> Bool) $ sendMsg nsTextAttachmentViewProvider (mkSelector "tracksTextAttachmentViewBounds") retCULong []

-- | @- setTracksTextAttachmentViewBounds:@
setTracksTextAttachmentViewBounds :: IsNSTextAttachmentViewProvider nsTextAttachmentViewProvider => nsTextAttachmentViewProvider -> Bool -> IO ()
setTracksTextAttachmentViewBounds nsTextAttachmentViewProvider  value =
    sendMsg nsTextAttachmentViewProvider (mkSelector "setTracksTextAttachmentViewBounds:") retVoid [argCULong (if value then 1 else 0)]

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @initWithTextAttachment:parentView:textLayoutManager:location:@
initWithTextAttachment_parentView_textLayoutManager_locationSelector :: Selector
initWithTextAttachment_parentView_textLayoutManager_locationSelector = mkSelector "initWithTextAttachment:parentView:textLayoutManager:location:"

-- | @Selector@ for @init@
initSelector :: Selector
initSelector = mkSelector "init"

-- | @Selector@ for @new@
newSelector :: Selector
newSelector = mkSelector "new"

-- | @Selector@ for @loadView@
loadViewSelector :: Selector
loadViewSelector = mkSelector "loadView"

-- | @Selector@ for @textAttachment@
textAttachmentSelector :: Selector
textAttachmentSelector = mkSelector "textAttachment"

-- | @Selector@ for @textLayoutManager@
textLayoutManagerSelector :: Selector
textLayoutManagerSelector = mkSelector "textLayoutManager"

-- | @Selector@ for @location@
locationSelector :: Selector
locationSelector = mkSelector "location"

-- | @Selector@ for @view@
viewSelector :: Selector
viewSelector = mkSelector "view"

-- | @Selector@ for @setView:@
setViewSelector :: Selector
setViewSelector = mkSelector "setView:"

-- | @Selector@ for @tracksTextAttachmentViewBounds@
tracksTextAttachmentViewBoundsSelector :: Selector
tracksTextAttachmentViewBoundsSelector = mkSelector "tracksTextAttachmentViewBounds"

-- | @Selector@ for @setTracksTextAttachmentViewBounds:@
setTracksTextAttachmentViewBoundsSelector :: Selector
setTracksTextAttachmentViewBoundsSelector = mkSelector "setTracksTextAttachmentViewBounds:"

