{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Generated bindings for @NSClipView@.
module ObjC.AppKit.NSClipView
  ( NSClipView
  , IsNSClipView(..)
  , viewFrameChanged
  , viewBoundsChanged
  , autoscroll
  , scrollToPoint
  , constrainBoundsRect
  , constrainScrollPoint
  , backgroundColor
  , setBackgroundColor
  , drawsBackground
  , setDrawsBackground
  , documentView
  , setDocumentView
  , documentRect
  , documentCursor
  , setDocumentCursor
  , documentVisibleRect
  , contentInsets
  , setContentInsets
  , automaticallyAdjustsContentInsets
  , setAutomaticallyAdjustsContentInsets
  , copiesOnScroll
  , setCopiesOnScroll
  , viewFrameChangedSelector
  , viewBoundsChangedSelector
  , autoscrollSelector
  , scrollToPointSelector
  , constrainBoundsRectSelector
  , constrainScrollPointSelector
  , backgroundColorSelector
  , setBackgroundColorSelector
  , drawsBackgroundSelector
  , setDrawsBackgroundSelector
  , documentViewSelector
  , setDocumentViewSelector
  , documentRectSelector
  , documentCursorSelector
  , setDocumentCursorSelector
  , documentVisibleRectSelector
  , contentInsetsSelector
  , setContentInsetsSelector
  , automaticallyAdjustsContentInsetsSelector
  , setAutomaticallyAdjustsContentInsetsSelector
  , copiesOnScrollSelector
  , setCopiesOnScrollSelector


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
import ObjC.Foundation.Internal.Structs
import ObjC.Foundation.Internal.Classes

-- | @- viewFrameChanged:@
viewFrameChanged :: (IsNSClipView nsClipView, IsNSNotification notification) => nsClipView -> notification -> IO ()
viewFrameChanged nsClipView  notification =
withObjCPtr notification $ \raw_notification ->
    sendMsg nsClipView (mkSelector "viewFrameChanged:") retVoid [argPtr (castPtr raw_notification :: Ptr ())]

-- | @- viewBoundsChanged:@
viewBoundsChanged :: (IsNSClipView nsClipView, IsNSNotification notification) => nsClipView -> notification -> IO ()
viewBoundsChanged nsClipView  notification =
withObjCPtr notification $ \raw_notification ->
    sendMsg nsClipView (mkSelector "viewBoundsChanged:") retVoid [argPtr (castPtr raw_notification :: Ptr ())]

-- | @- autoscroll:@
autoscroll :: (IsNSClipView nsClipView, IsNSEvent event) => nsClipView -> event -> IO Bool
autoscroll nsClipView  event =
withObjCPtr event $ \raw_event ->
    fmap ((/= 0) :: CULong -> Bool) $ sendMsg nsClipView (mkSelector "autoscroll:") retCULong [argPtr (castPtr raw_event :: Ptr ())]

-- | @- scrollToPoint:@
scrollToPoint :: IsNSClipView nsClipView => nsClipView -> NSPoint -> IO ()
scrollToPoint nsClipView  newOrigin =
  sendMsg nsClipView (mkSelector "scrollToPoint:") retVoid [argNSPoint newOrigin]

-- | @- constrainBoundsRect:@
constrainBoundsRect :: IsNSClipView nsClipView => nsClipView -> NSRect -> IO NSRect
constrainBoundsRect nsClipView  proposedBounds =
  sendMsgStret nsClipView (mkSelector "constrainBoundsRect:") retNSRect [argNSRect proposedBounds]

-- | @- constrainScrollPoint:@
constrainScrollPoint :: IsNSClipView nsClipView => nsClipView -> NSPoint -> IO NSPoint
constrainScrollPoint nsClipView  newOrigin =
  sendMsgStret nsClipView (mkSelector "constrainScrollPoint:") retNSPoint [argNSPoint newOrigin]

-- | @- backgroundColor@
backgroundColor :: IsNSClipView nsClipView => nsClipView -> IO (Id NSColor)
backgroundColor nsClipView  =
  sendMsg nsClipView (mkSelector "backgroundColor") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- setBackgroundColor:@
setBackgroundColor :: (IsNSClipView nsClipView, IsNSColor value) => nsClipView -> value -> IO ()
setBackgroundColor nsClipView  value =
withObjCPtr value $ \raw_value ->
    sendMsg nsClipView (mkSelector "setBackgroundColor:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- | @- drawsBackground@
drawsBackground :: IsNSClipView nsClipView => nsClipView -> IO Bool
drawsBackground nsClipView  =
  fmap ((/= 0) :: CULong -> Bool) $ sendMsg nsClipView (mkSelector "drawsBackground") retCULong []

-- | @- setDrawsBackground:@
setDrawsBackground :: IsNSClipView nsClipView => nsClipView -> Bool -> IO ()
setDrawsBackground nsClipView  value =
  sendMsg nsClipView (mkSelector "setDrawsBackground:") retVoid [argCULong (if value then 1 else 0)]

-- | @- documentView@
documentView :: IsNSClipView nsClipView => nsClipView -> IO (Id NSView)
documentView nsClipView  =
  sendMsg nsClipView (mkSelector "documentView") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- setDocumentView:@
setDocumentView :: (IsNSClipView nsClipView, IsNSView value) => nsClipView -> value -> IO ()
setDocumentView nsClipView  value =
withObjCPtr value $ \raw_value ->
    sendMsg nsClipView (mkSelector "setDocumentView:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- | @- documentRect@
documentRect :: IsNSClipView nsClipView => nsClipView -> IO NSRect
documentRect nsClipView  =
  sendMsgStret nsClipView (mkSelector "documentRect") retNSRect []

-- | @- documentCursor@
documentCursor :: IsNSClipView nsClipView => nsClipView -> IO (Id NSCursor)
documentCursor nsClipView  =
  sendMsg nsClipView (mkSelector "documentCursor") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- setDocumentCursor:@
setDocumentCursor :: (IsNSClipView nsClipView, IsNSCursor value) => nsClipView -> value -> IO ()
setDocumentCursor nsClipView  value =
withObjCPtr value $ \raw_value ->
    sendMsg nsClipView (mkSelector "setDocumentCursor:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- | @- documentVisibleRect@
documentVisibleRect :: IsNSClipView nsClipView => nsClipView -> IO NSRect
documentVisibleRect nsClipView  =
  sendMsgStret nsClipView (mkSelector "documentVisibleRect") retNSRect []

-- | @- contentInsets@
contentInsets :: IsNSClipView nsClipView => nsClipView -> IO NSEdgeInsets
contentInsets nsClipView  =
  sendMsgStret nsClipView (mkSelector "contentInsets") retNSEdgeInsets []

-- | @- setContentInsets:@
setContentInsets :: IsNSClipView nsClipView => nsClipView -> NSEdgeInsets -> IO ()
setContentInsets nsClipView  value =
  sendMsg nsClipView (mkSelector "setContentInsets:") retVoid [argNSEdgeInsets value]

-- | @- automaticallyAdjustsContentInsets@
automaticallyAdjustsContentInsets :: IsNSClipView nsClipView => nsClipView -> IO Bool
automaticallyAdjustsContentInsets nsClipView  =
  fmap ((/= 0) :: CULong -> Bool) $ sendMsg nsClipView (mkSelector "automaticallyAdjustsContentInsets") retCULong []

-- | @- setAutomaticallyAdjustsContentInsets:@
setAutomaticallyAdjustsContentInsets :: IsNSClipView nsClipView => nsClipView -> Bool -> IO ()
setAutomaticallyAdjustsContentInsets nsClipView  value =
  sendMsg nsClipView (mkSelector "setAutomaticallyAdjustsContentInsets:") retVoid [argCULong (if value then 1 else 0)]

-- | @- copiesOnScroll@
copiesOnScroll :: IsNSClipView nsClipView => nsClipView -> IO Bool
copiesOnScroll nsClipView  =
  fmap ((/= 0) :: CULong -> Bool) $ sendMsg nsClipView (mkSelector "copiesOnScroll") retCULong []

-- | @- setCopiesOnScroll:@
setCopiesOnScroll :: IsNSClipView nsClipView => nsClipView -> Bool -> IO ()
setCopiesOnScroll nsClipView  value =
  sendMsg nsClipView (mkSelector "setCopiesOnScroll:") retVoid [argCULong (if value then 1 else 0)]

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @viewFrameChanged:@
viewFrameChangedSelector :: Selector
viewFrameChangedSelector = mkSelector "viewFrameChanged:"

-- | @Selector@ for @viewBoundsChanged:@
viewBoundsChangedSelector :: Selector
viewBoundsChangedSelector = mkSelector "viewBoundsChanged:"

-- | @Selector@ for @autoscroll:@
autoscrollSelector :: Selector
autoscrollSelector = mkSelector "autoscroll:"

-- | @Selector@ for @scrollToPoint:@
scrollToPointSelector :: Selector
scrollToPointSelector = mkSelector "scrollToPoint:"

-- | @Selector@ for @constrainBoundsRect:@
constrainBoundsRectSelector :: Selector
constrainBoundsRectSelector = mkSelector "constrainBoundsRect:"

-- | @Selector@ for @constrainScrollPoint:@
constrainScrollPointSelector :: Selector
constrainScrollPointSelector = mkSelector "constrainScrollPoint:"

-- | @Selector@ for @backgroundColor@
backgroundColorSelector :: Selector
backgroundColorSelector = mkSelector "backgroundColor"

-- | @Selector@ for @setBackgroundColor:@
setBackgroundColorSelector :: Selector
setBackgroundColorSelector = mkSelector "setBackgroundColor:"

-- | @Selector@ for @drawsBackground@
drawsBackgroundSelector :: Selector
drawsBackgroundSelector = mkSelector "drawsBackground"

-- | @Selector@ for @setDrawsBackground:@
setDrawsBackgroundSelector :: Selector
setDrawsBackgroundSelector = mkSelector "setDrawsBackground:"

-- | @Selector@ for @documentView@
documentViewSelector :: Selector
documentViewSelector = mkSelector "documentView"

-- | @Selector@ for @setDocumentView:@
setDocumentViewSelector :: Selector
setDocumentViewSelector = mkSelector "setDocumentView:"

-- | @Selector@ for @documentRect@
documentRectSelector :: Selector
documentRectSelector = mkSelector "documentRect"

-- | @Selector@ for @documentCursor@
documentCursorSelector :: Selector
documentCursorSelector = mkSelector "documentCursor"

-- | @Selector@ for @setDocumentCursor:@
setDocumentCursorSelector :: Selector
setDocumentCursorSelector = mkSelector "setDocumentCursor:"

-- | @Selector@ for @documentVisibleRect@
documentVisibleRectSelector :: Selector
documentVisibleRectSelector = mkSelector "documentVisibleRect"

-- | @Selector@ for @contentInsets@
contentInsetsSelector :: Selector
contentInsetsSelector = mkSelector "contentInsets"

-- | @Selector@ for @setContentInsets:@
setContentInsetsSelector :: Selector
setContentInsetsSelector = mkSelector "setContentInsets:"

-- | @Selector@ for @automaticallyAdjustsContentInsets@
automaticallyAdjustsContentInsetsSelector :: Selector
automaticallyAdjustsContentInsetsSelector = mkSelector "automaticallyAdjustsContentInsets"

-- | @Selector@ for @setAutomaticallyAdjustsContentInsets:@
setAutomaticallyAdjustsContentInsetsSelector :: Selector
setAutomaticallyAdjustsContentInsetsSelector = mkSelector "setAutomaticallyAdjustsContentInsets:"

-- | @Selector@ for @copiesOnScroll@
copiesOnScrollSelector :: Selector
copiesOnScrollSelector = mkSelector "copiesOnScroll"

-- | @Selector@ for @setCopiesOnScroll:@
setCopiesOnScrollSelector :: Selector
setCopiesOnScrollSelector = mkSelector "setCopiesOnScroll:"

