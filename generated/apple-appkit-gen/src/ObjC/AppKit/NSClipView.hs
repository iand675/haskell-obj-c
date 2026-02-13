{-# LANGUAGE DataKinds #-}
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
  , automaticallyAdjustsContentInsetsSelector
  , autoscrollSelector
  , backgroundColorSelector
  , constrainBoundsRectSelector
  , constrainScrollPointSelector
  , contentInsetsSelector
  , copiesOnScrollSelector
  , documentCursorSelector
  , documentRectSelector
  , documentViewSelector
  , documentVisibleRectSelector
  , drawsBackgroundSelector
  , scrollToPointSelector
  , setAutomaticallyAdjustsContentInsetsSelector
  , setBackgroundColorSelector
  , setContentInsetsSelector
  , setCopiesOnScrollSelector
  , setDocumentCursorSelector
  , setDocumentViewSelector
  , setDrawsBackgroundSelector
  , viewBoundsChangedSelector
  , viewFrameChangedSelector


  ) where

import Foreign.Ptr (Ptr, FunPtr)
import Foreign.C.Types

import ObjC.Runtime.Types
import ObjC.Runtime.Message (sendMessage, sendOwnedMessage, sendClassMessage, sendOwnedClassMessage)
import ObjC.Runtime.Selector (mkSelector)
import ObjC.Runtime.Class (getRequiredClass)

import ObjC.AppKit.Internal.Classes
import ObjC.Foundation.Internal.Structs
import ObjC.Foundation.Internal.Classes

-- | @- viewFrameChanged:@
viewFrameChanged :: (IsNSClipView nsClipView, IsNSNotification notification) => nsClipView -> notification -> IO ()
viewFrameChanged nsClipView notification =
  sendMessage nsClipView viewFrameChangedSelector (toNSNotification notification)

-- | @- viewBoundsChanged:@
viewBoundsChanged :: (IsNSClipView nsClipView, IsNSNotification notification) => nsClipView -> notification -> IO ()
viewBoundsChanged nsClipView notification =
  sendMessage nsClipView viewBoundsChangedSelector (toNSNotification notification)

-- | @- autoscroll:@
autoscroll :: (IsNSClipView nsClipView, IsNSEvent event) => nsClipView -> event -> IO Bool
autoscroll nsClipView event =
  sendMessage nsClipView autoscrollSelector (toNSEvent event)

-- | @- scrollToPoint:@
scrollToPoint :: IsNSClipView nsClipView => nsClipView -> NSPoint -> IO ()
scrollToPoint nsClipView newOrigin =
  sendMessage nsClipView scrollToPointSelector newOrigin

-- | @- constrainBoundsRect:@
constrainBoundsRect :: IsNSClipView nsClipView => nsClipView -> NSRect -> IO NSRect
constrainBoundsRect nsClipView proposedBounds =
  sendMessage nsClipView constrainBoundsRectSelector proposedBounds

-- | @- constrainScrollPoint:@
constrainScrollPoint :: IsNSClipView nsClipView => nsClipView -> NSPoint -> IO NSPoint
constrainScrollPoint nsClipView newOrigin =
  sendMessage nsClipView constrainScrollPointSelector newOrigin

-- | @- backgroundColor@
backgroundColor :: IsNSClipView nsClipView => nsClipView -> IO (Id NSColor)
backgroundColor nsClipView =
  sendMessage nsClipView backgroundColorSelector

-- | @- setBackgroundColor:@
setBackgroundColor :: (IsNSClipView nsClipView, IsNSColor value) => nsClipView -> value -> IO ()
setBackgroundColor nsClipView value =
  sendMessage nsClipView setBackgroundColorSelector (toNSColor value)

-- | @- drawsBackground@
drawsBackground :: IsNSClipView nsClipView => nsClipView -> IO Bool
drawsBackground nsClipView =
  sendMessage nsClipView drawsBackgroundSelector

-- | @- setDrawsBackground:@
setDrawsBackground :: IsNSClipView nsClipView => nsClipView -> Bool -> IO ()
setDrawsBackground nsClipView value =
  sendMessage nsClipView setDrawsBackgroundSelector value

-- | @- documentView@
documentView :: IsNSClipView nsClipView => nsClipView -> IO (Id NSView)
documentView nsClipView =
  sendMessage nsClipView documentViewSelector

-- | @- setDocumentView:@
setDocumentView :: (IsNSClipView nsClipView, IsNSView value) => nsClipView -> value -> IO ()
setDocumentView nsClipView value =
  sendMessage nsClipView setDocumentViewSelector (toNSView value)

-- | @- documentRect@
documentRect :: IsNSClipView nsClipView => nsClipView -> IO NSRect
documentRect nsClipView =
  sendMessage nsClipView documentRectSelector

-- | @- documentCursor@
documentCursor :: IsNSClipView nsClipView => nsClipView -> IO (Id NSCursor)
documentCursor nsClipView =
  sendMessage nsClipView documentCursorSelector

-- | @- setDocumentCursor:@
setDocumentCursor :: (IsNSClipView nsClipView, IsNSCursor value) => nsClipView -> value -> IO ()
setDocumentCursor nsClipView value =
  sendMessage nsClipView setDocumentCursorSelector (toNSCursor value)

-- | @- documentVisibleRect@
documentVisibleRect :: IsNSClipView nsClipView => nsClipView -> IO NSRect
documentVisibleRect nsClipView =
  sendMessage nsClipView documentVisibleRectSelector

-- | @- contentInsets@
contentInsets :: IsNSClipView nsClipView => nsClipView -> IO NSEdgeInsets
contentInsets nsClipView =
  sendMessage nsClipView contentInsetsSelector

-- | @- setContentInsets:@
setContentInsets :: IsNSClipView nsClipView => nsClipView -> NSEdgeInsets -> IO ()
setContentInsets nsClipView value =
  sendMessage nsClipView setContentInsetsSelector value

-- | @- automaticallyAdjustsContentInsets@
automaticallyAdjustsContentInsets :: IsNSClipView nsClipView => nsClipView -> IO Bool
automaticallyAdjustsContentInsets nsClipView =
  sendMessage nsClipView automaticallyAdjustsContentInsetsSelector

-- | @- setAutomaticallyAdjustsContentInsets:@
setAutomaticallyAdjustsContentInsets :: IsNSClipView nsClipView => nsClipView -> Bool -> IO ()
setAutomaticallyAdjustsContentInsets nsClipView value =
  sendMessage nsClipView setAutomaticallyAdjustsContentInsetsSelector value

-- | @- copiesOnScroll@
copiesOnScroll :: IsNSClipView nsClipView => nsClipView -> IO Bool
copiesOnScroll nsClipView =
  sendMessage nsClipView copiesOnScrollSelector

-- | @- setCopiesOnScroll:@
setCopiesOnScroll :: IsNSClipView nsClipView => nsClipView -> Bool -> IO ()
setCopiesOnScroll nsClipView value =
  sendMessage nsClipView setCopiesOnScrollSelector value

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @viewFrameChanged:@
viewFrameChangedSelector :: Selector '[Id NSNotification] ()
viewFrameChangedSelector = mkSelector "viewFrameChanged:"

-- | @Selector@ for @viewBoundsChanged:@
viewBoundsChangedSelector :: Selector '[Id NSNotification] ()
viewBoundsChangedSelector = mkSelector "viewBoundsChanged:"

-- | @Selector@ for @autoscroll:@
autoscrollSelector :: Selector '[Id NSEvent] Bool
autoscrollSelector = mkSelector "autoscroll:"

-- | @Selector@ for @scrollToPoint:@
scrollToPointSelector :: Selector '[NSPoint] ()
scrollToPointSelector = mkSelector "scrollToPoint:"

-- | @Selector@ for @constrainBoundsRect:@
constrainBoundsRectSelector :: Selector '[NSRect] NSRect
constrainBoundsRectSelector = mkSelector "constrainBoundsRect:"

-- | @Selector@ for @constrainScrollPoint:@
constrainScrollPointSelector :: Selector '[NSPoint] NSPoint
constrainScrollPointSelector = mkSelector "constrainScrollPoint:"

-- | @Selector@ for @backgroundColor@
backgroundColorSelector :: Selector '[] (Id NSColor)
backgroundColorSelector = mkSelector "backgroundColor"

-- | @Selector@ for @setBackgroundColor:@
setBackgroundColorSelector :: Selector '[Id NSColor] ()
setBackgroundColorSelector = mkSelector "setBackgroundColor:"

-- | @Selector@ for @drawsBackground@
drawsBackgroundSelector :: Selector '[] Bool
drawsBackgroundSelector = mkSelector "drawsBackground"

-- | @Selector@ for @setDrawsBackground:@
setDrawsBackgroundSelector :: Selector '[Bool] ()
setDrawsBackgroundSelector = mkSelector "setDrawsBackground:"

-- | @Selector@ for @documentView@
documentViewSelector :: Selector '[] (Id NSView)
documentViewSelector = mkSelector "documentView"

-- | @Selector@ for @setDocumentView:@
setDocumentViewSelector :: Selector '[Id NSView] ()
setDocumentViewSelector = mkSelector "setDocumentView:"

-- | @Selector@ for @documentRect@
documentRectSelector :: Selector '[] NSRect
documentRectSelector = mkSelector "documentRect"

-- | @Selector@ for @documentCursor@
documentCursorSelector :: Selector '[] (Id NSCursor)
documentCursorSelector = mkSelector "documentCursor"

-- | @Selector@ for @setDocumentCursor:@
setDocumentCursorSelector :: Selector '[Id NSCursor] ()
setDocumentCursorSelector = mkSelector "setDocumentCursor:"

-- | @Selector@ for @documentVisibleRect@
documentVisibleRectSelector :: Selector '[] NSRect
documentVisibleRectSelector = mkSelector "documentVisibleRect"

-- | @Selector@ for @contentInsets@
contentInsetsSelector :: Selector '[] NSEdgeInsets
contentInsetsSelector = mkSelector "contentInsets"

-- | @Selector@ for @setContentInsets:@
setContentInsetsSelector :: Selector '[NSEdgeInsets] ()
setContentInsetsSelector = mkSelector "setContentInsets:"

-- | @Selector@ for @automaticallyAdjustsContentInsets@
automaticallyAdjustsContentInsetsSelector :: Selector '[] Bool
automaticallyAdjustsContentInsetsSelector = mkSelector "automaticallyAdjustsContentInsets"

-- | @Selector@ for @setAutomaticallyAdjustsContentInsets:@
setAutomaticallyAdjustsContentInsetsSelector :: Selector '[Bool] ()
setAutomaticallyAdjustsContentInsetsSelector = mkSelector "setAutomaticallyAdjustsContentInsets:"

-- | @Selector@ for @copiesOnScroll@
copiesOnScrollSelector :: Selector '[] Bool
copiesOnScrollSelector = mkSelector "copiesOnScroll"

-- | @Selector@ for @setCopiesOnScroll:@
setCopiesOnScrollSelector :: Selector '[Bool] ()
setCopiesOnScrollSelector = mkSelector "setCopiesOnScroll:"

