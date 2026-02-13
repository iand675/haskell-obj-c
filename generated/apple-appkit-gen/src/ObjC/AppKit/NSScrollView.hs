{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Generated bindings for @NSScrollView@.
module ObjC.AppKit.NSScrollView
  ( NSScrollView
  , IsNSScrollView(..)
  , initWithFrame
  , initWithCoder
  , frameSizeForContentSize_horizontalScrollerClass_verticalScrollerClass_borderType_controlSize_scrollerStyle
  , contentSizeForFrameSize_horizontalScrollerClass_verticalScrollerClass_borderType_controlSize_scrollerStyle
  , frameSizeForContentSize_hasHorizontalScroller_hasVerticalScroller_borderType
  , contentSizeForFrameSize_hasHorizontalScroller_hasVerticalScroller_borderType
  , tile
  , reflectScrolledClipView
  , scrollWheel
  , flashScrollers
  , magnifyToFitRect
  , setMagnification_centeredAtPoint
  , addFloatingSubview_forAxis
  , documentVisibleRect
  , contentSize
  , documentView
  , setDocumentView
  , contentView
  , setContentView
  , documentCursor
  , setDocumentCursor
  , borderType
  , setBorderType
  , backgroundColor
  , setBackgroundColor
  , drawsBackground
  , setDrawsBackground
  , hasVerticalScroller
  , setHasVerticalScroller
  , hasHorizontalScroller
  , setHasHorizontalScroller
  , verticalScroller
  , setVerticalScroller
  , horizontalScroller
  , setHorizontalScroller
  , autohidesScrollers
  , setAutohidesScrollers
  , horizontalLineScroll
  , setHorizontalLineScroll
  , verticalLineScroll
  , setVerticalLineScroll
  , lineScroll
  , setLineScroll
  , horizontalPageScroll
  , setHorizontalPageScroll
  , verticalPageScroll
  , setVerticalPageScroll
  , pageScroll
  , setPageScroll
  , scrollsDynamically
  , setScrollsDynamically
  , scrollerStyle
  , setScrollerStyle
  , scrollerKnobStyle
  , setScrollerKnobStyle
  , horizontalScrollElasticity
  , setHorizontalScrollElasticity
  , verticalScrollElasticity
  , setVerticalScrollElasticity
  , usesPredominantAxisScrolling
  , setUsesPredominantAxisScrolling
  , allowsMagnification
  , setAllowsMagnification
  , magnification
  , setMagnification
  , maxMagnification
  , setMaxMagnification
  , minMagnification
  , setMinMagnification
  , automaticallyAdjustsContentInsets
  , setAutomaticallyAdjustsContentInsets
  , contentInsets
  , setContentInsets
  , scrollerInsets
  , setScrollerInsets
  , findBarPosition
  , setFindBarPosition
  , rulerViewClass
  , setRulerViewClass
  , rulersVisible
  , setRulersVisible
  , hasHorizontalRuler
  , setHasHorizontalRuler
  , hasVerticalRuler
  , setHasVerticalRuler
  , horizontalRulerView
  , setHorizontalRulerView
  , verticalRulerView
  , setVerticalRulerView
  , addFloatingSubview_forAxisSelector
  , allowsMagnificationSelector
  , autohidesScrollersSelector
  , automaticallyAdjustsContentInsetsSelector
  , backgroundColorSelector
  , borderTypeSelector
  , contentInsetsSelector
  , contentSizeForFrameSize_hasHorizontalScroller_hasVerticalScroller_borderTypeSelector
  , contentSizeForFrameSize_horizontalScrollerClass_verticalScrollerClass_borderType_controlSize_scrollerStyleSelector
  , contentSizeSelector
  , contentViewSelector
  , documentCursorSelector
  , documentViewSelector
  , documentVisibleRectSelector
  , drawsBackgroundSelector
  , findBarPositionSelector
  , flashScrollersSelector
  , frameSizeForContentSize_hasHorizontalScroller_hasVerticalScroller_borderTypeSelector
  , frameSizeForContentSize_horizontalScrollerClass_verticalScrollerClass_borderType_controlSize_scrollerStyleSelector
  , hasHorizontalRulerSelector
  , hasHorizontalScrollerSelector
  , hasVerticalRulerSelector
  , hasVerticalScrollerSelector
  , horizontalLineScrollSelector
  , horizontalPageScrollSelector
  , horizontalRulerViewSelector
  , horizontalScrollElasticitySelector
  , horizontalScrollerSelector
  , initWithCoderSelector
  , initWithFrameSelector
  , lineScrollSelector
  , magnificationSelector
  , magnifyToFitRectSelector
  , maxMagnificationSelector
  , minMagnificationSelector
  , pageScrollSelector
  , reflectScrolledClipViewSelector
  , rulerViewClassSelector
  , rulersVisibleSelector
  , scrollWheelSelector
  , scrollerInsetsSelector
  , scrollerKnobStyleSelector
  , scrollerStyleSelector
  , scrollsDynamicallySelector
  , setAllowsMagnificationSelector
  , setAutohidesScrollersSelector
  , setAutomaticallyAdjustsContentInsetsSelector
  , setBackgroundColorSelector
  , setBorderTypeSelector
  , setContentInsetsSelector
  , setContentViewSelector
  , setDocumentCursorSelector
  , setDocumentViewSelector
  , setDrawsBackgroundSelector
  , setFindBarPositionSelector
  , setHasHorizontalRulerSelector
  , setHasHorizontalScrollerSelector
  , setHasVerticalRulerSelector
  , setHasVerticalScrollerSelector
  , setHorizontalLineScrollSelector
  , setHorizontalPageScrollSelector
  , setHorizontalRulerViewSelector
  , setHorizontalScrollElasticitySelector
  , setHorizontalScrollerSelector
  , setLineScrollSelector
  , setMagnificationSelector
  , setMagnification_centeredAtPointSelector
  , setMaxMagnificationSelector
  , setMinMagnificationSelector
  , setPageScrollSelector
  , setRulerViewClassSelector
  , setRulersVisibleSelector
  , setScrollerInsetsSelector
  , setScrollerKnobStyleSelector
  , setScrollerStyleSelector
  , setScrollsDynamicallySelector
  , setUsesPredominantAxisScrollingSelector
  , setVerticalLineScrollSelector
  , setVerticalPageScrollSelector
  , setVerticalRulerViewSelector
  , setVerticalScrollElasticitySelector
  , setVerticalScrollerSelector
  , tileSelector
  , usesPredominantAxisScrollingSelector
  , verticalLineScrollSelector
  , verticalPageScrollSelector
  , verticalRulerViewSelector
  , verticalScrollElasticitySelector
  , verticalScrollerSelector

  -- * Enum types
  , NSBorderType(NSBorderType)
  , pattern NSNoBorder
  , pattern NSLineBorder
  , pattern NSBezelBorder
  , pattern NSGrooveBorder
  , NSControlSize(NSControlSize)
  , pattern NSControlSizeRegular
  , pattern NSControlSizeSmall
  , pattern NSControlSizeMini
  , pattern NSControlSizeLarge
  , pattern NSControlSizeExtraLarge
  , NSEventGestureAxis(NSEventGestureAxis)
  , pattern NSEventGestureAxisNone
  , pattern NSEventGestureAxisHorizontal
  , pattern NSEventGestureAxisVertical
  , NSScrollElasticity(NSScrollElasticity)
  , pattern NSScrollElasticityAutomatic
  , pattern NSScrollElasticityNone
  , pattern NSScrollElasticityAllowed
  , NSScrollViewFindBarPosition(NSScrollViewFindBarPosition)
  , pattern NSScrollViewFindBarPositionAboveHorizontalRuler
  , pattern NSScrollViewFindBarPositionAboveContent
  , pattern NSScrollViewFindBarPositionBelowContent
  , NSScrollerKnobStyle(NSScrollerKnobStyle)
  , pattern NSScrollerKnobStyleDefault
  , pattern NSScrollerKnobStyleDark
  , pattern NSScrollerKnobStyleLight
  , NSScrollerStyle(NSScrollerStyle)
  , pattern NSScrollerStyleLegacy
  , pattern NSScrollerStyleOverlay

  ) where

import Foreign.Ptr (Ptr, FunPtr)
import Foreign.C.Types

import ObjC.Runtime.Types
import ObjC.Runtime.Message (sendMessage, sendOwnedMessage, sendClassMessage, sendOwnedClassMessage)
import ObjC.Runtime.Selector (mkSelector)
import ObjC.Runtime.Class (getRequiredClass)

import ObjC.AppKit.Internal.Classes
import ObjC.Foundation.Internal.Structs
import ObjC.AppKit.Internal.Enums
import ObjC.Foundation.Internal.Classes

-- | @- initWithFrame:@
initWithFrame :: IsNSScrollView nsScrollView => nsScrollView -> NSRect -> IO (Id NSScrollView)
initWithFrame nsScrollView frameRect =
  sendOwnedMessage nsScrollView initWithFrameSelector frameRect

-- | @- initWithCoder:@
initWithCoder :: (IsNSScrollView nsScrollView, IsNSCoder coder) => nsScrollView -> coder -> IO (Id NSScrollView)
initWithCoder nsScrollView coder =
  sendOwnedMessage nsScrollView initWithCoderSelector (toNSCoder coder)

-- | @+ frameSizeForContentSize:horizontalScrollerClass:verticalScrollerClass:borderType:controlSize:scrollerStyle:@
frameSizeForContentSize_horizontalScrollerClass_verticalScrollerClass_borderType_controlSize_scrollerStyle :: NSSize -> Class -> Class -> NSBorderType -> NSControlSize -> NSScrollerStyle -> IO NSSize
frameSizeForContentSize_horizontalScrollerClass_verticalScrollerClass_borderType_controlSize_scrollerStyle cSize horizontalScrollerClass verticalScrollerClass type_ controlSize scrollerStyle =
  do
    cls' <- getRequiredClass "NSScrollView"
    sendClassMessage cls' frameSizeForContentSize_horizontalScrollerClass_verticalScrollerClass_borderType_controlSize_scrollerStyleSelector cSize horizontalScrollerClass verticalScrollerClass type_ controlSize scrollerStyle

-- | @+ contentSizeForFrameSize:horizontalScrollerClass:verticalScrollerClass:borderType:controlSize:scrollerStyle:@
contentSizeForFrameSize_horizontalScrollerClass_verticalScrollerClass_borderType_controlSize_scrollerStyle :: NSSize -> Class -> Class -> NSBorderType -> NSControlSize -> NSScrollerStyle -> IO NSSize
contentSizeForFrameSize_horizontalScrollerClass_verticalScrollerClass_borderType_controlSize_scrollerStyle fSize horizontalScrollerClass verticalScrollerClass type_ controlSize scrollerStyle =
  do
    cls' <- getRequiredClass "NSScrollView"
    sendClassMessage cls' contentSizeForFrameSize_horizontalScrollerClass_verticalScrollerClass_borderType_controlSize_scrollerStyleSelector fSize horizontalScrollerClass verticalScrollerClass type_ controlSize scrollerStyle

-- | @+ frameSizeForContentSize:hasHorizontalScroller:hasVerticalScroller:borderType:@
frameSizeForContentSize_hasHorizontalScroller_hasVerticalScroller_borderType :: NSSize -> Bool -> Bool -> NSBorderType -> IO NSSize
frameSizeForContentSize_hasHorizontalScroller_hasVerticalScroller_borderType cSize hFlag vFlag type_ =
  do
    cls' <- getRequiredClass "NSScrollView"
    sendClassMessage cls' frameSizeForContentSize_hasHorizontalScroller_hasVerticalScroller_borderTypeSelector cSize hFlag vFlag type_

-- | @+ contentSizeForFrameSize:hasHorizontalScroller:hasVerticalScroller:borderType:@
contentSizeForFrameSize_hasHorizontalScroller_hasVerticalScroller_borderType :: NSSize -> Bool -> Bool -> NSBorderType -> IO NSSize
contentSizeForFrameSize_hasHorizontalScroller_hasVerticalScroller_borderType fSize hFlag vFlag type_ =
  do
    cls' <- getRequiredClass "NSScrollView"
    sendClassMessage cls' contentSizeForFrameSize_hasHorizontalScroller_hasVerticalScroller_borderTypeSelector fSize hFlag vFlag type_

-- | @- tile@
tile :: IsNSScrollView nsScrollView => nsScrollView -> IO ()
tile nsScrollView =
  sendMessage nsScrollView tileSelector

-- | @- reflectScrolledClipView:@
reflectScrolledClipView :: (IsNSScrollView nsScrollView, IsNSClipView cView) => nsScrollView -> cView -> IO ()
reflectScrolledClipView nsScrollView cView =
  sendMessage nsScrollView reflectScrolledClipViewSelector (toNSClipView cView)

-- | @- scrollWheel:@
scrollWheel :: (IsNSScrollView nsScrollView, IsNSEvent event) => nsScrollView -> event -> IO ()
scrollWheel nsScrollView event =
  sendMessage nsScrollView scrollWheelSelector (toNSEvent event)

-- | @- flashScrollers@
flashScrollers :: IsNSScrollView nsScrollView => nsScrollView -> IO ()
flashScrollers nsScrollView =
  sendMessage nsScrollView flashScrollersSelector

-- | @- magnifyToFitRect:@
magnifyToFitRect :: IsNSScrollView nsScrollView => nsScrollView -> NSRect -> IO ()
magnifyToFitRect nsScrollView rect =
  sendMessage nsScrollView magnifyToFitRectSelector rect

-- | @- setMagnification:centeredAtPoint:@
setMagnification_centeredAtPoint :: IsNSScrollView nsScrollView => nsScrollView -> CDouble -> NSPoint -> IO ()
setMagnification_centeredAtPoint nsScrollView magnification point =
  sendMessage nsScrollView setMagnification_centeredAtPointSelector magnification point

-- | @- addFloatingSubview:forAxis:@
addFloatingSubview_forAxis :: (IsNSScrollView nsScrollView, IsNSView view) => nsScrollView -> view -> NSEventGestureAxis -> IO ()
addFloatingSubview_forAxis nsScrollView view axis =
  sendMessage nsScrollView addFloatingSubview_forAxisSelector (toNSView view) axis

-- | @- documentVisibleRect@
documentVisibleRect :: IsNSScrollView nsScrollView => nsScrollView -> IO NSRect
documentVisibleRect nsScrollView =
  sendMessage nsScrollView documentVisibleRectSelector

-- | @- contentSize@
contentSize :: IsNSScrollView nsScrollView => nsScrollView -> IO NSSize
contentSize nsScrollView =
  sendMessage nsScrollView contentSizeSelector

-- | @- documentView@
documentView :: IsNSScrollView nsScrollView => nsScrollView -> IO (Id NSView)
documentView nsScrollView =
  sendMessage nsScrollView documentViewSelector

-- | @- setDocumentView:@
setDocumentView :: (IsNSScrollView nsScrollView, IsNSView value) => nsScrollView -> value -> IO ()
setDocumentView nsScrollView value =
  sendMessage nsScrollView setDocumentViewSelector (toNSView value)

-- | @- contentView@
contentView :: IsNSScrollView nsScrollView => nsScrollView -> IO (Id NSClipView)
contentView nsScrollView =
  sendMessage nsScrollView contentViewSelector

-- | @- setContentView:@
setContentView :: (IsNSScrollView nsScrollView, IsNSClipView value) => nsScrollView -> value -> IO ()
setContentView nsScrollView value =
  sendMessage nsScrollView setContentViewSelector (toNSClipView value)

-- | @- documentCursor@
documentCursor :: IsNSScrollView nsScrollView => nsScrollView -> IO (Id NSCursor)
documentCursor nsScrollView =
  sendMessage nsScrollView documentCursorSelector

-- | @- setDocumentCursor:@
setDocumentCursor :: (IsNSScrollView nsScrollView, IsNSCursor value) => nsScrollView -> value -> IO ()
setDocumentCursor nsScrollView value =
  sendMessage nsScrollView setDocumentCursorSelector (toNSCursor value)

-- | @- borderType@
borderType :: IsNSScrollView nsScrollView => nsScrollView -> IO NSBorderType
borderType nsScrollView =
  sendMessage nsScrollView borderTypeSelector

-- | @- setBorderType:@
setBorderType :: IsNSScrollView nsScrollView => nsScrollView -> NSBorderType -> IO ()
setBorderType nsScrollView value =
  sendMessage nsScrollView setBorderTypeSelector value

-- | @- backgroundColor@
backgroundColor :: IsNSScrollView nsScrollView => nsScrollView -> IO (Id NSColor)
backgroundColor nsScrollView =
  sendMessage nsScrollView backgroundColorSelector

-- | @- setBackgroundColor:@
setBackgroundColor :: (IsNSScrollView nsScrollView, IsNSColor value) => nsScrollView -> value -> IO ()
setBackgroundColor nsScrollView value =
  sendMessage nsScrollView setBackgroundColorSelector (toNSColor value)

-- | @- drawsBackground@
drawsBackground :: IsNSScrollView nsScrollView => nsScrollView -> IO Bool
drawsBackground nsScrollView =
  sendMessage nsScrollView drawsBackgroundSelector

-- | @- setDrawsBackground:@
setDrawsBackground :: IsNSScrollView nsScrollView => nsScrollView -> Bool -> IO ()
setDrawsBackground nsScrollView value =
  sendMessage nsScrollView setDrawsBackgroundSelector value

-- | @- hasVerticalScroller@
hasVerticalScroller :: IsNSScrollView nsScrollView => nsScrollView -> IO Bool
hasVerticalScroller nsScrollView =
  sendMessage nsScrollView hasVerticalScrollerSelector

-- | @- setHasVerticalScroller:@
setHasVerticalScroller :: IsNSScrollView nsScrollView => nsScrollView -> Bool -> IO ()
setHasVerticalScroller nsScrollView value =
  sendMessage nsScrollView setHasVerticalScrollerSelector value

-- | @- hasHorizontalScroller@
hasHorizontalScroller :: IsNSScrollView nsScrollView => nsScrollView -> IO Bool
hasHorizontalScroller nsScrollView =
  sendMessage nsScrollView hasHorizontalScrollerSelector

-- | @- setHasHorizontalScroller:@
setHasHorizontalScroller :: IsNSScrollView nsScrollView => nsScrollView -> Bool -> IO ()
setHasHorizontalScroller nsScrollView value =
  sendMessage nsScrollView setHasHorizontalScrollerSelector value

-- | @- verticalScroller@
verticalScroller :: IsNSScrollView nsScrollView => nsScrollView -> IO (Id NSScroller)
verticalScroller nsScrollView =
  sendMessage nsScrollView verticalScrollerSelector

-- | @- setVerticalScroller:@
setVerticalScroller :: (IsNSScrollView nsScrollView, IsNSScroller value) => nsScrollView -> value -> IO ()
setVerticalScroller nsScrollView value =
  sendMessage nsScrollView setVerticalScrollerSelector (toNSScroller value)

-- | @- horizontalScroller@
horizontalScroller :: IsNSScrollView nsScrollView => nsScrollView -> IO (Id NSScroller)
horizontalScroller nsScrollView =
  sendMessage nsScrollView horizontalScrollerSelector

-- | @- setHorizontalScroller:@
setHorizontalScroller :: (IsNSScrollView nsScrollView, IsNSScroller value) => nsScrollView -> value -> IO ()
setHorizontalScroller nsScrollView value =
  sendMessage nsScrollView setHorizontalScrollerSelector (toNSScroller value)

-- | @- autohidesScrollers@
autohidesScrollers :: IsNSScrollView nsScrollView => nsScrollView -> IO Bool
autohidesScrollers nsScrollView =
  sendMessage nsScrollView autohidesScrollersSelector

-- | @- setAutohidesScrollers:@
setAutohidesScrollers :: IsNSScrollView nsScrollView => nsScrollView -> Bool -> IO ()
setAutohidesScrollers nsScrollView value =
  sendMessage nsScrollView setAutohidesScrollersSelector value

-- | @- horizontalLineScroll@
horizontalLineScroll :: IsNSScrollView nsScrollView => nsScrollView -> IO CDouble
horizontalLineScroll nsScrollView =
  sendMessage nsScrollView horizontalLineScrollSelector

-- | @- setHorizontalLineScroll:@
setHorizontalLineScroll :: IsNSScrollView nsScrollView => nsScrollView -> CDouble -> IO ()
setHorizontalLineScroll nsScrollView value =
  sendMessage nsScrollView setHorizontalLineScrollSelector value

-- | @- verticalLineScroll@
verticalLineScroll :: IsNSScrollView nsScrollView => nsScrollView -> IO CDouble
verticalLineScroll nsScrollView =
  sendMessage nsScrollView verticalLineScrollSelector

-- | @- setVerticalLineScroll:@
setVerticalLineScroll :: IsNSScrollView nsScrollView => nsScrollView -> CDouble -> IO ()
setVerticalLineScroll nsScrollView value =
  sendMessage nsScrollView setVerticalLineScrollSelector value

-- | @- lineScroll@
lineScroll :: IsNSScrollView nsScrollView => nsScrollView -> IO CDouble
lineScroll nsScrollView =
  sendMessage nsScrollView lineScrollSelector

-- | @- setLineScroll:@
setLineScroll :: IsNSScrollView nsScrollView => nsScrollView -> CDouble -> IO ()
setLineScroll nsScrollView value =
  sendMessage nsScrollView setLineScrollSelector value

-- | @- horizontalPageScroll@
horizontalPageScroll :: IsNSScrollView nsScrollView => nsScrollView -> IO CDouble
horizontalPageScroll nsScrollView =
  sendMessage nsScrollView horizontalPageScrollSelector

-- | @- setHorizontalPageScroll:@
setHorizontalPageScroll :: IsNSScrollView nsScrollView => nsScrollView -> CDouble -> IO ()
setHorizontalPageScroll nsScrollView value =
  sendMessage nsScrollView setHorizontalPageScrollSelector value

-- | @- verticalPageScroll@
verticalPageScroll :: IsNSScrollView nsScrollView => nsScrollView -> IO CDouble
verticalPageScroll nsScrollView =
  sendMessage nsScrollView verticalPageScrollSelector

-- | @- setVerticalPageScroll:@
setVerticalPageScroll :: IsNSScrollView nsScrollView => nsScrollView -> CDouble -> IO ()
setVerticalPageScroll nsScrollView value =
  sendMessage nsScrollView setVerticalPageScrollSelector value

-- | @- pageScroll@
pageScroll :: IsNSScrollView nsScrollView => nsScrollView -> IO CDouble
pageScroll nsScrollView =
  sendMessage nsScrollView pageScrollSelector

-- | @- setPageScroll:@
setPageScroll :: IsNSScrollView nsScrollView => nsScrollView -> CDouble -> IO ()
setPageScroll nsScrollView value =
  sendMessage nsScrollView setPageScrollSelector value

-- | @- scrollsDynamically@
scrollsDynamically :: IsNSScrollView nsScrollView => nsScrollView -> IO Bool
scrollsDynamically nsScrollView =
  sendMessage nsScrollView scrollsDynamicallySelector

-- | @- setScrollsDynamically:@
setScrollsDynamically :: IsNSScrollView nsScrollView => nsScrollView -> Bool -> IO ()
setScrollsDynamically nsScrollView value =
  sendMessage nsScrollView setScrollsDynamicallySelector value

-- | @- scrollerStyle@
scrollerStyle :: IsNSScrollView nsScrollView => nsScrollView -> IO NSScrollerStyle
scrollerStyle nsScrollView =
  sendMessage nsScrollView scrollerStyleSelector

-- | @- setScrollerStyle:@
setScrollerStyle :: IsNSScrollView nsScrollView => nsScrollView -> NSScrollerStyle -> IO ()
setScrollerStyle nsScrollView value =
  sendMessage nsScrollView setScrollerStyleSelector value

-- | @- scrollerKnobStyle@
scrollerKnobStyle :: IsNSScrollView nsScrollView => nsScrollView -> IO NSScrollerKnobStyle
scrollerKnobStyle nsScrollView =
  sendMessage nsScrollView scrollerKnobStyleSelector

-- | @- setScrollerKnobStyle:@
setScrollerKnobStyle :: IsNSScrollView nsScrollView => nsScrollView -> NSScrollerKnobStyle -> IO ()
setScrollerKnobStyle nsScrollView value =
  sendMessage nsScrollView setScrollerKnobStyleSelector value

-- | @- horizontalScrollElasticity@
horizontalScrollElasticity :: IsNSScrollView nsScrollView => nsScrollView -> IO NSScrollElasticity
horizontalScrollElasticity nsScrollView =
  sendMessage nsScrollView horizontalScrollElasticitySelector

-- | @- setHorizontalScrollElasticity:@
setHorizontalScrollElasticity :: IsNSScrollView nsScrollView => nsScrollView -> NSScrollElasticity -> IO ()
setHorizontalScrollElasticity nsScrollView value =
  sendMessage nsScrollView setHorizontalScrollElasticitySelector value

-- | @- verticalScrollElasticity@
verticalScrollElasticity :: IsNSScrollView nsScrollView => nsScrollView -> IO NSScrollElasticity
verticalScrollElasticity nsScrollView =
  sendMessage nsScrollView verticalScrollElasticitySelector

-- | @- setVerticalScrollElasticity:@
setVerticalScrollElasticity :: IsNSScrollView nsScrollView => nsScrollView -> NSScrollElasticity -> IO ()
setVerticalScrollElasticity nsScrollView value =
  sendMessage nsScrollView setVerticalScrollElasticitySelector value

-- | @- usesPredominantAxisScrolling@
usesPredominantAxisScrolling :: IsNSScrollView nsScrollView => nsScrollView -> IO Bool
usesPredominantAxisScrolling nsScrollView =
  sendMessage nsScrollView usesPredominantAxisScrollingSelector

-- | @- setUsesPredominantAxisScrolling:@
setUsesPredominantAxisScrolling :: IsNSScrollView nsScrollView => nsScrollView -> Bool -> IO ()
setUsesPredominantAxisScrolling nsScrollView value =
  sendMessage nsScrollView setUsesPredominantAxisScrollingSelector value

-- | @- allowsMagnification@
allowsMagnification :: IsNSScrollView nsScrollView => nsScrollView -> IO Bool
allowsMagnification nsScrollView =
  sendMessage nsScrollView allowsMagnificationSelector

-- | @- setAllowsMagnification:@
setAllowsMagnification :: IsNSScrollView nsScrollView => nsScrollView -> Bool -> IO ()
setAllowsMagnification nsScrollView value =
  sendMessage nsScrollView setAllowsMagnificationSelector value

-- | @- magnification@
magnification :: IsNSScrollView nsScrollView => nsScrollView -> IO CDouble
magnification nsScrollView =
  sendMessage nsScrollView magnificationSelector

-- | @- setMagnification:@
setMagnification :: IsNSScrollView nsScrollView => nsScrollView -> CDouble -> IO ()
setMagnification nsScrollView value =
  sendMessage nsScrollView setMagnificationSelector value

-- | @- maxMagnification@
maxMagnification :: IsNSScrollView nsScrollView => nsScrollView -> IO CDouble
maxMagnification nsScrollView =
  sendMessage nsScrollView maxMagnificationSelector

-- | @- setMaxMagnification:@
setMaxMagnification :: IsNSScrollView nsScrollView => nsScrollView -> CDouble -> IO ()
setMaxMagnification nsScrollView value =
  sendMessage nsScrollView setMaxMagnificationSelector value

-- | @- minMagnification@
minMagnification :: IsNSScrollView nsScrollView => nsScrollView -> IO CDouble
minMagnification nsScrollView =
  sendMessage nsScrollView minMagnificationSelector

-- | @- setMinMagnification:@
setMinMagnification :: IsNSScrollView nsScrollView => nsScrollView -> CDouble -> IO ()
setMinMagnification nsScrollView value =
  sendMessage nsScrollView setMinMagnificationSelector value

-- | @- automaticallyAdjustsContentInsets@
automaticallyAdjustsContentInsets :: IsNSScrollView nsScrollView => nsScrollView -> IO Bool
automaticallyAdjustsContentInsets nsScrollView =
  sendMessage nsScrollView automaticallyAdjustsContentInsetsSelector

-- | @- setAutomaticallyAdjustsContentInsets:@
setAutomaticallyAdjustsContentInsets :: IsNSScrollView nsScrollView => nsScrollView -> Bool -> IO ()
setAutomaticallyAdjustsContentInsets nsScrollView value =
  sendMessage nsScrollView setAutomaticallyAdjustsContentInsetsSelector value

-- | @- contentInsets@
contentInsets :: IsNSScrollView nsScrollView => nsScrollView -> IO NSEdgeInsets
contentInsets nsScrollView =
  sendMessage nsScrollView contentInsetsSelector

-- | @- setContentInsets:@
setContentInsets :: IsNSScrollView nsScrollView => nsScrollView -> NSEdgeInsets -> IO ()
setContentInsets nsScrollView value =
  sendMessage nsScrollView setContentInsetsSelector value

-- | @- scrollerInsets@
scrollerInsets :: IsNSScrollView nsScrollView => nsScrollView -> IO NSEdgeInsets
scrollerInsets nsScrollView =
  sendMessage nsScrollView scrollerInsetsSelector

-- | @- setScrollerInsets:@
setScrollerInsets :: IsNSScrollView nsScrollView => nsScrollView -> NSEdgeInsets -> IO ()
setScrollerInsets nsScrollView value =
  sendMessage nsScrollView setScrollerInsetsSelector value

-- | @- findBarPosition@
findBarPosition :: IsNSScrollView nsScrollView => nsScrollView -> IO NSScrollViewFindBarPosition
findBarPosition nsScrollView =
  sendMessage nsScrollView findBarPositionSelector

-- | @- setFindBarPosition:@
setFindBarPosition :: IsNSScrollView nsScrollView => nsScrollView -> NSScrollViewFindBarPosition -> IO ()
setFindBarPosition nsScrollView value =
  sendMessage nsScrollView setFindBarPositionSelector value

-- | @+ rulerViewClass@
rulerViewClass :: IO Class
rulerViewClass  =
  do
    cls' <- getRequiredClass "NSScrollView"
    sendClassMessage cls' rulerViewClassSelector

-- | @+ setRulerViewClass:@
setRulerViewClass :: Class -> IO ()
setRulerViewClass value =
  do
    cls' <- getRequiredClass "NSScrollView"
    sendClassMessage cls' setRulerViewClassSelector value

-- | @- rulersVisible@
rulersVisible :: IsNSScrollView nsScrollView => nsScrollView -> IO Bool
rulersVisible nsScrollView =
  sendMessage nsScrollView rulersVisibleSelector

-- | @- setRulersVisible:@
setRulersVisible :: IsNSScrollView nsScrollView => nsScrollView -> Bool -> IO ()
setRulersVisible nsScrollView value =
  sendMessage nsScrollView setRulersVisibleSelector value

-- | @- hasHorizontalRuler@
hasHorizontalRuler :: IsNSScrollView nsScrollView => nsScrollView -> IO Bool
hasHorizontalRuler nsScrollView =
  sendMessage nsScrollView hasHorizontalRulerSelector

-- | @- setHasHorizontalRuler:@
setHasHorizontalRuler :: IsNSScrollView nsScrollView => nsScrollView -> Bool -> IO ()
setHasHorizontalRuler nsScrollView value =
  sendMessage nsScrollView setHasHorizontalRulerSelector value

-- | @- hasVerticalRuler@
hasVerticalRuler :: IsNSScrollView nsScrollView => nsScrollView -> IO Bool
hasVerticalRuler nsScrollView =
  sendMessage nsScrollView hasVerticalRulerSelector

-- | @- setHasVerticalRuler:@
setHasVerticalRuler :: IsNSScrollView nsScrollView => nsScrollView -> Bool -> IO ()
setHasVerticalRuler nsScrollView value =
  sendMessage nsScrollView setHasVerticalRulerSelector value

-- | @- horizontalRulerView@
horizontalRulerView :: IsNSScrollView nsScrollView => nsScrollView -> IO (Id NSRulerView)
horizontalRulerView nsScrollView =
  sendMessage nsScrollView horizontalRulerViewSelector

-- | @- setHorizontalRulerView:@
setHorizontalRulerView :: (IsNSScrollView nsScrollView, IsNSRulerView value) => nsScrollView -> value -> IO ()
setHorizontalRulerView nsScrollView value =
  sendMessage nsScrollView setHorizontalRulerViewSelector (toNSRulerView value)

-- | @- verticalRulerView@
verticalRulerView :: IsNSScrollView nsScrollView => nsScrollView -> IO (Id NSRulerView)
verticalRulerView nsScrollView =
  sendMessage nsScrollView verticalRulerViewSelector

-- | @- setVerticalRulerView:@
setVerticalRulerView :: (IsNSScrollView nsScrollView, IsNSRulerView value) => nsScrollView -> value -> IO ()
setVerticalRulerView nsScrollView value =
  sendMessage nsScrollView setVerticalRulerViewSelector (toNSRulerView value)

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @initWithFrame:@
initWithFrameSelector :: Selector '[NSRect] (Id NSScrollView)
initWithFrameSelector = mkSelector "initWithFrame:"

-- | @Selector@ for @initWithCoder:@
initWithCoderSelector :: Selector '[Id NSCoder] (Id NSScrollView)
initWithCoderSelector = mkSelector "initWithCoder:"

-- | @Selector@ for @frameSizeForContentSize:horizontalScrollerClass:verticalScrollerClass:borderType:controlSize:scrollerStyle:@
frameSizeForContentSize_horizontalScrollerClass_verticalScrollerClass_borderType_controlSize_scrollerStyleSelector :: Selector '[NSSize, Class, Class, NSBorderType, NSControlSize, NSScrollerStyle] NSSize
frameSizeForContentSize_horizontalScrollerClass_verticalScrollerClass_borderType_controlSize_scrollerStyleSelector = mkSelector "frameSizeForContentSize:horizontalScrollerClass:verticalScrollerClass:borderType:controlSize:scrollerStyle:"

-- | @Selector@ for @contentSizeForFrameSize:horizontalScrollerClass:verticalScrollerClass:borderType:controlSize:scrollerStyle:@
contentSizeForFrameSize_horizontalScrollerClass_verticalScrollerClass_borderType_controlSize_scrollerStyleSelector :: Selector '[NSSize, Class, Class, NSBorderType, NSControlSize, NSScrollerStyle] NSSize
contentSizeForFrameSize_horizontalScrollerClass_verticalScrollerClass_borderType_controlSize_scrollerStyleSelector = mkSelector "contentSizeForFrameSize:horizontalScrollerClass:verticalScrollerClass:borderType:controlSize:scrollerStyle:"

-- | @Selector@ for @frameSizeForContentSize:hasHorizontalScroller:hasVerticalScroller:borderType:@
frameSizeForContentSize_hasHorizontalScroller_hasVerticalScroller_borderTypeSelector :: Selector '[NSSize, Bool, Bool, NSBorderType] NSSize
frameSizeForContentSize_hasHorizontalScroller_hasVerticalScroller_borderTypeSelector = mkSelector "frameSizeForContentSize:hasHorizontalScroller:hasVerticalScroller:borderType:"

-- | @Selector@ for @contentSizeForFrameSize:hasHorizontalScroller:hasVerticalScroller:borderType:@
contentSizeForFrameSize_hasHorizontalScroller_hasVerticalScroller_borderTypeSelector :: Selector '[NSSize, Bool, Bool, NSBorderType] NSSize
contentSizeForFrameSize_hasHorizontalScroller_hasVerticalScroller_borderTypeSelector = mkSelector "contentSizeForFrameSize:hasHorizontalScroller:hasVerticalScroller:borderType:"

-- | @Selector@ for @tile@
tileSelector :: Selector '[] ()
tileSelector = mkSelector "tile"

-- | @Selector@ for @reflectScrolledClipView:@
reflectScrolledClipViewSelector :: Selector '[Id NSClipView] ()
reflectScrolledClipViewSelector = mkSelector "reflectScrolledClipView:"

-- | @Selector@ for @scrollWheel:@
scrollWheelSelector :: Selector '[Id NSEvent] ()
scrollWheelSelector = mkSelector "scrollWheel:"

-- | @Selector@ for @flashScrollers@
flashScrollersSelector :: Selector '[] ()
flashScrollersSelector = mkSelector "flashScrollers"

-- | @Selector@ for @magnifyToFitRect:@
magnifyToFitRectSelector :: Selector '[NSRect] ()
magnifyToFitRectSelector = mkSelector "magnifyToFitRect:"

-- | @Selector@ for @setMagnification:centeredAtPoint:@
setMagnification_centeredAtPointSelector :: Selector '[CDouble, NSPoint] ()
setMagnification_centeredAtPointSelector = mkSelector "setMagnification:centeredAtPoint:"

-- | @Selector@ for @addFloatingSubview:forAxis:@
addFloatingSubview_forAxisSelector :: Selector '[Id NSView, NSEventGestureAxis] ()
addFloatingSubview_forAxisSelector = mkSelector "addFloatingSubview:forAxis:"

-- | @Selector@ for @documentVisibleRect@
documentVisibleRectSelector :: Selector '[] NSRect
documentVisibleRectSelector = mkSelector "documentVisibleRect"

-- | @Selector@ for @contentSize@
contentSizeSelector :: Selector '[] NSSize
contentSizeSelector = mkSelector "contentSize"

-- | @Selector@ for @documentView@
documentViewSelector :: Selector '[] (Id NSView)
documentViewSelector = mkSelector "documentView"

-- | @Selector@ for @setDocumentView:@
setDocumentViewSelector :: Selector '[Id NSView] ()
setDocumentViewSelector = mkSelector "setDocumentView:"

-- | @Selector@ for @contentView@
contentViewSelector :: Selector '[] (Id NSClipView)
contentViewSelector = mkSelector "contentView"

-- | @Selector@ for @setContentView:@
setContentViewSelector :: Selector '[Id NSClipView] ()
setContentViewSelector = mkSelector "setContentView:"

-- | @Selector@ for @documentCursor@
documentCursorSelector :: Selector '[] (Id NSCursor)
documentCursorSelector = mkSelector "documentCursor"

-- | @Selector@ for @setDocumentCursor:@
setDocumentCursorSelector :: Selector '[Id NSCursor] ()
setDocumentCursorSelector = mkSelector "setDocumentCursor:"

-- | @Selector@ for @borderType@
borderTypeSelector :: Selector '[] NSBorderType
borderTypeSelector = mkSelector "borderType"

-- | @Selector@ for @setBorderType:@
setBorderTypeSelector :: Selector '[NSBorderType] ()
setBorderTypeSelector = mkSelector "setBorderType:"

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

-- | @Selector@ for @hasVerticalScroller@
hasVerticalScrollerSelector :: Selector '[] Bool
hasVerticalScrollerSelector = mkSelector "hasVerticalScroller"

-- | @Selector@ for @setHasVerticalScroller:@
setHasVerticalScrollerSelector :: Selector '[Bool] ()
setHasVerticalScrollerSelector = mkSelector "setHasVerticalScroller:"

-- | @Selector@ for @hasHorizontalScroller@
hasHorizontalScrollerSelector :: Selector '[] Bool
hasHorizontalScrollerSelector = mkSelector "hasHorizontalScroller"

-- | @Selector@ for @setHasHorizontalScroller:@
setHasHorizontalScrollerSelector :: Selector '[Bool] ()
setHasHorizontalScrollerSelector = mkSelector "setHasHorizontalScroller:"

-- | @Selector@ for @verticalScroller@
verticalScrollerSelector :: Selector '[] (Id NSScroller)
verticalScrollerSelector = mkSelector "verticalScroller"

-- | @Selector@ for @setVerticalScroller:@
setVerticalScrollerSelector :: Selector '[Id NSScroller] ()
setVerticalScrollerSelector = mkSelector "setVerticalScroller:"

-- | @Selector@ for @horizontalScroller@
horizontalScrollerSelector :: Selector '[] (Id NSScroller)
horizontalScrollerSelector = mkSelector "horizontalScroller"

-- | @Selector@ for @setHorizontalScroller:@
setHorizontalScrollerSelector :: Selector '[Id NSScroller] ()
setHorizontalScrollerSelector = mkSelector "setHorizontalScroller:"

-- | @Selector@ for @autohidesScrollers@
autohidesScrollersSelector :: Selector '[] Bool
autohidesScrollersSelector = mkSelector "autohidesScrollers"

-- | @Selector@ for @setAutohidesScrollers:@
setAutohidesScrollersSelector :: Selector '[Bool] ()
setAutohidesScrollersSelector = mkSelector "setAutohidesScrollers:"

-- | @Selector@ for @horizontalLineScroll@
horizontalLineScrollSelector :: Selector '[] CDouble
horizontalLineScrollSelector = mkSelector "horizontalLineScroll"

-- | @Selector@ for @setHorizontalLineScroll:@
setHorizontalLineScrollSelector :: Selector '[CDouble] ()
setHorizontalLineScrollSelector = mkSelector "setHorizontalLineScroll:"

-- | @Selector@ for @verticalLineScroll@
verticalLineScrollSelector :: Selector '[] CDouble
verticalLineScrollSelector = mkSelector "verticalLineScroll"

-- | @Selector@ for @setVerticalLineScroll:@
setVerticalLineScrollSelector :: Selector '[CDouble] ()
setVerticalLineScrollSelector = mkSelector "setVerticalLineScroll:"

-- | @Selector@ for @lineScroll@
lineScrollSelector :: Selector '[] CDouble
lineScrollSelector = mkSelector "lineScroll"

-- | @Selector@ for @setLineScroll:@
setLineScrollSelector :: Selector '[CDouble] ()
setLineScrollSelector = mkSelector "setLineScroll:"

-- | @Selector@ for @horizontalPageScroll@
horizontalPageScrollSelector :: Selector '[] CDouble
horizontalPageScrollSelector = mkSelector "horizontalPageScroll"

-- | @Selector@ for @setHorizontalPageScroll:@
setHorizontalPageScrollSelector :: Selector '[CDouble] ()
setHorizontalPageScrollSelector = mkSelector "setHorizontalPageScroll:"

-- | @Selector@ for @verticalPageScroll@
verticalPageScrollSelector :: Selector '[] CDouble
verticalPageScrollSelector = mkSelector "verticalPageScroll"

-- | @Selector@ for @setVerticalPageScroll:@
setVerticalPageScrollSelector :: Selector '[CDouble] ()
setVerticalPageScrollSelector = mkSelector "setVerticalPageScroll:"

-- | @Selector@ for @pageScroll@
pageScrollSelector :: Selector '[] CDouble
pageScrollSelector = mkSelector "pageScroll"

-- | @Selector@ for @setPageScroll:@
setPageScrollSelector :: Selector '[CDouble] ()
setPageScrollSelector = mkSelector "setPageScroll:"

-- | @Selector@ for @scrollsDynamically@
scrollsDynamicallySelector :: Selector '[] Bool
scrollsDynamicallySelector = mkSelector "scrollsDynamically"

-- | @Selector@ for @setScrollsDynamically:@
setScrollsDynamicallySelector :: Selector '[Bool] ()
setScrollsDynamicallySelector = mkSelector "setScrollsDynamically:"

-- | @Selector@ for @scrollerStyle@
scrollerStyleSelector :: Selector '[] NSScrollerStyle
scrollerStyleSelector = mkSelector "scrollerStyle"

-- | @Selector@ for @setScrollerStyle:@
setScrollerStyleSelector :: Selector '[NSScrollerStyle] ()
setScrollerStyleSelector = mkSelector "setScrollerStyle:"

-- | @Selector@ for @scrollerKnobStyle@
scrollerKnobStyleSelector :: Selector '[] NSScrollerKnobStyle
scrollerKnobStyleSelector = mkSelector "scrollerKnobStyle"

-- | @Selector@ for @setScrollerKnobStyle:@
setScrollerKnobStyleSelector :: Selector '[NSScrollerKnobStyle] ()
setScrollerKnobStyleSelector = mkSelector "setScrollerKnobStyle:"

-- | @Selector@ for @horizontalScrollElasticity@
horizontalScrollElasticitySelector :: Selector '[] NSScrollElasticity
horizontalScrollElasticitySelector = mkSelector "horizontalScrollElasticity"

-- | @Selector@ for @setHorizontalScrollElasticity:@
setHorizontalScrollElasticitySelector :: Selector '[NSScrollElasticity] ()
setHorizontalScrollElasticitySelector = mkSelector "setHorizontalScrollElasticity:"

-- | @Selector@ for @verticalScrollElasticity@
verticalScrollElasticitySelector :: Selector '[] NSScrollElasticity
verticalScrollElasticitySelector = mkSelector "verticalScrollElasticity"

-- | @Selector@ for @setVerticalScrollElasticity:@
setVerticalScrollElasticitySelector :: Selector '[NSScrollElasticity] ()
setVerticalScrollElasticitySelector = mkSelector "setVerticalScrollElasticity:"

-- | @Selector@ for @usesPredominantAxisScrolling@
usesPredominantAxisScrollingSelector :: Selector '[] Bool
usesPredominantAxisScrollingSelector = mkSelector "usesPredominantAxisScrolling"

-- | @Selector@ for @setUsesPredominantAxisScrolling:@
setUsesPredominantAxisScrollingSelector :: Selector '[Bool] ()
setUsesPredominantAxisScrollingSelector = mkSelector "setUsesPredominantAxisScrolling:"

-- | @Selector@ for @allowsMagnification@
allowsMagnificationSelector :: Selector '[] Bool
allowsMagnificationSelector = mkSelector "allowsMagnification"

-- | @Selector@ for @setAllowsMagnification:@
setAllowsMagnificationSelector :: Selector '[Bool] ()
setAllowsMagnificationSelector = mkSelector "setAllowsMagnification:"

-- | @Selector@ for @magnification@
magnificationSelector :: Selector '[] CDouble
magnificationSelector = mkSelector "magnification"

-- | @Selector@ for @setMagnification:@
setMagnificationSelector :: Selector '[CDouble] ()
setMagnificationSelector = mkSelector "setMagnification:"

-- | @Selector@ for @maxMagnification@
maxMagnificationSelector :: Selector '[] CDouble
maxMagnificationSelector = mkSelector "maxMagnification"

-- | @Selector@ for @setMaxMagnification:@
setMaxMagnificationSelector :: Selector '[CDouble] ()
setMaxMagnificationSelector = mkSelector "setMaxMagnification:"

-- | @Selector@ for @minMagnification@
minMagnificationSelector :: Selector '[] CDouble
minMagnificationSelector = mkSelector "minMagnification"

-- | @Selector@ for @setMinMagnification:@
setMinMagnificationSelector :: Selector '[CDouble] ()
setMinMagnificationSelector = mkSelector "setMinMagnification:"

-- | @Selector@ for @automaticallyAdjustsContentInsets@
automaticallyAdjustsContentInsetsSelector :: Selector '[] Bool
automaticallyAdjustsContentInsetsSelector = mkSelector "automaticallyAdjustsContentInsets"

-- | @Selector@ for @setAutomaticallyAdjustsContentInsets:@
setAutomaticallyAdjustsContentInsetsSelector :: Selector '[Bool] ()
setAutomaticallyAdjustsContentInsetsSelector = mkSelector "setAutomaticallyAdjustsContentInsets:"

-- | @Selector@ for @contentInsets@
contentInsetsSelector :: Selector '[] NSEdgeInsets
contentInsetsSelector = mkSelector "contentInsets"

-- | @Selector@ for @setContentInsets:@
setContentInsetsSelector :: Selector '[NSEdgeInsets] ()
setContentInsetsSelector = mkSelector "setContentInsets:"

-- | @Selector@ for @scrollerInsets@
scrollerInsetsSelector :: Selector '[] NSEdgeInsets
scrollerInsetsSelector = mkSelector "scrollerInsets"

-- | @Selector@ for @setScrollerInsets:@
setScrollerInsetsSelector :: Selector '[NSEdgeInsets] ()
setScrollerInsetsSelector = mkSelector "setScrollerInsets:"

-- | @Selector@ for @findBarPosition@
findBarPositionSelector :: Selector '[] NSScrollViewFindBarPosition
findBarPositionSelector = mkSelector "findBarPosition"

-- | @Selector@ for @setFindBarPosition:@
setFindBarPositionSelector :: Selector '[NSScrollViewFindBarPosition] ()
setFindBarPositionSelector = mkSelector "setFindBarPosition:"

-- | @Selector@ for @rulerViewClass@
rulerViewClassSelector :: Selector '[] Class
rulerViewClassSelector = mkSelector "rulerViewClass"

-- | @Selector@ for @setRulerViewClass:@
setRulerViewClassSelector :: Selector '[Class] ()
setRulerViewClassSelector = mkSelector "setRulerViewClass:"

-- | @Selector@ for @rulersVisible@
rulersVisibleSelector :: Selector '[] Bool
rulersVisibleSelector = mkSelector "rulersVisible"

-- | @Selector@ for @setRulersVisible:@
setRulersVisibleSelector :: Selector '[Bool] ()
setRulersVisibleSelector = mkSelector "setRulersVisible:"

-- | @Selector@ for @hasHorizontalRuler@
hasHorizontalRulerSelector :: Selector '[] Bool
hasHorizontalRulerSelector = mkSelector "hasHorizontalRuler"

-- | @Selector@ for @setHasHorizontalRuler:@
setHasHorizontalRulerSelector :: Selector '[Bool] ()
setHasHorizontalRulerSelector = mkSelector "setHasHorizontalRuler:"

-- | @Selector@ for @hasVerticalRuler@
hasVerticalRulerSelector :: Selector '[] Bool
hasVerticalRulerSelector = mkSelector "hasVerticalRuler"

-- | @Selector@ for @setHasVerticalRuler:@
setHasVerticalRulerSelector :: Selector '[Bool] ()
setHasVerticalRulerSelector = mkSelector "setHasVerticalRuler:"

-- | @Selector@ for @horizontalRulerView@
horizontalRulerViewSelector :: Selector '[] (Id NSRulerView)
horizontalRulerViewSelector = mkSelector "horizontalRulerView"

-- | @Selector@ for @setHorizontalRulerView:@
setHorizontalRulerViewSelector :: Selector '[Id NSRulerView] ()
setHorizontalRulerViewSelector = mkSelector "setHorizontalRulerView:"

-- | @Selector@ for @verticalRulerView@
verticalRulerViewSelector :: Selector '[] (Id NSRulerView)
verticalRulerViewSelector = mkSelector "verticalRulerView"

-- | @Selector@ for @setVerticalRulerView:@
setVerticalRulerViewSelector :: Selector '[Id NSRulerView] ()
setVerticalRulerViewSelector = mkSelector "setVerticalRulerView:"

