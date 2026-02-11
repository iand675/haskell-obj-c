{-# LANGUAGE PatternSynonyms #-}
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
  , initWithFrameSelector
  , initWithCoderSelector
  , frameSizeForContentSize_horizontalScrollerClass_verticalScrollerClass_borderType_controlSize_scrollerStyleSelector
  , contentSizeForFrameSize_horizontalScrollerClass_verticalScrollerClass_borderType_controlSize_scrollerStyleSelector
  , frameSizeForContentSize_hasHorizontalScroller_hasVerticalScroller_borderTypeSelector
  , contentSizeForFrameSize_hasHorizontalScroller_hasVerticalScroller_borderTypeSelector
  , tileSelector
  , reflectScrolledClipViewSelector
  , scrollWheelSelector
  , flashScrollersSelector
  , magnifyToFitRectSelector
  , setMagnification_centeredAtPointSelector
  , addFloatingSubview_forAxisSelector
  , documentVisibleRectSelector
  , contentSizeSelector
  , documentViewSelector
  , setDocumentViewSelector
  , contentViewSelector
  , setContentViewSelector
  , documentCursorSelector
  , setDocumentCursorSelector
  , borderTypeSelector
  , setBorderTypeSelector
  , backgroundColorSelector
  , setBackgroundColorSelector
  , drawsBackgroundSelector
  , setDrawsBackgroundSelector
  , hasVerticalScrollerSelector
  , setHasVerticalScrollerSelector
  , hasHorizontalScrollerSelector
  , setHasHorizontalScrollerSelector
  , verticalScrollerSelector
  , setVerticalScrollerSelector
  , horizontalScrollerSelector
  , setHorizontalScrollerSelector
  , autohidesScrollersSelector
  , setAutohidesScrollersSelector
  , horizontalLineScrollSelector
  , setHorizontalLineScrollSelector
  , verticalLineScrollSelector
  , setVerticalLineScrollSelector
  , lineScrollSelector
  , setLineScrollSelector
  , horizontalPageScrollSelector
  , setHorizontalPageScrollSelector
  , verticalPageScrollSelector
  , setVerticalPageScrollSelector
  , pageScrollSelector
  , setPageScrollSelector
  , scrollsDynamicallySelector
  , setScrollsDynamicallySelector
  , scrollerStyleSelector
  , setScrollerStyleSelector
  , scrollerKnobStyleSelector
  , setScrollerKnobStyleSelector
  , horizontalScrollElasticitySelector
  , setHorizontalScrollElasticitySelector
  , verticalScrollElasticitySelector
  , setVerticalScrollElasticitySelector
  , usesPredominantAxisScrollingSelector
  , setUsesPredominantAxisScrollingSelector
  , allowsMagnificationSelector
  , setAllowsMagnificationSelector
  , magnificationSelector
  , setMagnificationSelector
  , maxMagnificationSelector
  , setMaxMagnificationSelector
  , minMagnificationSelector
  , setMinMagnificationSelector
  , automaticallyAdjustsContentInsetsSelector
  , setAutomaticallyAdjustsContentInsetsSelector
  , contentInsetsSelector
  , setContentInsetsSelector
  , scrollerInsetsSelector
  , setScrollerInsetsSelector
  , findBarPositionSelector
  , setFindBarPositionSelector
  , rulerViewClassSelector
  , setRulerViewClassSelector
  , rulersVisibleSelector
  , setRulersVisibleSelector
  , hasHorizontalRulerSelector
  , setHasHorizontalRulerSelector
  , hasVerticalRulerSelector
  , setHasVerticalRulerSelector
  , horizontalRulerViewSelector
  , setHorizontalRulerViewSelector
  , verticalRulerViewSelector
  , setVerticalRulerViewSelector

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
import ObjC.AppKit.Internal.Enums
import ObjC.Foundation.Internal.Classes

-- | @- initWithFrame:@
initWithFrame :: IsNSScrollView nsScrollView => nsScrollView -> NSRect -> IO (Id NSScrollView)
initWithFrame nsScrollView  frameRect =
  sendMsg nsScrollView (mkSelector "initWithFrame:") (retPtr retVoid) [argNSRect frameRect] >>= ownedObject . castPtr

-- | @- initWithCoder:@
initWithCoder :: (IsNSScrollView nsScrollView, IsNSCoder coder) => nsScrollView -> coder -> IO (Id NSScrollView)
initWithCoder nsScrollView  coder =
withObjCPtr coder $ \raw_coder ->
    sendMsg nsScrollView (mkSelector "initWithCoder:") (retPtr retVoid) [argPtr (castPtr raw_coder :: Ptr ())] >>= ownedObject . castPtr

-- | @+ frameSizeForContentSize:horizontalScrollerClass:verticalScrollerClass:borderType:controlSize:scrollerStyle:@
frameSizeForContentSize_horizontalScrollerClass_verticalScrollerClass_borderType_controlSize_scrollerStyle :: NSSize -> Class -> Class -> NSBorderType -> NSControlSize -> NSScrollerStyle -> IO NSSize
frameSizeForContentSize_horizontalScrollerClass_verticalScrollerClass_borderType_controlSize_scrollerStyle cSize horizontalScrollerClass verticalScrollerClass type_ controlSize scrollerStyle =
  do
    cls' <- getRequiredClass "NSScrollView"
    sendClassMsgStret cls' (mkSelector "frameSizeForContentSize:horizontalScrollerClass:verticalScrollerClass:borderType:controlSize:scrollerStyle:") retNSSize [argNSSize cSize, argPtr (unClass horizontalScrollerClass), argPtr (unClass verticalScrollerClass), argCULong (coerce type_), argCULong (coerce controlSize), argCLong (coerce scrollerStyle)]

-- | @+ contentSizeForFrameSize:horizontalScrollerClass:verticalScrollerClass:borderType:controlSize:scrollerStyle:@
contentSizeForFrameSize_horizontalScrollerClass_verticalScrollerClass_borderType_controlSize_scrollerStyle :: NSSize -> Class -> Class -> NSBorderType -> NSControlSize -> NSScrollerStyle -> IO NSSize
contentSizeForFrameSize_horizontalScrollerClass_verticalScrollerClass_borderType_controlSize_scrollerStyle fSize horizontalScrollerClass verticalScrollerClass type_ controlSize scrollerStyle =
  do
    cls' <- getRequiredClass "NSScrollView"
    sendClassMsgStret cls' (mkSelector "contentSizeForFrameSize:horizontalScrollerClass:verticalScrollerClass:borderType:controlSize:scrollerStyle:") retNSSize [argNSSize fSize, argPtr (unClass horizontalScrollerClass), argPtr (unClass verticalScrollerClass), argCULong (coerce type_), argCULong (coerce controlSize), argCLong (coerce scrollerStyle)]

-- | @+ frameSizeForContentSize:hasHorizontalScroller:hasVerticalScroller:borderType:@
frameSizeForContentSize_hasHorizontalScroller_hasVerticalScroller_borderType :: NSSize -> Bool -> Bool -> NSBorderType -> IO NSSize
frameSizeForContentSize_hasHorizontalScroller_hasVerticalScroller_borderType cSize hFlag vFlag type_ =
  do
    cls' <- getRequiredClass "NSScrollView"
    sendClassMsgStret cls' (mkSelector "frameSizeForContentSize:hasHorizontalScroller:hasVerticalScroller:borderType:") retNSSize [argNSSize cSize, argCULong (if hFlag then 1 else 0), argCULong (if vFlag then 1 else 0), argCULong (coerce type_)]

-- | @+ contentSizeForFrameSize:hasHorizontalScroller:hasVerticalScroller:borderType:@
contentSizeForFrameSize_hasHorizontalScroller_hasVerticalScroller_borderType :: NSSize -> Bool -> Bool -> NSBorderType -> IO NSSize
contentSizeForFrameSize_hasHorizontalScroller_hasVerticalScroller_borderType fSize hFlag vFlag type_ =
  do
    cls' <- getRequiredClass "NSScrollView"
    sendClassMsgStret cls' (mkSelector "contentSizeForFrameSize:hasHorizontalScroller:hasVerticalScroller:borderType:") retNSSize [argNSSize fSize, argCULong (if hFlag then 1 else 0), argCULong (if vFlag then 1 else 0), argCULong (coerce type_)]

-- | @- tile@
tile :: IsNSScrollView nsScrollView => nsScrollView -> IO ()
tile nsScrollView  =
  sendMsg nsScrollView (mkSelector "tile") retVoid []

-- | @- reflectScrolledClipView:@
reflectScrolledClipView :: (IsNSScrollView nsScrollView, IsNSClipView cView) => nsScrollView -> cView -> IO ()
reflectScrolledClipView nsScrollView  cView =
withObjCPtr cView $ \raw_cView ->
    sendMsg nsScrollView (mkSelector "reflectScrolledClipView:") retVoid [argPtr (castPtr raw_cView :: Ptr ())]

-- | @- scrollWheel:@
scrollWheel :: (IsNSScrollView nsScrollView, IsNSEvent event) => nsScrollView -> event -> IO ()
scrollWheel nsScrollView  event =
withObjCPtr event $ \raw_event ->
    sendMsg nsScrollView (mkSelector "scrollWheel:") retVoid [argPtr (castPtr raw_event :: Ptr ())]

-- | @- flashScrollers@
flashScrollers :: IsNSScrollView nsScrollView => nsScrollView -> IO ()
flashScrollers nsScrollView  =
  sendMsg nsScrollView (mkSelector "flashScrollers") retVoid []

-- | @- magnifyToFitRect:@
magnifyToFitRect :: IsNSScrollView nsScrollView => nsScrollView -> NSRect -> IO ()
magnifyToFitRect nsScrollView  rect =
  sendMsg nsScrollView (mkSelector "magnifyToFitRect:") retVoid [argNSRect rect]

-- | @- setMagnification:centeredAtPoint:@
setMagnification_centeredAtPoint :: IsNSScrollView nsScrollView => nsScrollView -> CDouble -> NSPoint -> IO ()
setMagnification_centeredAtPoint nsScrollView  magnification point =
  sendMsg nsScrollView (mkSelector "setMagnification:centeredAtPoint:") retVoid [argCDouble (fromIntegral magnification), argNSPoint point]

-- | @- addFloatingSubview:forAxis:@
addFloatingSubview_forAxis :: (IsNSScrollView nsScrollView, IsNSView view) => nsScrollView -> view -> NSEventGestureAxis -> IO ()
addFloatingSubview_forAxis nsScrollView  view axis =
withObjCPtr view $ \raw_view ->
    sendMsg nsScrollView (mkSelector "addFloatingSubview:forAxis:") retVoid [argPtr (castPtr raw_view :: Ptr ()), argCLong (coerce axis)]

-- | @- documentVisibleRect@
documentVisibleRect :: IsNSScrollView nsScrollView => nsScrollView -> IO NSRect
documentVisibleRect nsScrollView  =
  sendMsgStret nsScrollView (mkSelector "documentVisibleRect") retNSRect []

-- | @- contentSize@
contentSize :: IsNSScrollView nsScrollView => nsScrollView -> IO NSSize
contentSize nsScrollView  =
  sendMsgStret nsScrollView (mkSelector "contentSize") retNSSize []

-- | @- documentView@
documentView :: IsNSScrollView nsScrollView => nsScrollView -> IO (Id NSView)
documentView nsScrollView  =
  sendMsg nsScrollView (mkSelector "documentView") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- setDocumentView:@
setDocumentView :: (IsNSScrollView nsScrollView, IsNSView value) => nsScrollView -> value -> IO ()
setDocumentView nsScrollView  value =
withObjCPtr value $ \raw_value ->
    sendMsg nsScrollView (mkSelector "setDocumentView:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- | @- contentView@
contentView :: IsNSScrollView nsScrollView => nsScrollView -> IO (Id NSClipView)
contentView nsScrollView  =
  sendMsg nsScrollView (mkSelector "contentView") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- setContentView:@
setContentView :: (IsNSScrollView nsScrollView, IsNSClipView value) => nsScrollView -> value -> IO ()
setContentView nsScrollView  value =
withObjCPtr value $ \raw_value ->
    sendMsg nsScrollView (mkSelector "setContentView:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- | @- documentCursor@
documentCursor :: IsNSScrollView nsScrollView => nsScrollView -> IO (Id NSCursor)
documentCursor nsScrollView  =
  sendMsg nsScrollView (mkSelector "documentCursor") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- setDocumentCursor:@
setDocumentCursor :: (IsNSScrollView nsScrollView, IsNSCursor value) => nsScrollView -> value -> IO ()
setDocumentCursor nsScrollView  value =
withObjCPtr value $ \raw_value ->
    sendMsg nsScrollView (mkSelector "setDocumentCursor:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- | @- borderType@
borderType :: IsNSScrollView nsScrollView => nsScrollView -> IO NSBorderType
borderType nsScrollView  =
  fmap (coerce :: CULong -> NSBorderType) $ sendMsg nsScrollView (mkSelector "borderType") retCULong []

-- | @- setBorderType:@
setBorderType :: IsNSScrollView nsScrollView => nsScrollView -> NSBorderType -> IO ()
setBorderType nsScrollView  value =
  sendMsg nsScrollView (mkSelector "setBorderType:") retVoid [argCULong (coerce value)]

-- | @- backgroundColor@
backgroundColor :: IsNSScrollView nsScrollView => nsScrollView -> IO (Id NSColor)
backgroundColor nsScrollView  =
  sendMsg nsScrollView (mkSelector "backgroundColor") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- setBackgroundColor:@
setBackgroundColor :: (IsNSScrollView nsScrollView, IsNSColor value) => nsScrollView -> value -> IO ()
setBackgroundColor nsScrollView  value =
withObjCPtr value $ \raw_value ->
    sendMsg nsScrollView (mkSelector "setBackgroundColor:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- | @- drawsBackground@
drawsBackground :: IsNSScrollView nsScrollView => nsScrollView -> IO Bool
drawsBackground nsScrollView  =
  fmap ((/= 0) :: CULong -> Bool) $ sendMsg nsScrollView (mkSelector "drawsBackground") retCULong []

-- | @- setDrawsBackground:@
setDrawsBackground :: IsNSScrollView nsScrollView => nsScrollView -> Bool -> IO ()
setDrawsBackground nsScrollView  value =
  sendMsg nsScrollView (mkSelector "setDrawsBackground:") retVoid [argCULong (if value then 1 else 0)]

-- | @- hasVerticalScroller@
hasVerticalScroller :: IsNSScrollView nsScrollView => nsScrollView -> IO Bool
hasVerticalScroller nsScrollView  =
  fmap ((/= 0) :: CULong -> Bool) $ sendMsg nsScrollView (mkSelector "hasVerticalScroller") retCULong []

-- | @- setHasVerticalScroller:@
setHasVerticalScroller :: IsNSScrollView nsScrollView => nsScrollView -> Bool -> IO ()
setHasVerticalScroller nsScrollView  value =
  sendMsg nsScrollView (mkSelector "setHasVerticalScroller:") retVoid [argCULong (if value then 1 else 0)]

-- | @- hasHorizontalScroller@
hasHorizontalScroller :: IsNSScrollView nsScrollView => nsScrollView -> IO Bool
hasHorizontalScroller nsScrollView  =
  fmap ((/= 0) :: CULong -> Bool) $ sendMsg nsScrollView (mkSelector "hasHorizontalScroller") retCULong []

-- | @- setHasHorizontalScroller:@
setHasHorizontalScroller :: IsNSScrollView nsScrollView => nsScrollView -> Bool -> IO ()
setHasHorizontalScroller nsScrollView  value =
  sendMsg nsScrollView (mkSelector "setHasHorizontalScroller:") retVoid [argCULong (if value then 1 else 0)]

-- | @- verticalScroller@
verticalScroller :: IsNSScrollView nsScrollView => nsScrollView -> IO (Id NSScroller)
verticalScroller nsScrollView  =
  sendMsg nsScrollView (mkSelector "verticalScroller") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- setVerticalScroller:@
setVerticalScroller :: (IsNSScrollView nsScrollView, IsNSScroller value) => nsScrollView -> value -> IO ()
setVerticalScroller nsScrollView  value =
withObjCPtr value $ \raw_value ->
    sendMsg nsScrollView (mkSelector "setVerticalScroller:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- | @- horizontalScroller@
horizontalScroller :: IsNSScrollView nsScrollView => nsScrollView -> IO (Id NSScroller)
horizontalScroller nsScrollView  =
  sendMsg nsScrollView (mkSelector "horizontalScroller") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- setHorizontalScroller:@
setHorizontalScroller :: (IsNSScrollView nsScrollView, IsNSScroller value) => nsScrollView -> value -> IO ()
setHorizontalScroller nsScrollView  value =
withObjCPtr value $ \raw_value ->
    sendMsg nsScrollView (mkSelector "setHorizontalScroller:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- | @- autohidesScrollers@
autohidesScrollers :: IsNSScrollView nsScrollView => nsScrollView -> IO Bool
autohidesScrollers nsScrollView  =
  fmap ((/= 0) :: CULong -> Bool) $ sendMsg nsScrollView (mkSelector "autohidesScrollers") retCULong []

-- | @- setAutohidesScrollers:@
setAutohidesScrollers :: IsNSScrollView nsScrollView => nsScrollView -> Bool -> IO ()
setAutohidesScrollers nsScrollView  value =
  sendMsg nsScrollView (mkSelector "setAutohidesScrollers:") retVoid [argCULong (if value then 1 else 0)]

-- | @- horizontalLineScroll@
horizontalLineScroll :: IsNSScrollView nsScrollView => nsScrollView -> IO CDouble
horizontalLineScroll nsScrollView  =
  sendMsg nsScrollView (mkSelector "horizontalLineScroll") retCDouble []

-- | @- setHorizontalLineScroll:@
setHorizontalLineScroll :: IsNSScrollView nsScrollView => nsScrollView -> CDouble -> IO ()
setHorizontalLineScroll nsScrollView  value =
  sendMsg nsScrollView (mkSelector "setHorizontalLineScroll:") retVoid [argCDouble (fromIntegral value)]

-- | @- verticalLineScroll@
verticalLineScroll :: IsNSScrollView nsScrollView => nsScrollView -> IO CDouble
verticalLineScroll nsScrollView  =
  sendMsg nsScrollView (mkSelector "verticalLineScroll") retCDouble []

-- | @- setVerticalLineScroll:@
setVerticalLineScroll :: IsNSScrollView nsScrollView => nsScrollView -> CDouble -> IO ()
setVerticalLineScroll nsScrollView  value =
  sendMsg nsScrollView (mkSelector "setVerticalLineScroll:") retVoid [argCDouble (fromIntegral value)]

-- | @- lineScroll@
lineScroll :: IsNSScrollView nsScrollView => nsScrollView -> IO CDouble
lineScroll nsScrollView  =
  sendMsg nsScrollView (mkSelector "lineScroll") retCDouble []

-- | @- setLineScroll:@
setLineScroll :: IsNSScrollView nsScrollView => nsScrollView -> CDouble -> IO ()
setLineScroll nsScrollView  value =
  sendMsg nsScrollView (mkSelector "setLineScroll:") retVoid [argCDouble (fromIntegral value)]

-- | @- horizontalPageScroll@
horizontalPageScroll :: IsNSScrollView nsScrollView => nsScrollView -> IO CDouble
horizontalPageScroll nsScrollView  =
  sendMsg nsScrollView (mkSelector "horizontalPageScroll") retCDouble []

-- | @- setHorizontalPageScroll:@
setHorizontalPageScroll :: IsNSScrollView nsScrollView => nsScrollView -> CDouble -> IO ()
setHorizontalPageScroll nsScrollView  value =
  sendMsg nsScrollView (mkSelector "setHorizontalPageScroll:") retVoid [argCDouble (fromIntegral value)]

-- | @- verticalPageScroll@
verticalPageScroll :: IsNSScrollView nsScrollView => nsScrollView -> IO CDouble
verticalPageScroll nsScrollView  =
  sendMsg nsScrollView (mkSelector "verticalPageScroll") retCDouble []

-- | @- setVerticalPageScroll:@
setVerticalPageScroll :: IsNSScrollView nsScrollView => nsScrollView -> CDouble -> IO ()
setVerticalPageScroll nsScrollView  value =
  sendMsg nsScrollView (mkSelector "setVerticalPageScroll:") retVoid [argCDouble (fromIntegral value)]

-- | @- pageScroll@
pageScroll :: IsNSScrollView nsScrollView => nsScrollView -> IO CDouble
pageScroll nsScrollView  =
  sendMsg nsScrollView (mkSelector "pageScroll") retCDouble []

-- | @- setPageScroll:@
setPageScroll :: IsNSScrollView nsScrollView => nsScrollView -> CDouble -> IO ()
setPageScroll nsScrollView  value =
  sendMsg nsScrollView (mkSelector "setPageScroll:") retVoid [argCDouble (fromIntegral value)]

-- | @- scrollsDynamically@
scrollsDynamically :: IsNSScrollView nsScrollView => nsScrollView -> IO Bool
scrollsDynamically nsScrollView  =
  fmap ((/= 0) :: CULong -> Bool) $ sendMsg nsScrollView (mkSelector "scrollsDynamically") retCULong []

-- | @- setScrollsDynamically:@
setScrollsDynamically :: IsNSScrollView nsScrollView => nsScrollView -> Bool -> IO ()
setScrollsDynamically nsScrollView  value =
  sendMsg nsScrollView (mkSelector "setScrollsDynamically:") retVoid [argCULong (if value then 1 else 0)]

-- | @- scrollerStyle@
scrollerStyle :: IsNSScrollView nsScrollView => nsScrollView -> IO NSScrollerStyle
scrollerStyle nsScrollView  =
  fmap (coerce :: CLong -> NSScrollerStyle) $ sendMsg nsScrollView (mkSelector "scrollerStyle") retCLong []

-- | @- setScrollerStyle:@
setScrollerStyle :: IsNSScrollView nsScrollView => nsScrollView -> NSScrollerStyle -> IO ()
setScrollerStyle nsScrollView  value =
  sendMsg nsScrollView (mkSelector "setScrollerStyle:") retVoid [argCLong (coerce value)]

-- | @- scrollerKnobStyle@
scrollerKnobStyle :: IsNSScrollView nsScrollView => nsScrollView -> IO NSScrollerKnobStyle
scrollerKnobStyle nsScrollView  =
  fmap (coerce :: CLong -> NSScrollerKnobStyle) $ sendMsg nsScrollView (mkSelector "scrollerKnobStyle") retCLong []

-- | @- setScrollerKnobStyle:@
setScrollerKnobStyle :: IsNSScrollView nsScrollView => nsScrollView -> NSScrollerKnobStyle -> IO ()
setScrollerKnobStyle nsScrollView  value =
  sendMsg nsScrollView (mkSelector "setScrollerKnobStyle:") retVoid [argCLong (coerce value)]

-- | @- horizontalScrollElasticity@
horizontalScrollElasticity :: IsNSScrollView nsScrollView => nsScrollView -> IO NSScrollElasticity
horizontalScrollElasticity nsScrollView  =
  fmap (coerce :: CLong -> NSScrollElasticity) $ sendMsg nsScrollView (mkSelector "horizontalScrollElasticity") retCLong []

-- | @- setHorizontalScrollElasticity:@
setHorizontalScrollElasticity :: IsNSScrollView nsScrollView => nsScrollView -> NSScrollElasticity -> IO ()
setHorizontalScrollElasticity nsScrollView  value =
  sendMsg nsScrollView (mkSelector "setHorizontalScrollElasticity:") retVoid [argCLong (coerce value)]

-- | @- verticalScrollElasticity@
verticalScrollElasticity :: IsNSScrollView nsScrollView => nsScrollView -> IO NSScrollElasticity
verticalScrollElasticity nsScrollView  =
  fmap (coerce :: CLong -> NSScrollElasticity) $ sendMsg nsScrollView (mkSelector "verticalScrollElasticity") retCLong []

-- | @- setVerticalScrollElasticity:@
setVerticalScrollElasticity :: IsNSScrollView nsScrollView => nsScrollView -> NSScrollElasticity -> IO ()
setVerticalScrollElasticity nsScrollView  value =
  sendMsg nsScrollView (mkSelector "setVerticalScrollElasticity:") retVoid [argCLong (coerce value)]

-- | @- usesPredominantAxisScrolling@
usesPredominantAxisScrolling :: IsNSScrollView nsScrollView => nsScrollView -> IO Bool
usesPredominantAxisScrolling nsScrollView  =
  fmap ((/= 0) :: CULong -> Bool) $ sendMsg nsScrollView (mkSelector "usesPredominantAxisScrolling") retCULong []

-- | @- setUsesPredominantAxisScrolling:@
setUsesPredominantAxisScrolling :: IsNSScrollView nsScrollView => nsScrollView -> Bool -> IO ()
setUsesPredominantAxisScrolling nsScrollView  value =
  sendMsg nsScrollView (mkSelector "setUsesPredominantAxisScrolling:") retVoid [argCULong (if value then 1 else 0)]

-- | @- allowsMagnification@
allowsMagnification :: IsNSScrollView nsScrollView => nsScrollView -> IO Bool
allowsMagnification nsScrollView  =
  fmap ((/= 0) :: CULong -> Bool) $ sendMsg nsScrollView (mkSelector "allowsMagnification") retCULong []

-- | @- setAllowsMagnification:@
setAllowsMagnification :: IsNSScrollView nsScrollView => nsScrollView -> Bool -> IO ()
setAllowsMagnification nsScrollView  value =
  sendMsg nsScrollView (mkSelector "setAllowsMagnification:") retVoid [argCULong (if value then 1 else 0)]

-- | @- magnification@
magnification :: IsNSScrollView nsScrollView => nsScrollView -> IO CDouble
magnification nsScrollView  =
  sendMsg nsScrollView (mkSelector "magnification") retCDouble []

-- | @- setMagnification:@
setMagnification :: IsNSScrollView nsScrollView => nsScrollView -> CDouble -> IO ()
setMagnification nsScrollView  value =
  sendMsg nsScrollView (mkSelector "setMagnification:") retVoid [argCDouble (fromIntegral value)]

-- | @- maxMagnification@
maxMagnification :: IsNSScrollView nsScrollView => nsScrollView -> IO CDouble
maxMagnification nsScrollView  =
  sendMsg nsScrollView (mkSelector "maxMagnification") retCDouble []

-- | @- setMaxMagnification:@
setMaxMagnification :: IsNSScrollView nsScrollView => nsScrollView -> CDouble -> IO ()
setMaxMagnification nsScrollView  value =
  sendMsg nsScrollView (mkSelector "setMaxMagnification:") retVoid [argCDouble (fromIntegral value)]

-- | @- minMagnification@
minMagnification :: IsNSScrollView nsScrollView => nsScrollView -> IO CDouble
minMagnification nsScrollView  =
  sendMsg nsScrollView (mkSelector "minMagnification") retCDouble []

-- | @- setMinMagnification:@
setMinMagnification :: IsNSScrollView nsScrollView => nsScrollView -> CDouble -> IO ()
setMinMagnification nsScrollView  value =
  sendMsg nsScrollView (mkSelector "setMinMagnification:") retVoid [argCDouble (fromIntegral value)]

-- | @- automaticallyAdjustsContentInsets@
automaticallyAdjustsContentInsets :: IsNSScrollView nsScrollView => nsScrollView -> IO Bool
automaticallyAdjustsContentInsets nsScrollView  =
  fmap ((/= 0) :: CULong -> Bool) $ sendMsg nsScrollView (mkSelector "automaticallyAdjustsContentInsets") retCULong []

-- | @- setAutomaticallyAdjustsContentInsets:@
setAutomaticallyAdjustsContentInsets :: IsNSScrollView nsScrollView => nsScrollView -> Bool -> IO ()
setAutomaticallyAdjustsContentInsets nsScrollView  value =
  sendMsg nsScrollView (mkSelector "setAutomaticallyAdjustsContentInsets:") retVoid [argCULong (if value then 1 else 0)]

-- | @- contentInsets@
contentInsets :: IsNSScrollView nsScrollView => nsScrollView -> IO NSEdgeInsets
contentInsets nsScrollView  =
  sendMsgStret nsScrollView (mkSelector "contentInsets") retNSEdgeInsets []

-- | @- setContentInsets:@
setContentInsets :: IsNSScrollView nsScrollView => nsScrollView -> NSEdgeInsets -> IO ()
setContentInsets nsScrollView  value =
  sendMsg nsScrollView (mkSelector "setContentInsets:") retVoid [argNSEdgeInsets value]

-- | @- scrollerInsets@
scrollerInsets :: IsNSScrollView nsScrollView => nsScrollView -> IO NSEdgeInsets
scrollerInsets nsScrollView  =
  sendMsgStret nsScrollView (mkSelector "scrollerInsets") retNSEdgeInsets []

-- | @- setScrollerInsets:@
setScrollerInsets :: IsNSScrollView nsScrollView => nsScrollView -> NSEdgeInsets -> IO ()
setScrollerInsets nsScrollView  value =
  sendMsg nsScrollView (mkSelector "setScrollerInsets:") retVoid [argNSEdgeInsets value]

-- | @- findBarPosition@
findBarPosition :: IsNSScrollView nsScrollView => nsScrollView -> IO NSScrollViewFindBarPosition
findBarPosition nsScrollView  =
  fmap (coerce :: CLong -> NSScrollViewFindBarPosition) $ sendMsg nsScrollView (mkSelector "findBarPosition") retCLong []

-- | @- setFindBarPosition:@
setFindBarPosition :: IsNSScrollView nsScrollView => nsScrollView -> NSScrollViewFindBarPosition -> IO ()
setFindBarPosition nsScrollView  value =
  sendMsg nsScrollView (mkSelector "setFindBarPosition:") retVoid [argCLong (coerce value)]

-- | @+ rulerViewClass@
rulerViewClass :: IO Class
rulerViewClass  =
  do
    cls' <- getRequiredClass "NSScrollView"
    fmap (Class . castPtr) $ sendClassMsg cls' (mkSelector "rulerViewClass") (retPtr retVoid) []

-- | @+ setRulerViewClass:@
setRulerViewClass :: Class -> IO ()
setRulerViewClass value =
  do
    cls' <- getRequiredClass "NSScrollView"
    sendClassMsg cls' (mkSelector "setRulerViewClass:") retVoid [argPtr (unClass value)]

-- | @- rulersVisible@
rulersVisible :: IsNSScrollView nsScrollView => nsScrollView -> IO Bool
rulersVisible nsScrollView  =
  fmap ((/= 0) :: CULong -> Bool) $ sendMsg nsScrollView (mkSelector "rulersVisible") retCULong []

-- | @- setRulersVisible:@
setRulersVisible :: IsNSScrollView nsScrollView => nsScrollView -> Bool -> IO ()
setRulersVisible nsScrollView  value =
  sendMsg nsScrollView (mkSelector "setRulersVisible:") retVoid [argCULong (if value then 1 else 0)]

-- | @- hasHorizontalRuler@
hasHorizontalRuler :: IsNSScrollView nsScrollView => nsScrollView -> IO Bool
hasHorizontalRuler nsScrollView  =
  fmap ((/= 0) :: CULong -> Bool) $ sendMsg nsScrollView (mkSelector "hasHorizontalRuler") retCULong []

-- | @- setHasHorizontalRuler:@
setHasHorizontalRuler :: IsNSScrollView nsScrollView => nsScrollView -> Bool -> IO ()
setHasHorizontalRuler nsScrollView  value =
  sendMsg nsScrollView (mkSelector "setHasHorizontalRuler:") retVoid [argCULong (if value then 1 else 0)]

-- | @- hasVerticalRuler@
hasVerticalRuler :: IsNSScrollView nsScrollView => nsScrollView -> IO Bool
hasVerticalRuler nsScrollView  =
  fmap ((/= 0) :: CULong -> Bool) $ sendMsg nsScrollView (mkSelector "hasVerticalRuler") retCULong []

-- | @- setHasVerticalRuler:@
setHasVerticalRuler :: IsNSScrollView nsScrollView => nsScrollView -> Bool -> IO ()
setHasVerticalRuler nsScrollView  value =
  sendMsg nsScrollView (mkSelector "setHasVerticalRuler:") retVoid [argCULong (if value then 1 else 0)]

-- | @- horizontalRulerView@
horizontalRulerView :: IsNSScrollView nsScrollView => nsScrollView -> IO (Id NSRulerView)
horizontalRulerView nsScrollView  =
  sendMsg nsScrollView (mkSelector "horizontalRulerView") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- setHorizontalRulerView:@
setHorizontalRulerView :: (IsNSScrollView nsScrollView, IsNSRulerView value) => nsScrollView -> value -> IO ()
setHorizontalRulerView nsScrollView  value =
withObjCPtr value $ \raw_value ->
    sendMsg nsScrollView (mkSelector "setHorizontalRulerView:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- | @- verticalRulerView@
verticalRulerView :: IsNSScrollView nsScrollView => nsScrollView -> IO (Id NSRulerView)
verticalRulerView nsScrollView  =
  sendMsg nsScrollView (mkSelector "verticalRulerView") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- setVerticalRulerView:@
setVerticalRulerView :: (IsNSScrollView nsScrollView, IsNSRulerView value) => nsScrollView -> value -> IO ()
setVerticalRulerView nsScrollView  value =
withObjCPtr value $ \raw_value ->
    sendMsg nsScrollView (mkSelector "setVerticalRulerView:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @initWithFrame:@
initWithFrameSelector :: Selector
initWithFrameSelector = mkSelector "initWithFrame:"

-- | @Selector@ for @initWithCoder:@
initWithCoderSelector :: Selector
initWithCoderSelector = mkSelector "initWithCoder:"

-- | @Selector@ for @frameSizeForContentSize:horizontalScrollerClass:verticalScrollerClass:borderType:controlSize:scrollerStyle:@
frameSizeForContentSize_horizontalScrollerClass_verticalScrollerClass_borderType_controlSize_scrollerStyleSelector :: Selector
frameSizeForContentSize_horizontalScrollerClass_verticalScrollerClass_borderType_controlSize_scrollerStyleSelector = mkSelector "frameSizeForContentSize:horizontalScrollerClass:verticalScrollerClass:borderType:controlSize:scrollerStyle:"

-- | @Selector@ for @contentSizeForFrameSize:horizontalScrollerClass:verticalScrollerClass:borderType:controlSize:scrollerStyle:@
contentSizeForFrameSize_horizontalScrollerClass_verticalScrollerClass_borderType_controlSize_scrollerStyleSelector :: Selector
contentSizeForFrameSize_horizontalScrollerClass_verticalScrollerClass_borderType_controlSize_scrollerStyleSelector = mkSelector "contentSizeForFrameSize:horizontalScrollerClass:verticalScrollerClass:borderType:controlSize:scrollerStyle:"

-- | @Selector@ for @frameSizeForContentSize:hasHorizontalScroller:hasVerticalScroller:borderType:@
frameSizeForContentSize_hasHorizontalScroller_hasVerticalScroller_borderTypeSelector :: Selector
frameSizeForContentSize_hasHorizontalScroller_hasVerticalScroller_borderTypeSelector = mkSelector "frameSizeForContentSize:hasHorizontalScroller:hasVerticalScroller:borderType:"

-- | @Selector@ for @contentSizeForFrameSize:hasHorizontalScroller:hasVerticalScroller:borderType:@
contentSizeForFrameSize_hasHorizontalScroller_hasVerticalScroller_borderTypeSelector :: Selector
contentSizeForFrameSize_hasHorizontalScroller_hasVerticalScroller_borderTypeSelector = mkSelector "contentSizeForFrameSize:hasHorizontalScroller:hasVerticalScroller:borderType:"

-- | @Selector@ for @tile@
tileSelector :: Selector
tileSelector = mkSelector "tile"

-- | @Selector@ for @reflectScrolledClipView:@
reflectScrolledClipViewSelector :: Selector
reflectScrolledClipViewSelector = mkSelector "reflectScrolledClipView:"

-- | @Selector@ for @scrollWheel:@
scrollWheelSelector :: Selector
scrollWheelSelector = mkSelector "scrollWheel:"

-- | @Selector@ for @flashScrollers@
flashScrollersSelector :: Selector
flashScrollersSelector = mkSelector "flashScrollers"

-- | @Selector@ for @magnifyToFitRect:@
magnifyToFitRectSelector :: Selector
magnifyToFitRectSelector = mkSelector "magnifyToFitRect:"

-- | @Selector@ for @setMagnification:centeredAtPoint:@
setMagnification_centeredAtPointSelector :: Selector
setMagnification_centeredAtPointSelector = mkSelector "setMagnification:centeredAtPoint:"

-- | @Selector@ for @addFloatingSubview:forAxis:@
addFloatingSubview_forAxisSelector :: Selector
addFloatingSubview_forAxisSelector = mkSelector "addFloatingSubview:forAxis:"

-- | @Selector@ for @documentVisibleRect@
documentVisibleRectSelector :: Selector
documentVisibleRectSelector = mkSelector "documentVisibleRect"

-- | @Selector@ for @contentSize@
contentSizeSelector :: Selector
contentSizeSelector = mkSelector "contentSize"

-- | @Selector@ for @documentView@
documentViewSelector :: Selector
documentViewSelector = mkSelector "documentView"

-- | @Selector@ for @setDocumentView:@
setDocumentViewSelector :: Selector
setDocumentViewSelector = mkSelector "setDocumentView:"

-- | @Selector@ for @contentView@
contentViewSelector :: Selector
contentViewSelector = mkSelector "contentView"

-- | @Selector@ for @setContentView:@
setContentViewSelector :: Selector
setContentViewSelector = mkSelector "setContentView:"

-- | @Selector@ for @documentCursor@
documentCursorSelector :: Selector
documentCursorSelector = mkSelector "documentCursor"

-- | @Selector@ for @setDocumentCursor:@
setDocumentCursorSelector :: Selector
setDocumentCursorSelector = mkSelector "setDocumentCursor:"

-- | @Selector@ for @borderType@
borderTypeSelector :: Selector
borderTypeSelector = mkSelector "borderType"

-- | @Selector@ for @setBorderType:@
setBorderTypeSelector :: Selector
setBorderTypeSelector = mkSelector "setBorderType:"

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

-- | @Selector@ for @hasVerticalScroller@
hasVerticalScrollerSelector :: Selector
hasVerticalScrollerSelector = mkSelector "hasVerticalScroller"

-- | @Selector@ for @setHasVerticalScroller:@
setHasVerticalScrollerSelector :: Selector
setHasVerticalScrollerSelector = mkSelector "setHasVerticalScroller:"

-- | @Selector@ for @hasHorizontalScroller@
hasHorizontalScrollerSelector :: Selector
hasHorizontalScrollerSelector = mkSelector "hasHorizontalScroller"

-- | @Selector@ for @setHasHorizontalScroller:@
setHasHorizontalScrollerSelector :: Selector
setHasHorizontalScrollerSelector = mkSelector "setHasHorizontalScroller:"

-- | @Selector@ for @verticalScroller@
verticalScrollerSelector :: Selector
verticalScrollerSelector = mkSelector "verticalScroller"

-- | @Selector@ for @setVerticalScroller:@
setVerticalScrollerSelector :: Selector
setVerticalScrollerSelector = mkSelector "setVerticalScroller:"

-- | @Selector@ for @horizontalScroller@
horizontalScrollerSelector :: Selector
horizontalScrollerSelector = mkSelector "horizontalScroller"

-- | @Selector@ for @setHorizontalScroller:@
setHorizontalScrollerSelector :: Selector
setHorizontalScrollerSelector = mkSelector "setHorizontalScroller:"

-- | @Selector@ for @autohidesScrollers@
autohidesScrollersSelector :: Selector
autohidesScrollersSelector = mkSelector "autohidesScrollers"

-- | @Selector@ for @setAutohidesScrollers:@
setAutohidesScrollersSelector :: Selector
setAutohidesScrollersSelector = mkSelector "setAutohidesScrollers:"

-- | @Selector@ for @horizontalLineScroll@
horizontalLineScrollSelector :: Selector
horizontalLineScrollSelector = mkSelector "horizontalLineScroll"

-- | @Selector@ for @setHorizontalLineScroll:@
setHorizontalLineScrollSelector :: Selector
setHorizontalLineScrollSelector = mkSelector "setHorizontalLineScroll:"

-- | @Selector@ for @verticalLineScroll@
verticalLineScrollSelector :: Selector
verticalLineScrollSelector = mkSelector "verticalLineScroll"

-- | @Selector@ for @setVerticalLineScroll:@
setVerticalLineScrollSelector :: Selector
setVerticalLineScrollSelector = mkSelector "setVerticalLineScroll:"

-- | @Selector@ for @lineScroll@
lineScrollSelector :: Selector
lineScrollSelector = mkSelector "lineScroll"

-- | @Selector@ for @setLineScroll:@
setLineScrollSelector :: Selector
setLineScrollSelector = mkSelector "setLineScroll:"

-- | @Selector@ for @horizontalPageScroll@
horizontalPageScrollSelector :: Selector
horizontalPageScrollSelector = mkSelector "horizontalPageScroll"

-- | @Selector@ for @setHorizontalPageScroll:@
setHorizontalPageScrollSelector :: Selector
setHorizontalPageScrollSelector = mkSelector "setHorizontalPageScroll:"

-- | @Selector@ for @verticalPageScroll@
verticalPageScrollSelector :: Selector
verticalPageScrollSelector = mkSelector "verticalPageScroll"

-- | @Selector@ for @setVerticalPageScroll:@
setVerticalPageScrollSelector :: Selector
setVerticalPageScrollSelector = mkSelector "setVerticalPageScroll:"

-- | @Selector@ for @pageScroll@
pageScrollSelector :: Selector
pageScrollSelector = mkSelector "pageScroll"

-- | @Selector@ for @setPageScroll:@
setPageScrollSelector :: Selector
setPageScrollSelector = mkSelector "setPageScroll:"

-- | @Selector@ for @scrollsDynamically@
scrollsDynamicallySelector :: Selector
scrollsDynamicallySelector = mkSelector "scrollsDynamically"

-- | @Selector@ for @setScrollsDynamically:@
setScrollsDynamicallySelector :: Selector
setScrollsDynamicallySelector = mkSelector "setScrollsDynamically:"

-- | @Selector@ for @scrollerStyle@
scrollerStyleSelector :: Selector
scrollerStyleSelector = mkSelector "scrollerStyle"

-- | @Selector@ for @setScrollerStyle:@
setScrollerStyleSelector :: Selector
setScrollerStyleSelector = mkSelector "setScrollerStyle:"

-- | @Selector@ for @scrollerKnobStyle@
scrollerKnobStyleSelector :: Selector
scrollerKnobStyleSelector = mkSelector "scrollerKnobStyle"

-- | @Selector@ for @setScrollerKnobStyle:@
setScrollerKnobStyleSelector :: Selector
setScrollerKnobStyleSelector = mkSelector "setScrollerKnobStyle:"

-- | @Selector@ for @horizontalScrollElasticity@
horizontalScrollElasticitySelector :: Selector
horizontalScrollElasticitySelector = mkSelector "horizontalScrollElasticity"

-- | @Selector@ for @setHorizontalScrollElasticity:@
setHorizontalScrollElasticitySelector :: Selector
setHorizontalScrollElasticitySelector = mkSelector "setHorizontalScrollElasticity:"

-- | @Selector@ for @verticalScrollElasticity@
verticalScrollElasticitySelector :: Selector
verticalScrollElasticitySelector = mkSelector "verticalScrollElasticity"

-- | @Selector@ for @setVerticalScrollElasticity:@
setVerticalScrollElasticitySelector :: Selector
setVerticalScrollElasticitySelector = mkSelector "setVerticalScrollElasticity:"

-- | @Selector@ for @usesPredominantAxisScrolling@
usesPredominantAxisScrollingSelector :: Selector
usesPredominantAxisScrollingSelector = mkSelector "usesPredominantAxisScrolling"

-- | @Selector@ for @setUsesPredominantAxisScrolling:@
setUsesPredominantAxisScrollingSelector :: Selector
setUsesPredominantAxisScrollingSelector = mkSelector "setUsesPredominantAxisScrolling:"

-- | @Selector@ for @allowsMagnification@
allowsMagnificationSelector :: Selector
allowsMagnificationSelector = mkSelector "allowsMagnification"

-- | @Selector@ for @setAllowsMagnification:@
setAllowsMagnificationSelector :: Selector
setAllowsMagnificationSelector = mkSelector "setAllowsMagnification:"

-- | @Selector@ for @magnification@
magnificationSelector :: Selector
magnificationSelector = mkSelector "magnification"

-- | @Selector@ for @setMagnification:@
setMagnificationSelector :: Selector
setMagnificationSelector = mkSelector "setMagnification:"

-- | @Selector@ for @maxMagnification@
maxMagnificationSelector :: Selector
maxMagnificationSelector = mkSelector "maxMagnification"

-- | @Selector@ for @setMaxMagnification:@
setMaxMagnificationSelector :: Selector
setMaxMagnificationSelector = mkSelector "setMaxMagnification:"

-- | @Selector@ for @minMagnification@
minMagnificationSelector :: Selector
minMagnificationSelector = mkSelector "minMagnification"

-- | @Selector@ for @setMinMagnification:@
setMinMagnificationSelector :: Selector
setMinMagnificationSelector = mkSelector "setMinMagnification:"

-- | @Selector@ for @automaticallyAdjustsContentInsets@
automaticallyAdjustsContentInsetsSelector :: Selector
automaticallyAdjustsContentInsetsSelector = mkSelector "automaticallyAdjustsContentInsets"

-- | @Selector@ for @setAutomaticallyAdjustsContentInsets:@
setAutomaticallyAdjustsContentInsetsSelector :: Selector
setAutomaticallyAdjustsContentInsetsSelector = mkSelector "setAutomaticallyAdjustsContentInsets:"

-- | @Selector@ for @contentInsets@
contentInsetsSelector :: Selector
contentInsetsSelector = mkSelector "contentInsets"

-- | @Selector@ for @setContentInsets:@
setContentInsetsSelector :: Selector
setContentInsetsSelector = mkSelector "setContentInsets:"

-- | @Selector@ for @scrollerInsets@
scrollerInsetsSelector :: Selector
scrollerInsetsSelector = mkSelector "scrollerInsets"

-- | @Selector@ for @setScrollerInsets:@
setScrollerInsetsSelector :: Selector
setScrollerInsetsSelector = mkSelector "setScrollerInsets:"

-- | @Selector@ for @findBarPosition@
findBarPositionSelector :: Selector
findBarPositionSelector = mkSelector "findBarPosition"

-- | @Selector@ for @setFindBarPosition:@
setFindBarPositionSelector :: Selector
setFindBarPositionSelector = mkSelector "setFindBarPosition:"

-- | @Selector@ for @rulerViewClass@
rulerViewClassSelector :: Selector
rulerViewClassSelector = mkSelector "rulerViewClass"

-- | @Selector@ for @setRulerViewClass:@
setRulerViewClassSelector :: Selector
setRulerViewClassSelector = mkSelector "setRulerViewClass:"

-- | @Selector@ for @rulersVisible@
rulersVisibleSelector :: Selector
rulersVisibleSelector = mkSelector "rulersVisible"

-- | @Selector@ for @setRulersVisible:@
setRulersVisibleSelector :: Selector
setRulersVisibleSelector = mkSelector "setRulersVisible:"

-- | @Selector@ for @hasHorizontalRuler@
hasHorizontalRulerSelector :: Selector
hasHorizontalRulerSelector = mkSelector "hasHorizontalRuler"

-- | @Selector@ for @setHasHorizontalRuler:@
setHasHorizontalRulerSelector :: Selector
setHasHorizontalRulerSelector = mkSelector "setHasHorizontalRuler:"

-- | @Selector@ for @hasVerticalRuler@
hasVerticalRulerSelector :: Selector
hasVerticalRulerSelector = mkSelector "hasVerticalRuler"

-- | @Selector@ for @setHasVerticalRuler:@
setHasVerticalRulerSelector :: Selector
setHasVerticalRulerSelector = mkSelector "setHasVerticalRuler:"

-- | @Selector@ for @horizontalRulerView@
horizontalRulerViewSelector :: Selector
horizontalRulerViewSelector = mkSelector "horizontalRulerView"

-- | @Selector@ for @setHorizontalRulerView:@
setHorizontalRulerViewSelector :: Selector
setHorizontalRulerViewSelector = mkSelector "setHorizontalRulerView:"

-- | @Selector@ for @verticalRulerView@
verticalRulerViewSelector :: Selector
verticalRulerViewSelector = mkSelector "verticalRulerView"

-- | @Selector@ for @setVerticalRulerView:@
setVerticalRulerViewSelector :: Selector
setVerticalRulerViewSelector = mkSelector "setVerticalRulerView:"

