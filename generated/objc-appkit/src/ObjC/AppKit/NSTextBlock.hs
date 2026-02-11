{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Generated bindings for @NSTextBlock@.
module ObjC.AppKit.NSTextBlock
  ( NSTextBlock
  , IsNSTextBlock(..)
  , init_
  , setValue_type_forDimension
  , valueForDimension
  , valueTypeForDimension
  , setContentWidth_type
  , setWidth_type_forLayer_edge
  , setWidth_type_forLayer
  , widthForLayer_edge
  , widthValueTypeForLayer_edge
  , setBorderColor_forEdge
  , setBorderColor
  , borderColorForEdge
  , rectForLayoutAtPoint_inRect_textContainer_characterRange
  , boundsRectForContentRect_inRect_textContainer_characterRange
  , drawBackgroundWithFrame_inView_characterRange_layoutManager
  , contentWidth
  , contentWidthValueType
  , verticalAlignment
  , setVerticalAlignment
  , backgroundColor
  , setBackgroundColor
  , initSelector
  , setValue_type_forDimensionSelector
  , valueForDimensionSelector
  , valueTypeForDimensionSelector
  , setContentWidth_typeSelector
  , setWidth_type_forLayer_edgeSelector
  , setWidth_type_forLayerSelector
  , widthForLayer_edgeSelector
  , widthValueTypeForLayer_edgeSelector
  , setBorderColor_forEdgeSelector
  , setBorderColorSelector
  , borderColorForEdgeSelector
  , rectForLayoutAtPoint_inRect_textContainer_characterRangeSelector
  , boundsRectForContentRect_inRect_textContainer_characterRangeSelector
  , drawBackgroundWithFrame_inView_characterRange_layoutManagerSelector
  , contentWidthSelector
  , contentWidthValueTypeSelector
  , verticalAlignmentSelector
  , setVerticalAlignmentSelector
  , backgroundColorSelector
  , setBackgroundColorSelector

  -- * Enum types
  , NSRectEdge(NSRectEdge)
  , pattern NSRectEdgeMinX
  , pattern NSRectEdgeMinY
  , pattern NSRectEdgeMaxX
  , pattern NSRectEdgeMaxY
  , pattern NSMinXEdge
  , pattern NSMinYEdge
  , pattern NSMaxXEdge
  , pattern NSMaxYEdge
  , NSTextBlockDimension(NSTextBlockDimension)
  , pattern NSTextBlockWidth
  , pattern NSTextBlockMinimumWidth
  , pattern NSTextBlockMaximumWidth
  , pattern NSTextBlockHeight
  , pattern NSTextBlockMinimumHeight
  , pattern NSTextBlockMaximumHeight
  , NSTextBlockLayer(NSTextBlockLayer)
  , pattern NSTextBlockPadding
  , pattern NSTextBlockBorder
  , pattern NSTextBlockMargin
  , NSTextBlockValueType(NSTextBlockValueType)
  , pattern NSTextBlockAbsoluteValueType
  , pattern NSTextBlockPercentageValueType
  , NSTextBlockVerticalAlignment(NSTextBlockVerticalAlignment)
  , pattern NSTextBlockTopAlignment
  , pattern NSTextBlockMiddleAlignment
  , pattern NSTextBlockBottomAlignment
  , pattern NSTextBlockBaselineAlignment

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
import ObjC.Foundation.Internal.Enums
import ObjC.Foundation.Internal.Classes

-- | @- init@
init_ :: IsNSTextBlock nsTextBlock => nsTextBlock -> IO (Id NSTextBlock)
init_ nsTextBlock  =
  sendMsg nsTextBlock (mkSelector "init") (retPtr retVoid) [] >>= ownedObject . castPtr

-- | @- setValue:type:forDimension:@
setValue_type_forDimension :: IsNSTextBlock nsTextBlock => nsTextBlock -> CDouble -> NSTextBlockValueType -> NSTextBlockDimension -> IO ()
setValue_type_forDimension nsTextBlock  val type_ dimension =
  sendMsg nsTextBlock (mkSelector "setValue:type:forDimension:") retVoid [argCDouble (fromIntegral val), argCULong (coerce type_), argCULong (coerce dimension)]

-- | @- valueForDimension:@
valueForDimension :: IsNSTextBlock nsTextBlock => nsTextBlock -> NSTextBlockDimension -> IO CDouble
valueForDimension nsTextBlock  dimension =
  sendMsg nsTextBlock (mkSelector "valueForDimension:") retCDouble [argCULong (coerce dimension)]

-- | @- valueTypeForDimension:@
valueTypeForDimension :: IsNSTextBlock nsTextBlock => nsTextBlock -> NSTextBlockDimension -> IO NSTextBlockValueType
valueTypeForDimension nsTextBlock  dimension =
  fmap (coerce :: CULong -> NSTextBlockValueType) $ sendMsg nsTextBlock (mkSelector "valueTypeForDimension:") retCULong [argCULong (coerce dimension)]

-- | @- setContentWidth:type:@
setContentWidth_type :: IsNSTextBlock nsTextBlock => nsTextBlock -> CDouble -> NSTextBlockValueType -> IO ()
setContentWidth_type nsTextBlock  val type_ =
  sendMsg nsTextBlock (mkSelector "setContentWidth:type:") retVoid [argCDouble (fromIntegral val), argCULong (coerce type_)]

-- | @- setWidth:type:forLayer:edge:@
setWidth_type_forLayer_edge :: IsNSTextBlock nsTextBlock => nsTextBlock -> CDouble -> NSTextBlockValueType -> NSTextBlockLayer -> NSRectEdge -> IO ()
setWidth_type_forLayer_edge nsTextBlock  val type_ layer edge =
  sendMsg nsTextBlock (mkSelector "setWidth:type:forLayer:edge:") retVoid [argCDouble (fromIntegral val), argCULong (coerce type_), argCLong (coerce layer), argCULong (coerce edge)]

-- | @- setWidth:type:forLayer:@
setWidth_type_forLayer :: IsNSTextBlock nsTextBlock => nsTextBlock -> CDouble -> NSTextBlockValueType -> NSTextBlockLayer -> IO ()
setWidth_type_forLayer nsTextBlock  val type_ layer =
  sendMsg nsTextBlock (mkSelector "setWidth:type:forLayer:") retVoid [argCDouble (fromIntegral val), argCULong (coerce type_), argCLong (coerce layer)]

-- | @- widthForLayer:edge:@
widthForLayer_edge :: IsNSTextBlock nsTextBlock => nsTextBlock -> NSTextBlockLayer -> NSRectEdge -> IO CDouble
widthForLayer_edge nsTextBlock  layer edge =
  sendMsg nsTextBlock (mkSelector "widthForLayer:edge:") retCDouble [argCLong (coerce layer), argCULong (coerce edge)]

-- | @- widthValueTypeForLayer:edge:@
widthValueTypeForLayer_edge :: IsNSTextBlock nsTextBlock => nsTextBlock -> NSTextBlockLayer -> NSRectEdge -> IO NSTextBlockValueType
widthValueTypeForLayer_edge nsTextBlock  layer edge =
  fmap (coerce :: CULong -> NSTextBlockValueType) $ sendMsg nsTextBlock (mkSelector "widthValueTypeForLayer:edge:") retCULong [argCLong (coerce layer), argCULong (coerce edge)]

-- | @- setBorderColor:forEdge:@
setBorderColor_forEdge :: (IsNSTextBlock nsTextBlock, IsNSColor color) => nsTextBlock -> color -> NSRectEdge -> IO ()
setBorderColor_forEdge nsTextBlock  color edge =
withObjCPtr color $ \raw_color ->
    sendMsg nsTextBlock (mkSelector "setBorderColor:forEdge:") retVoid [argPtr (castPtr raw_color :: Ptr ()), argCULong (coerce edge)]

-- | @- setBorderColor:@
setBorderColor :: (IsNSTextBlock nsTextBlock, IsNSColor color) => nsTextBlock -> color -> IO ()
setBorderColor nsTextBlock  color =
withObjCPtr color $ \raw_color ->
    sendMsg nsTextBlock (mkSelector "setBorderColor:") retVoid [argPtr (castPtr raw_color :: Ptr ())]

-- | @- borderColorForEdge:@
borderColorForEdge :: IsNSTextBlock nsTextBlock => nsTextBlock -> NSRectEdge -> IO (Id NSColor)
borderColorForEdge nsTextBlock  edge =
  sendMsg nsTextBlock (mkSelector "borderColorForEdge:") (retPtr retVoid) [argCULong (coerce edge)] >>= retainedObject . castPtr

-- | @- rectForLayoutAtPoint:inRect:textContainer:characterRange:@
rectForLayoutAtPoint_inRect_textContainer_characterRange :: (IsNSTextBlock nsTextBlock, IsNSTextContainer textContainer) => nsTextBlock -> NSPoint -> NSRect -> textContainer -> NSRange -> IO NSRect
rectForLayoutAtPoint_inRect_textContainer_characterRange nsTextBlock  startingPoint rect textContainer charRange =
withObjCPtr textContainer $ \raw_textContainer ->
    sendMsgStret nsTextBlock (mkSelector "rectForLayoutAtPoint:inRect:textContainer:characterRange:") retNSRect [argNSPoint startingPoint, argNSRect rect, argPtr (castPtr raw_textContainer :: Ptr ()), argNSRange charRange]

-- | @- boundsRectForContentRect:inRect:textContainer:characterRange:@
boundsRectForContentRect_inRect_textContainer_characterRange :: (IsNSTextBlock nsTextBlock, IsNSTextContainer textContainer) => nsTextBlock -> NSRect -> NSRect -> textContainer -> NSRange -> IO NSRect
boundsRectForContentRect_inRect_textContainer_characterRange nsTextBlock  contentRect rect textContainer charRange =
withObjCPtr textContainer $ \raw_textContainer ->
    sendMsgStret nsTextBlock (mkSelector "boundsRectForContentRect:inRect:textContainer:characterRange:") retNSRect [argNSRect contentRect, argNSRect rect, argPtr (castPtr raw_textContainer :: Ptr ()), argNSRange charRange]

-- | @- drawBackgroundWithFrame:inView:characterRange:layoutManager:@
drawBackgroundWithFrame_inView_characterRange_layoutManager :: (IsNSTextBlock nsTextBlock, IsNSView controlView, IsNSLayoutManager layoutManager) => nsTextBlock -> NSRect -> controlView -> NSRange -> layoutManager -> IO ()
drawBackgroundWithFrame_inView_characterRange_layoutManager nsTextBlock  frameRect controlView charRange layoutManager =
withObjCPtr controlView $ \raw_controlView ->
  withObjCPtr layoutManager $ \raw_layoutManager ->
      sendMsg nsTextBlock (mkSelector "drawBackgroundWithFrame:inView:characterRange:layoutManager:") retVoid [argNSRect frameRect, argPtr (castPtr raw_controlView :: Ptr ()), argNSRange charRange, argPtr (castPtr raw_layoutManager :: Ptr ())]

-- | @- contentWidth@
contentWidth :: IsNSTextBlock nsTextBlock => nsTextBlock -> IO CDouble
contentWidth nsTextBlock  =
  sendMsg nsTextBlock (mkSelector "contentWidth") retCDouble []

-- | @- contentWidthValueType@
contentWidthValueType :: IsNSTextBlock nsTextBlock => nsTextBlock -> IO NSTextBlockValueType
contentWidthValueType nsTextBlock  =
  fmap (coerce :: CULong -> NSTextBlockValueType) $ sendMsg nsTextBlock (mkSelector "contentWidthValueType") retCULong []

-- | @- verticalAlignment@
verticalAlignment :: IsNSTextBlock nsTextBlock => nsTextBlock -> IO NSTextBlockVerticalAlignment
verticalAlignment nsTextBlock  =
  fmap (coerce :: CULong -> NSTextBlockVerticalAlignment) $ sendMsg nsTextBlock (mkSelector "verticalAlignment") retCULong []

-- | @- setVerticalAlignment:@
setVerticalAlignment :: IsNSTextBlock nsTextBlock => nsTextBlock -> NSTextBlockVerticalAlignment -> IO ()
setVerticalAlignment nsTextBlock  value =
  sendMsg nsTextBlock (mkSelector "setVerticalAlignment:") retVoid [argCULong (coerce value)]

-- | @- backgroundColor@
backgroundColor :: IsNSTextBlock nsTextBlock => nsTextBlock -> IO (Id NSColor)
backgroundColor nsTextBlock  =
  sendMsg nsTextBlock (mkSelector "backgroundColor") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- setBackgroundColor:@
setBackgroundColor :: (IsNSTextBlock nsTextBlock, IsNSColor value) => nsTextBlock -> value -> IO ()
setBackgroundColor nsTextBlock  value =
withObjCPtr value $ \raw_value ->
    sendMsg nsTextBlock (mkSelector "setBackgroundColor:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @init@
initSelector :: Selector
initSelector = mkSelector "init"

-- | @Selector@ for @setValue:type:forDimension:@
setValue_type_forDimensionSelector :: Selector
setValue_type_forDimensionSelector = mkSelector "setValue:type:forDimension:"

-- | @Selector@ for @valueForDimension:@
valueForDimensionSelector :: Selector
valueForDimensionSelector = mkSelector "valueForDimension:"

-- | @Selector@ for @valueTypeForDimension:@
valueTypeForDimensionSelector :: Selector
valueTypeForDimensionSelector = mkSelector "valueTypeForDimension:"

-- | @Selector@ for @setContentWidth:type:@
setContentWidth_typeSelector :: Selector
setContentWidth_typeSelector = mkSelector "setContentWidth:type:"

-- | @Selector@ for @setWidth:type:forLayer:edge:@
setWidth_type_forLayer_edgeSelector :: Selector
setWidth_type_forLayer_edgeSelector = mkSelector "setWidth:type:forLayer:edge:"

-- | @Selector@ for @setWidth:type:forLayer:@
setWidth_type_forLayerSelector :: Selector
setWidth_type_forLayerSelector = mkSelector "setWidth:type:forLayer:"

-- | @Selector@ for @widthForLayer:edge:@
widthForLayer_edgeSelector :: Selector
widthForLayer_edgeSelector = mkSelector "widthForLayer:edge:"

-- | @Selector@ for @widthValueTypeForLayer:edge:@
widthValueTypeForLayer_edgeSelector :: Selector
widthValueTypeForLayer_edgeSelector = mkSelector "widthValueTypeForLayer:edge:"

-- | @Selector@ for @setBorderColor:forEdge:@
setBorderColor_forEdgeSelector :: Selector
setBorderColor_forEdgeSelector = mkSelector "setBorderColor:forEdge:"

-- | @Selector@ for @setBorderColor:@
setBorderColorSelector :: Selector
setBorderColorSelector = mkSelector "setBorderColor:"

-- | @Selector@ for @borderColorForEdge:@
borderColorForEdgeSelector :: Selector
borderColorForEdgeSelector = mkSelector "borderColorForEdge:"

-- | @Selector@ for @rectForLayoutAtPoint:inRect:textContainer:characterRange:@
rectForLayoutAtPoint_inRect_textContainer_characterRangeSelector :: Selector
rectForLayoutAtPoint_inRect_textContainer_characterRangeSelector = mkSelector "rectForLayoutAtPoint:inRect:textContainer:characterRange:"

-- | @Selector@ for @boundsRectForContentRect:inRect:textContainer:characterRange:@
boundsRectForContentRect_inRect_textContainer_characterRangeSelector :: Selector
boundsRectForContentRect_inRect_textContainer_characterRangeSelector = mkSelector "boundsRectForContentRect:inRect:textContainer:characterRange:"

-- | @Selector@ for @drawBackgroundWithFrame:inView:characterRange:layoutManager:@
drawBackgroundWithFrame_inView_characterRange_layoutManagerSelector :: Selector
drawBackgroundWithFrame_inView_characterRange_layoutManagerSelector = mkSelector "drawBackgroundWithFrame:inView:characterRange:layoutManager:"

-- | @Selector@ for @contentWidth@
contentWidthSelector :: Selector
contentWidthSelector = mkSelector "contentWidth"

-- | @Selector@ for @contentWidthValueType@
contentWidthValueTypeSelector :: Selector
contentWidthValueTypeSelector = mkSelector "contentWidthValueType"

-- | @Selector@ for @verticalAlignment@
verticalAlignmentSelector :: Selector
verticalAlignmentSelector = mkSelector "verticalAlignment"

-- | @Selector@ for @setVerticalAlignment:@
setVerticalAlignmentSelector :: Selector
setVerticalAlignmentSelector = mkSelector "setVerticalAlignment:"

-- | @Selector@ for @backgroundColor@
backgroundColorSelector :: Selector
backgroundColorSelector = mkSelector "backgroundColor"

-- | @Selector@ for @setBackgroundColor:@
setBackgroundColorSelector :: Selector
setBackgroundColorSelector = mkSelector "setBackgroundColor:"

