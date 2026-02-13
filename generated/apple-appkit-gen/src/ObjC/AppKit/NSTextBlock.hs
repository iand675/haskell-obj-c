{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE DataKinds #-}
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
  , backgroundColorSelector
  , borderColorForEdgeSelector
  , boundsRectForContentRect_inRect_textContainer_characterRangeSelector
  , contentWidthSelector
  , contentWidthValueTypeSelector
  , drawBackgroundWithFrame_inView_characterRange_layoutManagerSelector
  , initSelector
  , rectForLayoutAtPoint_inRect_textContainer_characterRangeSelector
  , setBackgroundColorSelector
  , setBorderColorSelector
  , setBorderColor_forEdgeSelector
  , setContentWidth_typeSelector
  , setValue_type_forDimensionSelector
  , setVerticalAlignmentSelector
  , setWidth_type_forLayerSelector
  , setWidth_type_forLayer_edgeSelector
  , valueForDimensionSelector
  , valueTypeForDimensionSelector
  , verticalAlignmentSelector
  , widthForLayer_edgeSelector
  , widthValueTypeForLayer_edgeSelector

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

import Foreign.Ptr (Ptr, FunPtr)
import Foreign.C.Types

import ObjC.Runtime.Types
import ObjC.Runtime.Message (sendMessage, sendOwnedMessage, sendClassMessage, sendOwnedClassMessage)
import ObjC.Runtime.Selector (mkSelector)
import ObjC.Runtime.Class (getRequiredClass)

import ObjC.AppKit.Internal.Classes
import ObjC.Foundation.Internal.Structs
import ObjC.AppKit.Internal.Enums
import ObjC.Foundation.Internal.Enums
import ObjC.Foundation.Internal.Classes

-- | @- init@
init_ :: IsNSTextBlock nsTextBlock => nsTextBlock -> IO (Id NSTextBlock)
init_ nsTextBlock =
  sendOwnedMessage nsTextBlock initSelector

-- | @- setValue:type:forDimension:@
setValue_type_forDimension :: IsNSTextBlock nsTextBlock => nsTextBlock -> CDouble -> NSTextBlockValueType -> NSTextBlockDimension -> IO ()
setValue_type_forDimension nsTextBlock val type_ dimension =
  sendMessage nsTextBlock setValue_type_forDimensionSelector val type_ dimension

-- | @- valueForDimension:@
valueForDimension :: IsNSTextBlock nsTextBlock => nsTextBlock -> NSTextBlockDimension -> IO CDouble
valueForDimension nsTextBlock dimension =
  sendMessage nsTextBlock valueForDimensionSelector dimension

-- | @- valueTypeForDimension:@
valueTypeForDimension :: IsNSTextBlock nsTextBlock => nsTextBlock -> NSTextBlockDimension -> IO NSTextBlockValueType
valueTypeForDimension nsTextBlock dimension =
  sendMessage nsTextBlock valueTypeForDimensionSelector dimension

-- | @- setContentWidth:type:@
setContentWidth_type :: IsNSTextBlock nsTextBlock => nsTextBlock -> CDouble -> NSTextBlockValueType -> IO ()
setContentWidth_type nsTextBlock val type_ =
  sendMessage nsTextBlock setContentWidth_typeSelector val type_

-- | @- setWidth:type:forLayer:edge:@
setWidth_type_forLayer_edge :: IsNSTextBlock nsTextBlock => nsTextBlock -> CDouble -> NSTextBlockValueType -> NSTextBlockLayer -> NSRectEdge -> IO ()
setWidth_type_forLayer_edge nsTextBlock val type_ layer edge =
  sendMessage nsTextBlock setWidth_type_forLayer_edgeSelector val type_ layer edge

-- | @- setWidth:type:forLayer:@
setWidth_type_forLayer :: IsNSTextBlock nsTextBlock => nsTextBlock -> CDouble -> NSTextBlockValueType -> NSTextBlockLayer -> IO ()
setWidth_type_forLayer nsTextBlock val type_ layer =
  sendMessage nsTextBlock setWidth_type_forLayerSelector val type_ layer

-- | @- widthForLayer:edge:@
widthForLayer_edge :: IsNSTextBlock nsTextBlock => nsTextBlock -> NSTextBlockLayer -> NSRectEdge -> IO CDouble
widthForLayer_edge nsTextBlock layer edge =
  sendMessage nsTextBlock widthForLayer_edgeSelector layer edge

-- | @- widthValueTypeForLayer:edge:@
widthValueTypeForLayer_edge :: IsNSTextBlock nsTextBlock => nsTextBlock -> NSTextBlockLayer -> NSRectEdge -> IO NSTextBlockValueType
widthValueTypeForLayer_edge nsTextBlock layer edge =
  sendMessage nsTextBlock widthValueTypeForLayer_edgeSelector layer edge

-- | @- setBorderColor:forEdge:@
setBorderColor_forEdge :: (IsNSTextBlock nsTextBlock, IsNSColor color) => nsTextBlock -> color -> NSRectEdge -> IO ()
setBorderColor_forEdge nsTextBlock color edge =
  sendMessage nsTextBlock setBorderColor_forEdgeSelector (toNSColor color) edge

-- | @- setBorderColor:@
setBorderColor :: (IsNSTextBlock nsTextBlock, IsNSColor color) => nsTextBlock -> color -> IO ()
setBorderColor nsTextBlock color =
  sendMessage nsTextBlock setBorderColorSelector (toNSColor color)

-- | @- borderColorForEdge:@
borderColorForEdge :: IsNSTextBlock nsTextBlock => nsTextBlock -> NSRectEdge -> IO (Id NSColor)
borderColorForEdge nsTextBlock edge =
  sendMessage nsTextBlock borderColorForEdgeSelector edge

-- | @- rectForLayoutAtPoint:inRect:textContainer:characterRange:@
rectForLayoutAtPoint_inRect_textContainer_characterRange :: (IsNSTextBlock nsTextBlock, IsNSTextContainer textContainer) => nsTextBlock -> NSPoint -> NSRect -> textContainer -> NSRange -> IO NSRect
rectForLayoutAtPoint_inRect_textContainer_characterRange nsTextBlock startingPoint rect textContainer charRange =
  sendMessage nsTextBlock rectForLayoutAtPoint_inRect_textContainer_characterRangeSelector startingPoint rect (toNSTextContainer textContainer) charRange

-- | @- boundsRectForContentRect:inRect:textContainer:characterRange:@
boundsRectForContentRect_inRect_textContainer_characterRange :: (IsNSTextBlock nsTextBlock, IsNSTextContainer textContainer) => nsTextBlock -> NSRect -> NSRect -> textContainer -> NSRange -> IO NSRect
boundsRectForContentRect_inRect_textContainer_characterRange nsTextBlock contentRect rect textContainer charRange =
  sendMessage nsTextBlock boundsRectForContentRect_inRect_textContainer_characterRangeSelector contentRect rect (toNSTextContainer textContainer) charRange

-- | @- drawBackgroundWithFrame:inView:characterRange:layoutManager:@
drawBackgroundWithFrame_inView_characterRange_layoutManager :: (IsNSTextBlock nsTextBlock, IsNSView controlView, IsNSLayoutManager layoutManager) => nsTextBlock -> NSRect -> controlView -> NSRange -> layoutManager -> IO ()
drawBackgroundWithFrame_inView_characterRange_layoutManager nsTextBlock frameRect controlView charRange layoutManager =
  sendMessage nsTextBlock drawBackgroundWithFrame_inView_characterRange_layoutManagerSelector frameRect (toNSView controlView) charRange (toNSLayoutManager layoutManager)

-- | @- contentWidth@
contentWidth :: IsNSTextBlock nsTextBlock => nsTextBlock -> IO CDouble
contentWidth nsTextBlock =
  sendMessage nsTextBlock contentWidthSelector

-- | @- contentWidthValueType@
contentWidthValueType :: IsNSTextBlock nsTextBlock => nsTextBlock -> IO NSTextBlockValueType
contentWidthValueType nsTextBlock =
  sendMessage nsTextBlock contentWidthValueTypeSelector

-- | @- verticalAlignment@
verticalAlignment :: IsNSTextBlock nsTextBlock => nsTextBlock -> IO NSTextBlockVerticalAlignment
verticalAlignment nsTextBlock =
  sendMessage nsTextBlock verticalAlignmentSelector

-- | @- setVerticalAlignment:@
setVerticalAlignment :: IsNSTextBlock nsTextBlock => nsTextBlock -> NSTextBlockVerticalAlignment -> IO ()
setVerticalAlignment nsTextBlock value =
  sendMessage nsTextBlock setVerticalAlignmentSelector value

-- | @- backgroundColor@
backgroundColor :: IsNSTextBlock nsTextBlock => nsTextBlock -> IO (Id NSColor)
backgroundColor nsTextBlock =
  sendMessage nsTextBlock backgroundColorSelector

-- | @- setBackgroundColor:@
setBackgroundColor :: (IsNSTextBlock nsTextBlock, IsNSColor value) => nsTextBlock -> value -> IO ()
setBackgroundColor nsTextBlock value =
  sendMessage nsTextBlock setBackgroundColorSelector (toNSColor value)

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @init@
initSelector :: Selector '[] (Id NSTextBlock)
initSelector = mkSelector "init"

-- | @Selector@ for @setValue:type:forDimension:@
setValue_type_forDimensionSelector :: Selector '[CDouble, NSTextBlockValueType, NSTextBlockDimension] ()
setValue_type_forDimensionSelector = mkSelector "setValue:type:forDimension:"

-- | @Selector@ for @valueForDimension:@
valueForDimensionSelector :: Selector '[NSTextBlockDimension] CDouble
valueForDimensionSelector = mkSelector "valueForDimension:"

-- | @Selector@ for @valueTypeForDimension:@
valueTypeForDimensionSelector :: Selector '[NSTextBlockDimension] NSTextBlockValueType
valueTypeForDimensionSelector = mkSelector "valueTypeForDimension:"

-- | @Selector@ for @setContentWidth:type:@
setContentWidth_typeSelector :: Selector '[CDouble, NSTextBlockValueType] ()
setContentWidth_typeSelector = mkSelector "setContentWidth:type:"

-- | @Selector@ for @setWidth:type:forLayer:edge:@
setWidth_type_forLayer_edgeSelector :: Selector '[CDouble, NSTextBlockValueType, NSTextBlockLayer, NSRectEdge] ()
setWidth_type_forLayer_edgeSelector = mkSelector "setWidth:type:forLayer:edge:"

-- | @Selector@ for @setWidth:type:forLayer:@
setWidth_type_forLayerSelector :: Selector '[CDouble, NSTextBlockValueType, NSTextBlockLayer] ()
setWidth_type_forLayerSelector = mkSelector "setWidth:type:forLayer:"

-- | @Selector@ for @widthForLayer:edge:@
widthForLayer_edgeSelector :: Selector '[NSTextBlockLayer, NSRectEdge] CDouble
widthForLayer_edgeSelector = mkSelector "widthForLayer:edge:"

-- | @Selector@ for @widthValueTypeForLayer:edge:@
widthValueTypeForLayer_edgeSelector :: Selector '[NSTextBlockLayer, NSRectEdge] NSTextBlockValueType
widthValueTypeForLayer_edgeSelector = mkSelector "widthValueTypeForLayer:edge:"

-- | @Selector@ for @setBorderColor:forEdge:@
setBorderColor_forEdgeSelector :: Selector '[Id NSColor, NSRectEdge] ()
setBorderColor_forEdgeSelector = mkSelector "setBorderColor:forEdge:"

-- | @Selector@ for @setBorderColor:@
setBorderColorSelector :: Selector '[Id NSColor] ()
setBorderColorSelector = mkSelector "setBorderColor:"

-- | @Selector@ for @borderColorForEdge:@
borderColorForEdgeSelector :: Selector '[NSRectEdge] (Id NSColor)
borderColorForEdgeSelector = mkSelector "borderColorForEdge:"

-- | @Selector@ for @rectForLayoutAtPoint:inRect:textContainer:characterRange:@
rectForLayoutAtPoint_inRect_textContainer_characterRangeSelector :: Selector '[NSPoint, NSRect, Id NSTextContainer, NSRange] NSRect
rectForLayoutAtPoint_inRect_textContainer_characterRangeSelector = mkSelector "rectForLayoutAtPoint:inRect:textContainer:characterRange:"

-- | @Selector@ for @boundsRectForContentRect:inRect:textContainer:characterRange:@
boundsRectForContentRect_inRect_textContainer_characterRangeSelector :: Selector '[NSRect, NSRect, Id NSTextContainer, NSRange] NSRect
boundsRectForContentRect_inRect_textContainer_characterRangeSelector = mkSelector "boundsRectForContentRect:inRect:textContainer:characterRange:"

-- | @Selector@ for @drawBackgroundWithFrame:inView:characterRange:layoutManager:@
drawBackgroundWithFrame_inView_characterRange_layoutManagerSelector :: Selector '[NSRect, Id NSView, NSRange, Id NSLayoutManager] ()
drawBackgroundWithFrame_inView_characterRange_layoutManagerSelector = mkSelector "drawBackgroundWithFrame:inView:characterRange:layoutManager:"

-- | @Selector@ for @contentWidth@
contentWidthSelector :: Selector '[] CDouble
contentWidthSelector = mkSelector "contentWidth"

-- | @Selector@ for @contentWidthValueType@
contentWidthValueTypeSelector :: Selector '[] NSTextBlockValueType
contentWidthValueTypeSelector = mkSelector "contentWidthValueType"

-- | @Selector@ for @verticalAlignment@
verticalAlignmentSelector :: Selector '[] NSTextBlockVerticalAlignment
verticalAlignmentSelector = mkSelector "verticalAlignment"

-- | @Selector@ for @setVerticalAlignment:@
setVerticalAlignmentSelector :: Selector '[NSTextBlockVerticalAlignment] ()
setVerticalAlignmentSelector = mkSelector "setVerticalAlignment:"

-- | @Selector@ for @backgroundColor@
backgroundColorSelector :: Selector '[] (Id NSColor)
backgroundColorSelector = mkSelector "backgroundColor"

-- | @Selector@ for @setBackgroundColor:@
setBackgroundColorSelector :: Selector '[Id NSColor] ()
setBackgroundColorSelector = mkSelector "setBackgroundColor:"

