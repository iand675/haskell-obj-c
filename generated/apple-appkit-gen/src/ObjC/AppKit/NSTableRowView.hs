{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Generated bindings for @NSTableRowView@.
module ObjC.AppKit.NSTableRowView
  ( NSTableRowView
  , IsNSTableRowView(..)
  , drawBackgroundInRect
  , drawSelectionInRect
  , drawSeparatorInRect
  , drawDraggingDestinationFeedbackInRect
  , viewAtColumn
  , selectionHighlightStyle
  , setSelectionHighlightStyle
  , emphasized
  , setEmphasized
  , groupRowStyle
  , setGroupRowStyle
  , selected
  , setSelected
  , previousRowSelected
  , setPreviousRowSelected
  , nextRowSelected
  , setNextRowSelected
  , floating
  , setFloating
  , targetForDropOperation
  , setTargetForDropOperation
  , draggingDestinationFeedbackStyle
  , setDraggingDestinationFeedbackStyle
  , indentationForDropOperation
  , setIndentationForDropOperation
  , interiorBackgroundStyle
  , backgroundColor
  , setBackgroundColor
  , numberOfColumns
  , backgroundColorSelector
  , draggingDestinationFeedbackStyleSelector
  , drawBackgroundInRectSelector
  , drawDraggingDestinationFeedbackInRectSelector
  , drawSelectionInRectSelector
  , drawSeparatorInRectSelector
  , emphasizedSelector
  , floatingSelector
  , groupRowStyleSelector
  , indentationForDropOperationSelector
  , interiorBackgroundStyleSelector
  , nextRowSelectedSelector
  , numberOfColumnsSelector
  , previousRowSelectedSelector
  , selectedSelector
  , selectionHighlightStyleSelector
  , setBackgroundColorSelector
  , setDraggingDestinationFeedbackStyleSelector
  , setEmphasizedSelector
  , setFloatingSelector
  , setGroupRowStyleSelector
  , setIndentationForDropOperationSelector
  , setNextRowSelectedSelector
  , setPreviousRowSelectedSelector
  , setSelectedSelector
  , setSelectionHighlightStyleSelector
  , setTargetForDropOperationSelector
  , targetForDropOperationSelector
  , viewAtColumnSelector

  -- * Enum types
  , NSBackgroundStyle(NSBackgroundStyle)
  , pattern NSBackgroundStyleNormal
  , pattern NSBackgroundStyleEmphasized
  , pattern NSBackgroundStyleRaised
  , pattern NSBackgroundStyleLowered
  , NSTableViewDraggingDestinationFeedbackStyle(NSTableViewDraggingDestinationFeedbackStyle)
  , pattern NSTableViewDraggingDestinationFeedbackStyleNone
  , pattern NSTableViewDraggingDestinationFeedbackStyleRegular
  , pattern NSTableViewDraggingDestinationFeedbackStyleSourceList
  , pattern NSTableViewDraggingDestinationFeedbackStyleGap
  , NSTableViewSelectionHighlightStyle(NSTableViewSelectionHighlightStyle)
  , pattern NSTableViewSelectionHighlightStyleNone
  , pattern NSTableViewSelectionHighlightStyleRegular
  , pattern NSTableViewSelectionHighlightStyleSourceList

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

-- | @- drawBackgroundInRect:@
drawBackgroundInRect :: IsNSTableRowView nsTableRowView => nsTableRowView -> NSRect -> IO ()
drawBackgroundInRect nsTableRowView dirtyRect =
  sendMessage nsTableRowView drawBackgroundInRectSelector dirtyRect

-- | @- drawSelectionInRect:@
drawSelectionInRect :: IsNSTableRowView nsTableRowView => nsTableRowView -> NSRect -> IO ()
drawSelectionInRect nsTableRowView dirtyRect =
  sendMessage nsTableRowView drawSelectionInRectSelector dirtyRect

-- | @- drawSeparatorInRect:@
drawSeparatorInRect :: IsNSTableRowView nsTableRowView => nsTableRowView -> NSRect -> IO ()
drawSeparatorInRect nsTableRowView dirtyRect =
  sendMessage nsTableRowView drawSeparatorInRectSelector dirtyRect

-- | @- drawDraggingDestinationFeedbackInRect:@
drawDraggingDestinationFeedbackInRect :: IsNSTableRowView nsTableRowView => nsTableRowView -> NSRect -> IO ()
drawDraggingDestinationFeedbackInRect nsTableRowView dirtyRect =
  sendMessage nsTableRowView drawDraggingDestinationFeedbackInRectSelector dirtyRect

-- | @- viewAtColumn:@
viewAtColumn :: IsNSTableRowView nsTableRowView => nsTableRowView -> CLong -> IO RawId
viewAtColumn nsTableRowView column =
  sendMessage nsTableRowView viewAtColumnSelector column

-- | @- selectionHighlightStyle@
selectionHighlightStyle :: IsNSTableRowView nsTableRowView => nsTableRowView -> IO NSTableViewSelectionHighlightStyle
selectionHighlightStyle nsTableRowView =
  sendMessage nsTableRowView selectionHighlightStyleSelector

-- | @- setSelectionHighlightStyle:@
setSelectionHighlightStyle :: IsNSTableRowView nsTableRowView => nsTableRowView -> NSTableViewSelectionHighlightStyle -> IO ()
setSelectionHighlightStyle nsTableRowView value =
  sendMessage nsTableRowView setSelectionHighlightStyleSelector value

-- | @- emphasized@
emphasized :: IsNSTableRowView nsTableRowView => nsTableRowView -> IO Bool
emphasized nsTableRowView =
  sendMessage nsTableRowView emphasizedSelector

-- | @- setEmphasized:@
setEmphasized :: IsNSTableRowView nsTableRowView => nsTableRowView -> Bool -> IO ()
setEmphasized nsTableRowView value =
  sendMessage nsTableRowView setEmphasizedSelector value

-- | @- groupRowStyle@
groupRowStyle :: IsNSTableRowView nsTableRowView => nsTableRowView -> IO Bool
groupRowStyle nsTableRowView =
  sendMessage nsTableRowView groupRowStyleSelector

-- | @- setGroupRowStyle:@
setGroupRowStyle :: IsNSTableRowView nsTableRowView => nsTableRowView -> Bool -> IO ()
setGroupRowStyle nsTableRowView value =
  sendMessage nsTableRowView setGroupRowStyleSelector value

-- | @- selected@
selected :: IsNSTableRowView nsTableRowView => nsTableRowView -> IO Bool
selected nsTableRowView =
  sendMessage nsTableRowView selectedSelector

-- | @- setSelected:@
setSelected :: IsNSTableRowView nsTableRowView => nsTableRowView -> Bool -> IO ()
setSelected nsTableRowView value =
  sendMessage nsTableRowView setSelectedSelector value

-- | @- previousRowSelected@
previousRowSelected :: IsNSTableRowView nsTableRowView => nsTableRowView -> IO Bool
previousRowSelected nsTableRowView =
  sendMessage nsTableRowView previousRowSelectedSelector

-- | @- setPreviousRowSelected:@
setPreviousRowSelected :: IsNSTableRowView nsTableRowView => nsTableRowView -> Bool -> IO ()
setPreviousRowSelected nsTableRowView value =
  sendMessage nsTableRowView setPreviousRowSelectedSelector value

-- | @- nextRowSelected@
nextRowSelected :: IsNSTableRowView nsTableRowView => nsTableRowView -> IO Bool
nextRowSelected nsTableRowView =
  sendMessage nsTableRowView nextRowSelectedSelector

-- | @- setNextRowSelected:@
setNextRowSelected :: IsNSTableRowView nsTableRowView => nsTableRowView -> Bool -> IO ()
setNextRowSelected nsTableRowView value =
  sendMessage nsTableRowView setNextRowSelectedSelector value

-- | @- floating@
floating :: IsNSTableRowView nsTableRowView => nsTableRowView -> IO Bool
floating nsTableRowView =
  sendMessage nsTableRowView floatingSelector

-- | @- setFloating:@
setFloating :: IsNSTableRowView nsTableRowView => nsTableRowView -> Bool -> IO ()
setFloating nsTableRowView value =
  sendMessage nsTableRowView setFloatingSelector value

-- | @- targetForDropOperation@
targetForDropOperation :: IsNSTableRowView nsTableRowView => nsTableRowView -> IO Bool
targetForDropOperation nsTableRowView =
  sendMessage nsTableRowView targetForDropOperationSelector

-- | @- setTargetForDropOperation:@
setTargetForDropOperation :: IsNSTableRowView nsTableRowView => nsTableRowView -> Bool -> IO ()
setTargetForDropOperation nsTableRowView value =
  sendMessage nsTableRowView setTargetForDropOperationSelector value

-- | @- draggingDestinationFeedbackStyle@
draggingDestinationFeedbackStyle :: IsNSTableRowView nsTableRowView => nsTableRowView -> IO NSTableViewDraggingDestinationFeedbackStyle
draggingDestinationFeedbackStyle nsTableRowView =
  sendMessage nsTableRowView draggingDestinationFeedbackStyleSelector

-- | @- setDraggingDestinationFeedbackStyle:@
setDraggingDestinationFeedbackStyle :: IsNSTableRowView nsTableRowView => nsTableRowView -> NSTableViewDraggingDestinationFeedbackStyle -> IO ()
setDraggingDestinationFeedbackStyle nsTableRowView value =
  sendMessage nsTableRowView setDraggingDestinationFeedbackStyleSelector value

-- | @- indentationForDropOperation@
indentationForDropOperation :: IsNSTableRowView nsTableRowView => nsTableRowView -> IO CDouble
indentationForDropOperation nsTableRowView =
  sendMessage nsTableRowView indentationForDropOperationSelector

-- | @- setIndentationForDropOperation:@
setIndentationForDropOperation :: IsNSTableRowView nsTableRowView => nsTableRowView -> CDouble -> IO ()
setIndentationForDropOperation nsTableRowView value =
  sendMessage nsTableRowView setIndentationForDropOperationSelector value

-- | @- interiorBackgroundStyle@
interiorBackgroundStyle :: IsNSTableRowView nsTableRowView => nsTableRowView -> IO NSBackgroundStyle
interiorBackgroundStyle nsTableRowView =
  sendMessage nsTableRowView interiorBackgroundStyleSelector

-- | @- backgroundColor@
backgroundColor :: IsNSTableRowView nsTableRowView => nsTableRowView -> IO (Id NSColor)
backgroundColor nsTableRowView =
  sendMessage nsTableRowView backgroundColorSelector

-- | @- setBackgroundColor:@
setBackgroundColor :: (IsNSTableRowView nsTableRowView, IsNSColor value) => nsTableRowView -> value -> IO ()
setBackgroundColor nsTableRowView value =
  sendMessage nsTableRowView setBackgroundColorSelector (toNSColor value)

-- | @- numberOfColumns@
numberOfColumns :: IsNSTableRowView nsTableRowView => nsTableRowView -> IO CLong
numberOfColumns nsTableRowView =
  sendMessage nsTableRowView numberOfColumnsSelector

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @drawBackgroundInRect:@
drawBackgroundInRectSelector :: Selector '[NSRect] ()
drawBackgroundInRectSelector = mkSelector "drawBackgroundInRect:"

-- | @Selector@ for @drawSelectionInRect:@
drawSelectionInRectSelector :: Selector '[NSRect] ()
drawSelectionInRectSelector = mkSelector "drawSelectionInRect:"

-- | @Selector@ for @drawSeparatorInRect:@
drawSeparatorInRectSelector :: Selector '[NSRect] ()
drawSeparatorInRectSelector = mkSelector "drawSeparatorInRect:"

-- | @Selector@ for @drawDraggingDestinationFeedbackInRect:@
drawDraggingDestinationFeedbackInRectSelector :: Selector '[NSRect] ()
drawDraggingDestinationFeedbackInRectSelector = mkSelector "drawDraggingDestinationFeedbackInRect:"

-- | @Selector@ for @viewAtColumn:@
viewAtColumnSelector :: Selector '[CLong] RawId
viewAtColumnSelector = mkSelector "viewAtColumn:"

-- | @Selector@ for @selectionHighlightStyle@
selectionHighlightStyleSelector :: Selector '[] NSTableViewSelectionHighlightStyle
selectionHighlightStyleSelector = mkSelector "selectionHighlightStyle"

-- | @Selector@ for @setSelectionHighlightStyle:@
setSelectionHighlightStyleSelector :: Selector '[NSTableViewSelectionHighlightStyle] ()
setSelectionHighlightStyleSelector = mkSelector "setSelectionHighlightStyle:"

-- | @Selector@ for @emphasized@
emphasizedSelector :: Selector '[] Bool
emphasizedSelector = mkSelector "emphasized"

-- | @Selector@ for @setEmphasized:@
setEmphasizedSelector :: Selector '[Bool] ()
setEmphasizedSelector = mkSelector "setEmphasized:"

-- | @Selector@ for @groupRowStyle@
groupRowStyleSelector :: Selector '[] Bool
groupRowStyleSelector = mkSelector "groupRowStyle"

-- | @Selector@ for @setGroupRowStyle:@
setGroupRowStyleSelector :: Selector '[Bool] ()
setGroupRowStyleSelector = mkSelector "setGroupRowStyle:"

-- | @Selector@ for @selected@
selectedSelector :: Selector '[] Bool
selectedSelector = mkSelector "selected"

-- | @Selector@ for @setSelected:@
setSelectedSelector :: Selector '[Bool] ()
setSelectedSelector = mkSelector "setSelected:"

-- | @Selector@ for @previousRowSelected@
previousRowSelectedSelector :: Selector '[] Bool
previousRowSelectedSelector = mkSelector "previousRowSelected"

-- | @Selector@ for @setPreviousRowSelected:@
setPreviousRowSelectedSelector :: Selector '[Bool] ()
setPreviousRowSelectedSelector = mkSelector "setPreviousRowSelected:"

-- | @Selector@ for @nextRowSelected@
nextRowSelectedSelector :: Selector '[] Bool
nextRowSelectedSelector = mkSelector "nextRowSelected"

-- | @Selector@ for @setNextRowSelected:@
setNextRowSelectedSelector :: Selector '[Bool] ()
setNextRowSelectedSelector = mkSelector "setNextRowSelected:"

-- | @Selector@ for @floating@
floatingSelector :: Selector '[] Bool
floatingSelector = mkSelector "floating"

-- | @Selector@ for @setFloating:@
setFloatingSelector :: Selector '[Bool] ()
setFloatingSelector = mkSelector "setFloating:"

-- | @Selector@ for @targetForDropOperation@
targetForDropOperationSelector :: Selector '[] Bool
targetForDropOperationSelector = mkSelector "targetForDropOperation"

-- | @Selector@ for @setTargetForDropOperation:@
setTargetForDropOperationSelector :: Selector '[Bool] ()
setTargetForDropOperationSelector = mkSelector "setTargetForDropOperation:"

-- | @Selector@ for @draggingDestinationFeedbackStyle@
draggingDestinationFeedbackStyleSelector :: Selector '[] NSTableViewDraggingDestinationFeedbackStyle
draggingDestinationFeedbackStyleSelector = mkSelector "draggingDestinationFeedbackStyle"

-- | @Selector@ for @setDraggingDestinationFeedbackStyle:@
setDraggingDestinationFeedbackStyleSelector :: Selector '[NSTableViewDraggingDestinationFeedbackStyle] ()
setDraggingDestinationFeedbackStyleSelector = mkSelector "setDraggingDestinationFeedbackStyle:"

-- | @Selector@ for @indentationForDropOperation@
indentationForDropOperationSelector :: Selector '[] CDouble
indentationForDropOperationSelector = mkSelector "indentationForDropOperation"

-- | @Selector@ for @setIndentationForDropOperation:@
setIndentationForDropOperationSelector :: Selector '[CDouble] ()
setIndentationForDropOperationSelector = mkSelector "setIndentationForDropOperation:"

-- | @Selector@ for @interiorBackgroundStyle@
interiorBackgroundStyleSelector :: Selector '[] NSBackgroundStyle
interiorBackgroundStyleSelector = mkSelector "interiorBackgroundStyle"

-- | @Selector@ for @backgroundColor@
backgroundColorSelector :: Selector '[] (Id NSColor)
backgroundColorSelector = mkSelector "backgroundColor"

-- | @Selector@ for @setBackgroundColor:@
setBackgroundColorSelector :: Selector '[Id NSColor] ()
setBackgroundColorSelector = mkSelector "setBackgroundColor:"

-- | @Selector@ for @numberOfColumns@
numberOfColumnsSelector :: Selector '[] CLong
numberOfColumnsSelector = mkSelector "numberOfColumns"

