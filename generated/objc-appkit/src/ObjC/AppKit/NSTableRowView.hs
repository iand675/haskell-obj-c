{-# LANGUAGE PatternSynonyms #-}
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
  , drawBackgroundInRectSelector
  , drawSelectionInRectSelector
  , drawSeparatorInRectSelector
  , drawDraggingDestinationFeedbackInRectSelector
  , viewAtColumnSelector
  , selectionHighlightStyleSelector
  , setSelectionHighlightStyleSelector
  , emphasizedSelector
  , setEmphasizedSelector
  , groupRowStyleSelector
  , setGroupRowStyleSelector
  , selectedSelector
  , setSelectedSelector
  , previousRowSelectedSelector
  , setPreviousRowSelectedSelector
  , nextRowSelectedSelector
  , setNextRowSelectedSelector
  , floatingSelector
  , setFloatingSelector
  , targetForDropOperationSelector
  , setTargetForDropOperationSelector
  , draggingDestinationFeedbackStyleSelector
  , setDraggingDestinationFeedbackStyleSelector
  , indentationForDropOperationSelector
  , setIndentationForDropOperationSelector
  , interiorBackgroundStyleSelector
  , backgroundColorSelector
  , setBackgroundColorSelector
  , numberOfColumnsSelector

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

import ObjC.AppKit.Internal.Classes
import ObjC.Foundation.Internal.Structs
import ObjC.AppKit.Internal.Enums
import ObjC.Foundation.Internal.Classes

-- | @- drawBackgroundInRect:@
drawBackgroundInRect :: IsNSTableRowView nsTableRowView => nsTableRowView -> NSRect -> IO ()
drawBackgroundInRect nsTableRowView  dirtyRect =
  sendMsg nsTableRowView (mkSelector "drawBackgroundInRect:") retVoid [argNSRect dirtyRect]

-- | @- drawSelectionInRect:@
drawSelectionInRect :: IsNSTableRowView nsTableRowView => nsTableRowView -> NSRect -> IO ()
drawSelectionInRect nsTableRowView  dirtyRect =
  sendMsg nsTableRowView (mkSelector "drawSelectionInRect:") retVoid [argNSRect dirtyRect]

-- | @- drawSeparatorInRect:@
drawSeparatorInRect :: IsNSTableRowView nsTableRowView => nsTableRowView -> NSRect -> IO ()
drawSeparatorInRect nsTableRowView  dirtyRect =
  sendMsg nsTableRowView (mkSelector "drawSeparatorInRect:") retVoid [argNSRect dirtyRect]

-- | @- drawDraggingDestinationFeedbackInRect:@
drawDraggingDestinationFeedbackInRect :: IsNSTableRowView nsTableRowView => nsTableRowView -> NSRect -> IO ()
drawDraggingDestinationFeedbackInRect nsTableRowView  dirtyRect =
  sendMsg nsTableRowView (mkSelector "drawDraggingDestinationFeedbackInRect:") retVoid [argNSRect dirtyRect]

-- | @- viewAtColumn:@
viewAtColumn :: IsNSTableRowView nsTableRowView => nsTableRowView -> CLong -> IO RawId
viewAtColumn nsTableRowView  column =
  fmap (RawId . castPtr) $ sendMsg nsTableRowView (mkSelector "viewAtColumn:") (retPtr retVoid) [argCLong (fromIntegral column)]

-- | @- selectionHighlightStyle@
selectionHighlightStyle :: IsNSTableRowView nsTableRowView => nsTableRowView -> IO NSTableViewSelectionHighlightStyle
selectionHighlightStyle nsTableRowView  =
  fmap (coerce :: CLong -> NSTableViewSelectionHighlightStyle) $ sendMsg nsTableRowView (mkSelector "selectionHighlightStyle") retCLong []

-- | @- setSelectionHighlightStyle:@
setSelectionHighlightStyle :: IsNSTableRowView nsTableRowView => nsTableRowView -> NSTableViewSelectionHighlightStyle -> IO ()
setSelectionHighlightStyle nsTableRowView  value =
  sendMsg nsTableRowView (mkSelector "setSelectionHighlightStyle:") retVoid [argCLong (coerce value)]

-- | @- emphasized@
emphasized :: IsNSTableRowView nsTableRowView => nsTableRowView -> IO Bool
emphasized nsTableRowView  =
  fmap ((/= 0) :: CULong -> Bool) $ sendMsg nsTableRowView (mkSelector "emphasized") retCULong []

-- | @- setEmphasized:@
setEmphasized :: IsNSTableRowView nsTableRowView => nsTableRowView -> Bool -> IO ()
setEmphasized nsTableRowView  value =
  sendMsg nsTableRowView (mkSelector "setEmphasized:") retVoid [argCULong (if value then 1 else 0)]

-- | @- groupRowStyle@
groupRowStyle :: IsNSTableRowView nsTableRowView => nsTableRowView -> IO Bool
groupRowStyle nsTableRowView  =
  fmap ((/= 0) :: CULong -> Bool) $ sendMsg nsTableRowView (mkSelector "groupRowStyle") retCULong []

-- | @- setGroupRowStyle:@
setGroupRowStyle :: IsNSTableRowView nsTableRowView => nsTableRowView -> Bool -> IO ()
setGroupRowStyle nsTableRowView  value =
  sendMsg nsTableRowView (mkSelector "setGroupRowStyle:") retVoid [argCULong (if value then 1 else 0)]

-- | @- selected@
selected :: IsNSTableRowView nsTableRowView => nsTableRowView -> IO Bool
selected nsTableRowView  =
  fmap ((/= 0) :: CULong -> Bool) $ sendMsg nsTableRowView (mkSelector "selected") retCULong []

-- | @- setSelected:@
setSelected :: IsNSTableRowView nsTableRowView => nsTableRowView -> Bool -> IO ()
setSelected nsTableRowView  value =
  sendMsg nsTableRowView (mkSelector "setSelected:") retVoid [argCULong (if value then 1 else 0)]

-- | @- previousRowSelected@
previousRowSelected :: IsNSTableRowView nsTableRowView => nsTableRowView -> IO Bool
previousRowSelected nsTableRowView  =
  fmap ((/= 0) :: CULong -> Bool) $ sendMsg nsTableRowView (mkSelector "previousRowSelected") retCULong []

-- | @- setPreviousRowSelected:@
setPreviousRowSelected :: IsNSTableRowView nsTableRowView => nsTableRowView -> Bool -> IO ()
setPreviousRowSelected nsTableRowView  value =
  sendMsg nsTableRowView (mkSelector "setPreviousRowSelected:") retVoid [argCULong (if value then 1 else 0)]

-- | @- nextRowSelected@
nextRowSelected :: IsNSTableRowView nsTableRowView => nsTableRowView -> IO Bool
nextRowSelected nsTableRowView  =
  fmap ((/= 0) :: CULong -> Bool) $ sendMsg nsTableRowView (mkSelector "nextRowSelected") retCULong []

-- | @- setNextRowSelected:@
setNextRowSelected :: IsNSTableRowView nsTableRowView => nsTableRowView -> Bool -> IO ()
setNextRowSelected nsTableRowView  value =
  sendMsg nsTableRowView (mkSelector "setNextRowSelected:") retVoid [argCULong (if value then 1 else 0)]

-- | @- floating@
floating :: IsNSTableRowView nsTableRowView => nsTableRowView -> IO Bool
floating nsTableRowView  =
  fmap ((/= 0) :: CULong -> Bool) $ sendMsg nsTableRowView (mkSelector "floating") retCULong []

-- | @- setFloating:@
setFloating :: IsNSTableRowView nsTableRowView => nsTableRowView -> Bool -> IO ()
setFloating nsTableRowView  value =
  sendMsg nsTableRowView (mkSelector "setFloating:") retVoid [argCULong (if value then 1 else 0)]

-- | @- targetForDropOperation@
targetForDropOperation :: IsNSTableRowView nsTableRowView => nsTableRowView -> IO Bool
targetForDropOperation nsTableRowView  =
  fmap ((/= 0) :: CULong -> Bool) $ sendMsg nsTableRowView (mkSelector "targetForDropOperation") retCULong []

-- | @- setTargetForDropOperation:@
setTargetForDropOperation :: IsNSTableRowView nsTableRowView => nsTableRowView -> Bool -> IO ()
setTargetForDropOperation nsTableRowView  value =
  sendMsg nsTableRowView (mkSelector "setTargetForDropOperation:") retVoid [argCULong (if value then 1 else 0)]

-- | @- draggingDestinationFeedbackStyle@
draggingDestinationFeedbackStyle :: IsNSTableRowView nsTableRowView => nsTableRowView -> IO NSTableViewDraggingDestinationFeedbackStyle
draggingDestinationFeedbackStyle nsTableRowView  =
  fmap (coerce :: CLong -> NSTableViewDraggingDestinationFeedbackStyle) $ sendMsg nsTableRowView (mkSelector "draggingDestinationFeedbackStyle") retCLong []

-- | @- setDraggingDestinationFeedbackStyle:@
setDraggingDestinationFeedbackStyle :: IsNSTableRowView nsTableRowView => nsTableRowView -> NSTableViewDraggingDestinationFeedbackStyle -> IO ()
setDraggingDestinationFeedbackStyle nsTableRowView  value =
  sendMsg nsTableRowView (mkSelector "setDraggingDestinationFeedbackStyle:") retVoid [argCLong (coerce value)]

-- | @- indentationForDropOperation@
indentationForDropOperation :: IsNSTableRowView nsTableRowView => nsTableRowView -> IO CDouble
indentationForDropOperation nsTableRowView  =
  sendMsg nsTableRowView (mkSelector "indentationForDropOperation") retCDouble []

-- | @- setIndentationForDropOperation:@
setIndentationForDropOperation :: IsNSTableRowView nsTableRowView => nsTableRowView -> CDouble -> IO ()
setIndentationForDropOperation nsTableRowView  value =
  sendMsg nsTableRowView (mkSelector "setIndentationForDropOperation:") retVoid [argCDouble (fromIntegral value)]

-- | @- interiorBackgroundStyle@
interiorBackgroundStyle :: IsNSTableRowView nsTableRowView => nsTableRowView -> IO NSBackgroundStyle
interiorBackgroundStyle nsTableRowView  =
  fmap (coerce :: CLong -> NSBackgroundStyle) $ sendMsg nsTableRowView (mkSelector "interiorBackgroundStyle") retCLong []

-- | @- backgroundColor@
backgroundColor :: IsNSTableRowView nsTableRowView => nsTableRowView -> IO (Id NSColor)
backgroundColor nsTableRowView  =
  sendMsg nsTableRowView (mkSelector "backgroundColor") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- setBackgroundColor:@
setBackgroundColor :: (IsNSTableRowView nsTableRowView, IsNSColor value) => nsTableRowView -> value -> IO ()
setBackgroundColor nsTableRowView  value =
withObjCPtr value $ \raw_value ->
    sendMsg nsTableRowView (mkSelector "setBackgroundColor:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- | @- numberOfColumns@
numberOfColumns :: IsNSTableRowView nsTableRowView => nsTableRowView -> IO CLong
numberOfColumns nsTableRowView  =
  sendMsg nsTableRowView (mkSelector "numberOfColumns") retCLong []

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @drawBackgroundInRect:@
drawBackgroundInRectSelector :: Selector
drawBackgroundInRectSelector = mkSelector "drawBackgroundInRect:"

-- | @Selector@ for @drawSelectionInRect:@
drawSelectionInRectSelector :: Selector
drawSelectionInRectSelector = mkSelector "drawSelectionInRect:"

-- | @Selector@ for @drawSeparatorInRect:@
drawSeparatorInRectSelector :: Selector
drawSeparatorInRectSelector = mkSelector "drawSeparatorInRect:"

-- | @Selector@ for @drawDraggingDestinationFeedbackInRect:@
drawDraggingDestinationFeedbackInRectSelector :: Selector
drawDraggingDestinationFeedbackInRectSelector = mkSelector "drawDraggingDestinationFeedbackInRect:"

-- | @Selector@ for @viewAtColumn:@
viewAtColumnSelector :: Selector
viewAtColumnSelector = mkSelector "viewAtColumn:"

-- | @Selector@ for @selectionHighlightStyle@
selectionHighlightStyleSelector :: Selector
selectionHighlightStyleSelector = mkSelector "selectionHighlightStyle"

-- | @Selector@ for @setSelectionHighlightStyle:@
setSelectionHighlightStyleSelector :: Selector
setSelectionHighlightStyleSelector = mkSelector "setSelectionHighlightStyle:"

-- | @Selector@ for @emphasized@
emphasizedSelector :: Selector
emphasizedSelector = mkSelector "emphasized"

-- | @Selector@ for @setEmphasized:@
setEmphasizedSelector :: Selector
setEmphasizedSelector = mkSelector "setEmphasized:"

-- | @Selector@ for @groupRowStyle@
groupRowStyleSelector :: Selector
groupRowStyleSelector = mkSelector "groupRowStyle"

-- | @Selector@ for @setGroupRowStyle:@
setGroupRowStyleSelector :: Selector
setGroupRowStyleSelector = mkSelector "setGroupRowStyle:"

-- | @Selector@ for @selected@
selectedSelector :: Selector
selectedSelector = mkSelector "selected"

-- | @Selector@ for @setSelected:@
setSelectedSelector :: Selector
setSelectedSelector = mkSelector "setSelected:"

-- | @Selector@ for @previousRowSelected@
previousRowSelectedSelector :: Selector
previousRowSelectedSelector = mkSelector "previousRowSelected"

-- | @Selector@ for @setPreviousRowSelected:@
setPreviousRowSelectedSelector :: Selector
setPreviousRowSelectedSelector = mkSelector "setPreviousRowSelected:"

-- | @Selector@ for @nextRowSelected@
nextRowSelectedSelector :: Selector
nextRowSelectedSelector = mkSelector "nextRowSelected"

-- | @Selector@ for @setNextRowSelected:@
setNextRowSelectedSelector :: Selector
setNextRowSelectedSelector = mkSelector "setNextRowSelected:"

-- | @Selector@ for @floating@
floatingSelector :: Selector
floatingSelector = mkSelector "floating"

-- | @Selector@ for @setFloating:@
setFloatingSelector :: Selector
setFloatingSelector = mkSelector "setFloating:"

-- | @Selector@ for @targetForDropOperation@
targetForDropOperationSelector :: Selector
targetForDropOperationSelector = mkSelector "targetForDropOperation"

-- | @Selector@ for @setTargetForDropOperation:@
setTargetForDropOperationSelector :: Selector
setTargetForDropOperationSelector = mkSelector "setTargetForDropOperation:"

-- | @Selector@ for @draggingDestinationFeedbackStyle@
draggingDestinationFeedbackStyleSelector :: Selector
draggingDestinationFeedbackStyleSelector = mkSelector "draggingDestinationFeedbackStyle"

-- | @Selector@ for @setDraggingDestinationFeedbackStyle:@
setDraggingDestinationFeedbackStyleSelector :: Selector
setDraggingDestinationFeedbackStyleSelector = mkSelector "setDraggingDestinationFeedbackStyle:"

-- | @Selector@ for @indentationForDropOperation@
indentationForDropOperationSelector :: Selector
indentationForDropOperationSelector = mkSelector "indentationForDropOperation"

-- | @Selector@ for @setIndentationForDropOperation:@
setIndentationForDropOperationSelector :: Selector
setIndentationForDropOperationSelector = mkSelector "setIndentationForDropOperation:"

-- | @Selector@ for @interiorBackgroundStyle@
interiorBackgroundStyleSelector :: Selector
interiorBackgroundStyleSelector = mkSelector "interiorBackgroundStyle"

-- | @Selector@ for @backgroundColor@
backgroundColorSelector :: Selector
backgroundColorSelector = mkSelector "backgroundColor"

-- | @Selector@ for @setBackgroundColor:@
setBackgroundColorSelector :: Selector
setBackgroundColorSelector = mkSelector "setBackgroundColor:"

-- | @Selector@ for @numberOfColumns@
numberOfColumnsSelector :: Selector
numberOfColumnsSelector = mkSelector "numberOfColumns"

