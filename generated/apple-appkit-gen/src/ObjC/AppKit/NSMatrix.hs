{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Generated bindings for @NSMatrix@.
module ObjC.AppKit.NSMatrix
  ( NSMatrix
  , IsNSMatrix(..)
  , initWithFrame
  , initWithFrame_mode_prototype_numberOfRows_numberOfColumns
  , initWithFrame_mode_cellClass_numberOfRows_numberOfColumns
  , makeCellAtRow_column
  , sendAction_to_forAllCells
  , sortUsingSelector
  , sortUsingFunction_context
  , setSelectionFrom_to_anchor_highlight
  , deselectSelectedCell
  , deselectAllCells
  , selectCellAtRow_column
  , selectAll
  , selectCellWithTag
  , setScrollable
  , setState_atRow_column
  , getNumberOfRows_columns
  , cellAtRow_column
  , cellFrameAtRow_column
  , getRow_column_ofCell
  , getRow_column_forPoint
  , renewRows_columns
  , putCell_atRow_column
  , addRow
  , addRowWithCells
  , insertRow
  , insertRow_withCells
  , removeRow
  , addColumn
  , addColumnWithCells
  , insertColumn
  , insertColumn_withCells
  , removeColumn
  , cellWithTag
  , sizeToCells
  , setValidateSize
  , drawCellAtRow_column
  , highlightCell_atRow_column
  , scrollCellToVisibleAtRow_column
  , mouseDown
  , performKeyEquivalent
  , sendAction
  , sendDoubleAction
  , textShouldBeginEditing
  , textShouldEndEditing
  , textDidBeginEditing
  , textDidEndEditing
  , textDidChange
  , selectText
  , selectTextAtRow_column
  , acceptsFirstMouse
  , resetCursorRects
  , setToolTip_forCell
  , toolTipForCell
  , cellClass
  , setCellClass
  , prototype
  , setPrototype
  , mode
  , setMode
  , allowsEmptySelection
  , setAllowsEmptySelection
  , cells
  , selectedCell
  , selectedCells
  , selectedRow
  , selectedColumn
  , selectionByRect
  , setSelectionByRect
  , cellSize
  , setCellSize
  , intercellSpacing
  , setIntercellSpacing
  , backgroundColor
  , setBackgroundColor
  , cellBackgroundColor
  , setCellBackgroundColor
  , drawsCellBackground
  , setDrawsCellBackground
  , drawsBackground
  , setDrawsBackground
  , numberOfRows
  , numberOfColumns
  , doubleAction
  , setDoubleAction
  , autosizesCells
  , setAutosizesCells
  , autoscroll
  , setAutoscroll
  , mouseDownFlags
  , delegate
  , setDelegate
  , autorecalculatesCellSize
  , setAutorecalculatesCellSize
  , tabKeyTraversesCells
  , setTabKeyTraversesCells
  , keyCell
  , setKeyCell
  , initWithFrameSelector
  , initWithFrame_mode_prototype_numberOfRows_numberOfColumnsSelector
  , initWithFrame_mode_cellClass_numberOfRows_numberOfColumnsSelector
  , makeCellAtRow_columnSelector
  , sendAction_to_forAllCellsSelector
  , sortUsingSelectorSelector
  , sortUsingFunction_contextSelector
  , setSelectionFrom_to_anchor_highlightSelector
  , deselectSelectedCellSelector
  , deselectAllCellsSelector
  , selectCellAtRow_columnSelector
  , selectAllSelector
  , selectCellWithTagSelector
  , setScrollableSelector
  , setState_atRow_columnSelector
  , getNumberOfRows_columnsSelector
  , cellAtRow_columnSelector
  , cellFrameAtRow_columnSelector
  , getRow_column_ofCellSelector
  , getRow_column_forPointSelector
  , renewRows_columnsSelector
  , putCell_atRow_columnSelector
  , addRowSelector
  , addRowWithCellsSelector
  , insertRowSelector
  , insertRow_withCellsSelector
  , removeRowSelector
  , addColumnSelector
  , addColumnWithCellsSelector
  , insertColumnSelector
  , insertColumn_withCellsSelector
  , removeColumnSelector
  , cellWithTagSelector
  , sizeToCellsSelector
  , setValidateSizeSelector
  , drawCellAtRow_columnSelector
  , highlightCell_atRow_columnSelector
  , scrollCellToVisibleAtRow_columnSelector
  , mouseDownSelector
  , performKeyEquivalentSelector
  , sendActionSelector
  , sendDoubleActionSelector
  , textShouldBeginEditingSelector
  , textShouldEndEditingSelector
  , textDidBeginEditingSelector
  , textDidEndEditingSelector
  , textDidChangeSelector
  , selectTextSelector
  , selectTextAtRow_columnSelector
  , acceptsFirstMouseSelector
  , resetCursorRectsSelector
  , setToolTip_forCellSelector
  , toolTipForCellSelector
  , cellClassSelector
  , setCellClassSelector
  , prototypeSelector
  , setPrototypeSelector
  , modeSelector
  , setModeSelector
  , allowsEmptySelectionSelector
  , setAllowsEmptySelectionSelector
  , cellsSelector
  , selectedCellSelector
  , selectedCellsSelector
  , selectedRowSelector
  , selectedColumnSelector
  , selectionByRectSelector
  , setSelectionByRectSelector
  , cellSizeSelector
  , setCellSizeSelector
  , intercellSpacingSelector
  , setIntercellSpacingSelector
  , backgroundColorSelector
  , setBackgroundColorSelector
  , cellBackgroundColorSelector
  , setCellBackgroundColorSelector
  , drawsCellBackgroundSelector
  , setDrawsCellBackgroundSelector
  , drawsBackgroundSelector
  , setDrawsBackgroundSelector
  , numberOfRowsSelector
  , numberOfColumnsSelector
  , doubleActionSelector
  , setDoubleActionSelector
  , autosizesCellsSelector
  , setAutosizesCellsSelector
  , autoscrollSelector
  , setAutoscrollSelector
  , mouseDownFlagsSelector
  , delegateSelector
  , setDelegateSelector
  , autorecalculatesCellSizeSelector
  , setAutorecalculatesCellSizeSelector
  , tabKeyTraversesCellsSelector
  , setTabKeyTraversesCellsSelector
  , keyCellSelector
  , setKeyCellSelector

  -- * Enum types
  , NSMatrixMode(NSMatrixMode)
  , pattern NSRadioModeMatrix
  , pattern NSHighlightModeMatrix
  , pattern NSListModeMatrix
  , pattern NSTrackModeMatrix

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
initWithFrame :: IsNSMatrix nsMatrix => nsMatrix -> NSRect -> IO (Id NSMatrix)
initWithFrame nsMatrix  frameRect =
    sendMsg nsMatrix (mkSelector "initWithFrame:") (retPtr retVoid) [argNSRect frameRect] >>= ownedObject . castPtr

-- | @- initWithFrame:mode:prototype:numberOfRows:numberOfColumns:@
initWithFrame_mode_prototype_numberOfRows_numberOfColumns :: (IsNSMatrix nsMatrix, IsNSCell cell) => nsMatrix -> NSRect -> NSMatrixMode -> cell -> CLong -> CLong -> IO (Id NSMatrix)
initWithFrame_mode_prototype_numberOfRows_numberOfColumns nsMatrix  frameRect mode cell rowsHigh colsWide =
  withObjCPtr cell $ \raw_cell ->
      sendMsg nsMatrix (mkSelector "initWithFrame:mode:prototype:numberOfRows:numberOfColumns:") (retPtr retVoid) [argNSRect frameRect, argCULong (coerce mode), argPtr (castPtr raw_cell :: Ptr ()), argCLong rowsHigh, argCLong colsWide] >>= ownedObject . castPtr

-- | @- initWithFrame:mode:cellClass:numberOfRows:numberOfColumns:@
initWithFrame_mode_cellClass_numberOfRows_numberOfColumns :: IsNSMatrix nsMatrix => nsMatrix -> NSRect -> NSMatrixMode -> Class -> CLong -> CLong -> IO (Id NSMatrix)
initWithFrame_mode_cellClass_numberOfRows_numberOfColumns nsMatrix  frameRect mode factoryId rowsHigh colsWide =
    sendMsg nsMatrix (mkSelector "initWithFrame:mode:cellClass:numberOfRows:numberOfColumns:") (retPtr retVoid) [argNSRect frameRect, argCULong (coerce mode), argPtr (unClass factoryId), argCLong rowsHigh, argCLong colsWide] >>= ownedObject . castPtr

-- | @- makeCellAtRow:column:@
makeCellAtRow_column :: IsNSMatrix nsMatrix => nsMatrix -> CLong -> CLong -> IO (Id NSCell)
makeCellAtRow_column nsMatrix  row col =
    sendMsg nsMatrix (mkSelector "makeCellAtRow:column:") (retPtr retVoid) [argCLong row, argCLong col] >>= retainedObject . castPtr

-- | @- sendAction:to:forAllCells:@
sendAction_to_forAllCells :: IsNSMatrix nsMatrix => nsMatrix -> Selector -> RawId -> Bool -> IO ()
sendAction_to_forAllCells nsMatrix  selector object flag =
    sendMsg nsMatrix (mkSelector "sendAction:to:forAllCells:") retVoid [argPtr (unSelector selector), argPtr (castPtr (unRawId object) :: Ptr ()), argCULong (if flag then 1 else 0)]

-- | @- sortUsingSelector:@
sortUsingSelector :: IsNSMatrix nsMatrix => nsMatrix -> Selector -> IO ()
sortUsingSelector nsMatrix  comparator =
    sendMsg nsMatrix (mkSelector "sortUsingSelector:") retVoid [argPtr (unSelector comparator)]

-- | @- sortUsingFunction:context:@
sortUsingFunction_context :: IsNSMatrix nsMatrix => nsMatrix -> Ptr () -> Ptr () -> IO ()
sortUsingFunction_context nsMatrix  compare_ context =
    sendMsg nsMatrix (mkSelector "sortUsingFunction:context:") retVoid [argPtr compare_, argPtr context]

-- | @- setSelectionFrom:to:anchor:highlight:@
setSelectionFrom_to_anchor_highlight :: IsNSMatrix nsMatrix => nsMatrix -> CLong -> CLong -> CLong -> Bool -> IO ()
setSelectionFrom_to_anchor_highlight nsMatrix  startPos endPos anchorPos lit =
    sendMsg nsMatrix (mkSelector "setSelectionFrom:to:anchor:highlight:") retVoid [argCLong startPos, argCLong endPos, argCLong anchorPos, argCULong (if lit then 1 else 0)]

-- | @- deselectSelectedCell@
deselectSelectedCell :: IsNSMatrix nsMatrix => nsMatrix -> IO ()
deselectSelectedCell nsMatrix  =
    sendMsg nsMatrix (mkSelector "deselectSelectedCell") retVoid []

-- | @- deselectAllCells@
deselectAllCells :: IsNSMatrix nsMatrix => nsMatrix -> IO ()
deselectAllCells nsMatrix  =
    sendMsg nsMatrix (mkSelector "deselectAllCells") retVoid []

-- | @- selectCellAtRow:column:@
selectCellAtRow_column :: IsNSMatrix nsMatrix => nsMatrix -> CLong -> CLong -> IO ()
selectCellAtRow_column nsMatrix  row col =
    sendMsg nsMatrix (mkSelector "selectCellAtRow:column:") retVoid [argCLong row, argCLong col]

-- | @- selectAll:@
selectAll :: IsNSMatrix nsMatrix => nsMatrix -> RawId -> IO ()
selectAll nsMatrix  sender =
    sendMsg nsMatrix (mkSelector "selectAll:") retVoid [argPtr (castPtr (unRawId sender) :: Ptr ())]

-- | @- selectCellWithTag:@
selectCellWithTag :: IsNSMatrix nsMatrix => nsMatrix -> CLong -> IO Bool
selectCellWithTag nsMatrix  tag =
    fmap ((/= 0) :: CULong -> Bool) $ sendMsg nsMatrix (mkSelector "selectCellWithTag:") retCULong [argCLong tag]

-- | @- setScrollable:@
setScrollable :: IsNSMatrix nsMatrix => nsMatrix -> Bool -> IO ()
setScrollable nsMatrix  flag =
    sendMsg nsMatrix (mkSelector "setScrollable:") retVoid [argCULong (if flag then 1 else 0)]

-- | @- setState:atRow:column:@
setState_atRow_column :: IsNSMatrix nsMatrix => nsMatrix -> CLong -> CLong -> CLong -> IO ()
setState_atRow_column nsMatrix  value row col =
    sendMsg nsMatrix (mkSelector "setState:atRow:column:") retVoid [argCLong value, argCLong row, argCLong col]

-- | @- getNumberOfRows:columns:@
getNumberOfRows_columns :: IsNSMatrix nsMatrix => nsMatrix -> Ptr CLong -> Ptr CLong -> IO ()
getNumberOfRows_columns nsMatrix  rowCount colCount =
    sendMsg nsMatrix (mkSelector "getNumberOfRows:columns:") retVoid [argPtr rowCount, argPtr colCount]

-- | @- cellAtRow:column:@
cellAtRow_column :: IsNSMatrix nsMatrix => nsMatrix -> CLong -> CLong -> IO (Id NSCell)
cellAtRow_column nsMatrix  row col =
    sendMsg nsMatrix (mkSelector "cellAtRow:column:") (retPtr retVoid) [argCLong row, argCLong col] >>= retainedObject . castPtr

-- | @- cellFrameAtRow:column:@
cellFrameAtRow_column :: IsNSMatrix nsMatrix => nsMatrix -> CLong -> CLong -> IO NSRect
cellFrameAtRow_column nsMatrix  row col =
    sendMsgStret nsMatrix (mkSelector "cellFrameAtRow:column:") retNSRect [argCLong row, argCLong col]

-- | @- getRow:column:ofCell:@
getRow_column_ofCell :: (IsNSMatrix nsMatrix, IsNSCell cell) => nsMatrix -> Ptr CLong -> Ptr CLong -> cell -> IO Bool
getRow_column_ofCell nsMatrix  row col cell =
  withObjCPtr cell $ \raw_cell ->
      fmap ((/= 0) :: CULong -> Bool) $ sendMsg nsMatrix (mkSelector "getRow:column:ofCell:") retCULong [argPtr row, argPtr col, argPtr (castPtr raw_cell :: Ptr ())]

-- | @- getRow:column:forPoint:@
getRow_column_forPoint :: IsNSMatrix nsMatrix => nsMatrix -> Ptr CLong -> Ptr CLong -> NSPoint -> IO Bool
getRow_column_forPoint nsMatrix  row col point =
    fmap ((/= 0) :: CULong -> Bool) $ sendMsg nsMatrix (mkSelector "getRow:column:forPoint:") retCULong [argPtr row, argPtr col, argNSPoint point]

-- | @- renewRows:columns:@
renewRows_columns :: IsNSMatrix nsMatrix => nsMatrix -> CLong -> CLong -> IO ()
renewRows_columns nsMatrix  newRows newCols =
    sendMsg nsMatrix (mkSelector "renewRows:columns:") retVoid [argCLong newRows, argCLong newCols]

-- | @- putCell:atRow:column:@
putCell_atRow_column :: (IsNSMatrix nsMatrix, IsNSCell newCell) => nsMatrix -> newCell -> CLong -> CLong -> IO ()
putCell_atRow_column nsMatrix  newCell row col =
  withObjCPtr newCell $ \raw_newCell ->
      sendMsg nsMatrix (mkSelector "putCell:atRow:column:") retVoid [argPtr (castPtr raw_newCell :: Ptr ()), argCLong row, argCLong col]

-- | @- addRow@
addRow :: IsNSMatrix nsMatrix => nsMatrix -> IO ()
addRow nsMatrix  =
    sendMsg nsMatrix (mkSelector "addRow") retVoid []

-- | @- addRowWithCells:@
addRowWithCells :: (IsNSMatrix nsMatrix, IsNSArray newCells) => nsMatrix -> newCells -> IO ()
addRowWithCells nsMatrix  newCells =
  withObjCPtr newCells $ \raw_newCells ->
      sendMsg nsMatrix (mkSelector "addRowWithCells:") retVoid [argPtr (castPtr raw_newCells :: Ptr ())]

-- | @- insertRow:@
insertRow :: IsNSMatrix nsMatrix => nsMatrix -> CLong -> IO ()
insertRow nsMatrix  row =
    sendMsg nsMatrix (mkSelector "insertRow:") retVoid [argCLong row]

-- | @- insertRow:withCells:@
insertRow_withCells :: (IsNSMatrix nsMatrix, IsNSArray newCells) => nsMatrix -> CLong -> newCells -> IO ()
insertRow_withCells nsMatrix  row newCells =
  withObjCPtr newCells $ \raw_newCells ->
      sendMsg nsMatrix (mkSelector "insertRow:withCells:") retVoid [argCLong row, argPtr (castPtr raw_newCells :: Ptr ())]

-- | @- removeRow:@
removeRow :: IsNSMatrix nsMatrix => nsMatrix -> CLong -> IO ()
removeRow nsMatrix  row =
    sendMsg nsMatrix (mkSelector "removeRow:") retVoid [argCLong row]

-- | @- addColumn@
addColumn :: IsNSMatrix nsMatrix => nsMatrix -> IO ()
addColumn nsMatrix  =
    sendMsg nsMatrix (mkSelector "addColumn") retVoid []

-- | @- addColumnWithCells:@
addColumnWithCells :: (IsNSMatrix nsMatrix, IsNSArray newCells) => nsMatrix -> newCells -> IO ()
addColumnWithCells nsMatrix  newCells =
  withObjCPtr newCells $ \raw_newCells ->
      sendMsg nsMatrix (mkSelector "addColumnWithCells:") retVoid [argPtr (castPtr raw_newCells :: Ptr ())]

-- | @- insertColumn:@
insertColumn :: IsNSMatrix nsMatrix => nsMatrix -> CLong -> IO ()
insertColumn nsMatrix  column =
    sendMsg nsMatrix (mkSelector "insertColumn:") retVoid [argCLong column]

-- | @- insertColumn:withCells:@
insertColumn_withCells :: (IsNSMatrix nsMatrix, IsNSArray newCells) => nsMatrix -> CLong -> newCells -> IO ()
insertColumn_withCells nsMatrix  column newCells =
  withObjCPtr newCells $ \raw_newCells ->
      sendMsg nsMatrix (mkSelector "insertColumn:withCells:") retVoid [argCLong column, argPtr (castPtr raw_newCells :: Ptr ())]

-- | @- removeColumn:@
removeColumn :: IsNSMatrix nsMatrix => nsMatrix -> CLong -> IO ()
removeColumn nsMatrix  col =
    sendMsg nsMatrix (mkSelector "removeColumn:") retVoid [argCLong col]

-- | @- cellWithTag:@
cellWithTag :: IsNSMatrix nsMatrix => nsMatrix -> CLong -> IO (Id NSCell)
cellWithTag nsMatrix  tag =
    sendMsg nsMatrix (mkSelector "cellWithTag:") (retPtr retVoid) [argCLong tag] >>= retainedObject . castPtr

-- | @- sizeToCells@
sizeToCells :: IsNSMatrix nsMatrix => nsMatrix -> IO ()
sizeToCells nsMatrix  =
    sendMsg nsMatrix (mkSelector "sizeToCells") retVoid []

-- | @- setValidateSize:@
setValidateSize :: IsNSMatrix nsMatrix => nsMatrix -> Bool -> IO ()
setValidateSize nsMatrix  flag =
    sendMsg nsMatrix (mkSelector "setValidateSize:") retVoid [argCULong (if flag then 1 else 0)]

-- | @- drawCellAtRow:column:@
drawCellAtRow_column :: IsNSMatrix nsMatrix => nsMatrix -> CLong -> CLong -> IO ()
drawCellAtRow_column nsMatrix  row col =
    sendMsg nsMatrix (mkSelector "drawCellAtRow:column:") retVoid [argCLong row, argCLong col]

-- | @- highlightCell:atRow:column:@
highlightCell_atRow_column :: IsNSMatrix nsMatrix => nsMatrix -> Bool -> CLong -> CLong -> IO ()
highlightCell_atRow_column nsMatrix  flag row col =
    sendMsg nsMatrix (mkSelector "highlightCell:atRow:column:") retVoid [argCULong (if flag then 1 else 0), argCLong row, argCLong col]

-- | @- scrollCellToVisibleAtRow:column:@
scrollCellToVisibleAtRow_column :: IsNSMatrix nsMatrix => nsMatrix -> CLong -> CLong -> IO ()
scrollCellToVisibleAtRow_column nsMatrix  row col =
    sendMsg nsMatrix (mkSelector "scrollCellToVisibleAtRow:column:") retVoid [argCLong row, argCLong col]

-- | @- mouseDown:@
mouseDown :: (IsNSMatrix nsMatrix, IsNSEvent event) => nsMatrix -> event -> IO ()
mouseDown nsMatrix  event =
  withObjCPtr event $ \raw_event ->
      sendMsg nsMatrix (mkSelector "mouseDown:") retVoid [argPtr (castPtr raw_event :: Ptr ())]

-- | @- performKeyEquivalent:@
performKeyEquivalent :: (IsNSMatrix nsMatrix, IsNSEvent event) => nsMatrix -> event -> IO Bool
performKeyEquivalent nsMatrix  event =
  withObjCPtr event $ \raw_event ->
      fmap ((/= 0) :: CULong -> Bool) $ sendMsg nsMatrix (mkSelector "performKeyEquivalent:") retCULong [argPtr (castPtr raw_event :: Ptr ())]

-- | @- sendAction@
sendAction :: IsNSMatrix nsMatrix => nsMatrix -> IO Bool
sendAction nsMatrix  =
    fmap ((/= 0) :: CULong -> Bool) $ sendMsg nsMatrix (mkSelector "sendAction") retCULong []

-- | @- sendDoubleAction@
sendDoubleAction :: IsNSMatrix nsMatrix => nsMatrix -> IO ()
sendDoubleAction nsMatrix  =
    sendMsg nsMatrix (mkSelector "sendDoubleAction") retVoid []

-- | @- textShouldBeginEditing:@
textShouldBeginEditing :: (IsNSMatrix nsMatrix, IsNSText textObject) => nsMatrix -> textObject -> IO Bool
textShouldBeginEditing nsMatrix  textObject =
  withObjCPtr textObject $ \raw_textObject ->
      fmap ((/= 0) :: CULong -> Bool) $ sendMsg nsMatrix (mkSelector "textShouldBeginEditing:") retCULong [argPtr (castPtr raw_textObject :: Ptr ())]

-- | @- textShouldEndEditing:@
textShouldEndEditing :: (IsNSMatrix nsMatrix, IsNSText textObject) => nsMatrix -> textObject -> IO Bool
textShouldEndEditing nsMatrix  textObject =
  withObjCPtr textObject $ \raw_textObject ->
      fmap ((/= 0) :: CULong -> Bool) $ sendMsg nsMatrix (mkSelector "textShouldEndEditing:") retCULong [argPtr (castPtr raw_textObject :: Ptr ())]

-- | @- textDidBeginEditing:@
textDidBeginEditing :: (IsNSMatrix nsMatrix, IsNSNotification notification) => nsMatrix -> notification -> IO ()
textDidBeginEditing nsMatrix  notification =
  withObjCPtr notification $ \raw_notification ->
      sendMsg nsMatrix (mkSelector "textDidBeginEditing:") retVoid [argPtr (castPtr raw_notification :: Ptr ())]

-- | @- textDidEndEditing:@
textDidEndEditing :: (IsNSMatrix nsMatrix, IsNSNotification notification) => nsMatrix -> notification -> IO ()
textDidEndEditing nsMatrix  notification =
  withObjCPtr notification $ \raw_notification ->
      sendMsg nsMatrix (mkSelector "textDidEndEditing:") retVoid [argPtr (castPtr raw_notification :: Ptr ())]

-- | @- textDidChange:@
textDidChange :: (IsNSMatrix nsMatrix, IsNSNotification notification) => nsMatrix -> notification -> IO ()
textDidChange nsMatrix  notification =
  withObjCPtr notification $ \raw_notification ->
      sendMsg nsMatrix (mkSelector "textDidChange:") retVoid [argPtr (castPtr raw_notification :: Ptr ())]

-- | @- selectText:@
selectText :: IsNSMatrix nsMatrix => nsMatrix -> RawId -> IO ()
selectText nsMatrix  sender =
    sendMsg nsMatrix (mkSelector "selectText:") retVoid [argPtr (castPtr (unRawId sender) :: Ptr ())]

-- | @- selectTextAtRow:column:@
selectTextAtRow_column :: IsNSMatrix nsMatrix => nsMatrix -> CLong -> CLong -> IO (Id NSCell)
selectTextAtRow_column nsMatrix  row col =
    sendMsg nsMatrix (mkSelector "selectTextAtRow:column:") (retPtr retVoid) [argCLong row, argCLong col] >>= retainedObject . castPtr

-- | @- acceptsFirstMouse:@
acceptsFirstMouse :: (IsNSMatrix nsMatrix, IsNSEvent event) => nsMatrix -> event -> IO Bool
acceptsFirstMouse nsMatrix  event =
  withObjCPtr event $ \raw_event ->
      fmap ((/= 0) :: CULong -> Bool) $ sendMsg nsMatrix (mkSelector "acceptsFirstMouse:") retCULong [argPtr (castPtr raw_event :: Ptr ())]

-- | @- resetCursorRects@
resetCursorRects :: IsNSMatrix nsMatrix => nsMatrix -> IO ()
resetCursorRects nsMatrix  =
    sendMsg nsMatrix (mkSelector "resetCursorRects") retVoid []

-- | @- setToolTip:forCell:@
setToolTip_forCell :: (IsNSMatrix nsMatrix, IsNSString toolTipString, IsNSCell cell) => nsMatrix -> toolTipString -> cell -> IO ()
setToolTip_forCell nsMatrix  toolTipString cell =
  withObjCPtr toolTipString $ \raw_toolTipString ->
    withObjCPtr cell $ \raw_cell ->
        sendMsg nsMatrix (mkSelector "setToolTip:forCell:") retVoid [argPtr (castPtr raw_toolTipString :: Ptr ()), argPtr (castPtr raw_cell :: Ptr ())]

-- | @- toolTipForCell:@
toolTipForCell :: (IsNSMatrix nsMatrix, IsNSCell cell) => nsMatrix -> cell -> IO (Id NSString)
toolTipForCell nsMatrix  cell =
  withObjCPtr cell $ \raw_cell ->
      sendMsg nsMatrix (mkSelector "toolTipForCell:") (retPtr retVoid) [argPtr (castPtr raw_cell :: Ptr ())] >>= retainedObject . castPtr

-- | @- cellClass@
cellClass :: IsNSMatrix nsMatrix => nsMatrix -> IO Class
cellClass nsMatrix  =
    fmap (Class . castPtr) $ sendMsg nsMatrix (mkSelector "cellClass") (retPtr retVoid) []

-- | @- setCellClass:@
setCellClass :: IsNSMatrix nsMatrix => nsMatrix -> Class -> IO ()
setCellClass nsMatrix  value =
    sendMsg nsMatrix (mkSelector "setCellClass:") retVoid [argPtr (unClass value)]

-- | @- prototype@
prototype :: IsNSMatrix nsMatrix => nsMatrix -> IO (Id NSCell)
prototype nsMatrix  =
    sendMsg nsMatrix (mkSelector "prototype") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- setPrototype:@
setPrototype :: (IsNSMatrix nsMatrix, IsNSCell value) => nsMatrix -> value -> IO ()
setPrototype nsMatrix  value =
  withObjCPtr value $ \raw_value ->
      sendMsg nsMatrix (mkSelector "setPrototype:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- | @- mode@
mode :: IsNSMatrix nsMatrix => nsMatrix -> IO NSMatrixMode
mode nsMatrix  =
    fmap (coerce :: CULong -> NSMatrixMode) $ sendMsg nsMatrix (mkSelector "mode") retCULong []

-- | @- setMode:@
setMode :: IsNSMatrix nsMatrix => nsMatrix -> NSMatrixMode -> IO ()
setMode nsMatrix  value =
    sendMsg nsMatrix (mkSelector "setMode:") retVoid [argCULong (coerce value)]

-- | @- allowsEmptySelection@
allowsEmptySelection :: IsNSMatrix nsMatrix => nsMatrix -> IO Bool
allowsEmptySelection nsMatrix  =
    fmap ((/= 0) :: CULong -> Bool) $ sendMsg nsMatrix (mkSelector "allowsEmptySelection") retCULong []

-- | @- setAllowsEmptySelection:@
setAllowsEmptySelection :: IsNSMatrix nsMatrix => nsMatrix -> Bool -> IO ()
setAllowsEmptySelection nsMatrix  value =
    sendMsg nsMatrix (mkSelector "setAllowsEmptySelection:") retVoid [argCULong (if value then 1 else 0)]

-- | @- cells@
cells :: IsNSMatrix nsMatrix => nsMatrix -> IO (Id NSArray)
cells nsMatrix  =
    sendMsg nsMatrix (mkSelector "cells") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- selectedCell@
selectedCell :: IsNSMatrix nsMatrix => nsMatrix -> IO (Id NSCell)
selectedCell nsMatrix  =
    sendMsg nsMatrix (mkSelector "selectedCell") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- selectedCells@
selectedCells :: IsNSMatrix nsMatrix => nsMatrix -> IO (Id NSArray)
selectedCells nsMatrix  =
    sendMsg nsMatrix (mkSelector "selectedCells") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- selectedRow@
selectedRow :: IsNSMatrix nsMatrix => nsMatrix -> IO CLong
selectedRow nsMatrix  =
    sendMsg nsMatrix (mkSelector "selectedRow") retCLong []

-- | @- selectedColumn@
selectedColumn :: IsNSMatrix nsMatrix => nsMatrix -> IO CLong
selectedColumn nsMatrix  =
    sendMsg nsMatrix (mkSelector "selectedColumn") retCLong []

-- | @- selectionByRect@
selectionByRect :: IsNSMatrix nsMatrix => nsMatrix -> IO Bool
selectionByRect nsMatrix  =
    fmap ((/= 0) :: CULong -> Bool) $ sendMsg nsMatrix (mkSelector "selectionByRect") retCULong []

-- | @- setSelectionByRect:@
setSelectionByRect :: IsNSMatrix nsMatrix => nsMatrix -> Bool -> IO ()
setSelectionByRect nsMatrix  value =
    sendMsg nsMatrix (mkSelector "setSelectionByRect:") retVoid [argCULong (if value then 1 else 0)]

-- | @- cellSize@
cellSize :: IsNSMatrix nsMatrix => nsMatrix -> IO NSSize
cellSize nsMatrix  =
    sendMsgStret nsMatrix (mkSelector "cellSize") retNSSize []

-- | @- setCellSize:@
setCellSize :: IsNSMatrix nsMatrix => nsMatrix -> NSSize -> IO ()
setCellSize nsMatrix  value =
    sendMsg nsMatrix (mkSelector "setCellSize:") retVoid [argNSSize value]

-- | @- intercellSpacing@
intercellSpacing :: IsNSMatrix nsMatrix => nsMatrix -> IO NSSize
intercellSpacing nsMatrix  =
    sendMsgStret nsMatrix (mkSelector "intercellSpacing") retNSSize []

-- | @- setIntercellSpacing:@
setIntercellSpacing :: IsNSMatrix nsMatrix => nsMatrix -> NSSize -> IO ()
setIntercellSpacing nsMatrix  value =
    sendMsg nsMatrix (mkSelector "setIntercellSpacing:") retVoid [argNSSize value]

-- | @- backgroundColor@
backgroundColor :: IsNSMatrix nsMatrix => nsMatrix -> IO (Id NSColor)
backgroundColor nsMatrix  =
    sendMsg nsMatrix (mkSelector "backgroundColor") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- setBackgroundColor:@
setBackgroundColor :: (IsNSMatrix nsMatrix, IsNSColor value) => nsMatrix -> value -> IO ()
setBackgroundColor nsMatrix  value =
  withObjCPtr value $ \raw_value ->
      sendMsg nsMatrix (mkSelector "setBackgroundColor:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- | @- cellBackgroundColor@
cellBackgroundColor :: IsNSMatrix nsMatrix => nsMatrix -> IO (Id NSColor)
cellBackgroundColor nsMatrix  =
    sendMsg nsMatrix (mkSelector "cellBackgroundColor") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- setCellBackgroundColor:@
setCellBackgroundColor :: (IsNSMatrix nsMatrix, IsNSColor value) => nsMatrix -> value -> IO ()
setCellBackgroundColor nsMatrix  value =
  withObjCPtr value $ \raw_value ->
      sendMsg nsMatrix (mkSelector "setCellBackgroundColor:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- | @- drawsCellBackground@
drawsCellBackground :: IsNSMatrix nsMatrix => nsMatrix -> IO Bool
drawsCellBackground nsMatrix  =
    fmap ((/= 0) :: CULong -> Bool) $ sendMsg nsMatrix (mkSelector "drawsCellBackground") retCULong []

-- | @- setDrawsCellBackground:@
setDrawsCellBackground :: IsNSMatrix nsMatrix => nsMatrix -> Bool -> IO ()
setDrawsCellBackground nsMatrix  value =
    sendMsg nsMatrix (mkSelector "setDrawsCellBackground:") retVoid [argCULong (if value then 1 else 0)]

-- | @- drawsBackground@
drawsBackground :: IsNSMatrix nsMatrix => nsMatrix -> IO Bool
drawsBackground nsMatrix  =
    fmap ((/= 0) :: CULong -> Bool) $ sendMsg nsMatrix (mkSelector "drawsBackground") retCULong []

-- | @- setDrawsBackground:@
setDrawsBackground :: IsNSMatrix nsMatrix => nsMatrix -> Bool -> IO ()
setDrawsBackground nsMatrix  value =
    sendMsg nsMatrix (mkSelector "setDrawsBackground:") retVoid [argCULong (if value then 1 else 0)]

-- | @- numberOfRows@
numberOfRows :: IsNSMatrix nsMatrix => nsMatrix -> IO CLong
numberOfRows nsMatrix  =
    sendMsg nsMatrix (mkSelector "numberOfRows") retCLong []

-- | @- numberOfColumns@
numberOfColumns :: IsNSMatrix nsMatrix => nsMatrix -> IO CLong
numberOfColumns nsMatrix  =
    sendMsg nsMatrix (mkSelector "numberOfColumns") retCLong []

-- | @- doubleAction@
doubleAction :: IsNSMatrix nsMatrix => nsMatrix -> IO Selector
doubleAction nsMatrix  =
    fmap (Selector . castPtr) $ sendMsg nsMatrix (mkSelector "doubleAction") (retPtr retVoid) []

-- | @- setDoubleAction:@
setDoubleAction :: IsNSMatrix nsMatrix => nsMatrix -> Selector -> IO ()
setDoubleAction nsMatrix  value =
    sendMsg nsMatrix (mkSelector "setDoubleAction:") retVoid [argPtr (unSelector value)]

-- | @- autosizesCells@
autosizesCells :: IsNSMatrix nsMatrix => nsMatrix -> IO Bool
autosizesCells nsMatrix  =
    fmap ((/= 0) :: CULong -> Bool) $ sendMsg nsMatrix (mkSelector "autosizesCells") retCULong []

-- | @- setAutosizesCells:@
setAutosizesCells :: IsNSMatrix nsMatrix => nsMatrix -> Bool -> IO ()
setAutosizesCells nsMatrix  value =
    sendMsg nsMatrix (mkSelector "setAutosizesCells:") retVoid [argCULong (if value then 1 else 0)]

-- | @- autoscroll@
autoscroll :: IsNSMatrix nsMatrix => nsMatrix -> IO Bool
autoscroll nsMatrix  =
    fmap ((/= 0) :: CULong -> Bool) $ sendMsg nsMatrix (mkSelector "autoscroll") retCULong []

-- | @- setAutoscroll:@
setAutoscroll :: IsNSMatrix nsMatrix => nsMatrix -> Bool -> IO ()
setAutoscroll nsMatrix  value =
    sendMsg nsMatrix (mkSelector "setAutoscroll:") retVoid [argCULong (if value then 1 else 0)]

-- | @- mouseDownFlags@
mouseDownFlags :: IsNSMatrix nsMatrix => nsMatrix -> IO CLong
mouseDownFlags nsMatrix  =
    sendMsg nsMatrix (mkSelector "mouseDownFlags") retCLong []

-- | @- delegate@
delegate :: IsNSMatrix nsMatrix => nsMatrix -> IO RawId
delegate nsMatrix  =
    fmap (RawId . castPtr) $ sendMsg nsMatrix (mkSelector "delegate") (retPtr retVoid) []

-- | @- setDelegate:@
setDelegate :: IsNSMatrix nsMatrix => nsMatrix -> RawId -> IO ()
setDelegate nsMatrix  value =
    sendMsg nsMatrix (mkSelector "setDelegate:") retVoid [argPtr (castPtr (unRawId value) :: Ptr ())]

-- | @- autorecalculatesCellSize@
autorecalculatesCellSize :: IsNSMatrix nsMatrix => nsMatrix -> IO Bool
autorecalculatesCellSize nsMatrix  =
    fmap ((/= 0) :: CULong -> Bool) $ sendMsg nsMatrix (mkSelector "autorecalculatesCellSize") retCULong []

-- | @- setAutorecalculatesCellSize:@
setAutorecalculatesCellSize :: IsNSMatrix nsMatrix => nsMatrix -> Bool -> IO ()
setAutorecalculatesCellSize nsMatrix  value =
    sendMsg nsMatrix (mkSelector "setAutorecalculatesCellSize:") retVoid [argCULong (if value then 1 else 0)]

-- | @- tabKeyTraversesCells@
tabKeyTraversesCells :: IsNSMatrix nsMatrix => nsMatrix -> IO Bool
tabKeyTraversesCells nsMatrix  =
    fmap ((/= 0) :: CULong -> Bool) $ sendMsg nsMatrix (mkSelector "tabKeyTraversesCells") retCULong []

-- | @- setTabKeyTraversesCells:@
setTabKeyTraversesCells :: IsNSMatrix nsMatrix => nsMatrix -> Bool -> IO ()
setTabKeyTraversesCells nsMatrix  value =
    sendMsg nsMatrix (mkSelector "setTabKeyTraversesCells:") retVoid [argCULong (if value then 1 else 0)]

-- | @- keyCell@
keyCell :: IsNSMatrix nsMatrix => nsMatrix -> IO (Id NSCell)
keyCell nsMatrix  =
    sendMsg nsMatrix (mkSelector "keyCell") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- setKeyCell:@
setKeyCell :: (IsNSMatrix nsMatrix, IsNSCell value) => nsMatrix -> value -> IO ()
setKeyCell nsMatrix  value =
  withObjCPtr value $ \raw_value ->
      sendMsg nsMatrix (mkSelector "setKeyCell:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @initWithFrame:@
initWithFrameSelector :: Selector
initWithFrameSelector = mkSelector "initWithFrame:"

-- | @Selector@ for @initWithFrame:mode:prototype:numberOfRows:numberOfColumns:@
initWithFrame_mode_prototype_numberOfRows_numberOfColumnsSelector :: Selector
initWithFrame_mode_prototype_numberOfRows_numberOfColumnsSelector = mkSelector "initWithFrame:mode:prototype:numberOfRows:numberOfColumns:"

-- | @Selector@ for @initWithFrame:mode:cellClass:numberOfRows:numberOfColumns:@
initWithFrame_mode_cellClass_numberOfRows_numberOfColumnsSelector :: Selector
initWithFrame_mode_cellClass_numberOfRows_numberOfColumnsSelector = mkSelector "initWithFrame:mode:cellClass:numberOfRows:numberOfColumns:"

-- | @Selector@ for @makeCellAtRow:column:@
makeCellAtRow_columnSelector :: Selector
makeCellAtRow_columnSelector = mkSelector "makeCellAtRow:column:"

-- | @Selector@ for @sendAction:to:forAllCells:@
sendAction_to_forAllCellsSelector :: Selector
sendAction_to_forAllCellsSelector = mkSelector "sendAction:to:forAllCells:"

-- | @Selector@ for @sortUsingSelector:@
sortUsingSelectorSelector :: Selector
sortUsingSelectorSelector = mkSelector "sortUsingSelector:"

-- | @Selector@ for @sortUsingFunction:context:@
sortUsingFunction_contextSelector :: Selector
sortUsingFunction_contextSelector = mkSelector "sortUsingFunction:context:"

-- | @Selector@ for @setSelectionFrom:to:anchor:highlight:@
setSelectionFrom_to_anchor_highlightSelector :: Selector
setSelectionFrom_to_anchor_highlightSelector = mkSelector "setSelectionFrom:to:anchor:highlight:"

-- | @Selector@ for @deselectSelectedCell@
deselectSelectedCellSelector :: Selector
deselectSelectedCellSelector = mkSelector "deselectSelectedCell"

-- | @Selector@ for @deselectAllCells@
deselectAllCellsSelector :: Selector
deselectAllCellsSelector = mkSelector "deselectAllCells"

-- | @Selector@ for @selectCellAtRow:column:@
selectCellAtRow_columnSelector :: Selector
selectCellAtRow_columnSelector = mkSelector "selectCellAtRow:column:"

-- | @Selector@ for @selectAll:@
selectAllSelector :: Selector
selectAllSelector = mkSelector "selectAll:"

-- | @Selector@ for @selectCellWithTag:@
selectCellWithTagSelector :: Selector
selectCellWithTagSelector = mkSelector "selectCellWithTag:"

-- | @Selector@ for @setScrollable:@
setScrollableSelector :: Selector
setScrollableSelector = mkSelector "setScrollable:"

-- | @Selector@ for @setState:atRow:column:@
setState_atRow_columnSelector :: Selector
setState_atRow_columnSelector = mkSelector "setState:atRow:column:"

-- | @Selector@ for @getNumberOfRows:columns:@
getNumberOfRows_columnsSelector :: Selector
getNumberOfRows_columnsSelector = mkSelector "getNumberOfRows:columns:"

-- | @Selector@ for @cellAtRow:column:@
cellAtRow_columnSelector :: Selector
cellAtRow_columnSelector = mkSelector "cellAtRow:column:"

-- | @Selector@ for @cellFrameAtRow:column:@
cellFrameAtRow_columnSelector :: Selector
cellFrameAtRow_columnSelector = mkSelector "cellFrameAtRow:column:"

-- | @Selector@ for @getRow:column:ofCell:@
getRow_column_ofCellSelector :: Selector
getRow_column_ofCellSelector = mkSelector "getRow:column:ofCell:"

-- | @Selector@ for @getRow:column:forPoint:@
getRow_column_forPointSelector :: Selector
getRow_column_forPointSelector = mkSelector "getRow:column:forPoint:"

-- | @Selector@ for @renewRows:columns:@
renewRows_columnsSelector :: Selector
renewRows_columnsSelector = mkSelector "renewRows:columns:"

-- | @Selector@ for @putCell:atRow:column:@
putCell_atRow_columnSelector :: Selector
putCell_atRow_columnSelector = mkSelector "putCell:atRow:column:"

-- | @Selector@ for @addRow@
addRowSelector :: Selector
addRowSelector = mkSelector "addRow"

-- | @Selector@ for @addRowWithCells:@
addRowWithCellsSelector :: Selector
addRowWithCellsSelector = mkSelector "addRowWithCells:"

-- | @Selector@ for @insertRow:@
insertRowSelector :: Selector
insertRowSelector = mkSelector "insertRow:"

-- | @Selector@ for @insertRow:withCells:@
insertRow_withCellsSelector :: Selector
insertRow_withCellsSelector = mkSelector "insertRow:withCells:"

-- | @Selector@ for @removeRow:@
removeRowSelector :: Selector
removeRowSelector = mkSelector "removeRow:"

-- | @Selector@ for @addColumn@
addColumnSelector :: Selector
addColumnSelector = mkSelector "addColumn"

-- | @Selector@ for @addColumnWithCells:@
addColumnWithCellsSelector :: Selector
addColumnWithCellsSelector = mkSelector "addColumnWithCells:"

-- | @Selector@ for @insertColumn:@
insertColumnSelector :: Selector
insertColumnSelector = mkSelector "insertColumn:"

-- | @Selector@ for @insertColumn:withCells:@
insertColumn_withCellsSelector :: Selector
insertColumn_withCellsSelector = mkSelector "insertColumn:withCells:"

-- | @Selector@ for @removeColumn:@
removeColumnSelector :: Selector
removeColumnSelector = mkSelector "removeColumn:"

-- | @Selector@ for @cellWithTag:@
cellWithTagSelector :: Selector
cellWithTagSelector = mkSelector "cellWithTag:"

-- | @Selector@ for @sizeToCells@
sizeToCellsSelector :: Selector
sizeToCellsSelector = mkSelector "sizeToCells"

-- | @Selector@ for @setValidateSize:@
setValidateSizeSelector :: Selector
setValidateSizeSelector = mkSelector "setValidateSize:"

-- | @Selector@ for @drawCellAtRow:column:@
drawCellAtRow_columnSelector :: Selector
drawCellAtRow_columnSelector = mkSelector "drawCellAtRow:column:"

-- | @Selector@ for @highlightCell:atRow:column:@
highlightCell_atRow_columnSelector :: Selector
highlightCell_atRow_columnSelector = mkSelector "highlightCell:atRow:column:"

-- | @Selector@ for @scrollCellToVisibleAtRow:column:@
scrollCellToVisibleAtRow_columnSelector :: Selector
scrollCellToVisibleAtRow_columnSelector = mkSelector "scrollCellToVisibleAtRow:column:"

-- | @Selector@ for @mouseDown:@
mouseDownSelector :: Selector
mouseDownSelector = mkSelector "mouseDown:"

-- | @Selector@ for @performKeyEquivalent:@
performKeyEquivalentSelector :: Selector
performKeyEquivalentSelector = mkSelector "performKeyEquivalent:"

-- | @Selector@ for @sendAction@
sendActionSelector :: Selector
sendActionSelector = mkSelector "sendAction"

-- | @Selector@ for @sendDoubleAction@
sendDoubleActionSelector :: Selector
sendDoubleActionSelector = mkSelector "sendDoubleAction"

-- | @Selector@ for @textShouldBeginEditing:@
textShouldBeginEditingSelector :: Selector
textShouldBeginEditingSelector = mkSelector "textShouldBeginEditing:"

-- | @Selector@ for @textShouldEndEditing:@
textShouldEndEditingSelector :: Selector
textShouldEndEditingSelector = mkSelector "textShouldEndEditing:"

-- | @Selector@ for @textDidBeginEditing:@
textDidBeginEditingSelector :: Selector
textDidBeginEditingSelector = mkSelector "textDidBeginEditing:"

-- | @Selector@ for @textDidEndEditing:@
textDidEndEditingSelector :: Selector
textDidEndEditingSelector = mkSelector "textDidEndEditing:"

-- | @Selector@ for @textDidChange:@
textDidChangeSelector :: Selector
textDidChangeSelector = mkSelector "textDidChange:"

-- | @Selector@ for @selectText:@
selectTextSelector :: Selector
selectTextSelector = mkSelector "selectText:"

-- | @Selector@ for @selectTextAtRow:column:@
selectTextAtRow_columnSelector :: Selector
selectTextAtRow_columnSelector = mkSelector "selectTextAtRow:column:"

-- | @Selector@ for @acceptsFirstMouse:@
acceptsFirstMouseSelector :: Selector
acceptsFirstMouseSelector = mkSelector "acceptsFirstMouse:"

-- | @Selector@ for @resetCursorRects@
resetCursorRectsSelector :: Selector
resetCursorRectsSelector = mkSelector "resetCursorRects"

-- | @Selector@ for @setToolTip:forCell:@
setToolTip_forCellSelector :: Selector
setToolTip_forCellSelector = mkSelector "setToolTip:forCell:"

-- | @Selector@ for @toolTipForCell:@
toolTipForCellSelector :: Selector
toolTipForCellSelector = mkSelector "toolTipForCell:"

-- | @Selector@ for @cellClass@
cellClassSelector :: Selector
cellClassSelector = mkSelector "cellClass"

-- | @Selector@ for @setCellClass:@
setCellClassSelector :: Selector
setCellClassSelector = mkSelector "setCellClass:"

-- | @Selector@ for @prototype@
prototypeSelector :: Selector
prototypeSelector = mkSelector "prototype"

-- | @Selector@ for @setPrototype:@
setPrototypeSelector :: Selector
setPrototypeSelector = mkSelector "setPrototype:"

-- | @Selector@ for @mode@
modeSelector :: Selector
modeSelector = mkSelector "mode"

-- | @Selector@ for @setMode:@
setModeSelector :: Selector
setModeSelector = mkSelector "setMode:"

-- | @Selector@ for @allowsEmptySelection@
allowsEmptySelectionSelector :: Selector
allowsEmptySelectionSelector = mkSelector "allowsEmptySelection"

-- | @Selector@ for @setAllowsEmptySelection:@
setAllowsEmptySelectionSelector :: Selector
setAllowsEmptySelectionSelector = mkSelector "setAllowsEmptySelection:"

-- | @Selector@ for @cells@
cellsSelector :: Selector
cellsSelector = mkSelector "cells"

-- | @Selector@ for @selectedCell@
selectedCellSelector :: Selector
selectedCellSelector = mkSelector "selectedCell"

-- | @Selector@ for @selectedCells@
selectedCellsSelector :: Selector
selectedCellsSelector = mkSelector "selectedCells"

-- | @Selector@ for @selectedRow@
selectedRowSelector :: Selector
selectedRowSelector = mkSelector "selectedRow"

-- | @Selector@ for @selectedColumn@
selectedColumnSelector :: Selector
selectedColumnSelector = mkSelector "selectedColumn"

-- | @Selector@ for @selectionByRect@
selectionByRectSelector :: Selector
selectionByRectSelector = mkSelector "selectionByRect"

-- | @Selector@ for @setSelectionByRect:@
setSelectionByRectSelector :: Selector
setSelectionByRectSelector = mkSelector "setSelectionByRect:"

-- | @Selector@ for @cellSize@
cellSizeSelector :: Selector
cellSizeSelector = mkSelector "cellSize"

-- | @Selector@ for @setCellSize:@
setCellSizeSelector :: Selector
setCellSizeSelector = mkSelector "setCellSize:"

-- | @Selector@ for @intercellSpacing@
intercellSpacingSelector :: Selector
intercellSpacingSelector = mkSelector "intercellSpacing"

-- | @Selector@ for @setIntercellSpacing:@
setIntercellSpacingSelector :: Selector
setIntercellSpacingSelector = mkSelector "setIntercellSpacing:"

-- | @Selector@ for @backgroundColor@
backgroundColorSelector :: Selector
backgroundColorSelector = mkSelector "backgroundColor"

-- | @Selector@ for @setBackgroundColor:@
setBackgroundColorSelector :: Selector
setBackgroundColorSelector = mkSelector "setBackgroundColor:"

-- | @Selector@ for @cellBackgroundColor@
cellBackgroundColorSelector :: Selector
cellBackgroundColorSelector = mkSelector "cellBackgroundColor"

-- | @Selector@ for @setCellBackgroundColor:@
setCellBackgroundColorSelector :: Selector
setCellBackgroundColorSelector = mkSelector "setCellBackgroundColor:"

-- | @Selector@ for @drawsCellBackground@
drawsCellBackgroundSelector :: Selector
drawsCellBackgroundSelector = mkSelector "drawsCellBackground"

-- | @Selector@ for @setDrawsCellBackground:@
setDrawsCellBackgroundSelector :: Selector
setDrawsCellBackgroundSelector = mkSelector "setDrawsCellBackground:"

-- | @Selector@ for @drawsBackground@
drawsBackgroundSelector :: Selector
drawsBackgroundSelector = mkSelector "drawsBackground"

-- | @Selector@ for @setDrawsBackground:@
setDrawsBackgroundSelector :: Selector
setDrawsBackgroundSelector = mkSelector "setDrawsBackground:"

-- | @Selector@ for @numberOfRows@
numberOfRowsSelector :: Selector
numberOfRowsSelector = mkSelector "numberOfRows"

-- | @Selector@ for @numberOfColumns@
numberOfColumnsSelector :: Selector
numberOfColumnsSelector = mkSelector "numberOfColumns"

-- | @Selector@ for @doubleAction@
doubleActionSelector :: Selector
doubleActionSelector = mkSelector "doubleAction"

-- | @Selector@ for @setDoubleAction:@
setDoubleActionSelector :: Selector
setDoubleActionSelector = mkSelector "setDoubleAction:"

-- | @Selector@ for @autosizesCells@
autosizesCellsSelector :: Selector
autosizesCellsSelector = mkSelector "autosizesCells"

-- | @Selector@ for @setAutosizesCells:@
setAutosizesCellsSelector :: Selector
setAutosizesCellsSelector = mkSelector "setAutosizesCells:"

-- | @Selector@ for @autoscroll@
autoscrollSelector :: Selector
autoscrollSelector = mkSelector "autoscroll"

-- | @Selector@ for @setAutoscroll:@
setAutoscrollSelector :: Selector
setAutoscrollSelector = mkSelector "setAutoscroll:"

-- | @Selector@ for @mouseDownFlags@
mouseDownFlagsSelector :: Selector
mouseDownFlagsSelector = mkSelector "mouseDownFlags"

-- | @Selector@ for @delegate@
delegateSelector :: Selector
delegateSelector = mkSelector "delegate"

-- | @Selector@ for @setDelegate:@
setDelegateSelector :: Selector
setDelegateSelector = mkSelector "setDelegate:"

-- | @Selector@ for @autorecalculatesCellSize@
autorecalculatesCellSizeSelector :: Selector
autorecalculatesCellSizeSelector = mkSelector "autorecalculatesCellSize"

-- | @Selector@ for @setAutorecalculatesCellSize:@
setAutorecalculatesCellSizeSelector :: Selector
setAutorecalculatesCellSizeSelector = mkSelector "setAutorecalculatesCellSize:"

-- | @Selector@ for @tabKeyTraversesCells@
tabKeyTraversesCellsSelector :: Selector
tabKeyTraversesCellsSelector = mkSelector "tabKeyTraversesCells"

-- | @Selector@ for @setTabKeyTraversesCells:@
setTabKeyTraversesCellsSelector :: Selector
setTabKeyTraversesCellsSelector = mkSelector "setTabKeyTraversesCells:"

-- | @Selector@ for @keyCell@
keyCellSelector :: Selector
keyCellSelector = mkSelector "keyCell"

-- | @Selector@ for @setKeyCell:@
setKeyCellSelector :: Selector
setKeyCellSelector = mkSelector "setKeyCell:"

