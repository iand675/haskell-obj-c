{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE DataKinds #-}
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
  , acceptsFirstMouseSelector
  , addColumnSelector
  , addColumnWithCellsSelector
  , addRowSelector
  , addRowWithCellsSelector
  , allowsEmptySelectionSelector
  , autorecalculatesCellSizeSelector
  , autoscrollSelector
  , autosizesCellsSelector
  , backgroundColorSelector
  , cellAtRow_columnSelector
  , cellBackgroundColorSelector
  , cellClassSelector
  , cellFrameAtRow_columnSelector
  , cellSizeSelector
  , cellWithTagSelector
  , cellsSelector
  , delegateSelector
  , deselectAllCellsSelector
  , deselectSelectedCellSelector
  , doubleActionSelector
  , drawCellAtRow_columnSelector
  , drawsBackgroundSelector
  , drawsCellBackgroundSelector
  , getNumberOfRows_columnsSelector
  , getRow_column_forPointSelector
  , getRow_column_ofCellSelector
  , highlightCell_atRow_columnSelector
  , initWithFrameSelector
  , initWithFrame_mode_cellClass_numberOfRows_numberOfColumnsSelector
  , initWithFrame_mode_prototype_numberOfRows_numberOfColumnsSelector
  , insertColumnSelector
  , insertColumn_withCellsSelector
  , insertRowSelector
  , insertRow_withCellsSelector
  , intercellSpacingSelector
  , keyCellSelector
  , makeCellAtRow_columnSelector
  , modeSelector
  , mouseDownFlagsSelector
  , mouseDownSelector
  , numberOfColumnsSelector
  , numberOfRowsSelector
  , performKeyEquivalentSelector
  , prototypeSelector
  , putCell_atRow_columnSelector
  , removeColumnSelector
  , removeRowSelector
  , renewRows_columnsSelector
  , resetCursorRectsSelector
  , scrollCellToVisibleAtRow_columnSelector
  , selectAllSelector
  , selectCellAtRow_columnSelector
  , selectCellWithTagSelector
  , selectTextAtRow_columnSelector
  , selectTextSelector
  , selectedCellSelector
  , selectedCellsSelector
  , selectedColumnSelector
  , selectedRowSelector
  , selectionByRectSelector
  , sendActionSelector
  , sendAction_to_forAllCellsSelector
  , sendDoubleActionSelector
  , setAllowsEmptySelectionSelector
  , setAutorecalculatesCellSizeSelector
  , setAutoscrollSelector
  , setAutosizesCellsSelector
  , setBackgroundColorSelector
  , setCellBackgroundColorSelector
  , setCellClassSelector
  , setCellSizeSelector
  , setDelegateSelector
  , setDoubleActionSelector
  , setDrawsBackgroundSelector
  , setDrawsCellBackgroundSelector
  , setIntercellSpacingSelector
  , setKeyCellSelector
  , setModeSelector
  , setPrototypeSelector
  , setScrollableSelector
  , setSelectionByRectSelector
  , setSelectionFrom_to_anchor_highlightSelector
  , setState_atRow_columnSelector
  , setTabKeyTraversesCellsSelector
  , setToolTip_forCellSelector
  , setValidateSizeSelector
  , sizeToCellsSelector
  , sortUsingFunction_contextSelector
  , sortUsingSelectorSelector
  , tabKeyTraversesCellsSelector
  , textDidBeginEditingSelector
  , textDidChangeSelector
  , textDidEndEditingSelector
  , textShouldBeginEditingSelector
  , textShouldEndEditingSelector
  , toolTipForCellSelector

  -- * Enum types
  , NSMatrixMode(NSMatrixMode)
  , pattern NSRadioModeMatrix
  , pattern NSHighlightModeMatrix
  , pattern NSListModeMatrix
  , pattern NSTrackModeMatrix

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
initWithFrame :: IsNSMatrix nsMatrix => nsMatrix -> NSRect -> IO (Id NSMatrix)
initWithFrame nsMatrix frameRect =
  sendOwnedMessage nsMatrix initWithFrameSelector frameRect

-- | @- initWithFrame:mode:prototype:numberOfRows:numberOfColumns:@
initWithFrame_mode_prototype_numberOfRows_numberOfColumns :: (IsNSMatrix nsMatrix, IsNSCell cell) => nsMatrix -> NSRect -> NSMatrixMode -> cell -> CLong -> CLong -> IO (Id NSMatrix)
initWithFrame_mode_prototype_numberOfRows_numberOfColumns nsMatrix frameRect mode cell rowsHigh colsWide =
  sendOwnedMessage nsMatrix initWithFrame_mode_prototype_numberOfRows_numberOfColumnsSelector frameRect mode (toNSCell cell) rowsHigh colsWide

-- | @- initWithFrame:mode:cellClass:numberOfRows:numberOfColumns:@
initWithFrame_mode_cellClass_numberOfRows_numberOfColumns :: IsNSMatrix nsMatrix => nsMatrix -> NSRect -> NSMatrixMode -> Class -> CLong -> CLong -> IO (Id NSMatrix)
initWithFrame_mode_cellClass_numberOfRows_numberOfColumns nsMatrix frameRect mode factoryId rowsHigh colsWide =
  sendOwnedMessage nsMatrix initWithFrame_mode_cellClass_numberOfRows_numberOfColumnsSelector frameRect mode factoryId rowsHigh colsWide

-- | @- makeCellAtRow:column:@
makeCellAtRow_column :: IsNSMatrix nsMatrix => nsMatrix -> CLong -> CLong -> IO (Id NSCell)
makeCellAtRow_column nsMatrix row col =
  sendMessage nsMatrix makeCellAtRow_columnSelector row col

-- | @- sendAction:to:forAllCells:@
sendAction_to_forAllCells :: IsNSMatrix nsMatrix => nsMatrix -> Sel -> RawId -> Bool -> IO ()
sendAction_to_forAllCells nsMatrix selector object flag =
  sendMessage nsMatrix sendAction_to_forAllCellsSelector selector object flag

-- | @- sortUsingSelector:@
sortUsingSelector :: IsNSMatrix nsMatrix => nsMatrix -> Sel -> IO ()
sortUsingSelector nsMatrix comparator =
  sendMessage nsMatrix sortUsingSelectorSelector comparator

-- | @- sortUsingFunction:context:@
sortUsingFunction_context :: IsNSMatrix nsMatrix => nsMatrix -> Ptr () -> Ptr () -> IO ()
sortUsingFunction_context nsMatrix compare_ context =
  sendMessage nsMatrix sortUsingFunction_contextSelector compare_ context

-- | @- setSelectionFrom:to:anchor:highlight:@
setSelectionFrom_to_anchor_highlight :: IsNSMatrix nsMatrix => nsMatrix -> CLong -> CLong -> CLong -> Bool -> IO ()
setSelectionFrom_to_anchor_highlight nsMatrix startPos endPos anchorPos lit =
  sendMessage nsMatrix setSelectionFrom_to_anchor_highlightSelector startPos endPos anchorPos lit

-- | @- deselectSelectedCell@
deselectSelectedCell :: IsNSMatrix nsMatrix => nsMatrix -> IO ()
deselectSelectedCell nsMatrix =
  sendMessage nsMatrix deselectSelectedCellSelector

-- | @- deselectAllCells@
deselectAllCells :: IsNSMatrix nsMatrix => nsMatrix -> IO ()
deselectAllCells nsMatrix =
  sendMessage nsMatrix deselectAllCellsSelector

-- | @- selectCellAtRow:column:@
selectCellAtRow_column :: IsNSMatrix nsMatrix => nsMatrix -> CLong -> CLong -> IO ()
selectCellAtRow_column nsMatrix row col =
  sendMessage nsMatrix selectCellAtRow_columnSelector row col

-- | @- selectAll:@
selectAll :: IsNSMatrix nsMatrix => nsMatrix -> RawId -> IO ()
selectAll nsMatrix sender =
  sendMessage nsMatrix selectAllSelector sender

-- | @- selectCellWithTag:@
selectCellWithTag :: IsNSMatrix nsMatrix => nsMatrix -> CLong -> IO Bool
selectCellWithTag nsMatrix tag =
  sendMessage nsMatrix selectCellWithTagSelector tag

-- | @- setScrollable:@
setScrollable :: IsNSMatrix nsMatrix => nsMatrix -> Bool -> IO ()
setScrollable nsMatrix flag =
  sendMessage nsMatrix setScrollableSelector flag

-- | @- setState:atRow:column:@
setState_atRow_column :: IsNSMatrix nsMatrix => nsMatrix -> CLong -> CLong -> CLong -> IO ()
setState_atRow_column nsMatrix value row col =
  sendMessage nsMatrix setState_atRow_columnSelector value row col

-- | @- getNumberOfRows:columns:@
getNumberOfRows_columns :: IsNSMatrix nsMatrix => nsMatrix -> Ptr CLong -> Ptr CLong -> IO ()
getNumberOfRows_columns nsMatrix rowCount colCount =
  sendMessage nsMatrix getNumberOfRows_columnsSelector rowCount colCount

-- | @- cellAtRow:column:@
cellAtRow_column :: IsNSMatrix nsMatrix => nsMatrix -> CLong -> CLong -> IO (Id NSCell)
cellAtRow_column nsMatrix row col =
  sendMessage nsMatrix cellAtRow_columnSelector row col

-- | @- cellFrameAtRow:column:@
cellFrameAtRow_column :: IsNSMatrix nsMatrix => nsMatrix -> CLong -> CLong -> IO NSRect
cellFrameAtRow_column nsMatrix row col =
  sendMessage nsMatrix cellFrameAtRow_columnSelector row col

-- | @- getRow:column:ofCell:@
getRow_column_ofCell :: (IsNSMatrix nsMatrix, IsNSCell cell) => nsMatrix -> Ptr CLong -> Ptr CLong -> cell -> IO Bool
getRow_column_ofCell nsMatrix row col cell =
  sendMessage nsMatrix getRow_column_ofCellSelector row col (toNSCell cell)

-- | @- getRow:column:forPoint:@
getRow_column_forPoint :: IsNSMatrix nsMatrix => nsMatrix -> Ptr CLong -> Ptr CLong -> NSPoint -> IO Bool
getRow_column_forPoint nsMatrix row col point =
  sendMessage nsMatrix getRow_column_forPointSelector row col point

-- | @- renewRows:columns:@
renewRows_columns :: IsNSMatrix nsMatrix => nsMatrix -> CLong -> CLong -> IO ()
renewRows_columns nsMatrix newRows newCols =
  sendMessage nsMatrix renewRows_columnsSelector newRows newCols

-- | @- putCell:atRow:column:@
putCell_atRow_column :: (IsNSMatrix nsMatrix, IsNSCell newCell) => nsMatrix -> newCell -> CLong -> CLong -> IO ()
putCell_atRow_column nsMatrix newCell row col =
  sendMessage nsMatrix putCell_atRow_columnSelector (toNSCell newCell) row col

-- | @- addRow@
addRow :: IsNSMatrix nsMatrix => nsMatrix -> IO ()
addRow nsMatrix =
  sendMessage nsMatrix addRowSelector

-- | @- addRowWithCells:@
addRowWithCells :: (IsNSMatrix nsMatrix, IsNSArray newCells) => nsMatrix -> newCells -> IO ()
addRowWithCells nsMatrix newCells =
  sendMessage nsMatrix addRowWithCellsSelector (toNSArray newCells)

-- | @- insertRow:@
insertRow :: IsNSMatrix nsMatrix => nsMatrix -> CLong -> IO ()
insertRow nsMatrix row =
  sendMessage nsMatrix insertRowSelector row

-- | @- insertRow:withCells:@
insertRow_withCells :: (IsNSMatrix nsMatrix, IsNSArray newCells) => nsMatrix -> CLong -> newCells -> IO ()
insertRow_withCells nsMatrix row newCells =
  sendMessage nsMatrix insertRow_withCellsSelector row (toNSArray newCells)

-- | @- removeRow:@
removeRow :: IsNSMatrix nsMatrix => nsMatrix -> CLong -> IO ()
removeRow nsMatrix row =
  sendMessage nsMatrix removeRowSelector row

-- | @- addColumn@
addColumn :: IsNSMatrix nsMatrix => nsMatrix -> IO ()
addColumn nsMatrix =
  sendMessage nsMatrix addColumnSelector

-- | @- addColumnWithCells:@
addColumnWithCells :: (IsNSMatrix nsMatrix, IsNSArray newCells) => nsMatrix -> newCells -> IO ()
addColumnWithCells nsMatrix newCells =
  sendMessage nsMatrix addColumnWithCellsSelector (toNSArray newCells)

-- | @- insertColumn:@
insertColumn :: IsNSMatrix nsMatrix => nsMatrix -> CLong -> IO ()
insertColumn nsMatrix column =
  sendMessage nsMatrix insertColumnSelector column

-- | @- insertColumn:withCells:@
insertColumn_withCells :: (IsNSMatrix nsMatrix, IsNSArray newCells) => nsMatrix -> CLong -> newCells -> IO ()
insertColumn_withCells nsMatrix column newCells =
  sendMessage nsMatrix insertColumn_withCellsSelector column (toNSArray newCells)

-- | @- removeColumn:@
removeColumn :: IsNSMatrix nsMatrix => nsMatrix -> CLong -> IO ()
removeColumn nsMatrix col =
  sendMessage nsMatrix removeColumnSelector col

-- | @- cellWithTag:@
cellWithTag :: IsNSMatrix nsMatrix => nsMatrix -> CLong -> IO (Id NSCell)
cellWithTag nsMatrix tag =
  sendMessage nsMatrix cellWithTagSelector tag

-- | @- sizeToCells@
sizeToCells :: IsNSMatrix nsMatrix => nsMatrix -> IO ()
sizeToCells nsMatrix =
  sendMessage nsMatrix sizeToCellsSelector

-- | @- setValidateSize:@
setValidateSize :: IsNSMatrix nsMatrix => nsMatrix -> Bool -> IO ()
setValidateSize nsMatrix flag =
  sendMessage nsMatrix setValidateSizeSelector flag

-- | @- drawCellAtRow:column:@
drawCellAtRow_column :: IsNSMatrix nsMatrix => nsMatrix -> CLong -> CLong -> IO ()
drawCellAtRow_column nsMatrix row col =
  sendMessage nsMatrix drawCellAtRow_columnSelector row col

-- | @- highlightCell:atRow:column:@
highlightCell_atRow_column :: IsNSMatrix nsMatrix => nsMatrix -> Bool -> CLong -> CLong -> IO ()
highlightCell_atRow_column nsMatrix flag row col =
  sendMessage nsMatrix highlightCell_atRow_columnSelector flag row col

-- | @- scrollCellToVisibleAtRow:column:@
scrollCellToVisibleAtRow_column :: IsNSMatrix nsMatrix => nsMatrix -> CLong -> CLong -> IO ()
scrollCellToVisibleAtRow_column nsMatrix row col =
  sendMessage nsMatrix scrollCellToVisibleAtRow_columnSelector row col

-- | @- mouseDown:@
mouseDown :: (IsNSMatrix nsMatrix, IsNSEvent event) => nsMatrix -> event -> IO ()
mouseDown nsMatrix event =
  sendMessage nsMatrix mouseDownSelector (toNSEvent event)

-- | @- performKeyEquivalent:@
performKeyEquivalent :: (IsNSMatrix nsMatrix, IsNSEvent event) => nsMatrix -> event -> IO Bool
performKeyEquivalent nsMatrix event =
  sendMessage nsMatrix performKeyEquivalentSelector (toNSEvent event)

-- | @- sendAction@
sendAction :: IsNSMatrix nsMatrix => nsMatrix -> IO Bool
sendAction nsMatrix =
  sendMessage nsMatrix sendActionSelector

-- | @- sendDoubleAction@
sendDoubleAction :: IsNSMatrix nsMatrix => nsMatrix -> IO ()
sendDoubleAction nsMatrix =
  sendMessage nsMatrix sendDoubleActionSelector

-- | @- textShouldBeginEditing:@
textShouldBeginEditing :: (IsNSMatrix nsMatrix, IsNSText textObject) => nsMatrix -> textObject -> IO Bool
textShouldBeginEditing nsMatrix textObject =
  sendMessage nsMatrix textShouldBeginEditingSelector (toNSText textObject)

-- | @- textShouldEndEditing:@
textShouldEndEditing :: (IsNSMatrix nsMatrix, IsNSText textObject) => nsMatrix -> textObject -> IO Bool
textShouldEndEditing nsMatrix textObject =
  sendMessage nsMatrix textShouldEndEditingSelector (toNSText textObject)

-- | @- textDidBeginEditing:@
textDidBeginEditing :: (IsNSMatrix nsMatrix, IsNSNotification notification) => nsMatrix -> notification -> IO ()
textDidBeginEditing nsMatrix notification =
  sendMessage nsMatrix textDidBeginEditingSelector (toNSNotification notification)

-- | @- textDidEndEditing:@
textDidEndEditing :: (IsNSMatrix nsMatrix, IsNSNotification notification) => nsMatrix -> notification -> IO ()
textDidEndEditing nsMatrix notification =
  sendMessage nsMatrix textDidEndEditingSelector (toNSNotification notification)

-- | @- textDidChange:@
textDidChange :: (IsNSMatrix nsMatrix, IsNSNotification notification) => nsMatrix -> notification -> IO ()
textDidChange nsMatrix notification =
  sendMessage nsMatrix textDidChangeSelector (toNSNotification notification)

-- | @- selectText:@
selectText :: IsNSMatrix nsMatrix => nsMatrix -> RawId -> IO ()
selectText nsMatrix sender =
  sendMessage nsMatrix selectTextSelector sender

-- | @- selectTextAtRow:column:@
selectTextAtRow_column :: IsNSMatrix nsMatrix => nsMatrix -> CLong -> CLong -> IO (Id NSCell)
selectTextAtRow_column nsMatrix row col =
  sendMessage nsMatrix selectTextAtRow_columnSelector row col

-- | @- acceptsFirstMouse:@
acceptsFirstMouse :: (IsNSMatrix nsMatrix, IsNSEvent event) => nsMatrix -> event -> IO Bool
acceptsFirstMouse nsMatrix event =
  sendMessage nsMatrix acceptsFirstMouseSelector (toNSEvent event)

-- | @- resetCursorRects@
resetCursorRects :: IsNSMatrix nsMatrix => nsMatrix -> IO ()
resetCursorRects nsMatrix =
  sendMessage nsMatrix resetCursorRectsSelector

-- | @- setToolTip:forCell:@
setToolTip_forCell :: (IsNSMatrix nsMatrix, IsNSString toolTipString, IsNSCell cell) => nsMatrix -> toolTipString -> cell -> IO ()
setToolTip_forCell nsMatrix toolTipString cell =
  sendMessage nsMatrix setToolTip_forCellSelector (toNSString toolTipString) (toNSCell cell)

-- | @- toolTipForCell:@
toolTipForCell :: (IsNSMatrix nsMatrix, IsNSCell cell) => nsMatrix -> cell -> IO (Id NSString)
toolTipForCell nsMatrix cell =
  sendMessage nsMatrix toolTipForCellSelector (toNSCell cell)

-- | @- cellClass@
cellClass :: IsNSMatrix nsMatrix => nsMatrix -> IO Class
cellClass nsMatrix =
  sendMessage nsMatrix cellClassSelector

-- | @- setCellClass:@
setCellClass :: IsNSMatrix nsMatrix => nsMatrix -> Class -> IO ()
setCellClass nsMatrix value =
  sendMessage nsMatrix setCellClassSelector value

-- | @- prototype@
prototype :: IsNSMatrix nsMatrix => nsMatrix -> IO (Id NSCell)
prototype nsMatrix =
  sendMessage nsMatrix prototypeSelector

-- | @- setPrototype:@
setPrototype :: (IsNSMatrix nsMatrix, IsNSCell value) => nsMatrix -> value -> IO ()
setPrototype nsMatrix value =
  sendMessage nsMatrix setPrototypeSelector (toNSCell value)

-- | @- mode@
mode :: IsNSMatrix nsMatrix => nsMatrix -> IO NSMatrixMode
mode nsMatrix =
  sendMessage nsMatrix modeSelector

-- | @- setMode:@
setMode :: IsNSMatrix nsMatrix => nsMatrix -> NSMatrixMode -> IO ()
setMode nsMatrix value =
  sendMessage nsMatrix setModeSelector value

-- | @- allowsEmptySelection@
allowsEmptySelection :: IsNSMatrix nsMatrix => nsMatrix -> IO Bool
allowsEmptySelection nsMatrix =
  sendMessage nsMatrix allowsEmptySelectionSelector

-- | @- setAllowsEmptySelection:@
setAllowsEmptySelection :: IsNSMatrix nsMatrix => nsMatrix -> Bool -> IO ()
setAllowsEmptySelection nsMatrix value =
  sendMessage nsMatrix setAllowsEmptySelectionSelector value

-- | @- cells@
cells :: IsNSMatrix nsMatrix => nsMatrix -> IO (Id NSArray)
cells nsMatrix =
  sendMessage nsMatrix cellsSelector

-- | @- selectedCell@
selectedCell :: IsNSMatrix nsMatrix => nsMatrix -> IO (Id NSCell)
selectedCell nsMatrix =
  sendMessage nsMatrix selectedCellSelector

-- | @- selectedCells@
selectedCells :: IsNSMatrix nsMatrix => nsMatrix -> IO (Id NSArray)
selectedCells nsMatrix =
  sendMessage nsMatrix selectedCellsSelector

-- | @- selectedRow@
selectedRow :: IsNSMatrix nsMatrix => nsMatrix -> IO CLong
selectedRow nsMatrix =
  sendMessage nsMatrix selectedRowSelector

-- | @- selectedColumn@
selectedColumn :: IsNSMatrix nsMatrix => nsMatrix -> IO CLong
selectedColumn nsMatrix =
  sendMessage nsMatrix selectedColumnSelector

-- | @- selectionByRect@
selectionByRect :: IsNSMatrix nsMatrix => nsMatrix -> IO Bool
selectionByRect nsMatrix =
  sendMessage nsMatrix selectionByRectSelector

-- | @- setSelectionByRect:@
setSelectionByRect :: IsNSMatrix nsMatrix => nsMatrix -> Bool -> IO ()
setSelectionByRect nsMatrix value =
  sendMessage nsMatrix setSelectionByRectSelector value

-- | @- cellSize@
cellSize :: IsNSMatrix nsMatrix => nsMatrix -> IO NSSize
cellSize nsMatrix =
  sendMessage nsMatrix cellSizeSelector

-- | @- setCellSize:@
setCellSize :: IsNSMatrix nsMatrix => nsMatrix -> NSSize -> IO ()
setCellSize nsMatrix value =
  sendMessage nsMatrix setCellSizeSelector value

-- | @- intercellSpacing@
intercellSpacing :: IsNSMatrix nsMatrix => nsMatrix -> IO NSSize
intercellSpacing nsMatrix =
  sendMessage nsMatrix intercellSpacingSelector

-- | @- setIntercellSpacing:@
setIntercellSpacing :: IsNSMatrix nsMatrix => nsMatrix -> NSSize -> IO ()
setIntercellSpacing nsMatrix value =
  sendMessage nsMatrix setIntercellSpacingSelector value

-- | @- backgroundColor@
backgroundColor :: IsNSMatrix nsMatrix => nsMatrix -> IO (Id NSColor)
backgroundColor nsMatrix =
  sendMessage nsMatrix backgroundColorSelector

-- | @- setBackgroundColor:@
setBackgroundColor :: (IsNSMatrix nsMatrix, IsNSColor value) => nsMatrix -> value -> IO ()
setBackgroundColor nsMatrix value =
  sendMessage nsMatrix setBackgroundColorSelector (toNSColor value)

-- | @- cellBackgroundColor@
cellBackgroundColor :: IsNSMatrix nsMatrix => nsMatrix -> IO (Id NSColor)
cellBackgroundColor nsMatrix =
  sendMessage nsMatrix cellBackgroundColorSelector

-- | @- setCellBackgroundColor:@
setCellBackgroundColor :: (IsNSMatrix nsMatrix, IsNSColor value) => nsMatrix -> value -> IO ()
setCellBackgroundColor nsMatrix value =
  sendMessage nsMatrix setCellBackgroundColorSelector (toNSColor value)

-- | @- drawsCellBackground@
drawsCellBackground :: IsNSMatrix nsMatrix => nsMatrix -> IO Bool
drawsCellBackground nsMatrix =
  sendMessage nsMatrix drawsCellBackgroundSelector

-- | @- setDrawsCellBackground:@
setDrawsCellBackground :: IsNSMatrix nsMatrix => nsMatrix -> Bool -> IO ()
setDrawsCellBackground nsMatrix value =
  sendMessage nsMatrix setDrawsCellBackgroundSelector value

-- | @- drawsBackground@
drawsBackground :: IsNSMatrix nsMatrix => nsMatrix -> IO Bool
drawsBackground nsMatrix =
  sendMessage nsMatrix drawsBackgroundSelector

-- | @- setDrawsBackground:@
setDrawsBackground :: IsNSMatrix nsMatrix => nsMatrix -> Bool -> IO ()
setDrawsBackground nsMatrix value =
  sendMessage nsMatrix setDrawsBackgroundSelector value

-- | @- numberOfRows@
numberOfRows :: IsNSMatrix nsMatrix => nsMatrix -> IO CLong
numberOfRows nsMatrix =
  sendMessage nsMatrix numberOfRowsSelector

-- | @- numberOfColumns@
numberOfColumns :: IsNSMatrix nsMatrix => nsMatrix -> IO CLong
numberOfColumns nsMatrix =
  sendMessage nsMatrix numberOfColumnsSelector

-- | @- doubleAction@
doubleAction :: IsNSMatrix nsMatrix => nsMatrix -> IO Sel
doubleAction nsMatrix =
  sendMessage nsMatrix doubleActionSelector

-- | @- setDoubleAction:@
setDoubleAction :: IsNSMatrix nsMatrix => nsMatrix -> Sel -> IO ()
setDoubleAction nsMatrix value =
  sendMessage nsMatrix setDoubleActionSelector value

-- | @- autosizesCells@
autosizesCells :: IsNSMatrix nsMatrix => nsMatrix -> IO Bool
autosizesCells nsMatrix =
  sendMessage nsMatrix autosizesCellsSelector

-- | @- setAutosizesCells:@
setAutosizesCells :: IsNSMatrix nsMatrix => nsMatrix -> Bool -> IO ()
setAutosizesCells nsMatrix value =
  sendMessage nsMatrix setAutosizesCellsSelector value

-- | @- autoscroll@
autoscroll :: IsNSMatrix nsMatrix => nsMatrix -> IO Bool
autoscroll nsMatrix =
  sendMessage nsMatrix autoscrollSelector

-- | @- setAutoscroll:@
setAutoscroll :: IsNSMatrix nsMatrix => nsMatrix -> Bool -> IO ()
setAutoscroll nsMatrix value =
  sendMessage nsMatrix setAutoscrollSelector value

-- | @- mouseDownFlags@
mouseDownFlags :: IsNSMatrix nsMatrix => nsMatrix -> IO CLong
mouseDownFlags nsMatrix =
  sendMessage nsMatrix mouseDownFlagsSelector

-- | @- delegate@
delegate :: IsNSMatrix nsMatrix => nsMatrix -> IO RawId
delegate nsMatrix =
  sendMessage nsMatrix delegateSelector

-- | @- setDelegate:@
setDelegate :: IsNSMatrix nsMatrix => nsMatrix -> RawId -> IO ()
setDelegate nsMatrix value =
  sendMessage nsMatrix setDelegateSelector value

-- | @- autorecalculatesCellSize@
autorecalculatesCellSize :: IsNSMatrix nsMatrix => nsMatrix -> IO Bool
autorecalculatesCellSize nsMatrix =
  sendMessage nsMatrix autorecalculatesCellSizeSelector

-- | @- setAutorecalculatesCellSize:@
setAutorecalculatesCellSize :: IsNSMatrix nsMatrix => nsMatrix -> Bool -> IO ()
setAutorecalculatesCellSize nsMatrix value =
  sendMessage nsMatrix setAutorecalculatesCellSizeSelector value

-- | @- tabKeyTraversesCells@
tabKeyTraversesCells :: IsNSMatrix nsMatrix => nsMatrix -> IO Bool
tabKeyTraversesCells nsMatrix =
  sendMessage nsMatrix tabKeyTraversesCellsSelector

-- | @- setTabKeyTraversesCells:@
setTabKeyTraversesCells :: IsNSMatrix nsMatrix => nsMatrix -> Bool -> IO ()
setTabKeyTraversesCells nsMatrix value =
  sendMessage nsMatrix setTabKeyTraversesCellsSelector value

-- | @- keyCell@
keyCell :: IsNSMatrix nsMatrix => nsMatrix -> IO (Id NSCell)
keyCell nsMatrix =
  sendMessage nsMatrix keyCellSelector

-- | @- setKeyCell:@
setKeyCell :: (IsNSMatrix nsMatrix, IsNSCell value) => nsMatrix -> value -> IO ()
setKeyCell nsMatrix value =
  sendMessage nsMatrix setKeyCellSelector (toNSCell value)

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @initWithFrame:@
initWithFrameSelector :: Selector '[NSRect] (Id NSMatrix)
initWithFrameSelector = mkSelector "initWithFrame:"

-- | @Selector@ for @initWithFrame:mode:prototype:numberOfRows:numberOfColumns:@
initWithFrame_mode_prototype_numberOfRows_numberOfColumnsSelector :: Selector '[NSRect, NSMatrixMode, Id NSCell, CLong, CLong] (Id NSMatrix)
initWithFrame_mode_prototype_numberOfRows_numberOfColumnsSelector = mkSelector "initWithFrame:mode:prototype:numberOfRows:numberOfColumns:"

-- | @Selector@ for @initWithFrame:mode:cellClass:numberOfRows:numberOfColumns:@
initWithFrame_mode_cellClass_numberOfRows_numberOfColumnsSelector :: Selector '[NSRect, NSMatrixMode, Class, CLong, CLong] (Id NSMatrix)
initWithFrame_mode_cellClass_numberOfRows_numberOfColumnsSelector = mkSelector "initWithFrame:mode:cellClass:numberOfRows:numberOfColumns:"

-- | @Selector@ for @makeCellAtRow:column:@
makeCellAtRow_columnSelector :: Selector '[CLong, CLong] (Id NSCell)
makeCellAtRow_columnSelector = mkSelector "makeCellAtRow:column:"

-- | @Selector@ for @sendAction:to:forAllCells:@
sendAction_to_forAllCellsSelector :: Selector '[Sel, RawId, Bool] ()
sendAction_to_forAllCellsSelector = mkSelector "sendAction:to:forAllCells:"

-- | @Selector@ for @sortUsingSelector:@
sortUsingSelectorSelector :: Selector '[Sel] ()
sortUsingSelectorSelector = mkSelector "sortUsingSelector:"

-- | @Selector@ for @sortUsingFunction:context:@
sortUsingFunction_contextSelector :: Selector '[Ptr (), Ptr ()] ()
sortUsingFunction_contextSelector = mkSelector "sortUsingFunction:context:"

-- | @Selector@ for @setSelectionFrom:to:anchor:highlight:@
setSelectionFrom_to_anchor_highlightSelector :: Selector '[CLong, CLong, CLong, Bool] ()
setSelectionFrom_to_anchor_highlightSelector = mkSelector "setSelectionFrom:to:anchor:highlight:"

-- | @Selector@ for @deselectSelectedCell@
deselectSelectedCellSelector :: Selector '[] ()
deselectSelectedCellSelector = mkSelector "deselectSelectedCell"

-- | @Selector@ for @deselectAllCells@
deselectAllCellsSelector :: Selector '[] ()
deselectAllCellsSelector = mkSelector "deselectAllCells"

-- | @Selector@ for @selectCellAtRow:column:@
selectCellAtRow_columnSelector :: Selector '[CLong, CLong] ()
selectCellAtRow_columnSelector = mkSelector "selectCellAtRow:column:"

-- | @Selector@ for @selectAll:@
selectAllSelector :: Selector '[RawId] ()
selectAllSelector = mkSelector "selectAll:"

-- | @Selector@ for @selectCellWithTag:@
selectCellWithTagSelector :: Selector '[CLong] Bool
selectCellWithTagSelector = mkSelector "selectCellWithTag:"

-- | @Selector@ for @setScrollable:@
setScrollableSelector :: Selector '[Bool] ()
setScrollableSelector = mkSelector "setScrollable:"

-- | @Selector@ for @setState:atRow:column:@
setState_atRow_columnSelector :: Selector '[CLong, CLong, CLong] ()
setState_atRow_columnSelector = mkSelector "setState:atRow:column:"

-- | @Selector@ for @getNumberOfRows:columns:@
getNumberOfRows_columnsSelector :: Selector '[Ptr CLong, Ptr CLong] ()
getNumberOfRows_columnsSelector = mkSelector "getNumberOfRows:columns:"

-- | @Selector@ for @cellAtRow:column:@
cellAtRow_columnSelector :: Selector '[CLong, CLong] (Id NSCell)
cellAtRow_columnSelector = mkSelector "cellAtRow:column:"

-- | @Selector@ for @cellFrameAtRow:column:@
cellFrameAtRow_columnSelector :: Selector '[CLong, CLong] NSRect
cellFrameAtRow_columnSelector = mkSelector "cellFrameAtRow:column:"

-- | @Selector@ for @getRow:column:ofCell:@
getRow_column_ofCellSelector :: Selector '[Ptr CLong, Ptr CLong, Id NSCell] Bool
getRow_column_ofCellSelector = mkSelector "getRow:column:ofCell:"

-- | @Selector@ for @getRow:column:forPoint:@
getRow_column_forPointSelector :: Selector '[Ptr CLong, Ptr CLong, NSPoint] Bool
getRow_column_forPointSelector = mkSelector "getRow:column:forPoint:"

-- | @Selector@ for @renewRows:columns:@
renewRows_columnsSelector :: Selector '[CLong, CLong] ()
renewRows_columnsSelector = mkSelector "renewRows:columns:"

-- | @Selector@ for @putCell:atRow:column:@
putCell_atRow_columnSelector :: Selector '[Id NSCell, CLong, CLong] ()
putCell_atRow_columnSelector = mkSelector "putCell:atRow:column:"

-- | @Selector@ for @addRow@
addRowSelector :: Selector '[] ()
addRowSelector = mkSelector "addRow"

-- | @Selector@ for @addRowWithCells:@
addRowWithCellsSelector :: Selector '[Id NSArray] ()
addRowWithCellsSelector = mkSelector "addRowWithCells:"

-- | @Selector@ for @insertRow:@
insertRowSelector :: Selector '[CLong] ()
insertRowSelector = mkSelector "insertRow:"

-- | @Selector@ for @insertRow:withCells:@
insertRow_withCellsSelector :: Selector '[CLong, Id NSArray] ()
insertRow_withCellsSelector = mkSelector "insertRow:withCells:"

-- | @Selector@ for @removeRow:@
removeRowSelector :: Selector '[CLong] ()
removeRowSelector = mkSelector "removeRow:"

-- | @Selector@ for @addColumn@
addColumnSelector :: Selector '[] ()
addColumnSelector = mkSelector "addColumn"

-- | @Selector@ for @addColumnWithCells:@
addColumnWithCellsSelector :: Selector '[Id NSArray] ()
addColumnWithCellsSelector = mkSelector "addColumnWithCells:"

-- | @Selector@ for @insertColumn:@
insertColumnSelector :: Selector '[CLong] ()
insertColumnSelector = mkSelector "insertColumn:"

-- | @Selector@ for @insertColumn:withCells:@
insertColumn_withCellsSelector :: Selector '[CLong, Id NSArray] ()
insertColumn_withCellsSelector = mkSelector "insertColumn:withCells:"

-- | @Selector@ for @removeColumn:@
removeColumnSelector :: Selector '[CLong] ()
removeColumnSelector = mkSelector "removeColumn:"

-- | @Selector@ for @cellWithTag:@
cellWithTagSelector :: Selector '[CLong] (Id NSCell)
cellWithTagSelector = mkSelector "cellWithTag:"

-- | @Selector@ for @sizeToCells@
sizeToCellsSelector :: Selector '[] ()
sizeToCellsSelector = mkSelector "sizeToCells"

-- | @Selector@ for @setValidateSize:@
setValidateSizeSelector :: Selector '[Bool] ()
setValidateSizeSelector = mkSelector "setValidateSize:"

-- | @Selector@ for @drawCellAtRow:column:@
drawCellAtRow_columnSelector :: Selector '[CLong, CLong] ()
drawCellAtRow_columnSelector = mkSelector "drawCellAtRow:column:"

-- | @Selector@ for @highlightCell:atRow:column:@
highlightCell_atRow_columnSelector :: Selector '[Bool, CLong, CLong] ()
highlightCell_atRow_columnSelector = mkSelector "highlightCell:atRow:column:"

-- | @Selector@ for @scrollCellToVisibleAtRow:column:@
scrollCellToVisibleAtRow_columnSelector :: Selector '[CLong, CLong] ()
scrollCellToVisibleAtRow_columnSelector = mkSelector "scrollCellToVisibleAtRow:column:"

-- | @Selector@ for @mouseDown:@
mouseDownSelector :: Selector '[Id NSEvent] ()
mouseDownSelector = mkSelector "mouseDown:"

-- | @Selector@ for @performKeyEquivalent:@
performKeyEquivalentSelector :: Selector '[Id NSEvent] Bool
performKeyEquivalentSelector = mkSelector "performKeyEquivalent:"

-- | @Selector@ for @sendAction@
sendActionSelector :: Selector '[] Bool
sendActionSelector = mkSelector "sendAction"

-- | @Selector@ for @sendDoubleAction@
sendDoubleActionSelector :: Selector '[] ()
sendDoubleActionSelector = mkSelector "sendDoubleAction"

-- | @Selector@ for @textShouldBeginEditing:@
textShouldBeginEditingSelector :: Selector '[Id NSText] Bool
textShouldBeginEditingSelector = mkSelector "textShouldBeginEditing:"

-- | @Selector@ for @textShouldEndEditing:@
textShouldEndEditingSelector :: Selector '[Id NSText] Bool
textShouldEndEditingSelector = mkSelector "textShouldEndEditing:"

-- | @Selector@ for @textDidBeginEditing:@
textDidBeginEditingSelector :: Selector '[Id NSNotification] ()
textDidBeginEditingSelector = mkSelector "textDidBeginEditing:"

-- | @Selector@ for @textDidEndEditing:@
textDidEndEditingSelector :: Selector '[Id NSNotification] ()
textDidEndEditingSelector = mkSelector "textDidEndEditing:"

-- | @Selector@ for @textDidChange:@
textDidChangeSelector :: Selector '[Id NSNotification] ()
textDidChangeSelector = mkSelector "textDidChange:"

-- | @Selector@ for @selectText:@
selectTextSelector :: Selector '[RawId] ()
selectTextSelector = mkSelector "selectText:"

-- | @Selector@ for @selectTextAtRow:column:@
selectTextAtRow_columnSelector :: Selector '[CLong, CLong] (Id NSCell)
selectTextAtRow_columnSelector = mkSelector "selectTextAtRow:column:"

-- | @Selector@ for @acceptsFirstMouse:@
acceptsFirstMouseSelector :: Selector '[Id NSEvent] Bool
acceptsFirstMouseSelector = mkSelector "acceptsFirstMouse:"

-- | @Selector@ for @resetCursorRects@
resetCursorRectsSelector :: Selector '[] ()
resetCursorRectsSelector = mkSelector "resetCursorRects"

-- | @Selector@ for @setToolTip:forCell:@
setToolTip_forCellSelector :: Selector '[Id NSString, Id NSCell] ()
setToolTip_forCellSelector = mkSelector "setToolTip:forCell:"

-- | @Selector@ for @toolTipForCell:@
toolTipForCellSelector :: Selector '[Id NSCell] (Id NSString)
toolTipForCellSelector = mkSelector "toolTipForCell:"

-- | @Selector@ for @cellClass@
cellClassSelector :: Selector '[] Class
cellClassSelector = mkSelector "cellClass"

-- | @Selector@ for @setCellClass:@
setCellClassSelector :: Selector '[Class] ()
setCellClassSelector = mkSelector "setCellClass:"

-- | @Selector@ for @prototype@
prototypeSelector :: Selector '[] (Id NSCell)
prototypeSelector = mkSelector "prototype"

-- | @Selector@ for @setPrototype:@
setPrototypeSelector :: Selector '[Id NSCell] ()
setPrototypeSelector = mkSelector "setPrototype:"

-- | @Selector@ for @mode@
modeSelector :: Selector '[] NSMatrixMode
modeSelector = mkSelector "mode"

-- | @Selector@ for @setMode:@
setModeSelector :: Selector '[NSMatrixMode] ()
setModeSelector = mkSelector "setMode:"

-- | @Selector@ for @allowsEmptySelection@
allowsEmptySelectionSelector :: Selector '[] Bool
allowsEmptySelectionSelector = mkSelector "allowsEmptySelection"

-- | @Selector@ for @setAllowsEmptySelection:@
setAllowsEmptySelectionSelector :: Selector '[Bool] ()
setAllowsEmptySelectionSelector = mkSelector "setAllowsEmptySelection:"

-- | @Selector@ for @cells@
cellsSelector :: Selector '[] (Id NSArray)
cellsSelector = mkSelector "cells"

-- | @Selector@ for @selectedCell@
selectedCellSelector :: Selector '[] (Id NSCell)
selectedCellSelector = mkSelector "selectedCell"

-- | @Selector@ for @selectedCells@
selectedCellsSelector :: Selector '[] (Id NSArray)
selectedCellsSelector = mkSelector "selectedCells"

-- | @Selector@ for @selectedRow@
selectedRowSelector :: Selector '[] CLong
selectedRowSelector = mkSelector "selectedRow"

-- | @Selector@ for @selectedColumn@
selectedColumnSelector :: Selector '[] CLong
selectedColumnSelector = mkSelector "selectedColumn"

-- | @Selector@ for @selectionByRect@
selectionByRectSelector :: Selector '[] Bool
selectionByRectSelector = mkSelector "selectionByRect"

-- | @Selector@ for @setSelectionByRect:@
setSelectionByRectSelector :: Selector '[Bool] ()
setSelectionByRectSelector = mkSelector "setSelectionByRect:"

-- | @Selector@ for @cellSize@
cellSizeSelector :: Selector '[] NSSize
cellSizeSelector = mkSelector "cellSize"

-- | @Selector@ for @setCellSize:@
setCellSizeSelector :: Selector '[NSSize] ()
setCellSizeSelector = mkSelector "setCellSize:"

-- | @Selector@ for @intercellSpacing@
intercellSpacingSelector :: Selector '[] NSSize
intercellSpacingSelector = mkSelector "intercellSpacing"

-- | @Selector@ for @setIntercellSpacing:@
setIntercellSpacingSelector :: Selector '[NSSize] ()
setIntercellSpacingSelector = mkSelector "setIntercellSpacing:"

-- | @Selector@ for @backgroundColor@
backgroundColorSelector :: Selector '[] (Id NSColor)
backgroundColorSelector = mkSelector "backgroundColor"

-- | @Selector@ for @setBackgroundColor:@
setBackgroundColorSelector :: Selector '[Id NSColor] ()
setBackgroundColorSelector = mkSelector "setBackgroundColor:"

-- | @Selector@ for @cellBackgroundColor@
cellBackgroundColorSelector :: Selector '[] (Id NSColor)
cellBackgroundColorSelector = mkSelector "cellBackgroundColor"

-- | @Selector@ for @setCellBackgroundColor:@
setCellBackgroundColorSelector :: Selector '[Id NSColor] ()
setCellBackgroundColorSelector = mkSelector "setCellBackgroundColor:"

-- | @Selector@ for @drawsCellBackground@
drawsCellBackgroundSelector :: Selector '[] Bool
drawsCellBackgroundSelector = mkSelector "drawsCellBackground"

-- | @Selector@ for @setDrawsCellBackground:@
setDrawsCellBackgroundSelector :: Selector '[Bool] ()
setDrawsCellBackgroundSelector = mkSelector "setDrawsCellBackground:"

-- | @Selector@ for @drawsBackground@
drawsBackgroundSelector :: Selector '[] Bool
drawsBackgroundSelector = mkSelector "drawsBackground"

-- | @Selector@ for @setDrawsBackground:@
setDrawsBackgroundSelector :: Selector '[Bool] ()
setDrawsBackgroundSelector = mkSelector "setDrawsBackground:"

-- | @Selector@ for @numberOfRows@
numberOfRowsSelector :: Selector '[] CLong
numberOfRowsSelector = mkSelector "numberOfRows"

-- | @Selector@ for @numberOfColumns@
numberOfColumnsSelector :: Selector '[] CLong
numberOfColumnsSelector = mkSelector "numberOfColumns"

-- | @Selector@ for @doubleAction@
doubleActionSelector :: Selector '[] Sel
doubleActionSelector = mkSelector "doubleAction"

-- | @Selector@ for @setDoubleAction:@
setDoubleActionSelector :: Selector '[Sel] ()
setDoubleActionSelector = mkSelector "setDoubleAction:"

-- | @Selector@ for @autosizesCells@
autosizesCellsSelector :: Selector '[] Bool
autosizesCellsSelector = mkSelector "autosizesCells"

-- | @Selector@ for @setAutosizesCells:@
setAutosizesCellsSelector :: Selector '[Bool] ()
setAutosizesCellsSelector = mkSelector "setAutosizesCells:"

-- | @Selector@ for @autoscroll@
autoscrollSelector :: Selector '[] Bool
autoscrollSelector = mkSelector "autoscroll"

-- | @Selector@ for @setAutoscroll:@
setAutoscrollSelector :: Selector '[Bool] ()
setAutoscrollSelector = mkSelector "setAutoscroll:"

-- | @Selector@ for @mouseDownFlags@
mouseDownFlagsSelector :: Selector '[] CLong
mouseDownFlagsSelector = mkSelector "mouseDownFlags"

-- | @Selector@ for @delegate@
delegateSelector :: Selector '[] RawId
delegateSelector = mkSelector "delegate"

-- | @Selector@ for @setDelegate:@
setDelegateSelector :: Selector '[RawId] ()
setDelegateSelector = mkSelector "setDelegate:"

-- | @Selector@ for @autorecalculatesCellSize@
autorecalculatesCellSizeSelector :: Selector '[] Bool
autorecalculatesCellSizeSelector = mkSelector "autorecalculatesCellSize"

-- | @Selector@ for @setAutorecalculatesCellSize:@
setAutorecalculatesCellSizeSelector :: Selector '[Bool] ()
setAutorecalculatesCellSizeSelector = mkSelector "setAutorecalculatesCellSize:"

-- | @Selector@ for @tabKeyTraversesCells@
tabKeyTraversesCellsSelector :: Selector '[] Bool
tabKeyTraversesCellsSelector = mkSelector "tabKeyTraversesCells"

-- | @Selector@ for @setTabKeyTraversesCells:@
setTabKeyTraversesCellsSelector :: Selector '[Bool] ()
setTabKeyTraversesCellsSelector = mkSelector "setTabKeyTraversesCells:"

-- | @Selector@ for @keyCell@
keyCellSelector :: Selector '[] (Id NSCell)
keyCellSelector = mkSelector "keyCell"

-- | @Selector@ for @setKeyCell:@
setKeyCellSelector :: Selector '[Id NSCell] ()
setKeyCellSelector = mkSelector "setKeyCell:"

