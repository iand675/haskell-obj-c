{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Generated bindings for @NSBrowser@.
module ObjC.AppKit.NSBrowser
  ( NSBrowser
  , IsNSBrowser(..)
  , loadColumnZero
  , setCellClass
  , itemAtIndexPath
  , itemAtRow_inColumn
  , indexPathForColumn
  , isLeafItem
  , reloadDataForRowIndexes_inColumn
  , parentForItemsInColumn
  , scrollRowToVisible_inColumn
  , setTitle_ofColumn
  , titleOfColumn
  , setPath
  , path
  , pathToColumn
  , selectedCellInColumn
  , selectRow_inColumn
  , selectedRowInColumn
  , selectRowIndexes_inColumn
  , selectedRowIndexesInColumn
  , reloadColumn
  , validateVisibleColumns
  , scrollColumnsRightBy
  , scrollColumnsLeftBy
  , scrollColumnToVisible
  , addColumn
  , loadedCellAtRow_column
  , selectAll
  , tile
  , doClick
  , doDoubleClick
  , sendAction
  , titleFrameOfColumn
  , drawTitleOfColumn_inRect
  , frameOfColumn
  , frameOfInsideOfColumn
  , frameOfRow_inColumn
  , getRow_column_forPoint
  , columnWidthForColumnContentWidth
  , columnContentWidthForColumnWidth
  , setWidth_ofColumn
  , widthOfColumn
  , noteHeightOfRowsWithIndexesChanged_inColumn
  , setDefaultColumnWidth
  , defaultColumnWidth
  , removeSavedColumnsWithAutosaveName
  , canDragRowsWithIndexes_inColumn_withEvent
  , draggingImageForRowsWithIndexes_inColumn_withEvent_offset
  , setDraggingSourceOperationMask_forLocal
  , editItemAtIndexPath_withEvent_select
  , setAcceptsArrowKeys
  , acceptsArrowKeys
  , displayColumn
  , displayAllColumns
  , scrollViaScroller
  , updateScroller
  , setMatrixClass
  , matrixClass
  , columnOfMatrix
  , matrixInColumn
  , cellClass
  , loaded
  , doubleAction
  , setDoubleAction
  , cellPrototype
  , setCellPrototype
  , delegate
  , setDelegate
  , reusesColumns
  , setReusesColumns
  , hasHorizontalScroller
  , setHasHorizontalScroller
  , autohidesScroller
  , setAutohidesScroller
  , separatesColumns
  , setSeparatesColumns
  , titled
  , setTitled
  , minColumnWidth
  , setMinColumnWidth
  , maxVisibleColumns
  , setMaxVisibleColumns
  , allowsMultipleSelection
  , setAllowsMultipleSelection
  , allowsBranchSelection
  , setAllowsBranchSelection
  , allowsEmptySelection
  , setAllowsEmptySelection
  , takesTitleFromPreviousColumn
  , setTakesTitleFromPreviousColumn
  , sendsActionOnArrowKeys
  , setSendsActionOnArrowKeys
  , pathSeparator
  , setPathSeparator
  , clickedColumn
  , clickedRow
  , selectedColumn
  , selectedCell
  , selectedCells
  , selectionIndexPath
  , setSelectionIndexPath
  , selectionIndexPaths
  , setSelectionIndexPaths
  , lastColumn
  , setLastColumn
  , numberOfVisibleColumns
  , firstVisibleColumn
  , lastVisibleColumn
  , titleHeight
  , columnResizingType
  , setColumnResizingType
  , prefersAllColumnUserResizing
  , setPrefersAllColumnUserResizing
  , rowHeight
  , setRowHeight
  , columnsAutosaveName
  , setColumnsAutosaveName
  , allowsTypeSelect
  , setAllowsTypeSelect
  , backgroundColor
  , setBackgroundColor
  , acceptsArrowKeysSelector
  , addColumnSelector
  , allowsBranchSelectionSelector
  , allowsEmptySelectionSelector
  , allowsMultipleSelectionSelector
  , allowsTypeSelectSelector
  , autohidesScrollerSelector
  , backgroundColorSelector
  , canDragRowsWithIndexes_inColumn_withEventSelector
  , cellClassSelector
  , cellPrototypeSelector
  , clickedColumnSelector
  , clickedRowSelector
  , columnContentWidthForColumnWidthSelector
  , columnOfMatrixSelector
  , columnResizingTypeSelector
  , columnWidthForColumnContentWidthSelector
  , columnsAutosaveNameSelector
  , defaultColumnWidthSelector
  , delegateSelector
  , displayAllColumnsSelector
  , displayColumnSelector
  , doClickSelector
  , doDoubleClickSelector
  , doubleActionSelector
  , draggingImageForRowsWithIndexes_inColumn_withEvent_offsetSelector
  , drawTitleOfColumn_inRectSelector
  , editItemAtIndexPath_withEvent_selectSelector
  , firstVisibleColumnSelector
  , frameOfColumnSelector
  , frameOfInsideOfColumnSelector
  , frameOfRow_inColumnSelector
  , getRow_column_forPointSelector
  , hasHorizontalScrollerSelector
  , indexPathForColumnSelector
  , isLeafItemSelector
  , itemAtIndexPathSelector
  , itemAtRow_inColumnSelector
  , lastColumnSelector
  , lastVisibleColumnSelector
  , loadColumnZeroSelector
  , loadedCellAtRow_columnSelector
  , loadedSelector
  , matrixClassSelector
  , matrixInColumnSelector
  , maxVisibleColumnsSelector
  , minColumnWidthSelector
  , noteHeightOfRowsWithIndexesChanged_inColumnSelector
  , numberOfVisibleColumnsSelector
  , parentForItemsInColumnSelector
  , pathSelector
  , pathSeparatorSelector
  , pathToColumnSelector
  , prefersAllColumnUserResizingSelector
  , reloadColumnSelector
  , reloadDataForRowIndexes_inColumnSelector
  , removeSavedColumnsWithAutosaveNameSelector
  , reusesColumnsSelector
  , rowHeightSelector
  , scrollColumnToVisibleSelector
  , scrollColumnsLeftBySelector
  , scrollColumnsRightBySelector
  , scrollRowToVisible_inColumnSelector
  , scrollViaScrollerSelector
  , selectAllSelector
  , selectRowIndexes_inColumnSelector
  , selectRow_inColumnSelector
  , selectedCellInColumnSelector
  , selectedCellSelector
  , selectedCellsSelector
  , selectedColumnSelector
  , selectedRowInColumnSelector
  , selectedRowIndexesInColumnSelector
  , selectionIndexPathSelector
  , selectionIndexPathsSelector
  , sendActionSelector
  , sendsActionOnArrowKeysSelector
  , separatesColumnsSelector
  , setAcceptsArrowKeysSelector
  , setAllowsBranchSelectionSelector
  , setAllowsEmptySelectionSelector
  , setAllowsMultipleSelectionSelector
  , setAllowsTypeSelectSelector
  , setAutohidesScrollerSelector
  , setBackgroundColorSelector
  , setCellClassSelector
  , setCellPrototypeSelector
  , setColumnResizingTypeSelector
  , setColumnsAutosaveNameSelector
  , setDefaultColumnWidthSelector
  , setDelegateSelector
  , setDoubleActionSelector
  , setDraggingSourceOperationMask_forLocalSelector
  , setHasHorizontalScrollerSelector
  , setLastColumnSelector
  , setMatrixClassSelector
  , setMaxVisibleColumnsSelector
  , setMinColumnWidthSelector
  , setPathSelector
  , setPathSeparatorSelector
  , setPrefersAllColumnUserResizingSelector
  , setReusesColumnsSelector
  , setRowHeightSelector
  , setSelectionIndexPathSelector
  , setSelectionIndexPathsSelector
  , setSendsActionOnArrowKeysSelector
  , setSeparatesColumnsSelector
  , setTakesTitleFromPreviousColumnSelector
  , setTitle_ofColumnSelector
  , setTitledSelector
  , setWidth_ofColumnSelector
  , takesTitleFromPreviousColumnSelector
  , tileSelector
  , titleFrameOfColumnSelector
  , titleHeightSelector
  , titleOfColumnSelector
  , titledSelector
  , updateScrollerSelector
  , validateVisibleColumnsSelector
  , widthOfColumnSelector

  -- * Enum types
  , NSBrowserColumnResizingType(NSBrowserColumnResizingType)
  , pattern NSBrowserNoColumnResizing
  , pattern NSBrowserAutoColumnResizing
  , pattern NSBrowserUserColumnResizing
  , NSDragOperation(NSDragOperation)
  , pattern NSDragOperationNone
  , pattern NSDragOperationCopy
  , pattern NSDragOperationLink
  , pattern NSDragOperationGeneric
  , pattern NSDragOperationPrivate
  , pattern NSDragOperationMove
  , pattern NSDragOperationDelete
  , pattern NSDragOperationEvery
  , pattern NSDragOperationAll_Obsolete
  , pattern NSDragOperationAll

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

-- | @- loadColumnZero@
loadColumnZero :: IsNSBrowser nsBrowser => nsBrowser -> IO ()
loadColumnZero nsBrowser =
  sendMessage nsBrowser loadColumnZeroSelector

-- | @- setCellClass:@
setCellClass :: IsNSBrowser nsBrowser => nsBrowser -> Class -> IO ()
setCellClass nsBrowser factoryId =
  sendMessage nsBrowser setCellClassSelector factoryId

-- | @- itemAtIndexPath:@
itemAtIndexPath :: (IsNSBrowser nsBrowser, IsNSIndexPath indexPath) => nsBrowser -> indexPath -> IO RawId
itemAtIndexPath nsBrowser indexPath =
  sendMessage nsBrowser itemAtIndexPathSelector (toNSIndexPath indexPath)

-- | @- itemAtRow:inColumn:@
itemAtRow_inColumn :: IsNSBrowser nsBrowser => nsBrowser -> CLong -> CLong -> IO RawId
itemAtRow_inColumn nsBrowser row column =
  sendMessage nsBrowser itemAtRow_inColumnSelector row column

-- | @- indexPathForColumn:@
indexPathForColumn :: IsNSBrowser nsBrowser => nsBrowser -> CLong -> IO (Id NSIndexPath)
indexPathForColumn nsBrowser column =
  sendMessage nsBrowser indexPathForColumnSelector column

-- | @- isLeafItem:@
isLeafItem :: IsNSBrowser nsBrowser => nsBrowser -> RawId -> IO Bool
isLeafItem nsBrowser item =
  sendMessage nsBrowser isLeafItemSelector item

-- | @- reloadDataForRowIndexes:inColumn:@
reloadDataForRowIndexes_inColumn :: (IsNSBrowser nsBrowser, IsNSIndexSet rowIndexes) => nsBrowser -> rowIndexes -> CLong -> IO ()
reloadDataForRowIndexes_inColumn nsBrowser rowIndexes column =
  sendMessage nsBrowser reloadDataForRowIndexes_inColumnSelector (toNSIndexSet rowIndexes) column

-- | @- parentForItemsInColumn:@
parentForItemsInColumn :: IsNSBrowser nsBrowser => nsBrowser -> CLong -> IO RawId
parentForItemsInColumn nsBrowser column =
  sendMessage nsBrowser parentForItemsInColumnSelector column

-- | @- scrollRowToVisible:inColumn:@
scrollRowToVisible_inColumn :: IsNSBrowser nsBrowser => nsBrowser -> CLong -> CLong -> IO ()
scrollRowToVisible_inColumn nsBrowser row column =
  sendMessage nsBrowser scrollRowToVisible_inColumnSelector row column

-- | @- setTitle:ofColumn:@
setTitle_ofColumn :: (IsNSBrowser nsBrowser, IsNSString string) => nsBrowser -> string -> CLong -> IO ()
setTitle_ofColumn nsBrowser string column =
  sendMessage nsBrowser setTitle_ofColumnSelector (toNSString string) column

-- | @- titleOfColumn:@
titleOfColumn :: IsNSBrowser nsBrowser => nsBrowser -> CLong -> IO (Id NSString)
titleOfColumn nsBrowser column =
  sendMessage nsBrowser titleOfColumnSelector column

-- | @- setPath:@
setPath :: (IsNSBrowser nsBrowser, IsNSString path) => nsBrowser -> path -> IO Bool
setPath nsBrowser path =
  sendMessage nsBrowser setPathSelector (toNSString path)

-- | @- path@
path :: IsNSBrowser nsBrowser => nsBrowser -> IO (Id NSString)
path nsBrowser =
  sendMessage nsBrowser pathSelector

-- | @- pathToColumn:@
pathToColumn :: IsNSBrowser nsBrowser => nsBrowser -> CLong -> IO (Id NSString)
pathToColumn nsBrowser column =
  sendMessage nsBrowser pathToColumnSelector column

-- | @- selectedCellInColumn:@
selectedCellInColumn :: IsNSBrowser nsBrowser => nsBrowser -> CLong -> IO RawId
selectedCellInColumn nsBrowser column =
  sendMessage nsBrowser selectedCellInColumnSelector column

-- | @- selectRow:inColumn:@
selectRow_inColumn :: IsNSBrowser nsBrowser => nsBrowser -> CLong -> CLong -> IO ()
selectRow_inColumn nsBrowser row column =
  sendMessage nsBrowser selectRow_inColumnSelector row column

-- | @- selectedRowInColumn:@
selectedRowInColumn :: IsNSBrowser nsBrowser => nsBrowser -> CLong -> IO CLong
selectedRowInColumn nsBrowser column =
  sendMessage nsBrowser selectedRowInColumnSelector column

-- | @- selectRowIndexes:inColumn:@
selectRowIndexes_inColumn :: (IsNSBrowser nsBrowser, IsNSIndexSet indexes) => nsBrowser -> indexes -> CLong -> IO ()
selectRowIndexes_inColumn nsBrowser indexes column =
  sendMessage nsBrowser selectRowIndexes_inColumnSelector (toNSIndexSet indexes) column

-- | @- selectedRowIndexesInColumn:@
selectedRowIndexesInColumn :: IsNSBrowser nsBrowser => nsBrowser -> CLong -> IO (Id NSIndexSet)
selectedRowIndexesInColumn nsBrowser column =
  sendMessage nsBrowser selectedRowIndexesInColumnSelector column

-- | @- reloadColumn:@
reloadColumn :: IsNSBrowser nsBrowser => nsBrowser -> CLong -> IO ()
reloadColumn nsBrowser column =
  sendMessage nsBrowser reloadColumnSelector column

-- | @- validateVisibleColumns@
validateVisibleColumns :: IsNSBrowser nsBrowser => nsBrowser -> IO ()
validateVisibleColumns nsBrowser =
  sendMessage nsBrowser validateVisibleColumnsSelector

-- | @- scrollColumnsRightBy:@
scrollColumnsRightBy :: IsNSBrowser nsBrowser => nsBrowser -> CLong -> IO ()
scrollColumnsRightBy nsBrowser shiftAmount =
  sendMessage nsBrowser scrollColumnsRightBySelector shiftAmount

-- | @- scrollColumnsLeftBy:@
scrollColumnsLeftBy :: IsNSBrowser nsBrowser => nsBrowser -> CLong -> IO ()
scrollColumnsLeftBy nsBrowser shiftAmount =
  sendMessage nsBrowser scrollColumnsLeftBySelector shiftAmount

-- | @- scrollColumnToVisible:@
scrollColumnToVisible :: IsNSBrowser nsBrowser => nsBrowser -> CLong -> IO ()
scrollColumnToVisible nsBrowser column =
  sendMessage nsBrowser scrollColumnToVisibleSelector column

-- | @- addColumn@
addColumn :: IsNSBrowser nsBrowser => nsBrowser -> IO ()
addColumn nsBrowser =
  sendMessage nsBrowser addColumnSelector

-- | @- loadedCellAtRow:column:@
loadedCellAtRow_column :: IsNSBrowser nsBrowser => nsBrowser -> CLong -> CLong -> IO RawId
loadedCellAtRow_column nsBrowser row col =
  sendMessage nsBrowser loadedCellAtRow_columnSelector row col

-- | @- selectAll:@
selectAll :: IsNSBrowser nsBrowser => nsBrowser -> RawId -> IO ()
selectAll nsBrowser sender =
  sendMessage nsBrowser selectAllSelector sender

-- | @- tile@
tile :: IsNSBrowser nsBrowser => nsBrowser -> IO ()
tile nsBrowser =
  sendMessage nsBrowser tileSelector

-- | @- doClick:@
doClick :: IsNSBrowser nsBrowser => nsBrowser -> RawId -> IO ()
doClick nsBrowser sender =
  sendMessage nsBrowser doClickSelector sender

-- | @- doDoubleClick:@
doDoubleClick :: IsNSBrowser nsBrowser => nsBrowser -> RawId -> IO ()
doDoubleClick nsBrowser sender =
  sendMessage nsBrowser doDoubleClickSelector sender

-- | @- sendAction@
sendAction :: IsNSBrowser nsBrowser => nsBrowser -> IO Bool
sendAction nsBrowser =
  sendMessage nsBrowser sendActionSelector

-- | @- titleFrameOfColumn:@
titleFrameOfColumn :: IsNSBrowser nsBrowser => nsBrowser -> CLong -> IO NSRect
titleFrameOfColumn nsBrowser column =
  sendMessage nsBrowser titleFrameOfColumnSelector column

-- | @- drawTitleOfColumn:inRect:@
drawTitleOfColumn_inRect :: IsNSBrowser nsBrowser => nsBrowser -> CLong -> NSRect -> IO ()
drawTitleOfColumn_inRect nsBrowser column rect =
  sendMessage nsBrowser drawTitleOfColumn_inRectSelector column rect

-- | @- frameOfColumn:@
frameOfColumn :: IsNSBrowser nsBrowser => nsBrowser -> CLong -> IO NSRect
frameOfColumn nsBrowser column =
  sendMessage nsBrowser frameOfColumnSelector column

-- | @- frameOfInsideOfColumn:@
frameOfInsideOfColumn :: IsNSBrowser nsBrowser => nsBrowser -> CLong -> IO NSRect
frameOfInsideOfColumn nsBrowser column =
  sendMessage nsBrowser frameOfInsideOfColumnSelector column

-- | @- frameOfRow:inColumn:@
frameOfRow_inColumn :: IsNSBrowser nsBrowser => nsBrowser -> CLong -> CLong -> IO NSRect
frameOfRow_inColumn nsBrowser row column =
  sendMessage nsBrowser frameOfRow_inColumnSelector row column

-- | @- getRow:column:forPoint:@
getRow_column_forPoint :: IsNSBrowser nsBrowser => nsBrowser -> Ptr CLong -> Ptr CLong -> NSPoint -> IO Bool
getRow_column_forPoint nsBrowser row column point =
  sendMessage nsBrowser getRow_column_forPointSelector row column point

-- | @- columnWidthForColumnContentWidth:@
columnWidthForColumnContentWidth :: IsNSBrowser nsBrowser => nsBrowser -> CDouble -> IO CDouble
columnWidthForColumnContentWidth nsBrowser columnContentWidth =
  sendMessage nsBrowser columnWidthForColumnContentWidthSelector columnContentWidth

-- | @- columnContentWidthForColumnWidth:@
columnContentWidthForColumnWidth :: IsNSBrowser nsBrowser => nsBrowser -> CDouble -> IO CDouble
columnContentWidthForColumnWidth nsBrowser columnWidth =
  sendMessage nsBrowser columnContentWidthForColumnWidthSelector columnWidth

-- | @- setWidth:ofColumn:@
setWidth_ofColumn :: IsNSBrowser nsBrowser => nsBrowser -> CDouble -> CLong -> IO ()
setWidth_ofColumn nsBrowser columnWidth columnIndex =
  sendMessage nsBrowser setWidth_ofColumnSelector columnWidth columnIndex

-- | @- widthOfColumn:@
widthOfColumn :: IsNSBrowser nsBrowser => nsBrowser -> CLong -> IO CDouble
widthOfColumn nsBrowser column =
  sendMessage nsBrowser widthOfColumnSelector column

-- | @- noteHeightOfRowsWithIndexesChanged:inColumn:@
noteHeightOfRowsWithIndexesChanged_inColumn :: (IsNSBrowser nsBrowser, IsNSIndexSet indexSet) => nsBrowser -> indexSet -> CLong -> IO ()
noteHeightOfRowsWithIndexesChanged_inColumn nsBrowser indexSet columnIndex =
  sendMessage nsBrowser noteHeightOfRowsWithIndexesChanged_inColumnSelector (toNSIndexSet indexSet) columnIndex

-- | @- setDefaultColumnWidth:@
setDefaultColumnWidth :: IsNSBrowser nsBrowser => nsBrowser -> CDouble -> IO ()
setDefaultColumnWidth nsBrowser columnWidth =
  sendMessage nsBrowser setDefaultColumnWidthSelector columnWidth

-- | @- defaultColumnWidth@
defaultColumnWidth :: IsNSBrowser nsBrowser => nsBrowser -> IO CDouble
defaultColumnWidth nsBrowser =
  sendMessage nsBrowser defaultColumnWidthSelector

-- | @+ removeSavedColumnsWithAutosaveName:@
removeSavedColumnsWithAutosaveName :: IsNSString name => name -> IO ()
removeSavedColumnsWithAutosaveName name =
  do
    cls' <- getRequiredClass "NSBrowser"
    sendClassMessage cls' removeSavedColumnsWithAutosaveNameSelector (toNSString name)

-- | @- canDragRowsWithIndexes:inColumn:withEvent:@
canDragRowsWithIndexes_inColumn_withEvent :: (IsNSBrowser nsBrowser, IsNSIndexSet rowIndexes, IsNSEvent event) => nsBrowser -> rowIndexes -> CLong -> event -> IO Bool
canDragRowsWithIndexes_inColumn_withEvent nsBrowser rowIndexes column event =
  sendMessage nsBrowser canDragRowsWithIndexes_inColumn_withEventSelector (toNSIndexSet rowIndexes) column (toNSEvent event)

-- | @- draggingImageForRowsWithIndexes:inColumn:withEvent:offset:@
draggingImageForRowsWithIndexes_inColumn_withEvent_offset :: (IsNSBrowser nsBrowser, IsNSIndexSet rowIndexes, IsNSEvent event) => nsBrowser -> rowIndexes -> CLong -> event -> Ptr NSPoint -> IO (Id NSImage)
draggingImageForRowsWithIndexes_inColumn_withEvent_offset nsBrowser rowIndexes column event dragImageOffset =
  sendMessage nsBrowser draggingImageForRowsWithIndexes_inColumn_withEvent_offsetSelector (toNSIndexSet rowIndexes) column (toNSEvent event) dragImageOffset

-- | @- setDraggingSourceOperationMask:forLocal:@
setDraggingSourceOperationMask_forLocal :: IsNSBrowser nsBrowser => nsBrowser -> NSDragOperation -> Bool -> IO ()
setDraggingSourceOperationMask_forLocal nsBrowser mask isLocal =
  sendMessage nsBrowser setDraggingSourceOperationMask_forLocalSelector mask isLocal

-- | @- editItemAtIndexPath:withEvent:select:@
editItemAtIndexPath_withEvent_select :: (IsNSBrowser nsBrowser, IsNSIndexPath indexPath, IsNSEvent event) => nsBrowser -> indexPath -> event -> Bool -> IO ()
editItemAtIndexPath_withEvent_select nsBrowser indexPath event select =
  sendMessage nsBrowser editItemAtIndexPath_withEvent_selectSelector (toNSIndexPath indexPath) (toNSEvent event) select

-- | @- setAcceptsArrowKeys:@
setAcceptsArrowKeys :: IsNSBrowser nsBrowser => nsBrowser -> Bool -> IO ()
setAcceptsArrowKeys nsBrowser flag =
  sendMessage nsBrowser setAcceptsArrowKeysSelector flag

-- | @- acceptsArrowKeys@
acceptsArrowKeys :: IsNSBrowser nsBrowser => nsBrowser -> IO Bool
acceptsArrowKeys nsBrowser =
  sendMessage nsBrowser acceptsArrowKeysSelector

-- | @- displayColumn:@
displayColumn :: IsNSBrowser nsBrowser => nsBrowser -> CLong -> IO ()
displayColumn nsBrowser column =
  sendMessage nsBrowser displayColumnSelector column

-- | @- displayAllColumns@
displayAllColumns :: IsNSBrowser nsBrowser => nsBrowser -> IO ()
displayAllColumns nsBrowser =
  sendMessage nsBrowser displayAllColumnsSelector

-- | @- scrollViaScroller:@
scrollViaScroller :: (IsNSBrowser nsBrowser, IsNSScroller sender) => nsBrowser -> sender -> IO ()
scrollViaScroller nsBrowser sender =
  sendMessage nsBrowser scrollViaScrollerSelector (toNSScroller sender)

-- | @- updateScroller@
updateScroller :: IsNSBrowser nsBrowser => nsBrowser -> IO ()
updateScroller nsBrowser =
  sendMessage nsBrowser updateScrollerSelector

-- | @- setMatrixClass:@
setMatrixClass :: IsNSBrowser nsBrowser => nsBrowser -> Class -> IO ()
setMatrixClass nsBrowser factoryId =
  sendMessage nsBrowser setMatrixClassSelector factoryId

-- | @- matrixClass@
matrixClass :: IsNSBrowser nsBrowser => nsBrowser -> IO Class
matrixClass nsBrowser =
  sendMessage nsBrowser matrixClassSelector

-- | @- columnOfMatrix:@
columnOfMatrix :: (IsNSBrowser nsBrowser, IsNSMatrix matrix) => nsBrowser -> matrix -> IO CLong
columnOfMatrix nsBrowser matrix =
  sendMessage nsBrowser columnOfMatrixSelector (toNSMatrix matrix)

-- | @- matrixInColumn:@
matrixInColumn :: IsNSBrowser nsBrowser => nsBrowser -> CLong -> IO (Id NSMatrix)
matrixInColumn nsBrowser column =
  sendMessage nsBrowser matrixInColumnSelector column

-- | @+ cellClass@
cellClass :: IO Class
cellClass  =
  do
    cls' <- getRequiredClass "NSBrowser"
    sendClassMessage cls' cellClassSelector

-- | @- loaded@
loaded :: IsNSBrowser nsBrowser => nsBrowser -> IO Bool
loaded nsBrowser =
  sendMessage nsBrowser loadedSelector

-- | @- doubleAction@
doubleAction :: IsNSBrowser nsBrowser => nsBrowser -> IO Sel
doubleAction nsBrowser =
  sendMessage nsBrowser doubleActionSelector

-- | @- setDoubleAction:@
setDoubleAction :: IsNSBrowser nsBrowser => nsBrowser -> Sel -> IO ()
setDoubleAction nsBrowser value =
  sendMessage nsBrowser setDoubleActionSelector value

-- | @- cellPrototype@
cellPrototype :: IsNSBrowser nsBrowser => nsBrowser -> IO RawId
cellPrototype nsBrowser =
  sendMessage nsBrowser cellPrototypeSelector

-- | @- setCellPrototype:@
setCellPrototype :: IsNSBrowser nsBrowser => nsBrowser -> RawId -> IO ()
setCellPrototype nsBrowser value =
  sendMessage nsBrowser setCellPrototypeSelector value

-- | @- delegate@
delegate :: IsNSBrowser nsBrowser => nsBrowser -> IO RawId
delegate nsBrowser =
  sendMessage nsBrowser delegateSelector

-- | @- setDelegate:@
setDelegate :: IsNSBrowser nsBrowser => nsBrowser -> RawId -> IO ()
setDelegate nsBrowser value =
  sendMessage nsBrowser setDelegateSelector value

-- | @- reusesColumns@
reusesColumns :: IsNSBrowser nsBrowser => nsBrowser -> IO Bool
reusesColumns nsBrowser =
  sendMessage nsBrowser reusesColumnsSelector

-- | @- setReusesColumns:@
setReusesColumns :: IsNSBrowser nsBrowser => nsBrowser -> Bool -> IO ()
setReusesColumns nsBrowser value =
  sendMessage nsBrowser setReusesColumnsSelector value

-- | @- hasHorizontalScroller@
hasHorizontalScroller :: IsNSBrowser nsBrowser => nsBrowser -> IO Bool
hasHorizontalScroller nsBrowser =
  sendMessage nsBrowser hasHorizontalScrollerSelector

-- | @- setHasHorizontalScroller:@
setHasHorizontalScroller :: IsNSBrowser nsBrowser => nsBrowser -> Bool -> IO ()
setHasHorizontalScroller nsBrowser value =
  sendMessage nsBrowser setHasHorizontalScrollerSelector value

-- | @- autohidesScroller@
autohidesScroller :: IsNSBrowser nsBrowser => nsBrowser -> IO Bool
autohidesScroller nsBrowser =
  sendMessage nsBrowser autohidesScrollerSelector

-- | @- setAutohidesScroller:@
setAutohidesScroller :: IsNSBrowser nsBrowser => nsBrowser -> Bool -> IO ()
setAutohidesScroller nsBrowser value =
  sendMessage nsBrowser setAutohidesScrollerSelector value

-- | @- separatesColumns@
separatesColumns :: IsNSBrowser nsBrowser => nsBrowser -> IO Bool
separatesColumns nsBrowser =
  sendMessage nsBrowser separatesColumnsSelector

-- | @- setSeparatesColumns:@
setSeparatesColumns :: IsNSBrowser nsBrowser => nsBrowser -> Bool -> IO ()
setSeparatesColumns nsBrowser value =
  sendMessage nsBrowser setSeparatesColumnsSelector value

-- | @- titled@
titled :: IsNSBrowser nsBrowser => nsBrowser -> IO Bool
titled nsBrowser =
  sendMessage nsBrowser titledSelector

-- | @- setTitled:@
setTitled :: IsNSBrowser nsBrowser => nsBrowser -> Bool -> IO ()
setTitled nsBrowser value =
  sendMessage nsBrowser setTitledSelector value

-- | @- minColumnWidth@
minColumnWidth :: IsNSBrowser nsBrowser => nsBrowser -> IO CDouble
minColumnWidth nsBrowser =
  sendMessage nsBrowser minColumnWidthSelector

-- | @- setMinColumnWidth:@
setMinColumnWidth :: IsNSBrowser nsBrowser => nsBrowser -> CDouble -> IO ()
setMinColumnWidth nsBrowser value =
  sendMessage nsBrowser setMinColumnWidthSelector value

-- | @- maxVisibleColumns@
maxVisibleColumns :: IsNSBrowser nsBrowser => nsBrowser -> IO CLong
maxVisibleColumns nsBrowser =
  sendMessage nsBrowser maxVisibleColumnsSelector

-- | @- setMaxVisibleColumns:@
setMaxVisibleColumns :: IsNSBrowser nsBrowser => nsBrowser -> CLong -> IO ()
setMaxVisibleColumns nsBrowser value =
  sendMessage nsBrowser setMaxVisibleColumnsSelector value

-- | @- allowsMultipleSelection@
allowsMultipleSelection :: IsNSBrowser nsBrowser => nsBrowser -> IO Bool
allowsMultipleSelection nsBrowser =
  sendMessage nsBrowser allowsMultipleSelectionSelector

-- | @- setAllowsMultipleSelection:@
setAllowsMultipleSelection :: IsNSBrowser nsBrowser => nsBrowser -> Bool -> IO ()
setAllowsMultipleSelection nsBrowser value =
  sendMessage nsBrowser setAllowsMultipleSelectionSelector value

-- | @- allowsBranchSelection@
allowsBranchSelection :: IsNSBrowser nsBrowser => nsBrowser -> IO Bool
allowsBranchSelection nsBrowser =
  sendMessage nsBrowser allowsBranchSelectionSelector

-- | @- setAllowsBranchSelection:@
setAllowsBranchSelection :: IsNSBrowser nsBrowser => nsBrowser -> Bool -> IO ()
setAllowsBranchSelection nsBrowser value =
  sendMessage nsBrowser setAllowsBranchSelectionSelector value

-- | @- allowsEmptySelection@
allowsEmptySelection :: IsNSBrowser nsBrowser => nsBrowser -> IO Bool
allowsEmptySelection nsBrowser =
  sendMessage nsBrowser allowsEmptySelectionSelector

-- | @- setAllowsEmptySelection:@
setAllowsEmptySelection :: IsNSBrowser nsBrowser => nsBrowser -> Bool -> IO ()
setAllowsEmptySelection nsBrowser value =
  sendMessage nsBrowser setAllowsEmptySelectionSelector value

-- | @- takesTitleFromPreviousColumn@
takesTitleFromPreviousColumn :: IsNSBrowser nsBrowser => nsBrowser -> IO Bool
takesTitleFromPreviousColumn nsBrowser =
  sendMessage nsBrowser takesTitleFromPreviousColumnSelector

-- | @- setTakesTitleFromPreviousColumn:@
setTakesTitleFromPreviousColumn :: IsNSBrowser nsBrowser => nsBrowser -> Bool -> IO ()
setTakesTitleFromPreviousColumn nsBrowser value =
  sendMessage nsBrowser setTakesTitleFromPreviousColumnSelector value

-- | @- sendsActionOnArrowKeys@
sendsActionOnArrowKeys :: IsNSBrowser nsBrowser => nsBrowser -> IO Bool
sendsActionOnArrowKeys nsBrowser =
  sendMessage nsBrowser sendsActionOnArrowKeysSelector

-- | @- setSendsActionOnArrowKeys:@
setSendsActionOnArrowKeys :: IsNSBrowser nsBrowser => nsBrowser -> Bool -> IO ()
setSendsActionOnArrowKeys nsBrowser value =
  sendMessage nsBrowser setSendsActionOnArrowKeysSelector value

-- | @- pathSeparator@
pathSeparator :: IsNSBrowser nsBrowser => nsBrowser -> IO (Id NSString)
pathSeparator nsBrowser =
  sendMessage nsBrowser pathSeparatorSelector

-- | @- setPathSeparator:@
setPathSeparator :: (IsNSBrowser nsBrowser, IsNSString value) => nsBrowser -> value -> IO ()
setPathSeparator nsBrowser value =
  sendMessage nsBrowser setPathSeparatorSelector (toNSString value)

-- | @- clickedColumn@
clickedColumn :: IsNSBrowser nsBrowser => nsBrowser -> IO CLong
clickedColumn nsBrowser =
  sendMessage nsBrowser clickedColumnSelector

-- | @- clickedRow@
clickedRow :: IsNSBrowser nsBrowser => nsBrowser -> IO CLong
clickedRow nsBrowser =
  sendMessage nsBrowser clickedRowSelector

-- | @- selectedColumn@
selectedColumn :: IsNSBrowser nsBrowser => nsBrowser -> IO CLong
selectedColumn nsBrowser =
  sendMessage nsBrowser selectedColumnSelector

-- | @- selectedCell@
selectedCell :: IsNSBrowser nsBrowser => nsBrowser -> IO RawId
selectedCell nsBrowser =
  sendMessage nsBrowser selectedCellSelector

-- | @- selectedCells@
selectedCells :: IsNSBrowser nsBrowser => nsBrowser -> IO (Id NSArray)
selectedCells nsBrowser =
  sendMessage nsBrowser selectedCellsSelector

-- | @- selectionIndexPath@
selectionIndexPath :: IsNSBrowser nsBrowser => nsBrowser -> IO (Id NSIndexPath)
selectionIndexPath nsBrowser =
  sendMessage nsBrowser selectionIndexPathSelector

-- | @- setSelectionIndexPath:@
setSelectionIndexPath :: (IsNSBrowser nsBrowser, IsNSIndexPath value) => nsBrowser -> value -> IO ()
setSelectionIndexPath nsBrowser value =
  sendMessage nsBrowser setSelectionIndexPathSelector (toNSIndexPath value)

-- | @- selectionIndexPaths@
selectionIndexPaths :: IsNSBrowser nsBrowser => nsBrowser -> IO (Id NSArray)
selectionIndexPaths nsBrowser =
  sendMessage nsBrowser selectionIndexPathsSelector

-- | @- setSelectionIndexPaths:@
setSelectionIndexPaths :: (IsNSBrowser nsBrowser, IsNSArray value) => nsBrowser -> value -> IO ()
setSelectionIndexPaths nsBrowser value =
  sendMessage nsBrowser setSelectionIndexPathsSelector (toNSArray value)

-- | @- lastColumn@
lastColumn :: IsNSBrowser nsBrowser => nsBrowser -> IO CLong
lastColumn nsBrowser =
  sendMessage nsBrowser lastColumnSelector

-- | @- setLastColumn:@
setLastColumn :: IsNSBrowser nsBrowser => nsBrowser -> CLong -> IO ()
setLastColumn nsBrowser value =
  sendMessage nsBrowser setLastColumnSelector value

-- | @- numberOfVisibleColumns@
numberOfVisibleColumns :: IsNSBrowser nsBrowser => nsBrowser -> IO CLong
numberOfVisibleColumns nsBrowser =
  sendMessage nsBrowser numberOfVisibleColumnsSelector

-- | @- firstVisibleColumn@
firstVisibleColumn :: IsNSBrowser nsBrowser => nsBrowser -> IO CLong
firstVisibleColumn nsBrowser =
  sendMessage nsBrowser firstVisibleColumnSelector

-- | @- lastVisibleColumn@
lastVisibleColumn :: IsNSBrowser nsBrowser => nsBrowser -> IO CLong
lastVisibleColumn nsBrowser =
  sendMessage nsBrowser lastVisibleColumnSelector

-- | @- titleHeight@
titleHeight :: IsNSBrowser nsBrowser => nsBrowser -> IO CDouble
titleHeight nsBrowser =
  sendMessage nsBrowser titleHeightSelector

-- | @- columnResizingType@
columnResizingType :: IsNSBrowser nsBrowser => nsBrowser -> IO NSBrowserColumnResizingType
columnResizingType nsBrowser =
  sendMessage nsBrowser columnResizingTypeSelector

-- | @- setColumnResizingType:@
setColumnResizingType :: IsNSBrowser nsBrowser => nsBrowser -> NSBrowserColumnResizingType -> IO ()
setColumnResizingType nsBrowser value =
  sendMessage nsBrowser setColumnResizingTypeSelector value

-- | @- prefersAllColumnUserResizing@
prefersAllColumnUserResizing :: IsNSBrowser nsBrowser => nsBrowser -> IO Bool
prefersAllColumnUserResizing nsBrowser =
  sendMessage nsBrowser prefersAllColumnUserResizingSelector

-- | @- setPrefersAllColumnUserResizing:@
setPrefersAllColumnUserResizing :: IsNSBrowser nsBrowser => nsBrowser -> Bool -> IO ()
setPrefersAllColumnUserResizing nsBrowser value =
  sendMessage nsBrowser setPrefersAllColumnUserResizingSelector value

-- | @- rowHeight@
rowHeight :: IsNSBrowser nsBrowser => nsBrowser -> IO CDouble
rowHeight nsBrowser =
  sendMessage nsBrowser rowHeightSelector

-- | @- setRowHeight:@
setRowHeight :: IsNSBrowser nsBrowser => nsBrowser -> CDouble -> IO ()
setRowHeight nsBrowser value =
  sendMessage nsBrowser setRowHeightSelector value

-- | @- columnsAutosaveName@
columnsAutosaveName :: IsNSBrowser nsBrowser => nsBrowser -> IO (Id NSString)
columnsAutosaveName nsBrowser =
  sendMessage nsBrowser columnsAutosaveNameSelector

-- | @- setColumnsAutosaveName:@
setColumnsAutosaveName :: (IsNSBrowser nsBrowser, IsNSString value) => nsBrowser -> value -> IO ()
setColumnsAutosaveName nsBrowser value =
  sendMessage nsBrowser setColumnsAutosaveNameSelector (toNSString value)

-- | @- allowsTypeSelect@
allowsTypeSelect :: IsNSBrowser nsBrowser => nsBrowser -> IO Bool
allowsTypeSelect nsBrowser =
  sendMessage nsBrowser allowsTypeSelectSelector

-- | @- setAllowsTypeSelect:@
setAllowsTypeSelect :: IsNSBrowser nsBrowser => nsBrowser -> Bool -> IO ()
setAllowsTypeSelect nsBrowser value =
  sendMessage nsBrowser setAllowsTypeSelectSelector value

-- | @- backgroundColor@
backgroundColor :: IsNSBrowser nsBrowser => nsBrowser -> IO (Id NSColor)
backgroundColor nsBrowser =
  sendMessage nsBrowser backgroundColorSelector

-- | @- setBackgroundColor:@
setBackgroundColor :: (IsNSBrowser nsBrowser, IsNSColor value) => nsBrowser -> value -> IO ()
setBackgroundColor nsBrowser value =
  sendMessage nsBrowser setBackgroundColorSelector (toNSColor value)

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @loadColumnZero@
loadColumnZeroSelector :: Selector '[] ()
loadColumnZeroSelector = mkSelector "loadColumnZero"

-- | @Selector@ for @setCellClass:@
setCellClassSelector :: Selector '[Class] ()
setCellClassSelector = mkSelector "setCellClass:"

-- | @Selector@ for @itemAtIndexPath:@
itemAtIndexPathSelector :: Selector '[Id NSIndexPath] RawId
itemAtIndexPathSelector = mkSelector "itemAtIndexPath:"

-- | @Selector@ for @itemAtRow:inColumn:@
itemAtRow_inColumnSelector :: Selector '[CLong, CLong] RawId
itemAtRow_inColumnSelector = mkSelector "itemAtRow:inColumn:"

-- | @Selector@ for @indexPathForColumn:@
indexPathForColumnSelector :: Selector '[CLong] (Id NSIndexPath)
indexPathForColumnSelector = mkSelector "indexPathForColumn:"

-- | @Selector@ for @isLeafItem:@
isLeafItemSelector :: Selector '[RawId] Bool
isLeafItemSelector = mkSelector "isLeafItem:"

-- | @Selector@ for @reloadDataForRowIndexes:inColumn:@
reloadDataForRowIndexes_inColumnSelector :: Selector '[Id NSIndexSet, CLong] ()
reloadDataForRowIndexes_inColumnSelector = mkSelector "reloadDataForRowIndexes:inColumn:"

-- | @Selector@ for @parentForItemsInColumn:@
parentForItemsInColumnSelector :: Selector '[CLong] RawId
parentForItemsInColumnSelector = mkSelector "parentForItemsInColumn:"

-- | @Selector@ for @scrollRowToVisible:inColumn:@
scrollRowToVisible_inColumnSelector :: Selector '[CLong, CLong] ()
scrollRowToVisible_inColumnSelector = mkSelector "scrollRowToVisible:inColumn:"

-- | @Selector@ for @setTitle:ofColumn:@
setTitle_ofColumnSelector :: Selector '[Id NSString, CLong] ()
setTitle_ofColumnSelector = mkSelector "setTitle:ofColumn:"

-- | @Selector@ for @titleOfColumn:@
titleOfColumnSelector :: Selector '[CLong] (Id NSString)
titleOfColumnSelector = mkSelector "titleOfColumn:"

-- | @Selector@ for @setPath:@
setPathSelector :: Selector '[Id NSString] Bool
setPathSelector = mkSelector "setPath:"

-- | @Selector@ for @path@
pathSelector :: Selector '[] (Id NSString)
pathSelector = mkSelector "path"

-- | @Selector@ for @pathToColumn:@
pathToColumnSelector :: Selector '[CLong] (Id NSString)
pathToColumnSelector = mkSelector "pathToColumn:"

-- | @Selector@ for @selectedCellInColumn:@
selectedCellInColumnSelector :: Selector '[CLong] RawId
selectedCellInColumnSelector = mkSelector "selectedCellInColumn:"

-- | @Selector@ for @selectRow:inColumn:@
selectRow_inColumnSelector :: Selector '[CLong, CLong] ()
selectRow_inColumnSelector = mkSelector "selectRow:inColumn:"

-- | @Selector@ for @selectedRowInColumn:@
selectedRowInColumnSelector :: Selector '[CLong] CLong
selectedRowInColumnSelector = mkSelector "selectedRowInColumn:"

-- | @Selector@ for @selectRowIndexes:inColumn:@
selectRowIndexes_inColumnSelector :: Selector '[Id NSIndexSet, CLong] ()
selectRowIndexes_inColumnSelector = mkSelector "selectRowIndexes:inColumn:"

-- | @Selector@ for @selectedRowIndexesInColumn:@
selectedRowIndexesInColumnSelector :: Selector '[CLong] (Id NSIndexSet)
selectedRowIndexesInColumnSelector = mkSelector "selectedRowIndexesInColumn:"

-- | @Selector@ for @reloadColumn:@
reloadColumnSelector :: Selector '[CLong] ()
reloadColumnSelector = mkSelector "reloadColumn:"

-- | @Selector@ for @validateVisibleColumns@
validateVisibleColumnsSelector :: Selector '[] ()
validateVisibleColumnsSelector = mkSelector "validateVisibleColumns"

-- | @Selector@ for @scrollColumnsRightBy:@
scrollColumnsRightBySelector :: Selector '[CLong] ()
scrollColumnsRightBySelector = mkSelector "scrollColumnsRightBy:"

-- | @Selector@ for @scrollColumnsLeftBy:@
scrollColumnsLeftBySelector :: Selector '[CLong] ()
scrollColumnsLeftBySelector = mkSelector "scrollColumnsLeftBy:"

-- | @Selector@ for @scrollColumnToVisible:@
scrollColumnToVisibleSelector :: Selector '[CLong] ()
scrollColumnToVisibleSelector = mkSelector "scrollColumnToVisible:"

-- | @Selector@ for @addColumn@
addColumnSelector :: Selector '[] ()
addColumnSelector = mkSelector "addColumn"

-- | @Selector@ for @loadedCellAtRow:column:@
loadedCellAtRow_columnSelector :: Selector '[CLong, CLong] RawId
loadedCellAtRow_columnSelector = mkSelector "loadedCellAtRow:column:"

-- | @Selector@ for @selectAll:@
selectAllSelector :: Selector '[RawId] ()
selectAllSelector = mkSelector "selectAll:"

-- | @Selector@ for @tile@
tileSelector :: Selector '[] ()
tileSelector = mkSelector "tile"

-- | @Selector@ for @doClick:@
doClickSelector :: Selector '[RawId] ()
doClickSelector = mkSelector "doClick:"

-- | @Selector@ for @doDoubleClick:@
doDoubleClickSelector :: Selector '[RawId] ()
doDoubleClickSelector = mkSelector "doDoubleClick:"

-- | @Selector@ for @sendAction@
sendActionSelector :: Selector '[] Bool
sendActionSelector = mkSelector "sendAction"

-- | @Selector@ for @titleFrameOfColumn:@
titleFrameOfColumnSelector :: Selector '[CLong] NSRect
titleFrameOfColumnSelector = mkSelector "titleFrameOfColumn:"

-- | @Selector@ for @drawTitleOfColumn:inRect:@
drawTitleOfColumn_inRectSelector :: Selector '[CLong, NSRect] ()
drawTitleOfColumn_inRectSelector = mkSelector "drawTitleOfColumn:inRect:"

-- | @Selector@ for @frameOfColumn:@
frameOfColumnSelector :: Selector '[CLong] NSRect
frameOfColumnSelector = mkSelector "frameOfColumn:"

-- | @Selector@ for @frameOfInsideOfColumn:@
frameOfInsideOfColumnSelector :: Selector '[CLong] NSRect
frameOfInsideOfColumnSelector = mkSelector "frameOfInsideOfColumn:"

-- | @Selector@ for @frameOfRow:inColumn:@
frameOfRow_inColumnSelector :: Selector '[CLong, CLong] NSRect
frameOfRow_inColumnSelector = mkSelector "frameOfRow:inColumn:"

-- | @Selector@ for @getRow:column:forPoint:@
getRow_column_forPointSelector :: Selector '[Ptr CLong, Ptr CLong, NSPoint] Bool
getRow_column_forPointSelector = mkSelector "getRow:column:forPoint:"

-- | @Selector@ for @columnWidthForColumnContentWidth:@
columnWidthForColumnContentWidthSelector :: Selector '[CDouble] CDouble
columnWidthForColumnContentWidthSelector = mkSelector "columnWidthForColumnContentWidth:"

-- | @Selector@ for @columnContentWidthForColumnWidth:@
columnContentWidthForColumnWidthSelector :: Selector '[CDouble] CDouble
columnContentWidthForColumnWidthSelector = mkSelector "columnContentWidthForColumnWidth:"

-- | @Selector@ for @setWidth:ofColumn:@
setWidth_ofColumnSelector :: Selector '[CDouble, CLong] ()
setWidth_ofColumnSelector = mkSelector "setWidth:ofColumn:"

-- | @Selector@ for @widthOfColumn:@
widthOfColumnSelector :: Selector '[CLong] CDouble
widthOfColumnSelector = mkSelector "widthOfColumn:"

-- | @Selector@ for @noteHeightOfRowsWithIndexesChanged:inColumn:@
noteHeightOfRowsWithIndexesChanged_inColumnSelector :: Selector '[Id NSIndexSet, CLong] ()
noteHeightOfRowsWithIndexesChanged_inColumnSelector = mkSelector "noteHeightOfRowsWithIndexesChanged:inColumn:"

-- | @Selector@ for @setDefaultColumnWidth:@
setDefaultColumnWidthSelector :: Selector '[CDouble] ()
setDefaultColumnWidthSelector = mkSelector "setDefaultColumnWidth:"

-- | @Selector@ for @defaultColumnWidth@
defaultColumnWidthSelector :: Selector '[] CDouble
defaultColumnWidthSelector = mkSelector "defaultColumnWidth"

-- | @Selector@ for @removeSavedColumnsWithAutosaveName:@
removeSavedColumnsWithAutosaveNameSelector :: Selector '[Id NSString] ()
removeSavedColumnsWithAutosaveNameSelector = mkSelector "removeSavedColumnsWithAutosaveName:"

-- | @Selector@ for @canDragRowsWithIndexes:inColumn:withEvent:@
canDragRowsWithIndexes_inColumn_withEventSelector :: Selector '[Id NSIndexSet, CLong, Id NSEvent] Bool
canDragRowsWithIndexes_inColumn_withEventSelector = mkSelector "canDragRowsWithIndexes:inColumn:withEvent:"

-- | @Selector@ for @draggingImageForRowsWithIndexes:inColumn:withEvent:offset:@
draggingImageForRowsWithIndexes_inColumn_withEvent_offsetSelector :: Selector '[Id NSIndexSet, CLong, Id NSEvent, Ptr NSPoint] (Id NSImage)
draggingImageForRowsWithIndexes_inColumn_withEvent_offsetSelector = mkSelector "draggingImageForRowsWithIndexes:inColumn:withEvent:offset:"

-- | @Selector@ for @setDraggingSourceOperationMask:forLocal:@
setDraggingSourceOperationMask_forLocalSelector :: Selector '[NSDragOperation, Bool] ()
setDraggingSourceOperationMask_forLocalSelector = mkSelector "setDraggingSourceOperationMask:forLocal:"

-- | @Selector@ for @editItemAtIndexPath:withEvent:select:@
editItemAtIndexPath_withEvent_selectSelector :: Selector '[Id NSIndexPath, Id NSEvent, Bool] ()
editItemAtIndexPath_withEvent_selectSelector = mkSelector "editItemAtIndexPath:withEvent:select:"

-- | @Selector@ for @setAcceptsArrowKeys:@
setAcceptsArrowKeysSelector :: Selector '[Bool] ()
setAcceptsArrowKeysSelector = mkSelector "setAcceptsArrowKeys:"

-- | @Selector@ for @acceptsArrowKeys@
acceptsArrowKeysSelector :: Selector '[] Bool
acceptsArrowKeysSelector = mkSelector "acceptsArrowKeys"

-- | @Selector@ for @displayColumn:@
displayColumnSelector :: Selector '[CLong] ()
displayColumnSelector = mkSelector "displayColumn:"

-- | @Selector@ for @displayAllColumns@
displayAllColumnsSelector :: Selector '[] ()
displayAllColumnsSelector = mkSelector "displayAllColumns"

-- | @Selector@ for @scrollViaScroller:@
scrollViaScrollerSelector :: Selector '[Id NSScroller] ()
scrollViaScrollerSelector = mkSelector "scrollViaScroller:"

-- | @Selector@ for @updateScroller@
updateScrollerSelector :: Selector '[] ()
updateScrollerSelector = mkSelector "updateScroller"

-- | @Selector@ for @setMatrixClass:@
setMatrixClassSelector :: Selector '[Class] ()
setMatrixClassSelector = mkSelector "setMatrixClass:"

-- | @Selector@ for @matrixClass@
matrixClassSelector :: Selector '[] Class
matrixClassSelector = mkSelector "matrixClass"

-- | @Selector@ for @columnOfMatrix:@
columnOfMatrixSelector :: Selector '[Id NSMatrix] CLong
columnOfMatrixSelector = mkSelector "columnOfMatrix:"

-- | @Selector@ for @matrixInColumn:@
matrixInColumnSelector :: Selector '[CLong] (Id NSMatrix)
matrixInColumnSelector = mkSelector "matrixInColumn:"

-- | @Selector@ for @cellClass@
cellClassSelector :: Selector '[] Class
cellClassSelector = mkSelector "cellClass"

-- | @Selector@ for @loaded@
loadedSelector :: Selector '[] Bool
loadedSelector = mkSelector "loaded"

-- | @Selector@ for @doubleAction@
doubleActionSelector :: Selector '[] Sel
doubleActionSelector = mkSelector "doubleAction"

-- | @Selector@ for @setDoubleAction:@
setDoubleActionSelector :: Selector '[Sel] ()
setDoubleActionSelector = mkSelector "setDoubleAction:"

-- | @Selector@ for @cellPrototype@
cellPrototypeSelector :: Selector '[] RawId
cellPrototypeSelector = mkSelector "cellPrototype"

-- | @Selector@ for @setCellPrototype:@
setCellPrototypeSelector :: Selector '[RawId] ()
setCellPrototypeSelector = mkSelector "setCellPrototype:"

-- | @Selector@ for @delegate@
delegateSelector :: Selector '[] RawId
delegateSelector = mkSelector "delegate"

-- | @Selector@ for @setDelegate:@
setDelegateSelector :: Selector '[RawId] ()
setDelegateSelector = mkSelector "setDelegate:"

-- | @Selector@ for @reusesColumns@
reusesColumnsSelector :: Selector '[] Bool
reusesColumnsSelector = mkSelector "reusesColumns"

-- | @Selector@ for @setReusesColumns:@
setReusesColumnsSelector :: Selector '[Bool] ()
setReusesColumnsSelector = mkSelector "setReusesColumns:"

-- | @Selector@ for @hasHorizontalScroller@
hasHorizontalScrollerSelector :: Selector '[] Bool
hasHorizontalScrollerSelector = mkSelector "hasHorizontalScroller"

-- | @Selector@ for @setHasHorizontalScroller:@
setHasHorizontalScrollerSelector :: Selector '[Bool] ()
setHasHorizontalScrollerSelector = mkSelector "setHasHorizontalScroller:"

-- | @Selector@ for @autohidesScroller@
autohidesScrollerSelector :: Selector '[] Bool
autohidesScrollerSelector = mkSelector "autohidesScroller"

-- | @Selector@ for @setAutohidesScroller:@
setAutohidesScrollerSelector :: Selector '[Bool] ()
setAutohidesScrollerSelector = mkSelector "setAutohidesScroller:"

-- | @Selector@ for @separatesColumns@
separatesColumnsSelector :: Selector '[] Bool
separatesColumnsSelector = mkSelector "separatesColumns"

-- | @Selector@ for @setSeparatesColumns:@
setSeparatesColumnsSelector :: Selector '[Bool] ()
setSeparatesColumnsSelector = mkSelector "setSeparatesColumns:"

-- | @Selector@ for @titled@
titledSelector :: Selector '[] Bool
titledSelector = mkSelector "titled"

-- | @Selector@ for @setTitled:@
setTitledSelector :: Selector '[Bool] ()
setTitledSelector = mkSelector "setTitled:"

-- | @Selector@ for @minColumnWidth@
minColumnWidthSelector :: Selector '[] CDouble
minColumnWidthSelector = mkSelector "minColumnWidth"

-- | @Selector@ for @setMinColumnWidth:@
setMinColumnWidthSelector :: Selector '[CDouble] ()
setMinColumnWidthSelector = mkSelector "setMinColumnWidth:"

-- | @Selector@ for @maxVisibleColumns@
maxVisibleColumnsSelector :: Selector '[] CLong
maxVisibleColumnsSelector = mkSelector "maxVisibleColumns"

-- | @Selector@ for @setMaxVisibleColumns:@
setMaxVisibleColumnsSelector :: Selector '[CLong] ()
setMaxVisibleColumnsSelector = mkSelector "setMaxVisibleColumns:"

-- | @Selector@ for @allowsMultipleSelection@
allowsMultipleSelectionSelector :: Selector '[] Bool
allowsMultipleSelectionSelector = mkSelector "allowsMultipleSelection"

-- | @Selector@ for @setAllowsMultipleSelection:@
setAllowsMultipleSelectionSelector :: Selector '[Bool] ()
setAllowsMultipleSelectionSelector = mkSelector "setAllowsMultipleSelection:"

-- | @Selector@ for @allowsBranchSelection@
allowsBranchSelectionSelector :: Selector '[] Bool
allowsBranchSelectionSelector = mkSelector "allowsBranchSelection"

-- | @Selector@ for @setAllowsBranchSelection:@
setAllowsBranchSelectionSelector :: Selector '[Bool] ()
setAllowsBranchSelectionSelector = mkSelector "setAllowsBranchSelection:"

-- | @Selector@ for @allowsEmptySelection@
allowsEmptySelectionSelector :: Selector '[] Bool
allowsEmptySelectionSelector = mkSelector "allowsEmptySelection"

-- | @Selector@ for @setAllowsEmptySelection:@
setAllowsEmptySelectionSelector :: Selector '[Bool] ()
setAllowsEmptySelectionSelector = mkSelector "setAllowsEmptySelection:"

-- | @Selector@ for @takesTitleFromPreviousColumn@
takesTitleFromPreviousColumnSelector :: Selector '[] Bool
takesTitleFromPreviousColumnSelector = mkSelector "takesTitleFromPreviousColumn"

-- | @Selector@ for @setTakesTitleFromPreviousColumn:@
setTakesTitleFromPreviousColumnSelector :: Selector '[Bool] ()
setTakesTitleFromPreviousColumnSelector = mkSelector "setTakesTitleFromPreviousColumn:"

-- | @Selector@ for @sendsActionOnArrowKeys@
sendsActionOnArrowKeysSelector :: Selector '[] Bool
sendsActionOnArrowKeysSelector = mkSelector "sendsActionOnArrowKeys"

-- | @Selector@ for @setSendsActionOnArrowKeys:@
setSendsActionOnArrowKeysSelector :: Selector '[Bool] ()
setSendsActionOnArrowKeysSelector = mkSelector "setSendsActionOnArrowKeys:"

-- | @Selector@ for @pathSeparator@
pathSeparatorSelector :: Selector '[] (Id NSString)
pathSeparatorSelector = mkSelector "pathSeparator"

-- | @Selector@ for @setPathSeparator:@
setPathSeparatorSelector :: Selector '[Id NSString] ()
setPathSeparatorSelector = mkSelector "setPathSeparator:"

-- | @Selector@ for @clickedColumn@
clickedColumnSelector :: Selector '[] CLong
clickedColumnSelector = mkSelector "clickedColumn"

-- | @Selector@ for @clickedRow@
clickedRowSelector :: Selector '[] CLong
clickedRowSelector = mkSelector "clickedRow"

-- | @Selector@ for @selectedColumn@
selectedColumnSelector :: Selector '[] CLong
selectedColumnSelector = mkSelector "selectedColumn"

-- | @Selector@ for @selectedCell@
selectedCellSelector :: Selector '[] RawId
selectedCellSelector = mkSelector "selectedCell"

-- | @Selector@ for @selectedCells@
selectedCellsSelector :: Selector '[] (Id NSArray)
selectedCellsSelector = mkSelector "selectedCells"

-- | @Selector@ for @selectionIndexPath@
selectionIndexPathSelector :: Selector '[] (Id NSIndexPath)
selectionIndexPathSelector = mkSelector "selectionIndexPath"

-- | @Selector@ for @setSelectionIndexPath:@
setSelectionIndexPathSelector :: Selector '[Id NSIndexPath] ()
setSelectionIndexPathSelector = mkSelector "setSelectionIndexPath:"

-- | @Selector@ for @selectionIndexPaths@
selectionIndexPathsSelector :: Selector '[] (Id NSArray)
selectionIndexPathsSelector = mkSelector "selectionIndexPaths"

-- | @Selector@ for @setSelectionIndexPaths:@
setSelectionIndexPathsSelector :: Selector '[Id NSArray] ()
setSelectionIndexPathsSelector = mkSelector "setSelectionIndexPaths:"

-- | @Selector@ for @lastColumn@
lastColumnSelector :: Selector '[] CLong
lastColumnSelector = mkSelector "lastColumn"

-- | @Selector@ for @setLastColumn:@
setLastColumnSelector :: Selector '[CLong] ()
setLastColumnSelector = mkSelector "setLastColumn:"

-- | @Selector@ for @numberOfVisibleColumns@
numberOfVisibleColumnsSelector :: Selector '[] CLong
numberOfVisibleColumnsSelector = mkSelector "numberOfVisibleColumns"

-- | @Selector@ for @firstVisibleColumn@
firstVisibleColumnSelector :: Selector '[] CLong
firstVisibleColumnSelector = mkSelector "firstVisibleColumn"

-- | @Selector@ for @lastVisibleColumn@
lastVisibleColumnSelector :: Selector '[] CLong
lastVisibleColumnSelector = mkSelector "lastVisibleColumn"

-- | @Selector@ for @titleHeight@
titleHeightSelector :: Selector '[] CDouble
titleHeightSelector = mkSelector "titleHeight"

-- | @Selector@ for @columnResizingType@
columnResizingTypeSelector :: Selector '[] NSBrowserColumnResizingType
columnResizingTypeSelector = mkSelector "columnResizingType"

-- | @Selector@ for @setColumnResizingType:@
setColumnResizingTypeSelector :: Selector '[NSBrowserColumnResizingType] ()
setColumnResizingTypeSelector = mkSelector "setColumnResizingType:"

-- | @Selector@ for @prefersAllColumnUserResizing@
prefersAllColumnUserResizingSelector :: Selector '[] Bool
prefersAllColumnUserResizingSelector = mkSelector "prefersAllColumnUserResizing"

-- | @Selector@ for @setPrefersAllColumnUserResizing:@
setPrefersAllColumnUserResizingSelector :: Selector '[Bool] ()
setPrefersAllColumnUserResizingSelector = mkSelector "setPrefersAllColumnUserResizing:"

-- | @Selector@ for @rowHeight@
rowHeightSelector :: Selector '[] CDouble
rowHeightSelector = mkSelector "rowHeight"

-- | @Selector@ for @setRowHeight:@
setRowHeightSelector :: Selector '[CDouble] ()
setRowHeightSelector = mkSelector "setRowHeight:"

-- | @Selector@ for @columnsAutosaveName@
columnsAutosaveNameSelector :: Selector '[] (Id NSString)
columnsAutosaveNameSelector = mkSelector "columnsAutosaveName"

-- | @Selector@ for @setColumnsAutosaveName:@
setColumnsAutosaveNameSelector :: Selector '[Id NSString] ()
setColumnsAutosaveNameSelector = mkSelector "setColumnsAutosaveName:"

-- | @Selector@ for @allowsTypeSelect@
allowsTypeSelectSelector :: Selector '[] Bool
allowsTypeSelectSelector = mkSelector "allowsTypeSelect"

-- | @Selector@ for @setAllowsTypeSelect:@
setAllowsTypeSelectSelector :: Selector '[Bool] ()
setAllowsTypeSelectSelector = mkSelector "setAllowsTypeSelect:"

-- | @Selector@ for @backgroundColor@
backgroundColorSelector :: Selector '[] (Id NSColor)
backgroundColorSelector = mkSelector "backgroundColor"

-- | @Selector@ for @setBackgroundColor:@
setBackgroundColorSelector :: Selector '[Id NSColor] ()
setBackgroundColorSelector = mkSelector "setBackgroundColor:"

