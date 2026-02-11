{-# LANGUAGE PatternSynonyms #-}
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
  , loadColumnZeroSelector
  , setCellClassSelector
  , itemAtIndexPathSelector
  , itemAtRow_inColumnSelector
  , indexPathForColumnSelector
  , isLeafItemSelector
  , reloadDataForRowIndexes_inColumnSelector
  , parentForItemsInColumnSelector
  , scrollRowToVisible_inColumnSelector
  , setTitle_ofColumnSelector
  , titleOfColumnSelector
  , setPathSelector
  , pathSelector
  , pathToColumnSelector
  , selectedCellInColumnSelector
  , selectRow_inColumnSelector
  , selectedRowInColumnSelector
  , selectRowIndexes_inColumnSelector
  , selectedRowIndexesInColumnSelector
  , reloadColumnSelector
  , validateVisibleColumnsSelector
  , scrollColumnsRightBySelector
  , scrollColumnsLeftBySelector
  , scrollColumnToVisibleSelector
  , addColumnSelector
  , loadedCellAtRow_columnSelector
  , selectAllSelector
  , tileSelector
  , doClickSelector
  , doDoubleClickSelector
  , sendActionSelector
  , titleFrameOfColumnSelector
  , drawTitleOfColumn_inRectSelector
  , frameOfColumnSelector
  , frameOfInsideOfColumnSelector
  , frameOfRow_inColumnSelector
  , getRow_column_forPointSelector
  , columnWidthForColumnContentWidthSelector
  , columnContentWidthForColumnWidthSelector
  , setWidth_ofColumnSelector
  , widthOfColumnSelector
  , noteHeightOfRowsWithIndexesChanged_inColumnSelector
  , setDefaultColumnWidthSelector
  , defaultColumnWidthSelector
  , removeSavedColumnsWithAutosaveNameSelector
  , canDragRowsWithIndexes_inColumn_withEventSelector
  , draggingImageForRowsWithIndexes_inColumn_withEvent_offsetSelector
  , setDraggingSourceOperationMask_forLocalSelector
  , editItemAtIndexPath_withEvent_selectSelector
  , setAcceptsArrowKeysSelector
  , acceptsArrowKeysSelector
  , displayColumnSelector
  , displayAllColumnsSelector
  , scrollViaScrollerSelector
  , updateScrollerSelector
  , setMatrixClassSelector
  , matrixClassSelector
  , columnOfMatrixSelector
  , matrixInColumnSelector
  , cellClassSelector
  , loadedSelector
  , doubleActionSelector
  , setDoubleActionSelector
  , cellPrototypeSelector
  , setCellPrototypeSelector
  , delegateSelector
  , setDelegateSelector
  , reusesColumnsSelector
  , setReusesColumnsSelector
  , hasHorizontalScrollerSelector
  , setHasHorizontalScrollerSelector
  , autohidesScrollerSelector
  , setAutohidesScrollerSelector
  , separatesColumnsSelector
  , setSeparatesColumnsSelector
  , titledSelector
  , setTitledSelector
  , minColumnWidthSelector
  , setMinColumnWidthSelector
  , maxVisibleColumnsSelector
  , setMaxVisibleColumnsSelector
  , allowsMultipleSelectionSelector
  , setAllowsMultipleSelectionSelector
  , allowsBranchSelectionSelector
  , setAllowsBranchSelectionSelector
  , allowsEmptySelectionSelector
  , setAllowsEmptySelectionSelector
  , takesTitleFromPreviousColumnSelector
  , setTakesTitleFromPreviousColumnSelector
  , sendsActionOnArrowKeysSelector
  , setSendsActionOnArrowKeysSelector
  , pathSeparatorSelector
  , setPathSeparatorSelector
  , clickedColumnSelector
  , clickedRowSelector
  , selectedColumnSelector
  , selectedCellSelector
  , selectedCellsSelector
  , selectionIndexPathSelector
  , setSelectionIndexPathSelector
  , selectionIndexPathsSelector
  , setSelectionIndexPathsSelector
  , lastColumnSelector
  , setLastColumnSelector
  , numberOfVisibleColumnsSelector
  , firstVisibleColumnSelector
  , lastVisibleColumnSelector
  , titleHeightSelector
  , columnResizingTypeSelector
  , setColumnResizingTypeSelector
  , prefersAllColumnUserResizingSelector
  , setPrefersAllColumnUserResizingSelector
  , rowHeightSelector
  , setRowHeightSelector
  , columnsAutosaveNameSelector
  , setColumnsAutosaveNameSelector
  , allowsTypeSelectSelector
  , setAllowsTypeSelectSelector
  , backgroundColorSelector
  , setBackgroundColorSelector

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

-- | @- loadColumnZero@
loadColumnZero :: IsNSBrowser nsBrowser => nsBrowser -> IO ()
loadColumnZero nsBrowser  =
    sendMsg nsBrowser (mkSelector "loadColumnZero") retVoid []

-- | @- setCellClass:@
setCellClass :: IsNSBrowser nsBrowser => nsBrowser -> Class -> IO ()
setCellClass nsBrowser  factoryId =
    sendMsg nsBrowser (mkSelector "setCellClass:") retVoid [argPtr (unClass factoryId)]

-- | @- itemAtIndexPath:@
itemAtIndexPath :: (IsNSBrowser nsBrowser, IsNSIndexPath indexPath) => nsBrowser -> indexPath -> IO RawId
itemAtIndexPath nsBrowser  indexPath =
  withObjCPtr indexPath $ \raw_indexPath ->
      fmap (RawId . castPtr) $ sendMsg nsBrowser (mkSelector "itemAtIndexPath:") (retPtr retVoid) [argPtr (castPtr raw_indexPath :: Ptr ())]

-- | @- itemAtRow:inColumn:@
itemAtRow_inColumn :: IsNSBrowser nsBrowser => nsBrowser -> CLong -> CLong -> IO RawId
itemAtRow_inColumn nsBrowser  row column =
    fmap (RawId . castPtr) $ sendMsg nsBrowser (mkSelector "itemAtRow:inColumn:") (retPtr retVoid) [argCLong row, argCLong column]

-- | @- indexPathForColumn:@
indexPathForColumn :: IsNSBrowser nsBrowser => nsBrowser -> CLong -> IO (Id NSIndexPath)
indexPathForColumn nsBrowser  column =
    sendMsg nsBrowser (mkSelector "indexPathForColumn:") (retPtr retVoid) [argCLong column] >>= retainedObject . castPtr

-- | @- isLeafItem:@
isLeafItem :: IsNSBrowser nsBrowser => nsBrowser -> RawId -> IO Bool
isLeafItem nsBrowser  item =
    fmap ((/= 0) :: CULong -> Bool) $ sendMsg nsBrowser (mkSelector "isLeafItem:") retCULong [argPtr (castPtr (unRawId item) :: Ptr ())]

-- | @- reloadDataForRowIndexes:inColumn:@
reloadDataForRowIndexes_inColumn :: (IsNSBrowser nsBrowser, IsNSIndexSet rowIndexes) => nsBrowser -> rowIndexes -> CLong -> IO ()
reloadDataForRowIndexes_inColumn nsBrowser  rowIndexes column =
  withObjCPtr rowIndexes $ \raw_rowIndexes ->
      sendMsg nsBrowser (mkSelector "reloadDataForRowIndexes:inColumn:") retVoid [argPtr (castPtr raw_rowIndexes :: Ptr ()), argCLong column]

-- | @- parentForItemsInColumn:@
parentForItemsInColumn :: IsNSBrowser nsBrowser => nsBrowser -> CLong -> IO RawId
parentForItemsInColumn nsBrowser  column =
    fmap (RawId . castPtr) $ sendMsg nsBrowser (mkSelector "parentForItemsInColumn:") (retPtr retVoid) [argCLong column]

-- | @- scrollRowToVisible:inColumn:@
scrollRowToVisible_inColumn :: IsNSBrowser nsBrowser => nsBrowser -> CLong -> CLong -> IO ()
scrollRowToVisible_inColumn nsBrowser  row column =
    sendMsg nsBrowser (mkSelector "scrollRowToVisible:inColumn:") retVoid [argCLong row, argCLong column]

-- | @- setTitle:ofColumn:@
setTitle_ofColumn :: (IsNSBrowser nsBrowser, IsNSString string) => nsBrowser -> string -> CLong -> IO ()
setTitle_ofColumn nsBrowser  string column =
  withObjCPtr string $ \raw_string ->
      sendMsg nsBrowser (mkSelector "setTitle:ofColumn:") retVoid [argPtr (castPtr raw_string :: Ptr ()), argCLong column]

-- | @- titleOfColumn:@
titleOfColumn :: IsNSBrowser nsBrowser => nsBrowser -> CLong -> IO (Id NSString)
titleOfColumn nsBrowser  column =
    sendMsg nsBrowser (mkSelector "titleOfColumn:") (retPtr retVoid) [argCLong column] >>= retainedObject . castPtr

-- | @- setPath:@
setPath :: (IsNSBrowser nsBrowser, IsNSString path) => nsBrowser -> path -> IO Bool
setPath nsBrowser  path =
  withObjCPtr path $ \raw_path ->
      fmap ((/= 0) :: CULong -> Bool) $ sendMsg nsBrowser (mkSelector "setPath:") retCULong [argPtr (castPtr raw_path :: Ptr ())]

-- | @- path@
path :: IsNSBrowser nsBrowser => nsBrowser -> IO (Id NSString)
path nsBrowser  =
    sendMsg nsBrowser (mkSelector "path") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- pathToColumn:@
pathToColumn :: IsNSBrowser nsBrowser => nsBrowser -> CLong -> IO (Id NSString)
pathToColumn nsBrowser  column =
    sendMsg nsBrowser (mkSelector "pathToColumn:") (retPtr retVoid) [argCLong column] >>= retainedObject . castPtr

-- | @- selectedCellInColumn:@
selectedCellInColumn :: IsNSBrowser nsBrowser => nsBrowser -> CLong -> IO RawId
selectedCellInColumn nsBrowser  column =
    fmap (RawId . castPtr) $ sendMsg nsBrowser (mkSelector "selectedCellInColumn:") (retPtr retVoid) [argCLong column]

-- | @- selectRow:inColumn:@
selectRow_inColumn :: IsNSBrowser nsBrowser => nsBrowser -> CLong -> CLong -> IO ()
selectRow_inColumn nsBrowser  row column =
    sendMsg nsBrowser (mkSelector "selectRow:inColumn:") retVoid [argCLong row, argCLong column]

-- | @- selectedRowInColumn:@
selectedRowInColumn :: IsNSBrowser nsBrowser => nsBrowser -> CLong -> IO CLong
selectedRowInColumn nsBrowser  column =
    sendMsg nsBrowser (mkSelector "selectedRowInColumn:") retCLong [argCLong column]

-- | @- selectRowIndexes:inColumn:@
selectRowIndexes_inColumn :: (IsNSBrowser nsBrowser, IsNSIndexSet indexes) => nsBrowser -> indexes -> CLong -> IO ()
selectRowIndexes_inColumn nsBrowser  indexes column =
  withObjCPtr indexes $ \raw_indexes ->
      sendMsg nsBrowser (mkSelector "selectRowIndexes:inColumn:") retVoid [argPtr (castPtr raw_indexes :: Ptr ()), argCLong column]

-- | @- selectedRowIndexesInColumn:@
selectedRowIndexesInColumn :: IsNSBrowser nsBrowser => nsBrowser -> CLong -> IO (Id NSIndexSet)
selectedRowIndexesInColumn nsBrowser  column =
    sendMsg nsBrowser (mkSelector "selectedRowIndexesInColumn:") (retPtr retVoid) [argCLong column] >>= retainedObject . castPtr

-- | @- reloadColumn:@
reloadColumn :: IsNSBrowser nsBrowser => nsBrowser -> CLong -> IO ()
reloadColumn nsBrowser  column =
    sendMsg nsBrowser (mkSelector "reloadColumn:") retVoid [argCLong column]

-- | @- validateVisibleColumns@
validateVisibleColumns :: IsNSBrowser nsBrowser => nsBrowser -> IO ()
validateVisibleColumns nsBrowser  =
    sendMsg nsBrowser (mkSelector "validateVisibleColumns") retVoid []

-- | @- scrollColumnsRightBy:@
scrollColumnsRightBy :: IsNSBrowser nsBrowser => nsBrowser -> CLong -> IO ()
scrollColumnsRightBy nsBrowser  shiftAmount =
    sendMsg nsBrowser (mkSelector "scrollColumnsRightBy:") retVoid [argCLong shiftAmount]

-- | @- scrollColumnsLeftBy:@
scrollColumnsLeftBy :: IsNSBrowser nsBrowser => nsBrowser -> CLong -> IO ()
scrollColumnsLeftBy nsBrowser  shiftAmount =
    sendMsg nsBrowser (mkSelector "scrollColumnsLeftBy:") retVoid [argCLong shiftAmount]

-- | @- scrollColumnToVisible:@
scrollColumnToVisible :: IsNSBrowser nsBrowser => nsBrowser -> CLong -> IO ()
scrollColumnToVisible nsBrowser  column =
    sendMsg nsBrowser (mkSelector "scrollColumnToVisible:") retVoid [argCLong column]

-- | @- addColumn@
addColumn :: IsNSBrowser nsBrowser => nsBrowser -> IO ()
addColumn nsBrowser  =
    sendMsg nsBrowser (mkSelector "addColumn") retVoid []

-- | @- loadedCellAtRow:column:@
loadedCellAtRow_column :: IsNSBrowser nsBrowser => nsBrowser -> CLong -> CLong -> IO RawId
loadedCellAtRow_column nsBrowser  row col =
    fmap (RawId . castPtr) $ sendMsg nsBrowser (mkSelector "loadedCellAtRow:column:") (retPtr retVoid) [argCLong row, argCLong col]

-- | @- selectAll:@
selectAll :: IsNSBrowser nsBrowser => nsBrowser -> RawId -> IO ()
selectAll nsBrowser  sender =
    sendMsg nsBrowser (mkSelector "selectAll:") retVoid [argPtr (castPtr (unRawId sender) :: Ptr ())]

-- | @- tile@
tile :: IsNSBrowser nsBrowser => nsBrowser -> IO ()
tile nsBrowser  =
    sendMsg nsBrowser (mkSelector "tile") retVoid []

-- | @- doClick:@
doClick :: IsNSBrowser nsBrowser => nsBrowser -> RawId -> IO ()
doClick nsBrowser  sender =
    sendMsg nsBrowser (mkSelector "doClick:") retVoid [argPtr (castPtr (unRawId sender) :: Ptr ())]

-- | @- doDoubleClick:@
doDoubleClick :: IsNSBrowser nsBrowser => nsBrowser -> RawId -> IO ()
doDoubleClick nsBrowser  sender =
    sendMsg nsBrowser (mkSelector "doDoubleClick:") retVoid [argPtr (castPtr (unRawId sender) :: Ptr ())]

-- | @- sendAction@
sendAction :: IsNSBrowser nsBrowser => nsBrowser -> IO Bool
sendAction nsBrowser  =
    fmap ((/= 0) :: CULong -> Bool) $ sendMsg nsBrowser (mkSelector "sendAction") retCULong []

-- | @- titleFrameOfColumn:@
titleFrameOfColumn :: IsNSBrowser nsBrowser => nsBrowser -> CLong -> IO NSRect
titleFrameOfColumn nsBrowser  column =
    sendMsgStret nsBrowser (mkSelector "titleFrameOfColumn:") retNSRect [argCLong column]

-- | @- drawTitleOfColumn:inRect:@
drawTitleOfColumn_inRect :: IsNSBrowser nsBrowser => nsBrowser -> CLong -> NSRect -> IO ()
drawTitleOfColumn_inRect nsBrowser  column rect =
    sendMsg nsBrowser (mkSelector "drawTitleOfColumn:inRect:") retVoid [argCLong column, argNSRect rect]

-- | @- frameOfColumn:@
frameOfColumn :: IsNSBrowser nsBrowser => nsBrowser -> CLong -> IO NSRect
frameOfColumn nsBrowser  column =
    sendMsgStret nsBrowser (mkSelector "frameOfColumn:") retNSRect [argCLong column]

-- | @- frameOfInsideOfColumn:@
frameOfInsideOfColumn :: IsNSBrowser nsBrowser => nsBrowser -> CLong -> IO NSRect
frameOfInsideOfColumn nsBrowser  column =
    sendMsgStret nsBrowser (mkSelector "frameOfInsideOfColumn:") retNSRect [argCLong column]

-- | @- frameOfRow:inColumn:@
frameOfRow_inColumn :: IsNSBrowser nsBrowser => nsBrowser -> CLong -> CLong -> IO NSRect
frameOfRow_inColumn nsBrowser  row column =
    sendMsgStret nsBrowser (mkSelector "frameOfRow:inColumn:") retNSRect [argCLong row, argCLong column]

-- | @- getRow:column:forPoint:@
getRow_column_forPoint :: IsNSBrowser nsBrowser => nsBrowser -> Ptr CLong -> Ptr CLong -> NSPoint -> IO Bool
getRow_column_forPoint nsBrowser  row column point =
    fmap ((/= 0) :: CULong -> Bool) $ sendMsg nsBrowser (mkSelector "getRow:column:forPoint:") retCULong [argPtr row, argPtr column, argNSPoint point]

-- | @- columnWidthForColumnContentWidth:@
columnWidthForColumnContentWidth :: IsNSBrowser nsBrowser => nsBrowser -> CDouble -> IO CDouble
columnWidthForColumnContentWidth nsBrowser  columnContentWidth =
    sendMsg nsBrowser (mkSelector "columnWidthForColumnContentWidth:") retCDouble [argCDouble columnContentWidth]

-- | @- columnContentWidthForColumnWidth:@
columnContentWidthForColumnWidth :: IsNSBrowser nsBrowser => nsBrowser -> CDouble -> IO CDouble
columnContentWidthForColumnWidth nsBrowser  columnWidth =
    sendMsg nsBrowser (mkSelector "columnContentWidthForColumnWidth:") retCDouble [argCDouble columnWidth]

-- | @- setWidth:ofColumn:@
setWidth_ofColumn :: IsNSBrowser nsBrowser => nsBrowser -> CDouble -> CLong -> IO ()
setWidth_ofColumn nsBrowser  columnWidth columnIndex =
    sendMsg nsBrowser (mkSelector "setWidth:ofColumn:") retVoid [argCDouble columnWidth, argCLong columnIndex]

-- | @- widthOfColumn:@
widthOfColumn :: IsNSBrowser nsBrowser => nsBrowser -> CLong -> IO CDouble
widthOfColumn nsBrowser  column =
    sendMsg nsBrowser (mkSelector "widthOfColumn:") retCDouble [argCLong column]

-- | @- noteHeightOfRowsWithIndexesChanged:inColumn:@
noteHeightOfRowsWithIndexesChanged_inColumn :: (IsNSBrowser nsBrowser, IsNSIndexSet indexSet) => nsBrowser -> indexSet -> CLong -> IO ()
noteHeightOfRowsWithIndexesChanged_inColumn nsBrowser  indexSet columnIndex =
  withObjCPtr indexSet $ \raw_indexSet ->
      sendMsg nsBrowser (mkSelector "noteHeightOfRowsWithIndexesChanged:inColumn:") retVoid [argPtr (castPtr raw_indexSet :: Ptr ()), argCLong columnIndex]

-- | @- setDefaultColumnWidth:@
setDefaultColumnWidth :: IsNSBrowser nsBrowser => nsBrowser -> CDouble -> IO ()
setDefaultColumnWidth nsBrowser  columnWidth =
    sendMsg nsBrowser (mkSelector "setDefaultColumnWidth:") retVoid [argCDouble columnWidth]

-- | @- defaultColumnWidth@
defaultColumnWidth :: IsNSBrowser nsBrowser => nsBrowser -> IO CDouble
defaultColumnWidth nsBrowser  =
    sendMsg nsBrowser (mkSelector "defaultColumnWidth") retCDouble []

-- | @+ removeSavedColumnsWithAutosaveName:@
removeSavedColumnsWithAutosaveName :: IsNSString name => name -> IO ()
removeSavedColumnsWithAutosaveName name =
  do
    cls' <- getRequiredClass "NSBrowser"
    withObjCPtr name $ \raw_name ->
      sendClassMsg cls' (mkSelector "removeSavedColumnsWithAutosaveName:") retVoid [argPtr (castPtr raw_name :: Ptr ())]

-- | @- canDragRowsWithIndexes:inColumn:withEvent:@
canDragRowsWithIndexes_inColumn_withEvent :: (IsNSBrowser nsBrowser, IsNSIndexSet rowIndexes, IsNSEvent event) => nsBrowser -> rowIndexes -> CLong -> event -> IO Bool
canDragRowsWithIndexes_inColumn_withEvent nsBrowser  rowIndexes column event =
  withObjCPtr rowIndexes $ \raw_rowIndexes ->
    withObjCPtr event $ \raw_event ->
        fmap ((/= 0) :: CULong -> Bool) $ sendMsg nsBrowser (mkSelector "canDragRowsWithIndexes:inColumn:withEvent:") retCULong [argPtr (castPtr raw_rowIndexes :: Ptr ()), argCLong column, argPtr (castPtr raw_event :: Ptr ())]

-- | @- draggingImageForRowsWithIndexes:inColumn:withEvent:offset:@
draggingImageForRowsWithIndexes_inColumn_withEvent_offset :: (IsNSBrowser nsBrowser, IsNSIndexSet rowIndexes, IsNSEvent event) => nsBrowser -> rowIndexes -> CLong -> event -> Ptr NSPoint -> IO (Id NSImage)
draggingImageForRowsWithIndexes_inColumn_withEvent_offset nsBrowser  rowIndexes column event dragImageOffset =
  withObjCPtr rowIndexes $ \raw_rowIndexes ->
    withObjCPtr event $ \raw_event ->
        sendMsg nsBrowser (mkSelector "draggingImageForRowsWithIndexes:inColumn:withEvent:offset:") (retPtr retVoid) [argPtr (castPtr raw_rowIndexes :: Ptr ()), argCLong column, argPtr (castPtr raw_event :: Ptr ()), argPtr dragImageOffset] >>= retainedObject . castPtr

-- | @- setDraggingSourceOperationMask:forLocal:@
setDraggingSourceOperationMask_forLocal :: IsNSBrowser nsBrowser => nsBrowser -> NSDragOperation -> Bool -> IO ()
setDraggingSourceOperationMask_forLocal nsBrowser  mask isLocal =
    sendMsg nsBrowser (mkSelector "setDraggingSourceOperationMask:forLocal:") retVoid [argCULong (coerce mask), argCULong (if isLocal then 1 else 0)]

-- | @- editItemAtIndexPath:withEvent:select:@
editItemAtIndexPath_withEvent_select :: (IsNSBrowser nsBrowser, IsNSIndexPath indexPath, IsNSEvent event) => nsBrowser -> indexPath -> event -> Bool -> IO ()
editItemAtIndexPath_withEvent_select nsBrowser  indexPath event select =
  withObjCPtr indexPath $ \raw_indexPath ->
    withObjCPtr event $ \raw_event ->
        sendMsg nsBrowser (mkSelector "editItemAtIndexPath:withEvent:select:") retVoid [argPtr (castPtr raw_indexPath :: Ptr ()), argPtr (castPtr raw_event :: Ptr ()), argCULong (if select then 1 else 0)]

-- | @- setAcceptsArrowKeys:@
setAcceptsArrowKeys :: IsNSBrowser nsBrowser => nsBrowser -> Bool -> IO ()
setAcceptsArrowKeys nsBrowser  flag =
    sendMsg nsBrowser (mkSelector "setAcceptsArrowKeys:") retVoid [argCULong (if flag then 1 else 0)]

-- | @- acceptsArrowKeys@
acceptsArrowKeys :: IsNSBrowser nsBrowser => nsBrowser -> IO Bool
acceptsArrowKeys nsBrowser  =
    fmap ((/= 0) :: CULong -> Bool) $ sendMsg nsBrowser (mkSelector "acceptsArrowKeys") retCULong []

-- | @- displayColumn:@
displayColumn :: IsNSBrowser nsBrowser => nsBrowser -> CLong -> IO ()
displayColumn nsBrowser  column =
    sendMsg nsBrowser (mkSelector "displayColumn:") retVoid [argCLong column]

-- | @- displayAllColumns@
displayAllColumns :: IsNSBrowser nsBrowser => nsBrowser -> IO ()
displayAllColumns nsBrowser  =
    sendMsg nsBrowser (mkSelector "displayAllColumns") retVoid []

-- | @- scrollViaScroller:@
scrollViaScroller :: (IsNSBrowser nsBrowser, IsNSScroller sender) => nsBrowser -> sender -> IO ()
scrollViaScroller nsBrowser  sender =
  withObjCPtr sender $ \raw_sender ->
      sendMsg nsBrowser (mkSelector "scrollViaScroller:") retVoid [argPtr (castPtr raw_sender :: Ptr ())]

-- | @- updateScroller@
updateScroller :: IsNSBrowser nsBrowser => nsBrowser -> IO ()
updateScroller nsBrowser  =
    sendMsg nsBrowser (mkSelector "updateScroller") retVoid []

-- | @- setMatrixClass:@
setMatrixClass :: IsNSBrowser nsBrowser => nsBrowser -> Class -> IO ()
setMatrixClass nsBrowser  factoryId =
    sendMsg nsBrowser (mkSelector "setMatrixClass:") retVoid [argPtr (unClass factoryId)]

-- | @- matrixClass@
matrixClass :: IsNSBrowser nsBrowser => nsBrowser -> IO Class
matrixClass nsBrowser  =
    fmap (Class . castPtr) $ sendMsg nsBrowser (mkSelector "matrixClass") (retPtr retVoid) []

-- | @- columnOfMatrix:@
columnOfMatrix :: (IsNSBrowser nsBrowser, IsNSMatrix matrix) => nsBrowser -> matrix -> IO CLong
columnOfMatrix nsBrowser  matrix =
  withObjCPtr matrix $ \raw_matrix ->
      sendMsg nsBrowser (mkSelector "columnOfMatrix:") retCLong [argPtr (castPtr raw_matrix :: Ptr ())]

-- | @- matrixInColumn:@
matrixInColumn :: IsNSBrowser nsBrowser => nsBrowser -> CLong -> IO (Id NSMatrix)
matrixInColumn nsBrowser  column =
    sendMsg nsBrowser (mkSelector "matrixInColumn:") (retPtr retVoid) [argCLong column] >>= retainedObject . castPtr

-- | @+ cellClass@
cellClass :: IO Class
cellClass  =
  do
    cls' <- getRequiredClass "NSBrowser"
    fmap (Class . castPtr) $ sendClassMsg cls' (mkSelector "cellClass") (retPtr retVoid) []

-- | @- loaded@
loaded :: IsNSBrowser nsBrowser => nsBrowser -> IO Bool
loaded nsBrowser  =
    fmap ((/= 0) :: CULong -> Bool) $ sendMsg nsBrowser (mkSelector "loaded") retCULong []

-- | @- doubleAction@
doubleAction :: IsNSBrowser nsBrowser => nsBrowser -> IO Selector
doubleAction nsBrowser  =
    fmap (Selector . castPtr) $ sendMsg nsBrowser (mkSelector "doubleAction") (retPtr retVoid) []

-- | @- setDoubleAction:@
setDoubleAction :: IsNSBrowser nsBrowser => nsBrowser -> Selector -> IO ()
setDoubleAction nsBrowser  value =
    sendMsg nsBrowser (mkSelector "setDoubleAction:") retVoid [argPtr (unSelector value)]

-- | @- cellPrototype@
cellPrototype :: IsNSBrowser nsBrowser => nsBrowser -> IO RawId
cellPrototype nsBrowser  =
    fmap (RawId . castPtr) $ sendMsg nsBrowser (mkSelector "cellPrototype") (retPtr retVoid) []

-- | @- setCellPrototype:@
setCellPrototype :: IsNSBrowser nsBrowser => nsBrowser -> RawId -> IO ()
setCellPrototype nsBrowser  value =
    sendMsg nsBrowser (mkSelector "setCellPrototype:") retVoid [argPtr (castPtr (unRawId value) :: Ptr ())]

-- | @- delegate@
delegate :: IsNSBrowser nsBrowser => nsBrowser -> IO RawId
delegate nsBrowser  =
    fmap (RawId . castPtr) $ sendMsg nsBrowser (mkSelector "delegate") (retPtr retVoid) []

-- | @- setDelegate:@
setDelegate :: IsNSBrowser nsBrowser => nsBrowser -> RawId -> IO ()
setDelegate nsBrowser  value =
    sendMsg nsBrowser (mkSelector "setDelegate:") retVoid [argPtr (castPtr (unRawId value) :: Ptr ())]

-- | @- reusesColumns@
reusesColumns :: IsNSBrowser nsBrowser => nsBrowser -> IO Bool
reusesColumns nsBrowser  =
    fmap ((/= 0) :: CULong -> Bool) $ sendMsg nsBrowser (mkSelector "reusesColumns") retCULong []

-- | @- setReusesColumns:@
setReusesColumns :: IsNSBrowser nsBrowser => nsBrowser -> Bool -> IO ()
setReusesColumns nsBrowser  value =
    sendMsg nsBrowser (mkSelector "setReusesColumns:") retVoid [argCULong (if value then 1 else 0)]

-- | @- hasHorizontalScroller@
hasHorizontalScroller :: IsNSBrowser nsBrowser => nsBrowser -> IO Bool
hasHorizontalScroller nsBrowser  =
    fmap ((/= 0) :: CULong -> Bool) $ sendMsg nsBrowser (mkSelector "hasHorizontalScroller") retCULong []

-- | @- setHasHorizontalScroller:@
setHasHorizontalScroller :: IsNSBrowser nsBrowser => nsBrowser -> Bool -> IO ()
setHasHorizontalScroller nsBrowser  value =
    sendMsg nsBrowser (mkSelector "setHasHorizontalScroller:") retVoid [argCULong (if value then 1 else 0)]

-- | @- autohidesScroller@
autohidesScroller :: IsNSBrowser nsBrowser => nsBrowser -> IO Bool
autohidesScroller nsBrowser  =
    fmap ((/= 0) :: CULong -> Bool) $ sendMsg nsBrowser (mkSelector "autohidesScroller") retCULong []

-- | @- setAutohidesScroller:@
setAutohidesScroller :: IsNSBrowser nsBrowser => nsBrowser -> Bool -> IO ()
setAutohidesScroller nsBrowser  value =
    sendMsg nsBrowser (mkSelector "setAutohidesScroller:") retVoid [argCULong (if value then 1 else 0)]

-- | @- separatesColumns@
separatesColumns :: IsNSBrowser nsBrowser => nsBrowser -> IO Bool
separatesColumns nsBrowser  =
    fmap ((/= 0) :: CULong -> Bool) $ sendMsg nsBrowser (mkSelector "separatesColumns") retCULong []

-- | @- setSeparatesColumns:@
setSeparatesColumns :: IsNSBrowser nsBrowser => nsBrowser -> Bool -> IO ()
setSeparatesColumns nsBrowser  value =
    sendMsg nsBrowser (mkSelector "setSeparatesColumns:") retVoid [argCULong (if value then 1 else 0)]

-- | @- titled@
titled :: IsNSBrowser nsBrowser => nsBrowser -> IO Bool
titled nsBrowser  =
    fmap ((/= 0) :: CULong -> Bool) $ sendMsg nsBrowser (mkSelector "titled") retCULong []

-- | @- setTitled:@
setTitled :: IsNSBrowser nsBrowser => nsBrowser -> Bool -> IO ()
setTitled nsBrowser  value =
    sendMsg nsBrowser (mkSelector "setTitled:") retVoid [argCULong (if value then 1 else 0)]

-- | @- minColumnWidth@
minColumnWidth :: IsNSBrowser nsBrowser => nsBrowser -> IO CDouble
minColumnWidth nsBrowser  =
    sendMsg nsBrowser (mkSelector "minColumnWidth") retCDouble []

-- | @- setMinColumnWidth:@
setMinColumnWidth :: IsNSBrowser nsBrowser => nsBrowser -> CDouble -> IO ()
setMinColumnWidth nsBrowser  value =
    sendMsg nsBrowser (mkSelector "setMinColumnWidth:") retVoid [argCDouble value]

-- | @- maxVisibleColumns@
maxVisibleColumns :: IsNSBrowser nsBrowser => nsBrowser -> IO CLong
maxVisibleColumns nsBrowser  =
    sendMsg nsBrowser (mkSelector "maxVisibleColumns") retCLong []

-- | @- setMaxVisibleColumns:@
setMaxVisibleColumns :: IsNSBrowser nsBrowser => nsBrowser -> CLong -> IO ()
setMaxVisibleColumns nsBrowser  value =
    sendMsg nsBrowser (mkSelector "setMaxVisibleColumns:") retVoid [argCLong value]

-- | @- allowsMultipleSelection@
allowsMultipleSelection :: IsNSBrowser nsBrowser => nsBrowser -> IO Bool
allowsMultipleSelection nsBrowser  =
    fmap ((/= 0) :: CULong -> Bool) $ sendMsg nsBrowser (mkSelector "allowsMultipleSelection") retCULong []

-- | @- setAllowsMultipleSelection:@
setAllowsMultipleSelection :: IsNSBrowser nsBrowser => nsBrowser -> Bool -> IO ()
setAllowsMultipleSelection nsBrowser  value =
    sendMsg nsBrowser (mkSelector "setAllowsMultipleSelection:") retVoid [argCULong (if value then 1 else 0)]

-- | @- allowsBranchSelection@
allowsBranchSelection :: IsNSBrowser nsBrowser => nsBrowser -> IO Bool
allowsBranchSelection nsBrowser  =
    fmap ((/= 0) :: CULong -> Bool) $ sendMsg nsBrowser (mkSelector "allowsBranchSelection") retCULong []

-- | @- setAllowsBranchSelection:@
setAllowsBranchSelection :: IsNSBrowser nsBrowser => nsBrowser -> Bool -> IO ()
setAllowsBranchSelection nsBrowser  value =
    sendMsg nsBrowser (mkSelector "setAllowsBranchSelection:") retVoid [argCULong (if value then 1 else 0)]

-- | @- allowsEmptySelection@
allowsEmptySelection :: IsNSBrowser nsBrowser => nsBrowser -> IO Bool
allowsEmptySelection nsBrowser  =
    fmap ((/= 0) :: CULong -> Bool) $ sendMsg nsBrowser (mkSelector "allowsEmptySelection") retCULong []

-- | @- setAllowsEmptySelection:@
setAllowsEmptySelection :: IsNSBrowser nsBrowser => nsBrowser -> Bool -> IO ()
setAllowsEmptySelection nsBrowser  value =
    sendMsg nsBrowser (mkSelector "setAllowsEmptySelection:") retVoid [argCULong (if value then 1 else 0)]

-- | @- takesTitleFromPreviousColumn@
takesTitleFromPreviousColumn :: IsNSBrowser nsBrowser => nsBrowser -> IO Bool
takesTitleFromPreviousColumn nsBrowser  =
    fmap ((/= 0) :: CULong -> Bool) $ sendMsg nsBrowser (mkSelector "takesTitleFromPreviousColumn") retCULong []

-- | @- setTakesTitleFromPreviousColumn:@
setTakesTitleFromPreviousColumn :: IsNSBrowser nsBrowser => nsBrowser -> Bool -> IO ()
setTakesTitleFromPreviousColumn nsBrowser  value =
    sendMsg nsBrowser (mkSelector "setTakesTitleFromPreviousColumn:") retVoid [argCULong (if value then 1 else 0)]

-- | @- sendsActionOnArrowKeys@
sendsActionOnArrowKeys :: IsNSBrowser nsBrowser => nsBrowser -> IO Bool
sendsActionOnArrowKeys nsBrowser  =
    fmap ((/= 0) :: CULong -> Bool) $ sendMsg nsBrowser (mkSelector "sendsActionOnArrowKeys") retCULong []

-- | @- setSendsActionOnArrowKeys:@
setSendsActionOnArrowKeys :: IsNSBrowser nsBrowser => nsBrowser -> Bool -> IO ()
setSendsActionOnArrowKeys nsBrowser  value =
    sendMsg nsBrowser (mkSelector "setSendsActionOnArrowKeys:") retVoid [argCULong (if value then 1 else 0)]

-- | @- pathSeparator@
pathSeparator :: IsNSBrowser nsBrowser => nsBrowser -> IO (Id NSString)
pathSeparator nsBrowser  =
    sendMsg nsBrowser (mkSelector "pathSeparator") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- setPathSeparator:@
setPathSeparator :: (IsNSBrowser nsBrowser, IsNSString value) => nsBrowser -> value -> IO ()
setPathSeparator nsBrowser  value =
  withObjCPtr value $ \raw_value ->
      sendMsg nsBrowser (mkSelector "setPathSeparator:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- | @- clickedColumn@
clickedColumn :: IsNSBrowser nsBrowser => nsBrowser -> IO CLong
clickedColumn nsBrowser  =
    sendMsg nsBrowser (mkSelector "clickedColumn") retCLong []

-- | @- clickedRow@
clickedRow :: IsNSBrowser nsBrowser => nsBrowser -> IO CLong
clickedRow nsBrowser  =
    sendMsg nsBrowser (mkSelector "clickedRow") retCLong []

-- | @- selectedColumn@
selectedColumn :: IsNSBrowser nsBrowser => nsBrowser -> IO CLong
selectedColumn nsBrowser  =
    sendMsg nsBrowser (mkSelector "selectedColumn") retCLong []

-- | @- selectedCell@
selectedCell :: IsNSBrowser nsBrowser => nsBrowser -> IO RawId
selectedCell nsBrowser  =
    fmap (RawId . castPtr) $ sendMsg nsBrowser (mkSelector "selectedCell") (retPtr retVoid) []

-- | @- selectedCells@
selectedCells :: IsNSBrowser nsBrowser => nsBrowser -> IO (Id NSArray)
selectedCells nsBrowser  =
    sendMsg nsBrowser (mkSelector "selectedCells") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- selectionIndexPath@
selectionIndexPath :: IsNSBrowser nsBrowser => nsBrowser -> IO (Id NSIndexPath)
selectionIndexPath nsBrowser  =
    sendMsg nsBrowser (mkSelector "selectionIndexPath") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- setSelectionIndexPath:@
setSelectionIndexPath :: (IsNSBrowser nsBrowser, IsNSIndexPath value) => nsBrowser -> value -> IO ()
setSelectionIndexPath nsBrowser  value =
  withObjCPtr value $ \raw_value ->
      sendMsg nsBrowser (mkSelector "setSelectionIndexPath:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- | @- selectionIndexPaths@
selectionIndexPaths :: IsNSBrowser nsBrowser => nsBrowser -> IO (Id NSArray)
selectionIndexPaths nsBrowser  =
    sendMsg nsBrowser (mkSelector "selectionIndexPaths") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- setSelectionIndexPaths:@
setSelectionIndexPaths :: (IsNSBrowser nsBrowser, IsNSArray value) => nsBrowser -> value -> IO ()
setSelectionIndexPaths nsBrowser  value =
  withObjCPtr value $ \raw_value ->
      sendMsg nsBrowser (mkSelector "setSelectionIndexPaths:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- | @- lastColumn@
lastColumn :: IsNSBrowser nsBrowser => nsBrowser -> IO CLong
lastColumn nsBrowser  =
    sendMsg nsBrowser (mkSelector "lastColumn") retCLong []

-- | @- setLastColumn:@
setLastColumn :: IsNSBrowser nsBrowser => nsBrowser -> CLong -> IO ()
setLastColumn nsBrowser  value =
    sendMsg nsBrowser (mkSelector "setLastColumn:") retVoid [argCLong value]

-- | @- numberOfVisibleColumns@
numberOfVisibleColumns :: IsNSBrowser nsBrowser => nsBrowser -> IO CLong
numberOfVisibleColumns nsBrowser  =
    sendMsg nsBrowser (mkSelector "numberOfVisibleColumns") retCLong []

-- | @- firstVisibleColumn@
firstVisibleColumn :: IsNSBrowser nsBrowser => nsBrowser -> IO CLong
firstVisibleColumn nsBrowser  =
    sendMsg nsBrowser (mkSelector "firstVisibleColumn") retCLong []

-- | @- lastVisibleColumn@
lastVisibleColumn :: IsNSBrowser nsBrowser => nsBrowser -> IO CLong
lastVisibleColumn nsBrowser  =
    sendMsg nsBrowser (mkSelector "lastVisibleColumn") retCLong []

-- | @- titleHeight@
titleHeight :: IsNSBrowser nsBrowser => nsBrowser -> IO CDouble
titleHeight nsBrowser  =
    sendMsg nsBrowser (mkSelector "titleHeight") retCDouble []

-- | @- columnResizingType@
columnResizingType :: IsNSBrowser nsBrowser => nsBrowser -> IO NSBrowserColumnResizingType
columnResizingType nsBrowser  =
    fmap (coerce :: CULong -> NSBrowserColumnResizingType) $ sendMsg nsBrowser (mkSelector "columnResizingType") retCULong []

-- | @- setColumnResizingType:@
setColumnResizingType :: IsNSBrowser nsBrowser => nsBrowser -> NSBrowserColumnResizingType -> IO ()
setColumnResizingType nsBrowser  value =
    sendMsg nsBrowser (mkSelector "setColumnResizingType:") retVoid [argCULong (coerce value)]

-- | @- prefersAllColumnUserResizing@
prefersAllColumnUserResizing :: IsNSBrowser nsBrowser => nsBrowser -> IO Bool
prefersAllColumnUserResizing nsBrowser  =
    fmap ((/= 0) :: CULong -> Bool) $ sendMsg nsBrowser (mkSelector "prefersAllColumnUserResizing") retCULong []

-- | @- setPrefersAllColumnUserResizing:@
setPrefersAllColumnUserResizing :: IsNSBrowser nsBrowser => nsBrowser -> Bool -> IO ()
setPrefersAllColumnUserResizing nsBrowser  value =
    sendMsg nsBrowser (mkSelector "setPrefersAllColumnUserResizing:") retVoid [argCULong (if value then 1 else 0)]

-- | @- rowHeight@
rowHeight :: IsNSBrowser nsBrowser => nsBrowser -> IO CDouble
rowHeight nsBrowser  =
    sendMsg nsBrowser (mkSelector "rowHeight") retCDouble []

-- | @- setRowHeight:@
setRowHeight :: IsNSBrowser nsBrowser => nsBrowser -> CDouble -> IO ()
setRowHeight nsBrowser  value =
    sendMsg nsBrowser (mkSelector "setRowHeight:") retVoid [argCDouble value]

-- | @- columnsAutosaveName@
columnsAutosaveName :: IsNSBrowser nsBrowser => nsBrowser -> IO (Id NSString)
columnsAutosaveName nsBrowser  =
    sendMsg nsBrowser (mkSelector "columnsAutosaveName") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- setColumnsAutosaveName:@
setColumnsAutosaveName :: (IsNSBrowser nsBrowser, IsNSString value) => nsBrowser -> value -> IO ()
setColumnsAutosaveName nsBrowser  value =
  withObjCPtr value $ \raw_value ->
      sendMsg nsBrowser (mkSelector "setColumnsAutosaveName:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- | @- allowsTypeSelect@
allowsTypeSelect :: IsNSBrowser nsBrowser => nsBrowser -> IO Bool
allowsTypeSelect nsBrowser  =
    fmap ((/= 0) :: CULong -> Bool) $ sendMsg nsBrowser (mkSelector "allowsTypeSelect") retCULong []

-- | @- setAllowsTypeSelect:@
setAllowsTypeSelect :: IsNSBrowser nsBrowser => nsBrowser -> Bool -> IO ()
setAllowsTypeSelect nsBrowser  value =
    sendMsg nsBrowser (mkSelector "setAllowsTypeSelect:") retVoid [argCULong (if value then 1 else 0)]

-- | @- backgroundColor@
backgroundColor :: IsNSBrowser nsBrowser => nsBrowser -> IO (Id NSColor)
backgroundColor nsBrowser  =
    sendMsg nsBrowser (mkSelector "backgroundColor") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- setBackgroundColor:@
setBackgroundColor :: (IsNSBrowser nsBrowser, IsNSColor value) => nsBrowser -> value -> IO ()
setBackgroundColor nsBrowser  value =
  withObjCPtr value $ \raw_value ->
      sendMsg nsBrowser (mkSelector "setBackgroundColor:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @loadColumnZero@
loadColumnZeroSelector :: Selector
loadColumnZeroSelector = mkSelector "loadColumnZero"

-- | @Selector@ for @setCellClass:@
setCellClassSelector :: Selector
setCellClassSelector = mkSelector "setCellClass:"

-- | @Selector@ for @itemAtIndexPath:@
itemAtIndexPathSelector :: Selector
itemAtIndexPathSelector = mkSelector "itemAtIndexPath:"

-- | @Selector@ for @itemAtRow:inColumn:@
itemAtRow_inColumnSelector :: Selector
itemAtRow_inColumnSelector = mkSelector "itemAtRow:inColumn:"

-- | @Selector@ for @indexPathForColumn:@
indexPathForColumnSelector :: Selector
indexPathForColumnSelector = mkSelector "indexPathForColumn:"

-- | @Selector@ for @isLeafItem:@
isLeafItemSelector :: Selector
isLeafItemSelector = mkSelector "isLeafItem:"

-- | @Selector@ for @reloadDataForRowIndexes:inColumn:@
reloadDataForRowIndexes_inColumnSelector :: Selector
reloadDataForRowIndexes_inColumnSelector = mkSelector "reloadDataForRowIndexes:inColumn:"

-- | @Selector@ for @parentForItemsInColumn:@
parentForItemsInColumnSelector :: Selector
parentForItemsInColumnSelector = mkSelector "parentForItemsInColumn:"

-- | @Selector@ for @scrollRowToVisible:inColumn:@
scrollRowToVisible_inColumnSelector :: Selector
scrollRowToVisible_inColumnSelector = mkSelector "scrollRowToVisible:inColumn:"

-- | @Selector@ for @setTitle:ofColumn:@
setTitle_ofColumnSelector :: Selector
setTitle_ofColumnSelector = mkSelector "setTitle:ofColumn:"

-- | @Selector@ for @titleOfColumn:@
titleOfColumnSelector :: Selector
titleOfColumnSelector = mkSelector "titleOfColumn:"

-- | @Selector@ for @setPath:@
setPathSelector :: Selector
setPathSelector = mkSelector "setPath:"

-- | @Selector@ for @path@
pathSelector :: Selector
pathSelector = mkSelector "path"

-- | @Selector@ for @pathToColumn:@
pathToColumnSelector :: Selector
pathToColumnSelector = mkSelector "pathToColumn:"

-- | @Selector@ for @selectedCellInColumn:@
selectedCellInColumnSelector :: Selector
selectedCellInColumnSelector = mkSelector "selectedCellInColumn:"

-- | @Selector@ for @selectRow:inColumn:@
selectRow_inColumnSelector :: Selector
selectRow_inColumnSelector = mkSelector "selectRow:inColumn:"

-- | @Selector@ for @selectedRowInColumn:@
selectedRowInColumnSelector :: Selector
selectedRowInColumnSelector = mkSelector "selectedRowInColumn:"

-- | @Selector@ for @selectRowIndexes:inColumn:@
selectRowIndexes_inColumnSelector :: Selector
selectRowIndexes_inColumnSelector = mkSelector "selectRowIndexes:inColumn:"

-- | @Selector@ for @selectedRowIndexesInColumn:@
selectedRowIndexesInColumnSelector :: Selector
selectedRowIndexesInColumnSelector = mkSelector "selectedRowIndexesInColumn:"

-- | @Selector@ for @reloadColumn:@
reloadColumnSelector :: Selector
reloadColumnSelector = mkSelector "reloadColumn:"

-- | @Selector@ for @validateVisibleColumns@
validateVisibleColumnsSelector :: Selector
validateVisibleColumnsSelector = mkSelector "validateVisibleColumns"

-- | @Selector@ for @scrollColumnsRightBy:@
scrollColumnsRightBySelector :: Selector
scrollColumnsRightBySelector = mkSelector "scrollColumnsRightBy:"

-- | @Selector@ for @scrollColumnsLeftBy:@
scrollColumnsLeftBySelector :: Selector
scrollColumnsLeftBySelector = mkSelector "scrollColumnsLeftBy:"

-- | @Selector@ for @scrollColumnToVisible:@
scrollColumnToVisibleSelector :: Selector
scrollColumnToVisibleSelector = mkSelector "scrollColumnToVisible:"

-- | @Selector@ for @addColumn@
addColumnSelector :: Selector
addColumnSelector = mkSelector "addColumn"

-- | @Selector@ for @loadedCellAtRow:column:@
loadedCellAtRow_columnSelector :: Selector
loadedCellAtRow_columnSelector = mkSelector "loadedCellAtRow:column:"

-- | @Selector@ for @selectAll:@
selectAllSelector :: Selector
selectAllSelector = mkSelector "selectAll:"

-- | @Selector@ for @tile@
tileSelector :: Selector
tileSelector = mkSelector "tile"

-- | @Selector@ for @doClick:@
doClickSelector :: Selector
doClickSelector = mkSelector "doClick:"

-- | @Selector@ for @doDoubleClick:@
doDoubleClickSelector :: Selector
doDoubleClickSelector = mkSelector "doDoubleClick:"

-- | @Selector@ for @sendAction@
sendActionSelector :: Selector
sendActionSelector = mkSelector "sendAction"

-- | @Selector@ for @titleFrameOfColumn:@
titleFrameOfColumnSelector :: Selector
titleFrameOfColumnSelector = mkSelector "titleFrameOfColumn:"

-- | @Selector@ for @drawTitleOfColumn:inRect:@
drawTitleOfColumn_inRectSelector :: Selector
drawTitleOfColumn_inRectSelector = mkSelector "drawTitleOfColumn:inRect:"

-- | @Selector@ for @frameOfColumn:@
frameOfColumnSelector :: Selector
frameOfColumnSelector = mkSelector "frameOfColumn:"

-- | @Selector@ for @frameOfInsideOfColumn:@
frameOfInsideOfColumnSelector :: Selector
frameOfInsideOfColumnSelector = mkSelector "frameOfInsideOfColumn:"

-- | @Selector@ for @frameOfRow:inColumn:@
frameOfRow_inColumnSelector :: Selector
frameOfRow_inColumnSelector = mkSelector "frameOfRow:inColumn:"

-- | @Selector@ for @getRow:column:forPoint:@
getRow_column_forPointSelector :: Selector
getRow_column_forPointSelector = mkSelector "getRow:column:forPoint:"

-- | @Selector@ for @columnWidthForColumnContentWidth:@
columnWidthForColumnContentWidthSelector :: Selector
columnWidthForColumnContentWidthSelector = mkSelector "columnWidthForColumnContentWidth:"

-- | @Selector@ for @columnContentWidthForColumnWidth:@
columnContentWidthForColumnWidthSelector :: Selector
columnContentWidthForColumnWidthSelector = mkSelector "columnContentWidthForColumnWidth:"

-- | @Selector@ for @setWidth:ofColumn:@
setWidth_ofColumnSelector :: Selector
setWidth_ofColumnSelector = mkSelector "setWidth:ofColumn:"

-- | @Selector@ for @widthOfColumn:@
widthOfColumnSelector :: Selector
widthOfColumnSelector = mkSelector "widthOfColumn:"

-- | @Selector@ for @noteHeightOfRowsWithIndexesChanged:inColumn:@
noteHeightOfRowsWithIndexesChanged_inColumnSelector :: Selector
noteHeightOfRowsWithIndexesChanged_inColumnSelector = mkSelector "noteHeightOfRowsWithIndexesChanged:inColumn:"

-- | @Selector@ for @setDefaultColumnWidth:@
setDefaultColumnWidthSelector :: Selector
setDefaultColumnWidthSelector = mkSelector "setDefaultColumnWidth:"

-- | @Selector@ for @defaultColumnWidth@
defaultColumnWidthSelector :: Selector
defaultColumnWidthSelector = mkSelector "defaultColumnWidth"

-- | @Selector@ for @removeSavedColumnsWithAutosaveName:@
removeSavedColumnsWithAutosaveNameSelector :: Selector
removeSavedColumnsWithAutosaveNameSelector = mkSelector "removeSavedColumnsWithAutosaveName:"

-- | @Selector@ for @canDragRowsWithIndexes:inColumn:withEvent:@
canDragRowsWithIndexes_inColumn_withEventSelector :: Selector
canDragRowsWithIndexes_inColumn_withEventSelector = mkSelector "canDragRowsWithIndexes:inColumn:withEvent:"

-- | @Selector@ for @draggingImageForRowsWithIndexes:inColumn:withEvent:offset:@
draggingImageForRowsWithIndexes_inColumn_withEvent_offsetSelector :: Selector
draggingImageForRowsWithIndexes_inColumn_withEvent_offsetSelector = mkSelector "draggingImageForRowsWithIndexes:inColumn:withEvent:offset:"

-- | @Selector@ for @setDraggingSourceOperationMask:forLocal:@
setDraggingSourceOperationMask_forLocalSelector :: Selector
setDraggingSourceOperationMask_forLocalSelector = mkSelector "setDraggingSourceOperationMask:forLocal:"

-- | @Selector@ for @editItemAtIndexPath:withEvent:select:@
editItemAtIndexPath_withEvent_selectSelector :: Selector
editItemAtIndexPath_withEvent_selectSelector = mkSelector "editItemAtIndexPath:withEvent:select:"

-- | @Selector@ for @setAcceptsArrowKeys:@
setAcceptsArrowKeysSelector :: Selector
setAcceptsArrowKeysSelector = mkSelector "setAcceptsArrowKeys:"

-- | @Selector@ for @acceptsArrowKeys@
acceptsArrowKeysSelector :: Selector
acceptsArrowKeysSelector = mkSelector "acceptsArrowKeys"

-- | @Selector@ for @displayColumn:@
displayColumnSelector :: Selector
displayColumnSelector = mkSelector "displayColumn:"

-- | @Selector@ for @displayAllColumns@
displayAllColumnsSelector :: Selector
displayAllColumnsSelector = mkSelector "displayAllColumns"

-- | @Selector@ for @scrollViaScroller:@
scrollViaScrollerSelector :: Selector
scrollViaScrollerSelector = mkSelector "scrollViaScroller:"

-- | @Selector@ for @updateScroller@
updateScrollerSelector :: Selector
updateScrollerSelector = mkSelector "updateScroller"

-- | @Selector@ for @setMatrixClass:@
setMatrixClassSelector :: Selector
setMatrixClassSelector = mkSelector "setMatrixClass:"

-- | @Selector@ for @matrixClass@
matrixClassSelector :: Selector
matrixClassSelector = mkSelector "matrixClass"

-- | @Selector@ for @columnOfMatrix:@
columnOfMatrixSelector :: Selector
columnOfMatrixSelector = mkSelector "columnOfMatrix:"

-- | @Selector@ for @matrixInColumn:@
matrixInColumnSelector :: Selector
matrixInColumnSelector = mkSelector "matrixInColumn:"

-- | @Selector@ for @cellClass@
cellClassSelector :: Selector
cellClassSelector = mkSelector "cellClass"

-- | @Selector@ for @loaded@
loadedSelector :: Selector
loadedSelector = mkSelector "loaded"

-- | @Selector@ for @doubleAction@
doubleActionSelector :: Selector
doubleActionSelector = mkSelector "doubleAction"

-- | @Selector@ for @setDoubleAction:@
setDoubleActionSelector :: Selector
setDoubleActionSelector = mkSelector "setDoubleAction:"

-- | @Selector@ for @cellPrototype@
cellPrototypeSelector :: Selector
cellPrototypeSelector = mkSelector "cellPrototype"

-- | @Selector@ for @setCellPrototype:@
setCellPrototypeSelector :: Selector
setCellPrototypeSelector = mkSelector "setCellPrototype:"

-- | @Selector@ for @delegate@
delegateSelector :: Selector
delegateSelector = mkSelector "delegate"

-- | @Selector@ for @setDelegate:@
setDelegateSelector :: Selector
setDelegateSelector = mkSelector "setDelegate:"

-- | @Selector@ for @reusesColumns@
reusesColumnsSelector :: Selector
reusesColumnsSelector = mkSelector "reusesColumns"

-- | @Selector@ for @setReusesColumns:@
setReusesColumnsSelector :: Selector
setReusesColumnsSelector = mkSelector "setReusesColumns:"

-- | @Selector@ for @hasHorizontalScroller@
hasHorizontalScrollerSelector :: Selector
hasHorizontalScrollerSelector = mkSelector "hasHorizontalScroller"

-- | @Selector@ for @setHasHorizontalScroller:@
setHasHorizontalScrollerSelector :: Selector
setHasHorizontalScrollerSelector = mkSelector "setHasHorizontalScroller:"

-- | @Selector@ for @autohidesScroller@
autohidesScrollerSelector :: Selector
autohidesScrollerSelector = mkSelector "autohidesScroller"

-- | @Selector@ for @setAutohidesScroller:@
setAutohidesScrollerSelector :: Selector
setAutohidesScrollerSelector = mkSelector "setAutohidesScroller:"

-- | @Selector@ for @separatesColumns@
separatesColumnsSelector :: Selector
separatesColumnsSelector = mkSelector "separatesColumns"

-- | @Selector@ for @setSeparatesColumns:@
setSeparatesColumnsSelector :: Selector
setSeparatesColumnsSelector = mkSelector "setSeparatesColumns:"

-- | @Selector@ for @titled@
titledSelector :: Selector
titledSelector = mkSelector "titled"

-- | @Selector@ for @setTitled:@
setTitledSelector :: Selector
setTitledSelector = mkSelector "setTitled:"

-- | @Selector@ for @minColumnWidth@
minColumnWidthSelector :: Selector
minColumnWidthSelector = mkSelector "minColumnWidth"

-- | @Selector@ for @setMinColumnWidth:@
setMinColumnWidthSelector :: Selector
setMinColumnWidthSelector = mkSelector "setMinColumnWidth:"

-- | @Selector@ for @maxVisibleColumns@
maxVisibleColumnsSelector :: Selector
maxVisibleColumnsSelector = mkSelector "maxVisibleColumns"

-- | @Selector@ for @setMaxVisibleColumns:@
setMaxVisibleColumnsSelector :: Selector
setMaxVisibleColumnsSelector = mkSelector "setMaxVisibleColumns:"

-- | @Selector@ for @allowsMultipleSelection@
allowsMultipleSelectionSelector :: Selector
allowsMultipleSelectionSelector = mkSelector "allowsMultipleSelection"

-- | @Selector@ for @setAllowsMultipleSelection:@
setAllowsMultipleSelectionSelector :: Selector
setAllowsMultipleSelectionSelector = mkSelector "setAllowsMultipleSelection:"

-- | @Selector@ for @allowsBranchSelection@
allowsBranchSelectionSelector :: Selector
allowsBranchSelectionSelector = mkSelector "allowsBranchSelection"

-- | @Selector@ for @setAllowsBranchSelection:@
setAllowsBranchSelectionSelector :: Selector
setAllowsBranchSelectionSelector = mkSelector "setAllowsBranchSelection:"

-- | @Selector@ for @allowsEmptySelection@
allowsEmptySelectionSelector :: Selector
allowsEmptySelectionSelector = mkSelector "allowsEmptySelection"

-- | @Selector@ for @setAllowsEmptySelection:@
setAllowsEmptySelectionSelector :: Selector
setAllowsEmptySelectionSelector = mkSelector "setAllowsEmptySelection:"

-- | @Selector@ for @takesTitleFromPreviousColumn@
takesTitleFromPreviousColumnSelector :: Selector
takesTitleFromPreviousColumnSelector = mkSelector "takesTitleFromPreviousColumn"

-- | @Selector@ for @setTakesTitleFromPreviousColumn:@
setTakesTitleFromPreviousColumnSelector :: Selector
setTakesTitleFromPreviousColumnSelector = mkSelector "setTakesTitleFromPreviousColumn:"

-- | @Selector@ for @sendsActionOnArrowKeys@
sendsActionOnArrowKeysSelector :: Selector
sendsActionOnArrowKeysSelector = mkSelector "sendsActionOnArrowKeys"

-- | @Selector@ for @setSendsActionOnArrowKeys:@
setSendsActionOnArrowKeysSelector :: Selector
setSendsActionOnArrowKeysSelector = mkSelector "setSendsActionOnArrowKeys:"

-- | @Selector@ for @pathSeparator@
pathSeparatorSelector :: Selector
pathSeparatorSelector = mkSelector "pathSeparator"

-- | @Selector@ for @setPathSeparator:@
setPathSeparatorSelector :: Selector
setPathSeparatorSelector = mkSelector "setPathSeparator:"

-- | @Selector@ for @clickedColumn@
clickedColumnSelector :: Selector
clickedColumnSelector = mkSelector "clickedColumn"

-- | @Selector@ for @clickedRow@
clickedRowSelector :: Selector
clickedRowSelector = mkSelector "clickedRow"

-- | @Selector@ for @selectedColumn@
selectedColumnSelector :: Selector
selectedColumnSelector = mkSelector "selectedColumn"

-- | @Selector@ for @selectedCell@
selectedCellSelector :: Selector
selectedCellSelector = mkSelector "selectedCell"

-- | @Selector@ for @selectedCells@
selectedCellsSelector :: Selector
selectedCellsSelector = mkSelector "selectedCells"

-- | @Selector@ for @selectionIndexPath@
selectionIndexPathSelector :: Selector
selectionIndexPathSelector = mkSelector "selectionIndexPath"

-- | @Selector@ for @setSelectionIndexPath:@
setSelectionIndexPathSelector :: Selector
setSelectionIndexPathSelector = mkSelector "setSelectionIndexPath:"

-- | @Selector@ for @selectionIndexPaths@
selectionIndexPathsSelector :: Selector
selectionIndexPathsSelector = mkSelector "selectionIndexPaths"

-- | @Selector@ for @setSelectionIndexPaths:@
setSelectionIndexPathsSelector :: Selector
setSelectionIndexPathsSelector = mkSelector "setSelectionIndexPaths:"

-- | @Selector@ for @lastColumn@
lastColumnSelector :: Selector
lastColumnSelector = mkSelector "lastColumn"

-- | @Selector@ for @setLastColumn:@
setLastColumnSelector :: Selector
setLastColumnSelector = mkSelector "setLastColumn:"

-- | @Selector@ for @numberOfVisibleColumns@
numberOfVisibleColumnsSelector :: Selector
numberOfVisibleColumnsSelector = mkSelector "numberOfVisibleColumns"

-- | @Selector@ for @firstVisibleColumn@
firstVisibleColumnSelector :: Selector
firstVisibleColumnSelector = mkSelector "firstVisibleColumn"

-- | @Selector@ for @lastVisibleColumn@
lastVisibleColumnSelector :: Selector
lastVisibleColumnSelector = mkSelector "lastVisibleColumn"

-- | @Selector@ for @titleHeight@
titleHeightSelector :: Selector
titleHeightSelector = mkSelector "titleHeight"

-- | @Selector@ for @columnResizingType@
columnResizingTypeSelector :: Selector
columnResizingTypeSelector = mkSelector "columnResizingType"

-- | @Selector@ for @setColumnResizingType:@
setColumnResizingTypeSelector :: Selector
setColumnResizingTypeSelector = mkSelector "setColumnResizingType:"

-- | @Selector@ for @prefersAllColumnUserResizing@
prefersAllColumnUserResizingSelector :: Selector
prefersAllColumnUserResizingSelector = mkSelector "prefersAllColumnUserResizing"

-- | @Selector@ for @setPrefersAllColumnUserResizing:@
setPrefersAllColumnUserResizingSelector :: Selector
setPrefersAllColumnUserResizingSelector = mkSelector "setPrefersAllColumnUserResizing:"

-- | @Selector@ for @rowHeight@
rowHeightSelector :: Selector
rowHeightSelector = mkSelector "rowHeight"

-- | @Selector@ for @setRowHeight:@
setRowHeightSelector :: Selector
setRowHeightSelector = mkSelector "setRowHeight:"

-- | @Selector@ for @columnsAutosaveName@
columnsAutosaveNameSelector :: Selector
columnsAutosaveNameSelector = mkSelector "columnsAutosaveName"

-- | @Selector@ for @setColumnsAutosaveName:@
setColumnsAutosaveNameSelector :: Selector
setColumnsAutosaveNameSelector = mkSelector "setColumnsAutosaveName:"

-- | @Selector@ for @allowsTypeSelect@
allowsTypeSelectSelector :: Selector
allowsTypeSelectSelector = mkSelector "allowsTypeSelect"

-- | @Selector@ for @setAllowsTypeSelect:@
setAllowsTypeSelectSelector :: Selector
setAllowsTypeSelectSelector = mkSelector "setAllowsTypeSelect:"

-- | @Selector@ for @backgroundColor@
backgroundColorSelector :: Selector
backgroundColorSelector = mkSelector "backgroundColor"

-- | @Selector@ for @setBackgroundColor:@
setBackgroundColorSelector :: Selector
setBackgroundColorSelector = mkSelector "setBackgroundColor:"

