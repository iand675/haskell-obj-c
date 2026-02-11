{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Generated bindings for @NSTableView@.
module ObjC.AppKit.NSTableView
  ( NSTableView
  , IsNSTableView(..)
  , initWithFrame
  , initWithCoder
  , noteHeightOfRowsWithIndexesChanged
  , addTableColumn
  , removeTableColumn
  , moveColumn_toColumn
  , columnWithIdentifier
  , tableColumnWithIdentifier
  , tile
  , sizeToFit
  , sizeLastColumnToFit
  , scrollRowToVisible
  , scrollColumnToVisible
  , reloadData
  , noteNumberOfRowsChanged
  , reloadDataForRowIndexes_columnIndexes
  , setIndicatorImage_inTableColumn
  , indicatorImageInTableColumn
  , canDragRowsWithIndexes_atPoint
  , dragImageForRowsWithIndexes_tableColumns_event_offset
  , setDraggingSourceOperationMask_forLocal
  , setDropRow_dropOperation
  , selectAll
  , deselectAll
  , selectColumnIndexes_byExtendingSelection
  , selectRowIndexes_byExtendingSelection
  , deselectColumn
  , deselectRow
  , isColumnSelected
  , isRowSelected
  , rectOfColumn
  , rectOfRow
  , columnIndexesInRect
  , rowsInRect
  , columnAtPoint
  , rowAtPoint
  , frameOfCellAtColumn_row
  , editColumn_row_withEvent_select
  , drawRow_clipRect
  , highlightSelectionInClipRect
  , drawGridInClipRect
  , drawBackgroundInClipRect
  , viewAtColumn_row_makeIfNecessary
  , rowViewAtRow_makeIfNecessary
  , rowForView
  , columnForView
  , makeViewWithIdentifier_owner
  , enumerateAvailableRowViewsUsingBlock
  , beginUpdates
  , endUpdates
  , insertRowsAtIndexes_withAnimation
  , removeRowsAtIndexes_withAnimation
  , moveRowAtIndex_toIndex
  , hideRowsAtIndexes_withAnimation
  , unhideRowsAtIndexes_withAnimation
  , registerNib_forIdentifier
  , didAddRowView_forRow
  , didRemoveRowView_forRow
  , setDrawsGrid
  , drawsGrid
  , selectColumn_byExtendingSelection
  , selectRow_byExtendingSelection
  , selectedColumnEnumerator
  , selectedRowEnumerator
  , dragImageForRows_event_dragImageOffset
  , setAutoresizesAllColumnsToFit
  , autoresizesAllColumnsToFit
  , columnsInRect
  , preparedCellAtColumn_row
  , textShouldBeginEditing
  , textShouldEndEditing
  , textDidBeginEditing
  , textDidEndEditing
  , textDidChange
  , shouldFocusCell_atColumn_row
  , focusedColumn
  , setFocusedColumn
  , performClickOnCellAtColumn_row
  , headerView
  , setHeaderView
  , cornerView
  , setCornerView
  , allowsColumnReordering
  , setAllowsColumnReordering
  , allowsColumnResizing
  , setAllowsColumnResizing
  , columnAutoresizingStyle
  , setColumnAutoresizingStyle
  , gridStyleMask
  , setGridStyleMask
  , intercellSpacing
  , setIntercellSpacing
  , usesAlternatingRowBackgroundColors
  , setUsesAlternatingRowBackgroundColors
  , backgroundColor
  , setBackgroundColor
  , gridColor
  , setGridColor
  , rowSizeStyle
  , setRowSizeStyle
  , effectiveRowSizeStyle
  , rowHeight
  , setRowHeight
  , tableColumns
  , numberOfColumns
  , numberOfRows
  , editedColumn
  , editedRow
  , clickedColumn
  , clickedRow
  , doubleAction
  , setDoubleAction
  , sortDescriptors
  , setSortDescriptors
  , highlightedTableColumn
  , setHighlightedTableColumn
  , verticalMotionCanBeginDrag
  , setVerticalMotionCanBeginDrag
  , allowsMultipleSelection
  , setAllowsMultipleSelection
  , allowsEmptySelection
  , setAllowsEmptySelection
  , allowsColumnSelection
  , setAllowsColumnSelection
  , selectedColumnIndexes
  , selectedRowIndexes
  , selectedColumn
  , selectedRow
  , numberOfSelectedColumns
  , numberOfSelectedRows
  , allowsTypeSelect
  , setAllowsTypeSelect
  , style
  , setStyle
  , effectiveStyle
  , selectionHighlightStyle
  , setSelectionHighlightStyle
  , draggingDestinationFeedbackStyle
  , setDraggingDestinationFeedbackStyle
  , autosaveName
  , setAutosaveName
  , autosaveTableColumns
  , setAutosaveTableColumns
  , floatsGroupRows
  , setFloatsGroupRows
  , rowActionsVisible
  , setRowActionsVisible
  , usesStaticContents
  , setUsesStaticContents
  , userInterfaceLayoutDirection
  , setUserInterfaceLayoutDirection
  , usesAutomaticRowHeights
  , setUsesAutomaticRowHeights
  , initWithFrameSelector
  , initWithCoderSelector
  , noteHeightOfRowsWithIndexesChangedSelector
  , addTableColumnSelector
  , removeTableColumnSelector
  , moveColumn_toColumnSelector
  , columnWithIdentifierSelector
  , tableColumnWithIdentifierSelector
  , tileSelector
  , sizeToFitSelector
  , sizeLastColumnToFitSelector
  , scrollRowToVisibleSelector
  , scrollColumnToVisibleSelector
  , reloadDataSelector
  , noteNumberOfRowsChangedSelector
  , reloadDataForRowIndexes_columnIndexesSelector
  , setIndicatorImage_inTableColumnSelector
  , indicatorImageInTableColumnSelector
  , canDragRowsWithIndexes_atPointSelector
  , dragImageForRowsWithIndexes_tableColumns_event_offsetSelector
  , setDraggingSourceOperationMask_forLocalSelector
  , setDropRow_dropOperationSelector
  , selectAllSelector
  , deselectAllSelector
  , selectColumnIndexes_byExtendingSelectionSelector
  , selectRowIndexes_byExtendingSelectionSelector
  , deselectColumnSelector
  , deselectRowSelector
  , isColumnSelectedSelector
  , isRowSelectedSelector
  , rectOfColumnSelector
  , rectOfRowSelector
  , columnIndexesInRectSelector
  , rowsInRectSelector
  , columnAtPointSelector
  , rowAtPointSelector
  , frameOfCellAtColumn_rowSelector
  , editColumn_row_withEvent_selectSelector
  , drawRow_clipRectSelector
  , highlightSelectionInClipRectSelector
  , drawGridInClipRectSelector
  , drawBackgroundInClipRectSelector
  , viewAtColumn_row_makeIfNecessarySelector
  , rowViewAtRow_makeIfNecessarySelector
  , rowForViewSelector
  , columnForViewSelector
  , makeViewWithIdentifier_ownerSelector
  , enumerateAvailableRowViewsUsingBlockSelector
  , beginUpdatesSelector
  , endUpdatesSelector
  , insertRowsAtIndexes_withAnimationSelector
  , removeRowsAtIndexes_withAnimationSelector
  , moveRowAtIndex_toIndexSelector
  , hideRowsAtIndexes_withAnimationSelector
  , unhideRowsAtIndexes_withAnimationSelector
  , registerNib_forIdentifierSelector
  , didAddRowView_forRowSelector
  , didRemoveRowView_forRowSelector
  , setDrawsGridSelector
  , drawsGridSelector
  , selectColumn_byExtendingSelectionSelector
  , selectRow_byExtendingSelectionSelector
  , selectedColumnEnumeratorSelector
  , selectedRowEnumeratorSelector
  , dragImageForRows_event_dragImageOffsetSelector
  , setAutoresizesAllColumnsToFitSelector
  , autoresizesAllColumnsToFitSelector
  , columnsInRectSelector
  , preparedCellAtColumn_rowSelector
  , textShouldBeginEditingSelector
  , textShouldEndEditingSelector
  , textDidBeginEditingSelector
  , textDidEndEditingSelector
  , textDidChangeSelector
  , shouldFocusCell_atColumn_rowSelector
  , focusedColumnSelector
  , setFocusedColumnSelector
  , performClickOnCellAtColumn_rowSelector
  , headerViewSelector
  , setHeaderViewSelector
  , cornerViewSelector
  , setCornerViewSelector
  , allowsColumnReorderingSelector
  , setAllowsColumnReorderingSelector
  , allowsColumnResizingSelector
  , setAllowsColumnResizingSelector
  , columnAutoresizingStyleSelector
  , setColumnAutoresizingStyleSelector
  , gridStyleMaskSelector
  , setGridStyleMaskSelector
  , intercellSpacingSelector
  , setIntercellSpacingSelector
  , usesAlternatingRowBackgroundColorsSelector
  , setUsesAlternatingRowBackgroundColorsSelector
  , backgroundColorSelector
  , setBackgroundColorSelector
  , gridColorSelector
  , setGridColorSelector
  , rowSizeStyleSelector
  , setRowSizeStyleSelector
  , effectiveRowSizeStyleSelector
  , rowHeightSelector
  , setRowHeightSelector
  , tableColumnsSelector
  , numberOfColumnsSelector
  , numberOfRowsSelector
  , editedColumnSelector
  , editedRowSelector
  , clickedColumnSelector
  , clickedRowSelector
  , doubleActionSelector
  , setDoubleActionSelector
  , sortDescriptorsSelector
  , setSortDescriptorsSelector
  , highlightedTableColumnSelector
  , setHighlightedTableColumnSelector
  , verticalMotionCanBeginDragSelector
  , setVerticalMotionCanBeginDragSelector
  , allowsMultipleSelectionSelector
  , setAllowsMultipleSelectionSelector
  , allowsEmptySelectionSelector
  , setAllowsEmptySelectionSelector
  , allowsColumnSelectionSelector
  , setAllowsColumnSelectionSelector
  , selectedColumnIndexesSelector
  , selectedRowIndexesSelector
  , selectedColumnSelector
  , selectedRowSelector
  , numberOfSelectedColumnsSelector
  , numberOfSelectedRowsSelector
  , allowsTypeSelectSelector
  , setAllowsTypeSelectSelector
  , styleSelector
  , setStyleSelector
  , effectiveStyleSelector
  , selectionHighlightStyleSelector
  , setSelectionHighlightStyleSelector
  , draggingDestinationFeedbackStyleSelector
  , setDraggingDestinationFeedbackStyleSelector
  , autosaveNameSelector
  , setAutosaveNameSelector
  , autosaveTableColumnsSelector
  , setAutosaveTableColumnsSelector
  , floatsGroupRowsSelector
  , setFloatsGroupRowsSelector
  , rowActionsVisibleSelector
  , setRowActionsVisibleSelector
  , usesStaticContentsSelector
  , setUsesStaticContentsSelector
  , userInterfaceLayoutDirectionSelector
  , setUserInterfaceLayoutDirectionSelector
  , usesAutomaticRowHeightsSelector
  , setUsesAutomaticRowHeightsSelector

  -- * Enum types
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
  , NSTableViewAnimationOptions(NSTableViewAnimationOptions)
  , pattern NSTableViewAnimationEffectNone
  , pattern NSTableViewAnimationEffectFade
  , pattern NSTableViewAnimationEffectGap
  , pattern NSTableViewAnimationSlideUp
  , pattern NSTableViewAnimationSlideDown
  , pattern NSTableViewAnimationSlideLeft
  , pattern NSTableViewAnimationSlideRight
  , NSTableViewColumnAutoresizingStyle(NSTableViewColumnAutoresizingStyle)
  , pattern NSTableViewNoColumnAutoresizing
  , pattern NSTableViewUniformColumnAutoresizingStyle
  , pattern NSTableViewSequentialColumnAutoresizingStyle
  , pattern NSTableViewReverseSequentialColumnAutoresizingStyle
  , pattern NSTableViewLastColumnOnlyAutoresizingStyle
  , pattern NSTableViewFirstColumnOnlyAutoresizingStyle
  , NSTableViewDraggingDestinationFeedbackStyle(NSTableViewDraggingDestinationFeedbackStyle)
  , pattern NSTableViewDraggingDestinationFeedbackStyleNone
  , pattern NSTableViewDraggingDestinationFeedbackStyleRegular
  , pattern NSTableViewDraggingDestinationFeedbackStyleSourceList
  , pattern NSTableViewDraggingDestinationFeedbackStyleGap
  , NSTableViewDropOperation(NSTableViewDropOperation)
  , pattern NSTableViewDropOn
  , pattern NSTableViewDropAbove
  , NSTableViewGridLineStyle(NSTableViewGridLineStyle)
  , pattern NSTableViewGridNone
  , pattern NSTableViewSolidVerticalGridLineMask
  , pattern NSTableViewSolidHorizontalGridLineMask
  , pattern NSTableViewDashedHorizontalGridLineMask
  , NSTableViewRowSizeStyle(NSTableViewRowSizeStyle)
  , pattern NSTableViewRowSizeStyleDefault
  , pattern NSTableViewRowSizeStyleCustom
  , pattern NSTableViewRowSizeStyleSmall
  , pattern NSTableViewRowSizeStyleMedium
  , pattern NSTableViewRowSizeStyleLarge
  , NSTableViewSelectionHighlightStyle(NSTableViewSelectionHighlightStyle)
  , pattern NSTableViewSelectionHighlightStyleNone
  , pattern NSTableViewSelectionHighlightStyleRegular
  , pattern NSTableViewSelectionHighlightStyleSourceList
  , NSTableViewStyle(NSTableViewStyle)
  , pattern NSTableViewStyleAutomatic
  , pattern NSTableViewStyleFullWidth
  , pattern NSTableViewStyleInset
  , pattern NSTableViewStyleSourceList
  , pattern NSTableViewStylePlain
  , NSUserInterfaceLayoutDirection(NSUserInterfaceLayoutDirection)
  , pattern NSUserInterfaceLayoutDirectionLeftToRight
  , pattern NSUserInterfaceLayoutDirectionRightToLeft

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
initWithFrame :: IsNSTableView nsTableView => nsTableView -> NSRect -> IO (Id NSTableView)
initWithFrame nsTableView  frameRect =
  sendMsg nsTableView (mkSelector "initWithFrame:") (retPtr retVoid) [argNSRect frameRect] >>= ownedObject . castPtr

-- | @- initWithCoder:@
initWithCoder :: (IsNSTableView nsTableView, IsNSCoder coder) => nsTableView -> coder -> IO (Id NSTableView)
initWithCoder nsTableView  coder =
withObjCPtr coder $ \raw_coder ->
    sendMsg nsTableView (mkSelector "initWithCoder:") (retPtr retVoid) [argPtr (castPtr raw_coder :: Ptr ())] >>= ownedObject . castPtr

-- | @- noteHeightOfRowsWithIndexesChanged:@
noteHeightOfRowsWithIndexesChanged :: (IsNSTableView nsTableView, IsNSIndexSet indexSet) => nsTableView -> indexSet -> IO ()
noteHeightOfRowsWithIndexesChanged nsTableView  indexSet =
withObjCPtr indexSet $ \raw_indexSet ->
    sendMsg nsTableView (mkSelector "noteHeightOfRowsWithIndexesChanged:") retVoid [argPtr (castPtr raw_indexSet :: Ptr ())]

-- | @- addTableColumn:@
addTableColumn :: (IsNSTableView nsTableView, IsNSTableColumn tableColumn) => nsTableView -> tableColumn -> IO ()
addTableColumn nsTableView  tableColumn =
withObjCPtr tableColumn $ \raw_tableColumn ->
    sendMsg nsTableView (mkSelector "addTableColumn:") retVoid [argPtr (castPtr raw_tableColumn :: Ptr ())]

-- | @- removeTableColumn:@
removeTableColumn :: (IsNSTableView nsTableView, IsNSTableColumn tableColumn) => nsTableView -> tableColumn -> IO ()
removeTableColumn nsTableView  tableColumn =
withObjCPtr tableColumn $ \raw_tableColumn ->
    sendMsg nsTableView (mkSelector "removeTableColumn:") retVoid [argPtr (castPtr raw_tableColumn :: Ptr ())]

-- | @- moveColumn:toColumn:@
moveColumn_toColumn :: IsNSTableView nsTableView => nsTableView -> CLong -> CLong -> IO ()
moveColumn_toColumn nsTableView  oldIndex newIndex =
  sendMsg nsTableView (mkSelector "moveColumn:toColumn:") retVoid [argCLong (fromIntegral oldIndex), argCLong (fromIntegral newIndex)]

-- | @- columnWithIdentifier:@
columnWithIdentifier :: (IsNSTableView nsTableView, IsNSString identifier) => nsTableView -> identifier -> IO CLong
columnWithIdentifier nsTableView  identifier =
withObjCPtr identifier $ \raw_identifier ->
    sendMsg nsTableView (mkSelector "columnWithIdentifier:") retCLong [argPtr (castPtr raw_identifier :: Ptr ())]

-- | @- tableColumnWithIdentifier:@
tableColumnWithIdentifier :: (IsNSTableView nsTableView, IsNSString identifier) => nsTableView -> identifier -> IO (Id NSTableColumn)
tableColumnWithIdentifier nsTableView  identifier =
withObjCPtr identifier $ \raw_identifier ->
    sendMsg nsTableView (mkSelector "tableColumnWithIdentifier:") (retPtr retVoid) [argPtr (castPtr raw_identifier :: Ptr ())] >>= retainedObject . castPtr

-- | @- tile@
tile :: IsNSTableView nsTableView => nsTableView -> IO ()
tile nsTableView  =
  sendMsg nsTableView (mkSelector "tile") retVoid []

-- | @- sizeToFit@
sizeToFit :: IsNSTableView nsTableView => nsTableView -> IO ()
sizeToFit nsTableView  =
  sendMsg nsTableView (mkSelector "sizeToFit") retVoid []

-- | @- sizeLastColumnToFit@
sizeLastColumnToFit :: IsNSTableView nsTableView => nsTableView -> IO ()
sizeLastColumnToFit nsTableView  =
  sendMsg nsTableView (mkSelector "sizeLastColumnToFit") retVoid []

-- | @- scrollRowToVisible:@
scrollRowToVisible :: IsNSTableView nsTableView => nsTableView -> CLong -> IO ()
scrollRowToVisible nsTableView  row =
  sendMsg nsTableView (mkSelector "scrollRowToVisible:") retVoid [argCLong (fromIntegral row)]

-- | @- scrollColumnToVisible:@
scrollColumnToVisible :: IsNSTableView nsTableView => nsTableView -> CLong -> IO ()
scrollColumnToVisible nsTableView  column =
  sendMsg nsTableView (mkSelector "scrollColumnToVisible:") retVoid [argCLong (fromIntegral column)]

-- | @- reloadData@
reloadData :: IsNSTableView nsTableView => nsTableView -> IO ()
reloadData nsTableView  =
  sendMsg nsTableView (mkSelector "reloadData") retVoid []

-- | @- noteNumberOfRowsChanged@
noteNumberOfRowsChanged :: IsNSTableView nsTableView => nsTableView -> IO ()
noteNumberOfRowsChanged nsTableView  =
  sendMsg nsTableView (mkSelector "noteNumberOfRowsChanged") retVoid []

-- | @- reloadDataForRowIndexes:columnIndexes:@
reloadDataForRowIndexes_columnIndexes :: (IsNSTableView nsTableView, IsNSIndexSet rowIndexes, IsNSIndexSet columnIndexes) => nsTableView -> rowIndexes -> columnIndexes -> IO ()
reloadDataForRowIndexes_columnIndexes nsTableView  rowIndexes columnIndexes =
withObjCPtr rowIndexes $ \raw_rowIndexes ->
  withObjCPtr columnIndexes $ \raw_columnIndexes ->
      sendMsg nsTableView (mkSelector "reloadDataForRowIndexes:columnIndexes:") retVoid [argPtr (castPtr raw_rowIndexes :: Ptr ()), argPtr (castPtr raw_columnIndexes :: Ptr ())]

-- | @- setIndicatorImage:inTableColumn:@
setIndicatorImage_inTableColumn :: (IsNSTableView nsTableView, IsNSImage image, IsNSTableColumn tableColumn) => nsTableView -> image -> tableColumn -> IO ()
setIndicatorImage_inTableColumn nsTableView  image tableColumn =
withObjCPtr image $ \raw_image ->
  withObjCPtr tableColumn $ \raw_tableColumn ->
      sendMsg nsTableView (mkSelector "setIndicatorImage:inTableColumn:") retVoid [argPtr (castPtr raw_image :: Ptr ()), argPtr (castPtr raw_tableColumn :: Ptr ())]

-- | @- indicatorImageInTableColumn:@
indicatorImageInTableColumn :: (IsNSTableView nsTableView, IsNSTableColumn tableColumn) => nsTableView -> tableColumn -> IO (Id NSImage)
indicatorImageInTableColumn nsTableView  tableColumn =
withObjCPtr tableColumn $ \raw_tableColumn ->
    sendMsg nsTableView (mkSelector "indicatorImageInTableColumn:") (retPtr retVoid) [argPtr (castPtr raw_tableColumn :: Ptr ())] >>= retainedObject . castPtr

-- | @- canDragRowsWithIndexes:atPoint:@
canDragRowsWithIndexes_atPoint :: (IsNSTableView nsTableView, IsNSIndexSet rowIndexes) => nsTableView -> rowIndexes -> NSPoint -> IO Bool
canDragRowsWithIndexes_atPoint nsTableView  rowIndexes mouseDownPoint =
withObjCPtr rowIndexes $ \raw_rowIndexes ->
    fmap ((/= 0) :: CULong -> Bool) $ sendMsg nsTableView (mkSelector "canDragRowsWithIndexes:atPoint:") retCULong [argPtr (castPtr raw_rowIndexes :: Ptr ()), argNSPoint mouseDownPoint]

-- | @- dragImageForRowsWithIndexes:tableColumns:event:offset:@
dragImageForRowsWithIndexes_tableColumns_event_offset :: (IsNSTableView nsTableView, IsNSIndexSet dragRows, IsNSArray tableColumns, IsNSEvent dragEvent) => nsTableView -> dragRows -> tableColumns -> dragEvent -> Ptr NSPoint -> IO (Id NSImage)
dragImageForRowsWithIndexes_tableColumns_event_offset nsTableView  dragRows tableColumns dragEvent dragImageOffset =
withObjCPtr dragRows $ \raw_dragRows ->
  withObjCPtr tableColumns $ \raw_tableColumns ->
    withObjCPtr dragEvent $ \raw_dragEvent ->
        sendMsg nsTableView (mkSelector "dragImageForRowsWithIndexes:tableColumns:event:offset:") (retPtr retVoid) [argPtr (castPtr raw_dragRows :: Ptr ()), argPtr (castPtr raw_tableColumns :: Ptr ()), argPtr (castPtr raw_dragEvent :: Ptr ()), argPtr dragImageOffset] >>= retainedObject . castPtr

-- | @- setDraggingSourceOperationMask:forLocal:@
setDraggingSourceOperationMask_forLocal :: IsNSTableView nsTableView => nsTableView -> NSDragOperation -> Bool -> IO ()
setDraggingSourceOperationMask_forLocal nsTableView  mask isLocal =
  sendMsg nsTableView (mkSelector "setDraggingSourceOperationMask:forLocal:") retVoid [argCULong (coerce mask), argCULong (if isLocal then 1 else 0)]

-- | @- setDropRow:dropOperation:@
setDropRow_dropOperation :: IsNSTableView nsTableView => nsTableView -> CLong -> NSTableViewDropOperation -> IO ()
setDropRow_dropOperation nsTableView  row dropOperation =
  sendMsg nsTableView (mkSelector "setDropRow:dropOperation:") retVoid [argCLong (fromIntegral row), argCULong (coerce dropOperation)]

-- | @- selectAll:@
selectAll :: IsNSTableView nsTableView => nsTableView -> RawId -> IO ()
selectAll nsTableView  sender =
  sendMsg nsTableView (mkSelector "selectAll:") retVoid [argPtr (castPtr (unRawId sender) :: Ptr ())]

-- | @- deselectAll:@
deselectAll :: IsNSTableView nsTableView => nsTableView -> RawId -> IO ()
deselectAll nsTableView  sender =
  sendMsg nsTableView (mkSelector "deselectAll:") retVoid [argPtr (castPtr (unRawId sender) :: Ptr ())]

-- | @- selectColumnIndexes:byExtendingSelection:@
selectColumnIndexes_byExtendingSelection :: (IsNSTableView nsTableView, IsNSIndexSet indexes) => nsTableView -> indexes -> Bool -> IO ()
selectColumnIndexes_byExtendingSelection nsTableView  indexes extend =
withObjCPtr indexes $ \raw_indexes ->
    sendMsg nsTableView (mkSelector "selectColumnIndexes:byExtendingSelection:") retVoid [argPtr (castPtr raw_indexes :: Ptr ()), argCULong (if extend then 1 else 0)]

-- | @- selectRowIndexes:byExtendingSelection:@
selectRowIndexes_byExtendingSelection :: (IsNSTableView nsTableView, IsNSIndexSet indexes) => nsTableView -> indexes -> Bool -> IO ()
selectRowIndexes_byExtendingSelection nsTableView  indexes extend =
withObjCPtr indexes $ \raw_indexes ->
    sendMsg nsTableView (mkSelector "selectRowIndexes:byExtendingSelection:") retVoid [argPtr (castPtr raw_indexes :: Ptr ()), argCULong (if extend then 1 else 0)]

-- | @- deselectColumn:@
deselectColumn :: IsNSTableView nsTableView => nsTableView -> CLong -> IO ()
deselectColumn nsTableView  column =
  sendMsg nsTableView (mkSelector "deselectColumn:") retVoid [argCLong (fromIntegral column)]

-- | @- deselectRow:@
deselectRow :: IsNSTableView nsTableView => nsTableView -> CLong -> IO ()
deselectRow nsTableView  row =
  sendMsg nsTableView (mkSelector "deselectRow:") retVoid [argCLong (fromIntegral row)]

-- | @- isColumnSelected:@
isColumnSelected :: IsNSTableView nsTableView => nsTableView -> CLong -> IO Bool
isColumnSelected nsTableView  column =
  fmap ((/= 0) :: CULong -> Bool) $ sendMsg nsTableView (mkSelector "isColumnSelected:") retCULong [argCLong (fromIntegral column)]

-- | @- isRowSelected:@
isRowSelected :: IsNSTableView nsTableView => nsTableView -> CLong -> IO Bool
isRowSelected nsTableView  row =
  fmap ((/= 0) :: CULong -> Bool) $ sendMsg nsTableView (mkSelector "isRowSelected:") retCULong [argCLong (fromIntegral row)]

-- | @- rectOfColumn:@
rectOfColumn :: IsNSTableView nsTableView => nsTableView -> CLong -> IO NSRect
rectOfColumn nsTableView  column =
  sendMsgStret nsTableView (mkSelector "rectOfColumn:") retNSRect [argCLong (fromIntegral column)]

-- | @- rectOfRow:@
rectOfRow :: IsNSTableView nsTableView => nsTableView -> CLong -> IO NSRect
rectOfRow nsTableView  row =
  sendMsgStret nsTableView (mkSelector "rectOfRow:") retNSRect [argCLong (fromIntegral row)]

-- | @- columnIndexesInRect:@
columnIndexesInRect :: IsNSTableView nsTableView => nsTableView -> NSRect -> IO (Id NSIndexSet)
columnIndexesInRect nsTableView  rect =
  sendMsg nsTableView (mkSelector "columnIndexesInRect:") (retPtr retVoid) [argNSRect rect] >>= retainedObject . castPtr

-- | @- rowsInRect:@
rowsInRect :: IsNSTableView nsTableView => nsTableView -> NSRect -> IO NSRange
rowsInRect nsTableView  rect =
  sendMsgStret nsTableView (mkSelector "rowsInRect:") retNSRange [argNSRect rect]

-- | @- columnAtPoint:@
columnAtPoint :: IsNSTableView nsTableView => nsTableView -> NSPoint -> IO CLong
columnAtPoint nsTableView  point =
  sendMsg nsTableView (mkSelector "columnAtPoint:") retCLong [argNSPoint point]

-- | @- rowAtPoint:@
rowAtPoint :: IsNSTableView nsTableView => nsTableView -> NSPoint -> IO CLong
rowAtPoint nsTableView  point =
  sendMsg nsTableView (mkSelector "rowAtPoint:") retCLong [argNSPoint point]

-- | @- frameOfCellAtColumn:row:@
frameOfCellAtColumn_row :: IsNSTableView nsTableView => nsTableView -> CLong -> CLong -> IO NSRect
frameOfCellAtColumn_row nsTableView  column row =
  sendMsgStret nsTableView (mkSelector "frameOfCellAtColumn:row:") retNSRect [argCLong (fromIntegral column), argCLong (fromIntegral row)]

-- | @- editColumn:row:withEvent:select:@
editColumn_row_withEvent_select :: (IsNSTableView nsTableView, IsNSEvent event) => nsTableView -> CLong -> CLong -> event -> Bool -> IO ()
editColumn_row_withEvent_select nsTableView  column row event select =
withObjCPtr event $ \raw_event ->
    sendMsg nsTableView (mkSelector "editColumn:row:withEvent:select:") retVoid [argCLong (fromIntegral column), argCLong (fromIntegral row), argPtr (castPtr raw_event :: Ptr ()), argCULong (if select then 1 else 0)]

-- | @- drawRow:clipRect:@
drawRow_clipRect :: IsNSTableView nsTableView => nsTableView -> CLong -> NSRect -> IO ()
drawRow_clipRect nsTableView  row clipRect =
  sendMsg nsTableView (mkSelector "drawRow:clipRect:") retVoid [argCLong (fromIntegral row), argNSRect clipRect]

-- | @- highlightSelectionInClipRect:@
highlightSelectionInClipRect :: IsNSTableView nsTableView => nsTableView -> NSRect -> IO ()
highlightSelectionInClipRect nsTableView  clipRect =
  sendMsg nsTableView (mkSelector "highlightSelectionInClipRect:") retVoid [argNSRect clipRect]

-- | @- drawGridInClipRect:@
drawGridInClipRect :: IsNSTableView nsTableView => nsTableView -> NSRect -> IO ()
drawGridInClipRect nsTableView  clipRect =
  sendMsg nsTableView (mkSelector "drawGridInClipRect:") retVoid [argNSRect clipRect]

-- | @- drawBackgroundInClipRect:@
drawBackgroundInClipRect :: IsNSTableView nsTableView => nsTableView -> NSRect -> IO ()
drawBackgroundInClipRect nsTableView  clipRect =
  sendMsg nsTableView (mkSelector "drawBackgroundInClipRect:") retVoid [argNSRect clipRect]

-- | @- viewAtColumn:row:makeIfNecessary:@
viewAtColumn_row_makeIfNecessary :: IsNSTableView nsTableView => nsTableView -> CLong -> CLong -> Bool -> IO (Id NSView)
viewAtColumn_row_makeIfNecessary nsTableView  column row makeIfNecessary =
  sendMsg nsTableView (mkSelector "viewAtColumn:row:makeIfNecessary:") (retPtr retVoid) [argCLong (fromIntegral column), argCLong (fromIntegral row), argCULong (if makeIfNecessary then 1 else 0)] >>= retainedObject . castPtr

-- | @- rowViewAtRow:makeIfNecessary:@
rowViewAtRow_makeIfNecessary :: IsNSTableView nsTableView => nsTableView -> CLong -> Bool -> IO (Id NSTableRowView)
rowViewAtRow_makeIfNecessary nsTableView  row makeIfNecessary =
  sendMsg nsTableView (mkSelector "rowViewAtRow:makeIfNecessary:") (retPtr retVoid) [argCLong (fromIntegral row), argCULong (if makeIfNecessary then 1 else 0)] >>= retainedObject . castPtr

-- | @- rowForView:@
rowForView :: (IsNSTableView nsTableView, IsNSView view) => nsTableView -> view -> IO CLong
rowForView nsTableView  view =
withObjCPtr view $ \raw_view ->
    sendMsg nsTableView (mkSelector "rowForView:") retCLong [argPtr (castPtr raw_view :: Ptr ())]

-- | @- columnForView:@
columnForView :: (IsNSTableView nsTableView, IsNSView view) => nsTableView -> view -> IO CLong
columnForView nsTableView  view =
withObjCPtr view $ \raw_view ->
    sendMsg nsTableView (mkSelector "columnForView:") retCLong [argPtr (castPtr raw_view :: Ptr ())]

-- | @- makeViewWithIdentifier:owner:@
makeViewWithIdentifier_owner :: (IsNSTableView nsTableView, IsNSString identifier) => nsTableView -> identifier -> RawId -> IO (Id NSView)
makeViewWithIdentifier_owner nsTableView  identifier owner =
withObjCPtr identifier $ \raw_identifier ->
    sendMsg nsTableView (mkSelector "makeViewWithIdentifier:owner:") (retPtr retVoid) [argPtr (castPtr raw_identifier :: Ptr ()), argPtr (castPtr (unRawId owner) :: Ptr ())] >>= retainedObject . castPtr

-- | @- enumerateAvailableRowViewsUsingBlock:@
enumerateAvailableRowViewsUsingBlock :: IsNSTableView nsTableView => nsTableView -> Ptr () -> IO ()
enumerateAvailableRowViewsUsingBlock nsTableView  handler =
  sendMsg nsTableView (mkSelector "enumerateAvailableRowViewsUsingBlock:") retVoid [argPtr (castPtr handler :: Ptr ())]

-- | @- beginUpdates@
beginUpdates :: IsNSTableView nsTableView => nsTableView -> IO ()
beginUpdates nsTableView  =
  sendMsg nsTableView (mkSelector "beginUpdates") retVoid []

-- | @- endUpdates@
endUpdates :: IsNSTableView nsTableView => nsTableView -> IO ()
endUpdates nsTableView  =
  sendMsg nsTableView (mkSelector "endUpdates") retVoid []

-- | @- insertRowsAtIndexes:withAnimation:@
insertRowsAtIndexes_withAnimation :: (IsNSTableView nsTableView, IsNSIndexSet indexes) => nsTableView -> indexes -> NSTableViewAnimationOptions -> IO ()
insertRowsAtIndexes_withAnimation nsTableView  indexes animationOptions =
withObjCPtr indexes $ \raw_indexes ->
    sendMsg nsTableView (mkSelector "insertRowsAtIndexes:withAnimation:") retVoid [argPtr (castPtr raw_indexes :: Ptr ()), argCULong (coerce animationOptions)]

-- | @- removeRowsAtIndexes:withAnimation:@
removeRowsAtIndexes_withAnimation :: (IsNSTableView nsTableView, IsNSIndexSet indexes) => nsTableView -> indexes -> NSTableViewAnimationOptions -> IO ()
removeRowsAtIndexes_withAnimation nsTableView  indexes animationOptions =
withObjCPtr indexes $ \raw_indexes ->
    sendMsg nsTableView (mkSelector "removeRowsAtIndexes:withAnimation:") retVoid [argPtr (castPtr raw_indexes :: Ptr ()), argCULong (coerce animationOptions)]

-- | @- moveRowAtIndex:toIndex:@
moveRowAtIndex_toIndex :: IsNSTableView nsTableView => nsTableView -> CLong -> CLong -> IO ()
moveRowAtIndex_toIndex nsTableView  oldIndex newIndex =
  sendMsg nsTableView (mkSelector "moveRowAtIndex:toIndex:") retVoid [argCLong (fromIntegral oldIndex), argCLong (fromIntegral newIndex)]

-- | @- hideRowsAtIndexes:withAnimation:@
hideRowsAtIndexes_withAnimation :: (IsNSTableView nsTableView, IsNSIndexSet indexes) => nsTableView -> indexes -> NSTableViewAnimationOptions -> IO ()
hideRowsAtIndexes_withAnimation nsTableView  indexes rowAnimation =
withObjCPtr indexes $ \raw_indexes ->
    sendMsg nsTableView (mkSelector "hideRowsAtIndexes:withAnimation:") retVoid [argPtr (castPtr raw_indexes :: Ptr ()), argCULong (coerce rowAnimation)]

-- | @- unhideRowsAtIndexes:withAnimation:@
unhideRowsAtIndexes_withAnimation :: (IsNSTableView nsTableView, IsNSIndexSet indexes) => nsTableView -> indexes -> NSTableViewAnimationOptions -> IO ()
unhideRowsAtIndexes_withAnimation nsTableView  indexes rowAnimation =
withObjCPtr indexes $ \raw_indexes ->
    sendMsg nsTableView (mkSelector "unhideRowsAtIndexes:withAnimation:") retVoid [argPtr (castPtr raw_indexes :: Ptr ()), argCULong (coerce rowAnimation)]

-- | @- registerNib:forIdentifier:@
registerNib_forIdentifier :: (IsNSTableView nsTableView, IsNSNib nib, IsNSString identifier) => nsTableView -> nib -> identifier -> IO ()
registerNib_forIdentifier nsTableView  nib identifier =
withObjCPtr nib $ \raw_nib ->
  withObjCPtr identifier $ \raw_identifier ->
      sendMsg nsTableView (mkSelector "registerNib:forIdentifier:") retVoid [argPtr (castPtr raw_nib :: Ptr ()), argPtr (castPtr raw_identifier :: Ptr ())]

-- | @- didAddRowView:forRow:@
didAddRowView_forRow :: (IsNSTableView nsTableView, IsNSTableRowView rowView) => nsTableView -> rowView -> CLong -> IO ()
didAddRowView_forRow nsTableView  rowView row =
withObjCPtr rowView $ \raw_rowView ->
    sendMsg nsTableView (mkSelector "didAddRowView:forRow:") retVoid [argPtr (castPtr raw_rowView :: Ptr ()), argCLong (fromIntegral row)]

-- | @- didRemoveRowView:forRow:@
didRemoveRowView_forRow :: (IsNSTableView nsTableView, IsNSTableRowView rowView) => nsTableView -> rowView -> CLong -> IO ()
didRemoveRowView_forRow nsTableView  rowView row =
withObjCPtr rowView $ \raw_rowView ->
    sendMsg nsTableView (mkSelector "didRemoveRowView:forRow:") retVoid [argPtr (castPtr raw_rowView :: Ptr ()), argCLong (fromIntegral row)]

-- | @- setDrawsGrid:@
setDrawsGrid :: IsNSTableView nsTableView => nsTableView -> Bool -> IO ()
setDrawsGrid nsTableView  flag =
  sendMsg nsTableView (mkSelector "setDrawsGrid:") retVoid [argCULong (if flag then 1 else 0)]

-- | @- drawsGrid@
drawsGrid :: IsNSTableView nsTableView => nsTableView -> IO Bool
drawsGrid nsTableView  =
  fmap ((/= 0) :: CULong -> Bool) $ sendMsg nsTableView (mkSelector "drawsGrid") retCULong []

-- | @- selectColumn:byExtendingSelection:@
selectColumn_byExtendingSelection :: IsNSTableView nsTableView => nsTableView -> CLong -> Bool -> IO ()
selectColumn_byExtendingSelection nsTableView  column extend =
  sendMsg nsTableView (mkSelector "selectColumn:byExtendingSelection:") retVoid [argCLong (fromIntegral column), argCULong (if extend then 1 else 0)]

-- | @- selectRow:byExtendingSelection:@
selectRow_byExtendingSelection :: IsNSTableView nsTableView => nsTableView -> CLong -> Bool -> IO ()
selectRow_byExtendingSelection nsTableView  row extend =
  sendMsg nsTableView (mkSelector "selectRow:byExtendingSelection:") retVoid [argCLong (fromIntegral row), argCULong (if extend then 1 else 0)]

-- | @- selectedColumnEnumerator@
selectedColumnEnumerator :: IsNSTableView nsTableView => nsTableView -> IO (Id NSEnumerator)
selectedColumnEnumerator nsTableView  =
  sendMsg nsTableView (mkSelector "selectedColumnEnumerator") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- selectedRowEnumerator@
selectedRowEnumerator :: IsNSTableView nsTableView => nsTableView -> IO (Id NSEnumerator)
selectedRowEnumerator nsTableView  =
  sendMsg nsTableView (mkSelector "selectedRowEnumerator") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- dragImageForRows:event:dragImageOffset:@
dragImageForRows_event_dragImageOffset :: (IsNSTableView nsTableView, IsNSArray dragRows, IsNSEvent dragEvent) => nsTableView -> dragRows -> dragEvent -> Ptr NSPoint -> IO (Id NSImage)
dragImageForRows_event_dragImageOffset nsTableView  dragRows dragEvent dragImageOffset =
withObjCPtr dragRows $ \raw_dragRows ->
  withObjCPtr dragEvent $ \raw_dragEvent ->
      sendMsg nsTableView (mkSelector "dragImageForRows:event:dragImageOffset:") (retPtr retVoid) [argPtr (castPtr raw_dragRows :: Ptr ()), argPtr (castPtr raw_dragEvent :: Ptr ()), argPtr dragImageOffset] >>= retainedObject . castPtr

-- | @- setAutoresizesAllColumnsToFit:@
setAutoresizesAllColumnsToFit :: IsNSTableView nsTableView => nsTableView -> Bool -> IO ()
setAutoresizesAllColumnsToFit nsTableView  flag =
  sendMsg nsTableView (mkSelector "setAutoresizesAllColumnsToFit:") retVoid [argCULong (if flag then 1 else 0)]

-- | @- autoresizesAllColumnsToFit@
autoresizesAllColumnsToFit :: IsNSTableView nsTableView => nsTableView -> IO Bool
autoresizesAllColumnsToFit nsTableView  =
  fmap ((/= 0) :: CULong -> Bool) $ sendMsg nsTableView (mkSelector "autoresizesAllColumnsToFit") retCULong []

-- | @- columnsInRect:@
columnsInRect :: IsNSTableView nsTableView => nsTableView -> NSRect -> IO NSRange
columnsInRect nsTableView  rect =
  sendMsgStret nsTableView (mkSelector "columnsInRect:") retNSRange [argNSRect rect]

-- | @- preparedCellAtColumn:row:@
preparedCellAtColumn_row :: IsNSTableView nsTableView => nsTableView -> CLong -> CLong -> IO (Id NSCell)
preparedCellAtColumn_row nsTableView  column row =
  sendMsg nsTableView (mkSelector "preparedCellAtColumn:row:") (retPtr retVoid) [argCLong (fromIntegral column), argCLong (fromIntegral row)] >>= retainedObject . castPtr

-- | @- textShouldBeginEditing:@
textShouldBeginEditing :: (IsNSTableView nsTableView, IsNSText textObject) => nsTableView -> textObject -> IO Bool
textShouldBeginEditing nsTableView  textObject =
withObjCPtr textObject $ \raw_textObject ->
    fmap ((/= 0) :: CULong -> Bool) $ sendMsg nsTableView (mkSelector "textShouldBeginEditing:") retCULong [argPtr (castPtr raw_textObject :: Ptr ())]

-- | @- textShouldEndEditing:@
textShouldEndEditing :: (IsNSTableView nsTableView, IsNSText textObject) => nsTableView -> textObject -> IO Bool
textShouldEndEditing nsTableView  textObject =
withObjCPtr textObject $ \raw_textObject ->
    fmap ((/= 0) :: CULong -> Bool) $ sendMsg nsTableView (mkSelector "textShouldEndEditing:") retCULong [argPtr (castPtr raw_textObject :: Ptr ())]

-- | @- textDidBeginEditing:@
textDidBeginEditing :: (IsNSTableView nsTableView, IsNSNotification notification) => nsTableView -> notification -> IO ()
textDidBeginEditing nsTableView  notification =
withObjCPtr notification $ \raw_notification ->
    sendMsg nsTableView (mkSelector "textDidBeginEditing:") retVoid [argPtr (castPtr raw_notification :: Ptr ())]

-- | @- textDidEndEditing:@
textDidEndEditing :: (IsNSTableView nsTableView, IsNSNotification notification) => nsTableView -> notification -> IO ()
textDidEndEditing nsTableView  notification =
withObjCPtr notification $ \raw_notification ->
    sendMsg nsTableView (mkSelector "textDidEndEditing:") retVoid [argPtr (castPtr raw_notification :: Ptr ())]

-- | @- textDidChange:@
textDidChange :: (IsNSTableView nsTableView, IsNSNotification notification) => nsTableView -> notification -> IO ()
textDidChange nsTableView  notification =
withObjCPtr notification $ \raw_notification ->
    sendMsg nsTableView (mkSelector "textDidChange:") retVoid [argPtr (castPtr raw_notification :: Ptr ())]

-- | @- shouldFocusCell:atColumn:row:@
shouldFocusCell_atColumn_row :: (IsNSTableView nsTableView, IsNSCell cell) => nsTableView -> cell -> CLong -> CLong -> IO Bool
shouldFocusCell_atColumn_row nsTableView  cell column row =
withObjCPtr cell $ \raw_cell ->
    fmap ((/= 0) :: CULong -> Bool) $ sendMsg nsTableView (mkSelector "shouldFocusCell:atColumn:row:") retCULong [argPtr (castPtr raw_cell :: Ptr ()), argCLong (fromIntegral column), argCLong (fromIntegral row)]

-- | @- focusedColumn@
focusedColumn :: IsNSTableView nsTableView => nsTableView -> IO CLong
focusedColumn nsTableView  =
  sendMsg nsTableView (mkSelector "focusedColumn") retCLong []

-- | @- setFocusedColumn:@
setFocusedColumn :: IsNSTableView nsTableView => nsTableView -> CLong -> IO ()
setFocusedColumn nsTableView  focusedColumn =
  sendMsg nsTableView (mkSelector "setFocusedColumn:") retVoid [argCLong (fromIntegral focusedColumn)]

-- | @- performClickOnCellAtColumn:row:@
performClickOnCellAtColumn_row :: IsNSTableView nsTableView => nsTableView -> CLong -> CLong -> IO ()
performClickOnCellAtColumn_row nsTableView  column row =
  sendMsg nsTableView (mkSelector "performClickOnCellAtColumn:row:") retVoid [argCLong (fromIntegral column), argCLong (fromIntegral row)]

-- | @- headerView@
headerView :: IsNSTableView nsTableView => nsTableView -> IO (Id NSTableHeaderView)
headerView nsTableView  =
  sendMsg nsTableView (mkSelector "headerView") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- setHeaderView:@
setHeaderView :: (IsNSTableView nsTableView, IsNSTableHeaderView value) => nsTableView -> value -> IO ()
setHeaderView nsTableView  value =
withObjCPtr value $ \raw_value ->
    sendMsg nsTableView (mkSelector "setHeaderView:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- | @- cornerView@
cornerView :: IsNSTableView nsTableView => nsTableView -> IO (Id NSView)
cornerView nsTableView  =
  sendMsg nsTableView (mkSelector "cornerView") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- setCornerView:@
setCornerView :: (IsNSTableView nsTableView, IsNSView value) => nsTableView -> value -> IO ()
setCornerView nsTableView  value =
withObjCPtr value $ \raw_value ->
    sendMsg nsTableView (mkSelector "setCornerView:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- | @- allowsColumnReordering@
allowsColumnReordering :: IsNSTableView nsTableView => nsTableView -> IO Bool
allowsColumnReordering nsTableView  =
  fmap ((/= 0) :: CULong -> Bool) $ sendMsg nsTableView (mkSelector "allowsColumnReordering") retCULong []

-- | @- setAllowsColumnReordering:@
setAllowsColumnReordering :: IsNSTableView nsTableView => nsTableView -> Bool -> IO ()
setAllowsColumnReordering nsTableView  value =
  sendMsg nsTableView (mkSelector "setAllowsColumnReordering:") retVoid [argCULong (if value then 1 else 0)]

-- | @- allowsColumnResizing@
allowsColumnResizing :: IsNSTableView nsTableView => nsTableView -> IO Bool
allowsColumnResizing nsTableView  =
  fmap ((/= 0) :: CULong -> Bool) $ sendMsg nsTableView (mkSelector "allowsColumnResizing") retCULong []

-- | @- setAllowsColumnResizing:@
setAllowsColumnResizing :: IsNSTableView nsTableView => nsTableView -> Bool -> IO ()
setAllowsColumnResizing nsTableView  value =
  sendMsg nsTableView (mkSelector "setAllowsColumnResizing:") retVoid [argCULong (if value then 1 else 0)]

-- | @- columnAutoresizingStyle@
columnAutoresizingStyle :: IsNSTableView nsTableView => nsTableView -> IO NSTableViewColumnAutoresizingStyle
columnAutoresizingStyle nsTableView  =
  fmap (coerce :: CULong -> NSTableViewColumnAutoresizingStyle) $ sendMsg nsTableView (mkSelector "columnAutoresizingStyle") retCULong []

-- | @- setColumnAutoresizingStyle:@
setColumnAutoresizingStyle :: IsNSTableView nsTableView => nsTableView -> NSTableViewColumnAutoresizingStyle -> IO ()
setColumnAutoresizingStyle nsTableView  value =
  sendMsg nsTableView (mkSelector "setColumnAutoresizingStyle:") retVoid [argCULong (coerce value)]

-- | @- gridStyleMask@
gridStyleMask :: IsNSTableView nsTableView => nsTableView -> IO NSTableViewGridLineStyle
gridStyleMask nsTableView  =
  fmap (coerce :: CULong -> NSTableViewGridLineStyle) $ sendMsg nsTableView (mkSelector "gridStyleMask") retCULong []

-- | @- setGridStyleMask:@
setGridStyleMask :: IsNSTableView nsTableView => nsTableView -> NSTableViewGridLineStyle -> IO ()
setGridStyleMask nsTableView  value =
  sendMsg nsTableView (mkSelector "setGridStyleMask:") retVoid [argCULong (coerce value)]

-- | @- intercellSpacing@
intercellSpacing :: IsNSTableView nsTableView => nsTableView -> IO NSSize
intercellSpacing nsTableView  =
  sendMsgStret nsTableView (mkSelector "intercellSpacing") retNSSize []

-- | @- setIntercellSpacing:@
setIntercellSpacing :: IsNSTableView nsTableView => nsTableView -> NSSize -> IO ()
setIntercellSpacing nsTableView  value =
  sendMsg nsTableView (mkSelector "setIntercellSpacing:") retVoid [argNSSize value]

-- | @- usesAlternatingRowBackgroundColors@
usesAlternatingRowBackgroundColors :: IsNSTableView nsTableView => nsTableView -> IO Bool
usesAlternatingRowBackgroundColors nsTableView  =
  fmap ((/= 0) :: CULong -> Bool) $ sendMsg nsTableView (mkSelector "usesAlternatingRowBackgroundColors") retCULong []

-- | @- setUsesAlternatingRowBackgroundColors:@
setUsesAlternatingRowBackgroundColors :: IsNSTableView nsTableView => nsTableView -> Bool -> IO ()
setUsesAlternatingRowBackgroundColors nsTableView  value =
  sendMsg nsTableView (mkSelector "setUsesAlternatingRowBackgroundColors:") retVoid [argCULong (if value then 1 else 0)]

-- | @- backgroundColor@
backgroundColor :: IsNSTableView nsTableView => nsTableView -> IO (Id NSColor)
backgroundColor nsTableView  =
  sendMsg nsTableView (mkSelector "backgroundColor") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- setBackgroundColor:@
setBackgroundColor :: (IsNSTableView nsTableView, IsNSColor value) => nsTableView -> value -> IO ()
setBackgroundColor nsTableView  value =
withObjCPtr value $ \raw_value ->
    sendMsg nsTableView (mkSelector "setBackgroundColor:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- | @- gridColor@
gridColor :: IsNSTableView nsTableView => nsTableView -> IO (Id NSColor)
gridColor nsTableView  =
  sendMsg nsTableView (mkSelector "gridColor") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- setGridColor:@
setGridColor :: (IsNSTableView nsTableView, IsNSColor value) => nsTableView -> value -> IO ()
setGridColor nsTableView  value =
withObjCPtr value $ \raw_value ->
    sendMsg nsTableView (mkSelector "setGridColor:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- | @- rowSizeStyle@
rowSizeStyle :: IsNSTableView nsTableView => nsTableView -> IO NSTableViewRowSizeStyle
rowSizeStyle nsTableView  =
  fmap (coerce :: CLong -> NSTableViewRowSizeStyle) $ sendMsg nsTableView (mkSelector "rowSizeStyle") retCLong []

-- | @- setRowSizeStyle:@
setRowSizeStyle :: IsNSTableView nsTableView => nsTableView -> NSTableViewRowSizeStyle -> IO ()
setRowSizeStyle nsTableView  value =
  sendMsg nsTableView (mkSelector "setRowSizeStyle:") retVoid [argCLong (coerce value)]

-- | @- effectiveRowSizeStyle@
effectiveRowSizeStyle :: IsNSTableView nsTableView => nsTableView -> IO NSTableViewRowSizeStyle
effectiveRowSizeStyle nsTableView  =
  fmap (coerce :: CLong -> NSTableViewRowSizeStyle) $ sendMsg nsTableView (mkSelector "effectiveRowSizeStyle") retCLong []

-- | @- rowHeight@
rowHeight :: IsNSTableView nsTableView => nsTableView -> IO CDouble
rowHeight nsTableView  =
  sendMsg nsTableView (mkSelector "rowHeight") retCDouble []

-- | @- setRowHeight:@
setRowHeight :: IsNSTableView nsTableView => nsTableView -> CDouble -> IO ()
setRowHeight nsTableView  value =
  sendMsg nsTableView (mkSelector "setRowHeight:") retVoid [argCDouble (fromIntegral value)]

-- | @- tableColumns@
tableColumns :: IsNSTableView nsTableView => nsTableView -> IO (Id NSArray)
tableColumns nsTableView  =
  sendMsg nsTableView (mkSelector "tableColumns") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- numberOfColumns@
numberOfColumns :: IsNSTableView nsTableView => nsTableView -> IO CLong
numberOfColumns nsTableView  =
  sendMsg nsTableView (mkSelector "numberOfColumns") retCLong []

-- | @- numberOfRows@
numberOfRows :: IsNSTableView nsTableView => nsTableView -> IO CLong
numberOfRows nsTableView  =
  sendMsg nsTableView (mkSelector "numberOfRows") retCLong []

-- | @- editedColumn@
editedColumn :: IsNSTableView nsTableView => nsTableView -> IO CLong
editedColumn nsTableView  =
  sendMsg nsTableView (mkSelector "editedColumn") retCLong []

-- | @- editedRow@
editedRow :: IsNSTableView nsTableView => nsTableView -> IO CLong
editedRow nsTableView  =
  sendMsg nsTableView (mkSelector "editedRow") retCLong []

-- | @- clickedColumn@
clickedColumn :: IsNSTableView nsTableView => nsTableView -> IO CLong
clickedColumn nsTableView  =
  sendMsg nsTableView (mkSelector "clickedColumn") retCLong []

-- | @- clickedRow@
clickedRow :: IsNSTableView nsTableView => nsTableView -> IO CLong
clickedRow nsTableView  =
  sendMsg nsTableView (mkSelector "clickedRow") retCLong []

-- | @- doubleAction@
doubleAction :: IsNSTableView nsTableView => nsTableView -> IO Selector
doubleAction nsTableView  =
  fmap (Selector . castPtr) $ sendMsg nsTableView (mkSelector "doubleAction") (retPtr retVoid) []

-- | @- setDoubleAction:@
setDoubleAction :: IsNSTableView nsTableView => nsTableView -> Selector -> IO ()
setDoubleAction nsTableView  value =
  sendMsg nsTableView (mkSelector "setDoubleAction:") retVoid [argPtr (unSelector value)]

-- | @- sortDescriptors@
sortDescriptors :: IsNSTableView nsTableView => nsTableView -> IO (Id NSArray)
sortDescriptors nsTableView  =
  sendMsg nsTableView (mkSelector "sortDescriptors") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- setSortDescriptors:@
setSortDescriptors :: (IsNSTableView nsTableView, IsNSArray value) => nsTableView -> value -> IO ()
setSortDescriptors nsTableView  value =
withObjCPtr value $ \raw_value ->
    sendMsg nsTableView (mkSelector "setSortDescriptors:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- | @- highlightedTableColumn@
highlightedTableColumn :: IsNSTableView nsTableView => nsTableView -> IO (Id NSTableColumn)
highlightedTableColumn nsTableView  =
  sendMsg nsTableView (mkSelector "highlightedTableColumn") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- setHighlightedTableColumn:@
setHighlightedTableColumn :: (IsNSTableView nsTableView, IsNSTableColumn value) => nsTableView -> value -> IO ()
setHighlightedTableColumn nsTableView  value =
withObjCPtr value $ \raw_value ->
    sendMsg nsTableView (mkSelector "setHighlightedTableColumn:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- | @- verticalMotionCanBeginDrag@
verticalMotionCanBeginDrag :: IsNSTableView nsTableView => nsTableView -> IO Bool
verticalMotionCanBeginDrag nsTableView  =
  fmap ((/= 0) :: CULong -> Bool) $ sendMsg nsTableView (mkSelector "verticalMotionCanBeginDrag") retCULong []

-- | @- setVerticalMotionCanBeginDrag:@
setVerticalMotionCanBeginDrag :: IsNSTableView nsTableView => nsTableView -> Bool -> IO ()
setVerticalMotionCanBeginDrag nsTableView  value =
  sendMsg nsTableView (mkSelector "setVerticalMotionCanBeginDrag:") retVoid [argCULong (if value then 1 else 0)]

-- | @- allowsMultipleSelection@
allowsMultipleSelection :: IsNSTableView nsTableView => nsTableView -> IO Bool
allowsMultipleSelection nsTableView  =
  fmap ((/= 0) :: CULong -> Bool) $ sendMsg nsTableView (mkSelector "allowsMultipleSelection") retCULong []

-- | @- setAllowsMultipleSelection:@
setAllowsMultipleSelection :: IsNSTableView nsTableView => nsTableView -> Bool -> IO ()
setAllowsMultipleSelection nsTableView  value =
  sendMsg nsTableView (mkSelector "setAllowsMultipleSelection:") retVoid [argCULong (if value then 1 else 0)]

-- | @- allowsEmptySelection@
allowsEmptySelection :: IsNSTableView nsTableView => nsTableView -> IO Bool
allowsEmptySelection nsTableView  =
  fmap ((/= 0) :: CULong -> Bool) $ sendMsg nsTableView (mkSelector "allowsEmptySelection") retCULong []

-- | @- setAllowsEmptySelection:@
setAllowsEmptySelection :: IsNSTableView nsTableView => nsTableView -> Bool -> IO ()
setAllowsEmptySelection nsTableView  value =
  sendMsg nsTableView (mkSelector "setAllowsEmptySelection:") retVoid [argCULong (if value then 1 else 0)]

-- | @- allowsColumnSelection@
allowsColumnSelection :: IsNSTableView nsTableView => nsTableView -> IO Bool
allowsColumnSelection nsTableView  =
  fmap ((/= 0) :: CULong -> Bool) $ sendMsg nsTableView (mkSelector "allowsColumnSelection") retCULong []

-- | @- setAllowsColumnSelection:@
setAllowsColumnSelection :: IsNSTableView nsTableView => nsTableView -> Bool -> IO ()
setAllowsColumnSelection nsTableView  value =
  sendMsg nsTableView (mkSelector "setAllowsColumnSelection:") retVoid [argCULong (if value then 1 else 0)]

-- | @- selectedColumnIndexes@
selectedColumnIndexes :: IsNSTableView nsTableView => nsTableView -> IO (Id NSIndexSet)
selectedColumnIndexes nsTableView  =
  sendMsg nsTableView (mkSelector "selectedColumnIndexes") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- selectedRowIndexes@
selectedRowIndexes :: IsNSTableView nsTableView => nsTableView -> IO (Id NSIndexSet)
selectedRowIndexes nsTableView  =
  sendMsg nsTableView (mkSelector "selectedRowIndexes") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- selectedColumn@
selectedColumn :: IsNSTableView nsTableView => nsTableView -> IO CLong
selectedColumn nsTableView  =
  sendMsg nsTableView (mkSelector "selectedColumn") retCLong []

-- | @- selectedRow@
selectedRow :: IsNSTableView nsTableView => nsTableView -> IO CLong
selectedRow nsTableView  =
  sendMsg nsTableView (mkSelector "selectedRow") retCLong []

-- | @- numberOfSelectedColumns@
numberOfSelectedColumns :: IsNSTableView nsTableView => nsTableView -> IO CLong
numberOfSelectedColumns nsTableView  =
  sendMsg nsTableView (mkSelector "numberOfSelectedColumns") retCLong []

-- | @- numberOfSelectedRows@
numberOfSelectedRows :: IsNSTableView nsTableView => nsTableView -> IO CLong
numberOfSelectedRows nsTableView  =
  sendMsg nsTableView (mkSelector "numberOfSelectedRows") retCLong []

-- | @- allowsTypeSelect@
allowsTypeSelect :: IsNSTableView nsTableView => nsTableView -> IO Bool
allowsTypeSelect nsTableView  =
  fmap ((/= 0) :: CULong -> Bool) $ sendMsg nsTableView (mkSelector "allowsTypeSelect") retCULong []

-- | @- setAllowsTypeSelect:@
setAllowsTypeSelect :: IsNSTableView nsTableView => nsTableView -> Bool -> IO ()
setAllowsTypeSelect nsTableView  value =
  sendMsg nsTableView (mkSelector "setAllowsTypeSelect:") retVoid [argCULong (if value then 1 else 0)]

-- | @- style@
style :: IsNSTableView nsTableView => nsTableView -> IO NSTableViewStyle
style nsTableView  =
  fmap (coerce :: CLong -> NSTableViewStyle) $ sendMsg nsTableView (mkSelector "style") retCLong []

-- | @- setStyle:@
setStyle :: IsNSTableView nsTableView => nsTableView -> NSTableViewStyle -> IO ()
setStyle nsTableView  value =
  sendMsg nsTableView (mkSelector "setStyle:") retVoid [argCLong (coerce value)]

-- | @- effectiveStyle@
effectiveStyle :: IsNSTableView nsTableView => nsTableView -> IO NSTableViewStyle
effectiveStyle nsTableView  =
  fmap (coerce :: CLong -> NSTableViewStyle) $ sendMsg nsTableView (mkSelector "effectiveStyle") retCLong []

-- | @- selectionHighlightStyle@
selectionHighlightStyle :: IsNSTableView nsTableView => nsTableView -> IO NSTableViewSelectionHighlightStyle
selectionHighlightStyle nsTableView  =
  fmap (coerce :: CLong -> NSTableViewSelectionHighlightStyle) $ sendMsg nsTableView (mkSelector "selectionHighlightStyle") retCLong []

-- | @- setSelectionHighlightStyle:@
setSelectionHighlightStyle :: IsNSTableView nsTableView => nsTableView -> NSTableViewSelectionHighlightStyle -> IO ()
setSelectionHighlightStyle nsTableView  value =
  sendMsg nsTableView (mkSelector "setSelectionHighlightStyle:") retVoid [argCLong (coerce value)]

-- | @- draggingDestinationFeedbackStyle@
draggingDestinationFeedbackStyle :: IsNSTableView nsTableView => nsTableView -> IO NSTableViewDraggingDestinationFeedbackStyle
draggingDestinationFeedbackStyle nsTableView  =
  fmap (coerce :: CLong -> NSTableViewDraggingDestinationFeedbackStyle) $ sendMsg nsTableView (mkSelector "draggingDestinationFeedbackStyle") retCLong []

-- | @- setDraggingDestinationFeedbackStyle:@
setDraggingDestinationFeedbackStyle :: IsNSTableView nsTableView => nsTableView -> NSTableViewDraggingDestinationFeedbackStyle -> IO ()
setDraggingDestinationFeedbackStyle nsTableView  value =
  sendMsg nsTableView (mkSelector "setDraggingDestinationFeedbackStyle:") retVoid [argCLong (coerce value)]

-- | @- autosaveName@
autosaveName :: IsNSTableView nsTableView => nsTableView -> IO (Id NSString)
autosaveName nsTableView  =
  sendMsg nsTableView (mkSelector "autosaveName") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- setAutosaveName:@
setAutosaveName :: (IsNSTableView nsTableView, IsNSString value) => nsTableView -> value -> IO ()
setAutosaveName nsTableView  value =
withObjCPtr value $ \raw_value ->
    sendMsg nsTableView (mkSelector "setAutosaveName:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- | @- autosaveTableColumns@
autosaveTableColumns :: IsNSTableView nsTableView => nsTableView -> IO Bool
autosaveTableColumns nsTableView  =
  fmap ((/= 0) :: CULong -> Bool) $ sendMsg nsTableView (mkSelector "autosaveTableColumns") retCULong []

-- | @- setAutosaveTableColumns:@
setAutosaveTableColumns :: IsNSTableView nsTableView => nsTableView -> Bool -> IO ()
setAutosaveTableColumns nsTableView  value =
  sendMsg nsTableView (mkSelector "setAutosaveTableColumns:") retVoid [argCULong (if value then 1 else 0)]

-- | @- floatsGroupRows@
floatsGroupRows :: IsNSTableView nsTableView => nsTableView -> IO Bool
floatsGroupRows nsTableView  =
  fmap ((/= 0) :: CULong -> Bool) $ sendMsg nsTableView (mkSelector "floatsGroupRows") retCULong []

-- | @- setFloatsGroupRows:@
setFloatsGroupRows :: IsNSTableView nsTableView => nsTableView -> Bool -> IO ()
setFloatsGroupRows nsTableView  value =
  sendMsg nsTableView (mkSelector "setFloatsGroupRows:") retVoid [argCULong (if value then 1 else 0)]

-- | @- rowActionsVisible@
rowActionsVisible :: IsNSTableView nsTableView => nsTableView -> IO Bool
rowActionsVisible nsTableView  =
  fmap ((/= 0) :: CULong -> Bool) $ sendMsg nsTableView (mkSelector "rowActionsVisible") retCULong []

-- | @- setRowActionsVisible:@
setRowActionsVisible :: IsNSTableView nsTableView => nsTableView -> Bool -> IO ()
setRowActionsVisible nsTableView  value =
  sendMsg nsTableView (mkSelector "setRowActionsVisible:") retVoid [argCULong (if value then 1 else 0)]

-- | @- usesStaticContents@
usesStaticContents :: IsNSTableView nsTableView => nsTableView -> IO Bool
usesStaticContents nsTableView  =
  fmap ((/= 0) :: CULong -> Bool) $ sendMsg nsTableView (mkSelector "usesStaticContents") retCULong []

-- | @- setUsesStaticContents:@
setUsesStaticContents :: IsNSTableView nsTableView => nsTableView -> Bool -> IO ()
setUsesStaticContents nsTableView  value =
  sendMsg nsTableView (mkSelector "setUsesStaticContents:") retVoid [argCULong (if value then 1 else 0)]

-- | @- userInterfaceLayoutDirection@
userInterfaceLayoutDirection :: IsNSTableView nsTableView => nsTableView -> IO NSUserInterfaceLayoutDirection
userInterfaceLayoutDirection nsTableView  =
  fmap (coerce :: CLong -> NSUserInterfaceLayoutDirection) $ sendMsg nsTableView (mkSelector "userInterfaceLayoutDirection") retCLong []

-- | @- setUserInterfaceLayoutDirection:@
setUserInterfaceLayoutDirection :: IsNSTableView nsTableView => nsTableView -> NSUserInterfaceLayoutDirection -> IO ()
setUserInterfaceLayoutDirection nsTableView  value =
  sendMsg nsTableView (mkSelector "setUserInterfaceLayoutDirection:") retVoid [argCLong (coerce value)]

-- | @- usesAutomaticRowHeights@
usesAutomaticRowHeights :: IsNSTableView nsTableView => nsTableView -> IO Bool
usesAutomaticRowHeights nsTableView  =
  fmap ((/= 0) :: CULong -> Bool) $ sendMsg nsTableView (mkSelector "usesAutomaticRowHeights") retCULong []

-- | @- setUsesAutomaticRowHeights:@
setUsesAutomaticRowHeights :: IsNSTableView nsTableView => nsTableView -> Bool -> IO ()
setUsesAutomaticRowHeights nsTableView  value =
  sendMsg nsTableView (mkSelector "setUsesAutomaticRowHeights:") retVoid [argCULong (if value then 1 else 0)]

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @initWithFrame:@
initWithFrameSelector :: Selector
initWithFrameSelector = mkSelector "initWithFrame:"

-- | @Selector@ for @initWithCoder:@
initWithCoderSelector :: Selector
initWithCoderSelector = mkSelector "initWithCoder:"

-- | @Selector@ for @noteHeightOfRowsWithIndexesChanged:@
noteHeightOfRowsWithIndexesChangedSelector :: Selector
noteHeightOfRowsWithIndexesChangedSelector = mkSelector "noteHeightOfRowsWithIndexesChanged:"

-- | @Selector@ for @addTableColumn:@
addTableColumnSelector :: Selector
addTableColumnSelector = mkSelector "addTableColumn:"

-- | @Selector@ for @removeTableColumn:@
removeTableColumnSelector :: Selector
removeTableColumnSelector = mkSelector "removeTableColumn:"

-- | @Selector@ for @moveColumn:toColumn:@
moveColumn_toColumnSelector :: Selector
moveColumn_toColumnSelector = mkSelector "moveColumn:toColumn:"

-- | @Selector@ for @columnWithIdentifier:@
columnWithIdentifierSelector :: Selector
columnWithIdentifierSelector = mkSelector "columnWithIdentifier:"

-- | @Selector@ for @tableColumnWithIdentifier:@
tableColumnWithIdentifierSelector :: Selector
tableColumnWithIdentifierSelector = mkSelector "tableColumnWithIdentifier:"

-- | @Selector@ for @tile@
tileSelector :: Selector
tileSelector = mkSelector "tile"

-- | @Selector@ for @sizeToFit@
sizeToFitSelector :: Selector
sizeToFitSelector = mkSelector "sizeToFit"

-- | @Selector@ for @sizeLastColumnToFit@
sizeLastColumnToFitSelector :: Selector
sizeLastColumnToFitSelector = mkSelector "sizeLastColumnToFit"

-- | @Selector@ for @scrollRowToVisible:@
scrollRowToVisibleSelector :: Selector
scrollRowToVisibleSelector = mkSelector "scrollRowToVisible:"

-- | @Selector@ for @scrollColumnToVisible:@
scrollColumnToVisibleSelector :: Selector
scrollColumnToVisibleSelector = mkSelector "scrollColumnToVisible:"

-- | @Selector@ for @reloadData@
reloadDataSelector :: Selector
reloadDataSelector = mkSelector "reloadData"

-- | @Selector@ for @noteNumberOfRowsChanged@
noteNumberOfRowsChangedSelector :: Selector
noteNumberOfRowsChangedSelector = mkSelector "noteNumberOfRowsChanged"

-- | @Selector@ for @reloadDataForRowIndexes:columnIndexes:@
reloadDataForRowIndexes_columnIndexesSelector :: Selector
reloadDataForRowIndexes_columnIndexesSelector = mkSelector "reloadDataForRowIndexes:columnIndexes:"

-- | @Selector@ for @setIndicatorImage:inTableColumn:@
setIndicatorImage_inTableColumnSelector :: Selector
setIndicatorImage_inTableColumnSelector = mkSelector "setIndicatorImage:inTableColumn:"

-- | @Selector@ for @indicatorImageInTableColumn:@
indicatorImageInTableColumnSelector :: Selector
indicatorImageInTableColumnSelector = mkSelector "indicatorImageInTableColumn:"

-- | @Selector@ for @canDragRowsWithIndexes:atPoint:@
canDragRowsWithIndexes_atPointSelector :: Selector
canDragRowsWithIndexes_atPointSelector = mkSelector "canDragRowsWithIndexes:atPoint:"

-- | @Selector@ for @dragImageForRowsWithIndexes:tableColumns:event:offset:@
dragImageForRowsWithIndexes_tableColumns_event_offsetSelector :: Selector
dragImageForRowsWithIndexes_tableColumns_event_offsetSelector = mkSelector "dragImageForRowsWithIndexes:tableColumns:event:offset:"

-- | @Selector@ for @setDraggingSourceOperationMask:forLocal:@
setDraggingSourceOperationMask_forLocalSelector :: Selector
setDraggingSourceOperationMask_forLocalSelector = mkSelector "setDraggingSourceOperationMask:forLocal:"

-- | @Selector@ for @setDropRow:dropOperation:@
setDropRow_dropOperationSelector :: Selector
setDropRow_dropOperationSelector = mkSelector "setDropRow:dropOperation:"

-- | @Selector@ for @selectAll:@
selectAllSelector :: Selector
selectAllSelector = mkSelector "selectAll:"

-- | @Selector@ for @deselectAll:@
deselectAllSelector :: Selector
deselectAllSelector = mkSelector "deselectAll:"

-- | @Selector@ for @selectColumnIndexes:byExtendingSelection:@
selectColumnIndexes_byExtendingSelectionSelector :: Selector
selectColumnIndexes_byExtendingSelectionSelector = mkSelector "selectColumnIndexes:byExtendingSelection:"

-- | @Selector@ for @selectRowIndexes:byExtendingSelection:@
selectRowIndexes_byExtendingSelectionSelector :: Selector
selectRowIndexes_byExtendingSelectionSelector = mkSelector "selectRowIndexes:byExtendingSelection:"

-- | @Selector@ for @deselectColumn:@
deselectColumnSelector :: Selector
deselectColumnSelector = mkSelector "deselectColumn:"

-- | @Selector@ for @deselectRow:@
deselectRowSelector :: Selector
deselectRowSelector = mkSelector "deselectRow:"

-- | @Selector@ for @isColumnSelected:@
isColumnSelectedSelector :: Selector
isColumnSelectedSelector = mkSelector "isColumnSelected:"

-- | @Selector@ for @isRowSelected:@
isRowSelectedSelector :: Selector
isRowSelectedSelector = mkSelector "isRowSelected:"

-- | @Selector@ for @rectOfColumn:@
rectOfColumnSelector :: Selector
rectOfColumnSelector = mkSelector "rectOfColumn:"

-- | @Selector@ for @rectOfRow:@
rectOfRowSelector :: Selector
rectOfRowSelector = mkSelector "rectOfRow:"

-- | @Selector@ for @columnIndexesInRect:@
columnIndexesInRectSelector :: Selector
columnIndexesInRectSelector = mkSelector "columnIndexesInRect:"

-- | @Selector@ for @rowsInRect:@
rowsInRectSelector :: Selector
rowsInRectSelector = mkSelector "rowsInRect:"

-- | @Selector@ for @columnAtPoint:@
columnAtPointSelector :: Selector
columnAtPointSelector = mkSelector "columnAtPoint:"

-- | @Selector@ for @rowAtPoint:@
rowAtPointSelector :: Selector
rowAtPointSelector = mkSelector "rowAtPoint:"

-- | @Selector@ for @frameOfCellAtColumn:row:@
frameOfCellAtColumn_rowSelector :: Selector
frameOfCellAtColumn_rowSelector = mkSelector "frameOfCellAtColumn:row:"

-- | @Selector@ for @editColumn:row:withEvent:select:@
editColumn_row_withEvent_selectSelector :: Selector
editColumn_row_withEvent_selectSelector = mkSelector "editColumn:row:withEvent:select:"

-- | @Selector@ for @drawRow:clipRect:@
drawRow_clipRectSelector :: Selector
drawRow_clipRectSelector = mkSelector "drawRow:clipRect:"

-- | @Selector@ for @highlightSelectionInClipRect:@
highlightSelectionInClipRectSelector :: Selector
highlightSelectionInClipRectSelector = mkSelector "highlightSelectionInClipRect:"

-- | @Selector@ for @drawGridInClipRect:@
drawGridInClipRectSelector :: Selector
drawGridInClipRectSelector = mkSelector "drawGridInClipRect:"

-- | @Selector@ for @drawBackgroundInClipRect:@
drawBackgroundInClipRectSelector :: Selector
drawBackgroundInClipRectSelector = mkSelector "drawBackgroundInClipRect:"

-- | @Selector@ for @viewAtColumn:row:makeIfNecessary:@
viewAtColumn_row_makeIfNecessarySelector :: Selector
viewAtColumn_row_makeIfNecessarySelector = mkSelector "viewAtColumn:row:makeIfNecessary:"

-- | @Selector@ for @rowViewAtRow:makeIfNecessary:@
rowViewAtRow_makeIfNecessarySelector :: Selector
rowViewAtRow_makeIfNecessarySelector = mkSelector "rowViewAtRow:makeIfNecessary:"

-- | @Selector@ for @rowForView:@
rowForViewSelector :: Selector
rowForViewSelector = mkSelector "rowForView:"

-- | @Selector@ for @columnForView:@
columnForViewSelector :: Selector
columnForViewSelector = mkSelector "columnForView:"

-- | @Selector@ for @makeViewWithIdentifier:owner:@
makeViewWithIdentifier_ownerSelector :: Selector
makeViewWithIdentifier_ownerSelector = mkSelector "makeViewWithIdentifier:owner:"

-- | @Selector@ for @enumerateAvailableRowViewsUsingBlock:@
enumerateAvailableRowViewsUsingBlockSelector :: Selector
enumerateAvailableRowViewsUsingBlockSelector = mkSelector "enumerateAvailableRowViewsUsingBlock:"

-- | @Selector@ for @beginUpdates@
beginUpdatesSelector :: Selector
beginUpdatesSelector = mkSelector "beginUpdates"

-- | @Selector@ for @endUpdates@
endUpdatesSelector :: Selector
endUpdatesSelector = mkSelector "endUpdates"

-- | @Selector@ for @insertRowsAtIndexes:withAnimation:@
insertRowsAtIndexes_withAnimationSelector :: Selector
insertRowsAtIndexes_withAnimationSelector = mkSelector "insertRowsAtIndexes:withAnimation:"

-- | @Selector@ for @removeRowsAtIndexes:withAnimation:@
removeRowsAtIndexes_withAnimationSelector :: Selector
removeRowsAtIndexes_withAnimationSelector = mkSelector "removeRowsAtIndexes:withAnimation:"

-- | @Selector@ for @moveRowAtIndex:toIndex:@
moveRowAtIndex_toIndexSelector :: Selector
moveRowAtIndex_toIndexSelector = mkSelector "moveRowAtIndex:toIndex:"

-- | @Selector@ for @hideRowsAtIndexes:withAnimation:@
hideRowsAtIndexes_withAnimationSelector :: Selector
hideRowsAtIndexes_withAnimationSelector = mkSelector "hideRowsAtIndexes:withAnimation:"

-- | @Selector@ for @unhideRowsAtIndexes:withAnimation:@
unhideRowsAtIndexes_withAnimationSelector :: Selector
unhideRowsAtIndexes_withAnimationSelector = mkSelector "unhideRowsAtIndexes:withAnimation:"

-- | @Selector@ for @registerNib:forIdentifier:@
registerNib_forIdentifierSelector :: Selector
registerNib_forIdentifierSelector = mkSelector "registerNib:forIdentifier:"

-- | @Selector@ for @didAddRowView:forRow:@
didAddRowView_forRowSelector :: Selector
didAddRowView_forRowSelector = mkSelector "didAddRowView:forRow:"

-- | @Selector@ for @didRemoveRowView:forRow:@
didRemoveRowView_forRowSelector :: Selector
didRemoveRowView_forRowSelector = mkSelector "didRemoveRowView:forRow:"

-- | @Selector@ for @setDrawsGrid:@
setDrawsGridSelector :: Selector
setDrawsGridSelector = mkSelector "setDrawsGrid:"

-- | @Selector@ for @drawsGrid@
drawsGridSelector :: Selector
drawsGridSelector = mkSelector "drawsGrid"

-- | @Selector@ for @selectColumn:byExtendingSelection:@
selectColumn_byExtendingSelectionSelector :: Selector
selectColumn_byExtendingSelectionSelector = mkSelector "selectColumn:byExtendingSelection:"

-- | @Selector@ for @selectRow:byExtendingSelection:@
selectRow_byExtendingSelectionSelector :: Selector
selectRow_byExtendingSelectionSelector = mkSelector "selectRow:byExtendingSelection:"

-- | @Selector@ for @selectedColumnEnumerator@
selectedColumnEnumeratorSelector :: Selector
selectedColumnEnumeratorSelector = mkSelector "selectedColumnEnumerator"

-- | @Selector@ for @selectedRowEnumerator@
selectedRowEnumeratorSelector :: Selector
selectedRowEnumeratorSelector = mkSelector "selectedRowEnumerator"

-- | @Selector@ for @dragImageForRows:event:dragImageOffset:@
dragImageForRows_event_dragImageOffsetSelector :: Selector
dragImageForRows_event_dragImageOffsetSelector = mkSelector "dragImageForRows:event:dragImageOffset:"

-- | @Selector@ for @setAutoresizesAllColumnsToFit:@
setAutoresizesAllColumnsToFitSelector :: Selector
setAutoresizesAllColumnsToFitSelector = mkSelector "setAutoresizesAllColumnsToFit:"

-- | @Selector@ for @autoresizesAllColumnsToFit@
autoresizesAllColumnsToFitSelector :: Selector
autoresizesAllColumnsToFitSelector = mkSelector "autoresizesAllColumnsToFit"

-- | @Selector@ for @columnsInRect:@
columnsInRectSelector :: Selector
columnsInRectSelector = mkSelector "columnsInRect:"

-- | @Selector@ for @preparedCellAtColumn:row:@
preparedCellAtColumn_rowSelector :: Selector
preparedCellAtColumn_rowSelector = mkSelector "preparedCellAtColumn:row:"

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

-- | @Selector@ for @shouldFocusCell:atColumn:row:@
shouldFocusCell_atColumn_rowSelector :: Selector
shouldFocusCell_atColumn_rowSelector = mkSelector "shouldFocusCell:atColumn:row:"

-- | @Selector@ for @focusedColumn@
focusedColumnSelector :: Selector
focusedColumnSelector = mkSelector "focusedColumn"

-- | @Selector@ for @setFocusedColumn:@
setFocusedColumnSelector :: Selector
setFocusedColumnSelector = mkSelector "setFocusedColumn:"

-- | @Selector@ for @performClickOnCellAtColumn:row:@
performClickOnCellAtColumn_rowSelector :: Selector
performClickOnCellAtColumn_rowSelector = mkSelector "performClickOnCellAtColumn:row:"

-- | @Selector@ for @headerView@
headerViewSelector :: Selector
headerViewSelector = mkSelector "headerView"

-- | @Selector@ for @setHeaderView:@
setHeaderViewSelector :: Selector
setHeaderViewSelector = mkSelector "setHeaderView:"

-- | @Selector@ for @cornerView@
cornerViewSelector :: Selector
cornerViewSelector = mkSelector "cornerView"

-- | @Selector@ for @setCornerView:@
setCornerViewSelector :: Selector
setCornerViewSelector = mkSelector "setCornerView:"

-- | @Selector@ for @allowsColumnReordering@
allowsColumnReorderingSelector :: Selector
allowsColumnReorderingSelector = mkSelector "allowsColumnReordering"

-- | @Selector@ for @setAllowsColumnReordering:@
setAllowsColumnReorderingSelector :: Selector
setAllowsColumnReorderingSelector = mkSelector "setAllowsColumnReordering:"

-- | @Selector@ for @allowsColumnResizing@
allowsColumnResizingSelector :: Selector
allowsColumnResizingSelector = mkSelector "allowsColumnResizing"

-- | @Selector@ for @setAllowsColumnResizing:@
setAllowsColumnResizingSelector :: Selector
setAllowsColumnResizingSelector = mkSelector "setAllowsColumnResizing:"

-- | @Selector@ for @columnAutoresizingStyle@
columnAutoresizingStyleSelector :: Selector
columnAutoresizingStyleSelector = mkSelector "columnAutoresizingStyle"

-- | @Selector@ for @setColumnAutoresizingStyle:@
setColumnAutoresizingStyleSelector :: Selector
setColumnAutoresizingStyleSelector = mkSelector "setColumnAutoresizingStyle:"

-- | @Selector@ for @gridStyleMask@
gridStyleMaskSelector :: Selector
gridStyleMaskSelector = mkSelector "gridStyleMask"

-- | @Selector@ for @setGridStyleMask:@
setGridStyleMaskSelector :: Selector
setGridStyleMaskSelector = mkSelector "setGridStyleMask:"

-- | @Selector@ for @intercellSpacing@
intercellSpacingSelector :: Selector
intercellSpacingSelector = mkSelector "intercellSpacing"

-- | @Selector@ for @setIntercellSpacing:@
setIntercellSpacingSelector :: Selector
setIntercellSpacingSelector = mkSelector "setIntercellSpacing:"

-- | @Selector@ for @usesAlternatingRowBackgroundColors@
usesAlternatingRowBackgroundColorsSelector :: Selector
usesAlternatingRowBackgroundColorsSelector = mkSelector "usesAlternatingRowBackgroundColors"

-- | @Selector@ for @setUsesAlternatingRowBackgroundColors:@
setUsesAlternatingRowBackgroundColorsSelector :: Selector
setUsesAlternatingRowBackgroundColorsSelector = mkSelector "setUsesAlternatingRowBackgroundColors:"

-- | @Selector@ for @backgroundColor@
backgroundColorSelector :: Selector
backgroundColorSelector = mkSelector "backgroundColor"

-- | @Selector@ for @setBackgroundColor:@
setBackgroundColorSelector :: Selector
setBackgroundColorSelector = mkSelector "setBackgroundColor:"

-- | @Selector@ for @gridColor@
gridColorSelector :: Selector
gridColorSelector = mkSelector "gridColor"

-- | @Selector@ for @setGridColor:@
setGridColorSelector :: Selector
setGridColorSelector = mkSelector "setGridColor:"

-- | @Selector@ for @rowSizeStyle@
rowSizeStyleSelector :: Selector
rowSizeStyleSelector = mkSelector "rowSizeStyle"

-- | @Selector@ for @setRowSizeStyle:@
setRowSizeStyleSelector :: Selector
setRowSizeStyleSelector = mkSelector "setRowSizeStyle:"

-- | @Selector@ for @effectiveRowSizeStyle@
effectiveRowSizeStyleSelector :: Selector
effectiveRowSizeStyleSelector = mkSelector "effectiveRowSizeStyle"

-- | @Selector@ for @rowHeight@
rowHeightSelector :: Selector
rowHeightSelector = mkSelector "rowHeight"

-- | @Selector@ for @setRowHeight:@
setRowHeightSelector :: Selector
setRowHeightSelector = mkSelector "setRowHeight:"

-- | @Selector@ for @tableColumns@
tableColumnsSelector :: Selector
tableColumnsSelector = mkSelector "tableColumns"

-- | @Selector@ for @numberOfColumns@
numberOfColumnsSelector :: Selector
numberOfColumnsSelector = mkSelector "numberOfColumns"

-- | @Selector@ for @numberOfRows@
numberOfRowsSelector :: Selector
numberOfRowsSelector = mkSelector "numberOfRows"

-- | @Selector@ for @editedColumn@
editedColumnSelector :: Selector
editedColumnSelector = mkSelector "editedColumn"

-- | @Selector@ for @editedRow@
editedRowSelector :: Selector
editedRowSelector = mkSelector "editedRow"

-- | @Selector@ for @clickedColumn@
clickedColumnSelector :: Selector
clickedColumnSelector = mkSelector "clickedColumn"

-- | @Selector@ for @clickedRow@
clickedRowSelector :: Selector
clickedRowSelector = mkSelector "clickedRow"

-- | @Selector@ for @doubleAction@
doubleActionSelector :: Selector
doubleActionSelector = mkSelector "doubleAction"

-- | @Selector@ for @setDoubleAction:@
setDoubleActionSelector :: Selector
setDoubleActionSelector = mkSelector "setDoubleAction:"

-- | @Selector@ for @sortDescriptors@
sortDescriptorsSelector :: Selector
sortDescriptorsSelector = mkSelector "sortDescriptors"

-- | @Selector@ for @setSortDescriptors:@
setSortDescriptorsSelector :: Selector
setSortDescriptorsSelector = mkSelector "setSortDescriptors:"

-- | @Selector@ for @highlightedTableColumn@
highlightedTableColumnSelector :: Selector
highlightedTableColumnSelector = mkSelector "highlightedTableColumn"

-- | @Selector@ for @setHighlightedTableColumn:@
setHighlightedTableColumnSelector :: Selector
setHighlightedTableColumnSelector = mkSelector "setHighlightedTableColumn:"

-- | @Selector@ for @verticalMotionCanBeginDrag@
verticalMotionCanBeginDragSelector :: Selector
verticalMotionCanBeginDragSelector = mkSelector "verticalMotionCanBeginDrag"

-- | @Selector@ for @setVerticalMotionCanBeginDrag:@
setVerticalMotionCanBeginDragSelector :: Selector
setVerticalMotionCanBeginDragSelector = mkSelector "setVerticalMotionCanBeginDrag:"

-- | @Selector@ for @allowsMultipleSelection@
allowsMultipleSelectionSelector :: Selector
allowsMultipleSelectionSelector = mkSelector "allowsMultipleSelection"

-- | @Selector@ for @setAllowsMultipleSelection:@
setAllowsMultipleSelectionSelector :: Selector
setAllowsMultipleSelectionSelector = mkSelector "setAllowsMultipleSelection:"

-- | @Selector@ for @allowsEmptySelection@
allowsEmptySelectionSelector :: Selector
allowsEmptySelectionSelector = mkSelector "allowsEmptySelection"

-- | @Selector@ for @setAllowsEmptySelection:@
setAllowsEmptySelectionSelector :: Selector
setAllowsEmptySelectionSelector = mkSelector "setAllowsEmptySelection:"

-- | @Selector@ for @allowsColumnSelection@
allowsColumnSelectionSelector :: Selector
allowsColumnSelectionSelector = mkSelector "allowsColumnSelection"

-- | @Selector@ for @setAllowsColumnSelection:@
setAllowsColumnSelectionSelector :: Selector
setAllowsColumnSelectionSelector = mkSelector "setAllowsColumnSelection:"

-- | @Selector@ for @selectedColumnIndexes@
selectedColumnIndexesSelector :: Selector
selectedColumnIndexesSelector = mkSelector "selectedColumnIndexes"

-- | @Selector@ for @selectedRowIndexes@
selectedRowIndexesSelector :: Selector
selectedRowIndexesSelector = mkSelector "selectedRowIndexes"

-- | @Selector@ for @selectedColumn@
selectedColumnSelector :: Selector
selectedColumnSelector = mkSelector "selectedColumn"

-- | @Selector@ for @selectedRow@
selectedRowSelector :: Selector
selectedRowSelector = mkSelector "selectedRow"

-- | @Selector@ for @numberOfSelectedColumns@
numberOfSelectedColumnsSelector :: Selector
numberOfSelectedColumnsSelector = mkSelector "numberOfSelectedColumns"

-- | @Selector@ for @numberOfSelectedRows@
numberOfSelectedRowsSelector :: Selector
numberOfSelectedRowsSelector = mkSelector "numberOfSelectedRows"

-- | @Selector@ for @allowsTypeSelect@
allowsTypeSelectSelector :: Selector
allowsTypeSelectSelector = mkSelector "allowsTypeSelect"

-- | @Selector@ for @setAllowsTypeSelect:@
setAllowsTypeSelectSelector :: Selector
setAllowsTypeSelectSelector = mkSelector "setAllowsTypeSelect:"

-- | @Selector@ for @style@
styleSelector :: Selector
styleSelector = mkSelector "style"

-- | @Selector@ for @setStyle:@
setStyleSelector :: Selector
setStyleSelector = mkSelector "setStyle:"

-- | @Selector@ for @effectiveStyle@
effectiveStyleSelector :: Selector
effectiveStyleSelector = mkSelector "effectiveStyle"

-- | @Selector@ for @selectionHighlightStyle@
selectionHighlightStyleSelector :: Selector
selectionHighlightStyleSelector = mkSelector "selectionHighlightStyle"

-- | @Selector@ for @setSelectionHighlightStyle:@
setSelectionHighlightStyleSelector :: Selector
setSelectionHighlightStyleSelector = mkSelector "setSelectionHighlightStyle:"

-- | @Selector@ for @draggingDestinationFeedbackStyle@
draggingDestinationFeedbackStyleSelector :: Selector
draggingDestinationFeedbackStyleSelector = mkSelector "draggingDestinationFeedbackStyle"

-- | @Selector@ for @setDraggingDestinationFeedbackStyle:@
setDraggingDestinationFeedbackStyleSelector :: Selector
setDraggingDestinationFeedbackStyleSelector = mkSelector "setDraggingDestinationFeedbackStyle:"

-- | @Selector@ for @autosaveName@
autosaveNameSelector :: Selector
autosaveNameSelector = mkSelector "autosaveName"

-- | @Selector@ for @setAutosaveName:@
setAutosaveNameSelector :: Selector
setAutosaveNameSelector = mkSelector "setAutosaveName:"

-- | @Selector@ for @autosaveTableColumns@
autosaveTableColumnsSelector :: Selector
autosaveTableColumnsSelector = mkSelector "autosaveTableColumns"

-- | @Selector@ for @setAutosaveTableColumns:@
setAutosaveTableColumnsSelector :: Selector
setAutosaveTableColumnsSelector = mkSelector "setAutosaveTableColumns:"

-- | @Selector@ for @floatsGroupRows@
floatsGroupRowsSelector :: Selector
floatsGroupRowsSelector = mkSelector "floatsGroupRows"

-- | @Selector@ for @setFloatsGroupRows:@
setFloatsGroupRowsSelector :: Selector
setFloatsGroupRowsSelector = mkSelector "setFloatsGroupRows:"

-- | @Selector@ for @rowActionsVisible@
rowActionsVisibleSelector :: Selector
rowActionsVisibleSelector = mkSelector "rowActionsVisible"

-- | @Selector@ for @setRowActionsVisible:@
setRowActionsVisibleSelector :: Selector
setRowActionsVisibleSelector = mkSelector "setRowActionsVisible:"

-- | @Selector@ for @usesStaticContents@
usesStaticContentsSelector :: Selector
usesStaticContentsSelector = mkSelector "usesStaticContents"

-- | @Selector@ for @setUsesStaticContents:@
setUsesStaticContentsSelector :: Selector
setUsesStaticContentsSelector = mkSelector "setUsesStaticContents:"

-- | @Selector@ for @userInterfaceLayoutDirection@
userInterfaceLayoutDirectionSelector :: Selector
userInterfaceLayoutDirectionSelector = mkSelector "userInterfaceLayoutDirection"

-- | @Selector@ for @setUserInterfaceLayoutDirection:@
setUserInterfaceLayoutDirectionSelector :: Selector
setUserInterfaceLayoutDirectionSelector = mkSelector "setUserInterfaceLayoutDirection:"

-- | @Selector@ for @usesAutomaticRowHeights@
usesAutomaticRowHeightsSelector :: Selector
usesAutomaticRowHeightsSelector = mkSelector "usesAutomaticRowHeights"

-- | @Selector@ for @setUsesAutomaticRowHeights:@
setUsesAutomaticRowHeightsSelector :: Selector
setUsesAutomaticRowHeightsSelector = mkSelector "setUsesAutomaticRowHeights:"

