{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE DataKinds #-}
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
  , dataSource
  , setDataSource
  , delegate
  , setDelegate
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
  , hiddenRowIndexes
  , registeredNibsByIdentifier
  , usesStaticContents
  , setUsesStaticContents
  , userInterfaceLayoutDirection
  , setUserInterfaceLayoutDirection
  , usesAutomaticRowHeights
  , setUsesAutomaticRowHeights
  , addTableColumnSelector
  , allowsColumnReorderingSelector
  , allowsColumnResizingSelector
  , allowsColumnSelectionSelector
  , allowsEmptySelectionSelector
  , allowsMultipleSelectionSelector
  , allowsTypeSelectSelector
  , autoresizesAllColumnsToFitSelector
  , autosaveNameSelector
  , autosaveTableColumnsSelector
  , backgroundColorSelector
  , beginUpdatesSelector
  , canDragRowsWithIndexes_atPointSelector
  , clickedColumnSelector
  , clickedRowSelector
  , columnAtPointSelector
  , columnAutoresizingStyleSelector
  , columnForViewSelector
  , columnIndexesInRectSelector
  , columnWithIdentifierSelector
  , columnsInRectSelector
  , cornerViewSelector
  , dataSourceSelector
  , delegateSelector
  , deselectAllSelector
  , deselectColumnSelector
  , deselectRowSelector
  , didAddRowView_forRowSelector
  , didRemoveRowView_forRowSelector
  , doubleActionSelector
  , dragImageForRowsWithIndexes_tableColumns_event_offsetSelector
  , dragImageForRows_event_dragImageOffsetSelector
  , draggingDestinationFeedbackStyleSelector
  , drawBackgroundInClipRectSelector
  , drawGridInClipRectSelector
  , drawRow_clipRectSelector
  , drawsGridSelector
  , editColumn_row_withEvent_selectSelector
  , editedColumnSelector
  , editedRowSelector
  , effectiveRowSizeStyleSelector
  , effectiveStyleSelector
  , endUpdatesSelector
  , enumerateAvailableRowViewsUsingBlockSelector
  , floatsGroupRowsSelector
  , focusedColumnSelector
  , frameOfCellAtColumn_rowSelector
  , gridColorSelector
  , gridStyleMaskSelector
  , headerViewSelector
  , hiddenRowIndexesSelector
  , hideRowsAtIndexes_withAnimationSelector
  , highlightSelectionInClipRectSelector
  , highlightedTableColumnSelector
  , indicatorImageInTableColumnSelector
  , initWithCoderSelector
  , initWithFrameSelector
  , insertRowsAtIndexes_withAnimationSelector
  , intercellSpacingSelector
  , isColumnSelectedSelector
  , isRowSelectedSelector
  , makeViewWithIdentifier_ownerSelector
  , moveColumn_toColumnSelector
  , moveRowAtIndex_toIndexSelector
  , noteHeightOfRowsWithIndexesChangedSelector
  , noteNumberOfRowsChangedSelector
  , numberOfColumnsSelector
  , numberOfRowsSelector
  , numberOfSelectedColumnsSelector
  , numberOfSelectedRowsSelector
  , performClickOnCellAtColumn_rowSelector
  , preparedCellAtColumn_rowSelector
  , rectOfColumnSelector
  , rectOfRowSelector
  , registerNib_forIdentifierSelector
  , registeredNibsByIdentifierSelector
  , reloadDataForRowIndexes_columnIndexesSelector
  , reloadDataSelector
  , removeRowsAtIndexes_withAnimationSelector
  , removeTableColumnSelector
  , rowActionsVisibleSelector
  , rowAtPointSelector
  , rowForViewSelector
  , rowHeightSelector
  , rowSizeStyleSelector
  , rowViewAtRow_makeIfNecessarySelector
  , rowsInRectSelector
  , scrollColumnToVisibleSelector
  , scrollRowToVisibleSelector
  , selectAllSelector
  , selectColumnIndexes_byExtendingSelectionSelector
  , selectColumn_byExtendingSelectionSelector
  , selectRowIndexes_byExtendingSelectionSelector
  , selectRow_byExtendingSelectionSelector
  , selectedColumnEnumeratorSelector
  , selectedColumnIndexesSelector
  , selectedColumnSelector
  , selectedRowEnumeratorSelector
  , selectedRowIndexesSelector
  , selectedRowSelector
  , selectionHighlightStyleSelector
  , setAllowsColumnReorderingSelector
  , setAllowsColumnResizingSelector
  , setAllowsColumnSelectionSelector
  , setAllowsEmptySelectionSelector
  , setAllowsMultipleSelectionSelector
  , setAllowsTypeSelectSelector
  , setAutoresizesAllColumnsToFitSelector
  , setAutosaveNameSelector
  , setAutosaveTableColumnsSelector
  , setBackgroundColorSelector
  , setColumnAutoresizingStyleSelector
  , setCornerViewSelector
  , setDataSourceSelector
  , setDelegateSelector
  , setDoubleActionSelector
  , setDraggingDestinationFeedbackStyleSelector
  , setDraggingSourceOperationMask_forLocalSelector
  , setDrawsGridSelector
  , setDropRow_dropOperationSelector
  , setFloatsGroupRowsSelector
  , setFocusedColumnSelector
  , setGridColorSelector
  , setGridStyleMaskSelector
  , setHeaderViewSelector
  , setHighlightedTableColumnSelector
  , setIndicatorImage_inTableColumnSelector
  , setIntercellSpacingSelector
  , setRowActionsVisibleSelector
  , setRowHeightSelector
  , setRowSizeStyleSelector
  , setSelectionHighlightStyleSelector
  , setSortDescriptorsSelector
  , setStyleSelector
  , setUserInterfaceLayoutDirectionSelector
  , setUsesAlternatingRowBackgroundColorsSelector
  , setUsesAutomaticRowHeightsSelector
  , setUsesStaticContentsSelector
  , setVerticalMotionCanBeginDragSelector
  , shouldFocusCell_atColumn_rowSelector
  , sizeLastColumnToFitSelector
  , sizeToFitSelector
  , sortDescriptorsSelector
  , styleSelector
  , tableColumnWithIdentifierSelector
  , tableColumnsSelector
  , textDidBeginEditingSelector
  , textDidChangeSelector
  , textDidEndEditingSelector
  , textShouldBeginEditingSelector
  , textShouldEndEditingSelector
  , tileSelector
  , unhideRowsAtIndexes_withAnimationSelector
  , userInterfaceLayoutDirectionSelector
  , usesAlternatingRowBackgroundColorsSelector
  , usesAutomaticRowHeightsSelector
  , usesStaticContentsSelector
  , verticalMotionCanBeginDragSelector
  , viewAtColumn_row_makeIfNecessarySelector

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
initWithFrame :: IsNSTableView nsTableView => nsTableView -> NSRect -> IO (Id NSTableView)
initWithFrame nsTableView frameRect =
  sendOwnedMessage nsTableView initWithFrameSelector frameRect

-- | @- initWithCoder:@
initWithCoder :: (IsNSTableView nsTableView, IsNSCoder coder) => nsTableView -> coder -> IO (Id NSTableView)
initWithCoder nsTableView coder =
  sendOwnedMessage nsTableView initWithCoderSelector (toNSCoder coder)

-- | @- noteHeightOfRowsWithIndexesChanged:@
noteHeightOfRowsWithIndexesChanged :: (IsNSTableView nsTableView, IsNSIndexSet indexSet) => nsTableView -> indexSet -> IO ()
noteHeightOfRowsWithIndexesChanged nsTableView indexSet =
  sendMessage nsTableView noteHeightOfRowsWithIndexesChangedSelector (toNSIndexSet indexSet)

-- | @- addTableColumn:@
addTableColumn :: (IsNSTableView nsTableView, IsNSTableColumn tableColumn) => nsTableView -> tableColumn -> IO ()
addTableColumn nsTableView tableColumn =
  sendMessage nsTableView addTableColumnSelector (toNSTableColumn tableColumn)

-- | @- removeTableColumn:@
removeTableColumn :: (IsNSTableView nsTableView, IsNSTableColumn tableColumn) => nsTableView -> tableColumn -> IO ()
removeTableColumn nsTableView tableColumn =
  sendMessage nsTableView removeTableColumnSelector (toNSTableColumn tableColumn)

-- | @- moveColumn:toColumn:@
moveColumn_toColumn :: IsNSTableView nsTableView => nsTableView -> CLong -> CLong -> IO ()
moveColumn_toColumn nsTableView oldIndex newIndex =
  sendMessage nsTableView moveColumn_toColumnSelector oldIndex newIndex

-- | @- columnWithIdentifier:@
columnWithIdentifier :: (IsNSTableView nsTableView, IsNSString identifier) => nsTableView -> identifier -> IO CLong
columnWithIdentifier nsTableView identifier =
  sendMessage nsTableView columnWithIdentifierSelector (toNSString identifier)

-- | @- tableColumnWithIdentifier:@
tableColumnWithIdentifier :: (IsNSTableView nsTableView, IsNSString identifier) => nsTableView -> identifier -> IO (Id NSTableColumn)
tableColumnWithIdentifier nsTableView identifier =
  sendMessage nsTableView tableColumnWithIdentifierSelector (toNSString identifier)

-- | @- tile@
tile :: IsNSTableView nsTableView => nsTableView -> IO ()
tile nsTableView =
  sendMessage nsTableView tileSelector

-- | @- sizeToFit@
sizeToFit :: IsNSTableView nsTableView => nsTableView -> IO ()
sizeToFit nsTableView =
  sendMessage nsTableView sizeToFitSelector

-- | @- sizeLastColumnToFit@
sizeLastColumnToFit :: IsNSTableView nsTableView => nsTableView -> IO ()
sizeLastColumnToFit nsTableView =
  sendMessage nsTableView sizeLastColumnToFitSelector

-- | @- scrollRowToVisible:@
scrollRowToVisible :: IsNSTableView nsTableView => nsTableView -> CLong -> IO ()
scrollRowToVisible nsTableView row =
  sendMessage nsTableView scrollRowToVisibleSelector row

-- | @- scrollColumnToVisible:@
scrollColumnToVisible :: IsNSTableView nsTableView => nsTableView -> CLong -> IO ()
scrollColumnToVisible nsTableView column =
  sendMessage nsTableView scrollColumnToVisibleSelector column

-- | @- reloadData@
reloadData :: IsNSTableView nsTableView => nsTableView -> IO ()
reloadData nsTableView =
  sendMessage nsTableView reloadDataSelector

-- | @- noteNumberOfRowsChanged@
noteNumberOfRowsChanged :: IsNSTableView nsTableView => nsTableView -> IO ()
noteNumberOfRowsChanged nsTableView =
  sendMessage nsTableView noteNumberOfRowsChangedSelector

-- | @- reloadDataForRowIndexes:columnIndexes:@
reloadDataForRowIndexes_columnIndexes :: (IsNSTableView nsTableView, IsNSIndexSet rowIndexes, IsNSIndexSet columnIndexes) => nsTableView -> rowIndexes -> columnIndexes -> IO ()
reloadDataForRowIndexes_columnIndexes nsTableView rowIndexes columnIndexes =
  sendMessage nsTableView reloadDataForRowIndexes_columnIndexesSelector (toNSIndexSet rowIndexes) (toNSIndexSet columnIndexes)

-- | @- setIndicatorImage:inTableColumn:@
setIndicatorImage_inTableColumn :: (IsNSTableView nsTableView, IsNSImage image, IsNSTableColumn tableColumn) => nsTableView -> image -> tableColumn -> IO ()
setIndicatorImage_inTableColumn nsTableView image tableColumn =
  sendMessage nsTableView setIndicatorImage_inTableColumnSelector (toNSImage image) (toNSTableColumn tableColumn)

-- | @- indicatorImageInTableColumn:@
indicatorImageInTableColumn :: (IsNSTableView nsTableView, IsNSTableColumn tableColumn) => nsTableView -> tableColumn -> IO (Id NSImage)
indicatorImageInTableColumn nsTableView tableColumn =
  sendMessage nsTableView indicatorImageInTableColumnSelector (toNSTableColumn tableColumn)

-- | @- canDragRowsWithIndexes:atPoint:@
canDragRowsWithIndexes_atPoint :: (IsNSTableView nsTableView, IsNSIndexSet rowIndexes) => nsTableView -> rowIndexes -> NSPoint -> IO Bool
canDragRowsWithIndexes_atPoint nsTableView rowIndexes mouseDownPoint =
  sendMessage nsTableView canDragRowsWithIndexes_atPointSelector (toNSIndexSet rowIndexes) mouseDownPoint

-- | @- dragImageForRowsWithIndexes:tableColumns:event:offset:@
dragImageForRowsWithIndexes_tableColumns_event_offset :: (IsNSTableView nsTableView, IsNSIndexSet dragRows, IsNSArray tableColumns, IsNSEvent dragEvent) => nsTableView -> dragRows -> tableColumns -> dragEvent -> Ptr NSPoint -> IO (Id NSImage)
dragImageForRowsWithIndexes_tableColumns_event_offset nsTableView dragRows tableColumns dragEvent dragImageOffset =
  sendMessage nsTableView dragImageForRowsWithIndexes_tableColumns_event_offsetSelector (toNSIndexSet dragRows) (toNSArray tableColumns) (toNSEvent dragEvent) dragImageOffset

-- | @- setDraggingSourceOperationMask:forLocal:@
setDraggingSourceOperationMask_forLocal :: IsNSTableView nsTableView => nsTableView -> NSDragOperation -> Bool -> IO ()
setDraggingSourceOperationMask_forLocal nsTableView mask isLocal =
  sendMessage nsTableView setDraggingSourceOperationMask_forLocalSelector mask isLocal

-- | @- setDropRow:dropOperation:@
setDropRow_dropOperation :: IsNSTableView nsTableView => nsTableView -> CLong -> NSTableViewDropOperation -> IO ()
setDropRow_dropOperation nsTableView row dropOperation =
  sendMessage nsTableView setDropRow_dropOperationSelector row dropOperation

-- | @- selectAll:@
selectAll :: IsNSTableView nsTableView => nsTableView -> RawId -> IO ()
selectAll nsTableView sender =
  sendMessage nsTableView selectAllSelector sender

-- | @- deselectAll:@
deselectAll :: IsNSTableView nsTableView => nsTableView -> RawId -> IO ()
deselectAll nsTableView sender =
  sendMessage nsTableView deselectAllSelector sender

-- | @- selectColumnIndexes:byExtendingSelection:@
selectColumnIndexes_byExtendingSelection :: (IsNSTableView nsTableView, IsNSIndexSet indexes) => nsTableView -> indexes -> Bool -> IO ()
selectColumnIndexes_byExtendingSelection nsTableView indexes extend =
  sendMessage nsTableView selectColumnIndexes_byExtendingSelectionSelector (toNSIndexSet indexes) extend

-- | @- selectRowIndexes:byExtendingSelection:@
selectRowIndexes_byExtendingSelection :: (IsNSTableView nsTableView, IsNSIndexSet indexes) => nsTableView -> indexes -> Bool -> IO ()
selectRowIndexes_byExtendingSelection nsTableView indexes extend =
  sendMessage nsTableView selectRowIndexes_byExtendingSelectionSelector (toNSIndexSet indexes) extend

-- | @- deselectColumn:@
deselectColumn :: IsNSTableView nsTableView => nsTableView -> CLong -> IO ()
deselectColumn nsTableView column =
  sendMessage nsTableView deselectColumnSelector column

-- | @- deselectRow:@
deselectRow :: IsNSTableView nsTableView => nsTableView -> CLong -> IO ()
deselectRow nsTableView row =
  sendMessage nsTableView deselectRowSelector row

-- | @- isColumnSelected:@
isColumnSelected :: IsNSTableView nsTableView => nsTableView -> CLong -> IO Bool
isColumnSelected nsTableView column =
  sendMessage nsTableView isColumnSelectedSelector column

-- | @- isRowSelected:@
isRowSelected :: IsNSTableView nsTableView => nsTableView -> CLong -> IO Bool
isRowSelected nsTableView row =
  sendMessage nsTableView isRowSelectedSelector row

-- | @- rectOfColumn:@
rectOfColumn :: IsNSTableView nsTableView => nsTableView -> CLong -> IO NSRect
rectOfColumn nsTableView column =
  sendMessage nsTableView rectOfColumnSelector column

-- | @- rectOfRow:@
rectOfRow :: IsNSTableView nsTableView => nsTableView -> CLong -> IO NSRect
rectOfRow nsTableView row =
  sendMessage nsTableView rectOfRowSelector row

-- | @- columnIndexesInRect:@
columnIndexesInRect :: IsNSTableView nsTableView => nsTableView -> NSRect -> IO (Id NSIndexSet)
columnIndexesInRect nsTableView rect =
  sendMessage nsTableView columnIndexesInRectSelector rect

-- | @- rowsInRect:@
rowsInRect :: IsNSTableView nsTableView => nsTableView -> NSRect -> IO NSRange
rowsInRect nsTableView rect =
  sendMessage nsTableView rowsInRectSelector rect

-- | @- columnAtPoint:@
columnAtPoint :: IsNSTableView nsTableView => nsTableView -> NSPoint -> IO CLong
columnAtPoint nsTableView point =
  sendMessage nsTableView columnAtPointSelector point

-- | @- rowAtPoint:@
rowAtPoint :: IsNSTableView nsTableView => nsTableView -> NSPoint -> IO CLong
rowAtPoint nsTableView point =
  sendMessage nsTableView rowAtPointSelector point

-- | @- frameOfCellAtColumn:row:@
frameOfCellAtColumn_row :: IsNSTableView nsTableView => nsTableView -> CLong -> CLong -> IO NSRect
frameOfCellAtColumn_row nsTableView column row =
  sendMessage nsTableView frameOfCellAtColumn_rowSelector column row

-- | @- editColumn:row:withEvent:select:@
editColumn_row_withEvent_select :: (IsNSTableView nsTableView, IsNSEvent event) => nsTableView -> CLong -> CLong -> event -> Bool -> IO ()
editColumn_row_withEvent_select nsTableView column row event select =
  sendMessage nsTableView editColumn_row_withEvent_selectSelector column row (toNSEvent event) select

-- | @- drawRow:clipRect:@
drawRow_clipRect :: IsNSTableView nsTableView => nsTableView -> CLong -> NSRect -> IO ()
drawRow_clipRect nsTableView row clipRect =
  sendMessage nsTableView drawRow_clipRectSelector row clipRect

-- | @- highlightSelectionInClipRect:@
highlightSelectionInClipRect :: IsNSTableView nsTableView => nsTableView -> NSRect -> IO ()
highlightSelectionInClipRect nsTableView clipRect =
  sendMessage nsTableView highlightSelectionInClipRectSelector clipRect

-- | @- drawGridInClipRect:@
drawGridInClipRect :: IsNSTableView nsTableView => nsTableView -> NSRect -> IO ()
drawGridInClipRect nsTableView clipRect =
  sendMessage nsTableView drawGridInClipRectSelector clipRect

-- | @- drawBackgroundInClipRect:@
drawBackgroundInClipRect :: IsNSTableView nsTableView => nsTableView -> NSRect -> IO ()
drawBackgroundInClipRect nsTableView clipRect =
  sendMessage nsTableView drawBackgroundInClipRectSelector clipRect

-- | @- viewAtColumn:row:makeIfNecessary:@
viewAtColumn_row_makeIfNecessary :: IsNSTableView nsTableView => nsTableView -> CLong -> CLong -> Bool -> IO (Id NSView)
viewAtColumn_row_makeIfNecessary nsTableView column row makeIfNecessary =
  sendMessage nsTableView viewAtColumn_row_makeIfNecessarySelector column row makeIfNecessary

-- | @- rowViewAtRow:makeIfNecessary:@
rowViewAtRow_makeIfNecessary :: IsNSTableView nsTableView => nsTableView -> CLong -> Bool -> IO (Id NSTableRowView)
rowViewAtRow_makeIfNecessary nsTableView row makeIfNecessary =
  sendMessage nsTableView rowViewAtRow_makeIfNecessarySelector row makeIfNecessary

-- | @- rowForView:@
rowForView :: (IsNSTableView nsTableView, IsNSView view) => nsTableView -> view -> IO CLong
rowForView nsTableView view =
  sendMessage nsTableView rowForViewSelector (toNSView view)

-- | @- columnForView:@
columnForView :: (IsNSTableView nsTableView, IsNSView view) => nsTableView -> view -> IO CLong
columnForView nsTableView view =
  sendMessage nsTableView columnForViewSelector (toNSView view)

-- | @- makeViewWithIdentifier:owner:@
makeViewWithIdentifier_owner :: (IsNSTableView nsTableView, IsNSString identifier) => nsTableView -> identifier -> RawId -> IO (Id NSView)
makeViewWithIdentifier_owner nsTableView identifier owner =
  sendMessage nsTableView makeViewWithIdentifier_ownerSelector (toNSString identifier) owner

-- | @- enumerateAvailableRowViewsUsingBlock:@
enumerateAvailableRowViewsUsingBlock :: IsNSTableView nsTableView => nsTableView -> Ptr () -> IO ()
enumerateAvailableRowViewsUsingBlock nsTableView handler =
  sendMessage nsTableView enumerateAvailableRowViewsUsingBlockSelector handler

-- | @- beginUpdates@
beginUpdates :: IsNSTableView nsTableView => nsTableView -> IO ()
beginUpdates nsTableView =
  sendMessage nsTableView beginUpdatesSelector

-- | @- endUpdates@
endUpdates :: IsNSTableView nsTableView => nsTableView -> IO ()
endUpdates nsTableView =
  sendMessage nsTableView endUpdatesSelector

-- | @- insertRowsAtIndexes:withAnimation:@
insertRowsAtIndexes_withAnimation :: (IsNSTableView nsTableView, IsNSIndexSet indexes) => nsTableView -> indexes -> NSTableViewAnimationOptions -> IO ()
insertRowsAtIndexes_withAnimation nsTableView indexes animationOptions =
  sendMessage nsTableView insertRowsAtIndexes_withAnimationSelector (toNSIndexSet indexes) animationOptions

-- | @- removeRowsAtIndexes:withAnimation:@
removeRowsAtIndexes_withAnimation :: (IsNSTableView nsTableView, IsNSIndexSet indexes) => nsTableView -> indexes -> NSTableViewAnimationOptions -> IO ()
removeRowsAtIndexes_withAnimation nsTableView indexes animationOptions =
  sendMessage nsTableView removeRowsAtIndexes_withAnimationSelector (toNSIndexSet indexes) animationOptions

-- | @- moveRowAtIndex:toIndex:@
moveRowAtIndex_toIndex :: IsNSTableView nsTableView => nsTableView -> CLong -> CLong -> IO ()
moveRowAtIndex_toIndex nsTableView oldIndex newIndex =
  sendMessage nsTableView moveRowAtIndex_toIndexSelector oldIndex newIndex

-- | @- hideRowsAtIndexes:withAnimation:@
hideRowsAtIndexes_withAnimation :: (IsNSTableView nsTableView, IsNSIndexSet indexes) => nsTableView -> indexes -> NSTableViewAnimationOptions -> IO ()
hideRowsAtIndexes_withAnimation nsTableView indexes rowAnimation =
  sendMessage nsTableView hideRowsAtIndexes_withAnimationSelector (toNSIndexSet indexes) rowAnimation

-- | @- unhideRowsAtIndexes:withAnimation:@
unhideRowsAtIndexes_withAnimation :: (IsNSTableView nsTableView, IsNSIndexSet indexes) => nsTableView -> indexes -> NSTableViewAnimationOptions -> IO ()
unhideRowsAtIndexes_withAnimation nsTableView indexes rowAnimation =
  sendMessage nsTableView unhideRowsAtIndexes_withAnimationSelector (toNSIndexSet indexes) rowAnimation

-- | @- registerNib:forIdentifier:@
registerNib_forIdentifier :: (IsNSTableView nsTableView, IsNSNib nib, IsNSString identifier) => nsTableView -> nib -> identifier -> IO ()
registerNib_forIdentifier nsTableView nib identifier =
  sendMessage nsTableView registerNib_forIdentifierSelector (toNSNib nib) (toNSString identifier)

-- | @- didAddRowView:forRow:@
didAddRowView_forRow :: (IsNSTableView nsTableView, IsNSTableRowView rowView) => nsTableView -> rowView -> CLong -> IO ()
didAddRowView_forRow nsTableView rowView row =
  sendMessage nsTableView didAddRowView_forRowSelector (toNSTableRowView rowView) row

-- | @- didRemoveRowView:forRow:@
didRemoveRowView_forRow :: (IsNSTableView nsTableView, IsNSTableRowView rowView) => nsTableView -> rowView -> CLong -> IO ()
didRemoveRowView_forRow nsTableView rowView row =
  sendMessage nsTableView didRemoveRowView_forRowSelector (toNSTableRowView rowView) row

-- | @- setDrawsGrid:@
setDrawsGrid :: IsNSTableView nsTableView => nsTableView -> Bool -> IO ()
setDrawsGrid nsTableView flag =
  sendMessage nsTableView setDrawsGridSelector flag

-- | @- drawsGrid@
drawsGrid :: IsNSTableView nsTableView => nsTableView -> IO Bool
drawsGrid nsTableView =
  sendMessage nsTableView drawsGridSelector

-- | @- selectColumn:byExtendingSelection:@
selectColumn_byExtendingSelection :: IsNSTableView nsTableView => nsTableView -> CLong -> Bool -> IO ()
selectColumn_byExtendingSelection nsTableView column extend =
  sendMessage nsTableView selectColumn_byExtendingSelectionSelector column extend

-- | @- selectRow:byExtendingSelection:@
selectRow_byExtendingSelection :: IsNSTableView nsTableView => nsTableView -> CLong -> Bool -> IO ()
selectRow_byExtendingSelection nsTableView row extend =
  sendMessage nsTableView selectRow_byExtendingSelectionSelector row extend

-- | @- selectedColumnEnumerator@
selectedColumnEnumerator :: IsNSTableView nsTableView => nsTableView -> IO (Id NSEnumerator)
selectedColumnEnumerator nsTableView =
  sendMessage nsTableView selectedColumnEnumeratorSelector

-- | @- selectedRowEnumerator@
selectedRowEnumerator :: IsNSTableView nsTableView => nsTableView -> IO (Id NSEnumerator)
selectedRowEnumerator nsTableView =
  sendMessage nsTableView selectedRowEnumeratorSelector

-- | @- dragImageForRows:event:dragImageOffset:@
dragImageForRows_event_dragImageOffset :: (IsNSTableView nsTableView, IsNSArray dragRows, IsNSEvent dragEvent) => nsTableView -> dragRows -> dragEvent -> Ptr NSPoint -> IO (Id NSImage)
dragImageForRows_event_dragImageOffset nsTableView dragRows dragEvent dragImageOffset =
  sendMessage nsTableView dragImageForRows_event_dragImageOffsetSelector (toNSArray dragRows) (toNSEvent dragEvent) dragImageOffset

-- | @- setAutoresizesAllColumnsToFit:@
setAutoresizesAllColumnsToFit :: IsNSTableView nsTableView => nsTableView -> Bool -> IO ()
setAutoresizesAllColumnsToFit nsTableView flag =
  sendMessage nsTableView setAutoresizesAllColumnsToFitSelector flag

-- | @- autoresizesAllColumnsToFit@
autoresizesAllColumnsToFit :: IsNSTableView nsTableView => nsTableView -> IO Bool
autoresizesAllColumnsToFit nsTableView =
  sendMessage nsTableView autoresizesAllColumnsToFitSelector

-- | @- columnsInRect:@
columnsInRect :: IsNSTableView nsTableView => nsTableView -> NSRect -> IO NSRange
columnsInRect nsTableView rect =
  sendMessage nsTableView columnsInRectSelector rect

-- | @- preparedCellAtColumn:row:@
preparedCellAtColumn_row :: IsNSTableView nsTableView => nsTableView -> CLong -> CLong -> IO (Id NSCell)
preparedCellAtColumn_row nsTableView column row =
  sendMessage nsTableView preparedCellAtColumn_rowSelector column row

-- | @- textShouldBeginEditing:@
textShouldBeginEditing :: (IsNSTableView nsTableView, IsNSText textObject) => nsTableView -> textObject -> IO Bool
textShouldBeginEditing nsTableView textObject =
  sendMessage nsTableView textShouldBeginEditingSelector (toNSText textObject)

-- | @- textShouldEndEditing:@
textShouldEndEditing :: (IsNSTableView nsTableView, IsNSText textObject) => nsTableView -> textObject -> IO Bool
textShouldEndEditing nsTableView textObject =
  sendMessage nsTableView textShouldEndEditingSelector (toNSText textObject)

-- | @- textDidBeginEditing:@
textDidBeginEditing :: (IsNSTableView nsTableView, IsNSNotification notification) => nsTableView -> notification -> IO ()
textDidBeginEditing nsTableView notification =
  sendMessage nsTableView textDidBeginEditingSelector (toNSNotification notification)

-- | @- textDidEndEditing:@
textDidEndEditing :: (IsNSTableView nsTableView, IsNSNotification notification) => nsTableView -> notification -> IO ()
textDidEndEditing nsTableView notification =
  sendMessage nsTableView textDidEndEditingSelector (toNSNotification notification)

-- | @- textDidChange:@
textDidChange :: (IsNSTableView nsTableView, IsNSNotification notification) => nsTableView -> notification -> IO ()
textDidChange nsTableView notification =
  sendMessage nsTableView textDidChangeSelector (toNSNotification notification)

-- | @- shouldFocusCell:atColumn:row:@
shouldFocusCell_atColumn_row :: (IsNSTableView nsTableView, IsNSCell cell) => nsTableView -> cell -> CLong -> CLong -> IO Bool
shouldFocusCell_atColumn_row nsTableView cell column row =
  sendMessage nsTableView shouldFocusCell_atColumn_rowSelector (toNSCell cell) column row

-- | @- focusedColumn@
focusedColumn :: IsNSTableView nsTableView => nsTableView -> IO CLong
focusedColumn nsTableView =
  sendMessage nsTableView focusedColumnSelector

-- | @- setFocusedColumn:@
setFocusedColumn :: IsNSTableView nsTableView => nsTableView -> CLong -> IO ()
setFocusedColumn nsTableView focusedColumn =
  sendMessage nsTableView setFocusedColumnSelector focusedColumn

-- | @- performClickOnCellAtColumn:row:@
performClickOnCellAtColumn_row :: IsNSTableView nsTableView => nsTableView -> CLong -> CLong -> IO ()
performClickOnCellAtColumn_row nsTableView column row =
  sendMessage nsTableView performClickOnCellAtColumn_rowSelector column row

-- | @- dataSource@
dataSource :: IsNSTableView nsTableView => nsTableView -> IO RawId
dataSource nsTableView =
  sendMessage nsTableView dataSourceSelector

-- | @- setDataSource:@
setDataSource :: IsNSTableView nsTableView => nsTableView -> RawId -> IO ()
setDataSource nsTableView value =
  sendMessage nsTableView setDataSourceSelector value

-- | @- delegate@
delegate :: IsNSTableView nsTableView => nsTableView -> IO RawId
delegate nsTableView =
  sendMessage nsTableView delegateSelector

-- | @- setDelegate:@
setDelegate :: IsNSTableView nsTableView => nsTableView -> RawId -> IO ()
setDelegate nsTableView value =
  sendMessage nsTableView setDelegateSelector value

-- | @- headerView@
headerView :: IsNSTableView nsTableView => nsTableView -> IO (Id NSTableHeaderView)
headerView nsTableView =
  sendMessage nsTableView headerViewSelector

-- | @- setHeaderView:@
setHeaderView :: (IsNSTableView nsTableView, IsNSTableHeaderView value) => nsTableView -> value -> IO ()
setHeaderView nsTableView value =
  sendMessage nsTableView setHeaderViewSelector (toNSTableHeaderView value)

-- | @- cornerView@
cornerView :: IsNSTableView nsTableView => nsTableView -> IO (Id NSView)
cornerView nsTableView =
  sendMessage nsTableView cornerViewSelector

-- | @- setCornerView:@
setCornerView :: (IsNSTableView nsTableView, IsNSView value) => nsTableView -> value -> IO ()
setCornerView nsTableView value =
  sendMessage nsTableView setCornerViewSelector (toNSView value)

-- | @- allowsColumnReordering@
allowsColumnReordering :: IsNSTableView nsTableView => nsTableView -> IO Bool
allowsColumnReordering nsTableView =
  sendMessage nsTableView allowsColumnReorderingSelector

-- | @- setAllowsColumnReordering:@
setAllowsColumnReordering :: IsNSTableView nsTableView => nsTableView -> Bool -> IO ()
setAllowsColumnReordering nsTableView value =
  sendMessage nsTableView setAllowsColumnReorderingSelector value

-- | @- allowsColumnResizing@
allowsColumnResizing :: IsNSTableView nsTableView => nsTableView -> IO Bool
allowsColumnResizing nsTableView =
  sendMessage nsTableView allowsColumnResizingSelector

-- | @- setAllowsColumnResizing:@
setAllowsColumnResizing :: IsNSTableView nsTableView => nsTableView -> Bool -> IO ()
setAllowsColumnResizing nsTableView value =
  sendMessage nsTableView setAllowsColumnResizingSelector value

-- | @- columnAutoresizingStyle@
columnAutoresizingStyle :: IsNSTableView nsTableView => nsTableView -> IO NSTableViewColumnAutoresizingStyle
columnAutoresizingStyle nsTableView =
  sendMessage nsTableView columnAutoresizingStyleSelector

-- | @- setColumnAutoresizingStyle:@
setColumnAutoresizingStyle :: IsNSTableView nsTableView => nsTableView -> NSTableViewColumnAutoresizingStyle -> IO ()
setColumnAutoresizingStyle nsTableView value =
  sendMessage nsTableView setColumnAutoresizingStyleSelector value

-- | @- gridStyleMask@
gridStyleMask :: IsNSTableView nsTableView => nsTableView -> IO NSTableViewGridLineStyle
gridStyleMask nsTableView =
  sendMessage nsTableView gridStyleMaskSelector

-- | @- setGridStyleMask:@
setGridStyleMask :: IsNSTableView nsTableView => nsTableView -> NSTableViewGridLineStyle -> IO ()
setGridStyleMask nsTableView value =
  sendMessage nsTableView setGridStyleMaskSelector value

-- | @- intercellSpacing@
intercellSpacing :: IsNSTableView nsTableView => nsTableView -> IO NSSize
intercellSpacing nsTableView =
  sendMessage nsTableView intercellSpacingSelector

-- | @- setIntercellSpacing:@
setIntercellSpacing :: IsNSTableView nsTableView => nsTableView -> NSSize -> IO ()
setIntercellSpacing nsTableView value =
  sendMessage nsTableView setIntercellSpacingSelector value

-- | @- usesAlternatingRowBackgroundColors@
usesAlternatingRowBackgroundColors :: IsNSTableView nsTableView => nsTableView -> IO Bool
usesAlternatingRowBackgroundColors nsTableView =
  sendMessage nsTableView usesAlternatingRowBackgroundColorsSelector

-- | @- setUsesAlternatingRowBackgroundColors:@
setUsesAlternatingRowBackgroundColors :: IsNSTableView nsTableView => nsTableView -> Bool -> IO ()
setUsesAlternatingRowBackgroundColors nsTableView value =
  sendMessage nsTableView setUsesAlternatingRowBackgroundColorsSelector value

-- | @- backgroundColor@
backgroundColor :: IsNSTableView nsTableView => nsTableView -> IO (Id NSColor)
backgroundColor nsTableView =
  sendMessage nsTableView backgroundColorSelector

-- | @- setBackgroundColor:@
setBackgroundColor :: (IsNSTableView nsTableView, IsNSColor value) => nsTableView -> value -> IO ()
setBackgroundColor nsTableView value =
  sendMessage nsTableView setBackgroundColorSelector (toNSColor value)

-- | @- gridColor@
gridColor :: IsNSTableView nsTableView => nsTableView -> IO (Id NSColor)
gridColor nsTableView =
  sendMessage nsTableView gridColorSelector

-- | @- setGridColor:@
setGridColor :: (IsNSTableView nsTableView, IsNSColor value) => nsTableView -> value -> IO ()
setGridColor nsTableView value =
  sendMessage nsTableView setGridColorSelector (toNSColor value)

-- | @- rowSizeStyle@
rowSizeStyle :: IsNSTableView nsTableView => nsTableView -> IO NSTableViewRowSizeStyle
rowSizeStyle nsTableView =
  sendMessage nsTableView rowSizeStyleSelector

-- | @- setRowSizeStyle:@
setRowSizeStyle :: IsNSTableView nsTableView => nsTableView -> NSTableViewRowSizeStyle -> IO ()
setRowSizeStyle nsTableView value =
  sendMessage nsTableView setRowSizeStyleSelector value

-- | @- effectiveRowSizeStyle@
effectiveRowSizeStyle :: IsNSTableView nsTableView => nsTableView -> IO NSTableViewRowSizeStyle
effectiveRowSizeStyle nsTableView =
  sendMessage nsTableView effectiveRowSizeStyleSelector

-- | @- rowHeight@
rowHeight :: IsNSTableView nsTableView => nsTableView -> IO CDouble
rowHeight nsTableView =
  sendMessage nsTableView rowHeightSelector

-- | @- setRowHeight:@
setRowHeight :: IsNSTableView nsTableView => nsTableView -> CDouble -> IO ()
setRowHeight nsTableView value =
  sendMessage nsTableView setRowHeightSelector value

-- | @- tableColumns@
tableColumns :: IsNSTableView nsTableView => nsTableView -> IO (Id NSArray)
tableColumns nsTableView =
  sendMessage nsTableView tableColumnsSelector

-- | @- numberOfColumns@
numberOfColumns :: IsNSTableView nsTableView => nsTableView -> IO CLong
numberOfColumns nsTableView =
  sendMessage nsTableView numberOfColumnsSelector

-- | @- numberOfRows@
numberOfRows :: IsNSTableView nsTableView => nsTableView -> IO CLong
numberOfRows nsTableView =
  sendMessage nsTableView numberOfRowsSelector

-- | @- editedColumn@
editedColumn :: IsNSTableView nsTableView => nsTableView -> IO CLong
editedColumn nsTableView =
  sendMessage nsTableView editedColumnSelector

-- | @- editedRow@
editedRow :: IsNSTableView nsTableView => nsTableView -> IO CLong
editedRow nsTableView =
  sendMessage nsTableView editedRowSelector

-- | @- clickedColumn@
clickedColumn :: IsNSTableView nsTableView => nsTableView -> IO CLong
clickedColumn nsTableView =
  sendMessage nsTableView clickedColumnSelector

-- | @- clickedRow@
clickedRow :: IsNSTableView nsTableView => nsTableView -> IO CLong
clickedRow nsTableView =
  sendMessage nsTableView clickedRowSelector

-- | @- doubleAction@
doubleAction :: IsNSTableView nsTableView => nsTableView -> IO Sel
doubleAction nsTableView =
  sendMessage nsTableView doubleActionSelector

-- | @- setDoubleAction:@
setDoubleAction :: IsNSTableView nsTableView => nsTableView -> Sel -> IO ()
setDoubleAction nsTableView value =
  sendMessage nsTableView setDoubleActionSelector value

-- | @- sortDescriptors@
sortDescriptors :: IsNSTableView nsTableView => nsTableView -> IO (Id NSArray)
sortDescriptors nsTableView =
  sendMessage nsTableView sortDescriptorsSelector

-- | @- setSortDescriptors:@
setSortDescriptors :: (IsNSTableView nsTableView, IsNSArray value) => nsTableView -> value -> IO ()
setSortDescriptors nsTableView value =
  sendMessage nsTableView setSortDescriptorsSelector (toNSArray value)

-- | @- highlightedTableColumn@
highlightedTableColumn :: IsNSTableView nsTableView => nsTableView -> IO (Id NSTableColumn)
highlightedTableColumn nsTableView =
  sendMessage nsTableView highlightedTableColumnSelector

-- | @- setHighlightedTableColumn:@
setHighlightedTableColumn :: (IsNSTableView nsTableView, IsNSTableColumn value) => nsTableView -> value -> IO ()
setHighlightedTableColumn nsTableView value =
  sendMessage nsTableView setHighlightedTableColumnSelector (toNSTableColumn value)

-- | @- verticalMotionCanBeginDrag@
verticalMotionCanBeginDrag :: IsNSTableView nsTableView => nsTableView -> IO Bool
verticalMotionCanBeginDrag nsTableView =
  sendMessage nsTableView verticalMotionCanBeginDragSelector

-- | @- setVerticalMotionCanBeginDrag:@
setVerticalMotionCanBeginDrag :: IsNSTableView nsTableView => nsTableView -> Bool -> IO ()
setVerticalMotionCanBeginDrag nsTableView value =
  sendMessage nsTableView setVerticalMotionCanBeginDragSelector value

-- | @- allowsMultipleSelection@
allowsMultipleSelection :: IsNSTableView nsTableView => nsTableView -> IO Bool
allowsMultipleSelection nsTableView =
  sendMessage nsTableView allowsMultipleSelectionSelector

-- | @- setAllowsMultipleSelection:@
setAllowsMultipleSelection :: IsNSTableView nsTableView => nsTableView -> Bool -> IO ()
setAllowsMultipleSelection nsTableView value =
  sendMessage nsTableView setAllowsMultipleSelectionSelector value

-- | @- allowsEmptySelection@
allowsEmptySelection :: IsNSTableView nsTableView => nsTableView -> IO Bool
allowsEmptySelection nsTableView =
  sendMessage nsTableView allowsEmptySelectionSelector

-- | @- setAllowsEmptySelection:@
setAllowsEmptySelection :: IsNSTableView nsTableView => nsTableView -> Bool -> IO ()
setAllowsEmptySelection nsTableView value =
  sendMessage nsTableView setAllowsEmptySelectionSelector value

-- | @- allowsColumnSelection@
allowsColumnSelection :: IsNSTableView nsTableView => nsTableView -> IO Bool
allowsColumnSelection nsTableView =
  sendMessage nsTableView allowsColumnSelectionSelector

-- | @- setAllowsColumnSelection:@
setAllowsColumnSelection :: IsNSTableView nsTableView => nsTableView -> Bool -> IO ()
setAllowsColumnSelection nsTableView value =
  sendMessage nsTableView setAllowsColumnSelectionSelector value

-- | @- selectedColumnIndexes@
selectedColumnIndexes :: IsNSTableView nsTableView => nsTableView -> IO (Id NSIndexSet)
selectedColumnIndexes nsTableView =
  sendMessage nsTableView selectedColumnIndexesSelector

-- | @- selectedRowIndexes@
selectedRowIndexes :: IsNSTableView nsTableView => nsTableView -> IO (Id NSIndexSet)
selectedRowIndexes nsTableView =
  sendMessage nsTableView selectedRowIndexesSelector

-- | @- selectedColumn@
selectedColumn :: IsNSTableView nsTableView => nsTableView -> IO CLong
selectedColumn nsTableView =
  sendMessage nsTableView selectedColumnSelector

-- | @- selectedRow@
selectedRow :: IsNSTableView nsTableView => nsTableView -> IO CLong
selectedRow nsTableView =
  sendMessage nsTableView selectedRowSelector

-- | @- numberOfSelectedColumns@
numberOfSelectedColumns :: IsNSTableView nsTableView => nsTableView -> IO CLong
numberOfSelectedColumns nsTableView =
  sendMessage nsTableView numberOfSelectedColumnsSelector

-- | @- numberOfSelectedRows@
numberOfSelectedRows :: IsNSTableView nsTableView => nsTableView -> IO CLong
numberOfSelectedRows nsTableView =
  sendMessage nsTableView numberOfSelectedRowsSelector

-- | @- allowsTypeSelect@
allowsTypeSelect :: IsNSTableView nsTableView => nsTableView -> IO Bool
allowsTypeSelect nsTableView =
  sendMessage nsTableView allowsTypeSelectSelector

-- | @- setAllowsTypeSelect:@
setAllowsTypeSelect :: IsNSTableView nsTableView => nsTableView -> Bool -> IO ()
setAllowsTypeSelect nsTableView value =
  sendMessage nsTableView setAllowsTypeSelectSelector value

-- | @- style@
style :: IsNSTableView nsTableView => nsTableView -> IO NSTableViewStyle
style nsTableView =
  sendMessage nsTableView styleSelector

-- | @- setStyle:@
setStyle :: IsNSTableView nsTableView => nsTableView -> NSTableViewStyle -> IO ()
setStyle nsTableView value =
  sendMessage nsTableView setStyleSelector value

-- | @- effectiveStyle@
effectiveStyle :: IsNSTableView nsTableView => nsTableView -> IO NSTableViewStyle
effectiveStyle nsTableView =
  sendMessage nsTableView effectiveStyleSelector

-- | @- selectionHighlightStyle@
selectionHighlightStyle :: IsNSTableView nsTableView => nsTableView -> IO NSTableViewSelectionHighlightStyle
selectionHighlightStyle nsTableView =
  sendMessage nsTableView selectionHighlightStyleSelector

-- | @- setSelectionHighlightStyle:@
setSelectionHighlightStyle :: IsNSTableView nsTableView => nsTableView -> NSTableViewSelectionHighlightStyle -> IO ()
setSelectionHighlightStyle nsTableView value =
  sendMessage nsTableView setSelectionHighlightStyleSelector value

-- | @- draggingDestinationFeedbackStyle@
draggingDestinationFeedbackStyle :: IsNSTableView nsTableView => nsTableView -> IO NSTableViewDraggingDestinationFeedbackStyle
draggingDestinationFeedbackStyle nsTableView =
  sendMessage nsTableView draggingDestinationFeedbackStyleSelector

-- | @- setDraggingDestinationFeedbackStyle:@
setDraggingDestinationFeedbackStyle :: IsNSTableView nsTableView => nsTableView -> NSTableViewDraggingDestinationFeedbackStyle -> IO ()
setDraggingDestinationFeedbackStyle nsTableView value =
  sendMessage nsTableView setDraggingDestinationFeedbackStyleSelector value

-- | @- autosaveName@
autosaveName :: IsNSTableView nsTableView => nsTableView -> IO (Id NSString)
autosaveName nsTableView =
  sendMessage nsTableView autosaveNameSelector

-- | @- setAutosaveName:@
setAutosaveName :: (IsNSTableView nsTableView, IsNSString value) => nsTableView -> value -> IO ()
setAutosaveName nsTableView value =
  sendMessage nsTableView setAutosaveNameSelector (toNSString value)

-- | @- autosaveTableColumns@
autosaveTableColumns :: IsNSTableView nsTableView => nsTableView -> IO Bool
autosaveTableColumns nsTableView =
  sendMessage nsTableView autosaveTableColumnsSelector

-- | @- setAutosaveTableColumns:@
setAutosaveTableColumns :: IsNSTableView nsTableView => nsTableView -> Bool -> IO ()
setAutosaveTableColumns nsTableView value =
  sendMessage nsTableView setAutosaveTableColumnsSelector value

-- | @- floatsGroupRows@
floatsGroupRows :: IsNSTableView nsTableView => nsTableView -> IO Bool
floatsGroupRows nsTableView =
  sendMessage nsTableView floatsGroupRowsSelector

-- | @- setFloatsGroupRows:@
setFloatsGroupRows :: IsNSTableView nsTableView => nsTableView -> Bool -> IO ()
setFloatsGroupRows nsTableView value =
  sendMessage nsTableView setFloatsGroupRowsSelector value

-- | @- rowActionsVisible@
rowActionsVisible :: IsNSTableView nsTableView => nsTableView -> IO Bool
rowActionsVisible nsTableView =
  sendMessage nsTableView rowActionsVisibleSelector

-- | @- setRowActionsVisible:@
setRowActionsVisible :: IsNSTableView nsTableView => nsTableView -> Bool -> IO ()
setRowActionsVisible nsTableView value =
  sendMessage nsTableView setRowActionsVisibleSelector value

-- | @- hiddenRowIndexes@
hiddenRowIndexes :: IsNSTableView nsTableView => nsTableView -> IO (Id NSIndexSet)
hiddenRowIndexes nsTableView =
  sendMessage nsTableView hiddenRowIndexesSelector

-- | @- registeredNibsByIdentifier@
registeredNibsByIdentifier :: IsNSTableView nsTableView => nsTableView -> IO (Id NSDictionary)
registeredNibsByIdentifier nsTableView =
  sendMessage nsTableView registeredNibsByIdentifierSelector

-- | @- usesStaticContents@
usesStaticContents :: IsNSTableView nsTableView => nsTableView -> IO Bool
usesStaticContents nsTableView =
  sendMessage nsTableView usesStaticContentsSelector

-- | @- setUsesStaticContents:@
setUsesStaticContents :: IsNSTableView nsTableView => nsTableView -> Bool -> IO ()
setUsesStaticContents nsTableView value =
  sendMessage nsTableView setUsesStaticContentsSelector value

-- | @- userInterfaceLayoutDirection@
userInterfaceLayoutDirection :: IsNSTableView nsTableView => nsTableView -> IO NSUserInterfaceLayoutDirection
userInterfaceLayoutDirection nsTableView =
  sendMessage nsTableView userInterfaceLayoutDirectionSelector

-- | @- setUserInterfaceLayoutDirection:@
setUserInterfaceLayoutDirection :: IsNSTableView nsTableView => nsTableView -> NSUserInterfaceLayoutDirection -> IO ()
setUserInterfaceLayoutDirection nsTableView value =
  sendMessage nsTableView setUserInterfaceLayoutDirectionSelector value

-- | @- usesAutomaticRowHeights@
usesAutomaticRowHeights :: IsNSTableView nsTableView => nsTableView -> IO Bool
usesAutomaticRowHeights nsTableView =
  sendMessage nsTableView usesAutomaticRowHeightsSelector

-- | @- setUsesAutomaticRowHeights:@
setUsesAutomaticRowHeights :: IsNSTableView nsTableView => nsTableView -> Bool -> IO ()
setUsesAutomaticRowHeights nsTableView value =
  sendMessage nsTableView setUsesAutomaticRowHeightsSelector value

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @initWithFrame:@
initWithFrameSelector :: Selector '[NSRect] (Id NSTableView)
initWithFrameSelector = mkSelector "initWithFrame:"

-- | @Selector@ for @initWithCoder:@
initWithCoderSelector :: Selector '[Id NSCoder] (Id NSTableView)
initWithCoderSelector = mkSelector "initWithCoder:"

-- | @Selector@ for @noteHeightOfRowsWithIndexesChanged:@
noteHeightOfRowsWithIndexesChangedSelector :: Selector '[Id NSIndexSet] ()
noteHeightOfRowsWithIndexesChangedSelector = mkSelector "noteHeightOfRowsWithIndexesChanged:"

-- | @Selector@ for @addTableColumn:@
addTableColumnSelector :: Selector '[Id NSTableColumn] ()
addTableColumnSelector = mkSelector "addTableColumn:"

-- | @Selector@ for @removeTableColumn:@
removeTableColumnSelector :: Selector '[Id NSTableColumn] ()
removeTableColumnSelector = mkSelector "removeTableColumn:"

-- | @Selector@ for @moveColumn:toColumn:@
moveColumn_toColumnSelector :: Selector '[CLong, CLong] ()
moveColumn_toColumnSelector = mkSelector "moveColumn:toColumn:"

-- | @Selector@ for @columnWithIdentifier:@
columnWithIdentifierSelector :: Selector '[Id NSString] CLong
columnWithIdentifierSelector = mkSelector "columnWithIdentifier:"

-- | @Selector@ for @tableColumnWithIdentifier:@
tableColumnWithIdentifierSelector :: Selector '[Id NSString] (Id NSTableColumn)
tableColumnWithIdentifierSelector = mkSelector "tableColumnWithIdentifier:"

-- | @Selector@ for @tile@
tileSelector :: Selector '[] ()
tileSelector = mkSelector "tile"

-- | @Selector@ for @sizeToFit@
sizeToFitSelector :: Selector '[] ()
sizeToFitSelector = mkSelector "sizeToFit"

-- | @Selector@ for @sizeLastColumnToFit@
sizeLastColumnToFitSelector :: Selector '[] ()
sizeLastColumnToFitSelector = mkSelector "sizeLastColumnToFit"

-- | @Selector@ for @scrollRowToVisible:@
scrollRowToVisibleSelector :: Selector '[CLong] ()
scrollRowToVisibleSelector = mkSelector "scrollRowToVisible:"

-- | @Selector@ for @scrollColumnToVisible:@
scrollColumnToVisibleSelector :: Selector '[CLong] ()
scrollColumnToVisibleSelector = mkSelector "scrollColumnToVisible:"

-- | @Selector@ for @reloadData@
reloadDataSelector :: Selector '[] ()
reloadDataSelector = mkSelector "reloadData"

-- | @Selector@ for @noteNumberOfRowsChanged@
noteNumberOfRowsChangedSelector :: Selector '[] ()
noteNumberOfRowsChangedSelector = mkSelector "noteNumberOfRowsChanged"

-- | @Selector@ for @reloadDataForRowIndexes:columnIndexes:@
reloadDataForRowIndexes_columnIndexesSelector :: Selector '[Id NSIndexSet, Id NSIndexSet] ()
reloadDataForRowIndexes_columnIndexesSelector = mkSelector "reloadDataForRowIndexes:columnIndexes:"

-- | @Selector@ for @setIndicatorImage:inTableColumn:@
setIndicatorImage_inTableColumnSelector :: Selector '[Id NSImage, Id NSTableColumn] ()
setIndicatorImage_inTableColumnSelector = mkSelector "setIndicatorImage:inTableColumn:"

-- | @Selector@ for @indicatorImageInTableColumn:@
indicatorImageInTableColumnSelector :: Selector '[Id NSTableColumn] (Id NSImage)
indicatorImageInTableColumnSelector = mkSelector "indicatorImageInTableColumn:"

-- | @Selector@ for @canDragRowsWithIndexes:atPoint:@
canDragRowsWithIndexes_atPointSelector :: Selector '[Id NSIndexSet, NSPoint] Bool
canDragRowsWithIndexes_atPointSelector = mkSelector "canDragRowsWithIndexes:atPoint:"

-- | @Selector@ for @dragImageForRowsWithIndexes:tableColumns:event:offset:@
dragImageForRowsWithIndexes_tableColumns_event_offsetSelector :: Selector '[Id NSIndexSet, Id NSArray, Id NSEvent, Ptr NSPoint] (Id NSImage)
dragImageForRowsWithIndexes_tableColumns_event_offsetSelector = mkSelector "dragImageForRowsWithIndexes:tableColumns:event:offset:"

-- | @Selector@ for @setDraggingSourceOperationMask:forLocal:@
setDraggingSourceOperationMask_forLocalSelector :: Selector '[NSDragOperation, Bool] ()
setDraggingSourceOperationMask_forLocalSelector = mkSelector "setDraggingSourceOperationMask:forLocal:"

-- | @Selector@ for @setDropRow:dropOperation:@
setDropRow_dropOperationSelector :: Selector '[CLong, NSTableViewDropOperation] ()
setDropRow_dropOperationSelector = mkSelector "setDropRow:dropOperation:"

-- | @Selector@ for @selectAll:@
selectAllSelector :: Selector '[RawId] ()
selectAllSelector = mkSelector "selectAll:"

-- | @Selector@ for @deselectAll:@
deselectAllSelector :: Selector '[RawId] ()
deselectAllSelector = mkSelector "deselectAll:"

-- | @Selector@ for @selectColumnIndexes:byExtendingSelection:@
selectColumnIndexes_byExtendingSelectionSelector :: Selector '[Id NSIndexSet, Bool] ()
selectColumnIndexes_byExtendingSelectionSelector = mkSelector "selectColumnIndexes:byExtendingSelection:"

-- | @Selector@ for @selectRowIndexes:byExtendingSelection:@
selectRowIndexes_byExtendingSelectionSelector :: Selector '[Id NSIndexSet, Bool] ()
selectRowIndexes_byExtendingSelectionSelector = mkSelector "selectRowIndexes:byExtendingSelection:"

-- | @Selector@ for @deselectColumn:@
deselectColumnSelector :: Selector '[CLong] ()
deselectColumnSelector = mkSelector "deselectColumn:"

-- | @Selector@ for @deselectRow:@
deselectRowSelector :: Selector '[CLong] ()
deselectRowSelector = mkSelector "deselectRow:"

-- | @Selector@ for @isColumnSelected:@
isColumnSelectedSelector :: Selector '[CLong] Bool
isColumnSelectedSelector = mkSelector "isColumnSelected:"

-- | @Selector@ for @isRowSelected:@
isRowSelectedSelector :: Selector '[CLong] Bool
isRowSelectedSelector = mkSelector "isRowSelected:"

-- | @Selector@ for @rectOfColumn:@
rectOfColumnSelector :: Selector '[CLong] NSRect
rectOfColumnSelector = mkSelector "rectOfColumn:"

-- | @Selector@ for @rectOfRow:@
rectOfRowSelector :: Selector '[CLong] NSRect
rectOfRowSelector = mkSelector "rectOfRow:"

-- | @Selector@ for @columnIndexesInRect:@
columnIndexesInRectSelector :: Selector '[NSRect] (Id NSIndexSet)
columnIndexesInRectSelector = mkSelector "columnIndexesInRect:"

-- | @Selector@ for @rowsInRect:@
rowsInRectSelector :: Selector '[NSRect] NSRange
rowsInRectSelector = mkSelector "rowsInRect:"

-- | @Selector@ for @columnAtPoint:@
columnAtPointSelector :: Selector '[NSPoint] CLong
columnAtPointSelector = mkSelector "columnAtPoint:"

-- | @Selector@ for @rowAtPoint:@
rowAtPointSelector :: Selector '[NSPoint] CLong
rowAtPointSelector = mkSelector "rowAtPoint:"

-- | @Selector@ for @frameOfCellAtColumn:row:@
frameOfCellAtColumn_rowSelector :: Selector '[CLong, CLong] NSRect
frameOfCellAtColumn_rowSelector = mkSelector "frameOfCellAtColumn:row:"

-- | @Selector@ for @editColumn:row:withEvent:select:@
editColumn_row_withEvent_selectSelector :: Selector '[CLong, CLong, Id NSEvent, Bool] ()
editColumn_row_withEvent_selectSelector = mkSelector "editColumn:row:withEvent:select:"

-- | @Selector@ for @drawRow:clipRect:@
drawRow_clipRectSelector :: Selector '[CLong, NSRect] ()
drawRow_clipRectSelector = mkSelector "drawRow:clipRect:"

-- | @Selector@ for @highlightSelectionInClipRect:@
highlightSelectionInClipRectSelector :: Selector '[NSRect] ()
highlightSelectionInClipRectSelector = mkSelector "highlightSelectionInClipRect:"

-- | @Selector@ for @drawGridInClipRect:@
drawGridInClipRectSelector :: Selector '[NSRect] ()
drawGridInClipRectSelector = mkSelector "drawGridInClipRect:"

-- | @Selector@ for @drawBackgroundInClipRect:@
drawBackgroundInClipRectSelector :: Selector '[NSRect] ()
drawBackgroundInClipRectSelector = mkSelector "drawBackgroundInClipRect:"

-- | @Selector@ for @viewAtColumn:row:makeIfNecessary:@
viewAtColumn_row_makeIfNecessarySelector :: Selector '[CLong, CLong, Bool] (Id NSView)
viewAtColumn_row_makeIfNecessarySelector = mkSelector "viewAtColumn:row:makeIfNecessary:"

-- | @Selector@ for @rowViewAtRow:makeIfNecessary:@
rowViewAtRow_makeIfNecessarySelector :: Selector '[CLong, Bool] (Id NSTableRowView)
rowViewAtRow_makeIfNecessarySelector = mkSelector "rowViewAtRow:makeIfNecessary:"

-- | @Selector@ for @rowForView:@
rowForViewSelector :: Selector '[Id NSView] CLong
rowForViewSelector = mkSelector "rowForView:"

-- | @Selector@ for @columnForView:@
columnForViewSelector :: Selector '[Id NSView] CLong
columnForViewSelector = mkSelector "columnForView:"

-- | @Selector@ for @makeViewWithIdentifier:owner:@
makeViewWithIdentifier_ownerSelector :: Selector '[Id NSString, RawId] (Id NSView)
makeViewWithIdentifier_ownerSelector = mkSelector "makeViewWithIdentifier:owner:"

-- | @Selector@ for @enumerateAvailableRowViewsUsingBlock:@
enumerateAvailableRowViewsUsingBlockSelector :: Selector '[Ptr ()] ()
enumerateAvailableRowViewsUsingBlockSelector = mkSelector "enumerateAvailableRowViewsUsingBlock:"

-- | @Selector@ for @beginUpdates@
beginUpdatesSelector :: Selector '[] ()
beginUpdatesSelector = mkSelector "beginUpdates"

-- | @Selector@ for @endUpdates@
endUpdatesSelector :: Selector '[] ()
endUpdatesSelector = mkSelector "endUpdates"

-- | @Selector@ for @insertRowsAtIndexes:withAnimation:@
insertRowsAtIndexes_withAnimationSelector :: Selector '[Id NSIndexSet, NSTableViewAnimationOptions] ()
insertRowsAtIndexes_withAnimationSelector = mkSelector "insertRowsAtIndexes:withAnimation:"

-- | @Selector@ for @removeRowsAtIndexes:withAnimation:@
removeRowsAtIndexes_withAnimationSelector :: Selector '[Id NSIndexSet, NSTableViewAnimationOptions] ()
removeRowsAtIndexes_withAnimationSelector = mkSelector "removeRowsAtIndexes:withAnimation:"

-- | @Selector@ for @moveRowAtIndex:toIndex:@
moveRowAtIndex_toIndexSelector :: Selector '[CLong, CLong] ()
moveRowAtIndex_toIndexSelector = mkSelector "moveRowAtIndex:toIndex:"

-- | @Selector@ for @hideRowsAtIndexes:withAnimation:@
hideRowsAtIndexes_withAnimationSelector :: Selector '[Id NSIndexSet, NSTableViewAnimationOptions] ()
hideRowsAtIndexes_withAnimationSelector = mkSelector "hideRowsAtIndexes:withAnimation:"

-- | @Selector@ for @unhideRowsAtIndexes:withAnimation:@
unhideRowsAtIndexes_withAnimationSelector :: Selector '[Id NSIndexSet, NSTableViewAnimationOptions] ()
unhideRowsAtIndexes_withAnimationSelector = mkSelector "unhideRowsAtIndexes:withAnimation:"

-- | @Selector@ for @registerNib:forIdentifier:@
registerNib_forIdentifierSelector :: Selector '[Id NSNib, Id NSString] ()
registerNib_forIdentifierSelector = mkSelector "registerNib:forIdentifier:"

-- | @Selector@ for @didAddRowView:forRow:@
didAddRowView_forRowSelector :: Selector '[Id NSTableRowView, CLong] ()
didAddRowView_forRowSelector = mkSelector "didAddRowView:forRow:"

-- | @Selector@ for @didRemoveRowView:forRow:@
didRemoveRowView_forRowSelector :: Selector '[Id NSTableRowView, CLong] ()
didRemoveRowView_forRowSelector = mkSelector "didRemoveRowView:forRow:"

-- | @Selector@ for @setDrawsGrid:@
setDrawsGridSelector :: Selector '[Bool] ()
setDrawsGridSelector = mkSelector "setDrawsGrid:"

-- | @Selector@ for @drawsGrid@
drawsGridSelector :: Selector '[] Bool
drawsGridSelector = mkSelector "drawsGrid"

-- | @Selector@ for @selectColumn:byExtendingSelection:@
selectColumn_byExtendingSelectionSelector :: Selector '[CLong, Bool] ()
selectColumn_byExtendingSelectionSelector = mkSelector "selectColumn:byExtendingSelection:"

-- | @Selector@ for @selectRow:byExtendingSelection:@
selectRow_byExtendingSelectionSelector :: Selector '[CLong, Bool] ()
selectRow_byExtendingSelectionSelector = mkSelector "selectRow:byExtendingSelection:"

-- | @Selector@ for @selectedColumnEnumerator@
selectedColumnEnumeratorSelector :: Selector '[] (Id NSEnumerator)
selectedColumnEnumeratorSelector = mkSelector "selectedColumnEnumerator"

-- | @Selector@ for @selectedRowEnumerator@
selectedRowEnumeratorSelector :: Selector '[] (Id NSEnumerator)
selectedRowEnumeratorSelector = mkSelector "selectedRowEnumerator"

-- | @Selector@ for @dragImageForRows:event:dragImageOffset:@
dragImageForRows_event_dragImageOffsetSelector :: Selector '[Id NSArray, Id NSEvent, Ptr NSPoint] (Id NSImage)
dragImageForRows_event_dragImageOffsetSelector = mkSelector "dragImageForRows:event:dragImageOffset:"

-- | @Selector@ for @setAutoresizesAllColumnsToFit:@
setAutoresizesAllColumnsToFitSelector :: Selector '[Bool] ()
setAutoresizesAllColumnsToFitSelector = mkSelector "setAutoresizesAllColumnsToFit:"

-- | @Selector@ for @autoresizesAllColumnsToFit@
autoresizesAllColumnsToFitSelector :: Selector '[] Bool
autoresizesAllColumnsToFitSelector = mkSelector "autoresizesAllColumnsToFit"

-- | @Selector@ for @columnsInRect:@
columnsInRectSelector :: Selector '[NSRect] NSRange
columnsInRectSelector = mkSelector "columnsInRect:"

-- | @Selector@ for @preparedCellAtColumn:row:@
preparedCellAtColumn_rowSelector :: Selector '[CLong, CLong] (Id NSCell)
preparedCellAtColumn_rowSelector = mkSelector "preparedCellAtColumn:row:"

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

-- | @Selector@ for @shouldFocusCell:atColumn:row:@
shouldFocusCell_atColumn_rowSelector :: Selector '[Id NSCell, CLong, CLong] Bool
shouldFocusCell_atColumn_rowSelector = mkSelector "shouldFocusCell:atColumn:row:"

-- | @Selector@ for @focusedColumn@
focusedColumnSelector :: Selector '[] CLong
focusedColumnSelector = mkSelector "focusedColumn"

-- | @Selector@ for @setFocusedColumn:@
setFocusedColumnSelector :: Selector '[CLong] ()
setFocusedColumnSelector = mkSelector "setFocusedColumn:"

-- | @Selector@ for @performClickOnCellAtColumn:row:@
performClickOnCellAtColumn_rowSelector :: Selector '[CLong, CLong] ()
performClickOnCellAtColumn_rowSelector = mkSelector "performClickOnCellAtColumn:row:"

-- | @Selector@ for @dataSource@
dataSourceSelector :: Selector '[] RawId
dataSourceSelector = mkSelector "dataSource"

-- | @Selector@ for @setDataSource:@
setDataSourceSelector :: Selector '[RawId] ()
setDataSourceSelector = mkSelector "setDataSource:"

-- | @Selector@ for @delegate@
delegateSelector :: Selector '[] RawId
delegateSelector = mkSelector "delegate"

-- | @Selector@ for @setDelegate:@
setDelegateSelector :: Selector '[RawId] ()
setDelegateSelector = mkSelector "setDelegate:"

-- | @Selector@ for @headerView@
headerViewSelector :: Selector '[] (Id NSTableHeaderView)
headerViewSelector = mkSelector "headerView"

-- | @Selector@ for @setHeaderView:@
setHeaderViewSelector :: Selector '[Id NSTableHeaderView] ()
setHeaderViewSelector = mkSelector "setHeaderView:"

-- | @Selector@ for @cornerView@
cornerViewSelector :: Selector '[] (Id NSView)
cornerViewSelector = mkSelector "cornerView"

-- | @Selector@ for @setCornerView:@
setCornerViewSelector :: Selector '[Id NSView] ()
setCornerViewSelector = mkSelector "setCornerView:"

-- | @Selector@ for @allowsColumnReordering@
allowsColumnReorderingSelector :: Selector '[] Bool
allowsColumnReorderingSelector = mkSelector "allowsColumnReordering"

-- | @Selector@ for @setAllowsColumnReordering:@
setAllowsColumnReorderingSelector :: Selector '[Bool] ()
setAllowsColumnReorderingSelector = mkSelector "setAllowsColumnReordering:"

-- | @Selector@ for @allowsColumnResizing@
allowsColumnResizingSelector :: Selector '[] Bool
allowsColumnResizingSelector = mkSelector "allowsColumnResizing"

-- | @Selector@ for @setAllowsColumnResizing:@
setAllowsColumnResizingSelector :: Selector '[Bool] ()
setAllowsColumnResizingSelector = mkSelector "setAllowsColumnResizing:"

-- | @Selector@ for @columnAutoresizingStyle@
columnAutoresizingStyleSelector :: Selector '[] NSTableViewColumnAutoresizingStyle
columnAutoresizingStyleSelector = mkSelector "columnAutoresizingStyle"

-- | @Selector@ for @setColumnAutoresizingStyle:@
setColumnAutoresizingStyleSelector :: Selector '[NSTableViewColumnAutoresizingStyle] ()
setColumnAutoresizingStyleSelector = mkSelector "setColumnAutoresizingStyle:"

-- | @Selector@ for @gridStyleMask@
gridStyleMaskSelector :: Selector '[] NSTableViewGridLineStyle
gridStyleMaskSelector = mkSelector "gridStyleMask"

-- | @Selector@ for @setGridStyleMask:@
setGridStyleMaskSelector :: Selector '[NSTableViewGridLineStyle] ()
setGridStyleMaskSelector = mkSelector "setGridStyleMask:"

-- | @Selector@ for @intercellSpacing@
intercellSpacingSelector :: Selector '[] NSSize
intercellSpacingSelector = mkSelector "intercellSpacing"

-- | @Selector@ for @setIntercellSpacing:@
setIntercellSpacingSelector :: Selector '[NSSize] ()
setIntercellSpacingSelector = mkSelector "setIntercellSpacing:"

-- | @Selector@ for @usesAlternatingRowBackgroundColors@
usesAlternatingRowBackgroundColorsSelector :: Selector '[] Bool
usesAlternatingRowBackgroundColorsSelector = mkSelector "usesAlternatingRowBackgroundColors"

-- | @Selector@ for @setUsesAlternatingRowBackgroundColors:@
setUsesAlternatingRowBackgroundColorsSelector :: Selector '[Bool] ()
setUsesAlternatingRowBackgroundColorsSelector = mkSelector "setUsesAlternatingRowBackgroundColors:"

-- | @Selector@ for @backgroundColor@
backgroundColorSelector :: Selector '[] (Id NSColor)
backgroundColorSelector = mkSelector "backgroundColor"

-- | @Selector@ for @setBackgroundColor:@
setBackgroundColorSelector :: Selector '[Id NSColor] ()
setBackgroundColorSelector = mkSelector "setBackgroundColor:"

-- | @Selector@ for @gridColor@
gridColorSelector :: Selector '[] (Id NSColor)
gridColorSelector = mkSelector "gridColor"

-- | @Selector@ for @setGridColor:@
setGridColorSelector :: Selector '[Id NSColor] ()
setGridColorSelector = mkSelector "setGridColor:"

-- | @Selector@ for @rowSizeStyle@
rowSizeStyleSelector :: Selector '[] NSTableViewRowSizeStyle
rowSizeStyleSelector = mkSelector "rowSizeStyle"

-- | @Selector@ for @setRowSizeStyle:@
setRowSizeStyleSelector :: Selector '[NSTableViewRowSizeStyle] ()
setRowSizeStyleSelector = mkSelector "setRowSizeStyle:"

-- | @Selector@ for @effectiveRowSizeStyle@
effectiveRowSizeStyleSelector :: Selector '[] NSTableViewRowSizeStyle
effectiveRowSizeStyleSelector = mkSelector "effectiveRowSizeStyle"

-- | @Selector@ for @rowHeight@
rowHeightSelector :: Selector '[] CDouble
rowHeightSelector = mkSelector "rowHeight"

-- | @Selector@ for @setRowHeight:@
setRowHeightSelector :: Selector '[CDouble] ()
setRowHeightSelector = mkSelector "setRowHeight:"

-- | @Selector@ for @tableColumns@
tableColumnsSelector :: Selector '[] (Id NSArray)
tableColumnsSelector = mkSelector "tableColumns"

-- | @Selector@ for @numberOfColumns@
numberOfColumnsSelector :: Selector '[] CLong
numberOfColumnsSelector = mkSelector "numberOfColumns"

-- | @Selector@ for @numberOfRows@
numberOfRowsSelector :: Selector '[] CLong
numberOfRowsSelector = mkSelector "numberOfRows"

-- | @Selector@ for @editedColumn@
editedColumnSelector :: Selector '[] CLong
editedColumnSelector = mkSelector "editedColumn"

-- | @Selector@ for @editedRow@
editedRowSelector :: Selector '[] CLong
editedRowSelector = mkSelector "editedRow"

-- | @Selector@ for @clickedColumn@
clickedColumnSelector :: Selector '[] CLong
clickedColumnSelector = mkSelector "clickedColumn"

-- | @Selector@ for @clickedRow@
clickedRowSelector :: Selector '[] CLong
clickedRowSelector = mkSelector "clickedRow"

-- | @Selector@ for @doubleAction@
doubleActionSelector :: Selector '[] Sel
doubleActionSelector = mkSelector "doubleAction"

-- | @Selector@ for @setDoubleAction:@
setDoubleActionSelector :: Selector '[Sel] ()
setDoubleActionSelector = mkSelector "setDoubleAction:"

-- | @Selector@ for @sortDescriptors@
sortDescriptorsSelector :: Selector '[] (Id NSArray)
sortDescriptorsSelector = mkSelector "sortDescriptors"

-- | @Selector@ for @setSortDescriptors:@
setSortDescriptorsSelector :: Selector '[Id NSArray] ()
setSortDescriptorsSelector = mkSelector "setSortDescriptors:"

-- | @Selector@ for @highlightedTableColumn@
highlightedTableColumnSelector :: Selector '[] (Id NSTableColumn)
highlightedTableColumnSelector = mkSelector "highlightedTableColumn"

-- | @Selector@ for @setHighlightedTableColumn:@
setHighlightedTableColumnSelector :: Selector '[Id NSTableColumn] ()
setHighlightedTableColumnSelector = mkSelector "setHighlightedTableColumn:"

-- | @Selector@ for @verticalMotionCanBeginDrag@
verticalMotionCanBeginDragSelector :: Selector '[] Bool
verticalMotionCanBeginDragSelector = mkSelector "verticalMotionCanBeginDrag"

-- | @Selector@ for @setVerticalMotionCanBeginDrag:@
setVerticalMotionCanBeginDragSelector :: Selector '[Bool] ()
setVerticalMotionCanBeginDragSelector = mkSelector "setVerticalMotionCanBeginDrag:"

-- | @Selector@ for @allowsMultipleSelection@
allowsMultipleSelectionSelector :: Selector '[] Bool
allowsMultipleSelectionSelector = mkSelector "allowsMultipleSelection"

-- | @Selector@ for @setAllowsMultipleSelection:@
setAllowsMultipleSelectionSelector :: Selector '[Bool] ()
setAllowsMultipleSelectionSelector = mkSelector "setAllowsMultipleSelection:"

-- | @Selector@ for @allowsEmptySelection@
allowsEmptySelectionSelector :: Selector '[] Bool
allowsEmptySelectionSelector = mkSelector "allowsEmptySelection"

-- | @Selector@ for @setAllowsEmptySelection:@
setAllowsEmptySelectionSelector :: Selector '[Bool] ()
setAllowsEmptySelectionSelector = mkSelector "setAllowsEmptySelection:"

-- | @Selector@ for @allowsColumnSelection@
allowsColumnSelectionSelector :: Selector '[] Bool
allowsColumnSelectionSelector = mkSelector "allowsColumnSelection"

-- | @Selector@ for @setAllowsColumnSelection:@
setAllowsColumnSelectionSelector :: Selector '[Bool] ()
setAllowsColumnSelectionSelector = mkSelector "setAllowsColumnSelection:"

-- | @Selector@ for @selectedColumnIndexes@
selectedColumnIndexesSelector :: Selector '[] (Id NSIndexSet)
selectedColumnIndexesSelector = mkSelector "selectedColumnIndexes"

-- | @Selector@ for @selectedRowIndexes@
selectedRowIndexesSelector :: Selector '[] (Id NSIndexSet)
selectedRowIndexesSelector = mkSelector "selectedRowIndexes"

-- | @Selector@ for @selectedColumn@
selectedColumnSelector :: Selector '[] CLong
selectedColumnSelector = mkSelector "selectedColumn"

-- | @Selector@ for @selectedRow@
selectedRowSelector :: Selector '[] CLong
selectedRowSelector = mkSelector "selectedRow"

-- | @Selector@ for @numberOfSelectedColumns@
numberOfSelectedColumnsSelector :: Selector '[] CLong
numberOfSelectedColumnsSelector = mkSelector "numberOfSelectedColumns"

-- | @Selector@ for @numberOfSelectedRows@
numberOfSelectedRowsSelector :: Selector '[] CLong
numberOfSelectedRowsSelector = mkSelector "numberOfSelectedRows"

-- | @Selector@ for @allowsTypeSelect@
allowsTypeSelectSelector :: Selector '[] Bool
allowsTypeSelectSelector = mkSelector "allowsTypeSelect"

-- | @Selector@ for @setAllowsTypeSelect:@
setAllowsTypeSelectSelector :: Selector '[Bool] ()
setAllowsTypeSelectSelector = mkSelector "setAllowsTypeSelect:"

-- | @Selector@ for @style@
styleSelector :: Selector '[] NSTableViewStyle
styleSelector = mkSelector "style"

-- | @Selector@ for @setStyle:@
setStyleSelector :: Selector '[NSTableViewStyle] ()
setStyleSelector = mkSelector "setStyle:"

-- | @Selector@ for @effectiveStyle@
effectiveStyleSelector :: Selector '[] NSTableViewStyle
effectiveStyleSelector = mkSelector "effectiveStyle"

-- | @Selector@ for @selectionHighlightStyle@
selectionHighlightStyleSelector :: Selector '[] NSTableViewSelectionHighlightStyle
selectionHighlightStyleSelector = mkSelector "selectionHighlightStyle"

-- | @Selector@ for @setSelectionHighlightStyle:@
setSelectionHighlightStyleSelector :: Selector '[NSTableViewSelectionHighlightStyle] ()
setSelectionHighlightStyleSelector = mkSelector "setSelectionHighlightStyle:"

-- | @Selector@ for @draggingDestinationFeedbackStyle@
draggingDestinationFeedbackStyleSelector :: Selector '[] NSTableViewDraggingDestinationFeedbackStyle
draggingDestinationFeedbackStyleSelector = mkSelector "draggingDestinationFeedbackStyle"

-- | @Selector@ for @setDraggingDestinationFeedbackStyle:@
setDraggingDestinationFeedbackStyleSelector :: Selector '[NSTableViewDraggingDestinationFeedbackStyle] ()
setDraggingDestinationFeedbackStyleSelector = mkSelector "setDraggingDestinationFeedbackStyle:"

-- | @Selector@ for @autosaveName@
autosaveNameSelector :: Selector '[] (Id NSString)
autosaveNameSelector = mkSelector "autosaveName"

-- | @Selector@ for @setAutosaveName:@
setAutosaveNameSelector :: Selector '[Id NSString] ()
setAutosaveNameSelector = mkSelector "setAutosaveName:"

-- | @Selector@ for @autosaveTableColumns@
autosaveTableColumnsSelector :: Selector '[] Bool
autosaveTableColumnsSelector = mkSelector "autosaveTableColumns"

-- | @Selector@ for @setAutosaveTableColumns:@
setAutosaveTableColumnsSelector :: Selector '[Bool] ()
setAutosaveTableColumnsSelector = mkSelector "setAutosaveTableColumns:"

-- | @Selector@ for @floatsGroupRows@
floatsGroupRowsSelector :: Selector '[] Bool
floatsGroupRowsSelector = mkSelector "floatsGroupRows"

-- | @Selector@ for @setFloatsGroupRows:@
setFloatsGroupRowsSelector :: Selector '[Bool] ()
setFloatsGroupRowsSelector = mkSelector "setFloatsGroupRows:"

-- | @Selector@ for @rowActionsVisible@
rowActionsVisibleSelector :: Selector '[] Bool
rowActionsVisibleSelector = mkSelector "rowActionsVisible"

-- | @Selector@ for @setRowActionsVisible:@
setRowActionsVisibleSelector :: Selector '[Bool] ()
setRowActionsVisibleSelector = mkSelector "setRowActionsVisible:"

-- | @Selector@ for @hiddenRowIndexes@
hiddenRowIndexesSelector :: Selector '[] (Id NSIndexSet)
hiddenRowIndexesSelector = mkSelector "hiddenRowIndexes"

-- | @Selector@ for @registeredNibsByIdentifier@
registeredNibsByIdentifierSelector :: Selector '[] (Id NSDictionary)
registeredNibsByIdentifierSelector = mkSelector "registeredNibsByIdentifier"

-- | @Selector@ for @usesStaticContents@
usesStaticContentsSelector :: Selector '[] Bool
usesStaticContentsSelector = mkSelector "usesStaticContents"

-- | @Selector@ for @setUsesStaticContents:@
setUsesStaticContentsSelector :: Selector '[Bool] ()
setUsesStaticContentsSelector = mkSelector "setUsesStaticContents:"

-- | @Selector@ for @userInterfaceLayoutDirection@
userInterfaceLayoutDirectionSelector :: Selector '[] NSUserInterfaceLayoutDirection
userInterfaceLayoutDirectionSelector = mkSelector "userInterfaceLayoutDirection"

-- | @Selector@ for @setUserInterfaceLayoutDirection:@
setUserInterfaceLayoutDirectionSelector :: Selector '[NSUserInterfaceLayoutDirection] ()
setUserInterfaceLayoutDirectionSelector = mkSelector "setUserInterfaceLayoutDirection:"

-- | @Selector@ for @usesAutomaticRowHeights@
usesAutomaticRowHeightsSelector :: Selector '[] Bool
usesAutomaticRowHeightsSelector = mkSelector "usesAutomaticRowHeights"

-- | @Selector@ for @setUsesAutomaticRowHeights:@
setUsesAutomaticRowHeightsSelector :: Selector '[Bool] ()
setUsesAutomaticRowHeightsSelector = mkSelector "setUsesAutomaticRowHeights:"

