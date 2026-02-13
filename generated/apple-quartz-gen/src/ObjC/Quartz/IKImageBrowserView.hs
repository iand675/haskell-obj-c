{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | IKImageBrowserView
--
-- An IKImageBrowserView object is a view that display and browse images and movies. It supports scrolling and zooming.
--
-- The IKImageBrowserView is deprecated. Please switch to NSCollectionView.
--
-- Generated bindings for @IKImageBrowserView@.
module ObjC.Quartz.IKImageBrowserView
  ( IKImageBrowserView
  , IsIKImageBrowserView(..)
  , setDraggingDestinationDelegate
  , draggingDestinationDelegate
  , indexAtLocationOfDroppedItem
  , setAllowsDroppingOnItems
  , allowsDroppingOnItems
  , selectionIndexes
  , setSelectionIndexes_byExtendingSelection
  , setAllowsMultipleSelection
  , allowsMultipleSelection
  , setAllowsEmptySelection
  , allowsEmptySelection
  , setAllowsReordering
  , allowsReordering
  , setAnimates
  , animates
  , expandGroupAtIndex
  , collapseGroupAtIndex
  , isGroupExpandedAtIndex
  , setZoomValue
  , zoomValue
  , setContentResizingMask
  , contentResizingMask
  , scrollIndexToVisible
  , setCellSize
  , cellSize
  , intercellSpacing
  , setIntercellSpacing
  , indexOfItemAtPoint
  , itemFrameAtIndex
  , visibleItemIndexes
  , rowIndexesInRect
  , columnIndexesInRect
  , rectOfColumn
  , rectOfRow
  , numberOfRows
  , numberOfColumns
  , setCanControlQuickLookPanel
  , canControlQuickLookPanel
  , setCellsStyleMask
  , cellsStyleMask
  , setConstrainsToOriginalSize
  , constrainsToOriginalSize
  , setBackgroundLayer
  , backgroundLayer
  , setForegroundLayer
  , foregroundLayer
  , newCellForRepresentedItem
  , cellForItemAtIndex
  , initWithFrame
  , reloadData
  , dataSource
  , setDataSource
  , delegate
  , setDelegate
  , allowsDroppingOnItemsSelector
  , allowsEmptySelectionSelector
  , allowsMultipleSelectionSelector
  , allowsReorderingSelector
  , animatesSelector
  , backgroundLayerSelector
  , canControlQuickLookPanelSelector
  , cellForItemAtIndexSelector
  , cellSizeSelector
  , cellsStyleMaskSelector
  , collapseGroupAtIndexSelector
  , columnIndexesInRectSelector
  , constrainsToOriginalSizeSelector
  , contentResizingMaskSelector
  , dataSourceSelector
  , delegateSelector
  , draggingDestinationDelegateSelector
  , expandGroupAtIndexSelector
  , foregroundLayerSelector
  , indexAtLocationOfDroppedItemSelector
  , indexOfItemAtPointSelector
  , initWithFrameSelector
  , intercellSpacingSelector
  , isGroupExpandedAtIndexSelector
  , itemFrameAtIndexSelector
  , newCellForRepresentedItemSelector
  , numberOfColumnsSelector
  , numberOfRowsSelector
  , rectOfColumnSelector
  , rectOfRowSelector
  , reloadDataSelector
  , rowIndexesInRectSelector
  , scrollIndexToVisibleSelector
  , selectionIndexesSelector
  , setAllowsDroppingOnItemsSelector
  , setAllowsEmptySelectionSelector
  , setAllowsMultipleSelectionSelector
  , setAllowsReorderingSelector
  , setAnimatesSelector
  , setBackgroundLayerSelector
  , setCanControlQuickLookPanelSelector
  , setCellSizeSelector
  , setCellsStyleMaskSelector
  , setConstrainsToOriginalSizeSelector
  , setContentResizingMaskSelector
  , setDataSourceSelector
  , setDelegateSelector
  , setDraggingDestinationDelegateSelector
  , setForegroundLayerSelector
  , setIntercellSpacingSelector
  , setSelectionIndexes_byExtendingSelectionSelector
  , setZoomValueSelector
  , visibleItemIndexesSelector
  , zoomValueSelector


  ) where

import Foreign.Ptr (Ptr, FunPtr)
import Foreign.C.Types

import ObjC.Runtime.Types
import ObjC.Runtime.Message (sendMessage, sendOwnedMessage, sendClassMessage, sendOwnedClassMessage)
import ObjC.Runtime.Selector (mkSelector)
import ObjC.Runtime.Class (getRequiredClass)

import ObjC.Quartz.Internal.Classes
import ObjC.Foundation.Internal.Structs
import ObjC.AppKit.Internal.Classes
import ObjC.Foundation.Internal.Classes
import ObjC.QuartzCore.Internal.Classes

-- | setDraggingDestinationDelegate:
--
-- Sets the receiver's dragging destination delegate to delegate.
--
-- ObjC selector: @- setDraggingDestinationDelegate:@
setDraggingDestinationDelegate :: IsIKImageBrowserView ikImageBrowserView => ikImageBrowserView -> RawId -> IO ()
setDraggingDestinationDelegate ikImageBrowserView delegate =
  sendMessage ikImageBrowserView setDraggingDestinationDelegateSelector delegate

-- | draggingDestinationDelegate
--
-- Returns the receiver's dragging destination delegate.
--
-- ObjC selector: @- draggingDestinationDelegate@
draggingDestinationDelegate :: IsIKImageBrowserView ikImageBrowserView => ikImageBrowserView -> IO RawId
draggingDestinationDelegate ikImageBrowserView =
  sendMessage ikImageBrowserView draggingDestinationDelegateSelector

-- | indexAtLocationOfDroppedItem
--
-- Returns the index of the cell where the drop operation occured. This index is valid when a drop occurred and until next drop.
--
-- ObjC selector: @- indexAtLocationOfDroppedItem@
indexAtLocationOfDroppedItem :: IsIKImageBrowserView ikImageBrowserView => ikImageBrowserView -> IO CULong
indexAtLocationOfDroppedItem ikImageBrowserView =
  sendMessage ikImageBrowserView indexAtLocationOfDroppedItemSelector

-- | setAllowsDroppingOnItems:
--
-- Controls whether the user can drop on items. Default is NO.
--
-- ObjC selector: @- setAllowsDroppingOnItems:@
setAllowsDroppingOnItems :: IsIKImageBrowserView ikImageBrowserView => ikImageBrowserView -> Bool -> IO ()
setAllowsDroppingOnItems ikImageBrowserView flag =
  sendMessage ikImageBrowserView setAllowsDroppingOnItemsSelector flag

-- | allowsDroppingOnItems
--
-- Returns YES if the receiver allows the user to drop on items, NO otherwise.
--
-- ObjC selector: @- allowsDroppingOnItems@
allowsDroppingOnItems :: IsIKImageBrowserView ikImageBrowserView => ikImageBrowserView -> IO Bool
allowsDroppingOnItems ikImageBrowserView =
  sendMessage ikImageBrowserView allowsDroppingOnItemsSelector

-- | selectionIndexes
--
-- Returns the indexes of the selected cells
--
-- ObjC selector: @- selectionIndexes@
selectionIndexes :: IsIKImageBrowserView ikImageBrowserView => ikImageBrowserView -> IO (Id NSIndexSet)
selectionIndexes ikImageBrowserView =
  sendMessage ikImageBrowserView selectionIndexesSelector

-- | setSelectionIndexes:byExtendingSelection:
--
-- Selects cells at indexes indexes. If extendSelection is YES it extends the current selection, otherwise it replaces the current selection.
--
-- ObjC selector: @- setSelectionIndexes:byExtendingSelection:@
setSelectionIndexes_byExtendingSelection :: (IsIKImageBrowserView ikImageBrowserView, IsNSIndexSet indexes) => ikImageBrowserView -> indexes -> Bool -> IO ()
setSelectionIndexes_byExtendingSelection ikImageBrowserView indexes extendSelection =
  sendMessage ikImageBrowserView setSelectionIndexes_byExtendingSelectionSelector (toNSIndexSet indexes) extendSelection

-- | setAllowsMultipleSelection:
--
-- Controls whether the user can select more than one cell at a time.
--
-- ObjC selector: @- setAllowsMultipleSelection:@
setAllowsMultipleSelection :: IsIKImageBrowserView ikImageBrowserView => ikImageBrowserView -> Bool -> IO ()
setAllowsMultipleSelection ikImageBrowserView flag =
  sendMessage ikImageBrowserView setAllowsMultipleSelectionSelector flag

-- | allowsMultipleSelection
--
-- Returns YES if the receiver allows the user to select more than one cell at a time, NO otherwise.
--
-- ObjC selector: @- allowsMultipleSelection@
allowsMultipleSelection :: IsIKImageBrowserView ikImageBrowserView => ikImageBrowserView -> IO Bool
allowsMultipleSelection ikImageBrowserView =
  sendMessage ikImageBrowserView allowsMultipleSelectionSelector

-- | setAllowsMultipleSelection:
--
-- Controls whether the receiver allows zero cell to be selected.
--
-- ObjC selector: @- setAllowsEmptySelection:@
setAllowsEmptySelection :: IsIKImageBrowserView ikImageBrowserView => ikImageBrowserView -> Bool -> IO ()
setAllowsEmptySelection ikImageBrowserView flag =
  sendMessage ikImageBrowserView setAllowsEmptySelectionSelector flag

-- | setAllowsEmptySelection
--
-- Returns YES if the receiver allows the user to select zero cell, NO otherwise.
--
-- ObjC selector: @- allowsEmptySelection@
allowsEmptySelection :: IsIKImageBrowserView ikImageBrowserView => ikImageBrowserView -> IO Bool
allowsEmptySelection ikImageBrowserView =
  sendMessage ikImageBrowserView allowsEmptySelectionSelector

-- | setAllowsReordering:
--
-- Controls whether the user can reorder items.
--
-- ObjC selector: @- setAllowsReordering:@
setAllowsReordering :: IsIKImageBrowserView ikImageBrowserView => ikImageBrowserView -> Bool -> IO ()
setAllowsReordering ikImageBrowserView flag =
  sendMessage ikImageBrowserView setAllowsReorderingSelector flag

-- | allowsReordering
--
-- Returns YES if the receiver allows the user to reorder items, NO otherwise.
--
-- ObjC selector: @- allowsReordering@
allowsReordering :: IsIKImageBrowserView ikImageBrowserView => ikImageBrowserView -> IO Bool
allowsReordering ikImageBrowserView =
  sendMessage ikImageBrowserView allowsReorderingSelector

-- | setAnimates:
--
-- Controls whether the receiver animate reordering and changes of the data source.
--
-- ObjC selector: @- setAnimates:@
setAnimates :: IsIKImageBrowserView ikImageBrowserView => ikImageBrowserView -> Bool -> IO ()
setAnimates ikImageBrowserView flag =
  sendMessage ikImageBrowserView setAnimatesSelector flag

-- | animates
--
-- Returns YES if the receiver animate changes of the data source, NO otherwise.
--
-- ObjC selector: @- animates@
animates :: IsIKImageBrowserView ikImageBrowserView => ikImageBrowserView -> IO Bool
animates ikImageBrowserView =
  sendMessage ikImageBrowserView animatesSelector

-- | expandGroupAtIndex:
--
-- Expands group at index 'index' if it is not already expanded; otherwise, does nothing.
--
-- ObjC selector: @- expandGroupAtIndex:@
expandGroupAtIndex :: IsIKImageBrowserView ikImageBrowserView => ikImageBrowserView -> CULong -> IO ()
expandGroupAtIndex ikImageBrowserView index =
  sendMessage ikImageBrowserView expandGroupAtIndexSelector index

-- | collapseGroupAtIndex:
--
-- Collapse group at index 'index' if it is expanded; otherwise, does nothing.
--
-- ObjC selector: @- collapseGroupAtIndex:@
collapseGroupAtIndex :: IsIKImageBrowserView ikImageBrowserView => ikImageBrowserView -> CULong -> IO ()
collapseGroupAtIndex ikImageBrowserView index =
  sendMessage ikImageBrowserView collapseGroupAtIndexSelector index

-- | isGroupExpandedAtIndex:
--
-- Returns YES if the group at index 'index' is expanded.
--
-- ObjC selector: @- isGroupExpandedAtIndex:@
isGroupExpandedAtIndex :: IsIKImageBrowserView ikImageBrowserView => ikImageBrowserView -> CULong -> IO Bool
isGroupExpandedAtIndex ikImageBrowserView index =
  sendMessage ikImageBrowserView isGroupExpandedAtIndexSelector index

-- | setZoomValue:
--
-- Sets the zoom value to aValue.
--
-- This value should be greater or equal to zero and less or equal than one. A zoom value of zero corresponds to the minimum size (40x40 pixels), A zoom value of one means images fit the browser bounds. Other values are interpolated.
--
-- ObjC selector: @- setZoomValue:@
setZoomValue :: IsIKImageBrowserView ikImageBrowserView => ikImageBrowserView -> CFloat -> IO ()
setZoomValue ikImageBrowserView aValue =
  sendMessage ikImageBrowserView setZoomValueSelector aValue

-- | zoomValue
--
-- Returns the current zoom value.
--
-- ObjC selector: @- zoomValue@
zoomValue :: IsIKImageBrowserView ikImageBrowserView => ikImageBrowserView -> IO CFloat
zoomValue ikImageBrowserView =
  sendMessage ikImageBrowserView zoomValueSelector

-- | setContentResizingMask
--
-- Determines how the receiver resize its content when zooming.
--
-- mask can be specified by combining any of the following options using the C bitwise OR operator:NSViewWidthSizable NSViewHeightSizable, other values are ignored.
--
-- ObjC selector: @- setContentResizingMask:@
setContentResizingMask :: IsIKImageBrowserView ikImageBrowserView => ikImageBrowserView -> CULong -> IO ()
setContentResizingMask ikImageBrowserView mask =
  sendMessage ikImageBrowserView setContentResizingMaskSelector mask

-- | setContentResizingMask
--
-- Returns the receiver�s content resizing mask, which determines how it�s content is resized while zooming.
--
-- ObjC selector: @- contentResizingMask@
contentResizingMask :: IsIKImageBrowserView ikImageBrowserView => ikImageBrowserView -> IO CULong
contentResizingMask ikImageBrowserView =
  sendMessage ikImageBrowserView contentResizingMaskSelector

-- | scrollIndexToVisible:
--
-- Scrolls the receiver so the item at the specified index is visible.
--
-- ObjC selector: @- scrollIndexToVisible:@
scrollIndexToVisible :: IsIKImageBrowserView ikImageBrowserView => ikImageBrowserView -> CLong -> IO ()
scrollIndexToVisible ikImageBrowserView index =
  sendMessage ikImageBrowserView scrollIndexToVisibleSelector index

-- | setCellSize:
--
-- sets the size of the cells to size
--
-- ObjC selector: @- setCellSize:@
setCellSize :: IsIKImageBrowserView ikImageBrowserView => ikImageBrowserView -> NSSize -> IO ()
setCellSize ikImageBrowserView size =
  sendMessage ikImageBrowserView setCellSizeSelector size

-- | setCellSize:
--
-- Returns the size of the cells
--
-- ObjC selector: @- cellSize@
cellSize :: IsIKImageBrowserView ikImageBrowserView => ikImageBrowserView -> IO NSSize
cellSize ikImageBrowserView =
  sendMessage ikImageBrowserView cellSizeSelector

-- | intercellSpacing
--
-- Returns the spacing between cells in the image browser.
--
-- ObjC selector: @- intercellSpacing@
intercellSpacing :: IsIKImageBrowserView ikImageBrowserView => ikImageBrowserView -> IO NSSize
intercellSpacing ikImageBrowserView =
  sendMessage ikImageBrowserView intercellSpacingSelector

-- | setIntercellSpacing:
--
-- Sets the spacing between cells in the matrix.
--
-- @aSize@ — The vertical and horizontal spacing to use between cells in the receiver. By default, both values are 10.0 in the receiver�s coordinate system.
--
-- ObjC selector: @- setIntercellSpacing:@
setIntercellSpacing :: IsIKImageBrowserView ikImageBrowserView => ikImageBrowserView -> NSSize -> IO ()
setIntercellSpacing ikImageBrowserView aSize =
  sendMessage ikImageBrowserView setIntercellSpacingSelector aSize

-- | indexOfItemAtPoint:
--
-- Returns the item at the specified location or NSNotFound if no item at this location.
--
-- ObjC selector: @- indexOfItemAtPoint:@
indexOfItemAtPoint :: IsIKImageBrowserView ikImageBrowserView => ikImageBrowserView -> NSPoint -> IO CLong
indexOfItemAtPoint ikImageBrowserView point =
  sendMessage ikImageBrowserView indexOfItemAtPointSelector point

-- | itemFrameAtIndex:
--
-- Returns the frame rectangle of the item that would be drawn at the specified location.
--
-- ObjC selector: @- itemFrameAtIndex:@
itemFrameAtIndex :: IsIKImageBrowserView ikImageBrowserView => ikImageBrowserView -> CLong -> IO NSRect
itemFrameAtIndex ikImageBrowserView index =
  sendMessage ikImageBrowserView itemFrameAtIndexSelector index

-- | visibleItemIndexes
--
-- Returns indexes of the receiver�s currently visible items.
--
-- ObjC selector: @- visibleItemIndexes@
visibleItemIndexes :: IsIKImageBrowserView ikImageBrowserView => ikImageBrowserView -> IO (Id NSIndexSet)
visibleItemIndexes ikImageBrowserView =
  sendMessage ikImageBrowserView visibleItemIndexesSelector

-- | rowIndexesInRect:
--
-- Returns the indexes of the receiver�s rows that intersect the specified rectangle.
--
-- ObjC selector: @- rowIndexesInRect:@
rowIndexesInRect :: IsIKImageBrowserView ikImageBrowserView => ikImageBrowserView -> NSRect -> IO (Id NSIndexSet)
rowIndexesInRect ikImageBrowserView rect =
  sendMessage ikImageBrowserView rowIndexesInRectSelector rect

-- | columnIndexesInRect:
--
-- Returns the indexes of the receiver�s columns that intersect the specified rectangle.
--
-- ObjC selector: @- columnIndexesInRect:@
columnIndexesInRect :: IsIKImageBrowserView ikImageBrowserView => ikImageBrowserView -> NSRect -> IO (Id NSIndexSet)
columnIndexesInRect ikImageBrowserView rect =
  sendMessage ikImageBrowserView columnIndexesInRectSelector rect

-- | rectOfColumn:
--
-- Returns the rectangle containing the column at a given index.
--
-- @columnIndex@ — The index of a column in the receiver.
--
-- ObjC selector: @- rectOfColumn:@
rectOfColumn :: IsIKImageBrowserView ikImageBrowserView => ikImageBrowserView -> CULong -> IO NSRect
rectOfColumn ikImageBrowserView columnIndex =
  sendMessage ikImageBrowserView rectOfColumnSelector columnIndex

-- | rectOfRow:
--
-- Returns the rectangle containing the row at a given index.
--
-- @rowIndex@ — The index of a row in the receiver.
--
-- ObjC selector: @- rectOfRow:@
rectOfRow :: IsIKImageBrowserView ikImageBrowserView => ikImageBrowserView -> CULong -> IO NSRect
rectOfRow ikImageBrowserView rowIndex =
  sendMessage ikImageBrowserView rectOfRowSelector rowIndex

-- | numberOfRows
--
-- Returns the number of rows in the receiver.
--
-- ObjC selector: @- numberOfRows@
numberOfRows :: IsIKImageBrowserView ikImageBrowserView => ikImageBrowserView -> IO CULong
numberOfRows ikImageBrowserView =
  sendMessage ikImageBrowserView numberOfRowsSelector

-- | numberOfColumns
--
-- Returns the number of columns in the receiver.
--
-- ObjC selector: @- numberOfColumns@
numberOfColumns :: IsIKImageBrowserView ikImageBrowserView => ikImageBrowserView -> IO CULong
numberOfColumns ikImageBrowserView =
  sendMessage ikImageBrowserView numberOfColumnsSelector

-- | setCanControlQuickLookPanel:
--
-- Sets whether the receiver can automatically take control of the Quick Look panel.
--
-- @flag@ — if YES, KImageBrowser can take control of the Quick Look panel automatically whenever it becomes first responder. This means that it provides the spacebar key store to open/close Quick Look panel and sets itself as the panel's datasource and delegate.
--
-- default value is NO. IKImageBrowserView's datasource items should provide file paths or URLs as their representation (see IKImageBrowserItem protocol).
--
-- ObjC selector: @- setCanControlQuickLookPanel:@
setCanControlQuickLookPanel :: IsIKImageBrowserView ikImageBrowserView => ikImageBrowserView -> Bool -> IO ()
setCanControlQuickLookPanel ikImageBrowserView flag =
  sendMessage ikImageBrowserView setCanControlQuickLookPanelSelector flag

-- | canControlQuickLookPanel
--
-- Returns a Boolean value that indicates whether the receiver can automatically take control of the Quick Look panel.
--
-- ObjC selector: @- canControlQuickLookPanel@
canControlQuickLookPanel :: IsIKImageBrowserView ikImageBrowserView => ikImageBrowserView -> IO Bool
canControlQuickLookPanel ikImageBrowserView =
  sendMessage ikImageBrowserView canControlQuickLookPanelSelector

-- | setCellsStyleMask:
--
-- Defines the cells appearance style.
--
-- @mask@ — An integer bit mask; see the discussion below.
--
-- mask can be specified by combining any of the options below using the C bitwise OR operator
--
-- ObjC selector: @- setCellsStyleMask:@
setCellsStyleMask :: IsIKImageBrowserView ikImageBrowserView => ikImageBrowserView -> CULong -> IO ()
setCellsStyleMask ikImageBrowserView mask =
  sendMessage ikImageBrowserView setCellsStyleMaskSelector mask

-- | cellsStyleMask
--
-- Return the cells appearance style mask.
--
-- ObjC selector: @- cellsStyleMask@
cellsStyleMask :: IsIKImageBrowserView ikImageBrowserView => ikImageBrowserView -> IO CULong
cellsStyleMask ikImageBrowserView =
  sendMessage ikImageBrowserView cellsStyleMaskSelector

-- | setConstrainsToOriginalSize:
--
-- Sets whether the receiver constraints the cells's image to their original size. Default is NO.
--
-- ObjC selector: @- setConstrainsToOriginalSize:@
setConstrainsToOriginalSize :: IsIKImageBrowserView ikImageBrowserView => ikImageBrowserView -> Bool -> IO ()
setConstrainsToOriginalSize ikImageBrowserView flag =
  sendMessage ikImageBrowserView setConstrainsToOriginalSizeSelector flag

-- | constrainsToOriginalSize
--
-- Returns whether the receiver constraints the cells's image to their original size.
--
-- ObjC selector: @- constrainsToOriginalSize@
constrainsToOriginalSize :: IsIKImageBrowserView ikImageBrowserView => ikImageBrowserView -> IO Bool
constrainsToOriginalSize ikImageBrowserView =
  sendMessage ikImageBrowserView constrainsToOriginalSizeSelector

-- | setBackgroundLayer:
--
-- Specifies the receiver�s background layer.
--
-- ObjC selector: @- setBackgroundLayer:@
setBackgroundLayer :: (IsIKImageBrowserView ikImageBrowserView, IsCALayer aLayer) => ikImageBrowserView -> aLayer -> IO ()
setBackgroundLayer ikImageBrowserView aLayer =
  sendMessage ikImageBrowserView setBackgroundLayerSelector (toCALayer aLayer)

-- | backgroundLayer
--
-- Provides the receiver�s background layer.
--
-- ObjC selector: @- backgroundLayer@
backgroundLayer :: IsIKImageBrowserView ikImageBrowserView => ikImageBrowserView -> IO (Id CALayer)
backgroundLayer ikImageBrowserView =
  sendMessage ikImageBrowserView backgroundLayerSelector

-- | setForegroundLayer:
--
-- Specifies the receiver�s foreground layer.
--
-- ObjC selector: @- setForegroundLayer:@
setForegroundLayer :: (IsIKImageBrowserView ikImageBrowserView, IsCALayer aLayer) => ikImageBrowserView -> aLayer -> IO ()
setForegroundLayer ikImageBrowserView aLayer =
  sendMessage ikImageBrowserView setForegroundLayerSelector (toCALayer aLayer)

-- | foregroundLayer
--
-- Provides the receiver�s foreground layer.
--
-- ObjC selector: @- foregroundLayer@
foregroundLayer :: IsIKImageBrowserView ikImageBrowserView => ikImageBrowserView -> IO (Id CALayer)
foregroundLayer ikImageBrowserView =
  sendMessage ikImageBrowserView foregroundLayerSelector

-- | newCellForRepresentedItem:
--
-- Returns the cell to use for the specified item. The returned cell should not be autoreleased.
--
-- @The@ — item that the returned cell will represent.
--
-- Subclasses can override this method to customize the appearance of the cell that will represent "anItem".
--
-- ObjC selector: @- newCellForRepresentedItem:@
newCellForRepresentedItem :: IsIKImageBrowserView ikImageBrowserView => ikImageBrowserView -> RawId -> IO (Id IKImageBrowserCell)
newCellForRepresentedItem ikImageBrowserView anItem =
  sendOwnedMessage ikImageBrowserView newCellForRepresentedItemSelector anItem

-- | cellForItemAtIndex:
--
-- Returns the cell at the specified index.
--
-- Subclasses must not override this method.
--
-- ObjC selector: @- cellForItemAtIndex:@
cellForItemAtIndex :: IsIKImageBrowserView ikImageBrowserView => ikImageBrowserView -> CULong -> IO (Id IKImageBrowserCell)
cellForItemAtIndex ikImageBrowserView index =
  sendMessage ikImageBrowserView cellForItemAtIndexSelector index

-- | initWithFrame:
--
-- Initializes and returns a newly allocated IKImageBrowserView object with a specified frame rectangle.
--
-- @frame@ — The frame rectangle for the created view object.
--
-- ObjC selector: @- initWithFrame:@
initWithFrame :: IsIKImageBrowserView ikImageBrowserView => ikImageBrowserView -> NSRect -> IO RawId
initWithFrame ikImageBrowserView frame =
  sendOwnedMessage ikImageBrowserView initWithFrameSelector frame

-- | reloadData
--
-- Marks the receiver as needing redisplay, so it will reload the data and draw the new values.
--
-- ObjC selector: @- reloadData@
reloadData :: IsIKImageBrowserView ikImageBrowserView => ikImageBrowserView -> IO ()
reloadData ikImageBrowserView =
  sendMessage ikImageBrowserView reloadDataSelector

-- | datasource
--
-- The receiver's data source. the data source is not retained by the receiver.
--
-- ObjC selector: @- dataSource@
dataSource :: IsIKImageBrowserView ikImageBrowserView => ikImageBrowserView -> IO RawId
dataSource ikImageBrowserView =
  sendMessage ikImageBrowserView dataSourceSelector

-- | datasource
--
-- The receiver's data source. the data source is not retained by the receiver.
--
-- ObjC selector: @- setDataSource:@
setDataSource :: IsIKImageBrowserView ikImageBrowserView => ikImageBrowserView -> RawId -> IO ()
setDataSource ikImageBrowserView value =
  sendMessage ikImageBrowserView setDataSourceSelector value

-- | delegate
--
-- The receiver's delegate. aDelegate is expected to implement the IKImageBrowserDelegate informal protocol.
--
-- ObjC selector: @- delegate@
delegate :: IsIKImageBrowserView ikImageBrowserView => ikImageBrowserView -> IO RawId
delegate ikImageBrowserView =
  sendMessage ikImageBrowserView delegateSelector

-- | delegate
--
-- The receiver's delegate. aDelegate is expected to implement the IKImageBrowserDelegate informal protocol.
--
-- ObjC selector: @- setDelegate:@
setDelegate :: IsIKImageBrowserView ikImageBrowserView => ikImageBrowserView -> RawId -> IO ()
setDelegate ikImageBrowserView value =
  sendMessage ikImageBrowserView setDelegateSelector value

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @setDraggingDestinationDelegate:@
setDraggingDestinationDelegateSelector :: Selector '[RawId] ()
setDraggingDestinationDelegateSelector = mkSelector "setDraggingDestinationDelegate:"

-- | @Selector@ for @draggingDestinationDelegate@
draggingDestinationDelegateSelector :: Selector '[] RawId
draggingDestinationDelegateSelector = mkSelector "draggingDestinationDelegate"

-- | @Selector@ for @indexAtLocationOfDroppedItem@
indexAtLocationOfDroppedItemSelector :: Selector '[] CULong
indexAtLocationOfDroppedItemSelector = mkSelector "indexAtLocationOfDroppedItem"

-- | @Selector@ for @setAllowsDroppingOnItems:@
setAllowsDroppingOnItemsSelector :: Selector '[Bool] ()
setAllowsDroppingOnItemsSelector = mkSelector "setAllowsDroppingOnItems:"

-- | @Selector@ for @allowsDroppingOnItems@
allowsDroppingOnItemsSelector :: Selector '[] Bool
allowsDroppingOnItemsSelector = mkSelector "allowsDroppingOnItems"

-- | @Selector@ for @selectionIndexes@
selectionIndexesSelector :: Selector '[] (Id NSIndexSet)
selectionIndexesSelector = mkSelector "selectionIndexes"

-- | @Selector@ for @setSelectionIndexes:byExtendingSelection:@
setSelectionIndexes_byExtendingSelectionSelector :: Selector '[Id NSIndexSet, Bool] ()
setSelectionIndexes_byExtendingSelectionSelector = mkSelector "setSelectionIndexes:byExtendingSelection:"

-- | @Selector@ for @setAllowsMultipleSelection:@
setAllowsMultipleSelectionSelector :: Selector '[Bool] ()
setAllowsMultipleSelectionSelector = mkSelector "setAllowsMultipleSelection:"

-- | @Selector@ for @allowsMultipleSelection@
allowsMultipleSelectionSelector :: Selector '[] Bool
allowsMultipleSelectionSelector = mkSelector "allowsMultipleSelection"

-- | @Selector@ for @setAllowsEmptySelection:@
setAllowsEmptySelectionSelector :: Selector '[Bool] ()
setAllowsEmptySelectionSelector = mkSelector "setAllowsEmptySelection:"

-- | @Selector@ for @allowsEmptySelection@
allowsEmptySelectionSelector :: Selector '[] Bool
allowsEmptySelectionSelector = mkSelector "allowsEmptySelection"

-- | @Selector@ for @setAllowsReordering:@
setAllowsReorderingSelector :: Selector '[Bool] ()
setAllowsReorderingSelector = mkSelector "setAllowsReordering:"

-- | @Selector@ for @allowsReordering@
allowsReorderingSelector :: Selector '[] Bool
allowsReorderingSelector = mkSelector "allowsReordering"

-- | @Selector@ for @setAnimates:@
setAnimatesSelector :: Selector '[Bool] ()
setAnimatesSelector = mkSelector "setAnimates:"

-- | @Selector@ for @animates@
animatesSelector :: Selector '[] Bool
animatesSelector = mkSelector "animates"

-- | @Selector@ for @expandGroupAtIndex:@
expandGroupAtIndexSelector :: Selector '[CULong] ()
expandGroupAtIndexSelector = mkSelector "expandGroupAtIndex:"

-- | @Selector@ for @collapseGroupAtIndex:@
collapseGroupAtIndexSelector :: Selector '[CULong] ()
collapseGroupAtIndexSelector = mkSelector "collapseGroupAtIndex:"

-- | @Selector@ for @isGroupExpandedAtIndex:@
isGroupExpandedAtIndexSelector :: Selector '[CULong] Bool
isGroupExpandedAtIndexSelector = mkSelector "isGroupExpandedAtIndex:"

-- | @Selector@ for @setZoomValue:@
setZoomValueSelector :: Selector '[CFloat] ()
setZoomValueSelector = mkSelector "setZoomValue:"

-- | @Selector@ for @zoomValue@
zoomValueSelector :: Selector '[] CFloat
zoomValueSelector = mkSelector "zoomValue"

-- | @Selector@ for @setContentResizingMask:@
setContentResizingMaskSelector :: Selector '[CULong] ()
setContentResizingMaskSelector = mkSelector "setContentResizingMask:"

-- | @Selector@ for @contentResizingMask@
contentResizingMaskSelector :: Selector '[] CULong
contentResizingMaskSelector = mkSelector "contentResizingMask"

-- | @Selector@ for @scrollIndexToVisible:@
scrollIndexToVisibleSelector :: Selector '[CLong] ()
scrollIndexToVisibleSelector = mkSelector "scrollIndexToVisible:"

-- | @Selector@ for @setCellSize:@
setCellSizeSelector :: Selector '[NSSize] ()
setCellSizeSelector = mkSelector "setCellSize:"

-- | @Selector@ for @cellSize@
cellSizeSelector :: Selector '[] NSSize
cellSizeSelector = mkSelector "cellSize"

-- | @Selector@ for @intercellSpacing@
intercellSpacingSelector :: Selector '[] NSSize
intercellSpacingSelector = mkSelector "intercellSpacing"

-- | @Selector@ for @setIntercellSpacing:@
setIntercellSpacingSelector :: Selector '[NSSize] ()
setIntercellSpacingSelector = mkSelector "setIntercellSpacing:"

-- | @Selector@ for @indexOfItemAtPoint:@
indexOfItemAtPointSelector :: Selector '[NSPoint] CLong
indexOfItemAtPointSelector = mkSelector "indexOfItemAtPoint:"

-- | @Selector@ for @itemFrameAtIndex:@
itemFrameAtIndexSelector :: Selector '[CLong] NSRect
itemFrameAtIndexSelector = mkSelector "itemFrameAtIndex:"

-- | @Selector@ for @visibleItemIndexes@
visibleItemIndexesSelector :: Selector '[] (Id NSIndexSet)
visibleItemIndexesSelector = mkSelector "visibleItemIndexes"

-- | @Selector@ for @rowIndexesInRect:@
rowIndexesInRectSelector :: Selector '[NSRect] (Id NSIndexSet)
rowIndexesInRectSelector = mkSelector "rowIndexesInRect:"

-- | @Selector@ for @columnIndexesInRect:@
columnIndexesInRectSelector :: Selector '[NSRect] (Id NSIndexSet)
columnIndexesInRectSelector = mkSelector "columnIndexesInRect:"

-- | @Selector@ for @rectOfColumn:@
rectOfColumnSelector :: Selector '[CULong] NSRect
rectOfColumnSelector = mkSelector "rectOfColumn:"

-- | @Selector@ for @rectOfRow:@
rectOfRowSelector :: Selector '[CULong] NSRect
rectOfRowSelector = mkSelector "rectOfRow:"

-- | @Selector@ for @numberOfRows@
numberOfRowsSelector :: Selector '[] CULong
numberOfRowsSelector = mkSelector "numberOfRows"

-- | @Selector@ for @numberOfColumns@
numberOfColumnsSelector :: Selector '[] CULong
numberOfColumnsSelector = mkSelector "numberOfColumns"

-- | @Selector@ for @setCanControlQuickLookPanel:@
setCanControlQuickLookPanelSelector :: Selector '[Bool] ()
setCanControlQuickLookPanelSelector = mkSelector "setCanControlQuickLookPanel:"

-- | @Selector@ for @canControlQuickLookPanel@
canControlQuickLookPanelSelector :: Selector '[] Bool
canControlQuickLookPanelSelector = mkSelector "canControlQuickLookPanel"

-- | @Selector@ for @setCellsStyleMask:@
setCellsStyleMaskSelector :: Selector '[CULong] ()
setCellsStyleMaskSelector = mkSelector "setCellsStyleMask:"

-- | @Selector@ for @cellsStyleMask@
cellsStyleMaskSelector :: Selector '[] CULong
cellsStyleMaskSelector = mkSelector "cellsStyleMask"

-- | @Selector@ for @setConstrainsToOriginalSize:@
setConstrainsToOriginalSizeSelector :: Selector '[Bool] ()
setConstrainsToOriginalSizeSelector = mkSelector "setConstrainsToOriginalSize:"

-- | @Selector@ for @constrainsToOriginalSize@
constrainsToOriginalSizeSelector :: Selector '[] Bool
constrainsToOriginalSizeSelector = mkSelector "constrainsToOriginalSize"

-- | @Selector@ for @setBackgroundLayer:@
setBackgroundLayerSelector :: Selector '[Id CALayer] ()
setBackgroundLayerSelector = mkSelector "setBackgroundLayer:"

-- | @Selector@ for @backgroundLayer@
backgroundLayerSelector :: Selector '[] (Id CALayer)
backgroundLayerSelector = mkSelector "backgroundLayer"

-- | @Selector@ for @setForegroundLayer:@
setForegroundLayerSelector :: Selector '[Id CALayer] ()
setForegroundLayerSelector = mkSelector "setForegroundLayer:"

-- | @Selector@ for @foregroundLayer@
foregroundLayerSelector :: Selector '[] (Id CALayer)
foregroundLayerSelector = mkSelector "foregroundLayer"

-- | @Selector@ for @newCellForRepresentedItem:@
newCellForRepresentedItemSelector :: Selector '[RawId] (Id IKImageBrowserCell)
newCellForRepresentedItemSelector = mkSelector "newCellForRepresentedItem:"

-- | @Selector@ for @cellForItemAtIndex:@
cellForItemAtIndexSelector :: Selector '[CULong] (Id IKImageBrowserCell)
cellForItemAtIndexSelector = mkSelector "cellForItemAtIndex:"

-- | @Selector@ for @initWithFrame:@
initWithFrameSelector :: Selector '[NSRect] RawId
initWithFrameSelector = mkSelector "initWithFrame:"

-- | @Selector@ for @reloadData@
reloadDataSelector :: Selector '[] ()
reloadDataSelector = mkSelector "reloadData"

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

