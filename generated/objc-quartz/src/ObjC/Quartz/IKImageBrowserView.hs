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
  , setDraggingDestinationDelegateSelector
  , draggingDestinationDelegateSelector
  , indexAtLocationOfDroppedItemSelector
  , setAllowsDroppingOnItemsSelector
  , allowsDroppingOnItemsSelector
  , selectionIndexesSelector
  , setSelectionIndexes_byExtendingSelectionSelector
  , setAllowsMultipleSelectionSelector
  , allowsMultipleSelectionSelector
  , setAllowsEmptySelectionSelector
  , allowsEmptySelectionSelector
  , setAllowsReorderingSelector
  , allowsReorderingSelector
  , setAnimatesSelector
  , animatesSelector
  , expandGroupAtIndexSelector
  , collapseGroupAtIndexSelector
  , isGroupExpandedAtIndexSelector
  , setZoomValueSelector
  , zoomValueSelector
  , setContentResizingMaskSelector
  , contentResizingMaskSelector
  , scrollIndexToVisibleSelector
  , setCellSizeSelector
  , cellSizeSelector
  , intercellSpacingSelector
  , setIntercellSpacingSelector
  , indexOfItemAtPointSelector
  , itemFrameAtIndexSelector
  , visibleItemIndexesSelector
  , rowIndexesInRectSelector
  , columnIndexesInRectSelector
  , rectOfColumnSelector
  , rectOfRowSelector
  , numberOfRowsSelector
  , numberOfColumnsSelector
  , setCanControlQuickLookPanelSelector
  , canControlQuickLookPanelSelector
  , setCellsStyleMaskSelector
  , cellsStyleMaskSelector
  , setConstrainsToOriginalSizeSelector
  , constrainsToOriginalSizeSelector
  , setBackgroundLayerSelector
  , backgroundLayerSelector
  , setForegroundLayerSelector
  , foregroundLayerSelector
  , newCellForRepresentedItemSelector
  , cellForItemAtIndexSelector
  , initWithFrameSelector
  , reloadDataSelector
  , dataSourceSelector
  , setDataSourceSelector
  , delegateSelector
  , setDelegateSelector


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
setDraggingDestinationDelegate ikImageBrowserView  delegate =
  sendMsg ikImageBrowserView (mkSelector "setDraggingDestinationDelegate:") retVoid [argPtr (castPtr (unRawId delegate) :: Ptr ())]

-- | draggingDestinationDelegate
--
-- Returns the receiver's dragging destination delegate.
--
-- ObjC selector: @- draggingDestinationDelegate@
draggingDestinationDelegate :: IsIKImageBrowserView ikImageBrowserView => ikImageBrowserView -> IO RawId
draggingDestinationDelegate ikImageBrowserView  =
  fmap (RawId . castPtr) $ sendMsg ikImageBrowserView (mkSelector "draggingDestinationDelegate") (retPtr retVoid) []

-- | indexAtLocationOfDroppedItem
--
-- Returns the index of the cell where the drop operation occured. This index is valid when a drop occurred and until next drop.
--
-- ObjC selector: @- indexAtLocationOfDroppedItem@
indexAtLocationOfDroppedItem :: IsIKImageBrowserView ikImageBrowserView => ikImageBrowserView -> IO CULong
indexAtLocationOfDroppedItem ikImageBrowserView  =
  sendMsg ikImageBrowserView (mkSelector "indexAtLocationOfDroppedItem") retCULong []

-- | setAllowsDroppingOnItems:
--
-- Controls whether the user can drop on items. Default is NO.
--
-- ObjC selector: @- setAllowsDroppingOnItems:@
setAllowsDroppingOnItems :: IsIKImageBrowserView ikImageBrowserView => ikImageBrowserView -> Bool -> IO ()
setAllowsDroppingOnItems ikImageBrowserView  flag =
  sendMsg ikImageBrowserView (mkSelector "setAllowsDroppingOnItems:") retVoid [argCULong (if flag then 1 else 0)]

-- | allowsDroppingOnItems
--
-- Returns YES if the receiver allows the user to drop on items, NO otherwise.
--
-- ObjC selector: @- allowsDroppingOnItems@
allowsDroppingOnItems :: IsIKImageBrowserView ikImageBrowserView => ikImageBrowserView -> IO Bool
allowsDroppingOnItems ikImageBrowserView  =
  fmap ((/= 0) :: CULong -> Bool) $ sendMsg ikImageBrowserView (mkSelector "allowsDroppingOnItems") retCULong []

-- | selectionIndexes
--
-- Returns the indexes of the selected cells
--
-- ObjC selector: @- selectionIndexes@
selectionIndexes :: IsIKImageBrowserView ikImageBrowserView => ikImageBrowserView -> IO (Id NSIndexSet)
selectionIndexes ikImageBrowserView  =
  sendMsg ikImageBrowserView (mkSelector "selectionIndexes") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | setSelectionIndexes:byExtendingSelection:
--
-- Selects cells at indexes indexes. If extendSelection is YES it extends the current selection, otherwise it replaces the current selection.
--
-- ObjC selector: @- setSelectionIndexes:byExtendingSelection:@
setSelectionIndexes_byExtendingSelection :: (IsIKImageBrowserView ikImageBrowserView, IsNSIndexSet indexes) => ikImageBrowserView -> indexes -> Bool -> IO ()
setSelectionIndexes_byExtendingSelection ikImageBrowserView  indexes extendSelection =
withObjCPtr indexes $ \raw_indexes ->
    sendMsg ikImageBrowserView (mkSelector "setSelectionIndexes:byExtendingSelection:") retVoid [argPtr (castPtr raw_indexes :: Ptr ()), argCULong (if extendSelection then 1 else 0)]

-- | setAllowsMultipleSelection:
--
-- Controls whether the user can select more than one cell at a time.
--
-- ObjC selector: @- setAllowsMultipleSelection:@
setAllowsMultipleSelection :: IsIKImageBrowserView ikImageBrowserView => ikImageBrowserView -> Bool -> IO ()
setAllowsMultipleSelection ikImageBrowserView  flag =
  sendMsg ikImageBrowserView (mkSelector "setAllowsMultipleSelection:") retVoid [argCULong (if flag then 1 else 0)]

-- | allowsMultipleSelection
--
-- Returns YES if the receiver allows the user to select more than one cell at a time, NO otherwise.
--
-- ObjC selector: @- allowsMultipleSelection@
allowsMultipleSelection :: IsIKImageBrowserView ikImageBrowserView => ikImageBrowserView -> IO Bool
allowsMultipleSelection ikImageBrowserView  =
  fmap ((/= 0) :: CULong -> Bool) $ sendMsg ikImageBrowserView (mkSelector "allowsMultipleSelection") retCULong []

-- | setAllowsMultipleSelection:
--
-- Controls whether the receiver allows zero cell to be selected.
--
-- ObjC selector: @- setAllowsEmptySelection:@
setAllowsEmptySelection :: IsIKImageBrowserView ikImageBrowserView => ikImageBrowserView -> Bool -> IO ()
setAllowsEmptySelection ikImageBrowserView  flag =
  sendMsg ikImageBrowserView (mkSelector "setAllowsEmptySelection:") retVoid [argCULong (if flag then 1 else 0)]

-- | setAllowsEmptySelection
--
-- Returns YES if the receiver allows the user to select zero cell, NO otherwise.
--
-- ObjC selector: @- allowsEmptySelection@
allowsEmptySelection :: IsIKImageBrowserView ikImageBrowserView => ikImageBrowserView -> IO Bool
allowsEmptySelection ikImageBrowserView  =
  fmap ((/= 0) :: CULong -> Bool) $ sendMsg ikImageBrowserView (mkSelector "allowsEmptySelection") retCULong []

-- | setAllowsReordering:
--
-- Controls whether the user can reorder items.
--
-- ObjC selector: @- setAllowsReordering:@
setAllowsReordering :: IsIKImageBrowserView ikImageBrowserView => ikImageBrowserView -> Bool -> IO ()
setAllowsReordering ikImageBrowserView  flag =
  sendMsg ikImageBrowserView (mkSelector "setAllowsReordering:") retVoid [argCULong (if flag then 1 else 0)]

-- | allowsReordering
--
-- Returns YES if the receiver allows the user to reorder items, NO otherwise.
--
-- ObjC selector: @- allowsReordering@
allowsReordering :: IsIKImageBrowserView ikImageBrowserView => ikImageBrowserView -> IO Bool
allowsReordering ikImageBrowserView  =
  fmap ((/= 0) :: CULong -> Bool) $ sendMsg ikImageBrowserView (mkSelector "allowsReordering") retCULong []

-- | setAnimates:
--
-- Controls whether the receiver animate reordering and changes of the data source.
--
-- ObjC selector: @- setAnimates:@
setAnimates :: IsIKImageBrowserView ikImageBrowserView => ikImageBrowserView -> Bool -> IO ()
setAnimates ikImageBrowserView  flag =
  sendMsg ikImageBrowserView (mkSelector "setAnimates:") retVoid [argCULong (if flag then 1 else 0)]

-- | animates
--
-- Returns YES if the receiver animate changes of the data source, NO otherwise.
--
-- ObjC selector: @- animates@
animates :: IsIKImageBrowserView ikImageBrowserView => ikImageBrowserView -> IO Bool
animates ikImageBrowserView  =
  fmap ((/= 0) :: CULong -> Bool) $ sendMsg ikImageBrowserView (mkSelector "animates") retCULong []

-- | expandGroupAtIndex:
--
-- Expands group at index 'index' if it is not already expanded; otherwise, does nothing.
--
-- ObjC selector: @- expandGroupAtIndex:@
expandGroupAtIndex :: IsIKImageBrowserView ikImageBrowserView => ikImageBrowserView -> CULong -> IO ()
expandGroupAtIndex ikImageBrowserView  index =
  sendMsg ikImageBrowserView (mkSelector "expandGroupAtIndex:") retVoid [argCULong (fromIntegral index)]

-- | collapseGroupAtIndex:
--
-- Collapse group at index 'index' if it is expanded; otherwise, does nothing.
--
-- ObjC selector: @- collapseGroupAtIndex:@
collapseGroupAtIndex :: IsIKImageBrowserView ikImageBrowserView => ikImageBrowserView -> CULong -> IO ()
collapseGroupAtIndex ikImageBrowserView  index =
  sendMsg ikImageBrowserView (mkSelector "collapseGroupAtIndex:") retVoid [argCULong (fromIntegral index)]

-- | isGroupExpandedAtIndex:
--
-- Returns YES if the group at index 'index' is expanded.
--
-- ObjC selector: @- isGroupExpandedAtIndex:@
isGroupExpandedAtIndex :: IsIKImageBrowserView ikImageBrowserView => ikImageBrowserView -> CULong -> IO Bool
isGroupExpandedAtIndex ikImageBrowserView  index =
  fmap ((/= 0) :: CULong -> Bool) $ sendMsg ikImageBrowserView (mkSelector "isGroupExpandedAtIndex:") retCULong [argCULong (fromIntegral index)]

-- | setZoomValue:
--
-- Sets the zoom value to aValue.
--
-- This value should be greater or equal to zero and less or equal than one. A zoom value of zero corresponds to the minimum size (40x40 pixels), A zoom value of one means images fit the browser bounds. Other values are interpolated.
--
-- ObjC selector: @- setZoomValue:@
setZoomValue :: IsIKImageBrowserView ikImageBrowserView => ikImageBrowserView -> CFloat -> IO ()
setZoomValue ikImageBrowserView  aValue =
  sendMsg ikImageBrowserView (mkSelector "setZoomValue:") retVoid [argCFloat (fromIntegral aValue)]

-- | zoomValue
--
-- Returns the current zoom value.
--
-- ObjC selector: @- zoomValue@
zoomValue :: IsIKImageBrowserView ikImageBrowserView => ikImageBrowserView -> IO CFloat
zoomValue ikImageBrowserView  =
  sendMsg ikImageBrowserView (mkSelector "zoomValue") retCFloat []

-- | setContentResizingMask
--
-- Determines how the receiver resize its content when zooming.
--
-- mask can be specified by combining any of the following options using the C bitwise OR operator:NSViewWidthSizable NSViewHeightSizable, other values are ignored.
--
-- ObjC selector: @- setContentResizingMask:@
setContentResizingMask :: IsIKImageBrowserView ikImageBrowserView => ikImageBrowserView -> CULong -> IO ()
setContentResizingMask ikImageBrowserView  mask =
  sendMsg ikImageBrowserView (mkSelector "setContentResizingMask:") retVoid [argCULong (fromIntegral mask)]

-- | setContentResizingMask
--
-- Returns the receiver�s content resizing mask, which determines how it�s content is resized while zooming.
--
-- ObjC selector: @- contentResizingMask@
contentResizingMask :: IsIKImageBrowserView ikImageBrowserView => ikImageBrowserView -> IO CULong
contentResizingMask ikImageBrowserView  =
  sendMsg ikImageBrowserView (mkSelector "contentResizingMask") retCULong []

-- | scrollIndexToVisible:
--
-- Scrolls the receiver so the item at the specified index is visible.
--
-- ObjC selector: @- scrollIndexToVisible:@
scrollIndexToVisible :: IsIKImageBrowserView ikImageBrowserView => ikImageBrowserView -> CLong -> IO ()
scrollIndexToVisible ikImageBrowserView  index =
  sendMsg ikImageBrowserView (mkSelector "scrollIndexToVisible:") retVoid [argCLong (fromIntegral index)]

-- | setCellSize:
--
-- sets the size of the cells to size
--
-- ObjC selector: @- setCellSize:@
setCellSize :: IsIKImageBrowserView ikImageBrowserView => ikImageBrowserView -> NSSize -> IO ()
setCellSize ikImageBrowserView  size =
  sendMsg ikImageBrowserView (mkSelector "setCellSize:") retVoid [argNSSize size]

-- | setCellSize:
--
-- Returns the size of the cells
--
-- ObjC selector: @- cellSize@
cellSize :: IsIKImageBrowserView ikImageBrowserView => ikImageBrowserView -> IO NSSize
cellSize ikImageBrowserView  =
  sendMsgStret ikImageBrowserView (mkSelector "cellSize") retNSSize []

-- | intercellSpacing
--
-- Returns the spacing between cells in the image browser.
--
-- ObjC selector: @- intercellSpacing@
intercellSpacing :: IsIKImageBrowserView ikImageBrowserView => ikImageBrowserView -> IO NSSize
intercellSpacing ikImageBrowserView  =
  sendMsgStret ikImageBrowserView (mkSelector "intercellSpacing") retNSSize []

-- | setIntercellSpacing:
--
-- Sets the spacing between cells in the matrix.
--
-- @aSize@ — The vertical and horizontal spacing to use between cells in the receiver. By default, both values are 10.0 in the receiver�s coordinate system.
--
-- ObjC selector: @- setIntercellSpacing:@
setIntercellSpacing :: IsIKImageBrowserView ikImageBrowserView => ikImageBrowserView -> NSSize -> IO ()
setIntercellSpacing ikImageBrowserView  aSize =
  sendMsg ikImageBrowserView (mkSelector "setIntercellSpacing:") retVoid [argNSSize aSize]

-- | indexOfItemAtPoint:
--
-- Returns the item at the specified location or NSNotFound if no item at this location.
--
-- ObjC selector: @- indexOfItemAtPoint:@
indexOfItemAtPoint :: IsIKImageBrowserView ikImageBrowserView => ikImageBrowserView -> NSPoint -> IO CLong
indexOfItemAtPoint ikImageBrowserView  point =
  sendMsg ikImageBrowserView (mkSelector "indexOfItemAtPoint:") retCLong [argNSPoint point]

-- | itemFrameAtIndex:
--
-- Returns the frame rectangle of the item that would be drawn at the specified location.
--
-- ObjC selector: @- itemFrameAtIndex:@
itemFrameAtIndex :: IsIKImageBrowserView ikImageBrowserView => ikImageBrowserView -> CLong -> IO NSRect
itemFrameAtIndex ikImageBrowserView  index =
  sendMsgStret ikImageBrowserView (mkSelector "itemFrameAtIndex:") retNSRect [argCLong (fromIntegral index)]

-- | visibleItemIndexes
--
-- Returns indexes of the receiver�s currently visible items.
--
-- ObjC selector: @- visibleItemIndexes@
visibleItemIndexes :: IsIKImageBrowserView ikImageBrowserView => ikImageBrowserView -> IO (Id NSIndexSet)
visibleItemIndexes ikImageBrowserView  =
  sendMsg ikImageBrowserView (mkSelector "visibleItemIndexes") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | rowIndexesInRect:
--
-- Returns the indexes of the receiver�s rows that intersect the specified rectangle.
--
-- ObjC selector: @- rowIndexesInRect:@
rowIndexesInRect :: IsIKImageBrowserView ikImageBrowserView => ikImageBrowserView -> NSRect -> IO (Id NSIndexSet)
rowIndexesInRect ikImageBrowserView  rect =
  sendMsg ikImageBrowserView (mkSelector "rowIndexesInRect:") (retPtr retVoid) [argNSRect rect] >>= retainedObject . castPtr

-- | columnIndexesInRect:
--
-- Returns the indexes of the receiver�s columns that intersect the specified rectangle.
--
-- ObjC selector: @- columnIndexesInRect:@
columnIndexesInRect :: IsIKImageBrowserView ikImageBrowserView => ikImageBrowserView -> NSRect -> IO (Id NSIndexSet)
columnIndexesInRect ikImageBrowserView  rect =
  sendMsg ikImageBrowserView (mkSelector "columnIndexesInRect:") (retPtr retVoid) [argNSRect rect] >>= retainedObject . castPtr

-- | rectOfColumn:
--
-- Returns the rectangle containing the column at a given index.
--
-- @columnIndex@ — The index of a column in the receiver.
--
-- ObjC selector: @- rectOfColumn:@
rectOfColumn :: IsIKImageBrowserView ikImageBrowserView => ikImageBrowserView -> CULong -> IO NSRect
rectOfColumn ikImageBrowserView  columnIndex =
  sendMsgStret ikImageBrowserView (mkSelector "rectOfColumn:") retNSRect [argCULong (fromIntegral columnIndex)]

-- | rectOfRow:
--
-- Returns the rectangle containing the row at a given index.
--
-- @rowIndex@ — The index of a row in the receiver.
--
-- ObjC selector: @- rectOfRow:@
rectOfRow :: IsIKImageBrowserView ikImageBrowserView => ikImageBrowserView -> CULong -> IO NSRect
rectOfRow ikImageBrowserView  rowIndex =
  sendMsgStret ikImageBrowserView (mkSelector "rectOfRow:") retNSRect [argCULong (fromIntegral rowIndex)]

-- | numberOfRows
--
-- Returns the number of rows in the receiver.
--
-- ObjC selector: @- numberOfRows@
numberOfRows :: IsIKImageBrowserView ikImageBrowserView => ikImageBrowserView -> IO CULong
numberOfRows ikImageBrowserView  =
  sendMsg ikImageBrowserView (mkSelector "numberOfRows") retCULong []

-- | numberOfColumns
--
-- Returns the number of columns in the receiver.
--
-- ObjC selector: @- numberOfColumns@
numberOfColumns :: IsIKImageBrowserView ikImageBrowserView => ikImageBrowserView -> IO CULong
numberOfColumns ikImageBrowserView  =
  sendMsg ikImageBrowserView (mkSelector "numberOfColumns") retCULong []

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
setCanControlQuickLookPanel ikImageBrowserView  flag =
  sendMsg ikImageBrowserView (mkSelector "setCanControlQuickLookPanel:") retVoid [argCULong (if flag then 1 else 0)]

-- | canControlQuickLookPanel
--
-- Returns a Boolean value that indicates whether the receiver can automatically take control of the Quick Look panel.
--
-- ObjC selector: @- canControlQuickLookPanel@
canControlQuickLookPanel :: IsIKImageBrowserView ikImageBrowserView => ikImageBrowserView -> IO Bool
canControlQuickLookPanel ikImageBrowserView  =
  fmap ((/= 0) :: CULong -> Bool) $ sendMsg ikImageBrowserView (mkSelector "canControlQuickLookPanel") retCULong []

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
setCellsStyleMask ikImageBrowserView  mask =
  sendMsg ikImageBrowserView (mkSelector "setCellsStyleMask:") retVoid [argCULong (fromIntegral mask)]

-- | cellsStyleMask
--
-- Return the cells appearance style mask.
--
-- ObjC selector: @- cellsStyleMask@
cellsStyleMask :: IsIKImageBrowserView ikImageBrowserView => ikImageBrowserView -> IO CULong
cellsStyleMask ikImageBrowserView  =
  sendMsg ikImageBrowserView (mkSelector "cellsStyleMask") retCULong []

-- | setConstrainsToOriginalSize:
--
-- Sets whether the receiver constraints the cells's image to their original size. Default is NO.
--
-- ObjC selector: @- setConstrainsToOriginalSize:@
setConstrainsToOriginalSize :: IsIKImageBrowserView ikImageBrowserView => ikImageBrowserView -> Bool -> IO ()
setConstrainsToOriginalSize ikImageBrowserView  flag =
  sendMsg ikImageBrowserView (mkSelector "setConstrainsToOriginalSize:") retVoid [argCULong (if flag then 1 else 0)]

-- | constrainsToOriginalSize
--
-- Returns whether the receiver constraints the cells's image to their original size.
--
-- ObjC selector: @- constrainsToOriginalSize@
constrainsToOriginalSize :: IsIKImageBrowserView ikImageBrowserView => ikImageBrowserView -> IO Bool
constrainsToOriginalSize ikImageBrowserView  =
  fmap ((/= 0) :: CULong -> Bool) $ sendMsg ikImageBrowserView (mkSelector "constrainsToOriginalSize") retCULong []

-- | setBackgroundLayer:
--
-- Specifies the receiver�s background layer.
--
-- ObjC selector: @- setBackgroundLayer:@
setBackgroundLayer :: (IsIKImageBrowserView ikImageBrowserView, IsCALayer aLayer) => ikImageBrowserView -> aLayer -> IO ()
setBackgroundLayer ikImageBrowserView  aLayer =
withObjCPtr aLayer $ \raw_aLayer ->
    sendMsg ikImageBrowserView (mkSelector "setBackgroundLayer:") retVoid [argPtr (castPtr raw_aLayer :: Ptr ())]

-- | backgroundLayer
--
-- Provides the receiver�s background layer.
--
-- ObjC selector: @- backgroundLayer@
backgroundLayer :: IsIKImageBrowserView ikImageBrowserView => ikImageBrowserView -> IO (Id CALayer)
backgroundLayer ikImageBrowserView  =
  sendMsg ikImageBrowserView (mkSelector "backgroundLayer") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | setForegroundLayer:
--
-- Specifies the receiver�s foreground layer.
--
-- ObjC selector: @- setForegroundLayer:@
setForegroundLayer :: (IsIKImageBrowserView ikImageBrowserView, IsCALayer aLayer) => ikImageBrowserView -> aLayer -> IO ()
setForegroundLayer ikImageBrowserView  aLayer =
withObjCPtr aLayer $ \raw_aLayer ->
    sendMsg ikImageBrowserView (mkSelector "setForegroundLayer:") retVoid [argPtr (castPtr raw_aLayer :: Ptr ())]

-- | foregroundLayer
--
-- Provides the receiver�s foreground layer.
--
-- ObjC selector: @- foregroundLayer@
foregroundLayer :: IsIKImageBrowserView ikImageBrowserView => ikImageBrowserView -> IO (Id CALayer)
foregroundLayer ikImageBrowserView  =
  sendMsg ikImageBrowserView (mkSelector "foregroundLayer") (retPtr retVoid) [] >>= retainedObject . castPtr

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
newCellForRepresentedItem ikImageBrowserView  anItem =
  sendMsg ikImageBrowserView (mkSelector "newCellForRepresentedItem:") (retPtr retVoid) [argPtr (castPtr (unRawId anItem) :: Ptr ())] >>= ownedObject . castPtr

-- | cellForItemAtIndex:
--
-- Returns the cell at the specified index.
--
-- Subclasses must not override this method.
--
-- ObjC selector: @- cellForItemAtIndex:@
cellForItemAtIndex :: IsIKImageBrowserView ikImageBrowserView => ikImageBrowserView -> CULong -> IO (Id IKImageBrowserCell)
cellForItemAtIndex ikImageBrowserView  index =
  sendMsg ikImageBrowserView (mkSelector "cellForItemAtIndex:") (retPtr retVoid) [argCULong (fromIntegral index)] >>= retainedObject . castPtr

-- | initWithFrame:
--
-- Initializes and returns a newly allocated IKImageBrowserView object with a specified frame rectangle.
--
-- @frame@ — The frame rectangle for the created view object.
--
-- ObjC selector: @- initWithFrame:@
initWithFrame :: IsIKImageBrowserView ikImageBrowserView => ikImageBrowserView -> NSRect -> IO RawId
initWithFrame ikImageBrowserView  frame =
  fmap (RawId . castPtr) $ sendMsg ikImageBrowserView (mkSelector "initWithFrame:") (retPtr retVoid) [argNSRect frame]

-- | reloadData
--
-- Marks the receiver as needing redisplay, so it will reload the data and draw the new values.
--
-- ObjC selector: @- reloadData@
reloadData :: IsIKImageBrowserView ikImageBrowserView => ikImageBrowserView -> IO ()
reloadData ikImageBrowserView  =
  sendMsg ikImageBrowserView (mkSelector "reloadData") retVoid []

-- | datasource
--
-- The receiver's data source. the data source is not retained by the receiver.
--
-- ObjC selector: @- dataSource@
dataSource :: IsIKImageBrowserView ikImageBrowserView => ikImageBrowserView -> IO RawId
dataSource ikImageBrowserView  =
  fmap (RawId . castPtr) $ sendMsg ikImageBrowserView (mkSelector "dataSource") (retPtr retVoid) []

-- | datasource
--
-- The receiver's data source. the data source is not retained by the receiver.
--
-- ObjC selector: @- setDataSource:@
setDataSource :: IsIKImageBrowserView ikImageBrowserView => ikImageBrowserView -> RawId -> IO ()
setDataSource ikImageBrowserView  value =
  sendMsg ikImageBrowserView (mkSelector "setDataSource:") retVoid [argPtr (castPtr (unRawId value) :: Ptr ())]

-- | delegate
--
-- The receiver's delegate. aDelegate is expected to implement the IKImageBrowserDelegate informal protocol.
--
-- ObjC selector: @- delegate@
delegate :: IsIKImageBrowserView ikImageBrowserView => ikImageBrowserView -> IO RawId
delegate ikImageBrowserView  =
  fmap (RawId . castPtr) $ sendMsg ikImageBrowserView (mkSelector "delegate") (retPtr retVoid) []

-- | delegate
--
-- The receiver's delegate. aDelegate is expected to implement the IKImageBrowserDelegate informal protocol.
--
-- ObjC selector: @- setDelegate:@
setDelegate :: IsIKImageBrowserView ikImageBrowserView => ikImageBrowserView -> RawId -> IO ()
setDelegate ikImageBrowserView  value =
  sendMsg ikImageBrowserView (mkSelector "setDelegate:") retVoid [argPtr (castPtr (unRawId value) :: Ptr ())]

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @setDraggingDestinationDelegate:@
setDraggingDestinationDelegateSelector :: Selector
setDraggingDestinationDelegateSelector = mkSelector "setDraggingDestinationDelegate:"

-- | @Selector@ for @draggingDestinationDelegate@
draggingDestinationDelegateSelector :: Selector
draggingDestinationDelegateSelector = mkSelector "draggingDestinationDelegate"

-- | @Selector@ for @indexAtLocationOfDroppedItem@
indexAtLocationOfDroppedItemSelector :: Selector
indexAtLocationOfDroppedItemSelector = mkSelector "indexAtLocationOfDroppedItem"

-- | @Selector@ for @setAllowsDroppingOnItems:@
setAllowsDroppingOnItemsSelector :: Selector
setAllowsDroppingOnItemsSelector = mkSelector "setAllowsDroppingOnItems:"

-- | @Selector@ for @allowsDroppingOnItems@
allowsDroppingOnItemsSelector :: Selector
allowsDroppingOnItemsSelector = mkSelector "allowsDroppingOnItems"

-- | @Selector@ for @selectionIndexes@
selectionIndexesSelector :: Selector
selectionIndexesSelector = mkSelector "selectionIndexes"

-- | @Selector@ for @setSelectionIndexes:byExtendingSelection:@
setSelectionIndexes_byExtendingSelectionSelector :: Selector
setSelectionIndexes_byExtendingSelectionSelector = mkSelector "setSelectionIndexes:byExtendingSelection:"

-- | @Selector@ for @setAllowsMultipleSelection:@
setAllowsMultipleSelectionSelector :: Selector
setAllowsMultipleSelectionSelector = mkSelector "setAllowsMultipleSelection:"

-- | @Selector@ for @allowsMultipleSelection@
allowsMultipleSelectionSelector :: Selector
allowsMultipleSelectionSelector = mkSelector "allowsMultipleSelection"

-- | @Selector@ for @setAllowsEmptySelection:@
setAllowsEmptySelectionSelector :: Selector
setAllowsEmptySelectionSelector = mkSelector "setAllowsEmptySelection:"

-- | @Selector@ for @allowsEmptySelection@
allowsEmptySelectionSelector :: Selector
allowsEmptySelectionSelector = mkSelector "allowsEmptySelection"

-- | @Selector@ for @setAllowsReordering:@
setAllowsReorderingSelector :: Selector
setAllowsReorderingSelector = mkSelector "setAllowsReordering:"

-- | @Selector@ for @allowsReordering@
allowsReorderingSelector :: Selector
allowsReorderingSelector = mkSelector "allowsReordering"

-- | @Selector@ for @setAnimates:@
setAnimatesSelector :: Selector
setAnimatesSelector = mkSelector "setAnimates:"

-- | @Selector@ for @animates@
animatesSelector :: Selector
animatesSelector = mkSelector "animates"

-- | @Selector@ for @expandGroupAtIndex:@
expandGroupAtIndexSelector :: Selector
expandGroupAtIndexSelector = mkSelector "expandGroupAtIndex:"

-- | @Selector@ for @collapseGroupAtIndex:@
collapseGroupAtIndexSelector :: Selector
collapseGroupAtIndexSelector = mkSelector "collapseGroupAtIndex:"

-- | @Selector@ for @isGroupExpandedAtIndex:@
isGroupExpandedAtIndexSelector :: Selector
isGroupExpandedAtIndexSelector = mkSelector "isGroupExpandedAtIndex:"

-- | @Selector@ for @setZoomValue:@
setZoomValueSelector :: Selector
setZoomValueSelector = mkSelector "setZoomValue:"

-- | @Selector@ for @zoomValue@
zoomValueSelector :: Selector
zoomValueSelector = mkSelector "zoomValue"

-- | @Selector@ for @setContentResizingMask:@
setContentResizingMaskSelector :: Selector
setContentResizingMaskSelector = mkSelector "setContentResizingMask:"

-- | @Selector@ for @contentResizingMask@
contentResizingMaskSelector :: Selector
contentResizingMaskSelector = mkSelector "contentResizingMask"

-- | @Selector@ for @scrollIndexToVisible:@
scrollIndexToVisibleSelector :: Selector
scrollIndexToVisibleSelector = mkSelector "scrollIndexToVisible:"

-- | @Selector@ for @setCellSize:@
setCellSizeSelector :: Selector
setCellSizeSelector = mkSelector "setCellSize:"

-- | @Selector@ for @cellSize@
cellSizeSelector :: Selector
cellSizeSelector = mkSelector "cellSize"

-- | @Selector@ for @intercellSpacing@
intercellSpacingSelector :: Selector
intercellSpacingSelector = mkSelector "intercellSpacing"

-- | @Selector@ for @setIntercellSpacing:@
setIntercellSpacingSelector :: Selector
setIntercellSpacingSelector = mkSelector "setIntercellSpacing:"

-- | @Selector@ for @indexOfItemAtPoint:@
indexOfItemAtPointSelector :: Selector
indexOfItemAtPointSelector = mkSelector "indexOfItemAtPoint:"

-- | @Selector@ for @itemFrameAtIndex:@
itemFrameAtIndexSelector :: Selector
itemFrameAtIndexSelector = mkSelector "itemFrameAtIndex:"

-- | @Selector@ for @visibleItemIndexes@
visibleItemIndexesSelector :: Selector
visibleItemIndexesSelector = mkSelector "visibleItemIndexes"

-- | @Selector@ for @rowIndexesInRect:@
rowIndexesInRectSelector :: Selector
rowIndexesInRectSelector = mkSelector "rowIndexesInRect:"

-- | @Selector@ for @columnIndexesInRect:@
columnIndexesInRectSelector :: Selector
columnIndexesInRectSelector = mkSelector "columnIndexesInRect:"

-- | @Selector@ for @rectOfColumn:@
rectOfColumnSelector :: Selector
rectOfColumnSelector = mkSelector "rectOfColumn:"

-- | @Selector@ for @rectOfRow:@
rectOfRowSelector :: Selector
rectOfRowSelector = mkSelector "rectOfRow:"

-- | @Selector@ for @numberOfRows@
numberOfRowsSelector :: Selector
numberOfRowsSelector = mkSelector "numberOfRows"

-- | @Selector@ for @numberOfColumns@
numberOfColumnsSelector :: Selector
numberOfColumnsSelector = mkSelector "numberOfColumns"

-- | @Selector@ for @setCanControlQuickLookPanel:@
setCanControlQuickLookPanelSelector :: Selector
setCanControlQuickLookPanelSelector = mkSelector "setCanControlQuickLookPanel:"

-- | @Selector@ for @canControlQuickLookPanel@
canControlQuickLookPanelSelector :: Selector
canControlQuickLookPanelSelector = mkSelector "canControlQuickLookPanel"

-- | @Selector@ for @setCellsStyleMask:@
setCellsStyleMaskSelector :: Selector
setCellsStyleMaskSelector = mkSelector "setCellsStyleMask:"

-- | @Selector@ for @cellsStyleMask@
cellsStyleMaskSelector :: Selector
cellsStyleMaskSelector = mkSelector "cellsStyleMask"

-- | @Selector@ for @setConstrainsToOriginalSize:@
setConstrainsToOriginalSizeSelector :: Selector
setConstrainsToOriginalSizeSelector = mkSelector "setConstrainsToOriginalSize:"

-- | @Selector@ for @constrainsToOriginalSize@
constrainsToOriginalSizeSelector :: Selector
constrainsToOriginalSizeSelector = mkSelector "constrainsToOriginalSize"

-- | @Selector@ for @setBackgroundLayer:@
setBackgroundLayerSelector :: Selector
setBackgroundLayerSelector = mkSelector "setBackgroundLayer:"

-- | @Selector@ for @backgroundLayer@
backgroundLayerSelector :: Selector
backgroundLayerSelector = mkSelector "backgroundLayer"

-- | @Selector@ for @setForegroundLayer:@
setForegroundLayerSelector :: Selector
setForegroundLayerSelector = mkSelector "setForegroundLayer:"

-- | @Selector@ for @foregroundLayer@
foregroundLayerSelector :: Selector
foregroundLayerSelector = mkSelector "foregroundLayer"

-- | @Selector@ for @newCellForRepresentedItem:@
newCellForRepresentedItemSelector :: Selector
newCellForRepresentedItemSelector = mkSelector "newCellForRepresentedItem:"

-- | @Selector@ for @cellForItemAtIndex:@
cellForItemAtIndexSelector :: Selector
cellForItemAtIndexSelector = mkSelector "cellForItemAtIndex:"

-- | @Selector@ for @initWithFrame:@
initWithFrameSelector :: Selector
initWithFrameSelector = mkSelector "initWithFrame:"

-- | @Selector@ for @reloadData@
reloadDataSelector :: Selector
reloadDataSelector = mkSelector "reloadData"

-- | @Selector@ for @dataSource@
dataSourceSelector :: Selector
dataSourceSelector = mkSelector "dataSource"

-- | @Selector@ for @setDataSource:@
setDataSourceSelector :: Selector
setDataSourceSelector = mkSelector "setDataSource:"

-- | @Selector@ for @delegate@
delegateSelector :: Selector
delegateSelector = mkSelector "delegate"

-- | @Selector@ for @setDelegate:@
setDelegateSelector :: Selector
setDelegateSelector = mkSelector "setDelegate:"

