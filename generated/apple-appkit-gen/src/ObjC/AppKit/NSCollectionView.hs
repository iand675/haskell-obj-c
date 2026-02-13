{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Generated bindings for @NSCollectionView@.
module ObjC.AppKit.NSCollectionView
  ( NSCollectionView
  , IsNSCollectionView(..)
  , reloadData
  , layoutAttributesForItemAtIndexPath
  , layoutAttributesForSupplementaryElementOfKind_atIndexPath
  , frameForItemAtIndex
  , frameForItemAtIndex_withNumberOfItems
  , numberOfItemsInSection
  , selectItemsAtIndexPaths_scrollPosition
  , deselectItemsAtIndexPaths
  , selectAll
  , deselectAll
  , registerClass_forItemWithIdentifier
  , registerNib_forItemWithIdentifier
  , registerClass_forSupplementaryViewOfKind_withIdentifier
  , registerNib_forSupplementaryViewOfKind_withIdentifier
  , makeItemWithIdentifier_forIndexPath
  , makeSupplementaryViewOfKind_withIdentifier_forIndexPath
  , itemAtIndex
  , itemAtIndexPath
  , visibleItems
  , indexPathsForVisibleItems
  , indexPathForItem
  , indexPathForItemAtPoint
  , supplementaryViewForElementKind_atIndexPath
  , visibleSupplementaryViewsOfKind
  , indexPathsForVisibleSupplementaryElementsOfKind
  , insertSections
  , deleteSections
  , reloadSections
  , moveSection_toSection
  , insertItemsAtIndexPaths
  , deleteItemsAtIndexPaths
  , reloadItemsAtIndexPaths
  , moveItemAtIndexPath_toIndexPath
  , performBatchUpdates_completionHandler
  , toggleSectionCollapse
  , scrollToItemsAtIndexPaths_scrollPosition
  , setDraggingSourceOperationMask_forLocal
  , draggingImageForItemsAtIndexPaths_withEvent_offset
  , draggingImageForItemsAtIndexes_withEvent_offset
  , newItemForRepresentedObject
  , dataSource
  , setDataSource
  , prefetchDataSource
  , setPrefetchDataSource
  , content
  , setContent
  , delegate
  , setDelegate
  , backgroundView
  , setBackgroundView
  , backgroundViewScrollsWithContent
  , setBackgroundViewScrollsWithContent
  , collectionViewLayout
  , setCollectionViewLayout
  , backgroundColors
  , setBackgroundColors
  , numberOfSections
  , firstResponder
  , selectable
  , setSelectable
  , allowsEmptySelection
  , setAllowsEmptySelection
  , allowsMultipleSelection
  , setAllowsMultipleSelection
  , selectionIndexes
  , setSelectionIndexes
  , selectionIndexPaths
  , setSelectionIndexPaths
  , itemPrototype
  , setItemPrototype
  , maxNumberOfRows
  , setMaxNumberOfRows
  , maxNumberOfColumns
  , setMaxNumberOfColumns
  , minItemSize
  , setMinItemSize
  , maxItemSize
  , setMaxItemSize
  , allowsEmptySelectionSelector
  , allowsMultipleSelectionSelector
  , backgroundColorsSelector
  , backgroundViewScrollsWithContentSelector
  , backgroundViewSelector
  , collectionViewLayoutSelector
  , contentSelector
  , dataSourceSelector
  , delegateSelector
  , deleteItemsAtIndexPathsSelector
  , deleteSectionsSelector
  , deselectAllSelector
  , deselectItemsAtIndexPathsSelector
  , draggingImageForItemsAtIndexPaths_withEvent_offsetSelector
  , draggingImageForItemsAtIndexes_withEvent_offsetSelector
  , firstResponderSelector
  , frameForItemAtIndexSelector
  , frameForItemAtIndex_withNumberOfItemsSelector
  , indexPathForItemAtPointSelector
  , indexPathForItemSelector
  , indexPathsForVisibleItemsSelector
  , indexPathsForVisibleSupplementaryElementsOfKindSelector
  , insertItemsAtIndexPathsSelector
  , insertSectionsSelector
  , itemAtIndexPathSelector
  , itemAtIndexSelector
  , itemPrototypeSelector
  , layoutAttributesForItemAtIndexPathSelector
  , layoutAttributesForSupplementaryElementOfKind_atIndexPathSelector
  , makeItemWithIdentifier_forIndexPathSelector
  , makeSupplementaryViewOfKind_withIdentifier_forIndexPathSelector
  , maxItemSizeSelector
  , maxNumberOfColumnsSelector
  , maxNumberOfRowsSelector
  , minItemSizeSelector
  , moveItemAtIndexPath_toIndexPathSelector
  , moveSection_toSectionSelector
  , newItemForRepresentedObjectSelector
  , numberOfItemsInSectionSelector
  , numberOfSectionsSelector
  , performBatchUpdates_completionHandlerSelector
  , prefetchDataSourceSelector
  , registerClass_forItemWithIdentifierSelector
  , registerClass_forSupplementaryViewOfKind_withIdentifierSelector
  , registerNib_forItemWithIdentifierSelector
  , registerNib_forSupplementaryViewOfKind_withIdentifierSelector
  , reloadDataSelector
  , reloadItemsAtIndexPathsSelector
  , reloadSectionsSelector
  , scrollToItemsAtIndexPaths_scrollPositionSelector
  , selectAllSelector
  , selectItemsAtIndexPaths_scrollPositionSelector
  , selectableSelector
  , selectionIndexPathsSelector
  , selectionIndexesSelector
  , setAllowsEmptySelectionSelector
  , setAllowsMultipleSelectionSelector
  , setBackgroundColorsSelector
  , setBackgroundViewScrollsWithContentSelector
  , setBackgroundViewSelector
  , setCollectionViewLayoutSelector
  , setContentSelector
  , setDataSourceSelector
  , setDelegateSelector
  , setDraggingSourceOperationMask_forLocalSelector
  , setItemPrototypeSelector
  , setMaxItemSizeSelector
  , setMaxNumberOfColumnsSelector
  , setMaxNumberOfRowsSelector
  , setMinItemSizeSelector
  , setPrefetchDataSourceSelector
  , setSelectableSelector
  , setSelectionIndexPathsSelector
  , setSelectionIndexesSelector
  , supplementaryViewForElementKind_atIndexPathSelector
  , toggleSectionCollapseSelector
  , visibleItemsSelector
  , visibleSupplementaryViewsOfKindSelector

  -- * Enum types
  , NSCollectionViewScrollPosition(NSCollectionViewScrollPosition)
  , pattern NSCollectionViewScrollPositionNone
  , pattern NSCollectionViewScrollPositionTop
  , pattern NSCollectionViewScrollPositionCenteredVertically
  , pattern NSCollectionViewScrollPositionBottom
  , pattern NSCollectionViewScrollPositionNearestHorizontalEdge
  , pattern NSCollectionViewScrollPositionLeft
  , pattern NSCollectionViewScrollPositionCenteredHorizontally
  , pattern NSCollectionViewScrollPositionRight
  , pattern NSCollectionViewScrollPositionLeadingEdge
  , pattern NSCollectionViewScrollPositionTrailingEdge
  , pattern NSCollectionViewScrollPositionNearestVerticalEdge
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

-- | @- reloadData@
reloadData :: IsNSCollectionView nsCollectionView => nsCollectionView -> IO ()
reloadData nsCollectionView =
  sendMessage nsCollectionView reloadDataSelector

-- | @- layoutAttributesForItemAtIndexPath:@
layoutAttributesForItemAtIndexPath :: (IsNSCollectionView nsCollectionView, IsNSIndexPath indexPath) => nsCollectionView -> indexPath -> IO (Id NSCollectionViewLayoutAttributes)
layoutAttributesForItemAtIndexPath nsCollectionView indexPath =
  sendMessage nsCollectionView layoutAttributesForItemAtIndexPathSelector (toNSIndexPath indexPath)

-- | @- layoutAttributesForSupplementaryElementOfKind:atIndexPath:@
layoutAttributesForSupplementaryElementOfKind_atIndexPath :: (IsNSCollectionView nsCollectionView, IsNSString kind, IsNSIndexPath indexPath) => nsCollectionView -> kind -> indexPath -> IO (Id NSCollectionViewLayoutAttributes)
layoutAttributesForSupplementaryElementOfKind_atIndexPath nsCollectionView kind indexPath =
  sendMessage nsCollectionView layoutAttributesForSupplementaryElementOfKind_atIndexPathSelector (toNSString kind) (toNSIndexPath indexPath)

-- | @- frameForItemAtIndex:@
frameForItemAtIndex :: IsNSCollectionView nsCollectionView => nsCollectionView -> CULong -> IO NSRect
frameForItemAtIndex nsCollectionView index =
  sendMessage nsCollectionView frameForItemAtIndexSelector index

-- | @- frameForItemAtIndex:withNumberOfItems:@
frameForItemAtIndex_withNumberOfItems :: IsNSCollectionView nsCollectionView => nsCollectionView -> CULong -> CULong -> IO NSRect
frameForItemAtIndex_withNumberOfItems nsCollectionView index numberOfItems =
  sendMessage nsCollectionView frameForItemAtIndex_withNumberOfItemsSelector index numberOfItems

-- | @- numberOfItemsInSection:@
numberOfItemsInSection :: IsNSCollectionView nsCollectionView => nsCollectionView -> CLong -> IO CLong
numberOfItemsInSection nsCollectionView section =
  sendMessage nsCollectionView numberOfItemsInSectionSelector section

-- | @- selectItemsAtIndexPaths:scrollPosition:@
selectItemsAtIndexPaths_scrollPosition :: (IsNSCollectionView nsCollectionView, IsNSSet indexPaths) => nsCollectionView -> indexPaths -> NSCollectionViewScrollPosition -> IO ()
selectItemsAtIndexPaths_scrollPosition nsCollectionView indexPaths scrollPosition =
  sendMessage nsCollectionView selectItemsAtIndexPaths_scrollPositionSelector (toNSSet indexPaths) scrollPosition

-- | @- deselectItemsAtIndexPaths:@
deselectItemsAtIndexPaths :: (IsNSCollectionView nsCollectionView, IsNSSet indexPaths) => nsCollectionView -> indexPaths -> IO ()
deselectItemsAtIndexPaths nsCollectionView indexPaths =
  sendMessage nsCollectionView deselectItemsAtIndexPathsSelector (toNSSet indexPaths)

-- | @- selectAll:@
selectAll :: IsNSCollectionView nsCollectionView => nsCollectionView -> RawId -> IO ()
selectAll nsCollectionView sender =
  sendMessage nsCollectionView selectAllSelector sender

-- | @- deselectAll:@
deselectAll :: IsNSCollectionView nsCollectionView => nsCollectionView -> RawId -> IO ()
deselectAll nsCollectionView sender =
  sendMessage nsCollectionView deselectAllSelector sender

-- | @- registerClass:forItemWithIdentifier:@
registerClass_forItemWithIdentifier :: (IsNSCollectionView nsCollectionView, IsNSString identifier) => nsCollectionView -> Class -> identifier -> IO ()
registerClass_forItemWithIdentifier nsCollectionView itemClass identifier =
  sendMessage nsCollectionView registerClass_forItemWithIdentifierSelector itemClass (toNSString identifier)

-- | @- registerNib:forItemWithIdentifier:@
registerNib_forItemWithIdentifier :: (IsNSCollectionView nsCollectionView, IsNSNib nib, IsNSString identifier) => nsCollectionView -> nib -> identifier -> IO ()
registerNib_forItemWithIdentifier nsCollectionView nib identifier =
  sendMessage nsCollectionView registerNib_forItemWithIdentifierSelector (toNSNib nib) (toNSString identifier)

-- | @- registerClass:forSupplementaryViewOfKind:withIdentifier:@
registerClass_forSupplementaryViewOfKind_withIdentifier :: (IsNSCollectionView nsCollectionView, IsNSString kind, IsNSString identifier) => nsCollectionView -> Class -> kind -> identifier -> IO ()
registerClass_forSupplementaryViewOfKind_withIdentifier nsCollectionView viewClass kind identifier =
  sendMessage nsCollectionView registerClass_forSupplementaryViewOfKind_withIdentifierSelector viewClass (toNSString kind) (toNSString identifier)

-- | @- registerNib:forSupplementaryViewOfKind:withIdentifier:@
registerNib_forSupplementaryViewOfKind_withIdentifier :: (IsNSCollectionView nsCollectionView, IsNSNib nib, IsNSString kind, IsNSString identifier) => nsCollectionView -> nib -> kind -> identifier -> IO ()
registerNib_forSupplementaryViewOfKind_withIdentifier nsCollectionView nib kind identifier =
  sendMessage nsCollectionView registerNib_forSupplementaryViewOfKind_withIdentifierSelector (toNSNib nib) (toNSString kind) (toNSString identifier)

-- | @- makeItemWithIdentifier:forIndexPath:@
makeItemWithIdentifier_forIndexPath :: (IsNSCollectionView nsCollectionView, IsNSString identifier, IsNSIndexPath indexPath) => nsCollectionView -> identifier -> indexPath -> IO (Id NSCollectionViewItem)
makeItemWithIdentifier_forIndexPath nsCollectionView identifier indexPath =
  sendMessage nsCollectionView makeItemWithIdentifier_forIndexPathSelector (toNSString identifier) (toNSIndexPath indexPath)

-- | @- makeSupplementaryViewOfKind:withIdentifier:forIndexPath:@
makeSupplementaryViewOfKind_withIdentifier_forIndexPath :: (IsNSCollectionView nsCollectionView, IsNSString elementKind, IsNSString identifier, IsNSIndexPath indexPath) => nsCollectionView -> elementKind -> identifier -> indexPath -> IO (Id NSView)
makeSupplementaryViewOfKind_withIdentifier_forIndexPath nsCollectionView elementKind identifier indexPath =
  sendMessage nsCollectionView makeSupplementaryViewOfKind_withIdentifier_forIndexPathSelector (toNSString elementKind) (toNSString identifier) (toNSIndexPath indexPath)

-- | @- itemAtIndex:@
itemAtIndex :: IsNSCollectionView nsCollectionView => nsCollectionView -> CULong -> IO (Id NSCollectionViewItem)
itemAtIndex nsCollectionView index =
  sendMessage nsCollectionView itemAtIndexSelector index

-- | @- itemAtIndexPath:@
itemAtIndexPath :: (IsNSCollectionView nsCollectionView, IsNSIndexPath indexPath) => nsCollectionView -> indexPath -> IO (Id NSCollectionViewItem)
itemAtIndexPath nsCollectionView indexPath =
  sendMessage nsCollectionView itemAtIndexPathSelector (toNSIndexPath indexPath)

-- | @- visibleItems@
visibleItems :: IsNSCollectionView nsCollectionView => nsCollectionView -> IO (Id NSArray)
visibleItems nsCollectionView =
  sendMessage nsCollectionView visibleItemsSelector

-- | @- indexPathsForVisibleItems@
indexPathsForVisibleItems :: IsNSCollectionView nsCollectionView => nsCollectionView -> IO (Id NSSet)
indexPathsForVisibleItems nsCollectionView =
  sendMessage nsCollectionView indexPathsForVisibleItemsSelector

-- | @- indexPathForItem:@
indexPathForItem :: (IsNSCollectionView nsCollectionView, IsNSCollectionViewItem item) => nsCollectionView -> item -> IO (Id NSIndexPath)
indexPathForItem nsCollectionView item =
  sendMessage nsCollectionView indexPathForItemSelector (toNSCollectionViewItem item)

-- | @- indexPathForItemAtPoint:@
indexPathForItemAtPoint :: IsNSCollectionView nsCollectionView => nsCollectionView -> NSPoint -> IO (Id NSIndexPath)
indexPathForItemAtPoint nsCollectionView point =
  sendMessage nsCollectionView indexPathForItemAtPointSelector point

-- | @- supplementaryViewForElementKind:atIndexPath:@
supplementaryViewForElementKind_atIndexPath :: (IsNSCollectionView nsCollectionView, IsNSString elementKind, IsNSIndexPath indexPath) => nsCollectionView -> elementKind -> indexPath -> IO (Id NSView)
supplementaryViewForElementKind_atIndexPath nsCollectionView elementKind indexPath =
  sendMessage nsCollectionView supplementaryViewForElementKind_atIndexPathSelector (toNSString elementKind) (toNSIndexPath indexPath)

-- | @- visibleSupplementaryViewsOfKind:@
visibleSupplementaryViewsOfKind :: (IsNSCollectionView nsCollectionView, IsNSString elementKind) => nsCollectionView -> elementKind -> IO (Id NSArray)
visibleSupplementaryViewsOfKind nsCollectionView elementKind =
  sendMessage nsCollectionView visibleSupplementaryViewsOfKindSelector (toNSString elementKind)

-- | @- indexPathsForVisibleSupplementaryElementsOfKind:@
indexPathsForVisibleSupplementaryElementsOfKind :: (IsNSCollectionView nsCollectionView, IsNSString elementKind) => nsCollectionView -> elementKind -> IO (Id NSSet)
indexPathsForVisibleSupplementaryElementsOfKind nsCollectionView elementKind =
  sendMessage nsCollectionView indexPathsForVisibleSupplementaryElementsOfKindSelector (toNSString elementKind)

-- | @- insertSections:@
insertSections :: (IsNSCollectionView nsCollectionView, IsNSIndexSet sections) => nsCollectionView -> sections -> IO ()
insertSections nsCollectionView sections =
  sendMessage nsCollectionView insertSectionsSelector (toNSIndexSet sections)

-- | @- deleteSections:@
deleteSections :: (IsNSCollectionView nsCollectionView, IsNSIndexSet sections) => nsCollectionView -> sections -> IO ()
deleteSections nsCollectionView sections =
  sendMessage nsCollectionView deleteSectionsSelector (toNSIndexSet sections)

-- | @- reloadSections:@
reloadSections :: (IsNSCollectionView nsCollectionView, IsNSIndexSet sections) => nsCollectionView -> sections -> IO ()
reloadSections nsCollectionView sections =
  sendMessage nsCollectionView reloadSectionsSelector (toNSIndexSet sections)

-- | @- moveSection:toSection:@
moveSection_toSection :: IsNSCollectionView nsCollectionView => nsCollectionView -> CLong -> CLong -> IO ()
moveSection_toSection nsCollectionView section newSection =
  sendMessage nsCollectionView moveSection_toSectionSelector section newSection

-- | @- insertItemsAtIndexPaths:@
insertItemsAtIndexPaths :: (IsNSCollectionView nsCollectionView, IsNSSet indexPaths) => nsCollectionView -> indexPaths -> IO ()
insertItemsAtIndexPaths nsCollectionView indexPaths =
  sendMessage nsCollectionView insertItemsAtIndexPathsSelector (toNSSet indexPaths)

-- | @- deleteItemsAtIndexPaths:@
deleteItemsAtIndexPaths :: (IsNSCollectionView nsCollectionView, IsNSSet indexPaths) => nsCollectionView -> indexPaths -> IO ()
deleteItemsAtIndexPaths nsCollectionView indexPaths =
  sendMessage nsCollectionView deleteItemsAtIndexPathsSelector (toNSSet indexPaths)

-- | @- reloadItemsAtIndexPaths:@
reloadItemsAtIndexPaths :: (IsNSCollectionView nsCollectionView, IsNSSet indexPaths) => nsCollectionView -> indexPaths -> IO ()
reloadItemsAtIndexPaths nsCollectionView indexPaths =
  sendMessage nsCollectionView reloadItemsAtIndexPathsSelector (toNSSet indexPaths)

-- | @- moveItemAtIndexPath:toIndexPath:@
moveItemAtIndexPath_toIndexPath :: (IsNSCollectionView nsCollectionView, IsNSIndexPath indexPath, IsNSIndexPath newIndexPath) => nsCollectionView -> indexPath -> newIndexPath -> IO ()
moveItemAtIndexPath_toIndexPath nsCollectionView indexPath newIndexPath =
  sendMessage nsCollectionView moveItemAtIndexPath_toIndexPathSelector (toNSIndexPath indexPath) (toNSIndexPath newIndexPath)

-- | @- performBatchUpdates:completionHandler:@
performBatchUpdates_completionHandler :: IsNSCollectionView nsCollectionView => nsCollectionView -> Ptr () -> Ptr () -> IO ()
performBatchUpdates_completionHandler nsCollectionView updates completionHandler =
  sendMessage nsCollectionView performBatchUpdates_completionHandlerSelector updates completionHandler

-- | @- toggleSectionCollapse:@
toggleSectionCollapse :: IsNSCollectionView nsCollectionView => nsCollectionView -> RawId -> IO ()
toggleSectionCollapse nsCollectionView sender =
  sendMessage nsCollectionView toggleSectionCollapseSelector sender

-- | @- scrollToItemsAtIndexPaths:scrollPosition:@
scrollToItemsAtIndexPaths_scrollPosition :: (IsNSCollectionView nsCollectionView, IsNSSet indexPaths) => nsCollectionView -> indexPaths -> NSCollectionViewScrollPosition -> IO ()
scrollToItemsAtIndexPaths_scrollPosition nsCollectionView indexPaths scrollPosition =
  sendMessage nsCollectionView scrollToItemsAtIndexPaths_scrollPositionSelector (toNSSet indexPaths) scrollPosition

-- | @- setDraggingSourceOperationMask:forLocal:@
setDraggingSourceOperationMask_forLocal :: IsNSCollectionView nsCollectionView => nsCollectionView -> NSDragOperation -> Bool -> IO ()
setDraggingSourceOperationMask_forLocal nsCollectionView dragOperationMask localDestination =
  sendMessage nsCollectionView setDraggingSourceOperationMask_forLocalSelector dragOperationMask localDestination

-- | @- draggingImageForItemsAtIndexPaths:withEvent:offset:@
draggingImageForItemsAtIndexPaths_withEvent_offset :: (IsNSCollectionView nsCollectionView, IsNSSet indexPaths, IsNSEvent event) => nsCollectionView -> indexPaths -> event -> Ptr NSPoint -> IO (Id NSImage)
draggingImageForItemsAtIndexPaths_withEvent_offset nsCollectionView indexPaths event dragImageOffset =
  sendMessage nsCollectionView draggingImageForItemsAtIndexPaths_withEvent_offsetSelector (toNSSet indexPaths) (toNSEvent event) dragImageOffset

-- | @- draggingImageForItemsAtIndexes:withEvent:offset:@
draggingImageForItemsAtIndexes_withEvent_offset :: (IsNSCollectionView nsCollectionView, IsNSIndexSet indexes, IsNSEvent event) => nsCollectionView -> indexes -> event -> Ptr NSPoint -> IO (Id NSImage)
draggingImageForItemsAtIndexes_withEvent_offset nsCollectionView indexes event dragImageOffset =
  sendMessage nsCollectionView draggingImageForItemsAtIndexes_withEvent_offsetSelector (toNSIndexSet indexes) (toNSEvent event) dragImageOffset

-- | @- newItemForRepresentedObject:@
newItemForRepresentedObject :: IsNSCollectionView nsCollectionView => nsCollectionView -> RawId -> IO (Id NSCollectionViewItem)
newItemForRepresentedObject nsCollectionView object =
  sendOwnedMessage nsCollectionView newItemForRepresentedObjectSelector object

-- | @- dataSource@
dataSource :: IsNSCollectionView nsCollectionView => nsCollectionView -> IO RawId
dataSource nsCollectionView =
  sendMessage nsCollectionView dataSourceSelector

-- | @- setDataSource:@
setDataSource :: IsNSCollectionView nsCollectionView => nsCollectionView -> RawId -> IO ()
setDataSource nsCollectionView value =
  sendMessage nsCollectionView setDataSourceSelector value

-- | @- prefetchDataSource@
prefetchDataSource :: IsNSCollectionView nsCollectionView => nsCollectionView -> IO RawId
prefetchDataSource nsCollectionView =
  sendMessage nsCollectionView prefetchDataSourceSelector

-- | @- setPrefetchDataSource:@
setPrefetchDataSource :: IsNSCollectionView nsCollectionView => nsCollectionView -> RawId -> IO ()
setPrefetchDataSource nsCollectionView value =
  sendMessage nsCollectionView setPrefetchDataSourceSelector value

-- | @- content@
content :: IsNSCollectionView nsCollectionView => nsCollectionView -> IO (Id NSArray)
content nsCollectionView =
  sendMessage nsCollectionView contentSelector

-- | @- setContent:@
setContent :: (IsNSCollectionView nsCollectionView, IsNSArray value) => nsCollectionView -> value -> IO ()
setContent nsCollectionView value =
  sendMessage nsCollectionView setContentSelector (toNSArray value)

-- | @- delegate@
delegate :: IsNSCollectionView nsCollectionView => nsCollectionView -> IO RawId
delegate nsCollectionView =
  sendMessage nsCollectionView delegateSelector

-- | @- setDelegate:@
setDelegate :: IsNSCollectionView nsCollectionView => nsCollectionView -> RawId -> IO ()
setDelegate nsCollectionView value =
  sendMessage nsCollectionView setDelegateSelector value

-- | @- backgroundView@
backgroundView :: IsNSCollectionView nsCollectionView => nsCollectionView -> IO (Id NSView)
backgroundView nsCollectionView =
  sendMessage nsCollectionView backgroundViewSelector

-- | @- setBackgroundView:@
setBackgroundView :: (IsNSCollectionView nsCollectionView, IsNSView value) => nsCollectionView -> value -> IO ()
setBackgroundView nsCollectionView value =
  sendMessage nsCollectionView setBackgroundViewSelector (toNSView value)

-- | @- backgroundViewScrollsWithContent@
backgroundViewScrollsWithContent :: IsNSCollectionView nsCollectionView => nsCollectionView -> IO Bool
backgroundViewScrollsWithContent nsCollectionView =
  sendMessage nsCollectionView backgroundViewScrollsWithContentSelector

-- | @- setBackgroundViewScrollsWithContent:@
setBackgroundViewScrollsWithContent :: IsNSCollectionView nsCollectionView => nsCollectionView -> Bool -> IO ()
setBackgroundViewScrollsWithContent nsCollectionView value =
  sendMessage nsCollectionView setBackgroundViewScrollsWithContentSelector value

-- | @- collectionViewLayout@
collectionViewLayout :: IsNSCollectionView nsCollectionView => nsCollectionView -> IO (Id NSCollectionViewLayout)
collectionViewLayout nsCollectionView =
  sendMessage nsCollectionView collectionViewLayoutSelector

-- | @- setCollectionViewLayout:@
setCollectionViewLayout :: (IsNSCollectionView nsCollectionView, IsNSCollectionViewLayout value) => nsCollectionView -> value -> IO ()
setCollectionViewLayout nsCollectionView value =
  sendMessage nsCollectionView setCollectionViewLayoutSelector (toNSCollectionViewLayout value)

-- | @- backgroundColors@
backgroundColors :: IsNSCollectionView nsCollectionView => nsCollectionView -> IO (Id NSArray)
backgroundColors nsCollectionView =
  sendMessage nsCollectionView backgroundColorsSelector

-- | @- setBackgroundColors:@
setBackgroundColors :: (IsNSCollectionView nsCollectionView, IsNSArray value) => nsCollectionView -> value -> IO ()
setBackgroundColors nsCollectionView value =
  sendMessage nsCollectionView setBackgroundColorsSelector (toNSArray value)

-- | @- numberOfSections@
numberOfSections :: IsNSCollectionView nsCollectionView => nsCollectionView -> IO CLong
numberOfSections nsCollectionView =
  sendMessage nsCollectionView numberOfSectionsSelector

-- | @- firstResponder@
firstResponder :: IsNSCollectionView nsCollectionView => nsCollectionView -> IO Bool
firstResponder nsCollectionView =
  sendMessage nsCollectionView firstResponderSelector

-- | @- selectable@
selectable :: IsNSCollectionView nsCollectionView => nsCollectionView -> IO Bool
selectable nsCollectionView =
  sendMessage nsCollectionView selectableSelector

-- | @- setSelectable:@
setSelectable :: IsNSCollectionView nsCollectionView => nsCollectionView -> Bool -> IO ()
setSelectable nsCollectionView value =
  sendMessage nsCollectionView setSelectableSelector value

-- | @- allowsEmptySelection@
allowsEmptySelection :: IsNSCollectionView nsCollectionView => nsCollectionView -> IO Bool
allowsEmptySelection nsCollectionView =
  sendMessage nsCollectionView allowsEmptySelectionSelector

-- | @- setAllowsEmptySelection:@
setAllowsEmptySelection :: IsNSCollectionView nsCollectionView => nsCollectionView -> Bool -> IO ()
setAllowsEmptySelection nsCollectionView value =
  sendMessage nsCollectionView setAllowsEmptySelectionSelector value

-- | @- allowsMultipleSelection@
allowsMultipleSelection :: IsNSCollectionView nsCollectionView => nsCollectionView -> IO Bool
allowsMultipleSelection nsCollectionView =
  sendMessage nsCollectionView allowsMultipleSelectionSelector

-- | @- setAllowsMultipleSelection:@
setAllowsMultipleSelection :: IsNSCollectionView nsCollectionView => nsCollectionView -> Bool -> IO ()
setAllowsMultipleSelection nsCollectionView value =
  sendMessage nsCollectionView setAllowsMultipleSelectionSelector value

-- | @- selectionIndexes@
selectionIndexes :: IsNSCollectionView nsCollectionView => nsCollectionView -> IO (Id NSIndexSet)
selectionIndexes nsCollectionView =
  sendMessage nsCollectionView selectionIndexesSelector

-- | @- setSelectionIndexes:@
setSelectionIndexes :: (IsNSCollectionView nsCollectionView, IsNSIndexSet value) => nsCollectionView -> value -> IO ()
setSelectionIndexes nsCollectionView value =
  sendMessage nsCollectionView setSelectionIndexesSelector (toNSIndexSet value)

-- | @- selectionIndexPaths@
selectionIndexPaths :: IsNSCollectionView nsCollectionView => nsCollectionView -> IO (Id NSSet)
selectionIndexPaths nsCollectionView =
  sendMessage nsCollectionView selectionIndexPathsSelector

-- | @- setSelectionIndexPaths:@
setSelectionIndexPaths :: (IsNSCollectionView nsCollectionView, IsNSSet value) => nsCollectionView -> value -> IO ()
setSelectionIndexPaths nsCollectionView value =
  sendMessage nsCollectionView setSelectionIndexPathsSelector (toNSSet value)

-- | @- itemPrototype@
itemPrototype :: IsNSCollectionView nsCollectionView => nsCollectionView -> IO (Id NSCollectionViewItem)
itemPrototype nsCollectionView =
  sendMessage nsCollectionView itemPrototypeSelector

-- | @- setItemPrototype:@
setItemPrototype :: (IsNSCollectionView nsCollectionView, IsNSCollectionViewItem value) => nsCollectionView -> value -> IO ()
setItemPrototype nsCollectionView value =
  sendMessage nsCollectionView setItemPrototypeSelector (toNSCollectionViewItem value)

-- | @- maxNumberOfRows@
maxNumberOfRows :: IsNSCollectionView nsCollectionView => nsCollectionView -> IO CULong
maxNumberOfRows nsCollectionView =
  sendMessage nsCollectionView maxNumberOfRowsSelector

-- | @- setMaxNumberOfRows:@
setMaxNumberOfRows :: IsNSCollectionView nsCollectionView => nsCollectionView -> CULong -> IO ()
setMaxNumberOfRows nsCollectionView value =
  sendMessage nsCollectionView setMaxNumberOfRowsSelector value

-- | @- maxNumberOfColumns@
maxNumberOfColumns :: IsNSCollectionView nsCollectionView => nsCollectionView -> IO CULong
maxNumberOfColumns nsCollectionView =
  sendMessage nsCollectionView maxNumberOfColumnsSelector

-- | @- setMaxNumberOfColumns:@
setMaxNumberOfColumns :: IsNSCollectionView nsCollectionView => nsCollectionView -> CULong -> IO ()
setMaxNumberOfColumns nsCollectionView value =
  sendMessage nsCollectionView setMaxNumberOfColumnsSelector value

-- | @- minItemSize@
minItemSize :: IsNSCollectionView nsCollectionView => nsCollectionView -> IO NSSize
minItemSize nsCollectionView =
  sendMessage nsCollectionView minItemSizeSelector

-- | @- setMinItemSize:@
setMinItemSize :: IsNSCollectionView nsCollectionView => nsCollectionView -> NSSize -> IO ()
setMinItemSize nsCollectionView value =
  sendMessage nsCollectionView setMinItemSizeSelector value

-- | @- maxItemSize@
maxItemSize :: IsNSCollectionView nsCollectionView => nsCollectionView -> IO NSSize
maxItemSize nsCollectionView =
  sendMessage nsCollectionView maxItemSizeSelector

-- | @- setMaxItemSize:@
setMaxItemSize :: IsNSCollectionView nsCollectionView => nsCollectionView -> NSSize -> IO ()
setMaxItemSize nsCollectionView value =
  sendMessage nsCollectionView setMaxItemSizeSelector value

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @reloadData@
reloadDataSelector :: Selector '[] ()
reloadDataSelector = mkSelector "reloadData"

-- | @Selector@ for @layoutAttributesForItemAtIndexPath:@
layoutAttributesForItemAtIndexPathSelector :: Selector '[Id NSIndexPath] (Id NSCollectionViewLayoutAttributes)
layoutAttributesForItemAtIndexPathSelector = mkSelector "layoutAttributesForItemAtIndexPath:"

-- | @Selector@ for @layoutAttributesForSupplementaryElementOfKind:atIndexPath:@
layoutAttributesForSupplementaryElementOfKind_atIndexPathSelector :: Selector '[Id NSString, Id NSIndexPath] (Id NSCollectionViewLayoutAttributes)
layoutAttributesForSupplementaryElementOfKind_atIndexPathSelector = mkSelector "layoutAttributesForSupplementaryElementOfKind:atIndexPath:"

-- | @Selector@ for @frameForItemAtIndex:@
frameForItemAtIndexSelector :: Selector '[CULong] NSRect
frameForItemAtIndexSelector = mkSelector "frameForItemAtIndex:"

-- | @Selector@ for @frameForItemAtIndex:withNumberOfItems:@
frameForItemAtIndex_withNumberOfItemsSelector :: Selector '[CULong, CULong] NSRect
frameForItemAtIndex_withNumberOfItemsSelector = mkSelector "frameForItemAtIndex:withNumberOfItems:"

-- | @Selector@ for @numberOfItemsInSection:@
numberOfItemsInSectionSelector :: Selector '[CLong] CLong
numberOfItemsInSectionSelector = mkSelector "numberOfItemsInSection:"

-- | @Selector@ for @selectItemsAtIndexPaths:scrollPosition:@
selectItemsAtIndexPaths_scrollPositionSelector :: Selector '[Id NSSet, NSCollectionViewScrollPosition] ()
selectItemsAtIndexPaths_scrollPositionSelector = mkSelector "selectItemsAtIndexPaths:scrollPosition:"

-- | @Selector@ for @deselectItemsAtIndexPaths:@
deselectItemsAtIndexPathsSelector :: Selector '[Id NSSet] ()
deselectItemsAtIndexPathsSelector = mkSelector "deselectItemsAtIndexPaths:"

-- | @Selector@ for @selectAll:@
selectAllSelector :: Selector '[RawId] ()
selectAllSelector = mkSelector "selectAll:"

-- | @Selector@ for @deselectAll:@
deselectAllSelector :: Selector '[RawId] ()
deselectAllSelector = mkSelector "deselectAll:"

-- | @Selector@ for @registerClass:forItemWithIdentifier:@
registerClass_forItemWithIdentifierSelector :: Selector '[Class, Id NSString] ()
registerClass_forItemWithIdentifierSelector = mkSelector "registerClass:forItemWithIdentifier:"

-- | @Selector@ for @registerNib:forItemWithIdentifier:@
registerNib_forItemWithIdentifierSelector :: Selector '[Id NSNib, Id NSString] ()
registerNib_forItemWithIdentifierSelector = mkSelector "registerNib:forItemWithIdentifier:"

-- | @Selector@ for @registerClass:forSupplementaryViewOfKind:withIdentifier:@
registerClass_forSupplementaryViewOfKind_withIdentifierSelector :: Selector '[Class, Id NSString, Id NSString] ()
registerClass_forSupplementaryViewOfKind_withIdentifierSelector = mkSelector "registerClass:forSupplementaryViewOfKind:withIdentifier:"

-- | @Selector@ for @registerNib:forSupplementaryViewOfKind:withIdentifier:@
registerNib_forSupplementaryViewOfKind_withIdentifierSelector :: Selector '[Id NSNib, Id NSString, Id NSString] ()
registerNib_forSupplementaryViewOfKind_withIdentifierSelector = mkSelector "registerNib:forSupplementaryViewOfKind:withIdentifier:"

-- | @Selector@ for @makeItemWithIdentifier:forIndexPath:@
makeItemWithIdentifier_forIndexPathSelector :: Selector '[Id NSString, Id NSIndexPath] (Id NSCollectionViewItem)
makeItemWithIdentifier_forIndexPathSelector = mkSelector "makeItemWithIdentifier:forIndexPath:"

-- | @Selector@ for @makeSupplementaryViewOfKind:withIdentifier:forIndexPath:@
makeSupplementaryViewOfKind_withIdentifier_forIndexPathSelector :: Selector '[Id NSString, Id NSString, Id NSIndexPath] (Id NSView)
makeSupplementaryViewOfKind_withIdentifier_forIndexPathSelector = mkSelector "makeSupplementaryViewOfKind:withIdentifier:forIndexPath:"

-- | @Selector@ for @itemAtIndex:@
itemAtIndexSelector :: Selector '[CULong] (Id NSCollectionViewItem)
itemAtIndexSelector = mkSelector "itemAtIndex:"

-- | @Selector@ for @itemAtIndexPath:@
itemAtIndexPathSelector :: Selector '[Id NSIndexPath] (Id NSCollectionViewItem)
itemAtIndexPathSelector = mkSelector "itemAtIndexPath:"

-- | @Selector@ for @visibleItems@
visibleItemsSelector :: Selector '[] (Id NSArray)
visibleItemsSelector = mkSelector "visibleItems"

-- | @Selector@ for @indexPathsForVisibleItems@
indexPathsForVisibleItemsSelector :: Selector '[] (Id NSSet)
indexPathsForVisibleItemsSelector = mkSelector "indexPathsForVisibleItems"

-- | @Selector@ for @indexPathForItem:@
indexPathForItemSelector :: Selector '[Id NSCollectionViewItem] (Id NSIndexPath)
indexPathForItemSelector = mkSelector "indexPathForItem:"

-- | @Selector@ for @indexPathForItemAtPoint:@
indexPathForItemAtPointSelector :: Selector '[NSPoint] (Id NSIndexPath)
indexPathForItemAtPointSelector = mkSelector "indexPathForItemAtPoint:"

-- | @Selector@ for @supplementaryViewForElementKind:atIndexPath:@
supplementaryViewForElementKind_atIndexPathSelector :: Selector '[Id NSString, Id NSIndexPath] (Id NSView)
supplementaryViewForElementKind_atIndexPathSelector = mkSelector "supplementaryViewForElementKind:atIndexPath:"

-- | @Selector@ for @visibleSupplementaryViewsOfKind:@
visibleSupplementaryViewsOfKindSelector :: Selector '[Id NSString] (Id NSArray)
visibleSupplementaryViewsOfKindSelector = mkSelector "visibleSupplementaryViewsOfKind:"

-- | @Selector@ for @indexPathsForVisibleSupplementaryElementsOfKind:@
indexPathsForVisibleSupplementaryElementsOfKindSelector :: Selector '[Id NSString] (Id NSSet)
indexPathsForVisibleSupplementaryElementsOfKindSelector = mkSelector "indexPathsForVisibleSupplementaryElementsOfKind:"

-- | @Selector@ for @insertSections:@
insertSectionsSelector :: Selector '[Id NSIndexSet] ()
insertSectionsSelector = mkSelector "insertSections:"

-- | @Selector@ for @deleteSections:@
deleteSectionsSelector :: Selector '[Id NSIndexSet] ()
deleteSectionsSelector = mkSelector "deleteSections:"

-- | @Selector@ for @reloadSections:@
reloadSectionsSelector :: Selector '[Id NSIndexSet] ()
reloadSectionsSelector = mkSelector "reloadSections:"

-- | @Selector@ for @moveSection:toSection:@
moveSection_toSectionSelector :: Selector '[CLong, CLong] ()
moveSection_toSectionSelector = mkSelector "moveSection:toSection:"

-- | @Selector@ for @insertItemsAtIndexPaths:@
insertItemsAtIndexPathsSelector :: Selector '[Id NSSet] ()
insertItemsAtIndexPathsSelector = mkSelector "insertItemsAtIndexPaths:"

-- | @Selector@ for @deleteItemsAtIndexPaths:@
deleteItemsAtIndexPathsSelector :: Selector '[Id NSSet] ()
deleteItemsAtIndexPathsSelector = mkSelector "deleteItemsAtIndexPaths:"

-- | @Selector@ for @reloadItemsAtIndexPaths:@
reloadItemsAtIndexPathsSelector :: Selector '[Id NSSet] ()
reloadItemsAtIndexPathsSelector = mkSelector "reloadItemsAtIndexPaths:"

-- | @Selector@ for @moveItemAtIndexPath:toIndexPath:@
moveItemAtIndexPath_toIndexPathSelector :: Selector '[Id NSIndexPath, Id NSIndexPath] ()
moveItemAtIndexPath_toIndexPathSelector = mkSelector "moveItemAtIndexPath:toIndexPath:"

-- | @Selector@ for @performBatchUpdates:completionHandler:@
performBatchUpdates_completionHandlerSelector :: Selector '[Ptr (), Ptr ()] ()
performBatchUpdates_completionHandlerSelector = mkSelector "performBatchUpdates:completionHandler:"

-- | @Selector@ for @toggleSectionCollapse:@
toggleSectionCollapseSelector :: Selector '[RawId] ()
toggleSectionCollapseSelector = mkSelector "toggleSectionCollapse:"

-- | @Selector@ for @scrollToItemsAtIndexPaths:scrollPosition:@
scrollToItemsAtIndexPaths_scrollPositionSelector :: Selector '[Id NSSet, NSCollectionViewScrollPosition] ()
scrollToItemsAtIndexPaths_scrollPositionSelector = mkSelector "scrollToItemsAtIndexPaths:scrollPosition:"

-- | @Selector@ for @setDraggingSourceOperationMask:forLocal:@
setDraggingSourceOperationMask_forLocalSelector :: Selector '[NSDragOperation, Bool] ()
setDraggingSourceOperationMask_forLocalSelector = mkSelector "setDraggingSourceOperationMask:forLocal:"

-- | @Selector@ for @draggingImageForItemsAtIndexPaths:withEvent:offset:@
draggingImageForItemsAtIndexPaths_withEvent_offsetSelector :: Selector '[Id NSSet, Id NSEvent, Ptr NSPoint] (Id NSImage)
draggingImageForItemsAtIndexPaths_withEvent_offsetSelector = mkSelector "draggingImageForItemsAtIndexPaths:withEvent:offset:"

-- | @Selector@ for @draggingImageForItemsAtIndexes:withEvent:offset:@
draggingImageForItemsAtIndexes_withEvent_offsetSelector :: Selector '[Id NSIndexSet, Id NSEvent, Ptr NSPoint] (Id NSImage)
draggingImageForItemsAtIndexes_withEvent_offsetSelector = mkSelector "draggingImageForItemsAtIndexes:withEvent:offset:"

-- | @Selector@ for @newItemForRepresentedObject:@
newItemForRepresentedObjectSelector :: Selector '[RawId] (Id NSCollectionViewItem)
newItemForRepresentedObjectSelector = mkSelector "newItemForRepresentedObject:"

-- | @Selector@ for @dataSource@
dataSourceSelector :: Selector '[] RawId
dataSourceSelector = mkSelector "dataSource"

-- | @Selector@ for @setDataSource:@
setDataSourceSelector :: Selector '[RawId] ()
setDataSourceSelector = mkSelector "setDataSource:"

-- | @Selector@ for @prefetchDataSource@
prefetchDataSourceSelector :: Selector '[] RawId
prefetchDataSourceSelector = mkSelector "prefetchDataSource"

-- | @Selector@ for @setPrefetchDataSource:@
setPrefetchDataSourceSelector :: Selector '[RawId] ()
setPrefetchDataSourceSelector = mkSelector "setPrefetchDataSource:"

-- | @Selector@ for @content@
contentSelector :: Selector '[] (Id NSArray)
contentSelector = mkSelector "content"

-- | @Selector@ for @setContent:@
setContentSelector :: Selector '[Id NSArray] ()
setContentSelector = mkSelector "setContent:"

-- | @Selector@ for @delegate@
delegateSelector :: Selector '[] RawId
delegateSelector = mkSelector "delegate"

-- | @Selector@ for @setDelegate:@
setDelegateSelector :: Selector '[RawId] ()
setDelegateSelector = mkSelector "setDelegate:"

-- | @Selector@ for @backgroundView@
backgroundViewSelector :: Selector '[] (Id NSView)
backgroundViewSelector = mkSelector "backgroundView"

-- | @Selector@ for @setBackgroundView:@
setBackgroundViewSelector :: Selector '[Id NSView] ()
setBackgroundViewSelector = mkSelector "setBackgroundView:"

-- | @Selector@ for @backgroundViewScrollsWithContent@
backgroundViewScrollsWithContentSelector :: Selector '[] Bool
backgroundViewScrollsWithContentSelector = mkSelector "backgroundViewScrollsWithContent"

-- | @Selector@ for @setBackgroundViewScrollsWithContent:@
setBackgroundViewScrollsWithContentSelector :: Selector '[Bool] ()
setBackgroundViewScrollsWithContentSelector = mkSelector "setBackgroundViewScrollsWithContent:"

-- | @Selector@ for @collectionViewLayout@
collectionViewLayoutSelector :: Selector '[] (Id NSCollectionViewLayout)
collectionViewLayoutSelector = mkSelector "collectionViewLayout"

-- | @Selector@ for @setCollectionViewLayout:@
setCollectionViewLayoutSelector :: Selector '[Id NSCollectionViewLayout] ()
setCollectionViewLayoutSelector = mkSelector "setCollectionViewLayout:"

-- | @Selector@ for @backgroundColors@
backgroundColorsSelector :: Selector '[] (Id NSArray)
backgroundColorsSelector = mkSelector "backgroundColors"

-- | @Selector@ for @setBackgroundColors:@
setBackgroundColorsSelector :: Selector '[Id NSArray] ()
setBackgroundColorsSelector = mkSelector "setBackgroundColors:"

-- | @Selector@ for @numberOfSections@
numberOfSectionsSelector :: Selector '[] CLong
numberOfSectionsSelector = mkSelector "numberOfSections"

-- | @Selector@ for @firstResponder@
firstResponderSelector :: Selector '[] Bool
firstResponderSelector = mkSelector "firstResponder"

-- | @Selector@ for @selectable@
selectableSelector :: Selector '[] Bool
selectableSelector = mkSelector "selectable"

-- | @Selector@ for @setSelectable:@
setSelectableSelector :: Selector '[Bool] ()
setSelectableSelector = mkSelector "setSelectable:"

-- | @Selector@ for @allowsEmptySelection@
allowsEmptySelectionSelector :: Selector '[] Bool
allowsEmptySelectionSelector = mkSelector "allowsEmptySelection"

-- | @Selector@ for @setAllowsEmptySelection:@
setAllowsEmptySelectionSelector :: Selector '[Bool] ()
setAllowsEmptySelectionSelector = mkSelector "setAllowsEmptySelection:"

-- | @Selector@ for @allowsMultipleSelection@
allowsMultipleSelectionSelector :: Selector '[] Bool
allowsMultipleSelectionSelector = mkSelector "allowsMultipleSelection"

-- | @Selector@ for @setAllowsMultipleSelection:@
setAllowsMultipleSelectionSelector :: Selector '[Bool] ()
setAllowsMultipleSelectionSelector = mkSelector "setAllowsMultipleSelection:"

-- | @Selector@ for @selectionIndexes@
selectionIndexesSelector :: Selector '[] (Id NSIndexSet)
selectionIndexesSelector = mkSelector "selectionIndexes"

-- | @Selector@ for @setSelectionIndexes:@
setSelectionIndexesSelector :: Selector '[Id NSIndexSet] ()
setSelectionIndexesSelector = mkSelector "setSelectionIndexes:"

-- | @Selector@ for @selectionIndexPaths@
selectionIndexPathsSelector :: Selector '[] (Id NSSet)
selectionIndexPathsSelector = mkSelector "selectionIndexPaths"

-- | @Selector@ for @setSelectionIndexPaths:@
setSelectionIndexPathsSelector :: Selector '[Id NSSet] ()
setSelectionIndexPathsSelector = mkSelector "setSelectionIndexPaths:"

-- | @Selector@ for @itemPrototype@
itemPrototypeSelector :: Selector '[] (Id NSCollectionViewItem)
itemPrototypeSelector = mkSelector "itemPrototype"

-- | @Selector@ for @setItemPrototype:@
setItemPrototypeSelector :: Selector '[Id NSCollectionViewItem] ()
setItemPrototypeSelector = mkSelector "setItemPrototype:"

-- | @Selector@ for @maxNumberOfRows@
maxNumberOfRowsSelector :: Selector '[] CULong
maxNumberOfRowsSelector = mkSelector "maxNumberOfRows"

-- | @Selector@ for @setMaxNumberOfRows:@
setMaxNumberOfRowsSelector :: Selector '[CULong] ()
setMaxNumberOfRowsSelector = mkSelector "setMaxNumberOfRows:"

-- | @Selector@ for @maxNumberOfColumns@
maxNumberOfColumnsSelector :: Selector '[] CULong
maxNumberOfColumnsSelector = mkSelector "maxNumberOfColumns"

-- | @Selector@ for @setMaxNumberOfColumns:@
setMaxNumberOfColumnsSelector :: Selector '[CULong] ()
setMaxNumberOfColumnsSelector = mkSelector "setMaxNumberOfColumns:"

-- | @Selector@ for @minItemSize@
minItemSizeSelector :: Selector '[] NSSize
minItemSizeSelector = mkSelector "minItemSize"

-- | @Selector@ for @setMinItemSize:@
setMinItemSizeSelector :: Selector '[NSSize] ()
setMinItemSizeSelector = mkSelector "setMinItemSize:"

-- | @Selector@ for @maxItemSize@
maxItemSizeSelector :: Selector '[] NSSize
maxItemSizeSelector = mkSelector "maxItemSize"

-- | @Selector@ for @setMaxItemSize:@
setMaxItemSizeSelector :: Selector '[NSSize] ()
setMaxItemSizeSelector = mkSelector "setMaxItemSize:"

