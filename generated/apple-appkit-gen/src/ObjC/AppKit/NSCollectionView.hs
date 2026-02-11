{-# LANGUAGE PatternSynonyms #-}
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
  , reloadDataSelector
  , layoutAttributesForItemAtIndexPathSelector
  , layoutAttributesForSupplementaryElementOfKind_atIndexPathSelector
  , frameForItemAtIndexSelector
  , frameForItemAtIndex_withNumberOfItemsSelector
  , numberOfItemsInSectionSelector
  , selectItemsAtIndexPaths_scrollPositionSelector
  , deselectItemsAtIndexPathsSelector
  , selectAllSelector
  , deselectAllSelector
  , registerClass_forItemWithIdentifierSelector
  , registerNib_forItemWithIdentifierSelector
  , registerClass_forSupplementaryViewOfKind_withIdentifierSelector
  , registerNib_forSupplementaryViewOfKind_withIdentifierSelector
  , makeItemWithIdentifier_forIndexPathSelector
  , makeSupplementaryViewOfKind_withIdentifier_forIndexPathSelector
  , itemAtIndexSelector
  , itemAtIndexPathSelector
  , visibleItemsSelector
  , indexPathsForVisibleItemsSelector
  , indexPathForItemSelector
  , indexPathForItemAtPointSelector
  , supplementaryViewForElementKind_atIndexPathSelector
  , visibleSupplementaryViewsOfKindSelector
  , indexPathsForVisibleSupplementaryElementsOfKindSelector
  , insertSectionsSelector
  , deleteSectionsSelector
  , reloadSectionsSelector
  , moveSection_toSectionSelector
  , insertItemsAtIndexPathsSelector
  , deleteItemsAtIndexPathsSelector
  , reloadItemsAtIndexPathsSelector
  , moveItemAtIndexPath_toIndexPathSelector
  , performBatchUpdates_completionHandlerSelector
  , toggleSectionCollapseSelector
  , scrollToItemsAtIndexPaths_scrollPositionSelector
  , setDraggingSourceOperationMask_forLocalSelector
  , draggingImageForItemsAtIndexPaths_withEvent_offsetSelector
  , draggingImageForItemsAtIndexes_withEvent_offsetSelector
  , newItemForRepresentedObjectSelector
  , dataSourceSelector
  , setDataSourceSelector
  , prefetchDataSourceSelector
  , setPrefetchDataSourceSelector
  , contentSelector
  , setContentSelector
  , delegateSelector
  , setDelegateSelector
  , backgroundViewSelector
  , setBackgroundViewSelector
  , backgroundViewScrollsWithContentSelector
  , setBackgroundViewScrollsWithContentSelector
  , collectionViewLayoutSelector
  , setCollectionViewLayoutSelector
  , backgroundColorsSelector
  , setBackgroundColorsSelector
  , numberOfSectionsSelector
  , firstResponderSelector
  , selectableSelector
  , setSelectableSelector
  , allowsEmptySelectionSelector
  , setAllowsEmptySelectionSelector
  , allowsMultipleSelectionSelector
  , setAllowsMultipleSelectionSelector
  , selectionIndexesSelector
  , setSelectionIndexesSelector
  , selectionIndexPathsSelector
  , setSelectionIndexPathsSelector
  , itemPrototypeSelector
  , setItemPrototypeSelector
  , maxNumberOfRowsSelector
  , setMaxNumberOfRowsSelector
  , maxNumberOfColumnsSelector
  , setMaxNumberOfColumnsSelector
  , minItemSizeSelector
  , setMinItemSizeSelector
  , maxItemSizeSelector
  , setMaxItemSizeSelector

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

-- | @- reloadData@
reloadData :: IsNSCollectionView nsCollectionView => nsCollectionView -> IO ()
reloadData nsCollectionView  =
    sendMsg nsCollectionView (mkSelector "reloadData") retVoid []

-- | @- layoutAttributesForItemAtIndexPath:@
layoutAttributesForItemAtIndexPath :: (IsNSCollectionView nsCollectionView, IsNSIndexPath indexPath) => nsCollectionView -> indexPath -> IO (Id NSCollectionViewLayoutAttributes)
layoutAttributesForItemAtIndexPath nsCollectionView  indexPath =
  withObjCPtr indexPath $ \raw_indexPath ->
      sendMsg nsCollectionView (mkSelector "layoutAttributesForItemAtIndexPath:") (retPtr retVoid) [argPtr (castPtr raw_indexPath :: Ptr ())] >>= retainedObject . castPtr

-- | @- layoutAttributesForSupplementaryElementOfKind:atIndexPath:@
layoutAttributesForSupplementaryElementOfKind_atIndexPath :: (IsNSCollectionView nsCollectionView, IsNSString kind, IsNSIndexPath indexPath) => nsCollectionView -> kind -> indexPath -> IO (Id NSCollectionViewLayoutAttributes)
layoutAttributesForSupplementaryElementOfKind_atIndexPath nsCollectionView  kind indexPath =
  withObjCPtr kind $ \raw_kind ->
    withObjCPtr indexPath $ \raw_indexPath ->
        sendMsg nsCollectionView (mkSelector "layoutAttributesForSupplementaryElementOfKind:atIndexPath:") (retPtr retVoid) [argPtr (castPtr raw_kind :: Ptr ()), argPtr (castPtr raw_indexPath :: Ptr ())] >>= retainedObject . castPtr

-- | @- frameForItemAtIndex:@
frameForItemAtIndex :: IsNSCollectionView nsCollectionView => nsCollectionView -> CULong -> IO NSRect
frameForItemAtIndex nsCollectionView  index =
    sendMsgStret nsCollectionView (mkSelector "frameForItemAtIndex:") retNSRect [argCULong index]

-- | @- frameForItemAtIndex:withNumberOfItems:@
frameForItemAtIndex_withNumberOfItems :: IsNSCollectionView nsCollectionView => nsCollectionView -> CULong -> CULong -> IO NSRect
frameForItemAtIndex_withNumberOfItems nsCollectionView  index numberOfItems =
    sendMsgStret nsCollectionView (mkSelector "frameForItemAtIndex:withNumberOfItems:") retNSRect [argCULong index, argCULong numberOfItems]

-- | @- numberOfItemsInSection:@
numberOfItemsInSection :: IsNSCollectionView nsCollectionView => nsCollectionView -> CLong -> IO CLong
numberOfItemsInSection nsCollectionView  section =
    sendMsg nsCollectionView (mkSelector "numberOfItemsInSection:") retCLong [argCLong section]

-- | @- selectItemsAtIndexPaths:scrollPosition:@
selectItemsAtIndexPaths_scrollPosition :: (IsNSCollectionView nsCollectionView, IsNSSet indexPaths) => nsCollectionView -> indexPaths -> NSCollectionViewScrollPosition -> IO ()
selectItemsAtIndexPaths_scrollPosition nsCollectionView  indexPaths scrollPosition =
  withObjCPtr indexPaths $ \raw_indexPaths ->
      sendMsg nsCollectionView (mkSelector "selectItemsAtIndexPaths:scrollPosition:") retVoid [argPtr (castPtr raw_indexPaths :: Ptr ()), argCULong (coerce scrollPosition)]

-- | @- deselectItemsAtIndexPaths:@
deselectItemsAtIndexPaths :: (IsNSCollectionView nsCollectionView, IsNSSet indexPaths) => nsCollectionView -> indexPaths -> IO ()
deselectItemsAtIndexPaths nsCollectionView  indexPaths =
  withObjCPtr indexPaths $ \raw_indexPaths ->
      sendMsg nsCollectionView (mkSelector "deselectItemsAtIndexPaths:") retVoid [argPtr (castPtr raw_indexPaths :: Ptr ())]

-- | @- selectAll:@
selectAll :: IsNSCollectionView nsCollectionView => nsCollectionView -> RawId -> IO ()
selectAll nsCollectionView  sender =
    sendMsg nsCollectionView (mkSelector "selectAll:") retVoid [argPtr (castPtr (unRawId sender) :: Ptr ())]

-- | @- deselectAll:@
deselectAll :: IsNSCollectionView nsCollectionView => nsCollectionView -> RawId -> IO ()
deselectAll nsCollectionView  sender =
    sendMsg nsCollectionView (mkSelector "deselectAll:") retVoid [argPtr (castPtr (unRawId sender) :: Ptr ())]

-- | @- registerClass:forItemWithIdentifier:@
registerClass_forItemWithIdentifier :: (IsNSCollectionView nsCollectionView, IsNSString identifier) => nsCollectionView -> Class -> identifier -> IO ()
registerClass_forItemWithIdentifier nsCollectionView  itemClass identifier =
  withObjCPtr identifier $ \raw_identifier ->
      sendMsg nsCollectionView (mkSelector "registerClass:forItemWithIdentifier:") retVoid [argPtr (unClass itemClass), argPtr (castPtr raw_identifier :: Ptr ())]

-- | @- registerNib:forItemWithIdentifier:@
registerNib_forItemWithIdentifier :: (IsNSCollectionView nsCollectionView, IsNSNib nib, IsNSString identifier) => nsCollectionView -> nib -> identifier -> IO ()
registerNib_forItemWithIdentifier nsCollectionView  nib identifier =
  withObjCPtr nib $ \raw_nib ->
    withObjCPtr identifier $ \raw_identifier ->
        sendMsg nsCollectionView (mkSelector "registerNib:forItemWithIdentifier:") retVoid [argPtr (castPtr raw_nib :: Ptr ()), argPtr (castPtr raw_identifier :: Ptr ())]

-- | @- registerClass:forSupplementaryViewOfKind:withIdentifier:@
registerClass_forSupplementaryViewOfKind_withIdentifier :: (IsNSCollectionView nsCollectionView, IsNSString kind, IsNSString identifier) => nsCollectionView -> Class -> kind -> identifier -> IO ()
registerClass_forSupplementaryViewOfKind_withIdentifier nsCollectionView  viewClass kind identifier =
  withObjCPtr kind $ \raw_kind ->
    withObjCPtr identifier $ \raw_identifier ->
        sendMsg nsCollectionView (mkSelector "registerClass:forSupplementaryViewOfKind:withIdentifier:") retVoid [argPtr (unClass viewClass), argPtr (castPtr raw_kind :: Ptr ()), argPtr (castPtr raw_identifier :: Ptr ())]

-- | @- registerNib:forSupplementaryViewOfKind:withIdentifier:@
registerNib_forSupplementaryViewOfKind_withIdentifier :: (IsNSCollectionView nsCollectionView, IsNSNib nib, IsNSString kind, IsNSString identifier) => nsCollectionView -> nib -> kind -> identifier -> IO ()
registerNib_forSupplementaryViewOfKind_withIdentifier nsCollectionView  nib kind identifier =
  withObjCPtr nib $ \raw_nib ->
    withObjCPtr kind $ \raw_kind ->
      withObjCPtr identifier $ \raw_identifier ->
          sendMsg nsCollectionView (mkSelector "registerNib:forSupplementaryViewOfKind:withIdentifier:") retVoid [argPtr (castPtr raw_nib :: Ptr ()), argPtr (castPtr raw_kind :: Ptr ()), argPtr (castPtr raw_identifier :: Ptr ())]

-- | @- makeItemWithIdentifier:forIndexPath:@
makeItemWithIdentifier_forIndexPath :: (IsNSCollectionView nsCollectionView, IsNSString identifier, IsNSIndexPath indexPath) => nsCollectionView -> identifier -> indexPath -> IO (Id NSCollectionViewItem)
makeItemWithIdentifier_forIndexPath nsCollectionView  identifier indexPath =
  withObjCPtr identifier $ \raw_identifier ->
    withObjCPtr indexPath $ \raw_indexPath ->
        sendMsg nsCollectionView (mkSelector "makeItemWithIdentifier:forIndexPath:") (retPtr retVoid) [argPtr (castPtr raw_identifier :: Ptr ()), argPtr (castPtr raw_indexPath :: Ptr ())] >>= retainedObject . castPtr

-- | @- makeSupplementaryViewOfKind:withIdentifier:forIndexPath:@
makeSupplementaryViewOfKind_withIdentifier_forIndexPath :: (IsNSCollectionView nsCollectionView, IsNSString elementKind, IsNSString identifier, IsNSIndexPath indexPath) => nsCollectionView -> elementKind -> identifier -> indexPath -> IO (Id NSView)
makeSupplementaryViewOfKind_withIdentifier_forIndexPath nsCollectionView  elementKind identifier indexPath =
  withObjCPtr elementKind $ \raw_elementKind ->
    withObjCPtr identifier $ \raw_identifier ->
      withObjCPtr indexPath $ \raw_indexPath ->
          sendMsg nsCollectionView (mkSelector "makeSupplementaryViewOfKind:withIdentifier:forIndexPath:") (retPtr retVoid) [argPtr (castPtr raw_elementKind :: Ptr ()), argPtr (castPtr raw_identifier :: Ptr ()), argPtr (castPtr raw_indexPath :: Ptr ())] >>= retainedObject . castPtr

-- | @- itemAtIndex:@
itemAtIndex :: IsNSCollectionView nsCollectionView => nsCollectionView -> CULong -> IO (Id NSCollectionViewItem)
itemAtIndex nsCollectionView  index =
    sendMsg nsCollectionView (mkSelector "itemAtIndex:") (retPtr retVoid) [argCULong index] >>= retainedObject . castPtr

-- | @- itemAtIndexPath:@
itemAtIndexPath :: (IsNSCollectionView nsCollectionView, IsNSIndexPath indexPath) => nsCollectionView -> indexPath -> IO (Id NSCollectionViewItem)
itemAtIndexPath nsCollectionView  indexPath =
  withObjCPtr indexPath $ \raw_indexPath ->
      sendMsg nsCollectionView (mkSelector "itemAtIndexPath:") (retPtr retVoid) [argPtr (castPtr raw_indexPath :: Ptr ())] >>= retainedObject . castPtr

-- | @- visibleItems@
visibleItems :: IsNSCollectionView nsCollectionView => nsCollectionView -> IO (Id NSArray)
visibleItems nsCollectionView  =
    sendMsg nsCollectionView (mkSelector "visibleItems") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- indexPathsForVisibleItems@
indexPathsForVisibleItems :: IsNSCollectionView nsCollectionView => nsCollectionView -> IO (Id NSSet)
indexPathsForVisibleItems nsCollectionView  =
    sendMsg nsCollectionView (mkSelector "indexPathsForVisibleItems") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- indexPathForItem:@
indexPathForItem :: (IsNSCollectionView nsCollectionView, IsNSCollectionViewItem item) => nsCollectionView -> item -> IO (Id NSIndexPath)
indexPathForItem nsCollectionView  item =
  withObjCPtr item $ \raw_item ->
      sendMsg nsCollectionView (mkSelector "indexPathForItem:") (retPtr retVoid) [argPtr (castPtr raw_item :: Ptr ())] >>= retainedObject . castPtr

-- | @- indexPathForItemAtPoint:@
indexPathForItemAtPoint :: IsNSCollectionView nsCollectionView => nsCollectionView -> NSPoint -> IO (Id NSIndexPath)
indexPathForItemAtPoint nsCollectionView  point =
    sendMsg nsCollectionView (mkSelector "indexPathForItemAtPoint:") (retPtr retVoid) [argNSPoint point] >>= retainedObject . castPtr

-- | @- supplementaryViewForElementKind:atIndexPath:@
supplementaryViewForElementKind_atIndexPath :: (IsNSCollectionView nsCollectionView, IsNSString elementKind, IsNSIndexPath indexPath) => nsCollectionView -> elementKind -> indexPath -> IO (Id NSView)
supplementaryViewForElementKind_atIndexPath nsCollectionView  elementKind indexPath =
  withObjCPtr elementKind $ \raw_elementKind ->
    withObjCPtr indexPath $ \raw_indexPath ->
        sendMsg nsCollectionView (mkSelector "supplementaryViewForElementKind:atIndexPath:") (retPtr retVoid) [argPtr (castPtr raw_elementKind :: Ptr ()), argPtr (castPtr raw_indexPath :: Ptr ())] >>= retainedObject . castPtr

-- | @- visibleSupplementaryViewsOfKind:@
visibleSupplementaryViewsOfKind :: (IsNSCollectionView nsCollectionView, IsNSString elementKind) => nsCollectionView -> elementKind -> IO (Id NSArray)
visibleSupplementaryViewsOfKind nsCollectionView  elementKind =
  withObjCPtr elementKind $ \raw_elementKind ->
      sendMsg nsCollectionView (mkSelector "visibleSupplementaryViewsOfKind:") (retPtr retVoid) [argPtr (castPtr raw_elementKind :: Ptr ())] >>= retainedObject . castPtr

-- | @- indexPathsForVisibleSupplementaryElementsOfKind:@
indexPathsForVisibleSupplementaryElementsOfKind :: (IsNSCollectionView nsCollectionView, IsNSString elementKind) => nsCollectionView -> elementKind -> IO (Id NSSet)
indexPathsForVisibleSupplementaryElementsOfKind nsCollectionView  elementKind =
  withObjCPtr elementKind $ \raw_elementKind ->
      sendMsg nsCollectionView (mkSelector "indexPathsForVisibleSupplementaryElementsOfKind:") (retPtr retVoid) [argPtr (castPtr raw_elementKind :: Ptr ())] >>= retainedObject . castPtr

-- | @- insertSections:@
insertSections :: (IsNSCollectionView nsCollectionView, IsNSIndexSet sections) => nsCollectionView -> sections -> IO ()
insertSections nsCollectionView  sections =
  withObjCPtr sections $ \raw_sections ->
      sendMsg nsCollectionView (mkSelector "insertSections:") retVoid [argPtr (castPtr raw_sections :: Ptr ())]

-- | @- deleteSections:@
deleteSections :: (IsNSCollectionView nsCollectionView, IsNSIndexSet sections) => nsCollectionView -> sections -> IO ()
deleteSections nsCollectionView  sections =
  withObjCPtr sections $ \raw_sections ->
      sendMsg nsCollectionView (mkSelector "deleteSections:") retVoid [argPtr (castPtr raw_sections :: Ptr ())]

-- | @- reloadSections:@
reloadSections :: (IsNSCollectionView nsCollectionView, IsNSIndexSet sections) => nsCollectionView -> sections -> IO ()
reloadSections nsCollectionView  sections =
  withObjCPtr sections $ \raw_sections ->
      sendMsg nsCollectionView (mkSelector "reloadSections:") retVoid [argPtr (castPtr raw_sections :: Ptr ())]

-- | @- moveSection:toSection:@
moveSection_toSection :: IsNSCollectionView nsCollectionView => nsCollectionView -> CLong -> CLong -> IO ()
moveSection_toSection nsCollectionView  section newSection =
    sendMsg nsCollectionView (mkSelector "moveSection:toSection:") retVoid [argCLong section, argCLong newSection]

-- | @- insertItemsAtIndexPaths:@
insertItemsAtIndexPaths :: (IsNSCollectionView nsCollectionView, IsNSSet indexPaths) => nsCollectionView -> indexPaths -> IO ()
insertItemsAtIndexPaths nsCollectionView  indexPaths =
  withObjCPtr indexPaths $ \raw_indexPaths ->
      sendMsg nsCollectionView (mkSelector "insertItemsAtIndexPaths:") retVoid [argPtr (castPtr raw_indexPaths :: Ptr ())]

-- | @- deleteItemsAtIndexPaths:@
deleteItemsAtIndexPaths :: (IsNSCollectionView nsCollectionView, IsNSSet indexPaths) => nsCollectionView -> indexPaths -> IO ()
deleteItemsAtIndexPaths nsCollectionView  indexPaths =
  withObjCPtr indexPaths $ \raw_indexPaths ->
      sendMsg nsCollectionView (mkSelector "deleteItemsAtIndexPaths:") retVoid [argPtr (castPtr raw_indexPaths :: Ptr ())]

-- | @- reloadItemsAtIndexPaths:@
reloadItemsAtIndexPaths :: (IsNSCollectionView nsCollectionView, IsNSSet indexPaths) => nsCollectionView -> indexPaths -> IO ()
reloadItemsAtIndexPaths nsCollectionView  indexPaths =
  withObjCPtr indexPaths $ \raw_indexPaths ->
      sendMsg nsCollectionView (mkSelector "reloadItemsAtIndexPaths:") retVoid [argPtr (castPtr raw_indexPaths :: Ptr ())]

-- | @- moveItemAtIndexPath:toIndexPath:@
moveItemAtIndexPath_toIndexPath :: (IsNSCollectionView nsCollectionView, IsNSIndexPath indexPath, IsNSIndexPath newIndexPath) => nsCollectionView -> indexPath -> newIndexPath -> IO ()
moveItemAtIndexPath_toIndexPath nsCollectionView  indexPath newIndexPath =
  withObjCPtr indexPath $ \raw_indexPath ->
    withObjCPtr newIndexPath $ \raw_newIndexPath ->
        sendMsg nsCollectionView (mkSelector "moveItemAtIndexPath:toIndexPath:") retVoid [argPtr (castPtr raw_indexPath :: Ptr ()), argPtr (castPtr raw_newIndexPath :: Ptr ())]

-- | @- performBatchUpdates:completionHandler:@
performBatchUpdates_completionHandler :: IsNSCollectionView nsCollectionView => nsCollectionView -> Ptr () -> Ptr () -> IO ()
performBatchUpdates_completionHandler nsCollectionView  updates completionHandler =
    sendMsg nsCollectionView (mkSelector "performBatchUpdates:completionHandler:") retVoid [argPtr (castPtr updates :: Ptr ()), argPtr (castPtr completionHandler :: Ptr ())]

-- | @- toggleSectionCollapse:@
toggleSectionCollapse :: IsNSCollectionView nsCollectionView => nsCollectionView -> RawId -> IO ()
toggleSectionCollapse nsCollectionView  sender =
    sendMsg nsCollectionView (mkSelector "toggleSectionCollapse:") retVoid [argPtr (castPtr (unRawId sender) :: Ptr ())]

-- | @- scrollToItemsAtIndexPaths:scrollPosition:@
scrollToItemsAtIndexPaths_scrollPosition :: (IsNSCollectionView nsCollectionView, IsNSSet indexPaths) => nsCollectionView -> indexPaths -> NSCollectionViewScrollPosition -> IO ()
scrollToItemsAtIndexPaths_scrollPosition nsCollectionView  indexPaths scrollPosition =
  withObjCPtr indexPaths $ \raw_indexPaths ->
      sendMsg nsCollectionView (mkSelector "scrollToItemsAtIndexPaths:scrollPosition:") retVoid [argPtr (castPtr raw_indexPaths :: Ptr ()), argCULong (coerce scrollPosition)]

-- | @- setDraggingSourceOperationMask:forLocal:@
setDraggingSourceOperationMask_forLocal :: IsNSCollectionView nsCollectionView => nsCollectionView -> NSDragOperation -> Bool -> IO ()
setDraggingSourceOperationMask_forLocal nsCollectionView  dragOperationMask localDestination =
    sendMsg nsCollectionView (mkSelector "setDraggingSourceOperationMask:forLocal:") retVoid [argCULong (coerce dragOperationMask), argCULong (if localDestination then 1 else 0)]

-- | @- draggingImageForItemsAtIndexPaths:withEvent:offset:@
draggingImageForItemsAtIndexPaths_withEvent_offset :: (IsNSCollectionView nsCollectionView, IsNSSet indexPaths, IsNSEvent event) => nsCollectionView -> indexPaths -> event -> Ptr NSPoint -> IO (Id NSImage)
draggingImageForItemsAtIndexPaths_withEvent_offset nsCollectionView  indexPaths event dragImageOffset =
  withObjCPtr indexPaths $ \raw_indexPaths ->
    withObjCPtr event $ \raw_event ->
        sendMsg nsCollectionView (mkSelector "draggingImageForItemsAtIndexPaths:withEvent:offset:") (retPtr retVoid) [argPtr (castPtr raw_indexPaths :: Ptr ()), argPtr (castPtr raw_event :: Ptr ()), argPtr dragImageOffset] >>= retainedObject . castPtr

-- | @- draggingImageForItemsAtIndexes:withEvent:offset:@
draggingImageForItemsAtIndexes_withEvent_offset :: (IsNSCollectionView nsCollectionView, IsNSIndexSet indexes, IsNSEvent event) => nsCollectionView -> indexes -> event -> Ptr NSPoint -> IO (Id NSImage)
draggingImageForItemsAtIndexes_withEvent_offset nsCollectionView  indexes event dragImageOffset =
  withObjCPtr indexes $ \raw_indexes ->
    withObjCPtr event $ \raw_event ->
        sendMsg nsCollectionView (mkSelector "draggingImageForItemsAtIndexes:withEvent:offset:") (retPtr retVoid) [argPtr (castPtr raw_indexes :: Ptr ()), argPtr (castPtr raw_event :: Ptr ()), argPtr dragImageOffset] >>= retainedObject . castPtr

-- | @- newItemForRepresentedObject:@
newItemForRepresentedObject :: IsNSCollectionView nsCollectionView => nsCollectionView -> RawId -> IO (Id NSCollectionViewItem)
newItemForRepresentedObject nsCollectionView  object =
    sendMsg nsCollectionView (mkSelector "newItemForRepresentedObject:") (retPtr retVoid) [argPtr (castPtr (unRawId object) :: Ptr ())] >>= ownedObject . castPtr

-- | @- dataSource@
dataSource :: IsNSCollectionView nsCollectionView => nsCollectionView -> IO RawId
dataSource nsCollectionView  =
    fmap (RawId . castPtr) $ sendMsg nsCollectionView (mkSelector "dataSource") (retPtr retVoid) []

-- | @- setDataSource:@
setDataSource :: IsNSCollectionView nsCollectionView => nsCollectionView -> RawId -> IO ()
setDataSource nsCollectionView  value =
    sendMsg nsCollectionView (mkSelector "setDataSource:") retVoid [argPtr (castPtr (unRawId value) :: Ptr ())]

-- | @- prefetchDataSource@
prefetchDataSource :: IsNSCollectionView nsCollectionView => nsCollectionView -> IO RawId
prefetchDataSource nsCollectionView  =
    fmap (RawId . castPtr) $ sendMsg nsCollectionView (mkSelector "prefetchDataSource") (retPtr retVoid) []

-- | @- setPrefetchDataSource:@
setPrefetchDataSource :: IsNSCollectionView nsCollectionView => nsCollectionView -> RawId -> IO ()
setPrefetchDataSource nsCollectionView  value =
    sendMsg nsCollectionView (mkSelector "setPrefetchDataSource:") retVoid [argPtr (castPtr (unRawId value) :: Ptr ())]

-- | @- content@
content :: IsNSCollectionView nsCollectionView => nsCollectionView -> IO (Id NSArray)
content nsCollectionView  =
    sendMsg nsCollectionView (mkSelector "content") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- setContent:@
setContent :: (IsNSCollectionView nsCollectionView, IsNSArray value) => nsCollectionView -> value -> IO ()
setContent nsCollectionView  value =
  withObjCPtr value $ \raw_value ->
      sendMsg nsCollectionView (mkSelector "setContent:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- | @- delegate@
delegate :: IsNSCollectionView nsCollectionView => nsCollectionView -> IO RawId
delegate nsCollectionView  =
    fmap (RawId . castPtr) $ sendMsg nsCollectionView (mkSelector "delegate") (retPtr retVoid) []

-- | @- setDelegate:@
setDelegate :: IsNSCollectionView nsCollectionView => nsCollectionView -> RawId -> IO ()
setDelegate nsCollectionView  value =
    sendMsg nsCollectionView (mkSelector "setDelegate:") retVoid [argPtr (castPtr (unRawId value) :: Ptr ())]

-- | @- backgroundView@
backgroundView :: IsNSCollectionView nsCollectionView => nsCollectionView -> IO (Id NSView)
backgroundView nsCollectionView  =
    sendMsg nsCollectionView (mkSelector "backgroundView") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- setBackgroundView:@
setBackgroundView :: (IsNSCollectionView nsCollectionView, IsNSView value) => nsCollectionView -> value -> IO ()
setBackgroundView nsCollectionView  value =
  withObjCPtr value $ \raw_value ->
      sendMsg nsCollectionView (mkSelector "setBackgroundView:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- | @- backgroundViewScrollsWithContent@
backgroundViewScrollsWithContent :: IsNSCollectionView nsCollectionView => nsCollectionView -> IO Bool
backgroundViewScrollsWithContent nsCollectionView  =
    fmap ((/= 0) :: CULong -> Bool) $ sendMsg nsCollectionView (mkSelector "backgroundViewScrollsWithContent") retCULong []

-- | @- setBackgroundViewScrollsWithContent:@
setBackgroundViewScrollsWithContent :: IsNSCollectionView nsCollectionView => nsCollectionView -> Bool -> IO ()
setBackgroundViewScrollsWithContent nsCollectionView  value =
    sendMsg nsCollectionView (mkSelector "setBackgroundViewScrollsWithContent:") retVoid [argCULong (if value then 1 else 0)]

-- | @- collectionViewLayout@
collectionViewLayout :: IsNSCollectionView nsCollectionView => nsCollectionView -> IO (Id NSCollectionViewLayout)
collectionViewLayout nsCollectionView  =
    sendMsg nsCollectionView (mkSelector "collectionViewLayout") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- setCollectionViewLayout:@
setCollectionViewLayout :: (IsNSCollectionView nsCollectionView, IsNSCollectionViewLayout value) => nsCollectionView -> value -> IO ()
setCollectionViewLayout nsCollectionView  value =
  withObjCPtr value $ \raw_value ->
      sendMsg nsCollectionView (mkSelector "setCollectionViewLayout:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- | @- backgroundColors@
backgroundColors :: IsNSCollectionView nsCollectionView => nsCollectionView -> IO (Id NSArray)
backgroundColors nsCollectionView  =
    sendMsg nsCollectionView (mkSelector "backgroundColors") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- setBackgroundColors:@
setBackgroundColors :: (IsNSCollectionView nsCollectionView, IsNSArray value) => nsCollectionView -> value -> IO ()
setBackgroundColors nsCollectionView  value =
  withObjCPtr value $ \raw_value ->
      sendMsg nsCollectionView (mkSelector "setBackgroundColors:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- | @- numberOfSections@
numberOfSections :: IsNSCollectionView nsCollectionView => nsCollectionView -> IO CLong
numberOfSections nsCollectionView  =
    sendMsg nsCollectionView (mkSelector "numberOfSections") retCLong []

-- | @- firstResponder@
firstResponder :: IsNSCollectionView nsCollectionView => nsCollectionView -> IO Bool
firstResponder nsCollectionView  =
    fmap ((/= 0) :: CULong -> Bool) $ sendMsg nsCollectionView (mkSelector "firstResponder") retCULong []

-- | @- selectable@
selectable :: IsNSCollectionView nsCollectionView => nsCollectionView -> IO Bool
selectable nsCollectionView  =
    fmap ((/= 0) :: CULong -> Bool) $ sendMsg nsCollectionView (mkSelector "selectable") retCULong []

-- | @- setSelectable:@
setSelectable :: IsNSCollectionView nsCollectionView => nsCollectionView -> Bool -> IO ()
setSelectable nsCollectionView  value =
    sendMsg nsCollectionView (mkSelector "setSelectable:") retVoid [argCULong (if value then 1 else 0)]

-- | @- allowsEmptySelection@
allowsEmptySelection :: IsNSCollectionView nsCollectionView => nsCollectionView -> IO Bool
allowsEmptySelection nsCollectionView  =
    fmap ((/= 0) :: CULong -> Bool) $ sendMsg nsCollectionView (mkSelector "allowsEmptySelection") retCULong []

-- | @- setAllowsEmptySelection:@
setAllowsEmptySelection :: IsNSCollectionView nsCollectionView => nsCollectionView -> Bool -> IO ()
setAllowsEmptySelection nsCollectionView  value =
    sendMsg nsCollectionView (mkSelector "setAllowsEmptySelection:") retVoid [argCULong (if value then 1 else 0)]

-- | @- allowsMultipleSelection@
allowsMultipleSelection :: IsNSCollectionView nsCollectionView => nsCollectionView -> IO Bool
allowsMultipleSelection nsCollectionView  =
    fmap ((/= 0) :: CULong -> Bool) $ sendMsg nsCollectionView (mkSelector "allowsMultipleSelection") retCULong []

-- | @- setAllowsMultipleSelection:@
setAllowsMultipleSelection :: IsNSCollectionView nsCollectionView => nsCollectionView -> Bool -> IO ()
setAllowsMultipleSelection nsCollectionView  value =
    sendMsg nsCollectionView (mkSelector "setAllowsMultipleSelection:") retVoid [argCULong (if value then 1 else 0)]

-- | @- selectionIndexes@
selectionIndexes :: IsNSCollectionView nsCollectionView => nsCollectionView -> IO (Id NSIndexSet)
selectionIndexes nsCollectionView  =
    sendMsg nsCollectionView (mkSelector "selectionIndexes") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- setSelectionIndexes:@
setSelectionIndexes :: (IsNSCollectionView nsCollectionView, IsNSIndexSet value) => nsCollectionView -> value -> IO ()
setSelectionIndexes nsCollectionView  value =
  withObjCPtr value $ \raw_value ->
      sendMsg nsCollectionView (mkSelector "setSelectionIndexes:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- | @- selectionIndexPaths@
selectionIndexPaths :: IsNSCollectionView nsCollectionView => nsCollectionView -> IO (Id NSSet)
selectionIndexPaths nsCollectionView  =
    sendMsg nsCollectionView (mkSelector "selectionIndexPaths") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- setSelectionIndexPaths:@
setSelectionIndexPaths :: (IsNSCollectionView nsCollectionView, IsNSSet value) => nsCollectionView -> value -> IO ()
setSelectionIndexPaths nsCollectionView  value =
  withObjCPtr value $ \raw_value ->
      sendMsg nsCollectionView (mkSelector "setSelectionIndexPaths:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- | @- itemPrototype@
itemPrototype :: IsNSCollectionView nsCollectionView => nsCollectionView -> IO (Id NSCollectionViewItem)
itemPrototype nsCollectionView  =
    sendMsg nsCollectionView (mkSelector "itemPrototype") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- setItemPrototype:@
setItemPrototype :: (IsNSCollectionView nsCollectionView, IsNSCollectionViewItem value) => nsCollectionView -> value -> IO ()
setItemPrototype nsCollectionView  value =
  withObjCPtr value $ \raw_value ->
      sendMsg nsCollectionView (mkSelector "setItemPrototype:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- | @- maxNumberOfRows@
maxNumberOfRows :: IsNSCollectionView nsCollectionView => nsCollectionView -> IO CULong
maxNumberOfRows nsCollectionView  =
    sendMsg nsCollectionView (mkSelector "maxNumberOfRows") retCULong []

-- | @- setMaxNumberOfRows:@
setMaxNumberOfRows :: IsNSCollectionView nsCollectionView => nsCollectionView -> CULong -> IO ()
setMaxNumberOfRows nsCollectionView  value =
    sendMsg nsCollectionView (mkSelector "setMaxNumberOfRows:") retVoid [argCULong value]

-- | @- maxNumberOfColumns@
maxNumberOfColumns :: IsNSCollectionView nsCollectionView => nsCollectionView -> IO CULong
maxNumberOfColumns nsCollectionView  =
    sendMsg nsCollectionView (mkSelector "maxNumberOfColumns") retCULong []

-- | @- setMaxNumberOfColumns:@
setMaxNumberOfColumns :: IsNSCollectionView nsCollectionView => nsCollectionView -> CULong -> IO ()
setMaxNumberOfColumns nsCollectionView  value =
    sendMsg nsCollectionView (mkSelector "setMaxNumberOfColumns:") retVoid [argCULong value]

-- | @- minItemSize@
minItemSize :: IsNSCollectionView nsCollectionView => nsCollectionView -> IO NSSize
minItemSize nsCollectionView  =
    sendMsgStret nsCollectionView (mkSelector "minItemSize") retNSSize []

-- | @- setMinItemSize:@
setMinItemSize :: IsNSCollectionView nsCollectionView => nsCollectionView -> NSSize -> IO ()
setMinItemSize nsCollectionView  value =
    sendMsg nsCollectionView (mkSelector "setMinItemSize:") retVoid [argNSSize value]

-- | @- maxItemSize@
maxItemSize :: IsNSCollectionView nsCollectionView => nsCollectionView -> IO NSSize
maxItemSize nsCollectionView  =
    sendMsgStret nsCollectionView (mkSelector "maxItemSize") retNSSize []

-- | @- setMaxItemSize:@
setMaxItemSize :: IsNSCollectionView nsCollectionView => nsCollectionView -> NSSize -> IO ()
setMaxItemSize nsCollectionView  value =
    sendMsg nsCollectionView (mkSelector "setMaxItemSize:") retVoid [argNSSize value]

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @reloadData@
reloadDataSelector :: Selector
reloadDataSelector = mkSelector "reloadData"

-- | @Selector@ for @layoutAttributesForItemAtIndexPath:@
layoutAttributesForItemAtIndexPathSelector :: Selector
layoutAttributesForItemAtIndexPathSelector = mkSelector "layoutAttributesForItemAtIndexPath:"

-- | @Selector@ for @layoutAttributesForSupplementaryElementOfKind:atIndexPath:@
layoutAttributesForSupplementaryElementOfKind_atIndexPathSelector :: Selector
layoutAttributesForSupplementaryElementOfKind_atIndexPathSelector = mkSelector "layoutAttributesForSupplementaryElementOfKind:atIndexPath:"

-- | @Selector@ for @frameForItemAtIndex:@
frameForItemAtIndexSelector :: Selector
frameForItemAtIndexSelector = mkSelector "frameForItemAtIndex:"

-- | @Selector@ for @frameForItemAtIndex:withNumberOfItems:@
frameForItemAtIndex_withNumberOfItemsSelector :: Selector
frameForItemAtIndex_withNumberOfItemsSelector = mkSelector "frameForItemAtIndex:withNumberOfItems:"

-- | @Selector@ for @numberOfItemsInSection:@
numberOfItemsInSectionSelector :: Selector
numberOfItemsInSectionSelector = mkSelector "numberOfItemsInSection:"

-- | @Selector@ for @selectItemsAtIndexPaths:scrollPosition:@
selectItemsAtIndexPaths_scrollPositionSelector :: Selector
selectItemsAtIndexPaths_scrollPositionSelector = mkSelector "selectItemsAtIndexPaths:scrollPosition:"

-- | @Selector@ for @deselectItemsAtIndexPaths:@
deselectItemsAtIndexPathsSelector :: Selector
deselectItemsAtIndexPathsSelector = mkSelector "deselectItemsAtIndexPaths:"

-- | @Selector@ for @selectAll:@
selectAllSelector :: Selector
selectAllSelector = mkSelector "selectAll:"

-- | @Selector@ for @deselectAll:@
deselectAllSelector :: Selector
deselectAllSelector = mkSelector "deselectAll:"

-- | @Selector@ for @registerClass:forItemWithIdentifier:@
registerClass_forItemWithIdentifierSelector :: Selector
registerClass_forItemWithIdentifierSelector = mkSelector "registerClass:forItemWithIdentifier:"

-- | @Selector@ for @registerNib:forItemWithIdentifier:@
registerNib_forItemWithIdentifierSelector :: Selector
registerNib_forItemWithIdentifierSelector = mkSelector "registerNib:forItemWithIdentifier:"

-- | @Selector@ for @registerClass:forSupplementaryViewOfKind:withIdentifier:@
registerClass_forSupplementaryViewOfKind_withIdentifierSelector :: Selector
registerClass_forSupplementaryViewOfKind_withIdentifierSelector = mkSelector "registerClass:forSupplementaryViewOfKind:withIdentifier:"

-- | @Selector@ for @registerNib:forSupplementaryViewOfKind:withIdentifier:@
registerNib_forSupplementaryViewOfKind_withIdentifierSelector :: Selector
registerNib_forSupplementaryViewOfKind_withIdentifierSelector = mkSelector "registerNib:forSupplementaryViewOfKind:withIdentifier:"

-- | @Selector@ for @makeItemWithIdentifier:forIndexPath:@
makeItemWithIdentifier_forIndexPathSelector :: Selector
makeItemWithIdentifier_forIndexPathSelector = mkSelector "makeItemWithIdentifier:forIndexPath:"

-- | @Selector@ for @makeSupplementaryViewOfKind:withIdentifier:forIndexPath:@
makeSupplementaryViewOfKind_withIdentifier_forIndexPathSelector :: Selector
makeSupplementaryViewOfKind_withIdentifier_forIndexPathSelector = mkSelector "makeSupplementaryViewOfKind:withIdentifier:forIndexPath:"

-- | @Selector@ for @itemAtIndex:@
itemAtIndexSelector :: Selector
itemAtIndexSelector = mkSelector "itemAtIndex:"

-- | @Selector@ for @itemAtIndexPath:@
itemAtIndexPathSelector :: Selector
itemAtIndexPathSelector = mkSelector "itemAtIndexPath:"

-- | @Selector@ for @visibleItems@
visibleItemsSelector :: Selector
visibleItemsSelector = mkSelector "visibleItems"

-- | @Selector@ for @indexPathsForVisibleItems@
indexPathsForVisibleItemsSelector :: Selector
indexPathsForVisibleItemsSelector = mkSelector "indexPathsForVisibleItems"

-- | @Selector@ for @indexPathForItem:@
indexPathForItemSelector :: Selector
indexPathForItemSelector = mkSelector "indexPathForItem:"

-- | @Selector@ for @indexPathForItemAtPoint:@
indexPathForItemAtPointSelector :: Selector
indexPathForItemAtPointSelector = mkSelector "indexPathForItemAtPoint:"

-- | @Selector@ for @supplementaryViewForElementKind:atIndexPath:@
supplementaryViewForElementKind_atIndexPathSelector :: Selector
supplementaryViewForElementKind_atIndexPathSelector = mkSelector "supplementaryViewForElementKind:atIndexPath:"

-- | @Selector@ for @visibleSupplementaryViewsOfKind:@
visibleSupplementaryViewsOfKindSelector :: Selector
visibleSupplementaryViewsOfKindSelector = mkSelector "visibleSupplementaryViewsOfKind:"

-- | @Selector@ for @indexPathsForVisibleSupplementaryElementsOfKind:@
indexPathsForVisibleSupplementaryElementsOfKindSelector :: Selector
indexPathsForVisibleSupplementaryElementsOfKindSelector = mkSelector "indexPathsForVisibleSupplementaryElementsOfKind:"

-- | @Selector@ for @insertSections:@
insertSectionsSelector :: Selector
insertSectionsSelector = mkSelector "insertSections:"

-- | @Selector@ for @deleteSections:@
deleteSectionsSelector :: Selector
deleteSectionsSelector = mkSelector "deleteSections:"

-- | @Selector@ for @reloadSections:@
reloadSectionsSelector :: Selector
reloadSectionsSelector = mkSelector "reloadSections:"

-- | @Selector@ for @moveSection:toSection:@
moveSection_toSectionSelector :: Selector
moveSection_toSectionSelector = mkSelector "moveSection:toSection:"

-- | @Selector@ for @insertItemsAtIndexPaths:@
insertItemsAtIndexPathsSelector :: Selector
insertItemsAtIndexPathsSelector = mkSelector "insertItemsAtIndexPaths:"

-- | @Selector@ for @deleteItemsAtIndexPaths:@
deleteItemsAtIndexPathsSelector :: Selector
deleteItemsAtIndexPathsSelector = mkSelector "deleteItemsAtIndexPaths:"

-- | @Selector@ for @reloadItemsAtIndexPaths:@
reloadItemsAtIndexPathsSelector :: Selector
reloadItemsAtIndexPathsSelector = mkSelector "reloadItemsAtIndexPaths:"

-- | @Selector@ for @moveItemAtIndexPath:toIndexPath:@
moveItemAtIndexPath_toIndexPathSelector :: Selector
moveItemAtIndexPath_toIndexPathSelector = mkSelector "moveItemAtIndexPath:toIndexPath:"

-- | @Selector@ for @performBatchUpdates:completionHandler:@
performBatchUpdates_completionHandlerSelector :: Selector
performBatchUpdates_completionHandlerSelector = mkSelector "performBatchUpdates:completionHandler:"

-- | @Selector@ for @toggleSectionCollapse:@
toggleSectionCollapseSelector :: Selector
toggleSectionCollapseSelector = mkSelector "toggleSectionCollapse:"

-- | @Selector@ for @scrollToItemsAtIndexPaths:scrollPosition:@
scrollToItemsAtIndexPaths_scrollPositionSelector :: Selector
scrollToItemsAtIndexPaths_scrollPositionSelector = mkSelector "scrollToItemsAtIndexPaths:scrollPosition:"

-- | @Selector@ for @setDraggingSourceOperationMask:forLocal:@
setDraggingSourceOperationMask_forLocalSelector :: Selector
setDraggingSourceOperationMask_forLocalSelector = mkSelector "setDraggingSourceOperationMask:forLocal:"

-- | @Selector@ for @draggingImageForItemsAtIndexPaths:withEvent:offset:@
draggingImageForItemsAtIndexPaths_withEvent_offsetSelector :: Selector
draggingImageForItemsAtIndexPaths_withEvent_offsetSelector = mkSelector "draggingImageForItemsAtIndexPaths:withEvent:offset:"

-- | @Selector@ for @draggingImageForItemsAtIndexes:withEvent:offset:@
draggingImageForItemsAtIndexes_withEvent_offsetSelector :: Selector
draggingImageForItemsAtIndexes_withEvent_offsetSelector = mkSelector "draggingImageForItemsAtIndexes:withEvent:offset:"

-- | @Selector@ for @newItemForRepresentedObject:@
newItemForRepresentedObjectSelector :: Selector
newItemForRepresentedObjectSelector = mkSelector "newItemForRepresentedObject:"

-- | @Selector@ for @dataSource@
dataSourceSelector :: Selector
dataSourceSelector = mkSelector "dataSource"

-- | @Selector@ for @setDataSource:@
setDataSourceSelector :: Selector
setDataSourceSelector = mkSelector "setDataSource:"

-- | @Selector@ for @prefetchDataSource@
prefetchDataSourceSelector :: Selector
prefetchDataSourceSelector = mkSelector "prefetchDataSource"

-- | @Selector@ for @setPrefetchDataSource:@
setPrefetchDataSourceSelector :: Selector
setPrefetchDataSourceSelector = mkSelector "setPrefetchDataSource:"

-- | @Selector@ for @content@
contentSelector :: Selector
contentSelector = mkSelector "content"

-- | @Selector@ for @setContent:@
setContentSelector :: Selector
setContentSelector = mkSelector "setContent:"

-- | @Selector@ for @delegate@
delegateSelector :: Selector
delegateSelector = mkSelector "delegate"

-- | @Selector@ for @setDelegate:@
setDelegateSelector :: Selector
setDelegateSelector = mkSelector "setDelegate:"

-- | @Selector@ for @backgroundView@
backgroundViewSelector :: Selector
backgroundViewSelector = mkSelector "backgroundView"

-- | @Selector@ for @setBackgroundView:@
setBackgroundViewSelector :: Selector
setBackgroundViewSelector = mkSelector "setBackgroundView:"

-- | @Selector@ for @backgroundViewScrollsWithContent@
backgroundViewScrollsWithContentSelector :: Selector
backgroundViewScrollsWithContentSelector = mkSelector "backgroundViewScrollsWithContent"

-- | @Selector@ for @setBackgroundViewScrollsWithContent:@
setBackgroundViewScrollsWithContentSelector :: Selector
setBackgroundViewScrollsWithContentSelector = mkSelector "setBackgroundViewScrollsWithContent:"

-- | @Selector@ for @collectionViewLayout@
collectionViewLayoutSelector :: Selector
collectionViewLayoutSelector = mkSelector "collectionViewLayout"

-- | @Selector@ for @setCollectionViewLayout:@
setCollectionViewLayoutSelector :: Selector
setCollectionViewLayoutSelector = mkSelector "setCollectionViewLayout:"

-- | @Selector@ for @backgroundColors@
backgroundColorsSelector :: Selector
backgroundColorsSelector = mkSelector "backgroundColors"

-- | @Selector@ for @setBackgroundColors:@
setBackgroundColorsSelector :: Selector
setBackgroundColorsSelector = mkSelector "setBackgroundColors:"

-- | @Selector@ for @numberOfSections@
numberOfSectionsSelector :: Selector
numberOfSectionsSelector = mkSelector "numberOfSections"

-- | @Selector@ for @firstResponder@
firstResponderSelector :: Selector
firstResponderSelector = mkSelector "firstResponder"

-- | @Selector@ for @selectable@
selectableSelector :: Selector
selectableSelector = mkSelector "selectable"

-- | @Selector@ for @setSelectable:@
setSelectableSelector :: Selector
setSelectableSelector = mkSelector "setSelectable:"

-- | @Selector@ for @allowsEmptySelection@
allowsEmptySelectionSelector :: Selector
allowsEmptySelectionSelector = mkSelector "allowsEmptySelection"

-- | @Selector@ for @setAllowsEmptySelection:@
setAllowsEmptySelectionSelector :: Selector
setAllowsEmptySelectionSelector = mkSelector "setAllowsEmptySelection:"

-- | @Selector@ for @allowsMultipleSelection@
allowsMultipleSelectionSelector :: Selector
allowsMultipleSelectionSelector = mkSelector "allowsMultipleSelection"

-- | @Selector@ for @setAllowsMultipleSelection:@
setAllowsMultipleSelectionSelector :: Selector
setAllowsMultipleSelectionSelector = mkSelector "setAllowsMultipleSelection:"

-- | @Selector@ for @selectionIndexes@
selectionIndexesSelector :: Selector
selectionIndexesSelector = mkSelector "selectionIndexes"

-- | @Selector@ for @setSelectionIndexes:@
setSelectionIndexesSelector :: Selector
setSelectionIndexesSelector = mkSelector "setSelectionIndexes:"

-- | @Selector@ for @selectionIndexPaths@
selectionIndexPathsSelector :: Selector
selectionIndexPathsSelector = mkSelector "selectionIndexPaths"

-- | @Selector@ for @setSelectionIndexPaths:@
setSelectionIndexPathsSelector :: Selector
setSelectionIndexPathsSelector = mkSelector "setSelectionIndexPaths:"

-- | @Selector@ for @itemPrototype@
itemPrototypeSelector :: Selector
itemPrototypeSelector = mkSelector "itemPrototype"

-- | @Selector@ for @setItemPrototype:@
setItemPrototypeSelector :: Selector
setItemPrototypeSelector = mkSelector "setItemPrototype:"

-- | @Selector@ for @maxNumberOfRows@
maxNumberOfRowsSelector :: Selector
maxNumberOfRowsSelector = mkSelector "maxNumberOfRows"

-- | @Selector@ for @setMaxNumberOfRows:@
setMaxNumberOfRowsSelector :: Selector
setMaxNumberOfRowsSelector = mkSelector "setMaxNumberOfRows:"

-- | @Selector@ for @maxNumberOfColumns@
maxNumberOfColumnsSelector :: Selector
maxNumberOfColumnsSelector = mkSelector "maxNumberOfColumns"

-- | @Selector@ for @setMaxNumberOfColumns:@
setMaxNumberOfColumnsSelector :: Selector
setMaxNumberOfColumnsSelector = mkSelector "setMaxNumberOfColumns:"

-- | @Selector@ for @minItemSize@
minItemSizeSelector :: Selector
minItemSizeSelector = mkSelector "minItemSize"

-- | @Selector@ for @setMinItemSize:@
setMinItemSizeSelector :: Selector
setMinItemSizeSelector = mkSelector "setMinItemSize:"

-- | @Selector@ for @maxItemSize@
maxItemSizeSelector :: Selector
maxItemSizeSelector = mkSelector "maxItemSize"

-- | @Selector@ for @setMaxItemSize:@
setMaxItemSizeSelector :: Selector
setMaxItemSizeSelector = mkSelector "setMaxItemSize:"

