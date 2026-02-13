{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Generated bindings for @PHCollectionListChangeRequest@.
module ObjC.Photos.PHCollectionListChangeRequest
  ( PHCollectionListChangeRequest
  , IsPHCollectionListChangeRequest(..)
  , creationRequestForCollectionListWithTitle
  , deleteCollectionLists
  , changeRequestForCollectionList
  , changeRequestForCollectionList_childCollections
  , changeRequestForTopLevelCollectionListUserCollections
  , addChildCollections
  , insertChildCollections_atIndexes
  , removeChildCollections
  , removeChildCollectionsAtIndexes
  , replaceChildCollectionsAtIndexes_withChildCollections
  , moveChildCollectionsAtIndexes_toIndex
  , placeholderForCreatedCollectionList
  , title
  , setTitle
  , addChildCollectionsSelector
  , changeRequestForCollectionListSelector
  , changeRequestForCollectionList_childCollectionsSelector
  , changeRequestForTopLevelCollectionListUserCollectionsSelector
  , creationRequestForCollectionListWithTitleSelector
  , deleteCollectionListsSelector
  , insertChildCollections_atIndexesSelector
  , moveChildCollectionsAtIndexes_toIndexSelector
  , placeholderForCreatedCollectionListSelector
  , removeChildCollectionsAtIndexesSelector
  , removeChildCollectionsSelector
  , replaceChildCollectionsAtIndexes_withChildCollectionsSelector
  , setTitleSelector
  , titleSelector


  ) where

import Foreign.Ptr (Ptr, FunPtr)
import Foreign.C.Types

import ObjC.Runtime.Types
import ObjC.Runtime.Message (sendMessage, sendOwnedMessage, sendClassMessage, sendOwnedClassMessage)
import ObjC.Runtime.Selector (mkSelector)
import ObjC.Runtime.Class (getRequiredClass)

import ObjC.Photos.Internal.Classes
import ObjC.Foundation.Internal.Classes

-- | @+ creationRequestForCollectionListWithTitle:@
creationRequestForCollectionListWithTitle :: IsNSString title => title -> IO (Id PHCollectionListChangeRequest)
creationRequestForCollectionListWithTitle title =
  do
    cls' <- getRequiredClass "PHCollectionListChangeRequest"
    sendClassMessage cls' creationRequestForCollectionListWithTitleSelector (toNSString title)

-- | @+ deleteCollectionLists:@
deleteCollectionLists :: RawId -> IO ()
deleteCollectionLists collectionLists =
  do
    cls' <- getRequiredClass "PHCollectionListChangeRequest"
    sendClassMessage cls' deleteCollectionListsSelector collectionLists

-- | @+ changeRequestForCollectionList:@
changeRequestForCollectionList :: IsPHCollectionList collectionList => collectionList -> IO (Id PHCollectionListChangeRequest)
changeRequestForCollectionList collectionList =
  do
    cls' <- getRequiredClass "PHCollectionListChangeRequest"
    sendClassMessage cls' changeRequestForCollectionListSelector (toPHCollectionList collectionList)

-- | @+ changeRequestForCollectionList:childCollections:@
changeRequestForCollectionList_childCollections :: (IsPHCollectionList collectionList, IsPHFetchResult childCollections) => collectionList -> childCollections -> IO (Id PHCollectionListChangeRequest)
changeRequestForCollectionList_childCollections collectionList childCollections =
  do
    cls' <- getRequiredClass "PHCollectionListChangeRequest"
    sendClassMessage cls' changeRequestForCollectionList_childCollectionsSelector (toPHCollectionList collectionList) (toPHFetchResult childCollections)

-- | @+ changeRequestForTopLevelCollectionListUserCollections:@
changeRequestForTopLevelCollectionListUserCollections :: IsPHFetchResult childCollections => childCollections -> IO (Id PHCollectionListChangeRequest)
changeRequestForTopLevelCollectionListUserCollections childCollections =
  do
    cls' <- getRequiredClass "PHCollectionListChangeRequest"
    sendClassMessage cls' changeRequestForTopLevelCollectionListUserCollectionsSelector (toPHFetchResult childCollections)

-- | @- addChildCollections:@
addChildCollections :: IsPHCollectionListChangeRequest phCollectionListChangeRequest => phCollectionListChangeRequest -> RawId -> IO ()
addChildCollections phCollectionListChangeRequest collections =
  sendMessage phCollectionListChangeRequest addChildCollectionsSelector collections

-- | @- insertChildCollections:atIndexes:@
insertChildCollections_atIndexes :: (IsPHCollectionListChangeRequest phCollectionListChangeRequest, IsNSIndexSet indexes) => phCollectionListChangeRequest -> RawId -> indexes -> IO ()
insertChildCollections_atIndexes phCollectionListChangeRequest collections indexes =
  sendMessage phCollectionListChangeRequest insertChildCollections_atIndexesSelector collections (toNSIndexSet indexes)

-- | @- removeChildCollections:@
removeChildCollections :: IsPHCollectionListChangeRequest phCollectionListChangeRequest => phCollectionListChangeRequest -> RawId -> IO ()
removeChildCollections phCollectionListChangeRequest collections =
  sendMessage phCollectionListChangeRequest removeChildCollectionsSelector collections

-- | @- removeChildCollectionsAtIndexes:@
removeChildCollectionsAtIndexes :: (IsPHCollectionListChangeRequest phCollectionListChangeRequest, IsNSIndexSet indexes) => phCollectionListChangeRequest -> indexes -> IO ()
removeChildCollectionsAtIndexes phCollectionListChangeRequest indexes =
  sendMessage phCollectionListChangeRequest removeChildCollectionsAtIndexesSelector (toNSIndexSet indexes)

-- | @- replaceChildCollectionsAtIndexes:withChildCollections:@
replaceChildCollectionsAtIndexes_withChildCollections :: (IsPHCollectionListChangeRequest phCollectionListChangeRequest, IsNSIndexSet indexes) => phCollectionListChangeRequest -> indexes -> RawId -> IO ()
replaceChildCollectionsAtIndexes_withChildCollections phCollectionListChangeRequest indexes collections =
  sendMessage phCollectionListChangeRequest replaceChildCollectionsAtIndexes_withChildCollectionsSelector (toNSIndexSet indexes) collections

-- | @- moveChildCollectionsAtIndexes:toIndex:@
moveChildCollectionsAtIndexes_toIndex :: (IsPHCollectionListChangeRequest phCollectionListChangeRequest, IsNSIndexSet indexes) => phCollectionListChangeRequest -> indexes -> CULong -> IO ()
moveChildCollectionsAtIndexes_toIndex phCollectionListChangeRequest indexes toIndex =
  sendMessage phCollectionListChangeRequest moveChildCollectionsAtIndexes_toIndexSelector (toNSIndexSet indexes) toIndex

-- | @- placeholderForCreatedCollectionList@
placeholderForCreatedCollectionList :: IsPHCollectionListChangeRequest phCollectionListChangeRequest => phCollectionListChangeRequest -> IO (Id PHObjectPlaceholder)
placeholderForCreatedCollectionList phCollectionListChangeRequest =
  sendMessage phCollectionListChangeRequest placeholderForCreatedCollectionListSelector

-- | @- title@
title :: IsPHCollectionListChangeRequest phCollectionListChangeRequest => phCollectionListChangeRequest -> IO (Id NSString)
title phCollectionListChangeRequest =
  sendMessage phCollectionListChangeRequest titleSelector

-- | @- setTitle:@
setTitle :: (IsPHCollectionListChangeRequest phCollectionListChangeRequest, IsNSString value) => phCollectionListChangeRequest -> value -> IO ()
setTitle phCollectionListChangeRequest value =
  sendMessage phCollectionListChangeRequest setTitleSelector (toNSString value)

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @creationRequestForCollectionListWithTitle:@
creationRequestForCollectionListWithTitleSelector :: Selector '[Id NSString] (Id PHCollectionListChangeRequest)
creationRequestForCollectionListWithTitleSelector = mkSelector "creationRequestForCollectionListWithTitle:"

-- | @Selector@ for @deleteCollectionLists:@
deleteCollectionListsSelector :: Selector '[RawId] ()
deleteCollectionListsSelector = mkSelector "deleteCollectionLists:"

-- | @Selector@ for @changeRequestForCollectionList:@
changeRequestForCollectionListSelector :: Selector '[Id PHCollectionList] (Id PHCollectionListChangeRequest)
changeRequestForCollectionListSelector = mkSelector "changeRequestForCollectionList:"

-- | @Selector@ for @changeRequestForCollectionList:childCollections:@
changeRequestForCollectionList_childCollectionsSelector :: Selector '[Id PHCollectionList, Id PHFetchResult] (Id PHCollectionListChangeRequest)
changeRequestForCollectionList_childCollectionsSelector = mkSelector "changeRequestForCollectionList:childCollections:"

-- | @Selector@ for @changeRequestForTopLevelCollectionListUserCollections:@
changeRequestForTopLevelCollectionListUserCollectionsSelector :: Selector '[Id PHFetchResult] (Id PHCollectionListChangeRequest)
changeRequestForTopLevelCollectionListUserCollectionsSelector = mkSelector "changeRequestForTopLevelCollectionListUserCollections:"

-- | @Selector@ for @addChildCollections:@
addChildCollectionsSelector :: Selector '[RawId] ()
addChildCollectionsSelector = mkSelector "addChildCollections:"

-- | @Selector@ for @insertChildCollections:atIndexes:@
insertChildCollections_atIndexesSelector :: Selector '[RawId, Id NSIndexSet] ()
insertChildCollections_atIndexesSelector = mkSelector "insertChildCollections:atIndexes:"

-- | @Selector@ for @removeChildCollections:@
removeChildCollectionsSelector :: Selector '[RawId] ()
removeChildCollectionsSelector = mkSelector "removeChildCollections:"

-- | @Selector@ for @removeChildCollectionsAtIndexes:@
removeChildCollectionsAtIndexesSelector :: Selector '[Id NSIndexSet] ()
removeChildCollectionsAtIndexesSelector = mkSelector "removeChildCollectionsAtIndexes:"

-- | @Selector@ for @replaceChildCollectionsAtIndexes:withChildCollections:@
replaceChildCollectionsAtIndexes_withChildCollectionsSelector :: Selector '[Id NSIndexSet, RawId] ()
replaceChildCollectionsAtIndexes_withChildCollectionsSelector = mkSelector "replaceChildCollectionsAtIndexes:withChildCollections:"

-- | @Selector@ for @moveChildCollectionsAtIndexes:toIndex:@
moveChildCollectionsAtIndexes_toIndexSelector :: Selector '[Id NSIndexSet, CULong] ()
moveChildCollectionsAtIndexes_toIndexSelector = mkSelector "moveChildCollectionsAtIndexes:toIndex:"

-- | @Selector@ for @placeholderForCreatedCollectionList@
placeholderForCreatedCollectionListSelector :: Selector '[] (Id PHObjectPlaceholder)
placeholderForCreatedCollectionListSelector = mkSelector "placeholderForCreatedCollectionList"

-- | @Selector@ for @title@
titleSelector :: Selector '[] (Id NSString)
titleSelector = mkSelector "title"

-- | @Selector@ for @setTitle:@
setTitleSelector :: Selector '[Id NSString] ()
setTitleSelector = mkSelector "setTitle:"

