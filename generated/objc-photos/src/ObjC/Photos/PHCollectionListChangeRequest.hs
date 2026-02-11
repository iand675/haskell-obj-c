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
  , creationRequestForCollectionListWithTitleSelector
  , deleteCollectionListsSelector
  , changeRequestForCollectionListSelector
  , changeRequestForCollectionList_childCollectionsSelector
  , changeRequestForTopLevelCollectionListUserCollectionsSelector
  , addChildCollectionsSelector
  , insertChildCollections_atIndexesSelector
  , removeChildCollectionsSelector
  , removeChildCollectionsAtIndexesSelector
  , replaceChildCollectionsAtIndexes_withChildCollectionsSelector
  , moveChildCollectionsAtIndexes_toIndexSelector
  , placeholderForCreatedCollectionListSelector
  , titleSelector
  , setTitleSelector


  ) where

import Foreign.Ptr (Ptr, nullPtr, castPtr)
import Foreign.LibFFI
import Foreign.C.Types
import Data.Int (Int8, Int16)
import Data.Word (Word16)
import Data.Coerce (coerce)

import ObjC.Runtime.Types
import ObjC.Runtime.MsgSend (sendMsg, sendClassMsg)
import ObjC.Runtime.Selector (mkSelector)
import ObjC.Runtime.Class (getRequiredClass)

import ObjC.Photos.Internal.Classes
import ObjC.Foundation.Internal.Classes

-- | @+ creationRequestForCollectionListWithTitle:@
creationRequestForCollectionListWithTitle :: IsNSString title => title -> IO (Id PHCollectionListChangeRequest)
creationRequestForCollectionListWithTitle title =
  do
    cls' <- getRequiredClass "PHCollectionListChangeRequest"
    withObjCPtr title $ \raw_title ->
      sendClassMsg cls' (mkSelector "creationRequestForCollectionListWithTitle:") (retPtr retVoid) [argPtr (castPtr raw_title :: Ptr ())] >>= retainedObject . castPtr

-- | @+ deleteCollectionLists:@
deleteCollectionLists :: RawId -> IO ()
deleteCollectionLists collectionLists =
  do
    cls' <- getRequiredClass "PHCollectionListChangeRequest"
    sendClassMsg cls' (mkSelector "deleteCollectionLists:") retVoid [argPtr (castPtr (unRawId collectionLists) :: Ptr ())]

-- | @+ changeRequestForCollectionList:@
changeRequestForCollectionList :: IsPHCollectionList collectionList => collectionList -> IO (Id PHCollectionListChangeRequest)
changeRequestForCollectionList collectionList =
  do
    cls' <- getRequiredClass "PHCollectionListChangeRequest"
    withObjCPtr collectionList $ \raw_collectionList ->
      sendClassMsg cls' (mkSelector "changeRequestForCollectionList:") (retPtr retVoid) [argPtr (castPtr raw_collectionList :: Ptr ())] >>= retainedObject . castPtr

-- | @+ changeRequestForCollectionList:childCollections:@
changeRequestForCollectionList_childCollections :: (IsPHCollectionList collectionList, IsPHFetchResult childCollections) => collectionList -> childCollections -> IO (Id PHCollectionListChangeRequest)
changeRequestForCollectionList_childCollections collectionList childCollections =
  do
    cls' <- getRequiredClass "PHCollectionListChangeRequest"
    withObjCPtr collectionList $ \raw_collectionList ->
      withObjCPtr childCollections $ \raw_childCollections ->
        sendClassMsg cls' (mkSelector "changeRequestForCollectionList:childCollections:") (retPtr retVoid) [argPtr (castPtr raw_collectionList :: Ptr ()), argPtr (castPtr raw_childCollections :: Ptr ())] >>= retainedObject . castPtr

-- | @+ changeRequestForTopLevelCollectionListUserCollections:@
changeRequestForTopLevelCollectionListUserCollections :: IsPHFetchResult childCollections => childCollections -> IO (Id PHCollectionListChangeRequest)
changeRequestForTopLevelCollectionListUserCollections childCollections =
  do
    cls' <- getRequiredClass "PHCollectionListChangeRequest"
    withObjCPtr childCollections $ \raw_childCollections ->
      sendClassMsg cls' (mkSelector "changeRequestForTopLevelCollectionListUserCollections:") (retPtr retVoid) [argPtr (castPtr raw_childCollections :: Ptr ())] >>= retainedObject . castPtr

-- | @- addChildCollections:@
addChildCollections :: IsPHCollectionListChangeRequest phCollectionListChangeRequest => phCollectionListChangeRequest -> RawId -> IO ()
addChildCollections phCollectionListChangeRequest  collections =
  sendMsg phCollectionListChangeRequest (mkSelector "addChildCollections:") retVoid [argPtr (castPtr (unRawId collections) :: Ptr ())]

-- | @- insertChildCollections:atIndexes:@
insertChildCollections_atIndexes :: (IsPHCollectionListChangeRequest phCollectionListChangeRequest, IsNSIndexSet indexes) => phCollectionListChangeRequest -> RawId -> indexes -> IO ()
insertChildCollections_atIndexes phCollectionListChangeRequest  collections indexes =
withObjCPtr indexes $ \raw_indexes ->
    sendMsg phCollectionListChangeRequest (mkSelector "insertChildCollections:atIndexes:") retVoid [argPtr (castPtr (unRawId collections) :: Ptr ()), argPtr (castPtr raw_indexes :: Ptr ())]

-- | @- removeChildCollections:@
removeChildCollections :: IsPHCollectionListChangeRequest phCollectionListChangeRequest => phCollectionListChangeRequest -> RawId -> IO ()
removeChildCollections phCollectionListChangeRequest  collections =
  sendMsg phCollectionListChangeRequest (mkSelector "removeChildCollections:") retVoid [argPtr (castPtr (unRawId collections) :: Ptr ())]

-- | @- removeChildCollectionsAtIndexes:@
removeChildCollectionsAtIndexes :: (IsPHCollectionListChangeRequest phCollectionListChangeRequest, IsNSIndexSet indexes) => phCollectionListChangeRequest -> indexes -> IO ()
removeChildCollectionsAtIndexes phCollectionListChangeRequest  indexes =
withObjCPtr indexes $ \raw_indexes ->
    sendMsg phCollectionListChangeRequest (mkSelector "removeChildCollectionsAtIndexes:") retVoid [argPtr (castPtr raw_indexes :: Ptr ())]

-- | @- replaceChildCollectionsAtIndexes:withChildCollections:@
replaceChildCollectionsAtIndexes_withChildCollections :: (IsPHCollectionListChangeRequest phCollectionListChangeRequest, IsNSIndexSet indexes) => phCollectionListChangeRequest -> indexes -> RawId -> IO ()
replaceChildCollectionsAtIndexes_withChildCollections phCollectionListChangeRequest  indexes collections =
withObjCPtr indexes $ \raw_indexes ->
    sendMsg phCollectionListChangeRequest (mkSelector "replaceChildCollectionsAtIndexes:withChildCollections:") retVoid [argPtr (castPtr raw_indexes :: Ptr ()), argPtr (castPtr (unRawId collections) :: Ptr ())]

-- | @- moveChildCollectionsAtIndexes:toIndex:@
moveChildCollectionsAtIndexes_toIndex :: (IsPHCollectionListChangeRequest phCollectionListChangeRequest, IsNSIndexSet indexes) => phCollectionListChangeRequest -> indexes -> CULong -> IO ()
moveChildCollectionsAtIndexes_toIndex phCollectionListChangeRequest  indexes toIndex =
withObjCPtr indexes $ \raw_indexes ->
    sendMsg phCollectionListChangeRequest (mkSelector "moveChildCollectionsAtIndexes:toIndex:") retVoid [argPtr (castPtr raw_indexes :: Ptr ()), argCULong (fromIntegral toIndex)]

-- | @- placeholderForCreatedCollectionList@
placeholderForCreatedCollectionList :: IsPHCollectionListChangeRequest phCollectionListChangeRequest => phCollectionListChangeRequest -> IO (Id PHObjectPlaceholder)
placeholderForCreatedCollectionList phCollectionListChangeRequest  =
  sendMsg phCollectionListChangeRequest (mkSelector "placeholderForCreatedCollectionList") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- title@
title :: IsPHCollectionListChangeRequest phCollectionListChangeRequest => phCollectionListChangeRequest -> IO (Id NSString)
title phCollectionListChangeRequest  =
  sendMsg phCollectionListChangeRequest (mkSelector "title") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- setTitle:@
setTitle :: (IsPHCollectionListChangeRequest phCollectionListChangeRequest, IsNSString value) => phCollectionListChangeRequest -> value -> IO ()
setTitle phCollectionListChangeRequest  value =
withObjCPtr value $ \raw_value ->
    sendMsg phCollectionListChangeRequest (mkSelector "setTitle:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @creationRequestForCollectionListWithTitle:@
creationRequestForCollectionListWithTitleSelector :: Selector
creationRequestForCollectionListWithTitleSelector = mkSelector "creationRequestForCollectionListWithTitle:"

-- | @Selector@ for @deleteCollectionLists:@
deleteCollectionListsSelector :: Selector
deleteCollectionListsSelector = mkSelector "deleteCollectionLists:"

-- | @Selector@ for @changeRequestForCollectionList:@
changeRequestForCollectionListSelector :: Selector
changeRequestForCollectionListSelector = mkSelector "changeRequestForCollectionList:"

-- | @Selector@ for @changeRequestForCollectionList:childCollections:@
changeRequestForCollectionList_childCollectionsSelector :: Selector
changeRequestForCollectionList_childCollectionsSelector = mkSelector "changeRequestForCollectionList:childCollections:"

-- | @Selector@ for @changeRequestForTopLevelCollectionListUserCollections:@
changeRequestForTopLevelCollectionListUserCollectionsSelector :: Selector
changeRequestForTopLevelCollectionListUserCollectionsSelector = mkSelector "changeRequestForTopLevelCollectionListUserCollections:"

-- | @Selector@ for @addChildCollections:@
addChildCollectionsSelector :: Selector
addChildCollectionsSelector = mkSelector "addChildCollections:"

-- | @Selector@ for @insertChildCollections:atIndexes:@
insertChildCollections_atIndexesSelector :: Selector
insertChildCollections_atIndexesSelector = mkSelector "insertChildCollections:atIndexes:"

-- | @Selector@ for @removeChildCollections:@
removeChildCollectionsSelector :: Selector
removeChildCollectionsSelector = mkSelector "removeChildCollections:"

-- | @Selector@ for @removeChildCollectionsAtIndexes:@
removeChildCollectionsAtIndexesSelector :: Selector
removeChildCollectionsAtIndexesSelector = mkSelector "removeChildCollectionsAtIndexes:"

-- | @Selector@ for @replaceChildCollectionsAtIndexes:withChildCollections:@
replaceChildCollectionsAtIndexes_withChildCollectionsSelector :: Selector
replaceChildCollectionsAtIndexes_withChildCollectionsSelector = mkSelector "replaceChildCollectionsAtIndexes:withChildCollections:"

-- | @Selector@ for @moveChildCollectionsAtIndexes:toIndex:@
moveChildCollectionsAtIndexes_toIndexSelector :: Selector
moveChildCollectionsAtIndexes_toIndexSelector = mkSelector "moveChildCollectionsAtIndexes:toIndex:"

-- | @Selector@ for @placeholderForCreatedCollectionList@
placeholderForCreatedCollectionListSelector :: Selector
placeholderForCreatedCollectionListSelector = mkSelector "placeholderForCreatedCollectionList"

-- | @Selector@ for @title@
titleSelector :: Selector
titleSelector = mkSelector "title"

-- | @Selector@ for @setTitle:@
setTitleSelector :: Selector
setTitleSelector = mkSelector "setTitle:"

