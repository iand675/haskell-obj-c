{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Generated bindings for @PHAssetCollectionChangeRequest@.
module ObjC.Photos.PHAssetCollectionChangeRequest
  ( PHAssetCollectionChangeRequest
  , IsPHAssetCollectionChangeRequest(..)
  , creationRequestForAssetCollectionWithTitle
  , deleteAssetCollections
  , changeRequestForAssetCollection
  , changeRequestForAssetCollection_assets
  , addAssets
  , insertAssets_atIndexes
  , removeAssets
  , removeAssetsAtIndexes
  , replaceAssetsAtIndexes_withAssets
  , moveAssetsAtIndexes_toIndex
  , placeholderForCreatedAssetCollection
  , title
  , setTitle
  , addAssetsSelector
  , changeRequestForAssetCollectionSelector
  , changeRequestForAssetCollection_assetsSelector
  , creationRequestForAssetCollectionWithTitleSelector
  , deleteAssetCollectionsSelector
  , insertAssets_atIndexesSelector
  , moveAssetsAtIndexes_toIndexSelector
  , placeholderForCreatedAssetCollectionSelector
  , removeAssetsAtIndexesSelector
  , removeAssetsSelector
  , replaceAssetsAtIndexes_withAssetsSelector
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

-- | @+ creationRequestForAssetCollectionWithTitle:@
creationRequestForAssetCollectionWithTitle :: IsNSString title => title -> IO (Id PHAssetCollectionChangeRequest)
creationRequestForAssetCollectionWithTitle title =
  do
    cls' <- getRequiredClass "PHAssetCollectionChangeRequest"
    sendClassMessage cls' creationRequestForAssetCollectionWithTitleSelector (toNSString title)

-- | @+ deleteAssetCollections:@
deleteAssetCollections :: RawId -> IO ()
deleteAssetCollections assetCollections =
  do
    cls' <- getRequiredClass "PHAssetCollectionChangeRequest"
    sendClassMessage cls' deleteAssetCollectionsSelector assetCollections

-- | @+ changeRequestForAssetCollection:@
changeRequestForAssetCollection :: IsPHAssetCollection assetCollection => assetCollection -> IO (Id PHAssetCollectionChangeRequest)
changeRequestForAssetCollection assetCollection =
  do
    cls' <- getRequiredClass "PHAssetCollectionChangeRequest"
    sendClassMessage cls' changeRequestForAssetCollectionSelector (toPHAssetCollection assetCollection)

-- | @+ changeRequestForAssetCollection:assets:@
changeRequestForAssetCollection_assets :: (IsPHAssetCollection assetCollection, IsPHFetchResult assets) => assetCollection -> assets -> IO (Id PHAssetCollectionChangeRequest)
changeRequestForAssetCollection_assets assetCollection assets =
  do
    cls' <- getRequiredClass "PHAssetCollectionChangeRequest"
    sendClassMessage cls' changeRequestForAssetCollection_assetsSelector (toPHAssetCollection assetCollection) (toPHFetchResult assets)

-- | @- addAssets:@
addAssets :: IsPHAssetCollectionChangeRequest phAssetCollectionChangeRequest => phAssetCollectionChangeRequest -> RawId -> IO ()
addAssets phAssetCollectionChangeRequest assets =
  sendMessage phAssetCollectionChangeRequest addAssetsSelector assets

-- | @- insertAssets:atIndexes:@
insertAssets_atIndexes :: (IsPHAssetCollectionChangeRequest phAssetCollectionChangeRequest, IsNSIndexSet indexes) => phAssetCollectionChangeRequest -> RawId -> indexes -> IO ()
insertAssets_atIndexes phAssetCollectionChangeRequest assets indexes =
  sendMessage phAssetCollectionChangeRequest insertAssets_atIndexesSelector assets (toNSIndexSet indexes)

-- | @- removeAssets:@
removeAssets :: IsPHAssetCollectionChangeRequest phAssetCollectionChangeRequest => phAssetCollectionChangeRequest -> RawId -> IO ()
removeAssets phAssetCollectionChangeRequest assets =
  sendMessage phAssetCollectionChangeRequest removeAssetsSelector assets

-- | @- removeAssetsAtIndexes:@
removeAssetsAtIndexes :: (IsPHAssetCollectionChangeRequest phAssetCollectionChangeRequest, IsNSIndexSet indexes) => phAssetCollectionChangeRequest -> indexes -> IO ()
removeAssetsAtIndexes phAssetCollectionChangeRequest indexes =
  sendMessage phAssetCollectionChangeRequest removeAssetsAtIndexesSelector (toNSIndexSet indexes)

-- | @- replaceAssetsAtIndexes:withAssets:@
replaceAssetsAtIndexes_withAssets :: (IsPHAssetCollectionChangeRequest phAssetCollectionChangeRequest, IsNSIndexSet indexes) => phAssetCollectionChangeRequest -> indexes -> RawId -> IO ()
replaceAssetsAtIndexes_withAssets phAssetCollectionChangeRequest indexes assets =
  sendMessage phAssetCollectionChangeRequest replaceAssetsAtIndexes_withAssetsSelector (toNSIndexSet indexes) assets

-- | @- moveAssetsAtIndexes:toIndex:@
moveAssetsAtIndexes_toIndex :: (IsPHAssetCollectionChangeRequest phAssetCollectionChangeRequest, IsNSIndexSet fromIndexes) => phAssetCollectionChangeRequest -> fromIndexes -> CULong -> IO ()
moveAssetsAtIndexes_toIndex phAssetCollectionChangeRequest fromIndexes toIndex =
  sendMessage phAssetCollectionChangeRequest moveAssetsAtIndexes_toIndexSelector (toNSIndexSet fromIndexes) toIndex

-- | @- placeholderForCreatedAssetCollection@
placeholderForCreatedAssetCollection :: IsPHAssetCollectionChangeRequest phAssetCollectionChangeRequest => phAssetCollectionChangeRequest -> IO (Id PHObjectPlaceholder)
placeholderForCreatedAssetCollection phAssetCollectionChangeRequest =
  sendMessage phAssetCollectionChangeRequest placeholderForCreatedAssetCollectionSelector

-- | @- title@
title :: IsPHAssetCollectionChangeRequest phAssetCollectionChangeRequest => phAssetCollectionChangeRequest -> IO (Id NSString)
title phAssetCollectionChangeRequest =
  sendMessage phAssetCollectionChangeRequest titleSelector

-- | @- setTitle:@
setTitle :: (IsPHAssetCollectionChangeRequest phAssetCollectionChangeRequest, IsNSString value) => phAssetCollectionChangeRequest -> value -> IO ()
setTitle phAssetCollectionChangeRequest value =
  sendMessage phAssetCollectionChangeRequest setTitleSelector (toNSString value)

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @creationRequestForAssetCollectionWithTitle:@
creationRequestForAssetCollectionWithTitleSelector :: Selector '[Id NSString] (Id PHAssetCollectionChangeRequest)
creationRequestForAssetCollectionWithTitleSelector = mkSelector "creationRequestForAssetCollectionWithTitle:"

-- | @Selector@ for @deleteAssetCollections:@
deleteAssetCollectionsSelector :: Selector '[RawId] ()
deleteAssetCollectionsSelector = mkSelector "deleteAssetCollections:"

-- | @Selector@ for @changeRequestForAssetCollection:@
changeRequestForAssetCollectionSelector :: Selector '[Id PHAssetCollection] (Id PHAssetCollectionChangeRequest)
changeRequestForAssetCollectionSelector = mkSelector "changeRequestForAssetCollection:"

-- | @Selector@ for @changeRequestForAssetCollection:assets:@
changeRequestForAssetCollection_assetsSelector :: Selector '[Id PHAssetCollection, Id PHFetchResult] (Id PHAssetCollectionChangeRequest)
changeRequestForAssetCollection_assetsSelector = mkSelector "changeRequestForAssetCollection:assets:"

-- | @Selector@ for @addAssets:@
addAssetsSelector :: Selector '[RawId] ()
addAssetsSelector = mkSelector "addAssets:"

-- | @Selector@ for @insertAssets:atIndexes:@
insertAssets_atIndexesSelector :: Selector '[RawId, Id NSIndexSet] ()
insertAssets_atIndexesSelector = mkSelector "insertAssets:atIndexes:"

-- | @Selector@ for @removeAssets:@
removeAssetsSelector :: Selector '[RawId] ()
removeAssetsSelector = mkSelector "removeAssets:"

-- | @Selector@ for @removeAssetsAtIndexes:@
removeAssetsAtIndexesSelector :: Selector '[Id NSIndexSet] ()
removeAssetsAtIndexesSelector = mkSelector "removeAssetsAtIndexes:"

-- | @Selector@ for @replaceAssetsAtIndexes:withAssets:@
replaceAssetsAtIndexes_withAssetsSelector :: Selector '[Id NSIndexSet, RawId] ()
replaceAssetsAtIndexes_withAssetsSelector = mkSelector "replaceAssetsAtIndexes:withAssets:"

-- | @Selector@ for @moveAssetsAtIndexes:toIndex:@
moveAssetsAtIndexes_toIndexSelector :: Selector '[Id NSIndexSet, CULong] ()
moveAssetsAtIndexes_toIndexSelector = mkSelector "moveAssetsAtIndexes:toIndex:"

-- | @Selector@ for @placeholderForCreatedAssetCollection@
placeholderForCreatedAssetCollectionSelector :: Selector '[] (Id PHObjectPlaceholder)
placeholderForCreatedAssetCollectionSelector = mkSelector "placeholderForCreatedAssetCollection"

-- | @Selector@ for @title@
titleSelector :: Selector '[] (Id NSString)
titleSelector = mkSelector "title"

-- | @Selector@ for @setTitle:@
setTitleSelector :: Selector '[Id NSString] ()
setTitleSelector = mkSelector "setTitle:"

