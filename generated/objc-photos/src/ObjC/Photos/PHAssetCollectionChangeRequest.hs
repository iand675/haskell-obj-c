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
  , creationRequestForAssetCollectionWithTitleSelector
  , deleteAssetCollectionsSelector
  , changeRequestForAssetCollectionSelector
  , changeRequestForAssetCollection_assetsSelector
  , addAssetsSelector
  , insertAssets_atIndexesSelector
  , removeAssetsSelector
  , removeAssetsAtIndexesSelector
  , replaceAssetsAtIndexes_withAssetsSelector
  , moveAssetsAtIndexes_toIndexSelector
  , placeholderForCreatedAssetCollectionSelector
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

-- | @+ creationRequestForAssetCollectionWithTitle:@
creationRequestForAssetCollectionWithTitle :: IsNSString title => title -> IO (Id PHAssetCollectionChangeRequest)
creationRequestForAssetCollectionWithTitle title =
  do
    cls' <- getRequiredClass "PHAssetCollectionChangeRequest"
    withObjCPtr title $ \raw_title ->
      sendClassMsg cls' (mkSelector "creationRequestForAssetCollectionWithTitle:") (retPtr retVoid) [argPtr (castPtr raw_title :: Ptr ())] >>= retainedObject . castPtr

-- | @+ deleteAssetCollections:@
deleteAssetCollections :: RawId -> IO ()
deleteAssetCollections assetCollections =
  do
    cls' <- getRequiredClass "PHAssetCollectionChangeRequest"
    sendClassMsg cls' (mkSelector "deleteAssetCollections:") retVoid [argPtr (castPtr (unRawId assetCollections) :: Ptr ())]

-- | @+ changeRequestForAssetCollection:@
changeRequestForAssetCollection :: IsPHAssetCollection assetCollection => assetCollection -> IO (Id PHAssetCollectionChangeRequest)
changeRequestForAssetCollection assetCollection =
  do
    cls' <- getRequiredClass "PHAssetCollectionChangeRequest"
    withObjCPtr assetCollection $ \raw_assetCollection ->
      sendClassMsg cls' (mkSelector "changeRequestForAssetCollection:") (retPtr retVoid) [argPtr (castPtr raw_assetCollection :: Ptr ())] >>= retainedObject . castPtr

-- | @+ changeRequestForAssetCollection:assets:@
changeRequestForAssetCollection_assets :: (IsPHAssetCollection assetCollection, IsPHFetchResult assets) => assetCollection -> assets -> IO (Id PHAssetCollectionChangeRequest)
changeRequestForAssetCollection_assets assetCollection assets =
  do
    cls' <- getRequiredClass "PHAssetCollectionChangeRequest"
    withObjCPtr assetCollection $ \raw_assetCollection ->
      withObjCPtr assets $ \raw_assets ->
        sendClassMsg cls' (mkSelector "changeRequestForAssetCollection:assets:") (retPtr retVoid) [argPtr (castPtr raw_assetCollection :: Ptr ()), argPtr (castPtr raw_assets :: Ptr ())] >>= retainedObject . castPtr

-- | @- addAssets:@
addAssets :: IsPHAssetCollectionChangeRequest phAssetCollectionChangeRequest => phAssetCollectionChangeRequest -> RawId -> IO ()
addAssets phAssetCollectionChangeRequest  assets =
  sendMsg phAssetCollectionChangeRequest (mkSelector "addAssets:") retVoid [argPtr (castPtr (unRawId assets) :: Ptr ())]

-- | @- insertAssets:atIndexes:@
insertAssets_atIndexes :: (IsPHAssetCollectionChangeRequest phAssetCollectionChangeRequest, IsNSIndexSet indexes) => phAssetCollectionChangeRequest -> RawId -> indexes -> IO ()
insertAssets_atIndexes phAssetCollectionChangeRequest  assets indexes =
withObjCPtr indexes $ \raw_indexes ->
    sendMsg phAssetCollectionChangeRequest (mkSelector "insertAssets:atIndexes:") retVoid [argPtr (castPtr (unRawId assets) :: Ptr ()), argPtr (castPtr raw_indexes :: Ptr ())]

-- | @- removeAssets:@
removeAssets :: IsPHAssetCollectionChangeRequest phAssetCollectionChangeRequest => phAssetCollectionChangeRequest -> RawId -> IO ()
removeAssets phAssetCollectionChangeRequest  assets =
  sendMsg phAssetCollectionChangeRequest (mkSelector "removeAssets:") retVoid [argPtr (castPtr (unRawId assets) :: Ptr ())]

-- | @- removeAssetsAtIndexes:@
removeAssetsAtIndexes :: (IsPHAssetCollectionChangeRequest phAssetCollectionChangeRequest, IsNSIndexSet indexes) => phAssetCollectionChangeRequest -> indexes -> IO ()
removeAssetsAtIndexes phAssetCollectionChangeRequest  indexes =
withObjCPtr indexes $ \raw_indexes ->
    sendMsg phAssetCollectionChangeRequest (mkSelector "removeAssetsAtIndexes:") retVoid [argPtr (castPtr raw_indexes :: Ptr ())]

-- | @- replaceAssetsAtIndexes:withAssets:@
replaceAssetsAtIndexes_withAssets :: (IsPHAssetCollectionChangeRequest phAssetCollectionChangeRequest, IsNSIndexSet indexes) => phAssetCollectionChangeRequest -> indexes -> RawId -> IO ()
replaceAssetsAtIndexes_withAssets phAssetCollectionChangeRequest  indexes assets =
withObjCPtr indexes $ \raw_indexes ->
    sendMsg phAssetCollectionChangeRequest (mkSelector "replaceAssetsAtIndexes:withAssets:") retVoid [argPtr (castPtr raw_indexes :: Ptr ()), argPtr (castPtr (unRawId assets) :: Ptr ())]

-- | @- moveAssetsAtIndexes:toIndex:@
moveAssetsAtIndexes_toIndex :: (IsPHAssetCollectionChangeRequest phAssetCollectionChangeRequest, IsNSIndexSet fromIndexes) => phAssetCollectionChangeRequest -> fromIndexes -> CULong -> IO ()
moveAssetsAtIndexes_toIndex phAssetCollectionChangeRequest  fromIndexes toIndex =
withObjCPtr fromIndexes $ \raw_fromIndexes ->
    sendMsg phAssetCollectionChangeRequest (mkSelector "moveAssetsAtIndexes:toIndex:") retVoid [argPtr (castPtr raw_fromIndexes :: Ptr ()), argCULong (fromIntegral toIndex)]

-- | @- placeholderForCreatedAssetCollection@
placeholderForCreatedAssetCollection :: IsPHAssetCollectionChangeRequest phAssetCollectionChangeRequest => phAssetCollectionChangeRequest -> IO (Id PHObjectPlaceholder)
placeholderForCreatedAssetCollection phAssetCollectionChangeRequest  =
  sendMsg phAssetCollectionChangeRequest (mkSelector "placeholderForCreatedAssetCollection") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- title@
title :: IsPHAssetCollectionChangeRequest phAssetCollectionChangeRequest => phAssetCollectionChangeRequest -> IO (Id NSString)
title phAssetCollectionChangeRequest  =
  sendMsg phAssetCollectionChangeRequest (mkSelector "title") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- setTitle:@
setTitle :: (IsPHAssetCollectionChangeRequest phAssetCollectionChangeRequest, IsNSString value) => phAssetCollectionChangeRequest -> value -> IO ()
setTitle phAssetCollectionChangeRequest  value =
withObjCPtr value $ \raw_value ->
    sendMsg phAssetCollectionChangeRequest (mkSelector "setTitle:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @creationRequestForAssetCollectionWithTitle:@
creationRequestForAssetCollectionWithTitleSelector :: Selector
creationRequestForAssetCollectionWithTitleSelector = mkSelector "creationRequestForAssetCollectionWithTitle:"

-- | @Selector@ for @deleteAssetCollections:@
deleteAssetCollectionsSelector :: Selector
deleteAssetCollectionsSelector = mkSelector "deleteAssetCollections:"

-- | @Selector@ for @changeRequestForAssetCollection:@
changeRequestForAssetCollectionSelector :: Selector
changeRequestForAssetCollectionSelector = mkSelector "changeRequestForAssetCollection:"

-- | @Selector@ for @changeRequestForAssetCollection:assets:@
changeRequestForAssetCollection_assetsSelector :: Selector
changeRequestForAssetCollection_assetsSelector = mkSelector "changeRequestForAssetCollection:assets:"

-- | @Selector@ for @addAssets:@
addAssetsSelector :: Selector
addAssetsSelector = mkSelector "addAssets:"

-- | @Selector@ for @insertAssets:atIndexes:@
insertAssets_atIndexesSelector :: Selector
insertAssets_atIndexesSelector = mkSelector "insertAssets:atIndexes:"

-- | @Selector@ for @removeAssets:@
removeAssetsSelector :: Selector
removeAssetsSelector = mkSelector "removeAssets:"

-- | @Selector@ for @removeAssetsAtIndexes:@
removeAssetsAtIndexesSelector :: Selector
removeAssetsAtIndexesSelector = mkSelector "removeAssetsAtIndexes:"

-- | @Selector@ for @replaceAssetsAtIndexes:withAssets:@
replaceAssetsAtIndexes_withAssetsSelector :: Selector
replaceAssetsAtIndexes_withAssetsSelector = mkSelector "replaceAssetsAtIndexes:withAssets:"

-- | @Selector@ for @moveAssetsAtIndexes:toIndex:@
moveAssetsAtIndexes_toIndexSelector :: Selector
moveAssetsAtIndexes_toIndexSelector = mkSelector "moveAssetsAtIndexes:toIndex:"

-- | @Selector@ for @placeholderForCreatedAssetCollection@
placeholderForCreatedAssetCollectionSelector :: Selector
placeholderForCreatedAssetCollectionSelector = mkSelector "placeholderForCreatedAssetCollection"

-- | @Selector@ for @title@
titleSelector :: Selector
titleSelector = mkSelector "title"

-- | @Selector@ for @setTitle:@
setTitleSelector :: Selector
setTitleSelector = mkSelector "setTitle:"

