{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Generated bindings for @PHCollection@.
module ObjC.Photos.PHCollection
  ( PHCollection
  , IsPHCollection(..)
  , canPerformEditOperation
  , fetchCollectionsInCollectionList_options
  , fetchTopLevelUserCollectionsWithOptions
  , canContainAssets
  , canContainCollections
  , canPerformEditOperationSelector
  , fetchCollectionsInCollectionList_optionsSelector
  , fetchTopLevelUserCollectionsWithOptionsSelector
  , canContainAssetsSelector
  , canContainCollectionsSelector

  -- * Enum types
  , PHCollectionEditOperation(PHCollectionEditOperation)
  , pattern PHCollectionEditOperationDeleteContent
  , pattern PHCollectionEditOperationRemoveContent
  , pattern PHCollectionEditOperationAddContent
  , pattern PHCollectionEditOperationCreateContent
  , pattern PHCollectionEditOperationRearrangeContent
  , pattern PHCollectionEditOperationDelete
  , pattern PHCollectionEditOperationRename

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
import ObjC.Photos.Internal.Enums
import ObjC.Foundation.Internal.Classes

-- | @- canPerformEditOperation:@
canPerformEditOperation :: IsPHCollection phCollection => phCollection -> PHCollectionEditOperation -> IO Bool
canPerformEditOperation phCollection  anOperation =
  fmap ((/= 0) :: CULong -> Bool) $ sendMsg phCollection (mkSelector "canPerformEditOperation:") retCULong [argCLong (coerce anOperation)]

-- | @+ fetchCollectionsInCollectionList:options:@
fetchCollectionsInCollectionList_options :: (IsPHCollectionList collectionList, IsPHFetchOptions options) => collectionList -> options -> IO (Id PHFetchResult)
fetchCollectionsInCollectionList_options collectionList options =
  do
    cls' <- getRequiredClass "PHCollection"
    withObjCPtr collectionList $ \raw_collectionList ->
      withObjCPtr options $ \raw_options ->
        sendClassMsg cls' (mkSelector "fetchCollectionsInCollectionList:options:") (retPtr retVoid) [argPtr (castPtr raw_collectionList :: Ptr ()), argPtr (castPtr raw_options :: Ptr ())] >>= retainedObject . castPtr

-- | @+ fetchTopLevelUserCollectionsWithOptions:@
fetchTopLevelUserCollectionsWithOptions :: IsPHFetchOptions options => options -> IO (Id PHFetchResult)
fetchTopLevelUserCollectionsWithOptions options =
  do
    cls' <- getRequiredClass "PHCollection"
    withObjCPtr options $ \raw_options ->
      sendClassMsg cls' (mkSelector "fetchTopLevelUserCollectionsWithOptions:") (retPtr retVoid) [argPtr (castPtr raw_options :: Ptr ())] >>= retainedObject . castPtr

-- | @- canContainAssets@
canContainAssets :: IsPHCollection phCollection => phCollection -> IO Bool
canContainAssets phCollection  =
  fmap ((/= 0) :: CULong -> Bool) $ sendMsg phCollection (mkSelector "canContainAssets") retCULong []

-- | @- canContainCollections@
canContainCollections :: IsPHCollection phCollection => phCollection -> IO Bool
canContainCollections phCollection  =
  fmap ((/= 0) :: CULong -> Bool) $ sendMsg phCollection (mkSelector "canContainCollections") retCULong []

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @canPerformEditOperation:@
canPerformEditOperationSelector :: Selector
canPerformEditOperationSelector = mkSelector "canPerformEditOperation:"

-- | @Selector@ for @fetchCollectionsInCollectionList:options:@
fetchCollectionsInCollectionList_optionsSelector :: Selector
fetchCollectionsInCollectionList_optionsSelector = mkSelector "fetchCollectionsInCollectionList:options:"

-- | @Selector@ for @fetchTopLevelUserCollectionsWithOptions:@
fetchTopLevelUserCollectionsWithOptionsSelector :: Selector
fetchTopLevelUserCollectionsWithOptionsSelector = mkSelector "fetchTopLevelUserCollectionsWithOptions:"

-- | @Selector@ for @canContainAssets@
canContainAssetsSelector :: Selector
canContainAssetsSelector = mkSelector "canContainAssets"

-- | @Selector@ for @canContainCollections@
canContainCollectionsSelector :: Selector
canContainCollectionsSelector = mkSelector "canContainCollections"

