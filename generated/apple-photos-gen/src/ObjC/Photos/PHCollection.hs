{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE DataKinds #-}
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
  , localizedTitle
  , canContainAssetsSelector
  , canContainCollectionsSelector
  , canPerformEditOperationSelector
  , fetchCollectionsInCollectionList_optionsSelector
  , fetchTopLevelUserCollectionsWithOptionsSelector
  , localizedTitleSelector

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

import Foreign.Ptr (Ptr, FunPtr)
import Foreign.C.Types

import ObjC.Runtime.Types
import ObjC.Runtime.Message (sendMessage, sendOwnedMessage, sendClassMessage, sendOwnedClassMessage)
import ObjC.Runtime.Selector (mkSelector)
import ObjC.Runtime.Class (getRequiredClass)

import ObjC.Photos.Internal.Classes
import ObjC.Photos.Internal.Enums
import ObjC.Foundation.Internal.Classes

-- | @- canPerformEditOperation:@
canPerformEditOperation :: IsPHCollection phCollection => phCollection -> PHCollectionEditOperation -> IO Bool
canPerformEditOperation phCollection anOperation =
  sendMessage phCollection canPerformEditOperationSelector anOperation

-- | @+ fetchCollectionsInCollectionList:options:@
fetchCollectionsInCollectionList_options :: (IsPHCollectionList collectionList, IsPHFetchOptions options) => collectionList -> options -> IO (Id PHFetchResult)
fetchCollectionsInCollectionList_options collectionList options =
  do
    cls' <- getRequiredClass "PHCollection"
    sendClassMessage cls' fetchCollectionsInCollectionList_optionsSelector (toPHCollectionList collectionList) (toPHFetchOptions options)

-- | @+ fetchTopLevelUserCollectionsWithOptions:@
fetchTopLevelUserCollectionsWithOptions :: IsPHFetchOptions options => options -> IO (Id PHFetchResult)
fetchTopLevelUserCollectionsWithOptions options =
  do
    cls' <- getRequiredClass "PHCollection"
    sendClassMessage cls' fetchTopLevelUserCollectionsWithOptionsSelector (toPHFetchOptions options)

-- | @- canContainAssets@
canContainAssets :: IsPHCollection phCollection => phCollection -> IO Bool
canContainAssets phCollection =
  sendMessage phCollection canContainAssetsSelector

-- | @- canContainCollections@
canContainCollections :: IsPHCollection phCollection => phCollection -> IO Bool
canContainCollections phCollection =
  sendMessage phCollection canContainCollectionsSelector

-- | @- localizedTitle@
localizedTitle :: IsPHCollection phCollection => phCollection -> IO (Id NSString)
localizedTitle phCollection =
  sendMessage phCollection localizedTitleSelector

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @canPerformEditOperation:@
canPerformEditOperationSelector :: Selector '[PHCollectionEditOperation] Bool
canPerformEditOperationSelector = mkSelector "canPerformEditOperation:"

-- | @Selector@ for @fetchCollectionsInCollectionList:options:@
fetchCollectionsInCollectionList_optionsSelector :: Selector '[Id PHCollectionList, Id PHFetchOptions] (Id PHFetchResult)
fetchCollectionsInCollectionList_optionsSelector = mkSelector "fetchCollectionsInCollectionList:options:"

-- | @Selector@ for @fetchTopLevelUserCollectionsWithOptions:@
fetchTopLevelUserCollectionsWithOptionsSelector :: Selector '[Id PHFetchOptions] (Id PHFetchResult)
fetchTopLevelUserCollectionsWithOptionsSelector = mkSelector "fetchTopLevelUserCollectionsWithOptions:"

-- | @Selector@ for @canContainAssets@
canContainAssetsSelector :: Selector '[] Bool
canContainAssetsSelector = mkSelector "canContainAssets"

-- | @Selector@ for @canContainCollections@
canContainCollectionsSelector :: Selector '[] Bool
canContainCollectionsSelector = mkSelector "canContainCollections"

-- | @Selector@ for @localizedTitle@
localizedTitleSelector :: Selector '[] (Id NSString)
localizedTitleSelector = mkSelector "localizedTitle"

