{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Generated bindings for @PHFetchOptions@.
module ObjC.Photos.PHFetchOptions
  ( PHFetchOptions
  , IsPHFetchOptions(..)
  , predicate
  , setPredicate
  , sortDescriptors
  , setSortDescriptors
  , includeHiddenAssets
  , setIncludeHiddenAssets
  , includeAllBurstAssets
  , setIncludeAllBurstAssets
  , includeAssetSourceTypes
  , setIncludeAssetSourceTypes
  , fetchLimit
  , setFetchLimit
  , wantsIncrementalChangeDetails
  , setWantsIncrementalChangeDetails
  , fetchLimitSelector
  , includeAllBurstAssetsSelector
  , includeAssetSourceTypesSelector
  , includeHiddenAssetsSelector
  , predicateSelector
  , setFetchLimitSelector
  , setIncludeAllBurstAssetsSelector
  , setIncludeAssetSourceTypesSelector
  , setIncludeHiddenAssetsSelector
  , setPredicateSelector
  , setSortDescriptorsSelector
  , setWantsIncrementalChangeDetailsSelector
  , sortDescriptorsSelector
  , wantsIncrementalChangeDetailsSelector

  -- * Enum types
  , PHAssetSourceType(PHAssetSourceType)
  , pattern PHAssetSourceTypeNone
  , pattern PHAssetSourceTypeUserLibrary
  , pattern PHAssetSourceTypeCloudShared
  , pattern PHAssetSourceTypeiTunesSynced

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

-- | @- predicate@
predicate :: IsPHFetchOptions phFetchOptions => phFetchOptions -> IO (Id NSPredicate)
predicate phFetchOptions =
  sendMessage phFetchOptions predicateSelector

-- | @- setPredicate:@
setPredicate :: (IsPHFetchOptions phFetchOptions, IsNSPredicate value) => phFetchOptions -> value -> IO ()
setPredicate phFetchOptions value =
  sendMessage phFetchOptions setPredicateSelector (toNSPredicate value)

-- | @- sortDescriptors@
sortDescriptors :: IsPHFetchOptions phFetchOptions => phFetchOptions -> IO (Id NSArray)
sortDescriptors phFetchOptions =
  sendMessage phFetchOptions sortDescriptorsSelector

-- | @- setSortDescriptors:@
setSortDescriptors :: (IsPHFetchOptions phFetchOptions, IsNSArray value) => phFetchOptions -> value -> IO ()
setSortDescriptors phFetchOptions value =
  sendMessage phFetchOptions setSortDescriptorsSelector (toNSArray value)

-- | @- includeHiddenAssets@
includeHiddenAssets :: IsPHFetchOptions phFetchOptions => phFetchOptions -> IO Bool
includeHiddenAssets phFetchOptions =
  sendMessage phFetchOptions includeHiddenAssetsSelector

-- | @- setIncludeHiddenAssets:@
setIncludeHiddenAssets :: IsPHFetchOptions phFetchOptions => phFetchOptions -> Bool -> IO ()
setIncludeHiddenAssets phFetchOptions value =
  sendMessage phFetchOptions setIncludeHiddenAssetsSelector value

-- | @- includeAllBurstAssets@
includeAllBurstAssets :: IsPHFetchOptions phFetchOptions => phFetchOptions -> IO Bool
includeAllBurstAssets phFetchOptions =
  sendMessage phFetchOptions includeAllBurstAssetsSelector

-- | @- setIncludeAllBurstAssets:@
setIncludeAllBurstAssets :: IsPHFetchOptions phFetchOptions => phFetchOptions -> Bool -> IO ()
setIncludeAllBurstAssets phFetchOptions value =
  sendMessage phFetchOptions setIncludeAllBurstAssetsSelector value

-- | @- includeAssetSourceTypes@
includeAssetSourceTypes :: IsPHFetchOptions phFetchOptions => phFetchOptions -> IO PHAssetSourceType
includeAssetSourceTypes phFetchOptions =
  sendMessage phFetchOptions includeAssetSourceTypesSelector

-- | @- setIncludeAssetSourceTypes:@
setIncludeAssetSourceTypes :: IsPHFetchOptions phFetchOptions => phFetchOptions -> PHAssetSourceType -> IO ()
setIncludeAssetSourceTypes phFetchOptions value =
  sendMessage phFetchOptions setIncludeAssetSourceTypesSelector value

-- | @- fetchLimit@
fetchLimit :: IsPHFetchOptions phFetchOptions => phFetchOptions -> IO CULong
fetchLimit phFetchOptions =
  sendMessage phFetchOptions fetchLimitSelector

-- | @- setFetchLimit:@
setFetchLimit :: IsPHFetchOptions phFetchOptions => phFetchOptions -> CULong -> IO ()
setFetchLimit phFetchOptions value =
  sendMessage phFetchOptions setFetchLimitSelector value

-- | @- wantsIncrementalChangeDetails@
wantsIncrementalChangeDetails :: IsPHFetchOptions phFetchOptions => phFetchOptions -> IO Bool
wantsIncrementalChangeDetails phFetchOptions =
  sendMessage phFetchOptions wantsIncrementalChangeDetailsSelector

-- | @- setWantsIncrementalChangeDetails:@
setWantsIncrementalChangeDetails :: IsPHFetchOptions phFetchOptions => phFetchOptions -> Bool -> IO ()
setWantsIncrementalChangeDetails phFetchOptions value =
  sendMessage phFetchOptions setWantsIncrementalChangeDetailsSelector value

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @predicate@
predicateSelector :: Selector '[] (Id NSPredicate)
predicateSelector = mkSelector "predicate"

-- | @Selector@ for @setPredicate:@
setPredicateSelector :: Selector '[Id NSPredicate] ()
setPredicateSelector = mkSelector "setPredicate:"

-- | @Selector@ for @sortDescriptors@
sortDescriptorsSelector :: Selector '[] (Id NSArray)
sortDescriptorsSelector = mkSelector "sortDescriptors"

-- | @Selector@ for @setSortDescriptors:@
setSortDescriptorsSelector :: Selector '[Id NSArray] ()
setSortDescriptorsSelector = mkSelector "setSortDescriptors:"

-- | @Selector@ for @includeHiddenAssets@
includeHiddenAssetsSelector :: Selector '[] Bool
includeHiddenAssetsSelector = mkSelector "includeHiddenAssets"

-- | @Selector@ for @setIncludeHiddenAssets:@
setIncludeHiddenAssetsSelector :: Selector '[Bool] ()
setIncludeHiddenAssetsSelector = mkSelector "setIncludeHiddenAssets:"

-- | @Selector@ for @includeAllBurstAssets@
includeAllBurstAssetsSelector :: Selector '[] Bool
includeAllBurstAssetsSelector = mkSelector "includeAllBurstAssets"

-- | @Selector@ for @setIncludeAllBurstAssets:@
setIncludeAllBurstAssetsSelector :: Selector '[Bool] ()
setIncludeAllBurstAssetsSelector = mkSelector "setIncludeAllBurstAssets:"

-- | @Selector@ for @includeAssetSourceTypes@
includeAssetSourceTypesSelector :: Selector '[] PHAssetSourceType
includeAssetSourceTypesSelector = mkSelector "includeAssetSourceTypes"

-- | @Selector@ for @setIncludeAssetSourceTypes:@
setIncludeAssetSourceTypesSelector :: Selector '[PHAssetSourceType] ()
setIncludeAssetSourceTypesSelector = mkSelector "setIncludeAssetSourceTypes:"

-- | @Selector@ for @fetchLimit@
fetchLimitSelector :: Selector '[] CULong
fetchLimitSelector = mkSelector "fetchLimit"

-- | @Selector@ for @setFetchLimit:@
setFetchLimitSelector :: Selector '[CULong] ()
setFetchLimitSelector = mkSelector "setFetchLimit:"

-- | @Selector@ for @wantsIncrementalChangeDetails@
wantsIncrementalChangeDetailsSelector :: Selector '[] Bool
wantsIncrementalChangeDetailsSelector = mkSelector "wantsIncrementalChangeDetails"

-- | @Selector@ for @setWantsIncrementalChangeDetails:@
setWantsIncrementalChangeDetailsSelector :: Selector '[Bool] ()
setWantsIncrementalChangeDetailsSelector = mkSelector "setWantsIncrementalChangeDetails:"

