{-# LANGUAGE PatternSynonyms #-}
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
  , predicateSelector
  , setPredicateSelector
  , sortDescriptorsSelector
  , setSortDescriptorsSelector
  , includeHiddenAssetsSelector
  , setIncludeHiddenAssetsSelector
  , includeAllBurstAssetsSelector
  , setIncludeAllBurstAssetsSelector
  , includeAssetSourceTypesSelector
  , setIncludeAssetSourceTypesSelector
  , fetchLimitSelector
  , setFetchLimitSelector
  , wantsIncrementalChangeDetailsSelector
  , setWantsIncrementalChangeDetailsSelector

  -- * Enum types
  , PHAssetSourceType(PHAssetSourceType)
  , pattern PHAssetSourceTypeNone
  , pattern PHAssetSourceTypeUserLibrary
  , pattern PHAssetSourceTypeCloudShared
  , pattern PHAssetSourceTypeiTunesSynced

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

-- | @- predicate@
predicate :: IsPHFetchOptions phFetchOptions => phFetchOptions -> IO (Id NSPredicate)
predicate phFetchOptions  =
  sendMsg phFetchOptions (mkSelector "predicate") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- setPredicate:@
setPredicate :: (IsPHFetchOptions phFetchOptions, IsNSPredicate value) => phFetchOptions -> value -> IO ()
setPredicate phFetchOptions  value =
withObjCPtr value $ \raw_value ->
    sendMsg phFetchOptions (mkSelector "setPredicate:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- | @- sortDescriptors@
sortDescriptors :: IsPHFetchOptions phFetchOptions => phFetchOptions -> IO (Id NSArray)
sortDescriptors phFetchOptions  =
  sendMsg phFetchOptions (mkSelector "sortDescriptors") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- setSortDescriptors:@
setSortDescriptors :: (IsPHFetchOptions phFetchOptions, IsNSArray value) => phFetchOptions -> value -> IO ()
setSortDescriptors phFetchOptions  value =
withObjCPtr value $ \raw_value ->
    sendMsg phFetchOptions (mkSelector "setSortDescriptors:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- | @- includeHiddenAssets@
includeHiddenAssets :: IsPHFetchOptions phFetchOptions => phFetchOptions -> IO Bool
includeHiddenAssets phFetchOptions  =
  fmap ((/= 0) :: CULong -> Bool) $ sendMsg phFetchOptions (mkSelector "includeHiddenAssets") retCULong []

-- | @- setIncludeHiddenAssets:@
setIncludeHiddenAssets :: IsPHFetchOptions phFetchOptions => phFetchOptions -> Bool -> IO ()
setIncludeHiddenAssets phFetchOptions  value =
  sendMsg phFetchOptions (mkSelector "setIncludeHiddenAssets:") retVoid [argCULong (if value then 1 else 0)]

-- | @- includeAllBurstAssets@
includeAllBurstAssets :: IsPHFetchOptions phFetchOptions => phFetchOptions -> IO Bool
includeAllBurstAssets phFetchOptions  =
  fmap ((/= 0) :: CULong -> Bool) $ sendMsg phFetchOptions (mkSelector "includeAllBurstAssets") retCULong []

-- | @- setIncludeAllBurstAssets:@
setIncludeAllBurstAssets :: IsPHFetchOptions phFetchOptions => phFetchOptions -> Bool -> IO ()
setIncludeAllBurstAssets phFetchOptions  value =
  sendMsg phFetchOptions (mkSelector "setIncludeAllBurstAssets:") retVoid [argCULong (if value then 1 else 0)]

-- | @- includeAssetSourceTypes@
includeAssetSourceTypes :: IsPHFetchOptions phFetchOptions => phFetchOptions -> IO PHAssetSourceType
includeAssetSourceTypes phFetchOptions  =
  fmap (coerce :: CULong -> PHAssetSourceType) $ sendMsg phFetchOptions (mkSelector "includeAssetSourceTypes") retCULong []

-- | @- setIncludeAssetSourceTypes:@
setIncludeAssetSourceTypes :: IsPHFetchOptions phFetchOptions => phFetchOptions -> PHAssetSourceType -> IO ()
setIncludeAssetSourceTypes phFetchOptions  value =
  sendMsg phFetchOptions (mkSelector "setIncludeAssetSourceTypes:") retVoid [argCULong (coerce value)]

-- | @- fetchLimit@
fetchLimit :: IsPHFetchOptions phFetchOptions => phFetchOptions -> IO CULong
fetchLimit phFetchOptions  =
  sendMsg phFetchOptions (mkSelector "fetchLimit") retCULong []

-- | @- setFetchLimit:@
setFetchLimit :: IsPHFetchOptions phFetchOptions => phFetchOptions -> CULong -> IO ()
setFetchLimit phFetchOptions  value =
  sendMsg phFetchOptions (mkSelector "setFetchLimit:") retVoid [argCULong (fromIntegral value)]

-- | @- wantsIncrementalChangeDetails@
wantsIncrementalChangeDetails :: IsPHFetchOptions phFetchOptions => phFetchOptions -> IO Bool
wantsIncrementalChangeDetails phFetchOptions  =
  fmap ((/= 0) :: CULong -> Bool) $ sendMsg phFetchOptions (mkSelector "wantsIncrementalChangeDetails") retCULong []

-- | @- setWantsIncrementalChangeDetails:@
setWantsIncrementalChangeDetails :: IsPHFetchOptions phFetchOptions => phFetchOptions -> Bool -> IO ()
setWantsIncrementalChangeDetails phFetchOptions  value =
  sendMsg phFetchOptions (mkSelector "setWantsIncrementalChangeDetails:") retVoid [argCULong (if value then 1 else 0)]

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @predicate@
predicateSelector :: Selector
predicateSelector = mkSelector "predicate"

-- | @Selector@ for @setPredicate:@
setPredicateSelector :: Selector
setPredicateSelector = mkSelector "setPredicate:"

-- | @Selector@ for @sortDescriptors@
sortDescriptorsSelector :: Selector
sortDescriptorsSelector = mkSelector "sortDescriptors"

-- | @Selector@ for @setSortDescriptors:@
setSortDescriptorsSelector :: Selector
setSortDescriptorsSelector = mkSelector "setSortDescriptors:"

-- | @Selector@ for @includeHiddenAssets@
includeHiddenAssetsSelector :: Selector
includeHiddenAssetsSelector = mkSelector "includeHiddenAssets"

-- | @Selector@ for @setIncludeHiddenAssets:@
setIncludeHiddenAssetsSelector :: Selector
setIncludeHiddenAssetsSelector = mkSelector "setIncludeHiddenAssets:"

-- | @Selector@ for @includeAllBurstAssets@
includeAllBurstAssetsSelector :: Selector
includeAllBurstAssetsSelector = mkSelector "includeAllBurstAssets"

-- | @Selector@ for @setIncludeAllBurstAssets:@
setIncludeAllBurstAssetsSelector :: Selector
setIncludeAllBurstAssetsSelector = mkSelector "setIncludeAllBurstAssets:"

-- | @Selector@ for @includeAssetSourceTypes@
includeAssetSourceTypesSelector :: Selector
includeAssetSourceTypesSelector = mkSelector "includeAssetSourceTypes"

-- | @Selector@ for @setIncludeAssetSourceTypes:@
setIncludeAssetSourceTypesSelector :: Selector
setIncludeAssetSourceTypesSelector = mkSelector "setIncludeAssetSourceTypes:"

-- | @Selector@ for @fetchLimit@
fetchLimitSelector :: Selector
fetchLimitSelector = mkSelector "fetchLimit"

-- | @Selector@ for @setFetchLimit:@
setFetchLimitSelector :: Selector
setFetchLimitSelector = mkSelector "setFetchLimit:"

-- | @Selector@ for @wantsIncrementalChangeDetails@
wantsIncrementalChangeDetailsSelector :: Selector
wantsIncrementalChangeDetailsSelector = mkSelector "wantsIncrementalChangeDetails"

-- | @Selector@ for @setWantsIncrementalChangeDetails:@
setWantsIncrementalChangeDetailsSelector :: Selector
setWantsIncrementalChangeDetailsSelector = mkSelector "setWantsIncrementalChangeDetails:"

