{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Generated bindings for @PHAssetCollection@.
module ObjC.Photos.PHAssetCollection
  ( PHAssetCollection
  , IsPHAssetCollection(..)
  , fetchAssetCollectionsWithLocalIdentifiers_options
  , fetchAssetCollectionsWithType_subtype_options
  , fetchAssetCollectionsContainingAsset_withType_options
  , fetchAssetCollectionsWithALAssetGroupURLs_options
  , fetchMomentsInMomentList_options
  , fetchMomentsWithOptions
  , transientAssetCollectionWithAssets_title
  , transientAssetCollectionWithAssetFetchResult_title
  , assetCollectionType
  , assetCollectionSubtype
  , estimatedAssetCount
  , startDate
  , endDate
  , localizedLocationNames
  , assetCollectionSubtypeSelector
  , assetCollectionTypeSelector
  , endDateSelector
  , estimatedAssetCountSelector
  , fetchAssetCollectionsContainingAsset_withType_optionsSelector
  , fetchAssetCollectionsWithALAssetGroupURLs_optionsSelector
  , fetchAssetCollectionsWithLocalIdentifiers_optionsSelector
  , fetchAssetCollectionsWithType_subtype_optionsSelector
  , fetchMomentsInMomentList_optionsSelector
  , fetchMomentsWithOptionsSelector
  , localizedLocationNamesSelector
  , startDateSelector
  , transientAssetCollectionWithAssetFetchResult_titleSelector
  , transientAssetCollectionWithAssets_titleSelector

  -- * Enum types
  , PHAssetCollectionSubtype(PHAssetCollectionSubtype)
  , pattern PHAssetCollectionSubtypeAlbumRegular
  , pattern PHAssetCollectionSubtypeAlbumSyncedEvent
  , pattern PHAssetCollectionSubtypeAlbumSyncedFaces
  , pattern PHAssetCollectionSubtypeAlbumSyncedAlbum
  , pattern PHAssetCollectionSubtypeAlbumImported
  , pattern PHAssetCollectionSubtypeAlbumMyPhotoStream
  , pattern PHAssetCollectionSubtypeAlbumCloudShared
  , pattern PHAssetCollectionSubtypeSmartAlbumGeneric
  , pattern PHAssetCollectionSubtypeSmartAlbumPanoramas
  , pattern PHAssetCollectionSubtypeSmartAlbumVideos
  , pattern PHAssetCollectionSubtypeSmartAlbumFavorites
  , pattern PHAssetCollectionSubtypeSmartAlbumTimelapses
  , pattern PHAssetCollectionSubtypeSmartAlbumAllHidden
  , pattern PHAssetCollectionSubtypeSmartAlbumRecentlyAdded
  , pattern PHAssetCollectionSubtypeSmartAlbumBursts
  , pattern PHAssetCollectionSubtypeSmartAlbumSlomoVideos
  , pattern PHAssetCollectionSubtypeSmartAlbumUserLibrary
  , pattern PHAssetCollectionSubtypeSmartAlbumSelfPortraits
  , pattern PHAssetCollectionSubtypeSmartAlbumScreenshots
  , pattern PHAssetCollectionSubtypeSmartAlbumDepthEffect
  , pattern PHAssetCollectionSubtypeSmartAlbumLivePhotos
  , pattern PHAssetCollectionSubtypeSmartAlbumAnimated
  , pattern PHAssetCollectionSubtypeSmartAlbumLongExposures
  , pattern PHAssetCollectionSubtypeSmartAlbumUnableToUpload
  , pattern PHAssetCollectionSubtypeSmartAlbumRAW
  , pattern PHAssetCollectionSubtypeSmartAlbumCinematic
  , pattern PHAssetCollectionSubtypeSmartAlbumSpatial
  , pattern PHAssetCollectionSubtypeSmartAlbumScreenRecordings
  , pattern PHAssetCollectionSubtypeAny
  , PHAssetCollectionType(PHAssetCollectionType)
  , pattern PHAssetCollectionTypeAlbum
  , pattern PHAssetCollectionTypeSmartAlbum
  , pattern PHAssetCollectionTypeMoment

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

-- | @+ fetchAssetCollectionsWithLocalIdentifiers:options:@
fetchAssetCollectionsWithLocalIdentifiers_options :: (IsNSArray identifiers, IsPHFetchOptions options) => identifiers -> options -> IO (Id PHFetchResult)
fetchAssetCollectionsWithLocalIdentifiers_options identifiers options =
  do
    cls' <- getRequiredClass "PHAssetCollection"
    sendClassMessage cls' fetchAssetCollectionsWithLocalIdentifiers_optionsSelector (toNSArray identifiers) (toPHFetchOptions options)

-- | @+ fetchAssetCollectionsWithType:subtype:options:@
fetchAssetCollectionsWithType_subtype_options :: IsPHFetchOptions options => PHAssetCollectionType -> PHAssetCollectionSubtype -> options -> IO (Id PHFetchResult)
fetchAssetCollectionsWithType_subtype_options type_ subtype options =
  do
    cls' <- getRequiredClass "PHAssetCollection"
    sendClassMessage cls' fetchAssetCollectionsWithType_subtype_optionsSelector type_ subtype (toPHFetchOptions options)

-- | @+ fetchAssetCollectionsContainingAsset:withType:options:@
fetchAssetCollectionsContainingAsset_withType_options :: (IsPHAsset asset, IsPHFetchOptions options) => asset -> PHAssetCollectionType -> options -> IO (Id PHFetchResult)
fetchAssetCollectionsContainingAsset_withType_options asset type_ options =
  do
    cls' <- getRequiredClass "PHAssetCollection"
    sendClassMessage cls' fetchAssetCollectionsContainingAsset_withType_optionsSelector (toPHAsset asset) type_ (toPHFetchOptions options)

-- | @+ fetchAssetCollectionsWithALAssetGroupURLs:options:@
fetchAssetCollectionsWithALAssetGroupURLs_options :: (IsNSArray assetGroupURLs, IsPHFetchOptions options) => assetGroupURLs -> options -> IO (Id PHFetchResult)
fetchAssetCollectionsWithALAssetGroupURLs_options assetGroupURLs options =
  do
    cls' <- getRequiredClass "PHAssetCollection"
    sendClassMessage cls' fetchAssetCollectionsWithALAssetGroupURLs_optionsSelector (toNSArray assetGroupURLs) (toPHFetchOptions options)

-- | @+ fetchMomentsInMomentList:options:@
fetchMomentsInMomentList_options :: (IsPHCollectionList momentList, IsPHFetchOptions options) => momentList -> options -> IO (Id PHFetchResult)
fetchMomentsInMomentList_options momentList options =
  do
    cls' <- getRequiredClass "PHAssetCollection"
    sendClassMessage cls' fetchMomentsInMomentList_optionsSelector (toPHCollectionList momentList) (toPHFetchOptions options)

-- | @+ fetchMomentsWithOptions:@
fetchMomentsWithOptions :: IsPHFetchOptions options => options -> IO (Id PHFetchResult)
fetchMomentsWithOptions options =
  do
    cls' <- getRequiredClass "PHAssetCollection"
    sendClassMessage cls' fetchMomentsWithOptionsSelector (toPHFetchOptions options)

-- | @+ transientAssetCollectionWithAssets:title:@
transientAssetCollectionWithAssets_title :: (IsNSArray assets, IsNSString title) => assets -> title -> IO (Id PHAssetCollection)
transientAssetCollectionWithAssets_title assets title =
  do
    cls' <- getRequiredClass "PHAssetCollection"
    sendClassMessage cls' transientAssetCollectionWithAssets_titleSelector (toNSArray assets) (toNSString title)

-- | @+ transientAssetCollectionWithAssetFetchResult:title:@
transientAssetCollectionWithAssetFetchResult_title :: (IsPHFetchResult fetchResult, IsNSString title) => fetchResult -> title -> IO (Id PHAssetCollection)
transientAssetCollectionWithAssetFetchResult_title fetchResult title =
  do
    cls' <- getRequiredClass "PHAssetCollection"
    sendClassMessage cls' transientAssetCollectionWithAssetFetchResult_titleSelector (toPHFetchResult fetchResult) (toNSString title)

-- | @- assetCollectionType@
assetCollectionType :: IsPHAssetCollection phAssetCollection => phAssetCollection -> IO PHAssetCollectionType
assetCollectionType phAssetCollection =
  sendMessage phAssetCollection assetCollectionTypeSelector

-- | @- assetCollectionSubtype@
assetCollectionSubtype :: IsPHAssetCollection phAssetCollection => phAssetCollection -> IO PHAssetCollectionSubtype
assetCollectionSubtype phAssetCollection =
  sendMessage phAssetCollection assetCollectionSubtypeSelector

-- | @- estimatedAssetCount@
estimatedAssetCount :: IsPHAssetCollection phAssetCollection => phAssetCollection -> IO CULong
estimatedAssetCount phAssetCollection =
  sendMessage phAssetCollection estimatedAssetCountSelector

-- | @- startDate@
startDate :: IsPHAssetCollection phAssetCollection => phAssetCollection -> IO (Id NSDate)
startDate phAssetCollection =
  sendMessage phAssetCollection startDateSelector

-- | @- endDate@
endDate :: IsPHAssetCollection phAssetCollection => phAssetCollection -> IO (Id NSDate)
endDate phAssetCollection =
  sendMessage phAssetCollection endDateSelector

-- | @- localizedLocationNames@
localizedLocationNames :: IsPHAssetCollection phAssetCollection => phAssetCollection -> IO (Id NSArray)
localizedLocationNames phAssetCollection =
  sendMessage phAssetCollection localizedLocationNamesSelector

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @fetchAssetCollectionsWithLocalIdentifiers:options:@
fetchAssetCollectionsWithLocalIdentifiers_optionsSelector :: Selector '[Id NSArray, Id PHFetchOptions] (Id PHFetchResult)
fetchAssetCollectionsWithLocalIdentifiers_optionsSelector = mkSelector "fetchAssetCollectionsWithLocalIdentifiers:options:"

-- | @Selector@ for @fetchAssetCollectionsWithType:subtype:options:@
fetchAssetCollectionsWithType_subtype_optionsSelector :: Selector '[PHAssetCollectionType, PHAssetCollectionSubtype, Id PHFetchOptions] (Id PHFetchResult)
fetchAssetCollectionsWithType_subtype_optionsSelector = mkSelector "fetchAssetCollectionsWithType:subtype:options:"

-- | @Selector@ for @fetchAssetCollectionsContainingAsset:withType:options:@
fetchAssetCollectionsContainingAsset_withType_optionsSelector :: Selector '[Id PHAsset, PHAssetCollectionType, Id PHFetchOptions] (Id PHFetchResult)
fetchAssetCollectionsContainingAsset_withType_optionsSelector = mkSelector "fetchAssetCollectionsContainingAsset:withType:options:"

-- | @Selector@ for @fetchAssetCollectionsWithALAssetGroupURLs:options:@
fetchAssetCollectionsWithALAssetGroupURLs_optionsSelector :: Selector '[Id NSArray, Id PHFetchOptions] (Id PHFetchResult)
fetchAssetCollectionsWithALAssetGroupURLs_optionsSelector = mkSelector "fetchAssetCollectionsWithALAssetGroupURLs:options:"

-- | @Selector@ for @fetchMomentsInMomentList:options:@
fetchMomentsInMomentList_optionsSelector :: Selector '[Id PHCollectionList, Id PHFetchOptions] (Id PHFetchResult)
fetchMomentsInMomentList_optionsSelector = mkSelector "fetchMomentsInMomentList:options:"

-- | @Selector@ for @fetchMomentsWithOptions:@
fetchMomentsWithOptionsSelector :: Selector '[Id PHFetchOptions] (Id PHFetchResult)
fetchMomentsWithOptionsSelector = mkSelector "fetchMomentsWithOptions:"

-- | @Selector@ for @transientAssetCollectionWithAssets:title:@
transientAssetCollectionWithAssets_titleSelector :: Selector '[Id NSArray, Id NSString] (Id PHAssetCollection)
transientAssetCollectionWithAssets_titleSelector = mkSelector "transientAssetCollectionWithAssets:title:"

-- | @Selector@ for @transientAssetCollectionWithAssetFetchResult:title:@
transientAssetCollectionWithAssetFetchResult_titleSelector :: Selector '[Id PHFetchResult, Id NSString] (Id PHAssetCollection)
transientAssetCollectionWithAssetFetchResult_titleSelector = mkSelector "transientAssetCollectionWithAssetFetchResult:title:"

-- | @Selector@ for @assetCollectionType@
assetCollectionTypeSelector :: Selector '[] PHAssetCollectionType
assetCollectionTypeSelector = mkSelector "assetCollectionType"

-- | @Selector@ for @assetCollectionSubtype@
assetCollectionSubtypeSelector :: Selector '[] PHAssetCollectionSubtype
assetCollectionSubtypeSelector = mkSelector "assetCollectionSubtype"

-- | @Selector@ for @estimatedAssetCount@
estimatedAssetCountSelector :: Selector '[] CULong
estimatedAssetCountSelector = mkSelector "estimatedAssetCount"

-- | @Selector@ for @startDate@
startDateSelector :: Selector '[] (Id NSDate)
startDateSelector = mkSelector "startDate"

-- | @Selector@ for @endDate@
endDateSelector :: Selector '[] (Id NSDate)
endDateSelector = mkSelector "endDate"

-- | @Selector@ for @localizedLocationNames@
localizedLocationNamesSelector :: Selector '[] (Id NSArray)
localizedLocationNamesSelector = mkSelector "localizedLocationNames"

