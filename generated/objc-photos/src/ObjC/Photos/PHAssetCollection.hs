{-# LANGUAGE PatternSynonyms #-}
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
  , fetchAssetCollectionsWithLocalIdentifiers_optionsSelector
  , fetchAssetCollectionsWithType_subtype_optionsSelector
  , fetchAssetCollectionsContainingAsset_withType_optionsSelector
  , fetchAssetCollectionsWithALAssetGroupURLs_optionsSelector
  , fetchMomentsInMomentList_optionsSelector
  , fetchMomentsWithOptionsSelector
  , transientAssetCollectionWithAssets_titleSelector
  , transientAssetCollectionWithAssetFetchResult_titleSelector
  , assetCollectionTypeSelector
  , assetCollectionSubtypeSelector
  , estimatedAssetCountSelector
  , startDateSelector
  , endDateSelector
  , localizedLocationNamesSelector

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

-- | @+ fetchAssetCollectionsWithLocalIdentifiers:options:@
fetchAssetCollectionsWithLocalIdentifiers_options :: (IsNSArray identifiers, IsPHFetchOptions options) => identifiers -> options -> IO (Id PHFetchResult)
fetchAssetCollectionsWithLocalIdentifiers_options identifiers options =
  do
    cls' <- getRequiredClass "PHAssetCollection"
    withObjCPtr identifiers $ \raw_identifiers ->
      withObjCPtr options $ \raw_options ->
        sendClassMsg cls' (mkSelector "fetchAssetCollectionsWithLocalIdentifiers:options:") (retPtr retVoid) [argPtr (castPtr raw_identifiers :: Ptr ()), argPtr (castPtr raw_options :: Ptr ())] >>= retainedObject . castPtr

-- | @+ fetchAssetCollectionsWithType:subtype:options:@
fetchAssetCollectionsWithType_subtype_options :: IsPHFetchOptions options => PHAssetCollectionType -> PHAssetCollectionSubtype -> options -> IO (Id PHFetchResult)
fetchAssetCollectionsWithType_subtype_options type_ subtype options =
  do
    cls' <- getRequiredClass "PHAssetCollection"
    withObjCPtr options $ \raw_options ->
      sendClassMsg cls' (mkSelector "fetchAssetCollectionsWithType:subtype:options:") (retPtr retVoid) [argCLong (coerce type_), argCLong (coerce subtype), argPtr (castPtr raw_options :: Ptr ())] >>= retainedObject . castPtr

-- | @+ fetchAssetCollectionsContainingAsset:withType:options:@
fetchAssetCollectionsContainingAsset_withType_options :: (IsPHAsset asset, IsPHFetchOptions options) => asset -> PHAssetCollectionType -> options -> IO (Id PHFetchResult)
fetchAssetCollectionsContainingAsset_withType_options asset type_ options =
  do
    cls' <- getRequiredClass "PHAssetCollection"
    withObjCPtr asset $ \raw_asset ->
      withObjCPtr options $ \raw_options ->
        sendClassMsg cls' (mkSelector "fetchAssetCollectionsContainingAsset:withType:options:") (retPtr retVoid) [argPtr (castPtr raw_asset :: Ptr ()), argCLong (coerce type_), argPtr (castPtr raw_options :: Ptr ())] >>= retainedObject . castPtr

-- | @+ fetchAssetCollectionsWithALAssetGroupURLs:options:@
fetchAssetCollectionsWithALAssetGroupURLs_options :: (IsNSArray assetGroupURLs, IsPHFetchOptions options) => assetGroupURLs -> options -> IO (Id PHFetchResult)
fetchAssetCollectionsWithALAssetGroupURLs_options assetGroupURLs options =
  do
    cls' <- getRequiredClass "PHAssetCollection"
    withObjCPtr assetGroupURLs $ \raw_assetGroupURLs ->
      withObjCPtr options $ \raw_options ->
        sendClassMsg cls' (mkSelector "fetchAssetCollectionsWithALAssetGroupURLs:options:") (retPtr retVoid) [argPtr (castPtr raw_assetGroupURLs :: Ptr ()), argPtr (castPtr raw_options :: Ptr ())] >>= retainedObject . castPtr

-- | @+ fetchMomentsInMomentList:options:@
fetchMomentsInMomentList_options :: (IsPHCollectionList momentList, IsPHFetchOptions options) => momentList -> options -> IO (Id PHFetchResult)
fetchMomentsInMomentList_options momentList options =
  do
    cls' <- getRequiredClass "PHAssetCollection"
    withObjCPtr momentList $ \raw_momentList ->
      withObjCPtr options $ \raw_options ->
        sendClassMsg cls' (mkSelector "fetchMomentsInMomentList:options:") (retPtr retVoid) [argPtr (castPtr raw_momentList :: Ptr ()), argPtr (castPtr raw_options :: Ptr ())] >>= retainedObject . castPtr

-- | @+ fetchMomentsWithOptions:@
fetchMomentsWithOptions :: IsPHFetchOptions options => options -> IO (Id PHFetchResult)
fetchMomentsWithOptions options =
  do
    cls' <- getRequiredClass "PHAssetCollection"
    withObjCPtr options $ \raw_options ->
      sendClassMsg cls' (mkSelector "fetchMomentsWithOptions:") (retPtr retVoid) [argPtr (castPtr raw_options :: Ptr ())] >>= retainedObject . castPtr

-- | @+ transientAssetCollectionWithAssets:title:@
transientAssetCollectionWithAssets_title :: (IsNSArray assets, IsNSString title) => assets -> title -> IO (Id PHAssetCollection)
transientAssetCollectionWithAssets_title assets title =
  do
    cls' <- getRequiredClass "PHAssetCollection"
    withObjCPtr assets $ \raw_assets ->
      withObjCPtr title $ \raw_title ->
        sendClassMsg cls' (mkSelector "transientAssetCollectionWithAssets:title:") (retPtr retVoid) [argPtr (castPtr raw_assets :: Ptr ()), argPtr (castPtr raw_title :: Ptr ())] >>= retainedObject . castPtr

-- | @+ transientAssetCollectionWithAssetFetchResult:title:@
transientAssetCollectionWithAssetFetchResult_title :: (IsPHFetchResult fetchResult, IsNSString title) => fetchResult -> title -> IO (Id PHAssetCollection)
transientAssetCollectionWithAssetFetchResult_title fetchResult title =
  do
    cls' <- getRequiredClass "PHAssetCollection"
    withObjCPtr fetchResult $ \raw_fetchResult ->
      withObjCPtr title $ \raw_title ->
        sendClassMsg cls' (mkSelector "transientAssetCollectionWithAssetFetchResult:title:") (retPtr retVoid) [argPtr (castPtr raw_fetchResult :: Ptr ()), argPtr (castPtr raw_title :: Ptr ())] >>= retainedObject . castPtr

-- | @- assetCollectionType@
assetCollectionType :: IsPHAssetCollection phAssetCollection => phAssetCollection -> IO PHAssetCollectionType
assetCollectionType phAssetCollection  =
  fmap (coerce :: CLong -> PHAssetCollectionType) $ sendMsg phAssetCollection (mkSelector "assetCollectionType") retCLong []

-- | @- assetCollectionSubtype@
assetCollectionSubtype :: IsPHAssetCollection phAssetCollection => phAssetCollection -> IO PHAssetCollectionSubtype
assetCollectionSubtype phAssetCollection  =
  fmap (coerce :: CLong -> PHAssetCollectionSubtype) $ sendMsg phAssetCollection (mkSelector "assetCollectionSubtype") retCLong []

-- | @- estimatedAssetCount@
estimatedAssetCount :: IsPHAssetCollection phAssetCollection => phAssetCollection -> IO CULong
estimatedAssetCount phAssetCollection  =
  sendMsg phAssetCollection (mkSelector "estimatedAssetCount") retCULong []

-- | @- startDate@
startDate :: IsPHAssetCollection phAssetCollection => phAssetCollection -> IO (Id NSDate)
startDate phAssetCollection  =
  sendMsg phAssetCollection (mkSelector "startDate") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- endDate@
endDate :: IsPHAssetCollection phAssetCollection => phAssetCollection -> IO (Id NSDate)
endDate phAssetCollection  =
  sendMsg phAssetCollection (mkSelector "endDate") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- localizedLocationNames@
localizedLocationNames :: IsPHAssetCollection phAssetCollection => phAssetCollection -> IO (Id NSArray)
localizedLocationNames phAssetCollection  =
  sendMsg phAssetCollection (mkSelector "localizedLocationNames") (retPtr retVoid) [] >>= retainedObject . castPtr

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @fetchAssetCollectionsWithLocalIdentifiers:options:@
fetchAssetCollectionsWithLocalIdentifiers_optionsSelector :: Selector
fetchAssetCollectionsWithLocalIdentifiers_optionsSelector = mkSelector "fetchAssetCollectionsWithLocalIdentifiers:options:"

-- | @Selector@ for @fetchAssetCollectionsWithType:subtype:options:@
fetchAssetCollectionsWithType_subtype_optionsSelector :: Selector
fetchAssetCollectionsWithType_subtype_optionsSelector = mkSelector "fetchAssetCollectionsWithType:subtype:options:"

-- | @Selector@ for @fetchAssetCollectionsContainingAsset:withType:options:@
fetchAssetCollectionsContainingAsset_withType_optionsSelector :: Selector
fetchAssetCollectionsContainingAsset_withType_optionsSelector = mkSelector "fetchAssetCollectionsContainingAsset:withType:options:"

-- | @Selector@ for @fetchAssetCollectionsWithALAssetGroupURLs:options:@
fetchAssetCollectionsWithALAssetGroupURLs_optionsSelector :: Selector
fetchAssetCollectionsWithALAssetGroupURLs_optionsSelector = mkSelector "fetchAssetCollectionsWithALAssetGroupURLs:options:"

-- | @Selector@ for @fetchMomentsInMomentList:options:@
fetchMomentsInMomentList_optionsSelector :: Selector
fetchMomentsInMomentList_optionsSelector = mkSelector "fetchMomentsInMomentList:options:"

-- | @Selector@ for @fetchMomentsWithOptions:@
fetchMomentsWithOptionsSelector :: Selector
fetchMomentsWithOptionsSelector = mkSelector "fetchMomentsWithOptions:"

-- | @Selector@ for @transientAssetCollectionWithAssets:title:@
transientAssetCollectionWithAssets_titleSelector :: Selector
transientAssetCollectionWithAssets_titleSelector = mkSelector "transientAssetCollectionWithAssets:title:"

-- | @Selector@ for @transientAssetCollectionWithAssetFetchResult:title:@
transientAssetCollectionWithAssetFetchResult_titleSelector :: Selector
transientAssetCollectionWithAssetFetchResult_titleSelector = mkSelector "transientAssetCollectionWithAssetFetchResult:title:"

-- | @Selector@ for @assetCollectionType@
assetCollectionTypeSelector :: Selector
assetCollectionTypeSelector = mkSelector "assetCollectionType"

-- | @Selector@ for @assetCollectionSubtype@
assetCollectionSubtypeSelector :: Selector
assetCollectionSubtypeSelector = mkSelector "assetCollectionSubtype"

-- | @Selector@ for @estimatedAssetCount@
estimatedAssetCountSelector :: Selector
estimatedAssetCountSelector = mkSelector "estimatedAssetCount"

-- | @Selector@ for @startDate@
startDateSelector :: Selector
startDateSelector = mkSelector "startDate"

-- | @Selector@ for @endDate@
endDateSelector :: Selector
endDateSelector = mkSelector "endDate"

-- | @Selector@ for @localizedLocationNames@
localizedLocationNamesSelector :: Selector
localizedLocationNamesSelector = mkSelector "localizedLocationNames"

