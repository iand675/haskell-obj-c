{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Generated bindings for @PHCollectionList@.
module ObjC.Photos.PHCollectionList
  ( PHCollectionList
  , IsPHCollectionList(..)
  , fetchCollectionListsContainingCollection_options
  , fetchCollectionListsWithLocalIdentifiers_options
  , fetchCollectionListsWithType_subtype_options
  , fetchMomentListsWithSubtype_containingMoment_options
  , fetchMomentListsWithSubtype_options
  , transientCollectionListWithCollections_title
  , transientCollectionListWithCollectionsFetchResult_title
  , collectionListType
  , collectionListSubtype
  , startDate
  , endDate
  , localizedLocationNames
  , collectionListSubtypeSelector
  , collectionListTypeSelector
  , endDateSelector
  , fetchCollectionListsContainingCollection_optionsSelector
  , fetchCollectionListsWithLocalIdentifiers_optionsSelector
  , fetchCollectionListsWithType_subtype_optionsSelector
  , fetchMomentListsWithSubtype_containingMoment_optionsSelector
  , fetchMomentListsWithSubtype_optionsSelector
  , localizedLocationNamesSelector
  , startDateSelector
  , transientCollectionListWithCollectionsFetchResult_titleSelector
  , transientCollectionListWithCollections_titleSelector

  -- * Enum types
  , PHCollectionListSubtype(PHCollectionListSubtype)
  , pattern PHCollectionListSubtypeMomentListCluster
  , pattern PHCollectionListSubtypeMomentListYear
  , pattern PHCollectionListSubtypeRegularFolder
  , pattern PHCollectionListSubtypeSmartFolderEvents
  , pattern PHCollectionListSubtypeSmartFolderFaces
  , pattern PHCollectionListSubtypeAny
  , PHCollectionListType(PHCollectionListType)
  , pattern PHCollectionListTypeMomentList
  , pattern PHCollectionListTypeFolder
  , pattern PHCollectionListTypeSmartFolder

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

-- | @+ fetchCollectionListsContainingCollection:options:@
fetchCollectionListsContainingCollection_options :: (IsPHCollection collection, IsPHFetchOptions options) => collection -> options -> IO (Id PHFetchResult)
fetchCollectionListsContainingCollection_options collection options =
  do
    cls' <- getRequiredClass "PHCollectionList"
    sendClassMessage cls' fetchCollectionListsContainingCollection_optionsSelector (toPHCollection collection) (toPHFetchOptions options)

-- | @+ fetchCollectionListsWithLocalIdentifiers:options:@
fetchCollectionListsWithLocalIdentifiers_options :: (IsNSArray identifiers, IsPHFetchOptions options) => identifiers -> options -> IO (Id PHFetchResult)
fetchCollectionListsWithLocalIdentifiers_options identifiers options =
  do
    cls' <- getRequiredClass "PHCollectionList"
    sendClassMessage cls' fetchCollectionListsWithLocalIdentifiers_optionsSelector (toNSArray identifiers) (toPHFetchOptions options)

-- | @+ fetchCollectionListsWithType:subtype:options:@
fetchCollectionListsWithType_subtype_options :: IsPHFetchOptions options => PHCollectionListType -> PHCollectionListSubtype -> options -> IO (Id PHFetchResult)
fetchCollectionListsWithType_subtype_options collectionListType subtype options =
  do
    cls' <- getRequiredClass "PHCollectionList"
    sendClassMessage cls' fetchCollectionListsWithType_subtype_optionsSelector collectionListType subtype (toPHFetchOptions options)

-- | @+ fetchMomentListsWithSubtype:containingMoment:options:@
fetchMomentListsWithSubtype_containingMoment_options :: (IsPHAssetCollection moment, IsPHFetchOptions options) => PHCollectionListSubtype -> moment -> options -> IO (Id PHFetchResult)
fetchMomentListsWithSubtype_containingMoment_options momentListSubtype moment options =
  do
    cls' <- getRequiredClass "PHCollectionList"
    sendClassMessage cls' fetchMomentListsWithSubtype_containingMoment_optionsSelector momentListSubtype (toPHAssetCollection moment) (toPHFetchOptions options)

-- | @+ fetchMomentListsWithSubtype:options:@
fetchMomentListsWithSubtype_options :: IsPHFetchOptions options => PHCollectionListSubtype -> options -> IO (Id PHFetchResult)
fetchMomentListsWithSubtype_options momentListSubtype options =
  do
    cls' <- getRequiredClass "PHCollectionList"
    sendClassMessage cls' fetchMomentListsWithSubtype_optionsSelector momentListSubtype (toPHFetchOptions options)

-- | @+ transientCollectionListWithCollections:title:@
transientCollectionListWithCollections_title :: (IsNSArray collections, IsNSString title) => collections -> title -> IO (Id PHCollectionList)
transientCollectionListWithCollections_title collections title =
  do
    cls' <- getRequiredClass "PHCollectionList"
    sendClassMessage cls' transientCollectionListWithCollections_titleSelector (toNSArray collections) (toNSString title)

-- | @+ transientCollectionListWithCollectionsFetchResult:title:@
transientCollectionListWithCollectionsFetchResult_title :: (IsPHFetchResult fetchResult, IsNSString title) => fetchResult -> title -> IO (Id PHCollectionList)
transientCollectionListWithCollectionsFetchResult_title fetchResult title =
  do
    cls' <- getRequiredClass "PHCollectionList"
    sendClassMessage cls' transientCollectionListWithCollectionsFetchResult_titleSelector (toPHFetchResult fetchResult) (toNSString title)

-- | @- collectionListType@
collectionListType :: IsPHCollectionList phCollectionList => phCollectionList -> IO PHCollectionListType
collectionListType phCollectionList =
  sendMessage phCollectionList collectionListTypeSelector

-- | @- collectionListSubtype@
collectionListSubtype :: IsPHCollectionList phCollectionList => phCollectionList -> IO PHCollectionListSubtype
collectionListSubtype phCollectionList =
  sendMessage phCollectionList collectionListSubtypeSelector

-- | @- startDate@
startDate :: IsPHCollectionList phCollectionList => phCollectionList -> IO (Id NSDate)
startDate phCollectionList =
  sendMessage phCollectionList startDateSelector

-- | @- endDate@
endDate :: IsPHCollectionList phCollectionList => phCollectionList -> IO (Id NSDate)
endDate phCollectionList =
  sendMessage phCollectionList endDateSelector

-- | @- localizedLocationNames@
localizedLocationNames :: IsPHCollectionList phCollectionList => phCollectionList -> IO (Id NSArray)
localizedLocationNames phCollectionList =
  sendMessage phCollectionList localizedLocationNamesSelector

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @fetchCollectionListsContainingCollection:options:@
fetchCollectionListsContainingCollection_optionsSelector :: Selector '[Id PHCollection, Id PHFetchOptions] (Id PHFetchResult)
fetchCollectionListsContainingCollection_optionsSelector = mkSelector "fetchCollectionListsContainingCollection:options:"

-- | @Selector@ for @fetchCollectionListsWithLocalIdentifiers:options:@
fetchCollectionListsWithLocalIdentifiers_optionsSelector :: Selector '[Id NSArray, Id PHFetchOptions] (Id PHFetchResult)
fetchCollectionListsWithLocalIdentifiers_optionsSelector = mkSelector "fetchCollectionListsWithLocalIdentifiers:options:"

-- | @Selector@ for @fetchCollectionListsWithType:subtype:options:@
fetchCollectionListsWithType_subtype_optionsSelector :: Selector '[PHCollectionListType, PHCollectionListSubtype, Id PHFetchOptions] (Id PHFetchResult)
fetchCollectionListsWithType_subtype_optionsSelector = mkSelector "fetchCollectionListsWithType:subtype:options:"

-- | @Selector@ for @fetchMomentListsWithSubtype:containingMoment:options:@
fetchMomentListsWithSubtype_containingMoment_optionsSelector :: Selector '[PHCollectionListSubtype, Id PHAssetCollection, Id PHFetchOptions] (Id PHFetchResult)
fetchMomentListsWithSubtype_containingMoment_optionsSelector = mkSelector "fetchMomentListsWithSubtype:containingMoment:options:"

-- | @Selector@ for @fetchMomentListsWithSubtype:options:@
fetchMomentListsWithSubtype_optionsSelector :: Selector '[PHCollectionListSubtype, Id PHFetchOptions] (Id PHFetchResult)
fetchMomentListsWithSubtype_optionsSelector = mkSelector "fetchMomentListsWithSubtype:options:"

-- | @Selector@ for @transientCollectionListWithCollections:title:@
transientCollectionListWithCollections_titleSelector :: Selector '[Id NSArray, Id NSString] (Id PHCollectionList)
transientCollectionListWithCollections_titleSelector = mkSelector "transientCollectionListWithCollections:title:"

-- | @Selector@ for @transientCollectionListWithCollectionsFetchResult:title:@
transientCollectionListWithCollectionsFetchResult_titleSelector :: Selector '[Id PHFetchResult, Id NSString] (Id PHCollectionList)
transientCollectionListWithCollectionsFetchResult_titleSelector = mkSelector "transientCollectionListWithCollectionsFetchResult:title:"

-- | @Selector@ for @collectionListType@
collectionListTypeSelector :: Selector '[] PHCollectionListType
collectionListTypeSelector = mkSelector "collectionListType"

-- | @Selector@ for @collectionListSubtype@
collectionListSubtypeSelector :: Selector '[] PHCollectionListSubtype
collectionListSubtypeSelector = mkSelector "collectionListSubtype"

-- | @Selector@ for @startDate@
startDateSelector :: Selector '[] (Id NSDate)
startDateSelector = mkSelector "startDate"

-- | @Selector@ for @endDate@
endDateSelector :: Selector '[] (Id NSDate)
endDateSelector = mkSelector "endDate"

-- | @Selector@ for @localizedLocationNames@
localizedLocationNamesSelector :: Selector '[] (Id NSArray)
localizedLocationNamesSelector = mkSelector "localizedLocationNames"

