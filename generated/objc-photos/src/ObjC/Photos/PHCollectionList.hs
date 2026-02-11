{-# LANGUAGE PatternSynonyms #-}
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
  , fetchCollectionListsContainingCollection_optionsSelector
  , fetchCollectionListsWithLocalIdentifiers_optionsSelector
  , fetchCollectionListsWithType_subtype_optionsSelector
  , fetchMomentListsWithSubtype_containingMoment_optionsSelector
  , fetchMomentListsWithSubtype_optionsSelector
  , transientCollectionListWithCollections_titleSelector
  , transientCollectionListWithCollectionsFetchResult_titleSelector
  , collectionListTypeSelector
  , collectionListSubtypeSelector
  , startDateSelector
  , endDateSelector
  , localizedLocationNamesSelector

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

-- | @+ fetchCollectionListsContainingCollection:options:@
fetchCollectionListsContainingCollection_options :: (IsPHCollection collection, IsPHFetchOptions options) => collection -> options -> IO (Id PHFetchResult)
fetchCollectionListsContainingCollection_options collection options =
  do
    cls' <- getRequiredClass "PHCollectionList"
    withObjCPtr collection $ \raw_collection ->
      withObjCPtr options $ \raw_options ->
        sendClassMsg cls' (mkSelector "fetchCollectionListsContainingCollection:options:") (retPtr retVoid) [argPtr (castPtr raw_collection :: Ptr ()), argPtr (castPtr raw_options :: Ptr ())] >>= retainedObject . castPtr

-- | @+ fetchCollectionListsWithLocalIdentifiers:options:@
fetchCollectionListsWithLocalIdentifiers_options :: (IsNSArray identifiers, IsPHFetchOptions options) => identifiers -> options -> IO (Id PHFetchResult)
fetchCollectionListsWithLocalIdentifiers_options identifiers options =
  do
    cls' <- getRequiredClass "PHCollectionList"
    withObjCPtr identifiers $ \raw_identifiers ->
      withObjCPtr options $ \raw_options ->
        sendClassMsg cls' (mkSelector "fetchCollectionListsWithLocalIdentifiers:options:") (retPtr retVoid) [argPtr (castPtr raw_identifiers :: Ptr ()), argPtr (castPtr raw_options :: Ptr ())] >>= retainedObject . castPtr

-- | @+ fetchCollectionListsWithType:subtype:options:@
fetchCollectionListsWithType_subtype_options :: IsPHFetchOptions options => PHCollectionListType -> PHCollectionListSubtype -> options -> IO (Id PHFetchResult)
fetchCollectionListsWithType_subtype_options collectionListType subtype options =
  do
    cls' <- getRequiredClass "PHCollectionList"
    withObjCPtr options $ \raw_options ->
      sendClassMsg cls' (mkSelector "fetchCollectionListsWithType:subtype:options:") (retPtr retVoid) [argCLong (coerce collectionListType), argCLong (coerce subtype), argPtr (castPtr raw_options :: Ptr ())] >>= retainedObject . castPtr

-- | @+ fetchMomentListsWithSubtype:containingMoment:options:@
fetchMomentListsWithSubtype_containingMoment_options :: (IsPHAssetCollection moment, IsPHFetchOptions options) => PHCollectionListSubtype -> moment -> options -> IO (Id PHFetchResult)
fetchMomentListsWithSubtype_containingMoment_options momentListSubtype moment options =
  do
    cls' <- getRequiredClass "PHCollectionList"
    withObjCPtr moment $ \raw_moment ->
      withObjCPtr options $ \raw_options ->
        sendClassMsg cls' (mkSelector "fetchMomentListsWithSubtype:containingMoment:options:") (retPtr retVoid) [argCLong (coerce momentListSubtype), argPtr (castPtr raw_moment :: Ptr ()), argPtr (castPtr raw_options :: Ptr ())] >>= retainedObject . castPtr

-- | @+ fetchMomentListsWithSubtype:options:@
fetchMomentListsWithSubtype_options :: IsPHFetchOptions options => PHCollectionListSubtype -> options -> IO (Id PHFetchResult)
fetchMomentListsWithSubtype_options momentListSubtype options =
  do
    cls' <- getRequiredClass "PHCollectionList"
    withObjCPtr options $ \raw_options ->
      sendClassMsg cls' (mkSelector "fetchMomentListsWithSubtype:options:") (retPtr retVoid) [argCLong (coerce momentListSubtype), argPtr (castPtr raw_options :: Ptr ())] >>= retainedObject . castPtr

-- | @+ transientCollectionListWithCollections:title:@
transientCollectionListWithCollections_title :: (IsNSArray collections, IsNSString title) => collections -> title -> IO (Id PHCollectionList)
transientCollectionListWithCollections_title collections title =
  do
    cls' <- getRequiredClass "PHCollectionList"
    withObjCPtr collections $ \raw_collections ->
      withObjCPtr title $ \raw_title ->
        sendClassMsg cls' (mkSelector "transientCollectionListWithCollections:title:") (retPtr retVoid) [argPtr (castPtr raw_collections :: Ptr ()), argPtr (castPtr raw_title :: Ptr ())] >>= retainedObject . castPtr

-- | @+ transientCollectionListWithCollectionsFetchResult:title:@
transientCollectionListWithCollectionsFetchResult_title :: (IsPHFetchResult fetchResult, IsNSString title) => fetchResult -> title -> IO (Id PHCollectionList)
transientCollectionListWithCollectionsFetchResult_title fetchResult title =
  do
    cls' <- getRequiredClass "PHCollectionList"
    withObjCPtr fetchResult $ \raw_fetchResult ->
      withObjCPtr title $ \raw_title ->
        sendClassMsg cls' (mkSelector "transientCollectionListWithCollectionsFetchResult:title:") (retPtr retVoid) [argPtr (castPtr raw_fetchResult :: Ptr ()), argPtr (castPtr raw_title :: Ptr ())] >>= retainedObject . castPtr

-- | @- collectionListType@
collectionListType :: IsPHCollectionList phCollectionList => phCollectionList -> IO PHCollectionListType
collectionListType phCollectionList  =
  fmap (coerce :: CLong -> PHCollectionListType) $ sendMsg phCollectionList (mkSelector "collectionListType") retCLong []

-- | @- collectionListSubtype@
collectionListSubtype :: IsPHCollectionList phCollectionList => phCollectionList -> IO PHCollectionListSubtype
collectionListSubtype phCollectionList  =
  fmap (coerce :: CLong -> PHCollectionListSubtype) $ sendMsg phCollectionList (mkSelector "collectionListSubtype") retCLong []

-- | @- startDate@
startDate :: IsPHCollectionList phCollectionList => phCollectionList -> IO (Id NSDate)
startDate phCollectionList  =
  sendMsg phCollectionList (mkSelector "startDate") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- endDate@
endDate :: IsPHCollectionList phCollectionList => phCollectionList -> IO (Id NSDate)
endDate phCollectionList  =
  sendMsg phCollectionList (mkSelector "endDate") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- localizedLocationNames@
localizedLocationNames :: IsPHCollectionList phCollectionList => phCollectionList -> IO (Id NSArray)
localizedLocationNames phCollectionList  =
  sendMsg phCollectionList (mkSelector "localizedLocationNames") (retPtr retVoid) [] >>= retainedObject . castPtr

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @fetchCollectionListsContainingCollection:options:@
fetchCollectionListsContainingCollection_optionsSelector :: Selector
fetchCollectionListsContainingCollection_optionsSelector = mkSelector "fetchCollectionListsContainingCollection:options:"

-- | @Selector@ for @fetchCollectionListsWithLocalIdentifiers:options:@
fetchCollectionListsWithLocalIdentifiers_optionsSelector :: Selector
fetchCollectionListsWithLocalIdentifiers_optionsSelector = mkSelector "fetchCollectionListsWithLocalIdentifiers:options:"

-- | @Selector@ for @fetchCollectionListsWithType:subtype:options:@
fetchCollectionListsWithType_subtype_optionsSelector :: Selector
fetchCollectionListsWithType_subtype_optionsSelector = mkSelector "fetchCollectionListsWithType:subtype:options:"

-- | @Selector@ for @fetchMomentListsWithSubtype:containingMoment:options:@
fetchMomentListsWithSubtype_containingMoment_optionsSelector :: Selector
fetchMomentListsWithSubtype_containingMoment_optionsSelector = mkSelector "fetchMomentListsWithSubtype:containingMoment:options:"

-- | @Selector@ for @fetchMomentListsWithSubtype:options:@
fetchMomentListsWithSubtype_optionsSelector :: Selector
fetchMomentListsWithSubtype_optionsSelector = mkSelector "fetchMomentListsWithSubtype:options:"

-- | @Selector@ for @transientCollectionListWithCollections:title:@
transientCollectionListWithCollections_titleSelector :: Selector
transientCollectionListWithCollections_titleSelector = mkSelector "transientCollectionListWithCollections:title:"

-- | @Selector@ for @transientCollectionListWithCollectionsFetchResult:title:@
transientCollectionListWithCollectionsFetchResult_titleSelector :: Selector
transientCollectionListWithCollectionsFetchResult_titleSelector = mkSelector "transientCollectionListWithCollectionsFetchResult:title:"

-- | @Selector@ for @collectionListType@
collectionListTypeSelector :: Selector
collectionListTypeSelector = mkSelector "collectionListType"

-- | @Selector@ for @collectionListSubtype@
collectionListSubtypeSelector :: Selector
collectionListSubtypeSelector = mkSelector "collectionListSubtype"

-- | @Selector@ for @startDate@
startDateSelector :: Selector
startDateSelector = mkSelector "startDate"

-- | @Selector@ for @endDate@
endDateSelector :: Selector
endDateSelector = mkSelector "endDate"

-- | @Selector@ for @localizedLocationNames@
localizedLocationNamesSelector :: Selector
localizedLocationNamesSelector = mkSelector "localizedLocationNames"

