{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Generated bindings for @PHAsset@.
module ObjC.Photos.PHAsset
  ( PHAsset
  , IsPHAsset(..)
  , canPerformEditOperation
  , fetchAssetsInAssetCollection_options
  , fetchAssetsWithLocalIdentifiers_options
  , fetchKeyAssetsInAssetCollection_options
  , fetchAssetsWithBurstIdentifier_options
  , fetchAssetsWithOptions
  , fetchAssetsWithMediaType_options
  , fetchAssetsWithALAssetURLs_options
  , requestContentEditingInputWithOptions_completionHandler
  , cancelContentEditingInputRequest
  , playbackStyle
  , mediaType
  , mediaSubtypes
  , contentType
  , pixelWidth
  , pixelHeight
  , creationDate
  , modificationDate
  , addedDate
  , duration
  , hidden
  , favorite
  , syncFailureHidden
  , burstIdentifier
  , burstSelectionTypes
  , representsBurst
  , sourceType
  , hasAdjustments
  , adjustmentFormatIdentifier
  , addedDateSelector
  , adjustmentFormatIdentifierSelector
  , burstIdentifierSelector
  , burstSelectionTypesSelector
  , canPerformEditOperationSelector
  , cancelContentEditingInputRequestSelector
  , contentTypeSelector
  , creationDateSelector
  , durationSelector
  , favoriteSelector
  , fetchAssetsInAssetCollection_optionsSelector
  , fetchAssetsWithALAssetURLs_optionsSelector
  , fetchAssetsWithBurstIdentifier_optionsSelector
  , fetchAssetsWithLocalIdentifiers_optionsSelector
  , fetchAssetsWithMediaType_optionsSelector
  , fetchAssetsWithOptionsSelector
  , fetchKeyAssetsInAssetCollection_optionsSelector
  , hasAdjustmentsSelector
  , hiddenSelector
  , mediaSubtypesSelector
  , mediaTypeSelector
  , modificationDateSelector
  , pixelHeightSelector
  , pixelWidthSelector
  , playbackStyleSelector
  , representsBurstSelector
  , requestContentEditingInputWithOptions_completionHandlerSelector
  , sourceTypeSelector
  , syncFailureHiddenSelector

  -- * Enum types
  , PHAssetBurstSelectionType(PHAssetBurstSelectionType)
  , pattern PHAssetBurstSelectionTypeNone
  , pattern PHAssetBurstSelectionTypeAutoPick
  , pattern PHAssetBurstSelectionTypeUserPick
  , PHAssetEditOperation(PHAssetEditOperation)
  , pattern PHAssetEditOperationDelete
  , pattern PHAssetEditOperationContent
  , pattern PHAssetEditOperationProperties
  , PHAssetMediaSubtype(PHAssetMediaSubtype)
  , pattern PHAssetMediaSubtypeNone
  , pattern PHAssetMediaSubtypePhotoPanorama
  , pattern PHAssetMediaSubtypePhotoHDR
  , pattern PHAssetMediaSubtypePhotoScreenshot
  , pattern PHAssetMediaSubtypePhotoLive
  , pattern PHAssetMediaSubtypePhotoDepthEffect
  , pattern PHAssetMediaSubtypeSpatialMedia
  , pattern PHAssetMediaSubtypeVideoStreamed
  , pattern PHAssetMediaSubtypeVideoHighFrameRate
  , pattern PHAssetMediaSubtypeVideoTimelapse
  , pattern PHAssetMediaSubtypeVideoScreenRecording
  , pattern PHAssetMediaSubtypeVideoCinematic
  , PHAssetMediaType(PHAssetMediaType)
  , pattern PHAssetMediaTypeUnknown
  , pattern PHAssetMediaTypeImage
  , pattern PHAssetMediaTypeVideo
  , pattern PHAssetMediaTypeAudio
  , PHAssetPlaybackStyle(PHAssetPlaybackStyle)
  , pattern PHAssetPlaybackStyleUnsupported
  , pattern PHAssetPlaybackStyleImage
  , pattern PHAssetPlaybackStyleImageAnimated
  , pattern PHAssetPlaybackStyleLivePhoto
  , pattern PHAssetPlaybackStyleVideo
  , pattern PHAssetPlaybackStyleVideoLooping
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
import ObjC.UniformTypeIdentifiers.Internal.Classes

-- | @- canPerformEditOperation:@
canPerformEditOperation :: IsPHAsset phAsset => phAsset -> PHAssetEditOperation -> IO Bool
canPerformEditOperation phAsset editOperation =
  sendMessage phAsset canPerformEditOperationSelector editOperation

-- | @+ fetchAssetsInAssetCollection:options:@
fetchAssetsInAssetCollection_options :: (IsPHAssetCollection assetCollection, IsPHFetchOptions options) => assetCollection -> options -> IO (Id PHFetchResult)
fetchAssetsInAssetCollection_options assetCollection options =
  do
    cls' <- getRequiredClass "PHAsset"
    sendClassMessage cls' fetchAssetsInAssetCollection_optionsSelector (toPHAssetCollection assetCollection) (toPHFetchOptions options)

-- | @+ fetchAssetsWithLocalIdentifiers:options:@
fetchAssetsWithLocalIdentifiers_options :: (IsNSArray identifiers, IsPHFetchOptions options) => identifiers -> options -> IO (Id PHFetchResult)
fetchAssetsWithLocalIdentifiers_options identifiers options =
  do
    cls' <- getRequiredClass "PHAsset"
    sendClassMessage cls' fetchAssetsWithLocalIdentifiers_optionsSelector (toNSArray identifiers) (toPHFetchOptions options)

-- | @+ fetchKeyAssetsInAssetCollection:options:@
fetchKeyAssetsInAssetCollection_options :: (IsPHAssetCollection assetCollection, IsPHFetchOptions options) => assetCollection -> options -> IO (Id PHFetchResult)
fetchKeyAssetsInAssetCollection_options assetCollection options =
  do
    cls' <- getRequiredClass "PHAsset"
    sendClassMessage cls' fetchKeyAssetsInAssetCollection_optionsSelector (toPHAssetCollection assetCollection) (toPHFetchOptions options)

-- | @+ fetchAssetsWithBurstIdentifier:options:@
fetchAssetsWithBurstIdentifier_options :: (IsNSString burstIdentifier, IsPHFetchOptions options) => burstIdentifier -> options -> IO (Id PHFetchResult)
fetchAssetsWithBurstIdentifier_options burstIdentifier options =
  do
    cls' <- getRequiredClass "PHAsset"
    sendClassMessage cls' fetchAssetsWithBurstIdentifier_optionsSelector (toNSString burstIdentifier) (toPHFetchOptions options)

-- | @+ fetchAssetsWithOptions:@
fetchAssetsWithOptions :: IsPHFetchOptions options => options -> IO (Id PHFetchResult)
fetchAssetsWithOptions options =
  do
    cls' <- getRequiredClass "PHAsset"
    sendClassMessage cls' fetchAssetsWithOptionsSelector (toPHFetchOptions options)

-- | @+ fetchAssetsWithMediaType:options:@
fetchAssetsWithMediaType_options :: IsPHFetchOptions options => PHAssetMediaType -> options -> IO (Id PHFetchResult)
fetchAssetsWithMediaType_options mediaType options =
  do
    cls' <- getRequiredClass "PHAsset"
    sendClassMessage cls' fetchAssetsWithMediaType_optionsSelector mediaType (toPHFetchOptions options)

-- | @+ fetchAssetsWithALAssetURLs:options:@
fetchAssetsWithALAssetURLs_options :: (IsNSArray assetURLs, IsPHFetchOptions options) => assetURLs -> options -> IO (Id PHFetchResult)
fetchAssetsWithALAssetURLs_options assetURLs options =
  do
    cls' <- getRequiredClass "PHAsset"
    sendClassMessage cls' fetchAssetsWithALAssetURLs_optionsSelector (toNSArray assetURLs) (toPHFetchOptions options)

-- | @- requestContentEditingInputWithOptions:completionHandler:@
requestContentEditingInputWithOptions_completionHandler :: (IsPHAsset phAsset, IsPHContentEditingInputRequestOptions options) => phAsset -> options -> Ptr () -> IO CULong
requestContentEditingInputWithOptions_completionHandler phAsset options completionHandler =
  sendMessage phAsset requestContentEditingInputWithOptions_completionHandlerSelector (toPHContentEditingInputRequestOptions options) completionHandler

-- | @- cancelContentEditingInputRequest:@
cancelContentEditingInputRequest :: IsPHAsset phAsset => phAsset -> CULong -> IO ()
cancelContentEditingInputRequest phAsset requestID =
  sendMessage phAsset cancelContentEditingInputRequestSelector requestID

-- | @- playbackStyle@
playbackStyle :: IsPHAsset phAsset => phAsset -> IO PHAssetPlaybackStyle
playbackStyle phAsset =
  sendMessage phAsset playbackStyleSelector

-- | @- mediaType@
mediaType :: IsPHAsset phAsset => phAsset -> IO PHAssetMediaType
mediaType phAsset =
  sendMessage phAsset mediaTypeSelector

-- | @- mediaSubtypes@
mediaSubtypes :: IsPHAsset phAsset => phAsset -> IO PHAssetMediaSubtype
mediaSubtypes phAsset =
  sendMessage phAsset mediaSubtypesSelector

-- | The type of image or video data that is presented for the asset
--
-- ObjC selector: @- contentType@
contentType :: IsPHAsset phAsset => phAsset -> IO (Id UTType)
contentType phAsset =
  sendMessage phAsset contentTypeSelector

-- | @- pixelWidth@
pixelWidth :: IsPHAsset phAsset => phAsset -> IO CULong
pixelWidth phAsset =
  sendMessage phAsset pixelWidthSelector

-- | @- pixelHeight@
pixelHeight :: IsPHAsset phAsset => phAsset -> IO CULong
pixelHeight phAsset =
  sendMessage phAsset pixelHeightSelector

-- | The date and time of this asset's creation (can be updated by the user)
--
-- ObjC selector: @- creationDate@
creationDate :: IsPHAsset phAsset => phAsset -> IO (Id NSDate)
creationDate phAsset =
  sendMessage phAsset creationDateSelector

-- | The date and time of the last modification to this asset or one of its properties
--
-- ObjC selector: @- modificationDate@
modificationDate :: IsPHAsset phAsset => phAsset -> IO (Id NSDate)
modificationDate phAsset =
  sendMessage phAsset modificationDateSelector

-- | The date and time this asset was added to the photo library (from the device that was used to add this asset)
--
-- ObjC selector: @- addedDate@
addedDate :: IsPHAsset phAsset => phAsset -> IO (Id NSDate)
addedDate phAsset =
  sendMessage phAsset addedDateSelector

-- | @- duration@
duration :: IsPHAsset phAsset => phAsset -> IO CDouble
duration phAsset =
  sendMessage phAsset durationSelector

-- | @- hidden@
hidden :: IsPHAsset phAsset => phAsset -> IO Bool
hidden phAsset =
  sendMessage phAsset hiddenSelector

-- | @- favorite@
favorite :: IsPHAsset phAsset => phAsset -> IO Bool
favorite phAsset =
  sendMessage phAsset favoriteSelector

-- | @- syncFailureHidden@
syncFailureHidden :: IsPHAsset phAsset => phAsset -> IO Bool
syncFailureHidden phAsset =
  sendMessage phAsset syncFailureHiddenSelector

-- | @- burstIdentifier@
burstIdentifier :: IsPHAsset phAsset => phAsset -> IO (Id NSString)
burstIdentifier phAsset =
  sendMessage phAsset burstIdentifierSelector

-- | @- burstSelectionTypes@
burstSelectionTypes :: IsPHAsset phAsset => phAsset -> IO PHAssetBurstSelectionType
burstSelectionTypes phAsset =
  sendMessage phAsset burstSelectionTypesSelector

-- | @- representsBurst@
representsBurst :: IsPHAsset phAsset => phAsset -> IO Bool
representsBurst phAsset =
  sendMessage phAsset representsBurstSelector

-- | @- sourceType@
sourceType :: IsPHAsset phAsset => phAsset -> IO PHAssetSourceType
sourceType phAsset =
  sendMessage phAsset sourceTypeSelector

-- | @- hasAdjustments@
hasAdjustments :: IsPHAsset phAsset => phAsset -> IO Bool
hasAdjustments phAsset =
  sendMessage phAsset hasAdjustmentsSelector

-- | @- adjustmentFormatIdentifier@
adjustmentFormatIdentifier :: IsPHAsset phAsset => phAsset -> IO (Id NSString)
adjustmentFormatIdentifier phAsset =
  sendMessage phAsset adjustmentFormatIdentifierSelector

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @canPerformEditOperation:@
canPerformEditOperationSelector :: Selector '[PHAssetEditOperation] Bool
canPerformEditOperationSelector = mkSelector "canPerformEditOperation:"

-- | @Selector@ for @fetchAssetsInAssetCollection:options:@
fetchAssetsInAssetCollection_optionsSelector :: Selector '[Id PHAssetCollection, Id PHFetchOptions] (Id PHFetchResult)
fetchAssetsInAssetCollection_optionsSelector = mkSelector "fetchAssetsInAssetCollection:options:"

-- | @Selector@ for @fetchAssetsWithLocalIdentifiers:options:@
fetchAssetsWithLocalIdentifiers_optionsSelector :: Selector '[Id NSArray, Id PHFetchOptions] (Id PHFetchResult)
fetchAssetsWithLocalIdentifiers_optionsSelector = mkSelector "fetchAssetsWithLocalIdentifiers:options:"

-- | @Selector@ for @fetchKeyAssetsInAssetCollection:options:@
fetchKeyAssetsInAssetCollection_optionsSelector :: Selector '[Id PHAssetCollection, Id PHFetchOptions] (Id PHFetchResult)
fetchKeyAssetsInAssetCollection_optionsSelector = mkSelector "fetchKeyAssetsInAssetCollection:options:"

-- | @Selector@ for @fetchAssetsWithBurstIdentifier:options:@
fetchAssetsWithBurstIdentifier_optionsSelector :: Selector '[Id NSString, Id PHFetchOptions] (Id PHFetchResult)
fetchAssetsWithBurstIdentifier_optionsSelector = mkSelector "fetchAssetsWithBurstIdentifier:options:"

-- | @Selector@ for @fetchAssetsWithOptions:@
fetchAssetsWithOptionsSelector :: Selector '[Id PHFetchOptions] (Id PHFetchResult)
fetchAssetsWithOptionsSelector = mkSelector "fetchAssetsWithOptions:"

-- | @Selector@ for @fetchAssetsWithMediaType:options:@
fetchAssetsWithMediaType_optionsSelector :: Selector '[PHAssetMediaType, Id PHFetchOptions] (Id PHFetchResult)
fetchAssetsWithMediaType_optionsSelector = mkSelector "fetchAssetsWithMediaType:options:"

-- | @Selector@ for @fetchAssetsWithALAssetURLs:options:@
fetchAssetsWithALAssetURLs_optionsSelector :: Selector '[Id NSArray, Id PHFetchOptions] (Id PHFetchResult)
fetchAssetsWithALAssetURLs_optionsSelector = mkSelector "fetchAssetsWithALAssetURLs:options:"

-- | @Selector@ for @requestContentEditingInputWithOptions:completionHandler:@
requestContentEditingInputWithOptions_completionHandlerSelector :: Selector '[Id PHContentEditingInputRequestOptions, Ptr ()] CULong
requestContentEditingInputWithOptions_completionHandlerSelector = mkSelector "requestContentEditingInputWithOptions:completionHandler:"

-- | @Selector@ for @cancelContentEditingInputRequest:@
cancelContentEditingInputRequestSelector :: Selector '[CULong] ()
cancelContentEditingInputRequestSelector = mkSelector "cancelContentEditingInputRequest:"

-- | @Selector@ for @playbackStyle@
playbackStyleSelector :: Selector '[] PHAssetPlaybackStyle
playbackStyleSelector = mkSelector "playbackStyle"

-- | @Selector@ for @mediaType@
mediaTypeSelector :: Selector '[] PHAssetMediaType
mediaTypeSelector = mkSelector "mediaType"

-- | @Selector@ for @mediaSubtypes@
mediaSubtypesSelector :: Selector '[] PHAssetMediaSubtype
mediaSubtypesSelector = mkSelector "mediaSubtypes"

-- | @Selector@ for @contentType@
contentTypeSelector :: Selector '[] (Id UTType)
contentTypeSelector = mkSelector "contentType"

-- | @Selector@ for @pixelWidth@
pixelWidthSelector :: Selector '[] CULong
pixelWidthSelector = mkSelector "pixelWidth"

-- | @Selector@ for @pixelHeight@
pixelHeightSelector :: Selector '[] CULong
pixelHeightSelector = mkSelector "pixelHeight"

-- | @Selector@ for @creationDate@
creationDateSelector :: Selector '[] (Id NSDate)
creationDateSelector = mkSelector "creationDate"

-- | @Selector@ for @modificationDate@
modificationDateSelector :: Selector '[] (Id NSDate)
modificationDateSelector = mkSelector "modificationDate"

-- | @Selector@ for @addedDate@
addedDateSelector :: Selector '[] (Id NSDate)
addedDateSelector = mkSelector "addedDate"

-- | @Selector@ for @duration@
durationSelector :: Selector '[] CDouble
durationSelector = mkSelector "duration"

-- | @Selector@ for @hidden@
hiddenSelector :: Selector '[] Bool
hiddenSelector = mkSelector "hidden"

-- | @Selector@ for @favorite@
favoriteSelector :: Selector '[] Bool
favoriteSelector = mkSelector "favorite"

-- | @Selector@ for @syncFailureHidden@
syncFailureHiddenSelector :: Selector '[] Bool
syncFailureHiddenSelector = mkSelector "syncFailureHidden"

-- | @Selector@ for @burstIdentifier@
burstIdentifierSelector :: Selector '[] (Id NSString)
burstIdentifierSelector = mkSelector "burstIdentifier"

-- | @Selector@ for @burstSelectionTypes@
burstSelectionTypesSelector :: Selector '[] PHAssetBurstSelectionType
burstSelectionTypesSelector = mkSelector "burstSelectionTypes"

-- | @Selector@ for @representsBurst@
representsBurstSelector :: Selector '[] Bool
representsBurstSelector = mkSelector "representsBurst"

-- | @Selector@ for @sourceType@
sourceTypeSelector :: Selector '[] PHAssetSourceType
sourceTypeSelector = mkSelector "sourceType"

-- | @Selector@ for @hasAdjustments@
hasAdjustmentsSelector :: Selector '[] Bool
hasAdjustmentsSelector = mkSelector "hasAdjustments"

-- | @Selector@ for @adjustmentFormatIdentifier@
adjustmentFormatIdentifierSelector :: Selector '[] (Id NSString)
adjustmentFormatIdentifierSelector = mkSelector "adjustmentFormatIdentifier"

