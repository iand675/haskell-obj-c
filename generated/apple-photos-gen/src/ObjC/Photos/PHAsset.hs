{-# LANGUAGE PatternSynonyms #-}
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
  , canPerformEditOperationSelector
  , fetchAssetsInAssetCollection_optionsSelector
  , fetchAssetsWithLocalIdentifiers_optionsSelector
  , fetchKeyAssetsInAssetCollection_optionsSelector
  , fetchAssetsWithBurstIdentifier_optionsSelector
  , fetchAssetsWithOptionsSelector
  , fetchAssetsWithMediaType_optionsSelector
  , fetchAssetsWithALAssetURLs_optionsSelector
  , requestContentEditingInputWithOptions_completionHandlerSelector
  , cancelContentEditingInputRequestSelector
  , playbackStyleSelector
  , mediaTypeSelector
  , mediaSubtypesSelector
  , contentTypeSelector
  , pixelWidthSelector
  , pixelHeightSelector
  , creationDateSelector
  , modificationDateSelector
  , addedDateSelector
  , durationSelector
  , hiddenSelector
  , favoriteSelector
  , syncFailureHiddenSelector
  , burstIdentifierSelector
  , burstSelectionTypesSelector
  , representsBurstSelector
  , sourceTypeSelector
  , hasAdjustmentsSelector
  , adjustmentFormatIdentifierSelector

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
import ObjC.UniformTypeIdentifiers.Internal.Classes

-- | @- canPerformEditOperation:@
canPerformEditOperation :: IsPHAsset phAsset => phAsset -> PHAssetEditOperation -> IO Bool
canPerformEditOperation phAsset  editOperation =
    fmap ((/= 0) :: CULong -> Bool) $ sendMsg phAsset (mkSelector "canPerformEditOperation:") retCULong [argCLong (coerce editOperation)]

-- | @+ fetchAssetsInAssetCollection:options:@
fetchAssetsInAssetCollection_options :: (IsPHAssetCollection assetCollection, IsPHFetchOptions options) => assetCollection -> options -> IO (Id PHFetchResult)
fetchAssetsInAssetCollection_options assetCollection options =
  do
    cls' <- getRequiredClass "PHAsset"
    withObjCPtr assetCollection $ \raw_assetCollection ->
      withObjCPtr options $ \raw_options ->
        sendClassMsg cls' (mkSelector "fetchAssetsInAssetCollection:options:") (retPtr retVoid) [argPtr (castPtr raw_assetCollection :: Ptr ()), argPtr (castPtr raw_options :: Ptr ())] >>= retainedObject . castPtr

-- | @+ fetchAssetsWithLocalIdentifiers:options:@
fetchAssetsWithLocalIdentifiers_options :: (IsNSArray identifiers, IsPHFetchOptions options) => identifiers -> options -> IO (Id PHFetchResult)
fetchAssetsWithLocalIdentifiers_options identifiers options =
  do
    cls' <- getRequiredClass "PHAsset"
    withObjCPtr identifiers $ \raw_identifiers ->
      withObjCPtr options $ \raw_options ->
        sendClassMsg cls' (mkSelector "fetchAssetsWithLocalIdentifiers:options:") (retPtr retVoid) [argPtr (castPtr raw_identifiers :: Ptr ()), argPtr (castPtr raw_options :: Ptr ())] >>= retainedObject . castPtr

-- | @+ fetchKeyAssetsInAssetCollection:options:@
fetchKeyAssetsInAssetCollection_options :: (IsPHAssetCollection assetCollection, IsPHFetchOptions options) => assetCollection -> options -> IO (Id PHFetchResult)
fetchKeyAssetsInAssetCollection_options assetCollection options =
  do
    cls' <- getRequiredClass "PHAsset"
    withObjCPtr assetCollection $ \raw_assetCollection ->
      withObjCPtr options $ \raw_options ->
        sendClassMsg cls' (mkSelector "fetchKeyAssetsInAssetCollection:options:") (retPtr retVoid) [argPtr (castPtr raw_assetCollection :: Ptr ()), argPtr (castPtr raw_options :: Ptr ())] >>= retainedObject . castPtr

-- | @+ fetchAssetsWithBurstIdentifier:options:@
fetchAssetsWithBurstIdentifier_options :: (IsNSString burstIdentifier, IsPHFetchOptions options) => burstIdentifier -> options -> IO (Id PHFetchResult)
fetchAssetsWithBurstIdentifier_options burstIdentifier options =
  do
    cls' <- getRequiredClass "PHAsset"
    withObjCPtr burstIdentifier $ \raw_burstIdentifier ->
      withObjCPtr options $ \raw_options ->
        sendClassMsg cls' (mkSelector "fetchAssetsWithBurstIdentifier:options:") (retPtr retVoid) [argPtr (castPtr raw_burstIdentifier :: Ptr ()), argPtr (castPtr raw_options :: Ptr ())] >>= retainedObject . castPtr

-- | @+ fetchAssetsWithOptions:@
fetchAssetsWithOptions :: IsPHFetchOptions options => options -> IO (Id PHFetchResult)
fetchAssetsWithOptions options =
  do
    cls' <- getRequiredClass "PHAsset"
    withObjCPtr options $ \raw_options ->
      sendClassMsg cls' (mkSelector "fetchAssetsWithOptions:") (retPtr retVoid) [argPtr (castPtr raw_options :: Ptr ())] >>= retainedObject . castPtr

-- | @+ fetchAssetsWithMediaType:options:@
fetchAssetsWithMediaType_options :: IsPHFetchOptions options => PHAssetMediaType -> options -> IO (Id PHFetchResult)
fetchAssetsWithMediaType_options mediaType options =
  do
    cls' <- getRequiredClass "PHAsset"
    withObjCPtr options $ \raw_options ->
      sendClassMsg cls' (mkSelector "fetchAssetsWithMediaType:options:") (retPtr retVoid) [argCLong (coerce mediaType), argPtr (castPtr raw_options :: Ptr ())] >>= retainedObject . castPtr

-- | @+ fetchAssetsWithALAssetURLs:options:@
fetchAssetsWithALAssetURLs_options :: (IsNSArray assetURLs, IsPHFetchOptions options) => assetURLs -> options -> IO (Id PHFetchResult)
fetchAssetsWithALAssetURLs_options assetURLs options =
  do
    cls' <- getRequiredClass "PHAsset"
    withObjCPtr assetURLs $ \raw_assetURLs ->
      withObjCPtr options $ \raw_options ->
        sendClassMsg cls' (mkSelector "fetchAssetsWithALAssetURLs:options:") (retPtr retVoid) [argPtr (castPtr raw_assetURLs :: Ptr ()), argPtr (castPtr raw_options :: Ptr ())] >>= retainedObject . castPtr

-- | @- requestContentEditingInputWithOptions:completionHandler:@
requestContentEditingInputWithOptions_completionHandler :: (IsPHAsset phAsset, IsPHContentEditingInputRequestOptions options) => phAsset -> options -> Ptr () -> IO CULong
requestContentEditingInputWithOptions_completionHandler phAsset  options completionHandler =
  withObjCPtr options $ \raw_options ->
      sendMsg phAsset (mkSelector "requestContentEditingInputWithOptions:completionHandler:") retCULong [argPtr (castPtr raw_options :: Ptr ()), argPtr (castPtr completionHandler :: Ptr ())]

-- | @- cancelContentEditingInputRequest:@
cancelContentEditingInputRequest :: IsPHAsset phAsset => phAsset -> CULong -> IO ()
cancelContentEditingInputRequest phAsset  requestID =
    sendMsg phAsset (mkSelector "cancelContentEditingInputRequest:") retVoid [argCULong requestID]

-- | @- playbackStyle@
playbackStyle :: IsPHAsset phAsset => phAsset -> IO PHAssetPlaybackStyle
playbackStyle phAsset  =
    fmap (coerce :: CLong -> PHAssetPlaybackStyle) $ sendMsg phAsset (mkSelector "playbackStyle") retCLong []

-- | @- mediaType@
mediaType :: IsPHAsset phAsset => phAsset -> IO PHAssetMediaType
mediaType phAsset  =
    fmap (coerce :: CLong -> PHAssetMediaType) $ sendMsg phAsset (mkSelector "mediaType") retCLong []

-- | @- mediaSubtypes@
mediaSubtypes :: IsPHAsset phAsset => phAsset -> IO PHAssetMediaSubtype
mediaSubtypes phAsset  =
    fmap (coerce :: CULong -> PHAssetMediaSubtype) $ sendMsg phAsset (mkSelector "mediaSubtypes") retCULong []

-- | The type of image or video data that is presented for the asset
--
-- ObjC selector: @- contentType@
contentType :: IsPHAsset phAsset => phAsset -> IO (Id UTType)
contentType phAsset  =
    sendMsg phAsset (mkSelector "contentType") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- pixelWidth@
pixelWidth :: IsPHAsset phAsset => phAsset -> IO CULong
pixelWidth phAsset  =
    sendMsg phAsset (mkSelector "pixelWidth") retCULong []

-- | @- pixelHeight@
pixelHeight :: IsPHAsset phAsset => phAsset -> IO CULong
pixelHeight phAsset  =
    sendMsg phAsset (mkSelector "pixelHeight") retCULong []

-- | The date and time of this asset's creation (can be updated by the user)
--
-- ObjC selector: @- creationDate@
creationDate :: IsPHAsset phAsset => phAsset -> IO (Id NSDate)
creationDate phAsset  =
    sendMsg phAsset (mkSelector "creationDate") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | The date and time of the last modification to this asset or one of its properties
--
-- ObjC selector: @- modificationDate@
modificationDate :: IsPHAsset phAsset => phAsset -> IO (Id NSDate)
modificationDate phAsset  =
    sendMsg phAsset (mkSelector "modificationDate") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | The date and time this asset was added to the photo library (from the device that was used to add this asset)
--
-- ObjC selector: @- addedDate@
addedDate :: IsPHAsset phAsset => phAsset -> IO (Id NSDate)
addedDate phAsset  =
    sendMsg phAsset (mkSelector "addedDate") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- duration@
duration :: IsPHAsset phAsset => phAsset -> IO CDouble
duration phAsset  =
    sendMsg phAsset (mkSelector "duration") retCDouble []

-- | @- hidden@
hidden :: IsPHAsset phAsset => phAsset -> IO Bool
hidden phAsset  =
    fmap ((/= 0) :: CULong -> Bool) $ sendMsg phAsset (mkSelector "hidden") retCULong []

-- | @- favorite@
favorite :: IsPHAsset phAsset => phAsset -> IO Bool
favorite phAsset  =
    fmap ((/= 0) :: CULong -> Bool) $ sendMsg phAsset (mkSelector "favorite") retCULong []

-- | @- syncFailureHidden@
syncFailureHidden :: IsPHAsset phAsset => phAsset -> IO Bool
syncFailureHidden phAsset  =
    fmap ((/= 0) :: CULong -> Bool) $ sendMsg phAsset (mkSelector "syncFailureHidden") retCULong []

-- | @- burstIdentifier@
burstIdentifier :: IsPHAsset phAsset => phAsset -> IO (Id NSString)
burstIdentifier phAsset  =
    sendMsg phAsset (mkSelector "burstIdentifier") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- burstSelectionTypes@
burstSelectionTypes :: IsPHAsset phAsset => phAsset -> IO PHAssetBurstSelectionType
burstSelectionTypes phAsset  =
    fmap (coerce :: CULong -> PHAssetBurstSelectionType) $ sendMsg phAsset (mkSelector "burstSelectionTypes") retCULong []

-- | @- representsBurst@
representsBurst :: IsPHAsset phAsset => phAsset -> IO Bool
representsBurst phAsset  =
    fmap ((/= 0) :: CULong -> Bool) $ sendMsg phAsset (mkSelector "representsBurst") retCULong []

-- | @- sourceType@
sourceType :: IsPHAsset phAsset => phAsset -> IO PHAssetSourceType
sourceType phAsset  =
    fmap (coerce :: CULong -> PHAssetSourceType) $ sendMsg phAsset (mkSelector "sourceType") retCULong []

-- | @- hasAdjustments@
hasAdjustments :: IsPHAsset phAsset => phAsset -> IO Bool
hasAdjustments phAsset  =
    fmap ((/= 0) :: CULong -> Bool) $ sendMsg phAsset (mkSelector "hasAdjustments") retCULong []

-- | @- adjustmentFormatIdentifier@
adjustmentFormatIdentifier :: IsPHAsset phAsset => phAsset -> IO (Id NSString)
adjustmentFormatIdentifier phAsset  =
    sendMsg phAsset (mkSelector "adjustmentFormatIdentifier") (retPtr retVoid) [] >>= retainedObject . castPtr

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @canPerformEditOperation:@
canPerformEditOperationSelector :: Selector
canPerformEditOperationSelector = mkSelector "canPerformEditOperation:"

-- | @Selector@ for @fetchAssetsInAssetCollection:options:@
fetchAssetsInAssetCollection_optionsSelector :: Selector
fetchAssetsInAssetCollection_optionsSelector = mkSelector "fetchAssetsInAssetCollection:options:"

-- | @Selector@ for @fetchAssetsWithLocalIdentifiers:options:@
fetchAssetsWithLocalIdentifiers_optionsSelector :: Selector
fetchAssetsWithLocalIdentifiers_optionsSelector = mkSelector "fetchAssetsWithLocalIdentifiers:options:"

-- | @Selector@ for @fetchKeyAssetsInAssetCollection:options:@
fetchKeyAssetsInAssetCollection_optionsSelector :: Selector
fetchKeyAssetsInAssetCollection_optionsSelector = mkSelector "fetchKeyAssetsInAssetCollection:options:"

-- | @Selector@ for @fetchAssetsWithBurstIdentifier:options:@
fetchAssetsWithBurstIdentifier_optionsSelector :: Selector
fetchAssetsWithBurstIdentifier_optionsSelector = mkSelector "fetchAssetsWithBurstIdentifier:options:"

-- | @Selector@ for @fetchAssetsWithOptions:@
fetchAssetsWithOptionsSelector :: Selector
fetchAssetsWithOptionsSelector = mkSelector "fetchAssetsWithOptions:"

-- | @Selector@ for @fetchAssetsWithMediaType:options:@
fetchAssetsWithMediaType_optionsSelector :: Selector
fetchAssetsWithMediaType_optionsSelector = mkSelector "fetchAssetsWithMediaType:options:"

-- | @Selector@ for @fetchAssetsWithALAssetURLs:options:@
fetchAssetsWithALAssetURLs_optionsSelector :: Selector
fetchAssetsWithALAssetURLs_optionsSelector = mkSelector "fetchAssetsWithALAssetURLs:options:"

-- | @Selector@ for @requestContentEditingInputWithOptions:completionHandler:@
requestContentEditingInputWithOptions_completionHandlerSelector :: Selector
requestContentEditingInputWithOptions_completionHandlerSelector = mkSelector "requestContentEditingInputWithOptions:completionHandler:"

-- | @Selector@ for @cancelContentEditingInputRequest:@
cancelContentEditingInputRequestSelector :: Selector
cancelContentEditingInputRequestSelector = mkSelector "cancelContentEditingInputRequest:"

-- | @Selector@ for @playbackStyle@
playbackStyleSelector :: Selector
playbackStyleSelector = mkSelector "playbackStyle"

-- | @Selector@ for @mediaType@
mediaTypeSelector :: Selector
mediaTypeSelector = mkSelector "mediaType"

-- | @Selector@ for @mediaSubtypes@
mediaSubtypesSelector :: Selector
mediaSubtypesSelector = mkSelector "mediaSubtypes"

-- | @Selector@ for @contentType@
contentTypeSelector :: Selector
contentTypeSelector = mkSelector "contentType"

-- | @Selector@ for @pixelWidth@
pixelWidthSelector :: Selector
pixelWidthSelector = mkSelector "pixelWidth"

-- | @Selector@ for @pixelHeight@
pixelHeightSelector :: Selector
pixelHeightSelector = mkSelector "pixelHeight"

-- | @Selector@ for @creationDate@
creationDateSelector :: Selector
creationDateSelector = mkSelector "creationDate"

-- | @Selector@ for @modificationDate@
modificationDateSelector :: Selector
modificationDateSelector = mkSelector "modificationDate"

-- | @Selector@ for @addedDate@
addedDateSelector :: Selector
addedDateSelector = mkSelector "addedDate"

-- | @Selector@ for @duration@
durationSelector :: Selector
durationSelector = mkSelector "duration"

-- | @Selector@ for @hidden@
hiddenSelector :: Selector
hiddenSelector = mkSelector "hidden"

-- | @Selector@ for @favorite@
favoriteSelector :: Selector
favoriteSelector = mkSelector "favorite"

-- | @Selector@ for @syncFailureHidden@
syncFailureHiddenSelector :: Selector
syncFailureHiddenSelector = mkSelector "syncFailureHidden"

-- | @Selector@ for @burstIdentifier@
burstIdentifierSelector :: Selector
burstIdentifierSelector = mkSelector "burstIdentifier"

-- | @Selector@ for @burstSelectionTypes@
burstSelectionTypesSelector :: Selector
burstSelectionTypesSelector = mkSelector "burstSelectionTypes"

-- | @Selector@ for @representsBurst@
representsBurstSelector :: Selector
representsBurstSelector = mkSelector "representsBurst"

-- | @Selector@ for @sourceType@
sourceTypeSelector :: Selector
sourceTypeSelector = mkSelector "sourceType"

-- | @Selector@ for @hasAdjustments@
hasAdjustmentsSelector :: Selector
hasAdjustmentsSelector = mkSelector "hasAdjustments"

-- | @Selector@ for @adjustmentFormatIdentifier@
adjustmentFormatIdentifierSelector :: Selector
adjustmentFormatIdentifierSelector = mkSelector "adjustmentFormatIdentifier"

