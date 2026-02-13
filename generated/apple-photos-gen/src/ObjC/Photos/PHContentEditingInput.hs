{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Generated bindings for @PHContentEditingInput@.
module ObjC.Photos.PHContentEditingInput
  ( PHContentEditingInput
  , IsPHContentEditingInput(..)
  , mediaType
  , mediaSubtypes
  , creationDate
  , contentType
  , uniformTypeIdentifier
  , playbackStyle
  , adjustmentData
  , displaySizeImage
  , fullSizeImageURL
  , fullSizeImageOrientation
  , avAsset
  , livePhoto
  , adjustmentDataSelector
  , avAssetSelector
  , contentTypeSelector
  , creationDateSelector
  , displaySizeImageSelector
  , fullSizeImageOrientationSelector
  , fullSizeImageURLSelector
  , livePhotoSelector
  , mediaSubtypesSelector
  , mediaTypeSelector
  , playbackStyleSelector
  , uniformTypeIdentifierSelector

  -- * Enum types
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

  ) where

import Foreign.Ptr (Ptr, FunPtr)
import Foreign.C.Types

import ObjC.Runtime.Types
import ObjC.Runtime.Message (sendMessage, sendOwnedMessage, sendClassMessage, sendOwnedClassMessage)
import ObjC.Runtime.Selector (mkSelector)
import ObjC.Runtime.Class (getRequiredClass)

import ObjC.Photos.Internal.Classes
import ObjC.Photos.Internal.Enums
import ObjC.AppKit.Internal.Classes
import ObjC.Foundation.Internal.Classes
import ObjC.UniformTypeIdentifiers.Internal.Classes

-- | @- mediaType@
mediaType :: IsPHContentEditingInput phContentEditingInput => phContentEditingInput -> IO PHAssetMediaType
mediaType phContentEditingInput =
  sendMessage phContentEditingInput mediaTypeSelector

-- | @- mediaSubtypes@
mediaSubtypes :: IsPHContentEditingInput phContentEditingInput => phContentEditingInput -> IO PHAssetMediaSubtype
mediaSubtypes phContentEditingInput =
  sendMessage phContentEditingInput mediaSubtypesSelector

-- | @- creationDate@
creationDate :: IsPHContentEditingInput phContentEditingInput => phContentEditingInput -> IO (Id NSDate)
creationDate phContentEditingInput =
  sendMessage phContentEditingInput creationDateSelector

-- | The type of data provided as the asset's content editing input image or video.
--
-- ObjC selector: @- contentType@
contentType :: IsPHContentEditingInput phContentEditingInput => phContentEditingInput -> IO (Id UTType)
contentType phContentEditingInput =
  sendMessage phContentEditingInput contentTypeSelector

-- | @- uniformTypeIdentifier@
uniformTypeIdentifier :: IsPHContentEditingInput phContentEditingInput => phContentEditingInput -> IO (Id NSString)
uniformTypeIdentifier phContentEditingInput =
  sendMessage phContentEditingInput uniformTypeIdentifierSelector

-- | @- playbackStyle@
playbackStyle :: IsPHContentEditingInput phContentEditingInput => phContentEditingInput -> IO PHAssetPlaybackStyle
playbackStyle phContentEditingInput =
  sendMessage phContentEditingInput playbackStyleSelector

-- | @- adjustmentData@
adjustmentData :: IsPHContentEditingInput phContentEditingInput => phContentEditingInput -> IO (Id PHAdjustmentData)
adjustmentData phContentEditingInput =
  sendMessage phContentEditingInput adjustmentDataSelector

-- | @- displaySizeImage@
displaySizeImage :: IsPHContentEditingInput phContentEditingInput => phContentEditingInput -> IO (Id NSImage)
displaySizeImage phContentEditingInput =
  sendMessage phContentEditingInput displaySizeImageSelector

-- | @- fullSizeImageURL@
fullSizeImageURL :: IsPHContentEditingInput phContentEditingInput => phContentEditingInput -> IO (Id NSURL)
fullSizeImageURL phContentEditingInput =
  sendMessage phContentEditingInput fullSizeImageURLSelector

-- | @- fullSizeImageOrientation@
fullSizeImageOrientation :: IsPHContentEditingInput phContentEditingInput => phContentEditingInput -> IO CInt
fullSizeImageOrientation phContentEditingInput =
  sendMessage phContentEditingInput fullSizeImageOrientationSelector

-- | @- avAsset@
avAsset :: IsPHContentEditingInput phContentEditingInput => phContentEditingInput -> IO RawId
avAsset phContentEditingInput =
  sendMessage phContentEditingInput avAssetSelector

-- | @- livePhoto@
livePhoto :: IsPHContentEditingInput phContentEditingInput => phContentEditingInput -> IO (Id PHLivePhoto)
livePhoto phContentEditingInput =
  sendMessage phContentEditingInput livePhotoSelector

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @mediaType@
mediaTypeSelector :: Selector '[] PHAssetMediaType
mediaTypeSelector = mkSelector "mediaType"

-- | @Selector@ for @mediaSubtypes@
mediaSubtypesSelector :: Selector '[] PHAssetMediaSubtype
mediaSubtypesSelector = mkSelector "mediaSubtypes"

-- | @Selector@ for @creationDate@
creationDateSelector :: Selector '[] (Id NSDate)
creationDateSelector = mkSelector "creationDate"

-- | @Selector@ for @contentType@
contentTypeSelector :: Selector '[] (Id UTType)
contentTypeSelector = mkSelector "contentType"

-- | @Selector@ for @uniformTypeIdentifier@
uniformTypeIdentifierSelector :: Selector '[] (Id NSString)
uniformTypeIdentifierSelector = mkSelector "uniformTypeIdentifier"

-- | @Selector@ for @playbackStyle@
playbackStyleSelector :: Selector '[] PHAssetPlaybackStyle
playbackStyleSelector = mkSelector "playbackStyle"

-- | @Selector@ for @adjustmentData@
adjustmentDataSelector :: Selector '[] (Id PHAdjustmentData)
adjustmentDataSelector = mkSelector "adjustmentData"

-- | @Selector@ for @displaySizeImage@
displaySizeImageSelector :: Selector '[] (Id NSImage)
displaySizeImageSelector = mkSelector "displaySizeImage"

-- | @Selector@ for @fullSizeImageURL@
fullSizeImageURLSelector :: Selector '[] (Id NSURL)
fullSizeImageURLSelector = mkSelector "fullSizeImageURL"

-- | @Selector@ for @fullSizeImageOrientation@
fullSizeImageOrientationSelector :: Selector '[] CInt
fullSizeImageOrientationSelector = mkSelector "fullSizeImageOrientation"

-- | @Selector@ for @avAsset@
avAssetSelector :: Selector '[] RawId
avAssetSelector = mkSelector "avAsset"

-- | @Selector@ for @livePhoto@
livePhotoSelector :: Selector '[] (Id PHLivePhoto)
livePhotoSelector = mkSelector "livePhoto"

