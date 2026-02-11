{-# LANGUAGE PatternSynonyms #-}
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
  , mediaTypeSelector
  , mediaSubtypesSelector
  , creationDateSelector
  , contentTypeSelector
  , uniformTypeIdentifierSelector
  , playbackStyleSelector
  , adjustmentDataSelector
  , displaySizeImageSelector
  , fullSizeImageURLSelector
  , fullSizeImageOrientationSelector
  , avAssetSelector
  , livePhotoSelector

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
import ObjC.AppKit.Internal.Classes
import ObjC.Foundation.Internal.Classes
import ObjC.UniformTypeIdentifiers.Internal.Classes

-- | @- mediaType@
mediaType :: IsPHContentEditingInput phContentEditingInput => phContentEditingInput -> IO PHAssetMediaType
mediaType phContentEditingInput  =
    fmap (coerce :: CLong -> PHAssetMediaType) $ sendMsg phContentEditingInput (mkSelector "mediaType") retCLong []

-- | @- mediaSubtypes@
mediaSubtypes :: IsPHContentEditingInput phContentEditingInput => phContentEditingInput -> IO PHAssetMediaSubtype
mediaSubtypes phContentEditingInput  =
    fmap (coerce :: CULong -> PHAssetMediaSubtype) $ sendMsg phContentEditingInput (mkSelector "mediaSubtypes") retCULong []

-- | @- creationDate@
creationDate :: IsPHContentEditingInput phContentEditingInput => phContentEditingInput -> IO (Id NSDate)
creationDate phContentEditingInput  =
    sendMsg phContentEditingInput (mkSelector "creationDate") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | The type of data provided as the asset's content editing input image or video.
--
-- ObjC selector: @- contentType@
contentType :: IsPHContentEditingInput phContentEditingInput => phContentEditingInput -> IO (Id UTType)
contentType phContentEditingInput  =
    sendMsg phContentEditingInput (mkSelector "contentType") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- uniformTypeIdentifier@
uniformTypeIdentifier :: IsPHContentEditingInput phContentEditingInput => phContentEditingInput -> IO (Id NSString)
uniformTypeIdentifier phContentEditingInput  =
    sendMsg phContentEditingInput (mkSelector "uniformTypeIdentifier") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- playbackStyle@
playbackStyle :: IsPHContentEditingInput phContentEditingInput => phContentEditingInput -> IO PHAssetPlaybackStyle
playbackStyle phContentEditingInput  =
    fmap (coerce :: CLong -> PHAssetPlaybackStyle) $ sendMsg phContentEditingInput (mkSelector "playbackStyle") retCLong []

-- | @- adjustmentData@
adjustmentData :: IsPHContentEditingInput phContentEditingInput => phContentEditingInput -> IO (Id PHAdjustmentData)
adjustmentData phContentEditingInput  =
    sendMsg phContentEditingInput (mkSelector "adjustmentData") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- displaySizeImage@
displaySizeImage :: IsPHContentEditingInput phContentEditingInput => phContentEditingInput -> IO (Id NSImage)
displaySizeImage phContentEditingInput  =
    sendMsg phContentEditingInput (mkSelector "displaySizeImage") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- fullSizeImageURL@
fullSizeImageURL :: IsPHContentEditingInput phContentEditingInput => phContentEditingInput -> IO (Id NSURL)
fullSizeImageURL phContentEditingInput  =
    sendMsg phContentEditingInput (mkSelector "fullSizeImageURL") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- fullSizeImageOrientation@
fullSizeImageOrientation :: IsPHContentEditingInput phContentEditingInput => phContentEditingInput -> IO CInt
fullSizeImageOrientation phContentEditingInput  =
    sendMsg phContentEditingInput (mkSelector "fullSizeImageOrientation") retCInt []

-- | @- avAsset@
avAsset :: IsPHContentEditingInput phContentEditingInput => phContentEditingInput -> IO RawId
avAsset phContentEditingInput  =
    fmap (RawId . castPtr) $ sendMsg phContentEditingInput (mkSelector "avAsset") (retPtr retVoid) []

-- | @- livePhoto@
livePhoto :: IsPHContentEditingInput phContentEditingInput => phContentEditingInput -> IO (Id PHLivePhoto)
livePhoto phContentEditingInput  =
    sendMsg phContentEditingInput (mkSelector "livePhoto") (retPtr retVoid) [] >>= retainedObject . castPtr

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @mediaType@
mediaTypeSelector :: Selector
mediaTypeSelector = mkSelector "mediaType"

-- | @Selector@ for @mediaSubtypes@
mediaSubtypesSelector :: Selector
mediaSubtypesSelector = mkSelector "mediaSubtypes"

-- | @Selector@ for @creationDate@
creationDateSelector :: Selector
creationDateSelector = mkSelector "creationDate"

-- | @Selector@ for @contentType@
contentTypeSelector :: Selector
contentTypeSelector = mkSelector "contentType"

-- | @Selector@ for @uniformTypeIdentifier@
uniformTypeIdentifierSelector :: Selector
uniformTypeIdentifierSelector = mkSelector "uniformTypeIdentifier"

-- | @Selector@ for @playbackStyle@
playbackStyleSelector :: Selector
playbackStyleSelector = mkSelector "playbackStyle"

-- | @Selector@ for @adjustmentData@
adjustmentDataSelector :: Selector
adjustmentDataSelector = mkSelector "adjustmentData"

-- | @Selector@ for @displaySizeImage@
displaySizeImageSelector :: Selector
displaySizeImageSelector = mkSelector "displaySizeImage"

-- | @Selector@ for @fullSizeImageURL@
fullSizeImageURLSelector :: Selector
fullSizeImageURLSelector = mkSelector "fullSizeImageURL"

-- | @Selector@ for @fullSizeImageOrientation@
fullSizeImageOrientationSelector :: Selector
fullSizeImageOrientationSelector = mkSelector "fullSizeImageOrientation"

-- | @Selector@ for @avAsset@
avAssetSelector :: Selector
avAssetSelector = mkSelector "avAsset"

-- | @Selector@ for @livePhoto@
livePhotoSelector :: Selector
livePhotoSelector = mkSelector "livePhoto"

