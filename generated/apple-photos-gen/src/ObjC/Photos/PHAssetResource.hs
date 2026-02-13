{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Generated bindings for @PHAssetResource@.
module ObjC.Photos.PHAssetResource
  ( PHAssetResource
  , IsPHAssetResource(..)
  , assetResourcesForAsset
  , assetResourcesForLivePhoto
  , type_
  , assetLocalIdentifier
  , originalFilename
  , contentType
  , uniformTypeIdentifier
  , pixelWidth
  , pixelHeight
  , assetLocalIdentifierSelector
  , assetResourcesForAssetSelector
  , assetResourcesForLivePhotoSelector
  , contentTypeSelector
  , originalFilenameSelector
  , pixelHeightSelector
  , pixelWidthSelector
  , typeSelector
  , uniformTypeIdentifierSelector

  -- * Enum types
  , PHAssetResourceType(PHAssetResourceType)
  , pattern PHAssetResourceTypePhoto
  , pattern PHAssetResourceTypeVideo
  , pattern PHAssetResourceTypeAudio
  , pattern PHAssetResourceTypeAlternatePhoto
  , pattern PHAssetResourceTypeFullSizePhoto
  , pattern PHAssetResourceTypeFullSizeVideo
  , pattern PHAssetResourceTypeAdjustmentData
  , pattern PHAssetResourceTypeAdjustmentBasePhoto
  , pattern PHAssetResourceTypePairedVideo
  , pattern PHAssetResourceTypeFullSizePairedVideo
  , pattern PHAssetResourceTypeAdjustmentBasePairedVideo
  , pattern PHAssetResourceTypeAdjustmentBaseVideo
  , pattern PHAssetResourceTypePhotoProxy

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

-- | @+ assetResourcesForAsset:@
assetResourcesForAsset :: IsPHAsset asset => asset -> IO (Id NSArray)
assetResourcesForAsset asset =
  do
    cls' <- getRequiredClass "PHAssetResource"
    sendClassMessage cls' assetResourcesForAssetSelector (toPHAsset asset)

-- | @+ assetResourcesForLivePhoto:@
assetResourcesForLivePhoto :: IsPHLivePhoto livePhoto => livePhoto -> IO (Id NSArray)
assetResourcesForLivePhoto livePhoto =
  do
    cls' <- getRequiredClass "PHAssetResource"
    sendClassMessage cls' assetResourcesForLivePhotoSelector (toPHLivePhoto livePhoto)

-- | @- type@
type_ :: IsPHAssetResource phAssetResource => phAssetResource -> IO PHAssetResourceType
type_ phAssetResource =
  sendMessage phAssetResource typeSelector

-- | @- assetLocalIdentifier@
assetLocalIdentifier :: IsPHAssetResource phAssetResource => phAssetResource -> IO (Id NSString)
assetLocalIdentifier phAssetResource =
  sendMessage phAssetResource assetLocalIdentifierSelector

-- | @- originalFilename@
originalFilename :: IsPHAssetResource phAssetResource => phAssetResource -> IO (Id NSString)
originalFilename phAssetResource =
  sendMessage phAssetResource originalFilenameSelector

-- | The type of data associated with this asset resource (the data can be retrieved via PHAssetResourceManager)
--
-- ObjC selector: @- contentType@
contentType :: IsPHAssetResource phAssetResource => phAssetResource -> IO (Id UTType)
contentType phAssetResource =
  sendMessage phAssetResource contentTypeSelector

-- | @- uniformTypeIdentifier@
uniformTypeIdentifier :: IsPHAssetResource phAssetResource => phAssetResource -> IO (Id NSString)
uniformTypeIdentifier phAssetResource =
  sendMessage phAssetResource uniformTypeIdentifierSelector

-- | @- pixelWidth@
pixelWidth :: IsPHAssetResource phAssetResource => phAssetResource -> IO CLong
pixelWidth phAssetResource =
  sendMessage phAssetResource pixelWidthSelector

-- | @- pixelHeight@
pixelHeight :: IsPHAssetResource phAssetResource => phAssetResource -> IO CLong
pixelHeight phAssetResource =
  sendMessage phAssetResource pixelHeightSelector

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @assetResourcesForAsset:@
assetResourcesForAssetSelector :: Selector '[Id PHAsset] (Id NSArray)
assetResourcesForAssetSelector = mkSelector "assetResourcesForAsset:"

-- | @Selector@ for @assetResourcesForLivePhoto:@
assetResourcesForLivePhotoSelector :: Selector '[Id PHLivePhoto] (Id NSArray)
assetResourcesForLivePhotoSelector = mkSelector "assetResourcesForLivePhoto:"

-- | @Selector@ for @type@
typeSelector :: Selector '[] PHAssetResourceType
typeSelector = mkSelector "type"

-- | @Selector@ for @assetLocalIdentifier@
assetLocalIdentifierSelector :: Selector '[] (Id NSString)
assetLocalIdentifierSelector = mkSelector "assetLocalIdentifier"

-- | @Selector@ for @originalFilename@
originalFilenameSelector :: Selector '[] (Id NSString)
originalFilenameSelector = mkSelector "originalFilename"

-- | @Selector@ for @contentType@
contentTypeSelector :: Selector '[] (Id UTType)
contentTypeSelector = mkSelector "contentType"

-- | @Selector@ for @uniformTypeIdentifier@
uniformTypeIdentifierSelector :: Selector '[] (Id NSString)
uniformTypeIdentifierSelector = mkSelector "uniformTypeIdentifier"

-- | @Selector@ for @pixelWidth@
pixelWidthSelector :: Selector '[] CLong
pixelWidthSelector = mkSelector "pixelWidth"

-- | @Selector@ for @pixelHeight@
pixelHeightSelector :: Selector '[] CLong
pixelHeightSelector = mkSelector "pixelHeight"

