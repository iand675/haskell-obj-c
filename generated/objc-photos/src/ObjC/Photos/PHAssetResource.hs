{-# LANGUAGE PatternSynonyms #-}
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
  , assetResourcesForAssetSelector
  , assetResourcesForLivePhotoSelector
  , typeSelector
  , assetLocalIdentifierSelector
  , originalFilenameSelector
  , contentTypeSelector
  , uniformTypeIdentifierSelector
  , pixelWidthSelector
  , pixelHeightSelector

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

-- | @+ assetResourcesForAsset:@
assetResourcesForAsset :: IsPHAsset asset => asset -> IO (Id NSArray)
assetResourcesForAsset asset =
  do
    cls' <- getRequiredClass "PHAssetResource"
    withObjCPtr asset $ \raw_asset ->
      sendClassMsg cls' (mkSelector "assetResourcesForAsset:") (retPtr retVoid) [argPtr (castPtr raw_asset :: Ptr ())] >>= retainedObject . castPtr

-- | @+ assetResourcesForLivePhoto:@
assetResourcesForLivePhoto :: IsPHLivePhoto livePhoto => livePhoto -> IO (Id NSArray)
assetResourcesForLivePhoto livePhoto =
  do
    cls' <- getRequiredClass "PHAssetResource"
    withObjCPtr livePhoto $ \raw_livePhoto ->
      sendClassMsg cls' (mkSelector "assetResourcesForLivePhoto:") (retPtr retVoid) [argPtr (castPtr raw_livePhoto :: Ptr ())] >>= retainedObject . castPtr

-- | @- type@
type_ :: IsPHAssetResource phAssetResource => phAssetResource -> IO PHAssetResourceType
type_ phAssetResource  =
  fmap (coerce :: CLong -> PHAssetResourceType) $ sendMsg phAssetResource (mkSelector "type") retCLong []

-- | @- assetLocalIdentifier@
assetLocalIdentifier :: IsPHAssetResource phAssetResource => phAssetResource -> IO (Id NSString)
assetLocalIdentifier phAssetResource  =
  sendMsg phAssetResource (mkSelector "assetLocalIdentifier") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- originalFilename@
originalFilename :: IsPHAssetResource phAssetResource => phAssetResource -> IO (Id NSString)
originalFilename phAssetResource  =
  sendMsg phAssetResource (mkSelector "originalFilename") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | The type of data associated with this asset resource (the data can be retrieved via PHAssetResourceManager)
--
-- ObjC selector: @- contentType@
contentType :: IsPHAssetResource phAssetResource => phAssetResource -> IO (Id UTType)
contentType phAssetResource  =
  sendMsg phAssetResource (mkSelector "contentType") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- uniformTypeIdentifier@
uniformTypeIdentifier :: IsPHAssetResource phAssetResource => phAssetResource -> IO (Id NSString)
uniformTypeIdentifier phAssetResource  =
  sendMsg phAssetResource (mkSelector "uniformTypeIdentifier") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- pixelWidth@
pixelWidth :: IsPHAssetResource phAssetResource => phAssetResource -> IO CLong
pixelWidth phAssetResource  =
  sendMsg phAssetResource (mkSelector "pixelWidth") retCLong []

-- | @- pixelHeight@
pixelHeight :: IsPHAssetResource phAssetResource => phAssetResource -> IO CLong
pixelHeight phAssetResource  =
  sendMsg phAssetResource (mkSelector "pixelHeight") retCLong []

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @assetResourcesForAsset:@
assetResourcesForAssetSelector :: Selector
assetResourcesForAssetSelector = mkSelector "assetResourcesForAsset:"

-- | @Selector@ for @assetResourcesForLivePhoto:@
assetResourcesForLivePhotoSelector :: Selector
assetResourcesForLivePhotoSelector = mkSelector "assetResourcesForLivePhoto:"

-- | @Selector@ for @type@
typeSelector :: Selector
typeSelector = mkSelector "type"

-- | @Selector@ for @assetLocalIdentifier@
assetLocalIdentifierSelector :: Selector
assetLocalIdentifierSelector = mkSelector "assetLocalIdentifier"

-- | @Selector@ for @originalFilename@
originalFilenameSelector :: Selector
originalFilenameSelector = mkSelector "originalFilename"

-- | @Selector@ for @contentType@
contentTypeSelector :: Selector
contentTypeSelector = mkSelector "contentType"

-- | @Selector@ for @uniformTypeIdentifier@
uniformTypeIdentifierSelector :: Selector
uniformTypeIdentifierSelector = mkSelector "uniformTypeIdentifier"

-- | @Selector@ for @pixelWidth@
pixelWidthSelector :: Selector
pixelWidthSelector = mkSelector "pixelWidth"

-- | @Selector@ for @pixelHeight@
pixelHeightSelector :: Selector
pixelHeightSelector = mkSelector "pixelHeight"

