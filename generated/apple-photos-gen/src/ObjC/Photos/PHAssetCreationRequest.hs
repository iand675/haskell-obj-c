{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Generated bindings for @PHAssetCreationRequest@.
module ObjC.Photos.PHAssetCreationRequest
  ( PHAssetCreationRequest
  , IsPHAssetCreationRequest(..)
  , creationRequestForAsset
  , supportsAssetResourceTypes
  , addResourceWithType_fileURL_options
  , addResourceWithType_data_options
  , addResourceWithType_data_optionsSelector
  , addResourceWithType_fileURL_optionsSelector
  , creationRequestForAssetSelector
  , supportsAssetResourceTypesSelector

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

-- | @+ creationRequestForAsset@
creationRequestForAsset :: IO (Id PHAssetCreationRequest)
creationRequestForAsset  =
  do
    cls' <- getRequiredClass "PHAssetCreationRequest"
    sendClassMessage cls' creationRequestForAssetSelector

-- | @+ supportsAssetResourceTypes:@
supportsAssetResourceTypes :: IsNSArray types => types -> IO Bool
supportsAssetResourceTypes types =
  do
    cls' <- getRequiredClass "PHAssetCreationRequest"
    sendClassMessage cls' supportsAssetResourceTypesSelector (toNSArray types)

-- | @- addResourceWithType:fileURL:options:@
addResourceWithType_fileURL_options :: (IsPHAssetCreationRequest phAssetCreationRequest, IsNSURL fileURL, IsPHAssetResourceCreationOptions options) => phAssetCreationRequest -> PHAssetResourceType -> fileURL -> options -> IO ()
addResourceWithType_fileURL_options phAssetCreationRequest type_ fileURL options =
  sendMessage phAssetCreationRequest addResourceWithType_fileURL_optionsSelector type_ (toNSURL fileURL) (toPHAssetResourceCreationOptions options)

-- | @- addResourceWithType:data:options:@
addResourceWithType_data_options :: (IsPHAssetCreationRequest phAssetCreationRequest, IsNSData data_, IsPHAssetResourceCreationOptions options) => phAssetCreationRequest -> PHAssetResourceType -> data_ -> options -> IO ()
addResourceWithType_data_options phAssetCreationRequest type_ data_ options =
  sendMessage phAssetCreationRequest addResourceWithType_data_optionsSelector type_ (toNSData data_) (toPHAssetResourceCreationOptions options)

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @creationRequestForAsset@
creationRequestForAssetSelector :: Selector '[] (Id PHAssetCreationRequest)
creationRequestForAssetSelector = mkSelector "creationRequestForAsset"

-- | @Selector@ for @supportsAssetResourceTypes:@
supportsAssetResourceTypesSelector :: Selector '[Id NSArray] Bool
supportsAssetResourceTypesSelector = mkSelector "supportsAssetResourceTypes:"

-- | @Selector@ for @addResourceWithType:fileURL:options:@
addResourceWithType_fileURL_optionsSelector :: Selector '[PHAssetResourceType, Id NSURL, Id PHAssetResourceCreationOptions] ()
addResourceWithType_fileURL_optionsSelector = mkSelector "addResourceWithType:fileURL:options:"

-- | @Selector@ for @addResourceWithType:data:options:@
addResourceWithType_data_optionsSelector :: Selector '[PHAssetResourceType, Id NSData, Id PHAssetResourceCreationOptions] ()
addResourceWithType_data_optionsSelector = mkSelector "addResourceWithType:data:options:"

