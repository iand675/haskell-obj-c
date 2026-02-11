{-# LANGUAGE PatternSynonyms #-}
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
  , creationRequestForAssetSelector
  , supportsAssetResourceTypesSelector
  , addResourceWithType_fileURL_optionsSelector
  , addResourceWithType_data_optionsSelector

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

-- | @+ creationRequestForAsset@
creationRequestForAsset :: IO (Id PHAssetCreationRequest)
creationRequestForAsset  =
  do
    cls' <- getRequiredClass "PHAssetCreationRequest"
    sendClassMsg cls' (mkSelector "creationRequestForAsset") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @+ supportsAssetResourceTypes:@
supportsAssetResourceTypes :: IsNSArray types => types -> IO Bool
supportsAssetResourceTypes types =
  do
    cls' <- getRequiredClass "PHAssetCreationRequest"
    withObjCPtr types $ \raw_types ->
      fmap ((/= 0) :: CULong -> Bool) $ sendClassMsg cls' (mkSelector "supportsAssetResourceTypes:") retCULong [argPtr (castPtr raw_types :: Ptr ())]

-- | @- addResourceWithType:fileURL:options:@
addResourceWithType_fileURL_options :: (IsPHAssetCreationRequest phAssetCreationRequest, IsNSURL fileURL, IsPHAssetResourceCreationOptions options) => phAssetCreationRequest -> PHAssetResourceType -> fileURL -> options -> IO ()
addResourceWithType_fileURL_options phAssetCreationRequest  type_ fileURL options =
withObjCPtr fileURL $ \raw_fileURL ->
  withObjCPtr options $ \raw_options ->
      sendMsg phAssetCreationRequest (mkSelector "addResourceWithType:fileURL:options:") retVoid [argCLong (coerce type_), argPtr (castPtr raw_fileURL :: Ptr ()), argPtr (castPtr raw_options :: Ptr ())]

-- | @- addResourceWithType:data:options:@
addResourceWithType_data_options :: (IsPHAssetCreationRequest phAssetCreationRequest, IsNSData data_, IsPHAssetResourceCreationOptions options) => phAssetCreationRequest -> PHAssetResourceType -> data_ -> options -> IO ()
addResourceWithType_data_options phAssetCreationRequest  type_ data_ options =
withObjCPtr data_ $ \raw_data_ ->
  withObjCPtr options $ \raw_options ->
      sendMsg phAssetCreationRequest (mkSelector "addResourceWithType:data:options:") retVoid [argCLong (coerce type_), argPtr (castPtr raw_data_ :: Ptr ()), argPtr (castPtr raw_options :: Ptr ())]

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @creationRequestForAsset@
creationRequestForAssetSelector :: Selector
creationRequestForAssetSelector = mkSelector "creationRequestForAsset"

-- | @Selector@ for @supportsAssetResourceTypes:@
supportsAssetResourceTypesSelector :: Selector
supportsAssetResourceTypesSelector = mkSelector "supportsAssetResourceTypes:"

-- | @Selector@ for @addResourceWithType:fileURL:options:@
addResourceWithType_fileURL_optionsSelector :: Selector
addResourceWithType_fileURL_optionsSelector = mkSelector "addResourceWithType:fileURL:options:"

-- | @Selector@ for @addResourceWithType:data:options:@
addResourceWithType_data_optionsSelector :: Selector
addResourceWithType_data_optionsSelector = mkSelector "addResourceWithType:data:options:"

