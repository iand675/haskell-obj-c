{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Generated bindings for @PHImageManager@.
module ObjC.Photos.PHImageManager
  ( PHImageManager
  , IsPHImageManager(..)
  , defaultManager
  , requestImageDataForAsset_options_resultHandler
  , requestImageDataAndOrientationForAsset_options_resultHandler
  , cancelImageRequest
  , requestPlayerItemForVideo_options_resultHandler
  , requestExportSessionForVideo_options_exportPreset_resultHandler
  , requestAVAssetForVideo_options_resultHandler
  , cancelImageRequestSelector
  , defaultManagerSelector
  , requestAVAssetForVideo_options_resultHandlerSelector
  , requestExportSessionForVideo_options_exportPreset_resultHandlerSelector
  , requestImageDataAndOrientationForAsset_options_resultHandlerSelector
  , requestImageDataForAsset_options_resultHandlerSelector
  , requestPlayerItemForVideo_options_resultHandlerSelector

  -- * Enum types
  , PHImageContentMode(PHImageContentMode)
  , pattern PHImageContentModeAspectFit
  , pattern PHImageContentModeAspectFill
  , pattern PHImageContentModeDefault

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

-- | @+ defaultManager@
defaultManager :: IO (Id PHImageManager)
defaultManager  =
  do
    cls' <- getRequiredClass "PHImageManager"
    sendClassMessage cls' defaultManagerSelector

-- | Request largest represented image as data bytes for the specified asset.
--
-- @asset@ — The asset whose image data is to be loaded.
--
-- @options@ — Options specifying how Photos should handle the request, format the requested image, and notify your app of progress or errors.      If PHImageRequestOptionsVersionCurrent is requested and the asset has adjustments then the largest rendered image data is returned. In all other cases then the original image data is returned.
--
-- @resultHandler@ — A block that is called exactly once either synchronously on the current thread or asynchronously on the main thread depending on the synchronous option specified in the PHImageRequestOptions options parameter (deliveryMode is ignored).
--
-- ObjC selector: @- requestImageDataForAsset:options:resultHandler:@
requestImageDataForAsset_options_resultHandler :: (IsPHImageManager phImageManager, IsPHAsset asset, IsPHImageRequestOptions options) => phImageManager -> asset -> options -> Ptr () -> IO CInt
requestImageDataForAsset_options_resultHandler phImageManager asset options resultHandler =
  sendMessage phImageManager requestImageDataForAsset_options_resultHandlerSelector (toPHAsset asset) (toPHImageRequestOptions options) resultHandler

-- | Request largest represented image as data bytes and EXIF orientation for the specified asset.
--
-- @asset@ — The asset whose image data is to be loaded.
--
-- @options@ — Options specifying how Photos should handle the request, format the requested image, and notify your app of progress or errors. If PHImageRequestOptionsVersionCurrent is requested and the asset has adjustments then the largest rendered image data is returned. In all other cases then the original image data is returned.
--
-- @resultHandler@ — A block that is called exactly once either synchronously on the current thread or asynchronously on the main thread depending on the synchronous option specified in the PHImageRequestOptions options parameter (deliveryMode is ignored). Orientation is an EXIF orientation as an CGImagePropertyOrientation. For iOS or tvOS, convert this to an UIImageOrientation.
--
-- ObjC selector: @- requestImageDataAndOrientationForAsset:options:resultHandler:@
requestImageDataAndOrientationForAsset_options_resultHandler :: (IsPHImageManager phImageManager, IsPHAsset asset, IsPHImageRequestOptions options) => phImageManager -> asset -> options -> Ptr () -> IO CInt
requestImageDataAndOrientationForAsset_options_resultHandler phImageManager asset options resultHandler =
  sendMessage phImageManager requestImageDataAndOrientationForAsset_options_resultHandlerSelector (toPHAsset asset) (toPHImageRequestOptions options) resultHandler

-- | @- cancelImageRequest:@
cancelImageRequest :: IsPHImageManager phImageManager => phImageManager -> CInt -> IO ()
cancelImageRequest phImageManager requestID =
  sendMessage phImageManager cancelImageRequestSelector requestID

-- | @- requestPlayerItemForVideo:options:resultHandler:@
requestPlayerItemForVideo_options_resultHandler :: (IsPHImageManager phImageManager, IsPHAsset asset, IsPHVideoRequestOptions options) => phImageManager -> asset -> options -> Ptr () -> IO CInt
requestPlayerItemForVideo_options_resultHandler phImageManager asset options resultHandler =
  sendMessage phImageManager requestPlayerItemForVideo_options_resultHandlerSelector (toPHAsset asset) (toPHVideoRequestOptions options) resultHandler

-- | @- requestExportSessionForVideo:options:exportPreset:resultHandler:@
requestExportSessionForVideo_options_exportPreset_resultHandler :: (IsPHImageManager phImageManager, IsPHAsset asset, IsPHVideoRequestOptions options, IsNSString exportPreset) => phImageManager -> asset -> options -> exportPreset -> Ptr () -> IO CInt
requestExportSessionForVideo_options_exportPreset_resultHandler phImageManager asset options exportPreset resultHandler =
  sendMessage phImageManager requestExportSessionForVideo_options_exportPreset_resultHandlerSelector (toPHAsset asset) (toPHVideoRequestOptions options) (toNSString exportPreset) resultHandler

-- | @- requestAVAssetForVideo:options:resultHandler:@
requestAVAssetForVideo_options_resultHandler :: (IsPHImageManager phImageManager, IsPHAsset asset, IsPHVideoRequestOptions options) => phImageManager -> asset -> options -> Ptr () -> IO CInt
requestAVAssetForVideo_options_resultHandler phImageManager asset options resultHandler =
  sendMessage phImageManager requestAVAssetForVideo_options_resultHandlerSelector (toPHAsset asset) (toPHVideoRequestOptions options) resultHandler

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @defaultManager@
defaultManagerSelector :: Selector '[] (Id PHImageManager)
defaultManagerSelector = mkSelector "defaultManager"

-- | @Selector@ for @requestImageDataForAsset:options:resultHandler:@
requestImageDataForAsset_options_resultHandlerSelector :: Selector '[Id PHAsset, Id PHImageRequestOptions, Ptr ()] CInt
requestImageDataForAsset_options_resultHandlerSelector = mkSelector "requestImageDataForAsset:options:resultHandler:"

-- | @Selector@ for @requestImageDataAndOrientationForAsset:options:resultHandler:@
requestImageDataAndOrientationForAsset_options_resultHandlerSelector :: Selector '[Id PHAsset, Id PHImageRequestOptions, Ptr ()] CInt
requestImageDataAndOrientationForAsset_options_resultHandlerSelector = mkSelector "requestImageDataAndOrientationForAsset:options:resultHandler:"

-- | @Selector@ for @cancelImageRequest:@
cancelImageRequestSelector :: Selector '[CInt] ()
cancelImageRequestSelector = mkSelector "cancelImageRequest:"

-- | @Selector@ for @requestPlayerItemForVideo:options:resultHandler:@
requestPlayerItemForVideo_options_resultHandlerSelector :: Selector '[Id PHAsset, Id PHVideoRequestOptions, Ptr ()] CInt
requestPlayerItemForVideo_options_resultHandlerSelector = mkSelector "requestPlayerItemForVideo:options:resultHandler:"

-- | @Selector@ for @requestExportSessionForVideo:options:exportPreset:resultHandler:@
requestExportSessionForVideo_options_exportPreset_resultHandlerSelector :: Selector '[Id PHAsset, Id PHVideoRequestOptions, Id NSString, Ptr ()] CInt
requestExportSessionForVideo_options_exportPreset_resultHandlerSelector = mkSelector "requestExportSessionForVideo:options:exportPreset:resultHandler:"

-- | @Selector@ for @requestAVAssetForVideo:options:resultHandler:@
requestAVAssetForVideo_options_resultHandlerSelector :: Selector '[Id PHAsset, Id PHVideoRequestOptions, Ptr ()] CInt
requestAVAssetForVideo_options_resultHandlerSelector = mkSelector "requestAVAssetForVideo:options:resultHandler:"

