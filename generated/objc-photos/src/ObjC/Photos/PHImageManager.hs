{-# LANGUAGE PatternSynonyms #-}
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
  , defaultManagerSelector
  , requestImageDataForAsset_options_resultHandlerSelector
  , requestImageDataAndOrientationForAsset_options_resultHandlerSelector
  , cancelImageRequestSelector
  , requestPlayerItemForVideo_options_resultHandlerSelector
  , requestExportSessionForVideo_options_exportPreset_resultHandlerSelector
  , requestAVAssetForVideo_options_resultHandlerSelector

  -- * Enum types
  , PHImageContentMode(PHImageContentMode)
  , pattern PHImageContentModeAspectFit
  , pattern PHImageContentModeAspectFill
  , pattern PHImageContentModeDefault

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

-- | @+ defaultManager@
defaultManager :: IO (Id PHImageManager)
defaultManager  =
  do
    cls' <- getRequiredClass "PHImageManager"
    sendClassMsg cls' (mkSelector "defaultManager") (retPtr retVoid) [] >>= retainedObject . castPtr

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
requestImageDataForAsset_options_resultHandler phImageManager  asset options resultHandler =
withObjCPtr asset $ \raw_asset ->
  withObjCPtr options $ \raw_options ->
      sendMsg phImageManager (mkSelector "requestImageDataForAsset:options:resultHandler:") retCInt [argPtr (castPtr raw_asset :: Ptr ()), argPtr (castPtr raw_options :: Ptr ()), argPtr (castPtr resultHandler :: Ptr ())]

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
requestImageDataAndOrientationForAsset_options_resultHandler phImageManager  asset options resultHandler =
withObjCPtr asset $ \raw_asset ->
  withObjCPtr options $ \raw_options ->
      sendMsg phImageManager (mkSelector "requestImageDataAndOrientationForAsset:options:resultHandler:") retCInt [argPtr (castPtr raw_asset :: Ptr ()), argPtr (castPtr raw_options :: Ptr ()), argPtr (castPtr resultHandler :: Ptr ())]

-- | @- cancelImageRequest:@
cancelImageRequest :: IsPHImageManager phImageManager => phImageManager -> CInt -> IO ()
cancelImageRequest phImageManager  requestID =
  sendMsg phImageManager (mkSelector "cancelImageRequest:") retVoid [argCInt (fromIntegral requestID)]

-- | @- requestPlayerItemForVideo:options:resultHandler:@
requestPlayerItemForVideo_options_resultHandler :: (IsPHImageManager phImageManager, IsPHAsset asset, IsPHVideoRequestOptions options) => phImageManager -> asset -> options -> Ptr () -> IO CInt
requestPlayerItemForVideo_options_resultHandler phImageManager  asset options resultHandler =
withObjCPtr asset $ \raw_asset ->
  withObjCPtr options $ \raw_options ->
      sendMsg phImageManager (mkSelector "requestPlayerItemForVideo:options:resultHandler:") retCInt [argPtr (castPtr raw_asset :: Ptr ()), argPtr (castPtr raw_options :: Ptr ()), argPtr (castPtr resultHandler :: Ptr ())]

-- | @- requestExportSessionForVideo:options:exportPreset:resultHandler:@
requestExportSessionForVideo_options_exportPreset_resultHandler :: (IsPHImageManager phImageManager, IsPHAsset asset, IsPHVideoRequestOptions options, IsNSString exportPreset) => phImageManager -> asset -> options -> exportPreset -> Ptr () -> IO CInt
requestExportSessionForVideo_options_exportPreset_resultHandler phImageManager  asset options exportPreset resultHandler =
withObjCPtr asset $ \raw_asset ->
  withObjCPtr options $ \raw_options ->
    withObjCPtr exportPreset $ \raw_exportPreset ->
        sendMsg phImageManager (mkSelector "requestExportSessionForVideo:options:exportPreset:resultHandler:") retCInt [argPtr (castPtr raw_asset :: Ptr ()), argPtr (castPtr raw_options :: Ptr ()), argPtr (castPtr raw_exportPreset :: Ptr ()), argPtr (castPtr resultHandler :: Ptr ())]

-- | @- requestAVAssetForVideo:options:resultHandler:@
requestAVAssetForVideo_options_resultHandler :: (IsPHImageManager phImageManager, IsPHAsset asset, IsPHVideoRequestOptions options) => phImageManager -> asset -> options -> Ptr () -> IO CInt
requestAVAssetForVideo_options_resultHandler phImageManager  asset options resultHandler =
withObjCPtr asset $ \raw_asset ->
  withObjCPtr options $ \raw_options ->
      sendMsg phImageManager (mkSelector "requestAVAssetForVideo:options:resultHandler:") retCInt [argPtr (castPtr raw_asset :: Ptr ()), argPtr (castPtr raw_options :: Ptr ()), argPtr (castPtr resultHandler :: Ptr ())]

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @defaultManager@
defaultManagerSelector :: Selector
defaultManagerSelector = mkSelector "defaultManager"

-- | @Selector@ for @requestImageDataForAsset:options:resultHandler:@
requestImageDataForAsset_options_resultHandlerSelector :: Selector
requestImageDataForAsset_options_resultHandlerSelector = mkSelector "requestImageDataForAsset:options:resultHandler:"

-- | @Selector@ for @requestImageDataAndOrientationForAsset:options:resultHandler:@
requestImageDataAndOrientationForAsset_options_resultHandlerSelector :: Selector
requestImageDataAndOrientationForAsset_options_resultHandlerSelector = mkSelector "requestImageDataAndOrientationForAsset:options:resultHandler:"

-- | @Selector@ for @cancelImageRequest:@
cancelImageRequestSelector :: Selector
cancelImageRequestSelector = mkSelector "cancelImageRequest:"

-- | @Selector@ for @requestPlayerItemForVideo:options:resultHandler:@
requestPlayerItemForVideo_options_resultHandlerSelector :: Selector
requestPlayerItemForVideo_options_resultHandlerSelector = mkSelector "requestPlayerItemForVideo:options:resultHandler:"

-- | @Selector@ for @requestExportSessionForVideo:options:exportPreset:resultHandler:@
requestExportSessionForVideo_options_exportPreset_resultHandlerSelector :: Selector
requestExportSessionForVideo_options_exportPreset_resultHandlerSelector = mkSelector "requestExportSessionForVideo:options:exportPreset:resultHandler:"

-- | @Selector@ for @requestAVAssetForVideo:options:resultHandler:@
requestAVAssetForVideo_options_resultHandlerSelector :: Selector
requestAVAssetForVideo_options_resultHandlerSelector = mkSelector "requestAVAssetForVideo:options:resultHandler:"

