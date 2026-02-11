{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Generated bindings for @PHProjectChangeRequest@.
module ObjC.Photos.PHProjectChangeRequest
  ( PHProjectChangeRequest
  , IsPHProjectChangeRequest(..)
  , initWithProject
  , setKeyAsset
  , setProjectPreviewImage
  , removeAssets
  , title
  , setTitle
  , projectExtensionData
  , setProjectExtensionData
  , initWithProjectSelector
  , setKeyAssetSelector
  , setProjectPreviewImageSelector
  , removeAssetsSelector
  , titleSelector
  , setTitleSelector
  , projectExtensionDataSelector
  , setProjectExtensionDataSelector


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
import ObjC.AppKit.Internal.Classes
import ObjC.Foundation.Internal.Classes

-- | @- initWithProject:@
initWithProject :: (IsPHProjectChangeRequest phProjectChangeRequest, IsPHProject project) => phProjectChangeRequest -> project -> IO (Id PHProjectChangeRequest)
initWithProject phProjectChangeRequest  project =
withObjCPtr project $ \raw_project ->
    sendMsg phProjectChangeRequest (mkSelector "initWithProject:") (retPtr retVoid) [argPtr (castPtr raw_project :: Ptr ())] >>= ownedObject . castPtr

-- | Sets the key asset representing the project. Deprecated in macOS 10.14, please use -[PHProjectChangeRequest setProjectPreviewImage:] to provide a rendered preview instead.
--
-- ObjC selector: @- setKeyAsset:@
setKeyAsset :: (IsPHProjectChangeRequest phProjectChangeRequest, IsPHAsset keyAsset) => phProjectChangeRequest -> keyAsset -> IO ()
setKeyAsset phProjectChangeRequest  keyAsset =
withObjCPtr keyAsset $ \raw_keyAsset ->
    sendMsg phProjectChangeRequest (mkSelector "setKeyAsset:") retVoid [argPtr (castPtr raw_keyAsset :: Ptr ())]

-- | @- setProjectPreviewImage:@
setProjectPreviewImage :: (IsPHProjectChangeRequest phProjectChangeRequest, IsNSImage previewImage) => phProjectChangeRequest -> previewImage -> IO ()
setProjectPreviewImage phProjectChangeRequest  previewImage =
withObjCPtr previewImage $ \raw_previewImage ->
    sendMsg phProjectChangeRequest (mkSelector "setProjectPreviewImage:") retVoid [argPtr (castPtr raw_previewImage :: Ptr ())]

-- | Removes the specified assets from the project.
--
-- @assets@ — A collection of PHAsset objects to be removed from the project.
--
-- ObjC selector: @- removeAssets:@
removeAssets :: IsPHProjectChangeRequest phProjectChangeRequest => phProjectChangeRequest -> RawId -> IO ()
removeAssets phProjectChangeRequest  assets =
  sendMsg phProjectChangeRequest (mkSelector "removeAssets:") retVoid [argPtr (castPtr (unRawId assets) :: Ptr ())]

-- | @- title@
title :: IsPHProjectChangeRequest phProjectChangeRequest => phProjectChangeRequest -> IO (Id NSString)
title phProjectChangeRequest  =
  sendMsg phProjectChangeRequest (mkSelector "title") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- setTitle:@
setTitle :: (IsPHProjectChangeRequest phProjectChangeRequest, IsNSString value) => phProjectChangeRequest -> value -> IO ()
setTitle phProjectChangeRequest  value =
withObjCPtr value $ \raw_value ->
    sendMsg phProjectChangeRequest (mkSelector "setTitle:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- | The projectExtensionData property is intended for storage of compressed, project specific data only. Do not include things like rasterized images that can be locally cached in this data. The total size of stored data is limited to 5 MB. Attempting to store more data than allowed will result in an error.
--
-- ObjC selector: @- projectExtensionData@
projectExtensionData :: IsPHProjectChangeRequest phProjectChangeRequest => phProjectChangeRequest -> IO (Id NSData)
projectExtensionData phProjectChangeRequest  =
  sendMsg phProjectChangeRequest (mkSelector "projectExtensionData") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | The projectExtensionData property is intended for storage of compressed, project specific data only. Do not include things like rasterized images that can be locally cached in this data. The total size of stored data is limited to 5 MB. Attempting to store more data than allowed will result in an error.
--
-- ObjC selector: @- setProjectExtensionData:@
setProjectExtensionData :: (IsPHProjectChangeRequest phProjectChangeRequest, IsNSData value) => phProjectChangeRequest -> value -> IO ()
setProjectExtensionData phProjectChangeRequest  value =
withObjCPtr value $ \raw_value ->
    sendMsg phProjectChangeRequest (mkSelector "setProjectExtensionData:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @initWithProject:@
initWithProjectSelector :: Selector
initWithProjectSelector = mkSelector "initWithProject:"

-- | @Selector@ for @setKeyAsset:@
setKeyAssetSelector :: Selector
setKeyAssetSelector = mkSelector "setKeyAsset:"

-- | @Selector@ for @setProjectPreviewImage:@
setProjectPreviewImageSelector :: Selector
setProjectPreviewImageSelector = mkSelector "setProjectPreviewImage:"

-- | @Selector@ for @removeAssets:@
removeAssetsSelector :: Selector
removeAssetsSelector = mkSelector "removeAssets:"

-- | @Selector@ for @title@
titleSelector :: Selector
titleSelector = mkSelector "title"

-- | @Selector@ for @setTitle:@
setTitleSelector :: Selector
setTitleSelector = mkSelector "setTitle:"

-- | @Selector@ for @projectExtensionData@
projectExtensionDataSelector :: Selector
projectExtensionDataSelector = mkSelector "projectExtensionData"

-- | @Selector@ for @setProjectExtensionData:@
setProjectExtensionDataSelector :: Selector
setProjectExtensionDataSelector = mkSelector "setProjectExtensionData:"

