{-# LANGUAGE DataKinds #-}
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
  , projectExtensionDataSelector
  , removeAssetsSelector
  , setKeyAssetSelector
  , setProjectExtensionDataSelector
  , setProjectPreviewImageSelector
  , setTitleSelector
  , titleSelector


  ) where

import Foreign.Ptr (Ptr, FunPtr)
import Foreign.C.Types

import ObjC.Runtime.Types
import ObjC.Runtime.Message (sendMessage, sendOwnedMessage, sendClassMessage, sendOwnedClassMessage)
import ObjC.Runtime.Selector (mkSelector)
import ObjC.Runtime.Class (getRequiredClass)

import ObjC.Photos.Internal.Classes
import ObjC.AppKit.Internal.Classes
import ObjC.Foundation.Internal.Classes

-- | @- initWithProject:@
initWithProject :: (IsPHProjectChangeRequest phProjectChangeRequest, IsPHProject project) => phProjectChangeRequest -> project -> IO (Id PHProjectChangeRequest)
initWithProject phProjectChangeRequest project =
  sendOwnedMessage phProjectChangeRequest initWithProjectSelector (toPHProject project)

-- | Sets the key asset representing the project. Deprecated in macOS 10.14, please use -[PHProjectChangeRequest setProjectPreviewImage:] to provide a rendered preview instead.
--
-- ObjC selector: @- setKeyAsset:@
setKeyAsset :: (IsPHProjectChangeRequest phProjectChangeRequest, IsPHAsset keyAsset) => phProjectChangeRequest -> keyAsset -> IO ()
setKeyAsset phProjectChangeRequest keyAsset =
  sendMessage phProjectChangeRequest setKeyAssetSelector (toPHAsset keyAsset)

-- | @- setProjectPreviewImage:@
setProjectPreviewImage :: (IsPHProjectChangeRequest phProjectChangeRequest, IsNSImage previewImage) => phProjectChangeRequest -> previewImage -> IO ()
setProjectPreviewImage phProjectChangeRequest previewImage =
  sendMessage phProjectChangeRequest setProjectPreviewImageSelector (toNSImage previewImage)

-- | Removes the specified assets from the project.
--
-- @assets@ — A collection of PHAsset objects to be removed from the project.
--
-- ObjC selector: @- removeAssets:@
removeAssets :: IsPHProjectChangeRequest phProjectChangeRequest => phProjectChangeRequest -> RawId -> IO ()
removeAssets phProjectChangeRequest assets =
  sendMessage phProjectChangeRequest removeAssetsSelector assets

-- | @- title@
title :: IsPHProjectChangeRequest phProjectChangeRequest => phProjectChangeRequest -> IO (Id NSString)
title phProjectChangeRequest =
  sendMessage phProjectChangeRequest titleSelector

-- | @- setTitle:@
setTitle :: (IsPHProjectChangeRequest phProjectChangeRequest, IsNSString value) => phProjectChangeRequest -> value -> IO ()
setTitle phProjectChangeRequest value =
  sendMessage phProjectChangeRequest setTitleSelector (toNSString value)

-- | The projectExtensionData property is intended for storage of compressed, project specific data only. Do not include things like rasterized images that can be locally cached in this data. The total size of stored data is limited to 5 MB. Attempting to store more data than allowed will result in an error.
--
-- ObjC selector: @- projectExtensionData@
projectExtensionData :: IsPHProjectChangeRequest phProjectChangeRequest => phProjectChangeRequest -> IO (Id NSData)
projectExtensionData phProjectChangeRequest =
  sendMessage phProjectChangeRequest projectExtensionDataSelector

-- | The projectExtensionData property is intended for storage of compressed, project specific data only. Do not include things like rasterized images that can be locally cached in this data. The total size of stored data is limited to 5 MB. Attempting to store more data than allowed will result in an error.
--
-- ObjC selector: @- setProjectExtensionData:@
setProjectExtensionData :: (IsPHProjectChangeRequest phProjectChangeRequest, IsNSData value) => phProjectChangeRequest -> value -> IO ()
setProjectExtensionData phProjectChangeRequest value =
  sendMessage phProjectChangeRequest setProjectExtensionDataSelector (toNSData value)

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @initWithProject:@
initWithProjectSelector :: Selector '[Id PHProject] (Id PHProjectChangeRequest)
initWithProjectSelector = mkSelector "initWithProject:"

-- | @Selector@ for @setKeyAsset:@
setKeyAssetSelector :: Selector '[Id PHAsset] ()
setKeyAssetSelector = mkSelector "setKeyAsset:"

-- | @Selector@ for @setProjectPreviewImage:@
setProjectPreviewImageSelector :: Selector '[Id NSImage] ()
setProjectPreviewImageSelector = mkSelector "setProjectPreviewImage:"

-- | @Selector@ for @removeAssets:@
removeAssetsSelector :: Selector '[RawId] ()
removeAssetsSelector = mkSelector "removeAssets:"

-- | @Selector@ for @title@
titleSelector :: Selector '[] (Id NSString)
titleSelector = mkSelector "title"

-- | @Selector@ for @setTitle:@
setTitleSelector :: Selector '[Id NSString] ()
setTitleSelector = mkSelector "setTitle:"

-- | @Selector@ for @projectExtensionData@
projectExtensionDataSelector :: Selector '[] (Id NSData)
projectExtensionDataSelector = mkSelector "projectExtensionData"

-- | @Selector@ for @setProjectExtensionData:@
setProjectExtensionDataSelector :: Selector '[Id NSData] ()
setProjectExtensionDataSelector = mkSelector "setProjectExtensionData:"

