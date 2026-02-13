{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | A representation of a manifest that lists asset packs that are available to download.
--
-- This class applies only when you want to manage your asset packs manually. Don’t use this class if you want to opt in to automatic management of asset packs.
--
-- Generated bindings for @BAAssetPackManifest@.
module ObjC.BackgroundAssets.BAAssetPackManifest
  ( BAAssetPackManifest
  , IsBAAssetPackManifest(..)
  , init_
  , initWithContentsOfURL_applicationGroupIdentifier_error
  , initFromData_applicationGroupIdentifier_error
  , new
  , allDownloads
  , allDownloadsForContentRequest
  , assetPacks
  , allDownloadsForContentRequestSelector
  , allDownloadsSelector
  , assetPacksSelector
  , initFromData_applicationGroupIdentifier_errorSelector
  , initSelector
  , initWithContentsOfURL_applicationGroupIdentifier_errorSelector
  , newSelector

  -- * Enum types
  , BAContentRequest(BAContentRequest)
  , pattern BAContentRequestInstall
  , pattern BAContentRequestUpdate
  , pattern BAContentRequestPeriodic

  ) where

import Foreign.Ptr (Ptr, FunPtr)
import Foreign.C.Types

import ObjC.Runtime.Types
import ObjC.Runtime.Message (sendMessage, sendOwnedMessage, sendClassMessage, sendOwnedClassMessage)
import ObjC.Runtime.Selector (mkSelector)
import ObjC.Runtime.Class (getRequiredClass)

import ObjC.BackgroundAssets.Internal.Classes
import ObjC.BackgroundAssets.Internal.Enums
import ObjC.Foundation.Internal.Classes

-- | @- init@
init_ :: IsBAAssetPackManifest baAssetPackManifest => baAssetPackManifest -> IO (Id BAAssetPackManifest)
init_ baAssetPackManifest =
  sendOwnedMessage baAssetPackManifest initSelector

-- | Initializes a representation of a manifest in memory given a URL to the manifest’s representation as a JSON file on disk. - Parameters:   - URL: A URL to a local JSON file.   - applicationGroupIdentifier: The identifier of the application group in which to store unmanaged asset packs that are downloaded from the manifest.   - error: A pointer to an error that will be set if an error occurs.
--
-- ObjC selector: @- initWithContentsOfURL:applicationGroupIdentifier:error:@
initWithContentsOfURL_applicationGroupIdentifier_error :: (IsBAAssetPackManifest baAssetPackManifest, IsNSURL url, IsNSString applicationGroupIdentifier, IsNSError error_) => baAssetPackManifest -> url -> applicationGroupIdentifier -> error_ -> IO (Id BAAssetPackManifest)
initWithContentsOfURL_applicationGroupIdentifier_error baAssetPackManifest url applicationGroupIdentifier error_ =
  sendOwnedMessage baAssetPackManifest initWithContentsOfURL_applicationGroupIdentifier_errorSelector (toNSURL url) (toNSString applicationGroupIdentifier) (toNSError error_)

-- | Initializes a representation of a manifest in memory from JSON-encoded data. - Parameters:   - data: JSON-encoded data.   - applicationGroupIdentifier: The identifier of the application group in which to store unmanaged asset packs that are downloaded from the manifest.   - error: A pointer to an error that will be set if an error occurs.
--
-- ObjC selector: @- initFromData:applicationGroupIdentifier:error:@
initFromData_applicationGroupIdentifier_error :: (IsBAAssetPackManifest baAssetPackManifest, IsNSData data_, IsNSString applicationGroupIdentifier, IsNSError error_) => baAssetPackManifest -> data_ -> applicationGroupIdentifier -> error_ -> IO (Id BAAssetPackManifest)
initFromData_applicationGroupIdentifier_error baAssetPackManifest data_ applicationGroupIdentifier error_ =
  sendOwnedMessage baAssetPackManifest initFromData_applicationGroupIdentifier_errorSelector (toNSData data_) (toNSString applicationGroupIdentifier) (toNSError error_)

-- | @+ new@
new :: IO (Id BAAssetPackManifest)
new  =
  do
    cls' <- getRequiredClass "BAAssetPackManifest"
    sendOwnedClassMessage cls' newSelector

-- | Creates download objects for every asset pack in this manifest.
--
-- The returned download objects can be scheduled with the download manager. - Returns: A collection of download objects. - Remark: Use this method in your main app; use @-allDownloadsForContentRequest:@ in your downloader extension.
--
-- ObjC selector: @- allDownloads@
allDownloads :: IsBAAssetPackManifest baAssetPackManifest => baAssetPackManifest -> IO (Id NSSet)
allDownloads baAssetPackManifest =
  sendMessage baAssetPackManifest allDownloadsSelector

-- | Creates download objects for every asset pack in this manifest.
--
-- The returned download objects can be scheduled with the download manager. - Parameter contentRequest: The content request for the current extension invocation. - Returns: A collection of download objects. - Remark: Use this method in your downloader extension; use @-allDownloads@ instead in your main app.
--
-- ObjC selector: @- allDownloadsForContentRequest:@
allDownloadsForContentRequest :: IsBAAssetPackManifest baAssetPackManifest => baAssetPackManifest -> BAContentRequest -> IO (Id NSSet)
allDownloadsForContentRequest baAssetPackManifest contentRequest =
  sendMessage baAssetPackManifest allDownloadsForContentRequestSelector contentRequest

-- | The asset packs that are available to download.
--
-- ObjC selector: @- assetPacks@
assetPacks :: IsBAAssetPackManifest baAssetPackManifest => baAssetPackManifest -> IO (Id NSSet)
assetPacks baAssetPackManifest =
  sendMessage baAssetPackManifest assetPacksSelector

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @init@
initSelector :: Selector '[] (Id BAAssetPackManifest)
initSelector = mkSelector "init"

-- | @Selector@ for @initWithContentsOfURL:applicationGroupIdentifier:error:@
initWithContentsOfURL_applicationGroupIdentifier_errorSelector :: Selector '[Id NSURL, Id NSString, Id NSError] (Id BAAssetPackManifest)
initWithContentsOfURL_applicationGroupIdentifier_errorSelector = mkSelector "initWithContentsOfURL:applicationGroupIdentifier:error:"

-- | @Selector@ for @initFromData:applicationGroupIdentifier:error:@
initFromData_applicationGroupIdentifier_errorSelector :: Selector '[Id NSData, Id NSString, Id NSError] (Id BAAssetPackManifest)
initFromData_applicationGroupIdentifier_errorSelector = mkSelector "initFromData:applicationGroupIdentifier:error:"

-- | @Selector@ for @new@
newSelector :: Selector '[] (Id BAAssetPackManifest)
newSelector = mkSelector "new"

-- | @Selector@ for @allDownloads@
allDownloadsSelector :: Selector '[] (Id NSSet)
allDownloadsSelector = mkSelector "allDownloads"

-- | @Selector@ for @allDownloadsForContentRequest:@
allDownloadsForContentRequestSelector :: Selector '[BAContentRequest] (Id NSSet)
allDownloadsForContentRequestSelector = mkSelector "allDownloadsForContentRequest:"

-- | @Selector@ for @assetPacks@
assetPacksSelector :: Selector '[] (Id NSSet)
assetPacksSelector = mkSelector "assetPacks"

