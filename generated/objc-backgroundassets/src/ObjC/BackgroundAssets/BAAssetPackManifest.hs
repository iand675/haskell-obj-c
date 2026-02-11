{-# LANGUAGE PatternSynonyms #-}
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
  , initSelector
  , initWithContentsOfURL_applicationGroupIdentifier_errorSelector
  , initFromData_applicationGroupIdentifier_errorSelector
  , newSelector
  , allDownloadsSelector
  , allDownloadsForContentRequestSelector
  , assetPacksSelector

  -- * Enum types
  , BAContentRequest(BAContentRequest)
  , pattern BAContentRequestInstall
  , pattern BAContentRequestUpdate
  , pattern BAContentRequestPeriodic

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

import ObjC.BackgroundAssets.Internal.Classes
import ObjC.BackgroundAssets.Internal.Enums
import ObjC.Foundation.Internal.Classes

-- | @- init@
init_ :: IsBAAssetPackManifest baAssetPackManifest => baAssetPackManifest -> IO (Id BAAssetPackManifest)
init_ baAssetPackManifest  =
  sendMsg baAssetPackManifest (mkSelector "init") (retPtr retVoid) [] >>= ownedObject . castPtr

-- | Initializes a representation of a manifest in memory given a URL to the manifest’s representation as a JSON file on disk. - Parameters:   - URL: A URL to a local JSON file.   - applicationGroupIdentifier: The identifier of the application group in which to store unmanaged asset packs that are downloaded from the manifest.   - error: A pointer to an error that will be set if an error occurs.
--
-- ObjC selector: @- initWithContentsOfURL:applicationGroupIdentifier:error:@
initWithContentsOfURL_applicationGroupIdentifier_error :: (IsBAAssetPackManifest baAssetPackManifest, IsNSURL url, IsNSString applicationGroupIdentifier, IsNSError error_) => baAssetPackManifest -> url -> applicationGroupIdentifier -> error_ -> IO (Id BAAssetPackManifest)
initWithContentsOfURL_applicationGroupIdentifier_error baAssetPackManifest  url applicationGroupIdentifier error_ =
withObjCPtr url $ \raw_url ->
  withObjCPtr applicationGroupIdentifier $ \raw_applicationGroupIdentifier ->
    withObjCPtr error_ $ \raw_error_ ->
        sendMsg baAssetPackManifest (mkSelector "initWithContentsOfURL:applicationGroupIdentifier:error:") (retPtr retVoid) [argPtr (castPtr raw_url :: Ptr ()), argPtr (castPtr raw_applicationGroupIdentifier :: Ptr ()), argPtr (castPtr raw_error_ :: Ptr ())] >>= ownedObject . castPtr

-- | Initializes a representation of a manifest in memory from JSON-encoded data. - Parameters:   - data: JSON-encoded data.   - applicationGroupIdentifier: The identifier of the application group in which to store unmanaged asset packs that are downloaded from the manifest.   - error: A pointer to an error that will be set if an error occurs.
--
-- ObjC selector: @- initFromData:applicationGroupIdentifier:error:@
initFromData_applicationGroupIdentifier_error :: (IsBAAssetPackManifest baAssetPackManifest, IsNSData data_, IsNSString applicationGroupIdentifier, IsNSError error_) => baAssetPackManifest -> data_ -> applicationGroupIdentifier -> error_ -> IO (Id BAAssetPackManifest)
initFromData_applicationGroupIdentifier_error baAssetPackManifest  data_ applicationGroupIdentifier error_ =
withObjCPtr data_ $ \raw_data_ ->
  withObjCPtr applicationGroupIdentifier $ \raw_applicationGroupIdentifier ->
    withObjCPtr error_ $ \raw_error_ ->
        sendMsg baAssetPackManifest (mkSelector "initFromData:applicationGroupIdentifier:error:") (retPtr retVoid) [argPtr (castPtr raw_data_ :: Ptr ()), argPtr (castPtr raw_applicationGroupIdentifier :: Ptr ()), argPtr (castPtr raw_error_ :: Ptr ())] >>= ownedObject . castPtr

-- | @+ new@
new :: IO (Id BAAssetPackManifest)
new  =
  do
    cls' <- getRequiredClass "BAAssetPackManifest"
    sendClassMsg cls' (mkSelector "new") (retPtr retVoid) [] >>= ownedObject . castPtr

-- | Creates download objects for every asset pack in this manifest.
--
-- The returned download objects can be scheduled with the download manager. - Returns: A collection of download objects. - Remark: Use this method in your main app; use @-allDownloadsForContentRequest:@ in your downloader extension.
--
-- ObjC selector: @- allDownloads@
allDownloads :: IsBAAssetPackManifest baAssetPackManifest => baAssetPackManifest -> IO (Id NSSet)
allDownloads baAssetPackManifest  =
  sendMsg baAssetPackManifest (mkSelector "allDownloads") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | Creates download objects for every asset pack in this manifest.
--
-- The returned download objects can be scheduled with the download manager. - Parameter contentRequest: The content request for the current extension invocation. - Returns: A collection of download objects. - Remark: Use this method in your downloader extension; use @-allDownloads@ instead in your main app.
--
-- ObjC selector: @- allDownloadsForContentRequest:@
allDownloadsForContentRequest :: IsBAAssetPackManifest baAssetPackManifest => baAssetPackManifest -> BAContentRequest -> IO (Id NSSet)
allDownloadsForContentRequest baAssetPackManifest  contentRequest =
  sendMsg baAssetPackManifest (mkSelector "allDownloadsForContentRequest:") (retPtr retVoid) [argCLong (coerce contentRequest)] >>= retainedObject . castPtr

-- | The asset packs that are available to download.
--
-- ObjC selector: @- assetPacks@
assetPacks :: IsBAAssetPackManifest baAssetPackManifest => baAssetPackManifest -> IO (Id NSSet)
assetPacks baAssetPackManifest  =
  sendMsg baAssetPackManifest (mkSelector "assetPacks") (retPtr retVoid) [] >>= retainedObject . castPtr

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @init@
initSelector :: Selector
initSelector = mkSelector "init"

-- | @Selector@ for @initWithContentsOfURL:applicationGroupIdentifier:error:@
initWithContentsOfURL_applicationGroupIdentifier_errorSelector :: Selector
initWithContentsOfURL_applicationGroupIdentifier_errorSelector = mkSelector "initWithContentsOfURL:applicationGroupIdentifier:error:"

-- | @Selector@ for @initFromData:applicationGroupIdentifier:error:@
initFromData_applicationGroupIdentifier_errorSelector :: Selector
initFromData_applicationGroupIdentifier_errorSelector = mkSelector "initFromData:applicationGroupIdentifier:error:"

-- | @Selector@ for @new@
newSelector :: Selector
newSelector = mkSelector "new"

-- | @Selector@ for @allDownloads@
allDownloadsSelector :: Selector
allDownloadsSelector = mkSelector "allDownloads"

-- | @Selector@ for @allDownloadsForContentRequest:@
allDownloadsForContentRequestSelector :: Selector
allDownloadsForContentRequestSelector = mkSelector "allDownloadsForContentRequest:"

-- | @Selector@ for @assetPacks@
assetPacksSelector :: Selector
assetPacksSelector = mkSelector "assetPacks"

