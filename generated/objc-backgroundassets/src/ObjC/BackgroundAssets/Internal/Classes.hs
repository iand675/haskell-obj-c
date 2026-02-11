{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}

-- | Internal module: all type tags, aliases, type classes, and
-- hierarchy instances for this framework.
--
-- Exists to break import cycles between per-class modules.
-- Import the per-class modules for the public API.
module ObjC.BackgroundAssets.Internal.Classes (
    module ObjC.BackgroundAssets.Internal.Classes,
    module ObjC.Foundation.Internal.Classes,
  ) where

import Data.Proxy (Proxy(..))
import ObjC.Runtime.Types
import ObjC.Runtime.Class (getRequiredClass)
import ObjC.Foundation.Internal.Classes

-- ---------- BAAppExtensionInfo ----------

-- | Phantom type for @BAAppExtensionInfo@.
data BAAppExtensionInfo

instance IsObjCObject (Id BAAppExtensionInfo) where
  toRawId = idToRawId
  withObjCPtr = withIdObjCPtr
  staticClass _ = getRequiredClass "BAAppExtensionInfo"

class IsNSObject a => IsBAAppExtensionInfo a where
  toBAAppExtensionInfo :: a -> Id BAAppExtensionInfo

instance IsBAAppExtensionInfo (Id BAAppExtensionInfo) where
  toBAAppExtensionInfo = unsafeCastId

instance IsNSObject (Id BAAppExtensionInfo) where
  toNSObject = unsafeCastId

-- ---------- BAAssetPack ----------

-- | An archive of assets that the system downloads together.
--
-- An instance of this class can be invalidated when the asset pack that it represents is updated on the server.
-- 
-- Phantom type for @BAAssetPack@.
data BAAssetPack

instance IsObjCObject (Id BAAssetPack) where
  toRawId = idToRawId
  withObjCPtr = withIdObjCPtr
  staticClass _ = getRequiredClass "BAAssetPack"

class IsNSObject a => IsBAAssetPack a where
  toBAAssetPack :: a -> Id BAAssetPack

instance IsBAAssetPack (Id BAAssetPack) where
  toBAAssetPack = unsafeCastId

instance IsNSObject (Id BAAssetPack) where
  toNSObject = unsafeCastId

-- ---------- BAAssetPackManager ----------

-- | A class that manages asset packs.
--
-- The first time that your code refers to the shared manager, Background Assets considers that your app is opting into automatic system management of your asset packs. - Important: When using the asset-pack manager, make sure that you also adopt the corresponding managed extension protocol. For apps that use Apple hosting, the corresponding protocol is @SKDownloaderExtension@ from StoreKit. For other apps, the corresponding protocol is ``BAManagedDownloaderExtension``. Not adopting the right protocol is a programmer error.
-- 
-- Phantom type for @BAAssetPackManager@.
data BAAssetPackManager

instance IsObjCObject (Id BAAssetPackManager) where
  toRawId = idToRawId
  withObjCPtr = withIdObjCPtr
  staticClass _ = getRequiredClass "BAAssetPackManager"

class IsNSObject a => IsBAAssetPackManager a where
  toBAAssetPackManager :: a -> Id BAAssetPackManager

instance IsBAAssetPackManager (Id BAAssetPackManager) where
  toBAAssetPackManager = unsafeCastId

instance IsNSObject (Id BAAssetPackManager) where
  toNSObject = unsafeCastId

-- ---------- BAAssetPackManifest ----------

-- | A representation of a manifest that lists asset packs that are available to download.
--
-- This class applies only when you want to manage your asset packs manually. Don’t use this class if you want to opt in to automatic management of asset packs.
-- 
-- Phantom type for @BAAssetPackManifest@.
data BAAssetPackManifest

instance IsObjCObject (Id BAAssetPackManifest) where
  toRawId = idToRawId
  withObjCPtr = withIdObjCPtr
  staticClass _ = getRequiredClass "BAAssetPackManifest"

class IsNSObject a => IsBAAssetPackManifest a where
  toBAAssetPackManifest :: a -> Id BAAssetPackManifest

instance IsBAAssetPackManifest (Id BAAssetPackManifest) where
  toBAAssetPackManifest = unsafeCastId

instance IsNSObject (Id BAAssetPackManifest) where
  toNSObject = unsafeCastId

-- ---------- BADownload ----------

-- | Phantom type for @BADownload@.
data BADownload

instance IsObjCObject (Id BADownload) where
  toRawId = idToRawId
  withObjCPtr = withIdObjCPtr
  staticClass _ = getRequiredClass "BADownload"

class IsNSObject a => IsBADownload a where
  toBADownload :: a -> Id BADownload

instance IsBADownload (Id BADownload) where
  toBADownload = unsafeCastId

instance IsNSObject (Id BADownload) where
  toNSObject = unsafeCastId

-- ---------- BADownloadManager ----------

-- | Phantom type for @BADownloadManager@.
data BADownloadManager

instance IsObjCObject (Id BADownloadManager) where
  toRawId = idToRawId
  withObjCPtr = withIdObjCPtr
  staticClass _ = getRequiredClass "BADownloadManager"

class IsNSObject a => IsBADownloadManager a where
  toBADownloadManager :: a -> Id BADownloadManager

instance IsBADownloadManager (Id BADownloadManager) where
  toBADownloadManager = unsafeCastId

instance IsNSObject (Id BADownloadManager) where
  toNSObject = unsafeCastId

-- ---------- BAURLDownload ----------

-- | Phantom type for @BAURLDownload@.
data BAURLDownload

instance IsObjCObject (Id BAURLDownload) where
  toRawId = idToRawId
  withObjCPtr = withIdObjCPtr
  staticClass _ = getRequiredClass "BAURLDownload"

class IsBADownload a => IsBAURLDownload a where
  toBAURLDownload :: a -> Id BAURLDownload

instance IsBAURLDownload (Id BAURLDownload) where
  toBAURLDownload = unsafeCastId

instance IsBADownload (Id BAURLDownload) where
  toBADownload = unsafeCastId

instance IsNSObject (Id BAURLDownload) where
  toNSObject = unsafeCastId
