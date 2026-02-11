{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}

-- | Internal module: all type tags, aliases, type classes, and
-- hierarchy instances for this framework.
--
-- Exists to break import cycles between per-class modules.
-- Import the per-class modules for the public API.
module ObjC.FileProvider.Internal.Classes (
    module ObjC.FileProvider.Internal.Classes,
    module ObjC.Foundation.Internal.Classes,
  ) where

import Data.Proxy (Proxy(..))
import ObjC.Runtime.Types
import ObjC.Runtime.Class (getRequiredClass)
import ObjC.Foundation.Internal.Classes

-- ---------- NSFileProviderDomain ----------

-- | File provider domain.
--
-- A file provider domain can be used to represent accounts or different locations exposed within a given file provider.
--
-- Domains can be registered to the system using @-[NSFileProviderMananger@ addDomain:completionHandler:]
--
-- By default, a file provider extension does not have any domain.
--
-- On the extension side, a separate instance of NSFileProviderExtension will be created for each @NSFileProviderDomain@ registered.  In that case, the @NSFileProviderExtension.domain@ properties will indicate which domain the NSFileProviderExtension belongs to (or nil if none).
--
-- All the files on disk belonging to the same domain must be grouped inside a common directory. That directory path is indicated by the @pathRelativeToDocumentStorage@ property.
-- 
-- Phantom type for @NSFileProviderDomain@.
data NSFileProviderDomain

instance IsObjCObject (Id NSFileProviderDomain) where
  toRawId = idToRawId
  withObjCPtr = withIdObjCPtr
  staticClass _ = getRequiredClass "NSFileProviderDomain"

class IsNSObject a => IsNSFileProviderDomain a where
  toNSFileProviderDomain :: a -> Id NSFileProviderDomain

instance IsNSFileProviderDomain (Id NSFileProviderDomain) where
  toNSFileProviderDomain = unsafeCastId

instance IsNSObject (Id NSFileProviderDomain) where
  toNSObject = unsafeCastId

-- ---------- NSFileProviderDomainVersion ----------

-- | File provider domain version.
--
-- This object can be used by the @NSFileProviderReplicatedExtension@ to describe the current version of the domain. This object is immutable and can safely be used as a key in a dictionary.
-- 
-- Phantom type for @NSFileProviderDomainVersion@.
data NSFileProviderDomainVersion

instance IsObjCObject (Id NSFileProviderDomainVersion) where
  toRawId = idToRawId
  withObjCPtr = withIdObjCPtr
  staticClass _ = getRequiredClass "NSFileProviderDomainVersion"

class IsNSObject a => IsNSFileProviderDomainVersion a where
  toNSFileProviderDomainVersion :: a -> Id NSFileProviderDomainVersion

instance IsNSFileProviderDomainVersion (Id NSFileProviderDomainVersion) where
  toNSFileProviderDomainVersion = unsafeCastId

instance IsNSObject (Id NSFileProviderDomainVersion) where
  toNSObject = unsafeCastId

-- ---------- NSFileProviderExtension ----------

-- | Phantom type for @NSFileProviderExtension@.
data NSFileProviderExtension

instance IsObjCObject (Id NSFileProviderExtension) where
  toRawId = idToRawId
  withObjCPtr = withIdObjCPtr
  staticClass _ = getRequiredClass "NSFileProviderExtension"

class IsNSObject a => IsNSFileProviderExtension a where
  toNSFileProviderExtension :: a -> Id NSFileProviderExtension

instance IsNSFileProviderExtension (Id NSFileProviderExtension) where
  toNSFileProviderExtension = unsafeCastId

instance IsNSObject (Id NSFileProviderExtension) where
  toNSObject = unsafeCastId

-- ---------- NSFileProviderItemVersion ----------

-- | Phantom type for @NSFileProviderItemVersion@.
data NSFileProviderItemVersion

instance IsObjCObject (Id NSFileProviderItemVersion) where
  toRawId = idToRawId
  withObjCPtr = withIdObjCPtr
  staticClass _ = getRequiredClass "NSFileProviderItemVersion"

class IsNSObject a => IsNSFileProviderItemVersion a where
  toNSFileProviderItemVersion :: a -> Id NSFileProviderItemVersion

instance IsNSFileProviderItemVersion (Id NSFileProviderItemVersion) where
  toNSFileProviderItemVersion = unsafeCastId

instance IsNSObject (Id NSFileProviderItemVersion) where
  toNSObject = unsafeCastId

-- ---------- NSFileProviderKnownFolderLocation ----------

-- | Specify the location of a known folder in the replicated tree.
-- 
-- Phantom type for @NSFileProviderKnownFolderLocation@.
data NSFileProviderKnownFolderLocation

instance IsObjCObject (Id NSFileProviderKnownFolderLocation) where
  toRawId = idToRawId
  withObjCPtr = withIdObjCPtr
  staticClass _ = getRequiredClass "NSFileProviderKnownFolderLocation"

class IsNSObject a => IsNSFileProviderKnownFolderLocation a where
  toNSFileProviderKnownFolderLocation :: a -> Id NSFileProviderKnownFolderLocation

instance IsNSFileProviderKnownFolderLocation (Id NSFileProviderKnownFolderLocation) where
  toNSFileProviderKnownFolderLocation = unsafeCastId

instance IsNSObject (Id NSFileProviderKnownFolderLocation) where
  toNSObject = unsafeCastId

-- ---------- NSFileProviderKnownFolderLocations ----------

-- | Specify the locations at which known folders should be synced in the replicated tree.
--
-- Desktop and Documents candidate items need to have the same parent folder.
-- 
-- Phantom type for @NSFileProviderKnownFolderLocations@.
data NSFileProviderKnownFolderLocations

instance IsObjCObject (Id NSFileProviderKnownFolderLocations) where
  toRawId = idToRawId
  withObjCPtr = withIdObjCPtr
  staticClass _ = getRequiredClass "NSFileProviderKnownFolderLocations"

class IsNSObject a => IsNSFileProviderKnownFolderLocations a where
  toNSFileProviderKnownFolderLocations :: a -> Id NSFileProviderKnownFolderLocations

instance IsNSFileProviderKnownFolderLocations (Id NSFileProviderKnownFolderLocations) where
  toNSFileProviderKnownFolderLocations = unsafeCastId

instance IsNSObject (Id NSFileProviderKnownFolderLocations) where
  toNSObject = unsafeCastId

-- ---------- NSFileProviderManager ----------

-- | The file provider manager allows you to communicate with the file provider framework from both the extension and related processes.
--
-- NSFileProviderManager can be used from the following processes: - the extension - the main app containing the extension - sibling extensions to the extension - executables contained in the main app bundle (on macOS only)
--
-- Executables contained in the main app bundle need to have a bundle identifier that is prefixed by the bundle identifier of the main app (note that this is generally required for extensions). They must also have access to the document group defined for the provider (via its @NSExtensionFileProviderDocumentGroup@ key).
--
-- The file provider framework will invoke your file provider extension in response to those calls if appropriate.
--
-- The class also provides methods to manage provider domains. Each domain has a corresponding manager.
-- 
-- Phantom type for @NSFileProviderManager@.
data NSFileProviderManager

instance IsObjCObject (Id NSFileProviderManager) where
  toRawId = idToRawId
  withObjCPtr = withIdObjCPtr
  staticClass _ = getRequiredClass "NSFileProviderManager"

class IsNSObject a => IsNSFileProviderManager a where
  toNSFileProviderManager :: a -> Id NSFileProviderManager

instance IsNSFileProviderManager (Id NSFileProviderManager) where
  toNSFileProviderManager = unsafeCastId

instance IsNSObject (Id NSFileProviderManager) where
  toNSObject = unsafeCastId

-- ---------- NSFileProviderRequest ----------

-- | Phantom type for @NSFileProviderRequest@.
data NSFileProviderRequest

instance IsObjCObject (Id NSFileProviderRequest) where
  toRawId = idToRawId
  withObjCPtr = withIdObjCPtr
  staticClass _ = getRequiredClass "NSFileProviderRequest"

class IsNSObject a => IsNSFileProviderRequest a where
  toNSFileProviderRequest :: a -> Id NSFileProviderRequest

instance IsNSFileProviderRequest (Id NSFileProviderRequest) where
  toNSFileProviderRequest = unsafeCastId

instance IsNSObject (Id NSFileProviderRequest) where
  toNSObject = unsafeCastId

-- ---------- NSFileProviderStringSearchRequest ----------

-- | Phantom type for @NSFileProviderStringSearchRequest@.
data NSFileProviderStringSearchRequest

instance IsObjCObject (Id NSFileProviderStringSearchRequest) where
  toRawId = idToRawId
  withObjCPtr = withIdObjCPtr
  staticClass _ = getRequiredClass "NSFileProviderStringSearchRequest"

class IsNSObject a => IsNSFileProviderStringSearchRequest a where
  toNSFileProviderStringSearchRequest :: a -> Id NSFileProviderStringSearchRequest

instance IsNSFileProviderStringSearchRequest (Id NSFileProviderStringSearchRequest) where
  toNSFileProviderStringSearchRequest = unsafeCastId

instance IsNSObject (Id NSFileProviderStringSearchRequest) where
  toNSObject = unsafeCastId
