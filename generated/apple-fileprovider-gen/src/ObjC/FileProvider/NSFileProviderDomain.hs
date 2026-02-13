{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

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
-- Generated bindings for @NSFileProviderDomain@.
module ObjC.FileProvider.NSFileProviderDomain
  ( NSFileProviderDomain
  , IsNSFileProviderDomain(..)
  , initWithIdentifier_displayName_pathRelativeToDocumentStorage
  , initWithIdentifier_displayName
  , initWithDisplayName_userInfo_volumeURL
  , identifier
  , displayName
  , pathRelativeToDocumentStorage
  , disconnected
  , userEnabled
  , hidden
  , setHidden
  , replicated
  , testingModes
  , setTestingModes
  , backingStoreIdentity
  , supportsSyncingTrash
  , setSupportsSyncingTrash
  , volumeUUID
  , userInfo
  , setUserInfo
  , replicatedKnownFolders
  , supportedKnownFolders
  , setSupportedKnownFolders
  , supportsStringSearchRequest
  , setSupportsStringSearchRequest
  , backingStoreIdentitySelector
  , disconnectedSelector
  , displayNameSelector
  , hiddenSelector
  , identifierSelector
  , initWithDisplayName_userInfo_volumeURLSelector
  , initWithIdentifier_displayNameSelector
  , initWithIdentifier_displayName_pathRelativeToDocumentStorageSelector
  , pathRelativeToDocumentStorageSelector
  , replicatedKnownFoldersSelector
  , replicatedSelector
  , setHiddenSelector
  , setSupportedKnownFoldersSelector
  , setSupportsStringSearchRequestSelector
  , setSupportsSyncingTrashSelector
  , setTestingModesSelector
  , setUserInfoSelector
  , supportedKnownFoldersSelector
  , supportsStringSearchRequestSelector
  , supportsSyncingTrashSelector
  , testingModesSelector
  , userEnabledSelector
  , userInfoSelector
  , volumeUUIDSelector

  -- * Enum types
  , NSFileProviderDomainTestingModes(NSFileProviderDomainTestingModes)
  , pattern NSFileProviderDomainTestingModeAlwaysEnabled
  , pattern NSFileProviderDomainTestingModeInteractive
  , NSFileProviderKnownFolders(NSFileProviderKnownFolders)
  , pattern NSFileProviderDesktop
  , pattern NSFileProviderDocuments

  ) where

import Foreign.Ptr (Ptr, FunPtr)
import Foreign.C.Types

import ObjC.Runtime.Types
import ObjC.Runtime.Message (sendMessage, sendOwnedMessage, sendClassMessage, sendOwnedClassMessage)
import ObjC.Runtime.Selector (mkSelector)
import ObjC.Runtime.Class (getRequiredClass)

import ObjC.FileProvider.Internal.Classes
import ObjC.FileProvider.Internal.Enums
import ObjC.Foundation.Internal.Classes

-- | Initialize a new non-replicated NSFileProviderDomain
--
-- The extension will be implementing NSFileProviderExtension.
--
-- The file provider extension implementation can pick any @identifier@ as it sees fit to identify the group of items. The identifier must not contain any characters from this set: [/:]
--
-- @displayName@ — a user visible string representing the group of items the file provider extension is using.
--
-- @pathRelativeToDocumentStorage@ — a path relative to @NSFileProviderExtension.documentStorageURL.@
--
-- ObjC selector: @- initWithIdentifier:displayName:pathRelativeToDocumentStorage:@
initWithIdentifier_displayName_pathRelativeToDocumentStorage :: (IsNSFileProviderDomain nsFileProviderDomain, IsNSString identifier, IsNSString displayName, IsNSString pathRelativeToDocumentStorage) => nsFileProviderDomain -> identifier -> displayName -> pathRelativeToDocumentStorage -> IO (Id NSFileProviderDomain)
initWithIdentifier_displayName_pathRelativeToDocumentStorage nsFileProviderDomain identifier displayName pathRelativeToDocumentStorage =
  sendOwnedMessage nsFileProviderDomain initWithIdentifier_displayName_pathRelativeToDocumentStorageSelector (toNSString identifier) (toNSString displayName) (toNSString pathRelativeToDocumentStorage)

-- | Initialize a new replicated NSFileProviderDomain
--
-- The extension will be implementing NSFileProviderReplicatedExtension.
--
-- The file provider extension implementation can pick any @identifier@ as it sees fit to identify the group of items. The identifier must not contain any characters from this set: [/:]
--
-- In order to migrate a non-replicated domain to a replicated one, implementers have to make sure that they do not use the default domain, and then call +[NSFileProviderManager addDomain:completionHandler:] using the NSFileProviderDomain object returned by that init method.
--
-- A domain with a specific identifier can be added multiple times; subsequent adds will update the properties of the existing domain. If a replicated domain is added "on top" of a non-replicated domain, the domain will be migrated to be replicated; existing bookmarks will remain valid, but the (externally visible) location of items will change to reflect the replicated location.
--
-- It is not possible to migrate the default domain in this manner (since the default domain can not be added). It is recommended to migrate usage of the default domain to a domain with an explicit identifier instead.
--
-- @displayName@ — a user visible string representing the group of items the file provider extension is using.
--
-- ObjC selector: @- initWithIdentifier:displayName:@
initWithIdentifier_displayName :: (IsNSFileProviderDomain nsFileProviderDomain, IsNSString identifier, IsNSString displayName) => nsFileProviderDomain -> identifier -> displayName -> IO (Id NSFileProviderDomain)
initWithIdentifier_displayName nsFileProviderDomain identifier displayName =
  sendOwnedMessage nsFileProviderDomain initWithIdentifier_displayNameSelector (toNSString identifier) (toNSString displayName)

-- | Initialize a new replicated NSFileProviderDomain on a specific volume.
--
-- If a volumeURL is specified, and that volume is eligible, the domain will be located on this volume. The URL is used to designate a volume but doesn't influence where on this volume is the domain going to be stored.
--
-- In order to avoid domainID collisions between volumes, the NSFileProviderDomainIdentifier of external domains are generated randomly by FileProvider. The provider should therefore use the userInfo to associate all necessary information to map the created object to the corresponding account. The userInfo will be persisted on the volume where the domain was created. If that is an external volume, the userInfo can be used on other devices to assist in setting up the domain on those devices. See the@NSFileProviderExternalVolumeHandling@ protocol for more details.
--
-- ObjC selector: @- initWithDisplayName:userInfo:volumeURL:@
initWithDisplayName_userInfo_volumeURL :: (IsNSFileProviderDomain nsFileProviderDomain, IsNSString displayName, IsNSDictionary userInfo, IsNSURL volumeURL) => nsFileProviderDomain -> displayName -> userInfo -> volumeURL -> IO (Id NSFileProviderDomain)
initWithDisplayName_userInfo_volumeURL nsFileProviderDomain displayName userInfo volumeURL =
  sendOwnedMessage nsFileProviderDomain initWithDisplayName_userInfo_volumeURLSelector (toNSString displayName) (toNSDictionary userInfo) (toNSURL volumeURL)

-- | The identifier - as provided by the file provider extension.
--
-- ObjC selector: @- identifier@
identifier :: IsNSFileProviderDomain nsFileProviderDomain => nsFileProviderDomain -> IO (Id NSString)
identifier nsFileProviderDomain =
  sendMessage nsFileProviderDomain identifierSelector

-- | The display name shown by the system to represent this domain.
--
-- ObjC selector: @- displayName@
displayName :: IsNSFileProviderDomain nsFileProviderDomain => nsFileProviderDomain -> IO (Id NSString)
displayName nsFileProviderDomain =
  sendMessage nsFileProviderDomain displayNameSelector

-- | The path relative to the document storage of the file provider extension. Files belonging to this domains should be stored under this path.
--
-- ObjC selector: @- pathRelativeToDocumentStorage@
pathRelativeToDocumentStorage :: IsNSFileProviderDomain nsFileProviderDomain => nsFileProviderDomain -> IO (Id NSString)
pathRelativeToDocumentStorage nsFileProviderDomain =
  sendMessage nsFileProviderDomain pathRelativeToDocumentStorageSelector

-- | If set, the domain is present, but disconnected from its extension. In this state, the user continues to be able to browse the domain's contents, but the extension doesn't receive updates on modifications to the files, nor is it consulted to update folder's contents.
--
-- The disconnected state can be modified on an existing domain via the disconnectWithReason method on NSFileProviderManager.
--
-- ObjC selector: @- disconnected@
disconnected :: IsNSFileProviderDomain nsFileProviderDomain => nsFileProviderDomain -> IO Bool
disconnected nsFileProviderDomain =
  sendMessage nsFileProviderDomain disconnectedSelector

-- | If user has disabled this domain from Files.app on iOS or System Settings on macOS, this will be set to NO.
--
-- ObjC selector: @- userEnabled@
userEnabled :: IsNSFileProviderDomain nsFileProviderDomain => nsFileProviderDomain -> IO Bool
userEnabled nsFileProviderDomain =
  sendMessage nsFileProviderDomain userEnabledSelector

-- | If this domain is not user visible.
--
-- Typically, this can be used for dry-run migration. The files are still on disk though.
--
-- ObjC selector: @- hidden@
hidden :: IsNSFileProviderDomain nsFileProviderDomain => nsFileProviderDomain -> IO Bool
hidden nsFileProviderDomain =
  sendMessage nsFileProviderDomain hiddenSelector

-- | If this domain is not user visible.
--
-- Typically, this can be used for dry-run migration. The files are still on disk though.
--
-- ObjC selector: @- setHidden:@
setHidden :: IsNSFileProviderDomain nsFileProviderDomain => nsFileProviderDomain -> Bool -> IO ()
setHidden nsFileProviderDomain value =
  sendMessage nsFileProviderDomain setHiddenSelector value

-- | If the domain is a replicated domain.
--
-- If set to YES, it means the domain is replicated. By default, on macOS, the value will always be YES.
--
-- On iOS, it will depend on the way the NSFileProviderDomain object is contructed. Calling -[NSFileProviderDomain initWithIdentifier:displayName:] will initialize a replicated domain. -[NSFileProviderDomain initWithIdentifier:displayName:pathRelativeToDocumentStorage:] will initialize a non-replicated domain.
--
-- To know whether a domain is replicated or not, users are advised to rely on the output of +[NSFileProviderManager getDomainsForProviderIdentifier:completionHandler:]
--
-- ObjC selector: @- replicated@
replicated :: IsNSFileProviderDomain nsFileProviderDomain => nsFileProviderDomain -> IO Bool
replicated nsFileProviderDomain =
  sendMessage nsFileProviderDomain replicatedSelector

-- | Testing modes.
--
-- Testing modes are exposed as a means for the provider to have more control over the system in a testing environment. Enabling a testing mode alters the behavior of the system and enables some APIs for that mode.
--
-- A process must have the com.apple.developer.fileprovider.testing-mode entitlement in order to configure a domain with non-empty testing modes.
--
-- ObjC selector: @- testingModes@
testingModes :: IsNSFileProviderDomain nsFileProviderDomain => nsFileProviderDomain -> IO NSFileProviderDomainTestingModes
testingModes nsFileProviderDomain =
  sendMessage nsFileProviderDomain testingModesSelector

-- | Testing modes.
--
-- Testing modes are exposed as a means for the provider to have more control over the system in a testing environment. Enabling a testing mode alters the behavior of the system and enables some APIs for that mode.
--
-- A process must have the com.apple.developer.fileprovider.testing-mode entitlement in order to configure a domain with non-empty testing modes.
--
-- ObjC selector: @- setTestingModes:@
setTestingModes :: IsNSFileProviderDomain nsFileProviderDomain => nsFileProviderDomain -> NSFileProviderDomainTestingModes -> IO ()
setTestingModes nsFileProviderDomain value =
  sendMessage nsFileProviderDomain setTestingModesSelector value

-- | Identity of the backing store of the domain on the system.
--
-- This property only applies for extensions that implement NSFileProviderReplicatedExtension.
--
-- This provides an identifier that uniquely identifies the backing store used by the system for the domain. When this identifier has changed, the system has dropped its backing store and is building a new one.
--
-- The system may decide to rebuild its backing store if it got corrupted. The backing store can also be rebuilt as a response to the provider calling @-[NSFileProviderManager reimportItemsBelowItemWithIdentifier:completionHandler:]@. It is guaranteed that calling reimport on the root item will cause the backing store to be rebuilt, but the system can also decide to do so when reimport is called on other items.
--
-- When rebuilding the backing store, the system will invalidate any extension instance associated to that domain. As a consequence, the identity of the backing store associated with that domain is guaranteed to be stable for the lifetime of the NSFileProviderReplicatedExtension instance.
--
-- ObjC selector: @- backingStoreIdentity@
backingStoreIdentity :: IsNSFileProviderDomain nsFileProviderDomain => nsFileProviderDomain -> IO (Id NSData)
backingStoreIdentity nsFileProviderDomain =
  sendMessage nsFileProviderDomain backingStoreIdentitySelector

-- | Whether the domain supports syncing the trash.
--
-- The system supports syncing a trash folder (NSFileProviderTrashContainerItemIdentifier) to the extension. On iOS, this is surfaced to the user as "Recently Deleted" in the Files app. On macOS, this is surfaced to the user as the Trash in Finder.
--
-- If the domain is configured with supportsSyncingTrash=YES, the system will reparent trashed files (which were located in the extension's domain) to NSFileProviderTrashContainerItemIdentifier. If the domain is configured with supportsSyncingTrash=NO, the system will decide how to handle the trashing operation (not guaranteed by API contract).
--
-- This property is only applicable for NSFileProviderReplicatedExtension-based domains.
--
-- This property defaults to YES.
--
-- ObjC selector: @- supportsSyncingTrash@
supportsSyncingTrash :: IsNSFileProviderDomain nsFileProviderDomain => nsFileProviderDomain -> IO Bool
supportsSyncingTrash nsFileProviderDomain =
  sendMessage nsFileProviderDomain supportsSyncingTrashSelector

-- | Whether the domain supports syncing the trash.
--
-- The system supports syncing a trash folder (NSFileProviderTrashContainerItemIdentifier) to the extension. On iOS, this is surfaced to the user as "Recently Deleted" in the Files app. On macOS, this is surfaced to the user as the Trash in Finder.
--
-- If the domain is configured with supportsSyncingTrash=YES, the system will reparent trashed files (which were located in the extension's domain) to NSFileProviderTrashContainerItemIdentifier. If the domain is configured with supportsSyncingTrash=NO, the system will decide how to handle the trashing operation (not guaranteed by API contract).
--
-- This property is only applicable for NSFileProviderReplicatedExtension-based domains.
--
-- This property defaults to YES.
--
-- ObjC selector: @- setSupportsSyncingTrash:@
setSupportsSyncingTrash :: IsNSFileProviderDomain nsFileProviderDomain => nsFileProviderDomain -> Bool -> IO ()
setSupportsSyncingTrash nsFileProviderDomain value =
  sendMessage nsFileProviderDomain setSupportsSyncingTrashSelector value

-- | @- volumeUUID@
volumeUUID :: IsNSFileProviderDomain nsFileProviderDomain => nsFileProviderDomain -> IO (Id NSUUID)
volumeUUID nsFileProviderDomain =
  sendMessage nsFileProviderDomain volumeUUIDSelector

-- | A dictionary set by the client app. Keys must be strings, values must be [String, Number, Date, Data]
--
-- ObjC selector: @- userInfo@
userInfo :: IsNSFileProviderDomain nsFileProviderDomain => nsFileProviderDomain -> IO (Id NSDictionary)
userInfo nsFileProviderDomain =
  sendMessage nsFileProviderDomain userInfoSelector

-- | A dictionary set by the client app. Keys must be strings, values must be [String, Number, Date, Data]
--
-- ObjC selector: @- setUserInfo:@
setUserInfo :: (IsNSFileProviderDomain nsFileProviderDomain, IsNSDictionary value) => nsFileProviderDomain -> value -> IO ()
setUserInfo nsFileProviderDomain value =
  sendMessage nsFileProviderDomain setUserInfoSelector (toNSDictionary value)

-- | List of known folders that are currently replicated by this domain.
--
-- ObjC selector: @- replicatedKnownFolders@
replicatedKnownFolders :: IsNSFileProviderDomain nsFileProviderDomain => nsFileProviderDomain -> IO NSFileProviderKnownFolders
replicatedKnownFolders nsFileProviderDomain =
  sendMessage nsFileProviderDomain replicatedKnownFoldersSelector

-- | List known folders that can be replicated by this domain.
--
-- ObjC selector: @- supportedKnownFolders@
supportedKnownFolders :: IsNSFileProviderDomain nsFileProviderDomain => nsFileProviderDomain -> IO NSFileProviderKnownFolders
supportedKnownFolders nsFileProviderDomain =
  sendMessage nsFileProviderDomain supportedKnownFoldersSelector

-- | List known folders that can be replicated by this domain.
--
-- ObjC selector: @- setSupportedKnownFolders:@
setSupportedKnownFolders :: IsNSFileProviderDomain nsFileProviderDomain => nsFileProviderDomain -> NSFileProviderKnownFolders -> IO ()
setSupportedKnownFolders nsFileProviderDomain value =
  sendMessage nsFileProviderDomain setSupportedKnownFoldersSelector value

-- | Whether the system should use this domain's  @NSFileProviderSearching@ implementation to support  search experiences.
--
-- Defaults to NO.
--
-- ObjC selector: @- supportsStringSearchRequest@
supportsStringSearchRequest :: IsNSFileProviderDomain nsFileProviderDomain => nsFileProviderDomain -> IO Bool
supportsStringSearchRequest nsFileProviderDomain =
  sendMessage nsFileProviderDomain supportsStringSearchRequestSelector

-- | Whether the system should use this domain's  @NSFileProviderSearching@ implementation to support  search experiences.
--
-- Defaults to NO.
--
-- ObjC selector: @- setSupportsStringSearchRequest:@
setSupportsStringSearchRequest :: IsNSFileProviderDomain nsFileProviderDomain => nsFileProviderDomain -> Bool -> IO ()
setSupportsStringSearchRequest nsFileProviderDomain value =
  sendMessage nsFileProviderDomain setSupportsStringSearchRequestSelector value

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @initWithIdentifier:displayName:pathRelativeToDocumentStorage:@
initWithIdentifier_displayName_pathRelativeToDocumentStorageSelector :: Selector '[Id NSString, Id NSString, Id NSString] (Id NSFileProviderDomain)
initWithIdentifier_displayName_pathRelativeToDocumentStorageSelector = mkSelector "initWithIdentifier:displayName:pathRelativeToDocumentStorage:"

-- | @Selector@ for @initWithIdentifier:displayName:@
initWithIdentifier_displayNameSelector :: Selector '[Id NSString, Id NSString] (Id NSFileProviderDomain)
initWithIdentifier_displayNameSelector = mkSelector "initWithIdentifier:displayName:"

-- | @Selector@ for @initWithDisplayName:userInfo:volumeURL:@
initWithDisplayName_userInfo_volumeURLSelector :: Selector '[Id NSString, Id NSDictionary, Id NSURL] (Id NSFileProviderDomain)
initWithDisplayName_userInfo_volumeURLSelector = mkSelector "initWithDisplayName:userInfo:volumeURL:"

-- | @Selector@ for @identifier@
identifierSelector :: Selector '[] (Id NSString)
identifierSelector = mkSelector "identifier"

-- | @Selector@ for @displayName@
displayNameSelector :: Selector '[] (Id NSString)
displayNameSelector = mkSelector "displayName"

-- | @Selector@ for @pathRelativeToDocumentStorage@
pathRelativeToDocumentStorageSelector :: Selector '[] (Id NSString)
pathRelativeToDocumentStorageSelector = mkSelector "pathRelativeToDocumentStorage"

-- | @Selector@ for @disconnected@
disconnectedSelector :: Selector '[] Bool
disconnectedSelector = mkSelector "disconnected"

-- | @Selector@ for @userEnabled@
userEnabledSelector :: Selector '[] Bool
userEnabledSelector = mkSelector "userEnabled"

-- | @Selector@ for @hidden@
hiddenSelector :: Selector '[] Bool
hiddenSelector = mkSelector "hidden"

-- | @Selector@ for @setHidden:@
setHiddenSelector :: Selector '[Bool] ()
setHiddenSelector = mkSelector "setHidden:"

-- | @Selector@ for @replicated@
replicatedSelector :: Selector '[] Bool
replicatedSelector = mkSelector "replicated"

-- | @Selector@ for @testingModes@
testingModesSelector :: Selector '[] NSFileProviderDomainTestingModes
testingModesSelector = mkSelector "testingModes"

-- | @Selector@ for @setTestingModes:@
setTestingModesSelector :: Selector '[NSFileProviderDomainTestingModes] ()
setTestingModesSelector = mkSelector "setTestingModes:"

-- | @Selector@ for @backingStoreIdentity@
backingStoreIdentitySelector :: Selector '[] (Id NSData)
backingStoreIdentitySelector = mkSelector "backingStoreIdentity"

-- | @Selector@ for @supportsSyncingTrash@
supportsSyncingTrashSelector :: Selector '[] Bool
supportsSyncingTrashSelector = mkSelector "supportsSyncingTrash"

-- | @Selector@ for @setSupportsSyncingTrash:@
setSupportsSyncingTrashSelector :: Selector '[Bool] ()
setSupportsSyncingTrashSelector = mkSelector "setSupportsSyncingTrash:"

-- | @Selector@ for @volumeUUID@
volumeUUIDSelector :: Selector '[] (Id NSUUID)
volumeUUIDSelector = mkSelector "volumeUUID"

-- | @Selector@ for @userInfo@
userInfoSelector :: Selector '[] (Id NSDictionary)
userInfoSelector = mkSelector "userInfo"

-- | @Selector@ for @setUserInfo:@
setUserInfoSelector :: Selector '[Id NSDictionary] ()
setUserInfoSelector = mkSelector "setUserInfo:"

-- | @Selector@ for @replicatedKnownFolders@
replicatedKnownFoldersSelector :: Selector '[] NSFileProviderKnownFolders
replicatedKnownFoldersSelector = mkSelector "replicatedKnownFolders"

-- | @Selector@ for @supportedKnownFolders@
supportedKnownFoldersSelector :: Selector '[] NSFileProviderKnownFolders
supportedKnownFoldersSelector = mkSelector "supportedKnownFolders"

-- | @Selector@ for @setSupportedKnownFolders:@
setSupportedKnownFoldersSelector :: Selector '[NSFileProviderKnownFolders] ()
setSupportedKnownFoldersSelector = mkSelector "setSupportedKnownFolders:"

-- | @Selector@ for @supportsStringSearchRequest@
supportsStringSearchRequestSelector :: Selector '[] Bool
supportsStringSearchRequestSelector = mkSelector "supportsStringSearchRequest"

-- | @Selector@ for @setSupportsStringSearchRequest:@
setSupportsStringSearchRequestSelector :: Selector '[Bool] ()
setSupportsStringSearchRequestSelector = mkSelector "setSupportsStringSearchRequest:"

