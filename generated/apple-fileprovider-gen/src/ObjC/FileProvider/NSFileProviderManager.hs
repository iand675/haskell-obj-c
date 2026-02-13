{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

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
-- Generated bindings for @NSFileProviderManager@.
module ObjC.FileProvider.NSFileProviderManager
  ( NSFileProviderManager
  , IsNSFileProviderManager(..)
  , init_
  , managerForDomain
  , signalEnumeratorForContainerItemIdentifier_completionHandler
  , getUserVisibleURLForItemIdentifier_completionHandler
  , getIdentifierForUserVisibleFileAtURL_completionHandler
  , registerURLSessionTask_forItemWithIdentifier_completionHandler
  , temporaryDirectoryURLWithError
  , placeholderURLForURL
  , addDomain_completionHandler
  , removeDomain_completionHandler
  , removeDomain_mode_completionHandler
  , removeAllDomainsWithCompletionHandler
  , signalErrorResolved_completionHandler
  , globalProgressForKind
  , claimKnownFolders_localizedReason_completionHandler
  , releaseKnownFolders_localizedReason_completionHandler
  , listAvailableTestingOperationsWithError
  , runTestingOperations_error
  , getServiceWithName_itemIdentifier_completionHandler
  , requestDiagnosticCollectionForItemWithIdentifier_errorReason_completionHandler
  , checkDomainsCanBeStored_onVolumeAtURL_unsupportedReason_error
  , stateDirectoryURLWithError
  , requestDownloadForItemWithIdentifier_requestedRange_completionHandler
  , disconnectWithReason_options_completionHandler
  , reconnectWithCompletionHandler
  , waitForStabilizationWithCompletionHandler
  , waitForChangesOnItemsBelowItemWithIdentifier_completionHandler
  , evictItemWithIdentifier_completionHandler
  , importDomain_fromDirectoryAtURL_completionHandler
  , reimportItemsBelowItemWithIdentifier_completionHandler
  , requestModificationOfFields_forItemWithIdentifier_options_completionHandler
  , enumeratorForPendingItems
  , enumeratorForMaterializedItems
  , defaultManager
  , providerIdentifier
  , documentStorageURL
  , addDomain_completionHandlerSelector
  , checkDomainsCanBeStored_onVolumeAtURL_unsupportedReason_errorSelector
  , claimKnownFolders_localizedReason_completionHandlerSelector
  , defaultManagerSelector
  , disconnectWithReason_options_completionHandlerSelector
  , documentStorageURLSelector
  , enumeratorForMaterializedItemsSelector
  , enumeratorForPendingItemsSelector
  , evictItemWithIdentifier_completionHandlerSelector
  , getIdentifierForUserVisibleFileAtURL_completionHandlerSelector
  , getServiceWithName_itemIdentifier_completionHandlerSelector
  , getUserVisibleURLForItemIdentifier_completionHandlerSelector
  , globalProgressForKindSelector
  , importDomain_fromDirectoryAtURL_completionHandlerSelector
  , initSelector
  , listAvailableTestingOperationsWithErrorSelector
  , managerForDomainSelector
  , placeholderURLForURLSelector
  , providerIdentifierSelector
  , reconnectWithCompletionHandlerSelector
  , registerURLSessionTask_forItemWithIdentifier_completionHandlerSelector
  , reimportItemsBelowItemWithIdentifier_completionHandlerSelector
  , releaseKnownFolders_localizedReason_completionHandlerSelector
  , removeAllDomainsWithCompletionHandlerSelector
  , removeDomain_completionHandlerSelector
  , removeDomain_mode_completionHandlerSelector
  , requestDiagnosticCollectionForItemWithIdentifier_errorReason_completionHandlerSelector
  , requestDownloadForItemWithIdentifier_requestedRange_completionHandlerSelector
  , requestModificationOfFields_forItemWithIdentifier_options_completionHandlerSelector
  , runTestingOperations_errorSelector
  , signalEnumeratorForContainerItemIdentifier_completionHandlerSelector
  , signalErrorResolved_completionHandlerSelector
  , stateDirectoryURLWithErrorSelector
  , temporaryDirectoryURLWithErrorSelector
  , waitForChangesOnItemsBelowItemWithIdentifier_completionHandlerSelector
  , waitForStabilizationWithCompletionHandlerSelector

  -- * Enum types
  , NSFileProviderDomainRemovalMode(NSFileProviderDomainRemovalMode)
  , pattern NSFileProviderDomainRemovalModeRemoveAll
  , pattern NSFileProviderDomainRemovalModePreserveDirtyUserData
  , pattern NSFileProviderDomainRemovalModePreserveDownloadedUserData
  , NSFileProviderItemFields(NSFileProviderItemFields)
  , pattern NSFileProviderItemContents
  , pattern NSFileProviderItemFilename
  , pattern NSFileProviderItemParentItemIdentifier
  , pattern NSFileProviderItemLastUsedDate
  , pattern NSFileProviderItemTagData
  , pattern NSFileProviderItemFavoriteRank
  , pattern NSFileProviderItemCreationDate
  , pattern NSFileProviderItemContentModificationDate
  , pattern NSFileProviderItemFileSystemFlags
  , pattern NSFileProviderItemExtendedAttributes
  , pattern NSFileProviderItemTypeAndCreator
  , NSFileProviderKnownFolders(NSFileProviderKnownFolders)
  , pattern NSFileProviderDesktop
  , pattern NSFileProviderDocuments
  , NSFileProviderManagerDisconnectionOptions(NSFileProviderManagerDisconnectionOptions)
  , pattern NSFileProviderManagerDisconnectionOptionsTemporary
  , NSFileProviderModifyItemOptions(NSFileProviderModifyItemOptions)
  , pattern NSFileProviderModifyItemMayAlreadyExist
  , pattern NSFileProviderModifyItemFailOnConflict
  , pattern NSFileProviderModifyItemIsImmediateUploadRequestByPresentingApplication

  ) where

import Foreign.Ptr (Ptr, FunPtr)
import Foreign.C.Types

import ObjC.Runtime.Types
import ObjC.Runtime.Message (sendMessage, sendOwnedMessage, sendClassMessage, sendOwnedClassMessage)
import ObjC.Runtime.Selector (mkSelector)
import ObjC.Runtime.Class (getRequiredClass)

import ObjC.FileProvider.Internal.Classes
import ObjC.Foundation.Internal.Structs
import ObjC.FileProvider.Internal.Enums
import ObjC.Foundation.Internal.Classes

-- | @- init@
init_ :: IsNSFileProviderManager nsFileProviderManager => nsFileProviderManager -> IO (Id NSFileProviderManager)
init_ nsFileProviderManager =
  sendOwnedMessage nsFileProviderManager initSelector

-- | Return the manager for the specified domain.
--
-- ObjC selector: @+ managerForDomain:@
managerForDomain :: IsNSFileProviderDomain domain => domain -> IO (Id NSFileProviderManager)
managerForDomain domain =
  do
    cls' <- getRequiredClass "NSFileProviderManager"
    sendClassMessage cls' managerForDomainSelector (toNSFileProviderDomain domain)

-- | Call this method either in the app or in the extension to trigger an enumeration, typically in response to a push.
--
-- When using NSFileProviderExtension, the system will enumerate containers while the user is viewing them in the UI. If there are changes to the container while an enumerator is open, call this method with the identifier of that container. This will trigger another call to -[NSFileProviderEnumerator enumerateChangesForObserver:fromSyncAnchor:] on that enumerator, and the UI will be refreshed, giving the user live updates on the presented enumeration.
--
-- If there are changes in the working set, call this method with containerItemIdentifier set to NSFileProviderWorkingSetContainerItemIdentifier, even if there is no live enumeration for the working set container.
--
-- When using NSFileProviderReplicatedExtension, only call this method with NSFileProviderWorkingSetContainerItemIdentifier. Other container identifiers are ignored. The system will automatically propagate working set changes to the UI, without explicitly signaling the containers currently being viewed in the UI.
--
-- In addition to using this method, your application/extension can register for pushes using the PKPushTypeFileProvider push type. Pushes of the form {     "container-identifier": "<identifier>",     "domain": "<domain identifier>" } with a topic of "<your application identifier>.pushkit.fileprovider" will be translated into a call to signalEnumeratorForContainerItemIdentifier:completionHandler:.
--
-- ObjC selector: @- signalEnumeratorForContainerItemIdentifier:completionHandler:@
signalEnumeratorForContainerItemIdentifier_completionHandler :: (IsNSFileProviderManager nsFileProviderManager, IsNSString containerItemIdentifier) => nsFileProviderManager -> containerItemIdentifier -> Ptr () -> IO ()
signalEnumeratorForContainerItemIdentifier_completionHandler nsFileProviderManager containerItemIdentifier completion =
  sendMessage nsFileProviderManager signalEnumeratorForContainerItemIdentifier_completionHandlerSelector (toNSString containerItemIdentifier) completion

-- | Return the security scoped URL to the user visible location for an item identifier.
--
-- The caller must use file coordination (see NSFileCoordinator) if it wishes to read the content or list the children of the URL. The caller should not try to manipulate files in the user visible location. All changes coming from the provider should go through updates in the working set that will be applied to the user visible items by the system.
--
-- The location may differ from the logical parentURL/filename. If an item on disk cannot be assigned the requested name (e.g. because the local file system has different case collision rules from the provider), one of the items can be assigned a different local name. In that case, the "com.apple.fileprovider.before-bounce#PX" extended attribute will contain the filename before collision resolution. This attribute is only set if the item has been assigned a different local name following a collision. Such local names are not synced up to the provider; the purpose of the attribute is to enable consistency checkers to detect this case.
--
-- Before accessing the content of the returned URL, the caller must call `-[NSURL startAccessingSecurityScopedResource] on the returned URL and call @-[NSURL stopAccessingSecurityScopedResource]@ when done accessing the content.
--
-- The returned URL grants read-write access to the user visible location for the corresponding item.
--
-- On iOS, for replicated domains, the extension process will never be granted access to the user visible location, this function will always fail with @NSFileReadNoPermissionError@.
--
-- ObjC selector: @- getUserVisibleURLForItemIdentifier:completionHandler:@
getUserVisibleURLForItemIdentifier_completionHandler :: (IsNSFileProviderManager nsFileProviderManager, IsNSString itemIdentifier) => nsFileProviderManager -> itemIdentifier -> Ptr () -> IO ()
getUserVisibleURLForItemIdentifier_completionHandler nsFileProviderManager itemIdentifier completionHandler =
  sendMessage nsFileProviderManager getUserVisibleURLForItemIdentifier_completionHandlerSelector (toNSString itemIdentifier) completionHandler

-- | Return the identifier and domain for a user visible URL.
--
-- This method returns the identifier and domain of a user visible URL if applicable. Calling this method on a file which doesn't reside in your provider/domain, or which hasn't yet been assigned an identifier by the provider will return the Cocoa error NSFileNoSuchFileError.
--
-- ObjC selector: @+ getIdentifierForUserVisibleFileAtURL:completionHandler:@
getIdentifierForUserVisibleFileAtURL_completionHandler :: IsNSURL url => url -> Ptr () -> IO ()
getIdentifierForUserVisibleFileAtURL_completionHandler url completionHandler =
  do
    cls' <- getRequiredClass "NSFileProviderManager"
    sendClassMessage cls' getIdentifierForUserVisibleFileAtURL_completionHandlerSelector (toNSURL url) completionHandler

-- | Registers the given NSURLSessionTask to be responsible for the specified item. A given item can only have one task registered at a time. The task must be suspended at the time of calling. The task's progress is displayed on the item when the task is executed.
--
-- ObjC selector: @- registerURLSessionTask:forItemWithIdentifier:completionHandler:@
registerURLSessionTask_forItemWithIdentifier_completionHandler :: (IsNSFileProviderManager nsFileProviderManager, IsNSURLSessionTask task, IsNSString identifier) => nsFileProviderManager -> task -> identifier -> Ptr () -> IO ()
registerURLSessionTask_forItemWithIdentifier_completionHandler nsFileProviderManager task identifier completion =
  sendMessage nsFileProviderManager registerURLSessionTask_forItemWithIdentifier_completionHandlerSelector (toNSURLSessionTask task) (toNSString identifier) completion

-- | A temporary directory suitable to store files that will be exchanged with the system.
--
-- The returned URL is guaranteed to be on the same volume as the user visible URL, making sure the system can atomatically clone/move files from that location to the user visible URL. The provider can also use that directory as a target for moves and clones of content URL passed to createItemBasedOnTemplate or modifyItem.
--
-- If the system cannot find a suitable directory, this calls will fail. This could happen e.g. if the domain does not exist or is in instance of initialization.
--
-- This call succeeds when called from the extension process with an instance of the extension for the domain  unless domain was disconnected by @-[NSFileProviderExternalVolumeHandling shouldConnectExternalDomainWithCompletionHandler:]@. It can also fail in the extension process if the domain (external) is being setup for the very first time (meaning it never existed).
--
-- ObjC selector: @- temporaryDirectoryURLWithError:@
temporaryDirectoryURLWithError :: (IsNSFileProviderManager nsFileProviderManager, IsNSError error_) => nsFileProviderManager -> error_ -> IO (Id NSURL)
temporaryDirectoryURLWithError nsFileProviderManager error_ =
  sendMessage nsFileProviderManager temporaryDirectoryURLWithErrorSelector (toNSError error_)

-- | Returns the designated placeholder URL for a given file URL. Used in conjunction with writePlaceholderAtURL.
--
-- ObjC selector: @+ placeholderURLForURL:@
placeholderURLForURL :: IsNSURL url => url -> IO (Id NSURL)
placeholderURLForURL url =
  do
    cls' <- getRequiredClass "NSFileProviderManager"
    sendClassMessage cls' placeholderURLForURLSelector (toNSURL url)

-- | Register a domain in which items can be stored.
--
-- If a domain with the same identifier already exists, @addDomain@ will update the display name and hidden state of the domain and succeed.
--
-- When the domain is backed by a NSFileProviderReplicatedExtension, the system will create a disk location where the domain will be replicated. If that location already exists on disk this call will fail with the code NSFileWriteFileExistsError.
--
-- ObjC selector: @+ addDomain:completionHandler:@
addDomain_completionHandler :: IsNSFileProviderDomain domain => domain -> Ptr () -> IO ()
addDomain_completionHandler domain completionHandler =
  do
    cls' <- getRequiredClass "NSFileProviderManager"
    sendClassMessage cls' addDomain_completionHandlerSelector (toNSFileProviderDomain domain) completionHandler

-- | Remove a domain.
--
-- ObjC selector: @+ removeDomain:completionHandler:@
removeDomain_completionHandler :: IsNSFileProviderDomain domain => domain -> Ptr () -> IO ()
removeDomain_completionHandler domain completionHandler =
  do
    cls' <- getRequiredClass "NSFileProviderManager"
    sendClassMessage cls' removeDomain_completionHandlerSelector (toNSFileProviderDomain domain) completionHandler

-- | Remove a domain with options
--
-- ObjC selector: @+ removeDomain:mode:completionHandler:@
removeDomain_mode_completionHandler :: IsNSFileProviderDomain domain => domain -> NSFileProviderDomainRemovalMode -> Ptr () -> IO ()
removeDomain_mode_completionHandler domain mode completionHandler =
  do
    cls' <- getRequiredClass "NSFileProviderManager"
    sendClassMessage cls' removeDomain_mode_completionHandlerSelector (toNSFileProviderDomain domain) mode completionHandler

-- | Remove all registered domains.
--
-- ObjC selector: @+ removeAllDomainsWithCompletionHandler:@
removeAllDomainsWithCompletionHandler :: Ptr () -> IO ()
removeAllDomainsWithCompletionHandler completionHandler =
  do
    cls' <- getRequiredClass "NSFileProviderManager"
    sendClassMessage cls' removeAllDomainsWithCompletionHandlerSelector completionHandler

-- | Calling this method will cause the system to cancel throttling on every item which has been throttled due to the given error.
--
-- This call supports the following errors: - NSFileProviderErrorNotAuthenticated - NSFileProviderErrorInsufficientQuota - NSFileProviderErrorServerUnreachable - NSFileProviderErrorCannotSynchronize - NSFileProviderErrorExcludedFromSync
--
-- ObjC selector: @- signalErrorResolved:completionHandler:@
signalErrorResolved_completionHandler :: (IsNSFileProviderManager nsFileProviderManager, IsNSError error_) => nsFileProviderManager -> error_ -> Ptr () -> IO ()
signalErrorResolved_completionHandler nsFileProviderManager error_ completionHandler =
  sendMessage nsFileProviderManager signalErrorResolved_completionHandlerSelector (toNSError error_) completionHandler

-- | Returns the global progress for the specified kind of operations
--
-- This progress tracks all the ongoing kind of operations (from disk to the provider). Uploading operations are the operations from disk to the provider. Downloading operations are the operations from the provider to the disk.
--
-- The global progress exposes the two following data: - Number of items with an ongoing matching kind operation along with the grand total; - Number of bytes already transferred along with the total amount of bytes to transfer.
--
-- @totalUnitCount@ will only be reset when there are no operations left. If new operations of the matching kind arrive while the global progress is already ongoing, they will just be summed to the existing global progress.
--
-- By default, when no matching kind operations are active, the progress has its values set to 1 and its state set to finished.
--
-- The progress will be updated on the main queue. It is to be retained by the caller and to be observed through KVO.
--
-- The two only supported values for kind are: - NSProgressFileOperationKindUploading - NSProgressFileOperationKindDownloading
--
-- The returned progress will have its fileOperationKind property set.
--
-- ObjC selector: @- globalProgressForKind:@
globalProgressForKind :: (IsNSFileProviderManager nsFileProviderManager, IsNSString kind) => nsFileProviderManager -> kind -> IO (Id NSProgress)
globalProgressForKind nsFileProviderManager kind =
  sendMessage nsFileProviderManager globalProgressForKindSelector (toNSString kind)

-- | Request the specified known folders to be synced by this domain.
--
-- This method allows the provider to claim a set of known folders described by the non-null properties of the knownFolders parameter. The system will only enable sync for those folders in that domain if the set of locations is valid and if the user agrees.
--
-- This API should only be called as a result of the user requesting, via UI in the provider's application, that they wish to start syncing the Desktop and Document folders. If the provider chooses to implement a UI which invokes this API, the provider should also implement a UI for the user to request to stop syncing the Desktop and Document folders, using the @-[NSFileProviderManager releaseKnownFolders:localizedReason:completionHandler:]@ method.
--
-- The reason specified in this call is a custom string that the provider can pass and will be presented to the user as a way to explain why it is claiming those known folders. One suggested phrasing would be:
--
-- > Keep your Desktop & Documents in sync with <Provider name> and access them from other devices and from <Provider website>.
--
-- If the user denies the transition of the known folders, the call will fail with @NSUserCancelledError@.
--
-- The call will fail if: - one or more locations are not folders - multiple locations are backed by the same folder - a known folder doesn't live on the same volume as the root of the domain - the known folders don't have the same parent folder - ...
--
-- Currently, only claiming both ~/Desktop and ~/Documents together is allowed.
--
-- ObjC selector: @- claimKnownFolders:localizedReason:completionHandler:@
claimKnownFolders_localizedReason_completionHandler :: (IsNSFileProviderManager nsFileProviderManager, IsNSFileProviderKnownFolderLocations knownFolders, IsNSString localizedReason) => nsFileProviderManager -> knownFolders -> localizedReason -> Ptr () -> IO ()
claimKnownFolders_localizedReason_completionHandler nsFileProviderManager knownFolders localizedReason completionHandler =
  sendMessage nsFileProviderManager claimKnownFolders_localizedReason_completionHandlerSelector (toNSFileProviderKnownFolderLocations knownFolders) (toNSString localizedReason) completionHandler

-- | Request that the system stops replicating the specified known folders in the domain.
--
-- This call can be used by the provider to immediately disable replication of the specified known folders.
--
-- ObjC selector: @- releaseKnownFolders:localizedReason:completionHandler:@
releaseKnownFolders_localizedReason_completionHandler :: (IsNSFileProviderManager nsFileProviderManager, IsNSString localizedReason) => nsFileProviderManager -> NSFileProviderKnownFolders -> localizedReason -> Ptr () -> IO ()
releaseKnownFolders_localizedReason_completionHandler nsFileProviderManager knownFolders localizedReason completionHandler =
  sendMessage nsFileProviderManager releaseKnownFolders_localizedReason_completionHandlerSelector knownFolders (toNSString localizedReason) completionHandler

-- | List the available operations.
--
-- This lists all of the operations that are ready to be scheduled by the system. The system waits for all the pending disk and working set updates to be known before returning.
--
-- The operations that are returned may become invalid if the system receives new disk or working set events, or if some operation are scheduled using -runTestingOperations:error:.
--
-- ObjC selector: @- listAvailableTestingOperationsWithError:@
listAvailableTestingOperationsWithError :: (IsNSFileProviderManager nsFileProviderManager, IsNSError error_) => nsFileProviderManager -> error_ -> IO (Id NSArray)
listAvailableTestingOperationsWithError nsFileProviderManager error_ =
  sendMessage nsFileProviderManager listAvailableTestingOperationsWithErrorSelector (toNSError error_)

-- | Run a set of operations.
--
-- Ask the system to schedule the execution of the listed operations. The system will wait until all those operations have completed and report a per-operation error in case an operation fails.
--
-- ObjC selector: @- runTestingOperations:error:@
runTestingOperations_error :: (IsNSFileProviderManager nsFileProviderManager, IsNSArray operations, IsNSError error_) => nsFileProviderManager -> operations -> error_ -> IO (Id NSDictionary)
runTestingOperations_error nsFileProviderManager operations error_ =
  sendMessage nsFileProviderManager runTestingOperations_errorSelector (toNSArray operations) (toNSError error_)

-- | Retrieve the service with the specified named for the specified item.
--
-- ObjC selector: @- getServiceWithName:itemIdentifier:completionHandler:@
getServiceWithName_itemIdentifier_completionHandler :: (IsNSFileProviderManager nsFileProviderManager, IsNSString serviceName, IsNSString itemIdentifier) => nsFileProviderManager -> serviceName -> itemIdentifier -> Ptr () -> IO ()
getServiceWithName_itemIdentifier_completionHandler nsFileProviderManager serviceName itemIdentifier completionHandler =
  sendMessage nsFileProviderManager getServiceWithName_itemIdentifier_completionHandlerSelector (toNSString serviceName) (toNSString itemIdentifier) completionHandler

-- | Request diagnostics collection for the item.
--
-- This will prompt the user about an issue with the sync in the provider and ask their permission to collection diagnostic information and to send them to Apple for further analysis.
--
-- This call is to be used wisely with care given there's global throttling on it preventing spamming the users. Furthermore it should be used in collaboration with Apple when you detect a misbehavior in the sync in your provider likely caused by a system bug and you need to work with Apple in order to resolve it.
--
-- This will return whether the call was allowed or not - not if it suceed This method will only return an error if the user was not on a Seed build
--
-- It is mandatory to provide an error for the item why the collection is requested. The error won't be shown to the user (a generic message will be shown instead) It will surface in the generated report though
--
-- It is important to note that even if the call is allowed, it might not trigger diagnostic collection nor prompt to the user depending on the system state and other throttling parameters
--
-- ObjC selector: @- requestDiagnosticCollectionForItemWithIdentifier:errorReason:completionHandler:@
requestDiagnosticCollectionForItemWithIdentifier_errorReason_completionHandler :: (IsNSFileProviderManager nsFileProviderManager, IsNSString itemIdentifier, IsNSError errorReason) => nsFileProviderManager -> itemIdentifier -> errorReason -> Ptr () -> IO ()
requestDiagnosticCollectionForItemWithIdentifier_errorReason_completionHandler nsFileProviderManager itemIdentifier errorReason completionHandler =
  sendMessage nsFileProviderManager requestDiagnosticCollectionForItemWithIdentifier_errorReason_completionHandlerSelector (toNSString itemIdentifier) (toNSError errorReason) completionHandler

-- | Check if a URL is eligible for storing a domain.
--
-- This returns whether the check has been performed succesfully - NOT whether the drive is eligible.
--
-- If an error was encountered while checking, this method returns FALSE and an error describing the problem will be set.
--
-- The eligible parameter will contain the result of the check and indicate whether the volume can be used to store FP domains. Its value is only defined if the call returns YES.
--
-- The url can be any existing and accessible URL on the volume for which you want to assess eligibility. The checks are volume-wide and the exact location on the volume doesn't impact them.
--
-- If a drive is eligible, unsupportedReason will be empty (0). Otherwise it will contain the list of identified conditions that currently prevent this drive from being used to store FP domains.
--
-- ObjC selector: @+ checkDomainsCanBeStored:onVolumeAtURL:unsupportedReason:error:@
checkDomainsCanBeStored_onVolumeAtURL_unsupportedReason_error :: (IsNSURL url, IsNSError error_) => Ptr Bool -> url -> Ptr NSFileProviderVolumeUnsupportedReason -> error_ -> IO Bool
checkDomainsCanBeStored_onVolumeAtURL_unsupportedReason_error eligible url unsupportedReason error_ =
  do
    cls' <- getRequiredClass "NSFileProviderManager"
    sendClassMessage cls' checkDomainsCanBeStored_onVolumeAtURL_unsupportedReason_errorSelector eligible (toNSURL url) unsupportedReason (toNSError error_)

-- | A directory suitable for storing state information for the domain.
--
-- The returned URL is guaranteed to be on the same volume as the user visible URL and the temporary URL, making sure the system can atomatically clone/move files from that location to the user visible URL. The caller is responsible for managing the security scope of the returned URL.
--
-- When syncing a domain on an external volume, all information about the sync state must be kept in this directory if the volume is to be shared between multiple machines.
--
-- If the system cannot find a suitable directory, this call will fail. This could happen e.g. if the domain does not exist or is in instance of initialization.
--
-- This call will not fail when called from the extension process with an active instance of the extension for that domain unless the domain is being setup for the very first time (meaning it never existed).
--
-- Removing the domain will remove the corresponding directory along with it.
--
-- ObjC selector: @- stateDirectoryURLWithError:@
stateDirectoryURLWithError :: (IsNSFileProviderManager nsFileProviderManager, IsNSError error_) => nsFileProviderManager -> error_ -> IO (Id NSURL)
stateDirectoryURLWithError nsFileProviderManager error_ =
  sendMessage nsFileProviderManager stateDirectoryURLWithErrorSelector (toNSError error_)

-- | Request that the system schedule a download for an item.
--
-- The completion handler is called when the system acknowledges the download request, or with an error indicating why it didn't (e.g NSFileProviderErrorNoSuchItem.) The system will then call -fetchContentsForItemWithIdentifier at the earliest convenient time.
--
-- Set rangeToMaterialize to NSMakeRange(offset, nbytes) to request a partial download. The system will then invoke -fetchPartialContentsForItemWithIdentifier instead of fetchContentsForItemWithIdentifier. For a full download, set rangeToMaterialize to NSMakeRange(NSNotFound, 0). -[NSFileProviderManager evictItemWithIdentifier:completionHandler:] must be called on a partially materialized file before requesting an extent to be downloaded from a later version of the file.
--
-- This method cannot be used to download directories recursively. When invoked on a dataless directory, it will trigger an enumeration of the directory, causing a materialization of the directory one level down only. All the children of the directory will remain dataless after the enumeration.
--
-- ObjC selector: @- requestDownloadForItemWithIdentifier:requestedRange:completionHandler:@
requestDownloadForItemWithIdentifier_requestedRange_completionHandler :: (IsNSFileProviderManager nsFileProviderManager, IsNSString itemIdentifier) => nsFileProviderManager -> itemIdentifier -> NSRange -> Ptr () -> IO ()
requestDownloadForItemWithIdentifier_requestedRange_completionHandler nsFileProviderManager itemIdentifier rangeToMaterialize completionHandler =
  sendMessage nsFileProviderManager requestDownloadForItemWithIdentifier_requestedRange_completionHandlerSelector (toNSString itemIdentifier) rangeToMaterialize completionHandler

-- | @- disconnectWithReason:options:completionHandler:@
disconnectWithReason_options_completionHandler :: (IsNSFileProviderManager nsFileProviderManager, IsNSString localizedReason) => nsFileProviderManager -> localizedReason -> NSFileProviderManagerDisconnectionOptions -> Ptr () -> IO ()
disconnectWithReason_options_completionHandler nsFileProviderManager localizedReason options completionHandler =
  sendMessage nsFileProviderManager disconnectWithReason_options_completionHandlerSelector (toNSString localizedReason) options completionHandler

-- | @- reconnectWithCompletionHandler:@
reconnectWithCompletionHandler :: IsNSFileProviderManager nsFileProviderManager => nsFileProviderManager -> Ptr () -> IO ()
reconnectWithCompletionHandler nsFileProviderManager completionHandler =
  sendMessage nsFileProviderManager reconnectWithCompletionHandlerSelector completionHandler

-- | Wait for stabilization of the domain.
--
-- The system will wait until it is caught up with the file system's changes up to the time of the call, then wait until it is caught up with the provider's changes up to the time of the call.
--
-- The completion handler is called when both sets of changes are caught up to at least the time of the call. This is useful to enforce a consistent state for testing.
--
-- ObjC selector: @- waitForStabilizationWithCompletionHandler:@
waitForStabilizationWithCompletionHandler :: IsNSFileProviderManager nsFileProviderManager => nsFileProviderManager -> Ptr () -> IO ()
waitForStabilizationWithCompletionHandler nsFileProviderManager completionHandler =
  sendMessage nsFileProviderManager waitForStabilizationWithCompletionHandlerSelector completionHandler

-- | Wait for all changes on disk in the sub-hierarchy of the item to be acknowledged by the extension.
--
-- This call can be used to guarantee operation ordering in a sub-hierarchy of the provider. The completion handler is called when all the changes for descendents of the item have been acknowledged by the extension. If any error is met during that process, an error will be raised, in which case the caller should not assume all the changes have been received.
--
-- This call will only wait for changes affecting items that were already descendents of the requested item in the provider, or items that have been newly created on disk. It will not wait for items that are already known from the provider and are being moved in the directory. As a consequence, that call can be used from within a call to -[NSFileProviderReplicatedExtension modifyItem:baseVersion:changedFields:contents:options:completionHandler:]. Also note that the call will return immediately on items that are not directories.
--
-- In case a change cannot be applied to the provider, the call will fail with NSFileProviderErrorCannotSynchronize including the NSFileProviderErrorItemKey with the identifier of the item that could not be synced if that item is known by the provider.
--
-- ObjC selector: @- waitForChangesOnItemsBelowItemWithIdentifier:completionHandler:@
waitForChangesOnItemsBelowItemWithIdentifier_completionHandler :: (IsNSFileProviderManager nsFileProviderManager, IsNSString itemIdentifier) => nsFileProviderManager -> itemIdentifier -> Ptr () -> IO ()
waitForChangesOnItemsBelowItemWithIdentifier_completionHandler nsFileProviderManager itemIdentifier completionHandler =
  sendMessage nsFileProviderManager waitForChangesOnItemsBelowItemWithIdentifier_completionHandlerSelector (toNSString itemIdentifier) completionHandler

-- | Request that the system remove an item from its cache.
--
-- When called on a file, the file will be made dataless.
--
-- When called on a directory, first each of the directory's children will be evicted (child files are made dataless, child directories are recursively evicted). Then the directory itself will be made dataless. If a non-evictable child is encountered, eviction will stop immediately and the completionHandler will be called with the NSFileProviderErrorNonEvictableChildren error. The error will include information on why and which children could not be evicted in -[NSError underlyingErrors].
--
-- The materialization state of the remaining items may be either materialized or evicted, depending on the traversal order.
--
-- The completion handler is called after the items have been evicted from disk or immediately when an error occurs.
--
-- Eviction might fail with the following errors :   - NSFileProviderErrorDomain.NSFileProviderErrorUnsyncedEdits if the item had non-uploaded changes.   - NSFileProviderErrorDomain.NSFileProviderErrorNonEvictable if the item has been marked as non-purgeable by the provider.   - NSPOSIXErrorDomain.EBUSY : if the item has open file descriptors on it.   - NSPOSIXErrorDomain.EMLINK : if the item has several hardlinks.   - other NSPOSIXErrorDomain error codes if the system was unable to access or manipulate the corresponding file.
--
-- ObjC selector: @- evictItemWithIdentifier:completionHandler:@
evictItemWithIdentifier_completionHandler :: (IsNSFileProviderManager nsFileProviderManager, IsNSString itemIdentifier) => nsFileProviderManager -> itemIdentifier -> Ptr () -> IO ()
evictItemWithIdentifier_completionHandler nsFileProviderManager itemIdentifier completionHandler =
  sendMessage nsFileProviderManager evictItemWithIdentifier_completionHandlerSelector (toNSString itemIdentifier) completionHandler

-- | Request the creation of a new domain that will take ownership of on-disk data that were previously managed without a file provider.
--
-- You can use this method in order to migrate from a software that managed a file hierarchy on disk to a NSFileProviderExtension without having to redownload the data that was already on disk.
--
-- The URL is expected to point to a directory. That directory will be moved away, its ownership being taken by the system. From this point, your extension's createItemFromTemplate method will be called for every item found in the directory with the special NSFileProviderCreateItemMayAlreadyExist option.
--
-- In case a domain with the same name already exists in the file provider manager, the call will fail with the code NSFileWriteFileExistsError. The URL will remain untouched. In case the system does not allow the extension to request a migration, the call will fail with NSFeatureUnsupportedError.
--
-- In case of success, the URL will become invalid and the domain will be created. The completion handler is called as soon as the domain is created. Your provider will receive calls to createItemBasedOnTemplate afterward.
--
-- When the import of the file hierarchy is finished, the system calls -[NSFileProviderExtension signalDidFinishImportingItemsFromDiskWithCompletionHandler:]. In case -[NSFileProviderManager reimportItemsBelowItemWithIdentifier:completionHandler:] is called before the end of the import, a single call to importDidFinishWithCompletionHandler will be received for both the import and the scan.
--
-- ObjC selector: @+ importDomain:fromDirectoryAtURL:completionHandler:@
importDomain_fromDirectoryAtURL_completionHandler :: (IsNSFileProviderDomain domain, IsNSURL url) => domain -> url -> Ptr () -> IO ()
importDomain_fromDirectoryAtURL_completionHandler domain url completionHandler =
  do
    cls' <- getRequiredClass "NSFileProviderManager"
    sendClassMessage cls' importDomain_fromDirectoryAtURL_completionHandlerSelector (toNSFileProviderDomain domain) (toNSURL url) completionHandler

-- | Notify the system that the itemIdentifiers known by the system are not valid anymore.
--
-- This can be called by an extension in case it has lost track of its synchronisation state and as a consequence is not able to guarantee the stability of the itemIdentifiers anymore. In that case, the system will trigger a scan of any data that is cached on disk and call createItemBasedOnTemplate with the special NSFileProviderCreateItemMayAlreadyExist option so that the extension can specify the new itemIdentifier for those items. The provided item identifier is inclusive, meaning the specified item will be re-import as well as any children in case it is a container.
--
-- In case the extension has lost its synchronisation state but is still able to guarantee the stability of the itemIdentifiers, it should make sure that querying the working set enumerator with an anchor that predates the synchronisation loss will cause a NSFileProviderErrorSyncAnchorExpired error.
--
-- In case the extension has lost its synchronisation state and is not interested in preserving the data cached on disk, it can remove and re-add the affected domain.
--
-- The completion handler is called as soon as the reimport is initiated and does not not reflect the end of the import. When the import of the file hierarchy is finished, the system calls -[NSFileProviderExtension importDidFinishWithCompletionHandler:].
--
-- In some circumstances, in particular in case the requested item is the root item, calling reimport will cause the system to stop the extension process. If the call is initiated from the extension, the system does not guarantee that the completion handler will be called before the extension is stopped. When called on the root item, reimport will cause the system to rebuild its backing store for the domain. See @-[NSFileProviderDomain backingStoreIdentity]@.
--
-- If this method succeeds, the system will reimport at least the requested sub-tree, but may import more.
--
-- If the requested item has no on-disk representation, the completion handler will be called with a NSFileProviderErrorNoSuchItem error. The same error will be reported if the reimport request happens quickly after a previous import / reimport and the corresponding item hasn't been reimported yet.
--
-- ObjC selector: @- reimportItemsBelowItemWithIdentifier:completionHandler:@
reimportItemsBelowItemWithIdentifier_completionHandler :: (IsNSFileProviderManager nsFileProviderManager, IsNSString itemIdentifier) => nsFileProviderManager -> itemIdentifier -> Ptr () -> IO ()
reimportItemsBelowItemWithIdentifier_completionHandler nsFileProviderManager itemIdentifier completionHandler =
  sendMessage nsFileProviderManager reimportItemsBelowItemWithIdentifier_completionHandlerSelector (toNSString itemIdentifier) completionHandler

-- | Request that the system schedules a call to -[NSFileProviderReplicatedExtension modifyItem:] for the given item identifier.  The fields passed to modifyItem will contain at least the set requested via the @fields@ parameter.  The completion handler is called when the system has persisted the request. There is no guarantee as to when the  modifyItem call will be scheduled.  The completion handler may be called with an error. If the provider passes the @.content@ field when the item  is not downloaded, or when the item is a folder, then the system will return CocoaError(.ubiquitousFileUnavailable).
--
-- ObjC selector: @- requestModificationOfFields:forItemWithIdentifier:options:completionHandler:@
requestModificationOfFields_forItemWithIdentifier_options_completionHandler :: (IsNSFileProviderManager nsFileProviderManager, IsNSString itemIdentifier) => nsFileProviderManager -> NSFileProviderItemFields -> itemIdentifier -> NSFileProviderModifyItemOptions -> Ptr () -> IO ()
requestModificationOfFields_forItemWithIdentifier_options_completionHandler nsFileProviderManager fields itemIdentifier options completionHandler =
  sendMessage nsFileProviderManager requestModificationOfFields_forItemWithIdentifier_options_completionHandlerSelector fields (toNSString itemIdentifier) options completionHandler

-- | Returns an enumerator for the set of pending items.
--
-- This enumerator behaves like the materialized set enumerator. On later modifications in the set, the system will call 'pendingItemsDidChangeWithCompletionHandler'.
--
-- ObjC selector: @- enumeratorForPendingItems@
enumeratorForPendingItems :: IsNSFileProviderManager nsFileProviderManager => nsFileProviderManager -> IO RawId
enumeratorForPendingItems nsFileProviderManager =
  sendMessage nsFileProviderManager enumeratorForPendingItemsSelector

-- | Returns an enumerator for the set of materialized items.
--
-- When calling -[NSFileProviderEnumerator enumerateItemsForObserver:startingAtPage:] on the returned enumerator, pass the result of [NSData new] as the starting page. The sorting page constants (NSFileProviderInitialPageSortedByName and NSFileProviderInitialPageSortedByDate) will not influence the order of the items enumerated from the materialized set.
--
-- This enumerator is unlike other enumerators because the roles of the system and the app/extension are reversed: - The system enumerates the working set after the extension calls   'signalEnumeratorForContainerItemIdentifier'; - The app/extension enumerates the materialized set after the system calls   'materializedItemsDidChangeWithCompletionHandler'.
--
-- ObjC selector: @- enumeratorForMaterializedItems@
enumeratorForMaterializedItems :: IsNSFileProviderManager nsFileProviderManager => nsFileProviderManager -> IO RawId
enumeratorForMaterializedItems nsFileProviderManager =
  sendMessage nsFileProviderManager enumeratorForMaterializedItemsSelector

-- | Return the manager responsible for the default domain.
--
-- ObjC selector: @+ defaultManager@
defaultManager :: IO (Id NSFileProviderManager)
defaultManager  =
  do
    cls' <- getRequiredClass "NSFileProviderManager"
    sendClassMessage cls' defaultManagerSelector

-- | The purpose identifier of your file provider extension. A coordination using a file coordinator with this purpose identifier set will not trigger your file provider extension. You can use this to e.g. perform speculative work on behalf of the file provider from the main app.
--
-- ObjC selector: @- providerIdentifier@
providerIdentifier :: IsNSFileProviderManager nsFileProviderManager => nsFileProviderManager -> IO (Id NSString)
providerIdentifier nsFileProviderManager =
  sendMessage nsFileProviderManager providerIdentifierSelector

-- | The root URL for provided documents. This URL is derived by consulting the NSExtensionFileProviderDocumentGroup property on your extension. The document storage URL is the folder "File Provider Storage" in the corresponding container.
--
-- If the NSExtensionFileProviderDocumentGroup property is not set, calling this method will result in an error.
--
-- ObjC selector: @- documentStorageURL@
documentStorageURL :: IsNSFileProviderManager nsFileProviderManager => nsFileProviderManager -> IO (Id NSURL)
documentStorageURL nsFileProviderManager =
  sendMessage nsFileProviderManager documentStorageURLSelector

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @init@
initSelector :: Selector '[] (Id NSFileProviderManager)
initSelector = mkSelector "init"

-- | @Selector@ for @managerForDomain:@
managerForDomainSelector :: Selector '[Id NSFileProviderDomain] (Id NSFileProviderManager)
managerForDomainSelector = mkSelector "managerForDomain:"

-- | @Selector@ for @signalEnumeratorForContainerItemIdentifier:completionHandler:@
signalEnumeratorForContainerItemIdentifier_completionHandlerSelector :: Selector '[Id NSString, Ptr ()] ()
signalEnumeratorForContainerItemIdentifier_completionHandlerSelector = mkSelector "signalEnumeratorForContainerItemIdentifier:completionHandler:"

-- | @Selector@ for @getUserVisibleURLForItemIdentifier:completionHandler:@
getUserVisibleURLForItemIdentifier_completionHandlerSelector :: Selector '[Id NSString, Ptr ()] ()
getUserVisibleURLForItemIdentifier_completionHandlerSelector = mkSelector "getUserVisibleURLForItemIdentifier:completionHandler:"

-- | @Selector@ for @getIdentifierForUserVisibleFileAtURL:completionHandler:@
getIdentifierForUserVisibleFileAtURL_completionHandlerSelector :: Selector '[Id NSURL, Ptr ()] ()
getIdentifierForUserVisibleFileAtURL_completionHandlerSelector = mkSelector "getIdentifierForUserVisibleFileAtURL:completionHandler:"

-- | @Selector@ for @registerURLSessionTask:forItemWithIdentifier:completionHandler:@
registerURLSessionTask_forItemWithIdentifier_completionHandlerSelector :: Selector '[Id NSURLSessionTask, Id NSString, Ptr ()] ()
registerURLSessionTask_forItemWithIdentifier_completionHandlerSelector = mkSelector "registerURLSessionTask:forItemWithIdentifier:completionHandler:"

-- | @Selector@ for @temporaryDirectoryURLWithError:@
temporaryDirectoryURLWithErrorSelector :: Selector '[Id NSError] (Id NSURL)
temporaryDirectoryURLWithErrorSelector = mkSelector "temporaryDirectoryURLWithError:"

-- | @Selector@ for @placeholderURLForURL:@
placeholderURLForURLSelector :: Selector '[Id NSURL] (Id NSURL)
placeholderURLForURLSelector = mkSelector "placeholderURLForURL:"

-- | @Selector@ for @addDomain:completionHandler:@
addDomain_completionHandlerSelector :: Selector '[Id NSFileProviderDomain, Ptr ()] ()
addDomain_completionHandlerSelector = mkSelector "addDomain:completionHandler:"

-- | @Selector@ for @removeDomain:completionHandler:@
removeDomain_completionHandlerSelector :: Selector '[Id NSFileProviderDomain, Ptr ()] ()
removeDomain_completionHandlerSelector = mkSelector "removeDomain:completionHandler:"

-- | @Selector@ for @removeDomain:mode:completionHandler:@
removeDomain_mode_completionHandlerSelector :: Selector '[Id NSFileProviderDomain, NSFileProviderDomainRemovalMode, Ptr ()] ()
removeDomain_mode_completionHandlerSelector = mkSelector "removeDomain:mode:completionHandler:"

-- | @Selector@ for @removeAllDomainsWithCompletionHandler:@
removeAllDomainsWithCompletionHandlerSelector :: Selector '[Ptr ()] ()
removeAllDomainsWithCompletionHandlerSelector = mkSelector "removeAllDomainsWithCompletionHandler:"

-- | @Selector@ for @signalErrorResolved:completionHandler:@
signalErrorResolved_completionHandlerSelector :: Selector '[Id NSError, Ptr ()] ()
signalErrorResolved_completionHandlerSelector = mkSelector "signalErrorResolved:completionHandler:"

-- | @Selector@ for @globalProgressForKind:@
globalProgressForKindSelector :: Selector '[Id NSString] (Id NSProgress)
globalProgressForKindSelector = mkSelector "globalProgressForKind:"

-- | @Selector@ for @claimKnownFolders:localizedReason:completionHandler:@
claimKnownFolders_localizedReason_completionHandlerSelector :: Selector '[Id NSFileProviderKnownFolderLocations, Id NSString, Ptr ()] ()
claimKnownFolders_localizedReason_completionHandlerSelector = mkSelector "claimKnownFolders:localizedReason:completionHandler:"

-- | @Selector@ for @releaseKnownFolders:localizedReason:completionHandler:@
releaseKnownFolders_localizedReason_completionHandlerSelector :: Selector '[NSFileProviderKnownFolders, Id NSString, Ptr ()] ()
releaseKnownFolders_localizedReason_completionHandlerSelector = mkSelector "releaseKnownFolders:localizedReason:completionHandler:"

-- | @Selector@ for @listAvailableTestingOperationsWithError:@
listAvailableTestingOperationsWithErrorSelector :: Selector '[Id NSError] (Id NSArray)
listAvailableTestingOperationsWithErrorSelector = mkSelector "listAvailableTestingOperationsWithError:"

-- | @Selector@ for @runTestingOperations:error:@
runTestingOperations_errorSelector :: Selector '[Id NSArray, Id NSError] (Id NSDictionary)
runTestingOperations_errorSelector = mkSelector "runTestingOperations:error:"

-- | @Selector@ for @getServiceWithName:itemIdentifier:completionHandler:@
getServiceWithName_itemIdentifier_completionHandlerSelector :: Selector '[Id NSString, Id NSString, Ptr ()] ()
getServiceWithName_itemIdentifier_completionHandlerSelector = mkSelector "getServiceWithName:itemIdentifier:completionHandler:"

-- | @Selector@ for @requestDiagnosticCollectionForItemWithIdentifier:errorReason:completionHandler:@
requestDiagnosticCollectionForItemWithIdentifier_errorReason_completionHandlerSelector :: Selector '[Id NSString, Id NSError, Ptr ()] ()
requestDiagnosticCollectionForItemWithIdentifier_errorReason_completionHandlerSelector = mkSelector "requestDiagnosticCollectionForItemWithIdentifier:errorReason:completionHandler:"

-- | @Selector@ for @checkDomainsCanBeStored:onVolumeAtURL:unsupportedReason:error:@
checkDomainsCanBeStored_onVolumeAtURL_unsupportedReason_errorSelector :: Selector '[Ptr Bool, Id NSURL, Ptr NSFileProviderVolumeUnsupportedReason, Id NSError] Bool
checkDomainsCanBeStored_onVolumeAtURL_unsupportedReason_errorSelector = mkSelector "checkDomainsCanBeStored:onVolumeAtURL:unsupportedReason:error:"

-- | @Selector@ for @stateDirectoryURLWithError:@
stateDirectoryURLWithErrorSelector :: Selector '[Id NSError] (Id NSURL)
stateDirectoryURLWithErrorSelector = mkSelector "stateDirectoryURLWithError:"

-- | @Selector@ for @requestDownloadForItemWithIdentifier:requestedRange:completionHandler:@
requestDownloadForItemWithIdentifier_requestedRange_completionHandlerSelector :: Selector '[Id NSString, NSRange, Ptr ()] ()
requestDownloadForItemWithIdentifier_requestedRange_completionHandlerSelector = mkSelector "requestDownloadForItemWithIdentifier:requestedRange:completionHandler:"

-- | @Selector@ for @disconnectWithReason:options:completionHandler:@
disconnectWithReason_options_completionHandlerSelector :: Selector '[Id NSString, NSFileProviderManagerDisconnectionOptions, Ptr ()] ()
disconnectWithReason_options_completionHandlerSelector = mkSelector "disconnectWithReason:options:completionHandler:"

-- | @Selector@ for @reconnectWithCompletionHandler:@
reconnectWithCompletionHandlerSelector :: Selector '[Ptr ()] ()
reconnectWithCompletionHandlerSelector = mkSelector "reconnectWithCompletionHandler:"

-- | @Selector@ for @waitForStabilizationWithCompletionHandler:@
waitForStabilizationWithCompletionHandlerSelector :: Selector '[Ptr ()] ()
waitForStabilizationWithCompletionHandlerSelector = mkSelector "waitForStabilizationWithCompletionHandler:"

-- | @Selector@ for @waitForChangesOnItemsBelowItemWithIdentifier:completionHandler:@
waitForChangesOnItemsBelowItemWithIdentifier_completionHandlerSelector :: Selector '[Id NSString, Ptr ()] ()
waitForChangesOnItemsBelowItemWithIdentifier_completionHandlerSelector = mkSelector "waitForChangesOnItemsBelowItemWithIdentifier:completionHandler:"

-- | @Selector@ for @evictItemWithIdentifier:completionHandler:@
evictItemWithIdentifier_completionHandlerSelector :: Selector '[Id NSString, Ptr ()] ()
evictItemWithIdentifier_completionHandlerSelector = mkSelector "evictItemWithIdentifier:completionHandler:"

-- | @Selector@ for @importDomain:fromDirectoryAtURL:completionHandler:@
importDomain_fromDirectoryAtURL_completionHandlerSelector :: Selector '[Id NSFileProviderDomain, Id NSURL, Ptr ()] ()
importDomain_fromDirectoryAtURL_completionHandlerSelector = mkSelector "importDomain:fromDirectoryAtURL:completionHandler:"

-- | @Selector@ for @reimportItemsBelowItemWithIdentifier:completionHandler:@
reimportItemsBelowItemWithIdentifier_completionHandlerSelector :: Selector '[Id NSString, Ptr ()] ()
reimportItemsBelowItemWithIdentifier_completionHandlerSelector = mkSelector "reimportItemsBelowItemWithIdentifier:completionHandler:"

-- | @Selector@ for @requestModificationOfFields:forItemWithIdentifier:options:completionHandler:@
requestModificationOfFields_forItemWithIdentifier_options_completionHandlerSelector :: Selector '[NSFileProviderItemFields, Id NSString, NSFileProviderModifyItemOptions, Ptr ()] ()
requestModificationOfFields_forItemWithIdentifier_options_completionHandlerSelector = mkSelector "requestModificationOfFields:forItemWithIdentifier:options:completionHandler:"

-- | @Selector@ for @enumeratorForPendingItems@
enumeratorForPendingItemsSelector :: Selector '[] RawId
enumeratorForPendingItemsSelector = mkSelector "enumeratorForPendingItems"

-- | @Selector@ for @enumeratorForMaterializedItems@
enumeratorForMaterializedItemsSelector :: Selector '[] RawId
enumeratorForMaterializedItemsSelector = mkSelector "enumeratorForMaterializedItems"

-- | @Selector@ for @defaultManager@
defaultManagerSelector :: Selector '[] (Id NSFileProviderManager)
defaultManagerSelector = mkSelector "defaultManager"

-- | @Selector@ for @providerIdentifier@
providerIdentifierSelector :: Selector '[] (Id NSString)
providerIdentifierSelector = mkSelector "providerIdentifier"

-- | @Selector@ for @documentStorageURL@
documentStorageURLSelector :: Selector '[] (Id NSURL)
documentStorageURLSelector = mkSelector "documentStorageURL"

