{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Generated bindings for @NSFileProviderExtension@.
module ObjC.FileProvider.NSFileProviderExtension
  ( NSFileProviderExtension
  , IsNSFileProviderExtension(..)
  , urlForItemWithPersistentIdentifier
  , persistentIdentifierForItemAtURL
  , providePlaceholderAtURL_completionHandler
  , startProvidingItemAtURL_completionHandler
  , stopProvidingItemAtURL
  , itemChangedAtURL
  , supportedServiceSourcesForItemIdentifier_error
  , importDocumentAtURL_toParentItemIdentifier_completionHandler
  , createDirectoryWithName_inParentItemIdentifier_completionHandler
  , renameItemWithIdentifier_toName_completionHandler
  , reparentItemWithIdentifier_toParentItemWithIdentifier_newName_completionHandler
  , trashItemWithIdentifier_completionHandler
  , untrashItemWithIdentifier_toParentItemIdentifier_completionHandler
  , deleteItemWithIdentifier_completionHandler
  , setLastUsedDate_forItemIdentifier_completionHandler
  , setTagData_forItemIdentifier_completionHandler
  , setFavoriteRank_forItemIdentifier_completionHandler
  , enumeratorForContainerItemIdentifier_error
  , writePlaceholderAtURL_withMetadata_error
  , placeholderURLForURL
  , domain
  , urlForItemWithPersistentIdentifierSelector
  , persistentIdentifierForItemAtURLSelector
  , providePlaceholderAtURL_completionHandlerSelector
  , startProvidingItemAtURL_completionHandlerSelector
  , stopProvidingItemAtURLSelector
  , itemChangedAtURLSelector
  , supportedServiceSourcesForItemIdentifier_errorSelector
  , importDocumentAtURL_toParentItemIdentifier_completionHandlerSelector
  , createDirectoryWithName_inParentItemIdentifier_completionHandlerSelector
  , renameItemWithIdentifier_toName_completionHandlerSelector
  , reparentItemWithIdentifier_toParentItemWithIdentifier_newName_completionHandlerSelector
  , trashItemWithIdentifier_completionHandlerSelector
  , untrashItemWithIdentifier_toParentItemIdentifier_completionHandlerSelector
  , deleteItemWithIdentifier_completionHandlerSelector
  , setLastUsedDate_forItemIdentifier_completionHandlerSelector
  , setTagData_forItemIdentifier_completionHandlerSelector
  , setFavoriteRank_forItemIdentifier_completionHandlerSelector
  , enumeratorForContainerItemIdentifier_errorSelector
  , writePlaceholderAtURL_withMetadata_errorSelector
  , placeholderURLForURLSelector
  , domainSelector


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

import ObjC.FileProvider.Internal.Classes
import ObjC.Foundation.Internal.Classes

-- | Should return the URL corresponding to a specific identifier. Fail if it's not a subpath of documentStorageURL.
--
-- This is a static mapping; each identifier must always return a path corresponding to the same file. By default, this returns the path relative to the path returned by documentStorageURL.
--
-- ObjC selector: @- URLForItemWithPersistentIdentifier:@
urlForItemWithPersistentIdentifier :: (IsNSFileProviderExtension nsFileProviderExtension, IsNSString identifier) => nsFileProviderExtension -> identifier -> IO (Id NSURL)
urlForItemWithPersistentIdentifier nsFileProviderExtension  identifier =
withObjCPtr identifier $ \raw_identifier ->
    sendMsg nsFileProviderExtension (mkSelector "URLForItemWithPersistentIdentifier:") (retPtr retVoid) [argPtr (castPtr raw_identifier :: Ptr ())] >>= retainedObject . castPtr

-- | @- persistentIdentifierForItemAtURL:@
persistentIdentifierForItemAtURL :: (IsNSFileProviderExtension nsFileProviderExtension, IsNSURL url) => nsFileProviderExtension -> url -> IO (Id NSString)
persistentIdentifierForItemAtURL nsFileProviderExtension  url =
withObjCPtr url $ \raw_url ->
    sendMsg nsFileProviderExtension (mkSelector "persistentIdentifierForItemAtURL:") (retPtr retVoid) [argPtr (castPtr raw_url :: Ptr ())] >>= retainedObject . castPtr

-- | This method is called when a placeholder URL should be provided for the item at the given URL.
--
-- The implementation of this method should call +[NSFileProviderManager writePlaceholderAtURL:withMetadata:error:] with the URL returned by +[NSFileProviderManager placeholderURLForURL:], then call the completion handler.
--
-- ObjC selector: @- providePlaceholderAtURL:completionHandler:@
providePlaceholderAtURL_completionHandler :: (IsNSFileProviderExtension nsFileProviderExtension, IsNSURL url) => nsFileProviderExtension -> url -> Ptr () -> IO ()
providePlaceholderAtURL_completionHandler nsFileProviderExtension  url completionHandler =
withObjCPtr url $ \raw_url ->
    sendMsg nsFileProviderExtension (mkSelector "providePlaceholderAtURL:completionHandler:") retVoid [argPtr (castPtr raw_url :: Ptr ()), argPtr (castPtr completionHandler :: Ptr ())]

-- | Should ensure that the actual file is in the position returned by URLForItemWithPersistentIdentifier:, then call the completion handler.
--
-- ObjC selector: @- startProvidingItemAtURL:completionHandler:@
startProvidingItemAtURL_completionHandler :: (IsNSFileProviderExtension nsFileProviderExtension, IsNSURL url) => nsFileProviderExtension -> url -> Ptr () -> IO ()
startProvidingItemAtURL_completionHandler nsFileProviderExtension  url completionHandler =
withObjCPtr url $ \raw_url ->
    sendMsg nsFileProviderExtension (mkSelector "startProvidingItemAtURL:completionHandler:") retVoid [argPtr (castPtr raw_url :: Ptr ()), argPtr (castPtr completionHandler :: Ptr ())]

-- | Called after the last claim to the file has been released. At this point, it is safe for the file provider to remove the content file.
--
-- Care should be taken that the corresponding placeholder file stays behind after the content file has been deleted.
--
-- ObjC selector: @- stopProvidingItemAtURL:@
stopProvidingItemAtURL :: (IsNSFileProviderExtension nsFileProviderExtension, IsNSURL url) => nsFileProviderExtension -> url -> IO ()
stopProvidingItemAtURL nsFileProviderExtension  url =
withObjCPtr url $ \raw_url ->
    sendMsg nsFileProviderExtension (mkSelector "stopProvidingItemAtURL:") retVoid [argPtr (castPtr raw_url :: Ptr ())]

-- | Called at some point after the file has changed; the provider may then trigger an upload.
--
-- ObjC selector: @- itemChangedAtURL:@
itemChangedAtURL :: (IsNSFileProviderExtension nsFileProviderExtension, IsNSURL url) => nsFileProviderExtension -> url -> IO ()
itemChangedAtURL nsFileProviderExtension  url =
withObjCPtr url $ \raw_url ->
    sendMsg nsFileProviderExtension (mkSelector "itemChangedAtURL:") retVoid [argPtr (castPtr raw_url :: Ptr ())]

-- | @- supportedServiceSourcesForItemIdentifier:error:@
supportedServiceSourcesForItemIdentifier_error :: (IsNSFileProviderExtension nsFileProviderExtension, IsNSString itemIdentifier, IsNSError error_) => nsFileProviderExtension -> itemIdentifier -> error_ -> IO (Id NSArray)
supportedServiceSourcesForItemIdentifier_error nsFileProviderExtension  itemIdentifier error_ =
withObjCPtr itemIdentifier $ \raw_itemIdentifier ->
  withObjCPtr error_ $ \raw_error_ ->
      sendMsg nsFileProviderExtension (mkSelector "supportedServiceSourcesForItemIdentifier:error:") (retPtr retVoid) [argPtr (castPtr raw_itemIdentifier :: Ptr ()), argPtr (castPtr raw_error_ :: Ptr ())] >>= retainedObject . castPtr

-- | Import a document.
--
-- The file or package at fileURL should be moved on disk into the file provider's own storage, where it will later be uploaded.  The completion block should be executed after the move on disk, but before the upload.  Before doing any network, actually.
--
-- In the completion block, importedDocumentItem should have these properties set:   - itemIdentifier   - parentItemIdentifier (to the value passed in to this method)   - filename (to fileURL's resource value for key NSURLNameKey)   - creationDate (to NSURLCreationDateKey)   - contentModificationDate (to NSURLContentModificationDateKey)   - contentType (to NSURLContentTypeKey)   - documentSize (to NSURLTotalFileSizeKey on a flat file, or to the sum of the     files sizes for a package)   - capabilities
--
-- [fileURL startAccessingSecurityScopedResource] needs to be called prior to accessing this security scoped URL, and stopAccessingSecurityScopedResource needs to be called when done.
--
-- Note that itemIdentifier should be set with no network call.  It doesn't have to be the final identifier.  If the identifier changes after talking to the server then the file provider should send a delete for the temporary, local identifier immediately followed by an add with the final identifier.
--
-- A reasonable way of organizing files in the file provider storage is:     <file provider storage path>/<itemIdentifier>/<filename>.<extension> If the item identifier was to change, you should move the file on disk to update its path, under coordination with NSFileCoordinatorWritingForMoving.
--
-- This is expected to work offline even if there might be a collision (another item with the same filename and parentItemIdentifier) only detected when later syncing up this change to the server.  In that case, it is suggested that a follow up update to the item change its filename to something unique.  This wouldn't be considered an error to import.
--
-- If however you can tell right away, with no communication to your server, that there is a collision then this call should fail with error code NSFileProviderErrorFilenameCollision generated with this method:   -[NSError (NSFileProviderError) fileProviderErrorForCollisionWithItem:].
--
-- The existing item set in this error will be used to handle the collision, and ask the user if she or he would like to replace the existing item.  This takes into account the existing item's capabilities (particularly NSFileProviderItemCapabilitiesAllowsTrashing and AllowsDeleting.)
--
-- Collision checks should be case insensitive even if the filesystem or file provider might allow two coexisting filenames differing only by their case.
--
-- Upload errors (such as NSFileProviderErrorInsufficientQuota) should be handled with a subsequent update to the item, setting its uploadingError property. Upload errors should not prevent creating or importing a document, because they can be resolved at a later date (for example, when the user has quota again.)
--
-- Other errors will be presented to the user, but are unexpected.  If you want to prevent imports in a given directory, then the directory item's capacities should exclude NSFileProviderItemCapabilitiesAllowsAddingSubItems.
--
-- ObjC selector: @- importDocumentAtURL:toParentItemIdentifier:completionHandler:@
importDocumentAtURL_toParentItemIdentifier_completionHandler :: (IsNSFileProviderExtension nsFileProviderExtension, IsNSURL fileURL, IsNSString parentItemIdentifier) => nsFileProviderExtension -> fileURL -> parentItemIdentifier -> Ptr () -> IO ()
importDocumentAtURL_toParentItemIdentifier_completionHandler nsFileProviderExtension  fileURL parentItemIdentifier completionHandler =
withObjCPtr fileURL $ \raw_fileURL ->
  withObjCPtr parentItemIdentifier $ \raw_parentItemIdentifier ->
      sendMsg nsFileProviderExtension (mkSelector "importDocumentAtURL:toParentItemIdentifier:completionHandler:") retVoid [argPtr (castPtr raw_fileURL :: Ptr ()), argPtr (castPtr raw_parentItemIdentifier :: Ptr ()), argPtr (castPtr completionHandler :: Ptr ())]

-- | Create a directory.
--
-- This is expected to complete before doing any network.
--
-- In the completion block, createdDirectoryItem should have these properties set: - itemIdentifier - parentItemIdentifier (to the value passed in to this method) - filename (to directoryName) - creationDate (to the current date and time) - contentType (to "public.folder" - UTTypeFolder) - childItemCount (to 0) - capabilities
--
-- Errors (including collision errors) are handled as documented for the import method above.  Directory creation is gated by the capabilities of the destination directory, with NSFileProviderItemCapabilitiesAllowsAddingSubItems.
--
-- ObjC selector: @- createDirectoryWithName:inParentItemIdentifier:completionHandler:@
createDirectoryWithName_inParentItemIdentifier_completionHandler :: (IsNSFileProviderExtension nsFileProviderExtension, IsNSString directoryName, IsNSString parentItemIdentifier) => nsFileProviderExtension -> directoryName -> parentItemIdentifier -> Ptr () -> IO ()
createDirectoryWithName_inParentItemIdentifier_completionHandler nsFileProviderExtension  directoryName parentItemIdentifier completionHandler =
withObjCPtr directoryName $ \raw_directoryName ->
  withObjCPtr parentItemIdentifier $ \raw_parentItemIdentifier ->
      sendMsg nsFileProviderExtension (mkSelector "createDirectoryWithName:inParentItemIdentifier:completionHandler:") retVoid [argPtr (castPtr raw_directoryName :: Ptr ()), argPtr (castPtr raw_parentItemIdentifier :: Ptr ()), argPtr (castPtr completionHandler :: Ptr ())]

-- | Rename a document or a directory.
--
-- This is expected to complete before doing any network.
--
-- itemName is the full file or directory name, complete with its file extension. In the completion block, property renamedItem.filename should change to itemName.  Property renamedItem.displayName should also be updated if you chose to overwrite that method.
--
-- Errors (including collision errors) are handled as documented for the import method above.  Renames are gated by the capabilities of the renamed item, with NSFileProviderItemCapabilitiesAllowsRenaming.
--
-- ObjC selector: @- renameItemWithIdentifier:toName:completionHandler:@
renameItemWithIdentifier_toName_completionHandler :: (IsNSFileProviderExtension nsFileProviderExtension, IsNSString itemIdentifier, IsNSString itemName) => nsFileProviderExtension -> itemIdentifier -> itemName -> Ptr () -> IO ()
renameItemWithIdentifier_toName_completionHandler nsFileProviderExtension  itemIdentifier itemName completionHandler =
withObjCPtr itemIdentifier $ \raw_itemIdentifier ->
  withObjCPtr itemName $ \raw_itemName ->
      sendMsg nsFileProviderExtension (mkSelector "renameItemWithIdentifier:toName:completionHandler:") retVoid [argPtr (castPtr raw_itemIdentifier :: Ptr ()), argPtr (castPtr raw_itemName :: Ptr ()), argPtr (castPtr completionHandler :: Ptr ())]

-- | Move an item to a new directory.
--
-- This is expected to complete before doing any network.
--
-- If newName is non null, the moved item should be renamed to newName.
--
-- Errors (including collision errors) are handled as documented for the import method above.  Moves are gated by the capabilities of both the moved item with NSFileProviderItemCapabilitiesAllowsReparenting, and the destination directory with NSFileProviderItemCapabilitiesAllowsAddingSubItems.
--
-- ObjC selector: @- reparentItemWithIdentifier:toParentItemWithIdentifier:newName:completionHandler:@
reparentItemWithIdentifier_toParentItemWithIdentifier_newName_completionHandler :: (IsNSFileProviderExtension nsFileProviderExtension, IsNSString itemIdentifier, IsNSString parentItemIdentifier, IsNSString newName) => nsFileProviderExtension -> itemIdentifier -> parentItemIdentifier -> newName -> Ptr () -> IO ()
reparentItemWithIdentifier_toParentItemWithIdentifier_newName_completionHandler nsFileProviderExtension  itemIdentifier parentItemIdentifier newName completionHandler =
withObjCPtr itemIdentifier $ \raw_itemIdentifier ->
  withObjCPtr parentItemIdentifier $ \raw_parentItemIdentifier ->
    withObjCPtr newName $ \raw_newName ->
        sendMsg nsFileProviderExtension (mkSelector "reparentItemWithIdentifier:toParentItemWithIdentifier:newName:completionHandler:") retVoid [argPtr (castPtr raw_itemIdentifier :: Ptr ()), argPtr (castPtr raw_parentItemIdentifier :: Ptr ()), argPtr (castPtr raw_newName :: Ptr ()), argPtr (castPtr completionHandler :: Ptr ())]

-- | Move an item to the trash.
--
-- In the completion block, property trashedItem.isTrashed should be set to YES. You should keep track of the original parentItemIdentifier of the item before it was moved to trash, so you can move the item back where it used to be in the untrash method.  You could use the trashedItem.parentItemIdentifier property for that.
--
-- The trashed item should continue to belong to the working set of documents cached on the device and visible offline to the user.  But if it is a directory, then all of its children should be removed from the working set and the file provider extension should send deletion events to make sure that they no longer appear in the recent lists.
--
-- It is also suggested that shared documents be unshared when trashed.
--
-- Trash is gated by the capabilities of the trashed item with NSFileProviderItemCapabilitiesAllowsTrashing.
--
-- ObjC selector: @- trashItemWithIdentifier:completionHandler:@
trashItemWithIdentifier_completionHandler :: (IsNSFileProviderExtension nsFileProviderExtension, IsNSString itemIdentifier) => nsFileProviderExtension -> itemIdentifier -> Ptr () -> IO ()
trashItemWithIdentifier_completionHandler nsFileProviderExtension  itemIdentifier completionHandler =
withObjCPtr itemIdentifier $ \raw_itemIdentifier ->
    sendMsg nsFileProviderExtension (mkSelector "trashItemWithIdentifier:completionHandler:") retVoid [argPtr (castPtr raw_itemIdentifier :: Ptr ()), argPtr (castPtr completionHandler :: Ptr ())]

-- | Move an item out of the trash.
--
-- If parentItemIdentifier is nil, the file provider should attempt to put the item back where it was when it was moved to the trash; otherwise it should move it to this new parent.
--
-- If the item is a directory, then the file provider extension should enumerate the children and send addition events in the working set so that the documents in the now untrashed directory may be reindexed.
--
-- Untrash is gated by the capabilities of the destination directory, with NSFileProviderItemCapabilitiesAllowsAddingSubItems.
--
-- ObjC selector: @- untrashItemWithIdentifier:toParentItemIdentifier:completionHandler:@
untrashItemWithIdentifier_toParentItemIdentifier_completionHandler :: (IsNSFileProviderExtension nsFileProviderExtension, IsNSString itemIdentifier, IsNSString parentItemIdentifier) => nsFileProviderExtension -> itemIdentifier -> parentItemIdentifier -> Ptr () -> IO ()
untrashItemWithIdentifier_toParentItemIdentifier_completionHandler nsFileProviderExtension  itemIdentifier parentItemIdentifier completionHandler =
withObjCPtr itemIdentifier $ \raw_itemIdentifier ->
  withObjCPtr parentItemIdentifier $ \raw_parentItemIdentifier ->
      sendMsg nsFileProviderExtension (mkSelector "untrashItemWithIdentifier:toParentItemIdentifier:completionHandler:") retVoid [argPtr (castPtr raw_itemIdentifier :: Ptr ()), argPtr (castPtr raw_parentItemIdentifier :: Ptr ()), argPtr (castPtr completionHandler :: Ptr ())]

-- | Delete an item forever.
--
-- This is called when the user deletes an item that was already in the Trash and the item should no longer appear there after this call.  This call should remove the item from the working set.
--
-- Delete is gated by the capabilities of the removed item with NSFileProviderItemCapabilitiesAllowsDeleting.
--
-- ObjC selector: @- deleteItemWithIdentifier:completionHandler:@
deleteItemWithIdentifier_completionHandler :: (IsNSFileProviderExtension nsFileProviderExtension, IsNSString itemIdentifier) => nsFileProviderExtension -> itemIdentifier -> Ptr () -> IO ()
deleteItemWithIdentifier_completionHandler nsFileProviderExtension  itemIdentifier completionHandler =
withObjCPtr itemIdentifier $ \raw_itemIdentifier ->
    sendMsg nsFileProviderExtension (mkSelector "deleteItemWithIdentifier:completionHandler:") retVoid [argPtr (castPtr raw_itemIdentifier :: Ptr ()), argPtr (castPtr completionHandler :: Ptr ())]

-- | Mark an item as recently used, or clear its lastUsedDate if nil.
--
-- This last used date is the sort key for the recent lists.  It is the primary hint that an item must belong to the working set cached and indexed on the user's device.
--
-- The file provider is asked to persist the new last used date on disk, then call the completion callback with the updated last used date.  At a later point, the file provider should sync the new last used date to their server.
--
-- The error parameter is here for debugging purposes alone; it won't be presented to the user or otherwise handled, but it will be logged.
--
-- ObjC selector: @- setLastUsedDate:forItemIdentifier:completionHandler:@
setLastUsedDate_forItemIdentifier_completionHandler :: (IsNSFileProviderExtension nsFileProviderExtension, IsNSDate lastUsedDate, IsNSString itemIdentifier) => nsFileProviderExtension -> lastUsedDate -> itemIdentifier -> Ptr () -> IO ()
setLastUsedDate_forItemIdentifier_completionHandler nsFileProviderExtension  lastUsedDate itemIdentifier completionHandler =
withObjCPtr lastUsedDate $ \raw_lastUsedDate ->
  withObjCPtr itemIdentifier $ \raw_itemIdentifier ->
      sendMsg nsFileProviderExtension (mkSelector "setLastUsedDate:forItemIdentifier:completionHandler:") retVoid [argPtr (castPtr raw_lastUsedDate :: Ptr ()), argPtr (castPtr raw_itemIdentifier :: Ptr ()), argPtr (castPtr completionHandler :: Ptr ())]

-- | Tag an item, or untag it if tagData is nil.
--
-- The file provider is asked to persist the new tag data on disk, then call the completion callback with the updated tagged data.  At a later point, the file provider should sync the new tag data to their server.
--
-- Tagged items are relevant to the user and should be in the working set even if they haven't been used recently.
--
-- If set, the error will be immediately presented to the user and the item just won't be tagged.
--
-- On shared items, tags should sync across the devices of any one participant but shouldn't sync across users.
--
-- ObjC selector: @- setTagData:forItemIdentifier:completionHandler:@
setTagData_forItemIdentifier_completionHandler :: (IsNSFileProviderExtension nsFileProviderExtension, IsNSData tagData, IsNSString itemIdentifier) => nsFileProviderExtension -> tagData -> itemIdentifier -> Ptr () -> IO ()
setTagData_forItemIdentifier_completionHandler nsFileProviderExtension  tagData itemIdentifier completionHandler =
withObjCPtr tagData $ \raw_tagData ->
  withObjCPtr itemIdentifier $ \raw_itemIdentifier ->
      sendMsg nsFileProviderExtension (mkSelector "setTagData:forItemIdentifier:completionHandler:") retVoid [argPtr (castPtr raw_tagData :: Ptr ()), argPtr (castPtr raw_itemIdentifier :: Ptr ()), argPtr (castPtr completionHandler :: Ptr ())]

-- | Mark a directory as favorite (or no longer favorite if favoriteRank is nil.)
--
-- The favorite rank is used to represent the relative order of two favorite directories in the UI.  It is a 64 bit unsigned integer.  It needs to be synced.
--
-- Favorite directories are relevant to the user and should be in the working set even if they haven't been used recently.  The documents and directories in the favorite directory however don't all have to be in the working set, and don't all have to be made accessible offline.
--
-- The file provider is asked to persist the new favorite rank on disk, then call the completion callback with the updated favorite rank.  At a later point, the file provider should sync the new favorite rank to their server.
--
-- ObjC selector: @- setFavoriteRank:forItemIdentifier:completionHandler:@
setFavoriteRank_forItemIdentifier_completionHandler :: (IsNSFileProviderExtension nsFileProviderExtension, IsNSNumber favoriteRank, IsNSString itemIdentifier) => nsFileProviderExtension -> favoriteRank -> itemIdentifier -> Ptr () -> IO ()
setFavoriteRank_forItemIdentifier_completionHandler nsFileProviderExtension  favoriteRank itemIdentifier completionHandler =
withObjCPtr favoriteRank $ \raw_favoriteRank ->
  withObjCPtr itemIdentifier $ \raw_itemIdentifier ->
      sendMsg nsFileProviderExtension (mkSelector "setFavoriteRank:forItemIdentifier:completionHandler:") retVoid [argPtr (castPtr raw_favoriteRank :: Ptr ()), argPtr (castPtr raw_itemIdentifier :: Ptr ()), argPtr (castPtr completionHandler :: Ptr ())]

-- | Create an enumerator for an item.
--
-- When the user opens the browse tab of the UIDocumentsBrowserViewController and selects a file provider, this is called with NSFileProviderRootContainerItemIdentifier, and -[NSFileProviderEnumerator enumerateItemsForObserver:startingAtPage:] is immediately called to list the first items available under at the root level of the file provider.
--
-- As the user navigates down into directories, new enumerators are created with this method, passing in the itemIdentifier of those directories.  Past enumerators are then invalidated.
--
-- This method is also called with NSFileProviderWorkingSetContainerItemIdentifier, which is enumerated with -[NSFileProviderEnumerator enumerateChangesForObserver:fromSyncAnchor:].  That enumeration is special in that it isn't driven by the UIDocumentsBrowserViewController.  It happens in the background to sync the working set down to the device.
--
-- This is also used to subscribe to live updates for a single document.  In that case, -[NSFileProviderEnumerator enumerateChangesToObserver:fromSyncAnchor:] will be called and the enumeration results shouldn't include items other than the very item that the enumeration was started on.
--
-- If returning nil, you must set the error out parameter.
--
-- ObjC selector: @- enumeratorForContainerItemIdentifier:error:@
enumeratorForContainerItemIdentifier_error :: (IsNSFileProviderExtension nsFileProviderExtension, IsNSString containerItemIdentifier, IsNSError error_) => nsFileProviderExtension -> containerItemIdentifier -> error_ -> IO RawId
enumeratorForContainerItemIdentifier_error nsFileProviderExtension  containerItemIdentifier error_ =
withObjCPtr containerItemIdentifier $ \raw_containerItemIdentifier ->
  withObjCPtr error_ $ \raw_error_ ->
      fmap (RawId . castPtr) $ sendMsg nsFileProviderExtension (mkSelector "enumeratorForContainerItemIdentifier:error:") (retPtr retVoid) [argPtr (castPtr raw_containerItemIdentifier :: Ptr ()), argPtr (castPtr raw_error_ :: Ptr ())]

-- | Writes out a placeholder at the specified URL. The URL should be one returned by placeholderURLForURL:; if URL resource values are requested, the system will consult the placeholder before consulting your app extension.
--
-- Metadata contains NSURLNameKey, NSURLFileSizeKey, NSURLIsPackageKey.
--
-- ObjC selector: @+ writePlaceholderAtURL:withMetadata:error:@
writePlaceholderAtURL_withMetadata_error :: (IsNSURL placeholderURL, IsNSDictionary metadata, IsNSError error_) => placeholderURL -> metadata -> error_ -> IO Bool
writePlaceholderAtURL_withMetadata_error placeholderURL metadata error_ =
  do
    cls' <- getRequiredClass "NSFileProviderExtension"
    withObjCPtr placeholderURL $ \raw_placeholderURL ->
      withObjCPtr metadata $ \raw_metadata ->
        withObjCPtr error_ $ \raw_error_ ->
          fmap ((/= 0) :: CULong -> Bool) $ sendClassMsg cls' (mkSelector "writePlaceholderAtURL:withMetadata:error:") retCULong [argPtr (castPtr raw_placeholderURL :: Ptr ()), argPtr (castPtr raw_metadata :: Ptr ()), argPtr (castPtr raw_error_ :: Ptr ())]

-- | Returns the designated placeholder URL for a given URL. This placeholder will be consulted before falling back to your app extension to enhance performance. To write out a placeholder, use the writePlaceHolderAtURL: method above.
--
-- ObjC selector: @+ placeholderURLForURL:@
placeholderURLForURL :: IsNSURL url => url -> IO (Id NSURL)
placeholderURLForURL url =
  do
    cls' <- getRequiredClass "NSFileProviderExtension"
    withObjCPtr url $ \raw_url ->
      sendClassMsg cls' (mkSelector "placeholderURLForURL:") (retPtr retVoid) [argPtr (castPtr raw_url :: Ptr ())] >>= retainedObject . castPtr

-- | @- domain@
domain :: IsNSFileProviderExtension nsFileProviderExtension => nsFileProviderExtension -> IO (Id NSFileProviderDomain)
domain nsFileProviderExtension  =
  sendMsg nsFileProviderExtension (mkSelector "domain") (retPtr retVoid) [] >>= retainedObject . castPtr

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @URLForItemWithPersistentIdentifier:@
urlForItemWithPersistentIdentifierSelector :: Selector
urlForItemWithPersistentIdentifierSelector = mkSelector "URLForItemWithPersistentIdentifier:"

-- | @Selector@ for @persistentIdentifierForItemAtURL:@
persistentIdentifierForItemAtURLSelector :: Selector
persistentIdentifierForItemAtURLSelector = mkSelector "persistentIdentifierForItemAtURL:"

-- | @Selector@ for @providePlaceholderAtURL:completionHandler:@
providePlaceholderAtURL_completionHandlerSelector :: Selector
providePlaceholderAtURL_completionHandlerSelector = mkSelector "providePlaceholderAtURL:completionHandler:"

-- | @Selector@ for @startProvidingItemAtURL:completionHandler:@
startProvidingItemAtURL_completionHandlerSelector :: Selector
startProvidingItemAtURL_completionHandlerSelector = mkSelector "startProvidingItemAtURL:completionHandler:"

-- | @Selector@ for @stopProvidingItemAtURL:@
stopProvidingItemAtURLSelector :: Selector
stopProvidingItemAtURLSelector = mkSelector "stopProvidingItemAtURL:"

-- | @Selector@ for @itemChangedAtURL:@
itemChangedAtURLSelector :: Selector
itemChangedAtURLSelector = mkSelector "itemChangedAtURL:"

-- | @Selector@ for @supportedServiceSourcesForItemIdentifier:error:@
supportedServiceSourcesForItemIdentifier_errorSelector :: Selector
supportedServiceSourcesForItemIdentifier_errorSelector = mkSelector "supportedServiceSourcesForItemIdentifier:error:"

-- | @Selector@ for @importDocumentAtURL:toParentItemIdentifier:completionHandler:@
importDocumentAtURL_toParentItemIdentifier_completionHandlerSelector :: Selector
importDocumentAtURL_toParentItemIdentifier_completionHandlerSelector = mkSelector "importDocumentAtURL:toParentItemIdentifier:completionHandler:"

-- | @Selector@ for @createDirectoryWithName:inParentItemIdentifier:completionHandler:@
createDirectoryWithName_inParentItemIdentifier_completionHandlerSelector :: Selector
createDirectoryWithName_inParentItemIdentifier_completionHandlerSelector = mkSelector "createDirectoryWithName:inParentItemIdentifier:completionHandler:"

-- | @Selector@ for @renameItemWithIdentifier:toName:completionHandler:@
renameItemWithIdentifier_toName_completionHandlerSelector :: Selector
renameItemWithIdentifier_toName_completionHandlerSelector = mkSelector "renameItemWithIdentifier:toName:completionHandler:"

-- | @Selector@ for @reparentItemWithIdentifier:toParentItemWithIdentifier:newName:completionHandler:@
reparentItemWithIdentifier_toParentItemWithIdentifier_newName_completionHandlerSelector :: Selector
reparentItemWithIdentifier_toParentItemWithIdentifier_newName_completionHandlerSelector = mkSelector "reparentItemWithIdentifier:toParentItemWithIdentifier:newName:completionHandler:"

-- | @Selector@ for @trashItemWithIdentifier:completionHandler:@
trashItemWithIdentifier_completionHandlerSelector :: Selector
trashItemWithIdentifier_completionHandlerSelector = mkSelector "trashItemWithIdentifier:completionHandler:"

-- | @Selector@ for @untrashItemWithIdentifier:toParentItemIdentifier:completionHandler:@
untrashItemWithIdentifier_toParentItemIdentifier_completionHandlerSelector :: Selector
untrashItemWithIdentifier_toParentItemIdentifier_completionHandlerSelector = mkSelector "untrashItemWithIdentifier:toParentItemIdentifier:completionHandler:"

-- | @Selector@ for @deleteItemWithIdentifier:completionHandler:@
deleteItemWithIdentifier_completionHandlerSelector :: Selector
deleteItemWithIdentifier_completionHandlerSelector = mkSelector "deleteItemWithIdentifier:completionHandler:"

-- | @Selector@ for @setLastUsedDate:forItemIdentifier:completionHandler:@
setLastUsedDate_forItemIdentifier_completionHandlerSelector :: Selector
setLastUsedDate_forItemIdentifier_completionHandlerSelector = mkSelector "setLastUsedDate:forItemIdentifier:completionHandler:"

-- | @Selector@ for @setTagData:forItemIdentifier:completionHandler:@
setTagData_forItemIdentifier_completionHandlerSelector :: Selector
setTagData_forItemIdentifier_completionHandlerSelector = mkSelector "setTagData:forItemIdentifier:completionHandler:"

-- | @Selector@ for @setFavoriteRank:forItemIdentifier:completionHandler:@
setFavoriteRank_forItemIdentifier_completionHandlerSelector :: Selector
setFavoriteRank_forItemIdentifier_completionHandlerSelector = mkSelector "setFavoriteRank:forItemIdentifier:completionHandler:"

-- | @Selector@ for @enumeratorForContainerItemIdentifier:error:@
enumeratorForContainerItemIdentifier_errorSelector :: Selector
enumeratorForContainerItemIdentifier_errorSelector = mkSelector "enumeratorForContainerItemIdentifier:error:"

-- | @Selector@ for @writePlaceholderAtURL:withMetadata:error:@
writePlaceholderAtURL_withMetadata_errorSelector :: Selector
writePlaceholderAtURL_withMetadata_errorSelector = mkSelector "writePlaceholderAtURL:withMetadata:error:"

-- | @Selector@ for @placeholderURLForURL:@
placeholderURLForURLSelector :: Selector
placeholderURLForURLSelector = mkSelector "placeholderURLForURL:"

-- | @Selector@ for @domain@
domainSelector :: Selector
domainSelector = mkSelector "domain"

