{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | A class that manages asset packs.
--
-- The first time that your code refers to the shared manager, Background Assets considers that your app is opting into automatic system management of your asset packs. - Important: When using the asset-pack manager, make sure that you also adopt the corresponding managed extension protocol. For apps that use Apple hosting, the corresponding protocol is @SKDownloaderExtension@ from StoreKit. For other apps, the corresponding protocol is ``BAManagedDownloaderExtension``. Not adopting the right protocol is a programmer error.
--
-- Generated bindings for @BAAssetPackManager@.
module ObjC.BackgroundAssets.BAAssetPackManager
  ( BAAssetPackManager
  , IsBAAssetPackManager(..)
  , init_
  , new
  , getAssetPackWithIdentifier_completionHandler
  , getStatusOfAssetPackWithIdentifier_completionHandler
  , ensureLocalAvailabilityOfAssetPack_completionHandler
  , contentsAtPath_searchingInAssetPackWithIdentifier_options_error
  , fileDescriptorForPath_searchingInAssetPackWithIdentifier_error
  , urlForPath_error
  , removeAssetPackWithIdentifier_completionHandler
  , initSelector
  , newSelector
  , getAssetPackWithIdentifier_completionHandlerSelector
  , getStatusOfAssetPackWithIdentifier_completionHandlerSelector
  , ensureLocalAvailabilityOfAssetPack_completionHandlerSelector
  , contentsAtPath_searchingInAssetPackWithIdentifier_options_errorSelector
  , fileDescriptorForPath_searchingInAssetPackWithIdentifier_errorSelector
  , urlForPath_errorSelector
  , removeAssetPackWithIdentifier_completionHandlerSelector

  -- * Enum types
  , NSDataReadingOptions(NSDataReadingOptions)
  , pattern NSDataReadingMappedIfSafe
  , pattern NSDataReadingUncached
  , pattern NSDataReadingMappedAlways
  , pattern NSDataReadingMapped
  , pattern NSMappedRead
  , pattern NSUncachedRead

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
import ObjC.Foundation.Internal.Enums
import ObjC.Foundation.Internal.Classes

-- | @- init@
init_ :: IsBAAssetPackManager baAssetPackManager => baAssetPackManager -> IO (Id BAAssetPackManager)
init_ baAssetPackManager  =
  sendMsg baAssetPackManager (mkSelector "init") (retPtr retVoid) [] >>= ownedObject . castPtr

-- | @+ new@
new :: IO (Id BAAssetPackManager)
new  =
  do
    cls' <- getRequiredClass "BAAssetPackManager"
    sendClassMsg cls' (mkSelector "new") (retPtr retVoid) [] >>= ownedObject . castPtr

-- | Gets the asset pack with the given identifier.
--
-- If no asset pack with the given identifier is found, then the block will receive an @NSError@ object with ``BAManagedErrorCode/BAManagedErrorCodeAssetPackNotFound`` as its code for the @error@ parameter. This method might attempt to get the latest asset-pack information from the server. - Parameters:   - assetPackIdentifier: The asset pack’s identifier.   - completionHandler: A block that receives the asset pack or an error if one occurs.
--
-- ObjC selector: @- getAssetPackWithIdentifier:completionHandler:@
getAssetPackWithIdentifier_completionHandler :: (IsBAAssetPackManager baAssetPackManager, IsNSString assetPackIdentifier) => baAssetPackManager -> assetPackIdentifier -> Ptr () -> IO ()
getAssetPackWithIdentifier_completionHandler baAssetPackManager  assetPackIdentifier completionHandler =
withObjCPtr assetPackIdentifier $ \raw_assetPackIdentifier ->
    sendMsg baAssetPackManager (mkSelector "getAssetPackWithIdentifier:completionHandler:") retVoid [argPtr (castPtr raw_assetPackIdentifier :: Ptr ()), argPtr (castPtr completionHandler :: Ptr ())]

-- | Gets the status of the asset pack with the specified identifier.
--
-- If no asset pack with the specified identifier is found, then the block will receive an @NSError@ object with ``BAManagedErrorCode/BAManagedErrorCodeAssetPackNotFound`` as its code for the @error@ parameter. This method attempts to get the latest asset-pack information from the server. No updates or removals are automatically triggered. - Parameters:   - assetPackIdentifier: The asset pack’s identifier.   - completionHandler: A block that receives the status of the asset pack or an error if one occurs.
--
-- ObjC selector: @- getStatusOfAssetPackWithIdentifier:completionHandler:@
getStatusOfAssetPackWithIdentifier_completionHandler :: (IsBAAssetPackManager baAssetPackManager, IsNSString assetPackIdentifier) => baAssetPackManager -> assetPackIdentifier -> Ptr () -> IO ()
getStatusOfAssetPackWithIdentifier_completionHandler baAssetPackManager  assetPackIdentifier completionHandler =
withObjCPtr assetPackIdentifier $ \raw_assetPackIdentifier ->
    sendMsg baAssetPackManager (mkSelector "getStatusOfAssetPackWithIdentifier:completionHandler:") retVoid [argPtr (castPtr raw_assetPackIdentifier :: Ptr ()), argPtr (castPtr completionHandler :: Ptr ())]

-- | Ensures that the specified asset pack be available locally.
--
-- This method checks if the asset pack is currently downloaded. If it isn’t, then it schedules it to be downloaded and calls the block with @nil@ for the block’s @error@ parameter when the download completes. It’s guaranteed that the requested asset pack will be available locally once the block is called with @nil@ for its @error@ parameter. If a non-@nil@ value is provided to the block’s @error@ parameter, then the asset pack is **not** guaranteed to be available locally. You can optionally monitor download progress by attaching a delegate object to @delegate@. - Parameters:   - assetPack: The asset pack the local availability of which to ensure.   - completionHandler: A block that’s called when the asset pack is available locally or that receives an error if one occurs.
--
-- ObjC selector: @- ensureLocalAvailabilityOfAssetPack:completionHandler:@
ensureLocalAvailabilityOfAssetPack_completionHandler :: (IsBAAssetPackManager baAssetPackManager, IsBAAssetPack assetPack) => baAssetPackManager -> assetPack -> Ptr () -> IO ()
ensureLocalAvailabilityOfAssetPack_completionHandler baAssetPackManager  assetPack completionHandler =
withObjCPtr assetPack $ \raw_assetPack ->
    sendMsg baAssetPackManager (mkSelector "ensureLocalAvailabilityOfAssetPack:completionHandler:") retVoid [argPtr (castPtr raw_assetPack :: Ptr ()), argPtr (castPtr completionHandler :: Ptr ())]

-- | Returns the contents of an asset file at the specified relative path.
--
-- All asset packs share the same namespace, so you can treat the overall collection of downloaded asset packs as if it were a single root directory that contains all of your subdirectories and asset files, regardless of the specific asset pack in which any particular file resides. If there’s a file-path collision across multiple asset packs, then it’s undefined from which asset pack the file will be read unless you explicitly limit the search to a particular asset pack by passing a non-@nil@ identifier to the @assetPackIdentifier@ parameter. - Parameters:   - path: The relative file path.   - assetPackIdentifier: The identifier of the asset pack in which you want to search for the file or @nil@ if you want to search in all asset packs.   - options: Options for how to read the contents of the file into a data object.   - error: A pointer to an error that will be set if an error occurs. If no file is found at @path@, then @error@ will point to an @NSError@ object with ``BAManagedErrorCode/BAManagedErrorCodeFileNotFound`` as its code. - Returns: The file’s contents.
--
-- ObjC selector: @- contentsAtPath:searchingInAssetPackWithIdentifier:options:error:@
contentsAtPath_searchingInAssetPackWithIdentifier_options_error :: (IsBAAssetPackManager baAssetPackManager, IsNSString path, IsNSString assetPackIdentifier, IsNSError error_) => baAssetPackManager -> path -> assetPackIdentifier -> NSDataReadingOptions -> error_ -> IO (Id NSData)
contentsAtPath_searchingInAssetPackWithIdentifier_options_error baAssetPackManager  path assetPackIdentifier options error_ =
withObjCPtr path $ \raw_path ->
  withObjCPtr assetPackIdentifier $ \raw_assetPackIdentifier ->
    withObjCPtr error_ $ \raw_error_ ->
        sendMsg baAssetPackManager (mkSelector "contentsAtPath:searchingInAssetPackWithIdentifier:options:error:") (retPtr retVoid) [argPtr (castPtr raw_path :: Ptr ()), argPtr (castPtr raw_assetPackIdentifier :: Ptr ()), argCULong (coerce options), argPtr (castPtr raw_error_ :: Ptr ())] >>= retainedObject . castPtr

-- | Opens and returns a file descriptor for the asset file at the specified relative path.
--
-- All asset packs share the same namespace, so you can treat the overall collection of downloaded asset packs as if it were a single root directory that contains all of your subdirectories and asset files, regardless of the specific asset pack in which any particular file resides. If there’s a file-path collision across multiple asset packs, then it’s undefined from which asset pack the file will be opened unless you explicitly limit the search to a particular asset pack by passing a non-@nil@ identifier to the @assetPackIdentifier@ parameter. A return value of @-1@ indicates that an error occurred. - Parameters:   - path: The relative file path.   - assetPackIdentifier: The identifier of the asset pack in which you want to search for the file or @nil@ if you want to search in all asset packs.   - error: A pointer to an error that will be set if an error occurs. If no file is found at @path@, then it will point to an @NSError@ object with ``BAManagedErrorCode/BAManagedErrorCodeFileNotFound`` as its code. - Returns: A descriptor for the opened file. - Important: It’s your responsibility to close the file descriptor when you’re done using it. - Remark: Use this method if you need low-level access to the file descriptor. If you don’t, then use ``BAAssetPackManager/contentsAtPath:searchingInAssetPackWithIdentifier:options:error:`` instead.
--
-- ObjC selector: @- fileDescriptorForPath:searchingInAssetPackWithIdentifier:error:@
fileDescriptorForPath_searchingInAssetPackWithIdentifier_error :: (IsBAAssetPackManager baAssetPackManager, IsNSString path, IsNSString assetPackIdentifier, IsNSError error_) => baAssetPackManager -> path -> assetPackIdentifier -> error_ -> IO CInt
fileDescriptorForPath_searchingInAssetPackWithIdentifier_error baAssetPackManager  path assetPackIdentifier error_ =
withObjCPtr path $ \raw_path ->
  withObjCPtr assetPackIdentifier $ \raw_assetPackIdentifier ->
    withObjCPtr error_ $ \raw_error_ ->
        sendMsg baAssetPackManager (mkSelector "fileDescriptorForPath:searchingInAssetPackWithIdentifier:error:") retCInt [argPtr (castPtr raw_path :: Ptr ()), argPtr (castPtr raw_assetPackIdentifier :: Ptr ()), argPtr (castPtr raw_error_ :: Ptr ())]

-- | Returns a URL for the specified relative path.
--
-- All asset packs share the same namespace, so you can treat the overall collection of downloaded asset packs as if it were a single root directory that contains all of your subdirectories and asset files, regardless of the specific asset pack in which any particular file resides. Unlike ``BAAssetPackManager/contentsAtPath:searchingInAssetPackWithIdentifier:options:error:`` and ``BAAssetPackManager/fileDescriptorForPath:searchingInAssetPackWithIdentifier:error:``, this method supports retrieving entire directories—including packages—in which case it merges the corresponding slices of the shared logical directory from all downloaded asset packs that contain such slices. If there’s a file-path collision across multiple asset packs, then it’s undefined from which asset pack an individual file will be resolved. - Parameters:   - path: The relative file path.   - error: A pointer to an error that will be set if an error occurs. - Warning: Don’t persist the returned URL beyond the lifetime of the current process. - Warning: This method is less efficient than are ``BAAssetPackManager/contentsAtPath:searchingInAssetPackWithIdentifier:options:error:`` and ``BAAssetPackManager/fileDescriptorForPath:searchingInAssetPackWithIdentifier:error:``; use those methods instead if you can do so. In particular, this method shouldn’t be used to get the URL to the root of the shared asset-pack namespace. Don’t use this method to block the main thread. - Note: This method will return a well formed URL even if no item exists at the specified relative path in any asset pack, in which case any attempts to get its contents—whether it’s a file or a directory—will fail.
--
-- ObjC selector: @- URLForPath:error:@
urlForPath_error :: (IsBAAssetPackManager baAssetPackManager, IsNSString path, IsNSError error_) => baAssetPackManager -> path -> error_ -> IO (Id NSURL)
urlForPath_error baAssetPackManager  path error_ =
withObjCPtr path $ \raw_path ->
  withObjCPtr error_ $ \raw_error_ ->
      sendMsg baAssetPackManager (mkSelector "URLForPath:error:") (retPtr retVoid) [argPtr (castPtr raw_path :: Ptr ()), argPtr (castPtr raw_error_ :: Ptr ())] >>= retainedObject . castPtr

-- | Removes the specified asset pack from the device. - Parameters:   - assetPackIdentifier: The asset pack’s identifier.   - completionHandler: A block that receives an error if one occurs.
--
-- ObjC selector: @- removeAssetPackWithIdentifier:completionHandler:@
removeAssetPackWithIdentifier_completionHandler :: (IsBAAssetPackManager baAssetPackManager, IsNSString assetPackIdentifier) => baAssetPackManager -> assetPackIdentifier -> Ptr () -> IO ()
removeAssetPackWithIdentifier_completionHandler baAssetPackManager  assetPackIdentifier completionHandler =
withObjCPtr assetPackIdentifier $ \raw_assetPackIdentifier ->
    sendMsg baAssetPackManager (mkSelector "removeAssetPackWithIdentifier:completionHandler:") retVoid [argPtr (castPtr raw_assetPackIdentifier :: Ptr ()), argPtr (castPtr completionHandler :: Ptr ())]

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @init@
initSelector :: Selector
initSelector = mkSelector "init"

-- | @Selector@ for @new@
newSelector :: Selector
newSelector = mkSelector "new"

-- | @Selector@ for @getAssetPackWithIdentifier:completionHandler:@
getAssetPackWithIdentifier_completionHandlerSelector :: Selector
getAssetPackWithIdentifier_completionHandlerSelector = mkSelector "getAssetPackWithIdentifier:completionHandler:"

-- | @Selector@ for @getStatusOfAssetPackWithIdentifier:completionHandler:@
getStatusOfAssetPackWithIdentifier_completionHandlerSelector :: Selector
getStatusOfAssetPackWithIdentifier_completionHandlerSelector = mkSelector "getStatusOfAssetPackWithIdentifier:completionHandler:"

-- | @Selector@ for @ensureLocalAvailabilityOfAssetPack:completionHandler:@
ensureLocalAvailabilityOfAssetPack_completionHandlerSelector :: Selector
ensureLocalAvailabilityOfAssetPack_completionHandlerSelector = mkSelector "ensureLocalAvailabilityOfAssetPack:completionHandler:"

-- | @Selector@ for @contentsAtPath:searchingInAssetPackWithIdentifier:options:error:@
contentsAtPath_searchingInAssetPackWithIdentifier_options_errorSelector :: Selector
contentsAtPath_searchingInAssetPackWithIdentifier_options_errorSelector = mkSelector "contentsAtPath:searchingInAssetPackWithIdentifier:options:error:"

-- | @Selector@ for @fileDescriptorForPath:searchingInAssetPackWithIdentifier:error:@
fileDescriptorForPath_searchingInAssetPackWithIdentifier_errorSelector :: Selector
fileDescriptorForPath_searchingInAssetPackWithIdentifier_errorSelector = mkSelector "fileDescriptorForPath:searchingInAssetPackWithIdentifier:error:"

-- | @Selector@ for @URLForPath:error:@
urlForPath_errorSelector :: Selector
urlForPath_errorSelector = mkSelector "URLForPath:error:"

-- | @Selector@ for @removeAssetPackWithIdentifier:completionHandler:@
removeAssetPackWithIdentifier_completionHandlerSelector :: Selector
removeAssetPackWithIdentifier_completionHandlerSelector = mkSelector "removeAssetPackWithIdentifier:completionHandler:"

