{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | A controller that acts as a bridge between your Finder Sync extension and the Finder itself.
--
-- Use the Finder Sync controller to configure your extension, to set badges on items in the Finder’s window, and to get a list of selected and targeted items.
--
-- Generated bindings for @FIFinderSyncController@.
module ObjC.FinderSync.FIFinderSyncController
  ( FIFinderSyncController
  , IsFIFinderSyncController(..)
  , defaultController
  , setBadgeImage_label_forBadgeIdentifier
  , setBadgeIdentifier_forURL
  , targetedURL
  , selectedItemURLs
  , lastUsedDateForItemWithURL
  , setLastUsedDate_forItemWithURL_completion
  , tagDataForItemWithURL
  , setTagData_forItemWithURL_completion
  , showExtensionManagementInterface
  , directoryURLs
  , setDirectoryURLs
  , extensionEnabled
  , defaultControllerSelector
  , directoryURLsSelector
  , extensionEnabledSelector
  , lastUsedDateForItemWithURLSelector
  , selectedItemURLsSelector
  , setBadgeIdentifier_forURLSelector
  , setBadgeImage_label_forBadgeIdentifierSelector
  , setDirectoryURLsSelector
  , setLastUsedDate_forItemWithURL_completionSelector
  , setTagData_forItemWithURL_completionSelector
  , showExtensionManagementInterfaceSelector
  , tagDataForItemWithURLSelector
  , targetedURLSelector


  ) where

import Foreign.Ptr (Ptr, FunPtr)
import Foreign.C.Types

import ObjC.Runtime.Types
import ObjC.Runtime.Message (sendMessage, sendOwnedMessage, sendClassMessage, sendOwnedClassMessage)
import ObjC.Runtime.Selector (mkSelector)
import ObjC.Runtime.Class (getRequiredClass)

import ObjC.FinderSync.Internal.Classes
import ObjC.AppKit.Internal.Classes
import ObjC.Foundation.Internal.Classes

-- | Returns the shared Finder Sync controller object.
--
-- - Returns: The default Finder Sync controller object for this extension.
--
-- ObjC selector: @+ defaultController@
defaultController :: IO (Id FIFinderSyncController)
defaultController  =
  do
    cls' <- getRequiredClass "FIFinderSyncController"
    sendClassMessage cls' defaultControllerSelector

-- | Sets the badge image and label for the given ID.
--
-- Use this method to configure your badges. Finder may display the image, the label or both. Your Finder Sync extension typically sets up a fixed number of badges during its @init@ method.
--
-- - Parameters:   - image: An <doc://com.apple.documentation/documentation/appkit/nsimage>     object. The system may or may not draw this image on top of the item’s     icon; when it does, the system determines the overlay position. Don't     add any padding to the image to adjust this positioning. The image draws     at up to 320 x 320 points.   - label: A label describing the sync state represented by this badge.     Each label should be a short localized string, such as "Waiting."   - badgeID: A unique ID, identifying this badge.
--
-- ObjC selector: @- setBadgeImage:label:forBadgeIdentifier:@
setBadgeImage_label_forBadgeIdentifier :: (IsFIFinderSyncController fiFinderSyncController, IsNSImage image, IsNSString label, IsNSString badgeID) => fiFinderSyncController -> image -> label -> badgeID -> IO ()
setBadgeImage_label_forBadgeIdentifier fiFinderSyncController image label badgeID =
  sendMessage fiFinderSyncController setBadgeImage_label_forBadgeIdentifierSelector (toNSImage image) (toNSString label) (toNSString badgeID)

-- | Sets the badge for a file or directory.
--
-- Adds the specified badge to the given file or directory. Setting the identifier to an empty string (`""`) removes the badge.
--
-- Avoid adding badges to items that the Finder hasn't displayed yet. When setting the initial badge, call this method from your Finder Sync extension’s ``FIFinderSync/requestBadgeIdentifierForURL:`` method. When updating badges, call this method only for items that have already received a badge.
--
-- - Parameters:   - badgeID: A unique ID, identifying the badge.   - url: The URL of the file or directory.
--
-- ## See Also   - ``FIFinderSync/requestBadgeIdentifierForURL:``
--
-- ObjC selector: @- setBadgeIdentifier:forURL:@
setBadgeIdentifier_forURL :: (IsFIFinderSyncController fiFinderSyncController, IsNSString badgeID, IsNSURL url) => fiFinderSyncController -> badgeID -> url -> IO ()
setBadgeIdentifier_forURL fiFinderSyncController badgeID url =
  sendMessage fiFinderSyncController setBadgeIdentifier_forURLSelector (toNSString badgeID) (toNSURL url)

-- | Returns the URL of the Finder’s current target.
--
-- Use this method when creating a custom shortcut menu for the Finder. This returns the URL of the item that the user Control-clicked, letting you customize the menu for that item.
--
-- This method returns valid values only from the Finder Sync extension’s ``FIFinderSync/menuForMenuKind:`` method or from one of the menu actions created in this method. If the selected items are outside the extension’s managed directories (for example, when the user clicks on the toolbar button), this method returns @nil@.
--
-- - Returns: The URL of the Finder’s current target.
--
-- ObjC selector: @- targetedURL@
targetedURL :: IsFIFinderSyncController fiFinderSyncController => fiFinderSyncController -> IO (Id NSURL)
targetedURL fiFinderSyncController =
  sendMessage fiFinderSyncController targetedURLSelector

-- | Returns an array of selected items.
--
-- Use this method when creating a shortcut menu or a menu for the extension’s toolbar button. You can then modify the menu’s content based on the items currently selected.
--
-- This method returns valid values only from the Finder Sync extension’s ``FIFinderSync/menuForMenuKind:`` method or from one of the menu actions created in this method. If the selected items are outside the extension’s managed directories (for example, when the user clicks on the toolbar button), this method returns @nil@.
--
-- - Returns: An array of items currently selected in the Finder window.
--
-- ObjC selector: @- selectedItemURLs@
selectedItemURLs :: IsFIFinderSyncController fiFinderSyncController => fiFinderSyncController -> IO (Id NSArray)
selectedItemURLs fiFinderSyncController =
  sendMessage fiFinderSyncController selectedItemURLsSelector

-- | @- lastUsedDateForItemWithURL:@
lastUsedDateForItemWithURL :: (IsFIFinderSyncController fiFinderSyncController, IsNSURL itemURL) => fiFinderSyncController -> itemURL -> IO (Id NSDate)
lastUsedDateForItemWithURL fiFinderSyncController itemURL =
  sendMessage fiFinderSyncController lastUsedDateForItemWithURLSelector (toNSURL itemURL)

-- | @- setLastUsedDate:forItemWithURL:completion:@
setLastUsedDate_forItemWithURL_completion :: (IsFIFinderSyncController fiFinderSyncController, IsNSDate lastUsedDate, IsNSURL itemURL) => fiFinderSyncController -> lastUsedDate -> itemURL -> Ptr () -> IO ()
setLastUsedDate_forItemWithURL_completion fiFinderSyncController lastUsedDate itemURL completion =
  sendMessage fiFinderSyncController setLastUsedDate_forItemWithURL_completionSelector (toNSDate lastUsedDate) (toNSURL itemURL) completion

-- | @- tagDataForItemWithURL:@
tagDataForItemWithURL :: (IsFIFinderSyncController fiFinderSyncController, IsNSURL itemURL) => fiFinderSyncController -> itemURL -> IO (Id NSData)
tagDataForItemWithURL fiFinderSyncController itemURL =
  sendMessage fiFinderSyncController tagDataForItemWithURLSelector (toNSURL itemURL)

-- | @- setTagData:forItemWithURL:completion:@
setTagData_forItemWithURL_completion :: (IsFIFinderSyncController fiFinderSyncController, IsNSData tagData, IsNSURL itemURL) => fiFinderSyncController -> tagData -> itemURL -> Ptr () -> IO ()
setTagData_forItemWithURL_completion fiFinderSyncController tagData itemURL completion =
  sendMessage fiFinderSyncController setTagData_forItemWithURL_completionSelector (toNSData tagData) (toNSURL itemURL) completion

-- | @+ showExtensionManagementInterface@
showExtensionManagementInterface :: IO ()
showExtensionManagementInterface  =
  do
    cls' <- getRequiredClass "FIFinderSyncController"
    sendClassMessage cls' showExtensionManagementInterfaceSelector

-- | The directories managed by this extension.
--
-- The extension receives ``FIFinderSync/beginObservingDirectoryAtURL:`` and ``FIFinderSync/endObservingDirectoryAtURL:`` messages for every directory in this set and for all of their subdirectories.
--
-- Always set @directoryURLs@ when the extension starts. If there are no directories to watch, set @directoryURLs@ to an empty set.
--
-- ObjC selector: @- directoryURLs@
directoryURLs :: IsFIFinderSyncController fiFinderSyncController => fiFinderSyncController -> IO (Id NSSet)
directoryURLs fiFinderSyncController =
  sendMessage fiFinderSyncController directoryURLsSelector

-- | The directories managed by this extension.
--
-- The extension receives ``FIFinderSync/beginObservingDirectoryAtURL:`` and ``FIFinderSync/endObservingDirectoryAtURL:`` messages for every directory in this set and for all of their subdirectories.
--
-- Always set @directoryURLs@ when the extension starts. If there are no directories to watch, set @directoryURLs@ to an empty set.
--
-- ObjC selector: @- setDirectoryURLs:@
setDirectoryURLs :: (IsFIFinderSyncController fiFinderSyncController, IsNSSet value) => fiFinderSyncController -> value -> IO ()
setDirectoryURLs fiFinderSyncController value =
  sendMessage fiFinderSyncController setDirectoryURLsSelector (toNSSet value)

-- | @+ extensionEnabled@
extensionEnabled :: IO Bool
extensionEnabled  =
  do
    cls' <- getRequiredClass "FIFinderSyncController"
    sendClassMessage cls' extensionEnabledSelector

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @defaultController@
defaultControllerSelector :: Selector '[] (Id FIFinderSyncController)
defaultControllerSelector = mkSelector "defaultController"

-- | @Selector@ for @setBadgeImage:label:forBadgeIdentifier:@
setBadgeImage_label_forBadgeIdentifierSelector :: Selector '[Id NSImage, Id NSString, Id NSString] ()
setBadgeImage_label_forBadgeIdentifierSelector = mkSelector "setBadgeImage:label:forBadgeIdentifier:"

-- | @Selector@ for @setBadgeIdentifier:forURL:@
setBadgeIdentifier_forURLSelector :: Selector '[Id NSString, Id NSURL] ()
setBadgeIdentifier_forURLSelector = mkSelector "setBadgeIdentifier:forURL:"

-- | @Selector@ for @targetedURL@
targetedURLSelector :: Selector '[] (Id NSURL)
targetedURLSelector = mkSelector "targetedURL"

-- | @Selector@ for @selectedItemURLs@
selectedItemURLsSelector :: Selector '[] (Id NSArray)
selectedItemURLsSelector = mkSelector "selectedItemURLs"

-- | @Selector@ for @lastUsedDateForItemWithURL:@
lastUsedDateForItemWithURLSelector :: Selector '[Id NSURL] (Id NSDate)
lastUsedDateForItemWithURLSelector = mkSelector "lastUsedDateForItemWithURL:"

-- | @Selector@ for @setLastUsedDate:forItemWithURL:completion:@
setLastUsedDate_forItemWithURL_completionSelector :: Selector '[Id NSDate, Id NSURL, Ptr ()] ()
setLastUsedDate_forItemWithURL_completionSelector = mkSelector "setLastUsedDate:forItemWithURL:completion:"

-- | @Selector@ for @tagDataForItemWithURL:@
tagDataForItemWithURLSelector :: Selector '[Id NSURL] (Id NSData)
tagDataForItemWithURLSelector = mkSelector "tagDataForItemWithURL:"

-- | @Selector@ for @setTagData:forItemWithURL:completion:@
setTagData_forItemWithURL_completionSelector :: Selector '[Id NSData, Id NSURL, Ptr ()] ()
setTagData_forItemWithURL_completionSelector = mkSelector "setTagData:forItemWithURL:completion:"

-- | @Selector@ for @showExtensionManagementInterface@
showExtensionManagementInterfaceSelector :: Selector '[] ()
showExtensionManagementInterfaceSelector = mkSelector "showExtensionManagementInterface"

-- | @Selector@ for @directoryURLs@
directoryURLsSelector :: Selector '[] (Id NSSet)
directoryURLsSelector = mkSelector "directoryURLs"

-- | @Selector@ for @setDirectoryURLs:@
setDirectoryURLsSelector :: Selector '[Id NSSet] ()
setDirectoryURLsSelector = mkSelector "setDirectoryURLs:"

-- | @Selector@ for @extensionEnabled@
extensionEnabledSelector :: Selector '[] Bool
extensionEnabledSelector = mkSelector "extensionEnabled"

