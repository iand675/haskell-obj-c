{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | A ``WKWebExtension`` object encapsulates a web extension’s resources that are defined by a @manifest.json@` file.
--
-- This class handles the reading and parsing of the manifest file along with the supporting resources like icons and localizations.
--
-- Generated bindings for @WKWebExtension@.
module ObjC.WebKit.WKWebExtension
  ( WKWebExtension
  , IsWKWebExtension(..)
  , new
  , init_
  , extensionWithAppExtensionBundle_completionHandler
  , extensionWithResourceBaseURL_completionHandler
  , supportsManifestVersion
  , errors
  , manifest
  , manifestVersion
  , defaultLocale
  , displayName
  , displayShortName
  , displayVersion
  , displayDescription
  , displayActionLabel
  , version
  , requestedPermissions
  , optionalPermissions
  , requestedPermissionMatchPatterns
  , optionalPermissionMatchPatterns
  , allRequestedMatchPatterns
  , hasBackgroundContent
  , hasPersistentBackgroundContent
  , hasInjectedContent
  , hasOptionsPage
  , hasOverrideNewTabPage
  , hasCommands
  , hasContentModificationRules
  , newSelector
  , initSelector
  , extensionWithAppExtensionBundle_completionHandlerSelector
  , extensionWithResourceBaseURL_completionHandlerSelector
  , supportsManifestVersionSelector
  , errorsSelector
  , manifestSelector
  , manifestVersionSelector
  , defaultLocaleSelector
  , displayNameSelector
  , displayShortNameSelector
  , displayVersionSelector
  , displayDescriptionSelector
  , displayActionLabelSelector
  , versionSelector
  , requestedPermissionsSelector
  , optionalPermissionsSelector
  , requestedPermissionMatchPatternsSelector
  , optionalPermissionMatchPatternsSelector
  , allRequestedMatchPatternsSelector
  , hasBackgroundContentSelector
  , hasPersistentBackgroundContentSelector
  , hasInjectedContentSelector
  , hasOptionsPageSelector
  , hasOverrideNewTabPageSelector
  , hasCommandsSelector
  , hasContentModificationRulesSelector


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

import ObjC.WebKit.Internal.Classes
import ObjC.AppKit.Internal.Classes
import ObjC.Foundation.Internal.Classes

-- | @+ new@
new :: IO (Id WKWebExtension)
new  =
  do
    cls' <- getRequiredClass "WKWebExtension"
    sendClassMsg cls' (mkSelector "new") (retPtr retVoid) [] >>= ownedObject . castPtr

-- | @- init@
init_ :: IsWKWebExtension wkWebExtension => wkWebExtension -> IO (Id WKWebExtension)
init_ wkWebExtension  =
  sendMsg wkWebExtension (mkSelector "init") (retPtr retVoid) [] >>= ownedObject . castPtr

-- | Returns a web extension initialized with a specified app extension bundle.
--
-- @appExtensionBundle@ — The bundle to use for the new web extension.
--
-- @completionHandler@ — A block to be called with an initialized web extension, or @nil@ if the object could not be initialized due to an error.
--
-- The app extension bundle must contain a @manifest.json@ file in its resources directory. If the manifest is invalid or missing, or the bundle is otherwise improperly configured, an error will be returned.
--
-- ObjC selector: @+ extensionWithAppExtensionBundle:completionHandler:@
extensionWithAppExtensionBundle_completionHandler :: IsNSBundle appExtensionBundle => appExtensionBundle -> Ptr () -> IO ()
extensionWithAppExtensionBundle_completionHandler appExtensionBundle completionHandler =
  do
    cls' <- getRequiredClass "WKWebExtension"
    withObjCPtr appExtensionBundle $ \raw_appExtensionBundle ->
      sendClassMsg cls' (mkSelector "extensionWithAppExtensionBundle:completionHandler:") retVoid [argPtr (castPtr raw_appExtensionBundle :: Ptr ()), argPtr (castPtr completionHandler :: Ptr ())]

-- | Returns a web extension initialized with a specified resource base URL, which can point to either a directory or a ZIP archive.
--
-- @resourceBaseURL@ — The file URL to use for the new web extension.
--
-- @completionHandler@ — A block to be called with an initialized web extension, or @nil@ if the object could not be initialized due to an error.
--
-- The URL must be a file URL that points to either a directory with a @manifest.json@ file or a ZIP archive containing a @manifest.json@ file. If the manifest is invalid or missing, or the URL points to an unsupported format or invalid archive, an error will be returned.
--
-- ObjC selector: @+ extensionWithResourceBaseURL:completionHandler:@
extensionWithResourceBaseURL_completionHandler :: IsNSURL resourceBaseURL => resourceBaseURL -> Ptr () -> IO ()
extensionWithResourceBaseURL_completionHandler resourceBaseURL completionHandler =
  do
    cls' <- getRequiredClass "WKWebExtension"
    withObjCPtr resourceBaseURL $ \raw_resourceBaseURL ->
      sendClassMsg cls' (mkSelector "extensionWithResourceBaseURL:completionHandler:") retVoid [argPtr (castPtr raw_resourceBaseURL :: Ptr ()), argPtr (castPtr completionHandler :: Ptr ())]

-- | Checks if a manifest version is supported by the extension.
--
-- @manifestVersion@ — The version number to check.
--
-- Returns: Returns @YES@ if the extension specified a manifest version that is greater than or equal to @manifestVersion@.
--
-- ObjC selector: @- supportsManifestVersion:@
supportsManifestVersion :: IsWKWebExtension wkWebExtension => wkWebExtension -> CDouble -> IO Bool
supportsManifestVersion wkWebExtension  manifestVersion =
  fmap ((/= 0) :: CULong -> Bool) $ sendMsg wkWebExtension (mkSelector "supportsManifestVersion:") retCULong [argCDouble (fromIntegral manifestVersion)]

-- | An array of all errors that occurred during the processing of the extension.
--
-- Provides an array of all parse-time errors for the extension, with repeat errors consolidated into a single entry for the original occurrence only. If no errors occurred, an empty array is returned.
--
-- Note: Once the extension is loaded, use the ``errors`` property on an extension context to monitor any runtime errors, as they can occur after the extension is loaded.
--
-- ObjC selector: @- errors@
errors :: IsWKWebExtension wkWebExtension => wkWebExtension -> IO (Id NSArray)
errors wkWebExtension  =
  sendMsg wkWebExtension (mkSelector "errors") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | The parsed manifest as a dictionary.
--
-- ObjC selector: @- manifest@
manifest :: IsWKWebExtension wkWebExtension => wkWebExtension -> IO (Id NSDictionary)
manifest wkWebExtension  =
  sendMsg wkWebExtension (mkSelector "manifest") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | The parsed manifest version, or @0@ if there is no version specified in the manifest.
--
-- Note: An ``WKWebExtensionErrorUnsupportedManifestVersion`` error will be reported if the manifest version isn't specified.
--
-- ObjC selector: @- manifestVersion@
manifestVersion :: IsWKWebExtension wkWebExtension => wkWebExtension -> IO CDouble
manifestVersion wkWebExtension  =
  sendMsg wkWebExtension (mkSelector "manifestVersion") retCDouble []

-- | The default locale for the extension. Returns @nil@ if there was no default locale specified.
--
-- ObjC selector: @- defaultLocale@
defaultLocale :: IsWKWebExtension wkWebExtension => wkWebExtension -> IO (Id NSLocale)
defaultLocale wkWebExtension  =
  sendMsg wkWebExtension (mkSelector "defaultLocale") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | The localized extension name. Returns @nil@ if there was no name specified.
--
-- ObjC selector: @- displayName@
displayName :: IsWKWebExtension wkWebExtension => wkWebExtension -> IO (Id NSString)
displayName wkWebExtension  =
  sendMsg wkWebExtension (mkSelector "displayName") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | The localized extension short name. Returns @nil@ if there was no short name specified.
--
-- ObjC selector: @- displayShortName@
displayShortName :: IsWKWebExtension wkWebExtension => wkWebExtension -> IO (Id NSString)
displayShortName wkWebExtension  =
  sendMsg wkWebExtension (mkSelector "displayShortName") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | The localized extension display version. Returns @nil@ if there was no display version specified.
--
-- ObjC selector: @- displayVersion@
displayVersion :: IsWKWebExtension wkWebExtension => wkWebExtension -> IO (Id NSString)
displayVersion wkWebExtension  =
  sendMsg wkWebExtension (mkSelector "displayVersion") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | The localized extension description. Returns @nil@ if there was no description specified.
--
-- ObjC selector: @- displayDescription@
displayDescription :: IsWKWebExtension wkWebExtension => wkWebExtension -> IO (Id NSString)
displayDescription wkWebExtension  =
  sendMsg wkWebExtension (mkSelector "displayDescription") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | The default localized extension action label. Returns @nil@ if there was no default action label specified.
--
-- This label serves as a default and should be used to represent the extension in contexts like action sheets or toolbars prior to the extension being loaded into an extension context. Once the extension is loaded, use the ``actionForTab:`` API to get the tab-specific label.
--
-- ObjC selector: @- displayActionLabel@
displayActionLabel :: IsWKWebExtension wkWebExtension => wkWebExtension -> IO (Id NSString)
displayActionLabel wkWebExtension  =
  sendMsg wkWebExtension (mkSelector "displayActionLabel") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | The extension version. Returns @nil@ if there was no version specified.
--
-- ObjC selector: @- version@
version :: IsWKWebExtension wkWebExtension => wkWebExtension -> IO (Id NSString)
version wkWebExtension  =
  sendMsg wkWebExtension (mkSelector "version") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | The set of permissions that the extension requires for its base functionality.
--
-- ObjC selector: @- requestedPermissions@
requestedPermissions :: IsWKWebExtension wkWebExtension => wkWebExtension -> IO (Id NSSet)
requestedPermissions wkWebExtension  =
  sendMsg wkWebExtension (mkSelector "requestedPermissions") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | The set of permissions that the extension may need for optional functionality. These permissions can be requested by the extension at a later time.
--
-- ObjC selector: @- optionalPermissions@
optionalPermissions :: IsWKWebExtension wkWebExtension => wkWebExtension -> IO (Id NSSet)
optionalPermissions wkWebExtension  =
  sendMsg wkWebExtension (mkSelector "optionalPermissions") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | The set of websites that the extension requires access to for its base functionality.
--
-- ObjC selector: @- requestedPermissionMatchPatterns@
requestedPermissionMatchPatterns :: IsWKWebExtension wkWebExtension => wkWebExtension -> IO (Id NSSet)
requestedPermissionMatchPatterns wkWebExtension  =
  sendMsg wkWebExtension (mkSelector "requestedPermissionMatchPatterns") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | The set of websites that the extension may need access to for optional functionality. These match patterns can be requested by the extension at a later time.
--
-- ObjC selector: @- optionalPermissionMatchPatterns@
optionalPermissionMatchPatterns :: IsWKWebExtension wkWebExtension => wkWebExtension -> IO (Id NSSet)
optionalPermissionMatchPatterns wkWebExtension  =
  sendMsg wkWebExtension (mkSelector "optionalPermissionMatchPatterns") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | The set of websites that the extension requires access to for injected content and for receiving messages from websites.
--
-- ObjC selector: @- allRequestedMatchPatterns@
allRequestedMatchPatterns :: IsWKWebExtension wkWebExtension => wkWebExtension -> IO (Id NSSet)
allRequestedMatchPatterns wkWebExtension  =
  sendMsg wkWebExtension (mkSelector "allRequestedMatchPatterns") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | A Boolean value indicating whether the extension has background content that can run when needed.
--
-- If this property is @YES@, the extension can run in the background even when no webpages are open.
--
-- ObjC selector: @- hasBackgroundContent@
hasBackgroundContent :: IsWKWebExtension wkWebExtension => wkWebExtension -> IO Bool
hasBackgroundContent wkWebExtension  =
  fmap ((/= 0) :: CULong -> Bool) $ sendMsg wkWebExtension (mkSelector "hasBackgroundContent") retCULong []

-- | A Boolean value indicating whether the extension has background content that stays in memory as long as the extension is loaded.
--
-- Note: Note that extensions are only allowed to have persistent background content on macOS. An ``WKWebExtensionErrorInvalidBackgroundPersistence`` error will be reported on iOS, iPadOS, and visionOS if an attempt is made to load a persistent extension.
--
-- ObjC selector: @- hasPersistentBackgroundContent@
hasPersistentBackgroundContent :: IsWKWebExtension wkWebExtension => wkWebExtension -> IO Bool
hasPersistentBackgroundContent wkWebExtension  =
  fmap ((/= 0) :: CULong -> Bool) $ sendMsg wkWebExtension (mkSelector "hasPersistentBackgroundContent") retCULong []

-- | A Boolean value indicating whether the extension has script or stylesheet content that can be injected into webpages.
--
-- If this property is @YES@, the extension has content that can be injected by matching against the extension's requested match patterns.
--
-- Note: Once the extension is loaded, use the ``hasInjectedContent`` property on an extension context, as the injectable content can change after the extension is loaded.
--
-- ObjC selector: @- hasInjectedContent@
hasInjectedContent :: IsWKWebExtension wkWebExtension => wkWebExtension -> IO Bool
hasInjectedContent wkWebExtension  =
  fmap ((/= 0) :: CULong -> Bool) $ sendMsg wkWebExtension (mkSelector "hasInjectedContent") retCULong []

-- | A Boolean value indicating whether the extension has an options page.
--
-- If this property is @YES@, the extension includes a dedicated options page where users can customize settings. The app should provide access to this page through a user interface element, which can be accessed via ``optionsPageURL`` on an extension context.
--
-- ObjC selector: @- hasOptionsPage@
hasOptionsPage :: IsWKWebExtension wkWebExtension => wkWebExtension -> IO Bool
hasOptionsPage wkWebExtension  =
  fmap ((/= 0) :: CULong -> Bool) $ sendMsg wkWebExtension (mkSelector "hasOptionsPage") retCULong []

-- | A Boolean value indicating whether the extension provides an alternative to the default new tab page.
--
-- If this property is @YES@, the extension can specify a custom page that can be displayed when a new tab is opened in the app, instead of the default new tab page. The app should prompt the user for permission to use the extension's new tab page as the default, which can be accessed via ``overrideNewTabPageURL`` on an extension context.
--
-- ObjC selector: @- hasOverrideNewTabPage@
hasOverrideNewTabPage :: IsWKWebExtension wkWebExtension => wkWebExtension -> IO Bool
hasOverrideNewTabPage wkWebExtension  =
  fmap ((/= 0) :: CULong -> Bool) $ sendMsg wkWebExtension (mkSelector "hasOverrideNewTabPage") retCULong []

-- | A Boolean value indicating whether the extension includes commands that users can invoke.
--
-- If this property is @YES@, the extension contains one or more commands that can be performed by the user. These commands should be accessible via keyboard shortcuts, menu items, or other user interface elements provided by the app. The list of commands can be accessed via ``commands`` on an extension context, and invoked via ``performCommand:``.
--
-- ObjC selector: @- hasCommands@
hasCommands :: IsWKWebExtension wkWebExtension => wkWebExtension -> IO Bool
hasCommands wkWebExtension  =
  fmap ((/= 0) :: CULong -> Bool) $ sendMsg wkWebExtension (mkSelector "hasCommands") retCULong []

-- | A boolean value indicating whether the extension includes rules used for content modification or blocking.
--
-- ObjC selector: @- hasContentModificationRules@
hasContentModificationRules :: IsWKWebExtension wkWebExtension => wkWebExtension -> IO Bool
hasContentModificationRules wkWebExtension  =
  fmap ((/= 0) :: CULong -> Bool) $ sendMsg wkWebExtension (mkSelector "hasContentModificationRules") retCULong []

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @new@
newSelector :: Selector
newSelector = mkSelector "new"

-- | @Selector@ for @init@
initSelector :: Selector
initSelector = mkSelector "init"

-- | @Selector@ for @extensionWithAppExtensionBundle:completionHandler:@
extensionWithAppExtensionBundle_completionHandlerSelector :: Selector
extensionWithAppExtensionBundle_completionHandlerSelector = mkSelector "extensionWithAppExtensionBundle:completionHandler:"

-- | @Selector@ for @extensionWithResourceBaseURL:completionHandler:@
extensionWithResourceBaseURL_completionHandlerSelector :: Selector
extensionWithResourceBaseURL_completionHandlerSelector = mkSelector "extensionWithResourceBaseURL:completionHandler:"

-- | @Selector@ for @supportsManifestVersion:@
supportsManifestVersionSelector :: Selector
supportsManifestVersionSelector = mkSelector "supportsManifestVersion:"

-- | @Selector@ for @errors@
errorsSelector :: Selector
errorsSelector = mkSelector "errors"

-- | @Selector@ for @manifest@
manifestSelector :: Selector
manifestSelector = mkSelector "manifest"

-- | @Selector@ for @manifestVersion@
manifestVersionSelector :: Selector
manifestVersionSelector = mkSelector "manifestVersion"

-- | @Selector@ for @defaultLocale@
defaultLocaleSelector :: Selector
defaultLocaleSelector = mkSelector "defaultLocale"

-- | @Selector@ for @displayName@
displayNameSelector :: Selector
displayNameSelector = mkSelector "displayName"

-- | @Selector@ for @displayShortName@
displayShortNameSelector :: Selector
displayShortNameSelector = mkSelector "displayShortName"

-- | @Selector@ for @displayVersion@
displayVersionSelector :: Selector
displayVersionSelector = mkSelector "displayVersion"

-- | @Selector@ for @displayDescription@
displayDescriptionSelector :: Selector
displayDescriptionSelector = mkSelector "displayDescription"

-- | @Selector@ for @displayActionLabel@
displayActionLabelSelector :: Selector
displayActionLabelSelector = mkSelector "displayActionLabel"

-- | @Selector@ for @version@
versionSelector :: Selector
versionSelector = mkSelector "version"

-- | @Selector@ for @requestedPermissions@
requestedPermissionsSelector :: Selector
requestedPermissionsSelector = mkSelector "requestedPermissions"

-- | @Selector@ for @optionalPermissions@
optionalPermissionsSelector :: Selector
optionalPermissionsSelector = mkSelector "optionalPermissions"

-- | @Selector@ for @requestedPermissionMatchPatterns@
requestedPermissionMatchPatternsSelector :: Selector
requestedPermissionMatchPatternsSelector = mkSelector "requestedPermissionMatchPatterns"

-- | @Selector@ for @optionalPermissionMatchPatterns@
optionalPermissionMatchPatternsSelector :: Selector
optionalPermissionMatchPatternsSelector = mkSelector "optionalPermissionMatchPatterns"

-- | @Selector@ for @allRequestedMatchPatterns@
allRequestedMatchPatternsSelector :: Selector
allRequestedMatchPatternsSelector = mkSelector "allRequestedMatchPatterns"

-- | @Selector@ for @hasBackgroundContent@
hasBackgroundContentSelector :: Selector
hasBackgroundContentSelector = mkSelector "hasBackgroundContent"

-- | @Selector@ for @hasPersistentBackgroundContent@
hasPersistentBackgroundContentSelector :: Selector
hasPersistentBackgroundContentSelector = mkSelector "hasPersistentBackgroundContent"

-- | @Selector@ for @hasInjectedContent@
hasInjectedContentSelector :: Selector
hasInjectedContentSelector = mkSelector "hasInjectedContent"

-- | @Selector@ for @hasOptionsPage@
hasOptionsPageSelector :: Selector
hasOptionsPageSelector = mkSelector "hasOptionsPage"

-- | @Selector@ for @hasOverrideNewTabPage@
hasOverrideNewTabPageSelector :: Selector
hasOverrideNewTabPageSelector = mkSelector "hasOverrideNewTabPage"

-- | @Selector@ for @hasCommands@
hasCommandsSelector :: Selector
hasCommandsSelector = mkSelector "hasCommands"

-- | @Selector@ for @hasContentModificationRules@
hasContentModificationRulesSelector :: Selector
hasContentModificationRulesSelector = mkSelector "hasContentModificationRules"

