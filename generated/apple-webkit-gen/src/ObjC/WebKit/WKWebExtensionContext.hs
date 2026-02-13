{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | A ``WKWebExtensionContext`` object represents the runtime environment for a web extension.
--
-- This class provides methods for managing the extension's permissions, allowing it to inject content, run background logic, show popovers, and display other web-based UI to the user.
--
-- Generated bindings for @WKWebExtensionContext@.
module ObjC.WebKit.WKWebExtensionContext
  ( WKWebExtensionContext
  , IsWKWebExtensionContext(..)
  , new
  , init_
  , contextForExtension
  , initForExtension
  , hasPermission
  , hasPermission_inTab
  , hasAccessToURL
  , hasAccessToURL_inTab
  , hasInjectedContentForURL
  , permissionStatusForPermission
  , permissionStatusForPermission_inTab
  , setPermissionStatus_forPermission
  , setPermissionStatus_forPermission_expirationDate
  , permissionStatusForURL
  , permissionStatusForURL_inTab
  , setPermissionStatus_forURL
  , setPermissionStatus_forURL_expirationDate
  , permissionStatusForMatchPattern
  , permissionStatusForMatchPattern_inTab
  , setPermissionStatus_forMatchPattern
  , setPermissionStatus_forMatchPattern_expirationDate
  , loadBackgroundContentWithCompletionHandler
  , actionForTab
  , performActionForTab
  , performCommand
  , performCommandForEvent
  , commandForEvent
  , menuItemsForTab
  , userGesturePerformedInTab
  , hasActiveUserGestureInTab
  , clearUserGestureInTab
  , didOpenWindow
  , didCloseWindow
  , didFocusWindow
  , didOpenTab
  , didCloseTab_windowIsClosing
  , didActivateTab_previousActiveTab
  , didSelectTabs
  , didDeselectTabs
  , didMoveTab_fromIndex_inWindow
  , didReplaceTab_withTab
  , didChangeTabProperties_forTab
  , webExtension
  , webExtensionController
  , loaded
  , errors
  , baseURL
  , setBaseURL
  , uniqueIdentifier
  , setUniqueIdentifier
  , inspectable
  , setInspectable
  , inspectionName
  , setInspectionName
  , unsupportedAPIs
  , setUnsupportedAPIs
  , webViewConfiguration
  , optionsPageURL
  , overrideNewTabPageURL
  , grantedPermissions
  , setGrantedPermissions
  , grantedPermissionMatchPatterns
  , setGrantedPermissionMatchPatterns
  , deniedPermissions
  , setDeniedPermissions
  , deniedPermissionMatchPatterns
  , setDeniedPermissionMatchPatterns
  , hasRequestedOptionalAccessToAllHosts
  , setHasRequestedOptionalAccessToAllHosts
  , hasAccessToPrivateData
  , setHasAccessToPrivateData
  , currentPermissions
  , currentPermissionMatchPatterns
  , hasAccessToAllURLs
  , hasAccessToAllHosts
  , hasInjectedContent
  , hasContentModificationRules
  , commands
  , openWindows
  , focusedWindow
  , openTabs
  , actionForTabSelector
  , baseURLSelector
  , clearUserGestureInTabSelector
  , commandForEventSelector
  , commandsSelector
  , contextForExtensionSelector
  , currentPermissionMatchPatternsSelector
  , currentPermissionsSelector
  , deniedPermissionMatchPatternsSelector
  , deniedPermissionsSelector
  , didActivateTab_previousActiveTabSelector
  , didChangeTabProperties_forTabSelector
  , didCloseTab_windowIsClosingSelector
  , didCloseWindowSelector
  , didDeselectTabsSelector
  , didFocusWindowSelector
  , didMoveTab_fromIndex_inWindowSelector
  , didOpenTabSelector
  , didOpenWindowSelector
  , didReplaceTab_withTabSelector
  , didSelectTabsSelector
  , errorsSelector
  , focusedWindowSelector
  , grantedPermissionMatchPatternsSelector
  , grantedPermissionsSelector
  , hasAccessToAllHostsSelector
  , hasAccessToAllURLsSelector
  , hasAccessToPrivateDataSelector
  , hasAccessToURLSelector
  , hasAccessToURL_inTabSelector
  , hasActiveUserGestureInTabSelector
  , hasContentModificationRulesSelector
  , hasInjectedContentForURLSelector
  , hasInjectedContentSelector
  , hasPermissionSelector
  , hasPermission_inTabSelector
  , hasRequestedOptionalAccessToAllHostsSelector
  , initForExtensionSelector
  , initSelector
  , inspectableSelector
  , inspectionNameSelector
  , loadBackgroundContentWithCompletionHandlerSelector
  , loadedSelector
  , menuItemsForTabSelector
  , newSelector
  , openTabsSelector
  , openWindowsSelector
  , optionsPageURLSelector
  , overrideNewTabPageURLSelector
  , performActionForTabSelector
  , performCommandForEventSelector
  , performCommandSelector
  , permissionStatusForMatchPatternSelector
  , permissionStatusForMatchPattern_inTabSelector
  , permissionStatusForPermissionSelector
  , permissionStatusForPermission_inTabSelector
  , permissionStatusForURLSelector
  , permissionStatusForURL_inTabSelector
  , setBaseURLSelector
  , setDeniedPermissionMatchPatternsSelector
  , setDeniedPermissionsSelector
  , setGrantedPermissionMatchPatternsSelector
  , setGrantedPermissionsSelector
  , setHasAccessToPrivateDataSelector
  , setHasRequestedOptionalAccessToAllHostsSelector
  , setInspectableSelector
  , setInspectionNameSelector
  , setPermissionStatus_forMatchPatternSelector
  , setPermissionStatus_forMatchPattern_expirationDateSelector
  , setPermissionStatus_forPermissionSelector
  , setPermissionStatus_forPermission_expirationDateSelector
  , setPermissionStatus_forURLSelector
  , setPermissionStatus_forURL_expirationDateSelector
  , setUniqueIdentifierSelector
  , setUnsupportedAPIsSelector
  , uniqueIdentifierSelector
  , unsupportedAPIsSelector
  , userGesturePerformedInTabSelector
  , webExtensionControllerSelector
  , webExtensionSelector
  , webViewConfigurationSelector

  -- * Enum types
  , WKWebExtensionContextPermissionStatus(WKWebExtensionContextPermissionStatus)
  , pattern WKWebExtensionContextPermissionStatusDeniedExplicitly
  , pattern WKWebExtensionContextPermissionStatusDeniedImplicitly
  , pattern WKWebExtensionContextPermissionStatusRequestedImplicitly
  , pattern WKWebExtensionContextPermissionStatusUnknown
  , pattern WKWebExtensionContextPermissionStatusRequestedExplicitly
  , pattern WKWebExtensionContextPermissionStatusGrantedImplicitly
  , pattern WKWebExtensionContextPermissionStatusGrantedExplicitly
  , WKWebExtensionTabChangedProperties(WKWebExtensionTabChangedProperties)
  , pattern WKWebExtensionTabChangedPropertiesNone
  , pattern WKWebExtensionTabChangedPropertiesLoading
  , pattern WKWebExtensionTabChangedPropertiesMuted
  , pattern WKWebExtensionTabChangedPropertiesPinned
  , pattern WKWebExtensionTabChangedPropertiesPlayingAudio
  , pattern WKWebExtensionTabChangedPropertiesReaderMode
  , pattern WKWebExtensionTabChangedPropertiesSize
  , pattern WKWebExtensionTabChangedPropertiesTitle
  , pattern WKWebExtensionTabChangedPropertiesURL
  , pattern WKWebExtensionTabChangedPropertiesZoomFactor

  ) where

import Foreign.Ptr (Ptr, FunPtr)
import Foreign.C.Types

import ObjC.Runtime.Types
import ObjC.Runtime.Message (sendMessage, sendOwnedMessage, sendClassMessage, sendOwnedClassMessage)
import ObjC.Runtime.Selector (mkSelector)
import ObjC.Runtime.Class (getRequiredClass)

import ObjC.WebKit.Internal.Classes
import ObjC.WebKit.Internal.Enums
import ObjC.AppKit.Internal.Classes
import ObjC.Foundation.Internal.Classes

-- | @+ new@
new :: IO (Id WKWebExtensionContext)
new  =
  do
    cls' <- getRequiredClass "WKWebExtensionContext"
    sendOwnedClassMessage cls' newSelector

-- | @- init@
init_ :: IsWKWebExtensionContext wkWebExtensionContext => wkWebExtensionContext -> IO (Id WKWebExtensionContext)
init_ wkWebExtensionContext =
  sendOwnedMessage wkWebExtensionContext initSelector

-- | Returns a web extension context initialized with the specified extension.
--
-- @extension@ — The extension to use for the new web extension context.
--
-- Returns: An initialized web extension context.
--
-- ObjC selector: @+ contextForExtension:@
contextForExtension :: IsWKWebExtension extension => extension -> IO (Id WKWebExtensionContext)
contextForExtension extension =
  do
    cls' <- getRequiredClass "WKWebExtensionContext"
    sendClassMessage cls' contextForExtensionSelector (toWKWebExtension extension)

-- | Returns a web extension context initialized with a specified extension.
--
-- @extension@ — The extension to use for the new web extension context.
--
-- Returns: An initialized web extension context.
--
-- This is a designated initializer.
--
-- ObjC selector: @- initForExtension:@
initForExtension :: (IsWKWebExtensionContext wkWebExtensionContext, IsWKWebExtension extension) => wkWebExtensionContext -> extension -> IO (Id WKWebExtensionContext)
initForExtension wkWebExtensionContext extension =
  sendOwnedMessage wkWebExtensionContext initForExtensionSelector (toWKWebExtension extension)

-- | Checks the specified permission against the currently granted permissions.
--
-- @permission@ — The permission for which to return the status.
--
-- currentPermissions
--
-- hasPermission:inTab:
--
-- permissionStatusForPermission:
--
-- permissionStatusForPermission:inTab:
--
-- ObjC selector: @- hasPermission:@
hasPermission :: (IsWKWebExtensionContext wkWebExtensionContext, IsNSString permission) => wkWebExtensionContext -> permission -> IO Bool
hasPermission wkWebExtensionContext permission =
  sendMessage wkWebExtensionContext hasPermissionSelector (toNSString permission)

-- | Checks the specified permission against the currently granted permissions in a specific tab.
--
-- @permission@ — The permission for which to return the status.
--
-- @tab@ — The tab in which to return the permission status, or @nil@ if the tab is not known or the global status is desired.
--
-- Permissions can be granted on a per-tab basis. When the tab is known, permission checks should always use this method.
--
-- currentPermissions
--
-- hasPermission:
--
-- permissionStatusForPermission:
--
-- permissionStatusForPermission:inTab:
--
-- ObjC selector: @- hasPermission:inTab:@
hasPermission_inTab :: (IsWKWebExtensionContext wkWebExtensionContext, IsNSString permission) => wkWebExtensionContext -> permission -> RawId -> IO Bool
hasPermission_inTab wkWebExtensionContext permission tab =
  sendMessage wkWebExtensionContext hasPermission_inTabSelector (toNSString permission) tab

-- | Checks the specified URL against the currently granted permission match patterns.
--
-- @url@ — The URL for which to return the status.
--
-- currentPermissionMatchPatterns
--
-- hasAccessToURL:inTab:
--
-- permissionStatusForURL:
--
-- permissionStatusForURL:inTab:
--
-- permissionStatusForMatchPattern:
--
-- permissionStatusForMatchPattern:inTab:
--
-- ObjC selector: @- hasAccessToURL:@
hasAccessToURL :: (IsWKWebExtensionContext wkWebExtensionContext, IsNSURL url) => wkWebExtensionContext -> url -> IO Bool
hasAccessToURL wkWebExtensionContext url =
  sendMessage wkWebExtensionContext hasAccessToURLSelector (toNSURL url)

-- | Checks the specified URL against the currently granted permission match patterns in a specific tab.
--
-- @url@ — The URL for which to return the status.
--
-- @tab@ — The tab in which to return the permission status, or @nil@ if the tab is not known or the global status is desired.
--
-- Some match patterns can be granted on a per-tab basis. When the tab is known, access checks should always use this method.
--
-- currentPermissionMatchPatterns
--
-- hasAccessToURL:
--
-- permissionStatusForURL:
--
-- permissionStatusForURL:inTab:
--
-- permissionStatusForMatchPattern:
--
-- permissionStatusForMatchPattern:inTab:
--
-- ObjC selector: @- hasAccessToURL:inTab:@
hasAccessToURL_inTab :: (IsWKWebExtensionContext wkWebExtensionContext, IsNSURL url) => wkWebExtensionContext -> url -> RawId -> IO Bool
hasAccessToURL_inTab wkWebExtensionContext url tab =
  sendMessage wkWebExtensionContext hasAccessToURL_inTabSelector (toNSURL url) tab

-- | Checks if the extension has script or stylesheet content that can be injected into the specified URL.
--
-- @url@ — The webpage URL to check.
--
-- Returns: Returns @YES@ if the extension has content that can be injected by matching the URL against the extension's requested match patterns.
--
-- The extension context will still need to be loaded and have granted website permissions for its content to actually be injected.
--
-- ObjC selector: @- hasInjectedContentForURL:@
hasInjectedContentForURL :: (IsWKWebExtensionContext wkWebExtensionContext, IsNSURL url) => wkWebExtensionContext -> url -> IO Bool
hasInjectedContentForURL wkWebExtensionContext url =
  sendMessage wkWebExtensionContext hasInjectedContentForURLSelector (toNSURL url)

-- | Checks the specified permission against the currently denied, granted, and requested permissions.
--
-- @permission@ — The permission for which to return the status.
--
-- Permissions can be granted on a per-tab basis. When the tab is known, access checks should always use the method that checks in a tab.
--
-- permissionStatusForPermission:inTab:
--
-- hasPermission:
--
-- ObjC selector: @- permissionStatusForPermission:@
permissionStatusForPermission :: (IsWKWebExtensionContext wkWebExtensionContext, IsNSString permission) => wkWebExtensionContext -> permission -> IO WKWebExtensionContextPermissionStatus
permissionStatusForPermission wkWebExtensionContext permission =
  sendMessage wkWebExtensionContext permissionStatusForPermissionSelector (toNSString permission)

-- | Checks the specified permission against the currently denied, granted, and requested permissions.
--
-- @permission@ — The permission for which to return the status.
--
-- @tab@ — The tab in which to return the permission status, or @nil@ if the tab is not known or the global status is desired.
--
-- Permissions can be granted on a per-tab basis. When the tab is known, access checks should always specify the tab.
--
-- permissionStatusForPermission:
--
-- hasPermission:inTab:
--
-- ObjC selector: @- permissionStatusForPermission:inTab:@
permissionStatusForPermission_inTab :: (IsWKWebExtensionContext wkWebExtensionContext, IsNSString permission) => wkWebExtensionContext -> permission -> RawId -> IO WKWebExtensionContextPermissionStatus
permissionStatusForPermission_inTab wkWebExtensionContext permission tab =
  sendMessage wkWebExtensionContext permissionStatusForPermission_inTabSelector (toNSString permission) tab

-- | Sets the status of a permission with a distant future expiration date.
--
-- @status@ — The new permission status to set for the given permission.
--
-- @permission@ — The permission for which to set the status.
--
-- This method will update ``grantedPermissions`` and ``deniedPermissions``. Use this method for changing a single permission's status. Only ``WKWebExtensionContextPermissionStatusDeniedExplicitly``, ``WKWebExtensionContextPermissionStatusUnknown``, and ``WKWebExtensionContextPermissionStatusGrantedExplicitly`` states are allowed to be set using this method.
--
-- setPermissionStatus:forPermission:expirationDate:
--
-- setPermissionStatus:forPermission:inTab:
--
-- ObjC selector: @- setPermissionStatus:forPermission:@
setPermissionStatus_forPermission :: (IsWKWebExtensionContext wkWebExtensionContext, IsNSString permission) => wkWebExtensionContext -> WKWebExtensionContextPermissionStatus -> permission -> IO ()
setPermissionStatus_forPermission wkWebExtensionContext status permission =
  sendMessage wkWebExtensionContext setPermissionStatus_forPermissionSelector status (toNSString permission)

-- | Sets the status of a permission with a specific expiration date.
--
-- @status@ — The new permission status to set for the given permission.
--
-- @permission@ — The permission for which to set the status.
--
-- @expirationDate@ — The expiration date for the new permission status, or @nil@ for distant future.
--
-- This method will update ``grantedPermissions`` and ``deniedPermissions``. Use this method for changing a single permission's status. Passing a @nil@ expiration date will be treated as a distant future date. Only ``WKWebExtensionContextPermissionStatusDeniedExplicitly``, ``WKWebExtensionContextPermissionStatusUnknown``, and ``WKWebExtensionContextPermissionStatusGrantedExplicitly`` states are allowed to be set using this method.
--
-- setPermissionStatus:forPermission:
--
-- setPermissionStatus:forPermission:inTab:
--
-- ObjC selector: @- setPermissionStatus:forPermission:expirationDate:@
setPermissionStatus_forPermission_expirationDate :: (IsWKWebExtensionContext wkWebExtensionContext, IsNSString permission, IsNSDate expirationDate) => wkWebExtensionContext -> WKWebExtensionContextPermissionStatus -> permission -> expirationDate -> IO ()
setPermissionStatus_forPermission_expirationDate wkWebExtensionContext status permission expirationDate =
  sendMessage wkWebExtensionContext setPermissionStatus_forPermission_expirationDateSelector status (toNSString permission) (toNSDate expirationDate)

-- | Checks the specified URL against the currently denied, granted, and requested permission match patterns.
--
-- @url@ — The URL for which to return the status.
--
-- URLs and match patterns can be granted on a per-tab basis. When the tab is known, access checks should always use the method that checks in a tab.
--
-- permissionStatusForURL:inTab:
--
-- hasAccessToURL:
--
-- ObjC selector: @- permissionStatusForURL:@
permissionStatusForURL :: (IsWKWebExtensionContext wkWebExtensionContext, IsNSURL url) => wkWebExtensionContext -> url -> IO WKWebExtensionContextPermissionStatus
permissionStatusForURL wkWebExtensionContext url =
  sendMessage wkWebExtensionContext permissionStatusForURLSelector (toNSURL url)

-- | Checks the specified URL against the currently denied, granted, and requested permission match patterns.
--
-- @url@ — The URL for which to return the status.
--
-- @tab@ — The tab in which to return the permission status, or @nil@ if the tab is not known or the global status is desired.
--
-- URLs and match patterns can be granted on a per-tab basis. When the tab is known, access checks should always use this method.
--
-- permissionStatusForURL:
--
-- hasAccessToURL:inTab:
--
-- ObjC selector: @- permissionStatusForURL:inTab:@
permissionStatusForURL_inTab :: (IsWKWebExtensionContext wkWebExtensionContext, IsNSURL url) => wkWebExtensionContext -> url -> RawId -> IO WKWebExtensionContextPermissionStatus
permissionStatusForURL_inTab wkWebExtensionContext url tab =
  sendMessage wkWebExtensionContext permissionStatusForURL_inTabSelector (toNSURL url) tab

-- | Sets the permission status of a URL with a distant future expiration date.
--
-- @status@ — The new permission status to set for the given URL.
--
-- @url@ — The URL for which to set the status.
--
-- The URL is converted into a match pattern and will update ``grantedPermissionMatchPatterns`` and ``deniedPermissionMatchPatterns``. Use this method for changing a single URL's status. Only ``WKWebExtensionContextPermissionStatusDeniedExplicitly``, ``WKWebExtensionContextPermissionStatusUnknown``, and ``WKWebExtensionContextPermissionStatusGrantedExplicitly`` states are allowed to be set using this method.
--
-- setPermissionStatus:forURL:expirationDate:
--
-- setPermissionStatus:forURL:inTab:
--
-- ObjC selector: @- setPermissionStatus:forURL:@
setPermissionStatus_forURL :: (IsWKWebExtensionContext wkWebExtensionContext, IsNSURL url) => wkWebExtensionContext -> WKWebExtensionContextPermissionStatus -> url -> IO ()
setPermissionStatus_forURL wkWebExtensionContext status url =
  sendMessage wkWebExtensionContext setPermissionStatus_forURLSelector status (toNSURL url)

-- | Sets the permission status of a URL with a distant future expiration date.
--
-- @status@ — The new permission status to set for the given URL.
--
-- @url@ — The URL for which to set the status.
--
-- @expirationDate@ — The expiration date for the new permission status, or @nil@ for distant future.
--
-- The URL is converted into a match pattern and will update ``grantedPermissionMatchPatterns`` and ``deniedPermissionMatchPatterns``. Use this method for changing a single URL's status. Passing a @nil@ expiration date will be treated as a distant future date. Only ``WKWebExtensionContextPermissionStatusDeniedExplicitly``, ``WKWebExtensionContextPermissionStatusUnknown``, and ``WKWebExtensionContextPermissionStatusGrantedExplicitly`` states are allowed to be set using this method.
--
-- setPermissionStatus:forURL:
--
-- setPermissionStatus:forURL:inTab:
--
-- ObjC selector: @- setPermissionStatus:forURL:expirationDate:@
setPermissionStatus_forURL_expirationDate :: (IsWKWebExtensionContext wkWebExtensionContext, IsNSURL url, IsNSDate expirationDate) => wkWebExtensionContext -> WKWebExtensionContextPermissionStatus -> url -> expirationDate -> IO ()
setPermissionStatus_forURL_expirationDate wkWebExtensionContext status url expirationDate =
  sendMessage wkWebExtensionContext setPermissionStatus_forURL_expirationDateSelector status (toNSURL url) (toNSDate expirationDate)

-- | Checks the specified match pattern against the currently denied, granted, and requested permission match patterns.
--
-- @pattern@ — The pattern for which to return the status.
--
-- Match patterns can be granted on a per-tab basis. When the tab is known, access checks should always use the method that checks in a tab.
--
-- permissionStatusForMatchPattern:inTab:
--
-- hasAccessToURL:inTab:
--
-- ObjC selector: @- permissionStatusForMatchPattern:@
permissionStatusForMatchPattern :: (IsWKWebExtensionContext wkWebExtensionContext, IsWKWebExtensionMatchPattern pattern_) => wkWebExtensionContext -> pattern_ -> IO WKWebExtensionContextPermissionStatus
permissionStatusForMatchPattern wkWebExtensionContext pattern_ =
  sendMessage wkWebExtensionContext permissionStatusForMatchPatternSelector (toWKWebExtensionMatchPattern pattern_)

-- | Checks the specified match pattern against the currently denied, granted, and requested permission match patterns.
--
-- @pattern@ — The pattern for which to return the status.
--
-- @tab@ — The tab in which to return the permission status, or @nil@ if the tab is not known or the global status is desired.
--
-- Match patterns can be granted on a per-tab basis. When the tab is known, access checks should always use this method.
--
-- permissionStatusForMatchPattern:
--
-- hasAccessToURL:inTab:
--
-- ObjC selector: @- permissionStatusForMatchPattern:inTab:@
permissionStatusForMatchPattern_inTab :: (IsWKWebExtensionContext wkWebExtensionContext, IsWKWebExtensionMatchPattern pattern_) => wkWebExtensionContext -> pattern_ -> RawId -> IO WKWebExtensionContextPermissionStatus
permissionStatusForMatchPattern_inTab wkWebExtensionContext pattern_ tab =
  sendMessage wkWebExtensionContext permissionStatusForMatchPattern_inTabSelector (toWKWebExtensionMatchPattern pattern_) tab

-- | Sets the status of a match pattern with a distant future expiration date.
--
-- @status@ — The new permission status to set for the given match pattern.
--
-- @pattern@ — The match pattern for which to set the status.
--
-- This method will update ``grantedPermissionMatchPatterns`` and ``deniedPermissionMatchPatterns``. Use this method for changing a single match pattern's status. Only ``WKWebExtensionContextPermissionStatusDeniedExplicitly``, ``WKWebExtensionContextPermissionStatusUnknown``, and ``WKWebExtensionContextPermissionStatusGrantedExplicitly`` states are allowed to be set using this method.
--
-- setPermissionStatus:forMatchPattern:expirationDate:
--
-- setPermissionStatus:forMatchPattern:inTab:
--
-- ObjC selector: @- setPermissionStatus:forMatchPattern:@
setPermissionStatus_forMatchPattern :: (IsWKWebExtensionContext wkWebExtensionContext, IsWKWebExtensionMatchPattern pattern_) => wkWebExtensionContext -> WKWebExtensionContextPermissionStatus -> pattern_ -> IO ()
setPermissionStatus_forMatchPattern wkWebExtensionContext status pattern_ =
  sendMessage wkWebExtensionContext setPermissionStatus_forMatchPatternSelector status (toWKWebExtensionMatchPattern pattern_)

-- | Sets the status of a match pattern with a specific expiration date.
--
-- @status@ — The new permission status to set for the given match pattern.
--
-- @pattern@ — The match pattern for which to set the status.
--
-- @expirationDate@ — The expiration date for the new permission status, or @nil@ for distant future.
--
-- This method will update ``grantedPermissionMatchPatterns`` and ``deniedPermissionMatchPatterns``. Use this method for changing a single match pattern's status. Passing a @nil@ expiration date will be treated as a distant future date. Only ``WKWebExtensionContextPermissionStatusDeniedExplicitly``, ``WKWebExtensionContextPermissionStatusUnknown``, and ``WKWebExtensionContextPermissionStatusGrantedExplicitly`` states are allowed to be set using this method.
--
-- setPermissionStatus:forMatchPattern:
--
-- setPermissionStatus:forMatchPattern:inTab:
--
-- ObjC selector: @- setPermissionStatus:forMatchPattern:expirationDate:@
setPermissionStatus_forMatchPattern_expirationDate :: (IsWKWebExtensionContext wkWebExtensionContext, IsWKWebExtensionMatchPattern pattern_, IsNSDate expirationDate) => wkWebExtensionContext -> WKWebExtensionContextPermissionStatus -> pattern_ -> expirationDate -> IO ()
setPermissionStatus_forMatchPattern_expirationDate wkWebExtensionContext status pattern_ expirationDate =
  sendMessage wkWebExtensionContext setPermissionStatus_forMatchPattern_expirationDateSelector status (toWKWebExtensionMatchPattern pattern_) (toNSDate expirationDate)

-- | Loads the background content if needed for the extension.
--
-- @completionHandler@ — A block to be called upon completion of the loading process, with an optional error.
--
-- This method forces the loading of the background content for the extension that will otherwise be loaded on-demand during specific events. It is useful when the app requires the background content to be loaded for other reasons. If the background content is already loaded, the completion handler will be called immediately. An error will occur if the extension does not have any background content to load or loading fails.
--
-- ObjC selector: @- loadBackgroundContentWithCompletionHandler:@
loadBackgroundContentWithCompletionHandler :: IsWKWebExtensionContext wkWebExtensionContext => wkWebExtensionContext -> Ptr () -> IO ()
loadBackgroundContentWithCompletionHandler wkWebExtensionContext completionHandler =
  sendMessage wkWebExtensionContext loadBackgroundContentWithCompletionHandlerSelector completionHandler

-- | Retrieves the extension action for a given tab, or the default action if @nil@ is passed.
--
-- @tab@ — The tab for which to retrieve the extension action, or @nil@ to get the default action.
--
-- The returned object represents the action specific to the tab when provided; otherwise, it returns the default action. The default action is useful when the context is unrelated to a specific tab. When possible, specify the tab to get the most context-relevant action.
--
-- performActionForTab:
--
-- ObjC selector: @- actionForTab:@
actionForTab :: IsWKWebExtensionContext wkWebExtensionContext => wkWebExtensionContext -> RawId -> IO (Id WKWebExtensionAction)
actionForTab wkWebExtensionContext tab =
  sendMessage wkWebExtensionContext actionForTabSelector tab

-- | Performs the extension action associated with the specified tab or performs the default action if @nil@ is passed.
--
-- @tab@ — The tab for which to perform the extension action, or @nil@ to perform the default action.
--
-- Performing the action will mark the tab, if specified, as having an active user gesture. When the ``tab`` parameter is @nil@, the default action is performed. The action can either trigger an event or display a popup, depending on how the extension is configured. If the action is configured to display a popup, implementing the appropriate web extension controller delegate method is required; otherwise, no action is performed for popup actions.
--
-- ObjC selector: @- performActionForTab:@
performActionForTab :: IsWKWebExtensionContext wkWebExtensionContext => wkWebExtensionContext -> RawId -> IO ()
performActionForTab wkWebExtensionContext tab =
  sendMessage wkWebExtensionContext performActionForTabSelector tab

-- | Performs the specified command, triggering events specific to this extension.
--
-- @command@ — The command to be performed.
--
-- This method performs the given command as if it was triggered by a user gesture within the context of the focused window and active tab.
--
-- ObjC selector: @- performCommand:@
performCommand :: (IsWKWebExtensionContext wkWebExtensionContext, IsWKWebExtensionCommand command) => wkWebExtensionContext -> command -> IO ()
performCommand wkWebExtensionContext command =
  sendMessage wkWebExtensionContext performCommandSelector (toWKWebExtensionCommand command)

-- | Performs the command associated with the given event.
--
-- This method checks for a command corresponding to the provided event and performs it, if available. The app should use this method to perform any extension commands at an appropriate time in the app's event handling, like in ``sendEvent:`` of ``NSApplication`` or ``NSWindow`` subclasses.
--
-- @event@ — The event representing the user input.
--
-- Returns: Returns @YES@ if a command corresponding to the event was found and performed, @NO@ otherwise.
--
-- ObjC selector: @- performCommandForEvent:@
performCommandForEvent :: (IsWKWebExtensionContext wkWebExtensionContext, IsNSEvent event) => wkWebExtensionContext -> event -> IO Bool
performCommandForEvent wkWebExtensionContext event =
  sendMessage wkWebExtensionContext performCommandForEventSelector (toNSEvent event)

-- | Retrieves the command associated with the given event without performing it.
--
-- Returns the command that corresponds to the provided event, if such a command exists. This provides a way to programmatically determine what action would occur for a given event, without triggering the command.
--
-- @event@ — The event for which to retrieve the corresponding command.
--
-- Returns: The command associated with the event, or @nil@ if there is no such command.
--
-- ObjC selector: @- commandForEvent:@
commandForEvent :: (IsWKWebExtensionContext wkWebExtensionContext, IsNSEvent event) => wkWebExtensionContext -> event -> IO (Id WKWebExtensionCommand)
commandForEvent wkWebExtensionContext event =
  sendMessage wkWebExtensionContext commandForEventSelector (toNSEvent event)

-- | @- menuItemsForTab:@
menuItemsForTab :: IsWKWebExtensionContext wkWebExtensionContext => wkWebExtensionContext -> RawId -> IO (Id NSArray)
menuItemsForTab wkWebExtensionContext tab =
  sendMessage wkWebExtensionContext menuItemsForTabSelector tab

-- | Should be called by the app when a user gesture is performed in a specific tab.
--
-- @tab@ — The tab in which the user gesture was performed.
--
-- When a user gesture is performed in a tab, this method should be called to update the extension context. This enables the extension to be aware of the user gesture, potentially granting it access to features that require user interaction, such as @activeTab@. Not required if using ``performActionForTab:``.
--
-- hasActiveUserGestureInTab:
--
-- ObjC selector: @- userGesturePerformedInTab:@
userGesturePerformedInTab :: IsWKWebExtensionContext wkWebExtensionContext => wkWebExtensionContext -> RawId -> IO ()
userGesturePerformedInTab wkWebExtensionContext tab =
  sendMessage wkWebExtensionContext userGesturePerformedInTabSelector tab

-- | Indicates if a user gesture is currently active in the specified tab.
--
-- @tab@ — The tab for which to check for an active user gesture.
--
-- An active user gesture may influence the availability of certain permissions, such as @activeTab@. User gestures can be triggered by various user interactions with the web extension, including clicking on extension menu items, executing extension commands, or interacting with extension actions. A tab as having an active user gesture enables the extension to access features that require user interaction.
--
-- userGesturePerformedInTab:
--
-- ObjC selector: @- hasActiveUserGestureInTab:@
hasActiveUserGestureInTab :: IsWKWebExtensionContext wkWebExtensionContext => wkWebExtensionContext -> RawId -> IO Bool
hasActiveUserGestureInTab wkWebExtensionContext tab =
  sendMessage wkWebExtensionContext hasActiveUserGestureInTabSelector tab

-- | Should be called by the app to clear a user gesture in a specific tab.
--
-- @tab@ — The tab from which the user gesture should be cleared.
--
-- When a user gesture is no longer relevant in a tab, this method should be called to update the extension context. This will revoke the extension's access to features that require active user interaction, such as @activeTab@. User gestures are automatically cleared during navigation in certain scenarios; this method is needed if the app intends to clear the gesture more aggressively.
--
-- userGesturePerformedInTab:
--
-- ObjC selector: @- clearUserGestureInTab:@
clearUserGestureInTab :: IsWKWebExtensionContext wkWebExtensionContext => wkWebExtensionContext -> RawId -> IO ()
clearUserGestureInTab wkWebExtensionContext tab =
  sendMessage wkWebExtensionContext clearUserGestureInTabSelector tab

-- | Should be called by the app when a new window is opened to fire appropriate events with only this extension.
--
-- @newWindow@ — The newly opened window.
--
-- This method informs only the specific extension of the opening of a new window. If the intention is to inform all loaded extensions consistently, you should use the respective method on the extension controller instead.
--
-- didCloseWindow:
--
-- openWindows
--
-- ObjC selector: @- didOpenWindow:@
didOpenWindow :: IsWKWebExtensionContext wkWebExtensionContext => wkWebExtensionContext -> RawId -> IO ()
didOpenWindow wkWebExtensionContext newWindow =
  sendMessage wkWebExtensionContext didOpenWindowSelector newWindow

-- | Should be called by the app when a window is closed to fire appropriate events with only this extension.
--
-- @newWindow@ — The window that was closed.
--
-- This method informs only the specific extension of the closure of a window. If the intention is to inform all loaded extensions consistently, you should use the respective method on the extension controller instead.
--
-- didOpenWindow:
--
-- openWindows
--
-- ObjC selector: @- didCloseWindow:@
didCloseWindow :: IsWKWebExtensionContext wkWebExtensionContext => wkWebExtensionContext -> RawId -> IO ()
didCloseWindow wkWebExtensionContext closedWindow =
  sendMessage wkWebExtensionContext didCloseWindowSelector closedWindow

-- | Should be called by the app when a window gains focus to fire appropriate events with only this extension.
--
-- @focusedWindow@ — The window that gained focus, or @nil@ if no window has focus or a window has focus that is not visible to this extension.
--
-- This method informs only the specific extension that a window has gained focus. If the intention is to inform all loaded extensions consistently, you should use the respective method on the extension controller instead.
--
-- ObjC selector: @- didFocusWindow:@
didFocusWindow :: IsWKWebExtensionContext wkWebExtensionContext => wkWebExtensionContext -> RawId -> IO ()
didFocusWindow wkWebExtensionContext focusedWindow =
  sendMessage wkWebExtensionContext didFocusWindowSelector focusedWindow

-- | Should be called by the app when a new tab is opened to fire appropriate events with only this extension.
--
-- @newTab@ — The newly opened tab.
--
-- This method informs only the specific extension of the opening of a new tab. If the intention is to inform all loaded extensions consistently, you should use the respective method on the extension controller instead.
--
-- didCloseTab:
--
-- openTabs
--
-- ObjC selector: @- didOpenTab:@
didOpenTab :: IsWKWebExtensionContext wkWebExtensionContext => wkWebExtensionContext -> RawId -> IO ()
didOpenTab wkWebExtensionContext newTab =
  sendMessage wkWebExtensionContext didOpenTabSelector newTab

-- | Should be called by the app when a tab is closed to fire appropriate events with only this extension.
--
-- @closedTab@ — The tab that was closed.
--
-- @windowIsClosing@ — A boolean value indicating whether the window containing the tab is also closing.
--
-- This method informs only the specific extension of the closure of a tab. If the intention is to inform all loaded extensions consistently, you should use the respective method on the extension controller instead.
--
-- didOpenTab:
--
-- openTabs
--
-- ObjC selector: @- didCloseTab:windowIsClosing:@
didCloseTab_windowIsClosing :: IsWKWebExtensionContext wkWebExtensionContext => wkWebExtensionContext -> RawId -> Bool -> IO ()
didCloseTab_windowIsClosing wkWebExtensionContext closedTab windowIsClosing =
  sendMessage wkWebExtensionContext didCloseTab_windowIsClosingSelector closedTab windowIsClosing

-- | Should be called by the app when a tab is activated to notify only this specific extension.
--
-- @activatedTab@ — The tab that has become active.
--
-- @previousTab@ — The tab that was active before. This parameter can be @nil@ if there was no previously active tab.
--
-- This method informs only the specific extension of the tab activation. If the intention is to inform all loaded extensions consistently, you should use the respective method on the extension controller instead.
--
-- ObjC selector: @- didActivateTab:previousActiveTab:@
didActivateTab_previousActiveTab :: IsWKWebExtensionContext wkWebExtensionContext => wkWebExtensionContext -> RawId -> RawId -> IO ()
didActivateTab_previousActiveTab wkWebExtensionContext activatedTab previousTab =
  sendMessage wkWebExtensionContext didActivateTab_previousActiveTabSelector activatedTab previousTab

-- | Should be called by the app when tabs are selected to fire appropriate events with only this extension.
--
-- @selectedTabs@ — The set of tabs that were selected.
--
-- This method informs only the specific extension that tabs have been selected. If the intention is to inform all loaded extensions consistently, you should use the respective method on the extension controller instead.
--
-- ObjC selector: @- didSelectTabs:@
didSelectTabs :: (IsWKWebExtensionContext wkWebExtensionContext, IsNSArray selectedTabs) => wkWebExtensionContext -> selectedTabs -> IO ()
didSelectTabs wkWebExtensionContext selectedTabs =
  sendMessage wkWebExtensionContext didSelectTabsSelector (toNSArray selectedTabs)

-- | Should be called by the app when tabs are deselected to fire appropriate events with only this extension.
--
-- @deselectedTabs@ — The set of tabs that were deselected.
--
-- This method informs only the specific extension that tabs have been deselected. If the intention is to inform all loaded extensions consistently, you should use the respective method on the extension controller instead.
--
-- ObjC selector: @- didDeselectTabs:@
didDeselectTabs :: (IsWKWebExtensionContext wkWebExtensionContext, IsNSArray deselectedTabs) => wkWebExtensionContext -> deselectedTabs -> IO ()
didDeselectTabs wkWebExtensionContext deselectedTabs =
  sendMessage wkWebExtensionContext didDeselectTabsSelector (toNSArray deselectedTabs)

-- | Should be called by the app when a tab is moved to fire appropriate events with only this extension.
--
-- @movedTab@ — The tab that was moved.
--
-- @index@ — The old index of the tab within the window.
--
-- @oldWindow@ — The window that the tab was moved from, or @nil@ if the tab is moving from no open window.
--
-- If the window is staying the same, the current window should be specified. This method informs only the specific extension that a tab has been moved. If the intention is to inform all loaded extensions consistently, you should use the respective method on the extension controller instead.
--
-- ObjC selector: @- didMoveTab:fromIndex:inWindow:@
didMoveTab_fromIndex_inWindow :: IsWKWebExtensionContext wkWebExtensionContext => wkWebExtensionContext -> RawId -> CULong -> RawId -> IO ()
didMoveTab_fromIndex_inWindow wkWebExtensionContext movedTab index oldWindow =
  sendMessage wkWebExtensionContext didMoveTab_fromIndex_inWindowSelector movedTab index oldWindow

-- | Should be called by the app when a tab is replaced by another tab to fire appropriate events with only this extension.
--
-- @oldTab@ — The tab that was replaced.
--
-- @newTab@ — The tab that replaced the old tab.
--
-- This method informs only the specific extension that a tab has been replaced. If the intention is to inform all loaded extensions consistently, you should use the respective method on the extension controller instead.
--
-- ObjC selector: @- didReplaceTab:withTab:@
didReplaceTab_withTab :: IsWKWebExtensionContext wkWebExtensionContext => wkWebExtensionContext -> RawId -> RawId -> IO ()
didReplaceTab_withTab wkWebExtensionContext oldTab newTab =
  sendMessage wkWebExtensionContext didReplaceTab_withTabSelector oldTab newTab

-- | Should be called by the app when the properties of a tab are changed to fire appropriate events with only this extension.
--
-- @properties@ — The properties of the tab that were changed.
--
-- @changedTab@ — The tab whose properties were changed.
--
-- This method informs only the specific extension of the changes to a tab's properties. If the intention is to inform all loaded extensions consistently, you should use the respective method on the extension controller instead.
--
-- ObjC selector: @- didChangeTabProperties:forTab:@
didChangeTabProperties_forTab :: IsWKWebExtensionContext wkWebExtensionContext => wkWebExtensionContext -> WKWebExtensionTabChangedProperties -> RawId -> IO ()
didChangeTabProperties_forTab wkWebExtensionContext properties changedTab =
  sendMessage wkWebExtensionContext didChangeTabProperties_forTabSelector properties changedTab

-- | The extension this context represents.
--
-- ObjC selector: @- webExtension@
webExtension :: IsWKWebExtensionContext wkWebExtensionContext => wkWebExtensionContext -> IO (Id WKWebExtension)
webExtension wkWebExtensionContext =
  sendMessage wkWebExtensionContext webExtensionSelector

-- | The extension controller this context is loaded in, otherwise @nil@ if it isn't loaded.
--
-- ObjC selector: @- webExtensionController@
webExtensionController :: IsWKWebExtensionContext wkWebExtensionContext => wkWebExtensionContext -> IO (Id WKWebExtensionController)
webExtensionController wkWebExtensionContext =
  sendMessage wkWebExtensionContext webExtensionControllerSelector

-- | A Boolean value indicating if this context is loaded in an extension controller.
--
-- ObjC selector: @- loaded@
loaded :: IsWKWebExtensionContext wkWebExtensionContext => wkWebExtensionContext -> IO Bool
loaded wkWebExtensionContext =
  sendMessage wkWebExtensionContext loadedSelector

-- | All errors that occurred in the extension context.
--
-- Provides an array of all parse-time and runtime errors for the extension and extension context, with repeat errors consolidated into a single entry for the original occurrence. If no errors occurred, an empty array is returned.
--
-- ObjC selector: @- errors@
errors :: IsWKWebExtensionContext wkWebExtensionContext => wkWebExtensionContext -> IO (Id NSArray)
errors wkWebExtensionContext =
  sendMessage wkWebExtensionContext errorsSelector

-- | The base URL the context uses for loading extension resources or injecting content into webpages.
--
-- The default value is a unique URL using the @webkit-extension@ scheme. The base URL can be set to any URL, but only the scheme and host will be used. The scheme cannot be a scheme that is already supported by ``WKWebView`` (e.g. http, https, etc.) Setting is only allowed when the context is not loaded.
--
-- ObjC selector: @- baseURL@
baseURL :: IsWKWebExtensionContext wkWebExtensionContext => wkWebExtensionContext -> IO (Id NSURL)
baseURL wkWebExtensionContext =
  sendMessage wkWebExtensionContext baseURLSelector

-- | The base URL the context uses for loading extension resources or injecting content into webpages.
--
-- The default value is a unique URL using the @webkit-extension@ scheme. The base URL can be set to any URL, but only the scheme and host will be used. The scheme cannot be a scheme that is already supported by ``WKWebView`` (e.g. http, https, etc.) Setting is only allowed when the context is not loaded.
--
-- ObjC selector: @- setBaseURL:@
setBaseURL :: (IsWKWebExtensionContext wkWebExtensionContext, IsNSURL value) => wkWebExtensionContext -> value -> IO ()
setBaseURL wkWebExtensionContext value =
  sendMessage wkWebExtensionContext setBaseURLSelector (toNSURL value)

-- | A unique identifier used to distinguish the extension from other extensions and target it for messages.
--
-- The default value is a unique value that matches the host in the default base URL. The identifier can be any value that is unique. Setting is only allowed when the context is not loaded. This value is accessible by the extension via @browser.runtime.id@ and is used for messaging the extension via @browser.runtime.sendMessage()@.
--
-- ObjC selector: @- uniqueIdentifier@
uniqueIdentifier :: IsWKWebExtensionContext wkWebExtensionContext => wkWebExtensionContext -> IO (Id NSString)
uniqueIdentifier wkWebExtensionContext =
  sendMessage wkWebExtensionContext uniqueIdentifierSelector

-- | A unique identifier used to distinguish the extension from other extensions and target it for messages.
--
-- The default value is a unique value that matches the host in the default base URL. The identifier can be any value that is unique. Setting is only allowed when the context is not loaded. This value is accessible by the extension via @browser.runtime.id@ and is used for messaging the extension via @browser.runtime.sendMessage()@.
--
-- ObjC selector: @- setUniqueIdentifier:@
setUniqueIdentifier :: (IsWKWebExtensionContext wkWebExtensionContext, IsNSString value) => wkWebExtensionContext -> value -> IO ()
setUniqueIdentifier wkWebExtensionContext value =
  sendMessage wkWebExtensionContext setUniqueIdentifierSelector (toNSString value)

-- | Determines whether Web Inspector can inspect the ``WKWebView`` instances for this context.
--
-- A context can control multiple ``WKWebView`` instances, from the background content, to the popover. You should set this to @YES@ when needed for debugging purposes. The default value is @NO@.
--
-- ObjC selector: @- inspectable@
inspectable :: IsWKWebExtensionContext wkWebExtensionContext => wkWebExtensionContext -> IO Bool
inspectable wkWebExtensionContext =
  sendMessage wkWebExtensionContext inspectableSelector

-- | Determines whether Web Inspector can inspect the ``WKWebView`` instances for this context.
--
-- A context can control multiple ``WKWebView`` instances, from the background content, to the popover. You should set this to @YES@ when needed for debugging purposes. The default value is @NO@.
--
-- ObjC selector: @- setInspectable:@
setInspectable :: IsWKWebExtensionContext wkWebExtensionContext => wkWebExtensionContext -> Bool -> IO ()
setInspectable wkWebExtensionContext value =
  sendMessage wkWebExtensionContext setInspectableSelector value

-- | The name shown when inspecting the background web view.
--
-- This is the text that will appear when inspecting the background web view.
--
-- ObjC selector: @- inspectionName@
inspectionName :: IsWKWebExtensionContext wkWebExtensionContext => wkWebExtensionContext -> IO (Id NSString)
inspectionName wkWebExtensionContext =
  sendMessage wkWebExtensionContext inspectionNameSelector

-- | The name shown when inspecting the background web view.
--
-- This is the text that will appear when inspecting the background web view.
--
-- ObjC selector: @- setInspectionName:@
setInspectionName :: (IsWKWebExtensionContext wkWebExtensionContext, IsNSString value) => wkWebExtensionContext -> value -> IO ()
setInspectionName wkWebExtensionContext value =
  sendMessage wkWebExtensionContext setInspectionNameSelector (toNSString value)

-- | Specifies unsupported APIs for this extension, making them @undefined@ in JavaScript.
--
-- This property allows the app to specify a subset of web extension APIs that it chooses not to support, effectively making these APIs @undefined@ within the extension's JavaScript contexts. This enables extensions to employ feature detection techniques for unsupported APIs, allowing them to adapt their behavior based on the APIs actually supported by the app. Setting is only allowed when the context is not loaded. Only certain APIs can be specified here, particularly those within the @browser@ namespace and other dynamic functions and properties, anything else will be silently ignored.
--
-- Note: For example, specifying @"browser.windows.create"@ and @"browser.storage"@ in this set will result in the @browser.windows.create()@ function and @browser.storage@ property being @undefined@.
--
-- ObjC selector: @- unsupportedAPIs@
unsupportedAPIs :: IsWKWebExtensionContext wkWebExtensionContext => wkWebExtensionContext -> IO (Id NSSet)
unsupportedAPIs wkWebExtensionContext =
  sendMessage wkWebExtensionContext unsupportedAPIsSelector

-- | Specifies unsupported APIs for this extension, making them @undefined@ in JavaScript.
--
-- This property allows the app to specify a subset of web extension APIs that it chooses not to support, effectively making these APIs @undefined@ within the extension's JavaScript contexts. This enables extensions to employ feature detection techniques for unsupported APIs, allowing them to adapt their behavior based on the APIs actually supported by the app. Setting is only allowed when the context is not loaded. Only certain APIs can be specified here, particularly those within the @browser@ namespace and other dynamic functions and properties, anything else will be silently ignored.
--
-- Note: For example, specifying @"browser.windows.create"@ and @"browser.storage"@ in this set will result in the @browser.windows.create()@ function and @browser.storage@ property being @undefined@.
--
-- ObjC selector: @- setUnsupportedAPIs:@
setUnsupportedAPIs :: (IsWKWebExtensionContext wkWebExtensionContext, IsNSSet value) => wkWebExtensionContext -> value -> IO ()
setUnsupportedAPIs wkWebExtensionContext value =
  sendMessage wkWebExtensionContext setUnsupportedAPIsSelector (toNSSet value)

-- | The web view configuration to use for web views that load pages from this extension.
--
-- Returns a customized copy of the configuration, originally set in the web extension controller configuration, for this extension. The app must use this configuration when initializing web views intended to navigate to a URL originating from this extension's base URL. The app must also swap web views in tabs when navigating to and from web extension URLs. This property returns @nil@ if the context isn't associated with a web extension controller. The returned configuration copy can be customized prior to web view initialization.
--
-- Note: Navigations will fail if a web view using this configuration attempts to navigate to a URL that doesn't originate from this extension's base URL. Similarly, navigations will be canceled if a web view not configured with this configuration attempts to navigate to a URL that does originate from this extension's base URL.
--
-- ObjC selector: @- webViewConfiguration@
webViewConfiguration :: IsWKWebExtensionContext wkWebExtensionContext => wkWebExtensionContext -> IO (Id WKWebViewConfiguration)
webViewConfiguration wkWebExtensionContext =
  sendMessage wkWebExtensionContext webViewConfigurationSelector

-- | The URL of the extension's options page, if the extension has one.
--
-- Provides the URL for the dedicated options page, if provided by the extension; otherwise @nil@ if no page is defined. The app should provide access to this page through a user interface element.
--
-- Note: Navigation to the options page is only possible after this extension has been loaded.
--
-- webViewConfiguration
--
-- ObjC selector: @- optionsPageURL@
optionsPageURL :: IsWKWebExtensionContext wkWebExtensionContext => wkWebExtensionContext -> IO (Id NSURL)
optionsPageURL wkWebExtensionContext =
  sendMessage wkWebExtensionContext optionsPageURLSelector

-- | The URL to use as an alternative to the default new tab page, if the extension has one.
--
-- Provides the URL for a new tab page, if provided by the extension; otherwise @nil@ if no page is defined. The app should prompt the user for permission to use the extension's new tab page as the default.
--
-- Note: Navigation to the override new tab page is only possible after this extension has been loaded.
--
-- webViewConfiguration
--
-- ObjC selector: @- overrideNewTabPageURL@
overrideNewTabPageURL :: IsWKWebExtensionContext wkWebExtensionContext => wkWebExtensionContext -> IO (Id NSURL)
overrideNewTabPageURL wkWebExtensionContext =
  sendMessage wkWebExtensionContext overrideNewTabPageURLSelector

-- | The currently granted permissions and their expiration dates.
--
-- Permissions that don't expire will have a distant future date. This will never include expired entries at time of access. Setting this property will replace all existing entries. Use this property for saving and restoring permission status in bulk. Permissions in this dictionary should be explicitly granted by the user before being added. Any permissions in this collection will not be presented for approval again until they expire. This value should be saved and restored as needed by the app.
--
-- setPermissionStatus:forPermission:
--
-- setPermissionStatus:forPermission:expirationDate:
--
-- ObjC selector: @- grantedPermissions@
grantedPermissions :: IsWKWebExtensionContext wkWebExtensionContext => wkWebExtensionContext -> IO (Id NSDictionary)
grantedPermissions wkWebExtensionContext =
  sendMessage wkWebExtensionContext grantedPermissionsSelector

-- | The currently granted permissions and their expiration dates.
--
-- Permissions that don't expire will have a distant future date. This will never include expired entries at time of access. Setting this property will replace all existing entries. Use this property for saving and restoring permission status in bulk. Permissions in this dictionary should be explicitly granted by the user before being added. Any permissions in this collection will not be presented for approval again until they expire. This value should be saved and restored as needed by the app.
--
-- setPermissionStatus:forPermission:
--
-- setPermissionStatus:forPermission:expirationDate:
--
-- ObjC selector: @- setGrantedPermissions:@
setGrantedPermissions :: (IsWKWebExtensionContext wkWebExtensionContext, IsNSDictionary value) => wkWebExtensionContext -> value -> IO ()
setGrantedPermissions wkWebExtensionContext value =
  sendMessage wkWebExtensionContext setGrantedPermissionsSelector (toNSDictionary value)

-- | The currently granted permission match patterns and their expiration dates.
--
-- Match patterns that don't expire will have a distant future date. This will never include expired entries at time of access. Setting this property will replace all existing entries. Use this property for saving and restoring permission status in bulk. Match patterns in this dictionary should be explicitly granted by the user before being added. Any match pattern in this collection will not be presented for approval again until they expire. This value should be saved and restored as needed by the app.
--
-- setPermissionStatus:forMatchPattern:
--
-- setPermissionStatus:forMatchPattern:expirationDate:
--
-- ObjC selector: @- grantedPermissionMatchPatterns@
grantedPermissionMatchPatterns :: IsWKWebExtensionContext wkWebExtensionContext => wkWebExtensionContext -> IO (Id NSDictionary)
grantedPermissionMatchPatterns wkWebExtensionContext =
  sendMessage wkWebExtensionContext grantedPermissionMatchPatternsSelector

-- | The currently granted permission match patterns and their expiration dates.
--
-- Match patterns that don't expire will have a distant future date. This will never include expired entries at time of access. Setting this property will replace all existing entries. Use this property for saving and restoring permission status in bulk. Match patterns in this dictionary should be explicitly granted by the user before being added. Any match pattern in this collection will not be presented for approval again until they expire. This value should be saved and restored as needed by the app.
--
-- setPermissionStatus:forMatchPattern:
--
-- setPermissionStatus:forMatchPattern:expirationDate:
--
-- ObjC selector: @- setGrantedPermissionMatchPatterns:@
setGrantedPermissionMatchPatterns :: (IsWKWebExtensionContext wkWebExtensionContext, IsNSDictionary value) => wkWebExtensionContext -> value -> IO ()
setGrantedPermissionMatchPatterns wkWebExtensionContext value =
  sendMessage wkWebExtensionContext setGrantedPermissionMatchPatternsSelector (toNSDictionary value)

-- | The currently denied permissions and their expiration dates.
--
-- Permissions that don't expire will have a distant future date. This will never include expired entries at time of access. Setting this property will replace all existing entries. Use this property for saving and restoring permission status in bulk. Permissions in this dictionary should be explicitly denied by the user before being added. Any match pattern in this collection will not be presented for approval again until they expire. This value should be saved and restored as needed by the app.
--
-- setPermissionStatus:forPermission:
--
-- setPermissionStatus:forPermission:expirationDate:
--
-- ObjC selector: @- deniedPermissions@
deniedPermissions :: IsWKWebExtensionContext wkWebExtensionContext => wkWebExtensionContext -> IO (Id NSDictionary)
deniedPermissions wkWebExtensionContext =
  sendMessage wkWebExtensionContext deniedPermissionsSelector

-- | The currently denied permissions and their expiration dates.
--
-- Permissions that don't expire will have a distant future date. This will never include expired entries at time of access. Setting this property will replace all existing entries. Use this property for saving and restoring permission status in bulk. Permissions in this dictionary should be explicitly denied by the user before being added. Any match pattern in this collection will not be presented for approval again until they expire. This value should be saved and restored as needed by the app.
--
-- setPermissionStatus:forPermission:
--
-- setPermissionStatus:forPermission:expirationDate:
--
-- ObjC selector: @- setDeniedPermissions:@
setDeniedPermissions :: (IsWKWebExtensionContext wkWebExtensionContext, IsNSDictionary value) => wkWebExtensionContext -> value -> IO ()
setDeniedPermissions wkWebExtensionContext value =
  sendMessage wkWebExtensionContext setDeniedPermissionsSelector (toNSDictionary value)

-- | The currently denied permission match patterns and their expiration dates.
--
-- Match patterns that don't expire will have a distant future date. This will never include expired entries at time of access. Setting this property will replace all existing entries. Use this property for saving and restoring permission status in bulk. Match patterns in this dictionary should be explicitly denied by the user before being added. Any match pattern in this collection will not be presented for approval again until they expire. This value should be saved and restored as needed by the app.
--
-- setPermissionStatus:forMatchPattern:
--
-- setPermissionStatus:forMatchPattern:expirationDate:
--
-- ObjC selector: @- deniedPermissionMatchPatterns@
deniedPermissionMatchPatterns :: IsWKWebExtensionContext wkWebExtensionContext => wkWebExtensionContext -> IO (Id NSDictionary)
deniedPermissionMatchPatterns wkWebExtensionContext =
  sendMessage wkWebExtensionContext deniedPermissionMatchPatternsSelector

-- | The currently denied permission match patterns and their expiration dates.
--
-- Match patterns that don't expire will have a distant future date. This will never include expired entries at time of access. Setting this property will replace all existing entries. Use this property for saving and restoring permission status in bulk. Match patterns in this dictionary should be explicitly denied by the user before being added. Any match pattern in this collection will not be presented for approval again until they expire. This value should be saved and restored as needed by the app.
--
-- setPermissionStatus:forMatchPattern:
--
-- setPermissionStatus:forMatchPattern:expirationDate:
--
-- ObjC selector: @- setDeniedPermissionMatchPatterns:@
setDeniedPermissionMatchPatterns :: (IsWKWebExtensionContext wkWebExtensionContext, IsNSDictionary value) => wkWebExtensionContext -> value -> IO ()
setDeniedPermissionMatchPatterns wkWebExtensionContext value =
  sendMessage wkWebExtensionContext setDeniedPermissionMatchPatternsSelector (toNSDictionary value)

-- | A Boolean value indicating if the extension has requested optional access to all hosts.
--
-- If this property is @YES@, the extension has asked for access to all hosts in a call to @browser.runtime.permissions.request()@, and future permission checks will present discrete hosts for approval as being implicitly requested. This value should be saved and restored as needed by the app.
--
-- ObjC selector: @- hasRequestedOptionalAccessToAllHosts@
hasRequestedOptionalAccessToAllHosts :: IsWKWebExtensionContext wkWebExtensionContext => wkWebExtensionContext -> IO Bool
hasRequestedOptionalAccessToAllHosts wkWebExtensionContext =
  sendMessage wkWebExtensionContext hasRequestedOptionalAccessToAllHostsSelector

-- | A Boolean value indicating if the extension has requested optional access to all hosts.
--
-- If this property is @YES@, the extension has asked for access to all hosts in a call to @browser.runtime.permissions.request()@, and future permission checks will present discrete hosts for approval as being implicitly requested. This value should be saved and restored as needed by the app.
--
-- ObjC selector: @- setHasRequestedOptionalAccessToAllHosts:@
setHasRequestedOptionalAccessToAllHosts :: IsWKWebExtensionContext wkWebExtensionContext => wkWebExtensionContext -> Bool -> IO ()
setHasRequestedOptionalAccessToAllHosts wkWebExtensionContext value =
  sendMessage wkWebExtensionContext setHasRequestedOptionalAccessToAllHostsSelector value

-- | A Boolean value indicating if the extension has access to private data.
--
-- If this property is @YES@, the extension is granted permission to interact with private windows, tabs, and cookies. Access to private data should be explicitly allowed by the user before setting this property. This value should be saved and restored as needed by the app.
--
-- Note: To ensure proper isolation between private and non-private data, web views associated with private data must use a different ``WKUserContentController``. Likewise, to be identified as a private web view and to ensure that cookies and other website data is not shared, private web views must be configured to use a non-persistent ``WKWebsiteDataStore``.
--
-- ObjC selector: @- hasAccessToPrivateData@
hasAccessToPrivateData :: IsWKWebExtensionContext wkWebExtensionContext => wkWebExtensionContext -> IO Bool
hasAccessToPrivateData wkWebExtensionContext =
  sendMessage wkWebExtensionContext hasAccessToPrivateDataSelector

-- | A Boolean value indicating if the extension has access to private data.
--
-- If this property is @YES@, the extension is granted permission to interact with private windows, tabs, and cookies. Access to private data should be explicitly allowed by the user before setting this property. This value should be saved and restored as needed by the app.
--
-- Note: To ensure proper isolation between private and non-private data, web views associated with private data must use a different ``WKUserContentController``. Likewise, to be identified as a private web view and to ensure that cookies and other website data is not shared, private web views must be configured to use a non-persistent ``WKWebsiteDataStore``.
--
-- ObjC selector: @- setHasAccessToPrivateData:@
setHasAccessToPrivateData :: IsWKWebExtensionContext wkWebExtensionContext => wkWebExtensionContext -> Bool -> IO ()
setHasAccessToPrivateData wkWebExtensionContext value =
  sendMessage wkWebExtensionContext setHasAccessToPrivateDataSelector value

-- | The currently granted permissions that have not expired.
--
-- grantedPermissions
--
-- ObjC selector: @- currentPermissions@
currentPermissions :: IsWKWebExtensionContext wkWebExtensionContext => wkWebExtensionContext -> IO (Id NSSet)
currentPermissions wkWebExtensionContext =
  sendMessage wkWebExtensionContext currentPermissionsSelector

-- | The currently granted permission match patterns that have not expired.
--
-- grantedPermissionMatchPatterns
--
-- ObjC selector: @- currentPermissionMatchPatterns@
currentPermissionMatchPatterns :: IsWKWebExtensionContext wkWebExtensionContext => wkWebExtensionContext -> IO (Id NSSet)
currentPermissionMatchPatterns wkWebExtensionContext =
  sendMessage wkWebExtensionContext currentPermissionMatchPatternsSelector

-- | A Boolean value indicating if the currently granted permission match patterns set contains the `<all_urls>` pattern.
--
-- This does not check for any @*@ host patterns. In most cases you should use the broader ``hasAccessToAllHosts``.
--
-- currentPermissionMatchPatterns
--
-- hasAccessToAllHosts
--
-- ObjC selector: @- hasAccessToAllURLs@
hasAccessToAllURLs :: IsWKWebExtensionContext wkWebExtensionContext => wkWebExtensionContext -> IO Bool
hasAccessToAllURLs wkWebExtensionContext =
  sendMessage wkWebExtensionContext hasAccessToAllURLsSelector

-- | A Boolean value indicating if the currently granted permission match patterns set contains the `<all_urls>@ pattern or any @*` host patterns.
--
-- currentPermissionMatchPatterns
--
-- hasAccessToAllURLs
--
-- ObjC selector: @- hasAccessToAllHosts@
hasAccessToAllHosts :: IsWKWebExtensionContext wkWebExtensionContext => wkWebExtensionContext -> IO Bool
hasAccessToAllHosts wkWebExtensionContext =
  sendMessage wkWebExtensionContext hasAccessToAllHostsSelector

-- | A Boolean value indicating whether the extension has script or stylesheet content that can be injected into webpages.
--
-- If this property is @YES@, the extension has content that can be injected by matching against the extension's requested match patterns.
--
-- hasInjectedContentForURL:
--
-- ObjC selector: @- hasInjectedContent@
hasInjectedContent :: IsWKWebExtensionContext wkWebExtensionContext => wkWebExtensionContext -> IO Bool
hasInjectedContent wkWebExtensionContext =
  sendMessage wkWebExtensionContext hasInjectedContentSelector

-- | A boolean value indicating whether the extension includes rules used for content modification or blocking.
--
-- This includes both static rules available in the extension's manifest and dynamic rules applied during a browsing session.
--
-- ObjC selector: @- hasContentModificationRules@
hasContentModificationRules :: IsWKWebExtensionContext wkWebExtensionContext => wkWebExtensionContext -> IO Bool
hasContentModificationRules wkWebExtensionContext =
  sendMessage wkWebExtensionContext hasContentModificationRulesSelector

-- | The commands associated with the extension.
--
-- Provides all commands registered within the extension. Each command represents an action or behavior available for the web extension.
--
-- performCommand:
--
-- ObjC selector: @- commands@
commands :: IsWKWebExtensionContext wkWebExtensionContext => wkWebExtensionContext -> IO (Id NSArray)
commands wkWebExtensionContext =
  sendMessage wkWebExtensionContext commandsSelector

-- | The open windows that are exposed to this extension.
--
-- Provides the windows that are open and visible to the extension, as updated by the ``didOpenWindow:`` and ``didCloseWindow:`` methods. Initially populated by the windows returned by the extension controller delegate method ``webExtensionController:openWindowsForExtensionContext:``.
--
-- didOpenWindow:
--
-- didCloseWindow:
--
-- ObjC selector: @- openWindows@
openWindows :: IsWKWebExtensionContext wkWebExtensionContext => wkWebExtensionContext -> IO (Id NSArray)
openWindows wkWebExtensionContext =
  sendMessage wkWebExtensionContext openWindowsSelector

-- | The window that currently has focus for this extension.
--
-- Provides the window that currently has focus, as set by the ``didFocusWindow:`` method. It will be @nil@ if no window has focus or if a window has focus that is not visible to the extension. Initially populated by the window returned by the extension controller delegate method ``webExtensionController:focusedWindowForExtensionContext:``.
--
-- didFocusWindow:
--
-- ObjC selector: @- focusedWindow@
focusedWindow :: IsWKWebExtensionContext wkWebExtensionContext => wkWebExtensionContext -> IO RawId
focusedWindow wkWebExtensionContext =
  sendMessage wkWebExtensionContext focusedWindowSelector

-- | A set of open tabs in all open windows that are exposed to this extension.
--
-- Provides a set of tabs in all open windows that are visible to the extension, as updated by the ``didOpenTab:`` and ``didCloseTab:`` methods. Initially populated by the tabs in the windows returned by the extension controller delegate method ``webExtensionController:openWindowsForExtensionContext:``.
--
-- didOpenTab:
--
-- didCloseTab:
--
-- ObjC selector: @- openTabs@
openTabs :: IsWKWebExtensionContext wkWebExtensionContext => wkWebExtensionContext -> IO (Id NSSet)
openTabs wkWebExtensionContext =
  sendMessage wkWebExtensionContext openTabsSelector

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @new@
newSelector :: Selector '[] (Id WKWebExtensionContext)
newSelector = mkSelector "new"

-- | @Selector@ for @init@
initSelector :: Selector '[] (Id WKWebExtensionContext)
initSelector = mkSelector "init"

-- | @Selector@ for @contextForExtension:@
contextForExtensionSelector :: Selector '[Id WKWebExtension] (Id WKWebExtensionContext)
contextForExtensionSelector = mkSelector "contextForExtension:"

-- | @Selector@ for @initForExtension:@
initForExtensionSelector :: Selector '[Id WKWebExtension] (Id WKWebExtensionContext)
initForExtensionSelector = mkSelector "initForExtension:"

-- | @Selector@ for @hasPermission:@
hasPermissionSelector :: Selector '[Id NSString] Bool
hasPermissionSelector = mkSelector "hasPermission:"

-- | @Selector@ for @hasPermission:inTab:@
hasPermission_inTabSelector :: Selector '[Id NSString, RawId] Bool
hasPermission_inTabSelector = mkSelector "hasPermission:inTab:"

-- | @Selector@ for @hasAccessToURL:@
hasAccessToURLSelector :: Selector '[Id NSURL] Bool
hasAccessToURLSelector = mkSelector "hasAccessToURL:"

-- | @Selector@ for @hasAccessToURL:inTab:@
hasAccessToURL_inTabSelector :: Selector '[Id NSURL, RawId] Bool
hasAccessToURL_inTabSelector = mkSelector "hasAccessToURL:inTab:"

-- | @Selector@ for @hasInjectedContentForURL:@
hasInjectedContentForURLSelector :: Selector '[Id NSURL] Bool
hasInjectedContentForURLSelector = mkSelector "hasInjectedContentForURL:"

-- | @Selector@ for @permissionStatusForPermission:@
permissionStatusForPermissionSelector :: Selector '[Id NSString] WKWebExtensionContextPermissionStatus
permissionStatusForPermissionSelector = mkSelector "permissionStatusForPermission:"

-- | @Selector@ for @permissionStatusForPermission:inTab:@
permissionStatusForPermission_inTabSelector :: Selector '[Id NSString, RawId] WKWebExtensionContextPermissionStatus
permissionStatusForPermission_inTabSelector = mkSelector "permissionStatusForPermission:inTab:"

-- | @Selector@ for @setPermissionStatus:forPermission:@
setPermissionStatus_forPermissionSelector :: Selector '[WKWebExtensionContextPermissionStatus, Id NSString] ()
setPermissionStatus_forPermissionSelector = mkSelector "setPermissionStatus:forPermission:"

-- | @Selector@ for @setPermissionStatus:forPermission:expirationDate:@
setPermissionStatus_forPermission_expirationDateSelector :: Selector '[WKWebExtensionContextPermissionStatus, Id NSString, Id NSDate] ()
setPermissionStatus_forPermission_expirationDateSelector = mkSelector "setPermissionStatus:forPermission:expirationDate:"

-- | @Selector@ for @permissionStatusForURL:@
permissionStatusForURLSelector :: Selector '[Id NSURL] WKWebExtensionContextPermissionStatus
permissionStatusForURLSelector = mkSelector "permissionStatusForURL:"

-- | @Selector@ for @permissionStatusForURL:inTab:@
permissionStatusForURL_inTabSelector :: Selector '[Id NSURL, RawId] WKWebExtensionContextPermissionStatus
permissionStatusForURL_inTabSelector = mkSelector "permissionStatusForURL:inTab:"

-- | @Selector@ for @setPermissionStatus:forURL:@
setPermissionStatus_forURLSelector :: Selector '[WKWebExtensionContextPermissionStatus, Id NSURL] ()
setPermissionStatus_forURLSelector = mkSelector "setPermissionStatus:forURL:"

-- | @Selector@ for @setPermissionStatus:forURL:expirationDate:@
setPermissionStatus_forURL_expirationDateSelector :: Selector '[WKWebExtensionContextPermissionStatus, Id NSURL, Id NSDate] ()
setPermissionStatus_forURL_expirationDateSelector = mkSelector "setPermissionStatus:forURL:expirationDate:"

-- | @Selector@ for @permissionStatusForMatchPattern:@
permissionStatusForMatchPatternSelector :: Selector '[Id WKWebExtensionMatchPattern] WKWebExtensionContextPermissionStatus
permissionStatusForMatchPatternSelector = mkSelector "permissionStatusForMatchPattern:"

-- | @Selector@ for @permissionStatusForMatchPattern:inTab:@
permissionStatusForMatchPattern_inTabSelector :: Selector '[Id WKWebExtensionMatchPattern, RawId] WKWebExtensionContextPermissionStatus
permissionStatusForMatchPattern_inTabSelector = mkSelector "permissionStatusForMatchPattern:inTab:"

-- | @Selector@ for @setPermissionStatus:forMatchPattern:@
setPermissionStatus_forMatchPatternSelector :: Selector '[WKWebExtensionContextPermissionStatus, Id WKWebExtensionMatchPattern] ()
setPermissionStatus_forMatchPatternSelector = mkSelector "setPermissionStatus:forMatchPattern:"

-- | @Selector@ for @setPermissionStatus:forMatchPattern:expirationDate:@
setPermissionStatus_forMatchPattern_expirationDateSelector :: Selector '[WKWebExtensionContextPermissionStatus, Id WKWebExtensionMatchPattern, Id NSDate] ()
setPermissionStatus_forMatchPattern_expirationDateSelector = mkSelector "setPermissionStatus:forMatchPattern:expirationDate:"

-- | @Selector@ for @loadBackgroundContentWithCompletionHandler:@
loadBackgroundContentWithCompletionHandlerSelector :: Selector '[Ptr ()] ()
loadBackgroundContentWithCompletionHandlerSelector = mkSelector "loadBackgroundContentWithCompletionHandler:"

-- | @Selector@ for @actionForTab:@
actionForTabSelector :: Selector '[RawId] (Id WKWebExtensionAction)
actionForTabSelector = mkSelector "actionForTab:"

-- | @Selector@ for @performActionForTab:@
performActionForTabSelector :: Selector '[RawId] ()
performActionForTabSelector = mkSelector "performActionForTab:"

-- | @Selector@ for @performCommand:@
performCommandSelector :: Selector '[Id WKWebExtensionCommand] ()
performCommandSelector = mkSelector "performCommand:"

-- | @Selector@ for @performCommandForEvent:@
performCommandForEventSelector :: Selector '[Id NSEvent] Bool
performCommandForEventSelector = mkSelector "performCommandForEvent:"

-- | @Selector@ for @commandForEvent:@
commandForEventSelector :: Selector '[Id NSEvent] (Id WKWebExtensionCommand)
commandForEventSelector = mkSelector "commandForEvent:"

-- | @Selector@ for @menuItemsForTab:@
menuItemsForTabSelector :: Selector '[RawId] (Id NSArray)
menuItemsForTabSelector = mkSelector "menuItemsForTab:"

-- | @Selector@ for @userGesturePerformedInTab:@
userGesturePerformedInTabSelector :: Selector '[RawId] ()
userGesturePerformedInTabSelector = mkSelector "userGesturePerformedInTab:"

-- | @Selector@ for @hasActiveUserGestureInTab:@
hasActiveUserGestureInTabSelector :: Selector '[RawId] Bool
hasActiveUserGestureInTabSelector = mkSelector "hasActiveUserGestureInTab:"

-- | @Selector@ for @clearUserGestureInTab:@
clearUserGestureInTabSelector :: Selector '[RawId] ()
clearUserGestureInTabSelector = mkSelector "clearUserGestureInTab:"

-- | @Selector@ for @didOpenWindow:@
didOpenWindowSelector :: Selector '[RawId] ()
didOpenWindowSelector = mkSelector "didOpenWindow:"

-- | @Selector@ for @didCloseWindow:@
didCloseWindowSelector :: Selector '[RawId] ()
didCloseWindowSelector = mkSelector "didCloseWindow:"

-- | @Selector@ for @didFocusWindow:@
didFocusWindowSelector :: Selector '[RawId] ()
didFocusWindowSelector = mkSelector "didFocusWindow:"

-- | @Selector@ for @didOpenTab:@
didOpenTabSelector :: Selector '[RawId] ()
didOpenTabSelector = mkSelector "didOpenTab:"

-- | @Selector@ for @didCloseTab:windowIsClosing:@
didCloseTab_windowIsClosingSelector :: Selector '[RawId, Bool] ()
didCloseTab_windowIsClosingSelector = mkSelector "didCloseTab:windowIsClosing:"

-- | @Selector@ for @didActivateTab:previousActiveTab:@
didActivateTab_previousActiveTabSelector :: Selector '[RawId, RawId] ()
didActivateTab_previousActiveTabSelector = mkSelector "didActivateTab:previousActiveTab:"

-- | @Selector@ for @didSelectTabs:@
didSelectTabsSelector :: Selector '[Id NSArray] ()
didSelectTabsSelector = mkSelector "didSelectTabs:"

-- | @Selector@ for @didDeselectTabs:@
didDeselectTabsSelector :: Selector '[Id NSArray] ()
didDeselectTabsSelector = mkSelector "didDeselectTabs:"

-- | @Selector@ for @didMoveTab:fromIndex:inWindow:@
didMoveTab_fromIndex_inWindowSelector :: Selector '[RawId, CULong, RawId] ()
didMoveTab_fromIndex_inWindowSelector = mkSelector "didMoveTab:fromIndex:inWindow:"

-- | @Selector@ for @didReplaceTab:withTab:@
didReplaceTab_withTabSelector :: Selector '[RawId, RawId] ()
didReplaceTab_withTabSelector = mkSelector "didReplaceTab:withTab:"

-- | @Selector@ for @didChangeTabProperties:forTab:@
didChangeTabProperties_forTabSelector :: Selector '[WKWebExtensionTabChangedProperties, RawId] ()
didChangeTabProperties_forTabSelector = mkSelector "didChangeTabProperties:forTab:"

-- | @Selector@ for @webExtension@
webExtensionSelector :: Selector '[] (Id WKWebExtension)
webExtensionSelector = mkSelector "webExtension"

-- | @Selector@ for @webExtensionController@
webExtensionControllerSelector :: Selector '[] (Id WKWebExtensionController)
webExtensionControllerSelector = mkSelector "webExtensionController"

-- | @Selector@ for @loaded@
loadedSelector :: Selector '[] Bool
loadedSelector = mkSelector "loaded"

-- | @Selector@ for @errors@
errorsSelector :: Selector '[] (Id NSArray)
errorsSelector = mkSelector "errors"

-- | @Selector@ for @baseURL@
baseURLSelector :: Selector '[] (Id NSURL)
baseURLSelector = mkSelector "baseURL"

-- | @Selector@ for @setBaseURL:@
setBaseURLSelector :: Selector '[Id NSURL] ()
setBaseURLSelector = mkSelector "setBaseURL:"

-- | @Selector@ for @uniqueIdentifier@
uniqueIdentifierSelector :: Selector '[] (Id NSString)
uniqueIdentifierSelector = mkSelector "uniqueIdentifier"

-- | @Selector@ for @setUniqueIdentifier:@
setUniqueIdentifierSelector :: Selector '[Id NSString] ()
setUniqueIdentifierSelector = mkSelector "setUniqueIdentifier:"

-- | @Selector@ for @inspectable@
inspectableSelector :: Selector '[] Bool
inspectableSelector = mkSelector "inspectable"

-- | @Selector@ for @setInspectable:@
setInspectableSelector :: Selector '[Bool] ()
setInspectableSelector = mkSelector "setInspectable:"

-- | @Selector@ for @inspectionName@
inspectionNameSelector :: Selector '[] (Id NSString)
inspectionNameSelector = mkSelector "inspectionName"

-- | @Selector@ for @setInspectionName:@
setInspectionNameSelector :: Selector '[Id NSString] ()
setInspectionNameSelector = mkSelector "setInspectionName:"

-- | @Selector@ for @unsupportedAPIs@
unsupportedAPIsSelector :: Selector '[] (Id NSSet)
unsupportedAPIsSelector = mkSelector "unsupportedAPIs"

-- | @Selector@ for @setUnsupportedAPIs:@
setUnsupportedAPIsSelector :: Selector '[Id NSSet] ()
setUnsupportedAPIsSelector = mkSelector "setUnsupportedAPIs:"

-- | @Selector@ for @webViewConfiguration@
webViewConfigurationSelector :: Selector '[] (Id WKWebViewConfiguration)
webViewConfigurationSelector = mkSelector "webViewConfiguration"

-- | @Selector@ for @optionsPageURL@
optionsPageURLSelector :: Selector '[] (Id NSURL)
optionsPageURLSelector = mkSelector "optionsPageURL"

-- | @Selector@ for @overrideNewTabPageURL@
overrideNewTabPageURLSelector :: Selector '[] (Id NSURL)
overrideNewTabPageURLSelector = mkSelector "overrideNewTabPageURL"

-- | @Selector@ for @grantedPermissions@
grantedPermissionsSelector :: Selector '[] (Id NSDictionary)
grantedPermissionsSelector = mkSelector "grantedPermissions"

-- | @Selector@ for @setGrantedPermissions:@
setGrantedPermissionsSelector :: Selector '[Id NSDictionary] ()
setGrantedPermissionsSelector = mkSelector "setGrantedPermissions:"

-- | @Selector@ for @grantedPermissionMatchPatterns@
grantedPermissionMatchPatternsSelector :: Selector '[] (Id NSDictionary)
grantedPermissionMatchPatternsSelector = mkSelector "grantedPermissionMatchPatterns"

-- | @Selector@ for @setGrantedPermissionMatchPatterns:@
setGrantedPermissionMatchPatternsSelector :: Selector '[Id NSDictionary] ()
setGrantedPermissionMatchPatternsSelector = mkSelector "setGrantedPermissionMatchPatterns:"

-- | @Selector@ for @deniedPermissions@
deniedPermissionsSelector :: Selector '[] (Id NSDictionary)
deniedPermissionsSelector = mkSelector "deniedPermissions"

-- | @Selector@ for @setDeniedPermissions:@
setDeniedPermissionsSelector :: Selector '[Id NSDictionary] ()
setDeniedPermissionsSelector = mkSelector "setDeniedPermissions:"

-- | @Selector@ for @deniedPermissionMatchPatterns@
deniedPermissionMatchPatternsSelector :: Selector '[] (Id NSDictionary)
deniedPermissionMatchPatternsSelector = mkSelector "deniedPermissionMatchPatterns"

-- | @Selector@ for @setDeniedPermissionMatchPatterns:@
setDeniedPermissionMatchPatternsSelector :: Selector '[Id NSDictionary] ()
setDeniedPermissionMatchPatternsSelector = mkSelector "setDeniedPermissionMatchPatterns:"

-- | @Selector@ for @hasRequestedOptionalAccessToAllHosts@
hasRequestedOptionalAccessToAllHostsSelector :: Selector '[] Bool
hasRequestedOptionalAccessToAllHostsSelector = mkSelector "hasRequestedOptionalAccessToAllHosts"

-- | @Selector@ for @setHasRequestedOptionalAccessToAllHosts:@
setHasRequestedOptionalAccessToAllHostsSelector :: Selector '[Bool] ()
setHasRequestedOptionalAccessToAllHostsSelector = mkSelector "setHasRequestedOptionalAccessToAllHosts:"

-- | @Selector@ for @hasAccessToPrivateData@
hasAccessToPrivateDataSelector :: Selector '[] Bool
hasAccessToPrivateDataSelector = mkSelector "hasAccessToPrivateData"

-- | @Selector@ for @setHasAccessToPrivateData:@
setHasAccessToPrivateDataSelector :: Selector '[Bool] ()
setHasAccessToPrivateDataSelector = mkSelector "setHasAccessToPrivateData:"

-- | @Selector@ for @currentPermissions@
currentPermissionsSelector :: Selector '[] (Id NSSet)
currentPermissionsSelector = mkSelector "currentPermissions"

-- | @Selector@ for @currentPermissionMatchPatterns@
currentPermissionMatchPatternsSelector :: Selector '[] (Id NSSet)
currentPermissionMatchPatternsSelector = mkSelector "currentPermissionMatchPatterns"

-- | @Selector@ for @hasAccessToAllURLs@
hasAccessToAllURLsSelector :: Selector '[] Bool
hasAccessToAllURLsSelector = mkSelector "hasAccessToAllURLs"

-- | @Selector@ for @hasAccessToAllHosts@
hasAccessToAllHostsSelector :: Selector '[] Bool
hasAccessToAllHostsSelector = mkSelector "hasAccessToAllHosts"

-- | @Selector@ for @hasInjectedContent@
hasInjectedContentSelector :: Selector '[] Bool
hasInjectedContentSelector = mkSelector "hasInjectedContent"

-- | @Selector@ for @hasContentModificationRules@
hasContentModificationRulesSelector :: Selector '[] Bool
hasContentModificationRulesSelector = mkSelector "hasContentModificationRules"

-- | @Selector@ for @commands@
commandsSelector :: Selector '[] (Id NSArray)
commandsSelector = mkSelector "commands"

-- | @Selector@ for @openWindows@
openWindowsSelector :: Selector '[] (Id NSArray)
openWindowsSelector = mkSelector "openWindows"

-- | @Selector@ for @focusedWindow@
focusedWindowSelector :: Selector '[] RawId
focusedWindowSelector = mkSelector "focusedWindow"

-- | @Selector@ for @openTabs@
openTabsSelector :: Selector '[] (Id NSSet)
openTabsSelector = mkSelector "openTabs"

