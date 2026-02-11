{-# LANGUAGE PatternSynonyms #-}
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
  , newSelector
  , initSelector
  , contextForExtensionSelector
  , initForExtensionSelector
  , hasPermissionSelector
  , hasPermission_inTabSelector
  , hasAccessToURLSelector
  , hasAccessToURL_inTabSelector
  , hasInjectedContentForURLSelector
  , permissionStatusForPermissionSelector
  , permissionStatusForPermission_inTabSelector
  , setPermissionStatus_forPermissionSelector
  , setPermissionStatus_forPermission_expirationDateSelector
  , permissionStatusForURLSelector
  , permissionStatusForURL_inTabSelector
  , setPermissionStatus_forURLSelector
  , setPermissionStatus_forURL_expirationDateSelector
  , permissionStatusForMatchPatternSelector
  , permissionStatusForMatchPattern_inTabSelector
  , setPermissionStatus_forMatchPatternSelector
  , setPermissionStatus_forMatchPattern_expirationDateSelector
  , loadBackgroundContentWithCompletionHandlerSelector
  , actionForTabSelector
  , performActionForTabSelector
  , performCommandSelector
  , performCommandForEventSelector
  , commandForEventSelector
  , menuItemsForTabSelector
  , userGesturePerformedInTabSelector
  , hasActiveUserGestureInTabSelector
  , clearUserGestureInTabSelector
  , didOpenWindowSelector
  , didCloseWindowSelector
  , didFocusWindowSelector
  , didOpenTabSelector
  , didCloseTab_windowIsClosingSelector
  , didActivateTab_previousActiveTabSelector
  , didSelectTabsSelector
  , didDeselectTabsSelector
  , didMoveTab_fromIndex_inWindowSelector
  , didReplaceTab_withTabSelector
  , didChangeTabProperties_forTabSelector
  , webExtensionSelector
  , webExtensionControllerSelector
  , loadedSelector
  , errorsSelector
  , baseURLSelector
  , setBaseURLSelector
  , uniqueIdentifierSelector
  , setUniqueIdentifierSelector
  , inspectableSelector
  , setInspectableSelector
  , inspectionNameSelector
  , setInspectionNameSelector
  , unsupportedAPIsSelector
  , setUnsupportedAPIsSelector
  , webViewConfigurationSelector
  , optionsPageURLSelector
  , overrideNewTabPageURLSelector
  , grantedPermissionsSelector
  , setGrantedPermissionsSelector
  , grantedPermissionMatchPatternsSelector
  , setGrantedPermissionMatchPatternsSelector
  , deniedPermissionsSelector
  , setDeniedPermissionsSelector
  , deniedPermissionMatchPatternsSelector
  , setDeniedPermissionMatchPatternsSelector
  , hasRequestedOptionalAccessToAllHostsSelector
  , setHasRequestedOptionalAccessToAllHostsSelector
  , hasAccessToPrivateDataSelector
  , setHasAccessToPrivateDataSelector
  , currentPermissionsSelector
  , currentPermissionMatchPatternsSelector
  , hasAccessToAllURLsSelector
  , hasAccessToAllHostsSelector
  , hasInjectedContentSelector
  , hasContentModificationRulesSelector
  , commandsSelector

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
import ObjC.WebKit.Internal.Enums
import ObjC.AppKit.Internal.Classes
import ObjC.Foundation.Internal.Classes

-- | @+ new@
new :: IO (Id WKWebExtensionContext)
new  =
  do
    cls' <- getRequiredClass "WKWebExtensionContext"
    sendClassMsg cls' (mkSelector "new") (retPtr retVoid) [] >>= ownedObject . castPtr

-- | @- init@
init_ :: IsWKWebExtensionContext wkWebExtensionContext => wkWebExtensionContext -> IO (Id WKWebExtensionContext)
init_ wkWebExtensionContext  =
  sendMsg wkWebExtensionContext (mkSelector "init") (retPtr retVoid) [] >>= ownedObject . castPtr

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
    withObjCPtr extension $ \raw_extension ->
      sendClassMsg cls' (mkSelector "contextForExtension:") (retPtr retVoid) [argPtr (castPtr raw_extension :: Ptr ())] >>= retainedObject . castPtr

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
initForExtension wkWebExtensionContext  extension =
withObjCPtr extension $ \raw_extension ->
    sendMsg wkWebExtensionContext (mkSelector "initForExtension:") (retPtr retVoid) [argPtr (castPtr raw_extension :: Ptr ())] >>= ownedObject . castPtr

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
hasPermission wkWebExtensionContext  permission =
withObjCPtr permission $ \raw_permission ->
    fmap ((/= 0) :: CULong -> Bool) $ sendMsg wkWebExtensionContext (mkSelector "hasPermission:") retCULong [argPtr (castPtr raw_permission :: Ptr ())]

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
hasPermission_inTab wkWebExtensionContext  permission tab =
withObjCPtr permission $ \raw_permission ->
    fmap ((/= 0) :: CULong -> Bool) $ sendMsg wkWebExtensionContext (mkSelector "hasPermission:inTab:") retCULong [argPtr (castPtr raw_permission :: Ptr ()), argPtr (castPtr (unRawId tab) :: Ptr ())]

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
hasAccessToURL wkWebExtensionContext  url =
withObjCPtr url $ \raw_url ->
    fmap ((/= 0) :: CULong -> Bool) $ sendMsg wkWebExtensionContext (mkSelector "hasAccessToURL:") retCULong [argPtr (castPtr raw_url :: Ptr ())]

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
hasAccessToURL_inTab wkWebExtensionContext  url tab =
withObjCPtr url $ \raw_url ->
    fmap ((/= 0) :: CULong -> Bool) $ sendMsg wkWebExtensionContext (mkSelector "hasAccessToURL:inTab:") retCULong [argPtr (castPtr raw_url :: Ptr ()), argPtr (castPtr (unRawId tab) :: Ptr ())]

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
hasInjectedContentForURL wkWebExtensionContext  url =
withObjCPtr url $ \raw_url ->
    fmap ((/= 0) :: CULong -> Bool) $ sendMsg wkWebExtensionContext (mkSelector "hasInjectedContentForURL:") retCULong [argPtr (castPtr raw_url :: Ptr ())]

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
permissionStatusForPermission wkWebExtensionContext  permission =
withObjCPtr permission $ \raw_permission ->
    fmap (coerce :: CLong -> WKWebExtensionContextPermissionStatus) $ sendMsg wkWebExtensionContext (mkSelector "permissionStatusForPermission:") retCLong [argPtr (castPtr raw_permission :: Ptr ())]

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
permissionStatusForPermission_inTab wkWebExtensionContext  permission tab =
withObjCPtr permission $ \raw_permission ->
    fmap (coerce :: CLong -> WKWebExtensionContextPermissionStatus) $ sendMsg wkWebExtensionContext (mkSelector "permissionStatusForPermission:inTab:") retCLong [argPtr (castPtr raw_permission :: Ptr ()), argPtr (castPtr (unRawId tab) :: Ptr ())]

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
setPermissionStatus_forPermission wkWebExtensionContext  status permission =
withObjCPtr permission $ \raw_permission ->
    sendMsg wkWebExtensionContext (mkSelector "setPermissionStatus:forPermission:") retVoid [argCLong (coerce status), argPtr (castPtr raw_permission :: Ptr ())]

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
setPermissionStatus_forPermission_expirationDate wkWebExtensionContext  status permission expirationDate =
withObjCPtr permission $ \raw_permission ->
  withObjCPtr expirationDate $ \raw_expirationDate ->
      sendMsg wkWebExtensionContext (mkSelector "setPermissionStatus:forPermission:expirationDate:") retVoid [argCLong (coerce status), argPtr (castPtr raw_permission :: Ptr ()), argPtr (castPtr raw_expirationDate :: Ptr ())]

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
permissionStatusForURL wkWebExtensionContext  url =
withObjCPtr url $ \raw_url ->
    fmap (coerce :: CLong -> WKWebExtensionContextPermissionStatus) $ sendMsg wkWebExtensionContext (mkSelector "permissionStatusForURL:") retCLong [argPtr (castPtr raw_url :: Ptr ())]

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
permissionStatusForURL_inTab wkWebExtensionContext  url tab =
withObjCPtr url $ \raw_url ->
    fmap (coerce :: CLong -> WKWebExtensionContextPermissionStatus) $ sendMsg wkWebExtensionContext (mkSelector "permissionStatusForURL:inTab:") retCLong [argPtr (castPtr raw_url :: Ptr ()), argPtr (castPtr (unRawId tab) :: Ptr ())]

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
setPermissionStatus_forURL wkWebExtensionContext  status url =
withObjCPtr url $ \raw_url ->
    sendMsg wkWebExtensionContext (mkSelector "setPermissionStatus:forURL:") retVoid [argCLong (coerce status), argPtr (castPtr raw_url :: Ptr ())]

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
setPermissionStatus_forURL_expirationDate wkWebExtensionContext  status url expirationDate =
withObjCPtr url $ \raw_url ->
  withObjCPtr expirationDate $ \raw_expirationDate ->
      sendMsg wkWebExtensionContext (mkSelector "setPermissionStatus:forURL:expirationDate:") retVoid [argCLong (coerce status), argPtr (castPtr raw_url :: Ptr ()), argPtr (castPtr raw_expirationDate :: Ptr ())]

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
permissionStatusForMatchPattern wkWebExtensionContext  pattern_ =
withObjCPtr pattern_ $ \raw_pattern_ ->
    fmap (coerce :: CLong -> WKWebExtensionContextPermissionStatus) $ sendMsg wkWebExtensionContext (mkSelector "permissionStatusForMatchPattern:") retCLong [argPtr (castPtr raw_pattern_ :: Ptr ())]

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
permissionStatusForMatchPattern_inTab wkWebExtensionContext  pattern_ tab =
withObjCPtr pattern_ $ \raw_pattern_ ->
    fmap (coerce :: CLong -> WKWebExtensionContextPermissionStatus) $ sendMsg wkWebExtensionContext (mkSelector "permissionStatusForMatchPattern:inTab:") retCLong [argPtr (castPtr raw_pattern_ :: Ptr ()), argPtr (castPtr (unRawId tab) :: Ptr ())]

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
setPermissionStatus_forMatchPattern wkWebExtensionContext  status pattern_ =
withObjCPtr pattern_ $ \raw_pattern_ ->
    sendMsg wkWebExtensionContext (mkSelector "setPermissionStatus:forMatchPattern:") retVoid [argCLong (coerce status), argPtr (castPtr raw_pattern_ :: Ptr ())]

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
setPermissionStatus_forMatchPattern_expirationDate wkWebExtensionContext  status pattern_ expirationDate =
withObjCPtr pattern_ $ \raw_pattern_ ->
  withObjCPtr expirationDate $ \raw_expirationDate ->
      sendMsg wkWebExtensionContext (mkSelector "setPermissionStatus:forMatchPattern:expirationDate:") retVoid [argCLong (coerce status), argPtr (castPtr raw_pattern_ :: Ptr ()), argPtr (castPtr raw_expirationDate :: Ptr ())]

-- | Loads the background content if needed for the extension.
--
-- @completionHandler@ — A block to be called upon completion of the loading process, with an optional error.
--
-- This method forces the loading of the background content for the extension that will otherwise be loaded on-demand during specific events. It is useful when the app requires the background content to be loaded for other reasons. If the background content is already loaded, the completion handler will be called immediately. An error will occur if the extension does not have any background content to load or loading fails.
--
-- ObjC selector: @- loadBackgroundContentWithCompletionHandler:@
loadBackgroundContentWithCompletionHandler :: IsWKWebExtensionContext wkWebExtensionContext => wkWebExtensionContext -> Ptr () -> IO ()
loadBackgroundContentWithCompletionHandler wkWebExtensionContext  completionHandler =
  sendMsg wkWebExtensionContext (mkSelector "loadBackgroundContentWithCompletionHandler:") retVoid [argPtr (castPtr completionHandler :: Ptr ())]

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
actionForTab wkWebExtensionContext  tab =
  sendMsg wkWebExtensionContext (mkSelector "actionForTab:") (retPtr retVoid) [argPtr (castPtr (unRawId tab) :: Ptr ())] >>= retainedObject . castPtr

-- | Performs the extension action associated with the specified tab or performs the default action if @nil@ is passed.
--
-- @tab@ — The tab for which to perform the extension action, or @nil@ to perform the default action.
--
-- Performing the action will mark the tab, if specified, as having an active user gesture. When the ``tab`` parameter is @nil@, the default action is performed. The action can either trigger an event or display a popup, depending on how the extension is configured. If the action is configured to display a popup, implementing the appropriate web extension controller delegate method is required; otherwise, no action is performed for popup actions.
--
-- ObjC selector: @- performActionForTab:@
performActionForTab :: IsWKWebExtensionContext wkWebExtensionContext => wkWebExtensionContext -> RawId -> IO ()
performActionForTab wkWebExtensionContext  tab =
  sendMsg wkWebExtensionContext (mkSelector "performActionForTab:") retVoid [argPtr (castPtr (unRawId tab) :: Ptr ())]

-- | Performs the specified command, triggering events specific to this extension.
--
-- @command@ — The command to be performed.
--
-- This method performs the given command as if it was triggered by a user gesture within the context of the focused window and active tab.
--
-- ObjC selector: @- performCommand:@
performCommand :: (IsWKWebExtensionContext wkWebExtensionContext, IsWKWebExtensionCommand command) => wkWebExtensionContext -> command -> IO ()
performCommand wkWebExtensionContext  command =
withObjCPtr command $ \raw_command ->
    sendMsg wkWebExtensionContext (mkSelector "performCommand:") retVoid [argPtr (castPtr raw_command :: Ptr ())]

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
performCommandForEvent wkWebExtensionContext  event =
withObjCPtr event $ \raw_event ->
    fmap ((/= 0) :: CULong -> Bool) $ sendMsg wkWebExtensionContext (mkSelector "performCommandForEvent:") retCULong [argPtr (castPtr raw_event :: Ptr ())]

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
commandForEvent wkWebExtensionContext  event =
withObjCPtr event $ \raw_event ->
    sendMsg wkWebExtensionContext (mkSelector "commandForEvent:") (retPtr retVoid) [argPtr (castPtr raw_event :: Ptr ())] >>= retainedObject . castPtr

-- | @- menuItemsForTab:@
menuItemsForTab :: IsWKWebExtensionContext wkWebExtensionContext => wkWebExtensionContext -> RawId -> IO (Id NSArray)
menuItemsForTab wkWebExtensionContext  tab =
  sendMsg wkWebExtensionContext (mkSelector "menuItemsForTab:") (retPtr retVoid) [argPtr (castPtr (unRawId tab) :: Ptr ())] >>= retainedObject . castPtr

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
userGesturePerformedInTab wkWebExtensionContext  tab =
  sendMsg wkWebExtensionContext (mkSelector "userGesturePerformedInTab:") retVoid [argPtr (castPtr (unRawId tab) :: Ptr ())]

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
hasActiveUserGestureInTab wkWebExtensionContext  tab =
  fmap ((/= 0) :: CULong -> Bool) $ sendMsg wkWebExtensionContext (mkSelector "hasActiveUserGestureInTab:") retCULong [argPtr (castPtr (unRawId tab) :: Ptr ())]

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
clearUserGestureInTab wkWebExtensionContext  tab =
  sendMsg wkWebExtensionContext (mkSelector "clearUserGestureInTab:") retVoid [argPtr (castPtr (unRawId tab) :: Ptr ())]

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
didOpenWindow wkWebExtensionContext  newWindow =
  sendMsg wkWebExtensionContext (mkSelector "didOpenWindow:") retVoid [argPtr (castPtr (unRawId newWindow) :: Ptr ())]

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
didCloseWindow wkWebExtensionContext  closedWindow =
  sendMsg wkWebExtensionContext (mkSelector "didCloseWindow:") retVoid [argPtr (castPtr (unRawId closedWindow) :: Ptr ())]

-- | Should be called by the app when a window gains focus to fire appropriate events with only this extension.
--
-- @focusedWindow@ — The window that gained focus, or @nil@ if no window has focus or a window has focus that is not visible to this extension.
--
-- This method informs only the specific extension that a window has gained focus. If the intention is to inform all loaded extensions consistently, you should use the respective method on the extension controller instead.
--
-- ObjC selector: @- didFocusWindow:@
didFocusWindow :: IsWKWebExtensionContext wkWebExtensionContext => wkWebExtensionContext -> RawId -> IO ()
didFocusWindow wkWebExtensionContext  focusedWindow =
  sendMsg wkWebExtensionContext (mkSelector "didFocusWindow:") retVoid [argPtr (castPtr (unRawId focusedWindow) :: Ptr ())]

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
didOpenTab wkWebExtensionContext  newTab =
  sendMsg wkWebExtensionContext (mkSelector "didOpenTab:") retVoid [argPtr (castPtr (unRawId newTab) :: Ptr ())]

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
didCloseTab_windowIsClosing wkWebExtensionContext  closedTab windowIsClosing =
  sendMsg wkWebExtensionContext (mkSelector "didCloseTab:windowIsClosing:") retVoid [argPtr (castPtr (unRawId closedTab) :: Ptr ()), argCULong (if windowIsClosing then 1 else 0)]

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
didActivateTab_previousActiveTab wkWebExtensionContext  activatedTab previousTab =
  sendMsg wkWebExtensionContext (mkSelector "didActivateTab:previousActiveTab:") retVoid [argPtr (castPtr (unRawId activatedTab) :: Ptr ()), argPtr (castPtr (unRawId previousTab) :: Ptr ())]

-- | Should be called by the app when tabs are selected to fire appropriate events with only this extension.
--
-- @selectedTabs@ — The set of tabs that were selected.
--
-- This method informs only the specific extension that tabs have been selected. If the intention is to inform all loaded extensions consistently, you should use the respective method on the extension controller instead.
--
-- ObjC selector: @- didSelectTabs:@
didSelectTabs :: (IsWKWebExtensionContext wkWebExtensionContext, IsNSArray selectedTabs) => wkWebExtensionContext -> selectedTabs -> IO ()
didSelectTabs wkWebExtensionContext  selectedTabs =
withObjCPtr selectedTabs $ \raw_selectedTabs ->
    sendMsg wkWebExtensionContext (mkSelector "didSelectTabs:") retVoid [argPtr (castPtr raw_selectedTabs :: Ptr ())]

-- | Should be called by the app when tabs are deselected to fire appropriate events with only this extension.
--
-- @deselectedTabs@ — The set of tabs that were deselected.
--
-- This method informs only the specific extension that tabs have been deselected. If the intention is to inform all loaded extensions consistently, you should use the respective method on the extension controller instead.
--
-- ObjC selector: @- didDeselectTabs:@
didDeselectTabs :: (IsWKWebExtensionContext wkWebExtensionContext, IsNSArray deselectedTabs) => wkWebExtensionContext -> deselectedTabs -> IO ()
didDeselectTabs wkWebExtensionContext  deselectedTabs =
withObjCPtr deselectedTabs $ \raw_deselectedTabs ->
    sendMsg wkWebExtensionContext (mkSelector "didDeselectTabs:") retVoid [argPtr (castPtr raw_deselectedTabs :: Ptr ())]

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
didMoveTab_fromIndex_inWindow wkWebExtensionContext  movedTab index oldWindow =
  sendMsg wkWebExtensionContext (mkSelector "didMoveTab:fromIndex:inWindow:") retVoid [argPtr (castPtr (unRawId movedTab) :: Ptr ()), argCULong (fromIntegral index), argPtr (castPtr (unRawId oldWindow) :: Ptr ())]

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
didReplaceTab_withTab wkWebExtensionContext  oldTab newTab =
  sendMsg wkWebExtensionContext (mkSelector "didReplaceTab:withTab:") retVoid [argPtr (castPtr (unRawId oldTab) :: Ptr ()), argPtr (castPtr (unRawId newTab) :: Ptr ())]

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
didChangeTabProperties_forTab wkWebExtensionContext  properties changedTab =
  sendMsg wkWebExtensionContext (mkSelector "didChangeTabProperties:forTab:") retVoid [argCULong (coerce properties), argPtr (castPtr (unRawId changedTab) :: Ptr ())]

-- | The extension this context represents.
--
-- ObjC selector: @- webExtension@
webExtension :: IsWKWebExtensionContext wkWebExtensionContext => wkWebExtensionContext -> IO (Id WKWebExtension)
webExtension wkWebExtensionContext  =
  sendMsg wkWebExtensionContext (mkSelector "webExtension") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | The extension controller this context is loaded in, otherwise @nil@ if it isn't loaded.
--
-- ObjC selector: @- webExtensionController@
webExtensionController :: IsWKWebExtensionContext wkWebExtensionContext => wkWebExtensionContext -> IO (Id WKWebExtensionController)
webExtensionController wkWebExtensionContext  =
  sendMsg wkWebExtensionContext (mkSelector "webExtensionController") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | A Boolean value indicating if this context is loaded in an extension controller.
--
-- ObjC selector: @- loaded@
loaded :: IsWKWebExtensionContext wkWebExtensionContext => wkWebExtensionContext -> IO Bool
loaded wkWebExtensionContext  =
  fmap ((/= 0) :: CULong -> Bool) $ sendMsg wkWebExtensionContext (mkSelector "loaded") retCULong []

-- | All errors that occurred in the extension context.
--
-- Provides an array of all parse-time and runtime errors for the extension and extension context, with repeat errors consolidated into a single entry for the original occurrence. If no errors occurred, an empty array is returned.
--
-- ObjC selector: @- errors@
errors :: IsWKWebExtensionContext wkWebExtensionContext => wkWebExtensionContext -> IO (Id NSArray)
errors wkWebExtensionContext  =
  sendMsg wkWebExtensionContext (mkSelector "errors") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | The base URL the context uses for loading extension resources or injecting content into webpages.
--
-- The default value is a unique URL using the @webkit-extension@ scheme. The base URL can be set to any URL, but only the scheme and host will be used. The scheme cannot be a scheme that is already supported by ``WKWebView`` (e.g. http, https, etc.) Setting is only allowed when the context is not loaded.
--
-- ObjC selector: @- baseURL@
baseURL :: IsWKWebExtensionContext wkWebExtensionContext => wkWebExtensionContext -> IO (Id NSURL)
baseURL wkWebExtensionContext  =
  sendMsg wkWebExtensionContext (mkSelector "baseURL") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | The base URL the context uses for loading extension resources or injecting content into webpages.
--
-- The default value is a unique URL using the @webkit-extension@ scheme. The base URL can be set to any URL, but only the scheme and host will be used. The scheme cannot be a scheme that is already supported by ``WKWebView`` (e.g. http, https, etc.) Setting is only allowed when the context is not loaded.
--
-- ObjC selector: @- setBaseURL:@
setBaseURL :: (IsWKWebExtensionContext wkWebExtensionContext, IsNSURL value) => wkWebExtensionContext -> value -> IO ()
setBaseURL wkWebExtensionContext  value =
withObjCPtr value $ \raw_value ->
    sendMsg wkWebExtensionContext (mkSelector "setBaseURL:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- | A unique identifier used to distinguish the extension from other extensions and target it for messages.
--
-- The default value is a unique value that matches the host in the default base URL. The identifier can be any value that is unique. Setting is only allowed when the context is not loaded. This value is accessible by the extension via @browser.runtime.id@ and is used for messaging the extension via @browser.runtime.sendMessage()@.
--
-- ObjC selector: @- uniqueIdentifier@
uniqueIdentifier :: IsWKWebExtensionContext wkWebExtensionContext => wkWebExtensionContext -> IO (Id NSString)
uniqueIdentifier wkWebExtensionContext  =
  sendMsg wkWebExtensionContext (mkSelector "uniqueIdentifier") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | A unique identifier used to distinguish the extension from other extensions and target it for messages.
--
-- The default value is a unique value that matches the host in the default base URL. The identifier can be any value that is unique. Setting is only allowed when the context is not loaded. This value is accessible by the extension via @browser.runtime.id@ and is used for messaging the extension via @browser.runtime.sendMessage()@.
--
-- ObjC selector: @- setUniqueIdentifier:@
setUniqueIdentifier :: (IsWKWebExtensionContext wkWebExtensionContext, IsNSString value) => wkWebExtensionContext -> value -> IO ()
setUniqueIdentifier wkWebExtensionContext  value =
withObjCPtr value $ \raw_value ->
    sendMsg wkWebExtensionContext (mkSelector "setUniqueIdentifier:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- | Determines whether Web Inspector can inspect the ``WKWebView`` instances for this context.
--
-- A context can control multiple ``WKWebView`` instances, from the background content, to the popover. You should set this to @YES@ when needed for debugging purposes. The default value is @NO@.
--
-- ObjC selector: @- inspectable@
inspectable :: IsWKWebExtensionContext wkWebExtensionContext => wkWebExtensionContext -> IO Bool
inspectable wkWebExtensionContext  =
  fmap ((/= 0) :: CULong -> Bool) $ sendMsg wkWebExtensionContext (mkSelector "inspectable") retCULong []

-- | Determines whether Web Inspector can inspect the ``WKWebView`` instances for this context.
--
-- A context can control multiple ``WKWebView`` instances, from the background content, to the popover. You should set this to @YES@ when needed for debugging purposes. The default value is @NO@.
--
-- ObjC selector: @- setInspectable:@
setInspectable :: IsWKWebExtensionContext wkWebExtensionContext => wkWebExtensionContext -> Bool -> IO ()
setInspectable wkWebExtensionContext  value =
  sendMsg wkWebExtensionContext (mkSelector "setInspectable:") retVoid [argCULong (if value then 1 else 0)]

-- | The name shown when inspecting the background web view.
--
-- This is the text that will appear when inspecting the background web view.
--
-- ObjC selector: @- inspectionName@
inspectionName :: IsWKWebExtensionContext wkWebExtensionContext => wkWebExtensionContext -> IO (Id NSString)
inspectionName wkWebExtensionContext  =
  sendMsg wkWebExtensionContext (mkSelector "inspectionName") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | The name shown when inspecting the background web view.
--
-- This is the text that will appear when inspecting the background web view.
--
-- ObjC selector: @- setInspectionName:@
setInspectionName :: (IsWKWebExtensionContext wkWebExtensionContext, IsNSString value) => wkWebExtensionContext -> value -> IO ()
setInspectionName wkWebExtensionContext  value =
withObjCPtr value $ \raw_value ->
    sendMsg wkWebExtensionContext (mkSelector "setInspectionName:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- | Specifies unsupported APIs for this extension, making them @undefined@ in JavaScript.
--
-- This property allows the app to specify a subset of web extension APIs that it chooses not to support, effectively making these APIs @undefined@ within the extension's JavaScript contexts. This enables extensions to employ feature detection techniques for unsupported APIs, allowing them to adapt their behavior based on the APIs actually supported by the app. Setting is only allowed when the context is not loaded. Only certain APIs can be specified here, particularly those within the @browser@ namespace and other dynamic functions and properties, anything else will be silently ignored.
--
-- Note: For example, specifying @"browser.windows.create"@ and @"browser.storage"@ in this set will result in the @browser.windows.create()@ function and @browser.storage@ property being @undefined@.
--
-- ObjC selector: @- unsupportedAPIs@
unsupportedAPIs :: IsWKWebExtensionContext wkWebExtensionContext => wkWebExtensionContext -> IO (Id NSSet)
unsupportedAPIs wkWebExtensionContext  =
  sendMsg wkWebExtensionContext (mkSelector "unsupportedAPIs") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | Specifies unsupported APIs for this extension, making them @undefined@ in JavaScript.
--
-- This property allows the app to specify a subset of web extension APIs that it chooses not to support, effectively making these APIs @undefined@ within the extension's JavaScript contexts. This enables extensions to employ feature detection techniques for unsupported APIs, allowing them to adapt their behavior based on the APIs actually supported by the app. Setting is only allowed when the context is not loaded. Only certain APIs can be specified here, particularly those within the @browser@ namespace and other dynamic functions and properties, anything else will be silently ignored.
--
-- Note: For example, specifying @"browser.windows.create"@ and @"browser.storage"@ in this set will result in the @browser.windows.create()@ function and @browser.storage@ property being @undefined@.
--
-- ObjC selector: @- setUnsupportedAPIs:@
setUnsupportedAPIs :: (IsWKWebExtensionContext wkWebExtensionContext, IsNSSet value) => wkWebExtensionContext -> value -> IO ()
setUnsupportedAPIs wkWebExtensionContext  value =
withObjCPtr value $ \raw_value ->
    sendMsg wkWebExtensionContext (mkSelector "setUnsupportedAPIs:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- | The web view configuration to use for web views that load pages from this extension.
--
-- Returns a customized copy of the configuration, originally set in the web extension controller configuration, for this extension. The app must use this configuration when initializing web views intended to navigate to a URL originating from this extension's base URL. The app must also swap web views in tabs when navigating to and from web extension URLs. This property returns @nil@ if the context isn't associated with a web extension controller. The returned configuration copy can be customized prior to web view initialization.
--
-- Note: Navigations will fail if a web view using this configuration attempts to navigate to a URL that doesn't originate from this extension's base URL. Similarly, navigations will be canceled if a web view not configured with this configuration attempts to navigate to a URL that does originate from this extension's base URL.
--
-- ObjC selector: @- webViewConfiguration@
webViewConfiguration :: IsWKWebExtensionContext wkWebExtensionContext => wkWebExtensionContext -> IO (Id WKWebViewConfiguration)
webViewConfiguration wkWebExtensionContext  =
  sendMsg wkWebExtensionContext (mkSelector "webViewConfiguration") (retPtr retVoid) [] >>= retainedObject . castPtr

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
optionsPageURL wkWebExtensionContext  =
  sendMsg wkWebExtensionContext (mkSelector "optionsPageURL") (retPtr retVoid) [] >>= retainedObject . castPtr

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
overrideNewTabPageURL wkWebExtensionContext  =
  sendMsg wkWebExtensionContext (mkSelector "overrideNewTabPageURL") (retPtr retVoid) [] >>= retainedObject . castPtr

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
grantedPermissions wkWebExtensionContext  =
  sendMsg wkWebExtensionContext (mkSelector "grantedPermissions") (retPtr retVoid) [] >>= retainedObject . castPtr

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
setGrantedPermissions wkWebExtensionContext  value =
withObjCPtr value $ \raw_value ->
    sendMsg wkWebExtensionContext (mkSelector "setGrantedPermissions:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

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
grantedPermissionMatchPatterns wkWebExtensionContext  =
  sendMsg wkWebExtensionContext (mkSelector "grantedPermissionMatchPatterns") (retPtr retVoid) [] >>= retainedObject . castPtr

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
setGrantedPermissionMatchPatterns wkWebExtensionContext  value =
withObjCPtr value $ \raw_value ->
    sendMsg wkWebExtensionContext (mkSelector "setGrantedPermissionMatchPatterns:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

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
deniedPermissions wkWebExtensionContext  =
  sendMsg wkWebExtensionContext (mkSelector "deniedPermissions") (retPtr retVoid) [] >>= retainedObject . castPtr

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
setDeniedPermissions wkWebExtensionContext  value =
withObjCPtr value $ \raw_value ->
    sendMsg wkWebExtensionContext (mkSelector "setDeniedPermissions:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

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
deniedPermissionMatchPatterns wkWebExtensionContext  =
  sendMsg wkWebExtensionContext (mkSelector "deniedPermissionMatchPatterns") (retPtr retVoid) [] >>= retainedObject . castPtr

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
setDeniedPermissionMatchPatterns wkWebExtensionContext  value =
withObjCPtr value $ \raw_value ->
    sendMsg wkWebExtensionContext (mkSelector "setDeniedPermissionMatchPatterns:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- | A Boolean value indicating if the extension has requested optional access to all hosts.
--
-- If this property is @YES@, the extension has asked for access to all hosts in a call to @browser.runtime.permissions.request()@, and future permission checks will present discrete hosts for approval as being implicitly requested. This value should be saved and restored as needed by the app.
--
-- ObjC selector: @- hasRequestedOptionalAccessToAllHosts@
hasRequestedOptionalAccessToAllHosts :: IsWKWebExtensionContext wkWebExtensionContext => wkWebExtensionContext -> IO Bool
hasRequestedOptionalAccessToAllHosts wkWebExtensionContext  =
  fmap ((/= 0) :: CULong -> Bool) $ sendMsg wkWebExtensionContext (mkSelector "hasRequestedOptionalAccessToAllHosts") retCULong []

-- | A Boolean value indicating if the extension has requested optional access to all hosts.
--
-- If this property is @YES@, the extension has asked for access to all hosts in a call to @browser.runtime.permissions.request()@, and future permission checks will present discrete hosts for approval as being implicitly requested. This value should be saved and restored as needed by the app.
--
-- ObjC selector: @- setHasRequestedOptionalAccessToAllHosts:@
setHasRequestedOptionalAccessToAllHosts :: IsWKWebExtensionContext wkWebExtensionContext => wkWebExtensionContext -> Bool -> IO ()
setHasRequestedOptionalAccessToAllHosts wkWebExtensionContext  value =
  sendMsg wkWebExtensionContext (mkSelector "setHasRequestedOptionalAccessToAllHosts:") retVoid [argCULong (if value then 1 else 0)]

-- | A Boolean value indicating if the extension has access to private data.
--
-- If this property is @YES@, the extension is granted permission to interact with private windows, tabs, and cookies. Access to private data should be explicitly allowed by the user before setting this property. This value should be saved and restored as needed by the app.
--
-- Note: To ensure proper isolation between private and non-private data, web views associated with private data must use a different ``WKUserContentController``. Likewise, to be identified as a private web view and to ensure that cookies and other website data is not shared, private web views must be configured to use a non-persistent ``WKWebsiteDataStore``.
--
-- ObjC selector: @- hasAccessToPrivateData@
hasAccessToPrivateData :: IsWKWebExtensionContext wkWebExtensionContext => wkWebExtensionContext -> IO Bool
hasAccessToPrivateData wkWebExtensionContext  =
  fmap ((/= 0) :: CULong -> Bool) $ sendMsg wkWebExtensionContext (mkSelector "hasAccessToPrivateData") retCULong []

-- | A Boolean value indicating if the extension has access to private data.
--
-- If this property is @YES@, the extension is granted permission to interact with private windows, tabs, and cookies. Access to private data should be explicitly allowed by the user before setting this property. This value should be saved and restored as needed by the app.
--
-- Note: To ensure proper isolation between private and non-private data, web views associated with private data must use a different ``WKUserContentController``. Likewise, to be identified as a private web view and to ensure that cookies and other website data is not shared, private web views must be configured to use a non-persistent ``WKWebsiteDataStore``.
--
-- ObjC selector: @- setHasAccessToPrivateData:@
setHasAccessToPrivateData :: IsWKWebExtensionContext wkWebExtensionContext => wkWebExtensionContext -> Bool -> IO ()
setHasAccessToPrivateData wkWebExtensionContext  value =
  sendMsg wkWebExtensionContext (mkSelector "setHasAccessToPrivateData:") retVoid [argCULong (if value then 1 else 0)]

-- | The currently granted permissions that have not expired.
--
-- grantedPermissions
--
-- ObjC selector: @- currentPermissions@
currentPermissions :: IsWKWebExtensionContext wkWebExtensionContext => wkWebExtensionContext -> IO (Id NSSet)
currentPermissions wkWebExtensionContext  =
  sendMsg wkWebExtensionContext (mkSelector "currentPermissions") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | The currently granted permission match patterns that have not expired.
--
-- grantedPermissionMatchPatterns
--
-- ObjC selector: @- currentPermissionMatchPatterns@
currentPermissionMatchPatterns :: IsWKWebExtensionContext wkWebExtensionContext => wkWebExtensionContext -> IO (Id NSSet)
currentPermissionMatchPatterns wkWebExtensionContext  =
  sendMsg wkWebExtensionContext (mkSelector "currentPermissionMatchPatterns") (retPtr retVoid) [] >>= retainedObject . castPtr

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
hasAccessToAllURLs wkWebExtensionContext  =
  fmap ((/= 0) :: CULong -> Bool) $ sendMsg wkWebExtensionContext (mkSelector "hasAccessToAllURLs") retCULong []

-- | A Boolean value indicating if the currently granted permission match patterns set contains the `<all_urls>@ pattern or any @*` host patterns.
--
-- currentPermissionMatchPatterns
--
-- hasAccessToAllURLs
--
-- ObjC selector: @- hasAccessToAllHosts@
hasAccessToAllHosts :: IsWKWebExtensionContext wkWebExtensionContext => wkWebExtensionContext -> IO Bool
hasAccessToAllHosts wkWebExtensionContext  =
  fmap ((/= 0) :: CULong -> Bool) $ sendMsg wkWebExtensionContext (mkSelector "hasAccessToAllHosts") retCULong []

-- | A Boolean value indicating whether the extension has script or stylesheet content that can be injected into webpages.
--
-- If this property is @YES@, the extension has content that can be injected by matching against the extension's requested match patterns.
--
-- hasInjectedContentForURL:
--
-- ObjC selector: @- hasInjectedContent@
hasInjectedContent :: IsWKWebExtensionContext wkWebExtensionContext => wkWebExtensionContext -> IO Bool
hasInjectedContent wkWebExtensionContext  =
  fmap ((/= 0) :: CULong -> Bool) $ sendMsg wkWebExtensionContext (mkSelector "hasInjectedContent") retCULong []

-- | A boolean value indicating whether the extension includes rules used for content modification or blocking.
--
-- This includes both static rules available in the extension's manifest and dynamic rules applied during a browsing session.
--
-- ObjC selector: @- hasContentModificationRules@
hasContentModificationRules :: IsWKWebExtensionContext wkWebExtensionContext => wkWebExtensionContext -> IO Bool
hasContentModificationRules wkWebExtensionContext  =
  fmap ((/= 0) :: CULong -> Bool) $ sendMsg wkWebExtensionContext (mkSelector "hasContentModificationRules") retCULong []

-- | The commands associated with the extension.
--
-- Provides all commands registered within the extension. Each command represents an action or behavior available for the web extension.
--
-- performCommand:
--
-- ObjC selector: @- commands@
commands :: IsWKWebExtensionContext wkWebExtensionContext => wkWebExtensionContext -> IO (Id NSArray)
commands wkWebExtensionContext  =
  sendMsg wkWebExtensionContext (mkSelector "commands") (retPtr retVoid) [] >>= retainedObject . castPtr

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @new@
newSelector :: Selector
newSelector = mkSelector "new"

-- | @Selector@ for @init@
initSelector :: Selector
initSelector = mkSelector "init"

-- | @Selector@ for @contextForExtension:@
contextForExtensionSelector :: Selector
contextForExtensionSelector = mkSelector "contextForExtension:"

-- | @Selector@ for @initForExtension:@
initForExtensionSelector :: Selector
initForExtensionSelector = mkSelector "initForExtension:"

-- | @Selector@ for @hasPermission:@
hasPermissionSelector :: Selector
hasPermissionSelector = mkSelector "hasPermission:"

-- | @Selector@ for @hasPermission:inTab:@
hasPermission_inTabSelector :: Selector
hasPermission_inTabSelector = mkSelector "hasPermission:inTab:"

-- | @Selector@ for @hasAccessToURL:@
hasAccessToURLSelector :: Selector
hasAccessToURLSelector = mkSelector "hasAccessToURL:"

-- | @Selector@ for @hasAccessToURL:inTab:@
hasAccessToURL_inTabSelector :: Selector
hasAccessToURL_inTabSelector = mkSelector "hasAccessToURL:inTab:"

-- | @Selector@ for @hasInjectedContentForURL:@
hasInjectedContentForURLSelector :: Selector
hasInjectedContentForURLSelector = mkSelector "hasInjectedContentForURL:"

-- | @Selector@ for @permissionStatusForPermission:@
permissionStatusForPermissionSelector :: Selector
permissionStatusForPermissionSelector = mkSelector "permissionStatusForPermission:"

-- | @Selector@ for @permissionStatusForPermission:inTab:@
permissionStatusForPermission_inTabSelector :: Selector
permissionStatusForPermission_inTabSelector = mkSelector "permissionStatusForPermission:inTab:"

-- | @Selector@ for @setPermissionStatus:forPermission:@
setPermissionStatus_forPermissionSelector :: Selector
setPermissionStatus_forPermissionSelector = mkSelector "setPermissionStatus:forPermission:"

-- | @Selector@ for @setPermissionStatus:forPermission:expirationDate:@
setPermissionStatus_forPermission_expirationDateSelector :: Selector
setPermissionStatus_forPermission_expirationDateSelector = mkSelector "setPermissionStatus:forPermission:expirationDate:"

-- | @Selector@ for @permissionStatusForURL:@
permissionStatusForURLSelector :: Selector
permissionStatusForURLSelector = mkSelector "permissionStatusForURL:"

-- | @Selector@ for @permissionStatusForURL:inTab:@
permissionStatusForURL_inTabSelector :: Selector
permissionStatusForURL_inTabSelector = mkSelector "permissionStatusForURL:inTab:"

-- | @Selector@ for @setPermissionStatus:forURL:@
setPermissionStatus_forURLSelector :: Selector
setPermissionStatus_forURLSelector = mkSelector "setPermissionStatus:forURL:"

-- | @Selector@ for @setPermissionStatus:forURL:expirationDate:@
setPermissionStatus_forURL_expirationDateSelector :: Selector
setPermissionStatus_forURL_expirationDateSelector = mkSelector "setPermissionStatus:forURL:expirationDate:"

-- | @Selector@ for @permissionStatusForMatchPattern:@
permissionStatusForMatchPatternSelector :: Selector
permissionStatusForMatchPatternSelector = mkSelector "permissionStatusForMatchPattern:"

-- | @Selector@ for @permissionStatusForMatchPattern:inTab:@
permissionStatusForMatchPattern_inTabSelector :: Selector
permissionStatusForMatchPattern_inTabSelector = mkSelector "permissionStatusForMatchPattern:inTab:"

-- | @Selector@ for @setPermissionStatus:forMatchPattern:@
setPermissionStatus_forMatchPatternSelector :: Selector
setPermissionStatus_forMatchPatternSelector = mkSelector "setPermissionStatus:forMatchPattern:"

-- | @Selector@ for @setPermissionStatus:forMatchPattern:expirationDate:@
setPermissionStatus_forMatchPattern_expirationDateSelector :: Selector
setPermissionStatus_forMatchPattern_expirationDateSelector = mkSelector "setPermissionStatus:forMatchPattern:expirationDate:"

-- | @Selector@ for @loadBackgroundContentWithCompletionHandler:@
loadBackgroundContentWithCompletionHandlerSelector :: Selector
loadBackgroundContentWithCompletionHandlerSelector = mkSelector "loadBackgroundContentWithCompletionHandler:"

-- | @Selector@ for @actionForTab:@
actionForTabSelector :: Selector
actionForTabSelector = mkSelector "actionForTab:"

-- | @Selector@ for @performActionForTab:@
performActionForTabSelector :: Selector
performActionForTabSelector = mkSelector "performActionForTab:"

-- | @Selector@ for @performCommand:@
performCommandSelector :: Selector
performCommandSelector = mkSelector "performCommand:"

-- | @Selector@ for @performCommandForEvent:@
performCommandForEventSelector :: Selector
performCommandForEventSelector = mkSelector "performCommandForEvent:"

-- | @Selector@ for @commandForEvent:@
commandForEventSelector :: Selector
commandForEventSelector = mkSelector "commandForEvent:"

-- | @Selector@ for @menuItemsForTab:@
menuItemsForTabSelector :: Selector
menuItemsForTabSelector = mkSelector "menuItemsForTab:"

-- | @Selector@ for @userGesturePerformedInTab:@
userGesturePerformedInTabSelector :: Selector
userGesturePerformedInTabSelector = mkSelector "userGesturePerformedInTab:"

-- | @Selector@ for @hasActiveUserGestureInTab:@
hasActiveUserGestureInTabSelector :: Selector
hasActiveUserGestureInTabSelector = mkSelector "hasActiveUserGestureInTab:"

-- | @Selector@ for @clearUserGestureInTab:@
clearUserGestureInTabSelector :: Selector
clearUserGestureInTabSelector = mkSelector "clearUserGestureInTab:"

-- | @Selector@ for @didOpenWindow:@
didOpenWindowSelector :: Selector
didOpenWindowSelector = mkSelector "didOpenWindow:"

-- | @Selector@ for @didCloseWindow:@
didCloseWindowSelector :: Selector
didCloseWindowSelector = mkSelector "didCloseWindow:"

-- | @Selector@ for @didFocusWindow:@
didFocusWindowSelector :: Selector
didFocusWindowSelector = mkSelector "didFocusWindow:"

-- | @Selector@ for @didOpenTab:@
didOpenTabSelector :: Selector
didOpenTabSelector = mkSelector "didOpenTab:"

-- | @Selector@ for @didCloseTab:windowIsClosing:@
didCloseTab_windowIsClosingSelector :: Selector
didCloseTab_windowIsClosingSelector = mkSelector "didCloseTab:windowIsClosing:"

-- | @Selector@ for @didActivateTab:previousActiveTab:@
didActivateTab_previousActiveTabSelector :: Selector
didActivateTab_previousActiveTabSelector = mkSelector "didActivateTab:previousActiveTab:"

-- | @Selector@ for @didSelectTabs:@
didSelectTabsSelector :: Selector
didSelectTabsSelector = mkSelector "didSelectTabs:"

-- | @Selector@ for @didDeselectTabs:@
didDeselectTabsSelector :: Selector
didDeselectTabsSelector = mkSelector "didDeselectTabs:"

-- | @Selector@ for @didMoveTab:fromIndex:inWindow:@
didMoveTab_fromIndex_inWindowSelector :: Selector
didMoveTab_fromIndex_inWindowSelector = mkSelector "didMoveTab:fromIndex:inWindow:"

-- | @Selector@ for @didReplaceTab:withTab:@
didReplaceTab_withTabSelector :: Selector
didReplaceTab_withTabSelector = mkSelector "didReplaceTab:withTab:"

-- | @Selector@ for @didChangeTabProperties:forTab:@
didChangeTabProperties_forTabSelector :: Selector
didChangeTabProperties_forTabSelector = mkSelector "didChangeTabProperties:forTab:"

-- | @Selector@ for @webExtension@
webExtensionSelector :: Selector
webExtensionSelector = mkSelector "webExtension"

-- | @Selector@ for @webExtensionController@
webExtensionControllerSelector :: Selector
webExtensionControllerSelector = mkSelector "webExtensionController"

-- | @Selector@ for @loaded@
loadedSelector :: Selector
loadedSelector = mkSelector "loaded"

-- | @Selector@ for @errors@
errorsSelector :: Selector
errorsSelector = mkSelector "errors"

-- | @Selector@ for @baseURL@
baseURLSelector :: Selector
baseURLSelector = mkSelector "baseURL"

-- | @Selector@ for @setBaseURL:@
setBaseURLSelector :: Selector
setBaseURLSelector = mkSelector "setBaseURL:"

-- | @Selector@ for @uniqueIdentifier@
uniqueIdentifierSelector :: Selector
uniqueIdentifierSelector = mkSelector "uniqueIdentifier"

-- | @Selector@ for @setUniqueIdentifier:@
setUniqueIdentifierSelector :: Selector
setUniqueIdentifierSelector = mkSelector "setUniqueIdentifier:"

-- | @Selector@ for @inspectable@
inspectableSelector :: Selector
inspectableSelector = mkSelector "inspectable"

-- | @Selector@ for @setInspectable:@
setInspectableSelector :: Selector
setInspectableSelector = mkSelector "setInspectable:"

-- | @Selector@ for @inspectionName@
inspectionNameSelector :: Selector
inspectionNameSelector = mkSelector "inspectionName"

-- | @Selector@ for @setInspectionName:@
setInspectionNameSelector :: Selector
setInspectionNameSelector = mkSelector "setInspectionName:"

-- | @Selector@ for @unsupportedAPIs@
unsupportedAPIsSelector :: Selector
unsupportedAPIsSelector = mkSelector "unsupportedAPIs"

-- | @Selector@ for @setUnsupportedAPIs:@
setUnsupportedAPIsSelector :: Selector
setUnsupportedAPIsSelector = mkSelector "setUnsupportedAPIs:"

-- | @Selector@ for @webViewConfiguration@
webViewConfigurationSelector :: Selector
webViewConfigurationSelector = mkSelector "webViewConfiguration"

-- | @Selector@ for @optionsPageURL@
optionsPageURLSelector :: Selector
optionsPageURLSelector = mkSelector "optionsPageURL"

-- | @Selector@ for @overrideNewTabPageURL@
overrideNewTabPageURLSelector :: Selector
overrideNewTabPageURLSelector = mkSelector "overrideNewTabPageURL"

-- | @Selector@ for @grantedPermissions@
grantedPermissionsSelector :: Selector
grantedPermissionsSelector = mkSelector "grantedPermissions"

-- | @Selector@ for @setGrantedPermissions:@
setGrantedPermissionsSelector :: Selector
setGrantedPermissionsSelector = mkSelector "setGrantedPermissions:"

-- | @Selector@ for @grantedPermissionMatchPatterns@
grantedPermissionMatchPatternsSelector :: Selector
grantedPermissionMatchPatternsSelector = mkSelector "grantedPermissionMatchPatterns"

-- | @Selector@ for @setGrantedPermissionMatchPatterns:@
setGrantedPermissionMatchPatternsSelector :: Selector
setGrantedPermissionMatchPatternsSelector = mkSelector "setGrantedPermissionMatchPatterns:"

-- | @Selector@ for @deniedPermissions@
deniedPermissionsSelector :: Selector
deniedPermissionsSelector = mkSelector "deniedPermissions"

-- | @Selector@ for @setDeniedPermissions:@
setDeniedPermissionsSelector :: Selector
setDeniedPermissionsSelector = mkSelector "setDeniedPermissions:"

-- | @Selector@ for @deniedPermissionMatchPatterns@
deniedPermissionMatchPatternsSelector :: Selector
deniedPermissionMatchPatternsSelector = mkSelector "deniedPermissionMatchPatterns"

-- | @Selector@ for @setDeniedPermissionMatchPatterns:@
setDeniedPermissionMatchPatternsSelector :: Selector
setDeniedPermissionMatchPatternsSelector = mkSelector "setDeniedPermissionMatchPatterns:"

-- | @Selector@ for @hasRequestedOptionalAccessToAllHosts@
hasRequestedOptionalAccessToAllHostsSelector :: Selector
hasRequestedOptionalAccessToAllHostsSelector = mkSelector "hasRequestedOptionalAccessToAllHosts"

-- | @Selector@ for @setHasRequestedOptionalAccessToAllHosts:@
setHasRequestedOptionalAccessToAllHostsSelector :: Selector
setHasRequestedOptionalAccessToAllHostsSelector = mkSelector "setHasRequestedOptionalAccessToAllHosts:"

-- | @Selector@ for @hasAccessToPrivateData@
hasAccessToPrivateDataSelector :: Selector
hasAccessToPrivateDataSelector = mkSelector "hasAccessToPrivateData"

-- | @Selector@ for @setHasAccessToPrivateData:@
setHasAccessToPrivateDataSelector :: Selector
setHasAccessToPrivateDataSelector = mkSelector "setHasAccessToPrivateData:"

-- | @Selector@ for @currentPermissions@
currentPermissionsSelector :: Selector
currentPermissionsSelector = mkSelector "currentPermissions"

-- | @Selector@ for @currentPermissionMatchPatterns@
currentPermissionMatchPatternsSelector :: Selector
currentPermissionMatchPatternsSelector = mkSelector "currentPermissionMatchPatterns"

-- | @Selector@ for @hasAccessToAllURLs@
hasAccessToAllURLsSelector :: Selector
hasAccessToAllURLsSelector = mkSelector "hasAccessToAllURLs"

-- | @Selector@ for @hasAccessToAllHosts@
hasAccessToAllHostsSelector :: Selector
hasAccessToAllHostsSelector = mkSelector "hasAccessToAllHosts"

-- | @Selector@ for @hasInjectedContent@
hasInjectedContentSelector :: Selector
hasInjectedContentSelector = mkSelector "hasInjectedContent"

-- | @Selector@ for @hasContentModificationRules@
hasContentModificationRulesSelector :: Selector
hasContentModificationRulesSelector = mkSelector "hasContentModificationRules"

-- | @Selector@ for @commands@
commandsSelector :: Selector
commandsSelector = mkSelector "commands"

