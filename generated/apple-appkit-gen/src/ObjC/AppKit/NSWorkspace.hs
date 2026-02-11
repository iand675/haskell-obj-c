{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Generated bindings for @NSWorkspace@.
module ObjC.AppKit.NSWorkspace
  ( NSWorkspace
  , IsNSWorkspace(..)
  , openURL
  , openURL_configuration_completionHandler
  , openURLs_withApplicationAtURL_configuration_completionHandler
  , openApplicationAtURL_configuration_completionHandler
  , selectFile_inFileViewerRootedAtPath
  , activateFileViewerSelectingURLs
  , showSearchResultsForQueryString
  , noteFileSystemChanged
  , isFilePackageAtPath
  , iconForFile
  , iconForFiles
  , iconForContentType
  , setIcon_forFile_options
  , getFileSystemInfoForPath_isRemovable_isWritable_isUnmountable_description_type
  , unmountAndEjectDeviceAtPath
  , unmountAndEjectDeviceAtURL_error
  , extendPowerOffBy
  , hideOtherApplications
  , urlForApplicationWithBundleIdentifier
  , urLsForApplicationsWithBundleIdentifier
  , urlForApplicationToOpenURL
  , urLsForApplicationsToOpenURL
  , setDefaultApplicationAtURL_toOpenContentTypeOfFileAtURL_completionHandler
  , setDefaultApplicationAtURL_toOpenURLsWithScheme_completionHandler
  , setDefaultApplicationAtURL_toOpenFileAtURL_completionHandler
  , urlForApplicationToOpenContentType
  , urLsForApplicationsToOpenContentType
  , setDefaultApplicationAtURL_toOpenContentType_completionHandler
  , openFile
  , openFile_withApplication
  , openFile_withApplication_andDeactivate
  , launchApplication
  , launchApplicationAtURL_options_configuration_error
  , openURL_options_configuration_error
  , openURLs_withApplicationAtURL_options_configuration_error
  , launchApplication_showIcon_autolaunch
  , fullPathForApplication
  , absolutePathForAppBundleWithIdentifier
  , launchAppWithBundleIdentifier_options_additionalEventParamDescriptor_launchIdentifier
  , openURLs_withAppBundleIdentifier_options_additionalEventParamDescriptor_launchIdentifiers
  , openTempFile
  , findApplications
  , noteUserDefaultsChanged
  , slideImage_from_to
  , checkForRemovableMedia
  , fileSystemChanged
  , userDefaultsChanged
  , mountNewRemovableMedia
  , activeApplication
  , mountedLocalVolumePaths
  , mountedRemovableMedia
  , launchedApplications
  , openFile_fromImage_at_inView
  , performFileOperation_source_destination_files_tag
  , getInfoForFile_application_type
  , iconForFileType
  , typeOfFile_error
  , localizedDescriptionForType
  , preferredFilenameExtensionForType
  , filenameExtension_isValidForType
  , type_conformsToType
  , requestAuthorizationOfType_completionHandler
  , setDesktopImageURL_forScreen_options_error
  , desktopImageURLForScreen
  , desktopImageOptionsForScreen
  , sharedWorkspace
  , notificationCenter
  , fileLabels
  , fileLabelColors
  , frontmostApplication
  , menuBarOwningApplication
  , runningApplications
  , voiceOverEnabled
  , switchControlEnabled
  , accessibilityDisplayShouldIncreaseContrast
  , accessibilityDisplayShouldDifferentiateWithoutColor
  , accessibilityDisplayShouldReduceTransparency
  , accessibilityDisplayShouldReduceMotion
  , accessibilityDisplayShouldInvertColors
  , openURLSelector
  , openURL_configuration_completionHandlerSelector
  , openURLs_withApplicationAtURL_configuration_completionHandlerSelector
  , openApplicationAtURL_configuration_completionHandlerSelector
  , selectFile_inFileViewerRootedAtPathSelector
  , activateFileViewerSelectingURLsSelector
  , showSearchResultsForQueryStringSelector
  , noteFileSystemChangedSelector
  , isFilePackageAtPathSelector
  , iconForFileSelector
  , iconForFilesSelector
  , iconForContentTypeSelector
  , setIcon_forFile_optionsSelector
  , getFileSystemInfoForPath_isRemovable_isWritable_isUnmountable_description_typeSelector
  , unmountAndEjectDeviceAtPathSelector
  , unmountAndEjectDeviceAtURL_errorSelector
  , extendPowerOffBySelector
  , hideOtherApplicationsSelector
  , urlForApplicationWithBundleIdentifierSelector
  , urLsForApplicationsWithBundleIdentifierSelector
  , urlForApplicationToOpenURLSelector
  , urLsForApplicationsToOpenURLSelector
  , setDefaultApplicationAtURL_toOpenContentTypeOfFileAtURL_completionHandlerSelector
  , setDefaultApplicationAtURL_toOpenURLsWithScheme_completionHandlerSelector
  , setDefaultApplicationAtURL_toOpenFileAtURL_completionHandlerSelector
  , urlForApplicationToOpenContentTypeSelector
  , urLsForApplicationsToOpenContentTypeSelector
  , setDefaultApplicationAtURL_toOpenContentType_completionHandlerSelector
  , openFileSelector
  , openFile_withApplicationSelector
  , openFile_withApplication_andDeactivateSelector
  , launchApplicationSelector
  , launchApplicationAtURL_options_configuration_errorSelector
  , openURL_options_configuration_errorSelector
  , openURLs_withApplicationAtURL_options_configuration_errorSelector
  , launchApplication_showIcon_autolaunchSelector
  , fullPathForApplicationSelector
  , absolutePathForAppBundleWithIdentifierSelector
  , launchAppWithBundleIdentifier_options_additionalEventParamDescriptor_launchIdentifierSelector
  , openURLs_withAppBundleIdentifier_options_additionalEventParamDescriptor_launchIdentifiersSelector
  , openTempFileSelector
  , findApplicationsSelector
  , noteUserDefaultsChangedSelector
  , slideImage_from_toSelector
  , checkForRemovableMediaSelector
  , fileSystemChangedSelector
  , userDefaultsChangedSelector
  , mountNewRemovableMediaSelector
  , activeApplicationSelector
  , mountedLocalVolumePathsSelector
  , mountedRemovableMediaSelector
  , launchedApplicationsSelector
  , openFile_fromImage_at_inViewSelector
  , performFileOperation_source_destination_files_tagSelector
  , getInfoForFile_application_typeSelector
  , iconForFileTypeSelector
  , typeOfFile_errorSelector
  , localizedDescriptionForTypeSelector
  , preferredFilenameExtensionForTypeSelector
  , filenameExtension_isValidForTypeSelector
  , type_conformsToTypeSelector
  , requestAuthorizationOfType_completionHandlerSelector
  , setDesktopImageURL_forScreen_options_errorSelector
  , desktopImageURLForScreenSelector
  , desktopImageOptionsForScreenSelector
  , sharedWorkspaceSelector
  , notificationCenterSelector
  , fileLabelsSelector
  , fileLabelColorsSelector
  , frontmostApplicationSelector
  , menuBarOwningApplicationSelector
  , runningApplicationsSelector
  , voiceOverEnabledSelector
  , switchControlEnabledSelector
  , accessibilityDisplayShouldIncreaseContrastSelector
  , accessibilityDisplayShouldDifferentiateWithoutColorSelector
  , accessibilityDisplayShouldReduceTransparencySelector
  , accessibilityDisplayShouldReduceMotionSelector
  , accessibilityDisplayShouldInvertColorsSelector

  -- * Enum types
  , NSWorkspaceAuthorizationType(NSWorkspaceAuthorizationType)
  , pattern NSWorkspaceAuthorizationTypeCreateSymbolicLink
  , pattern NSWorkspaceAuthorizationTypeSetAttributes
  , pattern NSWorkspaceAuthorizationTypeReplaceFile
  , NSWorkspaceIconCreationOptions(NSWorkspaceIconCreationOptions)
  , pattern NSExcludeQuickDrawElementsIconCreationOption
  , pattern NSExclude10_4ElementsIconCreationOption
  , NSWorkspaceLaunchOptions(NSWorkspaceLaunchOptions)
  , pattern NSWorkspaceLaunchAndPrint
  , pattern NSWorkspaceLaunchWithErrorPresentation
  , pattern NSWorkspaceLaunchInhibitingBackgroundOnly
  , pattern NSWorkspaceLaunchWithoutAddingToRecents
  , pattern NSWorkspaceLaunchWithoutActivation
  , pattern NSWorkspaceLaunchAsync
  , pattern NSWorkspaceLaunchNewInstance
  , pattern NSWorkspaceLaunchAndHide
  , pattern NSWorkspaceLaunchAndHideOthers
  , pattern NSWorkspaceLaunchDefault
  , pattern NSWorkspaceLaunchAllowingClassicStartup
  , pattern NSWorkspaceLaunchPreferringClassic

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

import ObjC.AppKit.Internal.Classes
import ObjC.Foundation.Internal.Structs
import ObjC.AppKit.Internal.Enums
import ObjC.Foundation.Internal.Classes
import ObjC.UniformTypeIdentifiers.Internal.Classes

-- | @- openURL:@
openURL :: (IsNSWorkspace nsWorkspace, IsNSURL url) => nsWorkspace -> url -> IO Bool
openURL nsWorkspace  url =
  withObjCPtr url $ \raw_url ->
      fmap ((/= 0) :: CULong -> Bool) $ sendMsg nsWorkspace (mkSelector "openURL:") retCULong [argPtr (castPtr raw_url :: Ptr ())]

-- | @- openURL:configuration:completionHandler:@
openURL_configuration_completionHandler :: (IsNSWorkspace nsWorkspace, IsNSURL url, IsNSWorkspaceOpenConfiguration configuration) => nsWorkspace -> url -> configuration -> Ptr () -> IO ()
openURL_configuration_completionHandler nsWorkspace  url configuration completionHandler =
  withObjCPtr url $ \raw_url ->
    withObjCPtr configuration $ \raw_configuration ->
        sendMsg nsWorkspace (mkSelector "openURL:configuration:completionHandler:") retVoid [argPtr (castPtr raw_url :: Ptr ()), argPtr (castPtr raw_configuration :: Ptr ()), argPtr (castPtr completionHandler :: Ptr ())]

-- | @- openURLs:withApplicationAtURL:configuration:completionHandler:@
openURLs_withApplicationAtURL_configuration_completionHandler :: (IsNSWorkspace nsWorkspace, IsNSArray urls, IsNSURL applicationURL, IsNSWorkspaceOpenConfiguration configuration) => nsWorkspace -> urls -> applicationURL -> configuration -> Ptr () -> IO ()
openURLs_withApplicationAtURL_configuration_completionHandler nsWorkspace  urls applicationURL configuration completionHandler =
  withObjCPtr urls $ \raw_urls ->
    withObjCPtr applicationURL $ \raw_applicationURL ->
      withObjCPtr configuration $ \raw_configuration ->
          sendMsg nsWorkspace (mkSelector "openURLs:withApplicationAtURL:configuration:completionHandler:") retVoid [argPtr (castPtr raw_urls :: Ptr ()), argPtr (castPtr raw_applicationURL :: Ptr ()), argPtr (castPtr raw_configuration :: Ptr ()), argPtr (castPtr completionHandler :: Ptr ())]

-- | @- openApplicationAtURL:configuration:completionHandler:@
openApplicationAtURL_configuration_completionHandler :: (IsNSWorkspace nsWorkspace, IsNSURL applicationURL, IsNSWorkspaceOpenConfiguration configuration) => nsWorkspace -> applicationURL -> configuration -> Ptr () -> IO ()
openApplicationAtURL_configuration_completionHandler nsWorkspace  applicationURL configuration completionHandler =
  withObjCPtr applicationURL $ \raw_applicationURL ->
    withObjCPtr configuration $ \raw_configuration ->
        sendMsg nsWorkspace (mkSelector "openApplicationAtURL:configuration:completionHandler:") retVoid [argPtr (castPtr raw_applicationURL :: Ptr ()), argPtr (castPtr raw_configuration :: Ptr ()), argPtr (castPtr completionHandler :: Ptr ())]

-- | @- selectFile:inFileViewerRootedAtPath:@
selectFile_inFileViewerRootedAtPath :: (IsNSWorkspace nsWorkspace, IsNSString fullPath, IsNSString rootFullPath) => nsWorkspace -> fullPath -> rootFullPath -> IO Bool
selectFile_inFileViewerRootedAtPath nsWorkspace  fullPath rootFullPath =
  withObjCPtr fullPath $ \raw_fullPath ->
    withObjCPtr rootFullPath $ \raw_rootFullPath ->
        fmap ((/= 0) :: CULong -> Bool) $ sendMsg nsWorkspace (mkSelector "selectFile:inFileViewerRootedAtPath:") retCULong [argPtr (castPtr raw_fullPath :: Ptr ()), argPtr (castPtr raw_rootFullPath :: Ptr ())]

-- | @- activateFileViewerSelectingURLs:@
activateFileViewerSelectingURLs :: (IsNSWorkspace nsWorkspace, IsNSArray fileURLs) => nsWorkspace -> fileURLs -> IO ()
activateFileViewerSelectingURLs nsWorkspace  fileURLs =
  withObjCPtr fileURLs $ \raw_fileURLs ->
      sendMsg nsWorkspace (mkSelector "activateFileViewerSelectingURLs:") retVoid [argPtr (castPtr raw_fileURLs :: Ptr ())]

-- | @- showSearchResultsForQueryString:@
showSearchResultsForQueryString :: (IsNSWorkspace nsWorkspace, IsNSString queryString) => nsWorkspace -> queryString -> IO Bool
showSearchResultsForQueryString nsWorkspace  queryString =
  withObjCPtr queryString $ \raw_queryString ->
      fmap ((/= 0) :: CULong -> Bool) $ sendMsg nsWorkspace (mkSelector "showSearchResultsForQueryString:") retCULong [argPtr (castPtr raw_queryString :: Ptr ())]

-- | @- noteFileSystemChanged:@
noteFileSystemChanged :: (IsNSWorkspace nsWorkspace, IsNSString path) => nsWorkspace -> path -> IO ()
noteFileSystemChanged nsWorkspace  path =
  withObjCPtr path $ \raw_path ->
      sendMsg nsWorkspace (mkSelector "noteFileSystemChanged:") retVoid [argPtr (castPtr raw_path :: Ptr ())]

-- | @- isFilePackageAtPath:@
isFilePackageAtPath :: (IsNSWorkspace nsWorkspace, IsNSString fullPath) => nsWorkspace -> fullPath -> IO Bool
isFilePackageAtPath nsWorkspace  fullPath =
  withObjCPtr fullPath $ \raw_fullPath ->
      fmap ((/= 0) :: CULong -> Bool) $ sendMsg nsWorkspace (mkSelector "isFilePackageAtPath:") retCULong [argPtr (castPtr raw_fullPath :: Ptr ())]

-- | @- iconForFile:@
iconForFile :: (IsNSWorkspace nsWorkspace, IsNSString fullPath) => nsWorkspace -> fullPath -> IO (Id NSImage)
iconForFile nsWorkspace  fullPath =
  withObjCPtr fullPath $ \raw_fullPath ->
      sendMsg nsWorkspace (mkSelector "iconForFile:") (retPtr retVoid) [argPtr (castPtr raw_fullPath :: Ptr ())] >>= retainedObject . castPtr

-- | @- iconForFiles:@
iconForFiles :: (IsNSWorkspace nsWorkspace, IsNSArray fullPaths) => nsWorkspace -> fullPaths -> IO (Id NSImage)
iconForFiles nsWorkspace  fullPaths =
  withObjCPtr fullPaths $ \raw_fullPaths ->
      sendMsg nsWorkspace (mkSelector "iconForFiles:") (retPtr retVoid) [argPtr (castPtr raw_fullPaths :: Ptr ())] >>= retainedObject . castPtr

-- | @- iconForContentType:@
iconForContentType :: (IsNSWorkspace nsWorkspace, IsUTType contentType) => nsWorkspace -> contentType -> IO (Id NSImage)
iconForContentType nsWorkspace  contentType =
  withObjCPtr contentType $ \raw_contentType ->
      sendMsg nsWorkspace (mkSelector "iconForContentType:") (retPtr retVoid) [argPtr (castPtr raw_contentType :: Ptr ())] >>= retainedObject . castPtr

-- | @- setIcon:forFile:options:@
setIcon_forFile_options :: (IsNSWorkspace nsWorkspace, IsNSImage image, IsNSString fullPath) => nsWorkspace -> image -> fullPath -> NSWorkspaceIconCreationOptions -> IO Bool
setIcon_forFile_options nsWorkspace  image fullPath options =
  withObjCPtr image $ \raw_image ->
    withObjCPtr fullPath $ \raw_fullPath ->
        fmap ((/= 0) :: CULong -> Bool) $ sendMsg nsWorkspace (mkSelector "setIcon:forFile:options:") retCULong [argPtr (castPtr raw_image :: Ptr ()), argPtr (castPtr raw_fullPath :: Ptr ()), argCULong (coerce options)]

-- | @- getFileSystemInfoForPath:isRemovable:isWritable:isUnmountable:description:type:@
getFileSystemInfoForPath_isRemovable_isWritable_isUnmountable_description_type :: (IsNSWorkspace nsWorkspace, IsNSString fullPath, IsNSString description, IsNSString fileSystemType) => nsWorkspace -> fullPath -> Ptr Bool -> Ptr Bool -> Ptr Bool -> description -> fileSystemType -> IO Bool
getFileSystemInfoForPath_isRemovable_isWritable_isUnmountable_description_type nsWorkspace  fullPath removableFlag writableFlag unmountableFlag description fileSystemType =
  withObjCPtr fullPath $ \raw_fullPath ->
    withObjCPtr description $ \raw_description ->
      withObjCPtr fileSystemType $ \raw_fileSystemType ->
          fmap ((/= 0) :: CULong -> Bool) $ sendMsg nsWorkspace (mkSelector "getFileSystemInfoForPath:isRemovable:isWritable:isUnmountable:description:type:") retCULong [argPtr (castPtr raw_fullPath :: Ptr ()), argPtr removableFlag, argPtr writableFlag, argPtr unmountableFlag, argPtr (castPtr raw_description :: Ptr ()), argPtr (castPtr raw_fileSystemType :: Ptr ())]

-- | @- unmountAndEjectDeviceAtPath:@
unmountAndEjectDeviceAtPath :: (IsNSWorkspace nsWorkspace, IsNSString path) => nsWorkspace -> path -> IO Bool
unmountAndEjectDeviceAtPath nsWorkspace  path =
  withObjCPtr path $ \raw_path ->
      fmap ((/= 0) :: CULong -> Bool) $ sendMsg nsWorkspace (mkSelector "unmountAndEjectDeviceAtPath:") retCULong [argPtr (castPtr raw_path :: Ptr ())]

-- | @- unmountAndEjectDeviceAtURL:error:@
unmountAndEjectDeviceAtURL_error :: (IsNSWorkspace nsWorkspace, IsNSURL url, IsNSError error_) => nsWorkspace -> url -> error_ -> IO Bool
unmountAndEjectDeviceAtURL_error nsWorkspace  url error_ =
  withObjCPtr url $ \raw_url ->
    withObjCPtr error_ $ \raw_error_ ->
        fmap ((/= 0) :: CULong -> Bool) $ sendMsg nsWorkspace (mkSelector "unmountAndEjectDeviceAtURL:error:") retCULong [argPtr (castPtr raw_url :: Ptr ()), argPtr (castPtr raw_error_ :: Ptr ())]

-- | @- extendPowerOffBy:@
extendPowerOffBy :: IsNSWorkspace nsWorkspace => nsWorkspace -> CLong -> IO CLong
extendPowerOffBy nsWorkspace  requested =
    sendMsg nsWorkspace (mkSelector "extendPowerOffBy:") retCLong [argCLong requested]

-- | @- hideOtherApplications@
hideOtherApplications :: IsNSWorkspace nsWorkspace => nsWorkspace -> IO ()
hideOtherApplications nsWorkspace  =
    sendMsg nsWorkspace (mkSelector "hideOtherApplications") retVoid []

-- | @- URLForApplicationWithBundleIdentifier:@
urlForApplicationWithBundleIdentifier :: (IsNSWorkspace nsWorkspace, IsNSString bundleIdentifier) => nsWorkspace -> bundleIdentifier -> IO (Id NSURL)
urlForApplicationWithBundleIdentifier nsWorkspace  bundleIdentifier =
  withObjCPtr bundleIdentifier $ \raw_bundleIdentifier ->
      sendMsg nsWorkspace (mkSelector "URLForApplicationWithBundleIdentifier:") (retPtr retVoid) [argPtr (castPtr raw_bundleIdentifier :: Ptr ())] >>= retainedObject . castPtr

-- | @- URLsForApplicationsWithBundleIdentifier:@
urLsForApplicationsWithBundleIdentifier :: (IsNSWorkspace nsWorkspace, IsNSString bundleIdentifier) => nsWorkspace -> bundleIdentifier -> IO (Id NSArray)
urLsForApplicationsWithBundleIdentifier nsWorkspace  bundleIdentifier =
  withObjCPtr bundleIdentifier $ \raw_bundleIdentifier ->
      sendMsg nsWorkspace (mkSelector "URLsForApplicationsWithBundleIdentifier:") (retPtr retVoid) [argPtr (castPtr raw_bundleIdentifier :: Ptr ())] >>= retainedObject . castPtr

-- | @- URLForApplicationToOpenURL:@
urlForApplicationToOpenURL :: (IsNSWorkspace nsWorkspace, IsNSURL url) => nsWorkspace -> url -> IO (Id NSURL)
urlForApplicationToOpenURL nsWorkspace  url =
  withObjCPtr url $ \raw_url ->
      sendMsg nsWorkspace (mkSelector "URLForApplicationToOpenURL:") (retPtr retVoid) [argPtr (castPtr raw_url :: Ptr ())] >>= retainedObject . castPtr

-- | @- URLsForApplicationsToOpenURL:@
urLsForApplicationsToOpenURL :: (IsNSWorkspace nsWorkspace, IsNSURL url) => nsWorkspace -> url -> IO (Id NSArray)
urLsForApplicationsToOpenURL nsWorkspace  url =
  withObjCPtr url $ \raw_url ->
      sendMsg nsWorkspace (mkSelector "URLsForApplicationsToOpenURL:") (retPtr retVoid) [argPtr (castPtr raw_url :: Ptr ())] >>= retainedObject . castPtr

-- | @- setDefaultApplicationAtURL:toOpenContentTypeOfFileAtURL:completionHandler:@
setDefaultApplicationAtURL_toOpenContentTypeOfFileAtURL_completionHandler :: (IsNSWorkspace nsWorkspace, IsNSURL applicationURL, IsNSURL url) => nsWorkspace -> applicationURL -> url -> Ptr () -> IO ()
setDefaultApplicationAtURL_toOpenContentTypeOfFileAtURL_completionHandler nsWorkspace  applicationURL url completionHandler =
  withObjCPtr applicationURL $ \raw_applicationURL ->
    withObjCPtr url $ \raw_url ->
        sendMsg nsWorkspace (mkSelector "setDefaultApplicationAtURL:toOpenContentTypeOfFileAtURL:completionHandler:") retVoid [argPtr (castPtr raw_applicationURL :: Ptr ()), argPtr (castPtr raw_url :: Ptr ()), argPtr (castPtr completionHandler :: Ptr ())]

-- | @- setDefaultApplicationAtURL:toOpenURLsWithScheme:completionHandler:@
setDefaultApplicationAtURL_toOpenURLsWithScheme_completionHandler :: (IsNSWorkspace nsWorkspace, IsNSURL applicationURL, IsNSString urlScheme) => nsWorkspace -> applicationURL -> urlScheme -> Ptr () -> IO ()
setDefaultApplicationAtURL_toOpenURLsWithScheme_completionHandler nsWorkspace  applicationURL urlScheme completionHandler =
  withObjCPtr applicationURL $ \raw_applicationURL ->
    withObjCPtr urlScheme $ \raw_urlScheme ->
        sendMsg nsWorkspace (mkSelector "setDefaultApplicationAtURL:toOpenURLsWithScheme:completionHandler:") retVoid [argPtr (castPtr raw_applicationURL :: Ptr ()), argPtr (castPtr raw_urlScheme :: Ptr ()), argPtr (castPtr completionHandler :: Ptr ())]

-- | @- setDefaultApplicationAtURL:toOpenFileAtURL:completionHandler:@
setDefaultApplicationAtURL_toOpenFileAtURL_completionHandler :: (IsNSWorkspace nsWorkspace, IsNSURL applicationURL, IsNSURL url) => nsWorkspace -> applicationURL -> url -> Ptr () -> IO ()
setDefaultApplicationAtURL_toOpenFileAtURL_completionHandler nsWorkspace  applicationURL url completionHandler =
  withObjCPtr applicationURL $ \raw_applicationURL ->
    withObjCPtr url $ \raw_url ->
        sendMsg nsWorkspace (mkSelector "setDefaultApplicationAtURL:toOpenFileAtURL:completionHandler:") retVoid [argPtr (castPtr raw_applicationURL :: Ptr ()), argPtr (castPtr raw_url :: Ptr ()), argPtr (castPtr completionHandler :: Ptr ())]

-- | @- URLForApplicationToOpenContentType:@
urlForApplicationToOpenContentType :: (IsNSWorkspace nsWorkspace, IsUTType contentType) => nsWorkspace -> contentType -> IO (Id NSURL)
urlForApplicationToOpenContentType nsWorkspace  contentType =
  withObjCPtr contentType $ \raw_contentType ->
      sendMsg nsWorkspace (mkSelector "URLForApplicationToOpenContentType:") (retPtr retVoid) [argPtr (castPtr raw_contentType :: Ptr ())] >>= retainedObject . castPtr

-- | @- URLsForApplicationsToOpenContentType:@
urLsForApplicationsToOpenContentType :: (IsNSWorkspace nsWorkspace, IsUTType contentType) => nsWorkspace -> contentType -> IO (Id NSArray)
urLsForApplicationsToOpenContentType nsWorkspace  contentType =
  withObjCPtr contentType $ \raw_contentType ->
      sendMsg nsWorkspace (mkSelector "URLsForApplicationsToOpenContentType:") (retPtr retVoid) [argPtr (castPtr raw_contentType :: Ptr ())] >>= retainedObject . castPtr

-- | @- setDefaultApplicationAtURL:toOpenContentType:completionHandler:@
setDefaultApplicationAtURL_toOpenContentType_completionHandler :: (IsNSWorkspace nsWorkspace, IsNSURL applicationURL, IsUTType contentType) => nsWorkspace -> applicationURL -> contentType -> Ptr () -> IO ()
setDefaultApplicationAtURL_toOpenContentType_completionHandler nsWorkspace  applicationURL contentType completionHandler =
  withObjCPtr applicationURL $ \raw_applicationURL ->
    withObjCPtr contentType $ \raw_contentType ->
        sendMsg nsWorkspace (mkSelector "setDefaultApplicationAtURL:toOpenContentType:completionHandler:") retVoid [argPtr (castPtr raw_applicationURL :: Ptr ()), argPtr (castPtr raw_contentType :: Ptr ()), argPtr (castPtr completionHandler :: Ptr ())]

-- | @- openFile:@
openFile :: (IsNSWorkspace nsWorkspace, IsNSString fullPath) => nsWorkspace -> fullPath -> IO Bool
openFile nsWorkspace  fullPath =
  withObjCPtr fullPath $ \raw_fullPath ->
      fmap ((/= 0) :: CULong -> Bool) $ sendMsg nsWorkspace (mkSelector "openFile:") retCULong [argPtr (castPtr raw_fullPath :: Ptr ())]

-- | @- openFile:withApplication:@
openFile_withApplication :: (IsNSWorkspace nsWorkspace, IsNSString fullPath, IsNSString appName) => nsWorkspace -> fullPath -> appName -> IO Bool
openFile_withApplication nsWorkspace  fullPath appName =
  withObjCPtr fullPath $ \raw_fullPath ->
    withObjCPtr appName $ \raw_appName ->
        fmap ((/= 0) :: CULong -> Bool) $ sendMsg nsWorkspace (mkSelector "openFile:withApplication:") retCULong [argPtr (castPtr raw_fullPath :: Ptr ()), argPtr (castPtr raw_appName :: Ptr ())]

-- | @- openFile:withApplication:andDeactivate:@
openFile_withApplication_andDeactivate :: (IsNSWorkspace nsWorkspace, IsNSString fullPath, IsNSString appName) => nsWorkspace -> fullPath -> appName -> Bool -> IO Bool
openFile_withApplication_andDeactivate nsWorkspace  fullPath appName flag =
  withObjCPtr fullPath $ \raw_fullPath ->
    withObjCPtr appName $ \raw_appName ->
        fmap ((/= 0) :: CULong -> Bool) $ sendMsg nsWorkspace (mkSelector "openFile:withApplication:andDeactivate:") retCULong [argPtr (castPtr raw_fullPath :: Ptr ()), argPtr (castPtr raw_appName :: Ptr ()), argCULong (if flag then 1 else 0)]

-- | @- launchApplication:@
launchApplication :: (IsNSWorkspace nsWorkspace, IsNSString appName) => nsWorkspace -> appName -> IO Bool
launchApplication nsWorkspace  appName =
  withObjCPtr appName $ \raw_appName ->
      fmap ((/= 0) :: CULong -> Bool) $ sendMsg nsWorkspace (mkSelector "launchApplication:") retCULong [argPtr (castPtr raw_appName :: Ptr ())]

-- | @- launchApplicationAtURL:options:configuration:error:@
launchApplicationAtURL_options_configuration_error :: (IsNSWorkspace nsWorkspace, IsNSURL url, IsNSDictionary configuration, IsNSError error_) => nsWorkspace -> url -> NSWorkspaceLaunchOptions -> configuration -> error_ -> IO (Id NSRunningApplication)
launchApplicationAtURL_options_configuration_error nsWorkspace  url options configuration error_ =
  withObjCPtr url $ \raw_url ->
    withObjCPtr configuration $ \raw_configuration ->
      withObjCPtr error_ $ \raw_error_ ->
          sendMsg nsWorkspace (mkSelector "launchApplicationAtURL:options:configuration:error:") (retPtr retVoid) [argPtr (castPtr raw_url :: Ptr ()), argCULong (coerce options), argPtr (castPtr raw_configuration :: Ptr ()), argPtr (castPtr raw_error_ :: Ptr ())] >>= retainedObject . castPtr

-- | @- openURL:options:configuration:error:@
openURL_options_configuration_error :: (IsNSWorkspace nsWorkspace, IsNSURL url, IsNSDictionary configuration, IsNSError error_) => nsWorkspace -> url -> NSWorkspaceLaunchOptions -> configuration -> error_ -> IO (Id NSRunningApplication)
openURL_options_configuration_error nsWorkspace  url options configuration error_ =
  withObjCPtr url $ \raw_url ->
    withObjCPtr configuration $ \raw_configuration ->
      withObjCPtr error_ $ \raw_error_ ->
          sendMsg nsWorkspace (mkSelector "openURL:options:configuration:error:") (retPtr retVoid) [argPtr (castPtr raw_url :: Ptr ()), argCULong (coerce options), argPtr (castPtr raw_configuration :: Ptr ()), argPtr (castPtr raw_error_ :: Ptr ())] >>= retainedObject . castPtr

-- | @- openURLs:withApplicationAtURL:options:configuration:error:@
openURLs_withApplicationAtURL_options_configuration_error :: (IsNSWorkspace nsWorkspace, IsNSArray urls, IsNSURL applicationURL, IsNSDictionary configuration, IsNSError error_) => nsWorkspace -> urls -> applicationURL -> NSWorkspaceLaunchOptions -> configuration -> error_ -> IO (Id NSRunningApplication)
openURLs_withApplicationAtURL_options_configuration_error nsWorkspace  urls applicationURL options configuration error_ =
  withObjCPtr urls $ \raw_urls ->
    withObjCPtr applicationURL $ \raw_applicationURL ->
      withObjCPtr configuration $ \raw_configuration ->
        withObjCPtr error_ $ \raw_error_ ->
            sendMsg nsWorkspace (mkSelector "openURLs:withApplicationAtURL:options:configuration:error:") (retPtr retVoid) [argPtr (castPtr raw_urls :: Ptr ()), argPtr (castPtr raw_applicationURL :: Ptr ()), argCULong (coerce options), argPtr (castPtr raw_configuration :: Ptr ()), argPtr (castPtr raw_error_ :: Ptr ())] >>= retainedObject . castPtr

-- | @- launchApplication:showIcon:autolaunch:@
launchApplication_showIcon_autolaunch :: (IsNSWorkspace nsWorkspace, IsNSString appName) => nsWorkspace -> appName -> Bool -> Bool -> IO Bool
launchApplication_showIcon_autolaunch nsWorkspace  appName showIcon autolaunch =
  withObjCPtr appName $ \raw_appName ->
      fmap ((/= 0) :: CULong -> Bool) $ sendMsg nsWorkspace (mkSelector "launchApplication:showIcon:autolaunch:") retCULong [argPtr (castPtr raw_appName :: Ptr ()), argCULong (if showIcon then 1 else 0), argCULong (if autolaunch then 1 else 0)]

-- | @- fullPathForApplication:@
fullPathForApplication :: (IsNSWorkspace nsWorkspace, IsNSString appName) => nsWorkspace -> appName -> IO (Id NSString)
fullPathForApplication nsWorkspace  appName =
  withObjCPtr appName $ \raw_appName ->
      sendMsg nsWorkspace (mkSelector "fullPathForApplication:") (retPtr retVoid) [argPtr (castPtr raw_appName :: Ptr ())] >>= retainedObject . castPtr

-- | @- absolutePathForAppBundleWithIdentifier:@
absolutePathForAppBundleWithIdentifier :: (IsNSWorkspace nsWorkspace, IsNSString bundleIdentifier) => nsWorkspace -> bundleIdentifier -> IO (Id NSString)
absolutePathForAppBundleWithIdentifier nsWorkspace  bundleIdentifier =
  withObjCPtr bundleIdentifier $ \raw_bundleIdentifier ->
      sendMsg nsWorkspace (mkSelector "absolutePathForAppBundleWithIdentifier:") (retPtr retVoid) [argPtr (castPtr raw_bundleIdentifier :: Ptr ())] >>= retainedObject . castPtr

-- | @- launchAppWithBundleIdentifier:options:additionalEventParamDescriptor:launchIdentifier:@
launchAppWithBundleIdentifier_options_additionalEventParamDescriptor_launchIdentifier :: (IsNSWorkspace nsWorkspace, IsNSString bundleIdentifier, IsNSAppleEventDescriptor descriptor, IsNSNumber identifier) => nsWorkspace -> bundleIdentifier -> NSWorkspaceLaunchOptions -> descriptor -> identifier -> IO Bool
launchAppWithBundleIdentifier_options_additionalEventParamDescriptor_launchIdentifier nsWorkspace  bundleIdentifier options descriptor identifier =
  withObjCPtr bundleIdentifier $ \raw_bundleIdentifier ->
    withObjCPtr descriptor $ \raw_descriptor ->
      withObjCPtr identifier $ \raw_identifier ->
          fmap ((/= 0) :: CULong -> Bool) $ sendMsg nsWorkspace (mkSelector "launchAppWithBundleIdentifier:options:additionalEventParamDescriptor:launchIdentifier:") retCULong [argPtr (castPtr raw_bundleIdentifier :: Ptr ()), argCULong (coerce options), argPtr (castPtr raw_descriptor :: Ptr ()), argPtr (castPtr raw_identifier :: Ptr ())]

-- | @- openURLs:withAppBundleIdentifier:options:additionalEventParamDescriptor:launchIdentifiers:@
openURLs_withAppBundleIdentifier_options_additionalEventParamDescriptor_launchIdentifiers :: (IsNSWorkspace nsWorkspace, IsNSArray urls, IsNSString bundleIdentifier, IsNSAppleEventDescriptor descriptor, IsNSArray identifiers) => nsWorkspace -> urls -> bundleIdentifier -> NSWorkspaceLaunchOptions -> descriptor -> identifiers -> IO Bool
openURLs_withAppBundleIdentifier_options_additionalEventParamDescriptor_launchIdentifiers nsWorkspace  urls bundleIdentifier options descriptor identifiers =
  withObjCPtr urls $ \raw_urls ->
    withObjCPtr bundleIdentifier $ \raw_bundleIdentifier ->
      withObjCPtr descriptor $ \raw_descriptor ->
        withObjCPtr identifiers $ \raw_identifiers ->
            fmap ((/= 0) :: CULong -> Bool) $ sendMsg nsWorkspace (mkSelector "openURLs:withAppBundleIdentifier:options:additionalEventParamDescriptor:launchIdentifiers:") retCULong [argPtr (castPtr raw_urls :: Ptr ()), argPtr (castPtr raw_bundleIdentifier :: Ptr ()), argCULong (coerce options), argPtr (castPtr raw_descriptor :: Ptr ()), argPtr (castPtr raw_identifiers :: Ptr ())]

-- | @- openTempFile:@
openTempFile :: (IsNSWorkspace nsWorkspace, IsNSString fullPath) => nsWorkspace -> fullPath -> IO Bool
openTempFile nsWorkspace  fullPath =
  withObjCPtr fullPath $ \raw_fullPath ->
      fmap ((/= 0) :: CULong -> Bool) $ sendMsg nsWorkspace (mkSelector "openTempFile:") retCULong [argPtr (castPtr raw_fullPath :: Ptr ())]

-- | @- findApplications@
findApplications :: IsNSWorkspace nsWorkspace => nsWorkspace -> IO ()
findApplications nsWorkspace  =
    sendMsg nsWorkspace (mkSelector "findApplications") retVoid []

-- | @- noteUserDefaultsChanged@
noteUserDefaultsChanged :: IsNSWorkspace nsWorkspace => nsWorkspace -> IO ()
noteUserDefaultsChanged nsWorkspace  =
    sendMsg nsWorkspace (mkSelector "noteUserDefaultsChanged") retVoid []

-- | @- slideImage:from:to:@
slideImage_from_to :: (IsNSWorkspace nsWorkspace, IsNSImage image) => nsWorkspace -> image -> NSPoint -> NSPoint -> IO ()
slideImage_from_to nsWorkspace  image fromPoint toPoint =
  withObjCPtr image $ \raw_image ->
      sendMsg nsWorkspace (mkSelector "slideImage:from:to:") retVoid [argPtr (castPtr raw_image :: Ptr ()), argNSPoint fromPoint, argNSPoint toPoint]

-- | @- checkForRemovableMedia@
checkForRemovableMedia :: IsNSWorkspace nsWorkspace => nsWorkspace -> IO ()
checkForRemovableMedia nsWorkspace  =
    sendMsg nsWorkspace (mkSelector "checkForRemovableMedia") retVoid []

-- | @- fileSystemChanged@
fileSystemChanged :: IsNSWorkspace nsWorkspace => nsWorkspace -> IO Bool
fileSystemChanged nsWorkspace  =
    fmap ((/= 0) :: CULong -> Bool) $ sendMsg nsWorkspace (mkSelector "fileSystemChanged") retCULong []

-- | @- userDefaultsChanged@
userDefaultsChanged :: IsNSWorkspace nsWorkspace => nsWorkspace -> IO Bool
userDefaultsChanged nsWorkspace  =
    fmap ((/= 0) :: CULong -> Bool) $ sendMsg nsWorkspace (mkSelector "userDefaultsChanged") retCULong []

-- | @- mountNewRemovableMedia@
mountNewRemovableMedia :: IsNSWorkspace nsWorkspace => nsWorkspace -> IO (Id NSArray)
mountNewRemovableMedia nsWorkspace  =
    sendMsg nsWorkspace (mkSelector "mountNewRemovableMedia") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- activeApplication@
activeApplication :: IsNSWorkspace nsWorkspace => nsWorkspace -> IO (Id NSDictionary)
activeApplication nsWorkspace  =
    sendMsg nsWorkspace (mkSelector "activeApplication") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- mountedLocalVolumePaths@
mountedLocalVolumePaths :: IsNSWorkspace nsWorkspace => nsWorkspace -> IO (Id NSArray)
mountedLocalVolumePaths nsWorkspace  =
    sendMsg nsWorkspace (mkSelector "mountedLocalVolumePaths") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- mountedRemovableMedia@
mountedRemovableMedia :: IsNSWorkspace nsWorkspace => nsWorkspace -> IO (Id NSArray)
mountedRemovableMedia nsWorkspace  =
    sendMsg nsWorkspace (mkSelector "mountedRemovableMedia") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- launchedApplications@
launchedApplications :: IsNSWorkspace nsWorkspace => nsWorkspace -> IO (Id NSArray)
launchedApplications nsWorkspace  =
    sendMsg nsWorkspace (mkSelector "launchedApplications") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- openFile:fromImage:at:inView:@
openFile_fromImage_at_inView :: (IsNSWorkspace nsWorkspace, IsNSString fullPath, IsNSImage image, IsNSView view) => nsWorkspace -> fullPath -> image -> NSPoint -> view -> IO Bool
openFile_fromImage_at_inView nsWorkspace  fullPath image point view =
  withObjCPtr fullPath $ \raw_fullPath ->
    withObjCPtr image $ \raw_image ->
      withObjCPtr view $ \raw_view ->
          fmap ((/= 0) :: CULong -> Bool) $ sendMsg nsWorkspace (mkSelector "openFile:fromImage:at:inView:") retCULong [argPtr (castPtr raw_fullPath :: Ptr ()), argPtr (castPtr raw_image :: Ptr ()), argNSPoint point, argPtr (castPtr raw_view :: Ptr ())]

-- | @- performFileOperation:source:destination:files:tag:@
performFileOperation_source_destination_files_tag :: (IsNSWorkspace nsWorkspace, IsNSString operation, IsNSString source, IsNSString destination, IsNSArray files) => nsWorkspace -> operation -> source -> destination -> files -> Ptr CLong -> IO Bool
performFileOperation_source_destination_files_tag nsWorkspace  operation source destination files tag =
  withObjCPtr operation $ \raw_operation ->
    withObjCPtr source $ \raw_source ->
      withObjCPtr destination $ \raw_destination ->
        withObjCPtr files $ \raw_files ->
            fmap ((/= 0) :: CULong -> Bool) $ sendMsg nsWorkspace (mkSelector "performFileOperation:source:destination:files:tag:") retCULong [argPtr (castPtr raw_operation :: Ptr ()), argPtr (castPtr raw_source :: Ptr ()), argPtr (castPtr raw_destination :: Ptr ()), argPtr (castPtr raw_files :: Ptr ()), argPtr tag]

-- | @- getInfoForFile:application:type:@
getInfoForFile_application_type :: (IsNSWorkspace nsWorkspace, IsNSString fullPath, IsNSString appName, IsNSString type_) => nsWorkspace -> fullPath -> appName -> type_ -> IO Bool
getInfoForFile_application_type nsWorkspace  fullPath appName type_ =
  withObjCPtr fullPath $ \raw_fullPath ->
    withObjCPtr appName $ \raw_appName ->
      withObjCPtr type_ $ \raw_type_ ->
          fmap ((/= 0) :: CULong -> Bool) $ sendMsg nsWorkspace (mkSelector "getInfoForFile:application:type:") retCULong [argPtr (castPtr raw_fullPath :: Ptr ()), argPtr (castPtr raw_appName :: Ptr ()), argPtr (castPtr raw_type_ :: Ptr ())]

-- | @- iconForFileType:@
iconForFileType :: (IsNSWorkspace nsWorkspace, IsNSString fileType) => nsWorkspace -> fileType -> IO (Id NSImage)
iconForFileType nsWorkspace  fileType =
  withObjCPtr fileType $ \raw_fileType ->
      sendMsg nsWorkspace (mkSelector "iconForFileType:") (retPtr retVoid) [argPtr (castPtr raw_fileType :: Ptr ())] >>= retainedObject . castPtr

-- | @- typeOfFile:error:@
typeOfFile_error :: (IsNSWorkspace nsWorkspace, IsNSString absoluteFilePath, IsNSError outError) => nsWorkspace -> absoluteFilePath -> outError -> IO (Id NSString)
typeOfFile_error nsWorkspace  absoluteFilePath outError =
  withObjCPtr absoluteFilePath $ \raw_absoluteFilePath ->
    withObjCPtr outError $ \raw_outError ->
        sendMsg nsWorkspace (mkSelector "typeOfFile:error:") (retPtr retVoid) [argPtr (castPtr raw_absoluteFilePath :: Ptr ()), argPtr (castPtr raw_outError :: Ptr ())] >>= retainedObject . castPtr

-- | @- localizedDescriptionForType:@
localizedDescriptionForType :: (IsNSWorkspace nsWorkspace, IsNSString typeName) => nsWorkspace -> typeName -> IO (Id NSString)
localizedDescriptionForType nsWorkspace  typeName =
  withObjCPtr typeName $ \raw_typeName ->
      sendMsg nsWorkspace (mkSelector "localizedDescriptionForType:") (retPtr retVoid) [argPtr (castPtr raw_typeName :: Ptr ())] >>= retainedObject . castPtr

-- | @- preferredFilenameExtensionForType:@
preferredFilenameExtensionForType :: (IsNSWorkspace nsWorkspace, IsNSString typeName) => nsWorkspace -> typeName -> IO (Id NSString)
preferredFilenameExtensionForType nsWorkspace  typeName =
  withObjCPtr typeName $ \raw_typeName ->
      sendMsg nsWorkspace (mkSelector "preferredFilenameExtensionForType:") (retPtr retVoid) [argPtr (castPtr raw_typeName :: Ptr ())] >>= retainedObject . castPtr

-- | @- filenameExtension:isValidForType:@
filenameExtension_isValidForType :: (IsNSWorkspace nsWorkspace, IsNSString filenameExtension, IsNSString typeName) => nsWorkspace -> filenameExtension -> typeName -> IO Bool
filenameExtension_isValidForType nsWorkspace  filenameExtension typeName =
  withObjCPtr filenameExtension $ \raw_filenameExtension ->
    withObjCPtr typeName $ \raw_typeName ->
        fmap ((/= 0) :: CULong -> Bool) $ sendMsg nsWorkspace (mkSelector "filenameExtension:isValidForType:") retCULong [argPtr (castPtr raw_filenameExtension :: Ptr ()), argPtr (castPtr raw_typeName :: Ptr ())]

-- | @- type:conformsToType:@
type_conformsToType :: (IsNSWorkspace nsWorkspace, IsNSString firstTypeName, IsNSString secondTypeName) => nsWorkspace -> firstTypeName -> secondTypeName -> IO Bool
type_conformsToType nsWorkspace  firstTypeName secondTypeName =
  withObjCPtr firstTypeName $ \raw_firstTypeName ->
    withObjCPtr secondTypeName $ \raw_secondTypeName ->
        fmap ((/= 0) :: CULong -> Bool) $ sendMsg nsWorkspace (mkSelector "type:conformsToType:") retCULong [argPtr (castPtr raw_firstTypeName :: Ptr ()), argPtr (castPtr raw_secondTypeName :: Ptr ())]

-- | @- requestAuthorizationOfType:completionHandler:@
requestAuthorizationOfType_completionHandler :: IsNSWorkspace nsWorkspace => nsWorkspace -> NSWorkspaceAuthorizationType -> Ptr () -> IO ()
requestAuthorizationOfType_completionHandler nsWorkspace  type_ completionHandler =
    sendMsg nsWorkspace (mkSelector "requestAuthorizationOfType:completionHandler:") retVoid [argCLong (coerce type_), argPtr (castPtr completionHandler :: Ptr ())]

-- | @- setDesktopImageURL:forScreen:options:error:@
setDesktopImageURL_forScreen_options_error :: (IsNSWorkspace nsWorkspace, IsNSURL url, IsNSScreen screen, IsNSDictionary options, IsNSError error_) => nsWorkspace -> url -> screen -> options -> error_ -> IO Bool
setDesktopImageURL_forScreen_options_error nsWorkspace  url screen options error_ =
  withObjCPtr url $ \raw_url ->
    withObjCPtr screen $ \raw_screen ->
      withObjCPtr options $ \raw_options ->
        withObjCPtr error_ $ \raw_error_ ->
            fmap ((/= 0) :: CULong -> Bool) $ sendMsg nsWorkspace (mkSelector "setDesktopImageURL:forScreen:options:error:") retCULong [argPtr (castPtr raw_url :: Ptr ()), argPtr (castPtr raw_screen :: Ptr ()), argPtr (castPtr raw_options :: Ptr ()), argPtr (castPtr raw_error_ :: Ptr ())]

-- | @- desktopImageURLForScreen:@
desktopImageURLForScreen :: (IsNSWorkspace nsWorkspace, IsNSScreen screen) => nsWorkspace -> screen -> IO (Id NSURL)
desktopImageURLForScreen nsWorkspace  screen =
  withObjCPtr screen $ \raw_screen ->
      sendMsg nsWorkspace (mkSelector "desktopImageURLForScreen:") (retPtr retVoid) [argPtr (castPtr raw_screen :: Ptr ())] >>= retainedObject . castPtr

-- | @- desktopImageOptionsForScreen:@
desktopImageOptionsForScreen :: (IsNSWorkspace nsWorkspace, IsNSScreen screen) => nsWorkspace -> screen -> IO (Id NSDictionary)
desktopImageOptionsForScreen nsWorkspace  screen =
  withObjCPtr screen $ \raw_screen ->
      sendMsg nsWorkspace (mkSelector "desktopImageOptionsForScreen:") (retPtr retVoid) [argPtr (castPtr raw_screen :: Ptr ())] >>= retainedObject . castPtr

-- | @+ sharedWorkspace@
sharedWorkspace :: IO (Id NSWorkspace)
sharedWorkspace  =
  do
    cls' <- getRequiredClass "NSWorkspace"
    sendClassMsg cls' (mkSelector "sharedWorkspace") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- notificationCenter@
notificationCenter :: IsNSWorkspace nsWorkspace => nsWorkspace -> IO (Id NSNotificationCenter)
notificationCenter nsWorkspace  =
    sendMsg nsWorkspace (mkSelector "notificationCenter") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- fileLabels@
fileLabels :: IsNSWorkspace nsWorkspace => nsWorkspace -> IO (Id NSArray)
fileLabels nsWorkspace  =
    sendMsg nsWorkspace (mkSelector "fileLabels") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- fileLabelColors@
fileLabelColors :: IsNSWorkspace nsWorkspace => nsWorkspace -> IO (Id NSArray)
fileLabelColors nsWorkspace  =
    sendMsg nsWorkspace (mkSelector "fileLabelColors") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- frontmostApplication@
frontmostApplication :: IsNSWorkspace nsWorkspace => nsWorkspace -> IO (Id NSRunningApplication)
frontmostApplication nsWorkspace  =
    sendMsg nsWorkspace (mkSelector "frontmostApplication") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- menuBarOwningApplication@
menuBarOwningApplication :: IsNSWorkspace nsWorkspace => nsWorkspace -> IO (Id NSRunningApplication)
menuBarOwningApplication nsWorkspace  =
    sendMsg nsWorkspace (mkSelector "menuBarOwningApplication") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | Returns: An array of @NSRunningApplication@s representing currently running applications. The order of the array is unspecified, but it is stable, meaning that the relative order of particular applications will not change across multiple calls to @runningApplications@. Similar to @NSRunningApplication@'s properties, this property will only change when the main run loop is run in a common mode.  Instead of polling, use key-value observing to be notified of changes to this array property.This property is thread safe, in that it may be called from background threads and the result is returned atomically.  This property is observable through KVO.
--
-- ObjC selector: @- runningApplications@
runningApplications :: IsNSWorkspace nsWorkspace => nsWorkspace -> IO (Id NSArray)
runningApplications nsWorkspace  =
    sendMsg nsWorkspace (mkSelector "runningApplications") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- voiceOverEnabled@
voiceOverEnabled :: IsNSWorkspace nsWorkspace => nsWorkspace -> IO Bool
voiceOverEnabled nsWorkspace  =
    fmap ((/= 0) :: CULong -> Bool) $ sendMsg nsWorkspace (mkSelector "voiceOverEnabled") retCULong []

-- | @- switchControlEnabled@
switchControlEnabled :: IsNSWorkspace nsWorkspace => nsWorkspace -> IO Bool
switchControlEnabled nsWorkspace  =
    fmap ((/= 0) :: CULong -> Bool) $ sendMsg nsWorkspace (mkSelector "switchControlEnabled") retCULong []

-- | @- accessibilityDisplayShouldIncreaseContrast@
accessibilityDisplayShouldIncreaseContrast :: IsNSWorkspace nsWorkspace => nsWorkspace -> IO Bool
accessibilityDisplayShouldIncreaseContrast nsWorkspace  =
    fmap ((/= 0) :: CULong -> Bool) $ sendMsg nsWorkspace (mkSelector "accessibilityDisplayShouldIncreaseContrast") retCULong []

-- | @- accessibilityDisplayShouldDifferentiateWithoutColor@
accessibilityDisplayShouldDifferentiateWithoutColor :: IsNSWorkspace nsWorkspace => nsWorkspace -> IO Bool
accessibilityDisplayShouldDifferentiateWithoutColor nsWorkspace  =
    fmap ((/= 0) :: CULong -> Bool) $ sendMsg nsWorkspace (mkSelector "accessibilityDisplayShouldDifferentiateWithoutColor") retCULong []

-- | @- accessibilityDisplayShouldReduceTransparency@
accessibilityDisplayShouldReduceTransparency :: IsNSWorkspace nsWorkspace => nsWorkspace -> IO Bool
accessibilityDisplayShouldReduceTransparency nsWorkspace  =
    fmap ((/= 0) :: CULong -> Bool) $ sendMsg nsWorkspace (mkSelector "accessibilityDisplayShouldReduceTransparency") retCULong []

-- | @- accessibilityDisplayShouldReduceMotion@
accessibilityDisplayShouldReduceMotion :: IsNSWorkspace nsWorkspace => nsWorkspace -> IO Bool
accessibilityDisplayShouldReduceMotion nsWorkspace  =
    fmap ((/= 0) :: CULong -> Bool) $ sendMsg nsWorkspace (mkSelector "accessibilityDisplayShouldReduceMotion") retCULong []

-- | @- accessibilityDisplayShouldInvertColors@
accessibilityDisplayShouldInvertColors :: IsNSWorkspace nsWorkspace => nsWorkspace -> IO Bool
accessibilityDisplayShouldInvertColors nsWorkspace  =
    fmap ((/= 0) :: CULong -> Bool) $ sendMsg nsWorkspace (mkSelector "accessibilityDisplayShouldInvertColors") retCULong []

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @openURL:@
openURLSelector :: Selector
openURLSelector = mkSelector "openURL:"

-- | @Selector@ for @openURL:configuration:completionHandler:@
openURL_configuration_completionHandlerSelector :: Selector
openURL_configuration_completionHandlerSelector = mkSelector "openURL:configuration:completionHandler:"

-- | @Selector@ for @openURLs:withApplicationAtURL:configuration:completionHandler:@
openURLs_withApplicationAtURL_configuration_completionHandlerSelector :: Selector
openURLs_withApplicationAtURL_configuration_completionHandlerSelector = mkSelector "openURLs:withApplicationAtURL:configuration:completionHandler:"

-- | @Selector@ for @openApplicationAtURL:configuration:completionHandler:@
openApplicationAtURL_configuration_completionHandlerSelector :: Selector
openApplicationAtURL_configuration_completionHandlerSelector = mkSelector "openApplicationAtURL:configuration:completionHandler:"

-- | @Selector@ for @selectFile:inFileViewerRootedAtPath:@
selectFile_inFileViewerRootedAtPathSelector :: Selector
selectFile_inFileViewerRootedAtPathSelector = mkSelector "selectFile:inFileViewerRootedAtPath:"

-- | @Selector@ for @activateFileViewerSelectingURLs:@
activateFileViewerSelectingURLsSelector :: Selector
activateFileViewerSelectingURLsSelector = mkSelector "activateFileViewerSelectingURLs:"

-- | @Selector@ for @showSearchResultsForQueryString:@
showSearchResultsForQueryStringSelector :: Selector
showSearchResultsForQueryStringSelector = mkSelector "showSearchResultsForQueryString:"

-- | @Selector@ for @noteFileSystemChanged:@
noteFileSystemChangedSelector :: Selector
noteFileSystemChangedSelector = mkSelector "noteFileSystemChanged:"

-- | @Selector@ for @isFilePackageAtPath:@
isFilePackageAtPathSelector :: Selector
isFilePackageAtPathSelector = mkSelector "isFilePackageAtPath:"

-- | @Selector@ for @iconForFile:@
iconForFileSelector :: Selector
iconForFileSelector = mkSelector "iconForFile:"

-- | @Selector@ for @iconForFiles:@
iconForFilesSelector :: Selector
iconForFilesSelector = mkSelector "iconForFiles:"

-- | @Selector@ for @iconForContentType:@
iconForContentTypeSelector :: Selector
iconForContentTypeSelector = mkSelector "iconForContentType:"

-- | @Selector@ for @setIcon:forFile:options:@
setIcon_forFile_optionsSelector :: Selector
setIcon_forFile_optionsSelector = mkSelector "setIcon:forFile:options:"

-- | @Selector@ for @getFileSystemInfoForPath:isRemovable:isWritable:isUnmountable:description:type:@
getFileSystemInfoForPath_isRemovable_isWritable_isUnmountable_description_typeSelector :: Selector
getFileSystemInfoForPath_isRemovable_isWritable_isUnmountable_description_typeSelector = mkSelector "getFileSystemInfoForPath:isRemovable:isWritable:isUnmountable:description:type:"

-- | @Selector@ for @unmountAndEjectDeviceAtPath:@
unmountAndEjectDeviceAtPathSelector :: Selector
unmountAndEjectDeviceAtPathSelector = mkSelector "unmountAndEjectDeviceAtPath:"

-- | @Selector@ for @unmountAndEjectDeviceAtURL:error:@
unmountAndEjectDeviceAtURL_errorSelector :: Selector
unmountAndEjectDeviceAtURL_errorSelector = mkSelector "unmountAndEjectDeviceAtURL:error:"

-- | @Selector@ for @extendPowerOffBy:@
extendPowerOffBySelector :: Selector
extendPowerOffBySelector = mkSelector "extendPowerOffBy:"

-- | @Selector@ for @hideOtherApplications@
hideOtherApplicationsSelector :: Selector
hideOtherApplicationsSelector = mkSelector "hideOtherApplications"

-- | @Selector@ for @URLForApplicationWithBundleIdentifier:@
urlForApplicationWithBundleIdentifierSelector :: Selector
urlForApplicationWithBundleIdentifierSelector = mkSelector "URLForApplicationWithBundleIdentifier:"

-- | @Selector@ for @URLsForApplicationsWithBundleIdentifier:@
urLsForApplicationsWithBundleIdentifierSelector :: Selector
urLsForApplicationsWithBundleIdentifierSelector = mkSelector "URLsForApplicationsWithBundleIdentifier:"

-- | @Selector@ for @URLForApplicationToOpenURL:@
urlForApplicationToOpenURLSelector :: Selector
urlForApplicationToOpenURLSelector = mkSelector "URLForApplicationToOpenURL:"

-- | @Selector@ for @URLsForApplicationsToOpenURL:@
urLsForApplicationsToOpenURLSelector :: Selector
urLsForApplicationsToOpenURLSelector = mkSelector "URLsForApplicationsToOpenURL:"

-- | @Selector@ for @setDefaultApplicationAtURL:toOpenContentTypeOfFileAtURL:completionHandler:@
setDefaultApplicationAtURL_toOpenContentTypeOfFileAtURL_completionHandlerSelector :: Selector
setDefaultApplicationAtURL_toOpenContentTypeOfFileAtURL_completionHandlerSelector = mkSelector "setDefaultApplicationAtURL:toOpenContentTypeOfFileAtURL:completionHandler:"

-- | @Selector@ for @setDefaultApplicationAtURL:toOpenURLsWithScheme:completionHandler:@
setDefaultApplicationAtURL_toOpenURLsWithScheme_completionHandlerSelector :: Selector
setDefaultApplicationAtURL_toOpenURLsWithScheme_completionHandlerSelector = mkSelector "setDefaultApplicationAtURL:toOpenURLsWithScheme:completionHandler:"

-- | @Selector@ for @setDefaultApplicationAtURL:toOpenFileAtURL:completionHandler:@
setDefaultApplicationAtURL_toOpenFileAtURL_completionHandlerSelector :: Selector
setDefaultApplicationAtURL_toOpenFileAtURL_completionHandlerSelector = mkSelector "setDefaultApplicationAtURL:toOpenFileAtURL:completionHandler:"

-- | @Selector@ for @URLForApplicationToOpenContentType:@
urlForApplicationToOpenContentTypeSelector :: Selector
urlForApplicationToOpenContentTypeSelector = mkSelector "URLForApplicationToOpenContentType:"

-- | @Selector@ for @URLsForApplicationsToOpenContentType:@
urLsForApplicationsToOpenContentTypeSelector :: Selector
urLsForApplicationsToOpenContentTypeSelector = mkSelector "URLsForApplicationsToOpenContentType:"

-- | @Selector@ for @setDefaultApplicationAtURL:toOpenContentType:completionHandler:@
setDefaultApplicationAtURL_toOpenContentType_completionHandlerSelector :: Selector
setDefaultApplicationAtURL_toOpenContentType_completionHandlerSelector = mkSelector "setDefaultApplicationAtURL:toOpenContentType:completionHandler:"

-- | @Selector@ for @openFile:@
openFileSelector :: Selector
openFileSelector = mkSelector "openFile:"

-- | @Selector@ for @openFile:withApplication:@
openFile_withApplicationSelector :: Selector
openFile_withApplicationSelector = mkSelector "openFile:withApplication:"

-- | @Selector@ for @openFile:withApplication:andDeactivate:@
openFile_withApplication_andDeactivateSelector :: Selector
openFile_withApplication_andDeactivateSelector = mkSelector "openFile:withApplication:andDeactivate:"

-- | @Selector@ for @launchApplication:@
launchApplicationSelector :: Selector
launchApplicationSelector = mkSelector "launchApplication:"

-- | @Selector@ for @launchApplicationAtURL:options:configuration:error:@
launchApplicationAtURL_options_configuration_errorSelector :: Selector
launchApplicationAtURL_options_configuration_errorSelector = mkSelector "launchApplicationAtURL:options:configuration:error:"

-- | @Selector@ for @openURL:options:configuration:error:@
openURL_options_configuration_errorSelector :: Selector
openURL_options_configuration_errorSelector = mkSelector "openURL:options:configuration:error:"

-- | @Selector@ for @openURLs:withApplicationAtURL:options:configuration:error:@
openURLs_withApplicationAtURL_options_configuration_errorSelector :: Selector
openURLs_withApplicationAtURL_options_configuration_errorSelector = mkSelector "openURLs:withApplicationAtURL:options:configuration:error:"

-- | @Selector@ for @launchApplication:showIcon:autolaunch:@
launchApplication_showIcon_autolaunchSelector :: Selector
launchApplication_showIcon_autolaunchSelector = mkSelector "launchApplication:showIcon:autolaunch:"

-- | @Selector@ for @fullPathForApplication:@
fullPathForApplicationSelector :: Selector
fullPathForApplicationSelector = mkSelector "fullPathForApplication:"

-- | @Selector@ for @absolutePathForAppBundleWithIdentifier:@
absolutePathForAppBundleWithIdentifierSelector :: Selector
absolutePathForAppBundleWithIdentifierSelector = mkSelector "absolutePathForAppBundleWithIdentifier:"

-- | @Selector@ for @launchAppWithBundleIdentifier:options:additionalEventParamDescriptor:launchIdentifier:@
launchAppWithBundleIdentifier_options_additionalEventParamDescriptor_launchIdentifierSelector :: Selector
launchAppWithBundleIdentifier_options_additionalEventParamDescriptor_launchIdentifierSelector = mkSelector "launchAppWithBundleIdentifier:options:additionalEventParamDescriptor:launchIdentifier:"

-- | @Selector@ for @openURLs:withAppBundleIdentifier:options:additionalEventParamDescriptor:launchIdentifiers:@
openURLs_withAppBundleIdentifier_options_additionalEventParamDescriptor_launchIdentifiersSelector :: Selector
openURLs_withAppBundleIdentifier_options_additionalEventParamDescriptor_launchIdentifiersSelector = mkSelector "openURLs:withAppBundleIdentifier:options:additionalEventParamDescriptor:launchIdentifiers:"

-- | @Selector@ for @openTempFile:@
openTempFileSelector :: Selector
openTempFileSelector = mkSelector "openTempFile:"

-- | @Selector@ for @findApplications@
findApplicationsSelector :: Selector
findApplicationsSelector = mkSelector "findApplications"

-- | @Selector@ for @noteUserDefaultsChanged@
noteUserDefaultsChangedSelector :: Selector
noteUserDefaultsChangedSelector = mkSelector "noteUserDefaultsChanged"

-- | @Selector@ for @slideImage:from:to:@
slideImage_from_toSelector :: Selector
slideImage_from_toSelector = mkSelector "slideImage:from:to:"

-- | @Selector@ for @checkForRemovableMedia@
checkForRemovableMediaSelector :: Selector
checkForRemovableMediaSelector = mkSelector "checkForRemovableMedia"

-- | @Selector@ for @fileSystemChanged@
fileSystemChangedSelector :: Selector
fileSystemChangedSelector = mkSelector "fileSystemChanged"

-- | @Selector@ for @userDefaultsChanged@
userDefaultsChangedSelector :: Selector
userDefaultsChangedSelector = mkSelector "userDefaultsChanged"

-- | @Selector@ for @mountNewRemovableMedia@
mountNewRemovableMediaSelector :: Selector
mountNewRemovableMediaSelector = mkSelector "mountNewRemovableMedia"

-- | @Selector@ for @activeApplication@
activeApplicationSelector :: Selector
activeApplicationSelector = mkSelector "activeApplication"

-- | @Selector@ for @mountedLocalVolumePaths@
mountedLocalVolumePathsSelector :: Selector
mountedLocalVolumePathsSelector = mkSelector "mountedLocalVolumePaths"

-- | @Selector@ for @mountedRemovableMedia@
mountedRemovableMediaSelector :: Selector
mountedRemovableMediaSelector = mkSelector "mountedRemovableMedia"

-- | @Selector@ for @launchedApplications@
launchedApplicationsSelector :: Selector
launchedApplicationsSelector = mkSelector "launchedApplications"

-- | @Selector@ for @openFile:fromImage:at:inView:@
openFile_fromImage_at_inViewSelector :: Selector
openFile_fromImage_at_inViewSelector = mkSelector "openFile:fromImage:at:inView:"

-- | @Selector@ for @performFileOperation:source:destination:files:tag:@
performFileOperation_source_destination_files_tagSelector :: Selector
performFileOperation_source_destination_files_tagSelector = mkSelector "performFileOperation:source:destination:files:tag:"

-- | @Selector@ for @getInfoForFile:application:type:@
getInfoForFile_application_typeSelector :: Selector
getInfoForFile_application_typeSelector = mkSelector "getInfoForFile:application:type:"

-- | @Selector@ for @iconForFileType:@
iconForFileTypeSelector :: Selector
iconForFileTypeSelector = mkSelector "iconForFileType:"

-- | @Selector@ for @typeOfFile:error:@
typeOfFile_errorSelector :: Selector
typeOfFile_errorSelector = mkSelector "typeOfFile:error:"

-- | @Selector@ for @localizedDescriptionForType:@
localizedDescriptionForTypeSelector :: Selector
localizedDescriptionForTypeSelector = mkSelector "localizedDescriptionForType:"

-- | @Selector@ for @preferredFilenameExtensionForType:@
preferredFilenameExtensionForTypeSelector :: Selector
preferredFilenameExtensionForTypeSelector = mkSelector "preferredFilenameExtensionForType:"

-- | @Selector@ for @filenameExtension:isValidForType:@
filenameExtension_isValidForTypeSelector :: Selector
filenameExtension_isValidForTypeSelector = mkSelector "filenameExtension:isValidForType:"

-- | @Selector@ for @type:conformsToType:@
type_conformsToTypeSelector :: Selector
type_conformsToTypeSelector = mkSelector "type:conformsToType:"

-- | @Selector@ for @requestAuthorizationOfType:completionHandler:@
requestAuthorizationOfType_completionHandlerSelector :: Selector
requestAuthorizationOfType_completionHandlerSelector = mkSelector "requestAuthorizationOfType:completionHandler:"

-- | @Selector@ for @setDesktopImageURL:forScreen:options:error:@
setDesktopImageURL_forScreen_options_errorSelector :: Selector
setDesktopImageURL_forScreen_options_errorSelector = mkSelector "setDesktopImageURL:forScreen:options:error:"

-- | @Selector@ for @desktopImageURLForScreen:@
desktopImageURLForScreenSelector :: Selector
desktopImageURLForScreenSelector = mkSelector "desktopImageURLForScreen:"

-- | @Selector@ for @desktopImageOptionsForScreen:@
desktopImageOptionsForScreenSelector :: Selector
desktopImageOptionsForScreenSelector = mkSelector "desktopImageOptionsForScreen:"

-- | @Selector@ for @sharedWorkspace@
sharedWorkspaceSelector :: Selector
sharedWorkspaceSelector = mkSelector "sharedWorkspace"

-- | @Selector@ for @notificationCenter@
notificationCenterSelector :: Selector
notificationCenterSelector = mkSelector "notificationCenter"

-- | @Selector@ for @fileLabels@
fileLabelsSelector :: Selector
fileLabelsSelector = mkSelector "fileLabels"

-- | @Selector@ for @fileLabelColors@
fileLabelColorsSelector :: Selector
fileLabelColorsSelector = mkSelector "fileLabelColors"

-- | @Selector@ for @frontmostApplication@
frontmostApplicationSelector :: Selector
frontmostApplicationSelector = mkSelector "frontmostApplication"

-- | @Selector@ for @menuBarOwningApplication@
menuBarOwningApplicationSelector :: Selector
menuBarOwningApplicationSelector = mkSelector "menuBarOwningApplication"

-- | @Selector@ for @runningApplications@
runningApplicationsSelector :: Selector
runningApplicationsSelector = mkSelector "runningApplications"

-- | @Selector@ for @voiceOverEnabled@
voiceOverEnabledSelector :: Selector
voiceOverEnabledSelector = mkSelector "voiceOverEnabled"

-- | @Selector@ for @switchControlEnabled@
switchControlEnabledSelector :: Selector
switchControlEnabledSelector = mkSelector "switchControlEnabled"

-- | @Selector@ for @accessibilityDisplayShouldIncreaseContrast@
accessibilityDisplayShouldIncreaseContrastSelector :: Selector
accessibilityDisplayShouldIncreaseContrastSelector = mkSelector "accessibilityDisplayShouldIncreaseContrast"

-- | @Selector@ for @accessibilityDisplayShouldDifferentiateWithoutColor@
accessibilityDisplayShouldDifferentiateWithoutColorSelector :: Selector
accessibilityDisplayShouldDifferentiateWithoutColorSelector = mkSelector "accessibilityDisplayShouldDifferentiateWithoutColor"

-- | @Selector@ for @accessibilityDisplayShouldReduceTransparency@
accessibilityDisplayShouldReduceTransparencySelector :: Selector
accessibilityDisplayShouldReduceTransparencySelector = mkSelector "accessibilityDisplayShouldReduceTransparency"

-- | @Selector@ for @accessibilityDisplayShouldReduceMotion@
accessibilityDisplayShouldReduceMotionSelector :: Selector
accessibilityDisplayShouldReduceMotionSelector = mkSelector "accessibilityDisplayShouldReduceMotion"

-- | @Selector@ for @accessibilityDisplayShouldInvertColors@
accessibilityDisplayShouldInvertColorsSelector :: Selector
accessibilityDisplayShouldInvertColorsSelector = mkSelector "accessibilityDisplayShouldInvertColors"

