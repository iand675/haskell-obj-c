{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE DataKinds #-}
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
  , absolutePathForAppBundleWithIdentifierSelector
  , accessibilityDisplayShouldDifferentiateWithoutColorSelector
  , accessibilityDisplayShouldIncreaseContrastSelector
  , accessibilityDisplayShouldInvertColorsSelector
  , accessibilityDisplayShouldReduceMotionSelector
  , accessibilityDisplayShouldReduceTransparencySelector
  , activateFileViewerSelectingURLsSelector
  , activeApplicationSelector
  , checkForRemovableMediaSelector
  , desktopImageOptionsForScreenSelector
  , desktopImageURLForScreenSelector
  , extendPowerOffBySelector
  , fileLabelColorsSelector
  , fileLabelsSelector
  , fileSystemChangedSelector
  , filenameExtension_isValidForTypeSelector
  , findApplicationsSelector
  , frontmostApplicationSelector
  , fullPathForApplicationSelector
  , getFileSystemInfoForPath_isRemovable_isWritable_isUnmountable_description_typeSelector
  , getInfoForFile_application_typeSelector
  , hideOtherApplicationsSelector
  , iconForContentTypeSelector
  , iconForFileSelector
  , iconForFileTypeSelector
  , iconForFilesSelector
  , isFilePackageAtPathSelector
  , launchAppWithBundleIdentifier_options_additionalEventParamDescriptor_launchIdentifierSelector
  , launchApplicationAtURL_options_configuration_errorSelector
  , launchApplicationSelector
  , launchApplication_showIcon_autolaunchSelector
  , launchedApplicationsSelector
  , localizedDescriptionForTypeSelector
  , menuBarOwningApplicationSelector
  , mountNewRemovableMediaSelector
  , mountedLocalVolumePathsSelector
  , mountedRemovableMediaSelector
  , noteFileSystemChangedSelector
  , noteUserDefaultsChangedSelector
  , notificationCenterSelector
  , openApplicationAtURL_configuration_completionHandlerSelector
  , openFileSelector
  , openFile_fromImage_at_inViewSelector
  , openFile_withApplicationSelector
  , openFile_withApplication_andDeactivateSelector
  , openTempFileSelector
  , openURLSelector
  , openURL_configuration_completionHandlerSelector
  , openURL_options_configuration_errorSelector
  , openURLs_withAppBundleIdentifier_options_additionalEventParamDescriptor_launchIdentifiersSelector
  , openURLs_withApplicationAtURL_configuration_completionHandlerSelector
  , openURLs_withApplicationAtURL_options_configuration_errorSelector
  , performFileOperation_source_destination_files_tagSelector
  , preferredFilenameExtensionForTypeSelector
  , requestAuthorizationOfType_completionHandlerSelector
  , runningApplicationsSelector
  , selectFile_inFileViewerRootedAtPathSelector
  , setDefaultApplicationAtURL_toOpenContentTypeOfFileAtURL_completionHandlerSelector
  , setDefaultApplicationAtURL_toOpenContentType_completionHandlerSelector
  , setDefaultApplicationAtURL_toOpenFileAtURL_completionHandlerSelector
  , setDefaultApplicationAtURL_toOpenURLsWithScheme_completionHandlerSelector
  , setDesktopImageURL_forScreen_options_errorSelector
  , setIcon_forFile_optionsSelector
  , sharedWorkspaceSelector
  , showSearchResultsForQueryStringSelector
  , slideImage_from_toSelector
  , switchControlEnabledSelector
  , typeOfFile_errorSelector
  , type_conformsToTypeSelector
  , unmountAndEjectDeviceAtPathSelector
  , unmountAndEjectDeviceAtURL_errorSelector
  , urLsForApplicationsToOpenContentTypeSelector
  , urLsForApplicationsToOpenURLSelector
  , urLsForApplicationsWithBundleIdentifierSelector
  , urlForApplicationToOpenContentTypeSelector
  , urlForApplicationToOpenURLSelector
  , urlForApplicationWithBundleIdentifierSelector
  , userDefaultsChangedSelector
  , voiceOverEnabledSelector

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

import Foreign.Ptr (Ptr, FunPtr)
import Foreign.C.Types

import ObjC.Runtime.Types
import ObjC.Runtime.Message (sendMessage, sendOwnedMessage, sendClassMessage, sendOwnedClassMessage)
import ObjC.Runtime.Selector (mkSelector)
import ObjC.Runtime.Class (getRequiredClass)

import ObjC.AppKit.Internal.Classes
import ObjC.Foundation.Internal.Structs
import ObjC.AppKit.Internal.Enums
import ObjC.Foundation.Internal.Classes
import ObjC.UniformTypeIdentifiers.Internal.Classes

-- | @- openURL:@
openURL :: (IsNSWorkspace nsWorkspace, IsNSURL url) => nsWorkspace -> url -> IO Bool
openURL nsWorkspace url =
  sendMessage nsWorkspace openURLSelector (toNSURL url)

-- | @- openURL:configuration:completionHandler:@
openURL_configuration_completionHandler :: (IsNSWorkspace nsWorkspace, IsNSURL url, IsNSWorkspaceOpenConfiguration configuration) => nsWorkspace -> url -> configuration -> Ptr () -> IO ()
openURL_configuration_completionHandler nsWorkspace url configuration completionHandler =
  sendMessage nsWorkspace openURL_configuration_completionHandlerSelector (toNSURL url) (toNSWorkspaceOpenConfiguration configuration) completionHandler

-- | @- openURLs:withApplicationAtURL:configuration:completionHandler:@
openURLs_withApplicationAtURL_configuration_completionHandler :: (IsNSWorkspace nsWorkspace, IsNSArray urls, IsNSURL applicationURL, IsNSWorkspaceOpenConfiguration configuration) => nsWorkspace -> urls -> applicationURL -> configuration -> Ptr () -> IO ()
openURLs_withApplicationAtURL_configuration_completionHandler nsWorkspace urls applicationURL configuration completionHandler =
  sendMessage nsWorkspace openURLs_withApplicationAtURL_configuration_completionHandlerSelector (toNSArray urls) (toNSURL applicationURL) (toNSWorkspaceOpenConfiguration configuration) completionHandler

-- | @- openApplicationAtURL:configuration:completionHandler:@
openApplicationAtURL_configuration_completionHandler :: (IsNSWorkspace nsWorkspace, IsNSURL applicationURL, IsNSWorkspaceOpenConfiguration configuration) => nsWorkspace -> applicationURL -> configuration -> Ptr () -> IO ()
openApplicationAtURL_configuration_completionHandler nsWorkspace applicationURL configuration completionHandler =
  sendMessage nsWorkspace openApplicationAtURL_configuration_completionHandlerSelector (toNSURL applicationURL) (toNSWorkspaceOpenConfiguration configuration) completionHandler

-- | @- selectFile:inFileViewerRootedAtPath:@
selectFile_inFileViewerRootedAtPath :: (IsNSWorkspace nsWorkspace, IsNSString fullPath, IsNSString rootFullPath) => nsWorkspace -> fullPath -> rootFullPath -> IO Bool
selectFile_inFileViewerRootedAtPath nsWorkspace fullPath rootFullPath =
  sendMessage nsWorkspace selectFile_inFileViewerRootedAtPathSelector (toNSString fullPath) (toNSString rootFullPath)

-- | @- activateFileViewerSelectingURLs:@
activateFileViewerSelectingURLs :: (IsNSWorkspace nsWorkspace, IsNSArray fileURLs) => nsWorkspace -> fileURLs -> IO ()
activateFileViewerSelectingURLs nsWorkspace fileURLs =
  sendMessage nsWorkspace activateFileViewerSelectingURLsSelector (toNSArray fileURLs)

-- | @- showSearchResultsForQueryString:@
showSearchResultsForQueryString :: (IsNSWorkspace nsWorkspace, IsNSString queryString) => nsWorkspace -> queryString -> IO Bool
showSearchResultsForQueryString nsWorkspace queryString =
  sendMessage nsWorkspace showSearchResultsForQueryStringSelector (toNSString queryString)

-- | @- noteFileSystemChanged:@
noteFileSystemChanged :: (IsNSWorkspace nsWorkspace, IsNSString path) => nsWorkspace -> path -> IO ()
noteFileSystemChanged nsWorkspace path =
  sendMessage nsWorkspace noteFileSystemChangedSelector (toNSString path)

-- | @- isFilePackageAtPath:@
isFilePackageAtPath :: (IsNSWorkspace nsWorkspace, IsNSString fullPath) => nsWorkspace -> fullPath -> IO Bool
isFilePackageAtPath nsWorkspace fullPath =
  sendMessage nsWorkspace isFilePackageAtPathSelector (toNSString fullPath)

-- | @- iconForFile:@
iconForFile :: (IsNSWorkspace nsWorkspace, IsNSString fullPath) => nsWorkspace -> fullPath -> IO (Id NSImage)
iconForFile nsWorkspace fullPath =
  sendMessage nsWorkspace iconForFileSelector (toNSString fullPath)

-- | @- iconForFiles:@
iconForFiles :: (IsNSWorkspace nsWorkspace, IsNSArray fullPaths) => nsWorkspace -> fullPaths -> IO (Id NSImage)
iconForFiles nsWorkspace fullPaths =
  sendMessage nsWorkspace iconForFilesSelector (toNSArray fullPaths)

-- | @- iconForContentType:@
iconForContentType :: (IsNSWorkspace nsWorkspace, IsUTType contentType) => nsWorkspace -> contentType -> IO (Id NSImage)
iconForContentType nsWorkspace contentType =
  sendMessage nsWorkspace iconForContentTypeSelector (toUTType contentType)

-- | @- setIcon:forFile:options:@
setIcon_forFile_options :: (IsNSWorkspace nsWorkspace, IsNSImage image, IsNSString fullPath) => nsWorkspace -> image -> fullPath -> NSWorkspaceIconCreationOptions -> IO Bool
setIcon_forFile_options nsWorkspace image fullPath options =
  sendMessage nsWorkspace setIcon_forFile_optionsSelector (toNSImage image) (toNSString fullPath) options

-- | @- getFileSystemInfoForPath:isRemovable:isWritable:isUnmountable:description:type:@
getFileSystemInfoForPath_isRemovable_isWritable_isUnmountable_description_type :: (IsNSWorkspace nsWorkspace, IsNSString fullPath, IsNSString description, IsNSString fileSystemType) => nsWorkspace -> fullPath -> Ptr Bool -> Ptr Bool -> Ptr Bool -> description -> fileSystemType -> IO Bool
getFileSystemInfoForPath_isRemovable_isWritable_isUnmountable_description_type nsWorkspace fullPath removableFlag writableFlag unmountableFlag description fileSystemType =
  sendMessage nsWorkspace getFileSystemInfoForPath_isRemovable_isWritable_isUnmountable_description_typeSelector (toNSString fullPath) removableFlag writableFlag unmountableFlag (toNSString description) (toNSString fileSystemType)

-- | @- unmountAndEjectDeviceAtPath:@
unmountAndEjectDeviceAtPath :: (IsNSWorkspace nsWorkspace, IsNSString path) => nsWorkspace -> path -> IO Bool
unmountAndEjectDeviceAtPath nsWorkspace path =
  sendMessage nsWorkspace unmountAndEjectDeviceAtPathSelector (toNSString path)

-- | @- unmountAndEjectDeviceAtURL:error:@
unmountAndEjectDeviceAtURL_error :: (IsNSWorkspace nsWorkspace, IsNSURL url, IsNSError error_) => nsWorkspace -> url -> error_ -> IO Bool
unmountAndEjectDeviceAtURL_error nsWorkspace url error_ =
  sendMessage nsWorkspace unmountAndEjectDeviceAtURL_errorSelector (toNSURL url) (toNSError error_)

-- | @- extendPowerOffBy:@
extendPowerOffBy :: IsNSWorkspace nsWorkspace => nsWorkspace -> CLong -> IO CLong
extendPowerOffBy nsWorkspace requested =
  sendMessage nsWorkspace extendPowerOffBySelector requested

-- | @- hideOtherApplications@
hideOtherApplications :: IsNSWorkspace nsWorkspace => nsWorkspace -> IO ()
hideOtherApplications nsWorkspace =
  sendMessage nsWorkspace hideOtherApplicationsSelector

-- | @- URLForApplicationWithBundleIdentifier:@
urlForApplicationWithBundleIdentifier :: (IsNSWorkspace nsWorkspace, IsNSString bundleIdentifier) => nsWorkspace -> bundleIdentifier -> IO (Id NSURL)
urlForApplicationWithBundleIdentifier nsWorkspace bundleIdentifier =
  sendMessage nsWorkspace urlForApplicationWithBundleIdentifierSelector (toNSString bundleIdentifier)

-- | @- URLsForApplicationsWithBundleIdentifier:@
urLsForApplicationsWithBundleIdentifier :: (IsNSWorkspace nsWorkspace, IsNSString bundleIdentifier) => nsWorkspace -> bundleIdentifier -> IO (Id NSArray)
urLsForApplicationsWithBundleIdentifier nsWorkspace bundleIdentifier =
  sendMessage nsWorkspace urLsForApplicationsWithBundleIdentifierSelector (toNSString bundleIdentifier)

-- | @- URLForApplicationToOpenURL:@
urlForApplicationToOpenURL :: (IsNSWorkspace nsWorkspace, IsNSURL url) => nsWorkspace -> url -> IO (Id NSURL)
urlForApplicationToOpenURL nsWorkspace url =
  sendMessage nsWorkspace urlForApplicationToOpenURLSelector (toNSURL url)

-- | @- URLsForApplicationsToOpenURL:@
urLsForApplicationsToOpenURL :: (IsNSWorkspace nsWorkspace, IsNSURL url) => nsWorkspace -> url -> IO (Id NSArray)
urLsForApplicationsToOpenURL nsWorkspace url =
  sendMessage nsWorkspace urLsForApplicationsToOpenURLSelector (toNSURL url)

-- | @- setDefaultApplicationAtURL:toOpenContentTypeOfFileAtURL:completionHandler:@
setDefaultApplicationAtURL_toOpenContentTypeOfFileAtURL_completionHandler :: (IsNSWorkspace nsWorkspace, IsNSURL applicationURL, IsNSURL url) => nsWorkspace -> applicationURL -> url -> Ptr () -> IO ()
setDefaultApplicationAtURL_toOpenContentTypeOfFileAtURL_completionHandler nsWorkspace applicationURL url completionHandler =
  sendMessage nsWorkspace setDefaultApplicationAtURL_toOpenContentTypeOfFileAtURL_completionHandlerSelector (toNSURL applicationURL) (toNSURL url) completionHandler

-- | @- setDefaultApplicationAtURL:toOpenURLsWithScheme:completionHandler:@
setDefaultApplicationAtURL_toOpenURLsWithScheme_completionHandler :: (IsNSWorkspace nsWorkspace, IsNSURL applicationURL, IsNSString urlScheme) => nsWorkspace -> applicationURL -> urlScheme -> Ptr () -> IO ()
setDefaultApplicationAtURL_toOpenURLsWithScheme_completionHandler nsWorkspace applicationURL urlScheme completionHandler =
  sendMessage nsWorkspace setDefaultApplicationAtURL_toOpenURLsWithScheme_completionHandlerSelector (toNSURL applicationURL) (toNSString urlScheme) completionHandler

-- | @- setDefaultApplicationAtURL:toOpenFileAtURL:completionHandler:@
setDefaultApplicationAtURL_toOpenFileAtURL_completionHandler :: (IsNSWorkspace nsWorkspace, IsNSURL applicationURL, IsNSURL url) => nsWorkspace -> applicationURL -> url -> Ptr () -> IO ()
setDefaultApplicationAtURL_toOpenFileAtURL_completionHandler nsWorkspace applicationURL url completionHandler =
  sendMessage nsWorkspace setDefaultApplicationAtURL_toOpenFileAtURL_completionHandlerSelector (toNSURL applicationURL) (toNSURL url) completionHandler

-- | @- URLForApplicationToOpenContentType:@
urlForApplicationToOpenContentType :: (IsNSWorkspace nsWorkspace, IsUTType contentType) => nsWorkspace -> contentType -> IO (Id NSURL)
urlForApplicationToOpenContentType nsWorkspace contentType =
  sendMessage nsWorkspace urlForApplicationToOpenContentTypeSelector (toUTType contentType)

-- | @- URLsForApplicationsToOpenContentType:@
urLsForApplicationsToOpenContentType :: (IsNSWorkspace nsWorkspace, IsUTType contentType) => nsWorkspace -> contentType -> IO (Id NSArray)
urLsForApplicationsToOpenContentType nsWorkspace contentType =
  sendMessage nsWorkspace urLsForApplicationsToOpenContentTypeSelector (toUTType contentType)

-- | @- setDefaultApplicationAtURL:toOpenContentType:completionHandler:@
setDefaultApplicationAtURL_toOpenContentType_completionHandler :: (IsNSWorkspace nsWorkspace, IsNSURL applicationURL, IsUTType contentType) => nsWorkspace -> applicationURL -> contentType -> Ptr () -> IO ()
setDefaultApplicationAtURL_toOpenContentType_completionHandler nsWorkspace applicationURL contentType completionHandler =
  sendMessage nsWorkspace setDefaultApplicationAtURL_toOpenContentType_completionHandlerSelector (toNSURL applicationURL) (toUTType contentType) completionHandler

-- | @- openFile:@
openFile :: (IsNSWorkspace nsWorkspace, IsNSString fullPath) => nsWorkspace -> fullPath -> IO Bool
openFile nsWorkspace fullPath =
  sendMessage nsWorkspace openFileSelector (toNSString fullPath)

-- | @- openFile:withApplication:@
openFile_withApplication :: (IsNSWorkspace nsWorkspace, IsNSString fullPath, IsNSString appName) => nsWorkspace -> fullPath -> appName -> IO Bool
openFile_withApplication nsWorkspace fullPath appName =
  sendMessage nsWorkspace openFile_withApplicationSelector (toNSString fullPath) (toNSString appName)

-- | @- openFile:withApplication:andDeactivate:@
openFile_withApplication_andDeactivate :: (IsNSWorkspace nsWorkspace, IsNSString fullPath, IsNSString appName) => nsWorkspace -> fullPath -> appName -> Bool -> IO Bool
openFile_withApplication_andDeactivate nsWorkspace fullPath appName flag =
  sendMessage nsWorkspace openFile_withApplication_andDeactivateSelector (toNSString fullPath) (toNSString appName) flag

-- | @- launchApplication:@
launchApplication :: (IsNSWorkspace nsWorkspace, IsNSString appName) => nsWorkspace -> appName -> IO Bool
launchApplication nsWorkspace appName =
  sendMessage nsWorkspace launchApplicationSelector (toNSString appName)

-- | @- launchApplicationAtURL:options:configuration:error:@
launchApplicationAtURL_options_configuration_error :: (IsNSWorkspace nsWorkspace, IsNSURL url, IsNSDictionary configuration, IsNSError error_) => nsWorkspace -> url -> NSWorkspaceLaunchOptions -> configuration -> error_ -> IO (Id NSRunningApplication)
launchApplicationAtURL_options_configuration_error nsWorkspace url options configuration error_ =
  sendMessage nsWorkspace launchApplicationAtURL_options_configuration_errorSelector (toNSURL url) options (toNSDictionary configuration) (toNSError error_)

-- | @- openURL:options:configuration:error:@
openURL_options_configuration_error :: (IsNSWorkspace nsWorkspace, IsNSURL url, IsNSDictionary configuration, IsNSError error_) => nsWorkspace -> url -> NSWorkspaceLaunchOptions -> configuration -> error_ -> IO (Id NSRunningApplication)
openURL_options_configuration_error nsWorkspace url options configuration error_ =
  sendMessage nsWorkspace openURL_options_configuration_errorSelector (toNSURL url) options (toNSDictionary configuration) (toNSError error_)

-- | @- openURLs:withApplicationAtURL:options:configuration:error:@
openURLs_withApplicationAtURL_options_configuration_error :: (IsNSWorkspace nsWorkspace, IsNSArray urls, IsNSURL applicationURL, IsNSDictionary configuration, IsNSError error_) => nsWorkspace -> urls -> applicationURL -> NSWorkspaceLaunchOptions -> configuration -> error_ -> IO (Id NSRunningApplication)
openURLs_withApplicationAtURL_options_configuration_error nsWorkspace urls applicationURL options configuration error_ =
  sendMessage nsWorkspace openURLs_withApplicationAtURL_options_configuration_errorSelector (toNSArray urls) (toNSURL applicationURL) options (toNSDictionary configuration) (toNSError error_)

-- | @- launchApplication:showIcon:autolaunch:@
launchApplication_showIcon_autolaunch :: (IsNSWorkspace nsWorkspace, IsNSString appName) => nsWorkspace -> appName -> Bool -> Bool -> IO Bool
launchApplication_showIcon_autolaunch nsWorkspace appName showIcon autolaunch =
  sendMessage nsWorkspace launchApplication_showIcon_autolaunchSelector (toNSString appName) showIcon autolaunch

-- | @- fullPathForApplication:@
fullPathForApplication :: (IsNSWorkspace nsWorkspace, IsNSString appName) => nsWorkspace -> appName -> IO (Id NSString)
fullPathForApplication nsWorkspace appName =
  sendMessage nsWorkspace fullPathForApplicationSelector (toNSString appName)

-- | @- absolutePathForAppBundleWithIdentifier:@
absolutePathForAppBundleWithIdentifier :: (IsNSWorkspace nsWorkspace, IsNSString bundleIdentifier) => nsWorkspace -> bundleIdentifier -> IO (Id NSString)
absolutePathForAppBundleWithIdentifier nsWorkspace bundleIdentifier =
  sendMessage nsWorkspace absolutePathForAppBundleWithIdentifierSelector (toNSString bundleIdentifier)

-- | @- launchAppWithBundleIdentifier:options:additionalEventParamDescriptor:launchIdentifier:@
launchAppWithBundleIdentifier_options_additionalEventParamDescriptor_launchIdentifier :: (IsNSWorkspace nsWorkspace, IsNSString bundleIdentifier, IsNSAppleEventDescriptor descriptor, IsNSNumber identifier) => nsWorkspace -> bundleIdentifier -> NSWorkspaceLaunchOptions -> descriptor -> identifier -> IO Bool
launchAppWithBundleIdentifier_options_additionalEventParamDescriptor_launchIdentifier nsWorkspace bundleIdentifier options descriptor identifier =
  sendMessage nsWorkspace launchAppWithBundleIdentifier_options_additionalEventParamDescriptor_launchIdentifierSelector (toNSString bundleIdentifier) options (toNSAppleEventDescriptor descriptor) (toNSNumber identifier)

-- | @- openURLs:withAppBundleIdentifier:options:additionalEventParamDescriptor:launchIdentifiers:@
openURLs_withAppBundleIdentifier_options_additionalEventParamDescriptor_launchIdentifiers :: (IsNSWorkspace nsWorkspace, IsNSArray urls, IsNSString bundleIdentifier, IsNSAppleEventDescriptor descriptor, IsNSArray identifiers) => nsWorkspace -> urls -> bundleIdentifier -> NSWorkspaceLaunchOptions -> descriptor -> identifiers -> IO Bool
openURLs_withAppBundleIdentifier_options_additionalEventParamDescriptor_launchIdentifiers nsWorkspace urls bundleIdentifier options descriptor identifiers =
  sendMessage nsWorkspace openURLs_withAppBundleIdentifier_options_additionalEventParamDescriptor_launchIdentifiersSelector (toNSArray urls) (toNSString bundleIdentifier) options (toNSAppleEventDescriptor descriptor) (toNSArray identifiers)

-- | @- openTempFile:@
openTempFile :: (IsNSWorkspace nsWorkspace, IsNSString fullPath) => nsWorkspace -> fullPath -> IO Bool
openTempFile nsWorkspace fullPath =
  sendMessage nsWorkspace openTempFileSelector (toNSString fullPath)

-- | @- findApplications@
findApplications :: IsNSWorkspace nsWorkspace => nsWorkspace -> IO ()
findApplications nsWorkspace =
  sendMessage nsWorkspace findApplicationsSelector

-- | @- noteUserDefaultsChanged@
noteUserDefaultsChanged :: IsNSWorkspace nsWorkspace => nsWorkspace -> IO ()
noteUserDefaultsChanged nsWorkspace =
  sendMessage nsWorkspace noteUserDefaultsChangedSelector

-- | @- slideImage:from:to:@
slideImage_from_to :: (IsNSWorkspace nsWorkspace, IsNSImage image) => nsWorkspace -> image -> NSPoint -> NSPoint -> IO ()
slideImage_from_to nsWorkspace image fromPoint toPoint =
  sendMessage nsWorkspace slideImage_from_toSelector (toNSImage image) fromPoint toPoint

-- | @- checkForRemovableMedia@
checkForRemovableMedia :: IsNSWorkspace nsWorkspace => nsWorkspace -> IO ()
checkForRemovableMedia nsWorkspace =
  sendMessage nsWorkspace checkForRemovableMediaSelector

-- | @- fileSystemChanged@
fileSystemChanged :: IsNSWorkspace nsWorkspace => nsWorkspace -> IO Bool
fileSystemChanged nsWorkspace =
  sendMessage nsWorkspace fileSystemChangedSelector

-- | @- userDefaultsChanged@
userDefaultsChanged :: IsNSWorkspace nsWorkspace => nsWorkspace -> IO Bool
userDefaultsChanged nsWorkspace =
  sendMessage nsWorkspace userDefaultsChangedSelector

-- | @- mountNewRemovableMedia@
mountNewRemovableMedia :: IsNSWorkspace nsWorkspace => nsWorkspace -> IO (Id NSArray)
mountNewRemovableMedia nsWorkspace =
  sendMessage nsWorkspace mountNewRemovableMediaSelector

-- | @- activeApplication@
activeApplication :: IsNSWorkspace nsWorkspace => nsWorkspace -> IO (Id NSDictionary)
activeApplication nsWorkspace =
  sendMessage nsWorkspace activeApplicationSelector

-- | @- mountedLocalVolumePaths@
mountedLocalVolumePaths :: IsNSWorkspace nsWorkspace => nsWorkspace -> IO (Id NSArray)
mountedLocalVolumePaths nsWorkspace =
  sendMessage nsWorkspace mountedLocalVolumePathsSelector

-- | @- mountedRemovableMedia@
mountedRemovableMedia :: IsNSWorkspace nsWorkspace => nsWorkspace -> IO (Id NSArray)
mountedRemovableMedia nsWorkspace =
  sendMessage nsWorkspace mountedRemovableMediaSelector

-- | @- launchedApplications@
launchedApplications :: IsNSWorkspace nsWorkspace => nsWorkspace -> IO (Id NSArray)
launchedApplications nsWorkspace =
  sendMessage nsWorkspace launchedApplicationsSelector

-- | @- openFile:fromImage:at:inView:@
openFile_fromImage_at_inView :: (IsNSWorkspace nsWorkspace, IsNSString fullPath, IsNSImage image, IsNSView view) => nsWorkspace -> fullPath -> image -> NSPoint -> view -> IO Bool
openFile_fromImage_at_inView nsWorkspace fullPath image point view =
  sendMessage nsWorkspace openFile_fromImage_at_inViewSelector (toNSString fullPath) (toNSImage image) point (toNSView view)

-- | @- performFileOperation:source:destination:files:tag:@
performFileOperation_source_destination_files_tag :: (IsNSWorkspace nsWorkspace, IsNSString operation, IsNSString source, IsNSString destination, IsNSArray files) => nsWorkspace -> operation -> source -> destination -> files -> Ptr CLong -> IO Bool
performFileOperation_source_destination_files_tag nsWorkspace operation source destination files tag =
  sendMessage nsWorkspace performFileOperation_source_destination_files_tagSelector (toNSString operation) (toNSString source) (toNSString destination) (toNSArray files) tag

-- | @- getInfoForFile:application:type:@
getInfoForFile_application_type :: (IsNSWorkspace nsWorkspace, IsNSString fullPath, IsNSString appName, IsNSString type_) => nsWorkspace -> fullPath -> appName -> type_ -> IO Bool
getInfoForFile_application_type nsWorkspace fullPath appName type_ =
  sendMessage nsWorkspace getInfoForFile_application_typeSelector (toNSString fullPath) (toNSString appName) (toNSString type_)

-- | @- iconForFileType:@
iconForFileType :: (IsNSWorkspace nsWorkspace, IsNSString fileType) => nsWorkspace -> fileType -> IO (Id NSImage)
iconForFileType nsWorkspace fileType =
  sendMessage nsWorkspace iconForFileTypeSelector (toNSString fileType)

-- | @- typeOfFile:error:@
typeOfFile_error :: (IsNSWorkspace nsWorkspace, IsNSString absoluteFilePath, IsNSError outError) => nsWorkspace -> absoluteFilePath -> outError -> IO (Id NSString)
typeOfFile_error nsWorkspace absoluteFilePath outError =
  sendMessage nsWorkspace typeOfFile_errorSelector (toNSString absoluteFilePath) (toNSError outError)

-- | @- localizedDescriptionForType:@
localizedDescriptionForType :: (IsNSWorkspace nsWorkspace, IsNSString typeName) => nsWorkspace -> typeName -> IO (Id NSString)
localizedDescriptionForType nsWorkspace typeName =
  sendMessage nsWorkspace localizedDescriptionForTypeSelector (toNSString typeName)

-- | @- preferredFilenameExtensionForType:@
preferredFilenameExtensionForType :: (IsNSWorkspace nsWorkspace, IsNSString typeName) => nsWorkspace -> typeName -> IO (Id NSString)
preferredFilenameExtensionForType nsWorkspace typeName =
  sendMessage nsWorkspace preferredFilenameExtensionForTypeSelector (toNSString typeName)

-- | @- filenameExtension:isValidForType:@
filenameExtension_isValidForType :: (IsNSWorkspace nsWorkspace, IsNSString filenameExtension, IsNSString typeName) => nsWorkspace -> filenameExtension -> typeName -> IO Bool
filenameExtension_isValidForType nsWorkspace filenameExtension typeName =
  sendMessage nsWorkspace filenameExtension_isValidForTypeSelector (toNSString filenameExtension) (toNSString typeName)

-- | @- type:conformsToType:@
type_conformsToType :: (IsNSWorkspace nsWorkspace, IsNSString firstTypeName, IsNSString secondTypeName) => nsWorkspace -> firstTypeName -> secondTypeName -> IO Bool
type_conformsToType nsWorkspace firstTypeName secondTypeName =
  sendMessage nsWorkspace type_conformsToTypeSelector (toNSString firstTypeName) (toNSString secondTypeName)

-- | @- requestAuthorizationOfType:completionHandler:@
requestAuthorizationOfType_completionHandler :: IsNSWorkspace nsWorkspace => nsWorkspace -> NSWorkspaceAuthorizationType -> Ptr () -> IO ()
requestAuthorizationOfType_completionHandler nsWorkspace type_ completionHandler =
  sendMessage nsWorkspace requestAuthorizationOfType_completionHandlerSelector type_ completionHandler

-- | @- setDesktopImageURL:forScreen:options:error:@
setDesktopImageURL_forScreen_options_error :: (IsNSWorkspace nsWorkspace, IsNSURL url, IsNSScreen screen, IsNSDictionary options, IsNSError error_) => nsWorkspace -> url -> screen -> options -> error_ -> IO Bool
setDesktopImageURL_forScreen_options_error nsWorkspace url screen options error_ =
  sendMessage nsWorkspace setDesktopImageURL_forScreen_options_errorSelector (toNSURL url) (toNSScreen screen) (toNSDictionary options) (toNSError error_)

-- | @- desktopImageURLForScreen:@
desktopImageURLForScreen :: (IsNSWorkspace nsWorkspace, IsNSScreen screen) => nsWorkspace -> screen -> IO (Id NSURL)
desktopImageURLForScreen nsWorkspace screen =
  sendMessage nsWorkspace desktopImageURLForScreenSelector (toNSScreen screen)

-- | @- desktopImageOptionsForScreen:@
desktopImageOptionsForScreen :: (IsNSWorkspace nsWorkspace, IsNSScreen screen) => nsWorkspace -> screen -> IO (Id NSDictionary)
desktopImageOptionsForScreen nsWorkspace screen =
  sendMessage nsWorkspace desktopImageOptionsForScreenSelector (toNSScreen screen)

-- | @+ sharedWorkspace@
sharedWorkspace :: IO (Id NSWorkspace)
sharedWorkspace  =
  do
    cls' <- getRequiredClass "NSWorkspace"
    sendClassMessage cls' sharedWorkspaceSelector

-- | @- notificationCenter@
notificationCenter :: IsNSWorkspace nsWorkspace => nsWorkspace -> IO (Id NSNotificationCenter)
notificationCenter nsWorkspace =
  sendMessage nsWorkspace notificationCenterSelector

-- | @- fileLabels@
fileLabels :: IsNSWorkspace nsWorkspace => nsWorkspace -> IO (Id NSArray)
fileLabels nsWorkspace =
  sendMessage nsWorkspace fileLabelsSelector

-- | @- fileLabelColors@
fileLabelColors :: IsNSWorkspace nsWorkspace => nsWorkspace -> IO (Id NSArray)
fileLabelColors nsWorkspace =
  sendMessage nsWorkspace fileLabelColorsSelector

-- | @- frontmostApplication@
frontmostApplication :: IsNSWorkspace nsWorkspace => nsWorkspace -> IO (Id NSRunningApplication)
frontmostApplication nsWorkspace =
  sendMessage nsWorkspace frontmostApplicationSelector

-- | @- menuBarOwningApplication@
menuBarOwningApplication :: IsNSWorkspace nsWorkspace => nsWorkspace -> IO (Id NSRunningApplication)
menuBarOwningApplication nsWorkspace =
  sendMessage nsWorkspace menuBarOwningApplicationSelector

-- | Returns: An array of @NSRunningApplication@s representing currently running applications. The order of the array is unspecified, but it is stable, meaning that the relative order of particular applications will not change across multiple calls to @runningApplications@. Similar to @NSRunningApplication@'s properties, this property will only change when the main run loop is run in a common mode.  Instead of polling, use key-value observing to be notified of changes to this array property.This property is thread safe, in that it may be called from background threads and the result is returned atomically.  This property is observable through KVO.
--
-- ObjC selector: @- runningApplications@
runningApplications :: IsNSWorkspace nsWorkspace => nsWorkspace -> IO (Id NSArray)
runningApplications nsWorkspace =
  sendMessage nsWorkspace runningApplicationsSelector

-- | @- voiceOverEnabled@
voiceOverEnabled :: IsNSWorkspace nsWorkspace => nsWorkspace -> IO Bool
voiceOverEnabled nsWorkspace =
  sendMessage nsWorkspace voiceOverEnabledSelector

-- | @- switchControlEnabled@
switchControlEnabled :: IsNSWorkspace nsWorkspace => nsWorkspace -> IO Bool
switchControlEnabled nsWorkspace =
  sendMessage nsWorkspace switchControlEnabledSelector

-- | @- accessibilityDisplayShouldIncreaseContrast@
accessibilityDisplayShouldIncreaseContrast :: IsNSWorkspace nsWorkspace => nsWorkspace -> IO Bool
accessibilityDisplayShouldIncreaseContrast nsWorkspace =
  sendMessage nsWorkspace accessibilityDisplayShouldIncreaseContrastSelector

-- | @- accessibilityDisplayShouldDifferentiateWithoutColor@
accessibilityDisplayShouldDifferentiateWithoutColor :: IsNSWorkspace nsWorkspace => nsWorkspace -> IO Bool
accessibilityDisplayShouldDifferentiateWithoutColor nsWorkspace =
  sendMessage nsWorkspace accessibilityDisplayShouldDifferentiateWithoutColorSelector

-- | @- accessibilityDisplayShouldReduceTransparency@
accessibilityDisplayShouldReduceTransparency :: IsNSWorkspace nsWorkspace => nsWorkspace -> IO Bool
accessibilityDisplayShouldReduceTransparency nsWorkspace =
  sendMessage nsWorkspace accessibilityDisplayShouldReduceTransparencySelector

-- | @- accessibilityDisplayShouldReduceMotion@
accessibilityDisplayShouldReduceMotion :: IsNSWorkspace nsWorkspace => nsWorkspace -> IO Bool
accessibilityDisplayShouldReduceMotion nsWorkspace =
  sendMessage nsWorkspace accessibilityDisplayShouldReduceMotionSelector

-- | @- accessibilityDisplayShouldInvertColors@
accessibilityDisplayShouldInvertColors :: IsNSWorkspace nsWorkspace => nsWorkspace -> IO Bool
accessibilityDisplayShouldInvertColors nsWorkspace =
  sendMessage nsWorkspace accessibilityDisplayShouldInvertColorsSelector

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @openURL:@
openURLSelector :: Selector '[Id NSURL] Bool
openURLSelector = mkSelector "openURL:"

-- | @Selector@ for @openURL:configuration:completionHandler:@
openURL_configuration_completionHandlerSelector :: Selector '[Id NSURL, Id NSWorkspaceOpenConfiguration, Ptr ()] ()
openURL_configuration_completionHandlerSelector = mkSelector "openURL:configuration:completionHandler:"

-- | @Selector@ for @openURLs:withApplicationAtURL:configuration:completionHandler:@
openURLs_withApplicationAtURL_configuration_completionHandlerSelector :: Selector '[Id NSArray, Id NSURL, Id NSWorkspaceOpenConfiguration, Ptr ()] ()
openURLs_withApplicationAtURL_configuration_completionHandlerSelector = mkSelector "openURLs:withApplicationAtURL:configuration:completionHandler:"

-- | @Selector@ for @openApplicationAtURL:configuration:completionHandler:@
openApplicationAtURL_configuration_completionHandlerSelector :: Selector '[Id NSURL, Id NSWorkspaceOpenConfiguration, Ptr ()] ()
openApplicationAtURL_configuration_completionHandlerSelector = mkSelector "openApplicationAtURL:configuration:completionHandler:"

-- | @Selector@ for @selectFile:inFileViewerRootedAtPath:@
selectFile_inFileViewerRootedAtPathSelector :: Selector '[Id NSString, Id NSString] Bool
selectFile_inFileViewerRootedAtPathSelector = mkSelector "selectFile:inFileViewerRootedAtPath:"

-- | @Selector@ for @activateFileViewerSelectingURLs:@
activateFileViewerSelectingURLsSelector :: Selector '[Id NSArray] ()
activateFileViewerSelectingURLsSelector = mkSelector "activateFileViewerSelectingURLs:"

-- | @Selector@ for @showSearchResultsForQueryString:@
showSearchResultsForQueryStringSelector :: Selector '[Id NSString] Bool
showSearchResultsForQueryStringSelector = mkSelector "showSearchResultsForQueryString:"

-- | @Selector@ for @noteFileSystemChanged:@
noteFileSystemChangedSelector :: Selector '[Id NSString] ()
noteFileSystemChangedSelector = mkSelector "noteFileSystemChanged:"

-- | @Selector@ for @isFilePackageAtPath:@
isFilePackageAtPathSelector :: Selector '[Id NSString] Bool
isFilePackageAtPathSelector = mkSelector "isFilePackageAtPath:"

-- | @Selector@ for @iconForFile:@
iconForFileSelector :: Selector '[Id NSString] (Id NSImage)
iconForFileSelector = mkSelector "iconForFile:"

-- | @Selector@ for @iconForFiles:@
iconForFilesSelector :: Selector '[Id NSArray] (Id NSImage)
iconForFilesSelector = mkSelector "iconForFiles:"

-- | @Selector@ for @iconForContentType:@
iconForContentTypeSelector :: Selector '[Id UTType] (Id NSImage)
iconForContentTypeSelector = mkSelector "iconForContentType:"

-- | @Selector@ for @setIcon:forFile:options:@
setIcon_forFile_optionsSelector :: Selector '[Id NSImage, Id NSString, NSWorkspaceIconCreationOptions] Bool
setIcon_forFile_optionsSelector = mkSelector "setIcon:forFile:options:"

-- | @Selector@ for @getFileSystemInfoForPath:isRemovable:isWritable:isUnmountable:description:type:@
getFileSystemInfoForPath_isRemovable_isWritable_isUnmountable_description_typeSelector :: Selector '[Id NSString, Ptr Bool, Ptr Bool, Ptr Bool, Id NSString, Id NSString] Bool
getFileSystemInfoForPath_isRemovable_isWritable_isUnmountable_description_typeSelector = mkSelector "getFileSystemInfoForPath:isRemovable:isWritable:isUnmountable:description:type:"

-- | @Selector@ for @unmountAndEjectDeviceAtPath:@
unmountAndEjectDeviceAtPathSelector :: Selector '[Id NSString] Bool
unmountAndEjectDeviceAtPathSelector = mkSelector "unmountAndEjectDeviceAtPath:"

-- | @Selector@ for @unmountAndEjectDeviceAtURL:error:@
unmountAndEjectDeviceAtURL_errorSelector :: Selector '[Id NSURL, Id NSError] Bool
unmountAndEjectDeviceAtURL_errorSelector = mkSelector "unmountAndEjectDeviceAtURL:error:"

-- | @Selector@ for @extendPowerOffBy:@
extendPowerOffBySelector :: Selector '[CLong] CLong
extendPowerOffBySelector = mkSelector "extendPowerOffBy:"

-- | @Selector@ for @hideOtherApplications@
hideOtherApplicationsSelector :: Selector '[] ()
hideOtherApplicationsSelector = mkSelector "hideOtherApplications"

-- | @Selector@ for @URLForApplicationWithBundleIdentifier:@
urlForApplicationWithBundleIdentifierSelector :: Selector '[Id NSString] (Id NSURL)
urlForApplicationWithBundleIdentifierSelector = mkSelector "URLForApplicationWithBundleIdentifier:"

-- | @Selector@ for @URLsForApplicationsWithBundleIdentifier:@
urLsForApplicationsWithBundleIdentifierSelector :: Selector '[Id NSString] (Id NSArray)
urLsForApplicationsWithBundleIdentifierSelector = mkSelector "URLsForApplicationsWithBundleIdentifier:"

-- | @Selector@ for @URLForApplicationToOpenURL:@
urlForApplicationToOpenURLSelector :: Selector '[Id NSURL] (Id NSURL)
urlForApplicationToOpenURLSelector = mkSelector "URLForApplicationToOpenURL:"

-- | @Selector@ for @URLsForApplicationsToOpenURL:@
urLsForApplicationsToOpenURLSelector :: Selector '[Id NSURL] (Id NSArray)
urLsForApplicationsToOpenURLSelector = mkSelector "URLsForApplicationsToOpenURL:"

-- | @Selector@ for @setDefaultApplicationAtURL:toOpenContentTypeOfFileAtURL:completionHandler:@
setDefaultApplicationAtURL_toOpenContentTypeOfFileAtURL_completionHandlerSelector :: Selector '[Id NSURL, Id NSURL, Ptr ()] ()
setDefaultApplicationAtURL_toOpenContentTypeOfFileAtURL_completionHandlerSelector = mkSelector "setDefaultApplicationAtURL:toOpenContentTypeOfFileAtURL:completionHandler:"

-- | @Selector@ for @setDefaultApplicationAtURL:toOpenURLsWithScheme:completionHandler:@
setDefaultApplicationAtURL_toOpenURLsWithScheme_completionHandlerSelector :: Selector '[Id NSURL, Id NSString, Ptr ()] ()
setDefaultApplicationAtURL_toOpenURLsWithScheme_completionHandlerSelector = mkSelector "setDefaultApplicationAtURL:toOpenURLsWithScheme:completionHandler:"

-- | @Selector@ for @setDefaultApplicationAtURL:toOpenFileAtURL:completionHandler:@
setDefaultApplicationAtURL_toOpenFileAtURL_completionHandlerSelector :: Selector '[Id NSURL, Id NSURL, Ptr ()] ()
setDefaultApplicationAtURL_toOpenFileAtURL_completionHandlerSelector = mkSelector "setDefaultApplicationAtURL:toOpenFileAtURL:completionHandler:"

-- | @Selector@ for @URLForApplicationToOpenContentType:@
urlForApplicationToOpenContentTypeSelector :: Selector '[Id UTType] (Id NSURL)
urlForApplicationToOpenContentTypeSelector = mkSelector "URLForApplicationToOpenContentType:"

-- | @Selector@ for @URLsForApplicationsToOpenContentType:@
urLsForApplicationsToOpenContentTypeSelector :: Selector '[Id UTType] (Id NSArray)
urLsForApplicationsToOpenContentTypeSelector = mkSelector "URLsForApplicationsToOpenContentType:"

-- | @Selector@ for @setDefaultApplicationAtURL:toOpenContentType:completionHandler:@
setDefaultApplicationAtURL_toOpenContentType_completionHandlerSelector :: Selector '[Id NSURL, Id UTType, Ptr ()] ()
setDefaultApplicationAtURL_toOpenContentType_completionHandlerSelector = mkSelector "setDefaultApplicationAtURL:toOpenContentType:completionHandler:"

-- | @Selector@ for @openFile:@
openFileSelector :: Selector '[Id NSString] Bool
openFileSelector = mkSelector "openFile:"

-- | @Selector@ for @openFile:withApplication:@
openFile_withApplicationSelector :: Selector '[Id NSString, Id NSString] Bool
openFile_withApplicationSelector = mkSelector "openFile:withApplication:"

-- | @Selector@ for @openFile:withApplication:andDeactivate:@
openFile_withApplication_andDeactivateSelector :: Selector '[Id NSString, Id NSString, Bool] Bool
openFile_withApplication_andDeactivateSelector = mkSelector "openFile:withApplication:andDeactivate:"

-- | @Selector@ for @launchApplication:@
launchApplicationSelector :: Selector '[Id NSString] Bool
launchApplicationSelector = mkSelector "launchApplication:"

-- | @Selector@ for @launchApplicationAtURL:options:configuration:error:@
launchApplicationAtURL_options_configuration_errorSelector :: Selector '[Id NSURL, NSWorkspaceLaunchOptions, Id NSDictionary, Id NSError] (Id NSRunningApplication)
launchApplicationAtURL_options_configuration_errorSelector = mkSelector "launchApplicationAtURL:options:configuration:error:"

-- | @Selector@ for @openURL:options:configuration:error:@
openURL_options_configuration_errorSelector :: Selector '[Id NSURL, NSWorkspaceLaunchOptions, Id NSDictionary, Id NSError] (Id NSRunningApplication)
openURL_options_configuration_errorSelector = mkSelector "openURL:options:configuration:error:"

-- | @Selector@ for @openURLs:withApplicationAtURL:options:configuration:error:@
openURLs_withApplicationAtURL_options_configuration_errorSelector :: Selector '[Id NSArray, Id NSURL, NSWorkspaceLaunchOptions, Id NSDictionary, Id NSError] (Id NSRunningApplication)
openURLs_withApplicationAtURL_options_configuration_errorSelector = mkSelector "openURLs:withApplicationAtURL:options:configuration:error:"

-- | @Selector@ for @launchApplication:showIcon:autolaunch:@
launchApplication_showIcon_autolaunchSelector :: Selector '[Id NSString, Bool, Bool] Bool
launchApplication_showIcon_autolaunchSelector = mkSelector "launchApplication:showIcon:autolaunch:"

-- | @Selector@ for @fullPathForApplication:@
fullPathForApplicationSelector :: Selector '[Id NSString] (Id NSString)
fullPathForApplicationSelector = mkSelector "fullPathForApplication:"

-- | @Selector@ for @absolutePathForAppBundleWithIdentifier:@
absolutePathForAppBundleWithIdentifierSelector :: Selector '[Id NSString] (Id NSString)
absolutePathForAppBundleWithIdentifierSelector = mkSelector "absolutePathForAppBundleWithIdentifier:"

-- | @Selector@ for @launchAppWithBundleIdentifier:options:additionalEventParamDescriptor:launchIdentifier:@
launchAppWithBundleIdentifier_options_additionalEventParamDescriptor_launchIdentifierSelector :: Selector '[Id NSString, NSWorkspaceLaunchOptions, Id NSAppleEventDescriptor, Id NSNumber] Bool
launchAppWithBundleIdentifier_options_additionalEventParamDescriptor_launchIdentifierSelector = mkSelector "launchAppWithBundleIdentifier:options:additionalEventParamDescriptor:launchIdentifier:"

-- | @Selector@ for @openURLs:withAppBundleIdentifier:options:additionalEventParamDescriptor:launchIdentifiers:@
openURLs_withAppBundleIdentifier_options_additionalEventParamDescriptor_launchIdentifiersSelector :: Selector '[Id NSArray, Id NSString, NSWorkspaceLaunchOptions, Id NSAppleEventDescriptor, Id NSArray] Bool
openURLs_withAppBundleIdentifier_options_additionalEventParamDescriptor_launchIdentifiersSelector = mkSelector "openURLs:withAppBundleIdentifier:options:additionalEventParamDescriptor:launchIdentifiers:"

-- | @Selector@ for @openTempFile:@
openTempFileSelector :: Selector '[Id NSString] Bool
openTempFileSelector = mkSelector "openTempFile:"

-- | @Selector@ for @findApplications@
findApplicationsSelector :: Selector '[] ()
findApplicationsSelector = mkSelector "findApplications"

-- | @Selector@ for @noteUserDefaultsChanged@
noteUserDefaultsChangedSelector :: Selector '[] ()
noteUserDefaultsChangedSelector = mkSelector "noteUserDefaultsChanged"

-- | @Selector@ for @slideImage:from:to:@
slideImage_from_toSelector :: Selector '[Id NSImage, NSPoint, NSPoint] ()
slideImage_from_toSelector = mkSelector "slideImage:from:to:"

-- | @Selector@ for @checkForRemovableMedia@
checkForRemovableMediaSelector :: Selector '[] ()
checkForRemovableMediaSelector = mkSelector "checkForRemovableMedia"

-- | @Selector@ for @fileSystemChanged@
fileSystemChangedSelector :: Selector '[] Bool
fileSystemChangedSelector = mkSelector "fileSystemChanged"

-- | @Selector@ for @userDefaultsChanged@
userDefaultsChangedSelector :: Selector '[] Bool
userDefaultsChangedSelector = mkSelector "userDefaultsChanged"

-- | @Selector@ for @mountNewRemovableMedia@
mountNewRemovableMediaSelector :: Selector '[] (Id NSArray)
mountNewRemovableMediaSelector = mkSelector "mountNewRemovableMedia"

-- | @Selector@ for @activeApplication@
activeApplicationSelector :: Selector '[] (Id NSDictionary)
activeApplicationSelector = mkSelector "activeApplication"

-- | @Selector@ for @mountedLocalVolumePaths@
mountedLocalVolumePathsSelector :: Selector '[] (Id NSArray)
mountedLocalVolumePathsSelector = mkSelector "mountedLocalVolumePaths"

-- | @Selector@ for @mountedRemovableMedia@
mountedRemovableMediaSelector :: Selector '[] (Id NSArray)
mountedRemovableMediaSelector = mkSelector "mountedRemovableMedia"

-- | @Selector@ for @launchedApplications@
launchedApplicationsSelector :: Selector '[] (Id NSArray)
launchedApplicationsSelector = mkSelector "launchedApplications"

-- | @Selector@ for @openFile:fromImage:at:inView:@
openFile_fromImage_at_inViewSelector :: Selector '[Id NSString, Id NSImage, NSPoint, Id NSView] Bool
openFile_fromImage_at_inViewSelector = mkSelector "openFile:fromImage:at:inView:"

-- | @Selector@ for @performFileOperation:source:destination:files:tag:@
performFileOperation_source_destination_files_tagSelector :: Selector '[Id NSString, Id NSString, Id NSString, Id NSArray, Ptr CLong] Bool
performFileOperation_source_destination_files_tagSelector = mkSelector "performFileOperation:source:destination:files:tag:"

-- | @Selector@ for @getInfoForFile:application:type:@
getInfoForFile_application_typeSelector :: Selector '[Id NSString, Id NSString, Id NSString] Bool
getInfoForFile_application_typeSelector = mkSelector "getInfoForFile:application:type:"

-- | @Selector@ for @iconForFileType:@
iconForFileTypeSelector :: Selector '[Id NSString] (Id NSImage)
iconForFileTypeSelector = mkSelector "iconForFileType:"

-- | @Selector@ for @typeOfFile:error:@
typeOfFile_errorSelector :: Selector '[Id NSString, Id NSError] (Id NSString)
typeOfFile_errorSelector = mkSelector "typeOfFile:error:"

-- | @Selector@ for @localizedDescriptionForType:@
localizedDescriptionForTypeSelector :: Selector '[Id NSString] (Id NSString)
localizedDescriptionForTypeSelector = mkSelector "localizedDescriptionForType:"

-- | @Selector@ for @preferredFilenameExtensionForType:@
preferredFilenameExtensionForTypeSelector :: Selector '[Id NSString] (Id NSString)
preferredFilenameExtensionForTypeSelector = mkSelector "preferredFilenameExtensionForType:"

-- | @Selector@ for @filenameExtension:isValidForType:@
filenameExtension_isValidForTypeSelector :: Selector '[Id NSString, Id NSString] Bool
filenameExtension_isValidForTypeSelector = mkSelector "filenameExtension:isValidForType:"

-- | @Selector@ for @type:conformsToType:@
type_conformsToTypeSelector :: Selector '[Id NSString, Id NSString] Bool
type_conformsToTypeSelector = mkSelector "type:conformsToType:"

-- | @Selector@ for @requestAuthorizationOfType:completionHandler:@
requestAuthorizationOfType_completionHandlerSelector :: Selector '[NSWorkspaceAuthorizationType, Ptr ()] ()
requestAuthorizationOfType_completionHandlerSelector = mkSelector "requestAuthorizationOfType:completionHandler:"

-- | @Selector@ for @setDesktopImageURL:forScreen:options:error:@
setDesktopImageURL_forScreen_options_errorSelector :: Selector '[Id NSURL, Id NSScreen, Id NSDictionary, Id NSError] Bool
setDesktopImageURL_forScreen_options_errorSelector = mkSelector "setDesktopImageURL:forScreen:options:error:"

-- | @Selector@ for @desktopImageURLForScreen:@
desktopImageURLForScreenSelector :: Selector '[Id NSScreen] (Id NSURL)
desktopImageURLForScreenSelector = mkSelector "desktopImageURLForScreen:"

-- | @Selector@ for @desktopImageOptionsForScreen:@
desktopImageOptionsForScreenSelector :: Selector '[Id NSScreen] (Id NSDictionary)
desktopImageOptionsForScreenSelector = mkSelector "desktopImageOptionsForScreen:"

-- | @Selector@ for @sharedWorkspace@
sharedWorkspaceSelector :: Selector '[] (Id NSWorkspace)
sharedWorkspaceSelector = mkSelector "sharedWorkspace"

-- | @Selector@ for @notificationCenter@
notificationCenterSelector :: Selector '[] (Id NSNotificationCenter)
notificationCenterSelector = mkSelector "notificationCenter"

-- | @Selector@ for @fileLabels@
fileLabelsSelector :: Selector '[] (Id NSArray)
fileLabelsSelector = mkSelector "fileLabels"

-- | @Selector@ for @fileLabelColors@
fileLabelColorsSelector :: Selector '[] (Id NSArray)
fileLabelColorsSelector = mkSelector "fileLabelColors"

-- | @Selector@ for @frontmostApplication@
frontmostApplicationSelector :: Selector '[] (Id NSRunningApplication)
frontmostApplicationSelector = mkSelector "frontmostApplication"

-- | @Selector@ for @menuBarOwningApplication@
menuBarOwningApplicationSelector :: Selector '[] (Id NSRunningApplication)
menuBarOwningApplicationSelector = mkSelector "menuBarOwningApplication"

-- | @Selector@ for @runningApplications@
runningApplicationsSelector :: Selector '[] (Id NSArray)
runningApplicationsSelector = mkSelector "runningApplications"

-- | @Selector@ for @voiceOverEnabled@
voiceOverEnabledSelector :: Selector '[] Bool
voiceOverEnabledSelector = mkSelector "voiceOverEnabled"

-- | @Selector@ for @switchControlEnabled@
switchControlEnabledSelector :: Selector '[] Bool
switchControlEnabledSelector = mkSelector "switchControlEnabled"

-- | @Selector@ for @accessibilityDisplayShouldIncreaseContrast@
accessibilityDisplayShouldIncreaseContrastSelector :: Selector '[] Bool
accessibilityDisplayShouldIncreaseContrastSelector = mkSelector "accessibilityDisplayShouldIncreaseContrast"

-- | @Selector@ for @accessibilityDisplayShouldDifferentiateWithoutColor@
accessibilityDisplayShouldDifferentiateWithoutColorSelector :: Selector '[] Bool
accessibilityDisplayShouldDifferentiateWithoutColorSelector = mkSelector "accessibilityDisplayShouldDifferentiateWithoutColor"

-- | @Selector@ for @accessibilityDisplayShouldReduceTransparency@
accessibilityDisplayShouldReduceTransparencySelector :: Selector '[] Bool
accessibilityDisplayShouldReduceTransparencySelector = mkSelector "accessibilityDisplayShouldReduceTransparency"

-- | @Selector@ for @accessibilityDisplayShouldReduceMotion@
accessibilityDisplayShouldReduceMotionSelector :: Selector '[] Bool
accessibilityDisplayShouldReduceMotionSelector = mkSelector "accessibilityDisplayShouldReduceMotion"

-- | @Selector@ for @accessibilityDisplayShouldInvertColors@
accessibilityDisplayShouldInvertColorsSelector :: Selector '[] Bool
accessibilityDisplayShouldInvertColorsSelector = mkSelector "accessibilityDisplayShouldInvertColors"

