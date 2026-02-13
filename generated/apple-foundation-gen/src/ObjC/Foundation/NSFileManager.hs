{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Generated bindings for @NSFileManager@.
module ObjC.Foundation.NSFileManager
  ( NSFileManager
  , IsNSFileManager(..)
  , mountedVolumeURLsIncludingResourceValuesForKeys_options
  , unmountVolumeAtURL_options_completionHandler
  , contentsOfDirectoryAtURL_includingPropertiesForKeys_options_error
  , urLsForDirectory_inDomains
  , urlForDirectory_inDomain_appropriateForURL_create_error
  , getRelationship_ofDirectoryAtURL_toItemAtURL_error
  , getRelationship_ofDirectory_inDomain_toItemAtURL_error
  , createDirectoryAtURL_withIntermediateDirectories_attributes_error
  , createSymbolicLinkAtURL_withDestinationURL_error
  , setAttributes_ofItemAtPath_error
  , createDirectoryAtPath_withIntermediateDirectories_attributes_error
  , contentsOfDirectoryAtPath_error
  , subpathsOfDirectoryAtPath_error
  , attributesOfItemAtPath_error
  , attributesOfFileSystemForPath_error
  , createSymbolicLinkAtPath_withDestinationPath_error
  , destinationOfSymbolicLinkAtPath_error
  , copyItemAtPath_toPath_error
  , moveItemAtPath_toPath_error
  , linkItemAtPath_toPath_error
  , removeItemAtPath_error
  , copyItemAtURL_toURL_error
  , moveItemAtURL_toURL_error
  , linkItemAtURL_toURL_error
  , removeItemAtURL_error
  , trashItemAtURL_resultingItemURL_error
  , fileAttributesAtPath_traverseLink
  , changeFileAttributes_atPath
  , directoryContentsAtPath
  , fileSystemAttributesAtPath
  , pathContentOfSymbolicLinkAtPath
  , createSymbolicLinkAtPath_pathContent
  , createDirectoryAtPath_attributes
  , linkPath_toPath_handler
  , copyPath_toPath_handler
  , movePath_toPath_handler
  , removeFileAtPath_handler
  , changeCurrentDirectoryPath
  , fileExistsAtPath
  , fileExistsAtPath_isDirectory
  , isReadableFileAtPath
  , isWritableFileAtPath
  , isExecutableFileAtPath
  , isDeletableFileAtPath
  , contentsEqualAtPath_andPath
  , displayNameAtPath
  , componentsToDisplayForPath
  , enumeratorAtPath
  , enumeratorAtURL_includingPropertiesForKeys_options_errorHandler
  , subpathsAtPath
  , contentsAtPath
  , createFileAtPath_contents_attributes
  , fileSystemRepresentationWithPath
  , stringWithFileSystemRepresentation_length
  , replaceItemAtURL_withItemAtURL_backupItemName_options_resultingItemURL_error
  , setUbiquitous_itemAtURL_destinationURL_error
  , isUbiquitousItemAtURL
  , startDownloadingUbiquitousItemAtURL_error
  , evictUbiquitousItemAtURL_error
  , urlForUbiquityContainerIdentifier
  , urlForPublishingUbiquitousItemAtURL_expirationDate_error
  , pauseSyncForUbiquitousItemAtURL_completionHandler
  , resumeSyncForUbiquitousItemAtURL_withBehavior_completionHandler
  , fetchLatestRemoteVersionOfItemAtURL_completionHandler
  , uploadLocalVersionOfUbiquitousItemAtURL_withConflictResolutionPolicy_completionHandler
  , containerURLForSecurityApplicationGroupIdentifier
  , homeDirectoryForUser
  , defaultManager
  , delegate
  , setDelegate
  , currentDirectoryPath
  , ubiquityIdentityToken
  , homeDirectoryForCurrentUser
  , temporaryDirectory
  , attributesOfFileSystemForPath_errorSelector
  , attributesOfItemAtPath_errorSelector
  , changeCurrentDirectoryPathSelector
  , changeFileAttributes_atPathSelector
  , componentsToDisplayForPathSelector
  , containerURLForSecurityApplicationGroupIdentifierSelector
  , contentsAtPathSelector
  , contentsEqualAtPath_andPathSelector
  , contentsOfDirectoryAtPath_errorSelector
  , contentsOfDirectoryAtURL_includingPropertiesForKeys_options_errorSelector
  , copyItemAtPath_toPath_errorSelector
  , copyItemAtURL_toURL_errorSelector
  , copyPath_toPath_handlerSelector
  , createDirectoryAtPath_attributesSelector
  , createDirectoryAtPath_withIntermediateDirectories_attributes_errorSelector
  , createDirectoryAtURL_withIntermediateDirectories_attributes_errorSelector
  , createFileAtPath_contents_attributesSelector
  , createSymbolicLinkAtPath_pathContentSelector
  , createSymbolicLinkAtPath_withDestinationPath_errorSelector
  , createSymbolicLinkAtURL_withDestinationURL_errorSelector
  , currentDirectoryPathSelector
  , defaultManagerSelector
  , delegateSelector
  , destinationOfSymbolicLinkAtPath_errorSelector
  , directoryContentsAtPathSelector
  , displayNameAtPathSelector
  , enumeratorAtPathSelector
  , enumeratorAtURL_includingPropertiesForKeys_options_errorHandlerSelector
  , evictUbiquitousItemAtURL_errorSelector
  , fetchLatestRemoteVersionOfItemAtURL_completionHandlerSelector
  , fileAttributesAtPath_traverseLinkSelector
  , fileExistsAtPathSelector
  , fileExistsAtPath_isDirectorySelector
  , fileSystemAttributesAtPathSelector
  , fileSystemRepresentationWithPathSelector
  , getRelationship_ofDirectoryAtURL_toItemAtURL_errorSelector
  , getRelationship_ofDirectory_inDomain_toItemAtURL_errorSelector
  , homeDirectoryForCurrentUserSelector
  , homeDirectoryForUserSelector
  , isDeletableFileAtPathSelector
  , isExecutableFileAtPathSelector
  , isReadableFileAtPathSelector
  , isUbiquitousItemAtURLSelector
  , isWritableFileAtPathSelector
  , linkItemAtPath_toPath_errorSelector
  , linkItemAtURL_toURL_errorSelector
  , linkPath_toPath_handlerSelector
  , mountedVolumeURLsIncludingResourceValuesForKeys_optionsSelector
  , moveItemAtPath_toPath_errorSelector
  , moveItemAtURL_toURL_errorSelector
  , movePath_toPath_handlerSelector
  , pathContentOfSymbolicLinkAtPathSelector
  , pauseSyncForUbiquitousItemAtURL_completionHandlerSelector
  , removeFileAtPath_handlerSelector
  , removeItemAtPath_errorSelector
  , removeItemAtURL_errorSelector
  , replaceItemAtURL_withItemAtURL_backupItemName_options_resultingItemURL_errorSelector
  , resumeSyncForUbiquitousItemAtURL_withBehavior_completionHandlerSelector
  , setAttributes_ofItemAtPath_errorSelector
  , setDelegateSelector
  , setUbiquitous_itemAtURL_destinationURL_errorSelector
  , startDownloadingUbiquitousItemAtURL_errorSelector
  , stringWithFileSystemRepresentation_lengthSelector
  , subpathsAtPathSelector
  , subpathsOfDirectoryAtPath_errorSelector
  , temporaryDirectorySelector
  , trashItemAtURL_resultingItemURL_errorSelector
  , ubiquityIdentityTokenSelector
  , unmountVolumeAtURL_options_completionHandlerSelector
  , uploadLocalVersionOfUbiquitousItemAtURL_withConflictResolutionPolicy_completionHandlerSelector
  , urLsForDirectory_inDomainsSelector
  , urlForDirectory_inDomain_appropriateForURL_create_errorSelector
  , urlForPublishingUbiquitousItemAtURL_expirationDate_errorSelector
  , urlForUbiquityContainerIdentifierSelector

  -- * Enum types
  , NSDirectoryEnumerationOptions(NSDirectoryEnumerationOptions)
  , pattern NSDirectoryEnumerationSkipsSubdirectoryDescendants
  , pattern NSDirectoryEnumerationSkipsPackageDescendants
  , pattern NSDirectoryEnumerationSkipsHiddenFiles
  , pattern NSDirectoryEnumerationIncludesDirectoriesPostOrder
  , pattern NSDirectoryEnumerationProducesRelativePathURLs
  , NSFileManagerItemReplacementOptions(NSFileManagerItemReplacementOptions)
  , pattern NSFileManagerItemReplacementUsingNewMetadataOnly
  , pattern NSFileManagerItemReplacementWithoutDeletingBackupItem
  , NSFileManagerResumeSyncBehavior(NSFileManagerResumeSyncBehavior)
  , pattern NSFileManagerResumeSyncBehaviorPreserveLocalChanges
  , pattern NSFileManagerResumeSyncBehaviorAfterUploadWithFailOnConflict
  , pattern NSFileManagerResumeSyncBehaviorDropLocalChanges
  , NSFileManagerUnmountOptions(NSFileManagerUnmountOptions)
  , pattern NSFileManagerUnmountAllPartitionsAndEjectDisk
  , pattern NSFileManagerUnmountWithoutUI
  , NSFileManagerUploadLocalVersionConflictPolicy(NSFileManagerUploadLocalVersionConflictPolicy)
  , pattern NSFileManagerUploadConflictPolicyDefault
  , pattern NSFileManagerUploadConflictPolicyFailOnConflict
  , NSSearchPathDirectory(NSSearchPathDirectory)
  , pattern NSApplicationDirectory
  , pattern NSDemoApplicationDirectory
  , pattern NSDeveloperApplicationDirectory
  , pattern NSAdminApplicationDirectory
  , pattern NSLibraryDirectory
  , pattern NSDeveloperDirectory
  , pattern NSUserDirectory
  , pattern NSDocumentationDirectory
  , pattern NSDocumentDirectory
  , pattern NSCoreServiceDirectory
  , pattern NSAutosavedInformationDirectory
  , pattern NSDesktopDirectory
  , pattern NSCachesDirectory
  , pattern NSApplicationSupportDirectory
  , pattern NSDownloadsDirectory
  , pattern NSInputMethodsDirectory
  , pattern NSMoviesDirectory
  , pattern NSMusicDirectory
  , pattern NSPicturesDirectory
  , pattern NSPrinterDescriptionDirectory
  , pattern NSSharedPublicDirectory
  , pattern NSPreferencePanesDirectory
  , pattern NSApplicationScriptsDirectory
  , pattern NSItemReplacementDirectory
  , pattern NSAllApplicationsDirectory
  , pattern NSAllLibrariesDirectory
  , pattern NSTrashDirectory
  , NSSearchPathDomainMask(NSSearchPathDomainMask)
  , pattern NSUserDomainMask
  , pattern NSLocalDomainMask
  , pattern NSNetworkDomainMask
  , pattern NSSystemDomainMask
  , pattern NSAllDomainsMask
  , NSVolumeEnumerationOptions(NSVolumeEnumerationOptions)
  , pattern NSVolumeEnumerationSkipHiddenVolumes
  , pattern NSVolumeEnumerationProduceFileReferenceURLs

  ) where

import Foreign.Ptr (Ptr, FunPtr)
import Foreign.C.Types

import ObjC.Runtime.Types
import ObjC.Runtime.Message (sendMessage, sendOwnedMessage, sendClassMessage, sendOwnedClassMessage)
import ObjC.Runtime.Selector (mkSelector)
import ObjC.Runtime.Class (getRequiredClass)

import ObjC.Foundation.Internal.Classes
import ObjC.Foundation.Internal.Enums

-- | @- mountedVolumeURLsIncludingResourceValuesForKeys:options:@
mountedVolumeURLsIncludingResourceValuesForKeys_options :: (IsNSFileManager nsFileManager, IsNSArray propertyKeys) => nsFileManager -> propertyKeys -> NSVolumeEnumerationOptions -> IO (Id NSArray)
mountedVolumeURLsIncludingResourceValuesForKeys_options nsFileManager propertyKeys options =
  sendMessage nsFileManager mountedVolumeURLsIncludingResourceValuesForKeys_optionsSelector (toNSArray propertyKeys) options

-- | @- unmountVolumeAtURL:options:completionHandler:@
unmountVolumeAtURL_options_completionHandler :: (IsNSFileManager nsFileManager, IsNSURL url) => nsFileManager -> url -> NSFileManagerUnmountOptions -> Ptr () -> IO ()
unmountVolumeAtURL_options_completionHandler nsFileManager url mask completionHandler =
  sendMessage nsFileManager unmountVolumeAtURL_options_completionHandlerSelector (toNSURL url) mask completionHandler

-- | @- contentsOfDirectoryAtURL:includingPropertiesForKeys:options:error:@
contentsOfDirectoryAtURL_includingPropertiesForKeys_options_error :: (IsNSFileManager nsFileManager, IsNSURL url, IsNSArray keys, IsNSError error_) => nsFileManager -> url -> keys -> NSDirectoryEnumerationOptions -> error_ -> IO (Id NSArray)
contentsOfDirectoryAtURL_includingPropertiesForKeys_options_error nsFileManager url keys mask error_ =
  sendMessage nsFileManager contentsOfDirectoryAtURL_includingPropertiesForKeys_options_errorSelector (toNSURL url) (toNSArray keys) mask (toNSError error_)

-- | @- URLsForDirectory:inDomains:@
urLsForDirectory_inDomains :: IsNSFileManager nsFileManager => nsFileManager -> NSSearchPathDirectory -> NSSearchPathDomainMask -> IO (Id NSArray)
urLsForDirectory_inDomains nsFileManager directory domainMask =
  sendMessage nsFileManager urLsForDirectory_inDomainsSelector directory domainMask

-- | @- URLForDirectory:inDomain:appropriateForURL:create:error:@
urlForDirectory_inDomain_appropriateForURL_create_error :: (IsNSFileManager nsFileManager, IsNSURL url, IsNSError error_) => nsFileManager -> NSSearchPathDirectory -> NSSearchPathDomainMask -> url -> Bool -> error_ -> IO (Id NSURL)
urlForDirectory_inDomain_appropriateForURL_create_error nsFileManager directory domain url shouldCreate error_ =
  sendMessage nsFileManager urlForDirectory_inDomain_appropriateForURL_create_errorSelector directory domain (toNSURL url) shouldCreate (toNSError error_)

-- | @- getRelationship:ofDirectoryAtURL:toItemAtURL:error:@
getRelationship_ofDirectoryAtURL_toItemAtURL_error :: (IsNSFileManager nsFileManager, IsNSURL directoryURL, IsNSURL otherURL, IsNSError error_) => nsFileManager -> Ptr NSURLRelationship -> directoryURL -> otherURL -> error_ -> IO Bool
getRelationship_ofDirectoryAtURL_toItemAtURL_error nsFileManager outRelationship directoryURL otherURL error_ =
  sendMessage nsFileManager getRelationship_ofDirectoryAtURL_toItemAtURL_errorSelector outRelationship (toNSURL directoryURL) (toNSURL otherURL) (toNSError error_)

-- | @- getRelationship:ofDirectory:inDomain:toItemAtURL:error:@
getRelationship_ofDirectory_inDomain_toItemAtURL_error :: (IsNSFileManager nsFileManager, IsNSURL url, IsNSError error_) => nsFileManager -> Ptr NSURLRelationship -> NSSearchPathDirectory -> NSSearchPathDomainMask -> url -> error_ -> IO Bool
getRelationship_ofDirectory_inDomain_toItemAtURL_error nsFileManager outRelationship directory domainMask url error_ =
  sendMessage nsFileManager getRelationship_ofDirectory_inDomain_toItemAtURL_errorSelector outRelationship directory domainMask (toNSURL url) (toNSError error_)

-- | @- createDirectoryAtURL:withIntermediateDirectories:attributes:error:@
createDirectoryAtURL_withIntermediateDirectories_attributes_error :: (IsNSFileManager nsFileManager, IsNSURL url, IsNSDictionary attributes, IsNSError error_) => nsFileManager -> url -> Bool -> attributes -> error_ -> IO Bool
createDirectoryAtURL_withIntermediateDirectories_attributes_error nsFileManager url createIntermediates attributes error_ =
  sendMessage nsFileManager createDirectoryAtURL_withIntermediateDirectories_attributes_errorSelector (toNSURL url) createIntermediates (toNSDictionary attributes) (toNSError error_)

-- | @- createSymbolicLinkAtURL:withDestinationURL:error:@
createSymbolicLinkAtURL_withDestinationURL_error :: (IsNSFileManager nsFileManager, IsNSURL url, IsNSURL destURL, IsNSError error_) => nsFileManager -> url -> destURL -> error_ -> IO Bool
createSymbolicLinkAtURL_withDestinationURL_error nsFileManager url destURL error_ =
  sendMessage nsFileManager createSymbolicLinkAtURL_withDestinationURL_errorSelector (toNSURL url) (toNSURL destURL) (toNSError error_)

-- | @- setAttributes:ofItemAtPath:error:@
setAttributes_ofItemAtPath_error :: (IsNSFileManager nsFileManager, IsNSDictionary attributes, IsNSString path, IsNSError error_) => nsFileManager -> attributes -> path -> error_ -> IO Bool
setAttributes_ofItemAtPath_error nsFileManager attributes path error_ =
  sendMessage nsFileManager setAttributes_ofItemAtPath_errorSelector (toNSDictionary attributes) (toNSString path) (toNSError error_)

-- | @- createDirectoryAtPath:withIntermediateDirectories:attributes:error:@
createDirectoryAtPath_withIntermediateDirectories_attributes_error :: (IsNSFileManager nsFileManager, IsNSString path, IsNSDictionary attributes, IsNSError error_) => nsFileManager -> path -> Bool -> attributes -> error_ -> IO Bool
createDirectoryAtPath_withIntermediateDirectories_attributes_error nsFileManager path createIntermediates attributes error_ =
  sendMessage nsFileManager createDirectoryAtPath_withIntermediateDirectories_attributes_errorSelector (toNSString path) createIntermediates (toNSDictionary attributes) (toNSError error_)

-- | @- contentsOfDirectoryAtPath:error:@
contentsOfDirectoryAtPath_error :: (IsNSFileManager nsFileManager, IsNSString path, IsNSError error_) => nsFileManager -> path -> error_ -> IO (Id NSArray)
contentsOfDirectoryAtPath_error nsFileManager path error_ =
  sendMessage nsFileManager contentsOfDirectoryAtPath_errorSelector (toNSString path) (toNSError error_)

-- | @- subpathsOfDirectoryAtPath:error:@
subpathsOfDirectoryAtPath_error :: (IsNSFileManager nsFileManager, IsNSString path, IsNSError error_) => nsFileManager -> path -> error_ -> IO (Id NSArray)
subpathsOfDirectoryAtPath_error nsFileManager path error_ =
  sendMessage nsFileManager subpathsOfDirectoryAtPath_errorSelector (toNSString path) (toNSError error_)

-- | @- attributesOfItemAtPath:error:@
attributesOfItemAtPath_error :: (IsNSFileManager nsFileManager, IsNSString path, IsNSError error_) => nsFileManager -> path -> error_ -> IO (Id NSDictionary)
attributesOfItemAtPath_error nsFileManager path error_ =
  sendMessage nsFileManager attributesOfItemAtPath_errorSelector (toNSString path) (toNSError error_)

-- | @- attributesOfFileSystemForPath:error:@
attributesOfFileSystemForPath_error :: (IsNSFileManager nsFileManager, IsNSString path, IsNSError error_) => nsFileManager -> path -> error_ -> IO (Id NSDictionary)
attributesOfFileSystemForPath_error nsFileManager path error_ =
  sendMessage nsFileManager attributesOfFileSystemForPath_errorSelector (toNSString path) (toNSError error_)

-- | @- createSymbolicLinkAtPath:withDestinationPath:error:@
createSymbolicLinkAtPath_withDestinationPath_error :: (IsNSFileManager nsFileManager, IsNSString path, IsNSString destPath, IsNSError error_) => nsFileManager -> path -> destPath -> error_ -> IO Bool
createSymbolicLinkAtPath_withDestinationPath_error nsFileManager path destPath error_ =
  sendMessage nsFileManager createSymbolicLinkAtPath_withDestinationPath_errorSelector (toNSString path) (toNSString destPath) (toNSError error_)

-- | @- destinationOfSymbolicLinkAtPath:error:@
destinationOfSymbolicLinkAtPath_error :: (IsNSFileManager nsFileManager, IsNSString path, IsNSError error_) => nsFileManager -> path -> error_ -> IO (Id NSString)
destinationOfSymbolicLinkAtPath_error nsFileManager path error_ =
  sendMessage nsFileManager destinationOfSymbolicLinkAtPath_errorSelector (toNSString path) (toNSError error_)

-- | @- copyItemAtPath:toPath:error:@
copyItemAtPath_toPath_error :: (IsNSFileManager nsFileManager, IsNSString srcPath, IsNSString dstPath, IsNSError error_) => nsFileManager -> srcPath -> dstPath -> error_ -> IO Bool
copyItemAtPath_toPath_error nsFileManager srcPath dstPath error_ =
  sendOwnedMessage nsFileManager copyItemAtPath_toPath_errorSelector (toNSString srcPath) (toNSString dstPath) (toNSError error_)

-- | @- moveItemAtPath:toPath:error:@
moveItemAtPath_toPath_error :: (IsNSFileManager nsFileManager, IsNSString srcPath, IsNSString dstPath, IsNSError error_) => nsFileManager -> srcPath -> dstPath -> error_ -> IO Bool
moveItemAtPath_toPath_error nsFileManager srcPath dstPath error_ =
  sendMessage nsFileManager moveItemAtPath_toPath_errorSelector (toNSString srcPath) (toNSString dstPath) (toNSError error_)

-- | @- linkItemAtPath:toPath:error:@
linkItemAtPath_toPath_error :: (IsNSFileManager nsFileManager, IsNSString srcPath, IsNSString dstPath, IsNSError error_) => nsFileManager -> srcPath -> dstPath -> error_ -> IO Bool
linkItemAtPath_toPath_error nsFileManager srcPath dstPath error_ =
  sendMessage nsFileManager linkItemAtPath_toPath_errorSelector (toNSString srcPath) (toNSString dstPath) (toNSError error_)

-- | @- removeItemAtPath:error:@
removeItemAtPath_error :: (IsNSFileManager nsFileManager, IsNSString path, IsNSError error_) => nsFileManager -> path -> error_ -> IO Bool
removeItemAtPath_error nsFileManager path error_ =
  sendMessage nsFileManager removeItemAtPath_errorSelector (toNSString path) (toNSError error_)

-- | @- copyItemAtURL:toURL:error:@
copyItemAtURL_toURL_error :: (IsNSFileManager nsFileManager, IsNSURL srcURL, IsNSURL dstURL, IsNSError error_) => nsFileManager -> srcURL -> dstURL -> error_ -> IO Bool
copyItemAtURL_toURL_error nsFileManager srcURL dstURL error_ =
  sendOwnedMessage nsFileManager copyItemAtURL_toURL_errorSelector (toNSURL srcURL) (toNSURL dstURL) (toNSError error_)

-- | @- moveItemAtURL:toURL:error:@
moveItemAtURL_toURL_error :: (IsNSFileManager nsFileManager, IsNSURL srcURL, IsNSURL dstURL, IsNSError error_) => nsFileManager -> srcURL -> dstURL -> error_ -> IO Bool
moveItemAtURL_toURL_error nsFileManager srcURL dstURL error_ =
  sendMessage nsFileManager moveItemAtURL_toURL_errorSelector (toNSURL srcURL) (toNSURL dstURL) (toNSError error_)

-- | @- linkItemAtURL:toURL:error:@
linkItemAtURL_toURL_error :: (IsNSFileManager nsFileManager, IsNSURL srcURL, IsNSURL dstURL, IsNSError error_) => nsFileManager -> srcURL -> dstURL -> error_ -> IO Bool
linkItemAtURL_toURL_error nsFileManager srcURL dstURL error_ =
  sendMessage nsFileManager linkItemAtURL_toURL_errorSelector (toNSURL srcURL) (toNSURL dstURL) (toNSError error_)

-- | @- removeItemAtURL:error:@
removeItemAtURL_error :: (IsNSFileManager nsFileManager, IsNSURL url, IsNSError error_) => nsFileManager -> url -> error_ -> IO Bool
removeItemAtURL_error nsFileManager url error_ =
  sendMessage nsFileManager removeItemAtURL_errorSelector (toNSURL url) (toNSError error_)

-- | @- trashItemAtURL:resultingItemURL:error:@
trashItemAtURL_resultingItemURL_error :: (IsNSFileManager nsFileManager, IsNSURL url, IsNSURL outResultingURL, IsNSError error_) => nsFileManager -> url -> outResultingURL -> error_ -> IO Bool
trashItemAtURL_resultingItemURL_error nsFileManager url outResultingURL error_ =
  sendMessage nsFileManager trashItemAtURL_resultingItemURL_errorSelector (toNSURL url) (toNSURL outResultingURL) (toNSError error_)

-- | @- fileAttributesAtPath:traverseLink:@
fileAttributesAtPath_traverseLink :: (IsNSFileManager nsFileManager, IsNSString path) => nsFileManager -> path -> Bool -> IO (Id NSDictionary)
fileAttributesAtPath_traverseLink nsFileManager path yorn =
  sendMessage nsFileManager fileAttributesAtPath_traverseLinkSelector (toNSString path) yorn

-- | @- changeFileAttributes:atPath:@
changeFileAttributes_atPath :: (IsNSFileManager nsFileManager, IsNSDictionary attributes, IsNSString path) => nsFileManager -> attributes -> path -> IO Bool
changeFileAttributes_atPath nsFileManager attributes path =
  sendMessage nsFileManager changeFileAttributes_atPathSelector (toNSDictionary attributes) (toNSString path)

-- | @- directoryContentsAtPath:@
directoryContentsAtPath :: (IsNSFileManager nsFileManager, IsNSString path) => nsFileManager -> path -> IO (Id NSArray)
directoryContentsAtPath nsFileManager path =
  sendMessage nsFileManager directoryContentsAtPathSelector (toNSString path)

-- | @- fileSystemAttributesAtPath:@
fileSystemAttributesAtPath :: (IsNSFileManager nsFileManager, IsNSString path) => nsFileManager -> path -> IO (Id NSDictionary)
fileSystemAttributesAtPath nsFileManager path =
  sendMessage nsFileManager fileSystemAttributesAtPathSelector (toNSString path)

-- | @- pathContentOfSymbolicLinkAtPath:@
pathContentOfSymbolicLinkAtPath :: (IsNSFileManager nsFileManager, IsNSString path) => nsFileManager -> path -> IO (Id NSString)
pathContentOfSymbolicLinkAtPath nsFileManager path =
  sendMessage nsFileManager pathContentOfSymbolicLinkAtPathSelector (toNSString path)

-- | @- createSymbolicLinkAtPath:pathContent:@
createSymbolicLinkAtPath_pathContent :: (IsNSFileManager nsFileManager, IsNSString path, IsNSString otherpath) => nsFileManager -> path -> otherpath -> IO Bool
createSymbolicLinkAtPath_pathContent nsFileManager path otherpath =
  sendMessage nsFileManager createSymbolicLinkAtPath_pathContentSelector (toNSString path) (toNSString otherpath)

-- | @- createDirectoryAtPath:attributes:@
createDirectoryAtPath_attributes :: (IsNSFileManager nsFileManager, IsNSString path, IsNSDictionary attributes) => nsFileManager -> path -> attributes -> IO Bool
createDirectoryAtPath_attributes nsFileManager path attributes =
  sendMessage nsFileManager createDirectoryAtPath_attributesSelector (toNSString path) (toNSDictionary attributes)

-- | @- linkPath:toPath:handler:@
linkPath_toPath_handler :: (IsNSFileManager nsFileManager, IsNSString src, IsNSString dest) => nsFileManager -> src -> dest -> RawId -> IO Bool
linkPath_toPath_handler nsFileManager src dest handler =
  sendMessage nsFileManager linkPath_toPath_handlerSelector (toNSString src) (toNSString dest) handler

-- | @- copyPath:toPath:handler:@
copyPath_toPath_handler :: (IsNSFileManager nsFileManager, IsNSString src, IsNSString dest) => nsFileManager -> src -> dest -> RawId -> IO Bool
copyPath_toPath_handler nsFileManager src dest handler =
  sendOwnedMessage nsFileManager copyPath_toPath_handlerSelector (toNSString src) (toNSString dest) handler

-- | @- movePath:toPath:handler:@
movePath_toPath_handler :: (IsNSFileManager nsFileManager, IsNSString src, IsNSString dest) => nsFileManager -> src -> dest -> RawId -> IO Bool
movePath_toPath_handler nsFileManager src dest handler =
  sendMessage nsFileManager movePath_toPath_handlerSelector (toNSString src) (toNSString dest) handler

-- | @- removeFileAtPath:handler:@
removeFileAtPath_handler :: (IsNSFileManager nsFileManager, IsNSString path) => nsFileManager -> path -> RawId -> IO Bool
removeFileAtPath_handler nsFileManager path handler =
  sendMessage nsFileManager removeFileAtPath_handlerSelector (toNSString path) handler

-- | @- changeCurrentDirectoryPath:@
changeCurrentDirectoryPath :: (IsNSFileManager nsFileManager, IsNSString path) => nsFileManager -> path -> IO Bool
changeCurrentDirectoryPath nsFileManager path =
  sendMessage nsFileManager changeCurrentDirectoryPathSelector (toNSString path)

-- | @- fileExistsAtPath:@
fileExistsAtPath :: (IsNSFileManager nsFileManager, IsNSString path) => nsFileManager -> path -> IO Bool
fileExistsAtPath nsFileManager path =
  sendMessage nsFileManager fileExistsAtPathSelector (toNSString path)

-- | @- fileExistsAtPath:isDirectory:@
fileExistsAtPath_isDirectory :: (IsNSFileManager nsFileManager, IsNSString path) => nsFileManager -> path -> Ptr Bool -> IO Bool
fileExistsAtPath_isDirectory nsFileManager path isDirectory =
  sendMessage nsFileManager fileExistsAtPath_isDirectorySelector (toNSString path) isDirectory

-- | @- isReadableFileAtPath:@
isReadableFileAtPath :: (IsNSFileManager nsFileManager, IsNSString path) => nsFileManager -> path -> IO Bool
isReadableFileAtPath nsFileManager path =
  sendMessage nsFileManager isReadableFileAtPathSelector (toNSString path)

-- | @- isWritableFileAtPath:@
isWritableFileAtPath :: (IsNSFileManager nsFileManager, IsNSString path) => nsFileManager -> path -> IO Bool
isWritableFileAtPath nsFileManager path =
  sendMessage nsFileManager isWritableFileAtPathSelector (toNSString path)

-- | @- isExecutableFileAtPath:@
isExecutableFileAtPath :: (IsNSFileManager nsFileManager, IsNSString path) => nsFileManager -> path -> IO Bool
isExecutableFileAtPath nsFileManager path =
  sendMessage nsFileManager isExecutableFileAtPathSelector (toNSString path)

-- | @- isDeletableFileAtPath:@
isDeletableFileAtPath :: (IsNSFileManager nsFileManager, IsNSString path) => nsFileManager -> path -> IO Bool
isDeletableFileAtPath nsFileManager path =
  sendMessage nsFileManager isDeletableFileAtPathSelector (toNSString path)

-- | @- contentsEqualAtPath:andPath:@
contentsEqualAtPath_andPath :: (IsNSFileManager nsFileManager, IsNSString path1, IsNSString path2) => nsFileManager -> path1 -> path2 -> IO Bool
contentsEqualAtPath_andPath nsFileManager path1 path2 =
  sendMessage nsFileManager contentsEqualAtPath_andPathSelector (toNSString path1) (toNSString path2)

-- | @- displayNameAtPath:@
displayNameAtPath :: (IsNSFileManager nsFileManager, IsNSString path) => nsFileManager -> path -> IO (Id NSString)
displayNameAtPath nsFileManager path =
  sendMessage nsFileManager displayNameAtPathSelector (toNSString path)

-- | @- componentsToDisplayForPath:@
componentsToDisplayForPath :: (IsNSFileManager nsFileManager, IsNSString path) => nsFileManager -> path -> IO (Id NSArray)
componentsToDisplayForPath nsFileManager path =
  sendMessage nsFileManager componentsToDisplayForPathSelector (toNSString path)

-- | @- enumeratorAtPath:@
enumeratorAtPath :: (IsNSFileManager nsFileManager, IsNSString path) => nsFileManager -> path -> IO (Id NSDirectoryEnumerator)
enumeratorAtPath nsFileManager path =
  sendMessage nsFileManager enumeratorAtPathSelector (toNSString path)

-- | @- enumeratorAtURL:includingPropertiesForKeys:options:errorHandler:@
enumeratorAtURL_includingPropertiesForKeys_options_errorHandler :: (IsNSFileManager nsFileManager, IsNSURL url, IsNSArray keys) => nsFileManager -> url -> keys -> NSDirectoryEnumerationOptions -> Ptr () -> IO (Id NSDirectoryEnumerator)
enumeratorAtURL_includingPropertiesForKeys_options_errorHandler nsFileManager url keys mask handler =
  sendMessage nsFileManager enumeratorAtURL_includingPropertiesForKeys_options_errorHandlerSelector (toNSURL url) (toNSArray keys) mask handler

-- | @- subpathsAtPath:@
subpathsAtPath :: (IsNSFileManager nsFileManager, IsNSString path) => nsFileManager -> path -> IO (Id NSArray)
subpathsAtPath nsFileManager path =
  sendMessage nsFileManager subpathsAtPathSelector (toNSString path)

-- | @- contentsAtPath:@
contentsAtPath :: (IsNSFileManager nsFileManager, IsNSString path) => nsFileManager -> path -> IO (Id NSData)
contentsAtPath nsFileManager path =
  sendMessage nsFileManager contentsAtPathSelector (toNSString path)

-- | @- createFileAtPath:contents:attributes:@
createFileAtPath_contents_attributes :: (IsNSFileManager nsFileManager, IsNSString path, IsNSData data_, IsNSDictionary attr) => nsFileManager -> path -> data_ -> attr -> IO Bool
createFileAtPath_contents_attributes nsFileManager path data_ attr =
  sendMessage nsFileManager createFileAtPath_contents_attributesSelector (toNSString path) (toNSData data_) (toNSDictionary attr)

-- | @- fileSystemRepresentationWithPath:@
fileSystemRepresentationWithPath :: (IsNSFileManager nsFileManager, IsNSString path) => nsFileManager -> path -> IO (Const (Ptr CChar))
fileSystemRepresentationWithPath nsFileManager path =
  sendMessage nsFileManager fileSystemRepresentationWithPathSelector (toNSString path)

-- | @- stringWithFileSystemRepresentation:length:@
stringWithFileSystemRepresentation_length :: IsNSFileManager nsFileManager => nsFileManager -> Const (Ptr CChar) -> CULong -> IO (Id NSString)
stringWithFileSystemRepresentation_length nsFileManager str len =
  sendMessage nsFileManager stringWithFileSystemRepresentation_lengthSelector str len

-- | @- replaceItemAtURL:withItemAtURL:backupItemName:options:resultingItemURL:error:@
replaceItemAtURL_withItemAtURL_backupItemName_options_resultingItemURL_error :: (IsNSFileManager nsFileManager, IsNSURL originalItemURL, IsNSURL newItemURL, IsNSString backupItemName, IsNSURL resultingURL, IsNSError error_) => nsFileManager -> originalItemURL -> newItemURL -> backupItemName -> NSFileManagerItemReplacementOptions -> resultingURL -> error_ -> IO Bool
replaceItemAtURL_withItemAtURL_backupItemName_options_resultingItemURL_error nsFileManager originalItemURL newItemURL backupItemName options resultingURL error_ =
  sendMessage nsFileManager replaceItemAtURL_withItemAtURL_backupItemName_options_resultingItemURL_errorSelector (toNSURL originalItemURL) (toNSURL newItemURL) (toNSString backupItemName) options (toNSURL resultingURL) (toNSError error_)

-- | @- setUbiquitous:itemAtURL:destinationURL:error:@
setUbiquitous_itemAtURL_destinationURL_error :: (IsNSFileManager nsFileManager, IsNSURL url, IsNSURL destinationURL, IsNSError error_) => nsFileManager -> Bool -> url -> destinationURL -> error_ -> IO Bool
setUbiquitous_itemAtURL_destinationURL_error nsFileManager flag url destinationURL error_ =
  sendMessage nsFileManager setUbiquitous_itemAtURL_destinationURL_errorSelector flag (toNSURL url) (toNSURL destinationURL) (toNSError error_)

-- | @- isUbiquitousItemAtURL:@
isUbiquitousItemAtURL :: (IsNSFileManager nsFileManager, IsNSURL url) => nsFileManager -> url -> IO Bool
isUbiquitousItemAtURL nsFileManager url =
  sendMessage nsFileManager isUbiquitousItemAtURLSelector (toNSURL url)

-- | @- startDownloadingUbiquitousItemAtURL:error:@
startDownloadingUbiquitousItemAtURL_error :: (IsNSFileManager nsFileManager, IsNSURL url, IsNSError error_) => nsFileManager -> url -> error_ -> IO Bool
startDownloadingUbiquitousItemAtURL_error nsFileManager url error_ =
  sendMessage nsFileManager startDownloadingUbiquitousItemAtURL_errorSelector (toNSURL url) (toNSError error_)

-- | @- evictUbiquitousItemAtURL:error:@
evictUbiquitousItemAtURL_error :: (IsNSFileManager nsFileManager, IsNSURL url, IsNSError error_) => nsFileManager -> url -> error_ -> IO Bool
evictUbiquitousItemAtURL_error nsFileManager url error_ =
  sendMessage nsFileManager evictUbiquitousItemAtURL_errorSelector (toNSURL url) (toNSError error_)

-- | @- URLForUbiquityContainerIdentifier:@
urlForUbiquityContainerIdentifier :: (IsNSFileManager nsFileManager, IsNSString containerIdentifier) => nsFileManager -> containerIdentifier -> IO (Id NSURL)
urlForUbiquityContainerIdentifier nsFileManager containerIdentifier =
  sendMessage nsFileManager urlForUbiquityContainerIdentifierSelector (toNSString containerIdentifier)

-- | @- URLForPublishingUbiquitousItemAtURL:expirationDate:error:@
urlForPublishingUbiquitousItemAtURL_expirationDate_error :: (IsNSFileManager nsFileManager, IsNSURL url, IsNSDate outDate, IsNSError error_) => nsFileManager -> url -> outDate -> error_ -> IO (Id NSURL)
urlForPublishingUbiquitousItemAtURL_expirationDate_error nsFileManager url outDate error_ =
  sendMessage nsFileManager urlForPublishingUbiquitousItemAtURL_expirationDate_errorSelector (toNSURL url) (toNSDate outDate) (toNSError error_)

-- | Asynchronously pauses sync of an item at the given URL.
--
-- Call this when opening an item to prevent sync from altering the contents of the URL. Once paused, the file provider will not upload local changes nor download remote changes.
--
-- While paused, call ``uploadLocalVersionOfUbiquitousItem(at:withConflictResolutionPolicy:completionHandler:)`` when the document is in a stable state. This action keeps the server version as up-to-date as possible.
--
-- If the item is already paused, a second call to this method reports success. If the file provider is already applying changes to the item, the pause fails with an ``NSFileWriteUnknownError-enum.case``, with an underlying error that has domain ``NSPOSIXErrorDomain`` and code ``POSIXError/EBUSY``. If the pause fails, wait for the state to stabilize before retrying. Pausing also fails with ``CocoaError/featureUnsupported`` if @url@ refers to a regular (non-package) directory.
--
-- Pausing sync is independent of the calling app's lifecycle; sync doesn't automatically resume if the app closes or crashes and relaunches later. To resume syncing, explicitly call ``resumeSyncForUbiquitousItem(at:with:completionHandler:)``. Always be sure to resume syncing before you close the item.
--
-- - Parameters:   - url: The URL of the item for which to pause sync.   - completionHandler: A closure or block that the framework calls when the pause action completes. It receives a single ``NSError`` parameter to indicate an error that prevented pausing; this value is @nil@ if the pause succeeded. In Swift, you can omit the completion handler and catch the thrown error instead.
--
-- ObjC selector: @- pauseSyncForUbiquitousItemAtURL:completionHandler:@
pauseSyncForUbiquitousItemAtURL_completionHandler :: (IsNSFileManager nsFileManager, IsNSURL url) => nsFileManager -> url -> Ptr () -> IO ()
pauseSyncForUbiquitousItemAtURL_completionHandler nsFileManager url completionHandler =
  sendMessage nsFileManager pauseSyncForUbiquitousItemAtURL_completionHandlerSelector (toNSURL url) completionHandler

-- | Asynchronously resumes the sync on a paused item using the given resume behavior.
--
-- Always call this method when your app closes an item to allow the file provider to sync local changes back to the server.
--
-- In most situations, the ``NSFileManagerResumeSyncBehavior/preserveLocalChanges`` behavior is the best choice to avoid any risk of data loss.
--
-- The resume call fails with ``CocoaError/featureUnsupported`` if @url@ isn't currently paused. If the device isn't connected to the network, the call may fail with ``NSFileWriteUnknownError-enum.case``, with the underlying error of <doc://com.apple.documentation/documentation/FileProvider/NSFileProviderError/serverUnreachable>.
--
-- - Parameters:   - url: The URL of the item for which to resume sync.   - behavior: A ``NSFileManagerResumeSyncBehavior`` value that tells the file manager how to handle conflicts between local and remote versions of files.   - completionHandler: A closure or block that the framework calls when the resume action completes. It receives a single ``NSError`` parameter to indicate an error that prevented the resume action; the value is @nil@ if the resume succeeded. In Swift, you can omit the completion handler and catch the thrown error instead.
--
-- ObjC selector: @- resumeSyncForUbiquitousItemAtURL:withBehavior:completionHandler:@
resumeSyncForUbiquitousItemAtURL_withBehavior_completionHandler :: (IsNSFileManager nsFileManager, IsNSURL url) => nsFileManager -> url -> NSFileManagerResumeSyncBehavior -> Ptr () -> IO ()
resumeSyncForUbiquitousItemAtURL_withBehavior_completionHandler nsFileManager url behavior completionHandler =
  sendMessage nsFileManager resumeSyncForUbiquitousItemAtURL_withBehavior_completionHandlerSelector (toNSURL url) behavior completionHandler

-- | Asynchronously fetches the latest remote version of a given item from the server.
--
-- Use this method if uploading fails due to a version conflict and sync is paused. In this case, fetching the latest remote version allows you to inspect the newer item from the server, resolve the conflict, and resume uploading.
--
-- The version provided by this call depends on several factors: * If there is no newer version of the file on the server, the caller receives the current version of the file. * If the server has a newer version and sync isn't paused, this call replaces the local item and provides the version of the new item. * If the server has a newer version but sync is paused, the returned version points to a side location. In this case, call ``NSFileVersion/replaceItem(at:options:)`` on the provided version object to replace the local item with the newer item from the server.
--
-- If the device isn't connected to the network, the call may fail with ``NSFileReadUnknownError-enum.case``, with the underlying error of <doc://com.apple.documentation/documentation/FileProvider/NSFileProviderError/serverUnreachable>.
--
-- - Parameters:   - url: The URL of the item for which to check the version.   - completionHandler: A closure or block that the framework calls when the fetch action completes. It receives parameters of types ``NSFileVersion`` and ``NSError``. The error is @nil@ if fetching the remote version succeeded; otherwise it indicates the error that caused the call to fail. In Swift, you can omit the completion handler, catching any error in a @do@-@catch@ block and receiving the version as the return value.
--
-- ObjC selector: @- fetchLatestRemoteVersionOfItemAtURL:completionHandler:@
fetchLatestRemoteVersionOfItemAtURL_completionHandler :: (IsNSFileManager nsFileManager, IsNSURL url) => nsFileManager -> url -> Ptr () -> IO ()
fetchLatestRemoteVersionOfItemAtURL_completionHandler nsFileManager url completionHandler =
  sendMessage nsFileManager fetchLatestRemoteVersionOfItemAtURL_completionHandlerSelector (toNSURL url) completionHandler

-- | Asynchronously uploads the local version of the item using the provided conflict resolution policy.
--
-- Once your app pauses a sync for an item, call this method every time your document is in a stable state. This action keeps the server version as up-to-date as possible.
--
-- If the server has a newer version than the one to which the app made changes, uploading fails with ``NSFileWriteUnknownError-enum.case``, with an underlying error of <doc://com.apple.documentation/documentation/FileProvider/NSFileProviderError/localVersionConflictingWithServer>. In this case, call ``FileManager/fetchLatestRemoteVersionOfItem(at:completionHandler:)``, rebase local changes on top of that version, and retry the upload.
--
-- If the device isn't connected to the network, the call may fail with ``NSFileWriteUnknownError-enum.case``, with the underlying error of <doc://com.apple.documentation/documentation/FileProvider/NSFileProviderError/serverUnreachable>.
--
-- - Parameters:   - url: The URL of the item for which to check the version.   - conflictResolutionPolicy: The policy the file manager applies if the local and server versions conflict.   - completionHandler: A closure or block that the framework calls when the upload completes. It receives parameters of types ``NSFileVersion`` and ``NSError``. The error is @nil@ if fetching the remote version succeeded; otherwise it indicates the error that caused the call to fail. In Swift, you can omit the completion handler, catching any error in a @do@-@catch@ block and receiving the version as the return value.
--
-- ObjC selector: @- uploadLocalVersionOfUbiquitousItemAtURL:withConflictResolutionPolicy:completionHandler:@
uploadLocalVersionOfUbiquitousItemAtURL_withConflictResolutionPolicy_completionHandler :: (IsNSFileManager nsFileManager, IsNSURL url) => nsFileManager -> url -> NSFileManagerUploadLocalVersionConflictPolicy -> Ptr () -> IO ()
uploadLocalVersionOfUbiquitousItemAtURL_withConflictResolutionPolicy_completionHandler nsFileManager url conflictResolutionPolicy completionHandler =
  sendMessage nsFileManager uploadLocalVersionOfUbiquitousItemAtURL_withConflictResolutionPolicy_completionHandlerSelector (toNSURL url) conflictResolutionPolicy completionHandler

-- | @- containerURLForSecurityApplicationGroupIdentifier:@
containerURLForSecurityApplicationGroupIdentifier :: (IsNSFileManager nsFileManager, IsNSString groupIdentifier) => nsFileManager -> groupIdentifier -> IO (Id NSURL)
containerURLForSecurityApplicationGroupIdentifier nsFileManager groupIdentifier =
  sendMessage nsFileManager containerURLForSecurityApplicationGroupIdentifierSelector (toNSString groupIdentifier)

-- | @- homeDirectoryForUser:@
homeDirectoryForUser :: (IsNSFileManager nsFileManager, IsNSString userName) => nsFileManager -> userName -> IO (Id NSURL)
homeDirectoryForUser nsFileManager userName =
  sendMessage nsFileManager homeDirectoryForUserSelector (toNSString userName)

-- | @+ defaultManager@
defaultManager :: IO (Id NSFileManager)
defaultManager  =
  do
    cls' <- getRequiredClass "NSFileManager"
    sendClassMessage cls' defaultManagerSelector

-- | @- delegate@
delegate :: IsNSFileManager nsFileManager => nsFileManager -> IO RawId
delegate nsFileManager =
  sendMessage nsFileManager delegateSelector

-- | @- setDelegate:@
setDelegate :: IsNSFileManager nsFileManager => nsFileManager -> RawId -> IO ()
setDelegate nsFileManager value =
  sendMessage nsFileManager setDelegateSelector value

-- | @- currentDirectoryPath@
currentDirectoryPath :: IsNSFileManager nsFileManager => nsFileManager -> IO (Id NSString)
currentDirectoryPath nsFileManager =
  sendMessage nsFileManager currentDirectoryPathSelector

-- | @- ubiquityIdentityToken@
ubiquityIdentityToken :: IsNSFileManager nsFileManager => nsFileManager -> IO RawId
ubiquityIdentityToken nsFileManager =
  sendMessage nsFileManager ubiquityIdentityTokenSelector

-- | @- homeDirectoryForCurrentUser@
homeDirectoryForCurrentUser :: IsNSFileManager nsFileManager => nsFileManager -> IO (Id NSURL)
homeDirectoryForCurrentUser nsFileManager =
  sendMessage nsFileManager homeDirectoryForCurrentUserSelector

-- | @- temporaryDirectory@
temporaryDirectory :: IsNSFileManager nsFileManager => nsFileManager -> IO (Id NSURL)
temporaryDirectory nsFileManager =
  sendMessage nsFileManager temporaryDirectorySelector

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @mountedVolumeURLsIncludingResourceValuesForKeys:options:@
mountedVolumeURLsIncludingResourceValuesForKeys_optionsSelector :: Selector '[Id NSArray, NSVolumeEnumerationOptions] (Id NSArray)
mountedVolumeURLsIncludingResourceValuesForKeys_optionsSelector = mkSelector "mountedVolumeURLsIncludingResourceValuesForKeys:options:"

-- | @Selector@ for @unmountVolumeAtURL:options:completionHandler:@
unmountVolumeAtURL_options_completionHandlerSelector :: Selector '[Id NSURL, NSFileManagerUnmountOptions, Ptr ()] ()
unmountVolumeAtURL_options_completionHandlerSelector = mkSelector "unmountVolumeAtURL:options:completionHandler:"

-- | @Selector@ for @contentsOfDirectoryAtURL:includingPropertiesForKeys:options:error:@
contentsOfDirectoryAtURL_includingPropertiesForKeys_options_errorSelector :: Selector '[Id NSURL, Id NSArray, NSDirectoryEnumerationOptions, Id NSError] (Id NSArray)
contentsOfDirectoryAtURL_includingPropertiesForKeys_options_errorSelector = mkSelector "contentsOfDirectoryAtURL:includingPropertiesForKeys:options:error:"

-- | @Selector@ for @URLsForDirectory:inDomains:@
urLsForDirectory_inDomainsSelector :: Selector '[NSSearchPathDirectory, NSSearchPathDomainMask] (Id NSArray)
urLsForDirectory_inDomainsSelector = mkSelector "URLsForDirectory:inDomains:"

-- | @Selector@ for @URLForDirectory:inDomain:appropriateForURL:create:error:@
urlForDirectory_inDomain_appropriateForURL_create_errorSelector :: Selector '[NSSearchPathDirectory, NSSearchPathDomainMask, Id NSURL, Bool, Id NSError] (Id NSURL)
urlForDirectory_inDomain_appropriateForURL_create_errorSelector = mkSelector "URLForDirectory:inDomain:appropriateForURL:create:error:"

-- | @Selector@ for @getRelationship:ofDirectoryAtURL:toItemAtURL:error:@
getRelationship_ofDirectoryAtURL_toItemAtURL_errorSelector :: Selector '[Ptr NSURLRelationship, Id NSURL, Id NSURL, Id NSError] Bool
getRelationship_ofDirectoryAtURL_toItemAtURL_errorSelector = mkSelector "getRelationship:ofDirectoryAtURL:toItemAtURL:error:"

-- | @Selector@ for @getRelationship:ofDirectory:inDomain:toItemAtURL:error:@
getRelationship_ofDirectory_inDomain_toItemAtURL_errorSelector :: Selector '[Ptr NSURLRelationship, NSSearchPathDirectory, NSSearchPathDomainMask, Id NSURL, Id NSError] Bool
getRelationship_ofDirectory_inDomain_toItemAtURL_errorSelector = mkSelector "getRelationship:ofDirectory:inDomain:toItemAtURL:error:"

-- | @Selector@ for @createDirectoryAtURL:withIntermediateDirectories:attributes:error:@
createDirectoryAtURL_withIntermediateDirectories_attributes_errorSelector :: Selector '[Id NSURL, Bool, Id NSDictionary, Id NSError] Bool
createDirectoryAtURL_withIntermediateDirectories_attributes_errorSelector = mkSelector "createDirectoryAtURL:withIntermediateDirectories:attributes:error:"

-- | @Selector@ for @createSymbolicLinkAtURL:withDestinationURL:error:@
createSymbolicLinkAtURL_withDestinationURL_errorSelector :: Selector '[Id NSURL, Id NSURL, Id NSError] Bool
createSymbolicLinkAtURL_withDestinationURL_errorSelector = mkSelector "createSymbolicLinkAtURL:withDestinationURL:error:"

-- | @Selector@ for @setAttributes:ofItemAtPath:error:@
setAttributes_ofItemAtPath_errorSelector :: Selector '[Id NSDictionary, Id NSString, Id NSError] Bool
setAttributes_ofItemAtPath_errorSelector = mkSelector "setAttributes:ofItemAtPath:error:"

-- | @Selector@ for @createDirectoryAtPath:withIntermediateDirectories:attributes:error:@
createDirectoryAtPath_withIntermediateDirectories_attributes_errorSelector :: Selector '[Id NSString, Bool, Id NSDictionary, Id NSError] Bool
createDirectoryAtPath_withIntermediateDirectories_attributes_errorSelector = mkSelector "createDirectoryAtPath:withIntermediateDirectories:attributes:error:"

-- | @Selector@ for @contentsOfDirectoryAtPath:error:@
contentsOfDirectoryAtPath_errorSelector :: Selector '[Id NSString, Id NSError] (Id NSArray)
contentsOfDirectoryAtPath_errorSelector = mkSelector "contentsOfDirectoryAtPath:error:"

-- | @Selector@ for @subpathsOfDirectoryAtPath:error:@
subpathsOfDirectoryAtPath_errorSelector :: Selector '[Id NSString, Id NSError] (Id NSArray)
subpathsOfDirectoryAtPath_errorSelector = mkSelector "subpathsOfDirectoryAtPath:error:"

-- | @Selector@ for @attributesOfItemAtPath:error:@
attributesOfItemAtPath_errorSelector :: Selector '[Id NSString, Id NSError] (Id NSDictionary)
attributesOfItemAtPath_errorSelector = mkSelector "attributesOfItemAtPath:error:"

-- | @Selector@ for @attributesOfFileSystemForPath:error:@
attributesOfFileSystemForPath_errorSelector :: Selector '[Id NSString, Id NSError] (Id NSDictionary)
attributesOfFileSystemForPath_errorSelector = mkSelector "attributesOfFileSystemForPath:error:"

-- | @Selector@ for @createSymbolicLinkAtPath:withDestinationPath:error:@
createSymbolicLinkAtPath_withDestinationPath_errorSelector :: Selector '[Id NSString, Id NSString, Id NSError] Bool
createSymbolicLinkAtPath_withDestinationPath_errorSelector = mkSelector "createSymbolicLinkAtPath:withDestinationPath:error:"

-- | @Selector@ for @destinationOfSymbolicLinkAtPath:error:@
destinationOfSymbolicLinkAtPath_errorSelector :: Selector '[Id NSString, Id NSError] (Id NSString)
destinationOfSymbolicLinkAtPath_errorSelector = mkSelector "destinationOfSymbolicLinkAtPath:error:"

-- | @Selector@ for @copyItemAtPath:toPath:error:@
copyItemAtPath_toPath_errorSelector :: Selector '[Id NSString, Id NSString, Id NSError] Bool
copyItemAtPath_toPath_errorSelector = mkSelector "copyItemAtPath:toPath:error:"

-- | @Selector@ for @moveItemAtPath:toPath:error:@
moveItemAtPath_toPath_errorSelector :: Selector '[Id NSString, Id NSString, Id NSError] Bool
moveItemAtPath_toPath_errorSelector = mkSelector "moveItemAtPath:toPath:error:"

-- | @Selector@ for @linkItemAtPath:toPath:error:@
linkItemAtPath_toPath_errorSelector :: Selector '[Id NSString, Id NSString, Id NSError] Bool
linkItemAtPath_toPath_errorSelector = mkSelector "linkItemAtPath:toPath:error:"

-- | @Selector@ for @removeItemAtPath:error:@
removeItemAtPath_errorSelector :: Selector '[Id NSString, Id NSError] Bool
removeItemAtPath_errorSelector = mkSelector "removeItemAtPath:error:"

-- | @Selector@ for @copyItemAtURL:toURL:error:@
copyItemAtURL_toURL_errorSelector :: Selector '[Id NSURL, Id NSURL, Id NSError] Bool
copyItemAtURL_toURL_errorSelector = mkSelector "copyItemAtURL:toURL:error:"

-- | @Selector@ for @moveItemAtURL:toURL:error:@
moveItemAtURL_toURL_errorSelector :: Selector '[Id NSURL, Id NSURL, Id NSError] Bool
moveItemAtURL_toURL_errorSelector = mkSelector "moveItemAtURL:toURL:error:"

-- | @Selector@ for @linkItemAtURL:toURL:error:@
linkItemAtURL_toURL_errorSelector :: Selector '[Id NSURL, Id NSURL, Id NSError] Bool
linkItemAtURL_toURL_errorSelector = mkSelector "linkItemAtURL:toURL:error:"

-- | @Selector@ for @removeItemAtURL:error:@
removeItemAtURL_errorSelector :: Selector '[Id NSURL, Id NSError] Bool
removeItemAtURL_errorSelector = mkSelector "removeItemAtURL:error:"

-- | @Selector@ for @trashItemAtURL:resultingItemURL:error:@
trashItemAtURL_resultingItemURL_errorSelector :: Selector '[Id NSURL, Id NSURL, Id NSError] Bool
trashItemAtURL_resultingItemURL_errorSelector = mkSelector "trashItemAtURL:resultingItemURL:error:"

-- | @Selector@ for @fileAttributesAtPath:traverseLink:@
fileAttributesAtPath_traverseLinkSelector :: Selector '[Id NSString, Bool] (Id NSDictionary)
fileAttributesAtPath_traverseLinkSelector = mkSelector "fileAttributesAtPath:traverseLink:"

-- | @Selector@ for @changeFileAttributes:atPath:@
changeFileAttributes_atPathSelector :: Selector '[Id NSDictionary, Id NSString] Bool
changeFileAttributes_atPathSelector = mkSelector "changeFileAttributes:atPath:"

-- | @Selector@ for @directoryContentsAtPath:@
directoryContentsAtPathSelector :: Selector '[Id NSString] (Id NSArray)
directoryContentsAtPathSelector = mkSelector "directoryContentsAtPath:"

-- | @Selector@ for @fileSystemAttributesAtPath:@
fileSystemAttributesAtPathSelector :: Selector '[Id NSString] (Id NSDictionary)
fileSystemAttributesAtPathSelector = mkSelector "fileSystemAttributesAtPath:"

-- | @Selector@ for @pathContentOfSymbolicLinkAtPath:@
pathContentOfSymbolicLinkAtPathSelector :: Selector '[Id NSString] (Id NSString)
pathContentOfSymbolicLinkAtPathSelector = mkSelector "pathContentOfSymbolicLinkAtPath:"

-- | @Selector@ for @createSymbolicLinkAtPath:pathContent:@
createSymbolicLinkAtPath_pathContentSelector :: Selector '[Id NSString, Id NSString] Bool
createSymbolicLinkAtPath_pathContentSelector = mkSelector "createSymbolicLinkAtPath:pathContent:"

-- | @Selector@ for @createDirectoryAtPath:attributes:@
createDirectoryAtPath_attributesSelector :: Selector '[Id NSString, Id NSDictionary] Bool
createDirectoryAtPath_attributesSelector = mkSelector "createDirectoryAtPath:attributes:"

-- | @Selector@ for @linkPath:toPath:handler:@
linkPath_toPath_handlerSelector :: Selector '[Id NSString, Id NSString, RawId] Bool
linkPath_toPath_handlerSelector = mkSelector "linkPath:toPath:handler:"

-- | @Selector@ for @copyPath:toPath:handler:@
copyPath_toPath_handlerSelector :: Selector '[Id NSString, Id NSString, RawId] Bool
copyPath_toPath_handlerSelector = mkSelector "copyPath:toPath:handler:"

-- | @Selector@ for @movePath:toPath:handler:@
movePath_toPath_handlerSelector :: Selector '[Id NSString, Id NSString, RawId] Bool
movePath_toPath_handlerSelector = mkSelector "movePath:toPath:handler:"

-- | @Selector@ for @removeFileAtPath:handler:@
removeFileAtPath_handlerSelector :: Selector '[Id NSString, RawId] Bool
removeFileAtPath_handlerSelector = mkSelector "removeFileAtPath:handler:"

-- | @Selector@ for @changeCurrentDirectoryPath:@
changeCurrentDirectoryPathSelector :: Selector '[Id NSString] Bool
changeCurrentDirectoryPathSelector = mkSelector "changeCurrentDirectoryPath:"

-- | @Selector@ for @fileExistsAtPath:@
fileExistsAtPathSelector :: Selector '[Id NSString] Bool
fileExistsAtPathSelector = mkSelector "fileExistsAtPath:"

-- | @Selector@ for @fileExistsAtPath:isDirectory:@
fileExistsAtPath_isDirectorySelector :: Selector '[Id NSString, Ptr Bool] Bool
fileExistsAtPath_isDirectorySelector = mkSelector "fileExistsAtPath:isDirectory:"

-- | @Selector@ for @isReadableFileAtPath:@
isReadableFileAtPathSelector :: Selector '[Id NSString] Bool
isReadableFileAtPathSelector = mkSelector "isReadableFileAtPath:"

-- | @Selector@ for @isWritableFileAtPath:@
isWritableFileAtPathSelector :: Selector '[Id NSString] Bool
isWritableFileAtPathSelector = mkSelector "isWritableFileAtPath:"

-- | @Selector@ for @isExecutableFileAtPath:@
isExecutableFileAtPathSelector :: Selector '[Id NSString] Bool
isExecutableFileAtPathSelector = mkSelector "isExecutableFileAtPath:"

-- | @Selector@ for @isDeletableFileAtPath:@
isDeletableFileAtPathSelector :: Selector '[Id NSString] Bool
isDeletableFileAtPathSelector = mkSelector "isDeletableFileAtPath:"

-- | @Selector@ for @contentsEqualAtPath:andPath:@
contentsEqualAtPath_andPathSelector :: Selector '[Id NSString, Id NSString] Bool
contentsEqualAtPath_andPathSelector = mkSelector "contentsEqualAtPath:andPath:"

-- | @Selector@ for @displayNameAtPath:@
displayNameAtPathSelector :: Selector '[Id NSString] (Id NSString)
displayNameAtPathSelector = mkSelector "displayNameAtPath:"

-- | @Selector@ for @componentsToDisplayForPath:@
componentsToDisplayForPathSelector :: Selector '[Id NSString] (Id NSArray)
componentsToDisplayForPathSelector = mkSelector "componentsToDisplayForPath:"

-- | @Selector@ for @enumeratorAtPath:@
enumeratorAtPathSelector :: Selector '[Id NSString] (Id NSDirectoryEnumerator)
enumeratorAtPathSelector = mkSelector "enumeratorAtPath:"

-- | @Selector@ for @enumeratorAtURL:includingPropertiesForKeys:options:errorHandler:@
enumeratorAtURL_includingPropertiesForKeys_options_errorHandlerSelector :: Selector '[Id NSURL, Id NSArray, NSDirectoryEnumerationOptions, Ptr ()] (Id NSDirectoryEnumerator)
enumeratorAtURL_includingPropertiesForKeys_options_errorHandlerSelector = mkSelector "enumeratorAtURL:includingPropertiesForKeys:options:errorHandler:"

-- | @Selector@ for @subpathsAtPath:@
subpathsAtPathSelector :: Selector '[Id NSString] (Id NSArray)
subpathsAtPathSelector = mkSelector "subpathsAtPath:"

-- | @Selector@ for @contentsAtPath:@
contentsAtPathSelector :: Selector '[Id NSString] (Id NSData)
contentsAtPathSelector = mkSelector "contentsAtPath:"

-- | @Selector@ for @createFileAtPath:contents:attributes:@
createFileAtPath_contents_attributesSelector :: Selector '[Id NSString, Id NSData, Id NSDictionary] Bool
createFileAtPath_contents_attributesSelector = mkSelector "createFileAtPath:contents:attributes:"

-- | @Selector@ for @fileSystemRepresentationWithPath:@
fileSystemRepresentationWithPathSelector :: Selector '[Id NSString] (Const (Ptr CChar))
fileSystemRepresentationWithPathSelector = mkSelector "fileSystemRepresentationWithPath:"

-- | @Selector@ for @stringWithFileSystemRepresentation:length:@
stringWithFileSystemRepresentation_lengthSelector :: Selector '[Const (Ptr CChar), CULong] (Id NSString)
stringWithFileSystemRepresentation_lengthSelector = mkSelector "stringWithFileSystemRepresentation:length:"

-- | @Selector@ for @replaceItemAtURL:withItemAtURL:backupItemName:options:resultingItemURL:error:@
replaceItemAtURL_withItemAtURL_backupItemName_options_resultingItemURL_errorSelector :: Selector '[Id NSURL, Id NSURL, Id NSString, NSFileManagerItemReplacementOptions, Id NSURL, Id NSError] Bool
replaceItemAtURL_withItemAtURL_backupItemName_options_resultingItemURL_errorSelector = mkSelector "replaceItemAtURL:withItemAtURL:backupItemName:options:resultingItemURL:error:"

-- | @Selector@ for @setUbiquitous:itemAtURL:destinationURL:error:@
setUbiquitous_itemAtURL_destinationURL_errorSelector :: Selector '[Bool, Id NSURL, Id NSURL, Id NSError] Bool
setUbiquitous_itemAtURL_destinationURL_errorSelector = mkSelector "setUbiquitous:itemAtURL:destinationURL:error:"

-- | @Selector@ for @isUbiquitousItemAtURL:@
isUbiquitousItemAtURLSelector :: Selector '[Id NSURL] Bool
isUbiquitousItemAtURLSelector = mkSelector "isUbiquitousItemAtURL:"

-- | @Selector@ for @startDownloadingUbiquitousItemAtURL:error:@
startDownloadingUbiquitousItemAtURL_errorSelector :: Selector '[Id NSURL, Id NSError] Bool
startDownloadingUbiquitousItemAtURL_errorSelector = mkSelector "startDownloadingUbiquitousItemAtURL:error:"

-- | @Selector@ for @evictUbiquitousItemAtURL:error:@
evictUbiquitousItemAtURL_errorSelector :: Selector '[Id NSURL, Id NSError] Bool
evictUbiquitousItemAtURL_errorSelector = mkSelector "evictUbiquitousItemAtURL:error:"

-- | @Selector@ for @URLForUbiquityContainerIdentifier:@
urlForUbiquityContainerIdentifierSelector :: Selector '[Id NSString] (Id NSURL)
urlForUbiquityContainerIdentifierSelector = mkSelector "URLForUbiquityContainerIdentifier:"

-- | @Selector@ for @URLForPublishingUbiquitousItemAtURL:expirationDate:error:@
urlForPublishingUbiquitousItemAtURL_expirationDate_errorSelector :: Selector '[Id NSURL, Id NSDate, Id NSError] (Id NSURL)
urlForPublishingUbiquitousItemAtURL_expirationDate_errorSelector = mkSelector "URLForPublishingUbiquitousItemAtURL:expirationDate:error:"

-- | @Selector@ for @pauseSyncForUbiquitousItemAtURL:completionHandler:@
pauseSyncForUbiquitousItemAtURL_completionHandlerSelector :: Selector '[Id NSURL, Ptr ()] ()
pauseSyncForUbiquitousItemAtURL_completionHandlerSelector = mkSelector "pauseSyncForUbiquitousItemAtURL:completionHandler:"

-- | @Selector@ for @resumeSyncForUbiquitousItemAtURL:withBehavior:completionHandler:@
resumeSyncForUbiquitousItemAtURL_withBehavior_completionHandlerSelector :: Selector '[Id NSURL, NSFileManagerResumeSyncBehavior, Ptr ()] ()
resumeSyncForUbiquitousItemAtURL_withBehavior_completionHandlerSelector = mkSelector "resumeSyncForUbiquitousItemAtURL:withBehavior:completionHandler:"

-- | @Selector@ for @fetchLatestRemoteVersionOfItemAtURL:completionHandler:@
fetchLatestRemoteVersionOfItemAtURL_completionHandlerSelector :: Selector '[Id NSURL, Ptr ()] ()
fetchLatestRemoteVersionOfItemAtURL_completionHandlerSelector = mkSelector "fetchLatestRemoteVersionOfItemAtURL:completionHandler:"

-- | @Selector@ for @uploadLocalVersionOfUbiquitousItemAtURL:withConflictResolutionPolicy:completionHandler:@
uploadLocalVersionOfUbiquitousItemAtURL_withConflictResolutionPolicy_completionHandlerSelector :: Selector '[Id NSURL, NSFileManagerUploadLocalVersionConflictPolicy, Ptr ()] ()
uploadLocalVersionOfUbiquitousItemAtURL_withConflictResolutionPolicy_completionHandlerSelector = mkSelector "uploadLocalVersionOfUbiquitousItemAtURL:withConflictResolutionPolicy:completionHandler:"

-- | @Selector@ for @containerURLForSecurityApplicationGroupIdentifier:@
containerURLForSecurityApplicationGroupIdentifierSelector :: Selector '[Id NSString] (Id NSURL)
containerURLForSecurityApplicationGroupIdentifierSelector = mkSelector "containerURLForSecurityApplicationGroupIdentifier:"

-- | @Selector@ for @homeDirectoryForUser:@
homeDirectoryForUserSelector :: Selector '[Id NSString] (Id NSURL)
homeDirectoryForUserSelector = mkSelector "homeDirectoryForUser:"

-- | @Selector@ for @defaultManager@
defaultManagerSelector :: Selector '[] (Id NSFileManager)
defaultManagerSelector = mkSelector "defaultManager"

-- | @Selector@ for @delegate@
delegateSelector :: Selector '[] RawId
delegateSelector = mkSelector "delegate"

-- | @Selector@ for @setDelegate:@
setDelegateSelector :: Selector '[RawId] ()
setDelegateSelector = mkSelector "setDelegate:"

-- | @Selector@ for @currentDirectoryPath@
currentDirectoryPathSelector :: Selector '[] (Id NSString)
currentDirectoryPathSelector = mkSelector "currentDirectoryPath"

-- | @Selector@ for @ubiquityIdentityToken@
ubiquityIdentityTokenSelector :: Selector '[] RawId
ubiquityIdentityTokenSelector = mkSelector "ubiquityIdentityToken"

-- | @Selector@ for @homeDirectoryForCurrentUser@
homeDirectoryForCurrentUserSelector :: Selector '[] (Id NSURL)
homeDirectoryForCurrentUserSelector = mkSelector "homeDirectoryForCurrentUser"

-- | @Selector@ for @temporaryDirectory@
temporaryDirectorySelector :: Selector '[] (Id NSURL)
temporaryDirectorySelector = mkSelector "temporaryDirectory"

