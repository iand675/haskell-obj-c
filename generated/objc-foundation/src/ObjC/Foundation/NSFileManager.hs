{-# LANGUAGE PatternSynonyms #-}
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
  , currentDirectoryPath
  , temporaryDirectory
  , mountedVolumeURLsIncludingResourceValuesForKeys_optionsSelector
  , unmountVolumeAtURL_options_completionHandlerSelector
  , contentsOfDirectoryAtURL_includingPropertiesForKeys_options_errorSelector
  , urLsForDirectory_inDomainsSelector
  , urlForDirectory_inDomain_appropriateForURL_create_errorSelector
  , getRelationship_ofDirectoryAtURL_toItemAtURL_errorSelector
  , getRelationship_ofDirectory_inDomain_toItemAtURL_errorSelector
  , createDirectoryAtURL_withIntermediateDirectories_attributes_errorSelector
  , createSymbolicLinkAtURL_withDestinationURL_errorSelector
  , setAttributes_ofItemAtPath_errorSelector
  , createDirectoryAtPath_withIntermediateDirectories_attributes_errorSelector
  , contentsOfDirectoryAtPath_errorSelector
  , subpathsOfDirectoryAtPath_errorSelector
  , attributesOfItemAtPath_errorSelector
  , attributesOfFileSystemForPath_errorSelector
  , createSymbolicLinkAtPath_withDestinationPath_errorSelector
  , destinationOfSymbolicLinkAtPath_errorSelector
  , copyItemAtPath_toPath_errorSelector
  , moveItemAtPath_toPath_errorSelector
  , linkItemAtPath_toPath_errorSelector
  , removeItemAtPath_errorSelector
  , copyItemAtURL_toURL_errorSelector
  , moveItemAtURL_toURL_errorSelector
  , linkItemAtURL_toURL_errorSelector
  , removeItemAtURL_errorSelector
  , trashItemAtURL_resultingItemURL_errorSelector
  , fileAttributesAtPath_traverseLinkSelector
  , changeFileAttributes_atPathSelector
  , directoryContentsAtPathSelector
  , fileSystemAttributesAtPathSelector
  , pathContentOfSymbolicLinkAtPathSelector
  , createSymbolicLinkAtPath_pathContentSelector
  , createDirectoryAtPath_attributesSelector
  , linkPath_toPath_handlerSelector
  , copyPath_toPath_handlerSelector
  , movePath_toPath_handlerSelector
  , removeFileAtPath_handlerSelector
  , changeCurrentDirectoryPathSelector
  , fileExistsAtPathSelector
  , fileExistsAtPath_isDirectorySelector
  , isReadableFileAtPathSelector
  , isWritableFileAtPathSelector
  , isExecutableFileAtPathSelector
  , isDeletableFileAtPathSelector
  , contentsEqualAtPath_andPathSelector
  , displayNameAtPathSelector
  , componentsToDisplayForPathSelector
  , enumeratorAtPathSelector
  , enumeratorAtURL_includingPropertiesForKeys_options_errorHandlerSelector
  , subpathsAtPathSelector
  , contentsAtPathSelector
  , createFileAtPath_contents_attributesSelector
  , fileSystemRepresentationWithPathSelector
  , stringWithFileSystemRepresentation_lengthSelector
  , replaceItemAtURL_withItemAtURL_backupItemName_options_resultingItemURL_errorSelector
  , setUbiquitous_itemAtURL_destinationURL_errorSelector
  , isUbiquitousItemAtURLSelector
  , startDownloadingUbiquitousItemAtURL_errorSelector
  , evictUbiquitousItemAtURL_errorSelector
  , urlForUbiquityContainerIdentifierSelector
  , urlForPublishingUbiquitousItemAtURL_expirationDate_errorSelector
  , pauseSyncForUbiquitousItemAtURL_completionHandlerSelector
  , resumeSyncForUbiquitousItemAtURL_withBehavior_completionHandlerSelector
  , fetchLatestRemoteVersionOfItemAtURL_completionHandlerSelector
  , uploadLocalVersionOfUbiquitousItemAtURL_withConflictResolutionPolicy_completionHandlerSelector
  , containerURLForSecurityApplicationGroupIdentifierSelector
  , homeDirectoryForUserSelector
  , defaultManagerSelector
  , currentDirectoryPathSelector
  , temporaryDirectorySelector

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

import ObjC.Foundation.Internal.Classes
import ObjC.Foundation.Internal.Enums

-- | @- mountedVolumeURLsIncludingResourceValuesForKeys:options:@
mountedVolumeURLsIncludingResourceValuesForKeys_options :: (IsNSFileManager nsFileManager, IsNSArray propertyKeys) => nsFileManager -> propertyKeys -> NSVolumeEnumerationOptions -> IO (Id NSArray)
mountedVolumeURLsIncludingResourceValuesForKeys_options nsFileManager  propertyKeys options =
withObjCPtr propertyKeys $ \raw_propertyKeys ->
    sendMsg nsFileManager (mkSelector "mountedVolumeURLsIncludingResourceValuesForKeys:options:") (retPtr retVoid) [argPtr (castPtr raw_propertyKeys :: Ptr ()), argCULong (coerce options)] >>= retainedObject . castPtr

-- | @- unmountVolumeAtURL:options:completionHandler:@
unmountVolumeAtURL_options_completionHandler :: (IsNSFileManager nsFileManager, IsNSURL url) => nsFileManager -> url -> NSFileManagerUnmountOptions -> Ptr () -> IO ()
unmountVolumeAtURL_options_completionHandler nsFileManager  url mask completionHandler =
withObjCPtr url $ \raw_url ->
    sendMsg nsFileManager (mkSelector "unmountVolumeAtURL:options:completionHandler:") retVoid [argPtr (castPtr raw_url :: Ptr ()), argCULong (coerce mask), argPtr (castPtr completionHandler :: Ptr ())]

-- | @- contentsOfDirectoryAtURL:includingPropertiesForKeys:options:error:@
contentsOfDirectoryAtURL_includingPropertiesForKeys_options_error :: (IsNSFileManager nsFileManager, IsNSURL url, IsNSArray keys, IsNSError error_) => nsFileManager -> url -> keys -> NSDirectoryEnumerationOptions -> error_ -> IO (Id NSArray)
contentsOfDirectoryAtURL_includingPropertiesForKeys_options_error nsFileManager  url keys mask error_ =
withObjCPtr url $ \raw_url ->
  withObjCPtr keys $ \raw_keys ->
    withObjCPtr error_ $ \raw_error_ ->
        sendMsg nsFileManager (mkSelector "contentsOfDirectoryAtURL:includingPropertiesForKeys:options:error:") (retPtr retVoid) [argPtr (castPtr raw_url :: Ptr ()), argPtr (castPtr raw_keys :: Ptr ()), argCULong (coerce mask), argPtr (castPtr raw_error_ :: Ptr ())] >>= retainedObject . castPtr

-- | @- URLsForDirectory:inDomains:@
urLsForDirectory_inDomains :: IsNSFileManager nsFileManager => nsFileManager -> NSSearchPathDirectory -> NSSearchPathDomainMask -> IO (Id NSArray)
urLsForDirectory_inDomains nsFileManager  directory domainMask =
  sendMsg nsFileManager (mkSelector "URLsForDirectory:inDomains:") (retPtr retVoid) [argCULong (coerce directory), argCULong (coerce domainMask)] >>= retainedObject . castPtr

-- | @- URLForDirectory:inDomain:appropriateForURL:create:error:@
urlForDirectory_inDomain_appropriateForURL_create_error :: (IsNSFileManager nsFileManager, IsNSURL url, IsNSError error_) => nsFileManager -> NSSearchPathDirectory -> NSSearchPathDomainMask -> url -> Bool -> error_ -> IO (Id NSURL)
urlForDirectory_inDomain_appropriateForURL_create_error nsFileManager  directory domain url shouldCreate error_ =
withObjCPtr url $ \raw_url ->
  withObjCPtr error_ $ \raw_error_ ->
      sendMsg nsFileManager (mkSelector "URLForDirectory:inDomain:appropriateForURL:create:error:") (retPtr retVoid) [argCULong (coerce directory), argCULong (coerce domain), argPtr (castPtr raw_url :: Ptr ()), argCULong (if shouldCreate then 1 else 0), argPtr (castPtr raw_error_ :: Ptr ())] >>= retainedObject . castPtr

-- | @- getRelationship:ofDirectoryAtURL:toItemAtURL:error:@
getRelationship_ofDirectoryAtURL_toItemAtURL_error :: (IsNSFileManager nsFileManager, IsNSURL directoryURL, IsNSURL otherURL, IsNSError error_) => nsFileManager -> Ptr NSURLRelationship -> directoryURL -> otherURL -> error_ -> IO Bool
getRelationship_ofDirectoryAtURL_toItemAtURL_error nsFileManager  outRelationship directoryURL otherURL error_ =
withObjCPtr directoryURL $ \raw_directoryURL ->
  withObjCPtr otherURL $ \raw_otherURL ->
    withObjCPtr error_ $ \raw_error_ ->
        fmap ((/= 0) :: CULong -> Bool) $ sendMsg nsFileManager (mkSelector "getRelationship:ofDirectoryAtURL:toItemAtURL:error:") retCULong [argPtr outRelationship, argPtr (castPtr raw_directoryURL :: Ptr ()), argPtr (castPtr raw_otherURL :: Ptr ()), argPtr (castPtr raw_error_ :: Ptr ())]

-- | @- getRelationship:ofDirectory:inDomain:toItemAtURL:error:@
getRelationship_ofDirectory_inDomain_toItemAtURL_error :: (IsNSFileManager nsFileManager, IsNSURL url, IsNSError error_) => nsFileManager -> Ptr NSURLRelationship -> NSSearchPathDirectory -> NSSearchPathDomainMask -> url -> error_ -> IO Bool
getRelationship_ofDirectory_inDomain_toItemAtURL_error nsFileManager  outRelationship directory domainMask url error_ =
withObjCPtr url $ \raw_url ->
  withObjCPtr error_ $ \raw_error_ ->
      fmap ((/= 0) :: CULong -> Bool) $ sendMsg nsFileManager (mkSelector "getRelationship:ofDirectory:inDomain:toItemAtURL:error:") retCULong [argPtr outRelationship, argCULong (coerce directory), argCULong (coerce domainMask), argPtr (castPtr raw_url :: Ptr ()), argPtr (castPtr raw_error_ :: Ptr ())]

-- | @- createDirectoryAtURL:withIntermediateDirectories:attributes:error:@
createDirectoryAtURL_withIntermediateDirectories_attributes_error :: (IsNSFileManager nsFileManager, IsNSURL url, IsNSDictionary attributes, IsNSError error_) => nsFileManager -> url -> Bool -> attributes -> error_ -> IO Bool
createDirectoryAtURL_withIntermediateDirectories_attributes_error nsFileManager  url createIntermediates attributes error_ =
withObjCPtr url $ \raw_url ->
  withObjCPtr attributes $ \raw_attributes ->
    withObjCPtr error_ $ \raw_error_ ->
        fmap ((/= 0) :: CULong -> Bool) $ sendMsg nsFileManager (mkSelector "createDirectoryAtURL:withIntermediateDirectories:attributes:error:") retCULong [argPtr (castPtr raw_url :: Ptr ()), argCULong (if createIntermediates then 1 else 0), argPtr (castPtr raw_attributes :: Ptr ()), argPtr (castPtr raw_error_ :: Ptr ())]

-- | @- createSymbolicLinkAtURL:withDestinationURL:error:@
createSymbolicLinkAtURL_withDestinationURL_error :: (IsNSFileManager nsFileManager, IsNSURL url, IsNSURL destURL, IsNSError error_) => nsFileManager -> url -> destURL -> error_ -> IO Bool
createSymbolicLinkAtURL_withDestinationURL_error nsFileManager  url destURL error_ =
withObjCPtr url $ \raw_url ->
  withObjCPtr destURL $ \raw_destURL ->
    withObjCPtr error_ $ \raw_error_ ->
        fmap ((/= 0) :: CULong -> Bool) $ sendMsg nsFileManager (mkSelector "createSymbolicLinkAtURL:withDestinationURL:error:") retCULong [argPtr (castPtr raw_url :: Ptr ()), argPtr (castPtr raw_destURL :: Ptr ()), argPtr (castPtr raw_error_ :: Ptr ())]

-- | @- setAttributes:ofItemAtPath:error:@
setAttributes_ofItemAtPath_error :: (IsNSFileManager nsFileManager, IsNSDictionary attributes, IsNSString path, IsNSError error_) => nsFileManager -> attributes -> path -> error_ -> IO Bool
setAttributes_ofItemAtPath_error nsFileManager  attributes path error_ =
withObjCPtr attributes $ \raw_attributes ->
  withObjCPtr path $ \raw_path ->
    withObjCPtr error_ $ \raw_error_ ->
        fmap ((/= 0) :: CULong -> Bool) $ sendMsg nsFileManager (mkSelector "setAttributes:ofItemAtPath:error:") retCULong [argPtr (castPtr raw_attributes :: Ptr ()), argPtr (castPtr raw_path :: Ptr ()), argPtr (castPtr raw_error_ :: Ptr ())]

-- | @- createDirectoryAtPath:withIntermediateDirectories:attributes:error:@
createDirectoryAtPath_withIntermediateDirectories_attributes_error :: (IsNSFileManager nsFileManager, IsNSString path, IsNSDictionary attributes, IsNSError error_) => nsFileManager -> path -> Bool -> attributes -> error_ -> IO Bool
createDirectoryAtPath_withIntermediateDirectories_attributes_error nsFileManager  path createIntermediates attributes error_ =
withObjCPtr path $ \raw_path ->
  withObjCPtr attributes $ \raw_attributes ->
    withObjCPtr error_ $ \raw_error_ ->
        fmap ((/= 0) :: CULong -> Bool) $ sendMsg nsFileManager (mkSelector "createDirectoryAtPath:withIntermediateDirectories:attributes:error:") retCULong [argPtr (castPtr raw_path :: Ptr ()), argCULong (if createIntermediates then 1 else 0), argPtr (castPtr raw_attributes :: Ptr ()), argPtr (castPtr raw_error_ :: Ptr ())]

-- | @- contentsOfDirectoryAtPath:error:@
contentsOfDirectoryAtPath_error :: (IsNSFileManager nsFileManager, IsNSString path, IsNSError error_) => nsFileManager -> path -> error_ -> IO (Id NSArray)
contentsOfDirectoryAtPath_error nsFileManager  path error_ =
withObjCPtr path $ \raw_path ->
  withObjCPtr error_ $ \raw_error_ ->
      sendMsg nsFileManager (mkSelector "contentsOfDirectoryAtPath:error:") (retPtr retVoid) [argPtr (castPtr raw_path :: Ptr ()), argPtr (castPtr raw_error_ :: Ptr ())] >>= retainedObject . castPtr

-- | @- subpathsOfDirectoryAtPath:error:@
subpathsOfDirectoryAtPath_error :: (IsNSFileManager nsFileManager, IsNSString path, IsNSError error_) => nsFileManager -> path -> error_ -> IO (Id NSArray)
subpathsOfDirectoryAtPath_error nsFileManager  path error_ =
withObjCPtr path $ \raw_path ->
  withObjCPtr error_ $ \raw_error_ ->
      sendMsg nsFileManager (mkSelector "subpathsOfDirectoryAtPath:error:") (retPtr retVoid) [argPtr (castPtr raw_path :: Ptr ()), argPtr (castPtr raw_error_ :: Ptr ())] >>= retainedObject . castPtr

-- | @- attributesOfItemAtPath:error:@
attributesOfItemAtPath_error :: (IsNSFileManager nsFileManager, IsNSString path, IsNSError error_) => nsFileManager -> path -> error_ -> IO (Id NSDictionary)
attributesOfItemAtPath_error nsFileManager  path error_ =
withObjCPtr path $ \raw_path ->
  withObjCPtr error_ $ \raw_error_ ->
      sendMsg nsFileManager (mkSelector "attributesOfItemAtPath:error:") (retPtr retVoid) [argPtr (castPtr raw_path :: Ptr ()), argPtr (castPtr raw_error_ :: Ptr ())] >>= retainedObject . castPtr

-- | @- attributesOfFileSystemForPath:error:@
attributesOfFileSystemForPath_error :: (IsNSFileManager nsFileManager, IsNSString path, IsNSError error_) => nsFileManager -> path -> error_ -> IO (Id NSDictionary)
attributesOfFileSystemForPath_error nsFileManager  path error_ =
withObjCPtr path $ \raw_path ->
  withObjCPtr error_ $ \raw_error_ ->
      sendMsg nsFileManager (mkSelector "attributesOfFileSystemForPath:error:") (retPtr retVoid) [argPtr (castPtr raw_path :: Ptr ()), argPtr (castPtr raw_error_ :: Ptr ())] >>= retainedObject . castPtr

-- | @- createSymbolicLinkAtPath:withDestinationPath:error:@
createSymbolicLinkAtPath_withDestinationPath_error :: (IsNSFileManager nsFileManager, IsNSString path, IsNSString destPath, IsNSError error_) => nsFileManager -> path -> destPath -> error_ -> IO Bool
createSymbolicLinkAtPath_withDestinationPath_error nsFileManager  path destPath error_ =
withObjCPtr path $ \raw_path ->
  withObjCPtr destPath $ \raw_destPath ->
    withObjCPtr error_ $ \raw_error_ ->
        fmap ((/= 0) :: CULong -> Bool) $ sendMsg nsFileManager (mkSelector "createSymbolicLinkAtPath:withDestinationPath:error:") retCULong [argPtr (castPtr raw_path :: Ptr ()), argPtr (castPtr raw_destPath :: Ptr ()), argPtr (castPtr raw_error_ :: Ptr ())]

-- | @- destinationOfSymbolicLinkAtPath:error:@
destinationOfSymbolicLinkAtPath_error :: (IsNSFileManager nsFileManager, IsNSString path, IsNSError error_) => nsFileManager -> path -> error_ -> IO (Id NSString)
destinationOfSymbolicLinkAtPath_error nsFileManager  path error_ =
withObjCPtr path $ \raw_path ->
  withObjCPtr error_ $ \raw_error_ ->
      sendMsg nsFileManager (mkSelector "destinationOfSymbolicLinkAtPath:error:") (retPtr retVoid) [argPtr (castPtr raw_path :: Ptr ()), argPtr (castPtr raw_error_ :: Ptr ())] >>= retainedObject . castPtr

-- | @- copyItemAtPath:toPath:error:@
copyItemAtPath_toPath_error :: (IsNSFileManager nsFileManager, IsNSString srcPath, IsNSString dstPath, IsNSError error_) => nsFileManager -> srcPath -> dstPath -> error_ -> IO Bool
copyItemAtPath_toPath_error nsFileManager  srcPath dstPath error_ =
withObjCPtr srcPath $ \raw_srcPath ->
  withObjCPtr dstPath $ \raw_dstPath ->
    withObjCPtr error_ $ \raw_error_ ->
        fmap ((/= 0) :: CULong -> Bool) $ sendMsg nsFileManager (mkSelector "copyItemAtPath:toPath:error:") retCULong [argPtr (castPtr raw_srcPath :: Ptr ()), argPtr (castPtr raw_dstPath :: Ptr ()), argPtr (castPtr raw_error_ :: Ptr ())]

-- | @- moveItemAtPath:toPath:error:@
moveItemAtPath_toPath_error :: (IsNSFileManager nsFileManager, IsNSString srcPath, IsNSString dstPath, IsNSError error_) => nsFileManager -> srcPath -> dstPath -> error_ -> IO Bool
moveItemAtPath_toPath_error nsFileManager  srcPath dstPath error_ =
withObjCPtr srcPath $ \raw_srcPath ->
  withObjCPtr dstPath $ \raw_dstPath ->
    withObjCPtr error_ $ \raw_error_ ->
        fmap ((/= 0) :: CULong -> Bool) $ sendMsg nsFileManager (mkSelector "moveItemAtPath:toPath:error:") retCULong [argPtr (castPtr raw_srcPath :: Ptr ()), argPtr (castPtr raw_dstPath :: Ptr ()), argPtr (castPtr raw_error_ :: Ptr ())]

-- | @- linkItemAtPath:toPath:error:@
linkItemAtPath_toPath_error :: (IsNSFileManager nsFileManager, IsNSString srcPath, IsNSString dstPath, IsNSError error_) => nsFileManager -> srcPath -> dstPath -> error_ -> IO Bool
linkItemAtPath_toPath_error nsFileManager  srcPath dstPath error_ =
withObjCPtr srcPath $ \raw_srcPath ->
  withObjCPtr dstPath $ \raw_dstPath ->
    withObjCPtr error_ $ \raw_error_ ->
        fmap ((/= 0) :: CULong -> Bool) $ sendMsg nsFileManager (mkSelector "linkItemAtPath:toPath:error:") retCULong [argPtr (castPtr raw_srcPath :: Ptr ()), argPtr (castPtr raw_dstPath :: Ptr ()), argPtr (castPtr raw_error_ :: Ptr ())]

-- | @- removeItemAtPath:error:@
removeItemAtPath_error :: (IsNSFileManager nsFileManager, IsNSString path, IsNSError error_) => nsFileManager -> path -> error_ -> IO Bool
removeItemAtPath_error nsFileManager  path error_ =
withObjCPtr path $ \raw_path ->
  withObjCPtr error_ $ \raw_error_ ->
      fmap ((/= 0) :: CULong -> Bool) $ sendMsg nsFileManager (mkSelector "removeItemAtPath:error:") retCULong [argPtr (castPtr raw_path :: Ptr ()), argPtr (castPtr raw_error_ :: Ptr ())]

-- | @- copyItemAtURL:toURL:error:@
copyItemAtURL_toURL_error :: (IsNSFileManager nsFileManager, IsNSURL srcURL, IsNSURL dstURL, IsNSError error_) => nsFileManager -> srcURL -> dstURL -> error_ -> IO Bool
copyItemAtURL_toURL_error nsFileManager  srcURL dstURL error_ =
withObjCPtr srcURL $ \raw_srcURL ->
  withObjCPtr dstURL $ \raw_dstURL ->
    withObjCPtr error_ $ \raw_error_ ->
        fmap ((/= 0) :: CULong -> Bool) $ sendMsg nsFileManager (mkSelector "copyItemAtURL:toURL:error:") retCULong [argPtr (castPtr raw_srcURL :: Ptr ()), argPtr (castPtr raw_dstURL :: Ptr ()), argPtr (castPtr raw_error_ :: Ptr ())]

-- | @- moveItemAtURL:toURL:error:@
moveItemAtURL_toURL_error :: (IsNSFileManager nsFileManager, IsNSURL srcURL, IsNSURL dstURL, IsNSError error_) => nsFileManager -> srcURL -> dstURL -> error_ -> IO Bool
moveItemAtURL_toURL_error nsFileManager  srcURL dstURL error_ =
withObjCPtr srcURL $ \raw_srcURL ->
  withObjCPtr dstURL $ \raw_dstURL ->
    withObjCPtr error_ $ \raw_error_ ->
        fmap ((/= 0) :: CULong -> Bool) $ sendMsg nsFileManager (mkSelector "moveItemAtURL:toURL:error:") retCULong [argPtr (castPtr raw_srcURL :: Ptr ()), argPtr (castPtr raw_dstURL :: Ptr ()), argPtr (castPtr raw_error_ :: Ptr ())]

-- | @- linkItemAtURL:toURL:error:@
linkItemAtURL_toURL_error :: (IsNSFileManager nsFileManager, IsNSURL srcURL, IsNSURL dstURL, IsNSError error_) => nsFileManager -> srcURL -> dstURL -> error_ -> IO Bool
linkItemAtURL_toURL_error nsFileManager  srcURL dstURL error_ =
withObjCPtr srcURL $ \raw_srcURL ->
  withObjCPtr dstURL $ \raw_dstURL ->
    withObjCPtr error_ $ \raw_error_ ->
        fmap ((/= 0) :: CULong -> Bool) $ sendMsg nsFileManager (mkSelector "linkItemAtURL:toURL:error:") retCULong [argPtr (castPtr raw_srcURL :: Ptr ()), argPtr (castPtr raw_dstURL :: Ptr ()), argPtr (castPtr raw_error_ :: Ptr ())]

-- | @- removeItemAtURL:error:@
removeItemAtURL_error :: (IsNSFileManager nsFileManager, IsNSURL url, IsNSError error_) => nsFileManager -> url -> error_ -> IO Bool
removeItemAtURL_error nsFileManager  url error_ =
withObjCPtr url $ \raw_url ->
  withObjCPtr error_ $ \raw_error_ ->
      fmap ((/= 0) :: CULong -> Bool) $ sendMsg nsFileManager (mkSelector "removeItemAtURL:error:") retCULong [argPtr (castPtr raw_url :: Ptr ()), argPtr (castPtr raw_error_ :: Ptr ())]

-- | @- trashItemAtURL:resultingItemURL:error:@
trashItemAtURL_resultingItemURL_error :: (IsNSFileManager nsFileManager, IsNSURL url, IsNSURL outResultingURL, IsNSError error_) => nsFileManager -> url -> outResultingURL -> error_ -> IO Bool
trashItemAtURL_resultingItemURL_error nsFileManager  url outResultingURL error_ =
withObjCPtr url $ \raw_url ->
  withObjCPtr outResultingURL $ \raw_outResultingURL ->
    withObjCPtr error_ $ \raw_error_ ->
        fmap ((/= 0) :: CULong -> Bool) $ sendMsg nsFileManager (mkSelector "trashItemAtURL:resultingItemURL:error:") retCULong [argPtr (castPtr raw_url :: Ptr ()), argPtr (castPtr raw_outResultingURL :: Ptr ()), argPtr (castPtr raw_error_ :: Ptr ())]

-- | @- fileAttributesAtPath:traverseLink:@
fileAttributesAtPath_traverseLink :: (IsNSFileManager nsFileManager, IsNSString path) => nsFileManager -> path -> Bool -> IO (Id NSDictionary)
fileAttributesAtPath_traverseLink nsFileManager  path yorn =
withObjCPtr path $ \raw_path ->
    sendMsg nsFileManager (mkSelector "fileAttributesAtPath:traverseLink:") (retPtr retVoid) [argPtr (castPtr raw_path :: Ptr ()), argCULong (if yorn then 1 else 0)] >>= retainedObject . castPtr

-- | @- changeFileAttributes:atPath:@
changeFileAttributes_atPath :: (IsNSFileManager nsFileManager, IsNSDictionary attributes, IsNSString path) => nsFileManager -> attributes -> path -> IO Bool
changeFileAttributes_atPath nsFileManager  attributes path =
withObjCPtr attributes $ \raw_attributes ->
  withObjCPtr path $ \raw_path ->
      fmap ((/= 0) :: CULong -> Bool) $ sendMsg nsFileManager (mkSelector "changeFileAttributes:atPath:") retCULong [argPtr (castPtr raw_attributes :: Ptr ()), argPtr (castPtr raw_path :: Ptr ())]

-- | @- directoryContentsAtPath:@
directoryContentsAtPath :: (IsNSFileManager nsFileManager, IsNSString path) => nsFileManager -> path -> IO (Id NSArray)
directoryContentsAtPath nsFileManager  path =
withObjCPtr path $ \raw_path ->
    sendMsg nsFileManager (mkSelector "directoryContentsAtPath:") (retPtr retVoid) [argPtr (castPtr raw_path :: Ptr ())] >>= retainedObject . castPtr

-- | @- fileSystemAttributesAtPath:@
fileSystemAttributesAtPath :: (IsNSFileManager nsFileManager, IsNSString path) => nsFileManager -> path -> IO (Id NSDictionary)
fileSystemAttributesAtPath nsFileManager  path =
withObjCPtr path $ \raw_path ->
    sendMsg nsFileManager (mkSelector "fileSystemAttributesAtPath:") (retPtr retVoid) [argPtr (castPtr raw_path :: Ptr ())] >>= retainedObject . castPtr

-- | @- pathContentOfSymbolicLinkAtPath:@
pathContentOfSymbolicLinkAtPath :: (IsNSFileManager nsFileManager, IsNSString path) => nsFileManager -> path -> IO (Id NSString)
pathContentOfSymbolicLinkAtPath nsFileManager  path =
withObjCPtr path $ \raw_path ->
    sendMsg nsFileManager (mkSelector "pathContentOfSymbolicLinkAtPath:") (retPtr retVoid) [argPtr (castPtr raw_path :: Ptr ())] >>= retainedObject . castPtr

-- | @- createSymbolicLinkAtPath:pathContent:@
createSymbolicLinkAtPath_pathContent :: (IsNSFileManager nsFileManager, IsNSString path, IsNSString otherpath) => nsFileManager -> path -> otherpath -> IO Bool
createSymbolicLinkAtPath_pathContent nsFileManager  path otherpath =
withObjCPtr path $ \raw_path ->
  withObjCPtr otherpath $ \raw_otherpath ->
      fmap ((/= 0) :: CULong -> Bool) $ sendMsg nsFileManager (mkSelector "createSymbolicLinkAtPath:pathContent:") retCULong [argPtr (castPtr raw_path :: Ptr ()), argPtr (castPtr raw_otherpath :: Ptr ())]

-- | @- createDirectoryAtPath:attributes:@
createDirectoryAtPath_attributes :: (IsNSFileManager nsFileManager, IsNSString path, IsNSDictionary attributes) => nsFileManager -> path -> attributes -> IO Bool
createDirectoryAtPath_attributes nsFileManager  path attributes =
withObjCPtr path $ \raw_path ->
  withObjCPtr attributes $ \raw_attributes ->
      fmap ((/= 0) :: CULong -> Bool) $ sendMsg nsFileManager (mkSelector "createDirectoryAtPath:attributes:") retCULong [argPtr (castPtr raw_path :: Ptr ()), argPtr (castPtr raw_attributes :: Ptr ())]

-- | @- linkPath:toPath:handler:@
linkPath_toPath_handler :: (IsNSFileManager nsFileManager, IsNSString src, IsNSString dest) => nsFileManager -> src -> dest -> RawId -> IO Bool
linkPath_toPath_handler nsFileManager  src dest handler =
withObjCPtr src $ \raw_src ->
  withObjCPtr dest $ \raw_dest ->
      fmap ((/= 0) :: CULong -> Bool) $ sendMsg nsFileManager (mkSelector "linkPath:toPath:handler:") retCULong [argPtr (castPtr raw_src :: Ptr ()), argPtr (castPtr raw_dest :: Ptr ()), argPtr (castPtr (unRawId handler) :: Ptr ())]

-- | @- copyPath:toPath:handler:@
copyPath_toPath_handler :: (IsNSFileManager nsFileManager, IsNSString src, IsNSString dest) => nsFileManager -> src -> dest -> RawId -> IO Bool
copyPath_toPath_handler nsFileManager  src dest handler =
withObjCPtr src $ \raw_src ->
  withObjCPtr dest $ \raw_dest ->
      fmap ((/= 0) :: CULong -> Bool) $ sendMsg nsFileManager (mkSelector "copyPath:toPath:handler:") retCULong [argPtr (castPtr raw_src :: Ptr ()), argPtr (castPtr raw_dest :: Ptr ()), argPtr (castPtr (unRawId handler) :: Ptr ())]

-- | @- movePath:toPath:handler:@
movePath_toPath_handler :: (IsNSFileManager nsFileManager, IsNSString src, IsNSString dest) => nsFileManager -> src -> dest -> RawId -> IO Bool
movePath_toPath_handler nsFileManager  src dest handler =
withObjCPtr src $ \raw_src ->
  withObjCPtr dest $ \raw_dest ->
      fmap ((/= 0) :: CULong -> Bool) $ sendMsg nsFileManager (mkSelector "movePath:toPath:handler:") retCULong [argPtr (castPtr raw_src :: Ptr ()), argPtr (castPtr raw_dest :: Ptr ()), argPtr (castPtr (unRawId handler) :: Ptr ())]

-- | @- removeFileAtPath:handler:@
removeFileAtPath_handler :: (IsNSFileManager nsFileManager, IsNSString path) => nsFileManager -> path -> RawId -> IO Bool
removeFileAtPath_handler nsFileManager  path handler =
withObjCPtr path $ \raw_path ->
    fmap ((/= 0) :: CULong -> Bool) $ sendMsg nsFileManager (mkSelector "removeFileAtPath:handler:") retCULong [argPtr (castPtr raw_path :: Ptr ()), argPtr (castPtr (unRawId handler) :: Ptr ())]

-- | @- changeCurrentDirectoryPath:@
changeCurrentDirectoryPath :: (IsNSFileManager nsFileManager, IsNSString path) => nsFileManager -> path -> IO Bool
changeCurrentDirectoryPath nsFileManager  path =
withObjCPtr path $ \raw_path ->
    fmap ((/= 0) :: CULong -> Bool) $ sendMsg nsFileManager (mkSelector "changeCurrentDirectoryPath:") retCULong [argPtr (castPtr raw_path :: Ptr ())]

-- | @- fileExistsAtPath:@
fileExistsAtPath :: (IsNSFileManager nsFileManager, IsNSString path) => nsFileManager -> path -> IO Bool
fileExistsAtPath nsFileManager  path =
withObjCPtr path $ \raw_path ->
    fmap ((/= 0) :: CULong -> Bool) $ sendMsg nsFileManager (mkSelector "fileExistsAtPath:") retCULong [argPtr (castPtr raw_path :: Ptr ())]

-- | @- fileExistsAtPath:isDirectory:@
fileExistsAtPath_isDirectory :: (IsNSFileManager nsFileManager, IsNSString path) => nsFileManager -> path -> Ptr Bool -> IO Bool
fileExistsAtPath_isDirectory nsFileManager  path isDirectory =
withObjCPtr path $ \raw_path ->
    fmap ((/= 0) :: CULong -> Bool) $ sendMsg nsFileManager (mkSelector "fileExistsAtPath:isDirectory:") retCULong [argPtr (castPtr raw_path :: Ptr ()), argPtr isDirectory]

-- | @- isReadableFileAtPath:@
isReadableFileAtPath :: (IsNSFileManager nsFileManager, IsNSString path) => nsFileManager -> path -> IO Bool
isReadableFileAtPath nsFileManager  path =
withObjCPtr path $ \raw_path ->
    fmap ((/= 0) :: CULong -> Bool) $ sendMsg nsFileManager (mkSelector "isReadableFileAtPath:") retCULong [argPtr (castPtr raw_path :: Ptr ())]

-- | @- isWritableFileAtPath:@
isWritableFileAtPath :: (IsNSFileManager nsFileManager, IsNSString path) => nsFileManager -> path -> IO Bool
isWritableFileAtPath nsFileManager  path =
withObjCPtr path $ \raw_path ->
    fmap ((/= 0) :: CULong -> Bool) $ sendMsg nsFileManager (mkSelector "isWritableFileAtPath:") retCULong [argPtr (castPtr raw_path :: Ptr ())]

-- | @- isExecutableFileAtPath:@
isExecutableFileAtPath :: (IsNSFileManager nsFileManager, IsNSString path) => nsFileManager -> path -> IO Bool
isExecutableFileAtPath nsFileManager  path =
withObjCPtr path $ \raw_path ->
    fmap ((/= 0) :: CULong -> Bool) $ sendMsg nsFileManager (mkSelector "isExecutableFileAtPath:") retCULong [argPtr (castPtr raw_path :: Ptr ())]

-- | @- isDeletableFileAtPath:@
isDeletableFileAtPath :: (IsNSFileManager nsFileManager, IsNSString path) => nsFileManager -> path -> IO Bool
isDeletableFileAtPath nsFileManager  path =
withObjCPtr path $ \raw_path ->
    fmap ((/= 0) :: CULong -> Bool) $ sendMsg nsFileManager (mkSelector "isDeletableFileAtPath:") retCULong [argPtr (castPtr raw_path :: Ptr ())]

-- | @- contentsEqualAtPath:andPath:@
contentsEqualAtPath_andPath :: (IsNSFileManager nsFileManager, IsNSString path1, IsNSString path2) => nsFileManager -> path1 -> path2 -> IO Bool
contentsEqualAtPath_andPath nsFileManager  path1 path2 =
withObjCPtr path1 $ \raw_path1 ->
  withObjCPtr path2 $ \raw_path2 ->
      fmap ((/= 0) :: CULong -> Bool) $ sendMsg nsFileManager (mkSelector "contentsEqualAtPath:andPath:") retCULong [argPtr (castPtr raw_path1 :: Ptr ()), argPtr (castPtr raw_path2 :: Ptr ())]

-- | @- displayNameAtPath:@
displayNameAtPath :: (IsNSFileManager nsFileManager, IsNSString path) => nsFileManager -> path -> IO (Id NSString)
displayNameAtPath nsFileManager  path =
withObjCPtr path $ \raw_path ->
    sendMsg nsFileManager (mkSelector "displayNameAtPath:") (retPtr retVoid) [argPtr (castPtr raw_path :: Ptr ())] >>= retainedObject . castPtr

-- | @- componentsToDisplayForPath:@
componentsToDisplayForPath :: (IsNSFileManager nsFileManager, IsNSString path) => nsFileManager -> path -> IO (Id NSArray)
componentsToDisplayForPath nsFileManager  path =
withObjCPtr path $ \raw_path ->
    sendMsg nsFileManager (mkSelector "componentsToDisplayForPath:") (retPtr retVoid) [argPtr (castPtr raw_path :: Ptr ())] >>= retainedObject . castPtr

-- | @- enumeratorAtPath:@
enumeratorAtPath :: (IsNSFileManager nsFileManager, IsNSString path) => nsFileManager -> path -> IO (Id NSDirectoryEnumerator)
enumeratorAtPath nsFileManager  path =
withObjCPtr path $ \raw_path ->
    sendMsg nsFileManager (mkSelector "enumeratorAtPath:") (retPtr retVoid) [argPtr (castPtr raw_path :: Ptr ())] >>= retainedObject . castPtr

-- | @- enumeratorAtURL:includingPropertiesForKeys:options:errorHandler:@
enumeratorAtURL_includingPropertiesForKeys_options_errorHandler :: (IsNSFileManager nsFileManager, IsNSURL url, IsNSArray keys) => nsFileManager -> url -> keys -> NSDirectoryEnumerationOptions -> Ptr () -> IO (Id NSDirectoryEnumerator)
enumeratorAtURL_includingPropertiesForKeys_options_errorHandler nsFileManager  url keys mask handler =
withObjCPtr url $ \raw_url ->
  withObjCPtr keys $ \raw_keys ->
      sendMsg nsFileManager (mkSelector "enumeratorAtURL:includingPropertiesForKeys:options:errorHandler:") (retPtr retVoid) [argPtr (castPtr raw_url :: Ptr ()), argPtr (castPtr raw_keys :: Ptr ()), argCULong (coerce mask), argPtr (castPtr handler :: Ptr ())] >>= retainedObject . castPtr

-- | @- subpathsAtPath:@
subpathsAtPath :: (IsNSFileManager nsFileManager, IsNSString path) => nsFileManager -> path -> IO (Id NSArray)
subpathsAtPath nsFileManager  path =
withObjCPtr path $ \raw_path ->
    sendMsg nsFileManager (mkSelector "subpathsAtPath:") (retPtr retVoid) [argPtr (castPtr raw_path :: Ptr ())] >>= retainedObject . castPtr

-- | @- contentsAtPath:@
contentsAtPath :: (IsNSFileManager nsFileManager, IsNSString path) => nsFileManager -> path -> IO (Id NSData)
contentsAtPath nsFileManager  path =
withObjCPtr path $ \raw_path ->
    sendMsg nsFileManager (mkSelector "contentsAtPath:") (retPtr retVoid) [argPtr (castPtr raw_path :: Ptr ())] >>= retainedObject . castPtr

-- | @- createFileAtPath:contents:attributes:@
createFileAtPath_contents_attributes :: (IsNSFileManager nsFileManager, IsNSString path, IsNSData data_, IsNSDictionary attr) => nsFileManager -> path -> data_ -> attr -> IO Bool
createFileAtPath_contents_attributes nsFileManager  path data_ attr =
withObjCPtr path $ \raw_path ->
  withObjCPtr data_ $ \raw_data_ ->
    withObjCPtr attr $ \raw_attr ->
        fmap ((/= 0) :: CULong -> Bool) $ sendMsg nsFileManager (mkSelector "createFileAtPath:contents:attributes:") retCULong [argPtr (castPtr raw_path :: Ptr ()), argPtr (castPtr raw_data_ :: Ptr ()), argPtr (castPtr raw_attr :: Ptr ())]

-- | @- fileSystemRepresentationWithPath:@
fileSystemRepresentationWithPath :: (IsNSFileManager nsFileManager, IsNSString path) => nsFileManager -> path -> IO (Const (Ptr CChar))
fileSystemRepresentationWithPath nsFileManager  path =
withObjCPtr path $ \raw_path ->
    fmap Const $ fmap castPtr $ sendMsg nsFileManager (mkSelector "fileSystemRepresentationWithPath:") (retPtr retVoid) [argPtr (castPtr raw_path :: Ptr ())]

-- | @- stringWithFileSystemRepresentation:length:@
stringWithFileSystemRepresentation_length :: IsNSFileManager nsFileManager => nsFileManager -> Const (Ptr CChar) -> CULong -> IO (Id NSString)
stringWithFileSystemRepresentation_length nsFileManager  str len =
  sendMsg nsFileManager (mkSelector "stringWithFileSystemRepresentation:length:") (retPtr retVoid) [argPtr (unConst str), argCULong (fromIntegral len)] >>= retainedObject . castPtr

-- | @- replaceItemAtURL:withItemAtURL:backupItemName:options:resultingItemURL:error:@
replaceItemAtURL_withItemAtURL_backupItemName_options_resultingItemURL_error :: (IsNSFileManager nsFileManager, IsNSURL originalItemURL, IsNSURL newItemURL, IsNSString backupItemName, IsNSURL resultingURL, IsNSError error_) => nsFileManager -> originalItemURL -> newItemURL -> backupItemName -> NSFileManagerItemReplacementOptions -> resultingURL -> error_ -> IO Bool
replaceItemAtURL_withItemAtURL_backupItemName_options_resultingItemURL_error nsFileManager  originalItemURL newItemURL backupItemName options resultingURL error_ =
withObjCPtr originalItemURL $ \raw_originalItemURL ->
  withObjCPtr newItemURL $ \raw_newItemURL ->
    withObjCPtr backupItemName $ \raw_backupItemName ->
      withObjCPtr resultingURL $ \raw_resultingURL ->
        withObjCPtr error_ $ \raw_error_ ->
            fmap ((/= 0) :: CULong -> Bool) $ sendMsg nsFileManager (mkSelector "replaceItemAtURL:withItemAtURL:backupItemName:options:resultingItemURL:error:") retCULong [argPtr (castPtr raw_originalItemURL :: Ptr ()), argPtr (castPtr raw_newItemURL :: Ptr ()), argPtr (castPtr raw_backupItemName :: Ptr ()), argCULong (coerce options), argPtr (castPtr raw_resultingURL :: Ptr ()), argPtr (castPtr raw_error_ :: Ptr ())]

-- | @- setUbiquitous:itemAtURL:destinationURL:error:@
setUbiquitous_itemAtURL_destinationURL_error :: (IsNSFileManager nsFileManager, IsNSURL url, IsNSURL destinationURL, IsNSError error_) => nsFileManager -> Bool -> url -> destinationURL -> error_ -> IO Bool
setUbiquitous_itemAtURL_destinationURL_error nsFileManager  flag url destinationURL error_ =
withObjCPtr url $ \raw_url ->
  withObjCPtr destinationURL $ \raw_destinationURL ->
    withObjCPtr error_ $ \raw_error_ ->
        fmap ((/= 0) :: CULong -> Bool) $ sendMsg nsFileManager (mkSelector "setUbiquitous:itemAtURL:destinationURL:error:") retCULong [argCULong (if flag then 1 else 0), argPtr (castPtr raw_url :: Ptr ()), argPtr (castPtr raw_destinationURL :: Ptr ()), argPtr (castPtr raw_error_ :: Ptr ())]

-- | @- isUbiquitousItemAtURL:@
isUbiquitousItemAtURL :: (IsNSFileManager nsFileManager, IsNSURL url) => nsFileManager -> url -> IO Bool
isUbiquitousItemAtURL nsFileManager  url =
withObjCPtr url $ \raw_url ->
    fmap ((/= 0) :: CULong -> Bool) $ sendMsg nsFileManager (mkSelector "isUbiquitousItemAtURL:") retCULong [argPtr (castPtr raw_url :: Ptr ())]

-- | @- startDownloadingUbiquitousItemAtURL:error:@
startDownloadingUbiquitousItemAtURL_error :: (IsNSFileManager nsFileManager, IsNSURL url, IsNSError error_) => nsFileManager -> url -> error_ -> IO Bool
startDownloadingUbiquitousItemAtURL_error nsFileManager  url error_ =
withObjCPtr url $ \raw_url ->
  withObjCPtr error_ $ \raw_error_ ->
      fmap ((/= 0) :: CULong -> Bool) $ sendMsg nsFileManager (mkSelector "startDownloadingUbiquitousItemAtURL:error:") retCULong [argPtr (castPtr raw_url :: Ptr ()), argPtr (castPtr raw_error_ :: Ptr ())]

-- | @- evictUbiquitousItemAtURL:error:@
evictUbiquitousItemAtURL_error :: (IsNSFileManager nsFileManager, IsNSURL url, IsNSError error_) => nsFileManager -> url -> error_ -> IO Bool
evictUbiquitousItemAtURL_error nsFileManager  url error_ =
withObjCPtr url $ \raw_url ->
  withObjCPtr error_ $ \raw_error_ ->
      fmap ((/= 0) :: CULong -> Bool) $ sendMsg nsFileManager (mkSelector "evictUbiquitousItemAtURL:error:") retCULong [argPtr (castPtr raw_url :: Ptr ()), argPtr (castPtr raw_error_ :: Ptr ())]

-- | @- URLForUbiquityContainerIdentifier:@
urlForUbiquityContainerIdentifier :: (IsNSFileManager nsFileManager, IsNSString containerIdentifier) => nsFileManager -> containerIdentifier -> IO (Id NSURL)
urlForUbiquityContainerIdentifier nsFileManager  containerIdentifier =
withObjCPtr containerIdentifier $ \raw_containerIdentifier ->
    sendMsg nsFileManager (mkSelector "URLForUbiquityContainerIdentifier:") (retPtr retVoid) [argPtr (castPtr raw_containerIdentifier :: Ptr ())] >>= retainedObject . castPtr

-- | @- URLForPublishingUbiquitousItemAtURL:expirationDate:error:@
urlForPublishingUbiquitousItemAtURL_expirationDate_error :: (IsNSFileManager nsFileManager, IsNSURL url, IsNSDate outDate, IsNSError error_) => nsFileManager -> url -> outDate -> error_ -> IO (Id NSURL)
urlForPublishingUbiquitousItemAtURL_expirationDate_error nsFileManager  url outDate error_ =
withObjCPtr url $ \raw_url ->
  withObjCPtr outDate $ \raw_outDate ->
    withObjCPtr error_ $ \raw_error_ ->
        sendMsg nsFileManager (mkSelector "URLForPublishingUbiquitousItemAtURL:expirationDate:error:") (retPtr retVoid) [argPtr (castPtr raw_url :: Ptr ()), argPtr (castPtr raw_outDate :: Ptr ()), argPtr (castPtr raw_error_ :: Ptr ())] >>= retainedObject . castPtr

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
pauseSyncForUbiquitousItemAtURL_completionHandler nsFileManager  url completionHandler =
withObjCPtr url $ \raw_url ->
    sendMsg nsFileManager (mkSelector "pauseSyncForUbiquitousItemAtURL:completionHandler:") retVoid [argPtr (castPtr raw_url :: Ptr ()), argPtr (castPtr completionHandler :: Ptr ())]

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
resumeSyncForUbiquitousItemAtURL_withBehavior_completionHandler nsFileManager  url behavior completionHandler =
withObjCPtr url $ \raw_url ->
    sendMsg nsFileManager (mkSelector "resumeSyncForUbiquitousItemAtURL:withBehavior:completionHandler:") retVoid [argPtr (castPtr raw_url :: Ptr ()), argCLong (coerce behavior), argPtr (castPtr completionHandler :: Ptr ())]

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
fetchLatestRemoteVersionOfItemAtURL_completionHandler nsFileManager  url completionHandler =
withObjCPtr url $ \raw_url ->
    sendMsg nsFileManager (mkSelector "fetchLatestRemoteVersionOfItemAtURL:completionHandler:") retVoid [argPtr (castPtr raw_url :: Ptr ()), argPtr (castPtr completionHandler :: Ptr ())]

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
uploadLocalVersionOfUbiquitousItemAtURL_withConflictResolutionPolicy_completionHandler nsFileManager  url conflictResolutionPolicy completionHandler =
withObjCPtr url $ \raw_url ->
    sendMsg nsFileManager (mkSelector "uploadLocalVersionOfUbiquitousItemAtURL:withConflictResolutionPolicy:completionHandler:") retVoid [argPtr (castPtr raw_url :: Ptr ()), argCLong (coerce conflictResolutionPolicy), argPtr (castPtr completionHandler :: Ptr ())]

-- | @- containerURLForSecurityApplicationGroupIdentifier:@
containerURLForSecurityApplicationGroupIdentifier :: (IsNSFileManager nsFileManager, IsNSString groupIdentifier) => nsFileManager -> groupIdentifier -> IO (Id NSURL)
containerURLForSecurityApplicationGroupIdentifier nsFileManager  groupIdentifier =
withObjCPtr groupIdentifier $ \raw_groupIdentifier ->
    sendMsg nsFileManager (mkSelector "containerURLForSecurityApplicationGroupIdentifier:") (retPtr retVoid) [argPtr (castPtr raw_groupIdentifier :: Ptr ())] >>= retainedObject . castPtr

-- | @- homeDirectoryForUser:@
homeDirectoryForUser :: (IsNSFileManager nsFileManager, IsNSString userName) => nsFileManager -> userName -> IO (Id NSURL)
homeDirectoryForUser nsFileManager  userName =
withObjCPtr userName $ \raw_userName ->
    sendMsg nsFileManager (mkSelector "homeDirectoryForUser:") (retPtr retVoid) [argPtr (castPtr raw_userName :: Ptr ())] >>= retainedObject . castPtr

-- | @+ defaultManager@
defaultManager :: IO (Id NSFileManager)
defaultManager  =
  do
    cls' <- getRequiredClass "NSFileManager"
    sendClassMsg cls' (mkSelector "defaultManager") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- currentDirectoryPath@
currentDirectoryPath :: IsNSFileManager nsFileManager => nsFileManager -> IO (Id NSString)
currentDirectoryPath nsFileManager  =
  sendMsg nsFileManager (mkSelector "currentDirectoryPath") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- temporaryDirectory@
temporaryDirectory :: IsNSFileManager nsFileManager => nsFileManager -> IO (Id NSURL)
temporaryDirectory nsFileManager  =
  sendMsg nsFileManager (mkSelector "temporaryDirectory") (retPtr retVoid) [] >>= retainedObject . castPtr

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @mountedVolumeURLsIncludingResourceValuesForKeys:options:@
mountedVolumeURLsIncludingResourceValuesForKeys_optionsSelector :: Selector
mountedVolumeURLsIncludingResourceValuesForKeys_optionsSelector = mkSelector "mountedVolumeURLsIncludingResourceValuesForKeys:options:"

-- | @Selector@ for @unmountVolumeAtURL:options:completionHandler:@
unmountVolumeAtURL_options_completionHandlerSelector :: Selector
unmountVolumeAtURL_options_completionHandlerSelector = mkSelector "unmountVolumeAtURL:options:completionHandler:"

-- | @Selector@ for @contentsOfDirectoryAtURL:includingPropertiesForKeys:options:error:@
contentsOfDirectoryAtURL_includingPropertiesForKeys_options_errorSelector :: Selector
contentsOfDirectoryAtURL_includingPropertiesForKeys_options_errorSelector = mkSelector "contentsOfDirectoryAtURL:includingPropertiesForKeys:options:error:"

-- | @Selector@ for @URLsForDirectory:inDomains:@
urLsForDirectory_inDomainsSelector :: Selector
urLsForDirectory_inDomainsSelector = mkSelector "URLsForDirectory:inDomains:"

-- | @Selector@ for @URLForDirectory:inDomain:appropriateForURL:create:error:@
urlForDirectory_inDomain_appropriateForURL_create_errorSelector :: Selector
urlForDirectory_inDomain_appropriateForURL_create_errorSelector = mkSelector "URLForDirectory:inDomain:appropriateForURL:create:error:"

-- | @Selector@ for @getRelationship:ofDirectoryAtURL:toItemAtURL:error:@
getRelationship_ofDirectoryAtURL_toItemAtURL_errorSelector :: Selector
getRelationship_ofDirectoryAtURL_toItemAtURL_errorSelector = mkSelector "getRelationship:ofDirectoryAtURL:toItemAtURL:error:"

-- | @Selector@ for @getRelationship:ofDirectory:inDomain:toItemAtURL:error:@
getRelationship_ofDirectory_inDomain_toItemAtURL_errorSelector :: Selector
getRelationship_ofDirectory_inDomain_toItemAtURL_errorSelector = mkSelector "getRelationship:ofDirectory:inDomain:toItemAtURL:error:"

-- | @Selector@ for @createDirectoryAtURL:withIntermediateDirectories:attributes:error:@
createDirectoryAtURL_withIntermediateDirectories_attributes_errorSelector :: Selector
createDirectoryAtURL_withIntermediateDirectories_attributes_errorSelector = mkSelector "createDirectoryAtURL:withIntermediateDirectories:attributes:error:"

-- | @Selector@ for @createSymbolicLinkAtURL:withDestinationURL:error:@
createSymbolicLinkAtURL_withDestinationURL_errorSelector :: Selector
createSymbolicLinkAtURL_withDestinationURL_errorSelector = mkSelector "createSymbolicLinkAtURL:withDestinationURL:error:"

-- | @Selector@ for @setAttributes:ofItemAtPath:error:@
setAttributes_ofItemAtPath_errorSelector :: Selector
setAttributes_ofItemAtPath_errorSelector = mkSelector "setAttributes:ofItemAtPath:error:"

-- | @Selector@ for @createDirectoryAtPath:withIntermediateDirectories:attributes:error:@
createDirectoryAtPath_withIntermediateDirectories_attributes_errorSelector :: Selector
createDirectoryAtPath_withIntermediateDirectories_attributes_errorSelector = mkSelector "createDirectoryAtPath:withIntermediateDirectories:attributes:error:"

-- | @Selector@ for @contentsOfDirectoryAtPath:error:@
contentsOfDirectoryAtPath_errorSelector :: Selector
contentsOfDirectoryAtPath_errorSelector = mkSelector "contentsOfDirectoryAtPath:error:"

-- | @Selector@ for @subpathsOfDirectoryAtPath:error:@
subpathsOfDirectoryAtPath_errorSelector :: Selector
subpathsOfDirectoryAtPath_errorSelector = mkSelector "subpathsOfDirectoryAtPath:error:"

-- | @Selector@ for @attributesOfItemAtPath:error:@
attributesOfItemAtPath_errorSelector :: Selector
attributesOfItemAtPath_errorSelector = mkSelector "attributesOfItemAtPath:error:"

-- | @Selector@ for @attributesOfFileSystemForPath:error:@
attributesOfFileSystemForPath_errorSelector :: Selector
attributesOfFileSystemForPath_errorSelector = mkSelector "attributesOfFileSystemForPath:error:"

-- | @Selector@ for @createSymbolicLinkAtPath:withDestinationPath:error:@
createSymbolicLinkAtPath_withDestinationPath_errorSelector :: Selector
createSymbolicLinkAtPath_withDestinationPath_errorSelector = mkSelector "createSymbolicLinkAtPath:withDestinationPath:error:"

-- | @Selector@ for @destinationOfSymbolicLinkAtPath:error:@
destinationOfSymbolicLinkAtPath_errorSelector :: Selector
destinationOfSymbolicLinkAtPath_errorSelector = mkSelector "destinationOfSymbolicLinkAtPath:error:"

-- | @Selector@ for @copyItemAtPath:toPath:error:@
copyItemAtPath_toPath_errorSelector :: Selector
copyItemAtPath_toPath_errorSelector = mkSelector "copyItemAtPath:toPath:error:"

-- | @Selector@ for @moveItemAtPath:toPath:error:@
moveItemAtPath_toPath_errorSelector :: Selector
moveItemAtPath_toPath_errorSelector = mkSelector "moveItemAtPath:toPath:error:"

-- | @Selector@ for @linkItemAtPath:toPath:error:@
linkItemAtPath_toPath_errorSelector :: Selector
linkItemAtPath_toPath_errorSelector = mkSelector "linkItemAtPath:toPath:error:"

-- | @Selector@ for @removeItemAtPath:error:@
removeItemAtPath_errorSelector :: Selector
removeItemAtPath_errorSelector = mkSelector "removeItemAtPath:error:"

-- | @Selector@ for @copyItemAtURL:toURL:error:@
copyItemAtURL_toURL_errorSelector :: Selector
copyItemAtURL_toURL_errorSelector = mkSelector "copyItemAtURL:toURL:error:"

-- | @Selector@ for @moveItemAtURL:toURL:error:@
moveItemAtURL_toURL_errorSelector :: Selector
moveItemAtURL_toURL_errorSelector = mkSelector "moveItemAtURL:toURL:error:"

-- | @Selector@ for @linkItemAtURL:toURL:error:@
linkItemAtURL_toURL_errorSelector :: Selector
linkItemAtURL_toURL_errorSelector = mkSelector "linkItemAtURL:toURL:error:"

-- | @Selector@ for @removeItemAtURL:error:@
removeItemAtURL_errorSelector :: Selector
removeItemAtURL_errorSelector = mkSelector "removeItemAtURL:error:"

-- | @Selector@ for @trashItemAtURL:resultingItemURL:error:@
trashItemAtURL_resultingItemURL_errorSelector :: Selector
trashItemAtURL_resultingItemURL_errorSelector = mkSelector "trashItemAtURL:resultingItemURL:error:"

-- | @Selector@ for @fileAttributesAtPath:traverseLink:@
fileAttributesAtPath_traverseLinkSelector :: Selector
fileAttributesAtPath_traverseLinkSelector = mkSelector "fileAttributesAtPath:traverseLink:"

-- | @Selector@ for @changeFileAttributes:atPath:@
changeFileAttributes_atPathSelector :: Selector
changeFileAttributes_atPathSelector = mkSelector "changeFileAttributes:atPath:"

-- | @Selector@ for @directoryContentsAtPath:@
directoryContentsAtPathSelector :: Selector
directoryContentsAtPathSelector = mkSelector "directoryContentsAtPath:"

-- | @Selector@ for @fileSystemAttributesAtPath:@
fileSystemAttributesAtPathSelector :: Selector
fileSystemAttributesAtPathSelector = mkSelector "fileSystemAttributesAtPath:"

-- | @Selector@ for @pathContentOfSymbolicLinkAtPath:@
pathContentOfSymbolicLinkAtPathSelector :: Selector
pathContentOfSymbolicLinkAtPathSelector = mkSelector "pathContentOfSymbolicLinkAtPath:"

-- | @Selector@ for @createSymbolicLinkAtPath:pathContent:@
createSymbolicLinkAtPath_pathContentSelector :: Selector
createSymbolicLinkAtPath_pathContentSelector = mkSelector "createSymbolicLinkAtPath:pathContent:"

-- | @Selector@ for @createDirectoryAtPath:attributes:@
createDirectoryAtPath_attributesSelector :: Selector
createDirectoryAtPath_attributesSelector = mkSelector "createDirectoryAtPath:attributes:"

-- | @Selector@ for @linkPath:toPath:handler:@
linkPath_toPath_handlerSelector :: Selector
linkPath_toPath_handlerSelector = mkSelector "linkPath:toPath:handler:"

-- | @Selector@ for @copyPath:toPath:handler:@
copyPath_toPath_handlerSelector :: Selector
copyPath_toPath_handlerSelector = mkSelector "copyPath:toPath:handler:"

-- | @Selector@ for @movePath:toPath:handler:@
movePath_toPath_handlerSelector :: Selector
movePath_toPath_handlerSelector = mkSelector "movePath:toPath:handler:"

-- | @Selector@ for @removeFileAtPath:handler:@
removeFileAtPath_handlerSelector :: Selector
removeFileAtPath_handlerSelector = mkSelector "removeFileAtPath:handler:"

-- | @Selector@ for @changeCurrentDirectoryPath:@
changeCurrentDirectoryPathSelector :: Selector
changeCurrentDirectoryPathSelector = mkSelector "changeCurrentDirectoryPath:"

-- | @Selector@ for @fileExistsAtPath:@
fileExistsAtPathSelector :: Selector
fileExistsAtPathSelector = mkSelector "fileExistsAtPath:"

-- | @Selector@ for @fileExistsAtPath:isDirectory:@
fileExistsAtPath_isDirectorySelector :: Selector
fileExistsAtPath_isDirectorySelector = mkSelector "fileExistsAtPath:isDirectory:"

-- | @Selector@ for @isReadableFileAtPath:@
isReadableFileAtPathSelector :: Selector
isReadableFileAtPathSelector = mkSelector "isReadableFileAtPath:"

-- | @Selector@ for @isWritableFileAtPath:@
isWritableFileAtPathSelector :: Selector
isWritableFileAtPathSelector = mkSelector "isWritableFileAtPath:"

-- | @Selector@ for @isExecutableFileAtPath:@
isExecutableFileAtPathSelector :: Selector
isExecutableFileAtPathSelector = mkSelector "isExecutableFileAtPath:"

-- | @Selector@ for @isDeletableFileAtPath:@
isDeletableFileAtPathSelector :: Selector
isDeletableFileAtPathSelector = mkSelector "isDeletableFileAtPath:"

-- | @Selector@ for @contentsEqualAtPath:andPath:@
contentsEqualAtPath_andPathSelector :: Selector
contentsEqualAtPath_andPathSelector = mkSelector "contentsEqualAtPath:andPath:"

-- | @Selector@ for @displayNameAtPath:@
displayNameAtPathSelector :: Selector
displayNameAtPathSelector = mkSelector "displayNameAtPath:"

-- | @Selector@ for @componentsToDisplayForPath:@
componentsToDisplayForPathSelector :: Selector
componentsToDisplayForPathSelector = mkSelector "componentsToDisplayForPath:"

-- | @Selector@ for @enumeratorAtPath:@
enumeratorAtPathSelector :: Selector
enumeratorAtPathSelector = mkSelector "enumeratorAtPath:"

-- | @Selector@ for @enumeratorAtURL:includingPropertiesForKeys:options:errorHandler:@
enumeratorAtURL_includingPropertiesForKeys_options_errorHandlerSelector :: Selector
enumeratorAtURL_includingPropertiesForKeys_options_errorHandlerSelector = mkSelector "enumeratorAtURL:includingPropertiesForKeys:options:errorHandler:"

-- | @Selector@ for @subpathsAtPath:@
subpathsAtPathSelector :: Selector
subpathsAtPathSelector = mkSelector "subpathsAtPath:"

-- | @Selector@ for @contentsAtPath:@
contentsAtPathSelector :: Selector
contentsAtPathSelector = mkSelector "contentsAtPath:"

-- | @Selector@ for @createFileAtPath:contents:attributes:@
createFileAtPath_contents_attributesSelector :: Selector
createFileAtPath_contents_attributesSelector = mkSelector "createFileAtPath:contents:attributes:"

-- | @Selector@ for @fileSystemRepresentationWithPath:@
fileSystemRepresentationWithPathSelector :: Selector
fileSystemRepresentationWithPathSelector = mkSelector "fileSystemRepresentationWithPath:"

-- | @Selector@ for @stringWithFileSystemRepresentation:length:@
stringWithFileSystemRepresentation_lengthSelector :: Selector
stringWithFileSystemRepresentation_lengthSelector = mkSelector "stringWithFileSystemRepresentation:length:"

-- | @Selector@ for @replaceItemAtURL:withItemAtURL:backupItemName:options:resultingItemURL:error:@
replaceItemAtURL_withItemAtURL_backupItemName_options_resultingItemURL_errorSelector :: Selector
replaceItemAtURL_withItemAtURL_backupItemName_options_resultingItemURL_errorSelector = mkSelector "replaceItemAtURL:withItemAtURL:backupItemName:options:resultingItemURL:error:"

-- | @Selector@ for @setUbiquitous:itemAtURL:destinationURL:error:@
setUbiquitous_itemAtURL_destinationURL_errorSelector :: Selector
setUbiquitous_itemAtURL_destinationURL_errorSelector = mkSelector "setUbiquitous:itemAtURL:destinationURL:error:"

-- | @Selector@ for @isUbiquitousItemAtURL:@
isUbiquitousItemAtURLSelector :: Selector
isUbiquitousItemAtURLSelector = mkSelector "isUbiquitousItemAtURL:"

-- | @Selector@ for @startDownloadingUbiquitousItemAtURL:error:@
startDownloadingUbiquitousItemAtURL_errorSelector :: Selector
startDownloadingUbiquitousItemAtURL_errorSelector = mkSelector "startDownloadingUbiquitousItemAtURL:error:"

-- | @Selector@ for @evictUbiquitousItemAtURL:error:@
evictUbiquitousItemAtURL_errorSelector :: Selector
evictUbiquitousItemAtURL_errorSelector = mkSelector "evictUbiquitousItemAtURL:error:"

-- | @Selector@ for @URLForUbiquityContainerIdentifier:@
urlForUbiquityContainerIdentifierSelector :: Selector
urlForUbiquityContainerIdentifierSelector = mkSelector "URLForUbiquityContainerIdentifier:"

-- | @Selector@ for @URLForPublishingUbiquitousItemAtURL:expirationDate:error:@
urlForPublishingUbiquitousItemAtURL_expirationDate_errorSelector :: Selector
urlForPublishingUbiquitousItemAtURL_expirationDate_errorSelector = mkSelector "URLForPublishingUbiquitousItemAtURL:expirationDate:error:"

-- | @Selector@ for @pauseSyncForUbiquitousItemAtURL:completionHandler:@
pauseSyncForUbiquitousItemAtURL_completionHandlerSelector :: Selector
pauseSyncForUbiquitousItemAtURL_completionHandlerSelector = mkSelector "pauseSyncForUbiquitousItemAtURL:completionHandler:"

-- | @Selector@ for @resumeSyncForUbiquitousItemAtURL:withBehavior:completionHandler:@
resumeSyncForUbiquitousItemAtURL_withBehavior_completionHandlerSelector :: Selector
resumeSyncForUbiquitousItemAtURL_withBehavior_completionHandlerSelector = mkSelector "resumeSyncForUbiquitousItemAtURL:withBehavior:completionHandler:"

-- | @Selector@ for @fetchLatestRemoteVersionOfItemAtURL:completionHandler:@
fetchLatestRemoteVersionOfItemAtURL_completionHandlerSelector :: Selector
fetchLatestRemoteVersionOfItemAtURL_completionHandlerSelector = mkSelector "fetchLatestRemoteVersionOfItemAtURL:completionHandler:"

-- | @Selector@ for @uploadLocalVersionOfUbiquitousItemAtURL:withConflictResolutionPolicy:completionHandler:@
uploadLocalVersionOfUbiquitousItemAtURL_withConflictResolutionPolicy_completionHandlerSelector :: Selector
uploadLocalVersionOfUbiquitousItemAtURL_withConflictResolutionPolicy_completionHandlerSelector = mkSelector "uploadLocalVersionOfUbiquitousItemAtURL:withConflictResolutionPolicy:completionHandler:"

-- | @Selector@ for @containerURLForSecurityApplicationGroupIdentifier:@
containerURLForSecurityApplicationGroupIdentifierSelector :: Selector
containerURLForSecurityApplicationGroupIdentifierSelector = mkSelector "containerURLForSecurityApplicationGroupIdentifier:"

-- | @Selector@ for @homeDirectoryForUser:@
homeDirectoryForUserSelector :: Selector
homeDirectoryForUserSelector = mkSelector "homeDirectoryForUser:"

-- | @Selector@ for @defaultManager@
defaultManagerSelector :: Selector
defaultManagerSelector = mkSelector "defaultManager"

-- | @Selector@ for @currentDirectoryPath@
currentDirectoryPathSelector :: Selector
currentDirectoryPathSelector = mkSelector "currentDirectoryPath"

-- | @Selector@ for @temporaryDirectory@
temporaryDirectorySelector :: Selector
temporaryDirectorySelector = mkSelector "temporaryDirectory"

