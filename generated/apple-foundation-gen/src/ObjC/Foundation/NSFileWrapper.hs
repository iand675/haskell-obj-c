{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Generated bindings for @NSFileWrapper@.
module ObjC.Foundation.NSFileWrapper
  ( NSFileWrapper
  , IsNSFileWrapper(..)
  , initWithURL_options_error
  , initDirectoryWithFileWrappers
  , initRegularFileWithContents
  , initSymbolicLinkWithDestinationURL
  , initWithSerializedRepresentation
  , initWithCoder
  , matchesContentsOfURL
  , readFromURL_options_error
  , writeToURL_options_originalContentsURL_error
  , addFileWrapper
  , addRegularFileWithContents_preferredFilename
  , removeFileWrapper
  , keyForFileWrapper
  , initWithPath
  , initSymbolicLinkWithDestination
  , needsToBeUpdatedFromPath
  , updateFromPath
  , writeToFile_atomically_updateFilenames
  , addFileWithPath
  , addSymbolicLinkWithDestination_preferredFilename
  , symbolicLinkDestination
  , directory
  , regularFile
  , symbolicLink
  , preferredFilename
  , setPreferredFilename
  , filename
  , setFilename
  , fileAttributes
  , setFileAttributes
  , serializedRepresentation
  , fileWrappers
  , regularFileContents
  , symbolicLinkDestinationURL
  , addFileWithPathSelector
  , addFileWrapperSelector
  , addRegularFileWithContents_preferredFilenameSelector
  , addSymbolicLinkWithDestination_preferredFilenameSelector
  , directorySelector
  , fileAttributesSelector
  , fileWrappersSelector
  , filenameSelector
  , initDirectoryWithFileWrappersSelector
  , initRegularFileWithContentsSelector
  , initSymbolicLinkWithDestinationSelector
  , initSymbolicLinkWithDestinationURLSelector
  , initWithCoderSelector
  , initWithPathSelector
  , initWithSerializedRepresentationSelector
  , initWithURL_options_errorSelector
  , keyForFileWrapperSelector
  , matchesContentsOfURLSelector
  , needsToBeUpdatedFromPathSelector
  , preferredFilenameSelector
  , readFromURL_options_errorSelector
  , regularFileContentsSelector
  , regularFileSelector
  , removeFileWrapperSelector
  , serializedRepresentationSelector
  , setFileAttributesSelector
  , setFilenameSelector
  , setPreferredFilenameSelector
  , symbolicLinkDestinationSelector
  , symbolicLinkDestinationURLSelector
  , symbolicLinkSelector
  , updateFromPathSelector
  , writeToFile_atomically_updateFilenamesSelector
  , writeToURL_options_originalContentsURL_errorSelector

  -- * Enum types
  , NSFileWrapperReadingOptions(NSFileWrapperReadingOptions)
  , pattern NSFileWrapperReadingImmediate
  , pattern NSFileWrapperReadingWithoutMapping
  , NSFileWrapperWritingOptions(NSFileWrapperWritingOptions)
  , pattern NSFileWrapperWritingAtomic
  , pattern NSFileWrapperWritingWithNameUpdating

  ) where

import Foreign.Ptr (Ptr, FunPtr)
import Foreign.C.Types

import ObjC.Runtime.Types
import ObjC.Runtime.Message (sendMessage, sendOwnedMessage, sendClassMessage, sendOwnedClassMessage)
import ObjC.Runtime.Selector (mkSelector)
import ObjC.Runtime.Class (getRequiredClass)

import ObjC.Foundation.Internal.Classes
import ObjC.Foundation.Internal.Enums

-- | @- initWithURL:options:error:@
initWithURL_options_error :: (IsNSFileWrapper nsFileWrapper, IsNSURL url, IsNSError outError) => nsFileWrapper -> url -> NSFileWrapperReadingOptions -> outError -> IO (Id NSFileWrapper)
initWithURL_options_error nsFileWrapper url options outError =
  sendOwnedMessage nsFileWrapper initWithURL_options_errorSelector (toNSURL url) options (toNSError outError)

-- | @- initDirectoryWithFileWrappers:@
initDirectoryWithFileWrappers :: (IsNSFileWrapper nsFileWrapper, IsNSDictionary childrenByPreferredName) => nsFileWrapper -> childrenByPreferredName -> IO (Id NSFileWrapper)
initDirectoryWithFileWrappers nsFileWrapper childrenByPreferredName =
  sendOwnedMessage nsFileWrapper initDirectoryWithFileWrappersSelector (toNSDictionary childrenByPreferredName)

-- | @- initRegularFileWithContents:@
initRegularFileWithContents :: (IsNSFileWrapper nsFileWrapper, IsNSData contents) => nsFileWrapper -> contents -> IO (Id NSFileWrapper)
initRegularFileWithContents nsFileWrapper contents =
  sendOwnedMessage nsFileWrapper initRegularFileWithContentsSelector (toNSData contents)

-- | @- initSymbolicLinkWithDestinationURL:@
initSymbolicLinkWithDestinationURL :: (IsNSFileWrapper nsFileWrapper, IsNSURL url) => nsFileWrapper -> url -> IO (Id NSFileWrapper)
initSymbolicLinkWithDestinationURL nsFileWrapper url =
  sendOwnedMessage nsFileWrapper initSymbolicLinkWithDestinationURLSelector (toNSURL url)

-- | @- initWithSerializedRepresentation:@
initWithSerializedRepresentation :: (IsNSFileWrapper nsFileWrapper, IsNSData serializeRepresentation) => nsFileWrapper -> serializeRepresentation -> IO (Id NSFileWrapper)
initWithSerializedRepresentation nsFileWrapper serializeRepresentation =
  sendOwnedMessage nsFileWrapper initWithSerializedRepresentationSelector (toNSData serializeRepresentation)

-- | @- initWithCoder:@
initWithCoder :: (IsNSFileWrapper nsFileWrapper, IsNSCoder inCoder) => nsFileWrapper -> inCoder -> IO (Id NSFileWrapper)
initWithCoder nsFileWrapper inCoder =
  sendOwnedMessage nsFileWrapper initWithCoderSelector (toNSCoder inCoder)

-- | @- matchesContentsOfURL:@
matchesContentsOfURL :: (IsNSFileWrapper nsFileWrapper, IsNSURL url) => nsFileWrapper -> url -> IO Bool
matchesContentsOfURL nsFileWrapper url =
  sendMessage nsFileWrapper matchesContentsOfURLSelector (toNSURL url)

-- | @- readFromURL:options:error:@
readFromURL_options_error :: (IsNSFileWrapper nsFileWrapper, IsNSURL url, IsNSError outError) => nsFileWrapper -> url -> NSFileWrapperReadingOptions -> outError -> IO Bool
readFromURL_options_error nsFileWrapper url options outError =
  sendMessage nsFileWrapper readFromURL_options_errorSelector (toNSURL url) options (toNSError outError)

-- | @- writeToURL:options:originalContentsURL:error:@
writeToURL_options_originalContentsURL_error :: (IsNSFileWrapper nsFileWrapper, IsNSURL url, IsNSURL originalContentsURL, IsNSError outError) => nsFileWrapper -> url -> NSFileWrapperWritingOptions -> originalContentsURL -> outError -> IO Bool
writeToURL_options_originalContentsURL_error nsFileWrapper url options originalContentsURL outError =
  sendMessage nsFileWrapper writeToURL_options_originalContentsURL_errorSelector (toNSURL url) options (toNSURL originalContentsURL) (toNSError outError)

-- | @- addFileWrapper:@
addFileWrapper :: (IsNSFileWrapper nsFileWrapper, IsNSFileWrapper child) => nsFileWrapper -> child -> IO (Id NSString)
addFileWrapper nsFileWrapper child =
  sendMessage nsFileWrapper addFileWrapperSelector (toNSFileWrapper child)

-- | @- addRegularFileWithContents:preferredFilename:@
addRegularFileWithContents_preferredFilename :: (IsNSFileWrapper nsFileWrapper, IsNSData data_, IsNSString fileName) => nsFileWrapper -> data_ -> fileName -> IO (Id NSString)
addRegularFileWithContents_preferredFilename nsFileWrapper data_ fileName =
  sendMessage nsFileWrapper addRegularFileWithContents_preferredFilenameSelector (toNSData data_) (toNSString fileName)

-- | @- removeFileWrapper:@
removeFileWrapper :: (IsNSFileWrapper nsFileWrapper, IsNSFileWrapper child) => nsFileWrapper -> child -> IO ()
removeFileWrapper nsFileWrapper child =
  sendMessage nsFileWrapper removeFileWrapperSelector (toNSFileWrapper child)

-- | @- keyForFileWrapper:@
keyForFileWrapper :: (IsNSFileWrapper nsFileWrapper, IsNSFileWrapper child) => nsFileWrapper -> child -> IO (Id NSString)
keyForFileWrapper nsFileWrapper child =
  sendMessage nsFileWrapper keyForFileWrapperSelector (toNSFileWrapper child)

-- | @- initWithPath:@
initWithPath :: (IsNSFileWrapper nsFileWrapper, IsNSString path) => nsFileWrapper -> path -> IO RawId
initWithPath nsFileWrapper path =
  sendOwnedMessage nsFileWrapper initWithPathSelector (toNSString path)

-- | @- initSymbolicLinkWithDestination:@
initSymbolicLinkWithDestination :: (IsNSFileWrapper nsFileWrapper, IsNSString path) => nsFileWrapper -> path -> IO RawId
initSymbolicLinkWithDestination nsFileWrapper path =
  sendOwnedMessage nsFileWrapper initSymbolicLinkWithDestinationSelector (toNSString path)

-- | @- needsToBeUpdatedFromPath:@
needsToBeUpdatedFromPath :: (IsNSFileWrapper nsFileWrapper, IsNSString path) => nsFileWrapper -> path -> IO Bool
needsToBeUpdatedFromPath nsFileWrapper path =
  sendMessage nsFileWrapper needsToBeUpdatedFromPathSelector (toNSString path)

-- | @- updateFromPath:@
updateFromPath :: (IsNSFileWrapper nsFileWrapper, IsNSString path) => nsFileWrapper -> path -> IO Bool
updateFromPath nsFileWrapper path =
  sendMessage nsFileWrapper updateFromPathSelector (toNSString path)

-- | @- writeToFile:atomically:updateFilenames:@
writeToFile_atomically_updateFilenames :: (IsNSFileWrapper nsFileWrapper, IsNSString path) => nsFileWrapper -> path -> Bool -> Bool -> IO Bool
writeToFile_atomically_updateFilenames nsFileWrapper path atomicFlag updateFilenamesFlag =
  sendMessage nsFileWrapper writeToFile_atomically_updateFilenamesSelector (toNSString path) atomicFlag updateFilenamesFlag

-- | @- addFileWithPath:@
addFileWithPath :: (IsNSFileWrapper nsFileWrapper, IsNSString path) => nsFileWrapper -> path -> IO (Id NSString)
addFileWithPath nsFileWrapper path =
  sendMessage nsFileWrapper addFileWithPathSelector (toNSString path)

-- | @- addSymbolicLinkWithDestination:preferredFilename:@
addSymbolicLinkWithDestination_preferredFilename :: (IsNSFileWrapper nsFileWrapper, IsNSString path, IsNSString filename) => nsFileWrapper -> path -> filename -> IO (Id NSString)
addSymbolicLinkWithDestination_preferredFilename nsFileWrapper path filename =
  sendMessage nsFileWrapper addSymbolicLinkWithDestination_preferredFilenameSelector (toNSString path) (toNSString filename)

-- | @- symbolicLinkDestination@
symbolicLinkDestination :: IsNSFileWrapper nsFileWrapper => nsFileWrapper -> IO (Id NSString)
symbolicLinkDestination nsFileWrapper =
  sendMessage nsFileWrapper symbolicLinkDestinationSelector

-- | @- directory@
directory :: IsNSFileWrapper nsFileWrapper => nsFileWrapper -> IO Bool
directory nsFileWrapper =
  sendMessage nsFileWrapper directorySelector

-- | @- regularFile@
regularFile :: IsNSFileWrapper nsFileWrapper => nsFileWrapper -> IO Bool
regularFile nsFileWrapper =
  sendMessage nsFileWrapper regularFileSelector

-- | @- symbolicLink@
symbolicLink :: IsNSFileWrapper nsFileWrapper => nsFileWrapper -> IO Bool
symbolicLink nsFileWrapper =
  sendMessage nsFileWrapper symbolicLinkSelector

-- | @- preferredFilename@
preferredFilename :: IsNSFileWrapper nsFileWrapper => nsFileWrapper -> IO (Id NSString)
preferredFilename nsFileWrapper =
  sendMessage nsFileWrapper preferredFilenameSelector

-- | @- setPreferredFilename:@
setPreferredFilename :: (IsNSFileWrapper nsFileWrapper, IsNSString value) => nsFileWrapper -> value -> IO ()
setPreferredFilename nsFileWrapper value =
  sendMessage nsFileWrapper setPreferredFilenameSelector (toNSString value)

-- | @- filename@
filename :: IsNSFileWrapper nsFileWrapper => nsFileWrapper -> IO (Id NSString)
filename nsFileWrapper =
  sendMessage nsFileWrapper filenameSelector

-- | @- setFilename:@
setFilename :: (IsNSFileWrapper nsFileWrapper, IsNSString value) => nsFileWrapper -> value -> IO ()
setFilename nsFileWrapper value =
  sendMessage nsFileWrapper setFilenameSelector (toNSString value)

-- | @- fileAttributes@
fileAttributes :: IsNSFileWrapper nsFileWrapper => nsFileWrapper -> IO (Id NSDictionary)
fileAttributes nsFileWrapper =
  sendMessage nsFileWrapper fileAttributesSelector

-- | @- setFileAttributes:@
setFileAttributes :: (IsNSFileWrapper nsFileWrapper, IsNSDictionary value) => nsFileWrapper -> value -> IO ()
setFileAttributes nsFileWrapper value =
  sendMessage nsFileWrapper setFileAttributesSelector (toNSDictionary value)

-- | @- serializedRepresentation@
serializedRepresentation :: IsNSFileWrapper nsFileWrapper => nsFileWrapper -> IO (Id NSData)
serializedRepresentation nsFileWrapper =
  sendMessage nsFileWrapper serializedRepresentationSelector

-- | @- fileWrappers@
fileWrappers :: IsNSFileWrapper nsFileWrapper => nsFileWrapper -> IO (Id NSDictionary)
fileWrappers nsFileWrapper =
  sendMessage nsFileWrapper fileWrappersSelector

-- | @- regularFileContents@
regularFileContents :: IsNSFileWrapper nsFileWrapper => nsFileWrapper -> IO (Id NSData)
regularFileContents nsFileWrapper =
  sendMessage nsFileWrapper regularFileContentsSelector

-- | @- symbolicLinkDestinationURL@
symbolicLinkDestinationURL :: IsNSFileWrapper nsFileWrapper => nsFileWrapper -> IO (Id NSURL)
symbolicLinkDestinationURL nsFileWrapper =
  sendMessage nsFileWrapper symbolicLinkDestinationURLSelector

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @initWithURL:options:error:@
initWithURL_options_errorSelector :: Selector '[Id NSURL, NSFileWrapperReadingOptions, Id NSError] (Id NSFileWrapper)
initWithURL_options_errorSelector = mkSelector "initWithURL:options:error:"

-- | @Selector@ for @initDirectoryWithFileWrappers:@
initDirectoryWithFileWrappersSelector :: Selector '[Id NSDictionary] (Id NSFileWrapper)
initDirectoryWithFileWrappersSelector = mkSelector "initDirectoryWithFileWrappers:"

-- | @Selector@ for @initRegularFileWithContents:@
initRegularFileWithContentsSelector :: Selector '[Id NSData] (Id NSFileWrapper)
initRegularFileWithContentsSelector = mkSelector "initRegularFileWithContents:"

-- | @Selector@ for @initSymbolicLinkWithDestinationURL:@
initSymbolicLinkWithDestinationURLSelector :: Selector '[Id NSURL] (Id NSFileWrapper)
initSymbolicLinkWithDestinationURLSelector = mkSelector "initSymbolicLinkWithDestinationURL:"

-- | @Selector@ for @initWithSerializedRepresentation:@
initWithSerializedRepresentationSelector :: Selector '[Id NSData] (Id NSFileWrapper)
initWithSerializedRepresentationSelector = mkSelector "initWithSerializedRepresentation:"

-- | @Selector@ for @initWithCoder:@
initWithCoderSelector :: Selector '[Id NSCoder] (Id NSFileWrapper)
initWithCoderSelector = mkSelector "initWithCoder:"

-- | @Selector@ for @matchesContentsOfURL:@
matchesContentsOfURLSelector :: Selector '[Id NSURL] Bool
matchesContentsOfURLSelector = mkSelector "matchesContentsOfURL:"

-- | @Selector@ for @readFromURL:options:error:@
readFromURL_options_errorSelector :: Selector '[Id NSURL, NSFileWrapperReadingOptions, Id NSError] Bool
readFromURL_options_errorSelector = mkSelector "readFromURL:options:error:"

-- | @Selector@ for @writeToURL:options:originalContentsURL:error:@
writeToURL_options_originalContentsURL_errorSelector :: Selector '[Id NSURL, NSFileWrapperWritingOptions, Id NSURL, Id NSError] Bool
writeToURL_options_originalContentsURL_errorSelector = mkSelector "writeToURL:options:originalContentsURL:error:"

-- | @Selector@ for @addFileWrapper:@
addFileWrapperSelector :: Selector '[Id NSFileWrapper] (Id NSString)
addFileWrapperSelector = mkSelector "addFileWrapper:"

-- | @Selector@ for @addRegularFileWithContents:preferredFilename:@
addRegularFileWithContents_preferredFilenameSelector :: Selector '[Id NSData, Id NSString] (Id NSString)
addRegularFileWithContents_preferredFilenameSelector = mkSelector "addRegularFileWithContents:preferredFilename:"

-- | @Selector@ for @removeFileWrapper:@
removeFileWrapperSelector :: Selector '[Id NSFileWrapper] ()
removeFileWrapperSelector = mkSelector "removeFileWrapper:"

-- | @Selector@ for @keyForFileWrapper:@
keyForFileWrapperSelector :: Selector '[Id NSFileWrapper] (Id NSString)
keyForFileWrapperSelector = mkSelector "keyForFileWrapper:"

-- | @Selector@ for @initWithPath:@
initWithPathSelector :: Selector '[Id NSString] RawId
initWithPathSelector = mkSelector "initWithPath:"

-- | @Selector@ for @initSymbolicLinkWithDestination:@
initSymbolicLinkWithDestinationSelector :: Selector '[Id NSString] RawId
initSymbolicLinkWithDestinationSelector = mkSelector "initSymbolicLinkWithDestination:"

-- | @Selector@ for @needsToBeUpdatedFromPath:@
needsToBeUpdatedFromPathSelector :: Selector '[Id NSString] Bool
needsToBeUpdatedFromPathSelector = mkSelector "needsToBeUpdatedFromPath:"

-- | @Selector@ for @updateFromPath:@
updateFromPathSelector :: Selector '[Id NSString] Bool
updateFromPathSelector = mkSelector "updateFromPath:"

-- | @Selector@ for @writeToFile:atomically:updateFilenames:@
writeToFile_atomically_updateFilenamesSelector :: Selector '[Id NSString, Bool, Bool] Bool
writeToFile_atomically_updateFilenamesSelector = mkSelector "writeToFile:atomically:updateFilenames:"

-- | @Selector@ for @addFileWithPath:@
addFileWithPathSelector :: Selector '[Id NSString] (Id NSString)
addFileWithPathSelector = mkSelector "addFileWithPath:"

-- | @Selector@ for @addSymbolicLinkWithDestination:preferredFilename:@
addSymbolicLinkWithDestination_preferredFilenameSelector :: Selector '[Id NSString, Id NSString] (Id NSString)
addSymbolicLinkWithDestination_preferredFilenameSelector = mkSelector "addSymbolicLinkWithDestination:preferredFilename:"

-- | @Selector@ for @symbolicLinkDestination@
symbolicLinkDestinationSelector :: Selector '[] (Id NSString)
symbolicLinkDestinationSelector = mkSelector "symbolicLinkDestination"

-- | @Selector@ for @directory@
directorySelector :: Selector '[] Bool
directorySelector = mkSelector "directory"

-- | @Selector@ for @regularFile@
regularFileSelector :: Selector '[] Bool
regularFileSelector = mkSelector "regularFile"

-- | @Selector@ for @symbolicLink@
symbolicLinkSelector :: Selector '[] Bool
symbolicLinkSelector = mkSelector "symbolicLink"

-- | @Selector@ for @preferredFilename@
preferredFilenameSelector :: Selector '[] (Id NSString)
preferredFilenameSelector = mkSelector "preferredFilename"

-- | @Selector@ for @setPreferredFilename:@
setPreferredFilenameSelector :: Selector '[Id NSString] ()
setPreferredFilenameSelector = mkSelector "setPreferredFilename:"

-- | @Selector@ for @filename@
filenameSelector :: Selector '[] (Id NSString)
filenameSelector = mkSelector "filename"

-- | @Selector@ for @setFilename:@
setFilenameSelector :: Selector '[Id NSString] ()
setFilenameSelector = mkSelector "setFilename:"

-- | @Selector@ for @fileAttributes@
fileAttributesSelector :: Selector '[] (Id NSDictionary)
fileAttributesSelector = mkSelector "fileAttributes"

-- | @Selector@ for @setFileAttributes:@
setFileAttributesSelector :: Selector '[Id NSDictionary] ()
setFileAttributesSelector = mkSelector "setFileAttributes:"

-- | @Selector@ for @serializedRepresentation@
serializedRepresentationSelector :: Selector '[] (Id NSData)
serializedRepresentationSelector = mkSelector "serializedRepresentation"

-- | @Selector@ for @fileWrappers@
fileWrappersSelector :: Selector '[] (Id NSDictionary)
fileWrappersSelector = mkSelector "fileWrappers"

-- | @Selector@ for @regularFileContents@
regularFileContentsSelector :: Selector '[] (Id NSData)
regularFileContentsSelector = mkSelector "regularFileContents"

-- | @Selector@ for @symbolicLinkDestinationURL@
symbolicLinkDestinationURLSelector :: Selector '[] (Id NSURL)
symbolicLinkDestinationURLSelector = mkSelector "symbolicLinkDestinationURL"

