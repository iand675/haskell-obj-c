{-# LANGUAGE PatternSynonyms #-}
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
  , initWithURL_options_errorSelector
  , initDirectoryWithFileWrappersSelector
  , initRegularFileWithContentsSelector
  , initSymbolicLinkWithDestinationURLSelector
  , initWithSerializedRepresentationSelector
  , initWithCoderSelector
  , matchesContentsOfURLSelector
  , readFromURL_options_errorSelector
  , writeToURL_options_originalContentsURL_errorSelector
  , addFileWrapperSelector
  , addRegularFileWithContents_preferredFilenameSelector
  , removeFileWrapperSelector
  , keyForFileWrapperSelector
  , initWithPathSelector
  , initSymbolicLinkWithDestinationSelector
  , needsToBeUpdatedFromPathSelector
  , updateFromPathSelector
  , writeToFile_atomically_updateFilenamesSelector
  , addFileWithPathSelector
  , addSymbolicLinkWithDestination_preferredFilenameSelector
  , symbolicLinkDestinationSelector
  , directorySelector
  , regularFileSelector
  , symbolicLinkSelector
  , preferredFilenameSelector
  , setPreferredFilenameSelector
  , filenameSelector
  , setFilenameSelector
  , fileAttributesSelector
  , setFileAttributesSelector
  , serializedRepresentationSelector
  , fileWrappersSelector
  , regularFileContentsSelector
  , symbolicLinkDestinationURLSelector

  -- * Enum types
  , NSFileWrapperReadingOptions(NSFileWrapperReadingOptions)
  , pattern NSFileWrapperReadingImmediate
  , pattern NSFileWrapperReadingWithoutMapping
  , NSFileWrapperWritingOptions(NSFileWrapperWritingOptions)
  , pattern NSFileWrapperWritingAtomic
  , pattern NSFileWrapperWritingWithNameUpdating

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

-- | @- initWithURL:options:error:@
initWithURL_options_error :: (IsNSFileWrapper nsFileWrapper, IsNSURL url, IsNSError outError) => nsFileWrapper -> url -> NSFileWrapperReadingOptions -> outError -> IO (Id NSFileWrapper)
initWithURL_options_error nsFileWrapper  url options outError =
withObjCPtr url $ \raw_url ->
  withObjCPtr outError $ \raw_outError ->
      sendMsg nsFileWrapper (mkSelector "initWithURL:options:error:") (retPtr retVoid) [argPtr (castPtr raw_url :: Ptr ()), argCULong (coerce options), argPtr (castPtr raw_outError :: Ptr ())] >>= ownedObject . castPtr

-- | @- initDirectoryWithFileWrappers:@
initDirectoryWithFileWrappers :: (IsNSFileWrapper nsFileWrapper, IsNSDictionary childrenByPreferredName) => nsFileWrapper -> childrenByPreferredName -> IO (Id NSFileWrapper)
initDirectoryWithFileWrappers nsFileWrapper  childrenByPreferredName =
withObjCPtr childrenByPreferredName $ \raw_childrenByPreferredName ->
    sendMsg nsFileWrapper (mkSelector "initDirectoryWithFileWrappers:") (retPtr retVoid) [argPtr (castPtr raw_childrenByPreferredName :: Ptr ())] >>= ownedObject . castPtr

-- | @- initRegularFileWithContents:@
initRegularFileWithContents :: (IsNSFileWrapper nsFileWrapper, IsNSData contents) => nsFileWrapper -> contents -> IO (Id NSFileWrapper)
initRegularFileWithContents nsFileWrapper  contents =
withObjCPtr contents $ \raw_contents ->
    sendMsg nsFileWrapper (mkSelector "initRegularFileWithContents:") (retPtr retVoid) [argPtr (castPtr raw_contents :: Ptr ())] >>= ownedObject . castPtr

-- | @- initSymbolicLinkWithDestinationURL:@
initSymbolicLinkWithDestinationURL :: (IsNSFileWrapper nsFileWrapper, IsNSURL url) => nsFileWrapper -> url -> IO (Id NSFileWrapper)
initSymbolicLinkWithDestinationURL nsFileWrapper  url =
withObjCPtr url $ \raw_url ->
    sendMsg nsFileWrapper (mkSelector "initSymbolicLinkWithDestinationURL:") (retPtr retVoid) [argPtr (castPtr raw_url :: Ptr ())] >>= ownedObject . castPtr

-- | @- initWithSerializedRepresentation:@
initWithSerializedRepresentation :: (IsNSFileWrapper nsFileWrapper, IsNSData serializeRepresentation) => nsFileWrapper -> serializeRepresentation -> IO (Id NSFileWrapper)
initWithSerializedRepresentation nsFileWrapper  serializeRepresentation =
withObjCPtr serializeRepresentation $ \raw_serializeRepresentation ->
    sendMsg nsFileWrapper (mkSelector "initWithSerializedRepresentation:") (retPtr retVoid) [argPtr (castPtr raw_serializeRepresentation :: Ptr ())] >>= ownedObject . castPtr

-- | @- initWithCoder:@
initWithCoder :: (IsNSFileWrapper nsFileWrapper, IsNSCoder inCoder) => nsFileWrapper -> inCoder -> IO (Id NSFileWrapper)
initWithCoder nsFileWrapper  inCoder =
withObjCPtr inCoder $ \raw_inCoder ->
    sendMsg nsFileWrapper (mkSelector "initWithCoder:") (retPtr retVoid) [argPtr (castPtr raw_inCoder :: Ptr ())] >>= ownedObject . castPtr

-- | @- matchesContentsOfURL:@
matchesContentsOfURL :: (IsNSFileWrapper nsFileWrapper, IsNSURL url) => nsFileWrapper -> url -> IO Bool
matchesContentsOfURL nsFileWrapper  url =
withObjCPtr url $ \raw_url ->
    fmap ((/= 0) :: CULong -> Bool) $ sendMsg nsFileWrapper (mkSelector "matchesContentsOfURL:") retCULong [argPtr (castPtr raw_url :: Ptr ())]

-- | @- readFromURL:options:error:@
readFromURL_options_error :: (IsNSFileWrapper nsFileWrapper, IsNSURL url, IsNSError outError) => nsFileWrapper -> url -> NSFileWrapperReadingOptions -> outError -> IO Bool
readFromURL_options_error nsFileWrapper  url options outError =
withObjCPtr url $ \raw_url ->
  withObjCPtr outError $ \raw_outError ->
      fmap ((/= 0) :: CULong -> Bool) $ sendMsg nsFileWrapper (mkSelector "readFromURL:options:error:") retCULong [argPtr (castPtr raw_url :: Ptr ()), argCULong (coerce options), argPtr (castPtr raw_outError :: Ptr ())]

-- | @- writeToURL:options:originalContentsURL:error:@
writeToURL_options_originalContentsURL_error :: (IsNSFileWrapper nsFileWrapper, IsNSURL url, IsNSURL originalContentsURL, IsNSError outError) => nsFileWrapper -> url -> NSFileWrapperWritingOptions -> originalContentsURL -> outError -> IO Bool
writeToURL_options_originalContentsURL_error nsFileWrapper  url options originalContentsURL outError =
withObjCPtr url $ \raw_url ->
  withObjCPtr originalContentsURL $ \raw_originalContentsURL ->
    withObjCPtr outError $ \raw_outError ->
        fmap ((/= 0) :: CULong -> Bool) $ sendMsg nsFileWrapper (mkSelector "writeToURL:options:originalContentsURL:error:") retCULong [argPtr (castPtr raw_url :: Ptr ()), argCULong (coerce options), argPtr (castPtr raw_originalContentsURL :: Ptr ()), argPtr (castPtr raw_outError :: Ptr ())]

-- | @- addFileWrapper:@
addFileWrapper :: (IsNSFileWrapper nsFileWrapper, IsNSFileWrapper child) => nsFileWrapper -> child -> IO (Id NSString)
addFileWrapper nsFileWrapper  child =
withObjCPtr child $ \raw_child ->
    sendMsg nsFileWrapper (mkSelector "addFileWrapper:") (retPtr retVoid) [argPtr (castPtr raw_child :: Ptr ())] >>= retainedObject . castPtr

-- | @- addRegularFileWithContents:preferredFilename:@
addRegularFileWithContents_preferredFilename :: (IsNSFileWrapper nsFileWrapper, IsNSData data_, IsNSString fileName) => nsFileWrapper -> data_ -> fileName -> IO (Id NSString)
addRegularFileWithContents_preferredFilename nsFileWrapper  data_ fileName =
withObjCPtr data_ $ \raw_data_ ->
  withObjCPtr fileName $ \raw_fileName ->
      sendMsg nsFileWrapper (mkSelector "addRegularFileWithContents:preferredFilename:") (retPtr retVoid) [argPtr (castPtr raw_data_ :: Ptr ()), argPtr (castPtr raw_fileName :: Ptr ())] >>= retainedObject . castPtr

-- | @- removeFileWrapper:@
removeFileWrapper :: (IsNSFileWrapper nsFileWrapper, IsNSFileWrapper child) => nsFileWrapper -> child -> IO ()
removeFileWrapper nsFileWrapper  child =
withObjCPtr child $ \raw_child ->
    sendMsg nsFileWrapper (mkSelector "removeFileWrapper:") retVoid [argPtr (castPtr raw_child :: Ptr ())]

-- | @- keyForFileWrapper:@
keyForFileWrapper :: (IsNSFileWrapper nsFileWrapper, IsNSFileWrapper child) => nsFileWrapper -> child -> IO (Id NSString)
keyForFileWrapper nsFileWrapper  child =
withObjCPtr child $ \raw_child ->
    sendMsg nsFileWrapper (mkSelector "keyForFileWrapper:") (retPtr retVoid) [argPtr (castPtr raw_child :: Ptr ())] >>= retainedObject . castPtr

-- | @- initWithPath:@
initWithPath :: (IsNSFileWrapper nsFileWrapper, IsNSString path) => nsFileWrapper -> path -> IO RawId
initWithPath nsFileWrapper  path =
withObjCPtr path $ \raw_path ->
    fmap (RawId . castPtr) $ sendMsg nsFileWrapper (mkSelector "initWithPath:") (retPtr retVoid) [argPtr (castPtr raw_path :: Ptr ())]

-- | @- initSymbolicLinkWithDestination:@
initSymbolicLinkWithDestination :: (IsNSFileWrapper nsFileWrapper, IsNSString path) => nsFileWrapper -> path -> IO RawId
initSymbolicLinkWithDestination nsFileWrapper  path =
withObjCPtr path $ \raw_path ->
    fmap (RawId . castPtr) $ sendMsg nsFileWrapper (mkSelector "initSymbolicLinkWithDestination:") (retPtr retVoid) [argPtr (castPtr raw_path :: Ptr ())]

-- | @- needsToBeUpdatedFromPath:@
needsToBeUpdatedFromPath :: (IsNSFileWrapper nsFileWrapper, IsNSString path) => nsFileWrapper -> path -> IO Bool
needsToBeUpdatedFromPath nsFileWrapper  path =
withObjCPtr path $ \raw_path ->
    fmap ((/= 0) :: CULong -> Bool) $ sendMsg nsFileWrapper (mkSelector "needsToBeUpdatedFromPath:") retCULong [argPtr (castPtr raw_path :: Ptr ())]

-- | @- updateFromPath:@
updateFromPath :: (IsNSFileWrapper nsFileWrapper, IsNSString path) => nsFileWrapper -> path -> IO Bool
updateFromPath nsFileWrapper  path =
withObjCPtr path $ \raw_path ->
    fmap ((/= 0) :: CULong -> Bool) $ sendMsg nsFileWrapper (mkSelector "updateFromPath:") retCULong [argPtr (castPtr raw_path :: Ptr ())]

-- | @- writeToFile:atomically:updateFilenames:@
writeToFile_atomically_updateFilenames :: (IsNSFileWrapper nsFileWrapper, IsNSString path) => nsFileWrapper -> path -> Bool -> Bool -> IO Bool
writeToFile_atomically_updateFilenames nsFileWrapper  path atomicFlag updateFilenamesFlag =
withObjCPtr path $ \raw_path ->
    fmap ((/= 0) :: CULong -> Bool) $ sendMsg nsFileWrapper (mkSelector "writeToFile:atomically:updateFilenames:") retCULong [argPtr (castPtr raw_path :: Ptr ()), argCULong (if atomicFlag then 1 else 0), argCULong (if updateFilenamesFlag then 1 else 0)]

-- | @- addFileWithPath:@
addFileWithPath :: (IsNSFileWrapper nsFileWrapper, IsNSString path) => nsFileWrapper -> path -> IO (Id NSString)
addFileWithPath nsFileWrapper  path =
withObjCPtr path $ \raw_path ->
    sendMsg nsFileWrapper (mkSelector "addFileWithPath:") (retPtr retVoid) [argPtr (castPtr raw_path :: Ptr ())] >>= retainedObject . castPtr

-- | @- addSymbolicLinkWithDestination:preferredFilename:@
addSymbolicLinkWithDestination_preferredFilename :: (IsNSFileWrapper nsFileWrapper, IsNSString path, IsNSString filename) => nsFileWrapper -> path -> filename -> IO (Id NSString)
addSymbolicLinkWithDestination_preferredFilename nsFileWrapper  path filename =
withObjCPtr path $ \raw_path ->
  withObjCPtr filename $ \raw_filename ->
      sendMsg nsFileWrapper (mkSelector "addSymbolicLinkWithDestination:preferredFilename:") (retPtr retVoid) [argPtr (castPtr raw_path :: Ptr ()), argPtr (castPtr raw_filename :: Ptr ())] >>= retainedObject . castPtr

-- | @- symbolicLinkDestination@
symbolicLinkDestination :: IsNSFileWrapper nsFileWrapper => nsFileWrapper -> IO (Id NSString)
symbolicLinkDestination nsFileWrapper  =
  sendMsg nsFileWrapper (mkSelector "symbolicLinkDestination") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- directory@
directory :: IsNSFileWrapper nsFileWrapper => nsFileWrapper -> IO Bool
directory nsFileWrapper  =
  fmap ((/= 0) :: CULong -> Bool) $ sendMsg nsFileWrapper (mkSelector "directory") retCULong []

-- | @- regularFile@
regularFile :: IsNSFileWrapper nsFileWrapper => nsFileWrapper -> IO Bool
regularFile nsFileWrapper  =
  fmap ((/= 0) :: CULong -> Bool) $ sendMsg nsFileWrapper (mkSelector "regularFile") retCULong []

-- | @- symbolicLink@
symbolicLink :: IsNSFileWrapper nsFileWrapper => nsFileWrapper -> IO Bool
symbolicLink nsFileWrapper  =
  fmap ((/= 0) :: CULong -> Bool) $ sendMsg nsFileWrapper (mkSelector "symbolicLink") retCULong []

-- | @- preferredFilename@
preferredFilename :: IsNSFileWrapper nsFileWrapper => nsFileWrapper -> IO (Id NSString)
preferredFilename nsFileWrapper  =
  sendMsg nsFileWrapper (mkSelector "preferredFilename") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- setPreferredFilename:@
setPreferredFilename :: (IsNSFileWrapper nsFileWrapper, IsNSString value) => nsFileWrapper -> value -> IO ()
setPreferredFilename nsFileWrapper  value =
withObjCPtr value $ \raw_value ->
    sendMsg nsFileWrapper (mkSelector "setPreferredFilename:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- | @- filename@
filename :: IsNSFileWrapper nsFileWrapper => nsFileWrapper -> IO (Id NSString)
filename nsFileWrapper  =
  sendMsg nsFileWrapper (mkSelector "filename") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- setFilename:@
setFilename :: (IsNSFileWrapper nsFileWrapper, IsNSString value) => nsFileWrapper -> value -> IO ()
setFilename nsFileWrapper  value =
withObjCPtr value $ \raw_value ->
    sendMsg nsFileWrapper (mkSelector "setFilename:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- | @- fileAttributes@
fileAttributes :: IsNSFileWrapper nsFileWrapper => nsFileWrapper -> IO (Id NSDictionary)
fileAttributes nsFileWrapper  =
  sendMsg nsFileWrapper (mkSelector "fileAttributes") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- setFileAttributes:@
setFileAttributes :: (IsNSFileWrapper nsFileWrapper, IsNSDictionary value) => nsFileWrapper -> value -> IO ()
setFileAttributes nsFileWrapper  value =
withObjCPtr value $ \raw_value ->
    sendMsg nsFileWrapper (mkSelector "setFileAttributes:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- | @- serializedRepresentation@
serializedRepresentation :: IsNSFileWrapper nsFileWrapper => nsFileWrapper -> IO (Id NSData)
serializedRepresentation nsFileWrapper  =
  sendMsg nsFileWrapper (mkSelector "serializedRepresentation") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- fileWrappers@
fileWrappers :: IsNSFileWrapper nsFileWrapper => nsFileWrapper -> IO (Id NSDictionary)
fileWrappers nsFileWrapper  =
  sendMsg nsFileWrapper (mkSelector "fileWrappers") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- regularFileContents@
regularFileContents :: IsNSFileWrapper nsFileWrapper => nsFileWrapper -> IO (Id NSData)
regularFileContents nsFileWrapper  =
  sendMsg nsFileWrapper (mkSelector "regularFileContents") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- symbolicLinkDestinationURL@
symbolicLinkDestinationURL :: IsNSFileWrapper nsFileWrapper => nsFileWrapper -> IO (Id NSURL)
symbolicLinkDestinationURL nsFileWrapper  =
  sendMsg nsFileWrapper (mkSelector "symbolicLinkDestinationURL") (retPtr retVoid) [] >>= retainedObject . castPtr

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @initWithURL:options:error:@
initWithURL_options_errorSelector :: Selector
initWithURL_options_errorSelector = mkSelector "initWithURL:options:error:"

-- | @Selector@ for @initDirectoryWithFileWrappers:@
initDirectoryWithFileWrappersSelector :: Selector
initDirectoryWithFileWrappersSelector = mkSelector "initDirectoryWithFileWrappers:"

-- | @Selector@ for @initRegularFileWithContents:@
initRegularFileWithContentsSelector :: Selector
initRegularFileWithContentsSelector = mkSelector "initRegularFileWithContents:"

-- | @Selector@ for @initSymbolicLinkWithDestinationURL:@
initSymbolicLinkWithDestinationURLSelector :: Selector
initSymbolicLinkWithDestinationURLSelector = mkSelector "initSymbolicLinkWithDestinationURL:"

-- | @Selector@ for @initWithSerializedRepresentation:@
initWithSerializedRepresentationSelector :: Selector
initWithSerializedRepresentationSelector = mkSelector "initWithSerializedRepresentation:"

-- | @Selector@ for @initWithCoder:@
initWithCoderSelector :: Selector
initWithCoderSelector = mkSelector "initWithCoder:"

-- | @Selector@ for @matchesContentsOfURL:@
matchesContentsOfURLSelector :: Selector
matchesContentsOfURLSelector = mkSelector "matchesContentsOfURL:"

-- | @Selector@ for @readFromURL:options:error:@
readFromURL_options_errorSelector :: Selector
readFromURL_options_errorSelector = mkSelector "readFromURL:options:error:"

-- | @Selector@ for @writeToURL:options:originalContentsURL:error:@
writeToURL_options_originalContentsURL_errorSelector :: Selector
writeToURL_options_originalContentsURL_errorSelector = mkSelector "writeToURL:options:originalContentsURL:error:"

-- | @Selector@ for @addFileWrapper:@
addFileWrapperSelector :: Selector
addFileWrapperSelector = mkSelector "addFileWrapper:"

-- | @Selector@ for @addRegularFileWithContents:preferredFilename:@
addRegularFileWithContents_preferredFilenameSelector :: Selector
addRegularFileWithContents_preferredFilenameSelector = mkSelector "addRegularFileWithContents:preferredFilename:"

-- | @Selector@ for @removeFileWrapper:@
removeFileWrapperSelector :: Selector
removeFileWrapperSelector = mkSelector "removeFileWrapper:"

-- | @Selector@ for @keyForFileWrapper:@
keyForFileWrapperSelector :: Selector
keyForFileWrapperSelector = mkSelector "keyForFileWrapper:"

-- | @Selector@ for @initWithPath:@
initWithPathSelector :: Selector
initWithPathSelector = mkSelector "initWithPath:"

-- | @Selector@ for @initSymbolicLinkWithDestination:@
initSymbolicLinkWithDestinationSelector :: Selector
initSymbolicLinkWithDestinationSelector = mkSelector "initSymbolicLinkWithDestination:"

-- | @Selector@ for @needsToBeUpdatedFromPath:@
needsToBeUpdatedFromPathSelector :: Selector
needsToBeUpdatedFromPathSelector = mkSelector "needsToBeUpdatedFromPath:"

-- | @Selector@ for @updateFromPath:@
updateFromPathSelector :: Selector
updateFromPathSelector = mkSelector "updateFromPath:"

-- | @Selector@ for @writeToFile:atomically:updateFilenames:@
writeToFile_atomically_updateFilenamesSelector :: Selector
writeToFile_atomically_updateFilenamesSelector = mkSelector "writeToFile:atomically:updateFilenames:"

-- | @Selector@ for @addFileWithPath:@
addFileWithPathSelector :: Selector
addFileWithPathSelector = mkSelector "addFileWithPath:"

-- | @Selector@ for @addSymbolicLinkWithDestination:preferredFilename:@
addSymbolicLinkWithDestination_preferredFilenameSelector :: Selector
addSymbolicLinkWithDestination_preferredFilenameSelector = mkSelector "addSymbolicLinkWithDestination:preferredFilename:"

-- | @Selector@ for @symbolicLinkDestination@
symbolicLinkDestinationSelector :: Selector
symbolicLinkDestinationSelector = mkSelector "symbolicLinkDestination"

-- | @Selector@ for @directory@
directorySelector :: Selector
directorySelector = mkSelector "directory"

-- | @Selector@ for @regularFile@
regularFileSelector :: Selector
regularFileSelector = mkSelector "regularFile"

-- | @Selector@ for @symbolicLink@
symbolicLinkSelector :: Selector
symbolicLinkSelector = mkSelector "symbolicLink"

-- | @Selector@ for @preferredFilename@
preferredFilenameSelector :: Selector
preferredFilenameSelector = mkSelector "preferredFilename"

-- | @Selector@ for @setPreferredFilename:@
setPreferredFilenameSelector :: Selector
setPreferredFilenameSelector = mkSelector "setPreferredFilename:"

-- | @Selector@ for @filename@
filenameSelector :: Selector
filenameSelector = mkSelector "filename"

-- | @Selector@ for @setFilename:@
setFilenameSelector :: Selector
setFilenameSelector = mkSelector "setFilename:"

-- | @Selector@ for @fileAttributes@
fileAttributesSelector :: Selector
fileAttributesSelector = mkSelector "fileAttributes"

-- | @Selector@ for @setFileAttributes:@
setFileAttributesSelector :: Selector
setFileAttributesSelector = mkSelector "setFileAttributes:"

-- | @Selector@ for @serializedRepresentation@
serializedRepresentationSelector :: Selector
serializedRepresentationSelector = mkSelector "serializedRepresentation"

-- | @Selector@ for @fileWrappers@
fileWrappersSelector :: Selector
fileWrappersSelector = mkSelector "fileWrappers"

-- | @Selector@ for @regularFileContents@
regularFileContentsSelector :: Selector
regularFileContentsSelector = mkSelector "regularFileContents"

-- | @Selector@ for @symbolicLinkDestinationURL@
symbolicLinkDestinationURLSelector :: Selector
symbolicLinkDestinationURLSelector = mkSelector "symbolicLinkDestinationURL"

