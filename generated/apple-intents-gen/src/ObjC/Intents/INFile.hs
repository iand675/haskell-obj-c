{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | An object that describes a piece of data and its associated name and uniform type identifier. This data can either be stored in a file on disk, or in memory.
--
-- Generated bindings for @INFile@.
module ObjC.Intents.INFile
  ( INFile
  , IsINFile(..)
  , fileWithData_filename_typeIdentifier
  , fileWithFileURL_filename_typeIdentifier
  , data_
  , filename
  , setFilename
  , typeIdentifier
  , fileURL
  , removedOnCompletion
  , setRemovedOnCompletion
  , dataSelector
  , fileURLSelector
  , fileWithData_filename_typeIdentifierSelector
  , fileWithFileURL_filename_typeIdentifierSelector
  , filenameSelector
  , removedOnCompletionSelector
  , setFilenameSelector
  , setRemovedOnCompletionSelector
  , typeIdentifierSelector


  ) where

import Foreign.Ptr (Ptr, FunPtr)
import Foreign.C.Types

import ObjC.Runtime.Types
import ObjC.Runtime.Message (sendMessage, sendOwnedMessage, sendClassMessage, sendOwnedClassMessage)
import ObjC.Runtime.Selector (mkSelector)
import ObjC.Runtime.Class (getRequiredClass)

import ObjC.Intents.Internal.Classes
import ObjC.Foundation.Internal.Classes

-- | @+ fileWithData:filename:typeIdentifier:@
fileWithData_filename_typeIdentifier :: (IsNSData data_, IsNSString filename, IsNSString typeIdentifier) => data_ -> filename -> typeIdentifier -> IO (Id INFile)
fileWithData_filename_typeIdentifier data_ filename typeIdentifier =
  do
    cls' <- getRequiredClass "INFile"
    sendClassMessage cls' fileWithData_filename_typeIdentifierSelector (toNSData data_) (toNSString filename) (toNSString typeIdentifier)

-- | @+ fileWithFileURL:filename:typeIdentifier:@
fileWithFileURL_filename_typeIdentifier :: (IsNSURL fileURL, IsNSString filename, IsNSString typeIdentifier) => fileURL -> filename -> typeIdentifier -> IO (Id INFile)
fileWithFileURL_filename_typeIdentifier fileURL filename typeIdentifier =
  do
    cls' <- getRequiredClass "INFile"
    sendClassMessage cls' fileWithFileURL_filename_typeIdentifierSelector (toNSURL fileURL) (toNSString filename) (toNSString typeIdentifier)

-- | The contents of the file. If the file was created with a URL, accessing this property will memory map the file contents.
--
-- ObjC selector: @- data@
data_ :: IsINFile inFile => inFile -> IO (Id NSData)
data_ inFile =
  sendMessage inFile dataSelector

-- | The human-readable name of the file, which will be displayed to the user.
--
-- ObjC selector: @- filename@
filename :: IsINFile inFile => inFile -> IO (Id NSString)
filename inFile =
  sendMessage inFile filenameSelector

-- | The human-readable name of the file, which will be displayed to the user.
--
-- ObjC selector: @- setFilename:@
setFilename :: (IsINFile inFile, IsNSString value) => inFile -> value -> IO ()
setFilename inFile value =
  sendMessage inFile setFilenameSelector (toNSString value)

-- | The uniform type identifier of the file. (i.e. "public.json", "public.png", or any custom type) More information about uniform type identifiers can be found in <CoreServices/UTCoreTypes.h>
--
-- ObjC selector: @- typeIdentifier@
typeIdentifier :: IsINFile inFile => inFile -> IO (Id NSString)
typeIdentifier inFile =
  sendMessage inFile typeIdentifierSelector

-- | URL to the file on disk, if any. If the file isn't stored on disk, access the contents using the @data@ property.
--
-- If the file was created elsewhere on the system, make sure to surround access to file contents with @-[NSURL startAccessingSecurityScopedResource]@ and @-[NSURL stopAccessingSecurityScopedResource]@.
--
-- ObjC selector: @- fileURL@
fileURL :: IsINFile inFile => inFile -> IO (Id NSURL)
fileURL inFile =
  sendMessage inFile fileURLSelector

-- | Indicates whether the file should be automatically deleted from disk when the Shortcut is done running. @false@ by default.
--
-- ObjC selector: @- removedOnCompletion@
removedOnCompletion :: IsINFile inFile => inFile -> IO Bool
removedOnCompletion inFile =
  sendMessage inFile removedOnCompletionSelector

-- | Indicates whether the file should be automatically deleted from disk when the Shortcut is done running. @false@ by default.
--
-- ObjC selector: @- setRemovedOnCompletion:@
setRemovedOnCompletion :: IsINFile inFile => inFile -> Bool -> IO ()
setRemovedOnCompletion inFile value =
  sendMessage inFile setRemovedOnCompletionSelector value

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @fileWithData:filename:typeIdentifier:@
fileWithData_filename_typeIdentifierSelector :: Selector '[Id NSData, Id NSString, Id NSString] (Id INFile)
fileWithData_filename_typeIdentifierSelector = mkSelector "fileWithData:filename:typeIdentifier:"

-- | @Selector@ for @fileWithFileURL:filename:typeIdentifier:@
fileWithFileURL_filename_typeIdentifierSelector :: Selector '[Id NSURL, Id NSString, Id NSString] (Id INFile)
fileWithFileURL_filename_typeIdentifierSelector = mkSelector "fileWithFileURL:filename:typeIdentifier:"

-- | @Selector@ for @data@
dataSelector :: Selector '[] (Id NSData)
dataSelector = mkSelector "data"

-- | @Selector@ for @filename@
filenameSelector :: Selector '[] (Id NSString)
filenameSelector = mkSelector "filename"

-- | @Selector@ for @setFilename:@
setFilenameSelector :: Selector '[Id NSString] ()
setFilenameSelector = mkSelector "setFilename:"

-- | @Selector@ for @typeIdentifier@
typeIdentifierSelector :: Selector '[] (Id NSString)
typeIdentifierSelector = mkSelector "typeIdentifier"

-- | @Selector@ for @fileURL@
fileURLSelector :: Selector '[] (Id NSURL)
fileURLSelector = mkSelector "fileURL"

-- | @Selector@ for @removedOnCompletion@
removedOnCompletionSelector :: Selector '[] Bool
removedOnCompletionSelector = mkSelector "removedOnCompletion"

-- | @Selector@ for @setRemovedOnCompletion:@
setRemovedOnCompletionSelector :: Selector '[Bool] ()
setRemovedOnCompletionSelector = mkSelector "setRemovedOnCompletion:"

