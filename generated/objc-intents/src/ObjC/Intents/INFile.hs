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
  , fileWithData_filename_typeIdentifierSelector
  , fileWithFileURL_filename_typeIdentifierSelector
  , dataSelector
  , filenameSelector
  , setFilenameSelector
  , typeIdentifierSelector
  , fileURLSelector
  , removedOnCompletionSelector
  , setRemovedOnCompletionSelector


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

import ObjC.Intents.Internal.Classes
import ObjC.Foundation.Internal.Classes

-- | @+ fileWithData:filename:typeIdentifier:@
fileWithData_filename_typeIdentifier :: (IsNSData data_, IsNSString filename, IsNSString typeIdentifier) => data_ -> filename -> typeIdentifier -> IO (Id INFile)
fileWithData_filename_typeIdentifier data_ filename typeIdentifier =
  do
    cls' <- getRequiredClass "INFile"
    withObjCPtr data_ $ \raw_data_ ->
      withObjCPtr filename $ \raw_filename ->
        withObjCPtr typeIdentifier $ \raw_typeIdentifier ->
          sendClassMsg cls' (mkSelector "fileWithData:filename:typeIdentifier:") (retPtr retVoid) [argPtr (castPtr raw_data_ :: Ptr ()), argPtr (castPtr raw_filename :: Ptr ()), argPtr (castPtr raw_typeIdentifier :: Ptr ())] >>= retainedObject . castPtr

-- | @+ fileWithFileURL:filename:typeIdentifier:@
fileWithFileURL_filename_typeIdentifier :: (IsNSURL fileURL, IsNSString filename, IsNSString typeIdentifier) => fileURL -> filename -> typeIdentifier -> IO (Id INFile)
fileWithFileURL_filename_typeIdentifier fileURL filename typeIdentifier =
  do
    cls' <- getRequiredClass "INFile"
    withObjCPtr fileURL $ \raw_fileURL ->
      withObjCPtr filename $ \raw_filename ->
        withObjCPtr typeIdentifier $ \raw_typeIdentifier ->
          sendClassMsg cls' (mkSelector "fileWithFileURL:filename:typeIdentifier:") (retPtr retVoid) [argPtr (castPtr raw_fileURL :: Ptr ()), argPtr (castPtr raw_filename :: Ptr ()), argPtr (castPtr raw_typeIdentifier :: Ptr ())] >>= retainedObject . castPtr

-- | The contents of the file. If the file was created with a URL, accessing this property will memory map the file contents.
--
-- ObjC selector: @- data@
data_ :: IsINFile inFile => inFile -> IO (Id NSData)
data_ inFile  =
  sendMsg inFile (mkSelector "data") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | The human-readable name of the file, which will be displayed to the user.
--
-- ObjC selector: @- filename@
filename :: IsINFile inFile => inFile -> IO (Id NSString)
filename inFile  =
  sendMsg inFile (mkSelector "filename") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | The human-readable name of the file, which will be displayed to the user.
--
-- ObjC selector: @- setFilename:@
setFilename :: (IsINFile inFile, IsNSString value) => inFile -> value -> IO ()
setFilename inFile  value =
withObjCPtr value $ \raw_value ->
    sendMsg inFile (mkSelector "setFilename:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- | The uniform type identifier of the file. (i.e. "public.json", "public.png", or any custom type) More information about uniform type identifiers can be found in <CoreServices/UTCoreTypes.h>
--
-- ObjC selector: @- typeIdentifier@
typeIdentifier :: IsINFile inFile => inFile -> IO (Id NSString)
typeIdentifier inFile  =
  sendMsg inFile (mkSelector "typeIdentifier") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | URL to the file on disk, if any. If the file isn't stored on disk, access the contents using the @data@ property.
--
-- If the file was created elsewhere on the system, make sure to surround access to file contents with @-[NSURL startAccessingSecurityScopedResource]@ and @-[NSURL stopAccessingSecurityScopedResource]@.
--
-- ObjC selector: @- fileURL@
fileURL :: IsINFile inFile => inFile -> IO (Id NSURL)
fileURL inFile  =
  sendMsg inFile (mkSelector "fileURL") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | Indicates whether the file should be automatically deleted from disk when the Shortcut is done running. @false@ by default.
--
-- ObjC selector: @- removedOnCompletion@
removedOnCompletion :: IsINFile inFile => inFile -> IO Bool
removedOnCompletion inFile  =
  fmap ((/= 0) :: CULong -> Bool) $ sendMsg inFile (mkSelector "removedOnCompletion") retCULong []

-- | Indicates whether the file should be automatically deleted from disk when the Shortcut is done running. @false@ by default.
--
-- ObjC selector: @- setRemovedOnCompletion:@
setRemovedOnCompletion :: IsINFile inFile => inFile -> Bool -> IO ()
setRemovedOnCompletion inFile  value =
  sendMsg inFile (mkSelector "setRemovedOnCompletion:") retVoid [argCULong (if value then 1 else 0)]

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @fileWithData:filename:typeIdentifier:@
fileWithData_filename_typeIdentifierSelector :: Selector
fileWithData_filename_typeIdentifierSelector = mkSelector "fileWithData:filename:typeIdentifier:"

-- | @Selector@ for @fileWithFileURL:filename:typeIdentifier:@
fileWithFileURL_filename_typeIdentifierSelector :: Selector
fileWithFileURL_filename_typeIdentifierSelector = mkSelector "fileWithFileURL:filename:typeIdentifier:"

-- | @Selector@ for @data@
dataSelector :: Selector
dataSelector = mkSelector "data"

-- | @Selector@ for @filename@
filenameSelector :: Selector
filenameSelector = mkSelector "filename"

-- | @Selector@ for @setFilename:@
setFilenameSelector :: Selector
setFilenameSelector = mkSelector "setFilename:"

-- | @Selector@ for @typeIdentifier@
typeIdentifierSelector :: Selector
typeIdentifierSelector = mkSelector "typeIdentifier"

-- | @Selector@ for @fileURL@
fileURLSelector :: Selector
fileURLSelector = mkSelector "fileURL"

-- | @Selector@ for @removedOnCompletion@
removedOnCompletionSelector :: Selector
removedOnCompletionSelector = mkSelector "removedOnCompletion"

-- | @Selector@ for @setRemovedOnCompletion:@
setRemovedOnCompletionSelector :: Selector
setRemovedOnCompletionSelector = mkSelector "setRemovedOnCompletion:"

