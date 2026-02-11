{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | An object for storing the reference signatures for custom audio recordings and their associated metadata.
--
-- Create a custom catalog by adding reference signatures that you generate from audio that you provide. You also add the associated metadata for each signature. Save your custom catalog and share it with others. You can also load a saved catalog.
--
-- Generated bindings for @SHCustomCatalog@.
module ObjC.ShazamKit.SHCustomCatalog
  ( SHCustomCatalog
  , IsSHCustomCatalog(..)
  , addReferenceSignature_representingMediaItems_error
  , addCustomCatalogFromURL_error
  , writeToURL_error
  , new
  , init_
  , initWithDataRepresentation_error
  , dataRepresentation
  , addReferenceSignature_representingMediaItems_errorSelector
  , addCustomCatalogFromURL_errorSelector
  , writeToURL_errorSelector
  , newSelector
  , initSelector
  , initWithDataRepresentation_errorSelector
  , dataRepresentationSelector


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

import ObjC.ShazamKit.Internal.Classes
import ObjC.Foundation.Internal.Classes

-- | Adds a reference signature and its associated metadata to a catalog.
--
-- > Note: > This system ignores calls to @addReferenceSignature(_:representing:)@ after adding the catalog to an @SHSession@.
--
-- - Parameters:   - signature: The reference signature for the audio recording.   - mediaItems: The metadata for the recording.
--
-- ObjC selector: @- addReferenceSignature:representingMediaItems:error:@
addReferenceSignature_representingMediaItems_error :: (IsSHCustomCatalog shCustomCatalog, IsSHSignature signature, IsNSArray mediaItems, IsNSError error_) => shCustomCatalog -> signature -> mediaItems -> error_ -> IO Bool
addReferenceSignature_representingMediaItems_error shCustomCatalog  signature mediaItems error_ =
withObjCPtr signature $ \raw_signature ->
  withObjCPtr mediaItems $ \raw_mediaItems ->
    withObjCPtr error_ $ \raw_error_ ->
        fmap ((/= 0) :: CULong -> Bool) $ sendMsg shCustomCatalog (mkSelector "addReferenceSignature:representingMediaItems:error:") retCULong [argPtr (castPtr raw_signature :: Ptr ()), argPtr (castPtr raw_mediaItems :: Ptr ()), argPtr (castPtr raw_error_ :: Ptr ())]

-- | Loads a saved custom catalog from a file.
--
-- - Parameters:  - customCatalogURL: The file URL for a custom catalog.  - error: An output value in Objective-C that indicates the type of error; otherwise, @nil@.
--
-- ObjC selector: @- addCustomCatalogFromURL:error:@
addCustomCatalogFromURL_error :: (IsSHCustomCatalog shCustomCatalog, IsNSURL customCatalogURL, IsNSError error_) => shCustomCatalog -> customCatalogURL -> error_ -> IO Bool
addCustomCatalogFromURL_error shCustomCatalog  customCatalogURL error_ =
withObjCPtr customCatalogURL $ \raw_customCatalogURL ->
  withObjCPtr error_ $ \raw_error_ ->
      fmap ((/= 0) :: CULong -> Bool) $ sendMsg shCustomCatalog (mkSelector "addCustomCatalogFromURL:error:") retCULong [argPtr (castPtr raw_customCatalogURL :: Ptr ()), argPtr (castPtr raw_error_ :: Ptr ())]

-- | Saves the custom catalog to a local file.
--
-- If @destinationURL@ is a directory, the system creates a @Signatures.shazamcatalog@ file.
--
-- - Parameters:   - destinationURL: A URL for the saved custom catalog file.   - error: An output value in Objective-C that indicates the type of error; otherwise, @nil@.
--
-- - Returns: @YES@ if the catalog writes to the file; otherwise, @NO@.
--
-- ObjC selector: @- writeToURL:error:@
writeToURL_error :: (IsSHCustomCatalog shCustomCatalog, IsNSURL destinationURL, IsNSError error_) => shCustomCatalog -> destinationURL -> error_ -> IO Bool
writeToURL_error shCustomCatalog  destinationURL error_ =
withObjCPtr destinationURL $ \raw_destinationURL ->
  withObjCPtr error_ $ \raw_error_ ->
      fmap ((/= 0) :: CULong -> Bool) $ sendMsg shCustomCatalog (mkSelector "writeToURL:error:") retCULong [argPtr (castPtr raw_destinationURL :: Ptr ()), argPtr (castPtr raw_error_ :: Ptr ())]

-- | Creates a new custom catalog object for storing reference audio signatures and their associated metadata.
--
-- - Returns: A new custom catalog for storing processed reference audio recordings and their associated metadata.
--
-- ObjC selector: @+ new@
new :: IO (Id SHCustomCatalog)
new  =
  do
    cls' <- getRequiredClass "SHCustomCatalog"
    sendClassMsg cls' (mkSelector "new") (retPtr retVoid) [] >>= ownedObject . castPtr

-- | Creates a new custom catalog object for storing reference audio signatures and their associated metadata.
--
-- ObjC selector: @- init@
init_ :: IsSHCustomCatalog shCustomCatalog => shCustomCatalog -> IO (Id SHCustomCatalog)
init_ shCustomCatalog  =
  sendMsg shCustomCatalog (mkSelector "init") (retPtr retVoid) [] >>= ownedObject . castPtr

-- | Load a @SHCustomCatalog@ from data
--
-- @dataRepresentation@ — The data representation of the @SHCustomCatalog@
--
-- @error@ — Error populated if not a valid data representation
--
-- ObjC selector: @- initWithDataRepresentation:error:@
initWithDataRepresentation_error :: (IsSHCustomCatalog shCustomCatalog, IsNSData dataRepresentation, IsNSError error_) => shCustomCatalog -> dataRepresentation -> error_ -> IO (Id SHCustomCatalog)
initWithDataRepresentation_error shCustomCatalog  dataRepresentation error_ =
withObjCPtr dataRepresentation $ \raw_dataRepresentation ->
  withObjCPtr error_ $ \raw_error_ ->
      sendMsg shCustomCatalog (mkSelector "initWithDataRepresentation:error:") (retPtr retVoid) [argPtr (castPtr raw_dataRepresentation :: Ptr ()), argPtr (castPtr raw_error_ :: Ptr ())] >>= ownedObject . castPtr

-- | The data representation of this file, it can be written to disk
--
-- ObjC selector: @- dataRepresentation@
dataRepresentation :: IsSHCustomCatalog shCustomCatalog => shCustomCatalog -> IO (Id NSData)
dataRepresentation shCustomCatalog  =
  sendMsg shCustomCatalog (mkSelector "dataRepresentation") (retPtr retVoid) [] >>= retainedObject . castPtr

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @addReferenceSignature:representingMediaItems:error:@
addReferenceSignature_representingMediaItems_errorSelector :: Selector
addReferenceSignature_representingMediaItems_errorSelector = mkSelector "addReferenceSignature:representingMediaItems:error:"

-- | @Selector@ for @addCustomCatalogFromURL:error:@
addCustomCatalogFromURL_errorSelector :: Selector
addCustomCatalogFromURL_errorSelector = mkSelector "addCustomCatalogFromURL:error:"

-- | @Selector@ for @writeToURL:error:@
writeToURL_errorSelector :: Selector
writeToURL_errorSelector = mkSelector "writeToURL:error:"

-- | @Selector@ for @new@
newSelector :: Selector
newSelector = mkSelector "new"

-- | @Selector@ for @init@
initSelector :: Selector
initSelector = mkSelector "init"

-- | @Selector@ for @initWithDataRepresentation:error:@
initWithDataRepresentation_errorSelector :: Selector
initWithDataRepresentation_errorSelector = mkSelector "initWithDataRepresentation:error:"

-- | @Selector@ for @dataRepresentation@
dataRepresentationSelector :: Selector
dataRepresentationSelector = mkSelector "dataRepresentation"

