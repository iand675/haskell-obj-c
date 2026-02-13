{-# LANGUAGE DataKinds #-}
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
  , addCustomCatalogFromURL_errorSelector
  , addReferenceSignature_representingMediaItems_errorSelector
  , dataRepresentationSelector
  , initSelector
  , initWithDataRepresentation_errorSelector
  , newSelector
  , writeToURL_errorSelector


  ) where

import Foreign.Ptr (Ptr, FunPtr)
import Foreign.C.Types

import ObjC.Runtime.Types
import ObjC.Runtime.Message (sendMessage, sendOwnedMessage, sendClassMessage, sendOwnedClassMessage)
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
addReferenceSignature_representingMediaItems_error shCustomCatalog signature mediaItems error_ =
  sendMessage shCustomCatalog addReferenceSignature_representingMediaItems_errorSelector (toSHSignature signature) (toNSArray mediaItems) (toNSError error_)

-- | Loads a saved custom catalog from a file.
--
-- - Parameters:  - customCatalogURL: The file URL for a custom catalog.  - error: An output value in Objective-C that indicates the type of error; otherwise, @nil@.
--
-- ObjC selector: @- addCustomCatalogFromURL:error:@
addCustomCatalogFromURL_error :: (IsSHCustomCatalog shCustomCatalog, IsNSURL customCatalogURL, IsNSError error_) => shCustomCatalog -> customCatalogURL -> error_ -> IO Bool
addCustomCatalogFromURL_error shCustomCatalog customCatalogURL error_ =
  sendMessage shCustomCatalog addCustomCatalogFromURL_errorSelector (toNSURL customCatalogURL) (toNSError error_)

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
writeToURL_error shCustomCatalog destinationURL error_ =
  sendMessage shCustomCatalog writeToURL_errorSelector (toNSURL destinationURL) (toNSError error_)

-- | Creates a new custom catalog object for storing reference audio signatures and their associated metadata.
--
-- - Returns: A new custom catalog for storing processed reference audio recordings and their associated metadata.
--
-- ObjC selector: @+ new@
new :: IO (Id SHCustomCatalog)
new  =
  do
    cls' <- getRequiredClass "SHCustomCatalog"
    sendOwnedClassMessage cls' newSelector

-- | Creates a new custom catalog object for storing reference audio signatures and their associated metadata.
--
-- ObjC selector: @- init@
init_ :: IsSHCustomCatalog shCustomCatalog => shCustomCatalog -> IO (Id SHCustomCatalog)
init_ shCustomCatalog =
  sendOwnedMessage shCustomCatalog initSelector

-- | Load a @SHCustomCatalog@ from data
--
-- @dataRepresentation@ — The data representation of the @SHCustomCatalog@
--
-- @error@ — Error populated if not a valid data representation
--
-- ObjC selector: @- initWithDataRepresentation:error:@
initWithDataRepresentation_error :: (IsSHCustomCatalog shCustomCatalog, IsNSData dataRepresentation, IsNSError error_) => shCustomCatalog -> dataRepresentation -> error_ -> IO (Id SHCustomCatalog)
initWithDataRepresentation_error shCustomCatalog dataRepresentation error_ =
  sendOwnedMessage shCustomCatalog initWithDataRepresentation_errorSelector (toNSData dataRepresentation) (toNSError error_)

-- | The data representation of this file, it can be written to disk
--
-- ObjC selector: @- dataRepresentation@
dataRepresentation :: IsSHCustomCatalog shCustomCatalog => shCustomCatalog -> IO (Id NSData)
dataRepresentation shCustomCatalog =
  sendMessage shCustomCatalog dataRepresentationSelector

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @addReferenceSignature:representingMediaItems:error:@
addReferenceSignature_representingMediaItems_errorSelector :: Selector '[Id SHSignature, Id NSArray, Id NSError] Bool
addReferenceSignature_representingMediaItems_errorSelector = mkSelector "addReferenceSignature:representingMediaItems:error:"

-- | @Selector@ for @addCustomCatalogFromURL:error:@
addCustomCatalogFromURL_errorSelector :: Selector '[Id NSURL, Id NSError] Bool
addCustomCatalogFromURL_errorSelector = mkSelector "addCustomCatalogFromURL:error:"

-- | @Selector@ for @writeToURL:error:@
writeToURL_errorSelector :: Selector '[Id NSURL, Id NSError] Bool
writeToURL_errorSelector = mkSelector "writeToURL:error:"

-- | @Selector@ for @new@
newSelector :: Selector '[] (Id SHCustomCatalog)
newSelector = mkSelector "new"

-- | @Selector@ for @init@
initSelector :: Selector '[] (Id SHCustomCatalog)
initSelector = mkSelector "init"

-- | @Selector@ for @initWithDataRepresentation:error:@
initWithDataRepresentation_errorSelector :: Selector '[Id NSData, Id NSError] (Id SHCustomCatalog)
initWithDataRepresentation_errorSelector = mkSelector "initWithDataRepresentation:error:"

-- | @Selector@ for @dataRepresentation@
dataRepresentationSelector :: Selector '[] (Id NSData)
dataRepresentationSelector = mkSelector "dataRepresentation"

