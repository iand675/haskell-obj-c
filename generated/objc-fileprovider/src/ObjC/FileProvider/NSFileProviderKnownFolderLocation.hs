{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Specify the location of a known folder in the replicated tree.
--
-- Generated bindings for @NSFileProviderKnownFolderLocation@.
module ObjC.FileProvider.NSFileProviderKnownFolderLocation
  ( NSFileProviderKnownFolderLocation
  , IsNSFileProviderKnownFolderLocation(..)
  , initWithParentItemIdentifier_filename
  , initWithExistingItemIdentifier
  , initWithParentItemIdentifier_filenameSelector
  , initWithExistingItemIdentifierSelector


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

import ObjC.FileProvider.Internal.Classes
import ObjC.Foundation.Internal.Classes

-- | Initialize a location with the filename of the folder in a specified parent.
--
-- When replicating a known folder the system will reuse a folder located at the specified filename within the parent if one exists, or create a new item at this location if none exists yet.
--
-- ObjC selector: @- initWithParentItemIdentifier:filename:@
initWithParentItemIdentifier_filename :: (IsNSFileProviderKnownFolderLocation nsFileProviderKnownFolderLocation, IsNSString parentItemIdentifier, IsNSString filename) => nsFileProviderKnownFolderLocation -> parentItemIdentifier -> filename -> IO (Id NSFileProviderKnownFolderLocation)
initWithParentItemIdentifier_filename nsFileProviderKnownFolderLocation  parentItemIdentifier filename =
withObjCPtr parentItemIdentifier $ \raw_parentItemIdentifier ->
  withObjCPtr filename $ \raw_filename ->
      sendMsg nsFileProviderKnownFolderLocation (mkSelector "initWithParentItemIdentifier:filename:") (retPtr retVoid) [argPtr (castPtr raw_parentItemIdentifier :: Ptr ()), argPtr (castPtr raw_filename :: Ptr ())] >>= ownedObject . castPtr

-- | Initialize a location with the item identifier of a folder that already exists on the server.
--
-- If the known folder already exists on the server, the provider can specify the exact identifier of the item that needs to be used to back the known folder.
--
-- ObjC selector: @- initWithExistingItemIdentifier:@
initWithExistingItemIdentifier :: (IsNSFileProviderKnownFolderLocation nsFileProviderKnownFolderLocation, IsNSString existingItemIdentifier) => nsFileProviderKnownFolderLocation -> existingItemIdentifier -> IO (Id NSFileProviderKnownFolderLocation)
initWithExistingItemIdentifier nsFileProviderKnownFolderLocation  existingItemIdentifier =
withObjCPtr existingItemIdentifier $ \raw_existingItemIdentifier ->
    sendMsg nsFileProviderKnownFolderLocation (mkSelector "initWithExistingItemIdentifier:") (retPtr retVoid) [argPtr (castPtr raw_existingItemIdentifier :: Ptr ())] >>= ownedObject . castPtr

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @initWithParentItemIdentifier:filename:@
initWithParentItemIdentifier_filenameSelector :: Selector
initWithParentItemIdentifier_filenameSelector = mkSelector "initWithParentItemIdentifier:filename:"

-- | @Selector@ for @initWithExistingItemIdentifier:@
initWithExistingItemIdentifierSelector :: Selector
initWithExistingItemIdentifierSelector = mkSelector "initWithExistingItemIdentifier:"

