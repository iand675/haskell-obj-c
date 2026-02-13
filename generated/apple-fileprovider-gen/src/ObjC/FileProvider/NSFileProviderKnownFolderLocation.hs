{-# LANGUAGE DataKinds #-}
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
  , initWithExistingItemIdentifierSelector
  , initWithParentItemIdentifier_filenameSelector


  ) where

import Foreign.Ptr (Ptr, FunPtr)
import Foreign.C.Types

import ObjC.Runtime.Types
import ObjC.Runtime.Message (sendMessage, sendOwnedMessage, sendClassMessage, sendOwnedClassMessage)
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
initWithParentItemIdentifier_filename nsFileProviderKnownFolderLocation parentItemIdentifier filename =
  sendOwnedMessage nsFileProviderKnownFolderLocation initWithParentItemIdentifier_filenameSelector (toNSString parentItemIdentifier) (toNSString filename)

-- | Initialize a location with the item identifier of a folder that already exists on the server.
--
-- If the known folder already exists on the server, the provider can specify the exact identifier of the item that needs to be used to back the known folder.
--
-- ObjC selector: @- initWithExistingItemIdentifier:@
initWithExistingItemIdentifier :: (IsNSFileProviderKnownFolderLocation nsFileProviderKnownFolderLocation, IsNSString existingItemIdentifier) => nsFileProviderKnownFolderLocation -> existingItemIdentifier -> IO (Id NSFileProviderKnownFolderLocation)
initWithExistingItemIdentifier nsFileProviderKnownFolderLocation existingItemIdentifier =
  sendOwnedMessage nsFileProviderKnownFolderLocation initWithExistingItemIdentifierSelector (toNSString existingItemIdentifier)

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @initWithParentItemIdentifier:filename:@
initWithParentItemIdentifier_filenameSelector :: Selector '[Id NSString, Id NSString] (Id NSFileProviderKnownFolderLocation)
initWithParentItemIdentifier_filenameSelector = mkSelector "initWithParentItemIdentifier:filename:"

-- | @Selector@ for @initWithExistingItemIdentifier:@
initWithExistingItemIdentifierSelector :: Selector '[Id NSString] (Id NSFileProviderKnownFolderLocation)
initWithExistingItemIdentifierSelector = mkSelector "initWithExistingItemIdentifier:"

