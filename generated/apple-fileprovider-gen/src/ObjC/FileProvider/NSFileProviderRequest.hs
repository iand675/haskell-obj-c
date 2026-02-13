{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Generated bindings for @NSFileProviderRequest@.
module ObjC.FileProvider.NSFileProviderRequest
  ( NSFileProviderRequest
  , IsNSFileProviderRequest(..)
  , isSystemRequest
  , isFileViewerRequest
  , requestingExecutable
  , domainVersion
  , domainVersionSelector
  , isFileViewerRequestSelector
  , isSystemRequestSelector
  , requestingExecutableSelector


  ) where

import Foreign.Ptr (Ptr, FunPtr)
import Foreign.C.Types

import ObjC.Runtime.Types
import ObjC.Runtime.Message (sendMessage, sendOwnedMessage, sendClassMessage, sendOwnedClassMessage)
import ObjC.Runtime.Selector (mkSelector)
import ObjC.Runtime.Class (getRequiredClass)

import ObjC.FileProvider.Internal.Classes
import ObjC.Foundation.Internal.Classes

-- | The request was made by the sync system, e.g. to update a file to its latest version after a remote update was pushed.
--
-- This is only valid for NSFileProviderRequest objects passed to these methods: - [NSFileProviderEnumerating enumeratorForContainerItemIdentifier:] - [NSFileProviderReplicatedExtension fetchContentsForItemWithIdentifier:]
--
-- For sync up methods (createItem/modifyItem/deleteItem), the system does not know which actor made the modifications to the file, so it cannot supply this information.
--
-- ObjC selector: @- isSystemRequest@
isSystemRequest :: IsNSFileProviderRequest nsFileProviderRequest => nsFileProviderRequest -> IO Bool
isSystemRequest nsFileProviderRequest =
  sendMessage nsFileProviderRequest isSystemRequestSelector

-- | The request was made by Finder or one of its helpers.
--
-- This is only valid for NSFileProviderRequest objects passed to these methods: - [NSFileProviderEnumerating enumeratorForContainerItemIdentifier:] - [NSFileProviderReplicatedExtension fetchContentsForItemWithIdentifier:]
--
-- For sync up methods (createItem/modifyItem/deleteItem), the system does not know which actor made the modifications to the file, so it cannot supply this information.
--
-- ObjC selector: @- isFileViewerRequest@
isFileViewerRequest :: IsNSFileProviderRequest nsFileProviderRequest => nsFileProviderRequest -> IO Bool
isFileViewerRequest nsFileProviderRequest =
  sendMessage nsFileProviderRequest isFileViewerRequestSelector

-- | The URL of the requesting executable. This will always be nil unless both an MDM profile key is set, and the provider's application is installed by an MDM profile.
--
-- ObjC selector: @- requestingExecutable@
requestingExecutable :: IsNSFileProviderRequest nsFileProviderRequest => nsFileProviderRequest -> IO (Id NSURL)
requestingExecutable nsFileProviderRequest =
  sendMessage nsFileProviderRequest requestingExecutableSelector

-- | The version of the domain when the event that triggered the request was observed.
--
-- If the extension doesn't implement the NSFileProviderDomainState protocol, this will be nil.
--
-- ObjC selector: @- domainVersion@
domainVersion :: IsNSFileProviderRequest nsFileProviderRequest => nsFileProviderRequest -> IO (Id NSFileProviderDomainVersion)
domainVersion nsFileProviderRequest =
  sendMessage nsFileProviderRequest domainVersionSelector

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @isSystemRequest@
isSystemRequestSelector :: Selector '[] Bool
isSystemRequestSelector = mkSelector "isSystemRequest"

-- | @Selector@ for @isFileViewerRequest@
isFileViewerRequestSelector :: Selector '[] Bool
isFileViewerRequestSelector = mkSelector "isFileViewerRequest"

-- | @Selector@ for @requestingExecutable@
requestingExecutableSelector :: Selector '[] (Id NSURL)
requestingExecutableSelector = mkSelector "requestingExecutable"

-- | @Selector@ for @domainVersion@
domainVersionSelector :: Selector '[] (Id NSFileProviderDomainVersion)
domainVersionSelector = mkSelector "domainVersion"

