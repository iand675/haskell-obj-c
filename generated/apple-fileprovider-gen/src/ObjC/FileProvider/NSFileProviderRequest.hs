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
  , isSystemRequestSelector
  , isFileViewerRequestSelector
  , requestingExecutableSelector
  , domainVersionSelector


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

-- | The request was made by the sync system, e.g. to update a file to its latest version after a remote update was pushed.
--
-- This is only valid for NSFileProviderRequest objects passed to these methods: - [NSFileProviderEnumerating enumeratorForContainerItemIdentifier:] - [NSFileProviderReplicatedExtension fetchContentsForItemWithIdentifier:]
--
-- For sync up methods (createItem/modifyItem/deleteItem), the system does not know which actor made the modifications to the file, so it cannot supply this information.
--
-- ObjC selector: @- isSystemRequest@
isSystemRequest :: IsNSFileProviderRequest nsFileProviderRequest => nsFileProviderRequest -> IO Bool
isSystemRequest nsFileProviderRequest  =
    fmap ((/= 0) :: CULong -> Bool) $ sendMsg nsFileProviderRequest (mkSelector "isSystemRequest") retCULong []

-- | The request was made by Finder or one of its helpers.
--
-- This is only valid for NSFileProviderRequest objects passed to these methods: - [NSFileProviderEnumerating enumeratorForContainerItemIdentifier:] - [NSFileProviderReplicatedExtension fetchContentsForItemWithIdentifier:]
--
-- For sync up methods (createItem/modifyItem/deleteItem), the system does not know which actor made the modifications to the file, so it cannot supply this information.
--
-- ObjC selector: @- isFileViewerRequest@
isFileViewerRequest :: IsNSFileProviderRequest nsFileProviderRequest => nsFileProviderRequest -> IO Bool
isFileViewerRequest nsFileProviderRequest  =
    fmap ((/= 0) :: CULong -> Bool) $ sendMsg nsFileProviderRequest (mkSelector "isFileViewerRequest") retCULong []

-- | The URL of the requesting executable. This will always be nil unless both an MDM profile key is set, and the provider's application is installed by an MDM profile.
--
-- ObjC selector: @- requestingExecutable@
requestingExecutable :: IsNSFileProviderRequest nsFileProviderRequest => nsFileProviderRequest -> IO (Id NSURL)
requestingExecutable nsFileProviderRequest  =
    sendMsg nsFileProviderRequest (mkSelector "requestingExecutable") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | The version of the domain when the event that triggered the request was observed.
--
-- If the extension doesn't implement the NSFileProviderDomainState protocol, this will be nil.
--
-- ObjC selector: @- domainVersion@
domainVersion :: IsNSFileProviderRequest nsFileProviderRequest => nsFileProviderRequest -> IO (Id NSFileProviderDomainVersion)
domainVersion nsFileProviderRequest  =
    sendMsg nsFileProviderRequest (mkSelector "domainVersion") (retPtr retVoid) [] >>= retainedObject . castPtr

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @isSystemRequest@
isSystemRequestSelector :: Selector
isSystemRequestSelector = mkSelector "isSystemRequest"

-- | @Selector@ for @isFileViewerRequest@
isFileViewerRequestSelector :: Selector
isFileViewerRequestSelector = mkSelector "isFileViewerRequest"

-- | @Selector@ for @requestingExecutable@
requestingExecutableSelector :: Selector
requestingExecutableSelector = mkSelector "requestingExecutable"

-- | @Selector@ for @domainVersion@
domainVersionSelector :: Selector
domainVersionSelector = mkSelector "domainVersion"

