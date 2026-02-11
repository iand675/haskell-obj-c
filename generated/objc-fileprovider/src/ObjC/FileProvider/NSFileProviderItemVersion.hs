{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Generated bindings for @NSFileProviderItemVersion@.
module ObjC.FileProvider.NSFileProviderItemVersion
  ( NSFileProviderItemVersion
  , IsNSFileProviderItemVersion(..)
  , initWithContentVersion_metadataVersion
  , beforeFirstSyncComponent
  , contentVersion
  , metadataVersion
  , initWithContentVersion_metadataVersionSelector
  , beforeFirstSyncComponentSelector
  , contentVersionSelector
  , metadataVersionSelector


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

-- | Items versions have two distinct components, one for the file contents and one for metadata.
--
-- Components are limited to 128 bytes in size.
--
-- ObjC selector: @- initWithContentVersion:metadataVersion:@
initWithContentVersion_metadataVersion :: (IsNSFileProviderItemVersion nsFileProviderItemVersion, IsNSData contentVersion, IsNSData metadataVersion) => nsFileProviderItemVersion -> contentVersion -> metadataVersion -> IO (Id NSFileProviderItemVersion)
initWithContentVersion_metadataVersion nsFileProviderItemVersion  contentVersion metadataVersion =
withObjCPtr contentVersion $ \raw_contentVersion ->
  withObjCPtr metadataVersion $ \raw_metadataVersion ->
      sendMsg nsFileProviderItemVersion (mkSelector "initWithContentVersion:metadataVersion:") (retPtr retVoid) [argPtr (castPtr raw_contentVersion :: Ptr ()), argPtr (castPtr raw_metadataVersion :: Ptr ())] >>= ownedObject . castPtr

-- | Version component exposed by the system to denote a state that predates a version returned by the provider.
--
-- In case an item was created by calling @createItemBasedOnTemplate@ and the item returned by the provider in the completion handler of that call didn't match the item template passed by the system, the system will try to apply the changes asked by the provider to the disk. However, the system may detect conflicts when applying those content back to the disk, which will cause the system to send the new disk version to the extension, by calling @modifyItem@ or @deleteItemWithIdentifier@ with a @baseVersion@ that represents the item as passed in the template of the @createItemBasedOnTemplate@ call.
--
-- This constant is used by the system to represent that specific version that was communicated by the system to the extension but does not have a corresponding version assigned by the extension.
--
-- ObjC selector: @+ beforeFirstSyncComponent@
beforeFirstSyncComponent :: IO (Id NSData)
beforeFirstSyncComponent  =
  do
    cls' <- getRequiredClass "NSFileProviderItemVersion"
    sendClassMsg cls' (mkSelector "beforeFirstSyncComponent") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | Version data for the content of the file.
--
-- This property is used by the system for two purposes: if the contentVersion changes, - the system assumes that the contents have changed and will trigger a redownload if   necessary. The exception to this is the case where the extension accepts a content   sent by the system when replying to a createItemBasedOnTemplate or modifyItem call   with shouldFetchContent set to NO. - the thumbnail cache is invalidated
--
-- Note that the resource fork of the file is considered content, so this version data should change when either the data fork or the resource fork changes.
--
-- ObjC selector: @- contentVersion@
contentVersion :: IsNSFileProviderItemVersion nsFileProviderItemVersion => nsFileProviderItemVersion -> IO (Id NSData)
contentVersion nsFileProviderItemVersion  =
  sendMsg nsFileProviderItemVersion (mkSelector "contentVersion") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | Version data for the metadata of the item, i.e everything but the data fork and the resource fork.
--
-- The system will store this version, but otherwise ignore it: - metadata changes on an item will be applied even if the metadataVersion remains unchanged - if the metadata version changes without any corresponding observable changes in the metadata returned   to the system, the system will simply store the updated metadata version (to return it as the base version   of a possible future change request).
--
-- ObjC selector: @- metadataVersion@
metadataVersion :: IsNSFileProviderItemVersion nsFileProviderItemVersion => nsFileProviderItemVersion -> IO (Id NSData)
metadataVersion nsFileProviderItemVersion  =
  sendMsg nsFileProviderItemVersion (mkSelector "metadataVersion") (retPtr retVoid) [] >>= retainedObject . castPtr

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @initWithContentVersion:metadataVersion:@
initWithContentVersion_metadataVersionSelector :: Selector
initWithContentVersion_metadataVersionSelector = mkSelector "initWithContentVersion:metadataVersion:"

-- | @Selector@ for @beforeFirstSyncComponent@
beforeFirstSyncComponentSelector :: Selector
beforeFirstSyncComponentSelector = mkSelector "beforeFirstSyncComponent"

-- | @Selector@ for @contentVersion@
contentVersionSelector :: Selector
contentVersionSelector = mkSelector "contentVersion"

-- | @Selector@ for @metadataVersion@
metadataVersionSelector :: Selector
metadataVersionSelector = mkSelector "metadataVersion"

