{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | MEFileInfo
--
-- A class incorporating file properties parsed from the media asset.
--
-- The MEFileInfo properties are parsed asynchronously through the loadFileInfoWithCompletionHandler method of MEFormatReader.
--
-- Generated bindings for @MEFileInfo@.
module ObjC.MediaExtension.MEFileInfo
  ( MEFileInfo
  , IsMEFileInfo(..)
  , fragmentsStatus
  , setFragmentsStatus
  , sidecarFileName
  , setSidecarFileName
  , fragmentsStatusSelector
  , setFragmentsStatusSelector
  , setSidecarFileNameSelector
  , sidecarFileNameSelector

  -- * Enum types
  , MEFileInfoFragmentsStatus(MEFileInfoFragmentsStatus)
  , pattern MEFileInfoCouldNotContainFragments
  , pattern MEFileInfoContainsFragments
  , pattern MEFileInfoCouldContainButDoesNotContainFragments

  ) where

import Foreign.Ptr (Ptr, FunPtr)
import Foreign.C.Types

import ObjC.Runtime.Types
import ObjC.Runtime.Message (sendMessage, sendOwnedMessage, sendClassMessage, sendOwnedClassMessage)
import ObjC.Runtime.Selector (mkSelector)
import ObjC.Runtime.Class (getRequiredClass)

import ObjC.MediaExtension.Internal.Classes
import ObjC.MediaExtension.Internal.Enums
import ObjC.Foundation.Internal.Classes

-- | fragmentsStatus
--
-- Indicates if the media asset is capable of being extended by fragments or contains fragments
--
-- See the MEFileInfoFragmentsStatus values for details of the return value. The value will default to MEFileInfoCouldNotContainFragments.
--
-- ObjC selector: @- fragmentsStatus@
fragmentsStatus :: IsMEFileInfo meFileInfo => meFileInfo -> IO MEFileInfoFragmentsStatus
fragmentsStatus meFileInfo =
  sendMessage meFileInfo fragmentsStatusSelector

-- | fragmentsStatus
--
-- Indicates if the media asset is capable of being extended by fragments or contains fragments
--
-- See the MEFileInfoFragmentsStatus values for details of the return value. The value will default to MEFileInfoCouldNotContainFragments.
--
-- ObjC selector: @- setFragmentsStatus:@
setFragmentsStatus :: IsMEFileInfo meFileInfo => meFileInfo -> MEFileInfoFragmentsStatus -> IO ()
setFragmentsStatus meFileInfo value =
  sendMessage meFileInfo setFragmentsStatusSelector value

-- | sidecarFileName
--
-- The sidecar filename used by the MediaExtension.
--
-- Represents a new or existing sidecar file located in the same directory as the primary media file. The filename should include the file extension, and should not contain the file path, or contain any slashes. The file extension should be supported by the format reader, and present in the EXAppExtensionAttributes and UTExportedTypeDeclarations dictionaries in the MediaExtension format reader Info.plist.
--
-- ObjC selector: @- sidecarFileName@
sidecarFileName :: IsMEFileInfo meFileInfo => meFileInfo -> IO (Id NSString)
sidecarFileName meFileInfo =
  sendMessage meFileInfo sidecarFileNameSelector

-- | sidecarFileName
--
-- The sidecar filename used by the MediaExtension.
--
-- Represents a new or existing sidecar file located in the same directory as the primary media file. The filename should include the file extension, and should not contain the file path, or contain any slashes. The file extension should be supported by the format reader, and present in the EXAppExtensionAttributes and UTExportedTypeDeclarations dictionaries in the MediaExtension format reader Info.plist.
--
-- ObjC selector: @- setSidecarFileName:@
setSidecarFileName :: (IsMEFileInfo meFileInfo, IsNSString value) => meFileInfo -> value -> IO ()
setSidecarFileName meFileInfo value =
  sendMessage meFileInfo setSidecarFileNameSelector (toNSString value)

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @fragmentsStatus@
fragmentsStatusSelector :: Selector '[] MEFileInfoFragmentsStatus
fragmentsStatusSelector = mkSelector "fragmentsStatus"

-- | @Selector@ for @setFragmentsStatus:@
setFragmentsStatusSelector :: Selector '[MEFileInfoFragmentsStatus] ()
setFragmentsStatusSelector = mkSelector "setFragmentsStatus:"

-- | @Selector@ for @sidecarFileName@
sidecarFileNameSelector :: Selector '[] (Id NSString)
sidecarFileNameSelector = mkSelector "sidecarFileName"

-- | @Selector@ for @setSidecarFileName:@
setSidecarFileNameSelector :: Selector '[Id NSString] ()
setSidecarFileNameSelector = mkSelector "setSidecarFileName:"

