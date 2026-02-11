{-# LANGUAGE PatternSynonyms #-}
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
  , sidecarFileNameSelector
  , setSidecarFileNameSelector

  -- * Enum types
  , MEFileInfoFragmentsStatus(MEFileInfoFragmentsStatus)
  , pattern MEFileInfoCouldNotContainFragments
  , pattern MEFileInfoContainsFragments
  , pattern MEFileInfoCouldContainButDoesNotContainFragments

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
fragmentsStatus meFileInfo  =
    fmap (coerce :: CLong -> MEFileInfoFragmentsStatus) $ sendMsg meFileInfo (mkSelector "fragmentsStatus") retCLong []

-- | fragmentsStatus
--
-- Indicates if the media asset is capable of being extended by fragments or contains fragments
--
-- See the MEFileInfoFragmentsStatus values for details of the return value. The value will default to MEFileInfoCouldNotContainFragments.
--
-- ObjC selector: @- setFragmentsStatus:@
setFragmentsStatus :: IsMEFileInfo meFileInfo => meFileInfo -> MEFileInfoFragmentsStatus -> IO ()
setFragmentsStatus meFileInfo  value =
    sendMsg meFileInfo (mkSelector "setFragmentsStatus:") retVoid [argCLong (coerce value)]

-- | sidecarFileName
--
-- The sidecar filename used by the MediaExtension.
--
-- Represents a new or existing sidecar file located in the same directory as the primary media file. The filename should include the file extension, and should not contain the file path, or contain any slashes. The file extension should be supported by the format reader, and present in the EXAppExtensionAttributes and UTExportedTypeDeclarations dictionaries in the MediaExtension format reader Info.plist.
--
-- ObjC selector: @- sidecarFileName@
sidecarFileName :: IsMEFileInfo meFileInfo => meFileInfo -> IO (Id NSString)
sidecarFileName meFileInfo  =
    sendMsg meFileInfo (mkSelector "sidecarFileName") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | sidecarFileName
--
-- The sidecar filename used by the MediaExtension.
--
-- Represents a new or existing sidecar file located in the same directory as the primary media file. The filename should include the file extension, and should not contain the file path, or contain any slashes. The file extension should be supported by the format reader, and present in the EXAppExtensionAttributes and UTExportedTypeDeclarations dictionaries in the MediaExtension format reader Info.plist.
--
-- ObjC selector: @- setSidecarFileName:@
setSidecarFileName :: (IsMEFileInfo meFileInfo, IsNSString value) => meFileInfo -> value -> IO ()
setSidecarFileName meFileInfo  value =
  withObjCPtr value $ \raw_value ->
      sendMsg meFileInfo (mkSelector "setSidecarFileName:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @fragmentsStatus@
fragmentsStatusSelector :: Selector
fragmentsStatusSelector = mkSelector "fragmentsStatus"

-- | @Selector@ for @setFragmentsStatus:@
setFragmentsStatusSelector :: Selector
setFragmentsStatusSelector = mkSelector "setFragmentsStatus:"

-- | @Selector@ for @sidecarFileName@
sidecarFileNameSelector :: Selector
sidecarFileNameSelector = mkSelector "sidecarFileName"

-- | @Selector@ for @setSidecarFileName:@
setSidecarFileNameSelector :: Selector
setSidecarFileNameSelector = mkSelector "setSidecarFileName:"

