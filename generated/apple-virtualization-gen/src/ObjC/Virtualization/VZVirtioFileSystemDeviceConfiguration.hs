{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Configuration of a Virtio file system device.
--
-- This configuration creates a Virtio file system device which allows for exposing    directories on the host to a guest via a tag label.
--
-- Generated bindings for @VZVirtioFileSystemDeviceConfiguration@.
module ObjC.Virtualization.VZVirtioFileSystemDeviceConfiguration
  ( VZVirtioFileSystemDeviceConfiguration
  , IsVZVirtioFileSystemDeviceConfiguration(..)
  , initWithTag
  , validateTag_error
  , tag
  , setTag
  , share
  , setShare
  , macOSGuestAutomountTag
  , initWithTagSelector
  , validateTag_errorSelector
  , tagSelector
  , setTagSelector
  , shareSelector
  , setShareSelector
  , macOSGuestAutomountTagSelector


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

import ObjC.Virtualization.Internal.Classes
import ObjC.Foundation.Internal.Classes

-- | Configuration of the Virtio file system device.
--
-- @tag@ — The label identifying this device in the guest.
--
-- The tag is presented as a label in the guest identifying this device for mounting. The tag must be valid, which can be checked with +[VZVirtioFileSystemDeviceConfiguration validateTag:error:].
--
-- See: +[VZVirtioFileSystemDeviceConfiguration validateTag:error:]
--
-- ObjC selector: @- initWithTag:@
initWithTag :: (IsVZVirtioFileSystemDeviceConfiguration vzVirtioFileSystemDeviceConfiguration, IsNSString tag) => vzVirtioFileSystemDeviceConfiguration -> tag -> IO (Id VZVirtioFileSystemDeviceConfiguration)
initWithTag vzVirtioFileSystemDeviceConfiguration  tag =
  withObjCPtr tag $ \raw_tag ->
      sendMsg vzVirtioFileSystemDeviceConfiguration (mkSelector "initWithTag:") (retPtr retVoid) [argPtr (castPtr raw_tag :: Ptr ())] >>= ownedObject . castPtr

-- | Check if tag is a valid Virtio file system tag.
--
-- @tag@ — The tag to validate.
--
-- @error@ — If not nil, assigned with an error describing why the tag is not valid.
--
-- The tag must be non-empty and less than 36 bytes when encoded in UTF-8.
--
-- ObjC selector: @+ validateTag:error:@
validateTag_error :: (IsNSString tag, IsNSError error_) => tag -> error_ -> IO Bool
validateTag_error tag error_ =
  do
    cls' <- getRequiredClass "VZVirtioFileSystemDeviceConfiguration"
    withObjCPtr tag $ \raw_tag ->
      withObjCPtr error_ $ \raw_error_ ->
        fmap ((/= 0) :: CULong -> Bool) $ sendClassMsg cls' (mkSelector "validateTag:error:") retCULong [argPtr (castPtr raw_tag :: Ptr ()), argPtr (castPtr raw_error_ :: Ptr ())]

-- | The tag is a string identifying the device.
--
-- The tag is presented as a label in the guest identifying this device for mounting. The tag must be valid, which can be checked with +[VZVirtioFileSystemDeviceConfiguration validateTag:error:].
--
-- See: +[VZVirtioFileSystemDeviceConfiguration validateTag:error:]
--
-- ObjC selector: @- tag@
tag :: IsVZVirtioFileSystemDeviceConfiguration vzVirtioFileSystemDeviceConfiguration => vzVirtioFileSystemDeviceConfiguration -> IO (Id NSString)
tag vzVirtioFileSystemDeviceConfiguration  =
    sendMsg vzVirtioFileSystemDeviceConfiguration (mkSelector "tag") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | The tag is a string identifying the device.
--
-- The tag is presented as a label in the guest identifying this device for mounting. The tag must be valid, which can be checked with +[VZVirtioFileSystemDeviceConfiguration validateTag:error:].
--
-- See: +[VZVirtioFileSystemDeviceConfiguration validateTag:error:]
--
-- ObjC selector: @- setTag:@
setTag :: (IsVZVirtioFileSystemDeviceConfiguration vzVirtioFileSystemDeviceConfiguration, IsNSString value) => vzVirtioFileSystemDeviceConfiguration -> value -> IO ()
setTag vzVirtioFileSystemDeviceConfiguration  value =
  withObjCPtr value $ \raw_value ->
      sendMsg vzVirtioFileSystemDeviceConfiguration (mkSelector "setTag:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- | Directory share. Defines how host resources are exposed to the guest virtual machine.
--
-- See: VZSingleDirectoryShare
--
-- See: VZMultipleDirectoryShare
--
-- See: VZLinuxRosettaDirectoryShare
--
-- ObjC selector: @- share@
share :: IsVZVirtioFileSystemDeviceConfiguration vzVirtioFileSystemDeviceConfiguration => vzVirtioFileSystemDeviceConfiguration -> IO (Id VZDirectoryShare)
share vzVirtioFileSystemDeviceConfiguration  =
    sendMsg vzVirtioFileSystemDeviceConfiguration (mkSelector "share") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | Directory share. Defines how host resources are exposed to the guest virtual machine.
--
-- See: VZSingleDirectoryShare
--
-- See: VZMultipleDirectoryShare
--
-- See: VZLinuxRosettaDirectoryShare
--
-- ObjC selector: @- setShare:@
setShare :: (IsVZVirtioFileSystemDeviceConfiguration vzVirtioFileSystemDeviceConfiguration, IsVZDirectoryShare value) => vzVirtioFileSystemDeviceConfiguration -> value -> IO ()
setShare vzVirtioFileSystemDeviceConfiguration  value =
  withObjCPtr value $ \raw_value ->
      sendMsg vzVirtioFileSystemDeviceConfiguration (mkSelector "setShare:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- | The macOS automount tag.
--
-- A device configured with this tag will be automatically mounted in a macOS guest.
--
-- ObjC selector: @+ macOSGuestAutomountTag@
macOSGuestAutomountTag :: IO (Id NSString)
macOSGuestAutomountTag  =
  do
    cls' <- getRequiredClass "VZVirtioFileSystemDeviceConfiguration"
    sendClassMsg cls' (mkSelector "macOSGuestAutomountTag") (retPtr retVoid) [] >>= retainedObject . castPtr

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @initWithTag:@
initWithTagSelector :: Selector
initWithTagSelector = mkSelector "initWithTag:"

-- | @Selector@ for @validateTag:error:@
validateTag_errorSelector :: Selector
validateTag_errorSelector = mkSelector "validateTag:error:"

-- | @Selector@ for @tag@
tagSelector :: Selector
tagSelector = mkSelector "tag"

-- | @Selector@ for @setTag:@
setTagSelector :: Selector
setTagSelector = mkSelector "setTag:"

-- | @Selector@ for @share@
shareSelector :: Selector
shareSelector = mkSelector "share"

-- | @Selector@ for @setShare:@
setShareSelector :: Selector
setShareSelector = mkSelector "setShare:"

-- | @Selector@ for @macOSGuestAutomountTag@
macOSGuestAutomountTagSelector :: Selector
macOSGuestAutomountTagSelector = mkSelector "macOSGuestAutomountTag"

