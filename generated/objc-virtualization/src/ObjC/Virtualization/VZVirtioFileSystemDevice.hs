{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Virtio File System Device
--
-- This is a device that exposes host resources to the guest as a file system mount.    The directory share defines which host resources are exposed to the guest.
--
-- This device is created through instantiating a VZVirtioFileSystemDeviceConfiguration in a VZVirtualMachineConfiguration and is available in the    VZVirtualMachine.directorySharingDevices property.
--
-- See: VZVirtioFileSystemDeviceConfiguration
--
-- See: VZSingleDirectoryShare
--
-- See: VZMultipleDirectoryShare
--
-- Generated bindings for @VZVirtioFileSystemDevice@.
module ObjC.Virtualization.VZVirtioFileSystemDevice
  ( VZVirtioFileSystemDevice
  , IsVZVirtioFileSystemDevice(..)
  , tag
  , share
  , setShare
  , tagSelector
  , shareSelector
  , setShareSelector


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

-- | The tag is a string identifying the device.
--
-- The tag is presented as a label in the guest identifying this device for mounting.
--
-- ObjC selector: @- tag@
tag :: IsVZVirtioFileSystemDevice vzVirtioFileSystemDevice => vzVirtioFileSystemDevice -> IO (Id NSString)
tag vzVirtioFileSystemDevice  =
  sendMsg vzVirtioFileSystemDevice (mkSelector "tag") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | Directory share. Defines how host resources are exposed to the guest virtual machine.
--
-- Setting this property to VZLinuxRosettaDirectoryShare is not supported and will cause an exception to be raised.
--
-- See: VZSingleDirectoryShare
--
-- See: VZMultipleDirectoryShare
--
-- ObjC selector: @- share@
share :: IsVZVirtioFileSystemDevice vzVirtioFileSystemDevice => vzVirtioFileSystemDevice -> IO (Id VZDirectoryShare)
share vzVirtioFileSystemDevice  =
  sendMsg vzVirtioFileSystemDevice (mkSelector "share") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | Directory share. Defines how host resources are exposed to the guest virtual machine.
--
-- Setting this property to VZLinuxRosettaDirectoryShare is not supported and will cause an exception to be raised.
--
-- See: VZSingleDirectoryShare
--
-- See: VZMultipleDirectoryShare
--
-- ObjC selector: @- setShare:@
setShare :: (IsVZVirtioFileSystemDevice vzVirtioFileSystemDevice, IsVZDirectoryShare value) => vzVirtioFileSystemDevice -> value -> IO ()
setShare vzVirtioFileSystemDevice  value =
withObjCPtr value $ \raw_value ->
    sendMsg vzVirtioFileSystemDevice (mkSelector "setShare:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @tag@
tagSelector :: Selector
tagSelector = mkSelector "tag"

-- | @Selector@ for @share@
shareSelector :: Selector
shareSelector = mkSelector "share"

-- | @Selector@ for @setShare:@
setShareSelector :: Selector
setShareSelector = mkSelector "setShare:"

