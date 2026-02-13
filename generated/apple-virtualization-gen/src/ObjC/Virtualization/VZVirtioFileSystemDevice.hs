{-# LANGUAGE DataKinds #-}
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
  , setShareSelector
  , shareSelector
  , tagSelector


  ) where

import Foreign.Ptr (Ptr, FunPtr)
import Foreign.C.Types

import ObjC.Runtime.Types
import ObjC.Runtime.Message (sendMessage, sendOwnedMessage, sendClassMessage, sendOwnedClassMessage)
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
tag vzVirtioFileSystemDevice =
  sendMessage vzVirtioFileSystemDevice tagSelector

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
share vzVirtioFileSystemDevice =
  sendMessage vzVirtioFileSystemDevice shareSelector

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
setShare vzVirtioFileSystemDevice value =
  sendMessage vzVirtioFileSystemDevice setShareSelector (toVZDirectoryShare value)

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @tag@
tagSelector :: Selector '[] (Id NSString)
tagSelector = mkSelector "tag"

-- | @Selector@ for @share@
shareSelector :: Selector '[] (Id VZDirectoryShare)
shareSelector = mkSelector "share"

-- | @Selector@ for @setShare:@
setShareSelector :: Selector '[Id VZDirectoryShare] ()
setShareSelector = mkSelector "setShare:"

