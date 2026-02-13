{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Base class representing a directory sharing device in a virtual machine.
--
-- VZDirectorySharingDevice should not be instantiated directly.
--
-- Directory sharing devices are first configured on the VZVirtualMachineConfiguration through a subclass of VZDirectorySharingDeviceConfiguration.    When a VZVirtualMachine is created from the configuration, the directory sharing devices are available through the VZVirtualMachine.directorySharingDevices property.
--
-- The real type of VZDirectorySharingDevice corresponds to the type used by the configuration.    For example, a VZVirtioFileSystemDeviceConfiguration leads to a device of type VZVirtioFileSystemDevice.
--
-- See: VZVirtioFileSystemDevice
--
-- See: VZVirtioFileSystemDeviceConfiguration
--
-- Generated bindings for @VZDirectorySharingDevice@.
module ObjC.Virtualization.VZDirectorySharingDevice
  ( VZDirectorySharingDevice
  , IsVZDirectorySharingDevice(..)
  , new
  , init_
  , initSelector
  , newSelector


  ) where

import Foreign.Ptr (Ptr, FunPtr)
import Foreign.C.Types

import ObjC.Runtime.Types
import ObjC.Runtime.Message (sendMessage, sendOwnedMessage, sendClassMessage, sendOwnedClassMessage)
import ObjC.Runtime.Selector (mkSelector)
import ObjC.Runtime.Class (getRequiredClass)

import ObjC.Virtualization.Internal.Classes
import ObjC.Foundation.Internal.Classes

-- | @+ new@
new :: IO (Id VZDirectorySharingDevice)
new  =
  do
    cls' <- getRequiredClass "VZDirectorySharingDevice"
    sendOwnedClassMessage cls' newSelector

-- | @- init@
init_ :: IsVZDirectorySharingDevice vzDirectorySharingDevice => vzDirectorySharingDevice -> IO (Id VZDirectorySharingDevice)
init_ vzDirectorySharingDevice =
  sendOwnedMessage vzDirectorySharingDevice initSelector

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @new@
newSelector :: Selector '[] (Id VZDirectorySharingDevice)
newSelector = mkSelector "new"

-- | @Selector@ for @init@
initSelector :: Selector '[] (Id VZDirectorySharingDevice)
initSelector = mkSelector "init"

