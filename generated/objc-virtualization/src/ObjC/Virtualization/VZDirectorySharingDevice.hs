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
  , newSelector
  , initSelector


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

-- | @+ new@
new :: IO (Id VZDirectorySharingDevice)
new  =
  do
    cls' <- getRequiredClass "VZDirectorySharingDevice"
    sendClassMsg cls' (mkSelector "new") (retPtr retVoid) [] >>= ownedObject . castPtr

-- | @- init@
init_ :: IsVZDirectorySharingDevice vzDirectorySharingDevice => vzDirectorySharingDevice -> IO (Id VZDirectorySharingDevice)
init_ vzDirectorySharingDevice  =
  sendMsg vzDirectorySharingDevice (mkSelector "init") (retPtr retVoid) [] >>= ownedObject . castPtr

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @new@
newSelector :: Selector
newSelector = mkSelector "new"

-- | @Selector@ for @init@
initSelector :: Selector
initSelector = mkSelector "init"

