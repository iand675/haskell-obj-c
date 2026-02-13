{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Base class representing a socket device in a virtual machine.
--
-- VZSocketDevice should not be instantiated directly.
--
-- Socket devices are first configured on the VZVirtualMachineConfiguration through a subclass of VZSocketDeviceConfiguration.    When a VZVirtualMachine is created from the configuration, the socket devices are available through the VZVirtualMachine.socketDevices property.
--
-- The real type of VZSocketDevice corresponds to the type used by the configuration.    For example, a VZVirtioSocketDeviceConfiguration leads to a device of type VZVirtioSocketDevice.
--
-- See: VZVirtioSocketDevice
--
-- See: VZVirtioSocketDeviceConfiguration
--
-- Generated bindings for @VZSocketDevice@.
module ObjC.Virtualization.VZSocketDevice
  ( VZSocketDevice
  , IsVZSocketDevice(..)
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
new :: IO (Id VZSocketDevice)
new  =
  do
    cls' <- getRequiredClass "VZSocketDevice"
    sendOwnedClassMessage cls' newSelector

-- | @- init@
init_ :: IsVZSocketDevice vzSocketDevice => vzSocketDevice -> IO (Id VZSocketDevice)
init_ vzSocketDevice =
  sendOwnedMessage vzSocketDevice initSelector

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @new@
newSelector :: Selector '[] (Id VZSocketDevice)
newSelector = mkSelector "new"

-- | @Selector@ for @init@
initSelector :: Selector '[] (Id VZSocketDevice)
initSelector = mkSelector "init"

