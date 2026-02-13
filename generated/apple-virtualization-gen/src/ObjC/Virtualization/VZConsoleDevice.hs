{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Class representing a console device in a virtual machine.
--
-- VZConsoleDevice should not be instantiated directly.
--
-- Console devices are first configured on the VZVirtualMachineConfiguration through a subclass of VZConsoleDeviceConfiguration.    When a VZVirtualMachine is created from the configuration, the console devices are available through the VZVirtualMachine.consoleDevices property.
--
-- The real type of VZConsoleDevice corresponds to the type used by the configuration.    For example, a VZVirtioConsoleDeviceConfiguration leads to a device of type VZVirtioConsoleDevice.
--
-- See: VZConsoleDeviceConfiguration
--
-- Generated bindings for @VZConsoleDevice@.
module ObjC.Virtualization.VZConsoleDevice
  ( VZConsoleDevice
  , IsVZConsoleDevice(..)
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
new :: IO (Id VZConsoleDevice)
new  =
  do
    cls' <- getRequiredClass "VZConsoleDevice"
    sendOwnedClassMessage cls' newSelector

-- | @- init@
init_ :: IsVZConsoleDevice vzConsoleDevice => vzConsoleDevice -> IO (Id VZConsoleDevice)
init_ vzConsoleDevice =
  sendOwnedMessage vzConsoleDevice initSelector

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @new@
newSelector :: Selector '[] (Id VZConsoleDevice)
newSelector = mkSelector "new"

-- | @Selector@ for @init@
initSelector :: Selector '[] (Id VZConsoleDevice)
initSelector = mkSelector "init"

