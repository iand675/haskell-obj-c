{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Virtio Console Device
--
-- This console device enables communication between the host and the guest using console ports through the Virtio interface.
--
-- The device sets up one or more ports via VZVirtioConsolePortConfiguration on the Virtio console device.
--
-- See: VZVirtioConsolePortConfiguration
--
-- See: VZVirtualMachineConfiguration.consoleDevices
--
-- Generated bindings for @VZVirtioConsoleDeviceConfiguration@.
module ObjC.Virtualization.VZVirtioConsoleDeviceConfiguration
  ( VZVirtioConsoleDeviceConfiguration
  , IsVZVirtioConsoleDeviceConfiguration(..)
  , init_
  , ports
  , initSelector
  , portsSelector


  ) where

import Foreign.Ptr (Ptr, FunPtr)
import Foreign.C.Types

import ObjC.Runtime.Types
import ObjC.Runtime.Message (sendMessage, sendOwnedMessage, sendClassMessage, sendOwnedClassMessage)
import ObjC.Runtime.Selector (mkSelector)
import ObjC.Runtime.Class (getRequiredClass)

import ObjC.Virtualization.Internal.Classes
import ObjC.Foundation.Internal.Classes

-- | @- init@
init_ :: IsVZVirtioConsoleDeviceConfiguration vzVirtioConsoleDeviceConfiguration => vzVirtioConsoleDeviceConfiguration -> IO (Id VZVirtioConsoleDeviceConfiguration)
init_ vzVirtioConsoleDeviceConfiguration =
  sendOwnedMessage vzVirtioConsoleDeviceConfiguration initSelector

-- | The console ports to be configured for this console device.
--
-- ObjC selector: @- ports@
ports :: IsVZVirtioConsoleDeviceConfiguration vzVirtioConsoleDeviceConfiguration => vzVirtioConsoleDeviceConfiguration -> IO (Id VZVirtioConsolePortConfigurationArray)
ports vzVirtioConsoleDeviceConfiguration =
  sendMessage vzVirtioConsoleDeviceConfiguration portsSelector

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @init@
initSelector :: Selector '[] (Id VZVirtioConsoleDeviceConfiguration)
initSelector = mkSelector "init"

-- | @Selector@ for @ports@
portsSelector :: Selector '[] (Id VZVirtioConsolePortConfigurationArray)
portsSelector = mkSelector "ports"

