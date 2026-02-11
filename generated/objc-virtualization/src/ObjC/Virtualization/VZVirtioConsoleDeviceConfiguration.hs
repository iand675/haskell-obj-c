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

-- | @- init@
init_ :: IsVZVirtioConsoleDeviceConfiguration vzVirtioConsoleDeviceConfiguration => vzVirtioConsoleDeviceConfiguration -> IO (Id VZVirtioConsoleDeviceConfiguration)
init_ vzVirtioConsoleDeviceConfiguration  =
  sendMsg vzVirtioConsoleDeviceConfiguration (mkSelector "init") (retPtr retVoid) [] >>= ownedObject . castPtr

-- | The console ports to be configured for this console device.
--
-- ObjC selector: @- ports@
ports :: IsVZVirtioConsoleDeviceConfiguration vzVirtioConsoleDeviceConfiguration => vzVirtioConsoleDeviceConfiguration -> IO (Id VZVirtioConsolePortConfigurationArray)
ports vzVirtioConsoleDeviceConfiguration  =
  sendMsg vzVirtioConsoleDeviceConfiguration (mkSelector "ports") (retPtr retVoid) [] >>= retainedObject . castPtr

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @init@
initSelector :: Selector
initSelector = mkSelector "init"

-- | @Selector@ for @ports@
portsSelector :: Selector
portsSelector = mkSelector "ports"

