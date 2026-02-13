{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Virtio Console Serial Port Device
--
-- The device creates a console which enables communication between the host and the guest through the Virtio interface.
--
-- The device sets up a single port on the Virtio console device.
--
-- Generated bindings for @VZVirtioConsoleDeviceSerialPortConfiguration@.
module ObjC.Virtualization.VZVirtioConsoleDeviceSerialPortConfiguration
  ( VZVirtioConsoleDeviceSerialPortConfiguration
  , IsVZVirtioConsoleDeviceSerialPortConfiguration(..)
  , init_
  , initSelector


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
init_ :: IsVZVirtioConsoleDeviceSerialPortConfiguration vzVirtioConsoleDeviceSerialPortConfiguration => vzVirtioConsoleDeviceSerialPortConfiguration -> IO (Id VZVirtioConsoleDeviceSerialPortConfiguration)
init_ vzVirtioConsoleDeviceSerialPortConfiguration =
  sendOwnedMessage vzVirtioConsoleDeviceSerialPortConfiguration initSelector

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @init@
initSelector :: Selector '[] (Id VZVirtioConsoleDeviceSerialPortConfiguration)
initSelector = mkSelector "init"

