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
init_ :: IsVZVirtioConsoleDeviceSerialPortConfiguration vzVirtioConsoleDeviceSerialPortConfiguration => vzVirtioConsoleDeviceSerialPortConfiguration -> IO (Id VZVirtioConsoleDeviceSerialPortConfiguration)
init_ vzVirtioConsoleDeviceSerialPortConfiguration  =
  sendMsg vzVirtioConsoleDeviceSerialPortConfiguration (mkSelector "init") (retPtr retVoid) [] >>= ownedObject . castPtr

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @init@
initSelector :: Selector
initSelector = mkSelector "init"

