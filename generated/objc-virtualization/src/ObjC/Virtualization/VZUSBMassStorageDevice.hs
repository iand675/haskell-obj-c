{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Class representing a hot-pluggable USB Mass Storage device.
--
-- This device is created through either instantiating it directly and passing VZUSBMassStorageDeviceConfiguration to its initializer    or instantiating a VZUSBMassStorageDeviceConfiguration in a VZVirtualMachineConfiguration. Direct instantiation will create    an object that can be passed to -[VZUSBController attachDevice:completionHandler:] method. Instantiation via VZUSBMassStorageDeviceConfiguration    will make the device available in VZUSBController.usbDevices property.
--
-- See: VZUSBController
--
-- Generated bindings for @VZUSBMassStorageDevice@.
module ObjC.Virtualization.VZUSBMassStorageDevice
  ( VZUSBMassStorageDevice
  , IsVZUSBMassStorageDevice(..)
  , initWithConfiguration
  , initWithConfigurationSelector


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

-- | Initialize the runtime USB Mass Storage device object.
--
-- @configuration@ — The configuration of the USB Mass Storage device.
--
-- See: VZUSBMassStorageDeviceConfiguration
--
-- ObjC selector: @- initWithConfiguration:@
initWithConfiguration :: (IsVZUSBMassStorageDevice vzusbMassStorageDevice, IsVZUSBMassStorageDeviceConfiguration configuration) => vzusbMassStorageDevice -> configuration -> IO (Id VZUSBMassStorageDevice)
initWithConfiguration vzusbMassStorageDevice  configuration =
withObjCPtr configuration $ \raw_configuration ->
    sendMsg vzusbMassStorageDevice (mkSelector "initWithConfiguration:") (retPtr retVoid) [argPtr (castPtr raw_configuration :: Ptr ())] >>= ownedObject . castPtr

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @initWithConfiguration:@
initWithConfigurationSelector :: Selector
initWithConfigurationSelector = mkSelector "initWithConfiguration:"

