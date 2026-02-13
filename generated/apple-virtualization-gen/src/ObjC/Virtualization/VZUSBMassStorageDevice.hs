{-# LANGUAGE DataKinds #-}
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

import Foreign.Ptr (Ptr, FunPtr)
import Foreign.C.Types

import ObjC.Runtime.Types
import ObjC.Runtime.Message (sendMessage, sendOwnedMessage, sendClassMessage, sendOwnedClassMessage)
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
initWithConfiguration vzusbMassStorageDevice configuration =
  sendOwnedMessage vzusbMassStorageDevice initWithConfigurationSelector (toVZUSBMassStorageDeviceConfiguration configuration)

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @initWithConfiguration:@
initWithConfigurationSelector :: Selector '[Id VZUSBMassStorageDeviceConfiguration] (Id VZUSBMassStorageDevice)
initWithConfigurationSelector = mkSelector "initWithConfiguration:"

