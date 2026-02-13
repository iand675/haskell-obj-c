{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Base class for a USB Controller configuration.
--
-- VZUSBControllerConfiguration should not be instantiated directly.    One of its subclasses like VZXHCIControllerConfiguration should be used instead.
--
-- See: VZXHCIControllerConfiguration
--
-- Generated bindings for @VZUSBControllerConfiguration@.
module ObjC.Virtualization.VZUSBControllerConfiguration
  ( VZUSBControllerConfiguration
  , IsVZUSBControllerConfiguration(..)
  , new
  , init_
  , usbDevices
  , setUsbDevices
  , initSelector
  , newSelector
  , setUsbDevicesSelector
  , usbDevicesSelector


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
new :: IO (Id VZUSBControllerConfiguration)
new  =
  do
    cls' <- getRequiredClass "VZUSBControllerConfiguration"
    sendOwnedClassMessage cls' newSelector

-- | @- init@
init_ :: IsVZUSBControllerConfiguration vzusbControllerConfiguration => vzusbControllerConfiguration -> IO (Id VZUSBControllerConfiguration)
init_ vzusbControllerConfiguration =
  sendOwnedMessage vzusbControllerConfiguration initSelector

-- | List of USB devices. Empty by default.
--
-- This list represents a set of USB devices that virtual machine will start with.    For each entry in this list, there will be a corresponding runtime object created in VZUSBController.usbDevices property.
--
-- See: VZUSBController
--
-- See: VZUSBDeviceConfiguration
--
-- See: VZUSBMassStorageDeviceConfiguration
--
-- ObjC selector: @- usbDevices@
usbDevices :: IsVZUSBControllerConfiguration vzusbControllerConfiguration => vzusbControllerConfiguration -> IO (Id NSArray)
usbDevices vzusbControllerConfiguration =
  sendMessage vzusbControllerConfiguration usbDevicesSelector

-- | List of USB devices. Empty by default.
--
-- This list represents a set of USB devices that virtual machine will start with.    For each entry in this list, there will be a corresponding runtime object created in VZUSBController.usbDevices property.
--
-- See: VZUSBController
--
-- See: VZUSBDeviceConfiguration
--
-- See: VZUSBMassStorageDeviceConfiguration
--
-- ObjC selector: @- setUsbDevices:@
setUsbDevices :: (IsVZUSBControllerConfiguration vzusbControllerConfiguration, IsNSArray value) => vzusbControllerConfiguration -> value -> IO ()
setUsbDevices vzusbControllerConfiguration value =
  sendMessage vzusbControllerConfiguration setUsbDevicesSelector (toNSArray value)

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @new@
newSelector :: Selector '[] (Id VZUSBControllerConfiguration)
newSelector = mkSelector "new"

-- | @Selector@ for @init@
initSelector :: Selector '[] (Id VZUSBControllerConfiguration)
initSelector = mkSelector "init"

-- | @Selector@ for @usbDevices@
usbDevicesSelector :: Selector '[] (Id NSArray)
usbDevicesSelector = mkSelector "usbDevices"

-- | @Selector@ for @setUsbDevices:@
setUsbDevicesSelector :: Selector '[Id NSArray] ()
setUsbDevicesSelector = mkSelector "setUsbDevices:"

