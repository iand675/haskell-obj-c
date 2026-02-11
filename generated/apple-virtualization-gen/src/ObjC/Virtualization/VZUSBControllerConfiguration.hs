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
  , newSelector
  , initSelector
  , usbDevicesSelector
  , setUsbDevicesSelector


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
new :: IO (Id VZUSBControllerConfiguration)
new  =
  do
    cls' <- getRequiredClass "VZUSBControllerConfiguration"
    sendClassMsg cls' (mkSelector "new") (retPtr retVoid) [] >>= ownedObject . castPtr

-- | @- init@
init_ :: IsVZUSBControllerConfiguration vzusbControllerConfiguration => vzusbControllerConfiguration -> IO (Id VZUSBControllerConfiguration)
init_ vzusbControllerConfiguration  =
    sendMsg vzusbControllerConfiguration (mkSelector "init") (retPtr retVoid) [] >>= ownedObject . castPtr

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
usbDevices vzusbControllerConfiguration  =
    sendMsg vzusbControllerConfiguration (mkSelector "usbDevices") (retPtr retVoid) [] >>= retainedObject . castPtr

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
setUsbDevices vzusbControllerConfiguration  value =
  withObjCPtr value $ \raw_value ->
      sendMsg vzusbControllerConfiguration (mkSelector "setUsbDevices:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @new@
newSelector :: Selector
newSelector = mkSelector "new"

-- | @Selector@ for @init@
initSelector :: Selector
initSelector = mkSelector "init"

-- | @Selector@ for @usbDevices@
usbDevicesSelector :: Selector
usbDevicesSelector = mkSelector "usbDevices"

-- | @Selector@ for @setUsbDevices:@
setUsbDevicesSelector :: Selector
setUsbDevicesSelector = mkSelector "setUsbDevices:"

