{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Class representing a USB controller in a virtual machine.
--
-- VZUSBController should not be instantiated directly.    USB controllers are first configured on the VZVirtualMachineConfiguration through a subclass of VZUSBControllerConfiguration.    When a VZVirtualMachine is created from the configuration, the USB controllers are available through the VZVirtualMachine.usbControllers property.    The real type of VZUSBController corresponds to the type used by the configuration.    For example, a VZXHCIControllerConfiguration leads to a device of type VZXHCIController.
--
-- See: VZUSBControllerConfiguration
--
-- Generated bindings for @VZUSBController@.
module ObjC.Virtualization.VZUSBController
  ( VZUSBController
  , IsVZUSBController(..)
  , new
  , init_
  , attachDevice_completionHandler
  , detachDevice_completionHandler
  , newSelector
  , initSelector
  , attachDevice_completionHandlerSelector
  , detachDevice_completionHandlerSelector


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
new :: IO (Id VZUSBController)
new  =
  do
    cls' <- getRequiredClass "VZUSBController"
    sendClassMsg cls' (mkSelector "new") (retPtr retVoid) [] >>= ownedObject . castPtr

-- | @- init@
init_ :: IsVZUSBController vzusbController => vzusbController -> IO (Id VZUSBController)
init_ vzusbController  =
  sendMsg vzusbController (mkSelector "init") (retPtr retVoid) [] >>= ownedObject . castPtr

-- | Attach a USB device.
--
-- If the device is successfully attached to the controller, it will appear in the usbDevices property,    its usbController property will be set to point to the USB controller that it is attached to    and completion handler will return nil.    If the device was previously attached to this or another USB controller, attach function will fail    with the @VZErrorDeviceAlreadyAttached@. If the device cannot be initialized correctly, attach    function will fail with @VZErrorDeviceInitializationFailure@.    This method must be called on the virtual machine's queue.
--
-- @device@ — USB device to attach.
--
-- @completionHandler@ — Block called after the device has been attached or on error.    The error parameter passed to the block is nil if the attach was successful.    It will be also invoked on an virtual machine's queue.
--
-- See: VZUSBDevice
--
-- ObjC selector: @- attachDevice:completionHandler:@
attachDevice_completionHandler :: IsVZUSBController vzusbController => vzusbController -> RawId -> Ptr () -> IO ()
attachDevice_completionHandler vzusbController  device completionHandler =
  sendMsg vzusbController (mkSelector "attachDevice:completionHandler:") retVoid [argPtr (castPtr (unRawId device) :: Ptr ()), argPtr (castPtr completionHandler :: Ptr ())]

-- | Detach a USB device.
--
-- If the device is successfully detached from the controller, it will disappear from the usbDevices property,    its usbController property will be set to nil and completion handler will return nil.    If the device wasn't attached to the controller at the time of calling detach method, it will fail    with the @VZErrorDeviceNotFound@ error.    This method must be called on the virtual machine's queue.
--
-- @device@ — USB device to detach.
--
-- @completionHandler@ — Block called after the device has been detached or on error.    The error parameter passed to the block is nil if the detach was successful.    It will be also invoked on an virtual machine's queue.
--
-- See: VZUSBDevice
--
-- ObjC selector: @- detachDevice:completionHandler:@
detachDevice_completionHandler :: IsVZUSBController vzusbController => vzusbController -> RawId -> Ptr () -> IO ()
detachDevice_completionHandler vzusbController  device completionHandler =
  sendMsg vzusbController (mkSelector "detachDevice:completionHandler:") retVoid [argPtr (castPtr (unRawId device) :: Ptr ()), argPtr (castPtr completionHandler :: Ptr ())]

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @new@
newSelector :: Selector
newSelector = mkSelector "new"

-- | @Selector@ for @init@
initSelector :: Selector
initSelector = mkSelector "init"

-- | @Selector@ for @attachDevice:completionHandler:@
attachDevice_completionHandlerSelector :: Selector
attachDevice_completionHandlerSelector = mkSelector "attachDevice:completionHandler:"

-- | @Selector@ for @detachDevice:completionHandler:@
detachDevice_completionHandlerSelector :: Selector
detachDevice_completionHandlerSelector = mkSelector "detachDevice:completionHandler:"

