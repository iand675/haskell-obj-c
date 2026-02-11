{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Class representing a network device in a virtual machine.
--
-- VZNetworkDevice should not be instantiated directly.
--
-- Network devices are first configured on the VZVirtualMachineConfiguration through a subclass of VZNetworkDeviceConfiguration.    When a VZVirtualMachine is created from the configuration, the network devices are available through the VZVirtualMachine.networkDevices property.
--
-- See: VZNetworkDeviceConfiguration
--
-- Generated bindings for @VZNetworkDevice@.
module ObjC.Virtualization.VZNetworkDevice
  ( VZNetworkDevice
  , IsVZNetworkDevice(..)
  , new
  , init_
  , attachment
  , setAttachment
  , newSelector
  , initSelector
  , attachmentSelector
  , setAttachmentSelector


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
new :: IO (Id VZNetworkDevice)
new  =
  do
    cls' <- getRequiredClass "VZNetworkDevice"
    sendClassMsg cls' (mkSelector "new") (retPtr retVoid) [] >>= ownedObject . castPtr

-- | @- init@
init_ :: IsVZNetworkDevice vzNetworkDevice => vzNetworkDevice -> IO (Id VZNetworkDevice)
init_ vzNetworkDevice  =
  sendMsg vzNetworkDevice (mkSelector "init") (retPtr retVoid) [] >>= ownedObject . castPtr

-- | The network attachment that's currently connected to this network device.
--
-- Setting this property will result in an attempt to change the network device attachment which may fail, in which case    the -[VZVirtualMachineDelegate virtualMachine:networkDevice:attachmentWasDisconnectedWithError:] will be invoked and this property    will be set to nil.
--
-- This property may change at any time while the VM is running based on the state of the host network.
--
-- ObjC selector: @- attachment@
attachment :: IsVZNetworkDevice vzNetworkDevice => vzNetworkDevice -> IO (Id VZNetworkDeviceAttachment)
attachment vzNetworkDevice  =
  sendMsg vzNetworkDevice (mkSelector "attachment") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | The network attachment that's currently connected to this network device.
--
-- Setting this property will result in an attempt to change the network device attachment which may fail, in which case    the -[VZVirtualMachineDelegate virtualMachine:networkDevice:attachmentWasDisconnectedWithError:] will be invoked and this property    will be set to nil.
--
-- This property may change at any time while the VM is running based on the state of the host network.
--
-- ObjC selector: @- setAttachment:@
setAttachment :: (IsVZNetworkDevice vzNetworkDevice, IsVZNetworkDeviceAttachment value) => vzNetworkDevice -> value -> IO ()
setAttachment vzNetworkDevice  value =
withObjCPtr value $ \raw_value ->
    sendMsg vzNetworkDevice (mkSelector "setAttachment:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @new@
newSelector :: Selector
newSelector = mkSelector "new"

-- | @Selector@ for @init@
initSelector :: Selector
initSelector = mkSelector "init"

-- | @Selector@ for @attachment@
attachmentSelector :: Selector
attachmentSelector = mkSelector "attachment"

-- | @Selector@ for @setAttachment:@
setAttachmentSelector :: Selector
setAttachmentSelector = mkSelector "setAttachment:"

