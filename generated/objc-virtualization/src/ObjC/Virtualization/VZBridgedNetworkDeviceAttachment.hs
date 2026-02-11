{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Network device attachment bridging a host physical interface with a virtual network device.
--
-- A bridged network allows the virtual machine to use the same physical interface as the host. Both host and virtual machine    send and receive packets on the same physical interface but have distinct network layers.
--
-- The bridge network device attachment is used with a VZNetworkDeviceConfiguration to define a virtual network device.
--
-- Using a VZBridgedNetworkDeviceAttachment requires the app to have the "com.apple.vm.networking" entitlement.
--
-- See: VZBridgedNetworkInterface
--
-- See: VZNetworkDeviceConfiguration
--
-- See: VZVirtioNetworkDeviceConfiguration
--
-- Generated bindings for @VZBridgedNetworkDeviceAttachment@.
module ObjC.Virtualization.VZBridgedNetworkDeviceAttachment
  ( VZBridgedNetworkDeviceAttachment
  , IsVZBridgedNetworkDeviceAttachment(..)
  , initWithInterface
  , interface
  , initWithInterfaceSelector
  , interfaceSelector


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

-- | Initialize a VZBridgedNetworkDeviceAttachment with a host network interface.
--
-- @interface@ — Host network interface controller.
--
-- ObjC selector: @- initWithInterface:@
initWithInterface :: (IsVZBridgedNetworkDeviceAttachment vzBridgedNetworkDeviceAttachment, IsVZBridgedNetworkInterface interface) => vzBridgedNetworkDeviceAttachment -> interface -> IO (Id VZBridgedNetworkDeviceAttachment)
initWithInterface vzBridgedNetworkDeviceAttachment  interface =
withObjCPtr interface $ \raw_interface ->
    sendMsg vzBridgedNetworkDeviceAttachment (mkSelector "initWithInterface:") (retPtr retVoid) [argPtr (castPtr raw_interface :: Ptr ())] >>= ownedObject . castPtr

-- | Network interface of this device attachment.
--
-- ObjC selector: @- interface@
interface :: IsVZBridgedNetworkDeviceAttachment vzBridgedNetworkDeviceAttachment => vzBridgedNetworkDeviceAttachment -> IO (Id VZBridgedNetworkInterface)
interface vzBridgedNetworkDeviceAttachment  =
  sendMsg vzBridgedNetworkDeviceAttachment (mkSelector "interface") (retPtr retVoid) [] >>= retainedObject . castPtr

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @initWithInterface:@
initWithInterfaceSelector :: Selector
initWithInterfaceSelector = mkSelector "initWithInterface:"

-- | @Selector@ for @interface@
interfaceSelector :: Selector
interfaceSelector = mkSelector "interface"

