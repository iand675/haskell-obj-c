{-# LANGUAGE DataKinds #-}
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

import Foreign.Ptr (Ptr, FunPtr)
import Foreign.C.Types

import ObjC.Runtime.Types
import ObjC.Runtime.Message (sendMessage, sendOwnedMessage, sendClassMessage, sendOwnedClassMessage)
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
initWithInterface vzBridgedNetworkDeviceAttachment interface =
  sendOwnedMessage vzBridgedNetworkDeviceAttachment initWithInterfaceSelector (toVZBridgedNetworkInterface interface)

-- | Network interface of this device attachment.
--
-- ObjC selector: @- interface@
interface :: IsVZBridgedNetworkDeviceAttachment vzBridgedNetworkDeviceAttachment => vzBridgedNetworkDeviceAttachment -> IO (Id VZBridgedNetworkInterface)
interface vzBridgedNetworkDeviceAttachment =
  sendMessage vzBridgedNetworkDeviceAttachment interfaceSelector

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @initWithInterface:@
initWithInterfaceSelector :: Selector '[Id VZBridgedNetworkInterface] (Id VZBridgedNetworkDeviceAttachment)
initWithInterfaceSelector = mkSelector "initWithInterface:"

-- | @Selector@ for @interface@
interfaceSelector :: Selector '[] (Id VZBridgedNetworkInterface)
interfaceSelector = mkSelector "interface"

