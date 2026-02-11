{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Network device attachment that allows custom network topology.
--
-- This attachment is backed by a logical network which is created and customized through    the vmnet framework APIs to allow custom network topology, which allows multiple virtual    machines to appear on the same network and connect with each other.
--
-- See: VZNetworkDeviceConfiguration
--
-- See: VZVirtioNetworkDeviceConfiguration
--
-- Generated bindings for @VZVmnetNetworkDeviceAttachment@.
module ObjC.Virtualization.VZVmnetNetworkDeviceAttachment
  ( VZVmnetNetworkDeviceAttachment
  , IsVZVmnetNetworkDeviceAttachment(..)
  , initWithNetwork
  , network
  , initWithNetworkSelector
  , networkSelector


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

-- | Initialize a @VZVmnetNetworkDeviceAttachment@ with a logical network.
--
-- @network@ — The logical network object.
--
-- Returns: An initialized @VZVmnetNetworkDeviceAttachment@.
--
-- To ensure proper isolation between application processes, a VM can only use the @network@ that is created by the    same application process. If a VM created by one application tries to use a @network@ created by another application,    starting the interface with this attachment will fail.
--
-- The vmnet framework requires an entitlement to create or configure a network. For more information, please    refer to the vmnet framework API documentation.
--
-- An example use of this API is:    ```    vmnet_return_t status;    auto network_configuration = vmnet_network_configuration_create(VMNET_SHARED_MODE, &status);    if (!network_configuration) {        // Handle error return @status@.    }
--
-- auto network = vmnet_network_create(network_configuration, &status);    if (!network) {        // Handle error return @status@.    }
--
-- VZVmnetNetworkDeviceAttachment *attachment = [[VZVmnetNetworkDeviceAttachment alloc] initWithNetwork:network];
--
-- VZVirtioNetworkDeviceConfiguration virtioNetworkDevice = [[VZVirtioNetworkDeviceConfiguration alloc] init];    virtioNetworkDevice.attachment = attachment;    ```
--
-- ObjC selector: @- initWithNetwork:@
initWithNetwork :: IsVZVmnetNetworkDeviceAttachment vzVmnetNetworkDeviceAttachment => vzVmnetNetworkDeviceAttachment -> Ptr () -> IO (Id VZVmnetNetworkDeviceAttachment)
initWithNetwork vzVmnetNetworkDeviceAttachment  network =
  sendMsg vzVmnetNetworkDeviceAttachment (mkSelector "initWithNetwork:") (retPtr retVoid) [argPtr network] >>= ownedObject . castPtr

-- | The network object that the attachment will be initialized with.
--
-- ObjC selector: @- network@
network :: IsVZVmnetNetworkDeviceAttachment vzVmnetNetworkDeviceAttachment => vzVmnetNetworkDeviceAttachment -> IO (Ptr ())
network vzVmnetNetworkDeviceAttachment  =
  fmap castPtr $ sendMsg vzVmnetNetworkDeviceAttachment (mkSelector "network") (retPtr retVoid) []

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @initWithNetwork:@
initWithNetworkSelector :: Selector
initWithNetworkSelector = mkSelector "initWithNetwork:"

-- | @Selector@ for @network@
networkSelector :: Selector
networkSelector = mkSelector "network"

