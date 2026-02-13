{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Configuration of a paravirtualized network device of type Virtio Network Device.
--
-- The communication channel used on the host is defined through the attachment. It is set with the VZNetworkDeviceConfiguration.attachment property.
--
-- The configuration is only valid with valid MACAddress and attachment.
--
-- See: VZVirtualMachineConfiguration.networkDevices
--
-- Generated bindings for @VZVirtioNetworkDeviceConfiguration@.
module ObjC.Virtualization.VZVirtioNetworkDeviceConfiguration
  ( VZVirtioNetworkDeviceConfiguration
  , IsVZVirtioNetworkDeviceConfiguration(..)
  , init_
  , initSelector


  ) where

import Foreign.Ptr (Ptr, FunPtr)
import Foreign.C.Types

import ObjC.Runtime.Types
import ObjC.Runtime.Message (sendMessage, sendOwnedMessage, sendClassMessage, sendOwnedClassMessage)
import ObjC.Runtime.Selector (mkSelector)
import ObjC.Runtime.Class (getRequiredClass)

import ObjC.Virtualization.Internal.Classes
import ObjC.Foundation.Internal.Classes

-- | @- init@
init_ :: IsVZVirtioNetworkDeviceConfiguration vzVirtioNetworkDeviceConfiguration => vzVirtioNetworkDeviceConfiguration -> IO (Id VZVirtioNetworkDeviceConfiguration)
init_ vzVirtioNetworkDeviceConfiguration =
  sendOwnedMessage vzVirtioNetworkDeviceConfiguration initSelector

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @init@
initSelector :: Selector '[] (Id VZVirtioNetworkDeviceConfiguration)
initSelector = mkSelector "init"

