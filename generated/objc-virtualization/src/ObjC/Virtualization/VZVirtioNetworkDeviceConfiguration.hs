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

-- | @- init@
init_ :: IsVZVirtioNetworkDeviceConfiguration vzVirtioNetworkDeviceConfiguration => vzVirtioNetworkDeviceConfiguration -> IO (Id VZVirtioNetworkDeviceConfiguration)
init_ vzVirtioNetworkDeviceConfiguration  =
  sendMsg vzVirtioNetworkDeviceConfiguration (mkSelector "init") (retPtr retVoid) [] >>= ownedObject . castPtr

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @init@
initSelector :: Selector
initSelector = mkSelector "init"

