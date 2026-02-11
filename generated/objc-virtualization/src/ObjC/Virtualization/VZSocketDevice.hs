{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Base class representing a socket device in a virtual machine.
--
-- VZSocketDevice should not be instantiated directly.
--
-- Socket devices are first configured on the VZVirtualMachineConfiguration through a subclass of VZSocketDeviceConfiguration.    When a VZVirtualMachine is created from the configuration, the socket devices are available through the VZVirtualMachine.socketDevices property.
--
-- The real type of VZSocketDevice corresponds to the type used by the configuration.    For example, a VZVirtioSocketDeviceConfiguration leads to a device of type VZVirtioSocketDevice.
--
-- See: VZVirtioSocketDevice
--
-- See: VZVirtioSocketDeviceConfiguration
--
-- Generated bindings for @VZSocketDevice@.
module ObjC.Virtualization.VZSocketDevice
  ( VZSocketDevice
  , IsVZSocketDevice(..)
  , new
  , init_
  , newSelector
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

-- | @+ new@
new :: IO (Id VZSocketDevice)
new  =
  do
    cls' <- getRequiredClass "VZSocketDevice"
    sendClassMsg cls' (mkSelector "new") (retPtr retVoid) [] >>= ownedObject . castPtr

-- | @- init@
init_ :: IsVZSocketDevice vzSocketDevice => vzSocketDevice -> IO (Id VZSocketDevice)
init_ vzSocketDevice  =
  sendMsg vzSocketDevice (mkSelector "init") (retPtr retVoid) [] >>= ownedObject . castPtr

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @new@
newSelector :: Selector
newSelector = mkSelector "new"

-- | @Selector@ for @init@
initSelector :: Selector
initSelector = mkSelector "init"

