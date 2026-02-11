{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Base class representing a memory balloon device in a virtual machine.
--
-- VZMemoryBalloonDevice should not be instantiated directly.
--
-- Memory balloon devices are first configured on the VZVirtualMachineConfiguration through a subclass of VZMemoryBalloonDeviceConfiguration.    When a VZVirtualMachine is created from the configuration, the memory balloon devices are available through the VZVirtualMachine.memoryBalloonDevices property.
--
-- The real type of VZMemoryBalloonDevice corresponds to the type used by the configuration.    For example, a VZVirtioTraditionalMemoryBalloonDeviceConfiguration leads to a device of type VZVirtioTraditionalMemoryBalloonDevice.
--
-- See: VZVirtioTraditionalMemoryBalloonDevice
--
-- See: VZVirtioTraditionalMemoryBalloonDeviceConfiguration
--
-- Generated bindings for @VZMemoryBalloonDevice@.
module ObjC.Virtualization.VZMemoryBalloonDevice
  ( VZMemoryBalloonDevice
  , IsVZMemoryBalloonDevice(..)
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
new :: IO (Id VZMemoryBalloonDevice)
new  =
  do
    cls' <- getRequiredClass "VZMemoryBalloonDevice"
    sendClassMsg cls' (mkSelector "new") (retPtr retVoid) [] >>= ownedObject . castPtr

-- | @- init@
init_ :: IsVZMemoryBalloonDevice vzMemoryBalloonDevice => vzMemoryBalloonDevice -> IO (Id VZMemoryBalloonDevice)
init_ vzMemoryBalloonDevice  =
  sendMsg vzMemoryBalloonDevice (mkSelector "init") (retPtr retVoid) [] >>= ownedObject . castPtr

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @new@
newSelector :: Selector
newSelector = mkSelector "new"

-- | @Selector@ for @init@
initSelector :: Selector
initSelector = mkSelector "init"

