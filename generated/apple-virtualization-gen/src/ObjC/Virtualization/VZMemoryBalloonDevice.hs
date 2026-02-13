{-# LANGUAGE DataKinds #-}
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
  , initSelector
  , newSelector


  ) where

import Foreign.Ptr (Ptr, FunPtr)
import Foreign.C.Types

import ObjC.Runtime.Types
import ObjC.Runtime.Message (sendMessage, sendOwnedMessage, sendClassMessage, sendOwnedClassMessage)
import ObjC.Runtime.Selector (mkSelector)
import ObjC.Runtime.Class (getRequiredClass)

import ObjC.Virtualization.Internal.Classes
import ObjC.Foundation.Internal.Classes

-- | @+ new@
new :: IO (Id VZMemoryBalloonDevice)
new  =
  do
    cls' <- getRequiredClass "VZMemoryBalloonDevice"
    sendOwnedClassMessage cls' newSelector

-- | @- init@
init_ :: IsVZMemoryBalloonDevice vzMemoryBalloonDevice => vzMemoryBalloonDevice -> IO (Id VZMemoryBalloonDevice)
init_ vzMemoryBalloonDevice =
  sendOwnedMessage vzMemoryBalloonDevice initSelector

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @new@
newSelector :: Selector '[] (Id VZMemoryBalloonDevice)
newSelector = mkSelector "new"

-- | @Selector@ for @init@
initSelector :: Selector '[] (Id VZMemoryBalloonDevice)
initSelector = mkSelector "init"

