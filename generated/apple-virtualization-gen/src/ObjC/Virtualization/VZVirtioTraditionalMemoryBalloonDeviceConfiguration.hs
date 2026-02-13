{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Configuration of the Virtio traditional memory balloon device.
--
-- This configuration creates a Virtio traditional memory balloon device which allows for managing guest memory.    Only one Virtio traditional memory balloon device can be used per virtual machine.
--
-- See: VZVirtioTraditionalMemoryBalloonDevice
--
-- Generated bindings for @VZVirtioTraditionalMemoryBalloonDeviceConfiguration@.
module ObjC.Virtualization.VZVirtioTraditionalMemoryBalloonDeviceConfiguration
  ( VZVirtioTraditionalMemoryBalloonDeviceConfiguration
  , IsVZVirtioTraditionalMemoryBalloonDeviceConfiguration(..)
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
init_ :: IsVZVirtioTraditionalMemoryBalloonDeviceConfiguration vzVirtioTraditionalMemoryBalloonDeviceConfiguration => vzVirtioTraditionalMemoryBalloonDeviceConfiguration -> IO (Id VZVirtioTraditionalMemoryBalloonDeviceConfiguration)
init_ vzVirtioTraditionalMemoryBalloonDeviceConfiguration =
  sendOwnedMessage vzVirtioTraditionalMemoryBalloonDeviceConfiguration initSelector

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @init@
initSelector :: Selector '[] (Id VZVirtioTraditionalMemoryBalloonDeviceConfiguration)
initSelector = mkSelector "init"

