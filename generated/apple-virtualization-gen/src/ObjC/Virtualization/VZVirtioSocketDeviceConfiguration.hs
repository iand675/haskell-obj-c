{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Configuration of the Virtio socket device.
--
-- This configuration creates a Virtio socket device for the guest which communicates with the host through the Virtio interface.
--
-- Only one Virtio socket device can be used per virtual machine.
--
-- See: VZVirtioSocketDevice
--
-- Generated bindings for @VZVirtioSocketDeviceConfiguration@.
module ObjC.Virtualization.VZVirtioSocketDeviceConfiguration
  ( VZVirtioSocketDeviceConfiguration
  , IsVZVirtioSocketDeviceConfiguration(..)
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
init_ :: IsVZVirtioSocketDeviceConfiguration vzVirtioSocketDeviceConfiguration => vzVirtioSocketDeviceConfiguration -> IO (Id VZVirtioSocketDeviceConfiguration)
init_ vzVirtioSocketDeviceConfiguration =
  sendOwnedMessage vzVirtioSocketDeviceConfiguration initSelector

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @init@
initSelector :: Selector '[] (Id VZVirtioSocketDeviceConfiguration)
initSelector = mkSelector "init"

