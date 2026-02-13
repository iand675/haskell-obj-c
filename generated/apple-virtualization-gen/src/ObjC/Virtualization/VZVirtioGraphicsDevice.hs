{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | A Virtio graphics device.
--
-- The VZVirtioGraphicsDevice is the runtime counterpart of VZVirtioGraphicsDeviceConfiguration.
--
-- For example, if a @VZVirtualMachineConfiguration.graphicsDevices[0]@ is an instance of @VZVirtioGraphicsDeviceConfiguration@, when creating the virtual machine, the @VZVirtualMachine.graphicsDevices[0]@ is the corresponding @VZVirtioGraphicsDevice@.
--
-- An important property is the @displays@ inherited from @VZGraphicsDevice@. It provides the list of scanouts on the graphics device,    each corresponding to the scanout configuration set on @VZVirtioGraphicsDeviceConfiguration@.
--
-- See: VZVirtioGraphicsDeviceConfiguration
--
-- See: VZGraphicsDevice.displays
--
-- Generated bindings for @VZVirtioGraphicsDevice@.
module ObjC.Virtualization.VZVirtioGraphicsDevice
  ( VZVirtioGraphicsDevice
  , IsVZVirtioGraphicsDevice(..)


  ) where

import Foreign.Ptr (Ptr, FunPtr)
import Foreign.C.Types

import ObjC.Runtime.Types
import ObjC.Runtime.Message (sendMessage, sendOwnedMessage, sendClassMessage, sendOwnedClassMessage)
import ObjC.Runtime.Selector (mkSelector)
import ObjC.Runtime.Class (getRequiredClass)

import ObjC.Virtualization.Internal.Classes
import ObjC.Foundation.Internal.Classes

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

