{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | A Mac graphics device.
--
-- The VZMacGraphicsDevice is the runtime counterpart of VZMacGraphicsDeviceConfiguration.
--
-- For example, if a @VZVirtualMachineConfiguration.graphicsDevices[0]@ is an instance of @VZMacGraphicsDeviceConfiguration@, when creating the virtual Mac, the @VZVirtualMachine.graphicsDevices[0]@ is the corresponding @VZMacGraphicsDevice@.
--
-- An important property is the @displays@ inherited from @VZGraphicsDevice@. It provides the list of displays on the graphics device,    each corresponding to the display configuration set on @VZMacGraphicsDeviceConfiguration@.
--
-- See: VZMacGraphicsDeviceConfiguration
--
-- See: VZGraphicsDevice.displays
--
-- Generated bindings for @VZMacGraphicsDevice@.
module ObjC.Virtualization.VZMacGraphicsDevice
  ( VZMacGraphicsDevice
  , IsVZMacGraphicsDevice(..)


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

