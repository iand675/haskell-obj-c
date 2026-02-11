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

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

