{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Class representing a virtual Mac graphics display.
--
-- The VZMacGraphicsDisplay is the runtime counterpart of VZMacGraphicsDisplayConfiguration.
--
-- When a graphics device is configured with class VZMacGraphicsDisplayConfiguration,    the VZGraphicsDevice's displays are in the same order as their configuration objects and they have the type VZMacGraphicsDisplay.
--
-- For example, if when setting up a virtual Mac, @VZMacGraphicsDeviceConfiguration.displays[0]@ is a @VZMacGraphicsDisplayConfiguration@,    then after creating a virtual machine from the configuration, the @VZVirtualMachine.graphicsDevices@ is a @VZMacGraphicsDevice@.    The @VZMacGraphicsDevice.displays[0]@ is a @VZMacGraphicsDisplay@ corresponding to the @VZMacGraphicsDeviceConfiguration@ in the configuration.
--
-- See: VZMacGraphicsDisplayConfiguration
--
-- See: VZGraphicsDevice
--
-- Generated bindings for @VZMacGraphicsDisplay@.
module ObjC.Virtualization.VZMacGraphicsDisplay
  ( VZMacGraphicsDisplay
  , IsVZMacGraphicsDisplay(..)
  , pixelsPerInch
  , pixelsPerInchSelector


  ) where

import Foreign.Ptr (Ptr, FunPtr)
import Foreign.C.Types

import ObjC.Runtime.Types
import ObjC.Runtime.Message (sendMessage, sendOwnedMessage, sendClassMessage, sendOwnedClassMessage)
import ObjC.Runtime.Selector (mkSelector)
import ObjC.Runtime.Class (getRequiredClass)

import ObjC.Virtualization.Internal.Classes
import ObjC.Foundation.Internal.Classes

-- | The pixel density as a number of pixels per inch.
--
-- ObjC selector: @- pixelsPerInch@
pixelsPerInch :: IsVZMacGraphicsDisplay vzMacGraphicsDisplay => vzMacGraphicsDisplay -> IO CLong
pixelsPerInch vzMacGraphicsDisplay =
  sendMessage vzMacGraphicsDisplay pixelsPerInchSelector

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @pixelsPerInch@
pixelsPerInchSelector :: Selector '[] CLong
pixelsPerInchSelector = mkSelector "pixelsPerInch"

