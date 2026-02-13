{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Class representing a Virtio graphics device scanout.
--
-- The VZVirtioGraphicsScanout is the runtime counterpart of VZVirtioGraphicsScanoutConfiguration.
--
-- When a graphics device is configured with class VZVirtioGraphicsScanoutConfiguration,    the VZGraphicsDevice's displays are in the same order as their configuration objects and they have the type VZVirtioGraphicsScanout.
--
-- For example, if when setting up a virtual machine, its @VZVirtioGraphicsDeviceConfiguration.scanouts[0]@ is a @VZVirtioGraphicsScanoutConfiguration@,    then after creating a virtual machine from the configuration, the @VZVirtualMachine.graphicsDevices@ is a @VZVirtioGraphicsDevice@.    The @VZVirtioGraphicsDevice.displays[0]@ is a @VZVirtioGraphicsScanout@ corresponding to the @VZVirtioGraphicsScanoutConfiguration@ in the configuration.
--
-- See: VZVirtioGraphicsScanoutConfiguration
--
-- See: VZGraphicsDisplay
--
-- See: VZGraphicsDevice
--
-- Generated bindings for @VZVirtioGraphicsScanout@.
module ObjC.Virtualization.VZVirtioGraphicsScanout
  ( VZVirtioGraphicsScanout
  , IsVZVirtioGraphicsScanout(..)


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

