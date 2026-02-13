{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Class representing a graphics device in a virtual machine.
--
-- VZGraphicsDevice should not be instantiated directly.
--
-- Graphics devices are first configured on the VZVirtualMachineConfiguration through a subclass of VZGraphicsDeviceConfiguration.    When a VZVirtualMachine is created from the configuration, the graphics devices are available through the VZVirtualMachine.graphicsDevices property.
--
-- The real type of VZGraphicsDevice corresponds to the type used by the configuration.    For example, a VZVirtioGraphicsDeviceConfiguration leads to a device of type VZVirtioGraphicsDevice.    And a VZMacGraphicsDeviceConfiguration leads to a device of type VZMacGraphicsDevice.
--
-- See: VZGraphicsDeviceConfiguration
--
-- Generated bindings for @VZGraphicsDevice@.
module ObjC.Virtualization.VZGraphicsDevice
  ( VZGraphicsDevice
  , IsVZGraphicsDevice(..)
  , new
  , init_
  , displays
  , displaysSelector
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
new :: IO (Id VZGraphicsDevice)
new  =
  do
    cls' <- getRequiredClass "VZGraphicsDevice"
    sendOwnedClassMessage cls' newSelector

-- | @- init@
init_ :: IsVZGraphicsDevice vzGraphicsDevice => vzGraphicsDevice -> IO (Id VZGraphicsDevice)
init_ vzGraphicsDevice =
  sendOwnedMessage vzGraphicsDevice initSelector

-- | Return the list of graphics displays configured for this graphics device.
--
-- Graphics displays are configured on the graphics device configuration.
--
-- See: VZMacGraphicsDisplayConfiguration
--
-- See: VZVirtioGraphicsScanoutConfiguration
--
-- ObjC selector: @- displays@
displays :: IsVZGraphicsDevice vzGraphicsDevice => vzGraphicsDevice -> IO (Id NSArray)
displays vzGraphicsDevice =
  sendMessage vzGraphicsDevice displaysSelector

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @new@
newSelector :: Selector '[] (Id VZGraphicsDevice)
newSelector = mkSelector "new"

-- | @Selector@ for @init@
initSelector :: Selector '[] (Id VZGraphicsDevice)
initSelector = mkSelector "init"

-- | @Selector@ for @displays@
displaysSelector :: Selector '[] (Id NSArray)
displaysSelector = mkSelector "displays"

