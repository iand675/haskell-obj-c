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
  , newSelector
  , initSelector
  , displaysSelector


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
new :: IO (Id VZGraphicsDevice)
new  =
  do
    cls' <- getRequiredClass "VZGraphicsDevice"
    sendClassMsg cls' (mkSelector "new") (retPtr retVoid) [] >>= ownedObject . castPtr

-- | @- init@
init_ :: IsVZGraphicsDevice vzGraphicsDevice => vzGraphicsDevice -> IO (Id VZGraphicsDevice)
init_ vzGraphicsDevice  =
  sendMsg vzGraphicsDevice (mkSelector "init") (retPtr retVoid) [] >>= ownedObject . castPtr

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
displays vzGraphicsDevice  =
  sendMsg vzGraphicsDevice (mkSelector "displays") (retPtr retVoid) [] >>= retainedObject . castPtr

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @new@
newSelector :: Selector
newSelector = mkSelector "new"

-- | @Selector@ for @init@
initSelector :: Selector
initSelector = mkSelector "init"

-- | @Selector@ for @displays@
displaysSelector :: Selector
displaysSelector = mkSelector "displays"

