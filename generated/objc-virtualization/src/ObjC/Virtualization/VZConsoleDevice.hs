{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Class representing a console device in a virtual machine.
--
-- VZConsoleDevice should not be instantiated directly.
--
-- Console devices are first configured on the VZVirtualMachineConfiguration through a subclass of VZConsoleDeviceConfiguration.    When a VZVirtualMachine is created from the configuration, the console devices are available through the VZVirtualMachine.consoleDevices property.
--
-- The real type of VZConsoleDevice corresponds to the type used by the configuration.    For example, a VZVirtioConsoleDeviceConfiguration leads to a device of type VZVirtioConsoleDevice.
--
-- See: VZConsoleDeviceConfiguration
--
-- Generated bindings for @VZConsoleDevice@.
module ObjC.Virtualization.VZConsoleDevice
  ( VZConsoleDevice
  , IsVZConsoleDevice(..)
  , new
  , init_
  , newSelector
  , initSelector


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
new :: IO (Id VZConsoleDevice)
new  =
  do
    cls' <- getRequiredClass "VZConsoleDevice"
    sendClassMsg cls' (mkSelector "new") (retPtr retVoid) [] >>= ownedObject . castPtr

-- | @- init@
init_ :: IsVZConsoleDevice vzConsoleDevice => vzConsoleDevice -> IO (Id VZConsoleDevice)
init_ vzConsoleDevice  =
  sendMsg vzConsoleDevice (mkSelector "init") (retPtr retVoid) [] >>= ownedObject . castPtr

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @new@
newSelector :: Selector
newSelector = mkSelector "new"

-- | @Selector@ for @init@
initSelector :: Selector
initSelector = mkSelector "init"

