{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Class representing a Virtio console device in a virtual machine.
--
-- VZVirtioConsoleDevice should not be instantiated directly.
--
-- See: VZConsoleDeviceConfiguration
--
-- Generated bindings for @VZVirtioConsoleDevice@.
module ObjC.Virtualization.VZVirtioConsoleDevice
  ( VZVirtioConsoleDevice
  , IsVZVirtioConsoleDevice(..)
  , new
  , init_
  , ports
  , newSelector
  , initSelector
  , portsSelector


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
new :: IO (Id VZVirtioConsoleDevice)
new  =
  do
    cls' <- getRequiredClass "VZVirtioConsoleDevice"
    sendClassMsg cls' (mkSelector "new") (retPtr retVoid) [] >>= ownedObject . castPtr

-- | @- init@
init_ :: IsVZVirtioConsoleDevice vzVirtioConsoleDevice => vzVirtioConsoleDevice -> IO (Id VZVirtioConsoleDevice)
init_ vzVirtioConsoleDevice  =
  sendMsg vzVirtioConsoleDevice (mkSelector "init") (retPtr retVoid) [] >>= ownedObject . castPtr

-- | The console ports currently being used by this console device.
--
-- ObjC selector: @- ports@
ports :: IsVZVirtioConsoleDevice vzVirtioConsoleDevice => vzVirtioConsoleDevice -> IO (Id VZVirtioConsolePortArray)
ports vzVirtioConsoleDevice  =
  sendMsg vzVirtioConsoleDevice (mkSelector "ports") (retPtr retVoid) [] >>= retainedObject . castPtr

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @new@
newSelector :: Selector
newSelector = mkSelector "new"

-- | @Selector@ for @init@
initSelector :: Selector
initSelector = mkSelector "init"

-- | @Selector@ for @ports@
portsSelector :: Selector
portsSelector = mkSelector "ports"

