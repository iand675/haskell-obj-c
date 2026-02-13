{-# LANGUAGE DataKinds #-}
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
  , delegate
  , setDelegate
  , ports
  , delegateSelector
  , initSelector
  , newSelector
  , portsSelector
  , setDelegateSelector


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
new :: IO (Id VZVirtioConsoleDevice)
new  =
  do
    cls' <- getRequiredClass "VZVirtioConsoleDevice"
    sendOwnedClassMessage cls' newSelector

-- | @- init@
init_ :: IsVZVirtioConsoleDevice vzVirtioConsoleDevice => vzVirtioConsoleDevice -> IO (Id VZVirtioConsoleDevice)
init_ vzVirtioConsoleDevice =
  sendOwnedMessage vzVirtioConsoleDevice initSelector

-- | Pointer to a delegate object for the console device.
--
-- ObjC selector: @- delegate@
delegate :: IsVZVirtioConsoleDevice vzVirtioConsoleDevice => vzVirtioConsoleDevice -> IO RawId
delegate vzVirtioConsoleDevice =
  sendMessage vzVirtioConsoleDevice delegateSelector

-- | Pointer to a delegate object for the console device.
--
-- ObjC selector: @- setDelegate:@
setDelegate :: IsVZVirtioConsoleDevice vzVirtioConsoleDevice => vzVirtioConsoleDevice -> RawId -> IO ()
setDelegate vzVirtioConsoleDevice value =
  sendMessage vzVirtioConsoleDevice setDelegateSelector value

-- | The console ports currently being used by this console device.
--
-- ObjC selector: @- ports@
ports :: IsVZVirtioConsoleDevice vzVirtioConsoleDevice => vzVirtioConsoleDevice -> IO (Id VZVirtioConsolePortArray)
ports vzVirtioConsoleDevice =
  sendMessage vzVirtioConsoleDevice portsSelector

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @new@
newSelector :: Selector '[] (Id VZVirtioConsoleDevice)
newSelector = mkSelector "new"

-- | @Selector@ for @init@
initSelector :: Selector '[] (Id VZVirtioConsoleDevice)
initSelector = mkSelector "init"

-- | @Selector@ for @delegate@
delegateSelector :: Selector '[] RawId
delegateSelector = mkSelector "delegate"

-- | @Selector@ for @setDelegate:@
setDelegateSelector :: Selector '[RawId] ()
setDelegateSelector = mkSelector "setDelegate:"

-- | @Selector@ for @ports@
portsSelector :: Selector '[] (Id VZVirtioConsolePortArray)
portsSelector = mkSelector "ports"

