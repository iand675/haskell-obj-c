{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Virtio Console Port
--
-- A console port is a two-way communication channel between a host VZSerialPortAttachment and a virtual machine console port. One or more console ports are attached to a Virtio console device.
--
-- An optional name may be set for a console port. A console port may also be configured for use as the system console.
--
-- See: VZConsolePortConfiguration
--
-- See: VZVirtualMachineConfiguration.consoleDevices
--
-- Generated bindings for @VZVirtioConsolePortConfiguration@.
module ObjC.Virtualization.VZVirtioConsolePortConfiguration
  ( VZVirtioConsolePortConfiguration
  , IsVZVirtioConsolePortConfiguration(..)
  , init_
  , name
  , setName
  , isConsole
  , setIsConsole
  , initSelector
  , isConsoleSelector
  , nameSelector
  , setIsConsoleSelector
  , setNameSelector


  ) where

import Foreign.Ptr (Ptr, FunPtr)
import Foreign.C.Types

import ObjC.Runtime.Types
import ObjC.Runtime.Message (sendMessage, sendOwnedMessage, sendClassMessage, sendOwnedClassMessage)
import ObjC.Runtime.Selector (mkSelector)
import ObjC.Runtime.Class (getRequiredClass)

import ObjC.Virtualization.Internal.Classes
import ObjC.Foundation.Internal.Classes

-- | @- init@
init_ :: IsVZVirtioConsolePortConfiguration vzVirtioConsolePortConfiguration => vzVirtioConsolePortConfiguration -> IO (Id VZVirtioConsolePortConfiguration)
init_ vzVirtioConsolePortConfiguration =
  sendOwnedMessage vzVirtioConsolePortConfiguration initSelector

-- | The console port's name. The default behavior is to not use a name unless set.
--
-- ObjC selector: @- name@
name :: IsVZVirtioConsolePortConfiguration vzVirtioConsolePortConfiguration => vzVirtioConsolePortConfiguration -> IO (Id NSString)
name vzVirtioConsolePortConfiguration =
  sendMessage vzVirtioConsolePortConfiguration nameSelector

-- | The console port's name. The default behavior is to not use a name unless set.
--
-- ObjC selector: @- setName:@
setName :: (IsVZVirtioConsolePortConfiguration vzVirtioConsolePortConfiguration, IsNSString value) => vzVirtioConsolePortConfiguration -> value -> IO ()
setName vzVirtioConsolePortConfiguration value =
  sendMessage vzVirtioConsolePortConfiguration setNameSelector (toNSString value)

-- | The console port may be marked for use as the system console. The default is false.
--
-- ObjC selector: @- isConsole@
isConsole :: IsVZVirtioConsolePortConfiguration vzVirtioConsolePortConfiguration => vzVirtioConsolePortConfiguration -> IO Bool
isConsole vzVirtioConsolePortConfiguration =
  sendMessage vzVirtioConsolePortConfiguration isConsoleSelector

-- | The console port may be marked for use as the system console. The default is false.
--
-- ObjC selector: @- setIsConsole:@
setIsConsole :: IsVZVirtioConsolePortConfiguration vzVirtioConsolePortConfiguration => vzVirtioConsolePortConfiguration -> Bool -> IO ()
setIsConsole vzVirtioConsolePortConfiguration value =
  sendMessage vzVirtioConsolePortConfiguration setIsConsoleSelector value

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @init@
initSelector :: Selector '[] (Id VZVirtioConsolePortConfiguration)
initSelector = mkSelector "init"

-- | @Selector@ for @name@
nameSelector :: Selector '[] (Id NSString)
nameSelector = mkSelector "name"

-- | @Selector@ for @setName:@
setNameSelector :: Selector '[Id NSString] ()
setNameSelector = mkSelector "setName:"

-- | @Selector@ for @isConsole@
isConsoleSelector :: Selector '[] Bool
isConsoleSelector = mkSelector "isConsole"

-- | @Selector@ for @setIsConsole:@
setIsConsoleSelector :: Selector '[Bool] ()
setIsConsoleSelector = mkSelector "setIsConsole:"

