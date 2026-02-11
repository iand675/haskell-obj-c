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
  , nameSelector
  , setNameSelector
  , isConsoleSelector
  , setIsConsoleSelector


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

-- | @- init@
init_ :: IsVZVirtioConsolePortConfiguration vzVirtioConsolePortConfiguration => vzVirtioConsolePortConfiguration -> IO (Id VZVirtioConsolePortConfiguration)
init_ vzVirtioConsolePortConfiguration  =
  sendMsg vzVirtioConsolePortConfiguration (mkSelector "init") (retPtr retVoid) [] >>= ownedObject . castPtr

-- | The console port's name. The default behavior is to not use a name unless set.
--
-- ObjC selector: @- name@
name :: IsVZVirtioConsolePortConfiguration vzVirtioConsolePortConfiguration => vzVirtioConsolePortConfiguration -> IO (Id NSString)
name vzVirtioConsolePortConfiguration  =
  sendMsg vzVirtioConsolePortConfiguration (mkSelector "name") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | The console port's name. The default behavior is to not use a name unless set.
--
-- ObjC selector: @- setName:@
setName :: (IsVZVirtioConsolePortConfiguration vzVirtioConsolePortConfiguration, IsNSString value) => vzVirtioConsolePortConfiguration -> value -> IO ()
setName vzVirtioConsolePortConfiguration  value =
withObjCPtr value $ \raw_value ->
    sendMsg vzVirtioConsolePortConfiguration (mkSelector "setName:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- | The console port may be marked for use as the system console. The default is false.
--
-- ObjC selector: @- isConsole@
isConsole :: IsVZVirtioConsolePortConfiguration vzVirtioConsolePortConfiguration => vzVirtioConsolePortConfiguration -> IO Bool
isConsole vzVirtioConsolePortConfiguration  =
  fmap ((/= 0) :: CULong -> Bool) $ sendMsg vzVirtioConsolePortConfiguration (mkSelector "isConsole") retCULong []

-- | The console port may be marked for use as the system console. The default is false.
--
-- ObjC selector: @- setIsConsole:@
setIsConsole :: IsVZVirtioConsolePortConfiguration vzVirtioConsolePortConfiguration => vzVirtioConsolePortConfiguration -> Bool -> IO ()
setIsConsole vzVirtioConsolePortConfiguration  value =
  sendMsg vzVirtioConsolePortConfiguration (mkSelector "setIsConsole:") retVoid [argCULong (if value then 1 else 0)]

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @init@
initSelector :: Selector
initSelector = mkSelector "init"

-- | @Selector@ for @name@
nameSelector :: Selector
nameSelector = mkSelector "name"

-- | @Selector@ for @setName:@
setNameSelector :: Selector
setNameSelector = mkSelector "setName:"

-- | @Selector@ for @isConsole@
isConsoleSelector :: Selector
isConsoleSelector = mkSelector "isConsole"

-- | @Selector@ for @setIsConsole:@
setIsConsoleSelector :: Selector
setIsConsoleSelector = mkSelector "setIsConsole:"

