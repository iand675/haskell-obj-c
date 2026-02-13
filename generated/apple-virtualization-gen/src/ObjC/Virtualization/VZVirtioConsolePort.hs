{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Class representing a Virtio console port in a virtual machine.
--
-- VZVirtioConsolePort should not be instantiated directly. This object can be retrieved from the VZVirtioConsoleDevice ports property.
--
-- Generated bindings for @VZVirtioConsolePort@.
module ObjC.Virtualization.VZVirtioConsolePort
  ( VZVirtioConsolePort
  , IsVZVirtioConsolePort(..)
  , new
  , init_
  , name
  , attachment
  , setAttachment
  , attachmentSelector
  , initSelector
  , nameSelector
  , newSelector
  , setAttachmentSelector


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
new :: IO (Id VZVirtioConsolePort)
new  =
  do
    cls' <- getRequiredClass "VZVirtioConsolePort"
    sendOwnedClassMessage cls' newSelector

-- | @- init@
init_ :: IsVZVirtioConsolePort vzVirtioConsolePort => vzVirtioConsolePort -> IO (Id VZVirtioConsolePort)
init_ vzVirtioConsolePort =
  sendOwnedMessage vzVirtioConsolePort initSelector

-- | The console port name currently being used by this port.
--
-- This property may not change while the VM is running. A null value indicates no name has been set.
--
-- ObjC selector: @- name@
name :: IsVZVirtioConsolePort vzVirtioConsolePort => vzVirtioConsolePort -> IO (Id NSString)
name vzVirtioConsolePort =
  sendMessage vzVirtioConsolePort nameSelector

-- | The console port attachment that's currently connected to this console port.
--
-- This property may change at any time while the VM is running.
--
-- ObjC selector: @- attachment@
attachment :: IsVZVirtioConsolePort vzVirtioConsolePort => vzVirtioConsolePort -> IO (Id VZSerialPortAttachment)
attachment vzVirtioConsolePort =
  sendMessage vzVirtioConsolePort attachmentSelector

-- | The console port attachment that's currently connected to this console port.
--
-- This property may change at any time while the VM is running.
--
-- ObjC selector: @- setAttachment:@
setAttachment :: (IsVZVirtioConsolePort vzVirtioConsolePort, IsVZSerialPortAttachment value) => vzVirtioConsolePort -> value -> IO ()
setAttachment vzVirtioConsolePort value =
  sendMessage vzVirtioConsolePort setAttachmentSelector (toVZSerialPortAttachment value)

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @new@
newSelector :: Selector '[] (Id VZVirtioConsolePort)
newSelector = mkSelector "new"

-- | @Selector@ for @init@
initSelector :: Selector '[] (Id VZVirtioConsolePort)
initSelector = mkSelector "init"

-- | @Selector@ for @name@
nameSelector :: Selector '[] (Id NSString)
nameSelector = mkSelector "name"

-- | @Selector@ for @attachment@
attachmentSelector :: Selector '[] (Id VZSerialPortAttachment)
attachmentSelector = mkSelector "attachment"

-- | @Selector@ for @setAttachment:@
setAttachmentSelector :: Selector '[Id VZSerialPortAttachment] ()
setAttachmentSelector = mkSelector "setAttachment:"

