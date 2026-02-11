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
  , newSelector
  , initSelector
  , nameSelector
  , attachmentSelector
  , setAttachmentSelector


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
new :: IO (Id VZVirtioConsolePort)
new  =
  do
    cls' <- getRequiredClass "VZVirtioConsolePort"
    sendClassMsg cls' (mkSelector "new") (retPtr retVoid) [] >>= ownedObject . castPtr

-- | @- init@
init_ :: IsVZVirtioConsolePort vzVirtioConsolePort => vzVirtioConsolePort -> IO (Id VZVirtioConsolePort)
init_ vzVirtioConsolePort  =
  sendMsg vzVirtioConsolePort (mkSelector "init") (retPtr retVoid) [] >>= ownedObject . castPtr

-- | The console port name currently being used by this port.
--
-- This property may not change while the VM is running. A null value indicates no name has been set.
--
-- ObjC selector: @- name@
name :: IsVZVirtioConsolePort vzVirtioConsolePort => vzVirtioConsolePort -> IO (Id NSString)
name vzVirtioConsolePort  =
  sendMsg vzVirtioConsolePort (mkSelector "name") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | The console port attachment that's currently connected to this console port.
--
-- This property may change at any time while the VM is running.
--
-- ObjC selector: @- attachment@
attachment :: IsVZVirtioConsolePort vzVirtioConsolePort => vzVirtioConsolePort -> IO (Id VZSerialPortAttachment)
attachment vzVirtioConsolePort  =
  sendMsg vzVirtioConsolePort (mkSelector "attachment") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | The console port attachment that's currently connected to this console port.
--
-- This property may change at any time while the VM is running.
--
-- ObjC selector: @- setAttachment:@
setAttachment :: (IsVZVirtioConsolePort vzVirtioConsolePort, IsVZSerialPortAttachment value) => vzVirtioConsolePort -> value -> IO ()
setAttachment vzVirtioConsolePort  value =
withObjCPtr value $ \raw_value ->
    sendMsg vzVirtioConsolePort (mkSelector "setAttachment:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @new@
newSelector :: Selector
newSelector = mkSelector "new"

-- | @Selector@ for @init@
initSelector :: Selector
initSelector = mkSelector "init"

-- | @Selector@ for @name@
nameSelector :: Selector
nameSelector = mkSelector "name"

-- | @Selector@ for @attachment@
attachmentSelector :: Selector
attachmentSelector = mkSelector "attachment"

-- | @Selector@ for @setAttachment:@
setAttachmentSelector :: Selector
setAttachmentSelector = mkSelector "setAttachment:"

