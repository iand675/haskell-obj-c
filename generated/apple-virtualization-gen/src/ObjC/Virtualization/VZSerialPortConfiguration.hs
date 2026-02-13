{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Base class for a serial port configuration.
--
-- VZSerialPortConfiguration should not be instantiated directly.    One of its subclasses like VZVirtioConsoleDeviceSerialPortConfiguration should be used instead.
--
-- See: VZVirtioConsoleDeviceSerialPortConfiguration
--
-- Generated bindings for @VZSerialPortConfiguration@.
module ObjC.Virtualization.VZSerialPortConfiguration
  ( VZSerialPortConfiguration
  , IsVZSerialPortConfiguration(..)
  , new
  , init_
  , attachment
  , setAttachment
  , attachmentSelector
  , initSelector
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
new :: IO (Id VZSerialPortConfiguration)
new  =
  do
    cls' <- getRequiredClass "VZSerialPortConfiguration"
    sendOwnedClassMessage cls' newSelector

-- | @- init@
init_ :: IsVZSerialPortConfiguration vzSerialPortConfiguration => vzSerialPortConfiguration -> IO (Id VZSerialPortConfiguration)
init_ vzSerialPortConfiguration =
  sendOwnedMessage vzSerialPortConfiguration initSelector

-- | Serial port attachment. Defines how the virtual machine's serial port interfaces with the host system. Default is nil.
--
-- See: VZFileHandleSerialPortAttachment
--
-- See: VZFileSerialPortAttachment
--
-- ObjC selector: @- attachment@
attachment :: IsVZSerialPortConfiguration vzSerialPortConfiguration => vzSerialPortConfiguration -> IO (Id VZSerialPortAttachment)
attachment vzSerialPortConfiguration =
  sendMessage vzSerialPortConfiguration attachmentSelector

-- | Serial port attachment. Defines how the virtual machine's serial port interfaces with the host system. Default is nil.
--
-- See: VZFileHandleSerialPortAttachment
--
-- See: VZFileSerialPortAttachment
--
-- ObjC selector: @- setAttachment:@
setAttachment :: (IsVZSerialPortConfiguration vzSerialPortConfiguration, IsVZSerialPortAttachment value) => vzSerialPortConfiguration -> value -> IO ()
setAttachment vzSerialPortConfiguration value =
  sendMessage vzSerialPortConfiguration setAttachmentSelector (toVZSerialPortAttachment value)

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @new@
newSelector :: Selector '[] (Id VZSerialPortConfiguration)
newSelector = mkSelector "new"

-- | @Selector@ for @init@
initSelector :: Selector '[] (Id VZSerialPortConfiguration)
initSelector = mkSelector "init"

-- | @Selector@ for @attachment@
attachmentSelector :: Selector '[] (Id VZSerialPortAttachment)
attachmentSelector = mkSelector "attachment"

-- | @Selector@ for @setAttachment:@
setAttachmentSelector :: Selector '[Id VZSerialPortAttachment] ()
setAttachmentSelector = mkSelector "setAttachment:"

