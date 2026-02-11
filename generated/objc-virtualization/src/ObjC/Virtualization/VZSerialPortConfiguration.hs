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
  , newSelector
  , initSelector
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
new :: IO (Id VZSerialPortConfiguration)
new  =
  do
    cls' <- getRequiredClass "VZSerialPortConfiguration"
    sendClassMsg cls' (mkSelector "new") (retPtr retVoid) [] >>= ownedObject . castPtr

-- | @- init@
init_ :: IsVZSerialPortConfiguration vzSerialPortConfiguration => vzSerialPortConfiguration -> IO (Id VZSerialPortConfiguration)
init_ vzSerialPortConfiguration  =
  sendMsg vzSerialPortConfiguration (mkSelector "init") (retPtr retVoid) [] >>= ownedObject . castPtr

-- | Serial port attachment. Defines how the virtual machine's serial port interfaces with the host system. Default is nil.
--
-- See: VZFileHandleSerialPortAttachment
--
-- See: VZFileSerialPortAttachment
--
-- ObjC selector: @- attachment@
attachment :: IsVZSerialPortConfiguration vzSerialPortConfiguration => vzSerialPortConfiguration -> IO (Id VZSerialPortAttachment)
attachment vzSerialPortConfiguration  =
  sendMsg vzSerialPortConfiguration (mkSelector "attachment") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | Serial port attachment. Defines how the virtual machine's serial port interfaces with the host system. Default is nil.
--
-- See: VZFileHandleSerialPortAttachment
--
-- See: VZFileSerialPortAttachment
--
-- ObjC selector: @- setAttachment:@
setAttachment :: (IsVZSerialPortConfiguration vzSerialPortConfiguration, IsVZSerialPortAttachment value) => vzSerialPortConfiguration -> value -> IO ()
setAttachment vzSerialPortConfiguration  value =
withObjCPtr value $ \raw_value ->
    sendMsg vzSerialPortConfiguration (mkSelector "setAttachment:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @new@
newSelector :: Selector
newSelector = mkSelector "new"

-- | @Selector@ for @init@
initSelector :: Selector
initSelector = mkSelector "init"

-- | @Selector@ for @attachment@
attachmentSelector :: Selector
attachmentSelector = mkSelector "attachment"

-- | @Selector@ for @setAttachment:@
setAttachmentSelector :: Selector
setAttachmentSelector = mkSelector "setAttachment:"

