{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Base class for a console port configuration.
--
-- VZConsolePortConfiguration should not be instantiated directly.    One of its subclasses like VZVirtioConsolePortConfiguration should be used instead.
--
-- See: VZVirtioConsolePortConfiguration
--
-- Generated bindings for @VZConsolePortConfiguration@.
module ObjC.Virtualization.VZConsolePortConfiguration
  ( VZConsolePortConfiguration
  , IsVZConsolePortConfiguration(..)
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
new :: IO (Id VZConsolePortConfiguration)
new  =
  do
    cls' <- getRequiredClass "VZConsolePortConfiguration"
    sendClassMsg cls' (mkSelector "new") (retPtr retVoid) [] >>= ownedObject . castPtr

-- | @- init@
init_ :: IsVZConsolePortConfiguration vzConsolePortConfiguration => vzConsolePortConfiguration -> IO (Id VZConsolePortConfiguration)
init_ vzConsolePortConfiguration  =
  sendMsg vzConsolePortConfiguration (mkSelector "init") (retPtr retVoid) [] >>= ownedObject . castPtr

-- | Console port attachment. Defines how the virtual machine's console port interfaces with the host system. Default is nil.
--
-- See: VZFileHandleSerialPortAttachment
--
-- See: VZFileSerialPortAttachment
--
-- See: VZSpiceAgentPortAttachment
--
-- ObjC selector: @- attachment@
attachment :: IsVZConsolePortConfiguration vzConsolePortConfiguration => vzConsolePortConfiguration -> IO (Id VZSerialPortAttachment)
attachment vzConsolePortConfiguration  =
  sendMsg vzConsolePortConfiguration (mkSelector "attachment") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | Console port attachment. Defines how the virtual machine's console port interfaces with the host system. Default is nil.
--
-- See: VZFileHandleSerialPortAttachment
--
-- See: VZFileSerialPortAttachment
--
-- See: VZSpiceAgentPortAttachment
--
-- ObjC selector: @- setAttachment:@
setAttachment :: (IsVZConsolePortConfiguration vzConsolePortConfiguration, IsVZSerialPortAttachment value) => vzConsolePortConfiguration -> value -> IO ()
setAttachment vzConsolePortConfiguration  value =
withObjCPtr value $ \raw_value ->
    sendMsg vzConsolePortConfiguration (mkSelector "setAttachment:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

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

