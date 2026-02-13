{-# LANGUAGE DataKinds #-}
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
new :: IO (Id VZConsolePortConfiguration)
new  =
  do
    cls' <- getRequiredClass "VZConsolePortConfiguration"
    sendOwnedClassMessage cls' newSelector

-- | @- init@
init_ :: IsVZConsolePortConfiguration vzConsolePortConfiguration => vzConsolePortConfiguration -> IO (Id VZConsolePortConfiguration)
init_ vzConsolePortConfiguration =
  sendOwnedMessage vzConsolePortConfiguration initSelector

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
attachment vzConsolePortConfiguration =
  sendMessage vzConsolePortConfiguration attachmentSelector

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
setAttachment vzConsolePortConfiguration value =
  sendMessage vzConsolePortConfiguration setAttachmentSelector (toVZSerialPortAttachment value)

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @new@
newSelector :: Selector '[] (Id VZConsolePortConfiguration)
newSelector = mkSelector "new"

-- | @Selector@ for @init@
initSelector :: Selector '[] (Id VZConsolePortConfiguration)
initSelector = mkSelector "init"

-- | @Selector@ for @attachment@
attachmentSelector :: Selector '[] (Id VZSerialPortAttachment)
attachmentSelector = mkSelector "attachment"

-- | @Selector@ for @setAttachment:@
setAttachmentSelector :: Selector '[Id VZSerialPortAttachment] ()
setAttachmentSelector = mkSelector "setAttachment:"

