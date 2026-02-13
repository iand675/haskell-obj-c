{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Base class for a serial port attachment.
--
-- A serial port attachment defines how the virtual machine's serial port interfaces with the host system.    VZSerialPortAttachment should not be instantiated directly.    One of its subclasses like VZFileHandleSerialPortAttachment should be used instead.
--
-- See: VZFileHandleSerialPortAttachment
--
-- See: VZFileSerialPortAttachment
--
-- Generated bindings for @VZSerialPortAttachment@.
module ObjC.Virtualization.VZSerialPortAttachment
  ( VZSerialPortAttachment
  , IsVZSerialPortAttachment(..)
  , new
  , init_
  , initSelector
  , newSelector


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
new :: IO (Id VZSerialPortAttachment)
new  =
  do
    cls' <- getRequiredClass "VZSerialPortAttachment"
    sendOwnedClassMessage cls' newSelector

-- | @- init@
init_ :: IsVZSerialPortAttachment vzSerialPortAttachment => vzSerialPortAttachment -> IO (Id VZSerialPortAttachment)
init_ vzSerialPortAttachment =
  sendOwnedMessage vzSerialPortAttachment initSelector

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @new@
newSelector :: Selector '[] (Id VZSerialPortAttachment)
newSelector = mkSelector "new"

-- | @Selector@ for @init@
initSelector :: Selector '[] (Id VZSerialPortAttachment)
initSelector = mkSelector "init"

