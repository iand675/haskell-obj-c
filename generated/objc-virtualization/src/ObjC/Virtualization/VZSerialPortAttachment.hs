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
  , newSelector
  , initSelector


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
new :: IO (Id VZSerialPortAttachment)
new  =
  do
    cls' <- getRequiredClass "VZSerialPortAttachment"
    sendClassMsg cls' (mkSelector "new") (retPtr retVoid) [] >>= ownedObject . castPtr

-- | @- init@
init_ :: IsVZSerialPortAttachment vzSerialPortAttachment => vzSerialPortAttachment -> IO (Id VZSerialPortAttachment)
init_ vzSerialPortAttachment  =
  sendMsg vzSerialPortAttachment (mkSelector "init") (retPtr retVoid) [] >>= ownedObject . castPtr

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @new@
newSelector :: Selector
newSelector = mkSelector "new"

-- | @Selector@ for @init@
initSelector :: Selector
initSelector = mkSelector "init"

