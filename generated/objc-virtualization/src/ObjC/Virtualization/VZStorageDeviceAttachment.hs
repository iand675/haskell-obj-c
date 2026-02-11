{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Base class for a storage device attachment.
--
-- A storage device attachment defines how a virtual machine storage device interfaces with the host system.
--
-- VZStorageDeviceAttachment should not be instantiated directly.    One of its subclasses like VZDiskImageStorageDeviceAttachment should be used instead.
--
-- See: VZDiskImageStorageDeviceAttachment
--
-- Generated bindings for @VZStorageDeviceAttachment@.
module ObjC.Virtualization.VZStorageDeviceAttachment
  ( VZStorageDeviceAttachment
  , IsVZStorageDeviceAttachment(..)
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
new :: IO (Id VZStorageDeviceAttachment)
new  =
  do
    cls' <- getRequiredClass "VZStorageDeviceAttachment"
    sendClassMsg cls' (mkSelector "new") (retPtr retVoid) [] >>= ownedObject . castPtr

-- | @- init@
init_ :: IsVZStorageDeviceAttachment vzStorageDeviceAttachment => vzStorageDeviceAttachment -> IO (Id VZStorageDeviceAttachment)
init_ vzStorageDeviceAttachment  =
  sendMsg vzStorageDeviceAttachment (mkSelector "init") (retPtr retVoid) [] >>= ownedObject . castPtr

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @new@
newSelector :: Selector
newSelector = mkSelector "new"

-- | @Selector@ for @init@
initSelector :: Selector
initSelector = mkSelector "init"

