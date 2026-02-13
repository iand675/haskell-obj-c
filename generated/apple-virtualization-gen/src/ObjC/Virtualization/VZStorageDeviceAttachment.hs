{-# LANGUAGE DataKinds #-}
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
new :: IO (Id VZStorageDeviceAttachment)
new  =
  do
    cls' <- getRequiredClass "VZStorageDeviceAttachment"
    sendOwnedClassMessage cls' newSelector

-- | @- init@
init_ :: IsVZStorageDeviceAttachment vzStorageDeviceAttachment => vzStorageDeviceAttachment -> IO (Id VZStorageDeviceAttachment)
init_ vzStorageDeviceAttachment =
  sendOwnedMessage vzStorageDeviceAttachment initSelector

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @new@
newSelector :: Selector '[] (Id VZStorageDeviceAttachment)
newSelector = mkSelector "new"

-- | @Selector@ for @init@
initSelector :: Selector '[] (Id VZStorageDeviceAttachment)
initSelector = mkSelector "init"

