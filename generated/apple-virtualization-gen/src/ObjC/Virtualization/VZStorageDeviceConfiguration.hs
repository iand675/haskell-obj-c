{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Base class for a storage device configuration.
--
-- VZStorageDeviceConfiguration should not be instantiated directly.    One of its subclasses like VZVirtioBlockDeviceConfiguration should be used instead.
--
-- See: VZNVMExpressControllerDeviceConfiguration
--
-- See: VZUSBMassStorageDeviceConfiguration
--
-- See: VZVirtioBlockDeviceConfiguration
--
-- Generated bindings for @VZStorageDeviceConfiguration@.
module ObjC.Virtualization.VZStorageDeviceConfiguration
  ( VZStorageDeviceConfiguration
  , IsVZStorageDeviceConfiguration(..)
  , new
  , init_
  , attachment
  , attachmentSelector
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
new :: IO (Id VZStorageDeviceConfiguration)
new  =
  do
    cls' <- getRequiredClass "VZStorageDeviceConfiguration"
    sendOwnedClassMessage cls' newSelector

-- | @- init@
init_ :: IsVZStorageDeviceConfiguration vzStorageDeviceConfiguration => vzStorageDeviceConfiguration -> IO (Id VZStorageDeviceConfiguration)
init_ vzStorageDeviceConfiguration =
  sendOwnedMessage vzStorageDeviceConfiguration initSelector

-- | Storage device attachment. Defines what local resource is exposed to the virtual machine as a disk.
--
-- See: VZDiskImageStorageDeviceAttachment
--
-- ObjC selector: @- attachment@
attachment :: IsVZStorageDeviceConfiguration vzStorageDeviceConfiguration => vzStorageDeviceConfiguration -> IO (Id VZStorageDeviceAttachment)
attachment vzStorageDeviceConfiguration =
  sendMessage vzStorageDeviceConfiguration attachmentSelector

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @new@
newSelector :: Selector '[] (Id VZStorageDeviceConfiguration)
newSelector = mkSelector "new"

-- | @Selector@ for @init@
initSelector :: Selector '[] (Id VZStorageDeviceConfiguration)
initSelector = mkSelector "init"

-- | @Selector@ for @attachment@
attachmentSelector :: Selector '[] (Id VZStorageDeviceAttachment)
attachmentSelector = mkSelector "attachment"

