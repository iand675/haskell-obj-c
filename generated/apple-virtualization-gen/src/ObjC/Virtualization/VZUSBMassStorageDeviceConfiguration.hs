{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Configuration of a USB Mass Storage storage device.
--
-- This device configuration creates a storage device that conforms to the USB Mass Storage specification.
--
-- Generated bindings for @VZUSBMassStorageDeviceConfiguration@.
module ObjC.Virtualization.VZUSBMassStorageDeviceConfiguration
  ( VZUSBMassStorageDeviceConfiguration
  , IsVZUSBMassStorageDeviceConfiguration(..)
  , initWithAttachment
  , initWithAttachmentSelector


  ) where

import Foreign.Ptr (Ptr, FunPtr)
import Foreign.C.Types

import ObjC.Runtime.Types
import ObjC.Runtime.Message (sendMessage, sendOwnedMessage, sendClassMessage, sendOwnedClassMessage)
import ObjC.Runtime.Selector (mkSelector)
import ObjC.Runtime.Class (getRequiredClass)

import ObjC.Virtualization.Internal.Classes
import ObjC.Foundation.Internal.Classes

-- | Initialize a VZUSBMassStorageDeviceConfiguration with a device attachment.
--
-- @attachment@ — The storage device attachment. This defines how the virtualized device operates on the host side.
--
-- See: VZDiskImageStorageDeviceAttachment
--
-- ObjC selector: @- initWithAttachment:@
initWithAttachment :: (IsVZUSBMassStorageDeviceConfiguration vzusbMassStorageDeviceConfiguration, IsVZStorageDeviceAttachment attachment) => vzusbMassStorageDeviceConfiguration -> attachment -> IO (Id VZUSBMassStorageDeviceConfiguration)
initWithAttachment vzusbMassStorageDeviceConfiguration attachment =
  sendOwnedMessage vzusbMassStorageDeviceConfiguration initWithAttachmentSelector (toVZStorageDeviceAttachment attachment)

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @initWithAttachment:@
initWithAttachmentSelector :: Selector '[Id VZStorageDeviceAttachment] (Id VZUSBMassStorageDeviceConfiguration)
initWithAttachmentSelector = mkSelector "initWithAttachment:"

