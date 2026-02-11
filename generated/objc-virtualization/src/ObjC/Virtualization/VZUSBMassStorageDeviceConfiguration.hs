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

-- | Initialize a VZUSBMassStorageDeviceConfiguration with a device attachment.
--
-- @attachment@ — The storage device attachment. This defines how the virtualized device operates on the host side.
--
-- See: VZDiskImageStorageDeviceAttachment
--
-- ObjC selector: @- initWithAttachment:@
initWithAttachment :: (IsVZUSBMassStorageDeviceConfiguration vzusbMassStorageDeviceConfiguration, IsVZStorageDeviceAttachment attachment) => vzusbMassStorageDeviceConfiguration -> attachment -> IO (Id VZUSBMassStorageDeviceConfiguration)
initWithAttachment vzusbMassStorageDeviceConfiguration  attachment =
withObjCPtr attachment $ \raw_attachment ->
    sendMsg vzusbMassStorageDeviceConfiguration (mkSelector "initWithAttachment:") (retPtr retVoid) [argPtr (castPtr raw_attachment :: Ptr ())] >>= ownedObject . castPtr

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @initWithAttachment:@
initWithAttachmentSelector :: Selector
initWithAttachmentSelector = mkSelector "initWithAttachment:"

