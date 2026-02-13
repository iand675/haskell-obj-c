{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Configuration of an NVM Express Controller storage device.
--
-- This device configuration creates a storage device that conforms to the NVM Express specification revision 1.1b.
--
-- The device configuration is valid only if used with VZGenericPlatformConfiguration.
--
-- See: VZGenericPlatformConfiguration
--
-- Generated bindings for @VZNVMExpressControllerDeviceConfiguration@.
module ObjC.Virtualization.VZNVMExpressControllerDeviceConfiguration
  ( VZNVMExpressControllerDeviceConfiguration
  , IsVZNVMExpressControllerDeviceConfiguration(..)
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

-- | Initialize a VZNVMExpressControllerDeviceConfiguration with a device attachment.
--
-- @attachment@ — The storage device attachment. This defines how the virtualized device operates on the host side.
--
-- See: VZDiskImageStorageDeviceAttachment
--
-- ObjC selector: @- initWithAttachment:@
initWithAttachment :: (IsVZNVMExpressControllerDeviceConfiguration vznvmExpressControllerDeviceConfiguration, IsVZStorageDeviceAttachment attachment) => vznvmExpressControllerDeviceConfiguration -> attachment -> IO (Id VZNVMExpressControllerDeviceConfiguration)
initWithAttachment vznvmExpressControllerDeviceConfiguration attachment =
  sendOwnedMessage vznvmExpressControllerDeviceConfiguration initWithAttachmentSelector (toVZStorageDeviceAttachment attachment)

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @initWithAttachment:@
initWithAttachmentSelector :: Selector '[Id VZStorageDeviceAttachment] (Id VZNVMExpressControllerDeviceConfiguration)
initWithAttachmentSelector = mkSelector "initWithAttachment:"

