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

-- | Initialize a VZNVMExpressControllerDeviceConfiguration with a device attachment.
--
-- @attachment@ — The storage device attachment. This defines how the virtualized device operates on the host side.
--
-- See: VZDiskImageStorageDeviceAttachment
--
-- ObjC selector: @- initWithAttachment:@
initWithAttachment :: (IsVZNVMExpressControllerDeviceConfiguration vznvmExpressControllerDeviceConfiguration, IsVZStorageDeviceAttachment attachment) => vznvmExpressControllerDeviceConfiguration -> attachment -> IO (Id VZNVMExpressControllerDeviceConfiguration)
initWithAttachment vznvmExpressControllerDeviceConfiguration  attachment =
withObjCPtr attachment $ \raw_attachment ->
    sendMsg vznvmExpressControllerDeviceConfiguration (mkSelector "initWithAttachment:") (retPtr retVoid) [argPtr (castPtr raw_attachment :: Ptr ())] >>= ownedObject . castPtr

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @initWithAttachment:@
initWithAttachmentSelector :: Selector
initWithAttachmentSelector = mkSelector "initWithAttachment:"

