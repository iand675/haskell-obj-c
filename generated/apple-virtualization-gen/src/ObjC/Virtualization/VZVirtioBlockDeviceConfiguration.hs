{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Configuration of a paravirtualized storage device of type Virtio Block Device.
--
-- This device configuration creates a storage device using paravirtualization.    The emulated device follows the Virtio Block Device specification.
--
-- The host implementation of the device is done through an attachment subclassing VZStorageDeviceAttachment    like VZDiskImageStorageDeviceAttachment.
--
-- Generated bindings for @VZVirtioBlockDeviceConfiguration@.
module ObjC.Virtualization.VZVirtioBlockDeviceConfiguration
  ( VZVirtioBlockDeviceConfiguration
  , IsVZVirtioBlockDeviceConfiguration(..)
  , initWithAttachment
  , validateBlockDeviceIdentifier_error
  , blockDeviceIdentifier
  , setBlockDeviceIdentifier
  , blockDeviceIdentifierSelector
  , initWithAttachmentSelector
  , setBlockDeviceIdentifierSelector
  , validateBlockDeviceIdentifier_errorSelector


  ) where

import Foreign.Ptr (Ptr, FunPtr)
import Foreign.C.Types

import ObjC.Runtime.Types
import ObjC.Runtime.Message (sendMessage, sendOwnedMessage, sendClassMessage, sendOwnedClassMessage)
import ObjC.Runtime.Selector (mkSelector)
import ObjC.Runtime.Class (getRequiredClass)

import ObjC.Virtualization.Internal.Classes
import ObjC.Foundation.Internal.Classes

-- | Initialize a VZVirtioBlockDeviceConfiguration with a device attachment.
--
-- @attachment@ — The storage device attachment. This defines how the virtualized device operates on the host side.
--
-- See: VZDiskImageStorageDeviceAttachment
--
-- ObjC selector: @- initWithAttachment:@
initWithAttachment :: (IsVZVirtioBlockDeviceConfiguration vzVirtioBlockDeviceConfiguration, IsVZStorageDeviceAttachment attachment) => vzVirtioBlockDeviceConfiguration -> attachment -> IO (Id VZVirtioBlockDeviceConfiguration)
initWithAttachment vzVirtioBlockDeviceConfiguration attachment =
  sendOwnedMessage vzVirtioBlockDeviceConfiguration initWithAttachmentSelector (toVZStorageDeviceAttachment attachment)

-- | Check if blockDeviceIdentifier is a valid Virtio block device identifier.
--
-- @blockDeviceIdentifier@ — The device identifier to validate.
--
-- @error@ — If not nil, assigned with an error describing why the device identifier is not valid.
--
-- The device identifier must be at most 20 bytes in length and ASCII-encodable.
--
-- ObjC selector: @+ validateBlockDeviceIdentifier:error:@
validateBlockDeviceIdentifier_error :: (IsNSString blockDeviceIdentifier, IsNSError error_) => blockDeviceIdentifier -> error_ -> IO Bool
validateBlockDeviceIdentifier_error blockDeviceIdentifier error_ =
  do
    cls' <- getRequiredClass "VZVirtioBlockDeviceConfiguration"
    sendClassMessage cls' validateBlockDeviceIdentifier_errorSelector (toNSString blockDeviceIdentifier) (toNSError error_)

-- | The device identifier is a string identifying the Virtio block device. Empty string by default.
--
-- The identifier can be retrieved in the guest via a VIRTIO_BLK_T_GET_ID request.
--
-- The identifier must be encodable as an ASCII string of length at most 20 bytes.    This property can be checked with +[VZVirtioBlockDeviceConfiguration validateBlockDeviceIdentifier:error:].
--
-- See: +[VZVirtioBlockDeviceConfiguration validateBlockDeviceIdentifier:error:]
--
-- ObjC selector: @- blockDeviceIdentifier@
blockDeviceIdentifier :: IsVZVirtioBlockDeviceConfiguration vzVirtioBlockDeviceConfiguration => vzVirtioBlockDeviceConfiguration -> IO (Id NSString)
blockDeviceIdentifier vzVirtioBlockDeviceConfiguration =
  sendMessage vzVirtioBlockDeviceConfiguration blockDeviceIdentifierSelector

-- | The device identifier is a string identifying the Virtio block device. Empty string by default.
--
-- The identifier can be retrieved in the guest via a VIRTIO_BLK_T_GET_ID request.
--
-- The identifier must be encodable as an ASCII string of length at most 20 bytes.    This property can be checked with +[VZVirtioBlockDeviceConfiguration validateBlockDeviceIdentifier:error:].
--
-- See: +[VZVirtioBlockDeviceConfiguration validateBlockDeviceIdentifier:error:]
--
-- ObjC selector: @- setBlockDeviceIdentifier:@
setBlockDeviceIdentifier :: (IsVZVirtioBlockDeviceConfiguration vzVirtioBlockDeviceConfiguration, IsNSString value) => vzVirtioBlockDeviceConfiguration -> value -> IO ()
setBlockDeviceIdentifier vzVirtioBlockDeviceConfiguration value =
  sendMessage vzVirtioBlockDeviceConfiguration setBlockDeviceIdentifierSelector (toNSString value)

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @initWithAttachment:@
initWithAttachmentSelector :: Selector '[Id VZStorageDeviceAttachment] (Id VZVirtioBlockDeviceConfiguration)
initWithAttachmentSelector = mkSelector "initWithAttachment:"

-- | @Selector@ for @validateBlockDeviceIdentifier:error:@
validateBlockDeviceIdentifier_errorSelector :: Selector '[Id NSString, Id NSError] Bool
validateBlockDeviceIdentifier_errorSelector = mkSelector "validateBlockDeviceIdentifier:error:"

-- | @Selector@ for @blockDeviceIdentifier@
blockDeviceIdentifierSelector :: Selector '[] (Id NSString)
blockDeviceIdentifierSelector = mkSelector "blockDeviceIdentifier"

-- | @Selector@ for @setBlockDeviceIdentifier:@
setBlockDeviceIdentifierSelector :: Selector '[Id NSString] ()
setBlockDeviceIdentifierSelector = mkSelector "setBlockDeviceIdentifier:"

