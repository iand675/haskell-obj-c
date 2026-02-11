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
  , initWithAttachmentSelector
  , validateBlockDeviceIdentifier_errorSelector
  , blockDeviceIdentifierSelector
  , setBlockDeviceIdentifierSelector


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

-- | Initialize a VZVirtioBlockDeviceConfiguration with a device attachment.
--
-- @attachment@ — The storage device attachment. This defines how the virtualized device operates on the host side.
--
-- See: VZDiskImageStorageDeviceAttachment
--
-- ObjC selector: @- initWithAttachment:@
initWithAttachment :: (IsVZVirtioBlockDeviceConfiguration vzVirtioBlockDeviceConfiguration, IsVZStorageDeviceAttachment attachment) => vzVirtioBlockDeviceConfiguration -> attachment -> IO (Id VZVirtioBlockDeviceConfiguration)
initWithAttachment vzVirtioBlockDeviceConfiguration  attachment =
  withObjCPtr attachment $ \raw_attachment ->
      sendMsg vzVirtioBlockDeviceConfiguration (mkSelector "initWithAttachment:") (retPtr retVoid) [argPtr (castPtr raw_attachment :: Ptr ())] >>= ownedObject . castPtr

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
    withObjCPtr blockDeviceIdentifier $ \raw_blockDeviceIdentifier ->
      withObjCPtr error_ $ \raw_error_ ->
        fmap ((/= 0) :: CULong -> Bool) $ sendClassMsg cls' (mkSelector "validateBlockDeviceIdentifier:error:") retCULong [argPtr (castPtr raw_blockDeviceIdentifier :: Ptr ()), argPtr (castPtr raw_error_ :: Ptr ())]

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
blockDeviceIdentifier vzVirtioBlockDeviceConfiguration  =
    sendMsg vzVirtioBlockDeviceConfiguration (mkSelector "blockDeviceIdentifier") (retPtr retVoid) [] >>= retainedObject . castPtr

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
setBlockDeviceIdentifier vzVirtioBlockDeviceConfiguration  value =
  withObjCPtr value $ \raw_value ->
      sendMsg vzVirtioBlockDeviceConfiguration (mkSelector "setBlockDeviceIdentifier:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @initWithAttachment:@
initWithAttachmentSelector :: Selector
initWithAttachmentSelector = mkSelector "initWithAttachment:"

-- | @Selector@ for @validateBlockDeviceIdentifier:error:@
validateBlockDeviceIdentifier_errorSelector :: Selector
validateBlockDeviceIdentifier_errorSelector = mkSelector "validateBlockDeviceIdentifier:error:"

-- | @Selector@ for @blockDeviceIdentifier@
blockDeviceIdentifierSelector :: Selector
blockDeviceIdentifierSelector = mkSelector "blockDeviceIdentifier"

-- | @Selector@ for @setBlockDeviceIdentifier:@
setBlockDeviceIdentifierSelector :: Selector
setBlockDeviceIdentifierSelector = mkSelector "setBlockDeviceIdentifier:"

