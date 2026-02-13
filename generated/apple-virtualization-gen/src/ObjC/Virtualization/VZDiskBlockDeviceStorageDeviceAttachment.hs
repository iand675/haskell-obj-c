{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Storage device attachment using a disk block device to store data.
--
-- The disk block device implements a storage attachment by using an actual disk rather than a disk image on a file system.
--
-- For example, a disk block device on the disk at @/dev/rdisk42@ would execute the I/O operations directly on that disk    rather than through a file system.
--
-- Note that if the disk has a file system formatted on it, the guest is able to destroy data in a way that is not recoverable.    The disk passed to this attachment needs to be handled with caution.
--
-- An example use of this API is:    ```    NSFileHandle *fileHandle = [NSFileHandle fileHandleForReadingAtPath:"/dev/rdisk42"];    if (!fileHandle) {        // Handle errors.    }
--
-- NSError *error;    VZDiskBlockDeviceStorageDeviceAttachment *attachment =        [[VZDiskBlockDeviceStorageDeviceAttachment alloc] initWithFileHandle:fileHandle                                                                     readOnly:YES                                                          synchronizationMode:VZDiskSynchronizationModeFull                                                                        error:error];    if (!attachment) {        // Handle errors.    }    ```
--
-- Disk file handles are typically only accessible by the @root@ user unless permission is explicitly granted.    Running virtual machines as root is not recommended. If @root@ access is required to open the file descriptor, it is recommended to do that operation    in a separate process then pass the file descriptor to a less privileged process running Virtualization framework.
--
-- See: VZDiskImageStorageDeviceAttachment
--
-- See: VZNVMExpressControllerDeviceConfiguration
--
-- See: VZUSBMassStorageDeviceConfiguration
--
-- See: VZVirtioBlockDeviceConfiguration
--
-- Generated bindings for @VZDiskBlockDeviceStorageDeviceAttachment@.
module ObjC.Virtualization.VZDiskBlockDeviceStorageDeviceAttachment
  ( VZDiskBlockDeviceStorageDeviceAttachment
  , IsVZDiskBlockDeviceStorageDeviceAttachment(..)
  , initWithFileHandle_readOnly_synchronizationMode_error
  , fileHandle
  , readOnly
  , synchronizationMode
  , fileHandleSelector
  , initWithFileHandle_readOnly_synchronizationMode_errorSelector
  , readOnlySelector
  , synchronizationModeSelector

  -- * Enum types
  , VZDiskSynchronizationMode(VZDiskSynchronizationMode)
  , pattern VZDiskSynchronizationModeFull
  , pattern VZDiskSynchronizationModeNone

  ) where

import Foreign.Ptr (Ptr, FunPtr)
import Foreign.C.Types

import ObjC.Runtime.Types
import ObjC.Runtime.Message (sendMessage, sendOwnedMessage, sendClassMessage, sendOwnedClassMessage)
import ObjC.Runtime.Selector (mkSelector)
import ObjC.Runtime.Class (getRequiredClass)

import ObjC.Virtualization.Internal.Classes
import ObjC.Virtualization.Internal.Enums
import ObjC.Foundation.Internal.Classes

-- | Initialize the disk attachment from a file handle.
--
-- @fileHandle@ — File handle to a block device.
--
-- @readOnly@ — If YES, the disk attachment is read only, otherwise, if the file handle allows writes, the device can write data into it.
--
-- @synchronizationMode@ — Defines how the disk synchronizes with the underlying storage when the guest operating system flushes data.
--
-- @error@ — If not nil, assigned with the error if the initialization failed.
--
-- Returns: An initialized @VZDiskBlockDeviceStorageDeviceAttachment@ or nil if there was an error.
--
-- The file handle is retained by the disk attachment.    The handle must be open when the virtual machine starts.
--
-- The @readOnly@ parameter affects how the disk is exposed to the guest operating system    by the storage controller. If the disk is intended to be used read-only, it is also recommended    to open the file handle as read-only.
--
-- ObjC selector: @- initWithFileHandle:readOnly:synchronizationMode:error:@
initWithFileHandle_readOnly_synchronizationMode_error :: (IsVZDiskBlockDeviceStorageDeviceAttachment vzDiskBlockDeviceStorageDeviceAttachment, IsNSFileHandle fileHandle, IsNSError error_) => vzDiskBlockDeviceStorageDeviceAttachment -> fileHandle -> Bool -> VZDiskSynchronizationMode -> error_ -> IO (Id VZDiskBlockDeviceStorageDeviceAttachment)
initWithFileHandle_readOnly_synchronizationMode_error vzDiskBlockDeviceStorageDeviceAttachment fileHandle readOnly synchronizationMode error_ =
  sendOwnedMessage vzDiskBlockDeviceStorageDeviceAttachment initWithFileHandle_readOnly_synchronizationMode_errorSelector (toNSFileHandle fileHandle) readOnly synchronizationMode (toNSError error_)

-- | File handle to the underlying disk used for storage by the attachment.
--
-- ObjC selector: @- fileHandle@
fileHandle :: IsVZDiskBlockDeviceStorageDeviceAttachment vzDiskBlockDeviceStorageDeviceAttachment => vzDiskBlockDeviceStorageDeviceAttachment -> IO (Id NSFileHandle)
fileHandle vzDiskBlockDeviceStorageDeviceAttachment =
  sendMessage vzDiskBlockDeviceStorageDeviceAttachment fileHandleSelector

-- | Whether the underlying disk attachment is read-only.
--
-- ObjC selector: @- readOnly@
readOnly :: IsVZDiskBlockDeviceStorageDeviceAttachment vzDiskBlockDeviceStorageDeviceAttachment => vzDiskBlockDeviceStorageDeviceAttachment -> IO Bool
readOnly vzDiskBlockDeviceStorageDeviceAttachment =
  sendMessage vzDiskBlockDeviceStorageDeviceAttachment readOnlySelector

-- | The mode in which the disk image synchronizes data with the underlying storage device.
--
-- ObjC selector: @- synchronizationMode@
synchronizationMode :: IsVZDiskBlockDeviceStorageDeviceAttachment vzDiskBlockDeviceStorageDeviceAttachment => vzDiskBlockDeviceStorageDeviceAttachment -> IO VZDiskSynchronizationMode
synchronizationMode vzDiskBlockDeviceStorageDeviceAttachment =
  sendMessage vzDiskBlockDeviceStorageDeviceAttachment synchronizationModeSelector

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @initWithFileHandle:readOnly:synchronizationMode:error:@
initWithFileHandle_readOnly_synchronizationMode_errorSelector :: Selector '[Id NSFileHandle, Bool, VZDiskSynchronizationMode, Id NSError] (Id VZDiskBlockDeviceStorageDeviceAttachment)
initWithFileHandle_readOnly_synchronizationMode_errorSelector = mkSelector "initWithFileHandle:readOnly:synchronizationMode:error:"

-- | @Selector@ for @fileHandle@
fileHandleSelector :: Selector '[] (Id NSFileHandle)
fileHandleSelector = mkSelector "fileHandle"

-- | @Selector@ for @readOnly@
readOnlySelector :: Selector '[] Bool
readOnlySelector = mkSelector "readOnly"

-- | @Selector@ for @synchronizationMode@
synchronizationModeSelector :: Selector '[] VZDiskSynchronizationMode
synchronizationModeSelector = mkSelector "synchronizationMode"

