{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Storage device attachment using a disk image to implement the storage.
--
-- This storage device attachment uses a disk image on the host file system as the drive of the storage device.
--
-- Only RAW data disk images are supported.
--
-- An empty RAW disk image can be created with @FileDescriptor.resize(to:retryOnInterrupt:)@ method in Swift,    the @ftruncate()@ function in Swift or Objective-C, or the @truncate@ command on the command line.    The size of the file must be a multiple of 512 bytes, the block size.
--
-- See: VZNVMExpressControllerDeviceConfiguration
--
-- See: VZUSBMassStorageDeviceConfiguration
--
-- See: VZVirtioBlockDeviceConfiguration
--
-- Generated bindings for @VZDiskImageStorageDeviceAttachment@.
module ObjC.Virtualization.VZDiskImageStorageDeviceAttachment
  ( VZDiskImageStorageDeviceAttachment
  , IsVZDiskImageStorageDeviceAttachment(..)
  , initWithURL_readOnly_error
  , initWithURL_readOnly_cachingMode_synchronizationMode_error
  , url
  , readOnly
  , cachingMode
  , synchronizationMode
  , cachingModeSelector
  , initWithURL_readOnly_cachingMode_synchronizationMode_errorSelector
  , initWithURL_readOnly_errorSelector
  , readOnlySelector
  , synchronizationModeSelector
  , urlSelector

  -- * Enum types
  , VZDiskImageCachingMode(VZDiskImageCachingMode)
  , pattern VZDiskImageCachingModeAutomatic
  , pattern VZDiskImageCachingModeUncached
  , pattern VZDiskImageCachingModeCached
  , VZDiskImageSynchronizationMode(VZDiskImageSynchronizationMode)
  , pattern VZDiskImageSynchronizationModeFull
  , pattern VZDiskImageSynchronizationModeFsync
  , pattern VZDiskImageSynchronizationModeNone

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

-- | Initialize the attachment from a local file url.
--
-- @url@ — Local file URL to the disk image in RAW format.
--
-- @readOnly@ — If YES, the device attachment is read-only, otherwise the device can write data to the disk image.
--
-- @error@ — If not nil, assigned with the error if the initialization failed.
--
-- Returns: A newly initialized VZDiskImageStorageDeviceAttachment. If an error was encountered returns @nil,@ and @error@ contains the error.
--
-- ObjC selector: @- initWithURL:readOnly:error:@
initWithURL_readOnly_error :: (IsVZDiskImageStorageDeviceAttachment vzDiskImageStorageDeviceAttachment, IsNSURL url, IsNSError error_) => vzDiskImageStorageDeviceAttachment -> url -> Bool -> error_ -> IO (Id VZDiskImageStorageDeviceAttachment)
initWithURL_readOnly_error vzDiskImageStorageDeviceAttachment url readOnly error_ =
  sendOwnedMessage vzDiskImageStorageDeviceAttachment initWithURL_readOnly_errorSelector (toNSURL url) readOnly (toNSError error_)

-- | Initialize the attachment from a local file url.
--
-- @url@ — Local file URL to the disk image in RAW format.
--
-- @readOnly@ — If YES, the device attachment is read-only, otherwise the device can write data to the disk image.
--
-- @cachingMode@ — Whether host data caching is enabled for the disk image.
--
-- @synchronizationMode@ — How the disk image synchronizes with the underlying storage when the guest operating system flushes data.
--
-- @error@ — If not nil, assigned with the error if the initialization failed.
--
-- Returns: A newly initialized VZDiskImageStorageDeviceAttachment. If an error was encountered returns @nil,@ and @error@ contains the error.
--
-- ObjC selector: @- initWithURL:readOnly:cachingMode:synchronizationMode:error:@
initWithURL_readOnly_cachingMode_synchronizationMode_error :: (IsVZDiskImageStorageDeviceAttachment vzDiskImageStorageDeviceAttachment, IsNSURL url, IsNSError error_) => vzDiskImageStorageDeviceAttachment -> url -> Bool -> VZDiskImageCachingMode -> VZDiskImageSynchronizationMode -> error_ -> IO (Id VZDiskImageStorageDeviceAttachment)
initWithURL_readOnly_cachingMode_synchronizationMode_error vzDiskImageStorageDeviceAttachment url readOnly cachingMode synchronizationMode error_ =
  sendOwnedMessage vzDiskImageStorageDeviceAttachment initWithURL_readOnly_cachingMode_synchronizationMode_errorSelector (toNSURL url) readOnly cachingMode synchronizationMode (toNSError error_)

-- | URL of the underlying disk image.
--
-- ObjC selector: @- URL@
url :: IsVZDiskImageStorageDeviceAttachment vzDiskImageStorageDeviceAttachment => vzDiskImageStorageDeviceAttachment -> IO (Id NSURL)
url vzDiskImageStorageDeviceAttachment =
  sendMessage vzDiskImageStorageDeviceAttachment urlSelector

-- | Whether the underlying disk image is read-only.
--
-- ObjC selector: @- readOnly@
readOnly :: IsVZDiskImageStorageDeviceAttachment vzDiskImageStorageDeviceAttachment => vzDiskImageStorageDeviceAttachment -> IO Bool
readOnly vzDiskImageStorageDeviceAttachment =
  sendMessage vzDiskImageStorageDeviceAttachment readOnlySelector

-- | How disk image data is cached by the host.
--
-- ObjC selector: @- cachingMode@
cachingMode :: IsVZDiskImageStorageDeviceAttachment vzDiskImageStorageDeviceAttachment => vzDiskImageStorageDeviceAttachment -> IO VZDiskImageCachingMode
cachingMode vzDiskImageStorageDeviceAttachment =
  sendMessage vzDiskImageStorageDeviceAttachment cachingModeSelector

-- | The mode in which the disk image synchronizes data with the underlying storage device.
--
-- ObjC selector: @- synchronizationMode@
synchronizationMode :: IsVZDiskImageStorageDeviceAttachment vzDiskImageStorageDeviceAttachment => vzDiskImageStorageDeviceAttachment -> IO VZDiskImageSynchronizationMode
synchronizationMode vzDiskImageStorageDeviceAttachment =
  sendMessage vzDiskImageStorageDeviceAttachment synchronizationModeSelector

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @initWithURL:readOnly:error:@
initWithURL_readOnly_errorSelector :: Selector '[Id NSURL, Bool, Id NSError] (Id VZDiskImageStorageDeviceAttachment)
initWithURL_readOnly_errorSelector = mkSelector "initWithURL:readOnly:error:"

-- | @Selector@ for @initWithURL:readOnly:cachingMode:synchronizationMode:error:@
initWithURL_readOnly_cachingMode_synchronizationMode_errorSelector :: Selector '[Id NSURL, Bool, VZDiskImageCachingMode, VZDiskImageSynchronizationMode, Id NSError] (Id VZDiskImageStorageDeviceAttachment)
initWithURL_readOnly_cachingMode_synchronizationMode_errorSelector = mkSelector "initWithURL:readOnly:cachingMode:synchronizationMode:error:"

-- | @Selector@ for @URL@
urlSelector :: Selector '[] (Id NSURL)
urlSelector = mkSelector "URL"

-- | @Selector@ for @readOnly@
readOnlySelector :: Selector '[] Bool
readOnlySelector = mkSelector "readOnly"

-- | @Selector@ for @cachingMode@
cachingModeSelector :: Selector '[] VZDiskImageCachingMode
cachingModeSelector = mkSelector "cachingMode"

-- | @Selector@ for @synchronizationMode@
synchronizationModeSelector :: Selector '[] VZDiskImageSynchronizationMode
synchronizationModeSelector = mkSelector "synchronizationMode"

