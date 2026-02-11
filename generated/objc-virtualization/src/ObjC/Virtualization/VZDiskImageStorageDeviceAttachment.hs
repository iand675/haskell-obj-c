{-# LANGUAGE PatternSynonyms #-}
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
  , initWithURL_readOnly_errorSelector
  , initWithURL_readOnly_cachingMode_synchronizationMode_errorSelector
  , urlSelector
  , readOnlySelector
  , cachingModeSelector
  , synchronizationModeSelector

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
initWithURL_readOnly_error vzDiskImageStorageDeviceAttachment  url readOnly error_ =
withObjCPtr url $ \raw_url ->
  withObjCPtr error_ $ \raw_error_ ->
      sendMsg vzDiskImageStorageDeviceAttachment (mkSelector "initWithURL:readOnly:error:") (retPtr retVoid) [argPtr (castPtr raw_url :: Ptr ()), argCULong (if readOnly then 1 else 0), argPtr (castPtr raw_error_ :: Ptr ())] >>= ownedObject . castPtr

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
initWithURL_readOnly_cachingMode_synchronizationMode_error vzDiskImageStorageDeviceAttachment  url readOnly cachingMode synchronizationMode error_ =
withObjCPtr url $ \raw_url ->
  withObjCPtr error_ $ \raw_error_ ->
      sendMsg vzDiskImageStorageDeviceAttachment (mkSelector "initWithURL:readOnly:cachingMode:synchronizationMode:error:") (retPtr retVoid) [argPtr (castPtr raw_url :: Ptr ()), argCULong (if readOnly then 1 else 0), argCLong (coerce cachingMode), argCLong (coerce synchronizationMode), argPtr (castPtr raw_error_ :: Ptr ())] >>= ownedObject . castPtr

-- | URL of the underlying disk image.
--
-- ObjC selector: @- URL@
url :: IsVZDiskImageStorageDeviceAttachment vzDiskImageStorageDeviceAttachment => vzDiskImageStorageDeviceAttachment -> IO (Id NSURL)
url vzDiskImageStorageDeviceAttachment  =
  sendMsg vzDiskImageStorageDeviceAttachment (mkSelector "URL") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | Whether the underlying disk image is read-only.
--
-- ObjC selector: @- readOnly@
readOnly :: IsVZDiskImageStorageDeviceAttachment vzDiskImageStorageDeviceAttachment => vzDiskImageStorageDeviceAttachment -> IO Bool
readOnly vzDiskImageStorageDeviceAttachment  =
  fmap ((/= 0) :: CULong -> Bool) $ sendMsg vzDiskImageStorageDeviceAttachment (mkSelector "readOnly") retCULong []

-- | How disk image data is cached by the host.
--
-- ObjC selector: @- cachingMode@
cachingMode :: IsVZDiskImageStorageDeviceAttachment vzDiskImageStorageDeviceAttachment => vzDiskImageStorageDeviceAttachment -> IO VZDiskImageCachingMode
cachingMode vzDiskImageStorageDeviceAttachment  =
  fmap (coerce :: CLong -> VZDiskImageCachingMode) $ sendMsg vzDiskImageStorageDeviceAttachment (mkSelector "cachingMode") retCLong []

-- | The mode in which the disk image synchronizes data with the underlying storage device.
--
-- ObjC selector: @- synchronizationMode@
synchronizationMode :: IsVZDiskImageStorageDeviceAttachment vzDiskImageStorageDeviceAttachment => vzDiskImageStorageDeviceAttachment -> IO VZDiskImageSynchronizationMode
synchronizationMode vzDiskImageStorageDeviceAttachment  =
  fmap (coerce :: CLong -> VZDiskImageSynchronizationMode) $ sendMsg vzDiskImageStorageDeviceAttachment (mkSelector "synchronizationMode") retCLong []

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @initWithURL:readOnly:error:@
initWithURL_readOnly_errorSelector :: Selector
initWithURL_readOnly_errorSelector = mkSelector "initWithURL:readOnly:error:"

-- | @Selector@ for @initWithURL:readOnly:cachingMode:synchronizationMode:error:@
initWithURL_readOnly_cachingMode_synchronizationMode_errorSelector :: Selector
initWithURL_readOnly_cachingMode_synchronizationMode_errorSelector = mkSelector "initWithURL:readOnly:cachingMode:synchronizationMode:error:"

-- | @Selector@ for @URL@
urlSelector :: Selector
urlSelector = mkSelector "URL"

-- | @Selector@ for @readOnly@
readOnlySelector :: Selector
readOnlySelector = mkSelector "readOnly"

-- | @Selector@ for @cachingMode@
cachingModeSelector :: Selector
cachingModeSelector = mkSelector "cachingMode"

-- | @Selector@ for @synchronizationMode@
synchronizationModeSelector :: Selector
synchronizationModeSelector = mkSelector "synchronizationMode"

