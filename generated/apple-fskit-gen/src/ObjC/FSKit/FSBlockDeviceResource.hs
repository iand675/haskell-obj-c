{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | A resource that represents a block storage disk partition.
--
-- A @FSBlockDeviceResource@ can exist in either a proxied or nonproxied version.  Only the @fskitd@ daemon creates "real" (nonproxied) instances of this class.  Client applications and daemons create proxy objects for requests, and @fskitd@ opens the underlying device during the processing of the request.
--
-- This class wraps a file descriptor for a disk device or partition.  Its fundamental identifier is the BSD disk name (``bsdName``) for the underlying IOMedia object.  However, ``FSBlockDeviceResource-c.class`` doesn't expose the underlying file descriptor.  Instead, it provides accessor methods that can read from and write to the partition, either directly or using the kernel buffer cache.
--
-- When you use a @FSBlockDeviceResource@, your file system implementation also conforms to a maintenance operation protocol.  These protocols add support for checking, repairing, and optionally formatting file systems.  The system doesn't mount block device file systems until they pass a file system check.  For an ``FSUnaryFileSystem`` that uses @FSBlockDeviceResource@, conform to @FSManageableResourceMaintenanceOperations@.
--
-- Generated bindings for @FSBlockDeviceResource@.
module ObjC.FSKit.FSBlockDeviceResource
  ( FSBlockDeviceResource
  , IsFSBlockDeviceResource(..)
  , init_
  , readInto_startingAt_length_completionHandler
  , readInto_startingAt_length_error
  , writeFrom_startingAt_length_completionHandler
  , writeFrom_startingAt_length_error
  , metadataReadInto_startingAt_length_error
  , metadataWriteFrom_startingAt_length_error
  , delayedMetadataWriteFrom_startingAt_length_error
  , metadataFlushWithError
  , asynchronousMetadataFlushWithError
  , metadataClear_withDelayedWrites_error
  , metadataPurge_error
  , bsdName
  , writable
  , blockSize
  , blockCount
  , physicalBlockSize
  , asynchronousMetadataFlushWithErrorSelector
  , blockCountSelector
  , blockSizeSelector
  , bsdNameSelector
  , delayedMetadataWriteFrom_startingAt_length_errorSelector
  , initSelector
  , metadataClear_withDelayedWrites_errorSelector
  , metadataFlushWithErrorSelector
  , metadataPurge_errorSelector
  , metadataReadInto_startingAt_length_errorSelector
  , metadataWriteFrom_startingAt_length_errorSelector
  , physicalBlockSizeSelector
  , readInto_startingAt_length_completionHandlerSelector
  , readInto_startingAt_length_errorSelector
  , writableSelector
  , writeFrom_startingAt_length_completionHandlerSelector
  , writeFrom_startingAt_length_errorSelector


  ) where

import Foreign.Ptr (Ptr, FunPtr)
import Foreign.C.Types

import ObjC.Runtime.Types
import ObjC.Runtime.Message (sendMessage, sendOwnedMessage, sendClassMessage, sendOwnedClassMessage)
import ObjC.Runtime.Selector (mkSelector)
import ObjC.Runtime.Class (getRequiredClass)

import ObjC.FSKit.Internal.Classes
import ObjC.Foundation.Internal.Classes

-- | @- init@
init_ :: IsFSBlockDeviceResource fsBlockDeviceResource => fsBlockDeviceResource -> IO (Id FSBlockDeviceResource)
init_ fsBlockDeviceResource =
  sendOwnedMessage fsBlockDeviceResource initSelector

-- | Reads data from the resource into a buffer and executes a block afterwards.
--
-- For the read to succeed, requests must conform to any transfer requirements of the underlying resource. Disk drives typically require sector (@physicalBlockSize@) addressed operations of one or more sector-aligned offsets.
--
-- - Parameters:   - buffer: A buffer to receive the data.   - offset: The offset into the resource from which to start reading.   - length: A maximum number of bytes to read. The completion handler receives a parameter with the actual number of bytes read.   - completionHandler: A block that executes after the read operation completes. If successful, the first parameter contains the number of bytes actually read. In the case of an error, the second parameter contains a non-@nil@ error. This value is @EFAULT@ if @buffer@ is @NULL@, or @errno@ if reading from the resource failed.
--
-- ObjC selector: @- readInto:startingAt:length:completionHandler:@
readInto_startingAt_length_completionHandler :: IsFSBlockDeviceResource fsBlockDeviceResource => fsBlockDeviceResource -> Ptr () -> CLong -> CULong -> Ptr () -> IO ()
readInto_startingAt_length_completionHandler fsBlockDeviceResource buffer offset length_ completionHandler =
  sendMessage fsBlockDeviceResource readInto_startingAt_length_completionHandlerSelector buffer offset length_ completionHandler

-- | Synchronously reads data from the resource into a buffer.
--
-- This is a synchronous version of ``readInto:startingAt:length:completionHandler:``.
--
-- > Note: In some cases, this method performs a partial read. In this case, the return value is shorter than the requested length, and the @error@ is set to @nil@.
--
-- - Parameters:   - buffer: A buffer to receive the data.   - offset: The offset into the resource from which to start reading.   - length: A maximum number of bytes to read. The method's return value contains the actual number of bytes read.   - error: On return, any error encountered while reading data, or @nil@ if no error occurred.
--
-- - Returns: The actual number of bytes read.
--
-- ObjC selector: @- readInto:startingAt:length:error:@
readInto_startingAt_length_error :: (IsFSBlockDeviceResource fsBlockDeviceResource, IsNSError error_) => fsBlockDeviceResource -> Ptr () -> CLong -> CULong -> error_ -> IO CULong
readInto_startingAt_length_error fsBlockDeviceResource buffer offset length_ error_ =
  sendMessage fsBlockDeviceResource readInto_startingAt_length_errorSelector buffer offset length_ (toNSError error_)

-- | Writes data from from a buffer to the resource and executes a block afterwards.
--
-- For the write to succeed, requests must conform to any transfer requirements of the underlying resource. Disk drives typically require sector (@physicalBlockSize@) addressed operations of one or more sector-aligned offsets.
--
-- - Parameters:   - buffer: A buffer to provide the data.   - offset: The offset into the resource from which to start writing.   - length: A maximum number of bytes to write. The completion handler receives a parameter with the actual number of bytes write.   - completionHandler: A block that executes after the write operation completes. If successful, the first parameter contains the number of bytes actually written. In the case of an error, the second parameter contains a non-@nil@ error. This value is @EFAULT@ if @buffer@ is @NULL@, or @errno@ if writing to the resource failed.
--
-- ObjC selector: @- writeFrom:startingAt:length:completionHandler:@
writeFrom_startingAt_length_completionHandler :: IsFSBlockDeviceResource fsBlockDeviceResource => fsBlockDeviceResource -> Ptr () -> CLong -> CULong -> Ptr () -> IO ()
writeFrom_startingAt_length_completionHandler fsBlockDeviceResource buffer offset length_ completionHandler =
  sendMessage fsBlockDeviceResource writeFrom_startingAt_length_completionHandlerSelector buffer offset length_ completionHandler

-- | Synchronously writes data from from a buffer to the resource and executes a block afterwards.
--
-- This is a synchronous version of ``writeFrom:startingAt:length:completionHandler:``.
--
-- > Note: In some cases, this method performs a partial write. In this case, the return value is shorter than the requested length, and the @error@ is set to @nil@.
--
-- - Parameters:   - buffer: A buffer to provide the data.   - offset: The offset into the resource from which to start writing.   - length: A maximum number of bytes to write. The completion handler receives a parameter with the actual number of bytes write.   - error: On return, any error encountered while writing data, or @nil@ if no error occurred.
--
-- - Returns: The actual number of bytes written.
--
-- ObjC selector: @- writeFrom:startingAt:length:error:@
writeFrom_startingAt_length_error :: (IsFSBlockDeviceResource fsBlockDeviceResource, IsNSError error_) => fsBlockDeviceResource -> Ptr () -> CLong -> CULong -> error_ -> IO CULong
writeFrom_startingAt_length_error fsBlockDeviceResource buffer offset length_ error_ =
  sendMessage fsBlockDeviceResource writeFrom_startingAt_length_errorSelector buffer offset length_ (toNSError error_)

-- | Synchronously reads file system metadata from the resource into a buffer.
--
-- This method provides access to the Kernel Buffer Cache, which is the primary system cache for file system metadata. Unlike equivalent kernel APIs, this method doesn't hold any kernel-level claim to the underlying buffers.
--
-- For the read to succeed, requests must conform to any transfer requirements of the underlying resource. Disk drives typically require sector (@physicalBlockSize@) addressed operations of one or more sector-aligned offsets.
--
-- This method doesn't support partial reading of metadata.
--
-- - Parameters:   - buffer: A buffer to receive the data.   - offset: The offset into the resource from which to start reading.   - length: The number of bytes to read.   - error: On return, any error encountered while reading data, or @nil@ if no error occurred.
--
-- - Returns: A Boolean value indicating whether the metadata read succeeded.
--
-- ObjC selector: @- metadataReadInto:startingAt:length:error:@
metadataReadInto_startingAt_length_error :: (IsFSBlockDeviceResource fsBlockDeviceResource, IsNSError error_) => fsBlockDeviceResource -> Ptr () -> CLong -> CULong -> error_ -> IO Bool
metadataReadInto_startingAt_length_error fsBlockDeviceResource buffer offset length_ error_ =
  sendMessage fsBlockDeviceResource metadataReadInto_startingAt_length_errorSelector buffer offset length_ (toNSError error_)

-- | Synchronously writes file system metadata from a buffer to the resource.
--
-- This method provides access to the Kernel Buffer Cache, which is the primary system cache for file system metadata. Unlike equivalent kernel APIs, this method doesn't hold any kernel-level claim to the underlying buffers.
--
-- For the write to succeed, requests must conform to any transfer requirements of the underlying resource. Disk drives typically require sector (@physicalBlockSize@) addressed operations of one or more sector-aligned offsets.
--
-- This method doesn't support partial writing of metadata.
--
-- - Parameters:   - buffer: A buffer to provide the data.   - offset: The offset into the resource from which to start writing.   - length: The number of bytes to writing.   - error: On return, any error encountered while writing data, or @nil@ if no error occurred.
--
-- - Returns: A Boolean value indicating whether the metadata write succeeded.
--
-- ObjC selector: @- metadataWriteFrom:startingAt:length:error:@
metadataWriteFrom_startingAt_length_error :: (IsFSBlockDeviceResource fsBlockDeviceResource, IsNSError error_) => fsBlockDeviceResource -> Ptr () -> CLong -> CULong -> error_ -> IO Bool
metadataWriteFrom_startingAt_length_error fsBlockDeviceResource buffer offset length_ error_ =
  sendMessage fsBlockDeviceResource metadataWriteFrom_startingAt_length_errorSelector buffer offset length_ (toNSError error_)

-- | Writes file system metadata from a buffer to a cache, prior to flushing it to the resource.
--
-- This method provides access to the Kernel Buffer Cache, which is the primary system cache for file system metadata. Unlike equivalent kernel APIs, this method doesn't hold any kernel-level claim to the underlying buffers.
--
-- This method is equivalent to ``metadataWriteFrom:startingAt:length:error:``, except that it writes data to the resource's buffer cache instead of writing to disk immediately. To ensure writing data to disk, the client must flush the metadata by calling ``metadataFlushWithError:`` or ``asynchronousMetadataFlushWithError:``.
--
-- Delayed writes offer two significant advantages: - Delayed writes are more performant, since the file system can avoid waiting for the actual write, reducing I/O latency. - When writing to a specific range repeatedly, delayed writes allow the file system to flush data to the disk only when necessary. This reduces disk usage by eliminating unnecessary writes.
--
-- For the write to succeed, requests must conform to any transfer requirements of the underlying resource. Disk drives typically require sector (@physicalBlockSize@) addressed operations of one or more sector-aligned offsets.
--
-- This method doesn't support partial writing of metadata.
--
-- - Parameters:   - buffer: A buffer to provide the data.   - offset: The offset into the resource from which to start writing.   - length: The number of bytes to writing.   - error: On return, any error encountered while writing data, or @nil@ if no error occurred.
--
-- - Returns: A Boolean value indicating whether the metadata write succeeded.
--
-- ObjC selector: @- delayedMetadataWriteFrom:startingAt:length:error:@
delayedMetadataWriteFrom_startingAt_length_error :: (IsFSBlockDeviceResource fsBlockDeviceResource, IsNSError error_) => fsBlockDeviceResource -> Ptr () -> CLong -> CULong -> error_ -> IO Bool
delayedMetadataWriteFrom_startingAt_length_error fsBlockDeviceResource buffer offset length_ error_ =
  sendMessage fsBlockDeviceResource delayedMetadataWriteFrom_startingAt_length_errorSelector buffer offset length_ (toNSError error_)

-- | Synchronously flushes the resource's buffer cache.
--
-- This method flushes data previously written with ``delayedMetadataWriteFrom:startingAt:length:error:`` to the resource.
--
-- - Parameter error: On return, any error encountered while writing data, or @nil@ if no error occurred.
--
-- - Returns: A Boolean value indicating whether the metadata flush succeeded.
--
-- ObjC selector: @- metadataFlushWithError:@
metadataFlushWithError :: (IsFSBlockDeviceResource fsBlockDeviceResource, IsNSError error_) => fsBlockDeviceResource -> error_ -> IO Bool
metadataFlushWithError fsBlockDeviceResource error_ =
  sendMessage fsBlockDeviceResource metadataFlushWithErrorSelector (toNSError error_)

-- | Asynchronously flushes the resource's buffer cache.
--
-- This method schedules a flush of data previously written with ``delayedMetadataWriteFrom:startingAt:length:error:`` to the resource and returns immediately without blocking. This method doesn't wait to check the flush's status. If an error prevents the flush from being scheduled, the error is indicated by the in-out @error@ parameter.
--
-- - Parameter error: On return, any error encountered while writing data, or @nil@ if no error occurred.
--
-- - Returns: A Boolean value indicating whether scheduling the metadata flush succeeded.
--
-- ObjC selector: @- asynchronousMetadataFlushWithError:@
asynchronousMetadataFlushWithError :: (IsFSBlockDeviceResource fsBlockDeviceResource, IsNSError error_) => fsBlockDeviceResource -> error_ -> IO Bool
asynchronousMetadataFlushWithError fsBlockDeviceResource error_ =
  sendMessage fsBlockDeviceResource asynchronousMetadataFlushWithErrorSelector (toNSError error_)

-- | Clears the given ranges within the buffer cache.
--
-- This method clears the specified ranges in the resource’s buffer cache by writing zeroes into them.
--
-- - Parameters:   - rangesToClear: The metadata ranges to clear.   - withDelayedWrites: A Boolean value that determines whether to perform the clear operation with delayed writes. The delay works in the same manner as ``delayedMetadataWriteFrom:startingAt:length:error:``. When using delayed writes, the client can flush the metadata with ``metadataFlushWithError:`` or ``asynchronousMetadataFlushWithError:``. The system also flushes stale data in the buffer cache periodically.   - error: On return, any error encountered while writing data, or @nil@ if no error occurred. This value is @EINVAL@ if @rangesToClear@ is invalid.
--
-- - Returns: A Boolean value indicating whether clearing the metadata succeeded.
--
-- ObjC selector: @- metadataClear:withDelayedWrites:error:@
metadataClear_withDelayedWrites_error :: (IsFSBlockDeviceResource fsBlockDeviceResource, IsNSArray rangesToClear, IsNSError error_) => fsBlockDeviceResource -> rangesToClear -> Bool -> error_ -> IO Bool
metadataClear_withDelayedWrites_error fsBlockDeviceResource rangesToClear withDelayedWrites error_ =
  sendMessage fsBlockDeviceResource metadataClear_withDelayedWrites_errorSelector (toNSArray rangesToClear) withDelayedWrites (toNSError error_)

-- | Synchronously purges the given ranges from the buffer cache.
--
-- This method removes the given ranges from the resource's buffer cache. This process drops any dirty data in the cache, preventing the data from reaching the device.
--
-- - Parameters:   - rangesToPurge: The metadata ranges to purge.   - error: On return, any error encountered while writing data, or @nil@ if no error occurred. This value is @EINVAL@ if @rangesToPurge@ is invalid.
--
-- - Returns: A Boolean value indicating whether purging the metadata succeeded.
--
-- ObjC selector: @- metadataPurge:error:@
metadataPurge_error :: (IsFSBlockDeviceResource fsBlockDeviceResource, IsNSArray rangesToPurge, IsNSError error_) => fsBlockDeviceResource -> rangesToPurge -> error_ -> IO Bool
metadataPurge_error fsBlockDeviceResource rangesToPurge error_ =
  sendMessage fsBlockDeviceResource metadataPurge_errorSelector (toNSArray rangesToPurge) (toNSError error_)

-- | The device name of the resource.
--
-- ObjC selector: @- BSDName@
bsdName :: IsFSBlockDeviceResource fsBlockDeviceResource => fsBlockDeviceResource -> IO (Id NSString)
bsdName fsBlockDeviceResource =
  sendMessage fsBlockDeviceResource bsdNameSelector

-- | A Boolean property that indicates whether the resource can write data to the device.
--
-- ObjC selector: @- writable@
writable :: IsFSBlockDeviceResource fsBlockDeviceResource => fsBlockDeviceResource -> IO Bool
writable fsBlockDeviceResource =
  sendMessage fsBlockDeviceResource writableSelector

-- | The logical block size, the size of data blocks used by the file system.
--
-- This is equivalent to the @DKIOCGETBLOCKSIZE@ device parameter.
--
-- ObjC selector: @- blockSize@
blockSize :: IsFSBlockDeviceResource fsBlockDeviceResource => fsBlockDeviceResource -> IO CULong
blockSize fsBlockDeviceResource =
  sendMessage fsBlockDeviceResource blockSizeSelector

-- | The block count on this resource.
--
-- ObjC selector: @- blockCount@
blockCount :: IsFSBlockDeviceResource fsBlockDeviceResource => fsBlockDeviceResource -> IO CULong
blockCount fsBlockDeviceResource =
  sendMessage fsBlockDeviceResource blockCountSelector

-- | The sector size of the device.
--
-- This is equivalent to the @DKIOCGETPHYSICALBLOCKSIZE@ device parameter.
--
-- ObjC selector: @- physicalBlockSize@
physicalBlockSize :: IsFSBlockDeviceResource fsBlockDeviceResource => fsBlockDeviceResource -> IO CULong
physicalBlockSize fsBlockDeviceResource =
  sendMessage fsBlockDeviceResource physicalBlockSizeSelector

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @init@
initSelector :: Selector '[] (Id FSBlockDeviceResource)
initSelector = mkSelector "init"

-- | @Selector@ for @readInto:startingAt:length:completionHandler:@
readInto_startingAt_length_completionHandlerSelector :: Selector '[Ptr (), CLong, CULong, Ptr ()] ()
readInto_startingAt_length_completionHandlerSelector = mkSelector "readInto:startingAt:length:completionHandler:"

-- | @Selector@ for @readInto:startingAt:length:error:@
readInto_startingAt_length_errorSelector :: Selector '[Ptr (), CLong, CULong, Id NSError] CULong
readInto_startingAt_length_errorSelector = mkSelector "readInto:startingAt:length:error:"

-- | @Selector@ for @writeFrom:startingAt:length:completionHandler:@
writeFrom_startingAt_length_completionHandlerSelector :: Selector '[Ptr (), CLong, CULong, Ptr ()] ()
writeFrom_startingAt_length_completionHandlerSelector = mkSelector "writeFrom:startingAt:length:completionHandler:"

-- | @Selector@ for @writeFrom:startingAt:length:error:@
writeFrom_startingAt_length_errorSelector :: Selector '[Ptr (), CLong, CULong, Id NSError] CULong
writeFrom_startingAt_length_errorSelector = mkSelector "writeFrom:startingAt:length:error:"

-- | @Selector@ for @metadataReadInto:startingAt:length:error:@
metadataReadInto_startingAt_length_errorSelector :: Selector '[Ptr (), CLong, CULong, Id NSError] Bool
metadataReadInto_startingAt_length_errorSelector = mkSelector "metadataReadInto:startingAt:length:error:"

-- | @Selector@ for @metadataWriteFrom:startingAt:length:error:@
metadataWriteFrom_startingAt_length_errorSelector :: Selector '[Ptr (), CLong, CULong, Id NSError] Bool
metadataWriteFrom_startingAt_length_errorSelector = mkSelector "metadataWriteFrom:startingAt:length:error:"

-- | @Selector@ for @delayedMetadataWriteFrom:startingAt:length:error:@
delayedMetadataWriteFrom_startingAt_length_errorSelector :: Selector '[Ptr (), CLong, CULong, Id NSError] Bool
delayedMetadataWriteFrom_startingAt_length_errorSelector = mkSelector "delayedMetadataWriteFrom:startingAt:length:error:"

-- | @Selector@ for @metadataFlushWithError:@
metadataFlushWithErrorSelector :: Selector '[Id NSError] Bool
metadataFlushWithErrorSelector = mkSelector "metadataFlushWithError:"

-- | @Selector@ for @asynchronousMetadataFlushWithError:@
asynchronousMetadataFlushWithErrorSelector :: Selector '[Id NSError] Bool
asynchronousMetadataFlushWithErrorSelector = mkSelector "asynchronousMetadataFlushWithError:"

-- | @Selector@ for @metadataClear:withDelayedWrites:error:@
metadataClear_withDelayedWrites_errorSelector :: Selector '[Id NSArray, Bool, Id NSError] Bool
metadataClear_withDelayedWrites_errorSelector = mkSelector "metadataClear:withDelayedWrites:error:"

-- | @Selector@ for @metadataPurge:error:@
metadataPurge_errorSelector :: Selector '[Id NSArray, Id NSError] Bool
metadataPurge_errorSelector = mkSelector "metadataPurge:error:"

-- | @Selector@ for @BSDName@
bsdNameSelector :: Selector '[] (Id NSString)
bsdNameSelector = mkSelector "BSDName"

-- | @Selector@ for @writable@
writableSelector :: Selector '[] Bool
writableSelector = mkSelector "writable"

-- | @Selector@ for @blockSize@
blockSizeSelector :: Selector '[] CULong
blockSizeSelector = mkSelector "blockSize"

-- | @Selector@ for @blockCount@
blockCountSelector :: Selector '[] CULong
blockCountSelector = mkSelector "blockCount"

-- | @Selector@ for @physicalBlockSize@
physicalBlockSizeSelector :: Selector '[] CULong
physicalBlockSizeSelector = mkSelector "physicalBlockSize"

