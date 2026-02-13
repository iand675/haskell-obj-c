{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | A type used to report a volume's statistics.
--
-- The names of this type's properties match those in the @statfs@ structure in @statfs(2)@, which reports these values for an FSKit file system. All numeric properties default to @0@. Override these values, unless a given property has no meaningful value to provide.
--
-- > Note: Available space, free space, total space, and used space have properties to express their values either as a number of blocks or a number of bytes. Your module may supply both of these values by setting both the relevant block or byte property. Alternatively, a module may set only one of the two properties. When you do this, FSKit calculates the matching value based on ``blockSize``.
--
-- For the read-only ``fileSystemTypeName``, set this value with the designated initializer.
--
-- Generated bindings for @FSStatFSResult@.
module ObjC.FSKit.FSStatFSResult
  ( FSStatFSResult
  , IsFSStatFSResult(..)
  , initWithFileSystemTypeName
  , init_
  , blockSize
  , setBlockSize
  , ioSize
  , setIoSize
  , totalBlocks
  , setTotalBlocks
  , availableBlocks
  , setAvailableBlocks
  , freeBlocks
  , setFreeBlocks
  , usedBlocks
  , setUsedBlocks
  , totalBytes
  , setTotalBytes
  , availableBytes
  , setAvailableBytes
  , freeBytes
  , setFreeBytes
  , usedBytes
  , setUsedBytes
  , totalFiles
  , setTotalFiles
  , freeFiles
  , setFreeFiles
  , fileSystemSubType
  , setFileSystemSubType
  , fileSystemTypeName
  , availableBlocksSelector
  , availableBytesSelector
  , blockSizeSelector
  , fileSystemSubTypeSelector
  , fileSystemTypeNameSelector
  , freeBlocksSelector
  , freeBytesSelector
  , freeFilesSelector
  , initSelector
  , initWithFileSystemTypeNameSelector
  , ioSizeSelector
  , setAvailableBlocksSelector
  , setAvailableBytesSelector
  , setBlockSizeSelector
  , setFileSystemSubTypeSelector
  , setFreeBlocksSelector
  , setFreeBytesSelector
  , setFreeFilesSelector
  , setIoSizeSelector
  , setTotalBlocksSelector
  , setTotalBytesSelector
  , setTotalFilesSelector
  , setUsedBlocksSelector
  , setUsedBytesSelector
  , totalBlocksSelector
  , totalBytesSelector
  , totalFilesSelector
  , usedBlocksSelector
  , usedBytesSelector


  ) where

import Foreign.Ptr (Ptr, FunPtr)
import Foreign.C.Types

import ObjC.Runtime.Types
import ObjC.Runtime.Message (sendMessage, sendOwnedMessage, sendClassMessage, sendOwnedClassMessage)
import ObjC.Runtime.Selector (mkSelector)
import ObjC.Runtime.Class (getRequiredClass)

import ObjC.FSKit.Internal.Classes
import ObjC.Foundation.Internal.Classes

-- | Creates an statistics result instance, using the given file system type name.
--
-- - Parameters fileSystemTypeName: A type name for the file system. The maximum allowed length is @MFSTYPENAMELEN@, including the terminating @NUL@ character.
--
-- ObjC selector: @- initWithFileSystemTypeName:@
initWithFileSystemTypeName :: (IsFSStatFSResult fsStatFSResult, IsNSString fileSystemTypeName) => fsStatFSResult -> fileSystemTypeName -> IO (Id FSStatFSResult)
initWithFileSystemTypeName fsStatFSResult fileSystemTypeName =
  sendOwnedMessage fsStatFSResult initWithFileSystemTypeNameSelector (toNSString fileSystemTypeName)

-- | @- init@
init_ :: IsFSStatFSResult fsStatFSResult => fsStatFSResult -> IO (Id FSStatFSResult)
init_ fsStatFSResult =
  sendOwnedMessage fsStatFSResult initSelector

-- | A property for the volume's block size, in bytes.
--
-- This value defaults to @4096@. Zero isn't a valid block size.
--
-- ObjC selector: @- blockSize@
blockSize :: IsFSStatFSResult fsStatFSResult => fsStatFSResult -> IO CLong
blockSize fsStatFSResult =
  sendMessage fsStatFSResult blockSizeSelector

-- | A property for the volume's block size, in bytes.
--
-- This value defaults to @4096@. Zero isn't a valid block size.
--
-- ObjC selector: @- setBlockSize:@
setBlockSize :: IsFSStatFSResult fsStatFSResult => fsStatFSResult -> CLong -> IO ()
setBlockSize fsStatFSResult value =
  sendMessage fsStatFSResult setBlockSizeSelector value

-- | A property for the optimal block size with which to perform I/O.
--
-- For best performance, specify an @ioSize@ that's an even multiple of ``blockSize``.
--
-- ObjC selector: @- ioSize@
ioSize :: IsFSStatFSResult fsStatFSResult => fsStatFSResult -> IO CLong
ioSize fsStatFSResult =
  sendMessage fsStatFSResult ioSizeSelector

-- | A property for the optimal block size with which to perform I/O.
--
-- For best performance, specify an @ioSize@ that's an even multiple of ``blockSize``.
--
-- ObjC selector: @- setIoSize:@
setIoSize :: IsFSStatFSResult fsStatFSResult => fsStatFSResult -> CLong -> IO ()
setIoSize fsStatFSResult value =
  sendMessage fsStatFSResult setIoSizeSelector value

-- | A property for the volume's total data block count.
--
-- ObjC selector: @- totalBlocks@
totalBlocks :: IsFSStatFSResult fsStatFSResult => fsStatFSResult -> IO CULong
totalBlocks fsStatFSResult =
  sendMessage fsStatFSResult totalBlocksSelector

-- | A property for the volume's total data block count.
--
-- ObjC selector: @- setTotalBlocks:@
setTotalBlocks :: IsFSStatFSResult fsStatFSResult => fsStatFSResult -> CULong -> IO ()
setTotalBlocks fsStatFSResult value =
  sendMessage fsStatFSResult setTotalBlocksSelector value

-- | A property for the number of free blocks available to a non-superuser on the volume.
--
-- ObjC selector: @- availableBlocks@
availableBlocks :: IsFSStatFSResult fsStatFSResult => fsStatFSResult -> IO CULong
availableBlocks fsStatFSResult =
  sendMessage fsStatFSResult availableBlocksSelector

-- | A property for the number of free blocks available to a non-superuser on the volume.
--
-- ObjC selector: @- setAvailableBlocks:@
setAvailableBlocks :: IsFSStatFSResult fsStatFSResult => fsStatFSResult -> CULong -> IO ()
setAvailableBlocks fsStatFSResult value =
  sendMessage fsStatFSResult setAvailableBlocksSelector value

-- | A property for the number of free blocks in the volume.
--
-- ObjC selector: @- freeBlocks@
freeBlocks :: IsFSStatFSResult fsStatFSResult => fsStatFSResult -> IO CULong
freeBlocks fsStatFSResult =
  sendMessage fsStatFSResult freeBlocksSelector

-- | A property for the number of free blocks in the volume.
--
-- ObjC selector: @- setFreeBlocks:@
setFreeBlocks :: IsFSStatFSResult fsStatFSResult => fsStatFSResult -> CULong -> IO ()
setFreeBlocks fsStatFSResult value =
  sendMessage fsStatFSResult setFreeBlocksSelector value

-- | A property for the number of used blocks in the volume.
--
-- ObjC selector: @- usedBlocks@
usedBlocks :: IsFSStatFSResult fsStatFSResult => fsStatFSResult -> IO CULong
usedBlocks fsStatFSResult =
  sendMessage fsStatFSResult usedBlocksSelector

-- | A property for the number of used blocks in the volume.
--
-- ObjC selector: @- setUsedBlocks:@
setUsedBlocks :: IsFSStatFSResult fsStatFSResult => fsStatFSResult -> CULong -> IO ()
setUsedBlocks fsStatFSResult value =
  sendMessage fsStatFSResult setUsedBlocksSelector value

-- | A property for the total size, in bytes, of the volume.
--
-- ObjC selector: @- totalBytes@
totalBytes :: IsFSStatFSResult fsStatFSResult => fsStatFSResult -> IO CULong
totalBytes fsStatFSResult =
  sendMessage fsStatFSResult totalBytesSelector

-- | A property for the total size, in bytes, of the volume.
--
-- ObjC selector: @- setTotalBytes:@
setTotalBytes :: IsFSStatFSResult fsStatFSResult => fsStatFSResult -> CULong -> IO ()
setTotalBytes fsStatFSResult value =
  sendMessage fsStatFSResult setTotalBytesSelector value

-- | A property for the amount of space available to users, in bytes, in the volume.
--
-- ObjC selector: @- availableBytes@
availableBytes :: IsFSStatFSResult fsStatFSResult => fsStatFSResult -> IO CULong
availableBytes fsStatFSResult =
  sendMessage fsStatFSResult availableBytesSelector

-- | A property for the amount of space available to users, in bytes, in the volume.
--
-- ObjC selector: @- setAvailableBytes:@
setAvailableBytes :: IsFSStatFSResult fsStatFSResult => fsStatFSResult -> CULong -> IO ()
setAvailableBytes fsStatFSResult value =
  sendMessage fsStatFSResult setAvailableBytesSelector value

-- | A property for the amount of free space, in bytes, in the volume.
--
-- ObjC selector: @- freeBytes@
freeBytes :: IsFSStatFSResult fsStatFSResult => fsStatFSResult -> IO CULong
freeBytes fsStatFSResult =
  sendMessage fsStatFSResult freeBytesSelector

-- | A property for the amount of free space, in bytes, in the volume.
--
-- ObjC selector: @- setFreeBytes:@
setFreeBytes :: IsFSStatFSResult fsStatFSResult => fsStatFSResult -> CULong -> IO ()
setFreeBytes fsStatFSResult value =
  sendMessage fsStatFSResult setFreeBytesSelector value

-- | A property for the amount of used space, in bytes, in the volume.
--
-- ObjC selector: @- usedBytes@
usedBytes :: IsFSStatFSResult fsStatFSResult => fsStatFSResult -> IO CULong
usedBytes fsStatFSResult =
  sendMessage fsStatFSResult usedBytesSelector

-- | A property for the amount of used space, in bytes, in the volume.
--
-- ObjC selector: @- setUsedBytes:@
setUsedBytes :: IsFSStatFSResult fsStatFSResult => fsStatFSResult -> CULong -> IO ()
setUsedBytes fsStatFSResult value =
  sendMessage fsStatFSResult setUsedBytesSelector value

-- | A property for the total number of file slots in the volume,
--
-- ObjC selector: @- totalFiles@
totalFiles :: IsFSStatFSResult fsStatFSResult => fsStatFSResult -> IO CULong
totalFiles fsStatFSResult =
  sendMessage fsStatFSResult totalFilesSelector

-- | A property for the total number of file slots in the volume,
--
-- ObjC selector: @- setTotalFiles:@
setTotalFiles :: IsFSStatFSResult fsStatFSResult => fsStatFSResult -> CULong -> IO ()
setTotalFiles fsStatFSResult value =
  sendMessage fsStatFSResult setTotalFilesSelector value

-- | A property for the total number of free file slots in the volume.
--
-- ObjC selector: @- freeFiles@
freeFiles :: IsFSStatFSResult fsStatFSResult => fsStatFSResult -> IO CULong
freeFiles fsStatFSResult =
  sendMessage fsStatFSResult freeFilesSelector

-- | A property for the total number of free file slots in the volume.
--
-- ObjC selector: @- setFreeFiles:@
setFreeFiles :: IsFSStatFSResult fsStatFSResult => fsStatFSResult -> CULong -> IO ()
setFreeFiles fsStatFSResult value =
  sendMessage fsStatFSResult setFreeFilesSelector value

-- | A property for the file system's subtype or flavor.
--
-- Match this value to the @FSPersonalities@'s @FSSubType@ attribute, if it exists within the @EXAppExtensionAttributes@ dictionary of the module's @Info.plist@.
--
-- ObjC selector: @- fileSystemSubType@
fileSystemSubType :: IsFSStatFSResult fsStatFSResult => fsStatFSResult -> IO CLong
fileSystemSubType fsStatFSResult =
  sendMessage fsStatFSResult fileSystemSubTypeSelector

-- | A property for the file system's subtype or flavor.
--
-- Match this value to the @FSPersonalities@'s @FSSubType@ attribute, if it exists within the @EXAppExtensionAttributes@ dictionary of the module's @Info.plist@.
--
-- ObjC selector: @- setFileSystemSubType:@
setFileSystemSubType :: IsFSStatFSResult fsStatFSResult => fsStatFSResult -> CLong -> IO ()
setFileSystemSubType fsStatFSResult value =
  sendMessage fsStatFSResult setFileSystemSubTypeSelector value

-- | A property for the file system type name.
--
-- Match this value to the @FSShortName@ attribute within the @EXAppExtensionAttributes@ dictionary of the module's @Info.plist@. The maximum allowed length is @MFSTYPENAMELEN@, including the terminating @NUL@ character.
--
-- ObjC selector: @- fileSystemTypeName@
fileSystemTypeName :: IsFSStatFSResult fsStatFSResult => fsStatFSResult -> IO (Id NSString)
fileSystemTypeName fsStatFSResult =
  sendMessage fsStatFSResult fileSystemTypeNameSelector

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @initWithFileSystemTypeName:@
initWithFileSystemTypeNameSelector :: Selector '[Id NSString] (Id FSStatFSResult)
initWithFileSystemTypeNameSelector = mkSelector "initWithFileSystemTypeName:"

-- | @Selector@ for @init@
initSelector :: Selector '[] (Id FSStatFSResult)
initSelector = mkSelector "init"

-- | @Selector@ for @blockSize@
blockSizeSelector :: Selector '[] CLong
blockSizeSelector = mkSelector "blockSize"

-- | @Selector@ for @setBlockSize:@
setBlockSizeSelector :: Selector '[CLong] ()
setBlockSizeSelector = mkSelector "setBlockSize:"

-- | @Selector@ for @ioSize@
ioSizeSelector :: Selector '[] CLong
ioSizeSelector = mkSelector "ioSize"

-- | @Selector@ for @setIoSize:@
setIoSizeSelector :: Selector '[CLong] ()
setIoSizeSelector = mkSelector "setIoSize:"

-- | @Selector@ for @totalBlocks@
totalBlocksSelector :: Selector '[] CULong
totalBlocksSelector = mkSelector "totalBlocks"

-- | @Selector@ for @setTotalBlocks:@
setTotalBlocksSelector :: Selector '[CULong] ()
setTotalBlocksSelector = mkSelector "setTotalBlocks:"

-- | @Selector@ for @availableBlocks@
availableBlocksSelector :: Selector '[] CULong
availableBlocksSelector = mkSelector "availableBlocks"

-- | @Selector@ for @setAvailableBlocks:@
setAvailableBlocksSelector :: Selector '[CULong] ()
setAvailableBlocksSelector = mkSelector "setAvailableBlocks:"

-- | @Selector@ for @freeBlocks@
freeBlocksSelector :: Selector '[] CULong
freeBlocksSelector = mkSelector "freeBlocks"

-- | @Selector@ for @setFreeBlocks:@
setFreeBlocksSelector :: Selector '[CULong] ()
setFreeBlocksSelector = mkSelector "setFreeBlocks:"

-- | @Selector@ for @usedBlocks@
usedBlocksSelector :: Selector '[] CULong
usedBlocksSelector = mkSelector "usedBlocks"

-- | @Selector@ for @setUsedBlocks:@
setUsedBlocksSelector :: Selector '[CULong] ()
setUsedBlocksSelector = mkSelector "setUsedBlocks:"

-- | @Selector@ for @totalBytes@
totalBytesSelector :: Selector '[] CULong
totalBytesSelector = mkSelector "totalBytes"

-- | @Selector@ for @setTotalBytes:@
setTotalBytesSelector :: Selector '[CULong] ()
setTotalBytesSelector = mkSelector "setTotalBytes:"

-- | @Selector@ for @availableBytes@
availableBytesSelector :: Selector '[] CULong
availableBytesSelector = mkSelector "availableBytes"

-- | @Selector@ for @setAvailableBytes:@
setAvailableBytesSelector :: Selector '[CULong] ()
setAvailableBytesSelector = mkSelector "setAvailableBytes:"

-- | @Selector@ for @freeBytes@
freeBytesSelector :: Selector '[] CULong
freeBytesSelector = mkSelector "freeBytes"

-- | @Selector@ for @setFreeBytes:@
setFreeBytesSelector :: Selector '[CULong] ()
setFreeBytesSelector = mkSelector "setFreeBytes:"

-- | @Selector@ for @usedBytes@
usedBytesSelector :: Selector '[] CULong
usedBytesSelector = mkSelector "usedBytes"

-- | @Selector@ for @setUsedBytes:@
setUsedBytesSelector :: Selector '[CULong] ()
setUsedBytesSelector = mkSelector "setUsedBytes:"

-- | @Selector@ for @totalFiles@
totalFilesSelector :: Selector '[] CULong
totalFilesSelector = mkSelector "totalFiles"

-- | @Selector@ for @setTotalFiles:@
setTotalFilesSelector :: Selector '[CULong] ()
setTotalFilesSelector = mkSelector "setTotalFiles:"

-- | @Selector@ for @freeFiles@
freeFilesSelector :: Selector '[] CULong
freeFilesSelector = mkSelector "freeFiles"

-- | @Selector@ for @setFreeFiles:@
setFreeFilesSelector :: Selector '[CULong] ()
setFreeFilesSelector = mkSelector "setFreeFiles:"

-- | @Selector@ for @fileSystemSubType@
fileSystemSubTypeSelector :: Selector '[] CLong
fileSystemSubTypeSelector = mkSelector "fileSystemSubType"

-- | @Selector@ for @setFileSystemSubType:@
setFileSystemSubTypeSelector :: Selector '[CLong] ()
setFileSystemSubTypeSelector = mkSelector "setFileSystemSubType:"

-- | @Selector@ for @fileSystemTypeName@
fileSystemTypeNameSelector :: Selector '[] (Id NSString)
fileSystemTypeNameSelector = mkSelector "fileSystemTypeName"

