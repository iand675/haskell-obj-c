{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | MPSVector
--
-- This depends on Metal.framework
--
-- A MPSVector object describes a 1-dimensional array of data and provides storage              for its values.  Some MPSMatrixKernel objects operate on MPSVector objects              for convenience.
--
-- Generated bindings for @MPSVector@.
module ObjC.MetalPerformanceShaders.MPSVector
  ( MPSVector
  , IsMPSVector(..)
  , initWithBuffer_descriptor
  , initWithBuffer_offset_descriptor
  , initWithDevice_descriptor
  , init_
  , synchronizeOnCommandBuffer
  , resourceSize
  , device
  , length_
  , vectors
  , dataType
  , vectorBytes
  , offset
  , data_
  , dataSelector
  , dataTypeSelector
  , deviceSelector
  , initSelector
  , initWithBuffer_descriptorSelector
  , initWithBuffer_offset_descriptorSelector
  , initWithDevice_descriptorSelector
  , lengthSelector
  , offsetSelector
  , resourceSizeSelector
  , synchronizeOnCommandBufferSelector
  , vectorBytesSelector
  , vectorsSelector

  -- * Enum types
  , MPSDataType(MPSDataType)
  , pattern MPSDataTypeInvalid
  , pattern MPSDataTypeFloatBit
  , pattern MPSDataTypeFloat32
  , pattern MPSDataTypeFloat16
  , pattern MPSDataTypeComplexBit
  , pattern MPSDataTypeComplexFloat32
  , pattern MPSDataTypeComplexFloat16
  , pattern MPSDataTypeSignedBit
  , pattern MPSDataTypeIntBit
  , pattern MPSDataTypeInt2
  , pattern MPSDataTypeInt4
  , pattern MPSDataTypeInt8
  , pattern MPSDataTypeInt16
  , pattern MPSDataTypeInt32
  , pattern MPSDataTypeInt64
  , pattern MPSDataTypeUInt2
  , pattern MPSDataTypeUInt4
  , pattern MPSDataTypeUInt8
  , pattern MPSDataTypeUInt16
  , pattern MPSDataTypeUInt32
  , pattern MPSDataTypeUInt64
  , pattern MPSDataTypeAlternateEncodingBit
  , pattern MPSDataTypeBool
  , pattern MPSDataTypeBFloat16
  , pattern MPSDataTypeNormalizedBit
  , pattern MPSDataTypeUnorm1
  , pattern MPSDataTypeUnorm8

  ) where

import Foreign.Ptr (Ptr, FunPtr)
import Foreign.C.Types

import ObjC.Runtime.Types
import ObjC.Runtime.Message (sendMessage, sendOwnedMessage, sendClassMessage, sendOwnedClassMessage)
import ObjC.Runtime.Selector (mkSelector)
import ObjC.Runtime.Class (getRequiredClass)

import ObjC.MetalPerformanceShaders.Internal.Classes
import ObjC.MetalPerformanceShaders.Internal.Enums
import ObjC.Foundation.Internal.Classes

-- | Initialize a MPSVector object with a MTLBuffer.
--
-- @buffer@ — The MTLBuffer object which contains the data to use for the                              MPSVector. May not be NULL.
--
-- @descriptor@ — The MPSVectorDescriptor. May not be NULL.
--
-- Returns: A valid MPSVector object or nil, if failure.
--
-- This function returns a MPSVector object which uses the supplied MTLBuffer.  The              length, number of vectors, and stride between vectors are specified by the              MPSVectorDescriptor object.
--
-- The provided MTLBuffer must have enough storage to hold
--
-- (descriptor.vectors-1) * descriptor.vectorBytes +                   descriptor.length * (element size) bytes.
--
-- ObjC selector: @- initWithBuffer:descriptor:@
initWithBuffer_descriptor :: (IsMPSVector mpsVector, IsMPSVectorDescriptor descriptor) => mpsVector -> RawId -> descriptor -> IO (Id MPSVector)
initWithBuffer_descriptor mpsVector buffer descriptor =
  sendOwnedMessage mpsVector initWithBuffer_descriptorSelector buffer (toMPSVectorDescriptor descriptor)

-- | Initialize a MPSVector object with a MTLBuffer and an offset.
--
-- @buffer@ — The MTLBuffer containing the data.
--
-- @offset@ — The offset, in bytes, into the buffer at which data begins.
--
-- @descriptor@ — The MPSVectorDescriptor.
--
-- ObjC selector: @- initWithBuffer:offset:descriptor:@
initWithBuffer_offset_descriptor :: (IsMPSVector mpsVector, IsMPSVectorDescriptor descriptor) => mpsVector -> RawId -> CULong -> descriptor -> IO (Id MPSVector)
initWithBuffer_offset_descriptor mpsVector buffer offset descriptor =
  sendOwnedMessage mpsVector initWithBuffer_offset_descriptorSelector buffer offset (toMPSVectorDescriptor descriptor)

-- | Initialize a lazily backed MPSVector object with a descriptor
--
-- @device@ — The device with which it will be used
--
-- @descriptor@ — The shape and style of the matrix
--
-- Returns: A valid MPSVector object or nil
--
-- The vector object will be created, but the storage to hold the              vector data will only be allocated when it is needed, typically              when the data property is invoked.  In conjunction              with -resourceSize, this will allow you to estimate storage needs              without actually creating the backing store for the vector.
--
-- ObjC selector: @- initWithDevice:descriptor:@
initWithDevice_descriptor :: (IsMPSVector mpsVector, IsMPSVectorDescriptor descriptor) => mpsVector -> RawId -> descriptor -> IO (Id MPSVector)
initWithDevice_descriptor mpsVector device descriptor =
  sendOwnedMessage mpsVector initWithDevice_descriptorSelector device (toMPSVectorDescriptor descriptor)

-- | @- init@
init_ :: IsMPSVector mpsVector => mpsVector -> IO (Id MPSVector)
init_ mpsVector =
  sendOwnedMessage mpsVector initSelector

-- | Flush the underlying MTLBuffer from the device's caches, and invalidate any CPU caches if needed.
--
-- This will call [id <MTLBlitEncoder> synchronizeResource: ] on the vector's MTLBuffer, if any.              This is necessary for all MTLStorageModeManaged resources. For other resources, including temporary              resources (these are all MTLStorageModePrivate), and buffers that have not yet been allocated, nothing is done.              It is more efficient to use this method than to attempt to do this yourself with the data property.
--
-- @commandBuffer@ — The commandbuffer on which to synchronize
--
-- ObjC selector: @- synchronizeOnCommandBuffer:@
synchronizeOnCommandBuffer :: IsMPSVector mpsVector => mpsVector -> RawId -> IO ()
synchronizeOnCommandBuffer mpsVector commandBuffer =
  sendMessage mpsVector synchronizeOnCommandBufferSelector commandBuffer

-- | Get the number of bytes used to allocate underyling MTLResources
--
-- This is the size of the backing store of underlying MTLResources.                  It does not include all storage used by the object, for example                  the storage used to hold the MPSVector instantiation and MTLBuffer                  is not included. It only measures the size of the allocation used                  to hold the vector data in the buffer. This value is subject to                  change between different devices and operating systems.
--
-- Except when -initWithBuffer:descriptor: is used, most MPSVectors are allocated                  without a backing store. The backing store is allocated lazily when                  it is needed, typically when the .texture property is called.                  Consequently, in most cases, it should be inexpensive to make                  a MPSMatrix to see how much memory it will need, and release it                  if it is too large.
--
-- This method may fail in certain circumstances, such as when the                  MPSMatrix is created with -initWithBuffer:descriptor:. In                  such cases, 0 will be returned.
--
-- ObjC selector: @- resourceSize@
resourceSize :: IsMPSVector mpsVector => mpsVector -> IO CULong
resourceSize mpsVector =
  sendMessage mpsVector resourceSizeSelector

-- | device
--
-- The device on which the MPSVector will be used.
--
-- ObjC selector: @- device@
device :: IsMPSVector mpsVector => mpsVector -> IO RawId
device mpsVector =
  sendMessage mpsVector deviceSelector

-- | length
--
-- The number of elements in the vector.
--
-- ObjC selector: @- length@
length_ :: IsMPSVector mpsVector => mpsVector -> IO CULong
length_ mpsVector =
  sendMessage mpsVector lengthSelector

-- | vectors
--
-- The number of vectors in the MPSVector.
--
-- ObjC selector: @- vectors@
vectors :: IsMPSVector mpsVector => mpsVector -> IO CULong
vectors mpsVector =
  sendMessage mpsVector vectorsSelector

-- | dataType
--
-- The type of the MPSVector data.
--
-- ObjC selector: @- dataType@
dataType :: IsMPSVector mpsVector => mpsVector -> IO MPSDataType
dataType mpsVector =
  sendMessage mpsVector dataTypeSelector

-- | vectorBytes
--
-- The stride, in bytes, between corresponding elements of              consecutive vectors.
--
-- ObjC selector: @- vectorBytes@
vectorBytes :: IsMPSVector mpsVector => mpsVector -> IO CULong
vectorBytes mpsVector =
  sendMessage mpsVector vectorBytesSelector

-- | offset
--
-- Byte-offset to the buffer where the vector data begins - see initWithBuffer: offset: descriptor: .
--
-- ObjC selector: @- offset@
offset :: IsMPSVector mpsVector => mpsVector -> IO CULong
offset mpsVector =
  sendMessage mpsVector offsetSelector

-- | data
--
-- An MTLBuffer to store the data.
--
-- ObjC selector: @- data@
data_ :: IsMPSVector mpsVector => mpsVector -> IO RawId
data_ mpsVector =
  sendMessage mpsVector dataSelector

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @initWithBuffer:descriptor:@
initWithBuffer_descriptorSelector :: Selector '[RawId, Id MPSVectorDescriptor] (Id MPSVector)
initWithBuffer_descriptorSelector = mkSelector "initWithBuffer:descriptor:"

-- | @Selector@ for @initWithBuffer:offset:descriptor:@
initWithBuffer_offset_descriptorSelector :: Selector '[RawId, CULong, Id MPSVectorDescriptor] (Id MPSVector)
initWithBuffer_offset_descriptorSelector = mkSelector "initWithBuffer:offset:descriptor:"

-- | @Selector@ for @initWithDevice:descriptor:@
initWithDevice_descriptorSelector :: Selector '[RawId, Id MPSVectorDescriptor] (Id MPSVector)
initWithDevice_descriptorSelector = mkSelector "initWithDevice:descriptor:"

-- | @Selector@ for @init@
initSelector :: Selector '[] (Id MPSVector)
initSelector = mkSelector "init"

-- | @Selector@ for @synchronizeOnCommandBuffer:@
synchronizeOnCommandBufferSelector :: Selector '[RawId] ()
synchronizeOnCommandBufferSelector = mkSelector "synchronizeOnCommandBuffer:"

-- | @Selector@ for @resourceSize@
resourceSizeSelector :: Selector '[] CULong
resourceSizeSelector = mkSelector "resourceSize"

-- | @Selector@ for @device@
deviceSelector :: Selector '[] RawId
deviceSelector = mkSelector "device"

-- | @Selector@ for @length@
lengthSelector :: Selector '[] CULong
lengthSelector = mkSelector "length"

-- | @Selector@ for @vectors@
vectorsSelector :: Selector '[] CULong
vectorsSelector = mkSelector "vectors"

-- | @Selector@ for @dataType@
dataTypeSelector :: Selector '[] MPSDataType
dataTypeSelector = mkSelector "dataType"

-- | @Selector@ for @vectorBytes@
vectorBytesSelector :: Selector '[] CULong
vectorBytesSelector = mkSelector "vectorBytes"

-- | @Selector@ for @offset@
offsetSelector :: Selector '[] CULong
offsetSelector = mkSelector "offset"

-- | @Selector@ for @data@
dataSelector :: Selector '[] RawId
dataSelector = mkSelector "data"

