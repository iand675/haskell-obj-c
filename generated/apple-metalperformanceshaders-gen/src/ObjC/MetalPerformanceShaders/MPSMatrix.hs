{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | MPSMatrix
--
-- This depends on Metal.framework
--
-- A MPSMatrix object describes a set of 2-dimensional arrays of data and provides storage              for its values.  MPSMatrix objects serve as inputs and outputs of MPSMatrixKernel              objects.
--
-- Implementation note:              A MPSMatrix object maintains its internal storage using a MTLBuffer object and thus              the same rules for maintaining coherency of a MTLBuffer's data between CPU memory and GPU              memory apply to a MPSMatrix.  An MPSMatrix object's data refers to an array of matrices.              Data is assumed to be ordered by matrix first, followed by row, followed by column.
--
-- For example, index [i,j] of the k'th matrix of an MPSMatrix is located at byte offset:                       k * matrixBytes + i * rowBytes + j * sizeof(dataType)
--
-- Where matrixBytes is a multiple of rowBytes at least equal to rows * rowBytes.
--
-- Generated bindings for @MPSMatrix@.
module ObjC.MetalPerformanceShaders.MPSMatrix
  ( MPSMatrix
  , IsMPSMatrix(..)
  , initWithBuffer_descriptor
  , initWithBuffer_offset_descriptor
  , initWithDevice_descriptor
  , init_
  , synchronizeOnCommandBuffer
  , resourceSize
  , device
  , rows
  , columns
  , matrices
  , dataType
  , rowBytes
  , matrixBytes
  , offset
  , data_
  , columnsSelector
  , dataSelector
  , dataTypeSelector
  , deviceSelector
  , initSelector
  , initWithBuffer_descriptorSelector
  , initWithBuffer_offset_descriptorSelector
  , initWithDevice_descriptorSelector
  , matricesSelector
  , matrixBytesSelector
  , offsetSelector
  , resourceSizeSelector
  , rowBytesSelector
  , rowsSelector
  , synchronizeOnCommandBufferSelector

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

-- | Initialize a MPSMatrix object with a MTLBuffer.
--
-- @buffer@ — The MTLBuffer object which contains the data to use for the                              MPSMatrix. May not be NULL.
--
-- @descriptor@ — The MPSMatrixDescriptor. May not be NULL.
--
-- Returns: A valid MPSMatrix object or nil, if failure.
--
-- This function returns a MPSMatrix object which uses the supplied MTLBuffer.  The              dimensions and stride of the matrix are specified by the MPSMatrixDescriptor object.
--
-- The provided MTLBuffer must have enough storage to hold
--
-- (descriptor.matrices-1) * descriptor.matrixBytes +                  (descriptor.rows-1) * descriptor.rowBytes +                   descriptor.columns * (element size) bytes.
--
-- ObjC selector: @- initWithBuffer:descriptor:@
initWithBuffer_descriptor :: (IsMPSMatrix mpsMatrix, IsMPSMatrixDescriptor descriptor) => mpsMatrix -> RawId -> descriptor -> IO (Id MPSMatrix)
initWithBuffer_descriptor mpsMatrix buffer descriptor =
  sendOwnedMessage mpsMatrix initWithBuffer_descriptorSelector buffer (toMPSMatrixDescriptor descriptor)

-- | Initialize a MPSMatrix object with a MTLBuffer at a given offset.
--
-- @buffer@ — The MTLBuffer object which contains the data to use for the                          MPSMatrix.  May not be NULL.
--
-- @offset@ — The offset, in bytes, into the buffer at which the data begins.
--
-- @descriptor@ — The MPSMatrixDescriptor describing the shape of the matrix.
--
-- ObjC selector: @- initWithBuffer:offset:descriptor:@
initWithBuffer_offset_descriptor :: (IsMPSMatrix mpsMatrix, IsMPSMatrixDescriptor descriptor) => mpsMatrix -> RawId -> CULong -> descriptor -> IO (Id MPSMatrix)
initWithBuffer_offset_descriptor mpsMatrix buffer offset descriptor =
  sendOwnedMessage mpsMatrix initWithBuffer_offset_descriptorSelector buffer offset (toMPSMatrixDescriptor descriptor)

-- | Initialize a MPSMatrix object with a descriptor. Allocate the buffer.
--
-- @device@ — The device with which it will be used
--
-- @descriptor@ — The shape and style of the matrix
--
-- Returns: A valid MPSMatrix object or nil
--
-- The matrix object will be created, but the storage to hold the              matrix data will only be allocated when it is needed, typically              when the data property is invoked.  In conjunction              with -resourceSize, this will allow you to estimate storage needs              without actually creating the backing store for the matrix.
--
-- ObjC selector: @- initWithDevice:descriptor:@
initWithDevice_descriptor :: (IsMPSMatrix mpsMatrix, IsMPSMatrixDescriptor descriptor) => mpsMatrix -> RawId -> descriptor -> IO (Id MPSMatrix)
initWithDevice_descriptor mpsMatrix device descriptor =
  sendOwnedMessage mpsMatrix initWithDevice_descriptorSelector device (toMPSMatrixDescriptor descriptor)

-- | @- init@
init_ :: IsMPSMatrix mpsMatrix => mpsMatrix -> IO (Id MPSMatrix)
init_ mpsMatrix =
  sendOwnedMessage mpsMatrix initSelector

-- | Flush the underlying MTLBuffer from the device's caches, and invalidate any CPU caches if needed.
--
-- This will call [id <MTLBlitEncoder> synchronizeResource: ] on the matrix's MTLBuffer, if any.              This is necessary for all MTLStorageModeManaged resources. For other resources, including temporary              resources (these are all MTLStorageModePrivate), and buffers that have not yet been allocated, nothing is done.              It is more efficient to use this method than to attempt to do this yourself with the data property.
--
-- @commandBuffer@ — The commandbuffer on which to synchronize
--
-- ObjC selector: @- synchronizeOnCommandBuffer:@
synchronizeOnCommandBuffer :: IsMPSMatrix mpsMatrix => mpsMatrix -> RawId -> IO ()
synchronizeOnCommandBuffer mpsMatrix commandBuffer =
  sendMessage mpsMatrix synchronizeOnCommandBufferSelector commandBuffer

-- | Get the number of bytes used to allocate underyling MTLResources
--
-- This is the size of the backing store of underlying MTLResources.                  It does not include all storage used by the object, for example                  the storage used to hold the MPSMatrix instantiation and MTLBuffer                  is not included. It only measures the size of the allocation used                  to hold the matrix data in the buffer. This value is subject to                  change between different devices and operating systems.
--
-- Except when -initWithBuffer:descriptor: is used, most MPSMatrixes are allocated                  without a backing store. The backing store is allocated lazily when                  it is needed, typically when the .texture property is called.                  Consequently, in most cases, it should be inexpensive to make                  a MPSImage to see how much memory it will need, and release it                  if it is too large.
--
-- This method may fail in certain circumstances, such as when the                  MPSImage is created with -initWithTexture:featureChannels:. In                  such cases, 0 will be returned.
--
-- ObjC selector: @- resourceSize@
resourceSize :: IsMPSMatrix mpsMatrix => mpsMatrix -> IO CULong
resourceSize mpsMatrix =
  sendMessage mpsMatrix resourceSizeSelector

-- | device
--
-- The device on which the MPSMatrix will be used.
--
-- ObjC selector: @- device@
device :: IsMPSMatrix mpsMatrix => mpsMatrix -> IO RawId
device mpsMatrix =
  sendMessage mpsMatrix deviceSelector

-- | rows
--
-- The number of rows in a matrix in the MPSMatrix.
--
-- ObjC selector: @- rows@
rows :: IsMPSMatrix mpsMatrix => mpsMatrix -> IO CULong
rows mpsMatrix =
  sendMessage mpsMatrix rowsSelector

-- | columns
--
-- The number of columns in a matrix in the MPSMatrix.
--
-- ObjC selector: @- columns@
columns :: IsMPSMatrix mpsMatrix => mpsMatrix -> IO CULong
columns mpsMatrix =
  sendMessage mpsMatrix columnsSelector

-- | matrices
--
-- The number of matrices in the MPSMatrix.
--
-- ObjC selector: @- matrices@
matrices :: IsMPSMatrix mpsMatrix => mpsMatrix -> IO CULong
matrices mpsMatrix =
  sendMessage mpsMatrix matricesSelector

-- | dataType
--
-- The type of the MPSMatrix data.
--
-- ObjC selector: @- dataType@
dataType :: IsMPSMatrix mpsMatrix => mpsMatrix -> IO MPSDataType
dataType mpsMatrix =
  sendMessage mpsMatrix dataTypeSelector

-- | rowBytes
--
-- The stride, in bytes, between corresponding elements of              consecutive rows.
--
-- ObjC selector: @- rowBytes@
rowBytes :: IsMPSMatrix mpsMatrix => mpsMatrix -> IO CULong
rowBytes mpsMatrix =
  sendMessage mpsMatrix rowBytesSelector

-- | matrixBytes
--
-- The stride, in bytes, between corresponding elements of              consecutive matrices.
--
-- ObjC selector: @- matrixBytes@
matrixBytes :: IsMPSMatrix mpsMatrix => mpsMatrix -> IO CULong
matrixBytes mpsMatrix =
  sendMessage mpsMatrix matrixBytesSelector

-- | offset
--
-- Byte-offset to the buffer where the matrix data begins - see initWithBuffer: offset: descriptor: .
--
-- ObjC selector: @- offset@
offset :: IsMPSMatrix mpsMatrix => mpsMatrix -> IO CULong
offset mpsMatrix =
  sendMessage mpsMatrix offsetSelector

-- | data
--
-- An MTLBuffer to store the data.
--
-- ObjC selector: @- data@
data_ :: IsMPSMatrix mpsMatrix => mpsMatrix -> IO RawId
data_ mpsMatrix =
  sendMessage mpsMatrix dataSelector

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @initWithBuffer:descriptor:@
initWithBuffer_descriptorSelector :: Selector '[RawId, Id MPSMatrixDescriptor] (Id MPSMatrix)
initWithBuffer_descriptorSelector = mkSelector "initWithBuffer:descriptor:"

-- | @Selector@ for @initWithBuffer:offset:descriptor:@
initWithBuffer_offset_descriptorSelector :: Selector '[RawId, CULong, Id MPSMatrixDescriptor] (Id MPSMatrix)
initWithBuffer_offset_descriptorSelector = mkSelector "initWithBuffer:offset:descriptor:"

-- | @Selector@ for @initWithDevice:descriptor:@
initWithDevice_descriptorSelector :: Selector '[RawId, Id MPSMatrixDescriptor] (Id MPSMatrix)
initWithDevice_descriptorSelector = mkSelector "initWithDevice:descriptor:"

-- | @Selector@ for @init@
initSelector :: Selector '[] (Id MPSMatrix)
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

-- | @Selector@ for @rows@
rowsSelector :: Selector '[] CULong
rowsSelector = mkSelector "rows"

-- | @Selector@ for @columns@
columnsSelector :: Selector '[] CULong
columnsSelector = mkSelector "columns"

-- | @Selector@ for @matrices@
matricesSelector :: Selector '[] CULong
matricesSelector = mkSelector "matrices"

-- | @Selector@ for @dataType@
dataTypeSelector :: Selector '[] MPSDataType
dataTypeSelector = mkSelector "dataType"

-- | @Selector@ for @rowBytes@
rowBytesSelector :: Selector '[] CULong
rowBytesSelector = mkSelector "rowBytes"

-- | @Selector@ for @matrixBytes@
matrixBytesSelector :: Selector '[] CULong
matrixBytesSelector = mkSelector "matrixBytes"

-- | @Selector@ for @offset@
offsetSelector :: Selector '[] CULong
offsetSelector = mkSelector "offset"

-- | @Selector@ for @data@
dataSelector :: Selector '[] RawId
dataSelector = mkSelector "data"

