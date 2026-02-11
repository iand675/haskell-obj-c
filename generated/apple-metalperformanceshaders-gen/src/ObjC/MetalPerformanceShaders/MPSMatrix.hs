{-# LANGUAGE PatternSynonyms #-}
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
  , initWithBuffer_descriptorSelector
  , initWithBuffer_offset_descriptorSelector
  , initWithDevice_descriptorSelector
  , initSelector
  , synchronizeOnCommandBufferSelector
  , resourceSizeSelector
  , deviceSelector
  , rowsSelector
  , columnsSelector
  , matricesSelector
  , dataTypeSelector
  , rowBytesSelector
  , matrixBytesSelector
  , offsetSelector
  , dataSelector

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
initWithBuffer_descriptor mpsMatrix  buffer descriptor =
  withObjCPtr descriptor $ \raw_descriptor ->
      sendMsg mpsMatrix (mkSelector "initWithBuffer:descriptor:") (retPtr retVoid) [argPtr (castPtr (unRawId buffer) :: Ptr ()), argPtr (castPtr raw_descriptor :: Ptr ())] >>= ownedObject . castPtr

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
initWithBuffer_offset_descriptor mpsMatrix  buffer offset descriptor =
  withObjCPtr descriptor $ \raw_descriptor ->
      sendMsg mpsMatrix (mkSelector "initWithBuffer:offset:descriptor:") (retPtr retVoid) [argPtr (castPtr (unRawId buffer) :: Ptr ()), argCULong offset, argPtr (castPtr raw_descriptor :: Ptr ())] >>= ownedObject . castPtr

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
initWithDevice_descriptor mpsMatrix  device descriptor =
  withObjCPtr descriptor $ \raw_descriptor ->
      sendMsg mpsMatrix (mkSelector "initWithDevice:descriptor:") (retPtr retVoid) [argPtr (castPtr (unRawId device) :: Ptr ()), argPtr (castPtr raw_descriptor :: Ptr ())] >>= ownedObject . castPtr

-- | @- init@
init_ :: IsMPSMatrix mpsMatrix => mpsMatrix -> IO (Id MPSMatrix)
init_ mpsMatrix  =
    sendMsg mpsMatrix (mkSelector "init") (retPtr retVoid) [] >>= ownedObject . castPtr

-- | Flush the underlying MTLBuffer from the device's caches, and invalidate any CPU caches if needed.
--
-- This will call [id <MTLBlitEncoder> synchronizeResource: ] on the matrix's MTLBuffer, if any.              This is necessary for all MTLStorageModeManaged resources. For other resources, including temporary              resources (these are all MTLStorageModePrivate), and buffers that have not yet been allocated, nothing is done.              It is more efficient to use this method than to attempt to do this yourself with the data property.
--
-- @commandBuffer@ — The commandbuffer on which to synchronize
--
-- ObjC selector: @- synchronizeOnCommandBuffer:@
synchronizeOnCommandBuffer :: IsMPSMatrix mpsMatrix => mpsMatrix -> RawId -> IO ()
synchronizeOnCommandBuffer mpsMatrix  commandBuffer =
    sendMsg mpsMatrix (mkSelector "synchronizeOnCommandBuffer:") retVoid [argPtr (castPtr (unRawId commandBuffer) :: Ptr ())]

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
resourceSize mpsMatrix  =
    sendMsg mpsMatrix (mkSelector "resourceSize") retCULong []

-- | device
--
-- The device on which the MPSMatrix will be used.
--
-- ObjC selector: @- device@
device :: IsMPSMatrix mpsMatrix => mpsMatrix -> IO RawId
device mpsMatrix  =
    fmap (RawId . castPtr) $ sendMsg mpsMatrix (mkSelector "device") (retPtr retVoid) []

-- | rows
--
-- The number of rows in a matrix in the MPSMatrix.
--
-- ObjC selector: @- rows@
rows :: IsMPSMatrix mpsMatrix => mpsMatrix -> IO CULong
rows mpsMatrix  =
    sendMsg mpsMatrix (mkSelector "rows") retCULong []

-- | columns
--
-- The number of columns in a matrix in the MPSMatrix.
--
-- ObjC selector: @- columns@
columns :: IsMPSMatrix mpsMatrix => mpsMatrix -> IO CULong
columns mpsMatrix  =
    sendMsg mpsMatrix (mkSelector "columns") retCULong []

-- | matrices
--
-- The number of matrices in the MPSMatrix.
--
-- ObjC selector: @- matrices@
matrices :: IsMPSMatrix mpsMatrix => mpsMatrix -> IO CULong
matrices mpsMatrix  =
    sendMsg mpsMatrix (mkSelector "matrices") retCULong []

-- | dataType
--
-- The type of the MPSMatrix data.
--
-- ObjC selector: @- dataType@
dataType :: IsMPSMatrix mpsMatrix => mpsMatrix -> IO MPSDataType
dataType mpsMatrix  =
    fmap (coerce :: CUInt -> MPSDataType) $ sendMsg mpsMatrix (mkSelector "dataType") retCUInt []

-- | rowBytes
--
-- The stride, in bytes, between corresponding elements of              consecutive rows.
--
-- ObjC selector: @- rowBytes@
rowBytes :: IsMPSMatrix mpsMatrix => mpsMatrix -> IO CULong
rowBytes mpsMatrix  =
    sendMsg mpsMatrix (mkSelector "rowBytes") retCULong []

-- | matrixBytes
--
-- The stride, in bytes, between corresponding elements of              consecutive matrices.
--
-- ObjC selector: @- matrixBytes@
matrixBytes :: IsMPSMatrix mpsMatrix => mpsMatrix -> IO CULong
matrixBytes mpsMatrix  =
    sendMsg mpsMatrix (mkSelector "matrixBytes") retCULong []

-- | offset
--
-- Byte-offset to the buffer where the matrix data begins - see initWithBuffer: offset: descriptor: .
--
-- ObjC selector: @- offset@
offset :: IsMPSMatrix mpsMatrix => mpsMatrix -> IO CULong
offset mpsMatrix  =
    sendMsg mpsMatrix (mkSelector "offset") retCULong []

-- | data
--
-- An MTLBuffer to store the data.
--
-- ObjC selector: @- data@
data_ :: IsMPSMatrix mpsMatrix => mpsMatrix -> IO RawId
data_ mpsMatrix  =
    fmap (RawId . castPtr) $ sendMsg mpsMatrix (mkSelector "data") (retPtr retVoid) []

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @initWithBuffer:descriptor:@
initWithBuffer_descriptorSelector :: Selector
initWithBuffer_descriptorSelector = mkSelector "initWithBuffer:descriptor:"

-- | @Selector@ for @initWithBuffer:offset:descriptor:@
initWithBuffer_offset_descriptorSelector :: Selector
initWithBuffer_offset_descriptorSelector = mkSelector "initWithBuffer:offset:descriptor:"

-- | @Selector@ for @initWithDevice:descriptor:@
initWithDevice_descriptorSelector :: Selector
initWithDevice_descriptorSelector = mkSelector "initWithDevice:descriptor:"

-- | @Selector@ for @init@
initSelector :: Selector
initSelector = mkSelector "init"

-- | @Selector@ for @synchronizeOnCommandBuffer:@
synchronizeOnCommandBufferSelector :: Selector
synchronizeOnCommandBufferSelector = mkSelector "synchronizeOnCommandBuffer:"

-- | @Selector@ for @resourceSize@
resourceSizeSelector :: Selector
resourceSizeSelector = mkSelector "resourceSize"

-- | @Selector@ for @device@
deviceSelector :: Selector
deviceSelector = mkSelector "device"

-- | @Selector@ for @rows@
rowsSelector :: Selector
rowsSelector = mkSelector "rows"

-- | @Selector@ for @columns@
columnsSelector :: Selector
columnsSelector = mkSelector "columns"

-- | @Selector@ for @matrices@
matricesSelector :: Selector
matricesSelector = mkSelector "matrices"

-- | @Selector@ for @dataType@
dataTypeSelector :: Selector
dataTypeSelector = mkSelector "dataType"

-- | @Selector@ for @rowBytes@
rowBytesSelector :: Selector
rowBytesSelector = mkSelector "rowBytes"

-- | @Selector@ for @matrixBytes@
matrixBytesSelector :: Selector
matrixBytesSelector = mkSelector "matrixBytes"

-- | @Selector@ for @offset@
offsetSelector :: Selector
offsetSelector = mkSelector "offset"

-- | @Selector@ for @data@
dataSelector :: Selector
dataSelector = mkSelector "data"

