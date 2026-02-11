{-# LANGUAGE PatternSynonyms #-}
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
  , initWithBuffer_descriptorSelector
  , initWithBuffer_offset_descriptorSelector
  , initWithDevice_descriptorSelector
  , initSelector
  , synchronizeOnCommandBufferSelector
  , resourceSizeSelector
  , deviceSelector
  , lengthSelector
  , vectorsSelector
  , dataTypeSelector
  , vectorBytesSelector
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
initWithBuffer_descriptor mpsVector  buffer descriptor =
  withObjCPtr descriptor $ \raw_descriptor ->
      sendMsg mpsVector (mkSelector "initWithBuffer:descriptor:") (retPtr retVoid) [argPtr (castPtr (unRawId buffer) :: Ptr ()), argPtr (castPtr raw_descriptor :: Ptr ())] >>= ownedObject . castPtr

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
initWithBuffer_offset_descriptor mpsVector  buffer offset descriptor =
  withObjCPtr descriptor $ \raw_descriptor ->
      sendMsg mpsVector (mkSelector "initWithBuffer:offset:descriptor:") (retPtr retVoid) [argPtr (castPtr (unRawId buffer) :: Ptr ()), argCULong offset, argPtr (castPtr raw_descriptor :: Ptr ())] >>= ownedObject . castPtr

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
initWithDevice_descriptor mpsVector  device descriptor =
  withObjCPtr descriptor $ \raw_descriptor ->
      sendMsg mpsVector (mkSelector "initWithDevice:descriptor:") (retPtr retVoid) [argPtr (castPtr (unRawId device) :: Ptr ()), argPtr (castPtr raw_descriptor :: Ptr ())] >>= ownedObject . castPtr

-- | @- init@
init_ :: IsMPSVector mpsVector => mpsVector -> IO (Id MPSVector)
init_ mpsVector  =
    sendMsg mpsVector (mkSelector "init") (retPtr retVoid) [] >>= ownedObject . castPtr

-- | Flush the underlying MTLBuffer from the device's caches, and invalidate any CPU caches if needed.
--
-- This will call [id <MTLBlitEncoder> synchronizeResource: ] on the vector's MTLBuffer, if any.              This is necessary for all MTLStorageModeManaged resources. For other resources, including temporary              resources (these are all MTLStorageModePrivate), and buffers that have not yet been allocated, nothing is done.              It is more efficient to use this method than to attempt to do this yourself with the data property.
--
-- @commandBuffer@ — The commandbuffer on which to synchronize
--
-- ObjC selector: @- synchronizeOnCommandBuffer:@
synchronizeOnCommandBuffer :: IsMPSVector mpsVector => mpsVector -> RawId -> IO ()
synchronizeOnCommandBuffer mpsVector  commandBuffer =
    sendMsg mpsVector (mkSelector "synchronizeOnCommandBuffer:") retVoid [argPtr (castPtr (unRawId commandBuffer) :: Ptr ())]

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
resourceSize mpsVector  =
    sendMsg mpsVector (mkSelector "resourceSize") retCULong []

-- | device
--
-- The device on which the MPSVector will be used.
--
-- ObjC selector: @- device@
device :: IsMPSVector mpsVector => mpsVector -> IO RawId
device mpsVector  =
    fmap (RawId . castPtr) $ sendMsg mpsVector (mkSelector "device") (retPtr retVoid) []

-- | length
--
-- The number of elements in the vector.
--
-- ObjC selector: @- length@
length_ :: IsMPSVector mpsVector => mpsVector -> IO CULong
length_ mpsVector  =
    sendMsg mpsVector (mkSelector "length") retCULong []

-- | vectors
--
-- The number of vectors in the MPSVector.
--
-- ObjC selector: @- vectors@
vectors :: IsMPSVector mpsVector => mpsVector -> IO CULong
vectors mpsVector  =
    sendMsg mpsVector (mkSelector "vectors") retCULong []

-- | dataType
--
-- The type of the MPSVector data.
--
-- ObjC selector: @- dataType@
dataType :: IsMPSVector mpsVector => mpsVector -> IO MPSDataType
dataType mpsVector  =
    fmap (coerce :: CUInt -> MPSDataType) $ sendMsg mpsVector (mkSelector "dataType") retCUInt []

-- | vectorBytes
--
-- The stride, in bytes, between corresponding elements of              consecutive vectors.
--
-- ObjC selector: @- vectorBytes@
vectorBytes :: IsMPSVector mpsVector => mpsVector -> IO CULong
vectorBytes mpsVector  =
    sendMsg mpsVector (mkSelector "vectorBytes") retCULong []

-- | offset
--
-- Byte-offset to the buffer where the vector data begins - see initWithBuffer: offset: descriptor: .
--
-- ObjC selector: @- offset@
offset :: IsMPSVector mpsVector => mpsVector -> IO CULong
offset mpsVector  =
    sendMsg mpsVector (mkSelector "offset") retCULong []

-- | data
--
-- An MTLBuffer to store the data.
--
-- ObjC selector: @- data@
data_ :: IsMPSVector mpsVector => mpsVector -> IO RawId
data_ mpsVector  =
    fmap (RawId . castPtr) $ sendMsg mpsVector (mkSelector "data") (retPtr retVoid) []

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

-- | @Selector@ for @length@
lengthSelector :: Selector
lengthSelector = mkSelector "length"

-- | @Selector@ for @vectors@
vectorsSelector :: Selector
vectorsSelector = mkSelector "vectors"

-- | @Selector@ for @dataType@
dataTypeSelector :: Selector
dataTypeSelector = mkSelector "dataType"

-- | @Selector@ for @vectorBytes@
vectorBytesSelector :: Selector
vectorBytesSelector = mkSelector "vectorBytes"

-- | @Selector@ for @offset@
offsetSelector :: Selector
offsetSelector = mkSelector "offset"

-- | @Selector@ for @data@
dataSelector :: Selector
dataSelector = mkSelector "data"

