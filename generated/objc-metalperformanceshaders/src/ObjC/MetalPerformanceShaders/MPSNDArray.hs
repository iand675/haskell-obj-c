{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | MPSNDArray
--
-- A MPSNDArray object is a MTLBuffer based storage container for multi-dimensional data.
--
-- Operations on MPSNDArrays will commonly implicitly reshape the multidimensional              structure into a 2-dimensional structure by reinterpreting higher dimensions as a single dimensional              array of matrix rows. For example a [a, b, c, d] NDArray passed to a matrix multiplication may              be implicitly reinterpreted as a [a*b*c, d] matrix and a 2D matrix multiplication performed.              In practice, the major row (the dimension in which successive elements appear adjacent to one              another in memory) is the 0th dimension (represented as 'd' in the above example).  It has both a              dimension size indicating the number of elements and a storage size which may be slightly bigger              to allow for performance improvement arising from better data alignment in memory.  In principle,              the rowBytes may also be used to create a 0th-dimension slice out of a larger array stored in the              underlying MTLBuffer.
--
-- MPS will automatically manage the storage size of the major row ("rowBytes") though you may              set it in the descriptor if you have a need to do so. Generally, it should be at least a multiple              of 16 bytes.   Dimensions after the 0th are a densely packed array of rows of size rowBytes.              Thus, the 1st dimension is an array of rows. The 2nd dimension is an array of arrays of rows with              identical size, and so forth.  When the reduction to 2 dimensions is done, no data is moved. MPS              just reinterprets a higher order N-1 dimensions of matrix rows as a single large 1-dimensional              array of rows.
--
-- It is a common desire to reorder the dimensions of NDArrays or define a subregion thereof. A transpose              or slice operation is performed by making a MPSNDArray view of the original. The dimensions to transpose              or slice are given by the descriptor for the new view. If both a transpose and slice operation are defined,              then the slice is performed first and the result of the slice is transposed. Because many MPS kernels can              operate on transposed data at speed, MPS will usually defer doing a physical transpose operation until later,              when it becomes clear that one is actually required. For this reason, conversions to formats that do not              support deferred transposes and slices such as MPSMatrix MPSVector view or using -exportWithCommandBuffer:              toBuffer:offset:rowStrides, may cause substantial new computation to be done and new memory to be allocated.              These should be avoided except when necessary.  As a general rule, transposes that do not involve the 0th              dimension should be able to be handled by nearly everything natively. MPSNDArrayMatrixMultiplication and reductions              can handle 0th dimension transposes. Other filters may insert a physical repacking operation. If you wish              to force a physical repacking use MPSAliasingStrategyShallNotAlias. To avoid confusion with aliased NDArrays              the parent property is provided.  MPSNDArrays that alias share a common ancestor.
--
-- Generated bindings for @MPSNDArray@.
module ObjC.MetalPerformanceShaders.MPSNDArray
  ( MPSNDArray
  , IsMPSNDArray(..)
  , defaultAllocator
  , lengthOfDimension
  , descriptor
  , init_
  , initWithDevice_descriptor
  , initWithDevice_scalar
  , initWithBuffer_offset_descriptor
  , userBuffer
  , resourceSize
  , arrayViewWithCommandBuffer_descriptor_aliasing
  , arrayViewWithDescriptor
  , arrayViewWithDimensionCount_dimensionSizes_strides
  , exportDataWithCommandBuffer_toBuffer_destinationDataType_offset_rowStrides
  , importDataWithCommandBuffer_fromBuffer_sourceDataType_offset_rowStrides
  , readBytes_strideBytes
  , writeBytes_strideBytes
  , synchronizeOnCommandBuffer
  , label
  , setLabel
  , dataType
  , dataTypeSize
  , numberOfDimensions
  , parent
  , defaultAllocatorSelector
  , lengthOfDimensionSelector
  , descriptorSelector
  , initSelector
  , initWithDevice_descriptorSelector
  , initWithDevice_scalarSelector
  , initWithBuffer_offset_descriptorSelector
  , userBufferSelector
  , resourceSizeSelector
  , arrayViewWithCommandBuffer_descriptor_aliasingSelector
  , arrayViewWithDescriptorSelector
  , arrayViewWithDimensionCount_dimensionSizes_stridesSelector
  , exportDataWithCommandBuffer_toBuffer_destinationDataType_offset_rowStridesSelector
  , importDataWithCommandBuffer_fromBuffer_sourceDataType_offset_rowStridesSelector
  , readBytes_strideBytesSelector
  , writeBytes_strideBytesSelector
  , synchronizeOnCommandBufferSelector
  , labelSelector
  , setLabelSelector
  , dataTypeSelector
  , dataTypeSizeSelector
  , numberOfDimensionsSelector
  , parentSelector

  -- * Enum types
  , MPSAliasingStrategy(MPSAliasingStrategy)
  , pattern MPSAliasingStrategyDefault
  , pattern MPSAliasingStrategyDontCare
  , pattern MPSAliasingStrategyShallAlias
  , pattern MPSAliasingStrategyShallNotAlias
  , pattern MPSAliasingStrategyAliasingReserved
  , pattern MPSAliasingStrategyPreferTemporaryMemory
  , pattern MPSAliasingStrategyPreferNonTemporaryMemory
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
import ObjC.MetalPerformanceShaders.Internal.Structs
import ObjC.MetalPerformanceShaders.Internal.Enums
import ObjC.Foundation.Internal.Classes

-- | Get a well known <MPSNDArrayAllocator> that makes standard MTLBuffers
--
-- ObjC selector: @+ defaultAllocator@
defaultAllocator :: IO RawId
defaultAllocator  =
  do
    cls' <- getRequiredClass "MPSNDArray"
    fmap (RawId . castPtr) $ sendClassMsg cls' (mkSelector "defaultAllocator") (retPtr retVoid) []

-- | The number of elements in the dimension at dimensionIndex
--
-- The dimension length is at least as large as the existing                  slice length.  Views of this MPSNDArray may have differing                  dimension lengths.
--
-- ObjC selector: @- lengthOfDimension:@
lengthOfDimension :: IsMPSNDArray mpsndArray => mpsndArray -> CULong -> IO CULong
lengthOfDimension mpsndArray  dimensionIndex =
  sendMsg mpsndArray (mkSelector "lengthOfDimension:") retCULong [argCULong (fromIntegral dimensionIndex)]

-- | Create a MPSNDArrayDescriptor that describes this MPSNDArray
--
-- The descriptor will describe the shape of the MPSNDArray              after all deferred slicing and transposes have completed.              A new descriptor is created each time to allow for              further customization of the descriptor by the application.
--
-- Returns: A new autoreleased MPSNDArrayDescriptor that matches the              shape of the MPSNDArray, suitable for introduction of slice,              cast and transpose operations.
--
-- ObjC selector: @- descriptor@
descriptor :: IsMPSNDArray mpsndArray => mpsndArray -> IO (Id MPSNDArrayDescriptor)
descriptor mpsndArray  =
  sendMsg mpsndArray (mkSelector "descriptor") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- init@
init_ :: IsMPSNDArray mpsndArray => mpsndArray -> IO (Id MPSNDArray)
init_ mpsndArray  =
  sendMsg mpsndArray (mkSelector "init") (retPtr retVoid) [] >>= ownedObject . castPtr

-- | Initialize an MPSNDArrayDescriptor object on a device              for given dimension sizes in descriptor.
--
-- @device@ — The device on which the data type will be created.
--
-- @descriptor@ — The MPSNDArrayDescriptor used for initializing the the NDArray
--
-- Returns: A valid MPSNDArray object or nil, if failure.
--
-- ObjC selector: @- initWithDevice:descriptor:@
initWithDevice_descriptor :: (IsMPSNDArray mpsndArray, IsMPSNDArrayDescriptor descriptor) => mpsndArray -> RawId -> descriptor -> IO (Id MPSNDArray)
initWithDevice_descriptor mpsndArray  device descriptor =
withObjCPtr descriptor $ \raw_descriptor ->
    sendMsg mpsndArray (mkSelector "initWithDevice:descriptor:") (retPtr retVoid) [argPtr (castPtr (unRawId device) :: Ptr ()), argPtr (castPtr raw_descriptor :: Ptr ())] >>= ownedObject . castPtr

-- | Create a 1-Dimensional length=1 NDArray to hold a scalar
--
-- ObjC selector: @- initWithDevice:scalar:@
initWithDevice_scalar :: IsMPSNDArray mpsndArray => mpsndArray -> RawId -> CDouble -> IO (Id MPSNDArray)
initWithDevice_scalar mpsndArray  device value =
  sendMsg mpsndArray (mkSelector "initWithDevice:scalar:") (retPtr retVoid) [argPtr (castPtr (unRawId device) :: Ptr ()), argCDouble (fromIntegral value)] >>= ownedObject . castPtr

-- | Initialize an MPSNDArray object from a Metal Buffer with a given descriptor and offset in bytes.
--
-- @buffer@ — The buffer used for initializing. The NDArray will alias to this buffer at the given offset.
--
-- @offset@ — Offset in bytes to the buffer.
--
-- @descriptor@ — The MPSNDArrayDescriptor used for initializing the the NDArray.
--
-- Returns: A valid MPSNDArray object or nil, if failure.
--
-- ObjC selector: @- initWithBuffer:offset:descriptor:@
initWithBuffer_offset_descriptor :: (IsMPSNDArray mpsndArray, IsMPSNDArrayDescriptor descriptor) => mpsndArray -> RawId -> CULong -> descriptor -> IO (Id MPSNDArray)
initWithBuffer_offset_descriptor mpsndArray  buffer offset descriptor =
withObjCPtr descriptor $ \raw_descriptor ->
    sendMsg mpsndArray (mkSelector "initWithBuffer:offset:descriptor:") (retPtr retVoid) [argPtr (castPtr (unRawId buffer) :: Ptr ()), argCULong (fromIntegral offset), argPtr (castPtr raw_descriptor :: Ptr ())] >>= ownedObject . castPtr

-- | Returns the user buffer in case the NDArray was initialized with an MTLBuffer.
--
-- Returns: The user-provided MTLBuffer that was used to initialize this MPSNDArray or nil, in case it was not..
--
-- ObjC selector: @- userBuffer@
userBuffer :: IsMPSNDArray mpsndArray => mpsndArray -> IO RawId
userBuffer mpsndArray  =
  fmap (RawId . castPtr) $ sendMsg mpsndArray (mkSelector "userBuffer") (retPtr retVoid) []

-- | Get the number of bytes used to allocate underyling MTLResources
--
-- This is the size of the backing store of underlying MTLResources.                  It does not include all storage used by the object, for example                  the storage used to hold the MPSNDArray instantiation and MTLBuffer                  is not included. It only measures the size of the allocation used                  to hold the MPSNDArray data in the MTLBuffer. This value is subject to                  change between different devices and operating systems.
--
-- Except when -initWithBuffer:descriptor: is used, most MPSNDArrays are allocated                  initiallly without a backing store. The backing store is allocated lazily when                  it is needed, typically when the MPSNDArray is written to the first time.                  Consequently, in most cases, it should be inexpensive to make                  a MPSImage to see how much memory it will need, and release it                  if it is too large.
--
-- ObjC selector: @- resourceSize@
resourceSize :: IsMPSNDArray mpsndArray => mpsndArray -> IO CULong
resourceSize mpsndArray  =
  sendMsg mpsndArray (mkSelector "resourceSize") retCULong []

-- | Make a new representation of a MPSNDArray with a slice, transpose or other change in property
--
-- If possible, the views will merely record the slice or transpose without performing the              operation. Many MPSKernels are able to operate on subregions of a MPSNDArray or operate on transposed              data, so making a new copy of the data for these operations would be wasteful.  A copy may be forced by              a change in dataType, rowBytes, or when using a view with a MPSKernel that does not support              the deferred operation. To force an operation to occur immediately, use MPSAliasingStrategyShallNotAlias              Otherwise, it is likely that the new MPSNDArray will share a MTLBuffer with the parent and alias              its memory.
--
-- @cmdBuf@ — The command buffer on which to perform physical copies if any are required
--
-- @descriptor@ — A MPSNDArrayDescriptor describing the shape of the new view of the data
--
-- @aliasing@ — A aliasing strategy to direct MPS how to respond to cases when aliasing can or can not                          be performed.
--
-- Returns: A new MPSNDArray, if it is possible to make one. Otherwise nil is returned. The MPSNDArray is autoreleased.
--
-- ObjC selector: @- arrayViewWithCommandBuffer:descriptor:aliasing:@
arrayViewWithCommandBuffer_descriptor_aliasing :: (IsMPSNDArray mpsndArray, IsMPSNDArrayDescriptor descriptor) => mpsndArray -> RawId -> descriptor -> MPSAliasingStrategy -> IO (Id MPSNDArray)
arrayViewWithCommandBuffer_descriptor_aliasing mpsndArray  cmdBuf descriptor aliasing =
withObjCPtr descriptor $ \raw_descriptor ->
    sendMsg mpsndArray (mkSelector "arrayViewWithCommandBuffer:descriptor:aliasing:") (retPtr retVoid) [argPtr (castPtr (unRawId cmdBuf) :: Ptr ()), argPtr (castPtr raw_descriptor :: Ptr ()), argCULong (coerce aliasing)] >>= retainedObject . castPtr

-- | Make a new representation of a MPSNDArray with a slice, transpose or other change in property, trying to alias to result.
--
-- The same as @arrayViewWithCommandBuffer@, except that tries to always alias, and therefore does not require a commanbuffer.              If aliasing is not possible nil is returned.              This method is useful in making aliasing transposes and slices, that are guaranteed to be able to alias. For reshapes it is recommended              to use the @MPSNDArrayIdentity@ methods.
--
-- @descriptor@ — A MPSNDArrayDescriptor describing the shape of the new view of the data
--
-- Returns: A new MPSNDArray, if it is possible to make one. Otherwise nil is returned. The MPSNDArray is autoreleased.
--
-- ObjC selector: @- arrayViewWithDescriptor:@
arrayViewWithDescriptor :: (IsMPSNDArray mpsndArray, IsMPSNDArrayDescriptor descriptor) => mpsndArray -> descriptor -> IO (Id MPSNDArray)
arrayViewWithDescriptor mpsndArray  descriptor =
withObjCPtr descriptor $ \raw_descriptor ->
    sendMsg mpsndArray (mkSelector "arrayViewWithDescriptor:") (retPtr retVoid) [argPtr (castPtr raw_descriptor :: Ptr ())] >>= retainedObject . castPtr

-- | Make a new representation of a MPSNDArray with given strides and a new shape.
--
-- This operation always returns a new view of the same underlying MTLBuffer, but works only with contiguous buffers.
--
-- @numberOfDimensions@ — Number of dimensions in the new view.
--
-- @dimensionSizes@ — Size of each new dimension. Fastest running dimension first. Must be of length numberOfDimensions.
--
-- @dimStrides@ — The strides for each dimension. First number must be one. Must be non-decreasing.  Must be of length numberOfDimensions.
--
-- Returns: A new MPSNDArray, if it is possible to make one. Otherwise nil is returned. The MPSNDArray is autoreleased.
--
-- ObjC selector: @- arrayViewWithDimensionCount:dimensionSizes:strides:@
arrayViewWithDimensionCount_dimensionSizes_strides :: IsMPSNDArray mpsndArray => mpsndArray -> CULong -> Const (Ptr CULong) -> Const (Ptr CULong) -> IO (Id MPSNDArray)
arrayViewWithDimensionCount_dimensionSizes_strides mpsndArray  numberOfDimensions dimensionSizes dimStrides =
  sendMsg mpsndArray (mkSelector "arrayViewWithDimensionCount:dimensionSizes:strides:") (retPtr retVoid) [argCULong (fromIntegral numberOfDimensions), argPtr (unConst dimensionSizes), argPtr (unConst dimStrides)] >>= retainedObject . castPtr

-- | Do a GPU side copy of the contents of a MPSNDArray to a MTLBuffer
--
-- To do a transpose or slice as part of the operation, make a MPSNDArray view first that encodes that operation.
--
-- @cmdBuf@ — The command buffer on which to encode the operation
--
-- @buffer@ — The destination to overwrite
--
-- @destinationDataType@ — The destination data type.
--
-- @offset@ — The byte offset to where the {0,0,0...}th element will be written
--
-- @rowStrides@ — An optional array of (numberOfDimensions-1) byte counts which describe                          the byte offset from position 0 of the respective dimension to position 1.
--
-- ObjC selector: @- exportDataWithCommandBuffer:toBuffer:destinationDataType:offset:rowStrides:@
exportDataWithCommandBuffer_toBuffer_destinationDataType_offset_rowStrides :: IsMPSNDArray mpsndArray => mpsndArray -> RawId -> RawId -> MPSDataType -> CULong -> Ptr CLong -> IO ()
exportDataWithCommandBuffer_toBuffer_destinationDataType_offset_rowStrides mpsndArray  cmdBuf buffer destinationDataType offset rowStrides =
  sendMsg mpsndArray (mkSelector "exportDataWithCommandBuffer:toBuffer:destinationDataType:offset:rowStrides:") retVoid [argPtr (castPtr (unRawId cmdBuf) :: Ptr ()), argPtr (castPtr (unRawId buffer) :: Ptr ()), argCUInt (coerce destinationDataType), argCULong (fromIntegral offset), argPtr rowStrides]

-- | Do a GPU side copy of the contents of a MTLBuffer into a MPSNDArray
--
-- Copy data from provided buffer to the NDArray. Implicit transposes and slicing shall be honored.
--
-- @cmdBuf@ — The command buffer on which to encode the operation
--
-- @buffer@ — The destination to read from
--
-- @sourceDataType@ — The source data type.
--
-- @offset@ — The byte offset in the buffer from where the {0,0,0...}th element is to be read.
--
-- @rowStrides@ — An optional array of (numberOfDimensions-1) byte counts which describe                          the byte offset from position 0 of the respective dimension to position 1.
--
-- ObjC selector: @- importDataWithCommandBuffer:fromBuffer:sourceDataType:offset:rowStrides:@
importDataWithCommandBuffer_fromBuffer_sourceDataType_offset_rowStrides :: IsMPSNDArray mpsndArray => mpsndArray -> RawId -> RawId -> MPSDataType -> CULong -> Ptr CLong -> IO ()
importDataWithCommandBuffer_fromBuffer_sourceDataType_offset_rowStrides mpsndArray  cmdBuf buffer sourceDataType offset rowStrides =
  sendMsg mpsndArray (mkSelector "importDataWithCommandBuffer:fromBuffer:sourceDataType:offset:rowStrides:") retVoid [argPtr (castPtr (unRawId cmdBuf) :: Ptr ()), argPtr (castPtr (unRawId buffer) :: Ptr ()), argCUInt (coerce sourceDataType), argCULong (fromIntegral offset), argPtr rowStrides]

-- | Copy bytes from MPSNDArray into buffer
--
-- The dimensionality and size of the copy region is given by the size of the MPSNDArray                  For subregions, use a MPSNDArray view.
--
-- @buffer@ — A pointer to memory where to write the data
--
-- @strideBytesPerDimension@ — An optional array of numberOfDimensions sizes, which gives the distance                                          in bytes from one element to the next in that dimension in buffer. The first value                                          is typically dataTypeSize. The next is a row bytes. The next is 2d matrix bytes,                                          and so forth.  If the value is nil, these are calculated for you assuming that the                                          data is packed without additional space in between elements, rows, etc.                                          0 and negative values are permitted.
--
-- ObjC selector: @- readBytes:strideBytes:@
readBytes_strideBytes :: IsMPSNDArray mpsndArray => mpsndArray -> Ptr () -> Ptr CLong -> IO ()
readBytes_strideBytes mpsndArray  buffer strideBytesPerDimension =
  sendMsg mpsndArray (mkSelector "readBytes:strideBytes:") retVoid [argPtr buffer, argPtr strideBytesPerDimension]

-- | Copy bytes from a buffer into the MPSNDArray
--
-- The dimensionality and size of the copy region is given by the size of the MPSNDArray                  For subregions, use a MPSNDArray view.
--
-- @buffer@ — A pointer to memory where to read the data
--
-- @strideBytesPerDimension@ — An optional array of numberOfDimensions sizes, which gives the distance                                          in bytes from one element to the next in that dimension in buffer. The first value                                          is typically dataTypeSize. The next is a row bytes. The next is 2d matrix bytes,                                          and so forth.  If strideBytesPerDimension is nil, these are calculated for you assuming that the                                          data is packed without additional space in between elements, rows, etc.                                          0 and negative values are permitted.
--
-- ObjC selector: @- writeBytes:strideBytes:@
writeBytes_strideBytes :: IsMPSNDArray mpsndArray => mpsndArray -> Ptr () -> Ptr CLong -> IO ()
writeBytes_strideBytes mpsndArray  buffer strideBytesPerDimension =
  sendMsg mpsndArray (mkSelector "writeBytes:strideBytes:") retVoid [argPtr buffer, argPtr strideBytesPerDimension]

-- | Use a blit encoder if a discrete device to update CPU contents of underlying buffer with latest GPU value
--
-- @commandBuffer@ — The commandBuffer on which we transfer the contents.
--
-- ObjC selector: @- synchronizeOnCommandBuffer:@
synchronizeOnCommandBuffer :: IsMPSNDArray mpsndArray => mpsndArray -> RawId -> IO ()
synchronizeOnCommandBuffer mpsndArray  commandBuffer =
  sendMsg mpsndArray (mkSelector "synchronizeOnCommandBuffer:") retVoid [argPtr (castPtr (unRawId commandBuffer) :: Ptr ())]

-- | A used specified string to help identify the array during debugging.
--
-- May be externally visible to tools like Instruments
--
-- ObjC selector: @- label@
label :: IsMPSNDArray mpsndArray => mpsndArray -> IO (Id NSString)
label mpsndArray  =
  sendMsg mpsndArray (mkSelector "label") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | A used specified string to help identify the array during debugging.
--
-- May be externally visible to tools like Instruments
--
-- ObjC selector: @- setLabel:@
setLabel :: (IsMPSNDArray mpsndArray, IsNSString value) => mpsndArray -> value -> IO ()
setLabel mpsndArray  value =
withObjCPtr value $ \raw_value ->
    sendMsg mpsndArray (mkSelector "setLabel:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- | The type of data stored by each element in the array
--
-- ObjC selector: @- dataType@
dataType :: IsMPSNDArray mpsndArray => mpsndArray -> IO MPSDataType
dataType mpsndArray  =
  fmap (coerce :: CUInt -> MPSDataType) $ sendMsg mpsndArray (mkSelector "dataType") retCUInt []

-- | The size of one element in the MPSNDArray
--
-- ObjC selector: @- dataTypeSize@
dataTypeSize :: IsMPSNDArray mpsndArray => mpsndArray -> IO CULong
dataTypeSize mpsndArray  =
  sendMsg mpsndArray (mkSelector "dataTypeSize") retCULong []

-- | Number of dimensions in the NDArray
--
-- ObjC selector: @- numberOfDimensions@
numberOfDimensions :: IsMPSNDArray mpsndArray => mpsndArray -> IO CULong
numberOfDimensions mpsndArray  =
  sendMsg mpsndArray (mkSelector "numberOfDimensions") retCULong []

-- | The parent MPSNDArray that this object aliases
--
-- If the MPSNDArray was createrd as a array view of another MPSNDArray object, and aliases content              in the same MTLBuffer, the original MPSNDArray will be retained as the parent here. Two MPSNDArrays              alias if they share a common ancestor. Note that the parent may itself have a parent, and so forth.
--
-- ObjC selector: @- parent@
parent :: IsMPSNDArray mpsndArray => mpsndArray -> IO (Id MPSNDArray)
parent mpsndArray  =
  sendMsg mpsndArray (mkSelector "parent") (retPtr retVoid) [] >>= retainedObject . castPtr

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @defaultAllocator@
defaultAllocatorSelector :: Selector
defaultAllocatorSelector = mkSelector "defaultAllocator"

-- | @Selector@ for @lengthOfDimension:@
lengthOfDimensionSelector :: Selector
lengthOfDimensionSelector = mkSelector "lengthOfDimension:"

-- | @Selector@ for @descriptor@
descriptorSelector :: Selector
descriptorSelector = mkSelector "descriptor"

-- | @Selector@ for @init@
initSelector :: Selector
initSelector = mkSelector "init"

-- | @Selector@ for @initWithDevice:descriptor:@
initWithDevice_descriptorSelector :: Selector
initWithDevice_descriptorSelector = mkSelector "initWithDevice:descriptor:"

-- | @Selector@ for @initWithDevice:scalar:@
initWithDevice_scalarSelector :: Selector
initWithDevice_scalarSelector = mkSelector "initWithDevice:scalar:"

-- | @Selector@ for @initWithBuffer:offset:descriptor:@
initWithBuffer_offset_descriptorSelector :: Selector
initWithBuffer_offset_descriptorSelector = mkSelector "initWithBuffer:offset:descriptor:"

-- | @Selector@ for @userBuffer@
userBufferSelector :: Selector
userBufferSelector = mkSelector "userBuffer"

-- | @Selector@ for @resourceSize@
resourceSizeSelector :: Selector
resourceSizeSelector = mkSelector "resourceSize"

-- | @Selector@ for @arrayViewWithCommandBuffer:descriptor:aliasing:@
arrayViewWithCommandBuffer_descriptor_aliasingSelector :: Selector
arrayViewWithCommandBuffer_descriptor_aliasingSelector = mkSelector "arrayViewWithCommandBuffer:descriptor:aliasing:"

-- | @Selector@ for @arrayViewWithDescriptor:@
arrayViewWithDescriptorSelector :: Selector
arrayViewWithDescriptorSelector = mkSelector "arrayViewWithDescriptor:"

-- | @Selector@ for @arrayViewWithDimensionCount:dimensionSizes:strides:@
arrayViewWithDimensionCount_dimensionSizes_stridesSelector :: Selector
arrayViewWithDimensionCount_dimensionSizes_stridesSelector = mkSelector "arrayViewWithDimensionCount:dimensionSizes:strides:"

-- | @Selector@ for @exportDataWithCommandBuffer:toBuffer:destinationDataType:offset:rowStrides:@
exportDataWithCommandBuffer_toBuffer_destinationDataType_offset_rowStridesSelector :: Selector
exportDataWithCommandBuffer_toBuffer_destinationDataType_offset_rowStridesSelector = mkSelector "exportDataWithCommandBuffer:toBuffer:destinationDataType:offset:rowStrides:"

-- | @Selector@ for @importDataWithCommandBuffer:fromBuffer:sourceDataType:offset:rowStrides:@
importDataWithCommandBuffer_fromBuffer_sourceDataType_offset_rowStridesSelector :: Selector
importDataWithCommandBuffer_fromBuffer_sourceDataType_offset_rowStridesSelector = mkSelector "importDataWithCommandBuffer:fromBuffer:sourceDataType:offset:rowStrides:"

-- | @Selector@ for @readBytes:strideBytes:@
readBytes_strideBytesSelector :: Selector
readBytes_strideBytesSelector = mkSelector "readBytes:strideBytes:"

-- | @Selector@ for @writeBytes:strideBytes:@
writeBytes_strideBytesSelector :: Selector
writeBytes_strideBytesSelector = mkSelector "writeBytes:strideBytes:"

-- | @Selector@ for @synchronizeOnCommandBuffer:@
synchronizeOnCommandBufferSelector :: Selector
synchronizeOnCommandBufferSelector = mkSelector "synchronizeOnCommandBuffer:"

-- | @Selector@ for @label@
labelSelector :: Selector
labelSelector = mkSelector "label"

-- | @Selector@ for @setLabel:@
setLabelSelector :: Selector
setLabelSelector = mkSelector "setLabel:"

-- | @Selector@ for @dataType@
dataTypeSelector :: Selector
dataTypeSelector = mkSelector "dataType"

-- | @Selector@ for @dataTypeSize@
dataTypeSizeSelector :: Selector
dataTypeSizeSelector = mkSelector "dataTypeSize"

-- | @Selector@ for @numberOfDimensions@
numberOfDimensionsSelector :: Selector
numberOfDimensionsSelector = mkSelector "numberOfDimensions"

-- | @Selector@ for @parent@
parentSelector :: Selector
parentSelector = mkSelector "parent"

