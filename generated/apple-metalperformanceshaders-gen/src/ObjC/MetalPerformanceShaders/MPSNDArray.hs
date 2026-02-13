{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE DataKinds #-}
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
  , arrayViewWithShape_strides
  , arrayViewWithDimensionCount_dimensionSizes_strides
  , exportDataWithCommandBuffer_toBuffer_destinationDataType_offset_rowStrides
  , importDataWithCommandBuffer_fromBuffer_sourceDataType_offset_rowStrides
  , exportDataWithCommandBuffer_toImages_offset
  , importDataWithCommandBuffer_fromImages_offset
  , readBytes_strideBytes
  , writeBytes_strideBytes
  , synchronizeOnCommandBuffer
  , label
  , setLabel
  , dataType
  , dataTypeSize
  , numberOfDimensions
  , device
  , parent
  , arrayViewWithCommandBuffer_descriptor_aliasingSelector
  , arrayViewWithDescriptorSelector
  , arrayViewWithDimensionCount_dimensionSizes_stridesSelector
  , arrayViewWithShape_stridesSelector
  , dataTypeSelector
  , dataTypeSizeSelector
  , defaultAllocatorSelector
  , descriptorSelector
  , deviceSelector
  , exportDataWithCommandBuffer_toBuffer_destinationDataType_offset_rowStridesSelector
  , exportDataWithCommandBuffer_toImages_offsetSelector
  , importDataWithCommandBuffer_fromBuffer_sourceDataType_offset_rowStridesSelector
  , importDataWithCommandBuffer_fromImages_offsetSelector
  , initSelector
  , initWithBuffer_offset_descriptorSelector
  , initWithDevice_descriptorSelector
  , initWithDevice_scalarSelector
  , labelSelector
  , lengthOfDimensionSelector
  , numberOfDimensionsSelector
  , parentSelector
  , readBytes_strideBytesSelector
  , resourceSizeSelector
  , setLabelSelector
  , synchronizeOnCommandBufferSelector
  , userBufferSelector
  , writeBytes_strideBytesSelector

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

import Foreign.Ptr (Ptr, FunPtr)
import Foreign.C.Types

import ObjC.Runtime.Types
import ObjC.Runtime.Message (sendMessage, sendOwnedMessage, sendClassMessage, sendOwnedClassMessage)
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
    sendClassMessage cls' defaultAllocatorSelector

-- | The number of elements in the dimension at dimensionIndex
--
-- The dimension length is at least as large as the existing                  slice length.  Views of this MPSNDArray may have differing                  dimension lengths.
--
-- ObjC selector: @- lengthOfDimension:@
lengthOfDimension :: IsMPSNDArray mpsndArray => mpsndArray -> CULong -> IO CULong
lengthOfDimension mpsndArray dimensionIndex =
  sendMessage mpsndArray lengthOfDimensionSelector dimensionIndex

-- | Create a MPSNDArrayDescriptor that describes this MPSNDArray
--
-- The descriptor will describe the shape of the MPSNDArray              after all deferred slicing and transposes have completed.              A new descriptor is created each time to allow for              further customization of the descriptor by the application.
--
-- Returns: A new autoreleased MPSNDArrayDescriptor that matches the              shape of the MPSNDArray, suitable for introduction of slice,              cast and transpose operations.
--
-- ObjC selector: @- descriptor@
descriptor :: IsMPSNDArray mpsndArray => mpsndArray -> IO (Id MPSNDArrayDescriptor)
descriptor mpsndArray =
  sendMessage mpsndArray descriptorSelector

-- | @- init@
init_ :: IsMPSNDArray mpsndArray => mpsndArray -> IO (Id MPSNDArray)
init_ mpsndArray =
  sendOwnedMessage mpsndArray initSelector

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
initWithDevice_descriptor mpsndArray device descriptor =
  sendOwnedMessage mpsndArray initWithDevice_descriptorSelector device (toMPSNDArrayDescriptor descriptor)

-- | Create a 1-Dimensional length=1 NDArray to hold a scalar
--
-- ObjC selector: @- initWithDevice:scalar:@
initWithDevice_scalar :: IsMPSNDArray mpsndArray => mpsndArray -> RawId -> CDouble -> IO (Id MPSNDArray)
initWithDevice_scalar mpsndArray device value =
  sendOwnedMessage mpsndArray initWithDevice_scalarSelector device value

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
initWithBuffer_offset_descriptor mpsndArray buffer offset descriptor =
  sendOwnedMessage mpsndArray initWithBuffer_offset_descriptorSelector buffer offset (toMPSNDArrayDescriptor descriptor)

-- | Returns the user buffer in case the NDArray was initialized with an MTLBuffer.
--
-- Returns: The user-provided MTLBuffer that was used to initialize this MPSNDArray or nil, in case it was not..
--
-- ObjC selector: @- userBuffer@
userBuffer :: IsMPSNDArray mpsndArray => mpsndArray -> IO RawId
userBuffer mpsndArray =
  sendMessage mpsndArray userBufferSelector

-- | Get the number of bytes used to allocate underyling MTLResources
--
-- This is the size of the backing store of underlying MTLResources.                  It does not include all storage used by the object, for example                  the storage used to hold the MPSNDArray instantiation and MTLBuffer                  is not included. It only measures the size of the allocation used                  to hold the MPSNDArray data in the MTLBuffer. This value is subject to                  change between different devices and operating systems.
--
-- Except when -initWithBuffer:descriptor: is used, most MPSNDArrays are allocated                  initiallly without a backing store. The backing store is allocated lazily when                  it is needed, typically when the MPSNDArray is written to the first time.                  Consequently, in most cases, it should be inexpensive to make                  a MPSImage to see how much memory it will need, and release it                  if it is too large.
--
-- ObjC selector: @- resourceSize@
resourceSize :: IsMPSNDArray mpsndArray => mpsndArray -> IO CULong
resourceSize mpsndArray =
  sendMessage mpsndArray resourceSizeSelector

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
arrayViewWithCommandBuffer_descriptor_aliasing mpsndArray cmdBuf descriptor aliasing =
  sendMessage mpsndArray arrayViewWithCommandBuffer_descriptor_aliasingSelector cmdBuf (toMPSNDArrayDescriptor descriptor) aliasing

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
arrayViewWithDescriptor mpsndArray descriptor =
  sendMessage mpsndArray arrayViewWithDescriptorSelector (toMPSNDArrayDescriptor descriptor)

-- | Make a new representation of a MPSNDArray with given strides and a new shape.
--
-- This operation always returns a new view of the same underlying MTLBuffer, but works only with contiguous buffers.
--
-- @shape@ — The new shape for the NDArray. Fastest running dimension last. If nil then current shape is used.
--
-- @strides@ — The strides for each dimension. Must be at least length of new shape. Last number must be one. Must be non-increasing.
--
-- Returns: A new MPSNDArray, if it is possible to make one. Otherwise nil is returned. The MPSNDArray is autoreleased.
--
-- ObjC selector: @- arrayViewWithShape:strides:@
arrayViewWithShape_strides :: IsMPSNDArray mpsndArray => mpsndArray -> RawId -> RawId -> IO (Id MPSNDArray)
arrayViewWithShape_strides mpsndArray shape strides =
  sendMessage mpsndArray arrayViewWithShape_stridesSelector shape strides

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
arrayViewWithDimensionCount_dimensionSizes_strides mpsndArray numberOfDimensions dimensionSizes dimStrides =
  sendMessage mpsndArray arrayViewWithDimensionCount_dimensionSizes_stridesSelector numberOfDimensions dimensionSizes dimStrides

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
exportDataWithCommandBuffer_toBuffer_destinationDataType_offset_rowStrides mpsndArray cmdBuf buffer destinationDataType offset rowStrides =
  sendMessage mpsndArray exportDataWithCommandBuffer_toBuffer_destinationDataType_offset_rowStridesSelector cmdBuf buffer destinationDataType offset rowStrides

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
importDataWithCommandBuffer_fromBuffer_sourceDataType_offset_rowStrides mpsndArray cmdBuf buffer sourceDataType offset rowStrides =
  sendMessage mpsndArray importDataWithCommandBuffer_fromBuffer_sourceDataType_offset_rowStridesSelector cmdBuf buffer sourceDataType offset rowStrides

-- | Do a GPU side copy of the contents of a MPSNDArray to a MPSImageBatch.
--
-- To do a transpose or slice as part of the operation, make a MPSNDArray view first that encodes that operation.              The shape of the array must be [ C, W, H, N, 1, 1, ... ], where C is dimension 0 (normally the fastest running index)              and is mapped to feature channels in the destination image, W and H are mapped to x and y coordinates in the destination              image and N is mapped to the image batch index. You can use arrayViewWithCommandBuffer: to transpose, slice and reshape              the source array to layout the data in the desired way for the image(s).
--
-- @cmdBuf@ — The command buffer on which to encode the operation/
--
-- @images@ — The destination images. NOTE: you can use [images subarrayWithRange:...] to get a sub-batch of images.
--
-- @offset@ — The offset to the image where to write - the size of the operation is defined by the source array.                          Note: offset.featureChannel must be multiple of four, otherwise results are undefined.
--
-- ObjC selector: @- exportDataWithCommandBuffer:toImages:offset:@
exportDataWithCommandBuffer_toImages_offset :: IsMPSNDArray mpsndArray => mpsndArray -> RawId -> RawId -> MPSImageCoordinate -> IO ()
exportDataWithCommandBuffer_toImages_offset mpsndArray cmdBuf images offset =
  sendMessage mpsndArray exportDataWithCommandBuffer_toImages_offsetSelector cmdBuf images offset

-- | Do a GPU side copy of the contents of a MPSImageBatch into a MPSNDArray.
--
-- This reverses exportDataWithCommandBuffer:toImages: function.
--
-- @cmdBuf@ — The command buffer on which to encode the operation.
--
-- @images@ — The source images. NOTE: you can use [images subarrayWithRange:...] to get a sub-batch of images.
--
-- @offset@ — The offset to the image where to read - the size of the operation is defined by the destination array.
--
-- ObjC selector: @- importDataWithCommandBuffer:fromImages:offset:@
importDataWithCommandBuffer_fromImages_offset :: IsMPSNDArray mpsndArray => mpsndArray -> RawId -> RawId -> MPSImageCoordinate -> IO ()
importDataWithCommandBuffer_fromImages_offset mpsndArray cmdBuf images offset =
  sendMessage mpsndArray importDataWithCommandBuffer_fromImages_offsetSelector cmdBuf images offset

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
readBytes_strideBytes mpsndArray buffer strideBytesPerDimension =
  sendMessage mpsndArray readBytes_strideBytesSelector buffer strideBytesPerDimension

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
writeBytes_strideBytes mpsndArray buffer strideBytesPerDimension =
  sendMessage mpsndArray writeBytes_strideBytesSelector buffer strideBytesPerDimension

-- | Use a blit encoder if a discrete device to update CPU contents of underlying buffer with latest GPU value
--
-- @commandBuffer@ — The commandBuffer on which we transfer the contents.
--
-- ObjC selector: @- synchronizeOnCommandBuffer:@
synchronizeOnCommandBuffer :: IsMPSNDArray mpsndArray => mpsndArray -> RawId -> IO ()
synchronizeOnCommandBuffer mpsndArray commandBuffer =
  sendMessage mpsndArray synchronizeOnCommandBufferSelector commandBuffer

-- | A used specified string to help identify the array during debugging.
--
-- May be externally visible to tools like Instruments
--
-- ObjC selector: @- label@
label :: IsMPSNDArray mpsndArray => mpsndArray -> IO (Id NSString)
label mpsndArray =
  sendMessage mpsndArray labelSelector

-- | A used specified string to help identify the array during debugging.
--
-- May be externally visible to tools like Instruments
--
-- ObjC selector: @- setLabel:@
setLabel :: (IsMPSNDArray mpsndArray, IsNSString value) => mpsndArray -> value -> IO ()
setLabel mpsndArray value =
  sendMessage mpsndArray setLabelSelector (toNSString value)

-- | The type of data stored by each element in the array
--
-- ObjC selector: @- dataType@
dataType :: IsMPSNDArray mpsndArray => mpsndArray -> IO MPSDataType
dataType mpsndArray =
  sendMessage mpsndArray dataTypeSelector

-- | The size of one element in the MPSNDArray
--
-- ObjC selector: @- dataTypeSize@
dataTypeSize :: IsMPSNDArray mpsndArray => mpsndArray -> IO CULong
dataTypeSize mpsndArray =
  sendMessage mpsndArray dataTypeSizeSelector

-- | Number of dimensions in the NDArray
--
-- ObjC selector: @- numberOfDimensions@
numberOfDimensions :: IsMPSNDArray mpsndArray => mpsndArray -> IO CULong
numberOfDimensions mpsndArray =
  sendMessage mpsndArray numberOfDimensionsSelector

-- | device
--
-- The device on which the MSPNDArray may be used
--
-- ObjC selector: @- device@
device :: IsMPSNDArray mpsndArray => mpsndArray -> IO RawId
device mpsndArray =
  sendMessage mpsndArray deviceSelector

-- | The parent MPSNDArray that this object aliases
--
-- If the MPSNDArray was createrd as a array view of another MPSNDArray object, and aliases content              in the same MTLBuffer, the original MPSNDArray will be retained as the parent here. Two MPSNDArrays              alias if they share a common ancestor. Note that the parent may itself have a parent, and so forth.
--
-- ObjC selector: @- parent@
parent :: IsMPSNDArray mpsndArray => mpsndArray -> IO (Id MPSNDArray)
parent mpsndArray =
  sendMessage mpsndArray parentSelector

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @defaultAllocator@
defaultAllocatorSelector :: Selector '[] RawId
defaultAllocatorSelector = mkSelector "defaultAllocator"

-- | @Selector@ for @lengthOfDimension:@
lengthOfDimensionSelector :: Selector '[CULong] CULong
lengthOfDimensionSelector = mkSelector "lengthOfDimension:"

-- | @Selector@ for @descriptor@
descriptorSelector :: Selector '[] (Id MPSNDArrayDescriptor)
descriptorSelector = mkSelector "descriptor"

-- | @Selector@ for @init@
initSelector :: Selector '[] (Id MPSNDArray)
initSelector = mkSelector "init"

-- | @Selector@ for @initWithDevice:descriptor:@
initWithDevice_descriptorSelector :: Selector '[RawId, Id MPSNDArrayDescriptor] (Id MPSNDArray)
initWithDevice_descriptorSelector = mkSelector "initWithDevice:descriptor:"

-- | @Selector@ for @initWithDevice:scalar:@
initWithDevice_scalarSelector :: Selector '[RawId, CDouble] (Id MPSNDArray)
initWithDevice_scalarSelector = mkSelector "initWithDevice:scalar:"

-- | @Selector@ for @initWithBuffer:offset:descriptor:@
initWithBuffer_offset_descriptorSelector :: Selector '[RawId, CULong, Id MPSNDArrayDescriptor] (Id MPSNDArray)
initWithBuffer_offset_descriptorSelector = mkSelector "initWithBuffer:offset:descriptor:"

-- | @Selector@ for @userBuffer@
userBufferSelector :: Selector '[] RawId
userBufferSelector = mkSelector "userBuffer"

-- | @Selector@ for @resourceSize@
resourceSizeSelector :: Selector '[] CULong
resourceSizeSelector = mkSelector "resourceSize"

-- | @Selector@ for @arrayViewWithCommandBuffer:descriptor:aliasing:@
arrayViewWithCommandBuffer_descriptor_aliasingSelector :: Selector '[RawId, Id MPSNDArrayDescriptor, MPSAliasingStrategy] (Id MPSNDArray)
arrayViewWithCommandBuffer_descriptor_aliasingSelector = mkSelector "arrayViewWithCommandBuffer:descriptor:aliasing:"

-- | @Selector@ for @arrayViewWithDescriptor:@
arrayViewWithDescriptorSelector :: Selector '[Id MPSNDArrayDescriptor] (Id MPSNDArray)
arrayViewWithDescriptorSelector = mkSelector "arrayViewWithDescriptor:"

-- | @Selector@ for @arrayViewWithShape:strides:@
arrayViewWithShape_stridesSelector :: Selector '[RawId, RawId] (Id MPSNDArray)
arrayViewWithShape_stridesSelector = mkSelector "arrayViewWithShape:strides:"

-- | @Selector@ for @arrayViewWithDimensionCount:dimensionSizes:strides:@
arrayViewWithDimensionCount_dimensionSizes_stridesSelector :: Selector '[CULong, Const (Ptr CULong), Const (Ptr CULong)] (Id MPSNDArray)
arrayViewWithDimensionCount_dimensionSizes_stridesSelector = mkSelector "arrayViewWithDimensionCount:dimensionSizes:strides:"

-- | @Selector@ for @exportDataWithCommandBuffer:toBuffer:destinationDataType:offset:rowStrides:@
exportDataWithCommandBuffer_toBuffer_destinationDataType_offset_rowStridesSelector :: Selector '[RawId, RawId, MPSDataType, CULong, Ptr CLong] ()
exportDataWithCommandBuffer_toBuffer_destinationDataType_offset_rowStridesSelector = mkSelector "exportDataWithCommandBuffer:toBuffer:destinationDataType:offset:rowStrides:"

-- | @Selector@ for @importDataWithCommandBuffer:fromBuffer:sourceDataType:offset:rowStrides:@
importDataWithCommandBuffer_fromBuffer_sourceDataType_offset_rowStridesSelector :: Selector '[RawId, RawId, MPSDataType, CULong, Ptr CLong] ()
importDataWithCommandBuffer_fromBuffer_sourceDataType_offset_rowStridesSelector = mkSelector "importDataWithCommandBuffer:fromBuffer:sourceDataType:offset:rowStrides:"

-- | @Selector@ for @exportDataWithCommandBuffer:toImages:offset:@
exportDataWithCommandBuffer_toImages_offsetSelector :: Selector '[RawId, RawId, MPSImageCoordinate] ()
exportDataWithCommandBuffer_toImages_offsetSelector = mkSelector "exportDataWithCommandBuffer:toImages:offset:"

-- | @Selector@ for @importDataWithCommandBuffer:fromImages:offset:@
importDataWithCommandBuffer_fromImages_offsetSelector :: Selector '[RawId, RawId, MPSImageCoordinate] ()
importDataWithCommandBuffer_fromImages_offsetSelector = mkSelector "importDataWithCommandBuffer:fromImages:offset:"

-- | @Selector@ for @readBytes:strideBytes:@
readBytes_strideBytesSelector :: Selector '[Ptr (), Ptr CLong] ()
readBytes_strideBytesSelector = mkSelector "readBytes:strideBytes:"

-- | @Selector@ for @writeBytes:strideBytes:@
writeBytes_strideBytesSelector :: Selector '[Ptr (), Ptr CLong] ()
writeBytes_strideBytesSelector = mkSelector "writeBytes:strideBytes:"

-- | @Selector@ for @synchronizeOnCommandBuffer:@
synchronizeOnCommandBufferSelector :: Selector '[RawId] ()
synchronizeOnCommandBufferSelector = mkSelector "synchronizeOnCommandBuffer:"

-- | @Selector@ for @label@
labelSelector :: Selector '[] (Id NSString)
labelSelector = mkSelector "label"

-- | @Selector@ for @setLabel:@
setLabelSelector :: Selector '[Id NSString] ()
setLabelSelector = mkSelector "setLabel:"

-- | @Selector@ for @dataType@
dataTypeSelector :: Selector '[] MPSDataType
dataTypeSelector = mkSelector "dataType"

-- | @Selector@ for @dataTypeSize@
dataTypeSizeSelector :: Selector '[] CULong
dataTypeSizeSelector = mkSelector "dataTypeSize"

-- | @Selector@ for @numberOfDimensions@
numberOfDimensionsSelector :: Selector '[] CULong
numberOfDimensionsSelector = mkSelector "numberOfDimensions"

-- | @Selector@ for @device@
deviceSelector :: Selector '[] RawId
deviceSelector = mkSelector "device"

-- | @Selector@ for @parent@
parentSelector :: Selector '[] (Id MPSNDArray)
parentSelector = mkSelector "parent"

