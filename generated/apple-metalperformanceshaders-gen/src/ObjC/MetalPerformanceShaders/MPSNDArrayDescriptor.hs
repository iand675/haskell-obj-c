{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | MPSNDArrayDescriptor
--
-- This depends on Metal.framework
--
-- A MPSNDArrayDescriptor object describes a attributes of MPSNDArray and is used to              create one (see MPSNDArray discussion below)
--
-- Generated bindings for @MPSNDArrayDescriptor@.
module ObjC.MetalPerformanceShaders.MPSNDArrayDescriptor
  ( MPSNDArrayDescriptor
  , IsMPSNDArrayDescriptor(..)
  , lengthOfDimension
  , sliceRangeForDimension
  , sliceDimension_withSubrange
  , transposeDimension_withDimension
  , permuteWithDimensionOrder
  , getShape
  , descriptorWithDataType_dimensionCount_dimensionSizes
  , descriptorWithDataType_shape
  , descriptorWithDataType_dimensionSizes
  , reshapeWithDimensionCount_dimensionSizes
  , reshapeWithShape
  , init_
  , dataType
  , setDataType
  , numberOfDimensions
  , setNumberOfDimensions
  , preferPackedRows
  , setPreferPackedRows
  , dataTypeSelector
  , descriptorWithDataType_dimensionCount_dimensionSizesSelector
  , descriptorWithDataType_dimensionSizesSelector
  , descriptorWithDataType_shapeSelector
  , getShapeSelector
  , initSelector
  , lengthOfDimensionSelector
  , numberOfDimensionsSelector
  , permuteWithDimensionOrderSelector
  , preferPackedRowsSelector
  , reshapeWithDimensionCount_dimensionSizesSelector
  , reshapeWithShapeSelector
  , setDataTypeSelector
  , setNumberOfDimensionsSelector
  , setPreferPackedRowsSelector
  , sliceDimension_withSubrangeSelector
  , sliceRangeForDimensionSelector
  , transposeDimension_withDimensionSelector

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
import ObjC.MetalPerformanceShaders.Internal.Structs
import ObjC.MetalPerformanceShaders.Internal.Enums
import ObjC.Foundation.Internal.Classes

-- | The number of elements of type dataType in the indicated dimension.
--
-- If dimensionIndex >= numberOfDimensions, 1 will be returned.
--
-- @dimensionIndex@ — dimension the MPSNDArray for which to return the length
--
-- Returns: The number of elements in that dimension.
--
-- ObjC selector: @- lengthOfDimension:@
lengthOfDimension :: IsMPSNDArrayDescriptor mpsndArrayDescriptor => mpsndArrayDescriptor -> CULong -> IO CULong
lengthOfDimension mpsndArrayDescriptor dimensionIndex =
  sendMessage mpsndArrayDescriptor lengthOfDimensionSelector dimensionIndex

-- | The slice dimensions for each dimension
--
-- A slice is a subregion of a dimension. It is                 used to calve off a fraction of a larger NDArray.
--
-- @dimensionIndex@ — The index of the dimension
--
-- Returns: Returns the slice range for the index. If the                 dimensionIndex >= numberOfDimensions, {0,1} is returned.
--
-- ObjC selector: @- sliceRangeForDimension:@
sliceRangeForDimension :: IsMPSNDArrayDescriptor mpsndArrayDescriptor => mpsndArrayDescriptor -> CULong -> IO MPSDimensionSlice
sliceRangeForDimension mpsndArrayDescriptor dimensionIndex =
  sendMessage mpsndArrayDescriptor sliceRangeForDimensionSelector dimensionIndex

-- | The slice dimensions for each dimension
--
-- A slice is a subregion of a dimension. It is                 used to calve off a fraction of a larger NDArray.
--
-- Default:  NSRange(0, lengthOfDimension(i))
--
-- @subRange@ — The region of the slice, start value is wrt dimensionLength of the NDArray.
--
-- @dimensionIndex@ — The index of the dimension. Must be < numberOfDimensions
--
-- ObjC selector: @- sliceDimension:withSubrange:@
sliceDimension_withSubrange :: IsMPSNDArrayDescriptor mpsndArrayDescriptor => mpsndArrayDescriptor -> CULong -> MPSDimensionSlice -> IO ()
sliceDimension_withSubrange mpsndArrayDescriptor dimensionIndex subRange =
  sendMessage mpsndArrayDescriptor sliceDimension_withSubrangeSelector dimensionIndex subRange

-- | transpose two dimensions
--
-- If the intention is to insert a length 1 dimension, increment the numberOfDimensions first.
--
-- @dimensionIndex@ — The first dimension. Must be < numberOfDimensions
--
-- @dimensionIndex2@ — The second dimension.  Must be < number of Dimensions.
--
-- ObjC selector: @- transposeDimension:withDimension:@
transposeDimension_withDimension :: IsMPSNDArrayDescriptor mpsndArrayDescriptor => mpsndArrayDescriptor -> CULong -> CULong -> IO ()
transposeDimension_withDimension mpsndArrayDescriptor dimensionIndex dimensionIndex2 =
  sendMessage mpsndArrayDescriptor transposeDimension_withDimensionSelector dimensionIndex dimensionIndex2

-- | Permutes the dimensions of the current descriptor
--
-- @dimensionOrder@ — A permutation of the dimensions of the NDArray.                               dimensionOrder[i] must contain the new postion of dimenson i.                               Size of the array must be equal to the original number of dimensions in the descriptor.                               Must have all the indices in [0, numberOfDimensions) present uniquely.
--
-- This permutation is applied on top of whatever transpostions/permutations that may have been performed on the descriptor before.
--
-- ObjC selector: @- permuteWithDimensionOrder:@
permuteWithDimensionOrder :: IsMPSNDArrayDescriptor mpsndArrayDescriptor => mpsndArrayDescriptor -> Ptr CULong -> IO ()
permuteWithDimensionOrder mpsndArrayDescriptor dimensionOrder =
  sendMessage mpsndArrayDescriptor permuteWithDimensionOrderSelector dimensionOrder

-- | Returns the shape of the NDArray as MPSShape
--
-- The length of the array is the number of dimensions and the size of the fastest running dimension is the last element in the array.
--
-- ObjC selector: @- getShape@
getShape :: IsMPSNDArrayDescriptor mpsndArrayDescriptor => mpsndArrayDescriptor -> IO (Id NSArray)
getShape mpsndArrayDescriptor =
  sendMessage mpsndArrayDescriptor getShapeSelector

-- | Create an MPSNDArrayDescriptor object for a given size of dimensions.
--
-- Sample code:
--
-- // Creates an NDArrayDescriptor of dimensions [32, 6, 5, 3]
-- NSUInteger sizes[] = {3,5,6,32};
-- [ MPSNDArray descriptorWithDataType: MPSDataTypeFloat32
-- dimensionCount: 4
-- dimensionSizes: sizes ];    // array of numberOfDimensions dimensions. Starts with dimension 0
--
-- @dataType@ — MPSDataType of elements in the MPSNDArray
--
-- @numberOfDimensions@ — Number of dimensions in the NDArray. May not exceed 16.
--
-- @dimensionSizes@ — An array of NSUIntegers where dimension lengths provided by the user goes from fastest                                 moving to slowest moving dimension.                                 The product of all dimension lengths must be less than 2**31.                                 Additional system memory limits may apply
--
-- Returns: A valid MPSNDArrayDescriptor object or nil, if failure.
--
-- ObjC selector: @+ descriptorWithDataType:dimensionCount:dimensionSizes:@
descriptorWithDataType_dimensionCount_dimensionSizes :: MPSDataType -> CULong -> Ptr CULong -> IO (Id MPSNDArrayDescriptor)
descriptorWithDataType_dimensionCount_dimensionSizes dataType numberOfDimensions dimensionSizes =
  do
    cls' <- getRequiredClass "MPSNDArrayDescriptor"
    sendClassMessage cls' descriptorWithDataType_dimensionCount_dimensionSizesSelector dataType numberOfDimensions dimensionSizes

-- | A convenience function to create an MPSNDArrayDescriptor object for a given size of dimensions.
--
-- Sample code:
--
-- // Creates an NDArrayDescriptor of dimensions [32, 6, 5, 3]
-- NSArray<NSNumber *> sizes = {@32,@6,@5,@3};
-- [ MPSNDArray descriptorWithDataType: MPSDataTypeFloat32
-- shape: &sizes];
--
-- @dataType@ — MPSDataType of elements in the MPSNDArray
--
-- @shape@ — An array of NSUIntegers where dimension lengths provided by the user goes from slowest                                 moving to fastest moving dimension. This is same order as MLMultiArray in coreML and most frameworks in Python                                 The product of all dimension lengths must be less than 2**31.                                 Additional system memory limits may apply
--
-- Returns: A valid MPSNDArrayDescriptor object or nil, if failure.
--
-- ObjC selector: @+ descriptorWithDataType:shape:@
descriptorWithDataType_shape :: IsNSArray shape => MPSDataType -> shape -> IO (Id MPSNDArrayDescriptor)
descriptorWithDataType_shape dataType shape =
  do
    cls' <- getRequiredClass "MPSNDArrayDescriptor"
    sendClassMessage cls' descriptorWithDataType_shapeSelector dataType (toNSArray shape)

-- | Create an MPSNDArrayDescriptor object for a given size of dimensions.
--
-- Sample code:
--
-- // Creates an NDArrayDescriptor of dimensions [32, 5, 6, 3]
-- [ MPSNDArray descriptorWithDataType: MPSDataTypeFloat32
-- dimensionSizes: 3, 6, 5, 32, 0 //<--list terminator! ]; // array of numberOfDimensions dimensions. Starts with dimension 0
--
-- @dataType@ — MPSDataType of elements in the MPSNDArray
--
-- @dimension0@ — The start of a 0-terminated variadric list of NSUIntegers where dimension lengths provided by the user goes from fastest                                 moving to slowest moving dimension.                                 The product of all dimension sizes must be less than 2**31.                                 Additional system memory limits may apply
--
-- Returns: A valid MPSNDArrayDescriptor object or nil, if failure.
--
-- ObjC selector: @+ descriptorWithDataType:dimensionSizes:@
descriptorWithDataType_dimensionSizes :: MPSDataType -> CULong -> IO (Id MPSNDArrayDescriptor)
descriptorWithDataType_dimensionSizes dataType dimension0 =
  do
    cls' <- getRequiredClass "MPSNDArrayDescriptor"
    sendClassMessage cls' descriptorWithDataType_dimensionSizesSelector dataType dimension0

-- | Changes dimension sizes and number of dimensions on the current descriptor
--
-- @numberOfDimensions@ — Number of dimensions in the NDArray. May not exceed 16.
--
-- @dimensionSizes@ — An array of NSUIntegers where dimension lengths provided by the user goes from fastest                                 moving to slowest moving dimension.                                 The product of all dimension lengths must be less than 2**31.                                 Additional system memory limits may apply
--
-- ObjC selector: @- reshapeWithDimensionCount:dimensionSizes:@
reshapeWithDimensionCount_dimensionSizes :: IsMPSNDArrayDescriptor mpsndArrayDescriptor => mpsndArrayDescriptor -> CULong -> Ptr CULong -> IO ()
reshapeWithDimensionCount_dimensionSizes mpsndArrayDescriptor numberOfDimensions dimensionSizes =
  sendMessage mpsndArrayDescriptor reshapeWithDimensionCount_dimensionSizesSelector numberOfDimensions dimensionSizes

-- | Changes dimension sizes and number of dimensions on the current descriptor
--
-- @shape@ — An array of NSUIntegers where dimension lengths provided by the user goes from slowest                                 moving to fastest moving dimension. This is same order as MLMultiArray in coreML and most frameworks in Python                                 The product of all dimension lengths must be less than 2**31.                                 Additional system memory limits may apply
--
-- ObjC selector: @- reshapeWithShape:@
reshapeWithShape :: (IsMPSNDArrayDescriptor mpsndArrayDescriptor, IsNSArray shape) => mpsndArrayDescriptor -> shape -> IO ()
reshapeWithShape mpsndArrayDescriptor shape =
  sendMessage mpsndArrayDescriptor reshapeWithShapeSelector (toNSArray shape)

-- | Use -descriptorWithDataType:... instead
--
-- ObjC selector: @- init@
init_ :: IsMPSNDArrayDescriptor mpsndArrayDescriptor => mpsndArrayDescriptor -> IO (Id MPSNDArrayDescriptor)
init_ mpsndArrayDescriptor =
  sendOwnedMessage mpsndArrayDescriptor initSelector

-- | Data Type of the MPSNDArray elements
--
-- ObjC selector: @- dataType@
dataType :: IsMPSNDArrayDescriptor mpsndArrayDescriptor => mpsndArrayDescriptor -> IO MPSDataType
dataType mpsndArrayDescriptor =
  sendMessage mpsndArrayDescriptor dataTypeSelector

-- | Data Type of the MPSNDArray elements
--
-- ObjC selector: @- setDataType:@
setDataType :: IsMPSNDArrayDescriptor mpsndArrayDescriptor => mpsndArrayDescriptor -> MPSDataType -> IO ()
setDataType mpsndArrayDescriptor value =
  sendMessage mpsndArrayDescriptor setDataTypeSelector value

-- | The number of dimensions in the NDArray.
--
-- May not exceed 16. A 0-diumension MPSNDArray is a single scalar value.              Undefined dimensions are implicitly length 1.
--
-- ObjC selector: @- numberOfDimensions@
numberOfDimensions :: IsMPSNDArrayDescriptor mpsndArrayDescriptor => mpsndArrayDescriptor -> IO CULong
numberOfDimensions mpsndArrayDescriptor =
  sendMessage mpsndArrayDescriptor numberOfDimensionsSelector

-- | The number of dimensions in the NDArray.
--
-- May not exceed 16. A 0-diumension MPSNDArray is a single scalar value.              Undefined dimensions are implicitly length 1.
--
-- ObjC selector: @- setNumberOfDimensions:@
setNumberOfDimensions :: IsMPSNDArrayDescriptor mpsndArrayDescriptor => mpsndArrayDescriptor -> CULong -> IO ()
setNumberOfDimensions mpsndArrayDescriptor value =
  sendMessage mpsndArrayDescriptor setNumberOfDimensionsSelector value

-- | preferPackedRows
--
-- If YES, then new NDArrays created with this descriptor will pack the rows. Default: NO.
--
-- ObjC selector: @- preferPackedRows@
preferPackedRows :: IsMPSNDArrayDescriptor mpsndArrayDescriptor => mpsndArrayDescriptor -> IO Bool
preferPackedRows mpsndArrayDescriptor =
  sendMessage mpsndArrayDescriptor preferPackedRowsSelector

-- | preferPackedRows
--
-- If YES, then new NDArrays created with this descriptor will pack the rows. Default: NO.
--
-- ObjC selector: @- setPreferPackedRows:@
setPreferPackedRows :: IsMPSNDArrayDescriptor mpsndArrayDescriptor => mpsndArrayDescriptor -> Bool -> IO ()
setPreferPackedRows mpsndArrayDescriptor value =
  sendMessage mpsndArrayDescriptor setPreferPackedRowsSelector value

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @lengthOfDimension:@
lengthOfDimensionSelector :: Selector '[CULong] CULong
lengthOfDimensionSelector = mkSelector "lengthOfDimension:"

-- | @Selector@ for @sliceRangeForDimension:@
sliceRangeForDimensionSelector :: Selector '[CULong] MPSDimensionSlice
sliceRangeForDimensionSelector = mkSelector "sliceRangeForDimension:"

-- | @Selector@ for @sliceDimension:withSubrange:@
sliceDimension_withSubrangeSelector :: Selector '[CULong, MPSDimensionSlice] ()
sliceDimension_withSubrangeSelector = mkSelector "sliceDimension:withSubrange:"

-- | @Selector@ for @transposeDimension:withDimension:@
transposeDimension_withDimensionSelector :: Selector '[CULong, CULong] ()
transposeDimension_withDimensionSelector = mkSelector "transposeDimension:withDimension:"

-- | @Selector@ for @permuteWithDimensionOrder:@
permuteWithDimensionOrderSelector :: Selector '[Ptr CULong] ()
permuteWithDimensionOrderSelector = mkSelector "permuteWithDimensionOrder:"

-- | @Selector@ for @getShape@
getShapeSelector :: Selector '[] (Id NSArray)
getShapeSelector = mkSelector "getShape"

-- | @Selector@ for @descriptorWithDataType:dimensionCount:dimensionSizes:@
descriptorWithDataType_dimensionCount_dimensionSizesSelector :: Selector '[MPSDataType, CULong, Ptr CULong] (Id MPSNDArrayDescriptor)
descriptorWithDataType_dimensionCount_dimensionSizesSelector = mkSelector "descriptorWithDataType:dimensionCount:dimensionSizes:"

-- | @Selector@ for @descriptorWithDataType:shape:@
descriptorWithDataType_shapeSelector :: Selector '[MPSDataType, Id NSArray] (Id MPSNDArrayDescriptor)
descriptorWithDataType_shapeSelector = mkSelector "descriptorWithDataType:shape:"

-- | @Selector@ for @descriptorWithDataType:dimensionSizes:@
descriptorWithDataType_dimensionSizesSelector :: Selector '[MPSDataType, CULong] (Id MPSNDArrayDescriptor)
descriptorWithDataType_dimensionSizesSelector = mkSelector "descriptorWithDataType:dimensionSizes:"

-- | @Selector@ for @reshapeWithDimensionCount:dimensionSizes:@
reshapeWithDimensionCount_dimensionSizesSelector :: Selector '[CULong, Ptr CULong] ()
reshapeWithDimensionCount_dimensionSizesSelector = mkSelector "reshapeWithDimensionCount:dimensionSizes:"

-- | @Selector@ for @reshapeWithShape:@
reshapeWithShapeSelector :: Selector '[Id NSArray] ()
reshapeWithShapeSelector = mkSelector "reshapeWithShape:"

-- | @Selector@ for @init@
initSelector :: Selector '[] (Id MPSNDArrayDescriptor)
initSelector = mkSelector "init"

-- | @Selector@ for @dataType@
dataTypeSelector :: Selector '[] MPSDataType
dataTypeSelector = mkSelector "dataType"

-- | @Selector@ for @setDataType:@
setDataTypeSelector :: Selector '[MPSDataType] ()
setDataTypeSelector = mkSelector "setDataType:"

-- | @Selector@ for @numberOfDimensions@
numberOfDimensionsSelector :: Selector '[] CULong
numberOfDimensionsSelector = mkSelector "numberOfDimensions"

-- | @Selector@ for @setNumberOfDimensions:@
setNumberOfDimensionsSelector :: Selector '[CULong] ()
setNumberOfDimensionsSelector = mkSelector "setNumberOfDimensions:"

-- | @Selector@ for @preferPackedRows@
preferPackedRowsSelector :: Selector '[] Bool
preferPackedRowsSelector = mkSelector "preferPackedRows"

-- | @Selector@ for @setPreferPackedRows:@
setPreferPackedRowsSelector :: Selector '[Bool] ()
setPreferPackedRowsSelector = mkSelector "setPreferPackedRows:"

