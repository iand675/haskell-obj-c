{-# LANGUAGE PatternSynonyms #-}
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
  , lengthOfDimensionSelector
  , sliceRangeForDimensionSelector
  , sliceDimension_withSubrangeSelector
  , transposeDimension_withDimensionSelector
  , permuteWithDimensionOrderSelector
  , getShapeSelector
  , descriptorWithDataType_dimensionCount_dimensionSizesSelector
  , descriptorWithDataType_shapeSelector
  , descriptorWithDataType_dimensionSizesSelector
  , reshapeWithDimensionCount_dimensionSizesSelector
  , reshapeWithShapeSelector
  , initSelector
  , dataTypeSelector
  , setDataTypeSelector
  , numberOfDimensionsSelector
  , setNumberOfDimensionsSelector
  , preferPackedRowsSelector
  , setPreferPackedRowsSelector

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
import ObjC.Runtime.MsgSend (sendMsg, sendClassMsg, sendMsgStret, sendClassMsgStret)
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
lengthOfDimension mpsndArrayDescriptor  dimensionIndex =
  sendMsg mpsndArrayDescriptor (mkSelector "lengthOfDimension:") retCULong [argCULong (fromIntegral dimensionIndex)]

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
sliceRangeForDimension mpsndArrayDescriptor  dimensionIndex =
  sendMsgStret mpsndArrayDescriptor (mkSelector "sliceRangeForDimension:") retMPSDimensionSlice [argCULong (fromIntegral dimensionIndex)]

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
sliceDimension_withSubrange mpsndArrayDescriptor  dimensionIndex subRange =
  sendMsg mpsndArrayDescriptor (mkSelector "sliceDimension:withSubrange:") retVoid [argCULong (fromIntegral dimensionIndex), argMPSDimensionSlice subRange]

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
transposeDimension_withDimension mpsndArrayDescriptor  dimensionIndex dimensionIndex2 =
  sendMsg mpsndArrayDescriptor (mkSelector "transposeDimension:withDimension:") retVoid [argCULong (fromIntegral dimensionIndex), argCULong (fromIntegral dimensionIndex2)]

-- | Permutes the dimensions of the current descriptor
--
-- @dimensionOrder@ — A permutation of the dimensions of the NDArray.                               dimensionOrder[i] must contain the new postion of dimenson i.                               Size of the array must be equal to the original number of dimensions in the descriptor.                               Must have all the indices in [0, numberOfDimensions) present uniquely.
--
-- This permutation is applied on top of whatever transpostions/permutations that may have been performed on the descriptor before.
--
-- ObjC selector: @- permuteWithDimensionOrder:@
permuteWithDimensionOrder :: IsMPSNDArrayDescriptor mpsndArrayDescriptor => mpsndArrayDescriptor -> Ptr CULong -> IO ()
permuteWithDimensionOrder mpsndArrayDescriptor  dimensionOrder =
  sendMsg mpsndArrayDescriptor (mkSelector "permuteWithDimensionOrder:") retVoid [argPtr dimensionOrder]

-- | Returns the shape of the NDArray as MPSShape
--
-- The length of the array is the number of dimensions and the size of the fastest running dimension is the last element in the array.
--
-- ObjC selector: @- getShape@
getShape :: IsMPSNDArrayDescriptor mpsndArrayDescriptor => mpsndArrayDescriptor -> IO (Id NSArray)
getShape mpsndArrayDescriptor  =
  sendMsg mpsndArrayDescriptor (mkSelector "getShape") (retPtr retVoid) [] >>= retainedObject . castPtr

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
    sendClassMsg cls' (mkSelector "descriptorWithDataType:dimensionCount:dimensionSizes:") (retPtr retVoid) [argCUInt (coerce dataType), argCULong (fromIntegral numberOfDimensions), argPtr dimensionSizes] >>= retainedObject . castPtr

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
    withObjCPtr shape $ \raw_shape ->
      sendClassMsg cls' (mkSelector "descriptorWithDataType:shape:") (retPtr retVoid) [argCUInt (coerce dataType), argPtr (castPtr raw_shape :: Ptr ())] >>= retainedObject . castPtr

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
    sendClassMsg cls' (mkSelector "descriptorWithDataType:dimensionSizes:") (retPtr retVoid) [argCUInt (coerce dataType), argCULong (fromIntegral dimension0)] >>= retainedObject . castPtr

-- | Changes dimension sizes and number of dimensions on the current descriptor
--
-- @numberOfDimensions@ — Number of dimensions in the NDArray. May not exceed 16.
--
-- @dimensionSizes@ — An array of NSUIntegers where dimension lengths provided by the user goes from fastest                                 moving to slowest moving dimension.                                 The product of all dimension lengths must be less than 2**31.                                 Additional system memory limits may apply
--
-- ObjC selector: @- reshapeWithDimensionCount:dimensionSizes:@
reshapeWithDimensionCount_dimensionSizes :: IsMPSNDArrayDescriptor mpsndArrayDescriptor => mpsndArrayDescriptor -> CULong -> Ptr CULong -> IO ()
reshapeWithDimensionCount_dimensionSizes mpsndArrayDescriptor  numberOfDimensions dimensionSizes =
  sendMsg mpsndArrayDescriptor (mkSelector "reshapeWithDimensionCount:dimensionSizes:") retVoid [argCULong (fromIntegral numberOfDimensions), argPtr dimensionSizes]

-- | Changes dimension sizes and number of dimensions on the current descriptor
--
-- @shape@ — An array of NSUIntegers where dimension lengths provided by the user goes from slowest                                 moving to fastest moving dimension. This is same order as MLMultiArray in coreML and most frameworks in Python                                 The product of all dimension lengths must be less than 2**31.                                 Additional system memory limits may apply
--
-- ObjC selector: @- reshapeWithShape:@
reshapeWithShape :: (IsMPSNDArrayDescriptor mpsndArrayDescriptor, IsNSArray shape) => mpsndArrayDescriptor -> shape -> IO ()
reshapeWithShape mpsndArrayDescriptor  shape =
withObjCPtr shape $ \raw_shape ->
    sendMsg mpsndArrayDescriptor (mkSelector "reshapeWithShape:") retVoid [argPtr (castPtr raw_shape :: Ptr ())]

-- | Use -descriptorWithDataType:... instead
--
-- ObjC selector: @- init@
init_ :: IsMPSNDArrayDescriptor mpsndArrayDescriptor => mpsndArrayDescriptor -> IO (Id MPSNDArrayDescriptor)
init_ mpsndArrayDescriptor  =
  sendMsg mpsndArrayDescriptor (mkSelector "init") (retPtr retVoid) [] >>= ownedObject . castPtr

-- | Data Type of the MPSNDArray elements
--
-- ObjC selector: @- dataType@
dataType :: IsMPSNDArrayDescriptor mpsndArrayDescriptor => mpsndArrayDescriptor -> IO MPSDataType
dataType mpsndArrayDescriptor  =
  fmap (coerce :: CUInt -> MPSDataType) $ sendMsg mpsndArrayDescriptor (mkSelector "dataType") retCUInt []

-- | Data Type of the MPSNDArray elements
--
-- ObjC selector: @- setDataType:@
setDataType :: IsMPSNDArrayDescriptor mpsndArrayDescriptor => mpsndArrayDescriptor -> MPSDataType -> IO ()
setDataType mpsndArrayDescriptor  value =
  sendMsg mpsndArrayDescriptor (mkSelector "setDataType:") retVoid [argCUInt (coerce value)]

-- | The number of dimensions in the NDArray.
--
-- May not exceed 16. A 0-diumension MPSNDArray is a single scalar value.              Undefined dimensions are implicitly length 1.
--
-- ObjC selector: @- numberOfDimensions@
numberOfDimensions :: IsMPSNDArrayDescriptor mpsndArrayDescriptor => mpsndArrayDescriptor -> IO CULong
numberOfDimensions mpsndArrayDescriptor  =
  sendMsg mpsndArrayDescriptor (mkSelector "numberOfDimensions") retCULong []

-- | The number of dimensions in the NDArray.
--
-- May not exceed 16. A 0-diumension MPSNDArray is a single scalar value.              Undefined dimensions are implicitly length 1.
--
-- ObjC selector: @- setNumberOfDimensions:@
setNumberOfDimensions :: IsMPSNDArrayDescriptor mpsndArrayDescriptor => mpsndArrayDescriptor -> CULong -> IO ()
setNumberOfDimensions mpsndArrayDescriptor  value =
  sendMsg mpsndArrayDescriptor (mkSelector "setNumberOfDimensions:") retVoid [argCULong (fromIntegral value)]

-- | preferPackedRows
--
-- If YES, then new NDArrays created with this descriptor will pack the rows. Default: NO.
--
-- ObjC selector: @- preferPackedRows@
preferPackedRows :: IsMPSNDArrayDescriptor mpsndArrayDescriptor => mpsndArrayDescriptor -> IO Bool
preferPackedRows mpsndArrayDescriptor  =
  fmap ((/= 0) :: CULong -> Bool) $ sendMsg mpsndArrayDescriptor (mkSelector "preferPackedRows") retCULong []

-- | preferPackedRows
--
-- If YES, then new NDArrays created with this descriptor will pack the rows. Default: NO.
--
-- ObjC selector: @- setPreferPackedRows:@
setPreferPackedRows :: IsMPSNDArrayDescriptor mpsndArrayDescriptor => mpsndArrayDescriptor -> Bool -> IO ()
setPreferPackedRows mpsndArrayDescriptor  value =
  sendMsg mpsndArrayDescriptor (mkSelector "setPreferPackedRows:") retVoid [argCULong (if value then 1 else 0)]

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @lengthOfDimension:@
lengthOfDimensionSelector :: Selector
lengthOfDimensionSelector = mkSelector "lengthOfDimension:"

-- | @Selector@ for @sliceRangeForDimension:@
sliceRangeForDimensionSelector :: Selector
sliceRangeForDimensionSelector = mkSelector "sliceRangeForDimension:"

-- | @Selector@ for @sliceDimension:withSubrange:@
sliceDimension_withSubrangeSelector :: Selector
sliceDimension_withSubrangeSelector = mkSelector "sliceDimension:withSubrange:"

-- | @Selector@ for @transposeDimension:withDimension:@
transposeDimension_withDimensionSelector :: Selector
transposeDimension_withDimensionSelector = mkSelector "transposeDimension:withDimension:"

-- | @Selector@ for @permuteWithDimensionOrder:@
permuteWithDimensionOrderSelector :: Selector
permuteWithDimensionOrderSelector = mkSelector "permuteWithDimensionOrder:"

-- | @Selector@ for @getShape@
getShapeSelector :: Selector
getShapeSelector = mkSelector "getShape"

-- | @Selector@ for @descriptorWithDataType:dimensionCount:dimensionSizes:@
descriptorWithDataType_dimensionCount_dimensionSizesSelector :: Selector
descriptorWithDataType_dimensionCount_dimensionSizesSelector = mkSelector "descriptorWithDataType:dimensionCount:dimensionSizes:"

-- | @Selector@ for @descriptorWithDataType:shape:@
descriptorWithDataType_shapeSelector :: Selector
descriptorWithDataType_shapeSelector = mkSelector "descriptorWithDataType:shape:"

-- | @Selector@ for @descriptorWithDataType:dimensionSizes:@
descriptorWithDataType_dimensionSizesSelector :: Selector
descriptorWithDataType_dimensionSizesSelector = mkSelector "descriptorWithDataType:dimensionSizes:"

-- | @Selector@ for @reshapeWithDimensionCount:dimensionSizes:@
reshapeWithDimensionCount_dimensionSizesSelector :: Selector
reshapeWithDimensionCount_dimensionSizesSelector = mkSelector "reshapeWithDimensionCount:dimensionSizes:"

-- | @Selector@ for @reshapeWithShape:@
reshapeWithShapeSelector :: Selector
reshapeWithShapeSelector = mkSelector "reshapeWithShape:"

-- | @Selector@ for @init@
initSelector :: Selector
initSelector = mkSelector "init"

-- | @Selector@ for @dataType@
dataTypeSelector :: Selector
dataTypeSelector = mkSelector "dataType"

-- | @Selector@ for @setDataType:@
setDataTypeSelector :: Selector
setDataTypeSelector = mkSelector "setDataType:"

-- | @Selector@ for @numberOfDimensions@
numberOfDimensionsSelector :: Selector
numberOfDimensionsSelector = mkSelector "numberOfDimensions"

-- | @Selector@ for @setNumberOfDimensions:@
setNumberOfDimensionsSelector :: Selector
setNumberOfDimensionsSelector = mkSelector "setNumberOfDimensions:"

-- | @Selector@ for @preferPackedRows@
preferPackedRowsSelector :: Selector
preferPackedRowsSelector = mkSelector "preferPackedRows"

-- | @Selector@ for @setPreferPackedRows:@
setPreferPackedRowsSelector :: Selector
setPreferPackedRowsSelector = mkSelector "setPreferPackedRows:"

