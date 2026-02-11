{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | The representation of a compute data type.
--
-- Pass data to a graph using a tensor data, a reference will be taken to your data and used just in time when the graph is run.
--
-- Generated bindings for @MPSGraphTensorData@.
module ObjC.MetalPerformanceShadersGraph.MPSGraphTensorData
  ( MPSGraphTensorData
  , IsMPSGraphTensorData(..)
  , initWithDevice_data_shape_dataType
  , initWithMTLBuffer_shape_dataType
  , initWithMTLBuffer_shape_dataType_rowBytes
  , initWithMPSMatrix
  , initWithMPSMatrix_rank
  , initWithMPSVector
  , initWithMPSVector_rank
  , initWithMPSNDArray
  , initWithMPSImageBatch
  , initWithMTLTensor
  , mpsndarray
  , shape
  , dataType
  , device
  , initWithDevice_data_shape_dataTypeSelector
  , initWithMTLBuffer_shape_dataTypeSelector
  , initWithMTLBuffer_shape_dataType_rowBytesSelector
  , initWithMPSMatrixSelector
  , initWithMPSMatrix_rankSelector
  , initWithMPSVectorSelector
  , initWithMPSVector_rankSelector
  , initWithMPSNDArraySelector
  , initWithMPSImageBatchSelector
  , initWithMTLTensorSelector
  , mpsndarraySelector
  , shapeSelector
  , dataTypeSelector
  , deviceSelector

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

import ObjC.MetalPerformanceShadersGraph.Internal.Classes
import ObjC.MetalPerformanceShaders.Internal.Enums
import ObjC.Foundation.Internal.Classes
import ObjC.MetalPerformanceShaders.Internal.Classes

-- | Initializes the tensor data with an @NSData@ on a device.
--
-- - Parameters:   - device: MPSDevice on which the MPSGraphTensorData exists   - data: NSData from which to copy the contents   - shape: shape of the output tensor   - dataType: dataType of the placeholder tensor - Returns: A valid MPSGraphTensorData, or nil if allocation failure.
--
-- ObjC selector: @- initWithDevice:data:shape:dataType:@
initWithDevice_data_shape_dataType :: (IsMPSGraphTensorData mpsGraphTensorData, IsMPSGraphDevice device, IsNSData data_) => mpsGraphTensorData -> device -> data_ -> RawId -> MPSDataType -> IO (Id MPSGraphTensorData)
initWithDevice_data_shape_dataType mpsGraphTensorData  device data_ shape dataType =
  withObjCPtr device $ \raw_device ->
    withObjCPtr data_ $ \raw_data_ ->
        sendMsg mpsGraphTensorData (mkSelector "initWithDevice:data:shape:dataType:") (retPtr retVoid) [argPtr (castPtr raw_device :: Ptr ()), argPtr (castPtr raw_data_ :: Ptr ()), argPtr (castPtr (unRawId shape) :: Ptr ()), argCUInt (coerce dataType)] >>= ownedObject . castPtr

-- | Initializes an tensor data with a metal buffer.
--
-- The device of the MTLBuffer will be used to get the MPSDevice for this MPSGraphTensorData.
--
-- - Parameters:   - buffer: MTLBuffer to be used within the MPSGraphTensorData   - shape: shape of the output tensor   - dataType: dataType of the placeholder tensor - Returns: A valid MPSGraphTensorData, or nil if allocation failure.
--
-- ObjC selector: @- initWithMTLBuffer:shape:dataType:@
initWithMTLBuffer_shape_dataType :: IsMPSGraphTensorData mpsGraphTensorData => mpsGraphTensorData -> RawId -> RawId -> MPSDataType -> IO (Id MPSGraphTensorData)
initWithMTLBuffer_shape_dataType mpsGraphTensorData  buffer shape dataType =
    sendMsg mpsGraphTensorData (mkSelector "initWithMTLBuffer:shape:dataType:") (retPtr retVoid) [argPtr (castPtr (unRawId buffer) :: Ptr ()), argPtr (castPtr (unRawId shape) :: Ptr ()), argCUInt (coerce dataType)] >>= ownedObject . castPtr

-- | Initializes an tensor data with a metal buffer.
--
-- The device of the MTLBuffer will be used to get the MPSDevice for this MPSGraphTensorData.
--
-- - Parameters:   - buffer: MTLBuffer to be used within the MPSGraphTensorData   - shape: shape of the output tensor   - dataType: dataType of the placeholder tensor   - rowBytes: rowBytes for the fastest moving dimension, must be larger than or equal to sizeOf(dataType)shape[rank - 1] and must be a multiple of sizeOf(dataType) - Returns: A valid MPSGraphTensorData, or nil if allocation failure.
--
-- ObjC selector: @- initWithMTLBuffer:shape:dataType:rowBytes:@
initWithMTLBuffer_shape_dataType_rowBytes :: IsMPSGraphTensorData mpsGraphTensorData => mpsGraphTensorData -> RawId -> RawId -> MPSDataType -> CULong -> IO (Id MPSGraphTensorData)
initWithMTLBuffer_shape_dataType_rowBytes mpsGraphTensorData  buffer shape dataType rowBytes =
    sendMsg mpsGraphTensorData (mkSelector "initWithMTLBuffer:shape:dataType:rowBytes:") (retPtr retVoid) [argPtr (castPtr (unRawId buffer) :: Ptr ()), argPtr (castPtr (unRawId shape) :: Ptr ()), argCUInt (coerce dataType), argCULong rowBytes] >>= ownedObject . castPtr

-- | Initializes a tensor data with an MPS matrix.
--
-- The device of the MPSMatrix will be used to get the MPSDevice for this MPSGraphTensorData.
--
-- - Parameters:   - matrix: MPSMatrix to be used within the MPSGraphTensorData - Returns: A valid MPSGraphTensorData, or nil if allocation failure.
--
-- ObjC selector: @- initWithMPSMatrix:@
initWithMPSMatrix :: (IsMPSGraphTensorData mpsGraphTensorData, IsMPSMatrix matrix) => mpsGraphTensorData -> matrix -> IO (Id MPSGraphTensorData)
initWithMPSMatrix mpsGraphTensorData  matrix =
  withObjCPtr matrix $ \raw_matrix ->
      sendMsg mpsGraphTensorData (mkSelector "initWithMPSMatrix:") (retPtr retVoid) [argPtr (castPtr raw_matrix :: Ptr ())] >>= ownedObject . castPtr

-- | Initializes a tensor data with an MPS matrix enforcing rank of the result.
--
-- The device of the MPSMatrix will be used to get the MPSDevice for this MPSGraphTensorData.
--
-- - Parameters:   - matrix: MPSMatrix to be used within the MPSGraphTensorData   - rank: The rank of the resulting TensorData tensor. NOTE: must be within { 1, ... ,16 }. - Returns: A valid MPSGraphTensorData of given rank, or nil if allocation failure.
--
-- ObjC selector: @- initWithMPSMatrix:rank:@
initWithMPSMatrix_rank :: (IsMPSGraphTensorData mpsGraphTensorData, IsMPSMatrix matrix) => mpsGraphTensorData -> matrix -> CULong -> IO (Id MPSGraphTensorData)
initWithMPSMatrix_rank mpsGraphTensorData  matrix rank =
  withObjCPtr matrix $ \raw_matrix ->
      sendMsg mpsGraphTensorData (mkSelector "initWithMPSMatrix:rank:") (retPtr retVoid) [argPtr (castPtr raw_matrix :: Ptr ()), argCULong rank] >>= ownedObject . castPtr

-- | Initializes a tensor data with an MPS vector.
--
-- The device of the MPSVector will be used to get the MPSDevice for this MPSGraphTensorData.
--
-- - Parameters:   - vector: MPSVector to be used within the MPSGraphTensorData - Returns: A valid MPSGraphTensorData, or nil if allocation failure.
--
-- ObjC selector: @- initWithMPSVector:@
initWithMPSVector :: (IsMPSGraphTensorData mpsGraphTensorData, IsMPSVector vector) => mpsGraphTensorData -> vector -> IO (Id MPSGraphTensorData)
initWithMPSVector mpsGraphTensorData  vector =
  withObjCPtr vector $ \raw_vector ->
      sendMsg mpsGraphTensorData (mkSelector "initWithMPSVector:") (retPtr retVoid) [argPtr (castPtr raw_vector :: Ptr ())] >>= ownedObject . castPtr

-- | Initializes a tensor data with an MPS vector enforcing rank of the result.
--
-- The device of the MPSVector will be used to get the MPSDevice for this MPSGraphTensorData.
--
-- - Parameters:   - vector: MPSVector to be used within the MPSGraphTensorData   - rank: The rank of the resulting TensorData tensor. NOTE: must be within { 1, ... ,16 }. - Returns: A valid MPSGraphTensorData, or nil if allocation failure.
--
-- ObjC selector: @- initWithMPSVector:rank:@
initWithMPSVector_rank :: (IsMPSGraphTensorData mpsGraphTensorData, IsMPSVector vector) => mpsGraphTensorData -> vector -> CULong -> IO (Id MPSGraphTensorData)
initWithMPSVector_rank mpsGraphTensorData  vector rank =
  withObjCPtr vector $ \raw_vector ->
      sendMsg mpsGraphTensorData (mkSelector "initWithMPSVector:rank:") (retPtr retVoid) [argPtr (castPtr raw_vector :: Ptr ()), argCULong rank] >>= ownedObject . castPtr

-- | Initializes an MPSGraphTensorData with an MPS ndarray.
--
-- The device of the MPSNDArray will be used to get the MPSDevice for this MPSGraphTensorData.
--
-- - Parameters:   - ndarray: MPSNDArray to be used within the MPSGraphTensorData. - Returns: A valid MPSGraphTensorData, or nil if allocation failure.
--
-- ObjC selector: @- initWithMPSNDArray:@
initWithMPSNDArray :: (IsMPSGraphTensorData mpsGraphTensorData, IsMPSNDArray ndarray) => mpsGraphTensorData -> ndarray -> IO (Id MPSGraphTensorData)
initWithMPSNDArray mpsGraphTensorData  ndarray =
  withObjCPtr ndarray $ \raw_ndarray ->
      sendMsg mpsGraphTensorData (mkSelector "initWithMPSNDArray:") (retPtr retVoid) [argPtr (castPtr raw_ndarray :: Ptr ())] >>= ownedObject . castPtr

-- | Initializes a tensor data with an MPS image batch.
--
-- The dataLayout used will be NHWC, call a transpose or permute to change to a layout of your choice.
--
-- - Parameters:   - imageBatch: The device on which the kernel will run, unorm8 and unorm16 images will create a float32 tensorData - Returns: A valid MPSGraphTensorData, or nil if allocation failure.
--
-- ObjC selector: @- initWithMPSImageBatch:@
initWithMPSImageBatch :: IsMPSGraphTensorData mpsGraphTensorData => mpsGraphTensorData -> RawId -> IO (Id MPSGraphTensorData)
initWithMPSImageBatch mpsGraphTensorData  imageBatch =
    sendMsg mpsGraphTensorData (mkSelector "initWithMPSImageBatch:") (retPtr retVoid) [argPtr (castPtr (unRawId imageBatch) :: Ptr ())] >>= ownedObject . castPtr

-- | Initializes an MPSGraphTensorData with an MTLTensor.
--
-- The internal storage of the MTLTensor will be aliased. Requires tensor to support MTLTensorUsageMachineLearning.
--
-- - Parameters:   - tensor: MTLTensor to be used within the MPSGraphTensorData - Returns: A valid MPSGraphTensorData, or nil if allocation failure.
--
-- ObjC selector: @- initWithMTLTensor:@
initWithMTLTensor :: IsMPSGraphTensorData mpsGraphTensorData => mpsGraphTensorData -> RawId -> IO (Id MPSGraphTensorData)
initWithMTLTensor mpsGraphTensorData  tensor =
    sendMsg mpsGraphTensorData (mkSelector "initWithMTLTensor:") (retPtr retVoid) [argPtr (castPtr (unRawId tensor) :: Ptr ())] >>= ownedObject . castPtr

-- | Return an mpsndarray object will copy contents if the contents are not stored in an MPS ndarray.
--
-- - Returns: A valid MPSNDArray, or nil if allocation fails.
--
-- ObjC selector: @- mpsndarray@
mpsndarray :: IsMPSGraphTensorData mpsGraphTensorData => mpsGraphTensorData -> IO (Id MPSNDArray)
mpsndarray mpsGraphTensorData  =
    sendMsg mpsGraphTensorData (mkSelector "mpsndarray") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | The shape of the tensor data.
--
-- ObjC selector: @- shape@
shape :: IsMPSGraphTensorData mpsGraphTensorData => mpsGraphTensorData -> IO RawId
shape mpsGraphTensorData  =
    fmap (RawId . castPtr) $ sendMsg mpsGraphTensorData (mkSelector "shape") (retPtr retVoid) []

-- | The data type of the tensor data.
--
-- ObjC selector: @- dataType@
dataType :: IsMPSGraphTensorData mpsGraphTensorData => mpsGraphTensorData -> IO MPSDataType
dataType mpsGraphTensorData  =
    fmap (coerce :: CUInt -> MPSDataType) $ sendMsg mpsGraphTensorData (mkSelector "dataType") retCUInt []

-- | The device of the tensor data.
--
-- ObjC selector: @- device@
device :: IsMPSGraphTensorData mpsGraphTensorData => mpsGraphTensorData -> IO (Id MPSGraphDevice)
device mpsGraphTensorData  =
    sendMsg mpsGraphTensorData (mkSelector "device") (retPtr retVoid) [] >>= retainedObject . castPtr

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @initWithDevice:data:shape:dataType:@
initWithDevice_data_shape_dataTypeSelector :: Selector
initWithDevice_data_shape_dataTypeSelector = mkSelector "initWithDevice:data:shape:dataType:"

-- | @Selector@ for @initWithMTLBuffer:shape:dataType:@
initWithMTLBuffer_shape_dataTypeSelector :: Selector
initWithMTLBuffer_shape_dataTypeSelector = mkSelector "initWithMTLBuffer:shape:dataType:"

-- | @Selector@ for @initWithMTLBuffer:shape:dataType:rowBytes:@
initWithMTLBuffer_shape_dataType_rowBytesSelector :: Selector
initWithMTLBuffer_shape_dataType_rowBytesSelector = mkSelector "initWithMTLBuffer:shape:dataType:rowBytes:"

-- | @Selector@ for @initWithMPSMatrix:@
initWithMPSMatrixSelector :: Selector
initWithMPSMatrixSelector = mkSelector "initWithMPSMatrix:"

-- | @Selector@ for @initWithMPSMatrix:rank:@
initWithMPSMatrix_rankSelector :: Selector
initWithMPSMatrix_rankSelector = mkSelector "initWithMPSMatrix:rank:"

-- | @Selector@ for @initWithMPSVector:@
initWithMPSVectorSelector :: Selector
initWithMPSVectorSelector = mkSelector "initWithMPSVector:"

-- | @Selector@ for @initWithMPSVector:rank:@
initWithMPSVector_rankSelector :: Selector
initWithMPSVector_rankSelector = mkSelector "initWithMPSVector:rank:"

-- | @Selector@ for @initWithMPSNDArray:@
initWithMPSNDArraySelector :: Selector
initWithMPSNDArraySelector = mkSelector "initWithMPSNDArray:"

-- | @Selector@ for @initWithMPSImageBatch:@
initWithMPSImageBatchSelector :: Selector
initWithMPSImageBatchSelector = mkSelector "initWithMPSImageBatch:"

-- | @Selector@ for @initWithMTLTensor:@
initWithMTLTensorSelector :: Selector
initWithMTLTensorSelector = mkSelector "initWithMTLTensor:"

-- | @Selector@ for @mpsndarray@
mpsndarraySelector :: Selector
mpsndarraySelector = mkSelector "mpsndarray"

-- | @Selector@ for @shape@
shapeSelector :: Selector
shapeSelector = mkSelector "shape"

-- | @Selector@ for @dataType@
dataTypeSelector :: Selector
dataTypeSelector = mkSelector "dataType"

-- | @Selector@ for @device@
deviceSelector :: Selector
deviceSelector = mkSelector "device"

