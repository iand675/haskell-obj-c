{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | MLCTensor
--
-- A tensor object
--
-- Generated bindings for @MLCTensor@.
module ObjC.MLCompute.MLCTensor
  ( MLCTensor
  , IsMLCTensor(..)
  , new
  , init_
  , tensorWithDescriptor
  , tensorWithDescriptor_randomInitializerType
  , tensorWithDescriptor_fillWithData
  , tensorWithDescriptor_data
  , tensorWithShape
  , tensorWithShape_randomInitializerType
  , tensorWithShape_randomInitializerType_dataType
  , tensorWithShape_dataType
  , tensorWithShape_data_dataType
  , tensorWithShape_fillWithData_dataType
  , tensorWithWidth_height_featureChannelCount_batchSize
  , tensorWithWidth_height_featureChannelCount_batchSize_fillWithData_dataType
  , tensorWithWidth_height_featureChannelCount_batchSize_randomInitializerType
  , tensorWithWidth_height_featureChannelCount_batchSize_data
  , tensorWithWidth_height_featureChannelCount_batchSize_data_dataType
  , tensorWithSequenceLength_featureChannelCount_batchSize
  , tensorWithSequenceLength_featureChannelCount_batchSize_randomInitializerType
  , tensorWithSequenceLength_featureChannelCount_batchSize_data
  , tensorWithSequenceLengths_sortedSequences_featureChannelCount_batchSize_randomInitializerType
  , tensorWithSequenceLengths_sortedSequences_featureChannelCount_batchSize_data
  , synchronizeData
  , synchronizeOptimizerData
  , copyDataFromDeviceMemoryToBytes_length_synchronizeWithDevice
  , bindAndWriteData_toDevice
  , bindOptimizerData_deviceData
  , tensorByQuantizingToType_scale_bias
  , tensorByQuantizingToType_scale_bias_axis
  , tensorByDequantizingToType_scale_bias
  , tensorByDequantizingToType_scale_bias_axis
  , tensorID
  , descriptor
  , data_
  , label
  , setLabel
  , device
  , optimizerData
  , optimizerDeviceData
  , hasValidNumerics
  , newSelector
  , initSelector
  , tensorWithDescriptorSelector
  , tensorWithDescriptor_randomInitializerTypeSelector
  , tensorWithDescriptor_fillWithDataSelector
  , tensorWithDescriptor_dataSelector
  , tensorWithShapeSelector
  , tensorWithShape_randomInitializerTypeSelector
  , tensorWithShape_randomInitializerType_dataTypeSelector
  , tensorWithShape_dataTypeSelector
  , tensorWithShape_data_dataTypeSelector
  , tensorWithShape_fillWithData_dataTypeSelector
  , tensorWithWidth_height_featureChannelCount_batchSizeSelector
  , tensorWithWidth_height_featureChannelCount_batchSize_fillWithData_dataTypeSelector
  , tensorWithWidth_height_featureChannelCount_batchSize_randomInitializerTypeSelector
  , tensorWithWidth_height_featureChannelCount_batchSize_dataSelector
  , tensorWithWidth_height_featureChannelCount_batchSize_data_dataTypeSelector
  , tensorWithSequenceLength_featureChannelCount_batchSizeSelector
  , tensorWithSequenceLength_featureChannelCount_batchSize_randomInitializerTypeSelector
  , tensorWithSequenceLength_featureChannelCount_batchSize_dataSelector
  , tensorWithSequenceLengths_sortedSequences_featureChannelCount_batchSize_randomInitializerTypeSelector
  , tensorWithSequenceLengths_sortedSequences_featureChannelCount_batchSize_dataSelector
  , synchronizeDataSelector
  , synchronizeOptimizerDataSelector
  , copyDataFromDeviceMemoryToBytes_length_synchronizeWithDeviceSelector
  , bindAndWriteData_toDeviceSelector
  , bindOptimizerData_deviceDataSelector
  , tensorByQuantizingToType_scale_biasSelector
  , tensorByQuantizingToType_scale_bias_axisSelector
  , tensorByDequantizingToType_scale_biasSelector
  , tensorByDequantizingToType_scale_bias_axisSelector
  , tensorIDSelector
  , descriptorSelector
  , dataSelector
  , labelSelector
  , setLabelSelector
  , deviceSelector
  , optimizerDataSelector
  , optimizerDeviceDataSelector
  , hasValidNumericsSelector

  -- * Enum types
  , MLCDataType(MLCDataType)
  , pattern MLCDataTypeInvalid
  , pattern MLCDataTypeFloat32
  , pattern MLCDataTypeFloat16
  , pattern MLCDataTypeBoolean
  , pattern MLCDataTypeInt64
  , pattern MLCDataTypeInt32
  , pattern MLCDataTypeInt8
  , pattern MLCDataTypeUInt8
  , pattern MLCDataTypeCount
  , MLCRandomInitializerType(MLCRandomInitializerType)
  , pattern MLCRandomInitializerTypeInvalid
  , pattern MLCRandomInitializerTypeUniform
  , pattern MLCRandomInitializerTypeGlorotUniform
  , pattern MLCRandomInitializerTypeXavier
  , pattern MLCRandomInitializerTypeCount

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

import ObjC.MLCompute.Internal.Classes
import ObjC.MLCompute.Internal.Enums
import ObjC.Foundation.Internal.Classes

-- | @+ new@
new :: IO (Id MLCTensor)
new  =
  do
    cls' <- getRequiredClass "MLCTensor"
    sendClassMsg cls' (mkSelector "new") (retPtr retVoid) [] >>= ownedObject . castPtr

-- | @- init@
init_ :: IsMLCTensor mlcTensor => mlcTensor -> IO (Id MLCTensor)
init_ mlcTensor  =
  sendMsg mlcTensor (mkSelector "init") (retPtr retVoid) [] >>= ownedObject . castPtr

-- | Create a MLCTensor object
--
-- Create a tensor object without any data
--
-- Returns: A new MLCTensor object
--
-- ObjC selector: @+ tensorWithDescriptor:@
tensorWithDescriptor :: IsMLCTensorDescriptor tensorDescriptor => tensorDescriptor -> IO (Id MLCTensor)
tensorWithDescriptor tensorDescriptor =
  do
    cls' <- getRequiredClass "MLCTensor"
    withObjCPtr tensorDescriptor $ \raw_tensorDescriptor ->
      sendClassMsg cls' (mkSelector "tensorWithDescriptor:") (retPtr retVoid) [argPtr (castPtr raw_tensorDescriptor :: Ptr ())] >>= retainedObject . castPtr

-- | Create a MLCTensor object
--
-- Create a tensor object initialized with a random initializer such as Glorot Uniform.
--
-- @tensorDescriptor@ — The tensor descriptor
--
-- @randomInitializerType@ — The random initializer type
--
-- Returns: A new MLCTensor object
--
-- ObjC selector: @+ tensorWithDescriptor:randomInitializerType:@
tensorWithDescriptor_randomInitializerType :: IsMLCTensorDescriptor tensorDescriptor => tensorDescriptor -> MLCRandomInitializerType -> IO (Id MLCTensor)
tensorWithDescriptor_randomInitializerType tensorDescriptor randomInitializerType =
  do
    cls' <- getRequiredClass "MLCTensor"
    withObjCPtr tensorDescriptor $ \raw_tensorDescriptor ->
      sendClassMsg cls' (mkSelector "tensorWithDescriptor:randomInitializerType:") (retPtr retVoid) [argPtr (castPtr raw_tensorDescriptor :: Ptr ()), argCInt (coerce randomInitializerType)] >>= retainedObject . castPtr

-- | Create a MLCTensor object
--
-- Create a tensor object with a MLCTensorData object that specifies the tensor data buffer
--
-- @tensorDescriptor@ — The tensor descriptor
--
-- @fillData@ — The scalar data to fill to tensor with
--
-- Returns: A new MLCTensor object
--
-- ObjC selector: @+ tensorWithDescriptor:fillWithData:@
tensorWithDescriptor_fillWithData :: (IsMLCTensorDescriptor tensorDescriptor, IsNSNumber fillData) => tensorDescriptor -> fillData -> IO (Id MLCTensor)
tensorWithDescriptor_fillWithData tensorDescriptor fillData =
  do
    cls' <- getRequiredClass "MLCTensor"
    withObjCPtr tensorDescriptor $ \raw_tensorDescriptor ->
      withObjCPtr fillData $ \raw_fillData ->
        sendClassMsg cls' (mkSelector "tensorWithDescriptor:fillWithData:") (retPtr retVoid) [argPtr (castPtr raw_tensorDescriptor :: Ptr ()), argPtr (castPtr raw_fillData :: Ptr ())] >>= retainedObject . castPtr

-- | Create a MLCTensor object
--
-- Create a tensor object with a MLCTensorData object that specifies the tensor data buffer
--
-- @tensorDescriptor@ — The tensor descriptor
--
-- @data@ — The random initializer type
--
-- Returns: A new MLCTensor object
--
-- ObjC selector: @+ tensorWithDescriptor:data:@
tensorWithDescriptor_data :: (IsMLCTensorDescriptor tensorDescriptor, IsMLCTensorData data_) => tensorDescriptor -> data_ -> IO (Id MLCTensor)
tensorWithDescriptor_data tensorDescriptor data_ =
  do
    cls' <- getRequiredClass "MLCTensor"
    withObjCPtr tensorDescriptor $ \raw_tensorDescriptor ->
      withObjCPtr data_ $ \raw_data_ ->
        sendClassMsg cls' (mkSelector "tensorWithDescriptor:data:") (retPtr retVoid) [argPtr (castPtr raw_tensorDescriptor :: Ptr ()), argPtr (castPtr raw_data_ :: Ptr ())] >>= retainedObject . castPtr

-- | Create a MLCTensor object
--
-- Create a tensor object without any data.  The tensor data type is MLCDataTypeFloat32.
--
-- @shape@ — The tensor shape
--
-- Returns: A new MLCTensor object
--
-- ObjC selector: @+ tensorWithShape:@
tensorWithShape :: IsNSArray shape => shape -> IO (Id MLCTensor)
tensorWithShape shape =
  do
    cls' <- getRequiredClass "MLCTensor"
    withObjCPtr shape $ \raw_shape ->
      sendClassMsg cls' (mkSelector "tensorWithShape:") (retPtr retVoid) [argPtr (castPtr raw_shape :: Ptr ())] >>= retainedObject . castPtr

-- | Create a MLCTensor object
--
-- Create a tensor object initialized with a random initializer such as Glorot Uniform.                The tensor data type is MLCDataTypeFloat32
--
-- @shape@ — The tensor shape
--
-- @randomInitializerType@ — The random initializer type
--
-- Returns: A new MLCTensor object
--
-- ObjC selector: @+ tensorWithShape:randomInitializerType:@
tensorWithShape_randomInitializerType :: IsNSArray shape => shape -> MLCRandomInitializerType -> IO (Id MLCTensor)
tensorWithShape_randomInitializerType shape randomInitializerType =
  do
    cls' <- getRequiredClass "MLCTensor"
    withObjCPtr shape $ \raw_shape ->
      sendClassMsg cls' (mkSelector "tensorWithShape:randomInitializerType:") (retPtr retVoid) [argPtr (castPtr raw_shape :: Ptr ()), argCInt (coerce randomInitializerType)] >>= retainedObject . castPtr

-- | Create a MLCTensor object
--
-- Create a tensor object initialized with a random initializer such as Glorot Uniform.                The tensor data type is MLCDataTypeFloat32
--
-- @shape@ — The tensor shape
--
-- @randomInitializerType@ — The random initializer type
--
-- @dataType@ — The tensor data type
--
-- Returns: A new MLCTensor object
--
-- ObjC selector: @+ tensorWithShape:randomInitializerType:dataType:@
tensorWithShape_randomInitializerType_dataType :: IsNSArray shape => shape -> MLCRandomInitializerType -> MLCDataType -> IO (Id MLCTensor)
tensorWithShape_randomInitializerType_dataType shape randomInitializerType dataType =
  do
    cls' <- getRequiredClass "MLCTensor"
    withObjCPtr shape $ \raw_shape ->
      sendClassMsg cls' (mkSelector "tensorWithShape:randomInitializerType:dataType:") (retPtr retVoid) [argPtr (castPtr raw_shape :: Ptr ()), argCInt (coerce randomInitializerType), argCInt (coerce dataType)] >>= retainedObject . castPtr

-- | Create a MLCTensor object
--
-- Create a tensor object without any data
--
-- @shape@ — The tensor shape
--
-- @dataType@ — The tensor data type
--
-- Returns: A new MLCTensor object
--
-- ObjC selector: @+ tensorWithShape:dataType:@
tensorWithShape_dataType :: IsNSArray shape => shape -> MLCDataType -> IO (Id MLCTensor)
tensorWithShape_dataType shape dataType =
  do
    cls' <- getRequiredClass "MLCTensor"
    withObjCPtr shape $ \raw_shape ->
      sendClassMsg cls' (mkSelector "tensorWithShape:dataType:") (retPtr retVoid) [argPtr (castPtr raw_shape :: Ptr ()), argCInt (coerce dataType)] >>= retainedObject . castPtr

-- | Create a MLCTensor object
--
-- Create a tensor object with data
--
-- @shape@ — The tensor shape
--
-- @data@ — The tensor data
--
-- @dataType@ — The tensor data type
--
-- Returns: A new MLCTensor object
--
-- ObjC selector: @+ tensorWithShape:data:dataType:@
tensorWithShape_data_dataType :: (IsNSArray shape, IsMLCTensorData data_) => shape -> data_ -> MLCDataType -> IO (Id MLCTensor)
tensorWithShape_data_dataType shape data_ dataType =
  do
    cls' <- getRequiredClass "MLCTensor"
    withObjCPtr shape $ \raw_shape ->
      withObjCPtr data_ $ \raw_data_ ->
        sendClassMsg cls' (mkSelector "tensorWithShape:data:dataType:") (retPtr retVoid) [argPtr (castPtr raw_shape :: Ptr ()), argPtr (castPtr raw_data_ :: Ptr ()), argCInt (coerce dataType)] >>= retainedObject . castPtr

-- | Create a MLCTensor object
--
-- Create a tensor object with data
--
-- @shape@ — The tensor shape
--
-- @fillData@ — The scalar value to initialize the tensor data with
--
-- @dataType@ — The tensor data type
--
-- Returns: A new MLCTensor object
--
-- ObjC selector: @+ tensorWithShape:fillWithData:dataType:@
tensorWithShape_fillWithData_dataType :: (IsNSArray shape, IsNSNumber fillData) => shape -> fillData -> MLCDataType -> IO (Id MLCTensor)
tensorWithShape_fillWithData_dataType shape fillData dataType =
  do
    cls' <- getRequiredClass "MLCTensor"
    withObjCPtr shape $ \raw_shape ->
      withObjCPtr fillData $ \raw_fillData ->
        sendClassMsg cls' (mkSelector "tensorWithShape:fillWithData:dataType:") (retPtr retVoid) [argPtr (castPtr raw_shape :: Ptr ()), argPtr (castPtr raw_fillData :: Ptr ()), argCInt (coerce dataType)] >>= retainedObject . castPtr

-- | Create a MLCTensor  object
--
-- Create a NCHW tensor object with tensor data type = MLCDataTypeFloat32
--
-- @width@ — The tensor width
--
-- @height@ — The tensor height
--
-- @featureChannelCount@ — Number of feature channels
--
-- @batchSize@ — The tensor batch size
--
-- Returns: A new MLCTensor object
--
-- ObjC selector: @+ tensorWithWidth:height:featureChannelCount:batchSize:@
tensorWithWidth_height_featureChannelCount_batchSize :: CULong -> CULong -> CULong -> CULong -> IO (Id MLCTensor)
tensorWithWidth_height_featureChannelCount_batchSize width height featureChannelCount batchSize =
  do
    cls' <- getRequiredClass "MLCTensor"
    sendClassMsg cls' (mkSelector "tensorWithWidth:height:featureChannelCount:batchSize:") (retPtr retVoid) [argCULong (fromIntegral width), argCULong (fromIntegral height), argCULong (fromIntegral featureChannelCount), argCULong (fromIntegral batchSize)] >>= retainedObject . castPtr

-- | Create a MLCTensor  object
--
-- Create a NCHW tensor object initialized with a scalar value
--
-- @width@ — The tensor width
--
-- @height@ — The tensor height
--
-- @featureChannelCount@ — Number of feature channels
--
-- @batchSize@ — The tensor batch size
--
-- @fillData@ — The scalar value to initialize the tensor data with
--
-- @dataType@ — The tensor data type
--
-- Returns: A new MLCTensorData object
--
-- ObjC selector: @+ tensorWithWidth:height:featureChannelCount:batchSize:fillWithData:dataType:@
tensorWithWidth_height_featureChannelCount_batchSize_fillWithData_dataType :: CULong -> CULong -> CULong -> CULong -> CFloat -> MLCDataType -> IO (Id MLCTensor)
tensorWithWidth_height_featureChannelCount_batchSize_fillWithData_dataType width height featureChannelCount batchSize fillData dataType =
  do
    cls' <- getRequiredClass "MLCTensor"
    sendClassMsg cls' (mkSelector "tensorWithWidth:height:featureChannelCount:batchSize:fillWithData:dataType:") (retPtr retVoid) [argCULong (fromIntegral width), argCULong (fromIntegral height), argCULong (fromIntegral featureChannelCount), argCULong (fromIntegral batchSize), argCFloat (fromIntegral fillData), argCInt (coerce dataType)] >>= retainedObject . castPtr

-- | Create a MLCTensor  object
--
-- Create a NCHW tensor object initialized with a random initializer type.                The tensor data type is MLCDataTypeFloat32
--
-- @width@ — The tensor width
--
-- @height@ — The tensor height
--
-- @featureChannelCount@ — Number of feature channels
--
-- @batchSize@ — The tensor batch size
--
-- @randomInitializerType@ — The random initializer type
--
-- Returns: A new MLCTensor object
--
-- ObjC selector: @+ tensorWithWidth:height:featureChannelCount:batchSize:randomInitializerType:@
tensorWithWidth_height_featureChannelCount_batchSize_randomInitializerType :: CULong -> CULong -> CULong -> CULong -> MLCRandomInitializerType -> IO (Id MLCTensor)
tensorWithWidth_height_featureChannelCount_batchSize_randomInitializerType width height featureChannelCount batchSize randomInitializerType =
  do
    cls' <- getRequiredClass "MLCTensor"
    sendClassMsg cls' (mkSelector "tensorWithWidth:height:featureChannelCount:batchSize:randomInitializerType:") (retPtr retVoid) [argCULong (fromIntegral width), argCULong (fromIntegral height), argCULong (fromIntegral featureChannelCount), argCULong (fromIntegral batchSize), argCInt (coerce randomInitializerType)] >>= retainedObject . castPtr

-- | Create a MLCTensor  object
--
-- Create a NCHW tensor object with a tensor data object                The tensor data type is MLCDataTypeFloat32.
--
-- @width@ — The tensor width
--
-- @height@ — The tensor height
--
-- @featureChannelCount@ — Number of feature channels
--
-- @batchSize@ — The tensor batch size
--
-- @data@ — The tensor data
--
-- Returns: A new MLCTensor object
--
-- ObjC selector: @+ tensorWithWidth:height:featureChannelCount:batchSize:data:@
tensorWithWidth_height_featureChannelCount_batchSize_data :: IsMLCTensorData data_ => CULong -> CULong -> CULong -> CULong -> data_ -> IO (Id MLCTensor)
tensorWithWidth_height_featureChannelCount_batchSize_data width height featureChannelCount batchSize data_ =
  do
    cls' <- getRequiredClass "MLCTensor"
    withObjCPtr data_ $ \raw_data_ ->
      sendClassMsg cls' (mkSelector "tensorWithWidth:height:featureChannelCount:batchSize:data:") (retPtr retVoid) [argCULong (fromIntegral width), argCULong (fromIntegral height), argCULong (fromIntegral featureChannelCount), argCULong (fromIntegral batchSize), argPtr (castPtr raw_data_ :: Ptr ())] >>= retainedObject . castPtr

-- | Create a MLCTensor  object
--
-- Create a NCHW tensor object with a tensor data object                The tensor data type is MLCDataTypeFloat32.
--
-- @width@ — The tensor width
--
-- @height@ — The tensor height
--
-- @featureChannelCount@ — Number of feature channels
--
-- @batchSize@ — The tensor batch size
--
-- @data@ — The tensor data
--
-- @dataType@ — The tensor data type
--
-- Returns: A new MLCTensor object
--
-- ObjC selector: @+ tensorWithWidth:height:featureChannelCount:batchSize:data:dataType:@
tensorWithWidth_height_featureChannelCount_batchSize_data_dataType :: IsMLCTensorData data_ => CULong -> CULong -> CULong -> CULong -> data_ -> MLCDataType -> IO (Id MLCTensor)
tensorWithWidth_height_featureChannelCount_batchSize_data_dataType width height featureChannelCount batchSize data_ dataType =
  do
    cls' <- getRequiredClass "MLCTensor"
    withObjCPtr data_ $ \raw_data_ ->
      sendClassMsg cls' (mkSelector "tensorWithWidth:height:featureChannelCount:batchSize:data:dataType:") (retPtr retVoid) [argCULong (fromIntegral width), argCULong (fromIntegral height), argCULong (fromIntegral featureChannelCount), argCULong (fromIntegral batchSize), argPtr (castPtr raw_data_ :: Ptr ()), argCInt (coerce dataType)] >>= retainedObject . castPtr

-- | Create a MLCTensor  object
--
-- Create a tensor typically used by a recurrent layer                The tensor data type is MLCDataTypeFloat32.
--
-- @sequenceLength@ — The length of sequences stored in the tensor
--
-- @featureChannelCount@ — Number of feature channels
--
-- @batchSize@ — The tensor batch size
--
-- Returns: A new MLCTensor object
--
-- ObjC selector: @+ tensorWithSequenceLength:featureChannelCount:batchSize:@
tensorWithSequenceLength_featureChannelCount_batchSize :: CULong -> CULong -> CULong -> IO (Id MLCTensor)
tensorWithSequenceLength_featureChannelCount_batchSize sequenceLength featureChannelCount batchSize =
  do
    cls' <- getRequiredClass "MLCTensor"
    sendClassMsg cls' (mkSelector "tensorWithSequenceLength:featureChannelCount:batchSize:") (retPtr retVoid) [argCULong (fromIntegral sequenceLength), argCULong (fromIntegral featureChannelCount), argCULong (fromIntegral batchSize)] >>= retainedObject . castPtr

-- | Create a MLCTensor  object
--
-- Create a tensor typically used by a recurrent layer                The tensor data type is MLCDataTypeFloat32.
--
-- @sequenceLength@ — The length of sequences stored in the tensor
--
-- @featureChannelCount@ — Number of feature channels
--
-- @batchSize@ — The tensor batch size
--
-- @randomInitializerType@ — The random initializer type
--
-- Returns: A new MLCTensor object
--
-- ObjC selector: @+ tensorWithSequenceLength:featureChannelCount:batchSize:randomInitializerType:@
tensorWithSequenceLength_featureChannelCount_batchSize_randomInitializerType :: CULong -> CULong -> CULong -> MLCRandomInitializerType -> IO (Id MLCTensor)
tensorWithSequenceLength_featureChannelCount_batchSize_randomInitializerType sequenceLength featureChannelCount batchSize randomInitializerType =
  do
    cls' <- getRequiredClass "MLCTensor"
    sendClassMsg cls' (mkSelector "tensorWithSequenceLength:featureChannelCount:batchSize:randomInitializerType:") (retPtr retVoid) [argCULong (fromIntegral sequenceLength), argCULong (fromIntegral featureChannelCount), argCULong (fromIntegral batchSize), argCInt (coerce randomInitializerType)] >>= retainedObject . castPtr

-- | Create a MLCTensor  object
--
-- Create a tensor typically used by a recurrent layer                The tensor data type is MLCDataTypeFloat32.
--
-- @sequenceLength@ — The length of sequences stored in the tensor
--
-- @featureChannelCount@ — Number of feature channels
--
-- @batchSize@ — The tensor batch size
--
-- @data@ — The tensor data
--
-- Returns: A new MLCTensor object
--
-- ObjC selector: @+ tensorWithSequenceLength:featureChannelCount:batchSize:data:@
tensorWithSequenceLength_featureChannelCount_batchSize_data :: IsMLCTensorData data_ => CULong -> CULong -> CULong -> data_ -> IO (Id MLCTensor)
tensorWithSequenceLength_featureChannelCount_batchSize_data sequenceLength featureChannelCount batchSize data_ =
  do
    cls' <- getRequiredClass "MLCTensor"
    withObjCPtr data_ $ \raw_data_ ->
      sendClassMsg cls' (mkSelector "tensorWithSequenceLength:featureChannelCount:batchSize:data:") (retPtr retVoid) [argCULong (fromIntegral sequenceLength), argCULong (fromIntegral featureChannelCount), argCULong (fromIntegral batchSize), argPtr (castPtr raw_data_ :: Ptr ())] >>= retainedObject . castPtr

-- | Create a MLCTensor  object
--
-- Create a tensor of variable length sequences typically used by a recurrent layer                The tensor data type is MLCDataTypeFloat32.
--
-- @sequenceLengths@ — An array of sequence lengths
--
-- @sortedSequences@ — A flag to indicate if the sequence lengths are sorted.  If yes, they must be sorted in descending order
--
-- @featureChannelCount@ — Number of feature channels
--
-- @batchSize@ — The tensor batch size
--
-- @randomInitializerType@ — The random initializer type
--
-- Returns: A new MLCTensor object
--
-- ObjC selector: @+ tensorWithSequenceLengths:sortedSequences:featureChannelCount:batchSize:randomInitializerType:@
tensorWithSequenceLengths_sortedSequences_featureChannelCount_batchSize_randomInitializerType :: IsNSArray sequenceLengths => sequenceLengths -> Bool -> CULong -> CULong -> MLCRandomInitializerType -> IO (Id MLCTensor)
tensorWithSequenceLengths_sortedSequences_featureChannelCount_batchSize_randomInitializerType sequenceLengths sortedSequences featureChannelCount batchSize randomInitializerType =
  do
    cls' <- getRequiredClass "MLCTensor"
    withObjCPtr sequenceLengths $ \raw_sequenceLengths ->
      sendClassMsg cls' (mkSelector "tensorWithSequenceLengths:sortedSequences:featureChannelCount:batchSize:randomInitializerType:") (retPtr retVoid) [argPtr (castPtr raw_sequenceLengths :: Ptr ()), argCULong (if sortedSequences then 1 else 0), argCULong (fromIntegral featureChannelCount), argCULong (fromIntegral batchSize), argCInt (coerce randomInitializerType)] >>= retainedObject . castPtr

-- | Create a MLCTensor  object
--
-- Create a tensor of variable length sequences typically used by a recurrent layer                The tensor data type is MLCDataTypeFloat32.
--
-- @sequenceLengths@ — An array of sequence lengths
--
-- @sortedSequences@ — A flag to indicate if the sequence lengths are sorted.  If yes, they must be sorted in descending order
--
-- @featureChannelCount@ — Number of feature channels
--
-- @batchSize@ — The tensor batch size
--
-- @data@ — The tensor data
--
-- Returns: A new MLCTensor object
--
-- ObjC selector: @+ tensorWithSequenceLengths:sortedSequences:featureChannelCount:batchSize:data:@
tensorWithSequenceLengths_sortedSequences_featureChannelCount_batchSize_data :: (IsNSArray sequenceLengths, IsMLCTensorData data_) => sequenceLengths -> Bool -> CULong -> CULong -> data_ -> IO (Id MLCTensor)
tensorWithSequenceLengths_sortedSequences_featureChannelCount_batchSize_data sequenceLengths sortedSequences featureChannelCount batchSize data_ =
  do
    cls' <- getRequiredClass "MLCTensor"
    withObjCPtr sequenceLengths $ \raw_sequenceLengths ->
      withObjCPtr data_ $ \raw_data_ ->
        sendClassMsg cls' (mkSelector "tensorWithSequenceLengths:sortedSequences:featureChannelCount:batchSize:data:") (retPtr retVoid) [argPtr (castPtr raw_sequenceLengths :: Ptr ()), argCULong (if sortedSequences then 1 else 0), argCULong (fromIntegral featureChannelCount), argCULong (fromIntegral batchSize), argPtr (castPtr raw_data_ :: Ptr ())] >>= retainedObject . castPtr

-- | Synchronize the data in host memory.
--
-- Synchronize the data in host memory i.e. tensor.data with latest contents in device memory                This should only be called once the graph that this tensor is used with has finished execution;                Otherwise the results in device memory may not be up to date.                NOTE:  This method should not be called from a completion callback when device is the GPU.
--
-- Returns: Returns YES if success, NO if there is a failure to synchronize
--
-- ObjC selector: @- synchronizeData@
synchronizeData :: IsMLCTensor mlcTensor => mlcTensor -> IO Bool
synchronizeData mlcTensor  =
  fmap ((/= 0) :: CULong -> Bool) $ sendMsg mlcTensor (mkSelector "synchronizeData") retCULong []

-- | Synchronize the optimizer data in host memory.
--
-- Synchronize the optimizer data in host memory with latest contents in device memory                This should only be called once the graph that this tensor is used with has finished execution;                Otherwise the results in device memory may not be up to date.                NOTE:  This method should not be called from a completion callback when device is the GPU.
--
-- Returns: Returns YES if success, NO if there is a failure to synchronize
--
-- ObjC selector: @- synchronizeOptimizerData@
synchronizeOptimizerData :: IsMLCTensor mlcTensor => mlcTensor -> IO Bool
synchronizeOptimizerData mlcTensor  =
  fmap ((/= 0) :: CULong -> Bool) $ sendMsg mlcTensor (mkSelector "synchronizeOptimizerData") retCULong []

-- | Copy tensor data from device memory to user specified memory
--
-- Before copying tensor data from device memory, one may need to synchronize the device memory for example                when device is the GPU.  The synchronizeWithDevice argumet can be set appropraitely to indicate this.                For CPU this is ignored.  If the tensor has been specified in outputs of a graph using addOutputs,                synchronizeWithDevice should be set to NO.                NOTE:  This method should only be called once the graph that this tensor is used with has finished execution;                Otherwise the results in device memory may not be up to date.  synchronizeWithDevice must be set to NO                when this method is called from a completion callback for GPU.
--
-- @bytes@ — The user specified data in which to copy
--
-- @length@ — The size in bytes to copy
--
-- @synchronizeWithDevice@ — Whether to synchronize device memory if device is GPU
--
-- Returns: Returns YES if success, NO if there is a failure to synchronize
--
-- ObjC selector: @- copyDataFromDeviceMemoryToBytes:length:synchronizeWithDevice:@
copyDataFromDeviceMemoryToBytes_length_synchronizeWithDevice :: IsMLCTensor mlcTensor => mlcTensor -> Ptr () -> CULong -> Bool -> IO Bool
copyDataFromDeviceMemoryToBytes_length_synchronizeWithDevice mlcTensor  bytes length_ synchronizeWithDevice =
  fmap ((/= 0) :: CULong -> Bool) $ sendMsg mlcTensor (mkSelector "copyDataFromDeviceMemoryToBytes:length:synchronizeWithDevice:") retCULong [argPtr bytes, argCULong (fromIntegral length_), argCULong (if synchronizeWithDevice then 1 else 0)]

-- | Associates the given data to the tensor. If the device is GPU, also copies the data to the device memory.                Returns true if the data is successfully associated with the tensor and copied to the device.
--
-- The caller must guarantee the lifetime of the underlying memory of @data@ for the entirety of the tensor's                lifetime.  For input tensors, we recommend that the bindAndwriteData method provided by MLCTrainingGraph                and MLCInferenceGraph be used.  This method should only be used to allocate and copy data to device memory                for tensors that are typically layer parameters such as weights, bias for convolution layers, beta, gamma for                normalization layers.
--
-- @data@ — The data to associated with the tensor
--
-- @device@ — The compute device
--
-- Returns: A Boolean value indicating whether the data is successfully associated with the tensor and copied to the device.
--
-- ObjC selector: @- bindAndWriteData:toDevice:@
bindAndWriteData_toDevice :: (IsMLCTensor mlcTensor, IsMLCTensorData data_, IsMLCDevice device) => mlcTensor -> data_ -> device -> IO Bool
bindAndWriteData_toDevice mlcTensor  data_ device =
withObjCPtr data_ $ \raw_data_ ->
  withObjCPtr device $ \raw_device ->
      fmap ((/= 0) :: CULong -> Bool) $ sendMsg mlcTensor (mkSelector "bindAndWriteData:toDevice:") retCULong [argPtr (castPtr raw_data_ :: Ptr ()), argPtr (castPtr raw_device :: Ptr ())]

-- | Associates the given optimizer data and device data buffers to the tensor.                Returns true if the data is successfully associated with the tensor and copied to the device.
--
-- The caller must guarantee the lifetime of the underlying memory of @data@ for the entirety of the tensor's                lifetime.  The @deviceData@ buffers are allocated by MLCompute.  This method must be called                before executeOptimizerUpdateWithOptions or executeWithInputsData is called for the training graph.
--
-- @data@ — The optimizer data to be associated with the tensor
--
-- @deviceData@ — The optimizer device data to be associated with the tensor
--
-- Returns: A Boolean value indicating whether the data is successfully associated with the tensor .
--
-- ObjC selector: @- bindOptimizerData:deviceData:@
bindOptimizerData_deviceData :: (IsMLCTensor mlcTensor, IsNSArray data_, IsNSArray deviceData) => mlcTensor -> data_ -> deviceData -> IO Bool
bindOptimizerData_deviceData mlcTensor  data_ deviceData =
withObjCPtr data_ $ \raw_data_ ->
  withObjCPtr deviceData $ \raw_deviceData ->
      fmap ((/= 0) :: CULong -> Bool) $ sendMsg mlcTensor (mkSelector "bindOptimizerData:deviceData:") retCULong [argPtr (castPtr raw_data_ :: Ptr ()), argPtr (castPtr raw_deviceData :: Ptr ())]

-- | Converts a 32-bit floating-point tensor with given scale and a zero point                Returns a quantized tensor
--
-- @type@ — The quantized data type.  Must be MLCDataTypeInt8, MLCDataTypeUInt8 or MLCDataTypeInt32
--
-- @scale@ — The scale to apply in quantization
--
-- @bias@ — The offset value that maps to float zero
--
-- Returns: A quantized tensor
--
-- ObjC selector: @- tensorByQuantizingToType:scale:bias:@
tensorByQuantizingToType_scale_bias :: IsMLCTensor mlcTensor => mlcTensor -> MLCDataType -> CFloat -> CLong -> IO (Id MLCTensor)
tensorByQuantizingToType_scale_bias mlcTensor  type_ scale bias =
  sendMsg mlcTensor (mkSelector "tensorByQuantizingToType:scale:bias:") (retPtr retVoid) [argCInt (coerce type_), argCFloat (fromIntegral scale), argCLong (fromIntegral bias)] >>= retainedObject . castPtr

-- | Converts a 32-bit floating-point tensor with given scale and a zero point                Returns a quantized tensor
--
-- @type@ — The quantized data type.  Must be MLCDataTypeInt8, MLCDataTypeUInt8 or MLCDataTypeInt32
--
-- @scale@ — The scale to apply in quantization
--
-- @bias@ — The offset value that maps to float zero
--
-- @axis@ — The dimension on which to apply per-channel quantization
--
-- Returns: A quantized tensor
--
-- ObjC selector: @- tensorByQuantizingToType:scale:bias:axis:@
tensorByQuantizingToType_scale_bias_axis :: (IsMLCTensor mlcTensor, IsMLCTensor scale, IsMLCTensor bias) => mlcTensor -> MLCDataType -> scale -> bias -> CLong -> IO (Id MLCTensor)
tensorByQuantizingToType_scale_bias_axis mlcTensor  type_ scale bias axis =
withObjCPtr scale $ \raw_scale ->
  withObjCPtr bias $ \raw_bias ->
      sendMsg mlcTensor (mkSelector "tensorByQuantizingToType:scale:bias:axis:") (retPtr retVoid) [argCInt (coerce type_), argPtr (castPtr raw_scale :: Ptr ()), argPtr (castPtr raw_bias :: Ptr ()), argCLong (fromIntegral axis)] >>= retainedObject . castPtr

-- | Converts a quantized tensor to a 32-bit floating-point tensor                Returns a de-quantized tensor
--
-- @type@ — The de-quantized data type.  Must be MLCFloat32
--
-- @scale@ — The scale thst was used for the quantized data
--
-- @bias@ — The offset value that maps to float zero used for the quantized data
--
-- Returns: A quantized tensor
--
-- ObjC selector: @- tensorByDequantizingToType:scale:bias:@
tensorByDequantizingToType_scale_bias :: (IsMLCTensor mlcTensor, IsMLCTensor scale, IsMLCTensor bias) => mlcTensor -> MLCDataType -> scale -> bias -> IO (Id MLCTensor)
tensorByDequantizingToType_scale_bias mlcTensor  type_ scale bias =
withObjCPtr scale $ \raw_scale ->
  withObjCPtr bias $ \raw_bias ->
      sendMsg mlcTensor (mkSelector "tensorByDequantizingToType:scale:bias:") (retPtr retVoid) [argCInt (coerce type_), argPtr (castPtr raw_scale :: Ptr ()), argPtr (castPtr raw_bias :: Ptr ())] >>= retainedObject . castPtr

-- | Converts a quantized tensor to a 32-bit floating-point tensor                Returns a de-quantized tensor
--
-- @type@ — The de-quantized data type.  Must be MLCFloat32
--
-- @scale@ — The scale thst was used for the quantized data
--
-- @bias@ — The offset value that maps to float zero used for the quantized data
--
-- @axis@ — The dimension on which to apply per-channel quantization
--
-- Returns: A quantized tensor
--
-- ObjC selector: @- tensorByDequantizingToType:scale:bias:axis:@
tensorByDequantizingToType_scale_bias_axis :: (IsMLCTensor mlcTensor, IsMLCTensor scale, IsMLCTensor bias) => mlcTensor -> MLCDataType -> scale -> bias -> CLong -> IO (Id MLCTensor)
tensorByDequantizingToType_scale_bias_axis mlcTensor  type_ scale bias axis =
withObjCPtr scale $ \raw_scale ->
  withObjCPtr bias $ \raw_bias ->
      sendMsg mlcTensor (mkSelector "tensorByDequantizingToType:scale:bias:axis:") (retPtr retVoid) [argCInt (coerce type_), argPtr (castPtr raw_scale :: Ptr ()), argPtr (castPtr raw_bias :: Ptr ()), argCLong (fromIntegral axis)] >>= retainedObject . castPtr

-- | tensorID
--
-- The tensor ID
--
-- A unique number to identify each tensor.  Assigned when the tensor is created.
--
-- ObjC selector: @- tensorID@
tensorID :: IsMLCTensor mlcTensor => mlcTensor -> IO CULong
tensorID mlcTensor  =
  sendMsg mlcTensor (mkSelector "tensorID") retCULong []

-- | descriptor
--
-- The tensor descriptor
--
-- ObjC selector: @- descriptor@
descriptor :: IsMLCTensor mlcTensor => mlcTensor -> IO (Id MLCTensorDescriptor)
descriptor mlcTensor  =
  sendMsg mlcTensor (mkSelector "descriptor") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | data
--
-- The tensor data
--
-- ObjC selector: @- data@
data_ :: IsMLCTensor mlcTensor => mlcTensor -> IO (Id NSData)
data_ mlcTensor  =
  sendMsg mlcTensor (mkSelector "data") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | label
--
-- A string to help identify this object.
--
-- ObjC selector: @- label@
label :: IsMLCTensor mlcTensor => mlcTensor -> IO (Id NSString)
label mlcTensor  =
  sendMsg mlcTensor (mkSelector "label") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | label
--
-- A string to help identify this object.
--
-- ObjC selector: @- setLabel:@
setLabel :: (IsMLCTensor mlcTensor, IsNSString value) => mlcTensor -> value -> IO ()
setLabel mlcTensor  value =
withObjCPtr value $ \raw_value ->
    sendMsg mlcTensor (mkSelector "setLabel:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- | device
--
-- The device associated with this tensor.
--
-- ObjC selector: @- device@
device :: IsMLCTensor mlcTensor => mlcTensor -> IO (Id MLCDevice)
device mlcTensor  =
  sendMsg mlcTensor (mkSelector "device") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | optimizer buffers to use if tensor is used as a parameter
--
-- These are the host side optimizer (momentum and velocity) buffers which developers can query and initialize
--
-- When customizing optimizer data, the contents of these buffers must be initialized before executing optimizer                update for a graph.
--
-- ObjC selector: @- optimizerData@
optimizerData :: IsMLCTensor mlcTensor => mlcTensor -> IO (Id NSArray)
optimizerData mlcTensor  =
  sendMsg mlcTensor (mkSelector "optimizerData") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | optimizer device buffers to use if tensor is used as a parameter
--
-- These are the device side optimizer (momentum and velocity) buffers which developers can query
--
-- ObjC selector: @- optimizerDeviceData@
optimizerDeviceData :: IsMLCTensor mlcTensor => mlcTensor -> IO (Id NSArray)
optimizerDeviceData mlcTensor  =
  sendMsg mlcTensor (mkSelector "optimizerDeviceData") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | Returns a Boolean value indicating whether the underlying data has valid floating-point numerics, i.e. it                does not contain NaN or INF floating-point values.
--
-- ObjC selector: @- hasValidNumerics@
hasValidNumerics :: IsMLCTensor mlcTensor => mlcTensor -> IO Bool
hasValidNumerics mlcTensor  =
  fmap ((/= 0) :: CULong -> Bool) $ sendMsg mlcTensor (mkSelector "hasValidNumerics") retCULong []

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @new@
newSelector :: Selector
newSelector = mkSelector "new"

-- | @Selector@ for @init@
initSelector :: Selector
initSelector = mkSelector "init"

-- | @Selector@ for @tensorWithDescriptor:@
tensorWithDescriptorSelector :: Selector
tensorWithDescriptorSelector = mkSelector "tensorWithDescriptor:"

-- | @Selector@ for @tensorWithDescriptor:randomInitializerType:@
tensorWithDescriptor_randomInitializerTypeSelector :: Selector
tensorWithDescriptor_randomInitializerTypeSelector = mkSelector "tensorWithDescriptor:randomInitializerType:"

-- | @Selector@ for @tensorWithDescriptor:fillWithData:@
tensorWithDescriptor_fillWithDataSelector :: Selector
tensorWithDescriptor_fillWithDataSelector = mkSelector "tensorWithDescriptor:fillWithData:"

-- | @Selector@ for @tensorWithDescriptor:data:@
tensorWithDescriptor_dataSelector :: Selector
tensorWithDescriptor_dataSelector = mkSelector "tensorWithDescriptor:data:"

-- | @Selector@ for @tensorWithShape:@
tensorWithShapeSelector :: Selector
tensorWithShapeSelector = mkSelector "tensorWithShape:"

-- | @Selector@ for @tensorWithShape:randomInitializerType:@
tensorWithShape_randomInitializerTypeSelector :: Selector
tensorWithShape_randomInitializerTypeSelector = mkSelector "tensorWithShape:randomInitializerType:"

-- | @Selector@ for @tensorWithShape:randomInitializerType:dataType:@
tensorWithShape_randomInitializerType_dataTypeSelector :: Selector
tensorWithShape_randomInitializerType_dataTypeSelector = mkSelector "tensorWithShape:randomInitializerType:dataType:"

-- | @Selector@ for @tensorWithShape:dataType:@
tensorWithShape_dataTypeSelector :: Selector
tensorWithShape_dataTypeSelector = mkSelector "tensorWithShape:dataType:"

-- | @Selector@ for @tensorWithShape:data:dataType:@
tensorWithShape_data_dataTypeSelector :: Selector
tensorWithShape_data_dataTypeSelector = mkSelector "tensorWithShape:data:dataType:"

-- | @Selector@ for @tensorWithShape:fillWithData:dataType:@
tensorWithShape_fillWithData_dataTypeSelector :: Selector
tensorWithShape_fillWithData_dataTypeSelector = mkSelector "tensorWithShape:fillWithData:dataType:"

-- | @Selector@ for @tensorWithWidth:height:featureChannelCount:batchSize:@
tensorWithWidth_height_featureChannelCount_batchSizeSelector :: Selector
tensorWithWidth_height_featureChannelCount_batchSizeSelector = mkSelector "tensorWithWidth:height:featureChannelCount:batchSize:"

-- | @Selector@ for @tensorWithWidth:height:featureChannelCount:batchSize:fillWithData:dataType:@
tensorWithWidth_height_featureChannelCount_batchSize_fillWithData_dataTypeSelector :: Selector
tensorWithWidth_height_featureChannelCount_batchSize_fillWithData_dataTypeSelector = mkSelector "tensorWithWidth:height:featureChannelCount:batchSize:fillWithData:dataType:"

-- | @Selector@ for @tensorWithWidth:height:featureChannelCount:batchSize:randomInitializerType:@
tensorWithWidth_height_featureChannelCount_batchSize_randomInitializerTypeSelector :: Selector
tensorWithWidth_height_featureChannelCount_batchSize_randomInitializerTypeSelector = mkSelector "tensorWithWidth:height:featureChannelCount:batchSize:randomInitializerType:"

-- | @Selector@ for @tensorWithWidth:height:featureChannelCount:batchSize:data:@
tensorWithWidth_height_featureChannelCount_batchSize_dataSelector :: Selector
tensorWithWidth_height_featureChannelCount_batchSize_dataSelector = mkSelector "tensorWithWidth:height:featureChannelCount:batchSize:data:"

-- | @Selector@ for @tensorWithWidth:height:featureChannelCount:batchSize:data:dataType:@
tensorWithWidth_height_featureChannelCount_batchSize_data_dataTypeSelector :: Selector
tensorWithWidth_height_featureChannelCount_batchSize_data_dataTypeSelector = mkSelector "tensorWithWidth:height:featureChannelCount:batchSize:data:dataType:"

-- | @Selector@ for @tensorWithSequenceLength:featureChannelCount:batchSize:@
tensorWithSequenceLength_featureChannelCount_batchSizeSelector :: Selector
tensorWithSequenceLength_featureChannelCount_batchSizeSelector = mkSelector "tensorWithSequenceLength:featureChannelCount:batchSize:"

-- | @Selector@ for @tensorWithSequenceLength:featureChannelCount:batchSize:randomInitializerType:@
tensorWithSequenceLength_featureChannelCount_batchSize_randomInitializerTypeSelector :: Selector
tensorWithSequenceLength_featureChannelCount_batchSize_randomInitializerTypeSelector = mkSelector "tensorWithSequenceLength:featureChannelCount:batchSize:randomInitializerType:"

-- | @Selector@ for @tensorWithSequenceLength:featureChannelCount:batchSize:data:@
tensorWithSequenceLength_featureChannelCount_batchSize_dataSelector :: Selector
tensorWithSequenceLength_featureChannelCount_batchSize_dataSelector = mkSelector "tensorWithSequenceLength:featureChannelCount:batchSize:data:"

-- | @Selector@ for @tensorWithSequenceLengths:sortedSequences:featureChannelCount:batchSize:randomInitializerType:@
tensorWithSequenceLengths_sortedSequences_featureChannelCount_batchSize_randomInitializerTypeSelector :: Selector
tensorWithSequenceLengths_sortedSequences_featureChannelCount_batchSize_randomInitializerTypeSelector = mkSelector "tensorWithSequenceLengths:sortedSequences:featureChannelCount:batchSize:randomInitializerType:"

-- | @Selector@ for @tensorWithSequenceLengths:sortedSequences:featureChannelCount:batchSize:data:@
tensorWithSequenceLengths_sortedSequences_featureChannelCount_batchSize_dataSelector :: Selector
tensorWithSequenceLengths_sortedSequences_featureChannelCount_batchSize_dataSelector = mkSelector "tensorWithSequenceLengths:sortedSequences:featureChannelCount:batchSize:data:"

-- | @Selector@ for @synchronizeData@
synchronizeDataSelector :: Selector
synchronizeDataSelector = mkSelector "synchronizeData"

-- | @Selector@ for @synchronizeOptimizerData@
synchronizeOptimizerDataSelector :: Selector
synchronizeOptimizerDataSelector = mkSelector "synchronizeOptimizerData"

-- | @Selector@ for @copyDataFromDeviceMemoryToBytes:length:synchronizeWithDevice:@
copyDataFromDeviceMemoryToBytes_length_synchronizeWithDeviceSelector :: Selector
copyDataFromDeviceMemoryToBytes_length_synchronizeWithDeviceSelector = mkSelector "copyDataFromDeviceMemoryToBytes:length:synchronizeWithDevice:"

-- | @Selector@ for @bindAndWriteData:toDevice:@
bindAndWriteData_toDeviceSelector :: Selector
bindAndWriteData_toDeviceSelector = mkSelector "bindAndWriteData:toDevice:"

-- | @Selector@ for @bindOptimizerData:deviceData:@
bindOptimizerData_deviceDataSelector :: Selector
bindOptimizerData_deviceDataSelector = mkSelector "bindOptimizerData:deviceData:"

-- | @Selector@ for @tensorByQuantizingToType:scale:bias:@
tensorByQuantizingToType_scale_biasSelector :: Selector
tensorByQuantizingToType_scale_biasSelector = mkSelector "tensorByQuantizingToType:scale:bias:"

-- | @Selector@ for @tensorByQuantizingToType:scale:bias:axis:@
tensorByQuantizingToType_scale_bias_axisSelector :: Selector
tensorByQuantizingToType_scale_bias_axisSelector = mkSelector "tensorByQuantizingToType:scale:bias:axis:"

-- | @Selector@ for @tensorByDequantizingToType:scale:bias:@
tensorByDequantizingToType_scale_biasSelector :: Selector
tensorByDequantizingToType_scale_biasSelector = mkSelector "tensorByDequantizingToType:scale:bias:"

-- | @Selector@ for @tensorByDequantizingToType:scale:bias:axis:@
tensorByDequantizingToType_scale_bias_axisSelector :: Selector
tensorByDequantizingToType_scale_bias_axisSelector = mkSelector "tensorByDequantizingToType:scale:bias:axis:"

-- | @Selector@ for @tensorID@
tensorIDSelector :: Selector
tensorIDSelector = mkSelector "tensorID"

-- | @Selector@ for @descriptor@
descriptorSelector :: Selector
descriptorSelector = mkSelector "descriptor"

-- | @Selector@ for @data@
dataSelector :: Selector
dataSelector = mkSelector "data"

-- | @Selector@ for @label@
labelSelector :: Selector
labelSelector = mkSelector "label"

-- | @Selector@ for @setLabel:@
setLabelSelector :: Selector
setLabelSelector = mkSelector "setLabel:"

-- | @Selector@ for @device@
deviceSelector :: Selector
deviceSelector = mkSelector "device"

-- | @Selector@ for @optimizerData@
optimizerDataSelector :: Selector
optimizerDataSelector = mkSelector "optimizerData"

-- | @Selector@ for @optimizerDeviceData@
optimizerDeviceDataSelector :: Selector
optimizerDeviceDataSelector = mkSelector "optimizerDeviceData"

-- | @Selector@ for @hasValidNumerics@
hasValidNumericsSelector :: Selector
hasValidNumericsSelector = mkSelector "hasValidNumerics"

