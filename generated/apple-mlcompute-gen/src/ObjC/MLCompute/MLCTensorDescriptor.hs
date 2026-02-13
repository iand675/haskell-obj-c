{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | MLCTensorDescriptor
--
-- The MLCTensorDescriptor specifies a tensor descriptor.
--
-- Generated bindings for @MLCTensorDescriptor@.
module ObjC.MLCompute.MLCTensorDescriptor
  ( MLCTensorDescriptor
  , IsMLCTensorDescriptor(..)
  , new
  , init_
  , descriptorWithShape_dataType
  , descriptorWithShape_sequenceLengths_sortedSequences_dataType
  , descriptorWithWidth_height_featureChannelCount_batchSize
  , descriptorWithWidth_height_featureChannelCount_batchSize_dataType
  , convolutionWeightsDescriptorWithWidth_height_inputFeatureChannelCount_outputFeatureChannelCount_dataType
  , convolutionWeightsDescriptorWithInputFeatureChannelCount_outputFeatureChannelCount_dataType
  , convolutionBiasesDescriptorWithFeatureChannelCount_dataType
  , dataType
  , dimensionCount
  , shape
  , stride
  , tensorAllocationSizeInBytes
  , sequenceLengths
  , sortedSequences
  , batchSizePerSequenceStep
  , maxTensorDimensions
  , batchSizePerSequenceStepSelector
  , convolutionBiasesDescriptorWithFeatureChannelCount_dataTypeSelector
  , convolutionWeightsDescriptorWithInputFeatureChannelCount_outputFeatureChannelCount_dataTypeSelector
  , convolutionWeightsDescriptorWithWidth_height_inputFeatureChannelCount_outputFeatureChannelCount_dataTypeSelector
  , dataTypeSelector
  , descriptorWithShape_dataTypeSelector
  , descriptorWithShape_sequenceLengths_sortedSequences_dataTypeSelector
  , descriptorWithWidth_height_featureChannelCount_batchSizeSelector
  , descriptorWithWidth_height_featureChannelCount_batchSize_dataTypeSelector
  , dimensionCountSelector
  , initSelector
  , maxTensorDimensionsSelector
  , newSelector
  , sequenceLengthsSelector
  , shapeSelector
  , sortedSequencesSelector
  , strideSelector
  , tensorAllocationSizeInBytesSelector

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

  ) where

import Foreign.Ptr (Ptr, FunPtr)
import Foreign.C.Types

import ObjC.Runtime.Types
import ObjC.Runtime.Message (sendMessage, sendOwnedMessage, sendClassMessage, sendOwnedClassMessage)
import ObjC.Runtime.Selector (mkSelector)
import ObjC.Runtime.Class (getRequiredClass)

import ObjC.MLCompute.Internal.Classes
import ObjC.MLCompute.Internal.Enums
import ObjC.Foundation.Internal.Classes

-- | @+ new@
new :: IO (Id MLCTensorDescriptor)
new  =
  do
    cls' <- getRequiredClass "MLCTensorDescriptor"
    sendOwnedClassMessage cls' newSelector

-- | @- init@
init_ :: IsMLCTensorDescriptor mlcTensorDescriptor => mlcTensorDescriptor -> IO (Id MLCTensorDescriptor)
init_ mlcTensorDescriptor =
  sendOwnedMessage mlcTensorDescriptor initSelector

-- | Create a MLCTensorDescriptor object
--
-- @shape@ — The tensor shape
--
-- @dataType@ — The tensor data type
--
-- Returns: A new MLCTensorDescriptor object or nil if failure.
--
-- ObjC selector: @+ descriptorWithShape:dataType:@
descriptorWithShape_dataType :: IsNSArray shape => shape -> MLCDataType -> IO (Id MLCTensorDescriptor)
descriptorWithShape_dataType shape dataType =
  do
    cls' <- getRequiredClass "MLCTensorDescriptor"
    sendClassMessage cls' descriptorWithShape_dataTypeSelector (toNSArray shape) dataType

-- | Create a MLCTensorDescriptor object
--
-- @shape@ — The tensor shape
--
-- @sequenceLengths@ — The sequence lengths in tensor
--
-- @sortedSequences@ — A boolean to indicate whether sequences are sorted
--
-- @dataType@ — The tensor data type
--
-- Returns: A new MLCTensorDescriptor object or nil if failure.
--
-- This method is provided as an easy to use API to create sequence tensors used by recurrent layers.
--
-- ObjC selector: @+ descriptorWithShape:sequenceLengths:sortedSequences:dataType:@
descriptorWithShape_sequenceLengths_sortedSequences_dataType :: (IsNSArray shape, IsNSArray sequenceLengths) => shape -> sequenceLengths -> Bool -> MLCDataType -> IO (Id MLCTensorDescriptor)
descriptorWithShape_sequenceLengths_sortedSequences_dataType shape sequenceLengths sortedSequences dataType =
  do
    cls' <- getRequiredClass "MLCTensorDescriptor"
    sendClassMessage cls' descriptorWithShape_sequenceLengths_sortedSequences_dataTypeSelector (toNSArray shape) (toNSArray sequenceLengths) sortedSequences dataType

-- | Create a MLCTensorDescriptor object
--
-- @width@ — The tensor width
--
-- @height@ — The tensor height
--
-- @featureChannels@ — The number of feature channels
--
-- @batchSize@ — The batch size
--
-- Returns: A new MLCTensorDescriptor object or nil if failure.
--
-- This method is provided as an easy to use API to create [NCHW] tensors used by convolutional layers.
--
-- ObjC selector: @+ descriptorWithWidth:height:featureChannelCount:batchSize:@
descriptorWithWidth_height_featureChannelCount_batchSize :: CULong -> CULong -> CULong -> CULong -> IO (Id MLCTensorDescriptor)
descriptorWithWidth_height_featureChannelCount_batchSize width height featureChannels batchSize =
  do
    cls' <- getRequiredClass "MLCTensorDescriptor"
    sendClassMessage cls' descriptorWithWidth_height_featureChannelCount_batchSizeSelector width height featureChannels batchSize

-- | Create a MLCTensorDescriptor object
--
-- @width@ — The tensor width
--
-- @height@ — The tensor height
--
-- @featureChannelCount@ — The number of feature channels
--
-- @batchSize@ — The batch size
--
-- @dataType@ — The tensor data type
--
-- Returns: A new MLCTensorDescriptor object or nil if failure.
--
-- This method is provided as an easy to use API to create [NCHW] tensors used by convolutional layers.
--
-- ObjC selector: @+ descriptorWithWidth:height:featureChannelCount:batchSize:dataType:@
descriptorWithWidth_height_featureChannelCount_batchSize_dataType :: CULong -> CULong -> CULong -> CULong -> MLCDataType -> IO (Id MLCTensorDescriptor)
descriptorWithWidth_height_featureChannelCount_batchSize_dataType width height featureChannelCount batchSize dataType =
  do
    cls' <- getRequiredClass "MLCTensorDescriptor"
    sendClassMessage cls' descriptorWithWidth_height_featureChannelCount_batchSize_dataTypeSelector width height featureChannelCount batchSize dataType

-- | Create a MLCTensorDescriptor object
--
-- @width@ — The tensor width
--
-- @height@ — The tensor height
--
-- @inputFeatureChannelCount@ — The number of input feature channels
--
-- @outputFeatureChannelCount@ — The number of output feature channels
--
-- @dataType@ — The tensor data type
--
-- Returns: A new MLCTensorDescriptor object or nil if failure.
--
-- This method is provided as an easy to use API to create a weight tensor.
--
-- ObjC selector: @+ convolutionWeightsDescriptorWithWidth:height:inputFeatureChannelCount:outputFeatureChannelCount:dataType:@
convolutionWeightsDescriptorWithWidth_height_inputFeatureChannelCount_outputFeatureChannelCount_dataType :: CULong -> CULong -> CULong -> CULong -> MLCDataType -> IO (Id MLCTensorDescriptor)
convolutionWeightsDescriptorWithWidth_height_inputFeatureChannelCount_outputFeatureChannelCount_dataType width height inputFeatureChannelCount outputFeatureChannelCount dataType =
  do
    cls' <- getRequiredClass "MLCTensorDescriptor"
    sendClassMessage cls' convolutionWeightsDescriptorWithWidth_height_inputFeatureChannelCount_outputFeatureChannelCount_dataTypeSelector width height inputFeatureChannelCount outputFeatureChannelCount dataType

-- | Create a MLCTensorDescriptor object
--
-- @inputFeatureChannelCount@ — The number of input feature channels
--
-- @outputFeatureChannelCount@ — The number of output feature channels
--
-- @dataType@ — The tensor data type
--
-- Returns: A new MLCTensorDescriptor object or nil if failure.
--
-- This method is provided as an easy to use API to create a weight tensor for a kernel of size 1.
--
-- ObjC selector: @+ convolutionWeightsDescriptorWithInputFeatureChannelCount:outputFeatureChannelCount:dataType:@
convolutionWeightsDescriptorWithInputFeatureChannelCount_outputFeatureChannelCount_dataType :: CULong -> CULong -> MLCDataType -> IO (Id MLCTensorDescriptor)
convolutionWeightsDescriptorWithInputFeatureChannelCount_outputFeatureChannelCount_dataType inputFeatureChannelCount outputFeatureChannelCount dataType =
  do
    cls' <- getRequiredClass "MLCTensorDescriptor"
    sendClassMessage cls' convolutionWeightsDescriptorWithInputFeatureChannelCount_outputFeatureChannelCount_dataTypeSelector inputFeatureChannelCount outputFeatureChannelCount dataType

-- | Create a MLCTensorDescriptor object
--
-- @featureChannelCount@ — The number of input feature channels
--
-- @dataType@ — The tensor data type
--
-- Returns: A new MLCTensorDescriptor object or nil if failure.
--
-- This method is provided as an easy to use API to create a bias tensor.
--
-- ObjC selector: @+ convolutionBiasesDescriptorWithFeatureChannelCount:dataType:@
convolutionBiasesDescriptorWithFeatureChannelCount_dataType :: CULong -> MLCDataType -> IO (Id MLCTensorDescriptor)
convolutionBiasesDescriptorWithFeatureChannelCount_dataType featureChannelCount dataType =
  do
    cls' <- getRequiredClass "MLCTensorDescriptor"
    sendClassMessage cls' convolutionBiasesDescriptorWithFeatureChannelCount_dataTypeSelector featureChannelCount dataType

-- | dataType
--
-- The tensor data type.  The default is MLCDataTypeFloat32.
--
-- ObjC selector: @- dataType@
dataType :: IsMLCTensorDescriptor mlcTensorDescriptor => mlcTensorDescriptor -> IO MLCDataType
dataType mlcTensorDescriptor =
  sendMessage mlcTensorDescriptor dataTypeSelector

-- | dimensionCount
--
-- The number of dimensions in the tensor
--
-- ObjC selector: @- dimensionCount@
dimensionCount :: IsMLCTensorDescriptor mlcTensorDescriptor => mlcTensorDescriptor -> IO CULong
dimensionCount mlcTensorDescriptor =
  sendMessage mlcTensorDescriptor dimensionCountSelector

-- | shape
--
-- The size in each dimension
--
-- ObjC selector: @- shape@
shape :: IsMLCTensorDescriptor mlcTensorDescriptor => mlcTensorDescriptor -> IO (Id NSArray)
shape mlcTensorDescriptor =
  sendMessage mlcTensorDescriptor shapeSelector

-- | stride
--
-- The stride in bytes in each dimension
--
-- ObjC selector: @- stride@
stride :: IsMLCTensorDescriptor mlcTensorDescriptor => mlcTensorDescriptor -> IO (Id NSArray)
stride mlcTensorDescriptor =
  sendMessage mlcTensorDescriptor strideSelector

-- | tensorAllocationSizeInBytes
--
-- The allocation size in bytes for a tensor.
--
-- ObjC selector: @- tensorAllocationSizeInBytes@
tensorAllocationSizeInBytes :: IsMLCTensorDescriptor mlcTensorDescriptor => mlcTensorDescriptor -> IO CULong
tensorAllocationSizeInBytes mlcTensorDescriptor =
  sendMessage mlcTensorDescriptor tensorAllocationSizeInBytesSelector

-- | sequenceLengths
--
-- TODO
--
-- ObjC selector: @- sequenceLengths@
sequenceLengths :: IsMLCTensorDescriptor mlcTensorDescriptor => mlcTensorDescriptor -> IO (Id NSArray)
sequenceLengths mlcTensorDescriptor =
  sendMessage mlcTensorDescriptor sequenceLengthsSelector

-- | sortedSequences
--
-- Specifies whether the sequences are sorted or not.
--
-- ObjC selector: @- sortedSequences@
sortedSequences :: IsMLCTensorDescriptor mlcTensorDescriptor => mlcTensorDescriptor -> IO Bool
sortedSequences mlcTensorDescriptor =
  sendMessage mlcTensorDescriptor sortedSequencesSelector

-- | batchSizePerSequenceStep
--
-- The batch size for each sequence
--
-- We populate this only when sequenceLengths is valid. The length of this array should be                the maximum sequence length in sequenceLengths (i.e sequenceLengths[0]).
--
-- ObjC selector: @- batchSizePerSequenceStep@
batchSizePerSequenceStep :: IsMLCTensorDescriptor mlcTensorDescriptor => mlcTensorDescriptor -> IO (Id NSArray)
batchSizePerSequenceStep mlcTensorDescriptor =
  sendMessage mlcTensorDescriptor batchSizePerSequenceStepSelector

-- | maxTensorDimensions
--
-- The maximum number of tensor dimensions supported
--
-- ObjC selector: @+ maxTensorDimensions@
maxTensorDimensions :: IO CULong
maxTensorDimensions  =
  do
    cls' <- getRequiredClass "MLCTensorDescriptor"
    sendClassMessage cls' maxTensorDimensionsSelector

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @new@
newSelector :: Selector '[] (Id MLCTensorDescriptor)
newSelector = mkSelector "new"

-- | @Selector@ for @init@
initSelector :: Selector '[] (Id MLCTensorDescriptor)
initSelector = mkSelector "init"

-- | @Selector@ for @descriptorWithShape:dataType:@
descriptorWithShape_dataTypeSelector :: Selector '[Id NSArray, MLCDataType] (Id MLCTensorDescriptor)
descriptorWithShape_dataTypeSelector = mkSelector "descriptorWithShape:dataType:"

-- | @Selector@ for @descriptorWithShape:sequenceLengths:sortedSequences:dataType:@
descriptorWithShape_sequenceLengths_sortedSequences_dataTypeSelector :: Selector '[Id NSArray, Id NSArray, Bool, MLCDataType] (Id MLCTensorDescriptor)
descriptorWithShape_sequenceLengths_sortedSequences_dataTypeSelector = mkSelector "descriptorWithShape:sequenceLengths:sortedSequences:dataType:"

-- | @Selector@ for @descriptorWithWidth:height:featureChannelCount:batchSize:@
descriptorWithWidth_height_featureChannelCount_batchSizeSelector :: Selector '[CULong, CULong, CULong, CULong] (Id MLCTensorDescriptor)
descriptorWithWidth_height_featureChannelCount_batchSizeSelector = mkSelector "descriptorWithWidth:height:featureChannelCount:batchSize:"

-- | @Selector@ for @descriptorWithWidth:height:featureChannelCount:batchSize:dataType:@
descriptorWithWidth_height_featureChannelCount_batchSize_dataTypeSelector :: Selector '[CULong, CULong, CULong, CULong, MLCDataType] (Id MLCTensorDescriptor)
descriptorWithWidth_height_featureChannelCount_batchSize_dataTypeSelector = mkSelector "descriptorWithWidth:height:featureChannelCount:batchSize:dataType:"

-- | @Selector@ for @convolutionWeightsDescriptorWithWidth:height:inputFeatureChannelCount:outputFeatureChannelCount:dataType:@
convolutionWeightsDescriptorWithWidth_height_inputFeatureChannelCount_outputFeatureChannelCount_dataTypeSelector :: Selector '[CULong, CULong, CULong, CULong, MLCDataType] (Id MLCTensorDescriptor)
convolutionWeightsDescriptorWithWidth_height_inputFeatureChannelCount_outputFeatureChannelCount_dataTypeSelector = mkSelector "convolutionWeightsDescriptorWithWidth:height:inputFeatureChannelCount:outputFeatureChannelCount:dataType:"

-- | @Selector@ for @convolutionWeightsDescriptorWithInputFeatureChannelCount:outputFeatureChannelCount:dataType:@
convolutionWeightsDescriptorWithInputFeatureChannelCount_outputFeatureChannelCount_dataTypeSelector :: Selector '[CULong, CULong, MLCDataType] (Id MLCTensorDescriptor)
convolutionWeightsDescriptorWithInputFeatureChannelCount_outputFeatureChannelCount_dataTypeSelector = mkSelector "convolutionWeightsDescriptorWithInputFeatureChannelCount:outputFeatureChannelCount:dataType:"

-- | @Selector@ for @convolutionBiasesDescriptorWithFeatureChannelCount:dataType:@
convolutionBiasesDescriptorWithFeatureChannelCount_dataTypeSelector :: Selector '[CULong, MLCDataType] (Id MLCTensorDescriptor)
convolutionBiasesDescriptorWithFeatureChannelCount_dataTypeSelector = mkSelector "convolutionBiasesDescriptorWithFeatureChannelCount:dataType:"

-- | @Selector@ for @dataType@
dataTypeSelector :: Selector '[] MLCDataType
dataTypeSelector = mkSelector "dataType"

-- | @Selector@ for @dimensionCount@
dimensionCountSelector :: Selector '[] CULong
dimensionCountSelector = mkSelector "dimensionCount"

-- | @Selector@ for @shape@
shapeSelector :: Selector '[] (Id NSArray)
shapeSelector = mkSelector "shape"

-- | @Selector@ for @stride@
strideSelector :: Selector '[] (Id NSArray)
strideSelector = mkSelector "stride"

-- | @Selector@ for @tensorAllocationSizeInBytes@
tensorAllocationSizeInBytesSelector :: Selector '[] CULong
tensorAllocationSizeInBytesSelector = mkSelector "tensorAllocationSizeInBytes"

-- | @Selector@ for @sequenceLengths@
sequenceLengthsSelector :: Selector '[] (Id NSArray)
sequenceLengthsSelector = mkSelector "sequenceLengths"

-- | @Selector@ for @sortedSequences@
sortedSequencesSelector :: Selector '[] Bool
sortedSequencesSelector = mkSelector "sortedSequences"

-- | @Selector@ for @batchSizePerSequenceStep@
batchSizePerSequenceStepSelector :: Selector '[] (Id NSArray)
batchSizePerSequenceStepSelector = mkSelector "batchSizePerSequenceStep"

-- | @Selector@ for @maxTensorDimensions@
maxTensorDimensionsSelector :: Selector '[] CULong
maxTensorDimensionsSelector = mkSelector "maxTensorDimensions"

