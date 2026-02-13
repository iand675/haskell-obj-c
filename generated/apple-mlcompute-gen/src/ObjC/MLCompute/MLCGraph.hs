{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | MLCGraph
--
-- A graph of layers that can be used to build a training or inference graph
--
-- Generated bindings for @MLCGraph@.
module ObjC.MLCompute.MLCGraph
  ( MLCGraph
  , IsMLCGraph(..)
  , graph
  , nodeWithLayer_source
  , nodeWithLayer_sources
  , nodeWithLayer_sources_disableUpdate
  , nodeWithLayer_sources_lossLabels
  , splitWithSource_splitCount_dimension
  , splitWithSource_splitSectionLengths_dimension
  , concatenateWithSources_dimension
  , reshapeWithShape_source
  , transposeWithDimensions_source
  , selectWithSources_condition
  , scatterWithDimension_source_indices_copyFrom_reductionType
  , gatherWithDimension_source_indices
  , bindAndWriteData_forInputs_toDevice_batchSize_synchronous
  , bindAndWriteData_forInputs_toDevice_synchronous
  , sourceTensorsForLayer
  , resultTensorsForLayer
  , device
  , layers
  , summarizedDOTDescription
  , bindAndWriteData_forInputs_toDevice_batchSize_synchronousSelector
  , bindAndWriteData_forInputs_toDevice_synchronousSelector
  , concatenateWithSources_dimensionSelector
  , deviceSelector
  , gatherWithDimension_source_indicesSelector
  , graphSelector
  , layersSelector
  , nodeWithLayer_sourceSelector
  , nodeWithLayer_sourcesSelector
  , nodeWithLayer_sources_disableUpdateSelector
  , nodeWithLayer_sources_lossLabelsSelector
  , reshapeWithShape_sourceSelector
  , resultTensorsForLayerSelector
  , scatterWithDimension_source_indices_copyFrom_reductionTypeSelector
  , selectWithSources_conditionSelector
  , sourceTensorsForLayerSelector
  , splitWithSource_splitCount_dimensionSelector
  , splitWithSource_splitSectionLengths_dimensionSelector
  , summarizedDOTDescriptionSelector
  , transposeWithDimensions_sourceSelector

  -- * Enum types
  , MLCReductionType(MLCReductionType)
  , pattern MLCReductionTypeNone
  , pattern MLCReductionTypeSum
  , pattern MLCReductionTypeMean
  , pattern MLCReductionTypeMax
  , pattern MLCReductionTypeMin
  , pattern MLCReductionTypeArgMax
  , pattern MLCReductionTypeArgMin
  , pattern MLCReductionTypeL1Norm
  , pattern MLCReductionTypeAny
  , pattern MLCReductionTypeAll
  , pattern MLCReductionTypeCount

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

-- | Creates a new graph.
--
-- Returns: A new graph.
--
-- ObjC selector: @+ graph@
graph :: IO (Id MLCGraph)
graph  =
  do
    cls' <- getRequiredClass "MLCGraph"
    sendClassMessage cls' graphSelector

-- | Add a layer to the graph
--
-- @layer@ — The layer
--
-- @source@ — The source tensor
--
-- Returns: A result tensor
--
-- ObjC selector: @- nodeWithLayer:source:@
nodeWithLayer_source :: (IsMLCGraph mlcGraph, IsMLCLayer layer, IsMLCTensor source) => mlcGraph -> layer -> source -> IO (Id MLCTensor)
nodeWithLayer_source mlcGraph layer source =
  sendMessage mlcGraph nodeWithLayer_sourceSelector (toMLCLayer layer) (toMLCTensor source)

-- | Add a layer to the graph
--
-- @layer@ — The layer
--
-- @sources@ — A list of source tensors
--
-- For variable length sequences of LSTMs/RNNs layers, create an MLCTensor of sortedSequenceLengths and pass it as the last index (i.e. index 2 or 4) of sources. This tensor must of be type MLCDataTypeInt32.
--
-- Returns: A result tensor
--
-- ObjC selector: @- nodeWithLayer:sources:@
nodeWithLayer_sources :: (IsMLCGraph mlcGraph, IsMLCLayer layer, IsNSArray sources) => mlcGraph -> layer -> sources -> IO (Id MLCTensor)
nodeWithLayer_sources mlcGraph layer sources =
  sendMessage mlcGraph nodeWithLayer_sourcesSelector (toMLCLayer layer) (toNSArray sources)

-- | Add a layer to the graph
--
-- @layer@ — The layer
--
-- @sources@ — A list of source tensors
--
-- @disableUpdate@ — A flag to indicate if optimizer update should be disabled for this layer
--
-- For variable length sequences of LSTMs/RNNs layers, create an MLCTensor of sortedSequenceLengths and pass it as the last index (i.e. index 2 or 4) of sources. This tensor must of be type MLCDataTypeInt32.
--
-- Returns: A result tensor
--
-- ObjC selector: @- nodeWithLayer:sources:disableUpdate:@
nodeWithLayer_sources_disableUpdate :: (IsMLCGraph mlcGraph, IsMLCLayer layer, IsNSArray sources) => mlcGraph -> layer -> sources -> Bool -> IO (Id MLCTensor)
nodeWithLayer_sources_disableUpdate mlcGraph layer sources disableUpdate =
  sendMessage mlcGraph nodeWithLayer_sources_disableUpdateSelector (toMLCLayer layer) (toNSArray sources) disableUpdate

-- | Add a loss layer to the graph
--
-- @layer@ — The loss layer
--
-- @lossLabels@ — The loss labels tensor
--
-- For variable length sequences of LSTMs/RNNs layers, create an MLCTensor of sortedSequenceLengths and pass it as the last index (i.e. index 2 or 4) of sources. This tensor must of be type MLCDataTypeInt32.
--
-- Returns: A result tensor
--
-- ObjC selector: @- nodeWithLayer:sources:lossLabels:@
nodeWithLayer_sources_lossLabels :: (IsMLCGraph mlcGraph, IsMLCLayer layer, IsNSArray sources, IsNSArray lossLabels) => mlcGraph -> layer -> sources -> lossLabels -> IO (Id MLCTensor)
nodeWithLayer_sources_lossLabels mlcGraph layer sources lossLabels =
  sendMessage mlcGraph nodeWithLayer_sources_lossLabelsSelector (toMLCLayer layer) (toNSArray sources) (toNSArray lossLabels)

-- | Add a split layer to the graph
--
-- @source@ — The source tensor
--
-- @splitCount@ — The number of splits
--
-- @dimension@ — The dimension to split the source tensor
--
-- Returns: A result tensor
--
-- ObjC selector: @- splitWithSource:splitCount:dimension:@
splitWithSource_splitCount_dimension :: (IsMLCGraph mlcGraph, IsMLCTensor source) => mlcGraph -> source -> CULong -> CULong -> IO (Id NSArray)
splitWithSource_splitCount_dimension mlcGraph source splitCount dimension =
  sendMessage mlcGraph splitWithSource_splitCount_dimensionSelector (toMLCTensor source) splitCount dimension

-- | Add a split layer to the graph
--
-- @source@ — The source tensor
--
-- @splitSectionLengths@ — The lengths of each split section
--
-- @dimension@ — The dimension to split the source tensor
--
-- Returns: A result tensor
--
-- ObjC selector: @- splitWithSource:splitSectionLengths:dimension:@
splitWithSource_splitSectionLengths_dimension :: (IsMLCGraph mlcGraph, IsMLCTensor source, IsNSArray splitSectionLengths) => mlcGraph -> source -> splitSectionLengths -> CULong -> IO (Id NSArray)
splitWithSource_splitSectionLengths_dimension mlcGraph source splitSectionLengths dimension =
  sendMessage mlcGraph splitWithSource_splitSectionLengths_dimensionSelector (toMLCTensor source) (toNSArray splitSectionLengths) dimension

-- | Add a concat layer to the graph
--
-- @sources@ — The source tensors to concatenate
--
-- @dimension@ — The concatenation dimension
--
-- Returns: A result tensor
--
-- ObjC selector: @- concatenateWithSources:dimension:@
concatenateWithSources_dimension :: (IsMLCGraph mlcGraph, IsNSArray sources) => mlcGraph -> sources -> CULong -> IO (Id MLCTensor)
concatenateWithSources_dimension mlcGraph sources dimension =
  sendMessage mlcGraph concatenateWithSources_dimensionSelector (toNSArray sources) dimension

-- | Add a reshape layer to the graph
--
-- @shape@ — An array representing the shape of result tensor
--
-- @source@ — The source tensor
--
-- Returns: A result tensor
--
-- ObjC selector: @- reshapeWithShape:source:@
reshapeWithShape_source :: (IsMLCGraph mlcGraph, IsNSArray shape, IsMLCTensor source) => mlcGraph -> shape -> source -> IO (Id MLCTensor)
reshapeWithShape_source mlcGraph shape source =
  sendMessage mlcGraph reshapeWithShape_sourceSelector (toNSArray shape) (toMLCTensor source)

-- | Add a transpose layer to the graph
--
-- @dimensions@ — NSArray<NSNumber *> representing the desired ordering of dimensions                The dimensions array specifies the input axis source for each output axis, such that the                K'th element in the dimensions array specifies the input axis source for the K'th axis in the                output.  The batch dimension which is typically axis 0 cannot be transposed.
--
-- Returns: A result tensor
--
-- ObjC selector: @- transposeWithDimensions:source:@
transposeWithDimensions_source :: (IsMLCGraph mlcGraph, IsNSArray dimensions, IsMLCTensor source) => mlcGraph -> dimensions -> source -> IO (Id MLCTensor)
transposeWithDimensions_source mlcGraph dimensions source =
  sendMessage mlcGraph transposeWithDimensions_sourceSelector (toNSArray dimensions) (toMLCTensor source)

-- | Add a select layer to the graph
--
-- @sources@ — The source tensors
--
-- @condition@ — The condition mask
--
-- Returns: A result tensor
--
-- ObjC selector: @- selectWithSources:condition:@
selectWithSources_condition :: (IsMLCGraph mlcGraph, IsNSArray sources, IsMLCTensor condition) => mlcGraph -> sources -> condition -> IO (Id MLCTensor)
selectWithSources_condition mlcGraph sources condition =
  sendMessage mlcGraph selectWithSources_conditionSelector (toNSArray sources) (toMLCTensor condition)

-- | Add a scatter layer to the graph
--
-- @dimension@ — The dimension along which to index
--
-- @source@ — The updates to use with scattering with index positions specified in indices to result tensor
--
-- @indices@ — The index of elements to scatter
--
-- @copyFrom@ — The source tensor whose data is  to be first copied to the result tensor
--
-- @reductionType@ — The reduction type applied for all values in source tensor that are scattered to a specific location in the result tensor.                                Must be: MLCReductionTypeNone or MLCReductionTypeSum.
--
-- Returns: A result tensor
--
-- ObjC selector: @- scatterWithDimension:source:indices:copyFrom:reductionType:@
scatterWithDimension_source_indices_copyFrom_reductionType :: (IsMLCGraph mlcGraph, IsMLCTensor source, IsMLCTensor indices, IsMLCTensor copyFrom) => mlcGraph -> CULong -> source -> indices -> copyFrom -> MLCReductionType -> IO (Id MLCTensor)
scatterWithDimension_source_indices_copyFrom_reductionType mlcGraph dimension source indices copyFrom reductionType =
  sendMessage mlcGraph scatterWithDimension_source_indices_copyFrom_reductionTypeSelector dimension (toMLCTensor source) (toMLCTensor indices) (toMLCTensor copyFrom) reductionType

-- | Add a gather layer to the graph
--
-- @dimension@ — The dimension along which to index
--
-- @source@ — The source tensor
--
-- @indices@ — The index of elements to gather
--
-- Returns: A result tensor
--
-- ObjC selector: @- gatherWithDimension:source:indices:@
gatherWithDimension_source_indices :: (IsMLCGraph mlcGraph, IsMLCTensor source, IsMLCTensor indices) => mlcGraph -> CULong -> source -> indices -> IO (Id MLCTensor)
gatherWithDimension_source_indices mlcGraph dimension source indices =
  sendMessage mlcGraph gatherWithDimension_source_indicesSelector dimension (toMLCTensor source) (toMLCTensor indices)

-- | Associates data with input tensors. If the device is GPU, also copies the data to the device memory.                Returns true if the data is successfully associated with input tensors.
--
-- This function should be used if you execute the forward, gradient and optimizer updates independently.                Before the forward pass is executed, the inputs should be written to device memory.  Similarly, before the                gradient pass is executed, the inputs (typically the initial gradient tensor) should be written to device                memory.  The caller must guarantee the lifetime of the underlying memory of each value of @inputsData@                for the entirety of each corresponding input tensor's lifetime.
--
-- @inputsData@ — The input data to use to write to device memory
--
-- @inputTensors@ — The list of tensors to perform writes on
--
-- @device@ — The device
--
-- @batchSize@ — The batch size.  This should be set to the actual batch size that may be used when we execute                              the graph and can be a value less than or equal to the batch size specified in the tensor.                              If set to 0, we use batch size specified in the tensor.
--
-- @synchronous@ — Whether to execute the copy to the device synchronously.  For performance, asynchronous                             execution is recommended.
--
-- Returns: A Boolean value indicating whether the data is successfully associated with the tensor.
--
-- ObjC selector: @- bindAndWriteData:forInputs:toDevice:batchSize:synchronous:@
bindAndWriteData_forInputs_toDevice_batchSize_synchronous :: (IsMLCGraph mlcGraph, IsNSDictionary inputsData, IsNSDictionary inputTensors, IsMLCDevice device) => mlcGraph -> inputsData -> inputTensors -> device -> CULong -> Bool -> IO Bool
bindAndWriteData_forInputs_toDevice_batchSize_synchronous mlcGraph inputsData inputTensors device batchSize synchronous =
  sendMessage mlcGraph bindAndWriteData_forInputs_toDevice_batchSize_synchronousSelector (toNSDictionary inputsData) (toNSDictionary inputTensors) (toMLCDevice device) batchSize synchronous

-- | Associates data with input tensors. If the device is GPU, also copies the data to the device memory.                Returns true if the data is successfully associated with input tensors.
--
-- This function should be used if you execute the forward, gradient and optimizer updates independently.                Before the forward pass is executed, the inputs should be written to device memory.  Similarly, before the                gradient pass is executed, the inputs (typically the initial gradient tensor) should be written to device                memory.  The caller must guarantee the lifetime of the underlying memory of each value of @inputsData@                for the entirety of each corresponding input tensor's lifetime.
--
-- @inputsData@ — The input data to use to write to device memory
--
-- @inputTensors@ — The list of tensors to perform writes on
--
-- @device@ — The device
--
-- @synchronous@ — Whether to execute the copy to the device synchronously.  For performance, asynchronous                             execution is recommended.
--
-- Returns: A Boolean value indicating whether the data is successfully associated with the tensor.
--
-- ObjC selector: @- bindAndWriteData:forInputs:toDevice:synchronous:@
bindAndWriteData_forInputs_toDevice_synchronous :: (IsMLCGraph mlcGraph, IsNSDictionary inputsData, IsNSDictionary inputTensors, IsMLCDevice device) => mlcGraph -> inputsData -> inputTensors -> device -> Bool -> IO Bool
bindAndWriteData_forInputs_toDevice_synchronous mlcGraph inputsData inputTensors device synchronous =
  sendMessage mlcGraph bindAndWriteData_forInputs_toDevice_synchronousSelector (toNSDictionary inputsData) (toNSDictionary inputTensors) (toMLCDevice device) synchronous

-- | Get the source tensors for a layer in the training graph
--
-- @layer@ — A layer in the training graph
--
-- Returns: A list of tensors
--
-- ObjC selector: @- sourceTensorsForLayer:@
sourceTensorsForLayer :: (IsMLCGraph mlcGraph, IsMLCLayer layer) => mlcGraph -> layer -> IO (Id NSArray)
sourceTensorsForLayer mlcGraph layer =
  sendMessage mlcGraph sourceTensorsForLayerSelector (toMLCLayer layer)

-- | Get the result tensors for a layer in the training graph
--
-- @layer@ — A layer in the training graph
--
-- Returns: A list of tensors
--
-- ObjC selector: @- resultTensorsForLayer:@
resultTensorsForLayer :: (IsMLCGraph mlcGraph, IsMLCLayer layer) => mlcGraph -> layer -> IO (Id NSArray)
resultTensorsForLayer mlcGraph layer =
  sendMessage mlcGraph resultTensorsForLayerSelector (toMLCLayer layer)

-- | The device to be used when compiling and executing a graph
--
-- ObjC selector: @- device@
device :: IsMLCGraph mlcGraph => mlcGraph -> IO (Id MLCDevice)
device mlcGraph =
  sendMessage mlcGraph deviceSelector

-- | Layers in the graph
--
-- ObjC selector: @- layers@
layers :: IsMLCGraph mlcGraph => mlcGraph -> IO (Id NSArray)
layers mlcGraph =
  sendMessage mlcGraph layersSelector

-- | A DOT representation of the graph.
--
-- For more info on the DOT language, refer to https://en.wikipedia.org/wiki/DOT_(graph_description_language).                Edges that have a dashed lines are those that have stop gradients, while those with solid lines don't.
--
-- ObjC selector: @- summarizedDOTDescription@
summarizedDOTDescription :: IsMLCGraph mlcGraph => mlcGraph -> IO (Id NSString)
summarizedDOTDescription mlcGraph =
  sendMessage mlcGraph summarizedDOTDescriptionSelector

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @graph@
graphSelector :: Selector '[] (Id MLCGraph)
graphSelector = mkSelector "graph"

-- | @Selector@ for @nodeWithLayer:source:@
nodeWithLayer_sourceSelector :: Selector '[Id MLCLayer, Id MLCTensor] (Id MLCTensor)
nodeWithLayer_sourceSelector = mkSelector "nodeWithLayer:source:"

-- | @Selector@ for @nodeWithLayer:sources:@
nodeWithLayer_sourcesSelector :: Selector '[Id MLCLayer, Id NSArray] (Id MLCTensor)
nodeWithLayer_sourcesSelector = mkSelector "nodeWithLayer:sources:"

-- | @Selector@ for @nodeWithLayer:sources:disableUpdate:@
nodeWithLayer_sources_disableUpdateSelector :: Selector '[Id MLCLayer, Id NSArray, Bool] (Id MLCTensor)
nodeWithLayer_sources_disableUpdateSelector = mkSelector "nodeWithLayer:sources:disableUpdate:"

-- | @Selector@ for @nodeWithLayer:sources:lossLabels:@
nodeWithLayer_sources_lossLabelsSelector :: Selector '[Id MLCLayer, Id NSArray, Id NSArray] (Id MLCTensor)
nodeWithLayer_sources_lossLabelsSelector = mkSelector "nodeWithLayer:sources:lossLabels:"

-- | @Selector@ for @splitWithSource:splitCount:dimension:@
splitWithSource_splitCount_dimensionSelector :: Selector '[Id MLCTensor, CULong, CULong] (Id NSArray)
splitWithSource_splitCount_dimensionSelector = mkSelector "splitWithSource:splitCount:dimension:"

-- | @Selector@ for @splitWithSource:splitSectionLengths:dimension:@
splitWithSource_splitSectionLengths_dimensionSelector :: Selector '[Id MLCTensor, Id NSArray, CULong] (Id NSArray)
splitWithSource_splitSectionLengths_dimensionSelector = mkSelector "splitWithSource:splitSectionLengths:dimension:"

-- | @Selector@ for @concatenateWithSources:dimension:@
concatenateWithSources_dimensionSelector :: Selector '[Id NSArray, CULong] (Id MLCTensor)
concatenateWithSources_dimensionSelector = mkSelector "concatenateWithSources:dimension:"

-- | @Selector@ for @reshapeWithShape:source:@
reshapeWithShape_sourceSelector :: Selector '[Id NSArray, Id MLCTensor] (Id MLCTensor)
reshapeWithShape_sourceSelector = mkSelector "reshapeWithShape:source:"

-- | @Selector@ for @transposeWithDimensions:source:@
transposeWithDimensions_sourceSelector :: Selector '[Id NSArray, Id MLCTensor] (Id MLCTensor)
transposeWithDimensions_sourceSelector = mkSelector "transposeWithDimensions:source:"

-- | @Selector@ for @selectWithSources:condition:@
selectWithSources_conditionSelector :: Selector '[Id NSArray, Id MLCTensor] (Id MLCTensor)
selectWithSources_conditionSelector = mkSelector "selectWithSources:condition:"

-- | @Selector@ for @scatterWithDimension:source:indices:copyFrom:reductionType:@
scatterWithDimension_source_indices_copyFrom_reductionTypeSelector :: Selector '[CULong, Id MLCTensor, Id MLCTensor, Id MLCTensor, MLCReductionType] (Id MLCTensor)
scatterWithDimension_source_indices_copyFrom_reductionTypeSelector = mkSelector "scatterWithDimension:source:indices:copyFrom:reductionType:"

-- | @Selector@ for @gatherWithDimension:source:indices:@
gatherWithDimension_source_indicesSelector :: Selector '[CULong, Id MLCTensor, Id MLCTensor] (Id MLCTensor)
gatherWithDimension_source_indicesSelector = mkSelector "gatherWithDimension:source:indices:"

-- | @Selector@ for @bindAndWriteData:forInputs:toDevice:batchSize:synchronous:@
bindAndWriteData_forInputs_toDevice_batchSize_synchronousSelector :: Selector '[Id NSDictionary, Id NSDictionary, Id MLCDevice, CULong, Bool] Bool
bindAndWriteData_forInputs_toDevice_batchSize_synchronousSelector = mkSelector "bindAndWriteData:forInputs:toDevice:batchSize:synchronous:"

-- | @Selector@ for @bindAndWriteData:forInputs:toDevice:synchronous:@
bindAndWriteData_forInputs_toDevice_synchronousSelector :: Selector '[Id NSDictionary, Id NSDictionary, Id MLCDevice, Bool] Bool
bindAndWriteData_forInputs_toDevice_synchronousSelector = mkSelector "bindAndWriteData:forInputs:toDevice:synchronous:"

-- | @Selector@ for @sourceTensorsForLayer:@
sourceTensorsForLayerSelector :: Selector '[Id MLCLayer] (Id NSArray)
sourceTensorsForLayerSelector = mkSelector "sourceTensorsForLayer:"

-- | @Selector@ for @resultTensorsForLayer:@
resultTensorsForLayerSelector :: Selector '[Id MLCLayer] (Id NSArray)
resultTensorsForLayerSelector = mkSelector "resultTensorsForLayer:"

-- | @Selector@ for @device@
deviceSelector :: Selector '[] (Id MLCDevice)
deviceSelector = mkSelector "device"

-- | @Selector@ for @layers@
layersSelector :: Selector '[] (Id NSArray)
layersSelector = mkSelector "layers"

-- | @Selector@ for @summarizedDOTDescription@
summarizedDOTDescriptionSelector :: Selector '[] (Id NSString)
summarizedDOTDescriptionSelector = mkSelector "summarizedDOTDescription"

