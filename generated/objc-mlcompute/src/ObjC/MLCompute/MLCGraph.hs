{-# LANGUAGE PatternSynonyms #-}
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
  , graphSelector
  , nodeWithLayer_sourceSelector
  , nodeWithLayer_sourcesSelector
  , nodeWithLayer_sources_disableUpdateSelector
  , nodeWithLayer_sources_lossLabelsSelector
  , splitWithSource_splitCount_dimensionSelector
  , splitWithSource_splitSectionLengths_dimensionSelector
  , concatenateWithSources_dimensionSelector
  , reshapeWithShape_sourceSelector
  , transposeWithDimensions_sourceSelector
  , selectWithSources_conditionSelector
  , scatterWithDimension_source_indices_copyFrom_reductionTypeSelector
  , gatherWithDimension_source_indicesSelector
  , bindAndWriteData_forInputs_toDevice_batchSize_synchronousSelector
  , bindAndWriteData_forInputs_toDevice_synchronousSelector
  , sourceTensorsForLayerSelector
  , resultTensorsForLayerSelector
  , deviceSelector
  , layersSelector
  , summarizedDOTDescriptionSelector

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

-- | Creates a new graph.
--
-- Returns: A new graph.
--
-- ObjC selector: @+ graph@
graph :: IO (Id MLCGraph)
graph  =
  do
    cls' <- getRequiredClass "MLCGraph"
    sendClassMsg cls' (mkSelector "graph") (retPtr retVoid) [] >>= retainedObject . castPtr

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
nodeWithLayer_source mlcGraph  layer source =
withObjCPtr layer $ \raw_layer ->
  withObjCPtr source $ \raw_source ->
      sendMsg mlcGraph (mkSelector "nodeWithLayer:source:") (retPtr retVoid) [argPtr (castPtr raw_layer :: Ptr ()), argPtr (castPtr raw_source :: Ptr ())] >>= retainedObject . castPtr

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
nodeWithLayer_sources mlcGraph  layer sources =
withObjCPtr layer $ \raw_layer ->
  withObjCPtr sources $ \raw_sources ->
      sendMsg mlcGraph (mkSelector "nodeWithLayer:sources:") (retPtr retVoid) [argPtr (castPtr raw_layer :: Ptr ()), argPtr (castPtr raw_sources :: Ptr ())] >>= retainedObject . castPtr

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
nodeWithLayer_sources_disableUpdate mlcGraph  layer sources disableUpdate =
withObjCPtr layer $ \raw_layer ->
  withObjCPtr sources $ \raw_sources ->
      sendMsg mlcGraph (mkSelector "nodeWithLayer:sources:disableUpdate:") (retPtr retVoid) [argPtr (castPtr raw_layer :: Ptr ()), argPtr (castPtr raw_sources :: Ptr ()), argCULong (if disableUpdate then 1 else 0)] >>= retainedObject . castPtr

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
nodeWithLayer_sources_lossLabels mlcGraph  layer sources lossLabels =
withObjCPtr layer $ \raw_layer ->
  withObjCPtr sources $ \raw_sources ->
    withObjCPtr lossLabels $ \raw_lossLabels ->
        sendMsg mlcGraph (mkSelector "nodeWithLayer:sources:lossLabels:") (retPtr retVoid) [argPtr (castPtr raw_layer :: Ptr ()), argPtr (castPtr raw_sources :: Ptr ()), argPtr (castPtr raw_lossLabels :: Ptr ())] >>= retainedObject . castPtr

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
splitWithSource_splitCount_dimension mlcGraph  source splitCount dimension =
withObjCPtr source $ \raw_source ->
    sendMsg mlcGraph (mkSelector "splitWithSource:splitCount:dimension:") (retPtr retVoid) [argPtr (castPtr raw_source :: Ptr ()), argCULong (fromIntegral splitCount), argCULong (fromIntegral dimension)] >>= retainedObject . castPtr

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
splitWithSource_splitSectionLengths_dimension mlcGraph  source splitSectionLengths dimension =
withObjCPtr source $ \raw_source ->
  withObjCPtr splitSectionLengths $ \raw_splitSectionLengths ->
      sendMsg mlcGraph (mkSelector "splitWithSource:splitSectionLengths:dimension:") (retPtr retVoid) [argPtr (castPtr raw_source :: Ptr ()), argPtr (castPtr raw_splitSectionLengths :: Ptr ()), argCULong (fromIntegral dimension)] >>= retainedObject . castPtr

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
concatenateWithSources_dimension mlcGraph  sources dimension =
withObjCPtr sources $ \raw_sources ->
    sendMsg mlcGraph (mkSelector "concatenateWithSources:dimension:") (retPtr retVoid) [argPtr (castPtr raw_sources :: Ptr ()), argCULong (fromIntegral dimension)] >>= retainedObject . castPtr

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
reshapeWithShape_source mlcGraph  shape source =
withObjCPtr shape $ \raw_shape ->
  withObjCPtr source $ \raw_source ->
      sendMsg mlcGraph (mkSelector "reshapeWithShape:source:") (retPtr retVoid) [argPtr (castPtr raw_shape :: Ptr ()), argPtr (castPtr raw_source :: Ptr ())] >>= retainedObject . castPtr

-- | Add a transpose layer to the graph
--
-- @dimensions@ — NSArray<NSNumber *> representing the desired ordering of dimensions                The dimensions array specifies the input axis source for each output axis, such that the                K'th element in the dimensions array specifies the input axis source for the K'th axis in the                output.  The batch dimension which is typically axis 0 cannot be transposed.
--
-- Returns: A result tensor
--
-- ObjC selector: @- transposeWithDimensions:source:@
transposeWithDimensions_source :: (IsMLCGraph mlcGraph, IsNSArray dimensions, IsMLCTensor source) => mlcGraph -> dimensions -> source -> IO (Id MLCTensor)
transposeWithDimensions_source mlcGraph  dimensions source =
withObjCPtr dimensions $ \raw_dimensions ->
  withObjCPtr source $ \raw_source ->
      sendMsg mlcGraph (mkSelector "transposeWithDimensions:source:") (retPtr retVoid) [argPtr (castPtr raw_dimensions :: Ptr ()), argPtr (castPtr raw_source :: Ptr ())] >>= retainedObject . castPtr

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
selectWithSources_condition mlcGraph  sources condition =
withObjCPtr sources $ \raw_sources ->
  withObjCPtr condition $ \raw_condition ->
      sendMsg mlcGraph (mkSelector "selectWithSources:condition:") (retPtr retVoid) [argPtr (castPtr raw_sources :: Ptr ()), argPtr (castPtr raw_condition :: Ptr ())] >>= retainedObject . castPtr

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
scatterWithDimension_source_indices_copyFrom_reductionType mlcGraph  dimension source indices copyFrom reductionType =
withObjCPtr source $ \raw_source ->
  withObjCPtr indices $ \raw_indices ->
    withObjCPtr copyFrom $ \raw_copyFrom ->
        sendMsg mlcGraph (mkSelector "scatterWithDimension:source:indices:copyFrom:reductionType:") (retPtr retVoid) [argCULong (fromIntegral dimension), argPtr (castPtr raw_source :: Ptr ()), argPtr (castPtr raw_indices :: Ptr ()), argPtr (castPtr raw_copyFrom :: Ptr ()), argCInt (coerce reductionType)] >>= retainedObject . castPtr

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
gatherWithDimension_source_indices mlcGraph  dimension source indices =
withObjCPtr source $ \raw_source ->
  withObjCPtr indices $ \raw_indices ->
      sendMsg mlcGraph (mkSelector "gatherWithDimension:source:indices:") (retPtr retVoid) [argCULong (fromIntegral dimension), argPtr (castPtr raw_source :: Ptr ()), argPtr (castPtr raw_indices :: Ptr ())] >>= retainedObject . castPtr

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
bindAndWriteData_forInputs_toDevice_batchSize_synchronous mlcGraph  inputsData inputTensors device batchSize synchronous =
withObjCPtr inputsData $ \raw_inputsData ->
  withObjCPtr inputTensors $ \raw_inputTensors ->
    withObjCPtr device $ \raw_device ->
        fmap ((/= 0) :: CULong -> Bool) $ sendMsg mlcGraph (mkSelector "bindAndWriteData:forInputs:toDevice:batchSize:synchronous:") retCULong [argPtr (castPtr raw_inputsData :: Ptr ()), argPtr (castPtr raw_inputTensors :: Ptr ()), argPtr (castPtr raw_device :: Ptr ()), argCULong (fromIntegral batchSize), argCULong (if synchronous then 1 else 0)]

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
bindAndWriteData_forInputs_toDevice_synchronous mlcGraph  inputsData inputTensors device synchronous =
withObjCPtr inputsData $ \raw_inputsData ->
  withObjCPtr inputTensors $ \raw_inputTensors ->
    withObjCPtr device $ \raw_device ->
        fmap ((/= 0) :: CULong -> Bool) $ sendMsg mlcGraph (mkSelector "bindAndWriteData:forInputs:toDevice:synchronous:") retCULong [argPtr (castPtr raw_inputsData :: Ptr ()), argPtr (castPtr raw_inputTensors :: Ptr ()), argPtr (castPtr raw_device :: Ptr ()), argCULong (if synchronous then 1 else 0)]

-- | Get the source tensors for a layer in the training graph
--
-- @layer@ — A layer in the training graph
--
-- Returns: A list of tensors
--
-- ObjC selector: @- sourceTensorsForLayer:@
sourceTensorsForLayer :: (IsMLCGraph mlcGraph, IsMLCLayer layer) => mlcGraph -> layer -> IO (Id NSArray)
sourceTensorsForLayer mlcGraph  layer =
withObjCPtr layer $ \raw_layer ->
    sendMsg mlcGraph (mkSelector "sourceTensorsForLayer:") (retPtr retVoid) [argPtr (castPtr raw_layer :: Ptr ())] >>= retainedObject . castPtr

-- | Get the result tensors for a layer in the training graph
--
-- @layer@ — A layer in the training graph
--
-- Returns: A list of tensors
--
-- ObjC selector: @- resultTensorsForLayer:@
resultTensorsForLayer :: (IsMLCGraph mlcGraph, IsMLCLayer layer) => mlcGraph -> layer -> IO (Id NSArray)
resultTensorsForLayer mlcGraph  layer =
withObjCPtr layer $ \raw_layer ->
    sendMsg mlcGraph (mkSelector "resultTensorsForLayer:") (retPtr retVoid) [argPtr (castPtr raw_layer :: Ptr ())] >>= retainedObject . castPtr

-- | The device to be used when compiling and executing a graph
--
-- ObjC selector: @- device@
device :: IsMLCGraph mlcGraph => mlcGraph -> IO (Id MLCDevice)
device mlcGraph  =
  sendMsg mlcGraph (mkSelector "device") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | Layers in the graph
--
-- ObjC selector: @- layers@
layers :: IsMLCGraph mlcGraph => mlcGraph -> IO (Id NSArray)
layers mlcGraph  =
  sendMsg mlcGraph (mkSelector "layers") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | A DOT representation of the graph.
--
-- For more info on the DOT language, refer to https://en.wikipedia.org/wiki/DOT_(graph_description_language).                Edges that have a dashed lines are those that have stop gradients, while those with solid lines don't.
--
-- ObjC selector: @- summarizedDOTDescription@
summarizedDOTDescription :: IsMLCGraph mlcGraph => mlcGraph -> IO (Id NSString)
summarizedDOTDescription mlcGraph  =
  sendMsg mlcGraph (mkSelector "summarizedDOTDescription") (retPtr retVoid) [] >>= retainedObject . castPtr

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @graph@
graphSelector :: Selector
graphSelector = mkSelector "graph"

-- | @Selector@ for @nodeWithLayer:source:@
nodeWithLayer_sourceSelector :: Selector
nodeWithLayer_sourceSelector = mkSelector "nodeWithLayer:source:"

-- | @Selector@ for @nodeWithLayer:sources:@
nodeWithLayer_sourcesSelector :: Selector
nodeWithLayer_sourcesSelector = mkSelector "nodeWithLayer:sources:"

-- | @Selector@ for @nodeWithLayer:sources:disableUpdate:@
nodeWithLayer_sources_disableUpdateSelector :: Selector
nodeWithLayer_sources_disableUpdateSelector = mkSelector "nodeWithLayer:sources:disableUpdate:"

-- | @Selector@ for @nodeWithLayer:sources:lossLabels:@
nodeWithLayer_sources_lossLabelsSelector :: Selector
nodeWithLayer_sources_lossLabelsSelector = mkSelector "nodeWithLayer:sources:lossLabels:"

-- | @Selector@ for @splitWithSource:splitCount:dimension:@
splitWithSource_splitCount_dimensionSelector :: Selector
splitWithSource_splitCount_dimensionSelector = mkSelector "splitWithSource:splitCount:dimension:"

-- | @Selector@ for @splitWithSource:splitSectionLengths:dimension:@
splitWithSource_splitSectionLengths_dimensionSelector :: Selector
splitWithSource_splitSectionLengths_dimensionSelector = mkSelector "splitWithSource:splitSectionLengths:dimension:"

-- | @Selector@ for @concatenateWithSources:dimension:@
concatenateWithSources_dimensionSelector :: Selector
concatenateWithSources_dimensionSelector = mkSelector "concatenateWithSources:dimension:"

-- | @Selector@ for @reshapeWithShape:source:@
reshapeWithShape_sourceSelector :: Selector
reshapeWithShape_sourceSelector = mkSelector "reshapeWithShape:source:"

-- | @Selector@ for @transposeWithDimensions:source:@
transposeWithDimensions_sourceSelector :: Selector
transposeWithDimensions_sourceSelector = mkSelector "transposeWithDimensions:source:"

-- | @Selector@ for @selectWithSources:condition:@
selectWithSources_conditionSelector :: Selector
selectWithSources_conditionSelector = mkSelector "selectWithSources:condition:"

-- | @Selector@ for @scatterWithDimension:source:indices:copyFrom:reductionType:@
scatterWithDimension_source_indices_copyFrom_reductionTypeSelector :: Selector
scatterWithDimension_source_indices_copyFrom_reductionTypeSelector = mkSelector "scatterWithDimension:source:indices:copyFrom:reductionType:"

-- | @Selector@ for @gatherWithDimension:source:indices:@
gatherWithDimension_source_indicesSelector :: Selector
gatherWithDimension_source_indicesSelector = mkSelector "gatherWithDimension:source:indices:"

-- | @Selector@ for @bindAndWriteData:forInputs:toDevice:batchSize:synchronous:@
bindAndWriteData_forInputs_toDevice_batchSize_synchronousSelector :: Selector
bindAndWriteData_forInputs_toDevice_batchSize_synchronousSelector = mkSelector "bindAndWriteData:forInputs:toDevice:batchSize:synchronous:"

-- | @Selector@ for @bindAndWriteData:forInputs:toDevice:synchronous:@
bindAndWriteData_forInputs_toDevice_synchronousSelector :: Selector
bindAndWriteData_forInputs_toDevice_synchronousSelector = mkSelector "bindAndWriteData:forInputs:toDevice:synchronous:"

-- | @Selector@ for @sourceTensorsForLayer:@
sourceTensorsForLayerSelector :: Selector
sourceTensorsForLayerSelector = mkSelector "sourceTensorsForLayer:"

-- | @Selector@ for @resultTensorsForLayer:@
resultTensorsForLayerSelector :: Selector
resultTensorsForLayerSelector = mkSelector "resultTensorsForLayer:"

-- | @Selector@ for @device@
deviceSelector :: Selector
deviceSelector = mkSelector "device"

-- | @Selector@ for @layers@
layersSelector :: Selector
layersSelector = mkSelector "layers"

-- | @Selector@ for @summarizedDOTDescription@
summarizedDOTDescriptionSelector :: Selector
summarizedDOTDescriptionSelector = mkSelector "summarizedDOTDescription"

