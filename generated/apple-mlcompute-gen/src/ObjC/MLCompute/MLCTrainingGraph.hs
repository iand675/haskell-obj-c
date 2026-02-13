{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | MLCTrainingGraph
--
-- A training graph created from one or more MLCGraph objects                plus additional layers added directly to the training graph.
--
-- Generated bindings for @MLCTrainingGraph@.
module ObjC.MLCompute.MLCTrainingGraph
  ( MLCTrainingGraph
  , IsMLCTrainingGraph(..)
  , graphWithGraphObjects_lossLayer_optimizer
  , addInputs_lossLabels
  , addInputs_lossLabels_lossLabelWeights
  , addOutputs
  , stopGradientForTensors
  , compileWithOptions_device
  , compileWithOptions_device_inputTensors_inputTensorsData
  , compileOptimizer
  , linkWithGraphs
  , gradientTensorForInput
  , sourceGradientTensorsForLayer
  , resultGradientTensorsForLayer
  , gradientDataForParameter_layer
  , allocateUserGradientForTensor
  , executeWithInputsData_lossLabelsData_lossLabelWeightsData_batchSize_options_completionHandler
  , executeWithInputsData_lossLabelsData_lossLabelWeightsData_outputsData_batchSize_options_completionHandler
  , executeForwardWithBatchSize_options_completionHandler
  , executeForwardWithBatchSize_options_outputsData_completionHandler
  , executeGradientWithBatchSize_options_completionHandler
  , executeGradientWithBatchSize_options_outputsData_completionHandler
  , executeOptimizerUpdateWithOptions_completionHandler
  , synchronizeUpdates
  , setTrainingTensorParameters
  , bindOptimizerData_deviceData_withTensor
  , optimizer
  , deviceMemorySize
  , addInputs_lossLabelsSelector
  , addInputs_lossLabels_lossLabelWeightsSelector
  , addOutputsSelector
  , allocateUserGradientForTensorSelector
  , bindOptimizerData_deviceData_withTensorSelector
  , compileOptimizerSelector
  , compileWithOptions_deviceSelector
  , compileWithOptions_device_inputTensors_inputTensorsDataSelector
  , deviceMemorySizeSelector
  , executeForwardWithBatchSize_options_completionHandlerSelector
  , executeForwardWithBatchSize_options_outputsData_completionHandlerSelector
  , executeGradientWithBatchSize_options_completionHandlerSelector
  , executeGradientWithBatchSize_options_outputsData_completionHandlerSelector
  , executeOptimizerUpdateWithOptions_completionHandlerSelector
  , executeWithInputsData_lossLabelsData_lossLabelWeightsData_batchSize_options_completionHandlerSelector
  , executeWithInputsData_lossLabelsData_lossLabelWeightsData_outputsData_batchSize_options_completionHandlerSelector
  , gradientDataForParameter_layerSelector
  , gradientTensorForInputSelector
  , graphWithGraphObjects_lossLayer_optimizerSelector
  , linkWithGraphsSelector
  , optimizerSelector
  , resultGradientTensorsForLayerSelector
  , setTrainingTensorParametersSelector
  , sourceGradientTensorsForLayerSelector
  , stopGradientForTensorsSelector
  , synchronizeUpdatesSelector

  -- * Enum types
  , MLCExecutionOptions(MLCExecutionOptions)
  , pattern MLCExecutionOptionsNone
  , pattern MLCExecutionOptionsSkipWritingInputDataToDevice
  , pattern MLCExecutionOptionsSynchronous
  , pattern MLCExecutionOptionsProfiling
  , pattern MLCExecutionOptionsForwardForInference
  , pattern MLCExecutionOptionsPerLayerProfiling
  , MLCGraphCompilationOptions(MLCGraphCompilationOptions)
  , pattern MLCGraphCompilationOptionsNone
  , pattern MLCGraphCompilationOptionsDebugLayers
  , pattern MLCGraphCompilationOptionsDisableLayerFusion
  , pattern MLCGraphCompilationOptionsLinkGraphs
  , pattern MLCGraphCompilationOptionsComputeAllGradients

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

-- | Create a training graph
--
-- @graphObjects@ — The layers from these graph objects will be added to the training graph
--
-- @lossLayer@ — The loss layer to use.  The loss layer can also be added to the training graph                              using nodeWithLayer:sources:lossLabels
--
-- @optimizer@ — The optimizer to use
--
-- Returns: A new training graph object
--
-- ObjC selector: @+ graphWithGraphObjects:lossLayer:optimizer:@
graphWithGraphObjects_lossLayer_optimizer :: (IsNSArray graphObjects, IsMLCLayer lossLayer, IsMLCOptimizer optimizer) => graphObjects -> lossLayer -> optimizer -> IO (Id MLCTrainingGraph)
graphWithGraphObjects_lossLayer_optimizer graphObjects lossLayer optimizer =
  do
    cls' <- getRequiredClass "MLCTrainingGraph"
    sendClassMessage cls' graphWithGraphObjects_lossLayer_optimizerSelector (toNSArray graphObjects) (toMLCLayer lossLayer) (toMLCOptimizer optimizer)

-- | Add the list of inputs to the training graph
--
-- @inputs@ — The inputs
--
-- @lossLabels@ — The loss label inputs
--
-- Returns: A boolean indicating success or failure
--
-- ObjC selector: @- addInputs:lossLabels:@
addInputs_lossLabels :: (IsMLCTrainingGraph mlcTrainingGraph, IsNSDictionary inputs, IsNSDictionary lossLabels) => mlcTrainingGraph -> inputs -> lossLabels -> IO Bool
addInputs_lossLabels mlcTrainingGraph inputs lossLabels =
  sendMessage mlcTrainingGraph addInputs_lossLabelsSelector (toNSDictionary inputs) (toNSDictionary lossLabels)

-- | Add the list of inputs to the training graph
--
-- Each input, loss label or label weights tensor is identified by a NSString.                When the training graph is executed, this NSString is used to identify which data object                should be as input data for each tensor whose device memory needs to be updated                before the graph is executed.
--
-- @inputs@ — The inputs
--
-- @lossLabels@ — The loss label inputs
--
-- @lossLabelWeights@ — The loss label weights
--
-- Returns: A boolean indicating success or failure
--
-- ObjC selector: @- addInputs:lossLabels:lossLabelWeights:@
addInputs_lossLabels_lossLabelWeights :: (IsMLCTrainingGraph mlcTrainingGraph, IsNSDictionary inputs, IsNSDictionary lossLabels, IsNSDictionary lossLabelWeights) => mlcTrainingGraph -> inputs -> lossLabels -> lossLabelWeights -> IO Bool
addInputs_lossLabels_lossLabelWeights mlcTrainingGraph inputs lossLabels lossLabelWeights =
  sendMessage mlcTrainingGraph addInputs_lossLabels_lossLabelWeightsSelector (toNSDictionary inputs) (toNSDictionary lossLabels) (toNSDictionary lossLabelWeights)

-- | Add the list of outputs to the training graph
--
-- @outputs@ — The outputs
--
-- Returns: A boolean indicating success or failure
--
-- ObjC selector: @- addOutputs:@
addOutputs :: (IsMLCTrainingGraph mlcTrainingGraph, IsNSDictionary outputs) => mlcTrainingGraph -> outputs -> IO Bool
addOutputs mlcTrainingGraph outputs =
  sendMessage mlcTrainingGraph addOutputsSelector (toNSDictionary outputs)

-- | Add the list of tensors whose contributions are not to be taken when computing gradients during gradient pass
--
-- @tensors@ — The list of tensors
--
-- Returns: A boolean indicating success or failure
--
-- ObjC selector: @- stopGradientForTensors:@
stopGradientForTensors :: (IsMLCTrainingGraph mlcTrainingGraph, IsNSArray tensors) => mlcTrainingGraph -> tensors -> IO Bool
stopGradientForTensors mlcTrainingGraph tensors =
  sendMessage mlcTrainingGraph stopGradientForTensorsSelector (toNSArray tensors)

-- | Compile the training graph for a device.
--
-- @options@ — The compiler options to use when compiling the training graph
--
-- @device@ — The MLCDevice object
--
-- Returns: A boolean indicating success or failure
--
-- ObjC selector: @- compileWithOptions:device:@
compileWithOptions_device :: (IsMLCTrainingGraph mlcTrainingGraph, IsMLCDevice device) => mlcTrainingGraph -> MLCGraphCompilationOptions -> device -> IO Bool
compileWithOptions_device mlcTrainingGraph options device =
  sendMessage mlcTrainingGraph compileWithOptions_deviceSelector options (toMLCDevice device)

-- | Compile the training graph for a device.
--
-- Specifying the list of constant tensors when we compile the graph allows MLCompute to perform additional optimizations at compile time.
--
-- @options@ — The compiler options to use when compiling the training graph
--
-- @device@ — The MLCDevice object
--
-- @inputTensors@ — The list of input tensors that are constants
--
-- @inputTensorsData@ — The tensor data to be used with these constant input tensors
--
-- Returns: A boolean indicating success or failure
--
-- ObjC selector: @- compileWithOptions:device:inputTensors:inputTensorsData:@
compileWithOptions_device_inputTensors_inputTensorsData :: (IsMLCTrainingGraph mlcTrainingGraph, IsMLCDevice device, IsNSDictionary inputTensors, IsNSDictionary inputTensorsData) => mlcTrainingGraph -> MLCGraphCompilationOptions -> device -> inputTensors -> inputTensorsData -> IO Bool
compileWithOptions_device_inputTensors_inputTensorsData mlcTrainingGraph options device inputTensors inputTensorsData =
  sendMessage mlcTrainingGraph compileWithOptions_device_inputTensors_inputTensorsDataSelector options (toMLCDevice device) (toNSDictionary inputTensors) (toNSDictionary inputTensorsData)

-- | Compile the optimizer to be used with a training graph.
--
-- Typically the optimizer to be used with a training graph is specifed when the training graph is created using                graphWithGraphObjects:lossLayer:optimizer.  The optimizer will be compiled in when compileWithOptions:device                is called if an optimizer is specified with the training graph.  In the case where the optimizer to be used is not known                when the graph is created or compiled, this method can be used to associate and compile a training graph with an optimizer.
--
-- @optimizer@ — The MLCOptimizer object
--
-- Returns: A boolean indicating success or failure
--
-- ObjC selector: @- compileOptimizer:@
compileOptimizer :: (IsMLCTrainingGraph mlcTrainingGraph, IsMLCOptimizer optimizer) => mlcTrainingGraph -> optimizer -> IO Bool
compileOptimizer mlcTrainingGraph optimizer =
  sendMessage mlcTrainingGraph compileOptimizerSelector (toMLCOptimizer optimizer)

-- | Link mutiple training graphs
--
-- This is used to link subsequent training graphs with first training sub-graph.                This method should be used when we have tensors shared by one or more layers in multiple sub-graphs
--
-- @graphs@ — The list of training graphs to link
--
-- Returns: A boolean indicating success or failure
--
-- ObjC selector: @- linkWithGraphs:@
linkWithGraphs :: (IsMLCTrainingGraph mlcTrainingGraph, IsNSArray graphs) => mlcTrainingGraph -> graphs -> IO Bool
linkWithGraphs mlcTrainingGraph graphs =
  sendMessage mlcTrainingGraph linkWithGraphsSelector (toNSArray graphs)

-- | Get the gradient tensor for an input tensor
--
-- @input@ — The input tensor
--
-- Returns: The gradient tensor
--
-- ObjC selector: @- gradientTensorForInput:@
gradientTensorForInput :: (IsMLCTrainingGraph mlcTrainingGraph, IsMLCTensor input) => mlcTrainingGraph -> input -> IO (Id MLCTensor)
gradientTensorForInput mlcTrainingGraph input =
  sendMessage mlcTrainingGraph gradientTensorForInputSelector (toMLCTensor input)

-- | Get the source gradient tensors for a layer in the training graph
--
-- @layer@ — A layer in the training graph
--
-- Returns: A list of tensors
--
-- ObjC selector: @- sourceGradientTensorsForLayer:@
sourceGradientTensorsForLayer :: (IsMLCTrainingGraph mlcTrainingGraph, IsMLCLayer layer) => mlcTrainingGraph -> layer -> IO (Id NSArray)
sourceGradientTensorsForLayer mlcTrainingGraph layer =
  sendMessage mlcTrainingGraph sourceGradientTensorsForLayerSelector (toMLCLayer layer)

-- | Get the result gradient tensors for a layer in the training graph
--
-- @layer@ — A layer in the training graph
--
-- Returns: A list of tensors
--
-- ObjC selector: @- resultGradientTensorsForLayer:@
resultGradientTensorsForLayer :: (IsMLCTrainingGraph mlcTrainingGraph, IsMLCLayer layer) => mlcTrainingGraph -> layer -> IO (Id NSArray)
resultGradientTensorsForLayer mlcTrainingGraph layer =
  sendMessage mlcTrainingGraph resultGradientTensorsForLayerSelector (toMLCLayer layer)

-- | Get the gradient data for a trainable parameter associated with a layer
--
-- This can be used to get the gradient data for weights or biases parameters associated with a convolution,                fully connected or convolution transpose layer
--
-- @parameter@ — The updatable parameter associated with the layer
--
-- @layer@ — A layer in the training graph.  Must be one of the following:                      - MLCConvolutionLayer                      - MLCFullyConnectedLayer                      - MLCBatchNormalizationLayer                      - MLCInstanceNormalizationLayer                      - MLCGroupNormalizationLayer                      - MLCLayerNormalizationLayer                      - MLCEmbeddingLayer                      - MLCMultiheadAttentionLayer
--
-- Returns: The gradient data.  Will return nil if the layer is marked as not trainable or if                training graph is not executed with separate calls to forward and gradient passes.
--
-- ObjC selector: @- gradientDataForParameter:layer:@
gradientDataForParameter_layer :: (IsMLCTrainingGraph mlcTrainingGraph, IsMLCTensor parameter, IsMLCLayer layer) => mlcTrainingGraph -> parameter -> layer -> IO (Id NSData)
gradientDataForParameter_layer mlcTrainingGraph parameter layer =
  sendMessage mlcTrainingGraph gradientDataForParameter_layerSelector (toMLCTensor parameter) (toMLCLayer layer)

-- | Allocate an entry for a user specified gradient for a tensor
--
-- @tensor@ — A result tensor produced by a layer in the training graph                       that is input to some user specified code and will need to                       provide a user gradient during the gradient pass.
--
-- Returns: A gradient tensor
--
-- ObjC selector: @- allocateUserGradientForTensor:@
allocateUserGradientForTensor :: (IsMLCTrainingGraph mlcTrainingGraph, IsMLCTensor tensor) => mlcTrainingGraph -> tensor -> IO (Id MLCTensor)
allocateUserGradientForTensor mlcTrainingGraph tensor =
  sendOwnedMessage mlcTrainingGraph allocateUserGradientForTensorSelector (toMLCTensor tensor)

-- | Execute the training graph (forward, gradient and optimizer update) with given source and label data
--
-- Execute the training graph with given source and label data.  If an optimizer is specified, the optimizer update is applied.                If MLCExecutionOptionsSynchronous is specified in 'options', this method returns after the graph has been executed.                Otherwise, this method returns after the graph has been queued for execution. The completion handler is called after the graph                has finished execution.
--
-- @inputsData@ — The data objects to use for inputs
--
-- @lossLabelsData@ — The data objects to use for loss labels
--
-- @lossLabelWeightsData@ — The data objects to use for loss label weights
--
-- @batchSize@ — The batch size to use.  For a graph where batch size changes between layers this value must be 0.
--
-- @options@ — The execution options
--
-- @completionHandler@ — The completion handler
--
-- Returns: A boolean indicating success or failure
--
-- ObjC selector: @- executeWithInputsData:lossLabelsData:lossLabelWeightsData:batchSize:options:completionHandler:@
executeWithInputsData_lossLabelsData_lossLabelWeightsData_batchSize_options_completionHandler :: (IsMLCTrainingGraph mlcTrainingGraph, IsNSDictionary inputsData, IsNSDictionary lossLabelsData, IsNSDictionary lossLabelWeightsData) => mlcTrainingGraph -> inputsData -> lossLabelsData -> lossLabelWeightsData -> CULong -> MLCExecutionOptions -> Ptr () -> IO Bool
executeWithInputsData_lossLabelsData_lossLabelWeightsData_batchSize_options_completionHandler mlcTrainingGraph inputsData lossLabelsData lossLabelWeightsData batchSize options completionHandler =
  sendMessage mlcTrainingGraph executeWithInputsData_lossLabelsData_lossLabelWeightsData_batchSize_options_completionHandlerSelector (toNSDictionary inputsData) (toNSDictionary lossLabelsData) (toNSDictionary lossLabelWeightsData) batchSize options completionHandler

-- | Execute the training graph (forward, gradient and optimizer update) with given source and label data
--
-- @inputsData@ — The data objects to use for inputs
--
-- @lossLabelsData@ — The data objects to use for loss labels
--
-- @lossLabelWeightsData@ — The data objects to use for loss label weights
--
-- @outputsData@ — The data objects to use for outputs
--
-- @batchSize@ — The batch size to use.  For a graph where batch size changes between layers this value must be 0.
--
-- @options@ — The execution options
--
-- @completionHandler@ — The completion handler
--
-- Returns: A boolean indicating success or failure
--
-- ObjC selector: @- executeWithInputsData:lossLabelsData:lossLabelWeightsData:outputsData:batchSize:options:completionHandler:@
executeWithInputsData_lossLabelsData_lossLabelWeightsData_outputsData_batchSize_options_completionHandler :: (IsMLCTrainingGraph mlcTrainingGraph, IsNSDictionary inputsData, IsNSDictionary lossLabelsData, IsNSDictionary lossLabelWeightsData, IsNSDictionary outputsData) => mlcTrainingGraph -> inputsData -> lossLabelsData -> lossLabelWeightsData -> outputsData -> CULong -> MLCExecutionOptions -> Ptr () -> IO Bool
executeWithInputsData_lossLabelsData_lossLabelWeightsData_outputsData_batchSize_options_completionHandler mlcTrainingGraph inputsData lossLabelsData lossLabelWeightsData outputsData batchSize options completionHandler =
  sendMessage mlcTrainingGraph executeWithInputsData_lossLabelsData_lossLabelWeightsData_outputsData_batchSize_options_completionHandlerSelector (toNSDictionary inputsData) (toNSDictionary lossLabelsData) (toNSDictionary lossLabelWeightsData) (toNSDictionary outputsData) batchSize options completionHandler

-- | Execute the forward pass of the training graph
--
-- @batchSize@ — The batch size to use.  For a graph where batch size changes between layers this value must be 0.
--
-- @options@ — The execution options
--
-- @completionHandler@ — The completion handler
--
-- Returns: A boolean indicating success or failure
--
-- ObjC selector: @- executeForwardWithBatchSize:options:completionHandler:@
executeForwardWithBatchSize_options_completionHandler :: IsMLCTrainingGraph mlcTrainingGraph => mlcTrainingGraph -> CULong -> MLCExecutionOptions -> Ptr () -> IO Bool
executeForwardWithBatchSize_options_completionHandler mlcTrainingGraph batchSize options completionHandler =
  sendMessage mlcTrainingGraph executeForwardWithBatchSize_options_completionHandlerSelector batchSize options completionHandler

-- | Execute the forward pass for the training graph
--
-- @batchSize@ — The batch size to use.  For a graph where batch size changes between layers this value must be 0.
--
-- @options@ — The execution options
--
-- @outputsData@ — The data objects to use for outputs
--
-- @completionHandler@ — The completion handler
--
-- Returns: A boolean indicating success or failure
--
-- ObjC selector: @- executeForwardWithBatchSize:options:outputsData:completionHandler:@
executeForwardWithBatchSize_options_outputsData_completionHandler :: (IsMLCTrainingGraph mlcTrainingGraph, IsNSDictionary outputsData) => mlcTrainingGraph -> CULong -> MLCExecutionOptions -> outputsData -> Ptr () -> IO Bool
executeForwardWithBatchSize_options_outputsData_completionHandler mlcTrainingGraph batchSize options outputsData completionHandler =
  sendMessage mlcTrainingGraph executeForwardWithBatchSize_options_outputsData_completionHandlerSelector batchSize options (toNSDictionary outputsData) completionHandler

-- | Execute the gradient pass of the training graph
--
-- @batchSize@ — The batch size to use.  For a graph where batch size changes between layers this value must be 0.
--
-- @options@ — The execution options
--
-- @completionHandler@ — The completion handler
--
-- Returns: A boolean indicating success or failure
--
-- ObjC selector: @- executeGradientWithBatchSize:options:completionHandler:@
executeGradientWithBatchSize_options_completionHandler :: IsMLCTrainingGraph mlcTrainingGraph => mlcTrainingGraph -> CULong -> MLCExecutionOptions -> Ptr () -> IO Bool
executeGradientWithBatchSize_options_completionHandler mlcTrainingGraph batchSize options completionHandler =
  sendMessage mlcTrainingGraph executeGradientWithBatchSize_options_completionHandlerSelector batchSize options completionHandler

-- | Execute the gradient pass of the training graph
--
-- @batchSize@ — The batch size to use.  For a graph where batch size changes between layers this value must be 0.
--
-- @options@ — The execution options
--
-- @outputsData@ — The data objects to use for outputs
--
-- @completionHandler@ — The completion handler
--
-- Returns: A boolean indicating success or failure
--
-- ObjC selector: @- executeGradientWithBatchSize:options:outputsData:completionHandler:@
executeGradientWithBatchSize_options_outputsData_completionHandler :: (IsMLCTrainingGraph mlcTrainingGraph, IsNSDictionary outputsData) => mlcTrainingGraph -> CULong -> MLCExecutionOptions -> outputsData -> Ptr () -> IO Bool
executeGradientWithBatchSize_options_outputsData_completionHandler mlcTrainingGraph batchSize options outputsData completionHandler =
  sendMessage mlcTrainingGraph executeGradientWithBatchSize_options_outputsData_completionHandlerSelector batchSize options (toNSDictionary outputsData) completionHandler

-- | Execute the optimizer update pass of the training graph
--
-- @options@ — The execution options
--
-- @completionHandler@ — The completion handler
--
-- Returns: A boolean indicating success or failure
--
-- ObjC selector: @- executeOptimizerUpdateWithOptions:completionHandler:@
executeOptimizerUpdateWithOptions_completionHandler :: IsMLCTrainingGraph mlcTrainingGraph => mlcTrainingGraph -> MLCExecutionOptions -> Ptr () -> IO Bool
executeOptimizerUpdateWithOptions_completionHandler mlcTrainingGraph options completionHandler =
  sendMessage mlcTrainingGraph executeOptimizerUpdateWithOptions_completionHandlerSelector options completionHandler

-- | Synchronize updates (weights/biases from convolution, fully connected and LSTM layers, tensor parameters)                from device memory to host memory.
--
-- ObjC selector: @- synchronizeUpdates@
synchronizeUpdates :: IsMLCTrainingGraph mlcTrainingGraph => mlcTrainingGraph -> IO ()
synchronizeUpdates mlcTrainingGraph =
  sendMessage mlcTrainingGraph synchronizeUpdatesSelector

-- | Set the input tensor parameters that also will be updated by the optimizer
--
-- These represent the list of input tensors to be updated when we execute the optimizer update                Weights, bias or beta, gamma tensors are not included in this list.  MLCompute automatically                adds them to the parameter list based on whether the layer is marked as updatable or not.
--
-- @parameters@ — The list of input tensors to be updated by the optimizer
--
-- Returns: A boolean indicating success or failure
--
-- ObjC selector: @- setTrainingTensorParameters:@
setTrainingTensorParameters :: (IsMLCTrainingGraph mlcTrainingGraph, IsNSArray parameters) => mlcTrainingGraph -> parameters -> IO Bool
setTrainingTensorParameters mlcTrainingGraph parameters =
  sendMessage mlcTrainingGraph setTrainingTensorParametersSelector (toNSArray parameters)

-- | Associates the given optimizer data and device data buffers with the tensor.                Returns true if the data is successfully associated with the tensor and copied to the device.
--
-- The caller must guarantee the lifetime of the underlying memory of @data@ for the entirety of the tensor's                lifetime.  The @deviceData@ buffers are allocated by MLCompute.  This method must be called                before executeOptimizerUpdateWithOptions or executeWithInputsData is called for the training graph.                We recommend using this method instead of using [MLCTensor bindOptimizerData] especially if the                optimizer update is being called multiple times for each batch.
--
-- @data@ — The optimizer data to be associated with the tensor
--
-- @deviceData@ — The optimizer device data to be associated with the tensor
--
-- @tensor@ — The tensor
--
-- Returns: A Boolean value indicating whether the data is successfully associated with the tensor .
--
-- ObjC selector: @- bindOptimizerData:deviceData:withTensor:@
bindOptimizerData_deviceData_withTensor :: (IsMLCTrainingGraph mlcTrainingGraph, IsNSArray data_, IsNSArray deviceData, IsMLCTensor tensor) => mlcTrainingGraph -> data_ -> deviceData -> tensor -> IO Bool
bindOptimizerData_deviceData_withTensor mlcTrainingGraph data_ deviceData tensor =
  sendMessage mlcTrainingGraph bindOptimizerData_deviceData_withTensorSelector (toNSArray data_) (toNSArray deviceData) (toMLCTensor tensor)

-- | optimizer
--
-- The optimizer to be used with the training graph
--
-- ObjC selector: @- optimizer@
optimizer :: IsMLCTrainingGraph mlcTrainingGraph => mlcTrainingGraph -> IO (Id MLCOptimizer)
optimizer mlcTrainingGraph =
  sendMessage mlcTrainingGraph optimizerSelector

-- | The device memory size used by the training graph
--
-- Returns the total size in bytes of device memory used for all intermediate tensors                for forward, gradient passes and optimizer update for all layers in the training graph.                We recommend executing an iteration before checking the device memory size as                the buffers needed get allocated when the corresponding pass such as gradient,                optimizer update is executed.
--
-- Returns: A NSUInteger value
--
-- ObjC selector: @- deviceMemorySize@
deviceMemorySize :: IsMLCTrainingGraph mlcTrainingGraph => mlcTrainingGraph -> IO CULong
deviceMemorySize mlcTrainingGraph =
  sendMessage mlcTrainingGraph deviceMemorySizeSelector

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @graphWithGraphObjects:lossLayer:optimizer:@
graphWithGraphObjects_lossLayer_optimizerSelector :: Selector '[Id NSArray, Id MLCLayer, Id MLCOptimizer] (Id MLCTrainingGraph)
graphWithGraphObjects_lossLayer_optimizerSelector = mkSelector "graphWithGraphObjects:lossLayer:optimizer:"

-- | @Selector@ for @addInputs:lossLabels:@
addInputs_lossLabelsSelector :: Selector '[Id NSDictionary, Id NSDictionary] Bool
addInputs_lossLabelsSelector = mkSelector "addInputs:lossLabels:"

-- | @Selector@ for @addInputs:lossLabels:lossLabelWeights:@
addInputs_lossLabels_lossLabelWeightsSelector :: Selector '[Id NSDictionary, Id NSDictionary, Id NSDictionary] Bool
addInputs_lossLabels_lossLabelWeightsSelector = mkSelector "addInputs:lossLabels:lossLabelWeights:"

-- | @Selector@ for @addOutputs:@
addOutputsSelector :: Selector '[Id NSDictionary] Bool
addOutputsSelector = mkSelector "addOutputs:"

-- | @Selector@ for @stopGradientForTensors:@
stopGradientForTensorsSelector :: Selector '[Id NSArray] Bool
stopGradientForTensorsSelector = mkSelector "stopGradientForTensors:"

-- | @Selector@ for @compileWithOptions:device:@
compileWithOptions_deviceSelector :: Selector '[MLCGraphCompilationOptions, Id MLCDevice] Bool
compileWithOptions_deviceSelector = mkSelector "compileWithOptions:device:"

-- | @Selector@ for @compileWithOptions:device:inputTensors:inputTensorsData:@
compileWithOptions_device_inputTensors_inputTensorsDataSelector :: Selector '[MLCGraphCompilationOptions, Id MLCDevice, Id NSDictionary, Id NSDictionary] Bool
compileWithOptions_device_inputTensors_inputTensorsDataSelector = mkSelector "compileWithOptions:device:inputTensors:inputTensorsData:"

-- | @Selector@ for @compileOptimizer:@
compileOptimizerSelector :: Selector '[Id MLCOptimizer] Bool
compileOptimizerSelector = mkSelector "compileOptimizer:"

-- | @Selector@ for @linkWithGraphs:@
linkWithGraphsSelector :: Selector '[Id NSArray] Bool
linkWithGraphsSelector = mkSelector "linkWithGraphs:"

-- | @Selector@ for @gradientTensorForInput:@
gradientTensorForInputSelector :: Selector '[Id MLCTensor] (Id MLCTensor)
gradientTensorForInputSelector = mkSelector "gradientTensorForInput:"

-- | @Selector@ for @sourceGradientTensorsForLayer:@
sourceGradientTensorsForLayerSelector :: Selector '[Id MLCLayer] (Id NSArray)
sourceGradientTensorsForLayerSelector = mkSelector "sourceGradientTensorsForLayer:"

-- | @Selector@ for @resultGradientTensorsForLayer:@
resultGradientTensorsForLayerSelector :: Selector '[Id MLCLayer] (Id NSArray)
resultGradientTensorsForLayerSelector = mkSelector "resultGradientTensorsForLayer:"

-- | @Selector@ for @gradientDataForParameter:layer:@
gradientDataForParameter_layerSelector :: Selector '[Id MLCTensor, Id MLCLayer] (Id NSData)
gradientDataForParameter_layerSelector = mkSelector "gradientDataForParameter:layer:"

-- | @Selector@ for @allocateUserGradientForTensor:@
allocateUserGradientForTensorSelector :: Selector '[Id MLCTensor] (Id MLCTensor)
allocateUserGradientForTensorSelector = mkSelector "allocateUserGradientForTensor:"

-- | @Selector@ for @executeWithInputsData:lossLabelsData:lossLabelWeightsData:batchSize:options:completionHandler:@
executeWithInputsData_lossLabelsData_lossLabelWeightsData_batchSize_options_completionHandlerSelector :: Selector '[Id NSDictionary, Id NSDictionary, Id NSDictionary, CULong, MLCExecutionOptions, Ptr ()] Bool
executeWithInputsData_lossLabelsData_lossLabelWeightsData_batchSize_options_completionHandlerSelector = mkSelector "executeWithInputsData:lossLabelsData:lossLabelWeightsData:batchSize:options:completionHandler:"

-- | @Selector@ for @executeWithInputsData:lossLabelsData:lossLabelWeightsData:outputsData:batchSize:options:completionHandler:@
executeWithInputsData_lossLabelsData_lossLabelWeightsData_outputsData_batchSize_options_completionHandlerSelector :: Selector '[Id NSDictionary, Id NSDictionary, Id NSDictionary, Id NSDictionary, CULong, MLCExecutionOptions, Ptr ()] Bool
executeWithInputsData_lossLabelsData_lossLabelWeightsData_outputsData_batchSize_options_completionHandlerSelector = mkSelector "executeWithInputsData:lossLabelsData:lossLabelWeightsData:outputsData:batchSize:options:completionHandler:"

-- | @Selector@ for @executeForwardWithBatchSize:options:completionHandler:@
executeForwardWithBatchSize_options_completionHandlerSelector :: Selector '[CULong, MLCExecutionOptions, Ptr ()] Bool
executeForwardWithBatchSize_options_completionHandlerSelector = mkSelector "executeForwardWithBatchSize:options:completionHandler:"

-- | @Selector@ for @executeForwardWithBatchSize:options:outputsData:completionHandler:@
executeForwardWithBatchSize_options_outputsData_completionHandlerSelector :: Selector '[CULong, MLCExecutionOptions, Id NSDictionary, Ptr ()] Bool
executeForwardWithBatchSize_options_outputsData_completionHandlerSelector = mkSelector "executeForwardWithBatchSize:options:outputsData:completionHandler:"

-- | @Selector@ for @executeGradientWithBatchSize:options:completionHandler:@
executeGradientWithBatchSize_options_completionHandlerSelector :: Selector '[CULong, MLCExecutionOptions, Ptr ()] Bool
executeGradientWithBatchSize_options_completionHandlerSelector = mkSelector "executeGradientWithBatchSize:options:completionHandler:"

-- | @Selector@ for @executeGradientWithBatchSize:options:outputsData:completionHandler:@
executeGradientWithBatchSize_options_outputsData_completionHandlerSelector :: Selector '[CULong, MLCExecutionOptions, Id NSDictionary, Ptr ()] Bool
executeGradientWithBatchSize_options_outputsData_completionHandlerSelector = mkSelector "executeGradientWithBatchSize:options:outputsData:completionHandler:"

-- | @Selector@ for @executeOptimizerUpdateWithOptions:completionHandler:@
executeOptimizerUpdateWithOptions_completionHandlerSelector :: Selector '[MLCExecutionOptions, Ptr ()] Bool
executeOptimizerUpdateWithOptions_completionHandlerSelector = mkSelector "executeOptimizerUpdateWithOptions:completionHandler:"

-- | @Selector@ for @synchronizeUpdates@
synchronizeUpdatesSelector :: Selector '[] ()
synchronizeUpdatesSelector = mkSelector "synchronizeUpdates"

-- | @Selector@ for @setTrainingTensorParameters:@
setTrainingTensorParametersSelector :: Selector '[Id NSArray] Bool
setTrainingTensorParametersSelector = mkSelector "setTrainingTensorParameters:"

-- | @Selector@ for @bindOptimizerData:deviceData:withTensor:@
bindOptimizerData_deviceData_withTensorSelector :: Selector '[Id NSArray, Id NSArray, Id MLCTensor] Bool
bindOptimizerData_deviceData_withTensorSelector = mkSelector "bindOptimizerData:deviceData:withTensor:"

-- | @Selector@ for @optimizer@
optimizerSelector :: Selector '[] (Id MLCOptimizer)
optimizerSelector = mkSelector "optimizer"

-- | @Selector@ for @deviceMemorySize@
deviceMemorySizeSelector :: Selector '[] CULong
deviceMemorySizeSelector = mkSelector "deviceMemorySize"

