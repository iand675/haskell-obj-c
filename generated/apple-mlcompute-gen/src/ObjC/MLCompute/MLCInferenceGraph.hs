{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | MLCInferenceGraph
--
-- An inference graph created from one or more MLCGraph objects                plus additional layers added directly to the inference graph.
--
-- Generated bindings for @MLCInferenceGraph@.
module ObjC.MLCompute.MLCInferenceGraph
  ( MLCInferenceGraph
  , IsMLCInferenceGraph(..)
  , new
  , init_
  , graphWithGraphObjects
  , addInputs
  , addInputs_lossLabels_lossLabelWeights
  , addOutputs
  , compileWithOptions_device
  , compileWithOptions_device_inputTensors_inputTensorsData
  , linkWithGraphs
  , executeWithInputsData_batchSize_options_completionHandler
  , executeWithInputsData_outputsData_batchSize_options_completionHandler
  , executeWithInputsData_lossLabelsData_lossLabelWeightsData_batchSize_options_completionHandler
  , executeWithInputsData_lossLabelsData_lossLabelWeightsData_outputsData_batchSize_options_completionHandler
  , deviceMemorySize
  , addInputsSelector
  , addInputs_lossLabels_lossLabelWeightsSelector
  , addOutputsSelector
  , compileWithOptions_deviceSelector
  , compileWithOptions_device_inputTensors_inputTensorsDataSelector
  , deviceMemorySizeSelector
  , executeWithInputsData_batchSize_options_completionHandlerSelector
  , executeWithInputsData_lossLabelsData_lossLabelWeightsData_batchSize_options_completionHandlerSelector
  , executeWithInputsData_lossLabelsData_lossLabelWeightsData_outputsData_batchSize_options_completionHandlerSelector
  , executeWithInputsData_outputsData_batchSize_options_completionHandlerSelector
  , graphWithGraphObjectsSelector
  , initSelector
  , linkWithGraphsSelector
  , newSelector

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

-- | @+ new@
new :: IO (Id MLCInferenceGraph)
new  =
  do
    cls' <- getRequiredClass "MLCInferenceGraph"
    sendOwnedClassMessage cls' newSelector

-- | @- init@
init_ :: IsMLCInferenceGraph mlcInferenceGraph => mlcInferenceGraph -> IO (Id MLCInferenceGraph)
init_ mlcInferenceGraph =
  sendOwnedMessage mlcInferenceGraph initSelector

-- | Create an inference graph
--
-- @graphObjects@ — The layers from these graph objects will be added to the training graph
--
-- Returns: A new inference graph object
--
-- ObjC selector: @+ graphWithGraphObjects:@
graphWithGraphObjects :: IsNSArray graphObjects => graphObjects -> IO (Id MLCInferenceGraph)
graphWithGraphObjects graphObjects =
  do
    cls' <- getRequiredClass "MLCInferenceGraph"
    sendClassMessage cls' graphWithGraphObjectsSelector (toNSArray graphObjects)

-- | Add the list of inputs to the inference graph
--
-- @inputs@ — The inputs
--
-- Returns: A boolean indicating success or failure
--
-- ObjC selector: @- addInputs:@
addInputs :: (IsMLCInferenceGraph mlcInferenceGraph, IsNSDictionary inputs) => mlcInferenceGraph -> inputs -> IO Bool
addInputs mlcInferenceGraph inputs =
  sendMessage mlcInferenceGraph addInputsSelector (toNSDictionary inputs)

-- | Add the list of inputs to the inference graph
--
-- Each input, loss label or label weights tensor is identified by a NSString.                When the inference graph is executed, this NSString is used to identify which data object                should be as input data for each tensor whose device memory needs to be updated                before the graph is executed.
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
addInputs_lossLabels_lossLabelWeights :: (IsMLCInferenceGraph mlcInferenceGraph, IsNSDictionary inputs, IsNSDictionary lossLabels, IsNSDictionary lossLabelWeights) => mlcInferenceGraph -> inputs -> lossLabels -> lossLabelWeights -> IO Bool
addInputs_lossLabels_lossLabelWeights mlcInferenceGraph inputs lossLabels lossLabelWeights =
  sendMessage mlcInferenceGraph addInputs_lossLabels_lossLabelWeightsSelector (toNSDictionary inputs) (toNSDictionary lossLabels) (toNSDictionary lossLabelWeights)

-- | Add the list of outputs to the inference graph
--
-- @outputs@ — The outputs
--
-- Returns: A boolean indicating success or failure
--
-- ObjC selector: @- addOutputs:@
addOutputs :: (IsMLCInferenceGraph mlcInferenceGraph, IsNSDictionary outputs) => mlcInferenceGraph -> outputs -> IO Bool
addOutputs mlcInferenceGraph outputs =
  sendMessage mlcInferenceGraph addOutputsSelector (toNSDictionary outputs)

-- | Compile the training graph for a device.
--
-- @options@ — The compiler options to use when compiling the training graph
--
-- @device@ — The MLCDevice object
--
-- Returns: A boolean indicating success or failure
--
-- ObjC selector: @- compileWithOptions:device:@
compileWithOptions_device :: (IsMLCInferenceGraph mlcInferenceGraph, IsMLCDevice device) => mlcInferenceGraph -> MLCGraphCompilationOptions -> device -> IO Bool
compileWithOptions_device mlcInferenceGraph options device =
  sendMessage mlcInferenceGraph compileWithOptions_deviceSelector options (toMLCDevice device)

-- | Compile the inference graph for a device.
--
-- Specifying the list of constant tensors when we compile the graph allows MLCompute to perform additional optimizations at compile time.
--
-- @options@ — The compiler options to use when compiling the inference graph
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
compileWithOptions_device_inputTensors_inputTensorsData :: (IsMLCInferenceGraph mlcInferenceGraph, IsMLCDevice device, IsNSDictionary inputTensors, IsNSDictionary inputTensorsData) => mlcInferenceGraph -> MLCGraphCompilationOptions -> device -> inputTensors -> inputTensorsData -> IO Bool
compileWithOptions_device_inputTensors_inputTensorsData mlcInferenceGraph options device inputTensors inputTensorsData =
  sendMessage mlcInferenceGraph compileWithOptions_device_inputTensors_inputTensorsDataSelector options (toMLCDevice device) (toNSDictionary inputTensors) (toNSDictionary inputTensorsData)

-- | Link mutiple inference graphs
--
-- This is used to link subsequent inference graphs with first inference sub-graph.                This method should be used when we have tensors shared by one or more layers in multiple sub-graphs
--
-- @graphs@ — The list of inference graphs to link
--
-- Returns: A boolean indicating success or failure
--
-- ObjC selector: @- linkWithGraphs:@
linkWithGraphs :: (IsMLCInferenceGraph mlcInferenceGraph, IsNSArray graphs) => mlcInferenceGraph -> graphs -> IO Bool
linkWithGraphs mlcInferenceGraph graphs =
  sendMessage mlcInferenceGraph linkWithGraphsSelector (toNSArray graphs)

-- | Execute the inference graph with given input data
--
-- Execute the inference graph given input data.                If MLCExecutionOptionsSynchronous is specified in 'options', this method returns after the graph has been executed.                Otherwise, this method returns after the graph has been queued for execution.  The completion handler  is called after the graph has finished execution.
--
-- @inputsData@ — The data objects to use for inputs
--
-- @batchSize@ — The batch size to use.  For a graph where batch size changes between layers this value must be 0.
--
-- @options@ — The execution options
--
-- @completionHandler@ — The completion handler
--
-- Returns: A boolean indicating success or failure
--
-- ObjC selector: @- executeWithInputsData:batchSize:options:completionHandler:@
executeWithInputsData_batchSize_options_completionHandler :: (IsMLCInferenceGraph mlcInferenceGraph, IsNSDictionary inputsData) => mlcInferenceGraph -> inputsData -> CULong -> MLCExecutionOptions -> Ptr () -> IO Bool
executeWithInputsData_batchSize_options_completionHandler mlcInferenceGraph inputsData batchSize options completionHandler =
  sendMessage mlcInferenceGraph executeWithInputsData_batchSize_options_completionHandlerSelector (toNSDictionary inputsData) batchSize options completionHandler

-- | Execute the inference graph with given input data
--
-- Execute the inference graph given input data.                If MLCExecutionOptionsSynchronous is specified in 'options', this method returns after the graph has been executed.                Otherwise, this method returns after the graph has been queued for execution.  The completion handler  is called after the graph has finished execution.
--
-- @inputsData@ — The data objects to use for inputs
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
-- ObjC selector: @- executeWithInputsData:outputsData:batchSize:options:completionHandler:@
executeWithInputsData_outputsData_batchSize_options_completionHandler :: (IsMLCInferenceGraph mlcInferenceGraph, IsNSDictionary inputsData, IsNSDictionary outputsData) => mlcInferenceGraph -> inputsData -> outputsData -> CULong -> MLCExecutionOptions -> Ptr () -> IO Bool
executeWithInputsData_outputsData_batchSize_options_completionHandler mlcInferenceGraph inputsData outputsData batchSize options completionHandler =
  sendMessage mlcInferenceGraph executeWithInputsData_outputsData_batchSize_options_completionHandlerSelector (toNSDictionary inputsData) (toNSDictionary outputsData) batchSize options completionHandler

-- | Execute the inference graph with given input data
--
-- Execute the inference graph given input data.                If MLCExecutionOptionsSynchronous is specified in 'options', this method returns after the graph has been executed.                Otherwise, this method returns after the graph has been queued for execution.  The completion handler  is called after the graph has finished execution.
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
executeWithInputsData_lossLabelsData_lossLabelWeightsData_batchSize_options_completionHandler :: (IsMLCInferenceGraph mlcInferenceGraph, IsNSDictionary inputsData, IsNSDictionary lossLabelsData, IsNSDictionary lossLabelWeightsData) => mlcInferenceGraph -> inputsData -> lossLabelsData -> lossLabelWeightsData -> CULong -> MLCExecutionOptions -> Ptr () -> IO Bool
executeWithInputsData_lossLabelsData_lossLabelWeightsData_batchSize_options_completionHandler mlcInferenceGraph inputsData lossLabelsData lossLabelWeightsData batchSize options completionHandler =
  sendMessage mlcInferenceGraph executeWithInputsData_lossLabelsData_lossLabelWeightsData_batchSize_options_completionHandlerSelector (toNSDictionary inputsData) (toNSDictionary lossLabelsData) (toNSDictionary lossLabelWeightsData) batchSize options completionHandler

-- | Execute the inference graph with given input data
--
-- Execute the inference graph given input data.                If MLCExecutionOptionsSynchronous is specified in 'options', this method returns after the graph has been executed.                Otherwise, this method returns after the graph has been queued for execution.  The completion handler  is called after the graph has finished execution.
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
executeWithInputsData_lossLabelsData_lossLabelWeightsData_outputsData_batchSize_options_completionHandler :: (IsMLCInferenceGraph mlcInferenceGraph, IsNSDictionary inputsData, IsNSDictionary lossLabelsData, IsNSDictionary lossLabelWeightsData, IsNSDictionary outputsData) => mlcInferenceGraph -> inputsData -> lossLabelsData -> lossLabelWeightsData -> outputsData -> CULong -> MLCExecutionOptions -> Ptr () -> IO Bool
executeWithInputsData_lossLabelsData_lossLabelWeightsData_outputsData_batchSize_options_completionHandler mlcInferenceGraph inputsData lossLabelsData lossLabelWeightsData outputsData batchSize options completionHandler =
  sendMessage mlcInferenceGraph executeWithInputsData_lossLabelsData_lossLabelWeightsData_outputsData_batchSize_options_completionHandlerSelector (toNSDictionary inputsData) (toNSDictionary lossLabelsData) (toNSDictionary lossLabelWeightsData) (toNSDictionary outputsData) batchSize options completionHandler

-- | The device memory size used by the inference graph
--
-- Returns the total size in bytes of device memory used by all intermediate tensors in the inference graph
--
-- Returns: A NSUInteger value
--
-- ObjC selector: @- deviceMemorySize@
deviceMemorySize :: IsMLCInferenceGraph mlcInferenceGraph => mlcInferenceGraph -> IO CULong
deviceMemorySize mlcInferenceGraph =
  sendMessage mlcInferenceGraph deviceMemorySizeSelector

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @new@
newSelector :: Selector '[] (Id MLCInferenceGraph)
newSelector = mkSelector "new"

-- | @Selector@ for @init@
initSelector :: Selector '[] (Id MLCInferenceGraph)
initSelector = mkSelector "init"

-- | @Selector@ for @graphWithGraphObjects:@
graphWithGraphObjectsSelector :: Selector '[Id NSArray] (Id MLCInferenceGraph)
graphWithGraphObjectsSelector = mkSelector "graphWithGraphObjects:"

-- | @Selector@ for @addInputs:@
addInputsSelector :: Selector '[Id NSDictionary] Bool
addInputsSelector = mkSelector "addInputs:"

-- | @Selector@ for @addInputs:lossLabels:lossLabelWeights:@
addInputs_lossLabels_lossLabelWeightsSelector :: Selector '[Id NSDictionary, Id NSDictionary, Id NSDictionary] Bool
addInputs_lossLabels_lossLabelWeightsSelector = mkSelector "addInputs:lossLabels:lossLabelWeights:"

-- | @Selector@ for @addOutputs:@
addOutputsSelector :: Selector '[Id NSDictionary] Bool
addOutputsSelector = mkSelector "addOutputs:"

-- | @Selector@ for @compileWithOptions:device:@
compileWithOptions_deviceSelector :: Selector '[MLCGraphCompilationOptions, Id MLCDevice] Bool
compileWithOptions_deviceSelector = mkSelector "compileWithOptions:device:"

-- | @Selector@ for @compileWithOptions:device:inputTensors:inputTensorsData:@
compileWithOptions_device_inputTensors_inputTensorsDataSelector :: Selector '[MLCGraphCompilationOptions, Id MLCDevice, Id NSDictionary, Id NSDictionary] Bool
compileWithOptions_device_inputTensors_inputTensorsDataSelector = mkSelector "compileWithOptions:device:inputTensors:inputTensorsData:"

-- | @Selector@ for @linkWithGraphs:@
linkWithGraphsSelector :: Selector '[Id NSArray] Bool
linkWithGraphsSelector = mkSelector "linkWithGraphs:"

-- | @Selector@ for @executeWithInputsData:batchSize:options:completionHandler:@
executeWithInputsData_batchSize_options_completionHandlerSelector :: Selector '[Id NSDictionary, CULong, MLCExecutionOptions, Ptr ()] Bool
executeWithInputsData_batchSize_options_completionHandlerSelector = mkSelector "executeWithInputsData:batchSize:options:completionHandler:"

-- | @Selector@ for @executeWithInputsData:outputsData:batchSize:options:completionHandler:@
executeWithInputsData_outputsData_batchSize_options_completionHandlerSelector :: Selector '[Id NSDictionary, Id NSDictionary, CULong, MLCExecutionOptions, Ptr ()] Bool
executeWithInputsData_outputsData_batchSize_options_completionHandlerSelector = mkSelector "executeWithInputsData:outputsData:batchSize:options:completionHandler:"

-- | @Selector@ for @executeWithInputsData:lossLabelsData:lossLabelWeightsData:batchSize:options:completionHandler:@
executeWithInputsData_lossLabelsData_lossLabelWeightsData_batchSize_options_completionHandlerSelector :: Selector '[Id NSDictionary, Id NSDictionary, Id NSDictionary, CULong, MLCExecutionOptions, Ptr ()] Bool
executeWithInputsData_lossLabelsData_lossLabelWeightsData_batchSize_options_completionHandlerSelector = mkSelector "executeWithInputsData:lossLabelsData:lossLabelWeightsData:batchSize:options:completionHandler:"

-- | @Selector@ for @executeWithInputsData:lossLabelsData:lossLabelWeightsData:outputsData:batchSize:options:completionHandler:@
executeWithInputsData_lossLabelsData_lossLabelWeightsData_outputsData_batchSize_options_completionHandlerSelector :: Selector '[Id NSDictionary, Id NSDictionary, Id NSDictionary, Id NSDictionary, CULong, MLCExecutionOptions, Ptr ()] Bool
executeWithInputsData_lossLabelsData_lossLabelWeightsData_outputsData_batchSize_options_completionHandlerSelector = mkSelector "executeWithInputsData:lossLabelsData:lossLabelWeightsData:outputsData:batchSize:options:completionHandler:"

-- | @Selector@ for @deviceMemorySize@
deviceMemorySizeSelector :: Selector '[] CULong
deviceMemorySizeSelector = mkSelector "deviceMemorySize"

