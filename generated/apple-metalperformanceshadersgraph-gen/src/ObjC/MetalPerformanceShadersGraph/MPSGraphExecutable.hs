{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | The compiled representation of a compute graph executable.
--
-- An @MPSGraphExecutable@ is a compiled graph for specific feeds for specific target tensors and target operations.
--
-- Generated bindings for @MPSGraphExecutable@.
module ObjC.MetalPerformanceShadersGraph.MPSGraphExecutable
  ( MPSGraphExecutable
  , IsMPSGraphExecutable(..)
  , specializeWithDevice_inputTypes_compilationDescriptor
  , getOutputTypesWithDevice_inputTypes_compilationDescriptor
  , runWithMTLCommandQueue_inputsArray_resultsArray_executionDescriptor
  , runAsyncWithMTLCommandQueue_inputsArray_resultsArray_executionDescriptor
  , encodeToCommandBuffer_inputsArray_resultsArray_executionDescriptor
  , serializeToMPSGraphPackageAtURL_descriptor
  , initWithMPSGraphPackageAtURL_compilationDescriptor
  , initWithCoreMLPackageAtURL_compilationDescriptor
  , options
  , setOptions
  , feedTensors
  , targetTensors
  , encodeToCommandBuffer_inputsArray_resultsArray_executionDescriptorSelector
  , feedTensorsSelector
  , getOutputTypesWithDevice_inputTypes_compilationDescriptorSelector
  , initWithCoreMLPackageAtURL_compilationDescriptorSelector
  , initWithMPSGraphPackageAtURL_compilationDescriptorSelector
  , optionsSelector
  , runAsyncWithMTLCommandQueue_inputsArray_resultsArray_executionDescriptorSelector
  , runWithMTLCommandQueue_inputsArray_resultsArray_executionDescriptorSelector
  , serializeToMPSGraphPackageAtURL_descriptorSelector
  , setOptionsSelector
  , specializeWithDevice_inputTypes_compilationDescriptorSelector
  , targetTensorsSelector

  -- * Enum types
  , MPSGraphOptions(MPSGraphOptions)
  , pattern MPSGraphOptionsNone
  , pattern MPSGraphOptionsSynchronizeResults
  , pattern MPSGraphOptionsVerbose
  , pattern MPSGraphOptionsDefault

  ) where

import Foreign.Ptr (Ptr, FunPtr)
import Foreign.C.Types

import ObjC.Runtime.Types
import ObjC.Runtime.Message (sendMessage, sendOwnedMessage, sendClassMessage, sendOwnedClassMessage)
import ObjC.Runtime.Selector (mkSelector)
import ObjC.Runtime.Class (getRequiredClass)

import ObjC.MetalPerformanceShadersGraph.Internal.Classes
import ObjC.MetalPerformanceShadersGraph.Internal.Enums
import ObjC.Foundation.Internal.Classes
import ObjC.MetalPerformanceShaders.Internal.Classes

-- | Specialize the executable and optimize it.
--
-- Use this method to choose when specialization happens, else it occurs at encode time automatically.
--
-- - Parameters:   - device:Ooptional MPSGraph device to compile with.   - inputTypes: Input types expected to be passed to the executable.   - compilationDescriptor: Compilation descriptor to be used to specialize, since the executable was created with a compilationDescriptor already this one overrides those settings to the extent it can.
--
-- ObjC selector: @- specializeWithDevice:inputTypes:compilationDescriptor:@
specializeWithDevice_inputTypes_compilationDescriptor :: (IsMPSGraphExecutable mpsGraphExecutable, IsMPSGraphDevice device, IsNSArray inputTypes, IsMPSGraphCompilationDescriptor compilationDescriptor) => mpsGraphExecutable -> device -> inputTypes -> compilationDescriptor -> IO ()
specializeWithDevice_inputTypes_compilationDescriptor mpsGraphExecutable device inputTypes compilationDescriptor =
  sendMessage mpsGraphExecutable specializeWithDevice_inputTypes_compilationDescriptorSelector (toMPSGraphDevice device) (toNSArray inputTypes) (toMPSGraphCompilationDescriptor compilationDescriptor)

-- | Get output shapes for a specialized executable.
--
-- In case specialization has not been done yet then calling this function will specialize for the given input shapes.
--
-- - Parameters:   - device: Optional MPSGraph device to compile with   - inputTypes: Input types expected to be passed to the executable.   - compilationDescriptor: CompilationDescriptor to be used to specialize, since the executable was created with a compilationDescriptor already this one overrides those settings to the extent it can.
--
-- ObjC selector: @- getOutputTypesWithDevice:inputTypes:compilationDescriptor:@
getOutputTypesWithDevice_inputTypes_compilationDescriptor :: (IsMPSGraphExecutable mpsGraphExecutable, IsMPSGraphDevice device, IsNSArray inputTypes, IsMPSGraphCompilationDescriptor compilationDescriptor) => mpsGraphExecutable -> device -> inputTypes -> compilationDescriptor -> IO (Id NSArray)
getOutputTypesWithDevice_inputTypes_compilationDescriptor mpsGraphExecutable device inputTypes compilationDescriptor =
  sendMessage mpsGraphExecutable getOutputTypesWithDevice_inputTypes_compilationDescriptorSelector (toMPSGraphDevice device) (toNSArray inputTypes) (toMPSGraphCompilationDescriptor compilationDescriptor)

-- | Runs the graph for the given feeds and returns the target tensor values, ensuring all target operations also executed.
--
-- This call is synchronous and will return on completion of execution.
--
-- - Parameters:   - commandQueue: CommandQueue passed to exectute the graph on.   - inputsArray: Feeds tensorData for the placeholder tensors, same order as arguments of main function.   - resultsArray: Results tensorData for which the caller wishes MPSGraphTensorData to be returned. - Returns: A valid MPSGraphTensorData array with results synchronized to the CPU memory if MPSGraphOptionsSynchronizeResults set.
--
-- ObjC selector: @- runWithMTLCommandQueue:inputsArray:resultsArray:executionDescriptor:@
runWithMTLCommandQueue_inputsArray_resultsArray_executionDescriptor :: (IsMPSGraphExecutable mpsGraphExecutable, IsNSArray inputsArray, IsNSArray resultsArray, IsMPSGraphExecutableExecutionDescriptor executionDescriptor) => mpsGraphExecutable -> RawId -> inputsArray -> resultsArray -> executionDescriptor -> IO (Id NSArray)
runWithMTLCommandQueue_inputsArray_resultsArray_executionDescriptor mpsGraphExecutable commandQueue inputsArray resultsArray executionDescriptor =
  sendMessage mpsGraphExecutable runWithMTLCommandQueue_inputsArray_resultsArray_executionDescriptorSelector commandQueue (toNSArray inputsArray) (toNSArray resultsArray) (toMPSGraphExecutableExecutionDescriptor executionDescriptor)

-- | Runs the graph for the given feeds and returns the target tensor values, ensuring all target operations also executed.  This call is asynchronous and will return immediately.
--
-- - Parameters:   - commandQueue: CommandQueue passed to exectute the graph on.   - inputsArray: Feeds tensorData for the placeholder tensors, same order as arguments of main function.   - resultsArray: Tensors for which the caller wishes MPSGraphTensorData to be returned.   - executionDescriptor: ExecutionDescriptor to be passed in and used. - Returns: A valid MPSGraphTensorData array with results synchronized to the CPU memory if MPSGraphOptionsSynchronizeResults set.
--
-- ObjC selector: @- runAsyncWithMTLCommandQueue:inputsArray:resultsArray:executionDescriptor:@
runAsyncWithMTLCommandQueue_inputsArray_resultsArray_executionDescriptor :: (IsMPSGraphExecutable mpsGraphExecutable, IsNSArray inputsArray, IsNSArray resultsArray, IsMPSGraphExecutableExecutionDescriptor executionDescriptor) => mpsGraphExecutable -> RawId -> inputsArray -> resultsArray -> executionDescriptor -> IO (Id NSArray)
runAsyncWithMTLCommandQueue_inputsArray_resultsArray_executionDescriptor mpsGraphExecutable commandQueue inputsArray resultsArray executionDescriptor =
  sendMessage mpsGraphExecutable runAsyncWithMTLCommandQueue_inputsArray_resultsArray_executionDescriptorSelector commandQueue (toNSArray inputsArray) (toNSArray resultsArray) (toMPSGraphExecutableExecutionDescriptor executionDescriptor)

-- | Runs the graph for the given feeds and returns the target tensor values, ensuring all target operations also executed.  This call is asynchronous and will return immediately after finishing encoding.
--
-- - Parameters:   - commandBuffer: CommandBuffer passed to exectute the graph on, commitAndContinue might be called, please don't rely on underlying MTLCommandBuffer to remain uncommitted   - inputsArray: Feeds tensorData for the placeholder tensors, same order as arguments of main function   - resultsArray: Tensors for which the caller wishes MPSGraphTensorData to be returned   - executionDescriptor: ExecutionDescriptor to be passed in and used, - Returns: A valid MPSGraphTensorData array with results synchronized to the CPU memory if MPSGraphOptionsSynchronizeResults set.
--
-- ObjC selector: @- encodeToCommandBuffer:inputsArray:resultsArray:executionDescriptor:@
encodeToCommandBuffer_inputsArray_resultsArray_executionDescriptor :: (IsMPSGraphExecutable mpsGraphExecutable, IsMPSCommandBuffer commandBuffer, IsNSArray inputsArray, IsNSArray resultsArray, IsMPSGraphExecutableExecutionDescriptor executionDescriptor) => mpsGraphExecutable -> commandBuffer -> inputsArray -> resultsArray -> executionDescriptor -> IO (Id NSArray)
encodeToCommandBuffer_inputsArray_resultsArray_executionDescriptor mpsGraphExecutable commandBuffer inputsArray resultsArray executionDescriptor =
  sendMessage mpsGraphExecutable encodeToCommandBuffer_inputsArray_resultsArray_executionDescriptorSelector (toMPSCommandBuffer commandBuffer) (toNSArray inputsArray) (toNSArray resultsArray) (toMPSGraphExecutableExecutionDescriptor executionDescriptor)

-- | Serialize the MPSGraph executable at the provided url.
--
-- - Parameters:   - url: The URL where to serialize the MPSGraph executable.   - descriptor: The descriptor to be used to serialize the graph.
--
-- ObjC selector: @- serializeToMPSGraphPackageAtURL:descriptor:@
serializeToMPSGraphPackageAtURL_descriptor :: (IsMPSGraphExecutable mpsGraphExecutable, IsNSURL url, IsMPSGraphExecutableSerializationDescriptor descriptor) => mpsGraphExecutable -> url -> descriptor -> IO ()
serializeToMPSGraphPackageAtURL_descriptor mpsGraphExecutable url descriptor =
  sendMessage mpsGraphExecutable serializeToMPSGraphPackageAtURL_descriptorSelector (toNSURL url) (toMPSGraphExecutableSerializationDescriptor descriptor)

-- | Initialize the executable with the Metal Performance Shaders Graph package at the provided URL.
--
-- - Parameters:   - mpsgraphPackageURL: The URL where to read the serialized MPSGraphExecutable.   - compilationDescriptor: Compilation descriptor to be used to specialize, since the executable was created with a compilationDescriptor already this one overrides those settings to the extent it can.
--
-- ObjC selector: @- initWithMPSGraphPackageAtURL:compilationDescriptor:@
initWithMPSGraphPackageAtURL_compilationDescriptor :: (IsMPSGraphExecutable mpsGraphExecutable, IsNSURL mpsgraphPackageURL, IsMPSGraphCompilationDescriptor compilationDescriptor) => mpsGraphExecutable -> mpsgraphPackageURL -> compilationDescriptor -> IO (Id MPSGraphExecutable)
initWithMPSGraphPackageAtURL_compilationDescriptor mpsGraphExecutable mpsgraphPackageURL compilationDescriptor =
  sendOwnedMessage mpsGraphExecutable initWithMPSGraphPackageAtURL_compilationDescriptorSelector (toNSURL mpsgraphPackageURL) (toMPSGraphCompilationDescriptor compilationDescriptor)

-- | Initialize the executable with the Core ML model package at the provided URL.
--
-- - Parameters:   - coreMLPackageURL: The URL where to read the Core ML model package.   - compilationDescriptor: Compilation descriptor to be used to specialize, since the executable was created with a compilationDescriptor already this one overrides those settings to the extent it can.
--
-- ObjC selector: @- initWithCoreMLPackageAtURL:compilationDescriptor:@
initWithCoreMLPackageAtURL_compilationDescriptor :: (IsMPSGraphExecutable mpsGraphExecutable, IsNSURL coreMLPackageURL, IsMPSGraphCompilationDescriptor compilationDescriptor) => mpsGraphExecutable -> coreMLPackageURL -> compilationDescriptor -> IO (Id MPSGraphExecutable)
initWithCoreMLPackageAtURL_compilationDescriptor mpsGraphExecutable coreMLPackageURL compilationDescriptor =
  sendOwnedMessage mpsGraphExecutable initWithCoreMLPackageAtURL_compilationDescriptorSelector (toNSURL coreMLPackageURL) (toMPSGraphCompilationDescriptor compilationDescriptor)

-- | Options for the graph executable.
--
-- Default value is @MPSGraphOptionsDefault@.
--
-- ObjC selector: @- options@
options :: IsMPSGraphExecutable mpsGraphExecutable => mpsGraphExecutable -> IO MPSGraphOptions
options mpsGraphExecutable =
  sendMessage mpsGraphExecutable optionsSelector

-- | Options for the graph executable.
--
-- Default value is @MPSGraphOptionsDefault@.
--
-- ObjC selector: @- setOptions:@
setOptions :: IsMPSGraphExecutable mpsGraphExecutable => mpsGraphExecutable -> MPSGraphOptions -> IO ()
setOptions mpsGraphExecutable value =
  sendMessage mpsGraphExecutable setOptionsSelector value

-- | Tensors fed to the graph, can be used to order the inputs when executable is created with a graph.
--
-- ObjC selector: @- feedTensors@
feedTensors :: IsMPSGraphExecutable mpsGraphExecutable => mpsGraphExecutable -> IO (Id NSArray)
feedTensors mpsGraphExecutable =
  sendMessage mpsGraphExecutable feedTensorsSelector

-- | Tensors targeted by the graph, can be used to order the outputs when executable was created with a graph.
--
-- ObjC selector: @- targetTensors@
targetTensors :: IsMPSGraphExecutable mpsGraphExecutable => mpsGraphExecutable -> IO (Id NSArray)
targetTensors mpsGraphExecutable =
  sendMessage mpsGraphExecutable targetTensorsSelector

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @specializeWithDevice:inputTypes:compilationDescriptor:@
specializeWithDevice_inputTypes_compilationDescriptorSelector :: Selector '[Id MPSGraphDevice, Id NSArray, Id MPSGraphCompilationDescriptor] ()
specializeWithDevice_inputTypes_compilationDescriptorSelector = mkSelector "specializeWithDevice:inputTypes:compilationDescriptor:"

-- | @Selector@ for @getOutputTypesWithDevice:inputTypes:compilationDescriptor:@
getOutputTypesWithDevice_inputTypes_compilationDescriptorSelector :: Selector '[Id MPSGraphDevice, Id NSArray, Id MPSGraphCompilationDescriptor] (Id NSArray)
getOutputTypesWithDevice_inputTypes_compilationDescriptorSelector = mkSelector "getOutputTypesWithDevice:inputTypes:compilationDescriptor:"

-- | @Selector@ for @runWithMTLCommandQueue:inputsArray:resultsArray:executionDescriptor:@
runWithMTLCommandQueue_inputsArray_resultsArray_executionDescriptorSelector :: Selector '[RawId, Id NSArray, Id NSArray, Id MPSGraphExecutableExecutionDescriptor] (Id NSArray)
runWithMTLCommandQueue_inputsArray_resultsArray_executionDescriptorSelector = mkSelector "runWithMTLCommandQueue:inputsArray:resultsArray:executionDescriptor:"

-- | @Selector@ for @runAsyncWithMTLCommandQueue:inputsArray:resultsArray:executionDescriptor:@
runAsyncWithMTLCommandQueue_inputsArray_resultsArray_executionDescriptorSelector :: Selector '[RawId, Id NSArray, Id NSArray, Id MPSGraphExecutableExecutionDescriptor] (Id NSArray)
runAsyncWithMTLCommandQueue_inputsArray_resultsArray_executionDescriptorSelector = mkSelector "runAsyncWithMTLCommandQueue:inputsArray:resultsArray:executionDescriptor:"

-- | @Selector@ for @encodeToCommandBuffer:inputsArray:resultsArray:executionDescriptor:@
encodeToCommandBuffer_inputsArray_resultsArray_executionDescriptorSelector :: Selector '[Id MPSCommandBuffer, Id NSArray, Id NSArray, Id MPSGraphExecutableExecutionDescriptor] (Id NSArray)
encodeToCommandBuffer_inputsArray_resultsArray_executionDescriptorSelector = mkSelector "encodeToCommandBuffer:inputsArray:resultsArray:executionDescriptor:"

-- | @Selector@ for @serializeToMPSGraphPackageAtURL:descriptor:@
serializeToMPSGraphPackageAtURL_descriptorSelector :: Selector '[Id NSURL, Id MPSGraphExecutableSerializationDescriptor] ()
serializeToMPSGraphPackageAtURL_descriptorSelector = mkSelector "serializeToMPSGraphPackageAtURL:descriptor:"

-- | @Selector@ for @initWithMPSGraphPackageAtURL:compilationDescriptor:@
initWithMPSGraphPackageAtURL_compilationDescriptorSelector :: Selector '[Id NSURL, Id MPSGraphCompilationDescriptor] (Id MPSGraphExecutable)
initWithMPSGraphPackageAtURL_compilationDescriptorSelector = mkSelector "initWithMPSGraphPackageAtURL:compilationDescriptor:"

-- | @Selector@ for @initWithCoreMLPackageAtURL:compilationDescriptor:@
initWithCoreMLPackageAtURL_compilationDescriptorSelector :: Selector '[Id NSURL, Id MPSGraphCompilationDescriptor] (Id MPSGraphExecutable)
initWithCoreMLPackageAtURL_compilationDescriptorSelector = mkSelector "initWithCoreMLPackageAtURL:compilationDescriptor:"

-- | @Selector@ for @options@
optionsSelector :: Selector '[] MPSGraphOptions
optionsSelector = mkSelector "options"

-- | @Selector@ for @setOptions:@
setOptionsSelector :: Selector '[MPSGraphOptions] ()
setOptionsSelector = mkSelector "setOptions:"

-- | @Selector@ for @feedTensors@
feedTensorsSelector :: Selector '[] (Id NSArray)
feedTensorsSelector = mkSelector "feedTensors"

-- | @Selector@ for @targetTensors@
targetTensorsSelector :: Selector '[] (Id NSArray)
targetTensorsSelector = mkSelector "targetTensors"

