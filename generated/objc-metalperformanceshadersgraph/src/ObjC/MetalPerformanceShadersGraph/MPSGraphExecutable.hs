{-# LANGUAGE PatternSynonyms #-}
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
  , specializeWithDevice_inputTypes_compilationDescriptorSelector
  , getOutputTypesWithDevice_inputTypes_compilationDescriptorSelector
  , runWithMTLCommandQueue_inputsArray_resultsArray_executionDescriptorSelector
  , runAsyncWithMTLCommandQueue_inputsArray_resultsArray_executionDescriptorSelector
  , encodeToCommandBuffer_inputsArray_resultsArray_executionDescriptorSelector
  , serializeToMPSGraphPackageAtURL_descriptorSelector
  , initWithMPSGraphPackageAtURL_compilationDescriptorSelector
  , initWithCoreMLPackageAtURL_compilationDescriptorSelector
  , optionsSelector
  , setOptionsSelector
  , feedTensorsSelector
  , targetTensorsSelector

  -- * Enum types
  , MPSGraphOptions(MPSGraphOptions)
  , pattern MPSGraphOptionsNone
  , pattern MPSGraphOptionsSynchronizeResults
  , pattern MPSGraphOptionsVerbose
  , pattern MPSGraphOptionsDefault

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
specializeWithDevice_inputTypes_compilationDescriptor mpsGraphExecutable  device inputTypes compilationDescriptor =
withObjCPtr device $ \raw_device ->
  withObjCPtr inputTypes $ \raw_inputTypes ->
    withObjCPtr compilationDescriptor $ \raw_compilationDescriptor ->
        sendMsg mpsGraphExecutable (mkSelector "specializeWithDevice:inputTypes:compilationDescriptor:") retVoid [argPtr (castPtr raw_device :: Ptr ()), argPtr (castPtr raw_inputTypes :: Ptr ()), argPtr (castPtr raw_compilationDescriptor :: Ptr ())]

-- | Get output shapes for a specialized executable.
--
-- In case specialization has not been done yet then calling this function will specialize for the given input shapes.
--
-- - Parameters:   - device: Optional MPSGraph device to compile with   - inputTypes: Input types expected to be passed to the executable.   - compilationDescriptor: CompilationDescriptor to be used to specialize, since the executable was created with a compilationDescriptor already this one overrides those settings to the extent it can.
--
-- ObjC selector: @- getOutputTypesWithDevice:inputTypes:compilationDescriptor:@
getOutputTypesWithDevice_inputTypes_compilationDescriptor :: (IsMPSGraphExecutable mpsGraphExecutable, IsMPSGraphDevice device, IsNSArray inputTypes, IsMPSGraphCompilationDescriptor compilationDescriptor) => mpsGraphExecutable -> device -> inputTypes -> compilationDescriptor -> IO (Id NSArray)
getOutputTypesWithDevice_inputTypes_compilationDescriptor mpsGraphExecutable  device inputTypes compilationDescriptor =
withObjCPtr device $ \raw_device ->
  withObjCPtr inputTypes $ \raw_inputTypes ->
    withObjCPtr compilationDescriptor $ \raw_compilationDescriptor ->
        sendMsg mpsGraphExecutable (mkSelector "getOutputTypesWithDevice:inputTypes:compilationDescriptor:") (retPtr retVoid) [argPtr (castPtr raw_device :: Ptr ()), argPtr (castPtr raw_inputTypes :: Ptr ()), argPtr (castPtr raw_compilationDescriptor :: Ptr ())] >>= retainedObject . castPtr

-- | Runs the graph for the given feeds and returns the target tensor values, ensuring all target operations also executed.
--
-- This call is synchronous and will return on completion of execution.
--
-- - Parameters:   - commandQueue: CommandQueue passed to exectute the graph on.   - inputsArray: Feeds tensorData for the placeholder tensors, same order as arguments of main function.   - resultsArray: Results tensorData for which the caller wishes MPSGraphTensorData to be returned. - Returns: A valid MPSGraphTensorData array with results synchronized to the CPU memory if MPSGraphOptionsSynchronizeResults set.
--
-- ObjC selector: @- runWithMTLCommandQueue:inputsArray:resultsArray:executionDescriptor:@
runWithMTLCommandQueue_inputsArray_resultsArray_executionDescriptor :: (IsMPSGraphExecutable mpsGraphExecutable, IsNSArray inputsArray, IsNSArray resultsArray, IsMPSGraphExecutableExecutionDescriptor executionDescriptor) => mpsGraphExecutable -> RawId -> inputsArray -> resultsArray -> executionDescriptor -> IO (Id NSArray)
runWithMTLCommandQueue_inputsArray_resultsArray_executionDescriptor mpsGraphExecutable  commandQueue inputsArray resultsArray executionDescriptor =
withObjCPtr inputsArray $ \raw_inputsArray ->
  withObjCPtr resultsArray $ \raw_resultsArray ->
    withObjCPtr executionDescriptor $ \raw_executionDescriptor ->
        sendMsg mpsGraphExecutable (mkSelector "runWithMTLCommandQueue:inputsArray:resultsArray:executionDescriptor:") (retPtr retVoid) [argPtr (castPtr (unRawId commandQueue) :: Ptr ()), argPtr (castPtr raw_inputsArray :: Ptr ()), argPtr (castPtr raw_resultsArray :: Ptr ()), argPtr (castPtr raw_executionDescriptor :: Ptr ())] >>= retainedObject . castPtr

-- | Runs the graph for the given feeds and returns the target tensor values, ensuring all target operations also executed.  This call is asynchronous and will return immediately.
--
-- - Parameters:   - commandQueue: CommandQueue passed to exectute the graph on.   - inputsArray: Feeds tensorData for the placeholder tensors, same order as arguments of main function.   - resultsArray: Tensors for which the caller wishes MPSGraphTensorData to be returned.   - executionDescriptor: ExecutionDescriptor to be passed in and used. - Returns: A valid MPSGraphTensorData array with results synchronized to the CPU memory if MPSGraphOptionsSynchronizeResults set.
--
-- ObjC selector: @- runAsyncWithMTLCommandQueue:inputsArray:resultsArray:executionDescriptor:@
runAsyncWithMTLCommandQueue_inputsArray_resultsArray_executionDescriptor :: (IsMPSGraphExecutable mpsGraphExecutable, IsNSArray inputsArray, IsNSArray resultsArray, IsMPSGraphExecutableExecutionDescriptor executionDescriptor) => mpsGraphExecutable -> RawId -> inputsArray -> resultsArray -> executionDescriptor -> IO (Id NSArray)
runAsyncWithMTLCommandQueue_inputsArray_resultsArray_executionDescriptor mpsGraphExecutable  commandQueue inputsArray resultsArray executionDescriptor =
withObjCPtr inputsArray $ \raw_inputsArray ->
  withObjCPtr resultsArray $ \raw_resultsArray ->
    withObjCPtr executionDescriptor $ \raw_executionDescriptor ->
        sendMsg mpsGraphExecutable (mkSelector "runAsyncWithMTLCommandQueue:inputsArray:resultsArray:executionDescriptor:") (retPtr retVoid) [argPtr (castPtr (unRawId commandQueue) :: Ptr ()), argPtr (castPtr raw_inputsArray :: Ptr ()), argPtr (castPtr raw_resultsArray :: Ptr ()), argPtr (castPtr raw_executionDescriptor :: Ptr ())] >>= retainedObject . castPtr

-- | Runs the graph for the given feeds and returns the target tensor values, ensuring all target operations also executed.  This call is asynchronous and will return immediately after finishing encoding.
--
-- - Parameters:   - commandBuffer: CommandBuffer passed to exectute the graph on, commitAndContinue might be called, please don't rely on underlying MTLCommandBuffer to remain uncommitted   - inputsArray: Feeds tensorData for the placeholder tensors, same order as arguments of main function   - resultsArray: Tensors for which the caller wishes MPSGraphTensorData to be returned   - executionDescriptor: ExecutionDescriptor to be passed in and used, - Returns: A valid MPSGraphTensorData array with results synchronized to the CPU memory if MPSGraphOptionsSynchronizeResults set.
--
-- ObjC selector: @- encodeToCommandBuffer:inputsArray:resultsArray:executionDescriptor:@
encodeToCommandBuffer_inputsArray_resultsArray_executionDescriptor :: (IsMPSGraphExecutable mpsGraphExecutable, IsMPSCommandBuffer commandBuffer, IsNSArray inputsArray, IsNSArray resultsArray, IsMPSGraphExecutableExecutionDescriptor executionDescriptor) => mpsGraphExecutable -> commandBuffer -> inputsArray -> resultsArray -> executionDescriptor -> IO (Id NSArray)
encodeToCommandBuffer_inputsArray_resultsArray_executionDescriptor mpsGraphExecutable  commandBuffer inputsArray resultsArray executionDescriptor =
withObjCPtr commandBuffer $ \raw_commandBuffer ->
  withObjCPtr inputsArray $ \raw_inputsArray ->
    withObjCPtr resultsArray $ \raw_resultsArray ->
      withObjCPtr executionDescriptor $ \raw_executionDescriptor ->
          sendMsg mpsGraphExecutable (mkSelector "encodeToCommandBuffer:inputsArray:resultsArray:executionDescriptor:") (retPtr retVoid) [argPtr (castPtr raw_commandBuffer :: Ptr ()), argPtr (castPtr raw_inputsArray :: Ptr ()), argPtr (castPtr raw_resultsArray :: Ptr ()), argPtr (castPtr raw_executionDescriptor :: Ptr ())] >>= retainedObject . castPtr

-- | Serialize the MPSGraph executable at the provided url.
--
-- - Parameters:   - url: The URL where to serialize the MPSGraph executable.   - descriptor: The descriptor to be used to serialize the graph.
--
-- ObjC selector: @- serializeToMPSGraphPackageAtURL:descriptor:@
serializeToMPSGraphPackageAtURL_descriptor :: (IsMPSGraphExecutable mpsGraphExecutable, IsNSURL url, IsMPSGraphExecutableSerializationDescriptor descriptor) => mpsGraphExecutable -> url -> descriptor -> IO ()
serializeToMPSGraphPackageAtURL_descriptor mpsGraphExecutable  url descriptor =
withObjCPtr url $ \raw_url ->
  withObjCPtr descriptor $ \raw_descriptor ->
      sendMsg mpsGraphExecutable (mkSelector "serializeToMPSGraphPackageAtURL:descriptor:") retVoid [argPtr (castPtr raw_url :: Ptr ()), argPtr (castPtr raw_descriptor :: Ptr ())]

-- | Initialize the executable with the Metal Performance Shaders Graph package at the provided URL.
--
-- - Parameters:   - mpsgraphPackageURL: The URL where to read the serialized MPSGraphExecutable.   - compilationDescriptor: Compilation descriptor to be used to specialize, since the executable was created with a compilationDescriptor already this one overrides those settings to the extent it can.
--
-- ObjC selector: @- initWithMPSGraphPackageAtURL:compilationDescriptor:@
initWithMPSGraphPackageAtURL_compilationDescriptor :: (IsMPSGraphExecutable mpsGraphExecutable, IsNSURL mpsgraphPackageURL, IsMPSGraphCompilationDescriptor compilationDescriptor) => mpsGraphExecutable -> mpsgraphPackageURL -> compilationDescriptor -> IO (Id MPSGraphExecutable)
initWithMPSGraphPackageAtURL_compilationDescriptor mpsGraphExecutable  mpsgraphPackageURL compilationDescriptor =
withObjCPtr mpsgraphPackageURL $ \raw_mpsgraphPackageURL ->
  withObjCPtr compilationDescriptor $ \raw_compilationDescriptor ->
      sendMsg mpsGraphExecutable (mkSelector "initWithMPSGraphPackageAtURL:compilationDescriptor:") (retPtr retVoid) [argPtr (castPtr raw_mpsgraphPackageURL :: Ptr ()), argPtr (castPtr raw_compilationDescriptor :: Ptr ())] >>= ownedObject . castPtr

-- | Initialize the executable with the Core ML model package at the provided URL.
--
-- - Parameters:   - coreMLPackageURL: The URL where to read the Core ML model package.   - compilationDescriptor: Compilation descriptor to be used to specialize, since the executable was created with a compilationDescriptor already this one overrides those settings to the extent it can.
--
-- ObjC selector: @- initWithCoreMLPackageAtURL:compilationDescriptor:@
initWithCoreMLPackageAtURL_compilationDescriptor :: (IsMPSGraphExecutable mpsGraphExecutable, IsNSURL coreMLPackageURL, IsMPSGraphCompilationDescriptor compilationDescriptor) => mpsGraphExecutable -> coreMLPackageURL -> compilationDescriptor -> IO (Id MPSGraphExecutable)
initWithCoreMLPackageAtURL_compilationDescriptor mpsGraphExecutable  coreMLPackageURL compilationDescriptor =
withObjCPtr coreMLPackageURL $ \raw_coreMLPackageURL ->
  withObjCPtr compilationDescriptor $ \raw_compilationDescriptor ->
      sendMsg mpsGraphExecutable (mkSelector "initWithCoreMLPackageAtURL:compilationDescriptor:") (retPtr retVoid) [argPtr (castPtr raw_coreMLPackageURL :: Ptr ()), argPtr (castPtr raw_compilationDescriptor :: Ptr ())] >>= ownedObject . castPtr

-- | Options for the graph executable.
--
-- Default value is @MPSGraphOptionsDefault@.
--
-- ObjC selector: @- options@
options :: IsMPSGraphExecutable mpsGraphExecutable => mpsGraphExecutable -> IO MPSGraphOptions
options mpsGraphExecutable  =
  fmap (coerce :: CULong -> MPSGraphOptions) $ sendMsg mpsGraphExecutable (mkSelector "options") retCULong []

-- | Options for the graph executable.
--
-- Default value is @MPSGraphOptionsDefault@.
--
-- ObjC selector: @- setOptions:@
setOptions :: IsMPSGraphExecutable mpsGraphExecutable => mpsGraphExecutable -> MPSGraphOptions -> IO ()
setOptions mpsGraphExecutable  value =
  sendMsg mpsGraphExecutable (mkSelector "setOptions:") retVoid [argCULong (coerce value)]

-- | Tensors fed to the graph, can be used to order the inputs when executable is created with a graph.
--
-- ObjC selector: @- feedTensors@
feedTensors :: IsMPSGraphExecutable mpsGraphExecutable => mpsGraphExecutable -> IO (Id NSArray)
feedTensors mpsGraphExecutable  =
  sendMsg mpsGraphExecutable (mkSelector "feedTensors") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | Tensors targeted by the graph, can be used to order the outputs when executable was created with a graph.
--
-- ObjC selector: @- targetTensors@
targetTensors :: IsMPSGraphExecutable mpsGraphExecutable => mpsGraphExecutable -> IO (Id NSArray)
targetTensors mpsGraphExecutable  =
  sendMsg mpsGraphExecutable (mkSelector "targetTensors") (retPtr retVoid) [] >>= retainedObject . castPtr

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @specializeWithDevice:inputTypes:compilationDescriptor:@
specializeWithDevice_inputTypes_compilationDescriptorSelector :: Selector
specializeWithDevice_inputTypes_compilationDescriptorSelector = mkSelector "specializeWithDevice:inputTypes:compilationDescriptor:"

-- | @Selector@ for @getOutputTypesWithDevice:inputTypes:compilationDescriptor:@
getOutputTypesWithDevice_inputTypes_compilationDescriptorSelector :: Selector
getOutputTypesWithDevice_inputTypes_compilationDescriptorSelector = mkSelector "getOutputTypesWithDevice:inputTypes:compilationDescriptor:"

-- | @Selector@ for @runWithMTLCommandQueue:inputsArray:resultsArray:executionDescriptor:@
runWithMTLCommandQueue_inputsArray_resultsArray_executionDescriptorSelector :: Selector
runWithMTLCommandQueue_inputsArray_resultsArray_executionDescriptorSelector = mkSelector "runWithMTLCommandQueue:inputsArray:resultsArray:executionDescriptor:"

-- | @Selector@ for @runAsyncWithMTLCommandQueue:inputsArray:resultsArray:executionDescriptor:@
runAsyncWithMTLCommandQueue_inputsArray_resultsArray_executionDescriptorSelector :: Selector
runAsyncWithMTLCommandQueue_inputsArray_resultsArray_executionDescriptorSelector = mkSelector "runAsyncWithMTLCommandQueue:inputsArray:resultsArray:executionDescriptor:"

-- | @Selector@ for @encodeToCommandBuffer:inputsArray:resultsArray:executionDescriptor:@
encodeToCommandBuffer_inputsArray_resultsArray_executionDescriptorSelector :: Selector
encodeToCommandBuffer_inputsArray_resultsArray_executionDescriptorSelector = mkSelector "encodeToCommandBuffer:inputsArray:resultsArray:executionDescriptor:"

-- | @Selector@ for @serializeToMPSGraphPackageAtURL:descriptor:@
serializeToMPSGraphPackageAtURL_descriptorSelector :: Selector
serializeToMPSGraphPackageAtURL_descriptorSelector = mkSelector "serializeToMPSGraphPackageAtURL:descriptor:"

-- | @Selector@ for @initWithMPSGraphPackageAtURL:compilationDescriptor:@
initWithMPSGraphPackageAtURL_compilationDescriptorSelector :: Selector
initWithMPSGraphPackageAtURL_compilationDescriptorSelector = mkSelector "initWithMPSGraphPackageAtURL:compilationDescriptor:"

-- | @Selector@ for @initWithCoreMLPackageAtURL:compilationDescriptor:@
initWithCoreMLPackageAtURL_compilationDescriptorSelector :: Selector
initWithCoreMLPackageAtURL_compilationDescriptorSelector = mkSelector "initWithCoreMLPackageAtURL:compilationDescriptor:"

-- | @Selector@ for @options@
optionsSelector :: Selector
optionsSelector = mkSelector "options"

-- | @Selector@ for @setOptions:@
setOptionsSelector :: Selector
setOptionsSelector = mkSelector "setOptions:"

-- | @Selector@ for @feedTensors@
feedTensorsSelector :: Selector
feedTensorsSelector = mkSelector "feedTensors"

-- | @Selector@ for @targetTensors@
targetTensorsSelector :: Selector
targetTensorsSelector = mkSelector "targetTensors"

