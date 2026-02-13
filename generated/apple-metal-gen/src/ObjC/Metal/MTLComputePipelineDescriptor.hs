{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Generated bindings for @MTLComputePipelineDescriptor@.
module ObjC.Metal.MTLComputePipelineDescriptor
  ( MTLComputePipelineDescriptor
  , IsMTLComputePipelineDescriptor(..)
  , reset
  , label
  , setLabel
  , computeFunction
  , setComputeFunction
  , threadGroupSizeIsMultipleOfThreadExecutionWidth
  , setThreadGroupSizeIsMultipleOfThreadExecutionWidth
  , maxTotalThreadsPerThreadgroup
  , setMaxTotalThreadsPerThreadgroup
  , stageInputDescriptor
  , setStageInputDescriptor
  , buffers
  , supportIndirectCommandBuffers
  , setSupportIndirectCommandBuffers
  , insertLibraries
  , setInsertLibraries
  , preloadedLibraries
  , setPreloadedLibraries
  , binaryArchives
  , setBinaryArchives
  , linkedFunctions
  , setLinkedFunctions
  , supportAddingBinaryFunctions
  , setSupportAddingBinaryFunctions
  , maxCallStackDepth
  , setMaxCallStackDepth
  , shaderValidation
  , setShaderValidation
  , binaryArchivesSelector
  , buffersSelector
  , computeFunctionSelector
  , insertLibrariesSelector
  , labelSelector
  , linkedFunctionsSelector
  , maxCallStackDepthSelector
  , maxTotalThreadsPerThreadgroupSelector
  , preloadedLibrariesSelector
  , resetSelector
  , setBinaryArchivesSelector
  , setComputeFunctionSelector
  , setInsertLibrariesSelector
  , setLabelSelector
  , setLinkedFunctionsSelector
  , setMaxCallStackDepthSelector
  , setMaxTotalThreadsPerThreadgroupSelector
  , setPreloadedLibrariesSelector
  , setShaderValidationSelector
  , setStageInputDescriptorSelector
  , setSupportAddingBinaryFunctionsSelector
  , setSupportIndirectCommandBuffersSelector
  , setThreadGroupSizeIsMultipleOfThreadExecutionWidthSelector
  , shaderValidationSelector
  , stageInputDescriptorSelector
  , supportAddingBinaryFunctionsSelector
  , supportIndirectCommandBuffersSelector
  , threadGroupSizeIsMultipleOfThreadExecutionWidthSelector

  -- * Enum types
  , MTLShaderValidation(MTLShaderValidation)
  , pattern MTLShaderValidationDefault
  , pattern MTLShaderValidationEnabled
  , pattern MTLShaderValidationDisabled

  ) where

import Foreign.Ptr (Ptr, FunPtr)
import Foreign.C.Types

import ObjC.Runtime.Types
import ObjC.Runtime.Message (sendMessage, sendOwnedMessage, sendClassMessage, sendOwnedClassMessage)
import ObjC.Runtime.Selector (mkSelector)
import ObjC.Runtime.Class (getRequiredClass)

import ObjC.Metal.Internal.Classes
import ObjC.Metal.Internal.Enums
import ObjC.Foundation.Internal.Classes

-- | reset
--
-- Restore all compute pipeline descriptor properties to their default values.
--
-- ObjC selector: @- reset@
reset :: IsMTLComputePipelineDescriptor mtlComputePipelineDescriptor => mtlComputePipelineDescriptor -> IO ()
reset mtlComputePipelineDescriptor =
  sendMessage mtlComputePipelineDescriptor resetSelector

-- | label
--
-- A string to help identify this object.
--
-- ObjC selector: @- label@
label :: IsMTLComputePipelineDescriptor mtlComputePipelineDescriptor => mtlComputePipelineDescriptor -> IO (Id NSString)
label mtlComputePipelineDescriptor =
  sendMessage mtlComputePipelineDescriptor labelSelector

-- | label
--
-- A string to help identify this object.
--
-- ObjC selector: @- setLabel:@
setLabel :: (IsMTLComputePipelineDescriptor mtlComputePipelineDescriptor, IsNSString value) => mtlComputePipelineDescriptor -> value -> IO ()
setLabel mtlComputePipelineDescriptor value =
  sendMessage mtlComputePipelineDescriptor setLabelSelector (toNSString value)

-- | computeFunction
--
-- The function to use with the MTLComputePipelineState
--
-- ObjC selector: @- computeFunction@
computeFunction :: IsMTLComputePipelineDescriptor mtlComputePipelineDescriptor => mtlComputePipelineDescriptor -> IO RawId
computeFunction mtlComputePipelineDescriptor =
  sendMessage mtlComputePipelineDescriptor computeFunctionSelector

-- | computeFunction
--
-- The function to use with the MTLComputePipelineState
--
-- ObjC selector: @- setComputeFunction:@
setComputeFunction :: IsMTLComputePipelineDescriptor mtlComputePipelineDescriptor => mtlComputePipelineDescriptor -> RawId -> IO ()
setComputeFunction mtlComputePipelineDescriptor value =
  sendMessage mtlComputePipelineDescriptor setComputeFunctionSelector value

-- | threadGroupSizeIsMultipleOfThreadExecutionWidth
--
-- An optimization flag, set if the thread group size will always be a multiple of thread execution width
--
-- ObjC selector: @- threadGroupSizeIsMultipleOfThreadExecutionWidth@
threadGroupSizeIsMultipleOfThreadExecutionWidth :: IsMTLComputePipelineDescriptor mtlComputePipelineDescriptor => mtlComputePipelineDescriptor -> IO Bool
threadGroupSizeIsMultipleOfThreadExecutionWidth mtlComputePipelineDescriptor =
  sendMessage mtlComputePipelineDescriptor threadGroupSizeIsMultipleOfThreadExecutionWidthSelector

-- | threadGroupSizeIsMultipleOfThreadExecutionWidth
--
-- An optimization flag, set if the thread group size will always be a multiple of thread execution width
--
-- ObjC selector: @- setThreadGroupSizeIsMultipleOfThreadExecutionWidth:@
setThreadGroupSizeIsMultipleOfThreadExecutionWidth :: IsMTLComputePipelineDescriptor mtlComputePipelineDescriptor => mtlComputePipelineDescriptor -> Bool -> IO ()
setThreadGroupSizeIsMultipleOfThreadExecutionWidth mtlComputePipelineDescriptor value =
  sendMessage mtlComputePipelineDescriptor setThreadGroupSizeIsMultipleOfThreadExecutionWidthSelector value

-- | maxTotalThreadsPerThreadgroup
--
-- Optional property. Set the maxTotalThreadsPerThreadgroup. If it is not set, returns zero.
--
-- ObjC selector: @- maxTotalThreadsPerThreadgroup@
maxTotalThreadsPerThreadgroup :: IsMTLComputePipelineDescriptor mtlComputePipelineDescriptor => mtlComputePipelineDescriptor -> IO CULong
maxTotalThreadsPerThreadgroup mtlComputePipelineDescriptor =
  sendMessage mtlComputePipelineDescriptor maxTotalThreadsPerThreadgroupSelector

-- | maxTotalThreadsPerThreadgroup
--
-- Optional property. Set the maxTotalThreadsPerThreadgroup. If it is not set, returns zero.
--
-- ObjC selector: @- setMaxTotalThreadsPerThreadgroup:@
setMaxTotalThreadsPerThreadgroup :: IsMTLComputePipelineDescriptor mtlComputePipelineDescriptor => mtlComputePipelineDescriptor -> CULong -> IO ()
setMaxTotalThreadsPerThreadgroup mtlComputePipelineDescriptor value =
  sendMessage mtlComputePipelineDescriptor setMaxTotalThreadsPerThreadgroupSelector value

-- | computeDataDescriptor
--
-- An MTLStageInputOutputDescriptor to fetch data from buffers
--
-- ObjC selector: @- stageInputDescriptor@
stageInputDescriptor :: IsMTLComputePipelineDescriptor mtlComputePipelineDescriptor => mtlComputePipelineDescriptor -> IO (Id MTLStageInputOutputDescriptor)
stageInputDescriptor mtlComputePipelineDescriptor =
  sendMessage mtlComputePipelineDescriptor stageInputDescriptorSelector

-- | computeDataDescriptor
--
-- An MTLStageInputOutputDescriptor to fetch data from buffers
--
-- ObjC selector: @- setStageInputDescriptor:@
setStageInputDescriptor :: (IsMTLComputePipelineDescriptor mtlComputePipelineDescriptor, IsMTLStageInputOutputDescriptor value) => mtlComputePipelineDescriptor -> value -> IO ()
setStageInputDescriptor mtlComputePipelineDescriptor value =
  sendMessage mtlComputePipelineDescriptor setStageInputDescriptorSelector (toMTLStageInputOutputDescriptor value)

-- | buffers
--
-- Optional properties for each buffer binding used by the compute function.
--
-- ObjC selector: @- buffers@
buffers :: IsMTLComputePipelineDescriptor mtlComputePipelineDescriptor => mtlComputePipelineDescriptor -> IO (Id MTLPipelineBufferDescriptorArray)
buffers mtlComputePipelineDescriptor =
  sendMessage mtlComputePipelineDescriptor buffersSelector

-- | supportIndirectCommandBuffers
--
-- This flag makes this pipeline usable with indirect command buffers.
--
-- ObjC selector: @- supportIndirectCommandBuffers@
supportIndirectCommandBuffers :: IsMTLComputePipelineDescriptor mtlComputePipelineDescriptor => mtlComputePipelineDescriptor -> IO Bool
supportIndirectCommandBuffers mtlComputePipelineDescriptor =
  sendMessage mtlComputePipelineDescriptor supportIndirectCommandBuffersSelector

-- | supportIndirectCommandBuffers
--
-- This flag makes this pipeline usable with indirect command buffers.
--
-- ObjC selector: @- setSupportIndirectCommandBuffers:@
setSupportIndirectCommandBuffers :: IsMTLComputePipelineDescriptor mtlComputePipelineDescriptor => mtlComputePipelineDescriptor -> Bool -> IO ()
setSupportIndirectCommandBuffers mtlComputePipelineDescriptor value =
  sendMessage mtlComputePipelineDescriptor setSupportIndirectCommandBuffersSelector value

-- | insertLibraries
--
-- The set of MTLDynamicLibrary to use to resolve external symbols before considering symbols from dependent MTLDynamicLibrary.
--
-- Typical workflows use the libraries property of MTLCompileOptions to record dependent libraries at compile time without having to use insertLibraries. This property can be used to override symbols from dependent libraries for experimentation or evaluating alternative implementations. It can also be used to provide dynamic libraries that are dynamically created (for example, from source) that have no stable installName that can be used to automatically load from the file system.
--
-- See: MTLDynamicLibrary
--
-- ObjC selector: @- insertLibraries@
insertLibraries :: IsMTLComputePipelineDescriptor mtlComputePipelineDescriptor => mtlComputePipelineDescriptor -> IO (Id NSArray)
insertLibraries mtlComputePipelineDescriptor =
  sendMessage mtlComputePipelineDescriptor insertLibrariesSelector

-- | insertLibraries
--
-- The set of MTLDynamicLibrary to use to resolve external symbols before considering symbols from dependent MTLDynamicLibrary.
--
-- Typical workflows use the libraries property of MTLCompileOptions to record dependent libraries at compile time without having to use insertLibraries. This property can be used to override symbols from dependent libraries for experimentation or evaluating alternative implementations. It can also be used to provide dynamic libraries that are dynamically created (for example, from source) that have no stable installName that can be used to automatically load from the file system.
--
-- See: MTLDynamicLibrary
--
-- ObjC selector: @- setInsertLibraries:@
setInsertLibraries :: (IsMTLComputePipelineDescriptor mtlComputePipelineDescriptor, IsNSArray value) => mtlComputePipelineDescriptor -> value -> IO ()
setInsertLibraries mtlComputePipelineDescriptor value =
  sendMessage mtlComputePipelineDescriptor setInsertLibrariesSelector (toNSArray value)

-- | preloadedLibraries
--
-- The set of MTLDynamicLibrary to use to resolve external symbols before considering symbols from dependent MTLDynamicLibrary.
--
-- Typical workflows use the libraries property of MTLCompileOptions to record dependent libraries at compile time without having to use preloadedLibraries. This property can be used to override symbols from dependent libraries for experimentation or evaluating alternative implementations. It can also be used to provide dynamic libraries that are dynamically created (for example, from source) that have no stable installName that can be used to automatically load from the file system.
--
-- See: MTLDynamicLibrary
--
-- ObjC selector: @- preloadedLibraries@
preloadedLibraries :: IsMTLComputePipelineDescriptor mtlComputePipelineDescriptor => mtlComputePipelineDescriptor -> IO (Id NSArray)
preloadedLibraries mtlComputePipelineDescriptor =
  sendMessage mtlComputePipelineDescriptor preloadedLibrariesSelector

-- | preloadedLibraries
--
-- The set of MTLDynamicLibrary to use to resolve external symbols before considering symbols from dependent MTLDynamicLibrary.
--
-- Typical workflows use the libraries property of MTLCompileOptions to record dependent libraries at compile time without having to use preloadedLibraries. This property can be used to override symbols from dependent libraries for experimentation or evaluating alternative implementations. It can also be used to provide dynamic libraries that are dynamically created (for example, from source) that have no stable installName that can be used to automatically load from the file system.
--
-- See: MTLDynamicLibrary
--
-- ObjC selector: @- setPreloadedLibraries:@
setPreloadedLibraries :: (IsMTLComputePipelineDescriptor mtlComputePipelineDescriptor, IsNSArray value) => mtlComputePipelineDescriptor -> value -> IO ()
setPreloadedLibraries mtlComputePipelineDescriptor value =
  sendMessage mtlComputePipelineDescriptor setPreloadedLibrariesSelector (toNSArray value)

-- | binaryArchives
--
-- The set of MTLBinaryArchive to search for compiled code when creating the pipeline state.
--
-- Accelerate pipeline state creation by providing archives of compiled code such that no compilation needs to happen on the fast path.
--
-- See: MTLBinaryArchive
--
-- ObjC selector: @- binaryArchives@
binaryArchives :: IsMTLComputePipelineDescriptor mtlComputePipelineDescriptor => mtlComputePipelineDescriptor -> IO (Id NSArray)
binaryArchives mtlComputePipelineDescriptor =
  sendMessage mtlComputePipelineDescriptor binaryArchivesSelector

-- | binaryArchives
--
-- The set of MTLBinaryArchive to search for compiled code when creating the pipeline state.
--
-- Accelerate pipeline state creation by providing archives of compiled code such that no compilation needs to happen on the fast path.
--
-- See: MTLBinaryArchive
--
-- ObjC selector: @- setBinaryArchives:@
setBinaryArchives :: (IsMTLComputePipelineDescriptor mtlComputePipelineDescriptor, IsNSArray value) => mtlComputePipelineDescriptor -> value -> IO ()
setBinaryArchives mtlComputePipelineDescriptor value =
  sendMessage mtlComputePipelineDescriptor setBinaryArchivesSelector (toNSArray value)

-- | linkedFunctions
--
-- The set of functions to be linked with the pipeline state and accessed from the compute function.
--
-- See: MTLLinkedFunctions
--
-- ObjC selector: @- linkedFunctions@
linkedFunctions :: IsMTLComputePipelineDescriptor mtlComputePipelineDescriptor => mtlComputePipelineDescriptor -> IO (Id MTLLinkedFunctions)
linkedFunctions mtlComputePipelineDescriptor =
  sendMessage mtlComputePipelineDescriptor linkedFunctionsSelector

-- | linkedFunctions
--
-- The set of functions to be linked with the pipeline state and accessed from the compute function.
--
-- See: MTLLinkedFunctions
--
-- ObjC selector: @- setLinkedFunctions:@
setLinkedFunctions :: (IsMTLComputePipelineDescriptor mtlComputePipelineDescriptor, IsMTLLinkedFunctions value) => mtlComputePipelineDescriptor -> value -> IO ()
setLinkedFunctions mtlComputePipelineDescriptor value =
  sendMessage mtlComputePipelineDescriptor setLinkedFunctionsSelector (toMTLLinkedFunctions value)

-- | supportAddingBinaryFunctions
--
-- This flag makes this pipeline support creating a new pipeline by adding binary functions.
--
-- ObjC selector: @- supportAddingBinaryFunctions@
supportAddingBinaryFunctions :: IsMTLComputePipelineDescriptor mtlComputePipelineDescriptor => mtlComputePipelineDescriptor -> IO Bool
supportAddingBinaryFunctions mtlComputePipelineDescriptor =
  sendMessage mtlComputePipelineDescriptor supportAddingBinaryFunctionsSelector

-- | supportAddingBinaryFunctions
--
-- This flag makes this pipeline support creating a new pipeline by adding binary functions.
--
-- ObjC selector: @- setSupportAddingBinaryFunctions:@
setSupportAddingBinaryFunctions :: IsMTLComputePipelineDescriptor mtlComputePipelineDescriptor => mtlComputePipelineDescriptor -> Bool -> IO ()
setSupportAddingBinaryFunctions mtlComputePipelineDescriptor value =
  sendMessage mtlComputePipelineDescriptor setSupportAddingBinaryFunctionsSelector value

-- | maxCallStackDepth
--
-- The maximum depth of the call stack in stack frames from the kernel. Defaults to 1 additional stack frame.
--
-- ObjC selector: @- maxCallStackDepth@
maxCallStackDepth :: IsMTLComputePipelineDescriptor mtlComputePipelineDescriptor => mtlComputePipelineDescriptor -> IO CULong
maxCallStackDepth mtlComputePipelineDescriptor =
  sendMessage mtlComputePipelineDescriptor maxCallStackDepthSelector

-- | maxCallStackDepth
--
-- The maximum depth of the call stack in stack frames from the kernel. Defaults to 1 additional stack frame.
--
-- ObjC selector: @- setMaxCallStackDepth:@
setMaxCallStackDepth :: IsMTLComputePipelineDescriptor mtlComputePipelineDescriptor => mtlComputePipelineDescriptor -> CULong -> IO ()
setMaxCallStackDepth mtlComputePipelineDescriptor value =
  sendMessage mtlComputePipelineDescriptor setMaxCallStackDepthSelector value

-- | shaderValidation
--
-- Toggle that determines whether Metal Shader Validation should be enabled or disabled for the pipeline.
--
-- The value can be overridden using @MTL_SHADER_VALIDATION_ENABLE_PIPELINES@ or @MTL_SHADER_VALIDATION_DISABLE_PIPELINES@ Environment Variables.
--
-- ObjC selector: @- shaderValidation@
shaderValidation :: IsMTLComputePipelineDescriptor mtlComputePipelineDescriptor => mtlComputePipelineDescriptor -> IO MTLShaderValidation
shaderValidation mtlComputePipelineDescriptor =
  sendMessage mtlComputePipelineDescriptor shaderValidationSelector

-- | shaderValidation
--
-- Toggle that determines whether Metal Shader Validation should be enabled or disabled for the pipeline.
--
-- The value can be overridden using @MTL_SHADER_VALIDATION_ENABLE_PIPELINES@ or @MTL_SHADER_VALIDATION_DISABLE_PIPELINES@ Environment Variables.
--
-- ObjC selector: @- setShaderValidation:@
setShaderValidation :: IsMTLComputePipelineDescriptor mtlComputePipelineDescriptor => mtlComputePipelineDescriptor -> MTLShaderValidation -> IO ()
setShaderValidation mtlComputePipelineDescriptor value =
  sendMessage mtlComputePipelineDescriptor setShaderValidationSelector value

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @reset@
resetSelector :: Selector '[] ()
resetSelector = mkSelector "reset"

-- | @Selector@ for @label@
labelSelector :: Selector '[] (Id NSString)
labelSelector = mkSelector "label"

-- | @Selector@ for @setLabel:@
setLabelSelector :: Selector '[Id NSString] ()
setLabelSelector = mkSelector "setLabel:"

-- | @Selector@ for @computeFunction@
computeFunctionSelector :: Selector '[] RawId
computeFunctionSelector = mkSelector "computeFunction"

-- | @Selector@ for @setComputeFunction:@
setComputeFunctionSelector :: Selector '[RawId] ()
setComputeFunctionSelector = mkSelector "setComputeFunction:"

-- | @Selector@ for @threadGroupSizeIsMultipleOfThreadExecutionWidth@
threadGroupSizeIsMultipleOfThreadExecutionWidthSelector :: Selector '[] Bool
threadGroupSizeIsMultipleOfThreadExecutionWidthSelector = mkSelector "threadGroupSizeIsMultipleOfThreadExecutionWidth"

-- | @Selector@ for @setThreadGroupSizeIsMultipleOfThreadExecutionWidth:@
setThreadGroupSizeIsMultipleOfThreadExecutionWidthSelector :: Selector '[Bool] ()
setThreadGroupSizeIsMultipleOfThreadExecutionWidthSelector = mkSelector "setThreadGroupSizeIsMultipleOfThreadExecutionWidth:"

-- | @Selector@ for @maxTotalThreadsPerThreadgroup@
maxTotalThreadsPerThreadgroupSelector :: Selector '[] CULong
maxTotalThreadsPerThreadgroupSelector = mkSelector "maxTotalThreadsPerThreadgroup"

-- | @Selector@ for @setMaxTotalThreadsPerThreadgroup:@
setMaxTotalThreadsPerThreadgroupSelector :: Selector '[CULong] ()
setMaxTotalThreadsPerThreadgroupSelector = mkSelector "setMaxTotalThreadsPerThreadgroup:"

-- | @Selector@ for @stageInputDescriptor@
stageInputDescriptorSelector :: Selector '[] (Id MTLStageInputOutputDescriptor)
stageInputDescriptorSelector = mkSelector "stageInputDescriptor"

-- | @Selector@ for @setStageInputDescriptor:@
setStageInputDescriptorSelector :: Selector '[Id MTLStageInputOutputDescriptor] ()
setStageInputDescriptorSelector = mkSelector "setStageInputDescriptor:"

-- | @Selector@ for @buffers@
buffersSelector :: Selector '[] (Id MTLPipelineBufferDescriptorArray)
buffersSelector = mkSelector "buffers"

-- | @Selector@ for @supportIndirectCommandBuffers@
supportIndirectCommandBuffersSelector :: Selector '[] Bool
supportIndirectCommandBuffersSelector = mkSelector "supportIndirectCommandBuffers"

-- | @Selector@ for @setSupportIndirectCommandBuffers:@
setSupportIndirectCommandBuffersSelector :: Selector '[Bool] ()
setSupportIndirectCommandBuffersSelector = mkSelector "setSupportIndirectCommandBuffers:"

-- | @Selector@ for @insertLibraries@
insertLibrariesSelector :: Selector '[] (Id NSArray)
insertLibrariesSelector = mkSelector "insertLibraries"

-- | @Selector@ for @setInsertLibraries:@
setInsertLibrariesSelector :: Selector '[Id NSArray] ()
setInsertLibrariesSelector = mkSelector "setInsertLibraries:"

-- | @Selector@ for @preloadedLibraries@
preloadedLibrariesSelector :: Selector '[] (Id NSArray)
preloadedLibrariesSelector = mkSelector "preloadedLibraries"

-- | @Selector@ for @setPreloadedLibraries:@
setPreloadedLibrariesSelector :: Selector '[Id NSArray] ()
setPreloadedLibrariesSelector = mkSelector "setPreloadedLibraries:"

-- | @Selector@ for @binaryArchives@
binaryArchivesSelector :: Selector '[] (Id NSArray)
binaryArchivesSelector = mkSelector "binaryArchives"

-- | @Selector@ for @setBinaryArchives:@
setBinaryArchivesSelector :: Selector '[Id NSArray] ()
setBinaryArchivesSelector = mkSelector "setBinaryArchives:"

-- | @Selector@ for @linkedFunctions@
linkedFunctionsSelector :: Selector '[] (Id MTLLinkedFunctions)
linkedFunctionsSelector = mkSelector "linkedFunctions"

-- | @Selector@ for @setLinkedFunctions:@
setLinkedFunctionsSelector :: Selector '[Id MTLLinkedFunctions] ()
setLinkedFunctionsSelector = mkSelector "setLinkedFunctions:"

-- | @Selector@ for @supportAddingBinaryFunctions@
supportAddingBinaryFunctionsSelector :: Selector '[] Bool
supportAddingBinaryFunctionsSelector = mkSelector "supportAddingBinaryFunctions"

-- | @Selector@ for @setSupportAddingBinaryFunctions:@
setSupportAddingBinaryFunctionsSelector :: Selector '[Bool] ()
setSupportAddingBinaryFunctionsSelector = mkSelector "setSupportAddingBinaryFunctions:"

-- | @Selector@ for @maxCallStackDepth@
maxCallStackDepthSelector :: Selector '[] CULong
maxCallStackDepthSelector = mkSelector "maxCallStackDepth"

-- | @Selector@ for @setMaxCallStackDepth:@
setMaxCallStackDepthSelector :: Selector '[CULong] ()
setMaxCallStackDepthSelector = mkSelector "setMaxCallStackDepth:"

-- | @Selector@ for @shaderValidation@
shaderValidationSelector :: Selector '[] MTLShaderValidation
shaderValidationSelector = mkSelector "shaderValidation"

-- | @Selector@ for @setShaderValidation:@
setShaderValidationSelector :: Selector '[MTLShaderValidation] ()
setShaderValidationSelector = mkSelector "setShaderValidation:"

