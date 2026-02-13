{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Generated bindings for @MTLTileRenderPipelineDescriptor@.
module ObjC.Metal.MTLTileRenderPipelineDescriptor
  ( MTLTileRenderPipelineDescriptor
  , IsMTLTileRenderPipelineDescriptor(..)
  , reset
  , label
  , setLabel
  , tileFunction
  , setTileFunction
  , rasterSampleCount
  , setRasterSampleCount
  , colorAttachments
  , threadgroupSizeMatchesTileSize
  , setThreadgroupSizeMatchesTileSize
  , tileBuffers
  , maxTotalThreadsPerThreadgroup
  , setMaxTotalThreadsPerThreadgroup
  , binaryArchives
  , setBinaryArchives
  , preloadedLibraries
  , setPreloadedLibraries
  , linkedFunctions
  , setLinkedFunctions
  , supportAddingBinaryFunctions
  , setSupportAddingBinaryFunctions
  , maxCallStackDepth
  , setMaxCallStackDepth
  , shaderValidation
  , setShaderValidation
  , binaryArchivesSelector
  , colorAttachmentsSelector
  , labelSelector
  , linkedFunctionsSelector
  , maxCallStackDepthSelector
  , maxTotalThreadsPerThreadgroupSelector
  , preloadedLibrariesSelector
  , rasterSampleCountSelector
  , resetSelector
  , setBinaryArchivesSelector
  , setLabelSelector
  , setLinkedFunctionsSelector
  , setMaxCallStackDepthSelector
  , setMaxTotalThreadsPerThreadgroupSelector
  , setPreloadedLibrariesSelector
  , setRasterSampleCountSelector
  , setShaderValidationSelector
  , setSupportAddingBinaryFunctionsSelector
  , setThreadgroupSizeMatchesTileSizeSelector
  , setTileFunctionSelector
  , shaderValidationSelector
  , supportAddingBinaryFunctionsSelector
  , threadgroupSizeMatchesTileSizeSelector
  , tileBuffersSelector
  , tileFunctionSelector

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

-- | @- reset@
reset :: IsMTLTileRenderPipelineDescriptor mtlTileRenderPipelineDescriptor => mtlTileRenderPipelineDescriptor -> IO ()
reset mtlTileRenderPipelineDescriptor =
  sendMessage mtlTileRenderPipelineDescriptor resetSelector

-- | label:
--
-- The descriptor label.
--
-- ObjC selector: @- label@
label :: IsMTLTileRenderPipelineDescriptor mtlTileRenderPipelineDescriptor => mtlTileRenderPipelineDescriptor -> IO (Id NSString)
label mtlTileRenderPipelineDescriptor =
  sendMessage mtlTileRenderPipelineDescriptor labelSelector

-- | label:
--
-- The descriptor label.
--
-- ObjC selector: @- setLabel:@
setLabel :: (IsMTLTileRenderPipelineDescriptor mtlTileRenderPipelineDescriptor, IsNSString value) => mtlTileRenderPipelineDescriptor -> value -> IO ()
setLabel mtlTileRenderPipelineDescriptor value =
  sendMessage mtlTileRenderPipelineDescriptor setLabelSelector (toNSString value)

-- | tileFunction:
--
-- The kernel or fragment function that serves as the tile shader for this pipeline.
--
-- Both kernel-based and fragment-based tile pipelines dispatches will barrier against previous draws and other dispatches. Kernel-based pipelines will wait until all prior access to the tile completes. Fragment-based pipelines will only wait until all prior access to the fragment's location completes.
--
-- ObjC selector: @- tileFunction@
tileFunction :: IsMTLTileRenderPipelineDescriptor mtlTileRenderPipelineDescriptor => mtlTileRenderPipelineDescriptor -> IO RawId
tileFunction mtlTileRenderPipelineDescriptor =
  sendMessage mtlTileRenderPipelineDescriptor tileFunctionSelector

-- | tileFunction:
--
-- The kernel or fragment function that serves as the tile shader for this pipeline.
--
-- Both kernel-based and fragment-based tile pipelines dispatches will barrier against previous draws and other dispatches. Kernel-based pipelines will wait until all prior access to the tile completes. Fragment-based pipelines will only wait until all prior access to the fragment's location completes.
--
-- ObjC selector: @- setTileFunction:@
setTileFunction :: IsMTLTileRenderPipelineDescriptor mtlTileRenderPipelineDescriptor => mtlTileRenderPipelineDescriptor -> RawId -> IO ()
setTileFunction mtlTileRenderPipelineDescriptor value =
  sendMessage mtlTileRenderPipelineDescriptor setTileFunctionSelector value

-- | @- rasterSampleCount@
rasterSampleCount :: IsMTLTileRenderPipelineDescriptor mtlTileRenderPipelineDescriptor => mtlTileRenderPipelineDescriptor -> IO CULong
rasterSampleCount mtlTileRenderPipelineDescriptor =
  sendMessage mtlTileRenderPipelineDescriptor rasterSampleCountSelector

-- | @- setRasterSampleCount:@
setRasterSampleCount :: IsMTLTileRenderPipelineDescriptor mtlTileRenderPipelineDescriptor => mtlTileRenderPipelineDescriptor -> CULong -> IO ()
setRasterSampleCount mtlTileRenderPipelineDescriptor value =
  sendMessage mtlTileRenderPipelineDescriptor setRasterSampleCountSelector value

-- | @- colorAttachments@
colorAttachments :: IsMTLTileRenderPipelineDescriptor mtlTileRenderPipelineDescriptor => mtlTileRenderPipelineDescriptor -> IO (Id MTLTileRenderPipelineColorAttachmentDescriptorArray)
colorAttachments mtlTileRenderPipelineDescriptor =
  sendMessage mtlTileRenderPipelineDescriptor colorAttachmentsSelector

-- | threadgroupSizeMatchesTileSize:
--
-- Whether all threadgroups associated with this pipeline will cover tiles entirely.
--
-- Metal can optimize code generation for this case.
--
-- ObjC selector: @- threadgroupSizeMatchesTileSize@
threadgroupSizeMatchesTileSize :: IsMTLTileRenderPipelineDescriptor mtlTileRenderPipelineDescriptor => mtlTileRenderPipelineDescriptor -> IO Bool
threadgroupSizeMatchesTileSize mtlTileRenderPipelineDescriptor =
  sendMessage mtlTileRenderPipelineDescriptor threadgroupSizeMatchesTileSizeSelector

-- | threadgroupSizeMatchesTileSize:
--
-- Whether all threadgroups associated with this pipeline will cover tiles entirely.
--
-- Metal can optimize code generation for this case.
--
-- ObjC selector: @- setThreadgroupSizeMatchesTileSize:@
setThreadgroupSizeMatchesTileSize :: IsMTLTileRenderPipelineDescriptor mtlTileRenderPipelineDescriptor => mtlTileRenderPipelineDescriptor -> Bool -> IO ()
setThreadgroupSizeMatchesTileSize mtlTileRenderPipelineDescriptor value =
  sendMessage mtlTileRenderPipelineDescriptor setThreadgroupSizeMatchesTileSizeSelector value

-- | @- tileBuffers@
tileBuffers :: IsMTLTileRenderPipelineDescriptor mtlTileRenderPipelineDescriptor => mtlTileRenderPipelineDescriptor -> IO (Id MTLPipelineBufferDescriptorArray)
tileBuffers mtlTileRenderPipelineDescriptor =
  sendMessage mtlTileRenderPipelineDescriptor tileBuffersSelector

-- | maxTotalThreadsPerThreadgroup
--
-- Optional property. Set the maxTotalThreadsPerThreadgroup. If it is not set, returns zero.
--
-- ObjC selector: @- maxTotalThreadsPerThreadgroup@
maxTotalThreadsPerThreadgroup :: IsMTLTileRenderPipelineDescriptor mtlTileRenderPipelineDescriptor => mtlTileRenderPipelineDescriptor -> IO CULong
maxTotalThreadsPerThreadgroup mtlTileRenderPipelineDescriptor =
  sendMessage mtlTileRenderPipelineDescriptor maxTotalThreadsPerThreadgroupSelector

-- | maxTotalThreadsPerThreadgroup
--
-- Optional property. Set the maxTotalThreadsPerThreadgroup. If it is not set, returns zero.
--
-- ObjC selector: @- setMaxTotalThreadsPerThreadgroup:@
setMaxTotalThreadsPerThreadgroup :: IsMTLTileRenderPipelineDescriptor mtlTileRenderPipelineDescriptor => mtlTileRenderPipelineDescriptor -> CULong -> IO ()
setMaxTotalThreadsPerThreadgroup mtlTileRenderPipelineDescriptor value =
  sendMessage mtlTileRenderPipelineDescriptor setMaxTotalThreadsPerThreadgroupSelector value

-- | binaryArchives
--
-- The set of MTLBinaryArchive to search for compiled code when creating the pipeline state.
--
-- Accelerate pipeline state creation by providing archives of compiled code such that no compilation needs to happen on the fast path.
--
-- See: MTLBinaryArchive
--
-- ObjC selector: @- binaryArchives@
binaryArchives :: IsMTLTileRenderPipelineDescriptor mtlTileRenderPipelineDescriptor => mtlTileRenderPipelineDescriptor -> IO (Id NSArray)
binaryArchives mtlTileRenderPipelineDescriptor =
  sendMessage mtlTileRenderPipelineDescriptor binaryArchivesSelector

-- | binaryArchives
--
-- The set of MTLBinaryArchive to search for compiled code when creating the pipeline state.
--
-- Accelerate pipeline state creation by providing archives of compiled code such that no compilation needs to happen on the fast path.
--
-- See: MTLBinaryArchive
--
-- ObjC selector: @- setBinaryArchives:@
setBinaryArchives :: (IsMTLTileRenderPipelineDescriptor mtlTileRenderPipelineDescriptor, IsNSArray value) => mtlTileRenderPipelineDescriptor -> value -> IO ()
setBinaryArchives mtlTileRenderPipelineDescriptor value =
  sendMessage mtlTileRenderPipelineDescriptor setBinaryArchivesSelector (toNSArray value)

-- | preloadedLibraries
--
-- The set of MTLDynamicLibrary to use to resolve external symbols before considering symbols from dependent MTLDynamicLibrary.
--
-- Typical workflows use the libraries property of MTLCompileOptions to record dependent libraries at compile time without having to use preloadedLibraries. This property can be used to override symbols from dependent libraries for experimentation or evaluating alternative implementations. It can also be used to provide dynamic libraries that are dynamically created (for example, from source) that have no stable installName that can be used to automatically load from the file system.
--
-- See: MTLDynamicLibrary
--
-- ObjC selector: @- preloadedLibraries@
preloadedLibraries :: IsMTLTileRenderPipelineDescriptor mtlTileRenderPipelineDescriptor => mtlTileRenderPipelineDescriptor -> IO (Id NSArray)
preloadedLibraries mtlTileRenderPipelineDescriptor =
  sendMessage mtlTileRenderPipelineDescriptor preloadedLibrariesSelector

-- | preloadedLibraries
--
-- The set of MTLDynamicLibrary to use to resolve external symbols before considering symbols from dependent MTLDynamicLibrary.
--
-- Typical workflows use the libraries property of MTLCompileOptions to record dependent libraries at compile time without having to use preloadedLibraries. This property can be used to override symbols from dependent libraries for experimentation or evaluating alternative implementations. It can also be used to provide dynamic libraries that are dynamically created (for example, from source) that have no stable installName that can be used to automatically load from the file system.
--
-- See: MTLDynamicLibrary
--
-- ObjC selector: @- setPreloadedLibraries:@
setPreloadedLibraries :: (IsMTLTileRenderPipelineDescriptor mtlTileRenderPipelineDescriptor, IsNSArray value) => mtlTileRenderPipelineDescriptor -> value -> IO ()
setPreloadedLibraries mtlTileRenderPipelineDescriptor value =
  sendMessage mtlTileRenderPipelineDescriptor setPreloadedLibrariesSelector (toNSArray value)

-- | linkedFunctions
--
-- The set of functions to be linked with the pipeline state and accessed from the tile function.
--
-- See: MTLLinkedFunctions
--
-- ObjC selector: @- linkedFunctions@
linkedFunctions :: IsMTLTileRenderPipelineDescriptor mtlTileRenderPipelineDescriptor => mtlTileRenderPipelineDescriptor -> IO (Id MTLLinkedFunctions)
linkedFunctions mtlTileRenderPipelineDescriptor =
  sendMessage mtlTileRenderPipelineDescriptor linkedFunctionsSelector

-- | linkedFunctions
--
-- The set of functions to be linked with the pipeline state and accessed from the tile function.
--
-- See: MTLLinkedFunctions
--
-- ObjC selector: @- setLinkedFunctions:@
setLinkedFunctions :: (IsMTLTileRenderPipelineDescriptor mtlTileRenderPipelineDescriptor, IsMTLLinkedFunctions value) => mtlTileRenderPipelineDescriptor -> value -> IO ()
setLinkedFunctions mtlTileRenderPipelineDescriptor value =
  sendMessage mtlTileRenderPipelineDescriptor setLinkedFunctionsSelector (toMTLLinkedFunctions value)

-- | supportAddingBinaryFunctions
--
-- This flag makes this pipeline support creating a new pipeline by adding binary functions.
--
-- ObjC selector: @- supportAddingBinaryFunctions@
supportAddingBinaryFunctions :: IsMTLTileRenderPipelineDescriptor mtlTileRenderPipelineDescriptor => mtlTileRenderPipelineDescriptor -> IO Bool
supportAddingBinaryFunctions mtlTileRenderPipelineDescriptor =
  sendMessage mtlTileRenderPipelineDescriptor supportAddingBinaryFunctionsSelector

-- | supportAddingBinaryFunctions
--
-- This flag makes this pipeline support creating a new pipeline by adding binary functions.
--
-- ObjC selector: @- setSupportAddingBinaryFunctions:@
setSupportAddingBinaryFunctions :: IsMTLTileRenderPipelineDescriptor mtlTileRenderPipelineDescriptor => mtlTileRenderPipelineDescriptor -> Bool -> IO ()
setSupportAddingBinaryFunctions mtlTileRenderPipelineDescriptor value =
  sendMessage mtlTileRenderPipelineDescriptor setSupportAddingBinaryFunctionsSelector value

-- | maxCallStackDepth
--
-- The maximum depth of the call stack in stack frames from the tile function. Defaults to 1 additional stack frame.
--
-- ObjC selector: @- maxCallStackDepth@
maxCallStackDepth :: IsMTLTileRenderPipelineDescriptor mtlTileRenderPipelineDescriptor => mtlTileRenderPipelineDescriptor -> IO CULong
maxCallStackDepth mtlTileRenderPipelineDescriptor =
  sendMessage mtlTileRenderPipelineDescriptor maxCallStackDepthSelector

-- | maxCallStackDepth
--
-- The maximum depth of the call stack in stack frames from the tile function. Defaults to 1 additional stack frame.
--
-- ObjC selector: @- setMaxCallStackDepth:@
setMaxCallStackDepth :: IsMTLTileRenderPipelineDescriptor mtlTileRenderPipelineDescriptor => mtlTileRenderPipelineDescriptor -> CULong -> IO ()
setMaxCallStackDepth mtlTileRenderPipelineDescriptor value =
  sendMessage mtlTileRenderPipelineDescriptor setMaxCallStackDepthSelector value

-- | shaderValidation
--
-- Toggle that determines whether Metal Shader Validation should be enabled or disabled for the pipeline.
--
-- The value can be overridden using @MTL_SHADER_VALIDATION_ENABLE_PIPELINES@ or @MTL_SHADER_VALIDATION_DISABLE_PIPELINES@ Environment Variables.
--
-- ObjC selector: @- shaderValidation@
shaderValidation :: IsMTLTileRenderPipelineDescriptor mtlTileRenderPipelineDescriptor => mtlTileRenderPipelineDescriptor -> IO MTLShaderValidation
shaderValidation mtlTileRenderPipelineDescriptor =
  sendMessage mtlTileRenderPipelineDescriptor shaderValidationSelector

-- | shaderValidation
--
-- Toggle that determines whether Metal Shader Validation should be enabled or disabled for the pipeline.
--
-- The value can be overridden using @MTL_SHADER_VALIDATION_ENABLE_PIPELINES@ or @MTL_SHADER_VALIDATION_DISABLE_PIPELINES@ Environment Variables.
--
-- ObjC selector: @- setShaderValidation:@
setShaderValidation :: IsMTLTileRenderPipelineDescriptor mtlTileRenderPipelineDescriptor => mtlTileRenderPipelineDescriptor -> MTLShaderValidation -> IO ()
setShaderValidation mtlTileRenderPipelineDescriptor value =
  sendMessage mtlTileRenderPipelineDescriptor setShaderValidationSelector value

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

-- | @Selector@ for @tileFunction@
tileFunctionSelector :: Selector '[] RawId
tileFunctionSelector = mkSelector "tileFunction"

-- | @Selector@ for @setTileFunction:@
setTileFunctionSelector :: Selector '[RawId] ()
setTileFunctionSelector = mkSelector "setTileFunction:"

-- | @Selector@ for @rasterSampleCount@
rasterSampleCountSelector :: Selector '[] CULong
rasterSampleCountSelector = mkSelector "rasterSampleCount"

-- | @Selector@ for @setRasterSampleCount:@
setRasterSampleCountSelector :: Selector '[CULong] ()
setRasterSampleCountSelector = mkSelector "setRasterSampleCount:"

-- | @Selector@ for @colorAttachments@
colorAttachmentsSelector :: Selector '[] (Id MTLTileRenderPipelineColorAttachmentDescriptorArray)
colorAttachmentsSelector = mkSelector "colorAttachments"

-- | @Selector@ for @threadgroupSizeMatchesTileSize@
threadgroupSizeMatchesTileSizeSelector :: Selector '[] Bool
threadgroupSizeMatchesTileSizeSelector = mkSelector "threadgroupSizeMatchesTileSize"

-- | @Selector@ for @setThreadgroupSizeMatchesTileSize:@
setThreadgroupSizeMatchesTileSizeSelector :: Selector '[Bool] ()
setThreadgroupSizeMatchesTileSizeSelector = mkSelector "setThreadgroupSizeMatchesTileSize:"

-- | @Selector@ for @tileBuffers@
tileBuffersSelector :: Selector '[] (Id MTLPipelineBufferDescriptorArray)
tileBuffersSelector = mkSelector "tileBuffers"

-- | @Selector@ for @maxTotalThreadsPerThreadgroup@
maxTotalThreadsPerThreadgroupSelector :: Selector '[] CULong
maxTotalThreadsPerThreadgroupSelector = mkSelector "maxTotalThreadsPerThreadgroup"

-- | @Selector@ for @setMaxTotalThreadsPerThreadgroup:@
setMaxTotalThreadsPerThreadgroupSelector :: Selector '[CULong] ()
setMaxTotalThreadsPerThreadgroupSelector = mkSelector "setMaxTotalThreadsPerThreadgroup:"

-- | @Selector@ for @binaryArchives@
binaryArchivesSelector :: Selector '[] (Id NSArray)
binaryArchivesSelector = mkSelector "binaryArchives"

-- | @Selector@ for @setBinaryArchives:@
setBinaryArchivesSelector :: Selector '[Id NSArray] ()
setBinaryArchivesSelector = mkSelector "setBinaryArchives:"

-- | @Selector@ for @preloadedLibraries@
preloadedLibrariesSelector :: Selector '[] (Id NSArray)
preloadedLibrariesSelector = mkSelector "preloadedLibraries"

-- | @Selector@ for @setPreloadedLibraries:@
setPreloadedLibrariesSelector :: Selector '[Id NSArray] ()
setPreloadedLibrariesSelector = mkSelector "setPreloadedLibraries:"

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

