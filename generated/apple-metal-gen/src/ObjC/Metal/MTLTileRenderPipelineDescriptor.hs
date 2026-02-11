{-# LANGUAGE PatternSynonyms #-}
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
  , resetSelector
  , labelSelector
  , setLabelSelector
  , tileFunctionSelector
  , setTileFunctionSelector
  , rasterSampleCountSelector
  , setRasterSampleCountSelector
  , colorAttachmentsSelector
  , threadgroupSizeMatchesTileSizeSelector
  , setThreadgroupSizeMatchesTileSizeSelector
  , tileBuffersSelector
  , maxTotalThreadsPerThreadgroupSelector
  , setMaxTotalThreadsPerThreadgroupSelector
  , binaryArchivesSelector
  , setBinaryArchivesSelector
  , preloadedLibrariesSelector
  , setPreloadedLibrariesSelector
  , linkedFunctionsSelector
  , setLinkedFunctionsSelector
  , supportAddingBinaryFunctionsSelector
  , setSupportAddingBinaryFunctionsSelector
  , maxCallStackDepthSelector
  , setMaxCallStackDepthSelector
  , shaderValidationSelector
  , setShaderValidationSelector

  -- * Enum types
  , MTLShaderValidation(MTLShaderValidation)
  , pattern MTLShaderValidationDefault
  , pattern MTLShaderValidationEnabled
  , pattern MTLShaderValidationDisabled

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

import ObjC.Metal.Internal.Classes
import ObjC.Metal.Internal.Enums
import ObjC.Foundation.Internal.Classes

-- | @- reset@
reset :: IsMTLTileRenderPipelineDescriptor mtlTileRenderPipelineDescriptor => mtlTileRenderPipelineDescriptor -> IO ()
reset mtlTileRenderPipelineDescriptor  =
    sendMsg mtlTileRenderPipelineDescriptor (mkSelector "reset") retVoid []

-- | label:
--
-- The descriptor label.
--
-- ObjC selector: @- label@
label :: IsMTLTileRenderPipelineDescriptor mtlTileRenderPipelineDescriptor => mtlTileRenderPipelineDescriptor -> IO (Id NSString)
label mtlTileRenderPipelineDescriptor  =
    sendMsg mtlTileRenderPipelineDescriptor (mkSelector "label") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | label:
--
-- The descriptor label.
--
-- ObjC selector: @- setLabel:@
setLabel :: (IsMTLTileRenderPipelineDescriptor mtlTileRenderPipelineDescriptor, IsNSString value) => mtlTileRenderPipelineDescriptor -> value -> IO ()
setLabel mtlTileRenderPipelineDescriptor  value =
  withObjCPtr value $ \raw_value ->
      sendMsg mtlTileRenderPipelineDescriptor (mkSelector "setLabel:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- | tileFunction:
--
-- The kernel or fragment function that serves as the tile shader for this pipeline.
--
-- Both kernel-based and fragment-based tile pipelines dispatches will barrier against previous draws and other dispatches. Kernel-based pipelines will wait until all prior access to the tile completes. Fragment-based pipelines will only wait until all prior access to the fragment's location completes.
--
-- ObjC selector: @- tileFunction@
tileFunction :: IsMTLTileRenderPipelineDescriptor mtlTileRenderPipelineDescriptor => mtlTileRenderPipelineDescriptor -> IO RawId
tileFunction mtlTileRenderPipelineDescriptor  =
    fmap (RawId . castPtr) $ sendMsg mtlTileRenderPipelineDescriptor (mkSelector "tileFunction") (retPtr retVoid) []

-- | tileFunction:
--
-- The kernel or fragment function that serves as the tile shader for this pipeline.
--
-- Both kernel-based and fragment-based tile pipelines dispatches will barrier against previous draws and other dispatches. Kernel-based pipelines will wait until all prior access to the tile completes. Fragment-based pipelines will only wait until all prior access to the fragment's location completes.
--
-- ObjC selector: @- setTileFunction:@
setTileFunction :: IsMTLTileRenderPipelineDescriptor mtlTileRenderPipelineDescriptor => mtlTileRenderPipelineDescriptor -> RawId -> IO ()
setTileFunction mtlTileRenderPipelineDescriptor  value =
    sendMsg mtlTileRenderPipelineDescriptor (mkSelector "setTileFunction:") retVoid [argPtr (castPtr (unRawId value) :: Ptr ())]

-- | @- rasterSampleCount@
rasterSampleCount :: IsMTLTileRenderPipelineDescriptor mtlTileRenderPipelineDescriptor => mtlTileRenderPipelineDescriptor -> IO CULong
rasterSampleCount mtlTileRenderPipelineDescriptor  =
    sendMsg mtlTileRenderPipelineDescriptor (mkSelector "rasterSampleCount") retCULong []

-- | @- setRasterSampleCount:@
setRasterSampleCount :: IsMTLTileRenderPipelineDescriptor mtlTileRenderPipelineDescriptor => mtlTileRenderPipelineDescriptor -> CULong -> IO ()
setRasterSampleCount mtlTileRenderPipelineDescriptor  value =
    sendMsg mtlTileRenderPipelineDescriptor (mkSelector "setRasterSampleCount:") retVoid [argCULong value]

-- | @- colorAttachments@
colorAttachments :: IsMTLTileRenderPipelineDescriptor mtlTileRenderPipelineDescriptor => mtlTileRenderPipelineDescriptor -> IO (Id MTLTileRenderPipelineColorAttachmentDescriptorArray)
colorAttachments mtlTileRenderPipelineDescriptor  =
    sendMsg mtlTileRenderPipelineDescriptor (mkSelector "colorAttachments") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | threadgroupSizeMatchesTileSize:
--
-- Whether all threadgroups associated with this pipeline will cover tiles entirely.
--
-- Metal can optimize code generation for this case.
--
-- ObjC selector: @- threadgroupSizeMatchesTileSize@
threadgroupSizeMatchesTileSize :: IsMTLTileRenderPipelineDescriptor mtlTileRenderPipelineDescriptor => mtlTileRenderPipelineDescriptor -> IO Bool
threadgroupSizeMatchesTileSize mtlTileRenderPipelineDescriptor  =
    fmap ((/= 0) :: CULong -> Bool) $ sendMsg mtlTileRenderPipelineDescriptor (mkSelector "threadgroupSizeMatchesTileSize") retCULong []

-- | threadgroupSizeMatchesTileSize:
--
-- Whether all threadgroups associated with this pipeline will cover tiles entirely.
--
-- Metal can optimize code generation for this case.
--
-- ObjC selector: @- setThreadgroupSizeMatchesTileSize:@
setThreadgroupSizeMatchesTileSize :: IsMTLTileRenderPipelineDescriptor mtlTileRenderPipelineDescriptor => mtlTileRenderPipelineDescriptor -> Bool -> IO ()
setThreadgroupSizeMatchesTileSize mtlTileRenderPipelineDescriptor  value =
    sendMsg mtlTileRenderPipelineDescriptor (mkSelector "setThreadgroupSizeMatchesTileSize:") retVoid [argCULong (if value then 1 else 0)]

-- | @- tileBuffers@
tileBuffers :: IsMTLTileRenderPipelineDescriptor mtlTileRenderPipelineDescriptor => mtlTileRenderPipelineDescriptor -> IO (Id MTLPipelineBufferDescriptorArray)
tileBuffers mtlTileRenderPipelineDescriptor  =
    sendMsg mtlTileRenderPipelineDescriptor (mkSelector "tileBuffers") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | maxTotalThreadsPerThreadgroup
--
-- Optional property. Set the maxTotalThreadsPerThreadgroup. If it is not set, returns zero.
--
-- ObjC selector: @- maxTotalThreadsPerThreadgroup@
maxTotalThreadsPerThreadgroup :: IsMTLTileRenderPipelineDescriptor mtlTileRenderPipelineDescriptor => mtlTileRenderPipelineDescriptor -> IO CULong
maxTotalThreadsPerThreadgroup mtlTileRenderPipelineDescriptor  =
    sendMsg mtlTileRenderPipelineDescriptor (mkSelector "maxTotalThreadsPerThreadgroup") retCULong []

-- | maxTotalThreadsPerThreadgroup
--
-- Optional property. Set the maxTotalThreadsPerThreadgroup. If it is not set, returns zero.
--
-- ObjC selector: @- setMaxTotalThreadsPerThreadgroup:@
setMaxTotalThreadsPerThreadgroup :: IsMTLTileRenderPipelineDescriptor mtlTileRenderPipelineDescriptor => mtlTileRenderPipelineDescriptor -> CULong -> IO ()
setMaxTotalThreadsPerThreadgroup mtlTileRenderPipelineDescriptor  value =
    sendMsg mtlTileRenderPipelineDescriptor (mkSelector "setMaxTotalThreadsPerThreadgroup:") retVoid [argCULong value]

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
binaryArchives mtlTileRenderPipelineDescriptor  =
    sendMsg mtlTileRenderPipelineDescriptor (mkSelector "binaryArchives") (retPtr retVoid) [] >>= retainedObject . castPtr

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
setBinaryArchives mtlTileRenderPipelineDescriptor  value =
  withObjCPtr value $ \raw_value ->
      sendMsg mtlTileRenderPipelineDescriptor (mkSelector "setBinaryArchives:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

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
preloadedLibraries mtlTileRenderPipelineDescriptor  =
    sendMsg mtlTileRenderPipelineDescriptor (mkSelector "preloadedLibraries") (retPtr retVoid) [] >>= retainedObject . castPtr

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
setPreloadedLibraries mtlTileRenderPipelineDescriptor  value =
  withObjCPtr value $ \raw_value ->
      sendMsg mtlTileRenderPipelineDescriptor (mkSelector "setPreloadedLibraries:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- | linkedFunctions
--
-- The set of functions to be linked with the pipeline state and accessed from the tile function.
--
-- See: MTLLinkedFunctions
--
-- ObjC selector: @- linkedFunctions@
linkedFunctions :: IsMTLTileRenderPipelineDescriptor mtlTileRenderPipelineDescriptor => mtlTileRenderPipelineDescriptor -> IO (Id MTLLinkedFunctions)
linkedFunctions mtlTileRenderPipelineDescriptor  =
    sendMsg mtlTileRenderPipelineDescriptor (mkSelector "linkedFunctions") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | linkedFunctions
--
-- The set of functions to be linked with the pipeline state and accessed from the tile function.
--
-- See: MTLLinkedFunctions
--
-- ObjC selector: @- setLinkedFunctions:@
setLinkedFunctions :: (IsMTLTileRenderPipelineDescriptor mtlTileRenderPipelineDescriptor, IsMTLLinkedFunctions value) => mtlTileRenderPipelineDescriptor -> value -> IO ()
setLinkedFunctions mtlTileRenderPipelineDescriptor  value =
  withObjCPtr value $ \raw_value ->
      sendMsg mtlTileRenderPipelineDescriptor (mkSelector "setLinkedFunctions:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- | supportAddingBinaryFunctions
--
-- This flag makes this pipeline support creating a new pipeline by adding binary functions.
--
-- ObjC selector: @- supportAddingBinaryFunctions@
supportAddingBinaryFunctions :: IsMTLTileRenderPipelineDescriptor mtlTileRenderPipelineDescriptor => mtlTileRenderPipelineDescriptor -> IO Bool
supportAddingBinaryFunctions mtlTileRenderPipelineDescriptor  =
    fmap ((/= 0) :: CULong -> Bool) $ sendMsg mtlTileRenderPipelineDescriptor (mkSelector "supportAddingBinaryFunctions") retCULong []

-- | supportAddingBinaryFunctions
--
-- This flag makes this pipeline support creating a new pipeline by adding binary functions.
--
-- ObjC selector: @- setSupportAddingBinaryFunctions:@
setSupportAddingBinaryFunctions :: IsMTLTileRenderPipelineDescriptor mtlTileRenderPipelineDescriptor => mtlTileRenderPipelineDescriptor -> Bool -> IO ()
setSupportAddingBinaryFunctions mtlTileRenderPipelineDescriptor  value =
    sendMsg mtlTileRenderPipelineDescriptor (mkSelector "setSupportAddingBinaryFunctions:") retVoid [argCULong (if value then 1 else 0)]

-- | maxCallStackDepth
--
-- The maximum depth of the call stack in stack frames from the tile function. Defaults to 1 additional stack frame.
--
-- ObjC selector: @- maxCallStackDepth@
maxCallStackDepth :: IsMTLTileRenderPipelineDescriptor mtlTileRenderPipelineDescriptor => mtlTileRenderPipelineDescriptor -> IO CULong
maxCallStackDepth mtlTileRenderPipelineDescriptor  =
    sendMsg mtlTileRenderPipelineDescriptor (mkSelector "maxCallStackDepth") retCULong []

-- | maxCallStackDepth
--
-- The maximum depth of the call stack in stack frames from the tile function. Defaults to 1 additional stack frame.
--
-- ObjC selector: @- setMaxCallStackDepth:@
setMaxCallStackDepth :: IsMTLTileRenderPipelineDescriptor mtlTileRenderPipelineDescriptor => mtlTileRenderPipelineDescriptor -> CULong -> IO ()
setMaxCallStackDepth mtlTileRenderPipelineDescriptor  value =
    sendMsg mtlTileRenderPipelineDescriptor (mkSelector "setMaxCallStackDepth:") retVoid [argCULong value]

-- | shaderValidation
--
-- Toggle that determines whether Metal Shader Validation should be enabled or disabled for the pipeline.
--
-- The value can be overridden using @MTL_SHADER_VALIDATION_ENABLE_PIPELINES@ or @MTL_SHADER_VALIDATION_DISABLE_PIPELINES@ Environment Variables.
--
-- ObjC selector: @- shaderValidation@
shaderValidation :: IsMTLTileRenderPipelineDescriptor mtlTileRenderPipelineDescriptor => mtlTileRenderPipelineDescriptor -> IO MTLShaderValidation
shaderValidation mtlTileRenderPipelineDescriptor  =
    fmap (coerce :: CLong -> MTLShaderValidation) $ sendMsg mtlTileRenderPipelineDescriptor (mkSelector "shaderValidation") retCLong []

-- | shaderValidation
--
-- Toggle that determines whether Metal Shader Validation should be enabled or disabled for the pipeline.
--
-- The value can be overridden using @MTL_SHADER_VALIDATION_ENABLE_PIPELINES@ or @MTL_SHADER_VALIDATION_DISABLE_PIPELINES@ Environment Variables.
--
-- ObjC selector: @- setShaderValidation:@
setShaderValidation :: IsMTLTileRenderPipelineDescriptor mtlTileRenderPipelineDescriptor => mtlTileRenderPipelineDescriptor -> MTLShaderValidation -> IO ()
setShaderValidation mtlTileRenderPipelineDescriptor  value =
    sendMsg mtlTileRenderPipelineDescriptor (mkSelector "setShaderValidation:") retVoid [argCLong (coerce value)]

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @reset@
resetSelector :: Selector
resetSelector = mkSelector "reset"

-- | @Selector@ for @label@
labelSelector :: Selector
labelSelector = mkSelector "label"

-- | @Selector@ for @setLabel:@
setLabelSelector :: Selector
setLabelSelector = mkSelector "setLabel:"

-- | @Selector@ for @tileFunction@
tileFunctionSelector :: Selector
tileFunctionSelector = mkSelector "tileFunction"

-- | @Selector@ for @setTileFunction:@
setTileFunctionSelector :: Selector
setTileFunctionSelector = mkSelector "setTileFunction:"

-- | @Selector@ for @rasterSampleCount@
rasterSampleCountSelector :: Selector
rasterSampleCountSelector = mkSelector "rasterSampleCount"

-- | @Selector@ for @setRasterSampleCount:@
setRasterSampleCountSelector :: Selector
setRasterSampleCountSelector = mkSelector "setRasterSampleCount:"

-- | @Selector@ for @colorAttachments@
colorAttachmentsSelector :: Selector
colorAttachmentsSelector = mkSelector "colorAttachments"

-- | @Selector@ for @threadgroupSizeMatchesTileSize@
threadgroupSizeMatchesTileSizeSelector :: Selector
threadgroupSizeMatchesTileSizeSelector = mkSelector "threadgroupSizeMatchesTileSize"

-- | @Selector@ for @setThreadgroupSizeMatchesTileSize:@
setThreadgroupSizeMatchesTileSizeSelector :: Selector
setThreadgroupSizeMatchesTileSizeSelector = mkSelector "setThreadgroupSizeMatchesTileSize:"

-- | @Selector@ for @tileBuffers@
tileBuffersSelector :: Selector
tileBuffersSelector = mkSelector "tileBuffers"

-- | @Selector@ for @maxTotalThreadsPerThreadgroup@
maxTotalThreadsPerThreadgroupSelector :: Selector
maxTotalThreadsPerThreadgroupSelector = mkSelector "maxTotalThreadsPerThreadgroup"

-- | @Selector@ for @setMaxTotalThreadsPerThreadgroup:@
setMaxTotalThreadsPerThreadgroupSelector :: Selector
setMaxTotalThreadsPerThreadgroupSelector = mkSelector "setMaxTotalThreadsPerThreadgroup:"

-- | @Selector@ for @binaryArchives@
binaryArchivesSelector :: Selector
binaryArchivesSelector = mkSelector "binaryArchives"

-- | @Selector@ for @setBinaryArchives:@
setBinaryArchivesSelector :: Selector
setBinaryArchivesSelector = mkSelector "setBinaryArchives:"

-- | @Selector@ for @preloadedLibraries@
preloadedLibrariesSelector :: Selector
preloadedLibrariesSelector = mkSelector "preloadedLibraries"

-- | @Selector@ for @setPreloadedLibraries:@
setPreloadedLibrariesSelector :: Selector
setPreloadedLibrariesSelector = mkSelector "setPreloadedLibraries:"

-- | @Selector@ for @linkedFunctions@
linkedFunctionsSelector :: Selector
linkedFunctionsSelector = mkSelector "linkedFunctions"

-- | @Selector@ for @setLinkedFunctions:@
setLinkedFunctionsSelector :: Selector
setLinkedFunctionsSelector = mkSelector "setLinkedFunctions:"

-- | @Selector@ for @supportAddingBinaryFunctions@
supportAddingBinaryFunctionsSelector :: Selector
supportAddingBinaryFunctionsSelector = mkSelector "supportAddingBinaryFunctions"

-- | @Selector@ for @setSupportAddingBinaryFunctions:@
setSupportAddingBinaryFunctionsSelector :: Selector
setSupportAddingBinaryFunctionsSelector = mkSelector "setSupportAddingBinaryFunctions:"

-- | @Selector@ for @maxCallStackDepth@
maxCallStackDepthSelector :: Selector
maxCallStackDepthSelector = mkSelector "maxCallStackDepth"

-- | @Selector@ for @setMaxCallStackDepth:@
setMaxCallStackDepthSelector :: Selector
setMaxCallStackDepthSelector = mkSelector "setMaxCallStackDepth:"

-- | @Selector@ for @shaderValidation@
shaderValidationSelector :: Selector
shaderValidationSelector = mkSelector "shaderValidation"

-- | @Selector@ for @setShaderValidation:@
setShaderValidationSelector :: Selector
setShaderValidationSelector = mkSelector "setShaderValidation:"

