{-# LANGUAGE PatternSynonyms #-}
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
  , threadGroupSizeIsMultipleOfThreadExecutionWidth
  , setThreadGroupSizeIsMultipleOfThreadExecutionWidth
  , maxTotalThreadsPerThreadgroup
  , setMaxTotalThreadsPerThreadgroup
  , stageInputDescriptor
  , setStageInputDescriptor
  , buffers
  , supportIndirectCommandBuffers
  , setSupportIndirectCommandBuffers
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
  , threadGroupSizeIsMultipleOfThreadExecutionWidthSelector
  , setThreadGroupSizeIsMultipleOfThreadExecutionWidthSelector
  , maxTotalThreadsPerThreadgroupSelector
  , setMaxTotalThreadsPerThreadgroupSelector
  , stageInputDescriptorSelector
  , setStageInputDescriptorSelector
  , buffersSelector
  , supportIndirectCommandBuffersSelector
  , setSupportIndirectCommandBuffersSelector
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

-- | reset
--
-- Restore all compute pipeline descriptor properties to their default values.
--
-- ObjC selector: @- reset@
reset :: IsMTLComputePipelineDescriptor mtlComputePipelineDescriptor => mtlComputePipelineDescriptor -> IO ()
reset mtlComputePipelineDescriptor  =
  sendMsg mtlComputePipelineDescriptor (mkSelector "reset") retVoid []

-- | label
--
-- A string to help identify this object.
--
-- ObjC selector: @- label@
label :: IsMTLComputePipelineDescriptor mtlComputePipelineDescriptor => mtlComputePipelineDescriptor -> IO (Id NSString)
label mtlComputePipelineDescriptor  =
  sendMsg mtlComputePipelineDescriptor (mkSelector "label") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | label
--
-- A string to help identify this object.
--
-- ObjC selector: @- setLabel:@
setLabel :: (IsMTLComputePipelineDescriptor mtlComputePipelineDescriptor, IsNSString value) => mtlComputePipelineDescriptor -> value -> IO ()
setLabel mtlComputePipelineDescriptor  value =
withObjCPtr value $ \raw_value ->
    sendMsg mtlComputePipelineDescriptor (mkSelector "setLabel:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- | threadGroupSizeIsMultipleOfThreadExecutionWidth
--
-- An optimization flag, set if the thread group size will always be a multiple of thread execution width
--
-- ObjC selector: @- threadGroupSizeIsMultipleOfThreadExecutionWidth@
threadGroupSizeIsMultipleOfThreadExecutionWidth :: IsMTLComputePipelineDescriptor mtlComputePipelineDescriptor => mtlComputePipelineDescriptor -> IO Bool
threadGroupSizeIsMultipleOfThreadExecutionWidth mtlComputePipelineDescriptor  =
  fmap ((/= 0) :: CULong -> Bool) $ sendMsg mtlComputePipelineDescriptor (mkSelector "threadGroupSizeIsMultipleOfThreadExecutionWidth") retCULong []

-- | threadGroupSizeIsMultipleOfThreadExecutionWidth
--
-- An optimization flag, set if the thread group size will always be a multiple of thread execution width
--
-- ObjC selector: @- setThreadGroupSizeIsMultipleOfThreadExecutionWidth:@
setThreadGroupSizeIsMultipleOfThreadExecutionWidth :: IsMTLComputePipelineDescriptor mtlComputePipelineDescriptor => mtlComputePipelineDescriptor -> Bool -> IO ()
setThreadGroupSizeIsMultipleOfThreadExecutionWidth mtlComputePipelineDescriptor  value =
  sendMsg mtlComputePipelineDescriptor (mkSelector "setThreadGroupSizeIsMultipleOfThreadExecutionWidth:") retVoid [argCULong (if value then 1 else 0)]

-- | maxTotalThreadsPerThreadgroup
--
-- Optional property. Set the maxTotalThreadsPerThreadgroup. If it is not set, returns zero.
--
-- ObjC selector: @- maxTotalThreadsPerThreadgroup@
maxTotalThreadsPerThreadgroup :: IsMTLComputePipelineDescriptor mtlComputePipelineDescriptor => mtlComputePipelineDescriptor -> IO CULong
maxTotalThreadsPerThreadgroup mtlComputePipelineDescriptor  =
  sendMsg mtlComputePipelineDescriptor (mkSelector "maxTotalThreadsPerThreadgroup") retCULong []

-- | maxTotalThreadsPerThreadgroup
--
-- Optional property. Set the maxTotalThreadsPerThreadgroup. If it is not set, returns zero.
--
-- ObjC selector: @- setMaxTotalThreadsPerThreadgroup:@
setMaxTotalThreadsPerThreadgroup :: IsMTLComputePipelineDescriptor mtlComputePipelineDescriptor => mtlComputePipelineDescriptor -> CULong -> IO ()
setMaxTotalThreadsPerThreadgroup mtlComputePipelineDescriptor  value =
  sendMsg mtlComputePipelineDescriptor (mkSelector "setMaxTotalThreadsPerThreadgroup:") retVoid [argCULong (fromIntegral value)]

-- | computeDataDescriptor
--
-- An MTLStageInputOutputDescriptor to fetch data from buffers
--
-- ObjC selector: @- stageInputDescriptor@
stageInputDescriptor :: IsMTLComputePipelineDescriptor mtlComputePipelineDescriptor => mtlComputePipelineDescriptor -> IO (Id MTLStageInputOutputDescriptor)
stageInputDescriptor mtlComputePipelineDescriptor  =
  sendMsg mtlComputePipelineDescriptor (mkSelector "stageInputDescriptor") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | computeDataDescriptor
--
-- An MTLStageInputOutputDescriptor to fetch data from buffers
--
-- ObjC selector: @- setStageInputDescriptor:@
setStageInputDescriptor :: (IsMTLComputePipelineDescriptor mtlComputePipelineDescriptor, IsMTLStageInputOutputDescriptor value) => mtlComputePipelineDescriptor -> value -> IO ()
setStageInputDescriptor mtlComputePipelineDescriptor  value =
withObjCPtr value $ \raw_value ->
    sendMsg mtlComputePipelineDescriptor (mkSelector "setStageInputDescriptor:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- | buffers
--
-- Optional properties for each buffer binding used by the compute function.
--
-- ObjC selector: @- buffers@
buffers :: IsMTLComputePipelineDescriptor mtlComputePipelineDescriptor => mtlComputePipelineDescriptor -> IO (Id MTLPipelineBufferDescriptorArray)
buffers mtlComputePipelineDescriptor  =
  sendMsg mtlComputePipelineDescriptor (mkSelector "buffers") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | supportIndirectCommandBuffers
--
-- This flag makes this pipeline usable with indirect command buffers.
--
-- ObjC selector: @- supportIndirectCommandBuffers@
supportIndirectCommandBuffers :: IsMTLComputePipelineDescriptor mtlComputePipelineDescriptor => mtlComputePipelineDescriptor -> IO Bool
supportIndirectCommandBuffers mtlComputePipelineDescriptor  =
  fmap ((/= 0) :: CULong -> Bool) $ sendMsg mtlComputePipelineDescriptor (mkSelector "supportIndirectCommandBuffers") retCULong []

-- | supportIndirectCommandBuffers
--
-- This flag makes this pipeline usable with indirect command buffers.
--
-- ObjC selector: @- setSupportIndirectCommandBuffers:@
setSupportIndirectCommandBuffers :: IsMTLComputePipelineDescriptor mtlComputePipelineDescriptor => mtlComputePipelineDescriptor -> Bool -> IO ()
setSupportIndirectCommandBuffers mtlComputePipelineDescriptor  value =
  sendMsg mtlComputePipelineDescriptor (mkSelector "setSupportIndirectCommandBuffers:") retVoid [argCULong (if value then 1 else 0)]

-- | linkedFunctions
--
-- The set of functions to be linked with the pipeline state and accessed from the compute function.
--
-- See: MTLLinkedFunctions
--
-- ObjC selector: @- linkedFunctions@
linkedFunctions :: IsMTLComputePipelineDescriptor mtlComputePipelineDescriptor => mtlComputePipelineDescriptor -> IO (Id MTLLinkedFunctions)
linkedFunctions mtlComputePipelineDescriptor  =
  sendMsg mtlComputePipelineDescriptor (mkSelector "linkedFunctions") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | linkedFunctions
--
-- The set of functions to be linked with the pipeline state and accessed from the compute function.
--
-- See: MTLLinkedFunctions
--
-- ObjC selector: @- setLinkedFunctions:@
setLinkedFunctions :: (IsMTLComputePipelineDescriptor mtlComputePipelineDescriptor, IsMTLLinkedFunctions value) => mtlComputePipelineDescriptor -> value -> IO ()
setLinkedFunctions mtlComputePipelineDescriptor  value =
withObjCPtr value $ \raw_value ->
    sendMsg mtlComputePipelineDescriptor (mkSelector "setLinkedFunctions:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- | supportAddingBinaryFunctions
--
-- This flag makes this pipeline support creating a new pipeline by adding binary functions.
--
-- ObjC selector: @- supportAddingBinaryFunctions@
supportAddingBinaryFunctions :: IsMTLComputePipelineDescriptor mtlComputePipelineDescriptor => mtlComputePipelineDescriptor -> IO Bool
supportAddingBinaryFunctions mtlComputePipelineDescriptor  =
  fmap ((/= 0) :: CULong -> Bool) $ sendMsg mtlComputePipelineDescriptor (mkSelector "supportAddingBinaryFunctions") retCULong []

-- | supportAddingBinaryFunctions
--
-- This flag makes this pipeline support creating a new pipeline by adding binary functions.
--
-- ObjC selector: @- setSupportAddingBinaryFunctions:@
setSupportAddingBinaryFunctions :: IsMTLComputePipelineDescriptor mtlComputePipelineDescriptor => mtlComputePipelineDescriptor -> Bool -> IO ()
setSupportAddingBinaryFunctions mtlComputePipelineDescriptor  value =
  sendMsg mtlComputePipelineDescriptor (mkSelector "setSupportAddingBinaryFunctions:") retVoid [argCULong (if value then 1 else 0)]

-- | maxCallStackDepth
--
-- The maximum depth of the call stack in stack frames from the kernel. Defaults to 1 additional stack frame.
--
-- ObjC selector: @- maxCallStackDepth@
maxCallStackDepth :: IsMTLComputePipelineDescriptor mtlComputePipelineDescriptor => mtlComputePipelineDescriptor -> IO CULong
maxCallStackDepth mtlComputePipelineDescriptor  =
  sendMsg mtlComputePipelineDescriptor (mkSelector "maxCallStackDepth") retCULong []

-- | maxCallStackDepth
--
-- The maximum depth of the call stack in stack frames from the kernel. Defaults to 1 additional stack frame.
--
-- ObjC selector: @- setMaxCallStackDepth:@
setMaxCallStackDepth :: IsMTLComputePipelineDescriptor mtlComputePipelineDescriptor => mtlComputePipelineDescriptor -> CULong -> IO ()
setMaxCallStackDepth mtlComputePipelineDescriptor  value =
  sendMsg mtlComputePipelineDescriptor (mkSelector "setMaxCallStackDepth:") retVoid [argCULong (fromIntegral value)]

-- | shaderValidation
--
-- Toggle that determines whether Metal Shader Validation should be enabled or disabled for the pipeline.
--
-- The value can be overridden using @MTL_SHADER_VALIDATION_ENABLE_PIPELINES@ or @MTL_SHADER_VALIDATION_DISABLE_PIPELINES@ Environment Variables.
--
-- ObjC selector: @- shaderValidation@
shaderValidation :: IsMTLComputePipelineDescriptor mtlComputePipelineDescriptor => mtlComputePipelineDescriptor -> IO MTLShaderValidation
shaderValidation mtlComputePipelineDescriptor  =
  fmap (coerce :: CLong -> MTLShaderValidation) $ sendMsg mtlComputePipelineDescriptor (mkSelector "shaderValidation") retCLong []

-- | shaderValidation
--
-- Toggle that determines whether Metal Shader Validation should be enabled or disabled for the pipeline.
--
-- The value can be overridden using @MTL_SHADER_VALIDATION_ENABLE_PIPELINES@ or @MTL_SHADER_VALIDATION_DISABLE_PIPELINES@ Environment Variables.
--
-- ObjC selector: @- setShaderValidation:@
setShaderValidation :: IsMTLComputePipelineDescriptor mtlComputePipelineDescriptor => mtlComputePipelineDescriptor -> MTLShaderValidation -> IO ()
setShaderValidation mtlComputePipelineDescriptor  value =
  sendMsg mtlComputePipelineDescriptor (mkSelector "setShaderValidation:") retVoid [argCLong (coerce value)]

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

-- | @Selector@ for @threadGroupSizeIsMultipleOfThreadExecutionWidth@
threadGroupSizeIsMultipleOfThreadExecutionWidthSelector :: Selector
threadGroupSizeIsMultipleOfThreadExecutionWidthSelector = mkSelector "threadGroupSizeIsMultipleOfThreadExecutionWidth"

-- | @Selector@ for @setThreadGroupSizeIsMultipleOfThreadExecutionWidth:@
setThreadGroupSizeIsMultipleOfThreadExecutionWidthSelector :: Selector
setThreadGroupSizeIsMultipleOfThreadExecutionWidthSelector = mkSelector "setThreadGroupSizeIsMultipleOfThreadExecutionWidth:"

-- | @Selector@ for @maxTotalThreadsPerThreadgroup@
maxTotalThreadsPerThreadgroupSelector :: Selector
maxTotalThreadsPerThreadgroupSelector = mkSelector "maxTotalThreadsPerThreadgroup"

-- | @Selector@ for @setMaxTotalThreadsPerThreadgroup:@
setMaxTotalThreadsPerThreadgroupSelector :: Selector
setMaxTotalThreadsPerThreadgroupSelector = mkSelector "setMaxTotalThreadsPerThreadgroup:"

-- | @Selector@ for @stageInputDescriptor@
stageInputDescriptorSelector :: Selector
stageInputDescriptorSelector = mkSelector "stageInputDescriptor"

-- | @Selector@ for @setStageInputDescriptor:@
setStageInputDescriptorSelector :: Selector
setStageInputDescriptorSelector = mkSelector "setStageInputDescriptor:"

-- | @Selector@ for @buffers@
buffersSelector :: Selector
buffersSelector = mkSelector "buffers"

-- | @Selector@ for @supportIndirectCommandBuffers@
supportIndirectCommandBuffersSelector :: Selector
supportIndirectCommandBuffersSelector = mkSelector "supportIndirectCommandBuffers"

-- | @Selector@ for @setSupportIndirectCommandBuffers:@
setSupportIndirectCommandBuffersSelector :: Selector
setSupportIndirectCommandBuffersSelector = mkSelector "setSupportIndirectCommandBuffers:"

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

