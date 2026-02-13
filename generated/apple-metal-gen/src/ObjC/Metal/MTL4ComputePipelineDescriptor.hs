{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Describes a compute pipeline state.
--
-- Generated bindings for @MTL4ComputePipelineDescriptor@.
module ObjC.Metal.MTL4ComputePipelineDescriptor
  ( MTL4ComputePipelineDescriptor
  , IsMTL4ComputePipelineDescriptor(..)
  , reset
  , computeFunctionDescriptor
  , setComputeFunctionDescriptor
  , threadGroupSizeIsMultipleOfThreadExecutionWidth
  , setThreadGroupSizeIsMultipleOfThreadExecutionWidth
  , maxTotalThreadsPerThreadgroup
  , setMaxTotalThreadsPerThreadgroup
  , supportBinaryLinking
  , setSupportBinaryLinking
  , staticLinkingDescriptor
  , setStaticLinkingDescriptor
  , supportIndirectCommandBuffers
  , setSupportIndirectCommandBuffers
  , computeFunctionDescriptorSelector
  , maxTotalThreadsPerThreadgroupSelector
  , resetSelector
  , setComputeFunctionDescriptorSelector
  , setMaxTotalThreadsPerThreadgroupSelector
  , setStaticLinkingDescriptorSelector
  , setSupportBinaryLinkingSelector
  , setSupportIndirectCommandBuffersSelector
  , setThreadGroupSizeIsMultipleOfThreadExecutionWidthSelector
  , staticLinkingDescriptorSelector
  , supportBinaryLinkingSelector
  , supportIndirectCommandBuffersSelector
  , threadGroupSizeIsMultipleOfThreadExecutionWidthSelector

  -- * Enum types
  , MTL4IndirectCommandBufferSupportState(MTL4IndirectCommandBufferSupportState)
  , pattern MTL4IndirectCommandBufferSupportStateDisabled
  , pattern MTL4IndirectCommandBufferSupportStateEnabled

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

-- | Resets the descriptor to its default values.
--
-- ObjC selector: @- reset@
reset :: IsMTL4ComputePipelineDescriptor mtL4ComputePipelineDescriptor => mtL4ComputePipelineDescriptor -> IO ()
reset mtL4ComputePipelineDescriptor =
  sendMessage mtL4ComputePipelineDescriptor resetSelector

-- | A descriptor representing the compute pipeline's function.
--
-- You don't assign instances of ``MTL4FunctionDescriptor`` to this property directly, instead assign an instance of one of its subclasses, such as ``MTL4LibraryFunctionDescriptor``, which represents a function from a Metal library.
--
-- ObjC selector: @- computeFunctionDescriptor@
computeFunctionDescriptor :: IsMTL4ComputePipelineDescriptor mtL4ComputePipelineDescriptor => mtL4ComputePipelineDescriptor -> IO (Id MTL4FunctionDescriptor)
computeFunctionDescriptor mtL4ComputePipelineDescriptor =
  sendMessage mtL4ComputePipelineDescriptor computeFunctionDescriptorSelector

-- | A descriptor representing the compute pipeline's function.
--
-- You don't assign instances of ``MTL4FunctionDescriptor`` to this property directly, instead assign an instance of one of its subclasses, such as ``MTL4LibraryFunctionDescriptor``, which represents a function from a Metal library.
--
-- ObjC selector: @- setComputeFunctionDescriptor:@
setComputeFunctionDescriptor :: (IsMTL4ComputePipelineDescriptor mtL4ComputePipelineDescriptor, IsMTL4FunctionDescriptor value) => mtL4ComputePipelineDescriptor -> value -> IO ()
setComputeFunctionDescriptor mtL4ComputePipelineDescriptor value =
  sendMessage mtL4ComputePipelineDescriptor setComputeFunctionDescriptorSelector (toMTL4FunctionDescriptor value)

-- | A boolean value indicating whether each dimension of the threadgroup size is a multiple of its corresponding thread execution width.
--
-- ObjC selector: @- threadGroupSizeIsMultipleOfThreadExecutionWidth@
threadGroupSizeIsMultipleOfThreadExecutionWidth :: IsMTL4ComputePipelineDescriptor mtL4ComputePipelineDescriptor => mtL4ComputePipelineDescriptor -> IO Bool
threadGroupSizeIsMultipleOfThreadExecutionWidth mtL4ComputePipelineDescriptor =
  sendMessage mtL4ComputePipelineDescriptor threadGroupSizeIsMultipleOfThreadExecutionWidthSelector

-- | A boolean value indicating whether each dimension of the threadgroup size is a multiple of its corresponding thread execution width.
--
-- ObjC selector: @- setThreadGroupSizeIsMultipleOfThreadExecutionWidth:@
setThreadGroupSizeIsMultipleOfThreadExecutionWidth :: IsMTL4ComputePipelineDescriptor mtL4ComputePipelineDescriptor => mtL4ComputePipelineDescriptor -> Bool -> IO ()
setThreadGroupSizeIsMultipleOfThreadExecutionWidth mtL4ComputePipelineDescriptor value =
  sendMessage mtL4ComputePipelineDescriptor setThreadGroupSizeIsMultipleOfThreadExecutionWidthSelector value

-- | The maximum total number of threads that Metal can execute in a single threadgroup for the compute function.
--
-- ObjC selector: @- maxTotalThreadsPerThreadgroup@
maxTotalThreadsPerThreadgroup :: IsMTL4ComputePipelineDescriptor mtL4ComputePipelineDescriptor => mtL4ComputePipelineDescriptor -> IO CULong
maxTotalThreadsPerThreadgroup mtL4ComputePipelineDescriptor =
  sendMessage mtL4ComputePipelineDescriptor maxTotalThreadsPerThreadgroupSelector

-- | The maximum total number of threads that Metal can execute in a single threadgroup for the compute function.
--
-- ObjC selector: @- setMaxTotalThreadsPerThreadgroup:@
setMaxTotalThreadsPerThreadgroup :: IsMTL4ComputePipelineDescriptor mtL4ComputePipelineDescriptor => mtL4ComputePipelineDescriptor -> CULong -> IO ()
setMaxTotalThreadsPerThreadgroup mtL4ComputePipelineDescriptor value =
  sendMessage mtL4ComputePipelineDescriptor setMaxTotalThreadsPerThreadgroupSelector value

-- | A boolean value indicating whether the compute pipeline supports linking binary functions.
--
-- ObjC selector: @- supportBinaryLinking@
supportBinaryLinking :: IsMTL4ComputePipelineDescriptor mtL4ComputePipelineDescriptor => mtL4ComputePipelineDescriptor -> IO Bool
supportBinaryLinking mtL4ComputePipelineDescriptor =
  sendMessage mtL4ComputePipelineDescriptor supportBinaryLinkingSelector

-- | A boolean value indicating whether the compute pipeline supports linking binary functions.
--
-- ObjC selector: @- setSupportBinaryLinking:@
setSupportBinaryLinking :: IsMTL4ComputePipelineDescriptor mtL4ComputePipelineDescriptor => mtL4ComputePipelineDescriptor -> Bool -> IO ()
setSupportBinaryLinking mtL4ComputePipelineDescriptor value =
  sendMessage mtL4ComputePipelineDescriptor setSupportBinaryLinkingSelector value

-- | An object that contains information about functions to link to the compute pipeline.
--
-- ObjC selector: @- staticLinkingDescriptor@
staticLinkingDescriptor :: IsMTL4ComputePipelineDescriptor mtL4ComputePipelineDescriptor => mtL4ComputePipelineDescriptor -> IO (Id MTL4StaticLinkingDescriptor)
staticLinkingDescriptor mtL4ComputePipelineDescriptor =
  sendMessage mtL4ComputePipelineDescriptor staticLinkingDescriptorSelector

-- | An object that contains information about functions to link to the compute pipeline.
--
-- ObjC selector: @- setStaticLinkingDescriptor:@
setStaticLinkingDescriptor :: (IsMTL4ComputePipelineDescriptor mtL4ComputePipelineDescriptor, IsMTL4StaticLinkingDescriptor value) => mtL4ComputePipelineDescriptor -> value -> IO ()
setStaticLinkingDescriptor mtL4ComputePipelineDescriptor value =
  sendMessage mtL4ComputePipelineDescriptor setStaticLinkingDescriptorSelector (toMTL4StaticLinkingDescriptor value)

-- | A value indicating whether the pipeline supports Metal indirect command buffers.
--
-- ObjC selector: @- supportIndirectCommandBuffers@
supportIndirectCommandBuffers :: IsMTL4ComputePipelineDescriptor mtL4ComputePipelineDescriptor => mtL4ComputePipelineDescriptor -> IO MTL4IndirectCommandBufferSupportState
supportIndirectCommandBuffers mtL4ComputePipelineDescriptor =
  sendMessage mtL4ComputePipelineDescriptor supportIndirectCommandBuffersSelector

-- | A value indicating whether the pipeline supports Metal indirect command buffers.
--
-- ObjC selector: @- setSupportIndirectCommandBuffers:@
setSupportIndirectCommandBuffers :: IsMTL4ComputePipelineDescriptor mtL4ComputePipelineDescriptor => mtL4ComputePipelineDescriptor -> MTL4IndirectCommandBufferSupportState -> IO ()
setSupportIndirectCommandBuffers mtL4ComputePipelineDescriptor value =
  sendMessage mtL4ComputePipelineDescriptor setSupportIndirectCommandBuffersSelector value

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @reset@
resetSelector :: Selector '[] ()
resetSelector = mkSelector "reset"

-- | @Selector@ for @computeFunctionDescriptor@
computeFunctionDescriptorSelector :: Selector '[] (Id MTL4FunctionDescriptor)
computeFunctionDescriptorSelector = mkSelector "computeFunctionDescriptor"

-- | @Selector@ for @setComputeFunctionDescriptor:@
setComputeFunctionDescriptorSelector :: Selector '[Id MTL4FunctionDescriptor] ()
setComputeFunctionDescriptorSelector = mkSelector "setComputeFunctionDescriptor:"

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

-- | @Selector@ for @supportBinaryLinking@
supportBinaryLinkingSelector :: Selector '[] Bool
supportBinaryLinkingSelector = mkSelector "supportBinaryLinking"

-- | @Selector@ for @setSupportBinaryLinking:@
setSupportBinaryLinkingSelector :: Selector '[Bool] ()
setSupportBinaryLinkingSelector = mkSelector "setSupportBinaryLinking:"

-- | @Selector@ for @staticLinkingDescriptor@
staticLinkingDescriptorSelector :: Selector '[] (Id MTL4StaticLinkingDescriptor)
staticLinkingDescriptorSelector = mkSelector "staticLinkingDescriptor"

-- | @Selector@ for @setStaticLinkingDescriptor:@
setStaticLinkingDescriptorSelector :: Selector '[Id MTL4StaticLinkingDescriptor] ()
setStaticLinkingDescriptorSelector = mkSelector "setStaticLinkingDescriptor:"

-- | @Selector@ for @supportIndirectCommandBuffers@
supportIndirectCommandBuffersSelector :: Selector '[] MTL4IndirectCommandBufferSupportState
supportIndirectCommandBuffersSelector = mkSelector "supportIndirectCommandBuffers"

-- | @Selector@ for @setSupportIndirectCommandBuffers:@
setSupportIndirectCommandBuffersSelector :: Selector '[MTL4IndirectCommandBufferSupportState] ()
setSupportIndirectCommandBuffersSelector = mkSelector "setSupportIndirectCommandBuffers:"

