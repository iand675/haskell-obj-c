{-# LANGUAGE PatternSynonyms #-}
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
  , resetSelector
  , computeFunctionDescriptorSelector
  , setComputeFunctionDescriptorSelector
  , threadGroupSizeIsMultipleOfThreadExecutionWidthSelector
  , setThreadGroupSizeIsMultipleOfThreadExecutionWidthSelector
  , maxTotalThreadsPerThreadgroupSelector
  , setMaxTotalThreadsPerThreadgroupSelector
  , supportBinaryLinkingSelector
  , setSupportBinaryLinkingSelector
  , staticLinkingDescriptorSelector
  , setStaticLinkingDescriptorSelector
  , supportIndirectCommandBuffersSelector
  , setSupportIndirectCommandBuffersSelector

  -- * Enum types
  , MTL4IndirectCommandBufferSupportState(MTL4IndirectCommandBufferSupportState)
  , pattern MTL4IndirectCommandBufferSupportStateDisabled
  , pattern MTL4IndirectCommandBufferSupportStateEnabled

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

-- | Resets the descriptor to its default values.
--
-- ObjC selector: @- reset@
reset :: IsMTL4ComputePipelineDescriptor mtL4ComputePipelineDescriptor => mtL4ComputePipelineDescriptor -> IO ()
reset mtL4ComputePipelineDescriptor  =
  sendMsg mtL4ComputePipelineDescriptor (mkSelector "reset") retVoid []

-- | A descriptor representing the compute pipeline's function.
--
-- You don't assign instances of ``MTL4FunctionDescriptor`` to this property directly, instead assign an instance of one of its subclasses, such as ``MTL4LibraryFunctionDescriptor``, which represents a function from a Metal library.
--
-- ObjC selector: @- computeFunctionDescriptor@
computeFunctionDescriptor :: IsMTL4ComputePipelineDescriptor mtL4ComputePipelineDescriptor => mtL4ComputePipelineDescriptor -> IO (Id MTL4FunctionDescriptor)
computeFunctionDescriptor mtL4ComputePipelineDescriptor  =
  sendMsg mtL4ComputePipelineDescriptor (mkSelector "computeFunctionDescriptor") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | A descriptor representing the compute pipeline's function.
--
-- You don't assign instances of ``MTL4FunctionDescriptor`` to this property directly, instead assign an instance of one of its subclasses, such as ``MTL4LibraryFunctionDescriptor``, which represents a function from a Metal library.
--
-- ObjC selector: @- setComputeFunctionDescriptor:@
setComputeFunctionDescriptor :: (IsMTL4ComputePipelineDescriptor mtL4ComputePipelineDescriptor, IsMTL4FunctionDescriptor value) => mtL4ComputePipelineDescriptor -> value -> IO ()
setComputeFunctionDescriptor mtL4ComputePipelineDescriptor  value =
withObjCPtr value $ \raw_value ->
    sendMsg mtL4ComputePipelineDescriptor (mkSelector "setComputeFunctionDescriptor:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- | A boolean value indicating whether each dimension of the threadgroup size is a multiple of its corresponding thread execution width.
--
-- ObjC selector: @- threadGroupSizeIsMultipleOfThreadExecutionWidth@
threadGroupSizeIsMultipleOfThreadExecutionWidth :: IsMTL4ComputePipelineDescriptor mtL4ComputePipelineDescriptor => mtL4ComputePipelineDescriptor -> IO Bool
threadGroupSizeIsMultipleOfThreadExecutionWidth mtL4ComputePipelineDescriptor  =
  fmap ((/= 0) :: CULong -> Bool) $ sendMsg mtL4ComputePipelineDescriptor (mkSelector "threadGroupSizeIsMultipleOfThreadExecutionWidth") retCULong []

-- | A boolean value indicating whether each dimension of the threadgroup size is a multiple of its corresponding thread execution width.
--
-- ObjC selector: @- setThreadGroupSizeIsMultipleOfThreadExecutionWidth:@
setThreadGroupSizeIsMultipleOfThreadExecutionWidth :: IsMTL4ComputePipelineDescriptor mtL4ComputePipelineDescriptor => mtL4ComputePipelineDescriptor -> Bool -> IO ()
setThreadGroupSizeIsMultipleOfThreadExecutionWidth mtL4ComputePipelineDescriptor  value =
  sendMsg mtL4ComputePipelineDescriptor (mkSelector "setThreadGroupSizeIsMultipleOfThreadExecutionWidth:") retVoid [argCULong (if value then 1 else 0)]

-- | The maximum total number of threads that Metal can execute in a single threadgroup for the compute function.
--
-- ObjC selector: @- maxTotalThreadsPerThreadgroup@
maxTotalThreadsPerThreadgroup :: IsMTL4ComputePipelineDescriptor mtL4ComputePipelineDescriptor => mtL4ComputePipelineDescriptor -> IO CULong
maxTotalThreadsPerThreadgroup mtL4ComputePipelineDescriptor  =
  sendMsg mtL4ComputePipelineDescriptor (mkSelector "maxTotalThreadsPerThreadgroup") retCULong []

-- | The maximum total number of threads that Metal can execute in a single threadgroup for the compute function.
--
-- ObjC selector: @- setMaxTotalThreadsPerThreadgroup:@
setMaxTotalThreadsPerThreadgroup :: IsMTL4ComputePipelineDescriptor mtL4ComputePipelineDescriptor => mtL4ComputePipelineDescriptor -> CULong -> IO ()
setMaxTotalThreadsPerThreadgroup mtL4ComputePipelineDescriptor  value =
  sendMsg mtL4ComputePipelineDescriptor (mkSelector "setMaxTotalThreadsPerThreadgroup:") retVoid [argCULong (fromIntegral value)]

-- | A boolean value indicating whether the compute pipeline supports linking binary functions.
--
-- ObjC selector: @- supportBinaryLinking@
supportBinaryLinking :: IsMTL4ComputePipelineDescriptor mtL4ComputePipelineDescriptor => mtL4ComputePipelineDescriptor -> IO Bool
supportBinaryLinking mtL4ComputePipelineDescriptor  =
  fmap ((/= 0) :: CULong -> Bool) $ sendMsg mtL4ComputePipelineDescriptor (mkSelector "supportBinaryLinking") retCULong []

-- | A boolean value indicating whether the compute pipeline supports linking binary functions.
--
-- ObjC selector: @- setSupportBinaryLinking:@
setSupportBinaryLinking :: IsMTL4ComputePipelineDescriptor mtL4ComputePipelineDescriptor => mtL4ComputePipelineDescriptor -> Bool -> IO ()
setSupportBinaryLinking mtL4ComputePipelineDescriptor  value =
  sendMsg mtL4ComputePipelineDescriptor (mkSelector "setSupportBinaryLinking:") retVoid [argCULong (if value then 1 else 0)]

-- | An object that contains information about functions to link to the compute pipeline.
--
-- ObjC selector: @- staticLinkingDescriptor@
staticLinkingDescriptor :: IsMTL4ComputePipelineDescriptor mtL4ComputePipelineDescriptor => mtL4ComputePipelineDescriptor -> IO (Id MTL4StaticLinkingDescriptor)
staticLinkingDescriptor mtL4ComputePipelineDescriptor  =
  sendMsg mtL4ComputePipelineDescriptor (mkSelector "staticLinkingDescriptor") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | An object that contains information about functions to link to the compute pipeline.
--
-- ObjC selector: @- setStaticLinkingDescriptor:@
setStaticLinkingDescriptor :: (IsMTL4ComputePipelineDescriptor mtL4ComputePipelineDescriptor, IsMTL4StaticLinkingDescriptor value) => mtL4ComputePipelineDescriptor -> value -> IO ()
setStaticLinkingDescriptor mtL4ComputePipelineDescriptor  value =
withObjCPtr value $ \raw_value ->
    sendMsg mtL4ComputePipelineDescriptor (mkSelector "setStaticLinkingDescriptor:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- | A value indicating whether the pipeline supports Metal indirect command buffers.
--
-- ObjC selector: @- supportIndirectCommandBuffers@
supportIndirectCommandBuffers :: IsMTL4ComputePipelineDescriptor mtL4ComputePipelineDescriptor => mtL4ComputePipelineDescriptor -> IO MTL4IndirectCommandBufferSupportState
supportIndirectCommandBuffers mtL4ComputePipelineDescriptor  =
  fmap (coerce :: CLong -> MTL4IndirectCommandBufferSupportState) $ sendMsg mtL4ComputePipelineDescriptor (mkSelector "supportIndirectCommandBuffers") retCLong []

-- | A value indicating whether the pipeline supports Metal indirect command buffers.
--
-- ObjC selector: @- setSupportIndirectCommandBuffers:@
setSupportIndirectCommandBuffers :: IsMTL4ComputePipelineDescriptor mtL4ComputePipelineDescriptor => mtL4ComputePipelineDescriptor -> MTL4IndirectCommandBufferSupportState -> IO ()
setSupportIndirectCommandBuffers mtL4ComputePipelineDescriptor  value =
  sendMsg mtL4ComputePipelineDescriptor (mkSelector "setSupportIndirectCommandBuffers:") retVoid [argCLong (coerce value)]

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @reset@
resetSelector :: Selector
resetSelector = mkSelector "reset"

-- | @Selector@ for @computeFunctionDescriptor@
computeFunctionDescriptorSelector :: Selector
computeFunctionDescriptorSelector = mkSelector "computeFunctionDescriptor"

-- | @Selector@ for @setComputeFunctionDescriptor:@
setComputeFunctionDescriptorSelector :: Selector
setComputeFunctionDescriptorSelector = mkSelector "setComputeFunctionDescriptor:"

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

-- | @Selector@ for @supportBinaryLinking@
supportBinaryLinkingSelector :: Selector
supportBinaryLinkingSelector = mkSelector "supportBinaryLinking"

-- | @Selector@ for @setSupportBinaryLinking:@
setSupportBinaryLinkingSelector :: Selector
setSupportBinaryLinkingSelector = mkSelector "setSupportBinaryLinking:"

-- | @Selector@ for @staticLinkingDescriptor@
staticLinkingDescriptorSelector :: Selector
staticLinkingDescriptorSelector = mkSelector "staticLinkingDescriptor"

-- | @Selector@ for @setStaticLinkingDescriptor:@
setStaticLinkingDescriptorSelector :: Selector
setStaticLinkingDescriptorSelector = mkSelector "setStaticLinkingDescriptor:"

-- | @Selector@ for @supportIndirectCommandBuffers@
supportIndirectCommandBuffersSelector :: Selector
supportIndirectCommandBuffersSelector = mkSelector "supportIndirectCommandBuffers"

-- | @Selector@ for @setSupportIndirectCommandBuffers:@
setSupportIndirectCommandBuffersSelector :: Selector
setSupportIndirectCommandBuffersSelector = mkSelector "setSupportIndirectCommandBuffers:"

