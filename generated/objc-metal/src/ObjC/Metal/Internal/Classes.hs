{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}

-- | Internal module: all type tags, aliases, type classes, and
-- hierarchy instances for this framework.
--
-- Exists to break import cycles between per-class modules.
-- Import the per-class modules for the public API.
module ObjC.Metal.Internal.Classes (
    module ObjC.Metal.Internal.Classes,
    module ObjC.Foundation.Internal.Classes,
  ) where

import Data.Proxy (Proxy(..))
import ObjC.Runtime.Types
import ObjC.Runtime.Class (getRequiredClass)
import ObjC.Foundation.Internal.Classes

-- ---------- MTL4AccelerationStructureGeometryDescriptor ----------

-- | Base class for all Metal 4 acceleration structure geometry descriptors.
--
-- Don't use this class directly. Use one of the derived classes instead.
-- 
-- Phantom type for @MTL4AccelerationStructureGeometryDescriptor@.
data MTL4AccelerationStructureGeometryDescriptor

instance IsObjCObject (Id MTL4AccelerationStructureGeometryDescriptor) where
  toRawId = idToRawId
  withObjCPtr = withIdObjCPtr
  staticClass _ = getRequiredClass "MTL4AccelerationStructureGeometryDescriptor"

class IsNSObject a => IsMTL4AccelerationStructureGeometryDescriptor a where
  toMTL4AccelerationStructureGeometryDescriptor :: a -> Id MTL4AccelerationStructureGeometryDescriptor

instance IsMTL4AccelerationStructureGeometryDescriptor (Id MTL4AccelerationStructureGeometryDescriptor) where
  toMTL4AccelerationStructureGeometryDescriptor = unsafeCastId

instance IsNSObject (Id MTL4AccelerationStructureGeometryDescriptor) where
  toNSObject = unsafeCastId

-- ---------- MTL4ArgumentTableDescriptor ----------

-- | Groups parameters for the creation of a Metal argument table.
--
-- Argument tables provide resource bindings to your Metal pipeline states.
-- 
-- Phantom type for @MTL4ArgumentTableDescriptor@.
data MTL4ArgumentTableDescriptor

instance IsObjCObject (Id MTL4ArgumentTableDescriptor) where
  toRawId = idToRawId
  withObjCPtr = withIdObjCPtr
  staticClass _ = getRequiredClass "MTL4ArgumentTableDescriptor"

class IsNSObject a => IsMTL4ArgumentTableDescriptor a where
  toMTL4ArgumentTableDescriptor :: a -> Id MTL4ArgumentTableDescriptor

instance IsMTL4ArgumentTableDescriptor (Id MTL4ArgumentTableDescriptor) where
  toMTL4ArgumentTableDescriptor = unsafeCastId

instance IsNSObject (Id MTL4ArgumentTableDescriptor) where
  toNSObject = unsafeCastId

-- ---------- MTL4BinaryFunctionDescriptor ----------

-- | Base interface for other function-derived interfaces.
-- 
-- Phantom type for @MTL4BinaryFunctionDescriptor@.
data MTL4BinaryFunctionDescriptor

instance IsObjCObject (Id MTL4BinaryFunctionDescriptor) where
  toRawId = idToRawId
  withObjCPtr = withIdObjCPtr
  staticClass _ = getRequiredClass "MTL4BinaryFunctionDescriptor"

class IsNSObject a => IsMTL4BinaryFunctionDescriptor a where
  toMTL4BinaryFunctionDescriptor :: a -> Id MTL4BinaryFunctionDescriptor

instance IsMTL4BinaryFunctionDescriptor (Id MTL4BinaryFunctionDescriptor) where
  toMTL4BinaryFunctionDescriptor = unsafeCastId

instance IsNSObject (Id MTL4BinaryFunctionDescriptor) where
  toNSObject = unsafeCastId

-- ---------- MTL4CommandAllocatorDescriptor ----------

-- | Groups together parameters for creating a command allocator.
-- 
-- Phantom type for @MTL4CommandAllocatorDescriptor@.
data MTL4CommandAllocatorDescriptor

instance IsObjCObject (Id MTL4CommandAllocatorDescriptor) where
  toRawId = idToRawId
  withObjCPtr = withIdObjCPtr
  staticClass _ = getRequiredClass "MTL4CommandAllocatorDescriptor"

class IsNSObject a => IsMTL4CommandAllocatorDescriptor a where
  toMTL4CommandAllocatorDescriptor :: a -> Id MTL4CommandAllocatorDescriptor

instance IsMTL4CommandAllocatorDescriptor (Id MTL4CommandAllocatorDescriptor) where
  toMTL4CommandAllocatorDescriptor = unsafeCastId

instance IsNSObject (Id MTL4CommandAllocatorDescriptor) where
  toNSObject = unsafeCastId

-- ---------- MTL4CommandBufferOptions ----------

-- | Options to configure a command buffer before encoding work into it.
-- 
-- Phantom type for @MTL4CommandBufferOptions@.
data MTL4CommandBufferOptions

instance IsObjCObject (Id MTL4CommandBufferOptions) where
  toRawId = idToRawId
  withObjCPtr = withIdObjCPtr
  staticClass _ = getRequiredClass "MTL4CommandBufferOptions"

class IsNSObject a => IsMTL4CommandBufferOptions a where
  toMTL4CommandBufferOptions :: a -> Id MTL4CommandBufferOptions

instance IsMTL4CommandBufferOptions (Id MTL4CommandBufferOptions) where
  toMTL4CommandBufferOptions = unsafeCastId

instance IsNSObject (Id MTL4CommandBufferOptions) where
  toNSObject = unsafeCastId

-- ---------- MTL4CommandQueueDescriptor ----------

-- | Groups together parameters for the creation of a new command queue.
-- 
-- Phantom type for @MTL4CommandQueueDescriptor@.
data MTL4CommandQueueDescriptor

instance IsObjCObject (Id MTL4CommandQueueDescriptor) where
  toRawId = idToRawId
  withObjCPtr = withIdObjCPtr
  staticClass _ = getRequiredClass "MTL4CommandQueueDescriptor"

class IsNSObject a => IsMTL4CommandQueueDescriptor a where
  toMTL4CommandQueueDescriptor :: a -> Id MTL4CommandQueueDescriptor

instance IsMTL4CommandQueueDescriptor (Id MTL4CommandQueueDescriptor) where
  toMTL4CommandQueueDescriptor = unsafeCastId

instance IsNSObject (Id MTL4CommandQueueDescriptor) where
  toNSObject = unsafeCastId

-- ---------- MTL4CommitOptions ----------

-- | Represents options to configure a commit operation on a command queue.
--
-- You pass these options as a parameter when you call ``MTL4CommandQueue/commit:count:options:``.
--
-- - Note Instances of this class are not thread-safe. If your app modifies a shared commit options instance from multiple threads simultaneously, you are responsible for providing external synchronization.
-- 
-- Phantom type for @MTL4CommitOptions@.
data MTL4CommitOptions

instance IsObjCObject (Id MTL4CommitOptions) where
  toRawId = idToRawId
  withObjCPtr = withIdObjCPtr
  staticClass _ = getRequiredClass "MTL4CommitOptions"

class IsNSObject a => IsMTL4CommitOptions a where
  toMTL4CommitOptions :: a -> Id MTL4CommitOptions

instance IsMTL4CommitOptions (Id MTL4CommitOptions) where
  toMTL4CommitOptions = unsafeCastId

instance IsNSObject (Id MTL4CommitOptions) where
  toNSObject = unsafeCastId

-- ---------- MTL4CompilerDescriptor ----------

-- | Groups together properties for creating a compiler context.
-- 
-- Phantom type for @MTL4CompilerDescriptor@.
data MTL4CompilerDescriptor

instance IsObjCObject (Id MTL4CompilerDescriptor) where
  toRawId = idToRawId
  withObjCPtr = withIdObjCPtr
  staticClass _ = getRequiredClass "MTL4CompilerDescriptor"

class IsNSObject a => IsMTL4CompilerDescriptor a where
  toMTL4CompilerDescriptor :: a -> Id MTL4CompilerDescriptor

instance IsMTL4CompilerDescriptor (Id MTL4CompilerDescriptor) where
  toMTL4CompilerDescriptor = unsafeCastId

instance IsNSObject (Id MTL4CompilerDescriptor) where
  toNSObject = unsafeCastId

-- ---------- MTL4CompilerTaskOptions ----------

-- | The configuration options that control the behavior of a compilation task for a Metal 4 compiler instance.
--
-- You can configure task-specific settings that affect a compilation task by creating an instance of this class, setting its properties, and passing it to one of the applicable methods of an ``MTL4Compiler`` instance.
-- 
-- Phantom type for @MTL4CompilerTaskOptions@.
data MTL4CompilerTaskOptions

instance IsObjCObject (Id MTL4CompilerTaskOptions) where
  toRawId = idToRawId
  withObjCPtr = withIdObjCPtr
  staticClass _ = getRequiredClass "MTL4CompilerTaskOptions"

class IsNSObject a => IsMTL4CompilerTaskOptions a where
  toMTL4CompilerTaskOptions :: a -> Id MTL4CompilerTaskOptions

instance IsMTL4CompilerTaskOptions (Id MTL4CompilerTaskOptions) where
  toMTL4CompilerTaskOptions = unsafeCastId

instance IsNSObject (Id MTL4CompilerTaskOptions) where
  toNSObject = unsafeCastId

-- ---------- MTL4CounterHeapDescriptor ----------

-- | Groups together parameters for configuring a counter heap object at creation time.
-- 
-- Phantom type for @MTL4CounterHeapDescriptor@.
data MTL4CounterHeapDescriptor

instance IsObjCObject (Id MTL4CounterHeapDescriptor) where
  toRawId = idToRawId
  withObjCPtr = withIdObjCPtr
  staticClass _ = getRequiredClass "MTL4CounterHeapDescriptor"

class IsNSObject a => IsMTL4CounterHeapDescriptor a where
  toMTL4CounterHeapDescriptor :: a -> Id MTL4CounterHeapDescriptor

instance IsMTL4CounterHeapDescriptor (Id MTL4CounterHeapDescriptor) where
  toMTL4CounterHeapDescriptor = unsafeCastId

instance IsNSObject (Id MTL4CounterHeapDescriptor) where
  toNSObject = unsafeCastId

-- ---------- MTL4FunctionDescriptor ----------

-- | Base interface for describing a Metal 4 shader function.
-- 
-- Phantom type for @MTL4FunctionDescriptor@.
data MTL4FunctionDescriptor

instance IsObjCObject (Id MTL4FunctionDescriptor) where
  toRawId = idToRawId
  withObjCPtr = withIdObjCPtr
  staticClass _ = getRequiredClass "MTL4FunctionDescriptor"

class IsNSObject a => IsMTL4FunctionDescriptor a where
  toMTL4FunctionDescriptor :: a -> Id MTL4FunctionDescriptor

instance IsMTL4FunctionDescriptor (Id MTL4FunctionDescriptor) where
  toMTL4FunctionDescriptor = unsafeCastId

instance IsNSObject (Id MTL4FunctionDescriptor) where
  toNSObject = unsafeCastId

-- ---------- MTL4LibraryDescriptor ----------

-- | Serves as the base descriptor for creating a Metal library.
-- 
-- Phantom type for @MTL4LibraryDescriptor@.
data MTL4LibraryDescriptor

instance IsObjCObject (Id MTL4LibraryDescriptor) where
  toRawId = idToRawId
  withObjCPtr = withIdObjCPtr
  staticClass _ = getRequiredClass "MTL4LibraryDescriptor"

class IsNSObject a => IsMTL4LibraryDescriptor a where
  toMTL4LibraryDescriptor :: a -> Id MTL4LibraryDescriptor

instance IsMTL4LibraryDescriptor (Id MTL4LibraryDescriptor) where
  toMTL4LibraryDescriptor = unsafeCastId

instance IsNSObject (Id MTL4LibraryDescriptor) where
  toNSObject = unsafeCastId

-- ---------- MTL4MachineLearningPipelineReflection ----------

-- | Represents reflection information for a machine learning pipeline state.
-- 
-- Phantom type for @MTL4MachineLearningPipelineReflection@.
data MTL4MachineLearningPipelineReflection

instance IsObjCObject (Id MTL4MachineLearningPipelineReflection) where
  toRawId = idToRawId
  withObjCPtr = withIdObjCPtr
  staticClass _ = getRequiredClass "MTL4MachineLearningPipelineReflection"

class IsNSObject a => IsMTL4MachineLearningPipelineReflection a where
  toMTL4MachineLearningPipelineReflection :: a -> Id MTL4MachineLearningPipelineReflection

instance IsMTL4MachineLearningPipelineReflection (Id MTL4MachineLearningPipelineReflection) where
  toMTL4MachineLearningPipelineReflection = unsafeCastId

instance IsNSObject (Id MTL4MachineLearningPipelineReflection) where
  toNSObject = unsafeCastId

-- ---------- MTL4PipelineDataSetSerializerDescriptor ----------

-- | Groups together properties to create a pipeline data set serializer.
-- 
-- Phantom type for @MTL4PipelineDataSetSerializerDescriptor@.
data MTL4PipelineDataSetSerializerDescriptor

instance IsObjCObject (Id MTL4PipelineDataSetSerializerDescriptor) where
  toRawId = idToRawId
  withObjCPtr = withIdObjCPtr
  staticClass _ = getRequiredClass "MTL4PipelineDataSetSerializerDescriptor"

class IsNSObject a => IsMTL4PipelineDataSetSerializerDescriptor a where
  toMTL4PipelineDataSetSerializerDescriptor :: a -> Id MTL4PipelineDataSetSerializerDescriptor

instance IsMTL4PipelineDataSetSerializerDescriptor (Id MTL4PipelineDataSetSerializerDescriptor) where
  toMTL4PipelineDataSetSerializerDescriptor = unsafeCastId

instance IsNSObject (Id MTL4PipelineDataSetSerializerDescriptor) where
  toNSObject = unsafeCastId

-- ---------- MTL4PipelineDescriptor ----------

-- | Base type for descriptors you use for building pipeline state objects.
-- 
-- Phantom type for @MTL4PipelineDescriptor@.
data MTL4PipelineDescriptor

instance IsObjCObject (Id MTL4PipelineDescriptor) where
  toRawId = idToRawId
  withObjCPtr = withIdObjCPtr
  staticClass _ = getRequiredClass "MTL4PipelineDescriptor"

class IsNSObject a => IsMTL4PipelineDescriptor a where
  toMTL4PipelineDescriptor :: a -> Id MTL4PipelineDescriptor

instance IsMTL4PipelineDescriptor (Id MTL4PipelineDescriptor) where
  toMTL4PipelineDescriptor = unsafeCastId

instance IsNSObject (Id MTL4PipelineDescriptor) where
  toNSObject = unsafeCastId

-- ---------- MTL4PipelineOptions ----------

-- | Provides options controlling how to compile a pipeline state.
--
-- You provide these options through the ``MTL4PipelineDescriptor`` class at compilation time.
-- 
-- Phantom type for @MTL4PipelineOptions@.
data MTL4PipelineOptions

instance IsObjCObject (Id MTL4PipelineOptions) where
  toRawId = idToRawId
  withObjCPtr = withIdObjCPtr
  staticClass _ = getRequiredClass "MTL4PipelineOptions"

class IsNSObject a => IsMTL4PipelineOptions a where
  toMTL4PipelineOptions :: a -> Id MTL4PipelineOptions

instance IsMTL4PipelineOptions (Id MTL4PipelineOptions) where
  toMTL4PipelineOptions = unsafeCastId

instance IsNSObject (Id MTL4PipelineOptions) where
  toNSObject = unsafeCastId

-- ---------- MTL4PipelineStageDynamicLinkingDescriptor ----------

-- | Groups together properties to drive the dynamic linking process of a pipeline stage.
-- 
-- Phantom type for @MTL4PipelineStageDynamicLinkingDescriptor@.
data MTL4PipelineStageDynamicLinkingDescriptor

instance IsObjCObject (Id MTL4PipelineStageDynamicLinkingDescriptor) where
  toRawId = idToRawId
  withObjCPtr = withIdObjCPtr
  staticClass _ = getRequiredClass "MTL4PipelineStageDynamicLinkingDescriptor"

class IsNSObject a => IsMTL4PipelineStageDynamicLinkingDescriptor a where
  toMTL4PipelineStageDynamicLinkingDescriptor :: a -> Id MTL4PipelineStageDynamicLinkingDescriptor

instance IsMTL4PipelineStageDynamicLinkingDescriptor (Id MTL4PipelineStageDynamicLinkingDescriptor) where
  toMTL4PipelineStageDynamicLinkingDescriptor = unsafeCastId

instance IsNSObject (Id MTL4PipelineStageDynamicLinkingDescriptor) where
  toNSObject = unsafeCastId

-- ---------- MTL4RenderPassDescriptor ----------

-- | Describes a render pass.
--
-- You use render pass descriptors to create instances of ``MTL4RenderCommandEncoder`` and encode draw commands into instances of ``MTL4CommandBuffer``.
--
-- To create render command encoders, you typically call ``MTL4CommandBuffer/renderCommandEncoderWithDescriptor:``. The ``MTL4CommandBuffer/renderCommandEncoderWithDescriptor:options:`` variant of this method allows you to specify additional options to encode a render pass in parallel from multiple CPU cores by creating *suspending* and *resuming* render passes.
-- 
-- Phantom type for @MTL4RenderPassDescriptor@.
data MTL4RenderPassDescriptor

instance IsObjCObject (Id MTL4RenderPassDescriptor) where
  toRawId = idToRawId
  withObjCPtr = withIdObjCPtr
  staticClass _ = getRequiredClass "MTL4RenderPassDescriptor"

class IsNSObject a => IsMTL4RenderPassDescriptor a where
  toMTL4RenderPassDescriptor :: a -> Id MTL4RenderPassDescriptor

instance IsMTL4RenderPassDescriptor (Id MTL4RenderPassDescriptor) where
  toMTL4RenderPassDescriptor = unsafeCastId

instance IsNSObject (Id MTL4RenderPassDescriptor) where
  toNSObject = unsafeCastId

-- ---------- MTL4RenderPipelineBinaryFunctionsDescriptor ----------

-- | Allows you to specify additional binary functions to link to each stage of a render pipeline.
-- 
-- Phantom type for @MTL4RenderPipelineBinaryFunctionsDescriptor@.
data MTL4RenderPipelineBinaryFunctionsDescriptor

instance IsObjCObject (Id MTL4RenderPipelineBinaryFunctionsDescriptor) where
  toRawId = idToRawId
  withObjCPtr = withIdObjCPtr
  staticClass _ = getRequiredClass "MTL4RenderPipelineBinaryFunctionsDescriptor"

class IsNSObject a => IsMTL4RenderPipelineBinaryFunctionsDescriptor a where
  toMTL4RenderPipelineBinaryFunctionsDescriptor :: a -> Id MTL4RenderPipelineBinaryFunctionsDescriptor

instance IsMTL4RenderPipelineBinaryFunctionsDescriptor (Id MTL4RenderPipelineBinaryFunctionsDescriptor) where
  toMTL4RenderPipelineBinaryFunctionsDescriptor = unsafeCastId

instance IsNSObject (Id MTL4RenderPipelineBinaryFunctionsDescriptor) where
  toNSObject = unsafeCastId

-- ---------- MTL4RenderPipelineColorAttachmentDescriptor ----------

-- | Phantom type for @MTL4RenderPipelineColorAttachmentDescriptor@.
data MTL4RenderPipelineColorAttachmentDescriptor

instance IsObjCObject (Id MTL4RenderPipelineColorAttachmentDescriptor) where
  toRawId = idToRawId
  withObjCPtr = withIdObjCPtr
  staticClass _ = getRequiredClass "MTL4RenderPipelineColorAttachmentDescriptor"

class IsNSObject a => IsMTL4RenderPipelineColorAttachmentDescriptor a where
  toMTL4RenderPipelineColorAttachmentDescriptor :: a -> Id MTL4RenderPipelineColorAttachmentDescriptor

instance IsMTL4RenderPipelineColorAttachmentDescriptor (Id MTL4RenderPipelineColorAttachmentDescriptor) where
  toMTL4RenderPipelineColorAttachmentDescriptor = unsafeCastId

instance IsNSObject (Id MTL4RenderPipelineColorAttachmentDescriptor) where
  toNSObject = unsafeCastId

-- ---------- MTL4RenderPipelineColorAttachmentDescriptorArray ----------

-- | An array of color attachment descriptions for a render pipeline.
-- 
-- Phantom type for @MTL4RenderPipelineColorAttachmentDescriptorArray@.
data MTL4RenderPipelineColorAttachmentDescriptorArray

instance IsObjCObject (Id MTL4RenderPipelineColorAttachmentDescriptorArray) where
  toRawId = idToRawId
  withObjCPtr = withIdObjCPtr
  staticClass _ = getRequiredClass "MTL4RenderPipelineColorAttachmentDescriptorArray"

class IsNSObject a => IsMTL4RenderPipelineColorAttachmentDescriptorArray a where
  toMTL4RenderPipelineColorAttachmentDescriptorArray :: a -> Id MTL4RenderPipelineColorAttachmentDescriptorArray

instance IsMTL4RenderPipelineColorAttachmentDescriptorArray (Id MTL4RenderPipelineColorAttachmentDescriptorArray) where
  toMTL4RenderPipelineColorAttachmentDescriptorArray = unsafeCastId

instance IsNSObject (Id MTL4RenderPipelineColorAttachmentDescriptorArray) where
  toNSObject = unsafeCastId

-- ---------- MTL4RenderPipelineDynamicLinkingDescriptor ----------

-- | Groups together properties that provide linking properties for render pipelines.
-- 
-- Phantom type for @MTL4RenderPipelineDynamicLinkingDescriptor@.
data MTL4RenderPipelineDynamicLinkingDescriptor

instance IsObjCObject (Id MTL4RenderPipelineDynamicLinkingDescriptor) where
  toRawId = idToRawId
  withObjCPtr = withIdObjCPtr
  staticClass _ = getRequiredClass "MTL4RenderPipelineDynamicLinkingDescriptor"

class IsNSObject a => IsMTL4RenderPipelineDynamicLinkingDescriptor a where
  toMTL4RenderPipelineDynamicLinkingDescriptor :: a -> Id MTL4RenderPipelineDynamicLinkingDescriptor

instance IsMTL4RenderPipelineDynamicLinkingDescriptor (Id MTL4RenderPipelineDynamicLinkingDescriptor) where
  toMTL4RenderPipelineDynamicLinkingDescriptor = unsafeCastId

instance IsNSObject (Id MTL4RenderPipelineDynamicLinkingDescriptor) where
  toNSObject = unsafeCastId

-- ---------- MTL4StaticLinkingDescriptor ----------

-- | Groups together properties to drive a static linking process.
-- 
-- Phantom type for @MTL4StaticLinkingDescriptor@.
data MTL4StaticLinkingDescriptor

instance IsObjCObject (Id MTL4StaticLinkingDescriptor) where
  toRawId = idToRawId
  withObjCPtr = withIdObjCPtr
  staticClass _ = getRequiredClass "MTL4StaticLinkingDescriptor"

class IsNSObject a => IsMTL4StaticLinkingDescriptor a where
  toMTL4StaticLinkingDescriptor :: a -> Id MTL4StaticLinkingDescriptor

instance IsMTL4StaticLinkingDescriptor (Id MTL4StaticLinkingDescriptor) where
  toMTL4StaticLinkingDescriptor = unsafeCastId

instance IsNSObject (Id MTL4StaticLinkingDescriptor) where
  toNSObject = unsafeCastId

-- ---------- MTLAccelerationStructureDescriptor ----------

-- | Base class for acceleration structure descriptors. Do not use this class directly. Use one of the derived classes instead.
-- 
-- Phantom type for @MTLAccelerationStructureDescriptor@.
data MTLAccelerationStructureDescriptor

instance IsObjCObject (Id MTLAccelerationStructureDescriptor) where
  toRawId = idToRawId
  withObjCPtr = withIdObjCPtr
  staticClass _ = getRequiredClass "MTLAccelerationStructureDescriptor"

class IsNSObject a => IsMTLAccelerationStructureDescriptor a where
  toMTLAccelerationStructureDescriptor :: a -> Id MTLAccelerationStructureDescriptor

instance IsMTLAccelerationStructureDescriptor (Id MTLAccelerationStructureDescriptor) where
  toMTLAccelerationStructureDescriptor = unsafeCastId

instance IsNSObject (Id MTLAccelerationStructureDescriptor) where
  toNSObject = unsafeCastId

-- ---------- MTLAccelerationStructureGeometryDescriptor ----------

-- | Base class for all geometry descriptors. Do not use this class directly. Use one of the derived classes instead.
-- 
-- Phantom type for @MTLAccelerationStructureGeometryDescriptor@.
data MTLAccelerationStructureGeometryDescriptor

instance IsObjCObject (Id MTLAccelerationStructureGeometryDescriptor) where
  toRawId = idToRawId
  withObjCPtr = withIdObjCPtr
  staticClass _ = getRequiredClass "MTLAccelerationStructureGeometryDescriptor"

class IsNSObject a => IsMTLAccelerationStructureGeometryDescriptor a where
  toMTLAccelerationStructureGeometryDescriptor :: a -> Id MTLAccelerationStructureGeometryDescriptor

instance IsMTLAccelerationStructureGeometryDescriptor (Id MTLAccelerationStructureGeometryDescriptor) where
  toMTLAccelerationStructureGeometryDescriptor = unsafeCastId

instance IsNSObject (Id MTLAccelerationStructureGeometryDescriptor) where
  toNSObject = unsafeCastId

-- ---------- MTLAccelerationStructurePassDescriptor ----------

-- | MTLAccelerationStructurePassDescriptor
--
-- MTLAccelerationStructurePassDescriptor represents a collection of attachments to be used to create a concrete acceleration structure encoder.
-- 
-- Phantom type for @MTLAccelerationStructurePassDescriptor@.
data MTLAccelerationStructurePassDescriptor

instance IsObjCObject (Id MTLAccelerationStructurePassDescriptor) where
  toRawId = idToRawId
  withObjCPtr = withIdObjCPtr
  staticClass _ = getRequiredClass "MTLAccelerationStructurePassDescriptor"

class IsNSObject a => IsMTLAccelerationStructurePassDescriptor a where
  toMTLAccelerationStructurePassDescriptor :: a -> Id MTLAccelerationStructurePassDescriptor

instance IsMTLAccelerationStructurePassDescriptor (Id MTLAccelerationStructurePassDescriptor) where
  toMTLAccelerationStructurePassDescriptor = unsafeCastId

instance IsNSObject (Id MTLAccelerationStructurePassDescriptor) where
  toNSObject = unsafeCastId

-- ---------- MTLAccelerationStructurePassSampleBufferAttachmentDescriptor ----------

-- | Phantom type for @MTLAccelerationStructurePassSampleBufferAttachmentDescriptor@.
data MTLAccelerationStructurePassSampleBufferAttachmentDescriptor

instance IsObjCObject (Id MTLAccelerationStructurePassSampleBufferAttachmentDescriptor) where
  toRawId = idToRawId
  withObjCPtr = withIdObjCPtr
  staticClass _ = getRequiredClass "MTLAccelerationStructurePassSampleBufferAttachmentDescriptor"

class IsNSObject a => IsMTLAccelerationStructurePassSampleBufferAttachmentDescriptor a where
  toMTLAccelerationStructurePassSampleBufferAttachmentDescriptor :: a -> Id MTLAccelerationStructurePassSampleBufferAttachmentDescriptor

instance IsMTLAccelerationStructurePassSampleBufferAttachmentDescriptor (Id MTLAccelerationStructurePassSampleBufferAttachmentDescriptor) where
  toMTLAccelerationStructurePassSampleBufferAttachmentDescriptor = unsafeCastId

instance IsNSObject (Id MTLAccelerationStructurePassSampleBufferAttachmentDescriptor) where
  toNSObject = unsafeCastId

-- ---------- MTLAccelerationStructurePassSampleBufferAttachmentDescriptorArray ----------

-- | Phantom type for @MTLAccelerationStructurePassSampleBufferAttachmentDescriptorArray@.
data MTLAccelerationStructurePassSampleBufferAttachmentDescriptorArray

instance IsObjCObject (Id MTLAccelerationStructurePassSampleBufferAttachmentDescriptorArray) where
  toRawId = idToRawId
  withObjCPtr = withIdObjCPtr
  staticClass _ = getRequiredClass "MTLAccelerationStructurePassSampleBufferAttachmentDescriptorArray"

class IsNSObject a => IsMTLAccelerationStructurePassSampleBufferAttachmentDescriptorArray a where
  toMTLAccelerationStructurePassSampleBufferAttachmentDescriptorArray :: a -> Id MTLAccelerationStructurePassSampleBufferAttachmentDescriptorArray

instance IsMTLAccelerationStructurePassSampleBufferAttachmentDescriptorArray (Id MTLAccelerationStructurePassSampleBufferAttachmentDescriptorArray) where
  toMTLAccelerationStructurePassSampleBufferAttachmentDescriptorArray = unsafeCastId

instance IsNSObject (Id MTLAccelerationStructurePassSampleBufferAttachmentDescriptorArray) where
  toNSObject = unsafeCastId

-- ---------- MTLArchitecture ----------

-- | MTLArchitecture
--
-- Contains information about the device's architecture
-- 
-- Phantom type for @MTLArchitecture@.
data MTLArchitecture

instance IsObjCObject (Id MTLArchitecture) where
  toRawId = idToRawId
  withObjCPtr = withIdObjCPtr
  staticClass _ = getRequiredClass "MTLArchitecture"

class IsNSObject a => IsMTLArchitecture a where
  toMTLArchitecture :: a -> Id MTLArchitecture

instance IsMTLArchitecture (Id MTLArchitecture) where
  toMTLArchitecture = unsafeCastId

instance IsNSObject (Id MTLArchitecture) where
  toNSObject = unsafeCastId

-- ---------- MTLArgument ----------

-- | MTLArgument
-- 
-- Phantom type for @MTLArgument@.
data MTLArgument

instance IsObjCObject (Id MTLArgument) where
  toRawId = idToRawId
  withObjCPtr = withIdObjCPtr
  staticClass _ = getRequiredClass "MTLArgument"

class IsNSObject a => IsMTLArgument a where
  toMTLArgument :: a -> Id MTLArgument

instance IsMTLArgument (Id MTLArgument) where
  toMTLArgument = unsafeCastId

instance IsNSObject (Id MTLArgument) where
  toNSObject = unsafeCastId

-- ---------- MTLArgumentDescriptor ----------

-- | MTLArgumentDescriptor
--
-- Represents a member of an argument buffer
-- 
-- Phantom type for @MTLArgumentDescriptor@.
data MTLArgumentDescriptor

instance IsObjCObject (Id MTLArgumentDescriptor) where
  toRawId = idToRawId
  withObjCPtr = withIdObjCPtr
  staticClass _ = getRequiredClass "MTLArgumentDescriptor"

class IsNSObject a => IsMTLArgumentDescriptor a where
  toMTLArgumentDescriptor :: a -> Id MTLArgumentDescriptor

instance IsMTLArgumentDescriptor (Id MTLArgumentDescriptor) where
  toMTLArgumentDescriptor = unsafeCastId

instance IsNSObject (Id MTLArgumentDescriptor) where
  toNSObject = unsafeCastId

-- ---------- MTLAttribute ----------

-- | Phantom type for @MTLAttribute@.
data MTLAttribute

instance IsObjCObject (Id MTLAttribute) where
  toRawId = idToRawId
  withObjCPtr = withIdObjCPtr
  staticClass _ = getRequiredClass "MTLAttribute"

class IsNSObject a => IsMTLAttribute a where
  toMTLAttribute :: a -> Id MTLAttribute

instance IsMTLAttribute (Id MTLAttribute) where
  toMTLAttribute = unsafeCastId

instance IsNSObject (Id MTLAttribute) where
  toNSObject = unsafeCastId

-- ---------- MTLAttributeDescriptor ----------

-- | Phantom type for @MTLAttributeDescriptor@.
data MTLAttributeDescriptor

instance IsObjCObject (Id MTLAttributeDescriptor) where
  toRawId = idToRawId
  withObjCPtr = withIdObjCPtr
  staticClass _ = getRequiredClass "MTLAttributeDescriptor"

class IsNSObject a => IsMTLAttributeDescriptor a where
  toMTLAttributeDescriptor :: a -> Id MTLAttributeDescriptor

instance IsMTLAttributeDescriptor (Id MTLAttributeDescriptor) where
  toMTLAttributeDescriptor = unsafeCastId

instance IsNSObject (Id MTLAttributeDescriptor) where
  toNSObject = unsafeCastId

-- ---------- MTLAttributeDescriptorArray ----------

-- | Phantom type for @MTLAttributeDescriptorArray@.
data MTLAttributeDescriptorArray

instance IsObjCObject (Id MTLAttributeDescriptorArray) where
  toRawId = idToRawId
  withObjCPtr = withIdObjCPtr
  staticClass _ = getRequiredClass "MTLAttributeDescriptorArray"

class IsNSObject a => IsMTLAttributeDescriptorArray a where
  toMTLAttributeDescriptorArray :: a -> Id MTLAttributeDescriptorArray

instance IsMTLAttributeDescriptorArray (Id MTLAttributeDescriptorArray) where
  toMTLAttributeDescriptorArray = unsafeCastId

instance IsNSObject (Id MTLAttributeDescriptorArray) where
  toNSObject = unsafeCastId

-- ---------- MTLBinaryArchiveDescriptor ----------

-- | MTLBinaryArchiveDescriptor
--
-- A class used to indicate how an archive should be created
-- 
-- Phantom type for @MTLBinaryArchiveDescriptor@.
data MTLBinaryArchiveDescriptor

instance IsObjCObject (Id MTLBinaryArchiveDescriptor) where
  toRawId = idToRawId
  withObjCPtr = withIdObjCPtr
  staticClass _ = getRequiredClass "MTLBinaryArchiveDescriptor"

class IsNSObject a => IsMTLBinaryArchiveDescriptor a where
  toMTLBinaryArchiveDescriptor :: a -> Id MTLBinaryArchiveDescriptor

instance IsMTLBinaryArchiveDescriptor (Id MTLBinaryArchiveDescriptor) where
  toMTLBinaryArchiveDescriptor = unsafeCastId

instance IsNSObject (Id MTLBinaryArchiveDescriptor) where
  toNSObject = unsafeCastId

-- ---------- MTLBlitPassDescriptor ----------

-- | MTLBlitPassDescriptor
--
-- MTLBlitPassDescriptor represents a collection of attachments to be used to create a concrete blit command encoder
-- 
-- Phantom type for @MTLBlitPassDescriptor@.
data MTLBlitPassDescriptor

instance IsObjCObject (Id MTLBlitPassDescriptor) where
  toRawId = idToRawId
  withObjCPtr = withIdObjCPtr
  staticClass _ = getRequiredClass "MTLBlitPassDescriptor"

class IsNSObject a => IsMTLBlitPassDescriptor a where
  toMTLBlitPassDescriptor :: a -> Id MTLBlitPassDescriptor

instance IsMTLBlitPassDescriptor (Id MTLBlitPassDescriptor) where
  toMTLBlitPassDescriptor = unsafeCastId

instance IsNSObject (Id MTLBlitPassDescriptor) where
  toNSObject = unsafeCastId

-- ---------- MTLBlitPassSampleBufferAttachmentDescriptor ----------

-- | Phantom type for @MTLBlitPassSampleBufferAttachmentDescriptor@.
data MTLBlitPassSampleBufferAttachmentDescriptor

instance IsObjCObject (Id MTLBlitPassSampleBufferAttachmentDescriptor) where
  toRawId = idToRawId
  withObjCPtr = withIdObjCPtr
  staticClass _ = getRequiredClass "MTLBlitPassSampleBufferAttachmentDescriptor"

class IsNSObject a => IsMTLBlitPassSampleBufferAttachmentDescriptor a where
  toMTLBlitPassSampleBufferAttachmentDescriptor :: a -> Id MTLBlitPassSampleBufferAttachmentDescriptor

instance IsMTLBlitPassSampleBufferAttachmentDescriptor (Id MTLBlitPassSampleBufferAttachmentDescriptor) where
  toMTLBlitPassSampleBufferAttachmentDescriptor = unsafeCastId

instance IsNSObject (Id MTLBlitPassSampleBufferAttachmentDescriptor) where
  toNSObject = unsafeCastId

-- ---------- MTLBlitPassSampleBufferAttachmentDescriptorArray ----------

-- | Phantom type for @MTLBlitPassSampleBufferAttachmentDescriptorArray@.
data MTLBlitPassSampleBufferAttachmentDescriptorArray

instance IsObjCObject (Id MTLBlitPassSampleBufferAttachmentDescriptorArray) where
  toRawId = idToRawId
  withObjCPtr = withIdObjCPtr
  staticClass _ = getRequiredClass "MTLBlitPassSampleBufferAttachmentDescriptorArray"

class IsNSObject a => IsMTLBlitPassSampleBufferAttachmentDescriptorArray a where
  toMTLBlitPassSampleBufferAttachmentDescriptorArray :: a -> Id MTLBlitPassSampleBufferAttachmentDescriptorArray

instance IsMTLBlitPassSampleBufferAttachmentDescriptorArray (Id MTLBlitPassSampleBufferAttachmentDescriptorArray) where
  toMTLBlitPassSampleBufferAttachmentDescriptorArray = unsafeCastId

instance IsNSObject (Id MTLBlitPassSampleBufferAttachmentDescriptorArray) where
  toNSObject = unsafeCastId

-- ---------- MTLBufferLayoutDescriptor ----------

-- | Phantom type for @MTLBufferLayoutDescriptor@.
data MTLBufferLayoutDescriptor

instance IsObjCObject (Id MTLBufferLayoutDescriptor) where
  toRawId = idToRawId
  withObjCPtr = withIdObjCPtr
  staticClass _ = getRequiredClass "MTLBufferLayoutDescriptor"

class IsNSObject a => IsMTLBufferLayoutDescriptor a where
  toMTLBufferLayoutDescriptor :: a -> Id MTLBufferLayoutDescriptor

instance IsMTLBufferLayoutDescriptor (Id MTLBufferLayoutDescriptor) where
  toMTLBufferLayoutDescriptor = unsafeCastId

instance IsNSObject (Id MTLBufferLayoutDescriptor) where
  toNSObject = unsafeCastId

-- ---------- MTLBufferLayoutDescriptorArray ----------

-- | Phantom type for @MTLBufferLayoutDescriptorArray@.
data MTLBufferLayoutDescriptorArray

instance IsObjCObject (Id MTLBufferLayoutDescriptorArray) where
  toRawId = idToRawId
  withObjCPtr = withIdObjCPtr
  staticClass _ = getRequiredClass "MTLBufferLayoutDescriptorArray"

class IsNSObject a => IsMTLBufferLayoutDescriptorArray a where
  toMTLBufferLayoutDescriptorArray :: a -> Id MTLBufferLayoutDescriptorArray

instance IsMTLBufferLayoutDescriptorArray (Id MTLBufferLayoutDescriptorArray) where
  toMTLBufferLayoutDescriptorArray = unsafeCastId

instance IsNSObject (Id MTLBufferLayoutDescriptorArray) where
  toNSObject = unsafeCastId

-- ---------- MTLCaptureDescriptor ----------

-- | Phantom type for @MTLCaptureDescriptor@.
data MTLCaptureDescriptor

instance IsObjCObject (Id MTLCaptureDescriptor) where
  toRawId = idToRawId
  withObjCPtr = withIdObjCPtr
  staticClass _ = getRequiredClass "MTLCaptureDescriptor"

class IsNSObject a => IsMTLCaptureDescriptor a where
  toMTLCaptureDescriptor :: a -> Id MTLCaptureDescriptor

instance IsMTLCaptureDescriptor (Id MTLCaptureDescriptor) where
  toMTLCaptureDescriptor = unsafeCastId

instance IsNSObject (Id MTLCaptureDescriptor) where
  toNSObject = unsafeCastId

-- ---------- MTLCaptureManager ----------

-- | Phantom type for @MTLCaptureManager@.
data MTLCaptureManager

instance IsObjCObject (Id MTLCaptureManager) where
  toRawId = idToRawId
  withObjCPtr = withIdObjCPtr
  staticClass _ = getRequiredClass "MTLCaptureManager"

class IsNSObject a => IsMTLCaptureManager a where
  toMTLCaptureManager :: a -> Id MTLCaptureManager

instance IsMTLCaptureManager (Id MTLCaptureManager) where
  toMTLCaptureManager = unsafeCastId

instance IsNSObject (Id MTLCaptureManager) where
  toNSObject = unsafeCastId

-- ---------- MTLCommandBufferDescriptor ----------

-- | MTLCommandBufferDescriptor
--
-- An object that you use to configure new Metal command buffer objects.
-- 
-- Phantom type for @MTLCommandBufferDescriptor@.
data MTLCommandBufferDescriptor

instance IsObjCObject (Id MTLCommandBufferDescriptor) where
  toRawId = idToRawId
  withObjCPtr = withIdObjCPtr
  staticClass _ = getRequiredClass "MTLCommandBufferDescriptor"

class IsNSObject a => IsMTLCommandBufferDescriptor a where
  toMTLCommandBufferDescriptor :: a -> Id MTLCommandBufferDescriptor

instance IsMTLCommandBufferDescriptor (Id MTLCommandBufferDescriptor) where
  toMTLCommandBufferDescriptor = unsafeCastId

instance IsNSObject (Id MTLCommandBufferDescriptor) where
  toNSObject = unsafeCastId

-- ---------- MTLCommandQueueDescriptor ----------

-- | Phantom type for @MTLCommandQueueDescriptor@.
data MTLCommandQueueDescriptor

instance IsObjCObject (Id MTLCommandQueueDescriptor) where
  toRawId = idToRawId
  withObjCPtr = withIdObjCPtr
  staticClass _ = getRequiredClass "MTLCommandQueueDescriptor"

class IsNSObject a => IsMTLCommandQueueDescriptor a where
  toMTLCommandQueueDescriptor :: a -> Id MTLCommandQueueDescriptor

instance IsMTLCommandQueueDescriptor (Id MTLCommandQueueDescriptor) where
  toMTLCommandQueueDescriptor = unsafeCastId

instance IsNSObject (Id MTLCommandQueueDescriptor) where
  toNSObject = unsafeCastId

-- ---------- MTLCompileOptions ----------

-- | Phantom type for @MTLCompileOptions@.
data MTLCompileOptions

instance IsObjCObject (Id MTLCompileOptions) where
  toRawId = idToRawId
  withObjCPtr = withIdObjCPtr
  staticClass _ = getRequiredClass "MTLCompileOptions"

class IsNSObject a => IsMTLCompileOptions a where
  toMTLCompileOptions :: a -> Id MTLCompileOptions

instance IsMTLCompileOptions (Id MTLCompileOptions) where
  toMTLCompileOptions = unsafeCastId

instance IsNSObject (Id MTLCompileOptions) where
  toNSObject = unsafeCastId

-- ---------- MTLComputePassDescriptor ----------

-- | MTLComputePassDescriptor
--
-- MTLComputePassDescriptor represents a collection of attachments to be used to create a concrete compute command encoder
-- 
-- Phantom type for @MTLComputePassDescriptor@.
data MTLComputePassDescriptor

instance IsObjCObject (Id MTLComputePassDescriptor) where
  toRawId = idToRawId
  withObjCPtr = withIdObjCPtr
  staticClass _ = getRequiredClass "MTLComputePassDescriptor"

class IsNSObject a => IsMTLComputePassDescriptor a where
  toMTLComputePassDescriptor :: a -> Id MTLComputePassDescriptor

instance IsMTLComputePassDescriptor (Id MTLComputePassDescriptor) where
  toMTLComputePassDescriptor = unsafeCastId

instance IsNSObject (Id MTLComputePassDescriptor) where
  toNSObject = unsafeCastId

-- ---------- MTLComputePassSampleBufferAttachmentDescriptor ----------

-- | Phantom type for @MTLComputePassSampleBufferAttachmentDescriptor@.
data MTLComputePassSampleBufferAttachmentDescriptor

instance IsObjCObject (Id MTLComputePassSampleBufferAttachmentDescriptor) where
  toRawId = idToRawId
  withObjCPtr = withIdObjCPtr
  staticClass _ = getRequiredClass "MTLComputePassSampleBufferAttachmentDescriptor"

class IsNSObject a => IsMTLComputePassSampleBufferAttachmentDescriptor a where
  toMTLComputePassSampleBufferAttachmentDescriptor :: a -> Id MTLComputePassSampleBufferAttachmentDescriptor

instance IsMTLComputePassSampleBufferAttachmentDescriptor (Id MTLComputePassSampleBufferAttachmentDescriptor) where
  toMTLComputePassSampleBufferAttachmentDescriptor = unsafeCastId

instance IsNSObject (Id MTLComputePassSampleBufferAttachmentDescriptor) where
  toNSObject = unsafeCastId

-- ---------- MTLComputePassSampleBufferAttachmentDescriptorArray ----------

-- | Phantom type for @MTLComputePassSampleBufferAttachmentDescriptorArray@.
data MTLComputePassSampleBufferAttachmentDescriptorArray

instance IsObjCObject (Id MTLComputePassSampleBufferAttachmentDescriptorArray) where
  toRawId = idToRawId
  withObjCPtr = withIdObjCPtr
  staticClass _ = getRequiredClass "MTLComputePassSampleBufferAttachmentDescriptorArray"

class IsNSObject a => IsMTLComputePassSampleBufferAttachmentDescriptorArray a where
  toMTLComputePassSampleBufferAttachmentDescriptorArray :: a -> Id MTLComputePassSampleBufferAttachmentDescriptorArray

instance IsMTLComputePassSampleBufferAttachmentDescriptorArray (Id MTLComputePassSampleBufferAttachmentDescriptorArray) where
  toMTLComputePassSampleBufferAttachmentDescriptorArray = unsafeCastId

instance IsNSObject (Id MTLComputePassSampleBufferAttachmentDescriptorArray) where
  toNSObject = unsafeCastId

-- ---------- MTLComputePipelineDescriptor ----------

-- | Phantom type for @MTLComputePipelineDescriptor@.
data MTLComputePipelineDescriptor

instance IsObjCObject (Id MTLComputePipelineDescriptor) where
  toRawId = idToRawId
  withObjCPtr = withIdObjCPtr
  staticClass _ = getRequiredClass "MTLComputePipelineDescriptor"

class IsNSObject a => IsMTLComputePipelineDescriptor a where
  toMTLComputePipelineDescriptor :: a -> Id MTLComputePipelineDescriptor

instance IsMTLComputePipelineDescriptor (Id MTLComputePipelineDescriptor) where
  toMTLComputePipelineDescriptor = unsafeCastId

instance IsNSObject (Id MTLComputePipelineDescriptor) where
  toNSObject = unsafeCastId

-- ---------- MTLComputePipelineReflection ----------

-- | Phantom type for @MTLComputePipelineReflection@.
data MTLComputePipelineReflection

instance IsObjCObject (Id MTLComputePipelineReflection) where
  toRawId = idToRawId
  withObjCPtr = withIdObjCPtr
  staticClass _ = getRequiredClass "MTLComputePipelineReflection"

class IsNSObject a => IsMTLComputePipelineReflection a where
  toMTLComputePipelineReflection :: a -> Id MTLComputePipelineReflection

instance IsMTLComputePipelineReflection (Id MTLComputePipelineReflection) where
  toMTLComputePipelineReflection = unsafeCastId

instance IsNSObject (Id MTLComputePipelineReflection) where
  toNSObject = unsafeCastId

-- ---------- MTLCounterSampleBufferDescriptor ----------

-- | MTLCounterSampleBufferDescriptor
--
-- Object to represent the counter state.
-- 
-- Phantom type for @MTLCounterSampleBufferDescriptor@.
data MTLCounterSampleBufferDescriptor

instance IsObjCObject (Id MTLCounterSampleBufferDescriptor) where
  toRawId = idToRawId
  withObjCPtr = withIdObjCPtr
  staticClass _ = getRequiredClass "MTLCounterSampleBufferDescriptor"

class IsNSObject a => IsMTLCounterSampleBufferDescriptor a where
  toMTLCounterSampleBufferDescriptor :: a -> Id MTLCounterSampleBufferDescriptor

instance IsMTLCounterSampleBufferDescriptor (Id MTLCounterSampleBufferDescriptor) where
  toMTLCounterSampleBufferDescriptor = unsafeCastId

instance IsNSObject (Id MTLCounterSampleBufferDescriptor) where
  toNSObject = unsafeCastId

-- ---------- MTLDepthStencilDescriptor ----------

-- | Phantom type for @MTLDepthStencilDescriptor@.
data MTLDepthStencilDescriptor

instance IsObjCObject (Id MTLDepthStencilDescriptor) where
  toRawId = idToRawId
  withObjCPtr = withIdObjCPtr
  staticClass _ = getRequiredClass "MTLDepthStencilDescriptor"

class IsNSObject a => IsMTLDepthStencilDescriptor a where
  toMTLDepthStencilDescriptor :: a -> Id MTLDepthStencilDescriptor

instance IsMTLDepthStencilDescriptor (Id MTLDepthStencilDescriptor) where
  toMTLDepthStencilDescriptor = unsafeCastId

instance IsNSObject (Id MTLDepthStencilDescriptor) where
  toNSObject = unsafeCastId

-- ---------- MTLFunctionConstant ----------

-- | MTLFunctionConstant
--
-- describe an uberShader constant used by the function
-- 
-- Phantom type for @MTLFunctionConstant@.
data MTLFunctionConstant

instance IsObjCObject (Id MTLFunctionConstant) where
  toRawId = idToRawId
  withObjCPtr = withIdObjCPtr
  staticClass _ = getRequiredClass "MTLFunctionConstant"

class IsNSObject a => IsMTLFunctionConstant a where
  toMTLFunctionConstant :: a -> Id MTLFunctionConstant

instance IsMTLFunctionConstant (Id MTLFunctionConstant) where
  toMTLFunctionConstant = unsafeCastId

instance IsNSObject (Id MTLFunctionConstant) where
  toNSObject = unsafeCastId

-- ---------- MTLFunctionConstantValues ----------

-- | Phantom type for @MTLFunctionConstantValues@.
data MTLFunctionConstantValues

instance IsObjCObject (Id MTLFunctionConstantValues) where
  toRawId = idToRawId
  withObjCPtr = withIdObjCPtr
  staticClass _ = getRequiredClass "MTLFunctionConstantValues"

class IsNSObject a => IsMTLFunctionConstantValues a where
  toMTLFunctionConstantValues :: a -> Id MTLFunctionConstantValues

instance IsMTLFunctionConstantValues (Id MTLFunctionConstantValues) where
  toMTLFunctionConstantValues = unsafeCastId

instance IsNSObject (Id MTLFunctionConstantValues) where
  toNSObject = unsafeCastId

-- ---------- MTLFunctionDescriptor ----------

-- | Phantom type for @MTLFunctionDescriptor@.
data MTLFunctionDescriptor

instance IsObjCObject (Id MTLFunctionDescriptor) where
  toRawId = idToRawId
  withObjCPtr = withIdObjCPtr
  staticClass _ = getRequiredClass "MTLFunctionDescriptor"

class IsNSObject a => IsMTLFunctionDescriptor a where
  toMTLFunctionDescriptor :: a -> Id MTLFunctionDescriptor

instance IsMTLFunctionDescriptor (Id MTLFunctionDescriptor) where
  toMTLFunctionDescriptor = unsafeCastId

instance IsNSObject (Id MTLFunctionDescriptor) where
  toNSObject = unsafeCastId

-- ---------- MTLFunctionReflection ----------

-- | Represents a reflection object containing information about a function in a Metal library.
-- 
-- Phantom type for @MTLFunctionReflection@.
data MTLFunctionReflection

instance IsObjCObject (Id MTLFunctionReflection) where
  toRawId = idToRawId
  withObjCPtr = withIdObjCPtr
  staticClass _ = getRequiredClass "MTLFunctionReflection"

class IsNSObject a => IsMTLFunctionReflection a where
  toMTLFunctionReflection :: a -> Id MTLFunctionReflection

instance IsMTLFunctionReflection (Id MTLFunctionReflection) where
  toMTLFunctionReflection = unsafeCastId

instance IsNSObject (Id MTLFunctionReflection) where
  toNSObject = unsafeCastId

-- ---------- MTLFunctionStitchingAttributeAlwaysInline ----------

-- | MTLFunctionStitchingAttributeAlwaysInline
--
-- Applies the @__attribute__((always_inline))@ attribute to the produced stitched function.
-- 
-- Phantom type for @MTLFunctionStitchingAttributeAlwaysInline@.
data MTLFunctionStitchingAttributeAlwaysInline

instance IsObjCObject (Id MTLFunctionStitchingAttributeAlwaysInline) where
  toRawId = idToRawId
  withObjCPtr = withIdObjCPtr
  staticClass _ = getRequiredClass "MTLFunctionStitchingAttributeAlwaysInline"

class IsNSObject a => IsMTLFunctionStitchingAttributeAlwaysInline a where
  toMTLFunctionStitchingAttributeAlwaysInline :: a -> Id MTLFunctionStitchingAttributeAlwaysInline

instance IsMTLFunctionStitchingAttributeAlwaysInline (Id MTLFunctionStitchingAttributeAlwaysInline) where
  toMTLFunctionStitchingAttributeAlwaysInline = unsafeCastId

instance IsNSObject (Id MTLFunctionStitchingAttributeAlwaysInline) where
  toNSObject = unsafeCastId

-- ---------- MTLFunctionStitchingFunctionNode ----------

-- | MTLFunctionStitchingFunctionNode
--
-- A function node that calls the specified function with arguments and ordering determined by data and control dependencies.
-- 
-- Phantom type for @MTLFunctionStitchingFunctionNode@.
data MTLFunctionStitchingFunctionNode

instance IsObjCObject (Id MTLFunctionStitchingFunctionNode) where
  toRawId = idToRawId
  withObjCPtr = withIdObjCPtr
  staticClass _ = getRequiredClass "MTLFunctionStitchingFunctionNode"

class IsNSObject a => IsMTLFunctionStitchingFunctionNode a where
  toMTLFunctionStitchingFunctionNode :: a -> Id MTLFunctionStitchingFunctionNode

instance IsMTLFunctionStitchingFunctionNode (Id MTLFunctionStitchingFunctionNode) where
  toMTLFunctionStitchingFunctionNode = unsafeCastId

instance IsNSObject (Id MTLFunctionStitchingFunctionNode) where
  toNSObject = unsafeCastId

-- ---------- MTLFunctionStitchingGraph ----------

-- | MTLFunctionStitchingGraph
--
-- A function graph that describes a directed acyclic graph.
--
-- The return value of the output node will be used as the return value for the final stitched graph.
-- 
-- Phantom type for @MTLFunctionStitchingGraph@.
data MTLFunctionStitchingGraph

instance IsObjCObject (Id MTLFunctionStitchingGraph) where
  toRawId = idToRawId
  withObjCPtr = withIdObjCPtr
  staticClass _ = getRequiredClass "MTLFunctionStitchingGraph"

class IsNSObject a => IsMTLFunctionStitchingGraph a where
  toMTLFunctionStitchingGraph :: a -> Id MTLFunctionStitchingGraph

instance IsMTLFunctionStitchingGraph (Id MTLFunctionStitchingGraph) where
  toMTLFunctionStitchingGraph = unsafeCastId

instance IsNSObject (Id MTLFunctionStitchingGraph) where
  toNSObject = unsafeCastId

-- ---------- MTLFunctionStitchingInputNode ----------

-- | MTLFunctionStitchingInputNode
--
-- An indexed input node of the produced stitched function.
-- 
-- Phantom type for @MTLFunctionStitchingInputNode@.
data MTLFunctionStitchingInputNode

instance IsObjCObject (Id MTLFunctionStitchingInputNode) where
  toRawId = idToRawId
  withObjCPtr = withIdObjCPtr
  staticClass _ = getRequiredClass "MTLFunctionStitchingInputNode"

class IsNSObject a => IsMTLFunctionStitchingInputNode a where
  toMTLFunctionStitchingInputNode :: a -> Id MTLFunctionStitchingInputNode

instance IsMTLFunctionStitchingInputNode (Id MTLFunctionStitchingInputNode) where
  toMTLFunctionStitchingInputNode = unsafeCastId

instance IsNSObject (Id MTLFunctionStitchingInputNode) where
  toNSObject = unsafeCastId

-- ---------- MTLHeapDescriptor ----------

-- | MTLHeapDescriptor
-- 
-- Phantom type for @MTLHeapDescriptor@.
data MTLHeapDescriptor

instance IsObjCObject (Id MTLHeapDescriptor) where
  toRawId = idToRawId
  withObjCPtr = withIdObjCPtr
  staticClass _ = getRequiredClass "MTLHeapDescriptor"

class IsNSObject a => IsMTLHeapDescriptor a where
  toMTLHeapDescriptor :: a -> Id MTLHeapDescriptor

instance IsMTLHeapDescriptor (Id MTLHeapDescriptor) where
  toMTLHeapDescriptor = unsafeCastId

instance IsNSObject (Id MTLHeapDescriptor) where
  toNSObject = unsafeCastId

-- ---------- MTLIOCommandQueueDescriptor ----------

-- | MTLIOCommandQueueDescriptor
--
-- Represents a descriptor to create a MTLIOCommandQueue.
-- 
-- Phantom type for @MTLIOCommandQueueDescriptor@.
data MTLIOCommandQueueDescriptor

instance IsObjCObject (Id MTLIOCommandQueueDescriptor) where
  toRawId = idToRawId
  withObjCPtr = withIdObjCPtr
  staticClass _ = getRequiredClass "MTLIOCommandQueueDescriptor"

class IsNSObject a => IsMTLIOCommandQueueDescriptor a where
  toMTLIOCommandQueueDescriptor :: a -> Id MTLIOCommandQueueDescriptor

instance IsMTLIOCommandQueueDescriptor (Id MTLIOCommandQueueDescriptor) where
  toMTLIOCommandQueueDescriptor = unsafeCastId

instance IsNSObject (Id MTLIOCommandQueueDescriptor) where
  toNSObject = unsafeCastId

-- ---------- MTLIndirectCommandBufferDescriptor ----------

-- | Describes the limits and features that can be used in an indirect command
-- 
-- Phantom type for @MTLIndirectCommandBufferDescriptor@.
data MTLIndirectCommandBufferDescriptor

instance IsObjCObject (Id MTLIndirectCommandBufferDescriptor) where
  toRawId = idToRawId
  withObjCPtr = withIdObjCPtr
  staticClass _ = getRequiredClass "MTLIndirectCommandBufferDescriptor"

class IsNSObject a => IsMTLIndirectCommandBufferDescriptor a where
  toMTLIndirectCommandBufferDescriptor :: a -> Id MTLIndirectCommandBufferDescriptor

instance IsMTLIndirectCommandBufferDescriptor (Id MTLIndirectCommandBufferDescriptor) where
  toMTLIndirectCommandBufferDescriptor = unsafeCastId

instance IsNSObject (Id MTLIndirectCommandBufferDescriptor) where
  toNSObject = unsafeCastId

-- ---------- MTLIntersectionFunctionTableDescriptor ----------

-- | Phantom type for @MTLIntersectionFunctionTableDescriptor@.
data MTLIntersectionFunctionTableDescriptor

instance IsObjCObject (Id MTLIntersectionFunctionTableDescriptor) where
  toRawId = idToRawId
  withObjCPtr = withIdObjCPtr
  staticClass _ = getRequiredClass "MTLIntersectionFunctionTableDescriptor"

class IsNSObject a => IsMTLIntersectionFunctionTableDescriptor a where
  toMTLIntersectionFunctionTableDescriptor :: a -> Id MTLIntersectionFunctionTableDescriptor

instance IsMTLIntersectionFunctionTableDescriptor (Id MTLIntersectionFunctionTableDescriptor) where
  toMTLIntersectionFunctionTableDescriptor = unsafeCastId

instance IsNSObject (Id MTLIntersectionFunctionTableDescriptor) where
  toNSObject = unsafeCastId

-- ---------- MTLLinkedFunctions ----------

-- | MTLLinkedFunctions
--
-- A class to set functions to be linked.
--
-- All functions set on this object must have unique names.
-- 
-- Phantom type for @MTLLinkedFunctions@.
data MTLLinkedFunctions

instance IsObjCObject (Id MTLLinkedFunctions) where
  toRawId = idToRawId
  withObjCPtr = withIdObjCPtr
  staticClass _ = getRequiredClass "MTLLinkedFunctions"

class IsNSObject a => IsMTLLinkedFunctions a where
  toMTLLinkedFunctions :: a -> Id MTLLinkedFunctions

instance IsMTLLinkedFunctions (Id MTLLinkedFunctions) where
  toMTLLinkedFunctions = unsafeCastId

instance IsNSObject (Id MTLLinkedFunctions) where
  toNSObject = unsafeCastId

-- ---------- MTLLogStateDescriptor ----------

-- | Phantom type for @MTLLogStateDescriptor@.
data MTLLogStateDescriptor

instance IsObjCObject (Id MTLLogStateDescriptor) where
  toRawId = idToRawId
  withObjCPtr = withIdObjCPtr
  staticClass _ = getRequiredClass "MTLLogStateDescriptor"

class IsNSObject a => IsMTLLogStateDescriptor a where
  toMTLLogStateDescriptor :: a -> Id MTLLogStateDescriptor

instance IsMTLLogStateDescriptor (Id MTLLogStateDescriptor) where
  toMTLLogStateDescriptor = unsafeCastId

instance IsNSObject (Id MTLLogStateDescriptor) where
  toNSObject = unsafeCastId

-- ---------- MTLLogicalToPhysicalColorAttachmentMap ----------

-- | Allows you to easily specify color attachment remapping from logical to physical indices.
-- 
-- Phantom type for @MTLLogicalToPhysicalColorAttachmentMap@.
data MTLLogicalToPhysicalColorAttachmentMap

instance IsObjCObject (Id MTLLogicalToPhysicalColorAttachmentMap) where
  toRawId = idToRawId
  withObjCPtr = withIdObjCPtr
  staticClass _ = getRequiredClass "MTLLogicalToPhysicalColorAttachmentMap"

class IsNSObject a => IsMTLLogicalToPhysicalColorAttachmentMap a where
  toMTLLogicalToPhysicalColorAttachmentMap :: a -> Id MTLLogicalToPhysicalColorAttachmentMap

instance IsMTLLogicalToPhysicalColorAttachmentMap (Id MTLLogicalToPhysicalColorAttachmentMap) where
  toMTLLogicalToPhysicalColorAttachmentMap = unsafeCastId

instance IsNSObject (Id MTLLogicalToPhysicalColorAttachmentMap) where
  toNSObject = unsafeCastId

-- ---------- MTLMeshRenderPipelineDescriptor ----------

-- | MTLMeshRenderPipelineDescriptor
--
-- As an alternative to a vertex + fragment shader render pipeline, this render pipeline uses a (object +) mesh + fragment shader for rendering geometry.
-- 
-- Phantom type for @MTLMeshRenderPipelineDescriptor@.
data MTLMeshRenderPipelineDescriptor

instance IsObjCObject (Id MTLMeshRenderPipelineDescriptor) where
  toRawId = idToRawId
  withObjCPtr = withIdObjCPtr
  staticClass _ = getRequiredClass "MTLMeshRenderPipelineDescriptor"

class IsNSObject a => IsMTLMeshRenderPipelineDescriptor a where
  toMTLMeshRenderPipelineDescriptor :: a -> Id MTLMeshRenderPipelineDescriptor

instance IsMTLMeshRenderPipelineDescriptor (Id MTLMeshRenderPipelineDescriptor) where
  toMTLMeshRenderPipelineDescriptor = unsafeCastId

instance IsNSObject (Id MTLMeshRenderPipelineDescriptor) where
  toNSObject = unsafeCastId

-- ---------- MTLMotionKeyframeData ----------

-- | MTLbuffer and description how the data is stored in it.
-- 
-- Phantom type for @MTLMotionKeyframeData@.
data MTLMotionKeyframeData

instance IsObjCObject (Id MTLMotionKeyframeData) where
  toRawId = idToRawId
  withObjCPtr = withIdObjCPtr
  staticClass _ = getRequiredClass "MTLMotionKeyframeData"

class IsNSObject a => IsMTLMotionKeyframeData a where
  toMTLMotionKeyframeData :: a -> Id MTLMotionKeyframeData

instance IsMTLMotionKeyframeData (Id MTLMotionKeyframeData) where
  toMTLMotionKeyframeData = unsafeCastId

instance IsNSObject (Id MTLMotionKeyframeData) where
  toNSObject = unsafeCastId

-- ---------- MTLPipelineBufferDescriptor ----------

-- | Phantom type for @MTLPipelineBufferDescriptor@.
data MTLPipelineBufferDescriptor

instance IsObjCObject (Id MTLPipelineBufferDescriptor) where
  toRawId = idToRawId
  withObjCPtr = withIdObjCPtr
  staticClass _ = getRequiredClass "MTLPipelineBufferDescriptor"

class IsNSObject a => IsMTLPipelineBufferDescriptor a where
  toMTLPipelineBufferDescriptor :: a -> Id MTLPipelineBufferDescriptor

instance IsMTLPipelineBufferDescriptor (Id MTLPipelineBufferDescriptor) where
  toMTLPipelineBufferDescriptor = unsafeCastId

instance IsNSObject (Id MTLPipelineBufferDescriptor) where
  toNSObject = unsafeCastId

-- ---------- MTLPipelineBufferDescriptorArray ----------

-- | Phantom type for @MTLPipelineBufferDescriptorArray@.
data MTLPipelineBufferDescriptorArray

instance IsObjCObject (Id MTLPipelineBufferDescriptorArray) where
  toRawId = idToRawId
  withObjCPtr = withIdObjCPtr
  staticClass _ = getRequiredClass "MTLPipelineBufferDescriptorArray"

class IsNSObject a => IsMTLPipelineBufferDescriptorArray a where
  toMTLPipelineBufferDescriptorArray :: a -> Id MTLPipelineBufferDescriptorArray

instance IsMTLPipelineBufferDescriptorArray (Id MTLPipelineBufferDescriptorArray) where
  toMTLPipelineBufferDescriptorArray = unsafeCastId

instance IsNSObject (Id MTLPipelineBufferDescriptorArray) where
  toNSObject = unsafeCastId

-- ---------- MTLRasterizationRateLayerArray ----------

-- | MTLRasterizationRateLayerArray
--
-- Mutable array of MTLRasterizationRateLayerDescriptor
-- 
-- Phantom type for @MTLRasterizationRateLayerArray@.
data MTLRasterizationRateLayerArray

instance IsObjCObject (Id MTLRasterizationRateLayerArray) where
  toRawId = idToRawId
  withObjCPtr = withIdObjCPtr
  staticClass _ = getRequiredClass "MTLRasterizationRateLayerArray"

class IsNSObject a => IsMTLRasterizationRateLayerArray a where
  toMTLRasterizationRateLayerArray :: a -> Id MTLRasterizationRateLayerArray

instance IsMTLRasterizationRateLayerArray (Id MTLRasterizationRateLayerArray) where
  toMTLRasterizationRateLayerArray = unsafeCastId

instance IsNSObject (Id MTLRasterizationRateLayerArray) where
  toNSObject = unsafeCastId

-- ---------- MTLRasterizationRateLayerDescriptor ----------

-- | MTLRasterizationRateLayerDescriptor
--
-- Describes the minimum rasterization rate screen space using two piecewise linear functions.
--
-- The two piecewise linear function (PLF) describe the desired rasterization quality on the horizontal and vertical axis separately. Each quality sample in the PLF is stored in an array as single precision floating point value between 0 (lowest quality) and 1 (highest quality). The first sample in the array describes the quality at the top (vertical) or left (horizontal) edge of screen space. The last sample in the array describes the quality at the bottom (vertical) or right (horizontal) edge of screen space. All other samples are spaced equidistant in screen space. MTLRasterizationRateLayerDescriptor instances will be stored inside a MTLRasterizationRateMapDescriptor which in turn is compiled by MTLDevice into a MTLRasterizationRateMap. Because MTLDevice may not support the requested granularity, the provided samples may be rounded up (towards higher quality) during compilation.
-- 
-- Phantom type for @MTLRasterizationRateLayerDescriptor@.
data MTLRasterizationRateLayerDescriptor

instance IsObjCObject (Id MTLRasterizationRateLayerDescriptor) where
  toRawId = idToRawId
  withObjCPtr = withIdObjCPtr
  staticClass _ = getRequiredClass "MTLRasterizationRateLayerDescriptor"

class IsNSObject a => IsMTLRasterizationRateLayerDescriptor a where
  toMTLRasterizationRateLayerDescriptor :: a -> Id MTLRasterizationRateLayerDescriptor

instance IsMTLRasterizationRateLayerDescriptor (Id MTLRasterizationRateLayerDescriptor) where
  toMTLRasterizationRateLayerDescriptor = unsafeCastId

instance IsNSObject (Id MTLRasterizationRateLayerDescriptor) where
  toNSObject = unsafeCastId

-- ---------- MTLRasterizationRateMapDescriptor ----------

-- | MTLRasterizationRateMapDescriptor
--
-- Describes a MTLRasterizationRateMap containing an arbitrary number of MTLRasterizationRateLayerDescriptor instances.
--
-- An MTLRasterizationRateMapDescriptor is compiled into an MTLRasterizationRateMap using MTLDevice.
-- 
-- Phantom type for @MTLRasterizationRateMapDescriptor@.
data MTLRasterizationRateMapDescriptor

instance IsObjCObject (Id MTLRasterizationRateMapDescriptor) where
  toRawId = idToRawId
  withObjCPtr = withIdObjCPtr
  staticClass _ = getRequiredClass "MTLRasterizationRateMapDescriptor"

class IsNSObject a => IsMTLRasterizationRateMapDescriptor a where
  toMTLRasterizationRateMapDescriptor :: a -> Id MTLRasterizationRateMapDescriptor

instance IsMTLRasterizationRateMapDescriptor (Id MTLRasterizationRateMapDescriptor) where
  toMTLRasterizationRateMapDescriptor = unsafeCastId

instance IsNSObject (Id MTLRasterizationRateMapDescriptor) where
  toNSObject = unsafeCastId

-- ---------- MTLRasterizationRateSampleArray ----------

-- | MTLRasterizationRateSampleArray
--
-- A helper object for convient access to samples stored in an array.
-- 
-- Phantom type for @MTLRasterizationRateSampleArray@.
data MTLRasterizationRateSampleArray

instance IsObjCObject (Id MTLRasterizationRateSampleArray) where
  toRawId = idToRawId
  withObjCPtr = withIdObjCPtr
  staticClass _ = getRequiredClass "MTLRasterizationRateSampleArray"

class IsNSObject a => IsMTLRasterizationRateSampleArray a where
  toMTLRasterizationRateSampleArray :: a -> Id MTLRasterizationRateSampleArray

instance IsMTLRasterizationRateSampleArray (Id MTLRasterizationRateSampleArray) where
  toMTLRasterizationRateSampleArray = unsafeCastId

instance IsNSObject (Id MTLRasterizationRateSampleArray) where
  toNSObject = unsafeCastId

-- ---------- MTLRenderPassAttachmentDescriptor ----------

-- | Phantom type for @MTLRenderPassAttachmentDescriptor@.
data MTLRenderPassAttachmentDescriptor

instance IsObjCObject (Id MTLRenderPassAttachmentDescriptor) where
  toRawId = idToRawId
  withObjCPtr = withIdObjCPtr
  staticClass _ = getRequiredClass "MTLRenderPassAttachmentDescriptor"

class IsNSObject a => IsMTLRenderPassAttachmentDescriptor a where
  toMTLRenderPassAttachmentDescriptor :: a -> Id MTLRenderPassAttachmentDescriptor

instance IsMTLRenderPassAttachmentDescriptor (Id MTLRenderPassAttachmentDescriptor) where
  toMTLRenderPassAttachmentDescriptor = unsafeCastId

instance IsNSObject (Id MTLRenderPassAttachmentDescriptor) where
  toNSObject = unsafeCastId

-- ---------- MTLRenderPassColorAttachmentDescriptorArray ----------

-- | Phantom type for @MTLRenderPassColorAttachmentDescriptorArray@.
data MTLRenderPassColorAttachmentDescriptorArray

instance IsObjCObject (Id MTLRenderPassColorAttachmentDescriptorArray) where
  toRawId = idToRawId
  withObjCPtr = withIdObjCPtr
  staticClass _ = getRequiredClass "MTLRenderPassColorAttachmentDescriptorArray"

class IsNSObject a => IsMTLRenderPassColorAttachmentDescriptorArray a where
  toMTLRenderPassColorAttachmentDescriptorArray :: a -> Id MTLRenderPassColorAttachmentDescriptorArray

instance IsMTLRenderPassColorAttachmentDescriptorArray (Id MTLRenderPassColorAttachmentDescriptorArray) where
  toMTLRenderPassColorAttachmentDescriptorArray = unsafeCastId

instance IsNSObject (Id MTLRenderPassColorAttachmentDescriptorArray) where
  toNSObject = unsafeCastId

-- ---------- MTLRenderPassDescriptor ----------

-- | MTLRenderPassDescriptor
--
-- MTLRenderPassDescriptor represents a collection of attachments to be used to create a concrete render command encoder
-- 
-- Phantom type for @MTLRenderPassDescriptor@.
data MTLRenderPassDescriptor

instance IsObjCObject (Id MTLRenderPassDescriptor) where
  toRawId = idToRawId
  withObjCPtr = withIdObjCPtr
  staticClass _ = getRequiredClass "MTLRenderPassDescriptor"

class IsNSObject a => IsMTLRenderPassDescriptor a where
  toMTLRenderPassDescriptor :: a -> Id MTLRenderPassDescriptor

instance IsMTLRenderPassDescriptor (Id MTLRenderPassDescriptor) where
  toMTLRenderPassDescriptor = unsafeCastId

instance IsNSObject (Id MTLRenderPassDescriptor) where
  toNSObject = unsafeCastId

-- ---------- MTLRenderPassSampleBufferAttachmentDescriptor ----------

-- | Phantom type for @MTLRenderPassSampleBufferAttachmentDescriptor@.
data MTLRenderPassSampleBufferAttachmentDescriptor

instance IsObjCObject (Id MTLRenderPassSampleBufferAttachmentDescriptor) where
  toRawId = idToRawId
  withObjCPtr = withIdObjCPtr
  staticClass _ = getRequiredClass "MTLRenderPassSampleBufferAttachmentDescriptor"

class IsNSObject a => IsMTLRenderPassSampleBufferAttachmentDescriptor a where
  toMTLRenderPassSampleBufferAttachmentDescriptor :: a -> Id MTLRenderPassSampleBufferAttachmentDescriptor

instance IsMTLRenderPassSampleBufferAttachmentDescriptor (Id MTLRenderPassSampleBufferAttachmentDescriptor) where
  toMTLRenderPassSampleBufferAttachmentDescriptor = unsafeCastId

instance IsNSObject (Id MTLRenderPassSampleBufferAttachmentDescriptor) where
  toNSObject = unsafeCastId

-- ---------- MTLRenderPassSampleBufferAttachmentDescriptorArray ----------

-- | Phantom type for @MTLRenderPassSampleBufferAttachmentDescriptorArray@.
data MTLRenderPassSampleBufferAttachmentDescriptorArray

instance IsObjCObject (Id MTLRenderPassSampleBufferAttachmentDescriptorArray) where
  toRawId = idToRawId
  withObjCPtr = withIdObjCPtr
  staticClass _ = getRequiredClass "MTLRenderPassSampleBufferAttachmentDescriptorArray"

class IsNSObject a => IsMTLRenderPassSampleBufferAttachmentDescriptorArray a where
  toMTLRenderPassSampleBufferAttachmentDescriptorArray :: a -> Id MTLRenderPassSampleBufferAttachmentDescriptorArray

instance IsMTLRenderPassSampleBufferAttachmentDescriptorArray (Id MTLRenderPassSampleBufferAttachmentDescriptorArray) where
  toMTLRenderPassSampleBufferAttachmentDescriptorArray = unsafeCastId

instance IsNSObject (Id MTLRenderPassSampleBufferAttachmentDescriptorArray) where
  toNSObject = unsafeCastId

-- ---------- MTLRenderPipelineColorAttachmentDescriptor ----------

-- | Phantom type for @MTLRenderPipelineColorAttachmentDescriptor@.
data MTLRenderPipelineColorAttachmentDescriptor

instance IsObjCObject (Id MTLRenderPipelineColorAttachmentDescriptor) where
  toRawId = idToRawId
  withObjCPtr = withIdObjCPtr
  staticClass _ = getRequiredClass "MTLRenderPipelineColorAttachmentDescriptor"

class IsNSObject a => IsMTLRenderPipelineColorAttachmentDescriptor a where
  toMTLRenderPipelineColorAttachmentDescriptor :: a -> Id MTLRenderPipelineColorAttachmentDescriptor

instance IsMTLRenderPipelineColorAttachmentDescriptor (Id MTLRenderPipelineColorAttachmentDescriptor) where
  toMTLRenderPipelineColorAttachmentDescriptor = unsafeCastId

instance IsNSObject (Id MTLRenderPipelineColorAttachmentDescriptor) where
  toNSObject = unsafeCastId

-- ---------- MTLRenderPipelineColorAttachmentDescriptorArray ----------

-- | Phantom type for @MTLRenderPipelineColorAttachmentDescriptorArray@.
data MTLRenderPipelineColorAttachmentDescriptorArray

instance IsObjCObject (Id MTLRenderPipelineColorAttachmentDescriptorArray) where
  toRawId = idToRawId
  withObjCPtr = withIdObjCPtr
  staticClass _ = getRequiredClass "MTLRenderPipelineColorAttachmentDescriptorArray"

class IsNSObject a => IsMTLRenderPipelineColorAttachmentDescriptorArray a where
  toMTLRenderPipelineColorAttachmentDescriptorArray :: a -> Id MTLRenderPipelineColorAttachmentDescriptorArray

instance IsMTLRenderPipelineColorAttachmentDescriptorArray (Id MTLRenderPipelineColorAttachmentDescriptorArray) where
  toMTLRenderPipelineColorAttachmentDescriptorArray = unsafeCastId

instance IsNSObject (Id MTLRenderPipelineColorAttachmentDescriptorArray) where
  toNSObject = unsafeCastId

-- ---------- MTLRenderPipelineDescriptor ----------

-- | Phantom type for @MTLRenderPipelineDescriptor@.
data MTLRenderPipelineDescriptor

instance IsObjCObject (Id MTLRenderPipelineDescriptor) where
  toRawId = idToRawId
  withObjCPtr = withIdObjCPtr
  staticClass _ = getRequiredClass "MTLRenderPipelineDescriptor"

class IsNSObject a => IsMTLRenderPipelineDescriptor a where
  toMTLRenderPipelineDescriptor :: a -> Id MTLRenderPipelineDescriptor

instance IsMTLRenderPipelineDescriptor (Id MTLRenderPipelineDescriptor) where
  toMTLRenderPipelineDescriptor = unsafeCastId

instance IsNSObject (Id MTLRenderPipelineDescriptor) where
  toNSObject = unsafeCastId

-- ---------- MTLRenderPipelineFunctionsDescriptor ----------

-- | Phantom type for @MTLRenderPipelineFunctionsDescriptor@.
data MTLRenderPipelineFunctionsDescriptor

instance IsObjCObject (Id MTLRenderPipelineFunctionsDescriptor) where
  toRawId = idToRawId
  withObjCPtr = withIdObjCPtr
  staticClass _ = getRequiredClass "MTLRenderPipelineFunctionsDescriptor"

class IsNSObject a => IsMTLRenderPipelineFunctionsDescriptor a where
  toMTLRenderPipelineFunctionsDescriptor :: a -> Id MTLRenderPipelineFunctionsDescriptor

instance IsMTLRenderPipelineFunctionsDescriptor (Id MTLRenderPipelineFunctionsDescriptor) where
  toMTLRenderPipelineFunctionsDescriptor = unsafeCastId

instance IsNSObject (Id MTLRenderPipelineFunctionsDescriptor) where
  toNSObject = unsafeCastId

-- ---------- MTLRenderPipelineReflection ----------

-- | Phantom type for @MTLRenderPipelineReflection@.
data MTLRenderPipelineReflection

instance IsObjCObject (Id MTLRenderPipelineReflection) where
  toRawId = idToRawId
  withObjCPtr = withIdObjCPtr
  staticClass _ = getRequiredClass "MTLRenderPipelineReflection"

class IsNSObject a => IsMTLRenderPipelineReflection a where
  toMTLRenderPipelineReflection :: a -> Id MTLRenderPipelineReflection

instance IsMTLRenderPipelineReflection (Id MTLRenderPipelineReflection) where
  toMTLRenderPipelineReflection = unsafeCastId

instance IsNSObject (Id MTLRenderPipelineReflection) where
  toNSObject = unsafeCastId

-- ---------- MTLResidencySetDescriptor ----------

-- | MTLResidencySetDescriptor
--
-- Specifies the parameters for MTLResidencySet creation.
-- 
-- Phantom type for @MTLResidencySetDescriptor@.
data MTLResidencySetDescriptor

instance IsObjCObject (Id MTLResidencySetDescriptor) where
  toRawId = idToRawId
  withObjCPtr = withIdObjCPtr
  staticClass _ = getRequiredClass "MTLResidencySetDescriptor"

class IsNSObject a => IsMTLResidencySetDescriptor a where
  toMTLResidencySetDescriptor :: a -> Id MTLResidencySetDescriptor

instance IsMTLResidencySetDescriptor (Id MTLResidencySetDescriptor) where
  toMTLResidencySetDescriptor = unsafeCastId

instance IsNSObject (Id MTLResidencySetDescriptor) where
  toNSObject = unsafeCastId

-- ---------- MTLResourceStatePassDescriptor ----------

-- | MTLResourceStatePassDescriptor
--
-- MTLResourceStatePassDescriptor represents a collection of attachments to be used to create a concrete resourceState command encoder
-- 
-- Phantom type for @MTLResourceStatePassDescriptor@.
data MTLResourceStatePassDescriptor

instance IsObjCObject (Id MTLResourceStatePassDescriptor) where
  toRawId = idToRawId
  withObjCPtr = withIdObjCPtr
  staticClass _ = getRequiredClass "MTLResourceStatePassDescriptor"

class IsNSObject a => IsMTLResourceStatePassDescriptor a where
  toMTLResourceStatePassDescriptor :: a -> Id MTLResourceStatePassDescriptor

instance IsMTLResourceStatePassDescriptor (Id MTLResourceStatePassDescriptor) where
  toMTLResourceStatePassDescriptor = unsafeCastId

instance IsNSObject (Id MTLResourceStatePassDescriptor) where
  toNSObject = unsafeCastId

-- ---------- MTLResourceStatePassSampleBufferAttachmentDescriptor ----------

-- | Phantom type for @MTLResourceStatePassSampleBufferAttachmentDescriptor@.
data MTLResourceStatePassSampleBufferAttachmentDescriptor

instance IsObjCObject (Id MTLResourceStatePassSampleBufferAttachmentDescriptor) where
  toRawId = idToRawId
  withObjCPtr = withIdObjCPtr
  staticClass _ = getRequiredClass "MTLResourceStatePassSampleBufferAttachmentDescriptor"

class IsNSObject a => IsMTLResourceStatePassSampleBufferAttachmentDescriptor a where
  toMTLResourceStatePassSampleBufferAttachmentDescriptor :: a -> Id MTLResourceStatePassSampleBufferAttachmentDescriptor

instance IsMTLResourceStatePassSampleBufferAttachmentDescriptor (Id MTLResourceStatePassSampleBufferAttachmentDescriptor) where
  toMTLResourceStatePassSampleBufferAttachmentDescriptor = unsafeCastId

instance IsNSObject (Id MTLResourceStatePassSampleBufferAttachmentDescriptor) where
  toNSObject = unsafeCastId

-- ---------- MTLResourceStatePassSampleBufferAttachmentDescriptorArray ----------

-- | Phantom type for @MTLResourceStatePassSampleBufferAttachmentDescriptorArray@.
data MTLResourceStatePassSampleBufferAttachmentDescriptorArray

instance IsObjCObject (Id MTLResourceStatePassSampleBufferAttachmentDescriptorArray) where
  toRawId = idToRawId
  withObjCPtr = withIdObjCPtr
  staticClass _ = getRequiredClass "MTLResourceStatePassSampleBufferAttachmentDescriptorArray"

class IsNSObject a => IsMTLResourceStatePassSampleBufferAttachmentDescriptorArray a where
  toMTLResourceStatePassSampleBufferAttachmentDescriptorArray :: a -> Id MTLResourceStatePassSampleBufferAttachmentDescriptorArray

instance IsMTLResourceStatePassSampleBufferAttachmentDescriptorArray (Id MTLResourceStatePassSampleBufferAttachmentDescriptorArray) where
  toMTLResourceStatePassSampleBufferAttachmentDescriptorArray = unsafeCastId

instance IsNSObject (Id MTLResourceStatePassSampleBufferAttachmentDescriptorArray) where
  toNSObject = unsafeCastId

-- ---------- MTLResourceViewPoolDescriptor ----------

-- | Provides parameters for creating a resource view pool.
-- 
-- Phantom type for @MTLResourceViewPoolDescriptor@.
data MTLResourceViewPoolDescriptor

instance IsObjCObject (Id MTLResourceViewPoolDescriptor) where
  toRawId = idToRawId
  withObjCPtr = withIdObjCPtr
  staticClass _ = getRequiredClass "MTLResourceViewPoolDescriptor"

class IsNSObject a => IsMTLResourceViewPoolDescriptor a where
  toMTLResourceViewPoolDescriptor :: a -> Id MTLResourceViewPoolDescriptor

instance IsMTLResourceViewPoolDescriptor (Id MTLResourceViewPoolDescriptor) where
  toMTLResourceViewPoolDescriptor = unsafeCastId

instance IsNSObject (Id MTLResourceViewPoolDescriptor) where
  toNSObject = unsafeCastId

-- ---------- MTLSamplerDescriptor ----------

-- | MTLSamplerDescriptor
--
-- A mutable descriptor used to configure a sampler.  When complete, this can be used to create an immutable MTLSamplerState.
-- 
-- Phantom type for @MTLSamplerDescriptor@.
data MTLSamplerDescriptor

instance IsObjCObject (Id MTLSamplerDescriptor) where
  toRawId = idToRawId
  withObjCPtr = withIdObjCPtr
  staticClass _ = getRequiredClass "MTLSamplerDescriptor"

class IsNSObject a => IsMTLSamplerDescriptor a where
  toMTLSamplerDescriptor :: a -> Id MTLSamplerDescriptor

instance IsMTLSamplerDescriptor (Id MTLSamplerDescriptor) where
  toMTLSamplerDescriptor = unsafeCastId

instance IsNSObject (Id MTLSamplerDescriptor) where
  toNSObject = unsafeCastId

-- ---------- MTLSharedEventHandle ----------

-- | Phantom type for @MTLSharedEventHandle@.
data MTLSharedEventHandle

instance IsObjCObject (Id MTLSharedEventHandle) where
  toRawId = idToRawId
  withObjCPtr = withIdObjCPtr
  staticClass _ = getRequiredClass "MTLSharedEventHandle"

class IsNSObject a => IsMTLSharedEventHandle a where
  toMTLSharedEventHandle :: a -> Id MTLSharedEventHandle

instance IsMTLSharedEventHandle (Id MTLSharedEventHandle) where
  toMTLSharedEventHandle = unsafeCastId

instance IsNSObject (Id MTLSharedEventHandle) where
  toNSObject = unsafeCastId

-- ---------- MTLSharedEventListener ----------

-- | MTLSharedEventListener
--
-- This class provides a simple interface for handling the dispatching of MTLSharedEvent notifications from Metal.
-- 
-- Phantom type for @MTLSharedEventListener@.
data MTLSharedEventListener

instance IsObjCObject (Id MTLSharedEventListener) where
  toRawId = idToRawId
  withObjCPtr = withIdObjCPtr
  staticClass _ = getRequiredClass "MTLSharedEventListener"

class IsNSObject a => IsMTLSharedEventListener a where
  toMTLSharedEventListener :: a -> Id MTLSharedEventListener

instance IsMTLSharedEventListener (Id MTLSharedEventListener) where
  toMTLSharedEventListener = unsafeCastId

instance IsNSObject (Id MTLSharedEventListener) where
  toNSObject = unsafeCastId

-- ---------- MTLSharedTextureHandle ----------

-- | Phantom type for @MTLSharedTextureHandle@.
data MTLSharedTextureHandle

instance IsObjCObject (Id MTLSharedTextureHandle) where
  toRawId = idToRawId
  withObjCPtr = withIdObjCPtr
  staticClass _ = getRequiredClass "MTLSharedTextureHandle"

class IsNSObject a => IsMTLSharedTextureHandle a where
  toMTLSharedTextureHandle :: a -> Id MTLSharedTextureHandle

instance IsMTLSharedTextureHandle (Id MTLSharedTextureHandle) where
  toMTLSharedTextureHandle = unsafeCastId

instance IsNSObject (Id MTLSharedTextureHandle) where
  toNSObject = unsafeCastId

-- ---------- MTLStageInputOutputDescriptor ----------

-- | Phantom type for @MTLStageInputOutputDescriptor@.
data MTLStageInputOutputDescriptor

instance IsObjCObject (Id MTLStageInputOutputDescriptor) where
  toRawId = idToRawId
  withObjCPtr = withIdObjCPtr
  staticClass _ = getRequiredClass "MTLStageInputOutputDescriptor"

class IsNSObject a => IsMTLStageInputOutputDescriptor a where
  toMTLStageInputOutputDescriptor :: a -> Id MTLStageInputOutputDescriptor

instance IsMTLStageInputOutputDescriptor (Id MTLStageInputOutputDescriptor) where
  toMTLStageInputOutputDescriptor = unsafeCastId

instance IsNSObject (Id MTLStageInputOutputDescriptor) where
  toNSObject = unsafeCastId

-- ---------- MTLStencilDescriptor ----------

-- | Phantom type for @MTLStencilDescriptor@.
data MTLStencilDescriptor

instance IsObjCObject (Id MTLStencilDescriptor) where
  toRawId = idToRawId
  withObjCPtr = withIdObjCPtr
  staticClass _ = getRequiredClass "MTLStencilDescriptor"

class IsNSObject a => IsMTLStencilDescriptor a where
  toMTLStencilDescriptor :: a -> Id MTLStencilDescriptor

instance IsMTLStencilDescriptor (Id MTLStencilDescriptor) where
  toMTLStencilDescriptor = unsafeCastId

instance IsNSObject (Id MTLStencilDescriptor) where
  toNSObject = unsafeCastId

-- ---------- MTLStitchedLibraryDescriptor ----------

-- | MTLStitchedLibraryDescriptor
--
-- A container for the graphs and functions needed to create the stitched functions described by the graphs.
-- 
-- Phantom type for @MTLStitchedLibraryDescriptor@.
data MTLStitchedLibraryDescriptor

instance IsObjCObject (Id MTLStitchedLibraryDescriptor) where
  toRawId = idToRawId
  withObjCPtr = withIdObjCPtr
  staticClass _ = getRequiredClass "MTLStitchedLibraryDescriptor"

class IsNSObject a => IsMTLStitchedLibraryDescriptor a where
  toMTLStitchedLibraryDescriptor :: a -> Id MTLStitchedLibraryDescriptor

instance IsMTLStitchedLibraryDescriptor (Id MTLStitchedLibraryDescriptor) where
  toMTLStitchedLibraryDescriptor = unsafeCastId

instance IsNSObject (Id MTLStitchedLibraryDescriptor) where
  toNSObject = unsafeCastId

-- ---------- MTLStructMember ----------

-- | Phantom type for @MTLStructMember@.
data MTLStructMember

instance IsObjCObject (Id MTLStructMember) where
  toRawId = idToRawId
  withObjCPtr = withIdObjCPtr
  staticClass _ = getRequiredClass "MTLStructMember"

class IsNSObject a => IsMTLStructMember a where
  toMTLStructMember :: a -> Id MTLStructMember

instance IsMTLStructMember (Id MTLStructMember) where
  toMTLStructMember = unsafeCastId

instance IsNSObject (Id MTLStructMember) where
  toNSObject = unsafeCastId

-- ---------- MTLTensorDescriptor ----------

-- | A configuration type for creating new tensor instances.
-- 
-- Phantom type for @MTLTensorDescriptor@.
data MTLTensorDescriptor

instance IsObjCObject (Id MTLTensorDescriptor) where
  toRawId = idToRawId
  withObjCPtr = withIdObjCPtr
  staticClass _ = getRequiredClass "MTLTensorDescriptor"

class IsNSObject a => IsMTLTensorDescriptor a where
  toMTLTensorDescriptor :: a -> Id MTLTensorDescriptor

instance IsMTLTensorDescriptor (Id MTLTensorDescriptor) where
  toMTLTensorDescriptor = unsafeCastId

instance IsNSObject (Id MTLTensorDescriptor) where
  toNSObject = unsafeCastId

-- ---------- MTLTensorExtents ----------

-- | An array of length matching the rank, holding the dimensions of a tensor.
--
-- Supports rank up to ``MTL_TENSOR_MAX_RANK``.
-- 
-- Phantom type for @MTLTensorExtents@.
data MTLTensorExtents

instance IsObjCObject (Id MTLTensorExtents) where
  toRawId = idToRawId
  withObjCPtr = withIdObjCPtr
  staticClass _ = getRequiredClass "MTLTensorExtents"

class IsNSObject a => IsMTLTensorExtents a where
  toMTLTensorExtents :: a -> Id MTLTensorExtents

instance IsMTLTensorExtents (Id MTLTensorExtents) where
  toMTLTensorExtents = unsafeCastId

instance IsNSObject (Id MTLTensorExtents) where
  toNSObject = unsafeCastId

-- ---------- MTLTextureDescriptor ----------

-- | Phantom type for @MTLTextureDescriptor@.
data MTLTextureDescriptor

instance IsObjCObject (Id MTLTextureDescriptor) where
  toRawId = idToRawId
  withObjCPtr = withIdObjCPtr
  staticClass _ = getRequiredClass "MTLTextureDescriptor"

class IsNSObject a => IsMTLTextureDescriptor a where
  toMTLTextureDescriptor :: a -> Id MTLTextureDescriptor

instance IsMTLTextureDescriptor (Id MTLTextureDescriptor) where
  toMTLTextureDescriptor = unsafeCastId

instance IsNSObject (Id MTLTextureDescriptor) where
  toNSObject = unsafeCastId

-- ---------- MTLTextureViewDescriptor ----------

-- | Phantom type for @MTLTextureViewDescriptor@.
data MTLTextureViewDescriptor

instance IsObjCObject (Id MTLTextureViewDescriptor) where
  toRawId = idToRawId
  withObjCPtr = withIdObjCPtr
  staticClass _ = getRequiredClass "MTLTextureViewDescriptor"

class IsNSObject a => IsMTLTextureViewDescriptor a where
  toMTLTextureViewDescriptor :: a -> Id MTLTextureViewDescriptor

instance IsMTLTextureViewDescriptor (Id MTLTextureViewDescriptor) where
  toMTLTextureViewDescriptor = unsafeCastId

instance IsNSObject (Id MTLTextureViewDescriptor) where
  toNSObject = unsafeCastId

-- ---------- MTLTileRenderPipelineColorAttachmentDescriptor ----------

-- | Phantom type for @MTLTileRenderPipelineColorAttachmentDescriptor@.
data MTLTileRenderPipelineColorAttachmentDescriptor

instance IsObjCObject (Id MTLTileRenderPipelineColorAttachmentDescriptor) where
  toRawId = idToRawId
  withObjCPtr = withIdObjCPtr
  staticClass _ = getRequiredClass "MTLTileRenderPipelineColorAttachmentDescriptor"

class IsNSObject a => IsMTLTileRenderPipelineColorAttachmentDescriptor a where
  toMTLTileRenderPipelineColorAttachmentDescriptor :: a -> Id MTLTileRenderPipelineColorAttachmentDescriptor

instance IsMTLTileRenderPipelineColorAttachmentDescriptor (Id MTLTileRenderPipelineColorAttachmentDescriptor) where
  toMTLTileRenderPipelineColorAttachmentDescriptor = unsafeCastId

instance IsNSObject (Id MTLTileRenderPipelineColorAttachmentDescriptor) where
  toNSObject = unsafeCastId

-- ---------- MTLTileRenderPipelineColorAttachmentDescriptorArray ----------

-- | Phantom type for @MTLTileRenderPipelineColorAttachmentDescriptorArray@.
data MTLTileRenderPipelineColorAttachmentDescriptorArray

instance IsObjCObject (Id MTLTileRenderPipelineColorAttachmentDescriptorArray) where
  toRawId = idToRawId
  withObjCPtr = withIdObjCPtr
  staticClass _ = getRequiredClass "MTLTileRenderPipelineColorAttachmentDescriptorArray"

class IsNSObject a => IsMTLTileRenderPipelineColorAttachmentDescriptorArray a where
  toMTLTileRenderPipelineColorAttachmentDescriptorArray :: a -> Id MTLTileRenderPipelineColorAttachmentDescriptorArray

instance IsMTLTileRenderPipelineColorAttachmentDescriptorArray (Id MTLTileRenderPipelineColorAttachmentDescriptorArray) where
  toMTLTileRenderPipelineColorAttachmentDescriptorArray = unsafeCastId

instance IsNSObject (Id MTLTileRenderPipelineColorAttachmentDescriptorArray) where
  toNSObject = unsafeCastId

-- ---------- MTLTileRenderPipelineDescriptor ----------

-- | Phantom type for @MTLTileRenderPipelineDescriptor@.
data MTLTileRenderPipelineDescriptor

instance IsObjCObject (Id MTLTileRenderPipelineDescriptor) where
  toRawId = idToRawId
  withObjCPtr = withIdObjCPtr
  staticClass _ = getRequiredClass "MTLTileRenderPipelineDescriptor"

class IsNSObject a => IsMTLTileRenderPipelineDescriptor a where
  toMTLTileRenderPipelineDescriptor :: a -> Id MTLTileRenderPipelineDescriptor

instance IsMTLTileRenderPipelineDescriptor (Id MTLTileRenderPipelineDescriptor) where
  toMTLTileRenderPipelineDescriptor = unsafeCastId

instance IsNSObject (Id MTLTileRenderPipelineDescriptor) where
  toNSObject = unsafeCastId

-- ---------- MTLType ----------

-- | Phantom type for @MTLType@.
data MTLType

instance IsObjCObject (Id MTLType) where
  toRawId = idToRawId
  withObjCPtr = withIdObjCPtr
  staticClass _ = getRequiredClass "MTLType"

class IsNSObject a => IsMTLType a where
  toMTLType :: a -> Id MTLType

instance IsMTLType (Id MTLType) where
  toMTLType = unsafeCastId

instance IsNSObject (Id MTLType) where
  toNSObject = unsafeCastId

-- ---------- MTLVertexAttribute ----------

-- | Phantom type for @MTLVertexAttribute@.
data MTLVertexAttribute

instance IsObjCObject (Id MTLVertexAttribute) where
  toRawId = idToRawId
  withObjCPtr = withIdObjCPtr
  staticClass _ = getRequiredClass "MTLVertexAttribute"

class IsNSObject a => IsMTLVertexAttribute a where
  toMTLVertexAttribute :: a -> Id MTLVertexAttribute

instance IsMTLVertexAttribute (Id MTLVertexAttribute) where
  toMTLVertexAttribute = unsafeCastId

instance IsNSObject (Id MTLVertexAttribute) where
  toNSObject = unsafeCastId

-- ---------- MTLVertexAttributeDescriptor ----------

-- | Phantom type for @MTLVertexAttributeDescriptor@.
data MTLVertexAttributeDescriptor

instance IsObjCObject (Id MTLVertexAttributeDescriptor) where
  toRawId = idToRawId
  withObjCPtr = withIdObjCPtr
  staticClass _ = getRequiredClass "MTLVertexAttributeDescriptor"

class IsNSObject a => IsMTLVertexAttributeDescriptor a where
  toMTLVertexAttributeDescriptor :: a -> Id MTLVertexAttributeDescriptor

instance IsMTLVertexAttributeDescriptor (Id MTLVertexAttributeDescriptor) where
  toMTLVertexAttributeDescriptor = unsafeCastId

instance IsNSObject (Id MTLVertexAttributeDescriptor) where
  toNSObject = unsafeCastId

-- ---------- MTLVertexAttributeDescriptorArray ----------

-- | Phantom type for @MTLVertexAttributeDescriptorArray@.
data MTLVertexAttributeDescriptorArray

instance IsObjCObject (Id MTLVertexAttributeDescriptorArray) where
  toRawId = idToRawId
  withObjCPtr = withIdObjCPtr
  staticClass _ = getRequiredClass "MTLVertexAttributeDescriptorArray"

class IsNSObject a => IsMTLVertexAttributeDescriptorArray a where
  toMTLVertexAttributeDescriptorArray :: a -> Id MTLVertexAttributeDescriptorArray

instance IsMTLVertexAttributeDescriptorArray (Id MTLVertexAttributeDescriptorArray) where
  toMTLVertexAttributeDescriptorArray = unsafeCastId

instance IsNSObject (Id MTLVertexAttributeDescriptorArray) where
  toNSObject = unsafeCastId

-- ---------- MTLVertexBufferLayoutDescriptor ----------

-- | Phantom type for @MTLVertexBufferLayoutDescriptor@.
data MTLVertexBufferLayoutDescriptor

instance IsObjCObject (Id MTLVertexBufferLayoutDescriptor) where
  toRawId = idToRawId
  withObjCPtr = withIdObjCPtr
  staticClass _ = getRequiredClass "MTLVertexBufferLayoutDescriptor"

class IsNSObject a => IsMTLVertexBufferLayoutDescriptor a where
  toMTLVertexBufferLayoutDescriptor :: a -> Id MTLVertexBufferLayoutDescriptor

instance IsMTLVertexBufferLayoutDescriptor (Id MTLVertexBufferLayoutDescriptor) where
  toMTLVertexBufferLayoutDescriptor = unsafeCastId

instance IsNSObject (Id MTLVertexBufferLayoutDescriptor) where
  toNSObject = unsafeCastId

-- ---------- MTLVertexBufferLayoutDescriptorArray ----------

-- | Phantom type for @MTLVertexBufferLayoutDescriptorArray@.
data MTLVertexBufferLayoutDescriptorArray

instance IsObjCObject (Id MTLVertexBufferLayoutDescriptorArray) where
  toRawId = idToRawId
  withObjCPtr = withIdObjCPtr
  staticClass _ = getRequiredClass "MTLVertexBufferLayoutDescriptorArray"

class IsNSObject a => IsMTLVertexBufferLayoutDescriptorArray a where
  toMTLVertexBufferLayoutDescriptorArray :: a -> Id MTLVertexBufferLayoutDescriptorArray

instance IsMTLVertexBufferLayoutDescriptorArray (Id MTLVertexBufferLayoutDescriptorArray) where
  toMTLVertexBufferLayoutDescriptorArray = unsafeCastId

instance IsNSObject (Id MTLVertexBufferLayoutDescriptorArray) where
  toNSObject = unsafeCastId

-- ---------- MTLVertexDescriptor ----------

-- | Phantom type for @MTLVertexDescriptor@.
data MTLVertexDescriptor

instance IsObjCObject (Id MTLVertexDescriptor) where
  toRawId = idToRawId
  withObjCPtr = withIdObjCPtr
  staticClass _ = getRequiredClass "MTLVertexDescriptor"

class IsNSObject a => IsMTLVertexDescriptor a where
  toMTLVertexDescriptor :: a -> Id MTLVertexDescriptor

instance IsMTLVertexDescriptor (Id MTLVertexDescriptor) where
  toMTLVertexDescriptor = unsafeCastId

instance IsNSObject (Id MTLVertexDescriptor) where
  toNSObject = unsafeCastId

-- ---------- MTLVisibleFunctionTableDescriptor ----------

-- | Phantom type for @MTLVisibleFunctionTableDescriptor@.
data MTLVisibleFunctionTableDescriptor

instance IsObjCObject (Id MTLVisibleFunctionTableDescriptor) where
  toRawId = idToRawId
  withObjCPtr = withIdObjCPtr
  staticClass _ = getRequiredClass "MTLVisibleFunctionTableDescriptor"

class IsNSObject a => IsMTLVisibleFunctionTableDescriptor a where
  toMTLVisibleFunctionTableDescriptor :: a -> Id MTLVisibleFunctionTableDescriptor

instance IsMTLVisibleFunctionTableDescriptor (Id MTLVisibleFunctionTableDescriptor) where
  toMTLVisibleFunctionTableDescriptor = unsafeCastId

instance IsNSObject (Id MTLVisibleFunctionTableDescriptor) where
  toNSObject = unsafeCastId

-- ---------- MTL4AccelerationStructureBoundingBoxGeometryDescriptor ----------

-- | Describes bounding-box geometry suitable for ray tracing.
--
-- You use bounding boxes to implement procedural geometry for ray tracing, such as spheres or any other shape you define by using intersection functions.
--
-- Use a ``MTLResidencySet`` to mark residency of all buffers this descriptor references when you build this acceleration structure.
-- 
-- Phantom type for @MTL4AccelerationStructureBoundingBoxGeometryDescriptor@.
data MTL4AccelerationStructureBoundingBoxGeometryDescriptor

instance IsObjCObject (Id MTL4AccelerationStructureBoundingBoxGeometryDescriptor) where
  toRawId = idToRawId
  withObjCPtr = withIdObjCPtr
  staticClass _ = getRequiredClass "MTL4AccelerationStructureBoundingBoxGeometryDescriptor"

class IsMTL4AccelerationStructureGeometryDescriptor a => IsMTL4AccelerationStructureBoundingBoxGeometryDescriptor a where
  toMTL4AccelerationStructureBoundingBoxGeometryDescriptor :: a -> Id MTL4AccelerationStructureBoundingBoxGeometryDescriptor

instance IsMTL4AccelerationStructureBoundingBoxGeometryDescriptor (Id MTL4AccelerationStructureBoundingBoxGeometryDescriptor) where
  toMTL4AccelerationStructureBoundingBoxGeometryDescriptor = unsafeCastId

instance IsMTL4AccelerationStructureGeometryDescriptor (Id MTL4AccelerationStructureBoundingBoxGeometryDescriptor) where
  toMTL4AccelerationStructureGeometryDescriptor = unsafeCastId

instance IsNSObject (Id MTL4AccelerationStructureBoundingBoxGeometryDescriptor) where
  toNSObject = unsafeCastId

-- ---------- MTL4AccelerationStructureCurveGeometryDescriptor ----------

-- | Describes curve geometry suitable for ray tracing.
--
-- Use a ``MTLResidencySet`` to mark residency of all buffers this descriptor references when you build this acceleration structure.
-- 
-- Phantom type for @MTL4AccelerationStructureCurveGeometryDescriptor@.
data MTL4AccelerationStructureCurveGeometryDescriptor

instance IsObjCObject (Id MTL4AccelerationStructureCurveGeometryDescriptor) where
  toRawId = idToRawId
  withObjCPtr = withIdObjCPtr
  staticClass _ = getRequiredClass "MTL4AccelerationStructureCurveGeometryDescriptor"

class IsMTL4AccelerationStructureGeometryDescriptor a => IsMTL4AccelerationStructureCurveGeometryDescriptor a where
  toMTL4AccelerationStructureCurveGeometryDescriptor :: a -> Id MTL4AccelerationStructureCurveGeometryDescriptor

instance IsMTL4AccelerationStructureCurveGeometryDescriptor (Id MTL4AccelerationStructureCurveGeometryDescriptor) where
  toMTL4AccelerationStructureCurveGeometryDescriptor = unsafeCastId

instance IsMTL4AccelerationStructureGeometryDescriptor (Id MTL4AccelerationStructureCurveGeometryDescriptor) where
  toMTL4AccelerationStructureGeometryDescriptor = unsafeCastId

instance IsNSObject (Id MTL4AccelerationStructureCurveGeometryDescriptor) where
  toNSObject = unsafeCastId

-- ---------- MTL4AccelerationStructureMotionBoundingBoxGeometryDescriptor ----------

-- | Describes motion bounding box geometry, suitable for motion ray tracing.
--
-- You use bounding boxes to implement procedural geometry for ray tracing, such as spheres or any other shape you define by using intersection functions.
--
-- Use a ``MTLResidencySet`` to mark residency of all buffers this descriptor references when you build this acceleration structure.
-- 
-- Phantom type for @MTL4AccelerationStructureMotionBoundingBoxGeometryDescriptor@.
data MTL4AccelerationStructureMotionBoundingBoxGeometryDescriptor

instance IsObjCObject (Id MTL4AccelerationStructureMotionBoundingBoxGeometryDescriptor) where
  toRawId = idToRawId
  withObjCPtr = withIdObjCPtr
  staticClass _ = getRequiredClass "MTL4AccelerationStructureMotionBoundingBoxGeometryDescriptor"

class IsMTL4AccelerationStructureGeometryDescriptor a => IsMTL4AccelerationStructureMotionBoundingBoxGeometryDescriptor a where
  toMTL4AccelerationStructureMotionBoundingBoxGeometryDescriptor :: a -> Id MTL4AccelerationStructureMotionBoundingBoxGeometryDescriptor

instance IsMTL4AccelerationStructureMotionBoundingBoxGeometryDescriptor (Id MTL4AccelerationStructureMotionBoundingBoxGeometryDescriptor) where
  toMTL4AccelerationStructureMotionBoundingBoxGeometryDescriptor = unsafeCastId

instance IsMTL4AccelerationStructureGeometryDescriptor (Id MTL4AccelerationStructureMotionBoundingBoxGeometryDescriptor) where
  toMTL4AccelerationStructureGeometryDescriptor = unsafeCastId

instance IsNSObject (Id MTL4AccelerationStructureMotionBoundingBoxGeometryDescriptor) where
  toNSObject = unsafeCastId

-- ---------- MTL4AccelerationStructureMotionCurveGeometryDescriptor ----------

-- | Describes motion curve geometry, suitable for motion ray tracing.
--
-- Use a ``MTLResidencySet`` to mark residency of all buffers this descriptor references when you build this acceleration structure.
-- 
-- Phantom type for @MTL4AccelerationStructureMotionCurveGeometryDescriptor@.
data MTL4AccelerationStructureMotionCurveGeometryDescriptor

instance IsObjCObject (Id MTL4AccelerationStructureMotionCurveGeometryDescriptor) where
  toRawId = idToRawId
  withObjCPtr = withIdObjCPtr
  staticClass _ = getRequiredClass "MTL4AccelerationStructureMotionCurveGeometryDescriptor"

class IsMTL4AccelerationStructureGeometryDescriptor a => IsMTL4AccelerationStructureMotionCurveGeometryDescriptor a where
  toMTL4AccelerationStructureMotionCurveGeometryDescriptor :: a -> Id MTL4AccelerationStructureMotionCurveGeometryDescriptor

instance IsMTL4AccelerationStructureMotionCurveGeometryDescriptor (Id MTL4AccelerationStructureMotionCurveGeometryDescriptor) where
  toMTL4AccelerationStructureMotionCurveGeometryDescriptor = unsafeCastId

instance IsMTL4AccelerationStructureGeometryDescriptor (Id MTL4AccelerationStructureMotionCurveGeometryDescriptor) where
  toMTL4AccelerationStructureGeometryDescriptor = unsafeCastId

instance IsNSObject (Id MTL4AccelerationStructureMotionCurveGeometryDescriptor) where
  toNSObject = unsafeCastId

-- ---------- MTL4AccelerationStructureMotionTriangleGeometryDescriptor ----------

-- | Describes motion triangle geometry, suitable for motion ray tracing.
--
-- Use a ``MTLResidencySet`` to mark residency of all buffers this descriptor references when you build this acceleration structure.
-- 
-- Phantom type for @MTL4AccelerationStructureMotionTriangleGeometryDescriptor@.
data MTL4AccelerationStructureMotionTriangleGeometryDescriptor

instance IsObjCObject (Id MTL4AccelerationStructureMotionTriangleGeometryDescriptor) where
  toRawId = idToRawId
  withObjCPtr = withIdObjCPtr
  staticClass _ = getRequiredClass "MTL4AccelerationStructureMotionTriangleGeometryDescriptor"

class IsMTL4AccelerationStructureGeometryDescriptor a => IsMTL4AccelerationStructureMotionTriangleGeometryDescriptor a where
  toMTL4AccelerationStructureMotionTriangleGeometryDescriptor :: a -> Id MTL4AccelerationStructureMotionTriangleGeometryDescriptor

instance IsMTL4AccelerationStructureMotionTriangleGeometryDescriptor (Id MTL4AccelerationStructureMotionTriangleGeometryDescriptor) where
  toMTL4AccelerationStructureMotionTriangleGeometryDescriptor = unsafeCastId

instance IsMTL4AccelerationStructureGeometryDescriptor (Id MTL4AccelerationStructureMotionTriangleGeometryDescriptor) where
  toMTL4AccelerationStructureGeometryDescriptor = unsafeCastId

instance IsNSObject (Id MTL4AccelerationStructureMotionTriangleGeometryDescriptor) where
  toNSObject = unsafeCastId

-- ---------- MTL4AccelerationStructureTriangleGeometryDescriptor ----------

-- | Describes triangle geometry suitable for ray tracing.
--
-- Use a ``MTLResidencySet`` to mark residency of all buffers this descriptor references when you build this acceleration structure.
-- 
-- Phantom type for @MTL4AccelerationStructureTriangleGeometryDescriptor@.
data MTL4AccelerationStructureTriangleGeometryDescriptor

instance IsObjCObject (Id MTL4AccelerationStructureTriangleGeometryDescriptor) where
  toRawId = idToRawId
  withObjCPtr = withIdObjCPtr
  staticClass _ = getRequiredClass "MTL4AccelerationStructureTriangleGeometryDescriptor"

class IsMTL4AccelerationStructureGeometryDescriptor a => IsMTL4AccelerationStructureTriangleGeometryDescriptor a where
  toMTL4AccelerationStructureTriangleGeometryDescriptor :: a -> Id MTL4AccelerationStructureTriangleGeometryDescriptor

instance IsMTL4AccelerationStructureTriangleGeometryDescriptor (Id MTL4AccelerationStructureTriangleGeometryDescriptor) where
  toMTL4AccelerationStructureTriangleGeometryDescriptor = unsafeCastId

instance IsMTL4AccelerationStructureGeometryDescriptor (Id MTL4AccelerationStructureTriangleGeometryDescriptor) where
  toMTL4AccelerationStructureGeometryDescriptor = unsafeCastId

instance IsNSObject (Id MTL4AccelerationStructureTriangleGeometryDescriptor) where
  toNSObject = unsafeCastId

-- ---------- MTL4LibraryFunctionDescriptor ----------

-- | Describes a shader function from a Metal library.
-- 
-- Phantom type for @MTL4LibraryFunctionDescriptor@.
data MTL4LibraryFunctionDescriptor

instance IsObjCObject (Id MTL4LibraryFunctionDescriptor) where
  toRawId = idToRawId
  withObjCPtr = withIdObjCPtr
  staticClass _ = getRequiredClass "MTL4LibraryFunctionDescriptor"

class IsMTL4FunctionDescriptor a => IsMTL4LibraryFunctionDescriptor a where
  toMTL4LibraryFunctionDescriptor :: a -> Id MTL4LibraryFunctionDescriptor

instance IsMTL4LibraryFunctionDescriptor (Id MTL4LibraryFunctionDescriptor) where
  toMTL4LibraryFunctionDescriptor = unsafeCastId

instance IsMTL4FunctionDescriptor (Id MTL4LibraryFunctionDescriptor) where
  toMTL4FunctionDescriptor = unsafeCastId

instance IsNSObject (Id MTL4LibraryFunctionDescriptor) where
  toNSObject = unsafeCastId

-- ---------- MTL4SpecializedFunctionDescriptor ----------

-- | Groups together properties to configure and create a specialized function by passing it to a factory method.
--
-- You can pass an instance of this class to any methods that accept a ``MTL4FunctionDescriptor`` parameter to provide extra configuration, such as function constants or a name.
-- 
-- Phantom type for @MTL4SpecializedFunctionDescriptor@.
data MTL4SpecializedFunctionDescriptor

instance IsObjCObject (Id MTL4SpecializedFunctionDescriptor) where
  toRawId = idToRawId
  withObjCPtr = withIdObjCPtr
  staticClass _ = getRequiredClass "MTL4SpecializedFunctionDescriptor"

class IsMTL4FunctionDescriptor a => IsMTL4SpecializedFunctionDescriptor a where
  toMTL4SpecializedFunctionDescriptor :: a -> Id MTL4SpecializedFunctionDescriptor

instance IsMTL4SpecializedFunctionDescriptor (Id MTL4SpecializedFunctionDescriptor) where
  toMTL4SpecializedFunctionDescriptor = unsafeCastId

instance IsMTL4FunctionDescriptor (Id MTL4SpecializedFunctionDescriptor) where
  toMTL4FunctionDescriptor = unsafeCastId

instance IsNSObject (Id MTL4SpecializedFunctionDescriptor) where
  toNSObject = unsafeCastId

-- ---------- MTL4StitchedFunctionDescriptor ----------

-- | Groups together properties that describe a shader function suitable for stitching.
-- 
-- Phantom type for @MTL4StitchedFunctionDescriptor@.
data MTL4StitchedFunctionDescriptor

instance IsObjCObject (Id MTL4StitchedFunctionDescriptor) where
  toRawId = idToRawId
  withObjCPtr = withIdObjCPtr
  staticClass _ = getRequiredClass "MTL4StitchedFunctionDescriptor"

class IsMTL4FunctionDescriptor a => IsMTL4StitchedFunctionDescriptor a where
  toMTL4StitchedFunctionDescriptor :: a -> Id MTL4StitchedFunctionDescriptor

instance IsMTL4StitchedFunctionDescriptor (Id MTL4StitchedFunctionDescriptor) where
  toMTL4StitchedFunctionDescriptor = unsafeCastId

instance IsMTL4FunctionDescriptor (Id MTL4StitchedFunctionDescriptor) where
  toMTL4FunctionDescriptor = unsafeCastId

instance IsNSObject (Id MTL4StitchedFunctionDescriptor) where
  toNSObject = unsafeCastId

-- ---------- MTL4ComputePipelineDescriptor ----------

-- | Describes a compute pipeline state.
-- 
-- Phantom type for @MTL4ComputePipelineDescriptor@.
data MTL4ComputePipelineDescriptor

instance IsObjCObject (Id MTL4ComputePipelineDescriptor) where
  toRawId = idToRawId
  withObjCPtr = withIdObjCPtr
  staticClass _ = getRequiredClass "MTL4ComputePipelineDescriptor"

class IsMTL4PipelineDescriptor a => IsMTL4ComputePipelineDescriptor a where
  toMTL4ComputePipelineDescriptor :: a -> Id MTL4ComputePipelineDescriptor

instance IsMTL4ComputePipelineDescriptor (Id MTL4ComputePipelineDescriptor) where
  toMTL4ComputePipelineDescriptor = unsafeCastId

instance IsMTL4PipelineDescriptor (Id MTL4ComputePipelineDescriptor) where
  toMTL4PipelineDescriptor = unsafeCastId

instance IsNSObject (Id MTL4ComputePipelineDescriptor) where
  toNSObject = unsafeCastId

-- ---------- MTL4MachineLearningPipelineDescriptor ----------

-- | Description for a machine learning pipeline state.
-- 
-- Phantom type for @MTL4MachineLearningPipelineDescriptor@.
data MTL4MachineLearningPipelineDescriptor

instance IsObjCObject (Id MTL4MachineLearningPipelineDescriptor) where
  toRawId = idToRawId
  withObjCPtr = withIdObjCPtr
  staticClass _ = getRequiredClass "MTL4MachineLearningPipelineDescriptor"

class IsMTL4PipelineDescriptor a => IsMTL4MachineLearningPipelineDescriptor a where
  toMTL4MachineLearningPipelineDescriptor :: a -> Id MTL4MachineLearningPipelineDescriptor

instance IsMTL4MachineLearningPipelineDescriptor (Id MTL4MachineLearningPipelineDescriptor) where
  toMTL4MachineLearningPipelineDescriptor = unsafeCastId

instance IsMTL4PipelineDescriptor (Id MTL4MachineLearningPipelineDescriptor) where
  toMTL4PipelineDescriptor = unsafeCastId

instance IsNSObject (Id MTL4MachineLearningPipelineDescriptor) where
  toNSObject = unsafeCastId

-- ---------- MTL4MeshRenderPipelineDescriptor ----------

-- | Groups together properties you use to create a mesh render pipeline state object.
--
-- Compared to ``MTLMeshRenderPipelineDescriptor``, this interface doesn't offer a mechanism to hint to Metal mutability of object, mesh, or fragment buffers. Additionally, when you use this descriptor, you don't specify binary archives.
-- 
-- Phantom type for @MTL4MeshRenderPipelineDescriptor@.
data MTL4MeshRenderPipelineDescriptor

instance IsObjCObject (Id MTL4MeshRenderPipelineDescriptor) where
  toRawId = idToRawId
  withObjCPtr = withIdObjCPtr
  staticClass _ = getRequiredClass "MTL4MeshRenderPipelineDescriptor"

class IsMTL4PipelineDescriptor a => IsMTL4MeshRenderPipelineDescriptor a where
  toMTL4MeshRenderPipelineDescriptor :: a -> Id MTL4MeshRenderPipelineDescriptor

instance IsMTL4MeshRenderPipelineDescriptor (Id MTL4MeshRenderPipelineDescriptor) where
  toMTL4MeshRenderPipelineDescriptor = unsafeCastId

instance IsMTL4PipelineDescriptor (Id MTL4MeshRenderPipelineDescriptor) where
  toMTL4PipelineDescriptor = unsafeCastId

instance IsNSObject (Id MTL4MeshRenderPipelineDescriptor) where
  toNSObject = unsafeCastId

-- ---------- MTL4RenderPipelineDescriptor ----------

-- | Groups together properties to create a render pipeline state object.
--
-- Compared to ``MTLRenderPipelineDescriptor``, this interface doesn't offer a mechanism to hint to Metal mutability of vertex and fragment buffers. Additionally, using this descriptor, you don't specify binary archives.
-- 
-- Phantom type for @MTL4RenderPipelineDescriptor@.
data MTL4RenderPipelineDescriptor

instance IsObjCObject (Id MTL4RenderPipelineDescriptor) where
  toRawId = idToRawId
  withObjCPtr = withIdObjCPtr
  staticClass _ = getRequiredClass "MTL4RenderPipelineDescriptor"

class IsMTL4PipelineDescriptor a => IsMTL4RenderPipelineDescriptor a where
  toMTL4RenderPipelineDescriptor :: a -> Id MTL4RenderPipelineDescriptor

instance IsMTL4RenderPipelineDescriptor (Id MTL4RenderPipelineDescriptor) where
  toMTL4RenderPipelineDescriptor = unsafeCastId

instance IsMTL4PipelineDescriptor (Id MTL4RenderPipelineDescriptor) where
  toMTL4PipelineDescriptor = unsafeCastId

instance IsNSObject (Id MTL4RenderPipelineDescriptor) where
  toNSObject = unsafeCastId

-- ---------- MTL4TileRenderPipelineDescriptor ----------

-- | Groups together properties you use to create a tile render pipeline state object.
-- 
-- Phantom type for @MTL4TileRenderPipelineDescriptor@.
data MTL4TileRenderPipelineDescriptor

instance IsObjCObject (Id MTL4TileRenderPipelineDescriptor) where
  toRawId = idToRawId
  withObjCPtr = withIdObjCPtr
  staticClass _ = getRequiredClass "MTL4TileRenderPipelineDescriptor"

class IsMTL4PipelineDescriptor a => IsMTL4TileRenderPipelineDescriptor a where
  toMTL4TileRenderPipelineDescriptor :: a -> Id MTL4TileRenderPipelineDescriptor

instance IsMTL4TileRenderPipelineDescriptor (Id MTL4TileRenderPipelineDescriptor) where
  toMTL4TileRenderPipelineDescriptor = unsafeCastId

instance IsMTL4PipelineDescriptor (Id MTL4TileRenderPipelineDescriptor) where
  toMTL4PipelineDescriptor = unsafeCastId

instance IsNSObject (Id MTL4TileRenderPipelineDescriptor) where
  toNSObject = unsafeCastId

-- ---------- MTL4AccelerationStructureDescriptor ----------

-- | Base class for Metal 4 acceleration structure descriptors.
--
-- Don't use this class directly. Use one of its subclasses instead.
-- 
-- Phantom type for @MTL4AccelerationStructureDescriptor@.
data MTL4AccelerationStructureDescriptor

instance IsObjCObject (Id MTL4AccelerationStructureDescriptor) where
  toRawId = idToRawId
  withObjCPtr = withIdObjCPtr
  staticClass _ = getRequiredClass "MTL4AccelerationStructureDescriptor"

class IsMTLAccelerationStructureDescriptor a => IsMTL4AccelerationStructureDescriptor a where
  toMTL4AccelerationStructureDescriptor :: a -> Id MTL4AccelerationStructureDescriptor

instance IsMTL4AccelerationStructureDescriptor (Id MTL4AccelerationStructureDescriptor) where
  toMTL4AccelerationStructureDescriptor = unsafeCastId

instance IsMTLAccelerationStructureDescriptor (Id MTL4AccelerationStructureDescriptor) where
  toMTLAccelerationStructureDescriptor = unsafeCastId

instance IsNSObject (Id MTL4AccelerationStructureDescriptor) where
  toNSObject = unsafeCastId

-- ---------- MTLIndirectInstanceAccelerationStructureDescriptor ----------

-- | Descriptor for an instance acceleration structure built with an indirected buffer of instances.
-- 
-- Phantom type for @MTLIndirectInstanceAccelerationStructureDescriptor@.
data MTLIndirectInstanceAccelerationStructureDescriptor

instance IsObjCObject (Id MTLIndirectInstanceAccelerationStructureDescriptor) where
  toRawId = idToRawId
  withObjCPtr = withIdObjCPtr
  staticClass _ = getRequiredClass "MTLIndirectInstanceAccelerationStructureDescriptor"

class IsMTLAccelerationStructureDescriptor a => IsMTLIndirectInstanceAccelerationStructureDescriptor a where
  toMTLIndirectInstanceAccelerationStructureDescriptor :: a -> Id MTLIndirectInstanceAccelerationStructureDescriptor

instance IsMTLIndirectInstanceAccelerationStructureDescriptor (Id MTLIndirectInstanceAccelerationStructureDescriptor) where
  toMTLIndirectInstanceAccelerationStructureDescriptor = unsafeCastId

instance IsMTLAccelerationStructureDescriptor (Id MTLIndirectInstanceAccelerationStructureDescriptor) where
  toMTLAccelerationStructureDescriptor = unsafeCastId

instance IsNSObject (Id MTLIndirectInstanceAccelerationStructureDescriptor) where
  toNSObject = unsafeCastId

-- ---------- MTLInstanceAccelerationStructureDescriptor ----------

-- | Descriptor for an instance acceleration structure
-- 
-- Phantom type for @MTLInstanceAccelerationStructureDescriptor@.
data MTLInstanceAccelerationStructureDescriptor

instance IsObjCObject (Id MTLInstanceAccelerationStructureDescriptor) where
  toRawId = idToRawId
  withObjCPtr = withIdObjCPtr
  staticClass _ = getRequiredClass "MTLInstanceAccelerationStructureDescriptor"

class IsMTLAccelerationStructureDescriptor a => IsMTLInstanceAccelerationStructureDescriptor a where
  toMTLInstanceAccelerationStructureDescriptor :: a -> Id MTLInstanceAccelerationStructureDescriptor

instance IsMTLInstanceAccelerationStructureDescriptor (Id MTLInstanceAccelerationStructureDescriptor) where
  toMTLInstanceAccelerationStructureDescriptor = unsafeCastId

instance IsMTLAccelerationStructureDescriptor (Id MTLInstanceAccelerationStructureDescriptor) where
  toMTLAccelerationStructureDescriptor = unsafeCastId

instance IsNSObject (Id MTLInstanceAccelerationStructureDescriptor) where
  toNSObject = unsafeCastId

-- ---------- MTLPrimitiveAccelerationStructureDescriptor ----------

-- | Descriptor for a primitive acceleration structure
-- 
-- Phantom type for @MTLPrimitiveAccelerationStructureDescriptor@.
data MTLPrimitiveAccelerationStructureDescriptor

instance IsObjCObject (Id MTLPrimitiveAccelerationStructureDescriptor) where
  toRawId = idToRawId
  withObjCPtr = withIdObjCPtr
  staticClass _ = getRequiredClass "MTLPrimitiveAccelerationStructureDescriptor"

class IsMTLAccelerationStructureDescriptor a => IsMTLPrimitiveAccelerationStructureDescriptor a where
  toMTLPrimitiveAccelerationStructureDescriptor :: a -> Id MTLPrimitiveAccelerationStructureDescriptor

instance IsMTLPrimitiveAccelerationStructureDescriptor (Id MTLPrimitiveAccelerationStructureDescriptor) where
  toMTLPrimitiveAccelerationStructureDescriptor = unsafeCastId

instance IsMTLAccelerationStructureDescriptor (Id MTLPrimitiveAccelerationStructureDescriptor) where
  toMTLAccelerationStructureDescriptor = unsafeCastId

instance IsNSObject (Id MTLPrimitiveAccelerationStructureDescriptor) where
  toNSObject = unsafeCastId

-- ---------- MTLAccelerationStructureBoundingBoxGeometryDescriptor ----------

-- | Descriptor for bounding box geometry
-- 
-- Phantom type for @MTLAccelerationStructureBoundingBoxGeometryDescriptor@.
data MTLAccelerationStructureBoundingBoxGeometryDescriptor

instance IsObjCObject (Id MTLAccelerationStructureBoundingBoxGeometryDescriptor) where
  toRawId = idToRawId
  withObjCPtr = withIdObjCPtr
  staticClass _ = getRequiredClass "MTLAccelerationStructureBoundingBoxGeometryDescriptor"

class IsMTLAccelerationStructureGeometryDescriptor a => IsMTLAccelerationStructureBoundingBoxGeometryDescriptor a where
  toMTLAccelerationStructureBoundingBoxGeometryDescriptor :: a -> Id MTLAccelerationStructureBoundingBoxGeometryDescriptor

instance IsMTLAccelerationStructureBoundingBoxGeometryDescriptor (Id MTLAccelerationStructureBoundingBoxGeometryDescriptor) where
  toMTLAccelerationStructureBoundingBoxGeometryDescriptor = unsafeCastId

instance IsMTLAccelerationStructureGeometryDescriptor (Id MTLAccelerationStructureBoundingBoxGeometryDescriptor) where
  toMTLAccelerationStructureGeometryDescriptor = unsafeCastId

instance IsNSObject (Id MTLAccelerationStructureBoundingBoxGeometryDescriptor) where
  toNSObject = unsafeCastId

-- ---------- MTLAccelerationStructureCurveGeometryDescriptor ----------

-- | Acceleration structure geometry descriptor describing geometry made of curve primitives
-- 
-- Phantom type for @MTLAccelerationStructureCurveGeometryDescriptor@.
data MTLAccelerationStructureCurveGeometryDescriptor

instance IsObjCObject (Id MTLAccelerationStructureCurveGeometryDescriptor) where
  toRawId = idToRawId
  withObjCPtr = withIdObjCPtr
  staticClass _ = getRequiredClass "MTLAccelerationStructureCurveGeometryDescriptor"

class IsMTLAccelerationStructureGeometryDescriptor a => IsMTLAccelerationStructureCurveGeometryDescriptor a where
  toMTLAccelerationStructureCurveGeometryDescriptor :: a -> Id MTLAccelerationStructureCurveGeometryDescriptor

instance IsMTLAccelerationStructureCurveGeometryDescriptor (Id MTLAccelerationStructureCurveGeometryDescriptor) where
  toMTLAccelerationStructureCurveGeometryDescriptor = unsafeCastId

instance IsMTLAccelerationStructureGeometryDescriptor (Id MTLAccelerationStructureCurveGeometryDescriptor) where
  toMTLAccelerationStructureGeometryDescriptor = unsafeCastId

instance IsNSObject (Id MTLAccelerationStructureCurveGeometryDescriptor) where
  toNSObject = unsafeCastId

-- ---------- MTLAccelerationStructureMotionBoundingBoxGeometryDescriptor ----------

-- | Descriptor for motion bounding box geometry
-- 
-- Phantom type for @MTLAccelerationStructureMotionBoundingBoxGeometryDescriptor@.
data MTLAccelerationStructureMotionBoundingBoxGeometryDescriptor

instance IsObjCObject (Id MTLAccelerationStructureMotionBoundingBoxGeometryDescriptor) where
  toRawId = idToRawId
  withObjCPtr = withIdObjCPtr
  staticClass _ = getRequiredClass "MTLAccelerationStructureMotionBoundingBoxGeometryDescriptor"

class IsMTLAccelerationStructureGeometryDescriptor a => IsMTLAccelerationStructureMotionBoundingBoxGeometryDescriptor a where
  toMTLAccelerationStructureMotionBoundingBoxGeometryDescriptor :: a -> Id MTLAccelerationStructureMotionBoundingBoxGeometryDescriptor

instance IsMTLAccelerationStructureMotionBoundingBoxGeometryDescriptor (Id MTLAccelerationStructureMotionBoundingBoxGeometryDescriptor) where
  toMTLAccelerationStructureMotionBoundingBoxGeometryDescriptor = unsafeCastId

instance IsMTLAccelerationStructureGeometryDescriptor (Id MTLAccelerationStructureMotionBoundingBoxGeometryDescriptor) where
  toMTLAccelerationStructureGeometryDescriptor = unsafeCastId

instance IsNSObject (Id MTLAccelerationStructureMotionBoundingBoxGeometryDescriptor) where
  toNSObject = unsafeCastId

-- ---------- MTLAccelerationStructureMotionCurveGeometryDescriptor ----------

-- | Acceleration structure motion geometry descriptor describing geometry made of curve primitives
-- 
-- Phantom type for @MTLAccelerationStructureMotionCurveGeometryDescriptor@.
data MTLAccelerationStructureMotionCurveGeometryDescriptor

instance IsObjCObject (Id MTLAccelerationStructureMotionCurveGeometryDescriptor) where
  toRawId = idToRawId
  withObjCPtr = withIdObjCPtr
  staticClass _ = getRequiredClass "MTLAccelerationStructureMotionCurveGeometryDescriptor"

class IsMTLAccelerationStructureGeometryDescriptor a => IsMTLAccelerationStructureMotionCurveGeometryDescriptor a where
  toMTLAccelerationStructureMotionCurveGeometryDescriptor :: a -> Id MTLAccelerationStructureMotionCurveGeometryDescriptor

instance IsMTLAccelerationStructureMotionCurveGeometryDescriptor (Id MTLAccelerationStructureMotionCurveGeometryDescriptor) where
  toMTLAccelerationStructureMotionCurveGeometryDescriptor = unsafeCastId

instance IsMTLAccelerationStructureGeometryDescriptor (Id MTLAccelerationStructureMotionCurveGeometryDescriptor) where
  toMTLAccelerationStructureGeometryDescriptor = unsafeCastId

instance IsNSObject (Id MTLAccelerationStructureMotionCurveGeometryDescriptor) where
  toNSObject = unsafeCastId

-- ---------- MTLAccelerationStructureMotionTriangleGeometryDescriptor ----------

-- | Descriptor for motion triangle geometry
-- 
-- Phantom type for @MTLAccelerationStructureMotionTriangleGeometryDescriptor@.
data MTLAccelerationStructureMotionTriangleGeometryDescriptor

instance IsObjCObject (Id MTLAccelerationStructureMotionTriangleGeometryDescriptor) where
  toRawId = idToRawId
  withObjCPtr = withIdObjCPtr
  staticClass _ = getRequiredClass "MTLAccelerationStructureMotionTriangleGeometryDescriptor"

class IsMTLAccelerationStructureGeometryDescriptor a => IsMTLAccelerationStructureMotionTriangleGeometryDescriptor a where
  toMTLAccelerationStructureMotionTriangleGeometryDescriptor :: a -> Id MTLAccelerationStructureMotionTriangleGeometryDescriptor

instance IsMTLAccelerationStructureMotionTriangleGeometryDescriptor (Id MTLAccelerationStructureMotionTriangleGeometryDescriptor) where
  toMTLAccelerationStructureMotionTriangleGeometryDescriptor = unsafeCastId

instance IsMTLAccelerationStructureGeometryDescriptor (Id MTLAccelerationStructureMotionTriangleGeometryDescriptor) where
  toMTLAccelerationStructureGeometryDescriptor = unsafeCastId

instance IsNSObject (Id MTLAccelerationStructureMotionTriangleGeometryDescriptor) where
  toNSObject = unsafeCastId

-- ---------- MTLAccelerationStructureTriangleGeometryDescriptor ----------

-- | Descriptor for triangle geometry
-- 
-- Phantom type for @MTLAccelerationStructureTriangleGeometryDescriptor@.
data MTLAccelerationStructureTriangleGeometryDescriptor

instance IsObjCObject (Id MTLAccelerationStructureTriangleGeometryDescriptor) where
  toRawId = idToRawId
  withObjCPtr = withIdObjCPtr
  staticClass _ = getRequiredClass "MTLAccelerationStructureTriangleGeometryDescriptor"

class IsMTLAccelerationStructureGeometryDescriptor a => IsMTLAccelerationStructureTriangleGeometryDescriptor a where
  toMTLAccelerationStructureTriangleGeometryDescriptor :: a -> Id MTLAccelerationStructureTriangleGeometryDescriptor

instance IsMTLAccelerationStructureTriangleGeometryDescriptor (Id MTLAccelerationStructureTriangleGeometryDescriptor) where
  toMTLAccelerationStructureTriangleGeometryDescriptor = unsafeCastId

instance IsMTLAccelerationStructureGeometryDescriptor (Id MTLAccelerationStructureTriangleGeometryDescriptor) where
  toMTLAccelerationStructureGeometryDescriptor = unsafeCastId

instance IsNSObject (Id MTLAccelerationStructureTriangleGeometryDescriptor) where
  toNSObject = unsafeCastId

-- ---------- MTLIntersectionFunctionDescriptor ----------

-- | Phantom type for @MTLIntersectionFunctionDescriptor@.
data MTLIntersectionFunctionDescriptor

instance IsObjCObject (Id MTLIntersectionFunctionDescriptor) where
  toRawId = idToRawId
  withObjCPtr = withIdObjCPtr
  staticClass _ = getRequiredClass "MTLIntersectionFunctionDescriptor"

class IsMTLFunctionDescriptor a => IsMTLIntersectionFunctionDescriptor a where
  toMTLIntersectionFunctionDescriptor :: a -> Id MTLIntersectionFunctionDescriptor

instance IsMTLIntersectionFunctionDescriptor (Id MTLIntersectionFunctionDescriptor) where
  toMTLIntersectionFunctionDescriptor = unsafeCastId

instance IsMTLFunctionDescriptor (Id MTLIntersectionFunctionDescriptor) where
  toMTLFunctionDescriptor = unsafeCastId

instance IsNSObject (Id MTLIntersectionFunctionDescriptor) where
  toNSObject = unsafeCastId

-- ---------- MTLRenderPassColorAttachmentDescriptor ----------

-- | Phantom type for @MTLRenderPassColorAttachmentDescriptor@.
data MTLRenderPassColorAttachmentDescriptor

instance IsObjCObject (Id MTLRenderPassColorAttachmentDescriptor) where
  toRawId = idToRawId
  withObjCPtr = withIdObjCPtr
  staticClass _ = getRequiredClass "MTLRenderPassColorAttachmentDescriptor"

class IsMTLRenderPassAttachmentDescriptor a => IsMTLRenderPassColorAttachmentDescriptor a where
  toMTLRenderPassColorAttachmentDescriptor :: a -> Id MTLRenderPassColorAttachmentDescriptor

instance IsMTLRenderPassColorAttachmentDescriptor (Id MTLRenderPassColorAttachmentDescriptor) where
  toMTLRenderPassColorAttachmentDescriptor = unsafeCastId

instance IsMTLRenderPassAttachmentDescriptor (Id MTLRenderPassColorAttachmentDescriptor) where
  toMTLRenderPassAttachmentDescriptor = unsafeCastId

instance IsNSObject (Id MTLRenderPassColorAttachmentDescriptor) where
  toNSObject = unsafeCastId

-- ---------- MTLRenderPassDepthAttachmentDescriptor ----------

-- | Phantom type for @MTLRenderPassDepthAttachmentDescriptor@.
data MTLRenderPassDepthAttachmentDescriptor

instance IsObjCObject (Id MTLRenderPassDepthAttachmentDescriptor) where
  toRawId = idToRawId
  withObjCPtr = withIdObjCPtr
  staticClass _ = getRequiredClass "MTLRenderPassDepthAttachmentDescriptor"

class IsMTLRenderPassAttachmentDescriptor a => IsMTLRenderPassDepthAttachmentDescriptor a where
  toMTLRenderPassDepthAttachmentDescriptor :: a -> Id MTLRenderPassDepthAttachmentDescriptor

instance IsMTLRenderPassDepthAttachmentDescriptor (Id MTLRenderPassDepthAttachmentDescriptor) where
  toMTLRenderPassDepthAttachmentDescriptor = unsafeCastId

instance IsMTLRenderPassAttachmentDescriptor (Id MTLRenderPassDepthAttachmentDescriptor) where
  toMTLRenderPassAttachmentDescriptor = unsafeCastId

instance IsNSObject (Id MTLRenderPassDepthAttachmentDescriptor) where
  toNSObject = unsafeCastId

-- ---------- MTLRenderPassStencilAttachmentDescriptor ----------

-- | Phantom type for @MTLRenderPassStencilAttachmentDescriptor@.
data MTLRenderPassStencilAttachmentDescriptor

instance IsObjCObject (Id MTLRenderPassStencilAttachmentDescriptor) where
  toRawId = idToRawId
  withObjCPtr = withIdObjCPtr
  staticClass _ = getRequiredClass "MTLRenderPassStencilAttachmentDescriptor"

class IsMTLRenderPassAttachmentDescriptor a => IsMTLRenderPassStencilAttachmentDescriptor a where
  toMTLRenderPassStencilAttachmentDescriptor :: a -> Id MTLRenderPassStencilAttachmentDescriptor

instance IsMTLRenderPassStencilAttachmentDescriptor (Id MTLRenderPassStencilAttachmentDescriptor) where
  toMTLRenderPassStencilAttachmentDescriptor = unsafeCastId

instance IsMTLRenderPassAttachmentDescriptor (Id MTLRenderPassStencilAttachmentDescriptor) where
  toMTLRenderPassAttachmentDescriptor = unsafeCastId

instance IsNSObject (Id MTLRenderPassStencilAttachmentDescriptor) where
  toNSObject = unsafeCastId

-- ---------- MTLArrayType ----------

-- | Phantom type for @MTLArrayType@.
data MTLArrayType

instance IsObjCObject (Id MTLArrayType) where
  toRawId = idToRawId
  withObjCPtr = withIdObjCPtr
  staticClass _ = getRequiredClass "MTLArrayType"

class IsMTLType a => IsMTLArrayType a where
  toMTLArrayType :: a -> Id MTLArrayType

instance IsMTLArrayType (Id MTLArrayType) where
  toMTLArrayType = unsafeCastId

instance IsMTLType (Id MTLArrayType) where
  toMTLType = unsafeCastId

instance IsNSObject (Id MTLArrayType) where
  toNSObject = unsafeCastId

-- ---------- MTLPointerType ----------

-- | Phantom type for @MTLPointerType@.
data MTLPointerType

instance IsObjCObject (Id MTLPointerType) where
  toRawId = idToRawId
  withObjCPtr = withIdObjCPtr
  staticClass _ = getRequiredClass "MTLPointerType"

class IsMTLType a => IsMTLPointerType a where
  toMTLPointerType :: a -> Id MTLPointerType

instance IsMTLPointerType (Id MTLPointerType) where
  toMTLPointerType = unsafeCastId

instance IsMTLType (Id MTLPointerType) where
  toMTLType = unsafeCastId

instance IsNSObject (Id MTLPointerType) where
  toNSObject = unsafeCastId

-- ---------- MTLStructType ----------

-- | Phantom type for @MTLStructType@.
data MTLStructType

instance IsObjCObject (Id MTLStructType) where
  toRawId = idToRawId
  withObjCPtr = withIdObjCPtr
  staticClass _ = getRequiredClass "MTLStructType"

class IsMTLType a => IsMTLStructType a where
  toMTLStructType :: a -> Id MTLStructType

instance IsMTLStructType (Id MTLStructType) where
  toMTLStructType = unsafeCastId

instance IsMTLType (Id MTLStructType) where
  toMTLType = unsafeCastId

instance IsNSObject (Id MTLStructType) where
  toNSObject = unsafeCastId

-- ---------- MTLTensorReferenceType ----------

-- | An object that represents a tensor in the shading language in a struct or array.
-- 
-- Phantom type for @MTLTensorReferenceType@.
data MTLTensorReferenceType

instance IsObjCObject (Id MTLTensorReferenceType) where
  toRawId = idToRawId
  withObjCPtr = withIdObjCPtr
  staticClass _ = getRequiredClass "MTLTensorReferenceType"

class IsMTLType a => IsMTLTensorReferenceType a where
  toMTLTensorReferenceType :: a -> Id MTLTensorReferenceType

instance IsMTLTensorReferenceType (Id MTLTensorReferenceType) where
  toMTLTensorReferenceType = unsafeCastId

instance IsMTLType (Id MTLTensorReferenceType) where
  toMTLType = unsafeCastId

instance IsNSObject (Id MTLTensorReferenceType) where
  toNSObject = unsafeCastId

-- ---------- MTLTextureReferenceType ----------

-- | Phantom type for @MTLTextureReferenceType@.
data MTLTextureReferenceType

instance IsObjCObject (Id MTLTextureReferenceType) where
  toRawId = idToRawId
  withObjCPtr = withIdObjCPtr
  staticClass _ = getRequiredClass "MTLTextureReferenceType"

class IsMTLType a => IsMTLTextureReferenceType a where
  toMTLTextureReferenceType :: a -> Id MTLTextureReferenceType

instance IsMTLTextureReferenceType (Id MTLTextureReferenceType) where
  toMTLTextureReferenceType = unsafeCastId

instance IsMTLType (Id MTLTextureReferenceType) where
  toMTLType = unsafeCastId

instance IsNSObject (Id MTLTextureReferenceType) where
  toNSObject = unsafeCastId

-- ---------- MTL4IndirectInstanceAccelerationStructureDescriptor ----------

-- | Descriptor for an "indirect" instance acceleration structure that allows providing the instance count and motion transform count indirectly, through buffer references.
--
-- An instance acceleration structure references other acceleration structures, and provides the ability to "instantiate" them multiple times, each one with potentially a different transformation matrix.
--
-- You specify the properties of the instances in the acceleration structure this descriptor builds by providing a buffer of @structs@ via its ``instanceDescriptorBuffer`` property.
--
-- Compared to ``MTL4InstanceAccelerationStructureDescriptor``, this descriptor allows you to provide the number of instances it references indirectly through a buffer reference, as well as the number of motion transforms.
--
-- This enables you to determine these counts indirectly in the GPU timeline via a compute pipeline. Metal needs only to know the maximum possible number of instances and motion transforms to support, which you specify via the ``maxInstanceCount`` and ``maxMotionTransformCount`` properties.
--
-- Use a ``MTLResidencySet`` to mark residency of all buffers and acceleration structures this descriptor references when you build this acceleration structure.
-- 
-- Phantom type for @MTL4IndirectInstanceAccelerationStructureDescriptor@.
data MTL4IndirectInstanceAccelerationStructureDescriptor

instance IsObjCObject (Id MTL4IndirectInstanceAccelerationStructureDescriptor) where
  toRawId = idToRawId
  withObjCPtr = withIdObjCPtr
  staticClass _ = getRequiredClass "MTL4IndirectInstanceAccelerationStructureDescriptor"

class IsMTL4AccelerationStructureDescriptor a => IsMTL4IndirectInstanceAccelerationStructureDescriptor a where
  toMTL4IndirectInstanceAccelerationStructureDescriptor :: a -> Id MTL4IndirectInstanceAccelerationStructureDescriptor

instance IsMTL4IndirectInstanceAccelerationStructureDescriptor (Id MTL4IndirectInstanceAccelerationStructureDescriptor) where
  toMTL4IndirectInstanceAccelerationStructureDescriptor = unsafeCastId

instance IsMTL4AccelerationStructureDescriptor (Id MTL4IndirectInstanceAccelerationStructureDescriptor) where
  toMTL4AccelerationStructureDescriptor = unsafeCastId

instance IsMTLAccelerationStructureDescriptor (Id MTL4IndirectInstanceAccelerationStructureDescriptor) where
  toMTLAccelerationStructureDescriptor = unsafeCastId

instance IsNSObject (Id MTL4IndirectInstanceAccelerationStructureDescriptor) where
  toNSObject = unsafeCastId

-- ---------- MTL4InstanceAccelerationStructureDescriptor ----------

-- | Descriptor for an instance acceleration structure.
--
-- An instance acceleration structure references other acceleration structures, and provides the ability to "instantiate" them multiple times, each one with potentially a different transformation matrix.
--
-- You specify the properties of the instances in the acceleration structure this descriptor builds by providing a buffer of @structs@ via its ``instanceDescriptorBuffer`` property.
--
-- Use a ``MTLResidencySet`` to mark residency of all buffers and acceleration structures this descriptor references when you build this acceleration structure.
-- 
-- Phantom type for @MTL4InstanceAccelerationStructureDescriptor@.
data MTL4InstanceAccelerationStructureDescriptor

instance IsObjCObject (Id MTL4InstanceAccelerationStructureDescriptor) where
  toRawId = idToRawId
  withObjCPtr = withIdObjCPtr
  staticClass _ = getRequiredClass "MTL4InstanceAccelerationStructureDescriptor"

class IsMTL4AccelerationStructureDescriptor a => IsMTL4InstanceAccelerationStructureDescriptor a where
  toMTL4InstanceAccelerationStructureDescriptor :: a -> Id MTL4InstanceAccelerationStructureDescriptor

instance IsMTL4InstanceAccelerationStructureDescriptor (Id MTL4InstanceAccelerationStructureDescriptor) where
  toMTL4InstanceAccelerationStructureDescriptor = unsafeCastId

instance IsMTL4AccelerationStructureDescriptor (Id MTL4InstanceAccelerationStructureDescriptor) where
  toMTL4AccelerationStructureDescriptor = unsafeCastId

instance IsMTLAccelerationStructureDescriptor (Id MTL4InstanceAccelerationStructureDescriptor) where
  toMTLAccelerationStructureDescriptor = unsafeCastId

instance IsNSObject (Id MTL4InstanceAccelerationStructureDescriptor) where
  toNSObject = unsafeCastId

-- ---------- MTL4PrimitiveAccelerationStructureDescriptor ----------

-- | Descriptor for a primitive acceleration structure that directly references geometric shapes, such as triangles and bounding boxes.
-- 
-- Phantom type for @MTL4PrimitiveAccelerationStructureDescriptor@.
data MTL4PrimitiveAccelerationStructureDescriptor

instance IsObjCObject (Id MTL4PrimitiveAccelerationStructureDescriptor) where
  toRawId = idToRawId
  withObjCPtr = withIdObjCPtr
  staticClass _ = getRequiredClass "MTL4PrimitiveAccelerationStructureDescriptor"

class IsMTL4AccelerationStructureDescriptor a => IsMTL4PrimitiveAccelerationStructureDescriptor a where
  toMTL4PrimitiveAccelerationStructureDescriptor :: a -> Id MTL4PrimitiveAccelerationStructureDescriptor

instance IsMTL4PrimitiveAccelerationStructureDescriptor (Id MTL4PrimitiveAccelerationStructureDescriptor) where
  toMTL4PrimitiveAccelerationStructureDescriptor = unsafeCastId

instance IsMTL4AccelerationStructureDescriptor (Id MTL4PrimitiveAccelerationStructureDescriptor) where
  toMTL4AccelerationStructureDescriptor = unsafeCastId

instance IsMTLAccelerationStructureDescriptor (Id MTL4PrimitiveAccelerationStructureDescriptor) where
  toMTLAccelerationStructureDescriptor = unsafeCastId

instance IsNSObject (Id MTL4PrimitiveAccelerationStructureDescriptor) where
  toNSObject = unsafeCastId
