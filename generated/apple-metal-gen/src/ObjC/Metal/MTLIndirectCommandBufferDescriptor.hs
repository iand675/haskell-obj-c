{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Describes the limits and features that can be used in an indirect command
--
-- Generated bindings for @MTLIndirectCommandBufferDescriptor@.
module ObjC.Metal.MTLIndirectCommandBufferDescriptor
  ( MTLIndirectCommandBufferDescriptor
  , IsMTLIndirectCommandBufferDescriptor(..)
  , commandTypes
  , setCommandTypes
  , inheritPipelineState
  , setInheritPipelineState
  , inheritBuffers
  , setInheritBuffers
  , inheritDepthStencilState
  , setInheritDepthStencilState
  , inheritDepthBias
  , setInheritDepthBias
  , inheritDepthClipMode
  , setInheritDepthClipMode
  , inheritCullMode
  , setInheritCullMode
  , inheritFrontFacingWinding
  , setInheritFrontFacingWinding
  , inheritTriangleFillMode
  , setInheritTriangleFillMode
  , maxVertexBufferBindCount
  , setMaxVertexBufferBindCount
  , maxFragmentBufferBindCount
  , setMaxFragmentBufferBindCount
  , maxKernelBufferBindCount
  , setMaxKernelBufferBindCount
  , maxKernelThreadgroupMemoryBindCount
  , setMaxKernelThreadgroupMemoryBindCount
  , maxObjectBufferBindCount
  , setMaxObjectBufferBindCount
  , maxMeshBufferBindCount
  , setMaxMeshBufferBindCount
  , maxObjectThreadgroupMemoryBindCount
  , setMaxObjectThreadgroupMemoryBindCount
  , supportRayTracing
  , setSupportRayTracing
  , supportDynamicAttributeStride
  , setSupportDynamicAttributeStride
  , supportColorAttachmentMapping
  , setSupportColorAttachmentMapping
  , commandTypesSelector
  , inheritBuffersSelector
  , inheritCullModeSelector
  , inheritDepthBiasSelector
  , inheritDepthClipModeSelector
  , inheritDepthStencilStateSelector
  , inheritFrontFacingWindingSelector
  , inheritPipelineStateSelector
  , inheritTriangleFillModeSelector
  , maxFragmentBufferBindCountSelector
  , maxKernelBufferBindCountSelector
  , maxKernelThreadgroupMemoryBindCountSelector
  , maxMeshBufferBindCountSelector
  , maxObjectBufferBindCountSelector
  , maxObjectThreadgroupMemoryBindCountSelector
  , maxVertexBufferBindCountSelector
  , setCommandTypesSelector
  , setInheritBuffersSelector
  , setInheritCullModeSelector
  , setInheritDepthBiasSelector
  , setInheritDepthClipModeSelector
  , setInheritDepthStencilStateSelector
  , setInheritFrontFacingWindingSelector
  , setInheritPipelineStateSelector
  , setInheritTriangleFillModeSelector
  , setMaxFragmentBufferBindCountSelector
  , setMaxKernelBufferBindCountSelector
  , setMaxKernelThreadgroupMemoryBindCountSelector
  , setMaxMeshBufferBindCountSelector
  , setMaxObjectBufferBindCountSelector
  , setMaxObjectThreadgroupMemoryBindCountSelector
  , setMaxVertexBufferBindCountSelector
  , setSupportColorAttachmentMappingSelector
  , setSupportDynamicAttributeStrideSelector
  , setSupportRayTracingSelector
  , supportColorAttachmentMappingSelector
  , supportDynamicAttributeStrideSelector
  , supportRayTracingSelector

  -- * Enum types
  , MTLIndirectCommandType(MTLIndirectCommandType)
  , pattern MTLIndirectCommandTypeDraw
  , pattern MTLIndirectCommandTypeDrawIndexed
  , pattern MTLIndirectCommandTypeDrawPatches
  , pattern MTLIndirectCommandTypeDrawIndexedPatches
  , pattern MTLIndirectCommandTypeConcurrentDispatch
  , pattern MTLIndirectCommandTypeConcurrentDispatchThreads
  , pattern MTLIndirectCommandTypeDrawMeshThreadgroups
  , pattern MTLIndirectCommandTypeDrawMeshThreads

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

-- | A bitfield of the command types that be encoded.
--
-- MTLCommandTypeDispatch cannot be mixed with any other command type.
--
-- ObjC selector: @- commandTypes@
commandTypes :: IsMTLIndirectCommandBufferDescriptor mtlIndirectCommandBufferDescriptor => mtlIndirectCommandBufferDescriptor -> IO MTLIndirectCommandType
commandTypes mtlIndirectCommandBufferDescriptor =
  sendMessage mtlIndirectCommandBufferDescriptor commandTypesSelector

-- | A bitfield of the command types that be encoded.
--
-- MTLCommandTypeDispatch cannot be mixed with any other command type.
--
-- ObjC selector: @- setCommandTypes:@
setCommandTypes :: IsMTLIndirectCommandBufferDescriptor mtlIndirectCommandBufferDescriptor => mtlIndirectCommandBufferDescriptor -> MTLIndirectCommandType -> IO ()
setCommandTypes mtlIndirectCommandBufferDescriptor value =
  sendMessage mtlIndirectCommandBufferDescriptor setCommandTypesSelector value

-- | Whether the render or compute pipeline are inherited from the encoder
--
-- ObjC selector: @- inheritPipelineState@
inheritPipelineState :: IsMTLIndirectCommandBufferDescriptor mtlIndirectCommandBufferDescriptor => mtlIndirectCommandBufferDescriptor -> IO Bool
inheritPipelineState mtlIndirectCommandBufferDescriptor =
  sendMessage mtlIndirectCommandBufferDescriptor inheritPipelineStateSelector

-- | Whether the render or compute pipeline are inherited from the encoder
--
-- ObjC selector: @- setInheritPipelineState:@
setInheritPipelineState :: IsMTLIndirectCommandBufferDescriptor mtlIndirectCommandBufferDescriptor => mtlIndirectCommandBufferDescriptor -> Bool -> IO ()
setInheritPipelineState mtlIndirectCommandBufferDescriptor value =
  sendMessage mtlIndirectCommandBufferDescriptor setInheritPipelineStateSelector value

-- | Whether the render or compute pipeline can set arguments.
--
-- ObjC selector: @- inheritBuffers@
inheritBuffers :: IsMTLIndirectCommandBufferDescriptor mtlIndirectCommandBufferDescriptor => mtlIndirectCommandBufferDescriptor -> IO Bool
inheritBuffers mtlIndirectCommandBufferDescriptor =
  sendMessage mtlIndirectCommandBufferDescriptor inheritBuffersSelector

-- | Whether the render or compute pipeline can set arguments.
--
-- ObjC selector: @- setInheritBuffers:@
setInheritBuffers :: IsMTLIndirectCommandBufferDescriptor mtlIndirectCommandBufferDescriptor => mtlIndirectCommandBufferDescriptor -> Bool -> IO ()
setInheritBuffers mtlIndirectCommandBufferDescriptor value =
  sendMessage mtlIndirectCommandBufferDescriptor setInheritBuffersSelector value

-- | Configures whether the indirect command buffer inherits the depth stencil state from the encoder.
--
-- The property's default value is <doc://com.apple.documentation/documentation/swift/true>.
--
-- ObjC selector: @- inheritDepthStencilState@
inheritDepthStencilState :: IsMTLIndirectCommandBufferDescriptor mtlIndirectCommandBufferDescriptor => mtlIndirectCommandBufferDescriptor -> IO Bool
inheritDepthStencilState mtlIndirectCommandBufferDescriptor =
  sendMessage mtlIndirectCommandBufferDescriptor inheritDepthStencilStateSelector

-- | Configures whether the indirect command buffer inherits the depth stencil state from the encoder.
--
-- The property's default value is <doc://com.apple.documentation/documentation/swift/true>.
--
-- ObjC selector: @- setInheritDepthStencilState:@
setInheritDepthStencilState :: IsMTLIndirectCommandBufferDescriptor mtlIndirectCommandBufferDescriptor => mtlIndirectCommandBufferDescriptor -> Bool -> IO ()
setInheritDepthStencilState mtlIndirectCommandBufferDescriptor value =
  sendMessage mtlIndirectCommandBufferDescriptor setInheritDepthStencilStateSelector value

-- | Configures whether the indirect command buffer inherits the depth bias from the encoder.
--
-- The property's default value is <doc://com.apple.documentation/documentation/swift/true>.
--
-- ObjC selector: @- inheritDepthBias@
inheritDepthBias :: IsMTLIndirectCommandBufferDescriptor mtlIndirectCommandBufferDescriptor => mtlIndirectCommandBufferDescriptor -> IO Bool
inheritDepthBias mtlIndirectCommandBufferDescriptor =
  sendMessage mtlIndirectCommandBufferDescriptor inheritDepthBiasSelector

-- | Configures whether the indirect command buffer inherits the depth bias from the encoder.
--
-- The property's default value is <doc://com.apple.documentation/documentation/swift/true>.
--
-- ObjC selector: @- setInheritDepthBias:@
setInheritDepthBias :: IsMTLIndirectCommandBufferDescriptor mtlIndirectCommandBufferDescriptor => mtlIndirectCommandBufferDescriptor -> Bool -> IO ()
setInheritDepthBias mtlIndirectCommandBufferDescriptor value =
  sendMessage mtlIndirectCommandBufferDescriptor setInheritDepthBiasSelector value

-- | Configures whether the indirect command buffer inherits the depth clip mode from the encoder.
--
-- The property's default value is <doc://com.apple.documentation/documentation/swift/true>.
--
-- ObjC selector: @- inheritDepthClipMode@
inheritDepthClipMode :: IsMTLIndirectCommandBufferDescriptor mtlIndirectCommandBufferDescriptor => mtlIndirectCommandBufferDescriptor -> IO Bool
inheritDepthClipMode mtlIndirectCommandBufferDescriptor =
  sendMessage mtlIndirectCommandBufferDescriptor inheritDepthClipModeSelector

-- | Configures whether the indirect command buffer inherits the depth clip mode from the encoder.
--
-- The property's default value is <doc://com.apple.documentation/documentation/swift/true>.
--
-- ObjC selector: @- setInheritDepthClipMode:@
setInheritDepthClipMode :: IsMTLIndirectCommandBufferDescriptor mtlIndirectCommandBufferDescriptor => mtlIndirectCommandBufferDescriptor -> Bool -> IO ()
setInheritDepthClipMode mtlIndirectCommandBufferDescriptor value =
  sendMessage mtlIndirectCommandBufferDescriptor setInheritDepthClipModeSelector value

-- | Configures whether the indirect command buffer inherits the cull mode from the encoder.
--
-- The property's default value is <doc://com.apple.documentation/documentation/swift/true>.
--
-- ObjC selector: @- inheritCullMode@
inheritCullMode :: IsMTLIndirectCommandBufferDescriptor mtlIndirectCommandBufferDescriptor => mtlIndirectCommandBufferDescriptor -> IO Bool
inheritCullMode mtlIndirectCommandBufferDescriptor =
  sendMessage mtlIndirectCommandBufferDescriptor inheritCullModeSelector

-- | Configures whether the indirect command buffer inherits the cull mode from the encoder.
--
-- The property's default value is <doc://com.apple.documentation/documentation/swift/true>.
--
-- ObjC selector: @- setInheritCullMode:@
setInheritCullMode :: IsMTLIndirectCommandBufferDescriptor mtlIndirectCommandBufferDescriptor => mtlIndirectCommandBufferDescriptor -> Bool -> IO ()
setInheritCullMode mtlIndirectCommandBufferDescriptor value =
  sendMessage mtlIndirectCommandBufferDescriptor setInheritCullModeSelector value

-- | Configures whether the indirect command buffer inherits the front facing winding from the encoder.
--
-- The property's default value is <doc://com.apple.documentation/documentation/swift/true>.
--
-- ObjC selector: @- inheritFrontFacingWinding@
inheritFrontFacingWinding :: IsMTLIndirectCommandBufferDescriptor mtlIndirectCommandBufferDescriptor => mtlIndirectCommandBufferDescriptor -> IO Bool
inheritFrontFacingWinding mtlIndirectCommandBufferDescriptor =
  sendMessage mtlIndirectCommandBufferDescriptor inheritFrontFacingWindingSelector

-- | Configures whether the indirect command buffer inherits the front facing winding from the encoder.
--
-- The property's default value is <doc://com.apple.documentation/documentation/swift/true>.
--
-- ObjC selector: @- setInheritFrontFacingWinding:@
setInheritFrontFacingWinding :: IsMTLIndirectCommandBufferDescriptor mtlIndirectCommandBufferDescriptor => mtlIndirectCommandBufferDescriptor -> Bool -> IO ()
setInheritFrontFacingWinding mtlIndirectCommandBufferDescriptor value =
  sendMessage mtlIndirectCommandBufferDescriptor setInheritFrontFacingWindingSelector value

-- | Configures whether the indirect command buffer inherits the triangle fill mode from the encoder.
--
-- The property's default value is <doc://com.apple.documentation/documentation/swift/true>.
--
-- ObjC selector: @- inheritTriangleFillMode@
inheritTriangleFillMode :: IsMTLIndirectCommandBufferDescriptor mtlIndirectCommandBufferDescriptor => mtlIndirectCommandBufferDescriptor -> IO Bool
inheritTriangleFillMode mtlIndirectCommandBufferDescriptor =
  sendMessage mtlIndirectCommandBufferDescriptor inheritTriangleFillModeSelector

-- | Configures whether the indirect command buffer inherits the triangle fill mode from the encoder.
--
-- The property's default value is <doc://com.apple.documentation/documentation/swift/true>.
--
-- ObjC selector: @- setInheritTriangleFillMode:@
setInheritTriangleFillMode :: IsMTLIndirectCommandBufferDescriptor mtlIndirectCommandBufferDescriptor => mtlIndirectCommandBufferDescriptor -> Bool -> IO ()
setInheritTriangleFillMode mtlIndirectCommandBufferDescriptor value =
  sendMessage mtlIndirectCommandBufferDescriptor setInheritTriangleFillModeSelector value

-- | The maximum bind index of vertex argument buffers that can be set per command.
--
-- ObjC selector: @- maxVertexBufferBindCount@
maxVertexBufferBindCount :: IsMTLIndirectCommandBufferDescriptor mtlIndirectCommandBufferDescriptor => mtlIndirectCommandBufferDescriptor -> IO CULong
maxVertexBufferBindCount mtlIndirectCommandBufferDescriptor =
  sendMessage mtlIndirectCommandBufferDescriptor maxVertexBufferBindCountSelector

-- | The maximum bind index of vertex argument buffers that can be set per command.
--
-- ObjC selector: @- setMaxVertexBufferBindCount:@
setMaxVertexBufferBindCount :: IsMTLIndirectCommandBufferDescriptor mtlIndirectCommandBufferDescriptor => mtlIndirectCommandBufferDescriptor -> CULong -> IO ()
setMaxVertexBufferBindCount mtlIndirectCommandBufferDescriptor value =
  sendMessage mtlIndirectCommandBufferDescriptor setMaxVertexBufferBindCountSelector value

-- | The maximum bind index of fragment argument buffers that can be set per command.
--
-- ObjC selector: @- maxFragmentBufferBindCount@
maxFragmentBufferBindCount :: IsMTLIndirectCommandBufferDescriptor mtlIndirectCommandBufferDescriptor => mtlIndirectCommandBufferDescriptor -> IO CULong
maxFragmentBufferBindCount mtlIndirectCommandBufferDescriptor =
  sendMessage mtlIndirectCommandBufferDescriptor maxFragmentBufferBindCountSelector

-- | The maximum bind index of fragment argument buffers that can be set per command.
--
-- ObjC selector: @- setMaxFragmentBufferBindCount:@
setMaxFragmentBufferBindCount :: IsMTLIndirectCommandBufferDescriptor mtlIndirectCommandBufferDescriptor => mtlIndirectCommandBufferDescriptor -> CULong -> IO ()
setMaxFragmentBufferBindCount mtlIndirectCommandBufferDescriptor value =
  sendMessage mtlIndirectCommandBufferDescriptor setMaxFragmentBufferBindCountSelector value

-- | The maximum bind index of kernel (or tile) argument buffers that can be set per command.
--
-- ObjC selector: @- maxKernelBufferBindCount@
maxKernelBufferBindCount :: IsMTLIndirectCommandBufferDescriptor mtlIndirectCommandBufferDescriptor => mtlIndirectCommandBufferDescriptor -> IO CULong
maxKernelBufferBindCount mtlIndirectCommandBufferDescriptor =
  sendMessage mtlIndirectCommandBufferDescriptor maxKernelBufferBindCountSelector

-- | The maximum bind index of kernel (or tile) argument buffers that can be set per command.
--
-- ObjC selector: @- setMaxKernelBufferBindCount:@
setMaxKernelBufferBindCount :: IsMTLIndirectCommandBufferDescriptor mtlIndirectCommandBufferDescriptor => mtlIndirectCommandBufferDescriptor -> CULong -> IO ()
setMaxKernelBufferBindCount mtlIndirectCommandBufferDescriptor value =
  sendMessage mtlIndirectCommandBufferDescriptor setMaxKernelBufferBindCountSelector value

-- | The maximum bind index of kernel (or tile) threadgroup memory that can be set per command. The default value is 31.
--
-- ObjC selector: @- maxKernelThreadgroupMemoryBindCount@
maxKernelThreadgroupMemoryBindCount :: IsMTLIndirectCommandBufferDescriptor mtlIndirectCommandBufferDescriptor => mtlIndirectCommandBufferDescriptor -> IO CULong
maxKernelThreadgroupMemoryBindCount mtlIndirectCommandBufferDescriptor =
  sendMessage mtlIndirectCommandBufferDescriptor maxKernelThreadgroupMemoryBindCountSelector

-- | The maximum bind index of kernel (or tile) threadgroup memory that can be set per command. The default value is 31.
--
-- ObjC selector: @- setMaxKernelThreadgroupMemoryBindCount:@
setMaxKernelThreadgroupMemoryBindCount :: IsMTLIndirectCommandBufferDescriptor mtlIndirectCommandBufferDescriptor => mtlIndirectCommandBufferDescriptor -> CULong -> IO ()
setMaxKernelThreadgroupMemoryBindCount mtlIndirectCommandBufferDescriptor value =
  sendMessage mtlIndirectCommandBufferDescriptor setMaxKernelThreadgroupMemoryBindCountSelector value

-- | The maximum bind index of object stage buffers that can be set per render command.
--
-- ObjC selector: @- maxObjectBufferBindCount@
maxObjectBufferBindCount :: IsMTLIndirectCommandBufferDescriptor mtlIndirectCommandBufferDescriptor => mtlIndirectCommandBufferDescriptor -> IO CULong
maxObjectBufferBindCount mtlIndirectCommandBufferDescriptor =
  sendMessage mtlIndirectCommandBufferDescriptor maxObjectBufferBindCountSelector

-- | The maximum bind index of object stage buffers that can be set per render command.
--
-- ObjC selector: @- setMaxObjectBufferBindCount:@
setMaxObjectBufferBindCount :: IsMTLIndirectCommandBufferDescriptor mtlIndirectCommandBufferDescriptor => mtlIndirectCommandBufferDescriptor -> CULong -> IO ()
setMaxObjectBufferBindCount mtlIndirectCommandBufferDescriptor value =
  sendMessage mtlIndirectCommandBufferDescriptor setMaxObjectBufferBindCountSelector value

-- | The maximum bind index of mesh stage buffers that can be set per render command.
--
-- ObjC selector: @- maxMeshBufferBindCount@
maxMeshBufferBindCount :: IsMTLIndirectCommandBufferDescriptor mtlIndirectCommandBufferDescriptor => mtlIndirectCommandBufferDescriptor -> IO CULong
maxMeshBufferBindCount mtlIndirectCommandBufferDescriptor =
  sendMessage mtlIndirectCommandBufferDescriptor maxMeshBufferBindCountSelector

-- | The maximum bind index of mesh stage buffers that can be set per render command.
--
-- ObjC selector: @- setMaxMeshBufferBindCount:@
setMaxMeshBufferBindCount :: IsMTLIndirectCommandBufferDescriptor mtlIndirectCommandBufferDescriptor => mtlIndirectCommandBufferDescriptor -> CULong -> IO ()
setMaxMeshBufferBindCount mtlIndirectCommandBufferDescriptor value =
  sendMessage mtlIndirectCommandBufferDescriptor setMaxMeshBufferBindCountSelector value

-- | The maximum bind index of object threadgroup memory that can be set per render command. The default value is 0.
--
-- ObjC selector: @- maxObjectThreadgroupMemoryBindCount@
maxObjectThreadgroupMemoryBindCount :: IsMTLIndirectCommandBufferDescriptor mtlIndirectCommandBufferDescriptor => mtlIndirectCommandBufferDescriptor -> IO CULong
maxObjectThreadgroupMemoryBindCount mtlIndirectCommandBufferDescriptor =
  sendMessage mtlIndirectCommandBufferDescriptor maxObjectThreadgroupMemoryBindCountSelector

-- | The maximum bind index of object threadgroup memory that can be set per render command. The default value is 0.
--
-- ObjC selector: @- setMaxObjectThreadgroupMemoryBindCount:@
setMaxObjectThreadgroupMemoryBindCount :: IsMTLIndirectCommandBufferDescriptor mtlIndirectCommandBufferDescriptor => mtlIndirectCommandBufferDescriptor -> CULong -> IO ()
setMaxObjectThreadgroupMemoryBindCount mtlIndirectCommandBufferDescriptor value =
  sendMessage mtlIndirectCommandBufferDescriptor setMaxObjectThreadgroupMemoryBindCountSelector value

-- | Whether the render or compute commands can use ray tracing. Default value is NO.
--
-- ObjC selector: @- supportRayTracing@
supportRayTracing :: IsMTLIndirectCommandBufferDescriptor mtlIndirectCommandBufferDescriptor => mtlIndirectCommandBufferDescriptor -> IO Bool
supportRayTracing mtlIndirectCommandBufferDescriptor =
  sendMessage mtlIndirectCommandBufferDescriptor supportRayTracingSelector

-- | Whether the render or compute commands can use ray tracing. Default value is NO.
--
-- ObjC selector: @- setSupportRayTracing:@
setSupportRayTracing :: IsMTLIndirectCommandBufferDescriptor mtlIndirectCommandBufferDescriptor => mtlIndirectCommandBufferDescriptor -> Bool -> IO ()
setSupportRayTracing mtlIndirectCommandBufferDescriptor value =
  sendMessage mtlIndirectCommandBufferDescriptor setSupportRayTracingSelector value

-- | allows binding pipelines that have at least one MTLBufferLayout with a    stride of @MTLBufferLayoutStrideDynamic@
--
-- will allow setting attributeStride in @setVertexBuffer@ / @setKernelBuffer@    calls
--
-- ObjC selector: @- supportDynamicAttributeStride@
supportDynamicAttributeStride :: IsMTLIndirectCommandBufferDescriptor mtlIndirectCommandBufferDescriptor => mtlIndirectCommandBufferDescriptor -> IO Bool
supportDynamicAttributeStride mtlIndirectCommandBufferDescriptor =
  sendMessage mtlIndirectCommandBufferDescriptor supportDynamicAttributeStrideSelector

-- | allows binding pipelines that have at least one MTLBufferLayout with a    stride of @MTLBufferLayoutStrideDynamic@
--
-- will allow setting attributeStride in @setVertexBuffer@ / @setKernelBuffer@    calls
--
-- ObjC selector: @- setSupportDynamicAttributeStride:@
setSupportDynamicAttributeStride :: IsMTLIndirectCommandBufferDescriptor mtlIndirectCommandBufferDescriptor => mtlIndirectCommandBufferDescriptor -> Bool -> IO ()
setSupportDynamicAttributeStride mtlIndirectCommandBufferDescriptor value =
  sendMessage mtlIndirectCommandBufferDescriptor setSupportDynamicAttributeStrideSelector value

-- | Specifies if the indirect command buffer should support color attachment mapping.
--
-- ObjC selector: @- supportColorAttachmentMapping@
supportColorAttachmentMapping :: IsMTLIndirectCommandBufferDescriptor mtlIndirectCommandBufferDescriptor => mtlIndirectCommandBufferDescriptor -> IO Bool
supportColorAttachmentMapping mtlIndirectCommandBufferDescriptor =
  sendMessage mtlIndirectCommandBufferDescriptor supportColorAttachmentMappingSelector

-- | Specifies if the indirect command buffer should support color attachment mapping.
--
-- ObjC selector: @- setSupportColorAttachmentMapping:@
setSupportColorAttachmentMapping :: IsMTLIndirectCommandBufferDescriptor mtlIndirectCommandBufferDescriptor => mtlIndirectCommandBufferDescriptor -> Bool -> IO ()
setSupportColorAttachmentMapping mtlIndirectCommandBufferDescriptor value =
  sendMessage mtlIndirectCommandBufferDescriptor setSupportColorAttachmentMappingSelector value

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @commandTypes@
commandTypesSelector :: Selector '[] MTLIndirectCommandType
commandTypesSelector = mkSelector "commandTypes"

-- | @Selector@ for @setCommandTypes:@
setCommandTypesSelector :: Selector '[MTLIndirectCommandType] ()
setCommandTypesSelector = mkSelector "setCommandTypes:"

-- | @Selector@ for @inheritPipelineState@
inheritPipelineStateSelector :: Selector '[] Bool
inheritPipelineStateSelector = mkSelector "inheritPipelineState"

-- | @Selector@ for @setInheritPipelineState:@
setInheritPipelineStateSelector :: Selector '[Bool] ()
setInheritPipelineStateSelector = mkSelector "setInheritPipelineState:"

-- | @Selector@ for @inheritBuffers@
inheritBuffersSelector :: Selector '[] Bool
inheritBuffersSelector = mkSelector "inheritBuffers"

-- | @Selector@ for @setInheritBuffers:@
setInheritBuffersSelector :: Selector '[Bool] ()
setInheritBuffersSelector = mkSelector "setInheritBuffers:"

-- | @Selector@ for @inheritDepthStencilState@
inheritDepthStencilStateSelector :: Selector '[] Bool
inheritDepthStencilStateSelector = mkSelector "inheritDepthStencilState"

-- | @Selector@ for @setInheritDepthStencilState:@
setInheritDepthStencilStateSelector :: Selector '[Bool] ()
setInheritDepthStencilStateSelector = mkSelector "setInheritDepthStencilState:"

-- | @Selector@ for @inheritDepthBias@
inheritDepthBiasSelector :: Selector '[] Bool
inheritDepthBiasSelector = mkSelector "inheritDepthBias"

-- | @Selector@ for @setInheritDepthBias:@
setInheritDepthBiasSelector :: Selector '[Bool] ()
setInheritDepthBiasSelector = mkSelector "setInheritDepthBias:"

-- | @Selector@ for @inheritDepthClipMode@
inheritDepthClipModeSelector :: Selector '[] Bool
inheritDepthClipModeSelector = mkSelector "inheritDepthClipMode"

-- | @Selector@ for @setInheritDepthClipMode:@
setInheritDepthClipModeSelector :: Selector '[Bool] ()
setInheritDepthClipModeSelector = mkSelector "setInheritDepthClipMode:"

-- | @Selector@ for @inheritCullMode@
inheritCullModeSelector :: Selector '[] Bool
inheritCullModeSelector = mkSelector "inheritCullMode"

-- | @Selector@ for @setInheritCullMode:@
setInheritCullModeSelector :: Selector '[Bool] ()
setInheritCullModeSelector = mkSelector "setInheritCullMode:"

-- | @Selector@ for @inheritFrontFacingWinding@
inheritFrontFacingWindingSelector :: Selector '[] Bool
inheritFrontFacingWindingSelector = mkSelector "inheritFrontFacingWinding"

-- | @Selector@ for @setInheritFrontFacingWinding:@
setInheritFrontFacingWindingSelector :: Selector '[Bool] ()
setInheritFrontFacingWindingSelector = mkSelector "setInheritFrontFacingWinding:"

-- | @Selector@ for @inheritTriangleFillMode@
inheritTriangleFillModeSelector :: Selector '[] Bool
inheritTriangleFillModeSelector = mkSelector "inheritTriangleFillMode"

-- | @Selector@ for @setInheritTriangleFillMode:@
setInheritTriangleFillModeSelector :: Selector '[Bool] ()
setInheritTriangleFillModeSelector = mkSelector "setInheritTriangleFillMode:"

-- | @Selector@ for @maxVertexBufferBindCount@
maxVertexBufferBindCountSelector :: Selector '[] CULong
maxVertexBufferBindCountSelector = mkSelector "maxVertexBufferBindCount"

-- | @Selector@ for @setMaxVertexBufferBindCount:@
setMaxVertexBufferBindCountSelector :: Selector '[CULong] ()
setMaxVertexBufferBindCountSelector = mkSelector "setMaxVertexBufferBindCount:"

-- | @Selector@ for @maxFragmentBufferBindCount@
maxFragmentBufferBindCountSelector :: Selector '[] CULong
maxFragmentBufferBindCountSelector = mkSelector "maxFragmentBufferBindCount"

-- | @Selector@ for @setMaxFragmentBufferBindCount:@
setMaxFragmentBufferBindCountSelector :: Selector '[CULong] ()
setMaxFragmentBufferBindCountSelector = mkSelector "setMaxFragmentBufferBindCount:"

-- | @Selector@ for @maxKernelBufferBindCount@
maxKernelBufferBindCountSelector :: Selector '[] CULong
maxKernelBufferBindCountSelector = mkSelector "maxKernelBufferBindCount"

-- | @Selector@ for @setMaxKernelBufferBindCount:@
setMaxKernelBufferBindCountSelector :: Selector '[CULong] ()
setMaxKernelBufferBindCountSelector = mkSelector "setMaxKernelBufferBindCount:"

-- | @Selector@ for @maxKernelThreadgroupMemoryBindCount@
maxKernelThreadgroupMemoryBindCountSelector :: Selector '[] CULong
maxKernelThreadgroupMemoryBindCountSelector = mkSelector "maxKernelThreadgroupMemoryBindCount"

-- | @Selector@ for @setMaxKernelThreadgroupMemoryBindCount:@
setMaxKernelThreadgroupMemoryBindCountSelector :: Selector '[CULong] ()
setMaxKernelThreadgroupMemoryBindCountSelector = mkSelector "setMaxKernelThreadgroupMemoryBindCount:"

-- | @Selector@ for @maxObjectBufferBindCount@
maxObjectBufferBindCountSelector :: Selector '[] CULong
maxObjectBufferBindCountSelector = mkSelector "maxObjectBufferBindCount"

-- | @Selector@ for @setMaxObjectBufferBindCount:@
setMaxObjectBufferBindCountSelector :: Selector '[CULong] ()
setMaxObjectBufferBindCountSelector = mkSelector "setMaxObjectBufferBindCount:"

-- | @Selector@ for @maxMeshBufferBindCount@
maxMeshBufferBindCountSelector :: Selector '[] CULong
maxMeshBufferBindCountSelector = mkSelector "maxMeshBufferBindCount"

-- | @Selector@ for @setMaxMeshBufferBindCount:@
setMaxMeshBufferBindCountSelector :: Selector '[CULong] ()
setMaxMeshBufferBindCountSelector = mkSelector "setMaxMeshBufferBindCount:"

-- | @Selector@ for @maxObjectThreadgroupMemoryBindCount@
maxObjectThreadgroupMemoryBindCountSelector :: Selector '[] CULong
maxObjectThreadgroupMemoryBindCountSelector = mkSelector "maxObjectThreadgroupMemoryBindCount"

-- | @Selector@ for @setMaxObjectThreadgroupMemoryBindCount:@
setMaxObjectThreadgroupMemoryBindCountSelector :: Selector '[CULong] ()
setMaxObjectThreadgroupMemoryBindCountSelector = mkSelector "setMaxObjectThreadgroupMemoryBindCount:"

-- | @Selector@ for @supportRayTracing@
supportRayTracingSelector :: Selector '[] Bool
supportRayTracingSelector = mkSelector "supportRayTracing"

-- | @Selector@ for @setSupportRayTracing:@
setSupportRayTracingSelector :: Selector '[Bool] ()
setSupportRayTracingSelector = mkSelector "setSupportRayTracing:"

-- | @Selector@ for @supportDynamicAttributeStride@
supportDynamicAttributeStrideSelector :: Selector '[] Bool
supportDynamicAttributeStrideSelector = mkSelector "supportDynamicAttributeStride"

-- | @Selector@ for @setSupportDynamicAttributeStride:@
setSupportDynamicAttributeStrideSelector :: Selector '[Bool] ()
setSupportDynamicAttributeStrideSelector = mkSelector "setSupportDynamicAttributeStride:"

-- | @Selector@ for @supportColorAttachmentMapping@
supportColorAttachmentMappingSelector :: Selector '[] Bool
supportColorAttachmentMappingSelector = mkSelector "supportColorAttachmentMapping"

-- | @Selector@ for @setSupportColorAttachmentMapping:@
setSupportColorAttachmentMappingSelector :: Selector '[Bool] ()
setSupportColorAttachmentMappingSelector = mkSelector "setSupportColorAttachmentMapping:"

