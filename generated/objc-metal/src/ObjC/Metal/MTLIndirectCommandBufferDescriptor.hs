{-# LANGUAGE PatternSynonyms #-}
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
  , setCommandTypesSelector
  , inheritPipelineStateSelector
  , setInheritPipelineStateSelector
  , inheritBuffersSelector
  , setInheritBuffersSelector
  , inheritDepthStencilStateSelector
  , setInheritDepthStencilStateSelector
  , inheritDepthBiasSelector
  , setInheritDepthBiasSelector
  , inheritDepthClipModeSelector
  , setInheritDepthClipModeSelector
  , inheritCullModeSelector
  , setInheritCullModeSelector
  , inheritFrontFacingWindingSelector
  , setInheritFrontFacingWindingSelector
  , inheritTriangleFillModeSelector
  , setInheritTriangleFillModeSelector
  , maxVertexBufferBindCountSelector
  , setMaxVertexBufferBindCountSelector
  , maxFragmentBufferBindCountSelector
  , setMaxFragmentBufferBindCountSelector
  , maxKernelBufferBindCountSelector
  , setMaxKernelBufferBindCountSelector
  , maxKernelThreadgroupMemoryBindCountSelector
  , setMaxKernelThreadgroupMemoryBindCountSelector
  , maxObjectBufferBindCountSelector
  , setMaxObjectBufferBindCountSelector
  , maxMeshBufferBindCountSelector
  , setMaxMeshBufferBindCountSelector
  , maxObjectThreadgroupMemoryBindCountSelector
  , setMaxObjectThreadgroupMemoryBindCountSelector
  , supportRayTracingSelector
  , setSupportRayTracingSelector
  , supportDynamicAttributeStrideSelector
  , setSupportDynamicAttributeStrideSelector
  , supportColorAttachmentMappingSelector
  , setSupportColorAttachmentMappingSelector

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

-- | A bitfield of the command types that be encoded.
--
-- MTLCommandTypeDispatch cannot be mixed with any other command type.
--
-- ObjC selector: @- commandTypes@
commandTypes :: IsMTLIndirectCommandBufferDescriptor mtlIndirectCommandBufferDescriptor => mtlIndirectCommandBufferDescriptor -> IO MTLIndirectCommandType
commandTypes mtlIndirectCommandBufferDescriptor  =
  fmap (coerce :: CULong -> MTLIndirectCommandType) $ sendMsg mtlIndirectCommandBufferDescriptor (mkSelector "commandTypes") retCULong []

-- | A bitfield of the command types that be encoded.
--
-- MTLCommandTypeDispatch cannot be mixed with any other command type.
--
-- ObjC selector: @- setCommandTypes:@
setCommandTypes :: IsMTLIndirectCommandBufferDescriptor mtlIndirectCommandBufferDescriptor => mtlIndirectCommandBufferDescriptor -> MTLIndirectCommandType -> IO ()
setCommandTypes mtlIndirectCommandBufferDescriptor  value =
  sendMsg mtlIndirectCommandBufferDescriptor (mkSelector "setCommandTypes:") retVoid [argCULong (coerce value)]

-- | Whether the render or compute pipeline are inherited from the encoder
--
-- ObjC selector: @- inheritPipelineState@
inheritPipelineState :: IsMTLIndirectCommandBufferDescriptor mtlIndirectCommandBufferDescriptor => mtlIndirectCommandBufferDescriptor -> IO Bool
inheritPipelineState mtlIndirectCommandBufferDescriptor  =
  fmap ((/= 0) :: CULong -> Bool) $ sendMsg mtlIndirectCommandBufferDescriptor (mkSelector "inheritPipelineState") retCULong []

-- | Whether the render or compute pipeline are inherited from the encoder
--
-- ObjC selector: @- setInheritPipelineState:@
setInheritPipelineState :: IsMTLIndirectCommandBufferDescriptor mtlIndirectCommandBufferDescriptor => mtlIndirectCommandBufferDescriptor -> Bool -> IO ()
setInheritPipelineState mtlIndirectCommandBufferDescriptor  value =
  sendMsg mtlIndirectCommandBufferDescriptor (mkSelector "setInheritPipelineState:") retVoid [argCULong (if value then 1 else 0)]

-- | Whether the render or compute pipeline can set arguments.
--
-- ObjC selector: @- inheritBuffers@
inheritBuffers :: IsMTLIndirectCommandBufferDescriptor mtlIndirectCommandBufferDescriptor => mtlIndirectCommandBufferDescriptor -> IO Bool
inheritBuffers mtlIndirectCommandBufferDescriptor  =
  fmap ((/= 0) :: CULong -> Bool) $ sendMsg mtlIndirectCommandBufferDescriptor (mkSelector "inheritBuffers") retCULong []

-- | Whether the render or compute pipeline can set arguments.
--
-- ObjC selector: @- setInheritBuffers:@
setInheritBuffers :: IsMTLIndirectCommandBufferDescriptor mtlIndirectCommandBufferDescriptor => mtlIndirectCommandBufferDescriptor -> Bool -> IO ()
setInheritBuffers mtlIndirectCommandBufferDescriptor  value =
  sendMsg mtlIndirectCommandBufferDescriptor (mkSelector "setInheritBuffers:") retVoid [argCULong (if value then 1 else 0)]

-- | Configures whether the indirect command buffer inherits the depth stencil state from the encoder.
--
-- The property's default value is <doc://com.apple.documentation/documentation/swift/true>.
--
-- ObjC selector: @- inheritDepthStencilState@
inheritDepthStencilState :: IsMTLIndirectCommandBufferDescriptor mtlIndirectCommandBufferDescriptor => mtlIndirectCommandBufferDescriptor -> IO Bool
inheritDepthStencilState mtlIndirectCommandBufferDescriptor  =
  fmap ((/= 0) :: CULong -> Bool) $ sendMsg mtlIndirectCommandBufferDescriptor (mkSelector "inheritDepthStencilState") retCULong []

-- | Configures whether the indirect command buffer inherits the depth stencil state from the encoder.
--
-- The property's default value is <doc://com.apple.documentation/documentation/swift/true>.
--
-- ObjC selector: @- setInheritDepthStencilState:@
setInheritDepthStencilState :: IsMTLIndirectCommandBufferDescriptor mtlIndirectCommandBufferDescriptor => mtlIndirectCommandBufferDescriptor -> Bool -> IO ()
setInheritDepthStencilState mtlIndirectCommandBufferDescriptor  value =
  sendMsg mtlIndirectCommandBufferDescriptor (mkSelector "setInheritDepthStencilState:") retVoid [argCULong (if value then 1 else 0)]

-- | Configures whether the indirect command buffer inherits the depth bias from the encoder.
--
-- The property's default value is <doc://com.apple.documentation/documentation/swift/true>.
--
-- ObjC selector: @- inheritDepthBias@
inheritDepthBias :: IsMTLIndirectCommandBufferDescriptor mtlIndirectCommandBufferDescriptor => mtlIndirectCommandBufferDescriptor -> IO Bool
inheritDepthBias mtlIndirectCommandBufferDescriptor  =
  fmap ((/= 0) :: CULong -> Bool) $ sendMsg mtlIndirectCommandBufferDescriptor (mkSelector "inheritDepthBias") retCULong []

-- | Configures whether the indirect command buffer inherits the depth bias from the encoder.
--
-- The property's default value is <doc://com.apple.documentation/documentation/swift/true>.
--
-- ObjC selector: @- setInheritDepthBias:@
setInheritDepthBias :: IsMTLIndirectCommandBufferDescriptor mtlIndirectCommandBufferDescriptor => mtlIndirectCommandBufferDescriptor -> Bool -> IO ()
setInheritDepthBias mtlIndirectCommandBufferDescriptor  value =
  sendMsg mtlIndirectCommandBufferDescriptor (mkSelector "setInheritDepthBias:") retVoid [argCULong (if value then 1 else 0)]

-- | Configures whether the indirect command buffer inherits the depth clip mode from the encoder.
--
-- The property's default value is <doc://com.apple.documentation/documentation/swift/true>.
--
-- ObjC selector: @- inheritDepthClipMode@
inheritDepthClipMode :: IsMTLIndirectCommandBufferDescriptor mtlIndirectCommandBufferDescriptor => mtlIndirectCommandBufferDescriptor -> IO Bool
inheritDepthClipMode mtlIndirectCommandBufferDescriptor  =
  fmap ((/= 0) :: CULong -> Bool) $ sendMsg mtlIndirectCommandBufferDescriptor (mkSelector "inheritDepthClipMode") retCULong []

-- | Configures whether the indirect command buffer inherits the depth clip mode from the encoder.
--
-- The property's default value is <doc://com.apple.documentation/documentation/swift/true>.
--
-- ObjC selector: @- setInheritDepthClipMode:@
setInheritDepthClipMode :: IsMTLIndirectCommandBufferDescriptor mtlIndirectCommandBufferDescriptor => mtlIndirectCommandBufferDescriptor -> Bool -> IO ()
setInheritDepthClipMode mtlIndirectCommandBufferDescriptor  value =
  sendMsg mtlIndirectCommandBufferDescriptor (mkSelector "setInheritDepthClipMode:") retVoid [argCULong (if value then 1 else 0)]

-- | Configures whether the indirect command buffer inherits the cull mode from the encoder.
--
-- The property's default value is <doc://com.apple.documentation/documentation/swift/true>.
--
-- ObjC selector: @- inheritCullMode@
inheritCullMode :: IsMTLIndirectCommandBufferDescriptor mtlIndirectCommandBufferDescriptor => mtlIndirectCommandBufferDescriptor -> IO Bool
inheritCullMode mtlIndirectCommandBufferDescriptor  =
  fmap ((/= 0) :: CULong -> Bool) $ sendMsg mtlIndirectCommandBufferDescriptor (mkSelector "inheritCullMode") retCULong []

-- | Configures whether the indirect command buffer inherits the cull mode from the encoder.
--
-- The property's default value is <doc://com.apple.documentation/documentation/swift/true>.
--
-- ObjC selector: @- setInheritCullMode:@
setInheritCullMode :: IsMTLIndirectCommandBufferDescriptor mtlIndirectCommandBufferDescriptor => mtlIndirectCommandBufferDescriptor -> Bool -> IO ()
setInheritCullMode mtlIndirectCommandBufferDescriptor  value =
  sendMsg mtlIndirectCommandBufferDescriptor (mkSelector "setInheritCullMode:") retVoid [argCULong (if value then 1 else 0)]

-- | Configures whether the indirect command buffer inherits the front facing winding from the encoder.
--
-- The property's default value is <doc://com.apple.documentation/documentation/swift/true>.
--
-- ObjC selector: @- inheritFrontFacingWinding@
inheritFrontFacingWinding :: IsMTLIndirectCommandBufferDescriptor mtlIndirectCommandBufferDescriptor => mtlIndirectCommandBufferDescriptor -> IO Bool
inheritFrontFacingWinding mtlIndirectCommandBufferDescriptor  =
  fmap ((/= 0) :: CULong -> Bool) $ sendMsg mtlIndirectCommandBufferDescriptor (mkSelector "inheritFrontFacingWinding") retCULong []

-- | Configures whether the indirect command buffer inherits the front facing winding from the encoder.
--
-- The property's default value is <doc://com.apple.documentation/documentation/swift/true>.
--
-- ObjC selector: @- setInheritFrontFacingWinding:@
setInheritFrontFacingWinding :: IsMTLIndirectCommandBufferDescriptor mtlIndirectCommandBufferDescriptor => mtlIndirectCommandBufferDescriptor -> Bool -> IO ()
setInheritFrontFacingWinding mtlIndirectCommandBufferDescriptor  value =
  sendMsg mtlIndirectCommandBufferDescriptor (mkSelector "setInheritFrontFacingWinding:") retVoid [argCULong (if value then 1 else 0)]

-- | Configures whether the indirect command buffer inherits the triangle fill mode from the encoder.
--
-- The property's default value is <doc://com.apple.documentation/documentation/swift/true>.
--
-- ObjC selector: @- inheritTriangleFillMode@
inheritTriangleFillMode :: IsMTLIndirectCommandBufferDescriptor mtlIndirectCommandBufferDescriptor => mtlIndirectCommandBufferDescriptor -> IO Bool
inheritTriangleFillMode mtlIndirectCommandBufferDescriptor  =
  fmap ((/= 0) :: CULong -> Bool) $ sendMsg mtlIndirectCommandBufferDescriptor (mkSelector "inheritTriangleFillMode") retCULong []

-- | Configures whether the indirect command buffer inherits the triangle fill mode from the encoder.
--
-- The property's default value is <doc://com.apple.documentation/documentation/swift/true>.
--
-- ObjC selector: @- setInheritTriangleFillMode:@
setInheritTriangleFillMode :: IsMTLIndirectCommandBufferDescriptor mtlIndirectCommandBufferDescriptor => mtlIndirectCommandBufferDescriptor -> Bool -> IO ()
setInheritTriangleFillMode mtlIndirectCommandBufferDescriptor  value =
  sendMsg mtlIndirectCommandBufferDescriptor (mkSelector "setInheritTriangleFillMode:") retVoid [argCULong (if value then 1 else 0)]

-- | The maximum bind index of vertex argument buffers that can be set per command.
--
-- ObjC selector: @- maxVertexBufferBindCount@
maxVertexBufferBindCount :: IsMTLIndirectCommandBufferDescriptor mtlIndirectCommandBufferDescriptor => mtlIndirectCommandBufferDescriptor -> IO CULong
maxVertexBufferBindCount mtlIndirectCommandBufferDescriptor  =
  sendMsg mtlIndirectCommandBufferDescriptor (mkSelector "maxVertexBufferBindCount") retCULong []

-- | The maximum bind index of vertex argument buffers that can be set per command.
--
-- ObjC selector: @- setMaxVertexBufferBindCount:@
setMaxVertexBufferBindCount :: IsMTLIndirectCommandBufferDescriptor mtlIndirectCommandBufferDescriptor => mtlIndirectCommandBufferDescriptor -> CULong -> IO ()
setMaxVertexBufferBindCount mtlIndirectCommandBufferDescriptor  value =
  sendMsg mtlIndirectCommandBufferDescriptor (mkSelector "setMaxVertexBufferBindCount:") retVoid [argCULong (fromIntegral value)]

-- | The maximum bind index of fragment argument buffers that can be set per command.
--
-- ObjC selector: @- maxFragmentBufferBindCount@
maxFragmentBufferBindCount :: IsMTLIndirectCommandBufferDescriptor mtlIndirectCommandBufferDescriptor => mtlIndirectCommandBufferDescriptor -> IO CULong
maxFragmentBufferBindCount mtlIndirectCommandBufferDescriptor  =
  sendMsg mtlIndirectCommandBufferDescriptor (mkSelector "maxFragmentBufferBindCount") retCULong []

-- | The maximum bind index of fragment argument buffers that can be set per command.
--
-- ObjC selector: @- setMaxFragmentBufferBindCount:@
setMaxFragmentBufferBindCount :: IsMTLIndirectCommandBufferDescriptor mtlIndirectCommandBufferDescriptor => mtlIndirectCommandBufferDescriptor -> CULong -> IO ()
setMaxFragmentBufferBindCount mtlIndirectCommandBufferDescriptor  value =
  sendMsg mtlIndirectCommandBufferDescriptor (mkSelector "setMaxFragmentBufferBindCount:") retVoid [argCULong (fromIntegral value)]

-- | The maximum bind index of kernel (or tile) argument buffers that can be set per command.
--
-- ObjC selector: @- maxKernelBufferBindCount@
maxKernelBufferBindCount :: IsMTLIndirectCommandBufferDescriptor mtlIndirectCommandBufferDescriptor => mtlIndirectCommandBufferDescriptor -> IO CULong
maxKernelBufferBindCount mtlIndirectCommandBufferDescriptor  =
  sendMsg mtlIndirectCommandBufferDescriptor (mkSelector "maxKernelBufferBindCount") retCULong []

-- | The maximum bind index of kernel (or tile) argument buffers that can be set per command.
--
-- ObjC selector: @- setMaxKernelBufferBindCount:@
setMaxKernelBufferBindCount :: IsMTLIndirectCommandBufferDescriptor mtlIndirectCommandBufferDescriptor => mtlIndirectCommandBufferDescriptor -> CULong -> IO ()
setMaxKernelBufferBindCount mtlIndirectCommandBufferDescriptor  value =
  sendMsg mtlIndirectCommandBufferDescriptor (mkSelector "setMaxKernelBufferBindCount:") retVoid [argCULong (fromIntegral value)]

-- | The maximum bind index of kernel (or tile) threadgroup memory that can be set per command. The default value is 31.
--
-- ObjC selector: @- maxKernelThreadgroupMemoryBindCount@
maxKernelThreadgroupMemoryBindCount :: IsMTLIndirectCommandBufferDescriptor mtlIndirectCommandBufferDescriptor => mtlIndirectCommandBufferDescriptor -> IO CULong
maxKernelThreadgroupMemoryBindCount mtlIndirectCommandBufferDescriptor  =
  sendMsg mtlIndirectCommandBufferDescriptor (mkSelector "maxKernelThreadgroupMemoryBindCount") retCULong []

-- | The maximum bind index of kernel (or tile) threadgroup memory that can be set per command. The default value is 31.
--
-- ObjC selector: @- setMaxKernelThreadgroupMemoryBindCount:@
setMaxKernelThreadgroupMemoryBindCount :: IsMTLIndirectCommandBufferDescriptor mtlIndirectCommandBufferDescriptor => mtlIndirectCommandBufferDescriptor -> CULong -> IO ()
setMaxKernelThreadgroupMemoryBindCount mtlIndirectCommandBufferDescriptor  value =
  sendMsg mtlIndirectCommandBufferDescriptor (mkSelector "setMaxKernelThreadgroupMemoryBindCount:") retVoid [argCULong (fromIntegral value)]

-- | The maximum bind index of object stage buffers that can be set per render command.
--
-- ObjC selector: @- maxObjectBufferBindCount@
maxObjectBufferBindCount :: IsMTLIndirectCommandBufferDescriptor mtlIndirectCommandBufferDescriptor => mtlIndirectCommandBufferDescriptor -> IO CULong
maxObjectBufferBindCount mtlIndirectCommandBufferDescriptor  =
  sendMsg mtlIndirectCommandBufferDescriptor (mkSelector "maxObjectBufferBindCount") retCULong []

-- | The maximum bind index of object stage buffers that can be set per render command.
--
-- ObjC selector: @- setMaxObjectBufferBindCount:@
setMaxObjectBufferBindCount :: IsMTLIndirectCommandBufferDescriptor mtlIndirectCommandBufferDescriptor => mtlIndirectCommandBufferDescriptor -> CULong -> IO ()
setMaxObjectBufferBindCount mtlIndirectCommandBufferDescriptor  value =
  sendMsg mtlIndirectCommandBufferDescriptor (mkSelector "setMaxObjectBufferBindCount:") retVoid [argCULong (fromIntegral value)]

-- | The maximum bind index of mesh stage buffers that can be set per render command.
--
-- ObjC selector: @- maxMeshBufferBindCount@
maxMeshBufferBindCount :: IsMTLIndirectCommandBufferDescriptor mtlIndirectCommandBufferDescriptor => mtlIndirectCommandBufferDescriptor -> IO CULong
maxMeshBufferBindCount mtlIndirectCommandBufferDescriptor  =
  sendMsg mtlIndirectCommandBufferDescriptor (mkSelector "maxMeshBufferBindCount") retCULong []

-- | The maximum bind index of mesh stage buffers that can be set per render command.
--
-- ObjC selector: @- setMaxMeshBufferBindCount:@
setMaxMeshBufferBindCount :: IsMTLIndirectCommandBufferDescriptor mtlIndirectCommandBufferDescriptor => mtlIndirectCommandBufferDescriptor -> CULong -> IO ()
setMaxMeshBufferBindCount mtlIndirectCommandBufferDescriptor  value =
  sendMsg mtlIndirectCommandBufferDescriptor (mkSelector "setMaxMeshBufferBindCount:") retVoid [argCULong (fromIntegral value)]

-- | The maximum bind index of object threadgroup memory that can be set per render command. The default value is 0.
--
-- ObjC selector: @- maxObjectThreadgroupMemoryBindCount@
maxObjectThreadgroupMemoryBindCount :: IsMTLIndirectCommandBufferDescriptor mtlIndirectCommandBufferDescriptor => mtlIndirectCommandBufferDescriptor -> IO CULong
maxObjectThreadgroupMemoryBindCount mtlIndirectCommandBufferDescriptor  =
  sendMsg mtlIndirectCommandBufferDescriptor (mkSelector "maxObjectThreadgroupMemoryBindCount") retCULong []

-- | The maximum bind index of object threadgroup memory that can be set per render command. The default value is 0.
--
-- ObjC selector: @- setMaxObjectThreadgroupMemoryBindCount:@
setMaxObjectThreadgroupMemoryBindCount :: IsMTLIndirectCommandBufferDescriptor mtlIndirectCommandBufferDescriptor => mtlIndirectCommandBufferDescriptor -> CULong -> IO ()
setMaxObjectThreadgroupMemoryBindCount mtlIndirectCommandBufferDescriptor  value =
  sendMsg mtlIndirectCommandBufferDescriptor (mkSelector "setMaxObjectThreadgroupMemoryBindCount:") retVoid [argCULong (fromIntegral value)]

-- | Whether the render or compute commands can use ray tracing. Default value is NO.
--
-- ObjC selector: @- supportRayTracing@
supportRayTracing :: IsMTLIndirectCommandBufferDescriptor mtlIndirectCommandBufferDescriptor => mtlIndirectCommandBufferDescriptor -> IO Bool
supportRayTracing mtlIndirectCommandBufferDescriptor  =
  fmap ((/= 0) :: CULong -> Bool) $ sendMsg mtlIndirectCommandBufferDescriptor (mkSelector "supportRayTracing") retCULong []

-- | Whether the render or compute commands can use ray tracing. Default value is NO.
--
-- ObjC selector: @- setSupportRayTracing:@
setSupportRayTracing :: IsMTLIndirectCommandBufferDescriptor mtlIndirectCommandBufferDescriptor => mtlIndirectCommandBufferDescriptor -> Bool -> IO ()
setSupportRayTracing mtlIndirectCommandBufferDescriptor  value =
  sendMsg mtlIndirectCommandBufferDescriptor (mkSelector "setSupportRayTracing:") retVoid [argCULong (if value then 1 else 0)]

-- | allows binding pipelines that have at least one MTLBufferLayout with a    stride of @MTLBufferLayoutStrideDynamic@
--
-- will allow setting attributeStride in @setVertexBuffer@ / @setKernelBuffer@    calls
--
-- ObjC selector: @- supportDynamicAttributeStride@
supportDynamicAttributeStride :: IsMTLIndirectCommandBufferDescriptor mtlIndirectCommandBufferDescriptor => mtlIndirectCommandBufferDescriptor -> IO Bool
supportDynamicAttributeStride mtlIndirectCommandBufferDescriptor  =
  fmap ((/= 0) :: CULong -> Bool) $ sendMsg mtlIndirectCommandBufferDescriptor (mkSelector "supportDynamicAttributeStride") retCULong []

-- | allows binding pipelines that have at least one MTLBufferLayout with a    stride of @MTLBufferLayoutStrideDynamic@
--
-- will allow setting attributeStride in @setVertexBuffer@ / @setKernelBuffer@    calls
--
-- ObjC selector: @- setSupportDynamicAttributeStride:@
setSupportDynamicAttributeStride :: IsMTLIndirectCommandBufferDescriptor mtlIndirectCommandBufferDescriptor => mtlIndirectCommandBufferDescriptor -> Bool -> IO ()
setSupportDynamicAttributeStride mtlIndirectCommandBufferDescriptor  value =
  sendMsg mtlIndirectCommandBufferDescriptor (mkSelector "setSupportDynamicAttributeStride:") retVoid [argCULong (if value then 1 else 0)]

-- | Specifies if the indirect command buffer should support color attachment mapping.
--
-- ObjC selector: @- supportColorAttachmentMapping@
supportColorAttachmentMapping :: IsMTLIndirectCommandBufferDescriptor mtlIndirectCommandBufferDescriptor => mtlIndirectCommandBufferDescriptor -> IO Bool
supportColorAttachmentMapping mtlIndirectCommandBufferDescriptor  =
  fmap ((/= 0) :: CULong -> Bool) $ sendMsg mtlIndirectCommandBufferDescriptor (mkSelector "supportColorAttachmentMapping") retCULong []

-- | Specifies if the indirect command buffer should support color attachment mapping.
--
-- ObjC selector: @- setSupportColorAttachmentMapping:@
setSupportColorAttachmentMapping :: IsMTLIndirectCommandBufferDescriptor mtlIndirectCommandBufferDescriptor => mtlIndirectCommandBufferDescriptor -> Bool -> IO ()
setSupportColorAttachmentMapping mtlIndirectCommandBufferDescriptor  value =
  sendMsg mtlIndirectCommandBufferDescriptor (mkSelector "setSupportColorAttachmentMapping:") retVoid [argCULong (if value then 1 else 0)]

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @commandTypes@
commandTypesSelector :: Selector
commandTypesSelector = mkSelector "commandTypes"

-- | @Selector@ for @setCommandTypes:@
setCommandTypesSelector :: Selector
setCommandTypesSelector = mkSelector "setCommandTypes:"

-- | @Selector@ for @inheritPipelineState@
inheritPipelineStateSelector :: Selector
inheritPipelineStateSelector = mkSelector "inheritPipelineState"

-- | @Selector@ for @setInheritPipelineState:@
setInheritPipelineStateSelector :: Selector
setInheritPipelineStateSelector = mkSelector "setInheritPipelineState:"

-- | @Selector@ for @inheritBuffers@
inheritBuffersSelector :: Selector
inheritBuffersSelector = mkSelector "inheritBuffers"

-- | @Selector@ for @setInheritBuffers:@
setInheritBuffersSelector :: Selector
setInheritBuffersSelector = mkSelector "setInheritBuffers:"

-- | @Selector@ for @inheritDepthStencilState@
inheritDepthStencilStateSelector :: Selector
inheritDepthStencilStateSelector = mkSelector "inheritDepthStencilState"

-- | @Selector@ for @setInheritDepthStencilState:@
setInheritDepthStencilStateSelector :: Selector
setInheritDepthStencilStateSelector = mkSelector "setInheritDepthStencilState:"

-- | @Selector@ for @inheritDepthBias@
inheritDepthBiasSelector :: Selector
inheritDepthBiasSelector = mkSelector "inheritDepthBias"

-- | @Selector@ for @setInheritDepthBias:@
setInheritDepthBiasSelector :: Selector
setInheritDepthBiasSelector = mkSelector "setInheritDepthBias:"

-- | @Selector@ for @inheritDepthClipMode@
inheritDepthClipModeSelector :: Selector
inheritDepthClipModeSelector = mkSelector "inheritDepthClipMode"

-- | @Selector@ for @setInheritDepthClipMode:@
setInheritDepthClipModeSelector :: Selector
setInheritDepthClipModeSelector = mkSelector "setInheritDepthClipMode:"

-- | @Selector@ for @inheritCullMode@
inheritCullModeSelector :: Selector
inheritCullModeSelector = mkSelector "inheritCullMode"

-- | @Selector@ for @setInheritCullMode:@
setInheritCullModeSelector :: Selector
setInheritCullModeSelector = mkSelector "setInheritCullMode:"

-- | @Selector@ for @inheritFrontFacingWinding@
inheritFrontFacingWindingSelector :: Selector
inheritFrontFacingWindingSelector = mkSelector "inheritFrontFacingWinding"

-- | @Selector@ for @setInheritFrontFacingWinding:@
setInheritFrontFacingWindingSelector :: Selector
setInheritFrontFacingWindingSelector = mkSelector "setInheritFrontFacingWinding:"

-- | @Selector@ for @inheritTriangleFillMode@
inheritTriangleFillModeSelector :: Selector
inheritTriangleFillModeSelector = mkSelector "inheritTriangleFillMode"

-- | @Selector@ for @setInheritTriangleFillMode:@
setInheritTriangleFillModeSelector :: Selector
setInheritTriangleFillModeSelector = mkSelector "setInheritTriangleFillMode:"

-- | @Selector@ for @maxVertexBufferBindCount@
maxVertexBufferBindCountSelector :: Selector
maxVertexBufferBindCountSelector = mkSelector "maxVertexBufferBindCount"

-- | @Selector@ for @setMaxVertexBufferBindCount:@
setMaxVertexBufferBindCountSelector :: Selector
setMaxVertexBufferBindCountSelector = mkSelector "setMaxVertexBufferBindCount:"

-- | @Selector@ for @maxFragmentBufferBindCount@
maxFragmentBufferBindCountSelector :: Selector
maxFragmentBufferBindCountSelector = mkSelector "maxFragmentBufferBindCount"

-- | @Selector@ for @setMaxFragmentBufferBindCount:@
setMaxFragmentBufferBindCountSelector :: Selector
setMaxFragmentBufferBindCountSelector = mkSelector "setMaxFragmentBufferBindCount:"

-- | @Selector@ for @maxKernelBufferBindCount@
maxKernelBufferBindCountSelector :: Selector
maxKernelBufferBindCountSelector = mkSelector "maxKernelBufferBindCount"

-- | @Selector@ for @setMaxKernelBufferBindCount:@
setMaxKernelBufferBindCountSelector :: Selector
setMaxKernelBufferBindCountSelector = mkSelector "setMaxKernelBufferBindCount:"

-- | @Selector@ for @maxKernelThreadgroupMemoryBindCount@
maxKernelThreadgroupMemoryBindCountSelector :: Selector
maxKernelThreadgroupMemoryBindCountSelector = mkSelector "maxKernelThreadgroupMemoryBindCount"

-- | @Selector@ for @setMaxKernelThreadgroupMemoryBindCount:@
setMaxKernelThreadgroupMemoryBindCountSelector :: Selector
setMaxKernelThreadgroupMemoryBindCountSelector = mkSelector "setMaxKernelThreadgroupMemoryBindCount:"

-- | @Selector@ for @maxObjectBufferBindCount@
maxObjectBufferBindCountSelector :: Selector
maxObjectBufferBindCountSelector = mkSelector "maxObjectBufferBindCount"

-- | @Selector@ for @setMaxObjectBufferBindCount:@
setMaxObjectBufferBindCountSelector :: Selector
setMaxObjectBufferBindCountSelector = mkSelector "setMaxObjectBufferBindCount:"

-- | @Selector@ for @maxMeshBufferBindCount@
maxMeshBufferBindCountSelector :: Selector
maxMeshBufferBindCountSelector = mkSelector "maxMeshBufferBindCount"

-- | @Selector@ for @setMaxMeshBufferBindCount:@
setMaxMeshBufferBindCountSelector :: Selector
setMaxMeshBufferBindCountSelector = mkSelector "setMaxMeshBufferBindCount:"

-- | @Selector@ for @maxObjectThreadgroupMemoryBindCount@
maxObjectThreadgroupMemoryBindCountSelector :: Selector
maxObjectThreadgroupMemoryBindCountSelector = mkSelector "maxObjectThreadgroupMemoryBindCount"

-- | @Selector@ for @setMaxObjectThreadgroupMemoryBindCount:@
setMaxObjectThreadgroupMemoryBindCountSelector :: Selector
setMaxObjectThreadgroupMemoryBindCountSelector = mkSelector "setMaxObjectThreadgroupMemoryBindCount:"

-- | @Selector@ for @supportRayTracing@
supportRayTracingSelector :: Selector
supportRayTracingSelector = mkSelector "supportRayTracing"

-- | @Selector@ for @setSupportRayTracing:@
setSupportRayTracingSelector :: Selector
setSupportRayTracingSelector = mkSelector "setSupportRayTracing:"

-- | @Selector@ for @supportDynamicAttributeStride@
supportDynamicAttributeStrideSelector :: Selector
supportDynamicAttributeStrideSelector = mkSelector "supportDynamicAttributeStride"

-- | @Selector@ for @setSupportDynamicAttributeStride:@
setSupportDynamicAttributeStrideSelector :: Selector
setSupportDynamicAttributeStrideSelector = mkSelector "setSupportDynamicAttributeStride:"

-- | @Selector@ for @supportColorAttachmentMapping@
supportColorAttachmentMappingSelector :: Selector
supportColorAttachmentMappingSelector = mkSelector "supportColorAttachmentMapping"

-- | @Selector@ for @setSupportColorAttachmentMapping:@
setSupportColorAttachmentMappingSelector :: Selector
setSupportColorAttachmentMappingSelector = mkSelector "setSupportColorAttachmentMapping:"

