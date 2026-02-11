{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Groups together properties you use to create a mesh render pipeline state object.
--
-- Compared to ``MTLMeshRenderPipelineDescriptor``, this interface doesn't offer a mechanism to hint to Metal mutability of object, mesh, or fragment buffers. Additionally, when you use this descriptor, you don't specify binary archives.
--
-- Generated bindings for @MTL4MeshRenderPipelineDescriptor@.
module ObjC.Metal.MTL4MeshRenderPipelineDescriptor
  ( MTL4MeshRenderPipelineDescriptor
  , IsMTL4MeshRenderPipelineDescriptor(..)
  , reset
  , objectFunctionDescriptor
  , setObjectFunctionDescriptor
  , meshFunctionDescriptor
  , setMeshFunctionDescriptor
  , fragmentFunctionDescriptor
  , setFragmentFunctionDescriptor
  , maxTotalThreadsPerObjectThreadgroup
  , setMaxTotalThreadsPerObjectThreadgroup
  , maxTotalThreadsPerMeshThreadgroup
  , setMaxTotalThreadsPerMeshThreadgroup
  , objectThreadgroupSizeIsMultipleOfThreadExecutionWidth
  , setObjectThreadgroupSizeIsMultipleOfThreadExecutionWidth
  , meshThreadgroupSizeIsMultipleOfThreadExecutionWidth
  , setMeshThreadgroupSizeIsMultipleOfThreadExecutionWidth
  , payloadMemoryLength
  , setPayloadMemoryLength
  , maxTotalThreadgroupsPerMeshGrid
  , setMaxTotalThreadgroupsPerMeshGrid
  , rasterSampleCount
  , setRasterSampleCount
  , alphaToCoverageState
  , setAlphaToCoverageState
  , alphaToOneState
  , setAlphaToOneState
  , rasterizationEnabled
  , setRasterizationEnabled
  , maxVertexAmplificationCount
  , setMaxVertexAmplificationCount
  , colorAttachments
  , objectStaticLinkingDescriptor
  , setObjectStaticLinkingDescriptor
  , meshStaticLinkingDescriptor
  , setMeshStaticLinkingDescriptor
  , fragmentStaticLinkingDescriptor
  , setFragmentStaticLinkingDescriptor
  , supportObjectBinaryLinking
  , setSupportObjectBinaryLinking
  , supportMeshBinaryLinking
  , setSupportMeshBinaryLinking
  , supportFragmentBinaryLinking
  , setSupportFragmentBinaryLinking
  , colorAttachmentMappingState
  , setColorAttachmentMappingState
  , supportIndirectCommandBuffers
  , setSupportIndirectCommandBuffers
  , resetSelector
  , objectFunctionDescriptorSelector
  , setObjectFunctionDescriptorSelector
  , meshFunctionDescriptorSelector
  , setMeshFunctionDescriptorSelector
  , fragmentFunctionDescriptorSelector
  , setFragmentFunctionDescriptorSelector
  , maxTotalThreadsPerObjectThreadgroupSelector
  , setMaxTotalThreadsPerObjectThreadgroupSelector
  , maxTotalThreadsPerMeshThreadgroupSelector
  , setMaxTotalThreadsPerMeshThreadgroupSelector
  , objectThreadgroupSizeIsMultipleOfThreadExecutionWidthSelector
  , setObjectThreadgroupSizeIsMultipleOfThreadExecutionWidthSelector
  , meshThreadgroupSizeIsMultipleOfThreadExecutionWidthSelector
  , setMeshThreadgroupSizeIsMultipleOfThreadExecutionWidthSelector
  , payloadMemoryLengthSelector
  , setPayloadMemoryLengthSelector
  , maxTotalThreadgroupsPerMeshGridSelector
  , setMaxTotalThreadgroupsPerMeshGridSelector
  , rasterSampleCountSelector
  , setRasterSampleCountSelector
  , alphaToCoverageStateSelector
  , setAlphaToCoverageStateSelector
  , alphaToOneStateSelector
  , setAlphaToOneStateSelector
  , rasterizationEnabledSelector
  , setRasterizationEnabledSelector
  , maxVertexAmplificationCountSelector
  , setMaxVertexAmplificationCountSelector
  , colorAttachmentsSelector
  , objectStaticLinkingDescriptorSelector
  , setObjectStaticLinkingDescriptorSelector
  , meshStaticLinkingDescriptorSelector
  , setMeshStaticLinkingDescriptorSelector
  , fragmentStaticLinkingDescriptorSelector
  , setFragmentStaticLinkingDescriptorSelector
  , supportObjectBinaryLinkingSelector
  , setSupportObjectBinaryLinkingSelector
  , supportMeshBinaryLinkingSelector
  , setSupportMeshBinaryLinkingSelector
  , supportFragmentBinaryLinkingSelector
  , setSupportFragmentBinaryLinkingSelector
  , colorAttachmentMappingStateSelector
  , setColorAttachmentMappingStateSelector
  , supportIndirectCommandBuffersSelector
  , setSupportIndirectCommandBuffersSelector

  -- * Enum types
  , MTL4AlphaToCoverageState(MTL4AlphaToCoverageState)
  , pattern MTL4AlphaToCoverageStateDisabled
  , pattern MTL4AlphaToCoverageStateEnabled
  , MTL4AlphaToOneState(MTL4AlphaToOneState)
  , pattern MTL4AlphaToOneStateDisabled
  , pattern MTL4AlphaToOneStateEnabled
  , MTL4IndirectCommandBufferSupportState(MTL4IndirectCommandBufferSupportState)
  , pattern MTL4IndirectCommandBufferSupportStateDisabled
  , pattern MTL4IndirectCommandBufferSupportStateEnabled
  , MTL4LogicalToPhysicalColorAttachmentMappingState(MTL4LogicalToPhysicalColorAttachmentMappingState)
  , pattern MTL4LogicalToPhysicalColorAttachmentMappingStateIdentity
  , pattern MTL4LogicalToPhysicalColorAttachmentMappingStateInherited

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

-- | Resets this descriptor to its default state.
--
-- ObjC selector: @- reset@
reset :: IsMTL4MeshRenderPipelineDescriptor mtL4MeshRenderPipelineDescriptor => mtL4MeshRenderPipelineDescriptor -> IO ()
reset mtL4MeshRenderPipelineDescriptor  =
  sendMsg mtL4MeshRenderPipelineDescriptor (mkSelector "reset") retVoid []

-- | Assigns a function descriptor representing the function this pipeline executes for each *object* in the object shader stage.
--
-- ObjC selector: @- objectFunctionDescriptor@
objectFunctionDescriptor :: IsMTL4MeshRenderPipelineDescriptor mtL4MeshRenderPipelineDescriptor => mtL4MeshRenderPipelineDescriptor -> IO (Id MTL4FunctionDescriptor)
objectFunctionDescriptor mtL4MeshRenderPipelineDescriptor  =
  sendMsg mtL4MeshRenderPipelineDescriptor (mkSelector "objectFunctionDescriptor") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | Assigns a function descriptor representing the function this pipeline executes for each *object* in the object shader stage.
--
-- ObjC selector: @- setObjectFunctionDescriptor:@
setObjectFunctionDescriptor :: (IsMTL4MeshRenderPipelineDescriptor mtL4MeshRenderPipelineDescriptor, IsMTL4FunctionDescriptor value) => mtL4MeshRenderPipelineDescriptor -> value -> IO ()
setObjectFunctionDescriptor mtL4MeshRenderPipelineDescriptor  value =
withObjCPtr value $ \raw_value ->
    sendMsg mtL4MeshRenderPipelineDescriptor (mkSelector "setObjectFunctionDescriptor:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- | Assigns a function descriptor representing the function this pipeline executes for each primitive in the mesh shader stage.
--
-- ObjC selector: @- meshFunctionDescriptor@
meshFunctionDescriptor :: IsMTL4MeshRenderPipelineDescriptor mtL4MeshRenderPipelineDescriptor => mtL4MeshRenderPipelineDescriptor -> IO (Id MTL4FunctionDescriptor)
meshFunctionDescriptor mtL4MeshRenderPipelineDescriptor  =
  sendMsg mtL4MeshRenderPipelineDescriptor (mkSelector "meshFunctionDescriptor") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | Assigns a function descriptor representing the function this pipeline executes for each primitive in the mesh shader stage.
--
-- ObjC selector: @- setMeshFunctionDescriptor:@
setMeshFunctionDescriptor :: (IsMTL4MeshRenderPipelineDescriptor mtL4MeshRenderPipelineDescriptor, IsMTL4FunctionDescriptor value) => mtL4MeshRenderPipelineDescriptor -> value -> IO ()
setMeshFunctionDescriptor mtL4MeshRenderPipelineDescriptor  value =
withObjCPtr value $ \raw_value ->
    sendMsg mtL4MeshRenderPipelineDescriptor (mkSelector "setMeshFunctionDescriptor:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- | Assigns a function descriptor representing the function this pipeline executes for each fragment.
--
-- ObjC selector: @- fragmentFunctionDescriptor@
fragmentFunctionDescriptor :: IsMTL4MeshRenderPipelineDescriptor mtL4MeshRenderPipelineDescriptor => mtL4MeshRenderPipelineDescriptor -> IO (Id MTL4FunctionDescriptor)
fragmentFunctionDescriptor mtL4MeshRenderPipelineDescriptor  =
  sendMsg mtL4MeshRenderPipelineDescriptor (mkSelector "fragmentFunctionDescriptor") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | Assigns a function descriptor representing the function this pipeline executes for each fragment.
--
-- ObjC selector: @- setFragmentFunctionDescriptor:@
setFragmentFunctionDescriptor :: (IsMTL4MeshRenderPipelineDescriptor mtL4MeshRenderPipelineDescriptor, IsMTL4FunctionDescriptor value) => mtL4MeshRenderPipelineDescriptor -> value -> IO ()
setFragmentFunctionDescriptor mtL4MeshRenderPipelineDescriptor  value =
withObjCPtr value $ \raw_value ->
    sendMsg mtL4MeshRenderPipelineDescriptor (mkSelector "setFragmentFunctionDescriptor:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- | Controls the largest number of threads the pipeline state can execute in a single object shader threadgroup dispatch.
--
-- This number represents the maximum size of the product of the components of parameter @threadsPerObjectThreadgroup@ that Metal can use when drawing with this pipeline in mesh shader dispatch methods, such as ``MTL4RenderCommandEncoder/drawMeshThreadgroups:threadsPerObjectThreadgroup:threadsPerMeshThreadgroup:``.
--
-- The compiler's optimizer can use the value of this property to generate more efficient code, specifically when the value doesn't exceed the thread execution width of the underlying GPU.
--
-- The default value of this property is @0@, which indicates that the number you pass to attribute @[[max_total_threads_per_threadgroup(N)]]@ of the pipeline's object function determines the maximum total threads per threadgroup.
--
-- When you specify both the @[[max_total_threads_per_threadgroup(N)]]@ attribute and this property, you are responsible for making sure these values match.
--
-- Additionally, you are responsible for ensuring this value doesn't exceed the "maximum threads per threadgroup" device limit documented in the "Metal Feature Set Tables" PDF: <https://developer.apple.com/metal/Metal-Feature-Set-Tables.pdf>.
--
-- ObjC selector: @- maxTotalThreadsPerObjectThreadgroup@
maxTotalThreadsPerObjectThreadgroup :: IsMTL4MeshRenderPipelineDescriptor mtL4MeshRenderPipelineDescriptor => mtL4MeshRenderPipelineDescriptor -> IO CULong
maxTotalThreadsPerObjectThreadgroup mtL4MeshRenderPipelineDescriptor  =
  sendMsg mtL4MeshRenderPipelineDescriptor (mkSelector "maxTotalThreadsPerObjectThreadgroup") retCULong []

-- | Controls the largest number of threads the pipeline state can execute in a single object shader threadgroup dispatch.
--
-- This number represents the maximum size of the product of the components of parameter @threadsPerObjectThreadgroup@ that Metal can use when drawing with this pipeline in mesh shader dispatch methods, such as ``MTL4RenderCommandEncoder/drawMeshThreadgroups:threadsPerObjectThreadgroup:threadsPerMeshThreadgroup:``.
--
-- The compiler's optimizer can use the value of this property to generate more efficient code, specifically when the value doesn't exceed the thread execution width of the underlying GPU.
--
-- The default value of this property is @0@, which indicates that the number you pass to attribute @[[max_total_threads_per_threadgroup(N)]]@ of the pipeline's object function determines the maximum total threads per threadgroup.
--
-- When you specify both the @[[max_total_threads_per_threadgroup(N)]]@ attribute and this property, you are responsible for making sure these values match.
--
-- Additionally, you are responsible for ensuring this value doesn't exceed the "maximum threads per threadgroup" device limit documented in the "Metal Feature Set Tables" PDF: <https://developer.apple.com/metal/Metal-Feature-Set-Tables.pdf>.
--
-- ObjC selector: @- setMaxTotalThreadsPerObjectThreadgroup:@
setMaxTotalThreadsPerObjectThreadgroup :: IsMTL4MeshRenderPipelineDescriptor mtL4MeshRenderPipelineDescriptor => mtL4MeshRenderPipelineDescriptor -> CULong -> IO ()
setMaxTotalThreadsPerObjectThreadgroup mtL4MeshRenderPipelineDescriptor  value =
  sendMsg mtL4MeshRenderPipelineDescriptor (mkSelector "setMaxTotalThreadsPerObjectThreadgroup:") retVoid [argCULong (fromIntegral value)]

-- | Controls the largest number of threads the pipeline state can execute in a single mesh shader threadgroup dispatch.
--
-- This number represents the maximum size of the product of the components of parameter @threadsPerMeshThreadgroup@ that Metal can use when drawing with this pipeline in mesh shader dispatch methods, such as ``MTL4RenderCommandEncoder/drawMeshThreadgroups:threadsPerObjectThreadgroup:threadsPerMeshThreadgroup:``.
--
-- The compiler's optimizer can use the value of this property to generate more efficient code, specifically when the value doesn't exceed the thread execution width of the underlying GPU.
--
-- The default value of this property is @0@, thish indicates that the Metal Shader Language attribute @[[max_total_threads_per_threadgroup]]@ you attache to the pipeline's mesh shader function determines the value of this property.
--
-- When you specify both the @[[max_total_threads_per_threadgroup(N)]]@ attribute and this property, you are responsible for making sure these values match.
--
-- Additionally, you are responsible for ensuring this value doesn't exceed the "maximum threads per threadgroup" device limit documented in the "Metal Feature Set Tables" PDF: <https://developer.apple.com/metal/Metal-Feature-Set-Tables.pdf>.
--
-- ObjC selector: @- maxTotalThreadsPerMeshThreadgroup@
maxTotalThreadsPerMeshThreadgroup :: IsMTL4MeshRenderPipelineDescriptor mtL4MeshRenderPipelineDescriptor => mtL4MeshRenderPipelineDescriptor -> IO CULong
maxTotalThreadsPerMeshThreadgroup mtL4MeshRenderPipelineDescriptor  =
  sendMsg mtL4MeshRenderPipelineDescriptor (mkSelector "maxTotalThreadsPerMeshThreadgroup") retCULong []

-- | Controls the largest number of threads the pipeline state can execute in a single mesh shader threadgroup dispatch.
--
-- This number represents the maximum size of the product of the components of parameter @threadsPerMeshThreadgroup@ that Metal can use when drawing with this pipeline in mesh shader dispatch methods, such as ``MTL4RenderCommandEncoder/drawMeshThreadgroups:threadsPerObjectThreadgroup:threadsPerMeshThreadgroup:``.
--
-- The compiler's optimizer can use the value of this property to generate more efficient code, specifically when the value doesn't exceed the thread execution width of the underlying GPU.
--
-- The default value of this property is @0@, thish indicates that the Metal Shader Language attribute @[[max_total_threads_per_threadgroup]]@ you attache to the pipeline's mesh shader function determines the value of this property.
--
-- When you specify both the @[[max_total_threads_per_threadgroup(N)]]@ attribute and this property, you are responsible for making sure these values match.
--
-- Additionally, you are responsible for ensuring this value doesn't exceed the "maximum threads per threadgroup" device limit documented in the "Metal Feature Set Tables" PDF: <https://developer.apple.com/metal/Metal-Feature-Set-Tables.pdf>.
--
-- ObjC selector: @- setMaxTotalThreadsPerMeshThreadgroup:@
setMaxTotalThreadsPerMeshThreadgroup :: IsMTL4MeshRenderPipelineDescriptor mtL4MeshRenderPipelineDescriptor => mtL4MeshRenderPipelineDescriptor -> CULong -> IO ()
setMaxTotalThreadsPerMeshThreadgroup mtL4MeshRenderPipelineDescriptor  value =
  sendMsg mtL4MeshRenderPipelineDescriptor (mkSelector "setMaxTotalThreadsPerMeshThreadgroup:") retVoid [argCULong (fromIntegral value)]

-- | Provides a guarantee to Metal regarding the number of threadgroup threads for the object stage of a pipeline you create from this descriptor.
--
-- If you set this property to <doc://com.apple.documentation/documentation/swift/true>, you state to Metal that when you use a mesh render pipeline you create from this descriptor, the number of threadgroup threads you dispatch for the object stage is a multiple of its ``MTLRenderPipelineState/objectThreadExecutionWidth``. The compiler's optimizer can use this guarantee to generate  more efficient code.
--
-- This property's default value is <doc://com.apple.documentation/documentation/swift/false>.
--
-- ObjC selector: @- objectThreadgroupSizeIsMultipleOfThreadExecutionWidth@
objectThreadgroupSizeIsMultipleOfThreadExecutionWidth :: IsMTL4MeshRenderPipelineDescriptor mtL4MeshRenderPipelineDescriptor => mtL4MeshRenderPipelineDescriptor -> IO Bool
objectThreadgroupSizeIsMultipleOfThreadExecutionWidth mtL4MeshRenderPipelineDescriptor  =
  fmap ((/= 0) :: CULong -> Bool) $ sendMsg mtL4MeshRenderPipelineDescriptor (mkSelector "objectThreadgroupSizeIsMultipleOfThreadExecutionWidth") retCULong []

-- | Provides a guarantee to Metal regarding the number of threadgroup threads for the object stage of a pipeline you create from this descriptor.
--
-- If you set this property to <doc://com.apple.documentation/documentation/swift/true>, you state to Metal that when you use a mesh render pipeline you create from this descriptor, the number of threadgroup threads you dispatch for the object stage is a multiple of its ``MTLRenderPipelineState/objectThreadExecutionWidth``. The compiler's optimizer can use this guarantee to generate  more efficient code.
--
-- This property's default value is <doc://com.apple.documentation/documentation/swift/false>.
--
-- ObjC selector: @- setObjectThreadgroupSizeIsMultipleOfThreadExecutionWidth:@
setObjectThreadgroupSizeIsMultipleOfThreadExecutionWidth :: IsMTL4MeshRenderPipelineDescriptor mtL4MeshRenderPipelineDescriptor => mtL4MeshRenderPipelineDescriptor -> Bool -> IO ()
setObjectThreadgroupSizeIsMultipleOfThreadExecutionWidth mtL4MeshRenderPipelineDescriptor  value =
  sendMsg mtL4MeshRenderPipelineDescriptor (mkSelector "setObjectThreadgroupSizeIsMultipleOfThreadExecutionWidth:") retVoid [argCULong (if value then 1 else 0)]

-- | Provides a guarantee to Metal regarding the number of threadgroup threads for the mesh stage of a pipeline you create from this descriptor.
--
-- If you set this property to <doc://com.apple.documentation/documentation/swift/true>, you state to Metal that when you use a mesh render pipeline you create from this descriptor, the number of threadgroup threads you dispatch for the mesh stage is a multiple of its ``MTLRenderPipelineState/meshThreadExecutionWidth``. The compiler's optimizer can use this guarantee to generate more efficient code.
--
-- This property's default value is <doc://com.apple.documentation/documentation/swift/false>.
--
-- ObjC selector: @- meshThreadgroupSizeIsMultipleOfThreadExecutionWidth@
meshThreadgroupSizeIsMultipleOfThreadExecutionWidth :: IsMTL4MeshRenderPipelineDescriptor mtL4MeshRenderPipelineDescriptor => mtL4MeshRenderPipelineDescriptor -> IO Bool
meshThreadgroupSizeIsMultipleOfThreadExecutionWidth mtL4MeshRenderPipelineDescriptor  =
  fmap ((/= 0) :: CULong -> Bool) $ sendMsg mtL4MeshRenderPipelineDescriptor (mkSelector "meshThreadgroupSizeIsMultipleOfThreadExecutionWidth") retCULong []

-- | Provides a guarantee to Metal regarding the number of threadgroup threads for the mesh stage of a pipeline you create from this descriptor.
--
-- If you set this property to <doc://com.apple.documentation/documentation/swift/true>, you state to Metal that when you use a mesh render pipeline you create from this descriptor, the number of threadgroup threads you dispatch for the mesh stage is a multiple of its ``MTLRenderPipelineState/meshThreadExecutionWidth``. The compiler's optimizer can use this guarantee to generate more efficient code.
--
-- This property's default value is <doc://com.apple.documentation/documentation/swift/false>.
--
-- ObjC selector: @- setMeshThreadgroupSizeIsMultipleOfThreadExecutionWidth:@
setMeshThreadgroupSizeIsMultipleOfThreadExecutionWidth :: IsMTL4MeshRenderPipelineDescriptor mtL4MeshRenderPipelineDescriptor => mtL4MeshRenderPipelineDescriptor -> Bool -> IO ()
setMeshThreadgroupSizeIsMultipleOfThreadExecutionWidth mtL4MeshRenderPipelineDescriptor  value =
  sendMsg mtL4MeshRenderPipelineDescriptor (mkSelector "setMeshThreadgroupSizeIsMultipleOfThreadExecutionWidth:") retVoid [argCULong (if value then 1 else 0)]

-- | Reserves storage for the object-to-mesh stage payload.
--
-- This property determines the size, in bytes, of the buffer you indicate via the Metal Shading Language @[[payload]]@ attribute in the object and mesh shader functions of the mesh render pipeline.
--
-- If this value is @0@, Metal derives the size from the (dereferenced) type you declare for the payload in the object shader function. If the type is a pointer, Metal reserves space for a single element.
--
-- The default value is @0@.
--
-- ObjC selector: @- payloadMemoryLength@
payloadMemoryLength :: IsMTL4MeshRenderPipelineDescriptor mtL4MeshRenderPipelineDescriptor => mtL4MeshRenderPipelineDescriptor -> IO CULong
payloadMemoryLength mtL4MeshRenderPipelineDescriptor  =
  sendMsg mtL4MeshRenderPipelineDescriptor (mkSelector "payloadMemoryLength") retCULong []

-- | Reserves storage for the object-to-mesh stage payload.
--
-- This property determines the size, in bytes, of the buffer you indicate via the Metal Shading Language @[[payload]]@ attribute in the object and mesh shader functions of the mesh render pipeline.
--
-- If this value is @0@, Metal derives the size from the (dereferenced) type you declare for the payload in the object shader function. If the type is a pointer, Metal reserves space for a single element.
--
-- The default value is @0@.
--
-- ObjC selector: @- setPayloadMemoryLength:@
setPayloadMemoryLength :: IsMTL4MeshRenderPipelineDescriptor mtL4MeshRenderPipelineDescriptor => mtL4MeshRenderPipelineDescriptor -> CULong -> IO ()
setPayloadMemoryLength mtL4MeshRenderPipelineDescriptor  value =
  sendMsg mtL4MeshRenderPipelineDescriptor (mkSelector "setPayloadMemoryLength:") retVoid [argCULong (fromIntegral value)]

-- | Controls the largest number of threads the pipeline state can execute when the object stage of a mesh render pipeline you create from this descriptor dispatches its mesh stage.
--
-- This number represents the maximum size of the product of the components of the parameter you pass to Metal Shading Language's built-in function @mesh_grid_properties::set_threadgroups_per_grid@.
--
-- The default value of this property is @0@, which indicates that the Metal Shading Language attribute @[[max_total_threadgroups_per_mesh_grid(N)]]@ you attach to the pipeline's mesh shader function determines the value of this property.
--
-- When you specify both the @[[max_total_threadgroups_per_mesh_grid(N)]]@ attribute and this property, you are responsible for making sure these values match.
--
-- Additionally, you are responsible for ensuring this value doesn't exceed the "maximum threads per mesh grid" device limit documented in the "Metal Feature Set Tables" PDF: <https://developer.apple.com/metal/Metal-Feature-Set-Tables.pdf>.
--
-- ObjC selector: @- maxTotalThreadgroupsPerMeshGrid@
maxTotalThreadgroupsPerMeshGrid :: IsMTL4MeshRenderPipelineDescriptor mtL4MeshRenderPipelineDescriptor => mtL4MeshRenderPipelineDescriptor -> IO CULong
maxTotalThreadgroupsPerMeshGrid mtL4MeshRenderPipelineDescriptor  =
  sendMsg mtL4MeshRenderPipelineDescriptor (mkSelector "maxTotalThreadgroupsPerMeshGrid") retCULong []

-- | Controls the largest number of threads the pipeline state can execute when the object stage of a mesh render pipeline you create from this descriptor dispatches its mesh stage.
--
-- This number represents the maximum size of the product of the components of the parameter you pass to Metal Shading Language's built-in function @mesh_grid_properties::set_threadgroups_per_grid@.
--
-- The default value of this property is @0@, which indicates that the Metal Shading Language attribute @[[max_total_threadgroups_per_mesh_grid(N)]]@ you attach to the pipeline's mesh shader function determines the value of this property.
--
-- When you specify both the @[[max_total_threadgroups_per_mesh_grid(N)]]@ attribute and this property, you are responsible for making sure these values match.
--
-- Additionally, you are responsible for ensuring this value doesn't exceed the "maximum threads per mesh grid" device limit documented in the "Metal Feature Set Tables" PDF: <https://developer.apple.com/metal/Metal-Feature-Set-Tables.pdf>.
--
-- ObjC selector: @- setMaxTotalThreadgroupsPerMeshGrid:@
setMaxTotalThreadgroupsPerMeshGrid :: IsMTL4MeshRenderPipelineDescriptor mtL4MeshRenderPipelineDescriptor => mtL4MeshRenderPipelineDescriptor -> CULong -> IO ()
setMaxTotalThreadgroupsPerMeshGrid mtL4MeshRenderPipelineDescriptor  value =
  sendMsg mtL4MeshRenderPipelineDescriptor (mkSelector "setMaxTotalThreadgroupsPerMeshGrid:") retVoid [argCULong (fromIntegral value)]

-- | Sets number of samples this pipeline applies for each fragment.
--
-- ObjC selector: @- rasterSampleCount@
rasterSampleCount :: IsMTL4MeshRenderPipelineDescriptor mtL4MeshRenderPipelineDescriptor => mtL4MeshRenderPipelineDescriptor -> IO CULong
rasterSampleCount mtL4MeshRenderPipelineDescriptor  =
  sendMsg mtL4MeshRenderPipelineDescriptor (mkSelector "rasterSampleCount") retCULong []

-- | Sets number of samples this pipeline applies for each fragment.
--
-- ObjC selector: @- setRasterSampleCount:@
setRasterSampleCount :: IsMTL4MeshRenderPipelineDescriptor mtL4MeshRenderPipelineDescriptor => mtL4MeshRenderPipelineDescriptor -> CULong -> IO ()
setRasterSampleCount mtL4MeshRenderPipelineDescriptor  value =
  sendMsg mtL4MeshRenderPipelineDescriptor (mkSelector "setRasterSampleCount:") retVoid [argCULong (fromIntegral value)]

-- | Indicates whether to read and use the alpha channel fragment output of color attachments to compute a sample coverage mask.
--
-- ObjC selector: @- alphaToCoverageState@
alphaToCoverageState :: IsMTL4MeshRenderPipelineDescriptor mtL4MeshRenderPipelineDescriptor => mtL4MeshRenderPipelineDescriptor -> IO MTL4AlphaToCoverageState
alphaToCoverageState mtL4MeshRenderPipelineDescriptor  =
  fmap (coerce :: CLong -> MTL4AlphaToCoverageState) $ sendMsg mtL4MeshRenderPipelineDescriptor (mkSelector "alphaToCoverageState") retCLong []

-- | Indicates whether to read and use the alpha channel fragment output of color attachments to compute a sample coverage mask.
--
-- ObjC selector: @- setAlphaToCoverageState:@
setAlphaToCoverageState :: IsMTL4MeshRenderPipelineDescriptor mtL4MeshRenderPipelineDescriptor => mtL4MeshRenderPipelineDescriptor -> MTL4AlphaToCoverageState -> IO ()
setAlphaToCoverageState mtL4MeshRenderPipelineDescriptor  value =
  sendMsg mtL4MeshRenderPipelineDescriptor (mkSelector "setAlphaToCoverageState:") retVoid [argCLong (coerce value)]

-- | Indicates whether the pipeline forces alpha channel values of color attachments to the largest representable value.
--
-- ObjC selector: @- alphaToOneState@
alphaToOneState :: IsMTL4MeshRenderPipelineDescriptor mtL4MeshRenderPipelineDescriptor => mtL4MeshRenderPipelineDescriptor -> IO MTL4AlphaToOneState
alphaToOneState mtL4MeshRenderPipelineDescriptor  =
  fmap (coerce :: CLong -> MTL4AlphaToOneState) $ sendMsg mtL4MeshRenderPipelineDescriptor (mkSelector "alphaToOneState") retCLong []

-- | Indicates whether the pipeline forces alpha channel values of color attachments to the largest representable value.
--
-- ObjC selector: @- setAlphaToOneState:@
setAlphaToOneState :: IsMTL4MeshRenderPipelineDescriptor mtL4MeshRenderPipelineDescriptor => mtL4MeshRenderPipelineDescriptor -> MTL4AlphaToOneState -> IO ()
setAlphaToOneState mtL4MeshRenderPipelineDescriptor  value =
  sendMsg mtL4MeshRenderPipelineDescriptor (mkSelector "setAlphaToOneState:") retVoid [argCLong (coerce value)]

-- | Determines whether the pipeline rasterizes primitives.
--
-- By default, this value is <doc://com.apple.documentation/documentation/swift/true>, specifying that this pipeline rasterizes primitives. Set this property to <doc://com.apple.documentation/documentation/swift/false> when you don't provide a fragment shader function via function ``fragmentFunctionDescriptor``.
--
-- ObjC selector: @- rasterizationEnabled@
rasterizationEnabled :: IsMTL4MeshRenderPipelineDescriptor mtL4MeshRenderPipelineDescriptor => mtL4MeshRenderPipelineDescriptor -> IO Bool
rasterizationEnabled mtL4MeshRenderPipelineDescriptor  =
  fmap ((/= 0) :: CULong -> Bool) $ sendMsg mtL4MeshRenderPipelineDescriptor (mkSelector "rasterizationEnabled") retCULong []

-- | Determines whether the pipeline rasterizes primitives.
--
-- By default, this value is <doc://com.apple.documentation/documentation/swift/true>, specifying that this pipeline rasterizes primitives. Set this property to <doc://com.apple.documentation/documentation/swift/false> when you don't provide a fragment shader function via function ``fragmentFunctionDescriptor``.
--
-- ObjC selector: @- setRasterizationEnabled:@
setRasterizationEnabled :: IsMTL4MeshRenderPipelineDescriptor mtL4MeshRenderPipelineDescriptor => mtL4MeshRenderPipelineDescriptor -> Bool -> IO ()
setRasterizationEnabled mtL4MeshRenderPipelineDescriptor  value =
  sendMsg mtL4MeshRenderPipelineDescriptor (mkSelector "setRasterizationEnabled:") retVoid [argCULong (if value then 1 else 0)]

-- | Determines the maximum value that can you can pass as the pipeline's amplification count.
--
-- This property controls the maximum count you pass to ``MTL4RenderCommandEncoder/setVertexAmplificationCount:viewMappings:`` when using vertex amplification with this pipeline.
--
-- ObjC selector: @- maxVertexAmplificationCount@
maxVertexAmplificationCount :: IsMTL4MeshRenderPipelineDescriptor mtL4MeshRenderPipelineDescriptor => mtL4MeshRenderPipelineDescriptor -> IO CULong
maxVertexAmplificationCount mtL4MeshRenderPipelineDescriptor  =
  sendMsg mtL4MeshRenderPipelineDescriptor (mkSelector "maxVertexAmplificationCount") retCULong []

-- | Determines the maximum value that can you can pass as the pipeline's amplification count.
--
-- This property controls the maximum count you pass to ``MTL4RenderCommandEncoder/setVertexAmplificationCount:viewMappings:`` when using vertex amplification with this pipeline.
--
-- ObjC selector: @- setMaxVertexAmplificationCount:@
setMaxVertexAmplificationCount :: IsMTL4MeshRenderPipelineDescriptor mtL4MeshRenderPipelineDescriptor => mtL4MeshRenderPipelineDescriptor -> CULong -> IO ()
setMaxVertexAmplificationCount mtL4MeshRenderPipelineDescriptor  value =
  sendMsg mtL4MeshRenderPipelineDescriptor (mkSelector "setMaxVertexAmplificationCount:") retVoid [argCULong (fromIntegral value)]

-- | Accesses an array containing descriptions of the color attachments this pipeline writes to.
--
-- ObjC selector: @- colorAttachments@
colorAttachments :: IsMTL4MeshRenderPipelineDescriptor mtL4MeshRenderPipelineDescriptor => mtL4MeshRenderPipelineDescriptor -> IO (Id MTL4RenderPipelineColorAttachmentDescriptorArray)
colorAttachments mtL4MeshRenderPipelineDescriptor  =
  sendMsg mtL4MeshRenderPipelineDescriptor (mkSelector "colorAttachments") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | Provides static linking information for the object stage of the render pipeline.
--
-- Use this property to link extra shader functions to the object stage of the render pipeline.
--
-- ObjC selector: @- objectStaticLinkingDescriptor@
objectStaticLinkingDescriptor :: IsMTL4MeshRenderPipelineDescriptor mtL4MeshRenderPipelineDescriptor => mtL4MeshRenderPipelineDescriptor -> IO (Id MTL4StaticLinkingDescriptor)
objectStaticLinkingDescriptor mtL4MeshRenderPipelineDescriptor  =
  sendMsg mtL4MeshRenderPipelineDescriptor (mkSelector "objectStaticLinkingDescriptor") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | Provides static linking information for the object stage of the render pipeline.
--
-- Use this property to link extra shader functions to the object stage of the render pipeline.
--
-- ObjC selector: @- setObjectStaticLinkingDescriptor:@
setObjectStaticLinkingDescriptor :: (IsMTL4MeshRenderPipelineDescriptor mtL4MeshRenderPipelineDescriptor, IsMTL4StaticLinkingDescriptor value) => mtL4MeshRenderPipelineDescriptor -> value -> IO ()
setObjectStaticLinkingDescriptor mtL4MeshRenderPipelineDescriptor  value =
withObjCPtr value $ \raw_value ->
    sendMsg mtL4MeshRenderPipelineDescriptor (mkSelector "setObjectStaticLinkingDescriptor:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- | Provides static linking information for the mesh stage of the render pipeline.
--
-- Use this property to link extra shader functions to the mesh stage of the render pipeline.
--
-- ObjC selector: @- meshStaticLinkingDescriptor@
meshStaticLinkingDescriptor :: IsMTL4MeshRenderPipelineDescriptor mtL4MeshRenderPipelineDescriptor => mtL4MeshRenderPipelineDescriptor -> IO (Id MTL4StaticLinkingDescriptor)
meshStaticLinkingDescriptor mtL4MeshRenderPipelineDescriptor  =
  sendMsg mtL4MeshRenderPipelineDescriptor (mkSelector "meshStaticLinkingDescriptor") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | Provides static linking information for the mesh stage of the render pipeline.
--
-- Use this property to link extra shader functions to the mesh stage of the render pipeline.
--
-- ObjC selector: @- setMeshStaticLinkingDescriptor:@
setMeshStaticLinkingDescriptor :: (IsMTL4MeshRenderPipelineDescriptor mtL4MeshRenderPipelineDescriptor, IsMTL4StaticLinkingDescriptor value) => mtL4MeshRenderPipelineDescriptor -> value -> IO ()
setMeshStaticLinkingDescriptor mtL4MeshRenderPipelineDescriptor  value =
withObjCPtr value $ \raw_value ->
    sendMsg mtL4MeshRenderPipelineDescriptor (mkSelector "setMeshStaticLinkingDescriptor:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- | Provides static linking information for the fragment stage of the render pipeline.
--
-- Use this property to link extra shader functions to the fragment stage of the render pipeline.
--
-- ObjC selector: @- fragmentStaticLinkingDescriptor@
fragmentStaticLinkingDescriptor :: IsMTL4MeshRenderPipelineDescriptor mtL4MeshRenderPipelineDescriptor => mtL4MeshRenderPipelineDescriptor -> IO (Id MTL4StaticLinkingDescriptor)
fragmentStaticLinkingDescriptor mtL4MeshRenderPipelineDescriptor  =
  sendMsg mtL4MeshRenderPipelineDescriptor (mkSelector "fragmentStaticLinkingDescriptor") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | Provides static linking information for the fragment stage of the render pipeline.
--
-- Use this property to link extra shader functions to the fragment stage of the render pipeline.
--
-- ObjC selector: @- setFragmentStaticLinkingDescriptor:@
setFragmentStaticLinkingDescriptor :: (IsMTL4MeshRenderPipelineDescriptor mtL4MeshRenderPipelineDescriptor, IsMTL4StaticLinkingDescriptor value) => mtL4MeshRenderPipelineDescriptor -> value -> IO ()
setFragmentStaticLinkingDescriptor mtL4MeshRenderPipelineDescriptor  value =
withObjCPtr value $ \raw_value ->
    sendMsg mtL4MeshRenderPipelineDescriptor (mkSelector "setFragmentStaticLinkingDescriptor:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- | Indicates whether you can use the render pipeline to create new pipelines by adding binary functions to the object shader function’s callable functions list.
--
-- ObjC selector: @- supportObjectBinaryLinking@
supportObjectBinaryLinking :: IsMTL4MeshRenderPipelineDescriptor mtL4MeshRenderPipelineDescriptor => mtL4MeshRenderPipelineDescriptor -> IO Bool
supportObjectBinaryLinking mtL4MeshRenderPipelineDescriptor  =
  fmap ((/= 0) :: CULong -> Bool) $ sendMsg mtL4MeshRenderPipelineDescriptor (mkSelector "supportObjectBinaryLinking") retCULong []

-- | Indicates whether you can use the render pipeline to create new pipelines by adding binary functions to the object shader function’s callable functions list.
--
-- ObjC selector: @- setSupportObjectBinaryLinking:@
setSupportObjectBinaryLinking :: IsMTL4MeshRenderPipelineDescriptor mtL4MeshRenderPipelineDescriptor => mtL4MeshRenderPipelineDescriptor -> Bool -> IO ()
setSupportObjectBinaryLinking mtL4MeshRenderPipelineDescriptor  value =
  sendMsg mtL4MeshRenderPipelineDescriptor (mkSelector "setSupportObjectBinaryLinking:") retVoid [argCULong (if value then 1 else 0)]

-- | Indicates whether you can use the render pipeline to create new pipelines by adding binary functions to the mesh shader function’s callable functions list.
--
-- ObjC selector: @- supportMeshBinaryLinking@
supportMeshBinaryLinking :: IsMTL4MeshRenderPipelineDescriptor mtL4MeshRenderPipelineDescriptor => mtL4MeshRenderPipelineDescriptor -> IO Bool
supportMeshBinaryLinking mtL4MeshRenderPipelineDescriptor  =
  fmap ((/= 0) :: CULong -> Bool) $ sendMsg mtL4MeshRenderPipelineDescriptor (mkSelector "supportMeshBinaryLinking") retCULong []

-- | Indicates whether you can use the render pipeline to create new pipelines by adding binary functions to the mesh shader function’s callable functions list.
--
-- ObjC selector: @- setSupportMeshBinaryLinking:@
setSupportMeshBinaryLinking :: IsMTL4MeshRenderPipelineDescriptor mtL4MeshRenderPipelineDescriptor => mtL4MeshRenderPipelineDescriptor -> Bool -> IO ()
setSupportMeshBinaryLinking mtL4MeshRenderPipelineDescriptor  value =
  sendMsg mtL4MeshRenderPipelineDescriptor (mkSelector "setSupportMeshBinaryLinking:") retVoid [argCULong (if value then 1 else 0)]

-- | Indicates whether you can use the render pipeline to create new pipelines by adding binary functions to the fragment shader function’s callable functions list.
--
-- ObjC selector: @- supportFragmentBinaryLinking@
supportFragmentBinaryLinking :: IsMTL4MeshRenderPipelineDescriptor mtL4MeshRenderPipelineDescriptor => mtL4MeshRenderPipelineDescriptor -> IO Bool
supportFragmentBinaryLinking mtL4MeshRenderPipelineDescriptor  =
  fmap ((/= 0) :: CULong -> Bool) $ sendMsg mtL4MeshRenderPipelineDescriptor (mkSelector "supportFragmentBinaryLinking") retCULong []

-- | Indicates whether you can use the render pipeline to create new pipelines by adding binary functions to the fragment shader function’s callable functions list.
--
-- ObjC selector: @- setSupportFragmentBinaryLinking:@
setSupportFragmentBinaryLinking :: IsMTL4MeshRenderPipelineDescriptor mtL4MeshRenderPipelineDescriptor => mtL4MeshRenderPipelineDescriptor -> Bool -> IO ()
setSupportFragmentBinaryLinking mtL4MeshRenderPipelineDescriptor  value =
  sendMsg mtL4MeshRenderPipelineDescriptor (mkSelector "setSupportFragmentBinaryLinking:") retVoid [argCULong (if value then 1 else 0)]

-- | Sets the logical-to-physical rendering remap state.
--
-- Use this property to assign how a ``MTL4RenderCommandEncoder`` instance maps the output of your fragment shader to physical color attachments.
--
-- ObjC selector: @- colorAttachmentMappingState@
colorAttachmentMappingState :: IsMTL4MeshRenderPipelineDescriptor mtL4MeshRenderPipelineDescriptor => mtL4MeshRenderPipelineDescriptor -> IO MTL4LogicalToPhysicalColorAttachmentMappingState
colorAttachmentMappingState mtL4MeshRenderPipelineDescriptor  =
  fmap (coerce :: CLong -> MTL4LogicalToPhysicalColorAttachmentMappingState) $ sendMsg mtL4MeshRenderPipelineDescriptor (mkSelector "colorAttachmentMappingState") retCLong []

-- | Sets the logical-to-physical rendering remap state.
--
-- Use this property to assign how a ``MTL4RenderCommandEncoder`` instance maps the output of your fragment shader to physical color attachments.
--
-- ObjC selector: @- setColorAttachmentMappingState:@
setColorAttachmentMappingState :: IsMTL4MeshRenderPipelineDescriptor mtL4MeshRenderPipelineDescriptor => mtL4MeshRenderPipelineDescriptor -> MTL4LogicalToPhysicalColorAttachmentMappingState -> IO ()
setColorAttachmentMappingState mtL4MeshRenderPipelineDescriptor  value =
  sendMsg mtL4MeshRenderPipelineDescriptor (mkSelector "setColorAttachmentMappingState:") retVoid [argCLong (coerce value)]

-- | Indicates whether the pipeline supports indirect command buffers.
--
-- ObjC selector: @- supportIndirectCommandBuffers@
supportIndirectCommandBuffers :: IsMTL4MeshRenderPipelineDescriptor mtL4MeshRenderPipelineDescriptor => mtL4MeshRenderPipelineDescriptor -> IO MTL4IndirectCommandBufferSupportState
supportIndirectCommandBuffers mtL4MeshRenderPipelineDescriptor  =
  fmap (coerce :: CLong -> MTL4IndirectCommandBufferSupportState) $ sendMsg mtL4MeshRenderPipelineDescriptor (mkSelector "supportIndirectCommandBuffers") retCLong []

-- | Indicates whether the pipeline supports indirect command buffers.
--
-- ObjC selector: @- setSupportIndirectCommandBuffers:@
setSupportIndirectCommandBuffers :: IsMTL4MeshRenderPipelineDescriptor mtL4MeshRenderPipelineDescriptor => mtL4MeshRenderPipelineDescriptor -> MTL4IndirectCommandBufferSupportState -> IO ()
setSupportIndirectCommandBuffers mtL4MeshRenderPipelineDescriptor  value =
  sendMsg mtL4MeshRenderPipelineDescriptor (mkSelector "setSupportIndirectCommandBuffers:") retVoid [argCLong (coerce value)]

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @reset@
resetSelector :: Selector
resetSelector = mkSelector "reset"

-- | @Selector@ for @objectFunctionDescriptor@
objectFunctionDescriptorSelector :: Selector
objectFunctionDescriptorSelector = mkSelector "objectFunctionDescriptor"

-- | @Selector@ for @setObjectFunctionDescriptor:@
setObjectFunctionDescriptorSelector :: Selector
setObjectFunctionDescriptorSelector = mkSelector "setObjectFunctionDescriptor:"

-- | @Selector@ for @meshFunctionDescriptor@
meshFunctionDescriptorSelector :: Selector
meshFunctionDescriptorSelector = mkSelector "meshFunctionDescriptor"

-- | @Selector@ for @setMeshFunctionDescriptor:@
setMeshFunctionDescriptorSelector :: Selector
setMeshFunctionDescriptorSelector = mkSelector "setMeshFunctionDescriptor:"

-- | @Selector@ for @fragmentFunctionDescriptor@
fragmentFunctionDescriptorSelector :: Selector
fragmentFunctionDescriptorSelector = mkSelector "fragmentFunctionDescriptor"

-- | @Selector@ for @setFragmentFunctionDescriptor:@
setFragmentFunctionDescriptorSelector :: Selector
setFragmentFunctionDescriptorSelector = mkSelector "setFragmentFunctionDescriptor:"

-- | @Selector@ for @maxTotalThreadsPerObjectThreadgroup@
maxTotalThreadsPerObjectThreadgroupSelector :: Selector
maxTotalThreadsPerObjectThreadgroupSelector = mkSelector "maxTotalThreadsPerObjectThreadgroup"

-- | @Selector@ for @setMaxTotalThreadsPerObjectThreadgroup:@
setMaxTotalThreadsPerObjectThreadgroupSelector :: Selector
setMaxTotalThreadsPerObjectThreadgroupSelector = mkSelector "setMaxTotalThreadsPerObjectThreadgroup:"

-- | @Selector@ for @maxTotalThreadsPerMeshThreadgroup@
maxTotalThreadsPerMeshThreadgroupSelector :: Selector
maxTotalThreadsPerMeshThreadgroupSelector = mkSelector "maxTotalThreadsPerMeshThreadgroup"

-- | @Selector@ for @setMaxTotalThreadsPerMeshThreadgroup:@
setMaxTotalThreadsPerMeshThreadgroupSelector :: Selector
setMaxTotalThreadsPerMeshThreadgroupSelector = mkSelector "setMaxTotalThreadsPerMeshThreadgroup:"

-- | @Selector@ for @objectThreadgroupSizeIsMultipleOfThreadExecutionWidth@
objectThreadgroupSizeIsMultipleOfThreadExecutionWidthSelector :: Selector
objectThreadgroupSizeIsMultipleOfThreadExecutionWidthSelector = mkSelector "objectThreadgroupSizeIsMultipleOfThreadExecutionWidth"

-- | @Selector@ for @setObjectThreadgroupSizeIsMultipleOfThreadExecutionWidth:@
setObjectThreadgroupSizeIsMultipleOfThreadExecutionWidthSelector :: Selector
setObjectThreadgroupSizeIsMultipleOfThreadExecutionWidthSelector = mkSelector "setObjectThreadgroupSizeIsMultipleOfThreadExecutionWidth:"

-- | @Selector@ for @meshThreadgroupSizeIsMultipleOfThreadExecutionWidth@
meshThreadgroupSizeIsMultipleOfThreadExecutionWidthSelector :: Selector
meshThreadgroupSizeIsMultipleOfThreadExecutionWidthSelector = mkSelector "meshThreadgroupSizeIsMultipleOfThreadExecutionWidth"

-- | @Selector@ for @setMeshThreadgroupSizeIsMultipleOfThreadExecutionWidth:@
setMeshThreadgroupSizeIsMultipleOfThreadExecutionWidthSelector :: Selector
setMeshThreadgroupSizeIsMultipleOfThreadExecutionWidthSelector = mkSelector "setMeshThreadgroupSizeIsMultipleOfThreadExecutionWidth:"

-- | @Selector@ for @payloadMemoryLength@
payloadMemoryLengthSelector :: Selector
payloadMemoryLengthSelector = mkSelector "payloadMemoryLength"

-- | @Selector@ for @setPayloadMemoryLength:@
setPayloadMemoryLengthSelector :: Selector
setPayloadMemoryLengthSelector = mkSelector "setPayloadMemoryLength:"

-- | @Selector@ for @maxTotalThreadgroupsPerMeshGrid@
maxTotalThreadgroupsPerMeshGridSelector :: Selector
maxTotalThreadgroupsPerMeshGridSelector = mkSelector "maxTotalThreadgroupsPerMeshGrid"

-- | @Selector@ for @setMaxTotalThreadgroupsPerMeshGrid:@
setMaxTotalThreadgroupsPerMeshGridSelector :: Selector
setMaxTotalThreadgroupsPerMeshGridSelector = mkSelector "setMaxTotalThreadgroupsPerMeshGrid:"

-- | @Selector@ for @rasterSampleCount@
rasterSampleCountSelector :: Selector
rasterSampleCountSelector = mkSelector "rasterSampleCount"

-- | @Selector@ for @setRasterSampleCount:@
setRasterSampleCountSelector :: Selector
setRasterSampleCountSelector = mkSelector "setRasterSampleCount:"

-- | @Selector@ for @alphaToCoverageState@
alphaToCoverageStateSelector :: Selector
alphaToCoverageStateSelector = mkSelector "alphaToCoverageState"

-- | @Selector@ for @setAlphaToCoverageState:@
setAlphaToCoverageStateSelector :: Selector
setAlphaToCoverageStateSelector = mkSelector "setAlphaToCoverageState:"

-- | @Selector@ for @alphaToOneState@
alphaToOneStateSelector :: Selector
alphaToOneStateSelector = mkSelector "alphaToOneState"

-- | @Selector@ for @setAlphaToOneState:@
setAlphaToOneStateSelector :: Selector
setAlphaToOneStateSelector = mkSelector "setAlphaToOneState:"

-- | @Selector@ for @rasterizationEnabled@
rasterizationEnabledSelector :: Selector
rasterizationEnabledSelector = mkSelector "rasterizationEnabled"

-- | @Selector@ for @setRasterizationEnabled:@
setRasterizationEnabledSelector :: Selector
setRasterizationEnabledSelector = mkSelector "setRasterizationEnabled:"

-- | @Selector@ for @maxVertexAmplificationCount@
maxVertexAmplificationCountSelector :: Selector
maxVertexAmplificationCountSelector = mkSelector "maxVertexAmplificationCount"

-- | @Selector@ for @setMaxVertexAmplificationCount:@
setMaxVertexAmplificationCountSelector :: Selector
setMaxVertexAmplificationCountSelector = mkSelector "setMaxVertexAmplificationCount:"

-- | @Selector@ for @colorAttachments@
colorAttachmentsSelector :: Selector
colorAttachmentsSelector = mkSelector "colorAttachments"

-- | @Selector@ for @objectStaticLinkingDescriptor@
objectStaticLinkingDescriptorSelector :: Selector
objectStaticLinkingDescriptorSelector = mkSelector "objectStaticLinkingDescriptor"

-- | @Selector@ for @setObjectStaticLinkingDescriptor:@
setObjectStaticLinkingDescriptorSelector :: Selector
setObjectStaticLinkingDescriptorSelector = mkSelector "setObjectStaticLinkingDescriptor:"

-- | @Selector@ for @meshStaticLinkingDescriptor@
meshStaticLinkingDescriptorSelector :: Selector
meshStaticLinkingDescriptorSelector = mkSelector "meshStaticLinkingDescriptor"

-- | @Selector@ for @setMeshStaticLinkingDescriptor:@
setMeshStaticLinkingDescriptorSelector :: Selector
setMeshStaticLinkingDescriptorSelector = mkSelector "setMeshStaticLinkingDescriptor:"

-- | @Selector@ for @fragmentStaticLinkingDescriptor@
fragmentStaticLinkingDescriptorSelector :: Selector
fragmentStaticLinkingDescriptorSelector = mkSelector "fragmentStaticLinkingDescriptor"

-- | @Selector@ for @setFragmentStaticLinkingDescriptor:@
setFragmentStaticLinkingDescriptorSelector :: Selector
setFragmentStaticLinkingDescriptorSelector = mkSelector "setFragmentStaticLinkingDescriptor:"

-- | @Selector@ for @supportObjectBinaryLinking@
supportObjectBinaryLinkingSelector :: Selector
supportObjectBinaryLinkingSelector = mkSelector "supportObjectBinaryLinking"

-- | @Selector@ for @setSupportObjectBinaryLinking:@
setSupportObjectBinaryLinkingSelector :: Selector
setSupportObjectBinaryLinkingSelector = mkSelector "setSupportObjectBinaryLinking:"

-- | @Selector@ for @supportMeshBinaryLinking@
supportMeshBinaryLinkingSelector :: Selector
supportMeshBinaryLinkingSelector = mkSelector "supportMeshBinaryLinking"

-- | @Selector@ for @setSupportMeshBinaryLinking:@
setSupportMeshBinaryLinkingSelector :: Selector
setSupportMeshBinaryLinkingSelector = mkSelector "setSupportMeshBinaryLinking:"

-- | @Selector@ for @supportFragmentBinaryLinking@
supportFragmentBinaryLinkingSelector :: Selector
supportFragmentBinaryLinkingSelector = mkSelector "supportFragmentBinaryLinking"

-- | @Selector@ for @setSupportFragmentBinaryLinking:@
setSupportFragmentBinaryLinkingSelector :: Selector
setSupportFragmentBinaryLinkingSelector = mkSelector "setSupportFragmentBinaryLinking:"

-- | @Selector@ for @colorAttachmentMappingState@
colorAttachmentMappingStateSelector :: Selector
colorAttachmentMappingStateSelector = mkSelector "colorAttachmentMappingState"

-- | @Selector@ for @setColorAttachmentMappingState:@
setColorAttachmentMappingStateSelector :: Selector
setColorAttachmentMappingStateSelector = mkSelector "setColorAttachmentMappingState:"

-- | @Selector@ for @supportIndirectCommandBuffers@
supportIndirectCommandBuffersSelector :: Selector
supportIndirectCommandBuffersSelector = mkSelector "supportIndirectCommandBuffers"

-- | @Selector@ for @setSupportIndirectCommandBuffers:@
setSupportIndirectCommandBuffersSelector :: Selector
setSupportIndirectCommandBuffersSelector = mkSelector "setSupportIndirectCommandBuffers:"

