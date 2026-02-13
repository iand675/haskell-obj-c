{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE DataKinds #-}
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
  , alphaToCoverageStateSelector
  , alphaToOneStateSelector
  , colorAttachmentMappingStateSelector
  , colorAttachmentsSelector
  , fragmentFunctionDescriptorSelector
  , fragmentStaticLinkingDescriptorSelector
  , maxTotalThreadgroupsPerMeshGridSelector
  , maxTotalThreadsPerMeshThreadgroupSelector
  , maxTotalThreadsPerObjectThreadgroupSelector
  , maxVertexAmplificationCountSelector
  , meshFunctionDescriptorSelector
  , meshStaticLinkingDescriptorSelector
  , meshThreadgroupSizeIsMultipleOfThreadExecutionWidthSelector
  , objectFunctionDescriptorSelector
  , objectStaticLinkingDescriptorSelector
  , objectThreadgroupSizeIsMultipleOfThreadExecutionWidthSelector
  , payloadMemoryLengthSelector
  , rasterSampleCountSelector
  , rasterizationEnabledSelector
  , resetSelector
  , setAlphaToCoverageStateSelector
  , setAlphaToOneStateSelector
  , setColorAttachmentMappingStateSelector
  , setFragmentFunctionDescriptorSelector
  , setFragmentStaticLinkingDescriptorSelector
  , setMaxTotalThreadgroupsPerMeshGridSelector
  , setMaxTotalThreadsPerMeshThreadgroupSelector
  , setMaxTotalThreadsPerObjectThreadgroupSelector
  , setMaxVertexAmplificationCountSelector
  , setMeshFunctionDescriptorSelector
  , setMeshStaticLinkingDescriptorSelector
  , setMeshThreadgroupSizeIsMultipleOfThreadExecutionWidthSelector
  , setObjectFunctionDescriptorSelector
  , setObjectStaticLinkingDescriptorSelector
  , setObjectThreadgroupSizeIsMultipleOfThreadExecutionWidthSelector
  , setPayloadMemoryLengthSelector
  , setRasterSampleCountSelector
  , setRasterizationEnabledSelector
  , setSupportFragmentBinaryLinkingSelector
  , setSupportIndirectCommandBuffersSelector
  , setSupportMeshBinaryLinkingSelector
  , setSupportObjectBinaryLinkingSelector
  , supportFragmentBinaryLinkingSelector
  , supportIndirectCommandBuffersSelector
  , supportMeshBinaryLinkingSelector
  , supportObjectBinaryLinkingSelector

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

import Foreign.Ptr (Ptr, FunPtr)
import Foreign.C.Types

import ObjC.Runtime.Types
import ObjC.Runtime.Message (sendMessage, sendOwnedMessage, sendClassMessage, sendOwnedClassMessage)
import ObjC.Runtime.Selector (mkSelector)
import ObjC.Runtime.Class (getRequiredClass)

import ObjC.Metal.Internal.Classes
import ObjC.Metal.Internal.Enums
import ObjC.Foundation.Internal.Classes

-- | Resets this descriptor to its default state.
--
-- ObjC selector: @- reset@
reset :: IsMTL4MeshRenderPipelineDescriptor mtL4MeshRenderPipelineDescriptor => mtL4MeshRenderPipelineDescriptor -> IO ()
reset mtL4MeshRenderPipelineDescriptor =
  sendMessage mtL4MeshRenderPipelineDescriptor resetSelector

-- | Assigns a function descriptor representing the function this pipeline executes for each *object* in the object shader stage.
--
-- ObjC selector: @- objectFunctionDescriptor@
objectFunctionDescriptor :: IsMTL4MeshRenderPipelineDescriptor mtL4MeshRenderPipelineDescriptor => mtL4MeshRenderPipelineDescriptor -> IO (Id MTL4FunctionDescriptor)
objectFunctionDescriptor mtL4MeshRenderPipelineDescriptor =
  sendMessage mtL4MeshRenderPipelineDescriptor objectFunctionDescriptorSelector

-- | Assigns a function descriptor representing the function this pipeline executes for each *object* in the object shader stage.
--
-- ObjC selector: @- setObjectFunctionDescriptor:@
setObjectFunctionDescriptor :: (IsMTL4MeshRenderPipelineDescriptor mtL4MeshRenderPipelineDescriptor, IsMTL4FunctionDescriptor value) => mtL4MeshRenderPipelineDescriptor -> value -> IO ()
setObjectFunctionDescriptor mtL4MeshRenderPipelineDescriptor value =
  sendMessage mtL4MeshRenderPipelineDescriptor setObjectFunctionDescriptorSelector (toMTL4FunctionDescriptor value)

-- | Assigns a function descriptor representing the function this pipeline executes for each primitive in the mesh shader stage.
--
-- ObjC selector: @- meshFunctionDescriptor@
meshFunctionDescriptor :: IsMTL4MeshRenderPipelineDescriptor mtL4MeshRenderPipelineDescriptor => mtL4MeshRenderPipelineDescriptor -> IO (Id MTL4FunctionDescriptor)
meshFunctionDescriptor mtL4MeshRenderPipelineDescriptor =
  sendMessage mtL4MeshRenderPipelineDescriptor meshFunctionDescriptorSelector

-- | Assigns a function descriptor representing the function this pipeline executes for each primitive in the mesh shader stage.
--
-- ObjC selector: @- setMeshFunctionDescriptor:@
setMeshFunctionDescriptor :: (IsMTL4MeshRenderPipelineDescriptor mtL4MeshRenderPipelineDescriptor, IsMTL4FunctionDescriptor value) => mtL4MeshRenderPipelineDescriptor -> value -> IO ()
setMeshFunctionDescriptor mtL4MeshRenderPipelineDescriptor value =
  sendMessage mtL4MeshRenderPipelineDescriptor setMeshFunctionDescriptorSelector (toMTL4FunctionDescriptor value)

-- | Assigns a function descriptor representing the function this pipeline executes for each fragment.
--
-- ObjC selector: @- fragmentFunctionDescriptor@
fragmentFunctionDescriptor :: IsMTL4MeshRenderPipelineDescriptor mtL4MeshRenderPipelineDescriptor => mtL4MeshRenderPipelineDescriptor -> IO (Id MTL4FunctionDescriptor)
fragmentFunctionDescriptor mtL4MeshRenderPipelineDescriptor =
  sendMessage mtL4MeshRenderPipelineDescriptor fragmentFunctionDescriptorSelector

-- | Assigns a function descriptor representing the function this pipeline executes for each fragment.
--
-- ObjC selector: @- setFragmentFunctionDescriptor:@
setFragmentFunctionDescriptor :: (IsMTL4MeshRenderPipelineDescriptor mtL4MeshRenderPipelineDescriptor, IsMTL4FunctionDescriptor value) => mtL4MeshRenderPipelineDescriptor -> value -> IO ()
setFragmentFunctionDescriptor mtL4MeshRenderPipelineDescriptor value =
  sendMessage mtL4MeshRenderPipelineDescriptor setFragmentFunctionDescriptorSelector (toMTL4FunctionDescriptor value)

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
maxTotalThreadsPerObjectThreadgroup mtL4MeshRenderPipelineDescriptor =
  sendMessage mtL4MeshRenderPipelineDescriptor maxTotalThreadsPerObjectThreadgroupSelector

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
setMaxTotalThreadsPerObjectThreadgroup mtL4MeshRenderPipelineDescriptor value =
  sendMessage mtL4MeshRenderPipelineDescriptor setMaxTotalThreadsPerObjectThreadgroupSelector value

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
maxTotalThreadsPerMeshThreadgroup mtL4MeshRenderPipelineDescriptor =
  sendMessage mtL4MeshRenderPipelineDescriptor maxTotalThreadsPerMeshThreadgroupSelector

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
setMaxTotalThreadsPerMeshThreadgroup mtL4MeshRenderPipelineDescriptor value =
  sendMessage mtL4MeshRenderPipelineDescriptor setMaxTotalThreadsPerMeshThreadgroupSelector value

-- | Provides a guarantee to Metal regarding the number of threadgroup threads for the object stage of a pipeline you create from this descriptor.
--
-- If you set this property to <doc://com.apple.documentation/documentation/swift/true>, you state to Metal that when you use a mesh render pipeline you create from this descriptor, the number of threadgroup threads you dispatch for the object stage is a multiple of its ``MTLRenderPipelineState/objectThreadExecutionWidth``. The compiler's optimizer can use this guarantee to generate  more efficient code.
--
-- This property's default value is <doc://com.apple.documentation/documentation/swift/false>.
--
-- ObjC selector: @- objectThreadgroupSizeIsMultipleOfThreadExecutionWidth@
objectThreadgroupSizeIsMultipleOfThreadExecutionWidth :: IsMTL4MeshRenderPipelineDescriptor mtL4MeshRenderPipelineDescriptor => mtL4MeshRenderPipelineDescriptor -> IO Bool
objectThreadgroupSizeIsMultipleOfThreadExecutionWidth mtL4MeshRenderPipelineDescriptor =
  sendMessage mtL4MeshRenderPipelineDescriptor objectThreadgroupSizeIsMultipleOfThreadExecutionWidthSelector

-- | Provides a guarantee to Metal regarding the number of threadgroup threads for the object stage of a pipeline you create from this descriptor.
--
-- If you set this property to <doc://com.apple.documentation/documentation/swift/true>, you state to Metal that when you use a mesh render pipeline you create from this descriptor, the number of threadgroup threads you dispatch for the object stage is a multiple of its ``MTLRenderPipelineState/objectThreadExecutionWidth``. The compiler's optimizer can use this guarantee to generate  more efficient code.
--
-- This property's default value is <doc://com.apple.documentation/documentation/swift/false>.
--
-- ObjC selector: @- setObjectThreadgroupSizeIsMultipleOfThreadExecutionWidth:@
setObjectThreadgroupSizeIsMultipleOfThreadExecutionWidth :: IsMTL4MeshRenderPipelineDescriptor mtL4MeshRenderPipelineDescriptor => mtL4MeshRenderPipelineDescriptor -> Bool -> IO ()
setObjectThreadgroupSizeIsMultipleOfThreadExecutionWidth mtL4MeshRenderPipelineDescriptor value =
  sendMessage mtL4MeshRenderPipelineDescriptor setObjectThreadgroupSizeIsMultipleOfThreadExecutionWidthSelector value

-- | Provides a guarantee to Metal regarding the number of threadgroup threads for the mesh stage of a pipeline you create from this descriptor.
--
-- If you set this property to <doc://com.apple.documentation/documentation/swift/true>, you state to Metal that when you use a mesh render pipeline you create from this descriptor, the number of threadgroup threads you dispatch for the mesh stage is a multiple of its ``MTLRenderPipelineState/meshThreadExecutionWidth``. The compiler's optimizer can use this guarantee to generate more efficient code.
--
-- This property's default value is <doc://com.apple.documentation/documentation/swift/false>.
--
-- ObjC selector: @- meshThreadgroupSizeIsMultipleOfThreadExecutionWidth@
meshThreadgroupSizeIsMultipleOfThreadExecutionWidth :: IsMTL4MeshRenderPipelineDescriptor mtL4MeshRenderPipelineDescriptor => mtL4MeshRenderPipelineDescriptor -> IO Bool
meshThreadgroupSizeIsMultipleOfThreadExecutionWidth mtL4MeshRenderPipelineDescriptor =
  sendMessage mtL4MeshRenderPipelineDescriptor meshThreadgroupSizeIsMultipleOfThreadExecutionWidthSelector

-- | Provides a guarantee to Metal regarding the number of threadgroup threads for the mesh stage of a pipeline you create from this descriptor.
--
-- If you set this property to <doc://com.apple.documentation/documentation/swift/true>, you state to Metal that when you use a mesh render pipeline you create from this descriptor, the number of threadgroup threads you dispatch for the mesh stage is a multiple of its ``MTLRenderPipelineState/meshThreadExecutionWidth``. The compiler's optimizer can use this guarantee to generate more efficient code.
--
-- This property's default value is <doc://com.apple.documentation/documentation/swift/false>.
--
-- ObjC selector: @- setMeshThreadgroupSizeIsMultipleOfThreadExecutionWidth:@
setMeshThreadgroupSizeIsMultipleOfThreadExecutionWidth :: IsMTL4MeshRenderPipelineDescriptor mtL4MeshRenderPipelineDescriptor => mtL4MeshRenderPipelineDescriptor -> Bool -> IO ()
setMeshThreadgroupSizeIsMultipleOfThreadExecutionWidth mtL4MeshRenderPipelineDescriptor value =
  sendMessage mtL4MeshRenderPipelineDescriptor setMeshThreadgroupSizeIsMultipleOfThreadExecutionWidthSelector value

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
payloadMemoryLength mtL4MeshRenderPipelineDescriptor =
  sendMessage mtL4MeshRenderPipelineDescriptor payloadMemoryLengthSelector

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
setPayloadMemoryLength mtL4MeshRenderPipelineDescriptor value =
  sendMessage mtL4MeshRenderPipelineDescriptor setPayloadMemoryLengthSelector value

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
maxTotalThreadgroupsPerMeshGrid mtL4MeshRenderPipelineDescriptor =
  sendMessage mtL4MeshRenderPipelineDescriptor maxTotalThreadgroupsPerMeshGridSelector

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
setMaxTotalThreadgroupsPerMeshGrid mtL4MeshRenderPipelineDescriptor value =
  sendMessage mtL4MeshRenderPipelineDescriptor setMaxTotalThreadgroupsPerMeshGridSelector value

-- | Sets number of samples this pipeline applies for each fragment.
--
-- ObjC selector: @- rasterSampleCount@
rasterSampleCount :: IsMTL4MeshRenderPipelineDescriptor mtL4MeshRenderPipelineDescriptor => mtL4MeshRenderPipelineDescriptor -> IO CULong
rasterSampleCount mtL4MeshRenderPipelineDescriptor =
  sendMessage mtL4MeshRenderPipelineDescriptor rasterSampleCountSelector

-- | Sets number of samples this pipeline applies for each fragment.
--
-- ObjC selector: @- setRasterSampleCount:@
setRasterSampleCount :: IsMTL4MeshRenderPipelineDescriptor mtL4MeshRenderPipelineDescriptor => mtL4MeshRenderPipelineDescriptor -> CULong -> IO ()
setRasterSampleCount mtL4MeshRenderPipelineDescriptor value =
  sendMessage mtL4MeshRenderPipelineDescriptor setRasterSampleCountSelector value

-- | Indicates whether to read and use the alpha channel fragment output of color attachments to compute a sample coverage mask.
--
-- ObjC selector: @- alphaToCoverageState@
alphaToCoverageState :: IsMTL4MeshRenderPipelineDescriptor mtL4MeshRenderPipelineDescriptor => mtL4MeshRenderPipelineDescriptor -> IO MTL4AlphaToCoverageState
alphaToCoverageState mtL4MeshRenderPipelineDescriptor =
  sendMessage mtL4MeshRenderPipelineDescriptor alphaToCoverageStateSelector

-- | Indicates whether to read and use the alpha channel fragment output of color attachments to compute a sample coverage mask.
--
-- ObjC selector: @- setAlphaToCoverageState:@
setAlphaToCoverageState :: IsMTL4MeshRenderPipelineDescriptor mtL4MeshRenderPipelineDescriptor => mtL4MeshRenderPipelineDescriptor -> MTL4AlphaToCoverageState -> IO ()
setAlphaToCoverageState mtL4MeshRenderPipelineDescriptor value =
  sendMessage mtL4MeshRenderPipelineDescriptor setAlphaToCoverageStateSelector value

-- | Indicates whether the pipeline forces alpha channel values of color attachments to the largest representable value.
--
-- ObjC selector: @- alphaToOneState@
alphaToOneState :: IsMTL4MeshRenderPipelineDescriptor mtL4MeshRenderPipelineDescriptor => mtL4MeshRenderPipelineDescriptor -> IO MTL4AlphaToOneState
alphaToOneState mtL4MeshRenderPipelineDescriptor =
  sendMessage mtL4MeshRenderPipelineDescriptor alphaToOneStateSelector

-- | Indicates whether the pipeline forces alpha channel values of color attachments to the largest representable value.
--
-- ObjC selector: @- setAlphaToOneState:@
setAlphaToOneState :: IsMTL4MeshRenderPipelineDescriptor mtL4MeshRenderPipelineDescriptor => mtL4MeshRenderPipelineDescriptor -> MTL4AlphaToOneState -> IO ()
setAlphaToOneState mtL4MeshRenderPipelineDescriptor value =
  sendMessage mtL4MeshRenderPipelineDescriptor setAlphaToOneStateSelector value

-- | Determines whether the pipeline rasterizes primitives.
--
-- By default, this value is <doc://com.apple.documentation/documentation/swift/true>, specifying that this pipeline rasterizes primitives. Set this property to <doc://com.apple.documentation/documentation/swift/false> when you don't provide a fragment shader function via function ``fragmentFunctionDescriptor``.
--
-- ObjC selector: @- rasterizationEnabled@
rasterizationEnabled :: IsMTL4MeshRenderPipelineDescriptor mtL4MeshRenderPipelineDescriptor => mtL4MeshRenderPipelineDescriptor -> IO Bool
rasterizationEnabled mtL4MeshRenderPipelineDescriptor =
  sendMessage mtL4MeshRenderPipelineDescriptor rasterizationEnabledSelector

-- | Determines whether the pipeline rasterizes primitives.
--
-- By default, this value is <doc://com.apple.documentation/documentation/swift/true>, specifying that this pipeline rasterizes primitives. Set this property to <doc://com.apple.documentation/documentation/swift/false> when you don't provide a fragment shader function via function ``fragmentFunctionDescriptor``.
--
-- ObjC selector: @- setRasterizationEnabled:@
setRasterizationEnabled :: IsMTL4MeshRenderPipelineDescriptor mtL4MeshRenderPipelineDescriptor => mtL4MeshRenderPipelineDescriptor -> Bool -> IO ()
setRasterizationEnabled mtL4MeshRenderPipelineDescriptor value =
  sendMessage mtL4MeshRenderPipelineDescriptor setRasterizationEnabledSelector value

-- | Determines the maximum value that can you can pass as the pipeline's amplification count.
--
-- This property controls the maximum count you pass to ``MTL4RenderCommandEncoder/setVertexAmplificationCount:viewMappings:`` when using vertex amplification with this pipeline.
--
-- ObjC selector: @- maxVertexAmplificationCount@
maxVertexAmplificationCount :: IsMTL4MeshRenderPipelineDescriptor mtL4MeshRenderPipelineDescriptor => mtL4MeshRenderPipelineDescriptor -> IO CULong
maxVertexAmplificationCount mtL4MeshRenderPipelineDescriptor =
  sendMessage mtL4MeshRenderPipelineDescriptor maxVertexAmplificationCountSelector

-- | Determines the maximum value that can you can pass as the pipeline's amplification count.
--
-- This property controls the maximum count you pass to ``MTL4RenderCommandEncoder/setVertexAmplificationCount:viewMappings:`` when using vertex amplification with this pipeline.
--
-- ObjC selector: @- setMaxVertexAmplificationCount:@
setMaxVertexAmplificationCount :: IsMTL4MeshRenderPipelineDescriptor mtL4MeshRenderPipelineDescriptor => mtL4MeshRenderPipelineDescriptor -> CULong -> IO ()
setMaxVertexAmplificationCount mtL4MeshRenderPipelineDescriptor value =
  sendMessage mtL4MeshRenderPipelineDescriptor setMaxVertexAmplificationCountSelector value

-- | Accesses an array containing descriptions of the color attachments this pipeline writes to.
--
-- ObjC selector: @- colorAttachments@
colorAttachments :: IsMTL4MeshRenderPipelineDescriptor mtL4MeshRenderPipelineDescriptor => mtL4MeshRenderPipelineDescriptor -> IO (Id MTL4RenderPipelineColorAttachmentDescriptorArray)
colorAttachments mtL4MeshRenderPipelineDescriptor =
  sendMessage mtL4MeshRenderPipelineDescriptor colorAttachmentsSelector

-- | Provides static linking information for the object stage of the render pipeline.
--
-- Use this property to link extra shader functions to the object stage of the render pipeline.
--
-- ObjC selector: @- objectStaticLinkingDescriptor@
objectStaticLinkingDescriptor :: IsMTL4MeshRenderPipelineDescriptor mtL4MeshRenderPipelineDescriptor => mtL4MeshRenderPipelineDescriptor -> IO (Id MTL4StaticLinkingDescriptor)
objectStaticLinkingDescriptor mtL4MeshRenderPipelineDescriptor =
  sendMessage mtL4MeshRenderPipelineDescriptor objectStaticLinkingDescriptorSelector

-- | Provides static linking information for the object stage of the render pipeline.
--
-- Use this property to link extra shader functions to the object stage of the render pipeline.
--
-- ObjC selector: @- setObjectStaticLinkingDescriptor:@
setObjectStaticLinkingDescriptor :: (IsMTL4MeshRenderPipelineDescriptor mtL4MeshRenderPipelineDescriptor, IsMTL4StaticLinkingDescriptor value) => mtL4MeshRenderPipelineDescriptor -> value -> IO ()
setObjectStaticLinkingDescriptor mtL4MeshRenderPipelineDescriptor value =
  sendMessage mtL4MeshRenderPipelineDescriptor setObjectStaticLinkingDescriptorSelector (toMTL4StaticLinkingDescriptor value)

-- | Provides static linking information for the mesh stage of the render pipeline.
--
-- Use this property to link extra shader functions to the mesh stage of the render pipeline.
--
-- ObjC selector: @- meshStaticLinkingDescriptor@
meshStaticLinkingDescriptor :: IsMTL4MeshRenderPipelineDescriptor mtL4MeshRenderPipelineDescriptor => mtL4MeshRenderPipelineDescriptor -> IO (Id MTL4StaticLinkingDescriptor)
meshStaticLinkingDescriptor mtL4MeshRenderPipelineDescriptor =
  sendMessage mtL4MeshRenderPipelineDescriptor meshStaticLinkingDescriptorSelector

-- | Provides static linking information for the mesh stage of the render pipeline.
--
-- Use this property to link extra shader functions to the mesh stage of the render pipeline.
--
-- ObjC selector: @- setMeshStaticLinkingDescriptor:@
setMeshStaticLinkingDescriptor :: (IsMTL4MeshRenderPipelineDescriptor mtL4MeshRenderPipelineDescriptor, IsMTL4StaticLinkingDescriptor value) => mtL4MeshRenderPipelineDescriptor -> value -> IO ()
setMeshStaticLinkingDescriptor mtL4MeshRenderPipelineDescriptor value =
  sendMessage mtL4MeshRenderPipelineDescriptor setMeshStaticLinkingDescriptorSelector (toMTL4StaticLinkingDescriptor value)

-- | Provides static linking information for the fragment stage of the render pipeline.
--
-- Use this property to link extra shader functions to the fragment stage of the render pipeline.
--
-- ObjC selector: @- fragmentStaticLinkingDescriptor@
fragmentStaticLinkingDescriptor :: IsMTL4MeshRenderPipelineDescriptor mtL4MeshRenderPipelineDescriptor => mtL4MeshRenderPipelineDescriptor -> IO (Id MTL4StaticLinkingDescriptor)
fragmentStaticLinkingDescriptor mtL4MeshRenderPipelineDescriptor =
  sendMessage mtL4MeshRenderPipelineDescriptor fragmentStaticLinkingDescriptorSelector

-- | Provides static linking information for the fragment stage of the render pipeline.
--
-- Use this property to link extra shader functions to the fragment stage of the render pipeline.
--
-- ObjC selector: @- setFragmentStaticLinkingDescriptor:@
setFragmentStaticLinkingDescriptor :: (IsMTL4MeshRenderPipelineDescriptor mtL4MeshRenderPipelineDescriptor, IsMTL4StaticLinkingDescriptor value) => mtL4MeshRenderPipelineDescriptor -> value -> IO ()
setFragmentStaticLinkingDescriptor mtL4MeshRenderPipelineDescriptor value =
  sendMessage mtL4MeshRenderPipelineDescriptor setFragmentStaticLinkingDescriptorSelector (toMTL4StaticLinkingDescriptor value)

-- | Indicates whether you can use the render pipeline to create new pipelines by adding binary functions to the object shader function’s callable functions list.
--
-- ObjC selector: @- supportObjectBinaryLinking@
supportObjectBinaryLinking :: IsMTL4MeshRenderPipelineDescriptor mtL4MeshRenderPipelineDescriptor => mtL4MeshRenderPipelineDescriptor -> IO Bool
supportObjectBinaryLinking mtL4MeshRenderPipelineDescriptor =
  sendMessage mtL4MeshRenderPipelineDescriptor supportObjectBinaryLinkingSelector

-- | Indicates whether you can use the render pipeline to create new pipelines by adding binary functions to the object shader function’s callable functions list.
--
-- ObjC selector: @- setSupportObjectBinaryLinking:@
setSupportObjectBinaryLinking :: IsMTL4MeshRenderPipelineDescriptor mtL4MeshRenderPipelineDescriptor => mtL4MeshRenderPipelineDescriptor -> Bool -> IO ()
setSupportObjectBinaryLinking mtL4MeshRenderPipelineDescriptor value =
  sendMessage mtL4MeshRenderPipelineDescriptor setSupportObjectBinaryLinkingSelector value

-- | Indicates whether you can use the render pipeline to create new pipelines by adding binary functions to the mesh shader function’s callable functions list.
--
-- ObjC selector: @- supportMeshBinaryLinking@
supportMeshBinaryLinking :: IsMTL4MeshRenderPipelineDescriptor mtL4MeshRenderPipelineDescriptor => mtL4MeshRenderPipelineDescriptor -> IO Bool
supportMeshBinaryLinking mtL4MeshRenderPipelineDescriptor =
  sendMessage mtL4MeshRenderPipelineDescriptor supportMeshBinaryLinkingSelector

-- | Indicates whether you can use the render pipeline to create new pipelines by adding binary functions to the mesh shader function’s callable functions list.
--
-- ObjC selector: @- setSupportMeshBinaryLinking:@
setSupportMeshBinaryLinking :: IsMTL4MeshRenderPipelineDescriptor mtL4MeshRenderPipelineDescriptor => mtL4MeshRenderPipelineDescriptor -> Bool -> IO ()
setSupportMeshBinaryLinking mtL4MeshRenderPipelineDescriptor value =
  sendMessage mtL4MeshRenderPipelineDescriptor setSupportMeshBinaryLinkingSelector value

-- | Indicates whether you can use the render pipeline to create new pipelines by adding binary functions to the fragment shader function’s callable functions list.
--
-- ObjC selector: @- supportFragmentBinaryLinking@
supportFragmentBinaryLinking :: IsMTL4MeshRenderPipelineDescriptor mtL4MeshRenderPipelineDescriptor => mtL4MeshRenderPipelineDescriptor -> IO Bool
supportFragmentBinaryLinking mtL4MeshRenderPipelineDescriptor =
  sendMessage mtL4MeshRenderPipelineDescriptor supportFragmentBinaryLinkingSelector

-- | Indicates whether you can use the render pipeline to create new pipelines by adding binary functions to the fragment shader function’s callable functions list.
--
-- ObjC selector: @- setSupportFragmentBinaryLinking:@
setSupportFragmentBinaryLinking :: IsMTL4MeshRenderPipelineDescriptor mtL4MeshRenderPipelineDescriptor => mtL4MeshRenderPipelineDescriptor -> Bool -> IO ()
setSupportFragmentBinaryLinking mtL4MeshRenderPipelineDescriptor value =
  sendMessage mtL4MeshRenderPipelineDescriptor setSupportFragmentBinaryLinkingSelector value

-- | Sets the logical-to-physical rendering remap state.
--
-- Use this property to assign how a ``MTL4RenderCommandEncoder`` instance maps the output of your fragment shader to physical color attachments.
--
-- ObjC selector: @- colorAttachmentMappingState@
colorAttachmentMappingState :: IsMTL4MeshRenderPipelineDescriptor mtL4MeshRenderPipelineDescriptor => mtL4MeshRenderPipelineDescriptor -> IO MTL4LogicalToPhysicalColorAttachmentMappingState
colorAttachmentMappingState mtL4MeshRenderPipelineDescriptor =
  sendMessage mtL4MeshRenderPipelineDescriptor colorAttachmentMappingStateSelector

-- | Sets the logical-to-physical rendering remap state.
--
-- Use this property to assign how a ``MTL4RenderCommandEncoder`` instance maps the output of your fragment shader to physical color attachments.
--
-- ObjC selector: @- setColorAttachmentMappingState:@
setColorAttachmentMappingState :: IsMTL4MeshRenderPipelineDescriptor mtL4MeshRenderPipelineDescriptor => mtL4MeshRenderPipelineDescriptor -> MTL4LogicalToPhysicalColorAttachmentMappingState -> IO ()
setColorAttachmentMappingState mtL4MeshRenderPipelineDescriptor value =
  sendMessage mtL4MeshRenderPipelineDescriptor setColorAttachmentMappingStateSelector value

-- | Indicates whether the pipeline supports indirect command buffers.
--
-- ObjC selector: @- supportIndirectCommandBuffers@
supportIndirectCommandBuffers :: IsMTL4MeshRenderPipelineDescriptor mtL4MeshRenderPipelineDescriptor => mtL4MeshRenderPipelineDescriptor -> IO MTL4IndirectCommandBufferSupportState
supportIndirectCommandBuffers mtL4MeshRenderPipelineDescriptor =
  sendMessage mtL4MeshRenderPipelineDescriptor supportIndirectCommandBuffersSelector

-- | Indicates whether the pipeline supports indirect command buffers.
--
-- ObjC selector: @- setSupportIndirectCommandBuffers:@
setSupportIndirectCommandBuffers :: IsMTL4MeshRenderPipelineDescriptor mtL4MeshRenderPipelineDescriptor => mtL4MeshRenderPipelineDescriptor -> MTL4IndirectCommandBufferSupportState -> IO ()
setSupportIndirectCommandBuffers mtL4MeshRenderPipelineDescriptor value =
  sendMessage mtL4MeshRenderPipelineDescriptor setSupportIndirectCommandBuffersSelector value

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @reset@
resetSelector :: Selector '[] ()
resetSelector = mkSelector "reset"

-- | @Selector@ for @objectFunctionDescriptor@
objectFunctionDescriptorSelector :: Selector '[] (Id MTL4FunctionDescriptor)
objectFunctionDescriptorSelector = mkSelector "objectFunctionDescriptor"

-- | @Selector@ for @setObjectFunctionDescriptor:@
setObjectFunctionDescriptorSelector :: Selector '[Id MTL4FunctionDescriptor] ()
setObjectFunctionDescriptorSelector = mkSelector "setObjectFunctionDescriptor:"

-- | @Selector@ for @meshFunctionDescriptor@
meshFunctionDescriptorSelector :: Selector '[] (Id MTL4FunctionDescriptor)
meshFunctionDescriptorSelector = mkSelector "meshFunctionDescriptor"

-- | @Selector@ for @setMeshFunctionDescriptor:@
setMeshFunctionDescriptorSelector :: Selector '[Id MTL4FunctionDescriptor] ()
setMeshFunctionDescriptorSelector = mkSelector "setMeshFunctionDescriptor:"

-- | @Selector@ for @fragmentFunctionDescriptor@
fragmentFunctionDescriptorSelector :: Selector '[] (Id MTL4FunctionDescriptor)
fragmentFunctionDescriptorSelector = mkSelector "fragmentFunctionDescriptor"

-- | @Selector@ for @setFragmentFunctionDescriptor:@
setFragmentFunctionDescriptorSelector :: Selector '[Id MTL4FunctionDescriptor] ()
setFragmentFunctionDescriptorSelector = mkSelector "setFragmentFunctionDescriptor:"

-- | @Selector@ for @maxTotalThreadsPerObjectThreadgroup@
maxTotalThreadsPerObjectThreadgroupSelector :: Selector '[] CULong
maxTotalThreadsPerObjectThreadgroupSelector = mkSelector "maxTotalThreadsPerObjectThreadgroup"

-- | @Selector@ for @setMaxTotalThreadsPerObjectThreadgroup:@
setMaxTotalThreadsPerObjectThreadgroupSelector :: Selector '[CULong] ()
setMaxTotalThreadsPerObjectThreadgroupSelector = mkSelector "setMaxTotalThreadsPerObjectThreadgroup:"

-- | @Selector@ for @maxTotalThreadsPerMeshThreadgroup@
maxTotalThreadsPerMeshThreadgroupSelector :: Selector '[] CULong
maxTotalThreadsPerMeshThreadgroupSelector = mkSelector "maxTotalThreadsPerMeshThreadgroup"

-- | @Selector@ for @setMaxTotalThreadsPerMeshThreadgroup:@
setMaxTotalThreadsPerMeshThreadgroupSelector :: Selector '[CULong] ()
setMaxTotalThreadsPerMeshThreadgroupSelector = mkSelector "setMaxTotalThreadsPerMeshThreadgroup:"

-- | @Selector@ for @objectThreadgroupSizeIsMultipleOfThreadExecutionWidth@
objectThreadgroupSizeIsMultipleOfThreadExecutionWidthSelector :: Selector '[] Bool
objectThreadgroupSizeIsMultipleOfThreadExecutionWidthSelector = mkSelector "objectThreadgroupSizeIsMultipleOfThreadExecutionWidth"

-- | @Selector@ for @setObjectThreadgroupSizeIsMultipleOfThreadExecutionWidth:@
setObjectThreadgroupSizeIsMultipleOfThreadExecutionWidthSelector :: Selector '[Bool] ()
setObjectThreadgroupSizeIsMultipleOfThreadExecutionWidthSelector = mkSelector "setObjectThreadgroupSizeIsMultipleOfThreadExecutionWidth:"

-- | @Selector@ for @meshThreadgroupSizeIsMultipleOfThreadExecutionWidth@
meshThreadgroupSizeIsMultipleOfThreadExecutionWidthSelector :: Selector '[] Bool
meshThreadgroupSizeIsMultipleOfThreadExecutionWidthSelector = mkSelector "meshThreadgroupSizeIsMultipleOfThreadExecutionWidth"

-- | @Selector@ for @setMeshThreadgroupSizeIsMultipleOfThreadExecutionWidth:@
setMeshThreadgroupSizeIsMultipleOfThreadExecutionWidthSelector :: Selector '[Bool] ()
setMeshThreadgroupSizeIsMultipleOfThreadExecutionWidthSelector = mkSelector "setMeshThreadgroupSizeIsMultipleOfThreadExecutionWidth:"

-- | @Selector@ for @payloadMemoryLength@
payloadMemoryLengthSelector :: Selector '[] CULong
payloadMemoryLengthSelector = mkSelector "payloadMemoryLength"

-- | @Selector@ for @setPayloadMemoryLength:@
setPayloadMemoryLengthSelector :: Selector '[CULong] ()
setPayloadMemoryLengthSelector = mkSelector "setPayloadMemoryLength:"

-- | @Selector@ for @maxTotalThreadgroupsPerMeshGrid@
maxTotalThreadgroupsPerMeshGridSelector :: Selector '[] CULong
maxTotalThreadgroupsPerMeshGridSelector = mkSelector "maxTotalThreadgroupsPerMeshGrid"

-- | @Selector@ for @setMaxTotalThreadgroupsPerMeshGrid:@
setMaxTotalThreadgroupsPerMeshGridSelector :: Selector '[CULong] ()
setMaxTotalThreadgroupsPerMeshGridSelector = mkSelector "setMaxTotalThreadgroupsPerMeshGrid:"

-- | @Selector@ for @rasterSampleCount@
rasterSampleCountSelector :: Selector '[] CULong
rasterSampleCountSelector = mkSelector "rasterSampleCount"

-- | @Selector@ for @setRasterSampleCount:@
setRasterSampleCountSelector :: Selector '[CULong] ()
setRasterSampleCountSelector = mkSelector "setRasterSampleCount:"

-- | @Selector@ for @alphaToCoverageState@
alphaToCoverageStateSelector :: Selector '[] MTL4AlphaToCoverageState
alphaToCoverageStateSelector = mkSelector "alphaToCoverageState"

-- | @Selector@ for @setAlphaToCoverageState:@
setAlphaToCoverageStateSelector :: Selector '[MTL4AlphaToCoverageState] ()
setAlphaToCoverageStateSelector = mkSelector "setAlphaToCoverageState:"

-- | @Selector@ for @alphaToOneState@
alphaToOneStateSelector :: Selector '[] MTL4AlphaToOneState
alphaToOneStateSelector = mkSelector "alphaToOneState"

-- | @Selector@ for @setAlphaToOneState:@
setAlphaToOneStateSelector :: Selector '[MTL4AlphaToOneState] ()
setAlphaToOneStateSelector = mkSelector "setAlphaToOneState:"

-- | @Selector@ for @rasterizationEnabled@
rasterizationEnabledSelector :: Selector '[] Bool
rasterizationEnabledSelector = mkSelector "rasterizationEnabled"

-- | @Selector@ for @setRasterizationEnabled:@
setRasterizationEnabledSelector :: Selector '[Bool] ()
setRasterizationEnabledSelector = mkSelector "setRasterizationEnabled:"

-- | @Selector@ for @maxVertexAmplificationCount@
maxVertexAmplificationCountSelector :: Selector '[] CULong
maxVertexAmplificationCountSelector = mkSelector "maxVertexAmplificationCount"

-- | @Selector@ for @setMaxVertexAmplificationCount:@
setMaxVertexAmplificationCountSelector :: Selector '[CULong] ()
setMaxVertexAmplificationCountSelector = mkSelector "setMaxVertexAmplificationCount:"

-- | @Selector@ for @colorAttachments@
colorAttachmentsSelector :: Selector '[] (Id MTL4RenderPipelineColorAttachmentDescriptorArray)
colorAttachmentsSelector = mkSelector "colorAttachments"

-- | @Selector@ for @objectStaticLinkingDescriptor@
objectStaticLinkingDescriptorSelector :: Selector '[] (Id MTL4StaticLinkingDescriptor)
objectStaticLinkingDescriptorSelector = mkSelector "objectStaticLinkingDescriptor"

-- | @Selector@ for @setObjectStaticLinkingDescriptor:@
setObjectStaticLinkingDescriptorSelector :: Selector '[Id MTL4StaticLinkingDescriptor] ()
setObjectStaticLinkingDescriptorSelector = mkSelector "setObjectStaticLinkingDescriptor:"

-- | @Selector@ for @meshStaticLinkingDescriptor@
meshStaticLinkingDescriptorSelector :: Selector '[] (Id MTL4StaticLinkingDescriptor)
meshStaticLinkingDescriptorSelector = mkSelector "meshStaticLinkingDescriptor"

-- | @Selector@ for @setMeshStaticLinkingDescriptor:@
setMeshStaticLinkingDescriptorSelector :: Selector '[Id MTL4StaticLinkingDescriptor] ()
setMeshStaticLinkingDescriptorSelector = mkSelector "setMeshStaticLinkingDescriptor:"

-- | @Selector@ for @fragmentStaticLinkingDescriptor@
fragmentStaticLinkingDescriptorSelector :: Selector '[] (Id MTL4StaticLinkingDescriptor)
fragmentStaticLinkingDescriptorSelector = mkSelector "fragmentStaticLinkingDescriptor"

-- | @Selector@ for @setFragmentStaticLinkingDescriptor:@
setFragmentStaticLinkingDescriptorSelector :: Selector '[Id MTL4StaticLinkingDescriptor] ()
setFragmentStaticLinkingDescriptorSelector = mkSelector "setFragmentStaticLinkingDescriptor:"

-- | @Selector@ for @supportObjectBinaryLinking@
supportObjectBinaryLinkingSelector :: Selector '[] Bool
supportObjectBinaryLinkingSelector = mkSelector "supportObjectBinaryLinking"

-- | @Selector@ for @setSupportObjectBinaryLinking:@
setSupportObjectBinaryLinkingSelector :: Selector '[Bool] ()
setSupportObjectBinaryLinkingSelector = mkSelector "setSupportObjectBinaryLinking:"

-- | @Selector@ for @supportMeshBinaryLinking@
supportMeshBinaryLinkingSelector :: Selector '[] Bool
supportMeshBinaryLinkingSelector = mkSelector "supportMeshBinaryLinking"

-- | @Selector@ for @setSupportMeshBinaryLinking:@
setSupportMeshBinaryLinkingSelector :: Selector '[Bool] ()
setSupportMeshBinaryLinkingSelector = mkSelector "setSupportMeshBinaryLinking:"

-- | @Selector@ for @supportFragmentBinaryLinking@
supportFragmentBinaryLinkingSelector :: Selector '[] Bool
supportFragmentBinaryLinkingSelector = mkSelector "supportFragmentBinaryLinking"

-- | @Selector@ for @setSupportFragmentBinaryLinking:@
setSupportFragmentBinaryLinkingSelector :: Selector '[Bool] ()
setSupportFragmentBinaryLinkingSelector = mkSelector "setSupportFragmentBinaryLinking:"

-- | @Selector@ for @colorAttachmentMappingState@
colorAttachmentMappingStateSelector :: Selector '[] MTL4LogicalToPhysicalColorAttachmentMappingState
colorAttachmentMappingStateSelector = mkSelector "colorAttachmentMappingState"

-- | @Selector@ for @setColorAttachmentMappingState:@
setColorAttachmentMappingStateSelector :: Selector '[MTL4LogicalToPhysicalColorAttachmentMappingState] ()
setColorAttachmentMappingStateSelector = mkSelector "setColorAttachmentMappingState:"

-- | @Selector@ for @supportIndirectCommandBuffers@
supportIndirectCommandBuffersSelector :: Selector '[] MTL4IndirectCommandBufferSupportState
supportIndirectCommandBuffersSelector = mkSelector "supportIndirectCommandBuffers"

-- | @Selector@ for @setSupportIndirectCommandBuffers:@
setSupportIndirectCommandBuffersSelector :: Selector '[MTL4IndirectCommandBufferSupportState] ()
setSupportIndirectCommandBuffersSelector = mkSelector "setSupportIndirectCommandBuffers:"

