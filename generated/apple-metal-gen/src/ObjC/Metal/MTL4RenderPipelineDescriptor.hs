{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Groups together properties to create a render pipeline state object.
--
-- Compared to ``MTLRenderPipelineDescriptor``, this interface doesn't offer a mechanism to hint to Metal mutability of vertex and fragment buffers. Additionally, using this descriptor, you don't specify binary archives.
--
-- Generated bindings for @MTL4RenderPipelineDescriptor@.
module ObjC.Metal.MTL4RenderPipelineDescriptor
  ( MTL4RenderPipelineDescriptor
  , IsMTL4RenderPipelineDescriptor(..)
  , reset
  , vertexFunctionDescriptor
  , setVertexFunctionDescriptor
  , fragmentFunctionDescriptor
  , setFragmentFunctionDescriptor
  , vertexDescriptor
  , setVertexDescriptor
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
  , inputPrimitiveTopology
  , setInputPrimitiveTopology
  , vertexStaticLinkingDescriptor
  , setVertexStaticLinkingDescriptor
  , fragmentStaticLinkingDescriptor
  , setFragmentStaticLinkingDescriptor
  , supportVertexBinaryLinking
  , setSupportVertexBinaryLinking
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
  , inputPrimitiveTopologySelector
  , maxVertexAmplificationCountSelector
  , rasterSampleCountSelector
  , rasterizationEnabledSelector
  , resetSelector
  , setAlphaToCoverageStateSelector
  , setAlphaToOneStateSelector
  , setColorAttachmentMappingStateSelector
  , setFragmentFunctionDescriptorSelector
  , setFragmentStaticLinkingDescriptorSelector
  , setInputPrimitiveTopologySelector
  , setMaxVertexAmplificationCountSelector
  , setRasterSampleCountSelector
  , setRasterizationEnabledSelector
  , setSupportFragmentBinaryLinkingSelector
  , setSupportIndirectCommandBuffersSelector
  , setSupportVertexBinaryLinkingSelector
  , setVertexDescriptorSelector
  , setVertexFunctionDescriptorSelector
  , setVertexStaticLinkingDescriptorSelector
  , supportFragmentBinaryLinkingSelector
  , supportIndirectCommandBuffersSelector
  , supportVertexBinaryLinkingSelector
  , vertexDescriptorSelector
  , vertexFunctionDescriptorSelector
  , vertexStaticLinkingDescriptorSelector

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
  , MTLPrimitiveTopologyClass(MTLPrimitiveTopologyClass)
  , pattern MTLPrimitiveTopologyClassUnspecified
  , pattern MTLPrimitiveTopologyClassPoint
  , pattern MTLPrimitiveTopologyClassLine
  , pattern MTLPrimitiveTopologyClassTriangle

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
reset :: IsMTL4RenderPipelineDescriptor mtL4RenderPipelineDescriptor => mtL4RenderPipelineDescriptor -> IO ()
reset mtL4RenderPipelineDescriptor =
  sendMessage mtL4RenderPipelineDescriptor resetSelector

-- | Assigns the shader function that this pipeline executes for each vertex.
--
-- ObjC selector: @- vertexFunctionDescriptor@
vertexFunctionDescriptor :: IsMTL4RenderPipelineDescriptor mtL4RenderPipelineDescriptor => mtL4RenderPipelineDescriptor -> IO (Id MTL4FunctionDescriptor)
vertexFunctionDescriptor mtL4RenderPipelineDescriptor =
  sendMessage mtL4RenderPipelineDescriptor vertexFunctionDescriptorSelector

-- | Assigns the shader function that this pipeline executes for each vertex.
--
-- ObjC selector: @- setVertexFunctionDescriptor:@
setVertexFunctionDescriptor :: (IsMTL4RenderPipelineDescriptor mtL4RenderPipelineDescriptor, IsMTL4FunctionDescriptor value) => mtL4RenderPipelineDescriptor -> value -> IO ()
setVertexFunctionDescriptor mtL4RenderPipelineDescriptor value =
  sendMessage mtL4RenderPipelineDescriptor setVertexFunctionDescriptorSelector (toMTL4FunctionDescriptor value)

-- | Assigns the shader function that this pipeline executes for each fragment.
--
-- When you don't specify a fragment function, you need to disable rasterization by setting property ``rasterizationEnabled`` to false.
--
-- ObjC selector: @- fragmentFunctionDescriptor@
fragmentFunctionDescriptor :: IsMTL4RenderPipelineDescriptor mtL4RenderPipelineDescriptor => mtL4RenderPipelineDescriptor -> IO (Id MTL4FunctionDescriptor)
fragmentFunctionDescriptor mtL4RenderPipelineDescriptor =
  sendMessage mtL4RenderPipelineDescriptor fragmentFunctionDescriptorSelector

-- | Assigns the shader function that this pipeline executes for each fragment.
--
-- When you don't specify a fragment function, you need to disable rasterization by setting property ``rasterizationEnabled`` to false.
--
-- ObjC selector: @- setFragmentFunctionDescriptor:@
setFragmentFunctionDescriptor :: (IsMTL4RenderPipelineDescriptor mtL4RenderPipelineDescriptor, IsMTL4FunctionDescriptor value) => mtL4RenderPipelineDescriptor -> value -> IO ()
setFragmentFunctionDescriptor mtL4RenderPipelineDescriptor value =
  sendMessage mtL4RenderPipelineDescriptor setFragmentFunctionDescriptorSelector (toMTL4FunctionDescriptor value)

-- | Configures an optional vertex descriptor for the vertex input.
--
-- A vertex descriptor specifies the layout of your vertex data, allowing your vertex shaders to access the content in your vertex arrays via the @[[stage_in]]@ attribute in Metal Shading Language.
--
-- ObjC selector: @- vertexDescriptor@
vertexDescriptor :: IsMTL4RenderPipelineDescriptor mtL4RenderPipelineDescriptor => mtL4RenderPipelineDescriptor -> IO (Id MTLVertexDescriptor)
vertexDescriptor mtL4RenderPipelineDescriptor =
  sendMessage mtL4RenderPipelineDescriptor vertexDescriptorSelector

-- | Configures an optional vertex descriptor for the vertex input.
--
-- A vertex descriptor specifies the layout of your vertex data, allowing your vertex shaders to access the content in your vertex arrays via the @[[stage_in]]@ attribute in Metal Shading Language.
--
-- ObjC selector: @- setVertexDescriptor:@
setVertexDescriptor :: (IsMTL4RenderPipelineDescriptor mtL4RenderPipelineDescriptor, IsMTLVertexDescriptor value) => mtL4RenderPipelineDescriptor -> value -> IO ()
setVertexDescriptor mtL4RenderPipelineDescriptor value =
  sendMessage mtL4RenderPipelineDescriptor setVertexDescriptorSelector (toMTLVertexDescriptor value)

-- | Controls the number of samples this pipeline applies for each fragment.
--
-- ObjC selector: @- rasterSampleCount@
rasterSampleCount :: IsMTL4RenderPipelineDescriptor mtL4RenderPipelineDescriptor => mtL4RenderPipelineDescriptor -> IO CULong
rasterSampleCount mtL4RenderPipelineDescriptor =
  sendMessage mtL4RenderPipelineDescriptor rasterSampleCountSelector

-- | Controls the number of samples this pipeline applies for each fragment.
--
-- ObjC selector: @- setRasterSampleCount:@
setRasterSampleCount :: IsMTL4RenderPipelineDescriptor mtL4RenderPipelineDescriptor => mtL4RenderPipelineDescriptor -> CULong -> IO ()
setRasterSampleCount mtL4RenderPipelineDescriptor value =
  sendMessage mtL4RenderPipelineDescriptor setRasterSampleCountSelector value

-- | Indicates whether to read and use the alpha channel fragment output of color attachments to compute a sample coverage mask.
--
-- ObjC selector: @- alphaToCoverageState@
alphaToCoverageState :: IsMTL4RenderPipelineDescriptor mtL4RenderPipelineDescriptor => mtL4RenderPipelineDescriptor -> IO MTL4AlphaToCoverageState
alphaToCoverageState mtL4RenderPipelineDescriptor =
  sendMessage mtL4RenderPipelineDescriptor alphaToCoverageStateSelector

-- | Indicates whether to read and use the alpha channel fragment output of color attachments to compute a sample coverage mask.
--
-- ObjC selector: @- setAlphaToCoverageState:@
setAlphaToCoverageState :: IsMTL4RenderPipelineDescriptor mtL4RenderPipelineDescriptor => mtL4RenderPipelineDescriptor -> MTL4AlphaToCoverageState -> IO ()
setAlphaToCoverageState mtL4RenderPipelineDescriptor value =
  sendMessage mtL4RenderPipelineDescriptor setAlphaToCoverageStateSelector value

-- | Indicates whether the pipeline forces alpha channel values of color attachments to the largest representable value.
--
-- ObjC selector: @- alphaToOneState@
alphaToOneState :: IsMTL4RenderPipelineDescriptor mtL4RenderPipelineDescriptor => mtL4RenderPipelineDescriptor -> IO MTL4AlphaToOneState
alphaToOneState mtL4RenderPipelineDescriptor =
  sendMessage mtL4RenderPipelineDescriptor alphaToOneStateSelector

-- | Indicates whether the pipeline forces alpha channel values of color attachments to the largest representable value.
--
-- ObjC selector: @- setAlphaToOneState:@
setAlphaToOneState :: IsMTL4RenderPipelineDescriptor mtL4RenderPipelineDescriptor => mtL4RenderPipelineDescriptor -> MTL4AlphaToOneState -> IO ()
setAlphaToOneState mtL4RenderPipelineDescriptor value =
  sendMessage mtL4RenderPipelineDescriptor setAlphaToOneStateSelector value

-- | Determines whether the pipeline rasterizes primitives.
--
-- By default, this value is <doc://com.apple.documentation/documentation/swift/true>, specifying that this pipeline rasterizes primitives. Set this property to <doc://com.apple.documentation/documentation/swift/false> when you don't provide a fragment shader function via function ``fragmentFunctionDescriptor``.
--
-- ObjC selector: @- rasterizationEnabled@
rasterizationEnabled :: IsMTL4RenderPipelineDescriptor mtL4RenderPipelineDescriptor => mtL4RenderPipelineDescriptor -> IO Bool
rasterizationEnabled mtL4RenderPipelineDescriptor =
  sendMessage mtL4RenderPipelineDescriptor rasterizationEnabledSelector

-- | Determines whether the pipeline rasterizes primitives.
--
-- By default, this value is <doc://com.apple.documentation/documentation/swift/true>, specifying that this pipeline rasterizes primitives. Set this property to <doc://com.apple.documentation/documentation/swift/false> when you don't provide a fragment shader function via function ``fragmentFunctionDescriptor``.
--
-- ObjC selector: @- setRasterizationEnabled:@
setRasterizationEnabled :: IsMTL4RenderPipelineDescriptor mtL4RenderPipelineDescriptor => mtL4RenderPipelineDescriptor -> Bool -> IO ()
setRasterizationEnabled mtL4RenderPipelineDescriptor value =
  sendMessage mtL4RenderPipelineDescriptor setRasterizationEnabledSelector value

-- | Determines the maximum value that can you can pass as the pipeline's amplification count.
--
-- This property controls the maximum count you pass to ``MTL4RenderCommandEncoder/setVertexAmplificationCount:viewMappings:`` when using vertex amplification with this pipeline.
--
-- ObjC selector: @- maxVertexAmplificationCount@
maxVertexAmplificationCount :: IsMTL4RenderPipelineDescriptor mtL4RenderPipelineDescriptor => mtL4RenderPipelineDescriptor -> IO CULong
maxVertexAmplificationCount mtL4RenderPipelineDescriptor =
  sendMessage mtL4RenderPipelineDescriptor maxVertexAmplificationCountSelector

-- | Determines the maximum value that can you can pass as the pipeline's amplification count.
--
-- This property controls the maximum count you pass to ``MTL4RenderCommandEncoder/setVertexAmplificationCount:viewMappings:`` when using vertex amplification with this pipeline.
--
-- ObjC selector: @- setMaxVertexAmplificationCount:@
setMaxVertexAmplificationCount :: IsMTL4RenderPipelineDescriptor mtL4RenderPipelineDescriptor => mtL4RenderPipelineDescriptor -> CULong -> IO ()
setMaxVertexAmplificationCount mtL4RenderPipelineDescriptor value =
  sendMessage mtL4RenderPipelineDescriptor setMaxVertexAmplificationCountSelector value

-- | Accesses an array containing descriptions of the color attachments this pipeline writes to.
--
-- ObjC selector: @- colorAttachments@
colorAttachments :: IsMTL4RenderPipelineDescriptor mtL4RenderPipelineDescriptor => mtL4RenderPipelineDescriptor -> IO (Id MTL4RenderPipelineColorAttachmentDescriptorArray)
colorAttachments mtL4RenderPipelineDescriptor =
  sendMessage mtL4RenderPipelineDescriptor colorAttachmentsSelector

-- | Assigns type of primitive topology this pipeline renders.
--
-- ObjC selector: @- inputPrimitiveTopology@
inputPrimitiveTopology :: IsMTL4RenderPipelineDescriptor mtL4RenderPipelineDescriptor => mtL4RenderPipelineDescriptor -> IO MTLPrimitiveTopologyClass
inputPrimitiveTopology mtL4RenderPipelineDescriptor =
  sendMessage mtL4RenderPipelineDescriptor inputPrimitiveTopologySelector

-- | Assigns type of primitive topology this pipeline renders.
--
-- ObjC selector: @- setInputPrimitiveTopology:@
setInputPrimitiveTopology :: IsMTL4RenderPipelineDescriptor mtL4RenderPipelineDescriptor => mtL4RenderPipelineDescriptor -> MTLPrimitiveTopologyClass -> IO ()
setInputPrimitiveTopology mtL4RenderPipelineDescriptor value =
  sendMessage mtL4RenderPipelineDescriptor setInputPrimitiveTopologySelector value

-- | Provides static linking information for the vertex stage of the render pipeline.
--
-- Use this property to link extra shader functions to the vertex stage of the render pipeline.
--
-- ObjC selector: @- vertexStaticLinkingDescriptor@
vertexStaticLinkingDescriptor :: IsMTL4RenderPipelineDescriptor mtL4RenderPipelineDescriptor => mtL4RenderPipelineDescriptor -> IO (Id MTL4StaticLinkingDescriptor)
vertexStaticLinkingDescriptor mtL4RenderPipelineDescriptor =
  sendMessage mtL4RenderPipelineDescriptor vertexStaticLinkingDescriptorSelector

-- | Provides static linking information for the vertex stage of the render pipeline.
--
-- Use this property to link extra shader functions to the vertex stage of the render pipeline.
--
-- ObjC selector: @- setVertexStaticLinkingDescriptor:@
setVertexStaticLinkingDescriptor :: (IsMTL4RenderPipelineDescriptor mtL4RenderPipelineDescriptor, IsMTL4StaticLinkingDescriptor value) => mtL4RenderPipelineDescriptor -> value -> IO ()
setVertexStaticLinkingDescriptor mtL4RenderPipelineDescriptor value =
  sendMessage mtL4RenderPipelineDescriptor setVertexStaticLinkingDescriptorSelector (toMTL4StaticLinkingDescriptor value)

-- | Provides static linking information for the fragment stage of the render pipeline.
--
-- Use this property to link extra shader functions to the fragment stage of the render pipeline.
--
-- ObjC selector: @- fragmentStaticLinkingDescriptor@
fragmentStaticLinkingDescriptor :: IsMTL4RenderPipelineDescriptor mtL4RenderPipelineDescriptor => mtL4RenderPipelineDescriptor -> IO (Id MTL4StaticLinkingDescriptor)
fragmentStaticLinkingDescriptor mtL4RenderPipelineDescriptor =
  sendMessage mtL4RenderPipelineDescriptor fragmentStaticLinkingDescriptorSelector

-- | Provides static linking information for the fragment stage of the render pipeline.
--
-- Use this property to link extra shader functions to the fragment stage of the render pipeline.
--
-- ObjC selector: @- setFragmentStaticLinkingDescriptor:@
setFragmentStaticLinkingDescriptor :: (IsMTL4RenderPipelineDescriptor mtL4RenderPipelineDescriptor, IsMTL4StaticLinkingDescriptor value) => mtL4RenderPipelineDescriptor -> value -> IO ()
setFragmentStaticLinkingDescriptor mtL4RenderPipelineDescriptor value =
  sendMessage mtL4RenderPipelineDescriptor setFragmentStaticLinkingDescriptorSelector (toMTL4StaticLinkingDescriptor value)

-- | Indicates whether you can use the render pipeline to create new pipelines by adding binary functions to the vertex shader function’s callable functions list.
--
-- ObjC selector: @- supportVertexBinaryLinking@
supportVertexBinaryLinking :: IsMTL4RenderPipelineDescriptor mtL4RenderPipelineDescriptor => mtL4RenderPipelineDescriptor -> IO Bool
supportVertexBinaryLinking mtL4RenderPipelineDescriptor =
  sendMessage mtL4RenderPipelineDescriptor supportVertexBinaryLinkingSelector

-- | Indicates whether you can use the render pipeline to create new pipelines by adding binary functions to the vertex shader function’s callable functions list.
--
-- ObjC selector: @- setSupportVertexBinaryLinking:@
setSupportVertexBinaryLinking :: IsMTL4RenderPipelineDescriptor mtL4RenderPipelineDescriptor => mtL4RenderPipelineDescriptor -> Bool -> IO ()
setSupportVertexBinaryLinking mtL4RenderPipelineDescriptor value =
  sendMessage mtL4RenderPipelineDescriptor setSupportVertexBinaryLinkingSelector value

-- | Indicates whether you can use the pipeline to create new pipelines by adding binary functions to the fragment shader function’s callable functions list.
--
-- ObjC selector: @- supportFragmentBinaryLinking@
supportFragmentBinaryLinking :: IsMTL4RenderPipelineDescriptor mtL4RenderPipelineDescriptor => mtL4RenderPipelineDescriptor -> IO Bool
supportFragmentBinaryLinking mtL4RenderPipelineDescriptor =
  sendMessage mtL4RenderPipelineDescriptor supportFragmentBinaryLinkingSelector

-- | Indicates whether you can use the pipeline to create new pipelines by adding binary functions to the fragment shader function’s callable functions list.
--
-- ObjC selector: @- setSupportFragmentBinaryLinking:@
setSupportFragmentBinaryLinking :: IsMTL4RenderPipelineDescriptor mtL4RenderPipelineDescriptor => mtL4RenderPipelineDescriptor -> Bool -> IO ()
setSupportFragmentBinaryLinking mtL4RenderPipelineDescriptor value =
  sendMessage mtL4RenderPipelineDescriptor setSupportFragmentBinaryLinkingSelector value

-- | Configures a logical-to-physical rendering remap state.
--
-- Use this property to assign how a ``MTL4RenderCommandEncoder`` instance maps the output of your fragment shader to physical color attachments.
--
-- ObjC selector: @- colorAttachmentMappingState@
colorAttachmentMappingState :: IsMTL4RenderPipelineDescriptor mtL4RenderPipelineDescriptor => mtL4RenderPipelineDescriptor -> IO MTL4LogicalToPhysicalColorAttachmentMappingState
colorAttachmentMappingState mtL4RenderPipelineDescriptor =
  sendMessage mtL4RenderPipelineDescriptor colorAttachmentMappingStateSelector

-- | Configures a logical-to-physical rendering remap state.
--
-- Use this property to assign how a ``MTL4RenderCommandEncoder`` instance maps the output of your fragment shader to physical color attachments.
--
-- ObjC selector: @- setColorAttachmentMappingState:@
setColorAttachmentMappingState :: IsMTL4RenderPipelineDescriptor mtL4RenderPipelineDescriptor => mtL4RenderPipelineDescriptor -> MTL4LogicalToPhysicalColorAttachmentMappingState -> IO ()
setColorAttachmentMappingState mtL4RenderPipelineDescriptor value =
  sendMessage mtL4RenderPipelineDescriptor setColorAttachmentMappingStateSelector value

-- | Indicates whether the pipeline supports indirect command buffers.
--
-- ObjC selector: @- supportIndirectCommandBuffers@
supportIndirectCommandBuffers :: IsMTL4RenderPipelineDescriptor mtL4RenderPipelineDescriptor => mtL4RenderPipelineDescriptor -> IO MTL4IndirectCommandBufferSupportState
supportIndirectCommandBuffers mtL4RenderPipelineDescriptor =
  sendMessage mtL4RenderPipelineDescriptor supportIndirectCommandBuffersSelector

-- | Indicates whether the pipeline supports indirect command buffers.
--
-- ObjC selector: @- setSupportIndirectCommandBuffers:@
setSupportIndirectCommandBuffers :: IsMTL4RenderPipelineDescriptor mtL4RenderPipelineDescriptor => mtL4RenderPipelineDescriptor -> MTL4IndirectCommandBufferSupportState -> IO ()
setSupportIndirectCommandBuffers mtL4RenderPipelineDescriptor value =
  sendMessage mtL4RenderPipelineDescriptor setSupportIndirectCommandBuffersSelector value

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @reset@
resetSelector :: Selector '[] ()
resetSelector = mkSelector "reset"

-- | @Selector@ for @vertexFunctionDescriptor@
vertexFunctionDescriptorSelector :: Selector '[] (Id MTL4FunctionDescriptor)
vertexFunctionDescriptorSelector = mkSelector "vertexFunctionDescriptor"

-- | @Selector@ for @setVertexFunctionDescriptor:@
setVertexFunctionDescriptorSelector :: Selector '[Id MTL4FunctionDescriptor] ()
setVertexFunctionDescriptorSelector = mkSelector "setVertexFunctionDescriptor:"

-- | @Selector@ for @fragmentFunctionDescriptor@
fragmentFunctionDescriptorSelector :: Selector '[] (Id MTL4FunctionDescriptor)
fragmentFunctionDescriptorSelector = mkSelector "fragmentFunctionDescriptor"

-- | @Selector@ for @setFragmentFunctionDescriptor:@
setFragmentFunctionDescriptorSelector :: Selector '[Id MTL4FunctionDescriptor] ()
setFragmentFunctionDescriptorSelector = mkSelector "setFragmentFunctionDescriptor:"

-- | @Selector@ for @vertexDescriptor@
vertexDescriptorSelector :: Selector '[] (Id MTLVertexDescriptor)
vertexDescriptorSelector = mkSelector "vertexDescriptor"

-- | @Selector@ for @setVertexDescriptor:@
setVertexDescriptorSelector :: Selector '[Id MTLVertexDescriptor] ()
setVertexDescriptorSelector = mkSelector "setVertexDescriptor:"

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

-- | @Selector@ for @inputPrimitiveTopology@
inputPrimitiveTopologySelector :: Selector '[] MTLPrimitiveTopologyClass
inputPrimitiveTopologySelector = mkSelector "inputPrimitiveTopology"

-- | @Selector@ for @setInputPrimitiveTopology:@
setInputPrimitiveTopologySelector :: Selector '[MTLPrimitiveTopologyClass] ()
setInputPrimitiveTopologySelector = mkSelector "setInputPrimitiveTopology:"

-- | @Selector@ for @vertexStaticLinkingDescriptor@
vertexStaticLinkingDescriptorSelector :: Selector '[] (Id MTL4StaticLinkingDescriptor)
vertexStaticLinkingDescriptorSelector = mkSelector "vertexStaticLinkingDescriptor"

-- | @Selector@ for @setVertexStaticLinkingDescriptor:@
setVertexStaticLinkingDescriptorSelector :: Selector '[Id MTL4StaticLinkingDescriptor] ()
setVertexStaticLinkingDescriptorSelector = mkSelector "setVertexStaticLinkingDescriptor:"

-- | @Selector@ for @fragmentStaticLinkingDescriptor@
fragmentStaticLinkingDescriptorSelector :: Selector '[] (Id MTL4StaticLinkingDescriptor)
fragmentStaticLinkingDescriptorSelector = mkSelector "fragmentStaticLinkingDescriptor"

-- | @Selector@ for @setFragmentStaticLinkingDescriptor:@
setFragmentStaticLinkingDescriptorSelector :: Selector '[Id MTL4StaticLinkingDescriptor] ()
setFragmentStaticLinkingDescriptorSelector = mkSelector "setFragmentStaticLinkingDescriptor:"

-- | @Selector@ for @supportVertexBinaryLinking@
supportVertexBinaryLinkingSelector :: Selector '[] Bool
supportVertexBinaryLinkingSelector = mkSelector "supportVertexBinaryLinking"

-- | @Selector@ for @setSupportVertexBinaryLinking:@
setSupportVertexBinaryLinkingSelector :: Selector '[Bool] ()
setSupportVertexBinaryLinkingSelector = mkSelector "setSupportVertexBinaryLinking:"

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

