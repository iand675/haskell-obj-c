{-# LANGUAGE PatternSynonyms #-}
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
  , resetSelector
  , vertexFunctionDescriptorSelector
  , setVertexFunctionDescriptorSelector
  , fragmentFunctionDescriptorSelector
  , setFragmentFunctionDescriptorSelector
  , vertexDescriptorSelector
  , setVertexDescriptorSelector
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
  , inputPrimitiveTopologySelector
  , setInputPrimitiveTopologySelector
  , vertexStaticLinkingDescriptorSelector
  , setVertexStaticLinkingDescriptorSelector
  , fragmentStaticLinkingDescriptorSelector
  , setFragmentStaticLinkingDescriptorSelector
  , supportVertexBinaryLinkingSelector
  , setSupportVertexBinaryLinkingSelector
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
  , MTLPrimitiveTopologyClass(MTLPrimitiveTopologyClass)
  , pattern MTLPrimitiveTopologyClassUnspecified
  , pattern MTLPrimitiveTopologyClassPoint
  , pattern MTLPrimitiveTopologyClassLine
  , pattern MTLPrimitiveTopologyClassTriangle

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
reset :: IsMTL4RenderPipelineDescriptor mtL4RenderPipelineDescriptor => mtL4RenderPipelineDescriptor -> IO ()
reset mtL4RenderPipelineDescriptor  =
  sendMsg mtL4RenderPipelineDescriptor (mkSelector "reset") retVoid []

-- | Assigns the shader function that this pipeline executes for each vertex.
--
-- ObjC selector: @- vertexFunctionDescriptor@
vertexFunctionDescriptor :: IsMTL4RenderPipelineDescriptor mtL4RenderPipelineDescriptor => mtL4RenderPipelineDescriptor -> IO (Id MTL4FunctionDescriptor)
vertexFunctionDescriptor mtL4RenderPipelineDescriptor  =
  sendMsg mtL4RenderPipelineDescriptor (mkSelector "vertexFunctionDescriptor") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | Assigns the shader function that this pipeline executes for each vertex.
--
-- ObjC selector: @- setVertexFunctionDescriptor:@
setVertexFunctionDescriptor :: (IsMTL4RenderPipelineDescriptor mtL4RenderPipelineDescriptor, IsMTL4FunctionDescriptor value) => mtL4RenderPipelineDescriptor -> value -> IO ()
setVertexFunctionDescriptor mtL4RenderPipelineDescriptor  value =
withObjCPtr value $ \raw_value ->
    sendMsg mtL4RenderPipelineDescriptor (mkSelector "setVertexFunctionDescriptor:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- | Assigns the shader function that this pipeline executes for each fragment.
--
-- When you don't specify a fragment function, you need to disable rasterization by setting property ``rasterizationEnabled`` to false.
--
-- ObjC selector: @- fragmentFunctionDescriptor@
fragmentFunctionDescriptor :: IsMTL4RenderPipelineDescriptor mtL4RenderPipelineDescriptor => mtL4RenderPipelineDescriptor -> IO (Id MTL4FunctionDescriptor)
fragmentFunctionDescriptor mtL4RenderPipelineDescriptor  =
  sendMsg mtL4RenderPipelineDescriptor (mkSelector "fragmentFunctionDescriptor") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | Assigns the shader function that this pipeline executes for each fragment.
--
-- When you don't specify a fragment function, you need to disable rasterization by setting property ``rasterizationEnabled`` to false.
--
-- ObjC selector: @- setFragmentFunctionDescriptor:@
setFragmentFunctionDescriptor :: (IsMTL4RenderPipelineDescriptor mtL4RenderPipelineDescriptor, IsMTL4FunctionDescriptor value) => mtL4RenderPipelineDescriptor -> value -> IO ()
setFragmentFunctionDescriptor mtL4RenderPipelineDescriptor  value =
withObjCPtr value $ \raw_value ->
    sendMsg mtL4RenderPipelineDescriptor (mkSelector "setFragmentFunctionDescriptor:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- | Configures an optional vertex descriptor for the vertex input.
--
-- A vertex descriptor specifies the layout of your vertex data, allowing your vertex shaders to access the content in your vertex arrays via the @[[stage_in]]@ attribute in Metal Shading Language.
--
-- ObjC selector: @- vertexDescriptor@
vertexDescriptor :: IsMTL4RenderPipelineDescriptor mtL4RenderPipelineDescriptor => mtL4RenderPipelineDescriptor -> IO (Id MTLVertexDescriptor)
vertexDescriptor mtL4RenderPipelineDescriptor  =
  sendMsg mtL4RenderPipelineDescriptor (mkSelector "vertexDescriptor") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | Configures an optional vertex descriptor for the vertex input.
--
-- A vertex descriptor specifies the layout of your vertex data, allowing your vertex shaders to access the content in your vertex arrays via the @[[stage_in]]@ attribute in Metal Shading Language.
--
-- ObjC selector: @- setVertexDescriptor:@
setVertexDescriptor :: (IsMTL4RenderPipelineDescriptor mtL4RenderPipelineDescriptor, IsMTLVertexDescriptor value) => mtL4RenderPipelineDescriptor -> value -> IO ()
setVertexDescriptor mtL4RenderPipelineDescriptor  value =
withObjCPtr value $ \raw_value ->
    sendMsg mtL4RenderPipelineDescriptor (mkSelector "setVertexDescriptor:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- | Controls the number of samples this pipeline applies for each fragment.
--
-- ObjC selector: @- rasterSampleCount@
rasterSampleCount :: IsMTL4RenderPipelineDescriptor mtL4RenderPipelineDescriptor => mtL4RenderPipelineDescriptor -> IO CULong
rasterSampleCount mtL4RenderPipelineDescriptor  =
  sendMsg mtL4RenderPipelineDescriptor (mkSelector "rasterSampleCount") retCULong []

-- | Controls the number of samples this pipeline applies for each fragment.
--
-- ObjC selector: @- setRasterSampleCount:@
setRasterSampleCount :: IsMTL4RenderPipelineDescriptor mtL4RenderPipelineDescriptor => mtL4RenderPipelineDescriptor -> CULong -> IO ()
setRasterSampleCount mtL4RenderPipelineDescriptor  value =
  sendMsg mtL4RenderPipelineDescriptor (mkSelector "setRasterSampleCount:") retVoid [argCULong (fromIntegral value)]

-- | Indicates whether to read and use the alpha channel fragment output of color attachments to compute a sample coverage mask.
--
-- ObjC selector: @- alphaToCoverageState@
alphaToCoverageState :: IsMTL4RenderPipelineDescriptor mtL4RenderPipelineDescriptor => mtL4RenderPipelineDescriptor -> IO MTL4AlphaToCoverageState
alphaToCoverageState mtL4RenderPipelineDescriptor  =
  fmap (coerce :: CLong -> MTL4AlphaToCoverageState) $ sendMsg mtL4RenderPipelineDescriptor (mkSelector "alphaToCoverageState") retCLong []

-- | Indicates whether to read and use the alpha channel fragment output of color attachments to compute a sample coverage mask.
--
-- ObjC selector: @- setAlphaToCoverageState:@
setAlphaToCoverageState :: IsMTL4RenderPipelineDescriptor mtL4RenderPipelineDescriptor => mtL4RenderPipelineDescriptor -> MTL4AlphaToCoverageState -> IO ()
setAlphaToCoverageState mtL4RenderPipelineDescriptor  value =
  sendMsg mtL4RenderPipelineDescriptor (mkSelector "setAlphaToCoverageState:") retVoid [argCLong (coerce value)]

-- | Indicates whether the pipeline forces alpha channel values of color attachments to the largest representable value.
--
-- ObjC selector: @- alphaToOneState@
alphaToOneState :: IsMTL4RenderPipelineDescriptor mtL4RenderPipelineDescriptor => mtL4RenderPipelineDescriptor -> IO MTL4AlphaToOneState
alphaToOneState mtL4RenderPipelineDescriptor  =
  fmap (coerce :: CLong -> MTL4AlphaToOneState) $ sendMsg mtL4RenderPipelineDescriptor (mkSelector "alphaToOneState") retCLong []

-- | Indicates whether the pipeline forces alpha channel values of color attachments to the largest representable value.
--
-- ObjC selector: @- setAlphaToOneState:@
setAlphaToOneState :: IsMTL4RenderPipelineDescriptor mtL4RenderPipelineDescriptor => mtL4RenderPipelineDescriptor -> MTL4AlphaToOneState -> IO ()
setAlphaToOneState mtL4RenderPipelineDescriptor  value =
  sendMsg mtL4RenderPipelineDescriptor (mkSelector "setAlphaToOneState:") retVoid [argCLong (coerce value)]

-- | Determines whether the pipeline rasterizes primitives.
--
-- By default, this value is <doc://com.apple.documentation/documentation/swift/true>, specifying that this pipeline rasterizes primitives. Set this property to <doc://com.apple.documentation/documentation/swift/false> when you don't provide a fragment shader function via function ``fragmentFunctionDescriptor``.
--
-- ObjC selector: @- rasterizationEnabled@
rasterizationEnabled :: IsMTL4RenderPipelineDescriptor mtL4RenderPipelineDescriptor => mtL4RenderPipelineDescriptor -> IO Bool
rasterizationEnabled mtL4RenderPipelineDescriptor  =
  fmap ((/= 0) :: CULong -> Bool) $ sendMsg mtL4RenderPipelineDescriptor (mkSelector "rasterizationEnabled") retCULong []

-- | Determines whether the pipeline rasterizes primitives.
--
-- By default, this value is <doc://com.apple.documentation/documentation/swift/true>, specifying that this pipeline rasterizes primitives. Set this property to <doc://com.apple.documentation/documentation/swift/false> when you don't provide a fragment shader function via function ``fragmentFunctionDescriptor``.
--
-- ObjC selector: @- setRasterizationEnabled:@
setRasterizationEnabled :: IsMTL4RenderPipelineDescriptor mtL4RenderPipelineDescriptor => mtL4RenderPipelineDescriptor -> Bool -> IO ()
setRasterizationEnabled mtL4RenderPipelineDescriptor  value =
  sendMsg mtL4RenderPipelineDescriptor (mkSelector "setRasterizationEnabled:") retVoid [argCULong (if value then 1 else 0)]

-- | Determines the maximum value that can you can pass as the pipeline's amplification count.
--
-- This property controls the maximum count you pass to ``MTL4RenderCommandEncoder/setVertexAmplificationCount:viewMappings:`` when using vertex amplification with this pipeline.
--
-- ObjC selector: @- maxVertexAmplificationCount@
maxVertexAmplificationCount :: IsMTL4RenderPipelineDescriptor mtL4RenderPipelineDescriptor => mtL4RenderPipelineDescriptor -> IO CULong
maxVertexAmplificationCount mtL4RenderPipelineDescriptor  =
  sendMsg mtL4RenderPipelineDescriptor (mkSelector "maxVertexAmplificationCount") retCULong []

-- | Determines the maximum value that can you can pass as the pipeline's amplification count.
--
-- This property controls the maximum count you pass to ``MTL4RenderCommandEncoder/setVertexAmplificationCount:viewMappings:`` when using vertex amplification with this pipeline.
--
-- ObjC selector: @- setMaxVertexAmplificationCount:@
setMaxVertexAmplificationCount :: IsMTL4RenderPipelineDescriptor mtL4RenderPipelineDescriptor => mtL4RenderPipelineDescriptor -> CULong -> IO ()
setMaxVertexAmplificationCount mtL4RenderPipelineDescriptor  value =
  sendMsg mtL4RenderPipelineDescriptor (mkSelector "setMaxVertexAmplificationCount:") retVoid [argCULong (fromIntegral value)]

-- | Accesses an array containing descriptions of the color attachments this pipeline writes to.
--
-- ObjC selector: @- colorAttachments@
colorAttachments :: IsMTL4RenderPipelineDescriptor mtL4RenderPipelineDescriptor => mtL4RenderPipelineDescriptor -> IO (Id MTL4RenderPipelineColorAttachmentDescriptorArray)
colorAttachments mtL4RenderPipelineDescriptor  =
  sendMsg mtL4RenderPipelineDescriptor (mkSelector "colorAttachments") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | Assigns type of primitive topology this pipeline renders.
--
-- ObjC selector: @- inputPrimitiveTopology@
inputPrimitiveTopology :: IsMTL4RenderPipelineDescriptor mtL4RenderPipelineDescriptor => mtL4RenderPipelineDescriptor -> IO MTLPrimitiveTopologyClass
inputPrimitiveTopology mtL4RenderPipelineDescriptor  =
  fmap (coerce :: CULong -> MTLPrimitiveTopologyClass) $ sendMsg mtL4RenderPipelineDescriptor (mkSelector "inputPrimitiveTopology") retCULong []

-- | Assigns type of primitive topology this pipeline renders.
--
-- ObjC selector: @- setInputPrimitiveTopology:@
setInputPrimitiveTopology :: IsMTL4RenderPipelineDescriptor mtL4RenderPipelineDescriptor => mtL4RenderPipelineDescriptor -> MTLPrimitiveTopologyClass -> IO ()
setInputPrimitiveTopology mtL4RenderPipelineDescriptor  value =
  sendMsg mtL4RenderPipelineDescriptor (mkSelector "setInputPrimitiveTopology:") retVoid [argCULong (coerce value)]

-- | Provides static linking information for the vertex stage of the render pipeline.
--
-- Use this property to link extra shader functions to the vertex stage of the render pipeline.
--
-- ObjC selector: @- vertexStaticLinkingDescriptor@
vertexStaticLinkingDescriptor :: IsMTL4RenderPipelineDescriptor mtL4RenderPipelineDescriptor => mtL4RenderPipelineDescriptor -> IO (Id MTL4StaticLinkingDescriptor)
vertexStaticLinkingDescriptor mtL4RenderPipelineDescriptor  =
  sendMsg mtL4RenderPipelineDescriptor (mkSelector "vertexStaticLinkingDescriptor") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | Provides static linking information for the vertex stage of the render pipeline.
--
-- Use this property to link extra shader functions to the vertex stage of the render pipeline.
--
-- ObjC selector: @- setVertexStaticLinkingDescriptor:@
setVertexStaticLinkingDescriptor :: (IsMTL4RenderPipelineDescriptor mtL4RenderPipelineDescriptor, IsMTL4StaticLinkingDescriptor value) => mtL4RenderPipelineDescriptor -> value -> IO ()
setVertexStaticLinkingDescriptor mtL4RenderPipelineDescriptor  value =
withObjCPtr value $ \raw_value ->
    sendMsg mtL4RenderPipelineDescriptor (mkSelector "setVertexStaticLinkingDescriptor:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- | Provides static linking information for the fragment stage of the render pipeline.
--
-- Use this property to link extra shader functions to the fragment stage of the render pipeline.
--
-- ObjC selector: @- fragmentStaticLinkingDescriptor@
fragmentStaticLinkingDescriptor :: IsMTL4RenderPipelineDescriptor mtL4RenderPipelineDescriptor => mtL4RenderPipelineDescriptor -> IO (Id MTL4StaticLinkingDescriptor)
fragmentStaticLinkingDescriptor mtL4RenderPipelineDescriptor  =
  sendMsg mtL4RenderPipelineDescriptor (mkSelector "fragmentStaticLinkingDescriptor") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | Provides static linking information for the fragment stage of the render pipeline.
--
-- Use this property to link extra shader functions to the fragment stage of the render pipeline.
--
-- ObjC selector: @- setFragmentStaticLinkingDescriptor:@
setFragmentStaticLinkingDescriptor :: (IsMTL4RenderPipelineDescriptor mtL4RenderPipelineDescriptor, IsMTL4StaticLinkingDescriptor value) => mtL4RenderPipelineDescriptor -> value -> IO ()
setFragmentStaticLinkingDescriptor mtL4RenderPipelineDescriptor  value =
withObjCPtr value $ \raw_value ->
    sendMsg mtL4RenderPipelineDescriptor (mkSelector "setFragmentStaticLinkingDescriptor:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- | Indicates whether you can use the render pipeline to create new pipelines by adding binary functions to the vertex shader function’s callable functions list.
--
-- ObjC selector: @- supportVertexBinaryLinking@
supportVertexBinaryLinking :: IsMTL4RenderPipelineDescriptor mtL4RenderPipelineDescriptor => mtL4RenderPipelineDescriptor -> IO Bool
supportVertexBinaryLinking mtL4RenderPipelineDescriptor  =
  fmap ((/= 0) :: CULong -> Bool) $ sendMsg mtL4RenderPipelineDescriptor (mkSelector "supportVertexBinaryLinking") retCULong []

-- | Indicates whether you can use the render pipeline to create new pipelines by adding binary functions to the vertex shader function’s callable functions list.
--
-- ObjC selector: @- setSupportVertexBinaryLinking:@
setSupportVertexBinaryLinking :: IsMTL4RenderPipelineDescriptor mtL4RenderPipelineDescriptor => mtL4RenderPipelineDescriptor -> Bool -> IO ()
setSupportVertexBinaryLinking mtL4RenderPipelineDescriptor  value =
  sendMsg mtL4RenderPipelineDescriptor (mkSelector "setSupportVertexBinaryLinking:") retVoid [argCULong (if value then 1 else 0)]

-- | Indicates whether you can use the pipeline to create new pipelines by adding binary functions to the fragment shader function’s callable functions list.
--
-- ObjC selector: @- supportFragmentBinaryLinking@
supportFragmentBinaryLinking :: IsMTL4RenderPipelineDescriptor mtL4RenderPipelineDescriptor => mtL4RenderPipelineDescriptor -> IO Bool
supportFragmentBinaryLinking mtL4RenderPipelineDescriptor  =
  fmap ((/= 0) :: CULong -> Bool) $ sendMsg mtL4RenderPipelineDescriptor (mkSelector "supportFragmentBinaryLinking") retCULong []

-- | Indicates whether you can use the pipeline to create new pipelines by adding binary functions to the fragment shader function’s callable functions list.
--
-- ObjC selector: @- setSupportFragmentBinaryLinking:@
setSupportFragmentBinaryLinking :: IsMTL4RenderPipelineDescriptor mtL4RenderPipelineDescriptor => mtL4RenderPipelineDescriptor -> Bool -> IO ()
setSupportFragmentBinaryLinking mtL4RenderPipelineDescriptor  value =
  sendMsg mtL4RenderPipelineDescriptor (mkSelector "setSupportFragmentBinaryLinking:") retVoid [argCULong (if value then 1 else 0)]

-- | Configures a logical-to-physical rendering remap state.
--
-- Use this property to assign how a ``MTL4RenderCommandEncoder`` instance maps the output of your fragment shader to physical color attachments.
--
-- ObjC selector: @- colorAttachmentMappingState@
colorAttachmentMappingState :: IsMTL4RenderPipelineDescriptor mtL4RenderPipelineDescriptor => mtL4RenderPipelineDescriptor -> IO MTL4LogicalToPhysicalColorAttachmentMappingState
colorAttachmentMappingState mtL4RenderPipelineDescriptor  =
  fmap (coerce :: CLong -> MTL4LogicalToPhysicalColorAttachmentMappingState) $ sendMsg mtL4RenderPipelineDescriptor (mkSelector "colorAttachmentMappingState") retCLong []

-- | Configures a logical-to-physical rendering remap state.
--
-- Use this property to assign how a ``MTL4RenderCommandEncoder`` instance maps the output of your fragment shader to physical color attachments.
--
-- ObjC selector: @- setColorAttachmentMappingState:@
setColorAttachmentMappingState :: IsMTL4RenderPipelineDescriptor mtL4RenderPipelineDescriptor => mtL4RenderPipelineDescriptor -> MTL4LogicalToPhysicalColorAttachmentMappingState -> IO ()
setColorAttachmentMappingState mtL4RenderPipelineDescriptor  value =
  sendMsg mtL4RenderPipelineDescriptor (mkSelector "setColorAttachmentMappingState:") retVoid [argCLong (coerce value)]

-- | Indicates whether the pipeline supports indirect command buffers.
--
-- ObjC selector: @- supportIndirectCommandBuffers@
supportIndirectCommandBuffers :: IsMTL4RenderPipelineDescriptor mtL4RenderPipelineDescriptor => mtL4RenderPipelineDescriptor -> IO MTL4IndirectCommandBufferSupportState
supportIndirectCommandBuffers mtL4RenderPipelineDescriptor  =
  fmap (coerce :: CLong -> MTL4IndirectCommandBufferSupportState) $ sendMsg mtL4RenderPipelineDescriptor (mkSelector "supportIndirectCommandBuffers") retCLong []

-- | Indicates whether the pipeline supports indirect command buffers.
--
-- ObjC selector: @- setSupportIndirectCommandBuffers:@
setSupportIndirectCommandBuffers :: IsMTL4RenderPipelineDescriptor mtL4RenderPipelineDescriptor => mtL4RenderPipelineDescriptor -> MTL4IndirectCommandBufferSupportState -> IO ()
setSupportIndirectCommandBuffers mtL4RenderPipelineDescriptor  value =
  sendMsg mtL4RenderPipelineDescriptor (mkSelector "setSupportIndirectCommandBuffers:") retVoid [argCLong (coerce value)]

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @reset@
resetSelector :: Selector
resetSelector = mkSelector "reset"

-- | @Selector@ for @vertexFunctionDescriptor@
vertexFunctionDescriptorSelector :: Selector
vertexFunctionDescriptorSelector = mkSelector "vertexFunctionDescriptor"

-- | @Selector@ for @setVertexFunctionDescriptor:@
setVertexFunctionDescriptorSelector :: Selector
setVertexFunctionDescriptorSelector = mkSelector "setVertexFunctionDescriptor:"

-- | @Selector@ for @fragmentFunctionDescriptor@
fragmentFunctionDescriptorSelector :: Selector
fragmentFunctionDescriptorSelector = mkSelector "fragmentFunctionDescriptor"

-- | @Selector@ for @setFragmentFunctionDescriptor:@
setFragmentFunctionDescriptorSelector :: Selector
setFragmentFunctionDescriptorSelector = mkSelector "setFragmentFunctionDescriptor:"

-- | @Selector@ for @vertexDescriptor@
vertexDescriptorSelector :: Selector
vertexDescriptorSelector = mkSelector "vertexDescriptor"

-- | @Selector@ for @setVertexDescriptor:@
setVertexDescriptorSelector :: Selector
setVertexDescriptorSelector = mkSelector "setVertexDescriptor:"

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

-- | @Selector@ for @inputPrimitiveTopology@
inputPrimitiveTopologySelector :: Selector
inputPrimitiveTopologySelector = mkSelector "inputPrimitiveTopology"

-- | @Selector@ for @setInputPrimitiveTopology:@
setInputPrimitiveTopologySelector :: Selector
setInputPrimitiveTopologySelector = mkSelector "setInputPrimitiveTopology:"

-- | @Selector@ for @vertexStaticLinkingDescriptor@
vertexStaticLinkingDescriptorSelector :: Selector
vertexStaticLinkingDescriptorSelector = mkSelector "vertexStaticLinkingDescriptor"

-- | @Selector@ for @setVertexStaticLinkingDescriptor:@
setVertexStaticLinkingDescriptorSelector :: Selector
setVertexStaticLinkingDescriptorSelector = mkSelector "setVertexStaticLinkingDescriptor:"

-- | @Selector@ for @fragmentStaticLinkingDescriptor@
fragmentStaticLinkingDescriptorSelector :: Selector
fragmentStaticLinkingDescriptorSelector = mkSelector "fragmentStaticLinkingDescriptor"

-- | @Selector@ for @setFragmentStaticLinkingDescriptor:@
setFragmentStaticLinkingDescriptorSelector :: Selector
setFragmentStaticLinkingDescriptorSelector = mkSelector "setFragmentStaticLinkingDescriptor:"

-- | @Selector@ for @supportVertexBinaryLinking@
supportVertexBinaryLinkingSelector :: Selector
supportVertexBinaryLinkingSelector = mkSelector "supportVertexBinaryLinking"

-- | @Selector@ for @setSupportVertexBinaryLinking:@
setSupportVertexBinaryLinkingSelector :: Selector
setSupportVertexBinaryLinkingSelector = mkSelector "setSupportVertexBinaryLinking:"

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

