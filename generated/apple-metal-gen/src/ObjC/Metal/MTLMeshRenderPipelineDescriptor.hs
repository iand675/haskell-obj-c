{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | MTLMeshRenderPipelineDescriptor
--
-- As an alternative to a vertex + fragment shader render pipeline, this render pipeline uses a (object +) mesh + fragment shader for rendering geometry.
--
-- Generated bindings for @MTLMeshRenderPipelineDescriptor@.
module ObjC.Metal.MTLMeshRenderPipelineDescriptor
  ( MTLMeshRenderPipelineDescriptor
  , IsMTLMeshRenderPipelineDescriptor(..)
  , reset
  , label
  , setLabel
  , objectFunction
  , setObjectFunction
  , meshFunction
  , setMeshFunction
  , fragmentFunction
  , setFragmentFunction
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
  , objectBuffers
  , meshBuffers
  , fragmentBuffers
  , rasterSampleCount
  , setRasterSampleCount
  , alphaToCoverageEnabled
  , setAlphaToCoverageEnabled
  , alphaToOneEnabled
  , setAlphaToOneEnabled
  , rasterizationEnabled
  , setRasterizationEnabled
  , maxVertexAmplificationCount
  , setMaxVertexAmplificationCount
  , colorAttachments
  , depthAttachmentPixelFormat
  , setDepthAttachmentPixelFormat
  , stencilAttachmentPixelFormat
  , setStencilAttachmentPixelFormat
  , supportIndirectCommandBuffers
  , setSupportIndirectCommandBuffers
  , binaryArchives
  , setBinaryArchives
  , objectLinkedFunctions
  , setObjectLinkedFunctions
  , meshLinkedFunctions
  , setMeshLinkedFunctions
  , fragmentLinkedFunctions
  , setFragmentLinkedFunctions
  , shaderValidation
  , setShaderValidation
  , resetSelector
  , labelSelector
  , setLabelSelector
  , objectFunctionSelector
  , setObjectFunctionSelector
  , meshFunctionSelector
  , setMeshFunctionSelector
  , fragmentFunctionSelector
  , setFragmentFunctionSelector
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
  , objectBuffersSelector
  , meshBuffersSelector
  , fragmentBuffersSelector
  , rasterSampleCountSelector
  , setRasterSampleCountSelector
  , alphaToCoverageEnabledSelector
  , setAlphaToCoverageEnabledSelector
  , alphaToOneEnabledSelector
  , setAlphaToOneEnabledSelector
  , rasterizationEnabledSelector
  , setRasterizationEnabledSelector
  , maxVertexAmplificationCountSelector
  , setMaxVertexAmplificationCountSelector
  , colorAttachmentsSelector
  , depthAttachmentPixelFormatSelector
  , setDepthAttachmentPixelFormatSelector
  , stencilAttachmentPixelFormatSelector
  , setStencilAttachmentPixelFormatSelector
  , supportIndirectCommandBuffersSelector
  , setSupportIndirectCommandBuffersSelector
  , binaryArchivesSelector
  , setBinaryArchivesSelector
  , objectLinkedFunctionsSelector
  , setObjectLinkedFunctionsSelector
  , meshLinkedFunctionsSelector
  , setMeshLinkedFunctionsSelector
  , fragmentLinkedFunctionsSelector
  , setFragmentLinkedFunctionsSelector
  , shaderValidationSelector
  , setShaderValidationSelector

  -- * Enum types
  , MTLPixelFormat(MTLPixelFormat)
  , pattern MTLPixelFormatInvalid
  , pattern MTLPixelFormatA8Unorm
  , pattern MTLPixelFormatR8Unorm
  , pattern MTLPixelFormatR8Unorm_sRGB
  , pattern MTLPixelFormatR8Snorm
  , pattern MTLPixelFormatR8Uint
  , pattern MTLPixelFormatR8Sint
  , pattern MTLPixelFormatR16Unorm
  , pattern MTLPixelFormatR16Snorm
  , pattern MTLPixelFormatR16Uint
  , pattern MTLPixelFormatR16Sint
  , pattern MTLPixelFormatR16Float
  , pattern MTLPixelFormatRG8Unorm
  , pattern MTLPixelFormatRG8Unorm_sRGB
  , pattern MTLPixelFormatRG8Snorm
  , pattern MTLPixelFormatRG8Uint
  , pattern MTLPixelFormatRG8Sint
  , pattern MTLPixelFormatB5G6R5Unorm
  , pattern MTLPixelFormatA1BGR5Unorm
  , pattern MTLPixelFormatABGR4Unorm
  , pattern MTLPixelFormatBGR5A1Unorm
  , pattern MTLPixelFormatR32Uint
  , pattern MTLPixelFormatR32Sint
  , pattern MTLPixelFormatR32Float
  , pattern MTLPixelFormatRG16Unorm
  , pattern MTLPixelFormatRG16Snorm
  , pattern MTLPixelFormatRG16Uint
  , pattern MTLPixelFormatRG16Sint
  , pattern MTLPixelFormatRG16Float
  , pattern MTLPixelFormatRGBA8Unorm
  , pattern MTLPixelFormatRGBA8Unorm_sRGB
  , pattern MTLPixelFormatRGBA8Snorm
  , pattern MTLPixelFormatRGBA8Uint
  , pattern MTLPixelFormatRGBA8Sint
  , pattern MTLPixelFormatBGRA8Unorm
  , pattern MTLPixelFormatBGRA8Unorm_sRGB
  , pattern MTLPixelFormatRGB10A2Unorm
  , pattern MTLPixelFormatRGB10A2Uint
  , pattern MTLPixelFormatRG11B10Float
  , pattern MTLPixelFormatRGB9E5Float
  , pattern MTLPixelFormatBGR10A2Unorm
  , pattern MTLPixelFormatBGR10_XR
  , pattern MTLPixelFormatBGR10_XR_sRGB
  , pattern MTLPixelFormatRG32Uint
  , pattern MTLPixelFormatRG32Sint
  , pattern MTLPixelFormatRG32Float
  , pattern MTLPixelFormatRGBA16Unorm
  , pattern MTLPixelFormatRGBA16Snorm
  , pattern MTLPixelFormatRGBA16Uint
  , pattern MTLPixelFormatRGBA16Sint
  , pattern MTLPixelFormatRGBA16Float
  , pattern MTLPixelFormatBGRA10_XR
  , pattern MTLPixelFormatBGRA10_XR_sRGB
  , pattern MTLPixelFormatRGBA32Uint
  , pattern MTLPixelFormatRGBA32Sint
  , pattern MTLPixelFormatRGBA32Float
  , pattern MTLPixelFormatBC1_RGBA
  , pattern MTLPixelFormatBC1_RGBA_sRGB
  , pattern MTLPixelFormatBC2_RGBA
  , pattern MTLPixelFormatBC2_RGBA_sRGB
  , pattern MTLPixelFormatBC3_RGBA
  , pattern MTLPixelFormatBC3_RGBA_sRGB
  , pattern MTLPixelFormatBC4_RUnorm
  , pattern MTLPixelFormatBC4_RSnorm
  , pattern MTLPixelFormatBC5_RGUnorm
  , pattern MTLPixelFormatBC5_RGSnorm
  , pattern MTLPixelFormatBC6H_RGBFloat
  , pattern MTLPixelFormatBC6H_RGBUfloat
  , pattern MTLPixelFormatBC7_RGBAUnorm
  , pattern MTLPixelFormatBC7_RGBAUnorm_sRGB
  , pattern MTLPixelFormatPVRTC_RGB_2BPP
  , pattern MTLPixelFormatPVRTC_RGB_2BPP_sRGB
  , pattern MTLPixelFormatPVRTC_RGB_4BPP
  , pattern MTLPixelFormatPVRTC_RGB_4BPP_sRGB
  , pattern MTLPixelFormatPVRTC_RGBA_2BPP
  , pattern MTLPixelFormatPVRTC_RGBA_2BPP_sRGB
  , pattern MTLPixelFormatPVRTC_RGBA_4BPP
  , pattern MTLPixelFormatPVRTC_RGBA_4BPP_sRGB
  , pattern MTLPixelFormatEAC_R11Unorm
  , pattern MTLPixelFormatEAC_R11Snorm
  , pattern MTLPixelFormatEAC_RG11Unorm
  , pattern MTLPixelFormatEAC_RG11Snorm
  , pattern MTLPixelFormatEAC_RGBA8
  , pattern MTLPixelFormatEAC_RGBA8_sRGB
  , pattern MTLPixelFormatETC2_RGB8
  , pattern MTLPixelFormatETC2_RGB8_sRGB
  , pattern MTLPixelFormatETC2_RGB8A1
  , pattern MTLPixelFormatETC2_RGB8A1_sRGB
  , pattern MTLPixelFormatASTC_4x4_sRGB
  , pattern MTLPixelFormatASTC_5x4_sRGB
  , pattern MTLPixelFormatASTC_5x5_sRGB
  , pattern MTLPixelFormatASTC_6x5_sRGB
  , pattern MTLPixelFormatASTC_6x6_sRGB
  , pattern MTLPixelFormatASTC_8x5_sRGB
  , pattern MTLPixelFormatASTC_8x6_sRGB
  , pattern MTLPixelFormatASTC_8x8_sRGB
  , pattern MTLPixelFormatASTC_10x5_sRGB
  , pattern MTLPixelFormatASTC_10x6_sRGB
  , pattern MTLPixelFormatASTC_10x8_sRGB
  , pattern MTLPixelFormatASTC_10x10_sRGB
  , pattern MTLPixelFormatASTC_12x10_sRGB
  , pattern MTLPixelFormatASTC_12x12_sRGB
  , pattern MTLPixelFormatASTC_4x4_LDR
  , pattern MTLPixelFormatASTC_5x4_LDR
  , pattern MTLPixelFormatASTC_5x5_LDR
  , pattern MTLPixelFormatASTC_6x5_LDR
  , pattern MTLPixelFormatASTC_6x6_LDR
  , pattern MTLPixelFormatASTC_8x5_LDR
  , pattern MTLPixelFormatASTC_8x6_LDR
  , pattern MTLPixelFormatASTC_8x8_LDR
  , pattern MTLPixelFormatASTC_10x5_LDR
  , pattern MTLPixelFormatASTC_10x6_LDR
  , pattern MTLPixelFormatASTC_10x8_LDR
  , pattern MTLPixelFormatASTC_10x10_LDR
  , pattern MTLPixelFormatASTC_12x10_LDR
  , pattern MTLPixelFormatASTC_12x12_LDR
  , pattern MTLPixelFormatASTC_4x4_HDR
  , pattern MTLPixelFormatASTC_5x4_HDR
  , pattern MTLPixelFormatASTC_5x5_HDR
  , pattern MTLPixelFormatASTC_6x5_HDR
  , pattern MTLPixelFormatASTC_6x6_HDR
  , pattern MTLPixelFormatASTC_8x5_HDR
  , pattern MTLPixelFormatASTC_8x6_HDR
  , pattern MTLPixelFormatASTC_8x8_HDR
  , pattern MTLPixelFormatASTC_10x5_HDR
  , pattern MTLPixelFormatASTC_10x6_HDR
  , pattern MTLPixelFormatASTC_10x8_HDR
  , pattern MTLPixelFormatASTC_10x10_HDR
  , pattern MTLPixelFormatASTC_12x10_HDR
  , pattern MTLPixelFormatASTC_12x12_HDR
  , pattern MTLPixelFormatGBGR422
  , pattern MTLPixelFormatBGRG422
  , pattern MTLPixelFormatDepth16Unorm
  , pattern MTLPixelFormatDepth32Float
  , pattern MTLPixelFormatStencil8
  , pattern MTLPixelFormatDepth24Unorm_Stencil8
  , pattern MTLPixelFormatDepth32Float_Stencil8
  , pattern MTLPixelFormatX32_Stencil8
  , pattern MTLPixelFormatX24_Stencil8
  , pattern MTLPixelFormatUnspecialized
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
-- Restore all mesh pipeline descriptor properties to their default values.
--
-- ObjC selector: @- reset@
reset :: IsMTLMeshRenderPipelineDescriptor mtlMeshRenderPipelineDescriptor => mtlMeshRenderPipelineDescriptor -> IO ()
reset mtlMeshRenderPipelineDescriptor  =
    sendMsg mtlMeshRenderPipelineDescriptor (mkSelector "reset") retVoid []

-- | label
--
-- A name or description provided by the application that will be displayed in debugging tools. The default value is nil.
--
-- ObjC selector: @- label@
label :: IsMTLMeshRenderPipelineDescriptor mtlMeshRenderPipelineDescriptor => mtlMeshRenderPipelineDescriptor -> IO (Id NSString)
label mtlMeshRenderPipelineDescriptor  =
    sendMsg mtlMeshRenderPipelineDescriptor (mkSelector "label") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | label
--
-- A name or description provided by the application that will be displayed in debugging tools. The default value is nil.
--
-- ObjC selector: @- setLabel:@
setLabel :: (IsMTLMeshRenderPipelineDescriptor mtlMeshRenderPipelineDescriptor, IsNSString value) => mtlMeshRenderPipelineDescriptor -> value -> IO ()
setLabel mtlMeshRenderPipelineDescriptor  value =
  withObjCPtr value $ \raw_value ->
      sendMsg mtlMeshRenderPipelineDescriptor (mkSelector "setLabel:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- | objectFunction
--
-- Optional shader function responsible for determining how many threadgroups of the mesh shader to run, can optionally provide payload data for the mesh stage. If this is nil, no payload data is available to the mesh function, and the draw command determines how many threadgroups of the mesh stage to run. The default value is nil.
--
-- ObjC selector: @- objectFunction@
objectFunction :: IsMTLMeshRenderPipelineDescriptor mtlMeshRenderPipelineDescriptor => mtlMeshRenderPipelineDescriptor -> IO RawId
objectFunction mtlMeshRenderPipelineDescriptor  =
    fmap (RawId . castPtr) $ sendMsg mtlMeshRenderPipelineDescriptor (mkSelector "objectFunction") (retPtr retVoid) []

-- | objectFunction
--
-- Optional shader function responsible for determining how many threadgroups of the mesh shader to run, can optionally provide payload data for the mesh stage. If this is nil, no payload data is available to the mesh function, and the draw command determines how many threadgroups of the mesh stage to run. The default value is nil.
--
-- ObjC selector: @- setObjectFunction:@
setObjectFunction :: IsMTLMeshRenderPipelineDescriptor mtlMeshRenderPipelineDescriptor => mtlMeshRenderPipelineDescriptor -> RawId -> IO ()
setObjectFunction mtlMeshRenderPipelineDescriptor  value =
    sendMsg mtlMeshRenderPipelineDescriptor (mkSelector "setObjectFunction:") retVoid [argPtr (castPtr (unRawId value) :: Ptr ())]

-- | meshFunction
--
-- Shader function responsible for exporting a chunk of geometry per threadgroup for the rasterizer. The default value is nil.
--
-- ObjC selector: @- meshFunction@
meshFunction :: IsMTLMeshRenderPipelineDescriptor mtlMeshRenderPipelineDescriptor => mtlMeshRenderPipelineDescriptor -> IO RawId
meshFunction mtlMeshRenderPipelineDescriptor  =
    fmap (RawId . castPtr) $ sendMsg mtlMeshRenderPipelineDescriptor (mkSelector "meshFunction") (retPtr retVoid) []

-- | meshFunction
--
-- Shader function responsible for exporting a chunk of geometry per threadgroup for the rasterizer. The default value is nil.
--
-- ObjC selector: @- setMeshFunction:@
setMeshFunction :: IsMTLMeshRenderPipelineDescriptor mtlMeshRenderPipelineDescriptor => mtlMeshRenderPipelineDescriptor -> RawId -> IO ()
setMeshFunction mtlMeshRenderPipelineDescriptor  value =
    sendMsg mtlMeshRenderPipelineDescriptor (mkSelector "setMeshFunction:") retVoid [argPtr (castPtr (unRawId value) :: Ptr ())]

-- | fragmentFunction
--
-- Like a classical render pipeline, this fragments covered by the rasterized geometry are shaded with this function. The default value is nil. To create a pipeline, you must either set fragmentFunction to non-nil, or set rasterizationEnabled to NO.
--
-- ObjC selector: @- fragmentFunction@
fragmentFunction :: IsMTLMeshRenderPipelineDescriptor mtlMeshRenderPipelineDescriptor => mtlMeshRenderPipelineDescriptor -> IO RawId
fragmentFunction mtlMeshRenderPipelineDescriptor  =
    fmap (RawId . castPtr) $ sendMsg mtlMeshRenderPipelineDescriptor (mkSelector "fragmentFunction") (retPtr retVoid) []

-- | fragmentFunction
--
-- Like a classical render pipeline, this fragments covered by the rasterized geometry are shaded with this function. The default value is nil. To create a pipeline, you must either set fragmentFunction to non-nil, or set rasterizationEnabled to NO.
--
-- ObjC selector: @- setFragmentFunction:@
setFragmentFunction :: IsMTLMeshRenderPipelineDescriptor mtlMeshRenderPipelineDescriptor => mtlMeshRenderPipelineDescriptor -> RawId -> IO ()
setFragmentFunction mtlMeshRenderPipelineDescriptor  value =
    sendMsg mtlMeshRenderPipelineDescriptor (mkSelector "setFragmentFunction:") retVoid [argPtr (castPtr (unRawId value) :: Ptr ())]

-- | maxTotalThreadsPerObjectThreadgroup
--
-- The maximum size of the product of threadsPerObjectThreadgroup that can be used for draws with this pipeline. This information can be used by the optimizer to generate more efficient code, specifically when the specified value does not exceed the thread execution width of the underlying GPU. The default value is 0, which means that the value specified with the [[max_total_threads_per_threadgroup(N)]] specified on objectFunction will be used. When both the [[max_total_threads_per_threadgroup(N)]] attribute and a non-zero value are specified, both values must match. Any value specified cannot exceed the device limit as documented in the "Metal Feature Set Tables" for "Maximum threads per threadgroup".
--
-- ObjC selector: @- maxTotalThreadsPerObjectThreadgroup@
maxTotalThreadsPerObjectThreadgroup :: IsMTLMeshRenderPipelineDescriptor mtlMeshRenderPipelineDescriptor => mtlMeshRenderPipelineDescriptor -> IO CULong
maxTotalThreadsPerObjectThreadgroup mtlMeshRenderPipelineDescriptor  =
    sendMsg mtlMeshRenderPipelineDescriptor (mkSelector "maxTotalThreadsPerObjectThreadgroup") retCULong []

-- | maxTotalThreadsPerObjectThreadgroup
--
-- The maximum size of the product of threadsPerObjectThreadgroup that can be used for draws with this pipeline. This information can be used by the optimizer to generate more efficient code, specifically when the specified value does not exceed the thread execution width of the underlying GPU. The default value is 0, which means that the value specified with the [[max_total_threads_per_threadgroup(N)]] specified on objectFunction will be used. When both the [[max_total_threads_per_threadgroup(N)]] attribute and a non-zero value are specified, both values must match. Any value specified cannot exceed the device limit as documented in the "Metal Feature Set Tables" for "Maximum threads per threadgroup".
--
-- ObjC selector: @- setMaxTotalThreadsPerObjectThreadgroup:@
setMaxTotalThreadsPerObjectThreadgroup :: IsMTLMeshRenderPipelineDescriptor mtlMeshRenderPipelineDescriptor => mtlMeshRenderPipelineDescriptor -> CULong -> IO ()
setMaxTotalThreadsPerObjectThreadgroup mtlMeshRenderPipelineDescriptor  value =
    sendMsg mtlMeshRenderPipelineDescriptor (mkSelector "setMaxTotalThreadsPerObjectThreadgroup:") retVoid [argCULong value]

-- | maxTotalThreadsPerMeshThreadgroup
--
-- The maximum size of the product of threadsPerMeshThreadgroup that can be used for draws with this pipeline. This information can be used by the optimizer to generate more efficient code, specifically when the specified value does not exceed the thread execution width of the underlying GPU. The default value is 0, which means that the value specified with the [[max_total_threads_per_threadgroup(N)]] specified on meshFunction will be used. When both the [[max_total_threads_per_threadgroup(N)]] attribute and a non-zero value are specified, both values must match. Any value specified cannot exceed the device limit as documented in the "Metal Feature Set Tables" for "Maximum threads per threadgroup".
--
-- ObjC selector: @- maxTotalThreadsPerMeshThreadgroup@
maxTotalThreadsPerMeshThreadgroup :: IsMTLMeshRenderPipelineDescriptor mtlMeshRenderPipelineDescriptor => mtlMeshRenderPipelineDescriptor -> IO CULong
maxTotalThreadsPerMeshThreadgroup mtlMeshRenderPipelineDescriptor  =
    sendMsg mtlMeshRenderPipelineDescriptor (mkSelector "maxTotalThreadsPerMeshThreadgroup") retCULong []

-- | maxTotalThreadsPerMeshThreadgroup
--
-- The maximum size of the product of threadsPerMeshThreadgroup that can be used for draws with this pipeline. This information can be used by the optimizer to generate more efficient code, specifically when the specified value does not exceed the thread execution width of the underlying GPU. The default value is 0, which means that the value specified with the [[max_total_threads_per_threadgroup(N)]] specified on meshFunction will be used. When both the [[max_total_threads_per_threadgroup(N)]] attribute and a non-zero value are specified, both values must match. Any value specified cannot exceed the device limit as documented in the "Metal Feature Set Tables" for "Maximum threads per threadgroup".
--
-- ObjC selector: @- setMaxTotalThreadsPerMeshThreadgroup:@
setMaxTotalThreadsPerMeshThreadgroup :: IsMTLMeshRenderPipelineDescriptor mtlMeshRenderPipelineDescriptor => mtlMeshRenderPipelineDescriptor -> CULong -> IO ()
setMaxTotalThreadsPerMeshThreadgroup mtlMeshRenderPipelineDescriptor  value =
    sendMsg mtlMeshRenderPipelineDescriptor (mkSelector "setMaxTotalThreadsPerMeshThreadgroup:") retVoid [argCULong value]

-- | objectThreadgroupSizeIsMultipleOfThreadExecutionWidth
--
-- Set this value to YES when you will only use draws with the product of threadsPerObjectThreadgroup set to a multiple of the objectThreadExecutionWidth of the returned pipeline state. This information can be used by the optimizer to generate more efficient code. The default value is NO.
--
-- ObjC selector: @- objectThreadgroupSizeIsMultipleOfThreadExecutionWidth@
objectThreadgroupSizeIsMultipleOfThreadExecutionWidth :: IsMTLMeshRenderPipelineDescriptor mtlMeshRenderPipelineDescriptor => mtlMeshRenderPipelineDescriptor -> IO Bool
objectThreadgroupSizeIsMultipleOfThreadExecutionWidth mtlMeshRenderPipelineDescriptor  =
    fmap ((/= 0) :: CULong -> Bool) $ sendMsg mtlMeshRenderPipelineDescriptor (mkSelector "objectThreadgroupSizeIsMultipleOfThreadExecutionWidth") retCULong []

-- | objectThreadgroupSizeIsMultipleOfThreadExecutionWidth
--
-- Set this value to YES when you will only use draws with the product of threadsPerObjectThreadgroup set to a multiple of the objectThreadExecutionWidth of the returned pipeline state. This information can be used by the optimizer to generate more efficient code. The default value is NO.
--
-- ObjC selector: @- setObjectThreadgroupSizeIsMultipleOfThreadExecutionWidth:@
setObjectThreadgroupSizeIsMultipleOfThreadExecutionWidth :: IsMTLMeshRenderPipelineDescriptor mtlMeshRenderPipelineDescriptor => mtlMeshRenderPipelineDescriptor -> Bool -> IO ()
setObjectThreadgroupSizeIsMultipleOfThreadExecutionWidth mtlMeshRenderPipelineDescriptor  value =
    sendMsg mtlMeshRenderPipelineDescriptor (mkSelector "setObjectThreadgroupSizeIsMultipleOfThreadExecutionWidth:") retVoid [argCULong (if value then 1 else 0)]

-- | meshThreadgroupSizeIsMultipleOfThreadExecutionWidth
--
-- Set this value to YES when you will only use draws with the product of threadsPerMeshThreadgroup set to a multiple of the meshThreadExecutionWidth of the returned pipeline state. This information can be used by the optimizer to generate more efficient code. The default value is NO.
--
-- ObjC selector: @- meshThreadgroupSizeIsMultipleOfThreadExecutionWidth@
meshThreadgroupSizeIsMultipleOfThreadExecutionWidth :: IsMTLMeshRenderPipelineDescriptor mtlMeshRenderPipelineDescriptor => mtlMeshRenderPipelineDescriptor -> IO Bool
meshThreadgroupSizeIsMultipleOfThreadExecutionWidth mtlMeshRenderPipelineDescriptor  =
    fmap ((/= 0) :: CULong -> Bool) $ sendMsg mtlMeshRenderPipelineDescriptor (mkSelector "meshThreadgroupSizeIsMultipleOfThreadExecutionWidth") retCULong []

-- | meshThreadgroupSizeIsMultipleOfThreadExecutionWidth
--
-- Set this value to YES when you will only use draws with the product of threadsPerMeshThreadgroup set to a multiple of the meshThreadExecutionWidth of the returned pipeline state. This information can be used by the optimizer to generate more efficient code. The default value is NO.
--
-- ObjC selector: @- setMeshThreadgroupSizeIsMultipleOfThreadExecutionWidth:@
setMeshThreadgroupSizeIsMultipleOfThreadExecutionWidth :: IsMTLMeshRenderPipelineDescriptor mtlMeshRenderPipelineDescriptor => mtlMeshRenderPipelineDescriptor -> Bool -> IO ()
setMeshThreadgroupSizeIsMultipleOfThreadExecutionWidth mtlMeshRenderPipelineDescriptor  value =
    sendMsg mtlMeshRenderPipelineDescriptor (mkSelector "setMeshThreadgroupSizeIsMultipleOfThreadExecutionWidth:") retVoid [argCULong (if value then 1 else 0)]

-- | payloadMemoryLength
--
-- The size, in bytes, of the buffer indicated by [[payload]] in the object and mesh shader. If this value is 0, the size of the dereferenced type declared in the object shader for the buffer is used (space for a single element is assumed for pointers). The default value is 0.
--
-- ObjC selector: @- payloadMemoryLength@
payloadMemoryLength :: IsMTLMeshRenderPipelineDescriptor mtlMeshRenderPipelineDescriptor => mtlMeshRenderPipelineDescriptor -> IO CULong
payloadMemoryLength mtlMeshRenderPipelineDescriptor  =
    sendMsg mtlMeshRenderPipelineDescriptor (mkSelector "payloadMemoryLength") retCULong []

-- | payloadMemoryLength
--
-- The size, in bytes, of the buffer indicated by [[payload]] in the object and mesh shader. If this value is 0, the size of the dereferenced type declared in the object shader for the buffer is used (space for a single element is assumed for pointers). The default value is 0.
--
-- ObjC selector: @- setPayloadMemoryLength:@
setPayloadMemoryLength :: IsMTLMeshRenderPipelineDescriptor mtlMeshRenderPipelineDescriptor => mtlMeshRenderPipelineDescriptor -> CULong -> IO ()
setPayloadMemoryLength mtlMeshRenderPipelineDescriptor  value =
    sendMsg mtlMeshRenderPipelineDescriptor (mkSelector "setPayloadMemoryLength:") retVoid [argCULong value]

-- | maxTotalThreadgroupsPerMeshGrid
--
-- The maximum value of the product of vector elements that the object shader may pass to the mesh_grid_properties::set_threadgroups_per_grid built-in function. The default value is 0, which means that the value specified with the [[max_total_threadgroups_per_mesh_grid(N)]] specified on objectFunction will be used. When both the [[max_total_threadgroups_per_mesh_grid(N)]] attribute and a non-zero value are specified, both values must match. Any value specified cannot exceed the device limit as documented in the "Metal Feature Set Tables" for "Maximum threadgroups per mesh grid". Specifying this value is optional; it may be used to improve scheduling of the workload. If neither this value nor the shader attribute are used, the device's maximum supported value is used instead.
--
-- ObjC selector: @- maxTotalThreadgroupsPerMeshGrid@
maxTotalThreadgroupsPerMeshGrid :: IsMTLMeshRenderPipelineDescriptor mtlMeshRenderPipelineDescriptor => mtlMeshRenderPipelineDescriptor -> IO CULong
maxTotalThreadgroupsPerMeshGrid mtlMeshRenderPipelineDescriptor  =
    sendMsg mtlMeshRenderPipelineDescriptor (mkSelector "maxTotalThreadgroupsPerMeshGrid") retCULong []

-- | maxTotalThreadgroupsPerMeshGrid
--
-- The maximum value of the product of vector elements that the object shader may pass to the mesh_grid_properties::set_threadgroups_per_grid built-in function. The default value is 0, which means that the value specified with the [[max_total_threadgroups_per_mesh_grid(N)]] specified on objectFunction will be used. When both the [[max_total_threadgroups_per_mesh_grid(N)]] attribute and a non-zero value are specified, both values must match. Any value specified cannot exceed the device limit as documented in the "Metal Feature Set Tables" for "Maximum threadgroups per mesh grid". Specifying this value is optional; it may be used to improve scheduling of the workload. If neither this value nor the shader attribute are used, the device's maximum supported value is used instead.
--
-- ObjC selector: @- setMaxTotalThreadgroupsPerMeshGrid:@
setMaxTotalThreadgroupsPerMeshGrid :: IsMTLMeshRenderPipelineDescriptor mtlMeshRenderPipelineDescriptor => mtlMeshRenderPipelineDescriptor -> CULong -> IO ()
setMaxTotalThreadgroupsPerMeshGrid mtlMeshRenderPipelineDescriptor  value =
    sendMsg mtlMeshRenderPipelineDescriptor (mkSelector "setMaxTotalThreadgroupsPerMeshGrid:") retVoid [argCULong value]

-- | objectBuffers
--
-- Provide mutability information on the buffers used by objectFunction.
--
-- Specifying these values is optional; it may be used to optimize the shader code.
--
-- ObjC selector: @- objectBuffers@
objectBuffers :: IsMTLMeshRenderPipelineDescriptor mtlMeshRenderPipelineDescriptor => mtlMeshRenderPipelineDescriptor -> IO (Id MTLPipelineBufferDescriptorArray)
objectBuffers mtlMeshRenderPipelineDescriptor  =
    sendMsg mtlMeshRenderPipelineDescriptor (mkSelector "objectBuffers") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | meshBuffers
--
-- Provide mutability information on the buffers used by meshFunction.
--
-- Specifying these values is optional; it may be used to optimize the shader code.
--
-- ObjC selector: @- meshBuffers@
meshBuffers :: IsMTLMeshRenderPipelineDescriptor mtlMeshRenderPipelineDescriptor => mtlMeshRenderPipelineDescriptor -> IO (Id MTLPipelineBufferDescriptorArray)
meshBuffers mtlMeshRenderPipelineDescriptor  =
    sendMsg mtlMeshRenderPipelineDescriptor (mkSelector "meshBuffers") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | fragmentBuffers
--
-- Provide mutability information on the buffers used by fragmentFunction.
--
-- Specifying these values is optional; it may be used to optimize the shader code.
--
-- ObjC selector: @- fragmentBuffers@
fragmentBuffers :: IsMTLMeshRenderPipelineDescriptor mtlMeshRenderPipelineDescriptor => mtlMeshRenderPipelineDescriptor -> IO (Id MTLPipelineBufferDescriptorArray)
fragmentBuffers mtlMeshRenderPipelineDescriptor  =
    sendMsg mtlMeshRenderPipelineDescriptor (mkSelector "fragmentBuffers") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | rasterSampleCount
--
-- The number of samples per fragment of the render pass in which this pipeline will be used.
--
-- ObjC selector: @- rasterSampleCount@
rasterSampleCount :: IsMTLMeshRenderPipelineDescriptor mtlMeshRenderPipelineDescriptor => mtlMeshRenderPipelineDescriptor -> IO CULong
rasterSampleCount mtlMeshRenderPipelineDescriptor  =
    sendMsg mtlMeshRenderPipelineDescriptor (mkSelector "rasterSampleCount") retCULong []

-- | rasterSampleCount
--
-- The number of samples per fragment of the render pass in which this pipeline will be used.
--
-- ObjC selector: @- setRasterSampleCount:@
setRasterSampleCount :: IsMTLMeshRenderPipelineDescriptor mtlMeshRenderPipelineDescriptor => mtlMeshRenderPipelineDescriptor -> CULong -> IO ()
setRasterSampleCount mtlMeshRenderPipelineDescriptor  value =
    sendMsg mtlMeshRenderPipelineDescriptor (mkSelector "setRasterSampleCount:") retVoid [argCULong value]

-- | alphaToCoverageEnabled
--
-- Whether the alpha value exported by the fragment shader for the first color attachment is converted to a sample mask, which is subsequently AND-ed with the fragments' sample mask
--
-- The default value is NO.
--
-- ObjC selector: @- alphaToCoverageEnabled@
alphaToCoverageEnabled :: IsMTLMeshRenderPipelineDescriptor mtlMeshRenderPipelineDescriptor => mtlMeshRenderPipelineDescriptor -> IO Bool
alphaToCoverageEnabled mtlMeshRenderPipelineDescriptor  =
    fmap ((/= 0) :: CULong -> Bool) $ sendMsg mtlMeshRenderPipelineDescriptor (mkSelector "alphaToCoverageEnabled") retCULong []

-- | alphaToCoverageEnabled
--
-- Whether the alpha value exported by the fragment shader for the first color attachment is converted to a sample mask, which is subsequently AND-ed with the fragments' sample mask
--
-- The default value is NO.
--
-- ObjC selector: @- setAlphaToCoverageEnabled:@
setAlphaToCoverageEnabled :: IsMTLMeshRenderPipelineDescriptor mtlMeshRenderPipelineDescriptor => mtlMeshRenderPipelineDescriptor -> Bool -> IO ()
setAlphaToCoverageEnabled mtlMeshRenderPipelineDescriptor  value =
    sendMsg mtlMeshRenderPipelineDescriptor (mkSelector "setAlphaToCoverageEnabled:") retVoid [argCULong (if value then 1 else 0)]

-- | alphaToOneEnabled
--
-- Whether the alpha value exported by the fragment shader for all color attachments is modified to 1 (after evaluating alphaToCoverage).
--
-- The default value is NO.
--
-- ObjC selector: @- alphaToOneEnabled@
alphaToOneEnabled :: IsMTLMeshRenderPipelineDescriptor mtlMeshRenderPipelineDescriptor => mtlMeshRenderPipelineDescriptor -> IO Bool
alphaToOneEnabled mtlMeshRenderPipelineDescriptor  =
    fmap ((/= 0) :: CULong -> Bool) $ sendMsg mtlMeshRenderPipelineDescriptor (mkSelector "alphaToOneEnabled") retCULong []

-- | alphaToOneEnabled
--
-- Whether the alpha value exported by the fragment shader for all color attachments is modified to 1 (after evaluating alphaToCoverage).
--
-- The default value is NO.
--
-- ObjC selector: @- setAlphaToOneEnabled:@
setAlphaToOneEnabled :: IsMTLMeshRenderPipelineDescriptor mtlMeshRenderPipelineDescriptor => mtlMeshRenderPipelineDescriptor -> Bool -> IO ()
setAlphaToOneEnabled mtlMeshRenderPipelineDescriptor  value =
    sendMsg mtlMeshRenderPipelineDescriptor (mkSelector "setAlphaToOneEnabled:") retVoid [argCULong (if value then 1 else 0)]

-- | rasterizationEnabled
--
-- Whether rasterization is disabled, all primitives are dropped prior to rasterization.
--
-- The default value is YES.
--
-- ObjC selector: @- rasterizationEnabled@
rasterizationEnabled :: IsMTLMeshRenderPipelineDescriptor mtlMeshRenderPipelineDescriptor => mtlMeshRenderPipelineDescriptor -> IO Bool
rasterizationEnabled mtlMeshRenderPipelineDescriptor  =
    fmap ((/= 0) :: CULong -> Bool) $ sendMsg mtlMeshRenderPipelineDescriptor (mkSelector "rasterizationEnabled") retCULong []

-- | rasterizationEnabled
--
-- Whether rasterization is disabled, all primitives are dropped prior to rasterization.
--
-- The default value is YES.
--
-- ObjC selector: @- setRasterizationEnabled:@
setRasterizationEnabled :: IsMTLMeshRenderPipelineDescriptor mtlMeshRenderPipelineDescriptor => mtlMeshRenderPipelineDescriptor -> Bool -> IO ()
setRasterizationEnabled mtlMeshRenderPipelineDescriptor  value =
    sendMsg mtlMeshRenderPipelineDescriptor (mkSelector "setRasterizationEnabled:") retVoid [argCULong (if value then 1 else 0)]

-- | maxVertexAmplificationCount
--
-- The maximum value that can be passed to setVertexAmplificationCount when using this pipeline.
--
-- The default value is 1. The value must be supported by the device, which can be checked with supportsVertexAmplificationCount.
--
-- ObjC selector: @- maxVertexAmplificationCount@
maxVertexAmplificationCount :: IsMTLMeshRenderPipelineDescriptor mtlMeshRenderPipelineDescriptor => mtlMeshRenderPipelineDescriptor -> IO CULong
maxVertexAmplificationCount mtlMeshRenderPipelineDescriptor  =
    sendMsg mtlMeshRenderPipelineDescriptor (mkSelector "maxVertexAmplificationCount") retCULong []

-- | maxVertexAmplificationCount
--
-- The maximum value that can be passed to setVertexAmplificationCount when using this pipeline.
--
-- The default value is 1. The value must be supported by the device, which can be checked with supportsVertexAmplificationCount.
--
-- ObjC selector: @- setMaxVertexAmplificationCount:@
setMaxVertexAmplificationCount :: IsMTLMeshRenderPipelineDescriptor mtlMeshRenderPipelineDescriptor => mtlMeshRenderPipelineDescriptor -> CULong -> IO ()
setMaxVertexAmplificationCount mtlMeshRenderPipelineDescriptor  value =
    sendMsg mtlMeshRenderPipelineDescriptor (mkSelector "setMaxVertexAmplificationCount:") retVoid [argCULong value]

-- | colorAttachments
--
-- Describes the color attachments of the render pass in which this pipeline will be used.
--
-- ObjC selector: @- colorAttachments@
colorAttachments :: IsMTLMeshRenderPipelineDescriptor mtlMeshRenderPipelineDescriptor => mtlMeshRenderPipelineDescriptor -> IO (Id MTLRenderPipelineColorAttachmentDescriptorArray)
colorAttachments mtlMeshRenderPipelineDescriptor  =
    sendMsg mtlMeshRenderPipelineDescriptor (mkSelector "colorAttachments") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | depthAttachmentPixelFormat
--
-- The pixel format of the depth attachment of the render pass in which this pipeline will be used.
--
-- The default value is MTLPixelFormatInvalid; indicating no depth attachment will be used.
--
-- ObjC selector: @- depthAttachmentPixelFormat@
depthAttachmentPixelFormat :: IsMTLMeshRenderPipelineDescriptor mtlMeshRenderPipelineDescriptor => mtlMeshRenderPipelineDescriptor -> IO MTLPixelFormat
depthAttachmentPixelFormat mtlMeshRenderPipelineDescriptor  =
    fmap (coerce :: CULong -> MTLPixelFormat) $ sendMsg mtlMeshRenderPipelineDescriptor (mkSelector "depthAttachmentPixelFormat") retCULong []

-- | depthAttachmentPixelFormat
--
-- The pixel format of the depth attachment of the render pass in which this pipeline will be used.
--
-- The default value is MTLPixelFormatInvalid; indicating no depth attachment will be used.
--
-- ObjC selector: @- setDepthAttachmentPixelFormat:@
setDepthAttachmentPixelFormat :: IsMTLMeshRenderPipelineDescriptor mtlMeshRenderPipelineDescriptor => mtlMeshRenderPipelineDescriptor -> MTLPixelFormat -> IO ()
setDepthAttachmentPixelFormat mtlMeshRenderPipelineDescriptor  value =
    sendMsg mtlMeshRenderPipelineDescriptor (mkSelector "setDepthAttachmentPixelFormat:") retVoid [argCULong (coerce value)]

-- | stencilAttachmentPixelFormat
--
-- The pixel format of the stencil attachment of the render pass in which this pipeline will be used.
--
-- The default value is MTLPixelFormatInvalid; indicating no stencil attachment will be used.
--
-- ObjC selector: @- stencilAttachmentPixelFormat@
stencilAttachmentPixelFormat :: IsMTLMeshRenderPipelineDescriptor mtlMeshRenderPipelineDescriptor => mtlMeshRenderPipelineDescriptor -> IO MTLPixelFormat
stencilAttachmentPixelFormat mtlMeshRenderPipelineDescriptor  =
    fmap (coerce :: CULong -> MTLPixelFormat) $ sendMsg mtlMeshRenderPipelineDescriptor (mkSelector "stencilAttachmentPixelFormat") retCULong []

-- | stencilAttachmentPixelFormat
--
-- The pixel format of the stencil attachment of the render pass in which this pipeline will be used.
--
-- The default value is MTLPixelFormatInvalid; indicating no stencil attachment will be used.
--
-- ObjC selector: @- setStencilAttachmentPixelFormat:@
setStencilAttachmentPixelFormat :: IsMTLMeshRenderPipelineDescriptor mtlMeshRenderPipelineDescriptor => mtlMeshRenderPipelineDescriptor -> MTLPixelFormat -> IO ()
setStencilAttachmentPixelFormat mtlMeshRenderPipelineDescriptor  value =
    sendMsg mtlMeshRenderPipelineDescriptor (mkSelector "setStencilAttachmentPixelFormat:") retVoid [argCULong (coerce value)]

-- | supportIndirectCommandBuffers
--
-- Whether this pipeline will support being used by commands in an indirect command buffer.
--
-- The default value is NO.
--
-- ObjC selector: @- supportIndirectCommandBuffers@
supportIndirectCommandBuffers :: IsMTLMeshRenderPipelineDescriptor mtlMeshRenderPipelineDescriptor => mtlMeshRenderPipelineDescriptor -> IO Bool
supportIndirectCommandBuffers mtlMeshRenderPipelineDescriptor  =
    fmap ((/= 0) :: CULong -> Bool) $ sendMsg mtlMeshRenderPipelineDescriptor (mkSelector "supportIndirectCommandBuffers") retCULong []

-- | supportIndirectCommandBuffers
--
-- Whether this pipeline will support being used by commands in an indirect command buffer.
--
-- The default value is NO.
--
-- ObjC selector: @- setSupportIndirectCommandBuffers:@
setSupportIndirectCommandBuffers :: IsMTLMeshRenderPipelineDescriptor mtlMeshRenderPipelineDescriptor => mtlMeshRenderPipelineDescriptor -> Bool -> IO ()
setSupportIndirectCommandBuffers mtlMeshRenderPipelineDescriptor  value =
    sendMsg mtlMeshRenderPipelineDescriptor (mkSelector "setSupportIndirectCommandBuffers:") retVoid [argCULong (if value then 1 else 0)]

-- | binaryArchives
--
-- The set of MTLBinaryArchive to search for compiled code when creating the pipeline state.
--
-- Accelerate pipeline state creation by providing archives of compiled code such that no compilation needs to happen on the fast path.
--
-- See: MTLBinaryArchive
--
-- ObjC selector: @- binaryArchives@
binaryArchives :: IsMTLMeshRenderPipelineDescriptor mtlMeshRenderPipelineDescriptor => mtlMeshRenderPipelineDescriptor -> IO (Id NSArray)
binaryArchives mtlMeshRenderPipelineDescriptor  =
    sendMsg mtlMeshRenderPipelineDescriptor (mkSelector "binaryArchives") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | binaryArchives
--
-- The set of MTLBinaryArchive to search for compiled code when creating the pipeline state.
--
-- Accelerate pipeline state creation by providing archives of compiled code such that no compilation needs to happen on the fast path.
--
-- See: MTLBinaryArchive
--
-- ObjC selector: @- setBinaryArchives:@
setBinaryArchives :: (IsMTLMeshRenderPipelineDescriptor mtlMeshRenderPipelineDescriptor, IsNSArray value) => mtlMeshRenderPipelineDescriptor -> value -> IO ()
setBinaryArchives mtlMeshRenderPipelineDescriptor  value =
  withObjCPtr value $ \raw_value ->
      sendMsg mtlMeshRenderPipelineDescriptor (mkSelector "setBinaryArchives:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- | objectLinkedFunctions
--
-- The set of functions to be linked with the pipeline state and accessed from the object function.
--
-- See: MTLLinkedFunctions
--
-- ObjC selector: @- objectLinkedFunctions@
objectLinkedFunctions :: IsMTLMeshRenderPipelineDescriptor mtlMeshRenderPipelineDescriptor => mtlMeshRenderPipelineDescriptor -> IO (Id MTLLinkedFunctions)
objectLinkedFunctions mtlMeshRenderPipelineDescriptor  =
    sendMsg mtlMeshRenderPipelineDescriptor (mkSelector "objectLinkedFunctions") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | objectLinkedFunctions
--
-- The set of functions to be linked with the pipeline state and accessed from the object function.
--
-- See: MTLLinkedFunctions
--
-- ObjC selector: @- setObjectLinkedFunctions:@
setObjectLinkedFunctions :: (IsMTLMeshRenderPipelineDescriptor mtlMeshRenderPipelineDescriptor, IsMTLLinkedFunctions value) => mtlMeshRenderPipelineDescriptor -> value -> IO ()
setObjectLinkedFunctions mtlMeshRenderPipelineDescriptor  value =
  withObjCPtr value $ \raw_value ->
      sendMsg mtlMeshRenderPipelineDescriptor (mkSelector "setObjectLinkedFunctions:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- | meshLinkedFunctions
--
-- The set of functions to be linked with the pipeline state and accessed from the mesh function.
--
-- See: MTLLinkedFunctions
--
-- ObjC selector: @- meshLinkedFunctions@
meshLinkedFunctions :: IsMTLMeshRenderPipelineDescriptor mtlMeshRenderPipelineDescriptor => mtlMeshRenderPipelineDescriptor -> IO (Id MTLLinkedFunctions)
meshLinkedFunctions mtlMeshRenderPipelineDescriptor  =
    sendMsg mtlMeshRenderPipelineDescriptor (mkSelector "meshLinkedFunctions") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | meshLinkedFunctions
--
-- The set of functions to be linked with the pipeline state and accessed from the mesh function.
--
-- See: MTLLinkedFunctions
--
-- ObjC selector: @- setMeshLinkedFunctions:@
setMeshLinkedFunctions :: (IsMTLMeshRenderPipelineDescriptor mtlMeshRenderPipelineDescriptor, IsMTLLinkedFunctions value) => mtlMeshRenderPipelineDescriptor -> value -> IO ()
setMeshLinkedFunctions mtlMeshRenderPipelineDescriptor  value =
  withObjCPtr value $ \raw_value ->
      sendMsg mtlMeshRenderPipelineDescriptor (mkSelector "setMeshLinkedFunctions:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- | fragmentLinkedFunctions
--
-- The set of functions to be linked with the pipeline state and accessed from the fragment function.
--
-- See: MTLLinkedFunctions
--
-- ObjC selector: @- fragmentLinkedFunctions@
fragmentLinkedFunctions :: IsMTLMeshRenderPipelineDescriptor mtlMeshRenderPipelineDescriptor => mtlMeshRenderPipelineDescriptor -> IO (Id MTLLinkedFunctions)
fragmentLinkedFunctions mtlMeshRenderPipelineDescriptor  =
    sendMsg mtlMeshRenderPipelineDescriptor (mkSelector "fragmentLinkedFunctions") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | fragmentLinkedFunctions
--
-- The set of functions to be linked with the pipeline state and accessed from the fragment function.
--
-- See: MTLLinkedFunctions
--
-- ObjC selector: @- setFragmentLinkedFunctions:@
setFragmentLinkedFunctions :: (IsMTLMeshRenderPipelineDescriptor mtlMeshRenderPipelineDescriptor, IsMTLLinkedFunctions value) => mtlMeshRenderPipelineDescriptor -> value -> IO ()
setFragmentLinkedFunctions mtlMeshRenderPipelineDescriptor  value =
  withObjCPtr value $ \raw_value ->
      sendMsg mtlMeshRenderPipelineDescriptor (mkSelector "setFragmentLinkedFunctions:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- | shaderValidation
--
-- Toggle that determines whether Metal Shader Validation should be enabled or disabled for the pipeline.
--
-- The value can be overridden using @MTL_SHADER_VALIDATION_ENABLE_PIPELINES@ or @MTL_SHADER_VALIDATION_DISABLE_PIPELINES@ Environment Variables.
--
-- ObjC selector: @- shaderValidation@
shaderValidation :: IsMTLMeshRenderPipelineDescriptor mtlMeshRenderPipelineDescriptor => mtlMeshRenderPipelineDescriptor -> IO MTLShaderValidation
shaderValidation mtlMeshRenderPipelineDescriptor  =
    fmap (coerce :: CLong -> MTLShaderValidation) $ sendMsg mtlMeshRenderPipelineDescriptor (mkSelector "shaderValidation") retCLong []

-- | shaderValidation
--
-- Toggle that determines whether Metal Shader Validation should be enabled or disabled for the pipeline.
--
-- The value can be overridden using @MTL_SHADER_VALIDATION_ENABLE_PIPELINES@ or @MTL_SHADER_VALIDATION_DISABLE_PIPELINES@ Environment Variables.
--
-- ObjC selector: @- setShaderValidation:@
setShaderValidation :: IsMTLMeshRenderPipelineDescriptor mtlMeshRenderPipelineDescriptor => mtlMeshRenderPipelineDescriptor -> MTLShaderValidation -> IO ()
setShaderValidation mtlMeshRenderPipelineDescriptor  value =
    sendMsg mtlMeshRenderPipelineDescriptor (mkSelector "setShaderValidation:") retVoid [argCLong (coerce value)]

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

-- | @Selector@ for @objectFunction@
objectFunctionSelector :: Selector
objectFunctionSelector = mkSelector "objectFunction"

-- | @Selector@ for @setObjectFunction:@
setObjectFunctionSelector :: Selector
setObjectFunctionSelector = mkSelector "setObjectFunction:"

-- | @Selector@ for @meshFunction@
meshFunctionSelector :: Selector
meshFunctionSelector = mkSelector "meshFunction"

-- | @Selector@ for @setMeshFunction:@
setMeshFunctionSelector :: Selector
setMeshFunctionSelector = mkSelector "setMeshFunction:"

-- | @Selector@ for @fragmentFunction@
fragmentFunctionSelector :: Selector
fragmentFunctionSelector = mkSelector "fragmentFunction"

-- | @Selector@ for @setFragmentFunction:@
setFragmentFunctionSelector :: Selector
setFragmentFunctionSelector = mkSelector "setFragmentFunction:"

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

-- | @Selector@ for @objectBuffers@
objectBuffersSelector :: Selector
objectBuffersSelector = mkSelector "objectBuffers"

-- | @Selector@ for @meshBuffers@
meshBuffersSelector :: Selector
meshBuffersSelector = mkSelector "meshBuffers"

-- | @Selector@ for @fragmentBuffers@
fragmentBuffersSelector :: Selector
fragmentBuffersSelector = mkSelector "fragmentBuffers"

-- | @Selector@ for @rasterSampleCount@
rasterSampleCountSelector :: Selector
rasterSampleCountSelector = mkSelector "rasterSampleCount"

-- | @Selector@ for @setRasterSampleCount:@
setRasterSampleCountSelector :: Selector
setRasterSampleCountSelector = mkSelector "setRasterSampleCount:"

-- | @Selector@ for @alphaToCoverageEnabled@
alphaToCoverageEnabledSelector :: Selector
alphaToCoverageEnabledSelector = mkSelector "alphaToCoverageEnabled"

-- | @Selector@ for @setAlphaToCoverageEnabled:@
setAlphaToCoverageEnabledSelector :: Selector
setAlphaToCoverageEnabledSelector = mkSelector "setAlphaToCoverageEnabled:"

-- | @Selector@ for @alphaToOneEnabled@
alphaToOneEnabledSelector :: Selector
alphaToOneEnabledSelector = mkSelector "alphaToOneEnabled"

-- | @Selector@ for @setAlphaToOneEnabled:@
setAlphaToOneEnabledSelector :: Selector
setAlphaToOneEnabledSelector = mkSelector "setAlphaToOneEnabled:"

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

-- | @Selector@ for @depthAttachmentPixelFormat@
depthAttachmentPixelFormatSelector :: Selector
depthAttachmentPixelFormatSelector = mkSelector "depthAttachmentPixelFormat"

-- | @Selector@ for @setDepthAttachmentPixelFormat:@
setDepthAttachmentPixelFormatSelector :: Selector
setDepthAttachmentPixelFormatSelector = mkSelector "setDepthAttachmentPixelFormat:"

-- | @Selector@ for @stencilAttachmentPixelFormat@
stencilAttachmentPixelFormatSelector :: Selector
stencilAttachmentPixelFormatSelector = mkSelector "stencilAttachmentPixelFormat"

-- | @Selector@ for @setStencilAttachmentPixelFormat:@
setStencilAttachmentPixelFormatSelector :: Selector
setStencilAttachmentPixelFormatSelector = mkSelector "setStencilAttachmentPixelFormat:"

-- | @Selector@ for @supportIndirectCommandBuffers@
supportIndirectCommandBuffersSelector :: Selector
supportIndirectCommandBuffersSelector = mkSelector "supportIndirectCommandBuffers"

-- | @Selector@ for @setSupportIndirectCommandBuffers:@
setSupportIndirectCommandBuffersSelector :: Selector
setSupportIndirectCommandBuffersSelector = mkSelector "setSupportIndirectCommandBuffers:"

-- | @Selector@ for @binaryArchives@
binaryArchivesSelector :: Selector
binaryArchivesSelector = mkSelector "binaryArchives"

-- | @Selector@ for @setBinaryArchives:@
setBinaryArchivesSelector :: Selector
setBinaryArchivesSelector = mkSelector "setBinaryArchives:"

-- | @Selector@ for @objectLinkedFunctions@
objectLinkedFunctionsSelector :: Selector
objectLinkedFunctionsSelector = mkSelector "objectLinkedFunctions"

-- | @Selector@ for @setObjectLinkedFunctions:@
setObjectLinkedFunctionsSelector :: Selector
setObjectLinkedFunctionsSelector = mkSelector "setObjectLinkedFunctions:"

-- | @Selector@ for @meshLinkedFunctions@
meshLinkedFunctionsSelector :: Selector
meshLinkedFunctionsSelector = mkSelector "meshLinkedFunctions"

-- | @Selector@ for @setMeshLinkedFunctions:@
setMeshLinkedFunctionsSelector :: Selector
setMeshLinkedFunctionsSelector = mkSelector "setMeshLinkedFunctions:"

-- | @Selector@ for @fragmentLinkedFunctions@
fragmentLinkedFunctionsSelector :: Selector
fragmentLinkedFunctionsSelector = mkSelector "fragmentLinkedFunctions"

-- | @Selector@ for @setFragmentLinkedFunctions:@
setFragmentLinkedFunctionsSelector :: Selector
setFragmentLinkedFunctionsSelector = mkSelector "setFragmentLinkedFunctions:"

-- | @Selector@ for @shaderValidation@
shaderValidationSelector :: Selector
shaderValidationSelector = mkSelector "shaderValidation"

-- | @Selector@ for @setShaderValidation:@
setShaderValidationSelector :: Selector
setShaderValidationSelector = mkSelector "setShaderValidation:"

