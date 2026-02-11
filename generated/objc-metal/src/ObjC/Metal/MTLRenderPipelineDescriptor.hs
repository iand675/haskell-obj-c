{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Generated bindings for @MTLRenderPipelineDescriptor@.
module ObjC.Metal.MTLRenderPipelineDescriptor
  ( MTLRenderPipelineDescriptor
  , IsMTLRenderPipelineDescriptor(..)
  , reset
  , label
  , setLabel
  , vertexDescriptor
  , setVertexDescriptor
  , sampleCount
  , setSampleCount
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
  , inputPrimitiveTopology
  , setInputPrimitiveTopology
  , tessellationPartitionMode
  , setTessellationPartitionMode
  , maxTessellationFactor
  , setMaxTessellationFactor
  , tessellationFactorScaleEnabled
  , setTessellationFactorScaleEnabled
  , tessellationFactorFormat
  , setTessellationFactorFormat
  , tessellationControlPointIndexType
  , setTessellationControlPointIndexType
  , tessellationFactorStepFunction
  , setTessellationFactorStepFunction
  , tessellationOutputWindingOrder
  , setTessellationOutputWindingOrder
  , vertexBuffers
  , fragmentBuffers
  , supportIndirectCommandBuffers
  , setSupportIndirectCommandBuffers
  , vertexLinkedFunctions
  , setVertexLinkedFunctions
  , fragmentLinkedFunctions
  , setFragmentLinkedFunctions
  , supportAddingVertexBinaryFunctions
  , setSupportAddingVertexBinaryFunctions
  , supportAddingFragmentBinaryFunctions
  , setSupportAddingFragmentBinaryFunctions
  , maxVertexCallStackDepth
  , setMaxVertexCallStackDepth
  , maxFragmentCallStackDepth
  , setMaxFragmentCallStackDepth
  , shaderValidation
  , setShaderValidation
  , resetSelector
  , labelSelector
  , setLabelSelector
  , vertexDescriptorSelector
  , setVertexDescriptorSelector
  , sampleCountSelector
  , setSampleCountSelector
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
  , inputPrimitiveTopologySelector
  , setInputPrimitiveTopologySelector
  , tessellationPartitionModeSelector
  , setTessellationPartitionModeSelector
  , maxTessellationFactorSelector
  , setMaxTessellationFactorSelector
  , tessellationFactorScaleEnabledSelector
  , setTessellationFactorScaleEnabledSelector
  , tessellationFactorFormatSelector
  , setTessellationFactorFormatSelector
  , tessellationControlPointIndexTypeSelector
  , setTessellationControlPointIndexTypeSelector
  , tessellationFactorStepFunctionSelector
  , setTessellationFactorStepFunctionSelector
  , tessellationOutputWindingOrderSelector
  , setTessellationOutputWindingOrderSelector
  , vertexBuffersSelector
  , fragmentBuffersSelector
  , supportIndirectCommandBuffersSelector
  , setSupportIndirectCommandBuffersSelector
  , vertexLinkedFunctionsSelector
  , setVertexLinkedFunctionsSelector
  , fragmentLinkedFunctionsSelector
  , setFragmentLinkedFunctionsSelector
  , supportAddingVertexBinaryFunctionsSelector
  , setSupportAddingVertexBinaryFunctionsSelector
  , supportAddingFragmentBinaryFunctionsSelector
  , setSupportAddingFragmentBinaryFunctionsSelector
  , maxVertexCallStackDepthSelector
  , setMaxVertexCallStackDepthSelector
  , maxFragmentCallStackDepthSelector
  , setMaxFragmentCallStackDepthSelector
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
  , MTLPrimitiveTopologyClass(MTLPrimitiveTopologyClass)
  , pattern MTLPrimitiveTopologyClassUnspecified
  , pattern MTLPrimitiveTopologyClassPoint
  , pattern MTLPrimitiveTopologyClassLine
  , pattern MTLPrimitiveTopologyClassTriangle
  , MTLShaderValidation(MTLShaderValidation)
  , pattern MTLShaderValidationDefault
  , pattern MTLShaderValidationEnabled
  , pattern MTLShaderValidationDisabled
  , MTLTessellationControlPointIndexType(MTLTessellationControlPointIndexType)
  , pattern MTLTessellationControlPointIndexTypeNone
  , pattern MTLTessellationControlPointIndexTypeUInt16
  , pattern MTLTessellationControlPointIndexTypeUInt32
  , MTLTessellationFactorFormat(MTLTessellationFactorFormat)
  , pattern MTLTessellationFactorFormatHalf
  , MTLTessellationFactorStepFunction(MTLTessellationFactorStepFunction)
  , pattern MTLTessellationFactorStepFunctionConstant
  , pattern MTLTessellationFactorStepFunctionPerPatch
  , pattern MTLTessellationFactorStepFunctionPerInstance
  , pattern MTLTessellationFactorStepFunctionPerPatchAndPerInstance
  , MTLTessellationPartitionMode(MTLTessellationPartitionMode)
  , pattern MTLTessellationPartitionModePow2
  , pattern MTLTessellationPartitionModeInteger
  , pattern MTLTessellationPartitionModeFractionalOdd
  , pattern MTLTessellationPartitionModeFractionalEven
  , MTLWinding(MTLWinding)
  , pattern MTLWindingClockwise
  , pattern MTLWindingCounterClockwise

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
-- Restore all pipeline descriptor properties to their default values.
--
-- ObjC selector: @- reset@
reset :: IsMTLRenderPipelineDescriptor mtlRenderPipelineDescriptor => mtlRenderPipelineDescriptor -> IO ()
reset mtlRenderPipelineDescriptor  =
  sendMsg mtlRenderPipelineDescriptor (mkSelector "reset") retVoid []

-- | @- label@
label :: IsMTLRenderPipelineDescriptor mtlRenderPipelineDescriptor => mtlRenderPipelineDescriptor -> IO (Id NSString)
label mtlRenderPipelineDescriptor  =
  sendMsg mtlRenderPipelineDescriptor (mkSelector "label") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- setLabel:@
setLabel :: (IsMTLRenderPipelineDescriptor mtlRenderPipelineDescriptor, IsNSString value) => mtlRenderPipelineDescriptor -> value -> IO ()
setLabel mtlRenderPipelineDescriptor  value =
withObjCPtr value $ \raw_value ->
    sendMsg mtlRenderPipelineDescriptor (mkSelector "setLabel:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- | @- vertexDescriptor@
vertexDescriptor :: IsMTLRenderPipelineDescriptor mtlRenderPipelineDescriptor => mtlRenderPipelineDescriptor -> IO (Id MTLVertexDescriptor)
vertexDescriptor mtlRenderPipelineDescriptor  =
  sendMsg mtlRenderPipelineDescriptor (mkSelector "vertexDescriptor") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- setVertexDescriptor:@
setVertexDescriptor :: (IsMTLRenderPipelineDescriptor mtlRenderPipelineDescriptor, IsMTLVertexDescriptor value) => mtlRenderPipelineDescriptor -> value -> IO ()
setVertexDescriptor mtlRenderPipelineDescriptor  value =
withObjCPtr value $ \raw_value ->
    sendMsg mtlRenderPipelineDescriptor (mkSelector "setVertexDescriptor:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- | @- sampleCount@
sampleCount :: IsMTLRenderPipelineDescriptor mtlRenderPipelineDescriptor => mtlRenderPipelineDescriptor -> IO CULong
sampleCount mtlRenderPipelineDescriptor  =
  sendMsg mtlRenderPipelineDescriptor (mkSelector "sampleCount") retCULong []

-- | @- setSampleCount:@
setSampleCount :: IsMTLRenderPipelineDescriptor mtlRenderPipelineDescriptor => mtlRenderPipelineDescriptor -> CULong -> IO ()
setSampleCount mtlRenderPipelineDescriptor  value =
  sendMsg mtlRenderPipelineDescriptor (mkSelector "setSampleCount:") retVoid [argCULong (fromIntegral value)]

-- | @- rasterSampleCount@
rasterSampleCount :: IsMTLRenderPipelineDescriptor mtlRenderPipelineDescriptor => mtlRenderPipelineDescriptor -> IO CULong
rasterSampleCount mtlRenderPipelineDescriptor  =
  sendMsg mtlRenderPipelineDescriptor (mkSelector "rasterSampleCount") retCULong []

-- | @- setRasterSampleCount:@
setRasterSampleCount :: IsMTLRenderPipelineDescriptor mtlRenderPipelineDescriptor => mtlRenderPipelineDescriptor -> CULong -> IO ()
setRasterSampleCount mtlRenderPipelineDescriptor  value =
  sendMsg mtlRenderPipelineDescriptor (mkSelector "setRasterSampleCount:") retVoid [argCULong (fromIntegral value)]

-- | @- alphaToCoverageEnabled@
alphaToCoverageEnabled :: IsMTLRenderPipelineDescriptor mtlRenderPipelineDescriptor => mtlRenderPipelineDescriptor -> IO Bool
alphaToCoverageEnabled mtlRenderPipelineDescriptor  =
  fmap ((/= 0) :: CULong -> Bool) $ sendMsg mtlRenderPipelineDescriptor (mkSelector "alphaToCoverageEnabled") retCULong []

-- | @- setAlphaToCoverageEnabled:@
setAlphaToCoverageEnabled :: IsMTLRenderPipelineDescriptor mtlRenderPipelineDescriptor => mtlRenderPipelineDescriptor -> Bool -> IO ()
setAlphaToCoverageEnabled mtlRenderPipelineDescriptor  value =
  sendMsg mtlRenderPipelineDescriptor (mkSelector "setAlphaToCoverageEnabled:") retVoid [argCULong (if value then 1 else 0)]

-- | @- alphaToOneEnabled@
alphaToOneEnabled :: IsMTLRenderPipelineDescriptor mtlRenderPipelineDescriptor => mtlRenderPipelineDescriptor -> IO Bool
alphaToOneEnabled mtlRenderPipelineDescriptor  =
  fmap ((/= 0) :: CULong -> Bool) $ sendMsg mtlRenderPipelineDescriptor (mkSelector "alphaToOneEnabled") retCULong []

-- | @- setAlphaToOneEnabled:@
setAlphaToOneEnabled :: IsMTLRenderPipelineDescriptor mtlRenderPipelineDescriptor => mtlRenderPipelineDescriptor -> Bool -> IO ()
setAlphaToOneEnabled mtlRenderPipelineDescriptor  value =
  sendMsg mtlRenderPipelineDescriptor (mkSelector "setAlphaToOneEnabled:") retVoid [argCULong (if value then 1 else 0)]

-- | @- rasterizationEnabled@
rasterizationEnabled :: IsMTLRenderPipelineDescriptor mtlRenderPipelineDescriptor => mtlRenderPipelineDescriptor -> IO Bool
rasterizationEnabled mtlRenderPipelineDescriptor  =
  fmap ((/= 0) :: CULong -> Bool) $ sendMsg mtlRenderPipelineDescriptor (mkSelector "rasterizationEnabled") retCULong []

-- | @- setRasterizationEnabled:@
setRasterizationEnabled :: IsMTLRenderPipelineDescriptor mtlRenderPipelineDescriptor => mtlRenderPipelineDescriptor -> Bool -> IO ()
setRasterizationEnabled mtlRenderPipelineDescriptor  value =
  sendMsg mtlRenderPipelineDescriptor (mkSelector "setRasterizationEnabled:") retVoid [argCULong (if value then 1 else 0)]

-- | @- maxVertexAmplificationCount@
maxVertexAmplificationCount :: IsMTLRenderPipelineDescriptor mtlRenderPipelineDescriptor => mtlRenderPipelineDescriptor -> IO CULong
maxVertexAmplificationCount mtlRenderPipelineDescriptor  =
  sendMsg mtlRenderPipelineDescriptor (mkSelector "maxVertexAmplificationCount") retCULong []

-- | @- setMaxVertexAmplificationCount:@
setMaxVertexAmplificationCount :: IsMTLRenderPipelineDescriptor mtlRenderPipelineDescriptor => mtlRenderPipelineDescriptor -> CULong -> IO ()
setMaxVertexAmplificationCount mtlRenderPipelineDescriptor  value =
  sendMsg mtlRenderPipelineDescriptor (mkSelector "setMaxVertexAmplificationCount:") retVoid [argCULong (fromIntegral value)]

-- | @- colorAttachments@
colorAttachments :: IsMTLRenderPipelineDescriptor mtlRenderPipelineDescriptor => mtlRenderPipelineDescriptor -> IO (Id MTLRenderPipelineColorAttachmentDescriptorArray)
colorAttachments mtlRenderPipelineDescriptor  =
  sendMsg mtlRenderPipelineDescriptor (mkSelector "colorAttachments") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- depthAttachmentPixelFormat@
depthAttachmentPixelFormat :: IsMTLRenderPipelineDescriptor mtlRenderPipelineDescriptor => mtlRenderPipelineDescriptor -> IO MTLPixelFormat
depthAttachmentPixelFormat mtlRenderPipelineDescriptor  =
  fmap (coerce :: CULong -> MTLPixelFormat) $ sendMsg mtlRenderPipelineDescriptor (mkSelector "depthAttachmentPixelFormat") retCULong []

-- | @- setDepthAttachmentPixelFormat:@
setDepthAttachmentPixelFormat :: IsMTLRenderPipelineDescriptor mtlRenderPipelineDescriptor => mtlRenderPipelineDescriptor -> MTLPixelFormat -> IO ()
setDepthAttachmentPixelFormat mtlRenderPipelineDescriptor  value =
  sendMsg mtlRenderPipelineDescriptor (mkSelector "setDepthAttachmentPixelFormat:") retVoid [argCULong (coerce value)]

-- | @- stencilAttachmentPixelFormat@
stencilAttachmentPixelFormat :: IsMTLRenderPipelineDescriptor mtlRenderPipelineDescriptor => mtlRenderPipelineDescriptor -> IO MTLPixelFormat
stencilAttachmentPixelFormat mtlRenderPipelineDescriptor  =
  fmap (coerce :: CULong -> MTLPixelFormat) $ sendMsg mtlRenderPipelineDescriptor (mkSelector "stencilAttachmentPixelFormat") retCULong []

-- | @- setStencilAttachmentPixelFormat:@
setStencilAttachmentPixelFormat :: IsMTLRenderPipelineDescriptor mtlRenderPipelineDescriptor => mtlRenderPipelineDescriptor -> MTLPixelFormat -> IO ()
setStencilAttachmentPixelFormat mtlRenderPipelineDescriptor  value =
  sendMsg mtlRenderPipelineDescriptor (mkSelector "setStencilAttachmentPixelFormat:") retVoid [argCULong (coerce value)]

-- | @- inputPrimitiveTopology@
inputPrimitiveTopology :: IsMTLRenderPipelineDescriptor mtlRenderPipelineDescriptor => mtlRenderPipelineDescriptor -> IO MTLPrimitiveTopologyClass
inputPrimitiveTopology mtlRenderPipelineDescriptor  =
  fmap (coerce :: CULong -> MTLPrimitiveTopologyClass) $ sendMsg mtlRenderPipelineDescriptor (mkSelector "inputPrimitiveTopology") retCULong []

-- | @- setInputPrimitiveTopology:@
setInputPrimitiveTopology :: IsMTLRenderPipelineDescriptor mtlRenderPipelineDescriptor => mtlRenderPipelineDescriptor -> MTLPrimitiveTopologyClass -> IO ()
setInputPrimitiveTopology mtlRenderPipelineDescriptor  value =
  sendMsg mtlRenderPipelineDescriptor (mkSelector "setInputPrimitiveTopology:") retVoid [argCULong (coerce value)]

-- | @- tessellationPartitionMode@
tessellationPartitionMode :: IsMTLRenderPipelineDescriptor mtlRenderPipelineDescriptor => mtlRenderPipelineDescriptor -> IO MTLTessellationPartitionMode
tessellationPartitionMode mtlRenderPipelineDescriptor  =
  fmap (coerce :: CULong -> MTLTessellationPartitionMode) $ sendMsg mtlRenderPipelineDescriptor (mkSelector "tessellationPartitionMode") retCULong []

-- | @- setTessellationPartitionMode:@
setTessellationPartitionMode :: IsMTLRenderPipelineDescriptor mtlRenderPipelineDescriptor => mtlRenderPipelineDescriptor -> MTLTessellationPartitionMode -> IO ()
setTessellationPartitionMode mtlRenderPipelineDescriptor  value =
  sendMsg mtlRenderPipelineDescriptor (mkSelector "setTessellationPartitionMode:") retVoid [argCULong (coerce value)]

-- | @- maxTessellationFactor@
maxTessellationFactor :: IsMTLRenderPipelineDescriptor mtlRenderPipelineDescriptor => mtlRenderPipelineDescriptor -> IO CULong
maxTessellationFactor mtlRenderPipelineDescriptor  =
  sendMsg mtlRenderPipelineDescriptor (mkSelector "maxTessellationFactor") retCULong []

-- | @- setMaxTessellationFactor:@
setMaxTessellationFactor :: IsMTLRenderPipelineDescriptor mtlRenderPipelineDescriptor => mtlRenderPipelineDescriptor -> CULong -> IO ()
setMaxTessellationFactor mtlRenderPipelineDescriptor  value =
  sendMsg mtlRenderPipelineDescriptor (mkSelector "setMaxTessellationFactor:") retVoid [argCULong (fromIntegral value)]

-- | @- tessellationFactorScaleEnabled@
tessellationFactorScaleEnabled :: IsMTLRenderPipelineDescriptor mtlRenderPipelineDescriptor => mtlRenderPipelineDescriptor -> IO Bool
tessellationFactorScaleEnabled mtlRenderPipelineDescriptor  =
  fmap ((/= 0) :: CULong -> Bool) $ sendMsg mtlRenderPipelineDescriptor (mkSelector "tessellationFactorScaleEnabled") retCULong []

-- | @- setTessellationFactorScaleEnabled:@
setTessellationFactorScaleEnabled :: IsMTLRenderPipelineDescriptor mtlRenderPipelineDescriptor => mtlRenderPipelineDescriptor -> Bool -> IO ()
setTessellationFactorScaleEnabled mtlRenderPipelineDescriptor  value =
  sendMsg mtlRenderPipelineDescriptor (mkSelector "setTessellationFactorScaleEnabled:") retVoid [argCULong (if value then 1 else 0)]

-- | @- tessellationFactorFormat@
tessellationFactorFormat :: IsMTLRenderPipelineDescriptor mtlRenderPipelineDescriptor => mtlRenderPipelineDescriptor -> IO MTLTessellationFactorFormat
tessellationFactorFormat mtlRenderPipelineDescriptor  =
  fmap (coerce :: CULong -> MTLTessellationFactorFormat) $ sendMsg mtlRenderPipelineDescriptor (mkSelector "tessellationFactorFormat") retCULong []

-- | @- setTessellationFactorFormat:@
setTessellationFactorFormat :: IsMTLRenderPipelineDescriptor mtlRenderPipelineDescriptor => mtlRenderPipelineDescriptor -> MTLTessellationFactorFormat -> IO ()
setTessellationFactorFormat mtlRenderPipelineDescriptor  value =
  sendMsg mtlRenderPipelineDescriptor (mkSelector "setTessellationFactorFormat:") retVoid [argCULong (coerce value)]

-- | @- tessellationControlPointIndexType@
tessellationControlPointIndexType :: IsMTLRenderPipelineDescriptor mtlRenderPipelineDescriptor => mtlRenderPipelineDescriptor -> IO MTLTessellationControlPointIndexType
tessellationControlPointIndexType mtlRenderPipelineDescriptor  =
  fmap (coerce :: CULong -> MTLTessellationControlPointIndexType) $ sendMsg mtlRenderPipelineDescriptor (mkSelector "tessellationControlPointIndexType") retCULong []

-- | @- setTessellationControlPointIndexType:@
setTessellationControlPointIndexType :: IsMTLRenderPipelineDescriptor mtlRenderPipelineDescriptor => mtlRenderPipelineDescriptor -> MTLTessellationControlPointIndexType -> IO ()
setTessellationControlPointIndexType mtlRenderPipelineDescriptor  value =
  sendMsg mtlRenderPipelineDescriptor (mkSelector "setTessellationControlPointIndexType:") retVoid [argCULong (coerce value)]

-- | @- tessellationFactorStepFunction@
tessellationFactorStepFunction :: IsMTLRenderPipelineDescriptor mtlRenderPipelineDescriptor => mtlRenderPipelineDescriptor -> IO MTLTessellationFactorStepFunction
tessellationFactorStepFunction mtlRenderPipelineDescriptor  =
  fmap (coerce :: CULong -> MTLTessellationFactorStepFunction) $ sendMsg mtlRenderPipelineDescriptor (mkSelector "tessellationFactorStepFunction") retCULong []

-- | @- setTessellationFactorStepFunction:@
setTessellationFactorStepFunction :: IsMTLRenderPipelineDescriptor mtlRenderPipelineDescriptor => mtlRenderPipelineDescriptor -> MTLTessellationFactorStepFunction -> IO ()
setTessellationFactorStepFunction mtlRenderPipelineDescriptor  value =
  sendMsg mtlRenderPipelineDescriptor (mkSelector "setTessellationFactorStepFunction:") retVoid [argCULong (coerce value)]

-- | @- tessellationOutputWindingOrder@
tessellationOutputWindingOrder :: IsMTLRenderPipelineDescriptor mtlRenderPipelineDescriptor => mtlRenderPipelineDescriptor -> IO MTLWinding
tessellationOutputWindingOrder mtlRenderPipelineDescriptor  =
  fmap (coerce :: CULong -> MTLWinding) $ sendMsg mtlRenderPipelineDescriptor (mkSelector "tessellationOutputWindingOrder") retCULong []

-- | @- setTessellationOutputWindingOrder:@
setTessellationOutputWindingOrder :: IsMTLRenderPipelineDescriptor mtlRenderPipelineDescriptor => mtlRenderPipelineDescriptor -> MTLWinding -> IO ()
setTessellationOutputWindingOrder mtlRenderPipelineDescriptor  value =
  sendMsg mtlRenderPipelineDescriptor (mkSelector "setTessellationOutputWindingOrder:") retVoid [argCULong (coerce value)]

-- | @- vertexBuffers@
vertexBuffers :: IsMTLRenderPipelineDescriptor mtlRenderPipelineDescriptor => mtlRenderPipelineDescriptor -> IO (Id MTLPipelineBufferDescriptorArray)
vertexBuffers mtlRenderPipelineDescriptor  =
  sendMsg mtlRenderPipelineDescriptor (mkSelector "vertexBuffers") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- fragmentBuffers@
fragmentBuffers :: IsMTLRenderPipelineDescriptor mtlRenderPipelineDescriptor => mtlRenderPipelineDescriptor -> IO (Id MTLPipelineBufferDescriptorArray)
fragmentBuffers mtlRenderPipelineDescriptor  =
  sendMsg mtlRenderPipelineDescriptor (mkSelector "fragmentBuffers") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- supportIndirectCommandBuffers@
supportIndirectCommandBuffers :: IsMTLRenderPipelineDescriptor mtlRenderPipelineDescriptor => mtlRenderPipelineDescriptor -> IO Bool
supportIndirectCommandBuffers mtlRenderPipelineDescriptor  =
  fmap ((/= 0) :: CULong -> Bool) $ sendMsg mtlRenderPipelineDescriptor (mkSelector "supportIndirectCommandBuffers") retCULong []

-- | @- setSupportIndirectCommandBuffers:@
setSupportIndirectCommandBuffers :: IsMTLRenderPipelineDescriptor mtlRenderPipelineDescriptor => mtlRenderPipelineDescriptor -> Bool -> IO ()
setSupportIndirectCommandBuffers mtlRenderPipelineDescriptor  value =
  sendMsg mtlRenderPipelineDescriptor (mkSelector "setSupportIndirectCommandBuffers:") retVoid [argCULong (if value then 1 else 0)]

-- | vertexLinkedFunctions
--
-- The set of functions to be linked with the pipeline state and accessed from the vertex function.
--
-- See: MTLLinkedFunctions
--
-- ObjC selector: @- vertexLinkedFunctions@
vertexLinkedFunctions :: IsMTLRenderPipelineDescriptor mtlRenderPipelineDescriptor => mtlRenderPipelineDescriptor -> IO (Id MTLLinkedFunctions)
vertexLinkedFunctions mtlRenderPipelineDescriptor  =
  sendMsg mtlRenderPipelineDescriptor (mkSelector "vertexLinkedFunctions") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | vertexLinkedFunctions
--
-- The set of functions to be linked with the pipeline state and accessed from the vertex function.
--
-- See: MTLLinkedFunctions
--
-- ObjC selector: @- setVertexLinkedFunctions:@
setVertexLinkedFunctions :: (IsMTLRenderPipelineDescriptor mtlRenderPipelineDescriptor, IsMTLLinkedFunctions value) => mtlRenderPipelineDescriptor -> value -> IO ()
setVertexLinkedFunctions mtlRenderPipelineDescriptor  value =
withObjCPtr value $ \raw_value ->
    sendMsg mtlRenderPipelineDescriptor (mkSelector "setVertexLinkedFunctions:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- | fragmentLinkedFunctions
--
-- The set of functions to be linked with the pipeline state and accessed from the fragment function.
--
-- See: MTLLinkedFunctions
--
-- ObjC selector: @- fragmentLinkedFunctions@
fragmentLinkedFunctions :: IsMTLRenderPipelineDescriptor mtlRenderPipelineDescriptor => mtlRenderPipelineDescriptor -> IO (Id MTLLinkedFunctions)
fragmentLinkedFunctions mtlRenderPipelineDescriptor  =
  sendMsg mtlRenderPipelineDescriptor (mkSelector "fragmentLinkedFunctions") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | fragmentLinkedFunctions
--
-- The set of functions to be linked with the pipeline state and accessed from the fragment function.
--
-- See: MTLLinkedFunctions
--
-- ObjC selector: @- setFragmentLinkedFunctions:@
setFragmentLinkedFunctions :: (IsMTLRenderPipelineDescriptor mtlRenderPipelineDescriptor, IsMTLLinkedFunctions value) => mtlRenderPipelineDescriptor -> value -> IO ()
setFragmentLinkedFunctions mtlRenderPipelineDescriptor  value =
withObjCPtr value $ \raw_value ->
    sendMsg mtlRenderPipelineDescriptor (mkSelector "setFragmentLinkedFunctions:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- | supportAddingVertexBinaryFunctions
--
-- This flag makes this pipeline support creating a new pipeline by adding binary functions.
--
-- ObjC selector: @- supportAddingVertexBinaryFunctions@
supportAddingVertexBinaryFunctions :: IsMTLRenderPipelineDescriptor mtlRenderPipelineDescriptor => mtlRenderPipelineDescriptor -> IO Bool
supportAddingVertexBinaryFunctions mtlRenderPipelineDescriptor  =
  fmap ((/= 0) :: CULong -> Bool) $ sendMsg mtlRenderPipelineDescriptor (mkSelector "supportAddingVertexBinaryFunctions") retCULong []

-- | supportAddingVertexBinaryFunctions
--
-- This flag makes this pipeline support creating a new pipeline by adding binary functions.
--
-- ObjC selector: @- setSupportAddingVertexBinaryFunctions:@
setSupportAddingVertexBinaryFunctions :: IsMTLRenderPipelineDescriptor mtlRenderPipelineDescriptor => mtlRenderPipelineDescriptor -> Bool -> IO ()
setSupportAddingVertexBinaryFunctions mtlRenderPipelineDescriptor  value =
  sendMsg mtlRenderPipelineDescriptor (mkSelector "setSupportAddingVertexBinaryFunctions:") retVoid [argCULong (if value then 1 else 0)]

-- | supportFragmentAddingBinaryFunctions
--
-- This flag makes this pipeline support creating a new pipeline by adding binary functions.
--
-- ObjC selector: @- supportAddingFragmentBinaryFunctions@
supportAddingFragmentBinaryFunctions :: IsMTLRenderPipelineDescriptor mtlRenderPipelineDescriptor => mtlRenderPipelineDescriptor -> IO Bool
supportAddingFragmentBinaryFunctions mtlRenderPipelineDescriptor  =
  fmap ((/= 0) :: CULong -> Bool) $ sendMsg mtlRenderPipelineDescriptor (mkSelector "supportAddingFragmentBinaryFunctions") retCULong []

-- | supportFragmentAddingBinaryFunctions
--
-- This flag makes this pipeline support creating a new pipeline by adding binary functions.
--
-- ObjC selector: @- setSupportAddingFragmentBinaryFunctions:@
setSupportAddingFragmentBinaryFunctions :: IsMTLRenderPipelineDescriptor mtlRenderPipelineDescriptor => mtlRenderPipelineDescriptor -> Bool -> IO ()
setSupportAddingFragmentBinaryFunctions mtlRenderPipelineDescriptor  value =
  sendMsg mtlRenderPipelineDescriptor (mkSelector "setSupportAddingFragmentBinaryFunctions:") retVoid [argCULong (if value then 1 else 0)]

-- | maxVertexCallStackDepth
--
-- The maximum depth of the call stack in stack frames from the shader. Defaults to 1 additional stack frame.
--
-- ObjC selector: @- maxVertexCallStackDepth@
maxVertexCallStackDepth :: IsMTLRenderPipelineDescriptor mtlRenderPipelineDescriptor => mtlRenderPipelineDescriptor -> IO CULong
maxVertexCallStackDepth mtlRenderPipelineDescriptor  =
  sendMsg mtlRenderPipelineDescriptor (mkSelector "maxVertexCallStackDepth") retCULong []

-- | maxVertexCallStackDepth
--
-- The maximum depth of the call stack in stack frames from the shader. Defaults to 1 additional stack frame.
--
-- ObjC selector: @- setMaxVertexCallStackDepth:@
setMaxVertexCallStackDepth :: IsMTLRenderPipelineDescriptor mtlRenderPipelineDescriptor => mtlRenderPipelineDescriptor -> CULong -> IO ()
setMaxVertexCallStackDepth mtlRenderPipelineDescriptor  value =
  sendMsg mtlRenderPipelineDescriptor (mkSelector "setMaxVertexCallStackDepth:") retVoid [argCULong (fromIntegral value)]

-- | maxFragmentCallStackDepth
--
-- The maximum depth of the call stack in stack frames from the shader. Defaults to 1 additional stack frame.
--
-- ObjC selector: @- maxFragmentCallStackDepth@
maxFragmentCallStackDepth :: IsMTLRenderPipelineDescriptor mtlRenderPipelineDescriptor => mtlRenderPipelineDescriptor -> IO CULong
maxFragmentCallStackDepth mtlRenderPipelineDescriptor  =
  sendMsg mtlRenderPipelineDescriptor (mkSelector "maxFragmentCallStackDepth") retCULong []

-- | maxFragmentCallStackDepth
--
-- The maximum depth of the call stack in stack frames from the shader. Defaults to 1 additional stack frame.
--
-- ObjC selector: @- setMaxFragmentCallStackDepth:@
setMaxFragmentCallStackDepth :: IsMTLRenderPipelineDescriptor mtlRenderPipelineDescriptor => mtlRenderPipelineDescriptor -> CULong -> IO ()
setMaxFragmentCallStackDepth mtlRenderPipelineDescriptor  value =
  sendMsg mtlRenderPipelineDescriptor (mkSelector "setMaxFragmentCallStackDepth:") retVoid [argCULong (fromIntegral value)]

-- | shaderValidation
--
-- Toggle that determines whether Metal Shader Validation should be enabled or disabled for the pipeline.
--
-- The value can be overridden using @MTL_SHADER_VALIDATION_ENABLE_PIPELINES@ or @MTL_SHADER_VALIDATION_DISABLE_PIPELINES@ Environment Variables.
--
-- ObjC selector: @- shaderValidation@
shaderValidation :: IsMTLRenderPipelineDescriptor mtlRenderPipelineDescriptor => mtlRenderPipelineDescriptor -> IO MTLShaderValidation
shaderValidation mtlRenderPipelineDescriptor  =
  fmap (coerce :: CLong -> MTLShaderValidation) $ sendMsg mtlRenderPipelineDescriptor (mkSelector "shaderValidation") retCLong []

-- | shaderValidation
--
-- Toggle that determines whether Metal Shader Validation should be enabled or disabled for the pipeline.
--
-- The value can be overridden using @MTL_SHADER_VALIDATION_ENABLE_PIPELINES@ or @MTL_SHADER_VALIDATION_DISABLE_PIPELINES@ Environment Variables.
--
-- ObjC selector: @- setShaderValidation:@
setShaderValidation :: IsMTLRenderPipelineDescriptor mtlRenderPipelineDescriptor => mtlRenderPipelineDescriptor -> MTLShaderValidation -> IO ()
setShaderValidation mtlRenderPipelineDescriptor  value =
  sendMsg mtlRenderPipelineDescriptor (mkSelector "setShaderValidation:") retVoid [argCLong (coerce value)]

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

-- | @Selector@ for @vertexDescriptor@
vertexDescriptorSelector :: Selector
vertexDescriptorSelector = mkSelector "vertexDescriptor"

-- | @Selector@ for @setVertexDescriptor:@
setVertexDescriptorSelector :: Selector
setVertexDescriptorSelector = mkSelector "setVertexDescriptor:"

-- | @Selector@ for @sampleCount@
sampleCountSelector :: Selector
sampleCountSelector = mkSelector "sampleCount"

-- | @Selector@ for @setSampleCount:@
setSampleCountSelector :: Selector
setSampleCountSelector = mkSelector "setSampleCount:"

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

-- | @Selector@ for @inputPrimitiveTopology@
inputPrimitiveTopologySelector :: Selector
inputPrimitiveTopologySelector = mkSelector "inputPrimitiveTopology"

-- | @Selector@ for @setInputPrimitiveTopology:@
setInputPrimitiveTopologySelector :: Selector
setInputPrimitiveTopologySelector = mkSelector "setInputPrimitiveTopology:"

-- | @Selector@ for @tessellationPartitionMode@
tessellationPartitionModeSelector :: Selector
tessellationPartitionModeSelector = mkSelector "tessellationPartitionMode"

-- | @Selector@ for @setTessellationPartitionMode:@
setTessellationPartitionModeSelector :: Selector
setTessellationPartitionModeSelector = mkSelector "setTessellationPartitionMode:"

-- | @Selector@ for @maxTessellationFactor@
maxTessellationFactorSelector :: Selector
maxTessellationFactorSelector = mkSelector "maxTessellationFactor"

-- | @Selector@ for @setMaxTessellationFactor:@
setMaxTessellationFactorSelector :: Selector
setMaxTessellationFactorSelector = mkSelector "setMaxTessellationFactor:"

-- | @Selector@ for @tessellationFactorScaleEnabled@
tessellationFactorScaleEnabledSelector :: Selector
tessellationFactorScaleEnabledSelector = mkSelector "tessellationFactorScaleEnabled"

-- | @Selector@ for @setTessellationFactorScaleEnabled:@
setTessellationFactorScaleEnabledSelector :: Selector
setTessellationFactorScaleEnabledSelector = mkSelector "setTessellationFactorScaleEnabled:"

-- | @Selector@ for @tessellationFactorFormat@
tessellationFactorFormatSelector :: Selector
tessellationFactorFormatSelector = mkSelector "tessellationFactorFormat"

-- | @Selector@ for @setTessellationFactorFormat:@
setTessellationFactorFormatSelector :: Selector
setTessellationFactorFormatSelector = mkSelector "setTessellationFactorFormat:"

-- | @Selector@ for @tessellationControlPointIndexType@
tessellationControlPointIndexTypeSelector :: Selector
tessellationControlPointIndexTypeSelector = mkSelector "tessellationControlPointIndexType"

-- | @Selector@ for @setTessellationControlPointIndexType:@
setTessellationControlPointIndexTypeSelector :: Selector
setTessellationControlPointIndexTypeSelector = mkSelector "setTessellationControlPointIndexType:"

-- | @Selector@ for @tessellationFactorStepFunction@
tessellationFactorStepFunctionSelector :: Selector
tessellationFactorStepFunctionSelector = mkSelector "tessellationFactorStepFunction"

-- | @Selector@ for @setTessellationFactorStepFunction:@
setTessellationFactorStepFunctionSelector :: Selector
setTessellationFactorStepFunctionSelector = mkSelector "setTessellationFactorStepFunction:"

-- | @Selector@ for @tessellationOutputWindingOrder@
tessellationOutputWindingOrderSelector :: Selector
tessellationOutputWindingOrderSelector = mkSelector "tessellationOutputWindingOrder"

-- | @Selector@ for @setTessellationOutputWindingOrder:@
setTessellationOutputWindingOrderSelector :: Selector
setTessellationOutputWindingOrderSelector = mkSelector "setTessellationOutputWindingOrder:"

-- | @Selector@ for @vertexBuffers@
vertexBuffersSelector :: Selector
vertexBuffersSelector = mkSelector "vertexBuffers"

-- | @Selector@ for @fragmentBuffers@
fragmentBuffersSelector :: Selector
fragmentBuffersSelector = mkSelector "fragmentBuffers"

-- | @Selector@ for @supportIndirectCommandBuffers@
supportIndirectCommandBuffersSelector :: Selector
supportIndirectCommandBuffersSelector = mkSelector "supportIndirectCommandBuffers"

-- | @Selector@ for @setSupportIndirectCommandBuffers:@
setSupportIndirectCommandBuffersSelector :: Selector
setSupportIndirectCommandBuffersSelector = mkSelector "setSupportIndirectCommandBuffers:"

-- | @Selector@ for @vertexLinkedFunctions@
vertexLinkedFunctionsSelector :: Selector
vertexLinkedFunctionsSelector = mkSelector "vertexLinkedFunctions"

-- | @Selector@ for @setVertexLinkedFunctions:@
setVertexLinkedFunctionsSelector :: Selector
setVertexLinkedFunctionsSelector = mkSelector "setVertexLinkedFunctions:"

-- | @Selector@ for @fragmentLinkedFunctions@
fragmentLinkedFunctionsSelector :: Selector
fragmentLinkedFunctionsSelector = mkSelector "fragmentLinkedFunctions"

-- | @Selector@ for @setFragmentLinkedFunctions:@
setFragmentLinkedFunctionsSelector :: Selector
setFragmentLinkedFunctionsSelector = mkSelector "setFragmentLinkedFunctions:"

-- | @Selector@ for @supportAddingVertexBinaryFunctions@
supportAddingVertexBinaryFunctionsSelector :: Selector
supportAddingVertexBinaryFunctionsSelector = mkSelector "supportAddingVertexBinaryFunctions"

-- | @Selector@ for @setSupportAddingVertexBinaryFunctions:@
setSupportAddingVertexBinaryFunctionsSelector :: Selector
setSupportAddingVertexBinaryFunctionsSelector = mkSelector "setSupportAddingVertexBinaryFunctions:"

-- | @Selector@ for @supportAddingFragmentBinaryFunctions@
supportAddingFragmentBinaryFunctionsSelector :: Selector
supportAddingFragmentBinaryFunctionsSelector = mkSelector "supportAddingFragmentBinaryFunctions"

-- | @Selector@ for @setSupportAddingFragmentBinaryFunctions:@
setSupportAddingFragmentBinaryFunctionsSelector :: Selector
setSupportAddingFragmentBinaryFunctionsSelector = mkSelector "setSupportAddingFragmentBinaryFunctions:"

-- | @Selector@ for @maxVertexCallStackDepth@
maxVertexCallStackDepthSelector :: Selector
maxVertexCallStackDepthSelector = mkSelector "maxVertexCallStackDepth"

-- | @Selector@ for @setMaxVertexCallStackDepth:@
setMaxVertexCallStackDepthSelector :: Selector
setMaxVertexCallStackDepthSelector = mkSelector "setMaxVertexCallStackDepth:"

-- | @Selector@ for @maxFragmentCallStackDepth@
maxFragmentCallStackDepthSelector :: Selector
maxFragmentCallStackDepthSelector = mkSelector "maxFragmentCallStackDepth"

-- | @Selector@ for @setMaxFragmentCallStackDepth:@
setMaxFragmentCallStackDepthSelector :: Selector
setMaxFragmentCallStackDepthSelector = mkSelector "setMaxFragmentCallStackDepth:"

-- | @Selector@ for @shaderValidation@
shaderValidationSelector :: Selector
shaderValidationSelector = mkSelector "shaderValidation"

-- | @Selector@ for @setShaderValidation:@
setShaderValidationSelector :: Selector
setShaderValidationSelector = mkSelector "setShaderValidation:"

