{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE DataKinds #-}
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
  , vertexFunction
  , setVertexFunction
  , fragmentFunction
  , setFragmentFunction
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
  , binaryArchives
  , setBinaryArchives
  , vertexPreloadedLibraries
  , setVertexPreloadedLibraries
  , fragmentPreloadedLibraries
  , setFragmentPreloadedLibraries
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
  , alphaToCoverageEnabledSelector
  , alphaToOneEnabledSelector
  , binaryArchivesSelector
  , colorAttachmentsSelector
  , depthAttachmentPixelFormatSelector
  , fragmentBuffersSelector
  , fragmentFunctionSelector
  , fragmentLinkedFunctionsSelector
  , fragmentPreloadedLibrariesSelector
  , inputPrimitiveTopologySelector
  , labelSelector
  , maxFragmentCallStackDepthSelector
  , maxTessellationFactorSelector
  , maxVertexAmplificationCountSelector
  , maxVertexCallStackDepthSelector
  , rasterSampleCountSelector
  , rasterizationEnabledSelector
  , resetSelector
  , sampleCountSelector
  , setAlphaToCoverageEnabledSelector
  , setAlphaToOneEnabledSelector
  , setBinaryArchivesSelector
  , setDepthAttachmentPixelFormatSelector
  , setFragmentFunctionSelector
  , setFragmentLinkedFunctionsSelector
  , setFragmentPreloadedLibrariesSelector
  , setInputPrimitiveTopologySelector
  , setLabelSelector
  , setMaxFragmentCallStackDepthSelector
  , setMaxTessellationFactorSelector
  , setMaxVertexAmplificationCountSelector
  , setMaxVertexCallStackDepthSelector
  , setRasterSampleCountSelector
  , setRasterizationEnabledSelector
  , setSampleCountSelector
  , setShaderValidationSelector
  , setStencilAttachmentPixelFormatSelector
  , setSupportAddingFragmentBinaryFunctionsSelector
  , setSupportAddingVertexBinaryFunctionsSelector
  , setSupportIndirectCommandBuffersSelector
  , setTessellationControlPointIndexTypeSelector
  , setTessellationFactorFormatSelector
  , setTessellationFactorScaleEnabledSelector
  , setTessellationFactorStepFunctionSelector
  , setTessellationOutputWindingOrderSelector
  , setTessellationPartitionModeSelector
  , setVertexDescriptorSelector
  , setVertexFunctionSelector
  , setVertexLinkedFunctionsSelector
  , setVertexPreloadedLibrariesSelector
  , shaderValidationSelector
  , stencilAttachmentPixelFormatSelector
  , supportAddingFragmentBinaryFunctionsSelector
  , supportAddingVertexBinaryFunctionsSelector
  , supportIndirectCommandBuffersSelector
  , tessellationControlPointIndexTypeSelector
  , tessellationFactorFormatSelector
  , tessellationFactorScaleEnabledSelector
  , tessellationFactorStepFunctionSelector
  , tessellationOutputWindingOrderSelector
  , tessellationPartitionModeSelector
  , vertexBuffersSelector
  , vertexDescriptorSelector
  , vertexFunctionSelector
  , vertexLinkedFunctionsSelector
  , vertexPreloadedLibrariesSelector

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

import Foreign.Ptr (Ptr, FunPtr)
import Foreign.C.Types

import ObjC.Runtime.Types
import ObjC.Runtime.Message (sendMessage, sendOwnedMessage, sendClassMessage, sendOwnedClassMessage)
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
reset mtlRenderPipelineDescriptor =
  sendMessage mtlRenderPipelineDescriptor resetSelector

-- | @- label@
label :: IsMTLRenderPipelineDescriptor mtlRenderPipelineDescriptor => mtlRenderPipelineDescriptor -> IO (Id NSString)
label mtlRenderPipelineDescriptor =
  sendMessage mtlRenderPipelineDescriptor labelSelector

-- | @- setLabel:@
setLabel :: (IsMTLRenderPipelineDescriptor mtlRenderPipelineDescriptor, IsNSString value) => mtlRenderPipelineDescriptor -> value -> IO ()
setLabel mtlRenderPipelineDescriptor value =
  sendMessage mtlRenderPipelineDescriptor setLabelSelector (toNSString value)

-- | @- vertexFunction@
vertexFunction :: IsMTLRenderPipelineDescriptor mtlRenderPipelineDescriptor => mtlRenderPipelineDescriptor -> IO RawId
vertexFunction mtlRenderPipelineDescriptor =
  sendMessage mtlRenderPipelineDescriptor vertexFunctionSelector

-- | @- setVertexFunction:@
setVertexFunction :: IsMTLRenderPipelineDescriptor mtlRenderPipelineDescriptor => mtlRenderPipelineDescriptor -> RawId -> IO ()
setVertexFunction mtlRenderPipelineDescriptor value =
  sendMessage mtlRenderPipelineDescriptor setVertexFunctionSelector value

-- | @- fragmentFunction@
fragmentFunction :: IsMTLRenderPipelineDescriptor mtlRenderPipelineDescriptor => mtlRenderPipelineDescriptor -> IO RawId
fragmentFunction mtlRenderPipelineDescriptor =
  sendMessage mtlRenderPipelineDescriptor fragmentFunctionSelector

-- | @- setFragmentFunction:@
setFragmentFunction :: IsMTLRenderPipelineDescriptor mtlRenderPipelineDescriptor => mtlRenderPipelineDescriptor -> RawId -> IO ()
setFragmentFunction mtlRenderPipelineDescriptor value =
  sendMessage mtlRenderPipelineDescriptor setFragmentFunctionSelector value

-- | @- vertexDescriptor@
vertexDescriptor :: IsMTLRenderPipelineDescriptor mtlRenderPipelineDescriptor => mtlRenderPipelineDescriptor -> IO (Id MTLVertexDescriptor)
vertexDescriptor mtlRenderPipelineDescriptor =
  sendMessage mtlRenderPipelineDescriptor vertexDescriptorSelector

-- | @- setVertexDescriptor:@
setVertexDescriptor :: (IsMTLRenderPipelineDescriptor mtlRenderPipelineDescriptor, IsMTLVertexDescriptor value) => mtlRenderPipelineDescriptor -> value -> IO ()
setVertexDescriptor mtlRenderPipelineDescriptor value =
  sendMessage mtlRenderPipelineDescriptor setVertexDescriptorSelector (toMTLVertexDescriptor value)

-- | @- sampleCount@
sampleCount :: IsMTLRenderPipelineDescriptor mtlRenderPipelineDescriptor => mtlRenderPipelineDescriptor -> IO CULong
sampleCount mtlRenderPipelineDescriptor =
  sendMessage mtlRenderPipelineDescriptor sampleCountSelector

-- | @- setSampleCount:@
setSampleCount :: IsMTLRenderPipelineDescriptor mtlRenderPipelineDescriptor => mtlRenderPipelineDescriptor -> CULong -> IO ()
setSampleCount mtlRenderPipelineDescriptor value =
  sendMessage mtlRenderPipelineDescriptor setSampleCountSelector value

-- | @- rasterSampleCount@
rasterSampleCount :: IsMTLRenderPipelineDescriptor mtlRenderPipelineDescriptor => mtlRenderPipelineDescriptor -> IO CULong
rasterSampleCount mtlRenderPipelineDescriptor =
  sendMessage mtlRenderPipelineDescriptor rasterSampleCountSelector

-- | @- setRasterSampleCount:@
setRasterSampleCount :: IsMTLRenderPipelineDescriptor mtlRenderPipelineDescriptor => mtlRenderPipelineDescriptor -> CULong -> IO ()
setRasterSampleCount mtlRenderPipelineDescriptor value =
  sendMessage mtlRenderPipelineDescriptor setRasterSampleCountSelector value

-- | @- alphaToCoverageEnabled@
alphaToCoverageEnabled :: IsMTLRenderPipelineDescriptor mtlRenderPipelineDescriptor => mtlRenderPipelineDescriptor -> IO Bool
alphaToCoverageEnabled mtlRenderPipelineDescriptor =
  sendMessage mtlRenderPipelineDescriptor alphaToCoverageEnabledSelector

-- | @- setAlphaToCoverageEnabled:@
setAlphaToCoverageEnabled :: IsMTLRenderPipelineDescriptor mtlRenderPipelineDescriptor => mtlRenderPipelineDescriptor -> Bool -> IO ()
setAlphaToCoverageEnabled mtlRenderPipelineDescriptor value =
  sendMessage mtlRenderPipelineDescriptor setAlphaToCoverageEnabledSelector value

-- | @- alphaToOneEnabled@
alphaToOneEnabled :: IsMTLRenderPipelineDescriptor mtlRenderPipelineDescriptor => mtlRenderPipelineDescriptor -> IO Bool
alphaToOneEnabled mtlRenderPipelineDescriptor =
  sendMessage mtlRenderPipelineDescriptor alphaToOneEnabledSelector

-- | @- setAlphaToOneEnabled:@
setAlphaToOneEnabled :: IsMTLRenderPipelineDescriptor mtlRenderPipelineDescriptor => mtlRenderPipelineDescriptor -> Bool -> IO ()
setAlphaToOneEnabled mtlRenderPipelineDescriptor value =
  sendMessage mtlRenderPipelineDescriptor setAlphaToOneEnabledSelector value

-- | @- rasterizationEnabled@
rasterizationEnabled :: IsMTLRenderPipelineDescriptor mtlRenderPipelineDescriptor => mtlRenderPipelineDescriptor -> IO Bool
rasterizationEnabled mtlRenderPipelineDescriptor =
  sendMessage mtlRenderPipelineDescriptor rasterizationEnabledSelector

-- | @- setRasterizationEnabled:@
setRasterizationEnabled :: IsMTLRenderPipelineDescriptor mtlRenderPipelineDescriptor => mtlRenderPipelineDescriptor -> Bool -> IO ()
setRasterizationEnabled mtlRenderPipelineDescriptor value =
  sendMessage mtlRenderPipelineDescriptor setRasterizationEnabledSelector value

-- | @- maxVertexAmplificationCount@
maxVertexAmplificationCount :: IsMTLRenderPipelineDescriptor mtlRenderPipelineDescriptor => mtlRenderPipelineDescriptor -> IO CULong
maxVertexAmplificationCount mtlRenderPipelineDescriptor =
  sendMessage mtlRenderPipelineDescriptor maxVertexAmplificationCountSelector

-- | @- setMaxVertexAmplificationCount:@
setMaxVertexAmplificationCount :: IsMTLRenderPipelineDescriptor mtlRenderPipelineDescriptor => mtlRenderPipelineDescriptor -> CULong -> IO ()
setMaxVertexAmplificationCount mtlRenderPipelineDescriptor value =
  sendMessage mtlRenderPipelineDescriptor setMaxVertexAmplificationCountSelector value

-- | @- colorAttachments@
colorAttachments :: IsMTLRenderPipelineDescriptor mtlRenderPipelineDescriptor => mtlRenderPipelineDescriptor -> IO (Id MTLRenderPipelineColorAttachmentDescriptorArray)
colorAttachments mtlRenderPipelineDescriptor =
  sendMessage mtlRenderPipelineDescriptor colorAttachmentsSelector

-- | @- depthAttachmentPixelFormat@
depthAttachmentPixelFormat :: IsMTLRenderPipelineDescriptor mtlRenderPipelineDescriptor => mtlRenderPipelineDescriptor -> IO MTLPixelFormat
depthAttachmentPixelFormat mtlRenderPipelineDescriptor =
  sendMessage mtlRenderPipelineDescriptor depthAttachmentPixelFormatSelector

-- | @- setDepthAttachmentPixelFormat:@
setDepthAttachmentPixelFormat :: IsMTLRenderPipelineDescriptor mtlRenderPipelineDescriptor => mtlRenderPipelineDescriptor -> MTLPixelFormat -> IO ()
setDepthAttachmentPixelFormat mtlRenderPipelineDescriptor value =
  sendMessage mtlRenderPipelineDescriptor setDepthAttachmentPixelFormatSelector value

-- | @- stencilAttachmentPixelFormat@
stencilAttachmentPixelFormat :: IsMTLRenderPipelineDescriptor mtlRenderPipelineDescriptor => mtlRenderPipelineDescriptor -> IO MTLPixelFormat
stencilAttachmentPixelFormat mtlRenderPipelineDescriptor =
  sendMessage mtlRenderPipelineDescriptor stencilAttachmentPixelFormatSelector

-- | @- setStencilAttachmentPixelFormat:@
setStencilAttachmentPixelFormat :: IsMTLRenderPipelineDescriptor mtlRenderPipelineDescriptor => mtlRenderPipelineDescriptor -> MTLPixelFormat -> IO ()
setStencilAttachmentPixelFormat mtlRenderPipelineDescriptor value =
  sendMessage mtlRenderPipelineDescriptor setStencilAttachmentPixelFormatSelector value

-- | @- inputPrimitiveTopology@
inputPrimitiveTopology :: IsMTLRenderPipelineDescriptor mtlRenderPipelineDescriptor => mtlRenderPipelineDescriptor -> IO MTLPrimitiveTopologyClass
inputPrimitiveTopology mtlRenderPipelineDescriptor =
  sendMessage mtlRenderPipelineDescriptor inputPrimitiveTopologySelector

-- | @- setInputPrimitiveTopology:@
setInputPrimitiveTopology :: IsMTLRenderPipelineDescriptor mtlRenderPipelineDescriptor => mtlRenderPipelineDescriptor -> MTLPrimitiveTopologyClass -> IO ()
setInputPrimitiveTopology mtlRenderPipelineDescriptor value =
  sendMessage mtlRenderPipelineDescriptor setInputPrimitiveTopologySelector value

-- | @- tessellationPartitionMode@
tessellationPartitionMode :: IsMTLRenderPipelineDescriptor mtlRenderPipelineDescriptor => mtlRenderPipelineDescriptor -> IO MTLTessellationPartitionMode
tessellationPartitionMode mtlRenderPipelineDescriptor =
  sendMessage mtlRenderPipelineDescriptor tessellationPartitionModeSelector

-- | @- setTessellationPartitionMode:@
setTessellationPartitionMode :: IsMTLRenderPipelineDescriptor mtlRenderPipelineDescriptor => mtlRenderPipelineDescriptor -> MTLTessellationPartitionMode -> IO ()
setTessellationPartitionMode mtlRenderPipelineDescriptor value =
  sendMessage mtlRenderPipelineDescriptor setTessellationPartitionModeSelector value

-- | @- maxTessellationFactor@
maxTessellationFactor :: IsMTLRenderPipelineDescriptor mtlRenderPipelineDescriptor => mtlRenderPipelineDescriptor -> IO CULong
maxTessellationFactor mtlRenderPipelineDescriptor =
  sendMessage mtlRenderPipelineDescriptor maxTessellationFactorSelector

-- | @- setMaxTessellationFactor:@
setMaxTessellationFactor :: IsMTLRenderPipelineDescriptor mtlRenderPipelineDescriptor => mtlRenderPipelineDescriptor -> CULong -> IO ()
setMaxTessellationFactor mtlRenderPipelineDescriptor value =
  sendMessage mtlRenderPipelineDescriptor setMaxTessellationFactorSelector value

-- | @- tessellationFactorScaleEnabled@
tessellationFactorScaleEnabled :: IsMTLRenderPipelineDescriptor mtlRenderPipelineDescriptor => mtlRenderPipelineDescriptor -> IO Bool
tessellationFactorScaleEnabled mtlRenderPipelineDescriptor =
  sendMessage mtlRenderPipelineDescriptor tessellationFactorScaleEnabledSelector

-- | @- setTessellationFactorScaleEnabled:@
setTessellationFactorScaleEnabled :: IsMTLRenderPipelineDescriptor mtlRenderPipelineDescriptor => mtlRenderPipelineDescriptor -> Bool -> IO ()
setTessellationFactorScaleEnabled mtlRenderPipelineDescriptor value =
  sendMessage mtlRenderPipelineDescriptor setTessellationFactorScaleEnabledSelector value

-- | @- tessellationFactorFormat@
tessellationFactorFormat :: IsMTLRenderPipelineDescriptor mtlRenderPipelineDescriptor => mtlRenderPipelineDescriptor -> IO MTLTessellationFactorFormat
tessellationFactorFormat mtlRenderPipelineDescriptor =
  sendMessage mtlRenderPipelineDescriptor tessellationFactorFormatSelector

-- | @- setTessellationFactorFormat:@
setTessellationFactorFormat :: IsMTLRenderPipelineDescriptor mtlRenderPipelineDescriptor => mtlRenderPipelineDescriptor -> MTLTessellationFactorFormat -> IO ()
setTessellationFactorFormat mtlRenderPipelineDescriptor value =
  sendMessage mtlRenderPipelineDescriptor setTessellationFactorFormatSelector value

-- | @- tessellationControlPointIndexType@
tessellationControlPointIndexType :: IsMTLRenderPipelineDescriptor mtlRenderPipelineDescriptor => mtlRenderPipelineDescriptor -> IO MTLTessellationControlPointIndexType
tessellationControlPointIndexType mtlRenderPipelineDescriptor =
  sendMessage mtlRenderPipelineDescriptor tessellationControlPointIndexTypeSelector

-- | @- setTessellationControlPointIndexType:@
setTessellationControlPointIndexType :: IsMTLRenderPipelineDescriptor mtlRenderPipelineDescriptor => mtlRenderPipelineDescriptor -> MTLTessellationControlPointIndexType -> IO ()
setTessellationControlPointIndexType mtlRenderPipelineDescriptor value =
  sendMessage mtlRenderPipelineDescriptor setTessellationControlPointIndexTypeSelector value

-- | @- tessellationFactorStepFunction@
tessellationFactorStepFunction :: IsMTLRenderPipelineDescriptor mtlRenderPipelineDescriptor => mtlRenderPipelineDescriptor -> IO MTLTessellationFactorStepFunction
tessellationFactorStepFunction mtlRenderPipelineDescriptor =
  sendMessage mtlRenderPipelineDescriptor tessellationFactorStepFunctionSelector

-- | @- setTessellationFactorStepFunction:@
setTessellationFactorStepFunction :: IsMTLRenderPipelineDescriptor mtlRenderPipelineDescriptor => mtlRenderPipelineDescriptor -> MTLTessellationFactorStepFunction -> IO ()
setTessellationFactorStepFunction mtlRenderPipelineDescriptor value =
  sendMessage mtlRenderPipelineDescriptor setTessellationFactorStepFunctionSelector value

-- | @- tessellationOutputWindingOrder@
tessellationOutputWindingOrder :: IsMTLRenderPipelineDescriptor mtlRenderPipelineDescriptor => mtlRenderPipelineDescriptor -> IO MTLWinding
tessellationOutputWindingOrder mtlRenderPipelineDescriptor =
  sendMessage mtlRenderPipelineDescriptor tessellationOutputWindingOrderSelector

-- | @- setTessellationOutputWindingOrder:@
setTessellationOutputWindingOrder :: IsMTLRenderPipelineDescriptor mtlRenderPipelineDescriptor => mtlRenderPipelineDescriptor -> MTLWinding -> IO ()
setTessellationOutputWindingOrder mtlRenderPipelineDescriptor value =
  sendMessage mtlRenderPipelineDescriptor setTessellationOutputWindingOrderSelector value

-- | @- vertexBuffers@
vertexBuffers :: IsMTLRenderPipelineDescriptor mtlRenderPipelineDescriptor => mtlRenderPipelineDescriptor -> IO (Id MTLPipelineBufferDescriptorArray)
vertexBuffers mtlRenderPipelineDescriptor =
  sendMessage mtlRenderPipelineDescriptor vertexBuffersSelector

-- | @- fragmentBuffers@
fragmentBuffers :: IsMTLRenderPipelineDescriptor mtlRenderPipelineDescriptor => mtlRenderPipelineDescriptor -> IO (Id MTLPipelineBufferDescriptorArray)
fragmentBuffers mtlRenderPipelineDescriptor =
  sendMessage mtlRenderPipelineDescriptor fragmentBuffersSelector

-- | @- supportIndirectCommandBuffers@
supportIndirectCommandBuffers :: IsMTLRenderPipelineDescriptor mtlRenderPipelineDescriptor => mtlRenderPipelineDescriptor -> IO Bool
supportIndirectCommandBuffers mtlRenderPipelineDescriptor =
  sendMessage mtlRenderPipelineDescriptor supportIndirectCommandBuffersSelector

-- | @- setSupportIndirectCommandBuffers:@
setSupportIndirectCommandBuffers :: IsMTLRenderPipelineDescriptor mtlRenderPipelineDescriptor => mtlRenderPipelineDescriptor -> Bool -> IO ()
setSupportIndirectCommandBuffers mtlRenderPipelineDescriptor value =
  sendMessage mtlRenderPipelineDescriptor setSupportIndirectCommandBuffersSelector value

-- | binaryArchives
--
-- The set of MTLBinaryArchive to search for compiled code when creating the pipeline state.
--
-- Accelerate pipeline state creation by providing archives of compiled code such that no compilation needs to happen on the fast path.
--
-- See: MTLBinaryArchive
--
-- ObjC selector: @- binaryArchives@
binaryArchives :: IsMTLRenderPipelineDescriptor mtlRenderPipelineDescriptor => mtlRenderPipelineDescriptor -> IO (Id NSArray)
binaryArchives mtlRenderPipelineDescriptor =
  sendMessage mtlRenderPipelineDescriptor binaryArchivesSelector

-- | binaryArchives
--
-- The set of MTLBinaryArchive to search for compiled code when creating the pipeline state.
--
-- Accelerate pipeline state creation by providing archives of compiled code such that no compilation needs to happen on the fast path.
--
-- See: MTLBinaryArchive
--
-- ObjC selector: @- setBinaryArchives:@
setBinaryArchives :: (IsMTLRenderPipelineDescriptor mtlRenderPipelineDescriptor, IsNSArray value) => mtlRenderPipelineDescriptor -> value -> IO ()
setBinaryArchives mtlRenderPipelineDescriptor value =
  sendMessage mtlRenderPipelineDescriptor setBinaryArchivesSelector (toNSArray value)

-- | vertexPreloadedLibraries
--
-- The set of MTLDynamicLibrary to use to resolve external symbols for the vertexFunction before considering symbols from dependent MTLDynamicLibrary.
--
-- Typical workflows use the libraries property of MTLCompileOptions to record dependent libraries at compile time without having to use vertexPreloadedLibraries. This property can be used to override symbols from dependent libraries for experimentation or evaluating alternative implementations. It can also be used to provide dynamic libraries that are dynamically created (for example, from source) that have no stable installName that can be used to automatically load from the file system.
--
-- See: MTLDynamicLibrary
--
-- ObjC selector: @- vertexPreloadedLibraries@
vertexPreloadedLibraries :: IsMTLRenderPipelineDescriptor mtlRenderPipelineDescriptor => mtlRenderPipelineDescriptor -> IO (Id NSArray)
vertexPreloadedLibraries mtlRenderPipelineDescriptor =
  sendMessage mtlRenderPipelineDescriptor vertexPreloadedLibrariesSelector

-- | vertexPreloadedLibraries
--
-- The set of MTLDynamicLibrary to use to resolve external symbols for the vertexFunction before considering symbols from dependent MTLDynamicLibrary.
--
-- Typical workflows use the libraries property of MTLCompileOptions to record dependent libraries at compile time without having to use vertexPreloadedLibraries. This property can be used to override symbols from dependent libraries for experimentation or evaluating alternative implementations. It can also be used to provide dynamic libraries that are dynamically created (for example, from source) that have no stable installName that can be used to automatically load from the file system.
--
-- See: MTLDynamicLibrary
--
-- ObjC selector: @- setVertexPreloadedLibraries:@
setVertexPreloadedLibraries :: (IsMTLRenderPipelineDescriptor mtlRenderPipelineDescriptor, IsNSArray value) => mtlRenderPipelineDescriptor -> value -> IO ()
setVertexPreloadedLibraries mtlRenderPipelineDescriptor value =
  sendMessage mtlRenderPipelineDescriptor setVertexPreloadedLibrariesSelector (toNSArray value)

-- | fragmentPreloadedLibraries
--
-- The set of MTLDynamicLibrary to use to resolve external symbols for the fragmentFunction before considering symbols from dependent MTLDynamicLibrary.
--
-- Typical workflows use the libraries property of MTLCompileOptions to record dependent libraries at compile time without having to use fragmentPreloadedLibraries. This property can be used to override symbols from dependent libraries for experimentation or evaluating alternative implementations. It can also be used to provide dynamic libraries that are dynamically created (for example, from source) that have no stable installName that can be used to automatically load from the file system.
--
-- See: MTLDynamicLibrary
--
-- ObjC selector: @- fragmentPreloadedLibraries@
fragmentPreloadedLibraries :: IsMTLRenderPipelineDescriptor mtlRenderPipelineDescriptor => mtlRenderPipelineDescriptor -> IO (Id NSArray)
fragmentPreloadedLibraries mtlRenderPipelineDescriptor =
  sendMessage mtlRenderPipelineDescriptor fragmentPreloadedLibrariesSelector

-- | fragmentPreloadedLibraries
--
-- The set of MTLDynamicLibrary to use to resolve external symbols for the fragmentFunction before considering symbols from dependent MTLDynamicLibrary.
--
-- Typical workflows use the libraries property of MTLCompileOptions to record dependent libraries at compile time without having to use fragmentPreloadedLibraries. This property can be used to override symbols from dependent libraries for experimentation or evaluating alternative implementations. It can also be used to provide dynamic libraries that are dynamically created (for example, from source) that have no stable installName that can be used to automatically load from the file system.
--
-- See: MTLDynamicLibrary
--
-- ObjC selector: @- setFragmentPreloadedLibraries:@
setFragmentPreloadedLibraries :: (IsMTLRenderPipelineDescriptor mtlRenderPipelineDescriptor, IsNSArray value) => mtlRenderPipelineDescriptor -> value -> IO ()
setFragmentPreloadedLibraries mtlRenderPipelineDescriptor value =
  sendMessage mtlRenderPipelineDescriptor setFragmentPreloadedLibrariesSelector (toNSArray value)

-- | vertexLinkedFunctions
--
-- The set of functions to be linked with the pipeline state and accessed from the vertex function.
--
-- See: MTLLinkedFunctions
--
-- ObjC selector: @- vertexLinkedFunctions@
vertexLinkedFunctions :: IsMTLRenderPipelineDescriptor mtlRenderPipelineDescriptor => mtlRenderPipelineDescriptor -> IO (Id MTLLinkedFunctions)
vertexLinkedFunctions mtlRenderPipelineDescriptor =
  sendMessage mtlRenderPipelineDescriptor vertexLinkedFunctionsSelector

-- | vertexLinkedFunctions
--
-- The set of functions to be linked with the pipeline state and accessed from the vertex function.
--
-- See: MTLLinkedFunctions
--
-- ObjC selector: @- setVertexLinkedFunctions:@
setVertexLinkedFunctions :: (IsMTLRenderPipelineDescriptor mtlRenderPipelineDescriptor, IsMTLLinkedFunctions value) => mtlRenderPipelineDescriptor -> value -> IO ()
setVertexLinkedFunctions mtlRenderPipelineDescriptor value =
  sendMessage mtlRenderPipelineDescriptor setVertexLinkedFunctionsSelector (toMTLLinkedFunctions value)

-- | fragmentLinkedFunctions
--
-- The set of functions to be linked with the pipeline state and accessed from the fragment function.
--
-- See: MTLLinkedFunctions
--
-- ObjC selector: @- fragmentLinkedFunctions@
fragmentLinkedFunctions :: IsMTLRenderPipelineDescriptor mtlRenderPipelineDescriptor => mtlRenderPipelineDescriptor -> IO (Id MTLLinkedFunctions)
fragmentLinkedFunctions mtlRenderPipelineDescriptor =
  sendMessage mtlRenderPipelineDescriptor fragmentLinkedFunctionsSelector

-- | fragmentLinkedFunctions
--
-- The set of functions to be linked with the pipeline state and accessed from the fragment function.
--
-- See: MTLLinkedFunctions
--
-- ObjC selector: @- setFragmentLinkedFunctions:@
setFragmentLinkedFunctions :: (IsMTLRenderPipelineDescriptor mtlRenderPipelineDescriptor, IsMTLLinkedFunctions value) => mtlRenderPipelineDescriptor -> value -> IO ()
setFragmentLinkedFunctions mtlRenderPipelineDescriptor value =
  sendMessage mtlRenderPipelineDescriptor setFragmentLinkedFunctionsSelector (toMTLLinkedFunctions value)

-- | supportAddingVertexBinaryFunctions
--
-- This flag makes this pipeline support creating a new pipeline by adding binary functions.
--
-- ObjC selector: @- supportAddingVertexBinaryFunctions@
supportAddingVertexBinaryFunctions :: IsMTLRenderPipelineDescriptor mtlRenderPipelineDescriptor => mtlRenderPipelineDescriptor -> IO Bool
supportAddingVertexBinaryFunctions mtlRenderPipelineDescriptor =
  sendMessage mtlRenderPipelineDescriptor supportAddingVertexBinaryFunctionsSelector

-- | supportAddingVertexBinaryFunctions
--
-- This flag makes this pipeline support creating a new pipeline by adding binary functions.
--
-- ObjC selector: @- setSupportAddingVertexBinaryFunctions:@
setSupportAddingVertexBinaryFunctions :: IsMTLRenderPipelineDescriptor mtlRenderPipelineDescriptor => mtlRenderPipelineDescriptor -> Bool -> IO ()
setSupportAddingVertexBinaryFunctions mtlRenderPipelineDescriptor value =
  sendMessage mtlRenderPipelineDescriptor setSupportAddingVertexBinaryFunctionsSelector value

-- | supportFragmentAddingBinaryFunctions
--
-- This flag makes this pipeline support creating a new pipeline by adding binary functions.
--
-- ObjC selector: @- supportAddingFragmentBinaryFunctions@
supportAddingFragmentBinaryFunctions :: IsMTLRenderPipelineDescriptor mtlRenderPipelineDescriptor => mtlRenderPipelineDescriptor -> IO Bool
supportAddingFragmentBinaryFunctions mtlRenderPipelineDescriptor =
  sendMessage mtlRenderPipelineDescriptor supportAddingFragmentBinaryFunctionsSelector

-- | supportFragmentAddingBinaryFunctions
--
-- This flag makes this pipeline support creating a new pipeline by adding binary functions.
--
-- ObjC selector: @- setSupportAddingFragmentBinaryFunctions:@
setSupportAddingFragmentBinaryFunctions :: IsMTLRenderPipelineDescriptor mtlRenderPipelineDescriptor => mtlRenderPipelineDescriptor -> Bool -> IO ()
setSupportAddingFragmentBinaryFunctions mtlRenderPipelineDescriptor value =
  sendMessage mtlRenderPipelineDescriptor setSupportAddingFragmentBinaryFunctionsSelector value

-- | maxVertexCallStackDepth
--
-- The maximum depth of the call stack in stack frames from the shader. Defaults to 1 additional stack frame.
--
-- ObjC selector: @- maxVertexCallStackDepth@
maxVertexCallStackDepth :: IsMTLRenderPipelineDescriptor mtlRenderPipelineDescriptor => mtlRenderPipelineDescriptor -> IO CULong
maxVertexCallStackDepth mtlRenderPipelineDescriptor =
  sendMessage mtlRenderPipelineDescriptor maxVertexCallStackDepthSelector

-- | maxVertexCallStackDepth
--
-- The maximum depth of the call stack in stack frames from the shader. Defaults to 1 additional stack frame.
--
-- ObjC selector: @- setMaxVertexCallStackDepth:@
setMaxVertexCallStackDepth :: IsMTLRenderPipelineDescriptor mtlRenderPipelineDescriptor => mtlRenderPipelineDescriptor -> CULong -> IO ()
setMaxVertexCallStackDepth mtlRenderPipelineDescriptor value =
  sendMessage mtlRenderPipelineDescriptor setMaxVertexCallStackDepthSelector value

-- | maxFragmentCallStackDepth
--
-- The maximum depth of the call stack in stack frames from the shader. Defaults to 1 additional stack frame.
--
-- ObjC selector: @- maxFragmentCallStackDepth@
maxFragmentCallStackDepth :: IsMTLRenderPipelineDescriptor mtlRenderPipelineDescriptor => mtlRenderPipelineDescriptor -> IO CULong
maxFragmentCallStackDepth mtlRenderPipelineDescriptor =
  sendMessage mtlRenderPipelineDescriptor maxFragmentCallStackDepthSelector

-- | maxFragmentCallStackDepth
--
-- The maximum depth of the call stack in stack frames from the shader. Defaults to 1 additional stack frame.
--
-- ObjC selector: @- setMaxFragmentCallStackDepth:@
setMaxFragmentCallStackDepth :: IsMTLRenderPipelineDescriptor mtlRenderPipelineDescriptor => mtlRenderPipelineDescriptor -> CULong -> IO ()
setMaxFragmentCallStackDepth mtlRenderPipelineDescriptor value =
  sendMessage mtlRenderPipelineDescriptor setMaxFragmentCallStackDepthSelector value

-- | shaderValidation
--
-- Toggle that determines whether Metal Shader Validation should be enabled or disabled for the pipeline.
--
-- The value can be overridden using @MTL_SHADER_VALIDATION_ENABLE_PIPELINES@ or @MTL_SHADER_VALIDATION_DISABLE_PIPELINES@ Environment Variables.
--
-- ObjC selector: @- shaderValidation@
shaderValidation :: IsMTLRenderPipelineDescriptor mtlRenderPipelineDescriptor => mtlRenderPipelineDescriptor -> IO MTLShaderValidation
shaderValidation mtlRenderPipelineDescriptor =
  sendMessage mtlRenderPipelineDescriptor shaderValidationSelector

-- | shaderValidation
--
-- Toggle that determines whether Metal Shader Validation should be enabled or disabled for the pipeline.
--
-- The value can be overridden using @MTL_SHADER_VALIDATION_ENABLE_PIPELINES@ or @MTL_SHADER_VALIDATION_DISABLE_PIPELINES@ Environment Variables.
--
-- ObjC selector: @- setShaderValidation:@
setShaderValidation :: IsMTLRenderPipelineDescriptor mtlRenderPipelineDescriptor => mtlRenderPipelineDescriptor -> MTLShaderValidation -> IO ()
setShaderValidation mtlRenderPipelineDescriptor value =
  sendMessage mtlRenderPipelineDescriptor setShaderValidationSelector value

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @reset@
resetSelector :: Selector '[] ()
resetSelector = mkSelector "reset"

-- | @Selector@ for @label@
labelSelector :: Selector '[] (Id NSString)
labelSelector = mkSelector "label"

-- | @Selector@ for @setLabel:@
setLabelSelector :: Selector '[Id NSString] ()
setLabelSelector = mkSelector "setLabel:"

-- | @Selector@ for @vertexFunction@
vertexFunctionSelector :: Selector '[] RawId
vertexFunctionSelector = mkSelector "vertexFunction"

-- | @Selector@ for @setVertexFunction:@
setVertexFunctionSelector :: Selector '[RawId] ()
setVertexFunctionSelector = mkSelector "setVertexFunction:"

-- | @Selector@ for @fragmentFunction@
fragmentFunctionSelector :: Selector '[] RawId
fragmentFunctionSelector = mkSelector "fragmentFunction"

-- | @Selector@ for @setFragmentFunction:@
setFragmentFunctionSelector :: Selector '[RawId] ()
setFragmentFunctionSelector = mkSelector "setFragmentFunction:"

-- | @Selector@ for @vertexDescriptor@
vertexDescriptorSelector :: Selector '[] (Id MTLVertexDescriptor)
vertexDescriptorSelector = mkSelector "vertexDescriptor"

-- | @Selector@ for @setVertexDescriptor:@
setVertexDescriptorSelector :: Selector '[Id MTLVertexDescriptor] ()
setVertexDescriptorSelector = mkSelector "setVertexDescriptor:"

-- | @Selector@ for @sampleCount@
sampleCountSelector :: Selector '[] CULong
sampleCountSelector = mkSelector "sampleCount"

-- | @Selector@ for @setSampleCount:@
setSampleCountSelector :: Selector '[CULong] ()
setSampleCountSelector = mkSelector "setSampleCount:"

-- | @Selector@ for @rasterSampleCount@
rasterSampleCountSelector :: Selector '[] CULong
rasterSampleCountSelector = mkSelector "rasterSampleCount"

-- | @Selector@ for @setRasterSampleCount:@
setRasterSampleCountSelector :: Selector '[CULong] ()
setRasterSampleCountSelector = mkSelector "setRasterSampleCount:"

-- | @Selector@ for @alphaToCoverageEnabled@
alphaToCoverageEnabledSelector :: Selector '[] Bool
alphaToCoverageEnabledSelector = mkSelector "alphaToCoverageEnabled"

-- | @Selector@ for @setAlphaToCoverageEnabled:@
setAlphaToCoverageEnabledSelector :: Selector '[Bool] ()
setAlphaToCoverageEnabledSelector = mkSelector "setAlphaToCoverageEnabled:"

-- | @Selector@ for @alphaToOneEnabled@
alphaToOneEnabledSelector :: Selector '[] Bool
alphaToOneEnabledSelector = mkSelector "alphaToOneEnabled"

-- | @Selector@ for @setAlphaToOneEnabled:@
setAlphaToOneEnabledSelector :: Selector '[Bool] ()
setAlphaToOneEnabledSelector = mkSelector "setAlphaToOneEnabled:"

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
colorAttachmentsSelector :: Selector '[] (Id MTLRenderPipelineColorAttachmentDescriptorArray)
colorAttachmentsSelector = mkSelector "colorAttachments"

-- | @Selector@ for @depthAttachmentPixelFormat@
depthAttachmentPixelFormatSelector :: Selector '[] MTLPixelFormat
depthAttachmentPixelFormatSelector = mkSelector "depthAttachmentPixelFormat"

-- | @Selector@ for @setDepthAttachmentPixelFormat:@
setDepthAttachmentPixelFormatSelector :: Selector '[MTLPixelFormat] ()
setDepthAttachmentPixelFormatSelector = mkSelector "setDepthAttachmentPixelFormat:"

-- | @Selector@ for @stencilAttachmentPixelFormat@
stencilAttachmentPixelFormatSelector :: Selector '[] MTLPixelFormat
stencilAttachmentPixelFormatSelector = mkSelector "stencilAttachmentPixelFormat"

-- | @Selector@ for @setStencilAttachmentPixelFormat:@
setStencilAttachmentPixelFormatSelector :: Selector '[MTLPixelFormat] ()
setStencilAttachmentPixelFormatSelector = mkSelector "setStencilAttachmentPixelFormat:"

-- | @Selector@ for @inputPrimitiveTopology@
inputPrimitiveTopologySelector :: Selector '[] MTLPrimitiveTopologyClass
inputPrimitiveTopologySelector = mkSelector "inputPrimitiveTopology"

-- | @Selector@ for @setInputPrimitiveTopology:@
setInputPrimitiveTopologySelector :: Selector '[MTLPrimitiveTopologyClass] ()
setInputPrimitiveTopologySelector = mkSelector "setInputPrimitiveTopology:"

-- | @Selector@ for @tessellationPartitionMode@
tessellationPartitionModeSelector :: Selector '[] MTLTessellationPartitionMode
tessellationPartitionModeSelector = mkSelector "tessellationPartitionMode"

-- | @Selector@ for @setTessellationPartitionMode:@
setTessellationPartitionModeSelector :: Selector '[MTLTessellationPartitionMode] ()
setTessellationPartitionModeSelector = mkSelector "setTessellationPartitionMode:"

-- | @Selector@ for @maxTessellationFactor@
maxTessellationFactorSelector :: Selector '[] CULong
maxTessellationFactorSelector = mkSelector "maxTessellationFactor"

-- | @Selector@ for @setMaxTessellationFactor:@
setMaxTessellationFactorSelector :: Selector '[CULong] ()
setMaxTessellationFactorSelector = mkSelector "setMaxTessellationFactor:"

-- | @Selector@ for @tessellationFactorScaleEnabled@
tessellationFactorScaleEnabledSelector :: Selector '[] Bool
tessellationFactorScaleEnabledSelector = mkSelector "tessellationFactorScaleEnabled"

-- | @Selector@ for @setTessellationFactorScaleEnabled:@
setTessellationFactorScaleEnabledSelector :: Selector '[Bool] ()
setTessellationFactorScaleEnabledSelector = mkSelector "setTessellationFactorScaleEnabled:"

-- | @Selector@ for @tessellationFactorFormat@
tessellationFactorFormatSelector :: Selector '[] MTLTessellationFactorFormat
tessellationFactorFormatSelector = mkSelector "tessellationFactorFormat"

-- | @Selector@ for @setTessellationFactorFormat:@
setTessellationFactorFormatSelector :: Selector '[MTLTessellationFactorFormat] ()
setTessellationFactorFormatSelector = mkSelector "setTessellationFactorFormat:"

-- | @Selector@ for @tessellationControlPointIndexType@
tessellationControlPointIndexTypeSelector :: Selector '[] MTLTessellationControlPointIndexType
tessellationControlPointIndexTypeSelector = mkSelector "tessellationControlPointIndexType"

-- | @Selector@ for @setTessellationControlPointIndexType:@
setTessellationControlPointIndexTypeSelector :: Selector '[MTLTessellationControlPointIndexType] ()
setTessellationControlPointIndexTypeSelector = mkSelector "setTessellationControlPointIndexType:"

-- | @Selector@ for @tessellationFactorStepFunction@
tessellationFactorStepFunctionSelector :: Selector '[] MTLTessellationFactorStepFunction
tessellationFactorStepFunctionSelector = mkSelector "tessellationFactorStepFunction"

-- | @Selector@ for @setTessellationFactorStepFunction:@
setTessellationFactorStepFunctionSelector :: Selector '[MTLTessellationFactorStepFunction] ()
setTessellationFactorStepFunctionSelector = mkSelector "setTessellationFactorStepFunction:"

-- | @Selector@ for @tessellationOutputWindingOrder@
tessellationOutputWindingOrderSelector :: Selector '[] MTLWinding
tessellationOutputWindingOrderSelector = mkSelector "tessellationOutputWindingOrder"

-- | @Selector@ for @setTessellationOutputWindingOrder:@
setTessellationOutputWindingOrderSelector :: Selector '[MTLWinding] ()
setTessellationOutputWindingOrderSelector = mkSelector "setTessellationOutputWindingOrder:"

-- | @Selector@ for @vertexBuffers@
vertexBuffersSelector :: Selector '[] (Id MTLPipelineBufferDescriptorArray)
vertexBuffersSelector = mkSelector "vertexBuffers"

-- | @Selector@ for @fragmentBuffers@
fragmentBuffersSelector :: Selector '[] (Id MTLPipelineBufferDescriptorArray)
fragmentBuffersSelector = mkSelector "fragmentBuffers"

-- | @Selector@ for @supportIndirectCommandBuffers@
supportIndirectCommandBuffersSelector :: Selector '[] Bool
supportIndirectCommandBuffersSelector = mkSelector "supportIndirectCommandBuffers"

-- | @Selector@ for @setSupportIndirectCommandBuffers:@
setSupportIndirectCommandBuffersSelector :: Selector '[Bool] ()
setSupportIndirectCommandBuffersSelector = mkSelector "setSupportIndirectCommandBuffers:"

-- | @Selector@ for @binaryArchives@
binaryArchivesSelector :: Selector '[] (Id NSArray)
binaryArchivesSelector = mkSelector "binaryArchives"

-- | @Selector@ for @setBinaryArchives:@
setBinaryArchivesSelector :: Selector '[Id NSArray] ()
setBinaryArchivesSelector = mkSelector "setBinaryArchives:"

-- | @Selector@ for @vertexPreloadedLibraries@
vertexPreloadedLibrariesSelector :: Selector '[] (Id NSArray)
vertexPreloadedLibrariesSelector = mkSelector "vertexPreloadedLibraries"

-- | @Selector@ for @setVertexPreloadedLibraries:@
setVertexPreloadedLibrariesSelector :: Selector '[Id NSArray] ()
setVertexPreloadedLibrariesSelector = mkSelector "setVertexPreloadedLibraries:"

-- | @Selector@ for @fragmentPreloadedLibraries@
fragmentPreloadedLibrariesSelector :: Selector '[] (Id NSArray)
fragmentPreloadedLibrariesSelector = mkSelector "fragmentPreloadedLibraries"

-- | @Selector@ for @setFragmentPreloadedLibraries:@
setFragmentPreloadedLibrariesSelector :: Selector '[Id NSArray] ()
setFragmentPreloadedLibrariesSelector = mkSelector "setFragmentPreloadedLibraries:"

-- | @Selector@ for @vertexLinkedFunctions@
vertexLinkedFunctionsSelector :: Selector '[] (Id MTLLinkedFunctions)
vertexLinkedFunctionsSelector = mkSelector "vertexLinkedFunctions"

-- | @Selector@ for @setVertexLinkedFunctions:@
setVertexLinkedFunctionsSelector :: Selector '[Id MTLLinkedFunctions] ()
setVertexLinkedFunctionsSelector = mkSelector "setVertexLinkedFunctions:"

-- | @Selector@ for @fragmentLinkedFunctions@
fragmentLinkedFunctionsSelector :: Selector '[] (Id MTLLinkedFunctions)
fragmentLinkedFunctionsSelector = mkSelector "fragmentLinkedFunctions"

-- | @Selector@ for @setFragmentLinkedFunctions:@
setFragmentLinkedFunctionsSelector :: Selector '[Id MTLLinkedFunctions] ()
setFragmentLinkedFunctionsSelector = mkSelector "setFragmentLinkedFunctions:"

-- | @Selector@ for @supportAddingVertexBinaryFunctions@
supportAddingVertexBinaryFunctionsSelector :: Selector '[] Bool
supportAddingVertexBinaryFunctionsSelector = mkSelector "supportAddingVertexBinaryFunctions"

-- | @Selector@ for @setSupportAddingVertexBinaryFunctions:@
setSupportAddingVertexBinaryFunctionsSelector :: Selector '[Bool] ()
setSupportAddingVertexBinaryFunctionsSelector = mkSelector "setSupportAddingVertexBinaryFunctions:"

-- | @Selector@ for @supportAddingFragmentBinaryFunctions@
supportAddingFragmentBinaryFunctionsSelector :: Selector '[] Bool
supportAddingFragmentBinaryFunctionsSelector = mkSelector "supportAddingFragmentBinaryFunctions"

-- | @Selector@ for @setSupportAddingFragmentBinaryFunctions:@
setSupportAddingFragmentBinaryFunctionsSelector :: Selector '[Bool] ()
setSupportAddingFragmentBinaryFunctionsSelector = mkSelector "setSupportAddingFragmentBinaryFunctions:"

-- | @Selector@ for @maxVertexCallStackDepth@
maxVertexCallStackDepthSelector :: Selector '[] CULong
maxVertexCallStackDepthSelector = mkSelector "maxVertexCallStackDepth"

-- | @Selector@ for @setMaxVertexCallStackDepth:@
setMaxVertexCallStackDepthSelector :: Selector '[CULong] ()
setMaxVertexCallStackDepthSelector = mkSelector "setMaxVertexCallStackDepth:"

-- | @Selector@ for @maxFragmentCallStackDepth@
maxFragmentCallStackDepthSelector :: Selector '[] CULong
maxFragmentCallStackDepthSelector = mkSelector "maxFragmentCallStackDepth"

-- | @Selector@ for @setMaxFragmentCallStackDepth:@
setMaxFragmentCallStackDepthSelector :: Selector '[CULong] ()
setMaxFragmentCallStackDepthSelector = mkSelector "setMaxFragmentCallStackDepth:"

-- | @Selector@ for @shaderValidation@
shaderValidationSelector :: Selector '[] MTLShaderValidation
shaderValidationSelector = mkSelector "shaderValidation"

-- | @Selector@ for @setShaderValidation:@
setShaderValidationSelector :: Selector '[MTLShaderValidation] ()
setShaderValidationSelector = mkSelector "setShaderValidation:"

