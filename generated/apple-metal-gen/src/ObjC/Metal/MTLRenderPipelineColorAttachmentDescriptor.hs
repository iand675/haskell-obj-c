{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Generated bindings for @MTLRenderPipelineColorAttachmentDescriptor@.
module ObjC.Metal.MTLRenderPipelineColorAttachmentDescriptor
  ( MTLRenderPipelineColorAttachmentDescriptor
  , IsMTLRenderPipelineColorAttachmentDescriptor(..)
  , pixelFormat
  , setPixelFormat
  , blendingEnabled
  , setBlendingEnabled
  , sourceRGBBlendFactor
  , setSourceRGBBlendFactor
  , destinationRGBBlendFactor
  , setDestinationRGBBlendFactor
  , rgbBlendOperation
  , setRgbBlendOperation
  , sourceAlphaBlendFactor
  , setSourceAlphaBlendFactor
  , destinationAlphaBlendFactor
  , setDestinationAlphaBlendFactor
  , alphaBlendOperation
  , setAlphaBlendOperation
  , writeMask
  , setWriteMask
  , alphaBlendOperationSelector
  , blendingEnabledSelector
  , destinationAlphaBlendFactorSelector
  , destinationRGBBlendFactorSelector
  , pixelFormatSelector
  , rgbBlendOperationSelector
  , setAlphaBlendOperationSelector
  , setBlendingEnabledSelector
  , setDestinationAlphaBlendFactorSelector
  , setDestinationRGBBlendFactorSelector
  , setPixelFormatSelector
  , setRgbBlendOperationSelector
  , setSourceAlphaBlendFactorSelector
  , setSourceRGBBlendFactorSelector
  , setWriteMaskSelector
  , sourceAlphaBlendFactorSelector
  , sourceRGBBlendFactorSelector
  , writeMaskSelector

  -- * Enum types
  , MTLBlendFactor(MTLBlendFactor)
  , pattern MTLBlendFactorZero
  , pattern MTLBlendFactorOne
  , pattern MTLBlendFactorSourceColor
  , pattern MTLBlendFactorOneMinusSourceColor
  , pattern MTLBlendFactorSourceAlpha
  , pattern MTLBlendFactorOneMinusSourceAlpha
  , pattern MTLBlendFactorDestinationColor
  , pattern MTLBlendFactorOneMinusDestinationColor
  , pattern MTLBlendFactorDestinationAlpha
  , pattern MTLBlendFactorOneMinusDestinationAlpha
  , pattern MTLBlendFactorSourceAlphaSaturated
  , pattern MTLBlendFactorBlendColor
  , pattern MTLBlendFactorOneMinusBlendColor
  , pattern MTLBlendFactorBlendAlpha
  , pattern MTLBlendFactorOneMinusBlendAlpha
  , pattern MTLBlendFactorSource1Color
  , pattern MTLBlendFactorOneMinusSource1Color
  , pattern MTLBlendFactorSource1Alpha
  , pattern MTLBlendFactorOneMinusSource1Alpha
  , pattern MTLBlendFactorUnspecialized
  , MTLBlendOperation(MTLBlendOperation)
  , pattern MTLBlendOperationAdd
  , pattern MTLBlendOperationSubtract
  , pattern MTLBlendOperationReverseSubtract
  , pattern MTLBlendOperationMin
  , pattern MTLBlendOperationMax
  , pattern MTLBlendOperationUnspecialized
  , MTLColorWriteMask(MTLColorWriteMask)
  , pattern MTLColorWriteMaskNone
  , pattern MTLColorWriteMaskRed
  , pattern MTLColorWriteMaskGreen
  , pattern MTLColorWriteMaskBlue
  , pattern MTLColorWriteMaskAlpha
  , pattern MTLColorWriteMaskAll
  , pattern MTLColorWriteMaskUnspecialized
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

-- | Pixel format.  Defaults to MTLPixelFormatInvalid
--
-- ObjC selector: @- pixelFormat@
pixelFormat :: IsMTLRenderPipelineColorAttachmentDescriptor mtlRenderPipelineColorAttachmentDescriptor => mtlRenderPipelineColorAttachmentDescriptor -> IO MTLPixelFormat
pixelFormat mtlRenderPipelineColorAttachmentDescriptor =
  sendMessage mtlRenderPipelineColorAttachmentDescriptor pixelFormatSelector

-- | Pixel format.  Defaults to MTLPixelFormatInvalid
--
-- ObjC selector: @- setPixelFormat:@
setPixelFormat :: IsMTLRenderPipelineColorAttachmentDescriptor mtlRenderPipelineColorAttachmentDescriptor => mtlRenderPipelineColorAttachmentDescriptor -> MTLPixelFormat -> IO ()
setPixelFormat mtlRenderPipelineColorAttachmentDescriptor value =
  sendMessage mtlRenderPipelineColorAttachmentDescriptor setPixelFormatSelector value

-- | Enable blending.  Defaults to NO.
--
-- ObjC selector: @- blendingEnabled@
blendingEnabled :: IsMTLRenderPipelineColorAttachmentDescriptor mtlRenderPipelineColorAttachmentDescriptor => mtlRenderPipelineColorAttachmentDescriptor -> IO Bool
blendingEnabled mtlRenderPipelineColorAttachmentDescriptor =
  sendMessage mtlRenderPipelineColorAttachmentDescriptor blendingEnabledSelector

-- | Enable blending.  Defaults to NO.
--
-- ObjC selector: @- setBlendingEnabled:@
setBlendingEnabled :: IsMTLRenderPipelineColorAttachmentDescriptor mtlRenderPipelineColorAttachmentDescriptor => mtlRenderPipelineColorAttachmentDescriptor -> Bool -> IO ()
setBlendingEnabled mtlRenderPipelineColorAttachmentDescriptor value =
  sendMessage mtlRenderPipelineColorAttachmentDescriptor setBlendingEnabledSelector value

-- | Defaults to MTLBlendFactorOne
--
-- ObjC selector: @- sourceRGBBlendFactor@
sourceRGBBlendFactor :: IsMTLRenderPipelineColorAttachmentDescriptor mtlRenderPipelineColorAttachmentDescriptor => mtlRenderPipelineColorAttachmentDescriptor -> IO MTLBlendFactor
sourceRGBBlendFactor mtlRenderPipelineColorAttachmentDescriptor =
  sendMessage mtlRenderPipelineColorAttachmentDescriptor sourceRGBBlendFactorSelector

-- | Defaults to MTLBlendFactorOne
--
-- ObjC selector: @- setSourceRGBBlendFactor:@
setSourceRGBBlendFactor :: IsMTLRenderPipelineColorAttachmentDescriptor mtlRenderPipelineColorAttachmentDescriptor => mtlRenderPipelineColorAttachmentDescriptor -> MTLBlendFactor -> IO ()
setSourceRGBBlendFactor mtlRenderPipelineColorAttachmentDescriptor value =
  sendMessage mtlRenderPipelineColorAttachmentDescriptor setSourceRGBBlendFactorSelector value

-- | Defaults to MTLBlendFactorZero
--
-- ObjC selector: @- destinationRGBBlendFactor@
destinationRGBBlendFactor :: IsMTLRenderPipelineColorAttachmentDescriptor mtlRenderPipelineColorAttachmentDescriptor => mtlRenderPipelineColorAttachmentDescriptor -> IO MTLBlendFactor
destinationRGBBlendFactor mtlRenderPipelineColorAttachmentDescriptor =
  sendMessage mtlRenderPipelineColorAttachmentDescriptor destinationRGBBlendFactorSelector

-- | Defaults to MTLBlendFactorZero
--
-- ObjC selector: @- setDestinationRGBBlendFactor:@
setDestinationRGBBlendFactor :: IsMTLRenderPipelineColorAttachmentDescriptor mtlRenderPipelineColorAttachmentDescriptor => mtlRenderPipelineColorAttachmentDescriptor -> MTLBlendFactor -> IO ()
setDestinationRGBBlendFactor mtlRenderPipelineColorAttachmentDescriptor value =
  sendMessage mtlRenderPipelineColorAttachmentDescriptor setDestinationRGBBlendFactorSelector value

-- | Defaults to MTLBlendOperationAdd
--
-- ObjC selector: @- rgbBlendOperation@
rgbBlendOperation :: IsMTLRenderPipelineColorAttachmentDescriptor mtlRenderPipelineColorAttachmentDescriptor => mtlRenderPipelineColorAttachmentDescriptor -> IO MTLBlendOperation
rgbBlendOperation mtlRenderPipelineColorAttachmentDescriptor =
  sendMessage mtlRenderPipelineColorAttachmentDescriptor rgbBlendOperationSelector

-- | Defaults to MTLBlendOperationAdd
--
-- ObjC selector: @- setRgbBlendOperation:@
setRgbBlendOperation :: IsMTLRenderPipelineColorAttachmentDescriptor mtlRenderPipelineColorAttachmentDescriptor => mtlRenderPipelineColorAttachmentDescriptor -> MTLBlendOperation -> IO ()
setRgbBlendOperation mtlRenderPipelineColorAttachmentDescriptor value =
  sendMessage mtlRenderPipelineColorAttachmentDescriptor setRgbBlendOperationSelector value

-- | Defaults to MTLBlendFactorOne
--
-- ObjC selector: @- sourceAlphaBlendFactor@
sourceAlphaBlendFactor :: IsMTLRenderPipelineColorAttachmentDescriptor mtlRenderPipelineColorAttachmentDescriptor => mtlRenderPipelineColorAttachmentDescriptor -> IO MTLBlendFactor
sourceAlphaBlendFactor mtlRenderPipelineColorAttachmentDescriptor =
  sendMessage mtlRenderPipelineColorAttachmentDescriptor sourceAlphaBlendFactorSelector

-- | Defaults to MTLBlendFactorOne
--
-- ObjC selector: @- setSourceAlphaBlendFactor:@
setSourceAlphaBlendFactor :: IsMTLRenderPipelineColorAttachmentDescriptor mtlRenderPipelineColorAttachmentDescriptor => mtlRenderPipelineColorAttachmentDescriptor -> MTLBlendFactor -> IO ()
setSourceAlphaBlendFactor mtlRenderPipelineColorAttachmentDescriptor value =
  sendMessage mtlRenderPipelineColorAttachmentDescriptor setSourceAlphaBlendFactorSelector value

-- | Defaults to MTLBlendFactorZero
--
-- ObjC selector: @- destinationAlphaBlendFactor@
destinationAlphaBlendFactor :: IsMTLRenderPipelineColorAttachmentDescriptor mtlRenderPipelineColorAttachmentDescriptor => mtlRenderPipelineColorAttachmentDescriptor -> IO MTLBlendFactor
destinationAlphaBlendFactor mtlRenderPipelineColorAttachmentDescriptor =
  sendMessage mtlRenderPipelineColorAttachmentDescriptor destinationAlphaBlendFactorSelector

-- | Defaults to MTLBlendFactorZero
--
-- ObjC selector: @- setDestinationAlphaBlendFactor:@
setDestinationAlphaBlendFactor :: IsMTLRenderPipelineColorAttachmentDescriptor mtlRenderPipelineColorAttachmentDescriptor => mtlRenderPipelineColorAttachmentDescriptor -> MTLBlendFactor -> IO ()
setDestinationAlphaBlendFactor mtlRenderPipelineColorAttachmentDescriptor value =
  sendMessage mtlRenderPipelineColorAttachmentDescriptor setDestinationAlphaBlendFactorSelector value

-- | Defaults to MTLBlendOperationAdd
--
-- ObjC selector: @- alphaBlendOperation@
alphaBlendOperation :: IsMTLRenderPipelineColorAttachmentDescriptor mtlRenderPipelineColorAttachmentDescriptor => mtlRenderPipelineColorAttachmentDescriptor -> IO MTLBlendOperation
alphaBlendOperation mtlRenderPipelineColorAttachmentDescriptor =
  sendMessage mtlRenderPipelineColorAttachmentDescriptor alphaBlendOperationSelector

-- | Defaults to MTLBlendOperationAdd
--
-- ObjC selector: @- setAlphaBlendOperation:@
setAlphaBlendOperation :: IsMTLRenderPipelineColorAttachmentDescriptor mtlRenderPipelineColorAttachmentDescriptor => mtlRenderPipelineColorAttachmentDescriptor -> MTLBlendOperation -> IO ()
setAlphaBlendOperation mtlRenderPipelineColorAttachmentDescriptor value =
  sendMessage mtlRenderPipelineColorAttachmentDescriptor setAlphaBlendOperationSelector value

-- | Defaults to MTLColorWriteMaskAll
--
-- ObjC selector: @- writeMask@
writeMask :: IsMTLRenderPipelineColorAttachmentDescriptor mtlRenderPipelineColorAttachmentDescriptor => mtlRenderPipelineColorAttachmentDescriptor -> IO MTLColorWriteMask
writeMask mtlRenderPipelineColorAttachmentDescriptor =
  sendMessage mtlRenderPipelineColorAttachmentDescriptor writeMaskSelector

-- | Defaults to MTLColorWriteMaskAll
--
-- ObjC selector: @- setWriteMask:@
setWriteMask :: IsMTLRenderPipelineColorAttachmentDescriptor mtlRenderPipelineColorAttachmentDescriptor => mtlRenderPipelineColorAttachmentDescriptor -> MTLColorWriteMask -> IO ()
setWriteMask mtlRenderPipelineColorAttachmentDescriptor value =
  sendMessage mtlRenderPipelineColorAttachmentDescriptor setWriteMaskSelector value

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @pixelFormat@
pixelFormatSelector :: Selector '[] MTLPixelFormat
pixelFormatSelector = mkSelector "pixelFormat"

-- | @Selector@ for @setPixelFormat:@
setPixelFormatSelector :: Selector '[MTLPixelFormat] ()
setPixelFormatSelector = mkSelector "setPixelFormat:"

-- | @Selector@ for @blendingEnabled@
blendingEnabledSelector :: Selector '[] Bool
blendingEnabledSelector = mkSelector "blendingEnabled"

-- | @Selector@ for @setBlendingEnabled:@
setBlendingEnabledSelector :: Selector '[Bool] ()
setBlendingEnabledSelector = mkSelector "setBlendingEnabled:"

-- | @Selector@ for @sourceRGBBlendFactor@
sourceRGBBlendFactorSelector :: Selector '[] MTLBlendFactor
sourceRGBBlendFactorSelector = mkSelector "sourceRGBBlendFactor"

-- | @Selector@ for @setSourceRGBBlendFactor:@
setSourceRGBBlendFactorSelector :: Selector '[MTLBlendFactor] ()
setSourceRGBBlendFactorSelector = mkSelector "setSourceRGBBlendFactor:"

-- | @Selector@ for @destinationRGBBlendFactor@
destinationRGBBlendFactorSelector :: Selector '[] MTLBlendFactor
destinationRGBBlendFactorSelector = mkSelector "destinationRGBBlendFactor"

-- | @Selector@ for @setDestinationRGBBlendFactor:@
setDestinationRGBBlendFactorSelector :: Selector '[MTLBlendFactor] ()
setDestinationRGBBlendFactorSelector = mkSelector "setDestinationRGBBlendFactor:"

-- | @Selector@ for @rgbBlendOperation@
rgbBlendOperationSelector :: Selector '[] MTLBlendOperation
rgbBlendOperationSelector = mkSelector "rgbBlendOperation"

-- | @Selector@ for @setRgbBlendOperation:@
setRgbBlendOperationSelector :: Selector '[MTLBlendOperation] ()
setRgbBlendOperationSelector = mkSelector "setRgbBlendOperation:"

-- | @Selector@ for @sourceAlphaBlendFactor@
sourceAlphaBlendFactorSelector :: Selector '[] MTLBlendFactor
sourceAlphaBlendFactorSelector = mkSelector "sourceAlphaBlendFactor"

-- | @Selector@ for @setSourceAlphaBlendFactor:@
setSourceAlphaBlendFactorSelector :: Selector '[MTLBlendFactor] ()
setSourceAlphaBlendFactorSelector = mkSelector "setSourceAlphaBlendFactor:"

-- | @Selector@ for @destinationAlphaBlendFactor@
destinationAlphaBlendFactorSelector :: Selector '[] MTLBlendFactor
destinationAlphaBlendFactorSelector = mkSelector "destinationAlphaBlendFactor"

-- | @Selector@ for @setDestinationAlphaBlendFactor:@
setDestinationAlphaBlendFactorSelector :: Selector '[MTLBlendFactor] ()
setDestinationAlphaBlendFactorSelector = mkSelector "setDestinationAlphaBlendFactor:"

-- | @Selector@ for @alphaBlendOperation@
alphaBlendOperationSelector :: Selector '[] MTLBlendOperation
alphaBlendOperationSelector = mkSelector "alphaBlendOperation"

-- | @Selector@ for @setAlphaBlendOperation:@
setAlphaBlendOperationSelector :: Selector '[MTLBlendOperation] ()
setAlphaBlendOperationSelector = mkSelector "setAlphaBlendOperation:"

-- | @Selector@ for @writeMask@
writeMaskSelector :: Selector '[] MTLColorWriteMask
writeMaskSelector = mkSelector "writeMask"

-- | @Selector@ for @setWriteMask:@
setWriteMaskSelector :: Selector '[MTLColorWriteMask] ()
setWriteMaskSelector = mkSelector "setWriteMask:"

