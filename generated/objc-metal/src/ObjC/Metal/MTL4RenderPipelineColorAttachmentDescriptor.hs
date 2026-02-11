{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Generated bindings for @MTL4RenderPipelineColorAttachmentDescriptor@.
module ObjC.Metal.MTL4RenderPipelineColorAttachmentDescriptor
  ( MTL4RenderPipelineColorAttachmentDescriptor
  , IsMTL4RenderPipelineColorAttachmentDescriptor(..)
  , reset
  , pixelFormat
  , setPixelFormat
  , blendingState
  , setBlendingState
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
  , resetSelector
  , pixelFormatSelector
  , setPixelFormatSelector
  , blendingStateSelector
  , setBlendingStateSelector
  , sourceRGBBlendFactorSelector
  , setSourceRGBBlendFactorSelector
  , destinationRGBBlendFactorSelector
  , setDestinationRGBBlendFactorSelector
  , rgbBlendOperationSelector
  , setRgbBlendOperationSelector
  , sourceAlphaBlendFactorSelector
  , setSourceAlphaBlendFactorSelector
  , destinationAlphaBlendFactorSelector
  , setDestinationAlphaBlendFactorSelector
  , alphaBlendOperationSelector
  , setAlphaBlendOperationSelector
  , writeMaskSelector
  , setWriteMaskSelector

  -- * Enum types
  , MTL4BlendState(MTL4BlendState)
  , pattern MTL4BlendStateDisabled
  , pattern MTL4BlendStateEnabled
  , pattern MTL4BlendStateUnspecialized
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
reset :: IsMTL4RenderPipelineColorAttachmentDescriptor mtL4RenderPipelineColorAttachmentDescriptor => mtL4RenderPipelineColorAttachmentDescriptor -> IO ()
reset mtL4RenderPipelineColorAttachmentDescriptor  =
  sendMsg mtL4RenderPipelineColorAttachmentDescriptor (mkSelector "reset") retVoid []

-- | Configures the pixel format.
--
-- This property defaults to ``MTLPixelFormatInvalid``.
--
-- ObjC selector: @- pixelFormat@
pixelFormat :: IsMTL4RenderPipelineColorAttachmentDescriptor mtL4RenderPipelineColorAttachmentDescriptor => mtL4RenderPipelineColorAttachmentDescriptor -> IO MTLPixelFormat
pixelFormat mtL4RenderPipelineColorAttachmentDescriptor  =
  fmap (coerce :: CULong -> MTLPixelFormat) $ sendMsg mtL4RenderPipelineColorAttachmentDescriptor (mkSelector "pixelFormat") retCULong []

-- | Configures the pixel format.
--
-- This property defaults to ``MTLPixelFormatInvalid``.
--
-- ObjC selector: @- setPixelFormat:@
setPixelFormat :: IsMTL4RenderPipelineColorAttachmentDescriptor mtL4RenderPipelineColorAttachmentDescriptor => mtL4RenderPipelineColorAttachmentDescriptor -> MTLPixelFormat -> IO ()
setPixelFormat mtL4RenderPipelineColorAttachmentDescriptor  value =
  sendMsg mtL4RenderPipelineColorAttachmentDescriptor (mkSelector "setPixelFormat:") retVoid [argCULong (coerce value)]

-- | Configure the blend state for color attachments the pipeline state uses.
--
-- This property's default value is ``MTL4BlendStateDisabled``.
--
-- ObjC selector: @- blendingState@
blendingState :: IsMTL4RenderPipelineColorAttachmentDescriptor mtL4RenderPipelineColorAttachmentDescriptor => mtL4RenderPipelineColorAttachmentDescriptor -> IO MTL4BlendState
blendingState mtL4RenderPipelineColorAttachmentDescriptor  =
  fmap (coerce :: CLong -> MTL4BlendState) $ sendMsg mtL4RenderPipelineColorAttachmentDescriptor (mkSelector "blendingState") retCLong []

-- | Configure the blend state for color attachments the pipeline state uses.
--
-- This property's default value is ``MTL4BlendStateDisabled``.
--
-- ObjC selector: @- setBlendingState:@
setBlendingState :: IsMTL4RenderPipelineColorAttachmentDescriptor mtL4RenderPipelineColorAttachmentDescriptor => mtL4RenderPipelineColorAttachmentDescriptor -> MTL4BlendState -> IO ()
setBlendingState mtL4RenderPipelineColorAttachmentDescriptor  value =
  sendMsg mtL4RenderPipelineColorAttachmentDescriptor (mkSelector "setBlendingState:") retVoid [argCLong (coerce value)]

-- | Configures the source RGB blend factor.
--
-- This property defaults to ``MTLBlendFactorOne``.
--
-- ObjC selector: @- sourceRGBBlendFactor@
sourceRGBBlendFactor :: IsMTL4RenderPipelineColorAttachmentDescriptor mtL4RenderPipelineColorAttachmentDescriptor => mtL4RenderPipelineColorAttachmentDescriptor -> IO MTLBlendFactor
sourceRGBBlendFactor mtL4RenderPipelineColorAttachmentDescriptor  =
  fmap (coerce :: CULong -> MTLBlendFactor) $ sendMsg mtL4RenderPipelineColorAttachmentDescriptor (mkSelector "sourceRGBBlendFactor") retCULong []

-- | Configures the source RGB blend factor.
--
-- This property defaults to ``MTLBlendFactorOne``.
--
-- ObjC selector: @- setSourceRGBBlendFactor:@
setSourceRGBBlendFactor :: IsMTL4RenderPipelineColorAttachmentDescriptor mtL4RenderPipelineColorAttachmentDescriptor => mtL4RenderPipelineColorAttachmentDescriptor -> MTLBlendFactor -> IO ()
setSourceRGBBlendFactor mtL4RenderPipelineColorAttachmentDescriptor  value =
  sendMsg mtL4RenderPipelineColorAttachmentDescriptor (mkSelector "setSourceRGBBlendFactor:") retVoid [argCULong (coerce value)]

-- | Configures the destination RGB blend factor.
--
-- This property defaults to ``MTLBlendFactorZero``.
--
-- ObjC selector: @- destinationRGBBlendFactor@
destinationRGBBlendFactor :: IsMTL4RenderPipelineColorAttachmentDescriptor mtL4RenderPipelineColorAttachmentDescriptor => mtL4RenderPipelineColorAttachmentDescriptor -> IO MTLBlendFactor
destinationRGBBlendFactor mtL4RenderPipelineColorAttachmentDescriptor  =
  fmap (coerce :: CULong -> MTLBlendFactor) $ sendMsg mtL4RenderPipelineColorAttachmentDescriptor (mkSelector "destinationRGBBlendFactor") retCULong []

-- | Configures the destination RGB blend factor.
--
-- This property defaults to ``MTLBlendFactorZero``.
--
-- ObjC selector: @- setDestinationRGBBlendFactor:@
setDestinationRGBBlendFactor :: IsMTL4RenderPipelineColorAttachmentDescriptor mtL4RenderPipelineColorAttachmentDescriptor => mtL4RenderPipelineColorAttachmentDescriptor -> MTLBlendFactor -> IO ()
setDestinationRGBBlendFactor mtL4RenderPipelineColorAttachmentDescriptor  value =
  sendMsg mtL4RenderPipelineColorAttachmentDescriptor (mkSelector "setDestinationRGBBlendFactor:") retVoid [argCULong (coerce value)]

-- | Configures the RGB blend operation.
--
-- This property defaults to ``MTLBlendOperationAdd``.
--
-- ObjC selector: @- rgbBlendOperation@
rgbBlendOperation :: IsMTL4RenderPipelineColorAttachmentDescriptor mtL4RenderPipelineColorAttachmentDescriptor => mtL4RenderPipelineColorAttachmentDescriptor -> IO MTLBlendOperation
rgbBlendOperation mtL4RenderPipelineColorAttachmentDescriptor  =
  fmap (coerce :: CULong -> MTLBlendOperation) $ sendMsg mtL4RenderPipelineColorAttachmentDescriptor (mkSelector "rgbBlendOperation") retCULong []

-- | Configures the RGB blend operation.
--
-- This property defaults to ``MTLBlendOperationAdd``.
--
-- ObjC selector: @- setRgbBlendOperation:@
setRgbBlendOperation :: IsMTL4RenderPipelineColorAttachmentDescriptor mtL4RenderPipelineColorAttachmentDescriptor => mtL4RenderPipelineColorAttachmentDescriptor -> MTLBlendOperation -> IO ()
setRgbBlendOperation mtL4RenderPipelineColorAttachmentDescriptor  value =
  sendMsg mtL4RenderPipelineColorAttachmentDescriptor (mkSelector "setRgbBlendOperation:") retVoid [argCULong (coerce value)]

-- | Configures the source-alpha blend factor.
--
-- This property defaults to ``MTLBlendFactorOne``.
--
-- ObjC selector: @- sourceAlphaBlendFactor@
sourceAlphaBlendFactor :: IsMTL4RenderPipelineColorAttachmentDescriptor mtL4RenderPipelineColorAttachmentDescriptor => mtL4RenderPipelineColorAttachmentDescriptor -> IO MTLBlendFactor
sourceAlphaBlendFactor mtL4RenderPipelineColorAttachmentDescriptor  =
  fmap (coerce :: CULong -> MTLBlendFactor) $ sendMsg mtL4RenderPipelineColorAttachmentDescriptor (mkSelector "sourceAlphaBlendFactor") retCULong []

-- | Configures the source-alpha blend factor.
--
-- This property defaults to ``MTLBlendFactorOne``.
--
-- ObjC selector: @- setSourceAlphaBlendFactor:@
setSourceAlphaBlendFactor :: IsMTL4RenderPipelineColorAttachmentDescriptor mtL4RenderPipelineColorAttachmentDescriptor => mtL4RenderPipelineColorAttachmentDescriptor -> MTLBlendFactor -> IO ()
setSourceAlphaBlendFactor mtL4RenderPipelineColorAttachmentDescriptor  value =
  sendMsg mtL4RenderPipelineColorAttachmentDescriptor (mkSelector "setSourceAlphaBlendFactor:") retVoid [argCULong (coerce value)]

-- | Configures the destination-alpha blend factor.
--
-- This property defaults to ``MTLBlendFactorZero``.
--
-- ObjC selector: @- destinationAlphaBlendFactor@
destinationAlphaBlendFactor :: IsMTL4RenderPipelineColorAttachmentDescriptor mtL4RenderPipelineColorAttachmentDescriptor => mtL4RenderPipelineColorAttachmentDescriptor -> IO MTLBlendFactor
destinationAlphaBlendFactor mtL4RenderPipelineColorAttachmentDescriptor  =
  fmap (coerce :: CULong -> MTLBlendFactor) $ sendMsg mtL4RenderPipelineColorAttachmentDescriptor (mkSelector "destinationAlphaBlendFactor") retCULong []

-- | Configures the destination-alpha blend factor.
--
-- This property defaults to ``MTLBlendFactorZero``.
--
-- ObjC selector: @- setDestinationAlphaBlendFactor:@
setDestinationAlphaBlendFactor :: IsMTL4RenderPipelineColorAttachmentDescriptor mtL4RenderPipelineColorAttachmentDescriptor => mtL4RenderPipelineColorAttachmentDescriptor -> MTLBlendFactor -> IO ()
setDestinationAlphaBlendFactor mtL4RenderPipelineColorAttachmentDescriptor  value =
  sendMsg mtL4RenderPipelineColorAttachmentDescriptor (mkSelector "setDestinationAlphaBlendFactor:") retVoid [argCULong (coerce value)]

-- | Configures the alpha blending operation.
--
-- This property defaults to ``MTLBlendOperationAdd``.
--
-- ObjC selector: @- alphaBlendOperation@
alphaBlendOperation :: IsMTL4RenderPipelineColorAttachmentDescriptor mtL4RenderPipelineColorAttachmentDescriptor => mtL4RenderPipelineColorAttachmentDescriptor -> IO MTLBlendOperation
alphaBlendOperation mtL4RenderPipelineColorAttachmentDescriptor  =
  fmap (coerce :: CULong -> MTLBlendOperation) $ sendMsg mtL4RenderPipelineColorAttachmentDescriptor (mkSelector "alphaBlendOperation") retCULong []

-- | Configures the alpha blending operation.
--
-- This property defaults to ``MTLBlendOperationAdd``.
--
-- ObjC selector: @- setAlphaBlendOperation:@
setAlphaBlendOperation :: IsMTL4RenderPipelineColorAttachmentDescriptor mtL4RenderPipelineColorAttachmentDescriptor => mtL4RenderPipelineColorAttachmentDescriptor -> MTLBlendOperation -> IO ()
setAlphaBlendOperation mtL4RenderPipelineColorAttachmentDescriptor  value =
  sendMsg mtL4RenderPipelineColorAttachmentDescriptor (mkSelector "setAlphaBlendOperation:") retVoid [argCULong (coerce value)]

-- | Configures the color write mask.
--
-- This property defaults to ``MTLColorWriteMaskAll``.
--
-- ObjC selector: @- writeMask@
writeMask :: IsMTL4RenderPipelineColorAttachmentDescriptor mtL4RenderPipelineColorAttachmentDescriptor => mtL4RenderPipelineColorAttachmentDescriptor -> IO MTLColorWriteMask
writeMask mtL4RenderPipelineColorAttachmentDescriptor  =
  fmap (coerce :: CULong -> MTLColorWriteMask) $ sendMsg mtL4RenderPipelineColorAttachmentDescriptor (mkSelector "writeMask") retCULong []

-- | Configures the color write mask.
--
-- This property defaults to ``MTLColorWriteMaskAll``.
--
-- ObjC selector: @- setWriteMask:@
setWriteMask :: IsMTL4RenderPipelineColorAttachmentDescriptor mtL4RenderPipelineColorAttachmentDescriptor => mtL4RenderPipelineColorAttachmentDescriptor -> MTLColorWriteMask -> IO ()
setWriteMask mtL4RenderPipelineColorAttachmentDescriptor  value =
  sendMsg mtL4RenderPipelineColorAttachmentDescriptor (mkSelector "setWriteMask:") retVoid [argCULong (coerce value)]

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @reset@
resetSelector :: Selector
resetSelector = mkSelector "reset"

-- | @Selector@ for @pixelFormat@
pixelFormatSelector :: Selector
pixelFormatSelector = mkSelector "pixelFormat"

-- | @Selector@ for @setPixelFormat:@
setPixelFormatSelector :: Selector
setPixelFormatSelector = mkSelector "setPixelFormat:"

-- | @Selector@ for @blendingState@
blendingStateSelector :: Selector
blendingStateSelector = mkSelector "blendingState"

-- | @Selector@ for @setBlendingState:@
setBlendingStateSelector :: Selector
setBlendingStateSelector = mkSelector "setBlendingState:"

-- | @Selector@ for @sourceRGBBlendFactor@
sourceRGBBlendFactorSelector :: Selector
sourceRGBBlendFactorSelector = mkSelector "sourceRGBBlendFactor"

-- | @Selector@ for @setSourceRGBBlendFactor:@
setSourceRGBBlendFactorSelector :: Selector
setSourceRGBBlendFactorSelector = mkSelector "setSourceRGBBlendFactor:"

-- | @Selector@ for @destinationRGBBlendFactor@
destinationRGBBlendFactorSelector :: Selector
destinationRGBBlendFactorSelector = mkSelector "destinationRGBBlendFactor"

-- | @Selector@ for @setDestinationRGBBlendFactor:@
setDestinationRGBBlendFactorSelector :: Selector
setDestinationRGBBlendFactorSelector = mkSelector "setDestinationRGBBlendFactor:"

-- | @Selector@ for @rgbBlendOperation@
rgbBlendOperationSelector :: Selector
rgbBlendOperationSelector = mkSelector "rgbBlendOperation"

-- | @Selector@ for @setRgbBlendOperation:@
setRgbBlendOperationSelector :: Selector
setRgbBlendOperationSelector = mkSelector "setRgbBlendOperation:"

-- | @Selector@ for @sourceAlphaBlendFactor@
sourceAlphaBlendFactorSelector :: Selector
sourceAlphaBlendFactorSelector = mkSelector "sourceAlphaBlendFactor"

-- | @Selector@ for @setSourceAlphaBlendFactor:@
setSourceAlphaBlendFactorSelector :: Selector
setSourceAlphaBlendFactorSelector = mkSelector "setSourceAlphaBlendFactor:"

-- | @Selector@ for @destinationAlphaBlendFactor@
destinationAlphaBlendFactorSelector :: Selector
destinationAlphaBlendFactorSelector = mkSelector "destinationAlphaBlendFactor"

-- | @Selector@ for @setDestinationAlphaBlendFactor:@
setDestinationAlphaBlendFactorSelector :: Selector
setDestinationAlphaBlendFactorSelector = mkSelector "setDestinationAlphaBlendFactor:"

-- | @Selector@ for @alphaBlendOperation@
alphaBlendOperationSelector :: Selector
alphaBlendOperationSelector = mkSelector "alphaBlendOperation"

-- | @Selector@ for @setAlphaBlendOperation:@
setAlphaBlendOperationSelector :: Selector
setAlphaBlendOperationSelector = mkSelector "setAlphaBlendOperation:"

-- | @Selector@ for @writeMask@
writeMaskSelector :: Selector
writeMaskSelector = mkSelector "writeMask"

-- | @Selector@ for @setWriteMask:@
setWriteMaskSelector :: Selector
setWriteMaskSelector = mkSelector "setWriteMask:"

