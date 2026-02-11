{-# LANGUAGE PatternSynonyms #-}
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
  , pixelFormatSelector
  , setPixelFormatSelector
  , blendingEnabledSelector
  , setBlendingEnabledSelector
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

-- | Pixel format.  Defaults to MTLPixelFormatInvalid
--
-- ObjC selector: @- pixelFormat@
pixelFormat :: IsMTLRenderPipelineColorAttachmentDescriptor mtlRenderPipelineColorAttachmentDescriptor => mtlRenderPipelineColorAttachmentDescriptor -> IO MTLPixelFormat
pixelFormat mtlRenderPipelineColorAttachmentDescriptor  =
  fmap (coerce :: CULong -> MTLPixelFormat) $ sendMsg mtlRenderPipelineColorAttachmentDescriptor (mkSelector "pixelFormat") retCULong []

-- | Pixel format.  Defaults to MTLPixelFormatInvalid
--
-- ObjC selector: @- setPixelFormat:@
setPixelFormat :: IsMTLRenderPipelineColorAttachmentDescriptor mtlRenderPipelineColorAttachmentDescriptor => mtlRenderPipelineColorAttachmentDescriptor -> MTLPixelFormat -> IO ()
setPixelFormat mtlRenderPipelineColorAttachmentDescriptor  value =
  sendMsg mtlRenderPipelineColorAttachmentDescriptor (mkSelector "setPixelFormat:") retVoid [argCULong (coerce value)]

-- | Enable blending.  Defaults to NO.
--
-- ObjC selector: @- blendingEnabled@
blendingEnabled :: IsMTLRenderPipelineColorAttachmentDescriptor mtlRenderPipelineColorAttachmentDescriptor => mtlRenderPipelineColorAttachmentDescriptor -> IO Bool
blendingEnabled mtlRenderPipelineColorAttachmentDescriptor  =
  fmap ((/= 0) :: CULong -> Bool) $ sendMsg mtlRenderPipelineColorAttachmentDescriptor (mkSelector "blendingEnabled") retCULong []

-- | Enable blending.  Defaults to NO.
--
-- ObjC selector: @- setBlendingEnabled:@
setBlendingEnabled :: IsMTLRenderPipelineColorAttachmentDescriptor mtlRenderPipelineColorAttachmentDescriptor => mtlRenderPipelineColorAttachmentDescriptor -> Bool -> IO ()
setBlendingEnabled mtlRenderPipelineColorAttachmentDescriptor  value =
  sendMsg mtlRenderPipelineColorAttachmentDescriptor (mkSelector "setBlendingEnabled:") retVoid [argCULong (if value then 1 else 0)]

-- | Defaults to MTLBlendFactorOne
--
-- ObjC selector: @- sourceRGBBlendFactor@
sourceRGBBlendFactor :: IsMTLRenderPipelineColorAttachmentDescriptor mtlRenderPipelineColorAttachmentDescriptor => mtlRenderPipelineColorAttachmentDescriptor -> IO MTLBlendFactor
sourceRGBBlendFactor mtlRenderPipelineColorAttachmentDescriptor  =
  fmap (coerce :: CULong -> MTLBlendFactor) $ sendMsg mtlRenderPipelineColorAttachmentDescriptor (mkSelector "sourceRGBBlendFactor") retCULong []

-- | Defaults to MTLBlendFactorOne
--
-- ObjC selector: @- setSourceRGBBlendFactor:@
setSourceRGBBlendFactor :: IsMTLRenderPipelineColorAttachmentDescriptor mtlRenderPipelineColorAttachmentDescriptor => mtlRenderPipelineColorAttachmentDescriptor -> MTLBlendFactor -> IO ()
setSourceRGBBlendFactor mtlRenderPipelineColorAttachmentDescriptor  value =
  sendMsg mtlRenderPipelineColorAttachmentDescriptor (mkSelector "setSourceRGBBlendFactor:") retVoid [argCULong (coerce value)]

-- | Defaults to MTLBlendFactorZero
--
-- ObjC selector: @- destinationRGBBlendFactor@
destinationRGBBlendFactor :: IsMTLRenderPipelineColorAttachmentDescriptor mtlRenderPipelineColorAttachmentDescriptor => mtlRenderPipelineColorAttachmentDescriptor -> IO MTLBlendFactor
destinationRGBBlendFactor mtlRenderPipelineColorAttachmentDescriptor  =
  fmap (coerce :: CULong -> MTLBlendFactor) $ sendMsg mtlRenderPipelineColorAttachmentDescriptor (mkSelector "destinationRGBBlendFactor") retCULong []

-- | Defaults to MTLBlendFactorZero
--
-- ObjC selector: @- setDestinationRGBBlendFactor:@
setDestinationRGBBlendFactor :: IsMTLRenderPipelineColorAttachmentDescriptor mtlRenderPipelineColorAttachmentDescriptor => mtlRenderPipelineColorAttachmentDescriptor -> MTLBlendFactor -> IO ()
setDestinationRGBBlendFactor mtlRenderPipelineColorAttachmentDescriptor  value =
  sendMsg mtlRenderPipelineColorAttachmentDescriptor (mkSelector "setDestinationRGBBlendFactor:") retVoid [argCULong (coerce value)]

-- | Defaults to MTLBlendOperationAdd
--
-- ObjC selector: @- rgbBlendOperation@
rgbBlendOperation :: IsMTLRenderPipelineColorAttachmentDescriptor mtlRenderPipelineColorAttachmentDescriptor => mtlRenderPipelineColorAttachmentDescriptor -> IO MTLBlendOperation
rgbBlendOperation mtlRenderPipelineColorAttachmentDescriptor  =
  fmap (coerce :: CULong -> MTLBlendOperation) $ sendMsg mtlRenderPipelineColorAttachmentDescriptor (mkSelector "rgbBlendOperation") retCULong []

-- | Defaults to MTLBlendOperationAdd
--
-- ObjC selector: @- setRgbBlendOperation:@
setRgbBlendOperation :: IsMTLRenderPipelineColorAttachmentDescriptor mtlRenderPipelineColorAttachmentDescriptor => mtlRenderPipelineColorAttachmentDescriptor -> MTLBlendOperation -> IO ()
setRgbBlendOperation mtlRenderPipelineColorAttachmentDescriptor  value =
  sendMsg mtlRenderPipelineColorAttachmentDescriptor (mkSelector "setRgbBlendOperation:") retVoid [argCULong (coerce value)]

-- | Defaults to MTLBlendFactorOne
--
-- ObjC selector: @- sourceAlphaBlendFactor@
sourceAlphaBlendFactor :: IsMTLRenderPipelineColorAttachmentDescriptor mtlRenderPipelineColorAttachmentDescriptor => mtlRenderPipelineColorAttachmentDescriptor -> IO MTLBlendFactor
sourceAlphaBlendFactor mtlRenderPipelineColorAttachmentDescriptor  =
  fmap (coerce :: CULong -> MTLBlendFactor) $ sendMsg mtlRenderPipelineColorAttachmentDescriptor (mkSelector "sourceAlphaBlendFactor") retCULong []

-- | Defaults to MTLBlendFactorOne
--
-- ObjC selector: @- setSourceAlphaBlendFactor:@
setSourceAlphaBlendFactor :: IsMTLRenderPipelineColorAttachmentDescriptor mtlRenderPipelineColorAttachmentDescriptor => mtlRenderPipelineColorAttachmentDescriptor -> MTLBlendFactor -> IO ()
setSourceAlphaBlendFactor mtlRenderPipelineColorAttachmentDescriptor  value =
  sendMsg mtlRenderPipelineColorAttachmentDescriptor (mkSelector "setSourceAlphaBlendFactor:") retVoid [argCULong (coerce value)]

-- | Defaults to MTLBlendFactorZero
--
-- ObjC selector: @- destinationAlphaBlendFactor@
destinationAlphaBlendFactor :: IsMTLRenderPipelineColorAttachmentDescriptor mtlRenderPipelineColorAttachmentDescriptor => mtlRenderPipelineColorAttachmentDescriptor -> IO MTLBlendFactor
destinationAlphaBlendFactor mtlRenderPipelineColorAttachmentDescriptor  =
  fmap (coerce :: CULong -> MTLBlendFactor) $ sendMsg mtlRenderPipelineColorAttachmentDescriptor (mkSelector "destinationAlphaBlendFactor") retCULong []

-- | Defaults to MTLBlendFactorZero
--
-- ObjC selector: @- setDestinationAlphaBlendFactor:@
setDestinationAlphaBlendFactor :: IsMTLRenderPipelineColorAttachmentDescriptor mtlRenderPipelineColorAttachmentDescriptor => mtlRenderPipelineColorAttachmentDescriptor -> MTLBlendFactor -> IO ()
setDestinationAlphaBlendFactor mtlRenderPipelineColorAttachmentDescriptor  value =
  sendMsg mtlRenderPipelineColorAttachmentDescriptor (mkSelector "setDestinationAlphaBlendFactor:") retVoid [argCULong (coerce value)]

-- | Defaults to MTLBlendOperationAdd
--
-- ObjC selector: @- alphaBlendOperation@
alphaBlendOperation :: IsMTLRenderPipelineColorAttachmentDescriptor mtlRenderPipelineColorAttachmentDescriptor => mtlRenderPipelineColorAttachmentDescriptor -> IO MTLBlendOperation
alphaBlendOperation mtlRenderPipelineColorAttachmentDescriptor  =
  fmap (coerce :: CULong -> MTLBlendOperation) $ sendMsg mtlRenderPipelineColorAttachmentDescriptor (mkSelector "alphaBlendOperation") retCULong []

-- | Defaults to MTLBlendOperationAdd
--
-- ObjC selector: @- setAlphaBlendOperation:@
setAlphaBlendOperation :: IsMTLRenderPipelineColorAttachmentDescriptor mtlRenderPipelineColorAttachmentDescriptor => mtlRenderPipelineColorAttachmentDescriptor -> MTLBlendOperation -> IO ()
setAlphaBlendOperation mtlRenderPipelineColorAttachmentDescriptor  value =
  sendMsg mtlRenderPipelineColorAttachmentDescriptor (mkSelector "setAlphaBlendOperation:") retVoid [argCULong (coerce value)]

-- | Defaults to MTLColorWriteMaskAll
--
-- ObjC selector: @- writeMask@
writeMask :: IsMTLRenderPipelineColorAttachmentDescriptor mtlRenderPipelineColorAttachmentDescriptor => mtlRenderPipelineColorAttachmentDescriptor -> IO MTLColorWriteMask
writeMask mtlRenderPipelineColorAttachmentDescriptor  =
  fmap (coerce :: CULong -> MTLColorWriteMask) $ sendMsg mtlRenderPipelineColorAttachmentDescriptor (mkSelector "writeMask") retCULong []

-- | Defaults to MTLColorWriteMaskAll
--
-- ObjC selector: @- setWriteMask:@
setWriteMask :: IsMTLRenderPipelineColorAttachmentDescriptor mtlRenderPipelineColorAttachmentDescriptor => mtlRenderPipelineColorAttachmentDescriptor -> MTLColorWriteMask -> IO ()
setWriteMask mtlRenderPipelineColorAttachmentDescriptor  value =
  sendMsg mtlRenderPipelineColorAttachmentDescriptor (mkSelector "setWriteMask:") retVoid [argCULong (coerce value)]

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @pixelFormat@
pixelFormatSelector :: Selector
pixelFormatSelector = mkSelector "pixelFormat"

-- | @Selector@ for @setPixelFormat:@
setPixelFormatSelector :: Selector
setPixelFormatSelector = mkSelector "setPixelFormat:"

-- | @Selector@ for @blendingEnabled@
blendingEnabledSelector :: Selector
blendingEnabledSelector = mkSelector "blendingEnabled"

-- | @Selector@ for @setBlendingEnabled:@
setBlendingEnabledSelector :: Selector
setBlendingEnabledSelector = mkSelector "setBlendingEnabled:"

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

