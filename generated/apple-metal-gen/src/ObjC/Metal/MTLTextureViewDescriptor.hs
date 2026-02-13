{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Generated bindings for @MTLTextureViewDescriptor@.
module ObjC.Metal.MTLTextureViewDescriptor
  ( MTLTextureViewDescriptor
  , IsMTLTextureViewDescriptor(..)
  , pixelFormat
  , setPixelFormat
  , textureType
  , setTextureType
  , levelRange
  , setLevelRange
  , sliceRange
  , setSliceRange
  , levelRangeSelector
  , pixelFormatSelector
  , setLevelRangeSelector
  , setPixelFormatSelector
  , setSliceRangeSelector
  , setTextureTypeSelector
  , sliceRangeSelector
  , textureTypeSelector

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
  , MTLTextureType(MTLTextureType)
  , pattern MTLTextureType1D
  , pattern MTLTextureType1DArray
  , pattern MTLTextureType2D
  , pattern MTLTextureType2DArray
  , pattern MTLTextureType2DMultisample
  , pattern MTLTextureTypeCube
  , pattern MTLTextureTypeCubeArray
  , pattern MTLTextureType3D
  , pattern MTLTextureType2DMultisampleArray
  , pattern MTLTextureTypeTextureBuffer

  ) where

import Foreign.Ptr (Ptr, FunPtr)
import Foreign.C.Types

import ObjC.Runtime.Types
import ObjC.Runtime.Message (sendMessage, sendOwnedMessage, sendClassMessage, sendOwnedClassMessage)
import ObjC.Runtime.Selector (mkSelector)
import ObjC.Runtime.Class (getRequiredClass)

import ObjC.Metal.Internal.Classes
import ObjC.Foundation.Internal.Structs
import ObjC.Metal.Internal.Enums
import ObjC.Foundation.Internal.Classes

-- | pixelFormat
--
-- A desired pixel format of a texture view.
--
-- ObjC selector: @- pixelFormat@
pixelFormat :: IsMTLTextureViewDescriptor mtlTextureViewDescriptor => mtlTextureViewDescriptor -> IO MTLPixelFormat
pixelFormat mtlTextureViewDescriptor =
  sendMessage mtlTextureViewDescriptor pixelFormatSelector

-- | pixelFormat
--
-- A desired pixel format of a texture view.
--
-- ObjC selector: @- setPixelFormat:@
setPixelFormat :: IsMTLTextureViewDescriptor mtlTextureViewDescriptor => mtlTextureViewDescriptor -> MTLPixelFormat -> IO ()
setPixelFormat mtlTextureViewDescriptor value =
  sendMessage mtlTextureViewDescriptor setPixelFormatSelector value

-- | textureType
--
-- A desired texture view of a texture view.
--
-- ObjC selector: @- textureType@
textureType :: IsMTLTextureViewDescriptor mtlTextureViewDescriptor => mtlTextureViewDescriptor -> IO MTLTextureType
textureType mtlTextureViewDescriptor =
  sendMessage mtlTextureViewDescriptor textureTypeSelector

-- | textureType
--
-- A desired texture view of a texture view.
--
-- ObjC selector: @- setTextureType:@
setTextureType :: IsMTLTextureViewDescriptor mtlTextureViewDescriptor => mtlTextureViewDescriptor -> MTLTextureType -> IO ()
setTextureType mtlTextureViewDescriptor value =
  sendMessage mtlTextureViewDescriptor setTextureTypeSelector value

-- | levelRange
--
-- A desired range of mip levels of a texture view.
--
-- ObjC selector: @- levelRange@
levelRange :: IsMTLTextureViewDescriptor mtlTextureViewDescriptor => mtlTextureViewDescriptor -> IO NSRange
levelRange mtlTextureViewDescriptor =
  sendMessage mtlTextureViewDescriptor levelRangeSelector

-- | levelRange
--
-- A desired range of mip levels of a texture view.
--
-- ObjC selector: @- setLevelRange:@
setLevelRange :: IsMTLTextureViewDescriptor mtlTextureViewDescriptor => mtlTextureViewDescriptor -> NSRange -> IO ()
setLevelRange mtlTextureViewDescriptor value =
  sendMessage mtlTextureViewDescriptor setLevelRangeSelector value

-- | sliceRange
--
-- A desired range of slices of a texture view.
--
-- ObjC selector: @- sliceRange@
sliceRange :: IsMTLTextureViewDescriptor mtlTextureViewDescriptor => mtlTextureViewDescriptor -> IO NSRange
sliceRange mtlTextureViewDescriptor =
  sendMessage mtlTextureViewDescriptor sliceRangeSelector

-- | sliceRange
--
-- A desired range of slices of a texture view.
--
-- ObjC selector: @- setSliceRange:@
setSliceRange :: IsMTLTextureViewDescriptor mtlTextureViewDescriptor => mtlTextureViewDescriptor -> NSRange -> IO ()
setSliceRange mtlTextureViewDescriptor value =
  sendMessage mtlTextureViewDescriptor setSliceRangeSelector value

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @pixelFormat@
pixelFormatSelector :: Selector '[] MTLPixelFormat
pixelFormatSelector = mkSelector "pixelFormat"

-- | @Selector@ for @setPixelFormat:@
setPixelFormatSelector :: Selector '[MTLPixelFormat] ()
setPixelFormatSelector = mkSelector "setPixelFormat:"

-- | @Selector@ for @textureType@
textureTypeSelector :: Selector '[] MTLTextureType
textureTypeSelector = mkSelector "textureType"

-- | @Selector@ for @setTextureType:@
setTextureTypeSelector :: Selector '[MTLTextureType] ()
setTextureTypeSelector = mkSelector "setTextureType:"

-- | @Selector@ for @levelRange@
levelRangeSelector :: Selector '[] NSRange
levelRangeSelector = mkSelector "levelRange"

-- | @Selector@ for @setLevelRange:@
setLevelRangeSelector :: Selector '[NSRange] ()
setLevelRangeSelector = mkSelector "setLevelRange:"

-- | @Selector@ for @sliceRange@
sliceRangeSelector :: Selector '[] NSRange
sliceRangeSelector = mkSelector "sliceRange"

-- | @Selector@ for @setSliceRange:@
setSliceRangeSelector :: Selector '[NSRange] ()
setSliceRangeSelector = mkSelector "setSliceRange:"

