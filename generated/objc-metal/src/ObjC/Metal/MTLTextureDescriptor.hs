{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Generated bindings for @MTLTextureDescriptor@.
module ObjC.Metal.MTLTextureDescriptor
  ( MTLTextureDescriptor
  , IsMTLTextureDescriptor(..)
  , texture2DDescriptorWithPixelFormat_width_height_mipmapped
  , textureCubeDescriptorWithPixelFormat_size_mipmapped
  , textureBufferDescriptorWithPixelFormat_width_resourceOptions_usage
  , textureType
  , setTextureType
  , pixelFormat
  , setPixelFormat
  , width
  , setWidth
  , height
  , setHeight
  , depth
  , setDepth
  , mipmapLevelCount
  , setMipmapLevelCount
  , sampleCount
  , setSampleCount
  , arrayLength
  , setArrayLength
  , resourceOptions
  , setResourceOptions
  , cpuCacheMode
  , setCpuCacheMode
  , storageMode
  , setStorageMode
  , hazardTrackingMode
  , setHazardTrackingMode
  , usage
  , setUsage
  , allowGPUOptimizedContents
  , setAllowGPUOptimizedContents
  , compressionType
  , setCompressionType
  , placementSparsePageSize
  , setPlacementSparsePageSize
  , texture2DDescriptorWithPixelFormat_width_height_mipmappedSelector
  , textureCubeDescriptorWithPixelFormat_size_mipmappedSelector
  , textureBufferDescriptorWithPixelFormat_width_resourceOptions_usageSelector
  , textureTypeSelector
  , setTextureTypeSelector
  , pixelFormatSelector
  , setPixelFormatSelector
  , widthSelector
  , setWidthSelector
  , heightSelector
  , setHeightSelector
  , depthSelector
  , setDepthSelector
  , mipmapLevelCountSelector
  , setMipmapLevelCountSelector
  , sampleCountSelector
  , setSampleCountSelector
  , arrayLengthSelector
  , setArrayLengthSelector
  , resourceOptionsSelector
  , setResourceOptionsSelector
  , cpuCacheModeSelector
  , setCpuCacheModeSelector
  , storageModeSelector
  , setStorageModeSelector
  , hazardTrackingModeSelector
  , setHazardTrackingModeSelector
  , usageSelector
  , setUsageSelector
  , allowGPUOptimizedContentsSelector
  , setAllowGPUOptimizedContentsSelector
  , compressionTypeSelector
  , setCompressionTypeSelector
  , placementSparsePageSizeSelector
  , setPlacementSparsePageSizeSelector

  -- * Enum types
  , MTLCPUCacheMode(MTLCPUCacheMode)
  , pattern MTLCPUCacheModeDefaultCache
  , pattern MTLCPUCacheModeWriteCombined
  , MTLHazardTrackingMode(MTLHazardTrackingMode)
  , pattern MTLHazardTrackingModeDefault
  , pattern MTLHazardTrackingModeUntracked
  , pattern MTLHazardTrackingModeTracked
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
  , MTLResourceOptions(MTLResourceOptions)
  , pattern MTLResourceCPUCacheModeDefaultCache
  , pattern MTLResourceCPUCacheModeWriteCombined
  , pattern MTLResourceStorageModeShared
  , pattern MTLResourceStorageModeManaged
  , pattern MTLResourceStorageModePrivate
  , pattern MTLResourceStorageModeMemoryless
  , pattern MTLResourceHazardTrackingModeDefault
  , pattern MTLResourceHazardTrackingModeUntracked
  , pattern MTLResourceHazardTrackingModeTracked
  , pattern MTLResourceOptionCPUCacheModeDefault
  , pattern MTLResourceOptionCPUCacheModeWriteCombined
  , MTLSparsePageSize(MTLSparsePageSize)
  , pattern MTLSparsePageSize16
  , pattern MTLSparsePageSize64
  , pattern MTLSparsePageSize256
  , MTLStorageMode(MTLStorageMode)
  , pattern MTLStorageModeShared
  , pattern MTLStorageModeManaged
  , pattern MTLStorageModePrivate
  , pattern MTLStorageModeMemoryless
  , MTLTextureCompressionType(MTLTextureCompressionType)
  , pattern MTLTextureCompressionTypeLossless
  , pattern MTLTextureCompressionTypeLossy
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
  , MTLTextureUsage(MTLTextureUsage)
  , pattern MTLTextureUsageUnknown
  , pattern MTLTextureUsageShaderRead
  , pattern MTLTextureUsageShaderWrite
  , pattern MTLTextureUsageRenderTarget
  , pattern MTLTextureUsagePixelFormatView
  , pattern MTLTextureUsageShaderAtomic

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

-- | texture2DDescriptorWithPixelFormat:width:height:mipmapped:
--
-- Create a TextureDescriptor for a common 2D texture.
--
-- ObjC selector: @+ texture2DDescriptorWithPixelFormat:width:height:mipmapped:@
texture2DDescriptorWithPixelFormat_width_height_mipmapped :: MTLPixelFormat -> CULong -> CULong -> Bool -> IO (Id MTLTextureDescriptor)
texture2DDescriptorWithPixelFormat_width_height_mipmapped pixelFormat width height mipmapped =
  do
    cls' <- getRequiredClass "MTLTextureDescriptor"
    sendClassMsg cls' (mkSelector "texture2DDescriptorWithPixelFormat:width:height:mipmapped:") (retPtr retVoid) [argCULong (coerce pixelFormat), argCULong (fromIntegral width), argCULong (fromIntegral height), argCULong (if mipmapped then 1 else 0)] >>= retainedObject . castPtr

-- | textureCubeDescriptorWithPixelFormat:size:mipmapped:
--
-- Create a TextureDescriptor for a common Cube texture.
--
-- ObjC selector: @+ textureCubeDescriptorWithPixelFormat:size:mipmapped:@
textureCubeDescriptorWithPixelFormat_size_mipmapped :: MTLPixelFormat -> CULong -> Bool -> IO (Id MTLTextureDescriptor)
textureCubeDescriptorWithPixelFormat_size_mipmapped pixelFormat size mipmapped =
  do
    cls' <- getRequiredClass "MTLTextureDescriptor"
    sendClassMsg cls' (mkSelector "textureCubeDescriptorWithPixelFormat:size:mipmapped:") (retPtr retVoid) [argCULong (coerce pixelFormat), argCULong (fromIntegral size), argCULong (if mipmapped then 1 else 0)] >>= retainedObject . castPtr

-- | textureBufferDescriptorWithPixelFormat:width:resourceOptions:usage:
--
-- Create a TextureDescriptor for a common texture buffer.
--
-- ObjC selector: @+ textureBufferDescriptorWithPixelFormat:width:resourceOptions:usage:@
textureBufferDescriptorWithPixelFormat_width_resourceOptions_usage :: MTLPixelFormat -> CULong -> MTLResourceOptions -> MTLTextureUsage -> IO (Id MTLTextureDescriptor)
textureBufferDescriptorWithPixelFormat_width_resourceOptions_usage pixelFormat width resourceOptions usage =
  do
    cls' <- getRequiredClass "MTLTextureDescriptor"
    sendClassMsg cls' (mkSelector "textureBufferDescriptorWithPixelFormat:width:resourceOptions:usage:") (retPtr retVoid) [argCULong (coerce pixelFormat), argCULong (fromIntegral width), argCULong (coerce resourceOptions), argCULong (coerce usage)] >>= retainedObject . castPtr

-- | type
--
-- The overall type of the texture to be created. The default value is MTLTextureType2D.
--
-- ObjC selector: @- textureType@
textureType :: IsMTLTextureDescriptor mtlTextureDescriptor => mtlTextureDescriptor -> IO MTLTextureType
textureType mtlTextureDescriptor  =
  fmap (coerce :: CULong -> MTLTextureType) $ sendMsg mtlTextureDescriptor (mkSelector "textureType") retCULong []

-- | type
--
-- The overall type of the texture to be created. The default value is MTLTextureType2D.
--
-- ObjC selector: @- setTextureType:@
setTextureType :: IsMTLTextureDescriptor mtlTextureDescriptor => mtlTextureDescriptor -> MTLTextureType -> IO ()
setTextureType mtlTextureDescriptor  value =
  sendMsg mtlTextureDescriptor (mkSelector "setTextureType:") retVoid [argCULong (coerce value)]

-- | pixelFormat
--
-- The pixel format to use when allocating this texture. This is also the pixel format that will be used to when the caller writes or reads pixels from this texture. The default value is MTLPixelFormatRGBA8Unorm.
--
-- ObjC selector: @- pixelFormat@
pixelFormat :: IsMTLTextureDescriptor mtlTextureDescriptor => mtlTextureDescriptor -> IO MTLPixelFormat
pixelFormat mtlTextureDescriptor  =
  fmap (coerce :: CULong -> MTLPixelFormat) $ sendMsg mtlTextureDescriptor (mkSelector "pixelFormat") retCULong []

-- | pixelFormat
--
-- The pixel format to use when allocating this texture. This is also the pixel format that will be used to when the caller writes or reads pixels from this texture. The default value is MTLPixelFormatRGBA8Unorm.
--
-- ObjC selector: @- setPixelFormat:@
setPixelFormat :: IsMTLTextureDescriptor mtlTextureDescriptor => mtlTextureDescriptor -> MTLPixelFormat -> IO ()
setPixelFormat mtlTextureDescriptor  value =
  sendMsg mtlTextureDescriptor (mkSelector "setPixelFormat:") retVoid [argCULong (coerce value)]

-- | width
--
-- The width of the texture to create. The default value is 1.
--
-- ObjC selector: @- width@
width :: IsMTLTextureDescriptor mtlTextureDescriptor => mtlTextureDescriptor -> IO CULong
width mtlTextureDescriptor  =
  sendMsg mtlTextureDescriptor (mkSelector "width") retCULong []

-- | width
--
-- The width of the texture to create. The default value is 1.
--
-- ObjC selector: @- setWidth:@
setWidth :: IsMTLTextureDescriptor mtlTextureDescriptor => mtlTextureDescriptor -> CULong -> IO ()
setWidth mtlTextureDescriptor  value =
  sendMsg mtlTextureDescriptor (mkSelector "setWidth:") retVoid [argCULong (fromIntegral value)]

-- | height
--
-- The height of the texture to create. The default value is 1.
--
-- height If allocating a 1D texture, height must be 1.
--
-- ObjC selector: @- height@
height :: IsMTLTextureDescriptor mtlTextureDescriptor => mtlTextureDescriptor -> IO CULong
height mtlTextureDescriptor  =
  sendMsg mtlTextureDescriptor (mkSelector "height") retCULong []

-- | height
--
-- The height of the texture to create. The default value is 1.
--
-- height If allocating a 1D texture, height must be 1.
--
-- ObjC selector: @- setHeight:@
setHeight :: IsMTLTextureDescriptor mtlTextureDescriptor => mtlTextureDescriptor -> CULong -> IO ()
setHeight mtlTextureDescriptor  value =
  sendMsg mtlTextureDescriptor (mkSelector "setHeight:") retVoid [argCULong (fromIntegral value)]

-- | depth
--
-- The depth of the texture to create. The default value is 1.
--
-- depth When allocating any texture types other than 3D, depth must be 1.
--
-- ObjC selector: @- depth@
depth :: IsMTLTextureDescriptor mtlTextureDescriptor => mtlTextureDescriptor -> IO CULong
depth mtlTextureDescriptor  =
  sendMsg mtlTextureDescriptor (mkSelector "depth") retCULong []

-- | depth
--
-- The depth of the texture to create. The default value is 1.
--
-- depth When allocating any texture types other than 3D, depth must be 1.
--
-- ObjC selector: @- setDepth:@
setDepth :: IsMTLTextureDescriptor mtlTextureDescriptor => mtlTextureDescriptor -> CULong -> IO ()
setDepth mtlTextureDescriptor  value =
  sendMsg mtlTextureDescriptor (mkSelector "setDepth:") retVoid [argCULong (fromIntegral value)]

-- | mipmapLevelCount
--
-- The number of mipmap levels to allocate. The default value is 1.
--
-- When creating Buffer and Multisample textures, mipmapLevelCount must be 1.
--
-- ObjC selector: @- mipmapLevelCount@
mipmapLevelCount :: IsMTLTextureDescriptor mtlTextureDescriptor => mtlTextureDescriptor -> IO CULong
mipmapLevelCount mtlTextureDescriptor  =
  sendMsg mtlTextureDescriptor (mkSelector "mipmapLevelCount") retCULong []

-- | mipmapLevelCount
--
-- The number of mipmap levels to allocate. The default value is 1.
--
-- When creating Buffer and Multisample textures, mipmapLevelCount must be 1.
--
-- ObjC selector: @- setMipmapLevelCount:@
setMipmapLevelCount :: IsMTLTextureDescriptor mtlTextureDescriptor => mtlTextureDescriptor -> CULong -> IO ()
setMipmapLevelCount mtlTextureDescriptor  value =
  sendMsg mtlTextureDescriptor (mkSelector "setMipmapLevelCount:") retVoid [argCULong (fromIntegral value)]

-- | sampleCount
--
-- The number of samples in the texture to create. The default value is 1.
--
-- When creating Buffer textures sampleCount must be 1. Implementations may round sample counts up to the next supported value.
--
-- ObjC selector: @- sampleCount@
sampleCount :: IsMTLTextureDescriptor mtlTextureDescriptor => mtlTextureDescriptor -> IO CULong
sampleCount mtlTextureDescriptor  =
  sendMsg mtlTextureDescriptor (mkSelector "sampleCount") retCULong []

-- | sampleCount
--
-- The number of samples in the texture to create. The default value is 1.
--
-- When creating Buffer textures sampleCount must be 1. Implementations may round sample counts up to the next supported value.
--
-- ObjC selector: @- setSampleCount:@
setSampleCount :: IsMTLTextureDescriptor mtlTextureDescriptor => mtlTextureDescriptor -> CULong -> IO ()
setSampleCount mtlTextureDescriptor  value =
  sendMsg mtlTextureDescriptor (mkSelector "setSampleCount:") retVoid [argCULong (fromIntegral value)]

-- | arrayLength
--
-- The number of array elements to allocate. The default value is 1.
--
-- When allocating any non-Array texture type, arrayLength has to be 1. Otherwise it must be set to something greater than 1 and less than 2048.
--
-- ObjC selector: @- arrayLength@
arrayLength :: IsMTLTextureDescriptor mtlTextureDescriptor => mtlTextureDescriptor -> IO CULong
arrayLength mtlTextureDescriptor  =
  sendMsg mtlTextureDescriptor (mkSelector "arrayLength") retCULong []

-- | arrayLength
--
-- The number of array elements to allocate. The default value is 1.
--
-- When allocating any non-Array texture type, arrayLength has to be 1. Otherwise it must be set to something greater than 1 and less than 2048.
--
-- ObjC selector: @- setArrayLength:@
setArrayLength :: IsMTLTextureDescriptor mtlTextureDescriptor => mtlTextureDescriptor -> CULong -> IO ()
setArrayLength mtlTextureDescriptor  value =
  sendMsg mtlTextureDescriptor (mkSelector "setArrayLength:") retVoid [argCULong (fromIntegral value)]

-- | resourceOptions
--
-- Options to control memory allocation parameters, etc.
--
-- Contains a packed set of the storageMode, cpuCacheMode and hazardTrackingMode properties.
--
-- ObjC selector: @- resourceOptions@
resourceOptions :: IsMTLTextureDescriptor mtlTextureDescriptor => mtlTextureDescriptor -> IO MTLResourceOptions
resourceOptions mtlTextureDescriptor  =
  fmap (coerce :: CULong -> MTLResourceOptions) $ sendMsg mtlTextureDescriptor (mkSelector "resourceOptions") retCULong []

-- | resourceOptions
--
-- Options to control memory allocation parameters, etc.
--
-- Contains a packed set of the storageMode, cpuCacheMode and hazardTrackingMode properties.
--
-- ObjC selector: @- setResourceOptions:@
setResourceOptions :: IsMTLTextureDescriptor mtlTextureDescriptor => mtlTextureDescriptor -> MTLResourceOptions -> IO ()
setResourceOptions mtlTextureDescriptor  value =
  sendMsg mtlTextureDescriptor (mkSelector "setResourceOptions:") retVoid [argCULong (coerce value)]

-- | cpuCacheMode
--
-- Options to specify CPU cache mode of texture resource.
--
-- ObjC selector: @- cpuCacheMode@
cpuCacheMode :: IsMTLTextureDescriptor mtlTextureDescriptor => mtlTextureDescriptor -> IO MTLCPUCacheMode
cpuCacheMode mtlTextureDescriptor  =
  fmap (coerce :: CULong -> MTLCPUCacheMode) $ sendMsg mtlTextureDescriptor (mkSelector "cpuCacheMode") retCULong []

-- | cpuCacheMode
--
-- Options to specify CPU cache mode of texture resource.
--
-- ObjC selector: @- setCpuCacheMode:@
setCpuCacheMode :: IsMTLTextureDescriptor mtlTextureDescriptor => mtlTextureDescriptor -> MTLCPUCacheMode -> IO ()
setCpuCacheMode mtlTextureDescriptor  value =
  sendMsg mtlTextureDescriptor (mkSelector "setCpuCacheMode:") retVoid [argCULong (coerce value)]

-- | storageMode
--
-- To specify storage mode of texture resource.
--
-- ObjC selector: @- storageMode@
storageMode :: IsMTLTextureDescriptor mtlTextureDescriptor => mtlTextureDescriptor -> IO MTLStorageMode
storageMode mtlTextureDescriptor  =
  fmap (coerce :: CULong -> MTLStorageMode) $ sendMsg mtlTextureDescriptor (mkSelector "storageMode") retCULong []

-- | storageMode
--
-- To specify storage mode of texture resource.
--
-- ObjC selector: @- setStorageMode:@
setStorageMode :: IsMTLTextureDescriptor mtlTextureDescriptor => mtlTextureDescriptor -> MTLStorageMode -> IO ()
setStorageMode mtlTextureDescriptor  value =
  sendMsg mtlTextureDescriptor (mkSelector "setStorageMode:") retVoid [argCULong (coerce value)]

-- | hazardTrackingMode
--
-- Set hazard tracking mode for the texture. The default value is MTLHazardTrackingModeDefault.
--
-- For resources created from the device, MTLHazardTrackingModeDefault is treated as MTLHazardTrackingModeTracked. For resources created on a heap, MTLHazardTrackingModeDefault is treated as the hazardTrackingMode of the heap itself. In either case, it is possible to opt-out of hazard tracking by setting MTLHazardTrackingModeUntracked. It is not possible to opt-in to hazard tracking on a heap that itself is not hazard tracked. For optimal performance, perform hazard tracking manually through MTLFence or MTLEvent instead.
--
-- ObjC selector: @- hazardTrackingMode@
hazardTrackingMode :: IsMTLTextureDescriptor mtlTextureDescriptor => mtlTextureDescriptor -> IO MTLHazardTrackingMode
hazardTrackingMode mtlTextureDescriptor  =
  fmap (coerce :: CULong -> MTLHazardTrackingMode) $ sendMsg mtlTextureDescriptor (mkSelector "hazardTrackingMode") retCULong []

-- | hazardTrackingMode
--
-- Set hazard tracking mode for the texture. The default value is MTLHazardTrackingModeDefault.
--
-- For resources created from the device, MTLHazardTrackingModeDefault is treated as MTLHazardTrackingModeTracked. For resources created on a heap, MTLHazardTrackingModeDefault is treated as the hazardTrackingMode of the heap itself. In either case, it is possible to opt-out of hazard tracking by setting MTLHazardTrackingModeUntracked. It is not possible to opt-in to hazard tracking on a heap that itself is not hazard tracked. For optimal performance, perform hazard tracking manually through MTLFence or MTLEvent instead.
--
-- ObjC selector: @- setHazardTrackingMode:@
setHazardTrackingMode :: IsMTLTextureDescriptor mtlTextureDescriptor => mtlTextureDescriptor -> MTLHazardTrackingMode -> IO ()
setHazardTrackingMode mtlTextureDescriptor  value =
  sendMsg mtlTextureDescriptor (mkSelector "setHazardTrackingMode:") retVoid [argCULong (coerce value)]

-- | usage
--
-- Description of texture usage
--
-- ObjC selector: @- usage@
usage :: IsMTLTextureDescriptor mtlTextureDescriptor => mtlTextureDescriptor -> IO MTLTextureUsage
usage mtlTextureDescriptor  =
  fmap (coerce :: CULong -> MTLTextureUsage) $ sendMsg mtlTextureDescriptor (mkSelector "usage") retCULong []

-- | usage
--
-- Description of texture usage
--
-- ObjC selector: @- setUsage:@
setUsage :: IsMTLTextureDescriptor mtlTextureDescriptor => mtlTextureDescriptor -> MTLTextureUsage -> IO ()
setUsage mtlTextureDescriptor  value =
  sendMsg mtlTextureDescriptor (mkSelector "setUsage:") retVoid [argCULong (coerce value)]

-- | allowGPUOptimizedContents
--
-- Allow GPU-optimization for the contents of this texture. The default value is true.
--
-- Useful for opting-out of GPU-optimization when implicit optimization (e.g. RT writes) is regressing CPU-read-back performance. See the documentation for optimizeContentsForGPUAccess: and optimizeContentsForCPUAccess: APIs.
--
-- ObjC selector: @- allowGPUOptimizedContents@
allowGPUOptimizedContents :: IsMTLTextureDescriptor mtlTextureDescriptor => mtlTextureDescriptor -> IO Bool
allowGPUOptimizedContents mtlTextureDescriptor  =
  fmap ((/= 0) :: CULong -> Bool) $ sendMsg mtlTextureDescriptor (mkSelector "allowGPUOptimizedContents") retCULong []

-- | allowGPUOptimizedContents
--
-- Allow GPU-optimization for the contents of this texture. The default value is true.
--
-- Useful for opting-out of GPU-optimization when implicit optimization (e.g. RT writes) is regressing CPU-read-back performance. See the documentation for optimizeContentsForGPUAccess: and optimizeContentsForCPUAccess: APIs.
--
-- ObjC selector: @- setAllowGPUOptimizedContents:@
setAllowGPUOptimizedContents :: IsMTLTextureDescriptor mtlTextureDescriptor => mtlTextureDescriptor -> Bool -> IO ()
setAllowGPUOptimizedContents mtlTextureDescriptor  value =
  sendMsg mtlTextureDescriptor (mkSelector "setAllowGPUOptimizedContents:") retVoid [argCULong (if value then 1 else 0)]

-- | compressionType
--
-- Controls how the texture contents will be compressed when written to by the GPU. Compression can be used to reduce the bandwidth usage and storage requirements of a texture.
--
-- The default compression type is lossless, meaning that no loss of precision will occur when the texture content is modified. Losslessly compressed textures may benefit from reduced bandwidth usage when regions of correlated color values are written, but do not benefit from reduced storage requirements. Enabling lossy compression for textures that can tolerate some precision loss will guarantee both reduced bandwidth usage and reduced storage requirements. The amount of precision loss depends on the color values stored; regions with correlated color values can be represented with limited to no precision loss, whereas regions with unrelated color values suffer more precision loss. Enabling lossy compression requires both storageMode == MTLStorageModePrivate, allowGPUOptimizedContents == YES, and cannot be combined with either MTLTextureUsagePixelFormatView, MTLTextureUsageShaderWrite, MTLTextureUsageShaderAtomic, MTLTextureType1D(Array) or MTLTextureTypeTextureBuffer. Moreover, not all MTLPixelFormat are supported with lossy compression, verify that the MTLDevice's GPU family supports the lossy compression feature for the pixelFormat requested. Set allowGPUOptimizedContents to NO to opt out of both lossless and lossy compression; such textures do not benefit from either reduced bandwidth usage or reduced storage requirements, but have predictable CPU readback performance.
--
-- ObjC selector: @- compressionType@
compressionType :: IsMTLTextureDescriptor mtlTextureDescriptor => mtlTextureDescriptor -> IO MTLTextureCompressionType
compressionType mtlTextureDescriptor  =
  fmap (coerce :: CLong -> MTLTextureCompressionType) $ sendMsg mtlTextureDescriptor (mkSelector "compressionType") retCLong []

-- | compressionType
--
-- Controls how the texture contents will be compressed when written to by the GPU. Compression can be used to reduce the bandwidth usage and storage requirements of a texture.
--
-- The default compression type is lossless, meaning that no loss of precision will occur when the texture content is modified. Losslessly compressed textures may benefit from reduced bandwidth usage when regions of correlated color values are written, but do not benefit from reduced storage requirements. Enabling lossy compression for textures that can tolerate some precision loss will guarantee both reduced bandwidth usage and reduced storage requirements. The amount of precision loss depends on the color values stored; regions with correlated color values can be represented with limited to no precision loss, whereas regions with unrelated color values suffer more precision loss. Enabling lossy compression requires both storageMode == MTLStorageModePrivate, allowGPUOptimizedContents == YES, and cannot be combined with either MTLTextureUsagePixelFormatView, MTLTextureUsageShaderWrite, MTLTextureUsageShaderAtomic, MTLTextureType1D(Array) or MTLTextureTypeTextureBuffer. Moreover, not all MTLPixelFormat are supported with lossy compression, verify that the MTLDevice's GPU family supports the lossy compression feature for the pixelFormat requested. Set allowGPUOptimizedContents to NO to opt out of both lossless and lossy compression; such textures do not benefit from either reduced bandwidth usage or reduced storage requirements, but have predictable CPU readback performance.
--
-- ObjC selector: @- setCompressionType:@
setCompressionType :: IsMTLTextureDescriptor mtlTextureDescriptor => mtlTextureDescriptor -> MTLTextureCompressionType -> IO ()
setCompressionType mtlTextureDescriptor  value =
  sendMsg mtlTextureDescriptor (mkSelector "setCompressionType:") retVoid [argCLong (coerce value)]

-- | Determines the page size for a placement sparse texture.
--
-- Set this property to a non-zero value to create a *placement sparse texture*.
--
-- Placement sparse textures are instances of ``MTLTexture`` that you assign memory to using a ``MTLHeap`` instance of type ``MTLHeapType/MTLHeapTypePlacement`` and a ``MTLHeapDescriptor/maxCompatiblePlacementSparsePageSize`` at least as large as the ``MTLSparsePageSize`` value you assign to this property.
--
-- This value is 0 by default.
--
-- ObjC selector: @- placementSparsePageSize@
placementSparsePageSize :: IsMTLTextureDescriptor mtlTextureDescriptor => mtlTextureDescriptor -> IO MTLSparsePageSize
placementSparsePageSize mtlTextureDescriptor  =
  fmap (coerce :: CLong -> MTLSparsePageSize) $ sendMsg mtlTextureDescriptor (mkSelector "placementSparsePageSize") retCLong []

-- | Determines the page size for a placement sparse texture.
--
-- Set this property to a non-zero value to create a *placement sparse texture*.
--
-- Placement sparse textures are instances of ``MTLTexture`` that you assign memory to using a ``MTLHeap`` instance of type ``MTLHeapType/MTLHeapTypePlacement`` and a ``MTLHeapDescriptor/maxCompatiblePlacementSparsePageSize`` at least as large as the ``MTLSparsePageSize`` value you assign to this property.
--
-- This value is 0 by default.
--
-- ObjC selector: @- setPlacementSparsePageSize:@
setPlacementSparsePageSize :: IsMTLTextureDescriptor mtlTextureDescriptor => mtlTextureDescriptor -> MTLSparsePageSize -> IO ()
setPlacementSparsePageSize mtlTextureDescriptor  value =
  sendMsg mtlTextureDescriptor (mkSelector "setPlacementSparsePageSize:") retVoid [argCLong (coerce value)]

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @texture2DDescriptorWithPixelFormat:width:height:mipmapped:@
texture2DDescriptorWithPixelFormat_width_height_mipmappedSelector :: Selector
texture2DDescriptorWithPixelFormat_width_height_mipmappedSelector = mkSelector "texture2DDescriptorWithPixelFormat:width:height:mipmapped:"

-- | @Selector@ for @textureCubeDescriptorWithPixelFormat:size:mipmapped:@
textureCubeDescriptorWithPixelFormat_size_mipmappedSelector :: Selector
textureCubeDescriptorWithPixelFormat_size_mipmappedSelector = mkSelector "textureCubeDescriptorWithPixelFormat:size:mipmapped:"

-- | @Selector@ for @textureBufferDescriptorWithPixelFormat:width:resourceOptions:usage:@
textureBufferDescriptorWithPixelFormat_width_resourceOptions_usageSelector :: Selector
textureBufferDescriptorWithPixelFormat_width_resourceOptions_usageSelector = mkSelector "textureBufferDescriptorWithPixelFormat:width:resourceOptions:usage:"

-- | @Selector@ for @textureType@
textureTypeSelector :: Selector
textureTypeSelector = mkSelector "textureType"

-- | @Selector@ for @setTextureType:@
setTextureTypeSelector :: Selector
setTextureTypeSelector = mkSelector "setTextureType:"

-- | @Selector@ for @pixelFormat@
pixelFormatSelector :: Selector
pixelFormatSelector = mkSelector "pixelFormat"

-- | @Selector@ for @setPixelFormat:@
setPixelFormatSelector :: Selector
setPixelFormatSelector = mkSelector "setPixelFormat:"

-- | @Selector@ for @width@
widthSelector :: Selector
widthSelector = mkSelector "width"

-- | @Selector@ for @setWidth:@
setWidthSelector :: Selector
setWidthSelector = mkSelector "setWidth:"

-- | @Selector@ for @height@
heightSelector :: Selector
heightSelector = mkSelector "height"

-- | @Selector@ for @setHeight:@
setHeightSelector :: Selector
setHeightSelector = mkSelector "setHeight:"

-- | @Selector@ for @depth@
depthSelector :: Selector
depthSelector = mkSelector "depth"

-- | @Selector@ for @setDepth:@
setDepthSelector :: Selector
setDepthSelector = mkSelector "setDepth:"

-- | @Selector@ for @mipmapLevelCount@
mipmapLevelCountSelector :: Selector
mipmapLevelCountSelector = mkSelector "mipmapLevelCount"

-- | @Selector@ for @setMipmapLevelCount:@
setMipmapLevelCountSelector :: Selector
setMipmapLevelCountSelector = mkSelector "setMipmapLevelCount:"

-- | @Selector@ for @sampleCount@
sampleCountSelector :: Selector
sampleCountSelector = mkSelector "sampleCount"

-- | @Selector@ for @setSampleCount:@
setSampleCountSelector :: Selector
setSampleCountSelector = mkSelector "setSampleCount:"

-- | @Selector@ for @arrayLength@
arrayLengthSelector :: Selector
arrayLengthSelector = mkSelector "arrayLength"

-- | @Selector@ for @setArrayLength:@
setArrayLengthSelector :: Selector
setArrayLengthSelector = mkSelector "setArrayLength:"

-- | @Selector@ for @resourceOptions@
resourceOptionsSelector :: Selector
resourceOptionsSelector = mkSelector "resourceOptions"

-- | @Selector@ for @setResourceOptions:@
setResourceOptionsSelector :: Selector
setResourceOptionsSelector = mkSelector "setResourceOptions:"

-- | @Selector@ for @cpuCacheMode@
cpuCacheModeSelector :: Selector
cpuCacheModeSelector = mkSelector "cpuCacheMode"

-- | @Selector@ for @setCpuCacheMode:@
setCpuCacheModeSelector :: Selector
setCpuCacheModeSelector = mkSelector "setCpuCacheMode:"

-- | @Selector@ for @storageMode@
storageModeSelector :: Selector
storageModeSelector = mkSelector "storageMode"

-- | @Selector@ for @setStorageMode:@
setStorageModeSelector :: Selector
setStorageModeSelector = mkSelector "setStorageMode:"

-- | @Selector@ for @hazardTrackingMode@
hazardTrackingModeSelector :: Selector
hazardTrackingModeSelector = mkSelector "hazardTrackingMode"

-- | @Selector@ for @setHazardTrackingMode:@
setHazardTrackingModeSelector :: Selector
setHazardTrackingModeSelector = mkSelector "setHazardTrackingMode:"

-- | @Selector@ for @usage@
usageSelector :: Selector
usageSelector = mkSelector "usage"

-- | @Selector@ for @setUsage:@
setUsageSelector :: Selector
setUsageSelector = mkSelector "setUsage:"

-- | @Selector@ for @allowGPUOptimizedContents@
allowGPUOptimizedContentsSelector :: Selector
allowGPUOptimizedContentsSelector = mkSelector "allowGPUOptimizedContents"

-- | @Selector@ for @setAllowGPUOptimizedContents:@
setAllowGPUOptimizedContentsSelector :: Selector
setAllowGPUOptimizedContentsSelector = mkSelector "setAllowGPUOptimizedContents:"

-- | @Selector@ for @compressionType@
compressionTypeSelector :: Selector
compressionTypeSelector = mkSelector "compressionType"

-- | @Selector@ for @setCompressionType:@
setCompressionTypeSelector :: Selector
setCompressionTypeSelector = mkSelector "setCompressionType:"

-- | @Selector@ for @placementSparsePageSize@
placementSparsePageSizeSelector :: Selector
placementSparsePageSizeSelector = mkSelector "placementSparsePageSize"

-- | @Selector@ for @setPlacementSparsePageSize:@
setPlacementSparsePageSizeSelector :: Selector
setPlacementSparsePageSizeSelector = mkSelector "setPlacementSparsePageSize:"

