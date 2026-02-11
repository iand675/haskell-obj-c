{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | MPSImageDescriptor
--
-- This depends on Metal.framework
--
-- A MPSImageDescriptor object describes a attributes of MPSImage and is used to              create one (see MPSImage discussion below)
--
-- Generated bindings for @MPSImageDescriptor@.
module ObjC.MetalPerformanceShaders.MPSImageDescriptor
  ( MPSImageDescriptor
  , IsMPSImageDescriptor(..)
  , imageDescriptorWithChannelFormat_width_height_featureChannels
  , imageDescriptorWithChannelFormat_width_height_featureChannels_numberOfImages_usage
  , copyWithZone
  , width
  , setWidth
  , height
  , setHeight
  , featureChannels
  , setFeatureChannels
  , numberOfImages
  , setNumberOfImages
  , pixelFormat
  , channelFormat
  , setChannelFormat
  , cpuCacheMode
  , setCpuCacheMode
  , storageMode
  , setStorageMode
  , usage
  , setUsage
  , imageDescriptorWithChannelFormat_width_height_featureChannelsSelector
  , imageDescriptorWithChannelFormat_width_height_featureChannels_numberOfImages_usageSelector
  , copyWithZoneSelector
  , widthSelector
  , setWidthSelector
  , heightSelector
  , setHeightSelector
  , featureChannelsSelector
  , setFeatureChannelsSelector
  , numberOfImagesSelector
  , setNumberOfImagesSelector
  , pixelFormatSelector
  , channelFormatSelector
  , setChannelFormatSelector
  , cpuCacheModeSelector
  , setCpuCacheModeSelector
  , storageModeSelector
  , setStorageModeSelector
  , usageSelector
  , setUsageSelector

  -- * Enum types
  , MPSImageFeatureChannelFormat(MPSImageFeatureChannelFormat)
  , pattern MPSImageFeatureChannelFormatNone
  , pattern MPSImageFeatureChannelFormatUnorm8
  , pattern MPSImageFeatureChannelFormatUnorm16
  , pattern MPSImageFeatureChannelFormatFloat16
  , pattern MPSImageFeatureChannelFormatFloat32
  , pattern MPSImageFeatureChannelFormat_reserved0
  , pattern MPSImageFeatureChannelFormatCount
  , MTLCPUCacheMode(MTLCPUCacheMode)
  , pattern MTLCPUCacheModeDefaultCache
  , pattern MTLCPUCacheModeWriteCombined
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
  , MTLStorageMode(MTLStorageMode)
  , pattern MTLStorageModeShared
  , pattern MTLStorageModeManaged
  , pattern MTLStorageModePrivate
  , pattern MTLStorageModeMemoryless
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

import ObjC.MetalPerformanceShaders.Internal.Classes
import ObjC.MetalPerformanceShaders.Internal.Enums
import ObjC.Metal.Internal.Enums
import ObjC.Foundation.Internal.Classes

-- | Create a MPSImageDescriptor for a single read/write cnn image.
--
-- ObjC selector: @+ imageDescriptorWithChannelFormat:width:height:featureChannels:@
imageDescriptorWithChannelFormat_width_height_featureChannels :: MPSImageFeatureChannelFormat -> CULong -> CULong -> CULong -> IO (Id MPSImageDescriptor)
imageDescriptorWithChannelFormat_width_height_featureChannels channelFormat width height featureChannels =
  do
    cls' <- getRequiredClass "MPSImageDescriptor"
    sendClassMsg cls' (mkSelector "imageDescriptorWithChannelFormat:width:height:featureChannels:") (retPtr retVoid) [argCULong (coerce channelFormat), argCULong (fromIntegral width), argCULong (fromIntegral height), argCULong (fromIntegral featureChannels)] >>= retainedObject . castPtr

-- | Create a MPSImageDescriptor for a read/write cnn image with option to set usage and batch size (numberOfImages).
--
-- ObjC selector: @+ imageDescriptorWithChannelFormat:width:height:featureChannels:numberOfImages:usage:@
imageDescriptorWithChannelFormat_width_height_featureChannels_numberOfImages_usage :: MPSImageFeatureChannelFormat -> CULong -> CULong -> CULong -> CULong -> MTLTextureUsage -> IO (Id MPSImageDescriptor)
imageDescriptorWithChannelFormat_width_height_featureChannels_numberOfImages_usage channelFormat width height featureChannels numberOfImages usage =
  do
    cls' <- getRequiredClass "MPSImageDescriptor"
    sendClassMsg cls' (mkSelector "imageDescriptorWithChannelFormat:width:height:featureChannels:numberOfImages:usage:") (retPtr retVoid) [argCULong (coerce channelFormat), argCULong (fromIntegral width), argCULong (fromIntegral height), argCULong (fromIntegral featureChannels), argCULong (fromIntegral numberOfImages), argCULong (coerce usage)] >>= retainedObject . castPtr

-- | @- copyWithZone:@
copyWithZone :: IsMPSImageDescriptor mpsImageDescriptor => mpsImageDescriptor -> Ptr () -> IO (Id MPSImageDescriptor)
copyWithZone mpsImageDescriptor  zone =
  sendMsg mpsImageDescriptor (mkSelector "copyWithZone:") (retPtr retVoid) [argPtr zone] >>= ownedObject . castPtr

-- | width
--
-- The width of the CNN image.
--
-- The formal width of the CNN image in pixels.  Default = 1.
--
-- ObjC selector: @- width@
width :: IsMPSImageDescriptor mpsImageDescriptor => mpsImageDescriptor -> IO CULong
width mpsImageDescriptor  =
  sendMsg mpsImageDescriptor (mkSelector "width") retCULong []

-- | width
--
-- The width of the CNN image.
--
-- The formal width of the CNN image in pixels.  Default = 1.
--
-- ObjC selector: @- setWidth:@
setWidth :: IsMPSImageDescriptor mpsImageDescriptor => mpsImageDescriptor -> CULong -> IO ()
setWidth mpsImageDescriptor  value =
  sendMsg mpsImageDescriptor (mkSelector "setWidth:") retVoid [argCULong (fromIntegral value)]

-- | height
--
-- The height of the CNN image.
--
-- The formal height of the CNN image in pixels. Default = 1.
--
-- ObjC selector: @- height@
height :: IsMPSImageDescriptor mpsImageDescriptor => mpsImageDescriptor -> IO CULong
height mpsImageDescriptor  =
  sendMsg mpsImageDescriptor (mkSelector "height") retCULong []

-- | height
--
-- The height of the CNN image.
--
-- The formal height of the CNN image in pixels. Default = 1.
--
-- ObjC selector: @- setHeight:@
setHeight :: IsMPSImageDescriptor mpsImageDescriptor => mpsImageDescriptor -> CULong -> IO ()
setHeight mpsImageDescriptor  value =
  sendMsg mpsImageDescriptor (mkSelector "setHeight:") retVoid [argCULong (fromIntegral value)]

-- | featureChannels
--
-- The number of feature channels per pixel.  Default = 1.
--
-- ObjC selector: @- featureChannels@
featureChannels :: IsMPSImageDescriptor mpsImageDescriptor => mpsImageDescriptor -> IO CULong
featureChannels mpsImageDescriptor  =
  sendMsg mpsImageDescriptor (mkSelector "featureChannels") retCULong []

-- | featureChannels
--
-- The number of feature channels per pixel.  Default = 1.
--
-- ObjC selector: @- setFeatureChannels:@
setFeatureChannels :: IsMPSImageDescriptor mpsImageDescriptor => mpsImageDescriptor -> CULong -> IO ()
setFeatureChannels mpsImageDescriptor  value =
  sendMsg mpsImageDescriptor (mkSelector "setFeatureChannels:") retVoid [argCULong (fromIntegral value)]

-- | numberOfImages
--
-- The number of images for batch processing.   Default = 1.
--
-- ObjC selector: @- numberOfImages@
numberOfImages :: IsMPSImageDescriptor mpsImageDescriptor => mpsImageDescriptor -> IO CULong
numberOfImages mpsImageDescriptor  =
  sendMsg mpsImageDescriptor (mkSelector "numberOfImages") retCULong []

-- | numberOfImages
--
-- The number of images for batch processing.   Default = 1.
--
-- ObjC selector: @- setNumberOfImages:@
setNumberOfImages :: IsMPSImageDescriptor mpsImageDescriptor => mpsImageDescriptor -> CULong -> IO ()
setNumberOfImages mpsImageDescriptor  value =
  sendMsg mpsImageDescriptor (mkSelector "setNumberOfImages:") retVoid [argCULong (fromIntegral value)]

-- | pixelFormat
--
-- The MTLPixelFormat expected for the underlying texture.
--
-- ObjC selector: @- pixelFormat@
pixelFormat :: IsMPSImageDescriptor mpsImageDescriptor => mpsImageDescriptor -> IO MTLPixelFormat
pixelFormat mpsImageDescriptor  =
  fmap (coerce :: CULong -> MTLPixelFormat) $ sendMsg mpsImageDescriptor (mkSelector "pixelFormat") retCULong []

-- | channelFormat
--
-- The storage format to use for each channel in the image.
--
-- ObjC selector: @- channelFormat@
channelFormat :: IsMPSImageDescriptor mpsImageDescriptor => mpsImageDescriptor -> IO MPSImageFeatureChannelFormat
channelFormat mpsImageDescriptor  =
  fmap (coerce :: CULong -> MPSImageFeatureChannelFormat) $ sendMsg mpsImageDescriptor (mkSelector "channelFormat") retCULong []

-- | channelFormat
--
-- The storage format to use for each channel in the image.
--
-- ObjC selector: @- setChannelFormat:@
setChannelFormat :: IsMPSImageDescriptor mpsImageDescriptor => mpsImageDescriptor -> MPSImageFeatureChannelFormat -> IO ()
setChannelFormat mpsImageDescriptor  value =
  sendMsg mpsImageDescriptor (mkSelector "setChannelFormat:") retVoid [argCULong (coerce value)]

-- | cpuCacheMode
--
-- Options to specify CPU cache mode of texture resource. Default = MTLCPUCacheModeDefaultCache
--
-- ObjC selector: @- cpuCacheMode@
cpuCacheMode :: IsMPSImageDescriptor mpsImageDescriptor => mpsImageDescriptor -> IO MTLCPUCacheMode
cpuCacheMode mpsImageDescriptor  =
  fmap (coerce :: CULong -> MTLCPUCacheMode) $ sendMsg mpsImageDescriptor (mkSelector "cpuCacheMode") retCULong []

-- | cpuCacheMode
--
-- Options to specify CPU cache mode of texture resource. Default = MTLCPUCacheModeDefaultCache
--
-- ObjC selector: @- setCpuCacheMode:@
setCpuCacheMode :: IsMPSImageDescriptor mpsImageDescriptor => mpsImageDescriptor -> MTLCPUCacheMode -> IO ()
setCpuCacheMode mpsImageDescriptor  value =
  sendMsg mpsImageDescriptor (mkSelector "setCpuCacheMode:") retVoid [argCULong (coerce value)]

-- | storageMode
--
-- To specify storage mode of texture resource.
--
-- Storage mode options:
--
-- Default =   MTLStorageModeShared on iOS
-- MTLStorageModeManaged on Mac OSX
-- MTLStorageModeShared not supported on Mac OSX.
-- See Metal headers for synchronization requirements when using StorageModeManaged
--
-- ObjC selector: @- storageMode@
storageMode :: IsMPSImageDescriptor mpsImageDescriptor => mpsImageDescriptor -> IO MTLStorageMode
storageMode mpsImageDescriptor  =
  fmap (coerce :: CULong -> MTLStorageMode) $ sendMsg mpsImageDescriptor (mkSelector "storageMode") retCULong []

-- | storageMode
--
-- To specify storage mode of texture resource.
--
-- Storage mode options:
--
-- Default =   MTLStorageModeShared on iOS
-- MTLStorageModeManaged on Mac OSX
-- MTLStorageModeShared not supported on Mac OSX.
-- See Metal headers for synchronization requirements when using StorageModeManaged
--
-- ObjC selector: @- setStorageMode:@
setStorageMode :: IsMPSImageDescriptor mpsImageDescriptor => mpsImageDescriptor -> MTLStorageMode -> IO ()
setStorageMode mpsImageDescriptor  value =
  sendMsg mpsImageDescriptor (mkSelector "setStorageMode:") retVoid [argCULong (coerce value)]

-- | usage
--
-- Description of texture usage.  Default = MTLTextureUsageShaderRead/Write
--
-- ObjC selector: @- usage@
usage :: IsMPSImageDescriptor mpsImageDescriptor => mpsImageDescriptor -> IO MTLTextureUsage
usage mpsImageDescriptor  =
  fmap (coerce :: CULong -> MTLTextureUsage) $ sendMsg mpsImageDescriptor (mkSelector "usage") retCULong []

-- | usage
--
-- Description of texture usage.  Default = MTLTextureUsageShaderRead/Write
--
-- ObjC selector: @- setUsage:@
setUsage :: IsMPSImageDescriptor mpsImageDescriptor => mpsImageDescriptor -> MTLTextureUsage -> IO ()
setUsage mpsImageDescriptor  value =
  sendMsg mpsImageDescriptor (mkSelector "setUsage:") retVoid [argCULong (coerce value)]

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @imageDescriptorWithChannelFormat:width:height:featureChannels:@
imageDescriptorWithChannelFormat_width_height_featureChannelsSelector :: Selector
imageDescriptorWithChannelFormat_width_height_featureChannelsSelector = mkSelector "imageDescriptorWithChannelFormat:width:height:featureChannels:"

-- | @Selector@ for @imageDescriptorWithChannelFormat:width:height:featureChannels:numberOfImages:usage:@
imageDescriptorWithChannelFormat_width_height_featureChannels_numberOfImages_usageSelector :: Selector
imageDescriptorWithChannelFormat_width_height_featureChannels_numberOfImages_usageSelector = mkSelector "imageDescriptorWithChannelFormat:width:height:featureChannels:numberOfImages:usage:"

-- | @Selector@ for @copyWithZone:@
copyWithZoneSelector :: Selector
copyWithZoneSelector = mkSelector "copyWithZone:"

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

-- | @Selector@ for @featureChannels@
featureChannelsSelector :: Selector
featureChannelsSelector = mkSelector "featureChannels"

-- | @Selector@ for @setFeatureChannels:@
setFeatureChannelsSelector :: Selector
setFeatureChannelsSelector = mkSelector "setFeatureChannels:"

-- | @Selector@ for @numberOfImages@
numberOfImagesSelector :: Selector
numberOfImagesSelector = mkSelector "numberOfImages"

-- | @Selector@ for @setNumberOfImages:@
setNumberOfImagesSelector :: Selector
setNumberOfImagesSelector = mkSelector "setNumberOfImages:"

-- | @Selector@ for @pixelFormat@
pixelFormatSelector :: Selector
pixelFormatSelector = mkSelector "pixelFormat"

-- | @Selector@ for @channelFormat@
channelFormatSelector :: Selector
channelFormatSelector = mkSelector "channelFormat"

-- | @Selector@ for @setChannelFormat:@
setChannelFormatSelector :: Selector
setChannelFormatSelector = mkSelector "setChannelFormat:"

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

-- | @Selector@ for @usage@
usageSelector :: Selector
usageSelector = mkSelector "usage"

-- | @Selector@ for @setUsage:@
setUsageSelector :: Selector
setUsageSelector = mkSelector "setUsage:"

