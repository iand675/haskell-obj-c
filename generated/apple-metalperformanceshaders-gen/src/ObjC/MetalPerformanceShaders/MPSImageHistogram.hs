{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | MPSImageHistogram
--
-- The MPSImageHistogram computes the histogram of an image.
--
-- Generated bindings for @MPSImageHistogram@.
module ObjC.MetalPerformanceShaders.MPSImageHistogram
  ( MPSImageHistogram
  , IsMPSImageHistogram(..)
  , initWithDevice_histogramInfo
  , initWithCoder_device
  , encodeToCommandBuffer_sourceTexture_histogram_histogramOffset
  , histogramSizeForSourceFormat
  , zeroHistogram
  , setZeroHistogram
  , initWithDevice_histogramInfoSelector
  , initWithCoder_deviceSelector
  , encodeToCommandBuffer_sourceTexture_histogram_histogramOffsetSelector
  , histogramSizeForSourceFormatSelector
  , zeroHistogramSelector
  , setZeroHistogramSelector

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
import ObjC.Metal.Internal.Enums
import ObjC.Foundation.Internal.Classes

-- | Specifies information to compute the histogram for channels of an image.
--
-- @device@ — The device the filter will run on
--
-- @histogramInfo@ — Pointer to the MPSHistogramInfo struct
--
-- Returns: A valid MPSImageHistogram object or nil, if failure.
--
-- ObjC selector: @- initWithDevice:histogramInfo:@
initWithDevice_histogramInfo :: IsMPSImageHistogram mpsImageHistogram => mpsImageHistogram -> RawId -> Const RawId -> IO (Id MPSImageHistogram)
initWithDevice_histogramInfo mpsImageHistogram  device histogramInfo =
    sendMsg mpsImageHistogram (mkSelector "initWithDevice:histogramInfo:") (retPtr retVoid) [argPtr (castPtr (unRawId device) :: Ptr ()), argPtr (castPtr (unRawId (unConst histogramInfo)) :: Ptr ())] >>= ownedObject . castPtr

-- | NSSecureCoding compatability
--
-- While the standard NSSecureCoding/NSCoding method              -initWithCoder: should work, since the file can't              know which device your data is allocated on, we              have to guess and may guess incorrectly.  To avoid              that problem, use initWithCoder:device instead.
--
-- @aDecoder@ — The NSCoder subclass with your serialized MPSKernel
--
-- @device@ — The MTLDevice on which to make the MPSKernel
--
-- Returns: A new MPSKernel object, or nil if failure.
--
-- ObjC selector: @- initWithCoder:device:@
initWithCoder_device :: (IsMPSImageHistogram mpsImageHistogram, IsNSCoder aDecoder) => mpsImageHistogram -> aDecoder -> RawId -> IO (Id MPSImageHistogram)
initWithCoder_device mpsImageHistogram  aDecoder device =
  withObjCPtr aDecoder $ \raw_aDecoder ->
      sendMsg mpsImageHistogram (mkSelector "initWithCoder:device:") (retPtr retVoid) [argPtr (castPtr raw_aDecoder :: Ptr ()), argPtr (castPtr (unRawId device) :: Ptr ())] >>= ownedObject . castPtr

-- | Encode the filter to a command buffer using a MTLComputeCommandEncoder.
--
-- The filter will not begin to execute until after the command  buffer has been enqueued and committed.
--
-- @commandBuffer@ — A valid MTLCommandBuffer.
--
-- @source@ — A valid MTLTexture containing the source image for the filter
--
-- @histogram@ — A valid MTLBuffer to receive the histogram results.
--
-- @histogramOffset@ — Byte offset into histogram buffer at which to write the histogram results. Must be a multiple of 32 bytes.                                  The histogram results / channel are stored together.  The number of channels for which                                  histogram results are stored is determined by the number of channels in the image.                                  If histogramInfo.histogramForAlpha is false and the source image is RGBA then only histogram                                  results for RGB channels are stored.
--
-- The histogram results are stored in the histogram buffer as follows:                                      - histogram results for the R channel for all bins followed by                                      - histogram results for the G channel for all bins followed by                                      - histogram results for the B channel for all bins followed by                                      - histogram results for the A channel for all bins
--
-- ObjC selector: @- encodeToCommandBuffer:sourceTexture:histogram:histogramOffset:@
encodeToCommandBuffer_sourceTexture_histogram_histogramOffset :: IsMPSImageHistogram mpsImageHistogram => mpsImageHistogram -> RawId -> RawId -> RawId -> CULong -> IO ()
encodeToCommandBuffer_sourceTexture_histogram_histogramOffset mpsImageHistogram  commandBuffer source histogram histogramOffset =
    sendMsg mpsImageHistogram (mkSelector "encodeToCommandBuffer:sourceTexture:histogram:histogramOffset:") retVoid [argPtr (castPtr (unRawId commandBuffer) :: Ptr ()), argPtr (castPtr (unRawId source) :: Ptr ()), argPtr (castPtr (unRawId histogram) :: Ptr ()), argCULong histogramOffset]

-- | The amount of space in the output MTLBuffer the histogram will take up.
--
-- This convenience function calculates the minimum amount of space              needed in the output histogram for the results.  The MTLBuffer should              be at least this length, longer if histogramOffset is non-zero.
--
-- @sourceFormat@ — The MTLPixelFormat of the source image. This is                                the source parameter of -encodeToCommandBuffer:                                sourceTexture:histogram:histogramOffset
--
-- Returns: The number of bytes needed to store the result histograms.
--
-- ObjC selector: @- histogramSizeForSourceFormat:@
histogramSizeForSourceFormat :: IsMPSImageHistogram mpsImageHistogram => mpsImageHistogram -> MTLPixelFormat -> IO CULong
histogramSizeForSourceFormat mpsImageHistogram  sourceFormat =
    sendMsg mpsImageHistogram (mkSelector "histogramSizeForSourceFormat:") retCULong [argCULong (coerce sourceFormat)]

-- | zeroHistogram
--
-- Zero-initalize the histogram results
--
-- Indicates that the memory region in which the histogram results are to be written in the              histogram buffer are to be zero-initialized or not. Default: YES.
--
-- ObjC selector: @- zeroHistogram@
zeroHistogram :: IsMPSImageHistogram mpsImageHistogram => mpsImageHistogram -> IO Bool
zeroHistogram mpsImageHistogram  =
    fmap ((/= 0) :: CULong -> Bool) $ sendMsg mpsImageHistogram (mkSelector "zeroHistogram") retCULong []

-- | zeroHistogram
--
-- Zero-initalize the histogram results
--
-- Indicates that the memory region in which the histogram results are to be written in the              histogram buffer are to be zero-initialized or not. Default: YES.
--
-- ObjC selector: @- setZeroHistogram:@
setZeroHistogram :: IsMPSImageHistogram mpsImageHistogram => mpsImageHistogram -> Bool -> IO ()
setZeroHistogram mpsImageHistogram  value =
    sendMsg mpsImageHistogram (mkSelector "setZeroHistogram:") retVoid [argCULong (if value then 1 else 0)]

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @initWithDevice:histogramInfo:@
initWithDevice_histogramInfoSelector :: Selector
initWithDevice_histogramInfoSelector = mkSelector "initWithDevice:histogramInfo:"

-- | @Selector@ for @initWithCoder:device:@
initWithCoder_deviceSelector :: Selector
initWithCoder_deviceSelector = mkSelector "initWithCoder:device:"

-- | @Selector@ for @encodeToCommandBuffer:sourceTexture:histogram:histogramOffset:@
encodeToCommandBuffer_sourceTexture_histogram_histogramOffsetSelector :: Selector
encodeToCommandBuffer_sourceTexture_histogram_histogramOffsetSelector = mkSelector "encodeToCommandBuffer:sourceTexture:histogram:histogramOffset:"

-- | @Selector@ for @histogramSizeForSourceFormat:@
histogramSizeForSourceFormatSelector :: Selector
histogramSizeForSourceFormatSelector = mkSelector "histogramSizeForSourceFormat:"

-- | @Selector@ for @zeroHistogram@
zeroHistogramSelector :: Selector
zeroHistogramSelector = mkSelector "zeroHistogram"

-- | @Selector@ for @setZeroHistogram:@
setZeroHistogramSelector :: Selector
setZeroHistogramSelector = mkSelector "setZeroHistogram:"

