{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | MPSImageEDLines
--
-- The MPSImageEDLInes class implements the EDLines line segmenting algorithm using edge-drawing (ED)              described here              https://ieeexplore.ieee.org/document/6116138
--
-- The EDLInes algorithm consists of 5 steps, the first 4 of which describe the ED algorithm:              1. Blur the source image using a Gaussian blur with a sigma parameter              2. Use horizontal and vertical Sobel filters to find a gradient magnitude and                direction.                  G = sqrt(Sx^2 + Sy^2)                  G_ang = arctan(Sy / Sx)              3. Compute anchor points, points with a high probability of being edge pixels.                Anchor points are local maxima, in the gradient image that lie on row and column                multiples of the detailRatio. This parameter effectively downsamples the gradient                image, and directly influences the density of anchor points. A larger detailRatio results                in fewer fine grained details, leaving long, main lines.              4. Anchor points are traced in a forward and backward direction along the gradient direction, until                the gradient falls below some gradientThreshold parameter or the edge of the image is reached.                The paths traced become an edge map of the image.              5. Points in the edges are fit to a line), and extended along the edge until the line error crosses a                lineErrorThreshold. Lines which are beyond a minimum length are labelled line segments and                will be outputs of the algorithm.
--
-- Generated bindings for @MPSImageEDLines@.
module ObjC.MetalPerformanceShaders.MPSImageEDLines
  ( MPSImageEDLines
  , IsMPSImageEDLines(..)
  , initWithDevice_gaussianSigma_minLineLength_maxLines_detailRatio_gradientThreshold_lineErrorThreshold_mergeLocalityThreshold
  , initWithCoder_device
  , encodeToCommandBuffer_sourceTexture_destinationTexture_endpointBuffer_endpointOffset
  , gaussianSigma
  , minLineLength
  , setMinLineLength
  , maxLines
  , setMaxLines
  , detailRatio
  , setDetailRatio
  , gradientThreshold
  , setGradientThreshold
  , lineErrorThreshold
  , setLineErrorThreshold
  , mergeLocalityThreshold
  , setMergeLocalityThreshold
  , initWithDevice_gaussianSigma_minLineLength_maxLines_detailRatio_gradientThreshold_lineErrorThreshold_mergeLocalityThresholdSelector
  , initWithCoder_deviceSelector
  , encodeToCommandBuffer_sourceTexture_destinationTexture_endpointBuffer_endpointOffsetSelector
  , gaussianSigmaSelector
  , minLineLengthSelector
  , setMinLineLengthSelector
  , maxLinesSelector
  , setMaxLinesSelector
  , detailRatioSelector
  , setDetailRatioSelector
  , gradientThresholdSelector
  , setGradientThresholdSelector
  , lineErrorThresholdSelector
  , setLineErrorThresholdSelector
  , mergeLocalityThresholdSelector
  , setMergeLocalityThresholdSelector


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
import ObjC.Foundation.Internal.Classes

-- | Initialize an EDLines kernel on a given device with specified parameters.
--
-- @device@ — The device the filter will run on
--
-- @gaussianSigma@ — The standard deviation of gaussian blur filter.                          Gaussian weight, centered at 0, at integer grid i is given as
--
-- w(i) = 1/sqrt(2*pi*sigma) * exp(-i^2/(2*sigma^2))
--
-- If we take cut off at 1% of w(0) (max weight) beyond which weights                          are considered 0, we have
--
-- ceil (sqrt(-log(0.01)*2)*sigma) ~ ceil(3.7*sigma)
--
-- as rough estimate of filter width
--
-- @minLineLength@ — The minimum length of output line segments.
--
-- @maxLines@ — The maximum amount of lines for the EDLines algorithm to output. The size of the                                endpointBuffer supplied at encode must be >= maxLines * 4 * sizeof(unsigned short) + sizeof(uint32_t).
--
-- @detailRatio@ — The detailRatio to use in the EDLines algorithm, which                                inversely effects the number of anchor points
--
-- @gradientThreshold@ — Any pixel with a gradient below the gradientThreshold will                                not be considerd an edge
--
-- @lineErrorThreshold@ — The limit of how much error a line segment can have relative                                to the edge it represents
--
-- @mergeLocalityThreshold@ — Determines how many pixels apart two lines can deviate spatially and still be merged.                                     This value is normalized to the diagonal length of the image.
--
-- Returns: A valid object or nil, if failure.
--
-- ObjC selector: @- initWithDevice:gaussianSigma:minLineLength:maxLines:detailRatio:gradientThreshold:lineErrorThreshold:mergeLocalityThreshold:@
initWithDevice_gaussianSigma_minLineLength_maxLines_detailRatio_gradientThreshold_lineErrorThreshold_mergeLocalityThreshold :: IsMPSImageEDLines mpsImageEDLines => mpsImageEDLines -> RawId -> Const CFloat -> Const CUShort -> Const CULong -> Const CUShort -> Const CFloat -> Const CFloat -> Const CFloat -> IO (Id MPSImageEDLines)
initWithDevice_gaussianSigma_minLineLength_maxLines_detailRatio_gradientThreshold_lineErrorThreshold_mergeLocalityThreshold mpsImageEDLines  device gaussianSigma minLineLength maxLines detailRatio gradientThreshold lineErrorThreshold mergeLocalityThreshold =
  sendMsg mpsImageEDLines (mkSelector "initWithDevice:gaussianSigma:minLineLength:maxLines:detailRatio:gradientThreshold:lineErrorThreshold:mergeLocalityThreshold:") (retPtr retVoid) [argPtr (castPtr (unRawId device) :: Ptr ()), argCFloat (fromIntegral (unConst gaussianSigma)), argCUInt (fromIntegral (unConst minLineLength)), argCULong (fromIntegral (unConst maxLines)), argCUInt (fromIntegral (unConst detailRatio)), argCFloat (fromIntegral (unConst gradientThreshold)), argCFloat (fromIntegral (unConst lineErrorThreshold)), argCFloat (fromIntegral (unConst mergeLocalityThreshold))] >>= ownedObject . castPtr

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
initWithCoder_device :: (IsMPSImageEDLines mpsImageEDLines, IsNSCoder aDecoder) => mpsImageEDLines -> aDecoder -> RawId -> IO (Id MPSImageEDLines)
initWithCoder_device mpsImageEDLines  aDecoder device =
withObjCPtr aDecoder $ \raw_aDecoder ->
    sendMsg mpsImageEDLines (mkSelector "initWithCoder:device:") (retPtr retVoid) [argPtr (castPtr raw_aDecoder :: Ptr ()), argPtr (castPtr (unRawId device) :: Ptr ())] >>= ownedObject . castPtr

-- | Encode the filter to a command buffer using a MTLComputeCommandEncoder.
--
-- The filter will not begin to execute until after the command    buffer has been enqueued and committed.
--
-- @commandBuffer@ — A valid MTLCommandBuffer.
--
-- @source@ — A valid MTLTexture containing the source image for the filter
--
-- @dest@ — A valid MTLTexture containing the destination image for the filter. If not nil, the output will be the edges                   found through the Edge Drawing algorithm.
--
-- @endpointBuffer@ — A valid MTLBuffer to receive the line segment count and endpoint results.
--
-- @endpointOffset@ — Byte offset into endpoint buffer at which to write the  line segment endpoint results. Must be a multiple of 32 bytes.                                The total line segment count and the line segment endpoints are written to the endpoint buffer. The count                                is written as a uint32_t at the start of the buffer. The line segments are written to the endpoint buffer as                                start and end pixel coordinates of the segment. Coordinates are stored as unsigned short pairs, and a                                single line segment will consist of two pairs, or four total unsigned shorts. The endpoint buffer size must                                be >= 4 * maxLines * sizeof(unsigned short) + sizeof(uint32_t).
--
-- ObjC selector: @- encodeToCommandBuffer:sourceTexture:destinationTexture:endpointBuffer:endpointOffset:@
encodeToCommandBuffer_sourceTexture_destinationTexture_endpointBuffer_endpointOffset :: IsMPSImageEDLines mpsImageEDLines => mpsImageEDLines -> RawId -> RawId -> RawId -> RawId -> CULong -> IO ()
encodeToCommandBuffer_sourceTexture_destinationTexture_endpointBuffer_endpointOffset mpsImageEDLines  commandBuffer source dest endpointBuffer endpointOffset =
  sendMsg mpsImageEDLines (mkSelector "encodeToCommandBuffer:sourceTexture:destinationTexture:endpointBuffer:endpointOffset:") retVoid [argPtr (castPtr (unRawId commandBuffer) :: Ptr ()), argPtr (castPtr (unRawId source) :: Ptr ()), argPtr (castPtr (unRawId dest) :: Ptr ()), argPtr (castPtr (unRawId endpointBuffer) :: Ptr ()), argCULong (fromIntegral endpointOffset)]

-- | sigma
--
-- Read-only sigma value used in performing Gaussian blur of the image.            Default is 2.0
--
-- ObjC selector: @- gaussianSigma@
gaussianSigma :: IsMPSImageEDLines mpsImageEDLines => mpsImageEDLines -> IO CFloat
gaussianSigma mpsImageEDLines  =
  sendMsg mpsImageEDLines (mkSelector "gaussianSigma") retCFloat []

-- | minLineLength
--
-- Read-write value used to set the minimum length of a line segment.            Default is 32
--
-- ObjC selector: @- minLineLength@
minLineLength :: IsMPSImageEDLines mpsImageEDLines => mpsImageEDLines -> IO CUShort
minLineLength mpsImageEDLines  =
  fmap fromIntegral $ sendMsg mpsImageEDLines (mkSelector "minLineLength") retCUInt []

-- | minLineLength
--
-- Read-write value used to set the minimum length of a line segment.            Default is 32
--
-- ObjC selector: @- setMinLineLength:@
setMinLineLength :: IsMPSImageEDLines mpsImageEDLines => mpsImageEDLines -> CUShort -> IO ()
setMinLineLength mpsImageEDLines  value =
  sendMsg mpsImageEDLines (mkSelector "setMinLineLength:") retVoid [argCUInt (fromIntegral value)]

-- | maxLines
--
-- Read-write value used to set the max number of line segments to be written out.            The endpointBuffer at encode must be >= maxLines * 4 * sizeof(unsigned short) + sizeof(uint32_t).            Default is 256
--
-- ObjC selector: @- maxLines@
maxLines :: IsMPSImageEDLines mpsImageEDLines => mpsImageEDLines -> IO CULong
maxLines mpsImageEDLines  =
  sendMsg mpsImageEDLines (mkSelector "maxLines") retCULong []

-- | maxLines
--
-- Read-write value used to set the max number of line segments to be written out.            The endpointBuffer at encode must be >= maxLines * 4 * sizeof(unsigned short) + sizeof(uint32_t).            Default is 256
--
-- ObjC selector: @- setMaxLines:@
setMaxLines :: IsMPSImageEDLines mpsImageEDLines => mpsImageEDLines -> CULong -> IO ()
setMaxLines mpsImageEDLines  value =
  sendMsg mpsImageEDLines (mkSelector "setMaxLines:") retVoid [argCULong (fromIntegral value)]

-- | detailRatio
--
-- Read-write value used to set the detailRatio to use in the EDLines algorithm            Default is 32
--
-- ObjC selector: @- detailRatio@
detailRatio :: IsMPSImageEDLines mpsImageEDLines => mpsImageEDLines -> IO CUShort
detailRatio mpsImageEDLines  =
  fmap fromIntegral $ sendMsg mpsImageEDLines (mkSelector "detailRatio") retCUInt []

-- | detailRatio
--
-- Read-write value used to set the detailRatio to use in the EDLines algorithm            Default is 32
--
-- ObjC selector: @- setDetailRatio:@
setDetailRatio :: IsMPSImageEDLines mpsImageEDLines => mpsImageEDLines -> CUShort -> IO ()
setDetailRatio mpsImageEDLines  value =
  sendMsg mpsImageEDLines (mkSelector "setDetailRatio:") retVoid [argCUInt (fromIntegral value)]

-- | gradientThreshold
--
-- Read-write value used to set the threshold for a pixel to be considered an edge            Default is 0.2
--
-- ObjC selector: @- gradientThreshold@
gradientThreshold :: IsMPSImageEDLines mpsImageEDLines => mpsImageEDLines -> IO CFloat
gradientThreshold mpsImageEDLines  =
  sendMsg mpsImageEDLines (mkSelector "gradientThreshold") retCFloat []

-- | gradientThreshold
--
-- Read-write value used to set the threshold for a pixel to be considered an edge            Default is 0.2
--
-- ObjC selector: @- setGradientThreshold:@
setGradientThreshold :: IsMPSImageEDLines mpsImageEDLines => mpsImageEDLines -> CFloat -> IO ()
setGradientThreshold mpsImageEDLines  value =
  sendMsg mpsImageEDLines (mkSelector "setGradientThreshold:") retVoid [argCFloat (fromIntegral value)]

-- | lineErrorThreshold
--
-- Read-write value used to set the limit on error for a line segment relative to the edge it fits            Default is 0.05
--
-- ObjC selector: @- lineErrorThreshold@
lineErrorThreshold :: IsMPSImageEDLines mpsImageEDLines => mpsImageEDLines -> IO CFloat
lineErrorThreshold mpsImageEDLines  =
  sendMsg mpsImageEDLines (mkSelector "lineErrorThreshold") retCFloat []

-- | lineErrorThreshold
--
-- Read-write value used to set the limit on error for a line segment relative to the edge it fits            Default is 0.05
--
-- ObjC selector: @- setLineErrorThreshold:@
setLineErrorThreshold :: IsMPSImageEDLines mpsImageEDLines => mpsImageEDLines -> CFloat -> IO ()
setLineErrorThreshold mpsImageEDLines  value =
  sendMsg mpsImageEDLines (mkSelector "setLineErrorThreshold:") retVoid [argCFloat (fromIntegral value)]

-- | mergeLocalityThreshold
--
-- Read-write value used to set how many pixels apart two lines can deviate spatially and still be merged.            Default is 0.0025
--
-- ObjC selector: @- mergeLocalityThreshold@
mergeLocalityThreshold :: IsMPSImageEDLines mpsImageEDLines => mpsImageEDLines -> IO CFloat
mergeLocalityThreshold mpsImageEDLines  =
  sendMsg mpsImageEDLines (mkSelector "mergeLocalityThreshold") retCFloat []

-- | mergeLocalityThreshold
--
-- Read-write value used to set how many pixels apart two lines can deviate spatially and still be merged.            Default is 0.0025
--
-- ObjC selector: @- setMergeLocalityThreshold:@
setMergeLocalityThreshold :: IsMPSImageEDLines mpsImageEDLines => mpsImageEDLines -> CFloat -> IO ()
setMergeLocalityThreshold mpsImageEDLines  value =
  sendMsg mpsImageEDLines (mkSelector "setMergeLocalityThreshold:") retVoid [argCFloat (fromIntegral value)]

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @initWithDevice:gaussianSigma:minLineLength:maxLines:detailRatio:gradientThreshold:lineErrorThreshold:mergeLocalityThreshold:@
initWithDevice_gaussianSigma_minLineLength_maxLines_detailRatio_gradientThreshold_lineErrorThreshold_mergeLocalityThresholdSelector :: Selector
initWithDevice_gaussianSigma_minLineLength_maxLines_detailRatio_gradientThreshold_lineErrorThreshold_mergeLocalityThresholdSelector = mkSelector "initWithDevice:gaussianSigma:minLineLength:maxLines:detailRatio:gradientThreshold:lineErrorThreshold:mergeLocalityThreshold:"

-- | @Selector@ for @initWithCoder:device:@
initWithCoder_deviceSelector :: Selector
initWithCoder_deviceSelector = mkSelector "initWithCoder:device:"

-- | @Selector@ for @encodeToCommandBuffer:sourceTexture:destinationTexture:endpointBuffer:endpointOffset:@
encodeToCommandBuffer_sourceTexture_destinationTexture_endpointBuffer_endpointOffsetSelector :: Selector
encodeToCommandBuffer_sourceTexture_destinationTexture_endpointBuffer_endpointOffsetSelector = mkSelector "encodeToCommandBuffer:sourceTexture:destinationTexture:endpointBuffer:endpointOffset:"

-- | @Selector@ for @gaussianSigma@
gaussianSigmaSelector :: Selector
gaussianSigmaSelector = mkSelector "gaussianSigma"

-- | @Selector@ for @minLineLength@
minLineLengthSelector :: Selector
minLineLengthSelector = mkSelector "minLineLength"

-- | @Selector@ for @setMinLineLength:@
setMinLineLengthSelector :: Selector
setMinLineLengthSelector = mkSelector "setMinLineLength:"

-- | @Selector@ for @maxLines@
maxLinesSelector :: Selector
maxLinesSelector = mkSelector "maxLines"

-- | @Selector@ for @setMaxLines:@
setMaxLinesSelector :: Selector
setMaxLinesSelector = mkSelector "setMaxLines:"

-- | @Selector@ for @detailRatio@
detailRatioSelector :: Selector
detailRatioSelector = mkSelector "detailRatio"

-- | @Selector@ for @setDetailRatio:@
setDetailRatioSelector :: Selector
setDetailRatioSelector = mkSelector "setDetailRatio:"

-- | @Selector@ for @gradientThreshold@
gradientThresholdSelector :: Selector
gradientThresholdSelector = mkSelector "gradientThreshold"

-- | @Selector@ for @setGradientThreshold:@
setGradientThresholdSelector :: Selector
setGradientThresholdSelector = mkSelector "setGradientThreshold:"

-- | @Selector@ for @lineErrorThreshold@
lineErrorThresholdSelector :: Selector
lineErrorThresholdSelector = mkSelector "lineErrorThreshold"

-- | @Selector@ for @setLineErrorThreshold:@
setLineErrorThresholdSelector :: Selector
setLineErrorThresholdSelector = mkSelector "setLineErrorThreshold:"

-- | @Selector@ for @mergeLocalityThreshold@
mergeLocalityThresholdSelector :: Selector
mergeLocalityThresholdSelector = mkSelector "mergeLocalityThreshold"

-- | @Selector@ for @setMergeLocalityThreshold:@
setMergeLocalityThresholdSelector :: Selector
setMergeLocalityThresholdSelector = mkSelector "setMergeLocalityThreshold:"

