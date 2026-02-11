{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | MPSImageHistogramSpecification
--
-- The MPSImageHistogramSpecification performs a histogram specification operation on an image.              It is a generalized version of histogram equalization operation.  The histogram specificaiton filter              converts the image so that its histogram matches the desired histogram.
--
-- Generated bindings for @MPSImageHistogramSpecification@.
module ObjC.MetalPerformanceShaders.MPSImageHistogramSpecification
  ( MPSImageHistogramSpecification
  , IsMPSImageHistogramSpecification(..)
  , initWithCoder_device
  , encodeTransformToCommandBuffer_sourceTexture_sourceHistogram_sourceHistogramOffset_desiredHistogram_desiredHistogramOffset
  , initWithCoder_deviceSelector
  , encodeTransformToCommandBuffer_sourceTexture_sourceHistogram_sourceHistogramOffset_desiredHistogram_desiredHistogramOffsetSelector


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
initWithCoder_device :: (IsMPSImageHistogramSpecification mpsImageHistogramSpecification, IsNSCoder aDecoder) => mpsImageHistogramSpecification -> aDecoder -> RawId -> IO (Id MPSImageHistogramSpecification)
initWithCoder_device mpsImageHistogramSpecification  aDecoder device =
withObjCPtr aDecoder $ \raw_aDecoder ->
    sendMsg mpsImageHistogramSpecification (mkSelector "initWithCoder:device:") (retPtr retVoid) [argPtr (castPtr raw_aDecoder :: Ptr ()), argPtr (castPtr (unRawId device) :: Ptr ())] >>= ownedObject . castPtr

-- | Encode the transform function to a command buffer using a MTLComputeCommandEncoder.            The transform function computes the specification lookup table.
--
-- The transform function will not begin to execute until after the command              buffer has been enqueued and committed. This step will need to be repeated              with the new MPSKernel if -copyWithZone:device or -copyWithZone: is called.
--
-- @commandBuffer@ — A valid MTLCommandBuffer.
--
-- @source@ — A valid MTLTexture containing the source image for the filter.
--
-- @sourceHistogram@ — A valid MTLBuffer containing the histogram results for the source image.  This filter                          will use these histogram results to generate the cumulative histogram for equalizing                          the image.  The histogram results / channel are stored together.  The number of channels                          for which histogram results are stored is determined by the number of channels in the image.                          If histogramInfo.histogramForAlpha is false and the source image is RGBA then only histogram                          results for RGB channels are stored.
--
-- @sourceHistogramOffset@ — A byte offset into the sourceHistogram MTLBuffer where the histogram starts. Must conform to                                  alignment requirements for [MTLComputeCommandEncoder setBuffer:offset:atIndex:] offset                                  parameter.
--
-- @desiredHistogram@ — A valid MTLBuffer containing the desired histogram results for the source image.                          The histogram results / channel are stored together.  The number of channels                          for which histogram results are stored is determined by the number of channels in the image.                          If histogramInfo.histogramForAlpha is false and the source image is RGBA then only histogram                          results for RGB channels are stored.
--
-- @desiredHistogramOffset@ — A byte offset into the desiredHistogram MTLBuffer where the histogram starts. Must conform to                                  alignment requirements for [MTLComputeCommandEncoder setBuffer:offset:atIndex:] offset                                  parameter.
--
-- ObjC selector: @- encodeTransformToCommandBuffer:sourceTexture:sourceHistogram:sourceHistogramOffset:desiredHistogram:desiredHistogramOffset:@
encodeTransformToCommandBuffer_sourceTexture_sourceHistogram_sourceHistogramOffset_desiredHistogram_desiredHistogramOffset :: IsMPSImageHistogramSpecification mpsImageHistogramSpecification => mpsImageHistogramSpecification -> RawId -> RawId -> RawId -> CULong -> RawId -> CULong -> IO ()
encodeTransformToCommandBuffer_sourceTexture_sourceHistogram_sourceHistogramOffset_desiredHistogram_desiredHistogramOffset mpsImageHistogramSpecification  commandBuffer source sourceHistogram sourceHistogramOffset desiredHistogram desiredHistogramOffset =
  sendMsg mpsImageHistogramSpecification (mkSelector "encodeTransformToCommandBuffer:sourceTexture:sourceHistogram:sourceHistogramOffset:desiredHistogram:desiredHistogramOffset:") retVoid [argPtr (castPtr (unRawId commandBuffer) :: Ptr ()), argPtr (castPtr (unRawId source) :: Ptr ()), argPtr (castPtr (unRawId sourceHistogram) :: Ptr ()), argCULong (fromIntegral sourceHistogramOffset), argPtr (castPtr (unRawId desiredHistogram) :: Ptr ()), argCULong (fromIntegral desiredHistogramOffset)]

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @initWithCoder:device:@
initWithCoder_deviceSelector :: Selector
initWithCoder_deviceSelector = mkSelector "initWithCoder:device:"

-- | @Selector@ for @encodeTransformToCommandBuffer:sourceTexture:sourceHistogram:sourceHistogramOffset:desiredHistogram:desiredHistogramOffset:@
encodeTransformToCommandBuffer_sourceTexture_sourceHistogram_sourceHistogramOffset_desiredHistogram_desiredHistogramOffsetSelector :: Selector
encodeTransformToCommandBuffer_sourceTexture_sourceHistogram_sourceHistogramOffset_desiredHistogram_desiredHistogramOffsetSelector = mkSelector "encodeTransformToCommandBuffer:sourceTexture:sourceHistogram:sourceHistogramOffset:desiredHistogram:desiredHistogramOffset:"

