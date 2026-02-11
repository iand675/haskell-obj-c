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
  , initWithDevice_histogramInfo
  , initWithCoder_device
  , encodeTransformToCommandBuffer_sourceTexture_sourceHistogram_sourceHistogramOffset_desiredHistogram_desiredHistogramOffset
  , initWithDevice_histogramInfoSelector
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

-- | Specifies information about the histogram for the channels of an image.
--
-- The MPSImageHistogramSpecification applies a transfor to convert the histogram               to a specified histogram. The process is divided into three steps:
--
-- -# Call -initWithDevice:histogramInfo:   This creates a MPSImageHistogramSpecification              object.  It is done when the method returns.
--
-- -# Call -encodeTransform:sourceTexture:sourceHistogram:sourceHistogramOffset:desiredHistogram:              desiredHistogramOffset: This creates a privately held image transform which will convert the              the distribution of the source histogram to the desired histogram. This process runs on a               MTLCommandBuffer when it is committed to a MTLCommandQueue. It must complete before the next               step can be run. It may be performed on the same MTLCommandBuffer.  The sourceTexture argument               is used by encodeTransform to determine the number of channels and therefore which histogram data               in sourceHistogram buffer to use. The sourceHistogram and desiredHistogram must have been computed               either on the CPU or using the MPSImageHistogram kernel
--
-- -# Call -encodeToCommandBuffer:sourceTexture:destinationTexture: to read data from              sourceTexture, apply the transform to it and write to destination texture.              This step is also done on the GPU on a MTLCommandQueue.
--
-- You can reuse the same specification transform on other images to perform the              same transform on those images. (Since their starting distribution is probably              different, they will probably not arrive at the same distribution as the desired              histogram.) This filter usually will not be able to work in place.
--
-- @device@ — The device the filter will run on
--
-- @histogramInfo@ — Pointer to the MPSHistogramInfo struct
--
-- Returns: A valid MPSImageHistogramSpecification object or nil, if failure.
--
-- ObjC selector: @- initWithDevice:histogramInfo:@
initWithDevice_histogramInfo :: IsMPSImageHistogramSpecification mpsImageHistogramSpecification => mpsImageHistogramSpecification -> RawId -> Const RawId -> IO (Id MPSImageHistogramSpecification)
initWithDevice_histogramInfo mpsImageHistogramSpecification  device histogramInfo =
    sendMsg mpsImageHistogramSpecification (mkSelector "initWithDevice:histogramInfo:") (retPtr retVoid) [argPtr (castPtr (unRawId device) :: Ptr ()), argPtr (castPtr (unRawId (unConst histogramInfo)) :: Ptr ())] >>= ownedObject . castPtr

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
    sendMsg mpsImageHistogramSpecification (mkSelector "encodeTransformToCommandBuffer:sourceTexture:sourceHistogram:sourceHistogramOffset:desiredHistogram:desiredHistogramOffset:") retVoid [argPtr (castPtr (unRawId commandBuffer) :: Ptr ()), argPtr (castPtr (unRawId source) :: Ptr ()), argPtr (castPtr (unRawId sourceHistogram) :: Ptr ()), argCULong sourceHistogramOffset, argPtr (castPtr (unRawId desiredHistogram) :: Ptr ()), argCULong desiredHistogramOffset]

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @initWithDevice:histogramInfo:@
initWithDevice_histogramInfoSelector :: Selector
initWithDevice_histogramInfoSelector = mkSelector "initWithDevice:histogramInfo:"

-- | @Selector@ for @initWithCoder:device:@
initWithCoder_deviceSelector :: Selector
initWithCoder_deviceSelector = mkSelector "initWithCoder:device:"

-- | @Selector@ for @encodeTransformToCommandBuffer:sourceTexture:sourceHistogram:sourceHistogramOffset:desiredHistogram:desiredHistogramOffset:@
encodeTransformToCommandBuffer_sourceTexture_sourceHistogram_sourceHistogramOffset_desiredHistogram_desiredHistogramOffsetSelector :: Selector
encodeTransformToCommandBuffer_sourceTexture_sourceHistogram_sourceHistogramOffset_desiredHistogram_desiredHistogramOffsetSelector = mkSelector "encodeTransformToCommandBuffer:sourceTexture:sourceHistogram:sourceHistogramOffset:desiredHistogram:desiredHistogramOffset:"

