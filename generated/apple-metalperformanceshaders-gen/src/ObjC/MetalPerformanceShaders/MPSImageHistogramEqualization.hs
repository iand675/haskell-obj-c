{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | MPSImageHistogramEqualization
--
-- The MPSImageHistogramEqualization performs equalizes the histogram of an image.              The process is divided into three steps.
--
-- -# Call -initWithDevice:histogramInfo:   This creates a MPSImageHistogramEqualization              object.   It is done when the method returns.
--
-- -# Call -encodeTransform:sourceTexture:histogram:histogramOffset:  This creates a privately held              image transform (i.e. a cumulative distribution function of the histogram) which will be used to               equalize the distribution of the histogram of the source image. This process runs on a MTLCommandBuffer              when it is committed to a MTLCommandQueue. It must complete before the next step can be run.              It may be performed on the same MTLCommandBuffer.  The histogram argument specifies the histogram              buffer which contains the histogram values for sourceTexture.  The sourceTexture argument is used by              encodeTransform to determine the number of channels and therefore which histogram data in histogram               buffer to use. The histogram for sourceTexture must have been computed either on the CPU or using               the MPSImageHistogram kernel
--
-- -# Call -encodeToCommandBuffer:sourceTexture:destinationTexture: to read data from              sourceTexture, apply the equalization transform to it and write to destination texture.              This step is also done on the GPU on a MTLCommandQueue.
--
-- You can reuse the same equalization transform on other images to perform the              same transform on those images. (Since their distribution is probably different,              they will probably not be equalized by it.) This filter usually will not be able               to work in place.
--
-- Generated bindings for @MPSImageHistogramEqualization@.
module ObjC.MetalPerformanceShaders.MPSImageHistogramEqualization
  ( MPSImageHistogramEqualization
  , IsMPSImageHistogramEqualization(..)
  , initWithDevice_histogramInfo
  , initWithCoder_device
  , encodeTransformToCommandBuffer_sourceTexture_histogram_histogramOffset
  , initWithDevice_histogramInfoSelector
  , initWithCoder_deviceSelector
  , encodeTransformToCommandBuffer_sourceTexture_histogram_histogramOffsetSelector


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
-- @device@ — The device the filter will run on
--
-- @histogramInfo@ — Pointer to the MPSHistogramInfo struct
--
-- Returns: A valid MPSImageHistogramEqualization object or nil, if failure.
--
-- ObjC selector: @- initWithDevice:histogramInfo:@
initWithDevice_histogramInfo :: IsMPSImageHistogramEqualization mpsImageHistogramEqualization => mpsImageHistogramEqualization -> RawId -> Const RawId -> IO (Id MPSImageHistogramEqualization)
initWithDevice_histogramInfo mpsImageHistogramEqualization  device histogramInfo =
    sendMsg mpsImageHistogramEqualization (mkSelector "initWithDevice:histogramInfo:") (retPtr retVoid) [argPtr (castPtr (unRawId device) :: Ptr ()), argPtr (castPtr (unRawId (unConst histogramInfo)) :: Ptr ())] >>= ownedObject . castPtr

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
initWithCoder_device :: (IsMPSImageHistogramEqualization mpsImageHistogramEqualization, IsNSCoder aDecoder) => mpsImageHistogramEqualization -> aDecoder -> RawId -> IO (Id MPSImageHistogramEqualization)
initWithCoder_device mpsImageHistogramEqualization  aDecoder device =
  withObjCPtr aDecoder $ \raw_aDecoder ->
      sendMsg mpsImageHistogramEqualization (mkSelector "initWithCoder:device:") (retPtr retVoid) [argPtr (castPtr raw_aDecoder :: Ptr ()), argPtr (castPtr (unRawId device) :: Ptr ())] >>= ownedObject . castPtr

-- | Encode the transform function to a command buffer using a MTLComputeCommandEncoder.            The transform function computes the equalization lookup table.
--
-- The transform function will not begin to execute until after the command              buffer has been enqueued and committed.  This step will need to be repeated              with the new MPSKernel if -copyWithZone:device or -copyWithZone: is called.              The transform is stored as internal state to the object. You still need to               call -encodeToCommandBuffer:sourceTexture:destinationTexture: afterward              to apply the transform to produce a result texture.
--
-- @commandBuffer@ — A valid MTLCommandBuffer.
--
-- @source@ — A valid MTLTexture containing the source image for the filter.
--
-- @histogram@ — A valid MTLBuffer containing the histogram results for an image.  This filter                          will use these histogram results to generate the cumulative histogram for equalizing                          the image.  The histogram results / channel are stored together.  The number of channels                          for which histogram results are stored is determined by the number of channels in the image.                          If histogramInfo.histogramForAlpha is false and the source image is RGBA then only histogram                          results for RGB channels are stored.
--
-- @histogramOffset@ — A byte offset into the histogram MTLBuffer where the histogram starts. Must conform to                          alignment requirements for [MTLComputeCommandEncoder setBuffer:offset:atIndex:] offset                          parameter.
--
-- ObjC selector: @- encodeTransformToCommandBuffer:sourceTexture:histogram:histogramOffset:@
encodeTransformToCommandBuffer_sourceTexture_histogram_histogramOffset :: IsMPSImageHistogramEqualization mpsImageHistogramEqualization => mpsImageHistogramEqualization -> RawId -> RawId -> RawId -> CULong -> IO ()
encodeTransformToCommandBuffer_sourceTexture_histogram_histogramOffset mpsImageHistogramEqualization  commandBuffer source histogram histogramOffset =
    sendMsg mpsImageHistogramEqualization (mkSelector "encodeTransformToCommandBuffer:sourceTexture:histogram:histogramOffset:") retVoid [argPtr (castPtr (unRawId commandBuffer) :: Ptr ()), argPtr (castPtr (unRawId source) :: Ptr ()), argPtr (castPtr (unRawId histogram) :: Ptr ()), argCULong histogramOffset]

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @initWithDevice:histogramInfo:@
initWithDevice_histogramInfoSelector :: Selector
initWithDevice_histogramInfoSelector = mkSelector "initWithDevice:histogramInfo:"

-- | @Selector@ for @initWithCoder:device:@
initWithCoder_deviceSelector :: Selector
initWithCoder_deviceSelector = mkSelector "initWithCoder:device:"

-- | @Selector@ for @encodeTransformToCommandBuffer:sourceTexture:histogram:histogramOffset:@
encodeTransformToCommandBuffer_sourceTexture_histogram_histogramOffsetSelector :: Selector
encodeTransformToCommandBuffer_sourceTexture_histogram_histogramOffsetSelector = mkSelector "encodeTransformToCommandBuffer:sourceTexture:histogram:histogramOffset:"

