{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | VNGenerateOpticalFlowRequest will determine directional change vectors for each pixel in the targeted image to transform it into the image processed        by the request handler, reporting this result with a single VNPixelBufferObservation.
--
-- Because this request works at the pixel level, both images must have the same dimensions in order for the request to be successfully performed.				Setting a region of interest will isolate where the change determination is performed; however, the resultant observation will still be reported				with a full resolution VNPixelBufferObservation.
--
-- Optical flow requests are very resource intensive, so it is recommended that only one request at a time be created and that the handler                where the request was issued be released immediately after generating optical flows.
--
-- Example usage:
--
-- - (nullable VNPixelBufferObservation*) opticalFlowFromImage:(CVPixelBufferRef)fromImage toImage:(CVPixelBuffer)toImage error:(NSError**)error					{						VNImageRequestHandler* imageRequestHandler = [[VNImageRequestHandler alloc] initWithCVPixelBuffer:fromImage options:\@{}];						VNGenerateOpticalFlowRequest* request = [[VNGenerateOpticalFlowRequest alloc] initWithTargetedCVPixelBuffer:toImage options:\@{}];						if (![imageRequestHandler performRequests:\@[ request ] error:error])						{							return nil;						}
--
-- return [[request results] firstObject];					}
--
-- Generated bindings for @VNGenerateOpticalFlowRequest@.
module ObjC.Vision.VNGenerateOpticalFlowRequest
  ( VNGenerateOpticalFlowRequest
  , IsVNGenerateOpticalFlowRequest(..)
  , computationAccuracy
  , setComputationAccuracy
  , outputPixelFormat
  , setOutputPixelFormat
  , keepNetworkOutput
  , setKeepNetworkOutput
  , results
  , computationAccuracySelector
  , setComputationAccuracySelector
  , outputPixelFormatSelector
  , setOutputPixelFormatSelector
  , keepNetworkOutputSelector
  , setKeepNetworkOutputSelector
  , resultsSelector

  -- * Enum types
  , VNGenerateOpticalFlowRequestComputationAccuracy(VNGenerateOpticalFlowRequestComputationAccuracy)
  , pattern VNGenerateOpticalFlowRequestComputationAccuracyLow
  , pattern VNGenerateOpticalFlowRequestComputationAccuracyMedium
  , pattern VNGenerateOpticalFlowRequestComputationAccuracyHigh
  , pattern VNGenerateOpticalFlowRequestComputationAccuracyVeryHigh

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

import ObjC.Vision.Internal.Classes
import ObjC.Vision.Internal.Enums
import ObjC.Foundation.Internal.Classes

-- | The level of accuracy used to compute the optical flow. Default is VNGenerateOpticalFlowRequestComputationAccuracyMedium.
--
-- The computational time typically trends with the accuracy level.  This parameter allows for selective tuning by the client application.
--
-- ObjC selector: @- computationAccuracy@
computationAccuracy :: IsVNGenerateOpticalFlowRequest vnGenerateOpticalFlowRequest => vnGenerateOpticalFlowRequest -> IO VNGenerateOpticalFlowRequestComputationAccuracy
computationAccuracy vnGenerateOpticalFlowRequest  =
  fmap (coerce :: CULong -> VNGenerateOpticalFlowRequestComputationAccuracy) $ sendMsg vnGenerateOpticalFlowRequest (mkSelector "computationAccuracy") retCULong []

-- | The level of accuracy used to compute the optical flow. Default is VNGenerateOpticalFlowRequestComputationAccuracyMedium.
--
-- The computational time typically trends with the accuracy level.  This parameter allows for selective tuning by the client application.
--
-- ObjC selector: @- setComputationAccuracy:@
setComputationAccuracy :: IsVNGenerateOpticalFlowRequest vnGenerateOpticalFlowRequest => vnGenerateOpticalFlowRequest -> VNGenerateOpticalFlowRequestComputationAccuracy -> IO ()
setComputationAccuracy vnGenerateOpticalFlowRequest  value =
  sendMsg vnGenerateOpticalFlowRequest (mkSelector "setComputationAccuracy:") retVoid [argCULong (coerce value)]

-- | Pixel format type of the output buffer. Valid values are kCVPixelFormatType_TwoComponent32Float and kCVPixelFormatType_TwoComponent16Half.        Default is kCVPixelFormatType_TwoComponent32Float.
--
-- ObjC selector: @- outputPixelFormat@
outputPixelFormat :: IsVNGenerateOpticalFlowRequest vnGenerateOpticalFlowRequest => vnGenerateOpticalFlowRequest -> IO CUInt
outputPixelFormat vnGenerateOpticalFlowRequest  =
  sendMsg vnGenerateOpticalFlowRequest (mkSelector "outputPixelFormat") retCUInt []

-- | Pixel format type of the output buffer. Valid values are kCVPixelFormatType_TwoComponent32Float and kCVPixelFormatType_TwoComponent16Half.        Default is kCVPixelFormatType_TwoComponent32Float.
--
-- ObjC selector: @- setOutputPixelFormat:@
setOutputPixelFormat :: IsVNGenerateOpticalFlowRequest vnGenerateOpticalFlowRequest => vnGenerateOpticalFlowRequest -> CUInt -> IO ()
setOutputPixelFormat vnGenerateOpticalFlowRequest  value =
  sendMsg vnGenerateOpticalFlowRequest (mkSelector "setOutputPixelFormat:") retVoid [argCUInt (fromIntegral value)]

-- | Setting this to YES will keep the raw pixel buffer coming from the the ML network. The default is NO.
--
-- When set to YES, the outputPixelFormat is ignored. Setting this for revision 1 is a no-op as it is not ML-based.
--
-- ObjC selector: @- keepNetworkOutput@
keepNetworkOutput :: IsVNGenerateOpticalFlowRequest vnGenerateOpticalFlowRequest => vnGenerateOpticalFlowRequest -> IO Bool
keepNetworkOutput vnGenerateOpticalFlowRequest  =
  fmap ((/= 0) :: CULong -> Bool) $ sendMsg vnGenerateOpticalFlowRequest (mkSelector "keepNetworkOutput") retCULong []

-- | Setting this to YES will keep the raw pixel buffer coming from the the ML network. The default is NO.
--
-- When set to YES, the outputPixelFormat is ignored. Setting this for revision 1 is a no-op as it is not ML-based.
--
-- ObjC selector: @- setKeepNetworkOutput:@
setKeepNetworkOutput :: IsVNGenerateOpticalFlowRequest vnGenerateOpticalFlowRequest => vnGenerateOpticalFlowRequest -> Bool -> IO ()
setKeepNetworkOutput vnGenerateOpticalFlowRequest  value =
  sendMsg vnGenerateOpticalFlowRequest (mkSelector "setKeepNetworkOutput:") retVoid [argCULong (if value then 1 else 0)]

-- | VNPixelBufferObservation results.
--
-- ObjC selector: @- results@
results :: IsVNGenerateOpticalFlowRequest vnGenerateOpticalFlowRequest => vnGenerateOpticalFlowRequest -> IO (Id NSArray)
results vnGenerateOpticalFlowRequest  =
  sendMsg vnGenerateOpticalFlowRequest (mkSelector "results") (retPtr retVoid) [] >>= retainedObject . castPtr

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @computationAccuracy@
computationAccuracySelector :: Selector
computationAccuracySelector = mkSelector "computationAccuracy"

-- | @Selector@ for @setComputationAccuracy:@
setComputationAccuracySelector :: Selector
setComputationAccuracySelector = mkSelector "setComputationAccuracy:"

-- | @Selector@ for @outputPixelFormat@
outputPixelFormatSelector :: Selector
outputPixelFormatSelector = mkSelector "outputPixelFormat"

-- | @Selector@ for @setOutputPixelFormat:@
setOutputPixelFormatSelector :: Selector
setOutputPixelFormatSelector = mkSelector "setOutputPixelFormat:"

-- | @Selector@ for @keepNetworkOutput@
keepNetworkOutputSelector :: Selector
keepNetworkOutputSelector = mkSelector "keepNetworkOutput"

-- | @Selector@ for @setKeepNetworkOutput:@
setKeepNetworkOutputSelector :: Selector
setKeepNetworkOutputSelector = mkSelector "setKeepNetworkOutput:"

-- | @Selector@ for @results@
resultsSelector :: Selector
resultsSelector = mkSelector "results"

