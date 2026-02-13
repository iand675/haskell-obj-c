{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE DataKinds #-}
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
  , keepNetworkOutputSelector
  , outputPixelFormatSelector
  , resultsSelector
  , setComputationAccuracySelector
  , setKeepNetworkOutputSelector
  , setOutputPixelFormatSelector

  -- * Enum types
  , VNGenerateOpticalFlowRequestComputationAccuracy(VNGenerateOpticalFlowRequestComputationAccuracy)
  , pattern VNGenerateOpticalFlowRequestComputationAccuracyLow
  , pattern VNGenerateOpticalFlowRequestComputationAccuracyMedium
  , pattern VNGenerateOpticalFlowRequestComputationAccuracyHigh
  , pattern VNGenerateOpticalFlowRequestComputationAccuracyVeryHigh

  ) where

import Foreign.Ptr (Ptr, FunPtr)
import Foreign.C.Types

import ObjC.Runtime.Types
import ObjC.Runtime.Message (sendMessage, sendOwnedMessage, sendClassMessage, sendOwnedClassMessage)
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
computationAccuracy vnGenerateOpticalFlowRequest =
  sendMessage vnGenerateOpticalFlowRequest computationAccuracySelector

-- | The level of accuracy used to compute the optical flow. Default is VNGenerateOpticalFlowRequestComputationAccuracyMedium.
--
-- The computational time typically trends with the accuracy level.  This parameter allows for selective tuning by the client application.
--
-- ObjC selector: @- setComputationAccuracy:@
setComputationAccuracy :: IsVNGenerateOpticalFlowRequest vnGenerateOpticalFlowRequest => vnGenerateOpticalFlowRequest -> VNGenerateOpticalFlowRequestComputationAccuracy -> IO ()
setComputationAccuracy vnGenerateOpticalFlowRequest value =
  sendMessage vnGenerateOpticalFlowRequest setComputationAccuracySelector value

-- | Pixel format type of the output buffer. Valid values are kCVPixelFormatType_TwoComponent32Float and kCVPixelFormatType_TwoComponent16Half.        Default is kCVPixelFormatType_TwoComponent32Float.
--
-- ObjC selector: @- outputPixelFormat@
outputPixelFormat :: IsVNGenerateOpticalFlowRequest vnGenerateOpticalFlowRequest => vnGenerateOpticalFlowRequest -> IO CUInt
outputPixelFormat vnGenerateOpticalFlowRequest =
  sendMessage vnGenerateOpticalFlowRequest outputPixelFormatSelector

-- | Pixel format type of the output buffer. Valid values are kCVPixelFormatType_TwoComponent32Float and kCVPixelFormatType_TwoComponent16Half.        Default is kCVPixelFormatType_TwoComponent32Float.
--
-- ObjC selector: @- setOutputPixelFormat:@
setOutputPixelFormat :: IsVNGenerateOpticalFlowRequest vnGenerateOpticalFlowRequest => vnGenerateOpticalFlowRequest -> CUInt -> IO ()
setOutputPixelFormat vnGenerateOpticalFlowRequest value =
  sendMessage vnGenerateOpticalFlowRequest setOutputPixelFormatSelector value

-- | Setting this to YES will keep the raw pixel buffer coming from the the ML network. The default is NO.
--
-- When set to YES, the outputPixelFormat is ignored. Setting this for revision 1 is a no-op as it is not ML-based.
--
-- ObjC selector: @- keepNetworkOutput@
keepNetworkOutput :: IsVNGenerateOpticalFlowRequest vnGenerateOpticalFlowRequest => vnGenerateOpticalFlowRequest -> IO Bool
keepNetworkOutput vnGenerateOpticalFlowRequest =
  sendMessage vnGenerateOpticalFlowRequest keepNetworkOutputSelector

-- | Setting this to YES will keep the raw pixel buffer coming from the the ML network. The default is NO.
--
-- When set to YES, the outputPixelFormat is ignored. Setting this for revision 1 is a no-op as it is not ML-based.
--
-- ObjC selector: @- setKeepNetworkOutput:@
setKeepNetworkOutput :: IsVNGenerateOpticalFlowRequest vnGenerateOpticalFlowRequest => vnGenerateOpticalFlowRequest -> Bool -> IO ()
setKeepNetworkOutput vnGenerateOpticalFlowRequest value =
  sendMessage vnGenerateOpticalFlowRequest setKeepNetworkOutputSelector value

-- | VNPixelBufferObservation results.
--
-- ObjC selector: @- results@
results :: IsVNGenerateOpticalFlowRequest vnGenerateOpticalFlowRequest => vnGenerateOpticalFlowRequest -> IO (Id NSArray)
results vnGenerateOpticalFlowRequest =
  sendMessage vnGenerateOpticalFlowRequest resultsSelector

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @computationAccuracy@
computationAccuracySelector :: Selector '[] VNGenerateOpticalFlowRequestComputationAccuracy
computationAccuracySelector = mkSelector "computationAccuracy"

-- | @Selector@ for @setComputationAccuracy:@
setComputationAccuracySelector :: Selector '[VNGenerateOpticalFlowRequestComputationAccuracy] ()
setComputationAccuracySelector = mkSelector "setComputationAccuracy:"

-- | @Selector@ for @outputPixelFormat@
outputPixelFormatSelector :: Selector '[] CUInt
outputPixelFormatSelector = mkSelector "outputPixelFormat"

-- | @Selector@ for @setOutputPixelFormat:@
setOutputPixelFormatSelector :: Selector '[CUInt] ()
setOutputPixelFormatSelector = mkSelector "setOutputPixelFormat:"

-- | @Selector@ for @keepNetworkOutput@
keepNetworkOutputSelector :: Selector '[] Bool
keepNetworkOutputSelector = mkSelector "keepNetworkOutput"

-- | @Selector@ for @setKeepNetworkOutput:@
setKeepNetworkOutputSelector :: Selector '[Bool] ()
setKeepNetworkOutputSelector = mkSelector "setKeepNetworkOutput:"

-- | @Selector@ for @results@
resultsSelector :: Selector '[] (Id NSArray)
resultsSelector = mkSelector "results"

