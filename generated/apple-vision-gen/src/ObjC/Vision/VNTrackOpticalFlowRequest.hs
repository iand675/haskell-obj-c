{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | @VNTrackOpticalFlowRequest@ will determine directional change vectors for each pixel from a previous to current image, reporting this result with a single @VNPixelBufferObservation@.
--
-- Because this request works at the pixel level, both images must have the same dimensions in order for the request to be successfully performed.				Setting a region of interest will isolate where the change determination is performed; however, the resultant observation will still be reported				with a full resolution `VNPixelBufferObservation.
--
-- Being a stateful request, at least two images must me processed in order to produce an observation.
--
-- Optical flow requests are very resource intensive, so it is recommended that only one request at a time be created and that the handler                where the request was issued be released immediately after generating optical flows.
--
-- Example usage:
--
-- - (nullable VNPixelBufferObservation*) opticalFlowFromImage:(CVPixelBufferRef)fromImage toImage:(CVPixelBuffer)toImage error:(NSError**)error					{						VNTrackOpticalFlowRequest* request = [[VNTrackOpticalFlowRequest alloc] init];
--
-- VNImageRequestHandler* imageRequestHandler = [[VNImageRequestHandler alloc] initWithCVPixelBuffer:fromImage options:\@{}];						if (![imageRequestHandler performRequests:\@[ request ] error:error])						{							return nil;						}
--
-- imageRequestHandler = [[VNImageRequestHandler alloc] initWithCVPixelBuffer:toImage options:\@{}];						if (![imageRequestHandler performRequests:\@[ request ] error:error])						{							return nil;						}
--
-- return [[request results] firstObject];					}
--
-- Generated bindings for @VNTrackOpticalFlowRequest@.
module ObjC.Vision.VNTrackOpticalFlowRequest
  ( VNTrackOpticalFlowRequest
  , IsVNTrackOpticalFlowRequest(..)
  , init_
  , initWithCompletionHandler
  , computationAccuracy
  , setComputationAccuracy
  , outputPixelFormat
  , setOutputPixelFormat
  , keepNetworkOutput
  , setKeepNetworkOutput
  , results
  , computationAccuracySelector
  , initSelector
  , initWithCompletionHandlerSelector
  , keepNetworkOutputSelector
  , outputPixelFormatSelector
  , resultsSelector
  , setComputationAccuracySelector
  , setKeepNetworkOutputSelector
  , setOutputPixelFormatSelector

  -- * Enum types
  , VNTrackOpticalFlowRequestComputationAccuracy(VNTrackOpticalFlowRequestComputationAccuracy)
  , pattern VNTrackOpticalFlowRequestComputationAccuracyLow
  , pattern VNTrackOpticalFlowRequestComputationAccuracyMedium
  , pattern VNTrackOpticalFlowRequestComputationAccuracyHigh
  , pattern VNTrackOpticalFlowRequestComputationAccuracyVeryHigh

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

-- | Create a new request that can statefully track the optical from from one image to another.
--
-- This is a convenience initializer for a frame analysis spacing of kCMTimeZero and a nil completion handler.
--
-- ObjC selector: @- init@
init_ :: IsVNTrackOpticalFlowRequest vnTrackOpticalFlowRequest => vnTrackOpticalFlowRequest -> IO (Id VNTrackOpticalFlowRequest)
init_ vnTrackOpticalFlowRequest =
  sendOwnedMessage vnTrackOpticalFlowRequest initSelector

-- | Create a new request that can statefully track the optical from from one image to another.
--
-- This is a convenience initializer for a frame analysis spacing of kCMTimeZero.
--
-- ObjC selector: @- initWithCompletionHandler:@
initWithCompletionHandler :: IsVNTrackOpticalFlowRequest vnTrackOpticalFlowRequest => vnTrackOpticalFlowRequest -> Ptr () -> IO (Id VNTrackOpticalFlowRequest)
initWithCompletionHandler vnTrackOpticalFlowRequest completionHandler =
  sendOwnedMessage vnTrackOpticalFlowRequest initWithCompletionHandlerSelector completionHandler

-- | The level of accuracy used to compute the optical flow. Default is VNTrackOpticalFlowRequestComputationAccuracyMedium.
--
-- The computational time typically trends with the accuracy level.  This parameter allows for selective tuning by the client application.
--
-- ObjC selector: @- computationAccuracy@
computationAccuracy :: IsVNTrackOpticalFlowRequest vnTrackOpticalFlowRequest => vnTrackOpticalFlowRequest -> IO VNTrackOpticalFlowRequestComputationAccuracy
computationAccuracy vnTrackOpticalFlowRequest =
  sendMessage vnTrackOpticalFlowRequest computationAccuracySelector

-- | The level of accuracy used to compute the optical flow. Default is VNTrackOpticalFlowRequestComputationAccuracyMedium.
--
-- The computational time typically trends with the accuracy level.  This parameter allows for selective tuning by the client application.
--
-- ObjC selector: @- setComputationAccuracy:@
setComputationAccuracy :: IsVNTrackOpticalFlowRequest vnTrackOpticalFlowRequest => vnTrackOpticalFlowRequest -> VNTrackOpticalFlowRequestComputationAccuracy -> IO ()
setComputationAccuracy vnTrackOpticalFlowRequest value =
  sendMessage vnTrackOpticalFlowRequest setComputationAccuracySelector value

-- | Pixel format type of the output buffer. Valid values are @kCVPixelFormatType_TwoComponent32Float@ and @kCVPixelFormatType_TwoComponent16Half@.  Default is @kCVPixelFormatType_TwoComponent32Float@.
--
-- ObjC selector: @- outputPixelFormat@
outputPixelFormat :: IsVNTrackOpticalFlowRequest vnTrackOpticalFlowRequest => vnTrackOpticalFlowRequest -> IO CUInt
outputPixelFormat vnTrackOpticalFlowRequest =
  sendMessage vnTrackOpticalFlowRequest outputPixelFormatSelector

-- | Pixel format type of the output buffer. Valid values are @kCVPixelFormatType_TwoComponent32Float@ and @kCVPixelFormatType_TwoComponent16Half@.  Default is @kCVPixelFormatType_TwoComponent32Float@.
--
-- ObjC selector: @- setOutputPixelFormat:@
setOutputPixelFormat :: IsVNTrackOpticalFlowRequest vnTrackOpticalFlowRequest => vnTrackOpticalFlowRequest -> CUInt -> IO ()
setOutputPixelFormat vnTrackOpticalFlowRequest value =
  sendMessage vnTrackOpticalFlowRequest setOutputPixelFormatSelector value

-- | Setting this to @YES@ will keep the raw pixel buffer coming from the the ML network. The default is @NO@.
--
-- When set to @YES@, the outputPixelFormat is ignored.
--
-- ObjC selector: @- keepNetworkOutput@
keepNetworkOutput :: IsVNTrackOpticalFlowRequest vnTrackOpticalFlowRequest => vnTrackOpticalFlowRequest -> IO Bool
keepNetworkOutput vnTrackOpticalFlowRequest =
  sendMessage vnTrackOpticalFlowRequest keepNetworkOutputSelector

-- | Setting this to @YES@ will keep the raw pixel buffer coming from the the ML network. The default is @NO@.
--
-- When set to @YES@, the outputPixelFormat is ignored.
--
-- ObjC selector: @- setKeepNetworkOutput:@
setKeepNetworkOutput :: IsVNTrackOpticalFlowRequest vnTrackOpticalFlowRequest => vnTrackOpticalFlowRequest -> Bool -> IO ()
setKeepNetworkOutput vnTrackOpticalFlowRequest value =
  sendMessage vnTrackOpticalFlowRequest setKeepNetworkOutputSelector value

-- | VNPixelBufferObservation results.
--
-- ObjC selector: @- results@
results :: IsVNTrackOpticalFlowRequest vnTrackOpticalFlowRequest => vnTrackOpticalFlowRequest -> IO (Id NSArray)
results vnTrackOpticalFlowRequest =
  sendMessage vnTrackOpticalFlowRequest resultsSelector

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @init@
initSelector :: Selector '[] (Id VNTrackOpticalFlowRequest)
initSelector = mkSelector "init"

-- | @Selector@ for @initWithCompletionHandler:@
initWithCompletionHandlerSelector :: Selector '[Ptr ()] (Id VNTrackOpticalFlowRequest)
initWithCompletionHandlerSelector = mkSelector "initWithCompletionHandler:"

-- | @Selector@ for @computationAccuracy@
computationAccuracySelector :: Selector '[] VNTrackOpticalFlowRequestComputationAccuracy
computationAccuracySelector = mkSelector "computationAccuracy"

-- | @Selector@ for @setComputationAccuracy:@
setComputationAccuracySelector :: Selector '[VNTrackOpticalFlowRequestComputationAccuracy] ()
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

