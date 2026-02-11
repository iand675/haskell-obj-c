{-# LANGUAGE PatternSynonyms #-}
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
  , initSelector
  , initWithCompletionHandlerSelector
  , computationAccuracySelector
  , setComputationAccuracySelector
  , outputPixelFormatSelector
  , setOutputPixelFormatSelector
  , keepNetworkOutputSelector
  , setKeepNetworkOutputSelector
  , resultsSelector

  -- * Enum types
  , VNTrackOpticalFlowRequestComputationAccuracy(VNTrackOpticalFlowRequestComputationAccuracy)
  , pattern VNTrackOpticalFlowRequestComputationAccuracyLow
  , pattern VNTrackOpticalFlowRequestComputationAccuracyMedium
  , pattern VNTrackOpticalFlowRequestComputationAccuracyHigh
  , pattern VNTrackOpticalFlowRequestComputationAccuracyVeryHigh

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

-- | Create a new request that can statefully track the optical from from one image to another.
--
-- This is a convenience initializer for a frame analysis spacing of kCMTimeZero and a nil completion handler.
--
-- ObjC selector: @- init@
init_ :: IsVNTrackOpticalFlowRequest vnTrackOpticalFlowRequest => vnTrackOpticalFlowRequest -> IO (Id VNTrackOpticalFlowRequest)
init_ vnTrackOpticalFlowRequest  =
  sendMsg vnTrackOpticalFlowRequest (mkSelector "init") (retPtr retVoid) [] >>= ownedObject . castPtr

-- | Create a new request that can statefully track the optical from from one image to another.
--
-- This is a convenience initializer for a frame analysis spacing of kCMTimeZero.
--
-- ObjC selector: @- initWithCompletionHandler:@
initWithCompletionHandler :: IsVNTrackOpticalFlowRequest vnTrackOpticalFlowRequest => vnTrackOpticalFlowRequest -> Ptr () -> IO (Id VNTrackOpticalFlowRequest)
initWithCompletionHandler vnTrackOpticalFlowRequest  completionHandler =
  sendMsg vnTrackOpticalFlowRequest (mkSelector "initWithCompletionHandler:") (retPtr retVoid) [argPtr (castPtr completionHandler :: Ptr ())] >>= ownedObject . castPtr

-- | The level of accuracy used to compute the optical flow. Default is VNTrackOpticalFlowRequestComputationAccuracyMedium.
--
-- The computational time typically trends with the accuracy level.  This parameter allows for selective tuning by the client application.
--
-- ObjC selector: @- computationAccuracy@
computationAccuracy :: IsVNTrackOpticalFlowRequest vnTrackOpticalFlowRequest => vnTrackOpticalFlowRequest -> IO VNTrackOpticalFlowRequestComputationAccuracy
computationAccuracy vnTrackOpticalFlowRequest  =
  fmap (coerce :: CULong -> VNTrackOpticalFlowRequestComputationAccuracy) $ sendMsg vnTrackOpticalFlowRequest (mkSelector "computationAccuracy") retCULong []

-- | The level of accuracy used to compute the optical flow. Default is VNTrackOpticalFlowRequestComputationAccuracyMedium.
--
-- The computational time typically trends with the accuracy level.  This parameter allows for selective tuning by the client application.
--
-- ObjC selector: @- setComputationAccuracy:@
setComputationAccuracy :: IsVNTrackOpticalFlowRequest vnTrackOpticalFlowRequest => vnTrackOpticalFlowRequest -> VNTrackOpticalFlowRequestComputationAccuracy -> IO ()
setComputationAccuracy vnTrackOpticalFlowRequest  value =
  sendMsg vnTrackOpticalFlowRequest (mkSelector "setComputationAccuracy:") retVoid [argCULong (coerce value)]

-- | Pixel format type of the output buffer. Valid values are @kCVPixelFormatType_TwoComponent32Float@ and @kCVPixelFormatType_TwoComponent16Half@.  Default is @kCVPixelFormatType_TwoComponent32Float@.
--
-- ObjC selector: @- outputPixelFormat@
outputPixelFormat :: IsVNTrackOpticalFlowRequest vnTrackOpticalFlowRequest => vnTrackOpticalFlowRequest -> IO CUInt
outputPixelFormat vnTrackOpticalFlowRequest  =
  sendMsg vnTrackOpticalFlowRequest (mkSelector "outputPixelFormat") retCUInt []

-- | Pixel format type of the output buffer. Valid values are @kCVPixelFormatType_TwoComponent32Float@ and @kCVPixelFormatType_TwoComponent16Half@.  Default is @kCVPixelFormatType_TwoComponent32Float@.
--
-- ObjC selector: @- setOutputPixelFormat:@
setOutputPixelFormat :: IsVNTrackOpticalFlowRequest vnTrackOpticalFlowRequest => vnTrackOpticalFlowRequest -> CUInt -> IO ()
setOutputPixelFormat vnTrackOpticalFlowRequest  value =
  sendMsg vnTrackOpticalFlowRequest (mkSelector "setOutputPixelFormat:") retVoid [argCUInt (fromIntegral value)]

-- | Setting this to @YES@ will keep the raw pixel buffer coming from the the ML network. The default is @NO@.
--
-- When set to @YES@, the outputPixelFormat is ignored.
--
-- ObjC selector: @- keepNetworkOutput@
keepNetworkOutput :: IsVNTrackOpticalFlowRequest vnTrackOpticalFlowRequest => vnTrackOpticalFlowRequest -> IO Bool
keepNetworkOutput vnTrackOpticalFlowRequest  =
  fmap ((/= 0) :: CULong -> Bool) $ sendMsg vnTrackOpticalFlowRequest (mkSelector "keepNetworkOutput") retCULong []

-- | Setting this to @YES@ will keep the raw pixel buffer coming from the the ML network. The default is @NO@.
--
-- When set to @YES@, the outputPixelFormat is ignored.
--
-- ObjC selector: @- setKeepNetworkOutput:@
setKeepNetworkOutput :: IsVNTrackOpticalFlowRequest vnTrackOpticalFlowRequest => vnTrackOpticalFlowRequest -> Bool -> IO ()
setKeepNetworkOutput vnTrackOpticalFlowRequest  value =
  sendMsg vnTrackOpticalFlowRequest (mkSelector "setKeepNetworkOutput:") retVoid [argCULong (if value then 1 else 0)]

-- | VNPixelBufferObservation results.
--
-- ObjC selector: @- results@
results :: IsVNTrackOpticalFlowRequest vnTrackOpticalFlowRequest => vnTrackOpticalFlowRequest -> IO (Id NSArray)
results vnTrackOpticalFlowRequest  =
  sendMsg vnTrackOpticalFlowRequest (mkSelector "results") (retPtr retVoid) [] >>= retainedObject . castPtr

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @init@
initSelector :: Selector
initSelector = mkSelector "init"

-- | @Selector@ for @initWithCompletionHandler:@
initWithCompletionHandlerSelector :: Selector
initWithCompletionHandlerSelector = mkSelector "initWithCompletionHandler:"

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

