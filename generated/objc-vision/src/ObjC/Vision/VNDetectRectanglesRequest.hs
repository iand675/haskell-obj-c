{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | A request that will detect rectangles in an image.
--
-- This request will generate VNRectangleObservation objects describing the location of rectangles detected in an image.
--
-- Generated bindings for @VNDetectRectanglesRequest@.
module ObjC.Vision.VNDetectRectanglesRequest
  ( VNDetectRectanglesRequest
  , IsVNDetectRectanglesRequest(..)
  , minimumAspectRatio
  , setMinimumAspectRatio
  , maximumAspectRatio
  , setMaximumAspectRatio
  , quadratureTolerance
  , setQuadratureTolerance
  , minimumSize
  , setMinimumSize
  , minimumConfidence
  , setMinimumConfidence
  , maximumObservations
  , setMaximumObservations
  , results
  , minimumAspectRatioSelector
  , setMinimumAspectRatioSelector
  , maximumAspectRatioSelector
  , setMaximumAspectRatioSelector
  , quadratureToleranceSelector
  , setQuadratureToleranceSelector
  , minimumSizeSelector
  , setMinimumSizeSelector
  , minimumConfidenceSelector
  , setMinimumConfidenceSelector
  , maximumObservationsSelector
  , setMaximumObservationsSelector
  , resultsSelector


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
import ObjC.Foundation.Internal.Classes

-- | Specifies the minimum aspect ratio of the rectangle(s) to look for, range [0.0, 1.0], default 0.5
--
-- ObjC selector: @- minimumAspectRatio@
minimumAspectRatio :: IsVNDetectRectanglesRequest vnDetectRectanglesRequest => vnDetectRectanglesRequest -> IO CFloat
minimumAspectRatio vnDetectRectanglesRequest  =
  sendMsg vnDetectRectanglesRequest (mkSelector "minimumAspectRatio") retCFloat []

-- | Specifies the minimum aspect ratio of the rectangle(s) to look for, range [0.0, 1.0], default 0.5
--
-- ObjC selector: @- setMinimumAspectRatio:@
setMinimumAspectRatio :: IsVNDetectRectanglesRequest vnDetectRectanglesRequest => vnDetectRectanglesRequest -> CFloat -> IO ()
setMinimumAspectRatio vnDetectRectanglesRequest  value =
  sendMsg vnDetectRectanglesRequest (mkSelector "setMinimumAspectRatio:") retVoid [argCFloat (fromIntegral value)]

-- | Specifies the maximum aspect ratio of the rectangle(s) to look for, range [0.0, 1.0], default 1.0
--
-- ObjC selector: @- maximumAspectRatio@
maximumAspectRatio :: IsVNDetectRectanglesRequest vnDetectRectanglesRequest => vnDetectRectanglesRequest -> IO CFloat
maximumAspectRatio vnDetectRectanglesRequest  =
  sendMsg vnDetectRectanglesRequest (mkSelector "maximumAspectRatio") retCFloat []

-- | Specifies the maximum aspect ratio of the rectangle(s) to look for, range [0.0, 1.0], default 1.0
--
-- ObjC selector: @- setMaximumAspectRatio:@
setMaximumAspectRatio :: IsVNDetectRectanglesRequest vnDetectRectanglesRequest => vnDetectRectanglesRequest -> CFloat -> IO ()
setMaximumAspectRatio vnDetectRectanglesRequest  value =
  sendMsg vnDetectRectanglesRequest (mkSelector "setMaximumAspectRatio:") retVoid [argCFloat (fromIntegral value)]

-- | Specifies the maximum number of degrees a rectangle corner angle can deviate from 90 degrees, range [0,45], default 30
--
-- ObjC selector: @- quadratureTolerance@
quadratureTolerance :: IsVNDetectRectanglesRequest vnDetectRectanglesRequest => vnDetectRectanglesRequest -> IO CFloat
quadratureTolerance vnDetectRectanglesRequest  =
  sendMsg vnDetectRectanglesRequest (mkSelector "quadratureTolerance") retCFloat []

-- | Specifies the maximum number of degrees a rectangle corner angle can deviate from 90 degrees, range [0,45], default 30
--
-- ObjC selector: @- setQuadratureTolerance:@
setQuadratureTolerance :: IsVNDetectRectanglesRequest vnDetectRectanglesRequest => vnDetectRectanglesRequest -> CFloat -> IO ()
setQuadratureTolerance vnDetectRectanglesRequest  value =
  sendMsg vnDetectRectanglesRequest (mkSelector "setQuadratureTolerance:") retVoid [argCFloat (fromIntegral value)]

-- | Specifies the minimum size of the rectangle to be detected, as a proportion of the smallest dimension, range [0.0, 1.0], default .2. Any smaller rectangles that may have been detected will not be returned.
--
-- ObjC selector: @- minimumSize@
minimumSize :: IsVNDetectRectanglesRequest vnDetectRectanglesRequest => vnDetectRectanglesRequest -> IO CFloat
minimumSize vnDetectRectanglesRequest  =
  sendMsg vnDetectRectanglesRequest (mkSelector "minimumSize") retCFloat []

-- | Specifies the minimum size of the rectangle to be detected, as a proportion of the smallest dimension, range [0.0, 1.0], default .2. Any smaller rectangles that may have been detected will not be returned.
--
-- ObjC selector: @- setMinimumSize:@
setMinimumSize :: IsVNDetectRectanglesRequest vnDetectRectanglesRequest => vnDetectRectanglesRequest -> CFloat -> IO ()
setMinimumSize vnDetectRectanglesRequest  value =
  sendMsg vnDetectRectanglesRequest (mkSelector "setMinimumSize:") retVoid [argCFloat (fromIntegral value)]

-- | Specifies a minimum confidence score, range [0.0, 1.0], default 0.0. Any rectangles with a lower confidence score will not be returned.
--
-- ObjC selector: @- minimumConfidence@
minimumConfidence :: IsVNDetectRectanglesRequest vnDetectRectanglesRequest => vnDetectRectanglesRequest -> IO CFloat
minimumConfidence vnDetectRectanglesRequest  =
  sendMsg vnDetectRectanglesRequest (mkSelector "minimumConfidence") retCFloat []

-- | Specifies a minimum confidence score, range [0.0, 1.0], default 0.0. Any rectangles with a lower confidence score will not be returned.
--
-- ObjC selector: @- setMinimumConfidence:@
setMinimumConfidence :: IsVNDetectRectanglesRequest vnDetectRectanglesRequest => vnDetectRectanglesRequest -> CFloat -> IO ()
setMinimumConfidence vnDetectRectanglesRequest  value =
  sendMsg vnDetectRectanglesRequest (mkSelector "setMinimumConfidence:") retVoid [argCFloat (fromIntegral value)]

-- | Specifies the maximum number of rectangles to be returned.  The default is 1.  Setting this property to 0 will allow an unlimited number of observations to be returned.
--
-- ObjC selector: @- maximumObservations@
maximumObservations :: IsVNDetectRectanglesRequest vnDetectRectanglesRequest => vnDetectRectanglesRequest -> IO CULong
maximumObservations vnDetectRectanglesRequest  =
  sendMsg vnDetectRectanglesRequest (mkSelector "maximumObservations") retCULong []

-- | Specifies the maximum number of rectangles to be returned.  The default is 1.  Setting this property to 0 will allow an unlimited number of observations to be returned.
--
-- ObjC selector: @- setMaximumObservations:@
setMaximumObservations :: IsVNDetectRectanglesRequest vnDetectRectanglesRequest => vnDetectRectanglesRequest -> CULong -> IO ()
setMaximumObservations vnDetectRectanglesRequest  value =
  sendMsg vnDetectRectanglesRequest (mkSelector "setMaximumObservations:") retVoid [argCULong (fromIntegral value)]

-- | VNRectangleObservation results.
--
-- ObjC selector: @- results@
results :: IsVNDetectRectanglesRequest vnDetectRectanglesRequest => vnDetectRectanglesRequest -> IO (Id NSArray)
results vnDetectRectanglesRequest  =
  sendMsg vnDetectRectanglesRequest (mkSelector "results") (retPtr retVoid) [] >>= retainedObject . castPtr

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @minimumAspectRatio@
minimumAspectRatioSelector :: Selector
minimumAspectRatioSelector = mkSelector "minimumAspectRatio"

-- | @Selector@ for @setMinimumAspectRatio:@
setMinimumAspectRatioSelector :: Selector
setMinimumAspectRatioSelector = mkSelector "setMinimumAspectRatio:"

-- | @Selector@ for @maximumAspectRatio@
maximumAspectRatioSelector :: Selector
maximumAspectRatioSelector = mkSelector "maximumAspectRatio"

-- | @Selector@ for @setMaximumAspectRatio:@
setMaximumAspectRatioSelector :: Selector
setMaximumAspectRatioSelector = mkSelector "setMaximumAspectRatio:"

-- | @Selector@ for @quadratureTolerance@
quadratureToleranceSelector :: Selector
quadratureToleranceSelector = mkSelector "quadratureTolerance"

-- | @Selector@ for @setQuadratureTolerance:@
setQuadratureToleranceSelector :: Selector
setQuadratureToleranceSelector = mkSelector "setQuadratureTolerance:"

-- | @Selector@ for @minimumSize@
minimumSizeSelector :: Selector
minimumSizeSelector = mkSelector "minimumSize"

-- | @Selector@ for @setMinimumSize:@
setMinimumSizeSelector :: Selector
setMinimumSizeSelector = mkSelector "setMinimumSize:"

-- | @Selector@ for @minimumConfidence@
minimumConfidenceSelector :: Selector
minimumConfidenceSelector = mkSelector "minimumConfidence"

-- | @Selector@ for @setMinimumConfidence:@
setMinimumConfidenceSelector :: Selector
setMinimumConfidenceSelector = mkSelector "setMinimumConfidence:"

-- | @Selector@ for @maximumObservations@
maximumObservationsSelector :: Selector
maximumObservationsSelector = mkSelector "maximumObservations"

-- | @Selector@ for @setMaximumObservations:@
setMaximumObservationsSelector :: Selector
setMaximumObservationsSelector = mkSelector "setMaximumObservations:"

-- | @Selector@ for @results@
resultsSelector :: Selector
resultsSelector = mkSelector "results"

