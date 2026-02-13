{-# LANGUAGE DataKinds #-}
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
  , maximumAspectRatioSelector
  , maximumObservationsSelector
  , minimumAspectRatioSelector
  , minimumConfidenceSelector
  , minimumSizeSelector
  , quadratureToleranceSelector
  , resultsSelector
  , setMaximumAspectRatioSelector
  , setMaximumObservationsSelector
  , setMinimumAspectRatioSelector
  , setMinimumConfidenceSelector
  , setMinimumSizeSelector
  , setQuadratureToleranceSelector


  ) where

import Foreign.Ptr (Ptr, FunPtr)
import Foreign.C.Types

import ObjC.Runtime.Types
import ObjC.Runtime.Message (sendMessage, sendOwnedMessage, sendClassMessage, sendOwnedClassMessage)
import ObjC.Runtime.Selector (mkSelector)
import ObjC.Runtime.Class (getRequiredClass)

import ObjC.Vision.Internal.Classes
import ObjC.Foundation.Internal.Classes

-- | Specifies the minimum aspect ratio of the rectangle(s) to look for, range [0.0, 1.0], default 0.5
--
-- ObjC selector: @- minimumAspectRatio@
minimumAspectRatio :: IsVNDetectRectanglesRequest vnDetectRectanglesRequest => vnDetectRectanglesRequest -> IO CFloat
minimumAspectRatio vnDetectRectanglesRequest =
  sendMessage vnDetectRectanglesRequest minimumAspectRatioSelector

-- | Specifies the minimum aspect ratio of the rectangle(s) to look for, range [0.0, 1.0], default 0.5
--
-- ObjC selector: @- setMinimumAspectRatio:@
setMinimumAspectRatio :: IsVNDetectRectanglesRequest vnDetectRectanglesRequest => vnDetectRectanglesRequest -> CFloat -> IO ()
setMinimumAspectRatio vnDetectRectanglesRequest value =
  sendMessage vnDetectRectanglesRequest setMinimumAspectRatioSelector value

-- | Specifies the maximum aspect ratio of the rectangle(s) to look for, range [0.0, 1.0], default 1.0
--
-- ObjC selector: @- maximumAspectRatio@
maximumAspectRatio :: IsVNDetectRectanglesRequest vnDetectRectanglesRequest => vnDetectRectanglesRequest -> IO CFloat
maximumAspectRatio vnDetectRectanglesRequest =
  sendMessage vnDetectRectanglesRequest maximumAspectRatioSelector

-- | Specifies the maximum aspect ratio of the rectangle(s) to look for, range [0.0, 1.0], default 1.0
--
-- ObjC selector: @- setMaximumAspectRatio:@
setMaximumAspectRatio :: IsVNDetectRectanglesRequest vnDetectRectanglesRequest => vnDetectRectanglesRequest -> CFloat -> IO ()
setMaximumAspectRatio vnDetectRectanglesRequest value =
  sendMessage vnDetectRectanglesRequest setMaximumAspectRatioSelector value

-- | Specifies the maximum number of degrees a rectangle corner angle can deviate from 90 degrees, range [0,45], default 30
--
-- ObjC selector: @- quadratureTolerance@
quadratureTolerance :: IsVNDetectRectanglesRequest vnDetectRectanglesRequest => vnDetectRectanglesRequest -> IO CFloat
quadratureTolerance vnDetectRectanglesRequest =
  sendMessage vnDetectRectanglesRequest quadratureToleranceSelector

-- | Specifies the maximum number of degrees a rectangle corner angle can deviate from 90 degrees, range [0,45], default 30
--
-- ObjC selector: @- setQuadratureTolerance:@
setQuadratureTolerance :: IsVNDetectRectanglesRequest vnDetectRectanglesRequest => vnDetectRectanglesRequest -> CFloat -> IO ()
setQuadratureTolerance vnDetectRectanglesRequest value =
  sendMessage vnDetectRectanglesRequest setQuadratureToleranceSelector value

-- | Specifies the minimum size of the rectangle to be detected, as a proportion of the smallest dimension, range [0.0, 1.0], default .2. Any smaller rectangles that may have been detected will not be returned.
--
-- ObjC selector: @- minimumSize@
minimumSize :: IsVNDetectRectanglesRequest vnDetectRectanglesRequest => vnDetectRectanglesRequest -> IO CFloat
minimumSize vnDetectRectanglesRequest =
  sendMessage vnDetectRectanglesRequest minimumSizeSelector

-- | Specifies the minimum size of the rectangle to be detected, as a proportion of the smallest dimension, range [0.0, 1.0], default .2. Any smaller rectangles that may have been detected will not be returned.
--
-- ObjC selector: @- setMinimumSize:@
setMinimumSize :: IsVNDetectRectanglesRequest vnDetectRectanglesRequest => vnDetectRectanglesRequest -> CFloat -> IO ()
setMinimumSize vnDetectRectanglesRequest value =
  sendMessage vnDetectRectanglesRequest setMinimumSizeSelector value

-- | Specifies a minimum confidence score, range [0.0, 1.0], default 0.0. Any rectangles with a lower confidence score will not be returned.
--
-- ObjC selector: @- minimumConfidence@
minimumConfidence :: IsVNDetectRectanglesRequest vnDetectRectanglesRequest => vnDetectRectanglesRequest -> IO CFloat
minimumConfidence vnDetectRectanglesRequest =
  sendMessage vnDetectRectanglesRequest minimumConfidenceSelector

-- | Specifies a minimum confidence score, range [0.0, 1.0], default 0.0. Any rectangles with a lower confidence score will not be returned.
--
-- ObjC selector: @- setMinimumConfidence:@
setMinimumConfidence :: IsVNDetectRectanglesRequest vnDetectRectanglesRequest => vnDetectRectanglesRequest -> CFloat -> IO ()
setMinimumConfidence vnDetectRectanglesRequest value =
  sendMessage vnDetectRectanglesRequest setMinimumConfidenceSelector value

-- | Specifies the maximum number of rectangles to be returned.  The default is 1.  Setting this property to 0 will allow an unlimited number of observations to be returned.
--
-- ObjC selector: @- maximumObservations@
maximumObservations :: IsVNDetectRectanglesRequest vnDetectRectanglesRequest => vnDetectRectanglesRequest -> IO CULong
maximumObservations vnDetectRectanglesRequest =
  sendMessage vnDetectRectanglesRequest maximumObservationsSelector

-- | Specifies the maximum number of rectangles to be returned.  The default is 1.  Setting this property to 0 will allow an unlimited number of observations to be returned.
--
-- ObjC selector: @- setMaximumObservations:@
setMaximumObservations :: IsVNDetectRectanglesRequest vnDetectRectanglesRequest => vnDetectRectanglesRequest -> CULong -> IO ()
setMaximumObservations vnDetectRectanglesRequest value =
  sendMessage vnDetectRectanglesRequest setMaximumObservationsSelector value

-- | VNRectangleObservation results.
--
-- ObjC selector: @- results@
results :: IsVNDetectRectanglesRequest vnDetectRectanglesRequest => vnDetectRectanglesRequest -> IO (Id NSArray)
results vnDetectRectanglesRequest =
  sendMessage vnDetectRectanglesRequest resultsSelector

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @minimumAspectRatio@
minimumAspectRatioSelector :: Selector '[] CFloat
minimumAspectRatioSelector = mkSelector "minimumAspectRatio"

-- | @Selector@ for @setMinimumAspectRatio:@
setMinimumAspectRatioSelector :: Selector '[CFloat] ()
setMinimumAspectRatioSelector = mkSelector "setMinimumAspectRatio:"

-- | @Selector@ for @maximumAspectRatio@
maximumAspectRatioSelector :: Selector '[] CFloat
maximumAspectRatioSelector = mkSelector "maximumAspectRatio"

-- | @Selector@ for @setMaximumAspectRatio:@
setMaximumAspectRatioSelector :: Selector '[CFloat] ()
setMaximumAspectRatioSelector = mkSelector "setMaximumAspectRatio:"

-- | @Selector@ for @quadratureTolerance@
quadratureToleranceSelector :: Selector '[] CFloat
quadratureToleranceSelector = mkSelector "quadratureTolerance"

-- | @Selector@ for @setQuadratureTolerance:@
setQuadratureToleranceSelector :: Selector '[CFloat] ()
setQuadratureToleranceSelector = mkSelector "setQuadratureTolerance:"

-- | @Selector@ for @minimumSize@
minimumSizeSelector :: Selector '[] CFloat
minimumSizeSelector = mkSelector "minimumSize"

-- | @Selector@ for @setMinimumSize:@
setMinimumSizeSelector :: Selector '[CFloat] ()
setMinimumSizeSelector = mkSelector "setMinimumSize:"

-- | @Selector@ for @minimumConfidence@
minimumConfidenceSelector :: Selector '[] CFloat
minimumConfidenceSelector = mkSelector "minimumConfidence"

-- | @Selector@ for @setMinimumConfidence:@
setMinimumConfidenceSelector :: Selector '[CFloat] ()
setMinimumConfidenceSelector = mkSelector "setMinimumConfidence:"

-- | @Selector@ for @maximumObservations@
maximumObservationsSelector :: Selector '[] CULong
maximumObservationsSelector = mkSelector "maximumObservations"

-- | @Selector@ for @setMaximumObservations:@
setMaximumObservationsSelector :: Selector '[CULong] ()
setMaximumObservationsSelector = mkSelector "setMaximumObservations:"

-- | @Selector@ for @results@
resultsSelector :: Selector '[] (Id NSArray)
resultsSelector = mkSelector "results"

