{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | A request that will detect the contours for the edges in an image.
--
-- This request will produce a VNContoursObservation which describes the contours.
--
-- Generated bindings for @VNDetectContoursRequest@.
module ObjC.Vision.VNDetectContoursRequest
  ( VNDetectContoursRequest
  , IsVNDetectContoursRequest(..)
  , contrastAdjustment
  , setContrastAdjustment
  , contrastPivot
  , setContrastPivot
  , detectsDarkOnLight
  , setDetectsDarkOnLight
  , detectDarkOnLight
  , setDetectDarkOnLight
  , maximumImageDimension
  , setMaximumImageDimension
  , results
  , contrastAdjustmentSelector
  , contrastPivotSelector
  , detectDarkOnLightSelector
  , detectsDarkOnLightSelector
  , maximumImageDimensionSelector
  , resultsSelector
  , setContrastAdjustmentSelector
  , setContrastPivotSelector
  , setDetectDarkOnLightSelector
  , setDetectsDarkOnLightSelector
  , setMaximumImageDimensionSelector


  ) where

import Foreign.Ptr (Ptr, FunPtr)
import Foreign.C.Types

import ObjC.Runtime.Types
import ObjC.Runtime.Message (sendMessage, sendOwnedMessage, sendClassMessage, sendOwnedClassMessage)
import ObjC.Runtime.Selector (mkSelector)
import ObjC.Runtime.Class (getRequiredClass)

import ObjC.Vision.Internal.Classes
import ObjC.Foundation.Internal.Classes

-- | The amount to adjust the image's contrast by.        A value of +1.0 means that the contrast is not adjusted. The default value is +2.0.
--
-- Contour detection works best with high contrast images. The default value of 2 doubles the image's contrast to aid in detection. If the image already has a high contrast then this value should be set to 1.
--
-- ObjC selector: @- contrastAdjustment@
contrastAdjustment :: IsVNDetectContoursRequest vnDetectContoursRequest => vnDetectContoursRequest -> IO CFloat
contrastAdjustment vnDetectContoursRequest =
  sendMessage vnDetectContoursRequest contrastAdjustmentSelector

-- | The amount to adjust the image's contrast by.        A value of +1.0 means that the contrast is not adjusted. The default value is +2.0.
--
-- Contour detection works best with high contrast images. The default value of 2 doubles the image's contrast to aid in detection. If the image already has a high contrast then this value should be set to 1.
--
-- ObjC selector: @- setContrastAdjustment:@
setContrastAdjustment :: IsVNDetectContoursRequest vnDetectContoursRequest => vnDetectContoursRequest -> CFloat -> IO ()
setContrastAdjustment vnDetectContoursRequest value =
  sendMessage vnDetectContoursRequest setContrastAdjustmentSelector value

-- | The pixel value to use as a pivot for the contrast. Valid values are from [0.0 ... +1.0], or nil to auto-detect based on image intensity.        The default value is +0.5 (i.e. pixel center).
--
-- ObjC selector: @- contrastPivot@
contrastPivot :: IsVNDetectContoursRequest vnDetectContoursRequest => vnDetectContoursRequest -> IO (Id NSNumber)
contrastPivot vnDetectContoursRequest =
  sendMessage vnDetectContoursRequest contrastPivotSelector

-- | The pixel value to use as a pivot for the contrast. Valid values are from [0.0 ... +1.0], or nil to auto-detect based on image intensity.        The default value is +0.5 (i.e. pixel center).
--
-- ObjC selector: @- setContrastPivot:@
setContrastPivot :: (IsVNDetectContoursRequest vnDetectContoursRequest, IsNSNumber value) => vnDetectContoursRequest -> value -> IO ()
setContrastPivot vnDetectContoursRequest value =
  sendMessage vnDetectContoursRequest setContrastPivotSelector (toNSNumber value)

-- | Identifies to the request if detecting a dark object on a light background, or vice versa, to aid in detection. The default value is YES.
--
-- ObjC selector: @- detectsDarkOnLight@
detectsDarkOnLight :: IsVNDetectContoursRequest vnDetectContoursRequest => vnDetectContoursRequest -> IO Bool
detectsDarkOnLight vnDetectContoursRequest =
  sendMessage vnDetectContoursRequest detectsDarkOnLightSelector

-- | Identifies to the request if detecting a dark object on a light background, or vice versa, to aid in detection. The default value is YES.
--
-- ObjC selector: @- setDetectsDarkOnLight:@
setDetectsDarkOnLight :: IsVNDetectContoursRequest vnDetectContoursRequest => vnDetectContoursRequest -> Bool -> IO ()
setDetectsDarkOnLight vnDetectContoursRequest value =
  sendMessage vnDetectContoursRequest setDetectsDarkOnLightSelector value

-- | @- detectDarkOnLight@
detectDarkOnLight :: IsVNDetectContoursRequest vnDetectContoursRequest => vnDetectContoursRequest -> IO Bool
detectDarkOnLight vnDetectContoursRequest =
  sendMessage vnDetectContoursRequest detectDarkOnLightSelector

-- | @- setDetectDarkOnLight:@
setDetectDarkOnLight :: IsVNDetectContoursRequest vnDetectContoursRequest => vnDetectContoursRequest -> Bool -> IO ()
setDetectDarkOnLight vnDetectContoursRequest value =
  sendMessage vnDetectContoursRequest setDetectDarkOnLightSelector value

-- | The limit on the maximum dimension of the image to be used for contour detection. Valid range of values is [64 ... NSUIntegerMax]. The default value is 512.
--
-- As the contour request is compute intensive, the input image is scaled down maintaining aspect ratio (if needed), such that its maximum dimension is the value of this property. The image never gets scaled up, so specifying the maximum value ensures that the image gets processed in its original size and not downscaled.
--
-- ObjC selector: @- maximumImageDimension@
maximumImageDimension :: IsVNDetectContoursRequest vnDetectContoursRequest => vnDetectContoursRequest -> IO CULong
maximumImageDimension vnDetectContoursRequest =
  sendMessage vnDetectContoursRequest maximumImageDimensionSelector

-- | The limit on the maximum dimension of the image to be used for contour detection. Valid range of values is [64 ... NSUIntegerMax]. The default value is 512.
--
-- As the contour request is compute intensive, the input image is scaled down maintaining aspect ratio (if needed), such that its maximum dimension is the value of this property. The image never gets scaled up, so specifying the maximum value ensures that the image gets processed in its original size and not downscaled.
--
-- ObjC selector: @- setMaximumImageDimension:@
setMaximumImageDimension :: IsVNDetectContoursRequest vnDetectContoursRequest => vnDetectContoursRequest -> CULong -> IO ()
setMaximumImageDimension vnDetectContoursRequest value =
  sendMessage vnDetectContoursRequest setMaximumImageDimensionSelector value

-- | VNContoursObservation results.
--
-- ObjC selector: @- results@
results :: IsVNDetectContoursRequest vnDetectContoursRequest => vnDetectContoursRequest -> IO (Id NSArray)
results vnDetectContoursRequest =
  sendMessage vnDetectContoursRequest resultsSelector

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @contrastAdjustment@
contrastAdjustmentSelector :: Selector '[] CFloat
contrastAdjustmentSelector = mkSelector "contrastAdjustment"

-- | @Selector@ for @setContrastAdjustment:@
setContrastAdjustmentSelector :: Selector '[CFloat] ()
setContrastAdjustmentSelector = mkSelector "setContrastAdjustment:"

-- | @Selector@ for @contrastPivot@
contrastPivotSelector :: Selector '[] (Id NSNumber)
contrastPivotSelector = mkSelector "contrastPivot"

-- | @Selector@ for @setContrastPivot:@
setContrastPivotSelector :: Selector '[Id NSNumber] ()
setContrastPivotSelector = mkSelector "setContrastPivot:"

-- | @Selector@ for @detectsDarkOnLight@
detectsDarkOnLightSelector :: Selector '[] Bool
detectsDarkOnLightSelector = mkSelector "detectsDarkOnLight"

-- | @Selector@ for @setDetectsDarkOnLight:@
setDetectsDarkOnLightSelector :: Selector '[Bool] ()
setDetectsDarkOnLightSelector = mkSelector "setDetectsDarkOnLight:"

-- | @Selector@ for @detectDarkOnLight@
detectDarkOnLightSelector :: Selector '[] Bool
detectDarkOnLightSelector = mkSelector "detectDarkOnLight"

-- | @Selector@ for @setDetectDarkOnLight:@
setDetectDarkOnLightSelector :: Selector '[Bool] ()
setDetectDarkOnLightSelector = mkSelector "setDetectDarkOnLight:"

-- | @Selector@ for @maximumImageDimension@
maximumImageDimensionSelector :: Selector '[] CULong
maximumImageDimensionSelector = mkSelector "maximumImageDimension"

-- | @Selector@ for @setMaximumImageDimension:@
setMaximumImageDimensionSelector :: Selector '[CULong] ()
setMaximumImageDimensionSelector = mkSelector "setMaximumImageDimension:"

-- | @Selector@ for @results@
resultsSelector :: Selector '[] (Id NSArray)
resultsSelector = mkSelector "results"

