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
  , setContrastAdjustmentSelector
  , contrastPivotSelector
  , setContrastPivotSelector
  , detectsDarkOnLightSelector
  , setDetectsDarkOnLightSelector
  , detectDarkOnLightSelector
  , setDetectDarkOnLightSelector
  , maximumImageDimensionSelector
  , setMaximumImageDimensionSelector
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

-- | The amount to adjust the image's contrast by.        A value of +1.0 means that the contrast is not adjusted. The default value is +2.0.
--
-- Contour detection works best with high contrast images. The default value of 2 doubles the image's contrast to aid in detection. If the image already has a high contrast then this value should be set to 1.
--
-- ObjC selector: @- contrastAdjustment@
contrastAdjustment :: IsVNDetectContoursRequest vnDetectContoursRequest => vnDetectContoursRequest -> IO CFloat
contrastAdjustment vnDetectContoursRequest  =
  sendMsg vnDetectContoursRequest (mkSelector "contrastAdjustment") retCFloat []

-- | The amount to adjust the image's contrast by.        A value of +1.0 means that the contrast is not adjusted. The default value is +2.0.
--
-- Contour detection works best with high contrast images. The default value of 2 doubles the image's contrast to aid in detection. If the image already has a high contrast then this value should be set to 1.
--
-- ObjC selector: @- setContrastAdjustment:@
setContrastAdjustment :: IsVNDetectContoursRequest vnDetectContoursRequest => vnDetectContoursRequest -> CFloat -> IO ()
setContrastAdjustment vnDetectContoursRequest  value =
  sendMsg vnDetectContoursRequest (mkSelector "setContrastAdjustment:") retVoid [argCFloat (fromIntegral value)]

-- | The pixel value to use as a pivot for the contrast. Valid values are from [0.0 ... +1.0], or nil to auto-detect based on image intensity.        The default value is +0.5 (i.e. pixel center).
--
-- ObjC selector: @- contrastPivot@
contrastPivot :: IsVNDetectContoursRequest vnDetectContoursRequest => vnDetectContoursRequest -> IO (Id NSNumber)
contrastPivot vnDetectContoursRequest  =
  sendMsg vnDetectContoursRequest (mkSelector "contrastPivot") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | The pixel value to use as a pivot for the contrast. Valid values are from [0.0 ... +1.0], or nil to auto-detect based on image intensity.        The default value is +0.5 (i.e. pixel center).
--
-- ObjC selector: @- setContrastPivot:@
setContrastPivot :: (IsVNDetectContoursRequest vnDetectContoursRequest, IsNSNumber value) => vnDetectContoursRequest -> value -> IO ()
setContrastPivot vnDetectContoursRequest  value =
withObjCPtr value $ \raw_value ->
    sendMsg vnDetectContoursRequest (mkSelector "setContrastPivot:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- | Identifies to the request if detecting a dark object on a light background, or vice versa, to aid in detection. The default value is YES.
--
-- ObjC selector: @- detectsDarkOnLight@
detectsDarkOnLight :: IsVNDetectContoursRequest vnDetectContoursRequest => vnDetectContoursRequest -> IO Bool
detectsDarkOnLight vnDetectContoursRequest  =
  fmap ((/= 0) :: CULong -> Bool) $ sendMsg vnDetectContoursRequest (mkSelector "detectsDarkOnLight") retCULong []

-- | Identifies to the request if detecting a dark object on a light background, or vice versa, to aid in detection. The default value is YES.
--
-- ObjC selector: @- setDetectsDarkOnLight:@
setDetectsDarkOnLight :: IsVNDetectContoursRequest vnDetectContoursRequest => vnDetectContoursRequest -> Bool -> IO ()
setDetectsDarkOnLight vnDetectContoursRequest  value =
  sendMsg vnDetectContoursRequest (mkSelector "setDetectsDarkOnLight:") retVoid [argCULong (if value then 1 else 0)]

-- | @- detectDarkOnLight@
detectDarkOnLight :: IsVNDetectContoursRequest vnDetectContoursRequest => vnDetectContoursRequest -> IO Bool
detectDarkOnLight vnDetectContoursRequest  =
  fmap ((/= 0) :: CULong -> Bool) $ sendMsg vnDetectContoursRequest (mkSelector "detectDarkOnLight") retCULong []

-- | @- setDetectDarkOnLight:@
setDetectDarkOnLight :: IsVNDetectContoursRequest vnDetectContoursRequest => vnDetectContoursRequest -> Bool -> IO ()
setDetectDarkOnLight vnDetectContoursRequest  value =
  sendMsg vnDetectContoursRequest (mkSelector "setDetectDarkOnLight:") retVoid [argCULong (if value then 1 else 0)]

-- | The limit on the maximum dimension of the image to be used for contour detection. Valid range of values is [64 ... NSUIntegerMax]. The default value is 512.
--
-- As the contour request is compute intensive, the input image is scaled down maintaining aspect ratio (if needed), such that its maximum dimension is the value of this property. The image never gets scaled up, so specifying the maximum value ensures that the image gets processed in its original size and not downscaled.
--
-- ObjC selector: @- maximumImageDimension@
maximumImageDimension :: IsVNDetectContoursRequest vnDetectContoursRequest => vnDetectContoursRequest -> IO CULong
maximumImageDimension vnDetectContoursRequest  =
  sendMsg vnDetectContoursRequest (mkSelector "maximumImageDimension") retCULong []

-- | The limit on the maximum dimension of the image to be used for contour detection. Valid range of values is [64 ... NSUIntegerMax]. The default value is 512.
--
-- As the contour request is compute intensive, the input image is scaled down maintaining aspect ratio (if needed), such that its maximum dimension is the value of this property. The image never gets scaled up, so specifying the maximum value ensures that the image gets processed in its original size and not downscaled.
--
-- ObjC selector: @- setMaximumImageDimension:@
setMaximumImageDimension :: IsVNDetectContoursRequest vnDetectContoursRequest => vnDetectContoursRequest -> CULong -> IO ()
setMaximumImageDimension vnDetectContoursRequest  value =
  sendMsg vnDetectContoursRequest (mkSelector "setMaximumImageDimension:") retVoid [argCULong (fromIntegral value)]

-- | VNContoursObservation results.
--
-- ObjC selector: @- results@
results :: IsVNDetectContoursRequest vnDetectContoursRequest => vnDetectContoursRequest -> IO (Id NSArray)
results vnDetectContoursRequest  =
  sendMsg vnDetectContoursRequest (mkSelector "results") (retPtr retVoid) [] >>= retainedObject . castPtr

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @contrastAdjustment@
contrastAdjustmentSelector :: Selector
contrastAdjustmentSelector = mkSelector "contrastAdjustment"

-- | @Selector@ for @setContrastAdjustment:@
setContrastAdjustmentSelector :: Selector
setContrastAdjustmentSelector = mkSelector "setContrastAdjustment:"

-- | @Selector@ for @contrastPivot@
contrastPivotSelector :: Selector
contrastPivotSelector = mkSelector "contrastPivot"

-- | @Selector@ for @setContrastPivot:@
setContrastPivotSelector :: Selector
setContrastPivotSelector = mkSelector "setContrastPivot:"

-- | @Selector@ for @detectsDarkOnLight@
detectsDarkOnLightSelector :: Selector
detectsDarkOnLightSelector = mkSelector "detectsDarkOnLight"

-- | @Selector@ for @setDetectsDarkOnLight:@
setDetectsDarkOnLightSelector :: Selector
setDetectsDarkOnLightSelector = mkSelector "setDetectsDarkOnLight:"

-- | @Selector@ for @detectDarkOnLight@
detectDarkOnLightSelector :: Selector
detectDarkOnLightSelector = mkSelector "detectDarkOnLight"

-- | @Selector@ for @setDetectDarkOnLight:@
setDetectDarkOnLightSelector :: Selector
setDetectDarkOnLightSelector = mkSelector "setDetectDarkOnLight:"

-- | @Selector@ for @maximumImageDimension@
maximumImageDimensionSelector :: Selector
maximumImageDimensionSelector = mkSelector "maximumImageDimension"

-- | @Selector@ for @setMaximumImageDimension:@
setMaximumImageDimensionSelector :: Selector
setMaximumImageDimensionSelector = mkSelector "setMaximumImageDimension:"

-- | @Selector@ for @results@
resultsSelector :: Selector
resultsSelector = mkSelector "results"

