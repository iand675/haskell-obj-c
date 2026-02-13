{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | A request for generating a feature print of an image.
--
-- This request will produce a @VNFeaturePrintObservation@ object.
--
-- Generated bindings for @VNGenerateImageFeaturePrintRequest@.
module ObjC.Vision.VNGenerateImageFeaturePrintRequest
  ( VNGenerateImageFeaturePrintRequest
  , IsVNGenerateImageFeaturePrintRequest(..)
  , imageCropAndScaleOption
  , setImageCropAndScaleOption
  , results
  , imageCropAndScaleOptionSelector
  , resultsSelector
  , setImageCropAndScaleOptionSelector

  -- * Enum types
  , VNImageCropAndScaleOption(VNImageCropAndScaleOption)
  , pattern VNImageCropAndScaleOptionCenterCrop
  , pattern VNImageCropAndScaleOptionScaleFit
  , pattern VNImageCropAndScaleOptionScaleFill
  , pattern VNImageCropAndScaleOptionScaleFitRotate90CCW
  , pattern VNImageCropAndScaleOptionScaleFillRotate90CCW

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

-- | Determine what type of croping and scaling action should be applied to the image before generating the feature print.
--
-- The default value for this property is @VNImageCropAndScaleOptionScaleFill@.
--
-- ObjC selector: @- imageCropAndScaleOption@
imageCropAndScaleOption :: IsVNGenerateImageFeaturePrintRequest vnGenerateImageFeaturePrintRequest => vnGenerateImageFeaturePrintRequest -> IO VNImageCropAndScaleOption
imageCropAndScaleOption vnGenerateImageFeaturePrintRequest =
  sendMessage vnGenerateImageFeaturePrintRequest imageCropAndScaleOptionSelector

-- | Determine what type of croping and scaling action should be applied to the image before generating the feature print.
--
-- The default value for this property is @VNImageCropAndScaleOptionScaleFill@.
--
-- ObjC selector: @- setImageCropAndScaleOption:@
setImageCropAndScaleOption :: IsVNGenerateImageFeaturePrintRequest vnGenerateImageFeaturePrintRequest => vnGenerateImageFeaturePrintRequest -> VNImageCropAndScaleOption -> IO ()
setImageCropAndScaleOption vnGenerateImageFeaturePrintRequest value =
  sendMessage vnGenerateImageFeaturePrintRequest setImageCropAndScaleOptionSelector value

-- | @VNFeaturePrintObservation@ results.
--
-- ObjC selector: @- results@
results :: IsVNGenerateImageFeaturePrintRequest vnGenerateImageFeaturePrintRequest => vnGenerateImageFeaturePrintRequest -> IO (Id NSArray)
results vnGenerateImageFeaturePrintRequest =
  sendMessage vnGenerateImageFeaturePrintRequest resultsSelector

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @imageCropAndScaleOption@
imageCropAndScaleOptionSelector :: Selector '[] VNImageCropAndScaleOption
imageCropAndScaleOptionSelector = mkSelector "imageCropAndScaleOption"

-- | @Selector@ for @setImageCropAndScaleOption:@
setImageCropAndScaleOptionSelector :: Selector '[VNImageCropAndScaleOption] ()
setImageCropAndScaleOptionSelector = mkSelector "setImageCropAndScaleOption:"

-- | @Selector@ for @results@
resultsSelector :: Selector '[] (Id NSArray)
resultsSelector = mkSelector "results"

