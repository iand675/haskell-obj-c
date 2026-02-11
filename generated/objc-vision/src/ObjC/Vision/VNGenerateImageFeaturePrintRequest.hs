{-# LANGUAGE PatternSynonyms #-}
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
  , setImageCropAndScaleOptionSelector
  , resultsSelector

  -- * Enum types
  , VNImageCropAndScaleOption(VNImageCropAndScaleOption)
  , pattern VNImageCropAndScaleOptionCenterCrop
  , pattern VNImageCropAndScaleOptionScaleFit
  , pattern VNImageCropAndScaleOptionScaleFill
  , pattern VNImageCropAndScaleOptionScaleFitRotate90CCW
  , pattern VNImageCropAndScaleOptionScaleFillRotate90CCW

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

-- | Determine what type of croping and scaling action should be applied to the image before generating the feature print.
--
-- The default value for this property is @VNImageCropAndScaleOptionScaleFill@.
--
-- ObjC selector: @- imageCropAndScaleOption@
imageCropAndScaleOption :: IsVNGenerateImageFeaturePrintRequest vnGenerateImageFeaturePrintRequest => vnGenerateImageFeaturePrintRequest -> IO VNImageCropAndScaleOption
imageCropAndScaleOption vnGenerateImageFeaturePrintRequest  =
  fmap (coerce :: CULong -> VNImageCropAndScaleOption) $ sendMsg vnGenerateImageFeaturePrintRequest (mkSelector "imageCropAndScaleOption") retCULong []

-- | Determine what type of croping and scaling action should be applied to the image before generating the feature print.
--
-- The default value for this property is @VNImageCropAndScaleOptionScaleFill@.
--
-- ObjC selector: @- setImageCropAndScaleOption:@
setImageCropAndScaleOption :: IsVNGenerateImageFeaturePrintRequest vnGenerateImageFeaturePrintRequest => vnGenerateImageFeaturePrintRequest -> VNImageCropAndScaleOption -> IO ()
setImageCropAndScaleOption vnGenerateImageFeaturePrintRequest  value =
  sendMsg vnGenerateImageFeaturePrintRequest (mkSelector "setImageCropAndScaleOption:") retVoid [argCULong (coerce value)]

-- | @VNFeaturePrintObservation@ results.
--
-- ObjC selector: @- results@
results :: IsVNGenerateImageFeaturePrintRequest vnGenerateImageFeaturePrintRequest => vnGenerateImageFeaturePrintRequest -> IO (Id NSArray)
results vnGenerateImageFeaturePrintRequest  =
  sendMsg vnGenerateImageFeaturePrintRequest (mkSelector "results") (retPtr retVoid) [] >>= retainedObject . castPtr

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @imageCropAndScaleOption@
imageCropAndScaleOptionSelector :: Selector
imageCropAndScaleOptionSelector = mkSelector "imageCropAndScaleOption"

-- | @Selector@ for @setImageCropAndScaleOption:@
setImageCropAndScaleOptionSelector :: Selector
setImageCropAndScaleOptionSelector = mkSelector "setImageCropAndScaleOption:"

-- | @Selector@ for @results@
resultsSelector :: Selector
resultsSelector = mkSelector "results"

