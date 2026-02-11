{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | VNCoreMLFeatureValueObservation
--
-- VNObservation
--
-- VNCoreMLFeatureValueObservation returns the prediction of a model as an MLFeatureValue.
--
-- This is the returned observations for models that are not classifiers and that do not return an image as a prediction. The confidence for these observations is always 1.0.
--
-- Generated bindings for @VNCoreMLFeatureValueObservation@.
module ObjC.Vision.VNCoreMLFeatureValueObservation
  ( VNCoreMLFeatureValueObservation
  , IsVNCoreMLFeatureValueObservation(..)
  , featureValue
  , featureName
  , featureValueSelector
  , featureNameSelector


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
import ObjC.CoreML.Internal.Classes
import ObjC.Foundation.Internal.Classes

-- | The result VNCoreMLRequest where the model produces an MLFeatureValue that is neither a classification or image. Refer to the Core ML documentation and the model itself for the handling of the content of the featureValue.
--
-- ObjC selector: @- featureValue@
featureValue :: IsVNCoreMLFeatureValueObservation vnCoreMLFeatureValueObservation => vnCoreMLFeatureValueObservation -> IO (Id MLFeatureValue)
featureValue vnCoreMLFeatureValueObservation  =
  sendMsg vnCoreMLFeatureValueObservation (mkSelector "featureValue") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | The name used in the model description of the CoreML model that produced this observation allowing to correlate the observation back to the output of the model.
--
-- ObjC selector: @- featureName@
featureName :: IsVNCoreMLFeatureValueObservation vnCoreMLFeatureValueObservation => vnCoreMLFeatureValueObservation -> IO (Id NSString)
featureName vnCoreMLFeatureValueObservation  =
  sendMsg vnCoreMLFeatureValueObservation (mkSelector "featureName") (retPtr retVoid) [] >>= retainedObject . castPtr

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @featureValue@
featureValueSelector :: Selector
featureValueSelector = mkSelector "featureValue"

-- | @Selector@ for @featureName@
featureNameSelector :: Selector
featureNameSelector = mkSelector "featureName"

