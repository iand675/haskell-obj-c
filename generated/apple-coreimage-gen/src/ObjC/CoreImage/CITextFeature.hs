{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Information about a text that was detected in a still or video image.
--
-- > Note: In macOS 10.13, iOS 11, and tvOS 11 or later, the Vision framework replaces these classes  for identifying and analyzing image features.  See <doc://com.apple.documentation/documentation/vision/vnrecognizetextrequest>)
--
-- A detected text feature is not necessarily rectangular in the plane of the image; rather, the  feature identifies a shape that may be rectangular in space (for example a text on a sign) but which  appears as a four-sided polygon in the image. The properties of a @CITextFeature@ object  identify its four corners in image coordinates.
--
-- To detect text in an image or video, choose the ``CIDetectorTypeText`` type when initializing a  ``CIDetector`` object, and use the @CIDetectorImageOrientation@ option to specify the desired  orientation for finding upright text.
--
-- Generated bindings for @CITextFeature@.
module ObjC.CoreImage.CITextFeature
  ( CITextFeature
  , IsCITextFeature(..)
  , subFeatures
  , subFeaturesSelector


  ) where

import Foreign.Ptr (Ptr, FunPtr)
import Foreign.C.Types

import ObjC.Runtime.Types
import ObjC.Runtime.Message (sendMessage, sendOwnedMessage, sendClassMessage, sendOwnedClassMessage)
import ObjC.Runtime.Selector (mkSelector)
import ObjC.Runtime.Class (getRequiredClass)

import ObjC.CoreImage.Internal.Classes
import ObjC.Foundation.Internal.Classes

-- | An array containing additional features detected within the feature.
--
-- A text detector can identify both a major region that is likely to contain text as well as the areas within that region that likely to contain individual text features. Such  features might be single characters, groups of closely-packed characters, or entire words.
--
-- To detect sub-features, ``/CIDetector/featuresInImage:options:`` needs to be called with  the ``CIDetectorReturnSubFeatures`` option set to true.
--
-- ObjC selector: @- subFeatures@
subFeatures :: IsCITextFeature ciTextFeature => ciTextFeature -> IO (Id NSArray)
subFeatures ciTextFeature =
  sendMessage ciTextFeature subFeaturesSelector

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @subFeatures@
subFeaturesSelector :: Selector '[] (Id NSArray)
subFeaturesSelector = mkSelector "subFeatures"

