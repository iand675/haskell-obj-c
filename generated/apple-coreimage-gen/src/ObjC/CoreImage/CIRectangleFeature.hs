{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Information about a rectangular region detected in a still or video image.
--
-- > Note: In macOS 10.13, iOS 11, and tvOS 11 or later, the Vision framework replaces these classes  for identifying and analyzing image features.  See <doc://com.apple.documentation/documentation/vision/vndetectfacerectanglesrequest>)
--
-- A detected rectangle feature is not necessarily rectangular in the plane of the image; rather, the  feature identifies a shape that may be rectangular in space (for example a book on a desk) but which  appears as a four-sided polygon in the image. The properties of a @CIRectangleFeature@ object  identify its four corners in image coordinates.
--
-- You can use rectangle feature detection together with the @CIPerspectiveCorrection@ filter  to transform the feature to a normal orientation.
--
-- To detect rectangles in an image or video, choose ``CIDetectorTypeRectangle`` when initializing a  ``CIDetector`` object, and use the @CIDetectorAspectRatio@ and @CIDetectorFocalLength@ options to  specify the approximate shape of rectangular features to search for. The detector returns at  most one rectangle feature, the most prominent found in the image.
--
-- Generated bindings for @CIRectangleFeature@.
module ObjC.CoreImage.CIRectangleFeature
  ( CIRectangleFeature
  , IsCIRectangleFeature(..)


  ) where

import Foreign.Ptr (Ptr, FunPtr)
import Foreign.C.Types

import ObjC.Runtime.Types
import ObjC.Runtime.Message (sendMessage, sendOwnedMessage, sendClassMessage, sendOwnedClassMessage)
import ObjC.Runtime.Selector (mkSelector)
import ObjC.Runtime.Class (getRequiredClass)

import ObjC.CoreImage.Internal.Classes
import ObjC.Foundation.Internal.Classes

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

