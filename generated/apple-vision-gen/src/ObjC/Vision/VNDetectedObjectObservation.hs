{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | VNDetectedObjectObservation
--
-- VNObservation
--
-- VNDetectedObjectObservation is VNObservation in an image that has a location and/or dimension. Further attributes depend on the specific detected object.
--
-- All result objects (faces, scene objects, shapes etc) must extend from this class.
--
-- Generated bindings for @VNDetectedObjectObservation@.
module ObjC.Vision.VNDetectedObjectObservation
  ( VNDetectedObjectObservation
  , IsVNDetectedObjectObservation(..)
  , globalSegmentationMask
  , globalSegmentationMaskSelector


  ) where

import Foreign.Ptr (Ptr, FunPtr)
import Foreign.C.Types

import ObjC.Runtime.Types
import ObjC.Runtime.Message (sendMessage, sendOwnedMessage, sendClassMessage, sendOwnedClassMessage)
import ObjC.Runtime.Selector (mkSelector)
import ObjC.Runtime.Class (getRequiredClass)

import ObjC.Vision.Internal.Classes
import ObjC.Foundation.Internal.Classes

-- | The resulting CVPixelBuffer from requests that generate a segmentation mask for the entire image.
--
-- ObjC selector: @- globalSegmentationMask@
globalSegmentationMask :: IsVNDetectedObjectObservation vnDetectedObjectObservation => vnDetectedObjectObservation -> IO (Id VNPixelBufferObservation)
globalSegmentationMask vnDetectedObjectObservation =
  sendMessage vnDetectedObjectObservation globalSegmentationMaskSelector

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @globalSegmentationMask@
globalSegmentationMaskSelector :: Selector '[] (Id VNPixelBufferObservation)
globalSegmentationMaskSelector = mkSelector "globalSegmentationMask"

