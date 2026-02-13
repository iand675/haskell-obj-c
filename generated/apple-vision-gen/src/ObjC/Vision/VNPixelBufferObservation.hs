{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | VNPixelBufferObservation
--
-- VNObservation
--
-- VNPixelBufferObservation returns the prediction of a model as a CVPixelBufferRef.
--
-- This is the returned observations for models that are not classifiers and return an image as a prediction. The confidence for these observations is always 1.0.
--
-- Generated bindings for @VNPixelBufferObservation@.
module ObjC.Vision.VNPixelBufferObservation
  ( VNPixelBufferObservation
  , IsVNPixelBufferObservation(..)
  , pixelBuffer
  , featureName
  , featureNameSelector
  , pixelBufferSelector


  ) where

import Foreign.Ptr (Ptr, FunPtr)
import Foreign.C.Types

import ObjC.Runtime.Types
import ObjC.Runtime.Message (sendMessage, sendOwnedMessage, sendClassMessage, sendOwnedClassMessage)
import ObjC.Runtime.Selector (mkSelector)
import ObjC.Runtime.Class (getRequiredClass)

import ObjC.Vision.Internal.Classes
import ObjC.Foundation.Internal.Classes

-- | The resulting image from a request like VNCoreMLRequest where the model produces an image as an output.
--
-- ObjC selector: @- pixelBuffer@
pixelBuffer :: IsVNPixelBufferObservation vnPixelBufferObservation => vnPixelBufferObservation -> IO (Ptr ())
pixelBuffer vnPixelBufferObservation =
  sendMessage vnPixelBufferObservation pixelBufferSelector

-- | The name used in the model description of the CoreML model that produced this observation allowing to correlate the observation back to the output of the model. This can be nil if the observation is not the result of a VNCoreMLRequest operation.
--
-- ObjC selector: @- featureName@
featureName :: IsVNPixelBufferObservation vnPixelBufferObservation => vnPixelBufferObservation -> IO (Id NSString)
featureName vnPixelBufferObservation =
  sendMessage vnPixelBufferObservation featureNameSelector

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @pixelBuffer@
pixelBufferSelector :: Selector '[] (Ptr ())
pixelBufferSelector = mkSelector "pixelBuffer"

-- | @Selector@ for @featureName@
featureNameSelector :: Selector '[] (Id NSString)
featureNameSelector = mkSelector "featureName"

