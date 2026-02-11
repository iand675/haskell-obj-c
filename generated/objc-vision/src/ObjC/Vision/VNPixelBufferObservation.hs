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
  , pixelBufferSelector
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
import ObjC.Foundation.Internal.Classes

-- | The resulting image from a request like VNCoreMLRequest where the model produces an image as an output.
--
-- ObjC selector: @- pixelBuffer@
pixelBuffer :: IsVNPixelBufferObservation vnPixelBufferObservation => vnPixelBufferObservation -> IO (Ptr ())
pixelBuffer vnPixelBufferObservation  =
  fmap castPtr $ sendMsg vnPixelBufferObservation (mkSelector "pixelBuffer") (retPtr retVoid) []

-- | The name used in the model description of the CoreML model that produced this observation allowing to correlate the observation back to the output of the model. This can be nil if the observation is not the result of a VNCoreMLRequest operation.
--
-- ObjC selector: @- featureName@
featureName :: IsVNPixelBufferObservation vnPixelBufferObservation => vnPixelBufferObservation -> IO (Id NSString)
featureName vnPixelBufferObservation  =
  sendMsg vnPixelBufferObservation (mkSelector "featureName") (retPtr retVoid) [] >>= retainedObject . castPtr

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @pixelBuffer@
pixelBufferSelector :: Selector
pixelBufferSelector = mkSelector "pixelBuffer"

-- | @Selector@ for @featureName@
featureNameSelector :: Selector
featureNameSelector = mkSelector "featureName"

