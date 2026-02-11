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

import Foreign.Ptr (Ptr, nullPtr, castPtr)
import Foreign.LibFFI
import Foreign.C.Types
import Data.Int (Int8, Int16)
import Data.Word (Word16)
import Data.Coerce (coerce)

import ObjC.Runtime.Types
import ObjC.Runtime.MsgSend (sendMsg, sendClassMsg, sendMsgStret, sendClassMsgStret)
import ObjC.Runtime.Selector (mkSelector)
import ObjC.Runtime.Class (getRequiredClass)

import ObjC.Vision.Internal.Classes
import ObjC.Foundation.Internal.Classes

-- | The resulting CVPixelBuffer from requests that generate a segmentation mask for the entire image.
--
-- ObjC selector: @- globalSegmentationMask@
globalSegmentationMask :: IsVNDetectedObjectObservation vnDetectedObjectObservation => vnDetectedObjectObservation -> IO (Id VNPixelBufferObservation)
globalSegmentationMask vnDetectedObjectObservation  =
  sendMsg vnDetectedObjectObservation (mkSelector "globalSegmentationMask") (retPtr retVoid) [] >>= retainedObject . castPtr

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @globalSegmentationMask@
globalSegmentationMaskSelector :: Selector
globalSegmentationMaskSelector = mkSelector "globalSegmentationMask"

