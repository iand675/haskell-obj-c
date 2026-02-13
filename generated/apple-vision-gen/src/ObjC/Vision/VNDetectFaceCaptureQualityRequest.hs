{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | A request that will produce or update a VNFaceObservation's property faceCaptureQuality with a float value. faceCaptureQuality is a float (wrapped by a NSNumber) that represents the capture quality of a given face in a photo. The float will be a value between 0 and 1, with 1 being the highest face capture quality and 0 being the lowest. If the request fails or the face observation has never been processed, the property faceCaptureQuality will be nil.
--
-- This request will generate VNFaceObservation objects with the face quality variable populated with information .
--
-- Generated bindings for @VNDetectFaceCaptureQualityRequest@.
module ObjC.Vision.VNDetectFaceCaptureQualityRequest
  ( VNDetectFaceCaptureQualityRequest
  , IsVNDetectFaceCaptureQualityRequest(..)
  , results
  , resultsSelector


  ) where

import Foreign.Ptr (Ptr, FunPtr)
import Foreign.C.Types

import ObjC.Runtime.Types
import ObjC.Runtime.Message (sendMessage, sendOwnedMessage, sendClassMessage, sendOwnedClassMessage)
import ObjC.Runtime.Selector (mkSelector)
import ObjC.Runtime.Class (getRequiredClass)

import ObjC.Vision.Internal.Classes
import ObjC.Foundation.Internal.Classes

-- | VNFaceObservation with populated faceCaptureQuality property results.
--
-- ObjC selector: @- results@
results :: IsVNDetectFaceCaptureQualityRequest vnDetectFaceCaptureQualityRequest => vnDetectFaceCaptureQualityRequest -> IO (Id NSArray)
results vnDetectFaceCaptureQualityRequest =
  sendMessage vnDetectFaceCaptureQualityRequest resultsSelector

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @results@
resultsSelector :: Selector '[] (Id NSArray)
resultsSelector = mkSelector "results"

