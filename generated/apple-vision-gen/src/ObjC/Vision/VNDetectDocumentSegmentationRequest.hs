{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Performs document detection and segmentation in an image.
--
-- Generated bindings for @VNDetectDocumentSegmentationRequest@.
module ObjC.Vision.VNDetectDocumentSegmentationRequest
  ( VNDetectDocumentSegmentationRequest
  , IsVNDetectDocumentSegmentationRequest(..)
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

-- | VNRectangleObservation results.
--
-- ObjC selector: @- results@
results :: IsVNDetectDocumentSegmentationRequest vnDetectDocumentSegmentationRequest => vnDetectDocumentSegmentationRequest -> IO (Id NSArray)
results vnDetectDocumentSegmentationRequest =
  sendMessage vnDetectDocumentSegmentationRequest resultsSelector

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @results@
resultsSelector :: Selector '[] (Id NSArray)
resultsSelector = mkSelector "results"

