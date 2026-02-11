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

-- | VNRectangleObservation results.
--
-- ObjC selector: @- results@
results :: IsVNDetectDocumentSegmentationRequest vnDetectDocumentSegmentationRequest => vnDetectDocumentSegmentationRequest -> IO (Id NSArray)
results vnDetectDocumentSegmentationRequest  =
  sendMsg vnDetectDocumentSegmentationRequest (mkSelector "results") (retPtr retVoid) [] >>= retainedObject . castPtr

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @results@
resultsSelector :: Selector
resultsSelector = mkSelector "results"

