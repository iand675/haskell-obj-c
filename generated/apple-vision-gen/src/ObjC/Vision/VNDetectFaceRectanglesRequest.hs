{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | A request that will detect faces in an image.
--
-- This request will generate VNFaceObservation objects with a defined boundingBox.
--
-- Generated bindings for @VNDetectFaceRectanglesRequest@.
module ObjC.Vision.VNDetectFaceRectanglesRequest
  ( VNDetectFaceRectanglesRequest
  , IsVNDetectFaceRectanglesRequest(..)
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

-- | VNFaceObservation results.
--
-- ObjC selector: @- results@
results :: IsVNDetectFaceRectanglesRequest vnDetectFaceRectanglesRequest => vnDetectFaceRectanglesRequest -> IO (Id NSArray)
results vnDetectFaceRectanglesRequest =
  sendMessage vnDetectFaceRectanglesRequest resultsSelector

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @results@
resultsSelector :: Selector '[] (Id NSArray)
resultsSelector = mkSelector "results"

