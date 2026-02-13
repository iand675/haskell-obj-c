{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Determine what the horizon tilt of an image is.
--
-- If the horizon tilt is detected in an image, the request will provide a VNHorizonObservation in the results which describe how to transform the image so that the horizon line becomes level.
--
-- Generated bindings for @VNDetectHorizonRequest@.
module ObjC.Vision.VNDetectHorizonRequest
  ( VNDetectHorizonRequest
  , IsVNDetectHorizonRequest(..)
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

-- | VNHorizonObservation results.
--
-- ObjC selector: @- results@
results :: IsVNDetectHorizonRequest vnDetectHorizonRequest => vnDetectHorizonRequest -> IO (Id NSArray)
results vnDetectHorizonRequest =
  sendMessage vnDetectHorizonRequest resultsSelector

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @results@
resultsSelector :: Selector '[] (Id NSArray)
resultsSelector = mkSelector "results"

