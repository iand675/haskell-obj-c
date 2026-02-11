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

-- | VNHorizonObservation results.
--
-- ObjC selector: @- results@
results :: IsVNDetectHorizonRequest vnDetectHorizonRequest => vnDetectHorizonRequest -> IO (Id NSArray)
results vnDetectHorizonRequest  =
  sendMsg vnDetectHorizonRequest (mkSelector "results") (retPtr retVoid) [] >>= retainedObject . castPtr

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @results@
resultsSelector :: Selector
resultsSelector = mkSelector "results"

