{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Generates an image that identifies which part(s) of a given image are most likely to be objects (i.e. something that a human is likely to see as an object). The resulting observation, VNSaliencyImageObservation, encodes this data as a heat map which can be used highlight regions of interest.
--
-- Generated bindings for @VNGenerateObjectnessBasedSaliencyImageRequest@.
module ObjC.Vision.VNGenerateObjectnessBasedSaliencyImageRequest
  ( VNGenerateObjectnessBasedSaliencyImageRequest
  , IsVNGenerateObjectnessBasedSaliencyImageRequest(..)
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

-- | VNSaliencyImageObservation results.
--
-- ObjC selector: @- results@
results :: IsVNGenerateObjectnessBasedSaliencyImageRequest vnGenerateObjectnessBasedSaliencyImageRequest => vnGenerateObjectnessBasedSaliencyImageRequest -> IO (Id NSArray)
results vnGenerateObjectnessBasedSaliencyImageRequest =
  sendMessage vnGenerateObjectnessBasedSaliencyImageRequest resultsSelector

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @results@
resultsSelector :: Selector '[] (Id NSArray)
resultsSelector = mkSelector "results"

