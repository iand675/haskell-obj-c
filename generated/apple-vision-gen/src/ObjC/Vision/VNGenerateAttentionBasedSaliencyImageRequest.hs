{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Generates an image that identifies which part(s) of a given image is most interesting (i.e. something that a human is likely to look at - hence attention based).			The resulting observation, VNSaliencyImageObservation, encodes this data as a heat map which can be used to highlight regions of interest.
--
-- Generated bindings for @VNGenerateAttentionBasedSaliencyImageRequest@.
module ObjC.Vision.VNGenerateAttentionBasedSaliencyImageRequest
  ( VNGenerateAttentionBasedSaliencyImageRequest
  , IsVNGenerateAttentionBasedSaliencyImageRequest(..)
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
results :: IsVNGenerateAttentionBasedSaliencyImageRequest vnGenerateAttentionBasedSaliencyImageRequest => vnGenerateAttentionBasedSaliencyImageRequest -> IO (Id NSArray)
results vnGenerateAttentionBasedSaliencyImageRequest =
  sendMessage vnGenerateAttentionBasedSaliencyImageRequest resultsSelector

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @results@
resultsSelector :: Selector '[] (Id NSArray)
resultsSelector = mkSelector "results"

