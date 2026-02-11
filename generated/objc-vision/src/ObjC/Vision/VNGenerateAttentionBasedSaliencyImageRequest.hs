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

-- | VNSaliencyImageObservation results.
--
-- ObjC selector: @- results@
results :: IsVNGenerateAttentionBasedSaliencyImageRequest vnGenerateAttentionBasedSaliencyImageRequest => vnGenerateAttentionBasedSaliencyImageRequest -> IO (Id NSArray)
results vnGenerateAttentionBasedSaliencyImageRequest  =
  sendMsg vnGenerateAttentionBasedSaliencyImageRequest (mkSelector "results") (retPtr retVoid) [] >>= retainedObject . castPtr

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @results@
resultsSelector :: Selector
resultsSelector = mkSelector "results"

