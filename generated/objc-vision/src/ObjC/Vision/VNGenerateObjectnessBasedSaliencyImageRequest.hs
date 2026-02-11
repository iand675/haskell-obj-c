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
results :: IsVNGenerateObjectnessBasedSaliencyImageRequest vnGenerateObjectnessBasedSaliencyImageRequest => vnGenerateObjectnessBasedSaliencyImageRequest -> IO (Id NSArray)
results vnGenerateObjectnessBasedSaliencyImageRequest  =
  sendMsg vnGenerateObjectnessBasedSaliencyImageRequest (mkSelector "results") (retPtr retVoid) [] >>= retainedObject . castPtr

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @results@
resultsSelector :: Selector
resultsSelector = mkSelector "results"

