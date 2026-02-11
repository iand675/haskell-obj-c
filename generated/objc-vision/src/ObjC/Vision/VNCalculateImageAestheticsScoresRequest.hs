{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Analyzes an image for aesthetically pleasing attributes and returns a VNImageAestheticsScoresObservation.             This observation calculates an overall aeshetically pleasing score for the image and checks for utility images.
--
-- Generated bindings for @VNCalculateImageAestheticsScoresRequest@.
module ObjC.Vision.VNCalculateImageAestheticsScoresRequest
  ( VNCalculateImageAestheticsScoresRequest
  , IsVNCalculateImageAestheticsScoresRequest(..)
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

-- | VNObservation results.
--
-- ObjC selector: @- results@
results :: IsVNCalculateImageAestheticsScoresRequest vnCalculateImageAestheticsScoresRequest => vnCalculateImageAestheticsScoresRequest -> IO (Id NSArray)
results vnCalculateImageAestheticsScoresRequest  =
  sendMsg vnCalculateImageAestheticsScoresRequest (mkSelector "results") (retPtr retVoid) [] >>= retainedObject . castPtr

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @results@
resultsSelector :: Selector
resultsSelector = mkSelector "results"

