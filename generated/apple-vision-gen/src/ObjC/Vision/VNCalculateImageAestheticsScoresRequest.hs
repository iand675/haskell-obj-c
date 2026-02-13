{-# LANGUAGE DataKinds #-}
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

import Foreign.Ptr (Ptr, FunPtr)
import Foreign.C.Types

import ObjC.Runtime.Types
import ObjC.Runtime.Message (sendMessage, sendOwnedMessage, sendClassMessage, sendOwnedClassMessage)
import ObjC.Runtime.Selector (mkSelector)
import ObjC.Runtime.Class (getRequiredClass)

import ObjC.Vision.Internal.Classes
import ObjC.Foundation.Internal.Classes

-- | VNObservation results.
--
-- ObjC selector: @- results@
results :: IsVNCalculateImageAestheticsScoresRequest vnCalculateImageAestheticsScoresRequest => vnCalculateImageAestheticsScoresRequest -> IO (Id NSArray)
results vnCalculateImageAestheticsScoresRequest =
  sendMessage vnCalculateImageAestheticsScoresRequest resultsSelector

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @results@
resultsSelector :: Selector '[] (Id NSArray)
resultsSelector = mkSelector "results"

