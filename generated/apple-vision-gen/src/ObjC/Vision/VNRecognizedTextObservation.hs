{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | VNRecognizedTextObservation
--
-- VNRectangleObservation
--
-- VNRecognizedTextObservation Describes a text area detected and recognized by the VNRecognizeTextRequest request.
--
-- Generated bindings for @VNRecognizedTextObservation@.
module ObjC.Vision.VNRecognizedTextObservation
  ( VNRecognizedTextObservation
  , IsVNRecognizedTextObservation(..)
  , topCandidates
  , topCandidatesSelector


  ) where

import Foreign.Ptr (Ptr, FunPtr)
import Foreign.C.Types

import ObjC.Runtime.Types
import ObjC.Runtime.Message (sendMessage, sendOwnedMessage, sendClassMessage, sendOwnedClassMessage)
import ObjC.Runtime.Selector (mkSelector)
import ObjC.Runtime.Class (getRequiredClass)

import ObjC.Vision.Internal.Classes
import ObjC.Foundation.Internal.Classes

-- | Returns the top N candidates sorted by decreasing confidence score
--
-- This will return no more than N but can be less than N candidates. The maximum number of candidates returned cannot exceed 10 candidates.
--
-- ObjC selector: @- topCandidates:@
topCandidates :: IsVNRecognizedTextObservation vnRecognizedTextObservation => vnRecognizedTextObservation -> CULong -> IO (Id NSArray)
topCandidates vnRecognizedTextObservation maxCandidateCount =
  sendMessage vnRecognizedTextObservation topCandidatesSelector maxCandidateCount

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @topCandidates:@
topCandidatesSelector :: Selector '[CULong] (Id NSArray)
topCandidatesSelector = mkSelector "topCandidates:"

