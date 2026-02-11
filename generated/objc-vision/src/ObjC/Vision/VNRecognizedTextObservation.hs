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

-- | Returns the top N candidates sorted by decreasing confidence score
--
-- This will return no more than N but can be less than N candidates. The maximum number of candidates returned cannot exceed 10 candidates.
--
-- ObjC selector: @- topCandidates:@
topCandidates :: IsVNRecognizedTextObservation vnRecognizedTextObservation => vnRecognizedTextObservation -> CULong -> IO (Id NSArray)
topCandidates vnRecognizedTextObservation  maxCandidateCount =
  sendMsg vnRecognizedTextObservation (mkSelector "topCandidates:") (retPtr retVoid) [argCULong (fromIntegral maxCandidateCount)] >>= retainedObject . castPtr

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @topCandidates:@
topCandidatesSelector :: Selector
topCandidatesSelector = mkSelector "topCandidates:"

