{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | VNImageAestheticsScoresObservation
--
-- VNObservation
--
-- VNImageAestheticsScoresObservation provides an overall score of aesthetic attributes for an image.
--
-- Generated bindings for @VNImageAestheticsScoresObservation@.
module ObjC.Vision.VNImageAestheticsScoresObservation
  ( VNImageAestheticsScoresObservation
  , IsVNImageAestheticsScoresObservation(..)
  , init_
  , isUtility
  , overallScore
  , initSelector
  , isUtilitySelector
  , overallScoreSelector


  ) where

import Foreign.Ptr (Ptr, FunPtr)
import Foreign.C.Types

import ObjC.Runtime.Types
import ObjC.Runtime.Message (sendMessage, sendOwnedMessage, sendClassMessage, sendOwnedClassMessage)
import ObjC.Runtime.Selector (mkSelector)
import ObjC.Runtime.Class (getRequiredClass)

import ObjC.Vision.Internal.Classes
import ObjC.Foundation.Internal.Classes

-- | @- init@
init_ :: IsVNImageAestheticsScoresObservation vnImageAestheticsScoresObservation => vnImageAestheticsScoresObservation -> IO (Id VNImageAestheticsScoresObservation)
init_ vnImageAestheticsScoresObservation =
  sendOwnedMessage vnImageAestheticsScoresObservation initSelector

-- | A Boolean value that represents images that are not necessarily of poor image quality, but may not have memorable or exciting content.
--
-- ObjC selector: @- isUtility@
isUtility :: IsVNImageAestheticsScoresObservation vnImageAestheticsScoresObservation => vnImageAestheticsScoresObservation -> IO Bool
isUtility vnImageAestheticsScoresObservation =
  sendMessage vnImageAestheticsScoresObservation isUtilitySelector

-- | A score which incorporates aesthetic score, failure score, and utility labels.
--
-- This returns a value within the range of @-1@ and @1@, where @-1@ is least desirable and @1@ is most desirable.
--
-- ObjC selector: @- overallScore@
overallScore :: IsVNImageAestheticsScoresObservation vnImageAestheticsScoresObservation => vnImageAestheticsScoresObservation -> IO CFloat
overallScore vnImageAestheticsScoresObservation =
  sendMessage vnImageAestheticsScoresObservation overallScoreSelector

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @init@
initSelector :: Selector '[] (Id VNImageAestheticsScoresObservation)
initSelector = mkSelector "init"

-- | @Selector@ for @isUtility@
isUtilitySelector :: Selector '[] Bool
isUtilitySelector = mkSelector "isUtility"

-- | @Selector@ for @overallScore@
overallScoreSelector :: Selector '[] CFloat
overallScoreSelector = mkSelector "overallScore"

