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

-- | @- init@
init_ :: IsVNImageAestheticsScoresObservation vnImageAestheticsScoresObservation => vnImageAestheticsScoresObservation -> IO (Id VNImageAestheticsScoresObservation)
init_ vnImageAestheticsScoresObservation  =
  sendMsg vnImageAestheticsScoresObservation (mkSelector "init") (retPtr retVoid) [] >>= ownedObject . castPtr

-- | A Boolean value that represents images that are not necessarily of poor image quality, but may not have memorable or exciting content.
--
-- ObjC selector: @- isUtility@
isUtility :: IsVNImageAestheticsScoresObservation vnImageAestheticsScoresObservation => vnImageAestheticsScoresObservation -> IO Bool
isUtility vnImageAestheticsScoresObservation  =
  fmap ((/= 0) :: CULong -> Bool) $ sendMsg vnImageAestheticsScoresObservation (mkSelector "isUtility") retCULong []

-- | A score which incorporates aesthetic score, failure score, and utility labels.
--
-- This returns a value within the range of @-1@ and @1@, where @-1@ is least desirable and @1@ is most desirable.
--
-- ObjC selector: @- overallScore@
overallScore :: IsVNImageAestheticsScoresObservation vnImageAestheticsScoresObservation => vnImageAestheticsScoresObservation -> IO CFloat
overallScore vnImageAestheticsScoresObservation  =
  sendMsg vnImageAestheticsScoresObservation (mkSelector "overallScore") retCFloat []

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @init@
initSelector :: Selector
initSelector = mkSelector "init"

-- | @Selector@ for @isUtility@
isUtilitySelector :: Selector
isUtilitySelector = mkSelector "isUtility"

-- | @Selector@ for @overallScore@
overallScoreSelector :: Selector
overallScoreSelector = mkSelector "overallScore"

