{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | HKScoredAssessment
--
-- An abstract HKSample subclass representing the results of a scored assessment.
--
-- Generated bindings for @HKScoredAssessment@.
module ObjC.HealthKit.HKScoredAssessment
  ( HKScoredAssessment
  , IsHKScoredAssessment(..)
  , init_
  , new
  , score
  , initSelector
  , newSelector
  , scoreSelector


  ) where

import Foreign.Ptr (Ptr, FunPtr)
import Foreign.C.Types

import ObjC.Runtime.Types
import ObjC.Runtime.Message (sendMessage, sendOwnedMessage, sendClassMessage, sendOwnedClassMessage)
import ObjC.Runtime.Selector (mkSelector)
import ObjC.Runtime.Class (getRequiredClass)

import ObjC.HealthKit.Internal.Classes
import ObjC.Foundation.Internal.Classes

-- | @- init@
init_ :: IsHKScoredAssessment hkScoredAssessment => hkScoredAssessment -> IO (Id HKScoredAssessment)
init_ hkScoredAssessment =
  sendOwnedMessage hkScoredAssessment initSelector

-- | @+ new@
new :: IO (Id HKScoredAssessment)
new  =
  do
    cls' <- getRequiredClass "HKScoredAssessment"
    sendOwnedClassMessage cls' newSelector

-- | The score determined by the answers on an assessment
--
-- ObjC selector: @- score@
score :: IsHKScoredAssessment hkScoredAssessment => hkScoredAssessment -> IO CLong
score hkScoredAssessment =
  sendMessage hkScoredAssessment scoreSelector

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @init@
initSelector :: Selector '[] (Id HKScoredAssessment)
initSelector = mkSelector "init"

-- | @Selector@ for @new@
newSelector :: Selector '[] (Id HKScoredAssessment)
newSelector = mkSelector "new"

-- | @Selector@ for @score@
scoreSelector :: Selector '[] CLong
scoreSelector = mkSelector "score"

