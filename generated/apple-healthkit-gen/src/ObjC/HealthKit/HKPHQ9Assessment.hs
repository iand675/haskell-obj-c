{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Represents the result of a PHQ-9 assessment. Learn more about Pfizer's PHQ-9 at https://support.apple.com/en-us/105070
--
-- Generated bindings for @HKPHQ9Assessment@.
module ObjC.HealthKit.HKPHQ9Assessment
  ( HKPHQ9Assessment
  , IsHKPHQ9Assessment(..)
  , assessmentWithDate_answers
  , assessmentWithDate_answers_metadata
  , init_
  , new
  , answers
  , risk
  , answersSelector
  , assessmentWithDate_answersSelector
  , assessmentWithDate_answers_metadataSelector
  , initSelector
  , newSelector
  , riskSelector

  -- * Enum types
  , HKPHQ9AssessmentRisk(HKPHQ9AssessmentRisk)
  , pattern HKPHQ9AssessmentRiskNoneToMinimal
  , pattern HKPHQ9AssessmentRiskMild
  , pattern HKPHQ9AssessmentRiskModerate
  , pattern HKPHQ9AssessmentRiskModeratelySevere
  , pattern HKPHQ9AssessmentRiskSevere

  ) where

import Foreign.Ptr (Ptr, FunPtr)
import Foreign.C.Types

import ObjC.Runtime.Types
import ObjC.Runtime.Message (sendMessage, sendOwnedMessage, sendClassMessage, sendOwnedClassMessage)
import ObjC.Runtime.Selector (mkSelector)
import ObjC.Runtime.Class (getRequiredClass)

import ObjC.HealthKit.Internal.Classes
import ObjC.HealthKit.Internal.Enums
import ObjC.Foundation.Internal.Classes

-- | Creates a new PHQ-9 sample. There must be exactly 9 elements in answers, each answer must be of type @HKPHQ9AssessmentAnswer@. Question #9 is considered optional. If the user does not answer #9, use @HKPHQ9AssessmentAnswerPreferNotToAnswer@
--
-- ObjC selector: @+ assessmentWithDate:answers:@
assessmentWithDate_answers :: (IsNSDate date, IsNSArray answers) => date -> answers -> IO (Id HKPHQ9Assessment)
assessmentWithDate_answers date answers =
  do
    cls' <- getRequiredClass "HKPHQ9Assessment"
    sendClassMessage cls' assessmentWithDate_answersSelector (toNSDate date) (toNSArray answers)

-- | Creates a new PHQ-9 sample. There must be exactly 9 elements in answers, each answer must be of type @HKPHQ9AssessmentAnswer@. Question #9 is considered optional. If the user does not answer #9, use @HKPHQ9AssessmentAnswerPreferNotToAnswer@
--
-- ObjC selector: @+ assessmentWithDate:answers:metadata:@
assessmentWithDate_answers_metadata :: (IsNSDate date, IsNSArray answers, IsNSDictionary metadata) => date -> answers -> metadata -> IO (Id HKPHQ9Assessment)
assessmentWithDate_answers_metadata date answers metadata =
  do
    cls' <- getRequiredClass "HKPHQ9Assessment"
    sendClassMessage cls' assessmentWithDate_answers_metadataSelector (toNSDate date) (toNSArray answers) (toNSDictionary metadata)

-- | @- init@
init_ :: IsHKPHQ9Assessment hkphQ9Assessment => hkphQ9Assessment -> IO (Id HKPHQ9Assessment)
init_ hkphQ9Assessment =
  sendOwnedMessage hkphQ9Assessment initSelector

-- | @+ new@
new :: IO (Id HKPHQ9Assessment)
new  =
  do
    cls' <- getRequiredClass "HKPHQ9Assessment"
    sendOwnedClassMessage cls' newSelector

-- | Answers on the PHQ-9 assessment. There are exactly 9 answers, one for each multiple choice question. Each answer is of type @HKPHQ9AssessmentAnswer@. If the 9th question was unanswered,  the answer is @HKPHQ9AssessmentAnswerPreferNotToAnswer@.
--
-- ObjC selector: @- answers@
answers :: IsHKPHQ9Assessment hkphQ9Assessment => hkphQ9Assessment -> IO (Id NSArray)
answers hkphQ9Assessment =
  sendMessage hkphQ9Assessment answersSelector

-- | risk
--
-- The risk determined by the score on a PHQ-9 assessment.
--
-- ObjC selector: @- risk@
risk :: IsHKPHQ9Assessment hkphQ9Assessment => hkphQ9Assessment -> IO HKPHQ9AssessmentRisk
risk hkphQ9Assessment =
  sendMessage hkphQ9Assessment riskSelector

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @assessmentWithDate:answers:@
assessmentWithDate_answersSelector :: Selector '[Id NSDate, Id NSArray] (Id HKPHQ9Assessment)
assessmentWithDate_answersSelector = mkSelector "assessmentWithDate:answers:"

-- | @Selector@ for @assessmentWithDate:answers:metadata:@
assessmentWithDate_answers_metadataSelector :: Selector '[Id NSDate, Id NSArray, Id NSDictionary] (Id HKPHQ9Assessment)
assessmentWithDate_answers_metadataSelector = mkSelector "assessmentWithDate:answers:metadata:"

-- | @Selector@ for @init@
initSelector :: Selector '[] (Id HKPHQ9Assessment)
initSelector = mkSelector "init"

-- | @Selector@ for @new@
newSelector :: Selector '[] (Id HKPHQ9Assessment)
newSelector = mkSelector "new"

-- | @Selector@ for @answers@
answersSelector :: Selector '[] (Id NSArray)
answersSelector = mkSelector "answers"

-- | @Selector@ for @risk@
riskSelector :: Selector '[] HKPHQ9AssessmentRisk
riskSelector = mkSelector "risk"

