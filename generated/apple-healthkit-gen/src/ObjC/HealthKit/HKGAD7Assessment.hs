{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Represents the result of a GAD-7 assessment. Learn more about Pfizer's GAD-7 at https://support.apple.com/en-us/105070
--
-- Generated bindings for @HKGAD7Assessment@.
module ObjC.HealthKit.HKGAD7Assessment
  ( HKGAD7Assessment
  , IsHKGAD7Assessment(..)
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
  , HKGAD7AssessmentRisk(HKGAD7AssessmentRisk)
  , pattern HKGAD7AssessmentRiskNoneToMinimal
  , pattern HKGAD7AssessmentRiskMild
  , pattern HKGAD7AssessmentRiskModerate
  , pattern HKGAD7AssessmentRiskSevere

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

-- | Creates a new GAD-7 sample. There must be exactly 7 elements in answers, each answer must be of type @HKGAD7AssessmentAnswer@.
--
-- ObjC selector: @+ assessmentWithDate:answers:@
assessmentWithDate_answers :: (IsNSDate date, IsNSArray answers) => date -> answers -> IO (Id HKGAD7Assessment)
assessmentWithDate_answers date answers =
  do
    cls' <- getRequiredClass "HKGAD7Assessment"
    sendClassMessage cls' assessmentWithDate_answersSelector (toNSDate date) (toNSArray answers)

-- | Creates a new GAD-7 sample. There must be exactly 7 elements in answers, each answer must be of type @HKGAD7AssessmentAnswer@.
--
-- ObjC selector: @+ assessmentWithDate:answers:metadata:@
assessmentWithDate_answers_metadata :: (IsNSDate date, IsNSArray answers, IsNSDictionary metadata) => date -> answers -> metadata -> IO (Id HKGAD7Assessment)
assessmentWithDate_answers_metadata date answers metadata =
  do
    cls' <- getRequiredClass "HKGAD7Assessment"
    sendClassMessage cls' assessmentWithDate_answers_metadataSelector (toNSDate date) (toNSArray answers) (toNSDictionary metadata)

-- | @- init@
init_ :: IsHKGAD7Assessment hkgaD7Assessment => hkgaD7Assessment -> IO (Id HKGAD7Assessment)
init_ hkgaD7Assessment =
  sendOwnedMessage hkgaD7Assessment initSelector

-- | @+ new@
new :: IO (Id HKGAD7Assessment)
new  =
  do
    cls' <- getRequiredClass "HKGAD7Assessment"
    sendOwnedClassMessage cls' newSelector

-- | Answers on the GAD-7 assessment. There are exactly 7 answers, one for each multiple choice question. Each answer is of type @HKGAD7AssessmentAnswer@.
--
-- ObjC selector: @- answers@
answers :: IsHKGAD7Assessment hkgaD7Assessment => hkgaD7Assessment -> IO (Id NSArray)
answers hkgaD7Assessment =
  sendMessage hkgaD7Assessment answersSelector

-- | risk
--
-- The risk determined by the score on a GAD-7 assessment.
--
-- ObjC selector: @- risk@
risk :: IsHKGAD7Assessment hkgaD7Assessment => hkgaD7Assessment -> IO HKGAD7AssessmentRisk
risk hkgaD7Assessment =
  sendMessage hkgaD7Assessment riskSelector

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @assessmentWithDate:answers:@
assessmentWithDate_answersSelector :: Selector '[Id NSDate, Id NSArray] (Id HKGAD7Assessment)
assessmentWithDate_answersSelector = mkSelector "assessmentWithDate:answers:"

-- | @Selector@ for @assessmentWithDate:answers:metadata:@
assessmentWithDate_answers_metadataSelector :: Selector '[Id NSDate, Id NSArray, Id NSDictionary] (Id HKGAD7Assessment)
assessmentWithDate_answers_metadataSelector = mkSelector "assessmentWithDate:answers:metadata:"

-- | @Selector@ for @init@
initSelector :: Selector '[] (Id HKGAD7Assessment)
initSelector = mkSelector "init"

-- | @Selector@ for @new@
newSelector :: Selector '[] (Id HKGAD7Assessment)
newSelector = mkSelector "new"

-- | @Selector@ for @answers@
answersSelector :: Selector '[] (Id NSArray)
answersSelector = mkSelector "answers"

-- | @Selector@ for @risk@
riskSelector :: Selector '[] HKGAD7AssessmentRisk
riskSelector = mkSelector "risk"

