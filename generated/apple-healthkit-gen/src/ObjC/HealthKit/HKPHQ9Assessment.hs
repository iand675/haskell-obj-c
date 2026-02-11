{-# LANGUAGE PatternSynonyms #-}
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
  , assessmentWithDate_answersSelector
  , assessmentWithDate_answers_metadataSelector
  , initSelector
  , newSelector
  , answersSelector
  , riskSelector

  -- * Enum types
  , HKPHQ9AssessmentRisk(HKPHQ9AssessmentRisk)
  , pattern HKPHQ9AssessmentRiskNoneToMinimal
  , pattern HKPHQ9AssessmentRiskMild
  , pattern HKPHQ9AssessmentRiskModerate
  , pattern HKPHQ9AssessmentRiskModeratelySevere
  , pattern HKPHQ9AssessmentRiskSevere

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
    withObjCPtr date $ \raw_date ->
      withObjCPtr answers $ \raw_answers ->
        sendClassMsg cls' (mkSelector "assessmentWithDate:answers:") (retPtr retVoid) [argPtr (castPtr raw_date :: Ptr ()), argPtr (castPtr raw_answers :: Ptr ())] >>= retainedObject . castPtr

-- | Creates a new PHQ-9 sample. There must be exactly 9 elements in answers, each answer must be of type @HKPHQ9AssessmentAnswer@. Question #9 is considered optional. If the user does not answer #9, use @HKPHQ9AssessmentAnswerPreferNotToAnswer@
--
-- ObjC selector: @+ assessmentWithDate:answers:metadata:@
assessmentWithDate_answers_metadata :: (IsNSDate date, IsNSArray answers, IsNSDictionary metadata) => date -> answers -> metadata -> IO (Id HKPHQ9Assessment)
assessmentWithDate_answers_metadata date answers metadata =
  do
    cls' <- getRequiredClass "HKPHQ9Assessment"
    withObjCPtr date $ \raw_date ->
      withObjCPtr answers $ \raw_answers ->
        withObjCPtr metadata $ \raw_metadata ->
          sendClassMsg cls' (mkSelector "assessmentWithDate:answers:metadata:") (retPtr retVoid) [argPtr (castPtr raw_date :: Ptr ()), argPtr (castPtr raw_answers :: Ptr ()), argPtr (castPtr raw_metadata :: Ptr ())] >>= retainedObject . castPtr

-- | @- init@
init_ :: IsHKPHQ9Assessment hkphQ9Assessment => hkphQ9Assessment -> IO (Id HKPHQ9Assessment)
init_ hkphQ9Assessment  =
    sendMsg hkphQ9Assessment (mkSelector "init") (retPtr retVoid) [] >>= ownedObject . castPtr

-- | @+ new@
new :: IO (Id HKPHQ9Assessment)
new  =
  do
    cls' <- getRequiredClass "HKPHQ9Assessment"
    sendClassMsg cls' (mkSelector "new") (retPtr retVoid) [] >>= ownedObject . castPtr

-- | Answers on the PHQ-9 assessment. There are exactly 9 answers, one for each multiple choice question. Each answer is of type @HKPHQ9AssessmentAnswer@. If the 9th question was unanswered,  the answer is @HKPHQ9AssessmentAnswerPreferNotToAnswer@.
--
-- ObjC selector: @- answers@
answers :: IsHKPHQ9Assessment hkphQ9Assessment => hkphQ9Assessment -> IO (Id NSArray)
answers hkphQ9Assessment  =
    sendMsg hkphQ9Assessment (mkSelector "answers") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | risk
--
-- The risk determined by the score on a PHQ-9 assessment.
--
-- ObjC selector: @- risk@
risk :: IsHKPHQ9Assessment hkphQ9Assessment => hkphQ9Assessment -> IO HKPHQ9AssessmentRisk
risk hkphQ9Assessment  =
    fmap (coerce :: CLong -> HKPHQ9AssessmentRisk) $ sendMsg hkphQ9Assessment (mkSelector "risk") retCLong []

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @assessmentWithDate:answers:@
assessmentWithDate_answersSelector :: Selector
assessmentWithDate_answersSelector = mkSelector "assessmentWithDate:answers:"

-- | @Selector@ for @assessmentWithDate:answers:metadata:@
assessmentWithDate_answers_metadataSelector :: Selector
assessmentWithDate_answers_metadataSelector = mkSelector "assessmentWithDate:answers:metadata:"

-- | @Selector@ for @init@
initSelector :: Selector
initSelector = mkSelector "init"

-- | @Selector@ for @new@
newSelector :: Selector
newSelector = mkSelector "new"

-- | @Selector@ for @answers@
answersSelector :: Selector
answersSelector = mkSelector "answers"

-- | @Selector@ for @risk@
riskSelector :: Selector
riskSelector = mkSelector "risk"

