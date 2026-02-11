{-# LANGUAGE PatternSynonyms #-}
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
  , assessmentWithDate_answersSelector
  , assessmentWithDate_answers_metadataSelector
  , initSelector
  , newSelector
  , answersSelector
  , riskSelector

  -- * Enum types
  , HKGAD7AssessmentRisk(HKGAD7AssessmentRisk)
  , pattern HKGAD7AssessmentRiskNoneToMinimal
  , pattern HKGAD7AssessmentRiskMild
  , pattern HKGAD7AssessmentRiskModerate
  , pattern HKGAD7AssessmentRiskSevere

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

-- | Creates a new GAD-7 sample. There must be exactly 7 elements in answers, each answer must be of type @HKGAD7AssessmentAnswer@.
--
-- ObjC selector: @+ assessmentWithDate:answers:@
assessmentWithDate_answers :: (IsNSDate date, IsNSArray answers) => date -> answers -> IO (Id HKGAD7Assessment)
assessmentWithDate_answers date answers =
  do
    cls' <- getRequiredClass "HKGAD7Assessment"
    withObjCPtr date $ \raw_date ->
      withObjCPtr answers $ \raw_answers ->
        sendClassMsg cls' (mkSelector "assessmentWithDate:answers:") (retPtr retVoid) [argPtr (castPtr raw_date :: Ptr ()), argPtr (castPtr raw_answers :: Ptr ())] >>= retainedObject . castPtr

-- | Creates a new GAD-7 sample. There must be exactly 7 elements in answers, each answer must be of type @HKGAD7AssessmentAnswer@.
--
-- ObjC selector: @+ assessmentWithDate:answers:metadata:@
assessmentWithDate_answers_metadata :: (IsNSDate date, IsNSArray answers, IsNSDictionary metadata) => date -> answers -> metadata -> IO (Id HKGAD7Assessment)
assessmentWithDate_answers_metadata date answers metadata =
  do
    cls' <- getRequiredClass "HKGAD7Assessment"
    withObjCPtr date $ \raw_date ->
      withObjCPtr answers $ \raw_answers ->
        withObjCPtr metadata $ \raw_metadata ->
          sendClassMsg cls' (mkSelector "assessmentWithDate:answers:metadata:") (retPtr retVoid) [argPtr (castPtr raw_date :: Ptr ()), argPtr (castPtr raw_answers :: Ptr ()), argPtr (castPtr raw_metadata :: Ptr ())] >>= retainedObject . castPtr

-- | @- init@
init_ :: IsHKGAD7Assessment hkgaD7Assessment => hkgaD7Assessment -> IO (Id HKGAD7Assessment)
init_ hkgaD7Assessment  =
    sendMsg hkgaD7Assessment (mkSelector "init") (retPtr retVoid) [] >>= ownedObject . castPtr

-- | @+ new@
new :: IO (Id HKGAD7Assessment)
new  =
  do
    cls' <- getRequiredClass "HKGAD7Assessment"
    sendClassMsg cls' (mkSelector "new") (retPtr retVoid) [] >>= ownedObject . castPtr

-- | Answers on the GAD-7 assessment. There are exactly 7 answers, one for each multiple choice question. Each answer is of type @HKGAD7AssessmentAnswer@.
--
-- ObjC selector: @- answers@
answers :: IsHKGAD7Assessment hkgaD7Assessment => hkgaD7Assessment -> IO (Id NSArray)
answers hkgaD7Assessment  =
    sendMsg hkgaD7Assessment (mkSelector "answers") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | risk
--
-- The risk determined by the score on a GAD-7 assessment.
--
-- ObjC selector: @- risk@
risk :: IsHKGAD7Assessment hkgaD7Assessment => hkgaD7Assessment -> IO HKGAD7AssessmentRisk
risk hkgaD7Assessment  =
    fmap (coerce :: CLong -> HKGAD7AssessmentRisk) $ sendMsg hkgaD7Assessment (mkSelector "risk") retCLong []

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

