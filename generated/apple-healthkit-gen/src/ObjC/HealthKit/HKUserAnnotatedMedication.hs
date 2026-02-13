{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | A reference to the tracked medication and the details a person can customize.
--
-- The details are relevant to the medication tracking experience.
--
-- Generated bindings for @HKUserAnnotatedMedication@.
module ObjC.HealthKit.HKUserAnnotatedMedication
  ( HKUserAnnotatedMedication
  , IsHKUserAnnotatedMedication(..)
  , init_
  , nickname
  , isArchived
  , hasSchedule
  , medication
  , hasScheduleSelector
  , initSelector
  , isArchivedSelector
  , medicationSelector
  , nicknameSelector


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
init_ :: IsHKUserAnnotatedMedication hkUserAnnotatedMedication => hkUserAnnotatedMedication -> IO (Id HKUserAnnotatedMedication)
init_ hkUserAnnotatedMedication =
  sendOwnedMessage hkUserAnnotatedMedication initSelector

-- | The nickname that a person added to a medication during the entry experience.
--
-- This can be edited at any point.
--
-- ObjC selector: @- nickname@
nickname :: IsHKUserAnnotatedMedication hkUserAnnotatedMedication => hkUserAnnotatedMedication -> IO (Id NSString)
nickname hkUserAnnotatedMedication =
  sendMessage hkUserAnnotatedMedication nicknameSelector

-- | A Boolean value that indicates whether a medication is archived.
--
-- The value is @true@ if a person moves a medication to the archived section in the Health App. The value is @false@ if a medication isn't in the archived section.
--
-- ObjC selector: @- isArchived@
isArchived :: IsHKUserAnnotatedMedication hkUserAnnotatedMedication => hkUserAnnotatedMedication -> IO Bool
isArchived hkUserAnnotatedMedication =
  sendMessage hkUserAnnotatedMedication isArchivedSelector

-- | A Boolean value that indicates whether a medication has a schedule set up.
--
-- The value is @true@ for medications for which a person has set up reminders and @false@ for medications that are only taken as needed. > Note: Scheduled medications can still be taken as needed.
--
-- ObjC selector: @- hasSchedule@
hasSchedule :: IsHKUserAnnotatedMedication hkUserAnnotatedMedication => hkUserAnnotatedMedication -> IO Bool
hasSchedule hkUserAnnotatedMedication =
  sendMessage hkUserAnnotatedMedication hasScheduleSelector

-- | A reference to the specific medication a person is tracking.
--
-- This concept's identifier is directly associated with the logged dose events.
--
-- ObjC selector: @- medication@
medication :: IsHKUserAnnotatedMedication hkUserAnnotatedMedication => hkUserAnnotatedMedication -> IO (Id HKMedicationConcept)
medication hkUserAnnotatedMedication =
  sendMessage hkUserAnnotatedMedication medicationSelector

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @init@
initSelector :: Selector '[] (Id HKUserAnnotatedMedication)
initSelector = mkSelector "init"

-- | @Selector@ for @nickname@
nicknameSelector :: Selector '[] (Id NSString)
nicknameSelector = mkSelector "nickname"

-- | @Selector@ for @isArchived@
isArchivedSelector :: Selector '[] Bool
isArchivedSelector = mkSelector "isArchived"

-- | @Selector@ for @hasSchedule@
hasScheduleSelector :: Selector '[] Bool
hasScheduleSelector = mkSelector "hasSchedule"

-- | @Selector@ for @medication@
medicationSelector :: Selector '[] (Id HKMedicationConcept)
medicationSelector = mkSelector "medication"

