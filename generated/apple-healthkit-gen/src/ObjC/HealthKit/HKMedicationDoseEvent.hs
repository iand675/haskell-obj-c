{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Generated bindings for @HKMedicationDoseEvent@.
module ObjC.HealthKit.HKMedicationDoseEvent
  ( HKMedicationDoseEvent
  , IsHKMedicationDoseEvent(..)
  , init_
  , new
  , medicationDoseEventType
  , scheduleType
  , medicationConceptIdentifier
  , scheduledDate
  , scheduledDoseQuantity
  , doseQuantity
  , logStatus
  , unit
  , doseQuantitySelector
  , initSelector
  , logStatusSelector
  , medicationConceptIdentifierSelector
  , medicationDoseEventTypeSelector
  , newSelector
  , scheduleTypeSelector
  , scheduledDateSelector
  , scheduledDoseQuantitySelector
  , unitSelector

  -- * Enum types
  , HKMedicationDoseEventLogStatus(HKMedicationDoseEventLogStatus)
  , pattern HKMedicationDoseEventLogStatusNotInteracted
  , pattern HKMedicationDoseEventLogStatusNotificationNotSent
  , pattern HKMedicationDoseEventLogStatusSnoozed
  , pattern HKMedicationDoseEventLogStatusTaken
  , pattern HKMedicationDoseEventLogStatusSkipped
  , pattern HKMedicationDoseEventLogStatusNotLogged
  , HKMedicationDoseEventScheduleType(HKMedicationDoseEventScheduleType)
  , pattern HKMedicationDoseEventScheduleTypeAsNeeded
  , pattern HKMedicationDoseEventScheduleTypeSchedule

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

-- | @- init@
init_ :: IsHKMedicationDoseEvent hkMedicationDoseEvent => hkMedicationDoseEvent -> IO (Id HKMedicationDoseEvent)
init_ hkMedicationDoseEvent =
  sendOwnedMessage hkMedicationDoseEvent initSelector

-- | @+ new@
new :: IO (Id HKMedicationDoseEvent)
new  =
  do
    cls' <- getRequiredClass "HKMedicationDoseEvent"
    sendOwnedClassMessage cls' newSelector

-- | The data type that identified the samples that store medication dose event data.
--
-- You use this type when creating queries or filtering results by sample type.
--
-- ObjC selector: @- medicationDoseEventType@
medicationDoseEventType :: IsHKMedicationDoseEvent hkMedicationDoseEvent => hkMedicationDoseEvent -> IO (Id HKMedicationDoseEventType)
medicationDoseEventType hkMedicationDoseEvent =
  sendMessage hkMedicationDoseEvent medicationDoseEventTypeSelector

-- | The scheduling context for this logged dose event.
--
-- The system sets this to ``HKMedicationDoseEvent/ScheduleType/asNeeded`` when the person logs a dose without a schedule and ``HKMedicationDoseEvent/ScheduleType/schedule`` when a person logs a dose from a scheduled medication reminder.
--
-- ObjC selector: @- scheduleType@
scheduleType :: IsHKMedicationDoseEvent hkMedicationDoseEvent => hkMedicationDoseEvent -> IO HKMedicationDoseEventScheduleType
scheduleType hkMedicationDoseEvent =
  sendMessage hkMedicationDoseEvent scheduleTypeSelector

-- | The identifier of the medication concept the system associates with this dose event.
--
-- The system uses this identifier to link the dose event back to its ``HKMedicationConcept`` object.
--
-- ObjC selector: @- medicationConceptIdentifier@
medicationConceptIdentifier :: IsHKMedicationDoseEvent hkMedicationDoseEvent => hkMedicationDoseEvent -> IO (Id HKHealthConceptIdentifier)
medicationConceptIdentifier hkMedicationDoseEvent =
  sendMessage hkMedicationDoseEvent medicationConceptIdentifierSelector

-- | The date and time the person takes the medication, if scheduled.
--
-- The value is always non-null for ``HKMedicationDoseEvent/ScheduleType/schedule`` and always null for  ``HKMedicationDoseEvent/ScheduleType/asNeeded``.
--
-- ObjC selector: @- scheduledDate@
scheduledDate :: IsHKMedicationDoseEvent hkMedicationDoseEvent => hkMedicationDoseEvent -> IO (Id NSDate)
scheduledDate hkMedicationDoseEvent =
  sendMessage hkMedicationDoseEvent scheduledDateSelector

-- | The dose quantity a person is expected to take based on their medication schedule.
--
-- The value is always non-null for ``HKMedicationDoseEvent/ScheduleType/schedule``, and always null for ``HKMedicationDoseEvent/ScheduleType/asNeeded``.
--
-- ObjC selector: @- scheduledDoseQuantity@
scheduledDoseQuantity :: IsHKMedicationDoseEvent hkMedicationDoseEvent => hkMedicationDoseEvent -> IO (Id NSNumber)
scheduledDoseQuantity hkMedicationDoseEvent =
  sendMessage hkMedicationDoseEvent scheduledDoseQuantitySelector

-- | The dose quantity the person reports as taken.
--
-- For scheduled dose events, the value defaults to the ``HKMedicationDoseEvent/scheduledDoseQuantity-477ge``, when logged from a reminder. For as needed dose events, the value defaults to @1@ in the medication tracking experience, but can always be edited by the person logging.
--
-- ObjC selector: @- doseQuantity@
doseQuantity :: IsHKMedicationDoseEvent hkMedicationDoseEvent => hkMedicationDoseEvent -> IO (Id NSNumber)
doseQuantity hkMedicationDoseEvent =
  sendMessage hkMedicationDoseEvent doseQuantitySelector

-- | The log status the system assigns to this dose event.
--
-- ObjC selector: @- logStatus@
logStatus :: IsHKMedicationDoseEvent hkMedicationDoseEvent => hkMedicationDoseEvent -> IO HKMedicationDoseEventLogStatus
logStatus hkMedicationDoseEvent =
  sendMessage hkMedicationDoseEvent logStatusSelector

-- | The unit that the system associates with the medication when the person logs the dose.
--
-- This ensures that the dose quantity is recorded with the correct measurement unit.
--
-- ObjC selector: @- unit@
unit :: IsHKMedicationDoseEvent hkMedicationDoseEvent => hkMedicationDoseEvent -> IO (Id HKUnit)
unit hkMedicationDoseEvent =
  sendMessage hkMedicationDoseEvent unitSelector

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @init@
initSelector :: Selector '[] (Id HKMedicationDoseEvent)
initSelector = mkSelector "init"

-- | @Selector@ for @new@
newSelector :: Selector '[] (Id HKMedicationDoseEvent)
newSelector = mkSelector "new"

-- | @Selector@ for @medicationDoseEventType@
medicationDoseEventTypeSelector :: Selector '[] (Id HKMedicationDoseEventType)
medicationDoseEventTypeSelector = mkSelector "medicationDoseEventType"

-- | @Selector@ for @scheduleType@
scheduleTypeSelector :: Selector '[] HKMedicationDoseEventScheduleType
scheduleTypeSelector = mkSelector "scheduleType"

-- | @Selector@ for @medicationConceptIdentifier@
medicationConceptIdentifierSelector :: Selector '[] (Id HKHealthConceptIdentifier)
medicationConceptIdentifierSelector = mkSelector "medicationConceptIdentifier"

-- | @Selector@ for @scheduledDate@
scheduledDateSelector :: Selector '[] (Id NSDate)
scheduledDateSelector = mkSelector "scheduledDate"

-- | @Selector@ for @scheduledDoseQuantity@
scheduledDoseQuantitySelector :: Selector '[] (Id NSNumber)
scheduledDoseQuantitySelector = mkSelector "scheduledDoseQuantity"

-- | @Selector@ for @doseQuantity@
doseQuantitySelector :: Selector '[] (Id NSNumber)
doseQuantitySelector = mkSelector "doseQuantity"

-- | @Selector@ for @logStatus@
logStatusSelector :: Selector '[] HKMedicationDoseEventLogStatus
logStatusSelector = mkSelector "logStatus"

-- | @Selector@ for @unit@
unitSelector :: Selector '[] (Id HKUnit)
unitSelector = mkSelector "unit"

