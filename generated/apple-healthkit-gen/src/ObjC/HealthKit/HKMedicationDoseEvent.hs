{-# LANGUAGE PatternSynonyms #-}
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
  , initSelector
  , newSelector
  , medicationDoseEventTypeSelector
  , scheduleTypeSelector
  , medicationConceptIdentifierSelector
  , scheduledDateSelector
  , scheduledDoseQuantitySelector
  , doseQuantitySelector
  , logStatusSelector
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

-- | @- init@
init_ :: IsHKMedicationDoseEvent hkMedicationDoseEvent => hkMedicationDoseEvent -> IO (Id HKMedicationDoseEvent)
init_ hkMedicationDoseEvent  =
    sendMsg hkMedicationDoseEvent (mkSelector "init") (retPtr retVoid) [] >>= ownedObject . castPtr

-- | @+ new@
new :: IO (Id HKMedicationDoseEvent)
new  =
  do
    cls' <- getRequiredClass "HKMedicationDoseEvent"
    sendClassMsg cls' (mkSelector "new") (retPtr retVoid) [] >>= ownedObject . castPtr

-- | The data type that identified the samples that store medication dose event data.
--
-- You use this type when creating queries or filtering results by sample type.
--
-- ObjC selector: @- medicationDoseEventType@
medicationDoseEventType :: IsHKMedicationDoseEvent hkMedicationDoseEvent => hkMedicationDoseEvent -> IO (Id HKMedicationDoseEventType)
medicationDoseEventType hkMedicationDoseEvent  =
    sendMsg hkMedicationDoseEvent (mkSelector "medicationDoseEventType") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | The scheduling context for this logged dose event.
--
-- The system sets this to ``HKMedicationDoseEvent/ScheduleType/asNeeded`` when the person logs a dose without a schedule and ``HKMedicationDoseEvent/ScheduleType/schedule`` when a person logs a dose from a scheduled medication reminder.
--
-- ObjC selector: @- scheduleType@
scheduleType :: IsHKMedicationDoseEvent hkMedicationDoseEvent => hkMedicationDoseEvent -> IO HKMedicationDoseEventScheduleType
scheduleType hkMedicationDoseEvent  =
    fmap (coerce :: CLong -> HKMedicationDoseEventScheduleType) $ sendMsg hkMedicationDoseEvent (mkSelector "scheduleType") retCLong []

-- | The identifier of the medication concept the system associates with this dose event.
--
-- The system uses this identifier to link the dose event back to its ``HKMedicationConcept`` object.
--
-- ObjC selector: @- medicationConceptIdentifier@
medicationConceptIdentifier :: IsHKMedicationDoseEvent hkMedicationDoseEvent => hkMedicationDoseEvent -> IO (Id HKHealthConceptIdentifier)
medicationConceptIdentifier hkMedicationDoseEvent  =
    sendMsg hkMedicationDoseEvent (mkSelector "medicationConceptIdentifier") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | The date and time the person takes the medication, if scheduled.
--
-- The value is always non-null for ``HKMedicationDoseEvent/ScheduleType/schedule`` and always null for  ``HKMedicationDoseEvent/ScheduleType/asNeeded``.
--
-- ObjC selector: @- scheduledDate@
scheduledDate :: IsHKMedicationDoseEvent hkMedicationDoseEvent => hkMedicationDoseEvent -> IO (Id NSDate)
scheduledDate hkMedicationDoseEvent  =
    sendMsg hkMedicationDoseEvent (mkSelector "scheduledDate") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | The dose quantity a person is expected to take based on their medication schedule.
--
-- The value is always non-null for ``HKMedicationDoseEvent/ScheduleType/schedule``, and always null for ``HKMedicationDoseEvent/ScheduleType/asNeeded``.
--
-- ObjC selector: @- scheduledDoseQuantity@
scheduledDoseQuantity :: IsHKMedicationDoseEvent hkMedicationDoseEvent => hkMedicationDoseEvent -> IO (Id NSNumber)
scheduledDoseQuantity hkMedicationDoseEvent  =
    sendMsg hkMedicationDoseEvent (mkSelector "scheduledDoseQuantity") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | The dose quantity the person reports as taken.
--
-- For scheduled dose events, the value defaults to the ``HKMedicationDoseEvent/scheduledDoseQuantity-477ge``, when logged from a reminder. For as needed dose events, the value defaults to @1@ in the medication tracking experience, but can always be edited by the person logging.
--
-- ObjC selector: @- doseQuantity@
doseQuantity :: IsHKMedicationDoseEvent hkMedicationDoseEvent => hkMedicationDoseEvent -> IO (Id NSNumber)
doseQuantity hkMedicationDoseEvent  =
    sendMsg hkMedicationDoseEvent (mkSelector "doseQuantity") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | The log status the system assigns to this dose event.
--
-- ObjC selector: @- logStatus@
logStatus :: IsHKMedicationDoseEvent hkMedicationDoseEvent => hkMedicationDoseEvent -> IO HKMedicationDoseEventLogStatus
logStatus hkMedicationDoseEvent  =
    fmap (coerce :: CLong -> HKMedicationDoseEventLogStatus) $ sendMsg hkMedicationDoseEvent (mkSelector "logStatus") retCLong []

-- | The unit that the system associates with the medication when the person logs the dose.
--
-- This ensures that the dose quantity is recorded with the correct measurement unit.
--
-- ObjC selector: @- unit@
unit :: IsHKMedicationDoseEvent hkMedicationDoseEvent => hkMedicationDoseEvent -> IO (Id HKUnit)
unit hkMedicationDoseEvent  =
    sendMsg hkMedicationDoseEvent (mkSelector "unit") (retPtr retVoid) [] >>= retainedObject . castPtr

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @init@
initSelector :: Selector
initSelector = mkSelector "init"

-- | @Selector@ for @new@
newSelector :: Selector
newSelector = mkSelector "new"

-- | @Selector@ for @medicationDoseEventType@
medicationDoseEventTypeSelector :: Selector
medicationDoseEventTypeSelector = mkSelector "medicationDoseEventType"

-- | @Selector@ for @scheduleType@
scheduleTypeSelector :: Selector
scheduleTypeSelector = mkSelector "scheduleType"

-- | @Selector@ for @medicationConceptIdentifier@
medicationConceptIdentifierSelector :: Selector
medicationConceptIdentifierSelector = mkSelector "medicationConceptIdentifier"

-- | @Selector@ for @scheduledDate@
scheduledDateSelector :: Selector
scheduledDateSelector = mkSelector "scheduledDate"

-- | @Selector@ for @scheduledDoseQuantity@
scheduledDoseQuantitySelector :: Selector
scheduledDoseQuantitySelector = mkSelector "scheduledDoseQuantity"

-- | @Selector@ for @doseQuantity@
doseQuantitySelector :: Selector
doseQuantitySelector = mkSelector "doseQuantity"

-- | @Selector@ for @logStatus@
logStatusSelector :: Selector
logStatusSelector = mkSelector "logStatus"

-- | @Selector@ for @unit@
unitSelector :: Selector
unitSelector = mkSelector "unit"

