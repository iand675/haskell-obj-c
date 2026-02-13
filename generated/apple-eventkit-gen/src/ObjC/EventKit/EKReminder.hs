{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | EKReminder
--
-- The EKReminder class represents a reminder (task/todo).
--
-- Generated bindings for @EKReminder@.
module ObjC.EventKit.EKReminder
  ( EKReminder
  , IsEKReminder(..)
  , reminderWithEventStore
  , startDateComponents
  , setStartDateComponents
  , dueDateComponents
  , setDueDateComponents
  , completed
  , setCompleted
  , completionDate
  , setCompletionDate
  , priority
  , setPriority
  , completedSelector
  , completionDateSelector
  , dueDateComponentsSelector
  , prioritySelector
  , reminderWithEventStoreSelector
  , setCompletedSelector
  , setCompletionDateSelector
  , setDueDateComponentsSelector
  , setPrioritySelector
  , setStartDateComponentsSelector
  , startDateComponentsSelector


  ) where

import Foreign.Ptr (Ptr, FunPtr)
import Foreign.C.Types

import ObjC.Runtime.Types
import ObjC.Runtime.Message (sendMessage, sendOwnedMessage, sendClassMessage, sendOwnedClassMessage)
import ObjC.Runtime.Selector (mkSelector)
import ObjC.Runtime.Class (getRequiredClass)

import ObjC.EventKit.Internal.Classes
import ObjC.Foundation.Internal.Classes

-- | reminderWithEventStore:
--
-- Creates a new reminder in the given event store.
--
-- ObjC selector: @+ reminderWithEventStore:@
reminderWithEventStore :: IsEKEventStore eventStore => eventStore -> IO (Id EKReminder)
reminderWithEventStore eventStore =
  do
    cls' <- getRequiredClass "EKReminder"
    sendClassMessage cls' reminderWithEventStoreSelector (toEKEventStore eventStore)

-- | startDateComponents
--
-- The start date of the task, as date components.
--
-- The use of date components allows the start date and its time zone to be represented in a single property.                 A nil time zone represents a floating date.  Setting a date component without a hour, minute and second component will set allDay to YES.                If you set this property, the calendar must be set to NSCalendarIdentifierGregorian. An exception is raised otherwise.
--
-- ObjC selector: @- startDateComponents@
startDateComponents :: IsEKReminder ekReminder => ekReminder -> IO (Id NSDateComponents)
startDateComponents ekReminder =
  sendMessage ekReminder startDateComponentsSelector

-- | startDateComponents
--
-- The start date of the task, as date components.
--
-- The use of date components allows the start date and its time zone to be represented in a single property.                 A nil time zone represents a floating date.  Setting a date component without a hour, minute and second component will set allDay to YES.                If you set this property, the calendar must be set to NSCalendarIdentifierGregorian. An exception is raised otherwise.
--
-- ObjC selector: @- setStartDateComponents:@
setStartDateComponents :: (IsEKReminder ekReminder, IsNSDateComponents value) => ekReminder -> value -> IO ()
setStartDateComponents ekReminder value =
  sendMessage ekReminder setStartDateComponentsSelector (toNSDateComponents value)

-- | dueDateComponents
--
-- The date by which this reminder should be completed.
--
-- The use of date components allows the due date and its time zone to be represented in a single property.                 A nil time zone represents a floating date.  Setting a date component without a hour, minute and second component will set allDay to YES.                If you set this property, the calendar must be set to NSCalendarIdentifierGregorian. An exception is raised otherwise.
--
-- On iOS, if you set the due date for a reminder, you must also set a start date, otherwise you will receive                an error (EKErrorNoStartDate) when attempting to save this reminder. This is not a requirement on OS X.
--
-- ObjC selector: @- dueDateComponents@
dueDateComponents :: IsEKReminder ekReminder => ekReminder -> IO (Id NSDateComponents)
dueDateComponents ekReminder =
  sendMessage ekReminder dueDateComponentsSelector

-- | dueDateComponents
--
-- The date by which this reminder should be completed.
--
-- The use of date components allows the due date and its time zone to be represented in a single property.                 A nil time zone represents a floating date.  Setting a date component without a hour, minute and second component will set allDay to YES.                If you set this property, the calendar must be set to NSCalendarIdentifierGregorian. An exception is raised otherwise.
--
-- On iOS, if you set the due date for a reminder, you must also set a start date, otherwise you will receive                an error (EKErrorNoStartDate) when attempting to save this reminder. This is not a requirement on OS X.
--
-- ObjC selector: @- setDueDateComponents:@
setDueDateComponents :: (IsEKReminder ekReminder, IsNSDateComponents value) => ekReminder -> value -> IO ()
setDueDateComponents ekReminder value =
  sendMessage ekReminder setDueDateComponentsSelector (toNSDateComponents value)

-- | completed
--
-- Whether or not the reminder is completed.
--
-- Setting it to YES will set the completed date to the current date.                 Setting it to NO will set the completed date to nil.
--
-- ObjC selector: @- completed@
completed :: IsEKReminder ekReminder => ekReminder -> IO Bool
completed ekReminder =
  sendMessage ekReminder completedSelector

-- | completed
--
-- Whether or not the reminder is completed.
--
-- Setting it to YES will set the completed date to the current date.                 Setting it to NO will set the completed date to nil.
--
-- ObjC selector: @- setCompleted:@
setCompleted :: IsEKReminder ekReminder => ekReminder -> Bool -> IO ()
setCompleted ekReminder value =
  sendMessage ekReminder setCompletedSelector value

-- | completionDate
--
-- The date on which this reminder was completed.
--
-- ObjC selector: @- completionDate@
completionDate :: IsEKReminder ekReminder => ekReminder -> IO (Id NSDate)
completionDate ekReminder =
  sendMessage ekReminder completionDateSelector

-- | completionDate
--
-- The date on which this reminder was completed.
--
-- ObjC selector: @- setCompletionDate:@
setCompletionDate :: (IsEKReminder ekReminder, IsNSDate value) => ekReminder -> value -> IO ()
setCompletionDate ekReminder value =
  sendMessage ekReminder setCompletionDateSelector (toNSDate value)

-- | priority
--
-- The priority of the reminder.
--
-- Priorities run from 1 (highest) to 9 (lowest).  A priority of 0 means no priority.                Saving a reminder with any other priority will fail.                Per RFC 5545, priorities of 1-4 are considered "high," a priority of 5 is "medium," and priorities of 6-9 are "low."
--
-- ObjC selector: @- priority@
priority :: IsEKReminder ekReminder => ekReminder -> IO CULong
priority ekReminder =
  sendMessage ekReminder prioritySelector

-- | priority
--
-- The priority of the reminder.
--
-- Priorities run from 1 (highest) to 9 (lowest).  A priority of 0 means no priority.                Saving a reminder with any other priority will fail.                Per RFC 5545, priorities of 1-4 are considered "high," a priority of 5 is "medium," and priorities of 6-9 are "low."
--
-- ObjC selector: @- setPriority:@
setPriority :: IsEKReminder ekReminder => ekReminder -> CULong -> IO ()
setPriority ekReminder value =
  sendMessage ekReminder setPrioritySelector value

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @reminderWithEventStore:@
reminderWithEventStoreSelector :: Selector '[Id EKEventStore] (Id EKReminder)
reminderWithEventStoreSelector = mkSelector "reminderWithEventStore:"

-- | @Selector@ for @startDateComponents@
startDateComponentsSelector :: Selector '[] (Id NSDateComponents)
startDateComponentsSelector = mkSelector "startDateComponents"

-- | @Selector@ for @setStartDateComponents:@
setStartDateComponentsSelector :: Selector '[Id NSDateComponents] ()
setStartDateComponentsSelector = mkSelector "setStartDateComponents:"

-- | @Selector@ for @dueDateComponents@
dueDateComponentsSelector :: Selector '[] (Id NSDateComponents)
dueDateComponentsSelector = mkSelector "dueDateComponents"

-- | @Selector@ for @setDueDateComponents:@
setDueDateComponentsSelector :: Selector '[Id NSDateComponents] ()
setDueDateComponentsSelector = mkSelector "setDueDateComponents:"

-- | @Selector@ for @completed@
completedSelector :: Selector '[] Bool
completedSelector = mkSelector "completed"

-- | @Selector@ for @setCompleted:@
setCompletedSelector :: Selector '[Bool] ()
setCompletedSelector = mkSelector "setCompleted:"

-- | @Selector@ for @completionDate@
completionDateSelector :: Selector '[] (Id NSDate)
completionDateSelector = mkSelector "completionDate"

-- | @Selector@ for @setCompletionDate:@
setCompletionDateSelector :: Selector '[Id NSDate] ()
setCompletionDateSelector = mkSelector "setCompletionDate:"

-- | @Selector@ for @priority@
prioritySelector :: Selector '[] CULong
prioritySelector = mkSelector "priority"

-- | @Selector@ for @setPriority:@
setPrioritySelector :: Selector '[CULong] ()
setPrioritySelector = mkSelector "setPriority:"

