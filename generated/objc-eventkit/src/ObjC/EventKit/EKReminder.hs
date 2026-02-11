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
  , reminderWithEventStoreSelector
  , startDateComponentsSelector
  , setStartDateComponentsSelector
  , dueDateComponentsSelector
  , setDueDateComponentsSelector
  , completedSelector
  , setCompletedSelector
  , completionDateSelector
  , setCompletionDateSelector
  , prioritySelector
  , setPrioritySelector


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
    withObjCPtr eventStore $ \raw_eventStore ->
      sendClassMsg cls' (mkSelector "reminderWithEventStore:") (retPtr retVoid) [argPtr (castPtr raw_eventStore :: Ptr ())] >>= retainedObject . castPtr

-- | startDateComponents
--
-- The start date of the task, as date components.
--
-- The use of date components allows the start date and its time zone to be represented in a single property.                 A nil time zone represents a floating date.  Setting a date component without a hour, minute and second component will set allDay to YES.                If you set this property, the calendar must be set to NSCalendarIdentifierGregorian. An exception is raised otherwise.
--
-- ObjC selector: @- startDateComponents@
startDateComponents :: IsEKReminder ekReminder => ekReminder -> IO (Id NSDateComponents)
startDateComponents ekReminder  =
  sendMsg ekReminder (mkSelector "startDateComponents") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | startDateComponents
--
-- The start date of the task, as date components.
--
-- The use of date components allows the start date and its time zone to be represented in a single property.                 A nil time zone represents a floating date.  Setting a date component without a hour, minute and second component will set allDay to YES.                If you set this property, the calendar must be set to NSCalendarIdentifierGregorian. An exception is raised otherwise.
--
-- ObjC selector: @- setStartDateComponents:@
setStartDateComponents :: (IsEKReminder ekReminder, IsNSDateComponents value) => ekReminder -> value -> IO ()
setStartDateComponents ekReminder  value =
withObjCPtr value $ \raw_value ->
    sendMsg ekReminder (mkSelector "setStartDateComponents:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

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
dueDateComponents ekReminder  =
  sendMsg ekReminder (mkSelector "dueDateComponents") (retPtr retVoid) [] >>= retainedObject . castPtr

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
setDueDateComponents ekReminder  value =
withObjCPtr value $ \raw_value ->
    sendMsg ekReminder (mkSelector "setDueDateComponents:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- | completed
--
-- Whether or not the reminder is completed.
--
-- Setting it to YES will set the completed date to the current date.                 Setting it to NO will set the completed date to nil.
--
-- ObjC selector: @- completed@
completed :: IsEKReminder ekReminder => ekReminder -> IO Bool
completed ekReminder  =
  fmap ((/= 0) :: CULong -> Bool) $ sendMsg ekReminder (mkSelector "completed") retCULong []

-- | completed
--
-- Whether or not the reminder is completed.
--
-- Setting it to YES will set the completed date to the current date.                 Setting it to NO will set the completed date to nil.
--
-- ObjC selector: @- setCompleted:@
setCompleted :: IsEKReminder ekReminder => ekReminder -> Bool -> IO ()
setCompleted ekReminder  value =
  sendMsg ekReminder (mkSelector "setCompleted:") retVoid [argCULong (if value then 1 else 0)]

-- | completionDate
--
-- The date on which this reminder was completed.
--
-- ObjC selector: @- completionDate@
completionDate :: IsEKReminder ekReminder => ekReminder -> IO (Id NSDate)
completionDate ekReminder  =
  sendMsg ekReminder (mkSelector "completionDate") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | completionDate
--
-- The date on which this reminder was completed.
--
-- ObjC selector: @- setCompletionDate:@
setCompletionDate :: (IsEKReminder ekReminder, IsNSDate value) => ekReminder -> value -> IO ()
setCompletionDate ekReminder  value =
withObjCPtr value $ \raw_value ->
    sendMsg ekReminder (mkSelector "setCompletionDate:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- | priority
--
-- The priority of the reminder.
--
-- Priorities run from 1 (highest) to 9 (lowest).  A priority of 0 means no priority.                Saving a reminder with any other priority will fail.                Per RFC 5545, priorities of 1-4 are considered "high," a priority of 5 is "medium," and priorities of 6-9 are "low."
--
-- ObjC selector: @- priority@
priority :: IsEKReminder ekReminder => ekReminder -> IO CULong
priority ekReminder  =
  sendMsg ekReminder (mkSelector "priority") retCULong []

-- | priority
--
-- The priority of the reminder.
--
-- Priorities run from 1 (highest) to 9 (lowest).  A priority of 0 means no priority.                Saving a reminder with any other priority will fail.                Per RFC 5545, priorities of 1-4 are considered "high," a priority of 5 is "medium," and priorities of 6-9 are "low."
--
-- ObjC selector: @- setPriority:@
setPriority :: IsEKReminder ekReminder => ekReminder -> CULong -> IO ()
setPriority ekReminder  value =
  sendMsg ekReminder (mkSelector "setPriority:") retVoid [argCULong (fromIntegral value)]

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @reminderWithEventStore:@
reminderWithEventStoreSelector :: Selector
reminderWithEventStoreSelector = mkSelector "reminderWithEventStore:"

-- | @Selector@ for @startDateComponents@
startDateComponentsSelector :: Selector
startDateComponentsSelector = mkSelector "startDateComponents"

-- | @Selector@ for @setStartDateComponents:@
setStartDateComponentsSelector :: Selector
setStartDateComponentsSelector = mkSelector "setStartDateComponents:"

-- | @Selector@ for @dueDateComponents@
dueDateComponentsSelector :: Selector
dueDateComponentsSelector = mkSelector "dueDateComponents"

-- | @Selector@ for @setDueDateComponents:@
setDueDateComponentsSelector :: Selector
setDueDateComponentsSelector = mkSelector "setDueDateComponents:"

-- | @Selector@ for @completed@
completedSelector :: Selector
completedSelector = mkSelector "completed"

-- | @Selector@ for @setCompleted:@
setCompletedSelector :: Selector
setCompletedSelector = mkSelector "setCompleted:"

-- | @Selector@ for @completionDate@
completionDateSelector :: Selector
completionDateSelector = mkSelector "completionDate"

-- | @Selector@ for @setCompletionDate:@
setCompletionDateSelector :: Selector
setCompletionDateSelector = mkSelector "setCompletionDate:"

-- | @Selector@ for @priority@
prioritySelector :: Selector
prioritySelector = mkSelector "priority"

-- | @Selector@ for @setPriority:@
setPrioritySelector :: Selector
setPrioritySelector = mkSelector "setPriority:"

