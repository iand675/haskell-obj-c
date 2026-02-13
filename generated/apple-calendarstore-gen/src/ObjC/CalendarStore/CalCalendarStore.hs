{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Generated bindings for @CalCalendarStore@.
module ObjC.CalendarStore.CalCalendarStore
  ( CalCalendarStore
  , IsCalCalendarStore(..)
  , defaultCalendarStore
  , calendars
  , calendarWithUID
  , saveCalendar_error
  , removeCalendar_error
  , eventsWithPredicate
  , eventWithUID_occurrence
  , tasksWithPredicate
  , taskWithUID
  , saveTask_error
  , removeTask_error
  , eventPredicateWithStartDate_endDate_calendars
  , eventPredicateWithStartDate_endDate_UID_calendars
  , taskPredicateWithCalendars
  , taskPredicateWithUncompletedTasks
  , taskPredicateWithUncompletedTasksDueBefore_calendars
  , taskPredicateWithTasksCompletedSince_calendars
  , calendarWithUIDSelector
  , calendarsSelector
  , defaultCalendarStoreSelector
  , eventPredicateWithStartDate_endDate_UID_calendarsSelector
  , eventPredicateWithStartDate_endDate_calendarsSelector
  , eventWithUID_occurrenceSelector
  , eventsWithPredicateSelector
  , removeCalendar_errorSelector
  , removeTask_errorSelector
  , saveCalendar_errorSelector
  , saveTask_errorSelector
  , taskPredicateWithCalendarsSelector
  , taskPredicateWithTasksCompletedSince_calendarsSelector
  , taskPredicateWithUncompletedTasksDueBefore_calendarsSelector
  , taskPredicateWithUncompletedTasksSelector
  , taskWithUIDSelector
  , tasksWithPredicateSelector


  ) where

import Foreign.Ptr (Ptr, FunPtr)
import Foreign.C.Types

import ObjC.Runtime.Types
import ObjC.Runtime.Message (sendMessage, sendOwnedMessage, sendClassMessage, sendOwnedClassMessage)
import ObjC.Runtime.Selector (mkSelector)
import ObjC.Runtime.Class (getRequiredClass)

import ObjC.CalendarStore.Internal.Classes
import ObjC.Foundation.Internal.Classes

-- | @+ defaultCalendarStore@
defaultCalendarStore :: IO (Id CalCalendarStore)
defaultCalendarStore  =
  do
    cls' <- getRequiredClass "CalCalendarStore"
    sendClassMessage cls' defaultCalendarStoreSelector

-- | @- calendars@
calendars :: IsCalCalendarStore calCalendarStore => calCalendarStore -> IO (Id NSArray)
calendars calCalendarStore =
  sendMessage calCalendarStore calendarsSelector

-- | @- calendarWithUID:@
calendarWithUID :: (IsCalCalendarStore calCalendarStore, IsNSString uid) => calCalendarStore -> uid -> IO (Id CalCalendar)
calendarWithUID calCalendarStore uid =
  sendMessage calCalendarStore calendarWithUIDSelector (toNSString uid)

-- | @- saveCalendar:error:@
saveCalendar_error :: (IsCalCalendarStore calCalendarStore, IsCalCalendar calendar, IsNSError error_) => calCalendarStore -> calendar -> error_ -> IO Bool
saveCalendar_error calCalendarStore calendar error_ =
  sendMessage calCalendarStore saveCalendar_errorSelector (toCalCalendar calendar) (toNSError error_)

-- | @- removeCalendar:error:@
removeCalendar_error :: (IsCalCalendarStore calCalendarStore, IsCalCalendar calendar, IsNSError error_) => calCalendarStore -> calendar -> error_ -> IO Bool
removeCalendar_error calCalendarStore calendar error_ =
  sendMessage calCalendarStore removeCalendar_errorSelector (toCalCalendar calendar) (toNSError error_)

-- | @- eventsWithPredicate:@
eventsWithPredicate :: (IsCalCalendarStore calCalendarStore, IsNSPredicate predicate) => calCalendarStore -> predicate -> IO (Id NSArray)
eventsWithPredicate calCalendarStore predicate =
  sendMessage calCalendarStore eventsWithPredicateSelector (toNSPredicate predicate)

-- | @- eventWithUID:occurrence:@
eventWithUID_occurrence :: (IsCalCalendarStore calCalendarStore, IsNSString uid, IsNSDate date) => calCalendarStore -> uid -> date -> IO (Id CalEvent)
eventWithUID_occurrence calCalendarStore uid date =
  sendMessage calCalendarStore eventWithUID_occurrenceSelector (toNSString uid) (toNSDate date)

-- | @- tasksWithPredicate:@
tasksWithPredicate :: (IsCalCalendarStore calCalendarStore, IsNSPredicate predicate) => calCalendarStore -> predicate -> IO (Id NSArray)
tasksWithPredicate calCalendarStore predicate =
  sendMessage calCalendarStore tasksWithPredicateSelector (toNSPredicate predicate)

-- | @- taskWithUID:@
taskWithUID :: (IsCalCalendarStore calCalendarStore, IsNSString uid) => calCalendarStore -> uid -> IO (Id CalTask)
taskWithUID calCalendarStore uid =
  sendMessage calCalendarStore taskWithUIDSelector (toNSString uid)

-- | @- saveTask:error:@
saveTask_error :: (IsCalCalendarStore calCalendarStore, IsCalTask task, IsNSError error_) => calCalendarStore -> task -> error_ -> IO Bool
saveTask_error calCalendarStore task error_ =
  sendMessage calCalendarStore saveTask_errorSelector (toCalTask task) (toNSError error_)

-- | @- removeTask:error:@
removeTask_error :: (IsCalCalendarStore calCalendarStore, IsCalTask task, IsNSError error_) => calCalendarStore -> task -> error_ -> IO Bool
removeTask_error calCalendarStore task error_ =
  sendMessage calCalendarStore removeTask_errorSelector (toCalTask task) (toNSError error_)

-- | @+ eventPredicateWithStartDate:endDate:calendars:@
eventPredicateWithStartDate_endDate_calendars :: (IsNSDate startDate, IsNSDate endDate, IsNSArray calendars) => startDate -> endDate -> calendars -> IO (Id NSPredicate)
eventPredicateWithStartDate_endDate_calendars startDate endDate calendars =
  do
    cls' <- getRequiredClass "CalCalendarStore"
    sendClassMessage cls' eventPredicateWithStartDate_endDate_calendarsSelector (toNSDate startDate) (toNSDate endDate) (toNSArray calendars)

-- | @+ eventPredicateWithStartDate:endDate:UID:calendars:@
eventPredicateWithStartDate_endDate_UID_calendars :: (IsNSDate startDate, IsNSDate endDate, IsNSString uid, IsNSArray calendars) => startDate -> endDate -> uid -> calendars -> IO (Id NSPredicate)
eventPredicateWithStartDate_endDate_UID_calendars startDate endDate uid calendars =
  do
    cls' <- getRequiredClass "CalCalendarStore"
    sendClassMessage cls' eventPredicateWithStartDate_endDate_UID_calendarsSelector (toNSDate startDate) (toNSDate endDate) (toNSString uid) (toNSArray calendars)

-- | @+ taskPredicateWithCalendars:@
taskPredicateWithCalendars :: IsNSArray calendars => calendars -> IO (Id NSPredicate)
taskPredicateWithCalendars calendars =
  do
    cls' <- getRequiredClass "CalCalendarStore"
    sendClassMessage cls' taskPredicateWithCalendarsSelector (toNSArray calendars)

-- | @+ taskPredicateWithUncompletedTasks:@
taskPredicateWithUncompletedTasks :: IsNSArray calendars => calendars -> IO (Id NSPredicate)
taskPredicateWithUncompletedTasks calendars =
  do
    cls' <- getRequiredClass "CalCalendarStore"
    sendClassMessage cls' taskPredicateWithUncompletedTasksSelector (toNSArray calendars)

-- | @+ taskPredicateWithUncompletedTasksDueBefore:calendars:@
taskPredicateWithUncompletedTasksDueBefore_calendars :: (IsNSDate dueDate, IsNSArray calendars) => dueDate -> calendars -> IO (Id NSPredicate)
taskPredicateWithUncompletedTasksDueBefore_calendars dueDate calendars =
  do
    cls' <- getRequiredClass "CalCalendarStore"
    sendClassMessage cls' taskPredicateWithUncompletedTasksDueBefore_calendarsSelector (toNSDate dueDate) (toNSArray calendars)

-- | @+ taskPredicateWithTasksCompletedSince:calendars:@
taskPredicateWithTasksCompletedSince_calendars :: (IsNSDate completedSince, IsNSArray calendars) => completedSince -> calendars -> IO (Id NSPredicate)
taskPredicateWithTasksCompletedSince_calendars completedSince calendars =
  do
    cls' <- getRequiredClass "CalCalendarStore"
    sendClassMessage cls' taskPredicateWithTasksCompletedSince_calendarsSelector (toNSDate completedSince) (toNSArray calendars)

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @defaultCalendarStore@
defaultCalendarStoreSelector :: Selector '[] (Id CalCalendarStore)
defaultCalendarStoreSelector = mkSelector "defaultCalendarStore"

-- | @Selector@ for @calendars@
calendarsSelector :: Selector '[] (Id NSArray)
calendarsSelector = mkSelector "calendars"

-- | @Selector@ for @calendarWithUID:@
calendarWithUIDSelector :: Selector '[Id NSString] (Id CalCalendar)
calendarWithUIDSelector = mkSelector "calendarWithUID:"

-- | @Selector@ for @saveCalendar:error:@
saveCalendar_errorSelector :: Selector '[Id CalCalendar, Id NSError] Bool
saveCalendar_errorSelector = mkSelector "saveCalendar:error:"

-- | @Selector@ for @removeCalendar:error:@
removeCalendar_errorSelector :: Selector '[Id CalCalendar, Id NSError] Bool
removeCalendar_errorSelector = mkSelector "removeCalendar:error:"

-- | @Selector@ for @eventsWithPredicate:@
eventsWithPredicateSelector :: Selector '[Id NSPredicate] (Id NSArray)
eventsWithPredicateSelector = mkSelector "eventsWithPredicate:"

-- | @Selector@ for @eventWithUID:occurrence:@
eventWithUID_occurrenceSelector :: Selector '[Id NSString, Id NSDate] (Id CalEvent)
eventWithUID_occurrenceSelector = mkSelector "eventWithUID:occurrence:"

-- | @Selector@ for @tasksWithPredicate:@
tasksWithPredicateSelector :: Selector '[Id NSPredicate] (Id NSArray)
tasksWithPredicateSelector = mkSelector "tasksWithPredicate:"

-- | @Selector@ for @taskWithUID:@
taskWithUIDSelector :: Selector '[Id NSString] (Id CalTask)
taskWithUIDSelector = mkSelector "taskWithUID:"

-- | @Selector@ for @saveTask:error:@
saveTask_errorSelector :: Selector '[Id CalTask, Id NSError] Bool
saveTask_errorSelector = mkSelector "saveTask:error:"

-- | @Selector@ for @removeTask:error:@
removeTask_errorSelector :: Selector '[Id CalTask, Id NSError] Bool
removeTask_errorSelector = mkSelector "removeTask:error:"

-- | @Selector@ for @eventPredicateWithStartDate:endDate:calendars:@
eventPredicateWithStartDate_endDate_calendarsSelector :: Selector '[Id NSDate, Id NSDate, Id NSArray] (Id NSPredicate)
eventPredicateWithStartDate_endDate_calendarsSelector = mkSelector "eventPredicateWithStartDate:endDate:calendars:"

-- | @Selector@ for @eventPredicateWithStartDate:endDate:UID:calendars:@
eventPredicateWithStartDate_endDate_UID_calendarsSelector :: Selector '[Id NSDate, Id NSDate, Id NSString, Id NSArray] (Id NSPredicate)
eventPredicateWithStartDate_endDate_UID_calendarsSelector = mkSelector "eventPredicateWithStartDate:endDate:UID:calendars:"

-- | @Selector@ for @taskPredicateWithCalendars:@
taskPredicateWithCalendarsSelector :: Selector '[Id NSArray] (Id NSPredicate)
taskPredicateWithCalendarsSelector = mkSelector "taskPredicateWithCalendars:"

-- | @Selector@ for @taskPredicateWithUncompletedTasks:@
taskPredicateWithUncompletedTasksSelector :: Selector '[Id NSArray] (Id NSPredicate)
taskPredicateWithUncompletedTasksSelector = mkSelector "taskPredicateWithUncompletedTasks:"

-- | @Selector@ for @taskPredicateWithUncompletedTasksDueBefore:calendars:@
taskPredicateWithUncompletedTasksDueBefore_calendarsSelector :: Selector '[Id NSDate, Id NSArray] (Id NSPredicate)
taskPredicateWithUncompletedTasksDueBefore_calendarsSelector = mkSelector "taskPredicateWithUncompletedTasksDueBefore:calendars:"

-- | @Selector@ for @taskPredicateWithTasksCompletedSince:calendars:@
taskPredicateWithTasksCompletedSince_calendarsSelector :: Selector '[Id NSDate, Id NSArray] (Id NSPredicate)
taskPredicateWithTasksCompletedSince_calendarsSelector = mkSelector "taskPredicateWithTasksCompletedSince:calendars:"

