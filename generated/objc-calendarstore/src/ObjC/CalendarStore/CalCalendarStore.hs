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
  , defaultCalendarStoreSelector
  , calendarsSelector
  , calendarWithUIDSelector
  , saveCalendar_errorSelector
  , removeCalendar_errorSelector
  , eventsWithPredicateSelector
  , eventWithUID_occurrenceSelector
  , tasksWithPredicateSelector
  , taskWithUIDSelector
  , saveTask_errorSelector
  , removeTask_errorSelector
  , eventPredicateWithStartDate_endDate_calendarsSelector
  , eventPredicateWithStartDate_endDate_UID_calendarsSelector
  , taskPredicateWithCalendarsSelector
  , taskPredicateWithUncompletedTasksSelector
  , taskPredicateWithUncompletedTasksDueBefore_calendarsSelector
  , taskPredicateWithTasksCompletedSince_calendarsSelector


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

import ObjC.CalendarStore.Internal.Classes
import ObjC.Foundation.Internal.Classes

-- | @+ defaultCalendarStore@
defaultCalendarStore :: IO (Id CalCalendarStore)
defaultCalendarStore  =
  do
    cls' <- getRequiredClass "CalCalendarStore"
    sendClassMsg cls' (mkSelector "defaultCalendarStore") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- calendars@
calendars :: IsCalCalendarStore calCalendarStore => calCalendarStore -> IO (Id NSArray)
calendars calCalendarStore  =
  sendMsg calCalendarStore (mkSelector "calendars") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- calendarWithUID:@
calendarWithUID :: (IsCalCalendarStore calCalendarStore, IsNSString uid) => calCalendarStore -> uid -> IO (Id CalCalendar)
calendarWithUID calCalendarStore  uid =
withObjCPtr uid $ \raw_uid ->
    sendMsg calCalendarStore (mkSelector "calendarWithUID:") (retPtr retVoid) [argPtr (castPtr raw_uid :: Ptr ())] >>= retainedObject . castPtr

-- | @- saveCalendar:error:@
saveCalendar_error :: (IsCalCalendarStore calCalendarStore, IsCalCalendar calendar, IsNSError error_) => calCalendarStore -> calendar -> error_ -> IO Bool
saveCalendar_error calCalendarStore  calendar error_ =
withObjCPtr calendar $ \raw_calendar ->
  withObjCPtr error_ $ \raw_error_ ->
      fmap ((/= 0) :: CULong -> Bool) $ sendMsg calCalendarStore (mkSelector "saveCalendar:error:") retCULong [argPtr (castPtr raw_calendar :: Ptr ()), argPtr (castPtr raw_error_ :: Ptr ())]

-- | @- removeCalendar:error:@
removeCalendar_error :: (IsCalCalendarStore calCalendarStore, IsCalCalendar calendar, IsNSError error_) => calCalendarStore -> calendar -> error_ -> IO Bool
removeCalendar_error calCalendarStore  calendar error_ =
withObjCPtr calendar $ \raw_calendar ->
  withObjCPtr error_ $ \raw_error_ ->
      fmap ((/= 0) :: CULong -> Bool) $ sendMsg calCalendarStore (mkSelector "removeCalendar:error:") retCULong [argPtr (castPtr raw_calendar :: Ptr ()), argPtr (castPtr raw_error_ :: Ptr ())]

-- | @- eventsWithPredicate:@
eventsWithPredicate :: (IsCalCalendarStore calCalendarStore, IsNSPredicate predicate) => calCalendarStore -> predicate -> IO (Id NSArray)
eventsWithPredicate calCalendarStore  predicate =
withObjCPtr predicate $ \raw_predicate ->
    sendMsg calCalendarStore (mkSelector "eventsWithPredicate:") (retPtr retVoid) [argPtr (castPtr raw_predicate :: Ptr ())] >>= retainedObject . castPtr

-- | @- eventWithUID:occurrence:@
eventWithUID_occurrence :: (IsCalCalendarStore calCalendarStore, IsNSString uid, IsNSDate date) => calCalendarStore -> uid -> date -> IO (Id CalEvent)
eventWithUID_occurrence calCalendarStore  uid date =
withObjCPtr uid $ \raw_uid ->
  withObjCPtr date $ \raw_date ->
      sendMsg calCalendarStore (mkSelector "eventWithUID:occurrence:") (retPtr retVoid) [argPtr (castPtr raw_uid :: Ptr ()), argPtr (castPtr raw_date :: Ptr ())] >>= retainedObject . castPtr

-- | @- tasksWithPredicate:@
tasksWithPredicate :: (IsCalCalendarStore calCalendarStore, IsNSPredicate predicate) => calCalendarStore -> predicate -> IO (Id NSArray)
tasksWithPredicate calCalendarStore  predicate =
withObjCPtr predicate $ \raw_predicate ->
    sendMsg calCalendarStore (mkSelector "tasksWithPredicate:") (retPtr retVoid) [argPtr (castPtr raw_predicate :: Ptr ())] >>= retainedObject . castPtr

-- | @- taskWithUID:@
taskWithUID :: (IsCalCalendarStore calCalendarStore, IsNSString uid) => calCalendarStore -> uid -> IO (Id CalTask)
taskWithUID calCalendarStore  uid =
withObjCPtr uid $ \raw_uid ->
    sendMsg calCalendarStore (mkSelector "taskWithUID:") (retPtr retVoid) [argPtr (castPtr raw_uid :: Ptr ())] >>= retainedObject . castPtr

-- | @- saveTask:error:@
saveTask_error :: (IsCalCalendarStore calCalendarStore, IsCalTask task, IsNSError error_) => calCalendarStore -> task -> error_ -> IO Bool
saveTask_error calCalendarStore  task error_ =
withObjCPtr task $ \raw_task ->
  withObjCPtr error_ $ \raw_error_ ->
      fmap ((/= 0) :: CULong -> Bool) $ sendMsg calCalendarStore (mkSelector "saveTask:error:") retCULong [argPtr (castPtr raw_task :: Ptr ()), argPtr (castPtr raw_error_ :: Ptr ())]

-- | @- removeTask:error:@
removeTask_error :: (IsCalCalendarStore calCalendarStore, IsCalTask task, IsNSError error_) => calCalendarStore -> task -> error_ -> IO Bool
removeTask_error calCalendarStore  task error_ =
withObjCPtr task $ \raw_task ->
  withObjCPtr error_ $ \raw_error_ ->
      fmap ((/= 0) :: CULong -> Bool) $ sendMsg calCalendarStore (mkSelector "removeTask:error:") retCULong [argPtr (castPtr raw_task :: Ptr ()), argPtr (castPtr raw_error_ :: Ptr ())]

-- | @+ eventPredicateWithStartDate:endDate:calendars:@
eventPredicateWithStartDate_endDate_calendars :: (IsNSDate startDate, IsNSDate endDate, IsNSArray calendars) => startDate -> endDate -> calendars -> IO (Id NSPredicate)
eventPredicateWithStartDate_endDate_calendars startDate endDate calendars =
  do
    cls' <- getRequiredClass "CalCalendarStore"
    withObjCPtr startDate $ \raw_startDate ->
      withObjCPtr endDate $ \raw_endDate ->
        withObjCPtr calendars $ \raw_calendars ->
          sendClassMsg cls' (mkSelector "eventPredicateWithStartDate:endDate:calendars:") (retPtr retVoid) [argPtr (castPtr raw_startDate :: Ptr ()), argPtr (castPtr raw_endDate :: Ptr ()), argPtr (castPtr raw_calendars :: Ptr ())] >>= retainedObject . castPtr

-- | @+ eventPredicateWithStartDate:endDate:UID:calendars:@
eventPredicateWithStartDate_endDate_UID_calendars :: (IsNSDate startDate, IsNSDate endDate, IsNSString uid, IsNSArray calendars) => startDate -> endDate -> uid -> calendars -> IO (Id NSPredicate)
eventPredicateWithStartDate_endDate_UID_calendars startDate endDate uid calendars =
  do
    cls' <- getRequiredClass "CalCalendarStore"
    withObjCPtr startDate $ \raw_startDate ->
      withObjCPtr endDate $ \raw_endDate ->
        withObjCPtr uid $ \raw_uid ->
          withObjCPtr calendars $ \raw_calendars ->
            sendClassMsg cls' (mkSelector "eventPredicateWithStartDate:endDate:UID:calendars:") (retPtr retVoid) [argPtr (castPtr raw_startDate :: Ptr ()), argPtr (castPtr raw_endDate :: Ptr ()), argPtr (castPtr raw_uid :: Ptr ()), argPtr (castPtr raw_calendars :: Ptr ())] >>= retainedObject . castPtr

-- | @+ taskPredicateWithCalendars:@
taskPredicateWithCalendars :: IsNSArray calendars => calendars -> IO (Id NSPredicate)
taskPredicateWithCalendars calendars =
  do
    cls' <- getRequiredClass "CalCalendarStore"
    withObjCPtr calendars $ \raw_calendars ->
      sendClassMsg cls' (mkSelector "taskPredicateWithCalendars:") (retPtr retVoid) [argPtr (castPtr raw_calendars :: Ptr ())] >>= retainedObject . castPtr

-- | @+ taskPredicateWithUncompletedTasks:@
taskPredicateWithUncompletedTasks :: IsNSArray calendars => calendars -> IO (Id NSPredicate)
taskPredicateWithUncompletedTasks calendars =
  do
    cls' <- getRequiredClass "CalCalendarStore"
    withObjCPtr calendars $ \raw_calendars ->
      sendClassMsg cls' (mkSelector "taskPredicateWithUncompletedTasks:") (retPtr retVoid) [argPtr (castPtr raw_calendars :: Ptr ())] >>= retainedObject . castPtr

-- | @+ taskPredicateWithUncompletedTasksDueBefore:calendars:@
taskPredicateWithUncompletedTasksDueBefore_calendars :: (IsNSDate dueDate, IsNSArray calendars) => dueDate -> calendars -> IO (Id NSPredicate)
taskPredicateWithUncompletedTasksDueBefore_calendars dueDate calendars =
  do
    cls' <- getRequiredClass "CalCalendarStore"
    withObjCPtr dueDate $ \raw_dueDate ->
      withObjCPtr calendars $ \raw_calendars ->
        sendClassMsg cls' (mkSelector "taskPredicateWithUncompletedTasksDueBefore:calendars:") (retPtr retVoid) [argPtr (castPtr raw_dueDate :: Ptr ()), argPtr (castPtr raw_calendars :: Ptr ())] >>= retainedObject . castPtr

-- | @+ taskPredicateWithTasksCompletedSince:calendars:@
taskPredicateWithTasksCompletedSince_calendars :: (IsNSDate completedSince, IsNSArray calendars) => completedSince -> calendars -> IO (Id NSPredicate)
taskPredicateWithTasksCompletedSince_calendars completedSince calendars =
  do
    cls' <- getRequiredClass "CalCalendarStore"
    withObjCPtr completedSince $ \raw_completedSince ->
      withObjCPtr calendars $ \raw_calendars ->
        sendClassMsg cls' (mkSelector "taskPredicateWithTasksCompletedSince:calendars:") (retPtr retVoid) [argPtr (castPtr raw_completedSince :: Ptr ()), argPtr (castPtr raw_calendars :: Ptr ())] >>= retainedObject . castPtr

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @defaultCalendarStore@
defaultCalendarStoreSelector :: Selector
defaultCalendarStoreSelector = mkSelector "defaultCalendarStore"

-- | @Selector@ for @calendars@
calendarsSelector :: Selector
calendarsSelector = mkSelector "calendars"

-- | @Selector@ for @calendarWithUID:@
calendarWithUIDSelector :: Selector
calendarWithUIDSelector = mkSelector "calendarWithUID:"

-- | @Selector@ for @saveCalendar:error:@
saveCalendar_errorSelector :: Selector
saveCalendar_errorSelector = mkSelector "saveCalendar:error:"

-- | @Selector@ for @removeCalendar:error:@
removeCalendar_errorSelector :: Selector
removeCalendar_errorSelector = mkSelector "removeCalendar:error:"

-- | @Selector@ for @eventsWithPredicate:@
eventsWithPredicateSelector :: Selector
eventsWithPredicateSelector = mkSelector "eventsWithPredicate:"

-- | @Selector@ for @eventWithUID:occurrence:@
eventWithUID_occurrenceSelector :: Selector
eventWithUID_occurrenceSelector = mkSelector "eventWithUID:occurrence:"

-- | @Selector@ for @tasksWithPredicate:@
tasksWithPredicateSelector :: Selector
tasksWithPredicateSelector = mkSelector "tasksWithPredicate:"

-- | @Selector@ for @taskWithUID:@
taskWithUIDSelector :: Selector
taskWithUIDSelector = mkSelector "taskWithUID:"

-- | @Selector@ for @saveTask:error:@
saveTask_errorSelector :: Selector
saveTask_errorSelector = mkSelector "saveTask:error:"

-- | @Selector@ for @removeTask:error:@
removeTask_errorSelector :: Selector
removeTask_errorSelector = mkSelector "removeTask:error:"

-- | @Selector@ for @eventPredicateWithStartDate:endDate:calendars:@
eventPredicateWithStartDate_endDate_calendarsSelector :: Selector
eventPredicateWithStartDate_endDate_calendarsSelector = mkSelector "eventPredicateWithStartDate:endDate:calendars:"

-- | @Selector@ for @eventPredicateWithStartDate:endDate:UID:calendars:@
eventPredicateWithStartDate_endDate_UID_calendarsSelector :: Selector
eventPredicateWithStartDate_endDate_UID_calendarsSelector = mkSelector "eventPredicateWithStartDate:endDate:UID:calendars:"

-- | @Selector@ for @taskPredicateWithCalendars:@
taskPredicateWithCalendarsSelector :: Selector
taskPredicateWithCalendarsSelector = mkSelector "taskPredicateWithCalendars:"

-- | @Selector@ for @taskPredicateWithUncompletedTasks:@
taskPredicateWithUncompletedTasksSelector :: Selector
taskPredicateWithUncompletedTasksSelector = mkSelector "taskPredicateWithUncompletedTasks:"

-- | @Selector@ for @taskPredicateWithUncompletedTasksDueBefore:calendars:@
taskPredicateWithUncompletedTasksDueBefore_calendarsSelector :: Selector
taskPredicateWithUncompletedTasksDueBefore_calendarsSelector = mkSelector "taskPredicateWithUncompletedTasksDueBefore:calendars:"

-- | @Selector@ for @taskPredicateWithTasksCompletedSince:calendars:@
taskPredicateWithTasksCompletedSince_calendarsSelector :: Selector
taskPredicateWithTasksCompletedSince_calendarsSelector = mkSelector "taskPredicateWithTasksCompletedSince:calendars:"

