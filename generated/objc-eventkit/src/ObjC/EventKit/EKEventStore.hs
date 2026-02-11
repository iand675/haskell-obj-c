{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | EKEventStore
--
-- The EKEventStore class provides an interface for accessing and manipulating calendar events and reminders.
--
-- The EKEventStore class is the main point of contact for accessing Calendar data. You must                 create a EKEventStore object in order to retrieve/add/delete events or reminders from the Calendar database.
--
-- Events, Reminders, and Calendar objects retrieved from an event store cannot be used with any other event                 store. It is generally best to hold onto a long-lived instance of an event store, most                 likely as a singleton instance in your application.
--
-- Generated bindings for @EKEventStore@.
module ObjC.EventKit.EKEventStore
  ( EKEventStore
  , IsEKEventStore(..)
  , authorizationStatusForEntityType
  , initWithAccessToEntityTypes
  , init_
  , initWithSources
  , requestFullAccessToEventsWithCompletion
  , requestWriteOnlyAccessToEventsWithCompletion
  , requestFullAccessToRemindersWithCompletion
  , requestAccessToEntityType_completion
  , sourceWithIdentifier
  , calendarsForEntityType
  , defaultCalendarForNewReminders
  , calendarWithIdentifier
  , saveCalendar_commit_error
  , removeCalendar_commit_error
  , calendarItemWithIdentifier
  , calendarItemsWithExternalIdentifier
  , saveEvent_span_error
  , removeEvent_span_error
  , saveEvent_span_commit_error
  , removeEvent_span_commit_error
  , eventWithIdentifier
  , eventsMatchingPredicate
  , enumerateEventsMatchingPredicate_usingBlock
  , predicateForEventsWithStartDate_endDate_calendars
  , saveReminder_commit_error
  , removeReminder_commit_error
  , cancelFetchRequest
  , predicateForRemindersInCalendars
  , predicateForIncompleteRemindersWithDueDateStarting_ending_calendars
  , predicateForCompletedRemindersWithCompletionDateStarting_ending_calendars
  , commit
  , reset
  , refreshSourcesIfNecessary
  , eventStoreIdentifier
  , defaultCalendarForNewEvents
  , authorizationStatusForEntityTypeSelector
  , initWithAccessToEntityTypesSelector
  , initSelector
  , initWithSourcesSelector
  , requestFullAccessToEventsWithCompletionSelector
  , requestWriteOnlyAccessToEventsWithCompletionSelector
  , requestFullAccessToRemindersWithCompletionSelector
  , requestAccessToEntityType_completionSelector
  , sourceWithIdentifierSelector
  , calendarsForEntityTypeSelector
  , defaultCalendarForNewRemindersSelector
  , calendarWithIdentifierSelector
  , saveCalendar_commit_errorSelector
  , removeCalendar_commit_errorSelector
  , calendarItemWithIdentifierSelector
  , calendarItemsWithExternalIdentifierSelector
  , saveEvent_span_errorSelector
  , removeEvent_span_errorSelector
  , saveEvent_span_commit_errorSelector
  , removeEvent_span_commit_errorSelector
  , eventWithIdentifierSelector
  , eventsMatchingPredicateSelector
  , enumerateEventsMatchingPredicate_usingBlockSelector
  , predicateForEventsWithStartDate_endDate_calendarsSelector
  , saveReminder_commit_errorSelector
  , removeReminder_commit_errorSelector
  , cancelFetchRequestSelector
  , predicateForRemindersInCalendarsSelector
  , predicateForIncompleteRemindersWithDueDateStarting_ending_calendarsSelector
  , predicateForCompletedRemindersWithCompletionDateStarting_ending_calendarsSelector
  , commitSelector
  , resetSelector
  , refreshSourcesIfNecessarySelector
  , eventStoreIdentifierSelector
  , defaultCalendarForNewEventsSelector

  -- * Enum types
  , EKAuthorizationStatus(EKAuthorizationStatus)
  , pattern EKAuthorizationStatusNotDetermined
  , pattern EKAuthorizationStatusRestricted
  , pattern EKAuthorizationStatusDenied
  , pattern EKAuthorizationStatusFullAccess
  , pattern EKAuthorizationStatusWriteOnly
  , pattern EKAuthorizationStatusAuthorized
  , EKEntityMask(EKEntityMask)
  , pattern EKEntityMaskEvent
  , pattern EKEntityMaskReminder
  , EKEntityType(EKEntityType)
  , pattern EKEntityTypeEvent
  , pattern EKEntityTypeReminder
  , EKSpan(EKSpan)
  , pattern EKSpanThisEvent
  , pattern EKSpanFutureEvents

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
import ObjC.EventKit.Internal.Enums
import ObjC.Foundation.Internal.Classes

-- | authorizationStatusForEntityType:
--
-- Returns the authorization status for the given entity type
--
-- ObjC selector: @+ authorizationStatusForEntityType:@
authorizationStatusForEntityType :: EKEntityType -> IO EKAuthorizationStatus
authorizationStatusForEntityType entityType =
  do
    cls' <- getRequiredClass "EKEventStore"
    fmap (coerce :: CLong -> EKAuthorizationStatus) $ sendClassMsg cls' (mkSelector "authorizationStatusForEntityType:") retCLong [argCULong (coerce entityType)]

-- | initWithAccessToEntityTypes:
--
-- Users are able to grant or deny access to event and reminder data on a per-app basis. To request access to                event and/or reminder data, instantiate an EKEventStore using this method. This call will not block the                program while the user is being asked to grant or deny access. Until access has been granted for an entity                type, this event store will not contain any calendars for that entity type, and any attempt to save entities                of that entity type will fail. If access is later granted or declined, the event store will broadcast an                EKEventStoreChangedNotification. You can check the current access status for an entity type                using +authorizationStatusForEntityType:. The user will only be prompted the first time access is requested; any                subsequent instantiations of EKEventStore will use the existing permissions.
--
-- @entityTypes@ — A bit mask of entity types to which you want access
--
-- ObjC selector: @- initWithAccessToEntityTypes:@
initWithAccessToEntityTypes :: IsEKEventStore ekEventStore => ekEventStore -> EKEntityMask -> IO RawId
initWithAccessToEntityTypes ekEventStore  entityTypes =
  fmap (RawId . castPtr) $ sendMsg ekEventStore (mkSelector "initWithAccessToEntityTypes:") (retPtr retVoid) [argCULong (coerce entityTypes)]

-- | init
--
-- ObjC selector: @- init@
init_ :: IsEKEventStore ekEventStore => ekEventStore -> IO RawId
init_ ekEventStore  =
  fmap (RawId . castPtr) $ sendMsg ekEventStore (mkSelector "init") (retPtr retVoid) []

-- | initWithSources:
--
-- Creates a new event store that only includes items and calendars for a subset of sources.
--
-- @sources@ — The sources you want this event store to recognize. This may include delegate sources.
--
-- ObjC selector: @- initWithSources:@
initWithSources :: (IsEKEventStore ekEventStore, IsNSArray sources) => ekEventStore -> sources -> IO (Id EKEventStore)
initWithSources ekEventStore  sources =
withObjCPtr sources $ \raw_sources ->
    sendMsg ekEventStore (mkSelector "initWithSources:") (retPtr retVoid) [argPtr (castPtr raw_sources :: Ptr ())] >>= ownedObject . castPtr

-- | @- requestFullAccessToEventsWithCompletion:@
requestFullAccessToEventsWithCompletion :: IsEKEventStore ekEventStore => ekEventStore -> Ptr () -> IO ()
requestFullAccessToEventsWithCompletion ekEventStore  completion =
  sendMsg ekEventStore (mkSelector "requestFullAccessToEventsWithCompletion:") retVoid [argPtr (castPtr completion :: Ptr ())]

-- | @- requestWriteOnlyAccessToEventsWithCompletion:@
requestWriteOnlyAccessToEventsWithCompletion :: IsEKEventStore ekEventStore => ekEventStore -> Ptr () -> IO ()
requestWriteOnlyAccessToEventsWithCompletion ekEventStore  completion =
  sendMsg ekEventStore (mkSelector "requestWriteOnlyAccessToEventsWithCompletion:") retVoid [argPtr (castPtr completion :: Ptr ())]

-- | @- requestFullAccessToRemindersWithCompletion:@
requestFullAccessToRemindersWithCompletion :: IsEKEventStore ekEventStore => ekEventStore -> Ptr () -> IO ()
requestFullAccessToRemindersWithCompletion ekEventStore  completion =
  sendMsg ekEventStore (mkSelector "requestFullAccessToRemindersWithCompletion:") retVoid [argPtr (castPtr completion :: Ptr ())]

-- | @- requestAccessToEntityType:completion:@
requestAccessToEntityType_completion :: IsEKEventStore ekEventStore => ekEventStore -> EKEntityType -> Ptr () -> IO ()
requestAccessToEntityType_completion ekEventStore  entityType completion =
  sendMsg ekEventStore (mkSelector "requestAccessToEntityType:completion:") retVoid [argCULong (coerce entityType), argPtr (castPtr completion :: Ptr ())]

-- | sourceWithIdentifier:
--
-- Returns a source with a specified identifier.
--
-- ObjC selector: @- sourceWithIdentifier:@
sourceWithIdentifier :: (IsEKEventStore ekEventStore, IsNSString identifier) => ekEventStore -> identifier -> IO (Id EKSource)
sourceWithIdentifier ekEventStore  identifier =
withObjCPtr identifier $ \raw_identifier ->
    sendMsg ekEventStore (mkSelector "sourceWithIdentifier:") (retPtr retVoid) [argPtr (castPtr raw_identifier :: Ptr ())] >>= retainedObject . castPtr

-- | calendarsForEntityType
--
-- Returns calendars that support a given entity type (reminders, events)
--
-- ObjC selector: @- calendarsForEntityType:@
calendarsForEntityType :: IsEKEventStore ekEventStore => ekEventStore -> EKEntityType -> IO (Id NSArray)
calendarsForEntityType ekEventStore  entityType =
  sendMsg ekEventStore (mkSelector "calendarsForEntityType:") (retPtr retVoid) [argCULong (coerce entityType)] >>= retainedObject . castPtr

-- | defaultCalendarForNewReminders
--
-- Returns the calendar that reminders should be added to by default.
--
-- This may be nil if there is no default calendar for new reminders.
--
-- ObjC selector: @- defaultCalendarForNewReminders@
defaultCalendarForNewReminders :: IsEKEventStore ekEventStore => ekEventStore -> IO (Id EKCalendar)
defaultCalendarForNewReminders ekEventStore  =
  sendMsg ekEventStore (mkSelector "defaultCalendarForNewReminders") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | calendarWithIdentifier:
--
-- Returns a calendar with a specified identifier.
--
-- ObjC selector: @- calendarWithIdentifier:@
calendarWithIdentifier :: (IsEKEventStore ekEventStore, IsNSString identifier) => ekEventStore -> identifier -> IO (Id EKCalendar)
calendarWithIdentifier ekEventStore  identifier =
withObjCPtr identifier $ \raw_identifier ->
    sendMsg ekEventStore (mkSelector "calendarWithIdentifier:") (retPtr retVoid) [argPtr (castPtr raw_identifier :: Ptr ())] >>= retainedObject . castPtr

-- | saveCalendar:commit:error:
--
-- Saves changes to a calendar, or adds a new calendar to the database.
--
-- This method attempts to save the given calendar to the calendar database. It                returns YES if successful and NO otherwise. Passing a calendar fetched from                another EKEventStore instance into this function will raise an exception.                On WatchOS, saving changes is not supported.
--
-- @calendar@ — The calendar to save.
--
-- @commit@ — Pass YES to cause the database to save. You can pass NO to save multiple                            calendars and then call commit: to save them all at once.
--
-- @error@ — If an error occurs, this will contain a valid NSError object on exit.
--
-- ObjC selector: @- saveCalendar:commit:error:@
saveCalendar_commit_error :: (IsEKEventStore ekEventStore, IsEKCalendar calendar, IsNSError error_) => ekEventStore -> calendar -> Bool -> error_ -> IO Bool
saveCalendar_commit_error ekEventStore  calendar commit error_ =
withObjCPtr calendar $ \raw_calendar ->
  withObjCPtr error_ $ \raw_error_ ->
      fmap ((/= 0) :: CULong -> Bool) $ sendMsg ekEventStore (mkSelector "saveCalendar:commit:error:") retCULong [argPtr (castPtr raw_calendar :: Ptr ()), argCULong (if commit then 1 else 0), argPtr (castPtr raw_error_ :: Ptr ())]

-- | removeCalendar:commit:error:
--
-- Removes a calendar from the database.
--
-- This method attempts to delete the given calendar from the calendar database. It                returns YES if successful and NO otherwise. Passing a calendar fetched from                another EKEventStore instance into this function will raise an exception.
--
-- If the calendar supports multiple entity types (allowedEntityTypes), but the user has                 not granted you access to all those entity types, then we will delete all of the entity types                 for which you have access and remove that entity type from the allowedEntityTypes.                For example: If a calendar supports both events and reminders, but you only have access to reminders,                we will delete all the reminders and make the calendar only support events.
--
-- If you have access to all of its allowedEntityTypes, then it will delete the calendar and                all of the events and reminders in the calendar.
--
-- On WatchOS, modifying the database is not supported.
--
-- @calendar@ — The calendar to delete.
--
-- @commit@ — Pass YES to cause the database to save. You can pass NO to batch multiple                            changes and then call commit: to save them all at once.
--
-- @error@ — If an error occurs, this will contain a valid NSError object on exit.
--
-- ObjC selector: @- removeCalendar:commit:error:@
removeCalendar_commit_error :: (IsEKEventStore ekEventStore, IsEKCalendar calendar, IsNSError error_) => ekEventStore -> calendar -> Bool -> error_ -> IO Bool
removeCalendar_commit_error ekEventStore  calendar commit error_ =
withObjCPtr calendar $ \raw_calendar ->
  withObjCPtr error_ $ \raw_error_ ->
      fmap ((/= 0) :: CULong -> Bool) $ sendMsg ekEventStore (mkSelector "removeCalendar:commit:error:") retCULong [argPtr (castPtr raw_calendar :: Ptr ()), argCULong (if commit then 1 else 0), argPtr (castPtr raw_error_ :: Ptr ())]

-- | calendarItemWithIdentifier:
--
-- Returns either a reminder or the first occurrence of an event.
--
-- ObjC selector: @- calendarItemWithIdentifier:@
calendarItemWithIdentifier :: (IsEKEventStore ekEventStore, IsNSString identifier) => ekEventStore -> identifier -> IO (Id EKCalendarItem)
calendarItemWithIdentifier ekEventStore  identifier =
withObjCPtr identifier $ \raw_identifier ->
    sendMsg ekEventStore (mkSelector "calendarItemWithIdentifier:") (retPtr retVoid) [argPtr (castPtr raw_identifier :: Ptr ())] >>= retainedObject . castPtr

-- | calendarItemsWithExternalIdentifier:
--
-- Returns either matching reminders or the first occurrences of any events matching                the given external identifier.
--
-- This method returns a set of EKEvents or EKReminders with the given external identifier.                Due to reasons discussed in -[EKCalendarItem calendarItemExternalIdentifier], there may be                more than one matching calendar item.
--
-- @externalIdentifier@ — The value obtained from EKCalendarItem's                calendarItemExternalIdentifier property
--
-- Returns: An unsorted array of EKCalendarItem instances
--
-- ObjC selector: @- calendarItemsWithExternalIdentifier:@
calendarItemsWithExternalIdentifier :: (IsEKEventStore ekEventStore, IsNSString externalIdentifier) => ekEventStore -> externalIdentifier -> IO (Id NSArray)
calendarItemsWithExternalIdentifier ekEventStore  externalIdentifier =
withObjCPtr externalIdentifier $ \raw_externalIdentifier ->
    sendMsg ekEventStore (mkSelector "calendarItemsWithExternalIdentifier:") (retPtr retVoid) [argPtr (castPtr raw_externalIdentifier :: Ptr ())] >>= retainedObject . castPtr

-- | saveEvent:span:error:
--
-- Saves changes to an event permanently.
--
-- This method attempts to save the event to the calendar database. It returns YES if                successful and NO otherwise. It's possible for this method to return NO, and error                will be set to nil. This occurs if the event wasn't dirty and didn't need saving. This                means the correct way to detect failure is a result of NO and a non-nil error parameter.                Passing an event fetched from another EKEventStore instance into this function will                raise an exception.
--
-- After an event is successfully saved, it is also put into sync with the database, meaning                that all fields you did not change will be updated to the latest values. If you save the                event, but it was deleted by a different store/process, you will effectively recreate the                event as a new event.
--
-- On WatchOS, saving changes is not supported.
--
-- @event@ — The event to save.
--
-- @span@ — The span to use (this event, or this and future events).
--
-- @error@ — If an error occurs, this will contain a valid NSError object on exit.
--
-- ObjC selector: @- saveEvent:span:error:@
saveEvent_span_error :: (IsEKEventStore ekEventStore, IsEKEvent event, IsNSError error_) => ekEventStore -> event -> EKSpan -> error_ -> IO Bool
saveEvent_span_error ekEventStore  event span error_ =
withObjCPtr event $ \raw_event ->
  withObjCPtr error_ $ \raw_error_ ->
      fmap ((/= 0) :: CULong -> Bool) $ sendMsg ekEventStore (mkSelector "saveEvent:span:error:") retCULong [argPtr (castPtr raw_event :: Ptr ()), argCLong (coerce span), argPtr (castPtr raw_error_ :: Ptr ())]

-- | removeEvent:span:error:
--
-- Removes an event from the calendar store.
--
-- This method attempts to remove the event from the calendar database. It returns YES if                successful and NO otherwise. It's possible for this method to return NO, and error                will be set to nil. This occurs if the event wasn't ever added and didn't need removing. This                means the correct way to detect failure is a result of NO and a non-nil error parameter.                Passing an event from another CalendarStore into this function will raise an exception. After                an event is removed, it is no longer tied to this calendar store, and all data in the event                is cleared except for the eventIdentifier.
--
-- On WatchOS, modifying the database is not supported.
--
-- @event@ — The event to save.
--
-- @span@ — The span to use (this event, or this and future events).
--
-- @error@ — If an error occurs, this will contain a valid NSError object on exit.
--
-- ObjC selector: @- removeEvent:span:error:@
removeEvent_span_error :: (IsEKEventStore ekEventStore, IsEKEvent event, IsNSError error_) => ekEventStore -> event -> EKSpan -> error_ -> IO Bool
removeEvent_span_error ekEventStore  event span error_ =
withObjCPtr event $ \raw_event ->
  withObjCPtr error_ $ \raw_error_ ->
      fmap ((/= 0) :: CULong -> Bool) $ sendMsg ekEventStore (mkSelector "removeEvent:span:error:") retCULong [argPtr (castPtr raw_event :: Ptr ()), argCLong (coerce span), argPtr (castPtr raw_error_ :: Ptr ())]

-- | @- saveEvent:span:commit:error:@
saveEvent_span_commit_error :: (IsEKEventStore ekEventStore, IsEKEvent event, IsNSError error_) => ekEventStore -> event -> EKSpan -> Bool -> error_ -> IO Bool
saveEvent_span_commit_error ekEventStore  event span commit error_ =
withObjCPtr event $ \raw_event ->
  withObjCPtr error_ $ \raw_error_ ->
      fmap ((/= 0) :: CULong -> Bool) $ sendMsg ekEventStore (mkSelector "saveEvent:span:commit:error:") retCULong [argPtr (castPtr raw_event :: Ptr ()), argCLong (coerce span), argCULong (if commit then 1 else 0), argPtr (castPtr raw_error_ :: Ptr ())]

-- | @- removeEvent:span:commit:error:@
removeEvent_span_commit_error :: (IsEKEventStore ekEventStore, IsEKEvent event, IsNSError error_) => ekEventStore -> event -> EKSpan -> Bool -> error_ -> IO Bool
removeEvent_span_commit_error ekEventStore  event span commit error_ =
withObjCPtr event $ \raw_event ->
  withObjCPtr error_ $ \raw_error_ ->
      fmap ((/= 0) :: CULong -> Bool) $ sendMsg ekEventStore (mkSelector "removeEvent:span:commit:error:") retCULong [argPtr (castPtr raw_event :: Ptr ()), argCLong (coerce span), argCULong (if commit then 1 else 0), argPtr (castPtr raw_error_ :: Ptr ())]

-- | eventWithIdentifier:
--
-- Returns the first occurrence of an event matching the given event identifier.
--
-- @identifier@ — The eventIdentifier to search for.
--
-- Returns: An EKEvent object, or nil if not found.
--
-- ObjC selector: @- eventWithIdentifier:@
eventWithIdentifier :: (IsEKEventStore ekEventStore, IsNSString identifier) => ekEventStore -> identifier -> IO (Id EKEvent)
eventWithIdentifier ekEventStore  identifier =
withObjCPtr identifier $ \raw_identifier ->
    sendMsg ekEventStore (mkSelector "eventWithIdentifier:") (retPtr retVoid) [argPtr (castPtr raw_identifier :: Ptr ())] >>= retainedObject . castPtr

-- | eventsMatchingPredicate:
--
-- Searches for events that match the given predicate.
--
-- This call executes a search for the events indicated by the predicate passed to it.
--
-- It is synchronous. If you want async behavior, you should either use dispatch_async or                NSOperation to run the query someplace other than the main thread, and then funnel the                array back to the main thread.
--
-- @predicate@ — The predicate to invoke. If this predicate was not created with the predicate                            creation functions in this class, an exception is raised.
--
-- Returns: An array of EKEvent objects, or nil. There is no guaranteed order to the events.
--
-- ObjC selector: @- eventsMatchingPredicate:@
eventsMatchingPredicate :: (IsEKEventStore ekEventStore, IsNSPredicate predicate) => ekEventStore -> predicate -> IO (Id NSArray)
eventsMatchingPredicate ekEventStore  predicate =
withObjCPtr predicate $ \raw_predicate ->
    sendMsg ekEventStore (mkSelector "eventsMatchingPredicate:") (retPtr retVoid) [argPtr (castPtr raw_predicate :: Ptr ())] >>= retainedObject . castPtr

-- | enumerateEventsMatchingPredicate:usingBlock:
--
-- Searches for events that match the given predicate.
--
-- This call executes a search for the events indicated by the predicate passed to it, calling                the block specified in the callback parameter for each event.
--
-- This method is synchronous. If you want async behavior, you should either use dispatch_async or                NSOperation to run the query someplace other than the main thread.
--
-- @predicate@ — The predicate to invoke. If this predicate was not created with the predicate                            creation functions in this class, an exception is raised.
--
-- @block@ — The block to call for each event. Your block should return YES in the stop                            parameter to stop iterating.
--
-- ObjC selector: @- enumerateEventsMatchingPredicate:usingBlock:@
enumerateEventsMatchingPredicate_usingBlock :: (IsEKEventStore ekEventStore, IsNSPredicate predicate) => ekEventStore -> predicate -> Ptr () -> IO ()
enumerateEventsMatchingPredicate_usingBlock ekEventStore  predicate block =
withObjCPtr predicate $ \raw_predicate ->
    sendMsg ekEventStore (mkSelector "enumerateEventsMatchingPredicate:usingBlock:") retVoid [argPtr (castPtr raw_predicate :: Ptr ()), argPtr (castPtr block :: Ptr ())]

-- | predicateForEventsWithStartDate:endDate:calendars:
--
-- Creates a predicate for use with eventsMatchingPredicate or enumerateEventsMatchingPredicate:usingBlock:.
--
-- Creates a simple query predicate to search for events within a certain date range. At present,                this will return events in the default time zone ([NSTimeZone defaultTimeZone]).
--
-- For performance reasons, this method will only return events within a four year timespan.                If the date range between the startDate and endDate is greater than four years, then it will be shortened                 to the first four years.
--
-- @startDate@ — The start date.
--
-- @endDate@ — The end date.
--
-- @calendars@ — The calendars to search for events in, or nil to search all calendars.
--
-- ObjC selector: @- predicateForEventsWithStartDate:endDate:calendars:@
predicateForEventsWithStartDate_endDate_calendars :: (IsEKEventStore ekEventStore, IsNSDate startDate, IsNSDate endDate, IsNSArray calendars) => ekEventStore -> startDate -> endDate -> calendars -> IO (Id NSPredicate)
predicateForEventsWithStartDate_endDate_calendars ekEventStore  startDate endDate calendars =
withObjCPtr startDate $ \raw_startDate ->
  withObjCPtr endDate $ \raw_endDate ->
    withObjCPtr calendars $ \raw_calendars ->
        sendMsg ekEventStore (mkSelector "predicateForEventsWithStartDate:endDate:calendars:") (retPtr retVoid) [argPtr (castPtr raw_startDate :: Ptr ()), argPtr (castPtr raw_endDate :: Ptr ()), argPtr (castPtr raw_calendars :: Ptr ())] >>= retainedObject . castPtr

-- | saveReminder:commit:error:
--
-- Saves changes to a reminder.
--
-- This method attempts to save the reminder to the event store database. It returns YES if                successful and NO otherwise. Passing a reminder fetched from another EKEventStore instance                into this function will raise an exception.
--
-- After a reminder is successfully saved, its fields are updated to the latest values in                the database.
--
-- On WatchOS, saving changes is not supported.
--
-- @reminder@ — The reminder to save.
--
-- @commit@ — Whether to save to the database or not. Pass NO to batch changes together and                            commit with [EKEventStore commit:].
--
-- @error@ — If an error occurs, this will contain a valid NSError object on exit.
--
-- ObjC selector: @- saveReminder:commit:error:@
saveReminder_commit_error :: (IsEKEventStore ekEventStore, IsEKReminder reminder, IsNSError error_) => ekEventStore -> reminder -> Bool -> error_ -> IO Bool
saveReminder_commit_error ekEventStore  reminder commit error_ =
withObjCPtr reminder $ \raw_reminder ->
  withObjCPtr error_ $ \raw_error_ ->
      fmap ((/= 0) :: CULong -> Bool) $ sendMsg ekEventStore (mkSelector "saveReminder:commit:error:") retCULong [argPtr (castPtr raw_reminder :: Ptr ()), argCULong (if commit then 1 else 0), argPtr (castPtr raw_error_ :: Ptr ())]

-- | removeReminder:commit:error:
--
-- Removes a reminder from the event store.
--
-- This method attempts to remove the reminder from the event store database. It returns YES if                successful and NO otherwise. Passing a reminder from another EKEventStore into this function                will raise an exception. After a reminder is removed, it is no longer tied to this event store.
--
-- On WatchOS, modifying the database is not supported.
--
-- @reminder@ — The reminder to save.
--
-- @commit@ — Whether to save to the database or not. Pass NO to batch changes together and                            commit with [EKEventStore commit:].
--
-- @error@ — If an error occurs, this will contain a valid NSError object on exit.
--
-- ObjC selector: @- removeReminder:commit:error:@
removeReminder_commit_error :: (IsEKEventStore ekEventStore, IsEKReminder reminder, IsNSError error_) => ekEventStore -> reminder -> Bool -> error_ -> IO Bool
removeReminder_commit_error ekEventStore  reminder commit error_ =
withObjCPtr reminder $ \raw_reminder ->
  withObjCPtr error_ $ \raw_error_ ->
      fmap ((/= 0) :: CULong -> Bool) $ sendMsg ekEventStore (mkSelector "removeReminder:commit:error:") retCULong [argPtr (castPtr raw_reminder :: Ptr ()), argCULong (if commit then 1 else 0), argPtr (castPtr raw_error_ :: Ptr ())]

-- | cancelFetchRequest:
--
-- Given a value returned from fetchRemindersMatchingPredicate, this method can be used to                cancel the request. Once called, the completion block specified in fetchReminders... will                not be called.
--
-- ObjC selector: @- cancelFetchRequest:@
cancelFetchRequest :: IsEKEventStore ekEventStore => ekEventStore -> RawId -> IO ()
cancelFetchRequest ekEventStore  fetchIdentifier =
  sendMsg ekEventStore (mkSelector "cancelFetchRequest:") retVoid [argPtr (castPtr (unRawId fetchIdentifier) :: Ptr ())]

-- | predicateForRemindersInCalendars:
--
-- Fetch all reminders in a set of calendars.
--
-- You can pass nil for calendars to fetch from all available calendars.
--
-- ObjC selector: @- predicateForRemindersInCalendars:@
predicateForRemindersInCalendars :: (IsEKEventStore ekEventStore, IsNSArray calendars) => ekEventStore -> calendars -> IO (Id NSPredicate)
predicateForRemindersInCalendars ekEventStore  calendars =
withObjCPtr calendars $ \raw_calendars ->
    sendMsg ekEventStore (mkSelector "predicateForRemindersInCalendars:") (retPtr retVoid) [argPtr (castPtr raw_calendars :: Ptr ())] >>= retainedObject . castPtr

-- | predicateForIncompleteRemindersWithDueDateStarting:ending:calendars:
--
-- Fetch incomplete reminders in a set of calendars.
--
-- You can use this method to search for incomplete reminders due in a range.                You can pass nil for start date to find all reminders due before endDate.                You can pass nil for both start and end date to get all incomplete reminders                in the specified calendars.                You can pass nil for calendars to fetch from all available calendars.
--
-- ObjC selector: @- predicateForIncompleteRemindersWithDueDateStarting:ending:calendars:@
predicateForIncompleteRemindersWithDueDateStarting_ending_calendars :: (IsEKEventStore ekEventStore, IsNSDate startDate, IsNSDate endDate, IsNSArray calendars) => ekEventStore -> startDate -> endDate -> calendars -> IO (Id NSPredicate)
predicateForIncompleteRemindersWithDueDateStarting_ending_calendars ekEventStore  startDate endDate calendars =
withObjCPtr startDate $ \raw_startDate ->
  withObjCPtr endDate $ \raw_endDate ->
    withObjCPtr calendars $ \raw_calendars ->
        sendMsg ekEventStore (mkSelector "predicateForIncompleteRemindersWithDueDateStarting:ending:calendars:") (retPtr retVoid) [argPtr (castPtr raw_startDate :: Ptr ()), argPtr (castPtr raw_endDate :: Ptr ()), argPtr (castPtr raw_calendars :: Ptr ())] >>= retainedObject . castPtr

-- | predicateForCompletedRemindersWithCompletionDateStarting:ending:calendars:
--
-- Fetch completed reminders in a set of calendars.
--
-- You can use this method to search for reminders completed between a range of dates.                You can pass nil for start date to find all reminders completed before endDate.                You can pass nil for both start and end date to get all completed reminders                in the specified calendars.                You can pass nil for calendars to fetch from all available calendars.
--
-- ObjC selector: @- predicateForCompletedRemindersWithCompletionDateStarting:ending:calendars:@
predicateForCompletedRemindersWithCompletionDateStarting_ending_calendars :: (IsEKEventStore ekEventStore, IsNSDate startDate, IsNSDate endDate, IsNSArray calendars) => ekEventStore -> startDate -> endDate -> calendars -> IO (Id NSPredicate)
predicateForCompletedRemindersWithCompletionDateStarting_ending_calendars ekEventStore  startDate endDate calendars =
withObjCPtr startDate $ \raw_startDate ->
  withObjCPtr endDate $ \raw_endDate ->
    withObjCPtr calendars $ \raw_calendars ->
        sendMsg ekEventStore (mkSelector "predicateForCompletedRemindersWithCompletionDateStarting:ending:calendars:") (retPtr retVoid) [argPtr (castPtr raw_startDate :: Ptr ()), argPtr (castPtr raw_endDate :: Ptr ()), argPtr (castPtr raw_calendars :: Ptr ())] >>= retainedObject . castPtr

-- | commit:
--
-- Commits pending changes to the database.
--
-- If you use saveCalendar/saveEvent/removeCalendar/removeEvent, etc. and you pass NO to their                parameter, you are batching changes for a later commit. This method does that commit. This                allows you to save the database only once for many additions or changes.  If you pass                YES to methods' commit parameter, then you don't need to call this method.
--
-- This method will return YES as long as nothing went awry, even if nothing was actually                committed. If it returns NO, error should contain the reason it became unhappy.
--
-- On WatchOS, modifying the database is not supported.
--
-- ObjC selector: @- commit:@
commit :: (IsEKEventStore ekEventStore, IsNSError error_) => ekEventStore -> error_ -> IO Bool
commit ekEventStore  error_ =
withObjCPtr error_ $ \raw_error_ ->
    fmap ((/= 0) :: CULong -> Bool) $ sendMsg ekEventStore (mkSelector "commit:") retCULong [argPtr (castPtr raw_error_ :: Ptr ())]

-- | reset
--
-- Resets the event store.
--
-- You can use this method to forget ALL changes made to the event store (all additions, all                fetched objects, etc.). It essentially is as if you released the store and then created a                new one. It brings it back to its initial state. All objects ever created/fetched, etc.                using this store are no longer connected to it and are considered invalid.
--
-- ObjC selector: @- reset@
reset :: IsEKEventStore ekEventStore => ekEventStore -> IO ()
reset ekEventStore  =
  sendMsg ekEventStore (mkSelector "reset") retVoid []

-- | refreshSourcesIfNecessary
--
-- Cause a sync to potentially occur taking into account the necessity of it.
--
-- You can call this method to pull new data from remote sources.                  This only updates the event store's data.  If you want to update your objects after                 refreshing the sources, you should call refresh on each of them afterwards.                On iOS and macOS, this sync only occurs if deemed necessary.                On WatchOS, initiating sync is not available. Sync will occur automatically with the paired iOS device.
--
-- ObjC selector: @- refreshSourcesIfNecessary@
refreshSourcesIfNecessary :: IsEKEventStore ekEventStore => ekEventStore -> IO ()
refreshSourcesIfNecessary ekEventStore  =
  sendMsg ekEventStore (mkSelector "refreshSourcesIfNecessary") retVoid []

-- | eventStoreIdentifier
--
-- Returns a unique identifier string representing this calendar store.
--
-- ObjC selector: @- eventStoreIdentifier@
eventStoreIdentifier :: IsEKEventStore ekEventStore => ekEventStore -> IO (Id NSString)
eventStoreIdentifier ekEventStore  =
  sendMsg ekEventStore (mkSelector "eventStoreIdentifier") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | defaultCalendarForNewEvents
--
-- Returns the calendar that events should be added to by default.
--
-- This may be nil if there is no default calendar for new events.
--
-- ObjC selector: @- defaultCalendarForNewEvents@
defaultCalendarForNewEvents :: IsEKEventStore ekEventStore => ekEventStore -> IO (Id EKCalendar)
defaultCalendarForNewEvents ekEventStore  =
  sendMsg ekEventStore (mkSelector "defaultCalendarForNewEvents") (retPtr retVoid) [] >>= retainedObject . castPtr

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @authorizationStatusForEntityType:@
authorizationStatusForEntityTypeSelector :: Selector
authorizationStatusForEntityTypeSelector = mkSelector "authorizationStatusForEntityType:"

-- | @Selector@ for @initWithAccessToEntityTypes:@
initWithAccessToEntityTypesSelector :: Selector
initWithAccessToEntityTypesSelector = mkSelector "initWithAccessToEntityTypes:"

-- | @Selector@ for @init@
initSelector :: Selector
initSelector = mkSelector "init"

-- | @Selector@ for @initWithSources:@
initWithSourcesSelector :: Selector
initWithSourcesSelector = mkSelector "initWithSources:"

-- | @Selector@ for @requestFullAccessToEventsWithCompletion:@
requestFullAccessToEventsWithCompletionSelector :: Selector
requestFullAccessToEventsWithCompletionSelector = mkSelector "requestFullAccessToEventsWithCompletion:"

-- | @Selector@ for @requestWriteOnlyAccessToEventsWithCompletion:@
requestWriteOnlyAccessToEventsWithCompletionSelector :: Selector
requestWriteOnlyAccessToEventsWithCompletionSelector = mkSelector "requestWriteOnlyAccessToEventsWithCompletion:"

-- | @Selector@ for @requestFullAccessToRemindersWithCompletion:@
requestFullAccessToRemindersWithCompletionSelector :: Selector
requestFullAccessToRemindersWithCompletionSelector = mkSelector "requestFullAccessToRemindersWithCompletion:"

-- | @Selector@ for @requestAccessToEntityType:completion:@
requestAccessToEntityType_completionSelector :: Selector
requestAccessToEntityType_completionSelector = mkSelector "requestAccessToEntityType:completion:"

-- | @Selector@ for @sourceWithIdentifier:@
sourceWithIdentifierSelector :: Selector
sourceWithIdentifierSelector = mkSelector "sourceWithIdentifier:"

-- | @Selector@ for @calendarsForEntityType:@
calendarsForEntityTypeSelector :: Selector
calendarsForEntityTypeSelector = mkSelector "calendarsForEntityType:"

-- | @Selector@ for @defaultCalendarForNewReminders@
defaultCalendarForNewRemindersSelector :: Selector
defaultCalendarForNewRemindersSelector = mkSelector "defaultCalendarForNewReminders"

-- | @Selector@ for @calendarWithIdentifier:@
calendarWithIdentifierSelector :: Selector
calendarWithIdentifierSelector = mkSelector "calendarWithIdentifier:"

-- | @Selector@ for @saveCalendar:commit:error:@
saveCalendar_commit_errorSelector :: Selector
saveCalendar_commit_errorSelector = mkSelector "saveCalendar:commit:error:"

-- | @Selector@ for @removeCalendar:commit:error:@
removeCalendar_commit_errorSelector :: Selector
removeCalendar_commit_errorSelector = mkSelector "removeCalendar:commit:error:"

-- | @Selector@ for @calendarItemWithIdentifier:@
calendarItemWithIdentifierSelector :: Selector
calendarItemWithIdentifierSelector = mkSelector "calendarItemWithIdentifier:"

-- | @Selector@ for @calendarItemsWithExternalIdentifier:@
calendarItemsWithExternalIdentifierSelector :: Selector
calendarItemsWithExternalIdentifierSelector = mkSelector "calendarItemsWithExternalIdentifier:"

-- | @Selector@ for @saveEvent:span:error:@
saveEvent_span_errorSelector :: Selector
saveEvent_span_errorSelector = mkSelector "saveEvent:span:error:"

-- | @Selector@ for @removeEvent:span:error:@
removeEvent_span_errorSelector :: Selector
removeEvent_span_errorSelector = mkSelector "removeEvent:span:error:"

-- | @Selector@ for @saveEvent:span:commit:error:@
saveEvent_span_commit_errorSelector :: Selector
saveEvent_span_commit_errorSelector = mkSelector "saveEvent:span:commit:error:"

-- | @Selector@ for @removeEvent:span:commit:error:@
removeEvent_span_commit_errorSelector :: Selector
removeEvent_span_commit_errorSelector = mkSelector "removeEvent:span:commit:error:"

-- | @Selector@ for @eventWithIdentifier:@
eventWithIdentifierSelector :: Selector
eventWithIdentifierSelector = mkSelector "eventWithIdentifier:"

-- | @Selector@ for @eventsMatchingPredicate:@
eventsMatchingPredicateSelector :: Selector
eventsMatchingPredicateSelector = mkSelector "eventsMatchingPredicate:"

-- | @Selector@ for @enumerateEventsMatchingPredicate:usingBlock:@
enumerateEventsMatchingPredicate_usingBlockSelector :: Selector
enumerateEventsMatchingPredicate_usingBlockSelector = mkSelector "enumerateEventsMatchingPredicate:usingBlock:"

-- | @Selector@ for @predicateForEventsWithStartDate:endDate:calendars:@
predicateForEventsWithStartDate_endDate_calendarsSelector :: Selector
predicateForEventsWithStartDate_endDate_calendarsSelector = mkSelector "predicateForEventsWithStartDate:endDate:calendars:"

-- | @Selector@ for @saveReminder:commit:error:@
saveReminder_commit_errorSelector :: Selector
saveReminder_commit_errorSelector = mkSelector "saveReminder:commit:error:"

-- | @Selector@ for @removeReminder:commit:error:@
removeReminder_commit_errorSelector :: Selector
removeReminder_commit_errorSelector = mkSelector "removeReminder:commit:error:"

-- | @Selector@ for @cancelFetchRequest:@
cancelFetchRequestSelector :: Selector
cancelFetchRequestSelector = mkSelector "cancelFetchRequest:"

-- | @Selector@ for @predicateForRemindersInCalendars:@
predicateForRemindersInCalendarsSelector :: Selector
predicateForRemindersInCalendarsSelector = mkSelector "predicateForRemindersInCalendars:"

-- | @Selector@ for @predicateForIncompleteRemindersWithDueDateStarting:ending:calendars:@
predicateForIncompleteRemindersWithDueDateStarting_ending_calendarsSelector :: Selector
predicateForIncompleteRemindersWithDueDateStarting_ending_calendarsSelector = mkSelector "predicateForIncompleteRemindersWithDueDateStarting:ending:calendars:"

-- | @Selector@ for @predicateForCompletedRemindersWithCompletionDateStarting:ending:calendars:@
predicateForCompletedRemindersWithCompletionDateStarting_ending_calendarsSelector :: Selector
predicateForCompletedRemindersWithCompletionDateStarting_ending_calendarsSelector = mkSelector "predicateForCompletedRemindersWithCompletionDateStarting:ending:calendars:"

-- | @Selector@ for @commit:@
commitSelector :: Selector
commitSelector = mkSelector "commit:"

-- | @Selector@ for @reset@
resetSelector :: Selector
resetSelector = mkSelector "reset"

-- | @Selector@ for @refreshSourcesIfNecessary@
refreshSourcesIfNecessarySelector :: Selector
refreshSourcesIfNecessarySelector = mkSelector "refreshSourcesIfNecessary"

-- | @Selector@ for @eventStoreIdentifier@
eventStoreIdentifierSelector :: Selector
eventStoreIdentifierSelector = mkSelector "eventStoreIdentifier"

-- | @Selector@ for @defaultCalendarForNewEvents@
defaultCalendarForNewEventsSelector :: Selector
defaultCalendarForNewEventsSelector = mkSelector "defaultCalendarForNewEvents"

