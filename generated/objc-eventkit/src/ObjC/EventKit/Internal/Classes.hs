{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}

-- | Internal module: all type tags, aliases, type classes, and
-- hierarchy instances for this framework.
--
-- Exists to break import cycles between per-class modules.
-- Import the per-class modules for the public API.
module ObjC.EventKit.Internal.Classes (
    module ObjC.EventKit.Internal.Classes,
    module ObjC.AddressBook.Internal.Classes,
    module ObjC.Foundation.Internal.Classes,
    module ObjC.MapKit.Internal.Classes,
  ) where

import Data.Proxy (Proxy(..))
import ObjC.Runtime.Types
import ObjC.Runtime.Class (getRequiredClass)
import ObjC.AddressBook.Internal.Classes
import ObjC.Foundation.Internal.Classes
import ObjC.MapKit.Internal.Classes

-- ---------- EKEventStore ----------

-- | EKEventStore
--
-- The EKEventStore class provides an interface for accessing and manipulating calendar events and reminders.
--
-- The EKEventStore class is the main point of contact for accessing Calendar data. You must                 create a EKEventStore object in order to retrieve/add/delete events or reminders from the Calendar database.
--
-- Events, Reminders, and Calendar objects retrieved from an event store cannot be used with any other event                 store. It is generally best to hold onto a long-lived instance of an event store, most                 likely as a singleton instance in your application.
-- 
-- Phantom type for @EKEventStore@.
data EKEventStore

instance IsObjCObject (Id EKEventStore) where
  toRawId = idToRawId
  withObjCPtr = withIdObjCPtr
  staticClass _ = getRequiredClass "EKEventStore"

class IsNSObject a => IsEKEventStore a where
  toEKEventStore :: a -> Id EKEventStore

instance IsEKEventStore (Id EKEventStore) where
  toEKEventStore = unsafeCastId

instance IsNSObject (Id EKEventStore) where
  toNSObject = unsafeCastId

-- ---------- EKObject ----------

-- | Phantom type for @EKObject@.
data EKObject

instance IsObjCObject (Id EKObject) where
  toRawId = idToRawId
  withObjCPtr = withIdObjCPtr
  staticClass _ = getRequiredClass "EKObject"

class IsNSObject a => IsEKObject a where
  toEKObject :: a -> Id EKObject

instance IsEKObject (Id EKObject) where
  toEKObject = unsafeCastId

instance IsNSObject (Id EKObject) where
  toNSObject = unsafeCastId

-- ---------- EKRecurrenceDayOfWeek ----------

-- | EKRecurrenceDayOfWeek
--
-- Class which represents a day of the week this recurrence will occur.
--
-- EKRecurrenceDayOfWeek specifies either a simple day of the week, or the nth instance                of a particular day of the week, such as the third Tuesday of every month. The week                number is only valid when used with monthly or yearly recurrences, since it would                be otherwise meaningless.
--
-- Valid values for dayOfTheWeek are integers 1-7, which correspond to days of the week                with Sunday = 1. Valid values for weekNumber portion are (+/-)1-53, where a negative                value indicates a value from the end of the range. For example, in a yearly event -1                means last week of the year. -1 in a Monthly recurrence indicates the last week of                the month.
--
-- The value 0 also indicates the weekNumber is irrelevant (every Sunday, etc.).
--
-- Day-of-week weekNumber values that are out of bounds for the recurrence type will                result in an exception when trying to initialize the recurrence. In particular,                weekNumber must be zero when passing EKRecurrenceDayOfWeek objects to initialize a weekly                 recurrence.
-- 
-- Phantom type for @EKRecurrenceDayOfWeek@.
data EKRecurrenceDayOfWeek

instance IsObjCObject (Id EKRecurrenceDayOfWeek) where
  toRawId = idToRawId
  withObjCPtr = withIdObjCPtr
  staticClass _ = getRequiredClass "EKRecurrenceDayOfWeek"

class IsNSObject a => IsEKRecurrenceDayOfWeek a where
  toEKRecurrenceDayOfWeek :: a -> Id EKRecurrenceDayOfWeek

instance IsEKRecurrenceDayOfWeek (Id EKRecurrenceDayOfWeek) where
  toEKRecurrenceDayOfWeek = unsafeCastId

instance IsNSObject (Id EKRecurrenceDayOfWeek) where
  toNSObject = unsafeCastId

-- ---------- EKRecurrenceEnd ----------

-- | EKRecurrenceEnd
--
-- Class which represents when a recurrence should end.
--
-- EKRecurrenceEnd is an attribute of EKRecurrenceRule that defines how long                the recurrence is scheduled to repeat. The recurrence can be defined either                with an NSUInteger that indicates the total number times it repeats, or with                an NSDate, after which it no longer repeats. An event which is set to never                end should have its EKRecurrenceEnd set to nil.
--
-- If the end of the pattern is defines with an NSDate, the client must pass a                valid NSDate, nil cannot be passed. If the end of the pattern is defined as                terms of a number of occurrences, the occurrenceCount passed to the initializer                must be positive, it cannot be 0. If the client attempts to initialize a                EKRecurrenceEnd with a nil NSDate or OccurrenceCount of 0, an exception is raised.
--
-- A EKRecurrenceEnd initialized with an end date will return 0 for occurrenceCount.                One initialized with a number of occurrences will return nil for its endDate.
-- 
-- Phantom type for @EKRecurrenceEnd@.
data EKRecurrenceEnd

instance IsObjCObject (Id EKRecurrenceEnd) where
  toRawId = idToRawId
  withObjCPtr = withIdObjCPtr
  staticClass _ = getRequiredClass "EKRecurrenceEnd"

class IsNSObject a => IsEKRecurrenceEnd a where
  toEKRecurrenceEnd :: a -> Id EKRecurrenceEnd

instance IsEKRecurrenceEnd (Id EKRecurrenceEnd) where
  toEKRecurrenceEnd = unsafeCastId

instance IsNSObject (Id EKRecurrenceEnd) where
  toNSObject = unsafeCastId

-- ---------- EKVirtualConferenceDescriptor ----------

-- | EKVirtualConferenceDescriptor
--
-- Describes a virtual conference.
-- 
-- Phantom type for @EKVirtualConferenceDescriptor@.
data EKVirtualConferenceDescriptor

instance IsObjCObject (Id EKVirtualConferenceDescriptor) where
  toRawId = idToRawId
  withObjCPtr = withIdObjCPtr
  staticClass _ = getRequiredClass "EKVirtualConferenceDescriptor"

class IsNSObject a => IsEKVirtualConferenceDescriptor a where
  toEKVirtualConferenceDescriptor :: a -> Id EKVirtualConferenceDescriptor

instance IsEKVirtualConferenceDescriptor (Id EKVirtualConferenceDescriptor) where
  toEKVirtualConferenceDescriptor = unsafeCastId

instance IsNSObject (Id EKVirtualConferenceDescriptor) where
  toNSObject = unsafeCastId

-- ---------- EKVirtualConferenceProvider ----------

-- | EKVirtualConferenceProvider
--
-- Provides virtual conferences to Calendar.
--
-- Subclass this class in your extension and override the below two methods.
-- 
-- Phantom type for @EKVirtualConferenceProvider@.
data EKVirtualConferenceProvider

instance IsObjCObject (Id EKVirtualConferenceProvider) where
  toRawId = idToRawId
  withObjCPtr = withIdObjCPtr
  staticClass _ = getRequiredClass "EKVirtualConferenceProvider"

class IsNSObject a => IsEKVirtualConferenceProvider a where
  toEKVirtualConferenceProvider :: a -> Id EKVirtualConferenceProvider

instance IsEKVirtualConferenceProvider (Id EKVirtualConferenceProvider) where
  toEKVirtualConferenceProvider = unsafeCastId

instance IsNSObject (Id EKVirtualConferenceProvider) where
  toNSObject = unsafeCastId

-- ---------- EKVirtualConferenceRoomTypeDescriptor ----------

-- | EKVirtualConferenceRoomTypeDescriptor
--
-- Describes a virtual conference room type.
-- 
-- Phantom type for @EKVirtualConferenceRoomTypeDescriptor@.
data EKVirtualConferenceRoomTypeDescriptor

instance IsObjCObject (Id EKVirtualConferenceRoomTypeDescriptor) where
  toRawId = idToRawId
  withObjCPtr = withIdObjCPtr
  staticClass _ = getRequiredClass "EKVirtualConferenceRoomTypeDescriptor"

class IsNSObject a => IsEKVirtualConferenceRoomTypeDescriptor a where
  toEKVirtualConferenceRoomTypeDescriptor :: a -> Id EKVirtualConferenceRoomTypeDescriptor

instance IsEKVirtualConferenceRoomTypeDescriptor (Id EKVirtualConferenceRoomTypeDescriptor) where
  toEKVirtualConferenceRoomTypeDescriptor = unsafeCastId

instance IsNSObject (Id EKVirtualConferenceRoomTypeDescriptor) where
  toNSObject = unsafeCastId

-- ---------- EKVirtualConferenceURLDescriptor ----------

-- | EKVirtualConferenceURLDescriptor
--
-- Describes a URL that can be used to join a virtual conference.
-- 
-- Phantom type for @EKVirtualConferenceURLDescriptor@.
data EKVirtualConferenceURLDescriptor

instance IsObjCObject (Id EKVirtualConferenceURLDescriptor) where
  toRawId = idToRawId
  withObjCPtr = withIdObjCPtr
  staticClass _ = getRequiredClass "EKVirtualConferenceURLDescriptor"

class IsNSObject a => IsEKVirtualConferenceURLDescriptor a where
  toEKVirtualConferenceURLDescriptor :: a -> Id EKVirtualConferenceURLDescriptor

instance IsEKVirtualConferenceURLDescriptor (Id EKVirtualConferenceURLDescriptor) where
  toEKVirtualConferenceURLDescriptor = unsafeCastId

instance IsNSObject (Id EKVirtualConferenceURLDescriptor) where
  toNSObject = unsafeCastId

-- ---------- EKAlarm ----------

-- | EKAlarm
--
-- The EKAlarm class provides an interface for accessing and manipulating calendar event alarms.
--
-- The EKAlarm class represents alarms on an event. An alarm can be relative (e.g. 15 mins before)                     or absolute (specific time).
-- 
-- Phantom type for @EKAlarm@.
data EKAlarm

instance IsObjCObject (Id EKAlarm) where
  toRawId = idToRawId
  withObjCPtr = withIdObjCPtr
  staticClass _ = getRequiredClass "EKAlarm"

class IsEKObject a => IsEKAlarm a where
  toEKAlarm :: a -> Id EKAlarm

instance IsEKAlarm (Id EKAlarm) where
  toEKAlarm = unsafeCastId

instance IsEKObject (Id EKAlarm) where
  toEKObject = unsafeCastId

instance IsNSObject (Id EKAlarm) where
  toNSObject = unsafeCastId

-- ---------- EKCalendar ----------

-- | EKCalendar
--
-- The EKCalendar class represents a calendar for events.
-- 
-- Phantom type for @EKCalendar@.
data EKCalendar

instance IsObjCObject (Id EKCalendar) where
  toRawId = idToRawId
  withObjCPtr = withIdObjCPtr
  staticClass _ = getRequiredClass "EKCalendar"

class IsEKObject a => IsEKCalendar a where
  toEKCalendar :: a -> Id EKCalendar

instance IsEKCalendar (Id EKCalendar) where
  toEKCalendar = unsafeCastId

instance IsEKObject (Id EKCalendar) where
  toEKObject = unsafeCastId

instance IsNSObject (Id EKCalendar) where
  toNSObject = unsafeCastId

-- ---------- EKCalendarItem ----------

-- | Phantom type for @EKCalendarItem@.
data EKCalendarItem

instance IsObjCObject (Id EKCalendarItem) where
  toRawId = idToRawId
  withObjCPtr = withIdObjCPtr
  staticClass _ = getRequiredClass "EKCalendarItem"

class IsEKObject a => IsEKCalendarItem a where
  toEKCalendarItem :: a -> Id EKCalendarItem

instance IsEKCalendarItem (Id EKCalendarItem) where
  toEKCalendarItem = unsafeCastId

instance IsEKObject (Id EKCalendarItem) where
  toEKObject = unsafeCastId

instance IsNSObject (Id EKCalendarItem) where
  toNSObject = unsafeCastId

-- ---------- EKParticipant ----------

-- | EKParticipant
--
-- Abstract class representing a participant attached to an event.
-- 
-- Phantom type for @EKParticipant@.
data EKParticipant

instance IsObjCObject (Id EKParticipant) where
  toRawId = idToRawId
  withObjCPtr = withIdObjCPtr
  staticClass _ = getRequiredClass "EKParticipant"

class IsEKObject a => IsEKParticipant a where
  toEKParticipant :: a -> Id EKParticipant

instance IsEKParticipant (Id EKParticipant) where
  toEKParticipant = unsafeCastId

instance IsEKObject (Id EKParticipant) where
  toEKObject = unsafeCastId

instance IsNSObject (Id EKParticipant) where
  toNSObject = unsafeCastId

-- ---------- EKRecurrenceRule ----------

-- | EKRecurrenceRule
--
-- Represents how an event repeats.
--
-- This class describes the recurrence pattern for a repeating event. The recurrence rules that                 can be expressed are not restricted to the recurrence patterns that can be set in Calendar's UI.
--
-- It is currently not possible to directly modify a EKRecurrenceRule or any of its properties.                 This functionality is achieved by creating a new EKRecurrenceRule, and setting an event to use the new rule.                 When a new recurrence rule is set on an EKEvent, that change is not saved until the client                 has passed the modified event to EKEventStore's saveEvent: method.
-- 
-- Phantom type for @EKRecurrenceRule@.
data EKRecurrenceRule

instance IsObjCObject (Id EKRecurrenceRule) where
  toRawId = idToRawId
  withObjCPtr = withIdObjCPtr
  staticClass _ = getRequiredClass "EKRecurrenceRule"

class IsEKObject a => IsEKRecurrenceRule a where
  toEKRecurrenceRule :: a -> Id EKRecurrenceRule

instance IsEKRecurrenceRule (Id EKRecurrenceRule) where
  toEKRecurrenceRule = unsafeCastId

instance IsEKObject (Id EKRecurrenceRule) where
  toEKObject = unsafeCastId

instance IsNSObject (Id EKRecurrenceRule) where
  toNSObject = unsafeCastId

-- ---------- EKSource ----------

-- | Phantom type for @EKSource@.
data EKSource

instance IsObjCObject (Id EKSource) where
  toRawId = idToRawId
  withObjCPtr = withIdObjCPtr
  staticClass _ = getRequiredClass "EKSource"

class IsEKObject a => IsEKSource a where
  toEKSource :: a -> Id EKSource

instance IsEKSource (Id EKSource) where
  toEKSource = unsafeCastId

instance IsEKObject (Id EKSource) where
  toEKObject = unsafeCastId

instance IsNSObject (Id EKSource) where
  toNSObject = unsafeCastId

-- ---------- EKStructuredLocation ----------

-- | Phantom type for @EKStructuredLocation@.
data EKStructuredLocation

instance IsObjCObject (Id EKStructuredLocation) where
  toRawId = idToRawId
  withObjCPtr = withIdObjCPtr
  staticClass _ = getRequiredClass "EKStructuredLocation"

class IsEKObject a => IsEKStructuredLocation a where
  toEKStructuredLocation :: a -> Id EKStructuredLocation

instance IsEKStructuredLocation (Id EKStructuredLocation) where
  toEKStructuredLocation = unsafeCastId

instance IsEKObject (Id EKStructuredLocation) where
  toEKObject = unsafeCastId

instance IsNSObject (Id EKStructuredLocation) where
  toNSObject = unsafeCastId

-- ---------- EKEvent ----------

-- | EKEvent
--
-- The EKEvent class represents an occurrence of an event.
-- 
-- Phantom type for @EKEvent@.
data EKEvent

instance IsObjCObject (Id EKEvent) where
  toRawId = idToRawId
  withObjCPtr = withIdObjCPtr
  staticClass _ = getRequiredClass "EKEvent"

class IsEKCalendarItem a => IsEKEvent a where
  toEKEvent :: a -> Id EKEvent

instance IsEKEvent (Id EKEvent) where
  toEKEvent = unsafeCastId

instance IsEKCalendarItem (Id EKEvent) where
  toEKCalendarItem = unsafeCastId

instance IsEKObject (Id EKEvent) where
  toEKObject = unsafeCastId

instance IsNSObject (Id EKEvent) where
  toNSObject = unsafeCastId

-- ---------- EKReminder ----------

-- | EKReminder
--
-- The EKReminder class represents a reminder (task/todo).
-- 
-- Phantom type for @EKReminder@.
data EKReminder

instance IsObjCObject (Id EKReminder) where
  toRawId = idToRawId
  withObjCPtr = withIdObjCPtr
  staticClass _ = getRequiredClass "EKReminder"

class IsEKCalendarItem a => IsEKReminder a where
  toEKReminder :: a -> Id EKReminder

instance IsEKReminder (Id EKReminder) where
  toEKReminder = unsafeCastId

instance IsEKCalendarItem (Id EKReminder) where
  toEKCalendarItem = unsafeCastId

instance IsEKObject (Id EKReminder) where
  toEKObject = unsafeCastId

instance IsNSObject (Id EKReminder) where
  toNSObject = unsafeCastId
