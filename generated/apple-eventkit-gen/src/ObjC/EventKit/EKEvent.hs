{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | EKEvent
--
-- The EKEvent class represents an occurrence of an event.
--
-- Generated bindings for @EKEvent@.
module ObjC.EventKit.EKEvent
  ( EKEvent
  , IsEKEvent(..)
  , eventWithEventStore
  , compareStartDateWithEvent
  , refresh
  , eventIdentifier
  , allDay
  , setAllDay
  , startDate
  , setStartDate
  , endDate
  , setEndDate
  , structuredLocation
  , setStructuredLocation
  , organizer
  , availability
  , setAvailability
  , status
  , isDetached
  , occurrenceDate
  , birthdayContactIdentifier
  , birthdayPersonID
  , birthdayPersonUniqueID
  , allDaySelector
  , availabilitySelector
  , birthdayContactIdentifierSelector
  , birthdayPersonIDSelector
  , birthdayPersonUniqueIDSelector
  , compareStartDateWithEventSelector
  , endDateSelector
  , eventIdentifierSelector
  , eventWithEventStoreSelector
  , isDetachedSelector
  , occurrenceDateSelector
  , organizerSelector
  , refreshSelector
  , setAllDaySelector
  , setAvailabilitySelector
  , setEndDateSelector
  , setStartDateSelector
  , setStructuredLocationSelector
  , startDateSelector
  , statusSelector
  , structuredLocationSelector

  -- * Enum types
  , EKEventAvailability(EKEventAvailability)
  , pattern EKEventAvailabilityNotSupported
  , pattern EKEventAvailabilityBusy
  , pattern EKEventAvailabilityFree
  , pattern EKEventAvailabilityTentative
  , pattern EKEventAvailabilityUnavailable
  , EKEventStatus(EKEventStatus)
  , pattern EKEventStatusNone
  , pattern EKEventStatusConfirmed
  , pattern EKEventStatusTentative
  , pattern EKEventStatusCanceled
  , NSComparisonResult(NSComparisonResult)
  , pattern NSOrderedAscending
  , pattern NSOrderedSame
  , pattern NSOrderedDescending

  ) where

import Foreign.Ptr (Ptr, FunPtr)
import Foreign.C.Types

import ObjC.Runtime.Types
import ObjC.Runtime.Message (sendMessage, sendOwnedMessage, sendClassMessage, sendOwnedClassMessage)
import ObjC.Runtime.Selector (mkSelector)
import ObjC.Runtime.Class (getRequiredClass)

import ObjC.EventKit.Internal.Classes
import ObjC.EventKit.Internal.Enums
import ObjC.Foundation.Internal.Enums
import ObjC.Foundation.Internal.Classes

-- | eventWithEventStore:
--
-- Creates a new autoreleased event object.
--
-- ObjC selector: @+ eventWithEventStore:@
eventWithEventStore :: IsEKEventStore eventStore => eventStore -> IO (Id EKEvent)
eventWithEventStore eventStore =
  do
    cls' <- getRequiredClass "EKEvent"
    sendClassMessage cls' eventWithEventStoreSelector (toEKEventStore eventStore)

-- | compareStartDateWithEvent
--
-- Comparison function you can pass to sort NSArrays of EKEvents by start date.
--
-- ObjC selector: @- compareStartDateWithEvent:@
compareStartDateWithEvent :: (IsEKEvent ekEvent, IsEKEvent other) => ekEvent -> other -> IO NSComparisonResult
compareStartDateWithEvent ekEvent other =
  sendMessage ekEvent compareStartDateWithEventSelector (toEKEvent other)

-- | refresh
--
-- Refreshes an event object to ensure it's still valid.
--
-- When the database changes, your application is sent an EKEventStoreChangedNotification                 note. You should generally consider all EKEvent instances to be invalid as soon as                 you receive the notification. However, for events you truly care to keep around, you                 can call this method. It ensures the record is still valid by ensuring the event and                 start date are still valid. It also attempts to refresh all properties except those                 you might have modified. If this method returns NO, the record has been deleted or is                 otherwise invalid. You should not continue to use it. If it returns YES, all is still                 well, and the record is ready for continued use. You should only call this method on                 events that are more critical to keep around if possible, such as an event that is                 being actively edited, as this call is fairly heavyweight. Do not use it to refresh                 the entire selected range of events you might have had selected. It is mostly pointless                 anyway, as recurrence information may have changed.
--
-- ObjC selector: @- refresh@
refresh :: IsEKEvent ekEvent => ekEvent -> IO Bool
refresh ekEvent =
  sendMessage ekEvent refreshSelector

-- | eventIdentifier
--
-- A unique identifier for this event.
--
-- This identifier can be used to look the event up using [EKEventStore eventWithIdentifier:].                You can use this not only to simply fetch the event, but also to validate the event                has not been deleted out from under you when you get an external change notification                via the EKEventStore database changed notification. If eventWithIdentifier: returns nil,                the event was deleted.
--
-- Please note that if you change the calendar of an event, this ID will likely change. It is                currently also possible for the ID to change due to a sync operation. For example, if                a user moved an event on a different client to another calendar, we'd see it as a                 completely new event here.
--
-- This may be nil for events that have not been saved.
--
-- ObjC selector: @- eventIdentifier@
eventIdentifier :: IsEKEvent ekEvent => ekEvent -> IO (Id NSString)
eventIdentifier ekEvent =
  sendMessage ekEvent eventIdentifierSelector

-- | allDay
--
-- Indicates this event is an 'all day' event.
--
-- ObjC selector: @- allDay@
allDay :: IsEKEvent ekEvent => ekEvent -> IO Bool
allDay ekEvent =
  sendMessage ekEvent allDaySelector

-- | allDay
--
-- Indicates this event is an 'all day' event.
--
-- ObjC selector: @- setAllDay:@
setAllDay :: IsEKEvent ekEvent => ekEvent -> Bool -> IO ()
setAllDay ekEvent value =
  sendMessage ekEvent setAllDaySelector value

-- | startDate
--
-- The start date for the event.
--
-- This property represents the start date for this event. Floating events (such                 as all-day events) are currently always returned in the default time zone.                 ([NSTimeZone defaultTimeZone])
--
-- This will be nil for new events until you set it.
--
-- ObjC selector: @- startDate@
startDate :: IsEKEvent ekEvent => ekEvent -> IO (Id NSDate)
startDate ekEvent =
  sendMessage ekEvent startDateSelector

-- | startDate
--
-- The start date for the event.
--
-- This property represents the start date for this event. Floating events (such                 as all-day events) are currently always returned in the default time zone.                 ([NSTimeZone defaultTimeZone])
--
-- This will be nil for new events until you set it.
--
-- ObjC selector: @- setStartDate:@
setStartDate :: (IsEKEvent ekEvent, IsNSDate value) => ekEvent -> value -> IO ()
setStartDate ekEvent value =
  sendMessage ekEvent setStartDateSelector (toNSDate value)

-- | endDate
--
-- The end date for the event.
--
-- This will be nil for new events until you set it.
--
-- ObjC selector: @- endDate@
endDate :: IsEKEvent ekEvent => ekEvent -> IO (Id NSDate)
endDate ekEvent =
  sendMessage ekEvent endDateSelector

-- | endDate
--
-- The end date for the event.
--
-- This will be nil for new events until you set it.
--
-- ObjC selector: @- setEndDate:@
setEndDate :: (IsEKEvent ekEvent, IsNSDate value) => ekEvent -> value -> IO ()
setEndDate ekEvent value =
  sendMessage ekEvent setEndDateSelector (toNSDate value)

-- | structuredLocation
--
-- Allows you to set a structured location (a location with a potential geo-coordinate) on an                event. The getter for EKEvent’s location property just returns the structured location’s title.                The setter for EKEvent’s location property is equivalent to                [event setStructuredLocation:[EKStructuredLocation locationWithTitle:…]].
--
-- ObjC selector: @- structuredLocation@
structuredLocation :: IsEKEvent ekEvent => ekEvent -> IO RawId
structuredLocation ekEvent =
  sendMessage ekEvent structuredLocationSelector

-- | structuredLocation
--
-- Allows you to set a structured location (a location with a potential geo-coordinate) on an                event. The getter for EKEvent’s location property just returns the structured location’s title.                The setter for EKEvent’s location property is equivalent to                [event setStructuredLocation:[EKStructuredLocation locationWithTitle:…]].
--
-- ObjC selector: @- setStructuredLocation:@
setStructuredLocation :: IsEKEvent ekEvent => ekEvent -> RawId -> IO ()
setStructuredLocation ekEvent value =
  sendMessage ekEvent setStructuredLocationSelector value

-- | organizer
--
-- The organizer of this event, or nil.
--
-- ObjC selector: @- organizer@
organizer :: IsEKEvent ekEvent => ekEvent -> IO (Id EKParticipant)
organizer ekEvent =
  sendMessage ekEvent organizerSelector

-- | availability
--
-- The availability setting for this event.
--
-- The availability setting is used by CalDAV and Exchange servers to indicate                how the time should be treated for scheduling. If the calendar the event is                currently in does not support event availability, EKEventAvailabilityNotSupported                is returned.
--
-- ObjC selector: @- availability@
availability :: IsEKEvent ekEvent => ekEvent -> IO EKEventAvailability
availability ekEvent =
  sendMessage ekEvent availabilitySelector

-- | availability
--
-- The availability setting for this event.
--
-- The availability setting is used by CalDAV and Exchange servers to indicate                how the time should be treated for scheduling. If the calendar the event is                currently in does not support event availability, EKEventAvailabilityNotSupported                is returned.
--
-- ObjC selector: @- setAvailability:@
setAvailability :: IsEKEvent ekEvent => ekEvent -> EKEventAvailability -> IO ()
setAvailability ekEvent value =
  sendMessage ekEvent setAvailabilitySelector value

-- | status
--
-- The status of the event.
--
-- While the status offers four different values in the EKEventStatus enumeration,                in practice, the only actionable and reliable status is canceled. Any other status                should be considered informational at best. You cannot set this property. If you                wish to cancel an event, you should simply remove it using removeEvent:.
--
-- ObjC selector: @- status@
status :: IsEKEvent ekEvent => ekEvent -> IO EKEventStatus
status ekEvent =
  sendMessage ekEvent statusSelector

-- | isDetached
--
-- Represents whether this event is detached from a recurring series.
--
-- If this EKEvent is an instance of a repeating event, and an attribute of this                 EKEvent has been changed from the default value generated by the repeating event,                isDetached will return YES. If the EKEvent is unchanged from its default state, or                is not a repeating event, isDetached returns NO.
--
-- ObjC selector: @- isDetached@
isDetached :: IsEKEvent ekEvent => ekEvent -> IO Bool
isDetached ekEvent =
  sendMessage ekEvent isDetachedSelector

-- | occurrenceDate:
--
-- The occurrence date of an event if it is part of a recurring series.
--
-- This is only set if the event is part of a recurring series. It returns                the date on which this event was originally scheduled to occur. For occurrences                that are unmodified from the recurring series, this is the same as the start date.                This value will remain the same even if the event has been detached and its start                 date has changed. Floating events (such as all-day events) are currently returned                in the default time zone. ([NSTimeZone defaultTimeZone])
--
-- This will be nil for new events until you set startDate.
--
-- ObjC selector: @- occurrenceDate@
occurrenceDate :: IsEKEvent ekEvent => ekEvent -> IO RawId
occurrenceDate ekEvent =
  sendMessage ekEvent occurrenceDateSelector

-- | birthdayContactIdentifier
--
-- Specifies the contact identifier of the person this event was created for.
--
-- This property is only valid for events in the built-in Birthdays calendar. It specifies                the contact identifier (for use with the Contacts framework) of the person this event was                created for. For any other type of event, this property returns nil.
--
-- ObjC selector: @- birthdayContactIdentifier@
birthdayContactIdentifier :: IsEKEvent ekEvent => ekEvent -> IO RawId
birthdayContactIdentifier ekEvent =
  sendMessage ekEvent birthdayContactIdentifierSelector

-- | birthdayPersonID
--
-- Specifies the address book ID of the person this event was created for.
--
-- This property is only valid for events in the built-in Birthdays calendar. It specifies                the Address Book ID of the person this event was created for. For any other type of event,                this property returns -1.
--
-- ObjC selector: @- birthdayPersonID@
birthdayPersonID :: IsEKEvent ekEvent => ekEvent -> IO CLong
birthdayPersonID ekEvent =
  sendMessage ekEvent birthdayPersonIDSelector

-- | birthdayPersonUniqueID
--
-- Specifies the address book unique ID of the person this event was created for.
--
-- This property is only valid for events in the built-in Birthdays calendar. It specifies                the Address Book unique ID of the person this event was created for. For any other type of event,                this property returns nil.
--
-- ObjC selector: @- birthdayPersonUniqueID@
birthdayPersonUniqueID :: IsEKEvent ekEvent => ekEvent -> IO RawId
birthdayPersonUniqueID ekEvent =
  sendMessage ekEvent birthdayPersonUniqueIDSelector

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @eventWithEventStore:@
eventWithEventStoreSelector :: Selector '[Id EKEventStore] (Id EKEvent)
eventWithEventStoreSelector = mkSelector "eventWithEventStore:"

-- | @Selector@ for @compareStartDateWithEvent:@
compareStartDateWithEventSelector :: Selector '[Id EKEvent] NSComparisonResult
compareStartDateWithEventSelector = mkSelector "compareStartDateWithEvent:"

-- | @Selector@ for @refresh@
refreshSelector :: Selector '[] Bool
refreshSelector = mkSelector "refresh"

-- | @Selector@ for @eventIdentifier@
eventIdentifierSelector :: Selector '[] (Id NSString)
eventIdentifierSelector = mkSelector "eventIdentifier"

-- | @Selector@ for @allDay@
allDaySelector :: Selector '[] Bool
allDaySelector = mkSelector "allDay"

-- | @Selector@ for @setAllDay:@
setAllDaySelector :: Selector '[Bool] ()
setAllDaySelector = mkSelector "setAllDay:"

-- | @Selector@ for @startDate@
startDateSelector :: Selector '[] (Id NSDate)
startDateSelector = mkSelector "startDate"

-- | @Selector@ for @setStartDate:@
setStartDateSelector :: Selector '[Id NSDate] ()
setStartDateSelector = mkSelector "setStartDate:"

-- | @Selector@ for @endDate@
endDateSelector :: Selector '[] (Id NSDate)
endDateSelector = mkSelector "endDate"

-- | @Selector@ for @setEndDate:@
setEndDateSelector :: Selector '[Id NSDate] ()
setEndDateSelector = mkSelector "setEndDate:"

-- | @Selector@ for @structuredLocation@
structuredLocationSelector :: Selector '[] RawId
structuredLocationSelector = mkSelector "structuredLocation"

-- | @Selector@ for @setStructuredLocation:@
setStructuredLocationSelector :: Selector '[RawId] ()
setStructuredLocationSelector = mkSelector "setStructuredLocation:"

-- | @Selector@ for @organizer@
organizerSelector :: Selector '[] (Id EKParticipant)
organizerSelector = mkSelector "organizer"

-- | @Selector@ for @availability@
availabilitySelector :: Selector '[] EKEventAvailability
availabilitySelector = mkSelector "availability"

-- | @Selector@ for @setAvailability:@
setAvailabilitySelector :: Selector '[EKEventAvailability] ()
setAvailabilitySelector = mkSelector "setAvailability:"

-- | @Selector@ for @status@
statusSelector :: Selector '[] EKEventStatus
statusSelector = mkSelector "status"

-- | @Selector@ for @isDetached@
isDetachedSelector :: Selector '[] Bool
isDetachedSelector = mkSelector "isDetached"

-- | @Selector@ for @occurrenceDate@
occurrenceDateSelector :: Selector '[] RawId
occurrenceDateSelector = mkSelector "occurrenceDate"

-- | @Selector@ for @birthdayContactIdentifier@
birthdayContactIdentifierSelector :: Selector '[] RawId
birthdayContactIdentifierSelector = mkSelector "birthdayContactIdentifier"

-- | @Selector@ for @birthdayPersonID@
birthdayPersonIDSelector :: Selector '[] CLong
birthdayPersonIDSelector = mkSelector "birthdayPersonID"

-- | @Selector@ for @birthdayPersonUniqueID@
birthdayPersonUniqueIDSelector :: Selector '[] RawId
birthdayPersonUniqueIDSelector = mkSelector "birthdayPersonUniqueID"

