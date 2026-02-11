{-# LANGUAGE PatternSynonyms #-}
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
  , organizer
  , availability
  , setAvailability
  , status
  , isDetached
  , birthdayPersonID
  , eventWithEventStoreSelector
  , compareStartDateWithEventSelector
  , refreshSelector
  , eventIdentifierSelector
  , allDaySelector
  , setAllDaySelector
  , startDateSelector
  , setStartDateSelector
  , endDateSelector
  , setEndDateSelector
  , organizerSelector
  , availabilitySelector
  , setAvailabilitySelector
  , statusSelector
  , isDetachedSelector
  , birthdayPersonIDSelector

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
    withObjCPtr eventStore $ \raw_eventStore ->
      sendClassMsg cls' (mkSelector "eventWithEventStore:") (retPtr retVoid) [argPtr (castPtr raw_eventStore :: Ptr ())] >>= retainedObject . castPtr

-- | compareStartDateWithEvent
--
-- Comparison function you can pass to sort NSArrays of EKEvents by start date.
--
-- ObjC selector: @- compareStartDateWithEvent:@
compareStartDateWithEvent :: (IsEKEvent ekEvent, IsEKEvent other) => ekEvent -> other -> IO NSComparisonResult
compareStartDateWithEvent ekEvent  other =
withObjCPtr other $ \raw_other ->
    fmap (coerce :: CLong -> NSComparisonResult) $ sendMsg ekEvent (mkSelector "compareStartDateWithEvent:") retCLong [argPtr (castPtr raw_other :: Ptr ())]

-- | refresh
--
-- Refreshes an event object to ensure it's still valid.
--
-- When the database changes, your application is sent an EKEventStoreChangedNotification                 note. You should generally consider all EKEvent instances to be invalid as soon as                 you receive the notification. However, for events you truly care to keep around, you                 can call this method. It ensures the record is still valid by ensuring the event and                 start date are still valid. It also attempts to refresh all properties except those                 you might have modified. If this method returns NO, the record has been deleted or is                 otherwise invalid. You should not continue to use it. If it returns YES, all is still                 well, and the record is ready for continued use. You should only call this method on                 events that are more critical to keep around if possible, such as an event that is                 being actively edited, as this call is fairly heavyweight. Do not use it to refresh                 the entire selected range of events you might have had selected. It is mostly pointless                 anyway, as recurrence information may have changed.
--
-- ObjC selector: @- refresh@
refresh :: IsEKEvent ekEvent => ekEvent -> IO Bool
refresh ekEvent  =
  fmap ((/= 0) :: CULong -> Bool) $ sendMsg ekEvent (mkSelector "refresh") retCULong []

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
eventIdentifier ekEvent  =
  sendMsg ekEvent (mkSelector "eventIdentifier") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | allDay
--
-- Indicates this event is an 'all day' event.
--
-- ObjC selector: @- allDay@
allDay :: IsEKEvent ekEvent => ekEvent -> IO Bool
allDay ekEvent  =
  fmap ((/= 0) :: CULong -> Bool) $ sendMsg ekEvent (mkSelector "allDay") retCULong []

-- | allDay
--
-- Indicates this event is an 'all day' event.
--
-- ObjC selector: @- setAllDay:@
setAllDay :: IsEKEvent ekEvent => ekEvent -> Bool -> IO ()
setAllDay ekEvent  value =
  sendMsg ekEvent (mkSelector "setAllDay:") retVoid [argCULong (if value then 1 else 0)]

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
startDate ekEvent  =
  sendMsg ekEvent (mkSelector "startDate") (retPtr retVoid) [] >>= retainedObject . castPtr

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
setStartDate ekEvent  value =
withObjCPtr value $ \raw_value ->
    sendMsg ekEvent (mkSelector "setStartDate:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- | endDate
--
-- The end date for the event.
--
-- This will be nil for new events until you set it.
--
-- ObjC selector: @- endDate@
endDate :: IsEKEvent ekEvent => ekEvent -> IO (Id NSDate)
endDate ekEvent  =
  sendMsg ekEvent (mkSelector "endDate") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | endDate
--
-- The end date for the event.
--
-- This will be nil for new events until you set it.
--
-- ObjC selector: @- setEndDate:@
setEndDate :: (IsEKEvent ekEvent, IsNSDate value) => ekEvent -> value -> IO ()
setEndDate ekEvent  value =
withObjCPtr value $ \raw_value ->
    sendMsg ekEvent (mkSelector "setEndDate:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- | organizer
--
-- The organizer of this event, or nil.
--
-- ObjC selector: @- organizer@
organizer :: IsEKEvent ekEvent => ekEvent -> IO (Id EKParticipant)
organizer ekEvent  =
  sendMsg ekEvent (mkSelector "organizer") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | availability
--
-- The availability setting for this event.
--
-- The availability setting is used by CalDAV and Exchange servers to indicate                how the time should be treated for scheduling. If the calendar the event is                currently in does not support event availability, EKEventAvailabilityNotSupported                is returned.
--
-- ObjC selector: @- availability@
availability :: IsEKEvent ekEvent => ekEvent -> IO EKEventAvailability
availability ekEvent  =
  fmap (coerce :: CLong -> EKEventAvailability) $ sendMsg ekEvent (mkSelector "availability") retCLong []

-- | availability
--
-- The availability setting for this event.
--
-- The availability setting is used by CalDAV and Exchange servers to indicate                how the time should be treated for scheduling. If the calendar the event is                currently in does not support event availability, EKEventAvailabilityNotSupported                is returned.
--
-- ObjC selector: @- setAvailability:@
setAvailability :: IsEKEvent ekEvent => ekEvent -> EKEventAvailability -> IO ()
setAvailability ekEvent  value =
  sendMsg ekEvent (mkSelector "setAvailability:") retVoid [argCLong (coerce value)]

-- | status
--
-- The status of the event.
--
-- While the status offers four different values in the EKEventStatus enumeration,                in practice, the only actionable and reliable status is canceled. Any other status                should be considered informational at best. You cannot set this property. If you                wish to cancel an event, you should simply remove it using removeEvent:.
--
-- ObjC selector: @- status@
status :: IsEKEvent ekEvent => ekEvent -> IO EKEventStatus
status ekEvent  =
  fmap (coerce :: CLong -> EKEventStatus) $ sendMsg ekEvent (mkSelector "status") retCLong []

-- | isDetached
--
-- Represents whether this event is detached from a recurring series.
--
-- If this EKEvent is an instance of a repeating event, and an attribute of this                 EKEvent has been changed from the default value generated by the repeating event,                isDetached will return YES. If the EKEvent is unchanged from its default state, or                is not a repeating event, isDetached returns NO.
--
-- ObjC selector: @- isDetached@
isDetached :: IsEKEvent ekEvent => ekEvent -> IO Bool
isDetached ekEvent  =
  fmap ((/= 0) :: CULong -> Bool) $ sendMsg ekEvent (mkSelector "isDetached") retCULong []

-- | birthdayPersonID
--
-- Specifies the address book ID of the person this event was created for.
--
-- This property is only valid for events in the built-in Birthdays calendar. It specifies                the Address Book ID of the person this event was created for. For any other type of event,                this property returns -1.
--
-- ObjC selector: @- birthdayPersonID@
birthdayPersonID :: IsEKEvent ekEvent => ekEvent -> IO CLong
birthdayPersonID ekEvent  =
  sendMsg ekEvent (mkSelector "birthdayPersonID") retCLong []

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @eventWithEventStore:@
eventWithEventStoreSelector :: Selector
eventWithEventStoreSelector = mkSelector "eventWithEventStore:"

-- | @Selector@ for @compareStartDateWithEvent:@
compareStartDateWithEventSelector :: Selector
compareStartDateWithEventSelector = mkSelector "compareStartDateWithEvent:"

-- | @Selector@ for @refresh@
refreshSelector :: Selector
refreshSelector = mkSelector "refresh"

-- | @Selector@ for @eventIdentifier@
eventIdentifierSelector :: Selector
eventIdentifierSelector = mkSelector "eventIdentifier"

-- | @Selector@ for @allDay@
allDaySelector :: Selector
allDaySelector = mkSelector "allDay"

-- | @Selector@ for @setAllDay:@
setAllDaySelector :: Selector
setAllDaySelector = mkSelector "setAllDay:"

-- | @Selector@ for @startDate@
startDateSelector :: Selector
startDateSelector = mkSelector "startDate"

-- | @Selector@ for @setStartDate:@
setStartDateSelector :: Selector
setStartDateSelector = mkSelector "setStartDate:"

-- | @Selector@ for @endDate@
endDateSelector :: Selector
endDateSelector = mkSelector "endDate"

-- | @Selector@ for @setEndDate:@
setEndDateSelector :: Selector
setEndDateSelector = mkSelector "setEndDate:"

-- | @Selector@ for @organizer@
organizerSelector :: Selector
organizerSelector = mkSelector "organizer"

-- | @Selector@ for @availability@
availabilitySelector :: Selector
availabilitySelector = mkSelector "availability"

-- | @Selector@ for @setAvailability:@
setAvailabilitySelector :: Selector
setAvailabilitySelector = mkSelector "setAvailability:"

-- | @Selector@ for @status@
statusSelector :: Selector
statusSelector = mkSelector "status"

-- | @Selector@ for @isDetached@
isDetachedSelector :: Selector
isDetachedSelector = mkSelector "isDetached"

-- | @Selector@ for @birthdayPersonID@
birthdayPersonIDSelector :: Selector
birthdayPersonIDSelector = mkSelector "birthdayPersonID"

