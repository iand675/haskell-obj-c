{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | EKCalendar
--
-- The EKCalendar class represents a calendar for events.
--
-- Generated bindings for @EKCalendar@.
module ObjC.EventKit.EKCalendar
  ( EKCalendar
  , IsEKCalendar(..)
  , calendarWithEventStore
  , calendarForEntityType_eventStore
  , source
  , setSource
  , calendarIdentifier
  , title
  , setTitle
  , type_
  , allowsContentModifications
  , subscribed
  , immutable
  , cgColor
  , setCGColor
  , color
  , setColor
  , supportedEventAvailabilities
  , allowedEntityTypes
  , allowedEntityTypesSelector
  , allowsContentModificationsSelector
  , calendarForEntityType_eventStoreSelector
  , calendarIdentifierSelector
  , calendarWithEventStoreSelector
  , cgColorSelector
  , colorSelector
  , immutableSelector
  , setCGColorSelector
  , setColorSelector
  , setSourceSelector
  , setTitleSelector
  , sourceSelector
  , subscribedSelector
  , supportedEventAvailabilitiesSelector
  , titleSelector
  , typeSelector

  -- * Enum types
  , EKCalendarEventAvailabilityMask(EKCalendarEventAvailabilityMask)
  , pattern EKCalendarEventAvailabilityNone
  , pattern EKCalendarEventAvailabilityBusy
  , pattern EKCalendarEventAvailabilityFree
  , pattern EKCalendarEventAvailabilityTentative
  , pattern EKCalendarEventAvailabilityUnavailable
  , EKCalendarType(EKCalendarType)
  , pattern EKCalendarTypeLocal
  , pattern EKCalendarTypeCalDAV
  , pattern EKCalendarTypeExchange
  , pattern EKCalendarTypeSubscription
  , pattern EKCalendarTypeBirthday
  , EKEntityMask(EKEntityMask)
  , pattern EKEntityMaskEvent
  , pattern EKEntityMaskReminder
  , EKEntityType(EKEntityType)
  , pattern EKEntityTypeEvent
  , pattern EKEntityTypeReminder

  ) where

import Foreign.Ptr (Ptr, FunPtr)
import Foreign.C.Types

import ObjC.Runtime.Types
import ObjC.Runtime.Message (sendMessage, sendOwnedMessage, sendClassMessage, sendOwnedClassMessage)
import ObjC.Runtime.Selector (mkSelector)
import ObjC.Runtime.Class (getRequiredClass)

import ObjC.EventKit.Internal.Classes
import ObjC.EventKit.Internal.Enums
import ObjC.Foundation.Internal.Classes

-- | @+ calendarWithEventStore:@
calendarWithEventStore :: IsEKEventStore eventStore => eventStore -> IO (Id EKCalendar)
calendarWithEventStore eventStore =
  do
    cls' <- getRequiredClass "EKCalendar"
    sendClassMessage cls' calendarWithEventStoreSelector (toEKEventStore eventStore)

-- | calendarForEntityType:
--
-- Creates a new autoreleased calendar that may contain the given entity type.
--
-- You can only create calendars that accept either reminders or events via our API.                 However, other servers might allow mixing the two (though it is not common).
--
-- @entityType@ — The entity type that this calendar may support.
--
-- @eventStore@ — The event store in which to create this calendar.
--
-- ObjC selector: @+ calendarForEntityType:eventStore:@
calendarForEntityType_eventStore :: IsEKEventStore eventStore => EKEntityType -> eventStore -> IO (Id EKCalendar)
calendarForEntityType_eventStore entityType eventStore =
  do
    cls' <- getRequiredClass "EKCalendar"
    sendClassMessage cls' calendarForEntityType_eventStoreSelector entityType (toEKEventStore eventStore)

-- | source
--
-- The source representing the 'account' this calendar belongs to.                This is only settable when initially creating a calendar and then                effectively read-only after that. That is, you can create a calendar,                 but you cannot move it to another source.
--
-- This will be nil for new calendars until you set it.
--
-- ObjC selector: @- source@
source :: IsEKCalendar ekCalendar => ekCalendar -> IO (Id EKSource)
source ekCalendar =
  sendMessage ekCalendar sourceSelector

-- | source
--
-- The source representing the 'account' this calendar belongs to.                This is only settable when initially creating a calendar and then                effectively read-only after that. That is, you can create a calendar,                 but you cannot move it to another source.
--
-- This will be nil for new calendars until you set it.
--
-- ObjC selector: @- setSource:@
setSource :: (IsEKCalendar ekCalendar, IsEKSource value) => ekCalendar -> value -> IO ()
setSource ekCalendar value =
  sendMessage ekCalendar setSourceSelector (toEKSource value)

-- | calendarIdentifier
--
-- A unique identifier for the calendar. It is not sync-proof in that a full                sync will lose this identifier, so you should always have a back up plan for dealing                with a calendar that is no longer fetchable by this property, e.g. by title, type, color, etc.                Use [EKEventStore calendarWithIdentifier:] to look up the calendar by this value.
--
-- ObjC selector: @- calendarIdentifier@
calendarIdentifier :: IsEKCalendar ekCalendar => ekCalendar -> IO RawId
calendarIdentifier ekCalendar =
  sendMessage ekCalendar calendarIdentifierSelector

-- | title
--
-- The title of the calendar.
--
-- ObjC selector: @- title@
title :: IsEKCalendar ekCalendar => ekCalendar -> IO (Id NSString)
title ekCalendar =
  sendMessage ekCalendar titleSelector

-- | title
--
-- The title of the calendar.
--
-- ObjC selector: @- setTitle:@
setTitle :: (IsEKCalendar ekCalendar, IsNSString value) => ekCalendar -> value -> IO ()
setTitle ekCalendar value =
  sendMessage ekCalendar setTitleSelector (toNSString value)

-- | type
--
-- The type of the calendar as a EKCalendarType. This is actually based on                what source the calendar is in, as well as whether it is a subscribed calendar.
--
-- CalDAV subscribed calendars have type EKCalendarTypeCalDAV with isSubscribed = YES.
--
-- ObjC selector: @- type@
type_ :: IsEKCalendar ekCalendar => ekCalendar -> IO EKCalendarType
type_ ekCalendar =
  sendMessage ekCalendar typeSelector

-- | allowsContentModifications
--
-- Represents whether you can this add, remove, or modify items in this calendar.
--
-- ObjC selector: @- allowsContentModifications@
allowsContentModifications :: IsEKCalendar ekCalendar => ekCalendar -> IO Bool
allowsContentModifications ekCalendar =
  sendMessage ekCalendar allowsContentModificationsSelector

-- | subscribed
--
-- YES if this calendar is a subscribed calendar.
--
-- ObjC selector: @- subscribed@
subscribed :: IsEKCalendar ekCalendar => ekCalendar -> IO Bool
subscribed ekCalendar =
  sendMessage ekCalendar subscribedSelector

-- | immutable
--
-- If this is set to YES, it means you cannot modify any attributes of                the calendar or delete it. It does NOT imply that you cannot add events                 or reminders to the calendar.
--
-- ObjC selector: @- immutable@
immutable :: IsEKCalendar ekCalendar => ekCalendar -> IO Bool
immutable ekCalendar =
  sendMessage ekCalendar immutableSelector

-- | color
--
-- Returns the calendar color as a CGColorRef.
--
-- This will be nil for new calendars until you set it.
--
-- ObjC selector: @- CGColor@
cgColor :: IsEKCalendar ekCalendar => ekCalendar -> IO (Ptr ())
cgColor ekCalendar =
  sendMessage ekCalendar cgColorSelector

-- | color
--
-- Returns the calendar color as a CGColorRef.
--
-- This will be nil for new calendars until you set it.
--
-- ObjC selector: @- setCGColor:@
setCGColor :: IsEKCalendar ekCalendar => ekCalendar -> Ptr () -> IO ()
setCGColor ekCalendar value =
  sendMessage ekCalendar setCGColorSelector value

-- | color
--
-- Returns the calendar color as a NSColor.
--
-- This will be nil for new calendars until you set it.
--
-- ObjC selector: @- color@
color :: IsEKCalendar ekCalendar => ekCalendar -> IO RawId
color ekCalendar =
  sendMessage ekCalendar colorSelector

-- | color
--
-- Returns the calendar color as a NSColor.
--
-- This will be nil for new calendars until you set it.
--
-- ObjC selector: @- setColor:@
setColor :: IsEKCalendar ekCalendar => ekCalendar -> RawId -> IO ()
setColor ekCalendar value =
  sendMessage ekCalendar setColorSelector value

-- | supportedEventAvailabilities
--
-- Returns a bitfield of supported event availabilities, or EKCalendarEventAvailabilityNone                if this calendar does not support setting availability on an event.
--
-- ObjC selector: @- supportedEventAvailabilities@
supportedEventAvailabilities :: IsEKCalendar ekCalendar => ekCalendar -> IO EKCalendarEventAvailabilityMask
supportedEventAvailabilities ekCalendar =
  sendMessage ekCalendar supportedEventAvailabilitiesSelector

-- | @- allowedEntityTypes@
allowedEntityTypes :: IsEKCalendar ekCalendar => ekCalendar -> IO EKEntityMask
allowedEntityTypes ekCalendar =
  sendMessage ekCalendar allowedEntityTypesSelector

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @calendarWithEventStore:@
calendarWithEventStoreSelector :: Selector '[Id EKEventStore] (Id EKCalendar)
calendarWithEventStoreSelector = mkSelector "calendarWithEventStore:"

-- | @Selector@ for @calendarForEntityType:eventStore:@
calendarForEntityType_eventStoreSelector :: Selector '[EKEntityType, Id EKEventStore] (Id EKCalendar)
calendarForEntityType_eventStoreSelector = mkSelector "calendarForEntityType:eventStore:"

-- | @Selector@ for @source@
sourceSelector :: Selector '[] (Id EKSource)
sourceSelector = mkSelector "source"

-- | @Selector@ for @setSource:@
setSourceSelector :: Selector '[Id EKSource] ()
setSourceSelector = mkSelector "setSource:"

-- | @Selector@ for @calendarIdentifier@
calendarIdentifierSelector :: Selector '[] RawId
calendarIdentifierSelector = mkSelector "calendarIdentifier"

-- | @Selector@ for @title@
titleSelector :: Selector '[] (Id NSString)
titleSelector = mkSelector "title"

-- | @Selector@ for @setTitle:@
setTitleSelector :: Selector '[Id NSString] ()
setTitleSelector = mkSelector "setTitle:"

-- | @Selector@ for @type@
typeSelector :: Selector '[] EKCalendarType
typeSelector = mkSelector "type"

-- | @Selector@ for @allowsContentModifications@
allowsContentModificationsSelector :: Selector '[] Bool
allowsContentModificationsSelector = mkSelector "allowsContentModifications"

-- | @Selector@ for @subscribed@
subscribedSelector :: Selector '[] Bool
subscribedSelector = mkSelector "subscribed"

-- | @Selector@ for @immutable@
immutableSelector :: Selector '[] Bool
immutableSelector = mkSelector "immutable"

-- | @Selector@ for @CGColor@
cgColorSelector :: Selector '[] (Ptr ())
cgColorSelector = mkSelector "CGColor"

-- | @Selector@ for @setCGColor:@
setCGColorSelector :: Selector '[Ptr ()] ()
setCGColorSelector = mkSelector "setCGColor:"

-- | @Selector@ for @color@
colorSelector :: Selector '[] RawId
colorSelector = mkSelector "color"

-- | @Selector@ for @setColor:@
setColorSelector :: Selector '[RawId] ()
setColorSelector = mkSelector "setColor:"

-- | @Selector@ for @supportedEventAvailabilities@
supportedEventAvailabilitiesSelector :: Selector '[] EKCalendarEventAvailabilityMask
supportedEventAvailabilitiesSelector = mkSelector "supportedEventAvailabilities"

-- | @Selector@ for @allowedEntityTypes@
allowedEntityTypesSelector :: Selector '[] EKEntityMask
allowedEntityTypesSelector = mkSelector "allowedEntityTypes"

