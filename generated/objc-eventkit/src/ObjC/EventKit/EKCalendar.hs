{-# LANGUAGE PatternSynonyms #-}
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
  , title
  , setTitle
  , type_
  , allowsContentModifications
  , subscribed
  , immutable
  , cgColor
  , setCGColor
  , supportedEventAvailabilities
  , allowedEntityTypes
  , calendarWithEventStoreSelector
  , calendarForEntityType_eventStoreSelector
  , sourceSelector
  , setSourceSelector
  , titleSelector
  , setTitleSelector
  , typeSelector
  , allowsContentModificationsSelector
  , subscribedSelector
  , immutableSelector
  , cgColorSelector
  , setCGColorSelector
  , supportedEventAvailabilitiesSelector
  , allowedEntityTypesSelector

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

-- | @+ calendarWithEventStore:@
calendarWithEventStore :: IsEKEventStore eventStore => eventStore -> IO (Id EKCalendar)
calendarWithEventStore eventStore =
  do
    cls' <- getRequiredClass "EKCalendar"
    withObjCPtr eventStore $ \raw_eventStore ->
      sendClassMsg cls' (mkSelector "calendarWithEventStore:") (retPtr retVoid) [argPtr (castPtr raw_eventStore :: Ptr ())] >>= retainedObject . castPtr

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
    withObjCPtr eventStore $ \raw_eventStore ->
      sendClassMsg cls' (mkSelector "calendarForEntityType:eventStore:") (retPtr retVoid) [argCULong (coerce entityType), argPtr (castPtr raw_eventStore :: Ptr ())] >>= retainedObject . castPtr

-- | source
--
-- The source representing the 'account' this calendar belongs to.                This is only settable when initially creating a calendar and then                effectively read-only after that. That is, you can create a calendar,                 but you cannot move it to another source.
--
-- This will be nil for new calendars until you set it.
--
-- ObjC selector: @- source@
source :: IsEKCalendar ekCalendar => ekCalendar -> IO (Id EKSource)
source ekCalendar  =
  sendMsg ekCalendar (mkSelector "source") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | source
--
-- The source representing the 'account' this calendar belongs to.                This is only settable when initially creating a calendar and then                effectively read-only after that. That is, you can create a calendar,                 but you cannot move it to another source.
--
-- This will be nil for new calendars until you set it.
--
-- ObjC selector: @- setSource:@
setSource :: (IsEKCalendar ekCalendar, IsEKSource value) => ekCalendar -> value -> IO ()
setSource ekCalendar  value =
withObjCPtr value $ \raw_value ->
    sendMsg ekCalendar (mkSelector "setSource:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- | title
--
-- The title of the calendar.
--
-- ObjC selector: @- title@
title :: IsEKCalendar ekCalendar => ekCalendar -> IO (Id NSString)
title ekCalendar  =
  sendMsg ekCalendar (mkSelector "title") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | title
--
-- The title of the calendar.
--
-- ObjC selector: @- setTitle:@
setTitle :: (IsEKCalendar ekCalendar, IsNSString value) => ekCalendar -> value -> IO ()
setTitle ekCalendar  value =
withObjCPtr value $ \raw_value ->
    sendMsg ekCalendar (mkSelector "setTitle:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- | type
--
-- The type of the calendar as a EKCalendarType. This is actually based on                what source the calendar is in, as well as whether it is a subscribed calendar.
--
-- CalDAV subscribed calendars have type EKCalendarTypeCalDAV with isSubscribed = YES.
--
-- ObjC selector: @- type@
type_ :: IsEKCalendar ekCalendar => ekCalendar -> IO EKCalendarType
type_ ekCalendar  =
  fmap (coerce :: CLong -> EKCalendarType) $ sendMsg ekCalendar (mkSelector "type") retCLong []

-- | allowsContentModifications
--
-- Represents whether you can this add, remove, or modify items in this calendar.
--
-- ObjC selector: @- allowsContentModifications@
allowsContentModifications :: IsEKCalendar ekCalendar => ekCalendar -> IO Bool
allowsContentModifications ekCalendar  =
  fmap ((/= 0) :: CULong -> Bool) $ sendMsg ekCalendar (mkSelector "allowsContentModifications") retCULong []

-- | subscribed
--
-- YES if this calendar is a subscribed calendar.
--
-- ObjC selector: @- subscribed@
subscribed :: IsEKCalendar ekCalendar => ekCalendar -> IO Bool
subscribed ekCalendar  =
  fmap ((/= 0) :: CULong -> Bool) $ sendMsg ekCalendar (mkSelector "subscribed") retCULong []

-- | immutable
--
-- If this is set to YES, it means you cannot modify any attributes of                the calendar or delete it. It does NOT imply that you cannot add events                 or reminders to the calendar.
--
-- ObjC selector: @- immutable@
immutable :: IsEKCalendar ekCalendar => ekCalendar -> IO Bool
immutable ekCalendar  =
  fmap ((/= 0) :: CULong -> Bool) $ sendMsg ekCalendar (mkSelector "immutable") retCULong []

-- | color
--
-- Returns the calendar color as a CGColorRef.
--
-- This will be nil for new calendars until you set it.
--
-- ObjC selector: @- CGColor@
cgColor :: IsEKCalendar ekCalendar => ekCalendar -> IO (Ptr ())
cgColor ekCalendar  =
  fmap castPtr $ sendMsg ekCalendar (mkSelector "CGColor") (retPtr retVoid) []

-- | color
--
-- Returns the calendar color as a CGColorRef.
--
-- This will be nil for new calendars until you set it.
--
-- ObjC selector: @- setCGColor:@
setCGColor :: IsEKCalendar ekCalendar => ekCalendar -> Ptr () -> IO ()
setCGColor ekCalendar  value =
  sendMsg ekCalendar (mkSelector "setCGColor:") retVoid [argPtr value]

-- | supportedEventAvailabilities
--
-- Returns a bitfield of supported event availabilities, or EKCalendarEventAvailabilityNone                if this calendar does not support setting availability on an event.
--
-- ObjC selector: @- supportedEventAvailabilities@
supportedEventAvailabilities :: IsEKCalendar ekCalendar => ekCalendar -> IO EKCalendarEventAvailabilityMask
supportedEventAvailabilities ekCalendar  =
  fmap (coerce :: CULong -> EKCalendarEventAvailabilityMask) $ sendMsg ekCalendar (mkSelector "supportedEventAvailabilities") retCULong []

-- | @- allowedEntityTypes@
allowedEntityTypes :: IsEKCalendar ekCalendar => ekCalendar -> IO EKEntityMask
allowedEntityTypes ekCalendar  =
  fmap (coerce :: CULong -> EKEntityMask) $ sendMsg ekCalendar (mkSelector "allowedEntityTypes") retCULong []

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @calendarWithEventStore:@
calendarWithEventStoreSelector :: Selector
calendarWithEventStoreSelector = mkSelector "calendarWithEventStore:"

-- | @Selector@ for @calendarForEntityType:eventStore:@
calendarForEntityType_eventStoreSelector :: Selector
calendarForEntityType_eventStoreSelector = mkSelector "calendarForEntityType:eventStore:"

-- | @Selector@ for @source@
sourceSelector :: Selector
sourceSelector = mkSelector "source"

-- | @Selector@ for @setSource:@
setSourceSelector :: Selector
setSourceSelector = mkSelector "setSource:"

-- | @Selector@ for @title@
titleSelector :: Selector
titleSelector = mkSelector "title"

-- | @Selector@ for @setTitle:@
setTitleSelector :: Selector
setTitleSelector = mkSelector "setTitle:"

-- | @Selector@ for @type@
typeSelector :: Selector
typeSelector = mkSelector "type"

-- | @Selector@ for @allowsContentModifications@
allowsContentModificationsSelector :: Selector
allowsContentModificationsSelector = mkSelector "allowsContentModifications"

-- | @Selector@ for @subscribed@
subscribedSelector :: Selector
subscribedSelector = mkSelector "subscribed"

-- | @Selector@ for @immutable@
immutableSelector :: Selector
immutableSelector = mkSelector "immutable"

-- | @Selector@ for @CGColor@
cgColorSelector :: Selector
cgColorSelector = mkSelector "CGColor"

-- | @Selector@ for @setCGColor:@
setCGColorSelector :: Selector
setCGColorSelector = mkSelector "setCGColor:"

-- | @Selector@ for @supportedEventAvailabilities@
supportedEventAvailabilitiesSelector :: Selector
supportedEventAvailabilitiesSelector = mkSelector "supportedEventAvailabilities"

-- | @Selector@ for @allowedEntityTypes@
allowedEntityTypesSelector :: Selector
allowedEntityTypesSelector = mkSelector "allowedEntityTypes"

