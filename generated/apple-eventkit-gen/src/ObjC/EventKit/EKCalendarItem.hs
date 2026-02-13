{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Generated bindings for @EKCalendarItem@.
module ObjC.EventKit.EKCalendarItem
  ( EKCalendarItem
  , IsEKCalendarItem(..)
  , addAlarm
  , removeAlarm
  , addRecurrenceRule
  , removeRecurrenceRule
  , uuid
  , calendar
  , setCalendar
  , calendarItemIdentifier
  , calendarItemExternalIdentifier
  , title
  , setTitle
  , location
  , setLocation
  , notes
  , setNotes
  , url
  , setURL
  , lastModifiedDate
  , creationDate
  , timeZone
  , setTimeZone
  , hasAlarms
  , hasRecurrenceRules
  , hasAttendees
  , hasNotes
  , attendees
  , alarms
  , setAlarms
  , addAlarmSelector
  , addRecurrenceRuleSelector
  , alarmsSelector
  , attendeesSelector
  , calendarItemExternalIdentifierSelector
  , calendarItemIdentifierSelector
  , calendarSelector
  , creationDateSelector
  , hasAlarmsSelector
  , hasAttendeesSelector
  , hasNotesSelector
  , hasRecurrenceRulesSelector
  , lastModifiedDateSelector
  , locationSelector
  , notesSelector
  , removeAlarmSelector
  , removeRecurrenceRuleSelector
  , setAlarmsSelector
  , setCalendarSelector
  , setLocationSelector
  , setNotesSelector
  , setTimeZoneSelector
  , setTitleSelector
  , setURLSelector
  , timeZoneSelector
  , titleSelector
  , urlSelector
  , uuidSelector


  ) where

import Foreign.Ptr (Ptr, FunPtr)
import Foreign.C.Types

import ObjC.Runtime.Types
import ObjC.Runtime.Message (sendMessage, sendOwnedMessage, sendClassMessage, sendOwnedClassMessage)
import ObjC.Runtime.Selector (mkSelector)
import ObjC.Runtime.Class (getRequiredClass)

import ObjC.EventKit.Internal.Classes
import ObjC.Foundation.Internal.Classes

-- | addAlarm:
--
-- Adds an alarm to this item.
--
-- This method add an alarm to an item. Be warned that some calendars can only                allow a certain maximum number of alarms. When this item is saved, it will                truncate any extra alarms from the array.
--
-- ObjC selector: @- addAlarm:@
addAlarm :: (IsEKCalendarItem ekCalendarItem, IsEKAlarm alarm) => ekCalendarItem -> alarm -> IO ()
addAlarm ekCalendarItem alarm =
  sendMessage ekCalendarItem addAlarmSelector (toEKAlarm alarm)

-- | removeAlarm:
--
-- Removes an alarm from this item.
--
-- ObjC selector: @- removeAlarm:@
removeAlarm :: (IsEKCalendarItem ekCalendarItem, IsEKAlarm alarm) => ekCalendarItem -> alarm -> IO ()
removeAlarm ekCalendarItem alarm =
  sendMessage ekCalendarItem removeAlarmSelector (toEKAlarm alarm)

-- | @- addRecurrenceRule:@
addRecurrenceRule :: (IsEKCalendarItem ekCalendarItem, IsEKRecurrenceRule rule) => ekCalendarItem -> rule -> IO ()
addRecurrenceRule ekCalendarItem rule =
  sendMessage ekCalendarItem addRecurrenceRuleSelector (toEKRecurrenceRule rule)

-- | @- removeRecurrenceRule:@
removeRecurrenceRule :: (IsEKCalendarItem ekCalendarItem, IsEKRecurrenceRule rule) => ekCalendarItem -> rule -> IO ()
removeRecurrenceRule ekCalendarItem rule =
  sendMessage ekCalendarItem removeRecurrenceRuleSelector (toEKRecurrenceRule rule)

-- | UUID
--
-- This is now deprecated; use calendarItemIdentifier instead.
--
-- ObjC selector: @- UUID@
uuid :: IsEKCalendarItem ekCalendarItem => ekCalendarItem -> IO RawId
uuid ekCalendarItem =
  sendMessage ekCalendarItem uuidSelector

-- | calendar
--
-- The calendar that this calendar item belongs to.
--
-- This will be nil for new calendar items until you set it.
--
-- ObjC selector: @- calendar@
calendar :: IsEKCalendarItem ekCalendarItem => ekCalendarItem -> IO (Id EKCalendar)
calendar ekCalendarItem =
  sendMessage ekCalendarItem calendarSelector

-- | calendar
--
-- The calendar that this calendar item belongs to.
--
-- This will be nil for new calendar items until you set it.
--
-- ObjC selector: @- setCalendar:@
setCalendar :: (IsEKCalendarItem ekCalendarItem, IsEKCalendar value) => ekCalendarItem -> value -> IO ()
setCalendar ekCalendarItem value =
  sendMessage ekCalendarItem setCalendarSelector (toEKCalendar value)

-- | calendarItemIdentifier
--
-- A unique identifier for a calendar item.
--
-- Item identifiers are not sync-proof in that a full sync will lose                this identifier, so you should always have a back up plan for dealing                with a reminder that is no longer fetchable by this property, e.g. by title, etc.                Use [EKEventStore calendarItemWithIdentifier:] to look up the item by this value.
--
-- ObjC selector: @- calendarItemIdentifier@
calendarItemIdentifier :: IsEKCalendarItem ekCalendarItem => ekCalendarItem -> IO RawId
calendarItemIdentifier ekCalendarItem =
  sendMessage ekCalendarItem calendarItemIdentifierSelector

-- | calendarItemExternalIdentifier
--
-- A server-provided identifier for this calendar item
--
-- This identifier, provided by the server, allows you to reference the same event or reminder across                multiple devices. For calendars stored locally on the device, including the birthday calendar,                it simply passes through to calendarItemIdentifier.
--
-- This identifier is unique as of creation for every calendar item.  However, there are some                cases where duplicate copies of a calendar item can exist in the same database, including:                - A calendar item was imported from an ICS file into multiple calendars                - An event was created in a calendar shared with the user and the user was also invited to the event                - The user is a delegate of a calendar that also has this event                - A subscribed calendar was added to multiple accounts                In such cases, you should choose between calendar items based on other factors, such as                the calendar or source.
--
-- This identifier is the same for all occurrences of a recurring event. If you wish to differentiate                between occurrences, you may want to use the start date.
--
-- This may be nil for new calendar items that do not yet belong to a calendar.
--
-- In addition, there are two caveats for Exchange-based calendars:                - This identifier will be different between EventKit on iOS versus OS X                - This identifier will be different between devices for EKReminders
--
-- ObjC selector: @- calendarItemExternalIdentifier@
calendarItemExternalIdentifier :: IsEKCalendarItem ekCalendarItem => ekCalendarItem -> IO RawId
calendarItemExternalIdentifier ekCalendarItem =
  sendMessage ekCalendarItem calendarItemExternalIdentifierSelector

-- | title
--
-- The title of this calendar item.
--
-- This will be an empty string for new calendar items until you set it.
--
-- ObjC selector: @- title@
title :: IsEKCalendarItem ekCalendarItem => ekCalendarItem -> IO (Id NSString)
title ekCalendarItem =
  sendMessage ekCalendarItem titleSelector

-- | title
--
-- The title of this calendar item.
--
-- This will be an empty string for new calendar items until you set it.
--
-- ObjC selector: @- setTitle:@
setTitle :: (IsEKCalendarItem ekCalendarItem, IsNSString value) => ekCalendarItem -> value -> IO ()
setTitle ekCalendarItem value =
  sendMessage ekCalendarItem setTitleSelector (toNSString value)

-- | @- location@
location :: IsEKCalendarItem ekCalendarItem => ekCalendarItem -> IO (Id NSString)
location ekCalendarItem =
  sendMessage ekCalendarItem locationSelector

-- | @- setLocation:@
setLocation :: (IsEKCalendarItem ekCalendarItem, IsNSString value) => ekCalendarItem -> value -> IO ()
setLocation ekCalendarItem value =
  sendMessage ekCalendarItem setLocationSelector (toNSString value)

-- | @- notes@
notes :: IsEKCalendarItem ekCalendarItem => ekCalendarItem -> IO (Id NSString)
notes ekCalendarItem =
  sendMessage ekCalendarItem notesSelector

-- | @- setNotes:@
setNotes :: (IsEKCalendarItem ekCalendarItem, IsNSString value) => ekCalendarItem -> value -> IO ()
setNotes ekCalendarItem value =
  sendMessage ekCalendarItem setNotesSelector (toNSString value)

-- | @- URL@
url :: IsEKCalendarItem ekCalendarItem => ekCalendarItem -> IO RawId
url ekCalendarItem =
  sendMessage ekCalendarItem urlSelector

-- | @- setURL:@
setURL :: IsEKCalendarItem ekCalendarItem => ekCalendarItem -> RawId -> IO ()
setURL ekCalendarItem value =
  sendMessage ekCalendarItem setURLSelector value

-- | @- lastModifiedDate@
lastModifiedDate :: IsEKCalendarItem ekCalendarItem => ekCalendarItem -> IO (Id NSDate)
lastModifiedDate ekCalendarItem =
  sendMessage ekCalendarItem lastModifiedDateSelector

-- | @- creationDate@
creationDate :: IsEKCalendarItem ekCalendarItem => ekCalendarItem -> IO RawId
creationDate ekCalendarItem =
  sendMessage ekCalendarItem creationDateSelector

-- | @- timeZone@
timeZone :: IsEKCalendarItem ekCalendarItem => ekCalendarItem -> IO RawId
timeZone ekCalendarItem =
  sendMessage ekCalendarItem timeZoneSelector

-- | @- setTimeZone:@
setTimeZone :: IsEKCalendarItem ekCalendarItem => ekCalendarItem -> RawId -> IO ()
setTimeZone ekCalendarItem value =
  sendMessage ekCalendarItem setTimeZoneSelector value

-- | @- hasAlarms@
hasAlarms :: IsEKCalendarItem ekCalendarItem => ekCalendarItem -> IO Bool
hasAlarms ekCalendarItem =
  sendMessage ekCalendarItem hasAlarmsSelector

-- | @- hasRecurrenceRules@
hasRecurrenceRules :: IsEKCalendarItem ekCalendarItem => ekCalendarItem -> IO Bool
hasRecurrenceRules ekCalendarItem =
  sendMessage ekCalendarItem hasRecurrenceRulesSelector

-- | @- hasAttendees@
hasAttendees :: IsEKCalendarItem ekCalendarItem => ekCalendarItem -> IO Bool
hasAttendees ekCalendarItem =
  sendMessage ekCalendarItem hasAttendeesSelector

-- | @- hasNotes@
hasNotes :: IsEKCalendarItem ekCalendarItem => ekCalendarItem -> IO Bool
hasNotes ekCalendarItem =
  sendMessage ekCalendarItem hasNotesSelector

-- | @- attendees@
attendees :: IsEKCalendarItem ekCalendarItem => ekCalendarItem -> IO (Id NSArray)
attendees ekCalendarItem =
  sendMessage ekCalendarItem attendeesSelector

-- | @- alarms@
alarms :: IsEKCalendarItem ekCalendarItem => ekCalendarItem -> IO (Id NSArray)
alarms ekCalendarItem =
  sendMessage ekCalendarItem alarmsSelector

-- | @- setAlarms:@
setAlarms :: (IsEKCalendarItem ekCalendarItem, IsNSArray value) => ekCalendarItem -> value -> IO ()
setAlarms ekCalendarItem value =
  sendMessage ekCalendarItem setAlarmsSelector (toNSArray value)

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @addAlarm:@
addAlarmSelector :: Selector '[Id EKAlarm] ()
addAlarmSelector = mkSelector "addAlarm:"

-- | @Selector@ for @removeAlarm:@
removeAlarmSelector :: Selector '[Id EKAlarm] ()
removeAlarmSelector = mkSelector "removeAlarm:"

-- | @Selector@ for @addRecurrenceRule:@
addRecurrenceRuleSelector :: Selector '[Id EKRecurrenceRule] ()
addRecurrenceRuleSelector = mkSelector "addRecurrenceRule:"

-- | @Selector@ for @removeRecurrenceRule:@
removeRecurrenceRuleSelector :: Selector '[Id EKRecurrenceRule] ()
removeRecurrenceRuleSelector = mkSelector "removeRecurrenceRule:"

-- | @Selector@ for @UUID@
uuidSelector :: Selector '[] RawId
uuidSelector = mkSelector "UUID"

-- | @Selector@ for @calendar@
calendarSelector :: Selector '[] (Id EKCalendar)
calendarSelector = mkSelector "calendar"

-- | @Selector@ for @setCalendar:@
setCalendarSelector :: Selector '[Id EKCalendar] ()
setCalendarSelector = mkSelector "setCalendar:"

-- | @Selector@ for @calendarItemIdentifier@
calendarItemIdentifierSelector :: Selector '[] RawId
calendarItemIdentifierSelector = mkSelector "calendarItemIdentifier"

-- | @Selector@ for @calendarItemExternalIdentifier@
calendarItemExternalIdentifierSelector :: Selector '[] RawId
calendarItemExternalIdentifierSelector = mkSelector "calendarItemExternalIdentifier"

-- | @Selector@ for @title@
titleSelector :: Selector '[] (Id NSString)
titleSelector = mkSelector "title"

-- | @Selector@ for @setTitle:@
setTitleSelector :: Selector '[Id NSString] ()
setTitleSelector = mkSelector "setTitle:"

-- | @Selector@ for @location@
locationSelector :: Selector '[] (Id NSString)
locationSelector = mkSelector "location"

-- | @Selector@ for @setLocation:@
setLocationSelector :: Selector '[Id NSString] ()
setLocationSelector = mkSelector "setLocation:"

-- | @Selector@ for @notes@
notesSelector :: Selector '[] (Id NSString)
notesSelector = mkSelector "notes"

-- | @Selector@ for @setNotes:@
setNotesSelector :: Selector '[Id NSString] ()
setNotesSelector = mkSelector "setNotes:"

-- | @Selector@ for @URL@
urlSelector :: Selector '[] RawId
urlSelector = mkSelector "URL"

-- | @Selector@ for @setURL:@
setURLSelector :: Selector '[RawId] ()
setURLSelector = mkSelector "setURL:"

-- | @Selector@ for @lastModifiedDate@
lastModifiedDateSelector :: Selector '[] (Id NSDate)
lastModifiedDateSelector = mkSelector "lastModifiedDate"

-- | @Selector@ for @creationDate@
creationDateSelector :: Selector '[] RawId
creationDateSelector = mkSelector "creationDate"

-- | @Selector@ for @timeZone@
timeZoneSelector :: Selector '[] RawId
timeZoneSelector = mkSelector "timeZone"

-- | @Selector@ for @setTimeZone:@
setTimeZoneSelector :: Selector '[RawId] ()
setTimeZoneSelector = mkSelector "setTimeZone:"

-- | @Selector@ for @hasAlarms@
hasAlarmsSelector :: Selector '[] Bool
hasAlarmsSelector = mkSelector "hasAlarms"

-- | @Selector@ for @hasRecurrenceRules@
hasRecurrenceRulesSelector :: Selector '[] Bool
hasRecurrenceRulesSelector = mkSelector "hasRecurrenceRules"

-- | @Selector@ for @hasAttendees@
hasAttendeesSelector :: Selector '[] Bool
hasAttendeesSelector = mkSelector "hasAttendees"

-- | @Selector@ for @hasNotes@
hasNotesSelector :: Selector '[] Bool
hasNotesSelector = mkSelector "hasNotes"

-- | @Selector@ for @attendees@
attendeesSelector :: Selector '[] (Id NSArray)
attendeesSelector = mkSelector "attendees"

-- | @Selector@ for @alarms@
alarmsSelector :: Selector '[] (Id NSArray)
alarmsSelector = mkSelector "alarms"

-- | @Selector@ for @setAlarms:@
setAlarmsSelector :: Selector '[Id NSArray] ()
setAlarmsSelector = mkSelector "setAlarms:"

