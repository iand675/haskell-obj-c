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
  , calendar
  , setCalendar
  , title
  , setTitle
  , location
  , setLocation
  , notes
  , setNotes
  , lastModifiedDate
  , hasAlarms
  , hasRecurrenceRules
  , hasAttendees
  , hasNotes
  , attendees
  , alarms
  , setAlarms
  , addAlarmSelector
  , removeAlarmSelector
  , addRecurrenceRuleSelector
  , removeRecurrenceRuleSelector
  , calendarSelector
  , setCalendarSelector
  , titleSelector
  , setTitleSelector
  , locationSelector
  , setLocationSelector
  , notesSelector
  , setNotesSelector
  , lastModifiedDateSelector
  , hasAlarmsSelector
  , hasRecurrenceRulesSelector
  , hasAttendeesSelector
  , hasNotesSelector
  , attendeesSelector
  , alarmsSelector
  , setAlarmsSelector


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

-- | addAlarm:
--
-- Adds an alarm to this item.
--
-- This method add an alarm to an item. Be warned that some calendars can only                allow a certain maximum number of alarms. When this item is saved, it will                truncate any extra alarms from the array.
--
-- ObjC selector: @- addAlarm:@
addAlarm :: (IsEKCalendarItem ekCalendarItem, IsEKAlarm alarm) => ekCalendarItem -> alarm -> IO ()
addAlarm ekCalendarItem  alarm =
withObjCPtr alarm $ \raw_alarm ->
    sendMsg ekCalendarItem (mkSelector "addAlarm:") retVoid [argPtr (castPtr raw_alarm :: Ptr ())]

-- | removeAlarm:
--
-- Removes an alarm from this item.
--
-- ObjC selector: @- removeAlarm:@
removeAlarm :: (IsEKCalendarItem ekCalendarItem, IsEKAlarm alarm) => ekCalendarItem -> alarm -> IO ()
removeAlarm ekCalendarItem  alarm =
withObjCPtr alarm $ \raw_alarm ->
    sendMsg ekCalendarItem (mkSelector "removeAlarm:") retVoid [argPtr (castPtr raw_alarm :: Ptr ())]

-- | @- addRecurrenceRule:@
addRecurrenceRule :: (IsEKCalendarItem ekCalendarItem, IsEKRecurrenceRule rule) => ekCalendarItem -> rule -> IO ()
addRecurrenceRule ekCalendarItem  rule =
withObjCPtr rule $ \raw_rule ->
    sendMsg ekCalendarItem (mkSelector "addRecurrenceRule:") retVoid [argPtr (castPtr raw_rule :: Ptr ())]

-- | @- removeRecurrenceRule:@
removeRecurrenceRule :: (IsEKCalendarItem ekCalendarItem, IsEKRecurrenceRule rule) => ekCalendarItem -> rule -> IO ()
removeRecurrenceRule ekCalendarItem  rule =
withObjCPtr rule $ \raw_rule ->
    sendMsg ekCalendarItem (mkSelector "removeRecurrenceRule:") retVoid [argPtr (castPtr raw_rule :: Ptr ())]

-- | calendar
--
-- The calendar that this calendar item belongs to.
--
-- This will be nil for new calendar items until you set it.
--
-- ObjC selector: @- calendar@
calendar :: IsEKCalendarItem ekCalendarItem => ekCalendarItem -> IO (Id EKCalendar)
calendar ekCalendarItem  =
  sendMsg ekCalendarItem (mkSelector "calendar") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | calendar
--
-- The calendar that this calendar item belongs to.
--
-- This will be nil for new calendar items until you set it.
--
-- ObjC selector: @- setCalendar:@
setCalendar :: (IsEKCalendarItem ekCalendarItem, IsEKCalendar value) => ekCalendarItem -> value -> IO ()
setCalendar ekCalendarItem  value =
withObjCPtr value $ \raw_value ->
    sendMsg ekCalendarItem (mkSelector "setCalendar:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- | title
--
-- The title of this calendar item.
--
-- This will be an empty string for new calendar items until you set it.
--
-- ObjC selector: @- title@
title :: IsEKCalendarItem ekCalendarItem => ekCalendarItem -> IO (Id NSString)
title ekCalendarItem  =
  sendMsg ekCalendarItem (mkSelector "title") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | title
--
-- The title of this calendar item.
--
-- This will be an empty string for new calendar items until you set it.
--
-- ObjC selector: @- setTitle:@
setTitle :: (IsEKCalendarItem ekCalendarItem, IsNSString value) => ekCalendarItem -> value -> IO ()
setTitle ekCalendarItem  value =
withObjCPtr value $ \raw_value ->
    sendMsg ekCalendarItem (mkSelector "setTitle:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- | @- location@
location :: IsEKCalendarItem ekCalendarItem => ekCalendarItem -> IO (Id NSString)
location ekCalendarItem  =
  sendMsg ekCalendarItem (mkSelector "location") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- setLocation:@
setLocation :: (IsEKCalendarItem ekCalendarItem, IsNSString value) => ekCalendarItem -> value -> IO ()
setLocation ekCalendarItem  value =
withObjCPtr value $ \raw_value ->
    sendMsg ekCalendarItem (mkSelector "setLocation:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- | @- notes@
notes :: IsEKCalendarItem ekCalendarItem => ekCalendarItem -> IO (Id NSString)
notes ekCalendarItem  =
  sendMsg ekCalendarItem (mkSelector "notes") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- setNotes:@
setNotes :: (IsEKCalendarItem ekCalendarItem, IsNSString value) => ekCalendarItem -> value -> IO ()
setNotes ekCalendarItem  value =
withObjCPtr value $ \raw_value ->
    sendMsg ekCalendarItem (mkSelector "setNotes:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- | @- lastModifiedDate@
lastModifiedDate :: IsEKCalendarItem ekCalendarItem => ekCalendarItem -> IO (Id NSDate)
lastModifiedDate ekCalendarItem  =
  sendMsg ekCalendarItem (mkSelector "lastModifiedDate") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- hasAlarms@
hasAlarms :: IsEKCalendarItem ekCalendarItem => ekCalendarItem -> IO Bool
hasAlarms ekCalendarItem  =
  fmap ((/= 0) :: CULong -> Bool) $ sendMsg ekCalendarItem (mkSelector "hasAlarms") retCULong []

-- | @- hasRecurrenceRules@
hasRecurrenceRules :: IsEKCalendarItem ekCalendarItem => ekCalendarItem -> IO Bool
hasRecurrenceRules ekCalendarItem  =
  fmap ((/= 0) :: CULong -> Bool) $ sendMsg ekCalendarItem (mkSelector "hasRecurrenceRules") retCULong []

-- | @- hasAttendees@
hasAttendees :: IsEKCalendarItem ekCalendarItem => ekCalendarItem -> IO Bool
hasAttendees ekCalendarItem  =
  fmap ((/= 0) :: CULong -> Bool) $ sendMsg ekCalendarItem (mkSelector "hasAttendees") retCULong []

-- | @- hasNotes@
hasNotes :: IsEKCalendarItem ekCalendarItem => ekCalendarItem -> IO Bool
hasNotes ekCalendarItem  =
  fmap ((/= 0) :: CULong -> Bool) $ sendMsg ekCalendarItem (mkSelector "hasNotes") retCULong []

-- | @- attendees@
attendees :: IsEKCalendarItem ekCalendarItem => ekCalendarItem -> IO (Id NSArray)
attendees ekCalendarItem  =
  sendMsg ekCalendarItem (mkSelector "attendees") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- alarms@
alarms :: IsEKCalendarItem ekCalendarItem => ekCalendarItem -> IO (Id NSArray)
alarms ekCalendarItem  =
  sendMsg ekCalendarItem (mkSelector "alarms") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- setAlarms:@
setAlarms :: (IsEKCalendarItem ekCalendarItem, IsNSArray value) => ekCalendarItem -> value -> IO ()
setAlarms ekCalendarItem  value =
withObjCPtr value $ \raw_value ->
    sendMsg ekCalendarItem (mkSelector "setAlarms:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @addAlarm:@
addAlarmSelector :: Selector
addAlarmSelector = mkSelector "addAlarm:"

-- | @Selector@ for @removeAlarm:@
removeAlarmSelector :: Selector
removeAlarmSelector = mkSelector "removeAlarm:"

-- | @Selector@ for @addRecurrenceRule:@
addRecurrenceRuleSelector :: Selector
addRecurrenceRuleSelector = mkSelector "addRecurrenceRule:"

-- | @Selector@ for @removeRecurrenceRule:@
removeRecurrenceRuleSelector :: Selector
removeRecurrenceRuleSelector = mkSelector "removeRecurrenceRule:"

-- | @Selector@ for @calendar@
calendarSelector :: Selector
calendarSelector = mkSelector "calendar"

-- | @Selector@ for @setCalendar:@
setCalendarSelector :: Selector
setCalendarSelector = mkSelector "setCalendar:"

-- | @Selector@ for @title@
titleSelector :: Selector
titleSelector = mkSelector "title"

-- | @Selector@ for @setTitle:@
setTitleSelector :: Selector
setTitleSelector = mkSelector "setTitle:"

-- | @Selector@ for @location@
locationSelector :: Selector
locationSelector = mkSelector "location"

-- | @Selector@ for @setLocation:@
setLocationSelector :: Selector
setLocationSelector = mkSelector "setLocation:"

-- | @Selector@ for @notes@
notesSelector :: Selector
notesSelector = mkSelector "notes"

-- | @Selector@ for @setNotes:@
setNotesSelector :: Selector
setNotesSelector = mkSelector "setNotes:"

-- | @Selector@ for @lastModifiedDate@
lastModifiedDateSelector :: Selector
lastModifiedDateSelector = mkSelector "lastModifiedDate"

-- | @Selector@ for @hasAlarms@
hasAlarmsSelector :: Selector
hasAlarmsSelector = mkSelector "hasAlarms"

-- | @Selector@ for @hasRecurrenceRules@
hasRecurrenceRulesSelector :: Selector
hasRecurrenceRulesSelector = mkSelector "hasRecurrenceRules"

-- | @Selector@ for @hasAttendees@
hasAttendeesSelector :: Selector
hasAttendeesSelector = mkSelector "hasAttendees"

-- | @Selector@ for @hasNotes@
hasNotesSelector :: Selector
hasNotesSelector = mkSelector "hasNotes"

-- | @Selector@ for @attendees@
attendeesSelector :: Selector
attendeesSelector = mkSelector "attendees"

-- | @Selector@ for @alarms@
alarmsSelector :: Selector
alarmsSelector = mkSelector "alarms"

-- | @Selector@ for @setAlarms:@
setAlarmsSelector :: Selector
setAlarmsSelector = mkSelector "setAlarms:"

