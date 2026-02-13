{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Generated bindings for @CalCalendarItem@.
module ObjC.CalendarStore.CalCalendarItem
  ( CalCalendarItem
  , IsCalCalendarItem(..)
  , hasAlarm
  , nextAlarmDate
  , addAlarm
  , addAlarms
  , removeAlarm
  , removeAlarms
  , calendar
  , setCalendar
  , notes
  , setNotes
  , url
  , setUrl
  , title
  , setTitle
  , uid
  , dateStamp
  , alarms
  , setAlarms
  , addAlarmSelector
  , addAlarmsSelector
  , alarmsSelector
  , calendarSelector
  , dateStampSelector
  , hasAlarmSelector
  , nextAlarmDateSelector
  , notesSelector
  , removeAlarmSelector
  , removeAlarmsSelector
  , setAlarmsSelector
  , setCalendarSelector
  , setNotesSelector
  , setTitleSelector
  , setUrlSelector
  , titleSelector
  , uidSelector
  , urlSelector


  ) where

import Foreign.Ptr (Ptr, FunPtr)
import Foreign.C.Types

import ObjC.Runtime.Types
import ObjC.Runtime.Message (sendMessage, sendOwnedMessage, sendClassMessage, sendOwnedClassMessage)
import ObjC.Runtime.Selector (mkSelector)
import ObjC.Runtime.Class (getRequiredClass)

import ObjC.CalendarStore.Internal.Classes
import ObjC.Foundation.Internal.Classes

-- | @- hasAlarm@
hasAlarm :: IsCalCalendarItem calCalendarItem => calCalendarItem -> IO Bool
hasAlarm calCalendarItem =
  sendMessage calCalendarItem hasAlarmSelector

-- | @- nextAlarmDate@
nextAlarmDate :: IsCalCalendarItem calCalendarItem => calCalendarItem -> IO (Id NSDate)
nextAlarmDate calCalendarItem =
  sendMessage calCalendarItem nextAlarmDateSelector

-- | @- addAlarm:@
addAlarm :: (IsCalCalendarItem calCalendarItem, IsCalAlarm alarm) => calCalendarItem -> alarm -> IO ()
addAlarm calCalendarItem alarm =
  sendMessage calCalendarItem addAlarmSelector (toCalAlarm alarm)

-- | @- addAlarms:@
addAlarms :: (IsCalCalendarItem calCalendarItem, IsNSArray alarms) => calCalendarItem -> alarms -> IO ()
addAlarms calCalendarItem alarms =
  sendMessage calCalendarItem addAlarmsSelector (toNSArray alarms)

-- | @- removeAlarm:@
removeAlarm :: (IsCalCalendarItem calCalendarItem, IsCalAlarm alarm) => calCalendarItem -> alarm -> IO ()
removeAlarm calCalendarItem alarm =
  sendMessage calCalendarItem removeAlarmSelector (toCalAlarm alarm)

-- | @- removeAlarms:@
removeAlarms :: (IsCalCalendarItem calCalendarItem, IsNSArray alarms) => calCalendarItem -> alarms -> IO ()
removeAlarms calCalendarItem alarms =
  sendMessage calCalendarItem removeAlarmsSelector (toNSArray alarms)

-- | @- calendar@
calendar :: IsCalCalendarItem calCalendarItem => calCalendarItem -> IO (Id CalCalendar)
calendar calCalendarItem =
  sendMessage calCalendarItem calendarSelector

-- | @- setCalendar:@
setCalendar :: (IsCalCalendarItem calCalendarItem, IsCalCalendar value) => calCalendarItem -> value -> IO ()
setCalendar calCalendarItem value =
  sendMessage calCalendarItem setCalendarSelector (toCalCalendar value)

-- | @- notes@
notes :: IsCalCalendarItem calCalendarItem => calCalendarItem -> IO (Id NSString)
notes calCalendarItem =
  sendMessage calCalendarItem notesSelector

-- | @- setNotes:@
setNotes :: (IsCalCalendarItem calCalendarItem, IsNSString value) => calCalendarItem -> value -> IO ()
setNotes calCalendarItem value =
  sendMessage calCalendarItem setNotesSelector (toNSString value)

-- | @- url@
url :: IsCalCalendarItem calCalendarItem => calCalendarItem -> IO (Id NSURL)
url calCalendarItem =
  sendMessage calCalendarItem urlSelector

-- | @- setUrl:@
setUrl :: (IsCalCalendarItem calCalendarItem, IsNSURL value) => calCalendarItem -> value -> IO ()
setUrl calCalendarItem value =
  sendMessage calCalendarItem setUrlSelector (toNSURL value)

-- | @- title@
title :: IsCalCalendarItem calCalendarItem => calCalendarItem -> IO (Id NSString)
title calCalendarItem =
  sendMessage calCalendarItem titleSelector

-- | @- setTitle:@
setTitle :: (IsCalCalendarItem calCalendarItem, IsNSString value) => calCalendarItem -> value -> IO ()
setTitle calCalendarItem value =
  sendMessage calCalendarItem setTitleSelector (toNSString value)

-- | @- uid@
uid :: IsCalCalendarItem calCalendarItem => calCalendarItem -> IO (Id NSString)
uid calCalendarItem =
  sendMessage calCalendarItem uidSelector

-- | @- dateStamp@
dateStamp :: IsCalCalendarItem calCalendarItem => calCalendarItem -> IO (Id NSDate)
dateStamp calCalendarItem =
  sendMessage calCalendarItem dateStampSelector

-- | @- alarms@
alarms :: IsCalCalendarItem calCalendarItem => calCalendarItem -> IO (Id NSArray)
alarms calCalendarItem =
  sendMessage calCalendarItem alarmsSelector

-- | @- setAlarms:@
setAlarms :: (IsCalCalendarItem calCalendarItem, IsNSArray value) => calCalendarItem -> value -> IO ()
setAlarms calCalendarItem value =
  sendMessage calCalendarItem setAlarmsSelector (toNSArray value)

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @hasAlarm@
hasAlarmSelector :: Selector '[] Bool
hasAlarmSelector = mkSelector "hasAlarm"

-- | @Selector@ for @nextAlarmDate@
nextAlarmDateSelector :: Selector '[] (Id NSDate)
nextAlarmDateSelector = mkSelector "nextAlarmDate"

-- | @Selector@ for @addAlarm:@
addAlarmSelector :: Selector '[Id CalAlarm] ()
addAlarmSelector = mkSelector "addAlarm:"

-- | @Selector@ for @addAlarms:@
addAlarmsSelector :: Selector '[Id NSArray] ()
addAlarmsSelector = mkSelector "addAlarms:"

-- | @Selector@ for @removeAlarm:@
removeAlarmSelector :: Selector '[Id CalAlarm] ()
removeAlarmSelector = mkSelector "removeAlarm:"

-- | @Selector@ for @removeAlarms:@
removeAlarmsSelector :: Selector '[Id NSArray] ()
removeAlarmsSelector = mkSelector "removeAlarms:"

-- | @Selector@ for @calendar@
calendarSelector :: Selector '[] (Id CalCalendar)
calendarSelector = mkSelector "calendar"

-- | @Selector@ for @setCalendar:@
setCalendarSelector :: Selector '[Id CalCalendar] ()
setCalendarSelector = mkSelector "setCalendar:"

-- | @Selector@ for @notes@
notesSelector :: Selector '[] (Id NSString)
notesSelector = mkSelector "notes"

-- | @Selector@ for @setNotes:@
setNotesSelector :: Selector '[Id NSString] ()
setNotesSelector = mkSelector "setNotes:"

-- | @Selector@ for @url@
urlSelector :: Selector '[] (Id NSURL)
urlSelector = mkSelector "url"

-- | @Selector@ for @setUrl:@
setUrlSelector :: Selector '[Id NSURL] ()
setUrlSelector = mkSelector "setUrl:"

-- | @Selector@ for @title@
titleSelector :: Selector '[] (Id NSString)
titleSelector = mkSelector "title"

-- | @Selector@ for @setTitle:@
setTitleSelector :: Selector '[Id NSString] ()
setTitleSelector = mkSelector "setTitle:"

-- | @Selector@ for @uid@
uidSelector :: Selector '[] (Id NSString)
uidSelector = mkSelector "uid"

-- | @Selector@ for @dateStamp@
dateStampSelector :: Selector '[] (Id NSDate)
dateStampSelector = mkSelector "dateStamp"

-- | @Selector@ for @alarms@
alarmsSelector :: Selector '[] (Id NSArray)
alarmsSelector = mkSelector "alarms"

-- | @Selector@ for @setAlarms:@
setAlarmsSelector :: Selector '[Id NSArray] ()
setAlarmsSelector = mkSelector "setAlarms:"

