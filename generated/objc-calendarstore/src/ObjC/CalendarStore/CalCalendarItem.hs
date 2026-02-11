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
  , hasAlarmSelector
  , nextAlarmDateSelector
  , addAlarmSelector
  , addAlarmsSelector
  , removeAlarmSelector
  , removeAlarmsSelector
  , calendarSelector
  , setCalendarSelector
  , notesSelector
  , setNotesSelector
  , urlSelector
  , setUrlSelector
  , titleSelector
  , setTitleSelector
  , uidSelector
  , dateStampSelector
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

import ObjC.CalendarStore.Internal.Classes
import ObjC.Foundation.Internal.Classes

-- | @- hasAlarm@
hasAlarm :: IsCalCalendarItem calCalendarItem => calCalendarItem -> IO Bool
hasAlarm calCalendarItem  =
  fmap ((/= 0) :: CULong -> Bool) $ sendMsg calCalendarItem (mkSelector "hasAlarm") retCULong []

-- | @- nextAlarmDate@
nextAlarmDate :: IsCalCalendarItem calCalendarItem => calCalendarItem -> IO (Id NSDate)
nextAlarmDate calCalendarItem  =
  sendMsg calCalendarItem (mkSelector "nextAlarmDate") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- addAlarm:@
addAlarm :: (IsCalCalendarItem calCalendarItem, IsCalAlarm alarm) => calCalendarItem -> alarm -> IO ()
addAlarm calCalendarItem  alarm =
withObjCPtr alarm $ \raw_alarm ->
    sendMsg calCalendarItem (mkSelector "addAlarm:") retVoid [argPtr (castPtr raw_alarm :: Ptr ())]

-- | @- addAlarms:@
addAlarms :: (IsCalCalendarItem calCalendarItem, IsNSArray alarms) => calCalendarItem -> alarms -> IO ()
addAlarms calCalendarItem  alarms =
withObjCPtr alarms $ \raw_alarms ->
    sendMsg calCalendarItem (mkSelector "addAlarms:") retVoid [argPtr (castPtr raw_alarms :: Ptr ())]

-- | @- removeAlarm:@
removeAlarm :: (IsCalCalendarItem calCalendarItem, IsCalAlarm alarm) => calCalendarItem -> alarm -> IO ()
removeAlarm calCalendarItem  alarm =
withObjCPtr alarm $ \raw_alarm ->
    sendMsg calCalendarItem (mkSelector "removeAlarm:") retVoid [argPtr (castPtr raw_alarm :: Ptr ())]

-- | @- removeAlarms:@
removeAlarms :: (IsCalCalendarItem calCalendarItem, IsNSArray alarms) => calCalendarItem -> alarms -> IO ()
removeAlarms calCalendarItem  alarms =
withObjCPtr alarms $ \raw_alarms ->
    sendMsg calCalendarItem (mkSelector "removeAlarms:") retVoid [argPtr (castPtr raw_alarms :: Ptr ())]

-- | @- calendar@
calendar :: IsCalCalendarItem calCalendarItem => calCalendarItem -> IO (Id CalCalendar)
calendar calCalendarItem  =
  sendMsg calCalendarItem (mkSelector "calendar") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- setCalendar:@
setCalendar :: (IsCalCalendarItem calCalendarItem, IsCalCalendar value) => calCalendarItem -> value -> IO ()
setCalendar calCalendarItem  value =
withObjCPtr value $ \raw_value ->
    sendMsg calCalendarItem (mkSelector "setCalendar:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- | @- notes@
notes :: IsCalCalendarItem calCalendarItem => calCalendarItem -> IO (Id NSString)
notes calCalendarItem  =
  sendMsg calCalendarItem (mkSelector "notes") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- setNotes:@
setNotes :: (IsCalCalendarItem calCalendarItem, IsNSString value) => calCalendarItem -> value -> IO ()
setNotes calCalendarItem  value =
withObjCPtr value $ \raw_value ->
    sendMsg calCalendarItem (mkSelector "setNotes:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- | @- url@
url :: IsCalCalendarItem calCalendarItem => calCalendarItem -> IO (Id NSURL)
url calCalendarItem  =
  sendMsg calCalendarItem (mkSelector "url") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- setUrl:@
setUrl :: (IsCalCalendarItem calCalendarItem, IsNSURL value) => calCalendarItem -> value -> IO ()
setUrl calCalendarItem  value =
withObjCPtr value $ \raw_value ->
    sendMsg calCalendarItem (mkSelector "setUrl:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- | @- title@
title :: IsCalCalendarItem calCalendarItem => calCalendarItem -> IO (Id NSString)
title calCalendarItem  =
  sendMsg calCalendarItem (mkSelector "title") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- setTitle:@
setTitle :: (IsCalCalendarItem calCalendarItem, IsNSString value) => calCalendarItem -> value -> IO ()
setTitle calCalendarItem  value =
withObjCPtr value $ \raw_value ->
    sendMsg calCalendarItem (mkSelector "setTitle:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- | @- uid@
uid :: IsCalCalendarItem calCalendarItem => calCalendarItem -> IO (Id NSString)
uid calCalendarItem  =
  sendMsg calCalendarItem (mkSelector "uid") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- dateStamp@
dateStamp :: IsCalCalendarItem calCalendarItem => calCalendarItem -> IO (Id NSDate)
dateStamp calCalendarItem  =
  sendMsg calCalendarItem (mkSelector "dateStamp") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- alarms@
alarms :: IsCalCalendarItem calCalendarItem => calCalendarItem -> IO (Id NSArray)
alarms calCalendarItem  =
  sendMsg calCalendarItem (mkSelector "alarms") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- setAlarms:@
setAlarms :: (IsCalCalendarItem calCalendarItem, IsNSArray value) => calCalendarItem -> value -> IO ()
setAlarms calCalendarItem  value =
withObjCPtr value $ \raw_value ->
    sendMsg calCalendarItem (mkSelector "setAlarms:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @hasAlarm@
hasAlarmSelector :: Selector
hasAlarmSelector = mkSelector "hasAlarm"

-- | @Selector@ for @nextAlarmDate@
nextAlarmDateSelector :: Selector
nextAlarmDateSelector = mkSelector "nextAlarmDate"

-- | @Selector@ for @addAlarm:@
addAlarmSelector :: Selector
addAlarmSelector = mkSelector "addAlarm:"

-- | @Selector@ for @addAlarms:@
addAlarmsSelector :: Selector
addAlarmsSelector = mkSelector "addAlarms:"

-- | @Selector@ for @removeAlarm:@
removeAlarmSelector :: Selector
removeAlarmSelector = mkSelector "removeAlarm:"

-- | @Selector@ for @removeAlarms:@
removeAlarmsSelector :: Selector
removeAlarmsSelector = mkSelector "removeAlarms:"

-- | @Selector@ for @calendar@
calendarSelector :: Selector
calendarSelector = mkSelector "calendar"

-- | @Selector@ for @setCalendar:@
setCalendarSelector :: Selector
setCalendarSelector = mkSelector "setCalendar:"

-- | @Selector@ for @notes@
notesSelector :: Selector
notesSelector = mkSelector "notes"

-- | @Selector@ for @setNotes:@
setNotesSelector :: Selector
setNotesSelector = mkSelector "setNotes:"

-- | @Selector@ for @url@
urlSelector :: Selector
urlSelector = mkSelector "url"

-- | @Selector@ for @setUrl:@
setUrlSelector :: Selector
setUrlSelector = mkSelector "setUrl:"

-- | @Selector@ for @title@
titleSelector :: Selector
titleSelector = mkSelector "title"

-- | @Selector@ for @setTitle:@
setTitleSelector :: Selector
setTitleSelector = mkSelector "setTitle:"

-- | @Selector@ for @uid@
uidSelector :: Selector
uidSelector = mkSelector "uid"

-- | @Selector@ for @dateStamp@
dateStampSelector :: Selector
dateStampSelector = mkSelector "dateStamp"

-- | @Selector@ for @alarms@
alarmsSelector :: Selector
alarmsSelector = mkSelector "alarms"

-- | @Selector@ for @setAlarms:@
setAlarmsSelector :: Selector
setAlarmsSelector = mkSelector "setAlarms:"

