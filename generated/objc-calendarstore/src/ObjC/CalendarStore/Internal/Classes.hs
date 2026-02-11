{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}

-- | Internal module: all type tags, aliases, type classes, and
-- hierarchy instances for this framework.
--
-- Exists to break import cycles between per-class modules.
-- Import the per-class modules for the public API.
module ObjC.CalendarStore.Internal.Classes (
    module ObjC.CalendarStore.Internal.Classes,
    module ObjC.Foundation.Internal.Classes,
  ) where

import Data.Proxy (Proxy(..))
import ObjC.Runtime.Types
import ObjC.Runtime.Class (getRequiredClass)
import ObjC.Foundation.Internal.Classes

-- ---------- CalAlarm ----------

-- | Phantom type for @CalAlarm@.
data CalAlarm

instance IsObjCObject (Id CalAlarm) where
  toRawId = idToRawId
  withObjCPtr = withIdObjCPtr
  staticClass _ = getRequiredClass "CalAlarm"

class IsNSObject a => IsCalAlarm a where
  toCalAlarm :: a -> Id CalAlarm

instance IsCalAlarm (Id CalAlarm) where
  toCalAlarm = unsafeCastId

instance IsNSObject (Id CalAlarm) where
  toNSObject = unsafeCastId

-- ---------- CalAttendee ----------

-- | Phantom type for @CalAttendee@.
data CalAttendee

instance IsObjCObject (Id CalAttendee) where
  toRawId = idToRawId
  withObjCPtr = withIdObjCPtr
  staticClass _ = getRequiredClass "CalAttendee"

class IsNSObject a => IsCalAttendee a where
  toCalAttendee :: a -> Id CalAttendee

instance IsCalAttendee (Id CalAttendee) where
  toCalAttendee = unsafeCastId

instance IsNSObject (Id CalAttendee) where
  toNSObject = unsafeCastId

-- ---------- CalCalendar ----------

-- | Phantom type for @CalCalendar@.
data CalCalendar

instance IsObjCObject (Id CalCalendar) where
  toRawId = idToRawId
  withObjCPtr = withIdObjCPtr
  staticClass _ = getRequiredClass "CalCalendar"

class IsNSObject a => IsCalCalendar a where
  toCalCalendar :: a -> Id CalCalendar

instance IsCalCalendar (Id CalCalendar) where
  toCalCalendar = unsafeCastId

instance IsNSObject (Id CalCalendar) where
  toNSObject = unsafeCastId

-- ---------- CalCalendarItem ----------

-- | Phantom type for @CalCalendarItem@.
data CalCalendarItem

instance IsObjCObject (Id CalCalendarItem) where
  toRawId = idToRawId
  withObjCPtr = withIdObjCPtr
  staticClass _ = getRequiredClass "CalCalendarItem"

class IsNSObject a => IsCalCalendarItem a where
  toCalCalendarItem :: a -> Id CalCalendarItem

instance IsCalCalendarItem (Id CalCalendarItem) where
  toCalCalendarItem = unsafeCastId

instance IsNSObject (Id CalCalendarItem) where
  toNSObject = unsafeCastId

-- ---------- CalCalendarStore ----------

-- | Phantom type for @CalCalendarStore@.
data CalCalendarStore

instance IsObjCObject (Id CalCalendarStore) where
  toRawId = idToRawId
  withObjCPtr = withIdObjCPtr
  staticClass _ = getRequiredClass "CalCalendarStore"

class IsNSObject a => IsCalCalendarStore a where
  toCalCalendarStore :: a -> Id CalCalendarStore

instance IsCalCalendarStore (Id CalCalendarStore) where
  toCalCalendarStore = unsafeCastId

instance IsNSObject (Id CalCalendarStore) where
  toNSObject = unsafeCastId

-- ---------- CalNthWeekDay ----------

-- | Phantom type for @CalNthWeekDay@.
data CalNthWeekDay

instance IsObjCObject (Id CalNthWeekDay) where
  toRawId = idToRawId
  withObjCPtr = withIdObjCPtr
  staticClass _ = getRequiredClass "CalNthWeekDay"

class IsNSObject a => IsCalNthWeekDay a where
  toCalNthWeekDay :: a -> Id CalNthWeekDay

instance IsCalNthWeekDay (Id CalNthWeekDay) where
  toCalNthWeekDay = unsafeCastId

instance IsNSObject (Id CalNthWeekDay) where
  toNSObject = unsafeCastId

-- ---------- CalRecurrenceEnd ----------

-- | Phantom type for @CalRecurrenceEnd@.
data CalRecurrenceEnd

instance IsObjCObject (Id CalRecurrenceEnd) where
  toRawId = idToRawId
  withObjCPtr = withIdObjCPtr
  staticClass _ = getRequiredClass "CalRecurrenceEnd"

class IsNSObject a => IsCalRecurrenceEnd a where
  toCalRecurrenceEnd :: a -> Id CalRecurrenceEnd

instance IsCalRecurrenceEnd (Id CalRecurrenceEnd) where
  toCalRecurrenceEnd = unsafeCastId

instance IsNSObject (Id CalRecurrenceEnd) where
  toNSObject = unsafeCastId

-- ---------- CalRecurrenceRule ----------

-- | Phantom type for @CalRecurrenceRule@.
data CalRecurrenceRule

instance IsObjCObject (Id CalRecurrenceRule) where
  toRawId = idToRawId
  withObjCPtr = withIdObjCPtr
  staticClass _ = getRequiredClass "CalRecurrenceRule"

class IsNSObject a => IsCalRecurrenceRule a where
  toCalRecurrenceRule :: a -> Id CalRecurrenceRule

instance IsCalRecurrenceRule (Id CalRecurrenceRule) where
  toCalRecurrenceRule = unsafeCastId

instance IsNSObject (Id CalRecurrenceRule) where
  toNSObject = unsafeCastId

-- ---------- CalEvent ----------

-- | Phantom type for @CalEvent@.
data CalEvent

instance IsObjCObject (Id CalEvent) where
  toRawId = idToRawId
  withObjCPtr = withIdObjCPtr
  staticClass _ = getRequiredClass "CalEvent"

class IsCalCalendarItem a => IsCalEvent a where
  toCalEvent :: a -> Id CalEvent

instance IsCalEvent (Id CalEvent) where
  toCalEvent = unsafeCastId

instance IsCalCalendarItem (Id CalEvent) where
  toCalCalendarItem = unsafeCastId

instance IsNSObject (Id CalEvent) where
  toNSObject = unsafeCastId

-- ---------- CalTask ----------

-- | Phantom type for @CalTask@.
data CalTask

instance IsObjCObject (Id CalTask) where
  toRawId = idToRawId
  withObjCPtr = withIdObjCPtr
  staticClass _ = getRequiredClass "CalTask"

class IsCalCalendarItem a => IsCalTask a where
  toCalTask :: a -> Id CalTask

instance IsCalTask (Id CalTask) where
  toCalTask = unsafeCastId

instance IsCalCalendarItem (Id CalTask) where
  toCalCalendarItem = unsafeCastId

instance IsNSObject (Id CalTask) where
  toNSObject = unsafeCastId
