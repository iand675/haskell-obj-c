{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Generated bindings for @CalEvent@.
module ObjC.CalendarStore.CalEvent
  ( CalEvent
  , IsCalEvent(..)
  , event
  , isAllDay
  , setIsAllDay
  , location
  , setLocation
  , recurrenceRule
  , setRecurrenceRule
  , startDate
  , setStartDate
  , endDate
  , setEndDate
  , attendees
  , isDetached
  , occurrence
  , attendeesSelector
  , endDateSelector
  , eventSelector
  , isAllDaySelector
  , isDetachedSelector
  , locationSelector
  , occurrenceSelector
  , recurrenceRuleSelector
  , setEndDateSelector
  , setIsAllDaySelector
  , setLocationSelector
  , setRecurrenceRuleSelector
  , setStartDateSelector
  , startDateSelector


  ) where

import Foreign.Ptr (Ptr, FunPtr)
import Foreign.C.Types

import ObjC.Runtime.Types
import ObjC.Runtime.Message (sendMessage, sendOwnedMessage, sendClassMessage, sendOwnedClassMessage)
import ObjC.Runtime.Selector (mkSelector)
import ObjC.Runtime.Class (getRequiredClass)

import ObjC.CalendarStore.Internal.Classes
import ObjC.Foundation.Internal.Classes

-- | @+ event@
event :: IO RawId
event  =
  do
    cls' <- getRequiredClass "CalEvent"
    sendClassMessage cls' eventSelector

-- | @- isAllDay@
isAllDay :: IsCalEvent calEvent => calEvent -> IO Bool
isAllDay calEvent =
  sendMessage calEvent isAllDaySelector

-- | @- setIsAllDay:@
setIsAllDay :: IsCalEvent calEvent => calEvent -> Bool -> IO ()
setIsAllDay calEvent value =
  sendMessage calEvent setIsAllDaySelector value

-- | @- location@
location :: IsCalEvent calEvent => calEvent -> IO (Id NSString)
location calEvent =
  sendMessage calEvent locationSelector

-- | @- setLocation:@
setLocation :: (IsCalEvent calEvent, IsNSString value) => calEvent -> value -> IO ()
setLocation calEvent value =
  sendMessage calEvent setLocationSelector (toNSString value)

-- | @- recurrenceRule@
recurrenceRule :: IsCalEvent calEvent => calEvent -> IO (Id CalRecurrenceRule)
recurrenceRule calEvent =
  sendMessage calEvent recurrenceRuleSelector

-- | @- setRecurrenceRule:@
setRecurrenceRule :: (IsCalEvent calEvent, IsCalRecurrenceRule value) => calEvent -> value -> IO ()
setRecurrenceRule calEvent value =
  sendMessage calEvent setRecurrenceRuleSelector (toCalRecurrenceRule value)

-- | @- startDate@
startDate :: IsCalEvent calEvent => calEvent -> IO (Id NSDate)
startDate calEvent =
  sendMessage calEvent startDateSelector

-- | @- setStartDate:@
setStartDate :: (IsCalEvent calEvent, IsNSDate value) => calEvent -> value -> IO ()
setStartDate calEvent value =
  sendMessage calEvent setStartDateSelector (toNSDate value)

-- | @- endDate@
endDate :: IsCalEvent calEvent => calEvent -> IO (Id NSDate)
endDate calEvent =
  sendMessage calEvent endDateSelector

-- | @- setEndDate:@
setEndDate :: (IsCalEvent calEvent, IsNSDate value) => calEvent -> value -> IO ()
setEndDate calEvent value =
  sendMessage calEvent setEndDateSelector (toNSDate value)

-- | @- attendees@
attendees :: IsCalEvent calEvent => calEvent -> IO (Id NSArray)
attendees calEvent =
  sendMessage calEvent attendeesSelector

-- | @- isDetached@
isDetached :: IsCalEvent calEvent => calEvent -> IO Bool
isDetached calEvent =
  sendMessage calEvent isDetachedSelector

-- | @- occurrence@
occurrence :: IsCalEvent calEvent => calEvent -> IO (Id NSDate)
occurrence calEvent =
  sendMessage calEvent occurrenceSelector

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @event@
eventSelector :: Selector '[] RawId
eventSelector = mkSelector "event"

-- | @Selector@ for @isAllDay@
isAllDaySelector :: Selector '[] Bool
isAllDaySelector = mkSelector "isAllDay"

-- | @Selector@ for @setIsAllDay:@
setIsAllDaySelector :: Selector '[Bool] ()
setIsAllDaySelector = mkSelector "setIsAllDay:"

-- | @Selector@ for @location@
locationSelector :: Selector '[] (Id NSString)
locationSelector = mkSelector "location"

-- | @Selector@ for @setLocation:@
setLocationSelector :: Selector '[Id NSString] ()
setLocationSelector = mkSelector "setLocation:"

-- | @Selector@ for @recurrenceRule@
recurrenceRuleSelector :: Selector '[] (Id CalRecurrenceRule)
recurrenceRuleSelector = mkSelector "recurrenceRule"

-- | @Selector@ for @setRecurrenceRule:@
setRecurrenceRuleSelector :: Selector '[Id CalRecurrenceRule] ()
setRecurrenceRuleSelector = mkSelector "setRecurrenceRule:"

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

-- | @Selector@ for @attendees@
attendeesSelector :: Selector '[] (Id NSArray)
attendeesSelector = mkSelector "attendees"

-- | @Selector@ for @isDetached@
isDetachedSelector :: Selector '[] Bool
isDetachedSelector = mkSelector "isDetached"

-- | @Selector@ for @occurrence@
occurrenceSelector :: Selector '[] (Id NSDate)
occurrenceSelector = mkSelector "occurrence"

