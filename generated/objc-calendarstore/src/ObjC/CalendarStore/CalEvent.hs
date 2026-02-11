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
  , eventSelector
  , isAllDaySelector
  , setIsAllDaySelector
  , locationSelector
  , setLocationSelector
  , recurrenceRuleSelector
  , setRecurrenceRuleSelector
  , startDateSelector
  , setStartDateSelector
  , endDateSelector
  , setEndDateSelector
  , attendeesSelector
  , isDetachedSelector
  , occurrenceSelector


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

-- | @+ event@
event :: IO RawId
event  =
  do
    cls' <- getRequiredClass "CalEvent"
    fmap (RawId . castPtr) $ sendClassMsg cls' (mkSelector "event") (retPtr retVoid) []

-- | @- isAllDay@
isAllDay :: IsCalEvent calEvent => calEvent -> IO Bool
isAllDay calEvent  =
  fmap ((/= 0) :: CULong -> Bool) $ sendMsg calEvent (mkSelector "isAllDay") retCULong []

-- | @- setIsAllDay:@
setIsAllDay :: IsCalEvent calEvent => calEvent -> Bool -> IO ()
setIsAllDay calEvent  value =
  sendMsg calEvent (mkSelector "setIsAllDay:") retVoid [argCULong (if value then 1 else 0)]

-- | @- location@
location :: IsCalEvent calEvent => calEvent -> IO (Id NSString)
location calEvent  =
  sendMsg calEvent (mkSelector "location") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- setLocation:@
setLocation :: (IsCalEvent calEvent, IsNSString value) => calEvent -> value -> IO ()
setLocation calEvent  value =
withObjCPtr value $ \raw_value ->
    sendMsg calEvent (mkSelector "setLocation:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- | @- recurrenceRule@
recurrenceRule :: IsCalEvent calEvent => calEvent -> IO (Id CalRecurrenceRule)
recurrenceRule calEvent  =
  sendMsg calEvent (mkSelector "recurrenceRule") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- setRecurrenceRule:@
setRecurrenceRule :: (IsCalEvent calEvent, IsCalRecurrenceRule value) => calEvent -> value -> IO ()
setRecurrenceRule calEvent  value =
withObjCPtr value $ \raw_value ->
    sendMsg calEvent (mkSelector "setRecurrenceRule:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- | @- startDate@
startDate :: IsCalEvent calEvent => calEvent -> IO (Id NSDate)
startDate calEvent  =
  sendMsg calEvent (mkSelector "startDate") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- setStartDate:@
setStartDate :: (IsCalEvent calEvent, IsNSDate value) => calEvent -> value -> IO ()
setStartDate calEvent  value =
withObjCPtr value $ \raw_value ->
    sendMsg calEvent (mkSelector "setStartDate:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- | @- endDate@
endDate :: IsCalEvent calEvent => calEvent -> IO (Id NSDate)
endDate calEvent  =
  sendMsg calEvent (mkSelector "endDate") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- setEndDate:@
setEndDate :: (IsCalEvent calEvent, IsNSDate value) => calEvent -> value -> IO ()
setEndDate calEvent  value =
withObjCPtr value $ \raw_value ->
    sendMsg calEvent (mkSelector "setEndDate:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- | @- attendees@
attendees :: IsCalEvent calEvent => calEvent -> IO (Id NSArray)
attendees calEvent  =
  sendMsg calEvent (mkSelector "attendees") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- isDetached@
isDetached :: IsCalEvent calEvent => calEvent -> IO Bool
isDetached calEvent  =
  fmap ((/= 0) :: CULong -> Bool) $ sendMsg calEvent (mkSelector "isDetached") retCULong []

-- | @- occurrence@
occurrence :: IsCalEvent calEvent => calEvent -> IO (Id NSDate)
occurrence calEvent  =
  sendMsg calEvent (mkSelector "occurrence") (retPtr retVoid) [] >>= retainedObject . castPtr

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @event@
eventSelector :: Selector
eventSelector = mkSelector "event"

-- | @Selector@ for @isAllDay@
isAllDaySelector :: Selector
isAllDaySelector = mkSelector "isAllDay"

-- | @Selector@ for @setIsAllDay:@
setIsAllDaySelector :: Selector
setIsAllDaySelector = mkSelector "setIsAllDay:"

-- | @Selector@ for @location@
locationSelector :: Selector
locationSelector = mkSelector "location"

-- | @Selector@ for @setLocation:@
setLocationSelector :: Selector
setLocationSelector = mkSelector "setLocation:"

-- | @Selector@ for @recurrenceRule@
recurrenceRuleSelector :: Selector
recurrenceRuleSelector = mkSelector "recurrenceRule"

-- | @Selector@ for @setRecurrenceRule:@
setRecurrenceRuleSelector :: Selector
setRecurrenceRuleSelector = mkSelector "setRecurrenceRule:"

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

-- | @Selector@ for @attendees@
attendeesSelector :: Selector
attendeesSelector = mkSelector "attendees"

-- | @Selector@ for @isDetached@
isDetachedSelector :: Selector
isDetachedSelector = mkSelector "isDetached"

-- | @Selector@ for @occurrence@
occurrenceSelector :: Selector
occurrenceSelector = mkSelector "occurrence"

