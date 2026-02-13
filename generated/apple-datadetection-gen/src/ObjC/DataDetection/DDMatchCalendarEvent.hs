{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | An object that represents a calendar date or date range that the data detection system matches.
--
-- The DataDetection framework returns a calendar event match in a @DDMatchCalendarEvent@ object, which has only a beginning date, only an end date, or both a beginning date and an end date.
--
-- Generated bindings for @DDMatchCalendarEvent@.
module ObjC.DataDetection.DDMatchCalendarEvent
  ( DDMatchCalendarEvent
  , IsDDMatchCalendarEvent(..)
  , allDay
  , startDate
  , startTimeZone
  , endDate
  , endTimeZone
  , allDaySelector
  , endDateSelector
  , endTimeZoneSelector
  , startDateSelector
  , startTimeZoneSelector


  ) where

import Foreign.Ptr (Ptr, FunPtr)
import Foreign.C.Types

import ObjC.Runtime.Types
import ObjC.Runtime.Message (sendMessage, sendOwnedMessage, sendClassMessage, sendOwnedClassMessage)
import ObjC.Runtime.Selector (mkSelector)
import ObjC.Runtime.Class (getRequiredClass)

import ObjC.DataDetection.Internal.Classes
import ObjC.Foundation.Internal.Classes

-- | A Boolean value that indicates whether the event is an all-day event.
--
-- ObjC selector: @- allDay@
allDay :: IsDDMatchCalendarEvent ddMatchCalendarEvent => ddMatchCalendarEvent -> IO Bool
allDay ddMatchCalendarEvent =
  sendMessage ddMatchCalendarEvent allDaySelector

-- | A date that represents the start of the event.
--
-- ObjC selector: @- startDate@
startDate :: IsDDMatchCalendarEvent ddMatchCalendarEvent => ddMatchCalendarEvent -> IO (Id NSDate)
startDate ddMatchCalendarEvent =
  sendMessage ddMatchCalendarEvent startDateSelector

-- | The time zone for the event’s start date.
--
-- ObjC selector: @- startTimeZone@
startTimeZone :: IsDDMatchCalendarEvent ddMatchCalendarEvent => ddMatchCalendarEvent -> IO (Id NSTimeZone)
startTimeZone ddMatchCalendarEvent =
  sendMessage ddMatchCalendarEvent startTimeZoneSelector

-- | A date that represents the end of the event.
--
-- ObjC selector: @- endDate@
endDate :: IsDDMatchCalendarEvent ddMatchCalendarEvent => ddMatchCalendarEvent -> IO (Id NSDate)
endDate ddMatchCalendarEvent =
  sendMessage ddMatchCalendarEvent endDateSelector

-- | The time zone for the event’s end date.
--
-- ObjC selector: @- endTimeZone@
endTimeZone :: IsDDMatchCalendarEvent ddMatchCalendarEvent => ddMatchCalendarEvent -> IO (Id NSTimeZone)
endTimeZone ddMatchCalendarEvent =
  sendMessage ddMatchCalendarEvent endTimeZoneSelector

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @allDay@
allDaySelector :: Selector '[] Bool
allDaySelector = mkSelector "allDay"

-- | @Selector@ for @startDate@
startDateSelector :: Selector '[] (Id NSDate)
startDateSelector = mkSelector "startDate"

-- | @Selector@ for @startTimeZone@
startTimeZoneSelector :: Selector '[] (Id NSTimeZone)
startTimeZoneSelector = mkSelector "startTimeZone"

-- | @Selector@ for @endDate@
endDateSelector :: Selector '[] (Id NSDate)
endDateSelector = mkSelector "endDate"

-- | @Selector@ for @endTimeZone@
endTimeZoneSelector :: Selector '[] (Id NSTimeZone)
endTimeZoneSelector = mkSelector "endTimeZone"

