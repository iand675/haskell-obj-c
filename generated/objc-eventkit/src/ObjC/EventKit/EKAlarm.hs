{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | EKAlarm
--
-- The EKAlarm class provides an interface for accessing and manipulating calendar event alarms.
--
-- The EKAlarm class represents alarms on an event. An alarm can be relative (e.g. 15 mins before)                     or absolute (specific time).
--
-- Generated bindings for @EKAlarm@.
module ObjC.EventKit.EKAlarm
  ( EKAlarm
  , IsEKAlarm(..)
  , alarmWithAbsoluteDate
  , alarmWithRelativeOffset
  , relativeOffset
  , setRelativeOffset
  , absoluteDate
  , setAbsoluteDate
  , structuredLocation
  , setStructuredLocation
  , proximity
  , setProximity
  , type_
  , alarmWithAbsoluteDateSelector
  , alarmWithRelativeOffsetSelector
  , relativeOffsetSelector
  , setRelativeOffsetSelector
  , absoluteDateSelector
  , setAbsoluteDateSelector
  , structuredLocationSelector
  , setStructuredLocationSelector
  , proximitySelector
  , setProximitySelector
  , typeSelector

  -- * Enum types
  , EKAlarmProximity(EKAlarmProximity)
  , pattern EKAlarmProximityNone
  , pattern EKAlarmProximityEnter
  , pattern EKAlarmProximityLeave
  , EKAlarmType(EKAlarmType)
  , pattern EKAlarmTypeDisplay
  , pattern EKAlarmTypeAudio
  , pattern EKAlarmTypeProcedure
  , pattern EKAlarmTypeEmail

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

-- | alarmWithAbsoluteDate:
--
-- Creates a new autoreleased alarm with an absolute trigger time.
--
-- @date@ — The date the alarm should fire.
--
-- ObjC selector: @+ alarmWithAbsoluteDate:@
alarmWithAbsoluteDate :: IsNSDate date => date -> IO (Id EKAlarm)
alarmWithAbsoluteDate date =
  do
    cls' <- getRequiredClass "EKAlarm"
    withObjCPtr date $ \raw_date ->
      sendClassMsg cls' (mkSelector "alarmWithAbsoluteDate:") (retPtr retVoid) [argPtr (castPtr raw_date :: Ptr ())] >>= retainedObject . castPtr

-- | alarmWithRelativeOffset:
--
-- Creates a new autoreleased alarm with a relative trigger time.
--
-- Creates a new autoreleased alarm with a relative trigger time. This offset                is added to the start date of the event.
--
-- @offset@ — The offset from the event start that the alarm should fire.
--
-- ObjC selector: @+ alarmWithRelativeOffset:@
alarmWithRelativeOffset :: CDouble -> IO (Id EKAlarm)
alarmWithRelativeOffset offset =
  do
    cls' <- getRequiredClass "EKAlarm"
    sendClassMsg cls' (mkSelector "alarmWithRelativeOffset:") (retPtr retVoid) [argCDouble (fromIntegral offset)] >>= retainedObject . castPtr

-- | relativeOffset
--
-- Specifies a relative offset from an event start date to fire an alarm.
--
-- Set this property to an appropriate negative value to establish an alarm trigger                relative to the start date/time of an event. Setting this clears any existing                date trigger.
--
-- ObjC selector: @- relativeOffset@
relativeOffset :: IsEKAlarm ekAlarm => ekAlarm -> IO CDouble
relativeOffset ekAlarm  =
  sendMsg ekAlarm (mkSelector "relativeOffset") retCDouble []

-- | relativeOffset
--
-- Specifies a relative offset from an event start date to fire an alarm.
--
-- Set this property to an appropriate negative value to establish an alarm trigger                relative to the start date/time of an event. Setting this clears any existing                date trigger.
--
-- ObjC selector: @- setRelativeOffset:@
setRelativeOffset :: IsEKAlarm ekAlarm => ekAlarm -> CDouble -> IO ()
setRelativeOffset ekAlarm  value =
  sendMsg ekAlarm (mkSelector "setRelativeOffset:") retVoid [argCDouble (fromIntegral value)]

-- | absoluteDate
--
-- Represents an alarm that fires at a specific date.
--
-- Set this property to a date to establish an absolute alarm trigger. Setting this                clears any relative interval trigger.
--
-- ObjC selector: @- absoluteDate@
absoluteDate :: IsEKAlarm ekAlarm => ekAlarm -> IO (Id NSDate)
absoluteDate ekAlarm  =
  sendMsg ekAlarm (mkSelector "absoluteDate") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | absoluteDate
--
-- Represents an alarm that fires at a specific date.
--
-- Set this property to a date to establish an absolute alarm trigger. Setting this                clears any relative interval trigger.
--
-- ObjC selector: @- setAbsoluteDate:@
setAbsoluteDate :: (IsEKAlarm ekAlarm, IsNSDate value) => ekAlarm -> value -> IO ()
setAbsoluteDate ekAlarm  value =
withObjCPtr value $ \raw_value ->
    sendMsg ekAlarm (mkSelector "setAbsoluteDate:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- | structuredLocation
--
-- Allows you to set a structured location (a location with a potential geo-coordinate)                on an alarm. This is used in conjunction with proximity to do geofence-based                triggering of reminders.
--
-- ObjC selector: @- structuredLocation@
structuredLocation :: IsEKAlarm ekAlarm => ekAlarm -> IO (Id EKStructuredLocation)
structuredLocation ekAlarm  =
  sendMsg ekAlarm (mkSelector "structuredLocation") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | structuredLocation
--
-- Allows you to set a structured location (a location with a potential geo-coordinate)                on an alarm. This is used in conjunction with proximity to do geofence-based                triggering of reminders.
--
-- ObjC selector: @- setStructuredLocation:@
setStructuredLocation :: (IsEKAlarm ekAlarm, IsEKStructuredLocation value) => ekAlarm -> value -> IO ()
setStructuredLocation ekAlarm  value =
withObjCPtr value $ \raw_value ->
    sendMsg ekAlarm (mkSelector "setStructuredLocation:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- | proximity
--
-- Defines whether this alarm triggers via entering/exiting a geofence as defined by                structuredLocation.
--
-- ObjC selector: @- proximity@
proximity :: IsEKAlarm ekAlarm => ekAlarm -> IO EKAlarmProximity
proximity ekAlarm  =
  fmap (coerce :: CLong -> EKAlarmProximity) $ sendMsg ekAlarm (mkSelector "proximity") retCLong []

-- | proximity
--
-- Defines whether this alarm triggers via entering/exiting a geofence as defined by                structuredLocation.
--
-- ObjC selector: @- setProximity:@
setProximity :: IsEKAlarm ekAlarm => ekAlarm -> EKAlarmProximity -> IO ()
setProximity ekAlarm  value =
  sendMsg ekAlarm (mkSelector "setProximity:") retVoid [argCLong (coerce value)]

-- | type
--
-- The type of alarm, based on the action taken when triggering the alarm.
--
-- This field is read-only; to change the type of alarm, set emailAddress for EKAlarmTypeEmail,             soundName for EKAlarmTypeAudio or url for EKAlarmTypeProcedure.             Setting all of those to nil will change it to EKAlarmTypeDisplay.
--
-- ObjC selector: @- type@
type_ :: IsEKAlarm ekAlarm => ekAlarm -> IO EKAlarmType
type_ ekAlarm  =
  fmap (coerce :: CLong -> EKAlarmType) $ sendMsg ekAlarm (mkSelector "type") retCLong []

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @alarmWithAbsoluteDate:@
alarmWithAbsoluteDateSelector :: Selector
alarmWithAbsoluteDateSelector = mkSelector "alarmWithAbsoluteDate:"

-- | @Selector@ for @alarmWithRelativeOffset:@
alarmWithRelativeOffsetSelector :: Selector
alarmWithRelativeOffsetSelector = mkSelector "alarmWithRelativeOffset:"

-- | @Selector@ for @relativeOffset@
relativeOffsetSelector :: Selector
relativeOffsetSelector = mkSelector "relativeOffset"

-- | @Selector@ for @setRelativeOffset:@
setRelativeOffsetSelector :: Selector
setRelativeOffsetSelector = mkSelector "setRelativeOffset:"

-- | @Selector@ for @absoluteDate@
absoluteDateSelector :: Selector
absoluteDateSelector = mkSelector "absoluteDate"

-- | @Selector@ for @setAbsoluteDate:@
setAbsoluteDateSelector :: Selector
setAbsoluteDateSelector = mkSelector "setAbsoluteDate:"

-- | @Selector@ for @structuredLocation@
structuredLocationSelector :: Selector
structuredLocationSelector = mkSelector "structuredLocation"

-- | @Selector@ for @setStructuredLocation:@
setStructuredLocationSelector :: Selector
setStructuredLocationSelector = mkSelector "setStructuredLocation:"

-- | @Selector@ for @proximity@
proximitySelector :: Selector
proximitySelector = mkSelector "proximity"

-- | @Selector@ for @setProximity:@
setProximitySelector :: Selector
setProximitySelector = mkSelector "setProximity:"

-- | @Selector@ for @type@
typeSelector :: Selector
typeSelector = mkSelector "type"

