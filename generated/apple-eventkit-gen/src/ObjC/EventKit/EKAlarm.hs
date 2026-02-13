{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE DataKinds #-}
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
  , emailAddress
  , setEmailAddress
  , soundName
  , setSoundName
  , url
  , setUrl
  , absoluteDateSelector
  , alarmWithAbsoluteDateSelector
  , alarmWithRelativeOffsetSelector
  , emailAddressSelector
  , proximitySelector
  , relativeOffsetSelector
  , setAbsoluteDateSelector
  , setEmailAddressSelector
  , setProximitySelector
  , setRelativeOffsetSelector
  , setSoundNameSelector
  , setStructuredLocationSelector
  , setUrlSelector
  , soundNameSelector
  , structuredLocationSelector
  , typeSelector
  , urlSelector

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

import Foreign.Ptr (Ptr, FunPtr)
import Foreign.C.Types

import ObjC.Runtime.Types
import ObjC.Runtime.Message (sendMessage, sendOwnedMessage, sendClassMessage, sendOwnedClassMessage)
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
    sendClassMessage cls' alarmWithAbsoluteDateSelector (toNSDate date)

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
    sendClassMessage cls' alarmWithRelativeOffsetSelector offset

-- | relativeOffset
--
-- Specifies a relative offset from an event start date to fire an alarm.
--
-- Set this property to an appropriate negative value to establish an alarm trigger                relative to the start date/time of an event. Setting this clears any existing                date trigger.
--
-- ObjC selector: @- relativeOffset@
relativeOffset :: IsEKAlarm ekAlarm => ekAlarm -> IO CDouble
relativeOffset ekAlarm =
  sendMessage ekAlarm relativeOffsetSelector

-- | relativeOffset
--
-- Specifies a relative offset from an event start date to fire an alarm.
--
-- Set this property to an appropriate negative value to establish an alarm trigger                relative to the start date/time of an event. Setting this clears any existing                date trigger.
--
-- ObjC selector: @- setRelativeOffset:@
setRelativeOffset :: IsEKAlarm ekAlarm => ekAlarm -> CDouble -> IO ()
setRelativeOffset ekAlarm value =
  sendMessage ekAlarm setRelativeOffsetSelector value

-- | absoluteDate
--
-- Represents an alarm that fires at a specific date.
--
-- Set this property to a date to establish an absolute alarm trigger. Setting this                clears any relative interval trigger.
--
-- ObjC selector: @- absoluteDate@
absoluteDate :: IsEKAlarm ekAlarm => ekAlarm -> IO (Id NSDate)
absoluteDate ekAlarm =
  sendMessage ekAlarm absoluteDateSelector

-- | absoluteDate
--
-- Represents an alarm that fires at a specific date.
--
-- Set this property to a date to establish an absolute alarm trigger. Setting this                clears any relative interval trigger.
--
-- ObjC selector: @- setAbsoluteDate:@
setAbsoluteDate :: (IsEKAlarm ekAlarm, IsNSDate value) => ekAlarm -> value -> IO ()
setAbsoluteDate ekAlarm value =
  sendMessage ekAlarm setAbsoluteDateSelector (toNSDate value)

-- | structuredLocation
--
-- Allows you to set a structured location (a location with a potential geo-coordinate)                on an alarm. This is used in conjunction with proximity to do geofence-based                triggering of reminders.
--
-- ObjC selector: @- structuredLocation@
structuredLocation :: IsEKAlarm ekAlarm => ekAlarm -> IO (Id EKStructuredLocation)
structuredLocation ekAlarm =
  sendMessage ekAlarm structuredLocationSelector

-- | structuredLocation
--
-- Allows you to set a structured location (a location with a potential geo-coordinate)                on an alarm. This is used in conjunction with proximity to do geofence-based                triggering of reminders.
--
-- ObjC selector: @- setStructuredLocation:@
setStructuredLocation :: (IsEKAlarm ekAlarm, IsEKStructuredLocation value) => ekAlarm -> value -> IO ()
setStructuredLocation ekAlarm value =
  sendMessage ekAlarm setStructuredLocationSelector (toEKStructuredLocation value)

-- | proximity
--
-- Defines whether this alarm triggers via entering/exiting a geofence as defined by                structuredLocation.
--
-- ObjC selector: @- proximity@
proximity :: IsEKAlarm ekAlarm => ekAlarm -> IO EKAlarmProximity
proximity ekAlarm =
  sendMessage ekAlarm proximitySelector

-- | proximity
--
-- Defines whether this alarm triggers via entering/exiting a geofence as defined by                structuredLocation.
--
-- ObjC selector: @- setProximity:@
setProximity :: IsEKAlarm ekAlarm => ekAlarm -> EKAlarmProximity -> IO ()
setProximity ekAlarm value =
  sendMessage ekAlarm setProximitySelector value

-- | type
--
-- The type of alarm, based on the action taken when triggering the alarm.
--
-- This field is read-only; to change the type of alarm, set emailAddress for EKAlarmTypeEmail,             soundName for EKAlarmTypeAudio or url for EKAlarmTypeProcedure.             Setting all of those to nil will change it to EKAlarmTypeDisplay.
--
-- ObjC selector: @- type@
type_ :: IsEKAlarm ekAlarm => ekAlarm -> IO EKAlarmType
type_ ekAlarm =
  sendMessage ekAlarm typeSelector

-- | emailAddress
--
-- An email address that is the recipient of an email alarm, which is an alarm that triggers an email message.
--
-- When you set the emailAddress property, the action property is set to EKAlarmTypeEmail,             and the soundName and url properties are set to nil.
--
-- ObjC selector: @- emailAddress@
emailAddress :: IsEKAlarm ekAlarm => ekAlarm -> IO RawId
emailAddress ekAlarm =
  sendMessage ekAlarm emailAddressSelector

-- | emailAddress
--
-- An email address that is the recipient of an email alarm, which is an alarm that triggers an email message.
--
-- When you set the emailAddress property, the action property is set to EKAlarmTypeEmail,             and the soundName and url properties are set to nil.
--
-- ObjC selector: @- setEmailAddress:@
setEmailAddress :: IsEKAlarm ekAlarm => ekAlarm -> RawId -> IO ()
setEmailAddress ekAlarm value =
  sendMessage ekAlarm setEmailAddressSelector value

-- | soundName
--
-- The name of the sound to play when the alarm triggers.
--
-- The value of this property is the name of a system sound that can be used with             the soundNamed: class method to create an NSSound object. When you set the soundName property,             the action property is set to EKAlarmTypeAudio, and the emailAddress and url properties are set to nil.
--
-- ObjC selector: @- soundName@
soundName :: IsEKAlarm ekAlarm => ekAlarm -> IO RawId
soundName ekAlarm =
  sendMessage ekAlarm soundNameSelector

-- | soundName
--
-- The name of the sound to play when the alarm triggers.
--
-- The value of this property is the name of a system sound that can be used with             the soundNamed: class method to create an NSSound object. When you set the soundName property,             the action property is set to EKAlarmTypeAudio, and the emailAddress and url properties are set to nil.
--
-- ObjC selector: @- setSoundName:@
setSoundName :: IsEKAlarm ekAlarm => ekAlarm -> RawId -> IO ()
setSoundName ekAlarm value =
  sendMessage ekAlarm setSoundNameSelector value

-- | url
--
-- The URL to open when the alarm triggers.
--
-- When you set the url property, the action property is set to EKAlarmTypeProcedure,             and the emailAddress and soundName properties are set to nil.             Note: Starting with OS X 10.9, it is not possible to create new procedure alarms or view URLs for existing procedure alarms.             Trying to save or modify a procedure alarm will result in a save error.             Editing other aspects of events or reminders that have existing procedure alarms is allowed as long as the alarm isn't modified.
--
-- ObjC selector: @- url@
url :: IsEKAlarm ekAlarm => ekAlarm -> IO RawId
url ekAlarm =
  sendMessage ekAlarm urlSelector

-- | url
--
-- The URL to open when the alarm triggers.
--
-- When you set the url property, the action property is set to EKAlarmTypeProcedure,             and the emailAddress and soundName properties are set to nil.             Note: Starting with OS X 10.9, it is not possible to create new procedure alarms or view URLs for existing procedure alarms.             Trying to save or modify a procedure alarm will result in a save error.             Editing other aspects of events or reminders that have existing procedure alarms is allowed as long as the alarm isn't modified.
--
-- ObjC selector: @- setUrl:@
setUrl :: IsEKAlarm ekAlarm => ekAlarm -> RawId -> IO ()
setUrl ekAlarm value =
  sendMessage ekAlarm setUrlSelector value

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @alarmWithAbsoluteDate:@
alarmWithAbsoluteDateSelector :: Selector '[Id NSDate] (Id EKAlarm)
alarmWithAbsoluteDateSelector = mkSelector "alarmWithAbsoluteDate:"

-- | @Selector@ for @alarmWithRelativeOffset:@
alarmWithRelativeOffsetSelector :: Selector '[CDouble] (Id EKAlarm)
alarmWithRelativeOffsetSelector = mkSelector "alarmWithRelativeOffset:"

-- | @Selector@ for @relativeOffset@
relativeOffsetSelector :: Selector '[] CDouble
relativeOffsetSelector = mkSelector "relativeOffset"

-- | @Selector@ for @setRelativeOffset:@
setRelativeOffsetSelector :: Selector '[CDouble] ()
setRelativeOffsetSelector = mkSelector "setRelativeOffset:"

-- | @Selector@ for @absoluteDate@
absoluteDateSelector :: Selector '[] (Id NSDate)
absoluteDateSelector = mkSelector "absoluteDate"

-- | @Selector@ for @setAbsoluteDate:@
setAbsoluteDateSelector :: Selector '[Id NSDate] ()
setAbsoluteDateSelector = mkSelector "setAbsoluteDate:"

-- | @Selector@ for @structuredLocation@
structuredLocationSelector :: Selector '[] (Id EKStructuredLocation)
structuredLocationSelector = mkSelector "structuredLocation"

-- | @Selector@ for @setStructuredLocation:@
setStructuredLocationSelector :: Selector '[Id EKStructuredLocation] ()
setStructuredLocationSelector = mkSelector "setStructuredLocation:"

-- | @Selector@ for @proximity@
proximitySelector :: Selector '[] EKAlarmProximity
proximitySelector = mkSelector "proximity"

-- | @Selector@ for @setProximity:@
setProximitySelector :: Selector '[EKAlarmProximity] ()
setProximitySelector = mkSelector "setProximity:"

-- | @Selector@ for @type@
typeSelector :: Selector '[] EKAlarmType
typeSelector = mkSelector "type"

-- | @Selector@ for @emailAddress@
emailAddressSelector :: Selector '[] RawId
emailAddressSelector = mkSelector "emailAddress"

-- | @Selector@ for @setEmailAddress:@
setEmailAddressSelector :: Selector '[RawId] ()
setEmailAddressSelector = mkSelector "setEmailAddress:"

-- | @Selector@ for @soundName@
soundNameSelector :: Selector '[] RawId
soundNameSelector = mkSelector "soundName"

-- | @Selector@ for @setSoundName:@
setSoundNameSelector :: Selector '[RawId] ()
setSoundNameSelector = mkSelector "setSoundName:"

-- | @Selector@ for @url@
urlSelector :: Selector '[] RawId
urlSelector = mkSelector "url"

-- | @Selector@ for @setUrl:@
setUrlSelector :: Selector '[RawId] ()
setUrlSelector = mkSelector "setUrl:"

