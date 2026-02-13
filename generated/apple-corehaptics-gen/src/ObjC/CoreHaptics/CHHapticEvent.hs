{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | CHHapticEvent
--
-- The description of a single haptic/audio event, plus optional Event parameters which modify the event.
--
-- CHHapticEvents have a relative time property to allow specifying the time relationship between events in a pattern.
--
-- Generated bindings for @CHHapticEvent@.
module ObjC.CoreHaptics.CHHapticEvent
  ( CHHapticEvent
  , IsCHHapticEvent(..)
  , init_
  , initWithEventType_parameters_relativeTime
  , initWithEventType_parameters_relativeTime_duration
  , initWithAudioResourceID_parameters_relativeTime
  , initWithAudioResourceID_parameters_relativeTime_duration
  , type_
  , eventParameters
  , relativeTime
  , setRelativeTime
  , duration
  , setDuration
  , durationSelector
  , eventParametersSelector
  , initSelector
  , initWithAudioResourceID_parameters_relativeTimeSelector
  , initWithAudioResourceID_parameters_relativeTime_durationSelector
  , initWithEventType_parameters_relativeTimeSelector
  , initWithEventType_parameters_relativeTime_durationSelector
  , relativeTimeSelector
  , setDurationSelector
  , setRelativeTimeSelector
  , typeSelector


  ) where

import Foreign.Ptr (Ptr, FunPtr)
import Foreign.C.Types

import ObjC.Runtime.Types
import ObjC.Runtime.Message (sendMessage, sendOwnedMessage, sendClassMessage, sendOwnedClassMessage)
import ObjC.Runtime.Selector (mkSelector)
import ObjC.Runtime.Class (getRequiredClass)

import ObjC.CoreHaptics.Internal.Classes
import ObjC.Foundation.Internal.Classes

-- | @- init@
init_ :: IsCHHapticEvent chHapticEvent => chHapticEvent -> IO (Id CHHapticEvent)
init_ chHapticEvent =
  sendOwnedMessage chHapticEvent initSelector

-- | initWithEventType:parameters:relativeTime
--
-- Initialize a new CHHapticEvent.  This can only be used to create Transient event types (which do not require a duration).
--
-- @type@ — The type of event.
--
-- @eventParams@ — An NSArray of Event parameters.  Can be empty.
--
-- @time@ — The relative time for this event versus the other events in the CHHapticPattern.
--
-- ObjC selector: @- initWithEventType:parameters:relativeTime:@
initWithEventType_parameters_relativeTime :: (IsCHHapticEvent chHapticEvent, IsNSString type_, IsNSArray eventParams) => chHapticEvent -> type_ -> eventParams -> CDouble -> IO (Id CHHapticEvent)
initWithEventType_parameters_relativeTime chHapticEvent type_ eventParams time =
  sendOwnedMessage chHapticEvent initWithEventType_parameters_relativeTimeSelector (toNSString type_) (toNSArray eventParams) time

-- | initWithEventType:parameters:relativeTime:duration
--
-- Initialize a new CHHapticEvent, providing a duration.
--
-- @type@ — The type of event.
--
-- @eventParams@ — An NSArray of Event parameters.  Can be empty.
--
-- @time@ — The relative time for this event versus the other events in the CHHapticPattern.
--
-- @duration@ — For Continuous event types, the length of time before the event playback begins its release. 		For Transient event types, the logical length of the event (used to determine pattern end and loop points).
--
-- ObjC selector: @- initWithEventType:parameters:relativeTime:duration:@
initWithEventType_parameters_relativeTime_duration :: (IsCHHapticEvent chHapticEvent, IsNSString type_, IsNSArray eventParams) => chHapticEvent -> type_ -> eventParams -> CDouble -> CDouble -> IO (Id CHHapticEvent)
initWithEventType_parameters_relativeTime_duration chHapticEvent type_ eventParams time duration =
  sendOwnedMessage chHapticEvent initWithEventType_parameters_relativeTime_durationSelector (toNSString type_) (toNSArray eventParams) time duration

-- | initWithAudioResourceID:parameters:relativeTime
--
-- Initialize a new CHHapticEvent using a previously-loaded audio resource.
--
-- @resID@ — A previously-registered audio resource ID (see @CHHapticEngine(registerAudioResource:options:error)@).
--
-- @eventParams@ — An NSArray of Event parameters.  Can be empty.
--
-- @time@ — The relative time for this event versus the other events in the CHHapticPattern.
--
-- ObjC selector: @- initWithAudioResourceID:parameters:relativeTime:@
initWithAudioResourceID_parameters_relativeTime :: (IsCHHapticEvent chHapticEvent, IsNSArray eventParams) => chHapticEvent -> CULong -> eventParams -> CDouble -> IO (Id CHHapticEvent)
initWithAudioResourceID_parameters_relativeTime chHapticEvent resID eventParams time =
  sendOwnedMessage chHapticEvent initWithAudioResourceID_parameters_relativeTimeSelector resID (toNSArray eventParams) time

-- | initWithAudioResourceID:parameters:relativeTime:duration
--
-- Initialize a new CHHapticEvent using a previously-loaded audio resource.
--
-- @resID@ — A previously-registered audio resource ID (see @CHHapticEngine(registerAudioResource:options:error)@).
--
-- @eventParams@ — An NSArray of Event parameters.  Can be empty.
--
-- @time@ — The relative time for this event versus the other events in the CHHapticPattern.
--
-- @duration@ — The duration of this event in seconds.
--
-- If the specified duration is less than the duration of the audio resource, its playback will be truncated.  If it		is greater, its playback will be padded with silence.  If zero, it will be ignored.
--
-- ObjC selector: @- initWithAudioResourceID:parameters:relativeTime:duration:@
initWithAudioResourceID_parameters_relativeTime_duration :: (IsCHHapticEvent chHapticEvent, IsNSArray eventParams) => chHapticEvent -> CULong -> eventParams -> CDouble -> CDouble -> IO (Id CHHapticEvent)
initWithAudioResourceID_parameters_relativeTime_duration chHapticEvent resID eventParams time duration =
  sendOwnedMessage chHapticEvent initWithAudioResourceID_parameters_relativeTime_durationSelector resID (toNSArray eventParams) time duration

-- | type
--
-- The type of event.
--
-- ObjC selector: @- type@
type_ :: IsCHHapticEvent chHapticEvent => chHapticEvent -> IO (Id NSString)
type_ chHapticEvent =
  sendMessage chHapticEvent typeSelector

-- | eventParameters
--
-- NSArray of Event parameters.  Can be empty.
--
-- ObjC selector: @- eventParameters@
eventParameters :: IsCHHapticEvent chHapticEvent => chHapticEvent -> IO (Id NSArray)
eventParameters chHapticEvent =
  sendMessage chHapticEvent eventParametersSelector

-- | relativeTime
--
-- The relative time for this event versus the start time of the pattern.  Units are seconds.
--
-- ObjC selector: @- relativeTime@
relativeTime :: IsCHHapticEvent chHapticEvent => chHapticEvent -> IO CDouble
relativeTime chHapticEvent =
  sendMessage chHapticEvent relativeTimeSelector

-- | relativeTime
--
-- The relative time for this event versus the start time of the pattern.  Units are seconds.
--
-- ObjC selector: @- setRelativeTime:@
setRelativeTime :: IsCHHapticEvent chHapticEvent => chHapticEvent -> CDouble -> IO ()
setRelativeTime chHapticEvent value =
  sendMessage chHapticEvent setRelativeTimeSelector value

-- | duration
--
-- The duration for this event.  Units are seconds.  If unset (0.0), Continuous event types will have no fixed end.
--
-- ObjC selector: @- duration@
duration :: IsCHHapticEvent chHapticEvent => chHapticEvent -> IO CDouble
duration chHapticEvent =
  sendMessage chHapticEvent durationSelector

-- | duration
--
-- The duration for this event.  Units are seconds.  If unset (0.0), Continuous event types will have no fixed end.
--
-- ObjC selector: @- setDuration:@
setDuration :: IsCHHapticEvent chHapticEvent => chHapticEvent -> CDouble -> IO ()
setDuration chHapticEvent value =
  sendMessage chHapticEvent setDurationSelector value

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @init@
initSelector :: Selector '[] (Id CHHapticEvent)
initSelector = mkSelector "init"

-- | @Selector@ for @initWithEventType:parameters:relativeTime:@
initWithEventType_parameters_relativeTimeSelector :: Selector '[Id NSString, Id NSArray, CDouble] (Id CHHapticEvent)
initWithEventType_parameters_relativeTimeSelector = mkSelector "initWithEventType:parameters:relativeTime:"

-- | @Selector@ for @initWithEventType:parameters:relativeTime:duration:@
initWithEventType_parameters_relativeTime_durationSelector :: Selector '[Id NSString, Id NSArray, CDouble, CDouble] (Id CHHapticEvent)
initWithEventType_parameters_relativeTime_durationSelector = mkSelector "initWithEventType:parameters:relativeTime:duration:"

-- | @Selector@ for @initWithAudioResourceID:parameters:relativeTime:@
initWithAudioResourceID_parameters_relativeTimeSelector :: Selector '[CULong, Id NSArray, CDouble] (Id CHHapticEvent)
initWithAudioResourceID_parameters_relativeTimeSelector = mkSelector "initWithAudioResourceID:parameters:relativeTime:"

-- | @Selector@ for @initWithAudioResourceID:parameters:relativeTime:duration:@
initWithAudioResourceID_parameters_relativeTime_durationSelector :: Selector '[CULong, Id NSArray, CDouble, CDouble] (Id CHHapticEvent)
initWithAudioResourceID_parameters_relativeTime_durationSelector = mkSelector "initWithAudioResourceID:parameters:relativeTime:duration:"

-- | @Selector@ for @type@
typeSelector :: Selector '[] (Id NSString)
typeSelector = mkSelector "type"

-- | @Selector@ for @eventParameters@
eventParametersSelector :: Selector '[] (Id NSArray)
eventParametersSelector = mkSelector "eventParameters"

-- | @Selector@ for @relativeTime@
relativeTimeSelector :: Selector '[] CDouble
relativeTimeSelector = mkSelector "relativeTime"

-- | @Selector@ for @setRelativeTime:@
setRelativeTimeSelector :: Selector '[CDouble] ()
setRelativeTimeSelector = mkSelector "setRelativeTime:"

-- | @Selector@ for @duration@
durationSelector :: Selector '[] CDouble
durationSelector = mkSelector "duration"

-- | @Selector@ for @setDuration:@
setDurationSelector :: Selector '[CDouble] ()
setDurationSelector = mkSelector "setDuration:"

