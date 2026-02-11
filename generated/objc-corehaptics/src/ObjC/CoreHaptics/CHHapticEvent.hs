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
  , initSelector
  , initWithEventType_parameters_relativeTimeSelector
  , initWithEventType_parameters_relativeTime_durationSelector
  , initWithAudioResourceID_parameters_relativeTimeSelector
  , initWithAudioResourceID_parameters_relativeTime_durationSelector
  , typeSelector
  , eventParametersSelector
  , relativeTimeSelector
  , setRelativeTimeSelector
  , durationSelector
  , setDurationSelector


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

import ObjC.CoreHaptics.Internal.Classes
import ObjC.Foundation.Internal.Classes

-- | @- init@
init_ :: IsCHHapticEvent chHapticEvent => chHapticEvent -> IO (Id CHHapticEvent)
init_ chHapticEvent  =
  sendMsg chHapticEvent (mkSelector "init") (retPtr retVoid) [] >>= ownedObject . castPtr

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
initWithEventType_parameters_relativeTime chHapticEvent  type_ eventParams time =
withObjCPtr type_ $ \raw_type_ ->
  withObjCPtr eventParams $ \raw_eventParams ->
      sendMsg chHapticEvent (mkSelector "initWithEventType:parameters:relativeTime:") (retPtr retVoid) [argPtr (castPtr raw_type_ :: Ptr ()), argPtr (castPtr raw_eventParams :: Ptr ()), argCDouble (fromIntegral time)] >>= ownedObject . castPtr

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
initWithEventType_parameters_relativeTime_duration chHapticEvent  type_ eventParams time duration =
withObjCPtr type_ $ \raw_type_ ->
  withObjCPtr eventParams $ \raw_eventParams ->
      sendMsg chHapticEvent (mkSelector "initWithEventType:parameters:relativeTime:duration:") (retPtr retVoid) [argPtr (castPtr raw_type_ :: Ptr ()), argPtr (castPtr raw_eventParams :: Ptr ()), argCDouble (fromIntegral time), argCDouble (fromIntegral duration)] >>= ownedObject . castPtr

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
initWithAudioResourceID_parameters_relativeTime chHapticEvent  resID eventParams time =
withObjCPtr eventParams $ \raw_eventParams ->
    sendMsg chHapticEvent (mkSelector "initWithAudioResourceID:parameters:relativeTime:") (retPtr retVoid) [argCULong (fromIntegral resID), argPtr (castPtr raw_eventParams :: Ptr ()), argCDouble (fromIntegral time)] >>= ownedObject . castPtr

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
initWithAudioResourceID_parameters_relativeTime_duration chHapticEvent  resID eventParams time duration =
withObjCPtr eventParams $ \raw_eventParams ->
    sendMsg chHapticEvent (mkSelector "initWithAudioResourceID:parameters:relativeTime:duration:") (retPtr retVoid) [argCULong (fromIntegral resID), argPtr (castPtr raw_eventParams :: Ptr ()), argCDouble (fromIntegral time), argCDouble (fromIntegral duration)] >>= ownedObject . castPtr

-- | type
--
-- The type of event.
--
-- ObjC selector: @- type@
type_ :: IsCHHapticEvent chHapticEvent => chHapticEvent -> IO (Id NSString)
type_ chHapticEvent  =
  sendMsg chHapticEvent (mkSelector "type") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | eventParameters
--
-- NSArray of Event parameters.  Can be empty.
--
-- ObjC selector: @- eventParameters@
eventParameters :: IsCHHapticEvent chHapticEvent => chHapticEvent -> IO (Id NSArray)
eventParameters chHapticEvent  =
  sendMsg chHapticEvent (mkSelector "eventParameters") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | relativeTime
--
-- The relative time for this event versus the start time of the pattern.  Units are seconds.
--
-- ObjC selector: @- relativeTime@
relativeTime :: IsCHHapticEvent chHapticEvent => chHapticEvent -> IO CDouble
relativeTime chHapticEvent  =
  sendMsg chHapticEvent (mkSelector "relativeTime") retCDouble []

-- | relativeTime
--
-- The relative time for this event versus the start time of the pattern.  Units are seconds.
--
-- ObjC selector: @- setRelativeTime:@
setRelativeTime :: IsCHHapticEvent chHapticEvent => chHapticEvent -> CDouble -> IO ()
setRelativeTime chHapticEvent  value =
  sendMsg chHapticEvent (mkSelector "setRelativeTime:") retVoid [argCDouble (fromIntegral value)]

-- | duration
--
-- The duration for this event.  Units are seconds.  If unset (0.0), Continuous event types will have no fixed end.
--
-- ObjC selector: @- duration@
duration :: IsCHHapticEvent chHapticEvent => chHapticEvent -> IO CDouble
duration chHapticEvent  =
  sendMsg chHapticEvent (mkSelector "duration") retCDouble []

-- | duration
--
-- The duration for this event.  Units are seconds.  If unset (0.0), Continuous event types will have no fixed end.
--
-- ObjC selector: @- setDuration:@
setDuration :: IsCHHapticEvent chHapticEvent => chHapticEvent -> CDouble -> IO ()
setDuration chHapticEvent  value =
  sendMsg chHapticEvent (mkSelector "setDuration:") retVoid [argCDouble (fromIntegral value)]

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @init@
initSelector :: Selector
initSelector = mkSelector "init"

-- | @Selector@ for @initWithEventType:parameters:relativeTime:@
initWithEventType_parameters_relativeTimeSelector :: Selector
initWithEventType_parameters_relativeTimeSelector = mkSelector "initWithEventType:parameters:relativeTime:"

-- | @Selector@ for @initWithEventType:parameters:relativeTime:duration:@
initWithEventType_parameters_relativeTime_durationSelector :: Selector
initWithEventType_parameters_relativeTime_durationSelector = mkSelector "initWithEventType:parameters:relativeTime:duration:"

-- | @Selector@ for @initWithAudioResourceID:parameters:relativeTime:@
initWithAudioResourceID_parameters_relativeTimeSelector :: Selector
initWithAudioResourceID_parameters_relativeTimeSelector = mkSelector "initWithAudioResourceID:parameters:relativeTime:"

-- | @Selector@ for @initWithAudioResourceID:parameters:relativeTime:duration:@
initWithAudioResourceID_parameters_relativeTime_durationSelector :: Selector
initWithAudioResourceID_parameters_relativeTime_durationSelector = mkSelector "initWithAudioResourceID:parameters:relativeTime:duration:"

-- | @Selector@ for @type@
typeSelector :: Selector
typeSelector = mkSelector "type"

-- | @Selector@ for @eventParameters@
eventParametersSelector :: Selector
eventParametersSelector = mkSelector "eventParameters"

-- | @Selector@ for @relativeTime@
relativeTimeSelector :: Selector
relativeTimeSelector = mkSelector "relativeTime"

-- | @Selector@ for @setRelativeTime:@
setRelativeTimeSelector :: Selector
setRelativeTimeSelector = mkSelector "setRelativeTime:"

-- | @Selector@ for @duration@
durationSelector :: Selector
durationSelector = mkSelector "duration"

-- | @Selector@ for @setDuration:@
setDurationSelector :: Selector
setDurationSelector = mkSelector "setDuration:"

