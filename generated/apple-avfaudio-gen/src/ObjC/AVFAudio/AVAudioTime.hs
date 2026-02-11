{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | AVAudioTime
--
-- Represent a moment in time.
--
-- AVAudioTime is used in AVAudioEngine to represent time. Instances are immutable.
--
-- A single moment in time may be represented in two different ways:		1. mach_absolute_time(), the system's basic clock. Commonly referred to as "host time."		2. audio samples at a particular sample rate
--
-- A single AVAudioTime instance may contain either or both representations; it might		represent only a sample time, only a host time, or both.
--
-- Rationale for using host time:[a] internally we are using AudioTimeStamp, which uses host time, and it seems silly to divide[b] it is consistent with a standard system timing service[c] we do provide conveniences to convert between host ticks and seconds (host time divided by	frequency) so client code wanting to do what should be straightforward time computations can at 	least not be cluttered by ugly multiplications and divisions by the host clock frequency.
--
-- Generated bindings for @AVAudioTime@.
module ObjC.AVFAudio.AVAudioTime
  ( AVAudioTime
  , IsAVAudioTime(..)
  , initWithAudioTimeStamp_sampleRate
  , initWithHostTime
  , initWithSampleTime_atRate
  , initWithHostTime_sampleTime_atRate
  , timeWithAudioTimeStamp_sampleRate
  , timeWithHostTime
  , timeWithSampleTime_atRate
  , timeWithHostTime_sampleTime_atRate
  , hostTimeForSeconds
  , secondsForHostTime
  , extrapolateTimeFromAnchor
  , hostTimeValid
  , hostTime
  , sampleTimeValid
  , sampleTime
  , sampleRate
  , initWithAudioTimeStamp_sampleRateSelector
  , initWithHostTimeSelector
  , initWithSampleTime_atRateSelector
  , initWithHostTime_sampleTime_atRateSelector
  , timeWithAudioTimeStamp_sampleRateSelector
  , timeWithHostTimeSelector
  , timeWithSampleTime_atRateSelector
  , timeWithHostTime_sampleTime_atRateSelector
  , hostTimeForSecondsSelector
  , secondsForHostTimeSelector
  , extrapolateTimeFromAnchorSelector
  , hostTimeValidSelector
  , hostTimeSelector
  , sampleTimeValidSelector
  , sampleTimeSelector
  , sampleRateSelector


  ) where

import Foreign.Ptr (Ptr, nullPtr, castPtr)
import Foreign.LibFFI
import Foreign.C.Types
import Data.Int (Int8, Int16)
import Data.Word (Word16)
import Data.Coerce (coerce)

import ObjC.Runtime.Types
import ObjC.Runtime.MsgSend (sendMsg, sendClassMsg, sendMsgStret, sendClassMsgStret)
import ObjC.Runtime.Selector (mkSelector)
import ObjC.Runtime.Class (getRequiredClass)

import ObjC.AVFAudio.Internal.Classes
import ObjC.Foundation.Internal.Classes

-- | initWithAudioTimeStamp:sampleRate:
--
-- ObjC selector: @- initWithAudioTimeStamp:sampleRate:@
initWithAudioTimeStamp_sampleRate :: IsAVAudioTime avAudioTime => avAudioTime -> Const RawId -> CDouble -> IO (Id AVAudioTime)
initWithAudioTimeStamp_sampleRate avAudioTime  ts sampleRate =
    sendMsg avAudioTime (mkSelector "initWithAudioTimeStamp:sampleRate:") (retPtr retVoid) [argPtr (castPtr (unRawId (unConst ts)) :: Ptr ()), argCDouble sampleRate] >>= ownedObject . castPtr

-- | initWithHostTime:
--
-- ObjC selector: @- initWithHostTime:@
initWithHostTime :: IsAVAudioTime avAudioTime => avAudioTime -> CULong -> IO (Id AVAudioTime)
initWithHostTime avAudioTime  hostTime =
    sendMsg avAudioTime (mkSelector "initWithHostTime:") (retPtr retVoid) [argCULong hostTime] >>= ownedObject . castPtr

-- | initWithSampleTime:atRate:
--
-- ObjC selector: @- initWithSampleTime:atRate:@
initWithSampleTime_atRate :: IsAVAudioTime avAudioTime => avAudioTime -> CLong -> CDouble -> IO (Id AVAudioTime)
initWithSampleTime_atRate avAudioTime  sampleTime sampleRate =
    sendMsg avAudioTime (mkSelector "initWithSampleTime:atRate:") (retPtr retVoid) [argCLong sampleTime, argCDouble sampleRate] >>= ownedObject . castPtr

-- | initWithHostTime:sampleTime:atRate:
--
-- ObjC selector: @- initWithHostTime:sampleTime:atRate:@
initWithHostTime_sampleTime_atRate :: IsAVAudioTime avAudioTime => avAudioTime -> CULong -> CLong -> CDouble -> IO (Id AVAudioTime)
initWithHostTime_sampleTime_atRate avAudioTime  hostTime sampleTime sampleRate =
    sendMsg avAudioTime (mkSelector "initWithHostTime:sampleTime:atRate:") (retPtr retVoid) [argCULong hostTime, argCLong sampleTime, argCDouble sampleRate] >>= ownedObject . castPtr

-- | timeWithAudioTimeStamp:sampleRate:
--
-- ObjC selector: @+ timeWithAudioTimeStamp:sampleRate:@
timeWithAudioTimeStamp_sampleRate :: Const RawId -> CDouble -> IO (Id AVAudioTime)
timeWithAudioTimeStamp_sampleRate ts sampleRate =
  do
    cls' <- getRequiredClass "AVAudioTime"
    sendClassMsg cls' (mkSelector "timeWithAudioTimeStamp:sampleRate:") (retPtr retVoid) [argPtr (castPtr (unRawId (unConst ts)) :: Ptr ()), argCDouble sampleRate] >>= retainedObject . castPtr

-- | timeWithHostTime:
--
-- ObjC selector: @+ timeWithHostTime:@
timeWithHostTime :: CULong -> IO (Id AVAudioTime)
timeWithHostTime hostTime =
  do
    cls' <- getRequiredClass "AVAudioTime"
    sendClassMsg cls' (mkSelector "timeWithHostTime:") (retPtr retVoid) [argCULong hostTime] >>= retainedObject . castPtr

-- | timeWithSampleTime:atRate:
--
-- ObjC selector: @+ timeWithSampleTime:atRate:@
timeWithSampleTime_atRate :: CLong -> CDouble -> IO (Id AVAudioTime)
timeWithSampleTime_atRate sampleTime sampleRate =
  do
    cls' <- getRequiredClass "AVAudioTime"
    sendClassMsg cls' (mkSelector "timeWithSampleTime:atRate:") (retPtr retVoid) [argCLong sampleTime, argCDouble sampleRate] >>= retainedObject . castPtr

-- | timeWithHostTime:sampleTime:atRate:
--
-- ObjC selector: @+ timeWithHostTime:sampleTime:atRate:@
timeWithHostTime_sampleTime_atRate :: CULong -> CLong -> CDouble -> IO (Id AVAudioTime)
timeWithHostTime_sampleTime_atRate hostTime sampleTime sampleRate =
  do
    cls' <- getRequiredClass "AVAudioTime"
    sendClassMsg cls' (mkSelector "timeWithHostTime:sampleTime:atRate:") (retPtr retVoid) [argCULong hostTime, argCLong sampleTime, argCDouble sampleRate] >>= retainedObject . castPtr

-- | hostTimeForSeconds:
--
-- Convert seconds to host time.
--
-- ObjC selector: @+ hostTimeForSeconds:@
hostTimeForSeconds :: CDouble -> IO CULong
hostTimeForSeconds seconds =
  do
    cls' <- getRequiredClass "AVAudioTime"
    sendClassMsg cls' (mkSelector "hostTimeForSeconds:") retCULong [argCDouble seconds]

-- | secondsForHostTime:
--
-- Convert host time to seconds.
--
-- ObjC selector: @+ secondsForHostTime:@
secondsForHostTime :: CULong -> IO CDouble
secondsForHostTime hostTime =
  do
    cls' <- getRequiredClass "AVAudioTime"
    sendClassMsg cls' (mkSelector "secondsForHostTime:") retCDouble [argCULong hostTime]

-- | extrapolateTimeFromAnchor:
--
-- Converts between host and sample time.
--
-- @anchorTime@ — An AVAudioTime with a more complete AudioTimeStamp than that of the receiver (self).
--
-- Returns: the extrapolated time
--
-- If anchorTime is an AVAudioTime where both host time and sample time are valid,		and self is another timestamp where only one of the two is valid, this method		returns a new AVAudioTime copied from self and where any additional valid fields provided by		the anchor are also valid.
--
-- Note that the anchorTime must have both host and sample time valid, and self must have		sample rate and at least one of host or sample time valid. Otherwise this method returns nil.
--
-- // time0 has a valid audio sample representation, but no host time representation.AVAudioTime *time0 = [AVAudioTime timeWithSampleTime: 0.0 atRate: 44100.0];// anchor has a valid host time representation and sample time representation.AVAudioTime *anchor = [player playerTimeForNodeTime: player.lastRenderTime];// fill in valid host time representationAVAudioTime *fullTime0 = [time0 extrapolateTimeFromAnchor: anchor];
--
-- ObjC selector: @- extrapolateTimeFromAnchor:@
extrapolateTimeFromAnchor :: (IsAVAudioTime avAudioTime, IsAVAudioTime anchorTime) => avAudioTime -> anchorTime -> IO (Id AVAudioTime)
extrapolateTimeFromAnchor avAudioTime  anchorTime =
  withObjCPtr anchorTime $ \raw_anchorTime ->
      sendMsg avAudioTime (mkSelector "extrapolateTimeFromAnchor:") (retPtr retVoid) [argPtr (castPtr raw_anchorTime :: Ptr ())] >>= retainedObject . castPtr

-- | hostTimeValid
--
-- Whether the hostTime property is valid.
--
-- ObjC selector: @- hostTimeValid@
hostTimeValid :: IsAVAudioTime avAudioTime => avAudioTime -> IO Bool
hostTimeValid avAudioTime  =
    fmap ((/= 0) :: CULong -> Bool) $ sendMsg avAudioTime (mkSelector "hostTimeValid") retCULong []

-- | hostTime
--
-- The host time.
--
-- ObjC selector: @- hostTime@
hostTime :: IsAVAudioTime avAudioTime => avAudioTime -> IO CULong
hostTime avAudioTime  =
    sendMsg avAudioTime (mkSelector "hostTime") retCULong []

-- | sampleTimeValid
--
-- Whether the sampleTime and sampleRate properties are valid.
--
-- ObjC selector: @- sampleTimeValid@
sampleTimeValid :: IsAVAudioTime avAudioTime => avAudioTime -> IO Bool
sampleTimeValid avAudioTime  =
    fmap ((/= 0) :: CULong -> Bool) $ sendMsg avAudioTime (mkSelector "sampleTimeValid") retCULong []

-- | sampleTime
--
-- The time as a number of audio samples, as tracked by the current audio device.
--
-- ObjC selector: @- sampleTime@
sampleTime :: IsAVAudioTime avAudioTime => avAudioTime -> IO CLong
sampleTime avAudioTime  =
    sendMsg avAudioTime (mkSelector "sampleTime") retCLong []

-- | sampleRate
--
-- The sample rate at which sampleTime is being expressed.
--
-- ObjC selector: @- sampleRate@
sampleRate :: IsAVAudioTime avAudioTime => avAudioTime -> IO CDouble
sampleRate avAudioTime  =
    sendMsg avAudioTime (mkSelector "sampleRate") retCDouble []

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @initWithAudioTimeStamp:sampleRate:@
initWithAudioTimeStamp_sampleRateSelector :: Selector
initWithAudioTimeStamp_sampleRateSelector = mkSelector "initWithAudioTimeStamp:sampleRate:"

-- | @Selector@ for @initWithHostTime:@
initWithHostTimeSelector :: Selector
initWithHostTimeSelector = mkSelector "initWithHostTime:"

-- | @Selector@ for @initWithSampleTime:atRate:@
initWithSampleTime_atRateSelector :: Selector
initWithSampleTime_atRateSelector = mkSelector "initWithSampleTime:atRate:"

-- | @Selector@ for @initWithHostTime:sampleTime:atRate:@
initWithHostTime_sampleTime_atRateSelector :: Selector
initWithHostTime_sampleTime_atRateSelector = mkSelector "initWithHostTime:sampleTime:atRate:"

-- | @Selector@ for @timeWithAudioTimeStamp:sampleRate:@
timeWithAudioTimeStamp_sampleRateSelector :: Selector
timeWithAudioTimeStamp_sampleRateSelector = mkSelector "timeWithAudioTimeStamp:sampleRate:"

-- | @Selector@ for @timeWithHostTime:@
timeWithHostTimeSelector :: Selector
timeWithHostTimeSelector = mkSelector "timeWithHostTime:"

-- | @Selector@ for @timeWithSampleTime:atRate:@
timeWithSampleTime_atRateSelector :: Selector
timeWithSampleTime_atRateSelector = mkSelector "timeWithSampleTime:atRate:"

-- | @Selector@ for @timeWithHostTime:sampleTime:atRate:@
timeWithHostTime_sampleTime_atRateSelector :: Selector
timeWithHostTime_sampleTime_atRateSelector = mkSelector "timeWithHostTime:sampleTime:atRate:"

-- | @Selector@ for @hostTimeForSeconds:@
hostTimeForSecondsSelector :: Selector
hostTimeForSecondsSelector = mkSelector "hostTimeForSeconds:"

-- | @Selector@ for @secondsForHostTime:@
secondsForHostTimeSelector :: Selector
secondsForHostTimeSelector = mkSelector "secondsForHostTime:"

-- | @Selector@ for @extrapolateTimeFromAnchor:@
extrapolateTimeFromAnchorSelector :: Selector
extrapolateTimeFromAnchorSelector = mkSelector "extrapolateTimeFromAnchor:"

-- | @Selector@ for @hostTimeValid@
hostTimeValidSelector :: Selector
hostTimeValidSelector = mkSelector "hostTimeValid"

-- | @Selector@ for @hostTime@
hostTimeSelector :: Selector
hostTimeSelector = mkSelector "hostTime"

-- | @Selector@ for @sampleTimeValid@
sampleTimeValidSelector :: Selector
sampleTimeValidSelector = mkSelector "sampleTimeValid"

-- | @Selector@ for @sampleTime@
sampleTimeSelector :: Selector
sampleTimeSelector = mkSelector "sampleTime"

-- | @Selector@ for @sampleRate@
sampleRateSelector :: Selector
sampleRateSelector = mkSelector "sampleRate"

