{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Generated bindings for @NSTimer@.
module ObjC.Foundation.NSTimer
  ( NSTimer
  , IsNSTimer(..)
  , timerWithTimeInterval_invocation_repeats
  , scheduledTimerWithTimeInterval_invocation_repeats
  , timerWithTimeInterval_target_selector_userInfo_repeats
  , scheduledTimerWithTimeInterval_target_selector_userInfo_repeats
  , timerWithTimeInterval_repeats_block
  , scheduledTimerWithTimeInterval_repeats_block
  , initWithFireDate_interval_repeats_block
  , initWithFireDate_interval_target_selector_userInfo_repeats
  , fire
  , invalidate
  , fireDate
  , setFireDate
  , timeInterval
  , tolerance
  , setTolerance
  , valid
  , userInfo
  , timerWithTimeInterval_invocation_repeatsSelector
  , scheduledTimerWithTimeInterval_invocation_repeatsSelector
  , timerWithTimeInterval_target_selector_userInfo_repeatsSelector
  , scheduledTimerWithTimeInterval_target_selector_userInfo_repeatsSelector
  , timerWithTimeInterval_repeats_blockSelector
  , scheduledTimerWithTimeInterval_repeats_blockSelector
  , initWithFireDate_interval_repeats_blockSelector
  , initWithFireDate_interval_target_selector_userInfo_repeatsSelector
  , fireSelector
  , invalidateSelector
  , fireDateSelector
  , setFireDateSelector
  , timeIntervalSelector
  , toleranceSelector
  , setToleranceSelector
  , validSelector
  , userInfoSelector


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

import ObjC.Foundation.Internal.Classes

-- | @+ timerWithTimeInterval:invocation:repeats:@
timerWithTimeInterval_invocation_repeats :: IsNSInvocation invocation => CDouble -> invocation -> Bool -> IO (Id NSTimer)
timerWithTimeInterval_invocation_repeats ti invocation yesOrNo =
  do
    cls' <- getRequiredClass "NSTimer"
    withObjCPtr invocation $ \raw_invocation ->
      sendClassMsg cls' (mkSelector "timerWithTimeInterval:invocation:repeats:") (retPtr retVoid) [argCDouble (fromIntegral ti), argPtr (castPtr raw_invocation :: Ptr ()), argCULong (if yesOrNo then 1 else 0)] >>= retainedObject . castPtr

-- | @+ scheduledTimerWithTimeInterval:invocation:repeats:@
scheduledTimerWithTimeInterval_invocation_repeats :: IsNSInvocation invocation => CDouble -> invocation -> Bool -> IO (Id NSTimer)
scheduledTimerWithTimeInterval_invocation_repeats ti invocation yesOrNo =
  do
    cls' <- getRequiredClass "NSTimer"
    withObjCPtr invocation $ \raw_invocation ->
      sendClassMsg cls' (mkSelector "scheduledTimerWithTimeInterval:invocation:repeats:") (retPtr retVoid) [argCDouble (fromIntegral ti), argPtr (castPtr raw_invocation :: Ptr ()), argCULong (if yesOrNo then 1 else 0)] >>= retainedObject . castPtr

-- | @+ timerWithTimeInterval:target:selector:userInfo:repeats:@
timerWithTimeInterval_target_selector_userInfo_repeats :: CDouble -> RawId -> Selector -> RawId -> Bool -> IO (Id NSTimer)
timerWithTimeInterval_target_selector_userInfo_repeats ti aTarget aSelector userInfo yesOrNo =
  do
    cls' <- getRequiredClass "NSTimer"
    sendClassMsg cls' (mkSelector "timerWithTimeInterval:target:selector:userInfo:repeats:") (retPtr retVoid) [argCDouble (fromIntegral ti), argPtr (castPtr (unRawId aTarget) :: Ptr ()), argPtr (unSelector aSelector), argPtr (castPtr (unRawId userInfo) :: Ptr ()), argCULong (if yesOrNo then 1 else 0)] >>= retainedObject . castPtr

-- | @+ scheduledTimerWithTimeInterval:target:selector:userInfo:repeats:@
scheduledTimerWithTimeInterval_target_selector_userInfo_repeats :: CDouble -> RawId -> Selector -> RawId -> Bool -> IO (Id NSTimer)
scheduledTimerWithTimeInterval_target_selector_userInfo_repeats ti aTarget aSelector userInfo yesOrNo =
  do
    cls' <- getRequiredClass "NSTimer"
    sendClassMsg cls' (mkSelector "scheduledTimerWithTimeInterval:target:selector:userInfo:repeats:") (retPtr retVoid) [argCDouble (fromIntegral ti), argPtr (castPtr (unRawId aTarget) :: Ptr ()), argPtr (unSelector aSelector), argPtr (castPtr (unRawId userInfo) :: Ptr ()), argCULong (if yesOrNo then 1 else 0)] >>= retainedObject . castPtr

-- | Creates and returns a new NSTimer object initialized with the specified block object. This timer needs to be scheduled on a run loop (via -[NSRunLoop addTimer:]) before it will fire. - parameter:  timeInterval  The number of seconds between firings of the timer. If seconds is less than or equal to 0.0, this method chooses the nonnegative value of 0.1 milliseconds instead - parameter:  repeats  If YES, the timer will repeatedly reschedule itself until invalidated. If NO, the timer will be invalidated after it fires. - parameter:  block  The execution body of the timer; the timer itself is passed as the parameter to this block when executed to aid in avoiding cyclical references
--
-- ObjC selector: @+ timerWithTimeInterval:repeats:block:@
timerWithTimeInterval_repeats_block :: CDouble -> Bool -> Ptr () -> IO (Id NSTimer)
timerWithTimeInterval_repeats_block interval repeats block =
  do
    cls' <- getRequiredClass "NSTimer"
    sendClassMsg cls' (mkSelector "timerWithTimeInterval:repeats:block:") (retPtr retVoid) [argCDouble (fromIntegral interval), argCULong (if repeats then 1 else 0), argPtr (castPtr block :: Ptr ())] >>= retainedObject . castPtr

-- | Creates and returns a new NSTimer object initialized with the specified block object and schedules it on the current run loop in the default mode. - parameter:  ti    The number of seconds between firings of the timer. If seconds is less than or equal to 0.0, this method chooses the nonnegative value of 0.1 milliseconds instead - parameter:  repeats  If YES, the timer will repeatedly reschedule itself until invalidated. If NO, the timer will be invalidated after it fires. - parameter:  block  The execution body of the timer; the timer itself is passed as the parameter to this block when executed to aid in avoiding cyclical references
--
-- ObjC selector: @+ scheduledTimerWithTimeInterval:repeats:block:@
scheduledTimerWithTimeInterval_repeats_block :: CDouble -> Bool -> Ptr () -> IO (Id NSTimer)
scheduledTimerWithTimeInterval_repeats_block interval repeats block =
  do
    cls' <- getRequiredClass "NSTimer"
    sendClassMsg cls' (mkSelector "scheduledTimerWithTimeInterval:repeats:block:") (retPtr retVoid) [argCDouble (fromIntegral interval), argCULong (if repeats then 1 else 0), argPtr (castPtr block :: Ptr ())] >>= retainedObject . castPtr

-- | Initializes a new NSTimer object using the block as the main body of execution for the timer. This timer needs to be scheduled on a run loop (via -[NSRunLoop addTimer:]) before it will fire. - parameter:  fireDate   The time at which the timer should first fire. - parameter:  interval  The number of seconds between firings of the timer. If seconds is less than or equal to 0.0, this method chooses the nonnegative value of 0.1 milliseconds instead - parameter:  repeats  If YES, the timer will repeatedly reschedule itself until invalidated. If NO, the timer will be invalidated after it fires. - parameter:  block  The execution body of the timer; the timer itself is passed as the parameter to this block when executed to aid in avoiding cyclical references
--
-- ObjC selector: @- initWithFireDate:interval:repeats:block:@
initWithFireDate_interval_repeats_block :: (IsNSTimer nsTimer, IsNSDate date) => nsTimer -> date -> CDouble -> Bool -> Ptr () -> IO (Id NSTimer)
initWithFireDate_interval_repeats_block nsTimer  date interval repeats block =
withObjCPtr date $ \raw_date ->
    sendMsg nsTimer (mkSelector "initWithFireDate:interval:repeats:block:") (retPtr retVoid) [argPtr (castPtr raw_date :: Ptr ()), argCDouble (fromIntegral interval), argCULong (if repeats then 1 else 0), argPtr (castPtr block :: Ptr ())] >>= ownedObject . castPtr

-- | @- initWithFireDate:interval:target:selector:userInfo:repeats:@
initWithFireDate_interval_target_selector_userInfo_repeats :: (IsNSTimer nsTimer, IsNSDate date) => nsTimer -> date -> CDouble -> RawId -> Selector -> RawId -> Bool -> IO (Id NSTimer)
initWithFireDate_interval_target_selector_userInfo_repeats nsTimer  date ti t s ui rep =
withObjCPtr date $ \raw_date ->
    sendMsg nsTimer (mkSelector "initWithFireDate:interval:target:selector:userInfo:repeats:") (retPtr retVoid) [argPtr (castPtr raw_date :: Ptr ()), argCDouble (fromIntegral ti), argPtr (castPtr (unRawId t) :: Ptr ()), argPtr (unSelector s), argPtr (castPtr (unRawId ui) :: Ptr ()), argCULong (if rep then 1 else 0)] >>= ownedObject . castPtr

-- | @- fire@
fire :: IsNSTimer nsTimer => nsTimer -> IO ()
fire nsTimer  =
  sendMsg nsTimer (mkSelector "fire") retVoid []

-- | @- invalidate@
invalidate :: IsNSTimer nsTimer => nsTimer -> IO ()
invalidate nsTimer  =
  sendMsg nsTimer (mkSelector "invalidate") retVoid []

-- | @- fireDate@
fireDate :: IsNSTimer nsTimer => nsTimer -> IO (Id NSDate)
fireDate nsTimer  =
  sendMsg nsTimer (mkSelector "fireDate") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- setFireDate:@
setFireDate :: (IsNSTimer nsTimer, IsNSDate value) => nsTimer -> value -> IO ()
setFireDate nsTimer  value =
withObjCPtr value $ \raw_value ->
    sendMsg nsTimer (mkSelector "setFireDate:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- | @- timeInterval@
timeInterval :: IsNSTimer nsTimer => nsTimer -> IO CDouble
timeInterval nsTimer  =
  sendMsg nsTimer (mkSelector "timeInterval") retCDouble []

-- | @- tolerance@
tolerance :: IsNSTimer nsTimer => nsTimer -> IO CDouble
tolerance nsTimer  =
  sendMsg nsTimer (mkSelector "tolerance") retCDouble []

-- | @- setTolerance:@
setTolerance :: IsNSTimer nsTimer => nsTimer -> CDouble -> IO ()
setTolerance nsTimer  value =
  sendMsg nsTimer (mkSelector "setTolerance:") retVoid [argCDouble (fromIntegral value)]

-- | @- valid@
valid :: IsNSTimer nsTimer => nsTimer -> IO Bool
valid nsTimer  =
  fmap ((/= 0) :: CULong -> Bool) $ sendMsg nsTimer (mkSelector "valid") retCULong []

-- | @- userInfo@
userInfo :: IsNSTimer nsTimer => nsTimer -> IO RawId
userInfo nsTimer  =
  fmap (RawId . castPtr) $ sendMsg nsTimer (mkSelector "userInfo") (retPtr retVoid) []

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @timerWithTimeInterval:invocation:repeats:@
timerWithTimeInterval_invocation_repeatsSelector :: Selector
timerWithTimeInterval_invocation_repeatsSelector = mkSelector "timerWithTimeInterval:invocation:repeats:"

-- | @Selector@ for @scheduledTimerWithTimeInterval:invocation:repeats:@
scheduledTimerWithTimeInterval_invocation_repeatsSelector :: Selector
scheduledTimerWithTimeInterval_invocation_repeatsSelector = mkSelector "scheduledTimerWithTimeInterval:invocation:repeats:"

-- | @Selector@ for @timerWithTimeInterval:target:selector:userInfo:repeats:@
timerWithTimeInterval_target_selector_userInfo_repeatsSelector :: Selector
timerWithTimeInterval_target_selector_userInfo_repeatsSelector = mkSelector "timerWithTimeInterval:target:selector:userInfo:repeats:"

-- | @Selector@ for @scheduledTimerWithTimeInterval:target:selector:userInfo:repeats:@
scheduledTimerWithTimeInterval_target_selector_userInfo_repeatsSelector :: Selector
scheduledTimerWithTimeInterval_target_selector_userInfo_repeatsSelector = mkSelector "scheduledTimerWithTimeInterval:target:selector:userInfo:repeats:"

-- | @Selector@ for @timerWithTimeInterval:repeats:block:@
timerWithTimeInterval_repeats_blockSelector :: Selector
timerWithTimeInterval_repeats_blockSelector = mkSelector "timerWithTimeInterval:repeats:block:"

-- | @Selector@ for @scheduledTimerWithTimeInterval:repeats:block:@
scheduledTimerWithTimeInterval_repeats_blockSelector :: Selector
scheduledTimerWithTimeInterval_repeats_blockSelector = mkSelector "scheduledTimerWithTimeInterval:repeats:block:"

-- | @Selector@ for @initWithFireDate:interval:repeats:block:@
initWithFireDate_interval_repeats_blockSelector :: Selector
initWithFireDate_interval_repeats_blockSelector = mkSelector "initWithFireDate:interval:repeats:block:"

-- | @Selector@ for @initWithFireDate:interval:target:selector:userInfo:repeats:@
initWithFireDate_interval_target_selector_userInfo_repeatsSelector :: Selector
initWithFireDate_interval_target_selector_userInfo_repeatsSelector = mkSelector "initWithFireDate:interval:target:selector:userInfo:repeats:"

-- | @Selector@ for @fire@
fireSelector :: Selector
fireSelector = mkSelector "fire"

-- | @Selector@ for @invalidate@
invalidateSelector :: Selector
invalidateSelector = mkSelector "invalidate"

-- | @Selector@ for @fireDate@
fireDateSelector :: Selector
fireDateSelector = mkSelector "fireDate"

-- | @Selector@ for @setFireDate:@
setFireDateSelector :: Selector
setFireDateSelector = mkSelector "setFireDate:"

-- | @Selector@ for @timeInterval@
timeIntervalSelector :: Selector
timeIntervalSelector = mkSelector "timeInterval"

-- | @Selector@ for @tolerance@
toleranceSelector :: Selector
toleranceSelector = mkSelector "tolerance"

-- | @Selector@ for @setTolerance:@
setToleranceSelector :: Selector
setToleranceSelector = mkSelector "setTolerance:"

-- | @Selector@ for @valid@
validSelector :: Selector
validSelector = mkSelector "valid"

-- | @Selector@ for @userInfo@
userInfoSelector :: Selector
userInfoSelector = mkSelector "userInfo"

