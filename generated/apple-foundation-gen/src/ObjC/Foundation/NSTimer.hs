{-# LANGUAGE DataKinds #-}
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
  , fireDateSelector
  , fireSelector
  , initWithFireDate_interval_repeats_blockSelector
  , initWithFireDate_interval_target_selector_userInfo_repeatsSelector
  , invalidateSelector
  , scheduledTimerWithTimeInterval_invocation_repeatsSelector
  , scheduledTimerWithTimeInterval_repeats_blockSelector
  , scheduledTimerWithTimeInterval_target_selector_userInfo_repeatsSelector
  , setFireDateSelector
  , setToleranceSelector
  , timeIntervalSelector
  , timerWithTimeInterval_invocation_repeatsSelector
  , timerWithTimeInterval_repeats_blockSelector
  , timerWithTimeInterval_target_selector_userInfo_repeatsSelector
  , toleranceSelector
  , userInfoSelector
  , validSelector


  ) where

import Foreign.Ptr (Ptr, FunPtr)
import Foreign.C.Types

import ObjC.Runtime.Types
import ObjC.Runtime.Message (sendMessage, sendOwnedMessage, sendClassMessage, sendOwnedClassMessage)
import ObjC.Runtime.Selector (mkSelector)
import ObjC.Runtime.Class (getRequiredClass)

import ObjC.Foundation.Internal.Classes

-- | @+ timerWithTimeInterval:invocation:repeats:@
timerWithTimeInterval_invocation_repeats :: IsNSInvocation invocation => CDouble -> invocation -> Bool -> IO (Id NSTimer)
timerWithTimeInterval_invocation_repeats ti invocation yesOrNo =
  do
    cls' <- getRequiredClass "NSTimer"
    sendClassMessage cls' timerWithTimeInterval_invocation_repeatsSelector ti (toNSInvocation invocation) yesOrNo

-- | @+ scheduledTimerWithTimeInterval:invocation:repeats:@
scheduledTimerWithTimeInterval_invocation_repeats :: IsNSInvocation invocation => CDouble -> invocation -> Bool -> IO (Id NSTimer)
scheduledTimerWithTimeInterval_invocation_repeats ti invocation yesOrNo =
  do
    cls' <- getRequiredClass "NSTimer"
    sendClassMessage cls' scheduledTimerWithTimeInterval_invocation_repeatsSelector ti (toNSInvocation invocation) yesOrNo

-- | @+ timerWithTimeInterval:target:selector:userInfo:repeats:@
timerWithTimeInterval_target_selector_userInfo_repeats :: CDouble -> RawId -> Sel -> RawId -> Bool -> IO (Id NSTimer)
timerWithTimeInterval_target_selector_userInfo_repeats ti aTarget aSelector userInfo yesOrNo =
  do
    cls' <- getRequiredClass "NSTimer"
    sendClassMessage cls' timerWithTimeInterval_target_selector_userInfo_repeatsSelector ti aTarget aSelector userInfo yesOrNo

-- | @+ scheduledTimerWithTimeInterval:target:selector:userInfo:repeats:@
scheduledTimerWithTimeInterval_target_selector_userInfo_repeats :: CDouble -> RawId -> Sel -> RawId -> Bool -> IO (Id NSTimer)
scheduledTimerWithTimeInterval_target_selector_userInfo_repeats ti aTarget aSelector userInfo yesOrNo =
  do
    cls' <- getRequiredClass "NSTimer"
    sendClassMessage cls' scheduledTimerWithTimeInterval_target_selector_userInfo_repeatsSelector ti aTarget aSelector userInfo yesOrNo

-- | Creates and returns a new NSTimer object initialized with the specified block object. This timer needs to be scheduled on a run loop (via -[NSRunLoop addTimer:]) before it will fire. - parameter:  timeInterval  The number of seconds between firings of the timer. If seconds is less than or equal to 0.0, this method chooses the nonnegative value of 0.1 milliseconds instead - parameter:  repeats  If YES, the timer will repeatedly reschedule itself until invalidated. If NO, the timer will be invalidated after it fires. - parameter:  block  The execution body of the timer; the timer itself is passed as the parameter to this block when executed to aid in avoiding cyclical references
--
-- ObjC selector: @+ timerWithTimeInterval:repeats:block:@
timerWithTimeInterval_repeats_block :: CDouble -> Bool -> Ptr () -> IO (Id NSTimer)
timerWithTimeInterval_repeats_block interval repeats block =
  do
    cls' <- getRequiredClass "NSTimer"
    sendClassMessage cls' timerWithTimeInterval_repeats_blockSelector interval repeats block

-- | Creates and returns a new NSTimer object initialized with the specified block object and schedules it on the current run loop in the default mode. - parameter:  ti    The number of seconds between firings of the timer. If seconds is less than or equal to 0.0, this method chooses the nonnegative value of 0.1 milliseconds instead - parameter:  repeats  If YES, the timer will repeatedly reschedule itself until invalidated. If NO, the timer will be invalidated after it fires. - parameter:  block  The execution body of the timer; the timer itself is passed as the parameter to this block when executed to aid in avoiding cyclical references
--
-- ObjC selector: @+ scheduledTimerWithTimeInterval:repeats:block:@
scheduledTimerWithTimeInterval_repeats_block :: CDouble -> Bool -> Ptr () -> IO (Id NSTimer)
scheduledTimerWithTimeInterval_repeats_block interval repeats block =
  do
    cls' <- getRequiredClass "NSTimer"
    sendClassMessage cls' scheduledTimerWithTimeInterval_repeats_blockSelector interval repeats block

-- | Initializes a new NSTimer object using the block as the main body of execution for the timer. This timer needs to be scheduled on a run loop (via -[NSRunLoop addTimer:]) before it will fire. - parameter:  fireDate   The time at which the timer should first fire. - parameter:  interval  The number of seconds between firings of the timer. If seconds is less than or equal to 0.0, this method chooses the nonnegative value of 0.1 milliseconds instead - parameter:  repeats  If YES, the timer will repeatedly reschedule itself until invalidated. If NO, the timer will be invalidated after it fires. - parameter:  block  The execution body of the timer; the timer itself is passed as the parameter to this block when executed to aid in avoiding cyclical references
--
-- ObjC selector: @- initWithFireDate:interval:repeats:block:@
initWithFireDate_interval_repeats_block :: (IsNSTimer nsTimer, IsNSDate date) => nsTimer -> date -> CDouble -> Bool -> Ptr () -> IO (Id NSTimer)
initWithFireDate_interval_repeats_block nsTimer date interval repeats block =
  sendOwnedMessage nsTimer initWithFireDate_interval_repeats_blockSelector (toNSDate date) interval repeats block

-- | @- initWithFireDate:interval:target:selector:userInfo:repeats:@
initWithFireDate_interval_target_selector_userInfo_repeats :: (IsNSTimer nsTimer, IsNSDate date) => nsTimer -> date -> CDouble -> RawId -> Sel -> RawId -> Bool -> IO (Id NSTimer)
initWithFireDate_interval_target_selector_userInfo_repeats nsTimer date ti t s ui rep =
  sendOwnedMessage nsTimer initWithFireDate_interval_target_selector_userInfo_repeatsSelector (toNSDate date) ti t s ui rep

-- | @- fire@
fire :: IsNSTimer nsTimer => nsTimer -> IO ()
fire nsTimer =
  sendMessage nsTimer fireSelector

-- | @- invalidate@
invalidate :: IsNSTimer nsTimer => nsTimer -> IO ()
invalidate nsTimer =
  sendMessage nsTimer invalidateSelector

-- | @- fireDate@
fireDate :: IsNSTimer nsTimer => nsTimer -> IO (Id NSDate)
fireDate nsTimer =
  sendMessage nsTimer fireDateSelector

-- | @- setFireDate:@
setFireDate :: (IsNSTimer nsTimer, IsNSDate value) => nsTimer -> value -> IO ()
setFireDate nsTimer value =
  sendMessage nsTimer setFireDateSelector (toNSDate value)

-- | @- timeInterval@
timeInterval :: IsNSTimer nsTimer => nsTimer -> IO CDouble
timeInterval nsTimer =
  sendMessage nsTimer timeIntervalSelector

-- | @- tolerance@
tolerance :: IsNSTimer nsTimer => nsTimer -> IO CDouble
tolerance nsTimer =
  sendMessage nsTimer toleranceSelector

-- | @- setTolerance:@
setTolerance :: IsNSTimer nsTimer => nsTimer -> CDouble -> IO ()
setTolerance nsTimer value =
  sendMessage nsTimer setToleranceSelector value

-- | @- valid@
valid :: IsNSTimer nsTimer => nsTimer -> IO Bool
valid nsTimer =
  sendMessage nsTimer validSelector

-- | @- userInfo@
userInfo :: IsNSTimer nsTimer => nsTimer -> IO RawId
userInfo nsTimer =
  sendMessage nsTimer userInfoSelector

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @timerWithTimeInterval:invocation:repeats:@
timerWithTimeInterval_invocation_repeatsSelector :: Selector '[CDouble, Id NSInvocation, Bool] (Id NSTimer)
timerWithTimeInterval_invocation_repeatsSelector = mkSelector "timerWithTimeInterval:invocation:repeats:"

-- | @Selector@ for @scheduledTimerWithTimeInterval:invocation:repeats:@
scheduledTimerWithTimeInterval_invocation_repeatsSelector :: Selector '[CDouble, Id NSInvocation, Bool] (Id NSTimer)
scheduledTimerWithTimeInterval_invocation_repeatsSelector = mkSelector "scheduledTimerWithTimeInterval:invocation:repeats:"

-- | @Selector@ for @timerWithTimeInterval:target:selector:userInfo:repeats:@
timerWithTimeInterval_target_selector_userInfo_repeatsSelector :: Selector '[CDouble, RawId, Sel, RawId, Bool] (Id NSTimer)
timerWithTimeInterval_target_selector_userInfo_repeatsSelector = mkSelector "timerWithTimeInterval:target:selector:userInfo:repeats:"

-- | @Selector@ for @scheduledTimerWithTimeInterval:target:selector:userInfo:repeats:@
scheduledTimerWithTimeInterval_target_selector_userInfo_repeatsSelector :: Selector '[CDouble, RawId, Sel, RawId, Bool] (Id NSTimer)
scheduledTimerWithTimeInterval_target_selector_userInfo_repeatsSelector = mkSelector "scheduledTimerWithTimeInterval:target:selector:userInfo:repeats:"

-- | @Selector@ for @timerWithTimeInterval:repeats:block:@
timerWithTimeInterval_repeats_blockSelector :: Selector '[CDouble, Bool, Ptr ()] (Id NSTimer)
timerWithTimeInterval_repeats_blockSelector = mkSelector "timerWithTimeInterval:repeats:block:"

-- | @Selector@ for @scheduledTimerWithTimeInterval:repeats:block:@
scheduledTimerWithTimeInterval_repeats_blockSelector :: Selector '[CDouble, Bool, Ptr ()] (Id NSTimer)
scheduledTimerWithTimeInterval_repeats_blockSelector = mkSelector "scheduledTimerWithTimeInterval:repeats:block:"

-- | @Selector@ for @initWithFireDate:interval:repeats:block:@
initWithFireDate_interval_repeats_blockSelector :: Selector '[Id NSDate, CDouble, Bool, Ptr ()] (Id NSTimer)
initWithFireDate_interval_repeats_blockSelector = mkSelector "initWithFireDate:interval:repeats:block:"

-- | @Selector@ for @initWithFireDate:interval:target:selector:userInfo:repeats:@
initWithFireDate_interval_target_selector_userInfo_repeatsSelector :: Selector '[Id NSDate, CDouble, RawId, Sel, RawId, Bool] (Id NSTimer)
initWithFireDate_interval_target_selector_userInfo_repeatsSelector = mkSelector "initWithFireDate:interval:target:selector:userInfo:repeats:"

-- | @Selector@ for @fire@
fireSelector :: Selector '[] ()
fireSelector = mkSelector "fire"

-- | @Selector@ for @invalidate@
invalidateSelector :: Selector '[] ()
invalidateSelector = mkSelector "invalidate"

-- | @Selector@ for @fireDate@
fireDateSelector :: Selector '[] (Id NSDate)
fireDateSelector = mkSelector "fireDate"

-- | @Selector@ for @setFireDate:@
setFireDateSelector :: Selector '[Id NSDate] ()
setFireDateSelector = mkSelector "setFireDate:"

-- | @Selector@ for @timeInterval@
timeIntervalSelector :: Selector '[] CDouble
timeIntervalSelector = mkSelector "timeInterval"

-- | @Selector@ for @tolerance@
toleranceSelector :: Selector '[] CDouble
toleranceSelector = mkSelector "tolerance"

-- | @Selector@ for @setTolerance:@
setToleranceSelector :: Selector '[CDouble] ()
setToleranceSelector = mkSelector "setTolerance:"

-- | @Selector@ for @valid@
validSelector :: Selector '[] Bool
validSelector = mkSelector "valid"

-- | @Selector@ for @userInfo@
userInfoSelector :: Selector '[] RawId
userInfoSelector = mkSelector "userInfo"

