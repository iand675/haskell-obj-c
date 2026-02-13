{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Generated bindings for @NSRunLoop@.
module ObjC.Foundation.NSRunLoop
  ( NSRunLoop
  , IsNSRunLoop(..)
  , getCFRunLoop
  , addTimer_forMode
  , addPort_forMode
  , removePort_forMode
  , limitDateForMode
  , acceptInputForMode_beforeDate
  , performSelector_target_argument_order_modes
  , cancelPerformSelector_target_argument
  , cancelPerformSelectorsWithTarget
  , run
  , runUntilDate
  , runMode_beforeDate
  , configureAsServer
  , performInModes_block
  , performBlock
  , currentRunLoop
  , mainRunLoop
  , currentMode
  , acceptInputForMode_beforeDateSelector
  , addPort_forModeSelector
  , addTimer_forModeSelector
  , cancelPerformSelector_target_argumentSelector
  , cancelPerformSelectorsWithTargetSelector
  , configureAsServerSelector
  , currentModeSelector
  , currentRunLoopSelector
  , getCFRunLoopSelector
  , limitDateForModeSelector
  , mainRunLoopSelector
  , performBlockSelector
  , performInModes_blockSelector
  , performSelector_target_argument_order_modesSelector
  , removePort_forModeSelector
  , runMode_beforeDateSelector
  , runSelector
  , runUntilDateSelector


  ) where

import Foreign.Ptr (Ptr, FunPtr)
import Foreign.C.Types

import ObjC.Runtime.Types
import ObjC.Runtime.Message (sendMessage, sendOwnedMessage, sendClassMessage, sendOwnedClassMessage)
import ObjC.Runtime.Selector (mkSelector)
import ObjC.Runtime.Class (getRequiredClass)

import ObjC.Foundation.Internal.Classes

-- | @- getCFRunLoop@
getCFRunLoop :: IsNSRunLoop nsRunLoop => nsRunLoop -> IO (Ptr ())
getCFRunLoop nsRunLoop =
  sendMessage nsRunLoop getCFRunLoopSelector

-- | @- addTimer:forMode:@
addTimer_forMode :: (IsNSRunLoop nsRunLoop, IsNSTimer timer, IsNSString mode) => nsRunLoop -> timer -> mode -> IO ()
addTimer_forMode nsRunLoop timer mode =
  sendMessage nsRunLoop addTimer_forModeSelector (toNSTimer timer) (toNSString mode)

-- | @- addPort:forMode:@
addPort_forMode :: (IsNSRunLoop nsRunLoop, IsNSPort aPort, IsNSString mode) => nsRunLoop -> aPort -> mode -> IO ()
addPort_forMode nsRunLoop aPort mode =
  sendMessage nsRunLoop addPort_forModeSelector (toNSPort aPort) (toNSString mode)

-- | @- removePort:forMode:@
removePort_forMode :: (IsNSRunLoop nsRunLoop, IsNSPort aPort, IsNSString mode) => nsRunLoop -> aPort -> mode -> IO ()
removePort_forMode nsRunLoop aPort mode =
  sendMessage nsRunLoop removePort_forModeSelector (toNSPort aPort) (toNSString mode)

-- | @- limitDateForMode:@
limitDateForMode :: (IsNSRunLoop nsRunLoop, IsNSString mode) => nsRunLoop -> mode -> IO (Id NSDate)
limitDateForMode nsRunLoop mode =
  sendMessage nsRunLoop limitDateForModeSelector (toNSString mode)

-- | @- acceptInputForMode:beforeDate:@
acceptInputForMode_beforeDate :: (IsNSRunLoop nsRunLoop, IsNSString mode, IsNSDate limitDate) => nsRunLoop -> mode -> limitDate -> IO ()
acceptInputForMode_beforeDate nsRunLoop mode limitDate =
  sendMessage nsRunLoop acceptInputForMode_beforeDateSelector (toNSString mode) (toNSDate limitDate)

-- | @- performSelector:target:argument:order:modes:@
performSelector_target_argument_order_modes :: (IsNSRunLoop nsRunLoop, IsNSArray modes) => nsRunLoop -> Sel -> RawId -> RawId -> CULong -> modes -> IO ()
performSelector_target_argument_order_modes nsRunLoop aSelector target arg order modes =
  sendMessage nsRunLoop performSelector_target_argument_order_modesSelector aSelector target arg order (toNSArray modes)

-- | @- cancelPerformSelector:target:argument:@
cancelPerformSelector_target_argument :: IsNSRunLoop nsRunLoop => nsRunLoop -> Sel -> RawId -> RawId -> IO ()
cancelPerformSelector_target_argument nsRunLoop aSelector target arg =
  sendMessage nsRunLoop cancelPerformSelector_target_argumentSelector aSelector target arg

-- | @- cancelPerformSelectorsWithTarget:@
cancelPerformSelectorsWithTarget :: IsNSRunLoop nsRunLoop => nsRunLoop -> RawId -> IO ()
cancelPerformSelectorsWithTarget nsRunLoop target =
  sendMessage nsRunLoop cancelPerformSelectorsWithTargetSelector target

-- | @- run@
run :: IsNSRunLoop nsRunLoop => nsRunLoop -> IO ()
run nsRunLoop =
  sendMessage nsRunLoop runSelector

-- | @- runUntilDate:@
runUntilDate :: (IsNSRunLoop nsRunLoop, IsNSDate limitDate) => nsRunLoop -> limitDate -> IO ()
runUntilDate nsRunLoop limitDate =
  sendMessage nsRunLoop runUntilDateSelector (toNSDate limitDate)

-- | @- runMode:beforeDate:@
runMode_beforeDate :: (IsNSRunLoop nsRunLoop, IsNSString mode, IsNSDate limitDate) => nsRunLoop -> mode -> limitDate -> IO Bool
runMode_beforeDate nsRunLoop mode limitDate =
  sendMessage nsRunLoop runMode_beforeDateSelector (toNSString mode) (toNSDate limitDate)

-- | @- configureAsServer@
configureAsServer :: IsNSRunLoop nsRunLoop => nsRunLoop -> IO ()
configureAsServer nsRunLoop =
  sendMessage nsRunLoop configureAsServerSelector

-- | Schedules the execution of a block on the target run loop in given modes. - parameter: modes   An array of input modes for which the block may be executed. - parameter: block   The block to execute
--
-- ObjC selector: @- performInModes:block:@
performInModes_block :: (IsNSRunLoop nsRunLoop, IsNSArray modes) => nsRunLoop -> modes -> Ptr () -> IO ()
performInModes_block nsRunLoop modes block =
  sendMessage nsRunLoop performInModes_blockSelector (toNSArray modes) block

-- | Schedules the execution of a block on the target run loop. - parameter: block   The block to execute
--
-- ObjC selector: @- performBlock:@
performBlock :: IsNSRunLoop nsRunLoop => nsRunLoop -> Ptr () -> IO ()
performBlock nsRunLoop block =
  sendMessage nsRunLoop performBlockSelector block

-- | @+ currentRunLoop@
currentRunLoop :: IO (Id NSRunLoop)
currentRunLoop  =
  do
    cls' <- getRequiredClass "NSRunLoop"
    sendClassMessage cls' currentRunLoopSelector

-- | @+ mainRunLoop@
mainRunLoop :: IO (Id NSRunLoop)
mainRunLoop  =
  do
    cls' <- getRequiredClass "NSRunLoop"
    sendClassMessage cls' mainRunLoopSelector

-- | @- currentMode@
currentMode :: IsNSRunLoop nsRunLoop => nsRunLoop -> IO (Id NSString)
currentMode nsRunLoop =
  sendMessage nsRunLoop currentModeSelector

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @getCFRunLoop@
getCFRunLoopSelector :: Selector '[] (Ptr ())
getCFRunLoopSelector = mkSelector "getCFRunLoop"

-- | @Selector@ for @addTimer:forMode:@
addTimer_forModeSelector :: Selector '[Id NSTimer, Id NSString] ()
addTimer_forModeSelector = mkSelector "addTimer:forMode:"

-- | @Selector@ for @addPort:forMode:@
addPort_forModeSelector :: Selector '[Id NSPort, Id NSString] ()
addPort_forModeSelector = mkSelector "addPort:forMode:"

-- | @Selector@ for @removePort:forMode:@
removePort_forModeSelector :: Selector '[Id NSPort, Id NSString] ()
removePort_forModeSelector = mkSelector "removePort:forMode:"

-- | @Selector@ for @limitDateForMode:@
limitDateForModeSelector :: Selector '[Id NSString] (Id NSDate)
limitDateForModeSelector = mkSelector "limitDateForMode:"

-- | @Selector@ for @acceptInputForMode:beforeDate:@
acceptInputForMode_beforeDateSelector :: Selector '[Id NSString, Id NSDate] ()
acceptInputForMode_beforeDateSelector = mkSelector "acceptInputForMode:beforeDate:"

-- | @Selector@ for @performSelector:target:argument:order:modes:@
performSelector_target_argument_order_modesSelector :: Selector '[Sel, RawId, RawId, CULong, Id NSArray] ()
performSelector_target_argument_order_modesSelector = mkSelector "performSelector:target:argument:order:modes:"

-- | @Selector@ for @cancelPerformSelector:target:argument:@
cancelPerformSelector_target_argumentSelector :: Selector '[Sel, RawId, RawId] ()
cancelPerformSelector_target_argumentSelector = mkSelector "cancelPerformSelector:target:argument:"

-- | @Selector@ for @cancelPerformSelectorsWithTarget:@
cancelPerformSelectorsWithTargetSelector :: Selector '[RawId] ()
cancelPerformSelectorsWithTargetSelector = mkSelector "cancelPerformSelectorsWithTarget:"

-- | @Selector@ for @run@
runSelector :: Selector '[] ()
runSelector = mkSelector "run"

-- | @Selector@ for @runUntilDate:@
runUntilDateSelector :: Selector '[Id NSDate] ()
runUntilDateSelector = mkSelector "runUntilDate:"

-- | @Selector@ for @runMode:beforeDate:@
runMode_beforeDateSelector :: Selector '[Id NSString, Id NSDate] Bool
runMode_beforeDateSelector = mkSelector "runMode:beforeDate:"

-- | @Selector@ for @configureAsServer@
configureAsServerSelector :: Selector '[] ()
configureAsServerSelector = mkSelector "configureAsServer"

-- | @Selector@ for @performInModes:block:@
performInModes_blockSelector :: Selector '[Id NSArray, Ptr ()] ()
performInModes_blockSelector = mkSelector "performInModes:block:"

-- | @Selector@ for @performBlock:@
performBlockSelector :: Selector '[Ptr ()] ()
performBlockSelector = mkSelector "performBlock:"

-- | @Selector@ for @currentRunLoop@
currentRunLoopSelector :: Selector '[] (Id NSRunLoop)
currentRunLoopSelector = mkSelector "currentRunLoop"

-- | @Selector@ for @mainRunLoop@
mainRunLoopSelector :: Selector '[] (Id NSRunLoop)
mainRunLoopSelector = mkSelector "mainRunLoop"

-- | @Selector@ for @currentMode@
currentModeSelector :: Selector '[] (Id NSString)
currentModeSelector = mkSelector "currentMode"

