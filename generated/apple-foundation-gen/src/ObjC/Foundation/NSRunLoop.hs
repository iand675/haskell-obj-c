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
  , getCFRunLoopSelector
  , addTimer_forModeSelector
  , addPort_forModeSelector
  , removePort_forModeSelector
  , limitDateForModeSelector
  , acceptInputForMode_beforeDateSelector
  , performSelector_target_argument_order_modesSelector
  , cancelPerformSelector_target_argumentSelector
  , cancelPerformSelectorsWithTargetSelector
  , runSelector
  , runUntilDateSelector
  , runMode_beforeDateSelector
  , configureAsServerSelector
  , performInModes_blockSelector
  , performBlockSelector
  , currentRunLoopSelector
  , mainRunLoopSelector
  , currentModeSelector


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

-- | @- getCFRunLoop@
getCFRunLoop :: IsNSRunLoop nsRunLoop => nsRunLoop -> IO (Ptr ())
getCFRunLoop nsRunLoop  =
    fmap castPtr $ sendMsg nsRunLoop (mkSelector "getCFRunLoop") (retPtr retVoid) []

-- | @- addTimer:forMode:@
addTimer_forMode :: (IsNSRunLoop nsRunLoop, IsNSTimer timer, IsNSString mode) => nsRunLoop -> timer -> mode -> IO ()
addTimer_forMode nsRunLoop  timer mode =
  withObjCPtr timer $ \raw_timer ->
    withObjCPtr mode $ \raw_mode ->
        sendMsg nsRunLoop (mkSelector "addTimer:forMode:") retVoid [argPtr (castPtr raw_timer :: Ptr ()), argPtr (castPtr raw_mode :: Ptr ())]

-- | @- addPort:forMode:@
addPort_forMode :: (IsNSRunLoop nsRunLoop, IsNSPort aPort, IsNSString mode) => nsRunLoop -> aPort -> mode -> IO ()
addPort_forMode nsRunLoop  aPort mode =
  withObjCPtr aPort $ \raw_aPort ->
    withObjCPtr mode $ \raw_mode ->
        sendMsg nsRunLoop (mkSelector "addPort:forMode:") retVoid [argPtr (castPtr raw_aPort :: Ptr ()), argPtr (castPtr raw_mode :: Ptr ())]

-- | @- removePort:forMode:@
removePort_forMode :: (IsNSRunLoop nsRunLoop, IsNSPort aPort, IsNSString mode) => nsRunLoop -> aPort -> mode -> IO ()
removePort_forMode nsRunLoop  aPort mode =
  withObjCPtr aPort $ \raw_aPort ->
    withObjCPtr mode $ \raw_mode ->
        sendMsg nsRunLoop (mkSelector "removePort:forMode:") retVoid [argPtr (castPtr raw_aPort :: Ptr ()), argPtr (castPtr raw_mode :: Ptr ())]

-- | @- limitDateForMode:@
limitDateForMode :: (IsNSRunLoop nsRunLoop, IsNSString mode) => nsRunLoop -> mode -> IO (Id NSDate)
limitDateForMode nsRunLoop  mode =
  withObjCPtr mode $ \raw_mode ->
      sendMsg nsRunLoop (mkSelector "limitDateForMode:") (retPtr retVoid) [argPtr (castPtr raw_mode :: Ptr ())] >>= retainedObject . castPtr

-- | @- acceptInputForMode:beforeDate:@
acceptInputForMode_beforeDate :: (IsNSRunLoop nsRunLoop, IsNSString mode, IsNSDate limitDate) => nsRunLoop -> mode -> limitDate -> IO ()
acceptInputForMode_beforeDate nsRunLoop  mode limitDate =
  withObjCPtr mode $ \raw_mode ->
    withObjCPtr limitDate $ \raw_limitDate ->
        sendMsg nsRunLoop (mkSelector "acceptInputForMode:beforeDate:") retVoid [argPtr (castPtr raw_mode :: Ptr ()), argPtr (castPtr raw_limitDate :: Ptr ())]

-- | @- performSelector:target:argument:order:modes:@
performSelector_target_argument_order_modes :: (IsNSRunLoop nsRunLoop, IsNSArray modes) => nsRunLoop -> Selector -> RawId -> RawId -> CULong -> modes -> IO ()
performSelector_target_argument_order_modes nsRunLoop  aSelector target arg order modes =
  withObjCPtr modes $ \raw_modes ->
      sendMsg nsRunLoop (mkSelector "performSelector:target:argument:order:modes:") retVoid [argPtr (unSelector aSelector), argPtr (castPtr (unRawId target) :: Ptr ()), argPtr (castPtr (unRawId arg) :: Ptr ()), argCULong order, argPtr (castPtr raw_modes :: Ptr ())]

-- | @- cancelPerformSelector:target:argument:@
cancelPerformSelector_target_argument :: IsNSRunLoop nsRunLoop => nsRunLoop -> Selector -> RawId -> RawId -> IO ()
cancelPerformSelector_target_argument nsRunLoop  aSelector target arg =
    sendMsg nsRunLoop (mkSelector "cancelPerformSelector:target:argument:") retVoid [argPtr (unSelector aSelector), argPtr (castPtr (unRawId target) :: Ptr ()), argPtr (castPtr (unRawId arg) :: Ptr ())]

-- | @- cancelPerformSelectorsWithTarget:@
cancelPerformSelectorsWithTarget :: IsNSRunLoop nsRunLoop => nsRunLoop -> RawId -> IO ()
cancelPerformSelectorsWithTarget nsRunLoop  target =
    sendMsg nsRunLoop (mkSelector "cancelPerformSelectorsWithTarget:") retVoid [argPtr (castPtr (unRawId target) :: Ptr ())]

-- | @- run@
run :: IsNSRunLoop nsRunLoop => nsRunLoop -> IO ()
run nsRunLoop  =
    sendMsg nsRunLoop (mkSelector "run") retVoid []

-- | @- runUntilDate:@
runUntilDate :: (IsNSRunLoop nsRunLoop, IsNSDate limitDate) => nsRunLoop -> limitDate -> IO ()
runUntilDate nsRunLoop  limitDate =
  withObjCPtr limitDate $ \raw_limitDate ->
      sendMsg nsRunLoop (mkSelector "runUntilDate:") retVoid [argPtr (castPtr raw_limitDate :: Ptr ())]

-- | @- runMode:beforeDate:@
runMode_beforeDate :: (IsNSRunLoop nsRunLoop, IsNSString mode, IsNSDate limitDate) => nsRunLoop -> mode -> limitDate -> IO Bool
runMode_beforeDate nsRunLoop  mode limitDate =
  withObjCPtr mode $ \raw_mode ->
    withObjCPtr limitDate $ \raw_limitDate ->
        fmap ((/= 0) :: CULong -> Bool) $ sendMsg nsRunLoop (mkSelector "runMode:beforeDate:") retCULong [argPtr (castPtr raw_mode :: Ptr ()), argPtr (castPtr raw_limitDate :: Ptr ())]

-- | @- configureAsServer@
configureAsServer :: IsNSRunLoop nsRunLoop => nsRunLoop -> IO ()
configureAsServer nsRunLoop  =
    sendMsg nsRunLoop (mkSelector "configureAsServer") retVoid []

-- | Schedules the execution of a block on the target run loop in given modes. - parameter: modes   An array of input modes for which the block may be executed. - parameter: block   The block to execute
--
-- ObjC selector: @- performInModes:block:@
performInModes_block :: (IsNSRunLoop nsRunLoop, IsNSArray modes) => nsRunLoop -> modes -> Ptr () -> IO ()
performInModes_block nsRunLoop  modes block =
  withObjCPtr modes $ \raw_modes ->
      sendMsg nsRunLoop (mkSelector "performInModes:block:") retVoid [argPtr (castPtr raw_modes :: Ptr ()), argPtr (castPtr block :: Ptr ())]

-- | Schedules the execution of a block on the target run loop. - parameter: block   The block to execute
--
-- ObjC selector: @- performBlock:@
performBlock :: IsNSRunLoop nsRunLoop => nsRunLoop -> Ptr () -> IO ()
performBlock nsRunLoop  block =
    sendMsg nsRunLoop (mkSelector "performBlock:") retVoid [argPtr (castPtr block :: Ptr ())]

-- | @+ currentRunLoop@
currentRunLoop :: IO (Id NSRunLoop)
currentRunLoop  =
  do
    cls' <- getRequiredClass "NSRunLoop"
    sendClassMsg cls' (mkSelector "currentRunLoop") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @+ mainRunLoop@
mainRunLoop :: IO (Id NSRunLoop)
mainRunLoop  =
  do
    cls' <- getRequiredClass "NSRunLoop"
    sendClassMsg cls' (mkSelector "mainRunLoop") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- currentMode@
currentMode :: IsNSRunLoop nsRunLoop => nsRunLoop -> IO (Id NSString)
currentMode nsRunLoop  =
    sendMsg nsRunLoop (mkSelector "currentMode") (retPtr retVoid) [] >>= retainedObject . castPtr

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @getCFRunLoop@
getCFRunLoopSelector :: Selector
getCFRunLoopSelector = mkSelector "getCFRunLoop"

-- | @Selector@ for @addTimer:forMode:@
addTimer_forModeSelector :: Selector
addTimer_forModeSelector = mkSelector "addTimer:forMode:"

-- | @Selector@ for @addPort:forMode:@
addPort_forModeSelector :: Selector
addPort_forModeSelector = mkSelector "addPort:forMode:"

-- | @Selector@ for @removePort:forMode:@
removePort_forModeSelector :: Selector
removePort_forModeSelector = mkSelector "removePort:forMode:"

-- | @Selector@ for @limitDateForMode:@
limitDateForModeSelector :: Selector
limitDateForModeSelector = mkSelector "limitDateForMode:"

-- | @Selector@ for @acceptInputForMode:beforeDate:@
acceptInputForMode_beforeDateSelector :: Selector
acceptInputForMode_beforeDateSelector = mkSelector "acceptInputForMode:beforeDate:"

-- | @Selector@ for @performSelector:target:argument:order:modes:@
performSelector_target_argument_order_modesSelector :: Selector
performSelector_target_argument_order_modesSelector = mkSelector "performSelector:target:argument:order:modes:"

-- | @Selector@ for @cancelPerformSelector:target:argument:@
cancelPerformSelector_target_argumentSelector :: Selector
cancelPerformSelector_target_argumentSelector = mkSelector "cancelPerformSelector:target:argument:"

-- | @Selector@ for @cancelPerformSelectorsWithTarget:@
cancelPerformSelectorsWithTargetSelector :: Selector
cancelPerformSelectorsWithTargetSelector = mkSelector "cancelPerformSelectorsWithTarget:"

-- | @Selector@ for @run@
runSelector :: Selector
runSelector = mkSelector "run"

-- | @Selector@ for @runUntilDate:@
runUntilDateSelector :: Selector
runUntilDateSelector = mkSelector "runUntilDate:"

-- | @Selector@ for @runMode:beforeDate:@
runMode_beforeDateSelector :: Selector
runMode_beforeDateSelector = mkSelector "runMode:beforeDate:"

-- | @Selector@ for @configureAsServer@
configureAsServerSelector :: Selector
configureAsServerSelector = mkSelector "configureAsServer"

-- | @Selector@ for @performInModes:block:@
performInModes_blockSelector :: Selector
performInModes_blockSelector = mkSelector "performInModes:block:"

-- | @Selector@ for @performBlock:@
performBlockSelector :: Selector
performBlockSelector = mkSelector "performBlock:"

-- | @Selector@ for @currentRunLoop@
currentRunLoopSelector :: Selector
currentRunLoopSelector = mkSelector "currentRunLoop"

-- | @Selector@ for @mainRunLoop@
mainRunLoopSelector :: Selector
mainRunLoopSelector = mkSelector "mainRunLoop"

-- | @Selector@ for @currentMode@
currentModeSelector :: Selector
currentModeSelector = mkSelector "currentMode"

