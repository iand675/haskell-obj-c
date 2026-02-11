{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | A class that consists of all the levers  to synchronize and schedule executable execution.
--
-- Generated bindings for @MPSGraphExecutableExecutionDescriptor@.
module ObjC.MetalPerformanceShadersGraph.MPSGraphExecutableExecutionDescriptor
  ( MPSGraphExecutableExecutionDescriptor
  , IsMPSGraphExecutableExecutionDescriptor(..)
  , waitForEvent_value
  , signalEvent_atExecutionEvent_value
  , scheduledHandler
  , setScheduledHandler
  , completionHandler
  , setCompletionHandler
  , waitUntilCompleted
  , setWaitUntilCompleted
  , waitForEvent_valueSelector
  , signalEvent_atExecutionEvent_valueSelector
  , scheduledHandlerSelector
  , setScheduledHandlerSelector
  , completionHandlerSelector
  , setCompletionHandlerSelector
  , waitUntilCompletedSelector
  , setWaitUntilCompletedSelector

  -- * Enum types
  , MPSGraphExecutionStage(MPSGraphExecutionStage)
  , pattern MPSGraphExecutionStageCompleted

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

import ObjC.MetalPerformanceShadersGraph.Internal.Classes
import ObjC.MetalPerformanceShadersGraph.Internal.Enums
import ObjC.Foundation.Internal.Classes

-- | Waits on these shared events before scheduling execution on the HW.
--
-- This does not include encoding which can still continue.
--
-- - Parameters:   - event: Shared event to wait on.   - value: Value for shared event to wait on.
--
-- ObjC selector: @- waitForEvent:value:@
waitForEvent_value :: IsMPSGraphExecutableExecutionDescriptor mpsGraphExecutableExecutionDescriptor => mpsGraphExecutableExecutionDescriptor -> RawId -> CULong -> IO ()
waitForEvent_value mpsGraphExecutableExecutionDescriptor  event value =
  sendMsg mpsGraphExecutableExecutionDescriptor (mkSelector "waitForEvent:value:") retVoid [argPtr (castPtr (unRawId event) :: Ptr ()), argCULong (fromIntegral value)]

-- | Signals these shared events at execution stage and immediately proceeds.
--
-- - Parameters:   - event: Shared event to signal.   - executionStage: Execution stage to signal event at.   - value: Value for shared event to wait on.
--
-- ObjC selector: @- signalEvent:atExecutionEvent:value:@
signalEvent_atExecutionEvent_value :: IsMPSGraphExecutableExecutionDescriptor mpsGraphExecutableExecutionDescriptor => mpsGraphExecutableExecutionDescriptor -> RawId -> MPSGraphExecutionStage -> CULong -> IO ()
signalEvent_atExecutionEvent_value mpsGraphExecutableExecutionDescriptor  event executionStage value =
  sendMsg mpsGraphExecutableExecutionDescriptor (mkSelector "signalEvent:atExecutionEvent:value:") retVoid [argPtr (castPtr (unRawId event) :: Ptr ()), argCULong (coerce executionStage), argCULong (fromIntegral value)]

-- | A notification that appears when graph-executable execution is scheduled.
--
-- Default value is nil.
--
-- ObjC selector: @- scheduledHandler@
scheduledHandler :: IsMPSGraphExecutableExecutionDescriptor mpsGraphExecutableExecutionDescriptor => mpsGraphExecutableExecutionDescriptor -> IO (Ptr ())
scheduledHandler mpsGraphExecutableExecutionDescriptor  =
  fmap castPtr $ sendMsg mpsGraphExecutableExecutionDescriptor (mkSelector "scheduledHandler") (retPtr retVoid) []

-- | A notification that appears when graph-executable execution is scheduled.
--
-- Default value is nil.
--
-- ObjC selector: @- setScheduledHandler:@
setScheduledHandler :: IsMPSGraphExecutableExecutionDescriptor mpsGraphExecutableExecutionDescriptor => mpsGraphExecutableExecutionDescriptor -> Ptr () -> IO ()
setScheduledHandler mpsGraphExecutableExecutionDescriptor  value =
  sendMsg mpsGraphExecutableExecutionDescriptor (mkSelector "setScheduledHandler:") retVoid [argPtr (castPtr value :: Ptr ())]

-- | A notification that appears when graph-executable execution is finished.
--
-- Default value is nil.
--
-- ObjC selector: @- completionHandler@
completionHandler :: IsMPSGraphExecutableExecutionDescriptor mpsGraphExecutableExecutionDescriptor => mpsGraphExecutableExecutionDescriptor -> IO (Ptr ())
completionHandler mpsGraphExecutableExecutionDescriptor  =
  fmap castPtr $ sendMsg mpsGraphExecutableExecutionDescriptor (mkSelector "completionHandler") (retPtr retVoid) []

-- | A notification that appears when graph-executable execution is finished.
--
-- Default value is nil.
--
-- ObjC selector: @- setCompletionHandler:@
setCompletionHandler :: IsMPSGraphExecutableExecutionDescriptor mpsGraphExecutableExecutionDescriptor => mpsGraphExecutableExecutionDescriptor -> Ptr () -> IO ()
setCompletionHandler mpsGraphExecutableExecutionDescriptor  value =
  sendMsg mpsGraphExecutableExecutionDescriptor (mkSelector "setCompletionHandler:") retVoid [argPtr (castPtr value :: Ptr ())]

-- | Flag for the graph executable to wait till the execution has completed.
--
-- Default value is false.
--
-- ObjC selector: @- waitUntilCompleted@
waitUntilCompleted :: IsMPSGraphExecutableExecutionDescriptor mpsGraphExecutableExecutionDescriptor => mpsGraphExecutableExecutionDescriptor -> IO Bool
waitUntilCompleted mpsGraphExecutableExecutionDescriptor  =
  fmap ((/= 0) :: CULong -> Bool) $ sendMsg mpsGraphExecutableExecutionDescriptor (mkSelector "waitUntilCompleted") retCULong []

-- | Flag for the graph executable to wait till the execution has completed.
--
-- Default value is false.
--
-- ObjC selector: @- setWaitUntilCompleted:@
setWaitUntilCompleted :: IsMPSGraphExecutableExecutionDescriptor mpsGraphExecutableExecutionDescriptor => mpsGraphExecutableExecutionDescriptor -> Bool -> IO ()
setWaitUntilCompleted mpsGraphExecutableExecutionDescriptor  value =
  sendMsg mpsGraphExecutableExecutionDescriptor (mkSelector "setWaitUntilCompleted:") retVoid [argCULong (if value then 1 else 0)]

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @waitForEvent:value:@
waitForEvent_valueSelector :: Selector
waitForEvent_valueSelector = mkSelector "waitForEvent:value:"

-- | @Selector@ for @signalEvent:atExecutionEvent:value:@
signalEvent_atExecutionEvent_valueSelector :: Selector
signalEvent_atExecutionEvent_valueSelector = mkSelector "signalEvent:atExecutionEvent:value:"

-- | @Selector@ for @scheduledHandler@
scheduledHandlerSelector :: Selector
scheduledHandlerSelector = mkSelector "scheduledHandler"

-- | @Selector@ for @setScheduledHandler:@
setScheduledHandlerSelector :: Selector
setScheduledHandlerSelector = mkSelector "setScheduledHandler:"

-- | @Selector@ for @completionHandler@
completionHandlerSelector :: Selector
completionHandlerSelector = mkSelector "completionHandler"

-- | @Selector@ for @setCompletionHandler:@
setCompletionHandlerSelector :: Selector
setCompletionHandlerSelector = mkSelector "setCompletionHandler:"

-- | @Selector@ for @waitUntilCompleted@
waitUntilCompletedSelector :: Selector
waitUntilCompletedSelector = mkSelector "waitUntilCompleted"

-- | @Selector@ for @setWaitUntilCompleted:@
setWaitUntilCompletedSelector :: Selector
setWaitUntilCompletedSelector = mkSelector "setWaitUntilCompleted:"

