{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE DataKinds #-}
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
  , completionHandlerSelector
  , scheduledHandlerSelector
  , setCompletionHandlerSelector
  , setScheduledHandlerSelector
  , setWaitUntilCompletedSelector
  , signalEvent_atExecutionEvent_valueSelector
  , waitForEvent_valueSelector
  , waitUntilCompletedSelector

  -- * Enum types
  , MPSGraphExecutionStage(MPSGraphExecutionStage)
  , pattern MPSGraphExecutionStageCompleted

  ) where

import Foreign.Ptr (Ptr, FunPtr)
import Foreign.C.Types

import ObjC.Runtime.Types
import ObjC.Runtime.Message (sendMessage, sendOwnedMessage, sendClassMessage, sendOwnedClassMessage)
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
waitForEvent_value mpsGraphExecutableExecutionDescriptor event value =
  sendMessage mpsGraphExecutableExecutionDescriptor waitForEvent_valueSelector event value

-- | Signals these shared events at execution stage and immediately proceeds.
--
-- - Parameters:   - event: Shared event to signal.   - executionStage: Execution stage to signal event at.   - value: Value for shared event to wait on.
--
-- ObjC selector: @- signalEvent:atExecutionEvent:value:@
signalEvent_atExecutionEvent_value :: IsMPSGraphExecutableExecutionDescriptor mpsGraphExecutableExecutionDescriptor => mpsGraphExecutableExecutionDescriptor -> RawId -> MPSGraphExecutionStage -> CULong -> IO ()
signalEvent_atExecutionEvent_value mpsGraphExecutableExecutionDescriptor event executionStage value =
  sendMessage mpsGraphExecutableExecutionDescriptor signalEvent_atExecutionEvent_valueSelector event executionStage value

-- | A notification that appears when graph-executable execution is scheduled.
--
-- Default value is nil.
--
-- ObjC selector: @- scheduledHandler@
scheduledHandler :: IsMPSGraphExecutableExecutionDescriptor mpsGraphExecutableExecutionDescriptor => mpsGraphExecutableExecutionDescriptor -> IO (Ptr ())
scheduledHandler mpsGraphExecutableExecutionDescriptor =
  sendMessage mpsGraphExecutableExecutionDescriptor scheduledHandlerSelector

-- | A notification that appears when graph-executable execution is scheduled.
--
-- Default value is nil.
--
-- ObjC selector: @- setScheduledHandler:@
setScheduledHandler :: IsMPSGraphExecutableExecutionDescriptor mpsGraphExecutableExecutionDescriptor => mpsGraphExecutableExecutionDescriptor -> Ptr () -> IO ()
setScheduledHandler mpsGraphExecutableExecutionDescriptor value =
  sendMessage mpsGraphExecutableExecutionDescriptor setScheduledHandlerSelector value

-- | A notification that appears when graph-executable execution is finished.
--
-- Default value is nil.
--
-- ObjC selector: @- completionHandler@
completionHandler :: IsMPSGraphExecutableExecutionDescriptor mpsGraphExecutableExecutionDescriptor => mpsGraphExecutableExecutionDescriptor -> IO (Ptr ())
completionHandler mpsGraphExecutableExecutionDescriptor =
  sendMessage mpsGraphExecutableExecutionDescriptor completionHandlerSelector

-- | A notification that appears when graph-executable execution is finished.
--
-- Default value is nil.
--
-- ObjC selector: @- setCompletionHandler:@
setCompletionHandler :: IsMPSGraphExecutableExecutionDescriptor mpsGraphExecutableExecutionDescriptor => mpsGraphExecutableExecutionDescriptor -> Ptr () -> IO ()
setCompletionHandler mpsGraphExecutableExecutionDescriptor value =
  sendMessage mpsGraphExecutableExecutionDescriptor setCompletionHandlerSelector value

-- | Flag for the graph executable to wait till the execution has completed.
--
-- Default value is false.
--
-- ObjC selector: @- waitUntilCompleted@
waitUntilCompleted :: IsMPSGraphExecutableExecutionDescriptor mpsGraphExecutableExecutionDescriptor => mpsGraphExecutableExecutionDescriptor -> IO Bool
waitUntilCompleted mpsGraphExecutableExecutionDescriptor =
  sendMessage mpsGraphExecutableExecutionDescriptor waitUntilCompletedSelector

-- | Flag for the graph executable to wait till the execution has completed.
--
-- Default value is false.
--
-- ObjC selector: @- setWaitUntilCompleted:@
setWaitUntilCompleted :: IsMPSGraphExecutableExecutionDescriptor mpsGraphExecutableExecutionDescriptor => mpsGraphExecutableExecutionDescriptor -> Bool -> IO ()
setWaitUntilCompleted mpsGraphExecutableExecutionDescriptor value =
  sendMessage mpsGraphExecutableExecutionDescriptor setWaitUntilCompletedSelector value

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @waitForEvent:value:@
waitForEvent_valueSelector :: Selector '[RawId, CULong] ()
waitForEvent_valueSelector = mkSelector "waitForEvent:value:"

-- | @Selector@ for @signalEvent:atExecutionEvent:value:@
signalEvent_atExecutionEvent_valueSelector :: Selector '[RawId, MPSGraphExecutionStage, CULong] ()
signalEvent_atExecutionEvent_valueSelector = mkSelector "signalEvent:atExecutionEvent:value:"

-- | @Selector@ for @scheduledHandler@
scheduledHandlerSelector :: Selector '[] (Ptr ())
scheduledHandlerSelector = mkSelector "scheduledHandler"

-- | @Selector@ for @setScheduledHandler:@
setScheduledHandlerSelector :: Selector '[Ptr ()] ()
setScheduledHandlerSelector = mkSelector "setScheduledHandler:"

-- | @Selector@ for @completionHandler@
completionHandlerSelector :: Selector '[] (Ptr ())
completionHandlerSelector = mkSelector "completionHandler"

-- | @Selector@ for @setCompletionHandler:@
setCompletionHandlerSelector :: Selector '[Ptr ()] ()
setCompletionHandlerSelector = mkSelector "setCompletionHandler:"

-- | @Selector@ for @waitUntilCompleted@
waitUntilCompletedSelector :: Selector '[] Bool
waitUntilCompletedSelector = mkSelector "waitUntilCompleted"

-- | @Selector@ for @setWaitUntilCompleted:@
setWaitUntilCompletedSelector :: Selector '[Bool] ()
setWaitUntilCompletedSelector = mkSelector "setWaitUntilCompleted:"

