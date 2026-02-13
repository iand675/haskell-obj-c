{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | A class that consists of all the levers  to synchronize and schedule graph execution.
--
-- Generated bindings for @MPSGraphExecutionDescriptor@.
module ObjC.MetalPerformanceShadersGraph.MPSGraphExecutionDescriptor
  ( MPSGraphExecutionDescriptor
  , IsMPSGraphExecutionDescriptor(..)
  , waitForEvent_value
  , signalEvent_atExecutionEvent_value
  , scheduledHandler
  , setScheduledHandler
  , completionHandler
  , setCompletionHandler
  , waitUntilCompleted
  , setWaitUntilCompleted
  , compilationDescriptor
  , setCompilationDescriptor
  , compilationDescriptorSelector
  , completionHandlerSelector
  , scheduledHandlerSelector
  , setCompilationDescriptorSelector
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

-- | Executable waits on these shared events before scheduling execution on the HW, this does not include encoding which can still continue.
--
-- - Parameters:   - event: shared event graph waits on.   - value: value of shared event graph waits on.
--
-- ObjC selector: @- waitForEvent:value:@
waitForEvent_value :: IsMPSGraphExecutionDescriptor mpsGraphExecutionDescriptor => mpsGraphExecutionDescriptor -> RawId -> CULong -> IO ()
waitForEvent_value mpsGraphExecutionDescriptor event value =
  sendMessage mpsGraphExecutionDescriptor waitForEvent_valueSelector event value

-- | Executable signals these shared events at execution stage and immediately proceeds.
--
-- - Parameters:   - event: shared event to signal.   - executionStage: execution stage to signal event at.   - value: value for shared event to wait on.
--
-- ObjC selector: @- signalEvent:atExecutionEvent:value:@
signalEvent_atExecutionEvent_value :: IsMPSGraphExecutionDescriptor mpsGraphExecutionDescriptor => mpsGraphExecutionDescriptor -> RawId -> MPSGraphExecutionStage -> CULong -> IO ()
signalEvent_atExecutionEvent_value mpsGraphExecutionDescriptor event executionStage value =
  sendMessage mpsGraphExecutionDescriptor signalEvent_atExecutionEvent_valueSelector event executionStage value

-- | The handler that graph calls when it schedules the execution.
--
-- Default value is nil.
--
-- ObjC selector: @- scheduledHandler@
scheduledHandler :: IsMPSGraphExecutionDescriptor mpsGraphExecutionDescriptor => mpsGraphExecutionDescriptor -> IO (Ptr ())
scheduledHandler mpsGraphExecutionDescriptor =
  sendMessage mpsGraphExecutionDescriptor scheduledHandlerSelector

-- | The handler that graph calls when it schedules the execution.
--
-- Default value is nil.
--
-- ObjC selector: @- setScheduledHandler:@
setScheduledHandler :: IsMPSGraphExecutionDescriptor mpsGraphExecutionDescriptor => mpsGraphExecutionDescriptor -> Ptr () -> IO ()
setScheduledHandler mpsGraphExecutionDescriptor value =
  sendMessage mpsGraphExecutionDescriptor setScheduledHandlerSelector value

-- | The handler that graph calls at the completion of the execution.
--
-- Default value is nil.
--
-- ObjC selector: @- completionHandler@
completionHandler :: IsMPSGraphExecutionDescriptor mpsGraphExecutionDescriptor => mpsGraphExecutionDescriptor -> IO (Ptr ())
completionHandler mpsGraphExecutionDescriptor =
  sendMessage mpsGraphExecutionDescriptor completionHandlerSelector

-- | The handler that graph calls at the completion of the execution.
--
-- Default value is nil.
--
-- ObjC selector: @- setCompletionHandler:@
setCompletionHandler :: IsMPSGraphExecutionDescriptor mpsGraphExecutionDescriptor => mpsGraphExecutionDescriptor -> Ptr () -> IO ()
setCompletionHandler mpsGraphExecutionDescriptor value =
  sendMessage mpsGraphExecutionDescriptor setCompletionHandlerSelector value

-- | The flag that blocks the execution call until the entire execution is complete.
--
-- Defaults to NO.
--
-- ObjC selector: @- waitUntilCompleted@
waitUntilCompleted :: IsMPSGraphExecutionDescriptor mpsGraphExecutionDescriptor => mpsGraphExecutionDescriptor -> IO Bool
waitUntilCompleted mpsGraphExecutionDescriptor =
  sendMessage mpsGraphExecutionDescriptor waitUntilCompletedSelector

-- | The flag that blocks the execution call until the entire execution is complete.
--
-- Defaults to NO.
--
-- ObjC selector: @- setWaitUntilCompleted:@
setWaitUntilCompleted :: IsMPSGraphExecutionDescriptor mpsGraphExecutionDescriptor => mpsGraphExecutionDescriptor -> Bool -> IO ()
setWaitUntilCompleted mpsGraphExecutionDescriptor value =
  sendMessage mpsGraphExecutionDescriptor setWaitUntilCompletedSelector value

-- | The compilation descriptor for the graph.
--
-- Default value is nil.
--
-- ObjC selector: @- compilationDescriptor@
compilationDescriptor :: IsMPSGraphExecutionDescriptor mpsGraphExecutionDescriptor => mpsGraphExecutionDescriptor -> IO (Id MPSGraphCompilationDescriptor)
compilationDescriptor mpsGraphExecutionDescriptor =
  sendMessage mpsGraphExecutionDescriptor compilationDescriptorSelector

-- | The compilation descriptor for the graph.
--
-- Default value is nil.
--
-- ObjC selector: @- setCompilationDescriptor:@
setCompilationDescriptor :: (IsMPSGraphExecutionDescriptor mpsGraphExecutionDescriptor, IsMPSGraphCompilationDescriptor value) => mpsGraphExecutionDescriptor -> value -> IO ()
setCompilationDescriptor mpsGraphExecutionDescriptor value =
  sendMessage mpsGraphExecutionDescriptor setCompilationDescriptorSelector (toMPSGraphCompilationDescriptor value)

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

-- | @Selector@ for @compilationDescriptor@
compilationDescriptorSelector :: Selector '[] (Id MPSGraphCompilationDescriptor)
compilationDescriptorSelector = mkSelector "compilationDescriptor"

-- | @Selector@ for @setCompilationDescriptor:@
setCompilationDescriptorSelector :: Selector '[Id MPSGraphCompilationDescriptor] ()
setCompilationDescriptorSelector = mkSelector "setCompilationDescriptor:"

