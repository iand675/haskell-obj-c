{-# LANGUAGE PatternSynonyms #-}
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
  , waitForEvent_valueSelector
  , signalEvent_atExecutionEvent_valueSelector
  , scheduledHandlerSelector
  , setScheduledHandlerSelector
  , completionHandlerSelector
  , setCompletionHandlerSelector
  , waitUntilCompletedSelector
  , setWaitUntilCompletedSelector
  , compilationDescriptorSelector
  , setCompilationDescriptorSelector

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

-- | Executable waits on these shared events before scheduling execution on the HW, this does not include encoding which can still continue.
--
-- - Parameters:   - event: shared event graph waits on.   - value: value of shared event graph waits on.
--
-- ObjC selector: @- waitForEvent:value:@
waitForEvent_value :: IsMPSGraphExecutionDescriptor mpsGraphExecutionDescriptor => mpsGraphExecutionDescriptor -> RawId -> CULong -> IO ()
waitForEvent_value mpsGraphExecutionDescriptor  event value =
  sendMsg mpsGraphExecutionDescriptor (mkSelector "waitForEvent:value:") retVoid [argPtr (castPtr (unRawId event) :: Ptr ()), argCULong (fromIntegral value)]

-- | Executable signals these shared events at execution stage and immediately proceeds.
--
-- - Parameters:   - event: shared event to signal.   - executionStage: execution stage to signal event at.   - value: value for shared event to wait on.
--
-- ObjC selector: @- signalEvent:atExecutionEvent:value:@
signalEvent_atExecutionEvent_value :: IsMPSGraphExecutionDescriptor mpsGraphExecutionDescriptor => mpsGraphExecutionDescriptor -> RawId -> MPSGraphExecutionStage -> CULong -> IO ()
signalEvent_atExecutionEvent_value mpsGraphExecutionDescriptor  event executionStage value =
  sendMsg mpsGraphExecutionDescriptor (mkSelector "signalEvent:atExecutionEvent:value:") retVoid [argPtr (castPtr (unRawId event) :: Ptr ()), argCULong (coerce executionStage), argCULong (fromIntegral value)]

-- | The handler that graph calls when it schedules the execution.
--
-- Default value is nil.
--
-- ObjC selector: @- scheduledHandler@
scheduledHandler :: IsMPSGraphExecutionDescriptor mpsGraphExecutionDescriptor => mpsGraphExecutionDescriptor -> IO (Ptr ())
scheduledHandler mpsGraphExecutionDescriptor  =
  fmap castPtr $ sendMsg mpsGraphExecutionDescriptor (mkSelector "scheduledHandler") (retPtr retVoid) []

-- | The handler that graph calls when it schedules the execution.
--
-- Default value is nil.
--
-- ObjC selector: @- setScheduledHandler:@
setScheduledHandler :: IsMPSGraphExecutionDescriptor mpsGraphExecutionDescriptor => mpsGraphExecutionDescriptor -> Ptr () -> IO ()
setScheduledHandler mpsGraphExecutionDescriptor  value =
  sendMsg mpsGraphExecutionDescriptor (mkSelector "setScheduledHandler:") retVoid [argPtr (castPtr value :: Ptr ())]

-- | The handler that graph calls at the completion of the execution.
--
-- Default value is nil.
--
-- ObjC selector: @- completionHandler@
completionHandler :: IsMPSGraphExecutionDescriptor mpsGraphExecutionDescriptor => mpsGraphExecutionDescriptor -> IO (Ptr ())
completionHandler mpsGraphExecutionDescriptor  =
  fmap castPtr $ sendMsg mpsGraphExecutionDescriptor (mkSelector "completionHandler") (retPtr retVoid) []

-- | The handler that graph calls at the completion of the execution.
--
-- Default value is nil.
--
-- ObjC selector: @- setCompletionHandler:@
setCompletionHandler :: IsMPSGraphExecutionDescriptor mpsGraphExecutionDescriptor => mpsGraphExecutionDescriptor -> Ptr () -> IO ()
setCompletionHandler mpsGraphExecutionDescriptor  value =
  sendMsg mpsGraphExecutionDescriptor (mkSelector "setCompletionHandler:") retVoid [argPtr (castPtr value :: Ptr ())]

-- | The flag that blocks the execution call until the entire execution is complete.
--
-- Defaults to NO.
--
-- ObjC selector: @- waitUntilCompleted@
waitUntilCompleted :: IsMPSGraphExecutionDescriptor mpsGraphExecutionDescriptor => mpsGraphExecutionDescriptor -> IO Bool
waitUntilCompleted mpsGraphExecutionDescriptor  =
  fmap ((/= 0) :: CULong -> Bool) $ sendMsg mpsGraphExecutionDescriptor (mkSelector "waitUntilCompleted") retCULong []

-- | The flag that blocks the execution call until the entire execution is complete.
--
-- Defaults to NO.
--
-- ObjC selector: @- setWaitUntilCompleted:@
setWaitUntilCompleted :: IsMPSGraphExecutionDescriptor mpsGraphExecutionDescriptor => mpsGraphExecutionDescriptor -> Bool -> IO ()
setWaitUntilCompleted mpsGraphExecutionDescriptor  value =
  sendMsg mpsGraphExecutionDescriptor (mkSelector "setWaitUntilCompleted:") retVoid [argCULong (if value then 1 else 0)]

-- | The compilation descriptor for the graph.
--
-- Default value is nil.
--
-- ObjC selector: @- compilationDescriptor@
compilationDescriptor :: IsMPSGraphExecutionDescriptor mpsGraphExecutionDescriptor => mpsGraphExecutionDescriptor -> IO (Id MPSGraphCompilationDescriptor)
compilationDescriptor mpsGraphExecutionDescriptor  =
  sendMsg mpsGraphExecutionDescriptor (mkSelector "compilationDescriptor") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | The compilation descriptor for the graph.
--
-- Default value is nil.
--
-- ObjC selector: @- setCompilationDescriptor:@
setCompilationDescriptor :: (IsMPSGraphExecutionDescriptor mpsGraphExecutionDescriptor, IsMPSGraphCompilationDescriptor value) => mpsGraphExecutionDescriptor -> value -> IO ()
setCompilationDescriptor mpsGraphExecutionDescriptor  value =
withObjCPtr value $ \raw_value ->
    sendMsg mpsGraphExecutionDescriptor (mkSelector "setCompilationDescriptor:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

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

-- | @Selector@ for @compilationDescriptor@
compilationDescriptorSelector :: Selector
compilationDescriptorSelector = mkSelector "compilationDescriptor"

-- | @Selector@ for @setCompilationDescriptor:@
setCompilationDescriptorSelector :: Selector
setCompilationDescriptorSelector = mkSelector "setCompilationDescriptor:"

