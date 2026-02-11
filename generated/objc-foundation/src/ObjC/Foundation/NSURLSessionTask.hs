{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Generated bindings for @NSURLSessionTask@.
module ObjC.Foundation.NSURLSessionTask
  ( NSURLSessionTask
  , IsNSURLSessionTask(..)
  , cancel
  , suspend
  , resume
  , init_
  , new
  , taskIdentifier
  , originalRequest
  , currentRequest
  , response
  , progress
  , earliestBeginDate
  , setEarliestBeginDate
  , countOfBytesClientExpectsToSend
  , setCountOfBytesClientExpectsToSend
  , countOfBytesClientExpectsToReceive
  , setCountOfBytesClientExpectsToReceive
  , countOfBytesSent
  , countOfBytesReceived
  , countOfBytesExpectedToSend
  , countOfBytesExpectedToReceive
  , taskDescription
  , setTaskDescription
  , state
  , error_
  , priority
  , setPriority
  , prefersIncrementalDelivery
  , setPrefersIncrementalDelivery
  , cancelSelector
  , suspendSelector
  , resumeSelector
  , initSelector
  , newSelector
  , taskIdentifierSelector
  , originalRequestSelector
  , currentRequestSelector
  , responseSelector
  , progressSelector
  , earliestBeginDateSelector
  , setEarliestBeginDateSelector
  , countOfBytesClientExpectsToSendSelector
  , setCountOfBytesClientExpectsToSendSelector
  , countOfBytesClientExpectsToReceiveSelector
  , setCountOfBytesClientExpectsToReceiveSelector
  , countOfBytesSentSelector
  , countOfBytesReceivedSelector
  , countOfBytesExpectedToSendSelector
  , countOfBytesExpectedToReceiveSelector
  , taskDescriptionSelector
  , setTaskDescriptionSelector
  , stateSelector
  , errorSelector
  , prioritySelector
  , setPrioritySelector
  , prefersIncrementalDeliverySelector
  , setPrefersIncrementalDeliverySelector

  -- * Enum types
  , NSURLSessionTaskState(NSURLSessionTaskState)
  , pattern NSURLSessionTaskStateRunning
  , pattern NSURLSessionTaskStateSuspended
  , pattern NSURLSessionTaskStateCanceling
  , pattern NSURLSessionTaskStateCompleted

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
import ObjC.Foundation.Internal.Enums

-- | @- cancel@
cancel :: IsNSURLSessionTask nsurlSessionTask => nsurlSessionTask -> IO ()
cancel nsurlSessionTask  =
  sendMsg nsurlSessionTask (mkSelector "cancel") retVoid []

-- | @- suspend@
suspend :: IsNSURLSessionTask nsurlSessionTask => nsurlSessionTask -> IO ()
suspend nsurlSessionTask  =
  sendMsg nsurlSessionTask (mkSelector "suspend") retVoid []

-- | @- resume@
resume :: IsNSURLSessionTask nsurlSessionTask => nsurlSessionTask -> IO ()
resume nsurlSessionTask  =
  sendMsg nsurlSessionTask (mkSelector "resume") retVoid []

-- | @- init@
init_ :: IsNSURLSessionTask nsurlSessionTask => nsurlSessionTask -> IO (Id NSURLSessionTask)
init_ nsurlSessionTask  =
  sendMsg nsurlSessionTask (mkSelector "init") (retPtr retVoid) [] >>= ownedObject . castPtr

-- | @+ new@
new :: IO (Id NSURLSessionTask)
new  =
  do
    cls' <- getRequiredClass "NSURLSessionTask"
    sendClassMsg cls' (mkSelector "new") (retPtr retVoid) [] >>= ownedObject . castPtr

-- | @- taskIdentifier@
taskIdentifier :: IsNSURLSessionTask nsurlSessionTask => nsurlSessionTask -> IO CULong
taskIdentifier nsurlSessionTask  =
  sendMsg nsurlSessionTask (mkSelector "taskIdentifier") retCULong []

-- | @- originalRequest@
originalRequest :: IsNSURLSessionTask nsurlSessionTask => nsurlSessionTask -> IO (Id NSURLRequest)
originalRequest nsurlSessionTask  =
  sendMsg nsurlSessionTask (mkSelector "originalRequest") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- currentRequest@
currentRequest :: IsNSURLSessionTask nsurlSessionTask => nsurlSessionTask -> IO (Id NSURLRequest)
currentRequest nsurlSessionTask  =
  sendMsg nsurlSessionTask (mkSelector "currentRequest") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- response@
response :: IsNSURLSessionTask nsurlSessionTask => nsurlSessionTask -> IO (Id NSURLResponse)
response nsurlSessionTask  =
  sendMsg nsurlSessionTask (mkSelector "response") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- progress@
progress :: IsNSURLSessionTask nsurlSessionTask => nsurlSessionTask -> IO (Id NSProgress)
progress nsurlSessionTask  =
  sendMsg nsurlSessionTask (mkSelector "progress") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- earliestBeginDate@
earliestBeginDate :: IsNSURLSessionTask nsurlSessionTask => nsurlSessionTask -> IO (Id NSDate)
earliestBeginDate nsurlSessionTask  =
  sendMsg nsurlSessionTask (mkSelector "earliestBeginDate") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- setEarliestBeginDate:@
setEarliestBeginDate :: (IsNSURLSessionTask nsurlSessionTask, IsNSDate value) => nsurlSessionTask -> value -> IO ()
setEarliestBeginDate nsurlSessionTask  value =
withObjCPtr value $ \raw_value ->
    sendMsg nsurlSessionTask (mkSelector "setEarliestBeginDate:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- | @- countOfBytesClientExpectsToSend@
countOfBytesClientExpectsToSend :: IsNSURLSessionTask nsurlSessionTask => nsurlSessionTask -> IO CLong
countOfBytesClientExpectsToSend nsurlSessionTask  =
  sendMsg nsurlSessionTask (mkSelector "countOfBytesClientExpectsToSend") retCLong []

-- | @- setCountOfBytesClientExpectsToSend:@
setCountOfBytesClientExpectsToSend :: IsNSURLSessionTask nsurlSessionTask => nsurlSessionTask -> CLong -> IO ()
setCountOfBytesClientExpectsToSend nsurlSessionTask  value =
  sendMsg nsurlSessionTask (mkSelector "setCountOfBytesClientExpectsToSend:") retVoid [argCLong (fromIntegral value)]

-- | @- countOfBytesClientExpectsToReceive@
countOfBytesClientExpectsToReceive :: IsNSURLSessionTask nsurlSessionTask => nsurlSessionTask -> IO CLong
countOfBytesClientExpectsToReceive nsurlSessionTask  =
  sendMsg nsurlSessionTask (mkSelector "countOfBytesClientExpectsToReceive") retCLong []

-- | @- setCountOfBytesClientExpectsToReceive:@
setCountOfBytesClientExpectsToReceive :: IsNSURLSessionTask nsurlSessionTask => nsurlSessionTask -> CLong -> IO ()
setCountOfBytesClientExpectsToReceive nsurlSessionTask  value =
  sendMsg nsurlSessionTask (mkSelector "setCountOfBytesClientExpectsToReceive:") retVoid [argCLong (fromIntegral value)]

-- | @- countOfBytesSent@
countOfBytesSent :: IsNSURLSessionTask nsurlSessionTask => nsurlSessionTask -> IO CLong
countOfBytesSent nsurlSessionTask  =
  sendMsg nsurlSessionTask (mkSelector "countOfBytesSent") retCLong []

-- | @- countOfBytesReceived@
countOfBytesReceived :: IsNSURLSessionTask nsurlSessionTask => nsurlSessionTask -> IO CLong
countOfBytesReceived nsurlSessionTask  =
  sendMsg nsurlSessionTask (mkSelector "countOfBytesReceived") retCLong []

-- | @- countOfBytesExpectedToSend@
countOfBytesExpectedToSend :: IsNSURLSessionTask nsurlSessionTask => nsurlSessionTask -> IO CLong
countOfBytesExpectedToSend nsurlSessionTask  =
  sendMsg nsurlSessionTask (mkSelector "countOfBytesExpectedToSend") retCLong []

-- | @- countOfBytesExpectedToReceive@
countOfBytesExpectedToReceive :: IsNSURLSessionTask nsurlSessionTask => nsurlSessionTask -> IO CLong
countOfBytesExpectedToReceive nsurlSessionTask  =
  sendMsg nsurlSessionTask (mkSelector "countOfBytesExpectedToReceive") retCLong []

-- | @- taskDescription@
taskDescription :: IsNSURLSessionTask nsurlSessionTask => nsurlSessionTask -> IO (Id NSString)
taskDescription nsurlSessionTask  =
  sendMsg nsurlSessionTask (mkSelector "taskDescription") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- setTaskDescription:@
setTaskDescription :: (IsNSURLSessionTask nsurlSessionTask, IsNSString value) => nsurlSessionTask -> value -> IO ()
setTaskDescription nsurlSessionTask  value =
withObjCPtr value $ \raw_value ->
    sendMsg nsurlSessionTask (mkSelector "setTaskDescription:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- | @- state@
state :: IsNSURLSessionTask nsurlSessionTask => nsurlSessionTask -> IO NSURLSessionTaskState
state nsurlSessionTask  =
  fmap (coerce :: CLong -> NSURLSessionTaskState) $ sendMsg nsurlSessionTask (mkSelector "state") retCLong []

-- | @- error@
error_ :: IsNSURLSessionTask nsurlSessionTask => nsurlSessionTask -> IO (Id NSError)
error_ nsurlSessionTask  =
  sendMsg nsurlSessionTask (mkSelector "error") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- priority@
priority :: IsNSURLSessionTask nsurlSessionTask => nsurlSessionTask -> IO CFloat
priority nsurlSessionTask  =
  sendMsg nsurlSessionTask (mkSelector "priority") retCFloat []

-- | @- setPriority:@
setPriority :: IsNSURLSessionTask nsurlSessionTask => nsurlSessionTask -> CFloat -> IO ()
setPriority nsurlSessionTask  value =
  sendMsg nsurlSessionTask (mkSelector "setPriority:") retVoid [argCFloat (fromIntegral value)]

-- | @- prefersIncrementalDelivery@
prefersIncrementalDelivery :: IsNSURLSessionTask nsurlSessionTask => nsurlSessionTask -> IO Bool
prefersIncrementalDelivery nsurlSessionTask  =
  fmap ((/= 0) :: CULong -> Bool) $ sendMsg nsurlSessionTask (mkSelector "prefersIncrementalDelivery") retCULong []

-- | @- setPrefersIncrementalDelivery:@
setPrefersIncrementalDelivery :: IsNSURLSessionTask nsurlSessionTask => nsurlSessionTask -> Bool -> IO ()
setPrefersIncrementalDelivery nsurlSessionTask  value =
  sendMsg nsurlSessionTask (mkSelector "setPrefersIncrementalDelivery:") retVoid [argCULong (if value then 1 else 0)]

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @cancel@
cancelSelector :: Selector
cancelSelector = mkSelector "cancel"

-- | @Selector@ for @suspend@
suspendSelector :: Selector
suspendSelector = mkSelector "suspend"

-- | @Selector@ for @resume@
resumeSelector :: Selector
resumeSelector = mkSelector "resume"

-- | @Selector@ for @init@
initSelector :: Selector
initSelector = mkSelector "init"

-- | @Selector@ for @new@
newSelector :: Selector
newSelector = mkSelector "new"

-- | @Selector@ for @taskIdentifier@
taskIdentifierSelector :: Selector
taskIdentifierSelector = mkSelector "taskIdentifier"

-- | @Selector@ for @originalRequest@
originalRequestSelector :: Selector
originalRequestSelector = mkSelector "originalRequest"

-- | @Selector@ for @currentRequest@
currentRequestSelector :: Selector
currentRequestSelector = mkSelector "currentRequest"

-- | @Selector@ for @response@
responseSelector :: Selector
responseSelector = mkSelector "response"

-- | @Selector@ for @progress@
progressSelector :: Selector
progressSelector = mkSelector "progress"

-- | @Selector@ for @earliestBeginDate@
earliestBeginDateSelector :: Selector
earliestBeginDateSelector = mkSelector "earliestBeginDate"

-- | @Selector@ for @setEarliestBeginDate:@
setEarliestBeginDateSelector :: Selector
setEarliestBeginDateSelector = mkSelector "setEarliestBeginDate:"

-- | @Selector@ for @countOfBytesClientExpectsToSend@
countOfBytesClientExpectsToSendSelector :: Selector
countOfBytesClientExpectsToSendSelector = mkSelector "countOfBytesClientExpectsToSend"

-- | @Selector@ for @setCountOfBytesClientExpectsToSend:@
setCountOfBytesClientExpectsToSendSelector :: Selector
setCountOfBytesClientExpectsToSendSelector = mkSelector "setCountOfBytesClientExpectsToSend:"

-- | @Selector@ for @countOfBytesClientExpectsToReceive@
countOfBytesClientExpectsToReceiveSelector :: Selector
countOfBytesClientExpectsToReceiveSelector = mkSelector "countOfBytesClientExpectsToReceive"

-- | @Selector@ for @setCountOfBytesClientExpectsToReceive:@
setCountOfBytesClientExpectsToReceiveSelector :: Selector
setCountOfBytesClientExpectsToReceiveSelector = mkSelector "setCountOfBytesClientExpectsToReceive:"

-- | @Selector@ for @countOfBytesSent@
countOfBytesSentSelector :: Selector
countOfBytesSentSelector = mkSelector "countOfBytesSent"

-- | @Selector@ for @countOfBytesReceived@
countOfBytesReceivedSelector :: Selector
countOfBytesReceivedSelector = mkSelector "countOfBytesReceived"

-- | @Selector@ for @countOfBytesExpectedToSend@
countOfBytesExpectedToSendSelector :: Selector
countOfBytesExpectedToSendSelector = mkSelector "countOfBytesExpectedToSend"

-- | @Selector@ for @countOfBytesExpectedToReceive@
countOfBytesExpectedToReceiveSelector :: Selector
countOfBytesExpectedToReceiveSelector = mkSelector "countOfBytesExpectedToReceive"

-- | @Selector@ for @taskDescription@
taskDescriptionSelector :: Selector
taskDescriptionSelector = mkSelector "taskDescription"

-- | @Selector@ for @setTaskDescription:@
setTaskDescriptionSelector :: Selector
setTaskDescriptionSelector = mkSelector "setTaskDescription:"

-- | @Selector@ for @state@
stateSelector :: Selector
stateSelector = mkSelector "state"

-- | @Selector@ for @error@
errorSelector :: Selector
errorSelector = mkSelector "error"

-- | @Selector@ for @priority@
prioritySelector :: Selector
prioritySelector = mkSelector "priority"

-- | @Selector@ for @setPriority:@
setPrioritySelector :: Selector
setPrioritySelector = mkSelector "setPriority:"

-- | @Selector@ for @prefersIncrementalDelivery@
prefersIncrementalDeliverySelector :: Selector
prefersIncrementalDeliverySelector = mkSelector "prefersIncrementalDelivery"

-- | @Selector@ for @setPrefersIncrementalDelivery:@
setPrefersIncrementalDeliverySelector :: Selector
setPrefersIncrementalDeliverySelector = mkSelector "setPrefersIncrementalDelivery:"

