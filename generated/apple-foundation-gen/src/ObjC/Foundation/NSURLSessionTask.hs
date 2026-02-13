{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE DataKinds #-}
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
  , delegate
  , setDelegate
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
  , countOfBytesClientExpectsToReceiveSelector
  , countOfBytesClientExpectsToSendSelector
  , countOfBytesExpectedToReceiveSelector
  , countOfBytesExpectedToSendSelector
  , countOfBytesReceivedSelector
  , countOfBytesSentSelector
  , currentRequestSelector
  , delegateSelector
  , earliestBeginDateSelector
  , errorSelector
  , initSelector
  , newSelector
  , originalRequestSelector
  , prefersIncrementalDeliverySelector
  , prioritySelector
  , progressSelector
  , responseSelector
  , resumeSelector
  , setCountOfBytesClientExpectsToReceiveSelector
  , setCountOfBytesClientExpectsToSendSelector
  , setDelegateSelector
  , setEarliestBeginDateSelector
  , setPrefersIncrementalDeliverySelector
  , setPrioritySelector
  , setTaskDescriptionSelector
  , stateSelector
  , suspendSelector
  , taskDescriptionSelector
  , taskIdentifierSelector

  -- * Enum types
  , NSURLSessionTaskState(NSURLSessionTaskState)
  , pattern NSURLSessionTaskStateRunning
  , pattern NSURLSessionTaskStateSuspended
  , pattern NSURLSessionTaskStateCanceling
  , pattern NSURLSessionTaskStateCompleted

  ) where

import Foreign.Ptr (Ptr, FunPtr)
import Foreign.C.Types

import ObjC.Runtime.Types
import ObjC.Runtime.Message (sendMessage, sendOwnedMessage, sendClassMessage, sendOwnedClassMessage)
import ObjC.Runtime.Selector (mkSelector)
import ObjC.Runtime.Class (getRequiredClass)

import ObjC.Foundation.Internal.Classes
import ObjC.Foundation.Internal.Enums

-- | @- cancel@
cancel :: IsNSURLSessionTask nsurlSessionTask => nsurlSessionTask -> IO ()
cancel nsurlSessionTask =
  sendMessage nsurlSessionTask cancelSelector

-- | @- suspend@
suspend :: IsNSURLSessionTask nsurlSessionTask => nsurlSessionTask -> IO ()
suspend nsurlSessionTask =
  sendMessage nsurlSessionTask suspendSelector

-- | @- resume@
resume :: IsNSURLSessionTask nsurlSessionTask => nsurlSessionTask -> IO ()
resume nsurlSessionTask =
  sendMessage nsurlSessionTask resumeSelector

-- | @- init@
init_ :: IsNSURLSessionTask nsurlSessionTask => nsurlSessionTask -> IO (Id NSURLSessionTask)
init_ nsurlSessionTask =
  sendOwnedMessage nsurlSessionTask initSelector

-- | @+ new@
new :: IO (Id NSURLSessionTask)
new  =
  do
    cls' <- getRequiredClass "NSURLSessionTask"
    sendOwnedClassMessage cls' newSelector

-- | @- taskIdentifier@
taskIdentifier :: IsNSURLSessionTask nsurlSessionTask => nsurlSessionTask -> IO CULong
taskIdentifier nsurlSessionTask =
  sendMessage nsurlSessionTask taskIdentifierSelector

-- | @- originalRequest@
originalRequest :: IsNSURLSessionTask nsurlSessionTask => nsurlSessionTask -> IO (Id NSURLRequest)
originalRequest nsurlSessionTask =
  sendMessage nsurlSessionTask originalRequestSelector

-- | @- currentRequest@
currentRequest :: IsNSURLSessionTask nsurlSessionTask => nsurlSessionTask -> IO (Id NSURLRequest)
currentRequest nsurlSessionTask =
  sendMessage nsurlSessionTask currentRequestSelector

-- | @- response@
response :: IsNSURLSessionTask nsurlSessionTask => nsurlSessionTask -> IO (Id NSURLResponse)
response nsurlSessionTask =
  sendMessage nsurlSessionTask responseSelector

-- | @- delegate@
delegate :: IsNSURLSessionTask nsurlSessionTask => nsurlSessionTask -> IO RawId
delegate nsurlSessionTask =
  sendMessage nsurlSessionTask delegateSelector

-- | @- setDelegate:@
setDelegate :: IsNSURLSessionTask nsurlSessionTask => nsurlSessionTask -> RawId -> IO ()
setDelegate nsurlSessionTask value =
  sendMessage nsurlSessionTask setDelegateSelector value

-- | @- progress@
progress :: IsNSURLSessionTask nsurlSessionTask => nsurlSessionTask -> IO (Id NSProgress)
progress nsurlSessionTask =
  sendMessage nsurlSessionTask progressSelector

-- | @- earliestBeginDate@
earliestBeginDate :: IsNSURLSessionTask nsurlSessionTask => nsurlSessionTask -> IO (Id NSDate)
earliestBeginDate nsurlSessionTask =
  sendMessage nsurlSessionTask earliestBeginDateSelector

-- | @- setEarliestBeginDate:@
setEarliestBeginDate :: (IsNSURLSessionTask nsurlSessionTask, IsNSDate value) => nsurlSessionTask -> value -> IO ()
setEarliestBeginDate nsurlSessionTask value =
  sendMessage nsurlSessionTask setEarliestBeginDateSelector (toNSDate value)

-- | @- countOfBytesClientExpectsToSend@
countOfBytesClientExpectsToSend :: IsNSURLSessionTask nsurlSessionTask => nsurlSessionTask -> IO CLong
countOfBytesClientExpectsToSend nsurlSessionTask =
  sendMessage nsurlSessionTask countOfBytesClientExpectsToSendSelector

-- | @- setCountOfBytesClientExpectsToSend:@
setCountOfBytesClientExpectsToSend :: IsNSURLSessionTask nsurlSessionTask => nsurlSessionTask -> CLong -> IO ()
setCountOfBytesClientExpectsToSend nsurlSessionTask value =
  sendMessage nsurlSessionTask setCountOfBytesClientExpectsToSendSelector value

-- | @- countOfBytesClientExpectsToReceive@
countOfBytesClientExpectsToReceive :: IsNSURLSessionTask nsurlSessionTask => nsurlSessionTask -> IO CLong
countOfBytesClientExpectsToReceive nsurlSessionTask =
  sendMessage nsurlSessionTask countOfBytesClientExpectsToReceiveSelector

-- | @- setCountOfBytesClientExpectsToReceive:@
setCountOfBytesClientExpectsToReceive :: IsNSURLSessionTask nsurlSessionTask => nsurlSessionTask -> CLong -> IO ()
setCountOfBytesClientExpectsToReceive nsurlSessionTask value =
  sendMessage nsurlSessionTask setCountOfBytesClientExpectsToReceiveSelector value

-- | @- countOfBytesSent@
countOfBytesSent :: IsNSURLSessionTask nsurlSessionTask => nsurlSessionTask -> IO CLong
countOfBytesSent nsurlSessionTask =
  sendMessage nsurlSessionTask countOfBytesSentSelector

-- | @- countOfBytesReceived@
countOfBytesReceived :: IsNSURLSessionTask nsurlSessionTask => nsurlSessionTask -> IO CLong
countOfBytesReceived nsurlSessionTask =
  sendMessage nsurlSessionTask countOfBytesReceivedSelector

-- | @- countOfBytesExpectedToSend@
countOfBytesExpectedToSend :: IsNSURLSessionTask nsurlSessionTask => nsurlSessionTask -> IO CLong
countOfBytesExpectedToSend nsurlSessionTask =
  sendMessage nsurlSessionTask countOfBytesExpectedToSendSelector

-- | @- countOfBytesExpectedToReceive@
countOfBytesExpectedToReceive :: IsNSURLSessionTask nsurlSessionTask => nsurlSessionTask -> IO CLong
countOfBytesExpectedToReceive nsurlSessionTask =
  sendMessage nsurlSessionTask countOfBytesExpectedToReceiveSelector

-- | @- taskDescription@
taskDescription :: IsNSURLSessionTask nsurlSessionTask => nsurlSessionTask -> IO (Id NSString)
taskDescription nsurlSessionTask =
  sendMessage nsurlSessionTask taskDescriptionSelector

-- | @- setTaskDescription:@
setTaskDescription :: (IsNSURLSessionTask nsurlSessionTask, IsNSString value) => nsurlSessionTask -> value -> IO ()
setTaskDescription nsurlSessionTask value =
  sendMessage nsurlSessionTask setTaskDescriptionSelector (toNSString value)

-- | @- state@
state :: IsNSURLSessionTask nsurlSessionTask => nsurlSessionTask -> IO NSURLSessionTaskState
state nsurlSessionTask =
  sendMessage nsurlSessionTask stateSelector

-- | @- error@
error_ :: IsNSURLSessionTask nsurlSessionTask => nsurlSessionTask -> IO (Id NSError)
error_ nsurlSessionTask =
  sendMessage nsurlSessionTask errorSelector

-- | @- priority@
priority :: IsNSURLSessionTask nsurlSessionTask => nsurlSessionTask -> IO CFloat
priority nsurlSessionTask =
  sendMessage nsurlSessionTask prioritySelector

-- | @- setPriority:@
setPriority :: IsNSURLSessionTask nsurlSessionTask => nsurlSessionTask -> CFloat -> IO ()
setPriority nsurlSessionTask value =
  sendMessage nsurlSessionTask setPrioritySelector value

-- | @- prefersIncrementalDelivery@
prefersIncrementalDelivery :: IsNSURLSessionTask nsurlSessionTask => nsurlSessionTask -> IO Bool
prefersIncrementalDelivery nsurlSessionTask =
  sendMessage nsurlSessionTask prefersIncrementalDeliverySelector

-- | @- setPrefersIncrementalDelivery:@
setPrefersIncrementalDelivery :: IsNSURLSessionTask nsurlSessionTask => nsurlSessionTask -> Bool -> IO ()
setPrefersIncrementalDelivery nsurlSessionTask value =
  sendMessage nsurlSessionTask setPrefersIncrementalDeliverySelector value

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @cancel@
cancelSelector :: Selector '[] ()
cancelSelector = mkSelector "cancel"

-- | @Selector@ for @suspend@
suspendSelector :: Selector '[] ()
suspendSelector = mkSelector "suspend"

-- | @Selector@ for @resume@
resumeSelector :: Selector '[] ()
resumeSelector = mkSelector "resume"

-- | @Selector@ for @init@
initSelector :: Selector '[] (Id NSURLSessionTask)
initSelector = mkSelector "init"

-- | @Selector@ for @new@
newSelector :: Selector '[] (Id NSURLSessionTask)
newSelector = mkSelector "new"

-- | @Selector@ for @taskIdentifier@
taskIdentifierSelector :: Selector '[] CULong
taskIdentifierSelector = mkSelector "taskIdentifier"

-- | @Selector@ for @originalRequest@
originalRequestSelector :: Selector '[] (Id NSURLRequest)
originalRequestSelector = mkSelector "originalRequest"

-- | @Selector@ for @currentRequest@
currentRequestSelector :: Selector '[] (Id NSURLRequest)
currentRequestSelector = mkSelector "currentRequest"

-- | @Selector@ for @response@
responseSelector :: Selector '[] (Id NSURLResponse)
responseSelector = mkSelector "response"

-- | @Selector@ for @delegate@
delegateSelector :: Selector '[] RawId
delegateSelector = mkSelector "delegate"

-- | @Selector@ for @setDelegate:@
setDelegateSelector :: Selector '[RawId] ()
setDelegateSelector = mkSelector "setDelegate:"

-- | @Selector@ for @progress@
progressSelector :: Selector '[] (Id NSProgress)
progressSelector = mkSelector "progress"

-- | @Selector@ for @earliestBeginDate@
earliestBeginDateSelector :: Selector '[] (Id NSDate)
earliestBeginDateSelector = mkSelector "earliestBeginDate"

-- | @Selector@ for @setEarliestBeginDate:@
setEarliestBeginDateSelector :: Selector '[Id NSDate] ()
setEarliestBeginDateSelector = mkSelector "setEarliestBeginDate:"

-- | @Selector@ for @countOfBytesClientExpectsToSend@
countOfBytesClientExpectsToSendSelector :: Selector '[] CLong
countOfBytesClientExpectsToSendSelector = mkSelector "countOfBytesClientExpectsToSend"

-- | @Selector@ for @setCountOfBytesClientExpectsToSend:@
setCountOfBytesClientExpectsToSendSelector :: Selector '[CLong] ()
setCountOfBytesClientExpectsToSendSelector = mkSelector "setCountOfBytesClientExpectsToSend:"

-- | @Selector@ for @countOfBytesClientExpectsToReceive@
countOfBytesClientExpectsToReceiveSelector :: Selector '[] CLong
countOfBytesClientExpectsToReceiveSelector = mkSelector "countOfBytesClientExpectsToReceive"

-- | @Selector@ for @setCountOfBytesClientExpectsToReceive:@
setCountOfBytesClientExpectsToReceiveSelector :: Selector '[CLong] ()
setCountOfBytesClientExpectsToReceiveSelector = mkSelector "setCountOfBytesClientExpectsToReceive:"

-- | @Selector@ for @countOfBytesSent@
countOfBytesSentSelector :: Selector '[] CLong
countOfBytesSentSelector = mkSelector "countOfBytesSent"

-- | @Selector@ for @countOfBytesReceived@
countOfBytesReceivedSelector :: Selector '[] CLong
countOfBytesReceivedSelector = mkSelector "countOfBytesReceived"

-- | @Selector@ for @countOfBytesExpectedToSend@
countOfBytesExpectedToSendSelector :: Selector '[] CLong
countOfBytesExpectedToSendSelector = mkSelector "countOfBytesExpectedToSend"

-- | @Selector@ for @countOfBytesExpectedToReceive@
countOfBytesExpectedToReceiveSelector :: Selector '[] CLong
countOfBytesExpectedToReceiveSelector = mkSelector "countOfBytesExpectedToReceive"

-- | @Selector@ for @taskDescription@
taskDescriptionSelector :: Selector '[] (Id NSString)
taskDescriptionSelector = mkSelector "taskDescription"

-- | @Selector@ for @setTaskDescription:@
setTaskDescriptionSelector :: Selector '[Id NSString] ()
setTaskDescriptionSelector = mkSelector "setTaskDescription:"

-- | @Selector@ for @state@
stateSelector :: Selector '[] NSURLSessionTaskState
stateSelector = mkSelector "state"

-- | @Selector@ for @error@
errorSelector :: Selector '[] (Id NSError)
errorSelector = mkSelector "error"

-- | @Selector@ for @priority@
prioritySelector :: Selector '[] CFloat
prioritySelector = mkSelector "priority"

-- | @Selector@ for @setPriority:@
setPrioritySelector :: Selector '[CFloat] ()
setPrioritySelector = mkSelector "setPriority:"

-- | @Selector@ for @prefersIncrementalDelivery@
prefersIncrementalDeliverySelector :: Selector '[] Bool
prefersIncrementalDeliverySelector = mkSelector "prefersIncrementalDelivery"

-- | @Selector@ for @setPrefersIncrementalDelivery:@
setPrefersIncrementalDeliverySelector :: Selector '[Bool] ()
setPrefersIncrementalDeliverySelector = mkSelector "setPrefersIncrementalDelivery:"

