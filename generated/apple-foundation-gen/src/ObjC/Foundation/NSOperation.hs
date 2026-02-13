{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Generated bindings for @NSOperation@.
module ObjC.Foundation.NSOperation
  ( NSOperation
  , IsNSOperation(..)
  , start
  , main
  , cancel
  , addDependency
  , removeDependency
  , waitUntilFinished
  , cancelled
  , executing
  , finished
  , concurrent
  , asynchronous
  , ready
  , dependencies
  , queuePriority
  , setQueuePriority
  , completionBlock
  , setCompletionBlock
  , threadPriority
  , setThreadPriority
  , qualityOfService
  , setQualityOfService
  , name
  , setName
  , addDependencySelector
  , asynchronousSelector
  , cancelSelector
  , cancelledSelector
  , completionBlockSelector
  , concurrentSelector
  , dependenciesSelector
  , executingSelector
  , finishedSelector
  , mainSelector
  , nameSelector
  , qualityOfServiceSelector
  , queuePrioritySelector
  , readySelector
  , removeDependencySelector
  , setCompletionBlockSelector
  , setNameSelector
  , setQualityOfServiceSelector
  , setQueuePrioritySelector
  , setThreadPrioritySelector
  , startSelector
  , threadPrioritySelector
  , waitUntilFinishedSelector

  -- * Enum types
  , NSOperationQueuePriority(NSOperationQueuePriority)
  , pattern NSOperationQueuePriorityVeryLow
  , pattern NSOperationQueuePriorityLow
  , pattern NSOperationQueuePriorityNormal
  , pattern NSOperationQueuePriorityHigh
  , pattern NSOperationQueuePriorityVeryHigh
  , NSQualityOfService(NSQualityOfService)
  , pattern NSQualityOfServiceUserInteractive
  , pattern NSQualityOfServiceUserInitiated
  , pattern NSQualityOfServiceUtility
  , pattern NSQualityOfServiceBackground
  , pattern NSQualityOfServiceDefault

  ) where

import Foreign.Ptr (Ptr, FunPtr)
import Foreign.C.Types

import ObjC.Runtime.Types
import ObjC.Runtime.Message (sendMessage, sendOwnedMessage, sendClassMessage, sendOwnedClassMessage)
import ObjC.Runtime.Selector (mkSelector)
import ObjC.Runtime.Class (getRequiredClass)

import ObjC.Foundation.Internal.Classes
import ObjC.Foundation.Internal.Enums

-- | @- start@
start :: IsNSOperation nsOperation => nsOperation -> IO ()
start nsOperation =
  sendMessage nsOperation startSelector

-- | @- main@
main :: IsNSOperation nsOperation => nsOperation -> IO ()
main nsOperation =
  sendMessage nsOperation mainSelector

-- | @- cancel@
cancel :: IsNSOperation nsOperation => nsOperation -> IO ()
cancel nsOperation =
  sendMessage nsOperation cancelSelector

-- | @- addDependency:@
addDependency :: (IsNSOperation nsOperation, IsNSOperation op) => nsOperation -> op -> IO ()
addDependency nsOperation op =
  sendMessage nsOperation addDependencySelector (toNSOperation op)

-- | @- removeDependency:@
removeDependency :: (IsNSOperation nsOperation, IsNSOperation op) => nsOperation -> op -> IO ()
removeDependency nsOperation op =
  sendMessage nsOperation removeDependencySelector (toNSOperation op)

-- | @- waitUntilFinished@
waitUntilFinished :: IsNSOperation nsOperation => nsOperation -> IO ()
waitUntilFinished nsOperation =
  sendMessage nsOperation waitUntilFinishedSelector

-- | @- cancelled@
cancelled :: IsNSOperation nsOperation => nsOperation -> IO Bool
cancelled nsOperation =
  sendMessage nsOperation cancelledSelector

-- | @- executing@
executing :: IsNSOperation nsOperation => nsOperation -> IO Bool
executing nsOperation =
  sendMessage nsOperation executingSelector

-- | @- finished@
finished :: IsNSOperation nsOperation => nsOperation -> IO Bool
finished nsOperation =
  sendMessage nsOperation finishedSelector

-- | @- concurrent@
concurrent :: IsNSOperation nsOperation => nsOperation -> IO Bool
concurrent nsOperation =
  sendMessage nsOperation concurrentSelector

-- | @- asynchronous@
asynchronous :: IsNSOperation nsOperation => nsOperation -> IO Bool
asynchronous nsOperation =
  sendMessage nsOperation asynchronousSelector

-- | @- ready@
ready :: IsNSOperation nsOperation => nsOperation -> IO Bool
ready nsOperation =
  sendMessage nsOperation readySelector

-- | @- dependencies@
dependencies :: IsNSOperation nsOperation => nsOperation -> IO (Id NSArray)
dependencies nsOperation =
  sendMessage nsOperation dependenciesSelector

-- | @- queuePriority@
queuePriority :: IsNSOperation nsOperation => nsOperation -> IO NSOperationQueuePriority
queuePriority nsOperation =
  sendMessage nsOperation queuePrioritySelector

-- | @- setQueuePriority:@
setQueuePriority :: IsNSOperation nsOperation => nsOperation -> NSOperationQueuePriority -> IO ()
setQueuePriority nsOperation value =
  sendMessage nsOperation setQueuePrioritySelector value

-- | @- completionBlock@
completionBlock :: IsNSOperation nsOperation => nsOperation -> IO (Ptr ())
completionBlock nsOperation =
  sendMessage nsOperation completionBlockSelector

-- | @- setCompletionBlock:@
setCompletionBlock :: IsNSOperation nsOperation => nsOperation -> Ptr () -> IO ()
setCompletionBlock nsOperation value =
  sendMessage nsOperation setCompletionBlockSelector value

-- | @- threadPriority@
threadPriority :: IsNSOperation nsOperation => nsOperation -> IO CDouble
threadPriority nsOperation =
  sendMessage nsOperation threadPrioritySelector

-- | @- setThreadPriority:@
setThreadPriority :: IsNSOperation nsOperation => nsOperation -> CDouble -> IO ()
setThreadPriority nsOperation value =
  sendMessage nsOperation setThreadPrioritySelector value

-- | @- qualityOfService@
qualityOfService :: IsNSOperation nsOperation => nsOperation -> IO NSQualityOfService
qualityOfService nsOperation =
  sendMessage nsOperation qualityOfServiceSelector

-- | @- setQualityOfService:@
setQualityOfService :: IsNSOperation nsOperation => nsOperation -> NSQualityOfService -> IO ()
setQualityOfService nsOperation value =
  sendMessage nsOperation setQualityOfServiceSelector value

-- | @- name@
name :: IsNSOperation nsOperation => nsOperation -> IO (Id NSString)
name nsOperation =
  sendMessage nsOperation nameSelector

-- | @- setName:@
setName :: (IsNSOperation nsOperation, IsNSString value) => nsOperation -> value -> IO ()
setName nsOperation value =
  sendMessage nsOperation setNameSelector (toNSString value)

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @start@
startSelector :: Selector '[] ()
startSelector = mkSelector "start"

-- | @Selector@ for @main@
mainSelector :: Selector '[] ()
mainSelector = mkSelector "main"

-- | @Selector@ for @cancel@
cancelSelector :: Selector '[] ()
cancelSelector = mkSelector "cancel"

-- | @Selector@ for @addDependency:@
addDependencySelector :: Selector '[Id NSOperation] ()
addDependencySelector = mkSelector "addDependency:"

-- | @Selector@ for @removeDependency:@
removeDependencySelector :: Selector '[Id NSOperation] ()
removeDependencySelector = mkSelector "removeDependency:"

-- | @Selector@ for @waitUntilFinished@
waitUntilFinishedSelector :: Selector '[] ()
waitUntilFinishedSelector = mkSelector "waitUntilFinished"

-- | @Selector@ for @cancelled@
cancelledSelector :: Selector '[] Bool
cancelledSelector = mkSelector "cancelled"

-- | @Selector@ for @executing@
executingSelector :: Selector '[] Bool
executingSelector = mkSelector "executing"

-- | @Selector@ for @finished@
finishedSelector :: Selector '[] Bool
finishedSelector = mkSelector "finished"

-- | @Selector@ for @concurrent@
concurrentSelector :: Selector '[] Bool
concurrentSelector = mkSelector "concurrent"

-- | @Selector@ for @asynchronous@
asynchronousSelector :: Selector '[] Bool
asynchronousSelector = mkSelector "asynchronous"

-- | @Selector@ for @ready@
readySelector :: Selector '[] Bool
readySelector = mkSelector "ready"

-- | @Selector@ for @dependencies@
dependenciesSelector :: Selector '[] (Id NSArray)
dependenciesSelector = mkSelector "dependencies"

-- | @Selector@ for @queuePriority@
queuePrioritySelector :: Selector '[] NSOperationQueuePriority
queuePrioritySelector = mkSelector "queuePriority"

-- | @Selector@ for @setQueuePriority:@
setQueuePrioritySelector :: Selector '[NSOperationQueuePriority] ()
setQueuePrioritySelector = mkSelector "setQueuePriority:"

-- | @Selector@ for @completionBlock@
completionBlockSelector :: Selector '[] (Ptr ())
completionBlockSelector = mkSelector "completionBlock"

-- | @Selector@ for @setCompletionBlock:@
setCompletionBlockSelector :: Selector '[Ptr ()] ()
setCompletionBlockSelector = mkSelector "setCompletionBlock:"

-- | @Selector@ for @threadPriority@
threadPrioritySelector :: Selector '[] CDouble
threadPrioritySelector = mkSelector "threadPriority"

-- | @Selector@ for @setThreadPriority:@
setThreadPrioritySelector :: Selector '[CDouble] ()
setThreadPrioritySelector = mkSelector "setThreadPriority:"

-- | @Selector@ for @qualityOfService@
qualityOfServiceSelector :: Selector '[] NSQualityOfService
qualityOfServiceSelector = mkSelector "qualityOfService"

-- | @Selector@ for @setQualityOfService:@
setQualityOfServiceSelector :: Selector '[NSQualityOfService] ()
setQualityOfServiceSelector = mkSelector "setQualityOfService:"

-- | @Selector@ for @name@
nameSelector :: Selector '[] (Id NSString)
nameSelector = mkSelector "name"

-- | @Selector@ for @setName:@
setNameSelector :: Selector '[Id NSString] ()
setNameSelector = mkSelector "setName:"

