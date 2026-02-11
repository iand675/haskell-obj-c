{-# LANGUAGE PatternSynonyms #-}
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
  , startSelector
  , mainSelector
  , cancelSelector
  , addDependencySelector
  , removeDependencySelector
  , waitUntilFinishedSelector
  , cancelledSelector
  , executingSelector
  , finishedSelector
  , concurrentSelector
  , asynchronousSelector
  , readySelector
  , dependenciesSelector
  , queuePrioritySelector
  , setQueuePrioritySelector
  , completionBlockSelector
  , setCompletionBlockSelector
  , threadPrioritySelector
  , setThreadPrioritySelector
  , qualityOfServiceSelector
  , setQualityOfServiceSelector
  , nameSelector
  , setNameSelector

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

-- | @- start@
start :: IsNSOperation nsOperation => nsOperation -> IO ()
start nsOperation  =
  sendMsg nsOperation (mkSelector "start") retVoid []

-- | @- main@
main :: IsNSOperation nsOperation => nsOperation -> IO ()
main nsOperation  =
  sendMsg nsOperation (mkSelector "main") retVoid []

-- | @- cancel@
cancel :: IsNSOperation nsOperation => nsOperation -> IO ()
cancel nsOperation  =
  sendMsg nsOperation (mkSelector "cancel") retVoid []

-- | @- addDependency:@
addDependency :: (IsNSOperation nsOperation, IsNSOperation op) => nsOperation -> op -> IO ()
addDependency nsOperation  op =
withObjCPtr op $ \raw_op ->
    sendMsg nsOperation (mkSelector "addDependency:") retVoid [argPtr (castPtr raw_op :: Ptr ())]

-- | @- removeDependency:@
removeDependency :: (IsNSOperation nsOperation, IsNSOperation op) => nsOperation -> op -> IO ()
removeDependency nsOperation  op =
withObjCPtr op $ \raw_op ->
    sendMsg nsOperation (mkSelector "removeDependency:") retVoid [argPtr (castPtr raw_op :: Ptr ())]

-- | @- waitUntilFinished@
waitUntilFinished :: IsNSOperation nsOperation => nsOperation -> IO ()
waitUntilFinished nsOperation  =
  sendMsg nsOperation (mkSelector "waitUntilFinished") retVoid []

-- | @- cancelled@
cancelled :: IsNSOperation nsOperation => nsOperation -> IO Bool
cancelled nsOperation  =
  fmap ((/= 0) :: CULong -> Bool) $ sendMsg nsOperation (mkSelector "cancelled") retCULong []

-- | @- executing@
executing :: IsNSOperation nsOperation => nsOperation -> IO Bool
executing nsOperation  =
  fmap ((/= 0) :: CULong -> Bool) $ sendMsg nsOperation (mkSelector "executing") retCULong []

-- | @- finished@
finished :: IsNSOperation nsOperation => nsOperation -> IO Bool
finished nsOperation  =
  fmap ((/= 0) :: CULong -> Bool) $ sendMsg nsOperation (mkSelector "finished") retCULong []

-- | @- concurrent@
concurrent :: IsNSOperation nsOperation => nsOperation -> IO Bool
concurrent nsOperation  =
  fmap ((/= 0) :: CULong -> Bool) $ sendMsg nsOperation (mkSelector "concurrent") retCULong []

-- | @- asynchronous@
asynchronous :: IsNSOperation nsOperation => nsOperation -> IO Bool
asynchronous nsOperation  =
  fmap ((/= 0) :: CULong -> Bool) $ sendMsg nsOperation (mkSelector "asynchronous") retCULong []

-- | @- ready@
ready :: IsNSOperation nsOperation => nsOperation -> IO Bool
ready nsOperation  =
  fmap ((/= 0) :: CULong -> Bool) $ sendMsg nsOperation (mkSelector "ready") retCULong []

-- | @- dependencies@
dependencies :: IsNSOperation nsOperation => nsOperation -> IO (Id NSArray)
dependencies nsOperation  =
  sendMsg nsOperation (mkSelector "dependencies") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- queuePriority@
queuePriority :: IsNSOperation nsOperation => nsOperation -> IO NSOperationQueuePriority
queuePriority nsOperation  =
  fmap (coerce :: CLong -> NSOperationQueuePriority) $ sendMsg nsOperation (mkSelector "queuePriority") retCLong []

-- | @- setQueuePriority:@
setQueuePriority :: IsNSOperation nsOperation => nsOperation -> NSOperationQueuePriority -> IO ()
setQueuePriority nsOperation  value =
  sendMsg nsOperation (mkSelector "setQueuePriority:") retVoid [argCLong (coerce value)]

-- | @- completionBlock@
completionBlock :: IsNSOperation nsOperation => nsOperation -> IO (Ptr ())
completionBlock nsOperation  =
  fmap castPtr $ sendMsg nsOperation (mkSelector "completionBlock") (retPtr retVoid) []

-- | @- setCompletionBlock:@
setCompletionBlock :: IsNSOperation nsOperation => nsOperation -> Ptr () -> IO ()
setCompletionBlock nsOperation  value =
  sendMsg nsOperation (mkSelector "setCompletionBlock:") retVoid [argPtr (castPtr value :: Ptr ())]

-- | @- threadPriority@
threadPriority :: IsNSOperation nsOperation => nsOperation -> IO CDouble
threadPriority nsOperation  =
  sendMsg nsOperation (mkSelector "threadPriority") retCDouble []

-- | @- setThreadPriority:@
setThreadPriority :: IsNSOperation nsOperation => nsOperation -> CDouble -> IO ()
setThreadPriority nsOperation  value =
  sendMsg nsOperation (mkSelector "setThreadPriority:") retVoid [argCDouble (fromIntegral value)]

-- | @- qualityOfService@
qualityOfService :: IsNSOperation nsOperation => nsOperation -> IO NSQualityOfService
qualityOfService nsOperation  =
  fmap (coerce :: CLong -> NSQualityOfService) $ sendMsg nsOperation (mkSelector "qualityOfService") retCLong []

-- | @- setQualityOfService:@
setQualityOfService :: IsNSOperation nsOperation => nsOperation -> NSQualityOfService -> IO ()
setQualityOfService nsOperation  value =
  sendMsg nsOperation (mkSelector "setQualityOfService:") retVoid [argCLong (coerce value)]

-- | @- name@
name :: IsNSOperation nsOperation => nsOperation -> IO (Id NSString)
name nsOperation  =
  sendMsg nsOperation (mkSelector "name") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- setName:@
setName :: (IsNSOperation nsOperation, IsNSString value) => nsOperation -> value -> IO ()
setName nsOperation  value =
withObjCPtr value $ \raw_value ->
    sendMsg nsOperation (mkSelector "setName:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @start@
startSelector :: Selector
startSelector = mkSelector "start"

-- | @Selector@ for @main@
mainSelector :: Selector
mainSelector = mkSelector "main"

-- | @Selector@ for @cancel@
cancelSelector :: Selector
cancelSelector = mkSelector "cancel"

-- | @Selector@ for @addDependency:@
addDependencySelector :: Selector
addDependencySelector = mkSelector "addDependency:"

-- | @Selector@ for @removeDependency:@
removeDependencySelector :: Selector
removeDependencySelector = mkSelector "removeDependency:"

-- | @Selector@ for @waitUntilFinished@
waitUntilFinishedSelector :: Selector
waitUntilFinishedSelector = mkSelector "waitUntilFinished"

-- | @Selector@ for @cancelled@
cancelledSelector :: Selector
cancelledSelector = mkSelector "cancelled"

-- | @Selector@ for @executing@
executingSelector :: Selector
executingSelector = mkSelector "executing"

-- | @Selector@ for @finished@
finishedSelector :: Selector
finishedSelector = mkSelector "finished"

-- | @Selector@ for @concurrent@
concurrentSelector :: Selector
concurrentSelector = mkSelector "concurrent"

-- | @Selector@ for @asynchronous@
asynchronousSelector :: Selector
asynchronousSelector = mkSelector "asynchronous"

-- | @Selector@ for @ready@
readySelector :: Selector
readySelector = mkSelector "ready"

-- | @Selector@ for @dependencies@
dependenciesSelector :: Selector
dependenciesSelector = mkSelector "dependencies"

-- | @Selector@ for @queuePriority@
queuePrioritySelector :: Selector
queuePrioritySelector = mkSelector "queuePriority"

-- | @Selector@ for @setQueuePriority:@
setQueuePrioritySelector :: Selector
setQueuePrioritySelector = mkSelector "setQueuePriority:"

-- | @Selector@ for @completionBlock@
completionBlockSelector :: Selector
completionBlockSelector = mkSelector "completionBlock"

-- | @Selector@ for @setCompletionBlock:@
setCompletionBlockSelector :: Selector
setCompletionBlockSelector = mkSelector "setCompletionBlock:"

-- | @Selector@ for @threadPriority@
threadPrioritySelector :: Selector
threadPrioritySelector = mkSelector "threadPriority"

-- | @Selector@ for @setThreadPriority:@
setThreadPrioritySelector :: Selector
setThreadPrioritySelector = mkSelector "setThreadPriority:"

-- | @Selector@ for @qualityOfService@
qualityOfServiceSelector :: Selector
qualityOfServiceSelector = mkSelector "qualityOfService"

-- | @Selector@ for @setQualityOfService:@
setQualityOfServiceSelector :: Selector
setQualityOfServiceSelector = mkSelector "setQualityOfService:"

-- | @Selector@ for @name@
nameSelector :: Selector
nameSelector = mkSelector "name"

-- | @Selector@ for @setName:@
setNameSelector :: Selector
setNameSelector = mkSelector "setName:"

