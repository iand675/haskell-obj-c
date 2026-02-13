{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Generated bindings for @NSOperationQueue@.
module ObjC.Foundation.NSOperationQueue
  ( NSOperationQueue
  , IsNSOperationQueue(..)
  , addOperation
  , addOperations_waitUntilFinished
  , addOperationWithBlock
  , addBarrierBlock
  , cancelAllOperations
  , waitUntilAllOperationsAreFinished
  , progress
  , maxConcurrentOperationCount
  , setMaxConcurrentOperationCount
  , suspended
  , setSuspended
  , name
  , setName
  , qualityOfService
  , setQualityOfService
  , underlyingQueue
  , setUnderlyingQueue
  , currentQueue
  , mainQueue
  , operations
  , operationCount
  , addBarrierBlockSelector
  , addOperationSelector
  , addOperationWithBlockSelector
  , addOperations_waitUntilFinishedSelector
  , cancelAllOperationsSelector
  , currentQueueSelector
  , mainQueueSelector
  , maxConcurrentOperationCountSelector
  , nameSelector
  , operationCountSelector
  , operationsSelector
  , progressSelector
  , qualityOfServiceSelector
  , setMaxConcurrentOperationCountSelector
  , setNameSelector
  , setQualityOfServiceSelector
  , setSuspendedSelector
  , setUnderlyingQueueSelector
  , suspendedSelector
  , underlyingQueueSelector
  , waitUntilAllOperationsAreFinishedSelector

  -- * Enum types
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

-- | @- addOperation:@
addOperation :: (IsNSOperationQueue nsOperationQueue, IsNSOperation op) => nsOperationQueue -> op -> IO ()
addOperation nsOperationQueue op =
  sendMessage nsOperationQueue addOperationSelector (toNSOperation op)

-- | @- addOperations:waitUntilFinished:@
addOperations_waitUntilFinished :: (IsNSOperationQueue nsOperationQueue, IsNSArray ops) => nsOperationQueue -> ops -> Bool -> IO ()
addOperations_waitUntilFinished nsOperationQueue ops wait =
  sendMessage nsOperationQueue addOperations_waitUntilFinishedSelector (toNSArray ops) wait

-- | @- addOperationWithBlock:@
addOperationWithBlock :: IsNSOperationQueue nsOperationQueue => nsOperationQueue -> Ptr () -> IO ()
addOperationWithBlock nsOperationQueue block =
  sendMessage nsOperationQueue addOperationWithBlockSelector block

-- | addBarrierBlock:
--
-- @barrier@ — A block to execute
--
-- The @addBarrierBlock:@ method executes the block when the NSOperationQueue has finished all enqueued operations and prevents any subsequent operations to be executed until the barrier has been completed. This acts similarly to the @dispatch_barrier_async@ function.
--
-- ObjC selector: @- addBarrierBlock:@
addBarrierBlock :: IsNSOperationQueue nsOperationQueue => nsOperationQueue -> Ptr () -> IO ()
addBarrierBlock nsOperationQueue barrier =
  sendMessage nsOperationQueue addBarrierBlockSelector barrier

-- | @- cancelAllOperations@
cancelAllOperations :: IsNSOperationQueue nsOperationQueue => nsOperationQueue -> IO ()
cancelAllOperations nsOperationQueue =
  sendMessage nsOperationQueue cancelAllOperationsSelector

-- | @- waitUntilAllOperationsAreFinished@
waitUntilAllOperationsAreFinished :: IsNSOperationQueue nsOperationQueue => nsOperationQueue -> IO ()
waitUntilAllOperationsAreFinished nsOperationQueue =
  sendMessage nsOperationQueue waitUntilAllOperationsAreFinishedSelector

-- | progress
--
-- The @progress@ property represents a total progress of the operations executed in the queue. By default NSOperationQueue does not report progress until the @totalUnitCount@ of the progress is set. When the @totalUnitCount@ property of the progress is set the queue then opts into participating in progress reporting. When enabled, each operation will contribute 1 unit of completion to the overall progress of the queue for operations that are finished by the end of main (operations that override start and do not invoke super will not contribute to progress). Special attention to race conditions should be made when updating the @totalUnitCount@ of the progress as well as care should be taken to avoid 'backwards progress'. For example; when a NSOperationQueue's progress is 5/10, representing 50% completed, and there are 90 more operations about to be added and the @totalUnitCount@ that would then make the progress report as 5/100 which represents 5%. In this example it would mean that any progress bar would jump from displaying 50% back to 5%, which might not be desirable. In the cases where the @totalUnitCount@ needs to be adjusted it is suggested to do this for thread-safety in a barrier by using the @addBarrierBlock:@ API. This ensures that no un-expected execution state occurs adjusting into a potentially backwards moving progress scenario.
--
-- NSOperationQueue *queue = [[NSOperationQueue alloc] init]; queue.progress.totalUnitCount = 10;
--
-- ObjC selector: @- progress@
progress :: IsNSOperationQueue nsOperationQueue => nsOperationQueue -> IO (Id NSProgress)
progress nsOperationQueue =
  sendMessage nsOperationQueue progressSelector

-- | @- maxConcurrentOperationCount@
maxConcurrentOperationCount :: IsNSOperationQueue nsOperationQueue => nsOperationQueue -> IO CLong
maxConcurrentOperationCount nsOperationQueue =
  sendMessage nsOperationQueue maxConcurrentOperationCountSelector

-- | @- setMaxConcurrentOperationCount:@
setMaxConcurrentOperationCount :: IsNSOperationQueue nsOperationQueue => nsOperationQueue -> CLong -> IO ()
setMaxConcurrentOperationCount nsOperationQueue value =
  sendMessage nsOperationQueue setMaxConcurrentOperationCountSelector value

-- | @- suspended@
suspended :: IsNSOperationQueue nsOperationQueue => nsOperationQueue -> IO Bool
suspended nsOperationQueue =
  sendMessage nsOperationQueue suspendedSelector

-- | @- setSuspended:@
setSuspended :: IsNSOperationQueue nsOperationQueue => nsOperationQueue -> Bool -> IO ()
setSuspended nsOperationQueue value =
  sendMessage nsOperationQueue setSuspendedSelector value

-- | @- name@
name :: IsNSOperationQueue nsOperationQueue => nsOperationQueue -> IO (Id NSString)
name nsOperationQueue =
  sendMessage nsOperationQueue nameSelector

-- | @- setName:@
setName :: (IsNSOperationQueue nsOperationQueue, IsNSString value) => nsOperationQueue -> value -> IO ()
setName nsOperationQueue value =
  sendMessage nsOperationQueue setNameSelector (toNSString value)

-- | @- qualityOfService@
qualityOfService :: IsNSOperationQueue nsOperationQueue => nsOperationQueue -> IO NSQualityOfService
qualityOfService nsOperationQueue =
  sendMessage nsOperationQueue qualityOfServiceSelector

-- | @- setQualityOfService:@
setQualityOfService :: IsNSOperationQueue nsOperationQueue => nsOperationQueue -> NSQualityOfService -> IO ()
setQualityOfService nsOperationQueue value =
  sendMessage nsOperationQueue setQualityOfServiceSelector value

-- | @- underlyingQueue@
underlyingQueue :: IsNSOperationQueue nsOperationQueue => nsOperationQueue -> IO (Id NSObject)
underlyingQueue nsOperationQueue =
  sendMessage nsOperationQueue underlyingQueueSelector

-- | @- setUnderlyingQueue:@
setUnderlyingQueue :: (IsNSOperationQueue nsOperationQueue, IsNSObject value) => nsOperationQueue -> value -> IO ()
setUnderlyingQueue nsOperationQueue value =
  sendMessage nsOperationQueue setUnderlyingQueueSelector (toNSObject value)

-- | @+ currentQueue@
currentQueue :: IO (Id NSOperationQueue)
currentQueue  =
  do
    cls' <- getRequiredClass "NSOperationQueue"
    sendClassMessage cls' currentQueueSelector

-- | @+ mainQueue@
mainQueue :: IO (Id NSOperationQueue)
mainQueue  =
  do
    cls' <- getRequiredClass "NSOperationQueue"
    sendClassMessage cls' mainQueueSelector

-- | @- operations@
operations :: IsNSOperationQueue nsOperationQueue => nsOperationQueue -> IO (Id NSArray)
operations nsOperationQueue =
  sendMessage nsOperationQueue operationsSelector

-- | @- operationCount@
operationCount :: IsNSOperationQueue nsOperationQueue => nsOperationQueue -> IO CULong
operationCount nsOperationQueue =
  sendMessage nsOperationQueue operationCountSelector

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @addOperation:@
addOperationSelector :: Selector '[Id NSOperation] ()
addOperationSelector = mkSelector "addOperation:"

-- | @Selector@ for @addOperations:waitUntilFinished:@
addOperations_waitUntilFinishedSelector :: Selector '[Id NSArray, Bool] ()
addOperations_waitUntilFinishedSelector = mkSelector "addOperations:waitUntilFinished:"

-- | @Selector@ for @addOperationWithBlock:@
addOperationWithBlockSelector :: Selector '[Ptr ()] ()
addOperationWithBlockSelector = mkSelector "addOperationWithBlock:"

-- | @Selector@ for @addBarrierBlock:@
addBarrierBlockSelector :: Selector '[Ptr ()] ()
addBarrierBlockSelector = mkSelector "addBarrierBlock:"

-- | @Selector@ for @cancelAllOperations@
cancelAllOperationsSelector :: Selector '[] ()
cancelAllOperationsSelector = mkSelector "cancelAllOperations"

-- | @Selector@ for @waitUntilAllOperationsAreFinished@
waitUntilAllOperationsAreFinishedSelector :: Selector '[] ()
waitUntilAllOperationsAreFinishedSelector = mkSelector "waitUntilAllOperationsAreFinished"

-- | @Selector@ for @progress@
progressSelector :: Selector '[] (Id NSProgress)
progressSelector = mkSelector "progress"

-- | @Selector@ for @maxConcurrentOperationCount@
maxConcurrentOperationCountSelector :: Selector '[] CLong
maxConcurrentOperationCountSelector = mkSelector "maxConcurrentOperationCount"

-- | @Selector@ for @setMaxConcurrentOperationCount:@
setMaxConcurrentOperationCountSelector :: Selector '[CLong] ()
setMaxConcurrentOperationCountSelector = mkSelector "setMaxConcurrentOperationCount:"

-- | @Selector@ for @suspended@
suspendedSelector :: Selector '[] Bool
suspendedSelector = mkSelector "suspended"

-- | @Selector@ for @setSuspended:@
setSuspendedSelector :: Selector '[Bool] ()
setSuspendedSelector = mkSelector "setSuspended:"

-- | @Selector@ for @name@
nameSelector :: Selector '[] (Id NSString)
nameSelector = mkSelector "name"

-- | @Selector@ for @setName:@
setNameSelector :: Selector '[Id NSString] ()
setNameSelector = mkSelector "setName:"

-- | @Selector@ for @qualityOfService@
qualityOfServiceSelector :: Selector '[] NSQualityOfService
qualityOfServiceSelector = mkSelector "qualityOfService"

-- | @Selector@ for @setQualityOfService:@
setQualityOfServiceSelector :: Selector '[NSQualityOfService] ()
setQualityOfServiceSelector = mkSelector "setQualityOfService:"

-- | @Selector@ for @underlyingQueue@
underlyingQueueSelector :: Selector '[] (Id NSObject)
underlyingQueueSelector = mkSelector "underlyingQueue"

-- | @Selector@ for @setUnderlyingQueue:@
setUnderlyingQueueSelector :: Selector '[Id NSObject] ()
setUnderlyingQueueSelector = mkSelector "setUnderlyingQueue:"

-- | @Selector@ for @currentQueue@
currentQueueSelector :: Selector '[] (Id NSOperationQueue)
currentQueueSelector = mkSelector "currentQueue"

-- | @Selector@ for @mainQueue@
mainQueueSelector :: Selector '[] (Id NSOperationQueue)
mainQueueSelector = mkSelector "mainQueue"

-- | @Selector@ for @operations@
operationsSelector :: Selector '[] (Id NSArray)
operationsSelector = mkSelector "operations"

-- | @Selector@ for @operationCount@
operationCountSelector :: Selector '[] CULong
operationCountSelector = mkSelector "operationCount"

