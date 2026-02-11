{-# LANGUAGE PatternSynonyms #-}
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
  , addOperationSelector
  , addOperations_waitUntilFinishedSelector
  , addOperationWithBlockSelector
  , addBarrierBlockSelector
  , cancelAllOperationsSelector
  , waitUntilAllOperationsAreFinishedSelector
  , progressSelector
  , maxConcurrentOperationCountSelector
  , setMaxConcurrentOperationCountSelector
  , suspendedSelector
  , setSuspendedSelector
  , nameSelector
  , setNameSelector
  , qualityOfServiceSelector
  , setQualityOfServiceSelector
  , underlyingQueueSelector
  , setUnderlyingQueueSelector
  , currentQueueSelector
  , mainQueueSelector
  , operationsSelector
  , operationCountSelector

  -- * Enum types
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

-- | @- addOperation:@
addOperation :: (IsNSOperationQueue nsOperationQueue, IsNSOperation op) => nsOperationQueue -> op -> IO ()
addOperation nsOperationQueue  op =
withObjCPtr op $ \raw_op ->
    sendMsg nsOperationQueue (mkSelector "addOperation:") retVoid [argPtr (castPtr raw_op :: Ptr ())]

-- | @- addOperations:waitUntilFinished:@
addOperations_waitUntilFinished :: (IsNSOperationQueue nsOperationQueue, IsNSArray ops) => nsOperationQueue -> ops -> Bool -> IO ()
addOperations_waitUntilFinished nsOperationQueue  ops wait =
withObjCPtr ops $ \raw_ops ->
    sendMsg nsOperationQueue (mkSelector "addOperations:waitUntilFinished:") retVoid [argPtr (castPtr raw_ops :: Ptr ()), argCULong (if wait then 1 else 0)]

-- | @- addOperationWithBlock:@
addOperationWithBlock :: IsNSOperationQueue nsOperationQueue => nsOperationQueue -> Ptr () -> IO ()
addOperationWithBlock nsOperationQueue  block =
  sendMsg nsOperationQueue (mkSelector "addOperationWithBlock:") retVoid [argPtr (castPtr block :: Ptr ())]

-- | addBarrierBlock:
--
-- @barrier@ — A block to execute
--
-- The @addBarrierBlock:@ method executes the block when the NSOperationQueue has finished all enqueued operations and prevents any subsequent operations to be executed until the barrier has been completed. This acts similarly to the @dispatch_barrier_async@ function.
--
-- ObjC selector: @- addBarrierBlock:@
addBarrierBlock :: IsNSOperationQueue nsOperationQueue => nsOperationQueue -> Ptr () -> IO ()
addBarrierBlock nsOperationQueue  barrier =
  sendMsg nsOperationQueue (mkSelector "addBarrierBlock:") retVoid [argPtr (castPtr barrier :: Ptr ())]

-- | @- cancelAllOperations@
cancelAllOperations :: IsNSOperationQueue nsOperationQueue => nsOperationQueue -> IO ()
cancelAllOperations nsOperationQueue  =
  sendMsg nsOperationQueue (mkSelector "cancelAllOperations") retVoid []

-- | @- waitUntilAllOperationsAreFinished@
waitUntilAllOperationsAreFinished :: IsNSOperationQueue nsOperationQueue => nsOperationQueue -> IO ()
waitUntilAllOperationsAreFinished nsOperationQueue  =
  sendMsg nsOperationQueue (mkSelector "waitUntilAllOperationsAreFinished") retVoid []

-- | progress
--
-- The @progress@ property represents a total progress of the operations executed in the queue. By default NSOperationQueue does not report progress until the @totalUnitCount@ of the progress is set. When the @totalUnitCount@ property of the progress is set the queue then opts into participating in progress reporting. When enabled, each operation will contribute 1 unit of completion to the overall progress of the queue for operations that are finished by the end of main (operations that override start and do not invoke super will not contribute to progress). Special attention to race conditions should be made when updating the @totalUnitCount@ of the progress as well as care should be taken to avoid 'backwards progress'. For example; when a NSOperationQueue's progress is 5/10, representing 50% completed, and there are 90 more operations about to be added and the @totalUnitCount@ that would then make the progress report as 5/100 which represents 5%. In this example it would mean that any progress bar would jump from displaying 50% back to 5%, which might not be desirable. In the cases where the @totalUnitCount@ needs to be adjusted it is suggested to do this for thread-safety in a barrier by using the @addBarrierBlock:@ API. This ensures that no un-expected execution state occurs adjusting into a potentially backwards moving progress scenario.
--
-- NSOperationQueue *queue = [[NSOperationQueue alloc] init]; queue.progress.totalUnitCount = 10;
--
-- ObjC selector: @- progress@
progress :: IsNSOperationQueue nsOperationQueue => nsOperationQueue -> IO (Id NSProgress)
progress nsOperationQueue  =
  sendMsg nsOperationQueue (mkSelector "progress") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- maxConcurrentOperationCount@
maxConcurrentOperationCount :: IsNSOperationQueue nsOperationQueue => nsOperationQueue -> IO CLong
maxConcurrentOperationCount nsOperationQueue  =
  sendMsg nsOperationQueue (mkSelector "maxConcurrentOperationCount") retCLong []

-- | @- setMaxConcurrentOperationCount:@
setMaxConcurrentOperationCount :: IsNSOperationQueue nsOperationQueue => nsOperationQueue -> CLong -> IO ()
setMaxConcurrentOperationCount nsOperationQueue  value =
  sendMsg nsOperationQueue (mkSelector "setMaxConcurrentOperationCount:") retVoid [argCLong (fromIntegral value)]

-- | @- suspended@
suspended :: IsNSOperationQueue nsOperationQueue => nsOperationQueue -> IO Bool
suspended nsOperationQueue  =
  fmap ((/= 0) :: CULong -> Bool) $ sendMsg nsOperationQueue (mkSelector "suspended") retCULong []

-- | @- setSuspended:@
setSuspended :: IsNSOperationQueue nsOperationQueue => nsOperationQueue -> Bool -> IO ()
setSuspended nsOperationQueue  value =
  sendMsg nsOperationQueue (mkSelector "setSuspended:") retVoid [argCULong (if value then 1 else 0)]

-- | @- name@
name :: IsNSOperationQueue nsOperationQueue => nsOperationQueue -> IO (Id NSString)
name nsOperationQueue  =
  sendMsg nsOperationQueue (mkSelector "name") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- setName:@
setName :: (IsNSOperationQueue nsOperationQueue, IsNSString value) => nsOperationQueue -> value -> IO ()
setName nsOperationQueue  value =
withObjCPtr value $ \raw_value ->
    sendMsg nsOperationQueue (mkSelector "setName:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- | @- qualityOfService@
qualityOfService :: IsNSOperationQueue nsOperationQueue => nsOperationQueue -> IO NSQualityOfService
qualityOfService nsOperationQueue  =
  fmap (coerce :: CLong -> NSQualityOfService) $ sendMsg nsOperationQueue (mkSelector "qualityOfService") retCLong []

-- | @- setQualityOfService:@
setQualityOfService :: IsNSOperationQueue nsOperationQueue => nsOperationQueue -> NSQualityOfService -> IO ()
setQualityOfService nsOperationQueue  value =
  sendMsg nsOperationQueue (mkSelector "setQualityOfService:") retVoid [argCLong (coerce value)]

-- | @- underlyingQueue@
underlyingQueue :: IsNSOperationQueue nsOperationQueue => nsOperationQueue -> IO (Id NSObject)
underlyingQueue nsOperationQueue  =
  sendMsg nsOperationQueue (mkSelector "underlyingQueue") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- setUnderlyingQueue:@
setUnderlyingQueue :: (IsNSOperationQueue nsOperationQueue, IsNSObject value) => nsOperationQueue -> value -> IO ()
setUnderlyingQueue nsOperationQueue  value =
withObjCPtr value $ \raw_value ->
    sendMsg nsOperationQueue (mkSelector "setUnderlyingQueue:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- | @+ currentQueue@
currentQueue :: IO (Id NSOperationQueue)
currentQueue  =
  do
    cls' <- getRequiredClass "NSOperationQueue"
    sendClassMsg cls' (mkSelector "currentQueue") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @+ mainQueue@
mainQueue :: IO (Id NSOperationQueue)
mainQueue  =
  do
    cls' <- getRequiredClass "NSOperationQueue"
    sendClassMsg cls' (mkSelector "mainQueue") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- operations@
operations :: IsNSOperationQueue nsOperationQueue => nsOperationQueue -> IO (Id NSArray)
operations nsOperationQueue  =
  sendMsg nsOperationQueue (mkSelector "operations") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- operationCount@
operationCount :: IsNSOperationQueue nsOperationQueue => nsOperationQueue -> IO CULong
operationCount nsOperationQueue  =
  sendMsg nsOperationQueue (mkSelector "operationCount") retCULong []

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @addOperation:@
addOperationSelector :: Selector
addOperationSelector = mkSelector "addOperation:"

-- | @Selector@ for @addOperations:waitUntilFinished:@
addOperations_waitUntilFinishedSelector :: Selector
addOperations_waitUntilFinishedSelector = mkSelector "addOperations:waitUntilFinished:"

-- | @Selector@ for @addOperationWithBlock:@
addOperationWithBlockSelector :: Selector
addOperationWithBlockSelector = mkSelector "addOperationWithBlock:"

-- | @Selector@ for @addBarrierBlock:@
addBarrierBlockSelector :: Selector
addBarrierBlockSelector = mkSelector "addBarrierBlock:"

-- | @Selector@ for @cancelAllOperations@
cancelAllOperationsSelector :: Selector
cancelAllOperationsSelector = mkSelector "cancelAllOperations"

-- | @Selector@ for @waitUntilAllOperationsAreFinished@
waitUntilAllOperationsAreFinishedSelector :: Selector
waitUntilAllOperationsAreFinishedSelector = mkSelector "waitUntilAllOperationsAreFinished"

-- | @Selector@ for @progress@
progressSelector :: Selector
progressSelector = mkSelector "progress"

-- | @Selector@ for @maxConcurrentOperationCount@
maxConcurrentOperationCountSelector :: Selector
maxConcurrentOperationCountSelector = mkSelector "maxConcurrentOperationCount"

-- | @Selector@ for @setMaxConcurrentOperationCount:@
setMaxConcurrentOperationCountSelector :: Selector
setMaxConcurrentOperationCountSelector = mkSelector "setMaxConcurrentOperationCount:"

-- | @Selector@ for @suspended@
suspendedSelector :: Selector
suspendedSelector = mkSelector "suspended"

-- | @Selector@ for @setSuspended:@
setSuspendedSelector :: Selector
setSuspendedSelector = mkSelector "setSuspended:"

-- | @Selector@ for @name@
nameSelector :: Selector
nameSelector = mkSelector "name"

-- | @Selector@ for @setName:@
setNameSelector :: Selector
setNameSelector = mkSelector "setName:"

-- | @Selector@ for @qualityOfService@
qualityOfServiceSelector :: Selector
qualityOfServiceSelector = mkSelector "qualityOfService"

-- | @Selector@ for @setQualityOfService:@
setQualityOfServiceSelector :: Selector
setQualityOfServiceSelector = mkSelector "setQualityOfService:"

-- | @Selector@ for @underlyingQueue@
underlyingQueueSelector :: Selector
underlyingQueueSelector = mkSelector "underlyingQueue"

-- | @Selector@ for @setUnderlyingQueue:@
setUnderlyingQueueSelector :: Selector
setUnderlyingQueueSelector = mkSelector "setUnderlyingQueue:"

-- | @Selector@ for @currentQueue@
currentQueueSelector :: Selector
currentQueueSelector = mkSelector "currentQueue"

-- | @Selector@ for @mainQueue@
mainQueueSelector :: Selector
mainQueueSelector = mkSelector "mainQueue"

-- | @Selector@ for @operations@
operationsSelector :: Selector
operationsSelector = mkSelector "operations"

-- | @Selector@ for @operationCount@
operationCountSelector :: Selector
operationCountSelector = mkSelector "operationCount"

