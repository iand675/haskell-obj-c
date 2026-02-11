{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Generated bindings for @NSThread@.
module ObjC.Foundation.NSThread
  ( NSThread
  , IsNSThread(..)
  , detachNewThreadWithBlock
  , detachNewThreadSelector_toTarget_withObject
  , isMultiThreaded
  , sleepUntilDate
  , sleepForTimeInterval
  , exit
  , nsThreadThreadPriority
  , nsThreadSetThreadPriority
  , init_
  , initWithTarget_selector_object
  , initWithBlock
  , cancel
  , start
  , main
  , currentThread
  , threadDictionary
  , threadPriority
  , setThreadPriority
  , qualityOfService
  , setQualityOfService
  , callStackReturnAddresses
  , callStackSymbols
  , name
  , setName
  , stackSize
  , setStackSize
  , isMainThread
  , nsThreadIsMainThread
  , mainThread
  , executing
  , finished
  , cancelled
  , detachNewThreadWithBlockSelector
  , detachNewThreadSelector_toTarget_withObjectSelector
  , isMultiThreadedSelector
  , sleepUntilDateSelector
  , sleepForTimeIntervalSelector
  , exitSelector
  , threadPrioritySelector
  , setThreadPrioritySelector
  , initSelector
  , initWithTarget_selector_objectSelector
  , initWithBlockSelector
  , cancelSelector
  , startSelector
  , mainSelector
  , currentThreadSelector
  , threadDictionarySelector
  , qualityOfServiceSelector
  , setQualityOfServiceSelector
  , callStackReturnAddressesSelector
  , callStackSymbolsSelector
  , nameSelector
  , setNameSelector
  , stackSizeSelector
  , setStackSizeSelector
  , isMainThreadSelector
  , mainThreadSelector
  , executingSelector
  , finishedSelector
  , cancelledSelector

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

-- | @+ detachNewThreadWithBlock:@
detachNewThreadWithBlock :: Ptr () -> IO ()
detachNewThreadWithBlock block =
  do
    cls' <- getRequiredClass "NSThread"
    sendClassMsg cls' (mkSelector "detachNewThreadWithBlock:") retVoid [argPtr (castPtr block :: Ptr ())]

-- | @+ detachNewThreadSelector:toTarget:withObject:@
detachNewThreadSelector_toTarget_withObject :: Selector -> RawId -> RawId -> IO ()
detachNewThreadSelector_toTarget_withObject selector target argument =
  do
    cls' <- getRequiredClass "NSThread"
    sendClassMsg cls' (mkSelector "detachNewThreadSelector:toTarget:withObject:") retVoid [argPtr (unSelector selector), argPtr (castPtr (unRawId target) :: Ptr ()), argPtr (castPtr (unRawId argument) :: Ptr ())]

-- | @+ isMultiThreaded@
isMultiThreaded :: IO Bool
isMultiThreaded  =
  do
    cls' <- getRequiredClass "NSThread"
    fmap ((/= 0) :: CULong -> Bool) $ sendClassMsg cls' (mkSelector "isMultiThreaded") retCULong []

-- | @+ sleepUntilDate:@
sleepUntilDate :: IsNSDate date => date -> IO ()
sleepUntilDate date =
  do
    cls' <- getRequiredClass "NSThread"
    withObjCPtr date $ \raw_date ->
      sendClassMsg cls' (mkSelector "sleepUntilDate:") retVoid [argPtr (castPtr raw_date :: Ptr ())]

-- | @+ sleepForTimeInterval:@
sleepForTimeInterval :: CDouble -> IO ()
sleepForTimeInterval ti =
  do
    cls' <- getRequiredClass "NSThread"
    sendClassMsg cls' (mkSelector "sleepForTimeInterval:") retVoid [argCDouble ti]

-- | @+ exit@
exit :: IO ()
exit  =
  do
    cls' <- getRequiredClass "NSThread"
    sendClassMsg cls' (mkSelector "exit") retVoid []

-- | @+ threadPriority@
nsThreadThreadPriority :: IO CDouble
nsThreadThreadPriority  =
  do
    cls' <- getRequiredClass "NSThread"
    sendClassMsg cls' (mkSelector "threadPriority") retCDouble []

-- | @+ setThreadPriority:@
nsThreadSetThreadPriority :: CDouble -> IO Bool
nsThreadSetThreadPriority p =
  do
    cls' <- getRequiredClass "NSThread"
    fmap ((/= 0) :: CULong -> Bool) $ sendClassMsg cls' (mkSelector "setThreadPriority:") retCULong [argCDouble p]

-- | @- init@
init_ :: IsNSThread nsThread => nsThread -> IO (Id NSThread)
init_ nsThread  =
    sendMsg nsThread (mkSelector "init") (retPtr retVoid) [] >>= ownedObject . castPtr

-- | @- initWithTarget:selector:object:@
initWithTarget_selector_object :: IsNSThread nsThread => nsThread -> RawId -> Selector -> RawId -> IO (Id NSThread)
initWithTarget_selector_object nsThread  target selector argument =
    sendMsg nsThread (mkSelector "initWithTarget:selector:object:") (retPtr retVoid) [argPtr (castPtr (unRawId target) :: Ptr ()), argPtr (unSelector selector), argPtr (castPtr (unRawId argument) :: Ptr ())] >>= ownedObject . castPtr

-- | @- initWithBlock:@
initWithBlock :: IsNSThread nsThread => nsThread -> Ptr () -> IO (Id NSThread)
initWithBlock nsThread  block =
    sendMsg nsThread (mkSelector "initWithBlock:") (retPtr retVoid) [argPtr (castPtr block :: Ptr ())] >>= ownedObject . castPtr

-- | @- cancel@
cancel :: IsNSThread nsThread => nsThread -> IO ()
cancel nsThread  =
    sendMsg nsThread (mkSelector "cancel") retVoid []

-- | @- start@
start :: IsNSThread nsThread => nsThread -> IO ()
start nsThread  =
    sendMsg nsThread (mkSelector "start") retVoid []

-- | @- main@
main :: IsNSThread nsThread => nsThread -> IO ()
main nsThread  =
    sendMsg nsThread (mkSelector "main") retVoid []

-- | @+ currentThread@
currentThread :: IO (Id NSThread)
currentThread  =
  do
    cls' <- getRequiredClass "NSThread"
    sendClassMsg cls' (mkSelector "currentThread") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- threadDictionary@
threadDictionary :: IsNSThread nsThread => nsThread -> IO (Id NSMutableDictionary)
threadDictionary nsThread  =
    sendMsg nsThread (mkSelector "threadDictionary") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- threadPriority@
threadPriority :: IsNSThread nsThread => nsThread -> IO CDouble
threadPriority nsThread  =
    sendMsg nsThread (mkSelector "threadPriority") retCDouble []

-- | @- setThreadPriority:@
setThreadPriority :: IsNSThread nsThread => nsThread -> CDouble -> IO ()
setThreadPriority nsThread  value =
    sendMsg nsThread (mkSelector "setThreadPriority:") retVoid [argCDouble value]

-- | @- qualityOfService@
qualityOfService :: IsNSThread nsThread => nsThread -> IO NSQualityOfService
qualityOfService nsThread  =
    fmap (coerce :: CLong -> NSQualityOfService) $ sendMsg nsThread (mkSelector "qualityOfService") retCLong []

-- | @- setQualityOfService:@
setQualityOfService :: IsNSThread nsThread => nsThread -> NSQualityOfService -> IO ()
setQualityOfService nsThread  value =
    sendMsg nsThread (mkSelector "setQualityOfService:") retVoid [argCLong (coerce value)]

-- | @+ callStackReturnAddresses@
callStackReturnAddresses :: IO (Id NSArray)
callStackReturnAddresses  =
  do
    cls' <- getRequiredClass "NSThread"
    sendClassMsg cls' (mkSelector "callStackReturnAddresses") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @+ callStackSymbols@
callStackSymbols :: IO (Id NSArray)
callStackSymbols  =
  do
    cls' <- getRequiredClass "NSThread"
    sendClassMsg cls' (mkSelector "callStackSymbols") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- name@
name :: IsNSThread nsThread => nsThread -> IO (Id NSString)
name nsThread  =
    sendMsg nsThread (mkSelector "name") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- setName:@
setName :: (IsNSThread nsThread, IsNSString value) => nsThread -> value -> IO ()
setName nsThread  value =
  withObjCPtr value $ \raw_value ->
      sendMsg nsThread (mkSelector "setName:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- | @- stackSize@
stackSize :: IsNSThread nsThread => nsThread -> IO CULong
stackSize nsThread  =
    sendMsg nsThread (mkSelector "stackSize") retCULong []

-- | @- setStackSize:@
setStackSize :: IsNSThread nsThread => nsThread -> CULong -> IO ()
setStackSize nsThread  value =
    sendMsg nsThread (mkSelector "setStackSize:") retVoid [argCULong value]

-- | @- isMainThread@
isMainThread :: IsNSThread nsThread => nsThread -> IO Bool
isMainThread nsThread  =
    fmap ((/= 0) :: CULong -> Bool) $ sendMsg nsThread (mkSelector "isMainThread") retCULong []

-- | @+ isMainThread@
nsThreadIsMainThread :: IO Bool
nsThreadIsMainThread  =
  do
    cls' <- getRequiredClass "NSThread"
    fmap ((/= 0) :: CULong -> Bool) $ sendClassMsg cls' (mkSelector "isMainThread") retCULong []

-- | @+ mainThread@
mainThread :: IO (Id NSThread)
mainThread  =
  do
    cls' <- getRequiredClass "NSThread"
    sendClassMsg cls' (mkSelector "mainThread") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- executing@
executing :: IsNSThread nsThread => nsThread -> IO Bool
executing nsThread  =
    fmap ((/= 0) :: CULong -> Bool) $ sendMsg nsThread (mkSelector "executing") retCULong []

-- | @- finished@
finished :: IsNSThread nsThread => nsThread -> IO Bool
finished nsThread  =
    fmap ((/= 0) :: CULong -> Bool) $ sendMsg nsThread (mkSelector "finished") retCULong []

-- | @- cancelled@
cancelled :: IsNSThread nsThread => nsThread -> IO Bool
cancelled nsThread  =
    fmap ((/= 0) :: CULong -> Bool) $ sendMsg nsThread (mkSelector "cancelled") retCULong []

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @detachNewThreadWithBlock:@
detachNewThreadWithBlockSelector :: Selector
detachNewThreadWithBlockSelector = mkSelector "detachNewThreadWithBlock:"

-- | @Selector@ for @detachNewThreadSelector:toTarget:withObject:@
detachNewThreadSelector_toTarget_withObjectSelector :: Selector
detachNewThreadSelector_toTarget_withObjectSelector = mkSelector "detachNewThreadSelector:toTarget:withObject:"

-- | @Selector@ for @isMultiThreaded@
isMultiThreadedSelector :: Selector
isMultiThreadedSelector = mkSelector "isMultiThreaded"

-- | @Selector@ for @sleepUntilDate:@
sleepUntilDateSelector :: Selector
sleepUntilDateSelector = mkSelector "sleepUntilDate:"

-- | @Selector@ for @sleepForTimeInterval:@
sleepForTimeIntervalSelector :: Selector
sleepForTimeIntervalSelector = mkSelector "sleepForTimeInterval:"

-- | @Selector@ for @exit@
exitSelector :: Selector
exitSelector = mkSelector "exit"

-- | @Selector@ for @threadPriority@
threadPrioritySelector :: Selector
threadPrioritySelector = mkSelector "threadPriority"

-- | @Selector@ for @setThreadPriority:@
setThreadPrioritySelector :: Selector
setThreadPrioritySelector = mkSelector "setThreadPriority:"

-- | @Selector@ for @init@
initSelector :: Selector
initSelector = mkSelector "init"

-- | @Selector@ for @initWithTarget:selector:object:@
initWithTarget_selector_objectSelector :: Selector
initWithTarget_selector_objectSelector = mkSelector "initWithTarget:selector:object:"

-- | @Selector@ for @initWithBlock:@
initWithBlockSelector :: Selector
initWithBlockSelector = mkSelector "initWithBlock:"

-- | @Selector@ for @cancel@
cancelSelector :: Selector
cancelSelector = mkSelector "cancel"

-- | @Selector@ for @start@
startSelector :: Selector
startSelector = mkSelector "start"

-- | @Selector@ for @main@
mainSelector :: Selector
mainSelector = mkSelector "main"

-- | @Selector@ for @currentThread@
currentThreadSelector :: Selector
currentThreadSelector = mkSelector "currentThread"

-- | @Selector@ for @threadDictionary@
threadDictionarySelector :: Selector
threadDictionarySelector = mkSelector "threadDictionary"

-- | @Selector@ for @qualityOfService@
qualityOfServiceSelector :: Selector
qualityOfServiceSelector = mkSelector "qualityOfService"

-- | @Selector@ for @setQualityOfService:@
setQualityOfServiceSelector :: Selector
setQualityOfServiceSelector = mkSelector "setQualityOfService:"

-- | @Selector@ for @callStackReturnAddresses@
callStackReturnAddressesSelector :: Selector
callStackReturnAddressesSelector = mkSelector "callStackReturnAddresses"

-- | @Selector@ for @callStackSymbols@
callStackSymbolsSelector :: Selector
callStackSymbolsSelector = mkSelector "callStackSymbols"

-- | @Selector@ for @name@
nameSelector :: Selector
nameSelector = mkSelector "name"

-- | @Selector@ for @setName:@
setNameSelector :: Selector
setNameSelector = mkSelector "setName:"

-- | @Selector@ for @stackSize@
stackSizeSelector :: Selector
stackSizeSelector = mkSelector "stackSize"

-- | @Selector@ for @setStackSize:@
setStackSizeSelector :: Selector
setStackSizeSelector = mkSelector "setStackSize:"

-- | @Selector@ for @isMainThread@
isMainThreadSelector :: Selector
isMainThreadSelector = mkSelector "isMainThread"

-- | @Selector@ for @mainThread@
mainThreadSelector :: Selector
mainThreadSelector = mkSelector "mainThread"

-- | @Selector@ for @executing@
executingSelector :: Selector
executingSelector = mkSelector "executing"

-- | @Selector@ for @finished@
finishedSelector :: Selector
finishedSelector = mkSelector "finished"

-- | @Selector@ for @cancelled@
cancelledSelector :: Selector
cancelledSelector = mkSelector "cancelled"

