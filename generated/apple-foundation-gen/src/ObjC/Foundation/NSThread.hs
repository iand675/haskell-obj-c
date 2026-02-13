{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE DataKinds #-}
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
  , callStackReturnAddressesSelector
  , callStackSymbolsSelector
  , cancelSelector
  , cancelledSelector
  , currentThreadSelector
  , detachNewThreadSelector_toTarget_withObjectSelector
  , detachNewThreadWithBlockSelector
  , executingSelector
  , exitSelector
  , finishedSelector
  , initSelector
  , initWithBlockSelector
  , initWithTarget_selector_objectSelector
  , isMainThreadSelector
  , isMultiThreadedSelector
  , mainSelector
  , mainThreadSelector
  , nameSelector
  , nsThreadIsMainThreadSelector
  , nsThreadSetThreadPrioritySelector
  , nsThreadThreadPrioritySelector
  , qualityOfServiceSelector
  , setNameSelector
  , setQualityOfServiceSelector
  , setStackSizeSelector
  , setThreadPrioritySelector
  , sleepForTimeIntervalSelector
  , sleepUntilDateSelector
  , stackSizeSelector
  , startSelector
  , threadDictionarySelector
  , threadPrioritySelector

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

-- | @+ detachNewThreadWithBlock:@
detachNewThreadWithBlock :: Ptr () -> IO ()
detachNewThreadWithBlock block =
  do
    cls' <- getRequiredClass "NSThread"
    sendClassMessage cls' detachNewThreadWithBlockSelector block

-- | @+ detachNewThreadSelector:toTarget:withObject:@
detachNewThreadSelector_toTarget_withObject :: Sel -> RawId -> RawId -> IO ()
detachNewThreadSelector_toTarget_withObject selector target argument =
  do
    cls' <- getRequiredClass "NSThread"
    sendClassMessage cls' detachNewThreadSelector_toTarget_withObjectSelector selector target argument

-- | @+ isMultiThreaded@
isMultiThreaded :: IO Bool
isMultiThreaded  =
  do
    cls' <- getRequiredClass "NSThread"
    sendClassMessage cls' isMultiThreadedSelector

-- | @+ sleepUntilDate:@
sleepUntilDate :: IsNSDate date => date -> IO ()
sleepUntilDate date =
  do
    cls' <- getRequiredClass "NSThread"
    sendClassMessage cls' sleepUntilDateSelector (toNSDate date)

-- | @+ sleepForTimeInterval:@
sleepForTimeInterval :: CDouble -> IO ()
sleepForTimeInterval ti =
  do
    cls' <- getRequiredClass "NSThread"
    sendClassMessage cls' sleepForTimeIntervalSelector ti

-- | @+ exit@
exit :: IO ()
exit  =
  do
    cls' <- getRequiredClass "NSThread"
    sendClassMessage cls' exitSelector

-- | @+ threadPriority@
nsThreadThreadPriority :: IO CDouble
nsThreadThreadPriority  =
  do
    cls' <- getRequiredClass "NSThread"
    sendClassMessage cls' nsThreadThreadPrioritySelector

-- | @+ setThreadPriority:@
nsThreadSetThreadPriority :: CDouble -> IO Bool
nsThreadSetThreadPriority p =
  do
    cls' <- getRequiredClass "NSThread"
    sendClassMessage cls' nsThreadSetThreadPrioritySelector p

-- | @- init@
init_ :: IsNSThread nsThread => nsThread -> IO (Id NSThread)
init_ nsThread =
  sendOwnedMessage nsThread initSelector

-- | @- initWithTarget:selector:object:@
initWithTarget_selector_object :: IsNSThread nsThread => nsThread -> RawId -> Sel -> RawId -> IO (Id NSThread)
initWithTarget_selector_object nsThread target selector argument =
  sendOwnedMessage nsThread initWithTarget_selector_objectSelector target selector argument

-- | @- initWithBlock:@
initWithBlock :: IsNSThread nsThread => nsThread -> Ptr () -> IO (Id NSThread)
initWithBlock nsThread block =
  sendOwnedMessage nsThread initWithBlockSelector block

-- | @- cancel@
cancel :: IsNSThread nsThread => nsThread -> IO ()
cancel nsThread =
  sendMessage nsThread cancelSelector

-- | @- start@
start :: IsNSThread nsThread => nsThread -> IO ()
start nsThread =
  sendMessage nsThread startSelector

-- | @- main@
main :: IsNSThread nsThread => nsThread -> IO ()
main nsThread =
  sendMessage nsThread mainSelector

-- | @+ currentThread@
currentThread :: IO (Id NSThread)
currentThread  =
  do
    cls' <- getRequiredClass "NSThread"
    sendClassMessage cls' currentThreadSelector

-- | @- threadDictionary@
threadDictionary :: IsNSThread nsThread => nsThread -> IO (Id NSMutableDictionary)
threadDictionary nsThread =
  sendMessage nsThread threadDictionarySelector

-- | @- threadPriority@
threadPriority :: IsNSThread nsThread => nsThread -> IO CDouble
threadPriority nsThread =
  sendMessage nsThread threadPrioritySelector

-- | @- setThreadPriority:@
setThreadPriority :: IsNSThread nsThread => nsThread -> CDouble -> IO ()
setThreadPriority nsThread value =
  sendMessage nsThread setThreadPrioritySelector value

-- | @- qualityOfService@
qualityOfService :: IsNSThread nsThread => nsThread -> IO NSQualityOfService
qualityOfService nsThread =
  sendMessage nsThread qualityOfServiceSelector

-- | @- setQualityOfService:@
setQualityOfService :: IsNSThread nsThread => nsThread -> NSQualityOfService -> IO ()
setQualityOfService nsThread value =
  sendMessage nsThread setQualityOfServiceSelector value

-- | @+ callStackReturnAddresses@
callStackReturnAddresses :: IO (Id NSArray)
callStackReturnAddresses  =
  do
    cls' <- getRequiredClass "NSThread"
    sendClassMessage cls' callStackReturnAddressesSelector

-- | @+ callStackSymbols@
callStackSymbols :: IO (Id NSArray)
callStackSymbols  =
  do
    cls' <- getRequiredClass "NSThread"
    sendClassMessage cls' callStackSymbolsSelector

-- | @- name@
name :: IsNSThread nsThread => nsThread -> IO (Id NSString)
name nsThread =
  sendMessage nsThread nameSelector

-- | @- setName:@
setName :: (IsNSThread nsThread, IsNSString value) => nsThread -> value -> IO ()
setName nsThread value =
  sendMessage nsThread setNameSelector (toNSString value)

-- | @- stackSize@
stackSize :: IsNSThread nsThread => nsThread -> IO CULong
stackSize nsThread =
  sendMessage nsThread stackSizeSelector

-- | @- setStackSize:@
setStackSize :: IsNSThread nsThread => nsThread -> CULong -> IO ()
setStackSize nsThread value =
  sendMessage nsThread setStackSizeSelector value

-- | @- isMainThread@
isMainThread :: IsNSThread nsThread => nsThread -> IO Bool
isMainThread nsThread =
  sendMessage nsThread isMainThreadSelector

-- | @+ isMainThread@
nsThreadIsMainThread :: IO Bool
nsThreadIsMainThread  =
  do
    cls' <- getRequiredClass "NSThread"
    sendClassMessage cls' nsThreadIsMainThreadSelector

-- | @+ mainThread@
mainThread :: IO (Id NSThread)
mainThread  =
  do
    cls' <- getRequiredClass "NSThread"
    sendClassMessage cls' mainThreadSelector

-- | @- executing@
executing :: IsNSThread nsThread => nsThread -> IO Bool
executing nsThread =
  sendMessage nsThread executingSelector

-- | @- finished@
finished :: IsNSThread nsThread => nsThread -> IO Bool
finished nsThread =
  sendMessage nsThread finishedSelector

-- | @- cancelled@
cancelled :: IsNSThread nsThread => nsThread -> IO Bool
cancelled nsThread =
  sendMessage nsThread cancelledSelector

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @detachNewThreadWithBlock:@
detachNewThreadWithBlockSelector :: Selector '[Ptr ()] ()
detachNewThreadWithBlockSelector = mkSelector "detachNewThreadWithBlock:"

-- | @Selector@ for @detachNewThreadSelector:toTarget:withObject:@
detachNewThreadSelector_toTarget_withObjectSelector :: Selector '[Sel, RawId, RawId] ()
detachNewThreadSelector_toTarget_withObjectSelector = mkSelector "detachNewThreadSelector:toTarget:withObject:"

-- | @Selector@ for @isMultiThreaded@
isMultiThreadedSelector :: Selector '[] Bool
isMultiThreadedSelector = mkSelector "isMultiThreaded"

-- | @Selector@ for @sleepUntilDate:@
sleepUntilDateSelector :: Selector '[Id NSDate] ()
sleepUntilDateSelector = mkSelector "sleepUntilDate:"

-- | @Selector@ for @sleepForTimeInterval:@
sleepForTimeIntervalSelector :: Selector '[CDouble] ()
sleepForTimeIntervalSelector = mkSelector "sleepForTimeInterval:"

-- | @Selector@ for @exit@
exitSelector :: Selector '[] ()
exitSelector = mkSelector "exit"

-- | @Selector@ for @threadPriority@
nsThreadThreadPrioritySelector :: Selector '[] CDouble
nsThreadThreadPrioritySelector = mkSelector "threadPriority"

-- | @Selector@ for @setThreadPriority:@
nsThreadSetThreadPrioritySelector :: Selector '[CDouble] Bool
nsThreadSetThreadPrioritySelector = mkSelector "setThreadPriority:"

-- | @Selector@ for @init@
initSelector :: Selector '[] (Id NSThread)
initSelector = mkSelector "init"

-- | @Selector@ for @initWithTarget:selector:object:@
initWithTarget_selector_objectSelector :: Selector '[RawId, Sel, RawId] (Id NSThread)
initWithTarget_selector_objectSelector = mkSelector "initWithTarget:selector:object:"

-- | @Selector@ for @initWithBlock:@
initWithBlockSelector :: Selector '[Ptr ()] (Id NSThread)
initWithBlockSelector = mkSelector "initWithBlock:"

-- | @Selector@ for @cancel@
cancelSelector :: Selector '[] ()
cancelSelector = mkSelector "cancel"

-- | @Selector@ for @start@
startSelector :: Selector '[] ()
startSelector = mkSelector "start"

-- | @Selector@ for @main@
mainSelector :: Selector '[] ()
mainSelector = mkSelector "main"

-- | @Selector@ for @currentThread@
currentThreadSelector :: Selector '[] (Id NSThread)
currentThreadSelector = mkSelector "currentThread"

-- | @Selector@ for @threadDictionary@
threadDictionarySelector :: Selector '[] (Id NSMutableDictionary)
threadDictionarySelector = mkSelector "threadDictionary"

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

-- | @Selector@ for @callStackReturnAddresses@
callStackReturnAddressesSelector :: Selector '[] (Id NSArray)
callStackReturnAddressesSelector = mkSelector "callStackReturnAddresses"

-- | @Selector@ for @callStackSymbols@
callStackSymbolsSelector :: Selector '[] (Id NSArray)
callStackSymbolsSelector = mkSelector "callStackSymbols"

-- | @Selector@ for @name@
nameSelector :: Selector '[] (Id NSString)
nameSelector = mkSelector "name"

-- | @Selector@ for @setName:@
setNameSelector :: Selector '[Id NSString] ()
setNameSelector = mkSelector "setName:"

-- | @Selector@ for @stackSize@
stackSizeSelector :: Selector '[] CULong
stackSizeSelector = mkSelector "stackSize"

-- | @Selector@ for @setStackSize:@
setStackSizeSelector :: Selector '[CULong] ()
setStackSizeSelector = mkSelector "setStackSize:"

-- | @Selector@ for @isMainThread@
isMainThreadSelector :: Selector '[] Bool
isMainThreadSelector = mkSelector "isMainThread"

-- | @Selector@ for @isMainThread@
nsThreadIsMainThreadSelector :: Selector '[] Bool
nsThreadIsMainThreadSelector = mkSelector "isMainThread"

-- | @Selector@ for @mainThread@
mainThreadSelector :: Selector '[] (Id NSThread)
mainThreadSelector = mkSelector "mainThread"

-- | @Selector@ for @executing@
executingSelector :: Selector '[] Bool
executingSelector = mkSelector "executing"

-- | @Selector@ for @finished@
finishedSelector :: Selector '[] Bool
finishedSelector = mkSelector "finished"

-- | @Selector@ for @cancelled@
cancelledSelector :: Selector '[] Bool
cancelledSelector = mkSelector "cancelled"

