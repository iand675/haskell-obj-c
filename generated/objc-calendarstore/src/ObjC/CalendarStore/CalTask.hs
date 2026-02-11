{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Generated bindings for @CalTask@.
module ObjC.CalendarStore.CalTask
  ( CalTask
  , IsCalTask(..)
  , task
  , dueDate
  , setDueDate
  , priority
  , setPriority
  , isCompleted
  , setIsCompleted
  , completedDate
  , setCompletedDate
  , taskSelector
  , dueDateSelector
  , setDueDateSelector
  , prioritySelector
  , setPrioritySelector
  , isCompletedSelector
  , setIsCompletedSelector
  , completedDateSelector
  , setCompletedDateSelector


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

import ObjC.CalendarStore.Internal.Classes
import ObjC.Foundation.Internal.Classes

-- | @+ task@
task :: IO RawId
task  =
  do
    cls' <- getRequiredClass "CalTask"
    fmap (RawId . castPtr) $ sendClassMsg cls' (mkSelector "task") (retPtr retVoid) []

-- | @- dueDate@
dueDate :: IsCalTask calTask => calTask -> IO (Id NSDate)
dueDate calTask  =
  sendMsg calTask (mkSelector "dueDate") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- setDueDate:@
setDueDate :: (IsCalTask calTask, IsNSDate value) => calTask -> value -> IO ()
setDueDate calTask  value =
withObjCPtr value $ \raw_value ->
    sendMsg calTask (mkSelector "setDueDate:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- | @- priority@
priority :: IsCalTask calTask => calTask -> IO CULong
priority calTask  =
  sendMsg calTask (mkSelector "priority") retCULong []

-- | @- setPriority:@
setPriority :: IsCalTask calTask => calTask -> CULong -> IO ()
setPriority calTask  value =
  sendMsg calTask (mkSelector "setPriority:") retVoid [argCULong (fromIntegral value)]

-- | @- isCompleted@
isCompleted :: IsCalTask calTask => calTask -> IO Bool
isCompleted calTask  =
  fmap ((/= 0) :: CULong -> Bool) $ sendMsg calTask (mkSelector "isCompleted") retCULong []

-- | @- setIsCompleted:@
setIsCompleted :: IsCalTask calTask => calTask -> Bool -> IO ()
setIsCompleted calTask  value =
  sendMsg calTask (mkSelector "setIsCompleted:") retVoid [argCULong (if value then 1 else 0)]

-- | @- completedDate@
completedDate :: IsCalTask calTask => calTask -> IO (Id NSDate)
completedDate calTask  =
  sendMsg calTask (mkSelector "completedDate") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- setCompletedDate:@
setCompletedDate :: (IsCalTask calTask, IsNSDate value) => calTask -> value -> IO ()
setCompletedDate calTask  value =
withObjCPtr value $ \raw_value ->
    sendMsg calTask (mkSelector "setCompletedDate:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @task@
taskSelector :: Selector
taskSelector = mkSelector "task"

-- | @Selector@ for @dueDate@
dueDateSelector :: Selector
dueDateSelector = mkSelector "dueDate"

-- | @Selector@ for @setDueDate:@
setDueDateSelector :: Selector
setDueDateSelector = mkSelector "setDueDate:"

-- | @Selector@ for @priority@
prioritySelector :: Selector
prioritySelector = mkSelector "priority"

-- | @Selector@ for @setPriority:@
setPrioritySelector :: Selector
setPrioritySelector = mkSelector "setPriority:"

-- | @Selector@ for @isCompleted@
isCompletedSelector :: Selector
isCompletedSelector = mkSelector "isCompleted"

-- | @Selector@ for @setIsCompleted:@
setIsCompletedSelector :: Selector
setIsCompletedSelector = mkSelector "setIsCompleted:"

-- | @Selector@ for @completedDate@
completedDateSelector :: Selector
completedDateSelector = mkSelector "completedDate"

-- | @Selector@ for @setCompletedDate:@
setCompletedDateSelector :: Selector
setCompletedDateSelector = mkSelector "setCompletedDate:"

