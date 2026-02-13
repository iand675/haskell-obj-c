{-# LANGUAGE DataKinds #-}
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
  , completedDateSelector
  , dueDateSelector
  , isCompletedSelector
  , prioritySelector
  , setCompletedDateSelector
  , setDueDateSelector
  , setIsCompletedSelector
  , setPrioritySelector
  , taskSelector


  ) where

import Foreign.Ptr (Ptr, FunPtr)
import Foreign.C.Types

import ObjC.Runtime.Types
import ObjC.Runtime.Message (sendMessage, sendOwnedMessage, sendClassMessage, sendOwnedClassMessage)
import ObjC.Runtime.Selector (mkSelector)
import ObjC.Runtime.Class (getRequiredClass)

import ObjC.CalendarStore.Internal.Classes
import ObjC.Foundation.Internal.Classes

-- | @+ task@
task :: IO RawId
task  =
  do
    cls' <- getRequiredClass "CalTask"
    sendClassMessage cls' taskSelector

-- | @- dueDate@
dueDate :: IsCalTask calTask => calTask -> IO (Id NSDate)
dueDate calTask =
  sendMessage calTask dueDateSelector

-- | @- setDueDate:@
setDueDate :: (IsCalTask calTask, IsNSDate value) => calTask -> value -> IO ()
setDueDate calTask value =
  sendMessage calTask setDueDateSelector (toNSDate value)

-- | @- priority@
priority :: IsCalTask calTask => calTask -> IO CULong
priority calTask =
  sendMessage calTask prioritySelector

-- | @- setPriority:@
setPriority :: IsCalTask calTask => calTask -> CULong -> IO ()
setPriority calTask value =
  sendMessage calTask setPrioritySelector value

-- | @- isCompleted@
isCompleted :: IsCalTask calTask => calTask -> IO Bool
isCompleted calTask =
  sendMessage calTask isCompletedSelector

-- | @- setIsCompleted:@
setIsCompleted :: IsCalTask calTask => calTask -> Bool -> IO ()
setIsCompleted calTask value =
  sendMessage calTask setIsCompletedSelector value

-- | @- completedDate@
completedDate :: IsCalTask calTask => calTask -> IO (Id NSDate)
completedDate calTask =
  sendMessage calTask completedDateSelector

-- | @- setCompletedDate:@
setCompletedDate :: (IsCalTask calTask, IsNSDate value) => calTask -> value -> IO ()
setCompletedDate calTask value =
  sendMessage calTask setCompletedDateSelector (toNSDate value)

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @task@
taskSelector :: Selector '[] RawId
taskSelector = mkSelector "task"

-- | @Selector@ for @dueDate@
dueDateSelector :: Selector '[] (Id NSDate)
dueDateSelector = mkSelector "dueDate"

-- | @Selector@ for @setDueDate:@
setDueDateSelector :: Selector '[Id NSDate] ()
setDueDateSelector = mkSelector "setDueDate:"

-- | @Selector@ for @priority@
prioritySelector :: Selector '[] CULong
prioritySelector = mkSelector "priority"

-- | @Selector@ for @setPriority:@
setPrioritySelector :: Selector '[CULong] ()
setPrioritySelector = mkSelector "setPriority:"

-- | @Selector@ for @isCompleted@
isCompletedSelector :: Selector '[] Bool
isCompletedSelector = mkSelector "isCompleted"

-- | @Selector@ for @setIsCompleted:@
setIsCompletedSelector :: Selector '[Bool] ()
setIsCompletedSelector = mkSelector "setIsCompleted:"

-- | @Selector@ for @completedDate@
completedDateSelector :: Selector '[] (Id NSDate)
completedDateSelector = mkSelector "completedDate"

-- | @Selector@ for @setCompletedDate:@
setCompletedDateSelector :: Selector '[Id NSDate] ()
setCompletedDateSelector = mkSelector "setCompletedDate:"

