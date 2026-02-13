{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Generated bindings for @INDeleteTasksIntent@.
module ObjC.Intents.INDeleteTasksIntent
  ( INDeleteTasksIntent
  , IsINDeleteTasksIntent(..)
  , initWithTaskList_tasks_all
  , taskList
  , tasks
  , all_
  , allSelector
  , initWithTaskList_tasks_allSelector
  , taskListSelector
  , tasksSelector


  ) where

import Foreign.Ptr (Ptr, FunPtr)
import Foreign.C.Types

import ObjC.Runtime.Types
import ObjC.Runtime.Message (sendMessage, sendOwnedMessage, sendClassMessage, sendOwnedClassMessage)
import ObjC.Runtime.Selector (mkSelector)
import ObjC.Runtime.Class (getRequiredClass)

import ObjC.Intents.Internal.Classes
import ObjC.Foundation.Internal.Classes

-- | @- initWithTaskList:tasks:all:@
initWithTaskList_tasks_all :: (IsINDeleteTasksIntent inDeleteTasksIntent, IsINTaskList taskList, IsNSArray tasks, IsNSNumber all_) => inDeleteTasksIntent -> taskList -> tasks -> all_ -> IO (Id INDeleteTasksIntent)
initWithTaskList_tasks_all inDeleteTasksIntent taskList tasks all_ =
  sendOwnedMessage inDeleteTasksIntent initWithTaskList_tasks_allSelector (toINTaskList taskList) (toNSArray tasks) (toNSNumber all_)

-- | @- taskList@
taskList :: IsINDeleteTasksIntent inDeleteTasksIntent => inDeleteTasksIntent -> IO (Id INTaskList)
taskList inDeleteTasksIntent =
  sendMessage inDeleteTasksIntent taskListSelector

-- | @- tasks@
tasks :: IsINDeleteTasksIntent inDeleteTasksIntent => inDeleteTasksIntent -> IO (Id NSArray)
tasks inDeleteTasksIntent =
  sendMessage inDeleteTasksIntent tasksSelector

-- | @- all@
all_ :: IsINDeleteTasksIntent inDeleteTasksIntent => inDeleteTasksIntent -> IO (Id NSNumber)
all_ inDeleteTasksIntent =
  sendMessage inDeleteTasksIntent allSelector

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @initWithTaskList:tasks:all:@
initWithTaskList_tasks_allSelector :: Selector '[Id INTaskList, Id NSArray, Id NSNumber] (Id INDeleteTasksIntent)
initWithTaskList_tasks_allSelector = mkSelector "initWithTaskList:tasks:all:"

-- | @Selector@ for @taskList@
taskListSelector :: Selector '[] (Id INTaskList)
taskListSelector = mkSelector "taskList"

-- | @Selector@ for @tasks@
tasksSelector :: Selector '[] (Id NSArray)
tasksSelector = mkSelector "tasks"

-- | @Selector@ for @all@
allSelector :: Selector '[] (Id NSNumber)
allSelector = mkSelector "all"

