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
  , initWithTaskList_tasks_allSelector
  , taskListSelector
  , tasksSelector
  , allSelector


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

import ObjC.Intents.Internal.Classes
import ObjC.Foundation.Internal.Classes

-- | @- initWithTaskList:tasks:all:@
initWithTaskList_tasks_all :: (IsINDeleteTasksIntent inDeleteTasksIntent, IsINTaskList taskList, IsNSArray tasks, IsNSNumber all_) => inDeleteTasksIntent -> taskList -> tasks -> all_ -> IO (Id INDeleteTasksIntent)
initWithTaskList_tasks_all inDeleteTasksIntent  taskList tasks all_ =
  withObjCPtr taskList $ \raw_taskList ->
    withObjCPtr tasks $ \raw_tasks ->
      withObjCPtr all_ $ \raw_all_ ->
          sendMsg inDeleteTasksIntent (mkSelector "initWithTaskList:tasks:all:") (retPtr retVoid) [argPtr (castPtr raw_taskList :: Ptr ()), argPtr (castPtr raw_tasks :: Ptr ()), argPtr (castPtr raw_all_ :: Ptr ())] >>= ownedObject . castPtr

-- | @- taskList@
taskList :: IsINDeleteTasksIntent inDeleteTasksIntent => inDeleteTasksIntent -> IO (Id INTaskList)
taskList inDeleteTasksIntent  =
    sendMsg inDeleteTasksIntent (mkSelector "taskList") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- tasks@
tasks :: IsINDeleteTasksIntent inDeleteTasksIntent => inDeleteTasksIntent -> IO (Id NSArray)
tasks inDeleteTasksIntent  =
    sendMsg inDeleteTasksIntent (mkSelector "tasks") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- all@
all_ :: IsINDeleteTasksIntent inDeleteTasksIntent => inDeleteTasksIntent -> IO (Id NSNumber)
all_ inDeleteTasksIntent  =
    sendMsg inDeleteTasksIntent (mkSelector "all") (retPtr retVoid) [] >>= retainedObject . castPtr

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @initWithTaskList:tasks:all:@
initWithTaskList_tasks_allSelector :: Selector
initWithTaskList_tasks_allSelector = mkSelector "initWithTaskList:tasks:all:"

-- | @Selector@ for @taskList@
taskListSelector :: Selector
taskListSelector = mkSelector "taskList"

-- | @Selector@ for @tasks@
tasksSelector :: Selector
tasksSelector = mkSelector "tasks"

-- | @Selector@ for @all@
allSelector :: Selector
allSelector = mkSelector "all"

