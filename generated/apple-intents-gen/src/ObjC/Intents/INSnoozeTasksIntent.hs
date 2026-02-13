{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Generated bindings for @INSnoozeTasksIntent@.
module ObjC.Intents.INSnoozeTasksIntent
  ( INSnoozeTasksIntent
  , IsINSnoozeTasksIntent(..)
  , initWithTasks_nextTriggerTime_all
  , tasks
  , nextTriggerTime
  , all_
  , allSelector
  , initWithTasks_nextTriggerTime_allSelector
  , nextTriggerTimeSelector
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

-- | @- initWithTasks:nextTriggerTime:all:@
initWithTasks_nextTriggerTime_all :: (IsINSnoozeTasksIntent inSnoozeTasksIntent, IsNSArray tasks, IsINDateComponentsRange nextTriggerTime, IsNSNumber all_) => inSnoozeTasksIntent -> tasks -> nextTriggerTime -> all_ -> IO (Id INSnoozeTasksIntent)
initWithTasks_nextTriggerTime_all inSnoozeTasksIntent tasks nextTriggerTime all_ =
  sendOwnedMessage inSnoozeTasksIntent initWithTasks_nextTriggerTime_allSelector (toNSArray tasks) (toINDateComponentsRange nextTriggerTime) (toNSNumber all_)

-- | @- tasks@
tasks :: IsINSnoozeTasksIntent inSnoozeTasksIntent => inSnoozeTasksIntent -> IO (Id NSArray)
tasks inSnoozeTasksIntent =
  sendMessage inSnoozeTasksIntent tasksSelector

-- | @- nextTriggerTime@
nextTriggerTime :: IsINSnoozeTasksIntent inSnoozeTasksIntent => inSnoozeTasksIntent -> IO (Id INDateComponentsRange)
nextTriggerTime inSnoozeTasksIntent =
  sendMessage inSnoozeTasksIntent nextTriggerTimeSelector

-- | @- all@
all_ :: IsINSnoozeTasksIntent inSnoozeTasksIntent => inSnoozeTasksIntent -> IO (Id NSNumber)
all_ inSnoozeTasksIntent =
  sendMessage inSnoozeTasksIntent allSelector

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @initWithTasks:nextTriggerTime:all:@
initWithTasks_nextTriggerTime_allSelector :: Selector '[Id NSArray, Id INDateComponentsRange, Id NSNumber] (Id INSnoozeTasksIntent)
initWithTasks_nextTriggerTime_allSelector = mkSelector "initWithTasks:nextTriggerTime:all:"

-- | @Selector@ for @tasks@
tasksSelector :: Selector '[] (Id NSArray)
tasksSelector = mkSelector "tasks"

-- | @Selector@ for @nextTriggerTime@
nextTriggerTimeSelector :: Selector '[] (Id INDateComponentsRange)
nextTriggerTimeSelector = mkSelector "nextTriggerTime"

-- | @Selector@ for @all@
allSelector :: Selector '[] (Id NSNumber)
allSelector = mkSelector "all"

