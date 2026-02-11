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
  , initWithTasks_nextTriggerTime_allSelector
  , tasksSelector
  , nextTriggerTimeSelector


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

-- | @- initWithTasks:nextTriggerTime:all:@
initWithTasks_nextTriggerTime_all :: (IsINSnoozeTasksIntent inSnoozeTasksIntent, IsNSArray tasks, IsINDateComponentsRange nextTriggerTime, IsNSNumber all_) => inSnoozeTasksIntent -> tasks -> nextTriggerTime -> all_ -> IO (Id INSnoozeTasksIntent)
initWithTasks_nextTriggerTime_all inSnoozeTasksIntent  tasks nextTriggerTime all_ =
withObjCPtr tasks $ \raw_tasks ->
  withObjCPtr nextTriggerTime $ \raw_nextTriggerTime ->
    withObjCPtr all_ $ \raw_all_ ->
        sendMsg inSnoozeTasksIntent (mkSelector "initWithTasks:nextTriggerTime:all:") (retPtr retVoid) [argPtr (castPtr raw_tasks :: Ptr ()), argPtr (castPtr raw_nextTriggerTime :: Ptr ()), argPtr (castPtr raw_all_ :: Ptr ())] >>= ownedObject . castPtr

-- | @- tasks@
tasks :: IsINSnoozeTasksIntent inSnoozeTasksIntent => inSnoozeTasksIntent -> IO (Id NSArray)
tasks inSnoozeTasksIntent  =
  sendMsg inSnoozeTasksIntent (mkSelector "tasks") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- nextTriggerTime@
nextTriggerTime :: IsINSnoozeTasksIntent inSnoozeTasksIntent => inSnoozeTasksIntent -> IO (Id INDateComponentsRange)
nextTriggerTime inSnoozeTasksIntent  =
  sendMsg inSnoozeTasksIntent (mkSelector "nextTriggerTime") (retPtr retVoid) [] >>= retainedObject . castPtr

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @initWithTasks:nextTriggerTime:all:@
initWithTasks_nextTriggerTime_allSelector :: Selector
initWithTasks_nextTriggerTime_allSelector = mkSelector "initWithTasks:nextTriggerTime:all:"

-- | @Selector@ for @tasks@
tasksSelector :: Selector
tasksSelector = mkSelector "tasks"

-- | @Selector@ for @nextTriggerTime@
nextTriggerTimeSelector :: Selector
nextTriggerTimeSelector = mkSelector "nextTriggerTime"

