{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Generated bindings for @INAddTasksIntent@.
module ObjC.Intents.INAddTasksIntent
  ( INAddTasksIntent
  , IsINAddTasksIntent(..)
  , initWithTargetTaskList_taskTitles_spatialEventTrigger_temporalEventTrigger_priority
  , initWithTargetTaskList_taskTitles_spatialEventTrigger_temporalEventTrigger
  , targetTaskList
  , taskTitles
  , spatialEventTrigger
  , temporalEventTrigger
  , priority
  , initWithTargetTaskList_taskTitles_spatialEventTrigger_temporalEventTrigger_prioritySelector
  , initWithTargetTaskList_taskTitles_spatialEventTrigger_temporalEventTriggerSelector
  , targetTaskListSelector
  , taskTitlesSelector
  , spatialEventTriggerSelector
  , temporalEventTriggerSelector
  , prioritySelector

  -- * Enum types
  , INTaskPriority(INTaskPriority)
  , pattern INTaskPriorityUnknown
  , pattern INTaskPriorityNotFlagged
  , pattern INTaskPriorityFlagged

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
import ObjC.Intents.Internal.Enums
import ObjC.Foundation.Internal.Classes

-- | @- initWithTargetTaskList:taskTitles:spatialEventTrigger:temporalEventTrigger:priority:@
initWithTargetTaskList_taskTitles_spatialEventTrigger_temporalEventTrigger_priority :: (IsINAddTasksIntent inAddTasksIntent, IsINTaskList targetTaskList, IsNSArray taskTitles, IsINSpatialEventTrigger spatialEventTrigger, IsINTemporalEventTrigger temporalEventTrigger) => inAddTasksIntent -> targetTaskList -> taskTitles -> spatialEventTrigger -> temporalEventTrigger -> INTaskPriority -> IO (Id INAddTasksIntent)
initWithTargetTaskList_taskTitles_spatialEventTrigger_temporalEventTrigger_priority inAddTasksIntent  targetTaskList taskTitles spatialEventTrigger temporalEventTrigger priority =
withObjCPtr targetTaskList $ \raw_targetTaskList ->
  withObjCPtr taskTitles $ \raw_taskTitles ->
    withObjCPtr spatialEventTrigger $ \raw_spatialEventTrigger ->
      withObjCPtr temporalEventTrigger $ \raw_temporalEventTrigger ->
          sendMsg inAddTasksIntent (mkSelector "initWithTargetTaskList:taskTitles:spatialEventTrigger:temporalEventTrigger:priority:") (retPtr retVoid) [argPtr (castPtr raw_targetTaskList :: Ptr ()), argPtr (castPtr raw_taskTitles :: Ptr ()), argPtr (castPtr raw_spatialEventTrigger :: Ptr ()), argPtr (castPtr raw_temporalEventTrigger :: Ptr ()), argCLong (coerce priority)] >>= ownedObject . castPtr

-- | @- initWithTargetTaskList:taskTitles:spatialEventTrigger:temporalEventTrigger:@
initWithTargetTaskList_taskTitles_spatialEventTrigger_temporalEventTrigger :: (IsINAddTasksIntent inAddTasksIntent, IsINTaskList targetTaskList, IsNSArray taskTitles, IsINSpatialEventTrigger spatialEventTrigger, IsINTemporalEventTrigger temporalEventTrigger) => inAddTasksIntent -> targetTaskList -> taskTitles -> spatialEventTrigger -> temporalEventTrigger -> IO (Id INAddTasksIntent)
initWithTargetTaskList_taskTitles_spatialEventTrigger_temporalEventTrigger inAddTasksIntent  targetTaskList taskTitles spatialEventTrigger temporalEventTrigger =
withObjCPtr targetTaskList $ \raw_targetTaskList ->
  withObjCPtr taskTitles $ \raw_taskTitles ->
    withObjCPtr spatialEventTrigger $ \raw_spatialEventTrigger ->
      withObjCPtr temporalEventTrigger $ \raw_temporalEventTrigger ->
          sendMsg inAddTasksIntent (mkSelector "initWithTargetTaskList:taskTitles:spatialEventTrigger:temporalEventTrigger:") (retPtr retVoid) [argPtr (castPtr raw_targetTaskList :: Ptr ()), argPtr (castPtr raw_taskTitles :: Ptr ()), argPtr (castPtr raw_spatialEventTrigger :: Ptr ()), argPtr (castPtr raw_temporalEventTrigger :: Ptr ())] >>= ownedObject . castPtr

-- | @- targetTaskList@
targetTaskList :: IsINAddTasksIntent inAddTasksIntent => inAddTasksIntent -> IO (Id INTaskList)
targetTaskList inAddTasksIntent  =
  sendMsg inAddTasksIntent (mkSelector "targetTaskList") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- taskTitles@
taskTitles :: IsINAddTasksIntent inAddTasksIntent => inAddTasksIntent -> IO (Id NSArray)
taskTitles inAddTasksIntent  =
  sendMsg inAddTasksIntent (mkSelector "taskTitles") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- spatialEventTrigger@
spatialEventTrigger :: IsINAddTasksIntent inAddTasksIntent => inAddTasksIntent -> IO (Id INSpatialEventTrigger)
spatialEventTrigger inAddTasksIntent  =
  sendMsg inAddTasksIntent (mkSelector "spatialEventTrigger") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- temporalEventTrigger@
temporalEventTrigger :: IsINAddTasksIntent inAddTasksIntent => inAddTasksIntent -> IO (Id INTemporalEventTrigger)
temporalEventTrigger inAddTasksIntent  =
  sendMsg inAddTasksIntent (mkSelector "temporalEventTrigger") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- priority@
priority :: IsINAddTasksIntent inAddTasksIntent => inAddTasksIntent -> IO INTaskPriority
priority inAddTasksIntent  =
  fmap (coerce :: CLong -> INTaskPriority) $ sendMsg inAddTasksIntent (mkSelector "priority") retCLong []

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @initWithTargetTaskList:taskTitles:spatialEventTrigger:temporalEventTrigger:priority:@
initWithTargetTaskList_taskTitles_spatialEventTrigger_temporalEventTrigger_prioritySelector :: Selector
initWithTargetTaskList_taskTitles_spatialEventTrigger_temporalEventTrigger_prioritySelector = mkSelector "initWithTargetTaskList:taskTitles:spatialEventTrigger:temporalEventTrigger:priority:"

-- | @Selector@ for @initWithTargetTaskList:taskTitles:spatialEventTrigger:temporalEventTrigger:@
initWithTargetTaskList_taskTitles_spatialEventTrigger_temporalEventTriggerSelector :: Selector
initWithTargetTaskList_taskTitles_spatialEventTrigger_temporalEventTriggerSelector = mkSelector "initWithTargetTaskList:taskTitles:spatialEventTrigger:temporalEventTrigger:"

-- | @Selector@ for @targetTaskList@
targetTaskListSelector :: Selector
targetTaskListSelector = mkSelector "targetTaskList"

-- | @Selector@ for @taskTitles@
taskTitlesSelector :: Selector
taskTitlesSelector = mkSelector "taskTitles"

-- | @Selector@ for @spatialEventTrigger@
spatialEventTriggerSelector :: Selector
spatialEventTriggerSelector = mkSelector "spatialEventTrigger"

-- | @Selector@ for @temporalEventTrigger@
temporalEventTriggerSelector :: Selector
temporalEventTriggerSelector = mkSelector "temporalEventTrigger"

-- | @Selector@ for @priority@
prioritySelector :: Selector
prioritySelector = mkSelector "priority"

