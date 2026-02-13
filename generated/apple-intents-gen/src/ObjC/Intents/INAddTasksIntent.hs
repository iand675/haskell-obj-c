{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE DataKinds #-}
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
  , initWithTargetTaskList_taskTitles_spatialEventTrigger_temporalEventTriggerSelector
  , initWithTargetTaskList_taskTitles_spatialEventTrigger_temporalEventTrigger_prioritySelector
  , prioritySelector
  , spatialEventTriggerSelector
  , targetTaskListSelector
  , taskTitlesSelector
  , temporalEventTriggerSelector

  -- * Enum types
  , INTaskPriority(INTaskPriority)
  , pattern INTaskPriorityUnknown
  , pattern INTaskPriorityNotFlagged
  , pattern INTaskPriorityFlagged

  ) where

import Foreign.Ptr (Ptr, FunPtr)
import Foreign.C.Types

import ObjC.Runtime.Types
import ObjC.Runtime.Message (sendMessage, sendOwnedMessage, sendClassMessage, sendOwnedClassMessage)
import ObjC.Runtime.Selector (mkSelector)
import ObjC.Runtime.Class (getRequiredClass)

import ObjC.Intents.Internal.Classes
import ObjC.Intents.Internal.Enums
import ObjC.Foundation.Internal.Classes

-- | @- initWithTargetTaskList:taskTitles:spatialEventTrigger:temporalEventTrigger:priority:@
initWithTargetTaskList_taskTitles_spatialEventTrigger_temporalEventTrigger_priority :: (IsINAddTasksIntent inAddTasksIntent, IsINTaskList targetTaskList, IsNSArray taskTitles, IsINSpatialEventTrigger spatialEventTrigger, IsINTemporalEventTrigger temporalEventTrigger) => inAddTasksIntent -> targetTaskList -> taskTitles -> spatialEventTrigger -> temporalEventTrigger -> INTaskPriority -> IO (Id INAddTasksIntent)
initWithTargetTaskList_taskTitles_spatialEventTrigger_temporalEventTrigger_priority inAddTasksIntent targetTaskList taskTitles spatialEventTrigger temporalEventTrigger priority =
  sendOwnedMessage inAddTasksIntent initWithTargetTaskList_taskTitles_spatialEventTrigger_temporalEventTrigger_prioritySelector (toINTaskList targetTaskList) (toNSArray taskTitles) (toINSpatialEventTrigger spatialEventTrigger) (toINTemporalEventTrigger temporalEventTrigger) priority

-- | @- initWithTargetTaskList:taskTitles:spatialEventTrigger:temporalEventTrigger:@
initWithTargetTaskList_taskTitles_spatialEventTrigger_temporalEventTrigger :: (IsINAddTasksIntent inAddTasksIntent, IsINTaskList targetTaskList, IsNSArray taskTitles, IsINSpatialEventTrigger spatialEventTrigger, IsINTemporalEventTrigger temporalEventTrigger) => inAddTasksIntent -> targetTaskList -> taskTitles -> spatialEventTrigger -> temporalEventTrigger -> IO (Id INAddTasksIntent)
initWithTargetTaskList_taskTitles_spatialEventTrigger_temporalEventTrigger inAddTasksIntent targetTaskList taskTitles spatialEventTrigger temporalEventTrigger =
  sendOwnedMessage inAddTasksIntent initWithTargetTaskList_taskTitles_spatialEventTrigger_temporalEventTriggerSelector (toINTaskList targetTaskList) (toNSArray taskTitles) (toINSpatialEventTrigger spatialEventTrigger) (toINTemporalEventTrigger temporalEventTrigger)

-- | @- targetTaskList@
targetTaskList :: IsINAddTasksIntent inAddTasksIntent => inAddTasksIntent -> IO (Id INTaskList)
targetTaskList inAddTasksIntent =
  sendMessage inAddTasksIntent targetTaskListSelector

-- | @- taskTitles@
taskTitles :: IsINAddTasksIntent inAddTasksIntent => inAddTasksIntent -> IO (Id NSArray)
taskTitles inAddTasksIntent =
  sendMessage inAddTasksIntent taskTitlesSelector

-- | @- spatialEventTrigger@
spatialEventTrigger :: IsINAddTasksIntent inAddTasksIntent => inAddTasksIntent -> IO (Id INSpatialEventTrigger)
spatialEventTrigger inAddTasksIntent =
  sendMessage inAddTasksIntent spatialEventTriggerSelector

-- | @- temporalEventTrigger@
temporalEventTrigger :: IsINAddTasksIntent inAddTasksIntent => inAddTasksIntent -> IO (Id INTemporalEventTrigger)
temporalEventTrigger inAddTasksIntent =
  sendMessage inAddTasksIntent temporalEventTriggerSelector

-- | @- priority@
priority :: IsINAddTasksIntent inAddTasksIntent => inAddTasksIntent -> IO INTaskPriority
priority inAddTasksIntent =
  sendMessage inAddTasksIntent prioritySelector

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @initWithTargetTaskList:taskTitles:spatialEventTrigger:temporalEventTrigger:priority:@
initWithTargetTaskList_taskTitles_spatialEventTrigger_temporalEventTrigger_prioritySelector :: Selector '[Id INTaskList, Id NSArray, Id INSpatialEventTrigger, Id INTemporalEventTrigger, INTaskPriority] (Id INAddTasksIntent)
initWithTargetTaskList_taskTitles_spatialEventTrigger_temporalEventTrigger_prioritySelector = mkSelector "initWithTargetTaskList:taskTitles:spatialEventTrigger:temporalEventTrigger:priority:"

-- | @Selector@ for @initWithTargetTaskList:taskTitles:spatialEventTrigger:temporalEventTrigger:@
initWithTargetTaskList_taskTitles_spatialEventTrigger_temporalEventTriggerSelector :: Selector '[Id INTaskList, Id NSArray, Id INSpatialEventTrigger, Id INTemporalEventTrigger] (Id INAddTasksIntent)
initWithTargetTaskList_taskTitles_spatialEventTrigger_temporalEventTriggerSelector = mkSelector "initWithTargetTaskList:taskTitles:spatialEventTrigger:temporalEventTrigger:"

-- | @Selector@ for @targetTaskList@
targetTaskListSelector :: Selector '[] (Id INTaskList)
targetTaskListSelector = mkSelector "targetTaskList"

-- | @Selector@ for @taskTitles@
taskTitlesSelector :: Selector '[] (Id NSArray)
taskTitlesSelector = mkSelector "taskTitles"

-- | @Selector@ for @spatialEventTrigger@
spatialEventTriggerSelector :: Selector '[] (Id INSpatialEventTrigger)
spatialEventTriggerSelector = mkSelector "spatialEventTrigger"

-- | @Selector@ for @temporalEventTrigger@
temporalEventTriggerSelector :: Selector '[] (Id INTemporalEventTrigger)
temporalEventTriggerSelector = mkSelector "temporalEventTrigger"

-- | @Selector@ for @priority@
prioritySelector :: Selector '[] INTaskPriority
prioritySelector = mkSelector "priority"

