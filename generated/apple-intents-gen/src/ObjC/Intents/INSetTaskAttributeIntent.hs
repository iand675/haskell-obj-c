{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Generated bindings for @INSetTaskAttributeIntent@.
module ObjC.Intents.INSetTaskAttributeIntent
  ( INSetTaskAttributeIntent
  , IsINSetTaskAttributeIntent(..)
  , initWithTargetTask_taskTitle_status_priority_spatialEventTrigger_temporalEventTrigger
  , initWithTargetTask_status_spatialEventTrigger_temporalEventTrigger
  , targetTask
  , taskTitle
  , status
  , priority
  , spatialEventTrigger
  , temporalEventTrigger
  , initWithTargetTask_status_spatialEventTrigger_temporalEventTriggerSelector
  , initWithTargetTask_taskTitle_status_priority_spatialEventTrigger_temporalEventTriggerSelector
  , prioritySelector
  , spatialEventTriggerSelector
  , statusSelector
  , targetTaskSelector
  , taskTitleSelector
  , temporalEventTriggerSelector

  -- * Enum types
  , INTaskPriority(INTaskPriority)
  , pattern INTaskPriorityUnknown
  , pattern INTaskPriorityNotFlagged
  , pattern INTaskPriorityFlagged
  , INTaskStatus(INTaskStatus)
  , pattern INTaskStatusUnknown
  , pattern INTaskStatusNotCompleted
  , pattern INTaskStatusCompleted

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

-- | @- initWithTargetTask:taskTitle:status:priority:spatialEventTrigger:temporalEventTrigger:@
initWithTargetTask_taskTitle_status_priority_spatialEventTrigger_temporalEventTrigger :: (IsINSetTaskAttributeIntent inSetTaskAttributeIntent, IsINTask targetTask, IsINSpeakableString taskTitle, IsINSpatialEventTrigger spatialEventTrigger, IsINTemporalEventTrigger temporalEventTrigger) => inSetTaskAttributeIntent -> targetTask -> taskTitle -> INTaskStatus -> INTaskPriority -> spatialEventTrigger -> temporalEventTrigger -> IO (Id INSetTaskAttributeIntent)
initWithTargetTask_taskTitle_status_priority_spatialEventTrigger_temporalEventTrigger inSetTaskAttributeIntent targetTask taskTitle status priority spatialEventTrigger temporalEventTrigger =
  sendOwnedMessage inSetTaskAttributeIntent initWithTargetTask_taskTitle_status_priority_spatialEventTrigger_temporalEventTriggerSelector (toINTask targetTask) (toINSpeakableString taskTitle) status priority (toINSpatialEventTrigger spatialEventTrigger) (toINTemporalEventTrigger temporalEventTrigger)

-- | @- initWithTargetTask:status:spatialEventTrigger:temporalEventTrigger:@
initWithTargetTask_status_spatialEventTrigger_temporalEventTrigger :: (IsINSetTaskAttributeIntent inSetTaskAttributeIntent, IsINTask targetTask, IsINSpatialEventTrigger spatialEventTrigger, IsINTemporalEventTrigger temporalEventTrigger) => inSetTaskAttributeIntent -> targetTask -> INTaskStatus -> spatialEventTrigger -> temporalEventTrigger -> IO (Id INSetTaskAttributeIntent)
initWithTargetTask_status_spatialEventTrigger_temporalEventTrigger inSetTaskAttributeIntent targetTask status spatialEventTrigger temporalEventTrigger =
  sendOwnedMessage inSetTaskAttributeIntent initWithTargetTask_status_spatialEventTrigger_temporalEventTriggerSelector (toINTask targetTask) status (toINSpatialEventTrigger spatialEventTrigger) (toINTemporalEventTrigger temporalEventTrigger)

-- | @- targetTask@
targetTask :: IsINSetTaskAttributeIntent inSetTaskAttributeIntent => inSetTaskAttributeIntent -> IO (Id INTask)
targetTask inSetTaskAttributeIntent =
  sendMessage inSetTaskAttributeIntent targetTaskSelector

-- | @- taskTitle@
taskTitle :: IsINSetTaskAttributeIntent inSetTaskAttributeIntent => inSetTaskAttributeIntent -> IO (Id INSpeakableString)
taskTitle inSetTaskAttributeIntent =
  sendMessage inSetTaskAttributeIntent taskTitleSelector

-- | @- status@
status :: IsINSetTaskAttributeIntent inSetTaskAttributeIntent => inSetTaskAttributeIntent -> IO INTaskStatus
status inSetTaskAttributeIntent =
  sendMessage inSetTaskAttributeIntent statusSelector

-- | @- priority@
priority :: IsINSetTaskAttributeIntent inSetTaskAttributeIntent => inSetTaskAttributeIntent -> IO INTaskPriority
priority inSetTaskAttributeIntent =
  sendMessage inSetTaskAttributeIntent prioritySelector

-- | @- spatialEventTrigger@
spatialEventTrigger :: IsINSetTaskAttributeIntent inSetTaskAttributeIntent => inSetTaskAttributeIntent -> IO (Id INSpatialEventTrigger)
spatialEventTrigger inSetTaskAttributeIntent =
  sendMessage inSetTaskAttributeIntent spatialEventTriggerSelector

-- | @- temporalEventTrigger@
temporalEventTrigger :: IsINSetTaskAttributeIntent inSetTaskAttributeIntent => inSetTaskAttributeIntent -> IO (Id INTemporalEventTrigger)
temporalEventTrigger inSetTaskAttributeIntent =
  sendMessage inSetTaskAttributeIntent temporalEventTriggerSelector

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @initWithTargetTask:taskTitle:status:priority:spatialEventTrigger:temporalEventTrigger:@
initWithTargetTask_taskTitle_status_priority_spatialEventTrigger_temporalEventTriggerSelector :: Selector '[Id INTask, Id INSpeakableString, INTaskStatus, INTaskPriority, Id INSpatialEventTrigger, Id INTemporalEventTrigger] (Id INSetTaskAttributeIntent)
initWithTargetTask_taskTitle_status_priority_spatialEventTrigger_temporalEventTriggerSelector = mkSelector "initWithTargetTask:taskTitle:status:priority:spatialEventTrigger:temporalEventTrigger:"

-- | @Selector@ for @initWithTargetTask:status:spatialEventTrigger:temporalEventTrigger:@
initWithTargetTask_status_spatialEventTrigger_temporalEventTriggerSelector :: Selector '[Id INTask, INTaskStatus, Id INSpatialEventTrigger, Id INTemporalEventTrigger] (Id INSetTaskAttributeIntent)
initWithTargetTask_status_spatialEventTrigger_temporalEventTriggerSelector = mkSelector "initWithTargetTask:status:spatialEventTrigger:temporalEventTrigger:"

-- | @Selector@ for @targetTask@
targetTaskSelector :: Selector '[] (Id INTask)
targetTaskSelector = mkSelector "targetTask"

-- | @Selector@ for @taskTitle@
taskTitleSelector :: Selector '[] (Id INSpeakableString)
taskTitleSelector = mkSelector "taskTitle"

-- | @Selector@ for @status@
statusSelector :: Selector '[] INTaskStatus
statusSelector = mkSelector "status"

-- | @Selector@ for @priority@
prioritySelector :: Selector '[] INTaskPriority
prioritySelector = mkSelector "priority"

-- | @Selector@ for @spatialEventTrigger@
spatialEventTriggerSelector :: Selector '[] (Id INSpatialEventTrigger)
spatialEventTriggerSelector = mkSelector "spatialEventTrigger"

-- | @Selector@ for @temporalEventTrigger@
temporalEventTriggerSelector :: Selector '[] (Id INTemporalEventTrigger)
temporalEventTriggerSelector = mkSelector "temporalEventTrigger"

