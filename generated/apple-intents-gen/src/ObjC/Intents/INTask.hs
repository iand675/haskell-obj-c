{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Generated bindings for @INTask@.
module ObjC.Intents.INTask
  ( INTask
  , IsINTask(..)
  , initWithTitle_status_taskType_spatialEventTrigger_temporalEventTrigger_createdDateComponents_modifiedDateComponents_identifier_priority
  , initWithTitle_status_taskType_spatialEventTrigger_temporalEventTrigger_createdDateComponents_modifiedDateComponents_identifier
  , title
  , status
  , spatialEventTrigger
  , temporalEventTrigger
  , createdDateComponents
  , modifiedDateComponents
  , identifier
  , taskType
  , priority
  , createdDateComponentsSelector
  , identifierSelector
  , initWithTitle_status_taskType_spatialEventTrigger_temporalEventTrigger_createdDateComponents_modifiedDateComponents_identifierSelector
  , initWithTitle_status_taskType_spatialEventTrigger_temporalEventTrigger_createdDateComponents_modifiedDateComponents_identifier_prioritySelector
  , modifiedDateComponentsSelector
  , prioritySelector
  , spatialEventTriggerSelector
  , statusSelector
  , taskTypeSelector
  , temporalEventTriggerSelector
  , titleSelector

  -- * Enum types
  , INTaskPriority(INTaskPriority)
  , pattern INTaskPriorityUnknown
  , pattern INTaskPriorityNotFlagged
  , pattern INTaskPriorityFlagged
  , INTaskStatus(INTaskStatus)
  , pattern INTaskStatusUnknown
  , pattern INTaskStatusNotCompleted
  , pattern INTaskStatusCompleted
  , INTaskType(INTaskType)
  , pattern INTaskTypeUnknown
  , pattern INTaskTypeNotCompletable
  , pattern INTaskTypeCompletable

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

-- | @- initWithTitle:status:taskType:spatialEventTrigger:temporalEventTrigger:createdDateComponents:modifiedDateComponents:identifier:priority:@
initWithTitle_status_taskType_spatialEventTrigger_temporalEventTrigger_createdDateComponents_modifiedDateComponents_identifier_priority :: (IsINTask inTask, IsINSpeakableString title, IsINSpatialEventTrigger spatialEventTrigger, IsINTemporalEventTrigger temporalEventTrigger, IsNSDateComponents createdDateComponents, IsNSDateComponents modifiedDateComponents, IsNSString identifier) => inTask -> title -> INTaskStatus -> INTaskType -> spatialEventTrigger -> temporalEventTrigger -> createdDateComponents -> modifiedDateComponents -> identifier -> INTaskPriority -> IO (Id INTask)
initWithTitle_status_taskType_spatialEventTrigger_temporalEventTrigger_createdDateComponents_modifiedDateComponents_identifier_priority inTask title status taskType spatialEventTrigger temporalEventTrigger createdDateComponents modifiedDateComponents identifier priority =
  sendOwnedMessage inTask initWithTitle_status_taskType_spatialEventTrigger_temporalEventTrigger_createdDateComponents_modifiedDateComponents_identifier_prioritySelector (toINSpeakableString title) status taskType (toINSpatialEventTrigger spatialEventTrigger) (toINTemporalEventTrigger temporalEventTrigger) (toNSDateComponents createdDateComponents) (toNSDateComponents modifiedDateComponents) (toNSString identifier) priority

-- | @- initWithTitle:status:taskType:spatialEventTrigger:temporalEventTrigger:createdDateComponents:modifiedDateComponents:identifier:@
initWithTitle_status_taskType_spatialEventTrigger_temporalEventTrigger_createdDateComponents_modifiedDateComponents_identifier :: (IsINTask inTask, IsINSpeakableString title, IsINSpatialEventTrigger spatialEventTrigger, IsINTemporalEventTrigger temporalEventTrigger, IsNSDateComponents createdDateComponents, IsNSDateComponents modifiedDateComponents, IsNSString identifier) => inTask -> title -> INTaskStatus -> INTaskType -> spatialEventTrigger -> temporalEventTrigger -> createdDateComponents -> modifiedDateComponents -> identifier -> IO (Id INTask)
initWithTitle_status_taskType_spatialEventTrigger_temporalEventTrigger_createdDateComponents_modifiedDateComponents_identifier inTask title status taskType spatialEventTrigger temporalEventTrigger createdDateComponents modifiedDateComponents identifier =
  sendOwnedMessage inTask initWithTitle_status_taskType_spatialEventTrigger_temporalEventTrigger_createdDateComponents_modifiedDateComponents_identifierSelector (toINSpeakableString title) status taskType (toINSpatialEventTrigger spatialEventTrigger) (toINTemporalEventTrigger temporalEventTrigger) (toNSDateComponents createdDateComponents) (toNSDateComponents modifiedDateComponents) (toNSString identifier)

-- | @- title@
title :: IsINTask inTask => inTask -> IO (Id INSpeakableString)
title inTask =
  sendMessage inTask titleSelector

-- | @- status@
status :: IsINTask inTask => inTask -> IO INTaskStatus
status inTask =
  sendMessage inTask statusSelector

-- | @- spatialEventTrigger@
spatialEventTrigger :: IsINTask inTask => inTask -> IO (Id INSpatialEventTrigger)
spatialEventTrigger inTask =
  sendMessage inTask spatialEventTriggerSelector

-- | @- temporalEventTrigger@
temporalEventTrigger :: IsINTask inTask => inTask -> IO (Id INTemporalEventTrigger)
temporalEventTrigger inTask =
  sendMessage inTask temporalEventTriggerSelector

-- | @- createdDateComponents@
createdDateComponents :: IsINTask inTask => inTask -> IO (Id NSDateComponents)
createdDateComponents inTask =
  sendMessage inTask createdDateComponentsSelector

-- | @- modifiedDateComponents@
modifiedDateComponents :: IsINTask inTask => inTask -> IO (Id NSDateComponents)
modifiedDateComponents inTask =
  sendMessage inTask modifiedDateComponentsSelector

-- | @- identifier@
identifier :: IsINTask inTask => inTask -> IO (Id NSString)
identifier inTask =
  sendMessage inTask identifierSelector

-- | @- taskType@
taskType :: IsINTask inTask => inTask -> IO INTaskType
taskType inTask =
  sendMessage inTask taskTypeSelector

-- | @- priority@
priority :: IsINTask inTask => inTask -> IO INTaskPriority
priority inTask =
  sendMessage inTask prioritySelector

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @initWithTitle:status:taskType:spatialEventTrigger:temporalEventTrigger:createdDateComponents:modifiedDateComponents:identifier:priority:@
initWithTitle_status_taskType_spatialEventTrigger_temporalEventTrigger_createdDateComponents_modifiedDateComponents_identifier_prioritySelector :: Selector '[Id INSpeakableString, INTaskStatus, INTaskType, Id INSpatialEventTrigger, Id INTemporalEventTrigger, Id NSDateComponents, Id NSDateComponents, Id NSString, INTaskPriority] (Id INTask)
initWithTitle_status_taskType_spatialEventTrigger_temporalEventTrigger_createdDateComponents_modifiedDateComponents_identifier_prioritySelector = mkSelector "initWithTitle:status:taskType:spatialEventTrigger:temporalEventTrigger:createdDateComponents:modifiedDateComponents:identifier:priority:"

-- | @Selector@ for @initWithTitle:status:taskType:spatialEventTrigger:temporalEventTrigger:createdDateComponents:modifiedDateComponents:identifier:@
initWithTitle_status_taskType_spatialEventTrigger_temporalEventTrigger_createdDateComponents_modifiedDateComponents_identifierSelector :: Selector '[Id INSpeakableString, INTaskStatus, INTaskType, Id INSpatialEventTrigger, Id INTemporalEventTrigger, Id NSDateComponents, Id NSDateComponents, Id NSString] (Id INTask)
initWithTitle_status_taskType_spatialEventTrigger_temporalEventTrigger_createdDateComponents_modifiedDateComponents_identifierSelector = mkSelector "initWithTitle:status:taskType:spatialEventTrigger:temporalEventTrigger:createdDateComponents:modifiedDateComponents:identifier:"

-- | @Selector@ for @title@
titleSelector :: Selector '[] (Id INSpeakableString)
titleSelector = mkSelector "title"

-- | @Selector@ for @status@
statusSelector :: Selector '[] INTaskStatus
statusSelector = mkSelector "status"

-- | @Selector@ for @spatialEventTrigger@
spatialEventTriggerSelector :: Selector '[] (Id INSpatialEventTrigger)
spatialEventTriggerSelector = mkSelector "spatialEventTrigger"

-- | @Selector@ for @temporalEventTrigger@
temporalEventTriggerSelector :: Selector '[] (Id INTemporalEventTrigger)
temporalEventTriggerSelector = mkSelector "temporalEventTrigger"

-- | @Selector@ for @createdDateComponents@
createdDateComponentsSelector :: Selector '[] (Id NSDateComponents)
createdDateComponentsSelector = mkSelector "createdDateComponents"

-- | @Selector@ for @modifiedDateComponents@
modifiedDateComponentsSelector :: Selector '[] (Id NSDateComponents)
modifiedDateComponentsSelector = mkSelector "modifiedDateComponents"

-- | @Selector@ for @identifier@
identifierSelector :: Selector '[] (Id NSString)
identifierSelector = mkSelector "identifier"

-- | @Selector@ for @taskType@
taskTypeSelector :: Selector '[] INTaskType
taskTypeSelector = mkSelector "taskType"

-- | @Selector@ for @priority@
prioritySelector :: Selector '[] INTaskPriority
prioritySelector = mkSelector "priority"

