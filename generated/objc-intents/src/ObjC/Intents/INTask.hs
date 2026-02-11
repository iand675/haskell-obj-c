{-# LANGUAGE PatternSynonyms #-}
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
  , initWithTitle_status_taskType_spatialEventTrigger_temporalEventTrigger_createdDateComponents_modifiedDateComponents_identifier_prioritySelector
  , initWithTitle_status_taskType_spatialEventTrigger_temporalEventTrigger_createdDateComponents_modifiedDateComponents_identifierSelector
  , titleSelector
  , statusSelector
  , spatialEventTriggerSelector
  , temporalEventTriggerSelector
  , createdDateComponentsSelector
  , modifiedDateComponentsSelector
  , identifierSelector
  , taskTypeSelector
  , prioritySelector

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

-- | @- initWithTitle:status:taskType:spatialEventTrigger:temporalEventTrigger:createdDateComponents:modifiedDateComponents:identifier:priority:@
initWithTitle_status_taskType_spatialEventTrigger_temporalEventTrigger_createdDateComponents_modifiedDateComponents_identifier_priority :: (IsINTask inTask, IsINSpeakableString title, IsINSpatialEventTrigger spatialEventTrigger, IsINTemporalEventTrigger temporalEventTrigger, IsNSDateComponents createdDateComponents, IsNSDateComponents modifiedDateComponents, IsNSString identifier) => inTask -> title -> INTaskStatus -> INTaskType -> spatialEventTrigger -> temporalEventTrigger -> createdDateComponents -> modifiedDateComponents -> identifier -> INTaskPriority -> IO (Id INTask)
initWithTitle_status_taskType_spatialEventTrigger_temporalEventTrigger_createdDateComponents_modifiedDateComponents_identifier_priority inTask  title status taskType spatialEventTrigger temporalEventTrigger createdDateComponents modifiedDateComponents identifier priority =
withObjCPtr title $ \raw_title ->
  withObjCPtr spatialEventTrigger $ \raw_spatialEventTrigger ->
    withObjCPtr temporalEventTrigger $ \raw_temporalEventTrigger ->
      withObjCPtr createdDateComponents $ \raw_createdDateComponents ->
        withObjCPtr modifiedDateComponents $ \raw_modifiedDateComponents ->
          withObjCPtr identifier $ \raw_identifier ->
              sendMsg inTask (mkSelector "initWithTitle:status:taskType:spatialEventTrigger:temporalEventTrigger:createdDateComponents:modifiedDateComponents:identifier:priority:") (retPtr retVoid) [argPtr (castPtr raw_title :: Ptr ()), argCLong (coerce status), argCLong (coerce taskType), argPtr (castPtr raw_spatialEventTrigger :: Ptr ()), argPtr (castPtr raw_temporalEventTrigger :: Ptr ()), argPtr (castPtr raw_createdDateComponents :: Ptr ()), argPtr (castPtr raw_modifiedDateComponents :: Ptr ()), argPtr (castPtr raw_identifier :: Ptr ()), argCLong (coerce priority)] >>= ownedObject . castPtr

-- | @- initWithTitle:status:taskType:spatialEventTrigger:temporalEventTrigger:createdDateComponents:modifiedDateComponents:identifier:@
initWithTitle_status_taskType_spatialEventTrigger_temporalEventTrigger_createdDateComponents_modifiedDateComponents_identifier :: (IsINTask inTask, IsINSpeakableString title, IsINSpatialEventTrigger spatialEventTrigger, IsINTemporalEventTrigger temporalEventTrigger, IsNSDateComponents createdDateComponents, IsNSDateComponents modifiedDateComponents, IsNSString identifier) => inTask -> title -> INTaskStatus -> INTaskType -> spatialEventTrigger -> temporalEventTrigger -> createdDateComponents -> modifiedDateComponents -> identifier -> IO (Id INTask)
initWithTitle_status_taskType_spatialEventTrigger_temporalEventTrigger_createdDateComponents_modifiedDateComponents_identifier inTask  title status taskType spatialEventTrigger temporalEventTrigger createdDateComponents modifiedDateComponents identifier =
withObjCPtr title $ \raw_title ->
  withObjCPtr spatialEventTrigger $ \raw_spatialEventTrigger ->
    withObjCPtr temporalEventTrigger $ \raw_temporalEventTrigger ->
      withObjCPtr createdDateComponents $ \raw_createdDateComponents ->
        withObjCPtr modifiedDateComponents $ \raw_modifiedDateComponents ->
          withObjCPtr identifier $ \raw_identifier ->
              sendMsg inTask (mkSelector "initWithTitle:status:taskType:spatialEventTrigger:temporalEventTrigger:createdDateComponents:modifiedDateComponents:identifier:") (retPtr retVoid) [argPtr (castPtr raw_title :: Ptr ()), argCLong (coerce status), argCLong (coerce taskType), argPtr (castPtr raw_spatialEventTrigger :: Ptr ()), argPtr (castPtr raw_temporalEventTrigger :: Ptr ()), argPtr (castPtr raw_createdDateComponents :: Ptr ()), argPtr (castPtr raw_modifiedDateComponents :: Ptr ()), argPtr (castPtr raw_identifier :: Ptr ())] >>= ownedObject . castPtr

-- | @- title@
title :: IsINTask inTask => inTask -> IO (Id INSpeakableString)
title inTask  =
  sendMsg inTask (mkSelector "title") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- status@
status :: IsINTask inTask => inTask -> IO INTaskStatus
status inTask  =
  fmap (coerce :: CLong -> INTaskStatus) $ sendMsg inTask (mkSelector "status") retCLong []

-- | @- spatialEventTrigger@
spatialEventTrigger :: IsINTask inTask => inTask -> IO (Id INSpatialEventTrigger)
spatialEventTrigger inTask  =
  sendMsg inTask (mkSelector "spatialEventTrigger") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- temporalEventTrigger@
temporalEventTrigger :: IsINTask inTask => inTask -> IO (Id INTemporalEventTrigger)
temporalEventTrigger inTask  =
  sendMsg inTask (mkSelector "temporalEventTrigger") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- createdDateComponents@
createdDateComponents :: IsINTask inTask => inTask -> IO (Id NSDateComponents)
createdDateComponents inTask  =
  sendMsg inTask (mkSelector "createdDateComponents") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- modifiedDateComponents@
modifiedDateComponents :: IsINTask inTask => inTask -> IO (Id NSDateComponents)
modifiedDateComponents inTask  =
  sendMsg inTask (mkSelector "modifiedDateComponents") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- identifier@
identifier :: IsINTask inTask => inTask -> IO (Id NSString)
identifier inTask  =
  sendMsg inTask (mkSelector "identifier") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- taskType@
taskType :: IsINTask inTask => inTask -> IO INTaskType
taskType inTask  =
  fmap (coerce :: CLong -> INTaskType) $ sendMsg inTask (mkSelector "taskType") retCLong []

-- | @- priority@
priority :: IsINTask inTask => inTask -> IO INTaskPriority
priority inTask  =
  fmap (coerce :: CLong -> INTaskPriority) $ sendMsg inTask (mkSelector "priority") retCLong []

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @initWithTitle:status:taskType:spatialEventTrigger:temporalEventTrigger:createdDateComponents:modifiedDateComponents:identifier:priority:@
initWithTitle_status_taskType_spatialEventTrigger_temporalEventTrigger_createdDateComponents_modifiedDateComponents_identifier_prioritySelector :: Selector
initWithTitle_status_taskType_spatialEventTrigger_temporalEventTrigger_createdDateComponents_modifiedDateComponents_identifier_prioritySelector = mkSelector "initWithTitle:status:taskType:spatialEventTrigger:temporalEventTrigger:createdDateComponents:modifiedDateComponents:identifier:priority:"

-- | @Selector@ for @initWithTitle:status:taskType:spatialEventTrigger:temporalEventTrigger:createdDateComponents:modifiedDateComponents:identifier:@
initWithTitle_status_taskType_spatialEventTrigger_temporalEventTrigger_createdDateComponents_modifiedDateComponents_identifierSelector :: Selector
initWithTitle_status_taskType_spatialEventTrigger_temporalEventTrigger_createdDateComponents_modifiedDateComponents_identifierSelector = mkSelector "initWithTitle:status:taskType:spatialEventTrigger:temporalEventTrigger:createdDateComponents:modifiedDateComponents:identifier:"

-- | @Selector@ for @title@
titleSelector :: Selector
titleSelector = mkSelector "title"

-- | @Selector@ for @status@
statusSelector :: Selector
statusSelector = mkSelector "status"

-- | @Selector@ for @spatialEventTrigger@
spatialEventTriggerSelector :: Selector
spatialEventTriggerSelector = mkSelector "spatialEventTrigger"

-- | @Selector@ for @temporalEventTrigger@
temporalEventTriggerSelector :: Selector
temporalEventTriggerSelector = mkSelector "temporalEventTrigger"

-- | @Selector@ for @createdDateComponents@
createdDateComponentsSelector :: Selector
createdDateComponentsSelector = mkSelector "createdDateComponents"

-- | @Selector@ for @modifiedDateComponents@
modifiedDateComponentsSelector :: Selector
modifiedDateComponentsSelector = mkSelector "modifiedDateComponents"

-- | @Selector@ for @identifier@
identifierSelector :: Selector
identifierSelector = mkSelector "identifier"

-- | @Selector@ for @taskType@
taskTypeSelector :: Selector
taskTypeSelector = mkSelector "taskType"

-- | @Selector@ for @priority@
prioritySelector :: Selector
prioritySelector = mkSelector "priority"

