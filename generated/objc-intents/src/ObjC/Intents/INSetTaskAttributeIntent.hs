{-# LANGUAGE PatternSynonyms #-}
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
  , initWithTargetTask_taskTitle_status_priority_spatialEventTrigger_temporalEventTriggerSelector
  , initWithTargetTask_status_spatialEventTrigger_temporalEventTriggerSelector
  , targetTaskSelector
  , taskTitleSelector
  , statusSelector
  , prioritySelector
  , spatialEventTriggerSelector
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

-- | @- initWithTargetTask:taskTitle:status:priority:spatialEventTrigger:temporalEventTrigger:@
initWithTargetTask_taskTitle_status_priority_spatialEventTrigger_temporalEventTrigger :: (IsINSetTaskAttributeIntent inSetTaskAttributeIntent, IsINTask targetTask, IsINSpeakableString taskTitle, IsINSpatialEventTrigger spatialEventTrigger, IsINTemporalEventTrigger temporalEventTrigger) => inSetTaskAttributeIntent -> targetTask -> taskTitle -> INTaskStatus -> INTaskPriority -> spatialEventTrigger -> temporalEventTrigger -> IO (Id INSetTaskAttributeIntent)
initWithTargetTask_taskTitle_status_priority_spatialEventTrigger_temporalEventTrigger inSetTaskAttributeIntent  targetTask taskTitle status priority spatialEventTrigger temporalEventTrigger =
withObjCPtr targetTask $ \raw_targetTask ->
  withObjCPtr taskTitle $ \raw_taskTitle ->
    withObjCPtr spatialEventTrigger $ \raw_spatialEventTrigger ->
      withObjCPtr temporalEventTrigger $ \raw_temporalEventTrigger ->
          sendMsg inSetTaskAttributeIntent (mkSelector "initWithTargetTask:taskTitle:status:priority:spatialEventTrigger:temporalEventTrigger:") (retPtr retVoid) [argPtr (castPtr raw_targetTask :: Ptr ()), argPtr (castPtr raw_taskTitle :: Ptr ()), argCLong (coerce status), argCLong (coerce priority), argPtr (castPtr raw_spatialEventTrigger :: Ptr ()), argPtr (castPtr raw_temporalEventTrigger :: Ptr ())] >>= ownedObject . castPtr

-- | @- initWithTargetTask:status:spatialEventTrigger:temporalEventTrigger:@
initWithTargetTask_status_spatialEventTrigger_temporalEventTrigger :: (IsINSetTaskAttributeIntent inSetTaskAttributeIntent, IsINTask targetTask, IsINSpatialEventTrigger spatialEventTrigger, IsINTemporalEventTrigger temporalEventTrigger) => inSetTaskAttributeIntent -> targetTask -> INTaskStatus -> spatialEventTrigger -> temporalEventTrigger -> IO (Id INSetTaskAttributeIntent)
initWithTargetTask_status_spatialEventTrigger_temporalEventTrigger inSetTaskAttributeIntent  targetTask status spatialEventTrigger temporalEventTrigger =
withObjCPtr targetTask $ \raw_targetTask ->
  withObjCPtr spatialEventTrigger $ \raw_spatialEventTrigger ->
    withObjCPtr temporalEventTrigger $ \raw_temporalEventTrigger ->
        sendMsg inSetTaskAttributeIntent (mkSelector "initWithTargetTask:status:spatialEventTrigger:temporalEventTrigger:") (retPtr retVoid) [argPtr (castPtr raw_targetTask :: Ptr ()), argCLong (coerce status), argPtr (castPtr raw_spatialEventTrigger :: Ptr ()), argPtr (castPtr raw_temporalEventTrigger :: Ptr ())] >>= ownedObject . castPtr

-- | @- targetTask@
targetTask :: IsINSetTaskAttributeIntent inSetTaskAttributeIntent => inSetTaskAttributeIntent -> IO (Id INTask)
targetTask inSetTaskAttributeIntent  =
  sendMsg inSetTaskAttributeIntent (mkSelector "targetTask") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- taskTitle@
taskTitle :: IsINSetTaskAttributeIntent inSetTaskAttributeIntent => inSetTaskAttributeIntent -> IO (Id INSpeakableString)
taskTitle inSetTaskAttributeIntent  =
  sendMsg inSetTaskAttributeIntent (mkSelector "taskTitle") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- status@
status :: IsINSetTaskAttributeIntent inSetTaskAttributeIntent => inSetTaskAttributeIntent -> IO INTaskStatus
status inSetTaskAttributeIntent  =
  fmap (coerce :: CLong -> INTaskStatus) $ sendMsg inSetTaskAttributeIntent (mkSelector "status") retCLong []

-- | @- priority@
priority :: IsINSetTaskAttributeIntent inSetTaskAttributeIntent => inSetTaskAttributeIntent -> IO INTaskPriority
priority inSetTaskAttributeIntent  =
  fmap (coerce :: CLong -> INTaskPriority) $ sendMsg inSetTaskAttributeIntent (mkSelector "priority") retCLong []

-- | @- spatialEventTrigger@
spatialEventTrigger :: IsINSetTaskAttributeIntent inSetTaskAttributeIntent => inSetTaskAttributeIntent -> IO (Id INSpatialEventTrigger)
spatialEventTrigger inSetTaskAttributeIntent  =
  sendMsg inSetTaskAttributeIntent (mkSelector "spatialEventTrigger") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- temporalEventTrigger@
temporalEventTrigger :: IsINSetTaskAttributeIntent inSetTaskAttributeIntent => inSetTaskAttributeIntent -> IO (Id INTemporalEventTrigger)
temporalEventTrigger inSetTaskAttributeIntent  =
  sendMsg inSetTaskAttributeIntent (mkSelector "temporalEventTrigger") (retPtr retVoid) [] >>= retainedObject . castPtr

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @initWithTargetTask:taskTitle:status:priority:spatialEventTrigger:temporalEventTrigger:@
initWithTargetTask_taskTitle_status_priority_spatialEventTrigger_temporalEventTriggerSelector :: Selector
initWithTargetTask_taskTitle_status_priority_spatialEventTrigger_temporalEventTriggerSelector = mkSelector "initWithTargetTask:taskTitle:status:priority:spatialEventTrigger:temporalEventTrigger:"

-- | @Selector@ for @initWithTargetTask:status:spatialEventTrigger:temporalEventTrigger:@
initWithTargetTask_status_spatialEventTrigger_temporalEventTriggerSelector :: Selector
initWithTargetTask_status_spatialEventTrigger_temporalEventTriggerSelector = mkSelector "initWithTargetTask:status:spatialEventTrigger:temporalEventTrigger:"

-- | @Selector@ for @targetTask@
targetTaskSelector :: Selector
targetTaskSelector = mkSelector "targetTask"

-- | @Selector@ for @taskTitle@
taskTitleSelector :: Selector
taskTitleSelector = mkSelector "taskTitle"

-- | @Selector@ for @status@
statusSelector :: Selector
statusSelector = mkSelector "status"

-- | @Selector@ for @priority@
prioritySelector :: Selector
prioritySelector = mkSelector "priority"

-- | @Selector@ for @spatialEventTrigger@
spatialEventTriggerSelector :: Selector
spatialEventTriggerSelector = mkSelector "spatialEventTrigger"

-- | @Selector@ for @temporalEventTrigger@
temporalEventTriggerSelector :: Selector
temporalEventTriggerSelector = mkSelector "temporalEventTrigger"

