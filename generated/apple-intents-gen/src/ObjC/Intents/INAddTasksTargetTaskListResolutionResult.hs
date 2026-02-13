{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Generated bindings for @INAddTasksTargetTaskListResolutionResult@.
module ObjC.Intents.INAddTasksTargetTaskListResolutionResult
  ( INAddTasksTargetTaskListResolutionResult
  , IsINAddTasksTargetTaskListResolutionResult(..)
  , confirmationRequiredWithTaskListToConfirm_forReason
  , initWithTaskListResolutionResult
  , confirmationRequiredWithTaskListToConfirm_forReasonSelector
  , initWithTaskListResolutionResultSelector

  -- * Enum types
  , INAddTasksTargetTaskListConfirmationReason(INAddTasksTargetTaskListConfirmationReason)
  , pattern INAddTasksTargetTaskListConfirmationReasonListShouldBeCreated

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

-- | @+ confirmationRequiredWithTaskListToConfirm:forReason:@
confirmationRequiredWithTaskListToConfirm_forReason :: IsINTaskList taskListToConfirm => taskListToConfirm -> INAddTasksTargetTaskListConfirmationReason -> IO (Id INAddTasksTargetTaskListResolutionResult)
confirmationRequiredWithTaskListToConfirm_forReason taskListToConfirm reason =
  do
    cls' <- getRequiredClass "INAddTasksTargetTaskListResolutionResult"
    sendClassMessage cls' confirmationRequiredWithTaskListToConfirm_forReasonSelector (toINTaskList taskListToConfirm) reason

-- | @- initWithTaskListResolutionResult:@
initWithTaskListResolutionResult :: (IsINAddTasksTargetTaskListResolutionResult inAddTasksTargetTaskListResolutionResult, IsINTaskListResolutionResult taskListResolutionResult) => inAddTasksTargetTaskListResolutionResult -> taskListResolutionResult -> IO (Id INAddTasksTargetTaskListResolutionResult)
initWithTaskListResolutionResult inAddTasksTargetTaskListResolutionResult taskListResolutionResult =
  sendOwnedMessage inAddTasksTargetTaskListResolutionResult initWithTaskListResolutionResultSelector (toINTaskListResolutionResult taskListResolutionResult)

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @confirmationRequiredWithTaskListToConfirm:forReason:@
confirmationRequiredWithTaskListToConfirm_forReasonSelector :: Selector '[Id INTaskList, INAddTasksTargetTaskListConfirmationReason] (Id INAddTasksTargetTaskListResolutionResult)
confirmationRequiredWithTaskListToConfirm_forReasonSelector = mkSelector "confirmationRequiredWithTaskListToConfirm:forReason:"

-- | @Selector@ for @initWithTaskListResolutionResult:@
initWithTaskListResolutionResultSelector :: Selector '[Id INTaskListResolutionResult] (Id INAddTasksTargetTaskListResolutionResult)
initWithTaskListResolutionResultSelector = mkSelector "initWithTaskListResolutionResult:"

