{-# LANGUAGE PatternSynonyms #-}
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

-- | @+ confirmationRequiredWithTaskListToConfirm:forReason:@
confirmationRequiredWithTaskListToConfirm_forReason :: IsINTaskList taskListToConfirm => taskListToConfirm -> INAddTasksTargetTaskListConfirmationReason -> IO (Id INAddTasksTargetTaskListResolutionResult)
confirmationRequiredWithTaskListToConfirm_forReason taskListToConfirm reason =
  do
    cls' <- getRequiredClass "INAddTasksTargetTaskListResolutionResult"
    withObjCPtr taskListToConfirm $ \raw_taskListToConfirm ->
      sendClassMsg cls' (mkSelector "confirmationRequiredWithTaskListToConfirm:forReason:") (retPtr retVoid) [argPtr (castPtr raw_taskListToConfirm :: Ptr ()), argCLong (coerce reason)] >>= retainedObject . castPtr

-- | @- initWithTaskListResolutionResult:@
initWithTaskListResolutionResult :: (IsINAddTasksTargetTaskListResolutionResult inAddTasksTargetTaskListResolutionResult, IsINTaskListResolutionResult taskListResolutionResult) => inAddTasksTargetTaskListResolutionResult -> taskListResolutionResult -> IO (Id INAddTasksTargetTaskListResolutionResult)
initWithTaskListResolutionResult inAddTasksTargetTaskListResolutionResult  taskListResolutionResult =
withObjCPtr taskListResolutionResult $ \raw_taskListResolutionResult ->
    sendMsg inAddTasksTargetTaskListResolutionResult (mkSelector "initWithTaskListResolutionResult:") (retPtr retVoid) [argPtr (castPtr raw_taskListResolutionResult :: Ptr ())] >>= ownedObject . castPtr

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @confirmationRequiredWithTaskListToConfirm:forReason:@
confirmationRequiredWithTaskListToConfirm_forReasonSelector :: Selector
confirmationRequiredWithTaskListToConfirm_forReasonSelector = mkSelector "confirmationRequiredWithTaskListToConfirm:forReason:"

-- | @Selector@ for @initWithTaskListResolutionResult:@
initWithTaskListResolutionResultSelector :: Selector
initWithTaskListResolutionResultSelector = mkSelector "initWithTaskListResolutionResult:"

