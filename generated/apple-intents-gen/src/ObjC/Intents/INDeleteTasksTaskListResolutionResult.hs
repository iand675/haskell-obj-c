{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Generated bindings for @INDeleteTasksTaskListResolutionResult@.
module ObjC.Intents.INDeleteTasksTaskListResolutionResult
  ( INDeleteTasksTaskListResolutionResult
  , IsINDeleteTasksTaskListResolutionResult(..)
  , unsupportedForReason
  , initWithTaskListResolutionResult
  , initWithTaskListResolutionResultSelector
  , unsupportedForReasonSelector

  -- * Enum types
  , INDeleteTasksTaskListUnsupportedReason(INDeleteTasksTaskListUnsupportedReason)
  , pattern INDeleteTasksTaskListUnsupportedReasonNoTaskListFound

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

-- | @+ unsupportedForReason:@
unsupportedForReason :: INDeleteTasksTaskListUnsupportedReason -> IO (Id INDeleteTasksTaskListResolutionResult)
unsupportedForReason reason =
  do
    cls' <- getRequiredClass "INDeleteTasksTaskListResolutionResult"
    sendClassMessage cls' unsupportedForReasonSelector reason

-- | @- initWithTaskListResolutionResult:@
initWithTaskListResolutionResult :: (IsINDeleteTasksTaskListResolutionResult inDeleteTasksTaskListResolutionResult, IsINTaskListResolutionResult taskListResolutionResult) => inDeleteTasksTaskListResolutionResult -> taskListResolutionResult -> IO (Id INDeleteTasksTaskListResolutionResult)
initWithTaskListResolutionResult inDeleteTasksTaskListResolutionResult taskListResolutionResult =
  sendOwnedMessage inDeleteTasksTaskListResolutionResult initWithTaskListResolutionResultSelector (toINTaskListResolutionResult taskListResolutionResult)

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @unsupportedForReason:@
unsupportedForReasonSelector :: Selector '[INDeleteTasksTaskListUnsupportedReason] (Id INDeleteTasksTaskListResolutionResult)
unsupportedForReasonSelector = mkSelector "unsupportedForReason:"

-- | @Selector@ for @initWithTaskListResolutionResult:@
initWithTaskListResolutionResultSelector :: Selector '[Id INTaskListResolutionResult] (Id INDeleteTasksTaskListResolutionResult)
initWithTaskListResolutionResultSelector = mkSelector "initWithTaskListResolutionResult:"

