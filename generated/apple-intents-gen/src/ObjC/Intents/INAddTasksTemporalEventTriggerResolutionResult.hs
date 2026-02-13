{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Generated bindings for @INAddTasksTemporalEventTriggerResolutionResult@.
module ObjC.Intents.INAddTasksTemporalEventTriggerResolutionResult
  ( INAddTasksTemporalEventTriggerResolutionResult
  , IsINAddTasksTemporalEventTriggerResolutionResult(..)
  , unsupportedForReason
  , initWithTemporalEventTriggerResolutionResult
  , initWithTemporalEventTriggerResolutionResultSelector
  , unsupportedForReasonSelector

  -- * Enum types
  , INAddTasksTemporalEventTriggerUnsupportedReason(INAddTasksTemporalEventTriggerUnsupportedReason)
  , pattern INAddTasksTemporalEventTriggerUnsupportedReasonTimeInPast
  , pattern INAddTasksTemporalEventTriggerUnsupportedReasonInvalidRecurrence

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
unsupportedForReason :: INAddTasksTemporalEventTriggerUnsupportedReason -> IO (Id INAddTasksTemporalEventTriggerResolutionResult)
unsupportedForReason reason =
  do
    cls' <- getRequiredClass "INAddTasksTemporalEventTriggerResolutionResult"
    sendClassMessage cls' unsupportedForReasonSelector reason

-- | @- initWithTemporalEventTriggerResolutionResult:@
initWithTemporalEventTriggerResolutionResult :: (IsINAddTasksTemporalEventTriggerResolutionResult inAddTasksTemporalEventTriggerResolutionResult, IsINTemporalEventTriggerResolutionResult temporalEventTriggerResolutionResult) => inAddTasksTemporalEventTriggerResolutionResult -> temporalEventTriggerResolutionResult -> IO (Id INAddTasksTemporalEventTriggerResolutionResult)
initWithTemporalEventTriggerResolutionResult inAddTasksTemporalEventTriggerResolutionResult temporalEventTriggerResolutionResult =
  sendOwnedMessage inAddTasksTemporalEventTriggerResolutionResult initWithTemporalEventTriggerResolutionResultSelector (toINTemporalEventTriggerResolutionResult temporalEventTriggerResolutionResult)

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @unsupportedForReason:@
unsupportedForReasonSelector :: Selector '[INAddTasksTemporalEventTriggerUnsupportedReason] (Id INAddTasksTemporalEventTriggerResolutionResult)
unsupportedForReasonSelector = mkSelector "unsupportedForReason:"

-- | @Selector@ for @initWithTemporalEventTriggerResolutionResult:@
initWithTemporalEventTriggerResolutionResultSelector :: Selector '[Id INTemporalEventTriggerResolutionResult] (Id INAddTasksTemporalEventTriggerResolutionResult)
initWithTemporalEventTriggerResolutionResultSelector = mkSelector "initWithTemporalEventTriggerResolutionResult:"

