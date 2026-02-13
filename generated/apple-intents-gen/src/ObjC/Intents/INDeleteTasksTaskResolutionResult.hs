{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Generated bindings for @INDeleteTasksTaskResolutionResult@.
module ObjC.Intents.INDeleteTasksTaskResolutionResult
  ( INDeleteTasksTaskResolutionResult
  , IsINDeleteTasksTaskResolutionResult(..)
  , unsupportedForReason
  , initWithTaskResolutionResult
  , initWithTaskResolutionResultSelector
  , unsupportedForReasonSelector

  -- * Enum types
  , INDeleteTasksTaskUnsupportedReason(INDeleteTasksTaskUnsupportedReason)
  , pattern INDeleteTasksTaskUnsupportedReasonNoTasksFound
  , pattern INDeleteTasksTaskUnsupportedReasonNoTasksInApp

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
unsupportedForReason :: INDeleteTasksTaskUnsupportedReason -> IO (Id INDeleteTasksTaskResolutionResult)
unsupportedForReason reason =
  do
    cls' <- getRequiredClass "INDeleteTasksTaskResolutionResult"
    sendClassMessage cls' unsupportedForReasonSelector reason

-- | @- initWithTaskResolutionResult:@
initWithTaskResolutionResult :: (IsINDeleteTasksTaskResolutionResult inDeleteTasksTaskResolutionResult, IsINTaskResolutionResult taskResolutionResult) => inDeleteTasksTaskResolutionResult -> taskResolutionResult -> IO (Id INDeleteTasksTaskResolutionResult)
initWithTaskResolutionResult inDeleteTasksTaskResolutionResult taskResolutionResult =
  sendOwnedMessage inDeleteTasksTaskResolutionResult initWithTaskResolutionResultSelector (toINTaskResolutionResult taskResolutionResult)

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @unsupportedForReason:@
unsupportedForReasonSelector :: Selector '[INDeleteTasksTaskUnsupportedReason] (Id INDeleteTasksTaskResolutionResult)
unsupportedForReasonSelector = mkSelector "unsupportedForReason:"

-- | @Selector@ for @initWithTaskResolutionResult:@
initWithTaskResolutionResultSelector :: Selector '[Id INTaskResolutionResult] (Id INDeleteTasksTaskResolutionResult)
initWithTaskResolutionResultSelector = mkSelector "initWithTaskResolutionResult:"

