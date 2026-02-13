{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Generated bindings for @INSnoozeTasksTaskResolutionResult@.
module ObjC.Intents.INSnoozeTasksTaskResolutionResult
  ( INSnoozeTasksTaskResolutionResult
  , IsINSnoozeTasksTaskResolutionResult(..)
  , unsupportedForReason
  , initWithTaskResolutionResult
  , initWithTaskResolutionResultSelector
  , unsupportedForReasonSelector

  -- * Enum types
  , INSnoozeTasksTaskUnsupportedReason(INSnoozeTasksTaskUnsupportedReason)
  , pattern INSnoozeTasksTaskUnsupportedReasonNoTasksFound

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
unsupportedForReason :: INSnoozeTasksTaskUnsupportedReason -> IO (Id INSnoozeTasksTaskResolutionResult)
unsupportedForReason reason =
  do
    cls' <- getRequiredClass "INSnoozeTasksTaskResolutionResult"
    sendClassMessage cls' unsupportedForReasonSelector reason

-- | @- initWithTaskResolutionResult:@
initWithTaskResolutionResult :: (IsINSnoozeTasksTaskResolutionResult inSnoozeTasksTaskResolutionResult, IsINTaskResolutionResult taskResolutionResult) => inSnoozeTasksTaskResolutionResult -> taskResolutionResult -> IO (Id INSnoozeTasksTaskResolutionResult)
initWithTaskResolutionResult inSnoozeTasksTaskResolutionResult taskResolutionResult =
  sendOwnedMessage inSnoozeTasksTaskResolutionResult initWithTaskResolutionResultSelector (toINTaskResolutionResult taskResolutionResult)

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @unsupportedForReason:@
unsupportedForReasonSelector :: Selector '[INSnoozeTasksTaskUnsupportedReason] (Id INSnoozeTasksTaskResolutionResult)
unsupportedForReasonSelector = mkSelector "unsupportedForReason:"

-- | @Selector@ for @initWithTaskResolutionResult:@
initWithTaskResolutionResultSelector :: Selector '[Id INTaskResolutionResult] (Id INSnoozeTasksTaskResolutionResult)
initWithTaskResolutionResultSelector = mkSelector "initWithTaskResolutionResult:"

