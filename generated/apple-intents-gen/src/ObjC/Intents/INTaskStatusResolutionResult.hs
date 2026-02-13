{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Generated bindings for @INTaskStatusResolutionResult@.
module ObjC.Intents.INTaskStatusResolutionResult
  ( INTaskStatusResolutionResult
  , IsINTaskStatusResolutionResult(..)
  , successWithResolvedTaskStatus
  , confirmationRequiredWithTaskStatusToConfirm
  , confirmationRequiredWithTaskStatusToConfirmSelector
  , successWithResolvedTaskStatusSelector

  -- * Enum types
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

-- | @+ successWithResolvedTaskStatus:@
successWithResolvedTaskStatus :: INTaskStatus -> IO (Id INTaskStatusResolutionResult)
successWithResolvedTaskStatus resolvedTaskStatus =
  do
    cls' <- getRequiredClass "INTaskStatusResolutionResult"
    sendClassMessage cls' successWithResolvedTaskStatusSelector resolvedTaskStatus

-- | @+ confirmationRequiredWithTaskStatusToConfirm:@
confirmationRequiredWithTaskStatusToConfirm :: INTaskStatus -> IO (Id INTaskStatusResolutionResult)
confirmationRequiredWithTaskStatusToConfirm taskStatusToConfirm =
  do
    cls' <- getRequiredClass "INTaskStatusResolutionResult"
    sendClassMessage cls' confirmationRequiredWithTaskStatusToConfirmSelector taskStatusToConfirm

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @successWithResolvedTaskStatus:@
successWithResolvedTaskStatusSelector :: Selector '[INTaskStatus] (Id INTaskStatusResolutionResult)
successWithResolvedTaskStatusSelector = mkSelector "successWithResolvedTaskStatus:"

-- | @Selector@ for @confirmationRequiredWithTaskStatusToConfirm:@
confirmationRequiredWithTaskStatusToConfirmSelector :: Selector '[INTaskStatus] (Id INTaskStatusResolutionResult)
confirmationRequiredWithTaskStatusToConfirmSelector = mkSelector "confirmationRequiredWithTaskStatusToConfirm:"

