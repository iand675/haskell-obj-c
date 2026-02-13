{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Generated bindings for @INTaskPriorityResolutionResult@.
module ObjC.Intents.INTaskPriorityResolutionResult
  ( INTaskPriorityResolutionResult
  , IsINTaskPriorityResolutionResult(..)
  , successWithResolvedTaskPriority
  , confirmationRequiredWithTaskPriorityToConfirm
  , confirmationRequiredWithTaskPriorityToConfirmSelector
  , successWithResolvedTaskPrioritySelector

  -- * Enum types
  , INTaskPriority(INTaskPriority)
  , pattern INTaskPriorityUnknown
  , pattern INTaskPriorityNotFlagged
  , pattern INTaskPriorityFlagged

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

-- | @+ successWithResolvedTaskPriority:@
successWithResolvedTaskPriority :: INTaskPriority -> IO (Id INTaskPriorityResolutionResult)
successWithResolvedTaskPriority resolvedTaskPriority =
  do
    cls' <- getRequiredClass "INTaskPriorityResolutionResult"
    sendClassMessage cls' successWithResolvedTaskPrioritySelector resolvedTaskPriority

-- | @+ confirmationRequiredWithTaskPriorityToConfirm:@
confirmationRequiredWithTaskPriorityToConfirm :: INTaskPriority -> IO (Id INTaskPriorityResolutionResult)
confirmationRequiredWithTaskPriorityToConfirm taskPriorityToConfirm =
  do
    cls' <- getRequiredClass "INTaskPriorityResolutionResult"
    sendClassMessage cls' confirmationRequiredWithTaskPriorityToConfirmSelector taskPriorityToConfirm

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @successWithResolvedTaskPriority:@
successWithResolvedTaskPrioritySelector :: Selector '[INTaskPriority] (Id INTaskPriorityResolutionResult)
successWithResolvedTaskPrioritySelector = mkSelector "successWithResolvedTaskPriority:"

-- | @Selector@ for @confirmationRequiredWithTaskPriorityToConfirm:@
confirmationRequiredWithTaskPriorityToConfirmSelector :: Selector '[INTaskPriority] (Id INTaskPriorityResolutionResult)
confirmationRequiredWithTaskPriorityToConfirmSelector = mkSelector "confirmationRequiredWithTaskPriorityToConfirm:"

