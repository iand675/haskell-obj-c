{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Generated bindings for @INTaskResolutionResult@.
module ObjC.Intents.INTaskResolutionResult
  ( INTaskResolutionResult
  , IsINTaskResolutionResult(..)
  , successWithResolvedTask
  , disambiguationWithTasksToDisambiguate
  , confirmationRequiredWithTaskToConfirm
  , confirmationRequiredWithTaskToConfirmSelector
  , disambiguationWithTasksToDisambiguateSelector
  , successWithResolvedTaskSelector


  ) where

import Foreign.Ptr (Ptr, FunPtr)
import Foreign.C.Types

import ObjC.Runtime.Types
import ObjC.Runtime.Message (sendMessage, sendOwnedMessage, sendClassMessage, sendOwnedClassMessage)
import ObjC.Runtime.Selector (mkSelector)
import ObjC.Runtime.Class (getRequiredClass)

import ObjC.Intents.Internal.Classes
import ObjC.Foundation.Internal.Classes

-- | @+ successWithResolvedTask:@
successWithResolvedTask :: IsINTask resolvedTask => resolvedTask -> IO (Id INTaskResolutionResult)
successWithResolvedTask resolvedTask =
  do
    cls' <- getRequiredClass "INTaskResolutionResult"
    sendClassMessage cls' successWithResolvedTaskSelector (toINTask resolvedTask)

-- | @+ disambiguationWithTasksToDisambiguate:@
disambiguationWithTasksToDisambiguate :: IsNSArray tasksToDisambiguate => tasksToDisambiguate -> IO (Id INTaskResolutionResult)
disambiguationWithTasksToDisambiguate tasksToDisambiguate =
  do
    cls' <- getRequiredClass "INTaskResolutionResult"
    sendClassMessage cls' disambiguationWithTasksToDisambiguateSelector (toNSArray tasksToDisambiguate)

-- | @+ confirmationRequiredWithTaskToConfirm:@
confirmationRequiredWithTaskToConfirm :: IsINTask taskToConfirm => taskToConfirm -> IO (Id INTaskResolutionResult)
confirmationRequiredWithTaskToConfirm taskToConfirm =
  do
    cls' <- getRequiredClass "INTaskResolutionResult"
    sendClassMessage cls' confirmationRequiredWithTaskToConfirmSelector (toINTask taskToConfirm)

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @successWithResolvedTask:@
successWithResolvedTaskSelector :: Selector '[Id INTask] (Id INTaskResolutionResult)
successWithResolvedTaskSelector = mkSelector "successWithResolvedTask:"

-- | @Selector@ for @disambiguationWithTasksToDisambiguate:@
disambiguationWithTasksToDisambiguateSelector :: Selector '[Id NSArray] (Id INTaskResolutionResult)
disambiguationWithTasksToDisambiguateSelector = mkSelector "disambiguationWithTasksToDisambiguate:"

-- | @Selector@ for @confirmationRequiredWithTaskToConfirm:@
confirmationRequiredWithTaskToConfirmSelector :: Selector '[Id INTask] (Id INTaskResolutionResult)
confirmationRequiredWithTaskToConfirmSelector = mkSelector "confirmationRequiredWithTaskToConfirm:"

