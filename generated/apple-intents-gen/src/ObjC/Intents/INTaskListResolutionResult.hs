{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Generated bindings for @INTaskListResolutionResult@.
module ObjC.Intents.INTaskListResolutionResult
  ( INTaskListResolutionResult
  , IsINTaskListResolutionResult(..)
  , successWithResolvedTaskList
  , disambiguationWithTaskListsToDisambiguate
  , confirmationRequiredWithTaskListToConfirm
  , confirmationRequiredWithTaskListToConfirmSelector
  , disambiguationWithTaskListsToDisambiguateSelector
  , successWithResolvedTaskListSelector


  ) where

import Foreign.Ptr (Ptr, FunPtr)
import Foreign.C.Types

import ObjC.Runtime.Types
import ObjC.Runtime.Message (sendMessage, sendOwnedMessage, sendClassMessage, sendOwnedClassMessage)
import ObjC.Runtime.Selector (mkSelector)
import ObjC.Runtime.Class (getRequiredClass)

import ObjC.Intents.Internal.Classes
import ObjC.Foundation.Internal.Classes

-- | @+ successWithResolvedTaskList:@
successWithResolvedTaskList :: IsINTaskList resolvedTaskList => resolvedTaskList -> IO (Id INTaskListResolutionResult)
successWithResolvedTaskList resolvedTaskList =
  do
    cls' <- getRequiredClass "INTaskListResolutionResult"
    sendClassMessage cls' successWithResolvedTaskListSelector (toINTaskList resolvedTaskList)

-- | @+ disambiguationWithTaskListsToDisambiguate:@
disambiguationWithTaskListsToDisambiguate :: IsNSArray taskListsToDisambiguate => taskListsToDisambiguate -> IO (Id INTaskListResolutionResult)
disambiguationWithTaskListsToDisambiguate taskListsToDisambiguate =
  do
    cls' <- getRequiredClass "INTaskListResolutionResult"
    sendClassMessage cls' disambiguationWithTaskListsToDisambiguateSelector (toNSArray taskListsToDisambiguate)

-- | @+ confirmationRequiredWithTaskListToConfirm:@
confirmationRequiredWithTaskListToConfirm :: IsINTaskList taskListToConfirm => taskListToConfirm -> IO (Id INTaskListResolutionResult)
confirmationRequiredWithTaskListToConfirm taskListToConfirm =
  do
    cls' <- getRequiredClass "INTaskListResolutionResult"
    sendClassMessage cls' confirmationRequiredWithTaskListToConfirmSelector (toINTaskList taskListToConfirm)

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @successWithResolvedTaskList:@
successWithResolvedTaskListSelector :: Selector '[Id INTaskList] (Id INTaskListResolutionResult)
successWithResolvedTaskListSelector = mkSelector "successWithResolvedTaskList:"

-- | @Selector@ for @disambiguationWithTaskListsToDisambiguate:@
disambiguationWithTaskListsToDisambiguateSelector :: Selector '[Id NSArray] (Id INTaskListResolutionResult)
disambiguationWithTaskListsToDisambiguateSelector = mkSelector "disambiguationWithTaskListsToDisambiguate:"

-- | @Selector@ for @confirmationRequiredWithTaskListToConfirm:@
confirmationRequiredWithTaskListToConfirmSelector :: Selector '[Id INTaskList] (Id INTaskListResolutionResult)
confirmationRequiredWithTaskListToConfirmSelector = mkSelector "confirmationRequiredWithTaskListToConfirm:"

