{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Generated bindings for @INNotebookItemTypeResolutionResult@.
module ObjC.Intents.INNotebookItemTypeResolutionResult
  ( INNotebookItemTypeResolutionResult
  , IsINNotebookItemTypeResolutionResult(..)
  , successWithResolvedNotebookItemType
  , disambiguationWithNotebookItemTypesToDisambiguate
  , confirmationRequiredWithNotebookItemTypeToConfirm
  , confirmationRequiredWithNotebookItemTypeToConfirmSelector
  , disambiguationWithNotebookItemTypesToDisambiguateSelector
  , successWithResolvedNotebookItemTypeSelector

  -- * Enum types
  , INNotebookItemType(INNotebookItemType)
  , pattern INNotebookItemTypeUnknown
  , pattern INNotebookItemTypeNote
  , pattern INNotebookItemTypeTaskList
  , pattern INNotebookItemTypeTask

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

-- | @+ successWithResolvedNotebookItemType:@
successWithResolvedNotebookItemType :: INNotebookItemType -> IO (Id INNotebookItemTypeResolutionResult)
successWithResolvedNotebookItemType resolvedNotebookItemType =
  do
    cls' <- getRequiredClass "INNotebookItemTypeResolutionResult"
    sendClassMessage cls' successWithResolvedNotebookItemTypeSelector resolvedNotebookItemType

-- | @+ disambiguationWithNotebookItemTypesToDisambiguate:@
disambiguationWithNotebookItemTypesToDisambiguate :: IsNSArray notebookItemTypesToDisambiguate => notebookItemTypesToDisambiguate -> IO (Id INNotebookItemTypeResolutionResult)
disambiguationWithNotebookItemTypesToDisambiguate notebookItemTypesToDisambiguate =
  do
    cls' <- getRequiredClass "INNotebookItemTypeResolutionResult"
    sendClassMessage cls' disambiguationWithNotebookItemTypesToDisambiguateSelector (toNSArray notebookItemTypesToDisambiguate)

-- | @+ confirmationRequiredWithNotebookItemTypeToConfirm:@
confirmationRequiredWithNotebookItemTypeToConfirm :: INNotebookItemType -> IO (Id INNotebookItemTypeResolutionResult)
confirmationRequiredWithNotebookItemTypeToConfirm notebookItemTypeToConfirm =
  do
    cls' <- getRequiredClass "INNotebookItemTypeResolutionResult"
    sendClassMessage cls' confirmationRequiredWithNotebookItemTypeToConfirmSelector notebookItemTypeToConfirm

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @successWithResolvedNotebookItemType:@
successWithResolvedNotebookItemTypeSelector :: Selector '[INNotebookItemType] (Id INNotebookItemTypeResolutionResult)
successWithResolvedNotebookItemTypeSelector = mkSelector "successWithResolvedNotebookItemType:"

-- | @Selector@ for @disambiguationWithNotebookItemTypesToDisambiguate:@
disambiguationWithNotebookItemTypesToDisambiguateSelector :: Selector '[Id NSArray] (Id INNotebookItemTypeResolutionResult)
disambiguationWithNotebookItemTypesToDisambiguateSelector = mkSelector "disambiguationWithNotebookItemTypesToDisambiguate:"

-- | @Selector@ for @confirmationRequiredWithNotebookItemTypeToConfirm:@
confirmationRequiredWithNotebookItemTypeToConfirmSelector :: Selector '[INNotebookItemType] (Id INNotebookItemTypeResolutionResult)
confirmationRequiredWithNotebookItemTypeToConfirmSelector = mkSelector "confirmationRequiredWithNotebookItemTypeToConfirm:"

