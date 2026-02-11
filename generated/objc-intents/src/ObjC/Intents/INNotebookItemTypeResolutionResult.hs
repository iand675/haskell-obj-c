{-# LANGUAGE PatternSynonyms #-}
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
  , successWithResolvedNotebookItemTypeSelector
  , disambiguationWithNotebookItemTypesToDisambiguateSelector
  , confirmationRequiredWithNotebookItemTypeToConfirmSelector

  -- * Enum types
  , INNotebookItemType(INNotebookItemType)
  , pattern INNotebookItemTypeUnknown
  , pattern INNotebookItemTypeNote
  , pattern INNotebookItemTypeTaskList
  , pattern INNotebookItemTypeTask

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

-- | @+ successWithResolvedNotebookItemType:@
successWithResolvedNotebookItemType :: INNotebookItemType -> IO (Id INNotebookItemTypeResolutionResult)
successWithResolvedNotebookItemType resolvedNotebookItemType =
  do
    cls' <- getRequiredClass "INNotebookItemTypeResolutionResult"
    sendClassMsg cls' (mkSelector "successWithResolvedNotebookItemType:") (retPtr retVoid) [argCLong (coerce resolvedNotebookItemType)] >>= retainedObject . castPtr

-- | @+ disambiguationWithNotebookItemTypesToDisambiguate:@
disambiguationWithNotebookItemTypesToDisambiguate :: IsNSArray notebookItemTypesToDisambiguate => notebookItemTypesToDisambiguate -> IO (Id INNotebookItemTypeResolutionResult)
disambiguationWithNotebookItemTypesToDisambiguate notebookItemTypesToDisambiguate =
  do
    cls' <- getRequiredClass "INNotebookItemTypeResolutionResult"
    withObjCPtr notebookItemTypesToDisambiguate $ \raw_notebookItemTypesToDisambiguate ->
      sendClassMsg cls' (mkSelector "disambiguationWithNotebookItemTypesToDisambiguate:") (retPtr retVoid) [argPtr (castPtr raw_notebookItemTypesToDisambiguate :: Ptr ())] >>= retainedObject . castPtr

-- | @+ confirmationRequiredWithNotebookItemTypeToConfirm:@
confirmationRequiredWithNotebookItemTypeToConfirm :: INNotebookItemType -> IO (Id INNotebookItemTypeResolutionResult)
confirmationRequiredWithNotebookItemTypeToConfirm notebookItemTypeToConfirm =
  do
    cls' <- getRequiredClass "INNotebookItemTypeResolutionResult"
    sendClassMsg cls' (mkSelector "confirmationRequiredWithNotebookItemTypeToConfirm:") (retPtr retVoid) [argCLong (coerce notebookItemTypeToConfirm)] >>= retainedObject . castPtr

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @successWithResolvedNotebookItemType:@
successWithResolvedNotebookItemTypeSelector :: Selector
successWithResolvedNotebookItemTypeSelector = mkSelector "successWithResolvedNotebookItemType:"

-- | @Selector@ for @disambiguationWithNotebookItemTypesToDisambiguate:@
disambiguationWithNotebookItemTypesToDisambiguateSelector :: Selector
disambiguationWithNotebookItemTypesToDisambiguateSelector = mkSelector "disambiguationWithNotebookItemTypesToDisambiguate:"

-- | @Selector@ for @confirmationRequiredWithNotebookItemTypeToConfirm:@
confirmationRequiredWithNotebookItemTypeToConfirmSelector :: Selector
confirmationRequiredWithNotebookItemTypeToConfirmSelector = mkSelector "confirmationRequiredWithNotebookItemTypeToConfirm:"

