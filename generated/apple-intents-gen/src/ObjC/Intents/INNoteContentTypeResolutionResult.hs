{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Generated bindings for @INNoteContentTypeResolutionResult@.
module ObjC.Intents.INNoteContentTypeResolutionResult
  ( INNoteContentTypeResolutionResult
  , IsINNoteContentTypeResolutionResult(..)
  , successWithResolvedNoteContentType
  , confirmationRequiredWithNoteContentTypeToConfirm
  , confirmationRequiredWithNoteContentTypeToConfirmSelector
  , successWithResolvedNoteContentTypeSelector

  -- * Enum types
  , INNoteContentType(INNoteContentType)
  , pattern INNoteContentTypeUnknown
  , pattern INNoteContentTypeText
  , pattern INNoteContentTypeImage

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

-- | @+ successWithResolvedNoteContentType:@
successWithResolvedNoteContentType :: INNoteContentType -> IO (Id INNoteContentTypeResolutionResult)
successWithResolvedNoteContentType resolvedNoteContentType =
  do
    cls' <- getRequiredClass "INNoteContentTypeResolutionResult"
    sendClassMessage cls' successWithResolvedNoteContentTypeSelector resolvedNoteContentType

-- | @+ confirmationRequiredWithNoteContentTypeToConfirm:@
confirmationRequiredWithNoteContentTypeToConfirm :: INNoteContentType -> IO (Id INNoteContentTypeResolutionResult)
confirmationRequiredWithNoteContentTypeToConfirm noteContentTypeToConfirm =
  do
    cls' <- getRequiredClass "INNoteContentTypeResolutionResult"
    sendClassMessage cls' confirmationRequiredWithNoteContentTypeToConfirmSelector noteContentTypeToConfirm

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @successWithResolvedNoteContentType:@
successWithResolvedNoteContentTypeSelector :: Selector '[INNoteContentType] (Id INNoteContentTypeResolutionResult)
successWithResolvedNoteContentTypeSelector = mkSelector "successWithResolvedNoteContentType:"

-- | @Selector@ for @confirmationRequiredWithNoteContentTypeToConfirm:@
confirmationRequiredWithNoteContentTypeToConfirmSelector :: Selector '[INNoteContentType] (Id INNoteContentTypeResolutionResult)
confirmationRequiredWithNoteContentTypeToConfirmSelector = mkSelector "confirmationRequiredWithNoteContentTypeToConfirm:"

