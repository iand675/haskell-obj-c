{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Generated bindings for @INDateSearchTypeResolutionResult@.
module ObjC.Intents.INDateSearchTypeResolutionResult
  ( INDateSearchTypeResolutionResult
  , IsINDateSearchTypeResolutionResult(..)
  , successWithResolvedDateSearchType
  , confirmationRequiredWithDateSearchTypeToConfirm
  , confirmationRequiredWithDateSearchTypeToConfirmSelector
  , successWithResolvedDateSearchTypeSelector

  -- * Enum types
  , INDateSearchType(INDateSearchType)
  , pattern INDateSearchTypeUnknown
  , pattern INDateSearchTypeByDueDate
  , pattern INDateSearchTypeByModifiedDate
  , pattern INDateSearchTypeByCreatedDate

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

-- | @+ successWithResolvedDateSearchType:@
successWithResolvedDateSearchType :: INDateSearchType -> IO (Id INDateSearchTypeResolutionResult)
successWithResolvedDateSearchType resolvedDateSearchType =
  do
    cls' <- getRequiredClass "INDateSearchTypeResolutionResult"
    sendClassMessage cls' successWithResolvedDateSearchTypeSelector resolvedDateSearchType

-- | @+ confirmationRequiredWithDateSearchTypeToConfirm:@
confirmationRequiredWithDateSearchTypeToConfirm :: INDateSearchType -> IO (Id INDateSearchTypeResolutionResult)
confirmationRequiredWithDateSearchTypeToConfirm dateSearchTypeToConfirm =
  do
    cls' <- getRequiredClass "INDateSearchTypeResolutionResult"
    sendClassMessage cls' confirmationRequiredWithDateSearchTypeToConfirmSelector dateSearchTypeToConfirm

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @successWithResolvedDateSearchType:@
successWithResolvedDateSearchTypeSelector :: Selector '[INDateSearchType] (Id INDateSearchTypeResolutionResult)
successWithResolvedDateSearchTypeSelector = mkSelector "successWithResolvedDateSearchType:"

-- | @Selector@ for @confirmationRequiredWithDateSearchTypeToConfirm:@
confirmationRequiredWithDateSearchTypeToConfirmSelector :: Selector '[INDateSearchType] (Id INDateSearchTypeResolutionResult)
confirmationRequiredWithDateSearchTypeToConfirmSelector = mkSelector "confirmationRequiredWithDateSearchTypeToConfirm:"

