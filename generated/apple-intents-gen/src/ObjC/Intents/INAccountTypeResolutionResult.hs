{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Generated bindings for @INAccountTypeResolutionResult@.
module ObjC.Intents.INAccountTypeResolutionResult
  ( INAccountTypeResolutionResult
  , IsINAccountTypeResolutionResult(..)
  , successWithResolvedAccountType
  , successWithResolvedValue
  , confirmationRequiredWithAccountTypeToConfirm
  , confirmationRequiredWithValueToConfirm
  , confirmationRequiredWithAccountTypeToConfirmSelector
  , confirmationRequiredWithValueToConfirmSelector
  , successWithResolvedAccountTypeSelector
  , successWithResolvedValueSelector

  -- * Enum types
  , INAccountType(INAccountType)
  , pattern INAccountTypeUnknown
  , pattern INAccountTypeChecking
  , pattern INAccountTypeCredit
  , pattern INAccountTypeDebit
  , pattern INAccountTypeInvestment
  , pattern INAccountTypeMortgage
  , pattern INAccountTypePrepaid
  , pattern INAccountTypeSaving

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

-- | @+ successWithResolvedAccountType:@
successWithResolvedAccountType :: INAccountType -> IO (Id INAccountTypeResolutionResult)
successWithResolvedAccountType resolvedAccountType =
  do
    cls' <- getRequiredClass "INAccountTypeResolutionResult"
    sendClassMessage cls' successWithResolvedAccountTypeSelector resolvedAccountType

-- | @+ successWithResolvedValue:@
successWithResolvedValue :: INAccountType -> IO (Id INAccountTypeResolutionResult)
successWithResolvedValue resolvedValue =
  do
    cls' <- getRequiredClass "INAccountTypeResolutionResult"
    sendClassMessage cls' successWithResolvedValueSelector resolvedValue

-- | @+ confirmationRequiredWithAccountTypeToConfirm:@
confirmationRequiredWithAccountTypeToConfirm :: INAccountType -> IO (Id INAccountTypeResolutionResult)
confirmationRequiredWithAccountTypeToConfirm accountTypeToConfirm =
  do
    cls' <- getRequiredClass "INAccountTypeResolutionResult"
    sendClassMessage cls' confirmationRequiredWithAccountTypeToConfirmSelector accountTypeToConfirm

-- | @+ confirmationRequiredWithValueToConfirm:@
confirmationRequiredWithValueToConfirm :: INAccountType -> IO (Id INAccountTypeResolutionResult)
confirmationRequiredWithValueToConfirm valueToConfirm =
  do
    cls' <- getRequiredClass "INAccountTypeResolutionResult"
    sendClassMessage cls' confirmationRequiredWithValueToConfirmSelector valueToConfirm

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @successWithResolvedAccountType:@
successWithResolvedAccountTypeSelector :: Selector '[INAccountType] (Id INAccountTypeResolutionResult)
successWithResolvedAccountTypeSelector = mkSelector "successWithResolvedAccountType:"

-- | @Selector@ for @successWithResolvedValue:@
successWithResolvedValueSelector :: Selector '[INAccountType] (Id INAccountTypeResolutionResult)
successWithResolvedValueSelector = mkSelector "successWithResolvedValue:"

-- | @Selector@ for @confirmationRequiredWithAccountTypeToConfirm:@
confirmationRequiredWithAccountTypeToConfirmSelector :: Selector '[INAccountType] (Id INAccountTypeResolutionResult)
confirmationRequiredWithAccountTypeToConfirmSelector = mkSelector "confirmationRequiredWithAccountTypeToConfirm:"

-- | @Selector@ for @confirmationRequiredWithValueToConfirm:@
confirmationRequiredWithValueToConfirmSelector :: Selector '[INAccountType] (Id INAccountTypeResolutionResult)
confirmationRequiredWithValueToConfirmSelector = mkSelector "confirmationRequiredWithValueToConfirm:"

