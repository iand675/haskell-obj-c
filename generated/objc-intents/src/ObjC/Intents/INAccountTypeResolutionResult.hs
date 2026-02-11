{-# LANGUAGE PatternSynonyms #-}
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
  , successWithResolvedAccountTypeSelector
  , successWithResolvedValueSelector
  , confirmationRequiredWithAccountTypeToConfirmSelector
  , confirmationRequiredWithValueToConfirmSelector

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

-- | @+ successWithResolvedAccountType:@
successWithResolvedAccountType :: INAccountType -> IO (Id INAccountTypeResolutionResult)
successWithResolvedAccountType resolvedAccountType =
  do
    cls' <- getRequiredClass "INAccountTypeResolutionResult"
    sendClassMsg cls' (mkSelector "successWithResolvedAccountType:") (retPtr retVoid) [argCLong (coerce resolvedAccountType)] >>= retainedObject . castPtr

-- | @+ successWithResolvedValue:@
successWithResolvedValue :: INAccountType -> IO (Id INAccountTypeResolutionResult)
successWithResolvedValue resolvedValue =
  do
    cls' <- getRequiredClass "INAccountTypeResolutionResult"
    sendClassMsg cls' (mkSelector "successWithResolvedValue:") (retPtr retVoid) [argCLong (coerce resolvedValue)] >>= retainedObject . castPtr

-- | @+ confirmationRequiredWithAccountTypeToConfirm:@
confirmationRequiredWithAccountTypeToConfirm :: INAccountType -> IO (Id INAccountTypeResolutionResult)
confirmationRequiredWithAccountTypeToConfirm accountTypeToConfirm =
  do
    cls' <- getRequiredClass "INAccountTypeResolutionResult"
    sendClassMsg cls' (mkSelector "confirmationRequiredWithAccountTypeToConfirm:") (retPtr retVoid) [argCLong (coerce accountTypeToConfirm)] >>= retainedObject . castPtr

-- | @+ confirmationRequiredWithValueToConfirm:@
confirmationRequiredWithValueToConfirm :: INAccountType -> IO (Id INAccountTypeResolutionResult)
confirmationRequiredWithValueToConfirm valueToConfirm =
  do
    cls' <- getRequiredClass "INAccountTypeResolutionResult"
    sendClassMsg cls' (mkSelector "confirmationRequiredWithValueToConfirm:") (retPtr retVoid) [argCLong (coerce valueToConfirm)] >>= retainedObject . castPtr

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @successWithResolvedAccountType:@
successWithResolvedAccountTypeSelector :: Selector
successWithResolvedAccountTypeSelector = mkSelector "successWithResolvedAccountType:"

-- | @Selector@ for @successWithResolvedValue:@
successWithResolvedValueSelector :: Selector
successWithResolvedValueSelector = mkSelector "successWithResolvedValue:"

-- | @Selector@ for @confirmationRequiredWithAccountTypeToConfirm:@
confirmationRequiredWithAccountTypeToConfirmSelector :: Selector
confirmationRequiredWithAccountTypeToConfirmSelector = mkSelector "confirmationRequiredWithAccountTypeToConfirm:"

-- | @Selector@ for @confirmationRequiredWithValueToConfirm:@
confirmationRequiredWithValueToConfirmSelector :: Selector
confirmationRequiredWithValueToConfirmSelector = mkSelector "confirmationRequiredWithValueToConfirm:"

