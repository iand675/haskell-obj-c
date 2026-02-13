{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Generated bindings for @INBalanceTypeResolutionResult@.
module ObjC.Intents.INBalanceTypeResolutionResult
  ( INBalanceTypeResolutionResult
  , IsINBalanceTypeResolutionResult(..)
  , successWithResolvedBalanceType
  , confirmationRequiredWithBalanceTypeToConfirm
  , confirmationRequiredWithBalanceTypeToConfirmSelector
  , successWithResolvedBalanceTypeSelector

  -- * Enum types
  , INBalanceType(INBalanceType)
  , pattern INBalanceTypeUnknown
  , pattern INBalanceTypeMoney
  , pattern INBalanceTypePoints
  , pattern INBalanceTypeMiles

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

-- | @+ successWithResolvedBalanceType:@
successWithResolvedBalanceType :: INBalanceType -> IO (Id INBalanceTypeResolutionResult)
successWithResolvedBalanceType resolvedBalanceType =
  do
    cls' <- getRequiredClass "INBalanceTypeResolutionResult"
    sendClassMessage cls' successWithResolvedBalanceTypeSelector resolvedBalanceType

-- | @+ confirmationRequiredWithBalanceTypeToConfirm:@
confirmationRequiredWithBalanceTypeToConfirm :: INBalanceType -> IO (Id INBalanceTypeResolutionResult)
confirmationRequiredWithBalanceTypeToConfirm balanceTypeToConfirm =
  do
    cls' <- getRequiredClass "INBalanceTypeResolutionResult"
    sendClassMessage cls' confirmationRequiredWithBalanceTypeToConfirmSelector balanceTypeToConfirm

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @successWithResolvedBalanceType:@
successWithResolvedBalanceTypeSelector :: Selector '[INBalanceType] (Id INBalanceTypeResolutionResult)
successWithResolvedBalanceTypeSelector = mkSelector "successWithResolvedBalanceType:"

-- | @Selector@ for @confirmationRequiredWithBalanceTypeToConfirm:@
confirmationRequiredWithBalanceTypeToConfirmSelector :: Selector '[INBalanceType] (Id INBalanceTypeResolutionResult)
confirmationRequiredWithBalanceTypeToConfirmSelector = mkSelector "confirmationRequiredWithBalanceTypeToConfirm:"

