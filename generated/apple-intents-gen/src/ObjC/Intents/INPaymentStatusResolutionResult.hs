{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Generated bindings for @INPaymentStatusResolutionResult@.
module ObjC.Intents.INPaymentStatusResolutionResult
  ( INPaymentStatusResolutionResult
  , IsINPaymentStatusResolutionResult(..)
  , successWithResolvedPaymentStatus
  , successWithResolvedValue
  , confirmationRequiredWithPaymentStatusToConfirm
  , confirmationRequiredWithValueToConfirm
  , confirmationRequiredWithPaymentStatusToConfirmSelector
  , confirmationRequiredWithValueToConfirmSelector
  , successWithResolvedPaymentStatusSelector
  , successWithResolvedValueSelector

  -- * Enum types
  , INPaymentStatus(INPaymentStatus)
  , pattern INPaymentStatusUnknown
  , pattern INPaymentStatusPending
  , pattern INPaymentStatusCompleted
  , pattern INPaymentStatusCanceled
  , pattern INPaymentStatusFailed
  , pattern INPaymentStatusUnpaid

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

-- | @+ successWithResolvedPaymentStatus:@
successWithResolvedPaymentStatus :: INPaymentStatus -> IO (Id INPaymentStatusResolutionResult)
successWithResolvedPaymentStatus resolvedPaymentStatus =
  do
    cls' <- getRequiredClass "INPaymentStatusResolutionResult"
    sendClassMessage cls' successWithResolvedPaymentStatusSelector resolvedPaymentStatus

-- | @+ successWithResolvedValue:@
successWithResolvedValue :: INPaymentStatus -> IO (Id INPaymentStatusResolutionResult)
successWithResolvedValue resolvedValue =
  do
    cls' <- getRequiredClass "INPaymentStatusResolutionResult"
    sendClassMessage cls' successWithResolvedValueSelector resolvedValue

-- | @+ confirmationRequiredWithPaymentStatusToConfirm:@
confirmationRequiredWithPaymentStatusToConfirm :: INPaymentStatus -> IO (Id INPaymentStatusResolutionResult)
confirmationRequiredWithPaymentStatusToConfirm paymentStatusToConfirm =
  do
    cls' <- getRequiredClass "INPaymentStatusResolutionResult"
    sendClassMessage cls' confirmationRequiredWithPaymentStatusToConfirmSelector paymentStatusToConfirm

-- | @+ confirmationRequiredWithValueToConfirm:@
confirmationRequiredWithValueToConfirm :: INPaymentStatus -> IO (Id INPaymentStatusResolutionResult)
confirmationRequiredWithValueToConfirm valueToConfirm =
  do
    cls' <- getRequiredClass "INPaymentStatusResolutionResult"
    sendClassMessage cls' confirmationRequiredWithValueToConfirmSelector valueToConfirm

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @successWithResolvedPaymentStatus:@
successWithResolvedPaymentStatusSelector :: Selector '[INPaymentStatus] (Id INPaymentStatusResolutionResult)
successWithResolvedPaymentStatusSelector = mkSelector "successWithResolvedPaymentStatus:"

-- | @Selector@ for @successWithResolvedValue:@
successWithResolvedValueSelector :: Selector '[INPaymentStatus] (Id INPaymentStatusResolutionResult)
successWithResolvedValueSelector = mkSelector "successWithResolvedValue:"

-- | @Selector@ for @confirmationRequiredWithPaymentStatusToConfirm:@
confirmationRequiredWithPaymentStatusToConfirmSelector :: Selector '[INPaymentStatus] (Id INPaymentStatusResolutionResult)
confirmationRequiredWithPaymentStatusToConfirmSelector = mkSelector "confirmationRequiredWithPaymentStatusToConfirm:"

-- | @Selector@ for @confirmationRequiredWithValueToConfirm:@
confirmationRequiredWithValueToConfirmSelector :: Selector '[INPaymentStatus] (Id INPaymentStatusResolutionResult)
confirmationRequiredWithValueToConfirmSelector = mkSelector "confirmationRequiredWithValueToConfirm:"

