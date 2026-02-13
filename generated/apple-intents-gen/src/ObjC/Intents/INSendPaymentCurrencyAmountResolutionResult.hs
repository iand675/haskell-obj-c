{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Generated bindings for @INSendPaymentCurrencyAmountResolutionResult@.
module ObjC.Intents.INSendPaymentCurrencyAmountResolutionResult
  ( INSendPaymentCurrencyAmountResolutionResult
  , IsINSendPaymentCurrencyAmountResolutionResult(..)
  , unsupportedForReason
  , initWithCurrencyAmountResolutionResult
  , initWithCurrencyAmountResolutionResultSelector
  , unsupportedForReasonSelector

  -- * Enum types
  , INSendPaymentCurrencyAmountUnsupportedReason(INSendPaymentCurrencyAmountUnsupportedReason)
  , pattern INSendPaymentCurrencyAmountUnsupportedReasonPaymentsAmountBelowMinimum
  , pattern INSendPaymentCurrencyAmountUnsupportedReasonPaymentsAmountAboveMaximum
  , pattern INSendPaymentCurrencyAmountUnsupportedReasonPaymentsCurrencyUnsupported

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

-- | @+ unsupportedForReason:@
unsupportedForReason :: INSendPaymentCurrencyAmountUnsupportedReason -> IO (Id INSendPaymentCurrencyAmountResolutionResult)
unsupportedForReason reason =
  do
    cls' <- getRequiredClass "INSendPaymentCurrencyAmountResolutionResult"
    sendClassMessage cls' unsupportedForReasonSelector reason

-- | @- initWithCurrencyAmountResolutionResult:@
initWithCurrencyAmountResolutionResult :: (IsINSendPaymentCurrencyAmountResolutionResult inSendPaymentCurrencyAmountResolutionResult, IsINCurrencyAmountResolutionResult currencyAmountResolutionResult) => inSendPaymentCurrencyAmountResolutionResult -> currencyAmountResolutionResult -> IO (Id INSendPaymentCurrencyAmountResolutionResult)
initWithCurrencyAmountResolutionResult inSendPaymentCurrencyAmountResolutionResult currencyAmountResolutionResult =
  sendOwnedMessage inSendPaymentCurrencyAmountResolutionResult initWithCurrencyAmountResolutionResultSelector (toINCurrencyAmountResolutionResult currencyAmountResolutionResult)

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @unsupportedForReason:@
unsupportedForReasonSelector :: Selector '[INSendPaymentCurrencyAmountUnsupportedReason] (Id INSendPaymentCurrencyAmountResolutionResult)
unsupportedForReasonSelector = mkSelector "unsupportedForReason:"

-- | @Selector@ for @initWithCurrencyAmountResolutionResult:@
initWithCurrencyAmountResolutionResultSelector :: Selector '[Id INCurrencyAmountResolutionResult] (Id INSendPaymentCurrencyAmountResolutionResult)
initWithCurrencyAmountResolutionResultSelector = mkSelector "initWithCurrencyAmountResolutionResult:"

