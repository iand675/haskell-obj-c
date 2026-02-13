{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Generated bindings for @INRequestPaymentCurrencyAmountResolutionResult@.
module ObjC.Intents.INRequestPaymentCurrencyAmountResolutionResult
  ( INRequestPaymentCurrencyAmountResolutionResult
  , IsINRequestPaymentCurrencyAmountResolutionResult(..)
  , unsupportedForReason
  , initWithCurrencyAmountResolutionResult
  , initWithCurrencyAmountResolutionResultSelector
  , unsupportedForReasonSelector

  -- * Enum types
  , INRequestPaymentCurrencyAmountUnsupportedReason(INRequestPaymentCurrencyAmountUnsupportedReason)
  , pattern INRequestPaymentCurrencyAmountUnsupportedReasonPaymentsAmountBelowMinimum
  , pattern INRequestPaymentCurrencyAmountUnsupportedReasonPaymentsAmountAboveMaximum
  , pattern INRequestPaymentCurrencyAmountUnsupportedReasonPaymentsCurrencyUnsupported

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
unsupportedForReason :: INRequestPaymentCurrencyAmountUnsupportedReason -> IO (Id INRequestPaymentCurrencyAmountResolutionResult)
unsupportedForReason reason =
  do
    cls' <- getRequiredClass "INRequestPaymentCurrencyAmountResolutionResult"
    sendClassMessage cls' unsupportedForReasonSelector reason

-- | @- initWithCurrencyAmountResolutionResult:@
initWithCurrencyAmountResolutionResult :: (IsINRequestPaymentCurrencyAmountResolutionResult inRequestPaymentCurrencyAmountResolutionResult, IsINCurrencyAmountResolutionResult currencyAmountResolutionResult) => inRequestPaymentCurrencyAmountResolutionResult -> currencyAmountResolutionResult -> IO (Id INRequestPaymentCurrencyAmountResolutionResult)
initWithCurrencyAmountResolutionResult inRequestPaymentCurrencyAmountResolutionResult currencyAmountResolutionResult =
  sendOwnedMessage inRequestPaymentCurrencyAmountResolutionResult initWithCurrencyAmountResolutionResultSelector (toINCurrencyAmountResolutionResult currencyAmountResolutionResult)

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @unsupportedForReason:@
unsupportedForReasonSelector :: Selector '[INRequestPaymentCurrencyAmountUnsupportedReason] (Id INRequestPaymentCurrencyAmountResolutionResult)
unsupportedForReasonSelector = mkSelector "unsupportedForReason:"

-- | @Selector@ for @initWithCurrencyAmountResolutionResult:@
initWithCurrencyAmountResolutionResultSelector :: Selector '[Id INCurrencyAmountResolutionResult] (Id INRequestPaymentCurrencyAmountResolutionResult)
initWithCurrencyAmountResolutionResultSelector = mkSelector "initWithCurrencyAmountResolutionResult:"

