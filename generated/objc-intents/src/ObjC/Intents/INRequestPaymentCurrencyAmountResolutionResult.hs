{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Generated bindings for @INRequestPaymentCurrencyAmountResolutionResult@.
module ObjC.Intents.INRequestPaymentCurrencyAmountResolutionResult
  ( INRequestPaymentCurrencyAmountResolutionResult
  , IsINRequestPaymentCurrencyAmountResolutionResult(..)
  , unsupportedForReason
  , initWithCurrencyAmountResolutionResult
  , unsupportedForReasonSelector
  , initWithCurrencyAmountResolutionResultSelector

  -- * Enum types
  , INRequestPaymentCurrencyAmountUnsupportedReason(INRequestPaymentCurrencyAmountUnsupportedReason)
  , pattern INRequestPaymentCurrencyAmountUnsupportedReasonPaymentsAmountBelowMinimum
  , pattern INRequestPaymentCurrencyAmountUnsupportedReasonPaymentsAmountAboveMaximum
  , pattern INRequestPaymentCurrencyAmountUnsupportedReasonPaymentsCurrencyUnsupported

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

-- | @+ unsupportedForReason:@
unsupportedForReason :: INRequestPaymentCurrencyAmountUnsupportedReason -> IO (Id INRequestPaymentCurrencyAmountResolutionResult)
unsupportedForReason reason =
  do
    cls' <- getRequiredClass "INRequestPaymentCurrencyAmountResolutionResult"
    sendClassMsg cls' (mkSelector "unsupportedForReason:") (retPtr retVoid) [argCLong (coerce reason)] >>= retainedObject . castPtr

-- | @- initWithCurrencyAmountResolutionResult:@
initWithCurrencyAmountResolutionResult :: (IsINRequestPaymentCurrencyAmountResolutionResult inRequestPaymentCurrencyAmountResolutionResult, IsINCurrencyAmountResolutionResult currencyAmountResolutionResult) => inRequestPaymentCurrencyAmountResolutionResult -> currencyAmountResolutionResult -> IO (Id INRequestPaymentCurrencyAmountResolutionResult)
initWithCurrencyAmountResolutionResult inRequestPaymentCurrencyAmountResolutionResult  currencyAmountResolutionResult =
withObjCPtr currencyAmountResolutionResult $ \raw_currencyAmountResolutionResult ->
    sendMsg inRequestPaymentCurrencyAmountResolutionResult (mkSelector "initWithCurrencyAmountResolutionResult:") (retPtr retVoid) [argPtr (castPtr raw_currencyAmountResolutionResult :: Ptr ())] >>= ownedObject . castPtr

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @unsupportedForReason:@
unsupportedForReasonSelector :: Selector
unsupportedForReasonSelector = mkSelector "unsupportedForReason:"

-- | @Selector@ for @initWithCurrencyAmountResolutionResult:@
initWithCurrencyAmountResolutionResultSelector :: Selector
initWithCurrencyAmountResolutionResultSelector = mkSelector "initWithCurrencyAmountResolutionResult:"

