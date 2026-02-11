{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Generated bindings for @INSendPaymentCurrencyAmountResolutionResult@.
module ObjC.Intents.INSendPaymentCurrencyAmountResolutionResult
  ( INSendPaymentCurrencyAmountResolutionResult
  , IsINSendPaymentCurrencyAmountResolutionResult(..)
  , unsupportedForReason
  , initWithCurrencyAmountResolutionResult
  , unsupportedForReasonSelector
  , initWithCurrencyAmountResolutionResultSelector

  -- * Enum types
  , INSendPaymentCurrencyAmountUnsupportedReason(INSendPaymentCurrencyAmountUnsupportedReason)
  , pattern INSendPaymentCurrencyAmountUnsupportedReasonPaymentsAmountBelowMinimum
  , pattern INSendPaymentCurrencyAmountUnsupportedReasonPaymentsAmountAboveMaximum
  , pattern INSendPaymentCurrencyAmountUnsupportedReasonPaymentsCurrencyUnsupported

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
unsupportedForReason :: INSendPaymentCurrencyAmountUnsupportedReason -> IO (Id INSendPaymentCurrencyAmountResolutionResult)
unsupportedForReason reason =
  do
    cls' <- getRequiredClass "INSendPaymentCurrencyAmountResolutionResult"
    sendClassMsg cls' (mkSelector "unsupportedForReason:") (retPtr retVoid) [argCLong (coerce reason)] >>= retainedObject . castPtr

-- | @- initWithCurrencyAmountResolutionResult:@
initWithCurrencyAmountResolutionResult :: (IsINSendPaymentCurrencyAmountResolutionResult inSendPaymentCurrencyAmountResolutionResult, IsINCurrencyAmountResolutionResult currencyAmountResolutionResult) => inSendPaymentCurrencyAmountResolutionResult -> currencyAmountResolutionResult -> IO (Id INSendPaymentCurrencyAmountResolutionResult)
initWithCurrencyAmountResolutionResult inSendPaymentCurrencyAmountResolutionResult  currencyAmountResolutionResult =
withObjCPtr currencyAmountResolutionResult $ \raw_currencyAmountResolutionResult ->
    sendMsg inSendPaymentCurrencyAmountResolutionResult (mkSelector "initWithCurrencyAmountResolutionResult:") (retPtr retVoid) [argPtr (castPtr raw_currencyAmountResolutionResult :: Ptr ())] >>= ownedObject . castPtr

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @unsupportedForReason:@
unsupportedForReasonSelector :: Selector
unsupportedForReasonSelector = mkSelector "unsupportedForReason:"

-- | @Selector@ for @initWithCurrencyAmountResolutionResult:@
initWithCurrencyAmountResolutionResultSelector :: Selector
initWithCurrencyAmountResolutionResultSelector = mkSelector "initWithCurrencyAmountResolutionResult:"

