{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Generated bindings for @PKPaymentRequestPaymentMethodUpdate@.
module ObjC.PassKit.PKPaymentRequestPaymentMethodUpdate
  ( PKPaymentRequestPaymentMethodUpdate
  , IsPKPaymentRequestPaymentMethodUpdate(..)
  , initWithErrors_paymentSummaryItems
  , errors
  , setErrors
  , errorsSelector
  , initWithErrors_paymentSummaryItemsSelector
  , setErrorsSelector


  ) where

import Foreign.Ptr (Ptr, FunPtr)
import Foreign.C.Types

import ObjC.Runtime.Types
import ObjC.Runtime.Message (sendMessage, sendOwnedMessage, sendClassMessage, sendOwnedClassMessage)
import ObjC.Runtime.Selector (mkSelector)
import ObjC.Runtime.Class (getRequiredClass)

import ObjC.PassKit.Internal.Classes
import ObjC.Foundation.Internal.Classes

-- | @- initWithErrors:paymentSummaryItems:@
initWithErrors_paymentSummaryItems :: (IsPKPaymentRequestPaymentMethodUpdate pkPaymentRequestPaymentMethodUpdate, IsNSArray errors, IsNSArray paymentSummaryItems) => pkPaymentRequestPaymentMethodUpdate -> errors -> paymentSummaryItems -> IO (Id PKPaymentRequestPaymentMethodUpdate)
initWithErrors_paymentSummaryItems pkPaymentRequestPaymentMethodUpdate errors paymentSummaryItems =
  sendOwnedMessage pkPaymentRequestPaymentMethodUpdate initWithErrors_paymentSummaryItemsSelector (toNSArray errors) (toNSArray paymentSummaryItems)

-- | @- errors@
errors :: IsPKPaymentRequestPaymentMethodUpdate pkPaymentRequestPaymentMethodUpdate => pkPaymentRequestPaymentMethodUpdate -> IO (Id NSArray)
errors pkPaymentRequestPaymentMethodUpdate =
  sendMessage pkPaymentRequestPaymentMethodUpdate errorsSelector

-- | @- setErrors:@
setErrors :: (IsPKPaymentRequestPaymentMethodUpdate pkPaymentRequestPaymentMethodUpdate, IsNSArray value) => pkPaymentRequestPaymentMethodUpdate -> value -> IO ()
setErrors pkPaymentRequestPaymentMethodUpdate value =
  sendMessage pkPaymentRequestPaymentMethodUpdate setErrorsSelector (toNSArray value)

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @initWithErrors:paymentSummaryItems:@
initWithErrors_paymentSummaryItemsSelector :: Selector '[Id NSArray, Id NSArray] (Id PKPaymentRequestPaymentMethodUpdate)
initWithErrors_paymentSummaryItemsSelector = mkSelector "initWithErrors:paymentSummaryItems:"

-- | @Selector@ for @errors@
errorsSelector :: Selector '[] (Id NSArray)
errorsSelector = mkSelector "errors"

-- | @Selector@ for @setErrors:@
setErrorsSelector :: Selector '[Id NSArray] ()
setErrorsSelector = mkSelector "setErrors:"

