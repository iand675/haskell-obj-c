{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Generated bindings for @PKPaymentRequestShippingContactUpdate@.
module ObjC.PassKit.PKPaymentRequestShippingContactUpdate
  ( PKPaymentRequestShippingContactUpdate
  , IsPKPaymentRequestShippingContactUpdate(..)
  , initWithErrors_paymentSummaryItems_shippingMethods
  , shippingMethods
  , setShippingMethods
  , errors
  , setErrors
  , errorsSelector
  , initWithErrors_paymentSummaryItems_shippingMethodsSelector
  , setErrorsSelector
  , setShippingMethodsSelector
  , shippingMethodsSelector


  ) where

import Foreign.Ptr (Ptr, FunPtr)
import Foreign.C.Types

import ObjC.Runtime.Types
import ObjC.Runtime.Message (sendMessage, sendOwnedMessage, sendClassMessage, sendOwnedClassMessage)
import ObjC.Runtime.Selector (mkSelector)
import ObjC.Runtime.Class (getRequiredClass)

import ObjC.PassKit.Internal.Classes
import ObjC.Foundation.Internal.Classes

-- | @- initWithErrors:paymentSummaryItems:shippingMethods:@
initWithErrors_paymentSummaryItems_shippingMethods :: (IsPKPaymentRequestShippingContactUpdate pkPaymentRequestShippingContactUpdate, IsNSArray errors, IsNSArray paymentSummaryItems, IsNSArray shippingMethods) => pkPaymentRequestShippingContactUpdate -> errors -> paymentSummaryItems -> shippingMethods -> IO (Id PKPaymentRequestShippingContactUpdate)
initWithErrors_paymentSummaryItems_shippingMethods pkPaymentRequestShippingContactUpdate errors paymentSummaryItems shippingMethods =
  sendOwnedMessage pkPaymentRequestShippingContactUpdate initWithErrors_paymentSummaryItems_shippingMethodsSelector (toNSArray errors) (toNSArray paymentSummaryItems) (toNSArray shippingMethods)

-- | @- shippingMethods@
shippingMethods :: IsPKPaymentRequestShippingContactUpdate pkPaymentRequestShippingContactUpdate => pkPaymentRequestShippingContactUpdate -> IO (Id NSArray)
shippingMethods pkPaymentRequestShippingContactUpdate =
  sendMessage pkPaymentRequestShippingContactUpdate shippingMethodsSelector

-- | @- setShippingMethods:@
setShippingMethods :: (IsPKPaymentRequestShippingContactUpdate pkPaymentRequestShippingContactUpdate, IsNSArray value) => pkPaymentRequestShippingContactUpdate -> value -> IO ()
setShippingMethods pkPaymentRequestShippingContactUpdate value =
  sendMessage pkPaymentRequestShippingContactUpdate setShippingMethodsSelector (toNSArray value)

-- | @- errors@
errors :: IsPKPaymentRequestShippingContactUpdate pkPaymentRequestShippingContactUpdate => pkPaymentRequestShippingContactUpdate -> IO (Id NSArray)
errors pkPaymentRequestShippingContactUpdate =
  sendMessage pkPaymentRequestShippingContactUpdate errorsSelector

-- | @- setErrors:@
setErrors :: (IsPKPaymentRequestShippingContactUpdate pkPaymentRequestShippingContactUpdate, IsNSArray value) => pkPaymentRequestShippingContactUpdate -> value -> IO ()
setErrors pkPaymentRequestShippingContactUpdate value =
  sendMessage pkPaymentRequestShippingContactUpdate setErrorsSelector (toNSArray value)

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @initWithErrors:paymentSummaryItems:shippingMethods:@
initWithErrors_paymentSummaryItems_shippingMethodsSelector :: Selector '[Id NSArray, Id NSArray, Id NSArray] (Id PKPaymentRequestShippingContactUpdate)
initWithErrors_paymentSummaryItems_shippingMethodsSelector = mkSelector "initWithErrors:paymentSummaryItems:shippingMethods:"

-- | @Selector@ for @shippingMethods@
shippingMethodsSelector :: Selector '[] (Id NSArray)
shippingMethodsSelector = mkSelector "shippingMethods"

-- | @Selector@ for @setShippingMethods:@
setShippingMethodsSelector :: Selector '[Id NSArray] ()
setShippingMethodsSelector = mkSelector "setShippingMethods:"

-- | @Selector@ for @errors@
errorsSelector :: Selector '[] (Id NSArray)
errorsSelector = mkSelector "errors"

-- | @Selector@ for @setErrors:@
setErrorsSelector :: Selector '[Id NSArray] ()
setErrorsSelector = mkSelector "setErrors:"

