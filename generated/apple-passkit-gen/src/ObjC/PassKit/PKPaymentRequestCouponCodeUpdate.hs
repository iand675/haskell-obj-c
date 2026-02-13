{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Generated bindings for @PKPaymentRequestCouponCodeUpdate@.
module ObjC.PassKit.PKPaymentRequestCouponCodeUpdate
  ( PKPaymentRequestCouponCodeUpdate
  , IsPKPaymentRequestCouponCodeUpdate(..)
  , initWithErrors_paymentSummaryItems_shippingMethods
  , errors
  , setErrors
  , errorsSelector
  , initWithErrors_paymentSummaryItems_shippingMethodsSelector
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

-- | @- initWithErrors:paymentSummaryItems:shippingMethods:@
initWithErrors_paymentSummaryItems_shippingMethods :: (IsPKPaymentRequestCouponCodeUpdate pkPaymentRequestCouponCodeUpdate, IsNSArray errors, IsNSArray paymentSummaryItems, IsNSArray shippingMethods) => pkPaymentRequestCouponCodeUpdate -> errors -> paymentSummaryItems -> shippingMethods -> IO (Id PKPaymentRequestCouponCodeUpdate)
initWithErrors_paymentSummaryItems_shippingMethods pkPaymentRequestCouponCodeUpdate errors paymentSummaryItems shippingMethods =
  sendOwnedMessage pkPaymentRequestCouponCodeUpdate initWithErrors_paymentSummaryItems_shippingMethodsSelector (toNSArray errors) (toNSArray paymentSummaryItems) (toNSArray shippingMethods)

-- | @- errors@
errors :: IsPKPaymentRequestCouponCodeUpdate pkPaymentRequestCouponCodeUpdate => pkPaymentRequestCouponCodeUpdate -> IO (Id NSArray)
errors pkPaymentRequestCouponCodeUpdate =
  sendMessage pkPaymentRequestCouponCodeUpdate errorsSelector

-- | @- setErrors:@
setErrors :: (IsPKPaymentRequestCouponCodeUpdate pkPaymentRequestCouponCodeUpdate, IsNSArray value) => pkPaymentRequestCouponCodeUpdate -> value -> IO ()
setErrors pkPaymentRequestCouponCodeUpdate value =
  sendMessage pkPaymentRequestCouponCodeUpdate setErrorsSelector (toNSArray value)

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @initWithErrors:paymentSummaryItems:shippingMethods:@
initWithErrors_paymentSummaryItems_shippingMethodsSelector :: Selector '[Id NSArray, Id NSArray, Id NSArray] (Id PKPaymentRequestCouponCodeUpdate)
initWithErrors_paymentSummaryItems_shippingMethodsSelector = mkSelector "initWithErrors:paymentSummaryItems:shippingMethods:"

-- | @Selector@ for @errors@
errorsSelector :: Selector '[] (Id NSArray)
errorsSelector = mkSelector "errors"

-- | @Selector@ for @setErrors:@
setErrorsSelector :: Selector '[Id NSArray] ()
setErrorsSelector = mkSelector "setErrors:"

