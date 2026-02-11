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
  , initWithErrors_paymentSummaryItems_shippingMethodsSelector
  , errorsSelector
  , setErrorsSelector


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

import ObjC.PassKit.Internal.Classes
import ObjC.Foundation.Internal.Classes

-- | @- initWithErrors:paymentSummaryItems:shippingMethods:@
initWithErrors_paymentSummaryItems_shippingMethods :: (IsPKPaymentRequestCouponCodeUpdate pkPaymentRequestCouponCodeUpdate, IsNSArray errors, IsNSArray paymentSummaryItems, IsNSArray shippingMethods) => pkPaymentRequestCouponCodeUpdate -> errors -> paymentSummaryItems -> shippingMethods -> IO (Id PKPaymentRequestCouponCodeUpdate)
initWithErrors_paymentSummaryItems_shippingMethods pkPaymentRequestCouponCodeUpdate  errors paymentSummaryItems shippingMethods =
withObjCPtr errors $ \raw_errors ->
  withObjCPtr paymentSummaryItems $ \raw_paymentSummaryItems ->
    withObjCPtr shippingMethods $ \raw_shippingMethods ->
        sendMsg pkPaymentRequestCouponCodeUpdate (mkSelector "initWithErrors:paymentSummaryItems:shippingMethods:") (retPtr retVoid) [argPtr (castPtr raw_errors :: Ptr ()), argPtr (castPtr raw_paymentSummaryItems :: Ptr ()), argPtr (castPtr raw_shippingMethods :: Ptr ())] >>= ownedObject . castPtr

-- | @- errors@
errors :: IsPKPaymentRequestCouponCodeUpdate pkPaymentRequestCouponCodeUpdate => pkPaymentRequestCouponCodeUpdate -> IO (Id NSArray)
errors pkPaymentRequestCouponCodeUpdate  =
  sendMsg pkPaymentRequestCouponCodeUpdate (mkSelector "errors") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- setErrors:@
setErrors :: (IsPKPaymentRequestCouponCodeUpdate pkPaymentRequestCouponCodeUpdate, IsNSArray value) => pkPaymentRequestCouponCodeUpdate -> value -> IO ()
setErrors pkPaymentRequestCouponCodeUpdate  value =
withObjCPtr value $ \raw_value ->
    sendMsg pkPaymentRequestCouponCodeUpdate (mkSelector "setErrors:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @initWithErrors:paymentSummaryItems:shippingMethods:@
initWithErrors_paymentSummaryItems_shippingMethodsSelector :: Selector
initWithErrors_paymentSummaryItems_shippingMethodsSelector = mkSelector "initWithErrors:paymentSummaryItems:shippingMethods:"

-- | @Selector@ for @errors@
errorsSelector :: Selector
errorsSelector = mkSelector "errors"

-- | @Selector@ for @setErrors:@
setErrorsSelector :: Selector
setErrorsSelector = mkSelector "setErrors:"

