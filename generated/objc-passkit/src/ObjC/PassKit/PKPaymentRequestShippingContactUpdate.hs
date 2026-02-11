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
  , initWithErrors_paymentSummaryItems_shippingMethodsSelector
  , shippingMethodsSelector
  , setShippingMethodsSelector
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
initWithErrors_paymentSummaryItems_shippingMethods :: (IsPKPaymentRequestShippingContactUpdate pkPaymentRequestShippingContactUpdate, IsNSArray errors, IsNSArray paymentSummaryItems, IsNSArray shippingMethods) => pkPaymentRequestShippingContactUpdate -> errors -> paymentSummaryItems -> shippingMethods -> IO (Id PKPaymentRequestShippingContactUpdate)
initWithErrors_paymentSummaryItems_shippingMethods pkPaymentRequestShippingContactUpdate  errors paymentSummaryItems shippingMethods =
withObjCPtr errors $ \raw_errors ->
  withObjCPtr paymentSummaryItems $ \raw_paymentSummaryItems ->
    withObjCPtr shippingMethods $ \raw_shippingMethods ->
        sendMsg pkPaymentRequestShippingContactUpdate (mkSelector "initWithErrors:paymentSummaryItems:shippingMethods:") (retPtr retVoid) [argPtr (castPtr raw_errors :: Ptr ()), argPtr (castPtr raw_paymentSummaryItems :: Ptr ()), argPtr (castPtr raw_shippingMethods :: Ptr ())] >>= ownedObject . castPtr

-- | @- shippingMethods@
shippingMethods :: IsPKPaymentRequestShippingContactUpdate pkPaymentRequestShippingContactUpdate => pkPaymentRequestShippingContactUpdate -> IO (Id NSArray)
shippingMethods pkPaymentRequestShippingContactUpdate  =
  sendMsg pkPaymentRequestShippingContactUpdate (mkSelector "shippingMethods") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- setShippingMethods:@
setShippingMethods :: (IsPKPaymentRequestShippingContactUpdate pkPaymentRequestShippingContactUpdate, IsNSArray value) => pkPaymentRequestShippingContactUpdate -> value -> IO ()
setShippingMethods pkPaymentRequestShippingContactUpdate  value =
withObjCPtr value $ \raw_value ->
    sendMsg pkPaymentRequestShippingContactUpdate (mkSelector "setShippingMethods:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- | @- errors@
errors :: IsPKPaymentRequestShippingContactUpdate pkPaymentRequestShippingContactUpdate => pkPaymentRequestShippingContactUpdate -> IO (Id NSArray)
errors pkPaymentRequestShippingContactUpdate  =
  sendMsg pkPaymentRequestShippingContactUpdate (mkSelector "errors") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- setErrors:@
setErrors :: (IsPKPaymentRequestShippingContactUpdate pkPaymentRequestShippingContactUpdate, IsNSArray value) => pkPaymentRequestShippingContactUpdate -> value -> IO ()
setErrors pkPaymentRequestShippingContactUpdate  value =
withObjCPtr value $ \raw_value ->
    sendMsg pkPaymentRequestShippingContactUpdate (mkSelector "setErrors:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @initWithErrors:paymentSummaryItems:shippingMethods:@
initWithErrors_paymentSummaryItems_shippingMethodsSelector :: Selector
initWithErrors_paymentSummaryItems_shippingMethodsSelector = mkSelector "initWithErrors:paymentSummaryItems:shippingMethods:"

-- | @Selector@ for @shippingMethods@
shippingMethodsSelector :: Selector
shippingMethodsSelector = mkSelector "shippingMethods"

-- | @Selector@ for @setShippingMethods:@
setShippingMethodsSelector :: Selector
setShippingMethodsSelector = mkSelector "setShippingMethods:"

-- | @Selector@ for @errors@
errorsSelector :: Selector
errorsSelector = mkSelector "errors"

-- | @Selector@ for @setErrors:@
setErrorsSelector :: Selector
setErrorsSelector = mkSelector "setErrors:"

