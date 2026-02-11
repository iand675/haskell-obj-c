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
  , initWithErrors_paymentSummaryItemsSelector
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

-- | @- initWithErrors:paymentSummaryItems:@
initWithErrors_paymentSummaryItems :: (IsPKPaymentRequestPaymentMethodUpdate pkPaymentRequestPaymentMethodUpdate, IsNSArray errors, IsNSArray paymentSummaryItems) => pkPaymentRequestPaymentMethodUpdate -> errors -> paymentSummaryItems -> IO (Id PKPaymentRequestPaymentMethodUpdate)
initWithErrors_paymentSummaryItems pkPaymentRequestPaymentMethodUpdate  errors paymentSummaryItems =
withObjCPtr errors $ \raw_errors ->
  withObjCPtr paymentSummaryItems $ \raw_paymentSummaryItems ->
      sendMsg pkPaymentRequestPaymentMethodUpdate (mkSelector "initWithErrors:paymentSummaryItems:") (retPtr retVoid) [argPtr (castPtr raw_errors :: Ptr ()), argPtr (castPtr raw_paymentSummaryItems :: Ptr ())] >>= ownedObject . castPtr

-- | @- errors@
errors :: IsPKPaymentRequestPaymentMethodUpdate pkPaymentRequestPaymentMethodUpdate => pkPaymentRequestPaymentMethodUpdate -> IO (Id NSArray)
errors pkPaymentRequestPaymentMethodUpdate  =
  sendMsg pkPaymentRequestPaymentMethodUpdate (mkSelector "errors") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- setErrors:@
setErrors :: (IsPKPaymentRequestPaymentMethodUpdate pkPaymentRequestPaymentMethodUpdate, IsNSArray value) => pkPaymentRequestPaymentMethodUpdate -> value -> IO ()
setErrors pkPaymentRequestPaymentMethodUpdate  value =
withObjCPtr value $ \raw_value ->
    sendMsg pkPaymentRequestPaymentMethodUpdate (mkSelector "setErrors:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @initWithErrors:paymentSummaryItems:@
initWithErrors_paymentSummaryItemsSelector :: Selector
initWithErrors_paymentSummaryItemsSelector = mkSelector "initWithErrors:paymentSummaryItems:"

-- | @Selector@ for @errors@
errorsSelector :: Selector
errorsSelector = mkSelector "errors"

-- | @Selector@ for @setErrors:@
setErrorsSelector :: Selector
setErrorsSelector = mkSelector "setErrors:"

