{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Generated bindings for @PKPaymentRequestUpdate@.
module ObjC.PassKit.PKPaymentRequestUpdate
  ( PKPaymentRequestUpdate
  , IsPKPaymentRequestUpdate(..)
  , initWithPaymentSummaryItems
  , status
  , setStatus
  , paymentSummaryItems
  , setPaymentSummaryItems
  , shippingMethods
  , setShippingMethods
  , initWithPaymentSummaryItemsSelector
  , statusSelector
  , setStatusSelector
  , paymentSummaryItemsSelector
  , setPaymentSummaryItemsSelector
  , shippingMethodsSelector
  , setShippingMethodsSelector

  -- * Enum types
  , PKPaymentAuthorizationStatus(PKPaymentAuthorizationStatus)
  , pattern PKPaymentAuthorizationStatusSuccess
  , pattern PKPaymentAuthorizationStatusFailure
  , pattern PKPaymentAuthorizationStatusInvalidBillingPostalAddress
  , pattern PKPaymentAuthorizationStatusInvalidShippingPostalAddress
  , pattern PKPaymentAuthorizationStatusInvalidShippingContact
  , pattern PKPaymentAuthorizationStatusPINRequired
  , pattern PKPaymentAuthorizationStatusPINIncorrect
  , pattern PKPaymentAuthorizationStatusPINLockout

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
import ObjC.PassKit.Internal.Enums
import ObjC.Foundation.Internal.Classes

-- | @- initWithPaymentSummaryItems:@
initWithPaymentSummaryItems :: (IsPKPaymentRequestUpdate pkPaymentRequestUpdate, IsNSArray paymentSummaryItems) => pkPaymentRequestUpdate -> paymentSummaryItems -> IO (Id PKPaymentRequestUpdate)
initWithPaymentSummaryItems pkPaymentRequestUpdate  paymentSummaryItems =
withObjCPtr paymentSummaryItems $ \raw_paymentSummaryItems ->
    sendMsg pkPaymentRequestUpdate (mkSelector "initWithPaymentSummaryItems:") (retPtr retVoid) [argPtr (castPtr raw_paymentSummaryItems :: Ptr ())] >>= ownedObject . castPtr

-- | @- status@
status :: IsPKPaymentRequestUpdate pkPaymentRequestUpdate => pkPaymentRequestUpdate -> IO PKPaymentAuthorizationStatus
status pkPaymentRequestUpdate  =
  fmap (coerce :: CLong -> PKPaymentAuthorizationStatus) $ sendMsg pkPaymentRequestUpdate (mkSelector "status") retCLong []

-- | @- setStatus:@
setStatus :: IsPKPaymentRequestUpdate pkPaymentRequestUpdate => pkPaymentRequestUpdate -> PKPaymentAuthorizationStatus -> IO ()
setStatus pkPaymentRequestUpdate  value =
  sendMsg pkPaymentRequestUpdate (mkSelector "setStatus:") retVoid [argCLong (coerce value)]

-- | @- paymentSummaryItems@
paymentSummaryItems :: IsPKPaymentRequestUpdate pkPaymentRequestUpdate => pkPaymentRequestUpdate -> IO (Id NSArray)
paymentSummaryItems pkPaymentRequestUpdate  =
  sendMsg pkPaymentRequestUpdate (mkSelector "paymentSummaryItems") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- setPaymentSummaryItems:@
setPaymentSummaryItems :: (IsPKPaymentRequestUpdate pkPaymentRequestUpdate, IsNSArray value) => pkPaymentRequestUpdate -> value -> IO ()
setPaymentSummaryItems pkPaymentRequestUpdate  value =
withObjCPtr value $ \raw_value ->
    sendMsg pkPaymentRequestUpdate (mkSelector "setPaymentSummaryItems:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- | @- shippingMethods@
shippingMethods :: IsPKPaymentRequestUpdate pkPaymentRequestUpdate => pkPaymentRequestUpdate -> IO (Id NSArray)
shippingMethods pkPaymentRequestUpdate  =
  sendMsg pkPaymentRequestUpdate (mkSelector "shippingMethods") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- setShippingMethods:@
setShippingMethods :: (IsPKPaymentRequestUpdate pkPaymentRequestUpdate, IsNSArray value) => pkPaymentRequestUpdate -> value -> IO ()
setShippingMethods pkPaymentRequestUpdate  value =
withObjCPtr value $ \raw_value ->
    sendMsg pkPaymentRequestUpdate (mkSelector "setShippingMethods:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @initWithPaymentSummaryItems:@
initWithPaymentSummaryItemsSelector :: Selector
initWithPaymentSummaryItemsSelector = mkSelector "initWithPaymentSummaryItems:"

-- | @Selector@ for @status@
statusSelector :: Selector
statusSelector = mkSelector "status"

-- | @Selector@ for @setStatus:@
setStatusSelector :: Selector
setStatusSelector = mkSelector "setStatus:"

-- | @Selector@ for @paymentSummaryItems@
paymentSummaryItemsSelector :: Selector
paymentSummaryItemsSelector = mkSelector "paymentSummaryItems"

-- | @Selector@ for @setPaymentSummaryItems:@
setPaymentSummaryItemsSelector :: Selector
setPaymentSummaryItemsSelector = mkSelector "setPaymentSummaryItems:"

-- | @Selector@ for @shippingMethods@
shippingMethodsSelector :: Selector
shippingMethodsSelector = mkSelector "shippingMethods"

-- | @Selector@ for @setShippingMethods:@
setShippingMethodsSelector :: Selector
setShippingMethodsSelector = mkSelector "setShippingMethods:"

