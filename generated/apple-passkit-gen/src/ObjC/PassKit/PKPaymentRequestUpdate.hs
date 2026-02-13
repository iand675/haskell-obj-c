{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE DataKinds #-}
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
  , multiTokenContexts
  , setMultiTokenContexts
  , recurringPaymentRequest
  , setRecurringPaymentRequest
  , automaticReloadPaymentRequest
  , setAutomaticReloadPaymentRequest
  , deferredPaymentRequest
  , setDeferredPaymentRequest
  , automaticReloadPaymentRequestSelector
  , deferredPaymentRequestSelector
  , initWithPaymentSummaryItemsSelector
  , multiTokenContextsSelector
  , paymentSummaryItemsSelector
  , recurringPaymentRequestSelector
  , setAutomaticReloadPaymentRequestSelector
  , setDeferredPaymentRequestSelector
  , setMultiTokenContextsSelector
  , setPaymentSummaryItemsSelector
  , setRecurringPaymentRequestSelector
  , setShippingMethodsSelector
  , setStatusSelector
  , shippingMethodsSelector
  , statusSelector

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

import Foreign.Ptr (Ptr, FunPtr)
import Foreign.C.Types

import ObjC.Runtime.Types
import ObjC.Runtime.Message (sendMessage, sendOwnedMessage, sendClassMessage, sendOwnedClassMessage)
import ObjC.Runtime.Selector (mkSelector)
import ObjC.Runtime.Class (getRequiredClass)

import ObjC.PassKit.Internal.Classes
import ObjC.PassKit.Internal.Enums
import ObjC.Foundation.Internal.Classes

-- | @- initWithPaymentSummaryItems:@
initWithPaymentSummaryItems :: (IsPKPaymentRequestUpdate pkPaymentRequestUpdate, IsNSArray paymentSummaryItems) => pkPaymentRequestUpdate -> paymentSummaryItems -> IO (Id PKPaymentRequestUpdate)
initWithPaymentSummaryItems pkPaymentRequestUpdate paymentSummaryItems =
  sendOwnedMessage pkPaymentRequestUpdate initWithPaymentSummaryItemsSelector (toNSArray paymentSummaryItems)

-- | @- status@
status :: IsPKPaymentRequestUpdate pkPaymentRequestUpdate => pkPaymentRequestUpdate -> IO PKPaymentAuthorizationStatus
status pkPaymentRequestUpdate =
  sendMessage pkPaymentRequestUpdate statusSelector

-- | @- setStatus:@
setStatus :: IsPKPaymentRequestUpdate pkPaymentRequestUpdate => pkPaymentRequestUpdate -> PKPaymentAuthorizationStatus -> IO ()
setStatus pkPaymentRequestUpdate value =
  sendMessage pkPaymentRequestUpdate setStatusSelector value

-- | @- paymentSummaryItems@
paymentSummaryItems :: IsPKPaymentRequestUpdate pkPaymentRequestUpdate => pkPaymentRequestUpdate -> IO (Id NSArray)
paymentSummaryItems pkPaymentRequestUpdate =
  sendMessage pkPaymentRequestUpdate paymentSummaryItemsSelector

-- | @- setPaymentSummaryItems:@
setPaymentSummaryItems :: (IsPKPaymentRequestUpdate pkPaymentRequestUpdate, IsNSArray value) => pkPaymentRequestUpdate -> value -> IO ()
setPaymentSummaryItems pkPaymentRequestUpdate value =
  sendMessage pkPaymentRequestUpdate setPaymentSummaryItemsSelector (toNSArray value)

-- | @- shippingMethods@
shippingMethods :: IsPKPaymentRequestUpdate pkPaymentRequestUpdate => pkPaymentRequestUpdate -> IO (Id NSArray)
shippingMethods pkPaymentRequestUpdate =
  sendMessage pkPaymentRequestUpdate shippingMethodsSelector

-- | @- setShippingMethods:@
setShippingMethods :: (IsPKPaymentRequestUpdate pkPaymentRequestUpdate, IsNSArray value) => pkPaymentRequestUpdate -> value -> IO ()
setShippingMethods pkPaymentRequestUpdate value =
  sendMessage pkPaymentRequestUpdate setShippingMethodsSelector (toNSArray value)

-- | @- multiTokenContexts@
multiTokenContexts :: IsPKPaymentRequestUpdate pkPaymentRequestUpdate => pkPaymentRequestUpdate -> IO (Id NSArray)
multiTokenContexts pkPaymentRequestUpdate =
  sendMessage pkPaymentRequestUpdate multiTokenContextsSelector

-- | @- setMultiTokenContexts:@
setMultiTokenContexts :: (IsPKPaymentRequestUpdate pkPaymentRequestUpdate, IsNSArray value) => pkPaymentRequestUpdate -> value -> IO ()
setMultiTokenContexts pkPaymentRequestUpdate value =
  sendMessage pkPaymentRequestUpdate setMultiTokenContextsSelector (toNSArray value)

-- | @- recurringPaymentRequest@
recurringPaymentRequest :: IsPKPaymentRequestUpdate pkPaymentRequestUpdate => pkPaymentRequestUpdate -> IO (Id PKRecurringPaymentRequest)
recurringPaymentRequest pkPaymentRequestUpdate =
  sendMessage pkPaymentRequestUpdate recurringPaymentRequestSelector

-- | @- setRecurringPaymentRequest:@
setRecurringPaymentRequest :: (IsPKPaymentRequestUpdate pkPaymentRequestUpdate, IsPKRecurringPaymentRequest value) => pkPaymentRequestUpdate -> value -> IO ()
setRecurringPaymentRequest pkPaymentRequestUpdate value =
  sendMessage pkPaymentRequestUpdate setRecurringPaymentRequestSelector (toPKRecurringPaymentRequest value)

-- | @- automaticReloadPaymentRequest@
automaticReloadPaymentRequest :: IsPKPaymentRequestUpdate pkPaymentRequestUpdate => pkPaymentRequestUpdate -> IO (Id PKAutomaticReloadPaymentRequest)
automaticReloadPaymentRequest pkPaymentRequestUpdate =
  sendMessage pkPaymentRequestUpdate automaticReloadPaymentRequestSelector

-- | @- setAutomaticReloadPaymentRequest:@
setAutomaticReloadPaymentRequest :: (IsPKPaymentRequestUpdate pkPaymentRequestUpdate, IsPKAutomaticReloadPaymentRequest value) => pkPaymentRequestUpdate -> value -> IO ()
setAutomaticReloadPaymentRequest pkPaymentRequestUpdate value =
  sendMessage pkPaymentRequestUpdate setAutomaticReloadPaymentRequestSelector (toPKAutomaticReloadPaymentRequest value)

-- | @- deferredPaymentRequest@
deferredPaymentRequest :: IsPKPaymentRequestUpdate pkPaymentRequestUpdate => pkPaymentRequestUpdate -> IO (Id PKDeferredPaymentRequest)
deferredPaymentRequest pkPaymentRequestUpdate =
  sendMessage pkPaymentRequestUpdate deferredPaymentRequestSelector

-- | @- setDeferredPaymentRequest:@
setDeferredPaymentRequest :: (IsPKPaymentRequestUpdate pkPaymentRequestUpdate, IsPKDeferredPaymentRequest value) => pkPaymentRequestUpdate -> value -> IO ()
setDeferredPaymentRequest pkPaymentRequestUpdate value =
  sendMessage pkPaymentRequestUpdate setDeferredPaymentRequestSelector (toPKDeferredPaymentRequest value)

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @initWithPaymentSummaryItems:@
initWithPaymentSummaryItemsSelector :: Selector '[Id NSArray] (Id PKPaymentRequestUpdate)
initWithPaymentSummaryItemsSelector = mkSelector "initWithPaymentSummaryItems:"

-- | @Selector@ for @status@
statusSelector :: Selector '[] PKPaymentAuthorizationStatus
statusSelector = mkSelector "status"

-- | @Selector@ for @setStatus:@
setStatusSelector :: Selector '[PKPaymentAuthorizationStatus] ()
setStatusSelector = mkSelector "setStatus:"

-- | @Selector@ for @paymentSummaryItems@
paymentSummaryItemsSelector :: Selector '[] (Id NSArray)
paymentSummaryItemsSelector = mkSelector "paymentSummaryItems"

-- | @Selector@ for @setPaymentSummaryItems:@
setPaymentSummaryItemsSelector :: Selector '[Id NSArray] ()
setPaymentSummaryItemsSelector = mkSelector "setPaymentSummaryItems:"

-- | @Selector@ for @shippingMethods@
shippingMethodsSelector :: Selector '[] (Id NSArray)
shippingMethodsSelector = mkSelector "shippingMethods"

-- | @Selector@ for @setShippingMethods:@
setShippingMethodsSelector :: Selector '[Id NSArray] ()
setShippingMethodsSelector = mkSelector "setShippingMethods:"

-- | @Selector@ for @multiTokenContexts@
multiTokenContextsSelector :: Selector '[] (Id NSArray)
multiTokenContextsSelector = mkSelector "multiTokenContexts"

-- | @Selector@ for @setMultiTokenContexts:@
setMultiTokenContextsSelector :: Selector '[Id NSArray] ()
setMultiTokenContextsSelector = mkSelector "setMultiTokenContexts:"

-- | @Selector@ for @recurringPaymentRequest@
recurringPaymentRequestSelector :: Selector '[] (Id PKRecurringPaymentRequest)
recurringPaymentRequestSelector = mkSelector "recurringPaymentRequest"

-- | @Selector@ for @setRecurringPaymentRequest:@
setRecurringPaymentRequestSelector :: Selector '[Id PKRecurringPaymentRequest] ()
setRecurringPaymentRequestSelector = mkSelector "setRecurringPaymentRequest:"

-- | @Selector@ for @automaticReloadPaymentRequest@
automaticReloadPaymentRequestSelector :: Selector '[] (Id PKAutomaticReloadPaymentRequest)
automaticReloadPaymentRequestSelector = mkSelector "automaticReloadPaymentRequest"

-- | @Selector@ for @setAutomaticReloadPaymentRequest:@
setAutomaticReloadPaymentRequestSelector :: Selector '[Id PKAutomaticReloadPaymentRequest] ()
setAutomaticReloadPaymentRequestSelector = mkSelector "setAutomaticReloadPaymentRequest:"

-- | @Selector@ for @deferredPaymentRequest@
deferredPaymentRequestSelector :: Selector '[] (Id PKDeferredPaymentRequest)
deferredPaymentRequestSelector = mkSelector "deferredPaymentRequest"

-- | @Selector@ for @setDeferredPaymentRequest:@
setDeferredPaymentRequestSelector :: Selector '[Id PKDeferredPaymentRequest] ()
setDeferredPaymentRequestSelector = mkSelector "setDeferredPaymentRequest:"

