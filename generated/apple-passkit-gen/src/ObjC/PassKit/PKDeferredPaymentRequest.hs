{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Generated bindings for @PKDeferredPaymentRequest@.
module ObjC.PassKit.PKDeferredPaymentRequest
  ( PKDeferredPaymentRequest
  , IsPKDeferredPaymentRequest(..)
  , init_
  , initWithPaymentDescription_deferredBilling_managementURL
  , paymentDescription
  , setPaymentDescription
  , deferredBilling
  , setDeferredBilling
  , billingAgreement
  , setBillingAgreement
  , managementURL
  , setManagementURL
  , tokenNotificationURL
  , setTokenNotificationURL
  , freeCancellationDate
  , setFreeCancellationDate
  , freeCancellationDateTimeZone
  , setFreeCancellationDateTimeZone
  , billingAgreementSelector
  , deferredBillingSelector
  , freeCancellationDateSelector
  , freeCancellationDateTimeZoneSelector
  , initSelector
  , initWithPaymentDescription_deferredBilling_managementURLSelector
  , managementURLSelector
  , paymentDescriptionSelector
  , setBillingAgreementSelector
  , setDeferredBillingSelector
  , setFreeCancellationDateSelector
  , setFreeCancellationDateTimeZoneSelector
  , setManagementURLSelector
  , setPaymentDescriptionSelector
  , setTokenNotificationURLSelector
  , tokenNotificationURLSelector


  ) where

import Foreign.Ptr (Ptr, FunPtr)
import Foreign.C.Types

import ObjC.Runtime.Types
import ObjC.Runtime.Message (sendMessage, sendOwnedMessage, sendClassMessage, sendOwnedClassMessage)
import ObjC.Runtime.Selector (mkSelector)
import ObjC.Runtime.Class (getRequiredClass)

import ObjC.PassKit.Internal.Classes
import ObjC.Foundation.Internal.Classes

-- | @- init@
init_ :: IsPKDeferredPaymentRequest pkDeferredPaymentRequest => pkDeferredPaymentRequest -> IO (Id PKDeferredPaymentRequest)
init_ pkDeferredPaymentRequest =
  sendOwnedMessage pkDeferredPaymentRequest initSelector

-- | @- initWithPaymentDescription:deferredBilling:managementURL:@
initWithPaymentDescription_deferredBilling_managementURL :: (IsPKDeferredPaymentRequest pkDeferredPaymentRequest, IsNSString paymentDescription, IsPKDeferredPaymentSummaryItem deferredBilling, IsNSURL managementURL) => pkDeferredPaymentRequest -> paymentDescription -> deferredBilling -> managementURL -> IO (Id PKDeferredPaymentRequest)
initWithPaymentDescription_deferredBilling_managementURL pkDeferredPaymentRequest paymentDescription deferredBilling managementURL =
  sendOwnedMessage pkDeferredPaymentRequest initWithPaymentDescription_deferredBilling_managementURLSelector (toNSString paymentDescription) (toPKDeferredPaymentSummaryItem deferredBilling) (toNSURL managementURL)

-- | @- paymentDescription@
paymentDescription :: IsPKDeferredPaymentRequest pkDeferredPaymentRequest => pkDeferredPaymentRequest -> IO (Id NSString)
paymentDescription pkDeferredPaymentRequest =
  sendMessage pkDeferredPaymentRequest paymentDescriptionSelector

-- | @- setPaymentDescription:@
setPaymentDescription :: (IsPKDeferredPaymentRequest pkDeferredPaymentRequest, IsNSString value) => pkDeferredPaymentRequest -> value -> IO ()
setPaymentDescription pkDeferredPaymentRequest value =
  sendMessage pkDeferredPaymentRequest setPaymentDescriptionSelector (toNSString value)

-- | @- deferredBilling@
deferredBilling :: IsPKDeferredPaymentRequest pkDeferredPaymentRequest => pkDeferredPaymentRequest -> IO (Id PKDeferredPaymentSummaryItem)
deferredBilling pkDeferredPaymentRequest =
  sendMessage pkDeferredPaymentRequest deferredBillingSelector

-- | @- setDeferredBilling:@
setDeferredBilling :: (IsPKDeferredPaymentRequest pkDeferredPaymentRequest, IsPKDeferredPaymentSummaryItem value) => pkDeferredPaymentRequest -> value -> IO ()
setDeferredBilling pkDeferredPaymentRequest value =
  sendMessage pkDeferredPaymentRequest setDeferredBillingSelector (toPKDeferredPaymentSummaryItem value)

-- | @- billingAgreement@
billingAgreement :: IsPKDeferredPaymentRequest pkDeferredPaymentRequest => pkDeferredPaymentRequest -> IO (Id NSString)
billingAgreement pkDeferredPaymentRequest =
  sendMessage pkDeferredPaymentRequest billingAgreementSelector

-- | @- setBillingAgreement:@
setBillingAgreement :: (IsPKDeferredPaymentRequest pkDeferredPaymentRequest, IsNSString value) => pkDeferredPaymentRequest -> value -> IO ()
setBillingAgreement pkDeferredPaymentRequest value =
  sendMessage pkDeferredPaymentRequest setBillingAgreementSelector (toNSString value)

-- | @- managementURL@
managementURL :: IsPKDeferredPaymentRequest pkDeferredPaymentRequest => pkDeferredPaymentRequest -> IO (Id NSURL)
managementURL pkDeferredPaymentRequest =
  sendMessage pkDeferredPaymentRequest managementURLSelector

-- | @- setManagementURL:@
setManagementURL :: (IsPKDeferredPaymentRequest pkDeferredPaymentRequest, IsNSURL value) => pkDeferredPaymentRequest -> value -> IO ()
setManagementURL pkDeferredPaymentRequest value =
  sendMessage pkDeferredPaymentRequest setManagementURLSelector (toNSURL value)

-- | @- tokenNotificationURL@
tokenNotificationURL :: IsPKDeferredPaymentRequest pkDeferredPaymentRequest => pkDeferredPaymentRequest -> IO (Id NSURL)
tokenNotificationURL pkDeferredPaymentRequest =
  sendMessage pkDeferredPaymentRequest tokenNotificationURLSelector

-- | @- setTokenNotificationURL:@
setTokenNotificationURL :: (IsPKDeferredPaymentRequest pkDeferredPaymentRequest, IsNSURL value) => pkDeferredPaymentRequest -> value -> IO ()
setTokenNotificationURL pkDeferredPaymentRequest value =
  sendMessage pkDeferredPaymentRequest setTokenNotificationURLSelector (toNSURL value)

-- | @- freeCancellationDate@
freeCancellationDate :: IsPKDeferredPaymentRequest pkDeferredPaymentRequest => pkDeferredPaymentRequest -> IO (Id NSDate)
freeCancellationDate pkDeferredPaymentRequest =
  sendMessage pkDeferredPaymentRequest freeCancellationDateSelector

-- | @- setFreeCancellationDate:@
setFreeCancellationDate :: (IsPKDeferredPaymentRequest pkDeferredPaymentRequest, IsNSDate value) => pkDeferredPaymentRequest -> value -> IO ()
setFreeCancellationDate pkDeferredPaymentRequest value =
  sendMessage pkDeferredPaymentRequest setFreeCancellationDateSelector (toNSDate value)

-- | @- freeCancellationDateTimeZone@
freeCancellationDateTimeZone :: IsPKDeferredPaymentRequest pkDeferredPaymentRequest => pkDeferredPaymentRequest -> IO (Id NSTimeZone)
freeCancellationDateTimeZone pkDeferredPaymentRequest =
  sendMessage pkDeferredPaymentRequest freeCancellationDateTimeZoneSelector

-- | @- setFreeCancellationDateTimeZone:@
setFreeCancellationDateTimeZone :: (IsPKDeferredPaymentRequest pkDeferredPaymentRequest, IsNSTimeZone value) => pkDeferredPaymentRequest -> value -> IO ()
setFreeCancellationDateTimeZone pkDeferredPaymentRequest value =
  sendMessage pkDeferredPaymentRequest setFreeCancellationDateTimeZoneSelector (toNSTimeZone value)

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @init@
initSelector :: Selector '[] (Id PKDeferredPaymentRequest)
initSelector = mkSelector "init"

-- | @Selector@ for @initWithPaymentDescription:deferredBilling:managementURL:@
initWithPaymentDescription_deferredBilling_managementURLSelector :: Selector '[Id NSString, Id PKDeferredPaymentSummaryItem, Id NSURL] (Id PKDeferredPaymentRequest)
initWithPaymentDescription_deferredBilling_managementURLSelector = mkSelector "initWithPaymentDescription:deferredBilling:managementURL:"

-- | @Selector@ for @paymentDescription@
paymentDescriptionSelector :: Selector '[] (Id NSString)
paymentDescriptionSelector = mkSelector "paymentDescription"

-- | @Selector@ for @setPaymentDescription:@
setPaymentDescriptionSelector :: Selector '[Id NSString] ()
setPaymentDescriptionSelector = mkSelector "setPaymentDescription:"

-- | @Selector@ for @deferredBilling@
deferredBillingSelector :: Selector '[] (Id PKDeferredPaymentSummaryItem)
deferredBillingSelector = mkSelector "deferredBilling"

-- | @Selector@ for @setDeferredBilling:@
setDeferredBillingSelector :: Selector '[Id PKDeferredPaymentSummaryItem] ()
setDeferredBillingSelector = mkSelector "setDeferredBilling:"

-- | @Selector@ for @billingAgreement@
billingAgreementSelector :: Selector '[] (Id NSString)
billingAgreementSelector = mkSelector "billingAgreement"

-- | @Selector@ for @setBillingAgreement:@
setBillingAgreementSelector :: Selector '[Id NSString] ()
setBillingAgreementSelector = mkSelector "setBillingAgreement:"

-- | @Selector@ for @managementURL@
managementURLSelector :: Selector '[] (Id NSURL)
managementURLSelector = mkSelector "managementURL"

-- | @Selector@ for @setManagementURL:@
setManagementURLSelector :: Selector '[Id NSURL] ()
setManagementURLSelector = mkSelector "setManagementURL:"

-- | @Selector@ for @tokenNotificationURL@
tokenNotificationURLSelector :: Selector '[] (Id NSURL)
tokenNotificationURLSelector = mkSelector "tokenNotificationURL"

-- | @Selector@ for @setTokenNotificationURL:@
setTokenNotificationURLSelector :: Selector '[Id NSURL] ()
setTokenNotificationURLSelector = mkSelector "setTokenNotificationURL:"

-- | @Selector@ for @freeCancellationDate@
freeCancellationDateSelector :: Selector '[] (Id NSDate)
freeCancellationDateSelector = mkSelector "freeCancellationDate"

-- | @Selector@ for @setFreeCancellationDate:@
setFreeCancellationDateSelector :: Selector '[Id NSDate] ()
setFreeCancellationDateSelector = mkSelector "setFreeCancellationDate:"

-- | @Selector@ for @freeCancellationDateTimeZone@
freeCancellationDateTimeZoneSelector :: Selector '[] (Id NSTimeZone)
freeCancellationDateTimeZoneSelector = mkSelector "freeCancellationDateTimeZone"

-- | @Selector@ for @setFreeCancellationDateTimeZone:@
setFreeCancellationDateTimeZoneSelector :: Selector '[Id NSTimeZone] ()
setFreeCancellationDateTimeZoneSelector = mkSelector "setFreeCancellationDateTimeZone:"

