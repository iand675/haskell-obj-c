{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Generated bindings for @PKAutomaticReloadPaymentRequest@.
module ObjC.PassKit.PKAutomaticReloadPaymentRequest
  ( PKAutomaticReloadPaymentRequest
  , IsPKAutomaticReloadPaymentRequest(..)
  , init_
  , initWithPaymentDescription_automaticReloadBilling_managementURL
  , paymentDescription
  , setPaymentDescription
  , automaticReloadBilling
  , setAutomaticReloadBilling
  , billingAgreement
  , setBillingAgreement
  , managementURL
  , setManagementURL
  , tokenNotificationURL
  , setTokenNotificationURL
  , automaticReloadBillingSelector
  , billingAgreementSelector
  , initSelector
  , initWithPaymentDescription_automaticReloadBilling_managementURLSelector
  , managementURLSelector
  , paymentDescriptionSelector
  , setAutomaticReloadBillingSelector
  , setBillingAgreementSelector
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
init_ :: IsPKAutomaticReloadPaymentRequest pkAutomaticReloadPaymentRequest => pkAutomaticReloadPaymentRequest -> IO (Id PKAutomaticReloadPaymentRequest)
init_ pkAutomaticReloadPaymentRequest =
  sendOwnedMessage pkAutomaticReloadPaymentRequest initSelector

-- | @- initWithPaymentDescription:automaticReloadBilling:managementURL:@
initWithPaymentDescription_automaticReloadBilling_managementURL :: (IsPKAutomaticReloadPaymentRequest pkAutomaticReloadPaymentRequest, IsNSString paymentDescription, IsPKAutomaticReloadPaymentSummaryItem automaticReloadBilling, IsNSURL managementURL) => pkAutomaticReloadPaymentRequest -> paymentDescription -> automaticReloadBilling -> managementURL -> IO (Id PKAutomaticReloadPaymentRequest)
initWithPaymentDescription_automaticReloadBilling_managementURL pkAutomaticReloadPaymentRequest paymentDescription automaticReloadBilling managementURL =
  sendOwnedMessage pkAutomaticReloadPaymentRequest initWithPaymentDescription_automaticReloadBilling_managementURLSelector (toNSString paymentDescription) (toPKAutomaticReloadPaymentSummaryItem automaticReloadBilling) (toNSURL managementURL)

-- | @- paymentDescription@
paymentDescription :: IsPKAutomaticReloadPaymentRequest pkAutomaticReloadPaymentRequest => pkAutomaticReloadPaymentRequest -> IO (Id NSString)
paymentDescription pkAutomaticReloadPaymentRequest =
  sendMessage pkAutomaticReloadPaymentRequest paymentDescriptionSelector

-- | @- setPaymentDescription:@
setPaymentDescription :: (IsPKAutomaticReloadPaymentRequest pkAutomaticReloadPaymentRequest, IsNSString value) => pkAutomaticReloadPaymentRequest -> value -> IO ()
setPaymentDescription pkAutomaticReloadPaymentRequest value =
  sendMessage pkAutomaticReloadPaymentRequest setPaymentDescriptionSelector (toNSString value)

-- | @- automaticReloadBilling@
automaticReloadBilling :: IsPKAutomaticReloadPaymentRequest pkAutomaticReloadPaymentRequest => pkAutomaticReloadPaymentRequest -> IO (Id PKAutomaticReloadPaymentSummaryItem)
automaticReloadBilling pkAutomaticReloadPaymentRequest =
  sendMessage pkAutomaticReloadPaymentRequest automaticReloadBillingSelector

-- | @- setAutomaticReloadBilling:@
setAutomaticReloadBilling :: (IsPKAutomaticReloadPaymentRequest pkAutomaticReloadPaymentRequest, IsPKAutomaticReloadPaymentSummaryItem value) => pkAutomaticReloadPaymentRequest -> value -> IO ()
setAutomaticReloadBilling pkAutomaticReloadPaymentRequest value =
  sendMessage pkAutomaticReloadPaymentRequest setAutomaticReloadBillingSelector (toPKAutomaticReloadPaymentSummaryItem value)

-- | @- billingAgreement@
billingAgreement :: IsPKAutomaticReloadPaymentRequest pkAutomaticReloadPaymentRequest => pkAutomaticReloadPaymentRequest -> IO (Id NSString)
billingAgreement pkAutomaticReloadPaymentRequest =
  sendMessage pkAutomaticReloadPaymentRequest billingAgreementSelector

-- | @- setBillingAgreement:@
setBillingAgreement :: (IsPKAutomaticReloadPaymentRequest pkAutomaticReloadPaymentRequest, IsNSString value) => pkAutomaticReloadPaymentRequest -> value -> IO ()
setBillingAgreement pkAutomaticReloadPaymentRequest value =
  sendMessage pkAutomaticReloadPaymentRequest setBillingAgreementSelector (toNSString value)

-- | @- managementURL@
managementURL :: IsPKAutomaticReloadPaymentRequest pkAutomaticReloadPaymentRequest => pkAutomaticReloadPaymentRequest -> IO (Id NSURL)
managementURL pkAutomaticReloadPaymentRequest =
  sendMessage pkAutomaticReloadPaymentRequest managementURLSelector

-- | @- setManagementURL:@
setManagementURL :: (IsPKAutomaticReloadPaymentRequest pkAutomaticReloadPaymentRequest, IsNSURL value) => pkAutomaticReloadPaymentRequest -> value -> IO ()
setManagementURL pkAutomaticReloadPaymentRequest value =
  sendMessage pkAutomaticReloadPaymentRequest setManagementURLSelector (toNSURL value)

-- | @- tokenNotificationURL@
tokenNotificationURL :: IsPKAutomaticReloadPaymentRequest pkAutomaticReloadPaymentRequest => pkAutomaticReloadPaymentRequest -> IO (Id NSURL)
tokenNotificationURL pkAutomaticReloadPaymentRequest =
  sendMessage pkAutomaticReloadPaymentRequest tokenNotificationURLSelector

-- | @- setTokenNotificationURL:@
setTokenNotificationURL :: (IsPKAutomaticReloadPaymentRequest pkAutomaticReloadPaymentRequest, IsNSURL value) => pkAutomaticReloadPaymentRequest -> value -> IO ()
setTokenNotificationURL pkAutomaticReloadPaymentRequest value =
  sendMessage pkAutomaticReloadPaymentRequest setTokenNotificationURLSelector (toNSURL value)

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @init@
initSelector :: Selector '[] (Id PKAutomaticReloadPaymentRequest)
initSelector = mkSelector "init"

-- | @Selector@ for @initWithPaymentDescription:automaticReloadBilling:managementURL:@
initWithPaymentDescription_automaticReloadBilling_managementURLSelector :: Selector '[Id NSString, Id PKAutomaticReloadPaymentSummaryItem, Id NSURL] (Id PKAutomaticReloadPaymentRequest)
initWithPaymentDescription_automaticReloadBilling_managementURLSelector = mkSelector "initWithPaymentDescription:automaticReloadBilling:managementURL:"

-- | @Selector@ for @paymentDescription@
paymentDescriptionSelector :: Selector '[] (Id NSString)
paymentDescriptionSelector = mkSelector "paymentDescription"

-- | @Selector@ for @setPaymentDescription:@
setPaymentDescriptionSelector :: Selector '[Id NSString] ()
setPaymentDescriptionSelector = mkSelector "setPaymentDescription:"

-- | @Selector@ for @automaticReloadBilling@
automaticReloadBillingSelector :: Selector '[] (Id PKAutomaticReloadPaymentSummaryItem)
automaticReloadBillingSelector = mkSelector "automaticReloadBilling"

-- | @Selector@ for @setAutomaticReloadBilling:@
setAutomaticReloadBillingSelector :: Selector '[Id PKAutomaticReloadPaymentSummaryItem] ()
setAutomaticReloadBillingSelector = mkSelector "setAutomaticReloadBilling:"

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

