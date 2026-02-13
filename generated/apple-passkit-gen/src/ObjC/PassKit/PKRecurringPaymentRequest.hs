{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Generated bindings for @PKRecurringPaymentRequest@.
module ObjC.PassKit.PKRecurringPaymentRequest
  ( PKRecurringPaymentRequest
  , IsPKRecurringPaymentRequest(..)
  , init_
  , initWithPaymentDescription_regularBilling_managementURL
  , paymentDescription
  , setPaymentDescription
  , regularBilling
  , setRegularBilling
  , trialBilling
  , setTrialBilling
  , billingAgreement
  , setBillingAgreement
  , managementURL
  , setManagementURL
  , tokenNotificationURL
  , setTokenNotificationURL
  , billingAgreementSelector
  , initSelector
  , initWithPaymentDescription_regularBilling_managementURLSelector
  , managementURLSelector
  , paymentDescriptionSelector
  , regularBillingSelector
  , setBillingAgreementSelector
  , setManagementURLSelector
  , setPaymentDescriptionSelector
  , setRegularBillingSelector
  , setTokenNotificationURLSelector
  , setTrialBillingSelector
  , tokenNotificationURLSelector
  , trialBillingSelector


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
init_ :: IsPKRecurringPaymentRequest pkRecurringPaymentRequest => pkRecurringPaymentRequest -> IO (Id PKRecurringPaymentRequest)
init_ pkRecurringPaymentRequest =
  sendOwnedMessage pkRecurringPaymentRequest initSelector

-- | @- initWithPaymentDescription:regularBilling:managementURL:@
initWithPaymentDescription_regularBilling_managementURL :: (IsPKRecurringPaymentRequest pkRecurringPaymentRequest, IsNSString paymentDescription, IsPKRecurringPaymentSummaryItem regularBilling, IsNSURL managementURL) => pkRecurringPaymentRequest -> paymentDescription -> regularBilling -> managementURL -> IO (Id PKRecurringPaymentRequest)
initWithPaymentDescription_regularBilling_managementURL pkRecurringPaymentRequest paymentDescription regularBilling managementURL =
  sendOwnedMessage pkRecurringPaymentRequest initWithPaymentDescription_regularBilling_managementURLSelector (toNSString paymentDescription) (toPKRecurringPaymentSummaryItem regularBilling) (toNSURL managementURL)

-- | @- paymentDescription@
paymentDescription :: IsPKRecurringPaymentRequest pkRecurringPaymentRequest => pkRecurringPaymentRequest -> IO (Id NSString)
paymentDescription pkRecurringPaymentRequest =
  sendMessage pkRecurringPaymentRequest paymentDescriptionSelector

-- | @- setPaymentDescription:@
setPaymentDescription :: (IsPKRecurringPaymentRequest pkRecurringPaymentRequest, IsNSString value) => pkRecurringPaymentRequest -> value -> IO ()
setPaymentDescription pkRecurringPaymentRequest value =
  sendMessage pkRecurringPaymentRequest setPaymentDescriptionSelector (toNSString value)

-- | @- regularBilling@
regularBilling :: IsPKRecurringPaymentRequest pkRecurringPaymentRequest => pkRecurringPaymentRequest -> IO (Id PKRecurringPaymentSummaryItem)
regularBilling pkRecurringPaymentRequest =
  sendMessage pkRecurringPaymentRequest regularBillingSelector

-- | @- setRegularBilling:@
setRegularBilling :: (IsPKRecurringPaymentRequest pkRecurringPaymentRequest, IsPKRecurringPaymentSummaryItem value) => pkRecurringPaymentRequest -> value -> IO ()
setRegularBilling pkRecurringPaymentRequest value =
  sendMessage pkRecurringPaymentRequest setRegularBillingSelector (toPKRecurringPaymentSummaryItem value)

-- | @- trialBilling@
trialBilling :: IsPKRecurringPaymentRequest pkRecurringPaymentRequest => pkRecurringPaymentRequest -> IO (Id PKRecurringPaymentSummaryItem)
trialBilling pkRecurringPaymentRequest =
  sendMessage pkRecurringPaymentRequest trialBillingSelector

-- | @- setTrialBilling:@
setTrialBilling :: (IsPKRecurringPaymentRequest pkRecurringPaymentRequest, IsPKRecurringPaymentSummaryItem value) => pkRecurringPaymentRequest -> value -> IO ()
setTrialBilling pkRecurringPaymentRequest value =
  sendMessage pkRecurringPaymentRequest setTrialBillingSelector (toPKRecurringPaymentSummaryItem value)

-- | @- billingAgreement@
billingAgreement :: IsPKRecurringPaymentRequest pkRecurringPaymentRequest => pkRecurringPaymentRequest -> IO (Id NSString)
billingAgreement pkRecurringPaymentRequest =
  sendMessage pkRecurringPaymentRequest billingAgreementSelector

-- | @- setBillingAgreement:@
setBillingAgreement :: (IsPKRecurringPaymentRequest pkRecurringPaymentRequest, IsNSString value) => pkRecurringPaymentRequest -> value -> IO ()
setBillingAgreement pkRecurringPaymentRequest value =
  sendMessage pkRecurringPaymentRequest setBillingAgreementSelector (toNSString value)

-- | @- managementURL@
managementURL :: IsPKRecurringPaymentRequest pkRecurringPaymentRequest => pkRecurringPaymentRequest -> IO (Id NSURL)
managementURL pkRecurringPaymentRequest =
  sendMessage pkRecurringPaymentRequest managementURLSelector

-- | @- setManagementURL:@
setManagementURL :: (IsPKRecurringPaymentRequest pkRecurringPaymentRequest, IsNSURL value) => pkRecurringPaymentRequest -> value -> IO ()
setManagementURL pkRecurringPaymentRequest value =
  sendMessage pkRecurringPaymentRequest setManagementURLSelector (toNSURL value)

-- | @- tokenNotificationURL@
tokenNotificationURL :: IsPKRecurringPaymentRequest pkRecurringPaymentRequest => pkRecurringPaymentRequest -> IO (Id NSURL)
tokenNotificationURL pkRecurringPaymentRequest =
  sendMessage pkRecurringPaymentRequest tokenNotificationURLSelector

-- | @- setTokenNotificationURL:@
setTokenNotificationURL :: (IsPKRecurringPaymentRequest pkRecurringPaymentRequest, IsNSURL value) => pkRecurringPaymentRequest -> value -> IO ()
setTokenNotificationURL pkRecurringPaymentRequest value =
  sendMessage pkRecurringPaymentRequest setTokenNotificationURLSelector (toNSURL value)

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @init@
initSelector :: Selector '[] (Id PKRecurringPaymentRequest)
initSelector = mkSelector "init"

-- | @Selector@ for @initWithPaymentDescription:regularBilling:managementURL:@
initWithPaymentDescription_regularBilling_managementURLSelector :: Selector '[Id NSString, Id PKRecurringPaymentSummaryItem, Id NSURL] (Id PKRecurringPaymentRequest)
initWithPaymentDescription_regularBilling_managementURLSelector = mkSelector "initWithPaymentDescription:regularBilling:managementURL:"

-- | @Selector@ for @paymentDescription@
paymentDescriptionSelector :: Selector '[] (Id NSString)
paymentDescriptionSelector = mkSelector "paymentDescription"

-- | @Selector@ for @setPaymentDescription:@
setPaymentDescriptionSelector :: Selector '[Id NSString] ()
setPaymentDescriptionSelector = mkSelector "setPaymentDescription:"

-- | @Selector@ for @regularBilling@
regularBillingSelector :: Selector '[] (Id PKRecurringPaymentSummaryItem)
regularBillingSelector = mkSelector "regularBilling"

-- | @Selector@ for @setRegularBilling:@
setRegularBillingSelector :: Selector '[Id PKRecurringPaymentSummaryItem] ()
setRegularBillingSelector = mkSelector "setRegularBilling:"

-- | @Selector@ for @trialBilling@
trialBillingSelector :: Selector '[] (Id PKRecurringPaymentSummaryItem)
trialBillingSelector = mkSelector "trialBilling"

-- | @Selector@ for @setTrialBilling:@
setTrialBillingSelector :: Selector '[Id PKRecurringPaymentSummaryItem] ()
setTrialBillingSelector = mkSelector "setTrialBilling:"

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

