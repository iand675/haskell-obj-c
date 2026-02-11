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
  , initSelector
  , initWithPaymentDescription_regularBilling_managementURLSelector
  , paymentDescriptionSelector
  , setPaymentDescriptionSelector
  , regularBillingSelector
  , setRegularBillingSelector
  , trialBillingSelector
  , setTrialBillingSelector
  , billingAgreementSelector
  , setBillingAgreementSelector
  , managementURLSelector
  , setManagementURLSelector
  , tokenNotificationURLSelector
  , setTokenNotificationURLSelector


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

-- | @- init@
init_ :: IsPKRecurringPaymentRequest pkRecurringPaymentRequest => pkRecurringPaymentRequest -> IO (Id PKRecurringPaymentRequest)
init_ pkRecurringPaymentRequest  =
  sendMsg pkRecurringPaymentRequest (mkSelector "init") (retPtr retVoid) [] >>= ownedObject . castPtr

-- | @- initWithPaymentDescription:regularBilling:managementURL:@
initWithPaymentDescription_regularBilling_managementURL :: (IsPKRecurringPaymentRequest pkRecurringPaymentRequest, IsNSString paymentDescription, IsPKRecurringPaymentSummaryItem regularBilling, IsNSURL managementURL) => pkRecurringPaymentRequest -> paymentDescription -> regularBilling -> managementURL -> IO (Id PKRecurringPaymentRequest)
initWithPaymentDescription_regularBilling_managementURL pkRecurringPaymentRequest  paymentDescription regularBilling managementURL =
withObjCPtr paymentDescription $ \raw_paymentDescription ->
  withObjCPtr regularBilling $ \raw_regularBilling ->
    withObjCPtr managementURL $ \raw_managementURL ->
        sendMsg pkRecurringPaymentRequest (mkSelector "initWithPaymentDescription:regularBilling:managementURL:") (retPtr retVoid) [argPtr (castPtr raw_paymentDescription :: Ptr ()), argPtr (castPtr raw_regularBilling :: Ptr ()), argPtr (castPtr raw_managementURL :: Ptr ())] >>= ownedObject . castPtr

-- | @- paymentDescription@
paymentDescription :: IsPKRecurringPaymentRequest pkRecurringPaymentRequest => pkRecurringPaymentRequest -> IO (Id NSString)
paymentDescription pkRecurringPaymentRequest  =
  sendMsg pkRecurringPaymentRequest (mkSelector "paymentDescription") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- setPaymentDescription:@
setPaymentDescription :: (IsPKRecurringPaymentRequest pkRecurringPaymentRequest, IsNSString value) => pkRecurringPaymentRequest -> value -> IO ()
setPaymentDescription pkRecurringPaymentRequest  value =
withObjCPtr value $ \raw_value ->
    sendMsg pkRecurringPaymentRequest (mkSelector "setPaymentDescription:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- | @- regularBilling@
regularBilling :: IsPKRecurringPaymentRequest pkRecurringPaymentRequest => pkRecurringPaymentRequest -> IO (Id PKRecurringPaymentSummaryItem)
regularBilling pkRecurringPaymentRequest  =
  sendMsg pkRecurringPaymentRequest (mkSelector "regularBilling") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- setRegularBilling:@
setRegularBilling :: (IsPKRecurringPaymentRequest pkRecurringPaymentRequest, IsPKRecurringPaymentSummaryItem value) => pkRecurringPaymentRequest -> value -> IO ()
setRegularBilling pkRecurringPaymentRequest  value =
withObjCPtr value $ \raw_value ->
    sendMsg pkRecurringPaymentRequest (mkSelector "setRegularBilling:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- | @- trialBilling@
trialBilling :: IsPKRecurringPaymentRequest pkRecurringPaymentRequest => pkRecurringPaymentRequest -> IO (Id PKRecurringPaymentSummaryItem)
trialBilling pkRecurringPaymentRequest  =
  sendMsg pkRecurringPaymentRequest (mkSelector "trialBilling") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- setTrialBilling:@
setTrialBilling :: (IsPKRecurringPaymentRequest pkRecurringPaymentRequest, IsPKRecurringPaymentSummaryItem value) => pkRecurringPaymentRequest -> value -> IO ()
setTrialBilling pkRecurringPaymentRequest  value =
withObjCPtr value $ \raw_value ->
    sendMsg pkRecurringPaymentRequest (mkSelector "setTrialBilling:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- | @- billingAgreement@
billingAgreement :: IsPKRecurringPaymentRequest pkRecurringPaymentRequest => pkRecurringPaymentRequest -> IO (Id NSString)
billingAgreement pkRecurringPaymentRequest  =
  sendMsg pkRecurringPaymentRequest (mkSelector "billingAgreement") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- setBillingAgreement:@
setBillingAgreement :: (IsPKRecurringPaymentRequest pkRecurringPaymentRequest, IsNSString value) => pkRecurringPaymentRequest -> value -> IO ()
setBillingAgreement pkRecurringPaymentRequest  value =
withObjCPtr value $ \raw_value ->
    sendMsg pkRecurringPaymentRequest (mkSelector "setBillingAgreement:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- | @- managementURL@
managementURL :: IsPKRecurringPaymentRequest pkRecurringPaymentRequest => pkRecurringPaymentRequest -> IO (Id NSURL)
managementURL pkRecurringPaymentRequest  =
  sendMsg pkRecurringPaymentRequest (mkSelector "managementURL") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- setManagementURL:@
setManagementURL :: (IsPKRecurringPaymentRequest pkRecurringPaymentRequest, IsNSURL value) => pkRecurringPaymentRequest -> value -> IO ()
setManagementURL pkRecurringPaymentRequest  value =
withObjCPtr value $ \raw_value ->
    sendMsg pkRecurringPaymentRequest (mkSelector "setManagementURL:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- | @- tokenNotificationURL@
tokenNotificationURL :: IsPKRecurringPaymentRequest pkRecurringPaymentRequest => pkRecurringPaymentRequest -> IO (Id NSURL)
tokenNotificationURL pkRecurringPaymentRequest  =
  sendMsg pkRecurringPaymentRequest (mkSelector "tokenNotificationURL") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- setTokenNotificationURL:@
setTokenNotificationURL :: (IsPKRecurringPaymentRequest pkRecurringPaymentRequest, IsNSURL value) => pkRecurringPaymentRequest -> value -> IO ()
setTokenNotificationURL pkRecurringPaymentRequest  value =
withObjCPtr value $ \raw_value ->
    sendMsg pkRecurringPaymentRequest (mkSelector "setTokenNotificationURL:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @init@
initSelector :: Selector
initSelector = mkSelector "init"

-- | @Selector@ for @initWithPaymentDescription:regularBilling:managementURL:@
initWithPaymentDescription_regularBilling_managementURLSelector :: Selector
initWithPaymentDescription_regularBilling_managementURLSelector = mkSelector "initWithPaymentDescription:regularBilling:managementURL:"

-- | @Selector@ for @paymentDescription@
paymentDescriptionSelector :: Selector
paymentDescriptionSelector = mkSelector "paymentDescription"

-- | @Selector@ for @setPaymentDescription:@
setPaymentDescriptionSelector :: Selector
setPaymentDescriptionSelector = mkSelector "setPaymentDescription:"

-- | @Selector@ for @regularBilling@
regularBillingSelector :: Selector
regularBillingSelector = mkSelector "regularBilling"

-- | @Selector@ for @setRegularBilling:@
setRegularBillingSelector :: Selector
setRegularBillingSelector = mkSelector "setRegularBilling:"

-- | @Selector@ for @trialBilling@
trialBillingSelector :: Selector
trialBillingSelector = mkSelector "trialBilling"

-- | @Selector@ for @setTrialBilling:@
setTrialBillingSelector :: Selector
setTrialBillingSelector = mkSelector "setTrialBilling:"

-- | @Selector@ for @billingAgreement@
billingAgreementSelector :: Selector
billingAgreementSelector = mkSelector "billingAgreement"

-- | @Selector@ for @setBillingAgreement:@
setBillingAgreementSelector :: Selector
setBillingAgreementSelector = mkSelector "setBillingAgreement:"

-- | @Selector@ for @managementURL@
managementURLSelector :: Selector
managementURLSelector = mkSelector "managementURL"

-- | @Selector@ for @setManagementURL:@
setManagementURLSelector :: Selector
setManagementURLSelector = mkSelector "setManagementURL:"

-- | @Selector@ for @tokenNotificationURL@
tokenNotificationURLSelector :: Selector
tokenNotificationURLSelector = mkSelector "tokenNotificationURL"

-- | @Selector@ for @setTokenNotificationURL:@
setTokenNotificationURLSelector :: Selector
setTokenNotificationURLSelector = mkSelector "setTokenNotificationURL:"

