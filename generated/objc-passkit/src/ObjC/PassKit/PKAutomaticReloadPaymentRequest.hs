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
  , initSelector
  , initWithPaymentDescription_automaticReloadBilling_managementURLSelector
  , paymentDescriptionSelector
  , setPaymentDescriptionSelector
  , automaticReloadBillingSelector
  , setAutomaticReloadBillingSelector
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
init_ :: IsPKAutomaticReloadPaymentRequest pkAutomaticReloadPaymentRequest => pkAutomaticReloadPaymentRequest -> IO (Id PKAutomaticReloadPaymentRequest)
init_ pkAutomaticReloadPaymentRequest  =
  sendMsg pkAutomaticReloadPaymentRequest (mkSelector "init") (retPtr retVoid) [] >>= ownedObject . castPtr

-- | @- initWithPaymentDescription:automaticReloadBilling:managementURL:@
initWithPaymentDescription_automaticReloadBilling_managementURL :: (IsPKAutomaticReloadPaymentRequest pkAutomaticReloadPaymentRequest, IsNSString paymentDescription, IsPKAutomaticReloadPaymentSummaryItem automaticReloadBilling, IsNSURL managementURL) => pkAutomaticReloadPaymentRequest -> paymentDescription -> automaticReloadBilling -> managementURL -> IO (Id PKAutomaticReloadPaymentRequest)
initWithPaymentDescription_automaticReloadBilling_managementURL pkAutomaticReloadPaymentRequest  paymentDescription automaticReloadBilling managementURL =
withObjCPtr paymentDescription $ \raw_paymentDescription ->
  withObjCPtr automaticReloadBilling $ \raw_automaticReloadBilling ->
    withObjCPtr managementURL $ \raw_managementURL ->
        sendMsg pkAutomaticReloadPaymentRequest (mkSelector "initWithPaymentDescription:automaticReloadBilling:managementURL:") (retPtr retVoid) [argPtr (castPtr raw_paymentDescription :: Ptr ()), argPtr (castPtr raw_automaticReloadBilling :: Ptr ()), argPtr (castPtr raw_managementURL :: Ptr ())] >>= ownedObject . castPtr

-- | @- paymentDescription@
paymentDescription :: IsPKAutomaticReloadPaymentRequest pkAutomaticReloadPaymentRequest => pkAutomaticReloadPaymentRequest -> IO (Id NSString)
paymentDescription pkAutomaticReloadPaymentRequest  =
  sendMsg pkAutomaticReloadPaymentRequest (mkSelector "paymentDescription") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- setPaymentDescription:@
setPaymentDescription :: (IsPKAutomaticReloadPaymentRequest pkAutomaticReloadPaymentRequest, IsNSString value) => pkAutomaticReloadPaymentRequest -> value -> IO ()
setPaymentDescription pkAutomaticReloadPaymentRequest  value =
withObjCPtr value $ \raw_value ->
    sendMsg pkAutomaticReloadPaymentRequest (mkSelector "setPaymentDescription:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- | @- automaticReloadBilling@
automaticReloadBilling :: IsPKAutomaticReloadPaymentRequest pkAutomaticReloadPaymentRequest => pkAutomaticReloadPaymentRequest -> IO (Id PKAutomaticReloadPaymentSummaryItem)
automaticReloadBilling pkAutomaticReloadPaymentRequest  =
  sendMsg pkAutomaticReloadPaymentRequest (mkSelector "automaticReloadBilling") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- setAutomaticReloadBilling:@
setAutomaticReloadBilling :: (IsPKAutomaticReloadPaymentRequest pkAutomaticReloadPaymentRequest, IsPKAutomaticReloadPaymentSummaryItem value) => pkAutomaticReloadPaymentRequest -> value -> IO ()
setAutomaticReloadBilling pkAutomaticReloadPaymentRequest  value =
withObjCPtr value $ \raw_value ->
    sendMsg pkAutomaticReloadPaymentRequest (mkSelector "setAutomaticReloadBilling:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- | @- billingAgreement@
billingAgreement :: IsPKAutomaticReloadPaymentRequest pkAutomaticReloadPaymentRequest => pkAutomaticReloadPaymentRequest -> IO (Id NSString)
billingAgreement pkAutomaticReloadPaymentRequest  =
  sendMsg pkAutomaticReloadPaymentRequest (mkSelector "billingAgreement") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- setBillingAgreement:@
setBillingAgreement :: (IsPKAutomaticReloadPaymentRequest pkAutomaticReloadPaymentRequest, IsNSString value) => pkAutomaticReloadPaymentRequest -> value -> IO ()
setBillingAgreement pkAutomaticReloadPaymentRequest  value =
withObjCPtr value $ \raw_value ->
    sendMsg pkAutomaticReloadPaymentRequest (mkSelector "setBillingAgreement:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- | @- managementURL@
managementURL :: IsPKAutomaticReloadPaymentRequest pkAutomaticReloadPaymentRequest => pkAutomaticReloadPaymentRequest -> IO (Id NSURL)
managementURL pkAutomaticReloadPaymentRequest  =
  sendMsg pkAutomaticReloadPaymentRequest (mkSelector "managementURL") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- setManagementURL:@
setManagementURL :: (IsPKAutomaticReloadPaymentRequest pkAutomaticReloadPaymentRequest, IsNSURL value) => pkAutomaticReloadPaymentRequest -> value -> IO ()
setManagementURL pkAutomaticReloadPaymentRequest  value =
withObjCPtr value $ \raw_value ->
    sendMsg pkAutomaticReloadPaymentRequest (mkSelector "setManagementURL:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- | @- tokenNotificationURL@
tokenNotificationURL :: IsPKAutomaticReloadPaymentRequest pkAutomaticReloadPaymentRequest => pkAutomaticReloadPaymentRequest -> IO (Id NSURL)
tokenNotificationURL pkAutomaticReloadPaymentRequest  =
  sendMsg pkAutomaticReloadPaymentRequest (mkSelector "tokenNotificationURL") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- setTokenNotificationURL:@
setTokenNotificationURL :: (IsPKAutomaticReloadPaymentRequest pkAutomaticReloadPaymentRequest, IsNSURL value) => pkAutomaticReloadPaymentRequest -> value -> IO ()
setTokenNotificationURL pkAutomaticReloadPaymentRequest  value =
withObjCPtr value $ \raw_value ->
    sendMsg pkAutomaticReloadPaymentRequest (mkSelector "setTokenNotificationURL:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @init@
initSelector :: Selector
initSelector = mkSelector "init"

-- | @Selector@ for @initWithPaymentDescription:automaticReloadBilling:managementURL:@
initWithPaymentDescription_automaticReloadBilling_managementURLSelector :: Selector
initWithPaymentDescription_automaticReloadBilling_managementURLSelector = mkSelector "initWithPaymentDescription:automaticReloadBilling:managementURL:"

-- | @Selector@ for @paymentDescription@
paymentDescriptionSelector :: Selector
paymentDescriptionSelector = mkSelector "paymentDescription"

-- | @Selector@ for @setPaymentDescription:@
setPaymentDescriptionSelector :: Selector
setPaymentDescriptionSelector = mkSelector "setPaymentDescription:"

-- | @Selector@ for @automaticReloadBilling@
automaticReloadBillingSelector :: Selector
automaticReloadBillingSelector = mkSelector "automaticReloadBilling"

-- | @Selector@ for @setAutomaticReloadBilling:@
setAutomaticReloadBillingSelector :: Selector
setAutomaticReloadBillingSelector = mkSelector "setAutomaticReloadBilling:"

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

