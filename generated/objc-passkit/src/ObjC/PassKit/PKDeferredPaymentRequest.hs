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
  , initSelector
  , initWithPaymentDescription_deferredBilling_managementURLSelector
  , paymentDescriptionSelector
  , setPaymentDescriptionSelector
  , deferredBillingSelector
  , setDeferredBillingSelector
  , billingAgreementSelector
  , setBillingAgreementSelector
  , managementURLSelector
  , setManagementURLSelector
  , tokenNotificationURLSelector
  , setTokenNotificationURLSelector
  , freeCancellationDateSelector
  , setFreeCancellationDateSelector
  , freeCancellationDateTimeZoneSelector
  , setFreeCancellationDateTimeZoneSelector


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
init_ :: IsPKDeferredPaymentRequest pkDeferredPaymentRequest => pkDeferredPaymentRequest -> IO (Id PKDeferredPaymentRequest)
init_ pkDeferredPaymentRequest  =
  sendMsg pkDeferredPaymentRequest (mkSelector "init") (retPtr retVoid) [] >>= ownedObject . castPtr

-- | @- initWithPaymentDescription:deferredBilling:managementURL:@
initWithPaymentDescription_deferredBilling_managementURL :: (IsPKDeferredPaymentRequest pkDeferredPaymentRequest, IsNSString paymentDescription, IsPKDeferredPaymentSummaryItem deferredBilling, IsNSURL managementURL) => pkDeferredPaymentRequest -> paymentDescription -> deferredBilling -> managementURL -> IO (Id PKDeferredPaymentRequest)
initWithPaymentDescription_deferredBilling_managementURL pkDeferredPaymentRequest  paymentDescription deferredBilling managementURL =
withObjCPtr paymentDescription $ \raw_paymentDescription ->
  withObjCPtr deferredBilling $ \raw_deferredBilling ->
    withObjCPtr managementURL $ \raw_managementURL ->
        sendMsg pkDeferredPaymentRequest (mkSelector "initWithPaymentDescription:deferredBilling:managementURL:") (retPtr retVoid) [argPtr (castPtr raw_paymentDescription :: Ptr ()), argPtr (castPtr raw_deferredBilling :: Ptr ()), argPtr (castPtr raw_managementURL :: Ptr ())] >>= ownedObject . castPtr

-- | @- paymentDescription@
paymentDescription :: IsPKDeferredPaymentRequest pkDeferredPaymentRequest => pkDeferredPaymentRequest -> IO (Id NSString)
paymentDescription pkDeferredPaymentRequest  =
  sendMsg pkDeferredPaymentRequest (mkSelector "paymentDescription") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- setPaymentDescription:@
setPaymentDescription :: (IsPKDeferredPaymentRequest pkDeferredPaymentRequest, IsNSString value) => pkDeferredPaymentRequest -> value -> IO ()
setPaymentDescription pkDeferredPaymentRequest  value =
withObjCPtr value $ \raw_value ->
    sendMsg pkDeferredPaymentRequest (mkSelector "setPaymentDescription:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- | @- deferredBilling@
deferredBilling :: IsPKDeferredPaymentRequest pkDeferredPaymentRequest => pkDeferredPaymentRequest -> IO (Id PKDeferredPaymentSummaryItem)
deferredBilling pkDeferredPaymentRequest  =
  sendMsg pkDeferredPaymentRequest (mkSelector "deferredBilling") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- setDeferredBilling:@
setDeferredBilling :: (IsPKDeferredPaymentRequest pkDeferredPaymentRequest, IsPKDeferredPaymentSummaryItem value) => pkDeferredPaymentRequest -> value -> IO ()
setDeferredBilling pkDeferredPaymentRequest  value =
withObjCPtr value $ \raw_value ->
    sendMsg pkDeferredPaymentRequest (mkSelector "setDeferredBilling:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- | @- billingAgreement@
billingAgreement :: IsPKDeferredPaymentRequest pkDeferredPaymentRequest => pkDeferredPaymentRequest -> IO (Id NSString)
billingAgreement pkDeferredPaymentRequest  =
  sendMsg pkDeferredPaymentRequest (mkSelector "billingAgreement") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- setBillingAgreement:@
setBillingAgreement :: (IsPKDeferredPaymentRequest pkDeferredPaymentRequest, IsNSString value) => pkDeferredPaymentRequest -> value -> IO ()
setBillingAgreement pkDeferredPaymentRequest  value =
withObjCPtr value $ \raw_value ->
    sendMsg pkDeferredPaymentRequest (mkSelector "setBillingAgreement:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- | @- managementURL@
managementURL :: IsPKDeferredPaymentRequest pkDeferredPaymentRequest => pkDeferredPaymentRequest -> IO (Id NSURL)
managementURL pkDeferredPaymentRequest  =
  sendMsg pkDeferredPaymentRequest (mkSelector "managementURL") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- setManagementURL:@
setManagementURL :: (IsPKDeferredPaymentRequest pkDeferredPaymentRequest, IsNSURL value) => pkDeferredPaymentRequest -> value -> IO ()
setManagementURL pkDeferredPaymentRequest  value =
withObjCPtr value $ \raw_value ->
    sendMsg pkDeferredPaymentRequest (mkSelector "setManagementURL:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- | @- tokenNotificationURL@
tokenNotificationURL :: IsPKDeferredPaymentRequest pkDeferredPaymentRequest => pkDeferredPaymentRequest -> IO (Id NSURL)
tokenNotificationURL pkDeferredPaymentRequest  =
  sendMsg pkDeferredPaymentRequest (mkSelector "tokenNotificationURL") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- setTokenNotificationURL:@
setTokenNotificationURL :: (IsPKDeferredPaymentRequest pkDeferredPaymentRequest, IsNSURL value) => pkDeferredPaymentRequest -> value -> IO ()
setTokenNotificationURL pkDeferredPaymentRequest  value =
withObjCPtr value $ \raw_value ->
    sendMsg pkDeferredPaymentRequest (mkSelector "setTokenNotificationURL:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- | @- freeCancellationDate@
freeCancellationDate :: IsPKDeferredPaymentRequest pkDeferredPaymentRequest => pkDeferredPaymentRequest -> IO (Id NSDate)
freeCancellationDate pkDeferredPaymentRequest  =
  sendMsg pkDeferredPaymentRequest (mkSelector "freeCancellationDate") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- setFreeCancellationDate:@
setFreeCancellationDate :: (IsPKDeferredPaymentRequest pkDeferredPaymentRequest, IsNSDate value) => pkDeferredPaymentRequest -> value -> IO ()
setFreeCancellationDate pkDeferredPaymentRequest  value =
withObjCPtr value $ \raw_value ->
    sendMsg pkDeferredPaymentRequest (mkSelector "setFreeCancellationDate:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- | @- freeCancellationDateTimeZone@
freeCancellationDateTimeZone :: IsPKDeferredPaymentRequest pkDeferredPaymentRequest => pkDeferredPaymentRequest -> IO (Id NSTimeZone)
freeCancellationDateTimeZone pkDeferredPaymentRequest  =
  sendMsg pkDeferredPaymentRequest (mkSelector "freeCancellationDateTimeZone") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- setFreeCancellationDateTimeZone:@
setFreeCancellationDateTimeZone :: (IsPKDeferredPaymentRequest pkDeferredPaymentRequest, IsNSTimeZone value) => pkDeferredPaymentRequest -> value -> IO ()
setFreeCancellationDateTimeZone pkDeferredPaymentRequest  value =
withObjCPtr value $ \raw_value ->
    sendMsg pkDeferredPaymentRequest (mkSelector "setFreeCancellationDateTimeZone:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @init@
initSelector :: Selector
initSelector = mkSelector "init"

-- | @Selector@ for @initWithPaymentDescription:deferredBilling:managementURL:@
initWithPaymentDescription_deferredBilling_managementURLSelector :: Selector
initWithPaymentDescription_deferredBilling_managementURLSelector = mkSelector "initWithPaymentDescription:deferredBilling:managementURL:"

-- | @Selector@ for @paymentDescription@
paymentDescriptionSelector :: Selector
paymentDescriptionSelector = mkSelector "paymentDescription"

-- | @Selector@ for @setPaymentDescription:@
setPaymentDescriptionSelector :: Selector
setPaymentDescriptionSelector = mkSelector "setPaymentDescription:"

-- | @Selector@ for @deferredBilling@
deferredBillingSelector :: Selector
deferredBillingSelector = mkSelector "deferredBilling"

-- | @Selector@ for @setDeferredBilling:@
setDeferredBillingSelector :: Selector
setDeferredBillingSelector = mkSelector "setDeferredBilling:"

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

-- | @Selector@ for @freeCancellationDate@
freeCancellationDateSelector :: Selector
freeCancellationDateSelector = mkSelector "freeCancellationDate"

-- | @Selector@ for @setFreeCancellationDate:@
setFreeCancellationDateSelector :: Selector
setFreeCancellationDateSelector = mkSelector "setFreeCancellationDate:"

-- | @Selector@ for @freeCancellationDateTimeZone@
freeCancellationDateTimeZoneSelector :: Selector
freeCancellationDateTimeZoneSelector = mkSelector "freeCancellationDateTimeZone"

-- | @Selector@ for @setFreeCancellationDateTimeZone:@
setFreeCancellationDateTimeZoneSelector :: Selector
setFreeCancellationDateTimeZoneSelector = mkSelector "setFreeCancellationDateTimeZone:"

